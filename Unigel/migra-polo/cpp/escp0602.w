
&Scoped-define WINDOW-NAME C-Win

/*------------------------------------------------------------------------
File.............: escp0602.w
Description......: Otimiza‡Æo de Corte de Bobinas 
Input Parameters : 
Output Parameters: 
Author...........: Amgra - Jos‚ Roberto.
Created..........: 10/04/2011   
OBS..............: 
------------------------------------------------------------------------*/
DEFINE VARIABLE r-codigo-rwi  AS ROWID                     NO-UNDO.
define buffer if-ped-venda for espmulti.if-ped-venda.    
define variable c-prog-gerado as character no-undo initial "escp0602".
    DEFINE BUFFER b-ped-venda FOR ped-venda.
define buffer empresa for mgmulti.empresa.
def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

CREATE WIDGET-POOL. 

/* ***************************  Definitions  ************************** */


/* Preprocessadores do Template de Relat¢rio                            */

&GLOBAL-DEFINE PGSEL f-pg-sel 
&GLOBAL-DEFINE PGPAR f-pg-par 
&GLOBAL-DEFINE PGDIG f-pg-dig 
&GLOBAL-DEFINE PGIMP f-pg-imp 

/* Include Com as Vari veis Globais */

def new global shared var i-ep-codigo-usuario  like mgmulti.empresa.ep-codigo no-undo.
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


/* Parameters Definitions ---                                           */ 


/* Temporary Table Definitions ---                                      */ 

/* alteracao para pegar filtro cliente fim quando unigel comercial Edson 16-11-2012*/
DEFINE TEMP-TABLE tt-peduc NO-UNDO
    FIELD nome-abrev LIKE ped-venda.nome-abrev
    FIELD nr-pedido LIKE ped-venda.nr-pedido
    FIELD cod-estabel LIKE ped-venda.cod-estabel
    INDEX ped IS PRIMARY UNIQUE nr-pedido.


    DEFINE TEMP-TABLE tt-pedido no-undo
        FIELD nr-pedido            AS INT     FORMAT ">>>>>>>9"       LABEL "Pedido"
        FIELD nr-sequencia         AS INT     FORMAT ">>>>>9"         LABEL "Seq.Ped"
        FIELD tp-pedido            AS CHAR    FORMAT "x(1)"           LABEL "Tp"
        FIELD it-codigo            AS CHAR    FORMAT "x(16)"          LABEL "Item"
        FIELD qt-pedida            AS DEC     FORMAT ">>>>>>>9.9999"  label "Qt.Pedida"
        FIELD qt-bobinas           AS INT     FORMAT ">>>>>9"         LABEL "Qt.Bobs"
        FIELD larg                 AS INT     FORMAT ">>>>9"          label "Larg"
        FIELD diex                 AS INT     FORMAT ">>>>9"          label "Diex"
        FIELD diin                 AS INT     FORMAT ">>>>9"          label "Diin"
        FIELD cod-refer            AS CHAR    FORMAT "x(8)"           LABEL "Refer."
        FIELD nome-abrev           AS CHAR    FORMAT "x(13)"          LABEL "Cliente"
        FIELD nome-abrev-fim       AS CHAR    FORMAT "x(13)"          LABEL "Cli.Fim"       
        FIELD dt-entrega           AS DATE    FORMAT "99/99/9999"     LABEL "Dt.Entrega"
        FIELD seq-resumo           AS INT     FORMAT ">>9"            LABEL "Seq.Res"
        FIELD peso-bob             AS DEC     FORMAT ">>>>9.999"      LABEL "Peso Bob"
        FIELD obs                  AS CHAR    FORMAT "x(3)"           LABEL "Obs"
        field nr-ord-produ         as dec     format ">>>>>>>>9"      label "NR.Ordem"
        INDEX chave IS PRIMARY UNIQUE nr-pedido
                                      nr-sequencia.

    DEFINE TEMP-TABLE tt-resumo-ped no-undo
        FIELD seq                  AS INT     FORMAT ">>9"            LABEL "Seq"
        FIELD diex                 AS INT     FORMAT ">>>>9"          label "Diex"
        FIELD diin                 AS INT     FORMAT ">>>>9"          label "Diin"
        FIELD larg                 AS INT     FORMAT ">>>>9"          label "Larg"
        FIELD it-codigo            AS CHAR    FORMAT "x(16)"          LABEL "Item"
        FIELD qt-bobinas           AS INT     FORMAT ">>>>>9"         LABEL "Qt.Bobs"
        FIELD peso-bob             AS DEC     FORMAT ">>>>9.999"      LABEL "Peso Bob"
        INDEX chave IS PRIMARY UNIQUE diex
                                      diin
                                      larg
                                      seq.

    DEFINE TEMP-TABLE tt-doff-sec no-undo
        FIELD sq-doff              AS INT     FORMAT ">>>>"       LABEL "Seq."
        FIELD diin                 AS INT     FORMAT ">>>>"       LABEL "Diin"
        FIELD diex                 AS INT     FORMAT ">>>>"       LABEL "Diex"
        FIELD qt-doff              AS INT     FORMAT ">>>>>>9"    LABEL "Qt.Doff"
        FIELD qt-bobinas           AS INT     FORMAT ">>>>>>9"    LABEL "Qt.Bobs"
        FIELD lg-faca-1            AS INT     FORMAT ">>>>"       LABEL "Lg. 1"
        FIELD lg-faca-2            AS INT     FORMAT ">>>>"       LABEL "Lg. 2"
        FIELD lg-faca-3            AS INT     FORMAT ">>>>"       LABEL "Lg. 3"
        FIELD lg-faca-4            AS INT     FORMAT ">>>>"       LABEL "Lg. 4"
        FIELD lg-faca-5            AS INT     FORMAT ">>>>"       LABEL "Lg. 5"
        FIELD lg-faca-6            AS INT     FORMAT ">>>>"       LABEL "Lg. 6"
        FIELD lg-faca-7            AS INT     FORMAT ">>>>"       LABEL "Lg. 7"
        FIELD lg-faca-8            AS INT     FORMAT ">>>>"       LABEL "Lg. 8"
        FIELD lg-faca-9            AS INT     FORMAT ">>>>"       LABEL "Lg. 9"
        FIELD lg-faca-10           AS INT     FORMAT ">>>>"       LABEL "Lg.10"
        FIELD lg-faca-11           AS INT     FORMAT ">>>>"       LABEL "Lg.11"
        FIELD lg-faca-12           AS INT     FORMAT ">>>>"       LABEL "Lg.12"
        FIELD lg-faca-13           AS INT     FORMAT ">>>>"       LABEL "Lg.13"
        FIELD lg-faca-14           AS INT     FORMAT ">>>>"       LABEL "Lg.14"
        FIELD lg-faca-15           AS INT     FORMAT ">>>>"       LABEL "Lg.15"
        FIELD lg-faca-16           AS INT     FORMAT ">>>>"       LABEL "Lg.16"
        FIELD lg-origem            AS INT     FORMAT ">>>>"       LABEL "Lg.Origem"
        FIELD qt-sobras            AS INT     FORMAT ">>>>>9"     LABEL "Sobras"
        FIELD qt-perdas            AS INT     FORMAT ">>>>>9"     LABEL "Perdas"
        FIELD qt-bob-est           AS INT     FORMAT ">>>>>>>9"   LABEL "Bob.Est"
        FIELD peso-doff            AS DEC     FORMAT ">>>>>9.999" LABEL "Peso Doff"
        FIELD lg-bob-mae-1         AS INT     FORMAT ">>>>"       LABEL "BB.MÆe 1"
        FIELD lg-bob-mae-2         AS INT     FORMAT ">>>>"       LABEL "BB.MÆe 2"
        FIELD kg-bob-mae-1         AS DEC     FORMAT ">>>>>9.999" LABEL "Peso MÆe 1"
        FIELD kg-bob-mae-2         AS DEC     FORMAT ">>>>>9.999" LABEL "Peso MÆe 2"
        FIELD peso-bob-mae         AS DEC     FORMAT ">>>>>9.999" LABEL "Peso Orig.MÆe"
        FIELD soma-larg            AS INT     FORMAT ">>>>>>>9"   LABEL "Soma Larg"
        FIELD obs                  AS CHAR    FORMAT "x(40)"      LABEL "Obs."
        INDEX chave IS PRIMARY UNIQUE sq-doff.

    DEFINE TEMP-TABLE tt-doff-rot no-undo
        FIELD sq-doff              AS INT     FORMAT ">>>>"       LABEL "Seq."
        FIELD diin                 AS INT     FORMAT ">>>>"       LABEL "Diin"
        FIELD diex                 AS INT     FORMAT ">>>>"       LABEL "Diex"
        FIELD qt-doff              AS INT     FORMAT ">>>>>>9"    LABEL "Qt.Doff"
        FIELD qt-bobinas           AS INT     FORMAT ">>>>>>9"    LABEL "Qt.Bobs"
        FIELD lg-faca-1            AS INT     FORMAT ">>>>"       LABEL "Lg. 1"
        FIELD lg-faca-2            AS INT     FORMAT ">>>>"       LABEL "Lg. 2"
        FIELD lg-faca-3            AS INT     FORMAT ">>>>"       LABEL "Lg. 3"
        FIELD lg-faca-4            AS INT     FORMAT ">>>>"       LABEL "Lg. 4"
        FIELD lg-faca-5            AS INT     FORMAT ">>>>"       LABEL "Lg. 5"
        FIELD lg-faca-6            AS INT     FORMAT ">>>>"       LABEL "Lg. 6"
        FIELD lg-faca-7            AS INT     FORMAT ">>>>"       LABEL "Lg. 7"
        FIELD lg-faca-8            AS INT     FORMAT ">>>>"       LABEL "Lg. 8"
        FIELD lg-faca-9            AS INT     FORMAT ">>>>"       LABEL "Lg. 9"
        FIELD lg-faca-10           AS INT     FORMAT ">>>>"       LABEL "Lg.10"
        FIELD lg-faca-11           AS INT     FORMAT ">>>>"       LABEL "Lg.11"
        FIELD lg-faca-12           AS INT     FORMAT ">>>>"       LABEL "Lg.12"
        FIELD lg-faca-13           AS INT     FORMAT ">>>>"       LABEL "Lg.13"
        FIELD lg-faca-14           AS INT     FORMAT ">>>>"       LABEL "Lg.14"
        FIELD lg-faca-15           AS INT     FORMAT ">>>>"       LABEL "Lg.15"
        FIELD lg-faca-16           AS INT     FORMAT ">>>>"       LABEL "Lg.16"
        FIELD lg-origem            AS INT     FORMAT ">>>>"       LABEL "Lg.Origem"
        FIELD qt-sobras            AS INT     FORMAT ">>>>>9"     LABEL "Sobras" 
        FIELD qt-perdas            AS INT     FORMAT ">>>>>9"     LABEL "Perdas" 
        FIELD qt-bob-est           AS INT     FORMAT ">>>>>>>9"   LABEL "Bob.Est"
        FIELD peso-doff            AS DEC     FORMAT ">>>>>9.999" LABEL "Peso Doff"
        FIELD obs                  AS CHAR    FORMAT "x(40)"      LABEL "Obs."
        INDEX chave IS PRIMARY UNIQUE sq-doff.

    DEFINE TEMP-TABLE tt-sel-bob no-undo
        FIELD diin                 AS INT          
        FIELD diex                 AS INT       
        FIELD larg                 AS INT    EXTENT 16   
        FIELD qtd-bob              AS INT    EXTENT 16 
        FIELD qtd-larg             AS INT
        INDEX chave IS PRIMARY UNIQUE diin
                                      diex.
                                     

    DEFINE TEMP-TABLE tt-bobinas no-undo
        FIELD diin                 AS INT          
        FIELD diex                 AS INT       
        FIELD larg                 AS INT    EXTENT 16   
        FIELD qtd-bob              AS INT    EXTENT 16 
        FIELD qtd-larg             AS INT
        INDEX chave IS PRIMARY UNIQUE diin
                                      diex.
                                     
    DEFINE TEMP-TABLE tt-conjuga-1 no-undo
        FIELD diin                 AS INT          
        FIELD diex                 AS INT   
        FIELD chave                AS CHAR 
        FIELD larg                 AS INT    EXTENT 16
        FIELD sobra                AS dec
        INDEX chave IS PRIMARY UNIQUE diin
                                      diex
                                      chave.
                                     

    DEFINE TEMP-TABLE tt-resumo-ant no-undo
        FIELD seq                  AS INT     FORMAT ">>9"            LABEL "Seq"
        FIELD diex                 AS INT     FORMAT ">>>>9"          label "Diex"
        FIELD diin                 AS INT     FORMAT ">>>>9"          label "Diin"
        FIELD larg                 AS INT     FORMAT ">>>>9"          label "Larg"
        FIELD it-codigo            AS CHAR    FORMAT "x(16)"          LABEL "Item"
        FIELD qt-bobinas           AS INT     FORMAT ">>>>>9"         LABEL "Qt.Bobs"
        FIELD peso-bob             AS DEC     FORMAT ">>>>9.999"      LABEL "Peso Bob"
        INDEX chave IS PRIMARY UNIQUE diex
                                      diin
                                      larg
                                      seq.


    DEFINE TEMP-TABLE tt-doff-met no-undo
        FIELD sq-doff              AS INT     FORMAT ">>>>"       LABEL "Seq."
        FIELD diin                 AS INT     FORMAT ">>>>"       LABEL "Diin"
        FIELD diex                 AS INT     FORMAT ">>>>"       LABEL "Diex"
        FIELD qt-doff              AS INT     FORMAT ">>>9"       LABEL "Q.Dof"
        FIELD qt-bobinas           AS INT     FORMAT ">>>>>>9"    LABEL "Q.Bob"
        FIELD lg-faca-1            AS INT     FORMAT ">>>>"       LABEL "Larg"
        FIELD lg-ajustada          AS INT     FORMAT ">>>>"       LABEL "Lg.Ajust"
        FIELD qt-sobras            AS INT     FORMAT ">>>>>9"     LABEL "Sobras" 
        FIELD qt-perdas            AS INT     FORMAT ">>>>>9"     LABEL "Perdas" 
        FIELD qt-bob-est           AS INT     FORMAT ">>>>>>>9"   LABEL "Bob.Est"
        FIELD peso-doff            AS DEC     FORMAT ">>>>>9.999" LABEL "Peso Doff"
        FIELD obs                  AS CHAR    FORMAT "x(40)"      LABEL "Obs."
        INDEX chave IS PRIMARY UNIQUE sq-doff.


    DEFINE TEMP-TABLE tt-doff-pri no-undo
        FIELD sq-doff              AS INT     FORMAT ">>>>"       LABEL "Seq."
        FIELD diin                 AS INT     FORMAT ">>>>"       LABEL "Diin"
        FIELD diex                 AS INT     FORMAT ">>>>"       LABEL "Diex"
        FIELD qt-doff              AS INT     FORMAT ">>>>>>9"    LABEL "Qt.Doff"
        FIELD qt-bobinas           AS INT     FORMAT ">>>>>>9"    LABEL "Qt.Bobs"
        FIELD lg-faca-1            AS INT     FORMAT ">>>>"       LABEL "Lg. 1"
        FIELD lg-faca-2            AS INT     FORMAT ">>>>"       LABEL "Lg. 2"
        FIELD lg-faca-3            AS INT     FORMAT ">>>>"       LABEL "Lg. 3"
        FIELD lg-faca-4            AS INT     FORMAT ">>>>"       LABEL "Lg. 4"
        FIELD lg-faca-5            AS INT     FORMAT ">>>>"       LABEL "Lg. 5"
        FIELD lg-faca-6            AS INT     FORMAT ">>>>"       LABEL "Lg. 6"
        FIELD lg-faca-7            AS INT     FORMAT ">>>>"       LABEL "Lg. 7"
        FIELD lg-faca-8            AS INT     FORMAT ">>>>"       LABEL "Lg. 8"
        FIELD lg-faca-9            AS INT     FORMAT ">>>>"       LABEL "Lg. 9"
        FIELD lg-faca-10           AS INT     FORMAT ">>>>"       LABEL "Lg.10"
        FIELD lg-faca-11           AS INT     FORMAT ">>>>"       LABEL "Lg.11"
        FIELD lg-faca-12           AS INT     FORMAT ">>>>"       LABEL "Lg.12"
        FIELD lg-faca-13           AS INT     FORMAT ">>>>"       LABEL "Lg.13"
        FIELD lg-faca-14           AS INT     FORMAT ">>>>"       LABEL "Lg.14"
        FIELD lg-faca-15           AS INT     FORMAT ">>>>"       LABEL "Lg.15"
        FIELD lg-faca-16           AS INT     FORMAT ">>>>"       LABEL "Lg.16"
        FIELD qt-sobras            AS INT     FORMAT ">>>>>9"     LABEL "Sobras" 
        FIELD qt-perdas            AS INT     FORMAT ">>>>>9"     LABEL "Perdas" 
        FIELD qt-bob-est           AS INT     FORMAT ">>>>>>>9"   LABEL "Bob.Est"
        FIELD obs                  AS CHAR    FORMAT "x(40)"      LABEL "Obs."
        INDEX chave IS PRIMARY UNIQUE sq-doff.


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


    DEFINE TEMP-TABLE tt-larg-sec no-undo
        FIELD larg                 AS INT     FORMAT ">>>>"       
        FIELD qt-bobinas           AS INT     FORMAT ">>>>>>9"    
        INDEX chave IS PRIMARY UNIQUE larg.



    DEFINE TEMP-TABLE tt-ordem-sec no-undo
        FIELD larg                 AS INT     FORMAT ">>>>"       
        FIELD nr-pedido            AS INT     FORMAT ">>>>>>>9"   
        FIELD seq-ped              AS INT     FORMAT ">>>>"       
        FIELD qt-bobinas           AS INT     FORMAT ">>>>>>9"
        FIELD nome-abrev           AS CHAR    FORMAT "x(12)"
        FIELD cod-refer            AS CHAR    FORMAT "x(8)"

        INDEX chave IS PRIMARY UNIQUE larg
                                      nr-pedido
                                      seq-ped.  


/************************************************************************************/

/* Transfer Definitions */

    DEFINE QUERY br-pedido FOR 
          tt-pedido SCROLLING.
                   

        /* Browse definitions                                                   */
    DEFINE BROWSE br-pedido
      QUERY br-pedido  DISPLAY 
        tt-pedido.nr-pedido         
        tt-pedido.nr-sequencia  
        tt-pedido.tp-pedido     
        tt-pedido.it-codigo     
        tt-pedido.qt-pedida         
        tt-pedido.qt-bobinas        
        tt-pedido.larg              
        tt-pedido.diin          
        tt-pedido.diex          
        tt-pedido.cod-refer         FORMAT "x(10)"  
        tt-pedido.nome-abrev    
        tt-pedido.nome-abrev-fim
        
        tt-pedido.dt-entrega
        tt-pedido.seq-resumo
        tt-pedido.peso-bob
        tt-pedido.nr-ord-produ
        tt-pedido.obs

        ENABLE tt-pedido.obs

        WITH SEPARATORS SIZE 92 BY 6.2
             BGCOLOR 18 FONT 1 TITLE "Pedidos Selecionados".


    DEFINE QUERY br-resumo-ped FOR 
          tt-resumo-ped SCROLLING.
                   

        /* Browse definitions                                                   */
    DEFINE BROWSE br-resumo-ped
      QUERY br-resumo-ped  DISPLAY 
        tt-resumo-ped.diex          
        tt-resumo-ped.diin          
        tt-resumo-ped.larg          
        tt-resumo-ped.it-codigo     
        tt-resumo-ped.qt-bobinas   
        tt-resumo-ped.seq   
        tt-resumo-ped.peso-bob

        ENABLE tt-resumo-ped.qt-bobinas

        WITH SEPARATORS SIZE 45 BY 6.2
             BGCOLOR 17 FONT 1 TITLE "Resumo de Larguras a Otimizar".


    DEFINE QUERY br-doff-sec FOR 
          tt-doff-sec SCROLLING.
                   

        /* Browse definitions  */
    DEFINE BROWSE br-doff-sec
      QUERY br-doff-sec  DISPLAY 
        tt-doff-sec.sq-doff   
        tt-doff-sec.diin
        tt-doff-sec.diex
        tt-doff-sec.qt-doff        
        tt-doff-sec.qt-bobinas     
        tt-doff-sec.lg-faca-1      
        tt-doff-sec.lg-faca-2      
        tt-doff-sec.lg-faca-3      
        tt-doff-sec.lg-faca-4      
        tt-doff-sec.lg-faca-5      
        tt-doff-sec.lg-faca-6      
        tt-doff-sec.lg-faca-7      
        tt-doff-sec.lg-faca-8      
        tt-doff-sec.lg-faca-9      
        tt-doff-sec.qt-sobras     
        tt-doff-sec.qt-perdas 
        tt-doff-sec.lg-origem
        tt-doff-sec.peso-doff
        tt-doff-sec.lg-faca-10     
        tt-doff-sec.lg-faca-11     
        tt-doff-sec.lg-faca-12     
        tt-doff-sec.lg-faca-13     
        tt-doff-sec.lg-faca-14     
        tt-doff-sec.lg-faca-15     
        tt-doff-sec.lg-faca-16     
        tt-doff-sec.obs        
        ENABLE tt-doff-sec.obs
        WITH SEPARATORS SIZE 92 BY 6.2
             BGCOLOR 11 FONT 1 TITLE "Conjuga‡Æo de Bobinas Otimizadas - Secund ria".


    DEFINE QUERY br-doff-met FOR 
          tt-doff-met SCROLLING.
                   

        /* Browse definitions  */
    DEFINE BROWSE br-doff-met
      QUERY br-doff-met  DISPLAY 
        tt-doff-met.sq-doff   
        tt-doff-met.diin
        tt-doff-met.diex
        tt-doff-met.qt-doff        
        tt-doff-met.qt-bobinas     
        tt-doff-met.lg-faca-1     
        tt-doff-met.lg-ajustada
        tt-doff-met.peso-doff
        tt-doff-met.obs      

        ENABLE tt-doff-met.qt-doff

        WITH SEPARATORS SIZE 45 BY 6.2
             BGCOLOR 18 FONT 1 TITLE "Conj.Bobinas Otimizadas-Metal".


    DEFINE QUERY br-doff-pri FOR 
          tt-doff-pri SCROLLING.
                   

        /* Browse definitions  */
    DEFINE BROWSE br-doff-pri
      QUERY br-doff-pri  DISPLAY 
        tt-doff-pri.sq-doff   
        tt-doff-pri.diin
        tt-doff-pri.diex
        tt-doff-pri.qt-doff        
        tt-doff-pri.qt-bobinas     
        tt-doff-pri.lg-faca-1      
        tt-doff-pri.lg-faca-2      
        tt-doff-pri.lg-faca-3      
        tt-doff-pri.lg-faca-4      
        tt-doff-pri.lg-faca-5      
        tt-doff-pri.lg-faca-6      
        tt-doff-pri.lg-faca-7      
        tt-doff-pri.lg-faca-8      
        tt-doff-pri.lg-faca-9      
        tt-doff-pri.qt-sobras     
        tt-doff-pri.qt-perdas        
        tt-doff-pri.lg-faca-10     
        tt-doff-pri.lg-faca-11     
        tt-doff-pri.lg-faca-12     
        tt-doff-pri.lg-faca-13     
        tt-doff-pri.lg-faca-14     
        tt-doff-pri.lg-faca-15     
        tt-doff-pri.lg-faca-16     
        tt-doff-pri.obs      
        ENABLE tt-doff-pri.lg-faca-1  
               tt-doff-pri.lg-faca-2  
               tt-doff-pri.lg-faca-3  
               tt-doff-pri.lg-faca-4  
               tt-doff-pri.lg-faca-5  
               tt-doff-pri.lg-faca-6  
               tt-doff-pri.lg-faca-7  
               tt-doff-pri.lg-faca-8  
               tt-doff-pri.lg-faca-9  
               tt-doff-pri.lg-faca-10 
               tt-doff-pri.lg-faca-11 
               tt-doff-pri.lg-faca-12 
               tt-doff-pri.lg-faca-13 
               tt-doff-pri.lg-faca-14 
               tt-doff-pri.lg-faca-15 
               tt-doff-pri.lg-faca-16 

        WITH SEPARATORS SIZE 92 BY 4.8
             BGCOLOR 17 FONT 1 TITLE "Conjuga‡Æo de Bobinas Otimizadas - Prim ria".


/* Local Variable Definitions ---                                       */ 

def var l-exclui             as logical no-undo. 
def var l-ok                 as logical no-undo. 

def var c-arq-digita         as char    no-undo. 
def var c-terminal           as char    no-undo. 
def var v-cod-pg-mouse-selec as char    no-undo. 
def var v-cod-prog-i-rprun   as char    no-undo. 
def var c-impressora-old     as char    no-undo. 
def var c-arquivo-old        as char    no-undo. 
def var c-destino-old        as char    no-undo. 
def var i-cont               as int     no-undo. 
def var v-cod-prog-gerado    as char    no-undo. 

def var h-acomp            as handle no-undo.
def var v-num-reg-lidos    as int    no-undo.

/* para executar o Excel */

def var c-modelo-planilha  as char format "x(50)"         no-undo.
def var c-excel            as com-handle                  NO-UNDO.
def var c-planilha         as com-handle.
def var c-relatorio        as com-handle.

DEF VAR c-arq             AS CHAR                NO-UNDO.
DEF VAR c-arq-anexo       AS CHAR                NO-UNDO.   
DEF VAR l-cria            AS LOG                 NO-UNDO.
DEF VAR i-pagina          AS INT INIT 1          NO-UNDO.
DEF VAR i-linha           AS INT                 NO-UNDO.
DEF VAR c-coluna          AS char                NO-UNDO.



/****************** Defini‡ao de Parƒmetros do Relat¢rio *********************/ 


/****************** Defini‡ao de Vari veis de Sele‡Æo do Relat¢rio *********************/ 


DEFINE VARIABLE c-cod-estabel-amg   AS CHARACTER FORMAT "X(3)"          INITIAL "422"          NO-UNDO.

DEFINE VARIABLE dt-entrega-ini      AS DATE      FORMAT "99/99/9999"    INITIAL TODAY          NO-UNDO.
DEFINE VARIABLE dt-entrega-fim      AS DATE      FORMAT "99/99/9999"    INITIAL TODAY          NO-UNDO.

DEFINE VARIABLE c-nome-abrev-ini    AS CHARACTER FORMAT "X(12)"         INITIAL ""             NO-UNDO.
DEFINE VARIABLE c-nome-abrev-fim    AS CHARACTER FORMAT "X(12)"         INITIAL "ZZZZZZZZZZZZ" NO-UNDO.

DEFINE VARIABLE c-it-codigo-ini     AS CHARACTER FORMAT "X(16)"         INITIAL ""             NO-UNDO.
DEFINE VARIABLE c-it-codigo-met     AS CHARACTER FORMAT "X(16)"         INITIAL ""             NO-UNDO.
DEFINE VARIABLE c-it-codigo-pri     AS CHARACTER FORMAT "X(16)"         INITIAL ""             NO-UNDO.
DEFINE VARIABLE c-it-codigo-mr      AS CHARACTER FORMAT "X(16)"         INITIAL ""             NO-UNDO.

DEFINE VARIABLE dt-inic-prod        AS DATE      FORMAT "99/99/9999"    INITIAL TODAY          NO-UNDO.
DEFINE VARIABLE dt-fim-prod         AS DATE      FORMAT "99/99/9999"    INITIAL TODAY          NO-UNDO.

DEFINE VARIABLE i-nr-linha-rec      AS INT       FORMAT ">>>>>9"        INITIAL 400            NO-UNDO.
DEFINE VARIABLE i-larg-util-rec-max AS INT       FORMAT ">>>>>9"        INITIAL 0              NO-UNDO.
DEFINE VARIABLE i-larg-util-pri-max AS INT       FORMAT ">>>>>9"        INITIAL 0              NO-UNDO.
DEFINE VARIABLE i-larg-util-rec-min AS INT       FORMAT ">>>>>9"        INITIAL 0              NO-UNDO.
DEFINE VARIABLE i-qtd-facas         AS INT       FORMAT ">>>>>9"        INITIAL 0              NO-UNDO.
DEFINE VARIABLE i-larg-min-estoq    AS INT       FORMAT ">>>>>9"        INITIAL 0              NO-UNDO.
DEFINE VARIABLE i-diin-util-rec     AS INT       FORMAT ">>>>>9"        INITIAL 0              NO-UNDO.
DEFINE VARIABLE i-diex-max          AS INT       FORMAT ">>>>>9"        INITIAL 0              NO-UNDO.
DEFINE VARIABLE c-desc-linha-rec    AS CHARACTER                                               NO-UNDO.
DEFINE VARIABLE c-desc-linha-met    AS CHARACTER                                               NO-UNDO.
DEFINE VARIABLE c-desc-linha-pri    AS CHARACTER                                               NO-UNDO.
DEFINE VARIABLE i-diin-prim         AS INT       FORMAT ">>>>>9"        INITIAL 0              NO-UNDO.
DEFINE VARIABLE i-diex-prim         AS INT       FORMAT ">>>>>9"        INITIAL 0              NO-UNDO.
DEFINE VARIABLE i-nr-linha-met      AS INT       FORMAT ">>>>>9"        INITIAL 300            NO-UNDO.
DEFINE VARIABLE i-nr-linha-pri      AS INT       FORMAT ">>>>>9"        INITIAL 202            NO-UNDO.
DEFINE VARIABLE i-larg-util-pri     AS INT       FORMAT ">>>>>9"        INITIAL 0              NO-UNDO.

DEFINE VARIABLE i-larg-util-rot-max AS INT       FORMAT ">>>>>9"        INITIAL 0              NO-UNDO.
DEFINE VARIABLE i-larg-util-rot-min AS INT       FORMAT ">>>>>9"        INITIAL 0              NO-UNDO.

DEFINE VARIABLE d-perc-perda-bob    AS DEC       FORMAT ">>9.99"        INITIAL 1.40           NO-UNDO.

ASSIGN dt-fim-prod    = TODAY + 4
       dt-entrega-fim = TODAY + 30.

/****************** Defini‡ao de Vari veis de Trabalho *********************/ 

DEFINE VARIABLE erro-jr            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mens-jr            AS CHARACTER  NO-UNDO.

DEFINE VARIABLE qt-bobinas-jr      AS INTEGER    NO-UNDO.
DEFINE VARIABLE qt-bobinas-plt     AS INTEGER    NO-UNDO.
DEFINE VARIABLE larg-jr            AS INTEGER    NO-UNDO.
DEFINE VARIABLE diex-jr            AS INTEGER    NO-UNDO.
DEFINE VARIABLE diin-jr            AS INTEGER    NO-UNDO.
DEFINE VARIABLE seq-jr             AS INTEGER    NO-UNDO.
DEFINE VARIABLE seq-sel-jr         AS INTEGER    NO-UNDO.
DEFINE VARIABLE seq-jr1            AS INTEGER    NO-UNDO.
DEFINE VARIABLE peso-bob-jr        AS DECIMAL    NO-UNDO.

DEFINE VARIABLE i-jr               AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-jr-10            AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-rnd              AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-idx              AS INTEGER    NO-UNDO.
DEFINE VARIABLE menor-larg-jr      AS INTEGER    NO-UNDO.
DEFINE VARIABLE max-facas-jr       AS DECIMAL    NO-UNDO.
DEFINE VARIABLE tem-jr             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE soma-larg-jr       AS INTEGER    NO-UNDO.

DEFINE VARIABLE int-rnd            AS INTEGER    NO-UNDO.
DEFINE VARIABLE var-rnd            AS CHARACTER  FORMAT "x(64)" NO-UNDO.
DEFINE VARIABLE var-rnd-1000       AS CHARACTER  FORMAT "x(64)" EXTENT 1000 NO-UNDO.

DEFINE VARIABLE testa-qt-larg      AS INTEGER    EXTENT 16 NO-UNDO.

DEFINE VARIABLE larg-bob-x         AS INTEGER    EXTENT 16 NO-UNDO.
DEFINE VARIABLE qt-bob-x           AS INTEGER    EXTENT 16 NO-UNDO.
DEFINE VARIABLE menor-doff-jr      AS INTEGER   NO-UNDO.
DEFINE VARIABLE larg-menor-doff-jr AS INTEGER   NO-UNDO.
DEFINE VARIABLE sq-doff-jr         AS INTEGER    NO-UNDO.
DEFINE VARIABLE qtd-bob-jr         AS INTEGER    NO-UNDO.
DEFINE VARIABLE tem-conjugacao     AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-doff-jr          AS INTEGER    NO-UNDO.

DEFINE VARIABLE i-diin-jr          AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-diex-jr          AS INTEGER    NO-UNDO.

DEFINE VARIABLE soma-perdas        AS INTEGER    NO-UNDO.
DEFINE VARIABLE soma-sobras        AS INTEGER    NO-UNDO.
DEFINE VARIABLE larg-orig-jr       AS INTEGER    NO-UNDO.

DEFINE VARIABLE seq-sec-jr         AS INTEGER    NO-UNDO.
DEFINE VARIABLE qtd-doff           AS INTEGER    NO-UNDO.

DEFINE VARIABLE lg-bob-mae-1       AS INTEGER    NO-UNDO.
DEFINE VARIABLE lg-bob-mae-2       AS INTEGER    NO-UNDO.
DEFINE VARIABLE kg-bob-mae-1       AS DEC        NO-UNDO.
DEFINE VARIABLE kg-bob-mae-2       AS DEC        NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */ 

DEFINE VARIABLE i-diin-gt1 AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-diex-gt1 AS INTEGER    NO-UNDO.
DEFINE VARIABLE peso-jr    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE peso2-jr   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE fator-jr   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE prod-kg-jr AS DECIMAL    NO-UNDO.
DEFINE VARIABLE item-mr-jr AS CHARACTER  FORMAT "X(16)" NO-UNDO.

DEFINE VARIABLE aparas-rec AS INTEGER    NO-UNDO.
DEFINE VARIABLE aparas-pri AS INTEGER    NO-UNDO.
DEFINE VARIABLE aparas-met AS INTEGER    NO-UNDO.
    
/* ***********************  Control Definitions  ********************** */ 

DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO. 


DEFINE BUTTON bt-arquivo
    IMAGE-UP FILE "image\im-sea"
    IMAGE-INSENSITIVE FILE "image\ii-sea"
    LABEL " "
    SIZE 4 BY 1.
    
DEFINE BUTTON bt-config-impr
    IMAGE-UP FILE "image\im-cfprt"
    LABEL " "
    SIZE 4 BY 1.
   

DEFINE RECTANGLE RECT-20
EDGE-PIXELS 2 GRAPHIC-EDGE 
SIZE 15 BY 1 
BGCOLOR 12.


DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 28 BY 2.8.


DEFINE IMAGE IMAGE-1
    FILENAME "image\im-fir"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-2
    FILENAME "image\im-las"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-3
    FILENAME "image\im-fir"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-4
    FILENAME "image\im-las"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-5
    FILENAME "image\im-fir"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-6
    FILENAME "image\im-las"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-7
    FILENAME "image\im-fir"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-8
    FILENAME "image\im-las"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-9
    FILENAME "image\im-fir"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-10
    FILENAME "image\im-las"
    SIZE 3 BY .88.
    
DEFINE IMAGE im-pg-imp
    FILENAME "image\im-fldup"
    SIZE 15.72 BY 1.19.
    
DEFINE IMAGE im-pg-par
    FILENAME "image\im-fldup"
    SIZE 15.72 BY 1.19.
    
DEFINE IMAGE im-pg-sel
    FILENAME "image\im-fldup"
    SIZE 20 BY 1.19.

DEFINE IMAGE im-pg-dig
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.19.

DEFINE VARIABLE c-arquivo AS CHARACTER 
VIEW-AS EDITOR MAX-CHARS 256 
SIZE 40 BY 1.00 
BGCOLOR 15  font 2 NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)" INITIAL "Destino"
VIEW-AS TEXT 
SIZE 8.57 BY .62 NO-UNDO. 

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)" INITIAL "Execu‡Æo"
VIEW-AS TEXT 
SIZE 10.86 BY .62 NO-UNDO.

DEFINE VARIABLE text-parametro AS CHARACTER FORMAT "X(256)" INITIAL "Parƒmetros de ImpressÆo"
VIEW-AS TEXT 
SIZE 24.72 BY .62 NO-UNDO.


DEFINE VARIABLE rs-destino AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
          "Arquivo", 2,
          "Terminal", 3
     SIZE 44 BY 1.08 NO-UNDO.


DEFINE VARIABLE rs-execucao AS INTEGER INITIAL 1 
VIEW-AS RADIO-SET HORIZONTAL 
RADIO-BUTTONS 
"On-Line", 1,
"Batch", 2
SIZE 27.72 BY .92 NO-UNDO.

DEFINE VARIABLE tb-parametro AS LOGICAL INITIAL no 
LABEL "Imprimir P gina de Parƒmetros"
VIEW-AS TOGGLE-BOX 
SIZE 32 BY .83 
NO-UNDO.

DEFINE VARIABLE rs-formato AS INTEGER INITIAL 2 
VIEW-AS RADIO-SET HORIZONTAL 
RADIO-BUTTONS 
"80 colunas", 1,
"132 colunas", 2
SIZE 32 BY .92 NO-UNDO.


DEFINE BUTTON bt-arquivo-saida 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo-entrada AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-saida AS CHARACTER FORMAT "X(256)":U INITIAL "Arquivo de Entrada" 
      VIEW-AS TEXT 
     SIZE 19 BY .63 NO-UNDO.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 1.83.

DEFINE RECTANGLE RECT-7 
EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-2
EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL
SIZE 140 BY 3.7 
BGCOLOR 7.      

DEFINE RECTANGLE RECT-9
EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL
SIZE 140 BY 2.42 
BGCOLOR 7.      

DEFINE RECTANGLE RECT-10 
EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
SIZE 140 BY 8.22
BGCOLOR 7.

DEFINE RECTANGLE RECT-11 
EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
SIZE 140 BY 8.0
BGCOLOR 7.

DEFINE RECTANGLE RECT-22
EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
SIZE 15 BY 2.3
BGCOLOR 7.


DEFINE VARIABLE l-param-1 AS LOGICAL INITIAL no 
LABEL "Parƒmetro 1"
VIEW-AS TOGGLE-BOX 
SIZE 44 BY 1.08 NO-UNDO. 


DEFINE BUTTON bt-novo
    IMAGE FILENAME "image\im-add"
    SIZE 4 BY 1.


DEFINE BUTTON bt-cancela
    IMAGE FILENAME "image\im-can"
    SIZE 4 BY 1.

DEFINE BUTTON bt-sai
    IMAGE FILENAME "image\im-exi"
    SIZE 4 BY 1.


DEFINE BUTTON bt-ajuda 
    IMAGE FILENAME "image\im-hel"
    SIZE 4 BY 1.


DEFINE BUTTON bt-cancela2 AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 11.5 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-consulta AUTO-GO 
     LABEL "&Doff's em Excel" 
     SIZE 11.5 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-gera-ord AUTO-GO 
     LABEL "&Gera Ordens de Produ‡Æo" 
     SIZE 28 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-conjuga AUTO-GO 
     LABEL "&Conjuga Secund ria" 
     SIZE 15 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-exclui-ped AUTO-GO 
     LABEL "&Exclui Pedido" 
     SIZE 13 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-agrupa-larg AUTO-GO 
     LABEL "&Agrupa Larg" 
     SIZE 13 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-inclui-larg AUTO-GO 
     LABEL "&Inclui" 
     SIZE 10 BY 1
     BGCOLOR 8 .


DEFINE BUTTON bt-inclui-met AUTO-GO 
     LABEL "&Inclui" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-exclui-met AUTO-GO 
     LABEL "&Exclui" 
     SIZE 10 BY 1
     BGCOLOR 8 .


DEFINE BUTTON bt-conjuga-pri AUTO-GO 
     LABEL "&Conjuga Prim ria" 
     SIZE 15 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-filtro 
    IMAGE FILENAME "image\im-sav"
    SIZE 6 BY 1.5.


DEFINE BUTTON bt-add 
     IMAGE-UP FILE "adeicon\next-au":U
     IMAGE-INSENSITIVE FILE "adeicon\next-ai":U
     LABEL "" 
     SIZE 8 BY 1.3.

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "adeicon\prev-au":U
     IMAGE-INSENSITIVE FILE "adeicon\prev-ai":U
     LABEL "" 
     SIZE 8 BY 1.3.





DEFINE RECTANGLE RECT-1
EDGE-PIXELS 2 GRAPHIC-EDGE 
SIZE 140 BY 1.42 
BGCOLOR 7.

DEFINE RECTANGLE RECT-6
EDGE-PIXELS 0     
SIZE 81.72 BY .12
BGCOLOR 7 .

DEFINE RECTANGLE rt-folder
EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL
SIZE 109 BY 14.5
FGCOLOR 0.

DEFINE RECTANGLE rt-folder-left
EDGE-PIXELS 0
SIZE .43 BY 14
BGCOLOR 15.

DEFINE RECTANGLE rt-folder-right
EDGE-PIXELS 0
SIZE .43 BY 14
BGCOLOR 7.

DEFINE RECTANGLE rt-folder-top
EDGE-PIXELS 0
SIZE 107 BY .12
BGCOLOR 15 .

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
    bt-novo     AT ROW 1.25 COL 4   HELP "Embarca"
    bt-cancela  AT ROW 1.25 COL 8.5  HELP "Cancela"
    bt-sai      AT ROW 1.25 COL 130   HELP "Encerra Programa"
    bt-ajuda    AT ROW 1.25 COL 135 HELP "Ajuda"

/* Colocar aqui os campos chaves do registro */

    c-cod-estabel-amg LABEL "Estab."
     at row 2.7 col 09 colon-aligned
     view-as fill-in 
     size 12 by .88
     font 1
   
    c-it-codigo-ini LABEL "Item"
     at row 2.7 col 31 colon-aligned
     view-as fill-in 
     size 12 by .88
     font 1
    
    dt-entrega-ini LABEL "Dt.Entrega"
     at row 3.7 col 09 colon-aligned
     view-as fill-in 
     size 12 by .88
     font 1
    
    dt-entrega-fim NO-LABEL 
     at row 3.7 col 31 colon-aligned
     view-as fill-in 
     size 12 by .88
     font 1 

    c-nome-abrev-ini LABEL "Cliente"
     at row 4.7 col 09 colon-aligned
     view-as fill-in 
     size 12 by .88
     font 1
    
    c-nome-abrev-fim NO-LABEL 
     at row 4.7 col 31 colon-aligned
     view-as fill-in 
     size 12 by .88
     font 1

    dt-inic-prod LABEL "Dt.Inic.Ord"
     at row 2.7 col 107 colon-aligned
     view-as fill-in 
     size 10 by .88
     font 1
    
    dt-fim-prod LABEL "Dt.Term.Ord."
     at row 2.7 col 127 colon-aligned
     view-as fill-in 
     size 10 by .88
     font 1 

    i-nr-linha-rec LABEL "Nr.Linha Rec"
     at row 2.7 col 55 colon-aligned
     view-as fill-in 
     size 14 by .88
     font 1

    i-larg-util-rec-min LABEL "Lg.Rec Min"
     at row 3.7 col 55 colon-aligned
     view-as fill-in 
     size 5 by .88
     font 1

    i-larg-util-rec-max LABEL "Max"
     at row 3.7 col 64 colon-aligned
     view-as fill-in 
     size 5 by .88
     font 1

    c-desc-linha-rec NO-LABEL 
     at row 1.27 col 40 colon-aligned
     view-as fill-in 
     size 20 by .88
     font 1

    c-desc-linha-met NO-LABEL 
     at row 1.27 col 65 colon-aligned
     view-as fill-in 
     size 20 by .88
     font 1

    c-desc-linha-pri NO-LABEL 
     at row 1.27 col 90 colon-aligned
     view-as fill-in 
     size 20 by .88
     font 1

    i-diin-util-rec LABEL "D.Int Rec"
     at row 4.7 col 55 colon-aligned
     view-as fill-in 
     size 14 by .88
     font 1

    i-nr-linha-met LABEL "Nr.Linha Met"
     at row 2.7 col 83 colon-aligned
     view-as fill-in 
     size 10 by .88
     font 1

    i-diex-prim LABEL "D.Ext.Met/Prim"
     at row 3.7 col 83 colon-aligned
     view-as fill-in 
     size 10 by .88
     font 1

    i-diin-prim LABEL "D.Int.Met/Prim"
     at row 4.7 col 83 colon-aligned
     view-as fill-in 
     size 10 by .88
     font 1

    i-nr-linha-pri LABEL "Nr.Linha Prim"
     at row 3.7 col 107 colon-aligned
     view-as fill-in 
     size 06 by .88
     font 1

    d-perc-perda-bob LABEL "% Perda Bob.Metal"
     at row 3.7 col 127 colon-aligned
     view-as fill-in 
     size 5 by .88
     font 1

    i-larg-util-pri LABEL "Larg.étil Prim"
     at row 4.7 col 107 colon-aligned
     view-as fill-in 
     size 06 by .88
     font 1

    i-larg-min-estoq LABEL "Lg.Min.Est"
     at row 4.7 col 127 colon-aligned
     view-as fill-in 
     size 5 by .88
     font 1

    c-it-codigo-met LABEL "Item Metalizado"
     at row 22.3 col 108 colon-aligned
     view-as fill-in 
     size 15 by .88
     font 1
    
    c-it-codigo-pri LABEL "Item Prim ria"
     at row 23.3 col 108 colon-aligned
     view-as fill-in 
     size 15 by .88
     font 1
    
    c-it-codigo-mr LABEL "Item ExtrusÆo"
     at row 24.3 col 108 colon-aligned
     view-as fill-in 
     size 15 by .88
     font 1
    


    bt-filtro   AT ROW 4.2   COL 135 

    br-pedido   AT ROW 6.4 COL 2.6 
                       
    br-resumo-ped AT ROW 6.4 COL 96 
                       
    br-doff-sec AT ROW 14.4 COL 2.6 
                       
    br-doff-met AT ROW 14.4 COL 96 
                       
    br-doff-pri AT ROW 22 COL 2.6
                       
    soma-sobras LABEL "Total Sobras"
     at row 20.8 col 60 colon-aligned
     view-as fill-in 
     size 10 by .88 BGCOLOR 18
     font 1 

    soma-perdas LABEL "Total Perdas"
     at row 20.8 col 80 colon-aligned
     view-as fill-in 
     size 10 by .88  BGCOLOR 14
     font 1


    bt-exclui-ped   AT ROW 12.8   COL 4

    bt-agrupa-larg  AT ROW 12.8   COL 20

    bt-add          AT ROW 12.65   COL 38
    bt-del          AT ROW 12.65   COL 50

    bt-inclui-larg  AT ROW 12.8   COL 100
    bt-conjuga      AT ROW 12.8   COL 125
    
    bt-inclui-met   AT ROW 20.8   COL 103
    bt-exclui-met   AT ROW 20.8   COL 114
    bt-conjuga-pri  AT ROW 20.8   COL 125
    
    bt-consulta     AT ROW 22.4   COL 129

    bt-cancela2     AT ROW 24.0   COL 129

    bt-gera-ord     AT ROW 25.5   COL 103

    RECT-1  AT ROW 1.05 COL 2    
    RECT-2  AT ROW 2.55 COL 2    
    RECT-10 AT ROW 6    COL 2  
    RECT-11 AT ROW 14   COL 2  

    IMAGE-1 AT ROW 3.8    COL 24
    IMAGE-2 AT ROW 3.8    COL 28
    IMAGE-3 AT ROW 4.8    COL 24
    IMAGE-4 AT ROW 4.8    COL 28

    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
    SIDE-LABELS NO-UNDERLINE THREE-D
    AT COL 1 ROW 1 FONT 1 
    SIZE 142.43 BY 25.88. 


/* ******** Acerto da posi‡Æo dos labels e tamanho dos radio-set ******* */

DEFINE VARIABLE h-label AS WIDGET-HANDLE NO-UNDO.

/* *************************  Create Window  ************************** */

CREATE WINDOW C-Win ASSIGN
   HIDDEN             = YES
   TITLE              = "Otimiza‡Æo de Corte de Bobinas"
   HEIGHT             = 25.88
   WIDTH              = 142.43
   MAX-HEIGHT         = 27.29
   MAX-WIDTH          = 155.57
   VIRTUAL-HEIGHT     = 27.29
   VIRTUAL-WIDTH      = 155.57
   RESIZE             = yes
   SCROLL-BARS        = no
   STATUS-AREA        = yes
   BGCOLOR            = ?
   FGCOLOR            = ?
   KEEP-FRAME-Z-ORDER = yes
   THREE-D            = yes
   MESSAGE-AREA       = no
   SENSITIVE          = yes
.

/* ***************  Runtime Attributes and UIB Settings  ************** */


IF SESSION:DISPLAY-TYPE = "GUI" AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* ************************* Included-Libraries *********************** */


define variable wh-pesquisa             as handle               no-undo.
define variable c-arq-old               as char                 no-undo.

def new shared var c-nom-prog-upc-mg97  as char format "x(50)" no-undo.
def new shared var c-nom-prog-appc-mg97 as char format "x(50)" no-undo.
def new shared var c-nom-prog-dpc-mg97  as char format "x(50)" no-undo.
def new shared var c-programa-mg97      as char format "x(08)" no-undo.
def new shared var c-versao-mg97        as char format "x(08)" no-undo.

define new shared variable c-imp-old               as char                 no-undo.
define new shared variable c-arq-old-batch         as char                 no-undo.


PROCEDURE pi-trata-state :
  DEFINE INPUT PARAMETER p-issuer-hdl   AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state        AS CHAR   NO-UNDO.
  
  case entry(1, p-state, '|'):
  end.
END PROCEDURE.

on "close" of this-procedure do:
    run disable_UI.
end.

/* ************************  Control Triggers  ************************ */


ON ROW-LEAVE OF br-resumo-ped IN FRAME f-relat
DO:
    
    if br-resumo-ped:NEW-ROW in frame f-relat then 
    do transaction on error undo, return no-apply:
        create tt-resumo-ped.  
        assign INPUT BROWSE br-resumo-ped tt-resumo-ped.seq  
               input browse br-resumo-ped tt-resumo-ped.diex       
               INPUT BROWSE br-resumo-ped tt-resumo-ped.diin       
               INPUT BROWSE br-resumo-ped tt-resumo-ped.larg         
               INPUT BROWSE br-resumo-ped tt-resumo-ped.it-codigo  
               input browse br-resumo-ped tt-resumo-ped.qt-bobinas 
               INPUT BROWSE br-resumo-ped tt-resumo-ped.peso-bob.   
               br-resumo-ped:CREATE-RESULT-LIST-ENTRY() in frame f-relat.
    end.
    else do transaction on error undo, return no-apply:
        
        IF AVAIL tt-resumo-ped THEN
        assign INPUT BROWSE br-resumo-ped tt-resumo-ped.seq   
               input browse br-resumo-ped tt-resumo-ped.diex        
               INPUT BROWSE br-resumo-ped tt-resumo-ped.diin        
               INPUT BROWSE br-resumo-ped tt-resumo-ped.larg       
               INPUT BROWSE br-resumo-ped tt-resumo-ped.it-codigo  
               input browse br-resumo-ped tt-resumo-ped.qt-bobinas 
               INPUT BROWSE br-resumo-ped tt-resumo-ped.peso-bob.  

         IF AVAIL tt-resumo-ped THEN
            display
                tt-resumo-ped.seq  
                tt-resumo-ped.diex       
                tt-resumo-ped.diin       
                tt-resumo-ped.larg       
                tt-resumo-ped.it-codigo  
                tt-resumo-ped.qt-bobinas 
                tt-resumo-ped.peso-bob  
                with browse br-resumo-ped. 
                                          
   end.
 
END.

ON CHOOSE OF bt-filtro IN FRAME f-relat /* Filtro de Pedidos */
DO:

    
    DEFINE BUTTON gt1-bt-cancel AUTO-END-KEY 
         LABEL "&Cancelar" 
         SIZE 10 BY 1
         BGCOLOR 8.
    
    DEFINE BUTTON gt1-bt-ok AUTO-GO 
         LABEL "&OK" 
         SIZE 10 BY 1
         BGCOLOR 8.
    
    DEFINE RECTANGLE gt1-rt-botoes
         EDGE-PIXELS 2 GRAPHIC-EDGE  
         SIZE 60 BY 1.42
         BGCOLOR 7.

    DEFINE RECTANGLE gt1-rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59 BY 3.
    
    DEFINE FRAME gt1-frame-1

        i-diin-gt1 LABEL "D.Interno" AT ROW 1.5 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 08 BY .88
        
        i-diex-gt1 LABEL "D.Externo" AT ROW 2.5 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 08 BY .88
         
         l-exclui lABEL "Ignora Pedido com Ordem" AT ROW 2.5 COL 28 COLON-ALIGNED
         VIEW-AS TOGGLE-BOX 
         SIZE 25 BY .88
         

        
        gt1-rect-1 AT ROW 1.2 COL 2

        gt1-bt-ok          AT ROW 4.8 COL 2.14
        gt1-bt-cancel      AT ROW 4.8 COL 13             
        gt1-rt-botoes      AT ROW 4.5 COL 1
        SPACE(0.28)
        WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE 
             THREE-D SCROLLABLE TITLE "Seleciona os Diƒmetros" FONT 1
             DEFAULT-BUTTON gt1-bt-ok CANCEL-BUTTON gt1-bt-cancel.

    ON "CHOOSE":U OF gt1-bt-ok IN FRAME gt1-frame-1 DO:

      assign l-exclui = l-exclui:checked in frame gt1-frame-1.

        ASSIGN i-diin-jr = int(i-diin-gt1:SCREEN-VALUE IN FRAME gt1-frame-1)
               i-diex-jr = int(i-diex-gt1:SCREEN-VALUE IN FRAME gt1-frame-1).

     RETURN.

    END.
    
     
    ASSIGN i-diin-gt1:SCREEN-VALUE IN FRAME gt1-frame-1 = string(i-diin-jr)
           i-diex-gt1:SCREEN-VALUE IN FRAME gt1-frame-1 = string(i-diex-jr).

    ENABLE i-diin-gt1 i-diex-gt1 gt1-bt-ok gt1-bt-cancel  l-exclui
        WITH FRAME gt1-frame-1. 
    
    WAIT-FOR "GO":U OF FRAME gt1-frame-1.


    RUN pi-monta-browse-ped.
    assign bt-gera-ord:sensitive  IN FRAME f-relat = yes.
    

    FOR EACH tt-doff-sec.
        DELETE tt-doff-sec. 
    END. 

    FOR EACH tt-doff-met.
        DELETE tt-doff-met. 
    END. 

    FOR EACH tt-doff-pri.
        DELETE tt-doff-pri. 
    END. 


    CLOSE QUERY br-doff-met.
    open query br-doff-met for each tt-doff-met.
    
    if num-results("br-doff-met") > 0 THEN DO:
       get current br-doff-met.
    END.

    FOR EACH tt-doff-pri.
        DELETE tt-doff-pri. 
    END. 

    CLOSE QUERY br-doff-pri.
    open query br-doff-pri for each tt-doff-pri.
    
    if num-results("br-doff-pri") > 0 THEN DO:
       get current br-doff-pri.
    END.

    CLOSE query br-doff-sec.
    open query br-doff-sec for each tt-doff-sec.

    if num-results("br-doff-sec") > 0 THEN DO:
       get current br-doff-sec.
    END.

END.


ON CHOOSE OF bt-exclui-ped IN FRAME f-relat /* Exclui Pedidos */
DO:

    if  br-pedido:num-selected-rows > 0 then do on error undo, return no-apply:
        get current br-pedido.
        delete tt-pedido.
        if  br-pedido:delete-current-row() in frame f-relat then.
    end. 

    CLOSE query br-pedido.
    open query br-pedido for each tt-pedido.

    if num-results("br-pedido") > 0 THEN DO:
       get current br-pedido.
    END.

END.


ON CHOOSE OF bt-agrupa-larg IN FRAME f-relat /* Agrupa Largura */
DO:

    RUN pi-seleciona-pedidos.

END.

ON CHOOSE OF bt-consulta IN FRAME f-relat /* Gera em Excel */
DO:

   RUN pi-gera-planilha-doff.

END.


ON CHOOSE OF bt-gera-ord IN FRAME f-relat /* Gera Ordem Produ‡Æo */
DO:

   FIND FIRST ITEM WHERE
       ITEM.it-codigo = INPUT FRAME f-relat c-it-codigo-met
       NO-LOCK NO-ERROR.

   IF NOT AVAIL ITEM OR item.it-codigo = "" THEN DO:

       run utp/ut-msgs.p (input "show":U, input 17006, "NÆo Informado o Item Metalizado").
       APPLY "entry" TO c-it-codigo-met IN FRAME f-relat.
       RETURN NO-APPLY.
       
   END.

   FIND FIRST ITEM WHERE
       ITEM.it-codigo = INPUT FRAME f-relat c-it-codigo-pri
       NO-LOCK NO-ERROR.

   IF NOT AVAIL ITEM OR item.it-codigo = "" THEN DO:

       run utp/ut-msgs.p (input "show":U, input 17006, "NÆo Informado o Item da Prim ria").
       APPLY "entry" TO c-it-codigo-pri IN FRAME f-relat.
       RETURN NO-APPLY.
       
   END.

   FIND FIRST ITEM WHERE
       ITEM.it-codigo = INPUT FRAME f-relat c-it-codigo-mr
       NO-LOCK NO-ERROR.

   IF NOT AVAIL ITEM OR item.it-codigo = "" THEN DO:

       run utp/ut-msgs.p (input "show":U, input 17006, "NÆo Informado o Item da ExtrusÆo").
       APPLY "entry" TO c-it-codigo-mr IN FRAME f-relat.
       RETURN NO-APPLY.
       
   END.
   
/*   if c-seg-usuario <> "super" then return.*/   
 
   RUN pi-gera-tt-ordem.
   /* Cria as resevas para as ordens */

   run utp/ut-acomp.p persistent set h-acomp.

    run pi-inicializar in h-acomp(input "Gera‡Æo de Ordens e resevas Aguarde").
    
 run pi-acompanhar in h-acomp(input  "Gera‡Æo de Reservas para Ordens Aguarde").


   RUN cpp/escp0602-1.p (INPUT-OUTPUT TABLE tt-abre-ordem).
   
      
   for each tt-abre-ordem where
      tt-abre-ordem.nr-ord-produ <> 0.
      
      find first ord-prod where
         ord-prod.nr-ord-produ = tt-abre-ordem.nr-ord-produ
         exclusive-lock no-error.
         
        if avail ord-prod then  
           ASSIGN ord-prod.dt-inicio       = INPUT FRAME f-relat dt-inic-prod
                  ord-prod.dt-termino      = INPUT FRAME f-relat dt-fim-prod
                  ord-prod.dt-orig         = INPUT FRAME f-relat dt-fim-prod
                  tt-abre-ordem.dt-inicio  = INPUT FRAME f-relat dt-inic-prod
                  tt-abre-ordem.dt-fim     = INPUT FRAME f-relat dt-fim-prod.
           
   end.  


   ASSIGN item-mr-jr = INPUT FRAME f-relat c-it-codigo-mr.

   RUN cpp/escp0602-2.p (INPUT-OUTPUT TABLE tt-abre-ordem,
                    INPUT item-mr-jr).
 run pi-acompanhar in h-acomp(input  "Gera‡Æo de Reservas Concluida").
                    
   run pi-finalizar in h-acomp.
   assign self:sensitive = no.

   /*--------------------------------------------------*/


   FIND FIRST pol-param-relatorio WHERE
       pol-param-relatorio.cod-prog-dtsul = "ESCP0602" AND
       pol-param-relatorio.cod-usuario    = ""        AND
       pol-param-relatorio.mes-refer      = 1         AND  
       pol-param-relatorio.ano-refer      = 9999
       NO-ERROR.
       
   IF NOT AVAIL pol-param-relatorio THEN DO:

       CREATE pol-param-relatorio.

       ASSIGN pol-param-relatorio.cod-prog-dtsul = "ESCP0602"
              pol-param-relatorio.cod-usuario    = ""       
              pol-param-relatorio.mes-refer      = 1        
              pol-param-relatorio.ano-refer      = 9999.

   END.

   ASSIGN pol-param-relatorio.int-1 = pol-param-relatorio.int-1 + 1.

END.


ON CHOOSE OF bt-conjuga IN FRAME f-relat /* Consuga 1 */
DO:

    ASSIGN sq-doff-jr = 0.

    ASSIGN seq-sec-jr = 0.

    FIND FIRST amg-cp-param-linha WHERE
         amg-cp-param-linha.cod-estabel = INPUT FRAME f-relat c-cod-estabel-amg AND
         amg-cp-param-linha.nr-linha    = INPUT FRAME f-relat i-nr-linha-rec
         NO-LOCK NO-ERROR.

    IF NOT AVAIL amg-cp-param-linha THEN DO:

        run utp/ut-msgs.p (input "show":U, input 17006, "NÆo Informado a Linha de Recorte").
        APPLY "entry" TO i-nr-linha-rec IN FRAME f-relat.
        RETURN NO-APPLY.

    END.

    FOR EACH tt-resumo-ant.
        DELETE tt-resumo-ant.
    END.
    
    FOR EACH tt-resumo-ped.
    

        CREATE tt-resumo-ant.

        ASSIGN  tt-resumo-ant.seq  = tt-resumo-ped.seq 
                tt-resumo-ant.diex       = tt-resumo-ped.diex      
                tt-resumo-ant.diin       = tt-resumo-ped.diin      
                tt-resumo-ant.larg       = tt-resumo-ped.larg      
                tt-resumo-ant.peso-bob   = tt-resumo-ped.peso-bob      
                tt-resumo-ant.it-codigo  = tt-resumo-ped.it-codigo 
                tt-resumo-ant.qt-bobinas = tt-resumo-ped.qt-bobinas.

    END.
    

    FOR EACH tt-doff-sec.
        DELETE tt-doff-sec. 
    END. 

    FOR EACH tt-doff-met.
        DELETE tt-doff-met. 
    END. 

    FOR EACH tt-doff-pri.
        DELETE tt-doff-pri. 
    END. 

    ASSIGN tem-conjugacao = 9.

    DO WHILE tem-conjugacao <> 0.

        ASSIGN tem-conjugacao = 0.

        FOR EACH tt-sel-bob.
            DELETE tt-sel-bob.
        END.
        
        FOR EACH tt-conjuga-1.
            DELETE tt-conjuga-1.
        END.

        FOR EACH tt-bobinas.
            DELETE tt-bobinas.
        END.

        FOR EACH tt-resumo-ped NO-LOCK.

            IF tt-resumo-ped.qt-bobinas <= 0 THEN NEXT.

            FIND FIRST tt-sel-bob WHERE
                tt-sel-bob.diin = tt-resumo-ped.diin AND
                tt-sel-bob.diex = tt-resumo-ped.diex
                NO-ERROR.
        
            IF NOT AVAIL tt-sel-bob THEN
                CREATE tt-sel-bob.
        
            ASSIGN tt-sel-bob.diin = tt-resumo-ped.diin
                   tt-sel-bob.diex = tt-resumo-ped.diex.
        
            ASSIGN i-jr = 0.

            ASSIGN tem-conjugacao = 9.
        
            DO WHILE i-jr < 16.
                ASSIGN i-jr = i-jr + 1.
                IF tt-sel-bob.larg [i-jr] = 0 THEN  DO:
                    ASSIGN tt-sel-bob.larg [i-jr]    = tt-resumo-ped.larg
                           tt-sel-bob.qtd-bob [i-jr] = tt-resumo-ped.qt-bobinas
                           tt-sel-bob.qtd-larg = tt-sel-bob.qtd-larg + 1.
                    LEAVE.
                END.
        
            END.
        
        END.


        IF tem-conjugacao <> 0 THEN DO:

            FOR EACH tt-sel-bob.

                CREATE tt-bobinas.

                ASSIGN tt-bobinas.diin        = tt-sel-bob.diin    
                       tt-bobinas.diex        = tt-sel-bob.diex
                       tt-bobinas.qtd-larg    = tt-sel-bob.qtd-larg.

                ASSIGN i-jr = 0.

                DO WHILE i-jr < 16.
                    ASSIGN i-jr = i-jr + 1.

                    ASSIGN tt-bobinas.larg [i-jr]    = tt-sel-bob.larg [i-jr]
                           tt-bobinas.qtd-bob [i-jr] = tt-sel-bob.qtd-bob [i-jr].

                END.

            END.

            ASSIGN i-larg-util-rot-max = int(i-larg-util-rec-max:SCREEN-VALUE IN FRAME f-relat)
                   i-larg-util-rot-min = int(i-larg-util-rec-min:SCREEN-VALUE IN FRAME f-relat).

            RUN pi-conjuga-bob.

            FOR EACH tt-doff-rot.

                ASSIGN qtd-doff = 1.

                DO WHILE tt-doff-rot.qt-doff >= qtd-doff :

                    ASSIGN seq-sec-jr = seq-sec-jr + 1.

                    CREATE tt-doff-sec.
    
                    ASSIGN tt-doff-sec.sq-doff     = seq-sec-jr   
                           tt-doff-sec.diin        = tt-doff-rot.diin      
                           tt-doff-sec.diex        = tt-doff-rot.diex      
                           tt-doff-sec.qt-doff     = 1   
                           tt-doff-sec.qt-bobinas  = tt-doff-rot.qt-bobinas / tt-doff-rot.qt-doff
                           tt-doff-sec.lg-faca-1   = tt-doff-rot.lg-faca-1 
                           tt-doff-sec.lg-faca-2   = tt-doff-rot.lg-faca-2 
                           tt-doff-sec.lg-faca-3   = tt-doff-rot.lg-faca-3 
                           tt-doff-sec.lg-faca-4   = tt-doff-rot.lg-faca-4 
                           tt-doff-sec.lg-faca-5   = tt-doff-rot.lg-faca-5 
                           tt-doff-sec.lg-faca-6   = tt-doff-rot.lg-faca-6 
                           tt-doff-sec.lg-faca-7   = tt-doff-rot.lg-faca-7 
                           tt-doff-sec.lg-faca-8   = tt-doff-rot.lg-faca-8 
                           tt-doff-sec.lg-faca-9   = tt-doff-rot.lg-faca-9 
                           tt-doff-sec.lg-faca-10  = tt-doff-rot.lg-faca-10
                           tt-doff-sec.lg-faca-11  = tt-doff-rot.lg-faca-11
                           tt-doff-sec.lg-faca-12  = tt-doff-rot.lg-faca-12
                           tt-doff-sec.lg-faca-13  = tt-doff-rot.lg-faca-13
                           tt-doff-sec.lg-faca-14  = tt-doff-rot.lg-faca-14
                           tt-doff-sec.lg-faca-15  = tt-doff-rot.lg-faca-15
                           tt-doff-sec.lg-faca-16  = tt-doff-rot.lg-faca-16
                           tt-doff-sec.obs         = tt-doff-rot.obs
                           tt-doff-sec.lg-origem   = tt-doff-rot.lg-origem.
    
                    ASSIGN tt-doff-sec.qt-sobras = tt-doff-rot.qt-sobras 
                           tt-doff-sec.qt-perdas = tt-doff-rot.qt-perdas .


                    ASSIGN fator-jr = 0.8778 /* 0.9050 */ 
                           peso2-jr = 0
                           peso2-jr = (((((tt-doff-sec.diex * tt-doff-sec.diex) -
                                    (tt-doff-sec.diin * tt-doff-sec.diin)) * 3.1416)
                                    / 4) * (tt-doff-sec.lg-origem * fator-jr) / 1000) / 1000.   /* peso das bobinas do doff */
                   
                    ASSIGN d-perc-perda-bob = DEC(d-perc-perda-bob:SCREEN-VALUE IN FRAME f-relat).  /* EDson 15052011*/

                    ASSIGN tt-doff-sec.peso-doff = peso2-jr * (1 + (d-perc-perda-bob / 100)).


                    ASSIGN qtd-doff = qtd-doff + 1.

                END.   /* do while */
    
            END.

        END.

        

        CLOSE query br-doff-sec.
        open query br-doff-sec for each tt-doff-sec.
        
        if num-results("br-doff-sec") > 0 THEN DO:
           get current br-doff-sec.
        END.

    END.

    ASSIGN soma-perdas = 0
           soma-sobras = 0
           prod-kg-jr  = 0.

    FOR EACH tt-doff-sec NO-LOCK.

        ASSIGN soma-sobras = soma-sobras + (tt-doff-sec.qt-sobras * tt-doff-sec.qt-doff)
               soma-perdas = soma-perdas + (tt-doff-sec.qt-perdas * tt-doff-sec.qt-doff).

        ASSIGN fator-jr =  0.8778 /* 0.9050 */ .

        ASSIGN peso-jr = 0
               peso-jr = (((((tt-doff-sec.diex * tt-doff-sec.diex) -
                         (tt-doff-sec.diin * tt-doff-sec.diin)) * 3.1416)
                         / 4) * (tt-doff-sec.qt-sobras * fator-jr) / 1000) / 1000.

        ASSIGN prod-kg-jr = prod-kg-jr + (peso-jr * tt-doff-sec.qt-doff).


    END.

    ASSIGN soma-sobras:SCREEN-VALUE IN FRAME f-relat = STRING(soma-sobras)
           soma-perdas:SCREEN-VALUE IN FRAME f-relat = STRING(soma-perdas).


    FOR EACH tt-resumo-ped.
        DELETE tt-resumo-ped.
    END.


    FOR EACH tt-resumo-ant.    

        CREATE tt-resumo-ped.
        
        ASSIGN  tt-resumo-ped.seq        = tt-resumo-ant.seq 
                tt-resumo-ped.diex       = tt-resumo-ant.diex      
                tt-resumo-ped.diin       = tt-resumo-ant.diin      
                tt-resumo-ped.larg       = tt-resumo-ant.larg      
                tt-resumo-ped.peso-bob   = tt-resumo-ant.peso-bob      
                tt-resumo-ped.it-codigo  = tt-resumo-ant.it-codigo 
                tt-resumo-ped.qt-bobinas = tt-resumo-ant.qt-bobinas.
                                
    END.

    CLOSE QUERY br-resumo-ped.
    open query br-resumo-ped for each tt-resumo-ped.
    
    if num-results("br-resumo-ped") > 0 THEN DO:
       get current br-resumo-ped.
    END.

    /* monta browse da metalizadora */

    ASSIGN diin-jr = INT(i-diin-prim:SCREEN-VALUE IN FRAME f-relat)
           diex-jr = INT(i-diex-prim:SCREEN-VALUE IN FRAME f-relat)
           seq-jr  = 0.

    ASSIGN lg-bob-mae-1 = 0
           lg-bob-mae-2 = 0
           kg-bob-mae-1 = 0
           kg-bob-mae-2 = 0.

    FOR EACH tt-doff-sec.

        ASSIGN larg-jr  = INT(tt-doff-sec.lg-origem)
               peso-jr  = 0
               peso2-jr = 0.

        ASSIGN fator-jr =  0.8778 /* 0.9050 */ .
        
        ASSIGN peso-jr = (((((diex-jr * diex-jr) -
                         (diin-jr * diin-jr)) * 3.1416)
                         / 4) * (larg-jr * fator-jr) / 1000) / 1000.   /* peso da bobina metalizada */


        ASSIGN peso2-jr = (((((tt-doff-sec.diex * tt-doff-sec.diex) -
                         (tt-doff-sec.diin * tt-doff-sec.diin)) * 3.1416)
                         / 4) * (larg-jr * fator-jr) / 1000) / 1000.   /* peso das bobinas do doff */


        ASSIGN d-perc-perda-bob = DEC(d-perc-perda-bob:SCREEN-VALUE IN FRAME f-relat).  /* EDson 15052011*/

                        

        ASSIGN tt-doff-sec.peso-doff = peso2-jr  * (1 + (d-perc-perda-bob / 100)).   /*Edson 15062011*/

        IF peso2-jr > kg-bob-mae-1 THEN DO:

            ASSIGN qt-bobinas-jr = 1.
            
            ASSIGN  seq-jr = seq-jr + 1.

            CREATE tt-doff-met.

            ASSIGN tt-doff-met.sq-doff      = seq-jr
                   tt-doff-met.diin         = diin-jr
                   tt-doff-met.diex         = diex-jr
                   tt-doff-met.lg-faca-1    = larg-jr.

            ASSIGN tt-doff-met.qt-doff      = 1  
                   tt-doff-met.qt-bobinas   = 1 
                   tt-doff-met.lg-ajustada  = larg-jr + aparas-met
                   tt-doff-met.peso-doff    = peso-jr.
            

        END.

        IF kg-bob-mae-1 = 0 THEN
            ASSIGN lg-bob-mae-1 = larg-jr
                   kg-bob-mae-1 = peso-jr
                   lg-bob-mae-2 = 0
                   kg-bob-mae-2 = 0.

        ASSIGN tt-doff-sec.peso-bob-mae = peso-jr.

        ASSIGN kg-bob-mae-1 = kg-bob-mae-1 - tt-doff-sec.peso-doff.

        IF kg-bob-mae-1 < 0 THEN 
            ASSIGN lg-bob-mae-2 = larg-jr
                   kg-bob-mae-2 = peso-jr + kg-bob-mae-1
                   tt-doff-sec.lg-bob-mae-1 = lg-bob-mae-1
                   tt-doff-sec.lg-bob-mae-2 = lg-bob-mae-2
                   tt-doff-sec.kg-bob-mae-1 = 0
                   tt-doff-sec.kg-bob-mae-2 = kg-bob-mae-2.
        ELSE
            ASSIGN lg-bob-mae-2 = 0
                   kg-bob-mae-2 = 0
                   tt-doff-sec.lg-bob-mae-1 = lg-bob-mae-1
                   tt-doff-sec.kg-bob-mae-1 = kg-bob-mae-1
                   tt-doff-sec.lg-bob-mae-2 = 0
                   tt-doff-sec.kg-bob-mae-2 = 0.


        
        IF kg-bob-mae-2 <> 0 THEN
            ASSIGN kg-bob-mae-1 = kg-bob-mae-2
                   lg-bob-mae-1 = lg-bob-mae-2
                   lg-bob-mae-2 = 0
                   kg-bob-mae-2 = 0.

        ASSIGN soma-larg-jr = tt-doff-sec.lg-faca-1 +
                              tt-doff-sec.lg-faca-2 +
                              tt-doff-sec.lg-faca-3 +
                              tt-doff-sec.lg-faca-4 +
                              tt-doff-sec.lg-faca-5 +
                              tt-doff-sec.lg-faca-6 +
                              tt-doff-sec.lg-faca-7 +
                              tt-doff-sec.lg-faca-8 +
                              tt-doff-sec.lg-faca-9 +
                              tt-doff-sec.lg-faca-10 +
                              tt-doff-sec.lg-faca-11 +
                              tt-doff-sec.lg-faca-12 +
                              tt-doff-sec.lg-faca-13 +
                              tt-doff-sec.lg-faca-14 +
                              tt-doff-sec.lg-faca-15 +
                              tt-doff-sec.lg-faca-16. 

        ASSIGN tt-doff-sec.soma-larg = soma-larg-jr.

    END.

    ASSIGN d-perc-perda-bob = DEC(d-perc-perda-bob:SCREEN-VALUE IN FRAME f-relat).

    /* aumenta o peso da bobina metalizada pelo percentual de perda digitada na tala */

    FOR EACH tt-doff-met.
       /* ASSIGN tt-doff-met.peso-doff = tt-doff-met.peso-doff * (1 + (d-perc-perda-bob / 100)).*/  /* EDson*/ 
    END.



    CLOSE QUERY br-doff-met.
    open query br-doff-met for each tt-doff-met.
    
    if num-results("br-doff-met") > 0 THEN DO:
       get current br-doff-met.
    END.

    RUN pi-conjuga-pri.

END.     /* CHOOSE OF bt-conjuga */


ON CHOOSE OF bt-conjuga-pri IN FRAME f-relat /* Consuga Prim ria */
DO:


    FOR EACH tt-doff-pri.
        DELETE tt-doff-pri. 
    END. 

    RUN pi-conjuga-pri.

END.


ON CHOOSE OF bt-inclui-larg IN FRAME f-relat /* Inclui Largura */
DO:

    RUN pi-inclui-larg.

    CLOSE QUERY br-resumo-ped.
    open query br-resumo-ped for each tt-resumo-ped.
    
    if num-results("br-resumo-ped") > 0 THEN DO:
       get current br-resumo-ped.
    END.

END.


ON CHOOSE OF bt-inclui-met IN FRAME f-relat /* Inclui Largura Metal*/
DO:

    RUN pi-inclui-larg-met.

    CLOSE QUERY br-doff-met.
    open query br-doff-met for each tt-doff-met.
    
    if num-results("br-doff-met") > 0 THEN DO:
       get current br-doff-met.
    END.

END.


ON CHOOSE OF bt-add IN FRAME f-relat /* Inclui Largura a Otimizar*/
DO:

    if  br-pedido:num-selected-rows > 0 then do on error undo,
        return no-apply:

        get current br-pedido.

        IF tt-pedido.seq-resumo <> 0 THEN DO:
            
           run utp/ut-msgs.p (input "show":U, input 17006, "Pedido J  Selecionado").
           RETURN NO-APPLY.

        END.

        FIND FIRST tt-resumo-ped WHERE
            tt-resumo-ped.diex = tt-pedido.diex AND
            tt-resumo-ped.diin = tt-pedido.diin AND
            tt-resumo-ped.larg = tt-pedido.larg
            NO-ERROR.

        IF NOT AVAIL tt-resumo-ped THEN DO:

            ASSIGN seq-sel-jr = seq-sel-jr + 1.

            CREATE tt-resumo-ped.

            ASSIGN tt-resumo-ped.seq      = seq-sel-jr
                   tt-resumo-ped.larg     = tt-pedido.larg
                   tt-resumo-ped.diin     = tt-pedido.diin 
                   tt-resumo-ped.diex     = tt-pedido.diex
                   tt-resumo-ped.peso-bob = tt-pedido.peso-bob.

        END.

        ASSIGN tt-resumo-ped.it-codigo  = tt-pedido.it-codigo
               tt-resumo-ped.qt-bobinas = tt-resumo-ped.qt-bobinas + tt-pedido.qt-bobinas.

        ASSIGN tt-pedido.seq-resumo = seq-sel-jr.

    END.


    CLOSE query br-resumo-ped.
    open query br-resumo-ped for each tt-resumo-ped.

    if num-results("br-resumo-ped") > 0 THEN DO:
       get current br-resumo-ped.
    END.


END.



ON CHOOSE OF bt-del IN FRAME f-relat /* Exclui Largura */
DO:

    if  br-pedido:num-selected-rows > 0 then do on error undo,
        return no-apply:

        get current br-pedido.

        IF tt-pedido.seq-resumo = 0 THEN DO:
            
           run utp/ut-msgs.p (input "show":U, input 17006, "Pedido Ainda NÆo Selecionado").
           RETURN NO-APPLY.

        END.

        FIND FIRST tt-resumo-ped WHERE
            tt-resumo-ped.diex = tt-pedido.diex AND
            tt-resumo-ped.diin = tt-pedido.diin AND
            tt-resumo-ped.larg = tt-pedido.larg
            NO-ERROR.

        IF NOT AVAIL tt-resumo-ped THEN DO:

            run utp/ut-msgs.p (input "show":U, input 17006, "Pedido Ainda NÆo Selecionado").
            RETURN NO-APPLY.

        END.

        ASSIGN tt-resumo-ped.qt-bobinas = tt-resumo-ped.qt-bobinas - tt-pedido.qt-bobinas.

        ASSIGN tt-pedido.seq-resumo = 0.

        IF tt-resumo-ped.qt-bobinas = 0 THEN 
           DELETE tt-resumo-ped.

    END.

    CLOSE query br-resumo-ped.
    open query br-resumo-ped for each tt-resumo-ped.

    if num-results("br-resumo-ped") > 0 THEN DO:
       get current br-resumo-ped.
    END.

END.


ON CHOOSE OF bt-exclui-met IN FRAME f-relat /* Exclui Metal */
DO:

    if  br-doff-met:num-selected-rows > 0 then do on error undo, return no-apply:
        get current br-doff-met.
        delete tt-doff-met.
        if  br-doff-met:delete-current-row() in frame f-relat then.
    end. 

    CLOSE query br-doff-met.
    open query br-doff-met for each tt-doff-met.

    if num-results("br-doff-met") > 0 THEN DO:
       get current br-doff-met.
    END.

END.



on LEAVE OF i-nr-linha-rec in frame f-relat do:

    FIND FIRST amg-cp-param-linha WHERE
         amg-cp-param-linha.cod-estabel = INPUT FRAME f-relat c-cod-estabel-amg AND
         amg-cp-param-linha.nr-linha    = INPUT FRAME f-relat i-nr-linha-rec
         NO-LOCK NO-ERROR.

    IF AVAIL amg-cp-param-linha THEN 
        ASSIGN i-larg-util-rec-max:SCREEN-VALUE IN FRAME f-relat  = string(amg-cp-param-linha.larg-util-max)
               i-larg-util-rec-min:SCREEN-VALUE IN FRAME f-relat  = string(amg-cp-param-linha.larg-util-min)
               i-qtd-facas = amg-cp-param-linha.qt-facas
               i-diin-util-rec:SCREEN-VALUE IN FRAME f-relat      = string(amg-cp-param-linha.diin-util)
               i-diex-max  = amg-cp-param-linha.diex-max
               i-larg-min-estoq:SCREEN-VALUE IN FRAME f-relat = string(amg-cp-param-linha.larg-min-estoq)
               c-desc-linha-rec:SCREEN-VALUE IN FRAME f-relat = amg-cp-param-linha.descricao
               aparas-rec = amg-cp-param-linha.int-1.

    ELSE
        ASSIGN i-larg-util-rec-max:SCREEN-VALUE IN FRAME f-relat      = "0"
               i-larg-util-rec-min:SCREEN-VALUE IN FRAME f-relat      = "0"
               i-qtd-facas = 0
               i-diin-util-rec:SCREEN-VALUE IN FRAME f-relat      = "0"
               i-diex-max  = 0
               i-larg-min-estoq:SCREEN-VALUE IN FRAME f-relat = "0" 
               c-desc-linha-rec:SCREEN-VALUE IN FRAME f-relat     = ""
               aparas-rec = 0.

end.    


on LEAVE OF i-nr-linha-met in frame f-relat do:

    FIND FIRST amg-cp-param-linha WHERE
         amg-cp-param-linha.cod-estabel = INPUT FRAME f-relat c-cod-estabel-amg AND
         amg-cp-param-linha.nr-linha    = INPUT FRAME f-relat i-nr-linha-met
         NO-LOCK NO-ERROR.

    IF AVAIL amg-cp-param-linha THEN 
        ASSIGN c-desc-linha-met:SCREEN-VALUE IN FRAME f-relat = amg-cp-param-linha.descricao
               i-diex-prim:SCREEN-VALUE IN FRAME f-relat      = string(amg-cp-param-linha.diex-max)
               i-diin-prim:SCREEN-VALUE IN FRAME f-relat      = string(amg-cp-param-linha.diin-util)
               aparas-met = amg-cp-param-linha.int-1.
        
    ELSE
        ASSIGN c-desc-linha-met:SCREEN-VALUE IN FRAME f-relat = ""
               i-diex-prim:SCREEN-VALUE IN FRAME f-relat      = ""
               i-diin-prim:SCREEN-VALUE IN FRAME f-relat      = ""
               aparas-met = 0.

end.    


on LEAVE OF i-nr-linha-pri in frame f-relat do:

    FIND FIRST amg-cp-param-linha WHERE
         amg-cp-param-linha.cod-estabel = INPUT FRAME f-relat c-cod-estabel-amg AND
         amg-cp-param-linha.nr-linha    = INPUT FRAME f-relat i-nr-linha-pri
         NO-LOCK NO-ERROR.

    IF AVAIL amg-cp-param-linha THEN 
        ASSIGN c-desc-linha-pri:SCREEN-VALUE IN FRAME f-relat = amg-cp-param-linha.descricao
               i-larg-util-pri:SCREEN-VALUE IN FRAME f-relat  = string(amg-cp-param-linha.larg-util-max)
               aparas-pri = amg-cp-param-linha.int-1
               i-larg-util-pri-max = amg-cp-param-linha.larg-util-max.

    ELSE
        ASSIGN c-desc-linha-pri:SCREEN-VALUE IN FRAME f-relat = ""
               aparas-pri = 0.

end.    



on LEAVE OF c-cod-estabel-amg in frame f-relat do:
    
   FIND FIRST estabelec WHERE
       estabelec.cod-estabel = c-cod-estabel-amg:SCREEN-VALUE IN FRAME f-relat
       NO-LOCK NO-ERROR.
   

   IF NOT AVAIL estabelec THEN DO:

   END.


end.    



ON CHOOSE OF bt-cancela2 IN FRAME f-relat
DO:

    RUN pi-limpa-campos.

    APPLY "entry" TO c-cod-estabel-amg IN FRAME f-relat.

END.


ON ENTER OF br-doff-sec in frame f-relat
ANYWHERE
DO:
  apply 'tab' to self.
END.

ON ROW-LEAVE OF br-doff-sec in frame f-relat
DO:

    /*  aqui que a grava‡Æo da linha da temp-table ? efetivada.
       Por‚m as valida‡äes dos registros devem ser feitas na procedure pi-executar,
       no local indicado pelo coment rio */   
    
    if br-doff-sec:NEW-ROW in frame f-relat then   
    do transaction on error undo, return no-apply:
        create tt-doff-sec.  
        assign INPUT BROWSE br-doff-sec tt-doff-sec.sq-doff    
               INPUT BROWSE br-doff-sec tt-doff-sec.qt-doff    
               input browse br-doff-sec tt-doff-sec.qt-bobinas 
               INPUT BROWSE br-doff-sec tt-doff-sec.lg-faca-1  
               INPUT BROWSE br-doff-sec tt-doff-sec.lg-faca-2  
               input browse br-doff-sec tt-doff-sec.lg-faca-3  
               INPUT BROWSE br-doff-sec tt-doff-sec.lg-faca-4  
               input browse br-doff-sec tt-doff-sec.lg-faca-5  
               INPUT BROWSE br-doff-sec tt-doff-sec.lg-faca-6  
               INPUT BROWSE br-doff-sec tt-doff-sec.lg-faca-7  
               INPUT BROWSE br-doff-sec tt-doff-sec.lg-faca-8  
               input browse br-doff-sec tt-doff-sec.lg-faca-9  
               INPUT BROWSE br-doff-sec tt-doff-sec.lg-faca-10 
               INPUT BROWSE br-doff-sec tt-doff-sec.lg-faca-11 
               input browse br-doff-sec tt-doff-sec.lg-faca-12 
               INPUT BROWSE br-doff-sec tt-doff-sec.lg-faca-13 
               input browse br-doff-sec tt-doff-sec.lg-faca-14 
               INPUT BROWSE br-doff-sec tt-doff-sec.lg-faca-15 
               INPUT BROWSE br-doff-sec tt-doff-sec.lg-faca-16 
               input browse br-doff-sec tt-doff-sec.qt-sobras 
               INPUT BROWSE br-doff-sec tt-doff-sec.qt-perdas    
               INPUT BROWSE br-doff-sec tt-doff-sec.obs  .      
               br-doff-sec:CREATE-RESULT-LIST-ENTRY() in frame f-relat.
    end.
    else do transaction on error undo, return no-apply:
        
        IF AVAIL tt-doff-sec THEN
            assign INPUT BROWSE br-doff-sec tt-doff-sec.sq-doff    
                   INPUT BROWSE br-doff-sec tt-doff-sec.qt-doff    
                   input browse br-doff-sec tt-doff-sec.qt-bobinas 
                   INPUT BROWSE br-doff-sec tt-doff-sec.lg-faca-1  
                   INPUT BROWSE br-doff-sec tt-doff-sec.lg-faca-2  
                   input browse br-doff-sec tt-doff-sec.lg-faca-3  
                   INPUT BROWSE br-doff-sec tt-doff-sec.lg-faca-4  
                   input browse br-doff-sec tt-doff-sec.lg-faca-5  
                   INPUT BROWSE br-doff-sec tt-doff-sec.lg-faca-6  
                   INPUT BROWSE br-doff-sec tt-doff-sec.lg-faca-7  
                   INPUT BROWSE br-doff-sec tt-doff-sec.lg-faca-8  
                   input browse br-doff-sec tt-doff-sec.lg-faca-9  
                   INPUT BROWSE br-doff-sec tt-doff-sec.lg-faca-10 
                   INPUT BROWSE br-doff-sec tt-doff-sec.lg-faca-11 
                   input browse br-doff-sec tt-doff-sec.lg-faca-12 
                   INPUT BROWSE br-doff-sec tt-doff-sec.lg-faca-13 
                   input browse br-doff-sec tt-doff-sec.lg-faca-14 
                   INPUT BROWSE br-doff-sec tt-doff-sec.lg-faca-15 
                   INPUT BROWSE br-doff-sec tt-doff-sec.lg-faca-16 
                   input browse br-doff-sec tt-doff-sec.qt-sobras 
                   INPUT BROWSE br-doff-sec tt-doff-sec.qt-perdas    
                   INPUT BROWSE br-doff-sec tt-doff-sec.obs.        
                   
         IF AVAIL tt-doff-sec THEN
            display
                tt-doff-sec.sq-doff  
                tt-doff-sec.diin
                tt-doff-sec.diex
                tt-doff-sec.qt-doff     
                tt-doff-sec.qt-bobinas  
                tt-doff-sec.lg-faca-1   
                tt-doff-sec.lg-faca-2   
                tt-doff-sec.lg-faca-3   
                tt-doff-sec.lg-faca-4   
                tt-doff-sec.lg-faca-5   
                tt-doff-sec.lg-faca-6   
                tt-doff-sec.lg-faca-7   
                tt-doff-sec.lg-faca-8   
                tt-doff-sec.lg-faca-9   
                tt-doff-sec.lg-faca-10  
                tt-doff-sec.lg-faca-11  
                tt-doff-sec.lg-faca-12  
                tt-doff-sec.lg-faca-13  
                tt-doff-sec.lg-faca-14  
                tt-doff-sec.lg-faca-15  
                tt-doff-sec.lg-faca-16  
                tt-doff-sec.qt-sobras  
                tt-doff-sec.qt-perdas     
                tt-doff-sec.obs         
                with browse br-doff-sec. 
                                          
   end.
 
END.


ON ROW-LEAVE OF br-doff-pri in frame f-relat
DO:

    /*  aqui que a grava‡Æo da linha da temp-table ? efetivada.
       Por‚m as valida‡äes dos registros devem ser feitas na procedure pi-executar,
       no local indicado pelo coment rio */   
    
    if br-doff-pri:NEW-ROW in frame f-relat then   
    do transaction on error undo, return no-apply:

        create tt-doff-pri.  
        assign INPUT BROWSE br-doff-pri tt-doff-pri.sq-doff    
               INPUT BROWSE br-doff-pri tt-doff-pri.qt-doff    
               input browse br-doff-pri tt-doff-pri.qt-bobinas 
               INPUT BROWSE br-doff-pri tt-doff-pri.lg-faca-1  
               INPUT BROWSE br-doff-pri tt-doff-pri.lg-faca-2  
               input browse br-doff-pri tt-doff-pri.lg-faca-3  
               INPUT BROWSE br-doff-pri tt-doff-pri.lg-faca-4  
               input browse br-doff-pri tt-doff-pri.lg-faca-5  
               INPUT BROWSE br-doff-pri tt-doff-pri.lg-faca-6  
               INPUT BROWSE br-doff-pri tt-doff-pri.lg-faca-7  
               INPUT BROWSE br-doff-pri tt-doff-pri.lg-faca-8  
               input browse br-doff-pri tt-doff-pri.lg-faca-9  
               INPUT BROWSE br-doff-pri tt-doff-pri.lg-faca-10 
               INPUT BROWSE br-doff-pri tt-doff-pri.lg-faca-11 
               input browse br-doff-pri tt-doff-pri.lg-faca-12 
               INPUT BROWSE br-doff-pri tt-doff-pri.lg-faca-13 
               input browse br-doff-pri tt-doff-pri.lg-faca-14 
               INPUT BROWSE br-doff-pri tt-doff-pri.lg-faca-15 
               INPUT BROWSE br-doff-pri tt-doff-pri.lg-faca-16 
               input browse br-doff-pri tt-doff-pri.qt-sobras 
               INPUT BROWSE br-doff-pri tt-doff-pri.qt-perdas    
               INPUT BROWSE br-doff-pri tt-doff-pri.obs  .      
               br-doff-pri:CREATE-RESULT-LIST-ENTRY() in frame f-relat.
    end.
    else do transaction on error undo, return no-apply:

        IF AVAIL tt-doff-pri THEN
            assign INPUT BROWSE br-doff-pri tt-doff-pri.sq-doff    
                   INPUT BROWSE br-doff-pri tt-doff-pri.qt-doff    
                   input browse br-doff-pri tt-doff-pri.qt-bobinas 
                   INPUT BROWSE br-doff-pri tt-doff-pri.lg-faca-1  
                   INPUT BROWSE br-doff-pri tt-doff-pri.lg-faca-2  
                   input browse br-doff-pri tt-doff-pri.lg-faca-3  
                   INPUT BROWSE br-doff-pri tt-doff-pri.lg-faca-4  
                   input browse br-doff-pri tt-doff-pri.lg-faca-5  
                   INPUT BROWSE br-doff-pri tt-doff-pri.lg-faca-6  
                   INPUT BROWSE br-doff-pri tt-doff-pri.lg-faca-7  
                   INPUT BROWSE br-doff-pri tt-doff-pri.lg-faca-8  
                   input browse br-doff-pri tt-doff-pri.lg-faca-9  
                   INPUT BROWSE br-doff-pri tt-doff-pri.lg-faca-10 
                   INPUT BROWSE br-doff-pri tt-doff-pri.lg-faca-11 
                   input browse br-doff-pri tt-doff-pri.lg-faca-12 
                   INPUT BROWSE br-doff-pri tt-doff-pri.lg-faca-13 
                   input browse br-doff-pri tt-doff-pri.lg-faca-14 
                   INPUT BROWSE br-doff-pri tt-doff-pri.lg-faca-15 
                   INPUT BROWSE br-doff-pri tt-doff-pri.lg-faca-16 
                   input browse br-doff-pri tt-doff-pri.qt-sobras 
                   INPUT BROWSE br-doff-pri tt-doff-pri.qt-perdas    
                   INPUT BROWSE br-doff-pri tt-doff-pri.obs.  

        IF AVAIL tt-doff-pri THEN DO:

            ASSIGN soma-larg-jr = tt-doff-pri.lg-faca-1    + 
                                  tt-doff-pri.lg-faca-2    +
                                  tt-doff-pri.lg-faca-3    +
                                  tt-doff-pri.lg-faca-4    +
                                  tt-doff-pri.lg-faca-5    +
                                  tt-doff-pri.lg-faca-6    +
                                  tt-doff-pri.lg-faca-7    +
                                  tt-doff-pri.lg-faca-8    +
                                  tt-doff-pri.lg-faca-9    +
                                  tt-doff-pri.lg-faca-10   +
                                  tt-doff-pri.lg-faca-11   +
                                  tt-doff-pri.lg-faca-12   +
                                  tt-doff-pri.lg-faca-13   +
                                  tt-doff-pri.lg-faca-14   +
                                  tt-doff-pri.lg-faca-15   +
                                  tt-doff-pri.lg-faca-16.

            IF soma-larg-jr > int(i-larg-util-pri:SCREEN-VALUE IN FRAME f-relat) THEN DO:

               run utp/ut-msgs.p (input "show":U, input 17006, "Erro na Largura do Corte Prim rio").
               APPLY "entry" TO i-nr-linha-rec IN FRAME f-relat.
               RETURN NO-APPLY.

            END.

            ASSIGN soma-larg-jr = int(i-larg-util-pri:SCREEN-VALUE IN FRAME f-relat) - soma-larg-jr.

            IF soma-larg-jr >= INT(i-larg-min-estoq:SCREEN-VALUE IN FRAME f-relat) THEN
                ASSIGN tt-doff-pri.qt-sobras =  soma-larg-jr
                       tt-doff-pri.qt-perdas =  0.
            ELSE
                ASSIGN tt-doff-pri.qt-perdas =  soma-larg-jr
                       tt-doff-pri.qt-sobras =  0.

            ASSIGN qt-bobinas-jr = 0.

            IF tt-doff-pri.lg-faca-1 > 0 THEN ASSIGN qt-bobinas-jr = qt-bobinas-jr + 1.
            IF tt-doff-pri.lg-faca-2 > 0 THEN ASSIGN qt-bobinas-jr = qt-bobinas-jr + 1.
            IF tt-doff-pri.lg-faca-3 > 0 THEN ASSIGN qt-bobinas-jr = qt-bobinas-jr + 1.
            IF tt-doff-pri.lg-faca-4 > 0 THEN ASSIGN qt-bobinas-jr = qt-bobinas-jr + 1.
            IF tt-doff-pri.lg-faca-5 > 0 THEN ASSIGN qt-bobinas-jr = qt-bobinas-jr + 1.
            IF tt-doff-pri.lg-faca-6 > 0 THEN ASSIGN qt-bobinas-jr = qt-bobinas-jr + 1.
            IF tt-doff-pri.lg-faca-7 > 0 THEN ASSIGN qt-bobinas-jr = qt-bobinas-jr + 1.
            IF tt-doff-pri.lg-faca-8 > 0 THEN ASSIGN qt-bobinas-jr = qt-bobinas-jr + 1.
            IF tt-doff-pri.lg-faca-9 > 0 THEN ASSIGN qt-bobinas-jr = qt-bobinas-jr + 1.
            IF tt-doff-pri.lg-faca-10 > 0 THEN ASSIGN qt-bobinas-jr = qt-bobinas-jr + 1.
            IF tt-doff-pri.lg-faca-11 > 0 THEN ASSIGN qt-bobinas-jr = qt-bobinas-jr + 1.
            IF tt-doff-pri.lg-faca-12 > 0 THEN ASSIGN qt-bobinas-jr = qt-bobinas-jr + 1.
            IF tt-doff-pri.lg-faca-13 > 0 THEN ASSIGN qt-bobinas-jr = qt-bobinas-jr + 1.
            IF tt-doff-pri.lg-faca-14 > 0 THEN ASSIGN qt-bobinas-jr = qt-bobinas-jr + 1.
            IF tt-doff-pri.lg-faca-15 > 0 THEN ASSIGN qt-bobinas-jr = qt-bobinas-jr + 1.
            IF tt-doff-pri.lg-faca-16 > 0 THEN ASSIGN qt-bobinas-jr = qt-bobinas-jr + 1.

            ASSIGN tt-doff-pri.qt-bobinas = qt-bobinas-jr * tt-doff-pri.qt-doff.

        END.

         IF AVAIL tt-doff-pri THEN
            display
                tt-doff-pri.sq-doff  
                tt-doff-pri.diin
                tt-doff-pri.diex
                tt-doff-pri.qt-doff     
                tt-doff-pri.qt-bobinas  
                tt-doff-pri.lg-faca-1   
                tt-doff-pri.lg-faca-2   
                tt-doff-pri.lg-faca-3   
                tt-doff-pri.lg-faca-4   
                tt-doff-pri.lg-faca-5   
                tt-doff-pri.lg-faca-6   
                tt-doff-pri.lg-faca-7   
                tt-doff-pri.lg-faca-8   
                tt-doff-pri.lg-faca-9   
                tt-doff-pri.lg-faca-10  
                tt-doff-pri.lg-faca-11  
                tt-doff-pri.lg-faca-12  
                tt-doff-pri.lg-faca-13  
                tt-doff-pri.lg-faca-14  
                tt-doff-pri.lg-faca-15  
                tt-doff-pri.lg-faca-16  
                tt-doff-pri.qt-sobras  
                tt-doff-pri.qt-perdas     
                tt-doff-pri.obs         
                with browse br-doff-pri. 
                                          
   end.
 
END.

ON END-ERROR OF C-Win
OR ENDKEY OF C-Win ANYWHERE DO:
   RETURN NO-APPLY.
END.


ON END-ERROR OF C-Win
OR ENDKEY OF C-Win ANYWHERE DO:
   RETURN NO-APPLY.
END.

ON WINDOW-CLOSE OF C-Win
DO:
   APPLY "CLOSE" TO THIS-PROCEDURE.
   RETURN NO-APPLY.
END.

ON ENDKEY OF FRAME f-relat DO:
  return no-apply.
END.


ON CHOOSE OF bt-cancela IN FRAME f-relat
DO:


   RUN pi-disable-campos.
   RUN pi-disable-bt-grava.
   RUN pi-enable-outros-botoes.
   RUN pi-limpa-campos.

   APPLY "entry" TO c-cod-estabel-amg IN FRAME f-relat.

END.



ON entry OF bt-novo IN FRAME f-relat
DO:

   RUN pi-enable-bt-cancela.

   RUN pi-disable-outros-botoes.


   RUN pi-limpa-campos. 
   
   RUN pi-mostra-campos.
   RUN pi-enable-campos.

   APPLY "entry" TO c-cod-estabel-amg IN FRAME f-relat.

END.

ON CHOOSE OF bt-novo IN FRAME f-relat
DO:

   RUN pi-enable-bt-cancela.

   RUN pi-disable-outros-botoes.


   RUN pi-limpa-campos. 
   
   RUN pi-mostra-campos.
   RUN pi-enable-campos.

   APPLY "entry" TO c-cod-estabel-amg IN FRAME f-relat.

END.

ON CHOOSE OF bt-ajuda IN FRAME f-relat
DO:


END.


ON CHOOSE OF bt-sai IN FRAME f-relat
DO:

   apply "close" to this-procedure.

END.

/* ***************************  Main Block  *************************** */


ASSIGN CURRENT-WINDOW             = {&WINDOW-NAME}
   THIS-PROCEDURE:CURRENT-WINDOW  = {&WINDOW-NAME}.

assign v-cod-prog-gerado = "escp0602".



def var c-tit as char no-undo.

ASSIGN c-tit = "escp0602 - Otimiza‡Æo de Corte de Bobinas".
assign {&WINDOW-NAME}:title = c-tit
       c-arq-old        = c-arquivo
       c-impressora-old = c-arquivo.


assign {&window-name}:virtual-width-chars  = {&window-name}:width-chars  
       {&window-name}:virtual-height-chars = {&window-name}:height-chars 
       {&window-name}:min-width-chars      = {&window-name}:width-chars  
       {&window-name}:max-width-chars      = {&window-name}:width-chars  
       {&window-name}:min-height-chars     = {&window-name}:height-chars 
       {&window-name}:max-height-chars     = {&window-name}:height-chars.
assign c-terminal = " Terminal".


ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.


def var wh-label-sel     as widget-handle no-undo.
def var wh-label-cla     as widget-handle no-undo.
def var wh-label-par     as widget-handle no-undo.
def var wh-label-dig     as widget-handle no-undo.
def var wh-label-imp     as widget-handle no-undo.
def var wh-group         as widget-handle no-undo.
def var wh-child         as widget-handle no-undo.
def var c-list-folders   as char          no-undo.
def var i-current-folder as integer       no-undo.
def var i-new-folder     as integer       no-undo.
def var c-aux            as char          no-undo.
def var i-aux            as integer       no-undo.

ON  CLOSE OF THIS-PROCEDURE DO:
    RUN disable_ui. 
END.


on  CTRL-TAB,SHIFT-CTRL-TAB anywhere do:
    define variable h_handle  as handle no-undo.       
    define variable c_imagem  as character no-undo.
    define variable l_direita as logical no-undo.            

    l_direita = last-event:label = 'CTRL-TAB'.
        
    block1:
    repeat:        
        if  l_direita then do:
            if  i-current-folder = num-entries(c-list-folders) then
                i-current-folder = 1.
            else
                i-current-folder = i-current-folder + 1.
        end.
        else do:
            if  i-current-folder = 1 then
                i-current-folder = num-entries(c-list-folders).
            else
                i-current-folder = i-current-folder - 1.
        end.
    
        assign c_imagem = entry(i-current-folder,c-list-folders)
               h_handle = frame f-relat:first-child
               h_handle = h_handle:first-child.

        do  while valid-handle(h_handle):
            if  h_handle:type = 'image' and
                h_handle:name =  c_imagem then do:
                if  h_handle:sensitive = no then 
                    next block1.
                apply 'mouse-select-click' to h_handle.
                leave block1.
            end.
            h_handle = h_handle:next-sibling.
        end.
    end.
end.



/********************************************************** 
** Procedure de troca de p gina por CTRL-TAB 
**********************************************************/
procedure pi-first-child:
        
    define input parameter wh-entry-folder as widget-handle.
    
    assign wh-entry-folder = wh-entry-folder:first-child
           wh-entry-folder = wh-entry-folder:first-child.
    do  while(valid-handle(wh-entry-folder)):
        if  wh-entry-folder:sensitive = yes 
        and wh-entry-folder:type <> 'rectangle' 
        and wh-entry-folder:type <> 'image'
        and wh-entry-folder:type <> 'browse' then do:
            apply 'entry' to wh-entry-folder.
            leave.
        end.
        else
            assign wh-entry-folder = wh-entry-folder:next-sibling.    
    end.
    
end.


/* define procedure externa para execucao do programa de visualizacao do relatorio */

PROCEDURE WinExec EXTERNAL "kernel32.dll":
  DEF INPUT  PARAM prg_name                          AS CHARACTER.
  DEF INPUT  PARAM prg_style                         AS SHORT.
END PROCEDURE.



PAUSE 0 BEFORE-HIDE.

MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    IF SESSION:SET-WAIT-STATE("":U) THEN.
    RUN enable_UI.

    RUN pi-disable-bt-grava.   


    assign v-cod-pg-mouse-selec = "im-pg-sel".


     view c-win.
     apply "entry" to frame f-Relat.
     apply "entry" to c-win.



    /* ----------------------------- */

   IF  NOT THIS-PROCEDURE:PERSISTENT THEN
      WAIT-FOR CLOSE OF THIS-PROCEDURE.


END.
/* **********************  Internal Procedures  *********************** */

PROCEDURE adm-row-available :
   /* Define variables needed by this internal procedure.             */
  /* Process the newly available records (i.e. display fields, 
     open queries, and/or pass records on to any RECORD-TARGETS).    */
END PROCEDURE.

PROCEDURE disable_UI :
   IF SESSION:DISPLAY-TYPE = "GUI" AND VALID-HANDLE(C-Win)
   THEN DELETE WIDGET C-Win.
   IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

PROCEDURE enable_UI :
   ENABLE  
          bt-novo
          bt-cancela
          bt-sai
          bt-ajuda
           
   WITH FRAME f-relat IN WINDOW C-Win.

   {&OPEN-BROWSERS-IN-QUERY-f-relat}
   {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}


  ENABLE br-pedido br-resumo-ped  
      WITH FRAME f-relat IN WINDOW c-win.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}



  ENABLE br-doff-sec  br-doff-met br-doff-pri 
      WITH FRAME f-relat IN WINDOW c-win.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
   
  ENABLE br-doff-sec   
      WITH FRAME f-relat IN WINDOW c-win.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}

  VIEW C-Win.

END PROCEDURE.

PROCEDURE local-exit :
   APPLY "CLOSE" TO THIS-PROCEDURE.
   RETURN.
END PROCEDURE.  

PROCEDURE pi-troca-pagina:


END PROCEDURE.

PROCEDURE send-records :
    /* Define variables needed by this internal procedure.               */ 
    /* For each requested table, put it':Us ROWID in the output list.      */
    /* Deal with any unexpected table requests before closing.           */ 
END PROCEDURE.

PROCEDURE state-changed :
    DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
    run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

Procedure pi-muda-cor-label-folder:
   def input parameter p-cod-label as char  no-undo.
   def var wh-pai   as widget-handle.
   def var wh-filho as widget-handle.

    assign wh-pai = frame f-relat:handle
           wh-pai = wh-pai:first-child.
   do while wh-pai <> ?:
       do  while valid-handle(wh-pai):
           assign wh-filho = wh-pai:first-child.
           do  while valid-handle(wh-filho):
               if  wh-filho:type = "TEXT"
                   then
                       if  wh-filho:screen-value = p-cod-label
                       then
                           assign wh-filho:fgcolor = 7.
                       assign wh-filho = wh-filho:next-sibling.
           end.
           assign wh-pai = wh-pai:next-sibling.
       end.
   end.

END PROCEDURE.

PROCEDURE OpenDocument:

    def input param c-doc as char  no-undo.
    def var c-exec as char  no-undo.
    def var h-Inst as int  no-undo.

    assign c-exec = fill("x",255).
    run FindExecutableA (input c-doc,
                         input "",
                         input-output c-exec,
                         output h-inst).

    if h-inst >= 0 and h-inst <=32 then
      run ShellExecuteA (input 0,
                         input "open",
                         input "rundll32.exe",
                         input "shell32.dll,OpenAs_RunDLL " + c-doc,
                         input "",
                         input 1,
                         output h-inst).

    run ShellExecuteA (input 0,
                       input "open",
                       input c-doc,
                       input "",
                       input "",
                       input 1,
                       output h-inst).

    if h-inst < 0 or h-inst > 32 then return "OK".
    else return "NOK".

END PROCEDURE.

PROCEDURE FindExecutableA EXTERNAL "Shell32.dll" persistent:

    define input parameter lpFile as char  no-undo.
    define input parameter lpDirectory as char  no-undo.
    define input-output parameter lpResult as char  no-undo.
    define return parameter hInstance as long.

END.

PROCEDURE ShellExecuteA EXTERNAL "Shell32.dll" persistent:

    define input parameter hwnd as long.
    define input parameter lpOperation as char  no-undo.
    define input parameter lpFile as char  no-undo.
    define input parameter lpParameters as char  no-undo.
    define input parameter lpDirectory as char  no-undo.
    define input parameter nShowCmd as long.
    define return parameter hInstance as long.

END PROCEDURE.

PROCEDURE pi-mostra-registro.


END PROCEDURE.

PROCEDURE pi-le-primeiro.


END PROCEDURE.


PROCEDURE pi-le-proximo.

    

END PROCEDURE.


PROCEDURE pi-le-anterior.

    

END PROCEDURE.

PROCEDURE pi-le-ultimo.

    

END PROCEDURE.



PROCEDURE le-registro-goto.
    
      
END PROCEDURE.


PROCEDURE pi-limpa-campos.


    FOR EACH tt-doff-sec.
        DELETE tt-doff-sec. 
    END. 

    CLOSE QUERY br-doff-sec.
    open query br-doff-sec for each tt-doff-sec.
    
    if num-results("br-doff-sec") > 0 THEN DO:
       get current br-doff-sec.
    END.

    FOR EACH tt-doff-met.
        DELETE tt-doff-met. 
    END. 

    CLOSE QUERY br-doff-met.
    open query br-doff-met for each tt-doff-met.
    
    if num-results("br-doff-met") > 0 THEN DO:
       get current br-doff-met.
    END.

    FOR EACH tt-doff-pri.
        DELETE tt-doff-pri. 
    END. 

    CLOSE QUERY br-doff-pri.
    open query br-doff-pri for each tt-doff-pri.
    
    if num-results("br-doff-pri") > 0 THEN DO:
       get current br-doff-pri.
    END.

    FOR EACH tt-pedido.
        DELETE tt-pedido. 
    END. 

    CLOSE QUERY br-pedido.
    open query br-pedido for each tt-pedido.
    
    if num-results("br-pedido") > 0 THEN DO:
       get current br-pedido.
    END.

    FOR EACH tt-resumo-ped.
        DELETE tt-resumo-ped. 
    END. 

    CLOSE QUERY br-resumo-ped.
    open query br-resumo-ped for each tt-resumo-ped.
    
    if num-results("br-resumo-ped") > 0 THEN DO:
       get current br-resumo-ped.
    END.

END PROCEDURE.

PROCEDURE pi-enable-campos.

    ENABLE c-cod-estabel-amg
        
           c-it-codigo-ini

           dt-entrega-ini
           dt-entrega-fim
           c-nome-abrev-ini
           c-nome-abrev-fim

           dt-inic-prod
           dt-fim-prod

           i-nr-linha-rec
           i-larg-util-rec-min
           i-larg-util-rec-max
           i-larg-min-estoq
           i-diin-util-rec
           i-nr-linha-met
           i-diin-prim
           i-diex-prim
           i-nr-linha-pri
           d-perc-perda-bob
           i-larg-util-pri

           c-it-codigo-met
           c-it-codigo-pri
           c-it-codigo-mr
           bt-add
           bt-inclui-larg
           bt-del
           bt-conjuga
           bt-inclui-met
           bt-exclui-met
           bt-conjuga-pri
           bt-cancela2
           bt-consulta
           bt-exclui-ped 
           bt-agrupa-larg 
           bt-gera-ord
           bt-filtro 

        WITH FRAME f-relat.

    APPLY "entry" TO c-cod-estabel-amg IN FRAME f-relat.


END PROCEDURE.

PROCEDURE pi-mostra-campos.

   DISPLAY c-cod-estabel-amg
        
           c-cod-estabel-amg    
                                
           c-it-codigo-ini      
                                
           dt-entrega-ini       
           dt-entrega-fim       
           c-nome-abrev-ini     
           c-nome-abrev-fim     
                                
           dt-inic-prod         
           dt-fim-prod          
                                
           i-nr-linha-rec   
           i-larg-util-rec-min
           i-larg-util-rec-max 
           c-desc-linha-rec
           i-larg-min-estoq
           i-diin-util-rec     
           i-nr-linha-met
           i-diin-prim
           i-diex-prim
           i-nr-linha-pri
           d-perc-perda-bob
           i-larg-util-pri

           c-it-codigo-met
           c-it-codigo-pri
           c-it-codigo-mr

           bt-add
           bt-inclui-larg
           bt-del 
           bt-conjuga
           bt-inclui-met
           bt-exclui-met
           bt-conjuga-pri

           bt-cancela2
           bt-consulta
           bt-exclui-ped 
           bt-agrupa-larg 
           bt-gera-ord
           bt-filtro 

        WITH FRAME f-relat.

END PROCEDURE.


PROCEDURE pi-disable-campos.


END PROCEDURE.

PROCEDURE pi-enable-todos-campos.


END PROCEDURE.



PROCEDURE pi-le-pela-chave.

END PROCEDURE.


PROCEDURE pi-grava-registro.

END PROCEDURE.

PROCEDURE pi-disable-bt-grava.

    assign /*bt-grava:SENSITIVE in frame f-relat   = no */
           bt-cancela:SENSITIVE in frame f-relat = no.

END PROCEDURE.

PROCEDURE pi-enable-bt-cancela.

    ENABLE bt-cancela
        WITH FRAME f-relat.

END PROCEDURE.

PROCEDURE pi-disable-outros-botoes.
   ASSIGN bt-novo:SENSITIVE in frame f-relat       = no. 
   
END PROCEDURE.

PROCEDURE pi-enable-outros-botoes.

   ENABLE bt-cancela  
          bt-sai
          bt-sai
   WITH FRAME f-relat IN WINDOW C-Win.
   
END PROCEDURE.

PROCEDURE pi-monta-browse-ped:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR de-peso-bobs AS DEC NO-UNDO.
    DEF VAR i-nr-bobinas AS DEC NO-UNDO.
    DEF VAR uc-nr-pedido LIKE  ped-venda.nr-pedido.
    DEF VAR uc-nome-abrev LIKE  ped-venda.nome-abrev.
    DEF VAR uc-cod-estabel LIKE  ped-venda.cod-estabel.
          
Do:

     /** Montando a tabela para tt-doff-sec ***/

    FOR EACH tt-pedido.
        DELETE tt-pedido.
    END. 

    FOR EACH tt-doff-sec.
        DELETE tt-doff-sec.
    END.

    FOR EACH tt-resumo-ped.
        DELETE tt-resumo-ped.
    END.

    ASSIGN seq-sel-jr = 0.

    CLOSE QUERY br-pedido.


    open query br-pedido for each tt-pedido.
    apply 'entry' to tt-pedido.nr-pedido in browse br-pedido. 

    if num-results("br-pedido") > 0 THEN DO:
       get current br-pedido.
    END.


    open query br-doff-sec for each tt-doff-sec.
    apply 'entry' to tt-doff-sec.sq-doff in browse br-doff-sec. 

    if num-results("br-doff-sec") > 0 THEN DO:
       get current br-doff-sec.
    END.

    open query br-resumo-ped for each tt-resumo-ped.
    apply 'entry' to tt-resumo-ped.seq in browse br-resumo-ped. 

    if num-results("br-resumo-ped") > 0 THEN DO:
       get current br-resumo-ped.
    END.



    run utp/ut-acomp.p persistent set h-acomp.

    run pi-inicializar in h-acomp(input "Lendo Carteira de Pedidos").

    assign v-num-reg-lidos = 0.
    
      EMPTY temp-tabLE tt-peduc.


    FOR EACH  ped-item
       WHERE 
             ped-item.it-codigo     = string(c-it-codigo-ini:SCREEN-VALUE IN FRAME f-relat)    AND
             ped-item.dt-entrega >= date(dt-entrega-ini:SCREEN-VALUE IN FRAME f-relat)     AND
             ped-item.dt-entrega <= date(dt-entrega-fim:SCREEN-VALUE IN FRAME f-relat)     AND
             ped-item.ind-componen <> 3                     AND
             ped-item.cod-sit-item <= 2                     AND
            (ped-item.qt-pedida - ped-item.qt-atendida) > 0 NO-LOCK,
       FIRST ped-venda OF ped-item
       WHERE ped-venda.cod-estabel  = string(c-cod-estabel-amg:SCREEN-VALUE IN FRAME f-relat)   AND
            
             ped-venda.cod-sit-ped <= 2               NO-LOCK.
        
        assign v-num-reg-lidos = v-num-reg-lidos + 1.
        run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).


       uc-nr-pedido = ped-venda.nr-pedido.
       uc-nome-abrev = ped-venda.nome-abrev.
       uc-cod-estabel = ped-venda.cod-estabel.


       FIND FIRST if-ped-venda WHERE if-ped-venda.nr-pedido-relac = ped-venda.nr-pedido NO-LOCK NO-ERROR.

      IF AVAIL if-ped-venda THEN NEXT.


       FIND FIRST if-ped-venda WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.

       IF AVAIL if-ped-venda THEN DO:


           FIND FIRST b-ped-venda WHERE b-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac NO-LOCK NO-ERROR.

           IF NOT AVAIL b-ped-venda THEN NEXT.

           uc-nr-pedido = ped-venda.nr-pedido.
           uc-nome-abrev = b-ped-venda.nome-abrev.
           uc-cod-estabel = if-ped-venda.cod-estab-atend.


       END.

 
       IF NOT( uc-nome-abrev   >=  string(c-nome-abrev-ini:SCREEN-VALUE IN FRAME f-relat)             AND
               uc-nome-abrev   <=  string(c-nome-abrev-fim:SCREEN-VALUE IN FRAME f-relat)   )          THEN NEXT.


       FIND FIRST tt-peduc WHERE tt-peduc.nr-pedido = uc-nr-pedido NO-ERROR.

       IF NOT AVAIL tt-peduc THEN DO:
           CREATE tt-peduc.
           ASSIGN 
               tt-peduc.nr-pedido = uc-nr-pedido
               tt-peduc.nome-abrev = uc-nome-abrev
               tt-peduc.cod-estabel = uc-cod-estabel.
       END.


 
END.



     FOR EACH tt-peduc ,
        FIRST ped-venda WHERE  
             ped-venda.nr-pedido   = tt-peduc.nr-pedido no-lock,
 

   
        EACH ped-item OF ped-venda WHERE
             ped-item.dt-entrega >= date(dt-entrega-ini:SCREEN-VALUE IN FRAME f-relat)     AND
             ped-item.dt-entrega <= date(dt-entrega-fim:SCREEN-VALUE IN FRAME f-relat)     AND

             ped-item.it-codigo = string(c-it-codigo-ini:SCREEN-VALUE IN FRAME f-relat)  AND
             ped-item.cod-sit-item < 3 AND
             ped-item.ind-componen <> 3
             NO-LOCK.
      
      
      find last ord-prod where 
       ord-prod.nome-abrev    = ped-venda.nome-abrev and
       ord-prod.nr-pedido     = ped-venda.nr-pedcli  and
       ord-prod.nr-sequencia  = ped-item.nr-sequencia and
       ord-prod.it-codigo     = ped-item.it-codigo  no-lock no-error.
      
    
      if avail ord-prod  and l-exclui then next.
      

      
        assign v-num-reg-lidos = v-num-reg-lidos + 1.
        run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

        ASSIGN larg-jr       = 0
               diex-jr       = 0
               diin-jr       = 0
               peso-bob-jr   = 0
               qt-bobinas-jr = 0.


        FIND FIRST var-result WHERE var-result.nome-var = "Largura" AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)           AND
             var-result.item-cotacao = ped-item.it-codigo 
             /*var-result.sequencia  = ped-item.nr-sequencia */
             NO-LOCK NO-ERROR.
        
        IF AVAIL var-result THEN
             ASSIGN larg-jr = var-result.valor-dec.
        
        FIND FIRST var-result WHERE var-result.nome-var = "qtdbob" AND 
             var-result.nr-estrut = int(ped-item.cod-refer)             AND
             var-result.item-cotacao = ped-item.it-codigo 
             /*var-result.sequencia = ped-item.nr-sequencia */
             NO-LOCK NO-ERROR.
        
        IF AVAIL var-result THEN
             ASSIGN qt-bobinas-jr = var-result.valor-dec.
        
        
        FIND FIRST var-result WHERE var-result.nome-var = "pesobob" AND 
             var-result.nr-estrut = int(ped-item.cod-refer)             AND
             var-result.item-cotacao = ped-item.it-codigo 
             /*var-result.sequencia = ped-item.nr-sequencia */
             NO-LOCK NO-ERROR.
        
        IF AVAIL var-result THEN
             ASSIGN peso-bob-jr = var-result.valor-dec.
        
        FIND FIRST var-result WHERE var-result.nome-var = "DIEX" AND 
             var-result.nr-estrut = int(ped-item.cod-refer)           AND
             var-result.item-cotacao = ped-item.it-codigo 
             /*var-result.sequencia = ped-item.nr-sequencia*/ 
             NO-LOCK NO-ERROR.
        
        IF AVAIL var-result THEN
             ASSIGN diex-jr = var-result.valor-dec.

        FIND FIRST var-result WHERE var-result.nome-var = "DIIN" AND 
             var-result.nr-estrut = int(ped-item.cod-refer)           AND
             var-result.item-cotacao = ped-item.it-codigo 
             /*var-result.sequencia = ped-item.nr-sequencia*/ 
             NO-LOCK NO-ERROR.
        
        IF AVAIL var-result THEN
             ASSIGN diin-jr = var-result.valor-dec.

        IF diin-jr <> i-diin-jr OR diex-jr <> i-diex-jr THEN NEXT.

        ASSIGN qt-bobinas-plt = 0.

        FOR EACH pallet WHERE
            pallet.nr-pedido = ped-venda.nr-pedido       and
            pallet.nr-sequencia = ped-item.nr-sequencia
            USE-INDEX pedido NO-LOCK.

            ASSIGN qt-bobinas-plt = qt-bobinas-plt + pallet.nr-bobinas.

        END.

        ASSIGN qt-bobinas-jr = qt-bobinas-jr - qt-bobinas-plt.

        IF qt-bobinas-jr < 1 THEN NEXT.

        CREATE tt-pedido.
        
        ASSIGN tt-pedido.nr-pedido       = ped-venda.nr-pedido
               tt-pedido.nr-sequencia    = ped-item.nr-sequencia   
               tt-pedido.tp-pedido       = ped-venda.tp-pedido    
               tt-pedido.it-codigo       = ped-item.it-codigo 
               tt-pedido.qt-pedida       = ped-item.qt-pedida   
               tt-pedido.qt-bobinas      = qt-bobinas-jr    
               tt-pedido.larg            = larg-jr      
               tt-pedido.diex            = diex-jr  
               tt-pedido.diin            = diin-jr 
               tt-pedido.peso-bob        = peso-bob-jr
               tt-pedido.cod-refer       = ped-item.cod-refer        
               tt-pedido.nome-abrev      = ped-venda.nome-abrev   
               tt-pedido.nome-abrev-fim  =  tt-peduc.nome-abrev
               tt-pedido.dt-entrega      = ped-item.dt-entrega
               tt-pedido.nr-ord-produ    = if avail ord-prod then ord-prod.nr-ord-produ else 0. 

    END. 

       /*** habilita **/

     open query br-pedido for each tt-pedido.
     apply 'entry' to tt-pedido.nr-pedido in browse br-pedido. 

     if num-results("br-pedido") > 0 THEN DO:
        get current br-pedido.
     END.

  end. 

  run pi-finalizar in h-acomp.

END PROCEDURE.
  
PROCEDURE pi-monta-browse-2:

Do:

     /** Montando a tabela para tt-doff-sec ***/

    FOR EACH tt-doff-sec.
        DELETE tt-doff-sec.
    END.

    CLOSE QUERY br-doff-sec.


       /*** habilita **/

     open query br-doff-sec for each tt-doff-sec.

     apply 'entry' to tt-doff-sec.obs in browse br-doff-sec. 

     if num-results("br-doff-sec") > 0 THEN DO:
        get current br-doff-sec.
     END.

  end. 

END PROCEDURE.

PROCEDURE pi-conjuga-bob.

   FOR EACH tt-doff-rot.
       DELETE tt-doff-rot.
   END.

   FIND FIRST tt-bobinas NO-LOCK NO-ERROR.   /* Lˆ uma conjuga‡Æo */

   IF AVAIL tt-bobinas THEN DO:

    ASSIGN i-jr           = 0
           qtd-bob-jr     = 0.

    DO WHILE i-jr < 16.  /* acha qtde de bobinas */

        ASSIGN i-jr = i-jr + 1.

        IF tt-bobinas.qtd-bob [i-jr] <= 0 THEN NEXT.

         ASSIGN qtd-bob-jr = qtd-bob-jr + tt-bobinas.qtd-bob [i-jr].

    END.

    IF qtd-bob-jr > 0 THEN DO: 

        ASSIGN menor-larg-jr = 9999999
               i-jr          = 0.

        DO WHILE i-jr < 16.  /* procura menor largura */

            ASSIGN i-jr = i-jr + 1.

            IF tt-bobinas.qtd-bob [i-jr] <= 0 THEN NEXT.

            IF tt-bobinas.larg [i-jr] = 0 THEN LEAVE.

            IF tt-bobinas.larg [i-jr] < menor-larg-jr THEN
                ASSIGN menor-larg-jr = tt-bobinas.larg [i-jr].

        END.

        ASSIGN max-facas-jr = i-larg-util-rot-max / menor-larg-jr
               max-facas-jr = TRUNCATE(max-facas-jr,0).

        IF max-facas-jr > qtd-bob-jr THEN
            ASSIGN  max-facas-jr = qtd-bob-jr.
        

        IF max-facas-jr < 1 OR tt-bobinas.qtd-larg < 1 THEN DO:

            run utp/ut-msgs.p (input "show":U, input 17006, "Erro na Largura étil da Linha de Recorte").
            APPLY "entry" TO i-nr-linha-rec IN FRAME f-relat.
            RETURN NO-APPLY.

        END.

        ASSIGN i-rnd        = 0
               var-rnd-1000  =  "".

        DO WHILE i-rnd < 1000.

            ASSIGN i-rnd = i-rnd + 1.

            ASSIGN i-jr    = 0
                   var-rnd = ""
                   testa-qt-larg = 0.

            DO WHILE i-jr < max-facas-jr.

                ASSIGN i-jr = i-jr + 1.

                IF tt-bobinas.qtd-larg > 1 THEN
                   ASSIGN int-rnd = RANDOM (1 , tt-bobinas.qtd-larg).
                ELSE
                   ASSIGN int-rnd = 1.

                IF SUBSTRING(var-rnd,((i-jr * 4) - 3),4) = "" THEN
                    ASSIGN SUBSTRING(var-rnd,((i-jr * 4) - 3),4) = string(tt-bobinas.larg [int-rnd],"9999").

                ASSIGN testa-qt-larg [int-rnd] = testa-qt-larg [int-rnd] + 1.

                ASSIGN i-idx = 0
                       soma-larg-jr = 0.

                DO WHILE i-idx <= i-jr.
                    ASSIGN i-idx = i-idx + 1.
                           soma-larg-jr = soma-larg-jr + int(SUBSTRING (var-rnd , (i-idx * 4 - 3), 4)).
                END.

                IF (soma-larg-jr > i-larg-util-rot-max) OR
                   (testa-qt-larg [int-rnd] > tt-bobinas.qtd-bob [int-rnd]) THEN DO:

                    ASSIGN SUBSTRING (var-rnd , (i-jr * 4 - 3), 4) = "0000"
                           i-jr = i-jr - 1
                           testa-qt-larg [int-rnd] = testa-qt-larg [int-rnd] - 1.

                END.

            END.

            ASSIGN var-rnd-1000 [i-rnd] = var-rnd .

        END.

        ASSIGN i-rnd = 0.

        DO WHILE i-rnd < 1000.

            ASSIGN i-rnd = i-rnd + 1.

            IF  var-rnd-1000 [i-rnd] = "" THEN NEXT.

            FIND FIRST tt-conjuga-1 WHERE
                tt-conjuga-1.diin  = tt-bobinas.diin AND 
                tt-conjuga-1.diex  = tt-bobinas.diex AND 
                tt-conjuga-1.chave = var-rnd-1000 [i-rnd]
                NO-ERROR.

            IF NOT AVAIL tt-conjuga-1 THEN DO:

                CREATE tt-conjuga-1.

                ASSIGN tt-conjuga-1.diin  = tt-bobinas.diin  
                       tt-conjuga-1.diex  = tt-bobinas.diex 
                       tt-conjuga-1.chave = var-rnd-1000 [i-rnd] .

            END.

            ASSIGN i-jr = 0
                   soma-larg-jr = 0.

            ASSIGN i-jr-10 = 0.   

            DO WHILE i-jr < 16.

                ASSIGN i-jr = i-jr + 1.

                IF int(SUBSTRING (tt-conjuga-1.chave , (i-jr * 4 - 3), 4)) <> 0 THEN   DO:   

                    ASSIGN i-jr-10 = i-jr-10 + 1.

                    ASSIGN tt-conjuga-1.larg [i-jr-10] = int(SUBSTRING (tt-conjuga-1.chave , (i-jr * 4 - 3), 4))
                          soma-larg-jr = soma-larg-jr + int(SUBSTRING (tt-conjuga-1.chave , (i-jr * 4 - 3), 4)).

                END.

            END.

            ASSIGN tt-conjuga-1.sobra = i-larg-util-rot-max - soma-larg-jr.


            IF tt-conjuga-1.sobra < 0 THEN DELETE tt-conjuga-1.


        END.

        FOR EACH tt-conjuga-1 NO-LOCK
            BY tt-conjuga-1.sobra.

            ASSIGN larg-bob-x = 0
                   qt-bob-x   = 0
                   i-jr       = 0.

            DO WHILE i-jr < 16.
                assign i-jr = i-jr + 1.

                IF tt-conjuga-1.larg [i-jr] = 0 THEN NEXT.


                ASSIGN i-idx = 0.

                DO WHILE i-idx < 16.
                    ASSIGN i-idx = i-idx + 1.

                    IF larg-bob-x [i-idx] = 0 or
                       larg-bob-x [i-idx] = tt-conjuga-1.larg [i-jr] THEN DO:

                        ASSIGN larg-bob-x [i-idx] = tt-conjuga-1.larg [i-jr]
                               qt-bob-x [i-idx] = qt-bob-x [i-idx] + 1.

                        LEAVE.

                    END.

                END.

            END.

            /*      
              Acha a menor quantidade de bobinas para saber 
                   quantos doffs serÆo gerados.
            */      

            ASSIGN i-idx         = 0
                   menor-doff-jr = 99999999
                   qtd-bob-jr    = 0.

            DO WHILE i-idx < 16.
                ASSIGN i-idx = i-idx + 1.

                IF larg-bob-x [i-idx] = 0 OR qt-bob-x [i-idx] = 0 THEN NEXT.

                ASSIGN qtd-bob-jr = qtd-bob-jr + qt-bob-x [i-idx].

                ASSIGN i-jr = 0.

                DO WHILE i-jr < 16.
                   assign i-jr = i-jr + 1.

                   IF tt-bobinas.larg [i-jr] = 0 OR tt-bobinas.qtd-bob [i-jr] = 0 THEN NEXT.

                   IF larg-bob-x [i-idx] = tt-bobinas.larg [i-jr]  THEN DO:

                       ASSIGN i-doff-jr =  TRUNCATE((tt-bobinas.qtd-bob [i-jr] / qt-bob-x [i-idx]),0).

                       IF i-doff-jr < menor-doff-jr THEN
                           ASSIGN menor-doff-jr = i-doff-jr.

                   END.

                END.

            END.

            ASSIGN sq-doff-jr = sq-doff-jr + 1.

            CREATE tt-doff-rot.

            ASSIGN tt-doff-rot.sq-doff     =  sq-doff-jr 
                   tt-doff-rot.diin        =  tt-bobinas.diin
                   tt-doff-rot.diex        =  tt-bobinas.diex
                   tt-doff-rot.qt-doff     =  menor-doff-jr
                   tt-doff-rot.qt-bobinas  =  qtd-bob-jr * menor-doff-jr 
                   tt-doff-rot.lg-faca-1   =  tt-conjuga-1.larg [1]
                   tt-doff-rot.lg-faca-2   =  tt-conjuga-1.larg [2]
                   tt-doff-rot.lg-faca-3   =  tt-conjuga-1.larg [3]
                   tt-doff-rot.lg-faca-4   =  tt-conjuga-1.larg [4]
                   tt-doff-rot.lg-faca-5   =  tt-conjuga-1.larg [5]
                   tt-doff-rot.lg-faca-6   =  tt-conjuga-1.larg [6]
                   tt-doff-rot.lg-faca-7   =  tt-conjuga-1.larg [7]
                   tt-doff-rot.lg-faca-8   =  tt-conjuga-1.larg [8]
                   tt-doff-rot.lg-faca-9   =  tt-conjuga-1.larg [9]
                   tt-doff-rot.lg-faca-10  =  tt-conjuga-1.larg [10]
                   tt-doff-rot.lg-faca-11  =  tt-conjuga-1.larg [11]
                   tt-doff-rot.lg-faca-12  =  tt-conjuga-1.larg [12]
                   tt-doff-rot.lg-faca-13  =  tt-conjuga-1.larg [13]
                   tt-doff-rot.lg-faca-14  =  tt-conjuga-1.larg [14]
                   tt-doff-rot.lg-faca-15  =  tt-conjuga-1.larg [15]
                   tt-doff-rot.lg-faca-16  =  tt-conjuga-1.larg [16]
                   tt-doff-rot.obs         = "".

                   ASSIGN soma-larg-jr = tt-conjuga-1.larg [1]  +
                                         tt-conjuga-1.larg [2]  +
                                         tt-conjuga-1.larg [3]  +
                                         tt-conjuga-1.larg [4]  +
                                         tt-conjuga-1.larg [5]  +
                                         tt-conjuga-1.larg [6]  +
                                         tt-conjuga-1.larg [7]  +
                                         tt-conjuga-1.larg [8]  +
                                         tt-conjuga-1.larg [9]  +
                                         tt-conjuga-1.larg [10] +
                                         tt-conjuga-1.larg [11] +
                                         tt-conjuga-1.larg [12] +
                                         tt-conjuga-1.larg [13] +
                                         tt-conjuga-1.larg [14] +
                                         tt-conjuga-1.larg [15] +
                                         tt-conjuga-1.larg [16].

                   ASSIGN larg-orig-jr = i-larg-util-rot-max - (i-larg-util-rot-max - soma-larg-jr).

                   IF larg-orig-jr < i-larg-util-rot-min THEN
                       ASSIGN larg-orig-jr = i-larg-util-rot-min.

                   ASSIGN tt-doff-rot.lg-origem = larg-orig-jr.

                   ASSIGN soma-larg-jr = larg-orig-jr - soma-larg-jr.

                   IF soma-larg-jr >= INT(i-larg-min-estoq:SCREEN-VALUE IN FRAME f-relat) THEN
                       ASSIGN tt-doff-rot.qt-sobras =  soma-larg-jr.
                   ELSE
                       ASSIGN tt-doff-rot.qt-perdas =  soma-larg-jr.

           
            /* Diminui as bobinas j  conjugadas para fazer nova conjuga‡Æo */

            ASSIGN i-jr = 0.

            DO WHILE i-jr < 16.
                assign i-jr = i-jr + 1.

                IF tt-conjuga-1.larg [i-jr] = 0 THEN NEXT.

                ASSIGN i-idx = 0.

                DO WHILE i-idx < 16.
                   ASSIGN i-idx = i-idx + 1.

                   IF tt-bobinas.larg [i-idx] <> tt-conjuga-1.larg [i-jr] THEN NEXT.

                   ASSIGN tt-bobinas.qtd-bob [i-idx] = tt-bobinas.qtd-bob [i-idx] - tt-doff-rot.qt-doff.

                   FIND FIRST tt-resumo-ped WHERE
                        tt-resumo-ped.diin = tt-doff-rot.diin            AND
                        tt-resumo-ped.diex = tt-doff-rot.diex            AND
                        tt-resumo-ped.larg = tt-bobinas.larg [i-idx]
                       NO-ERROR.

                   IF AVAIL tt-resumo-ped THEN
                      ASSIGN tt-resumo-ped.qt-bobinas = tt-resumo-ped.qt-bobinas - tt-doff-rot.qt-doff.

                   IF AVAIL tt-resumo-ped AND tt-resumo-ped.qt-bobinas <= 0 THEN 
                         DELETE tt-resumo-ped.

                END.

            END.

            LEAVE.     /* sai do for each tt-conjuga-1 */

        END.   /* FOR EACH tt-conjuga-1 */

    END. /* IF qtd-bob-jr > 0  */

   END.    /* if avail tt-bobinas */


END PROCEDURE.


PROCEDURE pi-seleciona-pedidos.

    FOR EACH tt-doff-sec.
        DELETE tt-doff-sec. 
    END. 

    FOR EACH tt-doff-met.
        DELETE tt-doff-met. 
    END. 

    FOR EACH tt-doff-pri.
        DELETE tt-doff-pri. 
    END. 

    CLOSE QUERY br-doff-met.
    open query br-doff-met for each tt-doff-met.
    
    if num-results("br-doff-met") > 0 THEN DO:
       get current br-doff-met.
    END.

    FOR EACH tt-doff-pri.
        DELETE tt-doff-pri. 
    END. 

    CLOSE QUERY br-doff-pri.
    open query br-doff-pri for each tt-doff-pri.
    
    if num-results("br-doff-pri") > 0 THEN DO:
       get current br-doff-pri.
    END.

    CLOSE query br-doff-sec.
    open query br-doff-sec for each tt-doff-sec.

    if num-results("br-doff-sec") > 0 THEN DO:
       get current br-doff-sec.
    END.

    FOR EACH tt-pedido.
        assign tt-pedido.seq-resumo = 0.
    END.

    FOR EACH tt-resumo-ped.
        DELETE tt-resumo-ped.
    END.

    ASSIGN seq-jr = 0.

    FOR EACH tt-pedido WHERE
        tt-pedido.diex = i-diex-jr AND
        tt-pedido.diin = i-diin-jr .

        FIND FIRST tt-resumo-ped WHERE
            tt-resumo-ped.diex = tt-pedido.diex AND
            tt-resumo-ped.diin = tt-pedido.diin AND
            tt-resumo-ped.larg = tt-pedido.larg
            NO-ERROR.

        IF NOT AVAIL tt-resumo-ped THEN DO:

            ASSIGN seq-jr = seq-jr + 1.

            CREATE tt-resumo-ped.

            ASSIGN tt-resumo-ped.seq      = seq-jr
                   tt-resumo-ped.larg     = tt-pedido.larg
                   tt-resumo-ped.diin     = tt-pedido.diin 
                   tt-resumo-ped.diex     = tt-pedido.diex
                   tt-resumo-ped.peso-bob = tt-pedido.peso-bob.

        END.

        ASSIGN tt-resumo-ped.it-codigo  = tt-pedido.it-codigo
               tt-resumo-ped.qt-bobinas = tt-resumo-ped.qt-bobinas + tt-pedido.qt-bobinas.

        ASSIGN tt-pedido.seq-resumo = seq-jr.

    END.


    CLOSE query br-resumo-ped.
    open query br-resumo-ped for each tt-resumo-ped.

    if num-results("br-resumo-ped") > 0 THEN DO:
       get current br-resumo-ped.
    END.


    CLOSE query br-pedido.
    open query br-pedido for each tt-pedido.

    if num-results("br-pedido") > 0 THEN DO:
       get current br-pedido.
    END.

END PROCEDURE.


PROCEDURE pi-inclui-larg.
    
    DEFINE BUTTON gt2-bt-cancel AUTO-END-KEY 
         LABEL "&Cancelar" 
         SIZE 10 BY 1
         BGCOLOR 8.
    
    DEFINE BUTTON gt2-bt-ok AUTO-GO 
         LABEL "&OK" 
         SIZE 10 BY 1
         BGCOLOR 8.
    
    DEFINE RECTANGLE gt2-rt-botoes
         EDGE-PIXELS 2 GRAPHIC-EDGE  
         SIZE 60 BY 1.42
         BGCOLOR 7.

    DEFINE VARIABLE i-diin-gt2 AS INTEGER NO-UNDO.
    DEFINE VARIABLE i-diex-gt2 AS INTEGER NO-UNDO.
    DEFINE VARIABLE i-larg-gt2 AS INTEGER NO-UNDO.

    DEFINE VARIABLE i-qt-bobinas-gt2 AS INTEGER    NO-UNDO.
    DEFINE VARIABLE i-sequencia-gt2  AS INTEGER    NO-UNDO.
    DEFINE VARIABLE c-it-codigo-gt2  AS CHARACTER  NO-UNDO.
    
    DEFINE RECTANGLE gt2-rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59 BY 7.
    
    DEFINE FRAME gt2-frame-1

        i-diin-gt2 LABEL "D.Interno" AT ROW 1.5 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 08 BY .88
        
        i-diex-gt2 LABEL "D.Externo" AT ROW 2.5 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 08 BY .88
        
        c-it-codigo-gt2 LABEL "Item" AT ROW 3.5 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 16 BY .88
        
        i-sequencia-gt2 LABEL "Sequencia" AT ROW 4.5 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 08 BY .88
        
        i-larg-gt2 LABEL "Largura" AT ROW 5.5 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 08 BY .88
        
        i-qt-bobinas-gt2 LABEL "Qt.bobinas" AT ROW 6.5 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 08 BY .88
        
        gt2-rect-1 AT ROW 1.2 COL 2

        gt2-bt-ok          AT ROW 8.8 COL 2.14
        gt2-bt-cancel      AT ROW 8.8 COL 13             
        gt2-rt-botoes      AT ROW 8.5 COL 1
        SPACE(0.28)
        WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE 
             THREE-D SCROLLABLE TITLE "Inclui uma nova largura" FONT 1
             DEFAULT-BUTTON gt2-bt-ok CANCEL-BUTTON gt2-bt-cancel.

    ON "CHOOSE":U OF gt2-bt-ok IN FRAME gt2-frame-1 DO:

        FIND FIRST tt-resumo-ped NO-LOCK NO-ERROR.

        IF AVAIL tt-resumo-ped THEN DO:

            IF tt-resumo-ped.diin <> int(i-diin-gt2:SCREEN-VALUE IN FRAME gt2-frame-1) OR
               tt-resumo-ped.diex <> int(i-diex-gt2:SCREEN-VALUE IN FRAME gt2-frame-1) THEN DO:
            
            run utp/ut-msgs.p (input "show":U, input 17006, "NÆo ‚ permitido diƒmetros diferentes na mesma configura‡Æo").
            
            RETURN NO-APPLY.
            
            END.

        END.

        FIND FIRST tt-resumo-ped WHERE
            tt-resumo-ped.diex = int(i-diex-gt2:SCREEN-VALUE IN FRAME gt2-frame-1) AND
            tt-resumo-ped.diin = int(i-diin-gt2:SCREEN-VALUE IN FRAME gt2-frame-1) AND
            tt-resumo-ped.larg = int(i-larg-gt2:SCREEN-VALUE IN FRAME gt2-frame-1) 
            NO-LOCK NO-ERROR.

        IF AVAIL tt-resumo-ped THEN DO:

            run utp/ut-msgs.p (input "show":U, input 17006, "J  existe esta configura‡Æo").
            RETURN NO-APPLY.

        END.
                  
        CREATE tt-resumo-ped.

        ASSIGN tt-resumo-ped.seq        = int(i-sequencia-gt2:SCREEN-VALUE IN FRAME gt2-frame-1)
               tt-resumo-ped.diex       = int(i-diex-gt2:SCREEN-VALUE IN FRAME gt2-frame-1)
               tt-resumo-ped.diin       = int(i-diin-gt2:SCREEN-VALUE IN FRAME gt2-frame-1)     
               tt-resumo-ped.larg       = int(i-larg-gt2:SCREEN-VALUE IN FRAME gt2-frame-1)
               tt-resumo-ped.it-codigo  = c-it-codigo-gt2:SCREEN-VALUE IN FRAME gt2-frame-1
               tt-resumo-ped.qt-bobinas = int(i-qt-bobinas-gt2:SCREEN-VALUE IN FRAME gt2-frame-1).

        ASSIGN fator-jr =  0.8778 /* 0.9050 */ .

        ASSIGN peso-jr = (((((tt-resumo-ped.diex * tt-resumo-ped.diex) -
                         (tt-resumo-ped.diin * tt-resumo-ped.diin)) * 3.1416)
                         / 4) * (tt-resumo-ped.larg * fator-jr) / 1000) / 1000.

        ASSIGN tt-resumo-ped.peso-bob   = peso-jr.

     RETURN.

    END.

    FIND FIRST tt-resumo-ped NO-LOCK NO-ERROR.

    IF AVAIL tt-resumo-ped THEN
       ASSIGN i-diin-gt2:SCREEN-VALUE IN FRAME gt2-frame-1 = string(tt-resumo-ped.diin)
              i-diex-gt2:SCREEN-VALUE IN FRAME gt2-frame-1 = string(tt-resumo-ped.diex)
              c-it-codigo-gt2:SCREEN-VALUE IN FRAME gt2-frame-1 = tt-resumo-ped.it-codigo.

       ELSE 
           ASSIGN i-diin-gt2:SCREEN-VALUE IN FRAME gt2-frame-1 = string(i-diin-gt1)
                  i-diex-gt2:SCREEN-VALUE IN FRAME gt2-frame-1 = string(i-diex-gt2)
                  c-it-codigo-gt2:SCREEN-VALUE IN FRAME gt2-frame-1 = c-it-codigo-ini:SCREEN-VALUE IN FRAME f-relat.
               
    ENABLE i-sequencia-gt2 i-diin-gt2 i-diex-gt2 i-larg-gt2 i-qt-bobinas-gt2 gt2-bt-ok gt2-bt-cancel 
        WITH FRAME gt2-frame-1. 
    
    WAIT-FOR "GO":U OF FRAME gt2-frame-1.

END PROCEDURE.


PROCEDURE pi-gera-planilha-doff:

DO:

    run utp/ut-acomp.p persistent set h-acomp.
    assign v-num-reg-lidos = 0.               
    run pi-inicializar in h-acomp(input "Gerando Planilha dos Doffs:").

    /* Cria Aplica‡Æo do Excel */

    CREATE "Excel.Application" c-excel.
    ASSIGN c-excel:DisplayAlerts = FALSE.

    ASSIGN c-modelo-planilha = search("modelos/mod-escp0602.xls") 
           c-arq             = SESSION:TEMP-DIRECTORY.

    RUN pi-cria-planilha.

    assign v-num-reg-lidos = 0.

    ASSIGN c-relatorio:range("V" + STRING(1)):VALUE = today.    

    ASSIGN c-relatorio:range("B" + STRING(3)):VALUE = c-cod-estabel-amg:SCREEN-VALUE IN FRAME f-relat
           c-relatorio:range("B" + STRING(4)):VALUE = i-nr-linha-rec:SCREEN-VALUE IN FRAME f-relat
           c-relatorio:range("B" + STRING(5)):VALUE = i-larg-util-rec-max:SCREEN-VALUE IN FRAME f-relat
           c-relatorio:range("B" + STRING(6)):VALUE = i-diin-util-rec:SCREEN-VALUE IN FRAME f-relat
           c-relatorio:range("C" + STRING(4)):VALUE = c-desc-linha-rec:SCREEN-VALUE IN FRAME f-relat

           c-relatorio:range("F" + STRING(4)):VALUE = i-nr-linha-met:SCREEN-VALUE IN FRAME f-relat
           c-relatorio:range("F" + STRING(5)):VALUE = i-qtd-facas
           c-relatorio:range("F" + STRING(6)):VALUE = i-diex-max
           c-relatorio:range("G" + STRING(4)):VALUE = c-desc-linha-met:SCREEN-VALUE IN FRAME f-relat

           c-relatorio:range("K" + STRING(4)):VALUE = i-nr-linha-pri:SCREEN-VALUE IN FRAME f-relat
           c-relatorio:range("K" + STRING(5)):VALUE = i-larg-util-pri-max
           c-relatorio:range("K" + STRING(6)):VALUE = i-larg-min-estoq:SCREEN-VALUE IN FRAME f-relat
           c-relatorio:range("L" + STRING(4)):VALUE = c-desc-linha-pri:SCREEN-VALUE IN FRAME f-relat

           c-relatorio:range("K" + STRING(3)):VALUE = c-it-codigo-ini:SCREEN-VALUE IN FRAME f-relat.


    FIND FIRST pol-param-relatorio WHERE
        pol-param-relatorio.cod-prog-dtsul = "ESCP0602" AND
        pol-param-relatorio.cod-usuario    = ""        AND
        pol-param-relatorio.mes-refer      = 1         AND  
        pol-param-relatorio.ano-refer      = 9999
        NO-LOCK NO-ERROR.

    IF AVAIL pol-param-relatorio THEN
        ASSIGN c-relatorio:range("V" + STRING(3)):VALUE = pol-param-relatorio.int-1.

    ASSIGN i-linha = 9. 

    ASSIGN soma-perdas = 0
           soma-sobras = 0.

    ASSIGN i-linha = i-linha + 1
           c-relatorio:range("F" + STRING(i-linha)):VALUE = "CORTE SECUNDµRIO"
           i-linha = i-linha + 1.


    FOR EACH tt-doff-sec NO-LOCK.

        assign v-num-reg-lidos = v-num-reg-lidos + 1.
        run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

        ASSIGN i-linha = i-linha + 1.

        ASSIGN c-relatorio:range("A" + STRING(i-linha)):VALUE = tt-doff-sec.sq-doff    
               c-relatorio:range("B" + STRING(i-linha)):VALUE = tt-doff-sec.diin  
               c-relatorio:range("C" + STRING(i-linha)):VALUE = tt-doff-sec.diex                      
               c-relatorio:range("D" + STRING(i-linha)):VALUE = tt-doff-sec.qt-doff    
               c-relatorio:range("E" + STRING(i-linha)):VALUE = tt-doff-sec.qt-bobinas    
               c-relatorio:range("F" + STRING(i-linha)):VALUE = IF tt-doff-sec.lg-faca-1 > 0 THEN string(tt-doff-sec.lg-faca-1) ELSE  " " 
               c-relatorio:range("G" + STRING(i-linha)):VALUE = IF tt-doff-sec.lg-faca-2  > 0 THEN string(tt-doff-sec.lg-faca-2) ELSE  " " 
               c-relatorio:range("H" + STRING(i-linha)):VALUE = IF tt-doff-sec.lg-faca-3  > 0 THEN string(tt-doff-sec.lg-faca-3) ELSE  " " 
               c-relatorio:range("I" + STRING(i-linha)):VALUE = IF tt-doff-sec.lg-faca-4  > 0 THEN string(tt-doff-sec.lg-faca-4) ELSE  " " 
               c-relatorio:range("J" + STRING(i-linha)):VALUE = IF tt-doff-sec.lg-faca-5  > 0 THEN string(tt-doff-sec.lg-faca-5) ELSE  " " 
               c-relatorio:range("K" + STRING(i-linha)):VALUE = IF tt-doff-sec.lg-faca-6  > 0 THEN string(tt-doff-sec.lg-faca-6) ELSE  " " 
               c-relatorio:range("L" + STRING(i-linha)):VALUE = IF tt-doff-sec.lg-faca-7  > 0 THEN string(tt-doff-sec.lg-faca-7) ELSE  " " 
               c-relatorio:range("M" + STRING(i-linha)):VALUE = IF tt-doff-sec.lg-faca-8  > 0 THEN string(tt-doff-sec.lg-faca-8) ELSE  " " 
               c-relatorio:range("N" + STRING(i-linha)):VALUE = IF tt-doff-sec.lg-faca-9  > 0 THEN string(tt-doff-sec.lg-faca-9) ELSE  " " 
               c-relatorio:range("O" + STRING(i-linha)):VALUE = IF tt-doff-sec.lg-faca-10 > 0 THEN string(tt-doff-sec.lg-faca-10) ELSE  " " 
               c-relatorio:range("P" + STRING(i-linha)):VALUE = IF tt-doff-sec.lg-faca-11 > 0 THEN string(tt-doff-sec.lg-faca-11) ELSE  " " 
               c-relatorio:range("Q" + STRING(i-linha)):VALUE = IF tt-doff-sec.lg-faca-12 > 0 THEN string(tt-doff-sec.lg-faca-12) ELSE  " " 
               c-relatorio:range("R" + STRING(i-linha)):VALUE = IF tt-doff-sec.lg-faca-13 > 0 THEN string(tt-doff-sec.lg-faca-13) ELSE  " " 
               c-relatorio:range("S" + STRING(i-linha)):VALUE = IF tt-doff-sec.lg-faca-14 > 0 THEN string(tt-doff-sec.lg-faca-14) ELSE  " " 
               c-relatorio:range("T" + STRING(i-linha)):VALUE = IF tt-doff-sec.lg-faca-15 > 0 THEN string(tt-doff-sec.lg-faca-15) ELSE  " " 
               c-relatorio:range("U" + STRING(i-linha)):VALUE = IF tt-doff-sec.lg-faca-16 > 0 THEN string(tt-doff-sec.lg-faca-16) ELSE  " " 

               c-relatorio:range("V" + STRING(i-linha)):VALUE = tt-doff-sec.qt-sobras   
               c-relatorio:range("W" + STRING(i-linha)):VALUE = tt-doff-sec.qt-perdas. 

        ASSIGN c-relatorio:range("M" + STRING(i-linha)):VALUE = "Orig:"
               c-relatorio:range("N" + STRING(i-linha)):VALUE = string(tt-doff-sec.lg-bob-mae-1) 

               c-relatorio:range("P" + STRING(i-linha)):VALUE = IF tt-doff-sec.lg-bob-mae-2 > 0 THEN "Emen:" ELSE  " "

               c-relatorio:range("Q" + STRING(i-linha)):VALUE = IF tt-doff-sec.lg-bob-mae-2 > 0 THEN string(tt-doff-sec.lg-bob-mae-2) ELSE  " ". 
        
        ASSIGN menor-larg-jr = tt-doff-sec.lg-bob-mae-1.
        
        IF tt-doff-sec.lg-bob-mae-2 <> 0  AND tt-doff-sec.lg-bob-mae-2 < menor-larg-jr THEN
           ASSIGN menor-larg-jr = tt-doff-sec.lg-bob-mae-2.

        ASSIGN menor-larg-jr = menor-larg-jr - tt-doff-sec.soma-larg.

        ASSIGN c-relatorio:range("V" + STRING(i-linha)):VALUE = string(menor-larg-jr).

        ASSIGN kg-bob-mae-1 = tt-doff-sec.kg-bob-mae-1 + tt-doff-sec.kg-bob-mae-2. 
               
        
        ASSIGN c-relatorio:range("X" + STRING(i-linha)):VALUE = kg-bob-mae-1
               c-relatorio:range("Y" + STRING(i-linha)):VALUE = ((kg-bob-mae-1 / tt-doff-sec.peso-bob-mae) * 100).



        ASSIGN soma-perdas = soma-perdas + (tt-doff-sec.qt-perdas * tt-doff-sec.qt-doff)
               soma-sobras = soma-sobras + (tt-doff-sec.qt-sobras * tt-doff-sec.qt-doff).

    END.

    ASSIGN i-linha = i-linha + 2
           c-relatorio:range("S" + STRING(i-linha)):VALUE = "TOTAL"
           c-relatorio:range("V" + STRING(i-linha)):VALUE = soma-sobras
           c-relatorio:range("W" + STRING(i-linha)):VALUE = soma-perdas             
           i-linha = i-linha + 1.



    ASSIGN soma-perdas = 0
           soma-sobras = 0.

    ASSIGN i-linha = i-linha + 1
           c-relatorio:range("F" + STRING(i-linha)):VALUE = "METALIZA€ÇO"
           i-linha = i-linha + 1.


    FOR EACH tt-doff-met NO-LOCK.

        assign v-num-reg-lidos = v-num-reg-lidos + 1.
        run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

        ASSIGN i-linha = i-linha + 1.

        ASSIGN c-relatorio:range("A" + STRING(i-linha)):VALUE = tt-doff-met.sq-doff    
               c-relatorio:range("B" + STRING(i-linha)):VALUE = tt-doff-met.diin  
               c-relatorio:range("C" + STRING(i-linha)):VALUE = tt-doff-met.diex                      
               c-relatorio:range("D" + STRING(i-linha)):VALUE = tt-doff-met.qt-doff    
               c-relatorio:range("E" + STRING(i-linha)):VALUE = tt-doff-met.qt-bobinas    
               c-relatorio:range("F" + STRING(i-linha)):VALUE = IF tt-doff-met.lg-ajustada > 0 THEN string(tt-doff-met.lg-ajustada) ELSE  " " 

               c-relatorio:range("V" + STRING(i-linha)):VALUE = tt-doff-met.qt-sobras   
               c-relatorio:range("W" + STRING(i-linha)):VALUE = tt-doff-met.qt-perdas.  

        ASSIGN soma-perdas = soma-perdas + (tt-doff-met.qt-perdas * tt-doff-met.qt-doff)
               soma-sobras = soma-sobras + (tt-doff-met.qt-sobras * tt-doff-met.qt-doff).

    END.

    ASSIGN i-linha = i-linha + 2
           c-relatorio:range("S" + STRING(i-linha)):VALUE = "TOTAL"
           c-relatorio:range("V" + STRING(i-linha)):VALUE = soma-sobras
           c-relatorio:range("W" + STRING(i-linha)):VALUE = soma-perdas             
           i-linha = i-linha + 1.



    ASSIGN soma-perdas = 0
           soma-sobras = 0.

    ASSIGN i-linha = i-linha + 1
           c-relatorio:range("F" + STRING(i-linha)):VALUE = "CORTE PRIMµRIO"
           i-linha = i-linha + 1.


    FOR EACH tt-doff-pri NO-LOCK.

        assign v-num-reg-lidos = v-num-reg-lidos + 1.
        run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

        ASSIGN i-linha = i-linha + 1.

        ASSIGN c-relatorio:range("A" + STRING(i-linha)):VALUE = tt-doff-pri.sq-doff    
               c-relatorio:range("B" + STRING(i-linha)):VALUE = tt-doff-pri.diin  
               c-relatorio:range("C" + STRING(i-linha)):VALUE = tt-doff-pri.diex                      
               c-relatorio:range("D" + STRING(i-linha)):VALUE = tt-doff-pri.qt-doff    
               c-relatorio:range("E" + STRING(i-linha)):VALUE = tt-doff-pri.qt-bobinas    
               c-relatorio:range("F" + STRING(i-linha)):VALUE = IF tt-doff-pri.lg-faca-1 > 0 THEN string(tt-doff-pri.lg-faca-1) ELSE  " " 
               c-relatorio:range("G" + STRING(i-linha)):VALUE = IF tt-doff-pri.lg-faca-2  > 0 THEN string(tt-doff-pri.lg-faca-2) ELSE  " " 
               c-relatorio:range("H" + STRING(i-linha)):VALUE = IF tt-doff-pri.lg-faca-3  > 0 THEN string(tt-doff-pri.lg-faca-3) ELSE  " " 
               c-relatorio:range("I" + STRING(i-linha)):VALUE = IF tt-doff-pri.lg-faca-4  > 0 THEN string(tt-doff-pri.lg-faca-4) ELSE  " " 
               c-relatorio:range("J" + STRING(i-linha)):VALUE = IF tt-doff-pri.lg-faca-5  > 0 THEN string(tt-doff-pri.lg-faca-5) ELSE  " " 
               c-relatorio:range("K" + STRING(i-linha)):VALUE = IF tt-doff-pri.lg-faca-6  > 0 THEN string(tt-doff-pri.lg-faca-6) ELSE  " " 
               c-relatorio:range("L" + STRING(i-linha)):VALUE = IF tt-doff-pri.lg-faca-7  > 0 THEN string(tt-doff-pri.lg-faca-7) ELSE  " " 
               c-relatorio:range("M" + STRING(i-linha)):VALUE = IF tt-doff-pri.lg-faca-8  > 0 THEN string(tt-doff-pri.lg-faca-8) ELSE  " " 
               c-relatorio:range("N" + STRING(i-linha)):VALUE = IF tt-doff-pri.lg-faca-9  > 0 THEN string(tt-doff-pri.lg-faca-9) ELSE  " " 
               c-relatorio:range("O" + STRING(i-linha)):VALUE = IF tt-doff-pri.lg-faca-10 > 0 THEN string(tt-doff-pri.lg-faca-10) ELSE  " " 
               c-relatorio:range("P" + STRING(i-linha)):VALUE = IF tt-doff-pri.lg-faca-11 > 0 THEN string(tt-doff-pri.lg-faca-11) ELSE  " " 
               c-relatorio:range("Q" + STRING(i-linha)):VALUE = IF tt-doff-pri.lg-faca-12 > 0 THEN string(tt-doff-pri.lg-faca-12) ELSE  " " 
               c-relatorio:range("R" + STRING(i-linha)):VALUE = IF tt-doff-pri.lg-faca-13 > 0 THEN string(tt-doff-pri.lg-faca-13) ELSE  " " 
               c-relatorio:range("S" + STRING(i-linha)):VALUE = IF tt-doff-pri.lg-faca-14 > 0 THEN string(tt-doff-pri.lg-faca-14) ELSE  " " 
               c-relatorio:range("T" + STRING(i-linha)):VALUE = IF tt-doff-pri.lg-faca-15 > 0 THEN string(tt-doff-pri.lg-faca-15) ELSE  " " 
               c-relatorio:range("U" + STRING(i-linha)):VALUE = IF tt-doff-pri.lg-faca-16 > 0 THEN string(tt-doff-pri.lg-faca-16) ELSE  " " 

               c-relatorio:range("V" + STRING(i-linha)):VALUE = tt-doff-pri.qt-sobras   
               c-relatorio:range("W" + STRING(i-linha)):VALUE = tt-doff-pri.qt-perdas.  

        ASSIGN soma-perdas = soma-perdas + (tt-doff-pri.qt-perdas * tt-doff-pri.qt-doff)
               soma-sobras = soma-sobras + (tt-doff-pri.qt-sobras * tt-doff-pri.qt-doff).

    END.

    ASSIGN i-linha = i-linha + 2
           c-relatorio:range("S" + STRING(i-linha)):VALUE = "TOTAL"
           c-relatorio:range("V" + STRING(i-linha)):VALUE = soma-sobras
           c-relatorio:range("W" + STRING(i-linha)):VALUE = soma-perdas             
           i-linha = i-linha + 1.


    ASSIGN i-linha = i-linha + 3
           c-relatorio:range("A" + STRING(i-linha)):VALUE = "ORDENS DE PRODU€ÇO ABERTAS"
           i-linha = i-linha + 2
           c-relatorio:range("A" + STRING(i-linha)):VALUE = "Fase"
           c-relatorio:range("B" + STRING(i-linha)):VALUE = "Nr.Ord"
           c-relatorio:range("C" + STRING(i-linha)):VALUE = "Item"
           c-relatorio:range("E" + STRING(i-linha)):VALUE = "Pedido"
           c-relatorio:range("F" + STRING(i-linha)):VALUE = "Seq"
           c-relatorio:range("I" + STRING(i-linha)):VALUE = "Qt.a Produzir"
           c-relatorio:range("L" + STRING(i-linha)):VALUE = "Qt.Bobs"
           c-relatorio:range("N" + STRING(i-linha)):VALUE = "Diin"
           c-relatorio:range("O" + STRING(i-linha)):VALUE = "Diex"
           c-relatorio:range("P" + STRING(i-linha)):VALUE = "Larg"
           c-relatorio:range("Q" + STRING(i-linha)):VALUE = "Cliente"
         /*  c-relatorio:range("R" + STRING(i-linha)):VALUE = "Pd.Final"
           c-relatorio:range("S" + STRING(i-linha)):VALUE = "Cliente Final"           
           */
           i-linha = i-linha + 1.


    FOR EACH tt-abre-ordem NO-LOCK.

        ASSIGN i-linha = i-linha + 1.

        IF tt-abre-ordem.fase = 1 THEN
            ASSIGN c-relatorio:range("A" + STRING(i-linha)):VALUE = "PRIMµRIA".
        IF tt-abre-ordem.fase = 2 THEN
            ASSIGN c-relatorio:range("A" + STRING(i-linha)):VALUE = "METAL".
        IF tt-abre-ordem.fase = 3 THEN
            ASSIGN c-relatorio:range("A" + STRING(i-linha)):VALUE = "SECUNDµRIA".

        ASSIGN c-relatorio:range("B" + STRING(i-linha)):VALUE = tt-abre-ordem.nr-ord-produ
               c-relatorio:range("C" + STRING(i-linha)):VALUE = tt-abre-ordem.it-codigo
               c-relatorio:range("E" + STRING(i-linha)):VALUE = tt-abre-ordem.nr-pedido
               c-relatorio:range("F" + STRING(i-linha)):VALUE = tt-abre-ordem.seq-ped
               c-relatorio:range("Q" + STRING(i-linha)):VALUE = tt-abre-ordem.nome-abrev
               c-relatorio:range("I" + STRING(i-linha)):VALUE = string("Qt:" + string(tt-abre-ordem.peso-kg, "zzzzzzz9.99" ))
               c-relatorio:range("L" + STRING(i-linha)):VALUE = tt-abre-ordem.qt-bobinas
               c-relatorio:range("N" + STRING(i-linha)):VALUE = tt-abre-ordem.diin
               c-relatorio:range("O" + STRING(i-linha)):VALUE = tt-abre-ordem.diex
               c-relatorio:range("P" + STRING(i-linha)):VALUE = tt-abre-ordem.larg.
               
               FIND FIRST if-ped-venda WHERE if-ped-venda.nr-pedido = tt-abre-ordem.nr-pedido NO-LOCK NO-ERROR.

               IF AVAIL if-ped-venda THEN DO:
         
         
                    FIND FIRST b-ped-venda WHERE b-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac NO-LOCK NO-ERROR.
         
                    IF  AVAIL b-ped-venda THEN do:
                         
                          c-relatorio:range("Q" + STRING(i-linha)):VALUE = b-ped-venda.nome-abrev + "-" + tt-abre-ordem.nome-abrev.
         
                    end.
               END.
               
    END.
                                 
  
    run pi-finalizar in h-acomp.

    RUN pi-finaliza-impressao.

END.

END PROCEDURE.


PROCEDURE pi-cria-planilha:

DEF VAR c-arquivo AS CHAR NO-UNDO.

    ASSIGN
    c-arq-anexo = "".

    c-arquivo = c-arq + 'mod-escp0602' + STRING(time)+ '.xls'.

    OS-COPY VALUE(c-modelo-planilha) VALUE(c-arquivo).

    assign c-planilha  = c-excel:Workbooks:OPEN(c-arquivo)
           c-relatorio = c-excel:Sheets:item(1)
           c-arq-anexo = (IF c-arq-anexo <> '' THEN ',' ELSE '') + c-arquivo.

END PROCEDURE.


PROCEDURE pi-finaliza-impressao:

DEF VAR i         AS INT  NO-UNDO.
DEF VAR c-arquivo AS CHAR NO-UNDO.

    c-planilha:SAVE().
    c-planilha:CLOSE(). 
     
    c-excel:VISIBLE = true.

    DO i = 1 TO NUM-ENTRIES(c-arq-anexo):
        c-arquivo = ENTRY(i,c-arq-anexo).
        c-planilha  = c-excel:Workbooks:OPEN(c-arquivo).
    END.

    /*c-excel:QUIT().*/

    RELEASE OBJECT c-excel.  

END PROCEDURE.                  

PROCEDURE pi-conjuga-pri.

    ASSIGN tem-conjugacao = 9
           sq-doff-jr = 0
           seq-jr1    = 0.

    FOR EACH tt-resumo-ped.
        DELETE tt-resumo-ped.
    END.

    FOR EACH tt-doff-met.
  
        FIND FIRST tt-resumo-ped WHERE
            tt-resumo-ped.diex = tt-doff-met.diex AND
            tt-resumo-ped.diin = tt-doff-met.diin AND
            tt-resumo-ped.larg = tt-doff-met.lg-ajustada
            NO-ERROR.

        IF NOT AVAIL tt-resumo-ped THEN DO:

            ASSIGN seq-jr1 = seq-jr1 + 1.

            CREATE tt-resumo-ped.

            ASSIGN tt-resumo-ped.seq      = seq-jr1
                   tt-resumo-ped.larg     = tt-doff-met.lg-ajustada
                   tt-resumo-ped.diin     = tt-doff-met.diin 
                   tt-resumo-ped.diex     = tt-doff-met.diex.
                   
        END.

        ASSIGN tt-resumo-ped.qt-bobinas = tt-resumo-ped.qt-bobinas + tt-doff-met.qt-doff.

    END.
    
    assign seq-jr1 = 0.
    
    DO WHILE tem-conjugacao <> 0.

        ASSIGN tem-conjugacao = 0.

        FOR EACH tt-sel-bob.
            DELETE tt-sel-bob.
        END.
        
        FOR EACH tt-conjuga-1.
            DELETE tt-conjuga-1.
        END.

        FOR EACH tt-bobinas.
            DELETE tt-bobinas.
        END.

        FOR EACH tt-resumo-ped NO-LOCK.

            IF tt-resumo-ped.qt-bobinas <= 0 THEN NEXT.

            FIND FIRST tt-sel-bob WHERE
                tt-sel-bob.diin = tt-resumo-ped.diin AND
                tt-sel-bob.diex = tt-resumo-ped.diex
                NO-ERROR.
        
            IF NOT AVAIL tt-sel-bob THEN
                CREATE tt-sel-bob.
        
            ASSIGN tt-sel-bob.diin = tt-resumo-ped.diin
                   tt-sel-bob.diex = tt-resumo-ped.diex.
        
            ASSIGN i-jr = 0.

            ASSIGN tem-conjugacao = 9.
        
            DO WHILE i-jr < 16.
                ASSIGN i-jr = i-jr + 1.
                IF tt-sel-bob.larg [i-jr] = 0 THEN  DO:
                    ASSIGN tt-sel-bob.larg [i-jr]    = tt-resumo-ped.larg
                           tt-sel-bob.qtd-bob [i-jr] = tt-resumo-ped.qt-bobinas
                           tt-sel-bob.qtd-larg = tt-sel-bob.qtd-larg + 1.
                    LEAVE.
                END.
        
            END.
        
        END.        

        IF tem-conjugacao <> 0 THEN DO:

            FOR EACH tt-sel-bob.

                CREATE tt-bobinas.

                ASSIGN tt-bobinas.diin        = tt-sel-bob.diin    
                       tt-bobinas.diex        = tt-sel-bob.diex
                       tt-bobinas.qtd-larg    = tt-sel-bob.qtd-larg.

                ASSIGN i-jr = 0.

                DO WHILE i-jr < 16.
                    ASSIGN i-jr = i-jr + 1.

                    ASSIGN tt-bobinas.larg [i-jr]    = tt-sel-bob.larg [i-jr]
                           tt-bobinas.qtd-bob [i-jr] = tt-sel-bob.qtd-bob [i-jr].

                END.

            END.

            ASSIGN i-larg-util-rot-max = int(i-larg-util-pri:SCREEN-VALUE IN FRAME f-relat)
                   i-larg-util-rot-min = int(i-larg-util-pri:SCREEN-VALUE IN FRAME f-relat).
                   
                   
                   
            RUN pi-conjuga-bob.

            FOR EACH tt-doff-rot.
            
                CREATE tt-doff-pri.
            
                ASSIGN tt-doff-pri.sq-doff     = tt-doff-rot.sq-doff   
                       tt-doff-pri.diin        = tt-doff-rot.diin      
                       tt-doff-pri.diex        = tt-doff-rot.diex      
                       tt-doff-pri.qt-doff     = tt-doff-rot.qt-doff   
                       tt-doff-pri.qt-bobinas  = tt-doff-rot.qt-bobinas
                       tt-doff-pri.lg-faca-1   = tt-doff-rot.lg-faca-1 
                       tt-doff-pri.lg-faca-2   = tt-doff-rot.lg-faca-2 
                       tt-doff-pri.lg-faca-3   = tt-doff-rot.lg-faca-3 
                       tt-doff-pri.lg-faca-4   = tt-doff-rot.lg-faca-4 
                       tt-doff-pri.lg-faca-5   = tt-doff-rot.lg-faca-5 
                       tt-doff-pri.lg-faca-6   = tt-doff-rot.lg-faca-6 
                       tt-doff-pri.lg-faca-7   = tt-doff-rot.lg-faca-7 
                       tt-doff-pri.lg-faca-8   = tt-doff-rot.lg-faca-8 
                       tt-doff-pri.lg-faca-9   = tt-doff-rot.lg-faca-9 
                       tt-doff-pri.lg-faca-10  = tt-doff-rot.lg-faca-10
                       tt-doff-pri.lg-faca-11  = tt-doff-rot.lg-faca-11
                       tt-doff-pri.lg-faca-12  = tt-doff-rot.lg-faca-12
                       tt-doff-pri.lg-faca-13  = tt-doff-rot.lg-faca-13
                       tt-doff-pri.lg-faca-14  = tt-doff-rot.lg-faca-14
                       tt-doff-pri.lg-faca-15  = tt-doff-rot.lg-faca-15
                       tt-doff-pri.lg-faca-16  = tt-doff-rot.lg-faca-16
                       tt-doff-pri.obs         = tt-doff-rot.obs.
            
                ASSIGN tt-doff-pri.qt-sobras = tt-doff-rot.qt-sobras 
                       tt-doff-pri.qt-perdas = tt-doff-rot.qt-perdas .
            
            END.

        END.

    END.

    CLOSE QUERY br-doff-pri.
    open query br-doff-pri for each tt-doff-pri.

    if num-results("br-doff-pri") > 0 THEN DO:
       get current br-doff-pri.
    END.


    FOR EACH tt-resumo-ped.
        DELETE tt-resumo-ped.
    END.

    FOR EACH tt-resumo-ant.

        CREATE tt-resumo-ped.
        
        ASSIGN  tt-resumo-ped.seq        = tt-resumo-ant.seq 
                tt-resumo-ped.diex       = tt-resumo-ant.diex      
                tt-resumo-ped.diin       = tt-resumo-ant.diin      
                tt-resumo-ped.larg       = tt-resumo-ant.larg      
                tt-resumo-ped.peso-bob   = tt-resumo-ant.peso-bob      
                tt-resumo-ped.it-codigo  = tt-resumo-ant.it-codigo 
                tt-resumo-ped.qt-bobinas = tt-resumo-ant.qt-bobinas.
                
    END.

    CLOSE QUERY br-resumo-ped.
    open query br-resumo-ped for each tt-resumo-ped.
    
    if num-results("br-resumo-ped") > 0 THEN DO:
       get current br-resumo-ped.
    END.

END PROCEDURE.


PROCEDURE pi-inclui-larg-met.
    
    DEFINE BUTTON gt3-bt-cancel AUTO-END-KEY 
         LABEL "&Cancelar" 
         SIZE 10 BY 1
         BGCOLOR 8.
    
    DEFINE BUTTON gt3-bt-ok AUTO-GO 
         LABEL "&OK" 
         SIZE 10 BY 1
         BGCOLOR 8.
    
    DEFINE RECTANGLE gt3-rt-botoes
         EDGE-PIXELS 2 GRAPHIC-EDGE  
         SIZE 60 BY 1.42
         BGCOLOR 7.

    DEFINE VARIABLE i-diin-gt3 AS INTEGER NO-UNDO.
    DEFINE VARIABLE i-diex-gt3 AS INTEGER NO-UNDO.
    DEFINE VARIABLE i-larg-gt3 AS INTEGER NO-UNDO.

    DEFINE VARIABLE i-qt-bobinas-gt3 AS INTEGER    NO-UNDO.
    DEFINE VARIABLE i-sequencia-gt3  AS INTEGER    NO-UNDO.
    DEFINE VARIABLE c-it-codigo-gt3  AS CHARACTER  NO-UNDO.
    
    DEFINE RECTANGLE gt3-rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59 BY 7.
    
    DEFINE FRAME gt3-frame-1

        i-diin-gt3 LABEL "D.Interno" AT ROW 1.5 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 08 BY .88
        
        i-diex-gt3 LABEL "D.Externo" AT ROW 2.5 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 08 BY .88
        
        c-it-codigo-gt3 LABEL "Item" AT ROW 3.5 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 16 BY .88
        
        i-sequencia-gt3 LABEL "Sequencia" AT ROW 4.5 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 08 BY .88
        
        i-larg-gt3 LABEL "Largura" AT ROW 5.5 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 08 BY .88
        
        i-qt-bobinas-gt3 LABEL "Qt.bobinas" AT ROW 6.5 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 08 BY .88
        
        gt3-rect-1 AT ROW 1.2 COL 2

        gt3-bt-ok          AT ROW 8.8 COL 2.14
        gt3-bt-cancel      AT ROW 8.8 COL 13             
        gt3-rt-botoes      AT ROW 8.5 COL 1
        SPACE(0.28)
        WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE 
             THREE-D SCROLLABLE TITLE "Inclui uma nova largura" FONT 1
             DEFAULT-BUTTON gt3-bt-ok CANCEL-BUTTON gt3-bt-cancel.

    ON "CHOOSE":U OF gt3-bt-ok IN FRAME gt3-frame-1 DO:

        FIND FIRST tt-doff-met NO-LOCK NO-ERROR.

        IF AVAIL tt-doff-met THEN DO:

            IF tt-doff-met.diin <> int(i-diin-gt3:SCREEN-VALUE IN FRAME gt3-frame-1) OR
               tt-doff-met.diex <> int(i-diex-gt3:SCREEN-VALUE IN FRAME gt3-frame-1) THEN DO:
            
            run utp/ut-msgs.p (input "show":U, input 17006, "NÆo ‚ permitido diƒmetros diferentes na mesma configura‡Æo").
            
            RETURN NO-APPLY.
            
            END.

        END.

        FIND FIRST tt-doff-met WHERE
            tt-doff-met.diex      = int(i-diex-gt3:SCREEN-VALUE IN FRAME gt3-frame-1) AND
            tt-doff-met.diin      = int(i-diin-gt3:SCREEN-VALUE IN FRAME gt3-frame-1) AND
            tt-doff-met.lg-faca-1 = int(i-larg-gt3:SCREEN-VALUE IN FRAME gt3-frame-1) 
            NO-LOCK NO-ERROR.

        IF AVAIL tt-doff-met THEN DO:

            run utp/ut-msgs.p (input "show":U, input 17006, "J  existe esta configura‡Æo").
            RETURN NO-APPLY.

        END.
                  
        CREATE tt-doff-met.

        ASSIGN tt-doff-met.sq-doff     = int(i-sequencia-gt3:SCREEN-VALUE IN FRAME gt3-frame-1)
               tt-doff-met.diex        = int(i-diex-gt3:SCREEN-VALUE IN FRAME gt3-frame-1)
               tt-doff-met.diin        = int(i-diin-gt3:SCREEN-VALUE IN FRAME gt3-frame-1)     
               tt-doff-met.lg-faca-1   = int(i-larg-gt3:SCREEN-VALUE IN FRAME gt3-frame-1)
               tt-doff-met.lg-ajustada = tt-doff-met.lg-faca-1 + aparas-met
               tt-doff-met.qt-bobinas  = 1
               tt-doff-met.qt-doff     = int(i-qt-bobinas-gt3:SCREEN-VALUE IN FRAME gt3-frame-1).

        ASSIGN fator-jr =  0.8778 /* 0.9050 */ .

        ASSIGN peso-jr = (((((tt-doff-met.diex * tt-doff-met.diex) -
                         (tt-doff-met.diin * tt-doff-met.diin)) * 3.1416)
                         / 4) * (tt-doff-met.lg-faca-1 * fator-jr) / 1000) / 1000.

        ASSIGN tt-doff-met.peso-doff  = peso-jr.

     RETURN.

    END.

    FIND FIRST tt-doff-met NO-LOCK NO-ERROR.

    IF AVAIL tt-doff-met THEN
       ASSIGN i-diin-gt3:SCREEN-VALUE IN FRAME gt3-frame-1 = string(tt-doff-met.diin)
              i-diex-gt3:SCREEN-VALUE IN FRAME gt3-frame-1 = string(tt-doff-met.diex).

       ELSE 
           ASSIGN i-diin-gt3:SCREEN-VALUE IN FRAME gt3-frame-1 = string(i-diin-gt1)
                  i-diex-gt3:SCREEN-VALUE IN FRAME gt3-frame-1 = string(i-diex-gt3).
               
    ENABLE i-sequencia-gt3 i-diin-gt3 i-diex-gt3 i-larg-gt3 i-qt-bobinas-gt3 gt3-bt-ok gt3-bt-cancel 
        WITH FRAME gt3-frame-1. 
    
    WAIT-FOR "GO":U OF FRAME gt3-frame-1.

END PROCEDURE.



PROCEDURE pi-gera-tt-ordem.

    FOR EACH tt-abre-ordem.
        DELETE tt-abre-ordem.
    END.

    FOR EACH tt-doff-pri NO-LOCK.

        ASSIGN larg-jr = tt-doff-pri.lg-faca-1.
        IF larg-jr <> 0 THEN 
            RUN pi-gera-tt-ordem-pri.

        ASSIGN larg-jr = tt-doff-pri.lg-faca-2.
        IF larg-jr <> 0 THEN 
            RUN pi-gera-tt-ordem-pri.

        ASSIGN larg-jr = tt-doff-pri.lg-faca-3.
        IF larg-jr <> 0 THEN 
            RUN pi-gera-tt-ordem-pri.

        ASSIGN larg-jr = tt-doff-pri.lg-faca-4.
        IF larg-jr <> 0 THEN 
            RUN pi-gera-tt-ordem-pri.

        ASSIGN larg-jr = tt-doff-pri.lg-faca-5.
        IF larg-jr <> 0 THEN 
            RUN pi-gera-tt-ordem-pri.

        ASSIGN larg-jr = tt-doff-pri.lg-faca-6.
        IF larg-jr <> 0 THEN 
            RUN pi-gera-tt-ordem-pri.

        ASSIGN larg-jr = tt-doff-pri.lg-faca-7.
        IF larg-jr <> 0 THEN 
            RUN pi-gera-tt-ordem-pri.

        ASSIGN larg-jr = tt-doff-pri.lg-faca-8.
        IF larg-jr <> 0 THEN 
            RUN pi-gera-tt-ordem-pri.

        ASSIGN larg-jr = tt-doff-pri.lg-faca-9.
        IF larg-jr <> 0 THEN 
            RUN pi-gera-tt-ordem-pri.

        ASSIGN larg-jr = tt-doff-pri.lg-faca-10.
        IF larg-jr <> 0 THEN 
            RUN pi-gera-tt-ordem-pri.

        ASSIGN larg-jr = tt-doff-pri.lg-faca-11.
        IF larg-jr <> 0 THEN 
            RUN pi-gera-tt-ordem-pri.

        ASSIGN larg-jr = tt-doff-pri.lg-faca-12.
        IF larg-jr <> 0 THEN 
            RUN pi-gera-tt-ordem-pri.

        ASSIGN larg-jr = tt-doff-pri.lg-faca-13.
        IF larg-jr <> 0 THEN 
            RUN pi-gera-tt-ordem-pri.

        ASSIGN larg-jr = tt-doff-pri.lg-faca-14.
        IF larg-jr <> 0 THEN 
            RUN pi-gera-tt-ordem-pri.

        ASSIGN larg-jr = tt-doff-pri.lg-faca-15.
        IF larg-jr <> 0 THEN 
            RUN pi-gera-tt-ordem-pri.

        ASSIGN larg-jr = tt-doff-pri.lg-faca-16.
        IF larg-jr <> 0 THEN 
            RUN pi-gera-tt-ordem-pri.

    END.

    FOR EACH tt-doff-met NO-LOCK.
 
        ASSIGN larg-jr = tt-doff-met.lg-ajustada. 
        IF larg-jr <> 0 THEN 
            RUN pi-gera-tt-ordem-met.

    END.

    FOR EACH tt-larg-sec.
        DELETE tt-larg-sec.
    END.

    FOR EACH tt-resumo-ped.

        FIND FIRST tt-larg-sec WHERE
            tt-larg-sec.larg = tt-resumo-ped.larg
            NO-ERROR.

        IF NOT AVAIL tt-larg-sec THEN
            CREATE tt-larg-sec.

        ASSIGN tt-larg-sec.larg       = tt-resumo-ped.larg
               tt-larg-sec.qt-bobinas = tt-larg-sec.qt-bobinas + tt-resumo-ped.qt-bobinas.

    END.

    FOR EACH tt-ordem-sec.
        DELETE tt-ordem-sec.
    END.

    FOR EACH tt-pedido WHERE
        tt-pedido.seq-resumo <> 0.

        CREATE tt-ordem-sec.

        ASSIGN tt-ordem-sec.larg       = tt-pedido.larg     
               tt-ordem-sec.nr-pedido  = tt-pedido.nr-pedido
               tt-ordem-sec.seq-ped    = tt-pedido.nr-sequencia
               tt-ordem-sec.qt-bobinas = tt-pedido.qt-bobinas
               tt-ordem-sec.nome-abrev = tt-pedido.nome-abrev
               tt-ordem-sec.cod-refer  = tt-pedido.cod-refer .


        FIND FIRST tt-larg-sec WHERE
            tt-larg-sec.larg = tt-ordem-sec.larg
            NO-ERROR.

        IF AVAIL tt-larg-sec THEN
            ASSIGN tt-larg-sec.qt-bobinas = tt-larg-sec.qt-bobinas - tt-ordem-sec.qt-bobinas.

    END.

    FOR EACH tt-larg-sec WHERE
        tt-larg-sec.qt-bobinas > 0.

        CREATE tt-ordem-sec.

        ASSIGN tt-ordem-sec.larg       = tt-larg-sec.larg     
               tt-ordem-sec.nr-pedido  = 0
               tt-ordem-sec.seq-ped    = 0
               tt-ordem-sec.qt-bobinas = tt-larg-sec.qt-bobinas
               tt-ordem-sec.nome-abrev = "".
        
    END.

    FIND FIRST tt-doff-sec NO-LOCK NO-ERROR.

    IF NOT AVAIL tt-doff-sec THEN RETURN.

    FOR EACH tt-ordem-sec.

        ASSIGN larg-jr = tt-ordem-sec.larg.

        IF larg-jr <> 0 THEN 
            RUN pi-gera-tt-ordem-sec.

    END.
    
END PROCEDURE.

PROCEDURE pi-gera-tt-ordem-pri.

        FIND FIRST tt-abre-ordem WHERE
            tt-abre-ordem.fase = 1                                        AND
            tt-abre-ordem.it-codigo = INPUT FRAME f-relat c-it-codigo-pri AND
            tt-abre-ordem.larg      = larg-jr                             AND
            tt-abre-ordem.diin      = tt-doff-pri.diin                    AND
            tt-abre-ordem.diex      = tt-doff-pri.diex                    AND
            tt-abre-ordem.nr-pedido = 0                                   AND
            tt-abre-ordem.seq-ped   = 0   
            NO-ERROR.

        IF NOT AVAIL tt-abre-ordem THEN DO:

            CREATE tt-abre-ordem.

            ASSIGN tt-abre-ordem.fase = 1                         
                   tt-abre-ordem.it-codigo = INPUT FRAME f-relat c-it-codigo-pri
                   tt-abre-ordem.larg      = larg-jr     
                   tt-abre-ordem.diin      = tt-doff-pri.diin     
                   tt-abre-ordem.diex      = tt-doff-pri.diex     
                   tt-abre-ordem.nr-pedido = 0
                   tt-abre-ordem.seq-ped   = 0.

        END.


        ASSIGN fator-jr =  0.8778 /* 0.9050 */ .

        ASSIGN peso-jr = (((((tt-doff-pri.diex * tt-doff-pri.diex) -
                         (tt-doff-pri.diin * tt-doff-pri.diin)) * 3.1416)
                         / 4) * (larg-jr * fator-jr) / 1000) / 1000.


        ASSIGN tt-abre-ordem.nr-linha      = INPUT FRAME f-relat i-nr-linha-pri
               tt-abre-ordem.qt-bobinas    = tt-abre-ordem.qt-bobinas + ( 1 * tt-doff-pri.qt-doff)
               tt-abre-ordem.nome-abrev    = ""
               tt-abre-ordem.peso-kg       = tt-abre-ordem.peso-kg + peso-jr
               tt-abre-ordem.dt-inicio     = INPUT FRAME f-relat dt-inic-prod
               tt-abre-ordem.dt-fim        = INPUT FRAME f-relat dt-fim-prod
               tt-abre-ordem.it-codigo-res = INPUT FRAME f-relat c-it-codigo-mr
               tt-abre-ordem.peso-kg-res   = 1.


END PROCEDURE.

PROCEDURE pi-gera-tt-ordem-met.

        FIND FIRST tt-abre-ordem WHERE
            tt-abre-ordem.fase = 2                                        AND
            tt-abre-ordem.it-codigo = INPUT FRAME f-relat c-it-codigo-met AND
            tt-abre-ordem.larg      = larg-jr                             AND
            tt-abre-ordem.diin      = tt-doff-met.diin                    AND
            tt-abre-ordem.diex      = tt-doff-met.diex                    AND
            tt-abre-ordem.nr-pedido = 0                                   AND
            tt-abre-ordem.seq-ped   = 0   
            NO-ERROR.

        IF NOT AVAIL tt-abre-ordem THEN DO:

            CREATE tt-abre-ordem.

            ASSIGN tt-abre-ordem.fase = 2                         
                   tt-abre-ordem.it-codigo = INPUT FRAME f-relat c-it-codigo-met
                   tt-abre-ordem.larg      = larg-jr     
                   tt-abre-ordem.diin      = tt-doff-met.diin     
                   tt-abre-ordem.diex      = tt-doff-met.diex     
                   tt-abre-ordem.nr-pedido = 0
                   tt-abre-ordem.seq-ped   = 0.

        END.


        ASSIGN fator-jr =  0.8778 /* 0.9050 */ .

        ASSIGN peso-jr = (((((tt-doff-met.diex * tt-doff-met.diex) -
                         (tt-doff-met.diin * tt-doff-met.diin)) * 3.1416)
                         / 4) * (larg-jr * fator-jr) / 1000) / 1000.



        ASSIGN tt-abre-ordem.nr-linha      = INPUT FRAME f-relat i-nr-linha-met
               tt-abre-ordem.qt-bobinas    = tt-abre-ordem.qt-bobinas + 1
               tt-abre-ordem.nome-abrev    = ""
               tt-abre-ordem.peso-kg       = tt-abre-ordem.peso-kg + peso-jr
               tt-abre-ordem.dt-inicio     = INPUT FRAME f-relat dt-inic-prod
               tt-abre-ordem.dt-fim        = INPUT FRAME f-relat dt-fim-prod
               tt-abre-ordem.it-codigo-res = INPUT FRAME f-relat c-it-codigo-pri
               tt-abre-ordem.peso-kg-res   = 1.


END PROCEDURE.

PROCEDURE pi-gera-tt-ordem-sec.

        FIND FIRST tt-abre-ordem WHERE
            tt-abre-ordem.fase = 3                                        AND
            tt-abre-ordem.it-codigo = INPUT FRAME f-relat c-it-codigo-ini AND
            tt-abre-ordem.larg      = larg-jr                             AND
            tt-abre-ordem.diin      = tt-doff-sec.diin                    AND
            tt-abre-ordem.diex      = tt-doff-sec.diex                    AND
            tt-abre-ordem.nr-pedido = tt-ordem-sec.nr-pedido                                   AND
            tt-abre-ordem.seq-ped   = tt-ordem-sec.seq-ped   
           

            NO-ERROR.

        IF NOT AVAIL tt-abre-ordem THEN DO:

            CREATE tt-abre-ordem.

            ASSIGN tt-abre-ordem.fase = 3                         
                   tt-abre-ordem.it-codigo = INPUT FRAME f-relat c-it-codigo-ini
                   tt-abre-ordem.larg      = larg-jr     
                   tt-abre-ordem.diin      = tt-doff-sec.diin     
                   tt-abre-ordem.diex      = tt-doff-sec.diex     
                   tt-abre-ordem.nr-pedido = tt-ordem-sec.nr-pedido
                   tt-abre-ordem.seq-ped   = tt-ordem-sec.seq-ped
                   tt-abre-ordem.cod-refer = tt-ordem-sec.cod-refer  .

        END.


        ASSIGN fator-jr =  0.8778 /* 0.9050 */ .

        ASSIGN peso-jr = (((((tt-doff-sec.diex * tt-doff-sec.diex) -
                         (tt-doff-sec.diin * tt-doff-sec.diin)) * 3.1416)
                         / 4) * (larg-jr * fator-jr) / 1000) / 1000.

        ASSIGN peso-jr = peso-jr * tt-ordem-sec.qt-bobinas.


        ASSIGN tt-abre-ordem.nr-linha      = INPUT FRAME f-relat i-nr-linha-rec
               tt-abre-ordem.qt-bobinas    = tt-abre-ordem.qt-bobinas + tt-ordem-sec.qt-bobinas
               tt-abre-ordem.nome-abrev    = tt-ordem-sec.nome-abrev
               tt-abre-ordem.peso-kg       = tt-abre-ordem.peso-kg + peso-jr
               tt-abre-ordem.dt-inicio     = INPUT FRAME f-relat dt-inic-prod
               tt-abre-ordem.dt-fim        = INPUT FRAME f-relat dt-fim-prod
               tt-abre-ordem.it-codigo-res = INPUT FRAME f-relat c-it-codigo-ini
               tt-abre-ordem.peso-kg-res   = 1.


END PROCEDURE.


RETURN 'ok'.

  




