/********************************************************************************
** Copyright DATASUL S.A. (2003)
** Todos os Direitos Reservados.
**
*******************************************************************************/
/************************************************************************
**
**  i-prgvrs.i - Programa para cria?’o do log de todos os programas 
**               e objetos do EMS 2.0 para objetos
**  {1} = objeto   provido pelo Roundtable
**  {2} = versao   provido pelo Roundtable
** 21/12/2007 - ALTERADO NOME DO CLIENTE SOMENTE PARA ESTABELECIMENTO 121 
                AO INVEZ DO NOME-ABREV
                
** 10/11/2011 Alterado estabelecimento de 121 para 423               
************************************************************************/
define buffer if-ped-venda for espmulti.if-ped-venda.
def new global shared var c-arquivo-log    as char  format "x(60)" no-undo.
def var c-prg-vrs as char init "[[[2.00.00.000[[[" no-undo.
def var c-prg-obj as char no-undo.
assign c-prg-vrs = "2.00.00.000"
       c-prg-obj = "ESSF0025".
if  c-arquivo-log <> "" and c-arquivo-log <> ? then do:
    find prog_dtsul
        where prog_dtsul.cod_prog_dtsul = "ESSF0025"
        no-lock no-error.
        
   if not avail prog_dtsul then do:
          if  c-prg-obj begins "btb":U then
              assign c-prg-obj = "btb~/":U + c-prg-obj.
          else if c-prg-obj begins "men":U then
                  assign c-prg-obj = "men~/":U + c-prg-obj.
          else if c-prg-obj begins "sec":U then
                  assign c-prg-obj = "sec~/":U + c-prg-obj.
          else if c-prg-obj begins "utb":U then
                  assign c-prg-obj = "utb~/":U + c-prg-obj.
          find prog_dtsul where
               prog_dtsul.nom_prog_ext begins c-prg-obj no-lock no-error.
   end .            /*if*/
    
    output to value(c-arquivo-log) append.
    put "ESSF0025" at 1 "2.00.00.000" at 69 today at 84 string(time,'HH:MM:SS':U) at 94 skip.
    if  avail prog_dtsul then do:
        if  prog_dtsul.nom_prog_dpc <> "" then
            put "DPC : ":U at 5 prog_dtsul.nom_prog_dpc  at 12 skip.
        if  prog_dtsul.nom_prog_appc <> "" then
            put "APPC: ":U at 5 prog_dtsul.nom_prog_appc at 12 skip.
        if  prog_dtsul.nom_prog_upc <> "" then
            put "UPC : ":U at 5 prog_dtsul.nom_prog_upc  at 12 skip.
    end.
    output close.        
end.  
error-status:error = no.
/***************************************************
** i_dbtype.i - Tipo de Gerenciadores utilizados
***************************************************/


        
    /* Preprocessadores que identificam os bancos do Produto EMS 5 */
                    
    /* Preprocessadores que identificam os bancos do Produto EMS 2 */
                                                                                                            
    /* Preprocessadores que identificam os bancos do Produto HR 2 */
            

/* Fim */

 

/*alteracao Anderson(tech540) em 04/02/2003 Include com a definicao 
da temp table utilizada nas includes btb008za.i1 e btb008za.i2 para 
execucao de programas via rpc*/
def temp-table tt-control-prog NO-UNDO
    field cod-versao-integracao as integer       format '999'
    field cod-erro              as integer       format '99999'
    field desc-erro             as character     format 'x(60)'
    field wgh-servid-rpc        as widget-handle format '>>>>>>9'.
 
 
/*fim alteracao Anderson 04/02/2003*/

/* altera‡Æo feita para atender ao WebEnabler - Marcilene Oliveira - 18/12/2003 */

DEFINE NEW GLOBAL SHARED VARIABLE hWenController AS HANDLE     NO-UNDO.
/*Constante utilizada apenas para verificar se a vari vel acima foi definida */ 

/* fim da alatera‡Æo */

   /*** 010000 ***/
/********************************************************************/
 
/* defini‡Æo das temp-tables para recebimento de parƒmetros */
def temp-table tt-raw-digita
    field raw-digita as raw.

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field cod-estabel      LIKE ord-prod.cod-estabel
    field nr-ord-ini       LIKE ord-prod.nr-ord-produ
    field nr-ord-fim       LIKE ord-prod.nr-ord-produ.

DEF TEMP-TABLE tt-dados
    FIELD nr-ord-prod  LIKE ord-prod.nr-ord-produ           LABEL "Ord.Prod.."
    FIELD nr-sequencia LIKE ord-prod.nr-sequencia           LABEL "Seq."
    FIELD cod-estabel  LIKE ord-prod.cod-estabel            LABEL "Estabelecimento"
    FIELD it-codigo    LIKE ord-prod.it-codigo              LABEL "Produto"
    FIELD dt-inicio    LIKE ord-prod.dt-inicio              LABEL "Data"
    FIELD tp-pedido    LIKE ped-venda.tp-pedido             lABEL "Pedido."
    FIELD nr-pedido    LIKE ord-prod.nr-pedido              
    FIELD nome-abrev   LIKE ord-prod.nome-abrev             
    FIELD nome-abrev-fim   LIKE ord-prod.nome-abrev             
    FIELD qtcortada      AS DECIMAL FORMAT ">>>,>>>,>>9.99" LABEL "Qt.Cortada"
    FIELD bbcortada      AS INTEGER FORMAT ">>>>9"          LABEL "BB.Cortada" /*">>>,>>>,>>9"*/
    FIELD dt-entrega   AS DATE FORMAT "99/99/9999"          LABEL "Entrega..." /*LIKE ped-item.dt-entrega*/ 
    FIELD qt-pedida      AS DECIMAL FORMAT ">>>,>>>,>>9.99" LABEL "Qtd.Pedida"
    FIELD qt-faturada    AS DECIMAL FORMAT ">>>,>>>,>>9.99" LABEL "Qt.Faturad"
    FIELD bbfaturada     AS INTEGER FORMAT ">>>>9"          LABEL "BB.Faturad" /*"999"*/
    FIELD perc-fat-ped LIKE emitente.perc-fat-ped           LABEL "%Variac.Qt"
    FIELD qtacortar      AS DECIMAL FORMAT ">>>,>>>,>>9.99" LABEL "Q.A Cortar"
    FIELD qtbacortar     AS INTEGER FORMAT ">>>>9"          LABEL "B.A Cortar" /*">>>,>>>,>>9"*/
    FIELD qt-palete      AS INTEGER FORMAT ">>>>9"          LABEL "Qt.Paletes"
    FIELD cod-cliente    AS CHAR FORMAT "x(10)"             LABEL "Cliente.."
    FIELD largura        AS CHAR FORMAT "x(10)"             LABEL "Largura"
    FIELD densotic       AS CHAR FORMAT "x(10)"             LABEL "Densidade Otica"
    FIELD diamin         AS CHAR FORMAT "x(10)"             LABEL "Diam.Int."
    FIELD diamex         AS CHAR FORMAT "x(10)"             LABEL "Diam. Ext."
    FIELD cod-referencia AS INTEGER                         LABEL "Referˆncia"
    FIELD qtdbob         AS integer FORMAT ">>>>9"          LABEL "Qtd. Bob.."  /*">>>,>>>,>>9"*/
    FIELD embalagem      AS CHAR FORMAT "x(10)"             LABEL "Embalagem."
    FIELD obs-pcp-cq     LIKE ord-prod.narrativa 
    FIELD aplicacao      AS CHAR FORMAT "x(60)"             LABEL "Aplicacao"
    FIELD mxembob        AS CHAR FORMAT "x(10)"             LABEL "Maximo Emenda por Bobina"
    FIELD qtdemen        AS CHAR FORMAT "x(10)"             LABEL "Maximo Emenda por Pedido"
    FIELD qt-otimizada   AS DECIMAL FORMAT ">>>,>>>,>>9.99" LABEL ">====> Qtd.Ordem Otimizador"
    FIELD bb-otimizada   AS integer FORMAT ">>>>9"          LABEL ">====> Qtd. BB Otimizador.."  /*">>>,>>>,>>9"*/
    FIELD obs-cliente    LIKE ord-prod.narrativa 
   INDEX ordem cod-estabel
                nr-ord-prod.


DEF TEMP-TABLE tt-embalagem
    FIELD emb-it-codigo LIKE ord-prod.it-codigo          
    FIELD emb-ref-polo  AS CHAR FORMAT "x(16)"           
    FIELD emb-descricao AS CHAR FORMAT "x(60)"               
    FIELD emb-unid      AS char FORMAT "x(2)"                     
    FIELD emb-qtde      AS DECIMAL FORMAT ">>>,>>9.99"   
    INDEX ordem-emb emb-it-codigo.

def temp-table tt-editor no-undo
    field linha      as integer
    field conteudo   as character format "x(80)"
    index editor-id is primary unique linha.

DEF BUFFER b-emitente FOR emitente.

DEF VAR c-obs-emb AS CHAR NO-UNDO.
DEF VAR c-obs-ger AS CHAR NO-UNDO.
DEF VAR c-obs-cq  AS CHAR NO-UNDO.
DEFINE VARIABLE obs-cliente-jr  AS CHARACTER FORMAT "x(800)" NO-UNDO.
DEFINE VARIABLE obs-cliente-jr2 AS CHARACTER FORMAT "x(800)" NO-UNDO.
DEFINE VARIABLE obs-cliente-jr1 AS CHARACTER FORMAT "x(80)"  NO-UNDO.
DEFINE VARIABLE linha-jr        AS INTEGER                  NO-UNDO.
DEFINE VARIABLE pos-jr          AS INTEGER                  NO-UNDO.
DEFINE VARIABLE obs-mercado     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i-cod-refer AS INTEGER    NO-UNDO.

DEFINE VARIABLE qt-bob-otimizada AS DECIMAL    NO-UNDO.
DEFINE VARIABLE qt-plt-otimizada AS DECIMAL    NO-UNDO.

DEFINE VARIABLE pais-jr        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE nome-abrev-jr  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cod-embal-esp  AS INTEGER    NO-UNDO.
DEFINE VARIABLE qtdbob-jr      AS INTEGER    NO-UNDO.
DEFINE VARIABLE bobped-jr      AS INTEGER    NO-UNDO.
DEFINE VARIABLE qtdplt-jr      AS INTEGER    NO-UNDO.
  
DEFINE VARIABLE embalagem-jr   AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE mercado-jr     AS INTEGER    NO-UNDO.

DEFINE VARIABLE diin-jrx       AS INTEGER    NO-UNDO.

DEFINE VARIABLE i-enters    AS INTEGER    NO-UNDO.
DEFINE VARIABLE obsrom-jr   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i-jr        AS INTEGER    NO-UNDO.




/* recebimento de parƒmetros */
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
/*****************************************************************************
**
**  I-RPVAR.I - Variaveis para Impress’o do Cabecalho Padr’o (ex-CD9500.I)
**
*****************************************************************************/

define var c-empresa       as character format "x(40)"      no-undo.
define var c-titulo-relat  as character format "x(50)"      no-undo.
define var c-sistema       as character format "x(25)"      no-undo.
define var i-numper-x      as integer   format "ZZ"         no-undo.
define var da-iniper-x     as date      format "99/99/9999" no-undo.
define var da-fimper-x     as date      format "99/99/9999" no-undo.
define var c-rodape        as character                     no-undo.
define var v_num_count     as integer                       no-undo.
define var c-arq-control   as character                     no-undo.
define var i-page-size-rel as integer                       no-undo.
define var c-programa      as character format "x(08)"      no-undo.
define var c-versao        as character format "x(04)"      no-undo.
define var c-revisao       as character format "999"        no-undo.
define var c-impressora   as character                      no-undo.
define var c-layout       as character                      no-undo.
define buffer b_ped_exec_style for ped_exec.
define buffer b_servid_exec_style for servid_exec.

    define new shared stream str-rp.

 
/* i-rpvar.i */

 

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

/* defini‡Æo de frames do relat¢rio */
FORM
  tt-dados.cod-estabel   AT 01
  estabelec.nome         AT 25 NO-LABEL
  "-----------------------------------------------------------------" AT 01
  "-----------------------------------------------------------------" AT 66
  tt-dados.nr-ord-prod   AT 01 SKIP
  tt-dados.it-codigo     AT 01
  ITEM.desc-item         NO-LABEL AT 27
  tt-dados.dt-inicio     AT 01
  tt-dados.densotic      AT 27
  "-----------------------------------------------------------------" AT 01
  "-----------------------------------------------------------------" AT 66
  tt-dados.tp-pedido     FORM "x"                                     AT 01
  tt-dados.nr-pedido     NO-LABEL
  tt-dados.nr-sequencia  AT 30 
  tt-dados.cod-cliente   AT 55
  tt-dados.nome-abrev-fim     no-label 
  tt-dados.largura       AT 01
  tt-dados.diamin         AT 30
  tt-dados.diamex         AT 53 
  tt-dados.cod-referencia AT 86
  "-----------------------------------------------------------------" AT 01
  "-----------------------------------------------------------------" AT 66
  tt-dados.qt-pedida
  tt-dados.qtdbob       AT 30 
  tt-dados.dt-entrega   AT 53 
  tt-dados.qt-otimizada AT 86 
  tt-dados.qtcortada    AT 01
  tt-dados.bbcortada    AT 30
  tt-dados.embalagem    AT 53 
  tt-dados.bb-otimizada AT 86
  tt-dados.qt-faturada  AT 01
  tt-dados.bbfaturada   AT 30
  tt-dados.perc-fat-ped AT 53 
  tt-dados.qtacortar    AT 01
  tt-dados.qtbacortar   AT 30
  tt-dados.qt-palete    AT 53
  "-----------------------------------------------------------------" AT 01
  "-----------------------------------------------------------------" AT 66
  tt-dados.aplicacao    AT 01                             
  "-----------------------------------------------------------------" AT 01
  "-----------------------------------------------------------------" AT 66
  /*ord-prod.narrativa   LABEL "Informacoes do PCP/CQ" */
  tt-dados.obs-pcp-cq   LABEL "Informacoes do PCP/CQ" 
  "-----------------------------------------------------------------" AT 01
  "-----------------------------------------------------------------" AT 66
  "Restri‡äes Especiais para este Cliente e Filme:"                   AT 01
  tt-dados.obs-cliente  NO-LABEL                                      AT 01
  "-----------------------------------------------------------------" AT 01
  "-----------------------------------------------------------------" AT 66

  "Informacoes da Producao:"  SKIP(5)
  "-----------------------------------------------------------------" AT 01
  "-----------------------------------------------------------------" AT 66
  tt-dados.mxembob    AT 01
  tt-dados.qtdemen    AT 41
  "%"
  "-----------------------------------------------------------------" AT 01
  "-----------------------------------------------------------------" AT 66
  WITH FRAME f-dados SIDE-LABELS 1 DOWN WIDTH 149 NO-ATTR-SPACE STREAM-IO.
   

/* defini‡Æo de frames da embalagem */
FORM
  emb-it-codigo      LABEL "Cod.Item"     AT 01 
  emb-ref-polo       LABEL "Ref.Polo"     AT 18 
  emb-unid           LABEL "Un"           AT 35
  emb-qtde           LABEL "Quantidade"   AT 38
  emb-descricao      LABEL "Descri‡Æo"   AT 52
  with down width 132 no-box stream-io frame f-embalagem.

FORM
    with down width 132 no-box stream-io frame f-cab-embal.

FORM
    with down width 132 no-box stream-io frame f-cab-embal-rest.

/* defini‡Æo de frames das observa‡äes do cliente*/
FORM
  obs-cliente-jr1  /*  LABEL "Observa‡äes do Cliente" */   AT 01 
  with down width 132 no-box stream-io frame f-cab-embal-1.


/* include padrÆo para output de relat¢rios */
/**************************************************************************
**
** I-RPOUT - Define saída para impressão do relatório - ex. cd9520.i
** Parametros: {&stream} = nome do stream de saida no formato "stream nome"
**             {&append} = append    
**             {&tofile} = nome da variável ou campo com arquivo de destino
**             {&pagesize} = tamanho da pagina
***************************************************************************/

def new global shared var c-dir-spool-servid-exec as char no-undo.
def new global shared var i-num-ped-exec-rpw as int no-undo.

if  tt-param.destino = 1 then do:
   
    if num-entries(tt-param.arquivo,":") = 2 then do:
   .                            
    
        assign c-impressora = substring(tt-param.arquivo,1,index(tt-param.arquivo,":") - 1).
        assign c-layout     = substring(tt-param.arquivo,index(tt-param.arquivo,":") + 1,length(tt-param.arquivo) - index(tt-param.arquivo,":")). 
    .                            

    find layout_impres no-lock
        where layout_impres.nom_impressora    = c-impressora
        and   layout_impres.cod_layout_impres = c-layout no-error.
    find imprsor_usuar no-lock
        where imprsor_usuar.nom_impressora = c-impressora
        and   imprsor_usuar.cod_usuario    = tt-param.usuario
        use-index imprsrsr_id no-error.
    find impressora  of imprsor_usuar no-lock no-error.
    find tip_imprsor of impressora    no-lock no-error.

    if  i-num-ped-exec-rpw <> 0 then do:
        find b_ped_exec_style where b_ped_exec_style.num_ped_exec = i-num-ped-exec-rpw no-lock no-error. 
        find b_servid_exec_style of b_ped_exec_style no-lock no-error.
        find servid_exec_imprsor of b_servid_exec_style
          where servid_exec_imprsor.nom_impressora = imprsor_usuar.nom_impressora no-lock no-error.

        if  available b_servid_exec_style and b_servid_exec_style.ind_tip_fila_exec = 'UNIX'
        then do:
            output  through value(servid_exec_imprsor.nom_disposit_so)
                   page-size value(layout_impres.num_lin_pag) convert target tip_imprsor.cod_pag_carac_conver.
        end /* if */.
        else do:
            output   to value(servid_exec_imprsor.nom_disposit_so)
                   page-size value(layout_impres.num_lin_pag) convert target tip_imprsor.cod_pag_carac_conver.
        end /* else */.
    end.
    else do:
        if layout_impres.num_lin_pag = 0 then do:
            /* sem salta pÿgina */
            output   
                    to value(imprsor_usuar.nom_disposit_so)
                    page-size 0
                    convert target tip_imprsor.cod_pag_carac_conver . 
        end.
        else do:
            /* com salta página */
            output  
                    to value(imprsor_usuar.nom_disposit_so)
                    paged page-size value(layout_impres.num_lin_pag) 
                    convert target tip_imprsor.cod_pag_carac_conver.
        end.
    end.

    for each configur_layout_impres no-lock
        where configur_layout_impres.num_id_layout_impres = layout_impres.num_id_layout_impres
        by configur_layout_impres.num_ord_funcao_imprsor:

        find configur_tip_imprsor no-lock
            where configur_tip_imprsor.cod_tip_imprsor        = layout_impres.cod_tip_imprsor
            and   configur_tip_imprsor.cod_funcao_imprsor     = configur_layout_impres.cod_funcao_imprsor
            and   configur_tip_imprsor.cod_opc_funcao_imprsor = configur_layout_impres.cod_opc_funcao_imprsor
            use-index cnfgrtpm_id no-error.
    
        do v_num_count = 1 to extent(configur_tip_imprsor.num_carac_configur):
          case configur_tip_imprsor.num_carac_configur[v_num_count]:
            when 0 then put  control null.
            when ? then leave.
            otherwise   put  control CODEPAGE-CONVERT(chr(configur_tip_imprsor.num_carac_configur[v_num_count]),
                                                               session:cpinternal, 
                                                               tip_imprsor.cod_pag_carac_conver).
          end case.
        end.
    end.
  end.
  else do:
    
        assign c-impressora  = entry(1,tt-param.arquivo,":").
        assign c-layout      = entry(2,tt-param.arquivo,":"). 
        if num-entries(tt-param.arquivo,":") = 4 then
          assign c-arq-control = entry(3,tt-param.arquivo,":") + ":" + entry(4,tt-param.arquivo,":").
        else 
          assign c-arq-control = entry(3,tt-param.arquivo,":").
    .                            

    find layout_impres no-lock
        where layout_impres.nom_impressora    = c-impressora
        and   layout_impres.cod_layout_impres = c-layout no-error.
    find imprsor_usuar no-lock
        where imprsor_usuar.nom_impressora = c-impressora
        and   imprsor_usuar.cod_usuario    = tt-param.usuario
        use-index imprsrsr_id no-error.
    find impressora  of imprsor_usuar no-lock no-error.
    find tip_imprsor of impressora    no-lock no-error.

    if  i-num-ped-exec-rpw <> 0 then do:
        find b_ped_exec_style where b_ped_exec_style.num_ped_exec = i-num-ped-exec-rpw no-lock no-error. 
        find b_servid_exec_style of b_ped_exec_style no-lock no-error.
        find servid_exec_imprsor of b_servid_exec_style 
          where servid_exec_imprsor.nom_impressora = imprsor_usuar.nom_impressora no-lock no-error.

        if  available b_servid_exec_style and b_servid_exec_style.ind_tip_fila_exec = 'UNIX'
        then do:
            output  to value(c-dir-spool-servid-exec + "~/" + c-arq-control)
                   page-size value(layout_impres.num_lin_pag) convert target tip_imprsor.cod_pag_carac_conver.
        end /* if */.
        else do:
            output   to value(c-dir-spool-servid-exec + "~/" + c-arq-control)
                   page-size value(layout_impres.num_lin_pag) convert target tip_imprsor.cod_pag_carac_conver.
        end /* else */.
    end.
    else do:
        if layout_impres.num_lin_pag = 0 then do:
            /* sem salta pÿgina */
            output   
                    to value(c-arq-control)
                    page-size 0
                    convert target tip_imprsor.cod_pag_carac_conver . 
        end.
        else do:
            /* com salta página */
            output  
                    to value(c-arq-control)
                    paged page-size value(layout_impres.num_lin_pag) 
                    convert target tip_imprsor.cod_pag_carac_conver.
        end.
    end.

    for each configur_layout_impres no-lock
        where configur_layout_impres.num_id_layout_impres = layout_impres.num_id_layout_impres
        by configur_layout_impres.num_ord_funcao_imprsor:

        find configur_tip_imprsor no-lock
            where configur_tip_imprsor.cod_tip_imprsor        = layout_impres.cod_tip_imprsor
            and   configur_tip_imprsor.cod_funcao_imprsor     = configur_layout_impres.cod_funcao_imprsor
            and   configur_tip_imprsor.cod_opc_funcao_imprsor = configur_layout_impres.cod_opc_funcao_imprsor
            use-index cnfgrtpm_id no-error.
    
        do v_num_count = 1 to extent(configur_tip_imprsor.num_carac_configur):
          case configur_tip_imprsor.num_carac_configur[v_num_count]:
            when 0 then put  control null.
            when ? then leave.
            otherwise   put  control 
              CODEPAGE-CONVERT(chr(configur_tip_imprsor.num_carac_configur[v_num_count]),
                               session:cpinternal, 
                               tip_imprsor.cod_pag_carac_conver).
                            
          end case.
        end.
    end.
  end.  
end.
else do:
    
        if  i-num-ped-exec-rpw <> 0 then do:
          
            output  
                   to value(c-dir-spool-servid-exec + "~/" + tt-param.arquivo) 
                   paged page-size 64
                   convert target "iso8859-1" .
          
        end.                             
        else do:
          
            output  
                   to value(tt-param.arquivo) 
                   paged page-size 64 
                   convert target "iso8859-1" .
          
        end.    
    
end.

/* i-rpout */
 
 

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
/****************************************************************************
**
**  I-RPCAB.I - Form do Cabe‡alho PadrÆo e Rodap‚ (ex-CD9500.F)
**                              
** {&STREAM} - indica o nome da stream (opcional)
****************************************************************************/


    
        form header
            fill("-", 132) format "x(132)" skip
            c-empresa c-titulo-relat at 50
            "P gina:":U at 120 page-number  at 128 format ">>>>9" skip
            fill("-", 112) format "x(110)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS":U) skip(1)
            with stream-io width 132 no-labels no-box page-top frame f-cabec.
        
        form header
            fill("-", 132) format "x(132)" skip
            c-empresa c-titulo-relat at 50
            "P gina:":U at 120 page-number  at 128 format ">>>>9" skip
            "Periodo:":U i-numper-x at 10 "-"
            da-iniper-x at 15 "a":U da-fimper-x
            fill("-", 74) format "x(72)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS":U) skip(1)
            with stream-io width 132 no-labels no-box page-top frame f-cabper.
    


c-rodape = "DATASUL - ":U + c-sistema + " - " + c-prg-obj + " - V:":U + c-prg-vrs.
c-rodape = fill("-", 132 - length(c-rodape)) + c-rodape.

form header
    c-rodape format "x(132)"
    with stream-io width 132 no-labels no-box page-bottom frame f-rodape.

/* I-RPCAB.I */
 

/* bloco Inicial do programa */
find first param-global no-lock no-error.
assign c-programa       = " ESSF0025RP"
       c-versao     = "1.00"
           c-revisao    = ".00.000"
           c-empresa    = param-global.grupo.

/**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Relatorio_Programacao_de_Corte/Recorte",
                    input "*",
                    input "r") no-error.    
                    
                    
/* ut-liter.i */                    
 
assign c-titulo-relat = trim(return-value).

/**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "PRODU€ÇO",
                    input "MCP",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-sistema = return-value.

run utp/ut-acomp.p persistent set h-acomp.
/**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Gerando",
                    input "*",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
run pi-inicializar in h-acomp (input RETURN-VALUE).

/*-- Bloco Principal --*/

FIND FIRST param-cp no-lock no-error.

 /*** CORTE - 200 A 299
      METALIZA€ÇO - 300 A 399
      RECORTE - 400 A 499 ***/

FOR EACH  ord-prod 
    WHERE ord-prod.cod-estabel   =  tt-param.cod-estabel 
      AND ord-prod.nr-ord-produ  >=  tt-param.nr-ord-ini  
      AND ord-prod.nr-ord-produ  <=  tt-param.nr-ord-fim 
      AND ord-prod.nr-linha      >= 200    /*CORTE-METALIZA€ÇO-RECORTE*/
      AND ord-prod.nr-linha      <= 499 NO-LOCK:

    run pi-acompanhar in h-acomp (input string(ord-prod.nr-ord-produ)).
    
    ASSIGN embalagem-jr = "".

    CREATE tt-dados.
    ASSIGN tt-dados.nr-ord-prod = ord-prod.nr-ord-produ
           tt-dados.nr-sequencia = ord-prod.nr-sequencia
           tt-dados.cod-estabel = ord-prod.cod-estabel
           tt-dados.it-codigo   = ord-prod.it-codigo
           tt-dados.nome-abrev  = ord-prod.nome-abrev
           tt-dados.nr-pedido   = ord-prod.nr-pedido
           tt-dados.dt-inicio   = ord-prod.dt-inicio
           tt-dados.cod-referencia  = ord-prod.nr-estrut.

           tt-dados.nome-abrev-fim = tt-dados.nome-abrev.    /*alteracao unigel comercial para imprimir cliente final Edson-16-11-2012*/

            FOR FIRST ped-venda WHERE ped-venda.nome-abrev = ord-prod.nome-abrev AND
                                      ped-venda.nr-pedcli  = ord-prod.nr-pedido NO-LOCK,
                FIRST if-ped-venda WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido NO-LOCK.

                 tt-dados.nome-abrev-fim =  if-ped-venda.nome-abrev.

            END.

                                           
    /* Quantidade Cortada e Bobina Cortada - ACA descontando EAC*/
    FOR EACH  movto-estoq
        WHERE movto-estoq.nr-ord-produ = ord-prod.nr-ord-produ
          AND movto-estoq.it-codigo    = ord-prod.it-codigo
          AND movto-estoq.cod-estabel  = ord-prod.cod-estabel
          AND movto-estoq.cod-depos <> substring(param-cp.char-2,1,3) /*deposito reciclado*/
          AND (movto-estoq.esp-docto   = 1  OR
               movto-estoq.esp-docto   = 8)  NO-LOCK
               USE-INDEX ORD-SEQ :  /*ACA ou EAC*/
           
        /*Soma os acabados*/
        IF movto-estoq.esp-docto = 1 AND movto-estoq.quantidade > 0 THEN
           ASSIGN tt-dados.QtCortada = tt-dados.qtcortada + movto-estoq.quantidade
                  tt-dados.bbcortada = tt-dados.bbcortada + 1.

        /*Desconta os estornos*/
        IF movto-estoq.esp-docto = 8 AND movto-estoq.quantidade > 0 THEN
           ASSIGN tt-dados.QtCortada = tt-dados.qtcortada - movto-estoq.quantidade
                  tt-dados.bbcortada = tt-dados.bbcortada - 1.
    END.
   
    
    IF ord-prod.nr-pedido <> "" THEN DO:
        FIND ped-item 
        WHERE ped-item.nome-abrev   = ord-prod.nome-abrev
          AND ped-item.nr-pedcli    = ord-prod.nr-pedido
          AND ped-item.nr-sequencia = ord-prod.nr-sequencia
          AND ped-item.it-codigo    = ord-prod.it-codigo
          AND ped-item.cod-refer    = ord-prod.cod-refer NO-LOCK NO-ERROR.
              
        IF AVAIL ped-item THEN DO:
            ASSIGN tt-dados.qt-pedida  = ped-item.qt-pedida
                   tt-dados.dt-entrega = ped-item.dt-entrega.
/* altera‡Æo para pegar quantidade se existir ped-campanha para ordem de produ‡Æo */
        FIND FIRST ped-campanha WHERE ped-campanha.nr-ord-produ = ord-prod.nr-ord-produ NO-LOCK NO-ERROR.

        IF AVAIL ped-campanha THEN DO: 
            ASSIGN tt-dados.qt-otimizada  = ped-campanha.qt-pedida
                   tt-dados.bb-otimizada  = ped-campanha.nr-bobinas.

        END.
     
 
/* edson - 16-05-04 - colocar tipo de pedido */         
            ASSIGN tt-dados.tp-pedido = "".

            FIND first  ped-venda of ped-item NO-LOCK NO-ERROR.
            IF AVAIL ped-venda THEN
                 ASSIGN tt-dados.tp-pedido = ped-venda.tp-pedido.

            /* Percentual de Variacao da QT */

            IF ped-venda.nome-abrev = "CBE" and
               ped-venda.nome-abrev-tri <> "" THEN 
             
               FIND emitente 
                  WHERE emitente.nome-abrev = ped-venda.nome-abrev-tri
                  NO-LOCK NO-ERROR.
            
            ELSE          

              FIND emitente
                 WHERE emitente.nome-abrev = tt-dados.nome-abrev-fim   /* unigel comercial Edson 16-11-2012*/ 
                 NO-LOCK NO-ERROR.
             
            IF AVAIL emitente THEN
                 ASSIGN tt-dados.perc-fat-ped = emitente.perc-fat-ped.
            
            /* Itens da Nota Fiscal */
            FOR EACH it-nota-fisc
                WHERE it-nota-fisc.nome-ab-cli = ped-item.nome-abrev  
                  AND it-nota-fisc.nr-pedcli   = ped-item.nr-pedcli       
                  AND it-nota-fisc.it-codigo   = ped-item.it-codigo  
                  AND it-nota-fisc.cod-refer   = ped-item.cod-refer       
                  AND it-nota-fisc.cod-estabel = ord-prod.cod-estabel NO-LOCK.
            
                  ASSIGN tt-dados.qt-faturada = it-nota-fisc.qt-faturada[1].
                 
                 /*-- Contar os lotes encontrados que correspondem a qtde de bobinas  j  faturadas --*/
                 FOR EACH fat-ser-lote
                     WHERE fat-ser-lote.cod-estabel = it-nota-fisc.cod-estabel  
                       AND fat-ser-lote.serie       = it-nota-fisc.serie
                       AND fat-ser-lote.nr-nota-fis = it-nota-fisc.nr-nota-fis
                       AND fat-ser-lote.nr-seq-fat  = it-nota-fisc.nr-seq-fat      
                       /*AND fat-ser-lote.cod-depos   = it-nota-fisc.cod-depos  
                       AND fat-ser-lote.cod-localiz = it-nota-fisc.cod-localiz*/
                       AND fat-ser-lote.it-codigo   = it-nota-fisc.it-codigo
                       /*AND fat-ser-lote.cod-refer   = it-nota-fisc.cod-refer*/ NO-LOCK:
    
                       /*Na Polo s¢ o pallet ‚ faturado*/
                       FIND FIRST pallet WHERE
                                  pallet.cod-estabel = fat-ser-lote.cod-estabel AND
                                  pallet.it-codigo   = fat-ser-lote.it-codigo AND
                                  pallet.nr-pallet   = fat-ser-lote.nr-serlote NO-LOCK NO-ERROR.
                       IF AVAIL pallet THEN 
                          ASSIGN tt-dados.bbfaturada = tt-dados.bbfaturada + pallet.nr-bobinas.
                 END.
                   
             END.
           
             FIND FIRST cot-est-mast
                 WHERE cot-est-mast.item-cotacao = ped-item.it-codigo 
                   AND cot-est-mast.nr-estrut    = ped-item.nr-config NO-LOCK NO-ERROR.
    
             IF AVAIL cot-est-mast THEN DO:
                 /* Resultado Vari veis da Estrutura */
    
                 /*Cliente */
                 FIND var-result 
                     WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                          AND var-result.nr-estrut    = cot-est-mast.nr-estrut
                          AND var-result.nome-var     = "PEDCLI"  NO-LOCK NO-ERROR.
                    
                 IF AVAIL var-result THEN
                     ASSIGN  tt-Dados.cod-cliente = var-result.des-result.
    
                 /*Largura */
                 FIND var-result 
                     WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                       AND var-result.nr-estrut    = cot-est-mast.nr-estrut
                       AND var-result.nome-var     = "LARGURA"  NO-LOCK NO-ERROR.
    
                 IF AVAIL var-result then
                     ASSIGN tt-Dados.largura = var-result.des-result.

                 /*Densidade Otica */
                 FIND var-result 
                     WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                       AND var-result.nr-estrut    = cot-est-mast.nr-estrut
                       AND var-result.nome-var     = "DENSOTIC"  NO-LOCK NO-ERROR.
    
                 IF AVAIL var-result then
                     ASSIGN tt-Dados.densotic = var-result.des-result.

                  /*Qtd. Paletes */
                 FIND var-result 
                     WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                       AND var-result.nr-estrut    = cot-est-mast.nr-estrut
                       AND var-result.nome-var     = "QTDPALETE"  NO-LOCK NO-ERROR.
    
                 IF AVAIL var-result then
                     ASSIGN tt-Dados.qt-palete = int(var-result.des-resul).


    
                  /*Diametro Interno */
                 FIND var-result 
                     WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                       AND var-result.nr-estrut    = cot-est-mast.nr-estrut
                       AND var-result.nome-var     = "DIIN"  NO-LOCK NO-ERROR.
    
                 IF AVAIL var-result then
                     ASSIGN tt-Dados.diamin = var-result.des-result.
    
                  /*Diametro Externo */
                 FIND var-result 
                     WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                          AND var-result.nr-estrut    = cot-est-mast.nr-estrut
                          AND var-result.nome-var     = "DIEX"  NO-LOCK no-error.
    
                 IF AVAIL var-result then
                     ASSIGN tt-Dados.diamex = var-result.des-result.
    
                  /*QTD.Bobina */
                 FIND var-result 
                     WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                       AND var-result.nr-estrut    = cot-est-mast.nr-estrut
                       AND var-result.nome-var     = "QTDBOB"  NO-LOCK no-error.
    
                 IF AVAIL var-result then
                     ASSIGN tt-Dados.qtdbob = int(var-result.des-result).
                    
                    
                   /*Embalagem */
                 FIND var-result 
                     WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                       AND var-result.nr-estrut    = cot-est-mast.nr-estrut
                       AND var-result.nome-var     = "CODEMBAL"  NO-LOCK no-error.
    
                 IF AVAIL var-result then
                     ASSIGN tt-Dados.embalagem = var-result.des-result.

                 
                  /*Aplicacao */
                 FIND var-result 
                     WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                       AND var-result.nr-estrut    = cot-est-mast.nr-estrut
                       AND var-result.nome-var     = "APLIC"  NO-LOCK no-error.
                 
                 IF AVAIL var-result then
                     ASSIGN tt-Dados.aplicacao = var-result.des-result.


                 /*Obs PCP/CQ*/

                 FIND var-result 
                     WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                       AND var-result.nr-estrut    = cot-est-mast.nr-estrut
                       AND var-result.nome-var     = "OBSGER"  NO-LOCK no-error.
                 
                 IF AVAIL var-result then
                     ASSIGN c-obs-ger = var-result.des-result.

                 FIND var-result 
                     WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                       AND var-result.nr-estrut    = cot-est-mast.nr-estrut
                       AND var-result.nome-var     = "OBSEMB"  NO-LOCK no-error.
                 
                 IF AVAIL var-result then
                     ASSIGN c-obs-emb = var-result.des-result.
                 
                 FIND var-result 
                     WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                       AND var-result.nr-estrut    = cot-est-mast.nr-estrut
                       AND var-result.nome-var     = "OBSCQ"  NO-LOCK no-error.
                 
                 IF AVAIL var-result then
                     ASSIGN c-obs-cq = var-result.des-result.
                 
/* Edson - altera‡Æo para cliente propaCk se surgir mais criar solu‡Æo especifica*/
                 IF tt-dados.nome-abrev-fim = "PROPACK" THEN   /*unigel comercial 16-11-2012 Edson*/
                    ASSIGN c-obs-emb = c-obs-emb + " (Necess rio etiqueta no tubete e final do filme)".                 

                 ASSIGN tt-dados.obs-pcp-cq = c-obs-emb + " - " + c-obs-cq + " - " + c-obs-ger.

                 /* Altera‡äes solicitadas pelo Rolim nas ordens de Produ‡Æo */

                 IF INT (emitente.natureza) <> 3 THEN 
                     ASSIGN obs-mercado =
                      "Mercado: Interno - Considerar atendimento, o peso solicitado preferencialmente nos " +
                         STRING (emitente.perc-fat-ped) + " % a maior".
                 ELSE 
                     ASSIGN obs-mercado =
                      "Mercado: Externo  - Considerar atendimento, o numero de bobinas e diametro externo solicitado".

                 IF ord-prod.cod-estabel = "423" THEN
                     ASSIGN obs-mercado = "".

                 ASSIGN tt-dados.obs-pcp-cq = tt-dados.obs-pcp-cq + " " + obs-mercado.

                 /* fim das Altera‡äes solicitadas pelo Rolim nas ordens de Produ‡Æo */

                 ASSIGN obs-cliente-jr2 = "".

                 RUN obs-cliente-restricoes.

                 ASSIGN tt-dados.obs-cliente = obs-cliente-jr2.


                 /*Maximo Emenda Bobina */
                 FIND var-result 
                     WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                       AND var-result.nr-estrut    = cot-est-mast.nr-estrut
                       AND var-result.nome-var     = "EMENBOB"  NO-LOCK NO-ERROR.
                 
                 IF AVAIL var-result then
                     ASSIGN tt-Dados.mxembob = var-result.des-result.

                 IF tt-Dados.cod-estabel = "423" THEN
                     ASSIGN tt-Dados.mxembob = "  ".
    
                 /*Quantidade de Emenda */
                 FIND var-result 
                     WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                       AND var-result.nr-estrut    = cot-est-mast.nr-estrut
                       AND var-result.nome-var     = "QTDEMEN"  NO-LOCK NO-ERROR.
                 
                 IF AVAIL var-result then
                     ASSIGN tt-Dados.qtdemen = var-result.des-result.
                 
                 IF tt-Dados.cod-estabel = "423" THEN
                     ASSIGN tt-Dados.qtdemen = "  ".
    

             END.
             /*ASSIGN tt-dados.qtacortar  =  tt-dados.qt-pedida - tt-dados.qtcortada.
                    tt-dados.qtbacortar =  dec(tt-dados.qtdbob) - tt-dados.bbcortada.*/
    
             ASSIGN tt-dados.qtacortar  =  IF (tt-dados.qt-pedida - tt-dados.qtcortada) > 0
                                           THEN tt-dados.qt-pedida - tt-dados.qtcortada
                                           ELSE 0 
                    tt-dados.qtbacortar =  IF (dec(tt-dados.qtdbob) - tt-dados.bbcortada) > 0
                                           THEN dec(tt-dados.qtdbob) - tt-dados.bbcortada
                                           ELSE 0
                    c-obs-emb           = ""
                    c-obs-GER           = ""
                    c-obs-cq            = "".                         
    
        END. /*IF AVAIL ped-item*/
    END. /*OP tem pedido relacionado*/

    ELSE DO: /*Qdo OP nÆo tem pedido relacionado */

        /* altera‡Æo para pegar quantidade se existir ped-campanha para ordem de produ‡Æo */
        FIND FIRST ped-campanha WHERE ped-campanha.nr-ord-produ = ord-prod.nr-ord-produ NO-LOCK NO-ERROR.

        IF AVAIL ped-campanha THEN DO: 
            ASSIGN tt-dados.qt-otimizada  = ped-campanha.qt-pedida
                   tt-dados.bb-otimizada  = ped-campanha.nr-bobinas.

        END.
          ASSIGN i-cod-refer = ord-prod.nr-estrut.

      IF ord-prod.nr-estrut <> INT(ord-prod.cod-refer) THEN
          ASSIGN  i-cod-refer = INT(ord-prod.cod-refer).
      /* Resultado Vari veis da Estrutura */

           /*Qtde Pedido que est  no configurador*/
             FIND var-result 
                 WHERE var-result.item-cotacao = ord-prod.it-codigo
                   AND var-result.nr-estrut    = i-cod-refer
                   AND var-result.nome-var     = "QTDPEDIDO"  NO-LOCK NO-ERROR.
                
             IF AVAIL var-result THEN
                 ASSIGN tt-dados.qt-pedida = dec(var-result.des-result).

             /*Cliente */
             FIND var-result 
                 WHERE var-result.item-cotacao = ord-prod.it-codigo
                   AND var-result.nr-estrut    = i-cod-refer
                   AND var-result.nome-var     = "PEDCLI"  NO-LOCK NO-ERROR.
                
             IF AVAIL var-result THEN
                 ASSIGN  tt-Dados.cod-cliente = var-result.des-result.
                 
             /*Largura */
             FIND var-result 
                 WHERE var-result.item-cotacao = ord-prod.it-codigo
                   AND var-result.nr-estrut    = i-cod-refer
                   AND var-result.nome-var     = "LARGURA"  NO-LOCK NO-ERROR.
                IF AVAIL var-result THEN
                    ASSIGN tt-Dados.largura = var-result.des-result.

             /*Densidade àtica */
             FIND var-result 
                 WHERE var-result.item-cotacao = ord-prod.it-codigo
                   AND var-result.nr-estrut    = i-cod-refer
                   AND var-result.nome-var     = "DENSOTIC"  NO-LOCK NO-ERROR.
                IF AVAIL var-result THEN
                    ASSIGN tt-Dados.densotic = var-result.des-result.

             /*Qtd. Paletes */
             FIND var-result 
                WHERE var-result.item-cotacao = ord-prod.it-codigo
                  AND var-result.nr-estrut    = i-cod-refer
                  AND var-result.nome-var     = "QTDPALETE"  NO-LOCK NO-ERROR.
    
                IF AVAIL var-result then
                   ASSIGN tt-Dados.qt-palete = int(var-result.des-resul).



              /*Diametro Interno */
             FIND var-result 
                 WHERE var-result.item-cotacao = ord-prod.it-codigo
                   AND var-result.nr-estrut    = i-cod-refer
                   AND var-result.nome-var     = "DIIN"  NO-LOCK NO-ERROR.

             IF AVAIL var-result then
                 ASSIGN tt-Dados.diamin = var-result.des-result.

              /*Diametro Externo */
             FIND var-result 
                 WHERE var-result.item-cotacao = ord-prod.it-codigo
                   AND var-result.nr-estrut    = i-cod-refer
                   AND var-result.nome-var     = "DIEX"  NO-LOCK no-error.

             IF AVAIL var-result then
                 ASSIGN tt-Dados.diamex = var-result.des-result.

              /*QTD.Bobina */
             FIND FIRST var-result 
                 WHERE var-result.item-cotacao = ord-prod.it-codigo
                   AND var-result.nr-estrut    = i-cod-refer
                   AND var-result.nome-var     = "QTDBOB"  NO-LOCK no-error.

             IF AVAIL var-result then
                 ASSIGN tt-Dados.qtdbob = int(var-result.des-result).

                
               /*Embalagem */
             FIND var-result 
                 WHERE var-result.item-cotacao = ord-prod.it-codigo
                   AND var-result.nr-estrut    = i-cod-refer
                   AND var-result.nome-var     = "CODEMBAL"  NO-LOCK no-error.

             IF AVAIL var-result then
                 ASSIGN tt-Dados.embalagem = var-result.des-result.


             

/* ---------- Rotina para buscar c¢digo de embalagem especial para ordens
              sem pedido de venda. -------------------------------------------*/

       IF INT (tt-dados.largura) <> 0 and INT (tt-dados.diamex) <> 0 and 
          INT (tt-dados.diamin)  <> 0 THEN do:
    


           ASSIGN diin-jrx = INT (tt-dados.diamin).

           IF diin-jrx = 153 THEN
               ASSIGN diin-jrx = 6.
           ELSE
               ASSIGN diin-jrx = 3.

           /* rotina para busca nas embalagens especiais */


           ASSIGN cod-embal-esp = 0
                  pais-jr       = "BRASIL".
    
    
           IF cod-embal-esp = 0 AND pais-jr <> "" THEN DO:
    
              FOR EACH polo-embalagem-esp WHERE
                  polo-embalagem-esp.nome-abrev = tt-dados.nome-abrev AND
                  polo-embalagem-esp.pais       = pais-jr
                  NO-LOCK ,
           
                  EACH polo-embalagem WHERE
                       polo-embalagem.cod-embal   = polo-embalagem-esp.cod-embal AND
                       /*polo-embalagem.cod-mercado = 3            AND*/
                       polo-embalagem.diin  = diin-jrx                       AND
                       INT (tt-dados.diamex) >= polo-embalagem.diex-ini      AND
                       INT (tt-dados.diamex) <= polo-embalagem.diex-fim      AND
                       INT (tt-dados.largura) >= polo-embalagem.larg-bob-ini AND
                       INT (tt-dados.largura) <= polo-embalagem.larg-bob-fim AND
                       polo-embalagem.situacao = "A"
                       NO-LOCK.
           
                   ASSIGN cod-embal-esp = polo-embalagem-esp.cod-embal
                          qtdbob-jr     = polo-embalagem.qt-bobinas.

    
              END.
    
           END.
           
           /*--------------------------------------------*/
    
           IF cod-embal-esp <> 0 THEN
               ASSIGN embalagem-jr = STRING (cod-embal-esp).
           ELSE DO:

             ASSIGN mercado-jr = 1. 

             
             FOR EACH polo-embalagem NO-LOCK WHERE
                    polo-embalagem.cod-mercado = mercado-jr AND
                    polo-embalagem.diin    = diin-jrx                AND
                    INT (tt-dados.diamex) >= polo-embalagem.diex-ini AND
                    INT (tt-dados.diamex) <= polo-embalagem.diex-fim AND
                    INT (tt-dados.largura) >= polo-embalagem.larg-bob-ini AND
                    INT (tt-dados.largura) <= polo-embalagem.larg-bob-fim AND
                    polo-embalagem.situacao = "A".
            
                ASSIGN embalagem-jr = string (polo-embalagem.cod-embal)
                       qtdbob-jr    = polo-embalagem.qt-bobinas.
            

             END.
            
             /* Verifica se o mercado ‚ interno e nÆo encontrou a embalagem,
                procurar pelo mercado externo. */
            
             IF embalagem-jr = "" AND mercado-jr = 1 THEN DO:
            
                ASSIGN mercado-jr = 2.
                
                FOR EACH polo-embalagem NO-LOCK WHERE
                    polo-embalagem.cod-mercado = mercado-jr AND
                    polo-embalagem.diin    = diin-jrx                AND
                    INT (tt-dados.diamex) >= polo-embalagem.diex-ini AND
                    INT (tt-dados.diamex) <= polo-embalagem.diex-fim AND
                    INT (tt-dados.largura) >= polo-embalagem.larg-bob-ini AND
                    INT (tt-dados.largura) <= polo-embalagem.larg-bob-fim AND
                    polo-embalagem.situacao = "A".
            
                  ASSIGN embalagem-jr = string (polo-embalagem.cod-embal)
                         qtdbob-jr    = polo-embalagem.qt-bobinas.
            

                END.  
            
             END.
    

             
           END.   /* cod-embal-jr = 0 */
    
           /*----------------Fim da Rotina de Troca de Mercado-----------*/
    
         IF embalagem-jr <> "" THEN DO:
    
            ASSIGN tt-dados.embalagem = STRING (embalagem-jr).

            FIND var-result 
                WHERE var-result.item-cotacao = ord-prod.it-codigo
                  AND var-result.nr-estrut    = i-cod-refer
                  AND var-result.nome-var     = "CODEMBAL"  exclusive-LOCK no-error.


            IF AVAIL var-result THEN 
               ASSIGN var-result.valor-char = embalagem-jr
                      var-result.des-result = embalagem-jr
                      VAR-result.valor-dec  = INT(embalagem-jr).
         
            Find var-result WHERE  var-result.item-cotacao = ord-prod.it-codigo AND 
                                   var-result.nr-estrut = i-cod-refer           AND
                                   var-result.nome-var = "bobpalete" EXCLUSIVE-LOCK  NO-ERROR.
            IF AVAIL var-result THEN 
               ASSIGN var-result.valor-char = STRING (qtdbob-jr)
                      var-result.des-result = STRING (qtdbob-jr)
                      VAR-result.valor-dec  = qtdbob-jr.
            

            Find var-result WHERE  var-result.item-cotacao = ord-prod.it-codigo AND 
                                   var-result.nr-estrut = i-cod-refer AND
                                   var-result.nome-var = "qtdbob" NO-LOCK NO-ERROR.

            IF AVAIL var-result THEN DO:
            
               ASSIGN bobped-jr = INT (var-result.des-result)
                      qtdplt-jr = TRUNCATE (((bobped-jr / qtdbob-jr) + 0.9999999), 0).
           
               Find var-result WHERE  var-result.item-cotacao = ord-prod.it-codigo AND 
                                      var-result.nr-estrut = i-cod-refer           AND
                                      var-result.nome-var = "qtdpalete" EXCLUSIVE-LOCK NO-ERROR.
               IF AVAIL var-result THEN 
                  ASSIGN var-result.valor-char = STRING (qtdplt-jr)
                         var-result.des-result = STRING (qtdplt-jr)
                         VAR-result.valor-dec  = qtdplt-jr
                         tt-Dados.qt-palete = int(var-result.des-resul).

           /* logica para calculo de quantidade de bobinas no ultimo palete - EDSON 17/03/2006*/
           
           
                       Find var-result WHERE  var-result.item-cotacao = ord-prod.it-codigo AND 
                                               var-result.nr-estrut = i-cod-refer          AND
                                               var-result.nome-var = "qbultpale" EXCLUSIVE-LOCK NO-ERROR.
                       IF AVAIL var-result THEN           
                             ASSIGN VAR-result.valor-dec   =  (bobped-jr - ( qtdbob-jr * ( qtdplt-jr - 1)))
                                    var-result.valor-char  =  string(VAR-result.valor-dec)
                                    var-result.des-result  =  var-result.valor-char.
    
          /* --------------fim quantidade de bobinas no ultimo palete ---------*/ 


            END. /* if AVAIL var-result */

           

         END.  /* IF embalagem-jr <> "" */

       END. /* Existe Largura,Diin e Diex */

       /* Fim da Rotina para buscar c¢digo de embalagem especial para ordens
          sem pedido de venda. */

             
              /*Aplicacao */
             FIND var-result 
                 WHERE var-result.item-cotacao = ord-prod.it-codigo
                   AND var-result.nr-estrut    = i-cod-refer
                   AND var-result.nome-var     = "APLIC"  NO-LOCK no-error.
             
             IF AVAIL var-result then
                 ASSIGN tt-Dados.aplicacao = var-result.des-result.


                 FIND var-result 
                     WHERE var-result.item-cotacao = ord-prod.it-codigo
                       AND var-result.nr-estrut    = i-cod-refer
                       AND var-result.nome-var     = "OBSGER"  NO-LOCK no-error.
                 
                 IF AVAIL var-result then
                     ASSIGN c-obs-ger = var-result.des-result.

             /*Obs PCP/CQ*/
                 FIND var-result 
                     WHERE var-result.item-cotacao = ord-prod.it-codigo
                       AND var-result.nr-estrut    = i-cod-refer
                       AND var-result.nome-var     = "OBSEMB"  NO-LOCK no-error.
                 
                 IF AVAIL var-result then
                     ASSIGN c-obs-emb = var-result.des-result.
                 
                 FIND var-result 
                     WHERE var-result.item-cotacao = ord-prod.it-codigo
                       AND var-result.nr-estrut    = i-cod-refer
                       AND var-result.nome-var     = "OBSCQ"  NO-LOCK no-error.
                 
                 IF AVAIL var-result then
                     ASSIGN c-obs-cq = var-result.des-result.
                 
                 ASSIGN tt-Dados.obs-pcp-cq = c-obs-emb + " - " + c-obs-cq + " - " + c-obs-GER.
             
             /*Maximo Emenda Bobina */
             FIND var-result 
                 WHERE var-result.item-cotacao = ord-prod.it-codigo
                   AND var-result.nr-estrut    = i-cod-refer
                   AND var-result.nome-var     = "EMENBOB"  NO-LOCK NO-ERROR.
             
             IF AVAIL var-result then
                 ASSIGN tt-Dados.mxembob = var-result.des-result.

             /*Quantidade de Emenda */
             FIND var-result 
                 WHERE var-result.item-cotacao = ord-prod.it-codigo
                   AND var-result.nr-estrut    = i-cod-refer
                   AND var-result.nome-var     = "QTDEMEN"  NO-LOCK NO-ERROR.
             
             IF AVAIL var-result then
                 ASSIGN tt-Dados.qtdemen = var-result.des-result.        

             ASSIGN tt-dados.qtacortar  =  IF (tt-dados.qt-pedida - tt-dados.qtcortada) > 0
                                           THEN tt-dados.qt-pedida - tt-dados.qtcortada
                                           ELSE 0 
                    tt-dados.qtbacortar =  IF (dec(tt-dados.qtdbob) - tt-dados.bbcortada) > 0
                                           THEN dec(tt-dados.qtdbob) - tt-dados.bbcortada
                                           ELSE 0
                    c-obs-emb           = ""
                    c-obs-GER           = ""
                    c-obs-cq            = "".   



    END. /*quando ord-prod.nr-pedido = "" */

    
END.


/* Impressao do Relatorio */

view frame f-cabec.
view frame f-rodape.

FOR EACH tt-dados:
    
    run pi-acompanhar in h-acomp (input string(tt-dados.nr-ord-prod)).
    
    FIND FIRST estabelec 
        WHERE estabelec.cod-estabel = tt-dados.cod-estabel NO-LOCK NO-ERROR.

    FIND FIRST ITEM 
        WHERE item.it-codigo = tt-dados.it-codigo NO-LOCK NO-ERROR.

    FIND FIRST ord-prod
        WHERE ord-prod.nr-ord-produ = tt-dados.nr-ord-prod NO-LOCK NO-ERROR.
    
    FIND FIRST b-emitente WHERE b-emitente.nome-abrev = tt-dados.nome-abrev-fim NO-LOCK NO-ERROR.

    display tt-dados.cod-estabel
            estabelec.nome 
            tt-dados.nr-ord-prod 
            tt-dados.nr-sequencia
            tt-dados.it-codigo
            ITEM.desc-item    
            tt-dados.dt-inicio
            tt-dados.densotic
            tt-dados.tp-pedido
            tt-dados.nr-pedido
            tt-dados.cod-cliente
            IF (tt-dados.cod-estabel = "423" AND AVAIL b-emitente) THEN b-emitente.nome-emit  
                    ELSE tt-dados.nome-abrev + " - " + tt-dados.nome-abrev-fim @ tt-dados.nome-abrev-fim FORMAT "x(60)"
            tt-dados.largura
            tt-dados.diamin
            tt-dados.diamex
            tt-dados.cod-referencia
            tt-dados.qtdbob
            tt-dados.qt-pedida
            tt-dados.dt-entrega
            tt-dados.qt-otimizada
            tt-dados.bb-otimizada
            tt-dados.qtcortada
            tt-dados.bbcortada
            tt-dados.embalagem
            tt-dados.qt-faturada
            tt-dados.bbfaturada
            tt-dados.perc-fat-ped
            tt-dados.qtacortar
            tt-dados.qtbacortar
            tt-dados.qt-palete
            tt-dados.aplicacao
            tt-dados.obs-pcp-cq /*ord-prod.narrativa*/
            tt-dados.obs-cliente
            tt-dados.mxembob
            tt-dados.qtdemen
            WITH FRAME f-dados.
     
     RUN estrutura-embalagem.
     RUN obs-cliente.
     RUN obs-cliente-restricoes.  
  /*   RUN obs-rom.   */
     
   PAGE.
END.
 
/* fechamento do output do relat¢rio  */
run pi-finalizar in h-acomp.
/**************************************************************************
**
** I-RPCLO - Define sa¡da para impressÆo do relat¢rio - ex. cd9540.i
** Parametros: {&stream} = nome do stream de saida no formato "stream nome"
***************************************************************************/

output  close.

/* i-rpout */

PROCEDURE estrutura-embalagem.

    FOR EACH tt-embalagem.
        DELETE tt-embalagem.
    END.
    
    FIND FIRST polo-embalagem WHERE
        polo-embalagem.cod-embal = INT (tt-dados.embalagem)
        NO-LOCK NO-ERROR.

    IF NOT AVAIL polo-embalagem THEN RETURN.


    ASSIGN qt-bob-otimizada = tt-dados.qtdbob
           qt-plt-otimizada = tt-dados.qt-palete.
    
    IF tt-dados.bb-otimizada <> 0 THEN
        ASSIGN qt-bob-otimizada = tt-dados.bb-otimizada
               qt-plt-otimizada = TRUNCATE ( ((tt-dados.bb-otimizada / polo-embalagem.qt-bobinas) + 0.9999),0).
    
    FOR EACH polo-embalagem-estrut WHERE
        polo-embalagem-estrut.cod-embal = polo-embalagem.cod-embal
        NO-LOCK.
        
        FIND FIRST ITEM WHERE 
            ITEM.it-codigo = polo-embalagem-estrut.it-codigo
            NO-LOCK NO-ERROR.

        IF NOT AVAIL ITEM THEN next. 

        CREATE tt-embalagem.
        ASSIGN emb-it-codigo = polo-embalagem-estrut.it-codigo
               emb-ref-polo  = ITEM.codigo-refer
               emb-descricao = ITEM.DESC-item
               emb-unid      = ITEM.un.
        
        IF polo-embalagem-estrut.tipo-cons = "P" THEN
           ASSIGN emb-qtde = polo-embalagem-estrut.quantidade * qt-plt-otimizada.
           ELSE
            ASSIGN emb-qtde = polo-embalagem-estrut.quantidade * qt-bob-otimizada.

    END.

    display 
        "PREVISÇO DE CONSUMO DE EMBALAGENS" AT 25
        "-------------------------------- " AT 25
        " " AT 25
        with stream-io frame f-cab-embal.
        down with frame f-cab-embal.

    FOR EACH tt-embalagem.
       
       display emb-it-codigo 
               emb-ref-polo  
               emb-unid
               emb-qtde 
               emb-descricao
               with stream-io frame f-embalagem.
               down with frame f-embalagem.
    
    END.

END PROCEDURE.


PROCEDURE obs-cliente.
    
    FIND FIRST polo-laudo-cliente WHERE
        polo-laudo-cliente.nome-abrev = tt-dados.nome-abrev-fim   /*unigel comercial - 16-11-2012 - Edson*/
        NO-LOCK NO-ERROR.

    IF NOT AVAIL polo-laudo-cliente THEN RETURN.
    
    display 
        "OBSERVA€åES DO CLIENTE" AT 29
        "----------------------" AT 29
        with stream-io frame f-cab-embal.

    ASSIGN obs-cliente-jr = polo-laudo-cliente.obs-cliente.

    run pi-print-editor (input obs-cliente-jr ,
                         input 080).
    for each tt-editor:
    
        PUT tt-editor.conteudo AT 01.
    end.

END PROCEDURE. 

PROCEDURE obs-cliente-restricoes.

    ASSIGN obs-cliente-jr2 = "".

    FIND FIRST  polo-esp-cliente-cq WHERE 
         polo-esp-cliente-cq.cod-estabel = tt-dados.cod-estabel AND 
         polo-esp-cliente-cq.nome-abrev  = tt-dados.nome-abrev-fim  AND   /*unigel comercial - 16-11-2012 - Edson*/
         polo-esp-cliente-cq.it-codigo   = tt-dados.it-codigo   AND
         polo-esp-cliente-cq.cod-comp    = 0                    AND
         polo-esp-cliente-cq.cod-exame   = 0 
         USE-INDEX cli-item NO-LOCK NO-ERROR.

    IF NOT AVAIL polo-esp-cliente-cq THEN DO:

      FIND FIRST  polo-esp-cliente-cq WHERE 
           polo-esp-cliente-cq.nome-abrev = tt-dados.nome-abrev-fim    AND  /*unigel comercial - 16-11-2012 - Edson*/
           polo-esp-cliente-cq.it-codigo  = tt-dados.it-codigo     AND  
           polo-esp-cliente-cq.cod-comp   = 0                      AND
           polo-esp-cliente-cq.cod-exame  = 0                      AND
           polo-esp-cliente-cq.log-imprime-alvo = YES 
           USE-INDEX cli-item NO-LOCK NO-ERROR.
     
    END.

    IF AVAIL polo-esp-cliente-cq THEN  
       ASSIGN obs-cliente-jr2 = polo-esp-cliente-cq.descricao.

    /* Para todos os itens */
    
    FIND FIRST  polo-esp-cliente-cq WHERE 
         polo-esp-cliente-cq.cod-estabel = tt-dados.cod-estabel AND 
         polo-esp-cliente-cq.nome-abrev  = tt-dados.nome-abrev-fim  AND  /*unigel comercial - 16-11-2012 - Edson*/
         polo-esp-cliente-cq.it-codigo   = ""                   AND 
         polo-esp-cliente-cq.cod-comp    = 0                    AND
         polo-esp-cliente-cq.cod-exame   = 0                      
         USE-INDEX cli-item NO-LOCK NO-ERROR.

    IF NOT AVAIL polo-esp-cliente-cq THEN DO:

      FIND FIRST  polo-esp-cliente-cq WHERE 
           polo-esp-cliente-cq.nome-abrev = tt-dados.nome-abrev-fim    AND  /*unigel comercial - 16-11-2012 - Edson*/
           polo-esp-cliente-cq.it-codigo  = ""                     AND
           polo-esp-cliente-cq.cod-comp   = 0                      AND
           polo-esp-cliente-cq.cod-exame  = 0                      AND
           polo-esp-cliente-cq.log-imprime-alvo = YES 
           USE-INDEX cli-item NO-LOCK NO-ERROR.
     

    END.         

    IF AVAIL  polo-esp-cliente-cq THEN 
       ASSIGN obs-cliente-jr2 = obs-cliente-jr2 + chr(10) +
              polo-esp-cliente-cq.descricao.

    /* Fim Para todos os itens */

END PROCEDURE. 

PROCEDURE obs-rom.
    
    FIND FIRST polo-laudo-cliente WHERE
        polo-laudo-cliente.nome-abrev = tt-dados.nome-abrev-fim /*unigel comercial - 16-11-2012 - Edson*/
        NO-LOCK NO-ERROR.

    IF NOT AVAIL polo-laudo-cliente THEN RETURN.

    IF trim(polo-laudo-cliente.obs-devol) = "" THEN  RETURN.
    
    display 
        "OBSERVA€åES DO MARKETING - EMBARQUES ANTERIORES" AT 16 NO-LABEL
        "-----------------------------------------------" AT 16 NO-LABEL
        with stream-io frame f-cab-embal-x.


    ASSIGN i-enters = NUM-ENTRIES(polo-laudo-cliente.obs-devol,CHR(10)).

    IF i-enters = 0 THEN NEXT.

    ASSIGN i-jr = i-enters.

    DO WHILE i-jr > 0.

        ASSIGN obsrom-jr = ENTRY(i-jr, polo-laudo-cliente.obs-devol,CHR(10)).

        IF TRIM(obsrom-jr) <> "" THEN do:

          ASSIGN obs-cliente-jr = obsrom-jr
                 linha-jr = 1
                 pos-jr   = 1.

          run pi-print-editor (input obs-cliente-jr ,
                               input 080).
          FOR EACH tt-editor.
    
              PUT tt-editor.conteudo AT 01.
        
          end.

        end.
 
        ASSIGN i-jr = i-jr - 1.

    END.

   
END PROCEDURE.     


procedure pi-print-editor:

    def input param c-editor    as char    no-undo.
    def input param i-len       as integer no-undo.

    def var i-linha  as integer no-undo.
    def var i-aux    as integer no-undo.
    def var c-aux    as char    no-undo.
    def var c-ret    as char    no-undo.

    for each tt-editor:
        delete tt-editor.
    end.

    assign c-ret = chr(255) + chr(255).

    do  while c-editor <> "":
        if  c-editor <> "" then do:
            assign i-aux = index(c-editor, chr(10)).
            if  i-aux > i-len or (i-aux = 0 and length(c-editor) > i-len) then
                assign i-aux = r-index(c-editor, " ", i-len + 1).
            if  i-aux = 0 then
                assign c-aux = substr(c-editor, 1, i-len)
                       c-editor = substr(c-editor, i-len + 1).
            else
                assign c-aux = substr(c-editor, 1, i-aux - 1)
                       c-editor = substr(c-editor, i-aux + 1).
            if  i-len = 0 then
                assign entry(1, c-ret, chr(255)) = c-aux.
            else do:
                assign i-linha = i-linha + 1.
                create tt-editor.
                assign tt-editor.linha    = i-linha
                       tt-editor.conteudo = c-aux.
            end.
        end.
        if  i-len = 0 then
            return c-ret.
    end.
    return c-ret.
end procedure.



return "OK":U.

/* Fim do programa */       
       

/* Layout do Relatorio
         1         2         3         4         5         6         7         8         9         0
1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123
-------------------------------------------------------------------------------------------------------
Datasul S.A.     RELATàRIO PROGRAMA€ÇO DE CORTE / RECORTE                       P gina:     1
--------------------------------------------------------------------------------09/11/2003 - 17:49:00

Estabelecimento:  121   POLO-TNT                                      
-------------------------------------------------------------------------------------------------------
Ord.Prod..: 316984        
Produto: 30TSY32          COEX 30 MY             
Data.: 07/11/2003                   
-------------------------------------------------------------------------------------------------------
Pedido....: P  8307       Cliente...:  10380        CHEMTON
Largura...:   1380        Diam. Int.:    153        Diam. Ext.:    584
-------------------------------------------------------------------------------------------------------
Qtd.Pedida:      4932.00  Qtd. Bob..:    017        Entrega...: 28/11/2003
Qt.Cortada:         0.00  BB.Cortada:    000        Embalagem.:   114066
Qt.Faturad:         0.00  BB.Faturad:    000        %Variac.Qt:  5
Q.A Cortar:      4932.00  B.A Cortar:    017        
-------------------------------------------------------------------------------------------------------
Aplicacao.:    101  BOMBONS, BALAS, DOCES E GOMAS
-------------------------------------------------------------------------------------------------------
Informacoes do PCP/CQ:



------------------------------------------------------------------------------------------------------
Informa‡äes da Produ‡Æo:




-------------------------------------------------------------------------------------------------------
Maximo Emenda por Bobina.:     100      Maximo Emenda por Pedido.:     100%
-------------------------------------------------------------------------------------------------------

*/
