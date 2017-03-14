/*------------------------------------------------------------------------
File.............: esfas0007.w
Description......: Gera Bem Patrimonio
Input Parameters : 
Output Parameters: 
Author...........: Ercole Ricci 
Created..........: 21/09/2007
OBS..............: 
------------------------------------------------------------------------*/
/*********************************************************
Author...........: Amilton Pedro Usatai - DLC 
LAST UPDATE..........: 21/09/2007
Remarks..............: Solicita‡Æo 119 - Jaqueline
                         NÆo estava importando relacionamentos da nota
                         de entrada para os bens
***********************************************************/
             
&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-relat 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i esfas0007 1.00.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Preprocessadores do Template de Relat¢rio                            */
/* Obs: Retirar o valor do preprocessador para as p ginas que nÆo existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
/*&GLOBAL-DEFINE PGCLA f-pg-cla
 * &GLOBAL-DEFINE PGPAR f-pg-par
 * &GLOBAL-DEFINE PGDIG f-pg-dig*/
&GLOBAL-DEFINE PGIMP f-pg-imp
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

define temp-table tt-param no-undo
    field destino           as integer
    field arquivo           as char format "x(35)"
    field usuario           as char format "x(12)"
    field data-exec         as date
    field hora-exec         as integer
    field classifica        as integer
    field desc-classifica   as char format "x(40)"
    FIELD cod-empresa       LIKE docto_entr.cod_empresa
    field estab-ini         like docto_entr.cod_estab
    field estab-fim         like docto_entr.cod_estab
    field data-ini          like docto_entr.dat_docto
    field data-fim          like docto_entr.dat_docto
    FIELD tg-gera-bem       AS LOGICAL.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions ---                                       */

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def new global shared var v_log_gerac_planilha
    as logical
    format "Sim/N’o"
    initial no
    view-as toggle-box
    label "Gera Planilha"
    no-undo.
define NEW shared var v_cod_arq_modul     as char no-undo.
define NEW shared var v_cod_arq_planilha  as char no-undo.
define NEW shared var v_cod_carac_lim     as char no-undo.
define NEW shared var v_ind_run_mode      as char no-undo.

DEFINE VARIABLE v_hdl_program AS HANDLE      NO-UNDO.

v_cod_arq_planilha = "v:\temp\bens.csv".
v_cod_carac_lim = ";".

DEFINE STREAM s-planilha.
/* para executar o Excel */

DEF VAR c-arq             AS CHAR                NO-UNDO.
DEF VAR c-arq-anexo       AS CHAR                NO-UNDO.   
DEF VAR l-cria            AS LOG                 NO-UNDO.
DEF VAR i-pagina          AS INT INIT 1          NO-UNDO.
DEF VAR i-linha           AS INT                 NO-UNDO.
DEF VAR c-coluna          AS char                NO-UNDO.
DEF VAR i-num_id_bem_pat  LIKE bem_pat_item_docto_entr.num_id_bem_pat NO-UNDO.

def var c-modelo-planilha  as char format "x(50)"         no-undo.
def var c-excel            as com-handle                  NO-UNDO.
def var c-planilha         as com-handle.
def var c-relatorio        as com-handle.

def var h-acomp              as handle no-undo.
DEFINE VARIABLE v-num-reg-lidos AS INTEGER    NO-UNDO.
DEFINE VARIABLE d-total AS DECIMAL    NO-UNDO.
DEFINE VARIABLE d-refer AS DATE  INITIAL TODAY     NO-UNDO.
DEFINE VARIABLE d-ini   AS DATE  INITIAL TODAY     NO-UNDO.
DEFINE VARIABLE d-fim   AS DATE  INITIAL TODAY     NO-UNDO.
ASSIGN d-ini = TODAY - 365.

/********************************** Variavies Especificas ***********************************/


def NEW shared temp-table tt_criacao_bem_pat_api_5 no-undo
    field tta_cod_unid_organ_ext           as character format "x(5)" label "Unid Organ Externa" column-label "Unid Organ Externa"
    field tta_cod_cta_pat                  as character format "x(18)" label "Conta Patrimonial" column-label "Conta Patrimonial"
    field tta_num_bem_pat                  as integer format ">>>>>>>>9" initial 0 label "Bem Patrimonial" column-label "Bem"
    field tta_num_seq_bem_pat              as integer format ">>>>9" initial 0 label "Sequ¼ncia Bem" column-label "Sequ¼ncia"
    field tta_des_bem_pat                  as character format "x(40)" label "Descri»’o Bem Pat" column-label "Descri»’o Bem Pat"
    field tta_dat_aquis_bem_pat            as date format "99/99/9999" initial today label "Data Aquisi»’o" column-label "Dat Aquis"
    field tta_cod_plano_ccusto             as character format "x(8)" label "Plano Centros Custo" column-label "Plano Centros Custo"
    field tta_cod_ccusto_ext               as character format "x(8)" label "Centro Custo Externo" column-label "CCusto Externo"
    field tta_cod_estab_ext                as character format "x(8)" label "Estabelecimento Exte" column-label "Estabelecimento Ext"
    field tta_cod_unid_negoc_ext           as character format "x(8)" label "Unid Neg½cio Externa" column-label "Unid Neg½cio Externa"
    field tta_cod_finalid_econ_ext         as character format "x(8)" label "Finalid Econ Externa" column-label "Finalidade Externa"
    field ttv_val_aquis_bem_pat            as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Aquisi»’o Bem" column-label "Aquisi»’o Bem"
    field ttv_log_erro                     as logical format "Sim/N’o" initial yes
    field tta_qtd_bem_pat_represen         as decimal format ">>>>>>>>9" initial 1 label "Quantidade Bens Representados" column-label "Bem Represen"
    field tta_cdn_fornecedor               as Integer format ">>>,>>>,>>9" initial 0 label "Fornecedor" column-label "Fornecedor"
    field tta_cod_docto_entr               as character format "x(8)" label "Docto Entrada" column-label "Docto Entrada"
    field tta_cod_ser_nota                 as character format "x(3)" label "S²rie Nota" column-label "S²rie Nota"
    field tta_num_item_docto_entr          as integer format ">>>,>>9" initial 0 label "Numero Item" column-label "Num Item"
    field tta_num_id_bem_pat               as integer format ">>,>>>,>>9" initial 0 label "Identifica»’o Bem" column-label "Identifica»’o Bem"
    field tta_des_narrat_bem_pat           as character format "x(2000)" label "Narrativa Bem" column-label "Narrativa Bem"
    field tta_log_bem_imptdo               as logical format "Sim/N’o" initial no label "Bem Importado" column-label "Bem Importado"
    field tta_log_cr_pis                   as logical format "Sim/N’o" initial no label "Credita PIS" column-label "Credita PIS"
    field tta_log_cr_cofins                as logical format "Sim/N’o" initial no label "Credita COFINS" column-label "Credita COFINS"
    field ttv_num_parc_pis_cofins          as integer format "99" initial 0 label "Nro Parcelas" column-label "Nro Parcelas"
    field tta_val_cr_pis                   as decimal format ">>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Cred PIS/PASEP" column-label "Vl Cred PIS/PASEP"
    field tta_val_cr_cofins                as decimal format ">>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Cr²dito COFINS" column-label "Credito COFINS"
    field ttv_log_cr_csll                  as logical format "Sim/N’o" initial no label "Credita CSLL" column-label "Credita CSLL"
    field ttv_num_exerc_cr_csll            as integer format "99" label "Exerc. Cr²dito CSLL" column-label "Exerc. Cr²dito CSLL".

def NEW shared temp-table tt_erros_criacao_bem_pat_api_1 no-undo
    field tta_cod_unid_organ_ext           as character format "x(5)" label "Unid Organ Externa" column-label "Unid Organ Externa"
    field tta_cod_cta_pat                  as character format "x(18)" label "Conta Patrimonial" column-label "Conta Patrimonial"
    field tta_num_bem_pat                  as integer format ">>>>>>>>9" initial 0 label "Bem Patrimonial" column-label "Bem"
    field tta_num_seq_bem_pat              as integer format ">>>>9" initial 0 label "Sequ¼ncia Bem" column-label "Sequ¼ncia"
    field tta_des_bem_pat                  as character format "x(40)" label "Descri»’o Bem Pat" column-label "Descri»’o Bem Pat"
    field tta_dat_aquis_bem_pat            as date format "99/99/9999" initial today label "Data Aquisi»’o" column-label "Dat Aquis"
    field tta_cod_finalid_econ_ext         as character format "x(8)" label "Finalid Econ Externa" column-label "Finalidade Externa"
    field ttv_des_mensagem                 as character format "x(50)" label "Mensagem" column-label "Mensagem".


/*** Conter  os registros dos itens das notas fiscais que serÆo vinculados ao bem ****/
def temp-table tt_criacao_bem_pat_item_api no-undo
    field ttv_rec_bem                      as recid format ">>>>>>9"
    field tta_cdn_fornecedor               as Integer format ">>>,>>>,>>9" initial 0 label "Fornecedor" column-label "Fornecedor"
    field tta_cod_docto_entr               as character format "x(8)" label "Docto Entrada" column-label "Docto Entrada"
    field tta_cod_ser_nota                 as character format "x(3)" label "S²rie Nota" column-label "S²rie Nota"
    field tta_num_item_docto_entr          as integer format ">>>,>>9" initial 0 label "Numero Item" column-label "Num Item"
    field tta_qtd_item_docto_entr          as decimal format ">>>>>>>>9" initial 0 label "Qtde Item Docto" column-label "Qtde Item Docto"
    index tt_id                            is primary unique
          ttv_rec_bem                      ascending
          tta_cdn_fornecedor               ascending
          tta_cod_docto_entr               ascending
          tta_cod_ser_nota                 ascending
          tta_num_item_docto_entr          ascending.

 /** Conter  os registros dos valores residuais do bem. ****/
def temp-table tt_criacao_bem_pat_val_resid no-undo
    field ttv_rec_bem                      as recid format ">>>>>>9"
    field tta_cod_tip_calc                 as character format "x(7)" label "Tipo Cÿlculo" column-label "Tipo Cÿlculo"
    field tta_cod_cenar_ctbl               as character format "x(8)" label "Cenÿrio Contÿbil" column-label "Cenÿrio Contÿbil"
    field tta_cod_finalid_econ             as character format "x(10)" label "Finalidade" column-label "Finalidade"
    field tta_val_resid_min                as decimal format ">>>>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Residual M­nimo" column-label "Residual"
    index tt_id                            is primary unique
          ttv_rec_bem                      ascending
          tta_cod_tip_calc                 ascending
          tta_cod_cenar_ctbl               ascending
          tta_cod_finalid_econ             ascending.

DEF BUFFER b-bem_pat    FOR bem_pat.

DEF TEMP-TABLE tt-bem_pat
    FIELD cod_empresa       LIKE bem_pat.cod_empresa
    FIELD cod_cta_pat       LIKE bem_pat.cod_cta_pat
    FIELD num_bem_pat       LIKE bem_pat.num_bem_pat
    FIELD num_seq_bem_pat   LIKE bem_pat.num_seq_bem_pat.

DEF VAR p_cod_return    AS CHAR NO-UNDO.
DEF VAR p_des_mensagem  AS CHAR NO-UNDO.

DEF VAR i-i1    AS INTEGER  NO-UNDO.
DEF VAR i-i2    AS INTEGER  NO-UNDO.
DEF VAR i-i3    AS INTEGER  NO-UNDO.
DEF VAR i-i4    AS INTEGER  NO-UNDO.
DEF VAR i-seq   AS INTEGER  NO-UNDO FORM '99999'.
DEF VAR l-gerou AS LOGICAL  NO-UNDO INIT NO.

DEF VAR c-num-projeto       LIKE ordem-inv.num-projeto  NO-UNDO.
DEF VAR c-num-ordem         LIKE ordem-inv.num-ordem    NO-UNDO.
DEF VAR c-num-secao         LIKE secao-inv.num-secao    NO-UNDO.
DEF VAR c-it-codigo         LIKE ITEM.it-codigo         NO-UNDO.
DEF VAR c-desc-bem          AS CHAR FORMAT 'X(40)'      NO-UNDO.
DEF VAR c-desc-bem-aux          AS CHAR FORMAT 'X(40)'      NO-UNDO.
DEF VAR c-obs               AS CHAR FORMAT 'X(60)'      NO-UNDO.

DEF VAR c-cod-livre         LIKE item_docto_entr.cod_livre_1    NO-UNDO.
    
DEF VAR c-cod_cta_pat       LIKE tt_criacao_bem_pat_api_5.tta_cod_cta_pat       NO-UNDO.
DEF VAR c-num_seq_bem_pat   LIKE tt_criacao_bem_pat_api_5.tta_num_seq_bem_pat   NO-UNDO.
DEF VAR c-cod_plano_ccusto  LIKE tt_criacao_bem_pat_api_5.tta_cod_plano_ccusto  NO-UNDO.
DEF VAR c-num_ord_invest    LIKE item_docto_entr.num_ord_invest                 NO-UNDO.
DEF VAR c-des-docto-entr    LIKE docto_entr.des_docto_entr                      NO-UNDO.


DEF VAR c-cod_empresa       LIKE docto_entr.cod_empresa NO-UNDO.
DEF VAR c-data_docto_ini    LIKE docto_entr.dat_docto   NO-UNDO.
DEF VAR c-data_docto_fim    LIKE docto_entr.dat_docto   NO-UNDO.

DEF VAR c-qtd_item_docto_entr   AS DECIMAL FORM '->>,>>>,>>9.99' NO-UNDO.
DEF VAR c-val_item_docto_entr   AS DECIMAL FORM '->>,>>>,>>9.99' NO-UNDO.

DEF VAR t-qtd_item_docto_entr   AS DECIMAL FORM '->>,>>>,>>9.99' NO-UNDO.
DEF VAR t-val_item_docto_entr   AS DECIMAL FORM '->>,>>>,>>9.99' NO-UNDO.

DEFINE TEMP-TABLE tt-imprime NO-UNDO
    FIELD cod_estab           LIKE docto_entr.cod_estab           
    FIELD cod_docto_entr      LIKE docto_entr.cod_docto_entr      
    FIELD cdn_fornecedor      LIKE docto_entr.cdn_fornecedor      
    FIELD cod_ser_nota        LIKE docto_entr.cod_ser_nota        
    FIELD dat_docto           LIKE docto_entr.dat_docto           
    FIELD ind_orig_docto      LIKE docto_entr.ind_orig_docto   
    FIELD cod_espec_bem       LIKE item_docto_entr.cod_espec_bem          
    FIELD num_item_docto_entr LIKE item_docto_entr.num_item_docto_entr    
    FIELD num_ord_invest      LIKE c-num_ord_invest         
    FIELD seq                 LIKE i-seq                                  
    FIELD qtd_item_docto_entr LIKE item_docto_entr.qtd_item_docto_entr    
    FIELD val_item_docto_entr LIKE item_docto_entr.val_item_docto_entr    
    FIELD cod_ccusto_benef    LIKE ordem-inv.cod-ccusto-benef
    FIELD it_codigo           LIKE c-it-codigo  
    FIELD desc_bem            LIKE c-desc-bem   
    FIELD obs                 LIKE c-obs.

FORM 
     tt-imprime.cod_estab               COLUMN-LABEL 'EST'
     tt-imprime.cod_docto_entr          COLUMN-LABEL 'DOCTO'
     tt-imprime.cod_ser_nota            COLUMN-LABEL 'SER'
     tt-imprime.cdn_fornecedor          COLUMN-LABEL 'FORN'
     tt-imprime.dat_docto               COLUMN-LABEL 'DT DOCTO'
     tt-imprime.num_item_docto_entr     COLUMN-LABEL 'NUM ITEM'
     tt-imprime.num_ord_invest          COLUMN-LABEL 'NUMERO BEM'       FORM '>>>,>>>,>>9'
     tt-imprime.seq                     COLUMN-LABEL 'SEQ BEM'
     tt-imprime.qtd_item_docto_entr     COLUMN-LABEL 'QUANTIDADE'       FORM '->>,>>>,>>9.99'
     tt-imprime.val_item_docto_entr     COLUMN-LABEL 'VALOR'
     tt-imprime.cod_ccusto_benef        COLUMN-LABEL 'CCUSTO'
     tt-imprime.ind_orig_docto          COLUMN-LABEL 'ORIGEM'
     tt-imprime.it_codigo               COLUMN-LABEL 'ITEM'
     tt-imprime.desc_bem FORMAT "x(40)" COLUMN-LABEL 'DESCRI€ÇO'
     tt-imprime.obs      FORMAT "x(60)" COLUMN-LABEL 'OBS DA IMPORTA€AO DOS BENS DO ATIVO IMOBILIZADO'
     WITH DOWN STREAM-IO WIDTH 500 FRAME f-1.

def var c-cab-265-1 as CHAR form "x(265)" no-undo.
def var c-cab-265-2 as CHAR form "x(265)" no-undo.
def var c-rod-265-1 as CHAR form "x(265)" no-undo.

DEFINE VARIABLE c-cod-unid-negoc    AS CHARACTER FORMAT 'x(03)'  NO-UNDO.

/* Begin_Include: i_declara_GetEntryField */
FUNCTION GetEntryField RETURNS CHARACTER (input p_num_posicao     AS INTEGER,
                                          INPUT p_cod_campo       AS CHARACTER,
                                          input p_cod_separador   AS CHARACTER):

/* ************* Parametros da FUN€ÇO *******************************
** Fun‡Æo para tratamento dos Entries dos c¢digos livres
** 
**  p_num_posicao     - N£mero do Entry que ser  atualizado
**  p_cod_campo       - Campo / Vari vel que ser  atualizada
**  p_cod_separador   - Separador que ser  utilizado
*******************************************************************/

    if  p_num_posicao <= 0  then do:
        assign p_num_posicao  = 1.
    end.
    if num-entries(p_cod_campo,p_cod_separador) >= p_num_posicao  then do:
       return entry(p_num_posicao,p_cod_campo,p_cod_separador).
    end.
    return "" /*l_*/ .

END FUNCTION.

/* End_Include: i_declara_GetEntryField */

/* Begin_Include: i_declara_SetEntryField */
FUNCTION SetEntryField RETURNS CHARACTER (input p_num_posicao     AS INTEGER,
                                          input p_cod_campo       AS CHARACTER,
                                          input p_cod_separador   AS CHARACTER,
                                          input p_cod_valor       AS CHARACTER):

/* ************* Parametros da FUN€ÇO *******************************
** Fun‡Æo para tratamento dos Entries dos c¢digos livres
** 
**  p_num_posicao     - N£mero do Entry / Posi‡Æo que ser  atualizado
**  p_cod_campo       - Campo / Vari vel que ser  atualizada
**  p_cod_separador   - Separador que ser  utilizado
**  p_cod_valor       - Valor que ser  atualizado no Entry passado 
*******************************************************************/

    def var v_num_cont        as integer initial 0 no-undo.
    def var v_num_entries_ini as integer initial 0 no-undo.

    /* ** No progress a menor Entry ‚ 1 ***/
    if p_num_posicao <= 0 then 
       assign p_num_posicao = 1.       

    /* ** Caso o Campo contenha um valor inv lido, este valor ser  convertido para Branco
         para possibilitar os c lculo ***/
    if p_cod_campo = ? then do:
       assign p_cod_campo = "" /* l_*/ .
    end.

    assign v_num_entries_ini = num-entries(p_cod_campo,p_cod_separador) + 1 .    
    if p_cod_campo = "" /* l_*/  then do:
       assign v_num_entries_ini = 2.
    end.

    do v_num_cont =  v_num_entries_ini to p_num_posicao :
       assign p_cod_campo = p_cod_campo + p_cod_separador.
    end.

    assign entry(p_num_posicao,p_cod_campo,p_cod_separador) = p_cod_valor.

    RETURN p_cod_campo.

END FUNCTION.



assign c-cab-265-1 = fill("-",265)
       c-cab-265-2 = fill("-",265).

form header
     c-cab-265-1                        AT 01
     "GERA€ÇO DE BENS DO ATIVO FIXO"    AT 95
     c-cab-265-2                        AT 01 
    skip(1)
    with no-box page-top width 300 frame f-cabec.

assign c-rod-265-1 = " " + "UNIGEL - " + string(today,"99/99/9999") + " - " + string(time, "HH:MM:SS").
assign c-rod-265-1 = fill("-", (265 - length(c-rod-265-1))) + c-rod-265-1.

form header
     c-rod-265-1
     with no-box page-bottom width 300 frame f-rodape.



DEFINE STREAM s-tela.

/*******************************************************************************************/



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-pg-imp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rs-destino bt-arquivo bt-excel bt-config-impr ~
c-arquivo rs-execucao RECT-7 RECT-9 
&Scoped-Define DISPLAYED-OBJECTS rs-destino c-arquivo rs-execucao 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-excel 
     IMAGE-UP FILE "image\im-excel":U
     IMAGE-INSENSITIVE FILE "image\im-exel":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-arquivo 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-config-impr 
     IMAGE-UP FILE "image\im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.57 BY .63 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execu‡Æo" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE rs-destino AS INTEGER INITIAL 2 
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

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 56.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE c-arquivo-1 AS CHARACTER FORMAT "X(50)":U INITIAL "V:\TEMP\BEM_AQUISI€ÇO.TXT" 
     VIEW-AS FILL-IN 
     SIZE 16.72 BY .88 TOOLTIP "Entre com o Nome do Arquivo" NO-UNDO.

DEFINE VARIABLE c-cod-estab-fim AS CHARACTER FORMAT "X(3)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 4.72 BY .88 TOOLTIP "Entre com o C¢digo do Estabelecimento Final" NO-UNDO.

DEFINE VARIABLE c-cod-estab-ini AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estab." 
     VIEW-AS FILL-IN 
     SIZE 4.72 BY .88 TOOLTIP "Entre com o C¢digo do Estabelecimento Inicial" NO-UNDO.

DEFINE VARIABLE d-data-fim AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88 TOOLTIP "Entre com a Data Final" NO-UNDO.

DEFINE VARIABLE d-data-ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Data" 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88 TOOLTIP "Entre com a Data Inicial" NO-UNDO.

DEFINE VARIABLE c-cod-empresa AS CHARACTER FORMAT "X(3)":U 
     LABEL "Empre." 
     VIEW-AS FILL-IN 
     SIZE 4.72 BY .88 TOOLTIP "Entre com o C¢digo da Empresa" NO-UNDO.

DEFINE VARIABLE tg-gera-bem AS LOGICAL INITIAL no 
     LABEL "Gera Bem Ativo Fixo?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .83 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-17
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-18
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-19
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-20
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Fechar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 10 BY 1.

DEFINE IMAGE im-pg-imp
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-sel
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 79 BY 1.42
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 0  
     SIZE 78.72 BY .13
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL 
     SIZE 79 BY 11.38
     FGCOLOR 0 .

DEFINE RECTANGLE rt-folder-left
     EDGE-PIXELS 0  
     SIZE .43 BY 11.21
     BGCOLOR 15 .

DEFINE RECTANGLE rt-folder-right
     EDGE-PIXELS 0  
     SIZE .43 BY 11.17
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder-top
     EDGE-PIXELS 0  
     SIZE 78.72 BY .13
     BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     rt-folder AT ROW 2.5 COL 2
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     RECT-6 AT ROW 13.75 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     im-pg-imp AT ROW 1.5 COL 17.86
     im-pg-sel AT ROW 1.5 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 2.38 COL 3.29 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     bt-arquivo AT ROW 3.58 COL 43.29 HELP
          "Escolha do nome do arquivo"
     bt-excel AT ROW 3.58 COL 53.29 HELP
          "Exporta para Excel"
     bt-config-impr AT ROW 3.58 COL 43.29 HELP
          "Configura‡Æo da impressora"
     c-arquivo AT ROW 3.63 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 5.75 COL 3 HELP
          "Modo de Execu‡Æo" NO-LABEL
     text-destino AT ROW 1.63 COL 3.86 NO-LABEL
     text-modo AT ROW 5 COL 1.29 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 73.72 BY 10.

DEFINE FRAME f-pg-sel
     c-cod-empresa      AT ROW 3 COL 10     COLON-ALIGNED 
     c-cod-estab-ini    AT ROW 4 COL 10     COLON-ALIGNED 
     c-cod-estab-fim    AT ROW 4 COL 40.14  COLON-ALIGNED NO-LABEL
     d-data-ini         AT ROW 5 COL 10     COLON-ALIGNED
     d-data-fim         AT ROW 5 COL 40.14  COLON-ALIGNED NO-LABEL
     tg-gera-bem        AT ROW 6 COL 12
     IMAGE-1            AT ROW 4 COL 31
     IMAGE-17           AT ROW 5 COL 31
     IMAGE-18           AT ROW 5 COL 36.14
     IMAGE-2            AT ROW 4 COL 36.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-relat
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-relat ASSIGN
         HIDDEN             = YES
         TITLE              = "Gera Bem Ativo Fixo - esfas0007 - 1.00.00.010"
         HEIGHT             = 15
         WIDTH              = 81
         MAX-HEIGHT         = 22.33
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22.33
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-relat 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-relat.i}
/*{utp/ut-glob.i}*/
def new global shared var h-rsocial as handle no-undo.
def new global shared var l-achou-prog as logical no-undo.
def var rw-log-exec                            as rowid no-undo.
def new Global shared var c-seg-usuario        as char format "x(12)" no-undo.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-relat
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-pg-imp
                                                                        */
/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Destino".

/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Execu‡Æo".

/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FRAME f-relat
                                                                        */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-left IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-right IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-top IN FRAME f-relat
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-relat)
THEN w-relat:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-imp
/* Query rebuild information for FRAME f-pg-imp
     _Query            is NOT OPENED
*/  /* FRAME f-pg-imp */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-sel
/* Query rebuild information for FRAME f-pg-sel
     _Query            is NOT OPENED
*/  /* FRAME f-pg-sel */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON END-ERROR OF w-relat /* Listagem de Familias */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Listagem de Familias */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-relat
ON CHOOSE OF bt-ajuda IN FRAME f-relat /* Ajuda */
DO:
   {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-excel w-relat
ON CHOOSE OF bt-excel IN FRAME f-pg-imp
DO:
    RUN pi-gera-excel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo w-relat
ON CHOOSE OF bt-arquivo IN FRAME f-pg-imp
DO:
    {include/i-rparq.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-relat
ON CHOOSE OF bt-cancelar IN FRAME f-relat /* Fechar */
DO:
   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-config-impr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-config-impr w-relat
ON CHOOSE OF bt-config-impr IN FRAME f-pg-imp
DO:
   {include/i-rpimp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar w-relat
ON CHOOSE OF bt-executar IN FRAME f-relat /* Executar */
DO:
   do  on error undo, return no-apply:
       run pi-executar.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-imp w-relat
ON MOUSE-SELECT-CLICK OF im-pg-imp IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-sel w-relat
ON MOUSE-SELECT-CLICK OF im-pg-sel IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME rs-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino w-relat
ON VALUE-CHANGED OF rs-destino IN FRAME f-pg-imp
DO:
do  with frame f-pg-imp:
    case self:screen-value:
        when "1" then do:
            assign c-arquivo:sensitive    = no
                   bt-arquivo:visible     = no
                   bt-config-impr:visible = yes.
        end.
        when "2" then do:
            assign c-arquivo:sensitive     = NO /*yes*/
                   bt-arquivo:visible      = yes
                   bt-config-impr:visible  = no.
        end.
        when "3" then do:
            assign c-arquivo:sensitive     = no
                   bt-arquivo:visible      = no
                   bt-config-impr:visible  = no.
        end.
    end case.
end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-execucao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-execucao w-relat
ON VALUE-CHANGED OF rs-execucao IN FRAME f-pg-imp
DO:
/*   {include/i-rprse.i}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/*{utp/ut9000.i "esfas0007" "9.99.99.999"}*/

/* inicializa‡äes do template de relat¢rio */
/*{include/i-rpini.i} edson*/

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/*{include/i-rplbl.i} edson*/

/*****************************************************************
**
**  I-RPLBL.I - Cria os labels para os DumbFolder do relat½rio
**
*******************************************************************/

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
def var c-aux            as char no-undo.
def var i-aux            as integer no-undo.
ON  CLOSE OF THIS-PROCEDURE DO:
    {include/i-logfin.i}  
    RUN disable_ui.
END.
&if "{&PGIMP}" <> "" &then
    ON "LEAVE" OF c-arquivo IN FRAME f-pg-imp DO:
        IF rs-execucao = 1 THEN
            ASSIGN c-arq-old = c-arquivo:SCREEN-VALUE.
        ELSE
            ASSIGN c-arq-old-batch = c-arquivo:SCREEN-VALUE.
    END.
    
    ON "ENTER":U OF c-arquivo IN FRAME f-pg-imp OR
       "RETURN":U OF c-arquivo IN FRAME f-pg-imp OR
       "CTRL-ENTER":U OF c-arquivo IN FRAME f-pg-imp OR
       "CTRL-J":U OF c-arquivo IN FRAME f-pg-imp OR
       "CTRL-Z":U OF c-arquivo IN FRAME f-pg-imp do:
        RETURN NO-APPLY.
    END.
    
    ON "\" OF c-arquivo IN FRAME f-pg-imp DO:
        APPLY "/" TO c-arquivo IN FRAME f-pg-imp.
        RETURN NO-APPLY.
    END.
    
    ON "VALUE-CHANGED":U OF rs-destino IN FRAME f-pg-imp DO:
        CASE rs-destino:SCREEN-VALUE IN FRAME f-pg-imp:
            WHEN "1" THEN DO:
                ASSIGN c-arquivo                                = c-imp-old
                       c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp = c-imp-old
                       c-arquivo:SENSITIVE IN FRAME f-pg-imp    = NO
                       c-arquivo:VISIBLE IN FRAME f-pg-imp      = YES
                       bt-arquivo:VISIBLE IN FRAME f-pg-imp     = NO
                       bt-config-impr:VISIBLE IN FRAME f-pg-imp = YES.
                if c-imp-old = "" then
                   run pi-impres-pad.
            END.
            WHEN "2" THEN
                ASSIGN c-arquivo = IF rs-execucao = 1 
                                   THEN c-arq-old
                                   ELSE c-arq-old-batch
                       c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp = c-arquivo
                       c-arquivo:SENSITIVE IN FRAME f-pg-imp    = NO /*YES*/
                       c-arquivo:VISIBLE IN FRAME f-pg-imp      = YES
                       bt-arquivo:VISIBLE IN FRAME f-pg-imp     = YES
                       bt-config-impr:VISIBLE IN FRAME f-pg-imp = NO.
            WHEN "3" THEN
                ASSIGN c-arquivo                                = ""
                       c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp = c-arquivo
                       c-arquivo:VISIBLE IN FRAME f-pg-imp      = NO
                       bt-arquivo:VISIBLE IN FRAME f-pg-imp     = NO
                       bt-config-impr:VISIBLE IN FRAME f-pg-imp = NO.
        END CASE.
    END.
&endif
/********************************************
** HELP FRAME
********************************************/
ON HELP OF FRAME F-RELAT DO:
    {include/ajuda.i}
END.
&IF "{&PGSEL}" &THEN 
ON HELP OF FRAME F-PG-SEL DO:
    {include/ajuda.i}
END.
&ENDIF
&IF "{&PGCLA}" &THEN 
ON HELP OF FRAME F-PG-CLA DO:
    {include/ajuda.i}
END.
&ENDIF
&IF "{&PGPAR}" &THEN 
ON HELP OF FRAME F-PG-PAR DO:
    {include/ajuda.i}
END.
&ENDIF
&IF "{&PGDIG}" &THEN 
ON HELP OF FRAME F-PG-DIG DO:
    {include/ajuda.i}
END.
&ENDIF
&IF "{&PGIMP}" &THEN 
ON HELP OF FRAME F-PG-IMP DO:
    {include/ajuda.i}
END.
&ENDIF
/********************************************************** 
** Tradu»’o pÿgina 0 - frame f-relat 
**********************************************************/
do  with frame f-relat:
    assign wh-group = frame f-relat:handle
           wh-group = wh-group:first-child.
    do  while valid-handle(wh-group):
        assign wh-child = wh-group:first-child.
        do  while valid-handle(wh-child):
            case wh-child:type:
                when "RADIO-SET":U then 
                    run pi-trad-radio-set (input wh-child).
                when "FILL-IN":U then
                    run pi-trad-fill-in (input wh-child).
                when "TOGGLE-BOX":U then
                    run pi-trad-toggle-box (input wh-child).
                when "COMBO-BOX":U then
                    run pi-trad-combo-box (input wh-child).
                when "BUTTON":U then
                    run pi-trad-button (input wh-child).
                when "EDITOR":U then
                    run pi-trad-editor (input wh-child).
            end case.
            assign wh-child = wh-child:next-sibling.
        end.
        assign wh-group = wh-group:next-sibling.
    end. 
end.     
/********************************************************** 
** Tradu‡Æo p gina sele‡Æo - frame f-pg-sel
**********************************************************/
&if "{&PGSEL}" <> "" &then
    run utp/ut-liter.p (input "Sele‡Æo",
                        input "*",
                        input "R").
    create text wh-label-sel
        assign frame        = frame f-relat:handle
               format       = "x(09)"
               font         = 1
               screen-value = return-value
               width        = 8
               row          = 1.8
               col          = im-pg-sel:col in frame f-relat + 1.86
               visible      = yes
         triggers:
             on mouse-select-click
                apply "mouse-select-click":U to im-pg-sel in frame f-relat.           
         end triggers.                   
     do  with frame f-pg-sel:
         assign wh-group = frame f-pg-sel:handle
                wh-group = wh-group:first-child.
         do  while valid-handle(wh-group):
             assign wh-child = wh-group:first-child.
             do  while valid-handle(wh-child):
                 case wh-child:type:
                    when "RADIO-SET":U then 
                        run pi-trad-radio-set (input wh-child).
                    when "FILL-IN":U then
                        run pi-trad-fill-in (input wh-child).
                    when "TOGGLE-BOX":U then
                        run pi-trad-toggle-box (input wh-child).
                    when "COMBO-BOX":U then
                        run pi-trad-combo-box (input wh-child).
                    when "BUTTON":U then
                        run pi-trad-button (input wh-child).
                    when "TEXT":U then
                        run pi-trad-text (input wh-child).
                    when "EDITOR":U then
                        run pi-trad-editor (input wh-child).
                 end case.
                 assign wh-child = wh-child:next-sibling.
             end.
             assign wh-group = wh-group:next-sibling.
         end. 
     
     end.     
         
&endif
/********************************************************** 
** Tradu‡Æo p gina classifica‡Æo - frame f-pg-cla
**********************************************************/
&if "{&PGCLA}" <> "" &then
    run utp/ut-liter.p (input "Classifica‡Æo",
                        input "*",
                        input "R").
    create text wh-label-cla
        assign frame        = frame f-relat:handle
               format       = "x(13)"
               font         = 1
               screen-value = return-value
               width        = 13
               row          = 1.8
               col          = im-pg-cla:col in frame f-relat + 1.7
               visible      = yes
         triggers:
             on mouse-select-click
                apply "mouse-select-click":U to im-pg-cla in frame f-relat.           
         end triggers.       
     do  with frame f-pg-cla:
         assign wh-group = frame f-pg-cla:handle
                wh-group = wh-group:first-child.
         do  while valid-handle(wh-group):
             assign wh-child = wh-group:first-child.
             do  while valid-handle(wh-child):
                 case wh-child:type:
                    when "RADIO-SET":U then 
                        run pi-trad-radio-set (input wh-child).
                    when "FILL-IN":U then
                        run pi-trad-fill-in (input wh-child).
                    when "TOGGLE-BOX":U then
                        run pi-trad-toggle-box (input wh-child).
                    when "COMBO-BOX":U then
                        run pi-trad-combo-box (input wh-child).
                    when "BUTTON":U then
                        run pi-trad-button (input wh-child).
                    when "TEXT":U then
                        run pi-trad-text (input wh-child).
                    when "EDITOR":U then
                        run pi-trad-editor (input wh-child).
                 end case.
                 assign wh-child = wh-child:next-sibling.
             end.
             assign wh-group = wh-group:next-sibling.
         end. 
     
     end.     
&endif
/********************************************************** 
** Tradu‡Æo p gina parƒmetros - frame f-pg-par
**********************************************************/
&if "{&PGPAR}" <> "" &then
    run utp/ut-liter.p (input "Parƒmetros",
                        input "*",
                        input "R").
    create text wh-label-par
        assign frame        = frame f-relat:handle
               format       = "x(10)"
               font         = 1
               screen-value = return-value
               width        = 11
               row          = 1.8
               col          = im-pg-par:col in frame f-relat + 1.7
               visible      = yes
         triggers:
             on mouse-select-click
                apply "mouse-select-click":U to im-pg-par in frame f-relat.           
         end triggers.
     do  with frame f-pg-par:
         assign wh-group = frame f-pg-par:handle
                wh-group = wh-group:first-child.
         do  while valid-handle(wh-group):
             assign wh-child = wh-group:first-child.
             do  while valid-handle(wh-child):
                 case wh-child:type:
                    when "RADIO-SET":U then 
                        run pi-trad-radio-set (input wh-child).
                    when "FILL-IN":U then
                        run pi-trad-fill-in (input wh-child).
                    when "TOGGLE-BOX":U then
                        run pi-trad-toggle-box (input wh-child).
                    when "COMBO-BOX":U then
                        run pi-trad-combo-box (input wh-child).
                    when "BUTTON":U then
                        run pi-trad-button (input wh-child).
                    when "TEXT":U then
                        run pi-trad-text (input wh-child).
                    when "EDITOR":U then
                        run pi-trad-editor (input wh-child).
                 end case.
                 assign wh-child = wh-child:next-sibling.
             end.
             assign wh-group = wh-group:next-sibling.
         end. 
     
     end.     
&endif
/********************************************************** 
** Tradu‡Æo p gina digita‡Æo - frame f-pg-dig
**********************************************************/
&if "{&PGDIG}" <> "" &then
    run utp/ut-liter.p (input "Digita‡Æo",
                        input "*",
                        input "R").
    create text wh-label-dig
        assign frame        = frame f-relat:handle
               format       = "x(09)"
               font         = 1
               screen-value = return-value
               width        = 10
               row          = 1.8
               col          = im-pg-dig:col in frame f-relat + 1.7
               visible      = yes
         triggers:
             on mouse-select-click
                apply "mouse-select-click" to im-pg-dig in frame f-relat.           
         end triggers.
     
     do with frame f-pg-dig:
         assign frame f-pg-dig:FONT = 2
                wh-group            = frame f-pg-dig:handle
                wh-group            = wh-group:first-child.
         
         do while valid-handle(wh-group):
             assign wh-child = wh-group:first-child.
             do while valid-handle(wh-child):
                 case wh-child:TYPE:
                    when "RADIO-SET":U then do:
                        assign wh-child:FONT = 1.
                        run pi-trad-radio-set (input wh-child).
                    end.
                    when "FILL-IN":U then do:
                        assign wh-child:FONT = 1.
                        run pi-trad-fill-in (input wh-child).
                    end.
                    when "TOGGLE-BOX":U then do:
                        assign wh-child:FONT = 1.
                        run pi-trad-toggle-box (input wh-child).
                    end.
                    when "COMBO-BOX":U then do:
                        assign wh-child:FONT = 1.
                        run pi-trad-combo-box (input wh-child).
                    end.
                    when "BUTTON":U then do:
                        assign wh-child:FONT = 1.
                        run pi-trad-button (input wh-child).
                    end.
                    when "TEXT":U then do:
                        assign wh-child:FONT = 1.
                        run pi-trad-text (input wh-child).
                    end.
                    when "BROWSE":U then
                        run pi-trad-browse (input wh-child).
                    when "EDITOR":U then
                        run pi-trad-editor (input wh-child).
                 end case.
                 
                 assign wh-child = wh-child:next-sibling.
             end.
             
             assign wh-group = wh-group:next-sibling.
         end. 
     
     end.     
&endif
/********************************************************** 
** Tradu‡Æo p gina impressÆo - frame f-pg-imp
**********************************************************/
&if "{&PGIMP}" <> "" &then
    run utp/ut-liter.p (input "ImpressÆo",
                        input "*",
                        input "R").
    create text wh-label-imp
        assign frame        = frame f-relat:handle
               format       = "x(09)"
               font         = 1
               screen-value = return-value
               width        = 10
               row          = 1.8
               col          = im-pg-imp:col in frame f-relat + 1.7
               visible      = yes
         triggers:
             on mouse-select-click
                apply "mouse-select-click":U to im-pg-imp in frame f-relat.           
         end triggers.                   
     do  with frame f-pg-imp:
         assign wh-group = frame f-pg-imp:handle
                wh-group = wh-group:first-child.
         do  while valid-handle(wh-group):
             assign wh-child = wh-group:first-child.
             do  while valid-handle(wh-child):
                 case wh-child:type:
                    when "RADIO-SET":U then 
                        run pi-trad-radio-set (input wh-child).
                    when "FILL-IN":U then
                        run pi-trad-fill-in (input wh-child).
                    when "TOGGLE-BOX":U then
                        run pi-trad-toggle-box (input wh-child).
                    when "COMBO-BOX":U then
                        run pi-trad-combo-box (input wh-child).
                    when "BUTTON":U then
                        run pi-trad-button (input wh-child).
                    when "TEXT":U then
                        run pi-trad-text (input wh-child).
                    when "EDITOR":U then
                        run pi-trad-editor (input wh-child).
                 end case.
                 assign wh-child = wh-child:next-sibling.
             end.
             assign wh-group = wh-group:next-sibling.
         end. 
     
     end.     
         
&endif
/********************************************************** 
** Troca de pÿgina por CTRL-TAB e SHIFT-CTRL-TAB
**********************************************************/

&IF "{&PGSEL}" <> "" &THEN 
    assign c-list-folders = c-list-folders + "im-pg-sel,":U.
&ENDIF
&IF "{&PGCLA}" <> "" &THEN 
    assign c-list-folders = c-list-folders + "im-pg-cla,":U.
&ENDIF
&IF "{&PGPAR}" <> "" &THEN 
    assign c-list-folders = c-list-folders + "im-pg-par,":U.
&ENDIF
&IF "{&PGDIG}" <> "" &THEN 
    assign c-list-folders = c-list-folders + "im-pg-dig,":U.
&ENDIF
&IF "{&PGIMP}" <> "" &THEN
    assign c-list-folders = c-list-folders + "im-pg-imp":U.
&ENDIF
if  substring(c-list-folders,length(c-list-folders)) = "," then 
    assign c-list-folders = substring(c-list-folders,1,length(c-list-folders) - 1 ).
on  CTRL-TAB,SHIFT-CTRL-TAB anywhere do:
    define variable h_handle  as handle no-undo.       
    define variable c_imagem  as character no-undo.
    define variable l_direita as logical no-undo.            
    l_direita = last-event:label = 'CTRL-TAB':U.
        
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
            if  h_handle:type = 'image':U and
                h_handle:name =  c_imagem then do:
                if  h_handle:sensitive = no then 
                    next block1.
                apply 'mouse-select-click':U to h_handle.
                leave block1.
            end.
            h_handle = h_handle:next-sibling.
        end.
    end.
end.
/********************************************************** 
** Procedure de troca de pÿgina por CTRL-TAB 
**********************************************************/
procedure pi-first-child:
        
    define input parameter wh-entry-folder as widget-handle.
    
    assign wh-entry-folder = wh-entry-folder:first-child
           wh-entry-folder = wh-entry-folder:first-child.
    do  while(valid-handle(wh-entry-folder)):
        if  wh-entry-folder:sensitive = yes 
        and wh-entry-folder:type <> 'rectangle':U 
        and wh-entry-folder:type <> 'image':U
        and wh-entry-folder:type <> 'browse':U then do:
            apply 'entry':U to wh-entry-folder.
            leave.
        end.
        else
            assign wh-entry-folder = wh-entry-folder:next-sibling.    
    end.
    
end.
/********************************************************** 
** Procedures de Traducao 
**********************************************************/
procedure pi-trad-radio-set:
   
    def input param wh-objeto    as widget-handle no-undo.
  
    assign c-aux = wh-objeto:radio-buttons.
    do  i-aux = 1 to num-entries(wh-objeto:radio-buttons):
        if  (i-aux mod 2) <> 0 then do:
            run utp/ut-liter.p (input replace(entry(i-aux, wh-objeto:radio-buttons), chr(32), "_"),
                                input "",
                                input "R"). 
            assign entry(i-aux, c-aux) = return-value.
        end.
    end.                                              
    assign wh-objeto:radio-buttons = c-aux.
    
    if  wh-objeto:help <> "" 
    and wh-objeto:help <> ? then do:
        run utp/ut-liter.p (input replace(wh-objeto:help, chr(32), "_"),
                            input "",
                            input "R"). 
        assign wh-objeto:help = return-value.
    end.  
end.
procedure pi-trad-fill-in:
   
    def input param wh-objeto    as widget-handle no-undo.
    
        if  wh-objeto:label <> ?
        and wh-objeto:label <> "" then do:
            run utp/ut-liter.p (input replace(wh-objeto:label, chr(32), "_"),
                                input "",
                                input "L"). 
            assign wh-objeto:label = return-value.
        end. 
        if  wh-objeto:help <> "" 
        and wh-objeto:help <> ? then do:
            run utp/ut-liter.p (input replace(wh-objeto:help, chr(32), "_"),
                                input "",
                                input "R"). 
            assign wh-objeto:help = return-value.
        end.         
    
end.
procedure pi-trad-editor:
    def input param wh-objeto    as widget-handle no-undo.
    
         /* editor n’o tem label, ent’o traduz apenas o help */
        if  wh-objeto:help <> "" 
        and wh-objeto:help <> ? then do:
            run utp/ut-liter.p (input replace(wh-objeto:help, chr(32), "_"),
                                input "",
                                input "R"). 
            assign wh-objeto:help = return-value.
        end.         
end.
procedure pi-trad-toggle-box:
   
    def input param wh-objeto    as widget-handle no-undo.
    
    if  wh-objeto:label <> ?
    and wh-objeto:label <> "" then do:
        run utp/ut-liter.p (input replace(wh-objeto:label, chr(32), "_"),
                            input "",
                            input "R"). 
        assign wh-objeto:label = return-value.
    end. 
    if  wh-objeto:help <> "" 
    and wh-objeto:help <> ? then do:
        run utp/ut-liter.p (input replace(wh-objeto:help, chr(32), "_"),
                            input "",
                            input "R"). 
        assign wh-objeto:help = return-value.
    end.         
    
end.
procedure pi-trad-combo-box:
                        /* nota: n’o traduz conteœdo */
    
    def input param wh-objeto    as widget-handle no-undo.
    
    if  wh-objeto:label <> ?
    and wh-objeto:label <> "" then do:
        run utp/ut-liter.p (input replace(wh-objeto:label, chr(32), "_"),
                            input "",
                            input "L"). 
        assign wh-objeto:label = return-value.
    end. 
    if  wh-objeto:help <> "" 
    and wh-objeto:help <> ? then do:
        run utp/ut-liter.p (input replace(wh-objeto:help, chr(32), "_"),
                            input "",
                            input "R"). 
        assign wh-objeto:help = return-value.
    end.         
    
end.
procedure pi-trad-button:
    
    def input param wh-objeto    as widget-handle no-undo.
    
    if  wh-objeto:label <> ?
    and wh-objeto:label <> "" then do:
        run utp/ut-liter.p (input replace(wh-objeto:label, chr(32), "_"),
                            input "",
                            input "C"). 
        assign wh-objeto:label = return-value.
    end. 
    if  wh-objeto:help <> "" 
    and wh-objeto:help <> ? then do:
        run utp/ut-liter.p (input replace(wh-objeto:help, chr(32), "_"),
                            input "",
                            input "R"). 
        assign wh-objeto:help    = return-value
               wh-objeto:tooltip = trim(return-value).
    end.         
    
end.
procedure pi-trad-text:
    
    def input param wh-objeto    as widget-handle no-undo.
    
    if  wh-objeto:screen-value <> ?
    and wh-objeto:screen-value <> "" then do:
        run utp/ut-liter.p (input replace(wh-objeto:screen-value, chr(32), "_"),
                            input "",
                            input "R"). 
        assign wh-objeto:screen-value = return-value.
               wh-objeto:width = length(return-value).
    end.
    else do:
        if  wh-objeto:private-data <> ?
        and wh-objeto:private-data <> "" then do:
            run utp/ut-liter.p (input replace(wh-objeto:private-data, chr(32), "_"),
                                input "",
                                input "R"). 
            assign wh-objeto:screen-value = " " + return-value.
                   wh-objeto:width = length(return-value) + 1.
        end.
    
    end.
    
end.
procedure pi-trad-browse:
    
    def input param wh-objeto    as widget-handle no-undo.
    def var wh-column            as widget-handle no-undo.
    if  wh-objeto:help <> "" 
    and wh-objeto:help <> ? then do:
        run utp/ut-liter.p (input replace(wh-objeto:help, chr(32), "_"),
                            input "",
                            input "R"). 
        assign wh-objeto:help = return-value.
    end.         
    if  wh-objeto:title <> "" 
    and wh-objeto:title <> ? then do:
        run utp/ut-liter.p (input replace(wh-objeto:title, chr(32), "_"),
                            input "",
                            input "R"). 
        assign wh-objeto:title = return-value.
    end.         
    assign wh-column = wh-objeto:first-column.
    do  while wh-column <> ?:
        run utp/ut-liter.p (input replace(wh-column:label, chr(32), "_"),
                            input "",
                            input "R"). 
        assign wh-column:label = return-value.
        assign wh-column = wh-column:next-column.
    end.
end.
/* i-rplbl */
&if "{&PGIMP}" <> "" &then
procedure pi-impres-pad:
do with frame f-pg-imp:
    find layout_impres_padr no-lock
         where layout_impres_padr.cod_usuario = c-seg-usuario
            and layout_impres_padr.cod_proced = c-programa-mg97  use-index lytmprsp_id  no-error. /*cl_default_procedure_user of layout_impres_padr*/
    if  not avail layout_impres_padr
    then do:
        find layout_impres_padr no-lock
             where layout_impres_padr.cod_usuario = "*"
               and layout_impres_padr.cod_proced = c-programa-mg97  use-index lytmprsp_id  no-error. /*cl_default_procedure of layout_impres_padr*/
        if  avail layout_impres_padr
        then do:
            find imprsor_usuar no-lock
                 where imprsor_usuar.nom_impressora = layout_impres_padr.nom_impressora
                   and imprsor_usuar.cod_usuario = string(c-seg-usuario)
                 use-index imprsrsr_id  no-error. /*cl_layout_current_user of imprsor_usuar*/
        end . /* if */
        if  not avail imprsor_usuar
            or not avail layout_impres_padr /* Por Thiago Garcia ref. FO 901.132 */
        then do:
            find layout_impres_padr no-lock
                 where layout_impres_padr.cod_usuario = c-seg-usuario
                   and layout_impres_padr.cod_proced = "*"
                 use-index lytmprsp_id  no-error. /*cl_default_user of layout_impres_padr*/
        end . /* if */
    end . /* if */
    if  avail layout_impres_padr
    then do:
        assign c-arquivo:screen-value in frame f-pg-imp = layout_impres_padr.nom_impressora
                                    + ":"
                                    + layout_impres_padr.cod_layout_impres.
    end . /* if */
    else do:
         c-arquivo:screen-value in frame f-pg-imp = "".
    end . /* else */
end . /* do dflt */
end.
/*pi-impres-pad */
&endif
/* define procedure externa para execucao do programa de visualizacao do relatorio */

PROCEDURE WinExec EXTERNAL "kernel32.dll":U:
  DEF INPUT  PARAM prg_name                          AS CHARACTER.
  DEF INPUT  PARAM prg_style                         AS SHORT.
END PROCEDURE.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

assign d-data-ini   = TODAY
       d-data-fim   = TODAY.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    RUN enable_UI.

  apply "value-changed":U to rs-destino in frame f-pg-imp.
&IF "{1}" <> "" &THEN
    apply "mouse-select-click":U to {1} in frame f-relat.
&ELSE
    apply "mouse-select-click":U to im-pg-sel in frame f-relat.
&ENDIF

 VIEW {&WINDOW-NAME}. /*View na window do relat½rio*/                                                                          
 APPLY "ENTRY":U TO FRAME f-relat. /*Transferindo focus para a frame principal do relat½rio*/                         
 APPLY "ENTRY":U TO {&WINDOW-NAME}. /*Transferindo focus para janela afim de evitar a vinda do menœ para a frente da janela*/  

    /*{include/i-rpmbl.i}*/
    
    if  im-pg-imp:sensitive in frame f-relat = NO then do:
        run pi-muda-cor-label-folder(input "ImpressÆo").

    end.
    
    IF  NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-relat  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-relat  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-relat  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-relat)
  THEN DELETE WIDGET w-relat.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-relat  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  ENABLE bt-executar bt-cancelar bt-ajuda im-pg-imp im-pg-sel 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY c-cod-empresa c-cod-estab-ini c-cod-estab-fim d-data-ini d-data-fim tg-gera-bem
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE c-cod-empresa c-cod-estab-ini c-cod-estab-fim d-data-ini d-data-fim tg-gera-bem
       IMAGE-1 IMAGE-17 IMAGE-18 IMAGE-2 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE bt-excel /*rs-destino bt-arquivo bt-config-impr c-arquivo rs-execucao*/ RECT-7 
         RECT-9 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  VIEW w-relat.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-relat 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar w-relat 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var r-tt-digita as rowid no-undo.



do on error undo, return error on stop  undo, return error:
    {include/i-rpexa.i}
    
    /* Coloque aqui as valida‡äes das outras p ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p gina com 
       problemas e colocar o focus no campo com problemas */
    
    /* 05.01.2011 */
    EMPTY TEMP-TABLE tt-imprime.

    if (input frame f-pg-sel c-cod-estab-fim <  input frame f-pg-sel c-cod-estab-ini) then do:
       
       message "Estabelecimento Final menor que Inicial !" 
        view-as alert-box buttons OK .
       apply "MOUSE-SELECT-CLICK":U to im-pg-sel in frame f-relat.
       apply "ENTRY":U to c-cod-estab-ini in frame f-pg-sel.
       return error.
        
    end. /* Fim Critica Establecimento */  
     
    if (input frame f-pg-sel d-data-fim <  input frame f-pg-sel d-data-ini) then do:
       
        message "Data Final menor que Inicial !" 
        view-as alert-box buttons OK .
       apply "MOUSE-SELECT-CLICK":U to im-pg-sel in frame f-relat.
       apply "ENTRY":U to d-data-ini in frame f-pg-sel.
       return error.
        
    end. /* Fim Critica Data */  

    /* Aqui sÆo gravados os campos da temp-table que ser  passada como parƒmetro
       para o programa RP.P */
    
    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame f-pg-imp rs-destino
           tt-param.data-exec       = today
           tt-param.hora-exec       = time.
               
    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".
    
    /* Coloque aqui a l¢gica de grava‡Æo dos demais campos que devem ser passados
       como parƒmetros para o programa RP.P, atrav‚s da temp-table tt-param */

    assign tt-param.cod-empresa    = input frame f-pg-sel c-cod-empresa
           tt-param.estab-ini      = input frame f-pg-sel c-cod-estab-ini
           tt-param.estab-fim      = input frame f-pg-sel c-cod-estab-fim
           tt-param.data-ini       = input frame f-pg-sel d-data-ini
           tt-param.data-fim       = input frame f-pg-sel d-data-fim
           tt-param.tg-gera-bem    = input frame f-pg-sel tg-gera-bem.

    
    /* Executar do programa RP.P que ir  criar o relat¢rio */
    /*{include/i-rpexb.i} edson*/
    
    SESSION:SET-WAIT-STATE("general":U).

    /* logica do relatorio */

    run utp/ut-acomp.p persistent set h-acomp.

    run pi-inicializar in h-acomp(input "Movimentos Encontrados:").

    assign c-cod-empresa
           c-cod-estab-ini
           c-cod-estab-fim
           d-data-ini
           d-data-fim
           tg-gera-bem.

     ASSIGN v-num-reg-lidos = 0
            d-ini   = input frame f-pg-sel d-data-ini
            d-fim   = input frame f-pg-sel d-data-fim.

    FOR EACH tt-bem_pat:
        DELETE tt-bem_pat.
    END.

    FOR EACH docto_entr NO-LOCK
        WHERE docto_entr.cod_empresa = c-cod-empresa
        AND   docto_entr.dat_docto  >= d-data-ini
        AND   docto_entr.dat_docto  <= d-data-fim
        BY docto_entr.cod_empresa
        BY docto_entr.cod_estab
        BY docto_entr.dat_docto:
        
        FIND FIRST item_docto_entr OF docto_entr
            WHERE item_docto_entr.log_classif_item_docto_entr = NO NO-LOCK NO-ERROR.
        IF  NOT AVAIL item_docto_entr THEN
            NEXT.
        
        

        ASSIGN i-seq = 1.
               i-i4  = 0.
    
        ASSIGN c-qtd_item_docto_entr = 0
               c-val_item_docto_entr = 0.
    

        FOR EACH item_docto_entr OF docto_entr
            BY item_docto_entr.cod_empresa
            BY item_docto_entr.cod_estab 
            BY item_docto_entr.cod_docto_entr
            BY item_docto_entr.num_item_docto_entr:
        
            IF  item_docto_entr.log_classif_item_docto_entr THEN
                NEXT.
    
            IF  item_docto_entr.dat_classif_item_docto_entr <> 01/01/0001 THEN
                NEXT.
            
            run pi-acompanhar in h-acomp (input "Doc Entr : " + docto_entr.cod_docto_entr + " Data: " + 
                                           STRING(docto_entr.dat_docto,"99/99/9999")).

            ASSIGN c-num-projeto = 0
                   c-num-ordem   = 0
                   c-it-codigo   = ''
                   c-desc-bem    = ''
                   c-cod-livre   = ''
                   i-num_id_bem_pat = 0. 
    
            ASSIGN l-gerou  = YES.
    
            IF  docto_entr.ind_orig_docto = 'Pr¢prio' THEN 
                ASSIGN c-des-docto-entr = docto_entr.des_docto_entr. 
            ELSE
                ASSIGN c-des-docto-entr = item_docto_entr.cod_livre_1.
    
            
            IF  c-des-docto-entr <> '' THEN DO:
    
               /* DO i-i3 = 1 TO 2000:
    
                    IF  SUBSTRING(c-des-docto-entr,i-i3,1) = CHR(10) OR 
                        SUBSTRING(c-des-docto-entr,i-i3,1) = CHR(13) OR 
                        SUBSTRING(c-des-docto-entr,i-i3,1) = CHR(09) THEN
                        ASSIGN c-cod-livre = c-cod-livre + ' '.
                    ELSE
                        ASSIGN c-cod-livre = c-cod-livre + SUBSTRING(c-des-docto-entr,i-i3,1).
    
                END.
                 */

                c-cod-livre = REPLACE(REPLACE(REPLACE(REPLACE(c-des-docto-entr,";"," "),CHR(10)," "),CHR(13)," "),CHR(09)," ").
    
                /* Separa codigo projeto */
                ASSIGN i-i1 = INDEX(c-cod-livre,'Num Projeto:') + 12
                       i-i2 = INDEX(c-cod-livre,'N£mero Ordem:').
    
                IF  i-i1 > 12 THEN
                    ASSIGN c-num-projeto = INTEGER(SUBSTRING(c-cod-livre,i-i1,(i-i2 - i-i1))). 
                ELSE
                    ASSIGN c-num-projeto = 0.
    
                /* Separa codigo da ordem */
                ASSIGN i-i1 = INDEX(c-cod-livre,'N£mero Ordem:') + 13
                       i-i2 = INDEX(c-cod-livre,'Se‡Æo').
    
                IF  i-i1 > 13 THEN
                    ASSIGN c-num-ordem = INTEGER(TRIM(SUBSTRING(c-cod-livre,i-i1,(i-i2 - i-i1)))).  
                ELSE
                    ASSIGN c-num-ordem = 0.
    
                /* Separa codigo da Se‡Æo */
                ASSIGN i-i1 = INDEX(c-cod-livre,'Se‡Æo:') + 6
                       i-i2 = INDEX(c-cod-livre,'Especialidade:').
    
                IF  i-i1 > 6 THEN
                    ASSIGN c-num-secao = INTEGER(TRIM(SUBSTRING(c-cod-livre,i-i1,(i-i2 - i-i1)))).  
                ELSE
                    ASSIGN c-num-secao = 0.
    
                /* Separa codigo do item */
                ASSIGN i-i1 = INDEX(c-cod-livre,'Item:') + 5.
    
                IF  i-i1 > 5 THEN
                    ASSIGN c-it-codigo = SUBSTRING(c-cod-livre,i-i1,11).
                ELSE
                    ASSIGN c-it-codigo = ''.

                IF NUM-ENTRIES(trim(c-it-codigo)," ") > 0 THEN
                     ASSIGN c-it-codigo = ENTRy(1,trim(c-it-codigo)," ").

 
    
                IF  docto_entr.ind_orig_docto = 'Pr¢prio' THEN DO:
                    /* Separa Desci‡Æo do item */
                    ASSIGN i-i1 = INDEX(c-cod-livre,'Descri‡Æo:') + 10.
                   
                    IF  i-i1 > 0 THEN
                        ASSIGN c-desc-bem  = SUBSTRING(c-cod-livre,i-i1,40)
                               c-cod-livre = SUBSTRING(c-cod-livre,i-i1,2000). 
                    ELSE
                        ASSIGN c-desc-bem  = item_docto_entr.des_item_docto_entr
                               c-cod-livre = item_docto_entr.des_item_docto_entr.
    
                    ASSIGN c-num_ord_invest = INTEGER(STRING(c-num-projeto,'9999') + 
                                                      STRING(c-num-ordem,'999')   + 
                                                      STRING(c-num-secao)).
                END.
                ELSE DO:
    
                    ASSIGN I-I1 = I-I1 + 7.
    
                    ASSIGN c-desc-bem  = TRIM(SUBSTRING(c-cod-livre,i-i1 + 2,40))
                           c-cod-livre = TRIM(SUBSTRING(c-cod-livre,i-i1,2000)). 
    
                    ASSIGN i-i1 = INDEX(c-cod-livre,'Descri‡Æo:').
    
                    IF  i-i1 > 0 THEN DO:
                    
                        ASSIGN i-i1 = INDEX(c-cod-livre,'Descri‡Æo:') + 10.
    
                        ASSIGN c-desc-bem  = SUBSTRING(c-cod-livre,i-i1,40)
                               c-cod-livre = SUBSTRING(c-cod-livre,i-i1,2000). 
                    END.
                    ELSE
                        IF  c-desc-bem = '' THEN
                            ASSIGN c-desc-bem  = item_docto_entr.des_item_docto_entr
                                   c-cod-livre = item_docto_entr.des_item_docto_entr.
                   IF i-i1 = 0 THEN DO:


                       c-desc-bem-aux = REPLACE(REPLACE(REPLACE(item_docto_entr.cod_livre_1,CHR(10), " "),CHR(09)," "),CHR(13)," ").
                        
                        ASSIGN i-i1 = INDEX(c-desc-bem-aux,'Item:') .

                         
                       IF i-i1 > 0 THEN
                        REPEAT.

                           IF SUBSTRING(c-desc-bem-aux,i-i1 ,1) = " " THEN
                                LEAVE.

                           i-i1 = i-i1 + 1.
                        END.
                        IF i-i1 > 0 THEN
                       c-desc-bem  = substring(trim(SUBSTRING(c-desc-bem-aux,i-i1 + 1,length(c-desc-bem-aux))),1,80).
                        
                          


                    END.

                    c-desc-bem = substring(c-desc-bem,1,40)  .

                 /* ASSIGN c-num_ord_invest = item_docto_entr.num_ord_invest.
    
                    IF  item_docto_entr.num_ord_invest = 0 THEN */
                    
                        ASSIGN c-num_ord_invest = INTEGER(STRING(c-num-projeto,'9999') + 
                                                          STRING(c-num-ordem,'999')   + 
                                                          STRING(c-num-secao)).
                                                          
                    /* message
                        'item_docto_entr.num_ord_invest ' item_docto_entr.num_ord_invest skip
                        'c-num-projeto    ' c-num-projeto skip
                        'c-num-ordem      ' c-num-ordem skip
                        'c-num-secao      ' c-num-secao skip
                        'c-num_ord_invest ' c-num_ord_invest
                    view-as alert-box. */
                    
                END.
    
            END. /* IF  c-des-docto-entr <> '' THEN DO: */

            IF INDEX(c-des-docto-entr,'Num Projeto:') = 0  THEN
                c-desc-bem = REPLACE(REPLACE(REPLACE(REPLACE(c-des-docto-entr,";"," "),CHR(10)," "),CHR(13)," "),CHR(09)," ").
                c-desc-bem = substring(c-desc-bem,1,40)                           .

            FIND FIRST ordem-inv WHERE 
                ordem-inv.ep-codigo     = item_docto_entr.cod_empresa       AND
                ordem-inv.cod-est-exec  = item_docto_entr.cod_estab         AND
                ordem-inv.num-projeto   = c-num-projeto                     AND
                ordem-inv.num-ordem     = c-num-ordem                       NO-LOCK NO-ERROR.
            
            IF  NOT AVAIL ordem-inv THEN
                ASSIGN l-gerou  = NO
                       c-obs    = 'NÇO FOI ENCONTRADO REGISTRO ORDEM-INV'.
    
            IF  c-num_ord_invest = 0 THEN
                ASSIGN l-gerou  = NO
                       c-obs    = 'NÇO FOI GERADO O CODIGO DO BEM'.
    
            /** Emerson - Migracao ASP (22/11/2010) e corrigido por Ercole em 11/07/2011 **/
            ASSIGN c-cod_cta_pat = (IF item_docto_entr.cod_empresa = "380"
                                    OR item_docto_entr.cod_empresa = "390"
                                    OR item_docto_entr.cod_empresa = "400" THEN "132570.146"
                                    ELSE IF (item_docto_entr.cod_empresa = "420" OR item_docto_entr.cod_empresa = "410") THEN "132570.000" /*solic-318*/ 
                                    ELSE "132570.00").

            ASSIGN i-i4 = i-i4 + 1.
    
            DO i-seq = i-i4 TO 99999:
        
                FIND FIRST b-bem_pat WHERE 
                    b-bem_pat.cod_empresa     = item_docto_entr.cod_empresa   AND
                    b-bem_pat.cod_cta_pat     = c-cod_cta_pat                 AND
                    b-bem_pat.num_bem_pat     = c-num_ord_invest              AND
                    b-bem_pat.num_seq_bem_pat = i-seq                         NO-LOCK NO-ERROR.
                IF  NOT AVAIL b-bem_pat THEN
                    LEAVE.

            END.

            /** Emerson - Migracao ASP (22/11/2010) **/
            FIND FIRST plano_ccusto NO-LOCK
                WHERE plano_ccusto.cod_empresa = item_docto_entr.cod_empresa  and                 
                    plano_ccusto.dat_fim_valid >=  today
                    NO-ERROR.
                    
            IF NOT AVAIL plano_ccusto THEN
                MESSAGE "NÆo encontrado Plano CCusto para Empresa" +
                        item_docto_entr.cod_empresa
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
            ELSE c-cod_plano_ccusto = plano_ccusto.cod_plano_ccusto.

            IF  c-desc-bem = '' AND item_docto_entr.des_item_docto_entr <> '' THEN
                ASSIGN c-desc-bem = item_docto_entr.des_item_docto_entr.
    
            IF  c-desc-bem = '' THEN
                ASSIGN c-desc-bem = 'DESCRI€ÇO DO BEM NÇO ENCONTRADA'.
    

            c-desc-bem = TRIM( c-desc-bem ).

            i-i1 = LENGTH(c-desc-bem).

      

            DO WHILE i-i1 >=  1.

                IF SUBSTRING(c-desc-bem, i-i1,1) <>  " " AND
                   SUBSTRING(c-desc-bem, i-i1,1) <>  "0" THEN leave.

                IF SUBSTRING(c-desc-bem, i-i1,2) = " 0" THEN DO:
              
                      c-desc-bem =   SUBSTRING(c-desc-bem, 1,i-i1).
                      LEAVE.
                END.
                i-i1 = i-i1 - 1.

            END.
            c-desc-bem = TRIM(c-desc-bem).
 
    
            IF  c-desc-bem = 'CONSUMO ALMOXARIFADO 00' THEN
                ASSIGN c-desc-bem = 'CONSUMO ALMOXARIFADO'.

             c-desc-bem = substring(c-desc-bem,1,40).

/*              MESSAGE  "tg-gera-bem" tg-gera-bem SKIP(1) */
/*                  "l-gerou" l-gerou SKIP(1)              */
/*                  "c-obs" c-obs VIEW-AS ALERT-BOX.       */

            IF  tg-gera-bem AND l-gerou THEN DO:
                FIND FIRST mat-rat-med-inv NO-LOCK
                   WHERE mat-rat-med-inv.ep-codigo       = ordem-inv.ep-codigo
                     AND mat-rat-med-inv.num-ordem       = ordem-inv.num-ordem
                     AND mat-rat-med-inv.num-projeto     = ordem-inv.num-projeto
                     AND mat-rat-med-inv.cod-est-exec    = ordem-inv.cod-est-exec
                     AND mat-rat-med-inv.nr-contrato     = 0
                     AND mat-rat-med-inv.num-seq-item    = 0
                     AND mat-rat-med-inv.numero-ordem    = 0
                     AND mat-rat-med-inv.num-seq-event   = 0
                     AND mat-rat-med-inv.num-seq-medicao = 0 NO-ERROR.

                    IF AVAIL mat-rat-med-inv THEN
                        ASSIGN c-cod-unid-negoc = mat-rat-med-inv.cod-unid-negoc.
                    ELSE c-cod-unid-negoc = "00".

                CREATE tt_criacao_bem_pat_api_5.
                ASSIGN tt_criacao_bem_pat_api_5.tta_cod_unid_organ_ext   = docto_entr.cod_empresa
                       tt_criacao_bem_pat_api_5.tta_cod_cta_pat          = c-cod_cta_pat
                       tt_criacao_bem_pat_api_5.tta_num_bem_pat          = c-num_ord_invest
                       tt_criacao_bem_pat_api_5.tta_num_seq_bem_pat      = i-seq                       
                       tt_criacao_bem_pat_api_5.tta_des_bem_pat          = c-desc-bem                  
                       tt_criacao_bem_pat_api_5.tta_dat_aquis_bem_pat    = docto_entr.dat_docto        
                       tt_criacao_bem_pat_api_5.tta_cod_plano_ccusto     = c-cod_plano_ccusto          
                       tt_criacao_bem_pat_api_5.tta_cod_ccusto_ext       = STRING(ordem-inv.cod-ccusto-benef)
                       tt_criacao_bem_pat_api_5.tta_cod_estab_ext        = docto_entr.cod_estab     
                       tt_criacao_bem_pat_api_5.tta_cod_unid_negoc_ext   = c-cod-unid-negoc
                       tt_criacao_bem_pat_api_5.tta_cod_finalid_econ_ext = '0' /* 'Corrente' */
                       tt_criacao_bem_pat_api_5.ttv_val_aquis_bem_pat    = item_docto_entr.val_item_docto_entr
                       tt_criacao_bem_pat_api_5.ttv_log_erro             = NO 
                       tt_criacao_bem_pat_api_5.tta_qtd_bem_pat_represen = item_docto_entr.qtd_item_docto_entr  
                       tt_criacao_bem_pat_api_5.tta_cdn_fornecedor       = docto_entr.cdn_fornecedor       
                       tt_criacao_bem_pat_api_5.tta_cod_docto_entr       = docto_entr.cod_docto_entr       
                       tt_criacao_bem_pat_api_5.tta_cod_ser_nota         = docto_entr.cod_ser_nota         
                       tt_criacao_bem_pat_api_5.tta_num_item_docto_entr  = item_docto_entr.num_item_docto_entr  
                       tt_criacao_bem_pat_api_5.tta_num_id_bem_pat       = 0                                    
                       tt_criacao_bem_pat_api_5.tta_des_narrat_bem_pat   = c-cod-livre                          
                       tt_criacao_bem_pat_api_5.tta_val_cr_pis           = 0                                    
                       tt_criacao_bem_pat_api_5.tta_val_cr_cofins        = 0 
                       tt_criacao_bem_pat_api_5.ttv_log_cr_csll          = NO 
                       tt_criacao_bem_pat_api_5.ttv_num_exerc_cr_csll    = 0
                       tt_criacao_bem_pat_api_5.tta_log_bem_imptdo       = NO. 
        
                CREATE tt_criacao_bem_pat_item_api.
                ASSIGN tt_criacao_bem_pat_item_api.ttv_rec_bem = RECID(tt_criacao_bem_pat_api_5) 
                     tt_criacao_bem_pat_item_api.tta_cdn_fornecedor =  docto_entr.cdn_fornecedor 
                     tt_criacao_bem_pat_item_api.tta_cod_docto_entr =  docto_entr.cod_docto_entr
                     tt_criacao_bem_pat_item_api.tta_cod_ser_nota     =   docto_entr.cod_ser_nota  
                      tt_criacao_bem_pat_item_api.tta_num_item_docto_entr   = item_docto_entr.num_item_docto_entr  
                     tt_criacao_bem_pat_item_api.tta_qtd_item_docto_entr = item_docto_entr.qtd_item_docto_entr.


               EMPTY TEMP-TABLE tt_criacao_bem_pat_val_resid.
               EMPTY TEMP-TABLE tt_erros_criacao_bem_pat_api_1.

                RUN prgfin\fas\fas735zh.py PERSISTENT SET v_hdl_program.

                RUN pi_main_api_criacao_bem_pat_8 IN v_hdl_program 
                                        (INPUT 1, 
                                         INPUT "", 
                                         INPUT "",
                                         INPUT "MOEDA",
                                         INPUT YES,
                                         INPUT TABLE tt_criacao_bem_pat_item_api,
                                         INPUT TABLE tt_criacao_bem_pat_val_resid,
                                         OUTPUT p_cod_return,
                                         OUTPUT p_des_mensagem).


                IF  p_cod_return = "ok" OR p_cod_return = "" THEN DO:
                    ASSIGN item_docto_entr.log_classif_item_docto_entr = YES
                           c-obs                                       = 'BEM GERADO COM SUCESSO'. 
    
                    FIND FIRST tt-bem_pat WHERE
                        tt-bem_pat.cod_empresa      = item_docto_entr.cod_empresa   AND
                        tt-bem_pat.cod_cta_pat      = c-cod_cta_pat                 AND
                        tt-bem_pat.num_bem_pat      = c-num_ord_invest              AND 
                        tt-bem_pat.num_seq_bem_pat  = i-seq                         NO-ERROR.
                    IF  NOT AVAIL tt-bem_pat THEN DO:
                        CREATE tt-bem_pat.
                        ASSIGN tt-bem_pat.cod_empresa      = item_docto_entr.cod_empresa  
                               tt-bem_pat.cod_cta_pat      = c-cod_cta_pat                
                               tt-bem_pat.num_bem_pat      = c-num_ord_invest             
                               tt-bem_pat.num_seq_bem_pat  = i-seq.
                    END.

                    /* Grava‡Æo da tabela de relacionamento de notas do bem - Solu‡Æo temporaria ate solu‡Æo Chamado*/
                    FIND FIRST b-bem_pat WHERE 
                        b-bem_pat.cod_empresa     = item_docto_entr.cod_empresa   AND
                        b-bem_pat.cod_cta_pat     = c-cod_cta_pat                 AND
                        b-bem_pat.num_bem_pat     = c-num_ord_invest              AND
                        b-bem_pat.num_seq_bem_pat = i-seq                         NO-LOCK NO-ERROR.

                    IF  AVAIL b-bem_pat THEN ASSIGN i-num_id_bem_pat =  b-bem_pat.num_id_bem_pat.

                    RUN pi-gera-relacao-nota. 
    
                END.
                ELSE
                   IF  CAN-FIND (FIRST tt_erros_criacao_bem_pat_api_1) THEN DO:                               
                      FOR EACH tt_erros_criacao_bem_pat_api_1:
                          /* DISP tt_erros_criacao_bem_pat_api.ttv_des_mensagem. */
                          ASSIGN c-obs = tt_erros_criacao_bem_pat_api_1.ttv_des_mensagem.
                      END.
                   END. 
        
                FOR EACH tt_criacao_bem_pat_api_5:
                    DELETE tt_criacao_bem_pat_api_5.
                END.
        
                FOR EACH tt_erros_criacao_bem_pat_api_1:
                    DELETE tt_erros_criacao_bem_pat_api_1.
                END.
    
                FOR EACH tt_criacao_bem_pat_item_api:
                      DELETE tt_criacao_bem_pat_item_api.
               END.

               DELETE PROCEDURE v_hdl_program.

            END. /* IF  tg-gera-bem THEN DO: */
             
            IF  NOT tg-gera-bem THEN
                ASSIGN c-obs = 'OPCAO RELATORIO. BEM NAO FOI GERADO'.
    
            /* -> 05.01.2011 */ /*
            IF c-num_ord_invest = 0 OR i-seq = 0 THEN NEXT.

            ASSIGN c-qtd_item_docto_entr = c-qtd_item_docto_entr + item_docto_entr.qtd_item_docto_entr
                   c-val_item_docto_entr = c-val_item_docto_entr + item_docto_entr.val_item_docto_entr.
    
            ASSIGN t-qtd_item_docto_entr = t-qtd_item_docto_entr + item_docto_entr.qtd_item_docto_entr
                   t-val_item_docto_entr = t-val_item_docto_entr + item_docto_entr.val_item_docto_entr.
*/
            CREATE tt-imprime.
            ASSIGN
                tt-imprime.cod_estab           = docto_entr.cod_estab           
                tt-imprime.cod_docto_entr      = docto_entr.cod_docto_entr      
                tt-imprime.cdn_fornecedor      = docto_entr.cdn_fornecedor      
                tt-imprime.cod_ser_nota        = docto_entr.cod_ser_nota        
                tt-imprime.dat_docto           = docto_entr.dat_docto           
                tt-imprime.ind_orig_docto      = docto_entr.ind_orig_docto  
                tt-imprime.cod_espec_bem       = item_docto_entr.cod_espec_bem          
                tt-imprime.num_item_docto_entr = item_docto_entr.num_item_docto_entr    
                tt-imprime.num_ord_invest      = c-num_ord_invest         
                tt-imprime.seq                 = i-seq                                  
                tt-imprime.qtd_item_docto_entr = item_docto_entr.qtd_item_docto_entr    
                tt-imprime.val_item_docto_entr = item_docto_entr.val_item_docto_entr    
                tt-imprime.cod_ccusto_benef    = IF AVAILABLE ordem-inv THEN ordem-inv.cod-ccusto-benef ELSE ""
                tt-imprime.it_codigo           = c-it-codigo  
                tt-imprime.desc_bem            = substring(c-desc-bem,1,40)   
                tt-imprime.obs                 = c-obs
                .
            /* <- 05.01.2011 */

            RELEASE ordem-inv.
    
        END.  /*  FOR EACH item_docto_entr OF docto_entr */
        
    END.
    
    DEF VAR c-dat_aquis_bem_pat   LIKE bem_pat.dat_aquis_bem_pat.
    
    FOR EACH tt-bem_pat NO-LOCK:
    
        FIND FIRST bem_pat WHERE
            bem_pat.cod_empresa      = tt-bem_pat.cod_empresa       AND
            bem_pat.cod_cta_pat      = tt-bem_pat.cod_cta_pat       AND
            bem_pat.num_bem_pat      = tt-bem_pat.num_bem_pat       AND 
            bem_pat.num_seq_bem_pat  = tt-bem_pat.num_seq_bem_pat   EXCLUSIVE-LOCK NO-ERROR.
        IF  AVAIL bem_pat THEN DO:
    
            IF  MONTH(bem_pat.dat_aquis_bem_pat) = 12 THEN
                ASSIGN c-dat_aquis_bem_pat = DATE(01,01,YEAR(bem_pat.dat_aquis_bem_pat) + 1) - 1.
            ELSE
                ASSIGN c-dat_aquis_bem_pat = DATE(MONTH(bem_pat.dat_aquis_bem_pat) + 1,01,YEAR(bem_pat.dat_aquis_bem_pat)) - 1.
            
            ASSIGN bem_pat.dat_calc_pat = c-dat_aquis_bem_pat
                   bem_pat.ind_orig_bem = 'Aquisi‡Æo'.
    
            FOR EACH PARAM_calc_bem_pat OF bem_pat EXCLUSIVE-LOCK:
                ASSIGN PARAM_calc_bem_pat.dat_inic_calc = c-dat_aquis_bem_pat.
            END.
    
        END.
    
    END.
    
    c-arquivo-1 = "V:\TEMP\BEM_AQUISICAO-" + c-cod-empresa + "-" + STRING(TIME) + ".TXT".
    IF v_log_gerac_planilha THEN DO:
      

         v_cod_arq_planilha = REPLACE(v_cod_arq_planilha,"/","\").

         IF NUM-ENTRIES(v_cod_arq_planilha,"\") = 0 THEN
               v_cod_arq_planilha = "v:\temp\bens.csv".
 
          IF trim(ENTRY(NUM-ENTRIES(v_cod_arq_planilha,"\"),v_cod_arq_planilha,"\")) = "" THEN  /*se nÆo colocou nome no arquivo for‡a*/
                v_cod_arq_planilha = "v:\temp\bens.csv".
          ELSE
              IF num-entries(trim(ENTRY(NUM-ENTRIES(v_cod_arq_planilha,"\"),v_cod_arq_planilha,"\")),".") = 0 THEN   /*sem extensÆo for‡a*/
                        v_cod_arq_planilha = "v:\temp\bens.csv".

 

        DOS SILENT DEL VALUE(v_cod_arq_planilha).

        OUTPUT STREAM s-planilha TO VALUE(v_cod_arq_planilha) NO-CONVERT .
 

         IF v_log_gerac_planilha THEN DO:
                PUT STREAM s-planilha UNFORMATTED
                 "EST"  v_cod_carac_lim
                 "DOCTO"  v_cod_carac_lim
                 "SER"  v_cod_carac_lim
                 "FORN"  v_cod_carac_lim
                 "DT DOCTO"  v_cod_carac_lim
                 "NUM ITEM"  v_cod_carac_lim
                 "NUMERO BEM"  v_cod_carac_lim
                 "SEQ BEM"  v_cod_carac_lim
                 "QUANTIDADE"  v_cod_carac_lim
                 "VALOR"  v_cod_carac_lim
                 "CCUSTO"  v_cod_carac_lim
                 "ORIGEM"  v_cod_carac_lim
                 "ITEM"  v_cod_carac_lim
                 "DESCRI€ÇO"  v_cod_carac_lim
                 "OBS DA IMPORTA€AO DOS BENS DO ATIVO IMOBILIZADO"
                    SKIP.


            END.

    END.
    OUTPUT TO VALUE(c-arquivo-1) PAGED NO-CONVERT.

        VIEW FRAME f-cabec.
        VIEW FRAME f-rodape.

        ASSIGN t-qtd_item_docto_entr = 0
               t-val_item_docto_entr = 0.

        FOR EACH tt-imprime:

            IF tg-gera-bem AND tt-imprime.num_ord_invest = 0 THEN NEXT.

            DISPLAY
                 tt-imprime.cod_estab            
                 tt-imprime.cod_docto_entr       
                 tt-imprime.cod_ser_nota         
                 tt-imprime.cdn_fornecedor       
                 tt-imprime.dat_docto            
                 tt-imprime.num_item_docto_entr  
                 tt-imprime.num_ord_invest       
                 tt-imprime.seq                  
                 tt-imprime.qtd_item_docto_entr  
                 tt-imprime.val_item_docto_entr  
                 tt-imprime.cod_ccusto_benef     
                 tt-imprime.ind_orig_docto       
                 tt-imprime.it_codigo          
                 tt-imprime.desc_bem           
                 tt-imprime.obs                
                 WITH FRAME f-1.
            DOWN WITH FRAME f-1.

            IF v_log_gerac_planilha THEN DO:
                PUT STREAM s-planilha UNFORMATTED
                 tt-imprime.cod_estab            v_cod_carac_lim
                 tt-imprime.cod_docto_entr       v_cod_carac_lim
                 tt-imprime.cod_ser_nota         v_cod_carac_lim
                 tt-imprime.cdn_fornecedor       v_cod_carac_lim
                 tt-imprime.dat_docto            v_cod_carac_lim
                 tt-imprime.num_item_docto_entr  v_cod_carac_lim
                 tt-imprime.num_ord_invest       v_cod_carac_lim
                 tt-imprime.seq                  v_cod_carac_lim
                 tt-imprime.qtd_item_docto_entr  v_cod_carac_lim
                 tt-imprime.val_item_docto_entr  v_cod_carac_lim
                 tt-imprime.cod_ccusto_benef     v_cod_carac_lim
                 tt-imprime.ind_orig_docto       v_cod_carac_lim
                 tt-imprime.it_codigo            v_cod_carac_lim
                replace(REPLACE( replace(tt-imprime.desc_bem,v_cod_carac_lim," "),";"," "),CHR(10)," ")    v_cod_carac_lim
                 replace(tt-imprime.obs  ,v_cod_carac_lim , " ")
                    SKIP.


            END.

            ASSIGN t-qtd_item_docto_entr = t-qtd_item_docto_entr + tt-imprime.qtd_item_docto_entr
                   t-val_item_docto_entr = t-val_item_docto_entr + tt-imprime.val_item_docto_entr.

        END.

        DISPLAY 
             '----------------'    @ tt-imprime.qtd_item_docto_entr
             '-------------------' @ tt-imprime.val_item_docto_entr
             WITH FRAME f-1.
        DOWN WITH FRAME f-1.
        
        DISPLAY 
             t-qtd_item_docto_entr @ tt-imprime.qtd_item_docto_entr
             t-val_item_docto_entr @ tt-imprime.val_item_docto_entr
             WITH FRAME f-1.
        DOWN WITH FRAME f-1.
    OUTPUT CLOSE.
 IF v_log_gerac_planilha THEN DO:

        OUTPUT  STREAM s-planilha CLOSE.

    END.
    RUN pi-finalizar IN h-acomp.  

     IF v_log_gerac_planilha THEN 
    
          MESSAGE "Gerados arquivos:"  c-arquivo-1      " e "  v_cod_arq_planilha
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ELSE
          MESSAGE "Gerado arquivo:"  c-arquivo-1      
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

    SESSION:SET-WAIT-STATE("":U).
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina w-relat 
PROCEDURE pi-troca-pagina :
/*------------------------------------------------------------------------------
  Purpose: Gerencia a Troca de P gina (folder)   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{include/i-rptrp.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-relat  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-relat, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-relat 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
  
  run pi-trata-state (p-issuer-hdl, p-state).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

PROCEDURE pi-cria-planilha:

DEF VAR c-arquivo AS CHAR NO-UNDO.
                                
    c-arquivo = c-arq + 'escr100' + STRING(time)+ '.xls'.

    OS-COPY VALUE(c-modelo-planilha) VALUE(c-arquivo).

    assign c-planilha  = c-excel:Workbooks:OPEN(c-arquivo)
           c-relatorio = c-excel:Sheets:item(1)
           c-arq-anexo = (IF c-arq-anexo <> '' THEN ',' ELSE '') + c-arquivo.

END PROCEDURE.


PROCEDURE pi-finaliza-impressao:
DEF VAR i         AS INT  NO-UNDO.
DEF VAR c-arquivo AS CHAR NO-UNDO.

    c-planilha:SAVE().
    /*c-planilha:CLOSE().*/
     
    c-excel:VISIBLE = true.

    /*DO i = 1 TO NUM-ENTRIES(c-arq-anexo):
        c-arquivo = ENTRY(i,c-arq-anexo).
        c-planilha  = c-excel:Workbooks:OPEN(c-arquivo).
    END.*/

    /*c-excel:QUIT().*/
     
     RELEASE OBJECT c-excel.

END PROCEDURE.

PROCEDURE pi-gera-excel.

    RUN prgfin\fas\esfas0007x.p.

IF trim(v_cod_carac_lim) = "" THEN
    v_cod_carac_lim = ";".
   

END PROCEDURE.

PROCEDURE pi-gera-relacao-nota.

    create bem_pat_item_docto_entr.
    assign bem_pat_item_docto_entr.num_id_bem_pat          = i-num_id_bem_pat
           bem_pat_item_docto_entr.num_seq_incorp_bem_pat  = 0
           bem_pat_item_docto_entr.cod_estab               = item_docto_entr.cod_estab
           bem_pat_item_docto_entr.cod_empresa             = item_docto_entr.cod_empresa
           bem_pat_item_docto_entr.cdn_fornecedor          = item_docto_entr.cdn_fornecedor
           bem_pat_item_docto_entr.cod_docto_entr          = item_docto_entr.cod_docto_entr
           bem_pat_item_docto_entr.cod_ser_nota            = item_docto_entr.cod_ser_nota
           bem_pat_item_docto_entr.num_item_docto_entr     = item_docto_entr.num_item_docto_entr
           bem_pat_item_docto_entr.qtd_item_docto_entr     = tt_criacao_bem_pat_item_api.tta_qtd_item_docto_entr
           bem_pat_item_docto_entr.des_item_docto_entr     = item_docto_entr.des_item_docto_entr
           bem_pat_item_docto_entr.cod_indic_econ          = item_docto_entr.cod_indic_econ
           bem_pat_item_docto_entr.cod_espec_bem           = item_docto_entr.cod_espec_bem
           bem_pat_item_docto_entr.cod_marca               = item_docto_entr.cod_marca
           bem_pat_item_docto_entr.cod_modelo              = item_docto_entr.cod_modelo
           bem_pat_item_docto_entr.num_ord_invest          = c-num_ord_invest /*item_docto_entr.num_ord_invest*/
           bem_pat_item_docto_entr.val_item_docto_entr     = item_docto_entr.val_item_docto_entr     / item_docto_entr.qtd_item_docto_entr
           bem_pat_item_docto_entr.val_aquis_bem_pat_fasb  = item_docto_entr.val_aquis_bem_pat_fasb  / item_docto_entr.qtd_item_docto_entr
           bem_pat_item_docto_entr.val_aquis_bem_pat_cmcac = item_docto_entr.val_aquis_bem_pat_cmcac / item_docto_entr.qtd_item_docto_entr
           bem_pat_item_docto_entr.num_id_ri_bem_pat       = &if '{&emsfin_version}' >= '5.07' &then
                                                             item_docto_entr.num_id_ri_bem_pat
                                                              &else
                                                              int(GetEntryField(6, item_docto_entr.cod_livre_1, chr(10)))
                                                             &endif
           bem_pat_item_docto_entr.cod_plano_ccusto        = GetEntryField(1,item_docto_entr.cod_livre_1,chr(10))
           bem_pat_item_docto_entr.cod_ccusto              = GetEntryField(2,item_docto_entr.cod_livre_1,chr(10)).


           /* Atualiza quantidade dispon¡vel do item */
           run pi_atualiza_quant_item_docto_entr(input recid(item_docto_entr),
                                                  input tt_criacao_bem_pat_item_api.tta_qtd_item_docto_entr,
                                                  input "Vincula" /*l_vincula*/ ).


END PROCEDURE.


PROCEDURE pi_atualiza_quant_item_docto_entr:

    /************************ Parameter Definition Begin ************************/

    def Input param p_rec_table
        as recid
        format ">>>>>>9"
        no-undo.
    def Input param p_num_quant_vincul
        as integer
        format ">>>>,>>9"
        no-undo.
    def Input param p_cod_acao
        as character
        format "x(8)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************** Buffer Definition Begin *************************/

    &if "{&emsfin_version}" >= "1.00" &then
    def buffer b_item_docto_entr
        for item_docto_entr.
    &endif


    /*************************** Buffer Definition End **************************/

    /************************* Variable Definition Begin ************************/

    def var v_num_quant_aux                  as integer         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    find first b_item_docto_entr exclusive-lock
         where recid(b_item_docto_entr) = p_rec_table
         no-error.

    if avail b_item_docto_entr then do:

        assign v_num_quant_aux = int(GetEntryField(5,b_item_docto_entr.cod_livre_1,chr(10))).

        if p_cod_acao = "Desvincula" /*l_desvincula*/  then do:
            assign v_num_quant_aux = v_num_quant_aux - p_num_quant_vincul.
        end.
        else do: /* Vincula */
            assign v_num_quant_aux = v_num_quant_aux + p_num_quant_vincul.
        end.

        assign b_item_docto_entr.cod_livre_1 = SetEntryField(5,b_item_docto_entr.cod_livre_1,chr(10),string(v_num_quant_aux)).

        /* Verifica se o item nÆo foi totalmente utilizado e atualiza o campo log_classif_item_docto_entr */
        if b_item_docto_entr.qtd_item_docto_entr <> int(GetEntryField(5,b_item_docto_entr.cod_livre_1,chr(10))) then
            assign b_item_docto_entr.log_classif_item_docto_entr = no.
        else 
            assign b_item_docto_entr.log_classif_item_docto_entr = yes.
    end.


END PROCEDURE. /* pi_atualiza_quant_item_docto_entr */
