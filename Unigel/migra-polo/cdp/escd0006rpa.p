/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
/************************************************************************
**
**  i-prgvrs.i - Programa para cria?’o do log de todos os programas 
**               e objetos do EMS 2.0 para objetos
**  {1} = objeto   provido pelo Roundtable
**  {2} = versao   provido pelo Roundtable
************************************************************************/

def new global shared var c-arquivo-log    as char  format "x(60)" no-undo.
def var c-prg-vrs as char init "[[[2.00.00.000[[[" no-undo.
def var c-prg-obj as char no-undo.
assign c-prg-vrs = "2.00.00.000"
       c-prg-obj = "escd0006RP".
if  c-arquivo-log <> "" and c-arquivo-log <> ? then do:
    find prog_dtsul
        where prog_dtsul.cod_prog_dtsul = "escd0006RP"
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
    put "escd0006RP" at 1 "2.00.00.000" at 69 today at 84 string(time,'HH:MM:SS':U) at 94 skip.
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
 
DEFINE BUFFER b-ordem-compra FOR ordem-compra. 

DEFINE NEW GLOBAL SHARED VARIABLE hWenController AS HANDLE     NO-UNDO.
/*Constante utilizada apenas para verificar se a vari vel acima foi definida */ 

/* fim da alatera‡Æo */

  /*** 010000 ***/
/* defini‡Æo das temp-tables para recebimento de parƒmetros */

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    .


define temp-table tt-digita no-undo
    FIELD marca            AS CHAR FORMAT "x(01)" LABEL "Sel."
    field cod-usuario      as CHAR FORMAT "x(10)" LABEL "Usu rio"
    field e-mail           as character format "x(60)" LABEL "E-mail"
    index id cod-usuario.


def temp-table tt-raw-digita
    field raw-digita as raw.

/* recebimento de parƒmetros */
def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

EMPTY TEMP-TABLE tt-digita.

for each tt-raw-digita:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita TO tt-digita .
end.


/* include padrÆo para vari veis de relat¢rio  */
/*****************************************************************************
**
**  I-RPVAR.I - Variaveis para Impress’o do Cabecalho Padr’o (ex-CD9500.I)
**
*****************************************************************************/
DEFINE BUFFER  usuar_grp_usuar         FOR usuar_grp_usuar.

def var  h-acomp as handle no-undo.
 
def new global shared var c-dir-spool-servid-exec as char no-undo.
def new global shared var i-num-ped-exec-rpw as int no-undo.
DEFINE NEW GLOBAL SHARED VARIABLE c-seg-usuario AS CHARACTER   NO-UNDO.

   

/* bloco principal do programa */
/*find first param-global no-lock no-error.                                                                                    
assign c-programa     = "escd0006RP"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-empresa      = param-global.grupo
       c-sistema      = "MCD"
       c-titulo-relat = "Importa‡Æo de Funcion rios para Requisi‡äes".*/
DEFINE NEW GLOBAL SHARED VARIABLE c-seg-usuario AS CHARACTER   NO-UNDO.

 

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp (input "Importando funcionarios").
       

    OUTPUT TO \\UNGUSB-VAP01\Sistemas\DTS\Log_Prd\escd0006.LOG APPEND .
     put UNFORMATTED "----------------------------------------------" SKIP "usuario:"c-seg-usuario " Dia:" string(today,"99/99/9999") " Hora:" string(time,"HH:MM:SS")
         skip.
         
    for each funcionario where (funcionario.cdn_estab = "422" OR funcionario.cdn_estab = "412)" no-lock. /*solic-318*/
    
      find first am-cd-funcionario-req where 
        am-cd-funcionario-req.cod-funcionario = funcionario.cdn_funcionario no-error.
                
         if not avail am-cd-funcionario-req then do:
     
             if  funcionario.dat_desligto_func <> ?  then next.
             
                create am-cd-funcionario-req.
                assign 
                    am-cd-funcionario-req.cod-funcionario = funcionario.cdn_funcionario
                    am-cd-funcionario-req.nome            = funcionario.nom_pessoa_fisic
                    am-cd-funcionario-req.cod-ccusto      = funcionario.cod_rh_ccusto 
                    am-cd-funcionario-req.dt-admissao     = funcionario.dat_admis_func
                    am-cd-funcionario-req.dt-desligamento = funcionario.dat_desligto_func
                    am-cd-funcionario-req.dt-nascimento   = funcionario.dat_nascimento
                    am-cd-funcionario-req.lotacao         = string(funcionario.cdn_estab )
                    am-cd-funcionario-req.sexo            = funcionario.idi_sexo.
                    
                      disp   
                        am-cd-funcionario-req.cod-funcionario  
                        am-cd-funcionario-req.nome             
                        am-cd-funcionario-req.cod-ccusto       
                        am-cd-funcionario-req.dt-admissao      
                        am-cd-funcionario-req.dt-desligamento  
                        am-cd-funcionario-req.dt-nascimento    
                        am-cd-funcionario-req.lotacao          
                        am-cd-funcionario-req.sexo             
                        with  width 300.
          
          
          end.
          else 
              am-cd-funcionario-req.dt-desligamento = funcionario.dat_desligto_func.
    
      
         
    

    end.
output close.

     
run pi-finalizar in h-acomp.


/* Fim do programa */

