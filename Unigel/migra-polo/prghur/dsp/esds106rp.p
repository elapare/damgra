/********************************************************************************
* Programa: esds106rp.p
* Data....: Out/2012
* Autor...: Edson
* Alteracao :  
* Objetivo: Resultado individual de avaliaá‰es, 22, 23 e 24
*         
* Vers∆o..: 2.00.000                            

*******************************************************************************/
/*---------------- Include de controle de Vers∆o ------------------*/ 
{bf/buffersHCM.i}
{include/i-prgvrs.i esds106RP 2.00.00.000}

def new global shared var i-ep-codigo-usuario  like mguni.empresa.ep-codigo no-undo.
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


 
if connected("dthrpyc") then do:
  def var v_han_fpapi003 as handle         no-undo.
  def VAR v_log_per_sal  as log    init no no-undo.
  run prghur/fpp/fpapi003.p persistent set v_han_fpapi003 (input  c-seg-usuario  /*tt-param.usuario*/,
                                                           input 1 /*tt-param.v_num_tip_aces_usuar*/ ).                                                         
                                                       
  RUN prghur/fpp/fpapi006.p (INPUT  v_cod_usuar_corren , 
                             INPUT  v_num_tip_aces_usuar, 
                             INPUT  v_cod_grp_usuar_lst, 
                             OUTPUT v_log_per_sal).                             
                                
end.
 
/******** Definiá∆o Temp-table para Recebimento de Parametro **********************/
def temp-table tt-raw-digita
    field raw-digita as raw.


define temp-table tt-param
    field destino          as integer
    field arq-destino      as char
    field arq-entrada      as char
    field todos            as integer
    field usuario          as char
    field data-exec        as date
    field hora-exec        as integer
    FIELD v_cdn_empres_usuar AS char
    field i-cdn_aval       as integer
    field dt-aval-ini      as date
    field dt-aval-fim      as date    
    FIELD i-cod-emp-ini     LIKE funcionario.cdn_empresa
    FIELD i-cod-emp-fim     LIKE funcionario.cdn_empresa
    field i-cod-func-ini  as integer
    field i-cod-func-fim  as integer
    field c-lotacao-ini  as char
    field c-lotacao-fim  as char   
    field c-ccusto-ini   as  char
    field c-ccusto-fim   as  char    .
    


 
/* recebimento de parÉmetros */
def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.
 
/****************** Definiáao de Vari†veis de Processamento do Relat¢rio *********************/
def var h-acomp              as handle no-undo.
Def Var dat-trab As Date.
/****************** Definiáao de  Frames e Forms do Relat¢rio 132 Colunas ***************************************/ 

/****************** INCLUDE COM VARIµVEIS GLOBAIS *********************/
 

define temp-table tt-func
field cdn_estab like funcionario.cdn_estab
field cdn_funcionario like funcionario.cdn_funcionario
field cdn_aval as integer
field data-aval as date
field tipo as integer  format "99"
field desc-tipo as char format "x(30)"
field cdn_grp_avpes like  respos_avpes_func.cdn_grp_avpes     
field desc-grupo as char format "x(30)"
field cdn_item_avpes like respos_avpes_func.cdn_item_avpes
field competencia as char format "x(30)"
field resultado-auto as dec
field resultado as dec extent 5
field qt-resultado as dec extent 5
field tt-resultado as dec extent 5
field media as dec
field seq as integer format "99"
field qtde-auto as integer
field qtde as dec extent 5
field g-result as dec
field g-qtde as dec
   index chave IS primary  unique
     cdn_estab
     cdn_funcionario
     tipo
     seq
     cdn_grp_avpes
     cdn_item_avpes.

def buffer b-tt-func for tt-func.

 
    define temp-table tt-func-102
field cdn_estab like funcionario.cdn_estab
field cdn_funcionario like funcionario.cdn_funcionario
field cod_rh_ccusto like rh_ccusto.cod_rh_ccusto
field cdn_aval as integer
field data-aval as date
field competencia as char
field resultado as dec
field dominio as char
field narrativa as char
field seq as integer
   index chave IS primary  unique
     cdn_estab
     cod_rh_ccusto
     cdn_funcionario
     seq
     
  .
def buffer b-tt-func-102 for tt-func-102.


    define temp-table tt-func-103
    field cdn_estab like funcionario.cdn_estab
    field cdn_empresa like funcionario.cdn_empresa

    field cod_rh_ccusto like rh_ccusto.cod_rh_ccusto
    field cdn_funcionario like funcionario.cdn_funcionario
    field cdn_aval as integer
    field data-aval as date
    field competencia as char
    field resultado       as DEC EXTENT 3
    field qt-resultado  as dec   EXTENT 3
    field md-resultado  as dec   EXTENT 3

    field seq as integer
       index chave IS primary  unique
         cdn_estab
         cod_rh_ccusto
         cdn_funcionario
         seq
         competencia     
      .


    define buffer b-emp-tt-func-103 for tt-func-103.
    define buffer b-cc-tt-func-103  for tt-func-103.

 /*Declaraá‰es*/
def  var appExcel As   com-handle.
def  var wb       As   com-handle.
def  var ws       As   com-handle.
def  var rng      As   com-handle.
def  var cht      As   com-handle.    
def  var obj      As   com-handle. 
def  var i-lin    As   integer.
def  var i-lin-grupo    As   integer.
def  var i-lin-grupo-fim    As   integer.

 
 
def var i-seq    As   integer.
def var c-logo as char no-undo. 

def var c-logo-dsp as char no-undo. 
 
def var l-mostra as logical initial  no no-undo.

def var g-qtde  as integer no-undo.
def var g-resultado-auto as dec no-undo.
def var g-media as dec no-undo.
def var g-resultado  as dec extent 5 no-undo.
def var i as integer no-undo.
def var ct as integer no-undo.
def var c-arquivo as char no-undo.
def var i-cdn_estab like funcionario.cdn_estab no-undo.
def var i-cdn_func like funcionario.cdn_funcionario no-undo.
def var i-cdn_aval as integer initial 3 no-undo.
def var c-competencia as char no-undo.
def var d-resultado as dec no-undo.
def var c-dominio as char no-undo.
def var c-narrativa as char no-undo.

def var c-cod_rh_ccusto as char no-undo.


def var dt-ref as date format "99/99/9999" no-undo.
def buffer b-avpes_pontuac_grp for  avpes_pontuac_grp.
def buffer b-avpes_emitid for avpes_emitid.
def buffer b-respos_avpes_func for respos_avpes_func.
DEFINE VARIABLE md-result AS DECIMAL EXTENT 3    NO-UNDO.
def var tt-result as DEC EXTENT 3 no-undo.
def var qt-result as int EXTENT 3 no-undo.
DEFINE VARIABLE qt-result-tt  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE d-result-tt   AS DECIMAL     NO-UNDO.

def var d-result as dec no-undo.
def var tt-result-it as dec no-undo.
def var qt-result-it as int no-undo.
def var d-result-it as dec no-undo.

def var tt-result-g as dec no-undo.
def var qt-result-g as int no-undo.
def var d-result-g as dec no-undo.

def var i-cod-func-ini as integer  initial 62 no-undo.
def var i-cod-func-fim as integer  initial 62 no-undo.
def var i-cod-emp-ini as CHAR initial "360" no-undo.
def var i-cod-emp-fim as CHAr initial "360"  no-undo.
def var c-ccusto-ini as char initial "" no-undo.
def var c-ccusto-fim as char initial "ZZZZZZ" no-undo.
def var c-lotacao-ini as char initial "" no-undo.
def var c-lotacao-fim as char initial "ZZZZZZ" no-undo.
def var dt-aval-ini as date format "99/99/9999" initial 09/30/2012 no-undo.
def var dt-aval-fim as date format "99/99/9999" initial 09/30/2012 no-undo.

c-logo-dsp = search("images\logodsp.jpg").
dt-ref = 09/30/2012.
 
 
assign 
    i-cdn_aval     = tt-param.i-cdn_aval
    dt-aval-ini    = tt-param.dt-aval-ini
    dt-aval-fim    = tt-param.dt-aval-fim
    i-cod-emp-ini  = tt-param.i-cod-emp-ini
    i-cod-emp-fim  = tt-param.i-cod-emp-fim
    i-cod-func-ini = tt-param.i-cod-func-ini
    i-cod-func-fim = tt-param.i-cod-func-fim
    c-lotacao-ini  = tt-param.c-lotacao-ini
    c-lotacao-fim  = tt-param.c-lotacao-fim
    c-ccusto-ini   = tt-param.c-ccusto-ini
    c-ccusto-fim   = tt-param.c-ccusto-fim   .
 
l-mostra = no.

/*
message
 i-cdn_aval     skip
    dt-aval-ini    skip
    dt-aval-fim    skip
    i-cod-emp-ini  skip
    i-cod-emp-fim  skip
    i-cod-func-ini skip
    i-cod-func-fim skip
    c-lotacao-ini  skip
    c-lotacao-fim  skip
    c-ccusto-ini   skip
    c-ccusto-fim
    view-as alert-box.
*/
run utp/ut-acomp.p persistent set h-acomp.
    
run pi-inicializar in h-acomp(input "Selecionando registros").

/* ACUMULA POR AREA E EMPRESA*/

    EMPTY TEMP-TABLE tt-func-103.

  for each  funcionario where 
            funcionario.cdn_empresa     >= string(i-cod-emp-ini) and
            funcionario.cdn_empresa     <= string(i-cod-emp-fim) /* AND
            funcionario.cdn_funcionario = 2049                     */ 
                      no-lock ,
   each avpes_emitid where 
            avpes_emitid.idi_tip_avaldor_avpes <> 1 and
            avpes_emitid.cdn_empresa     = funcionario.cdn_empresa and
            avpes_emitid.cdn_funcionario = funcionario.cdn_funcionario  and
            avpes_emitid.cdn_avpes_padr  =  i-cdn_aval and
            avpes_emitid.dat_refer_respos_avpes  >= dt-aval-ini and
            avpes_emitid.dat_refer_respos_avpes  <= dt-aval-fim and            
            avpes_emitid.dat_cancel_avpes = ?
            no-lock,
            each avpes_reg_mestre of avpes_emitid no-lock,
      
      each avpes_grp where avpes_grp.cdn_avpes_padr = avpes_emitid.cdn_avpes_padr     no-lock,
                each avpes_mestre_grp of avpes_grp no-lock ,
                each avpes_grp_item of avpes_mestre_grp no-lock,
            
                
          each respos_avpes_func  where 
            respos_avpes_func.num_avpes_emitid = avpes_emitid.num_avpes_emitid  and
            respos_avpes_func.cdn_grp_avpes    = avpes_grp.cdn_grp_avpes  and 
            respos_avpes_func.cdn_item_avpes   = avpes_grp_item.cdn_item_avpes /*and
            respos_avpes_func.idi_respos_efetd_func_avpes = 1   /* rsposta efetivada*/*/
                  no-lock,
          each avpes_item where avpes_item.cdn_item_avpes = respos_avpes_func.cdn_item_avpes no-lock
          break
           by funcionario.cdn_estab
           by funcionario.cdn_funcionario
           by avpes_grp.num_seq_impres_avpes
           
          .

      
       
             
             dt-ref = avpes_emitid.dat_refer_respos_avpes.
             
        run pi-acompanhar in h-acomp(input "Acumula Area: " +  STRING(funcionario.cdn_funcionario) + "-" + funcionario.nom_pessoa_fisic ).
                     
             
        for last func_ccusto of funcionario where
            func_ccusto.dat_inic_lotac_func  <=  dt-ref
                    no-lock .
    
        end.

                   
        if not avail func_ccusto then 
            find last func_ccusto of funcionario where 
                  func_ccusto.dat_fim_lotac_func  =  12/31/9999 
                  no-lock no-error.
                    
             
        if avail func_ccusto then 
           find rh_ccusto of func_ccusto no-lock no-error.
       
       
        if not avail func_ccusto or not avail rh_ccusto then
           find rh_ccusto of funcionario no-lock no-error.
           
        if not avail func_ccusto or not avail rh_ccusto then next.
           
       
             
             
                 
              if rh_ccusto.cod_rh_ccusto >= c-ccusto-ini and
                 rh_ccusto.cod_rh_ccusto <= c-ccusto-fim then do:   
                  
                   assign  
                    i-cdn_estab     =  funcionario.cdn_estab
                    i-cdn_func      =  0 
                    c-cod_rh_ccusto = rh_ccusto.cod_rh_ccusto
                    i-seq           =  avpes_grp.num_seq_impres_avpes
                    c-competencia   = avpes_mestre_grp.des_grp_avpes
                    d-resultado     = respos_avpes_func.val_pontuac_item_avpes.
                  
                    Run pi-grava-103.
                 
               end.   
                 
                 assign  
                 i-cdn_estab     =  funcionario.cdn_estab
                 i-cdn_func      =  0 
                 c-cod_rh_ccusto = ""
                 i-seq           =  avpes_grp.num_seq_impres_avpes
                 c-competencia   = avpes_mestre_grp.des_grp_avpes
                 d-resultado     = respos_avpes_func.val_pontuac_item_avpes.
               
                 Run pi-grava-103.
        
  
end.   /* FUNCIONARIO ACUMULA EMPRESA E AREA*/

IF CAN-FIND(FIRST tt-func-103) THEN   /* s¢ roda se encontrou acumulado*/

for each  funcionario where 
            funcionario.cdn_empresa     >= STRING(i-cod-emp-ini) and
            funcionario.cdn_empresa     <= STRING(i-cod-emp-fim) and
            funcionario.cdn_funcionario >= i-cod-func-ini and
            funcionario.cdn_funcionario <= i-cod-func-fim  and
            funcionario.cod_unid_lotac  >= c-lotacao-ini and
            funcionario.cod_unid_lotac  <= c-lotacao-fim           
                      no-lock .

    EMPTY TEMP-TABLE tt-func.
    EMPTY TEMP-TABLE tt-func-102.
   


  FOR each avpes_emitid where 
            
            avpes_emitid.cdn_empresa     = funcionario.cdn_empresa and
            avpes_emitid.cdn_funcionario = funcionario.cdn_funcionario  and
            avpes_emitid.cdn_avpes_padr  =  i-cdn_aval and
            avpes_emitid.dat_refer_respos_avpes  >= dt-aval-ini and
            avpes_emitid.dat_refer_respos_avpes  <= dt-aval-fim and            
            avpes_emitid.dat_cancel_avpes = ?
            no-lock,
            each avpes_reg_mestre of avpes_emitid no-lock,
      
       each avpes_grp where avpes_grp.cdn_avpes_padr = avpes_emitid.cdn_avpes_padr     no-lock,
                each avpes_mestre_grp of avpes_grp no-lock ,
                each avpes_grp_item of avpes_mestre_grp no-lock,
            
                
          each respos_avpes_func  where 
            respos_avpes_func.num_avpes_emitid = avpes_emitid.num_avpes_emitid  and
            respos_avpes_func.cdn_grp_avpes    = avpes_grp.cdn_grp_avpes  and 
            respos_avpes_func.cdn_item_avpes   = avpes_grp_item.cdn_item_avpes /*and
            respos_avpes_func.idi_respos_efetd_func_avpes = 1   /* rsposta efetivada*/*/
                  no-lock,
          each avpes_item where avpes_item.cdn_item_avpes = respos_avpes_func.cdn_item_avpes no-lock
          break

           by avpes_emitid.cdn_funcionario
           by avpes_mestre_grp.num_livre_2
           by avpes_grp.num_seq_impres_avpes
           by avpes_item.cdn_item_avpes
           by avpes_emitid.idi_tip_avaldor_avpes.
             
         
     
     
            
             dt-ref = avpes_emitid.dat_refer_respos_avpes.
           
             
     run pi-acompanhar in h-acomp(input "Funcionario: " +  STRING(funcionario.cdn_funcionario) + "-" + funcionario.nom_pessoa_fisic ).
                     
             
        for last func_ccusto of funcionario where
            func_ccusto.dat_inic_lotac_func  <=  dt-ref
                    no-lock .
    
        end.

                   
        if not avail func_ccusto then 
            find last func_ccusto of funcionario where 
                  func_ccusto.dat_fim_lotac_func  =  12/31/9999 
                  no-lock no-error.
                    
             
        if avail func_ccusto then 
           find rh_ccusto of func_ccusto no-lock no-error.
       
       
        if not avail func_ccusto or not avail rh_ccusto then
           find rh_ccusto of funcionario no-lock no-error.
           
        if not avail func_ccusto then next.
           
        if rh_ccusto.cod_rh_ccusto < c-ccusto-ini or
           rh_ccusto.cod_rh_ccusto > c-ccusto-fim then next.
       
      
      
      
      
      find first tt-func where 
             tt-func.cdn_estab       = funcionario.cdn_estab and
             tt-func.cdn_funcionario = funcionario.cdn_funcionario and
             tt-func.tipo            = avpes_mestre_grp.num_livre_2  and
             tt-func.seq             = avpes_grp.num_seq_impres_avpes and
             tt-func.cdn_grp_avpes   = avpes_mestre_grp.cdn_grp_avpes and
             tt-func.cdn_item_avpes  = respos_avpes_func.cdn_item_avpes no-error.
             
      if not avail tt-func then do:
      
         create tt-func.
         assign 
             tt-func.cdn_estab       = funcionario.cdn_estab  
             tt-func.cdn_funcionario = funcionario.cdn_funcionario  
             tt-func.tipo            = avpes_mestre_grp.num_livre_2 
             tt-func.desc-tipo       = if avpes_mestre_grp.num_livre_2 = 1 then "Competàncias Organizacionais" else "Competàncias Espec°ficas" 
             tt-func.seq             = avpes_grp.num_seq_impres_avpes  
             tt-func.cdn_grp_avpes   = avpes_mestre_grp.cdn_grp_avpes  
             tt-func.cdn_item_avpes  = respos_avpes_func.cdn_item_avpes
             tt-func.cdn_aval        = avpes_emitid.cdn_avpes_padr
             tt-func.data-aval       = dt-ref
             tt-func.desc-grupo      = avpes_mestre_grp.des_grp_avpes
             tt-func.competencia     = avpes_item.des_item_avpes
             .
         
         
      end.    
      
        
             
      if  avpes_emitid.idi_tip_avaldor_avpes = 1 then  
             tt-func.resultado-auto = respos_avpes_func.val_pontuac_item_avpes.
      else       
      do:
      
        IF  avpes_emitid.idi_tip_avaldor_avpes = 5 THEN i = 1.
        ELSE
            IF avpes_emitid.idi_tip_avaldor_avpes = 7 THEN i = 2.
        ELSE
             i= 3.
       
            ASSIGN  tt-func.tt-resultado[i] = tt-func.tt-resultado[i]  + respos_avpes_func.val_pontuac_item_avpes.
                    tt-func.qt-resultado[i] = tt-func.qt-resultado[i]  + (IF respos_avpes_func.val_pontuac_item_avpes > 0 THEN 1 ELSE 0).

              IF tt-func.qt-resultado[i] > 0 THEN
                  tt-func.resultado[i]    = tt-func.tt-resultado[i] / tt-func.qt-resultado[i] .
                 
      
      end.     
           
  END. /* avpes_emitid esds101*/

  IF NOT CAN-FIND(FIRST tt-func) THEN NEXT.  /* se n∆o encontrou vai outro funcionario*/
  
  FOR each avpes_emitid where 
            avpes_emitid.idi_tip_avaldor_avpes <> 1 and
            avpes_emitid.cdn_empresa     = funcionario.cdn_empresa and
            avpes_emitid.cdn_funcionario = funcionario.cdn_funcionario  and
            avpes_emitid.cdn_avpes_padr  =  i-cdn_aval and
            avpes_emitid.dat_refer_respos_avpes  >= dt-aval-ini and
            avpes_emitid.dat_refer_respos_avpes  <= dt-aval-fim and            
            avpes_emitid.dat_cancel_avpes = ?
            no-lock,
            each avpes_reg_mestre of avpes_emitid no-lock,
      
       each avpes_grp where avpes_grp.cdn_avpes_padr = avpes_emitid.cdn_avpes_padr     no-lock,
                each avpes_mestre_grp of avpes_grp no-lock ,
                each avpes_grp_item of avpes_mestre_grp no-lock,
            
                
          each respos_avpes_func  where 
            respos_avpes_func.num_avpes_emitid = avpes_emitid.num_avpes_emitid  and
            respos_avpes_func.cdn_grp_avpes    = avpes_grp.cdn_grp_avpes  and 
            respos_avpes_func.cdn_item_avpes   = avpes_grp_item.cdn_item_avpes /*and
            respos_avpes_func.idi_respos_efetd_func_avpes = 1   /* rsposta efetivada*/*/
                  no-lock,
          each avpes_item where avpes_item.cdn_item_avpes = respos_avpes_func.cdn_item_avpes no-lock
          break
           by avpes_emitid.cdn_estab
           by avpes_emitid.cdn_funcionario
           by avpes_grp.num_seq_impres_avpes
           by avpes_item.cdn_item_avpes
          .

            
             
             dt-ref = avpes_emitid.dat_refer_respos_avpes.
             
     run pi-acompanhar in h-acomp(input "Funcionario: " +  STRING(funcionario.cdn_funcionario) + "-102- " + funcionario.nom_pessoa_fisic ).
                     
             

      if first-of(avpes_grp.num_seq_impres_avpes) or first-of(avpes_emitid.cdn_funcionario) then 
                assign tt-result = 0
                       qt-result = 0
                       md-result = 0.


        for last func_ccusto of funcionario where
            func_ccusto.dat_inic_lotac_func  <=  dt-ref
                    no-lock .
    
        end.

                   
        if not avail func_ccusto then 
            find last func_ccusto of funcionario where 
                  func_ccusto.dat_fim_lotac_func  =  12/31/9999 
                  no-lock no-error.
                    
             
        if avail func_ccusto then 
           find rh_ccusto of func_ccusto no-lock no-error.
       
       
        if not avail func_ccusto or not avail rh_ccusto then
           find rh_ccusto of funcionario no-lock no-error.
           
        if not avail func_ccusto then next.
           
        if rh_ccusto.cod_rh_ccusto < c-ccusto-ini or
           rh_ccusto.cod_rh_ccusto > c-ccusto-fim then next.
       
             
         IF  avpes_emitid.idi_tip_avaldor_avpes = 5 THEN i = 1.
        ELSE
            IF avpes_emitid.idi_tip_avaldor_avpes = 7 THEN i = 2.
        ELSE
             i= 3.     
             
        IF respos_avpes_func.val_pontuac_item_avpes > 0 THEN 
        ASSIGN        
              tt-result[i] = tt-result[i] + respos_avpes_func.val_pontuac_item_avpes 
              qt-result[i] = qt-result[i] + 1.
              
      
        if last-of(avpes_grp.num_seq_impres_avpes) then  do:
            assign  
                qt-result-tt = 0
                d-result-tt  = 0
                d-result     = 0.

             
            DO i = 1 TO 3:
               
                IF qt-result[i] > 0 THEN DO:
                     md-result[i] =  tt-result[i] / qt-result[i] no-error.
                     qt-result-tt = qt-result-tt + 1.
                     d-result-tt  = d-result-tt + md-result[i].
                END.
                    

            END.
             
             if qt-result-tt > 0 then DO:
                    d-result =  d-result-tt / qt-result-tt.
             END.
             
             d-result = truncat(d-result,2).
             
           find first avpes_grp_parecer_restdo  where 
                 avpes_grp_parecer_restdo.cdn_grp_avpes            = avpes_mestre_grp.cdn_grp_avpes   and
                 avpes_grp_parecer_restdo.val_domin_inicial_avpes <= d-result and
                 avpes_grp_parecer_restdo.val_domin_final_avpes    >= d-result and
                 avpes_grp_parecer_restdo.dat_fim_valid           >= avpes_emitid.dat_refer_respos_avpes
                      no-lock no-error.
                      
            if avail  avpes_grp_parecer_restdo then           
              find  first  avpes_tip_restdo where  
                      avpes_tip_restdo.cdn_tip_restdo_avpes = avpes_grp_parecer_restdo.cdn_tip_restdo_avpes no-lock no-error.
                      
            
               assign  
                 i-cdn_estab   =  funcionario.cdn_estab
                 i-cdn_func    =  funcionario.cdn_funcionario
                 i-seq         =  avpes_grp.num_seq_impres_avpes
                 c-competencia = avpes_mestre_grp.des_grp_avpes
                 d-resultado   = d-result
                 c-cod_rh_ccusto =  rh_ccusto.cod_rh_ccusto 
                 c-dominio     = if  avail avpes_tip_restdo then avpes_tip_restdo.des_tip_restdo_avpes else ""
                 c-narrativa   = if avail   avpes_grp_parecer_restdo  then avpes_grp_parecer_restdo.des_impres_parecer_avpes else "". 
                 Run pi-grava.
                
                   
/*                         
                     disp
                      avpes_emitid.num_avpes_emitid
                           avpes_emitid.dat_refer_respos_avpes
                           avpes_emitid.cdn_empresa
                           funcionario.cdn_estab
                           avpes_emitid.cdn_funcionario
                           funcionario.nom_pessoa_fisic
                           avpes_emitid.cdn_avpes_padr
                           avpes_grp.num_seq_impres_avpes
                           avpes_mestre_grp.cdn_grp_avpes 
                           avpes_mestre_grp.des_grp_avpes
                     
                           d-result
                           
                           avpes_tip_restdo.des_tip_restdo_avpes when avail avpes_tip_restdo
                           
                           avpes_grp_parecer_restdo.des_impres_parecer_avpes when avail   avpes_grp_parecer_restdo 
                   
                     
                           
                               
                     with down  width 300 frame b 1 col .
  */                  
       
          
         
         end.
         
   END.   /* avpes_emitid esds102*/

   IF NOT CAN-FIND(FIRST tt-func-102) THEN NEXT.  /* se n∆o encontrou vai outro funcionario*/


 for each b-tt-func where b-tt-func.cdn_item_avpes  <> 999 .
         find first tt-func where 
                 tt-func.cdn_estab       = b-tt-func.cdn_estab and
                 tt-func.cdn_funcionario = b-tt-func.cdn_funcionario and
                 tt-func.tipo            = b-tt-func.tipo  and
                 tt-func.seq             = b-tt-func.seq and
                 tt-func.cdn_grp_avpes   = b-tt-func.cdn_grp_avpes and
                 tt-func.cdn_item_avpes  = 999 no-error.
                 
          if not avail tt-func then do:
          
             create tt-func.
             assign 
                  tt-func.cdn_estab       = b-tt-func.cdn_estab  
                 tt-func.cdn_funcionario = b-tt-func.cdn_funcionario  
                 tt-func.tipo            = b-tt-func.tipo   
                 tt-func.seq             = b-tt-func.seq  
                 tt-func.cdn_grp_avpes   = b-tt-func.cdn_grp_avpes  
                 tt-func.cdn_item_avpes  = 999
                 tt-func.cdn_aval        = b-tt-func.cdn_aval
                 tt-func.data-aval       = b-tt-func.data-aval
                 tt-func.desc-grupo      = b-tt-func.desc-grupo
                 tt-func.competencia     = "Total Grupo"
                 .
                 
          end.      
          
          if b-tt-func.resultado-auto > 0 then
               assign    
                  tt-func.resultado-auto = tt-func.resultado-auto + b-tt-func.resultado-auto
                  tt-func.qtde-auto      = tt-func.qtde-auto  + 1.
               
        /*  assign   
                tt-func.g-qtde   = tt-func.g-qtde   + b-tt-func.g-qtde
                tt-func.g-result = tt-func.g-result + b-tt-func.g-result.
          */        
          assign 
                b-tt-func.media = 0
                ct = 0.
                
          do i = 1 to 3:
               if b-tt-func.resultado[i] = 0 then next.
               
               b-tt-func.media = b-tt-func.media + b-tt-func.resultado[i].
               
               tt-func.resultado[i] =  tt-func.resultado[i] +  b-tt-func.resultado[i].
               tt-func.qtde [i]  = tt-func.qtde [i] + 1.
           

               ct = ct + 1.
          end. 
                
          if ct > 0 then 
                   b-tt-func.media = b-tt-func.media / ct.
       
 end.       
 
 
   for each tt-func where tt-func.cdn_item_avpes  = 999  .
   
                tt-func.resultado-auto = tt-func.resultado-auto /  tt-func.qtde-auto   .
                
                tt-func.media = 0 .
                ct = 0.
                do i = 1 to 3:
                
                   if tt-func.resultado[i] = 0 then next.
                   
                   
                   assign 
                       tt-func.media =  tt-func.media + tt-func.resultado[i]
                       ct = ct + tt-func.qtde [i]
                       tt-func.resultado[i] = tt-func.resultado[i] / tt-func.qtde [i].
                   
                 end.
                
                 
                 if  ct > 0 then  
                       tt-func.media = tt-func.media / ct.           
    
   end.   
   
 
       
      
   
/*  run pi-acompanhar in h-acomp(input "Gerando planilha..." ).*/

    for each tt-func
                                 break 
                            by tt-func.cdn_estab
                            by tt-func.data-aval
                            by tt-func.cdn_funcionario  
                            by tt-func.tipo                                                                                     
                            by tt-func.seq.
    
            if first-of (tt-func.cdn_funcionario) or first-of(tt-func.data-aval) then do:
             
             
                find rh_estab of funcionario no-lock no-error.
                
                Find rh_pessoa_jurid Of rh_estab No-lock No-error.
                 
                
                find unid_lotac of funcionario no-lock no-error.
                
                FIND rh_pessoa_fisic OF funcionario NO-LOCK NO-ERROR.
                
                Find Last histor_sal_func OF funcionario Where
                            histor_sal_func.dat_liber_sal <= tt-func.data-aval NO-LOCK NO-ERROR.
                
                If Avail histor_sal_func Then
                    FIND cargo OF histor_sal_func NO-LOCK NO-ERROR.
             
                     if not avail cargo or not Avail histor_sal_func then 
                      FIND cargo OF funcionario NO-LOCK NO-ERROR.
            
            
                run pi-cria-planilha.
                run pi-cabeca.
             
            end.     
            
            
          
            if first-of (tt-func.tipo) then do:
           
              ws:Range("A" + string(i-lin) + ":I" + string(i-lin)):mergeCells = yes .
              ws:Range("A" + string(i-lin) + ":I" + string(i-lin)):Horizontalalignment = 3.
              ws:Range("A" + string(i-lin) + ":I" + string(i-lin)):Interior:ColorIndex = 24.
              ws:Range("A" + string(i-lin)):value = tt-func.desc-tipo.
              
              ws:range("A" + string( i-lin) + ":I" + STRING(i-lin) ):Borders(01):LineStyle = 1.
              ws:range("A" + string( i-lin) + ":I" + STRING(i-lin) ):Borders(02):LineStyle = 1.
              ws:range("A" + string( i-lin) + ":I" + STRING(i-lin) ):Borders(03):LineStyle = 1.
              ws:range("A" + string( i-lin) + ":I" + STRING(i-lin) ):Borders(04):LineStyle = 1.
            /*  ws:Range("A" + string( i-lin) + ":I" + string(i-lin)):Font:FontStyle = "Negrito".  */
          
              i-lin = i-lin + 1.
               
            END.
            
     
         
            
            if first-of(tt-func.seq) or first-of (tt-func.tipo) then do:
               i-lin-grupo = i-lin.
               i-lin-grupo-fim = i-lin.
         
             
            
            end.
            
            if last-of(tt-func.seq) then do:
               i-lin-grupo-fim = i-lin.
              
     
          
               ws:Range("A" + string( i-lin-grupo) + ":A" + string(i-lin-grupo-fim)):mergeCells = yes .
               ws:Range("A" + string( i-lin-grupo)):value = tt-func.desc-grupo .
             
         
               ws:Range("A" + string( i-lin) ):Horizontalalignment = 3.
               ws:Range("B" + string( i-lin) ):value = "Resultado da Competància".
         
               ws:Range("B"+ string(i-lin) + ":I" + string(i-lin)):Interior:ColorIndex = 34. 
               ws:Range("E"+ string(i-lin) + ":I" + string(i-lin)):Horizontalalignment = 3.      
               
               ws:Range("B" + string(i-lin) + ":D" + string(i-lin)):mergeCells = yes.           
               ws:Range("B"+ string(i-lin) + ":d" + string(i-lin)):Horizontalalignment = 2.      
               ws:Range("E" + string( i-lin) ):Interior:ColorIndex = 23.      
               
               assign 
                  ws:Range("E" + string( i-lin) ):value = int(tt-func.resultado-auto * 100)  / 100
                /*  ws:Range("D" + string( i-lin) ):value = int(tt-func.resultado[1]   * 10)  / 10   
                  ws:Range("E" + string( i-lin) ):value = int(tt-func.resultado[2]   * 10)  / 10   
                  */
                  ws:Range("F" + string( i-lin) ):value = int(tt-func.resultado[1]   * 100)  / 100   
                  ws:Range("G" + string( i-lin) ):value = int(tt-func.resultado[2]   * 100)  / 100   
                  ws:Range("H" + string( i-lin) ):value = int(tt-func.resultado[3]   * 100)  / 100.   
    
    
    /*
               ws:Range("C" + string( i-lin) ):value = truncate(dec(string(tt-func.resultado-auto * 10,"->>,>>>,>>,>>9")),0)  / 10 .
               ws:Range("D" + string( i-lin) ):value = truncate(dec(string(tt-func.resultado[1]   * 10,"->>,>>>,>>,>>9")),0)  / 10.   
               ws:Range("E" + string( i-lin) ):value = truncate(dec(string(tt-func.resultado[2]   * 10,"->>,>>>,>>,>>9")),0)  / 10.   
               ws:Range("F" + string( i-lin) ):value = truncate(dec(string(tt-func.resultado[3]   * 10,"->>,>>>,>>,>>9")),0)  / 10.   
               ws:Range("G" + string( i-lin) ):value = truncate(dec(string(tt-func.resultado[4]   * 10,"->>,>>>,>>,>>9")),0)  / 10.   
               ws:Range("H" + string( i-lin) ):value = truncate(dec(string(tt-func.resultado[5]   * 10,"->>,>>>,>>,>>9")),0)  / 10.   
               
              
               ws:Range("C" + string( i-lin) ):value = truncate(dec(string(tt-func.resultado-auto / tt-func.qtde-auto * 10,"->>,>>>,>>,>>9")),0)  / 10 .
               ws:Range("D" + string( i-lin) ):value = truncate(dec(string(tt-func.resultado[1] / tt-func.qtde [1] * 10,"->>,>>>,>>,>>9")),0)  / 10.   
               ws:Range("E" + string( i-lin) ):value = truncate(dec(string(tt-func.resultado[2] / tt-func.qtde [2] * 10,"->>,>>>,>>,>>9")),0)  / 10.   
               ws:Range("F" + string( i-lin) ):value = truncate(dec(string(tt-func.resultado[3] / tt-func.qtde [3] * 10,"->>,>>>,>>,>>9")),0)  / 10.   
               ws:Range("G" + string( i-lin) ):value = truncate(dec(string(tt-func.resultado[4] / tt-func.qtde [4] * 10,"->>,>>>,>>,>>9")),0)  / 10.   
               ws:Range("H" + string( i-lin) ):value = truncate(dec(string(tt-func.resultado[5] / tt-func.qtde [5] * 10,"->>,>>>,>>,>>9")),0)  / 10.   
                     
               
              
               
               
               assign 
                 tt-result-g = 0 
                 qt-result-g = 0 
                 d-result-g  = 0. 
               do i = 1 to 5.
              
                 assign            
                                tt-result-g = tt-result-g + tt-func.resultado[i]
                                qt-result-g = qt-result-g +  tt-func.qtde [i]
                                 . 
               end.                 
                            
                            d-result-g  = tt-result-g / qt-result-g.
                                
                  tt-func.media = INT(d-result-g * 10) / 10.
         
              */
              
              tt-func.media = INT(tt-func.media * 100) / 100.
               
               
              ws:Range("B" + string( i-lin) + ":"+ "I" + string( i-lin) ):NumberFormat = "#.##0,00".
              ws:Range("I" + string( i-lin) ):value = tt-func.media.      
              ws:Range("I" + string( i-lin) ):Font:Color = -16776961.
               
              ws:Range("I9:I9"):Font:Color = -16776961.
              
              ws:range("A"+ string( i-lin-grupo) + ":I" + STRING(i-lin) ):Borders(01):LineStyle = 1.
              ws:range("A"+ string( i-lin-grupo) + ":I" + STRING(i-lin) ):Borders(02):LineStyle = 1.
              ws:range("A"+ string( i-lin-grupo) + ":I" + STRING(i-lin) ):Borders(03):LineStyle = 1.
              ws:range("A"+ string( i-lin-grupo) + ":I" + STRING(i-lin) ):Borders(04):LineStyle = 1.
              i-lin = i-lin + 1. 
             
        end.     
          
        if last-of (tt-func.tipo) then do:
         
            ws:rows(string(i-lin) + ":" + string(i-lin)):RowHeight = 6.75.
          
           i-lin = i-lin + 1.
        
             
        END.
      
        if tt-func.cdn_item_avpes  <> 999 then do:
            
           /*
             ws:Range("B140:I140"):select.
             appExcel:CutCopyMode = False.
             ws:Range("B140:I140"):Copy.
             ws:Range("B" + string( i-lin) + ":I" + string(i-lin)):Select.
             ws:Paste.
           */
     
             
            /*  ws:Range("b" + string( i-lin) ):Interior:ColorIndex = 6. */
              ws:Range("d" + string( i-lin) + ":" + "h" + string( i-lin) ):Font:FontStyle = "Normal". 
             ws:Range("B" + string( i-lin)):Font:FontStyle = "Normal".
            
             run pi-acompanhar in h-acomp(input "Funcionario: " +  STRING(funcionario.cdn_funcionario) + "- linha" +  string( i-lin)  ).
       
           
             ws:Range("B" + string( i-lin) ):value = tt-func.competencia.
           
             ws:Range("E"+ string(i-lin) + ":I" + string(i-lin)):Horizontalalignment = 3.      
              
             ws:Range("B" + string(i-lin) + ":D" + string(i-lin)):mergeCells = yes.           
             ws:Range("B"+ string(i-lin) + ":d" + string(i-lin)):Horizontalalignment = 2.  
             ws:Range("E" + string( i-lin) ):Interior:ColorIndex = 23.      
             ws:Range("E" + string( i-lin) ):value = tt-func.resultado-auto.      
             /*ws:Range("D" + string( i-lin) ):value = tt-func.resultado[1].      
             ws:Range("E" + string( i-lin) ):value = tt-func.resultado[2].      */
             ws:Range("F" + string( i-lin) ):value = tt-func.resultado[1].      
             ws:Range("G" + string( i-lin) ):value = tt-func.resultado[2].      
             ws:Range("H" + string( i-lin) ):value = tt-func.resultado[3].      
             ws:Range("I" + string( i-lin) ):value = tt-func.media.  
             ws:Range("d" + string( i-lin) + ":"+ "h" + string( i-lin) ):NumberFormat = "#.##0,00".         
             ws:Range("i" + string( i-lin) + ":"+ "I" + string( i-lin) ):NumberFormat = "#.##0,00".         
             
             
             assign 
             g-resultado-auto = g-resultado-auto + tt-func.resultado-auto
       
             g-media          = g-media +  tt-func.media
             g-qtde           = g-qtde + 1.
             
             do i = 1 to 5:
                   g-resultado[i]      = g-resultado [i] + tt-func.resultado[i].
                  
             end.
           
             i-lin = i-lin + 1.
        end.
         
        if last-of (tt-func.cdn_funcionario) then run pi-formata. 
         
      
    end.

    /* esds102*/
    run pi-acompanhar in h-acomp(input "Gerando Planilha 102...").
     
    ws = wb:Sheets:item("esds102 e 103").
    
    ws:activate.
    i-lin = 5.

    for each tt-func-102 break by tt-func-102.cdn_estab
                               by tt-func-102.data-aval
                               by tt-func-102.cdn_funcionario                                                                                       
                               by tt-func-102.seq.
    
      run pi-acompanhar in h-acomp(input "Funcionario: " +  STRING(funcionario.cdn_funcionario) + "-" + funcionario.nom_pessoa_fisic ).
    
        find first b-emp-tt-func-103 where 
            b-emp-tt-func-103.cdn_estab        = tt-func-102.cdn_estab and
            b-emp-tt-func-103.cod_rh_ccusto    = "" and
            b-emp-tt-func-103.cdn_funcionario  = 0     and
            b-emp-tt-func-103.seq              = tt-func-102.seq          and
            b-emp-tt-func-103.competencia      = tt-func-102.competencia  no-error.
    
        find first b-cc-tt-func-103 where 
            b-cc-tt-func-103.cdn_estab        = tt-func-102.cdn_estab and
            b-cc-tt-func-103.cod_rh_ccusto    = tt-func-102.cod_rh_ccusto and
            b-cc-tt-func-103.cdn_funcionario  = 0     and
            b-cc-tt-func-103.seq              = tt-func-102.seq          and
            b-cc-tt-func-103.competencia      = tt-func-102.competencia  no-error.
    
      run pi-dados.   
    
      if last-of (tt-func-102.cdn_funcionario) then 
        do: 
          run pi-formata-102.
          RUN pi-formata-103.
      END.
    
    
    
    end.



END.
RUN pi-finalizar IN h-acomp.

MESSAGE "Verifique arquivos gerados no c:\temp."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.


procedure pi-cria-planilha.
    /*A Aplicaá∆o Ç criada aqui:*/
 
    CREATE "Excel.Application" appExcel.
    ASSIGN appExcel:DisplayAlerts = FALSE.
    
   /*A linha abaixo Ç importante: vocà deve querer que
     sua aplicaá∆o seja vis°vel na maioria das vezes*/
     
    appExcel:Visible = no.
    
   /*Note que, na linha abaixo, foi adicionado um Workbook
     dentro da Aplicaá∆o appExcel:*/
     
     c-arquivo = search("prghur\modelos\mod-esds106.xlsx").
     if c-arquivo <> ? then do:
      
          wb = appExcel:Workbooks:open(c-arquivo). /*modelo em branco com fonte negrito carregada para fica rmais rapido*/
         
        
     end.     
     else
     do:
        wb = appExcel:Workbooks:add.
        
     end. 
 
   /*Da mesma forma, foi selecionada a primeira Planilha ws dentro do
     Workbook criado wb.*/
     
     ws = wb:Sheets:item(1).
    
 
end procedure.

procedure pi-cabeca.

 
    ws:Columns("A:i"):Verticalalignment = 2.
    
 
    
    find first avpes_reg_mestre where 
      avpes_reg_mestre.cdn_avpes_padr = tt-func.cdn_aval no-lock no-error.
      
      
  
        
    assign    

        rng = ws:Range("B2")
        rng:value = if avail avpes_reg_mestre then avpes_reg_mestre.des_avpes_padr else ""
      .  
     
  
   
 
   Assign
        ws:Range("B4") :value = string(funcionario.cdn_estab)       + " - " + rh_estab.nom_pessoa_jurid
        ws:Range("B5") :value = funcionario.nom_pessoa_fisic
        ws:Range("B6") :value = string(funcionario.cod_unid_lotac)  + " - " + unid_lotac.des_unid_lotac
        ws:Range("E4") :value = string(cargo.cdn_cargo_basic)       + "/"   + string(cargo.cdn_niv_cargo ) + "-" + cargo.des_cargo.
       

   for first avpes_reg_mestre where avpes_reg_mestre.cdn_avpes_padr =  i-cdn_aval no-lock,
       first avpes_emitid where 
            avpes_emitid.cdn_avpes_padr  =  avpes_reg_mestre.cdn_avpes_padr and
            avpes_emitid.cdn_empresa     = funcionario.cdn_empresa and
            avpes_emitid.cdn_funcionario = funcionario.cdn_funcionario and
            avpes_emitid.dat_refer_respos_avpes  = tt-func.data-aval  no-lock.
            
        ws:Range("E5"):value = string( avpes_reg_mestre.cdn_avpes_padr) + " - " + avpes_reg_mestre.des_avpes_padr.
        ws:Range("E6"):value = string(avpes_emitid.dat_refer_respos_avpes,"99/99/9999").
        ws:Range("I6"):value =  STRING(funcionario.cdn_funcionario).

    end.


    assign 
      g-resultado-auto = 0
      g-resultado      = 0
      g-media          = 0
      g-qtde           = 0.
          
      
     i-lin = 10.

end procedure.
 

procedure pi-formata.
  
   
 ws:Range("A" + string(i-lin)):value = "Resultado da Avaliaá∆o".
 ws:Range("A" + string(i-lin) + ":d" + string(i-lin)):mergeCells = yes. 
 ws:Range("A" + string(i-lin) + ":I" + string(i-lin)):Interior:ColorIndex = 17.
 ws:range("A"+ string( i-lin) + ":I" + STRING(i-lin) ):Borders(01):LineStyle = 1.
 ws:range("A"+ string( i-lin) + ":I" + STRING(i-lin) ):Borders(02):LineStyle = 1.
 ws:range("A"+ string( i-lin) + ":I" + STRING(i-lin) ):Borders(03):LineStyle = 1.
 ws:range("A"+ string( i-lin) + ":I" + STRING(i-lin) ):Borders(04):LineStyle = 1.
 
 ws:Range("E" + string(i-lin) + ":E" + string(i-lin)):Interior:ColorIndex = 23.

/* ws:Range("A" + string(i-lin) + ":I" + string(i-lin)):Font:FontStyle = "Negrito".*/
 ws:Range("A" + string(i-lin) + ":I" + string(i-lin)):Horizontalalignment = 3.
 ws:Range("A" + string(i-lin) + ":I" + string(i-lin)):Font:Color = -16776961.

 
      ws:Range("E" + string( i-lin) ):value =truncate( g-resultado-auto / g-qtde,2).      
      /*ws:Range("D" + string( i-lin) ):value =truncate( g-resultado[1] / g-qtde,1).      
      ws:Range("E" + string( i-lin) ):value =truncate( g-resultado[2] / g-qtde,1).      */
      ws:Range("F" + string( i-lin) ):value =truncate( g-resultado[1] / g-qtde,2).      
      ws:Range("G" + string( i-lin) ):value =truncate( g-resultado[2] / g-qtde,2).      
      ws:Range("H" + string( i-lin) ):value =truncate( g-resultado[3] / g-qtde,2).      
      ws:Range("I" + string( i-lin) ):value =truncate( g-media / g-qtde,2).  

      ws:range("F" + string( i-lin) + ":" + "I" + string( i-lin) ):NumberFormat = "#.##0,00".  

   i-lin = i-lin + 1.

 
    g-media = truncate( g-media / g-qtde,2).
    
ws:Range("A" + string(i-lin)):value = "Grade:".
i-lin = i-lin + 1.
ws:Range("A" + string(i-lin)):value = "de 1,00 a 1,80".
ws:Range("B" + string(i-lin)):value = "N∆o Atende as Expectativas".
ws:Range("C" + string(i-lin)):value = "de 2,61 a 3,40".
ws:Range("E" + string(i-lin)):value = "Atende as Expectativas".
ws:Range("C" + string(i-lin) + ":D" + string(i-lin)):mergeCells = yes.
ws:Range("E" + string(i-lin) + ":I" + string(i-lin)):mergeCells = yes.
 ws:range("A"+ string( i-lin) + ":I" + STRING(i-lin) ):Borders(01):LineStyle = 1.
 ws:range("A"+ string( i-lin) + ":I" + STRING(i-lin) ):Borders(02):LineStyle = 1.
 ws:range("A"+ string( i-lin) + ":I" + STRING(i-lin) ):Borders(03):LineStyle = 1.
 ws:range("A"+ string( i-lin) + ":I" + STRING(i-lin) ):Borders(04):LineStyle = 1.
 


i-lin = i-lin + 1.
ws:Range("A" + string(i-lin)):value = "de 1,81 a 2,60".
ws:Range("B" + string(i-lin)):value = "Atende Parcialmente as Expectativas".
ws:Range("C" + string(i-lin)):value = "de 3,41 a 4,20".
ws:Range("E" + string(i-lin)):value = "Supera Parcialmente".
ws:Range("C" + string(i-lin) + ":D" + string(i-lin)):mergeCells = yes.
ws:Range("E" + string(i-lin) + ":I" + string(i-lin)):mergeCells = yes.
 ws:range("A"+ string( i-lin) + ":I" + STRING(i-lin) ):Borders(01):LineStyle = 1.
 ws:range("A"+ string( i-lin) + ":I" + STRING(i-lin) ):Borders(02):LineStyle = 1.
 ws:range("A"+ string( i-lin) + ":I" + STRING(i-lin) ):Borders(03):LineStyle = 1.
 ws:range("A"+ string( i-lin) + ":I" + STRING(i-lin) ):Borders(04):LineStyle = 1.
 

i-lin = i-lin + 1.
ws:Range("C" + string(i-lin)):value = "de 4,21 a 5,00".
ws:Range("C" + string(i-lin) + ":D" + string(i-lin)):mergeCells = yes.
ws:Range("E" + string(i-lin)):value = "Supera as Expectativas".
ws:Range("E" + string(i-lin) + ":I" + string(i-lin)):mergeCells = yes.

 ws:range("A"+ string( i-lin) + ":I" + STRING(i-lin) ):Borders(01):LineStyle = 1.
 ws:range("A"+ string( i-lin) + ":I" + STRING(i-lin) ):Borders(02):LineStyle = 1.
 ws:range("A"+ string( i-lin) + ":I" + STRING(i-lin) ):Borders(03):LineStyle = 1.
 ws:range("A"+ string( i-lin) + ":I" + STRING(i-lin) ):Borders(04):LineStyle = 1.




 
    ws:Range("a" + string(i-lin - 2) + ":a" + string(i-lin     )):Borders(01):Weight = -4138.
    ws:Range("i" + string(i-lin - 2) + ":i" + string(i-lin     )):Borders(02):Weight = -4138.
    ws:Range("a" + string(i-lin - 2) + ":i" + string(i-lin - 2 )):Borders(03):Weight = -4138.
    ws:Range("a" + string(i-lin    ) + ":i" + string(i-lin     )):Borders(04):Weight = -4138.

 ws:Range("A" + string(i-lin - 3) + ":I" + string(i-lin)):Font:Color = -16776961.

if g-media < 1.81 then do:
 ws:Range("B" + string(i-lin - 2)):Interior:ColorIndex = 27.


end.
else
if g-media < 2.61 then do:
 ws:Range("B" + string(i-lin - 1)):Interior:ColorIndex = 27.
end.
else
if g-media < 3.41 then do:
 ws:Range("E" + string(i-lin - 2)):Interior:ColorIndex = 27.
end.
else
if g-media < 4.21 then do:
 ws:Range("E" + string(i-lin - 1)):Interior:ColorIndex = 27.

end.
else do:
 ws:Range("E" + string(i-lin )):Interior:ColorIndex = 27.

end.    
  
     c-logo = search("images\logo" + string(rh_estab.cdn_empresa) + ".jpg").
IF rh_estab.cdn_empresa = "420" OR rh_estab.cdn_empresa = "410" THEN DO:
    if c-logo <> ? then 
      ws:Shapes:AddPicture (c-logo,1,1,0,0,85,55).

END.
ELSE DO:
    if c-logo <> ? then 
      ws:Shapes:AddPicture (c-logo,1,1,0,0,130,40).

END.
     				
 

 /*
        if  trim(session:printer-name) <> "" then do:
                ws:PageSetup:Orientation    = 1 /*retrato*/ NO-ERROR.
                ws:PageSetup:PaperSize      = 9 /*A4*/ NO-ERROR.
                ws:PageSetup:FitToPagesWide = 1 NO-ERROR.
                ws:PageSetup:FitToPagesTall = 1 NO-ERROR.
                ws:PageSetup:LeftMargin     = 29 NO-ERROR. /* 58 Corresponde a 2cm */
                ws:PageSetup:RightMargin    = 29 NO-ERROR. /* 58 Corresponde a 2cm */
                ws:PageSetup:TopMargin      = 29 NO-ERROR. /* 58 Corresponde a 2cm */
                ws:PageSetup:BottomMargin   = 29 NO-ERROR. /* 58 Corresponde a 2cm */
                ws:PageSetup:FooterMargin   = 0 NO-ERROR.
                ws:PageSetup:HeaderMargin   = 0 NO-ERROR.
                ws:PageSetup:Zoom           = FALSE NO-ERROR.
/*                ws:PageSetup:PrintTitleRows = "$1:$8" NO-ERROR. */
            end.  
 */
    
   /*O mÇtodo SaveAs Ç semelhante ao comando 'Salvar Como':*/
   
   /*c-arquivo = session:temp-directory +  "Individual-agrup-"  + string(funcionario.cdn_estab) + "-" + string(funcionario.cdn_funcionario) + "-" + string(dt-ref,"99999999") + "-" + string( i-cdn_aval) + "-" + string(time) + ".xlsx".
     */
 ws:Rows("140:140"):Delete  .
    ws:range("A1:A1"):select.
     
    /*run pi-acompanhar in h-acomp(input "Salvando Planilha: " +  STRING(funcionario.cdn_funcionario) + "-" + funcionario.nom_pessoa_fisic ).
   
    wb:SaveAs(c-arquivo,51,,,,,).
      */
       
    /*51 = FileFormat:=xlOpenXMLWorkbook significa que o arquivo ser† salvo com a
           extens∆o .xlsx, ou seja, Pasta de Trabalho sem macro.*/
  
    /*  appExcel:Visible = l-mostra.         
      */
    /*Agora, deseja-se sair da Aplicaá∆o. Observe que o mÇtodo Ç executado
      no n°vel da Aplicaá∆o, finalizando a Aplicaá∆o e todos os objetos
      criados por ela. Se n∆o quiser sair da Aplcaá∆o, basta apagar a linha abaixo:*/
      
       
    
    /*Apenas para limpar mem¢ria*/
   
    
/*    if not l-mostra then appExcel:Quit().
     
    if valid-handle(obj) then  release object obj.
    if valid-handle(rng) then  RELEASE OBJECT rng.
    if valid-handle(ws) then  RELEASE OBJECT ws.
    if valid-handle(wb) then  RELEASE OBJECT wb.
    if valid-handle(cht) then  Release OBJECT cht.
    if valid-handle(appExcel) then  RELEASE OBJECT appExcel.

 
    
    dos silent copy value(c-arquivo) v:\temp.
    dos silent del value(session:temp-directory + "*.x*").
  */ 
    
End procedure.


 
procedure pi-grava.
    create tt-func-102.
    assign 
        tt-func-102.cdn_estab       = i-cdn_estab
        tt-func-102.cdn_funcionario = i-cdn_func
        tt-func-102.cod_rh_ccusto   = c-cod_rh_ccusto
        tt-func-102.cdn_aval        = i-cdn_aval
        tt-func-102.data-aval       = dt-ref
        tt-func-102.competencia     = c-competencia
        tt-func-102.resultado       = d-resultado
        tt-func-102.dominio         = c-dominio
        tt-func-102.narrativa       = c-narrativa
        tt-func-102.seq = i-seq .
        
    

End procedure.


procedure pi-dados.
    
  DEFINE VARIABLE d-media-103-emp     AS DECIMAL  NO-UNDO.
  DEFINE VARIABLE d-result-103-emp    AS DECIMAL  NO-UNDO.
  DEFINE VARIABLE i-qt-result-103-emp AS integer  NO-UNDO.
  DEFINE VARIABLE d-media-103-cc      AS DECIMAL  NO-UNDO.
  DEFINE VARIABLE d-result-103-cc     AS DECIMAL  NO-UNDO.
  DEFINE VARIABLE i-qt-result-103-cc  AS integer  NO-UNDO.

  ASSIGN d-media-103-emp      = 0
         d-result-103-emp     = 0
         i-qt-result-103-emp  = 0
         d-media-103-cc       = 0
         d-result-103-cc      = 0
         i-qt-result-103-cc   = 0.

  if avail b-cc-tt-func-103 then           
      DO i  = 1 TO 3:
         IF b-cc-tt-func-103.md-resultado[i] > 0 THEN DO:
             ASSIGN i-qt-result-103-cc = i-qt-result-103-cc + 1
                    d-result-103-cc    = d-result-103-cc + b-cc-tt-func-103.md-resultado[i]
                    d-media-103-cc     = d-result-103-cc / i-qt-result-103-cc.                
         END.
    
      END.

  if avail b-emp-tt-func-103 then           
      DO i  = 1 TO 3:
         IF b-emp-tt-func-103.md-resultado[i] > 0 THEN DO:
             ASSIGN i-qt-result-103-emp = i-qt-result-103-emp + 1
                    d-result-103-emp    = d-result-103-emp + b-emp-tt-func-103.md-resultado[i]
                    d-media-103-emp     = d-result-103-emp / i-qt-result-103-emp.                
         END.

      END.

  i-lin = i-lin + 1.
  

    Assign
            ws:Range("A" + string(i-lin)):value = tt-func-102.competencia
            ws:Range("B" + string(i-lin)):value = tt-func-102.resultado

            ws:Range("C" + string(i-lin)):value = d-media-103-cc
            ws:Range("D" + string(i-lin)):value = d-media-103-emp 


            ws:Range("E" + string(i-lin)):value = tt-func-102.dominio
            ws:Range("F" + string(i-lin)):value = tt-func-102.narrativa.
            ws:Range("E" + string(i-lin) + ":C" + string(i-lin)):WrapText = yes.

            ws:Range("F" + string(i-lin) + ":G" + string(i-lin)):unmerge.
            ws:Columns("F:F"):ColumnWidth = 59.            
            ws:Range("F" + string(i-lin) + ":F" + string(i-lin)):WrapText = yes.
            ws:Range("F" + string(i-lin) + ":F" + string(i-lin)):Rows:Autofit.
            ws:Range("F" + string(i-lin) + ":F" + string(i-lin)):Horizontalalignment = 2.
            ws:Range("G" + string(i-lin) + ":G" + string(i-lin)):WrapText = yes.
            ws:Range("G" + string(i-lin) + ":G" + string(i-lin)):Horizontalalignment = 2.
            ws:Range("F" + string(i-lin) + ":G" + string(i-lin)):SELECT.
            ws:Range("F" + string(i-lin) + ":G" + string(i-lin)):merge.
          
            ws:Columns("F:F"):ColumnWidth = 22.  
            
  
end procedure.

procedure pi-formata-102.

    /*
    rng = ws:range("A8:E8").
    
    rng:Select.
    
   
    
     rng:Font:FontStyle = "Negrito".
     rng:Font:Size = 12. 
     rng:Font:Name = "Arial".
    
   
   
       rng:Interior:ColorIndex = 24.
    
   */
     
    
    /* Formata numeros e datas
       "#.##0,00"   - numeros com 2 decimais
       "0"          - numeros inteiros
       "00000000"   - inteiros com zeros a esquerda
       "dd/mm/aaaa" - Datas dia , màs e ano
    
    */
    
    /*formata a coluna inteira*/
   
    
    ws:Columns("A:A"):NumberFormat = "@".
    ws:Columns("B:D"):NumberFormat = "#.##0,00".    
    
    /*  Alinhamentos
        1 - direita
        2 - Esquerda
        3 - centro
    
    */
    
    
     ws:Range("A8:e" + string(i-lin)):Horizontalalignment = 3.
 
   
     
    /*Bordas 4 lados*/
     
   /* ws:range("A4:E" + STRING(6) ):Borders(01):LineStyle = 1.
    ws:range("A4:E" + STRING(6) ):Borders(02):LineStyle = 1.
    ws:range("A4:E" + STRING(6) ):Borders(03):LineStyle = 1.
    ws:range("A4:E" + STRING(6) ):Borders(04):LineStyle = 1.
    */
    ws:range("A8:G" + STRING(i-lin) ):Borders(01):LineStyle = 1.
    ws:range("A8:G" + STRING(i-lin) ):Borders(02):LineStyle = 1.
    ws:range("A8:G" + STRING(i-lin) ):Borders(03):LineStyle = 1.
    ws:range("A8:G" + STRING(i-lin) ):Borders(04):LineStyle = 1.
            

   
    ws:range("A19"):Select.

    rng =   ws:range("A19"). 
     
     ws:Shapes:AddChart:Select.
     ws:ChartObjects(1):Left = 1.
     ws:ChartObjects(1):Top = rng:Top + 3.
     
     ws:ChartObjects(1):Height = 325.
     ws:ChartObjects(1):Width  = 621.
     
     cht = ws:ChartObjects(1):Chart.
     cht:ChartType = 82.
     
    cht:SeriesCollection:NewSeries.
    
    Assign
    
    cht:SeriesCollection(1):Name = "='esds102 e 103'!$B$5"
    cht:SeriesCollection(1):Values = "='esds102 e 103'!$B$6:$B$" + string(i-lin)
    cht:SeriesCollection(1):XValues = "='esds102 e 103'!$A$6:$A$" + string(i-lin)
    
    /*cht:SeriesCollection(1):Name = "=Plan4!$B$8"
    cht:SeriesCollection(1):Values = "=Plan4!$B$9:$B$" + string(i-lin)
    cht:SeriesCollection(1):XValues = "=Plan4!$A$9:$A$" + string(i-lin)
    */
    cht:Axes(2,1):MinimumScale = 1
    cht:Axes(2,1):MaximumScale = 5
    cht:Axes(2,1):MajorUnit = 1
    cht:Axes(2,1):MajorUnit = 0.8
    .
     cht:HasTitle = no.
     cht:HasLegend = no.
     

     
     /*obj = cht:SetElement(1) .
     obj:visible = no.
     */
 /* 
    ws:Range("A41"):value = "Registre abaixo as aá‰es alinhadas entre lider e liderado para construá∆o do seu PDI - Plano de Desenvolvimento Individual".
    ws:Range("A41:e41"):merge.
   
    
    ws:Range("A41:e41"):Interior:ColorIndex = 24.
    ws:Range("A42:e42"):Interior:ColorIndex = 34.
    ws:Range("A42"):value = "Aá∆o".
    ws:Range("A42:d42"):merge.
    ws:Range("A43:d43"):merge.
    ws:Range("A44:d44"):merge.
    ws:Range("A45:d45"):merge.
    ws:Range("A46:d46"):merge.                
    ws:Range("A42:d42"):Horizontalalignment = 3.    
    ws:Range("e42"):value = "Prazo".
    ws:Range("e42"):Horizontalalignment = 3.    
    ws:Rows("42:46"):RowHeight = 22.8.
 
    ws:Range("A41:E46"):Borders(01):LineStyle = 1.
    ws:Range("A41:E46"):Borders(02):LineStyle = 1.   
    ws:Range("A41:E46"):Borders(03):LineStyle = 1.
    ws:Range("A41:E46"):Borders(04):LineStyle = 1.  

    ws:Range("a41:a46"):Borders(01):Weight = -4138.
    ws:Range("e41:E46"):Borders(02):Weight = -4138.   
    ws:Range("A41:e41"):Borders(03):Weight = -4138.
    ws:Range("A46:E46"):Borders(04):Weight = -4138. 
    
      
 
     rng = ws:range("A41:E41").
    
     rng:Select.
     rng:Font:FontStyle = "Negrito".
     rng:Font:Size = 10. 
     rng:Font:Name = "Arial".
     
     rng = ws:range("A42:E42").
    
     rng:Select.
     rng:Font:FontStyle = "Negrito".
     rng:Font:Size = 12. 
     rng:Font:Name = "Arial".
   */  

    if c-logo-dsp <> ? then 
      ws:Shapes:AddPicture (c-logo-dsp,1,1,489,0,130,50).
   
     c-logo = search("images\logo" + string(rh_estab.cdn_empresa) + ".jpg").

    IF rh_estab.cdn_empresa = "420" OR rh_estab.cdn_empresa = "410" THEN DO: /*solic-318*/ 
        if c-logo <> ? then 
              ws:Shapes:AddPicture (c-logo,1,1,0,0,85,55).
        
    END.
    ELSE DO:
        if c-logo-dsp <> ? then 
          ws:Shapes:AddPicture (c-logo-dsp,1,1,0,0,130,40).
    
    END. 

          				
    if  trim(session:printer-name) <> "" then do:
                ws:PageSetup:Orientation    = 1 /* retrato*/ NO-ERROR.
                ws:PageSetup:PaperSize      = 9 /*A4*/ NO-ERROR.
                ws:PageSetup:FitToPagesWide = 1 NO-ERROR.
                ws:PageSetup:FitToPagesTall = 1 NO-ERROR.
                ws:PageSetup:LeftMargin     = 29 NO-ERROR. /* 58 Corresponde a 2cm */
                ws:PageSetup:RightMargin    = 29 NO-ERROR. /* 58 Corresponde a 2cm */
                ws:PageSetup:TopMargin      = 29 NO-ERROR. /* 58 Corresponde a 2cm */
                ws:PageSetup:BottomMargin   = 29 NO-ERROR. /* 58 Corresponde a 2cm */
                ws:PageSetup:FooterMargin   = 0 NO-ERROR.
                ws:PageSetup:HeaderMargin   = 0 NO-ERROR.
                ws:PageSetup:Zoom           = FALSE NO-ERROR.
/*                ws:PageSetup:PrintTitleRows = "$1:$8" NO-ERROR. */
            end.  

    
   /*O mÇtodo SaveAs Ç semelhante ao comando 'Salvar Como':*/
        /*
   c-arquivo = session:temp-directory + "Consolidado-" + string(funcionario.cdn_estab) + "-" + string(funcionario.cdn_funcionario) + "-" + string(dt-ref,"99999999") + "-" + string( i-cdn_aval) + "-" + string(time) + ".xlsx".

     run pi-acompanhar in h-acomp(input "Salvando Planilha: " +  STRING(funcionario.cdn_funcionario) + "-" + funcionario.nom_pessoa_fisic ).
   
    wb:SaveAs(c-arquivo,51,,,,,).
    
    /*51 = FileFormat:=xlOpenXMLWorkbook significa que o arquivo ser† salvo com a
           extens∆o .xlsx, ou seja, Pasta de Trabalho sem macro.*/
    
     appExcel:Visible = l-mostra.
      */ 
    
    /*Agora, deseja-se sair da Aplicaá∆o. Observe que o mÇtodo Ç executado
      no n°vel da Aplicaá∆o, finalizando a Aplicaá∆o e todos os objetos
      criados por ela. Se n∆o quiser sair da Aplcaá∆o, basta apagar a linha abaixo:*/
      
    
    
    /*Apenas para limpar mem¢ria*/
            /*
    ws:range("A1:A1"):select.
    
    
    if not l-mostra then appExcel:Quit().


    
    if valid-handle(obj) then  release object obj.
    if valid-handle(rng) then  RELEASE OBJECT rng.
    if valid-handle(ws) then  RELEASE OBJECT ws.
    if valid-handle(wb) then  RELEASE OBJECT wb.
    if valid-handle(cht) then  Release OBJECT cht.
    if valid-handle(appExcel) then  RELEASE OBJECT appExcel.
  
    
    dos silent copy value(c-arquivo)  v:\temp.
    dos silent del value(session:temp-directory + "*.x*").
          */
End procedure.


procedure pi-grava-103.
    find first tt-func-103 where 
        tt-func-103.cdn_estab        = i-cdn_estab     and
        tt-func-103.cod_rh_ccusto    = c-cod_rh_ccusto and
        tt-func-103.cdn_funcionario  = i-cdn_func      and
        tt-func-103.seq              = i-seq           and
        tt-func-103.competencia      = c-competencia  no-error.
        
    if not avail tt-func-103 then do:    
        create tt-func-103.
        assign 
            tt-func-103.cdn_estab       = i-cdn_estab
            tt-func-103.cdn_funcionario = i-cdn_func
            tt-func-103.cod_rh_ccusto   = c-cod_rh_ccusto
            tt-func-103.seq             = i-seq 
            tt-func-103.competencia     = c-competencia.

    end.    
    

          
         IF  avpes_emitid.idi_tip_avaldor_avpes = 5 THEN i = 1.
        ELSE
            IF avpes_emitid.idi_tip_avaldor_avpes = 7 THEN i = 2.
        ELSE
             i= 3.     
             
        
       
    assign     
        tt-func-103.cdn_aval        = i-cdn_aval
        tt-func-103.data-aval       = dt-ref
        tt-func-103.competencia     = c-competencia
        tt-func-103.resultado[i]       = tt-func-103.resultado[i] + d-resultado
        tt-func-103.qt-resultado[i]    = tt-func-103.qt-resultado[i] + 1
        tt-func-103.md-resultado[i]    =  tt-func-103.resultado[i] / tt-func-103.qt-resultado[i].
        tt-func-103.cdn_empresa     = funcionario.cdn_empresa.
        
    

End procedure.

procedure pi-formata-103.

 IF TRUE THEN DO:
      ws:range("A" + string(43)):Select.

    rng =   ws:range("A" + string(43)). 
     
     ws:Shapes:AddChart:Select.
     ws:ChartObjects(2):Left = 1.
     ws:ChartObjects(2):Top = rng:Top.
     
     ws:ChartObjects(2):Height = 368.
     ws:ChartObjects(2):Width  = 520.
     
     cht = ws:ChartObjects(2):Chart.
     cht:ChartType = 54.
     
    cht:SeriesCollection:NewSeries.
    
    Assign
    
    cht:SeriesCollection(1):Name = "='esds102 e 103'!$b$5"
    cht:SeriesCollection(1):Values = "='esds102 e 103'!$b$6:$b$" + string(i-lin).
     cht:SeriesCollection:NewSeries.
    Assign
    cht:SeriesCollection(2):Name = "='esds102 e 103'!$c$5"
    cht:SeriesCollection(2):Values = "='esds102 e 103'!$c$6:$c$" + string(i-lin) .
    cht:SeriesCollection:NewSeries.
    Assign
       
    cht:SeriesCollection(3):Name = "='esds102 e 103'!$d$5"
    cht:SeriesCollection(3):Values = "='esds102 e 103'!$d$6:$d$" + string(i-lin).
    
    cht:SeriesCollection(1):XValues = "='esds102 e 103'!$a$6:$a$" + string(i-lin).
    
    /*cht:SeriesCollection(1):Name = "=Plan4!$B$8"
    cht:SeriesCollection(1):Values = "=Plan4!$B$9:$B$" + string(i-lin)
    cht:SeriesCollection(1):XValues = "=Plan4!$A$9:$A$" + string(i-lin)
    */
    
    ASSIGN
    cht:Axes(2,1):MinimumScale = 1
    cht:Axes(2,1):MaximumScale = 5
    cht:Axes(2,1):MajorUnit = 1
    cht:Axes(2,1):MajorUnit = 0.8
    .
     cht:HasTitle = no.
     cht:HasLegend = YES.

 cht:PlotArea:Format:ThreeD:RotationX = 0.
 cht:PlotArea:Format:ThreeD:RotationY = 90.
 cht:PlotArea:Format:ThreeD:FieldOfView = 30.      
     /*
    cht:PlotArea:Select.
    cht:Selection:Format:ThreeD:RotationX = 20.
    cht:Selection:Format:ThreeD:RotationY = 15.
    cht:Selection:Format:ThreeD:FieldOfView = 0.
    */
    
     
     /*obj = cht:SetElement(1) .
     obj:visible = no.
     */
  
   
        ws:range("f1"):select.
 END.
     /*
    if c-logo-dsp <> ? then 
   

      ws:Shapes:AddPicture (c-logo-dsp,1,1,390,0,130,50).
   
     c-logo = search("images\logo" + string(rh_estab.cdn_empresa) + ".jpg").
     
     
     ws:range("a1"):select.
     
    if c-logo <> ? then 
        ws:Shapes:AddPicture (c-logo,1,1,0,0,130,40).
       */ 			
             
 if  trim(session:printer-name) <> "" then do:
                ws:PageSetup:Orientation    = 1 /* retrato*/ NO-ERROR.
                ws:PageSetup:PaperSize      = 9 /*A4*/ NO-ERROR.
                ws:PageSetup:FitToPagesWide = 1 NO-ERROR.
                ws:PageSetup:FitToPagesTall = 1 NO-ERROR.
                ws:PageSetup:LeftMargin     = 29 NO-ERROR. /* 58 Corresponde a 2cm */
                ws:PageSetup:RightMargin    = 29 NO-ERROR. /* 58 Corresponde a 2cm */
                ws:PageSetup:TopMargin      = 29 NO-ERROR. /* 58 Corresponde a 2cm */
                ws:PageSetup:BottomMargin   = 29 NO-ERROR. /* 58 Corresponde a 2cm */
                ws:PageSetup:FooterMargin   = 0 NO-ERROR.
                ws:PageSetup:HeaderMargin   = 0 NO-ERROR.
                ws:PageSetup:Zoom           = FALSE NO-ERROR.
/*                ws:PageSetup:PrintTitleRows = "$1:$8" NO-ERROR. */
            end.  

    
   /*O mÇtodo SaveAs Ç semelhante ao comando 'Salvar Como':*/
   
   c-arquivo = session:temp-directory + "esds106-consolidado-" + string(funcionario.cdn_estab) + "-" + string(funcionario.cdn_funcionario) + "-" + string(dt-ref,"99999999") + "-" + string( i-cdn_aval) + "-" + string(time) + ".xlsx".
 

     run pi-acompanhar in h-acomp(input "Salvando Planilha: " +  STRING(funcionario.cdn_funcionario) + "-" + funcionario.nom_pessoa_fisic ).
     
     ws = wb:Sheets:item("esds101").
     ws:activate.

     ws:range("A1:A1"):select.
      

    wb:SaveAs(c-arquivo,51,,,,,).
    
    /*51 = FileFormat:=xlOpenXMLWorkbook significa que o arquivo ser† salvo com a
           extens∆o .xlsx, ou seja, Pasta de Trabalho sem macro.*/
       appExcel:Visible = l-mostra.    
    
    /*Agora, deseja-se sair da Aplicaá∆o. Observe que o mÇtodo Ç executado
      no n°vel da Aplicaá∆o, finalizando a Aplicaá∆o e todos os objetos
      criados por ela. Se n∆o quiser sair da Aplcaá∆o, basta apagar a linha abaixo:*/
      
    
    /*Apenas para limpar mem¢ria*/
    
    
    if not l-mostra then appExcel:Quit().

    
    if valid-handle(obj) then  release object obj.
    RELEASE OBJECT rng.
    RELEASE OBJECT ws.
    RELEASE OBJECT wb.
    Release OBJECT cht.
    RELEASE OBJECT appExcel.
    
    dos silent copy value(c-arquivo)  v:\temp.
    dos silent del value(session:temp-directory + "*.x*").
    
    
End procedure.

