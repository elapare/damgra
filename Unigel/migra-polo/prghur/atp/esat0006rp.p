/********************************************************************************
* Programa: esat0006rp.p
* Data....: jan/2014
* Autor...: Edson
* Alteracao :  
* Objetivo: Lista avalia‡Æo de rea‡Æo
*         
* VersÆo..: 2.00.000                            

*******************************************************************************/
/*---------------- Include de controle de VersÆo ------------------*/ 
{bf/buffersHCM.i}
{include/i-prgvrs.i esat0006RP 2.00.00.000}


def new global shared var i-ep-codigo-usuario  like empresa.ep-codigo no-undo.
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
 
/******** Defini‡Æo Temp-table para Recebimento de Parametro **********************/
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
        FIELD v_cdn_empres_usuar AS CHAR
        field cdn_curso_trein  as integer
        field cdn_turma_trein  as integer
        field cdn_empresa_ini      as CHAR
        field cdn_empresa_fim      as CHAR
        FIELD cdn_estab_ini    AS CHAR
        FIELD cdn_estab_fim    AS CHAR
        FIELD cdn_funcionario_ini AS INTEGER
        FIELD cdn_funcionario_fim AS INTEGER   
        FIELD log_lista_demitidos AS LOGICAL
        FIELD tg-abre-tela AS LOGICAL
         .     



 
/* recebimento de parƒmetros */
def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.
  
  /*
CREATE tt-param.

ASSIGN
    
      tt-param.cdn_empresa_ini          = "420"   
      tt-param.cdn_empresa_fim              = "420"
      tt-param.cdn_estab_ini            = "421"
      tt-param.cdn_estab_fim            = "426"
      tt-param.cdn_funcionario_ini      = 0
      tt-param.cdn_funcionario_fim      = 370000
      tt-param.log_lista_demitidos  = NO 
      tt-param.tg-abre-tela = NO
      .    
    */
 
    
/****************** Defini‡ao de Vari veis de Processamento do Relat¢rio *********************/
def var h-acomp              as handle no-undo.
Def Var dat-trab As Date.
/****************** Defini‡ao de  Frames e Forms do Relat¢rio 132 Colunas ***************************************/ 

/****************** INCLUDE COM VARIµVEIS GLOBAIS *********************/
 


    DEFINE VARIABLE c-emp-image-ant AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE c-area AS CHARACTER  FORMAT "x(30)"  NO-UNDO.
    DEFINE VARIABLE c_des_local_trein AS CHARACTER FORMAT "x(30)"   NO-UNDO.
    DEFINE VARIABLE c_nome_instrutor AS CHARACTER FORMAT "x(30)"   NO-UNDO.
    DEFINE VARIABLE i-grupo-ant AS INTEGER     NO-UNDO.
    DEFINE VARIABLE l-trocou AS LOGICAL  INITIAL YES  NO-UNDO.
    DEFINE VARIABLE i-im AS INTEGER     NO-UNDO.
    DEFINE BUFFER b-rh_pessoa_fisic FOR rh_pessoa_fisic.
    DEFINE VARIABLE v_des_respos_efetd_func_avpes LIKE avpes_item.des_impres_item_avpes NO-UNDO.
    DEFINE VARIABLE c-desc-item AS CHARACTER  FORMAT "x(200)" NO-UNDO.
    def var c-modelo-planilha  as char format "x(50)"         no-undo.
    def var c-excel            as com-handle                  NO-UNDO.
    def var c-planilha         as com-handle.
    def var c-relatorio        as com-handle.
    def var c-figura           as com-handle.
    DEFINE VARIABLE c-arq AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE c-arq-anexo AS CHARACTER   NO-UNDO.
    DEF VAR c-arquivo AS CHAR NO-UNDO.

    DEFINE VARIABLE i-ct-func AS INTEGER     NO-UNDO.

    DEFINE VARIABLE i-linha AS INTEGER     NO-UNDO.
    DEFINE VARIABLE i-linha-ct AS INTEGER     NO-UNDO.
    def var c-logo as char no-undo. 

    def var c-logo-dsp as char no-undo. 
    
DEFINE BUFFER b1-curso_trein FOR curso_trein.            
DEFINE BUFFER b-curso_trein FOR curso_trein.            

DEFINE VARIABLE c-valor-1 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-valor-2 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-valor-3 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-valor-4 AS CHARACTER   NO-UNDO.
run utp/ut-acomp.p persistent set h-acomp.
    
run pi-inicializar in h-acomp(input "Selecionando e criando planilha").
         
for Each funcionario no-lock
                   where  
                       
                         funcionario.cdn_empresa        >= tt-param.cdn_empresa_ini                    and
                         funcionario.cdn_empresa        <= tt-param.cdn_empresa_fim                    and
                         funcionario.cdn_estab          >= tt-param.cdn_estab_ini       and
                         funcionario.cdn_estab          <= tt-param.cdn_estab_fim       and
                         funcionario.cdn_funcionario    >= tt-param.cdn_funcionario_ini and
                         funcionario.cdn_funcionario    <= tt-param.cdn_funcionario_fim and
                    /*     funcionario.cod_rh_ccusto      >= tt-param.cod_rh_ccusto_ini   and
                         funcionario.cod_rh_ccusto      <= tt-param.cod_rh_ccusto_fim   and
                         funcionario.cod_unid_lotac     >= tt-param.cod_unid_lotac_ini  and
                         funcionario.cod_unid_lotac     <= tt-param.cod_unid_lotac_fim  and*/
                         funcionario.dat_admis_func     <= TODAY  /*  AND
    CAN-FIND(FIRST func_neces_trein OF funcionario  NO-LOCK)   */   use-index fncnr_id


                          break by funcionario.cdn_empresa
                                by funcionario.cdn_estab
                                by funcionario.cdn_funcionario:


     
                 IF tt-param.log_lista_demitidos  = NO      AND 
                           funcionario.dat_desligto_func <> ? THEN NEXT.

                                             

                 for last func_ccusto of funcionario where
                        func_ccusto.dat_inic_lotac_func  <=  TODAY
                                no-lock
                            .

                 end.




                   if not avail func_ccusto then 
                        find last func_ccusto of funcionario where 
                              func_ccusto.dat_fim_lotac_func  =  12/31/9999 
                              no-lock no-error.

                           






                  find rh_estab of funcionario no-lock no-error.
                  Find rh_pessoa_jurid Of rh_estab No-lock No-error.

                  if avail func_ccusto then 
                   find rh_ccusto of func_ccusto no-lock no-error.       

                  if not avail func_ccusto or not avail rh_ccusto then
                   find rh_ccusto of funcionario no-lock no-error.


                  
                  FIND rh_pessoa_fisic OF funcionario NO-LOCK NO-ERROR.

                  /* Find Last histor_sal_func OF funcionario Where
                            histor_sal_func.dat_liber_sal <= tt-param.dt-ref-hist NO-LOCK NO-ERROR.
                   IF AVAIL  histor_sal_func THEN
                       FIND cargo OF histor_sal_func NO-LOCK NO-ERROR.

                     if not avail cargo OR NOT AVAIL histor_sal_func then */
                      FIND FIRST cargo OF funcionario NO-LOCK NO-ERROR.

                    /*   cargo.dsl_complet_cargo = "Para este cargo o funcionario dever  ter varias habilidade, inclusive de fazer malabarismos com o or‡amento da empresa, afim de que a margem de lucro cres‡a mensalmente. Tamb‚m dever  ter disponibilidade para trabalhar com alto risco de morte".
                      */
                  Find grau_instruc Where grau_instruc.cdn_grau_instruc = rh_pessoa_fisic.cdn_grau_instruc No-lock No-error.

                  find unid_lotac of funcionario no-lock no-error.

  i-linha = i-linha + 1.

    IF SUBSTRING(STRING(i-linha,"9999999"),7,1) = "0" THEN
    run pi-acompanhar in h-acomp(input "Contando Funcionario... " + STRING(i-linha,"9999999")).


                /*  run pi-acompanhar in h-acomp(input "Funcionario: " +  STRING(funcionario.cdn_funcionario) + "-" +  funcionario.nom_pessoa_fisic).
*/

                  RUN pi-cria-planilha.
                  IF RETURN-VALUE = "nok" THEN NEXT.
 

         ASSIGN
             c-relatorio:range("B" + STRING(4)):VALUE =  funcionario.cdn_estab + "-" + rh_estab.nom_pessoa_jurid
             c-relatorio:range("F" + STRING(4)):VALUE = STRING(funcionario.cdn_cargo_basic ) + "/" +
                   STRING(Funcionario.cdn_niv_cargo) + "-" + 
                   Caps(cargo.des_cargo)
             c-relatorio:range("B" + STRING(5)):VALUE = Caps(funcionario.nom_pessoa_fisic)  
             c-relatorio:range("F" + STRING(5)):VALUE = (if avail func_ccusto then func_ccusto.cod_rh_ccusto else funcionario.cod_rh_ccusto) 
              + "-" +  Caps(rh_ccusto.des_rh_ccusto) 

             c-relatorio:range("B" + STRING(6)):VALUE = STRING(funcionario.cod_unid_lotac ) + "-" +
                 (IF AVAIL unid_lotac THEN unid_lotac.des_unid_lotac ELSE "")
             c-relatorio:range("F" + STRING(6)):VALUE = STRING(TODAY,"99/99/9999")
             c-relatorio:range("H" + STRING(6)):VALUE = funcionario.cdn_funcionario
             c-relatorio:range("A" + STRING(9)):VALUE = cargo.dsl_complet_cargo.

         ASSIGN 
               c-valor-1 = ""
               c-valor-2 = ""
               c-valor-3 = ""
               c-valor-4 = ""
               i-linha = 15
               i-linha-ct = 0.
         FOR EACH  curso_lotac_cargo WHERE  
                             curso_lotac_cargo.cdn_empresa            =     funcionario.cdn_empresa     AND
                             curso_lotac_cargo.cdn_estab              =     funcionario.cdn_estab       AND
                             curso_lotac_cargo.cdn_cargo_basic        =     funcionario.cdn_cargo_basic    AND
                             curso_lotac_cargo.cdn_niv_cargo          =     funcionario.cdn_niv_cargo         AND
                             curso_lotac_cargo.cdn_plano_lotac        =     funcionario.cdn_plano_lotac  AND
                             curso_lotac_cargo.cod_unid_lotac         =     funcionario.cod_unid_lotac  NO-LOCK,
                EACH b1-curso_trein   WHERE 
                             b1-curso_trein.cdn_curso_trein = curso_lotac_cargo.cdn_curso_trein NO-LOCK.

                i-linha = i-linha + 1.
                i-linha-ct = i-linha-ct + 1.
                c-relatorio:rows(i-linha):SELECT.
                c-relatorio:rows(i-linha):COPY.
                c-excel:selection:INSERT.

                FIND FIRST b-curso_trein WHERE  
                                    b-curso_trein.cdn_curso_trein = curso_lotac_cargo.num_curso_equiv  NO-LOCK NO-ERROR.
                ASSIGN 
                   c-valor-1 =   STRING(curso_lotac_cargo.cdn_curso_trein) + "-" + 
                                  b1-curso_trein.des_curso_trein  
                   c-valor-2 =   IF AVAIL b-curso_trein THEN (STRING(curso_lotac_cargo.num_curso_equiv) + "-" +   
                                   b-curso_trein.des_curso_trein ) ELSE "" 
                   c-valor-3 =   ENTRY(curso_lotac_cargo.idi_niv_requis_cargo,"Indispens vel,Desejado,Recomendado" ) .
                   
                  ASSIGN
                      
                      c-relatorio:COLUMNS("A:A"):columnWIDTH = 38.86
                      c-relatorio:COLUMNS("C:C"):columnWIDTH = 49
                      c-relatorio:COLUMNS("F:F"):columnWIDTH = 46.58
                      c-relatorio:range("A" + STRING(i-linha)):VALUE =  c-valor-1
                      c-relatorio:range("C" + STRING(i-linha)):VALUE =  c-valor-2
                      c-relatorio:range("F" + STRING(i-linha)):VALUE =  c-valor-3.

                      c-relatorio:rows(i-linha):autofit.
                      c-relatorio:range("A" + STRING(i-linha) + ":B" + STRING(i-linha)):merge.
                      c-relatorio:COLUMNS("A:A"):columnWIDTH = 17.43.
                                            
                      c-relatorio:range("C" + STRING(i-linha) + ":E" + STRING(i-linha)):merge.
                      c-relatorio:COLUMNS("C:C"):columnWIDTH = 18.

                      c-relatorio:range("F" + STRING(i-linha) + ":I" + STRING(i-linha)):merge.
                      c-relatorio:COLUMNS("F:F"):columnWIDTH = 13.                                                           

         END.

         ASSIGN 
               c-valor-1 = ""
               c-valor-2 = ""
               c-valor-3 = ""
               c-valor-4 = ""
               i-linha = 20 + i-linha-ct.

       
         FOR EACH ficha_invent_trein WHERE ficha_invent_trein.num_pessoa_fisic = funcionario.num_pessoa_fisic NO-LOCK,
             EACH curso_trein WHERE curso_trein.cdn_curso_trein =  ficha_invent_trein.cdn_curso_trein NO-LOCK,
             FIRST tip_curso_trein WHERE 
                   tip_curso_trein.cdn_tip_curso_trein = curso_trein.cdn_tip_curso_trein NO-LOCK  .

               i-linha = i-linha + 1.
               i-linha-ct = i-linha-ct + 1.
               c-relatorio:rows(i-linha):SELECT.
               c-relatorio:rows(i-linha):COPY.
               c-excel:selection:INSERT.


              ASSIGN 
                   c-valor-1 =   STRING(ficha_invent_trein.cdn_curso_trein) + "-" + 
                                  curso_trein.des_curso_trein + " - " + tip_curso_trein.des_tip_curso_trein 
                   c-valor-2 =  ficha_invent_trein.nom_vers_trein 
                   c-valor-3 =   ficha_invent_trein.des_instit_trein .
                   
                  ASSIGN
                      
                      c-relatorio:COLUMNS("A:A"):columnWIDTH = 38.86
                      c-relatorio:COLUMNS("C:C"):columnWIDTH = 49
                      c-relatorio:COLUMNS("D:D"):columnWIDTH = 46.58
                      c-relatorio:range("A" + STRING(i-linha)):VALUE =  c-valor-1
                      c-relatorio:range("C" + STRING(i-linha)):VALUE =  c-valor-2
                      c-relatorio:range("D" + STRING(i-linha)):VALUE =  c-valor-3
                      c-relatorio:range("G" + STRING(i-linha)):VALUE =  ficha_invent_trein.qtd_hora_curso_trein 
                      c-relatorio:range("H" + STRING(i-linha)):VALUE =  ficha_invent_trein.dat_inic_curso_trein 
                      c-relatorio:range("I" + STRING(i-linha)):VALUE =  ficha_invent_trein.dat_fim_curso_trein  .

                      c-relatorio:rows(i-linha):autofit.
                      c-relatorio:range("A" + STRING(i-linha) + ":B" + STRING(i-linha)):merge.
                      c-relatorio:COLUMNS("A:A"):columnWIDTH = 17.43.
                                            
                      c-relatorio:range("C" + STRING(i-linha) + ":C" + STRING(i-linha)):merge.
                      c-relatorio:COLUMNS("C:C"):columnWIDTH = 18.

                      c-relatorio:range("D" + STRING(i-linha) + ":F" + STRING(i-linha)):merge.
                      c-relatorio:COLUMNS("D:D"):columnWIDTH = 17.43.                                                           

         
        
        END.
        
        ASSIGN 
              c-valor-1 = ""
              c-valor-2 = ""
              c-valor-3 = ""
              c-valor-4 = ""
              i-linha = 25 + i-linha-ct.

        FOR EACH  func_neces_trein OF funcionario NO-LOCK,
             EACH b-curso_trein WHERE             
                  b-curso_trein.cdn_curso_trein =   func_neces_trein.cdn_curso_trein NO-LOCK,
             EACH motiv_solicit_trein WHERE 
                   motiv_solicit_trein.cdn_motiv_solicit_trein = int(func_neces_trein.val_livre_1) NO-LOCK,
             FIRST tip_curso_trein WHERE 
                   tip_curso_trein.cdn_tip_curso_trein = b-curso_trein.cdn_tip_curso_trein NO-LOCK.


            i-linha = i-linha + 1.
            i-linha-ct = i-linha-ct + 1.
            c-relatorio:rows(i-linha):SELECT.
            c-relatorio:rows(i-linha):COPY.
            c-excel:selection:INSERT.
            
            
            ASSIGN 
                c-valor-1 =   STRING(func_neces_trein.cdn_curso_trein) + "-" + 
                               b-curso_trein.des_curso_trein + " - " + tip_curso_trein.des_tip_curso_trein 
                c-valor-2 =  motiv_solicit_trein.des_motiv_solicit_trein .
               
            
               ASSIGN
            
                   c-relatorio:COLUMNS("A:A"):columnWIDTH = 38.86
                   c-relatorio:COLUMNS("E:E"):columnWIDTH = 40.29
                   
                   c-relatorio:range("A" + STRING(i-linha)):VALUE =  c-valor-1
                   c-relatorio:range("E" + STRING(i-linha)):VALUE =  c-valor-2

                   c-relatorio:range("C" + STRING(i-linha)):VALUE =  ENTRY(func_neces_trein.idi_sit_neces_trein_func,{database/inpm/i02pm149.i 03})
                   c-relatorio:range("D" + STRING(i-linha)):VALUE =  entry(func_neces_trein.idi_niv_requis_cargo,{database/inpm/i06pm149.i 03}) 

                   c-relatorio:range("H" + STRING(i-linha)):VALUE =  func_neces_trein.dat_impl_nec_trein 
                   c-relatorio:range("I" + STRING(i-linha)):VALUE =  func_neces_trein.dat_lim_exec_trein.
            
                   c-relatorio:rows(i-linha):autofit.
                   c-relatorio:range("A" + STRING(i-linha) + ":B" + STRING(i-linha)):merge.
                   c-relatorio:COLUMNS("A:A"):columnWIDTH = 17.43.
            
                   c-relatorio:range("E" + STRING(i-linha) + ":G" + STRING(i-linha)):merge.
                   c-relatorio:COLUMNS("E:E"):columnWIDTH = 14.
            
                                                                    

            
              /*
            DISP 
                func_neces_trein.cdn_empresa 
                func_neces_trein.cdn_estab
                func_neces_trein.cdn_funcionario 
                func_neces_trein.cdn_cargo_basic 
                func_neces_trein.cdn_curso_trein
                b-curso_trein.des_curso_trein
                func_neces_trein.idi_sit_neces_trein_func
                func_neces_trein.idi_niv_requis_cargo 
                motiv_solicit_trein.des_motiv_solicit_trein
            
                 func_neces_trein.dat_impl_nec_trein 
                func_neces_trein.dat_lim_exec_trein 
            
                 func_neces_trein.cdn_tip_nec_func
                func_neces_trein.cdn_niv_cargo
                func_neces_trein.cdn_curso_substdo 
            
            
                func_neces_trein.cdn_instit_trein 
            
                func_neces_trein.dat_conclus_nec_func 
                func_neces_trein.dat_confir_nec_trein 
            
                func_neces_trein.des_nec_func_avpes
            
            /*       func_neces_trein.des_justif_subst*/ 
                 func_neces_trein.des_instit_trein 
                 func_neces_trein.idi_tip_sugest_nec_trein func_neces_trein.idi_orig_trein_aperfmto func_neces_trein.num_avpes_emitid func_neces_trein.num_nec_func_avpes func_neces_trein.num_pessoa_fisic func_neces_trein.num_pessoa_fisic_solicit
             WITH WIDTH 300 1 COL.
                */
            END.





            RUN pi-finaliza-impressao.

            i-ct-func = i-ct-func + 1.

            IF i-ct-func > 50 THEN LEAVE.
         
    END.

    RUN pi-finalizar IN h-acomp.

    IF VALID-HANDLE(c-excel)     THEN RELEASE OBJECT c-excel.
    IF VALID-HANDLE(c-relatorio) THEN RELEASE OBJECT c-relatorio.
    IF valid-handle(c-planilha)  THEN RELEASE OBJECT c-planilha.

    RETURN "ok".




PROCEDURE pi-cria-planilha:
    IF VALID-HANDLE(c-excel) THEN RELEASE OBJECT c-excel.
    IF VALID-HANDLE(c-relatorio) THEN RELEASE OBJECT c-relatorio.
    IF valid-handle(c-planilha) THEN RELEASE OBJECT c-planilha.


    CREATE "Excel.Application" c-excel.
              ASSIGN c-excel:DisplayAlerts = FALSE.
 
    ASSIGN c-modelo-planilha = search("prghur/modelos/mod-esat0006.xlsx") 
           c-arq             = SESSION:TEMP-DIRECTORY.       
       
    IF c-modelo-planilha = ?  THEN RETURN "nok".

    c-arquivo = c-arq + 'esat0006' + string(FUNCIONARIO.cdn_funcionario) + "-" + STRING(TODAY,"99999999") + STRING(time) + '.xlsx'.

    OS-COPY VALUE(c-modelo-planilha) VALUE(c-arquivo).
 
    assign c-planilha  = c-excel:Workbooks:OPEN(c-arquivo)
           c-relatorio = c-excel:Sheets:item(1)
           c-arq-anexo = (IF c-arq-anexo <> '' THEN ',' ELSE '') + c-arquivo.
 
END PROCEDURE.

PROCEDURE pi-finaliza-impressao:
DEF VAR i AS INT  NO-UNDO.


  c-planilha:SAVE().
 
  IF NOT tt-param.tg-abre-tela THEN 
        DOS SILENT COPY VALUE(c-arquivo)  VALUE(tt-param.arq-entrada).
  ELSE    
      ASSIGN
        c-excel:VISIBLE = YES.
        c-excel:WindowState = -4140.
 
    
END PROCEDURE.                  



