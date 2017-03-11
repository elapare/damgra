/**************************************************************\
***************************************************************
**  Programa: prghur\pep\upc\upc-pe2200-u00.p
**  Objetivo: verifica lanche no per°odo de hora extra".
**            caso encontre algu, latera para desconsidera e efetua o desc†lculo 
**            e o c†lculo novamente        
**  Autor...: Damgra - Edson 
**  Data....: Abril/2014
***************************************************************
\**************************************************************/

def input param p-ind-event  as char          no-undo.
def input param p-ind-object as char          no-undo.
def input param p-wgh-object as handle        no-undo.
def input param p-wgh-frame  as widget-handle no-undo.
def input param p-cod-table  as char          no-undo.
def input param p-row-table  as rowid         no-undo.
/*--------------------------------------------------------*/


{utp/ut-glob.i}



DEF NEW GLOBAL SHARED VAR h-bt-executar-pe2200     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-bt-executar-pe2200-esp     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-da-dat-ini-pe2200     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-da-dat-fim-pe2200     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-i-es-ini-pe2200     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-i-es-fim-pe2200     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-i-fc-ini-pe2200     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-i-fc-fim-pe2200     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-i-ind-selec-pe2200     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-br-digita-pe2200     AS WIDGET-HANDLE NO-UNDO.


DEFINE VARIABLE h-qry AS HANDLE      NO-UNDO.
DEFINE VARIABLE h-campo AS HANDLE      NO-UNDO.
DEFINE VARIABLE h-buffer AS HANDLE      NO-UNDO.
DEFINE VARIABLE  i-ncampos AS INTEGER     NO-UNDO.
DEFINE VARIABLE i AS INTEGER     NO-UNDO.

DEFINE VARIABLE l-lanche AS LOGICAL     NO-UNDO.
DEFINE VARIABLE i-interv AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-cdn_turno      AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-cdn_turma_trab AS INTEGER     NO-UNDO.


DEFINE VARIABLE dt-atu      AS DATE        NO-UNDO.
DEFINE VARIABLE d-acum      AS DECIMAL     NO-UNDO.
DEFINE VARIABLE d-acum-bh   AS DECIMAL     NO-UNDO.
DEFINE VARIABLE d-acum-ex   AS DECIMAL         NO-UNDO.

DEFINE VARIABLE c-desc      AS CHARACTER  FORMAT "X(16)"  NO-UNDO.
DEFINE VARIABLE d-fator     AS DECIMAL     NO-UNDO.
DEFINE VARIABLE c-data      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-mes       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE l-parcial   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE l-estouro   AS LOGICAL     NO-UNDO.



def var h-acomp              as handle no-undo.
def var raw-param as raw no-undo.
def temp-table tt-raw-digita
   field raw-digita as raw.

DEFINE BUFFER bfunc-ponto FOR funcionario.
DEF BUFFER b_func_ptoelet FOR func_ptoelet.

    define TEMP-TABLE tt-dia
        FIELD dt-dia AS DATE
        FIELD dt-sem AS CHAR
        INDEX chave IS PRIMARY UNIQUE
              dt-dia.

    DEFINE TEMP-TABLE tt_bco_hrs_compens_func NO-UNDO LIKE bco_hrs_compens_func
        FIELD cdn_tip_compcao_hrs_ant LIKE bco_hrs_compens_func.cdn_tip_compcao_hrs.


    DEFINE TEMP-TABLE tt-marc
    FIELD cdn_estab       LIKE par_marcac_ptoelet.cdn_estab
    FIELD cdn_funcionario LIKE par_marcac_ptoelet.cdn_funcionario
    FIELD dat_proces_mpe  LIKE par_marcac_ptoelet.dat_proces_mpe
    INDEX chave IS PRIMARY UNIQUE 
                    cdn_estab       
                    cdn_funcionario 
                    dat_proces_mpe  .

function fi-hora returns char (input  i-hora as int) .

if i-hora <= 86400 then
     return (string(i-hora,"HH:MM")).
    else
     return (string(i-hora - 86400,"HH:MM")).

end function.
DEFINE BUFFER b-marcac_ptoelet FOR marcac_ptoelet.
DEFINE VARIABLE v_cod_tip_dia AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dt-proces-mpe AS DATE        NO-UNDO.
DEFINE VARIABLE dt-ini-mpe AS DATE        NO-UNDO.
DEFINE VARIABLE dt-fim-mpe AS DATE        NO-UNDO.
DEFINE VARIABLE i-fc-ini   AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-fc-fim   AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-es-ini   AS CHAR    NO-UNDO.
DEFINE VARIABLE i-es-fim   AS CHAR    NO-UNDO.
 




&if "{&integr_param_calc}" = "" &then
    &global-define integr_param_calc  tt-param
    &global-define integr_digita_calc tt-digita
&endif

define {1} {2} temp-table {&integr_param_calc}
    field parametro               as logical /* Imprime parametros */
    field LOG_acomp               as logical
    field v_cdn_empres_usuar      like param_empres_rh.cdn_empresa
    field v_cdn_empresa_evento    like param_empres_rh.cdn_empresa
    field v_cod_unid_lotac_ini    like unid_lotac.cod_unid_lotac
    field v_cod_unid_lotac_fim    like unid_lotac.cod_unid_lotac
    field i-es-ini                like rh_estab.cdn_estab
    field i-es-fim                like rh_estab.cdn_estab
    field i-fc-ini                like funcionario.cdn_funcionario
    field i-fc-fim                like funcionario.cdn_funcionario
    field i-ccusto-ini            like funcionario.cod_rh_ccusto
    field i-ccusto-fim            like funcionario.cod_rh_ccust
    field cod_clas_ini            like func_ptoelet.cdn_clas_func
    field cod_clas_fim            like func_ptoelet.cdn_clas_func
    field i-turno-ini             like turno_trab.cdn_turno_trab
    field i-turno-fim             like turno_trab.cdn_turno_trab    
    field v_num_tip_aces_usuar    as integer format "9" 
    field v_cod_grp_usuar         as char
    field v_num_opcao             as int  format "9"
    field v_des_opcao             as char format "x(10)"
    field v_dat_valid             as date format "99/99/9999"
    field v_log_expande_estrut    as log
    field v_num_salta_pg          as integer
    field v_num_quebra            as integer
    field v_num_faixa             as integer
    field destino                 as integer
    field arquivo                 as char
    field usuario                 as char
    field data-exec               as date format "99/99/9999"
    field hora-exec               as integer
    field classifica              as integer
    field desc-classifica         as char format "x(40)"
    field da-dat-ini              as date    format "99/99/9999"
    field da-dat-fim              as date    format "99/99/9999"
    field i-ind-selec             as integer
    field l-impres                as logical
    field v_log_mensal            as logical
    field v_log_horista           as logical
    field v_log_quinzenal         as logical
    field v_log_semanal           as logical
    field v_log_tarefa            as logical
    field v_log_diarista          as logical
    field v_ind_calc              as int
    field v_cdn_prestdor_ini      like prestdor_serv.cdn_prestdor_serv
    field v_cdn_prestdor_fim      like prestdor_serv.cdn_prestdor_serv
    field v_orig_func             as logical label "Funcionario" 
    field v_orig_temp             as logical label "Temporˇrio"
    field v_orig_contratado       as logical label "Contratado"
    field v_orig_cooperado        as logical label "Cooperado"
    field v_orig_socio            as logical label "SΩcio"
    field v_orig_estag            as logical label "Estagiˇrio"
    field v_orig_terc             as logical label "Terceiro Ponto"
    field i-ctr-ini               as int format "99"
    field i-ctr-fim               as int format "99"
    field log-pe3200              as log initial no.

define {1} {2} temp-table {&integr_digita_calc}
    field i-cdn_clas_func   as integer   
    field v_cdn_empres_usuar  like funcionario.cdn_empresa
    field i-es-codigo      like funcionario.cdn_estab
    field i-fc-codigo      LIKE func_ptoelet.cdn_funcionario
    field i-dv-matric      as integer   format "9"
    field c-nome           as char      format "x(40)"
    field i-cs-codigo      as integer   format "99"
    field i-cdn-turno      as integer   format "9999"
    field i-cdn_turma_trab as integer   format ">9"
    field dat_ref_per      as date format "99/99/9999" initial ?
    field da-dat-ini       as date format "99/99/9999"
    index id      is primary 
          v_cdn_empres_usuar
          i-es-codigo 
          i-fc-codigo
    index calculo           
          i-cdn_clas_func 
          v_cdn_empres_usuar
          i-es-codigo 
          i-cs-codigo.

DEFINE TEMP-TABLE tt-digita-func
    FIELD cdn_empresa     LIKE funcionario.cdn_empresa
    FIELD cdn_estab       LIKE funcionario.cdn_estab
    FIELD cdn_funcionario LIKE funcionario.cdn_funcionario.

if p-ind-event = "initialize" and
      p-ind-object = "container"
      then  do:
                                            
      
      Run pi-busca-widget (Input  "da-dat-ini", Input  p-wgh-frame:PARENT,  Output h-da-dat-ini-pe2200).
      Run pi-busca-widget (Input  "da-dat-fim", Input  p-wgh-frame:PARENT,  Output h-da-dat-fim-pe2200).     

      IF VALID-HANDLE(h-da-dat-ini-pe2200) THEN 
           h-da-dat-ini-pe2200:SCREEN-VALUE = STRING(TODAY,"99/99/9999").

      IF VALID-HANDLE(h-da-dat-fim-pe2200) THEN
           h-da-dat-fim-pe2200:SCREEN-VALUE = STRING(TODAY,"99/99/9999"). 
END.

/*---------------------------------------------------------- A T E N C A O ---------------------------------------------------------------------------------------*/
/* daqui para baixo s¢ funciona para polo empresa 420, se precisar tratar o bot∆o executar precisa gerencia por empresa */

IF v_cdn_empres_usuar <> "420" THEN RETURN "ok".

   if p-ind-event = "choose-bt" THEN DO: 
       Run pi-busca-widget (Input  "i-ind-selec", Input  p-wgh-frame:PARENT , Output h-i-ind-selec-pe2200).

      

      IF VALID-HANDLE(h-i-ind-selec-pe2200) AND  h-i-ind-selec-pe2200:SCREEN-VALUE = "2" THEN DO:

           
                  Run pi-busca-widget (Input  "br-digita", Input  h-i-ind-selec-pe2200:frame:PARENT, Output  h-br-digita-pe2200).
          IF  VALID-HANDLE(h-br-digita-pe2200) THEN DO:
              EMPTY TEMP-TABLE tt-digita-func.
                   h-qry = h-br-digita-pe2200:QUERY.
                     
                  IF   h-qry:GET-FIRST THEN DO:
 
                      REPEAT:
                        CREATE tt-digita-func.

                              ASSIGN h-buffer = h-qry:GET-BUFFER-HANDLE[1]
                                            i-ncampos = h-buffer:NUM-FIELDS.

                                    DO i= 1 TO i-ncampos:
                                        ASSIGN h-campo = h-buffer:BUFFER-FIELD[i].
                                         
                                        IF h-campo:NAME = "v_cdn_empres_usuar" THEN DO:
                                           ASSIGN tt-digita-func.cdn_empresa =  h-campo:BUFFER-VALUE.

                                        END.
                                        IF h-campo:NAME = "i-es-codigo" THEN DO:
                                           ASSIGN tt-digita-func.cdn_estab  =  h-campo:BUFFER-VALUE.

                                        END.
                                        IF h-campo:NAME = "i-fc-codigo" THEN DO:
                                           ASSIGN tt-digita-func.cdn_funcionario = int( h-campo:BUFFER-VALUE).
   
                                        END.

                                    END.
                                     IF NOT h-qry:GET-NEXT () THEN LEAVE. 
                      END.
                  END.
          END.

            

      END.

       

       APPLY 'CHOOSE' TO h-bt-executar-pe2200.


       run utp/ut-msgs.p (input "show", input 30068, input "Atená∆o o c†lculo terminou. Deseja rodar verificaá∆o de hora extra no Intervalo de lanches").
       IF RETURN-VALUE = "no" THEN do:
           RUN pi-banco10.
           RETURN "ok".
       END.

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

            
       RUN pi-verifica.
       run pi-finalizar in h-acomp.

       RUN pi-calcula.
       RUN pi-banco10.

       run utp/ut-msgs.p (input "show", input 17006, input "Verificaá∆o terminada!~~Verificaá∆o de hora extra no Intervalo de lanches, terminou com sucesso").


   END.

   
   if p-ind-event = "initialize" and
      p-ind-object = "container"
      then  do:
                                            
      Run pi-busca-widget (Input  "bt-executar", Input  p-wgh-frame, Output h-bt-executar-pe2200).
      Run pi-busca-widget (Input  "da-dat-ini", Input  p-wgh-frame:PARENT,  Output h-da-dat-ini-pe2200).
      Run pi-busca-widget (Input  "da-dat-fim", Input  p-wgh-frame:PARENT,  Output h-da-dat-fim-pe2200).
      Run pi-busca-widget (Input  "i-es-ini", Input  p-wgh-frame:PARENT,  Output h-i-es-ini-pe2200).
      Run pi-busca-widget (Input  "i-es-fim", Input  p-wgh-frame:PARENT,  Output h-i-es-fim-pe2200).
      Run pi-busca-widget (Input  "i-fc-ini", Input  p-wgh-frame:PARENT,  Output h-i-fc-ini-pe2200).
      Run pi-busca-widget (Input  "i-fc-fim", Input  p-wgh-frame:PARENT,  Output h-i-fc-fim-pe2200).

      Run pi-busca-widget (Input  "i-ind-selec", Input  p-wgh-frame, Output h-i-ind-selec-pe2200).
      
      IF NOT VALID-HANDLE(h-bt-executar-pe2200) OR
          NOT VALID-HANDLE(h-da-dat-ini-pe2200) OR
          NOT VALID-HANDLE(h-da-dat-fim-pe2200) OR
          NOT VALID-HANDLE(h-i-es-ini-pe2200) OR
          NOT VALID-HANDLE(h-i-es-fim-pe2200) OR
          NOT VALID-HANDLE(h-i-fc-ini-pe2200) OR
          NOT VALID-HANDLE(h-i-fc-fim-pe2200) THEN DO:
           MESSAGE 
              h-bt-executar-pe2200:LABEL SKIP
              h-da-dat-ini-pe2200:SCREEN-VALUE      SKIP
              h-da-dat-fim-pe2200:SCREEN-VALUE SKIP
              h-i-es-ini-pe2200:SCREEN-VALUE      SKIP 
              h-i-es-fim-pe2200:SCREEN-VALUE SKIP      
              h-i-fc-ini-pe2200:SCREEN-VALUE      SKIP 
              h-i-fc-fim-pe2200:SCREEN-VALUE SKIP      

              VIEW-AS ALERT-BOX INFO BUTTONS OK.

           RETURN "nok".

      END.
          
          
       
       
       
       

      IF VALID-HANDLE (h-bt-executar-pe2200) AND NOT VALID-HANDLE(h-bt-executar-pe2200-esp) THEN DO:
         

          CREATE BUTTON h-bt-executar-pe2200-esp
          ASSIGN FRAME       = h-bt-executar-pe2200:frame
                 WIDTH       = h-bt-executar-pe2200:WIDTH
                 HEIGHT      = h-bt-executar-pe2200:HEIGHT
                 LABEL       = "*Executar"
                 ROW         = h-bt-executar-pe2200:ROW
                 COLUMN      = h-bt-executar-pe2200:COLUMN 
                 TOOLTIP     = h-bt-executar-pe2200:TOOLTIP
                 NAME        = h-bt-executar-pe2200:NAME + '-esp'
                 SENSITIVE   = YES
                 VISIBLE     = YES
              TRIGGERS:
                 ON 'choose':U PERSISTENT RUN prghur/pep/upc/upc-pe2200-u01.p (input  "choose-bt" , 
                                                                               input  p-ind-object,
                                                                               input  p-wgh-object,
                                                                               input  p-wgh-frame ,
                                                                               input  p-cod-table ,
                                                                               input  p-row-table ).



              END TRIGGERS.


      END.
      
   END.

PROCEDURE pi-calcula.
                 
OUTPUT TO VALUE("v:\temp\pe2200-HE-lanche-" + STRING(today,"99-99-9999") + STRING(TIME) + ".txt" ) NO-CONVERT.
                              
    FOR EACH  tt-marc ,
        FIRST  bfunc-ponto WHERE 
                        bfunc-ponto.cdn_empresa     = v_cdn_empres_usuar AND      
                        bfunc-ponto.cdn_estab       = tt-marc.cdn_estab AND                       
                        bfunc-ponto.cdn_funcionario = tt-marc.cdn_funcionario  NO-LOCK,
            FIRST b_func_ptoelet OF bfunc-ponto NO-LOCK.
         
         PUT UNFORMATTED 
                                   tt-marc.cdn_estab      ";"
                                   tt-marc.cdn_funcionario   ";"
                                   bfunc-ponto.nom_pessoa_fisic ";"
                                   tt-marc.dat_proces_mpe       SKIP.
    
            for each tt-param exclusive-lock:
               delete tt-param.
            end.
            
            for each tt-digita exclusive-lock:
               delete tt-digita.
            end.
    
       
            create tt-param.
            assign tt-param.data-exec          = today
                   tt-param.hora-exec          = time
                   tt-param.da-dat-ini         = tt-marc.dat_proces_mpe
                   tt-param.da-dat-fim         = tt-marc.dat_proces_mpe
                   tt-param.i-ind-selec        = 2
                   tt-param.v_cdn_empres_usuar = v_cdn_empres_usuar
                   tt-param.i-es-ini           = tt-marc.cdn_estab
                   tt-param.i-es-fim           = tt-marc.cdn_estab
                   tt-param.i-fc-ini           = tt-marc.cdn_funcionario
                   tt-param.i-fc-fim           = tt-marc.cdn_funcionario
                   tt-param.v_log_mensal       = yes
                   tt-param.v_log_horista      = yes
                   tt-param.v_log_quinzenal    = yes
                   tt-param.v_log_semanal      = yes
                   tt-param.v_log_tarefa       = yes
                   tt-param.v_log_diarista     = yes
                   tt-param.l-impres           = no
                   tt-param.v_des_opcao        = "pe3130"
                   tt-param.LOG_acomp          = NO
                   tt-param.arquivo            = "V:\temp\pe2220.txt".
            
            create tt-digita.
            assign tt-digita.i-cdn_clas_func    = b_func_ptoelet.cdn_clas_func
                   tt-digita.v_cdn_empres_usuar = b_func_ptoelet.cdn_empresa
                   tt-digita.i-es-codigo        = b_func_ptoelet.cdn_estab
                   tt-digita.i-fc-codigo        = b_func_ptoelet.cdn_funcionario
                   tt-digita.i-dv-matric        = b_func_ptoelet.num_digito_verfdor_func
                   tt-digita.c-nome             = b_func_ptoelet.NOM_PESSOA_FISIC
                   tt-digita.i-cs-codigo        = b_func_ptoelet.cdn_categ_sal
                   tt-digita.i-cdn-turno        = b_func_ptoelet.cdn_turno_trab
                   tt-digita.i-cdn_turma_trab   = b_func_ptoelet.cdn_turma_trab
                   tt-digita.da-dat-ini         = tt-marc.dat_proces_mpe.
    
     
        raw-transfer tt-param  to  raw-param.
        for each tt-raw-digita:
           delete tt-raw-digita.
        end.
        for each tt-digita:
           create tt-raw-digita.
           raw-transfer tt-digita to tt-raw-digita.raw-digita.
        end.
     
        run prghur/pep/pe2220rp.p(input raw-param,input table tt-raw-digita).
        for each tt-param exclusive-lock:
               delete tt-param.
            end.
            
            for each tt-digita exclusive-lock:
               delete tt-digita.
            end.
    
       
            create tt-param.
            assign tt-param.data-exec          = today
                   tt-param.hora-exec          = time
                   tt-param.da-dat-ini         = tt-marc.dat_proces_mpe
                   tt-param.da-dat-fim         = tt-marc.dat_proces_mpe
                   tt-param.i-ind-selec        = 2
                   tt-param.v_cdn_empres_usuar = v_cdn_empres_usuar
                   tt-param.i-es-ini           = tt-marc.cdn_estab
                   tt-param.i-es-fim           = tt-marc.cdn_estab
                   tt-param.i-fc-ini           = tt-marc.cdn_funcionario
                   tt-param.i-fc-fim           = tt-marc.cdn_funcionario
                   tt-param.v_log_mensal       = yes
                   tt-param.v_log_horista      = yes
                   tt-param.v_log_quinzenal    = yes
                   tt-param.v_log_semanal      = yes
                   tt-param.v_log_tarefa       = yes
                   tt-param.v_log_diarista     = yes
                   tt-param.l-impres           = no
                   tt-param.v_des_opcao        = "pe3130"
                   tt-param.LOG_acomp          = NO
                   tt-param.arquivo            = "V:\temp\pe2200.txt".
            
            create tt-digita.
            assign tt-digita.i-cdn_clas_func    = b_func_ptoelet.cdn_clas_func
                   tt-digita.v_cdn_empres_usuar = b_func_ptoelet.cdn_empresa
                   tt-digita.i-es-codigo        = b_func_ptoelet.cdn_estab
                   tt-digita.i-fc-codigo        = b_func_ptoelet.cdn_funcionario
                   tt-digita.i-dv-matric        = b_func_ptoelet.num_digito_verfdor_func
                   tt-digita.c-nome             = b_func_ptoelet.NOM_PESSOA_FISIC
                   tt-digita.i-cs-codigo        = b_func_ptoelet.cdn_categ_sal
                   tt-digita.i-cdn-turno        = b_func_ptoelet.cdn_turno_trab
                   tt-digita.i-cdn_turma_trab   = b_func_ptoelet.cdn_turma_trab
                   tt-digita.da-dat-ini         = tt-marc.dat_proces_mpe.
    
     
        raw-transfer tt-param  to  raw-param.
        for each tt-raw-digita:
           delete tt-raw-digita.
        end.
        for each tt-digita:
           create tt-raw-digita.
           raw-transfer tt-digita to tt-raw-digita.raw-digita.
        end.

        run prghur/pep/pe2200rp.p(input raw-param,input table tt-raw-digita).
    END.

    OUTPUT CLOSE.
end procedure.




 Procedure pi-busca-widget:
        Def Input  Param p-nome  As Char.
        Def Input  Param p-frame        As Widget-handle.
        Def Output Param p-object   As Widget-handle.

        Def Var h-frame                  As Widget-handle.
        Def Var wh-objeto                  As Widget-handle.

        Assign h-frame = p-frame:First-child.

        Do While Valid-handle(h-frame):
                If h-frame:Type <> "field-group" Then Do:
                        If h-frame:Type = "frame" Then Do:

                                Run pi-busca-widget(Input  p-nome,
                                                                        Input  h-frame,
                                                                        Output wh-objeto).

                                If wh-objeto <> ? Then Do:
                                        Assign p-object = wh-objeto.
                                        Leave.
                                End.
                        End.
 
                          
                        If h-frame:Name = p-nome Then Do:
                                Assign p-object = h-frame.
                                Leave.
                        End.

                        Assign h-frame = h-frame:Next-sibling.

                End.
                Else
                        Assign h-frame = h-frame:First-child.
        End.
End Procedure.



PROCEDURE pi-verifica.
         
     ASSIGN
          dt-ini-mpe = date(h-da-dat-ini-pe2200:SCREEN-VALUE)
          dt-fim-mpe = date(h-da-dat-fim-pe2200:SCREEN-VALUE).

         

    EMPTY temp-table  tt-marc.
     IF VALID-HANDLE(h-i-ind-selec-pe2200) AND  h-i-ind-selec-pe2200:SCREEN-VALUE = "2" THEN DO:
 
        for EACH  tt-digita-func NO-LOCK.
             
            ASSIGN i-es-ini = tt-digita-func.cdn_estab
                   i-es-fim = tt-digita-func.cdn_estab 
                   i-fc-ini = tt-digita-func.cdn_funcionario
                   i-fc-fim = tt-digita-func.cdn_funcionario.
    
            RUN pi-verifica-func.
        END.

    END.
    ELSE DO:
        ASSIGN 
              i-fc-ini   = INT(h-i-fc-ini-pe2200:SCREEN-VALUE )  
              i-fc-fim   = INT(h-i-fc-fim-pe2200:SCREEN-VALUE)
              i-es-ini   = h-i-es-ini-pe2200:SCREEN-VALUE
              i-es-fim   =  h-i-es-fim-pe2200:SCREEN-VALUE.
         RUN pi-verifica-func.
    END.
END PROCEDURE.

PROCEDURE  pi-verifica-func.
                      
    FOR EACH  bfunc-ponto WHERE 
                    bfunc-ponto.cdn_empresa     =  v_cdn_empres_usuar AND      
                    bfunc-ponto.cdn_estab       >= i-es-ini AND      
                    bfunc-ponto.cdn_estab       <= i-es-fim AND      
                    bfunc-ponto.cdn_funcionario >= i-fc-ini  and
                    bfunc-ponto.cdn_funcionario <= i-fc-fim 
          NO-LOCK,
      FIRST func_ptoelet OF bfunc-ponto NO-LOCK.
      DO dt-proces-mpe = dt-ini-mpe TO dt-fim-mpe:
         FOR  each par_marcac_ptoelet exclusive-lock where  /** Leitura das batidas importadas ou digitadas **/
             par_marcac_ptoelet.cdn_empresa      = bfunc-ponto.cdn_empresa and
             par_marcac_ptoelet.cdn_estab        = bfunc-ponto.cdn_estab and
             par_marcac_ptoelet.cdn_funcionario  = bfunc-ponto.cdn_funcionario and
             par_marcac_ptoelet.dat_proces_mpe   = dt-proces-mpe  AND
             par_marcac_ptoelet.idi_tip_ocor_mpe = 3 
                use-index prmrccpt_id.

            run pi-acompanhar in h-acomp(input "Verificando Func: " + string(bfunc-ponto.cdn_funcionario) + "-" + STRING(dt-proces-mpe,"99/99/9999")).

             ASSIGN i-cdn_turno      = 0
                    i-cdn_turma_trab = 0.
                FOR LAST func_turno_trab WHERE 
                  func_turno_trab.cdn_funcionario = bfunc-ponto.cdn_funcionario AND
                  func_turno_trab.cdn_empresa     = bfunc-ponto.cdn_empresa     AND
                  func_turno_trab.cdn_estab       = bfunc-ponto.cdn_estab       and
                  func_turno_trab.dat_inic_lotac_func_turno_trab <= dt-proces-mpe 
                  NO-LOCK.
                        ASSIGN i-cdn_turno      = func_turno_trab.cdn_turno_trab 
                               i-cdn_turma_trab = func_turno_trab.cdn_turma_trab.
                
                END.
                
                IF i-cdn_turno = 0  THEN DO:
                   ASSIGN i-cdn_turno      = func_ptoelet.cdn_turno 
                          i-cdn_turma_trab = func_ptoelet.cdn_turma .                              
                END.
 
     
           ASSIGN v_cod_tip_dia = " ".
    
           IF CAN-FIND(det_calend_func NO-LOCK WHERE
                       det_calend_func.cdn_empresa      = func_ptoelet.cdn_empresa AND
                       det_calend_func.cdn_estab        = func_ptoelet.cdn_estab   AND
                       det_calend_func.cdn_funcionario  = func_ptoelet.cdn_funcionario AND
                       det_calend_func.cdn_turno_trab   = i-cdn_turno        and
                       det_calend_func.cdn_turma_trab   = i-cdn_turma_trab   and
                       det_calend_func.cod_pais         = func_ptoelet.cod_pais    and
                       det_calend_func.cdn_localidade   = func_ptoelet.cdn_localid and
                       det_calend_func.dat_refer_calend = par_marcac_ptoelet.dat_proces_mpe) THEN DO:
              FIND det_calend_func NO-LOCK WHERE
                   det_calend_func.cdn_empresa      = func_ptoelet.cdn_empresa AND
                   det_calend_func.cdn_estab        = func_ptoelet.cdn_estab   AND
                   det_calend_func.cdn_funcionario  = func_ptoelet.cdn_funcionario AND
                   det_calend_func.cdn_turno_trab   = i-cdn_turno        and
                   det_calend_func.cdn_turma_trab   = i-cdn_turma_trab   and
                   det_calend_func.cod_pais         = func_ptoelet.cod_pais    and
                   det_calend_func.cdn_localidade   = func_ptoelet.cdn_localid and
                   det_calend_func.dat_refer_calend = par_marcac_ptoelet.dat_proces_mpe NO-ERROR.
              ASSIGN v_cod_tip_dia = det_calend_func.cod_tip_dia.
           END.
           ELSE DO: 
               find det_calend_turma_localid no-lock where
                    det_calend_turma_localid.cdn_turno_trab   = i-cdn_turno        and
                    det_calend_turma_localid.cdn_turma_trab   = i-cdn_turma_trab   and
                    det_calend_turma_localid.cod_pais         = func_ptoelet.cod_pais    and
                    det_calend_turma_localid.cdn_localidade   = func_ptoelet.cdn_localid and
                    det_calend_turma_localid.dat_refer_calend = par_marcac_ptoelet.dat_proces_mpe no-error.
               IF AVAIL det_calend_turma_localid THEN
                  ASSIGN v_cod_tip_dia = det_calend_turma_localid.cod_tip_dia.
               ELSE 
                  ASSIGN v_cod_tip_dia = " ".
           END.
    
           IF not(v_cod_tip_dia = "co" OR v_cod_tip_dia = "fe" OR v_cod_tip_dia = "re") THEN NEXT.
    
                             
           FIND  FIRST  marcac_ptoelet WHERE 
                marcac_ptoelet.cdn_empresa      = bfunc-ponto.cdn_empresa and
                marcac_ptoelet.cdn_estab        = bfunc-ponto.cdn_estab and
                marcac_ptoelet.cdn_funcionario  = bfunc-ponto.cdn_funcionario and
                marcac_ptoelet.dat_proces_mpe   = dt-proces-mpe AND
                marcac_ptoelet.num_horar_proces_mpe =  par_marcac_ptoelet.num_horar_fim_proces_mpe  AND
                marcac_ptoelet.idi_marcac_ptoelet_entr_saida  = 2 EXCLUSIVE-LOCK NO-ERROR.
    
           IF   AVAIL marcac_ptoelet THEN DO :
    
    
                FIND  FIRST  b-marcac_ptoelet WHERE ROWID(b-marcac_ptoelet) = ROWID(marcac_ptoelet) NO-LOCK NO-ERROR.
    
                FIND  NEXT  marcac_ptoelet WHERE 
                    marcac_ptoelet.cdn_empresa      = bfunc-ponto.cdn_empresa and
                    marcac_ptoelet.cdn_estab        = bfunc-ponto.cdn_estab and
                    marcac_ptoelet.cdn_funcionario  = bfunc-ponto.cdn_funcionario and
                    marcac_ptoelet.dat_proces_mpe   = dt-proces-mpe   EXCLUSIVE-LOCK NO-ERROR.
    
           END.
    
           IF NOT  AVAIL b-marcac_ptoelet THEN NEXT.
           IF NOT  AVAIL marcac_ptoelet THEN NEXT.
    
             
     
           IF b-marcac_ptoelet.idi_status_marcac = 2 THEN NEXT.                            
           IF marcac_ptoelet.idi_status_marcac = 2 THEN NEXT.                            
    
    
    
           assign l-lanche = no
                        .
    
           i-interv = int(marcac_ptoelet.num_horar_proces_mpe + (IF marcac_ptoelet.num_horar_proces_mpe <  b-marcac_ptoelet.num_horar_proces_mpe THEN  86400 ELSE 0)  -  b-marcac_ptoelet.num_horar_proces_mpe) / 60   .
  
           for each esp_lanche_turno_jorn  NO-LOCK where                      
                      esp_lanche_turno_jorn.cdn_empresa  = par_marcac_ptoelet.cdn_empresa AND
                      esp_lanche_turno_jorn.cdn_estab    = par_marcac_ptoelet.cdn_estab AND  
                     /* esp_lanche_turno_jorn.cdn_jorn_trab = par_marcac_ptoelet.num_livre_1 AND                                   */
                      esp_lanche_turno_jorn.cdn_turno_trab = i-cdn_turno     .
                                                             
    
    
                     if b-marcac_ptoelet.num_horar_proces_mpe >=  esp_lanche_turno_jorn.seg_interv_ini_1 and
                        b-marcac_ptoelet.num_horar_proces_mpe <= esp_lanche_turno_jorn.seg_interv_fim_1 and
                         i-interv <= esp_lanche_turno_jorn.duracao_lanche_1 + 
                         esp_lanche_turno_jorn.tolera_entrada_1 + esp_lanche_turno_jorn.tolera_saida_1  then do:
    
                         l-lanche = yes.
    
                         leave.
                     end.
    
                     if b-marcac_ptoelet.num_horar_proces_mpe + 86400 >=  esp_lanche_turno_jorn.seg_interv_ini_1 and
                      b-marcac_ptoelet.num_horar_proces_mpe + 86400 <=esp_lanche_turno_jorn.seg_interv_fim_1 and
                      i-interv <= esp_lanche_turno_jorn.duracao_lanche_1 + esp_lanche_turno_jorn.tolera_entrada_1 + esp_lanche_turno_jorn.tolera_saida_1  then do:
    
                       l-lanche = yes.
    
                       leave.
                     end.

                      if b-marcac_ptoelet.num_horar_proces_mpe - 86400 >=  esp_lanche_turno_jorn.seg_interv_ini_1 and
                      b-marcac_ptoelet.num_horar_proces_mpe - 86400 <=esp_lanche_turno_jorn.seg_interv_fim_1 and
                      i-interv <= esp_lanche_turno_jorn.duracao_lanche_1 + esp_lanche_turno_jorn.tolera_entrada_1 + esp_lanche_turno_jorn.tolera_saida_1  then do:
    
                       l-lanche = yes.
    
                       leave.
                     end.
    
    
                     if b-marcac_ptoelet.num_horar_proces_mpe >=  (if  esp_lanche_turno_jorn.seg_interv_ini_1 - esp_lanche_turno_jorn.tolera_entrada_1  * 60 > 0 then 
                                 esp_lanche_turno_jorn.seg_interv_ini_1 - esp_lanche_turno_jorn.tolera_entrada_1  * 60 else 86400 - ( esp_lanche_turno_jorn.seg_interv_ini_1 - esp_lanche_turno_jorn.tolera_entrada_1  * 60 )) and
                        i-interv <= esp_lanche_turno_jorn.duracao_lanche_1 + esp_lanche_turno_jorn.tolera_entrada_1 + esp_lanche_turno_jorn.tolera_saida_1 and
                        b-marcac_ptoelet.num_horar_proces_mpe <= esp_lanche_turno_jorn.seg_interv_fim_1 then do:
    
                         l-lanche = yes.
                         leave.
                     end.
    
    
                        IF esp_lanche_turno_jorn.seg_interv_ini_1 <> esp_lanche_turno_jorn.seg_interv_ini_2 THEN DO:
                                                          if b-marcac_ptoelet.num_horar_proces_mpe >=  esp_lanche_turno_jorn.seg_interv_ini_2 and
                                     b-marcac_ptoelet.num_horar_proces_mpe <= esp_lanche_turno_jorn.seg_interv_fim_2 and
                                     i-interv  <= esp_lanche_turno_jorn.duracao_lanche_2 + esp_lanche_turno_jorn.tolera_entrada_2 + esp_lanche_turno_jorn.tolera_saida_2  then do:
    
                                      l-lanche = yes.
    
                                      leave.
                                  end.
    
                                  if b-marcac_ptoelet.num_horar_proces_mpe + 86400 >=  esp_lanche_turno_jorn.seg_interv_ini_2 and
                                   b-marcac_ptoelet.num_horar_proces_mpe + 86400 <=esp_lanche_turno_jorn.seg_interv_fim_2 and
                                   i-interv <= esp_lanche_turno_jorn.duracao_lanche_2 + esp_lanche_turno_jorn.tolera_entrada_2 + esp_lanche_turno_jorn.tolera_saida_2  then do:
    
                                    l-lanche = yes.
    
                                    leave.
                                  end.

                                  if b-marcac_ptoelet.num_horar_proces_mpe - 86400 >=  esp_lanche_turno_jorn.seg_interv_ini_2 and
                                   b-marcac_ptoelet.num_horar_proces_mpe - 86400 <=esp_lanche_turno_jorn.seg_interv_fim_2 and
                                   i-interv <= esp_lanche_turno_jorn.duracao_lanche_2 + esp_lanche_turno_jorn.tolera_entrada_2 + esp_lanche_turno_jorn.tolera_saida_2  then do:
    
                                    l-lanche = yes.
    
                                    leave.
                                  end.
    
    
                                  if b-marcac_ptoelet.num_horar_proces_mpe >=  (if  esp_lanche_turno_jorn.seg_interv_ini_2 - esp_lanche_turno_jorn.tolera_entrada_2  * 60 > 0 then 
                                              esp_lanche_turno_jorn.seg_interv_ini_2 - esp_lanche_turno_jorn.tolera_entrada_2  * 60 else 86400 - ( esp_lanche_turno_jorn.seg_interv_ini_2 - esp_lanche_turno_jorn.tolera_entrada_2  * 60 )) and
                                     i-interv <= esp_lanche_turno_jorn.duracao_lanche_2 + esp_lanche_turno_jorn.tolera_entrada_2 + esp_lanche_turno_jorn.tolera_saida_2 and
                                     b-marcac_ptoelet.num_horar_proces_mpe <= esp_lanche_turno_jorn.seg_interv_fim_2 then do:
    
                                      l-lanche = yes.
                                      leave.
                                  end.
    
                        END.
           END.
    
    
           IF l-lanche THEN  DO:
                           FIND FIRST  tt-marc WHERE  tt-marc.cdn_estab       = bfunc-ponto.cdn_estab AND
                                                      tt-marc.cdn_funcionario = bfunc-ponto.cdn_funcionario AND
                                                      tt-marc.dat_proces_mpe  = dt-proces-mpe NO-ERROR.
                           IF NOT AVAIL tt-marc THEN DO:
                               CREATE tt-marc.
                               ASSIGN  tt-marc.cdn_estab       = bfunc-ponto.cdn_estab  
                                       tt-marc.cdn_funcionario = bfunc-ponto.cdn_funcionario  
                                       tt-marc.dat_proces_mpe  = dt-proces-mpe.
                           END.
    
                               ASSIGN           /*muda para desconsidera*/
                               b-marcac_ptoelet.idi_status_marcac = 2
                               b-marcac_ptoelet.cdn_motiv_marcac  = 8
                               marcac_ptoelet.idi_status_marcac   = 2
                               marcac_ptoelet.cdn_motiv_marcac    = 8.




                                                                                                                     
           END.
         END.
      END.
    END.
    
    

END PROCEDURE.

PROCEDURE pi-banco10.
   ASSIGN
          dt-ini-mpe = date(h-da-dat-ini-pe2200:SCREEN-VALUE) - 1
          dt-fim-mpe = date(h-da-dat-fim-pe2200:SCREEN-VALUE) + 1.

   DO  dt-atu = dt-ini-mpe - 20  TO dt-fim-mpe.
        FIND FIRST tt-dia WHERE 
            tt-dia.dt-dia = dt-atu NO-ERROR.
        IF AVAIL tt-dia THEN NEXT.
   
        RUN verifica-ano-mes-semana (INPUT dt-atu , OUTPUT c-data, OUTPUT c-mes).            
    
        CREATE tt-dia.
        ASSIGN 
            tt-dia.dt-dia = dt-atu
            tt-dia.dt-sem = c-data.
    END.
    
    DO  dt-atu = dt-ini-mpe  TO dt-ini-mpe - 10 BY -1.
       FIND FIRST tt-dia WHERE     
           tt-dia.dt-dia = dt-atu
           NO-LOCK NO-ERROR.
       IF NOT AVAIL tt-dia THEN NEXT.
       IF WEEKDAY(dt-atu - 1) = 1 THEN LEAVE.
    END.

                                   
     IF VALID-HANDLE(h-i-ind-selec-pe2200) AND  h-i-ind-selec-pe2200:SCREEN-VALUE = "2" THEN DO:
 
        for EACH  tt-digita-func NO-LOCK.
             
            ASSIGN i-es-ini = tt-digita-func.cdn_estab
                   i-es-fim = tt-digita-func.cdn_estab 
                   i-fc-ini = tt-digita-func.cdn_funcionario
                   i-fc-fim = tt-digita-func.cdn_funcionario.
    
            RUN pi-verifica-func-bh10.
        END.

    END.
    ELSE DO:
        ASSIGN 
              i-fc-ini   = INT(h-i-fc-ini-pe2200:SCREEN-VALUE )  
              i-fc-fim   = INT(h-i-fc-fim-pe2200:SCREEN-VALUE)
              i-es-ini   = h-i-es-ini-pe2200:SCREEN-VALUE
              i-es-fim   =  h-i-es-fim-pe2200:SCREEN-VALUE.
         RUN pi-verifica-func-bh10.
    END.

END.

PROCEDURE pi-verifica-func-bh10.
         
    FOR EACH rh_estab WHERE             
            rh_estab.cdn_empresa = v_cdn_empres_usuar AND
            rh_estab.cdn_estab   >= i-es-ini  AND
            rh_estab.cdn_estab   <= i-es-fim  and
            (rh_estab.cdn_estab = "422" OR rh_estab.cdn_estab = "412")   NO-LOCK,
           EACH funcionario WHERE      
                 funcionario.cdn_empresa       = rh_estab.cdn_empresa   AND
                 funcionario.cdn_estab         = rh_estab.cdn_estab     AND           
                 funcionario.cdn_funcionario  >= i-fc-ini   AND
                 funcionario.cdn_funcionario  <= i-fc-fim   AND
                 funcionario.dat_desligto_func = ?         NO-LOCK,
        FIRST func_ptoelet OF funcionario   NO-LOCK.       
    
        FOR EACH bco_hrs_compens_func WHERE      
                bco_hrs_compens_func.cdn_empresa             = rh_estab.cdn_empresa   AND
                bco_hrs_compens_func.cdn_estab               = rh_estab.cdn_estab     AND           
                bco_hrs_compens_func.cdn_funcionario         = funcionario.cdn_funcionario  AND         
                bco_hrs_compens_func.dat_atualiz_bco_hora   >= dt-atu               AND
                bco_hrs_compens_func.dat_atualiz_bco_hora   <= dt-fim-mpe       AND
                bco_hrs_compens_func.cdn_tip_compcao_hrs       = 1                  AND   
                bco_hrs_compens_func.idi_tratam_lancto_bco_hrs = 1  and
                ((bco_hrs_compens_func.idi_hrs_posit = 1) OR
                 (bco_hrs_compens_func.idi_hrs_posit = 3) OR
                 (bco_hrs_compens_func.idi_hrs_posit = 5))  NO-LOCK,     
            FIRST  tt-dia WHERE
                         tt-dia.dt-dia = (IF bco_hrs_compens_func.num_horar_term_mpe > 86400 THEN
                         bco_hrs_compens_func.dat_atualiz_bco_hora + 1 ELSE 
                         bco_hrs_compens_func.dat_atualiz_bco_hora) NO-LOCK
                   break
                        by bco_hrs_compens_func.cdn_empresa    
                        by bco_hrs_compens_func.cdn_estab      
                        by bco_hrs_compens_func.cdn_funcionario
                        BY tt-dia.dt-sem    /* quebra semanal*/
                        by bco_hrs_compens_func.dat_atualiz_bco_hora
                        by bco_hrs_compens_func.num_horar_inic_mpe . 
                    
        
                    IF FIRST-OF(tt-dia.dt-sem) THEN
                        ASSIGN d-acum     = 0
                               c-desc     = ""
                               d-acum-ex  = 0                       
                               d-acum-bh  = 0                     
                               l-estouro  = NO.       
        
                    ASSIGN 
                         d-acum = d-acum + bco_hrs_compens_func.qti_hrs_marcac_ptoelet
                         l-parcial  = NO.
        
                    IF d-acum > 36000 THEN DO:    /* 36000 = se passou de 10 horas semanais*/
                         l-estouro = YES.
                         IF bco_hrs_compens_func.idi_hrs_posit    =  1 THEN DO:
                             IF d-acum-bh > 0 THEN
                                  d-acum-ex = bco_hrs_compens_func.qti_hrs_marcac_ptoelet.
                             ELSE DO:
                                 d-acum-ex = (d-acum - 36000).
        
                                 l-parcial = YES.
                             END.
                             d-acum-bh = d-acum-bh + d-acum-ex.
                         END.
                    END.
        
        
                    c-desc = "ignorar".
                    IF  l-estouro AND NOT l-parcial AND bco_hrs_compens_func.idi_hrs_posit    =  1  THEN
                        c-desc = "Mover".
                    ELSE
                    IF l-estouro AND  l-parcial AND bco_hrs_compens_func.idi_hrs_posit    =  1  THEN DO:
                        c-desc = "Partir".              
        
                    END.                         
        
                    d-fator = 0.
                    FOR FIRST efp_hora_extra_tip_dia_sind WHERE 
                        efp_hora_extra_tip_dia_sind.cdn_empresa     =  funcionario.cdn_empresa AND
                        efp_hora_extra_tip_dia_sind.cdn_sindicato   =  funcionario.cdn_sindicato  AND
                        efp_hora_extra_tip_dia_sind.cod_tip_dia     = substr(bco_hrs_compens_func.cod_livre_1,1,2)AND
                        efp_hora_extra_tip_dia_sind.idi_tratam_hora = 2   NO-LOCK.
        
                       d-fator =  efp_hora_extra_tip_dia_sind.val_perc_hora_extra_diurno / 100 + 1 .
                    END.                          
        
                    IF c-desc <> "ignorar"  THEN RUN pi-trata-banco.                    
        END.
    END.

    /*CRIA O BANCO DE HORAS TIPO 6 EFETIVAMENTE*/
    RUN pi-atualiza-banco.
END.

procedure verifica-ano-mes-semana:

    DEF INPUT  PARAMETER p-dia      AS DATE NO-UNDO.
    def output parameter nm-dir-ret as char format "x(08)".
    def output parameter dt-mes     as char format "x(08)".
    /*-------- DEF VARS AUXILIARES ---*/
    def var dt-ano                  as char format "x(06)"   no-undo.
    def var dt-data-aux             as date 	             no-undo.
    def var nr-semana-ano-aux       as int format "99"       no-undo.
    
    assign dt-ano = string(year(p-dia)) 
           dt-mes = string(month(p-dia),"99").
    
    /*Calcula o dia da semana dentro do ano*/
    assign dt-data-aux     = date(01,01,year(p-dia)) 
         dt-data-aux       = dt-data-aux - WEEKDAY(dt-data-aux - 1) + 1 .
         nr-semana-ano-aux = interval(p-dia,dt-data-aux,"weeks") + 1.        
    assign nm-dir-ret      = string(dt-ano) + string(nr-semana-ano-aux,"99").
    return.
end procedure.

PROCEDURE pi-trata-banco.
      
    DEFINE VARIABLE d-baixar    AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE d-valor    AS DECIMAL     NO-UNDO.

    IF c-desc = "mover" THEN DO:  /*move o registro inteiro do banco 1 para o banco 6*/
        CREATE tt_bco_hrs_compens_func.
        BUFFER-COPY bco_hrs_compens_func EXCEPT cdn_tip_compcao_hrs TO tt_bco_hrs_compens_func
            ASSIGN tt_bco_hrs_compens_func.cdn_tip_compcao_hrs = 6.
                   tt_bco_hrs_compens_func.cdn_tip_compcao_hrs_ant = 1.
    END.
    ELSE DO:
        CREATE tt_bco_hrs_compens_func.    /*cria um registro no banco 6 com uma parte do movimento*/
        BUFFER-COPY bco_hrs_compens_func EXCEPT cdn_tip_compcao_hrs TO tt_bco_hrs_compens_func
            ASSIGN tt_bco_hrs_compens_func.cdn_tip_compcao_hrs = 6
                   tt_bco_hrs_compens_func.cdn_tip_compcao_hrs_ant = 1.
    
        ASSIGN tt_bco_hrs_compens_func.qti_hrs_marcac_ptoelet = d-acum-ex
               tt_bco_hrs_compens_func.num_horar_inic_mpe = bco_hrs_compens_func.num_horar_term_mpe - d-acum-ex.
    
        IF bco_hrs_compens_func.qti_hrs_marcac_ptoelet > bco_hrs_compens_func.qti_hrs_marcac_ptoelet_orig  THEN DO:
            ASSIGN 
                d-valor = d-acum-ex / (bco_hrs_compens_func.qti_hrs_marcac_ptoelet / bco_hrs_compens_func.qti_hrs_marcac_ptoelet_orig)
                tt_bco_hrs_compens_func.num_horar_inic_mpe = bco_hrs_compens_func.num_horar_term_mpe - TRUNCATE(d-valor,0)
                tt_bco_hrs_compens_func.qti_hrs_marcac_ptoelet_orig = tt_bco_hrs_compens_func.num_horar_term_mpe - tt_bco_hrs_compens_func.num_horar_inic_mpe.
        END.
        ELSE
            ASSIGN tt_bco_hrs_compens_func.qti_hrs_marcac_ptoelet_orig = tt_bco_hrs_compens_func.qti_hrs_marcac_ptoelet.
    
        ASSIGN 
            d-valor  = tt_bco_hrs_compens_func.num_horar_inic_mpe
            d-baixar = bco_hrs_compens_func.qti_hrs_marcac_ptoelet - tt_bco_hrs_compens_func.qti_hrs_marcac_ptoelet.
    
    
      CREATE tt_bco_hrs_compens_func.  /*altera o banco 1 com o saldo restando do que foi movido para o banco 6*/
      BUFFER-COPY bco_hrs_compens_func EXCEPT num_horar_term_mpe   qti_hrs_marcac_ptoelet qti_hrs_marcac_ptoelet_orig  TO tt_bco_hrs_compens_func
         ASSIGN  
                tt_bco_hrs_compens_func.num_horar_term_mpe   = d-valor                              
                tt_bco_hrs_compens_func.cdn_tip_compcao_hrs_ant = 1
                tt_bco_hrs_compens_func.qti_hrs_marcac_ptoelet = d-baixar
                tt_bco_hrs_compens_func.qti_hrs_marcac_ptoelet_orig = tt_bco_hrs_compens_func.num_horar_term_mpe - tt_bco_hrs_compens_func.num_horar_inic_mp.
    END.
                                                           
END PROCEDURE.
         
PROCEDURE pi-atualiza-banco.
    DEFINE VARIABLE l-erro AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE l-fez AS LOGICAL . /* deixar sem no-undo para saber se a rotina rodou inteira*/
    l-erro = NO.
    l-fez = NO.
    bloco_atualiza:
    DO TRANS: 
    
        l-fez = YES.
        FOR EACH tt_bco_hrs_compens_func ON ERROR UNDO bloco_atualiza:  .                            

             FIND FIRST bco_hrs_compens_func WHERE
                             bco_hrs_compens_func.cdn_empresa          =  tt_bco_hrs_compens_func.cdn_empresa           and 
                             bco_hrs_compens_func.cdn_estab            =  tt_bco_hrs_compens_func.cdn_estab             and
                             bco_hrs_compens_func.cdn_funcionario      =  tt_bco_hrs_compens_func.cdn_funcionario       and
                             bco_hrs_compens_func.cdn_tip_compcao_hrs  =  tt_bco_hrs_compens_func.cdn_tip_compcao_hrs  and
                             bco_hrs_compens_func.dat_atualiz_bco_hora =  tt_bco_hrs_compens_func.dat_atualiz_bco_hora  and
                             bco_hrs_compens_func.num_horar_inic_mpe   =  tt_bco_hrs_compens_func.num_horar_inic_mpe  NO-ERROR.

             IF AVAIL bco_hrs_compens_func AND bco_hrs_compens_func.cdn_tip_compcao_hrs = 6  THEN DO:
                 l-erro = YES.
                 UNDO bloco_atualiza, LEAVE bloco_atualiza.
             END.
            
             FIND FIRST bco_hrs_compens_func WHERE
                             bco_hrs_compens_func.cdn_empresa          =  tt_bco_hrs_compens_func.cdn_empresa           and 
                             bco_hrs_compens_func.cdn_estab            =  tt_bco_hrs_compens_func.cdn_estab             and
                             bco_hrs_compens_func.cdn_funcionario      =  tt_bco_hrs_compens_func.cdn_funcionario       and
                             bco_hrs_compens_func.cdn_tip_compcao_hrs  =  tt_bco_hrs_compens_func.cdn_tip_compcao_hrs_ant  and
                             bco_hrs_compens_func.dat_atualiz_bco_hora =  tt_bco_hrs_compens_func.dat_atualiz_bco_hora  and
                             bco_hrs_compens_func.num_horar_inic_mpe   =  tt_bco_hrs_compens_func.num_horar_inic_mpe  NO-ERROR.

             IF NOT AVAIL bco_hrs_compens_func THEN DO:
                 CREATE bco_hrs_compens_func.
             END.                    
             BUFFER-COPY tt_bco_hrs_compens_func TO bco_hrs_compens_func.                                                                                 
        END.
    END.

     
    IF l-fez = NO THEN   /*se ocorrer erro o conteudo desta vari†vel Ç desfeito porque foi declarada sem no-undo*/
        RETURN "nok".

END PROCEDURE.
