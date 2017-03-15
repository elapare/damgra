/********************************************************************************
* Programa: esat0007rp.p
* Data....: jan/2014
* Autor...: Edson
* Alteracao :  
* Objetivo: Por  rea (n§ de treinandos*n§ hora treinamento/ n§ total de funcion rios da  rea)
*         
* VersÆo..: 2.00.000                            

*******************************************************************************/
/*---------------- Include de controle de VersÆo ------------------*/ 
{bf/buffersHCM.i}
{include/i-prgvrs.i esat0007RP 2.00.00.000}


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

/*def new global shared var rw-graf            as  com-handle              no-undo.
  */

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
    field cod_emp          as CHAR
    FIELD cdn_estab_ini    AS char
    FIELD cdn_estab_fim    AS char
    field c_ccusto_ini     as CHAR
    field c_ccusto_fim     as CHAR    
    field dt_periodo_ini as date
    field dt_periodo_fim as date
    field tg-pendente      as LOGICAL           
    FIELD tg-confirmado    as LOGICAL
    FIELD tg-cursando      as LOGICAL       
    field tg-habilitado    as LOGICAL       
    field tg-desabilitado  as LOGICAL       
    field tg-cancelado     AS LOGICAL
     FIELD rs-head-count AS INTEGER
        . 
    
DEFINE TEMP-TABLE tt-treinando
    FIELD cdn_empresa LIKE funcionario.cdn_empresa
    FIELD cdn_estab   LIKE funcionario.cdn_estab
    FIELD cdn_treindo LIKE treindo_turma_trein.cdn_treindo 
    FIELD cdn_tip_curso_trein LIKE curso_trein.cdn_tip_curso_trein
    FIELD des_tip_curso_trein LIKE  tip_curso_trein.des_tip_curso_trein
    FIELD cdn_curso_trein LIKE turma_trein.cdn_curso_trein 
    FIELD cdn_turma_trein  LIKE turma_trein.cdn_turma_trein     
    FIELD nom_pessoa_fisic LIKE treindo_turma_trein.nom_pessoa_fisic
    FIELD des_curso_trein  LIKE curso_trein.des_curso_trein
    FIELD dat_inic_turma_trein LIKE turma_trein.dat_inic_turma_trein
    FIELD dat_fim_turma_trein LIKE turma_trein.dat_fim_turma_trein 
    FIELD idi_sit_trein LIKE treindo_turma_trein.idi_sit_trein
    FIELD cod_rh_ccusto LIKE funcionario.cod_rh_ccusto 
    FIELD des_rh_ccusto LIKE rh_ccusto.des_rh_ccusto
    FIELD qtd_hora_trein AS DECIMAL.


 
/* recebimento de parƒmetros */
def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.
 

/****************** Defini‡ao de Vari veis de Processamento do Relat¢rio *********************/
def var h-acomp              as handle no-undo.
Def Var dat-trab As Date.
DEFINE VARIABLE v_qtd_hora_trein  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE i-head-count AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-mes AS INTEGER     NO-UNDO.

DEFINE VARIABLE i-ano AS INTEGER     NO-UNDO.

/****************** Defini‡ao de  Frames e Forms do Relat¢rio 132 Colunas ***************************************/ 

/****************** INCLUDE COM VARIµVEIS GLOBAIS *********************/
 


DEFINE TEMP-TABLE tt-cursos
    FIELD cdn_empresa         LIKE funcionario.cdn_empresa
    FIELD cod_rh_ccusto       LIKE funcionario.cod_rh_ccusto
    FIELD cdn_tip_curso_trein LIKE curso_trein.cdn_tip_curso_trein
    FIELD des_tip_curso_trein LIKE  tip_curso_trein.des_tip_curso_trein
    FIELD des_ccusto         AS CHAR
    FIELD qtde AS INT   
    FIELD horas AS DEC
    FIELD qt-func AS INT
    FIELD perc AS DEC
    FIELD coluna AS DEC
    
    INDEX cc IS PRIMARY UNIQUE
    cdn_empresa
    cod_rh_ccusto       
    cdn_tip_curso_trein 
    INDEX tipo
    cdn_empresa
    cdn_tip_curso_trein
    cod_rh_ccusto.

 

DEFINE VARIABLE i-qtde     AS DEC     NO-UNDO.
DEFINE VARIABLE i-curso     AS DEC     NO-UNDO.
DEFINE VARIABLE i-curso-cc AS DEC     NO-UNDO.
DEFINE VARIABLE i-curso-tt AS DEC    NO-UNDO.
DEFINE VARIABLE c-empresa AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dt_periodo_ini AS DATE        NO-UNDO.
DEFINE VARIABLE dt_periodo_fim AS DATE        NO-UNDO.
DEFINE VARIABLE c-est-ini AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-est-fim AS CHARACTER   NO-UNDO.

def var c-modelo-planilha  as char format "x(50)"         no-undo.
def var c-excel            as com-handle                  NO-UNDO.
def var c-planilha         as com-handle.
def var c-relatorio        as com-handle.
def var c-grafico          as com-handle.
DEFINE VARIABLE c-arq AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-arq-anexo AS CHARACTER   NO-UNDO.
DEF VAR c-arquivo AS CHAR NO-UNDO.

DEFINE VARIABLE i-linha AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-linha-ini AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-linha-graf AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-graf AS INTEGER     NO-UNDO.
ASSIGN 
    c-empresa = tt-param.cod_emp
    dt_periodo_ini  = tt-param.dt_periodo_ini  
    dt_periodo_fim  = tt-param.dt_periodo_fim  
    c-est-ini = tt-param.cdn_estab_ini
    c-est-fim = tt-param.cdn_estab_fim
    .


run utp/ut-acomp.p persistent set h-acomp.
    
run pi-inicializar in h-acomp(input "Selecionando e criando planilha").
    
EMPTY TEMP-TABLE  tt-treinando.
i-linha = 0.
FOR EACH curso_trein NO-LOCK ,
    FIRST tip_curso_trein WHERE 
       tip_curso_trein.cdn_tip_curso_trein =  curso_trein.cdn_tip_curso_trein NO-LOCK,
     EACH  turma_trein OF CURSO_trein  NO-LOCK
     WHERE turma_trein.cdn_curso_trein = curso_trein.cdn_curso_trein AND 
           /*turma_trein.cdn_turma_trein = 1 AND*/
           turma_trein.dat_inic_turma_trein >= dt_periodo_ini AND
           turma_trein.dat_fim_turma_trein <= dt_periodo_fim,


   EACH treindo_turma_trein NO-LOCK WHERE
        treindo_turma_trein.cdn_curso_trein = curso_trein.cdn_curso_trein AND 
        treindo_turma_trein.cdn_turma_trein = turma_trein.cdn_turma_trein  AND
        treindo_turma_trein.idi_tip_treindo <> 2 AND
        treindo_turma_trein.cdn_empresa =  c-empresa AND
        treindo_turma_trein.cdn_estab   >= c-est-ini AND
        treindo_turma_trein.cdn_estab   <= c-est-fim AND
        ((tt-param.tg-pendente     AND treindo_turma_trein.idi_sit_trein = 1) or
         (tt-param.tg-confirmado   AND treindo_turma_trein.idi_sit_trein = 2) OR                
         (tt-param.tg-cursando     AND treindo_turma_trein.idi_sit_trein = 3) or
         (tt-param.tg-habilitado   AND treindo_turma_trein.idi_sit_trein = 4) or
         (tt-param.tg-desabilitado AND treindo_turma_trein.idi_sit_trein = 5) or
         (tt-param.tg-cancelado    AND treindo_turma_trein.idi_sit_trein = 6))
             ,
    FIRST funcionario WHERE funcionario.cdn_empresa     = treindo_turma_trein.cdn_empresa AND
                            funcionario.cdn_funcionario = treindo_turma_trein.cdn_treindo and
                            funcionario.cod_rh_ccusto   >= tt-param.c_ccusto_ini          AND
                            funcionario.cod_rh_ccusto   <= tt-param.c_ccusto_fim  NO-LOCK.
                          
        
          i-linha = i-linha + 1.
          if substring(string(i-linha,"9999999"),6,2) = "00" then 
             run pi-acompanhar in h-acomp(input "Treinamentos: " + string(i-linha,"9999999")).

                
          i-mes = MONTH(dt_periodo_ini).
          i-ano = year(dt_periodo_ini).
        
          assign v_qtd_hora_trein = 0.
           REPEAT:
              for each turma_carg_horar no-lock where
                       turma_carg_horar.cdn_curso_trein = turma_trein.cdn_curso_trein and
                       turma_carg_horar.cdn_turma_trein = turma_trein.cdn_turma_trein and
                       turma_carg_horar.num_mes_ano_turma = int(string( i-ano,"9999") + string(i-mes,"99")):
                  assign v_qtd_hora_trein = v_qtd_hora_trein + turma_carg_horar.qtd_hora_turma_mes.
              end.
              i-mes = i-mes + 1.
              IF i-mes > 12  THEN
                  ASSIGN i-mes = 1
                         i-ano = i-ano + 1.
              IF DATE(i-mes,1,i-ano) > DATE(MONTH(dt_periodo_fim),1,YEAR(dt_periodo_fim)) THEN LEAVE.
          end.
          if v_qtd_hora_trein = 0 then          
          for each horar_dia_turma_trein no-lock where
                   horar_dia_turma_trein.cdn_curso_trein       = turma_trein.cdn_curso_trein and
                   horar_dia_turma_trein.cdn_turma_trein       = turma_trein.cdn_turma_trein and
                   horar_dia_turma_trein.dat_aula_turma_trein >= dt_periodo_ini and
                   horar_dia_turma_trein.dat_aula_turma_trein <= dt_periodo_fim:
              assign v_qtd_hora_trein = v_qtd_hora_trein + horar_dia_turma_trein.qtd_hora_period_turma_trein.                
          end. /* for each horar_dia_turma_trein */

         

          FIND FIRST rh_ccusto WHERE 
                   rh_ccusto.cdn_empresa     = treindo_turma_trein.cdn_empresa   AND
                   rh_ccusto.cod_rh_ccusto   = funcionario.cod_rh_ccusto NO-LOCK NO-ERROR.

IF v_qtd_hora_trein >= 0 THEN DO: 
              CREATE tt-treinando.
          ASSIGN 
            tt-treinando.cdn_empresa          = treindo_turma_trein.cdn_empresa 
            tt-treinando.cdn_estab            = treindo_turma_trein.cdn_estab   
            tt-treinando.cdn_treindo          = treindo_turma_trein.cdn_treindo 
            tt-treinando.cdn_tip_curso_trein  = curso_trein.cdn_tip_curso_trein
            tt-treinando.des_tip_curso_trein  = tip_curso_trein.des_tip_curso_trein
            tt-treinando.cdn_curso_trein      = turma_trein.cdn_curso_trein 
            tt-treinando.cdn_turma_trein      = turma_trein.cdn_turma_trein     
            tt-treinando.nom_pessoa_fisic     = treindo_turma_trein.nom_pessoa_fisic
            tt-treinando.des_curso_trein      = curso_trein.des_curso_trein
            tt-treinando.dat_inic_turma_trein = turma_trein.dat_inic_turma_trein
            tt-treinando.dat_fim_turma_trein  = turma_trein.dat_fim_turma_trein 
            tt-treinando.idi_sit_trein        = treindo_turma_trein.idi_sit_trein
            tt-treinando.qtd_hora_trein       = v_qtd_hora_trein
            tt-treinando.cod_rh_ccusto        = funcionario.cod_rh_ccusto                                                                         
            tt-treinando.des_rh_ccusto        = IF AVAIL rh_ccusto THEN rh_ccusto.des_rh_ccusto ELSE "NÆo existe". 


               
          END.

        IF v_qtd_hora_trein = 0  THEN NEXT.


          FIND FIRST tt-cursos WHERE 
              tt-cursos.cod_rh_ccusto       = funcionario.cod_rh_ccusto  AND
              tt-cursos.cdn_tip_curso_trein = curso_trein.cdn_tip_curso_trein and
              tt-cursos.cdn_empresa         = treindo_turma_trein.cdn_empresa NO-ERROR.

          IF NOT AVAIL tt-cursos THEN DO:
              CREATE tt-cursos.

              FIND FIRST rh_ccusto WHERE 
                   rh_ccusto.cdn_empresa     = treindo_turma_trein.cdn_empresa   AND
                   rh_ccusto.cod_rh_ccusto   = funcionario.cod_rh_ccusto NO-LOCK NO-ERROR.

              ASSIGN  tt-cursos.cod_rh_ccusto       = funcionario.cod_rh_ccusto  
                      tt-cursos.cdn_tip_curso_trein = curso_trein.cdn_tip_curso_trein
                      tt-cursos.cdn_empresa         = treindo_turma_trein.cdn_empresa
                      tt-cursos.des_tip_curso_trein = tip_curso_trein.des_tip_curso_trein
                      tt-cursos.des_ccusto          = IF AVAIL rh_ccusto THEN rh_ccusto.des_rh_ccusto ELSE "NÆo existe".                                           

          END.

                        
              ASSIGN 
                  tt-cursos.qtde                = tt-cursos.qtde + 1
                  tt-cursos.horas               = tt-cursos.horas + v_qtd_hora_trein.

              /*    (IF turma_trein.qtd_hora_curso_trein > 0 THEN turma_trein.qtd_hora_curso_trein ELSE curso_trein.qtd_hora_curso_trein ). */
             
        
    
END.

 i-curso = 0.

run pi-acompanhar in h-acomp(input "Contando Funcionario... ").
 i-head-count = 0.


 if tt-param.rs-head-count = 1  THEN DO:
   FOR EACH esp_rh_head_count_ccusto WHERE 
      
     esp_rh_head_count_ccusto.ano         = YEAR(dt_periodo_ini) AND
     esp_rh_head_count_ccusto.cdn_estab  >= c-est-ini AND
     esp_rh_head_count_ccusto.cdn_estab  <= c-est-fim AND     
     esp_rh_head_count_ccusto.cdn_empresa = c-empresa  and
     esp_rh_head_count_ccusto.cod_rh_ccusto >= tt-param.c_ccusto_ini and
     esp_rh_head_count_ccusto.cod_rh_ccusto <= tt-param.c_ccusto_fim  NO-LOCK.
     
      i-linha = i-linha + 1.

    IF SUBSTRING(STRING(i-linha,"9999999"),7,1) = "0" THEN
    run pi-acompanhar in h-acomp(input "Contando headcount... " + STRING(i-linha,"9999999")).
    FOR EACH  tt-cursos WHERE    
        tt-cursos.cod_rh_ccusto       = esp_rh_head_count_ccusto.cod_rh_ccusto  AND             
        tt-cursos.cdn_empresa         = esp_rh_head_count_ccusto.cdn_empresa .
            tt-cursos.qt-func =  esp_rh_head_count_ccusto.head_count.
    END.


     ASSIGN i-head-count = i-head-count + esp_rh_head_count_ccusto.head_count .

   END.
 END.
 
 if i-head-count = 0 then
    FOR EACH funcionario FIELDS(dat_admis_func dat_desligto_func cdn_estab nom_pessoa_fisic)  WHERE
         funcionario.cdn_empresa       = c-empresa AND    
         funcionario.dat_admis_func   <= dt_periodo_fim AND
        (funcionario.dat_desligto_func = ? OR 
         funcionario.dat_desligto_func > dt_periodo_fim  ) USE-INDEX fncnr_empfunc  NO-LOCK.
        i-linha = i-linha + 1.
    
        IF SUBSTRING(STRING(i-linha,"9999999"),7,1) = "0" THEN
        run pi-acompanhar in h-acomp(input "Contando Funcionario... " + STRING(i-linha,"9999999")).
        FOR EACH  tt-cursos WHERE    
            tt-cursos.cod_rh_ccusto       = funcionario.cod_rh_ccusto  AND             
            tt-cursos.cdn_empresa         = funcionario.cdn_empresa USE-INDEX cc.
                tt-cursos.qt-func = tt-cursos.qt-func + 1.
        END.
    END.

ASSIGN 
 i-qtde = 0    
 i-curso = 0   
 i-curso-cc = 0.

    DEFINE BUFFER b-tt-cursos FOR tt-cursos.
  /*  
    FOR EACH  tt-cursos USE-INDEX cc 
               BREAK 
                   BY  tt-cursos.cdn_empresa 
                   BY  tt-cursos.cod_rh_ccusto              
                   BY  tt-cursos.cdn_tip_curso_trein.

        ASSIGN 
                 i-qtde =  i-qtde  + tt-cursos.qtde    
                 i-curso = i-curso + tt-cursos.horas.

        IF LAST-OF(tt-cursos.cod_rh_ccusto) THEN DO:
            FIND FIRST b-tt-cursos WHERE 
                b-tt-cursos.cod_rh_ccusto       = tt-cursos.cod_rh_ccusto       AND
                b-tt-cursos.cdn_tip_curso_trein = 0  and
                b-tt-cursos.cdn_empresa         = tt-cursos.cdn_empresa         NO-ERROR.

            

            IF NOT avail b-tt-cursos THEN DO:
                CREATE b-tt-cursos.
                ASSIGN 
                    b-tt-cursos.cod_rh_ccusto       = tt-cursos.cod_rh_ccusto       
                    b-tt-cursos.cdn_tip_curso_trein = 0  
                    b-tt-cursos.cdn_empresa         = tt-cursos.cdn_empresa  
                    b-tt-cursos.qt-func   = tt-cursos.qt-func
                    b-tt-cursos.qtde      = i-qtde   
                    b-tt-cursos.horas     = i-curso  
                    b-tt-cursos.des_tip_curso_trein = tt-cursos.des_tip_curso_trein
                    b-tt-cursos.des_ccusto          = "TOTAL"         
                    .

            END.

           ASSIGN 
             i-qtde = 0    
             i-curso = 0   
             i-curso-cc = 0.
        END.
           


            
    END.
*/
     ASSIGN 
             i-qtde = 0    
             i-curso = 0   
             i-curso-cc = 0
             i-curso-tt = 0.
    
    FOR EACH  tt-cursos USE-INDEX cc 
               BREAK 
                   BY  tt-cursos.cdn_empresa 
                   BY  tt-cursos.cod_rh_ccusto
                   BY  tt-cursos.cdn_tip_curso_trein
                                 
                  .

         tt-cursos.perc = (tt-cursos.qtde * tt-cursos.horas) / tt-cursos.qt-func.
        ASSIGN 
                 i-qtde =  i-qtde  + tt-cursos.qtde    
                 i-curso = i-curso + tt-cursos.horas
                 i-curso-tt = i-curso-tt + tt-cursos.perc.

        IF LAST-OF(tt-cursos.cod_rh_ccusto) THEN DO:
            FIND FIRST b-tt-cursos WHERE 
                b-tt-cursos.cod_rh_ccusto       = tt-cursos.cod_rh_ccusto      AND
                b-tt-cursos.cdn_tip_curso_trein = 0  and
                b-tt-cursos.cdn_empresa         = tt-cursos.cdn_empresa         NO-ERROR.

           


            IF NOT avail b-tt-cursos THEN DO:
                CREATE b-tt-cursos.
                ASSIGN 
                    b-tt-cursos.cod_rh_ccusto       = tt-cursos.cod_rh_ccusto       
                    b-tt-cursos.cdn_tip_curso_trein = 0  
                    b-tt-cursos.cdn_empresa         = tt-cursos.cdn_empresa        
                    b-tt-cursos.qt-func   = tt-cursos.qt-func
                    b-tt-cursos.qtde      = i-qtde   
                    b-tt-cursos.horas     = i-curso 
                    b-tt-cursos.des_tip_curso_trein = "TOTAL"
                    b-tt-cursos.des_ccusto          = tt-cursos.des_ccusto 
                    b-tt-cursos.perc                = i-curso-tt.

            END.

           ASSIGN 
             i-qtde = 0    
             i-curso = 0   
             i-curso-cc = 0
             i-curso-tt = 0.
        END.
           


            
    END.




 RUN pi-cria-planilha.

 IF RETURN-VALUE = "nok" THEN do:
     RUN pi-finalizar IN h-acomp.
     return.
 END.

  i-linha = 6.
  i-linha-graf = 9.
  i-graf = 1.
  FOR EACH  tt-cursos USE-INDEX cc 
               BREAK 
                   BY  tt-cursos.cdn_empresa 
                   BY  tt-cursos.cod_rh_ccusto              
                   BY  tt-cursos.cdn_tip_curso_trein.
     run pi-acompanhar in h-acomp(input "Gerando grafico linha: " + STRING(i-linha,"9999999")).
      IF tt-cursos.des_tip_curso_trein = "TOTAL" THEN
          i-linha = i-linha + 2.
      ELSE
          i-linha = i-linha + 1.

    IF FIRST-OF(tt-cursos.cod_rh_ccusto) THEN 

      ASSIGN 
        i-linha-ini = i-linha
          c-relatorio:range("A" + STRING(i-linha)):VALUE =  tt-cursos.des_ccusto.

      ASSIGN 
          c-relatorio:range("B" + STRING(i-linha)):VALUE =  tt-cursos.des_tip_curso_trein            
          c-relatorio:range("C" + STRING(i-linha)):VALUE =  tt-cursos.perc.
               
      IF LAST-OF(tt-cursos.cod_rh_ccusto) THEN DO:
           i-graf = i-graf + 1.
          
           
                RUN pi-gera-grafico.
             

      END.
  END.


  i-linha = 7.
 DEFINE VARIABLE i-ct AS INTEGER  INITIAL 1   NO-UNDO.

  FOR EACH  tt-cursos USE-INDEX TIPO 
               BREAK 
                   BY  tt-cursos.cdn_empresa 
                   BY  tt-cursos.cdn_tip_curso_trein
                   BY  tt-cursos.cod_rh_ccusto              
                  .
     run pi-acompanhar in h-acomp(input "Gerando grafico linha: " + STRING(i-linha,"9999999")).
     
          i-linha = i-linha + 1.


     IF FIRST-OF(tt-cursos.cdn_tip_curso_trein) THEN 

      ASSIGN 
          c-relatorio:range("E" + STRING(i-linha)):VALUE =  tt-cursos.des_tip_curso_trein
          i-ct = i-ct + 1.

     tt-cursos.coluna = i-ct.

     ASSIGN
          c-relatorio:range("F" + STRING(i-linha)):VALUE =  tt-cursos.des_ccusto            
          c-relatorio:range("G" + STRING(i-linha)):VALUE =  tt-cursos.perc.
               
  END.
  
   c-grafico = c-relatorio:ChartObjects(1):Chart.
    
   
   c-grafico:SetSourceData(c-relatorio:Range("e8:g":U + STRING(i-linha)), 2).
  
   i-ct = 0.
    FOR EACH  tt-cursos USE-INDEX TIPO 
               BREAK 
                   BY  tt-cursos.cdn_empresa 
                   BY  tt-cursos.cdn_tip_curso_trein
                   BY  tt-cursos.cod_rh_ccusto              
                  .

        i-ct = i-ct + 1.

        run pi-acompanhar in h-acomp(input "Colorindo coluna: " + STRING(i-ct,"9999999")).
    /*c-excel:VISIBLE = YES.*/
  /*  c-grafico:SeriesCollection(1):Points(i-ct):Format:Fill:ForeColor:RGB = 5645717 + i-ct * 1000000.*/
   /*c-grafico:SeriesCollection(1):Points(i-ct):Format:Fill:ForeColor:RGB = (tt-cursos.coluna + 150) * (tt-cursos.coluna + 90) * (tt-cursos.coluna + 80) .*/
    c-grafico:SeriesCollection(1):Points(i-ct):Format:Fill:ForeColor:RGB = 13000000 + (tt-cursos.coluna * 720000  ).
   END.
   
   /*
   rw-graf  = c-grafico.
   */
    
    c-relatorio:range("B1"):SELECT.

    c-excel:Sheets:item(2):NAME = "DADOS".

c-relatorio = c-excel:Sheets:item(2).



 i-linha = 0.

  ASSIGN 
     i-linha = i-linha + 1                            

     c-relatorio:range("A" + STRING(i-linha)):VALUE =  "ESTAB."                          
     c-relatorio:range("B" + STRING(i-linha)):VALUE =  "TIPO" 
     c-relatorio:range("C" + STRING(i-linha)):VALUE =  "DESCRI€ÇO TIPO"
     c-relatorio:range("D" + STRING(i-linha)):VALUE =  "CURSO"     
     c-relatorio:range("E" + STRING(i-linha)):VALUE =  "CURSO"  
     c-relatorio:range("F" + STRING(i-linha)):VALUE =  "TURMA"     
     c-relatorio:range("G" + STRING(i-linha)):VALUE =  "MATRÖCULA"  
     c-relatorio:range("H" + STRING(i-linha)):VALUE =  "NOME TREINANDO"   
     c-relatorio:range("I" + STRING(i-linha)):VALUE =  "DATA INÖCIO"
     c-relatorio:range("J" + STRING(i-linha)):VALUE =  "DATA FIM" 
     c-relatorio:range("K" + STRING(i-linha)):VALUE =  "SITUA€ÇO"      
     c-relatorio:range("L" + STRING(i-linha)):VALUE =  "CARGA HORµRIA" 
     c-relatorio:range("M" + STRING(i-linha)):VALUE =  "C.CUSTO"      
     c-relatorio:range("N" + STRING(i-linha)):VALUE =  "DESCRICAO C.CUSTO" 
     c-relatorio:range("O" + STRING(i-linha)):VALUE =  "QTD.FUNC CC"

      .

  DEFINE VARIABLE c-linha AS CHARACTER   NO-UNDO.

 FOR EACH tt-treinando.

     i-qtde = 0.

     FOR FIRST tt-cursos WHERE    
        tt-cursos.cod_rh_ccusto       = tt-treinando.cod_rh_ccusto  AND             
        tt-cursos.cdn_empresa         =tt-treinando.cdn_empresa USE-INDEX cc.
            i-qtde = tt-cursos.qt-func .
    END.
    

          if substring(string(i-linha,"9999999"),6,2) = "00" then 

     run pi-acompanhar in h-acomp(input "Gr fico: " +  STRING(tt-treinando.cdn_treindo) 
                                                    + "-" +  tt-treinando.nom_pessoa_fisic).

    ASSIGN 
     i-linha = i-linha + 1    
     c-linha    = string(tt-treinando.cdn_estab                 )       
                         + chr(160) + string(  tt-treinando.cdn_tip_curso_trein       )
                         + chr(160) + string(  tt-treinando.des_tip_curso_trein       )
                         + chr(160) + string(  tt-treinando.cdn_curso_trein           )
                         + chr(160) + string(  tt-treinando.des_curso_trein           )
                         + chr(160) + string(  tt-treinando.cdn_turma_trein           )
                         + chr(160) + string(  tt-treinando.cdn_treindo               )
                         + chr(160) + string(  tt-treinando.nom_pessoa_fisic          )         
                         + chr(160) + string(  tt-treinando.dat_inic_turma_trein      )
                         + chr(160) + string(  tt-treinando.dat_fim_turma_trein       )
                         + chr(160) + string(  tt-treinando.idi_sit_trein             )
                         + chr(160) + string(  tt-treinando.qtd_hora_trein            )
                         + chr(160) + string(  tt-treinando.cod_rh_ccusto             )
                         + chr(160) + string(  tt-treinando.des_rh_ccusto             )
                         + chr(160) + string(  i-qtde                                 )
                        
                         c-relatorio:range("A" + STRING(i-linha)):VALUE =  c-linha.   


 END.

 c-excel:Sheets:item(2):SELECT.

   c-relatorio:Range("A" + string(2) + ":A" + string(i-Linha)):select.

   c-excel:selection:TextToColumns (,         /* Destination          */
                                         1,        /* DataType             */
                                         ,         /* TextQualifier        */
                                         ,         /* ConsecutiveDelimiter */
                                         ,         /* Tab                  */
                                         ,         /* Semicolon            */
                                         ,         /* Comma                */
                                         ,         /* Space                */
                                         true,     /* Other                */
                                         CHR(160), /* OtherChar            */
                                         ,         /* FieldInfo            */
                                         ) no-error.




                c-relatorio:range("A1:O1"):Interior:ColorIndex = 55.
                c-relatorio:range("A1:O1"):Font:Name = "Arial".
                c-relatorio:range("A1:O1"):Font:FontStyle = "Negrito".
                c-relatorio:range("A1:O1"):Font:Size = 10.
                c-relatorio:range("A1:O1"):Font:ColorIndex = 2.
                c-relatorio:Rows("1:1"):Autofilter(,,,).

                c-relatorio:Cells:EntireColumn:AutoFit.


/* c-relatorio:range("B2"):SELECT.*/


  c-excel:Sheets:item(1):SELECT.




RUN pi-finalizar IN h-acomp.

RUN pi-finaliza-impressao.

PROCEDURE pi-cria-planilha:
    
    CREATE "Excel.Application" c-excel.
              ASSIGN c-excel:DisplayAlerts = FALSE.
 
              
               ASSIGN c-modelo-planilha = search("prghur/modelos/mod-esat0007.xlsx") 
               c-arq             = SESSION:TEMP-DIRECTORY.

            
              
       

       IF c-modelo-planilha = ?  THEN RETURN "nok".

    c-arquivo = c-arq + 'esat0007' + "-" + STRING(TODAY,"99999999") + STRING(time) + '.xlsx'.

    OS-COPY VALUE(c-modelo-planilha) VALUE(c-arquivo).
 
    assign c-planilha  = c-excel:Workbooks:OPEN(c-arquivo)
           c-relatorio = c-excel:Sheets:item(1)
           c-arq-anexo = (IF c-arq-anexo <> '' THEN ',' ELSE '') + c-arquivo.
 
END PROCEDURE.
 
PROCEDURE pi-gera-grafico.
    i-linha-graf = i-linha-graf + 22.
         c-relatorio:Shapes:AddChart:Select.
     c-relatorio:ChartObjects(i-graf):Left = c-relatorio:range("I" + string(i-linha-graf) + ":I" + string(i-linha-graf)):LEFT.
     c-relatorio:ChartObjects(i-graf):Top = c-relatorio:range("I" + string(i-linha-graf) + ":I" + string(i-linha-graf)):Top.
     
     c-relatorio:ChartObjects(i-graf):Height = 268.
     c-relatorio:ChartObjects(i-graf):Width  = 420.
     
     c-grafico = c-relatorio:ChartObjects(i-graf):Chart.
     c-grafico:ChartType = 51.
     c-grafico:SetSourceData(c-relatorio:Range("a" + string(i-linha-ini) + ":c":U + STRING(i-linha)), 2).
       c-grafico:HasLegend = no.
        c-grafico:HasTitle = YES.
        c-grafico:ChartTitle:Text = c-relatorio:Range("a" + string(i-linha-ini) + ":a":U + STRING(i-linha-ini)):VALUE .

   /*  
    c-grafico:SeriesCollection:NewSeries.
                           MESSAGE "aqui" "a" + string(i-linha-ini) + ":c":U + STRING(i-linha)
                               VIEW-AS ALERT-BOX INFO BUTTONS OK.
    c-grafico:SetSourceData(c-relatorio:Range("a" + string(i-linha-ini) + ":c":U + STRING(i-linha)), 2).
           MESSAGE "aqui2"
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
               */
    /*Assign
    
    c-grafico:SeriesCollection(1):Name = "=HHT AREA!" + string(i-linh-ini)
    c-grafico:SeriesCollection(1):Values = "=HHT AREA!$C$11:$C$" + string(i-linha).
     c-grafico:SeriesCollection:NewSeries.
    Assign
    c-grafico:SeriesCollection(2):Name = "=HHT AREA!$D$10"
    c-grafico:SeriesCollection(2):Values = "=HHT AREA!$D$11:$D$" + string(i-linha) .
    c-grafico:SeriesCollection:NewSeries.
    Assign
       
    c-grafico:SeriesCollection(3):Name = "=HHT AREA!$E$10"
    c-grafico:SeriesCollection(3):Values = "=HHT AREA!$E$11:$E$" + string(i-linha).
    
    c-grafico:SeriesCollection(i-graf):XValues = "=HHT AREA!$B$11:$B$" + string(i-linha).
      */
    /*c-grafico:SeriesCollection(1):Name = "=Plan4!$B$8"
    c-grafico:SeriesCollection(1):Values = "=Plan4!$B$9:$B$" + string(i-linha)
    c-grafico:SeriesCollection(1):XValues = "=Plan4!$A$9:$A$" + string(i-linha)
    */
   /* 
    ASSIGN

       c-grafico:TITLE = c-relatorio:Range("a" + string(i-linha-ini) + ":a":U + STRING(i-linha-ini)):VALUE.*/
    /*
    c-grafico:Axes(2,1):MinimumScale = 1
    c-grafico:Axes(2,1):MaximumScale = 5
    c-grafico:Axes(2,1):MajorUnit = 1
    c-grafico:Axes(2,1):MajorUnit = 0.8
    .
     c-grafico:HasTitle = no.
     c-grafico:HasLegend = YES.

 c-grafico:PlotArea:Format:ThreeD:RotationX = 0.
 c-grafico:PlotArea:Format:ThreeD:RotationY = 90.
 c-grafico:PlotArea:Format:ThreeD:FieldOfView = 30.      
      */
END.
  
PROCEDURE pi-finaliza-impressao:
DEF VAR i         AS INT  NO-UNDO.


    c-planilha:SAVE().
/*    c-planilha:CLOSE().
  */   
    c-excel:VISIBLE = YES.
 
    DOS SILENT COPY VALUE(c-arquivo)  VALUE(tt-param.arq-entrada).
    /*    c-excel:Workbooks:OPEN(c-arquivo).
      */

    /*c-excel:QUIT().*/
    IF VALID-HANDLE(c-excel) THEN RELEASE OBJECT c-excel.
    IF VALID-HANDLE(c-relatorio) THEN RELEASE OBJECT c-relatorio.
    IF valid-handle(c-planilha) THEN RELEASE OBJECT c-planilha.
  /*  IF VALID-HANDLE(c-grafico)  THEN RELEASE object c-grafico.*/



END PROCEDURE.      

 
