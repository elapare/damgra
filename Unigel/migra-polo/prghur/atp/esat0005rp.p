/********************************************************************************
* Programa: esat0005rp.p
* Data....: jan/2014
* Autor...: Edson
* Alteracao :  
* Objetivo: Gr f.Universo Pessoas Treinadas
*         
* VersÆo..: 2.00.000                            

*******************************************************************************/
/*---------------- Include de controle de VersÆo ------------------*/ 
{bf/buffersHCM.i}
{include/i-prgvrs.i esat0005RP 2.00.00.000}


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
    field cod_emp          as CHAR
    FIELD cdn_estab_ini    AS char
    FIELD cdn_estab_fim    AS char
    field c_ccusto_ini     as CHAR
    field c_ccusto_fim     as CHAR    
    field dt_periodo_ini as date
    field dt_periodo_fim as date
    field tg-grafico       as LOGICAL  
    field tg-pendente      as LOGICAL           
    FIELD tg-confirmado    as LOGICAL
    FIELD tg-cursando      as LOGICAL       
    field tg-habilitado    as LOGICAL       
    field tg-desabilitado  as LOGICAL       
    field tg-cancelado     AS LOGICAL
        . 
    

DEFINE TEMP-TABLE tt-plano    
        FIELD cdn_empresa LIKE plano_trein.cdn_empresa
        FIELD cdn_curso_trein LIKE curso_trein.cdn_curso_trein
        FIELD des_curso_trein LIKE curso_trein.des_curso_trein
        FIELD cdn_tip_curso_trein LIKE curso_trein.cdn_tip_curso_trein
        FIELD des_tip_curso_trein LIKE tip_curso_trein.des_tip_curso_trein
        FIELD cod_rh_ccusto like plano_trein.cod_rh_ccusto 
        FIELD dat_prev_inic_trein LIKE plano_trein_curso.dat_prev_inic_trein 
        FIELD dat_prev_term_trein like plano_trein_curso.dat_prev_term_trein  
        FIELD qti_participan_plano LIKE plano_trein_curso.qti_participan_plano
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
  .


 
/* recebimento de parƒmetros */
def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.
  
  /*
CREATE tt-param.
ASSIGN 
      /*  tt-param.cdn_curso_trein  = 51 
        tt-param.cdn_turma_trein  = 1*/
        tt-param.cod_emp          = "420"   
        tt-param.cdn_estab_ini    = "421"
        tt-param.cdn_estab_fim    = "426"
        tt-param.dt_periodo_ini         = 01/01/2013
        tt-param.dt_periodo_fim         = 12/31/2013
        tt-param.tg-pendente      = yes 
       
        tt-param.tg-confirmado    = yes 
      
        tt-param.tg-cursando      = yes 
        tt-param.tg-habilitado    = yes 
        tt-param.tg-desabilitado  = yes 
        tt-param.tg-cancelado     = NO
       .
*/
 
/****************** Defini‡ao de Vari veis de Processamento do Relat¢rio *********************/
def var h-acomp              as handle no-undo.
Def Var dat-trab As Date.
/****************** Defini‡ao de  Frames e Forms do Relat¢rio 132 Colunas ***************************************/ 

/****************** INCLUDE COM VARIµVEIS GLOBAIS *********************/
 


 DEFINE TEMP-TABLE tt-cursos
    FIELD cdn_empresa LIKE funcionario.cdn_empresa    
    FIELD cdn_curso_trein LIKE curso_trein.cdn_curso_trein
    FIELD des_curso_trein LIKE  curso_trein.des_curso_trein
    FIELD qtde AS int 
    FIELD qtde-p AS INT  .

  
 

 
DEFINE VARIABLE i-qtde-tt AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-qtde    AS DEC     NO-UNDO.
DEFINE VARIABLE i-curso AS DEC     NO-UNDO.
DEFINE VARIABLE i-curso-tt AS DEC     NO-UNDO.
DEFINE VARIABLE c-empresa AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dt_periodo_ini AS DATE        NO-UNDO.
DEFINE VARIABLE dt_periodo_fim AS DATE        NO-UNDO.
DEFINE VARIABLE c-est-ini AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-est-fim AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-pos AS INTEGER     NO-UNDO.
def var c-modelo-planilha  as char format "x(50)"         no-undo.
def var c-excel            as com-handle                  NO-UNDO.
def var c-planilha         as com-handle.
def var c-relatorio        as com-handle.
def var c-grafico          as com-handle.
DEFINE VARIABLE c-arq AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-arq-anexo AS CHARACTER   NO-UNDO.
DEF VAR c-arquivo AS CHAR NO-UNDO.

DEFINE VARIABLE i-linha AS INTEGER     NO-UNDO.
    
ASSIGN 
    c-empresa = tt-param.cod_emp
    dt_periodo_ini  = tt-param.dt_periodo_ini  
    dt_periodo_fim  = tt-param.dt_periodo_fim  
    c-est-ini = tt-param.cdn_estab_ini
    c-est-fim = tt-param.cdn_estab_fim
    .


run utp/ut-acomp.p persistent set h-acomp.
    
run pi-inicializar in h-acomp(input "Selecionando e criando planilha").
    

  FOR EACH plano_trein WHERE plano_trein.cdn_empresa = c-empresa NO-LOCK,
    EACH plano_trein_curso OF plano_trein where
        plano_trein_curso.dat_prev_inic_trein >= dt_periodo_ini AND
        plano_trein_curso.dat_prev_inic_trein <= dt_periodo_fim AND
        plano_trein_curso.cod_rh_ccusto   >= tt-param.c_ccusto_ini          AND
        plano_trein_curso.cod_rh_ccusto   <= tt-param.c_ccusto_fim   NO-LOCK,
      EACH curso_trein WHERE curso_trein.cdn_curso_trein = plano_trein_curso.cdn_curso_trein
      NO-LOCK,
      FIRST tip_curso_trein WHERE 
        tip_curso_trein.cdn_tip_curso_trein =  curso_trein.cdn_tip_curso_trein NO-LOCK.
   
        IF plano_trein_curso.dat_prev_inic_trein = ? OR YEAR(plano_trein_curso.dat_prev_term_trein ) < 2000 THEN NEXT.
        IF plano_trein_curso.dat_prev_term_trein  = ? OR YEAR(plano_trein_curso.dat_prev_term_trein ) < 2000 THEN NEXT.
    
     if substring(string(i-linha,"9999999"),6,2) = "00" then 
        run pi-acompanhar in h-acomp(input "Lendo plano: " + string(i-linha,"9999999")).

    ASSIGN 
     i-linha = i-linha + 1.

          
          CREATE tt-plano.
          ASSIGN                
             tt-plano.cdn_empresa          = plano_trein.cdn_empresa
             tt-plano.cdn_curso_trein      = curso_trein.cdn_curso_trein
             tt-plano.des_curso_trein      = curso_trein.des_curso_trein
             tt-plano.cdn_tip_curso_trein  = curso_trein.cdn_tip_curso_trein
             tt-plano.des_tip_curso_trein  = tip_curso_trein.des_tip_curso_trein
             tt-plano.cod_rh_ccusto        = plano_trein.cod_rh_ccusto 
             tt-plano.dat_prev_inic_trein  = plano_trein_curso.dat_prev_inic_trein 
             tt-plano.dat_prev_term_trein  = plano_trein_curso.dat_prev_term_trein
             tt-plano.qti_participan_plano = plano_trein_curso.qti_participan_plano
        
               .
        

          FIND FIRST tt-cursos WHERE 
                  tt-cursos.cdn_empresa         = plano_trein.cdn_empresa AND              
                  tt-cursos.cdn_curso_trein     = curso_trein.cdn_curso_trein NO-ERROR.
          IF NOT AVAIL tt-cursos THEN DO:
              CREATE tt-cursos.

              ASSIGN 
                  tt-cursos.cdn_empresa         = plano_trein.cdn_empresa                  
                  tt-cursos.cdn_curso_trein     = curso_trein.cdn_curso_trein
                  tt-cursos.des_curso_trein     = curso_trein.des_curso_trein .
          END.
          ASSIGN
                  tt-cursos.qtde                = 0
                  tt-cursos.qtde-p              =  tt-cursos.qtde-p + plano_trein_curso.qti_participan_plano.


END.

  i-linha = 0.
  
FOR EACH curso_trein NO-LOCK ,    
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
        
        ((tt-param.tg-pendente     AND treindo_turma_trein.idi_sit_trein = 1) or
         (tt-param.tg-confirmado   AND treindo_turma_trein.idi_sit_trein = 2) OR                
         (tt-param.tg-cursando     AND treindo_turma_trein.idi_sit_trein = 3) or
         (tt-param.tg-habilitado   AND treindo_turma_trein.idi_sit_trein = 4) or
         (tt-param.tg-desabilitado AND treindo_turma_trein.idi_sit_trein = 5) or
         (tt-param.tg-cancelado    AND treindo_turma_trein.idi_sit_trein = 6)),
    FIRST funcionario WHERE funcionario.cdn_empresa     = treindo_turma_trein.cdn_empresa AND
                          funcionario.cdn_funcionario = treindo_turma_trein.cdn_treindo and
                          funcionario.cod_rh_ccusto   >= tt-param.c_ccusto_ini          AND
                          funcionario.cod_rh_ccusto   <= tt-param.c_ccusto_fim NO-LOCK,
    FIRST tip_curso_trein WHERE 
   tip_curso_trein.cdn_tip_curso_trein =  curso_trein.cdn_tip_curso_trein NO-LOCK.

                     
                .
        
     if substring(string(i-linha,"9999999"),6,2) = "00" then 
        run pi-acompanhar in h-acomp(input "Lendo Treinamentos: " + string(i-linha,"9999999")).

    ASSIGN 
     i-linha = i-linha + 1.

          FIND FIRST rh_ccusto WHERE 
                       rh_ccusto.cdn_empresa     = treindo_turma_trein.cdn_empresa   AND
                       rh_ccusto.cod_rh_ccusto   = funcionario.cod_rh_ccusto NO-LOCK NO-ERROR.


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
               tt-treinando.cod_rh_ccusto        = funcionario.cod_rh_ccusto                                                                         
               tt-treinando.des_rh_ccusto        = IF AVAIL rh_ccusto THEN rh_ccusto.des_rh_ccusto ELSE "NÆo existe". 



          FIND FIRST tt-cursos WHERE 
                  tt-cursos.cdn_empresa         = treindo_turma_trein.cdn_empresa AND              
                  tt-cursos.cdn_curso_trein     = curso_trein.cdn_curso_trein NO-ERROR.
          IF NOT AVAIL tt-cursos THEN DO:
              CREATE tt-cursos.

              ASSIGN 
                  tt-cursos.cdn_empresa         = treindo_turma_trein.cdn_empresa                  
                  tt-cursos.cdn_curso_trein     = curso_trein.cdn_curso_trein
                  tt-cursos.des_curso_trein     = curso_trein.des_curso_trein .
          END.
          ASSIGN
                  tt-cursos.qtde                = tt-cursos.qtde + 1.                  
                   
    
END.

 
 RUN pi-cria-planilha.

 IF RETURN-VALUE = "nok" THEN do:
     RUN pi-finalizar IN h-acomp.
     return.
 END.

 i-linha = 2.
  i-qtde-tt = 0.
  i-qtde    = 0. 
 FOR EACH tt-cursos WHERE  BREAK BY tt-cursos.des_curso_trein .
         assign
             i-linha = i-linha + 1
             c-relatorio:range("A" + STRING(i-linha)):VALUE =  tt-cursos.des_curso_trein
             c-relatorio:range("B" + STRING(i-linha)):VALUE =  tt-cursos.qtde
             c-relatorio:range("C" + STRING(i-linha)):VALUE =  tt-cursos.qtde-p. 

          i-qtde-tt = i-qtde-tt + tt-cursos.qtde-P. 
          i-qtde    = i-qtde    + tt-cursos.qtde.   
    
END.




    i-linha = i-linha + 1.
    ASSIGN 
        c-relatorio:range("A" + STRING(i-linha)):VALUE =  "TOTAL GERAL"    
        c-relatorio:range("B" + STRING(i-linha)):VALUE =  i-qtde
        c-relatorio:range("C" + STRING(i-linha)):VALUE =  i-qtde-tt   .
                  ASSIGN
                 
                  c-relatorio:range("A" + STRING(2) + ":C" + STRING(i-linha)):Borders(9):LineStyle  = 1
                  c-relatorio:range("A" + STRING(2) + ":C" + STRING(i-linha)):Borders(9):Weight     = -4138
                  c-relatorio:range("A" + STRING(2) + ":C" + STRING(i-linha)):Borders(9):ColorIndex = -4105                 
                  c-relatorio:range("A" + STRING(2) + ":C" + STRING(i-linha)):Borders(10):LineStyle  = 1
                  c-relatorio:range("A" + STRING(2) + ":C" + STRING(i-linha)):Borders(10):Weight     = -4138
                  c-relatorio:range("A" + STRING(2) + ":C" + STRING(i-linha)):Borders(10):ColorIndex = -4105
                  c-relatorio:range("A" + STRING(2) + ":C" + STRING(i-linha)):Borders(8):LineStyle  = 1
                  c-relatorio:range("A" + STRING(2) + ":C" + STRING(i-linha)):Borders(8):Weight     = -4138
                  c-relatorio:range("A" + STRING(2) + ":C" + STRING(i-linha)):Borders(8):ColorIndex = -4105
                  c-relatorio:range("A" + STRING(2) + ":C" + STRING(i-linha)):Borders(7):LineStyle  = 1
                  c-relatorio:range("A" + STRING(2) + ":C" + STRING(i-linha)):Borders(7):Weight     = -4138
                  c-relatorio:range("A" + STRING(2) + ":C" + STRING(i-linha)):Borders(7):ColorIndex = -4105.

  IF tt-param.tg-grafico THEN  DO:
  
   c-grafico = c-relatorio:ChartObjects(1):Chart.

   c-grafico:SetSourceData(c-relatorio:Range("a2:c":U + STRING(i-linha - 1 )), 2).
  END.

    
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
     c-relatorio:range("L" + STRING(i-linha)):VALUE =  "C.CUSTO"      
     c-relatorio:range("M" + STRING(i-linha)):VALUE =  "DESCRICAO C.CUSTO" 

      .


 FOR EACH tt-treinando.
 
  if substring(string(i-linha,"9999999"),6,2) = "00" then 
        run pi-acompanhar in h-acomp(input "Gerando DADOS: " + string(i-linha,"9999999")).

    
 
    ASSIGN 
     i-linha = i-linha + 1                            

     c-relatorio:range("A" + STRING(i-linha)):VALUE =  tt-treinando.cdn_estab                        
                           + chr(160) + string(tt-treinando.cdn_tip_curso_trein )
                           + chr(160) + string(tt-treinando.des_tip_curso_trein )
                           + chr(160) + string( tt-treinando.cdn_curso_trein     )
                           + chr(160) + string(tt-treinando.des_curso_trein  )
                           + chr(160) + string(tt-treinando.cdn_turma_trein )
                           + chr(160) + string( tt-treinando.cdn_treindo )
                           + chr(160) + string(tt-treinando.nom_pessoa_fisic  )
                           + chr(160) + string(tt-treinando.dat_inic_turma_trein )
                           + chr(160) + string(tt-treinando.dat_fim_turma_trein  )
                           + chr(160) + string(tt-treinando.idi_sit_trein )            
                           + chr(160) + string(tt-treinando.cod_rh_ccusto      )
                           + chr(160) + string(tt-treinando.des_rh_ccusto )
     

      .


 END.
 
      c-excel:Sheets:item(2):SELECT.
   
     c-relatorio:Range("A2"):select.
      
     c-relatorio:Range("A2:a3"):select.
   
  

     c-relatorio:Range("A" + string(2) + ":A" + string(i-Linha)):select.

   
   
  
     c-excel:selection:TextToColumns    (,         /* Destination          */
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


 
                c-relatorio:range("A1:M1"):Interior:ColorIndex = 55.
                c-relatorio:range("A1:M1"):Font:Name = "Arial".
                c-relatorio:range("A1:M1"):Font:FontStyle = "Negrito".
                c-relatorio:range("A1:M1"):Font:Size = 10.
                c-relatorio:range("A1:M1"):Font:ColorIndex = 2.
                c-relatorio:Rows("1:1"):Autofilter(,,,).

                c-relatorio:Cells:EntireColumn:AutoFit.


/* c-relatorio:range("B2"):SELECT.*/


                c-excel:Sheets:item(3):NAME = "PLANOS".

                c-relatorio = c-excel:Sheets:item(3).



                   i-linha = 0.
                  ASSIGN 
                     i-linha = i-linha + 1                            
                     c-relatorio:range("A" + STRING(i-linha)):VALUE =  "EMPRESA"                          
                     c-relatorio:range("B" + STRING(i-linha)):VALUE =  "TIPO" 
                     c-relatorio:range("C" + STRING(i-linha)):VALUE =  "DESCRI€ÇO TIPO"
                     c-relatorio:range("D" + STRING(i-linha)):VALUE =  "CURSO"     
                     c-relatorio:range("E" + STRING(i-linha)):VALUE =  "CURSO"               
                     c-relatorio:range("F" + STRING(i-linha)):VALUE =  "DATA INÖCIO"
                     c-relatorio:range("G" + STRING(i-linha)):VALUE =  "DATA FIM"     
                     c-relatorio:range("H" + STRING(i-linha)):VALUE =  "C.CUSTO" 
                     c-relatorio:range("I" + STRING(i-linha)):VALUE =  "DESCRICAO C.CUSTO" 
                     c-relatorio:range("J" + STRING(i-linha)):VALUE =  "QTD.PESSOAS" 

                      .
                 FOR EACH tt-plano BY tt-plano.cdn_empresa            
                                   BY tt-plano.cdn_tip_curso_trein    .

                          run pi-acompanhar in h-acomp(input "Plano: " +  STRING( i-linha) ).

                   FIND FIRST rh_ccusto WHERE 
                             rh_ccusto.cdn_empresa     =  tt-plano.cdn_empresa   AND
                             rh_ccusto.cod_rh_ccusto   =  tt-plano.cod_rh_ccusto NO-LOCK NO-ERROR.


                    ASSIGN 
                     i-linha = i-linha + 1                            

                     c-relatorio:range("A" + STRING(i-linha)):VALUE =  tt-plano.cdn_empresa                       
                     c-relatorio:range("B" + STRING(i-linha)):VALUE =  tt-plano.cdn_tip_curso_trein 
                     c-relatorio:range("C" + STRING(i-linha)):VALUE =  tt-plano.des_tip_curso_trein 
                     c-relatorio:range("D" + STRING(i-linha)):VALUE =  tt-plano.cdn_curso_trein     
                     c-relatorio:range("E" + STRING(i-linha)):VALUE =  tt-plano.des_curso_trein            
                     c-relatorio:range("F" + STRING(i-linha)):VALUE =  tt-plano.dat_prev_inic_trein 
                     c-relatorio:range("G" + STRING(i-linha)):VALUE =  tt-plano.dat_prev_term_trein     
                     c-relatorio:range("H" + STRING(i-linha)):VALUE =  tt-plano.cod_rh_ccusto      
                     c-relatorio:range("I" + STRING(i-linha)):VALUE =  (IF AVAIL rh_ccusto THEN rh_ccusto.des_rh_ccusto ELSE "")
                     c-relatorio:range("J" + STRING(i-linha)):VALUE =  tt-plano.qti_participan_plano 


                      .


                 END.
                                c-relatorio:range("A1:J1"):Interior:ColorIndex = 55.
                                c-relatorio:range("A1:J1"):Font:Name = "Arial".
                                c-relatorio:range("A1:J1"):Font:FontStyle = "Negrito".
                                c-relatorio:range("A1:J1"):Font:Size = 10.
                                c-relatorio:range("A1:J1"):Font:ColorIndex = 2.
                                c-relatorio:Rows("1:1"):Autofilter(,,,).

                                c-relatorio:Cells:EntireColumn:AutoFit.




 c-relatorio = c-excel:Sheets:item(1).

RUN pi-finalizar IN h-acomp.

RUN pi-finaliza-impressao.

PROCEDURE pi-cria-planilha:
    
    CREATE "Excel.Application" c-excel.
              ASSIGN c-excel:DisplayAlerts = FALSE.
 
               
               ASSIGN c-modelo-planilha = IF tt-param.tg-grafico THEN search("prghur/modelos/mod-esat0005.xlsx") ELSE search("prghur/modelos/mod-esat0005a.xlsx") 
               c-arq             = SESSION:TEMP-DIRECTORY.

            
              
       

       IF c-modelo-planilha = ?  THEN RETURN "nok".

    c-arquivo = c-arq + 'esat0005' + "-" + STRING(TODAY,"99999999") + STRING(time) + '.xlsx'.

    OS-COPY VALUE(c-modelo-planilha) VALUE(c-arquivo).
 
    assign c-planilha  = c-excel:Workbooks:OPEN(c-arquivo)
           c-relatorio = c-excel:Sheets:item(1)
           c-arq-anexo = (IF c-arq-anexo <> '' THEN ',' ELSE '') + c-arquivo.
 
END PROCEDURE.

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
    IF VALID-HANDLE(c-grafico)  THEN RELEASE object c-grafico.



END PROCEDURE.      

 
