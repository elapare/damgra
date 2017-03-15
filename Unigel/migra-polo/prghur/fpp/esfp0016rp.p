/*****************************************************************************
**
**       Programa: esfp0016rp.p
**
**       Author...........: Amgra / Edson
**       Created..........: 25/04/2011     
**
**       Objetivo: Exporta Informaá‰es funcion†rios Seguro
**
**       OBS.....: 
**
*******************************************************************************/
{bf/buffersHCM.i}


define variable c-prog-gerado as character no-undo initial "esfp0016rp".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.


def input param i-empresa-ini as char initial "400" no-undo.
def input param i-empresa-fim as CHAR initial "400" no-undo. 
def input param c-cod-est-ini as CHAR initial "401" no-undo.
def input param c-cod-est-fim as CHAR initial "401" no-undo.
def input param i-mes-ref     as int FORM "99"   initial 08   no-undo.
def input param i-ano-ref     as int form "9999" initial 2012 no-undo. 


/*
run grapi/gr2002.p (input c-prog-gerado, input "2.00.00.000").
*/

/****************** Definiá∆o de Tabelas Tempor†rias do Relat¢rio **********************/

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
        

/****************** INCLUDE COM VARIµVEIS GLOBAIS *********************/

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

/****************** FIM INCLUDE COM VARIµVEIS GLOBAIS *********************/
/****************** Definiáao de ParÉmetros do Relat¢rio *********************/ 

/****************** Definiáao de Vari†veis de Seleá∆o do Relat¢rio *********************/ 



/****************** Definiáao de Vari†veis p/ Campos Virtuais do Relat¢rio *******************/ 

/****************** Definiáao de Vari†veis Campo do Layout do Relat¢rio **********************/ 

/****************** Definiáao de Vari†veis do Relat¢rio N∆o Pedidas em Tela ******************/ 

/* para executar o Excel */

DEF VAR c-arq             AS CHAR                NO-UNDO.
DEF VAR c-arq-anexo       AS CHAR                NO-UNDO.   
DEF VAR l-cria            AS LOG                 NO-UNDO.
DEF VAR i-pagina          AS INT INIT 1          NO-UNDO.
DEF VAR i-linha           AS INT                 NO-UNDO.
DEF VAR c-coluna          AS char                NO-UNDO.

def var c-modelo-planilha  as char format "x(50)"         no-undo.
def var c-excel            as com-handle                  NO-UNDO.
def var c-planilha         as com-handle.
def var c-relatorio        as com-handle.
DEFINE VARIABLE arquivo-jr   AS CHARACTER  FORMAT "x(50)" NO-UNDO.
/****************** Definiáao de Vari†veis de Total do Relat¢rio *****************************/ 

/****************** Definiáao de Vari†veis dos Calculos do Relat¢rio *************************/ 


/***************** Definiáao de Vari†veis de Processamento do Relat¢rio *********************/
def var h-acomp              as handle no-undo.
def var v-cod-destino-impres as char   no-undo.
def var v-num-reg-lidos      as int    no-undo.
def var v-num-point          as int    no-undo.
def var v-num-set            as int    no-undo.
def var v-num-linha          as int    no-undo.

/****************** Definiáao de Forms do Relat¢rio 132 Colunas ***************************************/ 

form HEADER
    fill("-", 170) format "x(170)" SKIP 
    WITH DOWN WIDTH 170 NO-BOX STREAM-IO FRAME f-relat-linha.

form HEADER
    fill(" ", 170) format "x(170)" SKIP 
    WITH DOWN WIDTH 170 NO-BOX STREAM-IO FRAME f-relat-branco.


 
def temp-table tt-editor no-undo
    field linha      as integer
    field conteudo   as character format "x(80)"
    index editor-id is primary unique linha.


def var rw-log-exec                            as rowid no-undo.
def var c-erro-rpc as character format "x(60)" initial " " no-undo.
def var c-erro-aux as character format "x(60)" initial " " no-undo.
def var c-ret-temp as char no-undo.
def var h-servid-rpc as handle no-undo.     
define var c-empresa       as character format "x(40)"      no-undo.
define var c-titulo-relat  as character format "x(50)"      no-undo.
define var i-numper-x      as integer   format "ZZ"         no-undo.
define var da-iniper-x     as date      format "99/99/9999" no-undo.
define var da-fimper-x     as date      format "99/99/9999" no-undo.
define var i-page-size-rel as integer                       no-undo.
define var c-programa      as character format "x(08)"      no-undo.
define var c-versao        as character format "x(04)"      no-undo.
define var c-revisao       as character format "999"        no-undo.


def var capital-exced  as dec no-undo.
def var valor-seguro  as dec no-undo.
def var d-remun       as dec no-undo.
def var b-seguro      as dec no-undo.
def var idx           as integer no-undo.




define new shared var c-impressora   as character                      no-undo.
define new shared var c-layout       as character                      no-undo.
define new shared var v_num_count     as integer                       no-undo.
define new shared var c-arq-control   as character                     no-undo.
define new shared var c-sistema       as character format "x(25)"      no-undo.
define new shared var c-rodape        as character                     no-undo.
/*
define new shared buffer b_ped_exec_style for mgmulti.ped_exec.
define new shared buffer b_servid_exec_style for mgmulti.servid_exec.
*/
DEFINE VARIABLE dt-sai-transf AS DATE       NO-UNDO.
DEFINE VARIABLE dt-ENT-transf AS DATE       NO-UNDO.
define new shared stream str-rp.
DEF VAR c-arquivo_2 AS CHAR FORMAT "x(50)" NO-UNDO.
DEF VAR c-arquivo_txt AS CHAR FORMAT "x(50)" NO-UNDO.
DEF VAR c-arquivo_xls AS CHAR FORMAT "x(50)" NO-UNDO.

def var dt-data-ini as date no-undo.
def var dt-data-fim as date no-undo.
def var d-mult-sal    as dec no-undo.
def var d-taxa        as dec no-undo.
def var d-lim-min-cap as dec no-undo.
def var d-lim-max-cap as dec no-undo.
def var d-apolice     as dec format ">>,>>>,>>9" no-undo.
def var i-empresa   as CHAR initial "400" no-undo.



 
assign c-programa     = "esfp0016rp"
       c-versao       = "2.00"
       c-revisao      = "1.00.000"
       c-titulo-relat = "Exporta Informaá‰es funcion†rios Seguro"
       c-sistema      = "".

form header
    fill("-", 170) format "x(170)" skip
    c-empresa c-titulo-relat at 50
    "Folha:" at 159 page-number(str-rp) at 166 format ">>>>9" skip
    fill("-", 148) format "x(148)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") SKIP
    fill("-", 170) format "x(170)" skip
    with stream-io width 170 no-labels no-box page-top frame f-cabec.

form header
    c-rodape format "x(170)"
    with stream-io width 170 no-labels no-box page-bottom frame f-rodape.
 
/* for each e disp */



 
run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Gerando Relat¢rio, Aguarde... ").

for each  empresa where empresa.ep-codigo >= i-empresa-ini and
                        empresa.ep-codigo <= i-empresa-fim no-lock,
   each rh_estab  where rh_estab.cdn_empresa = empresa.ep-codigo and
                        rh_estab.cdn_estab >= c-cod-est-ini  AND
                        rh_estab.cdn_estab <= c-cod-est-fim no-lock.

    i-empresa = empresa.ep-codigo.

    find first val_unit_form_fp where val_unit_form_fp.cdn_val_unit_fp =  515 and
               val_unit_form_fp.cdn_empresa =  i-empresa no-lock no-error.
    
    
    if not avail val_unit_form_fp then do: 
        run pi-acompanhar in h-acomp(input "Falta Ap¢lice empresa: " + string(i-empresa)).
        pause 1.
                   
        next.
    end.
    
    d-apolice = val_unit_form_fp.val_calcul_efp.
    
    
    find first val_unit_form_fp where val_unit_form_fp.cdn_val_unit_fp =  514 and
               val_unit_form_fp.cdn_empresa =  i-empresa no-lock no-error.
    
    if not avail val_unit_form_fp then do:
     run pi-acompanhar in h-acomp(input "Falta Taxa empresa: " + string(i-empresa)).
    
     pause 1.
                   
        next.
    end.
    

          
           d-taxa = val_unit_form_fp.val_calcul_efp / 100000.
 

    
    
     find first val_unit_form_fp where val_unit_form_fp.cdn_val_unit_fp =  (if index ("353, 354 , 393",string(rh_estab.cdn_estab)) > 0 then 513 else 510 ) and
                   val_unit_form_fp.cdn_empresa =  i-empresa no-lock no-error.
                   
     if not avail val_unit_form_fp then do:
          run pi-acompanhar in h-acomp(input "Falta Multiplo salarial: " + string(i-empresa)).
    
          pause 1.
                   
          next.
     end.

        
        d-mult-sal = val_unit_form_fp.val_calcul_efp.
    
    
     
    find first val_unit_form_fp where val_unit_form_fp.cdn_val_unit_fp =  512 and
               val_unit_form_fp.cdn_empresa =  i-empresa no-lock no-error.
    
    if not avail val_unit_form_fp then do:
     run pi-acompanhar in h-acomp(input "Falta Limite maximo empresa: " + string(i-empresa)).
    
     pause 1.
                   
        next.
    end.
    d-lim-max-cap = val_unit_form_fp.val_calcul_efp.
    d-lim-min-cap = 0.
    
    
    assign v-num-reg-lidos = 0.
   
ASSIGN c-arquivo_txt = session:TEMP-DIRECTORY + "esfp0016_" + string(rh_estab.cdn_estab) + "_" + replace(STRING(TIME,"HH:MM:SS"),":","") + ".csv".

OUTPUT TO VALUE (c-arquivo_txt) NO-CONVERT.   


 
  dt-data-ini = date(i-mes-ref,01,i-ano-ref).
  dt-data-fim = date(i-mes-ref,28,i-ano-ref).
  
  dt-data-fim =   dt-data-fim + 10.
  dt-data-fim =  date(month(  dt-data-fim),01,year(  dt-data-fim)).
  dt-data-fim =   dt-data-fim - 1.
    

PUT  unformatted  string(rh_estab.cdn_estab) + "-" + empresa.nome  ";" "sub 000" SKIP.
PUT  "Ap¢lice:" ";" d-apolice format ">>,>>>,>>9"  SKIP.
PUT  unformatted "Vigància:" ";" string(dt-data-ini,"99/99/9999") + " Ö " + string(dt-data-fim,"99/99/9999") "; Legenda" SKIP.
PUT  unformatted ";;Data Nascimento;Sexo;Estado Civil;;Data Admiss∆o;;M£ltiplo;Taxa;Limite M°nimo	;Limite M†ximo"  SKIP.
PUT  unformatted ";;dd/mm/aaaa;M (masc);S (solteiro);	C (casado);dd/mm/aaaa;;Salarial;;de Capital;de Capital"  SKIP.

 
 
 

PUT unformatted ";;;F (fem);D (desquitado);V (vi£vo);;;" 
d-mult-sal ";" 
d-taxa ";"
d-lim-min-cap ";"
d-lim-max-cap ";"  SKIP.


PUT  ";"  SKIP.

    
PUT  UNFORMATTED
"Nß Matr°cula" ";"
"Nome do Segurado" ";"
"Data Nascimento" ";"
"Data Admiss∆o" ";"
"CPF do Segurado" ";"
"Sexo" ";"
"Estado Civil" ";"
"Ocupaá∆o" ";"
"Sal†rio" ";"
"Capital" ";"
"Pràmio Total" ";"
"Capital Excedente" ";"
"Centro de Custo" ";"
"Data demiss∆o" ";"
"Data Afastamento/ Aposentadoria" ";"
"Motivo do Afastamento/ Aposentadoria" SKIP.

 
 v-num-reg-lidos = 0.     

  FOR EACH funcionario WHERE 
    funcionario.cdn_empresa = rh_estab.cdn_empresa AND
    funcionario.cdn_estab   = rh_estab.cdn_estab  NO-LOCK 
                        BY funcionario.nom_pessoa_fisic:
                          
     if funcionario.dat_desligto_func <> ? then do:
        if funcionario.dat_desligto_func < date(i-mes-ref,1,i-ano-ref ) then next.

     
     end.
     
     
     assign v-num-reg-lidos = v-num-reg-lidos + 1.

            run pi-acompanhar in h-acomp(input "Registros lidos empresa: " + string(rh_estab.cdn_estab) + " - "  + string(v-num-reg-lidos)).


       

     
        ASSIGN valor-seguro = 0.
       
    FIND LAST histor_sal_func OF funcionario NO-LOCK NO-ERROR.
    FIND cargo OF funcionario NO-LOCK NO-ERROR.
    FIND rh_pessoa_fisic OF funcionario NO-LOCK NO-ERROR.
    
    
     FOR EACH movto_calcul_func OF funcionario WHERE
         movto_calcul_func.idi_tip_fp                = 1 AND
         movto_calcul_func.qti_parc_habilit_calc_fp  = 9 AND
         movto_calcul_func.num_ano_refer_fp          = i-ano-ref AND
         movto_calcul_func.num_mes_refer_fp          = i-mes-ref NO-LOCK:
         Do idx = 1 To movto_calcul_func.qti_efp:
            IF movto_calcul_func.cdn_event_fp[idx] = "423" or   
               movto_calcul_func.cdn_event_fp[idx] = "786" THEN
               ASSIGN valor-seguro = valor-seguro + movto_calcul_func.val_calcul_efp[idx].
         END.
     END.
     
     
     if valor-seguro = 0 then next.

    

     ASSIGN d-remun = funcionario.val_salario_atual.

     if funcionario.cdn_categ_sal <> 1 then do:

        find last  histor_sal_func of funcionario no-lock no-error.
    
        if avail histor_sal_func then 
           d-remun = histor_sal_func.val_salario_mensal.
        else
           d-remun = funcionario.val_salario_atual * 220.
     
     end.


     FOR EACH remun_var_func OF funcionario  where funcionario.cdn_empresa <> "420" AND funcionario.cdn_empresa <> "410" NO-LOCK, /*solic-318*/ 

        each remun_var of remun_var_func  where substring(remun_var.des_remun_va,1,2) <> "VP" no-lock.
    
        IF remun_var_func.log_remun_var_func_suspen THEN
            NEXT.
        IF remun_var_func.dat_remun_var_fim <> 12/31/9999 THEN NEXT.
        
        ASSIGN d-remun = d-remun + remun_var_func.val_remun_var.
    END. 

     
/* Lima */        
       If funcionario.cdn_empresa = "320" and funcionario.cdn_estab = "321" and
            funcionario.cdn_funcionario = 999 then
            assign d-remun = 9000.00.
       
       If funcionario.cdn_empresa = "380" and funcionario.cdn_estab = "383" and
            funcionario.cdn_funcionario = 173 then
            assign d-remun = 4003.46.
     
     
       FIND rh_ccusto OF funcionario NO-LOCK NO-ERROR.
       
       b-seguro = d-remun  * d-mult-sal.
       
       if  b-seguro > d-lim-max-cap then
         assign 
             capital-exced =  b-seguro - d-lim-max-cap
             b-seguro      = d-lim-max-cap.
       else
             capital-exced = 0.
       
    put   
        
         funcionario.cdn_funcionario ";"
         funcionario.nom_pessoa_fisic ";"
         funcionario.dat_nascimento ";"
         funcionario.dat_admis_func ";"
         funcionario.cod_id_feder FORM "999.999.999-99" ";"
         
         
         ENTRY(rh_pessoa_fisic.idi_sexo,"M,F")  ";"
         CAPS(ENTRY(rh_pessoa_fisic.idi_estado_civil,"casado,solteiro,desquitado,divorciado,viuvo,separado judicialmente,outros,uniao estˇvel")) 
           FORM "x(15)" ";"
         cargo.des_cargo ";"  
         d-remun        FORM "zzz,zzz,zz9.99" ";"
         b-seguro       FORM "zzz,zzz,zz9.99" ";"
         valor-seguro   FORM "zzz,zzz,zz9.99" ";"
         capital-exced  FORM "zzz,zzz,zz9.99" ";"
         
         funcionario.cod_rh_ccusto ";"
         funcionario.dat_desligto_func ";"

skip .

      
    

end.                        
    
    

 OUTPUT CLOSE.
if v-num-reg-lidos = 0 then do:
    dos silent del value(c-arquivo_txt).
    
    next.

end.


c-arquivo_xls = session:TEMP-DIRECTORY + "Seguro_" + string(rh_estab.cdn_estab) + "_" + replace(STRING(TIME,"HH:MM:SS"),":","") + ".xlsx".

 

    CREATE "Excel.Application" c-excel.
    ASSIGN c-excel:DisplayAlerts = FALSE.

            
    /* cria planilha*/
    c-arquivo_2 = c-arquivo_xls.       
    c-planilha  = c-excel:Workbooks:OPEN(c-arquivo_txt).
    c-planilha:SAVEas(c-arquivo_2,51,,,,,).
             
    c-planilha:CLOSE().

 
          
    ASSIGN c-planilha  = c-excel:Workbooks:OPEN(c-arquivo_2)
    c-relatorio = c-excel:Sheets:item(1).
     
       run pi-acompanhar in h-acomp(input "Aguarde formatando planilha gerada - " + string(rh_estab.cdn_estab)).

         

    RUN pi-salva-planilha.
 run pi-acompanhar in h-acomp(input "Aguarde copiando planilha gerada").
 dos silent copy value(c-arquivo_2) v:\temp.
 dos silent del value(c-arquivo_2) .
 dos silent del value(c-arquivo_txt).
    
   
      c-excel:visible = yes.
       

    RELEASE OBJECT c-relatorio.
    RELEASE OBJECT c-planilha.

    RELEASE OBJECT c-excel.
 end.
 RUN pi-finalizar IN h-acomp.
RETURN 'OK'.



PROCEDURE pi-salva-planilha:
    
 
          
            c-relatorio:range("a8:p8"):Interior:ColorIndex = 37.
            
 
            c-relatorio:range("a8:p8"):Font:Name = "Arial".
             
            c-relatorio:range("a8:p8"):Font:FontStyle = "Negrito".
            c-relatorio:range("a8:p8"):Font:Size = 10.
/*            c-relatorio:range("a8:p8"):Font:ColorIndex = 2.*/
            
            c-relatorio:Rows("8:8"):Autofilter (,,,).

      
            c-relatorio:Columns("c:d"):NumberFormat = "dd/mm/aaaa".
            c-relatorio:Columns("n:n"):NumberFormat = "dd/mm/aaaa".

 
             c-relatorio:Columns("i:l"):NumberFormat = "#.##0,00".
          

            
            c-relatorio:Cells:Select.
            c-relatorio:Cells:EntireColumn:AutoFit.
            c-relatorio:Columns("A:A"):ColumnWidth = 14.
            c-relatorio:range("a2"):Select.

            c-relatorio:range("c3:c3"):Borders(01):Weight = 3.
            c-relatorio:range("g3:g3"):Borders(02):Weight = 3.
            c-relatorio:range("c3:g3"):Borders(03):Weight = 3.
            c-relatorio:range("c3:g3"):Borders(04):Weight = 3.
            c-relatorio:range("c4:g4"):Borders(04):Weight = 3.
            c-relatorio:range("c6:g6"):Borders(04):Weight = 3.
            c-relatorio:range("c4:c6"):Borders(01):Weight = 3.
            c-relatorio:range("d4:d6"):Borders(01):Weight = 3.
            c-relatorio:range("e4:e6"):Borders(01):Weight = 3.
            c-relatorio:range("f5:f6"):Borders(01):Weight = 3.
            c-relatorio:range("g4:g6"):Borders(01):Weight = 3.
            c-relatorio:range("h4:h6"):Borders(01):Weight = 3.
            
            c-relatorio:range("i3:l3"):Borders(04):Weight = 3.
            c-relatorio:range("i5:l5"):Borders(04):Weight = 3.
            c-relatorio:range("i6:l6"):Borders(04):Weight = 3.
            
            c-relatorio:range("i4:i6"):Borders(01):Weight = 3.
            c-relatorio:range("j4:j6"):Borders(01):Weight = 3.
            c-relatorio:range("k4:k6"):Borders(01):Weight = 3.
            c-relatorio:range("l4:l6"):Borders(01):Weight = 3.
            c-relatorio:range("m4:m6"):Borders(01):Weight = 3.
                
            c-relatorio:range("a8:p8"):Borders(01):Weight = 2.
            c-relatorio:range("a8:p8"):Borders(02):Weight = 2.
            c-relatorio:range("a8:p8"):Borders(03):Weight = 2.
            c-relatorio:range("a8:p8"):Borders(04):Weight = 2.


                  
            c-relatorio:Columns("c:d"):NumberFormat = "dd/mm/aaaa".
            c-relatorio:Columns("n:n"):NumberFormat = "dd/mm/aaaa".

            c-relatorio:range("j6:j6"):NumberFormat = "#.##0,0000000".
            
            c-relatorio:range("b1:b3"):Horizontalalignment = 2.

            c-relatorio:range("k2:l2"):Select.
            c-relatorio:range("k2:l2"):Font:FontStyle = "Negrito".
            c-relatorio:range("k2:l2"):Font:Size = 14. 
            c-relatorio:range("k2"):VALUE = "Total Pràmio:".
            c-relatorio:range("l2"):VALUE = "=sum(K:K)".


     c-planilha:SAVE().

END PROCEDURE.


                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  

