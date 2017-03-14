/***********************************
 ****** Autor: Edson   -  prghur\fbp\fb000501.p
 ****** Objetivo: F¢rmula de C lculo para considerar Itens de Remunera‡Æo
 **********************************************************************/


Def shared var d-acum-tot as dec extent 10 no-undo.
Def shared var i-inf-cd as dec extent 15 no-undo.
def shared var i-cd-benefic like beneficio.cdn_beneficio   no-undo.
def shared var d-vl-benefic like movto_benefic.val_calcul_efp         no-undo.

def shared buffer bmovto_integr_benefic_fp for movto_integr_benefic_fp.
def shared buffer bfunciona for funcionario.
def shared buffer bparam_empres_rh for param_empres_rh.



/**********    inicio programa especifico*********************************/

DEF VAR d-remun           AS DEC       NO-UNDO.
DEF VAR d-remun_polo           AS DEC       NO-UNDO.
DEF VAR d-perc-func       AS DEC       NO-UNDO.
Def var d-vl-func as dec no-undo.
Def var d-vl-emp as dec no-undo.

def var d-mult-sal    as dec no-undo.
def var d-taxa        as dec no-undo.
def var d-lim-max-cap as dec no-undo.

ASSIGN d-remun = bfunciona.val_salario_atual.


if bfunciona.cdn_categ_sal <> 1 then do:

    find last  histor_sal_func of bfunciona no-lock no-error.

    if avail histor_sal_func then 
       d-remun = histor_sal_func.val_salario_mensal.
    else
       d-remun = bfunciona.val_salario_atual * 220.
 
end.
   
d-remun_polo = d-remun.
   
FOR EACH remun_var_func OF bfunciona  where bfunciona.cdn_empresa <> "420" AND bfunciona.cdn_empresa <> "410" NO-LOCK, /*solic-318*/ 

    each remun_var of remun_var_func  where substring(remun_var.des_remun_va,1,2) <> "VP" no-lock.

    IF remun_var_func.log_remun_var_func_suspen THEN
        NEXT.
    IF remun_var_func.dat_remun_var_fim <> 12/31/9999 THEN NEXT.
    
    ASSIGN d-remun = d-remun + remun_var_func.val_remun_var.
END.



ASSIGN d-taxa        = 0
       d-mult-sal    = 0
       d-perc-func   = 0
       d-lim-max-cap = 0.
       
       
FOR  first val_unit_form_fp where 
               val_unit_form_fp.cdn_val_unit_fp =  511 and
               val_unit_form_fp.cdn_empresa     =  bfunciona.cdn_empresa no-lock.
         
    assign d-taxa = val_unit_form_fp.val_calcul_efp / 100000. /* / 100.*/
end.

FOR  first val_unit_form_fp where 
                val_unit_form_fp.cdn_val_unit_fp = (IF bfunciona.cdn_empresa = "420" OR bfunciona.cdn_empresa = "410" THEN 517 ELSE  516) and /*solic-318*/ 
                val_unit_form_fp.cdn_empresa     =  bfunciona.cdn_empresa no-lock.

    ASSIGN d-perc-func = val_unit_form_fp.val_calcul_efp / 100.
end.
FOR first val_unit_form_fp where 
                  val_unit_form_fp.cdn_val_unit_fp =   510 and
                  val_unit_form_fp.cdn_empresa     =  bfunciona.cdn_empresa no-lock.                         
        
    ASSIGN d-mult-sal = val_unit_form_fp.val_calcul_efp.       

end.

FOR first val_unit_form_fp where 
                val_unit_form_fp.cdn_val_unit_fp =  512 and
                val_unit_form_fp.cdn_empresa     =  bfunciona.cdn_empresa no-lock .    
   
     ASSIGN d-lim-max-cap = val_unit_form_fp.val_calcul_efp.
end.


ASSIGN d-remun = d-remun * d-mult-sal.

IF d-remun > d-lim-max-cap THEN 
        ASSIGN d-remun = d-lim-max-cap.



iF bfunciona.cdn_empresa = "420" OR bfunciona.cdn_empresa = "410" then do: /*solic-318*/ 
    ASSIGN d-vl-benefic = truncate(d-remun * d-taxa + 0.005,2)
           d-vl-func    = truncate(d-vl-benefic *  d-perc-func ,2)
           d-vl-emp     = d-vl-benefic - d-vl-func.
end.
else
do:
    ASSIGN d-vl-benefic = truncate(d-remun * d-taxa + 0.005,2)
           d-vl-func    = truncate(d-vl-benefic *  d-perc-func,2)
           d-vl-emp     = d-vl-benefic - d-vl-func.
end.


assign d-acum-tot[01] =  d-vl-func
       d-acum-tot[02] =  d-vl-emp.
            

 



