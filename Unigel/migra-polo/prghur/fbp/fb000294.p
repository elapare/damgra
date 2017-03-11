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
DEF VAR d-valor-217       AS DEC       NO-UNDO.
DEF VAR d-perc-func       AS DEC       NO-UNDO.
Def var d-vl-func as dec no-undo.
Def var d-vl-dep as dec no-undo.
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
   
d-valor-217 = 0.
FOR EACH val_unit_form_fp WHERE 
                val_unit_form_fp.cdn_empresa = bfunciona.cdn_empresa AND
                val_unit_form_fp.cdn_val_unit_fp= 217  NO-LOCK.                

                d-valor-217 = val_unit_form_fp.val_calcul_efp .
END.



d-perc-func = 0.

IF INDEX ("321, 322, 351, 352, 353, 354, 383, 392, 421, 422, 423", bfunciona.cdn_estab) > 0  THEN DO:
    IF d-remun <= 2206.60 THEN
        ASSIGN d-perc-func = 5.
    ELSE
        IF d-remun > 2206.60 AND d-remun <= 3861.55  THEN
               ASSIGN  d-perc-func = 10.
        ELSE
            IF d-remun > 3861.55 AND d-remun <= 5516.50  THEN
                   ASSIGN  d-perc-func = 15.
            ELSE
                IF d-remun > 5516.50   THEN
                       ASSIGN  d-perc-func = 20.
END.

IF bfunciona.cdn_estab = "381"  THEN DO:
    IF d-remun <= 2210.00 THEN
        ASSIGN d-perc-func = 5.
    ELSE
        IF d-remun > 2210.00  AND d-remun <= 3867.50  THEN
               ASSIGN  d-perc-func = 10.
        ELSE
            IF d-remun > 3867.50 AND d-remun <= 5525.00  THEN
                   ASSIGN  d-perc-func = 15.
            ELSE
                IF d-remun > 5525.00   THEN
                       ASSIGN  d-perc-func = 20.
END.
IF INDEX (" 391, 393, 401, 404", bfunciona.cdn_estab) > 0  THEN DO:
    IF d-remun <= 2409.01 THEN
        ASSIGN d-perc-func = 5.
    ELSE
        IF d-remun > 2409.01  AND d-remun <= 4215.77  THEN
               ASSIGN  d-perc-func = 10.
        ELSE
            IF d-remun > 4215.77 AND d-remun <= 6022.52  THEN
                   ASSIGN  d-perc-func = 15.
            ELSE
                IF d-remun > 6022.52   THEN
                       ASSIGN  d-perc-func = 20.
END.


ASSIGN    
d-vl-dep =  ROUND (d-valor-217 * d-perc-func / 100,2)
d-vl-emp =  d-valor-217 - d-vl-dep.


ASSIGN
 /*d-acum-tot[01] =  d-vl-func*/
 d-acum-tot[02] =  d-vl-emp
 d-acum-tot[03] =  d-vl-dep     .

 
