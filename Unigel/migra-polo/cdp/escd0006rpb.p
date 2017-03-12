
/*------------------------------------------------------------------------
File.............: escd0006rpb.p
Description......: Importa‡Æo de Funcion rios para Requisi‡äes
Input Parameters : 
Output Parameters: 
Author...........: Amgra - Edson
Created..........: 11/01/2013
OBS..............: 
------------------------------------------------------------------------*/
DEFINE NEW GLOBAL SHARED VARIABLE c-seg-usuario AS CHARACTER   NO-UNDO.

if connected ("hcm") then do:
   
      
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
end.

return 'OK'.

/* fim do programa */
