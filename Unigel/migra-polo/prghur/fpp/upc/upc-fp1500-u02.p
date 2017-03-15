/*************************************************************************************
**  Programa: upc-fp1500-u02.P
**  Objetivo: Tela dialog para pesquisa por parte do nome do funcionario ou numero PIS
**  Data....: 10/02/2013
**  Versao..: 2.06.001 - Edson - Amgra
**  chamado pelo programa upc-fp1500-u01    
  
**************************************************************************************/
{bf/buffersHCM.i}

def new global shared var v_cdn_empres_usuar   like empresa.ep-codigo        no-undo.
def var i-cdn_estab   like funcionario.cdn_estab no-undo.
def var c-nome        like funcionario.nom_pessoa_fisic  no-undo.
def var c-cod-pis-ini like funcionario.cod_pis no-undo.
def var c-cod-pis-fim like funcionario.cod_pis no-undo.
def var i-estab-ini   like funcionario.cdn_estab no-undo.
def var i-estab-fim   like funcionario.cdn_estab no-undo.
def var i-empresa-ini   like funcionario.cdn_empresa no-undo.
def var i-empresa-fim   like funcionario.cdn_empresa no-undo.

def var h-prog        as handle no-undo.
 
define temp-table tt-func
    field cdn_funcionario   like funcionario.cdn_funcionario 
    field nom_pessoa_fisic  like funcionario.nom_pessoa_fisic 
    field dat_admis_func    like funcionario.dat_admis_func 
    field dat_desligto_func like funcionario.dat_desligto_func 
    field cod_pis           like funcionario.cod_pis 
    field cod_rh_ccusto     like funcionario.cod_rh_ccusto 
    field cdn_empresa       like funcionario.cdn_empresa 
    field cdn_estab         like funcionario.cdn_estab 
    field num_pessoa_fisic  like funcionario.num_pessoa_fisic.
     

assign
   i-empresa-ini = "320"
   i-empresa-fim = STRING({cdp\poloestab.i 420}). /*solic-318*/ 

     
run pi-digita.


procedure pi-digita.

    
      
    
    DEFINE BUTTON gt-bt-cancel AUTO-END-KEY 
         LABEL "&Cancelar" 
         SIZE 10 BY 1
         BGCOLOR 8.
    
    DEFINE BUTTON gt-bt-ok AUTO-GO 
         LABEL "&OK" 
         SIZE 10 BY 1
         BGCOLOR 8.
         
     DEFINE BUTTON gt-bt-pesq  
         LABEL "&Pesquisar" 
         SIZE 10 BY 1
         image file "image\check.bmp"
         BGCOLOR 8.
     
    
    DEFINE RECTANGLE gt-rt-botoes
         EDGE-PIXELS 2 GRAPHIC-EDGE  
         SIZE 100 BY 1.42
         BGCOLOR 7.
         
    DEFINE VARIABLE rs-tp-arquivo AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET horizontal
     RADIO-BUTTONS 
        "Por Parte nome do Funcion rio", 1,
        "Por n£mero de Pis", 2 

     SIZE 45 BY 1.08 NO-UNDO.
             
     

 
      
    DEFINE RECTANGLE gt-rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59 BY 5.50.
    
    DEFINE RECTANGLE gt-rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59 BY 6.
    
    
    DEFINE IMAGE IMAGE-1
         FILENAME "image\im-fir":U
         SIZE 3 BY .88.
    
    DEFINE IMAGE IMAGE-2
         FILENAME "image\im-las":U
         SIZE 3 BY .88.

    DEFINE IMAGE IMAGE-3
         FILENAME "image\im-fir":U
         SIZE 3 BY .88.
    
    DEFINE IMAGE IMAGE-4
         FILENAME "image\im-las":U
         SIZE 3 BY .88.

    DEFINE IMAGE IMAGE-5
         FILENAME "image\im-fir":U
         SIZE 3 BY .88.
    
    DEFINE IMAGE IMAGE-6
         FILENAME "image\im-las":U
         SIZE 3 BY .88.


    DEFINE QUERY qry-func  for  tt-func
    fields (
         tt-func.cdn_empresa        
         tt-func.cdn_estab 
         tt-func.cdn_funcionario  
         tt-func.nom_pessoa_fisic  
         tt-func.dat_admis_func     
         tt-func.dat_desligto_func  
         tt-func.cod_pis            
         tt-func.cod_rh_ccusto                      
         tt-func.num_pessoa_fisic  ) 
    .
    


    DEFINE BROWSE br-func 
     QUERY qry-func
      DISPLAY 
         tt-func.cdn_empresa        
         tt-func.cdn_estab 
         tt-func.cdn_funcionario  
         tt-func.nom_pessoa_fisic FORMAT "x(51)" 
         tt-func.dat_admis_func     
         tt-func.dat_desligto_func  
         tt-func.cod_pis            FORMAT "x(14)"
         tt-func.cod_rh_ccusto                       
         tt-func.num_pessoa_fisic  
          WITH NO-ASSIGN SEPARATORS SIZE 99 BY 14. 
    

    DEFINE FRAME gt-frame-1

 
         i-empresa-ini LABEL "Empresa" AT ROW 1.5 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 4 BY .88
         
         i-empresa-fim no-LABEL  AT ROW 1.5 COL 48 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 4 BY .88
         
         i-estab-ini LABEL "Estabelecimento" AT ROW 2.5 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 4 BY .88
         
         i-estab-fim no-LABEL  AT ROW 2.5 COL 48 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 4 BY .88
         
         
         c-cod-pis-ini LABEL "N£mero do Pis" AT ROW 3.5 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 16 BY .88
         
         c-cod-pis-fim no-LABEL  AT ROW 3.5 COL 48 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 16 BY .88


         c-nome LABEL "Nome ou parte do nome" AT ROW 4.5 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 46 BY .88

         IMAGE-1  AT ROW 1.5 COL 40
         IMAGE-2  AT ROW 1.5 COL 43.57
         IMAGE-3  AT ROW 2.5 COL 40
         IMAGE-4  AT ROW 2.5 COL 43.57
         IMAGE-5  AT ROW 3.5 COL 40
         IMAGE-6  AT ROW 3.5 COL 43.57



        

         rs-tp-arquivo  no-LABEL AT ROW 5.5 COL 17.5 COLON-ALIGNED
        
         gt-bt-pesq  no-LABEL AT ROW 4.5 COL 67.5 COLON-ALIGNED

         br-func at row 6.5 col 2

         gt-bt-ok          AT ROW 22.3 COL 3.14
         gt-bt-cancel      AT ROW 22.3 COL 14             
         gt-rt-botoes      AT ROW 22.0 COL 2
         SPACE(0.28)
         WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE 
             THREE-D SCROLLABLE TITLE "upc-fp1500-u02 - Pesquisa de funcion rios" FONT 1
             DEFAULT-BUTTON gt-bt-ok CANCEL-BUTTON gt-bt-cancel.
   
         ON "value-changed":U OF rs-tp-arquivo IN FRAME gt-frame-1 DO:
                 if rs-tp-arquivo:screen-value  in FRAME gt-frame-1 = "1" then do:
                 
                        assign 
                           c-nome:screen-value in FRAME gt-frame-1 = ""
                           c-cod-pis-ini:screen-value in FRAME gt-frame-1 = ""
                           c-cod-pis-fim:screen-value in FRAME gt-frame-1 = "ZZZZZZZZZZZ" 
                           c-nome:sensitive in FRAME gt-frame-1 = yes
                           c-cod-pis-ini:sensitive  in FRAME gt-frame-1 = no
                           c-cod-pis-fim:sensitive  in FRAME gt-frame-1 = no 
                        .
                       
                 end.
                 else
                 do:
                         assign 
                           c-nome:screen-value in FRAME gt-frame-1 = ""
                           c-cod-pis-ini:screen-value in FRAME gt-frame-1 = ""
                           c-cod-pis-fim:screen-value in FRAME gt-frame-1 = "ZZZZZZZZZZZ" 
                           c-nome:sensitive in FRAME gt-frame-1 = no
                           c-cod-pis-ini:sensitive  in FRAME gt-frame-1 = yes
                           c-cod-pis-fim:sensitive  in FRAME gt-frame-1 = yes 
                        .
    
                 end.
                 
                 
         end.
        
         ON "value-changed" OF c-cod-pis-ini IN FRAME gt-frame-1 DO:
            assign
                c-cod-pis-fim:screen-value in frame gt-frame-1 = c-cod-pis-ini:screen-value in frame gt-frame-1 + "ZZZ".
        
    
         
         
         end.
        
         ON "CHOOSE":U OF gt-bt-pesq IN FRAME gt-frame-1 DO:
        
                empty temp-table tt-func.
                
                if rs-tp-arquivo:screen-value  in FRAME gt-frame-1 = "1" then do:
               
                   
             
                     for each funcionario where 
                                        funcionario.cdn_empresa >= STRING(i-empresa-ini:screen-value  in FRAME gt-frame-1) and
                                        funcionario.cdn_empresa <= STRING(i-empresa-fim:screen-value  in FRAME gt-frame-1) and                     
                                        index(funcionario.nom_pessoa_fisic,c-nome:screen-value  in FRAME gt-frame-1) > 0 and
                                        funcionario.cdn_estab >= STRING(i-estab-ini:screen-value  in FRAME gt-frame-1) and
                                        funcionario.cdn_estab <= STRING(i-estab-fim:screen-value  in FRAME gt-frame-1) no-lock.
                                        
                                        create tt-func.
                                        assign      
                                            tt-func.cdn_funcionario   = funcionario.cdn_funcionario 
                                            tt-func.nom_pessoa_fisic  = funcionario.nom_pessoa_fisic 
                                            tt-func.dat_admis_func    = funcionario.dat_admis_func 
                                            tt-func.dat_desligto_func = funcionario.dat_desligto_func 
                                            tt-func.cod_pis           = funcionario.cod_pis 
                                            tt-func.cod_rh_ccusto     = funcionario.cod_rh_ccusto 
                                            tt-func.cdn_empresa       = funcionario.cdn_empresa 
                                            tt-func.cdn_estab         = funcionario.cdn_estab 
                                            tt-func.num_pessoa_fisic  = funcionario.num_pessoa_fisic.        
                                                                                    
                                            
                
                      end.  
                      
               
             
                   
                
                     
                   
                end.  
                else
                if rs-tp-arquivo:screen-value  in FRAME gt-frame-1 = "2" then do:
                
              
             
                   for each funcionario where  
                                        funcionario.cdn_empresa >= STRING(i-empresa-ini:screen-value  in FRAME gt-frame-1) and
                                        funcionario.cdn_empresa <= STRING(i-empresa-fim:screen-value  in FRAME gt-frame-1) and 
                                        funcionario.cod_pis     >= c-cod-pis-ini:screen-value  in FRAME gt-frame-1 and
                                        funcionario.cod_pis     <= c-cod-pis-fim:screen-value  in FRAME gt-frame-1 and                           
                                        funcionario.cdn_estab   >= STRING(i-estab-ini:screen-value  in FRAME gt-frame-1) and
                                        funcionario.cdn_estab   <= STRING(i-estab-fim:screen-value  in FRAME gt-frame-1)  no-lock.
                                        
                                                            create tt-func.
                                        assign      
                                            tt-func.cdn_funcionario   = funcionario.cdn_funcionario 
                                            tt-func.nom_pessoa_fisic  = funcionario.nom_pessoa_fisic 
                                            tt-func.dat_admis_func    = funcionario.dat_admis_func 
                                            tt-func.dat_desligto_func = funcionario.dat_desligto_func 
                                            tt-func.cod_pis           = funcionario.cod_pis 
                                            tt-func.cod_rh_ccusto     = funcionario.cod_rh_ccusto 
                                            tt-func.cdn_empresa       = funcionario.cdn_empresa 
                                            tt-func.cdn_estab         = funcionario.cdn_estab 
                                            tt-func.num_pessoa_fisic  = funcionario.num_pessoa_fisic.        
                    
                
                   end. 
                   
               
             
                END.
    
                find first tt-func no-error.
                if avail tt-func then do:
                    OPEN QUERY qry-func FOR EACH tt-func.
                    display br-func with frame gt-frame-1.
                end.
            
    
         end.
          ON "return":U OF c-nome IN FRAME gt-frame-1 DO:
             apply "choose" to gt-bt-pesq in frame gt-frame-1.
          end.

         ON "CHOOSE":U OF gt-bt-ok IN FRAME gt-frame-1 DO:
        
                
                
                h-prog = session:first-procedure.
                do while(valid-handle(h-prog:next-sibling)):
                    h-prog = h-prog:next-sibling.
                    if index(h-prog:file-name,"q01py085") > 0 then leave.
                end.
    
    
                
                if not valid-handle(h-prog) then return.
                
                if index(h-prog:file-name,"q01py085") = 0 then return.
                
                find first funcionario where 
                           funcionario.cdn_empresa     = tt-func.cdn_empres and
                           funcionario.cdn_funcionario = tt-func.cdn_funcionario  no-lock.
                
                if avail funcionario  and funcionario.cdn_empresa = v_cdn_empres_usuar then 
    
                      run pi-reposiciona-query in h-prog(rowid(funcionario)).
    
    
         end.
         
          ON "MOUSE-SELECT-DBLCLICK":U OF br-func IN FRAME gt-frame-1 DO:
        
               apply "choose" to gt-bt-ok IN FRAME gt-frame-1.
          end.     
           
         disp   i-empresa-ini  i-empresa-fim i-estab-ini  i-estab-fim c-nome c-cod-pis-ini c-cod-pis-fim  rs-tp-arquivo        
               br-func gt-bt-pesq gt-bt-ok gt-bt-cancel        
               WITH FRAME gt-frame-1. 
         assign 
           c-nome:screen-value in FRAME gt-frame-1 = ""
           c-cod-pis-ini:screen-value in FRAME gt-frame-1 = ""
           c-cod-pis-fim:screen-value in FRAME gt-frame-1 = "ZZZZZZZZZZZ" 
           i-estab-ini:screen-value in FRAME gt-frame-1 = "0"
           i-estab-fim:screen-value in FRAME gt-frame-1 = "999"
           c-nome:sensitive in FRAME gt-frame-1 = yes
           c-cod-pis-ini:sensitive  in FRAME gt-frame-1 = no
           c-cod-pis-fim:sensitive  in FRAME gt-frame-1 = no .
    
        
    
     
     
         ENABLE   i-empresa-ini  i-empresa-fim  i-estab-ini  i-estab-fim c-nome   rs-tp-arquivo
                 gt-bt-pesq br-func gt-bt-ok gt-bt-cancel 
                 WITH FRAME gt-frame-1. 
                 
         apply "entry"  to  c-nome in frame gt-frame-1.       
        
         WAIT-FOR "GO":U OF FRAME gt-frame-1.
    
end procedure.



 

