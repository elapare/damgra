/********************************************************************************
** Programa: UPC-CD0922-U01A.P                                                 **
** Data....: 26.11.04                                                          **
** Versío..: 1.00.000                                                          **
** OBS.....: UPC - CD0922 - Bot∆o Geraá∆o Autom†tica Produto   
             11.07.05 - Item referencia para o produto                          **
********************************************************************************/

def new global shared var wgh-btGeraProduto   as widget-handle no-undo.

/*Geraá∆o de produto MPS para os itens MILL ROLL de Montenegro*/
for each item no-lock use-index grupo where
         item.ge-codigo    = 41 and
         (item.cod-estabel  = '422' OR item.cod-estabel  = '412') and /*solic-318*/
         item.tipo-con-est < 4:  /*Mill Roll n∆o Ç controlado por referància*/
    
    /*O Mill Roll deve ser utilizado em estrutura do configurado.*/
   
    if not can-find (FIRST estr-mod-cf where
                     estr-mod-cf.it-codigo = item.it-codigo) then next.
    
    /*Para cada Mill Roll gerar um produto*/
    find produto where 
         produto.cd-produto = item.it-codigo exclusive-lock no-error.
    if  not avail produto then do:
        create produto.                      
        assign produto.cd-produto             = item.it-codigo
               produto.tipo-produto           = 2 /*Grupo Produtos*/
               produto.descricao              = item.desc-item
               produto.un                     = item.un
               SUBSTRING(produto.char-1,1,16) = ITEM.it-codigo.
  
        run pi-gera-est-prod (input item.it-codigo,
                              input 0).
    end.
    ELSE DO:
        IF SUBSTRING(produto.char-1,1,16) <> ITEM.it-codigo THEN
            ASSIGN SUBSTRING(produto.char-1,1,16) = ITEM.it-codigo.
    END.

end.

procedure pi-gera-est-prod:
 /*Gerar estrutura de Produto*/
 def input parameter c-produto  as char no-undo.
 def input parameter i-seq      as int  no-undo.

   for each estr-mod-cf no-lock where
            estr-mod-cf.it-codigo = c-produto:
        find first est-prod where
                   est-prod.cd-produto = estr-mod-cf.it-codigo and
                   est-prod.sequencia  = i-seq and
                   est-prod.it-codigo  = estr-mod-cf.mo-codigo exclusive-lock no-error.
        if not avail est-prod then do:
            assign i-seq = i-seq + 1.
            create est-prod.
            assign est-prod.cd-produto = estr-mod-cf.it-codigo 
                   est-prod.sequencia  = i-seq
                   est-prod.it-codigo  = estr-mod-cf.mo-codigo.
        end.
   end.
end procedure.
