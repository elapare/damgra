/*EPC TRIGGER WRITE SALDO-ESTOQ*/

/***********************************************************************
**  Descricao : Upc WRITE SALDO-ESTOQ
**  Speto
************************************************************************/
/* 01/04/2008 - inclus∆o da rotina de transferencia de paletes de estabelecimento - AMGRA - Edson*/
/* 01/08/2008 - melhora de performance amgra - edson use-index item tabela pallet*/
/* 19/08/2008 - n∆o estava tratando retorno de palete transferido*/
define parameter buffer bsaldo     for saldo-estoq.
define parameter buffer bsaldo-old for saldo-estoq.
define buffer if-ped-venda for espmulti.if-ped-venda.
DEFINE BUFFER b-pallet FOR pallet.
DEFINE BUFFER b-it-pallet FOR it-pallet.
DEFINE VARIABLE r-pallet AS ROWID      NO-UNDO.
    
DEFINE VARIABLE l-retorno AS LOGICAL    NO-UNDO.
    
IF (PROGRAM-NAME(3) + PROGRAM-NAME(4) + PROGRAM-NAME(5) + PROGRAM-NAME(6) + PROGRAM-NAME(7) + PROGRAM-NAME(8)) MATCHES "*re1005*" THEN
        RUN pi-transf-pallet.

find first item 
    where item.it-codigo = bsaldo.it-codigo NO-LOCK no-error. 
IF AVAIL ITEM  THEN DO:

    IF   ITEM.tipo-con-est > 2 THEN DO:
         
    
       /*Cria ou atualiza a extensao de saldo-estoque*/
       IF NOT CAN-FIND(ext-saldo-estoq
            where ext-saldo-estoq.cod-depos   = bsaldo.cod-depos
              and ext-saldo-estoq.cod-estabel = bsaldo.cod-estabel
              and ext-saldo-estoq.cod-localiz = bsaldo.cod-localiz
              and ext-saldo-estoq.lote        = bsaldo.lote
              and ext-saldo-estoq.it-codigo   = bsaldo.it-codigo
              and ext-saldo-estoq.cod-refer   = bsaldo.cod-refer) THEN DO:
      
          create ext-saldo-estoq.
          assign ext-saldo-estoq.cod-depos   = bsaldo.cod-depos  
                 ext-saldo-estoq.cod-estabel = bsaldo.cod-estabel
                 ext-saldo-estoq.cod-localiz = bsaldo.cod-localiz
                 ext-saldo-estoq.lote        = bsaldo.lote       
                 ext-saldo-estoq.it-codigo   = bsaldo.it-codigo  
                 ext-saldo-estoq.cod-refer = bsaldo.cod-refer.
        
       END. /* se n∆o tem ext-saldo-estoq */
       find ext-saldo-estoq
            where ext-saldo-estoq.cod-depos   = bsaldo.cod-depos
              and ext-saldo-estoq.cod-estabel = bsaldo.cod-estabel
              and ext-saldo-estoq.cod-localiz = bsaldo.cod-localiz
              and ext-saldo-estoq.lote        = bsaldo.lote
              and ext-saldo-estoq.it-codigo   = bsaldo.it-codigo
              and ext-saldo-estoq.cod-refer   = bsaldo.cod-refer EXCLUSIVE-LOCK NO-ERROR.  


       if avail ext-saldo-estoq then do:
       
      
           
           if bsaldo.qtidade-atu - (bsaldo.qt-aloc-ped + 
                                bsaldo.qt-aloc-prod +
                                bsaldo.qt-alocada) <> 0 then
                 assign ext-saldo-estoq.log-saldo = yes.
           else
                 assign ext-saldo-estoq.log-saldo = no.
       END.
     
    
       /* Cria o relacionamento entre o item e o lote             */
       /* Somente para itens com controle por Lote e ReferÍncia   */
       
       find first estabelec where estabelec.cod-estabel = bsaldo.cod-estabel no-lock no-error.
       
       if avail estabelec and (estabelec.ep-codigo = "420" or estabelec.ep-codigo = "430" OR estabelec.ep-codigo = "410" or estabelec.ep-codigo = "440" ) then do:
       
           IF NOT CAN-FIND(first lote-item
                 where lote-item.it-codigo = bsaldo.it-codigo
                      and lote-item.lote      = bsaldo.lote) THEN DO:
                                
                    create lote-item.
                    assign lote-item.it-codigo    = bsaldo.it-codigo 
                           lote-item.lote         = bsaldo.lote
                           lote-item.dt-vali-lote = 12/31/9999.
        
           END.  /* se na‰ tem lote-item */
       
   
           /*Cria os componentes da folha para o lote*/
           if item.cd-folh-lote <> "" then do:
               
               IF NOT CAN-FIND(FIRST lote-carac-tec 
                                where lote-carac-tec.it-codigo = bsaldo.it-codigo 
                                and   lote-carac-tec.lote      = bsaldo.lote) THEN DO:
          
                    find folh-espec where folh-espec.cd-folha = item.cd-folh-lote no-lock no-error.
                    IF AVAIL folh-espec THEN DO:
                
                       for each comp-folh no-lock
                         where comp-folh.cd-folha = folh-espec.cd-folha:
    
                          IF comp-folh.cd-comp  = "IDFLUID" AND 
                             comp-folh.cd-folha = "REC TNT" THEN NEXT.
                    
                          find lote-carac-tec 
                                where lote-carac-tec.it-codigo = bsaldo.it-codigo 
                                and   lote-carac-tec.lote      = bsaldo.lote
                                and   lote-carac-tec.cd-folha  = comp-folh.cd-folha 
                                and   lote-carac-tec.cd-comp   = comp-folh.cd-comp exclusive-lock no-error.
                          if not available lote-carac-tec then do:
                              create lote-carac-tec.
                              assign lote-carac-tec.it-codigo   = bsaldo.it-codigo
                                     lote-carac-tec.lote        = bsaldo.lote
                                     lote-carac-tec.cd-folha    = comp-folh.cd-folha 
                                     lote-carac-tec.cd-comp     = comp-folh.cd-comp 
                                     lote-carac-tec.nr-tabela   = comp-folh.nr-tabela
                                     lote-carac-tec.tipo-result = comp-folh.tipo-result.
                              if comp-folh.tipo-result = 1 then
                                 assign lote-carac-tec.vl-result = 0.                      
        
                          end. /* se vai criar caracteristica */    
                       end. /* end do for each*/
        
                    END. /* se a folha do item existe */
        
        
               END. /* se n∆o tem lote caracteristica */
                   
           END. /* se tem folha */
            
       end.
    
    END. /* se Ç controlado por lote */ 
    
END. /* se existe item */
RETURN "OK".

PROCEDURE pi-transf-pallet. /* rotina para criar registro de paletes transferidos entre estabelecimentos */
    DEFINE VARIABLE  salva-peso-liquido AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE  salva-peso-bruto   AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE  salva-nr-bobinas   AS integer    NO-UNDO.

    FIND FIRST pallet WHERE pallet.nr-pallet = bsaldo.lote AND
                            pallet.it-codigo = bsaldo.it-codigo AND
                            pallet.cod-estabel = bsaldo.cod-estabel NO-LOCK NO-ERROR.
    ASSIGN l-retorno = NO.

    IF AVAIL PALLET THEN  DO:
        ASSIGN  r-pallet = rowid(pallet)
                l-retorno = YES.

    END.
       


    FIND FIRST pallet WHERE pallet.nr-pallet = bsaldo.lote AND
                            pallet.it-codigo = bsaldo.it-codigo AND
                            pallet.cod-estabel <> bsaldo.cod-estabel AND
                            pallet.situacao = 2 USE-INDEX ITEM EXCLUSIVE-LOCK NO-ERROR.
   

    IF AVAIL pallet THEN DO:

        IF NOT l-retorno THEN DO:
            ASSIGN salva-peso-liquido = pallet.peso-liquido 
                   salva-peso-bruto   = pallet.peso-bruto   
                   salva-nr-bobinas   = pallet.nr-bobinas   .
    
    
            FIND b-pallet
                WHERE b-pallet.cod-estabel = bsaldo.cod-estabel
                AND   b-pallet.it-codigo   = bsaldo.it-codigo
                AND   b-pallet.nr-pallet   = bsaldo.lote
                   NO-ERROR.
            IF NOT AVAIL b-pallet THEN DO:
                /*** criando o pallet no estabelecimento destino ***/
                CREATE b-pallet.
                BUFFER-COPY pallet EXCEPT cod-estabel TO b-pallet. 
                ASSIGN b-pallet.cod-estabel = bsaldo.cod-estabel
                       b-pallet.cod-refer   = bsaldo.cod-refer.
            END.
            /*** criando os itens do pallet ***/
             
            FOR EACH it-pallet NO-LOCK
                WHERE it-pallet.cod-estabel = pallet.cod-estabel
                AND   it-pallet.it-codigo   = pallet.it-codigo
                AND   it-pallet.nr-pallet   = pallet.nr-pallet:
            
               
                FIND FIRST b-it-pallet 
                    WHERE b-it-pallet.cod-estabel = bsaldo.cod-estabel
                    AND   b-it-pallet.it-codigo   = it-pallet.it-codigo
                    AND   b-it-pallet.nr-pallet   = it-pallet.nr-pallet
                    AND   b-it-pallet.lote-bobina = it-pallet.lote-bobina
                    EXCLUSIVE-LOCK NO-ERROR.
    
                IF NOT AVAIL b-it-pallet THEN DO:
                    CREATE b-it-pallet.
                    BUFFER-COPY it-pallet EXCEPT cod-estabel TO b-it-pallet.
                    ASSIGN b-it-pallet.cod-estabel = bsaldo.cod-estabel.
                END.
                
            END.
        
        
    
            ASSIGN pallet.situacao = 3. /** TRANSFERIDO **/
        
            assign b-pallet.peso-liquido      = salva-peso-liquido 
                   b-pallet.peso-bruto        = salva-peso-bruto   
                   b-pallet.nr-bobinas        = salva-nr-bobinas  . 
                   
            if b-pallet.nr-pedido <> 0 then do:   /*unigel comercial - Edson para trocar par ao pedido final*/
                FOR FIRST ped-venda WHERE  
                                      ped-venda.nr-pedido  = b-pallet.nr-pedido NO-LOCK,
                   FIRST if-ped-venda WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido NO-LOCK.
   
                   b-pallet.nr-pedido =  if-ped-venda.nr-pedido-relac.

                END.
            end.       
        END.
        ELSE DO:   /* retorno de transferencia */

            ASSIGN pallet.situacao = 3. /** TRANSFERIDO **/

            FIND b-pallet WHERE ROWID(b-pallet) = r-pallet NO-ERROR.

            IF AVAIL b-pallet THEN
                 ASSIGN b-pallet.situacao = 2.  /** paletizado*/


        END.
    
    END.
       
    
END PROCEDURE.
