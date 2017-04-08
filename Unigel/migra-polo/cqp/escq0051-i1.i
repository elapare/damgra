/*----------------------------------------------------------------------
   Include para a gera‡Æo da planilha excel do laudo de qualidade
   escq0051-i1.i 
---------------------------------------------------------------------*/
   
PROCEDURE pi-gera-planilha.

    IF am-cq-laudo.int-1 = 0 THEN DO:     /* Modelo Normal */


             /* Rotina para buscar validade de filmes na fam¡lia do produto */

             ASSIGN dias-jr = 180
                     mes-valid-jr  = 6.

             FIND FIRST ITEM WHERE
                 ITEM.it-codigo = am-cq-laudo.it-codigo NO-LOCK NO-ERROR.

             IF AVAIL ITEM THEN DO:

                 FIND FIRST am-cp-valid-familia WHERE
                     am-cp-valid-familia.fm-codigo = item.fm-codigo
                     NO-LOCK NO-ERROR.
                
                 IF AVAIL am-cp-valid-familia THEN DO:

                     IF am-cp-valid-familia.qtd-meses = 12 THEN
                         ASSIGN dias-jr = 365.
                     ELSE
                         ASSIGN dias-jr = (am-cp-valid-familia.qtd-meses * 30).

                     ASSIGN mes-valid-jr = am-cp-valid-familia.qtd-meses.

                
                 END.

             END.

             FIND FIRST nota-fiscal WHERE
                 nota-fiscal.cod-estabel = am-cq-laudo.cod-estabel   AND
                 nota-fiscal.serie       = substring(am-cq-laudo.char-1,1,5) AND
                 nota-fiscal.nr-nota-fis = am-cq-laudo.nr-nota-fis 
                 NO-LOCK NO-ERROR.
                 
             if not avail nota-fiscal then DO:

                 FIND FIRST nota-fiscal WHERE
                 nota-fiscal.cod-estabel = am-cq-laudo.cod-estabel   AND
                 nota-fiscal.serie       = "22"                      AND
                 nota-fiscal.nr-nota-fis = am-cq-laudo.nr-nota-fis 
                 NO-LOCK NO-ERROR.
                 
                 if not avail nota-fiscal then 
                      
                 FIND FIRST nota-fiscal WHERE
                 nota-fiscal.cod-estabel = am-cq-laudo.cod-estabel   AND
                 nota-fiscal.serie       = "20"                      AND
                 nota-fiscal.nr-nota-fis = am-cq-laudo.nr-nota-fis 
                 NO-LOCK NO-ERROR.

             END. 

             IF AVAIL nota-fiscal THEN DO:
               
                /*kraft*/
                IF nota-fiscal.nome-ab-cli = "KRAFT FOODS" THEN
                    ASSIGN dias-jr = 180
                           mes-valid-jr = 6
                           c-relatorio:range("A" + string(15)):value = "Lotes:".





                ASSIGN /* c-relatorio:range("F" + STRING(12)):VALUE =  nota-fiscal.dt-emis-nota */
                       dt-validade-jr = nota-fiscal.dt-emis-nota + dias-jr 
                  /*   c-relatorio:range("G" + STRING(13)):VALUE = dt-validade-jr */
                       c-relatorio:range("F" + STRING(13)):VALUE = STRING(mes-valid-jr, ">9") + " meses ap¢s produ‡Æo / months after production".



            END.

           
             /* ----------------------------------------------------------- */






        ASSIGN c-relatorio:range("D" + STRING(4)):VALUE = am-cq-laudo.nr-laudo.
    
        /* cabe‡alho do excel */
    
         ASSIGN i-linha = 7.
    
         IF am-cq-laudo.tipo-laudo = 2 THEN
            ASSIGN c-relatorio:range("B" + STRING(i-linha)):VALUE = "(  X  )".
    
         IF am-cq-laudo.tipo-laudo = 1 THEN
            ASSIGN c-relatorio:range("E" + STRING(i-linha)):VALUE = "     (  X  )".
    
    
         ASSIGN c-relatorio:range("C" + "09"):VALUE = am-cq-laudo.nome-abrev
                c-relatorio:range("C" + "11"):VALUE = am-cq-laudo.qtd-bobinas
                c-relatorio:range("C" + "13"):VALUE = am-cq-laudo.peso-liq.
    
         ASSIGN c-relatorio:range("C" + "10"):VALUE = am-cq-laudo.it-codigo.
         
         ASSIGN c-relatorio:range("C" + "12"):VALUE = am-cq-laudo.nr-nota-fis.
         ASSIGN c-relatorio:range("B" + "14"):VALUE = am-cq-laudo.pedido.
    
         ASSIGN c-relatorio:range("B" + "17"):VALUE = am-cq-laudo.larg.
    
         ASSIGN c-relatorio:range("B" + "19"):VALUE = am-cq-laudo.diex.
    
         ASSIGN c-relatorio:range("B" + "21"):VALUE = am-cq-laudo.diin.
    /*
         ASSIGN c-relatorio:range("B" + "23"):VALUE = am-cq-laudo.pallet.
    */     
         FOR FIRST polo-laudo-cliente WHERE
            polo-laudo-cliente.nome-abrev = am-cq-laudo.nome-abrev NO-LOCK.
                IF substring(polo-laudo-cliente.char-1,101,1) = "3" THEN DO:             
                   ASSIGN c-relatorio:range("A" + "16"):VALUE = "Largura Nominal (mm) / Roll Width (in):".
                   ASSIGN c-relatorio:range("A" + "18"):VALUE = "Diƒmetro Externo Nominal (mm) / Roll diameter (in):".
                   ASSIGN c-relatorio:range("A" + "20"):VALUE = "Diƒmetro N£cleo Nominal (mm) / Core Size (in):".
                END.
                ELSE
                IF substring(polo-laudo-cliente.char-1,101,1) = "2" THEN DO:             
                   ASSIGN c-relatorio:range("A" + "16"):VALUE = "Roll Width (in):".
                   ASSIGN c-relatorio:range("A" + "18"):VALUE = "Roll diameter (in):".
                   ASSIGN c-relatorio:range("A" + "20"):VALUE = "Core Size (in):".
                END.
         END.


         /* kraft e outros clientes que precisam de data de produ‡Æo das bobinas */

         RUN pi-dt-prod-bobinas.

         ASSIGN i-linha = 59 + linhas-novas.
    
         ASSIGN c-relatorio:range("B" + STRING(i-linha)):VALUE = am-cq-laudo.nome-responsavel.
    
         ASSIGN c-relatorio:range("F" + STRING(i-linha)):VALUE = am-cq-laudo.dt-criacao.
         
         ASSIGN c-relatorio:range("B" + string(56 + linhas-novas)):VALUE = am-cq-laudo.observacao.
         
         IF am-cq-laudo.it-codigo = "20TFW30" and 
           (am-cq-laudo.nome-abrev = "M DIAS BA" or
            am-cq-laudo.nome-abrev = "M DIAS BRANC" or
            am-cq-laudo.nome-abrev = "M. Dias BA" or
            am-cq-laudo.nome-abrev = "M.Dias BRAN" or
            am-cq-laudo.cod-emitente = 17548 or
            am-cq-laudo.cod-emitente = 17515) THEN
            ASSIGN c-relatorio:range("B" + string(56 + linhas-novas)):VALUE = string("ET.8.2.129.DENPB.R03  " + am-cq-laudo.observacao).
         
         if am-cq-laudo.zint <> "" then
            ASSIGN c-relatorio:range("C" + string(25 + linhas-novas)):VALUE = string("Film: " + trim(am-cq-laudo.zint)).

         ASSIGN c-relatorio:range("B" + string(25 + linhas-novas)):VALUE = pedido-cliente.

    
         ASSIGN i-linha = 27 + linhas-novas.

         FOR EACH am-cq-result-laudo WHERE
             am-cq-result-laudo.nr-laudo = am-cq-laudo.nr-laudo
             no-LOCK.

             ASSIGN decimais-jr = am-cq-result-laudo.nr-decimais.

             ASSIGN i-linha = i-linha + 1.
    
              ASSIGN c-relatorio:range("A" + STRING(i-linha)):VALUE = am-cq-result-laudo.descricao.

              ASSIGN media-result-jr = am-cq-result-laudo.media-result.
    
              IF decimais-jr = 0 THEN
                 ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9").
                ELSE
                  IF decimais-jr = 1 THEN
                     ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9").
                  ELSE
                      IF decimais-jr = 2 THEN
                         ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.99").
                      ELSE
                          IF decimais-jr = 3 THEN
                             ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.999").
                          ELSE
                              IF decimais-jr > 3 THEN
                                 ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9999").
    
    

              IF am-cq-result-laudo.cod-exame = 2006 AND
                  (am-cq-result-laudo.cod-comp = 73 OR
                   am-cq-result-laudo.cod-comp = 74) THEN DO:

                  IF media-result-jr = 10 THEN
                      ASSIGN media-result-jr-x = "BOA".
                      ELSE
                          ASSIGN media-result-jr-x = "RUIM".

              END.


              IF am-cq-result-laudo.cod-exame = 2006 AND
                  am-cq-result-laudo.cod-comp = 38   AND
                  am-cq-laudo.nome-abrev = "peeqflex" THEN DO:

                     ASSIGN media-result-jr-x = "BOA".

              END.



              ASSIGN c-relatorio:range("D" + STRING(i-linha)):VALUE = TRIM (media-result-jr-x).
    
                 ASSIGN media-jr2    = media-result-jr
                        media-result-jr = am-cq-result-laudo.espec-alvo.
    
              IF decimais-jr = 0 THEN
                 ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9").
                ELSE
                  IF decimais-jr = 1 THEN
                     ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9").
                  ELSE
                      IF decimais-jr = 2 THEN
                         ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.99").
                      ELSE
                          IF decimais-jr = 3 THEN
                             ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.999").
                          ELSE
                              IF decimais-jr > 3 THEN
                                 ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9999").
    

              IF am-cq-result-laudo.cod-exame = 2006 AND
                  (am-cq-result-laudo.cod-comp = 73 OR
                   am-cq-result-laudo.cod-comp = 74) THEN 
                  ASSIGN media-result-jr-x = "BOA".


              IF am-cq-result-laudo.cod-exame = 2006 AND
                 am-cq-result-laudo.cod-comp = 38   AND
                 am-cq-laudo.nome-abrev = "peeqflex" THEN DO:

                     ASSIGN media-result-jr-x = "BOA".

              END.

    
              ASSIGN c-relatorio:range("E" + STRING(i-linha)):VALUE = TRIM (media-result-jr-x).

              ASSIGN unidade-jr = am-cq-result-laudo.unidade.
    
              IF unidade-jr = "g/m2" THEN unidade-jr = "g/mý".
              
              ASSIGN c-relatorio:range("F" + STRING(i-linha)):VALUE = unidade-jr 
                     c-relatorio:range("G" + STRING(i-linha)):VALUE = am-cq-result-laudo.metodo.

         END.  

    
    END. /* Fim do Modelo normal */

    /*
    IF am-cq-laudo.int-1 = 2 THEN DO:     /* Modelo Mobil */

        ASSIGN c-relatorio:range("H" + STRING(2)):VALUE = am-cq-laudo.nr-laudo.


        ASSIGN c-relatorio:range("E" + STRING(40)):VALUE = am-cq-laudo.nome-responsavel.

        ASSIGN c-relatorio:range("E" + STRING(41)):VALUE = am-cq-laudo.dt-criacao.

        FIND FIRST emitente WHERE
            emitente.cod-emitente = am-cq-laudo.cod-emitente
            NO-LOCK NO-ERROR.

        IF AVAIL emitente THEN
           ASSIGN c-relatorio:range("C" + "04"):VALUE = emitente.nome-emit
                  c-relatorio:range("C" + "05"):VALUE = emitente.endereco
                  c-relatorio:range("C" + "06"):VALUE = (emitente.cidade + " - " + emitente.estado).
        ELSE
            ASSIGN c-relatorio:range("C" + "04"):VALUE = ""
                   c-relatorio:range("C" + "05"):VALUE = ""
                   c-relatorio:range("C" + "06"):VALUE = "".



        ASSIGN c-relatorio:range("I" + "09"):VALUE = am-cq-laudo.peso-liq.


        ASSIGN c-relatorio:range("C" + "11"):VALUE = am-cq-laudo.it-codigo.

        ASSIGN c-relatorio:range("I" + "06"):VALUE = am-cq-laudo.zint.
        ASSIGN c-relatorio:range("I" + "04"):VALUE = am-cq-laudo.dt-criacao.
        ASSIGN c-relatorio:range("I" + "07"):VALUE = am-cq-laudo.pedido.

        ASSIGN c-relatorio:range("C" + "12"):VALUE = am-cq-laudo.larg.

        ASSIGN z = num-entries(trim(am-cq-laudo.larg)," ")
               larg-pol-x = ""
               z1 = 0.

        IF z > 0 THEN DO:

            DO z1 = 1 TO z.

                ASSIGN larg-pol-x1 = STRING(DEC(ENTRY(z1,am-cq-laudo.larg," ")) / 25.4,">>9.99").

                ASSIGN larg-pol-x = larg-pol-x + larg-pol-x1 + " ".

            END.

        END.

        ASSIGN c-relatorio:range("C" + "13"):VALUE = larg-pol-x.

        ASSIGN c-relatorio:range("I" + "05"):VALUE = pedido-cliente.

        ASSIGN i-linha = 16.

        FOR EACH am-cq-result-laudo WHERE
            am-cq-result-laudo.nr-laudo = am-cq-laudo.nr-laudo
            no-LOCK.

            ASSIGN decimais-jr = am-cq-result-laudo.nr-decimais.

            ASSIGN i-linha = i-linha + 1.

             ASSIGN c-relatorio:range("B" + STRING(i-linha)):VALUE = am-cq-result-laudo.descricao.

             ASSIGN media-result-jr = am-cq-result-laudo.media-result.

             IF decimais-jr = 0 THEN
                ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9").
               ELSE
                 IF decimais-jr = 1 THEN
                    ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9").
                 ELSE
                     IF decimais-jr = 2 THEN
                        ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.99").
                     ELSE
                         IF decimais-jr = 3 THEN
                            ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.999").
                         ELSE
                             IF decimais-jr > 3 THEN
                                ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9999").
    

              IF am-cq-result-laudo.cod-exame = 2006 AND
                  (am-cq-result-laudo.cod-comp = 73 OR
                   am-cq-result-laudo.cod-comp = 74) THEN DO:

                  IF media-result-jr = 10 THEN
                      ASSIGN media-result-jr-x = "BOA".
                      ELSE
                          ASSIGN media-result-jr-x = "RUIM".

              END.


             ASSIGN c-relatorio:range("G" + STRING(i-linha)):VALUE = TRIM (media-result-jr-x).

             ASSIGN media-jr2    = media-result-jr
                    media-result-jr = am-cq-result-laudo.espec-min.

             IF decimais-jr = 0 THEN
                ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9").
               ELSE
                 IF decimais-jr = 1 THEN
                    ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9").
                 ELSE
                     IF decimais-jr = 2 THEN
                        ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.99").
                     ELSE
                         IF decimais-jr = 3 THEN
                            ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.999").
                         ELSE
                             IF decimais-jr > 3 THEN
                                ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9999").

             ASSIGN c-relatorio:range("J" + STRING(i-linha)):VALUE = TRIM (media-result-jr-x).


             ASSIGN media-result-jr = am-cq-result-laudo.menor-result.

             IF decimais-jr = 0 THEN
                ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9").
               ELSE
                 IF decimais-jr = 1 THEN
                    ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9").
                 ELSE
                     IF decimais-jr = 2 THEN
                        ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.99").
                     ELSE
                         IF decimais-jr = 3 THEN
                            ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.999").
                         ELSE
                             IF decimais-jr > 3 THEN
                                ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9999").

             ASSIGN c-relatorio:range("H" + STRING(i-linha)):VALUE = TRIM (media-result-jr-x).



             ASSIGN media-result-jr = am-cq-result-laudo.maior-result.

             IF decimais-jr = 0 THEN
                ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9").
               ELSE
                 IF decimais-jr = 1 THEN
                    ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9").
                 ELSE
                     IF decimais-jr = 2 THEN
                        ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.99").
                     ELSE
                         IF decimais-jr = 3 THEN
                            ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.999").
                         ELSE
                             IF decimais-jr > 3 THEN
                                ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9999").

             ASSIGN c-relatorio:range("I" + STRING(i-linha)):VALUE = TRIM (media-result-jr-x).

             ASSIGN c-relatorio:range("F" + STRING(i-linha)):VALUE = am-cq-result-laudo.qtd-result.



             ASSIGN media-result-jr = am-cq-result-laudo.espec-max.

             IF decimais-jr = 0 THEN
                ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9").
               ELSE
                 IF decimais-jr = 1 THEN
                    ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9").
                 ELSE
                     IF decimais-jr = 2 THEN
                        ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.99").
                     ELSE
                         IF decimais-jr = 3 THEN
                            ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.999").
                         ELSE
                             IF decimais-jr > 3 THEN
                                ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9999").

             ASSIGN c-relatorio:range("K" + STRING(i-linha)):VALUE = TRIM (media-result-jr-x).

             ASSIGN unidade-jr = am-cq-result-laudo.unidade.

             IF unidade-jr = "g/m2" THEN unidade-jr = "g/mý".

             ASSIGN c-relatorio:range("E" + STRING(i-linha)):VALUE = unidade-jr 
                    c-relatorio:range("D" + STRING(i-linha)):VALUE = am-cq-result-laudo.metodo.

        END.  


    END. /* Fim do Modelo Mobil */
    */

    IF am-cq-laudo.int-1 = 2 THEN DO:     /* Modelo Souza Cruz */

        ASSIGN c-relatorio:range("E" + STRING(6)):VALUE = am-cq-laudo.nr-laudo.
    
        /* cabe‡alho do excel */
    
         ASSIGN i-linha = 9.
    
         IF am-cq-laudo.tipo-laudo = 2 THEN
            ASSIGN c-relatorio:range("C" + STRING(i-linha)):VALUE = "(  X  )".
    
         IF am-cq-laudo.tipo-laudo = 1 THEN
            ASSIGN c-relatorio:range("G" + STRING(i-linha)):VALUE = "     (  X  )".
    
         ASSIGN i-linha = 49.
    
         ASSIGN c-relatorio:range("A" + STRING(i-linha)):VALUE = am-cq-laudo.nome-responsavel.
    
         ASSIGN c-relatorio:range("H" + STRING(i-linha)):VALUE = am-cq-laudo.dt-criacao.
         
         ASSIGN c-relatorio:range("A" + "44"):VALUE = am-cq-laudo.observacao.
    
         ASSIGN c-relatorio:range("C" + "11"):VALUE = am-cq-laudo.nome-abrev
                c-relatorio:range("C" + "13"):VALUE = am-cq-laudo.qtd-bobinas.
    
         ASSIGN c-relatorio:range("C" + "12"):VALUE = am-cq-laudo.it-codigo.
         ASSIGN c-relatorio:range("C" + "14"):VALUE = am-cq-laudo.zint.
    
         ASSIGN c-relatorio:range("H" + "14"):VALUE = am-cq-laudo.nr-nota-fis.
         ASSIGN c-relatorio:range("H" + "11"):VALUE = am-cq-laudo.pedido.
    
         ASSIGN c-relatorio:range("H" + "12"):VALUE = am-cq-laudo.larg.
    
         ASSIGN c-relatorio:range("H" + "13"):VALUE = am-cq-laudo.diex.
    
         ASSIGN c-relatorio:range("H" + "15"):VALUE = am-cq-laudo.diin.
    
         ASSIGN c-relatorio:range("C" + "17"):VALUE = am-cq-laudo.pallet.
         
    
         ASSIGN i-linha = 23.

         FOR EACH am-cq-result-laudo WHERE
             am-cq-result-laudo.nr-laudo = am-cq-laudo.nr-laudo
             no-LOCK.

             ASSIGN decimais-jr = am-cq-result-laudo.nr-decimais.

             ASSIGN i-linha = i-linha + 1.

             IF i-linha > 39 THEN NEXT.
    
              ASSIGN c-relatorio:range("A" + STRING(i-linha)):VALUE = am-cq-result-laudo.descricao.

              ASSIGN media-result-jr = am-cq-result-laudo.media-result.
    
              IF decimais-jr = 0 THEN
                 ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9").
                ELSE
                  IF decimais-jr = 1 THEN
                     ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9").
                  ELSE
                      IF decimais-jr = 2 THEN
                         ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.99").
                      ELSE
                          IF decimais-jr = 3 THEN
                             ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.999").
                          ELSE
                              IF decimais-jr > 3 THEN
                                 ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9999").
    

              IF am-cq-result-laudo.cod-exame = 2006 AND
                  (am-cq-result-laudo.cod-comp = 73 OR
                   am-cq-result-laudo.cod-comp = 74) THEN DO:

                  IF media-result-jr = 10 THEN
                      ASSIGN media-result-jr-x = "BOA".
                      ELSE
                          ASSIGN media-result-jr-x = "RUIM".

              END.

              ASSIGN c-relatorio:range("D" + STRING(i-linha)):VALUE = TRIM (media-result-jr-x).
    
                 ASSIGN media-jr2    = media-result-jr
                        media-result-jr = am-cq-result-laudo.espec-alvo.
    
              IF decimais-jr = 0 THEN
                 ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9").
                ELSE
                  IF decimais-jr = 1 THEN
                     ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9").
                  ELSE
                      IF decimais-jr = 2 THEN
                         ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.99").
                      ELSE
                          IF decimais-jr = 3 THEN
                             ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.999").
                          ELSE
                              IF decimais-jr > 3 THEN
                                 ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9999").
    
    

              IF am-cq-result-laudo.cod-exame = 2006 AND
                  (am-cq-result-laudo.cod-comp = 73 OR
                   am-cq-result-laudo.cod-comp = 74) THEN 
                  ASSIGN media-result-jr-x = "BOA".


              ASSIGN c-relatorio:range("G" + STRING(i-linha)):VALUE = TRIM (media-result-jr-x).

              
              ASSIGN media-result-jr = am-cq-result-laudo.maior-result.

              IF decimais-jr = 0 THEN
                 ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9").
                ELSE
                  IF decimais-jr = 1 THEN
                     ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9").
                  ELSE
                      IF decimais-jr = 2 THEN
                         ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.99").
                      ELSE
                          IF decimais-jr = 3 THEN
                             ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.999").
                          ELSE
                              IF decimais-jr > 3 THEN
                                 ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9999").



              ASSIGN c-relatorio:range("E" + STRING(i-linha)):VALUE = TRIM (media-result-jr-x).
                 
                 
              ASSIGN media-result-jr = am-cq-result-laudo.menor-result.
              
              IF decimais-jr = 0 THEN
                 ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9").
                ELSE
                  IF decimais-jr = 1 THEN
                     ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9").
                  ELSE
                      IF decimais-jr = 2 THEN
                         ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.99").
                      ELSE
                          IF decimais-jr = 3 THEN
                             ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.999").
                          ELSE
                              IF decimais-jr > 3 THEN
                                 ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9999").



              ASSIGN c-relatorio:range("F" + STRING(i-linha)):VALUE = TRIM (media-result-jr-x).
              
              
              ASSIGN unidade-jr = am-cq-result-laudo.unidade.
    
              IF unidade-jr = "g/m2" THEN unidade-jr = "g/mý".

              ASSIGN c-relatorio:range("H" + STRING(i-linha)):VALUE = unidade-jr 
                     c-relatorio:range("I" + STRING(i-linha)):VALUE = am-cq-result-laudo.metodo.



              IF am-cq-result-laudo.cod-exame = 2011 AND
                  (am-cq-result-laudo.cod-comp = 17 OR
                   am-cq-result-laudo.cod-comp = 18 OR
                   am-cq-result-laudo.cod-comp = 19) THEN DO:
                   
                 ASSIGN c-relatorio:range("E" + STRING(i-linha)):VALUE = "-" 
                        c-relatorio:range("F" + STRING(i-linha)):VALUE = "-"
                        c-relatorio:range("I" + STRING(i-linha)):VALUE = "-".
                        
              end.



         END.  

    
    END. /* Fim do Modelo S.Cruz */


    IF am-cq-laudo.int-1 = 3 THEN DO:     /* Modelo Philip Morris */

        ASSIGN  c-relatorio:range("C" + STRING(6)):VALUE = "POLO FILMS"
                c-relatorio:range("C" + STRING(7)):VALUE = "PGZ".
    
        /* cabe‡alho do excel */
    
         ASSIGN i-linha = 56.
    
         ASSIGN c-relatorio:range("A" + STRING(i-linha)):VALUE = am-cq-laudo.nome-responsavel.
    
         ASSIGN c-relatorio:range("E" + STRING(i-linha)):VALUE = am-cq-laudo.dt-criacao.
         
         ASSIGN c-relatorio:range("C" + "08"):VALUE = am-cq-laudo.nome-abrev
                c-relatorio:range("C" + "13"):VALUE = am-cq-laudo.qtd-bobinas.
    
         ASSIGN c-relatorio:range("C" + "11"):VALUE = am-cq-laudo.dt-criacao.
         
         ASSIGN c-relatorio:range("C" + "10"):VALUE = am-cq-laudo.pedido-cliente
                c-relatorio:range("C" + "09"):VALUE = am-cq-laudo.zint 
                c-relatorio:range("C" + "14"):VALUE = am-cq-laudo.nr-laudo.    

         ASSIGN c-relatorio:range("C" + "12"):VALUE = STRING("WF " + string(am-cq-laudo.it-codigo) + " " + string(am-cq-laudo.larg) + " mm").

         IF LENGTH(am-cq-laudo.zint) > 5  THEN
            ASSIGN referencia-pm = SUBSTRING(am-cq-laudo.zint,1,2) + "WFPGZ" + 
                   SUBSTRING(am-cq-laudo.zint,(LENGTH(am-cq-laudo.zint) - 3),4).
         ELSE
             ASSIGN referencia-pm = "".

         ASSIGN c-relatorio:range("C" + "15"):VALUE = referencia-pm.


         /* -----------------------------------------------------------
            Classifica para ordem de an lise Philip Morris */

         FOR EACH am-cq-result-laudo WHERE
             am-cq-result-laudo.nr-laudo = am-cq-laudo.nr-laudo
             exclusive-lock.

             FIND FIRST am-cq-analise-cli WHERE
                 am-cq-analise-cli.cod-emitente = am-cq-laudo.cod-emitente     AND
                 am-cq-analise-cli.cod-exame    = am-cq-result-laudo.cod-exame AND
                 am-cq-analise-cli.cod-comp     = am-cq-result-laudo.cod-comp
                 NO-LOCK NO-ERROR.

             IF NOT AVAIL am-cq-analise-cli THEN NEXT.

             ASSIGN am-cq-result-laudo.char-1 =  am-cq-analise-cli.cod-an-cli.

         END.

         /* Classifica para ordem de an lise Philip Morris
         ------------------------------------------------------------------ */



         ASSIGN i-linha = 19.

         FOR EACH am-cq-result-laudo WHERE
             am-cq-result-laudo.nr-laudo = am-cq-laudo.nr-laudo
             no-LOCK
             BY am-cq-result-laudo.char-1.


             FIND FIRST am-cq-analise-cli WHERE
                 am-cq-analise-cli.cod-emitente = am-cq-laudo.cod-emitente     AND
                 am-cq-analise-cli.cod-exame    = am-cq-result-laudo.cod-exame AND
                 am-cq-analise-cli.cod-comp     = am-cq-result-laudo.cod-comp
                 NO-LOCK NO-ERROR.

             IF NOT AVAIL am-cq-analise-cli THEN NEXT.

             ASSIGN decimais-jr = am-cq-analise-cli.nr-decimais.

             ASSIGN i-linha = i-linha + 1.

             IF i-linha > 37 THEN NEXT.
    
              ASSIGN c-relatorio:range("B" + STRING(i-linha)):VALUE = am-cq-analise-cli.descricao.

              ASSIGN media-result-jr = am-cq-result-laudo.media-result.
    
              IF decimais-jr = 0 THEN
                 ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9").
                ELSE
                  IF decimais-jr = 1 THEN
                     ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9").
                  ELSE
                      IF decimais-jr = 2 THEN
                         ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.99").
                      ELSE
                          IF decimais-jr = 3 THEN
                             ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.999").
                          ELSE
                              IF decimais-jr > 3 THEN
                                 ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9999").
    

              ASSIGN c-relatorio:range("F" + STRING(i-linha)):VALUE = TRIM (media-result-jr-x).
    
                 ASSIGN media-jr2    = media-result-jr
                        media-result-jr = am-cq-result-laudo.espec-alvo.
    
              IF decimais-jr = 0 THEN
                 ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9").
                ELSE
                  IF decimais-jr = 1 THEN
                     ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9").
                  ELSE
                      IF decimais-jr = 2 THEN
                         ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.99").
                      ELSE
                          IF decimais-jr = 3 THEN
                             ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.999").
                          ELSE
                              IF decimais-jr > 3 THEN
                                 ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9999").
    
/*    

              ASSIGN c-relatorio:range("G" + STRING(i-linha)):VALUE = TRIM (media-result-jr-x).
*/
              
              ASSIGN media-result-jr = am-cq-result-laudo.maior-result.

              IF decimais-jr = 0 THEN
                 ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9").
                ELSE
                  IF decimais-jr = 1 THEN
                     ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9").
                  ELSE
                      IF decimais-jr = 2 THEN
                         ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.99").
                      ELSE
                          IF decimais-jr = 3 THEN
                             ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.999").
                          ELSE
                              IF decimais-jr > 3 THEN
                                 ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9999").


/*
              ASSIGN c-relatorio:range("E" + STRING(i-linha)):VALUE = TRIM (media-result-jr-x).
*/                 
                 
              ASSIGN media-result-jr = am-cq-result-laudo.qtd-result.

              ASSIGN decimais-jr = 0.
              
              IF decimais-jr = 0 THEN
                 ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9").
                ELSE
                  IF decimais-jr = 1 THEN
                     ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9").
                  ELSE
                      IF decimais-jr = 2 THEN
                         ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.99").
                      ELSE
                          IF decimais-jr = 3 THEN
                             ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.999").
                          ELSE
                              IF decimais-jr > 3 THEN
                                 ASSIGN media-result-jr-x = STRING (media-result-jr,">>>>>>>>9.9999").



              ASSIGN c-relatorio:range("E" + STRING(i-linha)):VALUE = TRIM (media-result-jr-x).
              
              ASSIGN c-relatorio:range("G" + STRING(i-linha)):VALUE = am-cq-result-laudo.espec-max.

              ASSIGN unidade-jr = am-cq-analise-cli.unidade.
    
              IF unidade-jr = "g/m2" THEN unidade-jr = "g/mý".

              ASSIGN c-relatorio:range("D" + STRING(i-linha)):VALUE = unidade-jr 
                     c-relatorio:range("A" + STRING(i-linha)):VALUE = am-cq-analise-cli.cod-an-cli
                     c-relatorio:range("H" + STRING(i-linha)):VALUE = "OK".


              
         END.  

    
    END. /* Fim do Modelo Philip Morris */


END PROCEDURE.


PROCEDURE pi-dt-prod-bobinas.
/*
    DEFINE VARIABLE a-plt AS INTEGER    NO-UNDO.
    DEFINE VARIABLE x-plt AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE b-plt AS INTEGER    NO-UNDO.

    DEFINE VARIABLE c-bob-col1  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE c-dat-col1  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE c-bob-col2  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE c-dat-col2  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE c-bob-col3  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE c-dat-col3  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE x-col-1     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE x-col-2     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE x-col-3     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i-tt-bb AS INTEGER     NO-UNDO.
*/
    ASSIGN b-plt = NUM-ENTRIES(am-cq-laudo.pallet," ")
        i-tt-bb = 0.

    FOR EACH tt-bob-prod.
        DELETE tt-bob-prod.
    END.

    DO a-plt = 1 TO b-plt.

        ASSIGN x-plt = ENTRY(a-plt,am-cq-laudo.pallet," ").

        IF x-plt = "" THEN NEXT.

        FOR EACH it-pallet WHERE
            it-pallet.nr-pallet = x-plt AND
            it-pallet.cod-estabel = am-cq-laudo.cod-estabel AND
            it-pallet.it-codigo   = am-cq-laudo.it-codigo NO-LOCK,

            LAST movto-mat WHERE
                 movto-mat.it-codigo = it-pallet.it-codigo AND
                 movto-mat.lote      = it-pallet.lote-bobina AND
                 movto-mat.esp-docto = 1
                 USE-INDEX lote NO-LOCK.

            FIND FIRST tt-bob-prod WHERE
                tt-bob-prod.lote = movto-mat.lote
                NO-ERROR.

            IF NOT AVAIL tt-bob-prod THEN DO:
                CREATE tt-bob-prod.
                ASSIGN tt-bob-prod.lote    = movto-mat.lote
                       tt-bob-prod.dt-prod = movto-mat.dt-trans.
            END.

             i-tt-bb =  i-tt-bb + 1.

        END.

    END.

    ASSIGN c-bob-col1 = ""
           c-dat-col1 = ""
           c-bob-col2 = ""
           c-dat-col2 = ""
           c-bob-col3 = ""
           c-dat-col3 = ""
           x-col-1    = ""
           x-col-2    = ""
           x-col-3    = "".

    assign conta-bob    = 0
           linhas-novas = 1
           lin-bob      = 22.

    for each tt-bob-prod NO-LOCK.

        assign conta-bob = conta-bob + 1.

        if conta-bob > 3 then do:      /* imprime a linha */

           run imprime-linha-bob. 
           
           assign conta-bob = 1.

        end.

        if conta-bob = 1 then
            assign c-bob-col1 = string(tt-bob-prod.lote)
                   c-dat-col1 = string(tt-bob-prod.dt-prod, "99/99/9999").

        else do:
 
            if conta-bob = 2 then
                assign c-bob-col2 = string(tt-bob-prod.lote)
                       c-dat-col2 = string(tt-bob-prod.dt-prod, "99/99/9999").

            else

                assign c-bob-col3 = string(tt-bob-prod.lote)
                       c-dat-col3 = string(tt-bob-prod.dt-prod, "99/99/9999").

        end.

    end.
    if conta-bob > 0 then do:      /* imprime a linha */

           run imprime-linha-bob. 
           
           

    end.

c-relatorio:range("a1"):select.



/*

    FOR EACH tt-bob-prod NO-LOCK.

        if x-col-1 = "" then do:

            assign c-bob-col1 = c-bob-col1 + string(tt-bob-prod.lote, "x(10)") + " "
                   c-dat-col1 = c-dat-col1 + string(tt-bob-prod.dt-prod, "99/99/9999") + " "
                   x-col-1    = "S".

            next.

        end.

        if x-col-2 = "" then do:

            assign c-bob-col2 = c-bob-col2 + string(tt-bob-prod.lote, "x(10)") + " "
                   c-dat-col2 = c-dat-col2 + string(tt-bob-prod.dt-prod, "99/99/9999") + " "
                   x-col-2    = "S".

            next.

        end.

        if x-col-3 = "" then do:

            assign c-bob-col3 = c-bob-col3 + string(tt-bob-prod.lote, "x(10)") + "     "
                   c-dat-col3 = c-dat-col3 + string(tt-bob-prod.dt-prod, "99/99/9999") + " "
                   x-col-1    = ""
                   x-col-2    = ""
                   x-col-3    = "".

            next.

        end.

    END.

    ASSIGN c-relatorio:range("A" + STRING(23)):VALUE = string(c-bob-col1)
           c-relatorio:range("B" + STRING(23)):VALUE = string(c-dat-col1).
    
    ASSIGN c-relatorio:range("C" + STRING(23)):VALUE = string(c-bob-col2)
           c-relatorio:range("D" + STRING(23)):VALUE = string(c-dat-col2).
    
    ASSIGN c-relatorio:range("E" + STRING(23)):VALUE = string(c-bob-col3)
           c-relatorio:range("F" + STRING(23)):VALUE = string(c-dat-col3).

IF i-tt-bb > 15 THEN c-relatorio:Rows("23:23"):autofit.

*/

END PROCEDURE.


procedure imprime-linha-bob.
 
    assign lin-bob = lin-bob + 1
           linhas-novas = linhas-novas + 1.
     c-relatorio:Rows(lin-bob):SELECT.
     c-relatorio:Rows(lin-bob):copy.

    assign lin-bob2 = lin-bob + 1.
           c-relatorio:Rows(lin-bob2):select.
           c-relatorio:Rows(lin-bob2):INSERT.
           
  

    ASSIGN c-relatorio:range("A" + STRING(lin-bob)):VALUE = string(c-bob-col1)
           c-relatorio:range("B" + STRING(lin-bob)):VALUE = string(c-dat-col1).
    
    ASSIGN c-relatorio:range("C" + STRING(lin-bob)):VALUE = string(c-bob-col2)
           c-relatorio:range("D" + STRING(lin-bob)):VALUE = string(c-dat-col2).
    
    ASSIGN c-relatorio:range("E" + STRING(lin-bob)):VALUE = string(c-bob-col3)
           c-relatorio:range("F" + STRING(lin-bob)):VALUE = string(c-dat-col3).
    

    ASSIGN c-bob-col1 = ""
           c-dat-col1 = ""
           c-bob-col2 = ""
           c-dat-col2 = ""
           c-bob-col3 = ""
           c-dat-col3 = ""
           x-col-1    = ""
           x-col-2    = ""
           x-col-3    = "".

end procedure.





