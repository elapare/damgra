
/*************************************************
 Programa   : ESCE0011rp.p
 Autor      : Datasul Bahia
 Data       : Maio de 2003
 Descri‡Æo  : Relat¢rio de Saldo F¡sico
 Cliente    : Grupo Unigel
 *************************************************/

/* *************************** Altera‡äes no Programa ************************************
   
   Autor    :
   Data     :
   Objetivo :
   
   
   *************************************************************************************** */                          
   
                          
/* ***************************  Definitions  ************************** */

/* Temporary Table Definitions ---                                      */

DEF NEW GLOBAL SHARED VAR i-num-ped-exec-rpw      AS INT  NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-dir-spool-servid-exec AS CHAR NO-UNDO.

/* Temporary Table Definitions ---                                      */

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    FIELD estab-ini        AS   CHAR
    FIELD estab-fim        AS   CHAR 
    FIELD grupo-ini        LIKE ITEM.ge-codigo
    FIELD grupo-fim        LIKE ITEM.ge-codigo
    FIELD familia-ini      LIKE ITEM.fm-codigo 
    FIELD familia-fim      LIKE ITEM.fm-codigo 
    FIELD periodo-ini      AS   DATE 
    FIELD item-ini         LIKE ITEM.it-codigo
    FIELD item-fim         LIKE ITEM.it-codigo
    FIELD dep-ini          LIKE saldo-estoq.cod-depos
    FIELD dep-fim          LIKE saldo-estoq.cod-depos
    FIELD local-ini        LIKE saldo-estoq.cod-localiz
    FIELD local-fim        LIKE saldo-estoq.cod-localiz
    FIELD refer-ini        LIKE saldo-estoq.cod-refer
    FIELD refer-fim        LIKE saldo-estoq.cod-refer
    FIELD famcomini        LIKE item.fm-cod-com
    FIELD famcomfim        LIKE item.fm-cod-com 
    FIELD arquivo-excel    AS   CHAR
    FIELD i-obsoletos      AS   INT
    FIELD saldo-positivo   AS   LOG 
    FIELD saldo-negativo   AS   LOG 
    FIELD saldo-zerado     AS   LOG 
    FIELD sem-saldo        AS   LOG 
    FIELD qtd-alocado      AS   LOG
    FIELD detalhado        AS   LOG 
    FIELD consumo          AS   LOG
    FIELD moeda            LIKE moeda.mo-codigo
    FIELD i-familia        AS INT.
    

define temp-table tt-digita no-undo
    field cod-estabel      LIKE estabelec.cod-estabel
    field cod-depos        LIKE deposito.cod-depos
    index id cod-estabel.
   
def temp-table tt-raw-digita
   field raw-digita      as raw.


DEFINE TEMP-TABLE tt-saldo-estoq NO-UNDO LIKE saldo-estoq    
 FIELD preco-unit       LIKE movto-estoq.valor-mat-m[1]
 FIELD preco-total      LIKE movto-estoq.valor-mat-m[1]
 FIELD preco-total-ggf  LIKE movto-estoq.valor-ggf-m[1]
 FIELD preco-total-MOB  LIKE movto-estoq.valor-MOB-m[1]
 FIELD imprimir         AS   LOGICAL
 FIELD peso-liquido     LIKE ITEM.peso-liquido
      
 INDEX idx02
       it-codigo   
       cod-estabel 
       cod-depos  
       cod-localiz 
       lote        
       cod-refer
    
 .


/* Parameters Definitions ---                                           */

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.
 
/* *** Transfer Definitions **** */

create tt-param.
raw-transfer raw-param to tt-param.

 FOR EACH tt-raw-digita NO-LOCK:
     CREATE tt-digita.
     RAW-TRANSFER raw-digita TO tt-digita.
 END.

 def var h-acomp as handle no-undo.


  /* Vari veis usadas para gerar planilha excel. */

      DEF VAR c-arq             AS CHAR                NO-UNDO.
      DEF VAR c-arq-anexo       AS CHAR                NO-UNDO.   
      DEF VAR l-cria            AS LOG                 NO-UNDO.
      DEF VAR i-pagina          AS INT INIT 1          NO-UNDO.
      DEF VAR i-linha           AS INT                 NO-UNDO.
      DEF VAR i-linha1          AS INT                 NO-UNDO.
      DEF VAR c-coluna          AS char                NO-UNDO.
DEFINE VARIABLE c-linha AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-linha1 AS CHARACTER   NO-UNDO.
      /* para executar o Excel */

      def var c-modelo-planilha  as char format "x(50)"         no-undo.
      def var c-excel            as com-handle                  NO-UNDO.
      def var c-planilha         as com-handle.
      def var c-planilha1        as com-handle.
      def var c-relatorio        as com-handle.
      def var c-relatorio1       as com-handle.
      DEF VAR c-arquivo          AS CHAR NO-UNDO.
      DEF VAR c-arquivo-rel      AS CHAR NO-UNDO.

              /* Cria Aplica‡Æo do Excel */

  
DEFINE VARIABLE d-qtde            AS DECIMAL     NO-UNDO.
DEFINE VARIABLE d-peso            AS DECIMAL     NO-UNDO.
DEFINE VARIABLE d-preco-total     AS DECIMAL     NO-UNDO.
DEFINE VARIABLE d-total-ggf       AS DECIMAL     NO-UNDO.
DEFINE VARIABLE d-preco-total-MOB AS DECIMAL     NO-UNDO.
DEFINE VARIABLE d-valor-total     AS DECIMAL     NO-UNDO.
DEFINE VARIABLE c-quebra          AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE i-ct AS INTEGER     NO-UNDO.
    DEFINE VARIABLE i-time-ini AS INTEGER     NO-UNDO.
    DEFINE VARIABLE i-time-fim AS INTEGER     NO-UNDO.

    i-time-ini = TIME.

 DEFINE VARIABLE i-it-codigo-ini AS CHARACTER INITIAL ""  NO-UNDO.
 DEFINE VARIABLE i-it-codigo-fim AS CHARACTER INITIAL "zzz"   NO-UNDO.
 DEFINE VARIABLE i-ge-codigo-ini AS INTEGER   INITIAL 0 NO-UNDO.
 DEFINE VARIABLE i-ge-codigo-fim AS INTEGER   INITIAL 99  NO-UNDO.
 DEFINE VARIABLE c-fm-cod-com-ini AS CHARACTER   INITIAL ""  NO-UNDO.
 DEFINE VARIABLE c-fm-cod-com-fim AS CHARACTER   INITIAL "zzz"  NO-UNDO.
 DEFINE VARIABLE c-fm-codigo-ini AS CHARACTER   INITIAL ""  NO-UNDO.
 DEFINE VARIABLE c-fm-codigo-fim AS CHARACTER   INITIAL "zzz"  NO-UNDO.
 DEFINE VARIABLE c-estab-ini AS CHARACTER  INITIAL ""  NO-UNDO.  /*solic-318*/ 
 DEFINE VARIABLE c-estab-fim AS CHARACTER  INITIAL "zzz"  NO-UNDO.  /*solic-318*/ 
 DEFINE VARIABLE c-depos-ini AS CHARACTER  INITIAL "" NO-UNDO.
 DEFINE VARIABLE c-depos-fim AS CHARACTER  INITIAL "zzz"  NO-UNDO.
 DEFINE VARIABLE c-local-ini AS CHARACTER  INITIAL "" NO-UNDO.
 DEFINE VARIABLE c-local-fim AS CHARACTER  INITIAL "zzz"  NO-UNDO.
 DEFINE VARIABLE c-refer-ini AS CHARACTER  INITIAL "" NO-UNDO.
 DEFINE VARIABLE c-refer-fim AS CHARACTER  INITIAL "zzz"  NO-UNDO.
 DEFINE VARIABLE dt-ref AS DATE  INITIAL 09/27/2013      NO-UNDO.
DEFINE VARIABLE c-fm-codigo  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-ge-codigo  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-fm-cod-com AS CHARACTER   NO-UNDO.
           
DEFINE VARIABLE l-digita AS LOGICAL   INITIAL NO  NO-UNDO.




DEFINE VARIABLE c-it-codigo-per AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-cod-estabel-per AS CHARACTER   NO-UNDO.
DEFINE VARIABLE d-periodo-per AS DATE        NO-UNDO.
DEFINE VARIABLE i-ct-per AS INTEGER     NO-UNDO.


DEFINE VARIABLE da-iniper       AS DATE FORMAT "99/99/9999"       NO-UNDO.
DEFINE VARIABLE da-fimper       AS DATE FORMAT "99/99/9999"       NO-UNDO.
DEFINE VARIABLE i-per-corrente  AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-ano-corrente  AS INTEGER     NO-UNDO.
DEFINE VARIABLE da-iniper-fech  AS DATE        NO-UNDO.
DEFINE VARIABLE da-fimper-fech  AS DATE        NO-UNDO.





{utp/ut-glob.i} 

    ASSIGN c-modelo-planilha = search("modelos\mod-ESCE0011.xlsx") .


    if c-modelo-planilha = ? then return.

 RUN pi-cria-planilha.


 run utp/ut-acomp.p persistent set h-acomp.
 run pi-inicializar in h-acomp (input "Imprimindo...").




ASSIGN
 c-estab-ini        = tt-param.estab-ini  
 c-estab-fim        = tt-param.estab-fim  
 i-ge-codigo-ini    = tt-param.grupo-ini  
 i-ge-codigo-fim    = tt-param.grupo-fim  
 c-fm-codigo-ini    = tt-param.familia-ini
 c-fm-codigo-fim    = tt-param.familia-fim
 dt-ref             = tt-param.periodo-ini
 i-it-codigo-ini    = tt-param.item-ini   
 i-it-codigo-fim    = tt-param.item-fim   
 c-depos-ini        = tt-param.dep-ini    
 c-depos-fim        = tt-param.dep-fim    
 c-local-ini        = tt-param.local-ini  
 c-local-fim        = tt-param.local-fim  
 c-refer-ini        = tt-param.refer-ini  
 c-refer-fim        = tt-param.refer-fim  
 c-fm-cod-com-ini   = tt-param.famcomini  
 c-fm-cod-com-fim   = tt-param.famcomfim  .





 FIND FIRST moeda WHERE
           (moeda.mo-codigo = tt-param.moeda) NO-LOCK NO-ERROR.

FIND FIRST param-estoq NO-LOCK.
FIND FIRST param-global NO-LOCK.

RUN CDP/CDAPI005.P (INPUT param-estoq.ult-per-fech,
                    OUTPUT da-iniper,
                    OUTPUT da-fimper,
                    OUTPUT i-per-corrente,
                    OUTPUT i-ano-corrente,
                    OUTPUT da-iniper-fech,
                    OUTPUT da-fimper-fech).
FIND FIRST tt-digita NO-LOCK NO-ERROR.

IF  AVAIL tt-digita THEN DO:
  l-digita = YES.
END.

 RUN pi-saldo-estoq.

    FOR EACH ITEM fields(ge-codigo it-codigo peso-liquido cod-obsoleto) WHERE 
                        ITEM.ge-codigo >= i-ge-codigo-ini AND
                        ITEM.ge-codigo <= i-ge-codigo-fim AND
                        ITEM.fm-codigo >= c-fm-codigo-ini AND
                        ITEM.fm-codigo <= c-fm-codigo-fim AND
                        ITEM.fm-cod-com >= c-fm-cod-com-ini AND
                        item.fm-cod-com <= c-fm-cod-com-fim AND
                        ITEM.it-codigo >= i-it-codigo-ini AND
                        ITEM.it-codigo <= i-it-codigo-fim NO-LOCK USE-INDEX codigo,
            EACH item-estab WHERE
                 item-estab.it-codigo = ITEM.it-codigo and
                 item-estab.cod-estabel >=  c-estab-ini AND
                 item-estab.cod-estabel <=  c-estab-fim  NO-LOCK.
                           .

          if (item.cod-obsoleto    = 1) and
            (tt-param.i-obsoleto  = 2) then next.
            
        if (item.cod-obsoleto    <> 1) and
            (tt-param.i-obsoleto  = 1) then next.

         IF l-digita THEN DO:
            IF NOT CAN-FIND (FIRST tt-digita WHERE
                               tt-digita.cod-estabel = item-estab.cod-estabel  )
                     THEN NEXT.
         END.    

          i-ct-per = 0.
          IF DAY(dt-ref + 1) = 1 THEN
          d-periodo-per = dt-ref.
          ELSE

          d-periodo-per = date(month(dt-ref),1,YEAR(dt-ref)) - 1.

          

          REPEAT.
              
              i-ct-per = i-ct-per + 1.
              IF i-ct-per > 6 THEN LEAVE.

              FIND FIRST sl-it-per WHERE 
                   sl-it-per.it-codigo    = ITEM.it-codigo AND  
                   sl-it-per.cod-estabel  = item-estab.cod-estabel AND
                   sl-it-per.cod-estabel  = item-estab.deposito-pad AND
                   sl-it-per.periodo      = d-periodo-per NO-LOCK USE-INDEX per-item NO-ERROR.
              IF NOT AVAIL sl-it-per THEN
              FIND FIRST sl-it-per WHERE 
                   sl-it-per.it-codigo    = ITEM.it-codigo AND  
                   sl-it-per.cod-estabel  = item-estab.cod-estabel AND
                   sl-it-per.periodo      = d-periodo-per and
                   sl-it-per.val-unit-mat-m[1] > 0 NO-LOCK USE-INDEX per-item NO-ERROR.
              IF NOT AVAIL sl-it-per THEN
              FIND FIRST sl-it-per WHERE 
                   sl-it-per.it-codigo    = ITEM.it-codigo AND  
                   sl-it-per.cod-estabel  = item-estab.cod-estabel AND
                   sl-it-per.periodo      = d-periodo-per   NO-LOCK USE-INDEX per-item NO-ERROR.

             
              IF AVAIL sl-it-per THEN do:
                  i-ct-per = 88.
                  LEAVE.
              END.



              d-periodo-per = date(month(d-periodo-per),1,YEAR(d-periodo-per)) - 1.

          END.
         /*
          IF NOT AVAIL sl-it-per THEN DO:
              FIND LAST sl-it-per WHERE 
                   sl-it-per.it-codigo    = ITEM.it-codigo AND  
                   sl-it-per.cod-estabel  = item-estab.cod-estabel AND
                   sl-it-per.periodo      <= dt-ref NO-LOCK NO-ERROR.
              i-ct-per = 99 .
          END.
          */
          IF  (AVAIL sl-it-per AND sl-it-per.val-unit-mat-m[1] =  0) OR NOT AVAIL sl-it-per THEN DO:
                i-ct-per = 0.
                IF DAY(dt-ref + 1) = 1 THEN
                d-periodo-per = dt-ref.
                ELSE
                
                d-periodo-per = date(month(dt-ref),1,YEAR(dt-ref)) - 1.
                
                
                
                REPEAT.
                
                    i-ct-per = i-ct-per + 1.
                    IF i-ct-per > 6 THEN LEAVE.
                
                    FIND FIRST pr-it-per WHERE 
                         pr-it-per.it-codigo    = ITEM.it-codigo AND  
                         pr-it-per.cod-estabel  = item-estab.cod-estabel AND
                         pr-it-per.periodo      = d-periodo-per NO-LOCK USE-INDEX codigo NO-ERROR.
                
                    IF AVAIL pr-it-per THEN do:
                        i-ct-per = 88.
                        LEAVE.
                    END.
                
                
                
                    d-periodo-per = date(month(d-periodo-per),1,YEAR(d-periodo-per)) - 1.
                
                END.
                
                IF NOT AVAIL pr-it-per THEN DO:
                    FIND LAST pr-it-per WHERE 
                         pr-it-per.it-codigo    = ITEM.it-codigo AND  
                         pr-it-per.cod-estabel  = item-estab.cod-estabel AND
                         pr-it-per.periodo      <= dt-ref NO-LOCK NO-ERROR.
                    i-ct-per = 99 .
                END.

          END.

      FOR EACH tt-saldo-estoq WHERE 
           tt-saldo-estoq.it-codigo = item-estab.it-codigo AND
           tt-saldo-estoq.cod-estabel = item-estab.cod-estabel .
            
          tt-saldo-estoq.peso-liquido   = item.peso-liquido.
          
         IF  AVAIL sl-it-per AND 
             sl-it-per.val-unit-mat-m[1] <> 0  THEN DO:
             IF (moeda.mo-codigo = 0) THEN
               ASSIGN tt-saldo-estoq.preco-unit      = sl-it-per.val-unit-mat-m[1]
                      tt-saldo-estoq.preco-total     = sl-it-per.val-unit-mat-m[1] * tt-saldo-estoq.qtidade-atu
                      tt-saldo-estoq.preco-total-ggf = sl-it-per.val-unit-ggf-m[1] * tt-saldo-estoq.qtidade-atu
                      tt-saldo-estoq.preco-total-MOB = (sl-it-per.val-unit-MOB-m[1] + sl-it-per.val-unit-MOB-p[1]) * tt-saldo-estoq.qtidade-atu.   
             ELSE IF (moeda.mo-codigo = 1) THEN
               ASSIGN tt-saldo-estoq.preco-unit      = sl-it-per.val-unit-mat-m[2]
                      tt-saldo-estoq.preco-total     = sl-it-per.val-unit-mat-m[2] * tt-saldo-estoq.qtidade-atu
                      tt-saldo-estoq.preco-total-ggf = sl-it-per.val-unit-ggf-m[2] * tt-saldo-estoq.qtidade-atu
                      tt-saldo-estoq.preco-total-MOB = (sl-it-per.val-unit-MOB-m[2] + sl-it-per.val-unit-MOB-p[2]) * tt-saldo-estoq.qtidade-atu.
             ELSE IF (moeda.mo-codigo = 2) THEN
               ASSIGN tt-saldo-estoq.preco-unit      = sl-it-per.val-unit-mat-m[3]
                      tt-saldo-estoq.preco-total     = sl-it-per.val-unit-mat-m[3] * tt-saldo-estoq.qtidade-atu
                      tt-saldo-estoq.preco-total-ggf = sl-it-per.val-unit-ggf-m[3] * tt-saldo-estoq.qtidade-atu
                      tt-saldo-estoq.preco-total-MOB = (sl-it-per.val-unit-MOB-m[3] + sl-it-per.val-unit-MOB-p[3]) * tt-saldo-estoq.qtidade-atu.

          
                
            
         END.
         ELSE 

         IF  AVAIL sl-it-per AND AVAIL pr-it-per AND 
                     pr-it-per.val-unit-mat-m[1] <> 0 AND pr-it-per.it-codigo    = item-estab.it-codigo THEN DO:
                     IF (moeda.mo-codigo = 0) THEN
                       ASSIGN tt-saldo-estoq.preco-unit      = pr-it-per.val-unit-mat-m[1]
                              tt-saldo-estoq.preco-total     = pr-it-per.val-unit-mat-m[1] * tt-saldo-estoq.qtidade-atu
                              tt-saldo-estoq.preco-total-ggf = pr-it-per.val-unit-ggf-m[1] * tt-saldo-estoq.qtidade-atu
                              tt-saldo-estoq.preco-total-MOB = (pr-it-per.val-unit-MOB-m[1] + pr-it-per.val-unit-MOB-p[1]) * tt-saldo-estoq.qtidade-atu.   
                     ELSE IF (moeda.mo-codigo = 1) THEN
                       ASSIGN tt-saldo-estoq.preco-unit      = pr-it-per.val-unit-mat-m[2]
                              tt-saldo-estoq.preco-total     = pr-it-per.val-unit-mat-m[2] * tt-saldo-estoq.qtidade-atu
                              tt-saldo-estoq.preco-total-ggf = pr-it-per.val-unit-ggf-m[2] * tt-saldo-estoq.qtidade-atu
                              tt-saldo-estoq.preco-total-MOB = (pr-it-per.val-unit-MOB-m[2] + pr-it-per.val-unit-MOB-p[2]) * tt-saldo-estoq.qtidade-atu.
                     ELSE IF (moeda.mo-codigo = 2) THEN
                       ASSIGN tt-saldo-estoq.preco-unit      = pr-it-per.val-unit-mat-m[3]
                              tt-saldo-estoq.preco-total     = pr-it-per.val-unit-mat-m[3] * tt-saldo-estoq.qtidade-atu
                              tt-saldo-estoq.preco-total-ggf = pr-it-per.val-unit-ggf-m[3] * tt-saldo-estoq.qtidade-atu
                              tt-saldo-estoq.preco-total-MOB = (pr-it-per.val-unit-MOB-m[3] + pr-it-per.val-unit-MOB-p[3]) * tt-saldo-estoq.qtidade-atu.

         END.
         ELSE DO:
               
             IF (moeda.mo-codigo = 0) THEN    
                          ASSIGN tt-saldo-estoq.preco-unit      = item-estab.val-unit-mat-m[1]
                                 tt-saldo-estoq.preco-total     = item-estab.val-unit-mat-m[1] * tt-saldo-estoq.qtidade-atu
                                 tt-saldo-estoq.preco-total-ggf = item-estab.val-unit-ggf-m[1] * tt-saldo-estoq.qtidade-atu
                                 tt-saldo-estoq.preco-total-MOB = (item-estab.val-unit-MOB-m[1] + item-estab.val-unit-MOB-p[1]) * tt-saldo-estoq.qtidade-atu.
                       ELSE IF (moeda.mo-codigo = 1) THEN
                           ASSIGN tt-saldo-estoq.preco-unit      = item-estab.val-unit-mat-m[2]
                                  tt-saldo-estoq.preco-total     = item-estab.val-unit-mat-m[2] * tt-saldo-estoq.qtidade-atu
                                  tt-saldo-estoq.preco-total-ggf = item-estab.val-unit-ggf-m[2] * tt-saldo-estoq.qtidade-atu
                                  tt-saldo-estoq.preco-total-MOB = (item-estab.val-unit-MOB-m[2] + item-estab.val-unit-MOB-p[2]) * tt-saldo-estoq.qtidade-atu.
                       ELSE IF (moeda.mo-codigo = 2) THEN
                           ASSIGN tt-saldo-estoq.preco-unit      = item-estab.val-unit-mat-m[3]
                                  tt-saldo-estoq.preco-total     = item-estab.val-unit-mat-m[3] * tt-saldo-estoq.qtidade-atu
                                  tt-saldo-estoq.preco-total-ggf = item-estab.val-unit-ggf-m[3] * tt-saldo-estoq.qtidade-atu
                                  tt-saldo-estoq.preco-total-MOB = (item-estab.val-unit-MOB-m[3] + item-estab.val-unit-MOB-p[3]) * tt-saldo-estoq.qtidade-atu.

         END.

      END.


            i-ct = i-ct + 1.
            
          IF SUBSTRING(STRING(i-ct,"999999999"),8,2) = "00" THEN run pi-acompanhar in h-acomp (INPUT string(i-ct) + "IT:" + ITEM.it-codigo +  "Est:" + item-estab.cod-estabel).
    
 
    
    END.

      


 /*
 i-time-fim = TIME.
MESSAGE i-ct-per skip
    i-ct SKIP
    i-time-ini SKIP
    i-time-fim SKIP
    string(i-time-fim - i-time-ini,"hh:mm:ss")
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
i-ct = 0.
 i-time-ini = TIME.
 */


 
i-linha = 4.
i-linha1 = 5.
c-quebra = "".

 ASSIGN        d-qtde            = 0
               d-peso            = 0
               d-preco-total     = 0
               d-total-ggf       = 0
               d-preco-total-MOB = 0              
               d-valor-total     = 0.



FOR EACH tt-saldo-estoq USE-INDEX idx02 NO-LOCK,
    EACH ITEM WHERE ITEM.it-codigo = tt-saldo-estoq.it-codigo NO-LOCK.

    FIND FIRST contabiliza WHERE 

      contabiliza.cod-depos =  tt-saldo-estoq.cod-depos AND
          contabiliza.cod-estabel =  tt-saldo-estoq.cod-estabel AND
           
          contabiliza.ge-codigo     =  ITEM.ge-codigo NO-LOCK NO-ERROR.


      IF ((tt-saldo-estoq.qtidade-atu > 0) and
       (tt-param.saldo-positivo = YES) OR

       (tt-saldo-estoq.qtidade-atu < 0) and
       (tt-param.saldo-negativo = yes) OR

       (tt-saldo-estoq.qtidade-atu = 0) and
       (tt-param.saldo-zerado = yes)) THEN  . ELSE NEXT.


    ASSIGN  c-fm-codigo  = ""
            c-ge-codigo  = ""
            c-fm-cod-com = "".


     FIND FIRST familia WHERE
                (familia.fm-codigo = ITEM.fm-codigo) NO-LOCK NO-ERROR.

     IF AVAIL familia THEN
               c-fm-codigo = familia.descricao .

     FIND FIRST grup-estoque WHERE
                (grup-estoque.ge-codigo = ITEM.ge-codigo) NO-LOCK NO-ERROR.
     IF AVAIL grup-estoque THEN
               c-ge-codigo = grup-estoque.descricao. 


     FIND fam-comerc WHERE fam-comerc.fm-cod-com = ITEM.fm-cod-com NO-LOCK NO-ERROR.
            
    IF AVAIL fam-comerc THEN
               c-fm-cod-com = fam-comerc.descricao .

                                    
    IF c-quebra <> "" AND c-quebra <>  tt-saldo-estoq.cod-estabel + "_" +  tt-saldo-estoq.it-codigo + "_" +  tt-saldo-estoq.cod-depos + "_" +  tt-saldo-estoq.cod-localiz THEN  DO:
       RUN pi-total.



    END.

    
        ASSIGN d-qtde = d-qtde + tt-saldo-estoq.qtidade-atu
               d-peso = d-peso + tt-saldo-estoq.qtidade-atu   * ITEM.peso-liquido   
               d-preco-total     = d-preco-total + tt-saldo-estoq.preco-total 
               d-total-ggf       = d-total-ggf + tt-saldo-estoq.preco-total-ggf 
               d-preco-total-MOB = d-preco-total-MOB + tt-saldo-estoq.preco-total-MOB
              
               d-valor-total     = d-valor-total + d-preco-total   + d-total-ggf  + d-preco-total-MOB .     

        
         c-quebra = tt-saldo-estoq.cod-estabel + "_" +  tt-saldo-estoq.it-codigo + "_" +  tt-saldo-estoq.cod-depos + "_" +  tt-saldo-estoq.cod-localiz.
   

    i-ct = i-ct + 1.

    

            IF SUBSTRING(STRING(i-ct,"999999999"),8,2) = "00" THEN run pi-acompanhar in h-acomp(input  "saldo" + string(i-ct)).

        

     c-linha1 =     
         tt-saldo-estoq.cod-estabel  + chr(161) +
         string(ITEM.ge-codigo)  + chr(161) +
         c-ge-codigo + CHR(161) + 
         ITEM.fm-codigo + "-" +  c-fm-codigo + chr(161) +
         ITEM.fm-cod-com + "-" + c-fm-cod-com + chr(161) +
         tt-saldo-estoq.it-codigo    + chr(161) +
         replace(ITEM.desc-item, ";"," ") + chr(161) +
         ITEM.un  + chr(161) +         
         tt-saldo-estoq.cod-depos       + chr(161) +
         tt-saldo-estoq.cod-localiz  + CHR(161) +
         /* (IF AVAIL contabiliza THEN contabiliza.conta-contabil ELSE "") */
         (IF AVAIL contabiliza THEN contabiliza.ct-codigo ELSE "") + CHR(161) +
         (IF AVAIL contabiliza THEN contabiliza.sc-codigo ELSE "")
        .

     c-quebra = tt-saldo-estoq.cod-estabel + "_" +  tt-saldo-estoq.it-codigo + "_" +  tt-saldo-estoq.cod-depos + "_" +  tt-saldo-estoq.cod-localiz.

     c-linha =     
         tt-saldo-estoq.cod-estabel  + chr(161) +
         string(ITEM.ge-codigo)  + chr(161) +
         c-ge-codigo + CHR(161) + 
         ITEM.fm-codigo + "-" +  c-fm-codigo + chr(161) +
         ITEM.fm-cod-com + "-" + c-fm-cod-com + chr(161) +
         tt-saldo-estoq.it-codigo    + chr(161) +
         replace(ITEM.desc-item, ";"," ") + chr(161) +
         ITEM.un  + chr(161) +         
         tt-saldo-estoq.cod-depos       + chr(161) +
         tt-saldo-estoq.cod-localiz     + CHR(161) +
         /* (IF AVAIL contabiliza THEN contabiliza.conta-contabil ELSE "") + chr(161) + */
         (IF AVAIL contabiliza THEN contabiliza.ct-codigo ELSE "") + CHR(161) +
         (IF AVAIL contabiliza THEN contabiliza.sc-codigo ELSE "") + CHR(161) +
         trim(tt-saldo-estoq.cod-refer)      + chr(161) +
         trim(tt-saldo-estoq.lote)          + chr(161) +    
         string(tt-saldo-estoq.qtidade-atu )      + chr(161) +
         string(tt-saldo-estoq.qtidade-atu   * ITEM.peso-liquido)    + chr(161) +
         string(tt-saldo-estoq.preco-unit   )       + chr(161) +
         string(tt-saldo-estoq.preco-total )         + chr(161) +
         string(tt-saldo-estoq.preco-total-ggf)       + chr(161) +
         string(tt-saldo-estoq.preco-total-MOB)  + chr(161) +
         string(tt-saldo-estoq.preco-total        +
         tt-saldo-estoq.preco-total-ggf      +
         tt-saldo-estoq.preco-total-MOB)
        .

        IF    tt-param.detalhado = YES  THEN
             ASSIGN  i-linha = i-linha + 1

                     c-relatorio:range("A" + string(i-linha)):VALUE = c-linha.
          

 
END.

 
    IF c-quebra <> "" THEN 
       RUN pi-total.
RUN pi-mostra-planilha.
 
run pi-finalizar in h-acomp. 
 i-time-fim = TIME.
 /*
MESSAGE i-ct SKIP
    i-time-ini SKIP
    i-time-fim SKIP
    string(i-time-fim - i-time-ini,"hh:mm:ss")
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
   */
PROCEDURE pi-saldo-estoq.
    FOR EACH estabelec WHERE 
        estabelec.cod-estabel >=  c-estab-ini AND
        estabelec.cod-estabel <=  c-estab-fim NO-LOCK.

        IF l-digita THEN DO:
            IF NOT CAN-FIND (FIRST tt-digita WHERE tt-digita.cod-estabel = estabelec.cod-estabel)
                  THEN NEXT.
        END.

    FOR  EACH  saldo-estoq   WHERE 
     
        saldo-estoq.cod-estabel = estabelec.cod-estabel and
        saldo-estoq.it-codigo >= i-it-codigo-ini AND
        saldo-estoq.it-codigo <= i-it-codigo-fim AND
        saldo-estoq.qtidade-atu <> 0 NO-LOCK,
        EACH ITEM fields(ge-codigo cod-obsoleto) WHERE ITEM.it-codigo = saldo-estoq.it-codigo AND
                        ITEM.ge-codigo >= i-ge-codigo-ini AND
                        ITEM.ge-codigo <= i-ge-codigo-fim AND
                        ITEM.fm-codigo >= c-fm-codigo-ini AND
                        ITEM.fm-codigo <= c-fm-codigo-fim AND
                        ITEM.fm-cod-com >= c-fm-cod-com-ini AND
                        ITEM.fm-cod-com <= c-fm-cod-com-fim NO-LOCK.
      
         if (item.cod-obsoleto    = 1) and
            (tt-param.i-obsoleto  = 2) then next.
            
        if (item.cod-obsoleto    <> 1) and
            (tt-param.i-obsoleto  = 1) then next.


         IF l-digita THEN DO:
            IF NOT CAN-FIND (FIRST tt-digita WHERE
                               tt-digita.cod-estabel = estabelec.cod-estabel AND
                               tt-digita.cod-depos   = saldo-estoq.cod-depos  )
                     THEN NEXT.
         END.

  /* *** Critica no Deposito *** */
            if  tt-param.dep-ini <> " " then 
                if (saldo-estoq.cod-depos < tt-param.dep-ini) or
                   (saldo-estoq.cod-depos > tt-param.dep-fim) then next.
                
            if  tt-param.refer-ini <> " " then 
                if (saldo-estoq.cod-refer < tt-param.refer-ini) or
                   (saldo-estoq.cod-refer > tt-param.refer-fim) then next.

            if  tt-param.local-ini <> " " then 
                if (saldo-estoq.cod-localiz < tt-param.local-ini) or
                   (saldo-estoq.cod-localiz > tt-param.local-fim) then next.

            

       FIND FIRST tt-saldo-estoq WHERE 
                  tt-saldo-estoq.cod-estabel = saldo-estoq.cod-estabel  AND
                  tt-saldo-estoq.it-codigo   = saldo-estoq.it-codigo    AND
                  tt-saldo-estoq.cod-depos   = saldo-estoq.cod-depos    AND
                  tt-saldo-estoq.cod-localiz = saldo-estoq.cod-localiz  AND
                  tt-saldo-estoq.lote        = saldo-estoq.lote         AND
                  tt-saldo-estoq.cod-refer   = saldo-estoq.cod-refer   
            
           
           NO-ERROR.

       IF NOT AVAIL tt-saldo-estoq THEN DO:
           CREATE tt-saldo-estoq.
           BUFFER-COPY saldo-estoq TO tt-saldo-estoq.
           tt-saldo-estoq.qtidade-atu = saldo-estoq.qtidade-atu.

       END.
                
        i-ct = i-ct + 1.
        
        IF SUBSTRING(STRING(i-ct,"999999999"),8,2) = "00" THEN run pi-acompanhar in h-acomp (INPUT  "Est:" + estabelec.cod-estabel +  "-" + string(i-ct)).
    END.
                  /*
    IF dt-ref > da-fimper-fech THEN DO:
      IF substring(TRIM(c-fm-codigo-ini + c-fm-codigo-fim ),1,1) <> "z" THEN DO:
              FOR EACH ITEM fields(ge-codigo it-codigo  peso-liquido) WHERE   
                            ITEM.ge-codigo >= i-ge-codigo-ini AND
                            ITEM.ge-codigo <= i-ge-codigo-fim AND
                            ITEM.fm-codigo >= c-fm-codigo-ini AND
                            ITEM.fm-codigo <= c-fm-codigo-fim and
                            item.it-codigo >= i-it-codigo-ini AND
                            ITEM.it-codigo <= i-it-codigo-fim USE-INDEX familia NO-LOCK,
                  
                  
                  EACH movto-estoq WHERE  
                         movto-estoq.cod-estabel = estabelec.cod-estabel AND
                         movto-estoq.dt-trans <= dt-ref AND
                         movto-estoq.dt-trans > da-fimper-fech AND
                         movto-estoq.quantidade <> 0 AND
                         movto-estoq.it-codigo = item.it-codigo   USE-INDEX item-data NO-LOCK
                        .
                       
                     RUN pi-saldo-frente.
              
              END.
     END.
     ELSE IF i-ge-codigo-ini <> 0 OR i-ge-codigo-fim <> 99 THEN DO:
             
              FOR EACH ITEM fields(ge-codigo it-codigo  peso-liquido) WHERE   
                            ITEM.ge-codigo >= i-ge-codigo-ini AND
                            ITEM.ge-codigo <= i-ge-codigo-fim AND
                            ITEM.fm-codigo >= c-fm-codigo-ini AND
                            ITEM.fm-codigo <= c-fm-codigo-fim and
                            item.it-codigo >= i-it-codigo-ini AND
                            ITEM.it-codigo <= i-it-codigo-fim USE-INDEX grupo NO-LOCK,
                  
                  
                  EACH movto-estoq WHERE  
                         movto-estoq.cod-estabel = estabelec.cod-estabel AND
                         movto-estoq.dt-trans <= dt-ref AND
                         movto-estoq.dt-trans > da-fimper-fech AND
                         movto-estoq.quantidade <> 0 AND
                         movto-estoq.it-codigo = item.it-codigo   USE-INDEX item-data NO-LOCK
                        .
                       
                     RUN pi-saldo-frente.
              
              END.
     END.
     ELSE IF substring(TRIM(i-it-codigo-ini + i-it-codigo-fim ),1,1) <> "z" THEN DO:
              FOR EACH ITEM fields(ge-codigo it-codigo  peso-liquido) WHERE  
                           
                            ITEM.ge-codigo >= i-ge-codigo-ini AND
                            ITEM.ge-codigo <= i-ge-codigo-fim AND
                            ITEM.fm-codigo >= c-fm-codigo-ini AND
                            ITEM.fm-codigo <= c-fm-codigo-fim and
                            item.it-codigo >= i-it-codigo-ini AND
                            ITEM.it-codigo <= i-it-codigo-fim USE-INDEX codigo NO-LOCK,
                  
                  
                  EACH movto-estoq WHERE  
                         movto-estoq.cod-estabel = estabelec.cod-estabel AND
                         movto-estoq.dt-trans <= dt-ref AND
                         movto-estoq.dt-trans > da-fimper-fech AND
                         movto-estoq.quantidade <> 0 AND
                         movto-estoq.it-codigo = item.it-codigo USE-INDEX item-data NO-LOCK.
                        .
                       
                     RUN pi-saldo-frente.
              
              END.
     END.

     ELSE DO:
        FOR EACH movto-estoq WHERE  
             movto-estoq.cod-estabel = estabelec.cod-estabel AND
             movto-estoq.dt-trans <= dt-ref AND
             movto-estoq.dt-trans > da-fimper-fech AND 
             movto-estoq.quantidade <> 0 AND
             movto-estoq.it-codigo >= i-it-codigo-ini AND
             movto-estoq.it-codigo <= i-it-codigo-fim USE-INDEX estab-dep NO-LOCK,
            EACH ITEM fields(ge-codigo it-codigo  peso-liquido) WHERE 
                            ITEM.it-codigo = movto-estoq.it-codigo AND
                            ITEM.ge-codigo >= i-ge-codigo-ini AND
                            ITEM.ge-codigo <= i-ge-codigo-fim AND
                            ITEM.fm-codigo >= c-fm-codigo-ini AND
                            ITEM.fm-codigo <= c-fm-codigo-fim NO-LOCK.
           
              RUN pi-saldo-frente.
              
        END.
     END.

        
    END.        */

    IF dt-ref < TODAY THEN DO:
        IF substring(TRIM(c-fm-codigo-ini + c-fm-codigo-fim ),1,1) <> "z" THEN DO:

             FOR EACH ITEM fields(ge-codigo it-codigo  peso-liquido) WHERE  
                           
                            ITEM.ge-codigo >= i-ge-codigo-ini AND
                            ITEM.ge-codigo <= i-ge-codigo-fim AND
                            ITEM.fm-codigo >= c-fm-codigo-ini AND
                            ITEM.fm-codigo <= c-fm-codigo-fim and
                            item.it-codigo >= i-it-codigo-ini AND
                            ITEM.it-codigo <= i-it-codigo-fim USE-INDEX familia NO-LOCK,
                  
                  
                  EACH movto-estoq WHERE  
                         movto-estoq.cod-estabel = estabelec.cod-estabel AND
                         movto-estoq.dt-trans > dt-ref AND
                         /*movto-estoq.dt-trans <= da-fimper-fech AND */
                         movto-estoq.quantidade <> 0 AND
                         movto-estoq.it-codigo = item.it-codigo USE-INDEX item-data NO-LOCK.
                        .

    
                RUN pi-saldo-tras.
            END.       
        END.
        ELSE IF i-ge-codigo-ini <> 0 OR i-ge-codigo-fim <> 99 THEN DO:

             FOR EACH ITEM fields(ge-codigo it-codigo  peso-liquido) WHERE  
                           
                            ITEM.ge-codigo >= i-ge-codigo-ini AND
                            ITEM.ge-codigo <= i-ge-codigo-fim AND
                            ITEM.fm-codigo >= c-fm-codigo-ini AND
                            ITEM.fm-codigo <= c-fm-codigo-fim and
                            item.it-codigo >= i-it-codigo-ini AND
                            ITEM.it-codigo <= i-it-codigo-fim USE-INDEX grupo NO-LOCK,
                  
                  
                  EACH movto-estoq WHERE  
                         movto-estoq.cod-estabel = estabelec.cod-estabel AND
                         movto-estoq.dt-trans > dt-ref AND
                        /* movto-estoq.dt-trans <= da-fimper-fech AND */
                         movto-estoq.quantidade <> 0 AND
                         movto-estoq.it-codigo = item.it-codigo USE-INDEX item-data NO-LOCK.
                        .

    
                RUN pi-saldo-tras.
            END.       
        END.
        ELSE IF substring(TRIM(i-it-codigo-ini + i-it-codigo-fim ),1,1) <> "z" THEN DO:

             FOR EACH ITEM fields(ge-codigo it-codigo  peso-liquido) WHERE  
                           
                            ITEM.ge-codigo >= i-ge-codigo-ini AND
                            ITEM.ge-codigo <= i-ge-codigo-fim AND
                            ITEM.fm-codigo >= c-fm-codigo-ini AND
                            ITEM.fm-codigo <= c-fm-codigo-fim and
                            item.it-codigo >= i-it-codigo-ini AND
                            ITEM.it-codigo <= i-it-codigo-fim USE-INDEX codigo NO-LOCK,
                  
                  
                  EACH movto-estoq WHERE  
                         movto-estoq.cod-estabel = estabelec.cod-estabel AND
                         movto-estoq.dt-trans > dt-ref AND
                         /*movto-estoq.dt-trans <= da-fimper-fech AND     */
                         movto-estoq.quantidade <> 0 AND
                         movto-estoq.it-codigo = item.it-codigo USE-INDEX item-data NO-LOCK.
                        .

    
                RUN pi-saldo-tras.
            END.       
        END.
        ELSE DO:
            FOR EACH movto-estoq WHERE  
               movto-estoq.cod-estabel = estabelec.cod-estabel AND
               movto-estoq.dt-trans > dt-ref AND
               /*movto-estoq.dt-trans <= da-fimper-fech AND */
               movto-estoq.quantidade <> 0 AND
               movto-estoq.it-codigo >= i-it-codigo-ini AND
               movto-estoq.it-codigo <= i-it-codigo-fim  USE-INDEX estab-dep NO-LOCK,
              EACH ITEM fields(ge-codigo it-codigo) WHERE  
                              ITEM.it-codigo = movto-estoq.it-codigo AND
                              ITEM.ge-codigo >= i-ge-codigo-ini AND
                              ITEM.ge-codigo <= i-ge-codigo-fim AND
                              ITEM.fm-codigo >= c-fm-codigo-ini AND
                              ITEM.fm-codigo <= c-fm-codigo-fim NO-LOCK.
    
                RUN pi-saldo-tras.
            END.       


        END.
    END.
 
END.
 

END PROCEDURE.

PROCEDURE pi-saldo-tras:
            
    
              FIND FIRST tt-saldo-estoq  WHERE
                 tt-saldo-estoq.it-codigo    = movto-estoq.it-codigo      AND
                 tt-saldo-estoq.cod-estabel  = movto-estoq.cod-estabel    AND
                 tt-saldo-estoq.cod-depos    = movto-estoq.cod-depos      AND
                 tt-saldo-estoq.cod-localiz  = movto-estoq.cod-localiz    AND
                 tt-saldo-estoq.lote         = movto-estoq.lote           AND
                 tt-saldo-estoq.cod-refer    = movto-estoq.cod-refer 
            
                              
                  
                  
                  
                  NO-ERROR.
    
           IF NOT AVAIL tt-saldo-estoq THEN DO:
               FIND FIRST saldo-estoq OF movto-estoq NO-LOCK NO-ERROR.
               IF NOT AVAIL saldo-estoq THEN DO:
               
                   DISP movto-estoq.quantidade movto-estoq.it-codigo  with WIDTH 300 1 COL.
                   RETURN.
               END.
               FIND FIRST tt-saldo-estoq  where
                   tt-saldo-estoq.cod-estabel = saldo-estoq.cod-estabel AND
                   tt-saldo-estoq.it-codigo   = saldo-estoq.it-codigo   AND
                   tt-saldo-estoq.cod-depos   = saldo-estoq.cod-depos   AND
                   tt-saldo-estoq.cod-localiz = saldo-estoq.cod-localiz AND
                   tt-saldo-estoq.lote        = saldo-estoq.lote        AND
                   tt-saldo-estoq.cod-refer   = saldo-estoq.cod-refer   
                             NO-ERROR.
    
               IF NOT AVAIL tt-saldo-estoq THEN DO:
                   CREATE tt-saldo-estoq.
                   BUFFER-COPY saldo-estoq TO tt-saldo-estoq.
                   tt-saldo-estoq.qtidade-atu = saldo-estoq.qtidade-atu.
        
               END.
    
           END.
               ASSIGN  tt-saldo-estoq.qtidade-atu =  tt-saldo-estoq.qtidade-atu + 
                     movto-estoq.quantidade * (IF movto-estoq.tipo-trans = 1 THEN -1 ELSE 1).
        
                i-ct = i-ct + 1.
                    IF SUBSTRING(STRING(i-ct,"999999999"),7,3) = "000" THEN run pi-acompanhar in h-acomp(INPUT "Est:" +  estabelec.cod-estabel + " Mov:" + string(i-ct)).
                    
        
END PROCEDURE.

PROCEDURE pi-saldo-frente:

     

     FIND FIRST tt-saldo-estoq   where
                   tt-saldo-estoq.cod-estabel = movto-estoq.cod-estabel AND
                   tt-saldo-estoq.it-codigo   = movto-estoq.it-codigo   AND
                   tt-saldo-estoq.cod-depos   = movto-estoq.cod-depos   AND
                   tt-saldo-estoq.cod-localiz = movto-estoq.cod-localiz AND
                   tt-saldo-estoq.lote        = movto-estoq.lote        AND
                   tt-saldo-estoq.cod-refer   = movto-estoq.cod-refer   
                             NO-ERROR.
    
           IF NOT AVAIL tt-saldo-estoq THEN DO:
               FIND FIRST saldo-estoq OF movto-estoq NO-LOCK NO-ERROR.
               IF NOT AVAIL saldo-estoq THEN DO:
               
                   DISP movto-estoq.quantidade movto-estoq.it-codigo  with WIDTH 300 1 COL.
                   return.
               END.
               FIND FIRST tt-saldo-estoq   where
                   tt-saldo-estoq.cod-estabel = saldo-estoq.cod-estabel AND
                   tt-saldo-estoq.it-codigo   = saldo-estoq.it-codigo   AND
                   tt-saldo-estoq.cod-depos   = saldo-estoq.cod-depos   AND
                   tt-saldo-estoq.cod-localiz = saldo-estoq.cod-localiz AND
                   tt-saldo-estoq.lote        = saldo-estoq.lote        AND
                   tt-saldo-estoq.cod-refer   = saldo-estoq.cod-refer   
                             NO-ERROR.
    
               IF NOT AVAIL tt-saldo-estoq THEN DO:
                   CREATE tt-saldo-estoq.
                   BUFFER-COPY saldo-estoq TO tt-saldo-estoq.
                   tt-saldo-estoq.qtidade-atu = 0.
        
               END.
    
           END.
            ASSIGN  tt-saldo-estoq.qtidade-atu =  tt-saldo-estoq.qtidade-atu + 
                 movto-estoq.quantidade * (IF movto-estoq.tipo-trans =1 THEN 1 ELSE -1).
     
            i-ct = i-ct + 1.
                IF SUBSTRING(STRING(i-ct,"999999999"),7,3) = "000" THEN  run pi-acompanhar in h-acomp("Est" +  estabelec.cod-estabel +  "Mov" + STRING( i-ct)).
                

END PROCEDURE.



PROCEDURE pi-cria-planilha:

    IF i-num-ped-exec-rpw = 0 THEN DO:
        c-arquivo = SESSION:TEMP-DIRECTORY + 'ESCE0011-' + REPLACE(STRING(TODAY, '99/99/9999'), '/', '') + REPLACE(STRING(TIME, 'HH:MM:SS'), ':', '') + '.XLSX'.
           
    END.
    ELSE DO:
        c-arquivo =  c-dir-spool-servid-exec + '\ESCE0011-' + REPLACE(STRING(TODAY, '99/99/9999'), '/', '') + REPLACE(STRING(TIME, 'HH:MM:SS'), ':', '') + '.XLSX'.
               
    END.
                  
   
    dos silent copy VALUE(c-modelo-planilha) VALUE(c-arquivo).

    CREATE "Excel.Application" c-excel.
    ASSIGN c-excel:DisplayAlerts = FALSE    
         c-excel:ScreenUpdating = YES
           C-Excel:visible       = NO.
  
    assign c-planilha  = c-excel:Workbooks:OPEN(c-arquivo)
           c-relatorio = c-excel:Sheets:item(2).
           c-relatorio1 = c-excel:Sheets:item(1).

END PROCEDURE.

PROCEDURE pi-formata-planilha:

/*
   C-Excel:ActiveWorkbook:save.
   C-Excel:visible       = YES.
   */
   c-relatorio = c-excel:Sheets:item(2).
   c-relatorio:SELECT.

   

     if i-linha <= 4 THEN return.
         else
             i-linha = i-linha + 2.

        c-relatorio:Range("A" + string(5) + ":A" + string(i-Linha)):select.
         c-excel:selection:TextToColumns (,         /* Destination          */
                                         1,        /* DataType             */
                                         ,         /* TextQualifier        */
                                         ,         /* ConsecutiveDelimiter */
                                         ,         /* Tab                  */
                                         ,         /* Semicolon            */
                                         ,         /* Comma                */
                                         ,         /* Space                */
                                         true,     /* Other                */
                                         chr(161), /* OtherChar            */
                                         ,         /* FieldInfo            */
                                         ) no-error.
 
       
        c-relatorio:Cells:Select.
        c-relatorio:Cells:EntireRow:AutoFit.

        c-relatorio:Range("a1"):select.


 

END PROCEDURE.

PROCEDURE pi-formata-planilha1:

/*
   C-Excel:ActiveWorkbook:save.
   C-Excel:visible       = YES.
   */
   c-relatorio = c-excel:Sheets:item(1).
   c-relatorio:SELECT.

   

     if i-linha1 <= 5 THEN return.
         else
             i-linha1 = i-linha1 + 2.

        c-relatorio:Range("A" + string(6) + ":A" + string(i-Linha1)):select.
         c-excel:selection:TextToColumns (,         /* Destination          */
                                         1,        /* DataType             */
                                         ,         /* TextQualifier        */
                                         ,         /* ConsecutiveDelimiter */
                                         ,         /* Tab                  */
                                         ,         /* Semicolon            */
                                         ,         /* Comma                */
                                         ,         /* Space                */
                                         true,     /* Other                */
                                         chr(161), /* OtherChar            */
                                         ,         /* FieldInfo            */
                                         ) no-error.
 
        c-relatorio:Range("G4"):VALUE = dt-ref.
        c-relatorio:Cells:Select.
        c-relatorio:Cells:EntireRow:AutoFit.

        c-relatorio:Range("a1"):select.


 

END PROCEDURE.

PROCEDURE pi-mostra-planilha:
 
RUN pi-formata-planilha.
RUN pi-formata-planilha1.
  if valid-handle(c-excel) then DO:
         C-Excel:ActiveWorkbook:save.
         
         if  i-num-ped-exec-rpw = 0 then
               C-Excel:visible  = YES.                   

  END.
    
    if valid-handle(c-relatorio) then 
          RELEASE OBJECT c-relatorio.
    if valid-handle(c-planilha) then 
          RELEASE OBJECT c-planilha.
    if valid-handle(c-excel) then 
          RELEASE OBJECT c-excel.


END PROCEDURE.

PROCEDURE pi-total .




    
   
       
               
        c-linha1 = c-linha1 +    
              chr(161) +
               
            string(d-qtde )      + chr(161) +
            string(d-peso)    + chr(161) +
            (IF d-qtde <> 0 THEN string(d-preco-total / d-qtde ) ELSE "")       + chr(161) +
            string(d-preco-total)         + chr(161) +
            string(d-total-ggf)       + chr(161) +
            string(d-preco-total-MOB)  + chr(161) +
            string(d-preco-total +
                   d-total-ggf +     
                   d-preco-total-MOB).

 

    
   
         i-linha1 = i-linha1 + 1.
        ASSIGN c-relatorio1:range("A" + string(i-linha1)):VALUE = c-linha1.
    

         ASSIGN        
               d-qtde = 0
               d-peso = 0
               d-preco-total     = 0
               d-total-ggf       = 0
               d-preco-total-MOB = 0              
               d-valor-total     = 0.

END PROCEDURE.
