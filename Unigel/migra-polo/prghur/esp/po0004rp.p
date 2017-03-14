/*****************************************************************************
* Programa: po0004rp.p
* Data....: 12/07/2003
* Autor...: Fl†vio Capitanio -  (11)9756-8761 - AFVIEW SERVIÄOS EMPRESARIAIS LTDA
* Objetivo: Arquivo MagnÇtico Participaá∆o Segurados - Saude UNIMED MONTENEGRO
* Vers∆o..: 2.06.000                            
*******************************************************************************/
/*---------------- Include de controle de Vers∆o ------------------*/ 
{include/i-prgvrs.i po0004RP 2.06.00.000}
{bf\buffersHCM.i} /*buffers de tabelas hcm para compilaá∆o totvs 12*/

/******** Definiá∆o Temp-table para Recebimento de Parametro **********************/
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
    field i-benef          like benefic_func.cdn_beneficio
    FIELD i-ep-ini         AS i
    FIELD i-ep-fim         AS i
    FIELD nr-fatura        AS c
    field i-ano-ref        as int form "9999"
    field i-mes-ref        as int form "99".

/* recebimento de parÉmetros */
def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/****************** Definiáao de Vari†veis de Processamento do Relat¢rio *********************/

/****************** Definiáao de  Frames e Forms do Relat¢rio 132 Colunas ***************************************/ 

/****************** INCLUDE COM VARIµVEIS GLOBAIS *********************/

/****************** FIM INCLUDE COM VARIµVEIS GLOBAIS *********************/

DEF TEMP-TABLE tt-saude
    FIELD cdn_empresa     LIKE funcionario.cdn_empresa
    FIELD cdn_estab       LIKE funcionario.cdn_estab
    FIELD cdn_funcionario LIKE funcionario.cdn_funcionario
    FIELD cod_rh_ccusto   LIKE funcionario.cod_rh_ccusto
    FIELD cdn_dependente  AS i
    FIELD nome_usuario    AS c FORM "x(40)"
    FIELD cdn_formula     LIKE movto_benefic.cdn_formul_calc_benefic
    FIELD nr-docto        LIKE movto_benefic.cod_docto_movto_benefic
    FIELD vlr_saude       AS de 
    FIELD seq-cons        AS i
    INDEX idx1 IS UNIQUE PRIMARY cdn_estab cdn_funcionario cdn_dependente cdn_formula nr-docto .

/****************** Definiáao de ParÉmetros do Relat¢rio *********************/ 

/****************** Definiáao de Vari†veis de Seleá∆o do Relat¢rio *********************/ 


/*include padr∆o para vari†veis para o log */
{include/i-rpvar.i}
/* definiá∆o de vari†veis e streams */
define stream s-exp.
define var h-acomp as handle no-undo.
define var v-cod-destino-impres as char   no-undo.
define var v-num-reg-lidos      as int    no-undo.
define var v-num-point          as int    no-undo.
define var v-num-set            as int    no-undo.
define var v-num-linha          as int    no-undo.
DEF    VAR ident-ant            AS c      NO-UNDO.
define var i-cont               as int    no-undo.
DEF    VAR c-set                AS c FORM "x(206)" .
DEF    VAR idx                  AS i.
DEF    VAR idx1                 AS i.
DEF    VAR cont                 AS i.
DEF    VAR i-consulta           AS i.
DEF    VAR valor                AS de FORM "zzz,zzz9.99".
DEF    VAR c-valor              AS c FORM "x(16)".
DEF    VAR c-str                AS c INIT "-,/,|,\,".
DEF    VAR ident                AS c FORM "x(20)".
DEF    VAR qtd                  AS INT.
DEF    VAR cod-amb              AS c.
DEF    VAR nr-docto             AS c FORM "x(24)".

/* definiá∆o de frames do log */

/* include padr∆o para output de log */
{include/i-rpout.i &STREAM="stream str-rp" &TOFILE=tt-param.arq-destino}

/* include com a definiá∆o da frame de cabeáalho e rodapÇ */
{include/i-rpcab.i &STREAM="str-rp"}

/* bloco principal do programa */

find first param_empres_rh no-lock no-error.
find empresa where
     empresa.ep-codigo = param_empres_rh.cdn_empresa 
     no-lock no-error.

assign c-empresa      = empresa.razao-social
       c-programa     = "po0004RP"
       c-versao       = "2.06"
       c-revisao      = "000"
       c-titulo-relat = "Participaá∆o Segurados Montenegro - Saude Unimed"
       c-sistema      = "Beneficios Sociais".

view stream str-rp frame f-cabec.
view stream str-rp frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Importando *}

run pi-inicializar in h-acomp (input RETURN-VALUE).

/* define a sa°da para o arquivo de sa°da informado na p†gina de parÉmetros */

/*-------------------------- bloco principal do programa -------------------------*/
assign v-num-reg-lidos = 0.

/******************* Tabelas Temporarias ******/
if SEARCH(arq-entrada) = ? THEN 
    message " Nao encontrado o arquivo " tt-param.arq-entrada skip
            view-as alert-box information. 
else
 do:
    input from value(arq-entrada) no-echo.
    bl-a:
    repeat:
      IMPORT UNFORMATTED  c-set.
      IF substr(c-set,15,4) <> "0576" THEN NEXT. 
      
      ASSIGN c-valor = SUBSTR(c-set,209,08)
             qtd     = INT(SUBSTR(c-set,136,12)) / 100
             cod-amb = SUBSTR(c-set,78,08)
             nr-docto = trim(substr(c-set,6,8)) + trim(substr(c-set,16,16)).
           
      DO cont = 1 TO NUM-ENTRIES(c-str):
         ASSIGN c-valor = REPLACE(c-valor,ENTRY(cont,c-str), "").
      END.
      ASSIGN valor = dec(c-valor). 
    
      ASSIGN v-num-reg-lidos = v-num-reg-lidos + 1.
      run pi-acompanhar in h-acomp(input string(v-num-reg-lidos) ).
      FIND FIRST benefic_func NO-LOCK WHERE 
                 (benefic_func.cdn_empresa   = "420" OR benefic_func.cdn_empresa   = "410" )AND /*solic-318*/ 
                 benefic_func.cdn_beneficio = tt-param.i-benef AND
                 benefic_func.nom_ident_benefic_func  = substr(c-set,20,6) NO-ERROR.

      IF NOT AVAIL benefic_func THEN
      DO:
         DISP stream str-rp
            /*  substr(c-set,15,4)   LABEL  "Codigo" */
              SUBSTR(c-set,32,26)  LABEL "Nome Usu†rio" FORM "x(26)"
              substr(c-set,16,16)  LABEL "Identif"     FORM "x(16)"
              SUBSTR(c-set,78,08)  LABEL "AMB" FORM "x(08)"
              SUBSTR(c-set,136,12) LABEL "Qtd"
              SUBSTR(c-set,194,12) LABEL "Unit" FORM "x(16)"
              SUBSTR(c-set,206,12) LABEL "Total" FORM "x(16)"
              "Benificio N∆o informado ou Identificaá∆o n∆o existe via BS0520" FORM "X(75)"      LABEL "Mensagem"
              WITH FRAME f-rel-132-a WIDTH 182 DOWN OVERLAY STREAM-IO.
         DOWN WITH FRAME f-rel-132-a.
         NEXT bl-a.
      END.
      IF AVAIL benefic_func THEN
         FIND funcionario OF benefic_func WHERE funcionario.dat_desligto_func = ? NO-LOCK NO-ERROR.
      ELSE NEXT bl-a.
      IF NOT AVAIL funcionario THEN
      DO:
         DISP stream str-rp
              SUBSTR(c-set,32,26)  LABEL "Nome Usu†rio" FORM "x(26)"
              substr(c-set,16,16)  LABEL "Identif"     FORM "x(16)"
              SUBSTR(c-set,78,08)  LABEL "AMB" FORM "x(08)"
              SUBSTR(c-set,136,12) LABEL "Qtd"
              SUBSTR(c-set,194,12) LABEL "Unit" FORM "x(16)"
              SUBSTR(c-set,206,12) LABEL "Total" FORM "x(16)"
              "Funcionario Desligado ou n∆o existe no cadastrado " FORM "X(75)"   LABEL "Mensagem"
              WITH FRAME f-rel-132-b WIDTH 182 DOWN OVERLAY STREAM-IO.
         DOWN WITH FRAME f-rel-132-b.
         NEXT bl-a.
      END.
      FIND efp_benefic WHERE 
           efp_benefic.cdn_empresa = funcionario.cdn_empresa AND
           efp_benefic.cdn_estab   = funcionario.cdn_estab   AND
           efp_benefic.cdn_beneficio = tt-param.i-benef NO-LOCK NO-ERROR.
       /** identificar qual formula devera ser empresa e funcioionario */

       IF ident-ant = "" THEN
          ASSIGN ident-ant = substr(c-set,20,6).

       IF ident-ant = substr(c-set,20,6) AND 
          (cod-amb = "00010014" OR cod-amb = "00010071") THEN
           ASSIGN i-consulta = i-consulta + 1.
       ELSE
          IF ident-ant <> substr(c-set,20,6) THEN
             ASSIGN i-consulta = 0.

       FIND tt-saude WHERE 
            tt-saude.cdn_empresa     = funcionario.cdn_empresa     AND 
            tt-saude.cdn_estab       = funcionario.cdn_estab       AND
            tt-saude.cdn_funcionario = funcionario.cdn_funcionario AND
            tt-saude.cdn_dependente  = 0                           AND 
            tt-saude.cdn_formula     = (IF i-consulta > 2 THEN 14
                                        ELSE
                                          IF (cod-amb = "00010014" OR cod-amb = "00010071") THEN /** consulta **/
                                               10
                                          ELSE
                                             IF AVAIL benefic_func AND SUBSTR(benefic_func.cod_livre_1,2,2)  = "23" THEN
                                                23
                                           ELSE 17)                   AND 
            tt-saude.nr-docto        =  nr-docto   NO-ERROR.
       IF AVAIL tt-saude THEN        
          ASSIGN TT-SAUDE.vlr_saude = tt-saude.vlr_saude + valor
                 tt-saude.seq-cons  = tt-saude.seq-cons + 1 .
          ELSE
          DO:
             CREATE tt-saude.
             ASSIGN
              tt-saude.cdn_empresa     = funcionario.cdn_empresa
              tt-saude.cdn_estab       = funcionario.cdn_estab
              tt-saude.cdn_funcionario = funcionario.cdn_funcionario
              tt-saude.cod_rh_ccusto   = funcionario.cod_rh_ccusto
              tt-saude.cdn_dependente  = 0              
              tt-saude.nome_usuario    = substr(c-set,16,40)
              tt-saude.cdn_formula     = (IF i-consulta > 2 THEN 14
                                          ELSE
                                            IF (cod-amb = "00010014" OR cod-amb = "00010071") THEN /** consulta **/
                                               10
                                               ELSE
                                               IF AVAIL benefic_func AND SUBSTR(benefic_func.cod_livre_1,2,2)  = "23" THEN
                                                  23
                                            ELSE 17)
              tt-saude.nr-docto        = nr-docto
              tt-saude.vlr_saude       = valor
              tt-saude.seq-cons        = 1 .
          END.
          ASSIGN ident-ant = substr(c-set,20,6).


        END.
        
 end. /* do */

/***************************************************************************/
 assign v-num-reg-lidos = 0
        cont            = 0
        idx             = 0.

FOR EACH TT-SAUDE BREAK BY tt-saude.cdn_funcionario:
    ASSIGN cont = IF tt-saude.cdn_formula = 10 THEN cont + 1 ELSE cont
           tt-saude.cdn_formula = IF tt-saude.cdn_formula = 10 AND cont > 2 THEN 14 ELSE  tt-saude.cdn_formula
           cont = IF tt-saude.cdn_formula = 10 AND cont > 2 THEN cont - 1 ELSE  cont .
    IF LAST-OF(tt-saude.cdn_funcionario) THEN ASSIGN cont = 0.


END.
assign v-num-reg-lidos = 0
       cont            = 0
       idx             = 0.
/** alterado em funá∆o da Datasul incorporar um novo indice ***/

DEF BUFFER b_movto_benefic FOR movto_benefic.
HIDE FRAME f-rel-132-a NO-PAUSE. 
HIDE FRAME f-rel-132-b NO-PAUSE. 
PAGE.
ASSIGN idx = 0.

FOR EACH
    tt-saude NO-LOCK:
    RUN pi-acompanhar in h-acomp(input "Nome: " + string(tt-saude.nome_usuario) ).
    FIND LAST b_movto_benefic USE-INDEX mvtbnfc_id  WHERE
          b_movto_benefic.cdn_empresa            = tt-saude.cdn_empresa AND
          b_movto_benefic.cdn_estab              = tt-saude.cdn_estab   AND 
          b_movto_benefic.cdn_funcionario        = tt-saude.cdn_funcionario AND
          b_movto_benefic.num_seq_movto_benefic  = idx NO-ERROR.

    IF AVAIL b_movto_benefic THEN
       ASSIGN idx = b_movto_benefic.num_seq_movto_benefic . 
    
    ASSIGN v-num-reg-lidos = v-num-reg-lidos + 1
           idx              = idx + 10.

    FIND FIRST conven_benefic WHERE                      
               conven_benefic.cdn_beneficio = tt-param.i-benef AND
               conven_benefic.cdn_estab     = string(tt-saude.cdn_funcionario) AND
               conven_benefic.cdn_empresa   = string(tt-saude.cdn_empresa) NO-LOCK NO-ERROR.
    FIND movto_benefic WHERE
         movto_benefic.cdn_empresa       = tt-saude.cdn_empresa AND
         movto_benefic.cdn_estab         = tt-saude.cdn_estab   AND 
         movto_benefic.cdn_funcionario   = tt-saude.cdn_funcionario AND
         movto_benefic.num_seq_movto_benefic  = idx NO-ERROR.
    IF AVAIL movto_benefic THEN
    DO:
       DISP stream str-rp
           tt-saude.cdn_empresa     LABEL "Emp"
           tt-saude.cdn_estab       LABEL "Est"
           tt-saude.cdn_funcionario LABEL "Matricula"
           tt-saude.nome_usuario    LABEL "Nome Usu†rio"
           tt-param.i-benef         LABEL "Cod.Benef."
           tt-saude.cdn_formula     LABEL "Formula"
           tt-saude.vlr_saude       LABEL "Valor"
           "Movimento J† Existe"   LABEL "Mensagem"
        WITH FRAME f-rel-132-f WIDTH 182 DOWN OVERLAY STREAM-io.
       DOWN WITH FRAME f-rel-132-f.
           NEXT.
    END.
    ELSE
     DO:
        DISP stream str-rp
             tt-saude.cdn_empresa     LABEL "Emp"
             tt-saude.cdn_estab       LABEL "Est"
             tt-saude.cdn_funcionario LABEL "Matricula"
             tt-saude.nome_usuario    LABEL "Nome Usu†rio"
             tt-param.i-benef         LABEL "Cod.Benef."
             tt-saude.cdn_formula     LABEL "Formula"
             tt-saude.vlr_saude       LABEL "Valor"
             "Importado com sucesso"  LABEL "Mensagem"
             WITH FRAME f-rel-132 WIDTH 182 DOWN OVERLAY STREAM-io.
       DOWN WITH FRAME f-rel-132.
    END.
   DISABLE TRIGGERS FOR LOAD OF movto_benefic.                   
   CREATE movto_benefic.
   ASSIGN
     movto_benefic.cdn_empresa                 = tt-saude.cdn_empresa
     movto_benefic.cdn_estab                   = tt-saude.cdn_estab
     movto_benefic.cdn_funcionario             = tt-saude.cdn_funcionario
     movto_benefic.num_seq_movto_benefic       = idx
     movto_benefic.num_seq_movto_benefic_orig  = idx
     movto_benefic.cdn_depend_func             = 0
     movto_benefic.cdn_beneficio               = tt-param.i-benef
     movto_benefic.num_ano_refer_movto_benefic = tt-param.i-ano-ref
     movto_benefic.num_mes_refer_movto_benefic = tt-param.i-mes-ref
     movto_benefic.cdn_formul_calc_benefic     = tt-saude.cdn_formula
     movto_benefic.cdn_prestdor_serv           = IF AVAIL conven_benefic THEN
                                                    conven_benefic.cdn_prestdor_serv
                                               ELSE 21 
     movto_benefic.cod_docto_movto_benefic     = tt-saude.nr-docto
     movto_benefic.qtd_unid_acordo_efp         = tt-saude.seq-cons
     movto_benefic.val_calcul_efp              = TT-SAUDE.vlr_saude
     movto_benefic.val_origin_movto_benefic    = TT-SAUDE.vlr_saude
     movto_benefic.num_ano_lote_movto_benefic  = tt-param.i-ano-ref 
     movto_benefic.num_mes_lote_movto_benefic  = tt-param.i-mes-ref 
     movto_benefic.num_lote_movto_benefic      = 0
     movto_benefic.idi_orig_movto_benefic      = 1
     movto_benefic.dat_pagto_efet_efp          = DATE(tt-param.i-mes-ref,01,tt-param.i-ano-ref)
     movto_benefic.dat_ocor_movto_benefic      = DATE(tt-param.i-mes-ref,01,tt-param.i-ano-ref)
     movto_benefic.log_pago_pelo_sist          = NO
     movto_benefic.log_gerac_pagto_prestdor    = NO
     movto_benefic.cod_rh_ccusto               = tt-saude.cod_rh_ccusto.
     
end. /****  FUNCIONARIO ****/

INPUT stream s-exp close.
/* fechamento do output do log */
{include/i-rpclo.i &STREAM="stream str-rp"}
run pi-finalizar in h-acomp.
return "OK":U.
/* fim do programa */
