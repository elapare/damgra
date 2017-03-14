/*****************************************************************************
* Programa: po0034rp.p
* Data....: 21.06.04
* Autor...: Fl vio Capitanio -  (11)9756-8761 - AFVIEW SERVI€OS EMPRESARIAIS LTDA
* Objetivo: Arquivo Magn‚tico Banco Alplha
* VersÆo..: 2.06.000                            
*           
*******************************************************************************/
/*---------------- Include de controle de VersÆo ------------------*/ 
{include/i-prgvrs.i po0034RP 2.06.00.000}
{bf\buffersHCM.i} /*buffers de tabelas hcm para compila‡Æo totvs 12*/


/******** Defini‡Æo Temp-table para Recebimento de Parametro **********************/
def temp-table tt-raw-digita
    field raw-digita as raw.

define temp-table tt-param
    field destino          as integer
    field arq-destino      as char
    field arq-entrada      as char
    field arq-saida        as char
    field todos            as integer
    field usuario          as char
    field data-exec        as date
    field hora-exec        as integer
    field i-benef          like benefic_func.cdn_beneficio
    FIELD i-ep-ini         AS i
    FIELD i-ep-fim         AS i
    field i-ano-ref        as int form "9999"
    field i-mes-ref        as int form "99".

/* recebimento de parƒmetros */
def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

create tt-param.
    raw-transfer raw-param to tt-param.

DEF TEMP-TABLE tt-imp
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
    INDEX idx1 IS UNIQUE PRIMARY cdn_estab cdn_funcionario cdn_dependente cdn_formula nr-docto.

/****************** Defini‡ao de Parƒmetros do Relat¢rio *********************/ 

/****************** Defini‡ao de Vari veis de Sele‡Æo do Relat¢rio *********************/ 


/*include padrÆo para vari veis para o log */
{include/i-rpvar.i}
/* defini‡Æo de vari veis e streams */
define stream s-exp.
define var h-acomp as handle no-undo.
define var v-cod-destino-impres as char   no-undo.
define var v-num-reg-lidos      as int    no-undo.
define var v-num-point          as int    no-undo.
define var v-num-set            as int    no-undo.
define var v-num-linha          as int    no-undo.
define var i-cont               as int    no-undo.
DEF    VAR c-set                AS c FORM "x(250)" .
DEF    VAR idx                  AS i.
DEF STREAM s-saida.
/* defini‡Æo de frames do log */

/* include padrÆo para output de log */
{include/i-rpout.i &STREAM="stream str-rp" &TOFILE=tt-param.arq-destino}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i &STREAM="str-rp"}

/* bloco principal do programa */

find first param_empres_rh no-lock no-error.
find empresa where
     empresa.ep-codigo = param_empres_rh.cdn_empresa 
     no-lock no-error.

assign c-empresa      = empresa.razao-social
       c-programa     = "po0034RP"
       c-versao       = "2.06"
       c-revisao      = "000"
       c-titulo-relat = "Arquivo Magn‚tico - Empr‚stimo Banco Alpha"
       c-sistema      = "Beneficios Sociais".

view stream str-rp frame f-cabec.
view stream str-rp frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Importando *}

run pi-inicializar in h-acomp (input RETURN-VALUE).

/* define a sa¡da para o arquivo de sa¡da informado na p gina de parƒmetros */

/*-------------------------- bloco principal do programa -------------------------*/
assign v-num-reg-lidos = 0.

/******************* Tabelas Temporarias ******/
if SEARCH(arq-entrada) = ? THEN 
    message " Nao encontrado o arquivo " tt-param.arq-entrada skip
            view-as alert-box information. 
else
 do:
    OUTPUT STREAM s-saida TO VALUE(tt-param.arq-saida).   
    input from value(arq-entrada) no-echo.
    RUN pi-troca-registro.
    OUTPUT STREAM s-saida CLOSE.
    INPUT CLOSE.
    input from value(arq-entrada) no-echo.
    repeat:
      IMPORT UNFORMATTED  c-set.
      IF substr(c-set,2,1) <> "1" THEN NEXT.
      
      FIND FIRST funcionario WHERE 
                 funcionario.cdn_funcionario = INT(substr(c-set,10,8))  NO-LOCK NO-ERROR.
      run pi-acompanhar in h-acomp(input " Mat.: " + substr(c-set,10,8)).
      IF AVAIL funcionario AND funcionario.dat_desligto_func <> ? THEN
      DO:
        FIND FIRST funcionario WHERE 
                   funcionario.cdn_funcionario = INT(substr(c-set,10,8)) AND
                   funcionario.dat_desligto_func = ? NO-LOCK NO-ERROR.
        IF NOT AVAIL funcionario THEN
        DO:
           DISP stream str-rp
            substr(c-set,10,8)    LABEL "Mat  "    FORM "zzzzz9"
            substr(c-set,88,40)   LABEL "Nome Usu rio" FORM "x(30)"
            substr(c-set,31,14)   LABEL "Valor" FORM "zzz,zzz,zz9.99"
            "Funcionario NÆo existe " FORM "X(60)"      LABEL "Mensagem"
           WITH FRAME f-rel-132-c WIDTH 182 DOWN OVERLAY STREAM-IO.
          DOWN WITH FRAME f-rel-132-c.
          NEXT.
        END.
      END.

      IF NOT AVAIL funcionario THEN
      DO:
           DISP stream str-rp
                substr(c-set,10,8)    LABEL "Mat  "    FORM "zzzzz9"
                substr(c-set,88,40)   LABEL "Nome Usu rio" FORM "x(30)"
                substr(c-set,31,14)   LABEL "Valor" FORM "zzz,zzz,zz9.99"
                 "Funcionario NÆo existe "  FORM "X(60)"      LABEL "Mensagem"
                WITH FRAME f-rel-132-a WIDTH 182 DOWN OVERLAY STREAM-IO.
           DOWN WITH FRAME f-rel-132-a.
           NEXT.
      END.
      FIND tt-imp WHERE 
            tt-imp.cdn_empresa     = funcionario.cdn_empresa     AND 
            tt-imp.cdn_estab       = funcionario.cdn_estab       AND
            tt-imp.cdn_funcionario = funcionario.cdn_funcionario AND
            tt-imp.cdn_dependente  = 0                           AND 
            tt-imp.cdn_formula     = 601                         AND 
            tt-imp.nr-docto        =  substr(c-set,215,30)  NO-ERROR.
       IF AVAIL tt-imp THEN        
          ASSIGN tt-imp.vlr_saude = tt-imp.vlr_saude + dec(substr(c-set,31,14)) / 100
                 tt-imp.seq-cons  = tt-imp.seq-cons + 1 .
          ELSE
          DO:
             CREATE tt-imp.
             ASSIGN
              tt-imp.cdn_empresa     = funcionario.cdn_empresa
              tt-imp.cdn_estab       = funcionario.cdn_estab
              tt-imp.cdn_funcionario = funcionario.cdn_funcionario
              tt-imp.cod_rh_ccusto   = funcionario.cod_rh_ccusto
              tt-imp.cdn_dependente  = 0
              tt-imp.nome_usuario    = substr(c-set,88,40)
              tt-imp.cdn_formula     = 601
              tt-imp.nr-docto        = substr(c-set,215,30)
              tt-imp.vlr_saude       = dec(substr(c-set,31,14)) / 100
              tt-imp.seq-cons        = 1 .
          END.
                     
        END.
                     
 end. /* do */

/***************************************************************************/
assign v-num-reg-lidos = 0.

FIND LAST movto_benefic NO-ERROR.
IF AVAIL movto_benefic THEN
   ASSIGN idx = movto_benefic.num_seq_movto_benefic. 
HIDE FRAME f-rel-132-a NO-PAUSE. 
PAGE.
FOR EACH
    tt-imp NO-LOCK:
    FIND FIRST conven_benefic WHERE                      
               conven_benefic.cdn_beneficio = tt-param.i-benef AND
               conven_benefic.cdn_estab     = tt-imp.cdn_estab AND
               conven_benefic.cdn_empresa   = tt-imp.cdn_empresa NO-LOCK NO-ERROR.
   
    DISP stream str-rp
         tt-imp.cdn_empresa       LABEL "Emp"
         tt-imp.cdn_estab         LABEL "Est"
         tt-imp.cdn_funcionario   LABEL "Matricula"
         tt-imp.nome_usuario      LABEL "Nome Usu rio"
         tt-param.i-benef         LABEL "Cod.Benef."
         tt-imp.cdn_formula       LABEL "Formula"
         tt-imp.vlr_saude         LABEL "Valor"
         "Importado com sucesso"  LABEL "Mensagem"
         WITH FRAME f-rel-132 WIDTH 182 DOWN OVERLAY STREAM-io.
   DOWN WITH FRAME f-rel-132.
   ASSIGN v-num-reg-lidos = v-num-reg-lidos + 1.
   run pi-acompanhar in h-acomp("Nome: " + tt-imp.nome_usuario).
                 
   CREATE movto_benefic.
   ASSIGN
     idx                                       = idx + 1
     movto_benefic.num_seq_movto_benefic       = idx
     movto_benefic.num_seq_movto_benefic_orig  = idx
     movto_benefic.cdn_empresa                 = tt-imp.cdn_empresa
     movto_benefic.cdn_estab                   = tt-imp.cdn_estab
     movto_benefic.cdn_funcionario             = tt-imp.cdn_funcionario
     movto_benefic.cdn_depend_func             = 0
     movto_benefic.cdn_beneficio               = tt-param.i-benef
     movto_benefic.num_ano_refer_movto_benefic = tt-param.i-ano-ref
     movto_benefic.num_mes_refer_movto_benefic = tt-param.i-mes-ref
     movto_benefic.cdn_formul_calc_benefic     = tt-imp.cdn_formula
     movto_benefic.cdn_prestdor_serv           = IF AVAIL conven_benefic THEN
                                                    conven_benefic.cdn_prestdor_serv
                                               ELSE 
                                                  IF tt-imp.cdn_empresa = "420" or tt-imp.cdn_empresa = "410" THEN 122  /*solic-318*/ 
                                                  ELSE 22
     movto_benefic.cod_docto_movto_benefic     = tt-imp.nr-docto
     movto_benefic.qtd_unid_acordo_efp         = tt-imp.seq-cons
     movto_benefic.val_calcul_efp              = tt-imp.vlr_saude
     movto_benefic.val_origin_movto_benefic    = tt-imp.vlr_saude
     movto_benefic.num_ano_lote_movto_benefic  = tt-param.i-ano-ref 
     movto_benefic.num_mes_lote_movto_benefic  = tt-param.i-mes-ref 
     movto_benefic.num_lote_movto_benefic      = 0
     movto_benefic.idi_orig_movto_benefic      = 1
     movto_benefic.dat_pagto_efet_efp          = DATE(tt-param.i-mes-ref,01,tt-param.i-ano-ref)
     movto_benefic.dat_ocor_movto_benefic      = DATE(tt-param.i-mes-ref,01,tt-param.i-ano-ref)
     movto_benefic.log_pago_pelo_sist          = NO
     movto_benefic.log_gerac_pagto_prestdor    = NO
     movto_benefic.cdn_motiv_lote_benefic      = 1
     movto_benefic.cod_rh_ccusto               = tt-imp.cod_rh_ccusto.
                   
end. /****  FUNCIONARIO ****/
PROCEDURE pi-troca-registro:
   repeat:
     IMPORT UNFORMATTED  c-set.
     IF substr(c-set,2,1) = "0" THEN 
        SUBSTR(c-set,11,8) = "2RETORNO".
     PUT STREAM s-saida c-set SKIP.
  END.
END.
INPUT stream s-exp close.

/*OS-COPY VALUE(tt-param.arq-entrada)  VALUE(tt-param.arq-saida). */
/* fechamento do output do log */
{include/i-rpclo.i &STREAM="stream str-rp"}
run pi-finalizar in h-acomp.
return "OK":U.
/* fim do programa */
