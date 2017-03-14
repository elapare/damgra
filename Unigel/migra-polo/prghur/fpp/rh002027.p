/*****************************************************************************
**       Programa: rh002027.p
**       Data....: 13/02/17
**       Autor...: DATASUL S.A.
**       Objetivo: Funcionarios para AD
**       VersÆo..: 1.00.000 - geblima
**       OBS.....: Este fonte foi gerado pelo Data Viewer 3.00
*******************************************************************************/

define variable c-prog-gerado as character no-undo initial "RH002027".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

run grapi/gr2013.p (input c-prog-gerado, input "2.00.00.000").

/****************** Definição de Tabelas Temporárias do Relatório **********************/

define temp-table tt-raw-digita
    field raw-digita as raw.

define temp-table tt-param
    field destino              as integer
    field arquivo              as char
    field usuario              as char
    field data-exec            as date
    field hora-exec            as integer
    field parametro            as logical
    field formato              as integer
    field v_num_tip_aces_usuar as integer
    field ep-codigo            like mguni.empresa.ep-codigo
    field i-cdn_empresa-ini like funcionario.cdn_empresa
    field i-cdn_empresa-fim like funcionario.cdn_empresa
    field i-cdn_estab-ini like funcionario.cdn_estab
    field i-cdn_estab-fim like funcionario.cdn_estab
    field i-cdn_funcionario-ini like funcionario.cdn_funcionario
    field i-cdn_funcionario-fim like funcionario.cdn_funcionario
    field da-dat_admis_func-ini like funcionario.dat_admis_func
    field da-dat_admis_func-fim like funcionario.dat_admis_func
.

/****************** INCLUDE COM VARIµVEIS GLOBAIS *********************/

def new global shared var i-ep-codigo-usuario  like mguni.empresa.ep-codigo no-undo.
def new Global shared var l-implanta           as logical    init no.
def new Global shared var c-seg-usuario        as char format "x(12)" no-undo.
def new global shared var i-num-ped-exec-rpw  as integer no-undo.   
def new global shared var i-pais-impto-usuario as integer format ">>9" no-undo.
def new global shared var l-rpc as logical no-undo.
def new global shared var r-registro-atual as rowid no-undo.
def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var i-num-ped as integer no-undo.         
def new global shared var v_cdn_empres_usuar   like mguni.empresa.ep-codigo        no-undo.
def new global shared var v_cod_usuar_corren   like emsfnd.usuar_mestre.cod_usuario no-undo.
def new global shared var h_prog_segur_estab     as handle                   no-undo.
def new global shared var v_cod_grp_usuar_lst    as char                     no-undo.
def new global shared var v_num_tip_aces_usuar   as int                      no-undo.
def new global shared var rw-log-exec            as rowid                    no-undo.


def new global shared var c-dir-spool-servid-exec as CHAR no-undo.
/****************** Defini‡ao de Parƒmetros do Relat¢rio *********************/ 

/****************** Defini‡ao de Vari veis de Sele‡Æo do Relat¢rio *********************/ 

def new shared var i-cdn_empresa-ini like funcionario.cdn_empresa format "x(3)" initial "" no-undo.
def new shared var i-cdn_empresa-fim like funcionario.cdn_empresa format "x(3)" initial "ZZZ" no-undo.
def new shared var i-cdn_estab-ini like funcionario.cdn_estab format "x(5)" initial "" no-undo.
def new shared var i-cdn_estab-fim like funcionario.cdn_estab format "x(5)" initial "ZZZZZ" no-undo.
def new shared var i-cdn_funcionario-ini like funcionario.cdn_funcionario format "zzzzzzz9" initial 0 no-undo.
def new shared var i-cdn_funcionario-fim like funcionario.cdn_funcionario format "zzzzzzz9" initial 99999999 no-undo.
def new shared var da-dat_admis_func-ini like funcionario.dat_admis_func format "99/99/9999" initial "01/01/1800" no-undo.
def new shared var da-dat_admis_func-fim like funcionario.dat_admis_func format "99/99/9999" initial "12/31/9999" no-undo.

/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 

/****************** Defini‡ao de Vari veis Campo Calculado do Relat¢rio **********************/ 

def var Adname as character.
def var ccusto as character.
def var Description as character.
def var Endereco as character.

/****************** Defini‡ao de Vari veis do Relat¢rio NÆo Pedidas em Tela ******************/ 

/****************** Defini‡ao de Vari veis de Total do Relat¢rio *****************************/ 

/****************** Defini‡ao de Vari veis dos Calculos do Relat¢rio *************************/ 

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

/***************** Defini‡ao de Vari veis de Processamento do Relat¢rio *********************/

def var h-acomp              as handle no-undo.
def var h-FunctionLibrary    as handle no-undo.
def var v-cod-destino-impres as char   no-undo.
def var v-num-reg-lidos      as int    no-undo.
def var v-num-point          as int    no-undo.
def var v-num-set            as int    no-undo.
def var v-num-linha          as int    no-undo.
def var v-cont-registro      as int    no-undo.
def var v-des-retorno        as char   no-undo.
def var v-des-ext            as char   no-undo.
def var v-des-local-layout   as char   no-undo.

/****************** Defini‡ao de Forms do Relat¢rio 132 Colunas ***************************************/ 

form funcionario.nom_pessoa_fisic column-label "Nome" format "x(40)" at 001
     rh_estab.nom_pessoa_jurid column-label "Nome" format "x(40)" at 042
     ccusto column-label "Centro de Custo" format "x(12)" at 083 skip
     Adname column-label "AD Name" format "x(40)" at 001 skip
     Endereco column-label "Endereco" format "x(100)" at 001
     rh_pessoa_jurid.nom_cidad_rh column-label "Cidade" format "x(25)" at 102
     rh_pessoa_jurid.cod_unid_federac_rh column-label "UF" format "x(04)" at 128 skip
     rh_pessoa_jurid.cod_cep_rh column-label "CEP" format "99999-999" at 001
     cargo.des_cargo column-label "Descri‡Æo" format "x(36)" at 011
     rh_ccusto.des_rh_ccusto column-label "Descri‡Æo" format "x(40)" at 048 skip
     rh_pessoa_fisic_ext_esp.fone_rede column-label "Fone" format "x(100)" at 001 skip
     Description column-label "Description" format "x(40)" at 001
     rh_pessoa_fisic_ext_esp.login_rede column-label "Logim" format "x(30)" at 042
     with down width 132 no-box stream-io frame f-relat-09-132.

create tt-param.
raw-transfer raw-param to tt-param.

def temp-table tt-editor no-undo
    field linha      as integer
    field conteudo   as character format "x(80)"
    index editor-id is primary unique linha.


def var rw-log-exec                            as rowid no-undo.
def var c-erro-rpc as character format "x(60)" initial " " no-undo.
def var c-erro-aux as character format "x(60)" initial " " no-undo.
def var c-ret-temp as char no-undo.
def var h-servid-rpc as handle no-undo.     
define var c-empresa       as character format "x(40)"      no-undo.
define var c-titulo-relat  as character format "x(50)"      no-undo.
define var i-numper-x      as integer   format "ZZ"         no-undo.
define var da-iniper-x     as date      format "99/99/9999" no-undo.
define var da-fimper-x     as date      format "99/99/9999" no-undo.
define var i-page-size-rel as integer                       no-undo.
define var c-programa      as character format "x(08)"      no-undo.
define var c-versao        as character format "x(04)"      no-undo.
define var c-revisao       as character format "999"        no-undo.

define new shared var c-impressora   as character                      no-undo.
define new shared var c-layout       as character                      no-undo.
define new shared var v_num_count     as integer                       no-undo.
define new shared var c-arq-control   as character                     no-undo.
define new shared var c-sistema       as character format "x(25)"      no-undo.
define new shared var c-rodape        as character                     no-undo.

define new shared buffer b_ped_exec_style for emsfnd.ped_exec.
define new shared buffer b_servid_exec_style for emsfnd.servid_exec.

define new shared stream str-rp.


if connected("dthrpyc") then do:
  def var v_han_fpapi003 as handle         no-undo.
  def VAR v_log_per_sal  as log    init no no-undo.
  run prghur/fpp/fpapi003.p persistent set v_han_fpapi003 (input tt-param.usuario,
                                                           input tt-param.v_num_tip_aces_usuar).
  RUN prghur/fpp/fpapi006.p (INPUT  v_cod_usuar_corren, 
                             INPUT  v_num_tip_aces_usuar, 
                             INPUT  v_cod_grp_usuar_lst, 
                             OUTPUT v_log_per_sal).
end.


assign c-programa     = "rh002027"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "Funcionarios para AD"
       c-sistema      = "".


find first mguni.empresa no-lock
    where mguni.empresa.ep-codigo = i-ep-codigo-usuario no-error.
if  avail mguni.empresa
then
    assign c-empresa = mguni.empresa.razao-social.
else
    assign c-empresa = "".

if  tt-param.formato = 2 then do:


form header
    fill("-", 132) format "x(132)" skip
    c-empresa c-titulo-relat at 50
    "Folha:" at 122 page-number(str-rp) at 128 format ">>>>9" skip
    fill("-", 112) format "x(110)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 132 no-labels no-box page-top frame f-cabec.

form header
    fill("-", 132) format "x(132)" skip
    c-empresa c-titulo-relat at 50
    "Folha:" at 122 page-number(str-rp) at 128 format ">>>>9" skip
    "Periodo:" i-numper-x at 08 "-"
    da-iniper-x at 14 "to" da-fimper-x
    fill("-", 74) format "x(72)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 132 no-labels no-box page-top frame f-cabper.

run grapi/gr2004.p.

form header
    c-rodape format "x(132)"
    with stream-io width 132 no-labels no-box page-bottom frame f-rodape.


end. /* tt-param.formato = 2 */


run grapi/gr2013c.p (input tt-param.destino,
                    input tt-param.arquivo,
                    input tt-param.usuario,
                    input yes).

assign i-ep-codigo-usuario = tt-param.ep-codigo
       v_cdn_empres_usuar  = i-ep-codigo-usuario
       i-cdn_empresa-ini = tt-param.i-cdn_empresa-ini
       i-cdn_empresa-fim = tt-param.i-cdn_empresa-fim
       i-cdn_estab-ini = tt-param.i-cdn_estab-ini
       i-cdn_estab-fim = tt-param.i-cdn_estab-fim
       i-cdn_funcionario-ini = tt-param.i-cdn_funcionario-ini
       i-cdn_funcionario-fim = tt-param.i-cdn_funcionario-fim
       da-dat_admis_func-ini = tt-param.da-dat_admis_func-ini
       da-dat_admis_func-fim = tt-param.da-dat_admis_func-fim
.

def var l-imprime as logical no-undo.

assign l-imprime = no.
DEFINE BUFFER empresa   for ems5.empresa.
DEFINE VARIABLE c-nome       AS CHARACTER FORMAT "X(50)"  NO-UNDO.
DEFINE VARIABLE c-nome-aux   AS CHARACTER FORMAT "X(50)"  NO-UNDO.
DEFINE VARIABLE i-cont       AS INTEGER     NO-UNDO.

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

assign v-num-reg-lidos = 0.

/* gr9020f.p */

for each funcionario no-lock
         where funcionario.cdn_empresa >= i-cdn_empresa-ini and 
               funcionario.cdn_empresa <= i-cdn_empresa-fim and
               funcionario.cdn_estab >= i-cdn_estab-ini and 
               funcionario.cdn_estab <= i-cdn_estab-fim and
               funcionario.cdn_funcionario >= i-cdn_funcionario-ini and 
               funcionario.cdn_funcionario <= i-cdn_funcionario-fim and
               funcionario.dat_admis_func >= da-dat_admis_func-ini and 
               funcionario.dat_admis_func <= da-dat_admis_func-fim,
    each rh_pessoa_fisic_ext_esp no-lock
         where rh_pessoa_fisic_ext_esp.num_pessoa_fisic = funcionario.num_pessoa_fisic,
    each unid_lotac_plano no-lock
         where unid_lotac_plano.cdn_plano_lotac = funcionario.cdn_plano_lotac and
               unid_lotac_plano.cod_unid_lotac = funcionario.cod_unid_lotac,
    each unid_lotac no-lock
         where unid_lotac.cod_unid_lotac = unid_lotac_plano.cod_unid_lotac,
    each rh_ccusto no-lock
         where rh_ccusto.cdn_empresa = funcionario.cdn_empresa and
               rh_ccusto.cod_rh_ccusto = funcionario.cod_rh_ccusto,
    each cargo no-lock
         where cargo.cdn_cargo_basic = funcionario.cdn_cargo_basic and
               cargo.cdn_niv_cargo = funcionario.cdn_niv_cargo,
    each rh_estab no-lock
         where rh_estab.cdn_empresa = funcionario.cdn_empresa and
               rh_estab.cdn_estab = funcionario.cdn_estab,
    each rh_pessoa_jurid no-lock
         where rh_pessoa_jurid.num_pessoa_jurid = rh_estab.num_pessoa_jurid,
    each empresa no-lock
         where empresa.cod_empresa = funcionario.cdn_empresa and
               funcionario.dat_desligto_func  = ? and
               rh_pessoa_fisic_ext_esp.login_rede  <> ""
    break by funcionario.cdn_estab
          by funcionario.nom_pessoa_fisic:
    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

    assign ccusto = funcionario.cdn_estab + "-" + funcionario.cod_rh_ccusto .
    assign Adname = "".
    assign Endereco = rh_pessoa_jurid.nom_ender_rh  + ", " + string(rh_pessoa_jurid.num_livre_1) + " - " + trim(substring(rh_pessoa_jurid.cod_livre_1,1,20)).
    assign Description = if funcionario.cdn_estab = "321" then "UNG USP - " + cargo.des_cargo else if funcionario.cdn_estab = "351" then "UNG CSP - " + cargo.des_cargo else if funcionario.cdn_estab = "352" then "UNG CCB - " + cargo.des_cargo else if funcionario.cdn_estab = "353" then "UNG CSJ - " + cargo.des_cargo else if funcionario.cdn_estab = "354" then "UNG CGJ - " + cargo.des_cargo else if funcionario.cdn_estab = "362" then "UNG ECM - " + cargo.des_cargo else if funcionario.cdn_estab = "381" then "UNG LCN - " + cargo.des_cargo else if funcionario.cdn_estab = "383" then "UNG LSP - " + cargo.des_cargo else if funcionario.cdn_estab = "391" then "UNG PCN - " + cargo.des_cargo else if funcionario.cdn_estab = "392" then "UNG PSP - " + cargo.des_cargo else if funcionario.cdn_estab = "393" then "UNG PCM - " + cargo.des_cargo else if funcionario.cdn_estab = "401" then "UNG ACM - " + cargo.des_cargo else if funcionario.cdn_estab = "404" then "UNG ACM - " + cargo.des_cargo else if funcionario.cdn_estab = "421" OR funcionario.cdn_estab = "411" then "UNG PSP - " + cargo.des_cargo else if funcionario.cdn_estab = "422" OR funcionario.cdn_estab = "412" then "UNG PMT - " + cargo.des_cargo else if funcionario.cdn_estab = "341" then "TRS CSP - " + cargo.des_cargo else if funcionario.cdn_estab = "342" then "TRS CGJ - " + cargo.des_cargo else if funcionario.cdn_estab = "343" then "TRS SPP - " + cargo.des_cargo else "Editar RH0020AA"./*solic-318*/
    assign l-imprime = yes.
    ASSIGN c-nome = LC(funcionario.nom_pessoa_fisic ).
    ASSIGN c-nome = REPLACE(c-nome," ","|").
    DO i-cont = 1 TO NUM-ENTRIES(c-nome,"|"):
        ASSIGN c-nome-aux = ENTRY(i-cont,c-nome,"|").
        IF NOT CAN-DO("de,da,do,e",c-nome-aux) THEN
            ASSIGN OVERLAY(c-nome-aux,1,1)  = CAPS(SUBSTRING(c-nome-aux,1,1)).
        ASSIGN Adname = Adname + c-nome-aux + " ".
    END.
    ASSIGN Adname = SUBSTRING(Adname,1,LENGTH(Adname) - 1).
    put stream str-rp unformatted trim(funcionario.nom_pessoa_fisic) "*"
            trim(rh_estab.nom_pessoa_jurid) "*"
            trim(ccusto) "*"
            trim(Adname) "*"
            trim(Endereco) "*"
            trim(rh_pessoa_jurid.nom_cidad_rh) "*"
            trim(rh_pessoa_jurid.cod_unid_federac_rh) "*"
            trim(rh_pessoa_jurid.cod_cep_rh) "*"
            trim(cargo.des_cargo) "*"
            trim(rh_ccusto.des_rh_ccusto) "*"
            trim(rh_pessoa_fisic_ext_esp.fone_rede) "*"
            trim(Description) "*"
            trim(rh_pessoa_fisic_ext_esp.login_rede) skip.
    
end.




if connected("dthrpyc") then
  delete procedure v_han_fpapi003.

    output stream str-rp close.

procedure pi-print-editor:

    def input param c-editor    as char    no-undo.
    def input param i-len       as integer no-undo.

    def var i-linha  as integer no-undo.
    def var i-aux    as integer no-undo.
    def var c-aux    as char    no-undo.
    def var c-ret    as char    no-undo.

    for each tt-editor:
        delete tt-editor.
    end.

    assign c-ret = chr(255) + chr(255).

    do  while c-editor <> "":
        if  c-editor <> "" then do:
            assign i-aux = index(c-editor, chr(10)).
            if  i-aux > i-len or (i-aux = 0 and length(c-editor) > i-len) then
                assign i-aux = r-index(c-editor, " ", i-len + 1).
            if  i-aux = 0 then
                assign c-aux = substr(c-editor, 1, i-len)
                       c-editor = substr(c-editor, i-len + 1).
            else
                assign c-aux = substr(c-editor, 1, i-aux - 1)
                       c-editor = substr(c-editor, i-aux + 1).
            if  i-len = 0 then
                assign entry(1, c-ret, chr(255)) = c-aux.
            else do:
                assign i-linha = i-linha + 1.
                create tt-editor.
                assign tt-editor.linha    = i-linha
                       tt-editor.conteudo = c-aux.
            end.
        end.
        if  i-len = 0 then
            return c-ret.
    end.
    return c-ret.
end procedure.

IF VALID-HANDLE(h-acomp) THEN /*gr9030g*/
    RUN pi-finalizar IN h-acomp NO-ERROR.

return 'OK'.

/* fim do programa */
