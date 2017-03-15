/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer empresa for mgmulti.empresa.
{include/i-prgvrs.i FT0518F 2.00.00.000}  /*** 010000 ***/

&if "{&EMSFND_VERSION}" >= "1.00" &then
    {include/i-license-manager.i ft0518f MFT}
&endif

/******************************************************************************
**
**  Programa: FT0518F.P
**
**  Objetivo: Carregar informaá‰es das tabelas tempor†rias para impress∆o
**            da NF-e baseado no retorno das TT's do programa axsep017
**
******************************************************************************/

{cdp/cdcfgdis.i}

DEF TEMP-TABLE ttArquivo NO-UNDO
      FIELD sequencia   AS INT
      FIELD nomeArquivo AS CHAR
      INDEX idx1 sequencia.

/**** Parametros ****/
DEF INPUT-OUTPUT PARAM TABLE FOR ttArquivo.

/**** Variaveis ****/
DEFINE VARIABLE h-axsep017                   AS HANDLE      NO-UNDO.
DEFINE VARIABLE h-bcapi016                   AS HANDLE      NO-UNDO.
DEFINE VARIABLE iTpAmbSEFAZ                  AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-cod-uf-ibge                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-mult-nfe                   AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-count-nfe                  AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-soma-mod-nfe               AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-dig-ver-nfe                AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-cont-itens                 AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-modelo-DANFE               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-chave-acesso-adicional-nfe AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-cont                       AS INTEGER     NO-UNDO.
DEFINE VARIABLE l-sem-Word                   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE c-desc-mod-frete             AS CHARACTER   NO-UNDO.

/****  Variaveis Compartilhadas  ****/
DEFINE SHARED VAR r-nota       AS ROWID.
DEFINE SHARED VAR c-hr-saida   AS CHAR    FORMAT "xx:xx:xx" INIT "000000".
DEFINE SHARED VAR l-dt         AS LOGICAL FORMAT "Sim/Nao"  INIT NO.

/*customizaá∆o FCI EDSON - 31-10-2013*/
DEFINE VARIABLE h-bodi538 AS HANDLE      NO-UNDO.
DEFINE VARIABLE c-nro-FCI AS CHARACTER   NO-UNDO.
RUN dibo/bodi538.p PERSISTENT SET h-bodi538.


/*customizacao para permitir layout por empresa */
DEFINE NEW GLOBAL SHARED VARIABLE c-empresa-layout AS CHARACTER   NO-UNDO.
def var c-pedido          as char format "x(15)".
DEFINE VARIABLE c-desc-prod AS CHARACTER  NO-UNDO.
DEFINE VARIABLE l-desc-prod AS LOGICAL    NO-UNDO.
/*customizaá∆o unigel comercial*/
define buffer if-ped-venda for espmulti.if-ped-venda.

/*customizado POLO FILMS*/
DEFINE VARIABLE c-esp-ext AS CHARACTER   NO-UNDO.
def var c-nome-tr as char no-undo.
def var i-cont-item as integer no-undo.
DEF BUFFER buc-estabelec         for estabelec.  /*unigel comercial*/
DEF BUFFER b-var-result          FOR var-result.  /*LARGURA NA DESCRICAO*/
DEF BUFFER bvar-result           FOR var-result.
DEFINE BUFFER b-it-nota-fisc-ori FOR it-nota-fisc.
def buffer b-it-nota-fisc-jr FOR it-nota-fisc.
DEFINE BUFFER b-ped-item-ori FOR ped-item.
def buffer b-ped-venda for ped-venda.
DEFINE BUFFER b-ped-venda-ori FOR ped-venda.
DEFINE BUFFER b-nota-fiscal-ori  FOR nota-fiscal.
DEF BUFFER b-it-nota-fisc FOR it-nota-fisc.
def var c-mess-item      as char format "x(200)".
def var c-mess-laudo     as char format "x(200)".
DEF var PEDCLI-NF        as character format "x(16)".
DEF var PRODCLI-NF       as character format "x(16)".
Def Var larg-jr          As INTEGER.
DEF VAR c-end-tr-rd      AS CHAR   FORM "x(82)" no-undo.
def var ct               as int no-undo.
def var l-prim-mess      as logical.
def var sai-laudo-jr     as char no-undo.

def var am-vlipiit-tt    as dec no-undo.
def var am-vlbcicmit-tt  as dec no-undo.
def var am-vlicmit-tt    as dec no-undo.
/*faturamento por bobina*/
DEF var unfat            AS CHARACTER   NO-UNDO.
def var de-qt-fatur-un   as   decimal format ">>>>,>>9.9999"    no-undo.
def var de-pr-fatur-un   as   decimal format ">>>>,>>9.9999"    no-undo.

/*fim customizado*/
             
/*customizacao estabeleciento , empresa unigel comercial*/
DEFINE VARIABLE i-emp-uc AS char    NO-UNDO.
DEFINE VARIABLE c-estab-uc AS CHARACTER  NO-UNDO.



DEFINE VARIABLE l-item-polo AS LOGICAL     NO-UNDO.




/********************* POLI ***********************/
/********************* POLI ***********************/
def var i-vl-preori   like it-nota-fisc.vl-preori.
def var i-vl-tot-nota like it-nota-fisc.vl-merc-liq.
def var i-vl-merc-liq like it-nota-fisc.vl-merc-liq.
def var c-end01 as char form "x(40)" no-undo.
def var c-end02 as char form "x(40)" no-undo.
def var c-end03 as char form "x(40)" no-undo.
def var l-natureza     as logical init no no-undo.
def var c-natur-codigo like natur-oper.nat-operacao.
def var i-item         as inte                       no-undo init 0.
def var c-natur-descri like natur-oper.denominacao.
DEF VAR c-data-docum   as char form "x(10)" extent 2 no-undo.
def var c-nota-docum   as char form "x(07)" extent 2 no-undo.
def var c-quan-docum   as char form "x(14)" extent 2 no-undo.
def var c-valo-docum   as char form "x(14)" extent 2 no-undo.
def var c-des-operacao    like natur-oper.denominacao.
def var c-nat-operacao    as char format "x(10)".
DEF VAR c-ender          like nota-fiscal.endereco.
def var c-estado         like nota-fiscal.estado.
def var c-cidade         like nota-fiscal.cidade.
def var c-bairro         like nota-fiscal.bairro.
def var i-cep            like nota-fiscal.cep.
def buffer b-natureza      for natur-oper.


/********************* POLI ***********************/
/*cstomizacao para descricao de nota de remessa*/
DEFINE BUFFER b-nota-desc  FOR nota-fiscal.
DEFINE BUFFER b-natur-desc FOR natur-oper.
DEFINE VARIABLE i-pos AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-pos-i AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-pos-f AS INTEGER     NO-UNDO.
DEFINE BUFFER b-nota-fiscal FOR nota-fiscal.
DEFINE VARIABLE c-nt AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-serie AS CHARACTER   NO-UNDO.
/**************/

/* customizado */
def var de-vl-des-zf  as dec no-undo.
DEFINE VARIABLE i-idi-forma-emis-nf-eletro  AS INTEGER     NO-UNDO.
DEFINE VARIABLE l-imprime-lote AS LOGICAL INITIAL NO    NO-UNDO.
DEFINE VARIABLE l-lotes-desc  AS LOGICAL     NO-UNDO.
DEFINE VARIABLE c-mens-391-amonia AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-mens-GE-31 AS CHARACTER   NO-UNDO.

DEF TEMP-TABLE tt-fat-ser-lote LIKE fat-ser-lote.
DEFINE VARIABLE c-mens-devol AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-desc-lote AS CHARACTER  NO-UNDO.
DEFINE VARIABLE d-fator AS DECIMAL    NO-UNDO.
DEFINE VARIABLE c-mens-redu AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-desc-lotes AS CHARACTER  NO-UNDO.

EMPTY TEMP-TABLE tt-fat-ser-lote.
find nota-fiscal where rowid(nota-fiscal) = r-nota no-lock.

find estabelec of nota-fiscal no-lock.




/*customizacao para permitir layout por empresa */
find natur-oper where natur-oper.nat-operacao = nota-fiscal.nat-operacao no-lock no-error.

ASSIGN c-empresa-layout = STRING(estabelec.ep-codigo).
/*custumizacao para pegar numero do pedido do cliente*/
ASSIGN c-pedido = ""
       c-mens-devol = ""
       c-mens-391-amonia = ""
       c-mens-GE-31 = "".
/*customizacao para notas de decolucao*/
 IF nota-fiscal.esp-docto EQ 20 THEN DO:
     FIND FIRST it-nota-fisc OF nota-fiscal WHERE INT(it-nota-fisc.nr-docum) NE 0 NO-LOCK NO-ERROR NO-WAIT. 

     FIND docum-est WHERE docum-est.cod-estabel  EQ it-nota-fisc.cod-estabel 
                      AND docum-est.nro-docto    EQ it-nota-fisc.nr-docum
                      AND docum-est.serie        EQ it-nota-fisc.serie-docum
                      AND docum-est.cod-emitente EQ it-nota-fisc.cd-emitente
                      AND docum-est.nat-operacao EQ it-nota-fisc.nat-docum
                      NO-LOCK NO-ERROR NO-WAIT.
     IF AVAIL docum-est THEN ASSIGN c-mens-devol = " - DEVOLUCAO DE MATERIAL RECEBIDO ATRAVES DE SUA NF. " + docum-est.nro-docto + " DE " + STRING(docum-est.dt-emissao,"99/99/9999") + ". ".
  END.

  ASSIGN c-estab-uc  = ""
         i-emp-uc    = ""
         l-item-polo = NO.

  for each it-nota-fisc of nota-fiscal NO-LOCK,
      each item
      where item.it-codigo = it-nota-fisc.it-codigo no-lock:

     

      /*customizacao quando for unigel comercial*/

      IF item.ge-codigo >= 41 AND ITEM.ge-codigo < 50  THEN l-item-polo = YES.

      IF c-estab-uc = "" THEN  DO:
            FOR FIRST ped-venda WHERE ped-venda.nr-pedido = it-nota-fisc.nr-pedido  NO-LOCK,
                FIRST if-ped-venda WHERE if-ped-venda.nr-pedido-relac = ped-venda.nr-pedido NO-LOCK,
                FIRST b-ped-venda WHERE  b-ped-venda.nr-pedido = if-ped-venda.nr-pedido NO-LOCK,
                FIRST buc-estabelec WHERE buc-estabelec.cod-estabel = b-ped-venda.cod-estabel NO-LOCK.


                ASSIGN c-estab-uc = buc-estabelec.cod-estabel
                       i-emp-uc   = buc-estabelec.ep-codigo.

             END.
      END.
      
      IF c-estab-uc = "" AND not(ITEM.ge-codigo >= 41 AND ITEM.ge-codigo <= 49) THEN do:
         if index("435,433,432,443,436",estabelec.cod-estabel) > 0 then  /*solic-318*/  
         ASSIGN   c-estab-uc = "381"
                  i-emp-uc   = "380".


      end.

      IF c-estab-uc = "" THEN
        ASSIGN   c-estab-uc = nota-fiscal.cod-estabel
                 i-emp-uc   = estabelec.ep-codigo.


 

       /*customizacao AMGRA- 18-01-2010*/
                IF item.fm-codigo = "31-10" AND c-estab-uc = "391" THEN
                            c-mens-391-amonia = " DATA DE FABRICACAO:" + STRING(nota-fiscal.dt-emis-nota,"99/99/9999").



        
                IF item.GE-codigo = 31 AND c-estab-uc = "391" THEN
                            c-mens-GE-31 = " (EP BA-08451-4)".
                IF item.GE-codigo = 31 AND c-estab-uc = "394" THEN
                            c-mens-GE-31 = " (EP MG-90589-5)".
                IF item.GE-codigo = 31 AND c-estab-uc = "395" THEN
                            c-mens-GE-31 = " (EC BA-32148-6)".
                   

      /*custumizacao para pegar numero do pedido do cliente*/
      IF INDEX (c-pedido,it-nota-fisc.nr-pedcli) = 0 THEN 
      IF LENGTH(c-pedido) = 0 THEN
          c-pedido = "Pedido(s) Cliente: "+ it-nota-fisc.nr-pedcli.
      ELSE
          c-pedido = c-pedido + "/" + it-nota-fisc.nr-pedcli.
  /***********************************************************/
  END.
/*customizacao quando n∆o for unigel comercial*/
 IF c-estab-uc = "" THEN
    ASSIGN   c-estab-uc = nota-fiscal.cod-estabel
             i-emp-uc   = estabelec.ep-codigo.

{ftp/ft0518f.i5} /* ttDanfe, ttDanfeItem */

{adapters/xml/ep2/axsep017.i} /*Temp-Tables da NF-e, ttNFe, ttIde, ttDet, etc.*/

/*Temp-Table com todos os campos que s∆o impressos no DANFE, referente ao ICMS*/
DEFINE TEMP-TABLE ttICMSDanfe NO-UNDO  
    FIELD orig           AS CHARACTER INITIAL ?                                         /*origem da mercadoria: 0 - Nacional 1 - Estrangeira - Importaá∆o direta 2 - Estrangeira - Adquirida no mercado interno */
    FIELD CST            AS CHARACTER INITIAL ?                                         /*Tributá∆o pelo ICMS 00 - Tributada integralmente*/
    FIELD vBC            AS DECIMAL   INITIAL ?  FORMAT ">>>>>>>>>>>>9.99" DECIMALS 2   /*Valor da BC do ICMS*/ 
    FIELD vICMS          AS DECIMAL   INITIAL ?  FORMAT ">>>>>>>>>>>>9.99" DECIMALS 2   /*Valor do ICMS*/       
    FIELD pICMS          AS DECIMAL   INITIAL ?  FORMAT ">>9.99"           DECIMALS 2   /*Al°quota do ICMS*/    
    FIELD vBCST          AS DECIMAL   INITIAL ?  FORMAT ">>>>>>>>>>>>9.99" DECIMALS 2   /*Valor da BC do ICMS ST*/ 
    FIELD vICMSST        AS DECIMAL   INITIAL ?  FORMAT ">>>>>>>>>>>>9.99" DECIMALS 2   /*Valor do ICMS ST*/ 
    /*Chave EMS*/
    FIELD CodEstabelNF   AS CHARACTER INITIAL ?
    FIELD SerieNF        AS CHARACTER INITIAL ?
    FIELD NrNotaFisNF    AS CHARACTER INITIAL ?
    FIELD ItCodigoNF     AS CHARACTER INITIAL ?
    FIELD NrSeqFatNF     AS INTEGER   INITIAL ?
    INDEX ch-ttICMSDanfe CodEstabelNF SerieNF NrNotaFisNF NrSeqFatNF ItCodigoNF.


RUN adapters/xml/ep2/axsep017.p PERSISTENT SET h-axsep017.
RUN pi-seta-nota-fiscal    IN h-axsep017 (INPUT r-nota).
RUN pi-prepara-dados       IN h-axsep017.
RUN pi-devolve-temp-tables IN h-axsep017 (OUTPUT  TABLE ttAdi       ,
                                          OUTPUT  TABLE ttArma      ,
                                          OUTPUT  TABLE ttAvulsa    ,
                                          OUTPUT  TABLE ttCobr      ,
                                          OUTPUT  TABLE ttCOFINSAliq,
                                          OUTPUT  TABLE ttCOFINSNT  ,
                                          OUTPUT  TABLE ttCOFINSOutr,
                                          OUTPUT  TABLE ttCOFINSQtde,
                                          OUTPUT  TABLE ttCOFINSST  ,
                                          OUTPUT  TABLE ttComb      ,
                                          OUTPUT  TABLE ttCompra    ,
                                          OUTPUT  TABLE ttDest      ,
                                          OUTPUT  TABLE ttDet       ,
                                          OUTPUT  TABLE ttDI        ,
                                          OUTPUT  TABLE ttDup       ,
                                          OUTPUT  TABLE ttEmit      ,
                                          OUTPUT  TABLE ttEntrega   ,
                                          OUTPUT  TABLE ttExporta   ,
                                          OUTPUT  TABLE ttICMS00    ,
                                          OUTPUT  TABLE ttICMS10    ,
                                          OUTPUT  TABLE ttICMS20    ,
                                          OUTPUT  TABLE ttICMS30    ,
                                          OUTPUT  TABLE ttICMS40    ,
                                          OUTPUT  TABLE ttICMS51    ,
                                          OUTPUT  TABLE ttICMS60    ,
                                          OUTPUT  TABLE ttICMS70    ,
                                          OUTPUT  TABLE ttICMS90    ,
                                          OUTPUT  TABLE ttICMSTot   ,
                                          OUTPUT  TABLE ttIde       ,
                                          OUTPUT  TABLE ttII        ,
                                          OUTPUT  TABLE ttInfAdic   ,
                                          OUTPUT  TABLE ttIPI       ,
                                          OUTPUT  TABLE ttISSQN     ,
                                          OUTPUT  TABLE ttISSQNtot  ,
                                          OUTPUT  TABLE ttLacres    ,
                                          OUTPUT  TABLE ttMed       ,
                                          OUTPUT  TABLE ttNFe       ,
                                          OUTPUT  TABLE ttrefNF     ,
                                          OUTPUT  TABLE ttObsCont   ,
                                          OUTPUT  TABLE ttObsFisco  ,
                                          OUTPUT  TABLE ttPISAliq   ,
                                          OUTPUT  TABLE ttPISNT     ,
                                          OUTPUT  TABLE ttPISOutr   ,
                                          OUTPUT  TABLE ttPISQtde   ,
                                          OUTPUT  TABLE ttPISST     ,
                                          OUTPUT  TABLE ttProcRef   ,
                                          OUTPUT  TABLE ttReboque   ,
                                          OUTPUT  TABLE ttRetirada  ,
                                          OUTPUT  TABLE ttRetTrib   ,
                                          OUTPUT  TABLE ttTransp    ,
                                          OUTPUT  TABLE ttVeic      ,
                                          OUTPUT  TABLE ttVol       ,
                                          OUTPUT  TABLE ttrefNFP    ,
                                          OUTPUT  TABLE ttrefCTe    ,
                                          OUTPUT  TABLE ttrefECF    ,
                                          OUTPUT  TABLE ttICMSPart  ,
                                          OUTPUT  TABLE ttICMSST    ,
                                          OUTPUT  TABLE ttICMSSN101 ,
                                          OUTPUT  TABLE ttICMSSN102 ,
                                          OUTPUT  TABLE ttICMSSN201 ,
                                          OUTPUT  TABLE ttICMSSN202 ,
                                          OUTPUT  TABLE ttICMSSN500 ,
                                          OUTPUT  TABLE ttICMSSN900 ,
                                          OUTPUT  TABLE ttCana      ,
                                          OUTPUT  TABLE ttForDia    ,
                                          OUTPUT  TABLE ttDeduc     ).

IF  VALID-HANDLE(h-axsep017) THEN DO:
    DELETE PROCEDURE h-axsep017.
    ASSIGN h-axsep017 = ?.
END.

EMPTY TEMP-TABLE ttDanfe.
EMPTY TEMP-TABLE ttDanfeItem.

FIND FIRST nota-fiscal   NO-LOCK WHERE ROWID(nota-fiscal) = r-nota NO-ERROR.
FIND FIRST param-global  NO-LOCK                                   NO-ERROR.

FIND FIRST ttNFe         NO-LOCK NO-ERROR.
FIND FIRST ttIde         NO-LOCK NO-ERROR.
FIND FIRST ttEmit        NO-LOCK NO-ERROR.
FIND FIRST ttDest        NO-LOCK NO-ERROR.
FIND FIRST ttICMSTot     NO-LOCK NO-ERROR.
FIND FIRST ttTransp      NO-LOCK NO-ERROR.
FIND FIRST ttISSQNTot    NO-LOCK NO-ERROR.
FIND FIRST ttInfAdic     NO-LOCK NO-ERROR.

CREATE ttDanfe.

/* Codigo de Barras - Chave de Acesso */
RUN bcp/bcapi016.p PERSISTENT SET h-bcapi016.
RUN generateCODE128C IN h-bcapi016 (TRIM(ttNFe.ChaveAcessoNFe),OUTPUT ttDanfe.BCCODE128-chave).

DELETE PROCEDURE h-bcapi016.
ASSIGN h-bcapi016 = ?.
/* Fim Codigo de Barras */

/* Ambiente (SEFAZ) do envio da Nota */
ASSIGN iTpAmbSEFAZ = INT(&if '{&bf_dis_versao_ems}' >= '2.09':U
                         &then nota-fiscal.idi-tip-emis-amb-sefaz
                         &else SUBSTRING(nota-fiscal.char-2,200,1) &endif) NO-ERROR.

IF  NOT (iTpAmbSEFAZ > 0) THEN
    ASSIGN iTpAmbSEFAZ = INT(ttIde.tpAmb).
/* Fim - Ambiente (SEFAZ) do envio da Nota */

/* Modelo de DANFE: 1=Retrato 2=Paisagem */
FIND FIRST ser-estab NO-LOCK
     WHERE ser-estab.cod-estabel = nota-fiscal.cod-estabel
       AND ser-estab.serie       = nota-fiscal.serie NO-ERROR.

ASSIGN c-modelo-DANFE = (&if  "{&bf_dis_versao_ems}" < "2.07":U 
                         &then SUBSTRING(ser-estab.char-1,4,1) 
                         &else STRING(ser-estab.idi-format-emis-danfe)  &endif) WHEN AVAIL ser-estab.
/* Fim - MODELO DO DANFE*/

/* Utiliza ou N∆o Word da impress∆o do DANFE */
ASSIGN l-sem-Word     = (&if "{&bf_dis_versao_ems}":U >= "2.08":U
                       &then ser-estab.log-word-danfe
                       &else substring(ser-estab.char-1,70,1) = "S":U &endif) WHEN AVAIL ser-estab.
/* Fim - Utiliza ou N∆o Word da impress∆o do DANFE */


ASSIGN ttDanfe.chavedeacessonfe          = STRING(ttNFe.ChaveAcessoNFe,"9999 9999 9999 9999 9999 9999 9999 9999 9999 9999 9999")
       ttDanfe.sn                        = ttIde.tpNF
       ttDanfe.razaosocialempresa        = ttEmit.xNome
       ttDanfe.enderecoemp               = (ttEmit.xLgr + " " + ttEmit.nro + " " + ttEmit.xCpl)
       ttDanfe.bairroemp                 = ttEmit.xBairro
       ttDanfe.cidadeemp                 = ttEmit.xMun
       ttDanfe.ufemp                     = ttEmit.UF
       ttDanfe.cepemp                    = STRING( (IF   ttEmit.CEP <> ""
                                                    THEN FILL("0", 8 - LENGTH(STRING(ttEmit.CEP))) + STRING(ttEmit.CEP)
                                                    ELSE STRING(ttEmit.CEP))  ,"99999-999")
       ttDanfe.foneemp                   = ttEmit.fone
       /*ttDanfe.siteemp                   =*/
       ttDanfe.nrnota                    = ttNFe.NrNotaFisNF
       ttDanfe.ser                       = ttNFe.SerieNF
       /*ttDanfe.n1                        =*/
       /*ttDanfe.nnn                       =*/
       ttDanfe.naturezaoperacao          = ttIde.natOp
       ttDanfe.inscrestadempresa         = ttEmit.IE
       ttDanfe.inscrestadsubstituto      = ttEmit.IEST
       ttDanfe.cnpjempresa               = (IF   param-global.formato-id-federal <> ""
                                            THEN STRING(ttEmit.CNPJ, param-global.formato-id-federal)
                                            ELSE ttEmit.CNPJ)
       ttDanfe.cnpjdestinatario          = (IF   ttDest.i-natureza = 1 /*Pessoa Fisica*/
                                            THEN IF   param-global.formato-id-pessoal <> ""
                                                 THEN STRING(ttDest.CPF,  param-global.formato-id-pessoal)
                                                 ELSE ttDest.CPF
                                            ELSE IF   param-global.formato-id-federal <> ""
                                                 THEN STRING(ttDest.CNPJ, param-global.formato-id-federal)
                                                 ELSE ttDest.CNPJ)
       ttDanfe.razaosocialdestinatario   = ttDest.xNome
       ttDanfe.dataemissao               = STRING(ttIde.dEmi,"99/99/9999")
       ttDanfe.dataentrega               = STRING(nota-fiscal.dt-saida,"99/99/9999")
       ttDanfe.horasaida                 = c-hr-saida WHEN (c-hr-saida <> "00:00:00" AND l-dt)
       ttDanfe.enderecodestinatario      = (ttDest.xLgr + " " + ttDest.nro + " " + ttDest.xCpl)
       ttDanfe.cidadedestinatario        = ttDest.xMun
       ttDanfe.bairrodestinatario        = ttDest.xBairro
       ttDanfe.cepdestinatario           = STRING( (IF   ttDest.CEP <> ""
                                                    THEN FILL("0", 8 - LENGTH(STRING(ttDest.CEP))) + STRING(ttDest.CEP)
                                                    ELSE STRING(ttDest.CEP))  ,"99999-999")
       ttDanfe.fonedestinatario          = ttDest.fone
       ttDanfe.ufdest                    = ttDest.UF
       ttDanfe.inscrestaddestinatario    = ttDest.IE
       
       ttDanfe.vlbcicmsnota              = TRIM(STRING(ttICMSTot.vBC   ,"->>>,>>>,>>>,>>9.99"))
       ttDanfe.vlicmsnota                = TRIM(STRING(ttICMSTot.vICMS ,"->>>,>>>,>>>,>>9.99"))
       ttDanfe.vlbcicmsstnota            = TRIM(STRING(ttICMSTot.vBCST ,"->>>,>>>,>>>,>>9.99"))
       ttDanfe.vlicmsstnota              = TRIM(STRING(ttICMSTot.vST   ,"->>>,>>>,>>>,>>9.99"))
       ttDanfe.vltotprod                 = TRIM(STRING(ttICMSTot.vProd ,"->>>,>>>,>>>,>>9.99"))
       ttDanfe.vlfretenota               = TRIM(STRING(ttICMSTot.vFrete,"->>>,>>>,>>>,>>9.99"))
       ttDanfe.vlseguronota              = TRIM(STRING(ttICMSTot.vSeg  ,"->>>,>>>,>>>,>>9.99"))
       ttDanfe.vldescontonota            = TRIM(STRING(ttICMSTot.vDesc ,"->>>,>>>,>>>,>>9.99"))
       ttDanfe.vldespesasnota            = TRIM(STRING(ttICMSTot.vOutro,"->>>,>>>,>>>,>>9.99"))
       ttDanfe.vlipinota                 = TRIM(STRING(ttICMSTot.vIPI  ,"->>>,>>>,>>>,>>9.99"))
       ttDanfe.vltotnota                 = TRIM(STRING(ttICMSTot.vNF   ,"->>>,>>>,>>>,>>9.99"))

       ttDanfe.nometransp                = ttTransp.xNome
       /*ttDanfe.codantt1                  =*/
       /*ttDanfe.codantt2                  =*/
       ttDanfe.placa1                    = ttTransp.placa
       /*ttDanfe.placa2                    =*/
       ttDanfe.ufpl1                     = ttTransp.UFPlaca
       /*ttDanfe.ufpl2                     =*/
       ttDanfe.cnpjtransp                = (IF   ttTransp.i-natureza = 1 /*Pessoa Fisica*/
                                            THEN IF   param-global.formato-id-pessoal <> ""
                                                 THEN STRING(ttTransp.CPF,  param-global.formato-id-pessoal)
                                                 ELSE ttTransp.CPF
                                            ELSE IF   param-global.formato-id-federal <> ""
                                                 THEN STRING(ttTransp.CNPJ, param-global.formato-id-federal)
                                                 ELSE ttTransp.CNPJ)
       ttDanfe.enderecotransp            = ttTransp.xEnder
       ttDanfe.cidadetransp              = ttTransp.xMun
       ttDanfe.uftran                    = ttTransp.UF
       ttDanfe.inscrestadtransp          = ttTransp.IE
       ttDanfe.inscrmunicipaliss         = ttEmit.IM
       ttDanfe.vltotalsevicos            = IF  AVAIL ttISSQNTot 
                                           THEN TRIM(STRING(ttISSQNTot.vServ,"->>>,>>>,>>>,>>9.99")) 
                                           ELSE TRIM(STRING(0               ,"->>>,>>>,>>>,>>9.99")) 
       ttDanfe.vlbciss                   = IF  AVAIL ttISSQNTot 
                                           THEN TRIM(STRING(ttISSQNTot.vBC  ,"->>>,>>>,>>>,>>9.99")) 
                                           ELSE TRIM(STRING(0               ,"->>>,>>>,>>>,>>9.99")) 
       ttDanfe.vlisstotal                = IF  AVAIL ttISSQNTot 
                                           THEN TRIM(STRING(ttISSQNTot.vISS ,"->>>,>>>,>>>,>>9.99")) 
                                           ELSE TRIM(STRING(0               ,"->>>,>>>,>>>,>>9.99")) 
       ttDanfe.informacoescomplementares = ttInfAdic.infCpl
       /*ttDanfe.contingencia              =*/
       ttDanfe.homologacao1              = (IF   iTpAmbSEFAZ = 2 /*Homologacao*/
                                            THEN "SEM VALOR FISCAL":U
                                            ELSE "")
       ttDanfe.homologacao2              = (IF   iTpAmbSEFAZ = 2 /*Homologacao*/
                                            THEN "Nota Fiscal Eletrìnica Autorizada em Ambiente de HOMOLOGAÄ«O.":U
                                            ELSE "").
       
       IF nota-fiscal.cod-estabel = "433" THEN DO:
            FOR FIRST natur-oper WHERE 
                      natur-oper.nat-operacao = nota-fiscal.nat-operacao NO-LOCK.       
               FOR FIRST nota-fisc-adc WHERE
                    nota-fisc-adc.cod-estab          = nota-fiscal.cod-estabel  and
                    nota-fisc-adc.cod-serie          = nota-fiscal.serie        and
                    nota-fisc-adc.cod-nota-fisc      = nota-fiscal.nr-nota-fis  and
                    nota-fisc-adc.cdn-emitente       = nota-fiscal.cod-emitente and
                    nota-fisc-adc.cod-natur-operac   = natur-oper.cod-cfop      and
                    nota-fisc-adc.idi-tip-dado       = 20                       and
                    nota-fisc-adc.num-seq            = 1 NO-LOCK.

                   ASSIGN
                        ttDanfe.dataentrega = substring(nota-fisc-adc.cod-livre-3,9,2) + "/" + substring(nota-fisc-adc.cod-livre-3,6,2) + "/" + substring(nota-fisc-adc.cod-livre-3,1,4)
                        ttDanfe.horasaida   = STRING(REPLACE(nota-fisc-adc.hra-saida,":",""),"99:99:99").         
               END.
            END.
       END.
 

/*=====================================================================================================================================================
  LOCAL DE ENTREGA (quando for diferente do endereáo padr∆o do destinat†rio da nota fiscal) 
=====================================================================================================================================================*/

FOR FIRST ttEntrega NO-LOCK: /*S¢ haver† registro na ttEntrega na condiá∆o de o local de entrega ser diferente do endereáo do destinatario/cliente da nota*/

    ASSIGN ttDanfe.informacoescomplementares = ttDanfe.informacoescomplementares +
                                               "  LOCAL DE ENTREGA: "    + (ttEntrega.xLgr + " " + ttEntrega.nro + " " + ttEntrega.xCpl) +
                                               " Bairro/Distrito: "      + ttEntrega.xBairro +
                                               " Municipio: "            + ttEntrega.xMun    + 
                                               " UF: "                   + ttEntrega.UF      +
                                               " Pais: "                 + nota-fiscal.pais.
END.
/*===================================================================================================================================================*/



/*=====================================================================================================================================================
  MODALIDADE DE FRETE (No XML da NF-e envia-se apenas os C¢digos. No DANFE, imprime-se Codigo + Descricao) 
  
  0 ? Emitente
  1 ? Destinat†rio/Remetente
  2 ? Terceiros
  9 ? Sem Frete
=====================================================================================================================================================*/

CASE ttTransp.modfrete:
    WHEN "0":U THEN
        ASSIGN c-desc-mod-frete = "0 - Emitente":U.
    WHEN "1":U THEN
        ASSIGN c-desc-mod-frete = "1 - Destinat†rio/Remetente":U.
    WHEN "2":U THEN
        ASSIGN c-desc-mod-frete = "2 - Terceiros":U.
    WHEN "9":U THEN
        ASSIGN c-desc-mod-frete = "9 - Sem Frete":U.
END CASE.


ASSIGN ttDanfe.idfr = c-desc-mod-frete.

/*===================================================================================================================================================*/



/*=====================================================================================================================================================
  VOLUMES E EMBALAGENS
=====================================================================================================================================================*/
ASSIGN ttDanfe.especievolume = "".

FOR EACH ttVol NO-LOCK
    BREAK BY ttVol.siglaEmb:

    ACCUMULATE INT(ttVol.qVol) (TOTAL).
    ACCUMULATE     ttVol.pesoB (TOTAL).
    ACCUMULATE     ttVol.pesoL (TOTAL).

    IF  LAST (ttVol.siglaEmb) THEN
        ASSIGN ttDanfe.qtvolume              = TRIM(STRING(ACCUM TOTAL INT(ttVol.qVol),"->>>,>>>,>>>,>>9.99"))
               ttDanfe.marcavolume           = ttVol.marca
               ttDanfe.numeracaovolume       = ttVol.nVol
               ttDanfe.pesobrutototal        = TRIM(STRING(ACCUM TOTAL ttVol.pesoB,"->>>,>>>,>>>,>>9.999"))
               ttDanfe.pesoliquidototal      = TRIM(STRING(ACCUM TOTAL ttVol.pesoL,"->>>,>>>,>>>,>>9.999")).

    IF  ttVol.esp <> ?  AND 
        ttVol.esp <> "" THEN DO:
        IF  ttDanfe.especievolume <> "" THEN
            ttDanfe.especievolume = ttDanfe.especievolume + "/".

        ASSIGN ttDanfe.especievolume = ttDanfe.especievolume + ttVol.esp.
    END.
        
    
END.
/*===================================================================================================================================================*/



/*=====================================================================================================================================================
  PROTOCOLO DE AUTORIZACAO + Data e Hora
=====================================================================================================================================================*/

ASSIGN ttDanfe.protocoloautorizacao = &if "{&bf_dis_versao_ems}":U >= "2.07":U 
                                      &then RIGHT-TRIM(nota-fiscal.cod-protoc)
                                      &else RIGHT-TRIM(SUBSTRING(nota-fiscal.char-1,97,15))
                                      &endif.

FOR EACH ret-nf-eletro NO-LOCK
   WHERE ret-nf-eletro.cod-estabel = nota-fiscal.cod-estabel
     AND ret-nf-eletro.cod-serie   = nota-fiscal.serie      
     AND ret-nf-eletro.nr-nota-fis = nota-fiscal.nr-nota-fis
      BY ret-nf-eletro.dat-ret DESC
      BY ret-nf-eletro.hra-ret DESC:

    IF &if "{&bf_dis_versao_ems}" >= "2.07":U &then
         ret-nf-eletro.cod-protoc
       &else
         ret-nf-eletro.cod-livre-1
       &endif
    = TRIM(ttDanfe.protocoloautorizacao) THEN DO:

        ASSIGN ttDanfe.protocoloautorizacao = ttDanfe.protocoloautorizacao + 
                                              "   " + STRING(ret-nf-eletro.dat-ret,"99/99/9999")  + /*DATA*/
                                              "   " + STRING(ret-nf-eletro.hra-ret,"xx:xx:xx").     /*HORA*/
        LEAVE.
    END.
END.
/*===================================================================================================================================================*/




/*=====================================================================================================================================================
  chavedeacessoadicionalnfe = cUF + tpEmis + CNPJ + vNF + ICMSp + ICMSs + DD + DV 
  onde: cUF    = codigo da UF do destinatario do documento fiscal
        tpEmis = forma de emissao da NF-e
        CNPJ   = CNPJ do destinatario
        vNF    = valor total da NF-e(sem ponto decimal, informar sempre os centavos
        ICMSp  = Destaque de ICMS proprio na NF-e e no seguinte formato: 1 - ha destaque de ICMS proprio | 2 - nao ha destaque de ICMS proprio
        ICMSs  = Destaque de ICMS por substituiÁao tributaria na NF-e e no seguinte formato: 1 - ha destaque de ICMS ST | 2 - nao ha destaque de ICMS ST
        DD     = Dia da emissao da NF-e
        DV     = Digito Verificador
  
  IMPRESSAO DE CODIGO DE BARRA ADICIONAL, PARA IMPRESSAO EM CONTINGENCIA PARA FORMULARIO ESPECIAL (FS e FS-DA)      
=====================================================================================================================================================*/

    IF  ttIde.tpEmis = "2" OR ttIde.tpEmis = "5" THEN DO: /* Tipo de Emissao -> 2 - Contingencia FS / 5 - Contingencia FS-DA */

        /*Busca Codigo da UF do Destinatario da Nota*/
        IF  ttDest.xPais = "Brasil":U THEN
            FOR FIRST unid-feder NO-LOCK
                WHERE unid-feder.pais   = ttDest.xPais
                  AND unid-feder.estado = ttDest.UF:

                ASSIGN c-cod-uf-ibge = (&if  "{&bf_dis_versao_ems}"  >=  "2.07":U
                                        &then STRING(unid-feder.cod-uf-ibge)
                                        &else STRING(subSTRING(unid-feder.char-1,1,2))
                                        &endif).
            END. /* for first unid-feder no-lock */
        ELSE 
            ASSIGN c-cod-uf-ibge = "99".
        
        ASSIGN c-chave-acesso-adicional-nfe = /* cUF      */  STRING(INT(c-cod-uf-ibge), '99') + 
                                              /* tpEmis   */  ttIde.tpEmis +
                                              /* CNPJ/CPF */  STRING(DEC(TRIM(REPLACE(REPLACE(REPLACE(ttDanfe.cnpjdestinatario,".",""),"-",""),"/",""))), '99999999999999') +
                                              /* vNF      */  STRING(ttICMSTot.vNF * 100, '99999999999999') + 
                                              /* ICMSp    */  (IF  ttICMSTot.vICMS > 0 
                                                               THEN "1"
                                                               ELSE "2") +
                                              /* ICMSs    */  (IF  ttICMSTot.vST > 0
                                                               THEN "1"
                                                               ELSE "2") +  
                                              /* DD       */  STRING(DAY(ttIde.dEmi),"99").

        ASSIGN i-mult-nfe = 2.

        DO  i-count-nfe = LENGTH(c-chave-acesso-adicional-nfe) TO 1 BY -1:
            ASSIGN i-soma-mod-nfe = i-soma-mod-nfe + (INT(SUBSTRING(c-chave-acesso-adicional-nfe,i-count-nfe,1)) * i-mult-nfe).
        
            ASSIGN i-mult-nfe = i-mult-nfe + 1.
            IF i-mult-nfe = 10 THEN ASSIGN i-mult-nfe = 2.
        END.
        
        IF i-soma-mod-nfe MODULO 11 = 0 OR
           i-soma-mod-nfe MODULO 11 = 1 THEN ASSIGN i-dig-ver-nfe = 0.
                                        ELSE ASSIGN i-dig-ver-nfe = 11 - (i-soma-mod-nfe MODULO 11).
            
        ASSIGN c-chave-acesso-adicional-nfe = c-chave-acesso-adicional-nfe + STRING(i-dig-ver-nfe). /* DV */

        RUN bcp/bcapi016.p PERSISTENT SET h-bcapi016.
        RUN generateCODE128C IN h-bcapi016 (INPUT TRIM(c-chave-acesso-adicional-nfe),OUTPUT ttDanfe.BCCODE128-chaveadicional).

        ASSIGN ttDanfe.chavedeacessoadicionalnfe = STRING(c-chave-acesso-adicional-nfe, "9999 9999 9999 9999 9999 9999 9999 9999 9999").

        DELETE PROCEDURE h-bcapi016.
        ASSIGN h-bcapi016 = ?.
        
    END.
    ELSE DO:
        ASSIGN c-chave-acesso-adicional-nfe      = ""
               ttDanfe.chavedeacessoadicionalnfe = "".
    END.
/*===================================================================================================================================================*/



/*=====================================================================================================================================================
  DUPLICATAS
=====================================================================================================================================================*/

ASSIGN i-cont = 1.

FOR EACH ttDup NO-LOCK:

    IF  i-cont = 8 THEN LEAVE. /*Danfe Padr∆o somente com 8 duplicatas. Se houverem mais, sair e nao imprimir. No XML ir∆o todas as fat-duplic existentes*/

    IF  i-cont = 1 THEN
        ASSIGN ttDanfe.fatura1  = ttDup.nDup
               ttDanfe.vencfat1 = STRING(ttDup.dVenc,"99/99/9999")
               ttDanfe.vlfat1   = TRIM(STRING(ttDup.vDup,"->>>>,>>>,>>>,>>9.99")).
    IF  i-cont = 2 THEN
        ASSIGN ttDanfe.fatura2  = ttDup.nDup                                                 
               ttDanfe.vencfat2 = STRING(ttDup.dVenc,"99/99/9999")                           
               ttDanfe.vlfat2   = TRIM(STRING(ttDup.vDup,"->>>>,>>>,>>>,>>9.99")).

    IF  i-cont = 3 THEN
        ASSIGN ttDanfe.fatura3  = ttDup.nDup                                                 
               ttDanfe.vencfat3 = STRING(ttDup.dVenc,"99/99/9999")                           
               ttDanfe.vlfat3   = TRIM(STRING(ttDup.vDup,"->>>>,>>>,>>>,>>9.99")).

    IF  i-cont = 4 THEN
        ASSIGN ttDanfe.fatura4  = ttDup.nDup                                                 
               ttDanfe.vencfat4 = STRING(ttDup.dVenc,"99/99/9999")                           
               ttDanfe.vlfat4   = TRIM(STRING(ttDup.vDup,"->>>>,>>>,>>>,>>9.99")).

    IF  i-cont = 5 THEN
        ASSIGN ttDanfe.fatura5  = ttDup.nDup                                                 
               ttDanfe.vencfat5 = STRING(ttDup.dVenc,"99/99/9999")                           
               ttDanfe.vlfat5   = TRIM(STRING(ttDup.vDup,"->>>>,>>>,>>>,>>9.99")).

    IF  i-cont = 6 THEN
        ASSIGN ttDanfe.fatura6  = ttDup.nDup                                                 
               ttDanfe.vencfat6 = STRING(ttDup.dVenc,"99/99/9999")                           
               ttDanfe.vlfat6   = TRIM(STRING(ttDup.vDup,"->>>>,>>>,>>>,>>9.99")).

    IF  i-cont = 7 THEN
        ASSIGN ttDanfe.fatura7  = ttDup.nDup                                                 
               ttDanfe.vencfat7 = STRING(ttDup.dVenc,"99/99/9999")                           
               ttDanfe.vlfat7   = TRIM(STRING(ttDup.vDup,"->>>>,>>>,>>>,>>9.99")).

    IF  i-cont = 8 THEN
        ASSIGN ttDanfe.fatura8  = ttDup.nDup                                                 
               ttDanfe.vencfat8 = STRING(ttDup.dVenc,"99/99/9999")                           
               ttDanfe.vlfat8   = TRIM(STRING(ttDup.vDup,"->>>>,>>>,>>>,>>9.99")).

    ASSIGN i-cont = i-cont + 1.
END.
/*===================================================================================================================================================*/



/*=====================================================================================================================================================
  INFORMAÄÂES ESPECIAIS PARA EMISS«O EM CONTING“NCIA (VARIAVEIS DOS ARQUIVOS .RTF -> conteudovariavel1 E conteudovariavel2)
=====================================================================================================================================================*/

IF  ttIde.tpEmis = "1" OR ttIde.tpEmis = "3" THEN /* Tipo de Emiss∆o -> 1 - Normal / 3 - Contingància SCAN */
    
    ASSIGN ttDanfe.conteudovariavel1 = "www.nfe.fazenda.gov.br/portal ou no site da Sefaz Autorizadora":U
           ttDanfe.conteudovariavel2 = "PROTOCOLO DE AUTORIZAÄ«O DE USO":U.

ELSE IF ttIde.tpEmis = "4" THEN /* Tipo de Emiss∆o -> 4 - Contingància DPEC */
        
    ASSIGN ttDanfe.conteudovariavel1 = "www.nfe.fazenda.gov.br/portal":U
           ttDanfe.conteudovariavel2 = "NÈMERO DE REGISTRO DPEC":U.
/*===================================================================================================================================================*/

   /*customizacao se n∆o teve nenhum pedido do cliente n∆o imprime a mensagem */
  
  IF c-pedido = "Pedido(s) Cliente: " THEN c-pedido = "".

  ASSIGN ttDanfe.informacoescomplementares = ttDanfe.informacoescomplementares + c-mens-redu + ". " + c-pedido + c-mens-devol  + c-mens-391-amonia + c-mens-GE-31 .

  RUN pi-mensagem-remessa.


/***********************************/


/*=====================================================================================================================================================
  ITENS DA NOTA FISCAL
=====================================================================================================================================================*/
   /* customizacao para varias empresas*/
 ASSIGN l-imprime-lote = NO
        l-lotes-desc = NO.

 EMPTY TEMP-TABLE tt-fat-ser-lote.

    IF (    /*(nota-fiscal.cod-estabel > "380" AND nota-fiscal.cod-estabel < "390")  OR  MAXTANIA 12-01-12 */
     c-estab-uc = "353" OR c-estab-uc = "354" OR c-estab-uc = "343" )  /* 343 nova empresa Trinseo 05-09-2016*/
          THEN
        l-imprime-lote = YES.
 
    IF l-imprime-lote THEN DO:

           FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK,
                FIRST ITEM FIELDS(ge-codigo)
     where item.it-codigo = it-nota-fisc.it-codigo no-lock,
               EACH fat-ser-lote of it-nota-fisc NO-LOCK.
 
               IF   ITEM.ge-codigo = 17 THEN DO:
                    l-imprime-lote = NO.
                    l-lotes-desc = YES.
                    EMPTY TEMP-TABLE tt-fat-ser-lote.
        
                      LEAVE.

               END.


               FIND FIRST tt-fat-ser-lote WHERE
                tt-fat-ser-lote.serie        = fat-ser-lote.serie         AND
                tt-fat-ser-lote.nr-nota-fis  = fat-ser-lote.nr-nota-fis   AND
                tt-fat-ser-lote.nr-seq-fat   = fat-ser-lote.nr-seq-fat    AND
                tt-fat-ser-lote.cod-depos    = fat-ser-lote.cod-depos     AND
                tt-fat-ser-lote.cod-localiz  = ""                         AND
                tt-fat-ser-lote.nr-serlote   = fat-ser-lote.nr-serlote    AND
                tt-fat-ser-lote.it-codigo    = fat-ser-lote.it-codigo     AND
                tt-fat-ser-lote.cod-refer    = fat-ser-lote.cod-refer NO-ERROR.


               IF NOT AVAIL tt-fat-ser-lote THEN DO:
                   CREATE tt-fat-ser-lote.
                   BUFFER-COPY fat-ser-lote EXCEPT cod-localiz TO tt-fat-ser-lote ASSIGN
                       tt-fat-ser-lote.cod-localiz = "".
               END.
               ELSE DO:
                   ASSIGN 
                     tt-fat-ser-lote.qt-baixada[1] =  tt-fat-ser-lote.qt-baixada[1] + fat-ser-lote.qt-baixada[1]
                     tt-fat-ser-lote.qt-baixada[2] =  tt-fat-ser-lote.qt-baixada[2] + fat-ser-lote.qt-baixada[2] .

               END.

           END.



    END.

    IF NOT CAN-FIND( FIRST tt-fat-ser-lote) THEN l-imprime-lote = NO.

IF l-imprime-lote = NO THEN DO:


    ASSIGN i-cont-itens = 1.
          
    ASSIGN sai-laudo-jr = "Nao"
           c-mess-laudo = ""
           c-mess-item = "".
    
    FOR EACH ttDet NO-LOCK,
    
       EACH  ITEM WHERE ITEM.it-codigo = ttDet.ItCodigoNF 
          NO-LOCK .


        /*customizaá∆o faturamento por bobina*/
        ASSIGN unfat = "".

        /*customizaá∆o FCI*/
         c-nro-FCI = "".
         FOR first it-nota-fisc WHERE 
                it-nota-fisc.cod-estabel = ttDet.CodEstabelNF AND
                it-nota-fisc.serie       = ttDet.SerieNF      AND
                it-nota-fisc.nr-nota-fis = ttDet.NrNotaFisNF  AND 
                it-nota-fisc.it-codigo   = ttDet.ItCodigoNF   AND
                it-nota-fisc.nr-seq-fat  = ttDet.NrSeqFatNF   NO-LOCK.

               RUN pi-buscaFCI-ItNotaFisc-Nfe IN h-bodi538 (INPUT ROWID(it-nota-fisc),
                                           OUTPUT c-nro-FCI).


         END.







        IF ITEM.ge-codigo = 46 THEN
            assign sai-laudo-jr = "Sim".

        /*   atencao para quem nao vai ter largura      */    
                 ASSIGN c-desc-prod =  ""
                        l-desc-prod = YES.
                   

               
            /* customizacao POLOFILMS largura e faturamento por bobina cigarreira*/
             FOR first it-nota-fisc WHERE 
                it-nota-fisc.cod-estabel = ttDet.CodEstabelNF AND
                it-nota-fisc.serie       = ttDet.SerieNF      AND
                it-nota-fisc.nr-nota-fis = ttDet.NrNotaFisNF  AND 
                it-nota-fisc.it-codigo   = ttDet.ItCodigoNF   AND
                it-nota-fisc.nr-seq-fat  = ttDet.NrSeqFatNF   NO-LOCK.
                
               
        
                 IF  item.ge-codigo >= 40  AND item.ge-codigo <= 49  THEN DO:
        
                             ASSIGN larg-jr = 0.
                 
                      /*** Pesquisar a largura do item ***/
                       FIND FIRST  ped-item  WHERE
                                   /*ped-item.nome-abrev   = it-nota-fisc.nome-ab-cli  AND nf triangular nao acha largura edson 050105*/
                                   ped-item.nr-pedcli    = it-nota-fisc.nr-pedcli    AND
                                   ped-item.nr-sequencia = it-nota-fisc.nr-seq-ped   AND
                                   ped-item.it-codigo    = it-nota-fisc.it-codigo    AND
                                   ped-item.cod-refer    = it-nota-fisc.cod-refer NO-LOCK NO-ERROR.
                
                
                      /*IF AVAIL ped-item THEN*/
                      FIND var-result USE-INDEX id WHERE 
                           var-result.item-cotacao  = it-nota-fisc.it-codigo   AND
                           var-result.nr-estrut     = (IF AVAIL PED-ITEM THEN ped-item.nr-config ELSE int(it-nota-fisc.cod-refer))       AND
                           var-result.nome-var      = "LARGURA"  NO-LOCK NO-ERROR.
                
                      IF AVAIL VAR-result THEN
                          ASSIGN larg-jr = INT(var-result.des-result).
                             
                      IF larg-jr = 0 or natur-oper.transf THEN DO:
                
                          FIND FIRST fat-ser-lote WHERE
                              fat-ser-lote.cod-estabel = nota-fiscal.cod-estabel AND
                              fat-ser-lote.serie       = it-nota-fisc.serie       AND
                              fat-ser-lote.nr-nota-fis = it-nota-fisc.nr-nota-fis AND
                              fat-ser-lote.nr-seq-fat  = it-nota-fisc.nr-seq-fat  AND
                              fat-ser-lote.it-codigo   = it-nota-fisc.it-codigo
                              NO-LOCK NO-ERROR.
                
                          IF AVAIL fat-ser-lote THEN DO:
                
                              FIND FIRST it-pallet WHERE 
                                  it-pallet.cod-estabel = nota-fiscal.cod-estabel AND
                                  it-pallet.it-codigo   = it-nota-fisc.it-codigo  AND
                                  it-pallet.nr-pallet   = fat-ser-lote.nr-serlote  
                                  NO-LOCK NO-ERROR.
                                  
                    
                
                              IF AVAIL it-pallet THEN DO:
                
                                find first pallet where 
                                    pallet.nr-pallet = it-pallet.nr-pallet and
                                    pallet.it-codigo = it-pallet.it-codigo no-lock no-error.
                                
                                if avail pallet and pallet.nr-pedido <> 0 then do:
                                    run pi-philip-morris.
                                end.
                
                                FIND FIRST lote-carac-tec WHERE 
                                    lote-carac-tec.it-codigo = it-pallet.it-codigo AND
                                    lote-carac-tec.lote      = it-pallet.lote-bobina AND
                                    lote-carac-tec.cd-comp   = "largura"
                                    NO-LOCK NO-ERROR.
                
                                 IF AVAIL lote-carac-tec THEN
                                      ASSIGN larg-jr = INT (lote-carac-tec.vl-result).
                
                              END.
                          END.
                      END. 
                         
                      FIND ped-venda USE-INDEX ch-nr-pedcli NO-LOCK WHERE
                           ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli AND
                           ped-venda.nome-abrev = nota-fiscal.nome-ab-cli NO-ERROR.
                     
                      FIND bvar-result USE-INDEX id WHERE 
                           bvar-result.item-cotacao  = it-nota-fisc.it-codigo   AND
                           bvar-result.nr-estrut     = (IF AVAIL PED-ITEM THEN ped-item.nr-config ELSE 0)         AND
                           bvar-result.nome-var      = "UNFAT"  NO-LOCK NO-ERROR.

                      IF AVAIL bvar-result AND bvar-result.valor-char = "BB" THEN 
                        ASSIGN  unfat =  bvar-result.valor-char.
                      

                                                                                                     
                         IF larg-jr  <> 0 THEN 
                             assign c-desc-prod = " LG:" + trim(STRING(larg-jr,">>>9")) + " ".

                         IF SUBSTRING(it-nota-fisc.nat-operacao,1,1) = "7"  THEN DO:
                             l-desc-prod = NO.

                             c-esp-ext = substring(it-nota-fisc.it-codigo,1,2).

                             if substring(it-nota-fisc.it-codigo,1,4) = "20TB" THEN 
                                 ASSIGN c-esp-ext = "28,6".
                             ELSE
                             if substring(it-nota-fisc.it-codigo,1,4) = "23TB" THEN 
                                 ASSIGN c-esp-ext = "32,8".
                             ELSE
                             if substring(it-nota-fisc.it-codigo,1,4) = "26TB" THEN 
                                 ASSIGN c-esp-ext = "37,1".
                             ELSE
                             if substring(it-nota-fisc.it-codigo,1,4) = "22TB" THEN 
                                 ASSIGN c-esp-ext = "31,4".
                             ELSE
                             if substring(it-nota-fisc.it-codigo,1,4) = "35BM" THEN 
                                 ASSIGN c-esp-ext = "32,8".
                             ELSE
                             if substring(it-nota-fisc.it-codigo,1,4) = "40BM" THEN 
                                  ASSIGN c-esp-ext = "37,1".


                                IF larg-jr > 0  THEN 
                                       ASSIGN 
                                                     c-desc-prod = "FILME DE POLIPROPILENO BIAXIALMENTE ORIENTADO"
                                                     c-desc-prod = c-desc-prod + " LARGURA:" + STRING(larg-jr) + "MM "
                                                     c-desc-prod = c-desc-prod + "ESPESSURA:" + c-esp-ext + " MICRA"
                                                     c-desc-prod = c-desc-prod + ", EM ROLOS ".  
                                   ELSE
                                       ASSIGN 
                                                     c-desc-prod = "FILME DE POLIPROPILENO BIAXIALMENTE ORIENTADO"
                            
                                                     c-desc-prod = c-desc-prod + "ESPESSURA:" + c-esp-ext + " MICRA"
                                                     c-desc-prod = c-desc-prod + ", EM ROLOS". 
                            
                         END.

                
                
                              FIND var-result USE-INDEX id WHERE 
                                  var-result.item-cotacao  = it-nota-fisc.it-codigo   AND
                                  var-result.nr-estrut     = (IF AVAIL PED-ITEM THEN ped-item.nr-config ELSE int(it-nota-fisc.cod-refer))         AND
                                  var-result.nome-var      = "PRODCLIE"  NO-LOCK NO-ERROR.
                        
                             ASSIGN PRODCLI-NF = IF AVAIL var-result THEN TRIM(STRING(var-result.des-result)) ELSE "".
                             FIND var-result USE-INDEX id WHERE 
                                  var-result.item-cotacao  = it-nota-fisc.it-codigo   AND
                                  var-result.nr-estrut     = (IF AVAIL PED-ITEM THEN ped-item.nr-config ELSE int(it-nota-fisc.cod-refer))         AND
                                  var-result.nome-var      = "PEDCLI"  NO-LOCK NO-ERROR.
                             ASSIGN PEDCLI-NF = IF AVAIL var-result THEN TRIM(STRING(var-result.des-result)) ELSE "".
                             FIND first ped-venda USE-INDEX ch-nr-pedcli NO-LOCK WHERE
                                  ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli /*AND ->nf triangular nao sai configurado edson 050105
                                  ped-venda.nome-abrev = nota-fiscal.nome-ab-cli*/ NO-ERROR.
                        
                             IF STRING(nota-fiscal.nr-pedcli) <> "" AND AVAIL ped-item THEN
                             DO:
                        
                             /*customizado*/
                               if l-prim-mess then    
                                  assign c-mess-item = c-mess-item + "     "
                                         l-prim-mess = no.
                             /*customizado*/                    
                        
                               ASSIGN c-mess-item = c-mess-item + STRING(i-cont-item) + "-N/PED:" 
                                                                    + (IF AVAIL ped-venda THEN
                                                                          STRING(ped-venda.nr-pedcli) /* trocado de nr-pedcli para nr-pedido Edson unigel comercial 18-11-2012*/
                                                                       ELSE "")
                                                                    + " S/PED:" + trim(pedcli-nf) + " Prod: "
                                                                    + TRIM(prodcli-nf)  + " - ".
                        
                                 
                             END.
                 
                   /* dados do pedido da nota de faturamento para o corpo da nota de remessa */
                
                     IF STRING(nota-fiscal.nr-pedcli) = "" AND int(it-nota-fisc.cod-refer) <> 0 THEN
                     DO:
                
                         FIND b-it-nota-fisc-ori WHERE 
                                 b-it-nota-fisc-ori.nr-nota-fis     = it-nota-fisc.nr-docum AND
                                 b-it-nota-fisc-ori.it-codigo       = it-nota-fisc.it-codigo AND
                                 b-it-nota-fisc-ori.serie           = it-nota-fisc.serie    AND
                                 b-it-nota-fisc-ori.cod-estabel     = it-nota-fisc.cod-estabel AND
                                 b-it-nota-fisc-ori.nr-seq-fat      = it-nota-fisc.nr-seq-fat NO-LOCK NO-ERROR.           
                         IF AVAIL  b-it-nota-fisc-ori THEN DO:
                             FIND b-nota-fiscal-ori OF b-it-nota-fisc-ori NO-LOCK NO-ERROR.
                
                             FIND FIRST  b-ped-item-ori  WHERE
                                         /* ped-item.nome-abrev   = it-nota-fisc.nome-ab-cli  AND -> nf triangular nao sai largura edson 050105 */
                                          b-ped-item-ori.nr-pedcli    = b-it-nota-fisc-ori.nr-pedcli    AND
                                          b-ped-item-ori.nr-sequencia = b-it-nota-fisc-ori.nr-seq-ped   AND
                                          b-ped-item-ori.it-codigo    = b-it-nota-fisc-ori.it-codigo    AND
                                          b-ped-item-ori.cod-refer    = b-it-nota-fisc-ori.cod-refer NO-LOCK NO-ERROR.
                
                             FIND var-result USE-INDEX id WHERE 
                                          var-result.item-cotacao  = b-it-nota-fisc-ori.it-codigo   AND
                                          var-result.nr-estrut     = (IF AVAIL b-ped-item-ori THEN b-ped-item-ori.nr-config 
                                                                      ELSE int(b-it-nota-fisc-ori.cod-refer))         AND
                                          var-result.nome-var      = "PRODCLIE"  NO-LOCK NO-ERROR.
                
                              ASSIGN PRODCLI-NF = IF AVAIL var-result THEN TRIM(STRING(var-result.des-result)) ELSE "".
                
                              FIND var-result USE-INDEX id WHERE 
                                   var-result.item-cotacao  = b-it-nota-fisc-ori.it-codigo   AND
                                   var-result.nr-estrut     = (IF AVAIL b-PED-ITEM-ori THEN b-ped-item-ori.nr-config 
                                                               ELSE int(b-it-nota-fisc-ori.cod-refer))         AND
                                   var-result.nome-var      = "PEDCLI"  NO-LOCK NO-ERROR.
                
                             ASSIGN PEDCLI-NF = IF AVAIL var-result THEN TRIM(STRING(var-result.des-result)) ELSE "".
                
                             FIND b-ped-venda-ori USE-INDEX ch-nr-pedcli NO-LOCK WHERE
                                  b-ped-venda-ori.nr-pedcli  = b-nota-fiscal-ori.nr-pedcli /*AND ->nf triangular nao sai configurado edson 050105
                                  ped-venda.nome-abrev = nota-fiscal.nome-ab-cli*/ NO-ERROR.
                
                
                
                
                       /*customizado*/
                           if l-prim-mess then    
                              assign c-mess-item = c-mess-item + "     "
                                     l-prim-mess = no.
                       /*customizado*/  
                
                
                            ASSIGN c-mess-item = c-mess-item +  STRING(i-cont-item) + "-N/PED:" 
                                                                  + (IF AVAIL b-ped-venda-ori THEN
                                                                        STRING(b-ped-venda-ori.nr-pedido) /*mas garantido usar nr-pedido do que nr-pedcli Edson 18-11-2012*/
                                                                     ELSE "")
                                                                  + " S/PED:" + trim(pedcli-nf) + " Prod: "
                                                                  + TRIM(prodcli-nf)  + " - ".
                
                         END.
                
                     END. /* fim da rotina da mensagem dos dados para nota de remessa*/
                
        

                          
                 END.



                

        /************************/
        

        
        
        
        
        
             END.

       /*customizacao para imprimir referecia para empresa 380 na descricao*/
                  IF ITEM.codigo-refer <> ""  AND (i-emp-uc = "380" )  THEN
                      ASSIGN c-desc-prod = c-desc-prod + " (" + trim(ITEM.codigo-refer) + ")".
                   
       /*Customizacao para imprimir lotes na descrica para empresa 380*/    
       
               c-desc-lotes = "".           
               if  i-emp-uc = "380" OR estabelec.cod-estabel = "342" OR l-lotes-desc then do:  /*05092016 - Ricardo pediu para sair para 342 tambem - Edson */
                                        
                    FOR first it-nota-fisc WHERE 
                        it-nota-fisc.cod-estabel = ttDet.CodEstabelNF AND
                        it-nota-fisc.serie       = ttDet.SerieNF      AND
                        it-nota-fisc.nr-nota-fis = ttDet.NrNotaFisNF  AND 
                        it-nota-fisc.it-codigo   = ttDet.ItCodigoNF   AND
                        it-nota-fisc.nr-seq-fat  = ttDet.NrSeqFatNF   NO-LOCK.
                        
                        
                         for each  fat-ser-lote WHERE
                              fat-ser-lote.cod-estabel = nota-fiscal.cod-estabel AND
                              fat-ser-lote.serie       = it-nota-fisc.serie       AND
                              fat-ser-lote.nr-nota-fis = it-nota-fisc.nr-nota-fis AND
                              fat-ser-lote.nr-seq-fat  = it-nota-fisc.nr-seq-fat  AND
                              fat-ser-lote.it-codigo   = it-nota-fisc.it-codigo
                              NO-LOCK .
                              
                              if  c-desc-lotes = ""  then 
                                   c-desc-lotes = fat-ser-lote.nr-serlote.
                              else
                                   c-desc-lotes = c-desc-lotes + ", " + fat-ser-lote.nr-serlote.
                         end.
                         
                         
                         if c-desc-lotes <> ""   then c-desc-lotes = "(LT:" + c-desc-lotes + ")".
                        

                    end.
               end.    /* fim customizacao 380 paravimprimir lote na descricao*/ 
                
 
         FOR FIRST item-cli WHERE item-cli.it-codigo    = ITEM.it-codigo AND
                                  item-cli.cod-emitente = nota-fiscal.cod-emitente AND
                                  item-cli.log-2  NO-LOCK.
             c-desc-prod =    item-cli.item-do-cli + " " + item-cli.narrativa + " " + c-desc-prod.
             l-desc-prod = NO.
         END.
             

        CREATE ttDanfeItem.
        ASSIGN ttDanfeItem.iSeq           = i-cont-itens
               ttDanfeItem.cprod          = ttDet.cProd
               ttDanfeItem.descitem       = (IF l-desc-prod THEN ttDet.xProd ELSE "") + c-desc-prod + c-desc-lotes + ttDet.infAdProd  + (IF TRIM(c-nro-FCI) = "" THEN "" ELSE (" (FCI:" + c-nro-FCI + ")"))  /*customizacao largura Polofilms*/
               ttDanfeItem.ncm            = ttDet.NCM
               ttDanfeItem.cfop           = ttDet.CFOP
               /*Comercial*/
               ttDanfeItem.u              = ttDet.uCom
               ttDanfeItem.quantitem      = STRING(ttDet.qCom, "->>>,>>>,>>>,>>9.99<<":U)
/*               ttDanfeItem.vlunit         = STRING(ttDet.vUnCom , "->,>>>,>>>,>>>,>>9.99<<<<<<<<":U)  customizado Ricardo solicitou somente 4 casas*/
               ttDanfeItem.vlunit         = STRING(ttDet.vUnCom , "->,>>>,>>>,>>>,>>9.99<<":U)
               /*Tribut†vel*/
               ttDanfeItem.u-trib         = ttDet.uTrib
               ttDanfeItem.quantitem-trib = STRING(ttDet.qTrib, "->>>,>>>,>>>,>>9.99<<":U)
/*             ttDanfeItem.vlunit-trib    = STRING(ttDet.vUnTrib, "->,>>>,>>>,>>>,>>9.99<<<<<<<<":U)  customizado */
               ttDanfeItem.vlunit-trib    = STRING(ttDet.vUnTrib, "->,>>>,>>>,>>>,>>9.99<<":U)               
               ttDanfeItem.vltotitem      = STRING(ttDet.vProd, "->>>,>>>,>>>,>>>,>>9.99":U)
               /*ttDanfeItem.infAdProd      =*/
               /*ttDanfeItem.textoitem      =*/ .


        IF unfat = "BB"  THEN DO:
            FOR first it-nota-fisc WHERE 
                       it-nota-fisc.cod-estabel = ttDet.CodEstabelNF AND
                       it-nota-fisc.serie       = ttDet.SerieNF      AND
                       it-nota-fisc.nr-nota-fis = ttDet.NrNotaFisNF  AND 
                       it-nota-fisc.it-codigo   = ttDet.ItCodigoNF   AND
                       it-nota-fisc.nr-seq-fat  = ttDet.NrSeqFatNF   NO-LOCK.


               ASSIGN ttDanfeItem.u = "UN"  
                      ttDanfeItem.u-trib = "UN"
                      ttDanfeItem.vlunit-trib = STRING((IF dec(ttDet.vUnTrib) = dec(ttDet.vUnCom) THEN (dec(ttDet.vProd) / it-nota-fisc.qt-faturada[2]) ELSE
                                                 (dec(ttDet.vUnTrib) * dec(ttDet.qTrib) / it-nota-fisc.qt-faturada[2])), "->,>>>,>>>,>>>,>>9.99<<")
                      ttDanfeItem.vlunit = string(dec(ttDet.vProd) / it-nota-fisc.qt-faturada[2], "->,>>>,>>>,>>>,>>9.99<<" )                                   
                      ttDanfeItem.quantitem =  STRING(it-nota-fisc.qt-faturada[2] ,  "->>>,>>>,>>>,>>9.99<<") 
                      ttDanfeItem.quantitem-trib = string(it-nota-fisc.qt-faturada[2],  "->>>,>>>,>>>,>>9.99<<"). 
            END.
        END.
    
        FOR EACH ttICMSDanfe:
            DELETE ttICMSDanfe.
        END.
    
        /*--- Valores e Informaá‰es conforme tributaá∆o do ICMS --*/
        ASSIGN ttDanfeItem.vlbcicmit    = STRING(0, "->>>,>>>,>>>,>>9.99":U)
               ttDanfeItem.vlicmit      = STRING(0, "->>>,>>>,>>>,>>9.99":U)
               ttDanfeItem.icm          = STRING(0, "->>9.99":U)
               ttDanfeItem.vlbcicmit-st = STRING(0, "->>>,>>>,>>>,>>9.99":U)
               ttDanfeItem.vlicmit-st   = STRING(0, "->>>,>>>,>>>,>>9.99":U).
    
        {ftp/ft0518f.i6 ttICMS00}
        {ftp/ft0518f.i6 ttICMS10}
        {ftp/ft0518f.i6 ttICMS20}
        {ftp/ft0518f.i6 ttICMS30}
        {ftp/ft0518f.i6 ttICMS40}
        {ftp/ft0518f.i6 ttICMS51}
        {ftp/ft0518f.i6 ttICMS60}
        {ftp/ft0518f.i6 ttICMS70}
        {ftp/ft0518f.i6 ttICMS90}
        /*--- Fim - Valores e Informaá‰es conforme tributaá∆o do ICMS --*/
        
        /*--- Valores e Informaá‰es de IPI --*/
        ASSIGN ttDanfeItem.vlipiit = STRING(0, "->>>,>>>,>>>,>>9.99":U)
               ttDanfeItem.ipi     = STRING(0, "->>9.99":U).
    
        FOR FIRST ttIPI
           WHERE ttIPI.CodEstabelNF = ttDet.CodEstabelNF
             AND ttIPI.SerieNF      = ttDet.SerieNF     
             AND ttIPI.NrNotaFisNF  = ttDet.NrNotaFisNF 
             AND ttIPI.NrSeqFatNF   = ttDet.NrSeqFatNF
             AND ttIPI.ItCodigoNF   = ttDet.ItCodigoNF 
             AND ttIPI.l-ipi-trib   <> ?: /* quando l-ipi-trib = YES indica que tributa IPI */
            
            ASSIGN ttDanfeItem.vlipiit = STRING({ftp/ft0518f.i8 ttIPI.vIPI}, "->>>,>>>,>>>,>>9.99":U)
                   ttDanfeItem.ipi     = STRING({ftp/ft0518f.i8 ttIPI.pIPI}, "->>9.99":U).
    
        END.
        /*--- Fim - Valores e Informaá‰es de IPI --*/
    
        ASSIGN i-cont-itens = i-cont-itens + 1.
    END.

        FIND FIRST polo-laudo-cliente WHERE polo-laudo-cliente.nome-abrev = nota-fiscal.nome-ab-cli  NO-LOCK no-error.
        
              IF AVAIL polo-laudo-cliente AND polo-laudo-cliente.log-acompanha-nf = yes and sai-laudo-jr = "Sim" then
              
                  ASSIGN c-mess-laudo = c-mess-laudo + " *** CERTIFICADO DE QUALIDADE EM ANEXO *** " .



          find transporte where transporte.nome-abrev = nota-fiscal.nome-tr-red no-lock no-error.
          
          assign c-nome-tr = (IF AVAIL transporte THEN  transporte.nome  ELSE "" ) +
                             (IF AVAIL transporte THEN " - CGC: " + 
                                 string(transporte.cgc,"99.999.999/9999-99") ELSE "") 
                 c-end-tr-rd = "".
        
          IF AVAIL transporte THEN
             ASSIGN c-end-tr-rd = transporte.endereco + ", " +
                                  transporte.bairro   + ", " +
                                  transporte.cidade  + ", " + transporte.estado.
        
        
          if  c-nome-tr <> " "  then 
                assign   c-nome-tr = " - Transp.Redespacho:" + c-nome-tr + " - " + c-end-tr-rd.
          
        
        
        
            ASSIGN ttDanfe.informacoescomplementares = ttDanfe.informacoescomplementares + " " + c-mess-item + " " + c-mess-laudo + " " .
                
                /* ver se vai manter isto para todas empresas*/

            
            IF (substring(c-estab-uc,1,2) = "42" OR substring(c-estab-uc,1,2) = "41") OR  c-estab-uc = "702" OR l-item-polo THEN   /*solic-318*/  /*mudado para unigel comercial 16-11-2012 Edson*/
            ASSIGN ttDanfe.informacoescomplementares = ttDanfe.informacoescomplementares + " " + c-nome-tr.
        
        

END.
ELSE DO:  /*customizacao para imprimir um lote por linha*/

    ASSIGN i-cont-itens = 1.

    FOR EACH ttDet NO-LOCK, 
        EACH it-nota-fisc WHERE 
        it-nota-fisc.cod-estabel = ttDet.CodEstabelNF AND
        it-nota-fisc.serie       = ttDet.SerieNF      AND
        it-nota-fisc.nr-nota-fis = ttDet.NrNotaFisNF  AND 
        it-nota-fisc.it-codigo   = ttDet.ItCodigoNF   AND
        it-nota-fisc.nr-seq-fat  = ttDet.NrSeqFatNF   NO-LOCK,
        EACH tt-fat-ser-lote OF it-nota-fisc NO-LOCK,
        
        EACH  ITEM WHERE ITEM.it-codigo = ttDet.ItCodigoNF 
          NO-LOCK .


        d-fator = 1.
        c-desc-prod = "".

          ASSIGN d-fator = (IF it-nota-fisc.qt-faturada[1] <> it-nota-fisc.qt-faturada[2] THEN 
                           tt-fat-ser-lote.qt-baixada[2] / it-nota-fisc.qt-faturada[2] 
                      ELSE tt-fat-ser-lote.qt-baixada[1]) / it-nota-fisc.qt-faturada[1] .
                             

      /*customizacao para imprimir referecia para empresa 380 na descricao*/
                  IF ITEM.codigo-refer <> ""  AND (i-emp-uc = "380"  )  THEN
                      ASSIGN c-desc-prod = c-desc-prod + " (" + trim(ITEM.codigo-refer) + ")".


        FOR FIRST item-cli WHERE item-cli.it-codigo    = ITEM.it-codigo AND
                                      item-cli.cod-emitente = nota-fiscal.cod-emitente AND
                                      item-cli.log-2  NO-LOCK.
                 c-desc-prod =    item-cli.item-do-cli + " " + item-cli.narrativa + " " + c-desc-prod.
                 l-desc-prod = NO.
        END.
        CREATE ttDanfeItem.
        ASSIGN ttDanfeItem.iSeq           = i-cont-itens
               ttDanfeItem.cprod          = ttDet.cProd
               ttDanfeItem.descitem       = (IF AVAIL item-cli THEN "" ELSE ttDet.xProd) + c-desc-prod + ttDet.infAdProd + " LT." + string(tt-fat-ser-lote.nr-serlote,"x(10)") + (IF TRIM(c-nro-FCI) = "" THEN "" ELSE (" (FCI:" + c-nro-FCI + ")"))
               ttDanfeItem.ncm            = ttDet.NCM
               ttDanfeItem.cfop           = ttDet.CFOP
               /*Comercial*/
               ttDanfeItem.u              = ttDet.uCom
               ttDanfeItem.quantitem      = STRING(ttDet.qCom * d-fator, "->>>,>>>,>>>,>>9.99<<":U)
/*               ttDanfeItem.vlunit         = STRING(ttDet.vUnCom, "->,>>>,>>>,>>>,>>9.99<<<<<<<<":U) customizados*/
               ttDanfeItem.vlunit         = STRING(ttDet.vUnCom, "->,>>>,>>>,>>>,>>9.99<<":U)
               /*Tribut†vel*/
               ttDanfeItem.u-trib         = ttDet.uTrib
               ttDanfeItem.quantitem-trib = STRING(ttDet.qTrib * d-fator, "->>>,>>>,>>>,>>9.99<<":U)
/*               ttDanfeItem.vlunit-trib    = STRING(ttDet.vUnTrib, "->,>>>,>>>,>>>,>>9.99<<<<<<<<":U)  customizados*/
               ttDanfeItem.vlunit-trib    = STRING(ttDet.vUnTrib, "->,>>>,>>>,>>>,>>9.99<<":U)
               ttDanfeItem.vltotitem      = STRING(ttDet.vProd * d-fator, "->>>,>>>,>>>,>>>,>>9.99":U)
               /*ttDanfeItem.infAdProd      =*/
               /*ttDanfeItem.textoitem      =*/ .
    
        FOR EACH ttICMSDanfe:
            DELETE ttICMSDanfe.
        END.
    
        /*--- Valores e Informaá‰es conforme tributaá∆o do ICMS --*/
        ASSIGN ttDanfeItem.vlbcicmit    = STRING(0, "->>>,>>>,>>>,>>9.99":U)
               ttDanfeItem.vlicmit      = STRING(0, "->>>,>>>,>>>,>>9.99":U)
               ttDanfeItem.icm          = STRING(0, "->>9.99":U)
               ttDanfeItem.vlbcicmit-st = STRING(0, "->>>,>>>,>>>,>>9.99":U)
               ttDanfeItem.vlicmit-st   = STRING(0, "->>>,>>>,>>>,>>9.99":U).
    
        {ftp/ft0518f.i6 ttICMS00}
        {ftp/ft0518f.i6 ttICMS10}
        {ftp/ft0518f.i6 ttICMS20}
        {ftp/ft0518f.i6 ttICMS30}
        {ftp/ft0518f.i6 ttICMS40}
        {ftp/ft0518f.i6 ttICMS51}
        {ftp/ft0518f.i6 ttICMS60}
        {ftp/ft0518f.i6 ttICMS70}
        {ftp/ft0518f.i6 ttICMS90}

            ASSIGN  
              ttDanfeItem.vlbcicmit      = STRING(DEC(ttDanfeItem.vlbcicmit) * d-fator    , "->>>,>>>,>>>,>>9.99":U)
              ttDanfeItem.vlicmit        = STRING(DEC(ttDanfeItem.vlicmit) * d-fator  , "->>>,>>>,>>>,>>9.99":U)
              ttDanfeItem.icm            = STRING(DEC(ttDanfeItem.icm)    , "->>9.99":U)
              /*ICMS ST*/
              ttDanfeItem.vlbcicmit-st   = STRING(DEC(ttDanfeItem.vlbcicmit-st) * d-fator  , "->>>,>>>,>>>,>>9.99":U)
              ttDanfeItem.vlicmit-st     = STRING(DEC(ttDanfeItem.vlicmit-st) * d-fator, "->>>,>>>,>>>,>>9.99":U).





        /*--- Fim - Valores e Informaá‰es conforme tributaá∆o do ICMS --*/
    
        /*--- Valores e Informaá‰es de IPI --*/
        ASSIGN ttDanfeItem.vlipiit = STRING(0, "->>>,>>>,>>>,>>9.99":U)
               ttDanfeItem.ipi     = STRING(0, "->>9.99":U).
    
        FOR FIRST ttIPI
           WHERE ttIPI.CodEstabelNF = ttDet.CodEstabelNF
             AND ttIPI.SerieNF      = ttDet.SerieNF     
             AND ttIPI.NrNotaFisNF  = ttDet.NrNotaFisNF 
             AND ttIPI.NrSeqFatNF   = ttDet.NrSeqFatNF
             AND ttIPI.ItCodigoNF   = ttDet.ItCodigoNF 
             AND ttIPI.l-ipi-trib   <> ?: /* quando l-ipi-trib = YES indica que tributa IPI */
    
            ASSIGN ttDanfeItem.vlipiit = STRING({ftp/ft0518f.i8 ttIPI.vIPI}, "->>>,>>>,>>>,>>9.99":U)
                   ttDanfeItem.ipi     = STRING({ftp/ft0518f.i8 ttIPI.pIPI}, "->>9.99":U).
    
        END.

        ASSIGN ttDanfeItem.vlipiit = STRING(DEC(ttDanfeItem.vlipiit) * d-fator, "->>>,>>>,>>>,>>9.99":U).


        /*--- Fim - Valores e Informaá‰es de IPI --*/
    
        ASSIGN i-cont-itens = i-cont-itens + 1.
    END.


END.

/*===================================================================================================================================================*/
                      
/*customizaá∆o FCI*/
IF VALID-HANDLE(h-bodi538) THEN
    DELETE PROCEDURE h-bodi538.

DEFINE BUFFER bf-ttArquivo FOR ttArquivo.

FOR LAST bf-ttArquivo: END.
CREATE ttArquivo.
ASSIGN ttArquivo.sequencia   = IF AVAIL bf-ttArquivo THEN bf-ttArquivo.sequencia + 1 ELSE 1
       ttArquivo.nomeArquivo = TRIM(nota-fiscal.cod-estabel) + "-" + TRIM(nota-fiscal.serie) + "-" + TRIM(nota-fiscal.nr-nota-fis) + "-" + ".doc".


RUN ftp/ft0518f1.p(INPUT TABLE ttDanfe,
                   INPUT TABLE ttDanfeItem,
                   INPUT ttArquivo.nomeArquivo,
                   INPUT c-modelo-Danfe,
                   INPUT l-sem-Word).


/*customizacao
  envio de packinf list 
  na migracao mudar para um contato a ser definido
  ex: PAKING01, PAKING02, etc buscar os e-mails dentro do programa esft0034
  mas confirmar com usuarios
  */

  IF (substring(c-estab-uc,1,2) = "42" or substring(c-estab-uc,1,2) = "41" or c-estab-uc = "702" )  /*solic-318*/ /*mudado para unigel comercial em 16-11-2012 Edson*/
  and nota-fiscal.ind-sit-nota <> 2 then do:
  
         FOR first  cont-emit WHERE 
            cont-emit.cod-emitente = nota-fiscal.cod-emitente AND
            substring(cont-emit.nome,1,7) = "PACKING"  NO-LOCK .

 
     
             RUN ftp/esft0034rp.p (    input  nota-fiscal.nr-nota-fis,
                                      INPUT nota-fiscal.nr-nota-fis,
                                      input nota-fiscal.cod-emitente,
                                      input nota-fiscal.cod-estabel,
                                      input nota-fiscal.serie      ).
         end.

  end.

RETURN "OK":U.

/*fim*/
PROCEDURE pi-mensagem-remessa.


    i-pos =  INDEX (ttDanfe.informacoescomplementares,"nota fiscal numero").

    IF i-pos = 0 THEN RETURN.

    c-nt    = SUBSTRING(ttDanfe.informacoescomplementares, i-pos + 19 ,7).
    c-serie = SUBSTRING(ttDanfe.informacoescomplementares, i-pos + 34 ,2 ).


    FIND FIRST b-nota-desc WHERE b-nota-desc.nr-nota-fis = c-nt AND
                                 b-nota-desc.serie       = c-serie AND
                                 b-nota-desc.cod-estabel = nota-fiscal.cod-estabel.

    IF NOT avail b-nota-desc THEN return.

    FIND  b-natur-desc WHERE b-natur-desc.nat-operacao = b-nota-desc.nat-operacao NO-LOCK NO-ERROR.

    IF NOT avail b-natur-desc THEN return.

    i-pos-i = INDEX (b-nota-desc.observ-nota ,"Produto sera entregue por sua conta e ordem na").
    i-pos-f = INDEX (b-nota-desc.observ-nota , "emitida em " + STRING(nota-fiscal.dt-emis-nota,"99/99/9999")).

    IF  b-natur-desc.log-oper-triang AND 
        b-natur-desc.nat-vinculada = nota-fiscal.nat-operacao AND
        i-pos-i > 0 AND i-pos-f > 0 and
        INDEX(ttDanfe.informacoescomplementares,SUBSTRING(b-nota-desc.observ-nota,1,20) ) = 0
        THEN DO:

        ASSIGN ttDanfe.informacoescomplementares =  TRIM( ttDanfe.informacoescomplementares) + " " + 
                                                    SUBSTRING(b-nota-desc.observ-nota,1,i-pos-i - 1)  + " " + 
                                                    SUBSTRING(b-nota-desc.observ-nota,i-pos-f + 23,LENGTH(b-nota-desc.observ-nota)).


    END.



END PROCEDURE.

/*customizado*/
procedure pi-philip-morris.

DEFINE VARIABLE i-nr-ped-pmi AS INTEGER    NO-UNDO.

ASSIGN i-nr-ped-pmi = pallet.nr-pedido.  /* unigel comercial em 16-11-2012 Edson*/

    FOR  FIRST if-ped-venda WHERE if-ped-venda.nr-pedido = pallet.nr-pedido NO-LOCK.

        i-nr-ped-pmi =  if-ped-venda.nr-pedido-relac.

    END.

   find first b-ped-venda where b-ped-venda.nr-pedido = i-nr-ped-pmi no-lock no-error.

   if avail b-ped-venda and natur-oper.transf  then do:
     if  (
        b-ped-venda.cod-emitente = 17715 or
        b-ped-venda.cod-emitente = 17848 or
        b-ped-venda.cod-emitente = 17855 or
        b-ped-venda.cod-emitente = 17707 or
        b-ped-venda.cod-emitente = 17835 or
        b-ped-venda.cod-emitente = 17836)   then 
          c-mess-laudo = "***PMI - EMITIR LAUDO PELA NF DE TRANSFERENCIA".
   
   end.
   
  
end.


 
