/***********************************************************************
**  Descricao : Upc WRITE polo_off_ord_prod
**  Edson - Damgra
************************************************************************/
TRIGGER PROCEDURE FOR WRITE OF polo_off_ord_prod OLD BUFFER oldpolo_off_ord_prod.

    FIND FIRST ord-prod NO-ERROR.
    CREATE ord-prod.
    ASSIGN 
    ord-prod.nr-ord-produ                     =       polo_off_ord_prod.nr_ord_produ
    ord-prod.it-codigo                        =        "20tsy32mr"
    ord-prod.qt-ordem                         =      1
    ord-prod.qt-produzida                     =      0
    ord-prod.qt-refugada                      =      0
    ord-prod.un                               =        "KG"
    ord-prod.dt-inicio                        =        04/13/2014
    ord-prod.dt-termino                       =        04/14/2014 
    ord-prod.cd-planejado                     =        "10"
    ord-prod.estado                           =        7          
    ord-prod.emite-requis                     =        NO
    ord-prod.emite-ordem                      =       YES
    ord-prod.cod-depos                        =       "PRO"
    ord-prod.nr-req-sum                       =        0
    ord-prod.dt-emissao                       =        04/13/2014
    ord-prod.ct-codigo                        =        "117930"   
    ord-prod.sc-codigo                        =         "00000"
    ord-prod.qt-reportada                     =     0
    ord-prod.qt-requisita                     =      0
    ord-prod.lote-serie                       =        ""
    ord-prod.narrativa                        =        ""
    ord-prod.nr-linha                         =        1          
    ord-prod.tipo                             =        1 
    ord-prod.usuario-alt                      =        ?  
    ord-prod.data-alt                         =        04/13/2014 
    ord-prod.cod-estabel                      =       "{cdp\poloestab.i 422}"/*solic-318*/
    ord-prod.nome-abrev                       =       ""
    ord-prod.nr-pedido                        =       ""
    ord-prod.dt-orig                          =       04/13/2014 
    ord-prod.valorizada                       =        yes
    ord-prod.calc-cs-mat                      =        2
    ord-prod.reporte-mob                      =        2
    ord-prod.req-emitida                      =        yes
    ord-prod.prioridade                       =        99
    ord-prod.val-per                          =        no
    ord-prod.cod-refer                        =        ""
    ord-prod.cod-gr-cli                       =        0
    ord-prod.nr-ult-seq                       =        1
    ord-prod.rep-prod                         =        1
    ord-prod.qt-apr-cond                      =        0   
    ord-prod.qt-perda                         =        0 
    ord-prod.custeio-prop-mob                 =        1 
    ord-prod.qt-inicial                       =        1
    ord-prod.custeio-prop-mat                 =        1  
    ord-prod.prod-repet                       =        no
    ord-prod.nr-ord-aber                      =        0 
    ord-prod.nr-sequencia                     =        0
    ord-prod.cons-mrp                         =        YES 
    ord-prod.cons-pmp                         =        YES 
    ord-prod.ct-desp                          =        ""
    ord-prod.sc-desp                          =        "" 
    ord-prod.origem                           =        "CP"
    ord-prod.sit-aloc                         =        1 
    ord-prod.nr-ficha                         =        1 
    ord-prod.enc-mensal                       =        NO 
    ord-prod.it-inspec                        =        ""
    ord-prod.ct-imob                          =        "" 
    ord-prod.sc-imob                          =        "" 
    ord-prod.prototipo                        =        NO
    ord-prod.num-ord-inv                      =        0
    ord-prod.dest-manut                       =        1 
    ord-prod.nr-entrega                       =        0 
    ord-prod.nr-ord-refer                     =        0 
    ord-prod.conta-ordem                      =        "1179300000" 
    ord-prod.conta-despesa                    =        ""
    ord-prod.conta-imob                       =        "" 
    ord-prod.custeio-prop-ggf                 =        1
    ord-prod.calc-cs-mob                      =        2
    ord-prod.calc-cs-ggf                      =        2
    ord-prod.reporte-ggf                      =        1   
    ord-prod.nr-estrut                        =        1
    ord-prod.item-cotacao                     =        ""
    ord-prod.sequencia                        =        0
    ord-prod.es-codigo                        =        "" 
    ord-prod.nr-estrut-filha                  =        0 
    ord-prod.char-1                           =        "" 
    ord-prod.char-2                           =        "" 
    ord-prod.dec-1                            =        0
    ord-prod.dec-2                            =        0
    ord-prod.int-1                            =        0 
    ord-prod.int-2                            =        0 
    ord-prod.log-1                            =        NO 
    ord-prod.log-2                            =        NO 
    ord-prod.data-1                           =         ?
    ord-prod.data-2                           =         ?
    ord-prod.check-sum                        =        ""
    ord-prod.cod-roteiro                      =        ""
    ord-prod.cod-lista-compon                 =        "" 
    ord-prod.dt-efetiv-term                   =        04/14/2014 
    ord-prod.hr-efetiv-term                   =        "212811"
    ord-prod.cod-estab-ctr                    =        "" 
    ord-prod.nr-seq-contr-it                  =        0
    ord-prod.nr-contrato-venda                =        0
    ord-prod.cdn-orig-aps                     =        1 
    ord-prod.log-ord-export-aps               =        NO
    ord-prod.log-control-estoq-refugo         =        NO 
    ord-prod.log-refugo-preco-fisc            =        NO 
    ord-prod.cod-item-refugo                  =        ""
    ord-prod.val-relac-refugo-item            =        0
    ord-prod.cod-unid-negoc                   =        "00" 

    .


/* Fim do Programa */
