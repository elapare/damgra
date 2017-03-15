&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          espmulti           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i V99XX999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/viewerd.w

/* global variable definitions */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var v-row-parent as rowid no-undo.

DEFINE VARIABLE sai-laudo-jr  AS LOGICAL    NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE nome-abrev-jr AS CHARACTER  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE it-codigo-jr  AS CHARACTER  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES am-cq-result-laudo
&Scoped-define FIRST-EXTERNAL-TABLE am-cq-result-laudo


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR am-cq-result-laudo.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS am-cq-result-laudo.descricao ~
am-cq-result-laudo.metodo am-cq-result-laudo.unidade ~
am-cq-result-laudo.soma-result am-cq-result-laudo.qtd-result ~
am-cq-result-laudo.menor-result am-cq-result-laudo.maior-result ~
am-cq-result-laudo.media-result am-cq-result-laudo.espec-min ~
am-cq-result-laudo.espec-max am-cq-result-laudo.espec-alvo 
&Scoped-define ENABLED-TABLES am-cq-result-laudo
&Scoped-define FIRST-ENABLED-TABLE am-cq-result-laudo
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS am-cq-result-laudo.cod-estabel ~
am-cq-result-laudo.nr-nota-fis am-cq-result-laudo.nr-laudo ~
am-cq-result-laudo.cod-exame am-cq-result-laudo.cod-comp ~
am-cq-result-laudo.descricao am-cq-result-laudo.metodo ~
am-cq-result-laudo.unidade am-cq-result-laudo.soma-result ~
am-cq-result-laudo.qtd-result am-cq-result-laudo.menor-result ~
am-cq-result-laudo.maior-result am-cq-result-laudo.media-result ~
am-cq-result-laudo.espec-min am-cq-result-laudo.espec-max ~
am-cq-result-laudo.espec-alvo 
&Scoped-define DISPLAYED-TABLES am-cq-result-laudo
&Scoped-define FIRST-DISPLAYED-TABLE am-cq-result-laudo


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS am-cq-result-laudo.cod-exame ~
am-cq-result-laudo.cod-comp 
&Scoped-define ADM-ASSIGN-FIELDS am-cq-result-laudo.cod-estabel ~
am-cq-result-laudo.nr-nota-fis am-cq-result-laudo.nr-laudo 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88.57 BY 2.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88.57 BY 6.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     am-cq-result-laudo.cod-estabel AT ROW 1.17 COL 11 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7.86 BY .88
     am-cq-result-laudo.nr-nota-fis AT ROW 1.17 COL 34.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.14 BY .88
     am-cq-result-laudo.nr-laudo AT ROW 1.17 COL 71.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.43 BY .88
     am-cq-result-laudo.cod-exame AT ROW 2.17 COL 11 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     am-cq-result-laudo.cod-comp AT ROW 2.17 COL 34.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.57 BY .88
     am-cq-result-laudo.descricao AT ROW 2.17 COL 55 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 31 BY .88
     am-cq-result-laudo.metodo AT ROW 3.71 COL 11 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 31.14 BY .88
     am-cq-result-laudo.unidade AT ROW 3.71 COL 61 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 25 BY .88
     am-cq-result-laudo.soma-result AT ROW 4.71 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY .88
     am-cq-result-laudo.qtd-result AT ROW 4.71 COL 61 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 25 BY .88
     am-cq-result-laudo.menor-result AT ROW 5.71 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY .88
     am-cq-result-laudo.maior-result AT ROW 5.71 COL 61 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 25 BY .88
     am-cq-result-laudo.media-result AT ROW 6.71 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY .88
     am-cq-result-laudo.espec-min AT ROW 6.71 COL 61 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 25 BY .88
     am-cq-result-laudo.espec-max AT ROW 7.67 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY .88
     am-cq-result-laudo.espec-alvo AT ROW 7.67 COL 61 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 25 BY .88
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 3.5 COL 1
     "Est:" VIEW-AS TEXT
          SIZE 4 BY .67 AT ROW 1.25 COL 8.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: espmulti.am-cq-result-laudo
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 8.63
         WIDTH              = 88.72.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{include/c-viewer.i}
{utp/ut-glob.i}
{include/i_dbtype.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN am-cq-result-laudo.cod-comp IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN am-cq-result-laudo.cod-estabel IN FRAME f-main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN am-cq-result-laudo.cod-exame IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN am-cq-result-laudo.nr-laudo IN FRAME f-main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN am-cq-result-laudo.nr-nota-fis IN FRAME f-main
   NO-ENABLE 2                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME am-cq-result-laudo.cod-comp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL am-cq-result-laudo.cod-comp V-table-Win
ON LEAVE OF am-cq-result-laudo.cod-comp IN FRAME f-main /* Componente */
DO:

   ASSIGN sai-laudo-jr = NO.

   FIND FIRST polo-esp-cliente-cq WHERE
        polo-esp-cliente-cq.nome-abrev  = nome-abrev-jr  AND 
        polo-esp-cliente-cq.it-codigo   = it-codigo-jr     AND
        polo-esp-cliente-cq.cod-exame   = INT(am-cq-result-laudo.cod-exame:SCREEN-VALUE IN FRAME {&FRAME-NAME}) AND
        polo-esp-cliente-cq.cod-comp    = int(am-cq-result-laudo.cod-comp:SCREEN-VALUE IN FRAME {&FRAME-NAME})
        NO-LOCK NO-ERROR. 
   
   IF AVAIL polo-esp-cliente-cq  AND polo-esp-cliente-cq.log-imprime-laudo = YES 
      THEN 
       ASSIGN am-cq-result-laudo.descricao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = polo-esp-cliente-cq.descricao
              am-cq-result-laudo.espec-min:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(polo-esp-cliente-cq.espec-min)
              am-cq-result-laudo.espec-max:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(polo-esp-cliente-cq.espec-max)
              am-cq-result-laudo.espec-alvo:SCREEN-VALUE IN FRAME {&FRAME-NAME}= string(polo-esp-cliente-cq.espec-alvo)
              am-cq-result-laudo.unidade:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = polo-esp-cliente-cq.unidade
              am-cq-result-laudo.metodo:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = polo-esp-cliente-cq.metodo
              sai-laudo-jr  = yes.
   
   IF sai-laudo-jr = no THEN DO:
   
       IF am-cq-result-laudo.cod-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "421" OR am-cq-result-laudo.cod-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "411" THEN  /*solic-318*/ 
           ASSIGN nome-abrev-jr = "POLO MG".
         ELSE
           ASSIGN nome-abrev-jr = "POLO RS".
   
   
       FIND FIRST polo-esp-cliente-cq WHERE
            polo-esp-cliente-cq.nome-abrev  = nome-abrev-jr  AND 
            polo-esp-cliente-cq.it-codigo   = it-codigo-jr     AND
            polo-esp-cliente-cq.cod-exame   = INT(am-cq-result-laudo.cod-exame:SCREEN-VALUE IN FRAME {&FRAME-NAME}) AND
            polo-esp-cliente-cq.cod-comp    = int(am-cq-result-laudo.cod-comp:SCREEN-VALUE IN FRAME {&FRAME-NAME})     
            NO-LOCK NO-ERROR. 
   
   
       IF AVAIL polo-esp-cliente-cq  AND polo-esp-cliente-cq.log-imprime-laudo = YES 
          THEN 
   
           ASSIGN am-cq-result-laudo.descricao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = polo-esp-cliente-cq.descricao
                  am-cq-result-laudo.espec-min:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(polo-esp-cliente-cq.espec-min)
                  am-cq-result-laudo.espec-max:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(polo-esp-cliente-cq.espec-max)
                  am-cq-result-laudo.espec-alvo:SCREEN-VALUE IN FRAME {&FRAME-NAME}= string(polo-esp-cliente-cq.espec-alvo)
                  am-cq-result-laudo.unidade:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = polo-esp-cliente-cq.unidade
                  am-cq-result-laudo.metodo:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = polo-esp-cliente-cq.metodo
                  sai-laudo-jr = yes.
   
     
   END. 

   IF sai-laudo-jr = NO THEN DO:

         run utp/ut-msgs.p (input "show":U, input 28250, "Cliente, Item, Exame, Componente").
         APPLY "entry" TO am-cq-result-laudo.cod-exame IN FRAME {&FRAME-NAME}.
         RETURN 'nok':U.

   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME am-cq-result-laudo.cod-exame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL am-cq-result-laudo.cod-exame V-table-Win
ON ENTRY OF am-cq-result-laudo.cod-exame IN FRAME f-main /* Exame */
DO:
  
        FIND am-cq-laudo WHERE ROWID (am-cq-laudo) = v-row-parent
            NO-LOCK NO-ERROR.
        
        IF AVAIL am-cq-laudo THEN DO:
        
            ASSIGN am-cq-result-laudo.cod-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = am-cq-laudo.cod-estabel
                   am-cq-result-laudo.nr-nota-fis:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = am-cq-laudo.nr-nota-fis
                   am-cq-result-laudo.nr-laudo:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = STRING (am-cq-laudo.nr-laudo).
        
        END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL am-cq-result-laudo.cod-exame V-table-Win
ON F5 OF am-cq-result-laudo.cod-exame IN FRAME f-main /* Exame */
DO:
    
    {include/zoomvar.i &prog-zoom=inzoom/z01in114.w
    &campo=am-cq-result-laudo.cod-exame
    &campozoom=cod-exame}.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL am-cq-result-laudo.cod-exame V-table-Win
ON LEAVE OF am-cq-result-laudo.cod-exame IN FRAME f-main /* Exame */
DO:
        
     FIND FIRST exame WHERE
         exame.cod-exame = int(am-cq-result-laudo.cod-exame:SCREEN-VALUE IN FRAME {&FRAME-NAME}) 
         NO-LOCK NO-ERROR.

     IF NOT AVAIL exame THEN DO:

        run utp/ut-msgs.p (input "show":U, input 2, "Exame").
        APPLY "entry" TO am-cq-result-laudo.cod-exame IN FRAME {&FRAME-NAME}.
        RETURN "nok".

     END.

  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL am-cq-result-laudo.cod-exame V-table-Win
ON MOUSE-SELECT-DBLCLICK OF am-cq-result-laudo.cod-exame IN FRAME f-main /* Exame */
DO:
    APPLY "f5" TO SELF.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

    am-cq-result-laudo.cod-exame:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "am-cq-result-laudo"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "am-cq-result-laudo"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME f-main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/


    /* Code placed here will execute PRIOR to standard behavior. */

    IF NOT FRAME {&FRAME-NAME}:VALIDATE() THEN
        RETURN 'ADM-ERROR':U.
    
    /*:T Ponha na pi-validate todas as valida‡äes */
    /*:T NÆo gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
     FIND FIRST exame WHERE
         exame.cod-exame = int(am-cq-result-laudo.cod-exame:SCREEN-VALUE IN FRAME {&FRAME-NAME}) 
         NO-LOCK NO-ERROR.

     IF NOT AVAIL exame THEN DO:

        run utp/ut-msgs.p (input "show":U, input 2, "Exame").
        APPLY "entry" TO am-cq-result-laudo.descricao IN FRAME {&FRAME-NAME}.
        return 'ADM-ERROR':U.

     END.


     ASSIGN sai-laudo-jr = NO.
    
     FIND FIRST polo-esp-cliente-cq WHERE
          polo-esp-cliente-cq.nome-abrev  = nome-abrev-jr  AND 
          polo-esp-cliente-cq.it-codigo   = it-codigo-jr     AND
          polo-esp-cliente-cq.cod-exame   = INT(am-cq-result-laudo.cod-exame:SCREEN-VALUE IN FRAME {&FRAME-NAME}) AND
          polo-esp-cliente-cq.cod-comp    = int(am-cq-result-laudo.cod-comp:SCREEN-VALUE IN FRAME {&FRAME-NAME})
          NO-LOCK NO-ERROR. 
     
     IF AVAIL polo-esp-cliente-cq  AND polo-esp-cliente-cq.log-imprime-laudo = YES 
        THEN ASSIGN sai-laudo-jr  = yes.
     
     IF sai-laudo-jr = no THEN DO:
     
         IF am-cq-result-laudo.cod-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "421" OR am-cq-result-laudo.cod-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "411" THEN /*solic-318*/ 
             ASSIGN nome-abrev-jr = "POLO MG".
           ELSE
             ASSIGN nome-abrev-jr = "POLO RS".
     
     
         FIND FIRST polo-esp-cliente-cq WHERE
              polo-esp-cliente-cq.nome-abrev  = nome-abrev-jr  AND 
              polo-esp-cliente-cq.it-codigo   = it-codigo-jr     AND
              polo-esp-cliente-cq.cod-exame   = INT(am-cq-result-laudo.cod-exame:SCREEN-VALUE IN FRAME {&FRAME-NAME}) AND
              polo-esp-cliente-cq.cod-comp    = int(am-cq-result-laudo.cod-comp:SCREEN-VALUE IN FRAME {&FRAME-NAME})     
              NO-LOCK NO-ERROR. 
     
     
         IF AVAIL polo-esp-cliente-cq  AND polo-esp-cliente-cq.log-imprime-laudo = YES 
            THEN ASSIGN sai-laudo-jr = yes.
       
     END. 
    
     IF sai-laudo-jr = NO THEN DO:
    
           run utp/ut-msgs.p (input "show":U, input 28250, "Cliente, Item, Exame, Componente").
           APPLY "entry" TO am-cq-result-laudo.descricao IN FRAME {&FRAME-NAME}.
           return 'ADM-ERROR':U.
    
     END.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignïs nÆo feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /* Dispatch standard ADM method.                             */

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  FIND am-cq-laudo WHERE
      ROWID(am-cq-laudo) = v-row-parent
      NO-LOCK NO-ERROR.

  IF AVAIL am-cq-laudo THEN DO:

      ASSIGN am-cq-result-laudo.cod-estabel = am-cq-laudo.cod-estabel
             am-cq-result-laudo.nr-nota-fis = am-cq-laudo.nr-nota-fis
             am-cq-result-laudo.nr-laudo    = am-cq-laudo.nr-laudo.
             
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    disable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
  if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.

    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    if adm-new-record = yes then
        enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-parent V-table-Win 
PROCEDURE pi-atualiza-parent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define input parameter v-row-parent-externo as rowid no-undo.
    
    assign v-row-parent = v-row-parent-externo.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*:T------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: NÆo fazer assign aqui. Nesta procedure
  devem ser colocadas apenas valida‡äes, pois neste ponto do programa o registro 
  ainda nÆo foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /*:T Valida‡Æo de dicion rio */
    
/*:T    Segue um exemplo de valida‡Æo de programa */
/*       find tabela where tabela.campo1 = c-variavel and               */
/*                         tabela.campo2 > i-variavel no-lock no-error. */
      
      /*:T Este include deve ser colocado sempre antes do ut-msgs.p */
/*       {include/i-vldprg.i}                                             */
/*       run utp/ut-msgs.p (input "show":U, input 7, input return-value). */
/*       return 'ADM-ERROR':U.                                            */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "am-cq-result-laudo"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

