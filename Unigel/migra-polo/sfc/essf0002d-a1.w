&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*:T*******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i D99XX999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def new global shared var l-peso-bal as logical INITIAL NO.
DEF VAR am-ep-codigo LIKE  permissao.ep-codigo NO-UNDO.
 
DEFINE VARIABLE c-seg-usuario-ant AS CHARACTER  NO-UNDO.
{utp/ut-glob.i}
DEFINE VARIABLE c-computador AS CHARACTER FORMAT "x(20)" NO-UNDO.
{include/i-win.i}
RUN utp/ut-utils.p PERSISTENT SET h-prog.
RUN GetComputerName IN h-prog(OUTPUT c-computador).
DELETE PROCEDURE h-prog. 
DEFINE VARIABLE os-file AS CHARACTER NO-UNDO FORMAT "x(60)" LABEL "File". 


define temp-table tt-erros
    field cod-erro  as integer
    field desc-erro as character format "x(256)"
    field desc-arq  as character.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-usuario fi-senha bt-ok bt-cancela ~
bt-ajuda rt-buttom 
&Scoped-Define DISPLAYED-OBJECTS fi-usuario fi-senha 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "&Ajuda" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE fi-senha AS CHARACTER FORMAT "X(256)":U 
     LABEL "Senha" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE fi-usuario AS CHARACTER FORMAT "X(256)":U 
     LABEL "Usuario" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 49 BY 1.42
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     fi-usuario AT ROW 2 COL 13 COLON-ALIGNED
     fi-senha AT ROW 3.75 COL 13 COLON-ALIGNED BLANK 
     bt-ok AT ROW 5.5 COL 3
     bt-cancela AT ROW 5.5 COL 14
     bt-ajuda AT ROW 5.5 COL 39
     rt-buttom AT ROW 5.25 COL 1
     SPACE(0.00) SKIP(0.07)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert SmartDialog title>"
         DEFAULT-BUTTON bt-ok.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/d-dialog.i}
/*{utp/ut-glob.i}*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   L-To-R                                                               */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* <insert SmartDialog title> */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda D-Dialog
ON CHOOSE OF bt-ajuda IN FRAME D-Dialog /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok D-Dialog
ON CHOOSE OF bt-ok IN FRAME D-Dialog /* OK */
DO:
     ASSIGN l-peso-bal = no.
 ASSIGN c-seg-usuario-ant = c-seg-usuario.
   FIND usuar_mestre WHERE usuar_mestre.cod_usuario =input FI-USUARIO:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.
   IF NOT AVAIL usuar_mestre OR ENCODE(input FI-SENHA:SCREEN-VALUE IN FRAME {&FRAME-NAME}) <> usuar_mestre.cod_senha THEN DO:

/*run btb/btapi910za.p (input FI-USUARIO:SCREEN-VALUE IN FRAME {&FRAME-NAME}, input FI-SENHA:SCREEN-VALUE IN FRAME {&FRAME-NAME}, output table tt-erros).

ASSIGN c-seg-usuario = c-seg-usuario-ant.

FIND FIRST tt-erros NO-LOCK NO-ERROR.
IF AVAIL tt-erros THEN DO:*/
      run utp/ut-msgs.p (input "show",
                         input 158,
                         input "USUµRIO/SENHA").
        APPLY "ENTRY" TO FI-USUARIO IN FRAME {&FRAME-NAME}.
        return NO-APPLY.
END.
 
FIND FIRST estabelec WHERE estabelec.cod-estabel = "412" OR estabelec.cod-estabel = "422" NO-LOCK NO-ERROR./*solic-318*/




               FIND ext_usuar_grp_usuar WHERE
                          ext_usuar_grp_usuar.cod_grp     = "SFCPESO" AND
                          ext_usuar_grp_usuar.cod_usuario = FI-USUARIO:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                          NO-LOCK NO-ERROR.
                  
               if  not avail ext_usuar_grp_usuar  then 
               DO:
                       RUN utp/ut-msgs.p (INPUT ("show"),INPUT 18728,
                                           INPUT "Liberar digita‡Æo de Pesagem").
                               APPLY "ENTRY" TO FI-USUARIO IN FRAME {&FRAME-NAME}.
                             return NO-APPLY.
               END.
               ELSE
               DO:
                    ASSIGN l-peso-bal = YES.

                    OS-FILE = "m:\dts\log_prd".
 
                    FILE-INFO:FILE-NAME = os-file.  

                    IF FILE-INFO:PATHNAME = ? THEN DO:
                       OS-FILE = "\\ungusb-vap01\Sistemas\DTS\Log_Prd\essf0002.LOG".

                    END.
                    ELSE DO:
                        OS-FILE = "m:\dts\log_prd\essf0002.LOG".
                    END.

                    
                    
                        OUTPUT TO VALUE(OS-FILE) APPEND.
                                DISP "Prog: essf0002 " FORMAT "x(19)" string(TODAY) "-" string(TIME,"hh:mm:ss") "-" " Comp:" c-computador NO-LABEL " Liberado por: " FI-USUARIO:SCREEN-VALUE IN FRAME {&FRAME-NAME} " Logado: " string(c-seg-usuario) WITH WIDTH 300.
                        OUTPUT CLOSE.
                    
               END.
                
         

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fi-usuario fi-senha 
      WITH FRAME D-Dialog.
  ENABLE fi-usuario fi-senha bt-ok bt-cancela bt-ajuda rt-buttom 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy D-Dialog 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  {utp/ut9000.i "essf0002-A1" "1.00.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
  
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

