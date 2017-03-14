/******************************************************************************
**  programa..: POLCQ0210UPC.P
**  elaboracao: DEZEMBRO / 2003.
**  autor.....: Carlos Rafael Nobrega - Datasul Manufatura.
**  objetivo..: InclusÆo de um novo Folder no programa CQ0210.
******************************************************************************/

/*********************** Defini‡Æo de Parƒmetros *************************/
define input parameter p-ind-event               as character      no-undo.
define input parameter p-ind-object              as character      no-undo.
define input parameter p-wgh-object              as handle         no-undo.
define input parameter p-wgh-frame               as widget-handle  no-undo.
define input parameter p-cod-table               as character      no-undo.
define input parameter p-row-table               as rowid          no-undo.

/******************** Defini‡Æo de Vari veis Globais *********************/
define new global shared var  v-log-grava           as logical        no-undo.

define new global shared var  adm-broker-hdl        as handle         no-undo.
define new global shared var  h-viewer              as handle         no-undo.
Define var  c-folder                             as Char           no-undo.
Define Var  c-objects                            as Char           no-undo.
Define Var  wh-objeto                            as widget-handle  no-undo.
define new global shared var h-folder            as handle         no-undo.
def var c-return as char        no-undo.
def var l-erro as log         no-undo.
def var iCont as int         no-undo.
def var i-objects as int         no-undo.
def var h-object as handle         no-undo.
def var l-record as log         no-undo.
def var l-group-assign as log         no-undo.
def var c-objeto  as CHAR          no-undo.

define new global shared var h-button           as handle         no-undo.
define /*new global shared*/ var wh-bt-cliente      as widget-handle         no-undo.
define /*new global shared*/ var wh-bt-lote-item    as widget-handle         no-undo.

define new global shared var grw-exam-ficha as rowid no-undo.
define new global shared var grw-ficha-cq as rowid no-undo.



assign c-objeto = entry(num-entries(p-wgh-object:private-data, "~/"), p-wgh-object:private-data, "~/").

/************************** In¡cio do Programa ***************************/

/**************** Cria‡Æo das P ginas 'ESPECIFICAS' para o Folder j  existente ****************/
if not valid-handle(h-button) then
   run getFieldHandle (input "bt-resultado",
                       output h-button).
if valid-handle(h-button) then
   assign h-button:sensitive = no.
   
   
if p-cod-table = "exam-ficha" and
   p-row-table <> ?           then
   assign grw-exam-ficha = p-row-table.

if p-cod-table = "ficha-cq" and
   p-row-table <> ?           then
   assign grw-ficha-cq = p-row-table.
   
IF  valid-handle(h-button) THEN  assign h-button:sensitive = YES.
IF  grw-exam-ficha <> ? and
    valid-handle(h-button) THEN DO:
    FIND ficha-cq WHERE ROWID(ficha-cq) = grw-exam-ficha NO-LOCK NO-ERROR.
    IF AVAIL ficha-cq  THEN DO:
        FIND estabelec WHERE estabelec.cod-estabel = ficha-cq.cod-estabel NO-LOCK NO-ERROR.
        IF AVAIL estabelec AND (estabelec.ep-codigo = "420" OR estabelec.ep-codigo = "410") THEN  /*solic-318*/ 
             assign h-button:sensitive = no.

    END.

END.


if c-objeto = "b01in113.w"           and 
   not valid-handle(wh-bt-cliente)   and 
   not valid-handle(wh-bt-lote-item) then do:

   create button wh-bt-cliente
   assign frame = p-wgh-frame
          row = if valid-handle(h-button) then h-button:row else 0
          col = 35
          width = 15 
          font = 1
          height = 1
          label = "Lib.Lote Cliente"
          visible = yes
          sensitive = yes
          hidden = no
        triggers:
          on choose persistent run cqp/upc/upc-cq0210-a.p.
        end triggers. 


   create button wh-bt-lote-item
   assign frame = p-wgh-frame
          row = if valid-handle(h-button) then h-button:row else 0
          col = 50
          width = 15 
          font = 1
          height = 1
          label = "Config.Lote"
          visible = yes
          sensitive = yes
          hidden = no
        triggers:
          on choose persistent run cqp/upc/upc-cq0210-b.p.
        end triggers. 

end.

if  p-ind-event   = "INITIALIZE"   and 
    p-ind-object  = "CONTAINER"    then do:

    RUN get-link-handle IN adm-broker-hdl (INPUT p-wgh-object,
                                           INPUT "PAGE-SOURCE":U,
                                           OUTPUT c-folder).
    assign h-folder = Widget-Handle(c-folder) No-Error.
    If  Valid-Handle(h-folder) Then Do:
        RUN create-folder-page IN h-folder (INPUT 2,
                                            INPUT "Exame Cliente":U).
        RUN create-folder-label IN h-folder (INPUT 2,
                                             INPUT "Exame Cliente":U).

        RUN select-page IN p-wgh-object (INPUT 2).
 
        RUN init-object IN p-wgh-object (INPUT "cqp\upc\polcq0210v01.w", 
                                         INPUT p-wgh-frame,
                                         INPUT "Layout = ":U,
                                         OUTPUT h-viewer).
        RUN set-position IN h-viewer (10.5 , 3.00).
        
        RUN get-link-handle IN adm-broker-hdl (INPUT p-wgh-object,
                                               INPUT "CONTAINER-TARGET":U,
                                               OUTPUT c-objects).


        do i-objects = 1 to num-entries(c-objects):
           assign h-object = widget-handle(entry(i-objects, c-objects)).

           if index(h-object:private-data, "qry") <> 0 and  /* Vocˆ deve verificar se e a query principal */
               not l-record then do:
               assign l-record = yes.
               RUN add-link IN adm-broker-hdl (INPUT h-object,
                                               INPUT "Record":U,
                                               INPUT h-viewer).
           end.
          
           if index(h-object:private-data, "vwr") <> 0 and /* Voce deve verificar se e a viewer principal */
              not l-group-assign then do:
              assign l-group-assign = yes.
             
              RUN add-link IN adm-broker-hdl (INPUT h-object, 
                                              INPUT "Group-Assign":U,
                                              INPUT h-viewer).
           end.
        end.
       
        RUN dispatch IN h-viewer ("initialize":U).
        RUN select-page IN p-wgh-object (INPUT 2).

    End.
End.

PROCEDURE getFieldHandle:
   DEF INPUT PARAMETER  p-campo  AS CHAR NO-UNDO.
   DEF OUTPUT PARAMETER p-handle AS HANDLE NO-UNDO.
  
   def var h_frame as widget-handle no-undo. 

   ASSIGN h_Frame = p-wgh-frame:FIRST-CHILD. /* pegando o Field-Group */
   ASSIGN h_Frame = h_Frame:FIRST-CHILD.     /* pegando o 1o. Campo */
   
   DO WHILE h_Frame <> ? :
      IF h_frame:type <> "field-group" THEN DO:  
         IF h_Frame:NAME = p-campo THEN DO :
            assign p-handle = h_Frame.
            leave.
         END.
         ASSIGN h_Frame = h_Frame:NEXT-SIBLING.
      END. 
      ELSE
         ASSIGN h_frame = h_frame:first-child.
   END.

END.

