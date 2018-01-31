$PBExportHeader$n_rehab_progress.sru
$PBExportComments$Rehab Progress business rules
forward
global type n_rehab_progress from n_pdc
end type
end forward

global type n_rehab_progress from n_pdc
end type
global n_rehab_progress n_rehab_progress

type variables
LONG              			il_claim_no, il_task_no
DATE						idt_current_date
BOOLEAN					ib_suppress_pbmessage, ib_create_authorization
U_DS						ids_rehab_task_authorization_insert
DataWindowChild		idw_event_type_list,	idw_event_specific_type_list
end variables

forward prototypes
public function integer nf_check_bus_rule ()
public function integer nf_check_mandatory ()
public function integer nf_set_unused_fields ()
public function long nf_set_identifiers ()
public function integer nf_change_item (long al_datawindow, long al_row, string as_dw_column, string as_data)
public subroutine nf_protect_add_event (boolean ab_protect)
public function integer nf_set_task_no (long al_task_no)
public function integer nf_set_current_date ()
public function integer nf_clear_progress_note_event ()
public function integer nf_add_event ()
public function integer nf_set_claim_no (long al_claim_no)
public function integer nf_add_progress_note (long al_event_no)
public function long nf_get_next_claim_event_no ()
public function boolean nf_check_accident_date (date adt_date)
public function integer nf_set_defaults (integer ai_datawindow, long al_current_row, long al_event_no, long al_docid)
public function integer nf_retrieve_progress ()
public function integer nf_display_task_success ()
public function integer nf_check_selected_doc (long al_docid, string as_document_type, integer ai_event_no)
public function integer nf_attach_document (long al_docid, integer ai_event_no)
public function integer nf_add_physio_tele_authorization (long al_claim_no, long al_task_no)
public function long nf_get_next_authorization_no ()
public function integer nf_update ()
end prototypes

public function integer nf_check_bus_rule ();LONG			ll_row, ll_claim_no, ll_task_no
STRING			ls_task_status,		ls_success_code_required, ls_event_type_code, ls_service_code, ls_program_code
DATETIME		ldtm_start_date,	ldtm_completion_date
INTEGER		li_check, li_return, li_authorization_thing_to_do, li_count
DATE			ldt_current


/* ----  VALIDATE TASK PROGRESS DETAILS  ---- */
ldtm_start_date 				= idw_dw[1].GetItemDateTime(1,"actual_start_date")
ll_claim_no							= idw_dw[1].getitemnumber(1,'claim_no')
ll_task_no 							= idw_dw[1].getitemnumber(1,'task_no')

IF idw_dw[2].ROWCOUNT() > 0 THEN
	ls_event_type_code   		= idw_dw[2].getitemstring(1,'event_type_code')
END IF 

/* Actual Start and Completion Dates*/
IF NOT IsNull(ldtm_start_date) THEN
	IF NOT nf_check_accident_date(Date(ldtm_start_date))  THEN
		MessageBox('Invalid Actual Start','The Actual Start date cannot be before the accident date.')
		idw_dw[1].SetColumn('actual_start_date')
		idw_dw[1].SetFocus()
		RETURN -1
	END IF
END IF

/* Validate - Actual Start Date cannot be in the future*/
IF Date(ldtm_start_date) > Date(f_server_datetime()) THEN
	MessageBox('Invalid Actual Start Date', "The Actual Start Date cannot be in the future")
	idw_dw[1].SetColumn('actual_start_date')
	idw_dw[1].SetFocus()
	RETURN -1
END IF

/* Validate - actual completion date cannot be prior to actual start date & cannot be in the future */
ldtm_completion_date= idw_dw[1].GetItemDateTime(1,"actual_completion_date")	
IF not isnull(ldtm_completion_date) THEN
	IF not isnull(ldtm_start_date) THEN
		IF ldtm_completion_date < ldtm_start_date THEN
			MessageBox('Invalid Actual Completion','The Actual Completion date cannot be before the Actual Start date.')
			RETURN -1
		END IF
	ELSE
		MessageBox('Invalid Actual Start','The Actual Start date cannot be blank when the Actual Completion Date is entered')
		RETURN -1
	END IF	
END IF

ls_task_status = idw_dw[1].GetItemString(1,"task_status_code")

/*	Task Status 
	If the Status is Planned, the Actual Start and Actual Completion dates should be blank
	If the Status is In Progress, the Actual Start date should be entered but the Actual Completion date should not
	If the Status is Cancelled or Completed, Actual Start and Actual Completion dates should be entered
*/	

CHOOSE CASE ls_task_status 
		
	CASE '01' 	/* Planned Task */
		IF NOT isnull(ldtm_start_date) OR NOT isnull(ldtm_completion_date) THEN
			MessageBox('Invalid Actual Dates','The Actual Start date and the Actual Completion date must be blank for a Planned task')
			RETURN -1
		END IF	

	CASE '02' 	/* In Progress Task */
		IF isnull(ldtm_start_date)  THEN
			MessageBox('Invalid Actual Dates','The Actual Start date should NOT be blank for a task that is In Progress')
			RETURN -1
		END IF	
		
		IF NOT isnull(ldtm_completion_date) THEN
			MessageBox('Invalid Actual Dates','The Actual Completion date should be blank for a task that is In Progress')
			RETURN -1
		END IF	

	CASE ELSE	/* Cancelled or Completed Task */
		IF isnull(ldtm_start_date) OR isnull(ldtm_completion_date) THEN
			MessageBox('Invalid Actual Dates','The Actual Start and Completion dates must NOT be blank for a Cancelled or Completed task')
			RETURN -1
		END IF	

END CHOOSE

/* Task Success and Task Result
		- 	For tasks that require a task result, the task result should be either successful, unsuccessful or
			not yet determined when the task is completed or cancelled
		- 	If the tasks result is successful, unsuccessful or not yet determined, then the task status should
			be either cancelled or completed
*/
ls_success_code_required = idw_dw[1].GetItemString(1,'success_code_required')

IF (ls_task_status = '03' OR ls_task_status = '04') AND ls_success_code_required = "Y" AND (idw_dw[1].GetItemString(1,"task_success_code") = 'I') THEN
	MessageBox('Invalid Task Success','The Task Result must be Successful, Not Successful or Not Yet Determined '+ &
	                     +'for a Cancelled or Completed task.')
	RETURN -1
END IF

IF (ls_task_status = '01' OR ls_task_status = '02') AND ls_success_code_required = "Y" AND (idw_dw[1].GetItemString(1,"task_success_code") <> 'X') THEN
	MessageBox('Invalid Task Success', 'The Task Success must be Not Yet Determined for a Planned or In Progress task.')
	RETURN -1
END IF

/* the majority of the business rules have been checked now insert an authorization if applicable - there are no BR's associated with this process
	(i.e the REHAB_TASK.rehab_service_code = ‘S022’ and the REHAB_TASK.rehab_program_code = ‘P001’).
	IF ls_rehab_program_code = 'P001' AND ls_rehab_service_code = 'S002' THEN

	On the Progress Note tab:
	For the new task type that is active:
	Task status is cancelled – allow the progress note to be logged. If the progress note is for the special Telephone Consult, inform the user but do not create the Authorization for the Telephone Consult. 
	Task status is closed –  allow the progress note to be logged. If the progress note is for the special Telephone Consult, warn the user on the Save and give the user the option to create the authorization or not. The progress note will always be saved.
	For the old task types that are no longer active:
	Regardless of the task status, if the progress note is for the special Telephone Consult, do not create the Authorization for the Telephone Consult.
	
	//	ls_task_status_code = dw_progress_task_list.getitemstring(ll_row,'task_status_code')
	
	NOTE: New changes in function call.
	
	
	task_status_code task_status_desc
	---------------- ----------------------------------------
	01               planned
	02               in progress
	03               cancelled
	04               closed
	05               reset
*/

/* ALSO ..... 

5.50    An authorization for a telephone consultation must be automatically generated if a progress note of a physiotherapist telephone consultation is logged, provided:
•	the telephone consultation is an authorized billable item for the type of task 
•	the course of treatment is for Primary physiotherapy
•	the task is Planned or In Progress 
•	an authorization for a telephone consultation does not already exist for the task for the same authorization date or, 
     if it already exists, the user confirmed that this authorization is to be created 
	(i.e  if a Telephone Consultation-Physio progress note event_type_code ‘061’ for a  REHAB_TASK.rehab_service_code = ‘S022’ 
	and the REHAB_TASK.rehab_program_code = ‘P001’ and task_status_code  = ‘01’ or ’02 and the billable item ‘257’ exists on the Billable_Item_Rehab_Task_Xref 
	for the task_type_code, task_sub_type_cod & task_specific_code, create the REHAB_TASK_AUTHROIZATION for billable item ‘257 
	if it doesn’t already exist with REHAB_TASK_AUTHROIZATION.authorized_date = current date or if it does exist, the user confirmed that the new authorization should still be created ’).
*/
ldt_current = DATE(f_server_datetime())

SELECT count(*) 
INTO :li_count 
FROM REHAB_TASK_AUTHORIZATION  a
	join billable_item_rehab_task_xref b ON a.billable_xref_no = b.billable_xref_no
WHERE billable_Item_no 	= 257 
AND 		a.claim_no 			= :ll_claim_no
AND 		a.task_no 				= :ll_task_no
AND 		convert(char(10), a.authorized_date, 121) = :ldt_current
USING	SQLCA;
SQLCA.nf_handle_error('n_rehab_progress','nf_add_physio_tele_authorization()','SELECT count(*) ...') 

/* we have the task_no and claim_no the task exists so simply select the information we require right off the top */
SELECT 	rehab_service_code, rehab_program_code
INTO			:ls_service_code, :ls_program_code
FROM		REHAB_TASK
WHERE 	claim_no 	= :ll_claim_no
AND 			task_no 	= :ll_task_no
USING 		SQLCA;
SQLCA.nf_handle_error('n_rehab_progress','nf_check_bus_rule()','SELECT rehab_service_code, rehab_pro...') 

IF ls_service_code = 'S022' AND ls_program_code = 'P001'  AND ls_event_type_code = '061' AND idw_dw[2].GetItemStatus(1,0,Primary!) = NEWMODIFIED!THEN 
		
		li_authorization_thing_to_do 	= 0 //create authorization
		ib_create_authorization 			= FALSE
		
		CHOOSE CASE ls_task_status 
			
			CASE '01', '02'	/* Planned Task, In Progress Task  */	
				
				li_authorization_thing_to_do = 0 //create authorization
				
				IF li_count > 0 THEN
					IF messagebox( 'Telephone Consultation', 'The progress note will be created but an authorization for a Telephone Consultation already exists for today.' +&
									                                            '~rClick OK to create another authorization or CANCEL if you do not want another authorization created.',  Exclamation!, OKCancel!, 2) = 2 THEN 
							li_authorization_thing_to_do = 1
					ELSE
							li_authorization_thing_to_do = 0 
					END IF 		
				END IF 
						
			CASE '03' /* cancelled */
					/* Task status is cancelled – allow the progress note to be logged. If the progress note is for the special Telephone Consult, 
						 inform the user but do not create the Authorization for the Telephone Consult. 
					*/
					MessageBox('Task status is cancelled','The progress Note is for the special Telephone Consult. An authorization cannot be created due to the task status')
					li_authorization_thing_to_do = 1
									
			CASE '04' /* closed */
				
					/* Task status is closed –  allow the progress note to be logged. If the progress note is for the special Telephone Consult, warn the user on the 
						 Save and give the user the option to create the authorization or not. The progress note will always be saved.
						 
						 REVISED: 20120503
						 Can we change the wording from  
						 ‘Click OK to continue with save. Cancel will save the Progress Note but will not save the Authorization’  
							to

							 ‘Click OK to Create the Authorization for the Telephone Consult. CANCEL will Save the Progress Note but will NOT create the Authorization’	 
					*/
					li_return = MessageBox('Task status is closed','The progress Note is for the special Telephone Consult. Click OK to Create the Authorization for the Telephone Consult. ' +&
					                                  '~rCANCEL will Save the Progress Note but will NOT create the Authorization', question!, OKCancel! ,2)
					IF li_return = 1 THEN 
						//DO NOTHING 
						li_authorization_thing_to_do = 0 //create authorization
					ELSE 
						li_authorization_thing_to_do = 1 //do not create authorization
					END IF 
					
				CASE '05' /* Reset */
					
						li_authorization_thing_to_do = 1 //do not create authorization
							
		END CHOOSE
	
		// Create the telephone authorization	
		IF li_authorization_thing_to_do = 0 THEN 
			li_check = nf_add_physio_tele_authorization(ll_claim_no, ll_task_no)
			CHOOSE CASE li_check
				CASE 0 //NOTHING TO DO
					 ib_create_authorization = FALSE 
				CASE 1 // GOOD
					 ib_create_authorization = TRUE 
				CASE 100 // NOT FOUND 
					 ib_create_authorization = FALSE 
					/*	For the old task types that are no longer active:
						Regardless of the task status, if the progress note is for the special Telephone Consult, do not create the Authorization for the Telephone Consult. */
				CASE ELSE
					 ib_create_authorization = FALSE 
					MessageBox('Error','There was a problem creating a new auto Physio Telephone Authorization. Please try again.')
					RETURN -1
			END CHOOSE 
	END IF
END IF 

/* ----  VALIDATE PROGRESS NOTE EVENT  ---- */
IF idw_dw[2].GetItemStatus(1,0,Primary!) = New! THEN
/*	New row but no data was entered */
	idw_dw[2].DeleteRow(1)		
END IF

RETURN 0




end function

public function integer nf_check_mandatory ();/* accept the data windows */
IF idw_dw[1].AcceptText() < 0 THEN RETURN -1
IF idw_dw[2].AcceptText() < 0 THEN RETURN -1

IF idw_dw[1].RowCount() < 1 THEN
	MessageBox('Missing Task Information','The task information is missing.  Please select a task and try again.')
	RETURN -1
END IF

IF idw_dw[2].RowCount() > 1 THEN
	IF IsNull(idw_dw[2].GetItemDateTime(1,'event_date')) THEN
		MessageBox('Missing Progress Date','Please enter date for progress note.')
		RETURN -1
	END IF
END IF

RETURN 0
end function

public function integer nf_set_unused_fields ();LONG  ll_row

/* Determine which fields are null and fill them in with a default value
 	at some point in time any of the following fields may be unused
*/

/* Check the Progress Event */
ll_row = idw_dw[2].GetRow()
IF ll_row > 0 THEN
   IF Isnull(idw_dw[2].GetItemString(ll_row,'event_specific_code')) or idw_dw[2].GetItemString(ll_row,'event_specific_code') = '' THEN
		IF idw_dw[2].GetItemStatus(1,0,Primary!) <> NotModified! THEN
			idw_dw[2].SetItem(ll_row, 'event_specific_code', ' ')
		END IF
	END IF
END IF

Return 0


end function

public function long nf_set_identifiers ();LONG  ll_event_no, ll_row

/* If a Progress Note Event was added, get the next event no for the claim */
ll_row = idw_dw[2].GetRow()
IF idw_dw[2].RowCount() > 0 THEN
	IF idw_dw[2].GetItemStatus(1,0,Primary!) <> notmodified! THEN
   	ll_event_no = nf_get_next_claim_event_no()
		idw_dw[2].Setitem(1,"event_no", ll_event_no)
		/* Set up the Rehab Task Progress Note datawindow */
		IF nf_add_progress_note(ll_event_no) < 0 THEN Return -1
		IF il_task_no = 0 THEN
			MessageBox("Task Number Error","A task number of 0 is invalid, please cancel and try again.")
			RETURN -1	// Temporary fix. Somehow the task number is getting a value of 0. Not allowed.
		END IF
	ELSE
		idw_dw[3].Reset()
	END IF
END IF

Return 0

end function

public function integer nf_change_item (long al_datawindow, long al_row, string as_dw_column, string as_data);STRING				ls_event_specific_code
DATETIME			ldtm_date
LONG 				ll_cnt

CHOOSE CASE al_datawindow
	CASE 1 	/* Task Progress details */
			
		CHOOSE CASE as_dw_column
			CASE "task_status_code" // REMOVED WINDOW NO LONGER HAS UPDATE FUNCTIONALITY
										
		 	CASE 'actual_start_date'  // REMOVED WINDOW NO LONGER HAS UPDATE FUNCTIONALITY
				
			CASE 'actual_completion_date'  // REMOVED WINDOW NO LONGER HAS UPDATE FUNCTIONALITY
 	  	END CHOOSE
					
  CASE 2 	/* Progress Note Events*/
			
		ib_suppress_pbmessage = True

   	CHOOSE CASE as_dw_column
			CASE "event_type_code"
				idw_event_specific_type_list.SetFilter("event_type_code = '" + as_data + "'")
				idw_event_specific_type_list.Filter()

				ll_cnt = idw_event_specific_type_list.RowCount()
					
				IF idw_event_specific_type_list.RowCount() > 0 THEN
					idw_dw[2].Modify("event_specific_code.Protect=0")
					ls_event_specific_code = idw_event_specific_type_list.GetItemString(1,"event_specific_code")
					idw_dw[2].SetItem(1,"event_specific_code",ls_event_specific_code)
				ELSE
					idw_dw[2].Modify("event_specific_code.Protect=1")
					idw_dw[2].SetItem(1,"event_specific_code","")
				END IF
    END CHOOSE
			
		ib_suppress_pbmessage = False
END CHOOSE

RETURN 0
end function

public subroutine nf_protect_add_event (boolean ab_protect);/* 
	This function protects/unprotects the event columns. Used to control whether the
	user may update the columns
*/

IF ab_protect THEN
		idw_dw[2].Modify("event_date.protect=1")
		idw_dw[2].Modify("event_type_code.protect=1")
		idw_dw[2].Modify("event_comment.Edit.DisplayOnly=True")
		idw_dw[2].Modify("event_specific_code.protect=1")
	ELSE
		idw_dw[2].Modify("event_date.protect=0")
		idw_dw[2].Modify("event_type_code.protect=0")
		idw_dw[2].Modify("event_comment.Edit.DisplayOnly=False")
		idw_dw[2].Modify("event_specific_code.protect=0")
	END IF

end subroutine

public function integer nf_set_task_no (long al_task_no);/* Set the task number of the currently-selected task
*/

il_task_no = al_task_no

return 0
end function

public function integer nf_set_current_date ();
/* set the default values for the Progress Note insert */
	
idt_current_date = Date(f_server_datetime())


return 0
end function

public function integer nf_clear_progress_note_event ();/* On Cancel or if the user selects another task from the task list,
	clear the progress note event datawindow 
*/
	IF idw_dw[2].GetRow() = 1 THEN
		idw_dw[2].Reset()
	END IF
	
	return 0
		
end function

public function integer nf_add_event ();/* This function is used to add new progress note events  */
LONG   ll_current_row, ll_results
	
idw_dw[2].Reset()

ll_current_row = idw_dw[2].InsertRow(0)
IF ll_current_row < 0 THEN RETURN -1
	
/* Allow the user to enter the event details
*/
nf_protect_add_event(FALSE)

/* Get the drop-down event type and event sepcific type lists 
*/
idw_dw[2].GetChild("event_type_code",idw_event_type_list)
idw_dw[2].GetChild("event_specific_code",idw_event_specific_type_list)
	
/* Default the Claim Event columns. The arguments for this function are
	the datawindow number, current row number, event number and document id
*/
IF nf_set_defaults(2,ll_current_row,0,0) < 0 THEN RETURN -1

/* Filter the Event Specific List to the "Other" Event Type
*/
idw_event_specific_type_list.SetFilter("event_type_code = '999'")
idw_event_specific_type_list.Filter()
	
idw_dw[2].SetRedraw(True)
idw_dw[2].SetFocus()
idw_dw[2].SetColumn("event_type_code")
	
RETURN 0
end function

public function integer nf_set_claim_no (long al_claim_no);/* Set the claim number retrieved for the rehab plan */
	
il_claim_no = al_claim_no
 
Return 0
end function

public function integer nf_add_progress_note (long al_event_no);LONG  	ll_current_row

/* Insert a new Rehab Task Progress Note into the datawindow  */
idw_dw[3].Reset()
ll_current_row = idw_dw[3].InsertRow(0)

IF ll_current_row < 0 THEN	RETURN -1

	
/* Default the Progress Note columns. The arguments for this function are
	the datawindow number, current row number, event number and document id
*/
IF nf_set_defaults(3,ll_current_row, al_event_no,0) < 0 THEN RETURN -1

RETURN 0

end function

public function long nf_get_next_claim_event_no ();LONG	ll_event_no

/* Function Name: nf_get_next_claim_event_no                                            
	Purpose:       The purpose of this module is to get the next available event number for the claim
	Arguments:     Claim Number                                                                                  
	Return Values: The next available event number for the claim                                                    

*/

/*	get the next available event no for the claim */
SELECT Max(event_no)
INTO 	:ll_event_no
FROM 	CLAIM_EVENT
WHERE claim_no = :il_claim_no;
	
SQLCA.nf_handle_error("Embedded SQL: SELECT CLAIM_EVENT","n_rehab_progress","nf_get_next_claim_event_no")

IF IsNull(ll_event_no) THEN
	ll_event_no = 1
ELSE
	ll_event_no = ll_event_no + 1
END IF

RETURN ll_event_no
end function

public function boolean nf_check_accident_date (date adt_date);DATETIME	ldtm_accident_date

SELECT accident_date
INTO 	:ldtm_accident_date
FROM 	CLAIM
WHERE claim_no = :il_claim_no
USING 	SQLCA;
	 
SQLCA.nf_handle_error('Embedded SQL: select from CLAIM','n_rehab_progress','nf_check_accident_date()') 
	
/* If the date passed in is before the accident date - the check fails 	*/
IF adt_date < Date(ldtm_accident_date)   THEN		RETURN FALSE
	
RETURN TRUE
end function

public function integer nf_set_defaults (integer ai_datawindow, long al_current_row, long al_event_no, long al_docid);
CHOOSE CASE ai_datawindow

CASE 2
	/* Set the default columns for the Claim Event */
	IF al_current_row > 0 THEN
		idw_dw[2].SetItem(al_current_row, "claim_no", il_claim_no)
		idw_dw[2].SetItem(al_current_row, "event_date", idt_current_date)
		idw_dw[2].SetItem(al_current_row, "event_type_code", "999")
	END IF


CASE 3
	/* Set the columns for the Rehab Task Progress Note for the Claim Event	*/
	IF al_current_row > 0 THEN
		idw_dw[3].SetItem(al_current_row,"claim_no", il_claim_no)
		idw_dw[3].SetItem(al_current_row,"task_no", il_task_no)
		idw_dw[3].SetItem(al_current_row,"event_no", al_event_no)
	END IF

CASE 4
	/* Set the columns for the Rehab Task Attachment based on the document selected	*/
	IF al_current_row > 0 THEN
		idw_dw[4].SetItem(al_current_row,"claim_no", il_claim_no)
		idw_dw[4].SetItem(al_current_row,"task_no", il_task_no)
		idw_dw[4].SetItem(al_current_row,"event_no", al_event_no)
		idw_dw[4].SetItem(al_current_row,"docid", al_docid)
	END IF


END CHOOSE


Return 0
end function

public function integer nf_retrieve_progress ();
/* Re-Retrieve the Task Progress Details */
idw_dw[1].Retrieve(il_claim_no , il_task_no)
SQLCA.nf_handle_error('Retrieve of dw','n_rehab_progress','nf_retrieve_progress') 

/* Determine whether to display the Task Success Flag for the selected task 
*/
nf_display_task_success()

RETURN 0
end function

public function integer nf_display_task_success ();LONG	ll_row


/* Determine whether or not to display the Task Success Code
	depending on the type of task selected. Only Surgery and Treatment 
	tasks can modify this code (those tasks with a 'success flag required' = Yes) */

ll_row = idw_dw[1].GetRow()
IF ll_row > 0 THEN
	IF idw_dw[1].GetItemString(ll_row,'success_code_required') = "Y" THEN
		idw_dw[1].Object.task_success_label.Visible = 1
		idw_dw[1].Object.task_success_code.Visible = 1
	ELSE
		idw_dw[1].Object.task_success_label.Visible = 0
		idw_dw[1].Object.task_success_code.Visible = 0
	END IF
END IF

return 0

end function

public function integer nf_check_selected_doc (long al_docid, string as_document_type, integer ai_event_no);INTEGER 	li_response 
STRING   ls_attach_word

/* Verify the Document to be attached is the one selected and that
	this document is not already attached to the task for this claim.
*/
	IF ai_event_no > 0 THEN
		ls_attach_word = 'progress note'
	ELSE
		ls_attach_word = 'task'
	END IF

	IF idw_dw[4].Retrieve(il_claim_no, il_task_no, al_docid) > 0 THEN
		MessageBox ("Document Attachment", "The document selected is already attached to this task/note.", StopSign!)
		Return -1
	ELSE
		li_response = MessageBox ("Document Attachment","Attach this document (" + as_document_type + ") ~r~n" + &
			"to the " + ls_attach_word + " ?" , Question!, YesNo!)
		IF li_response = 1 THEN
			// THIS function begins and commits a transaction
			li_response = nf_attach_document(al_docid, ai_event_no)
		ELSE
			li_response = -1
		END IF
	END IF

	Return li_response

end function

public function integer nf_attach_document (long al_docid, integer ai_event_no);/* This function is used to add a document attachment to a task/note for a claim */

LONG   		ll_current_row, ll_results,	ll_docid
INTEGER  	li_event_no

/* Set the document id to the document id passed in. */
ll_docid = al_docid
 
/* Set the event_no to the event_no passed in. */
li_event_no = ai_event_no
 
/* Insert a new rehab document attachment */
idw_dw[4].Reset()
ll_current_row = idw_dw[4].InsertRow(0)
IF ll_current_row < 0 THEN RETURN -1

/* Default the Rehab Attachment columns. The arguments for this function are
	the datawindow number, current row number, event number and document id
*/
IF nf_set_defaults(4,ll_current_row,li_event_no,ll_docid) < 0 THEN 	RETURN -1

SQLCA.nf_begin_transaction()

/* Update the data  */
idw_dw[4].Update()
SQLCA.nf_handle_error("Rehab Task Attachment ","n_rehab_progress","nf_attach_document()") 

SQLCA.nf_commit_transaction()


RETURN 0
	
end function

public function integer nf_add_physio_tele_authorization (long al_claim_no, long al_task_no);/* 
Rehab Plan

5.50    An authorization for a telephone consultation must be automatically generated for a progress note of a physiotherapist telephone 
consultation that is logged under the course of treatment for primary physiotherapy, provided the task is planned or in progress 
and the telephone consultation is an authorized billable item for the type of task  (i.e  if a Telephone Consultation-Physio progress note for a  
REHAB_TASK.rehab_service_code = ‘S022’ and the REHAB_TASK.rehab_program_code = ‘P001’ and task_status_code  = ‘01’ or ’02
and the billable item ‘257’ exists on the Billable_Item_Rehab_Task_Xref for the task_type_code, task_sub_type_cod & task_specific_code, 
create the REHAB_TASK_AUTHROIZATION for billable item ‘257’).

An authorization for a physiotherapist telephone consultation must be automatically generated 
if a progress note for a physiotherapist telephone consultation is logged under the course of treatment for primary physiotherapy 
(i.e the REHAB_TASK.rehab_service_code = ‘S022’ and the REHAB_TASK.rehab_program_code = ‘P001’).

Claim Event
A claim event for a Telephone Consultation-Physio must not be manually created by the user.

	For each billable item to be authorized automatically for a Task:
	Create a Rehab Task Authorization record with the following values:
	Authorization number	system generated
	Task number						REHAB_TASK.task_no for the task
	Claim number						REHAB_TASK.claim_no for the task
	Billable xref number			Billable_Item_Rehab_Task.billable_xref_no
	Authorized quantity			Billable_Item_Rehab_Task.default_authorized_qty
	Authorized amount			If the Billable_Item_Rehab_Task.fixed_fee_flag is Yes
												Billable_Item.fee for the billable item and the current date
											Otherwise,
												Billable_Item_Rehab_Task.max_authorized_amount
	Paid Quantity						0 (Zero)
	Paid Amount						0 (Zero)
	Authorized Provider Type	REHAB_TASK.provider_type_code
	Authorized Provider No		REHAB_TASK.provider_no
	Authorized Date					Current date
	Authorized By User Id			User Id of the user creating the Task
	Authorization Comment		Blank
	Expedited Billing Flag			No
	Fixed Fee Flag					Billable_Item_Rehab_Task.fixed_fee_flag  
	Auto Created Flag				Yes
	
	NEW: 2012-02-15
	
	Diana and I discussed this and the solution we came up with is that we would add functionality to the Progress Note tab that would 
	display a pop-up message to determine if the user wanted an authorization created if there was already one created that day:

	If all conditions are met that would allow for an authorization for Telephone Consult to be created 
	If there already is a Telephone Consult authorization created for this task with an authorization date  of the current date (i.e. another exists for today)
		Then,  
	Inform the user that a Telephone Consult was already authorized for today and ask the user if they want to create another one
	If the user says YES,
		Create a new authorization for Telephone Consult
	If the user says NO,
		Do Not create a new authorization for Telephone Consult
	Otherwise
		Create a new authorization for Telephone Consult
					
*/

INTEGER		 li_row, li_count, li_message
STRING			ls_service_code, ls_program_code, ls_task_type_code, ls_task_subtype_code, ls_task_specific_code
STRING			ls_fixed_fee_flag, ls_auto_create_authorization_flag, ls_provider_type_code, ls_user_id
STRING			ls_authorized_amount_required_flag, ls_explicit_authorization_flag, ls_auto_invoice_flag, ls_manual_invoice_flag
LONG			ll_billable_xref_no, ll_provider_no, ll_billable_item_no, ll_no
DATE			ldt_current_date
DECIMAL		ldec_default_authorized_qty, ldec_max_authorized_amount, ldec_authorized_amount, ldec_authorized_quantity
DECIMAL		ldec_max_authorized_qty
DATE			ldt_current

/* we have the task_no and claim_no the task exists so simply select the information we require right off the top */
SELECT 	rehab_service_code, rehab_program_code, task_type_code, task_sub_type_code, 
           		task_specific_code, provider_no, provider_type_code
INTO			:ls_service_code, :ls_program_code, :ls_task_type_code, :ls_task_subtype_code, :ls_task_specific_code,
            	:ll_provider_no, :ls_provider_type_code
FROM		REHAB_TASK
WHERE 	claim_no 	= :al_claim_no
AND 			task_no 	= :al_task_no
USING 		SQLCA;
SQLCA.nf_handle_error('n_rehab_progress','nf_add_physio_tele_authorization()','SELECT rehab_service_code, rehab_pro...') 

/*
An authorization for a physiotherapist telephone consultation must be automatically generated if a progress note for a physiotherapist
telephone consultation is logged under the course of treatment for primary physiotherapy
(i.e the REHAB_TASK.rehab_service_code = ‘S022’ and the REHAB_TASK.rehab_program_code = ‘P001’).
THIS CHECK IS ALSO DONE FROM THE CALLING CODE
*/
IF ls_service_code <> 'S022' OR ls_program_code <> 'P001'  THEN RETURN 0 //NOTHING TO DO

/* filter for the specific one we want or do a select here based on the values we need*/
ll_billable_item_no 	= 257

/* check if one already exists  for the day */
ldt_current = DATE(f_server_datetime())

/*
AND			task_type_code         	= 'TR'
AND         task_sub_type_code  	= '031'
*/

/* retrieve the various authorizations we require */
SELECT 	default_authorized_qty,   						max_authorized_qty,   					max_authorized_amount,   
				authorized_amount_required_flag,  		auto_create_authorization_flag,   	explicit_authorization_flag,  
				fixed_fee_flag,             				 			auto_invoice_flag,   						manual_invoice_flag,
				billable_xref_no
INTO			:ldec_default_authorized_qty,   				:ldec_max_authorized_qty,   			:ldec_max_authorized_amount,   
				:ls_authorized_amount_required_flag, 	:ls_auto_create_authorization_flag, :ls_explicit_authorization_flag,  
				:ls_fixed_fee_flag,             				 	:ls_auto_invoice_flag,   					:ls_manual_invoice_flag,
				:ll_billable_xref_no
FROM 		billable_item_rehab_task_xref   
WHERE 	active_flag 					= 'Y'
AND 			rehab_service_code 		= :ls_service_code
AND 			rehab_program_code 	= :ls_program_code
AND			task_type_code         	= :ls_task_type_code
AND         task_sub_type_code  	= :ls_task_subtype_code
AND         billable_item_no         	= :ll_billable_item_no
AND			task_specific_code        = :ls_task_specific_code
USING 		SQLCA;

//no rows - there must be 1 row
IF SQLCA.nf_handle_error('n_rehab_progress','nf_add_physio_tele_authorization()','SELECT default_authorized_qty, max_authorized_qty, ') = 100 THEN RETURN 100

f_user_id(ls_user_id)

//Default the values needed - these are taken from the DD as we do not have a direct reference from the database
ids_rehab_task_authorization_insert 					= CREATE u_ds
ids_rehab_task_authorization_insert.DataObject 	= 'd_rehab_task_authorization_insert'
ids_rehab_task_authorization_insert.SetTransObject(SQLCA)

ls_auto_create_authorization_flag 	= 'Y' //as per document
                       
li_row = ids_rehab_task_authorization_insert.insertrow(0)

ldec_authorized_amount = ldec_max_authorized_amount

ids_rehab_task_authorization_insert.SetItem(li_row,'claim_no', al_claim_no)
ids_rehab_task_authorization_insert.SetItem(li_row,'task_no', al_task_no)
	
ll_no = nf_get_next_authorization_no()
IF ll_no > 0 THEN
	ids_rehab_task_authorization_insert.SetItem(li_row,'authorization_no',ll_no)
ELSE
	RETURN -1
END IF
	
//This is checked by the Business rules as the values come the user selection
IF ISNULL(ls_provider_type_code) 	THEN ls_provider_type_code 	= ''
IF ISNULL(ll_provider_no) 				THEN ll_provider_no 				= 0
	
ids_rehab_task_authorization_insert.SetItem(li_row,'billable_xref_no', ll_billable_xref_no)
ids_rehab_task_authorization_insert.SetItem(li_row,'paid_quantity',0)
ids_rehab_task_authorization_insert.SetItem(li_row,'paid_amount',0)
ids_rehab_task_authorization_insert.SetItem(li_row,'authorized_provider_type_code', ls_provider_type_code)
ids_rehab_task_authorization_insert.SetItem(li_row,'authorized_provider_no', ll_provider_no)
ids_rehab_task_authorization_insert.SetItem(li_row,'authorized_date',ldt_current)
ids_rehab_task_authorization_insert.SetItem(li_row,'authorized_by_user_id',ls_user_id)
ids_rehab_task_authorization_insert.SetItem(li_row,'authorization_comment','')
ids_rehab_task_authorization_insert.SetItem(li_row,'expedited_billing_flag','N')
ids_rehab_task_authorization_insert.SetItem(li_row,'fixed_fee_flag', ls_fixed_fee_flag)
ids_rehab_task_authorization_insert.SetItem(li_row,'auto_created_flag', ls_auto_create_authorization_flag)
//ids_rehab_task_authorization_insert.SetItem(li_row,'web_create_id',0)
//ids_rehab_task_authorization_insert.SetItem(li_row,'web_modify_id',0)
ids_rehab_task_authorization_insert.SetItem(li_row,'authorized_amount',ldec_authorized_amount)
ids_rehab_task_authorization_insert.SetItem(li_row,'authorized_quantity',ldec_default_authorized_qty)

//  (JP) Hi In the WorkBench, you should be able to rely on the database defaults to set the values for the web_create_id, web_modify_id, 
//    web_create_date and web_modify_date.

RETURN 1
end function

public function long nf_get_next_authorization_no ();LONG	ll_authorization_no

UPDATE Last_Authorization_No SET last_authorization_no = last_authorization_no + 1 USING SQLCA;
SQLCA.nf_handle_error("n_rehab_progress","nf_get_next_authorization_no()","UPDATE Last_Authorization_No SET last_authorization_no")

CHOOSE CASE SQLCA.SQLNRows
	/*	If update was successful (ie. SQLNRows would equal 1), read back the identifier */	
	CASE 1
	
		SELECT Last_Authorization_No.last_authorization_no INTO :ll_authorization_no FROM Last_Authorization_No USING SQLCA;
		 SQLCA.nf_handle_error("n_rehab_progress","nf_get_next_authorization_no()","SELECT Last_Authorization_No.last_authorization_no")
			
	CASE ELSE
		/*		if anything other than 1 record found, display error*/
		SQLCA.nf_rollback_transaction()
		IF SQLCA.SQLCode <> 0 THEN
			Error.Text = "Error during rollback of Last_authorization_No in function wf_get_next_authorization_no"
			Error.WindowMenu="w_authorization"
			Error.Object=""
			Error.ObjectEvent="uf_next_authorization_no"
			SignalError()
		END IF		
		MessageBox("authorization Module - Data Integrity Error", string(SQLCA.SQLNRows) + " record(s) found in Last_authorization_No~r~nPlease call the help desk",Exclamation!)
		RETURN -1

	END CHOOSE
		
RETURN ll_authorization_no


end function

public function integer nf_update ();INT	li_cntr = 1, li_bound

li_bound = UpperBound(idw_dw)
DO WHILE li_cntr <= li_bound
	IF li_cntr = 1 THEN 
		li_cntr ++
	ELSE
		idw_dw[li_cntr].Update()
		 inv_transobj.nf_handle_error("n_rehab_progress","nf_update()","idw_dw[li_cntr].Update()")
		// Error Handling
		li_cntr ++
	END IF 
LOOP

/* need to update this for telephone consult */
IF ib_create_authorization = TRUE THEN 
	IF ISVALID(ids_rehab_task_authorization_insert) AND ids_rehab_task_authorization_insert.rowcount() > 0 THEN 
		ids_rehab_task_authorization_insert.update()
		inv_transobj.nf_handle_error("n_rehab_progress","nf_update()","ids_rehab_task_authorization_insert.update()")
	END IF 
END IF 

RETURN 1
end function

on n_rehab_progress.create
call super::create
end on

on n_rehab_progress.destroy
call super::destroy
end on

