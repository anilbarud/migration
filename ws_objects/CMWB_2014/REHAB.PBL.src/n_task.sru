$PBExportHeader$n_task.sru
$PBExportComments$object with all the business rules for updating tasks
forward
global type n_task from n_pdc
end type
end forward

global type n_task from n_pdc
end type
global n_task n_task

type variables
LONG  			il_claim_no
DECIMAL 		idec_qty
DATE 			idt_current_date, idt_ephysio_implentation_date, idt_ephysio_br_check_date
BOOLEAN 		ib_authorization, ib_new_task
BOOLEAN		ib_canceled_related_action_items 		= False
BOOLEAN		ib_rescheduled_related_action_items 	= False

n_action_item		inv_action_item
u_ds						ids_rehab_referral_insert 					

end variables

forward prototypes
public function integer nf_set_defaults ()
public function integer nf_set_unused_fields ()
public function long nf_get_next_identifier ()
public function long nf_set_identifiers ()
public function integer nf_retrieve (long al_claim_no, long al_task_no)
public function integer nf_set_task_specific_filter (string as_task_type_code, string as_task_sub_type_code)
public function integer nf_insert (long al_row)
public function boolean nf_check_accident_date (datetime adt_date)
public function long nf_get_next_authorization_no ()
public function integer nf_provider_address (string as_recipient_type_code, long al_recipient_no, long al_dw)
public function integer nf_display_task_success ()
public function integer nf_change_item (long al_datawindow, long al_row, string as_dw_column, string as_data)
public subroutine nf_set_new_task (boolean ab_new_task)
public function integer nf_set_task_sub_type_filter (string as_task_type_code)
public function integer nf_update ()
public function integer nf_cancel_related_follow_ups ()
public function integer nf_set_claim_no (long al_claim_no)
public function integer nf_check_bus_rule ()
public function integer nf_check_mandatory ()
public function integer nf_insert_rehab_task_authorization ()
public function integer nf_insert_rehab_referral ()
public function long nf_get_next_rehab_referral_no ()
public function integer nf_set_task_sub_specific_filter (string as_service_code, string as_program_code)
protected function integer nf_update_provider_on_authorization (long al_task_no, long al_claim_no, long al_provider_no, string as_provider_type_code, long al_old_provider_no, string as_old_provider_type)
public function boolean nf_check_if_report_type_exists (long al_claim_no, long al_task_no, string as_report_status_code)
public function boolean nf_check_client_discharged (long al_claim_no, long al_task_no)
public function date nf_get_treatment_discharge_date (long al_claim_no, long al_task_no)
public function boolean nf_client_appropriate_for_program (long al_claim_no, long al_task_no)
public function date nf_get_initial_assessment_date (long al_claim_no, long al_task_no)
public function boolean nf_provider_approved_for_program (long al_provider_no, string as_provider_type, string as_program_code)
public function string nf_get_program_description (string as_program_code)
public function string nf_get_physio_contract_flag (long al_provider_no, string as_provider_type)
public function string nf_get_ephysio_flag (long al_no, string as_type)
end prototypes

public function integer nf_set_defaults ();LONG 						ll_row
DataWindowChild		ldwc_responsible_user

ll_row = idw_dw[1].GetRow()
IF ll_row > 0 THEN
/* set the default values for the individual insert */
	idw_dw[1].GetChild('responsible_user_id',ldwc_responsible_user)
	IF ldwc_responsible_user.Find("user_id = '" + vgst_user_profile.user_id + "'",1,ldwc_responsible_user.RowCount()) > 0 THEN
		idw_dw[1].SetItem(ll_row,'responsible_user_id', vgst_user_profile.user_id)
	END IF
	
   idw_dw[1].SetItem(ll_row,'planned_start_date', Date(f_server_datetime()))
	idw_dw[1].SetItem(ll_row,'task_success_code','X')
	idw_dw[1].SetItem(ll_row,'task_status_code','01')
	idw_dw[1].SetItem(ll_row,'task_specific_code','.')
	idw_dw[1].SetItem(ll_row,'task_sub_type_code','.')
	idw_dw[1].SetItem(ll_row,'comment','')
	idw_dw[1].SetItem(ll_row,'auto_created_flag','N')
	idw_dw[1].SetItem(ll_row,'expedited_service_flag','N')	
	
END IF

RETURN 0

end function

public function integer nf_set_unused_fields ();LONG  ll_row

/* determine which fields are null and fill them in with a default value
	at some point in time any of the following fields may be unused */

ll_row = idw_dw[1].GetRow()
IF ll_row > 0 THEN
   IF IsNull(idw_dw[1].GetItemString(ll_row, 'task_status_code')) OR idw_dw[1].GetItemString(ll_row, 'task_status_code') = '' THEN
      idw_dw[1].SetItem(ll_row, 'task_status_code', ' ')
   END IF
	
	IF IsNull(idw_dw[1].GetItemString(ll_row, 'comment')) OR TRIM(idw_dw[1].GetItemString(ll_row, 'comment')) = '' THEN
		idw_dw[1].SetItem(ll_row, 'comment','')
	END IF
END IF

RETURN 0
end function

public function long nf_get_next_identifier ();LONG  ll_task_no, ll_row

/* get the next task number */
ll_row = idw_dw[1].GetRow()
IF ll_row > 0 THEN
   IF idw_dw[1].GetItemNumber(ll_row,'task_no') = 0 OR  IsNull(idw_dw[1].GetItemNumber(ll_row,'task_no')) THEN 
		
		/*	get the next task number */
		SELECT 	Max(task_no)
		INTO 		:ll_task_no
		FROM 		REHAB_TASK
		WHERE 	claim_no = :il_claim_no
		USING 		SQLCA;
		 
		SQLCA.nf_handle_error('Embedded SQL: select from Rehab_Task','n_task','nf_get_next_identifier') 
		
		IF IsNull(ll_task_no) THEN
			ll_task_no = 0
		END IF
		
		ll_task_no = ll_task_no + 1
		
	END IF

ELSE
   RETURN 0
END IF

RETURN ll_task_no

end function

public function long nf_set_identifiers ();LONG  		ll_task_no, ll_row,  ll_rehab_referral_no, ll_claim_no, ll_provider_no, ll_provider_no_original
STRING		ls_provider_type, ls_provider_type_original

ll_row = idw_dw[1].GetRow()
IF ll_row > 0 THEN
	IF idw_dw[1].GetItemNumber(1,'task_no') = 0 OR IsNull(idw_dw[1].GetItemNumber(1,'task_no')) THEN

		idw_dw[1].SetItem(1,'claim_no',il_claim_no)
		ll_task_no = nf_get_next_identifier()
		IF ll_task_no > 0 THEN
			idw_dw[1].SetItem(1,'task_no',ll_task_no)
		ELSE
			SignalError(-666,'Error Creating new Task. Unable to determine next task number')
		END IF
   END IF
ELSE
	RETURN -1
END IF

IF ib_new_task = TRUE THEN 
	IF idw_dw[2].RowCount() > 0 THEN
	
		/*	For a Rehab Referral set up for a Task:
			Create a Rehab Referral record with the following values:
	
			Task number						REHAB_TASK.task_no for the task
			Claim number						REHAB_TASK.claim_no for the task
			Rehab referral number		system generated
			Rehab service code			REHAB_TASK.rehab_service_code
			Referring Provider Type		value from the screen
			Referring Provider Number	value from the screen
			Document identifier			0 (zero)
		*/
	
		//I don't like the way this is done - last rehab referral has a lock on it
		ll_rehab_referral_no = nf_get_next_rehab_referral_no()
		IF ll_rehab_referral_no <= 0 OR ISNULL(ll_rehab_referral_no) THEN 
			SignalError(-666,'Error Creating Rehab Referral number. Unable to determine the next Rehab Referral number')
		END IF 
		
		idw_dw[2].SetItem(1,'rehab_referral_no', ll_rehab_referral_no)
		idw_dw[2].SetItem(1,'claim_no', idw_dw[1].GetItemNumber(1,'claim_no'))
		idw_dw[2].SetItem(1,'task_no', idw_dw[1].GetItemNumber(1,'task_no'))
		idw_dw[2].SetItem(1,'rehab_service_code', idw_dw[1].GetItemstring(1,'rehab_service_code'))
		idw_dw[2].SetItem(1,'doc_id', 0)
	END IF

/* ADD THE REHAB_TASK_AUTHORIZATIONS RECORDS  - If Applicable. 
	At this point the task has a task no so we need to add the auto authorizations here
*/
	IF idw_dw[1].getitemstatus(1,0,primary!) = newmodified!  THEN 
		IF nf_insert_rehab_task_authorization() = -1 THEN RETURN -1
	END IF 
	
ELSE
	//check if the user has added an external rehab referral no
	IF 	idw_dw[2].getitemstatus(1,0,primary!) = newmodified!  THEN 
		//I don't like the way this is done - last rehab referral has a lock on it
		ll_rehab_referral_no = nf_get_next_rehab_referral_no()
		IF ll_rehab_referral_no <= 0 OR ISNULL(ll_rehab_referral_no) THEN 
			SignalError(-666,'Error Creating Rehab Referral number. Unable to determine the next Rehab Referral number')
		END IF 
		
		idw_dw[2].SetItem(1,'rehab_referral_no', ll_rehab_referral_no)
		idw_dw[2].SetItem(1,'claim_no', idw_dw[1].GetItemNumber(1,'claim_no'))
		idw_dw[2].SetItem(1,'task_no', idw_dw[1].GetItemNumber(1,'task_no'))
		idw_dw[2].SetItem(1,'rehab_service_code', idw_dw[1].GetItemstring(1,'rehab_service_code'))
		idw_dw[2].SetItem(1,'doc_id', 0)
	END IF 
END IF 

/* at this point we need to update related rehab_task_authorizations  we have to do it here because message
    boxes are still called up to this point which would cause a lock on the rehab_task_authorization table 
	 
	NOTE: There is a spreadsheet that contains all of the cases for updates - also noted in nf_update_provider_on_authorization()
*/

//grab the information that we need
ll_claim_no  			= idw_dw[1].GetItemNumber(1,'claim_no') 
ll_provider_no 		= idw_dw[1].GetItemNumber(1,'provider_no') 
ls_provider_type 	= idw_dw[1].GetItemstring(1,'provider_type_code') 
ll_task_no 				= idw_dw[1].GetItemNumber(1,'task_no') 

//original values
ll_provider_no_original 		= idw_dw[1].GetItemNumber(1,'provider_no', primary!, true) 
ls_provider_type_original 	= idw_dw[1].GetItemstring(1,'provider_type_code', primary!, true) 

IF ll_claim_no > 0 AND ll_task_no > 0 THEN//which it will always be
	
	IF ls_provider_type <> ls_provider_type_original OR ll_provider_no <> ll_provider_no_original THEN 
		
			nf_update_provider_on_authorization(ll_task_no, ll_claim_no, ll_provider_no, ls_provider_type,ll_provider_no_original ,ls_provider_type_original ) 
	END IF 
END IF 

RETURN ll_task_no
end function

public function integer nf_retrieve (long al_claim_no, long al_task_no);LONG  							ll_rows, ll_no, ll_rowcount
STRING							ls_string, ls_sub_type_code, ls_type_code, ls_program_code_orig, ls_service_code_orig, ls_display_text
STRING							ls_task_type_orig, ls_task_sub_type_orig, ls_task_specific_code_orig
DATAWINDOWCHILD		ldwc_child

idw_dw[3].Reset()				/* don't retrieve authorizations - only default on insert */

/* Retrieve TASK section */
ll_rows = idw_dw[1].Retrieve(al_claim_no, al_task_no)
SQLCA.nf_handle_error('Retrieve of dw','n_task','nf_retrieve') 

IF ll_rows < 1 THEN
	MessageBox('Retrieval Error','Unable to retrieve requested task.')
	RETURN -1
END IF

/* set up the display dropdown */
ls_program_code_orig 		= idw_dw[1].getitemstring(1,'rehab_program_code')
ls_service_code_orig			= idw_dw[1].getitemstring(1,'rehab_service_code')
ls_task_type_orig				= idw_dw[1].getitemstring(1,'task_type_code')
ls_task_sub_type_orig		= idw_dw[1].getitemstring(1,'task_sub_type_code')
ls_task_specific_code_orig	= idw_dw[1].getitemstring(1,'task_specific_code')

//program information
idw_dw[1].GetChild('rehab_service_program_display',ldwc_child)
ll_rowcount = ldwc_child.RowCount()
				
ll_no = ldwc_child.Find('rehab_service_code = "' + ls_service_code_orig + '" and rehab_program_code = "' + ls_program_code_orig + '"', 1,ldwc_child.RowCount())
IF ISNULL(ll_no) OR ll_no <= 0  THEN 
	//NEED TO DO SOMETHING HERE
END IF 

ls_display_text = ldwc_child.getitemstring(ll_no,'rehab_service_program_computed' )

idw_dw[1].setitem(1,'rehab_service_program_display', ls_display_text)

//retrieve only the related ones
nf_set_task_sub_specific_filter(ls_service_code_orig, ls_program_code_orig)

/* now retrieve the task information */
idw_dw[1].GetChild('task_xref_display',ldwc_child)
ll_rowcount = ldwc_child.RowCount()
		
ll_no = ldwc_child.Find( 'task_type_code  = "' + ls_task_type_orig + '" and task_sub_type_code = "' + ls_task_sub_type_orig + '" and task_specific_code = "'  + ls_task_specific_code_orig + '"'	, 1,ldwc_child.RowCount())
IF ISNULL(ll_no) OR ll_no <= 0  THEN 
	//NEED TO DO SOMETHING HERE
END IF 

ls_display_text = ldwc_child.getitemstring(ll_no,'rehab_task_sub_specific_computed' )

idw_dw[1].setitem(1,'task_xref_display', ls_display_text)

/* Determine whether to display the Task Success Code for the selected task */
nf_display_task_success()

/* Retrieve EXTERNAL REQUESTOR section */
ll_rows = idw_dw[2].Retrieve(al_claim_no , al_task_no)
SQLCA.nf_handle_error('Retrieve of dw','n_task','nf_retrieve()') 

IF ll_rows = 0 THEN
	idw_dw[2].InsertRow(0)
ELSE
	ll_no 			= idw_dw[2].GetItemNumber(1,'referring_provider_no')
	ls_string 	= idw_dw[2].GetItemString(1,'referring_provider_type_code')
	
	IF ll_no > 0 THEN
		nf_provider_address(ls_string, ll_no,2)
	END IF
END IF

ls_type_code 			= idw_dw[1].GetItemString(1,'task_type_code')
ls_sub_type_code 		= idw_dw[1].GetItemString(1,'task_sub_type_code')
nf_set_task_sub_type_filter(ls_type_code)
nf_set_task_specific_filter(ls_type_code, ls_sub_type_code)

/* Retrieve AUTHORIZATIONS section */
ll_no 			= idw_dw[1].GetItemNumber(1,'provider_no')
ls_string 	= idw_dw[1].GetItemString(1,'provider_type_code')

IF ll_no > 0 THEN
	nf_provider_address(ls_string, ll_no,1)
END IF

RETURN ll_rows

end function

public function integer nf_set_task_specific_filter (string as_task_type_code, string as_task_sub_type_code);DATAWINDOWCHILD	ldwc_child
LONG					ll_row
STRING					ls_filter
INTEGER				li_result


ll_row = idw_dw[1].GetChild('task_specific_code', ldwc_child)
	
IF ll_row > 0 THEN
	IF ib_new_task = TRUE THEN
		ls_filter = "task_type_code = '" + as_task_type_code + "' AND task_sub_type_code = '" + as_task_sub_type_code +&
						"' AND active_flag = 'Y'"
	ELSE
		ls_filter = "task_type_code = '" + as_task_type_code + "' AND task_sub_type_code = '" +&
						as_task_sub_type_code + "'"
	END IF
ELSE
	RETURN 1
END IF

li_result = ldwc_child.SetFilter(ls_filter)
li_result = ldwc_child.SetSort("task_specific_desc A")
li_result = ldwc_child.Filter()
li_result = ldwc_child.Sort()

RETURN 0

end function

public function integer nf_insert (long al_row);
idw_dw[1].Reset()
idw_dw[2].Reset()
idw_dw[3].Reset()
	
IF idw_dw[1].InsertRow(al_row)  < 0 THEN RETURN -1		/* task */
IF nf_set_defaults() < 0 THEN RETURN -1

nf_set_task_sub_type_filter('')
nf_set_task_specific_filter('','')
	
IF idw_dw[2].InsertRow(0) < 0 THEN RETURN -1				/* external requestors */
		
RETURN 0



end function

public function boolean nf_check_accident_date (datetime adt_date);DATETIME		ldtm_date

SELECT 	accident_date
INTO 		:ldtm_date
FROM 		CLAIM
WHERE 	claim_no = :il_claim_no
USING 		SQLCA;
SQLCA.nf_handle_error('Embedded SQL: select from CLAIM','n_task','nf_check_accident_date()') 

/* the date passed in comes before the accident date - the check fails*/
IF Date(ldtm_date) > Date(adt_date) THEN RETURN FALSE
	
RETURN TRUE
end function

public function long nf_get_next_authorization_no ();LONG	ll_authorization_no

UPDATE Last_Authorization_No SET last_authorization_no = last_authorization_no + 1 USING SQLCA;
SQLCA.nf_handle_error("Embedded SQL: Update Last_Authorization_No","n_tasks","nf_get_next_authorization_no")

CHOOSE CASE SQLCA.SQLNRows
	/*	If update was successful (ie. SQLNRows would equal 1), read back the identifier */	
	CASE 1
	
		SELECT Last_Authorization_No.last_authorization_no INTO :ll_authorization_no FROM Last_Authorization_No USING SQLCA;
		 SQLCA.nf_handle_error("Embedded SQL: Update Last_Authorization_No","n_tasks","nf_get_next_authorization_no")
			
	CASE ELSE
		/*		if anything other than 1 record found, display error*/
		SQLCA.nf_rollback_transaction()
		IF SQLCA.SQLCode <> 0 THEN
			Error.Text 				= "Error during rollback of Last_authorization_No in function wf_get_next_authorization_no"
			Error.WindowMenu	= "w_authorization"
			Error.Object				= ""
			Error.ObjectEvent		= "uf_next_authorization_no"
			SignalError()
		END IF		
		MessageBox("authorization Module - Data Integrity Error", string(SQLCA.SQLNRows) + " record(s) found in Last_authorization_No~r~nPlease call the help desk",Exclamation!)
		RETURN -1

	END CHOOSE
		
RETURN ll_authorization_no


end function

public function integer nf_provider_address (string as_recipient_type_code, long al_recipient_no, long al_dw);STRING		ls_address_line1, ls_address_line2, ls_city, ls_country, ls_province, ls_postal_code, ls_name, ls_telephone_no
			
IF IsNull(al_recipient_no) THEN
	MessageBox('Warning','The Service provider number is missing')
	RETURN -1
END IF

IF IsNull(as_recipient_type_code) THEN
	MessageBox('Warning','The Service provider Type is missing')
	RETURN -1
END IF

SELECT name, 		address_line1, 		address_line2,   		city, 		prov_state_code, 	country_code, 	postal_code, telephone_no
INTO 	:ls_name, :ls_address_line1, 	:ls_address_line2, 	:ls_city,  :ls_province, 		:ls_country, 		:ls_postal_code, :ls_telephone_no  
FROM PROVIDER  
WHERE provider_no 				= :al_recipient_no  
AND   	provider_type_code 	= :as_recipient_type_code 
//AND		active_flag 				= 'Y'  
USING SQLCA ;

IF SQLCA.nf_handle_error("Embedded SQL: Retrieve on PROVIDER","N_TASK","nf_provider_address()") = 100 THEN 
	idw_dw[al_dw].SetItem(1,"name",'')
	idw_dw[al_dw].SetItem(1,"address_line1",'')
	idw_dw[al_dw].SetItem(1,"address_line2",'')
	idw_dw[al_dw].SetItem(1,"city",'')
	idw_dw[al_dw].SetItem(1,"prov_state_code",'')
	idw_dw[al_dw].SetItem(1,"country",'')
	idw_dw[al_dw].SetItem(1,"postal_code",'')
	idw_dw[al_dw].SetItem(1,"telephone_no",'')
	RETURN 100
END IF 

idw_dw[al_dw].SetItem(1,"name",ls_name)
idw_dw[al_dw].SetItem(1,"address_line1",ls_address_line1)
idw_dw[al_dw].SetItem(1,"address_line2",ls_address_line2)
idw_dw[al_dw].SetItem(1,"city",ls_city)
idw_dw[al_dw].SetItem(1,"prov_state_code",ls_province)
idw_dw[al_dw].SetItem(1,"country",ls_country)
idw_dw[al_dw].SetItem(1,"postal_code",ls_postal_code)
idw_dw[al_dw].SetItem(1,"telephone_no",string(ls_telephone_no,"(@@@)-@@@-@@@@"))

RETURN 0
end function

public function integer nf_display_task_success ();LONG	ll_row
STRING	ls_success_code

/* Determine whether or not to display the Task Success Code
	depending on the type of task selected. Currently, only Surgery & Treatment 
	tasks can modify this code (those tasks with a 'success code required' = Yes) */

ll_row = idw_dw[1].GetRow()
IF ll_row > 0 THEN
	ls_success_code = idw_dw[1].GetItemString(ll_row, 'task_success_code')
	IF idw_dw[1].GetItemString(ll_row,'success_code_required') = "Y" THEN
		idw_dw[1].Object.task_success_code.Visible = 1
		IF IsNull(ls_success_code) THEN
			idw_dw[1].SetItem(1, 'task_success_code', 'X') /* sets default value of 'Not Yet Determined' */
		END IF
	ELSE
		idw_dw[1].Object.task_success_code.Visible = 0
		IF IsNull(ls_success_code) THEN
			idw_dw[1].SetItem(1, 'task_success_code', 'I') /* sets default value of 'Inapplicable' */
		END IF
	END IF
END IF

RETURN 0

end function

public function integer nf_change_item (long al_datawindow, long al_row, string as_dw_column, string as_data);STRING			ls_string, ls_program_code, ls_program_code_orig, ls_service_code_orig, ls_service_code
STRING			ls_task_type_code, ls_success_code_required, ls_task_status_code, ls_task_specific_code
STRING			ls_task_type_code_orig, ls_task_sub_type_code_orig, ls_task_specific_code_orig, ls_task_sub_type_code
STRING			ls_check, ls_provider_type_code, ls_provider_sub_type_code, ls_provider_type_code_original, ls_display_text
LONG			ll_no, ll_count, ll_claim_no, ll_rowcount, ll_row, ll_task_no, ll_provider_no, ll_provider_no_origional
DATETIME		ldtm_date
DATE			ld_date, ld_null_date, ldt_current_date
DATAWINDOWCHILD	ldwc_child


ldt_current_date =  Date(f_server_datetime())

CHOOSE CASE al_datawindow
  	CASE 1//REHAB_TASK
     	CHOOSE CASE as_dw_column
			CASE 'rehab_service_program_display'
				
				ls_check = as_data
				ls_check = idw_dw[1].getitemstring(al_row,'rehab_service_program_display')
				
				ls_program_code_orig 	= idw_dw[1].getitemstring(al_row,'rehab_program_code')
				ls_service_code_orig		= idw_dw[1].getitemstring(al_row,'rehab_service_code')
				
				idw_dw[1].GetChild('rehab_service_program_display',ldwc_child)
				
				ll_row 					= ldwc_child.getrow()
				ls_program_code 	= ldwc_child.getitemstring(ll_row,'rehab_program_code')
				ls_service_code 		= ldwc_child.getitemstring(ll_row,'rehab_service_code')
				
				IF ISNULL(ls_program_code)  THEN ls_program_code = ''
				
				//don't set it if we don't need to 
				IF (ls_program_code <> ls_program_code_orig OR ISNULL(ls_program_code_orig)) THEN
					idw_dw[1].SetItem(1,'rehab_program_code',ls_program_code)
				END IF 
				
				//don't set it if we don't need to 
				IF (ls_service_code <> ls_service_code_orig OR ISNULL(ls_service_code_orig)) THEN
					idw_dw[1].SetItem(1,'rehab_service_code',ls_service_code)
				END IF 
		
				//retrieve only the related ones
				nf_set_task_sub_specific_filter(ls_service_code, ls_program_code)
				
				//reset the task stuff
				idw_dw[1].SetItem(1,'task_xref_display',"")
				
				//set the columns to nothing -- the users must select them
				idw_dw[1].SetItem(1,'task_type_code',"")
				idw_dw[1].SetItem(1,'task_sub_type_code',"")
				idw_dw[1].SetItem(1,'task_specific_code',"")
				
				IF ls_service_code = 'S022' THEN
	
					SELECT 	count(*)
					INTO	 		:ll_count
					FROM 		PROVIDER
					WHERE 	provider_no 				= :ll_provider_no
					AND 			provider_type_code 	= :ls_provider_type_code
					USING 		SQLCA;
					SQLCA.nf_handle_error("n_task","nf_change_item()","SELECT count(*) FROM PROVIDER.(2)") 
					
					
					/*   3.500	The service provider (type & number) should be entered if the task is for a Physiotherapy Service (i.e. REHAB_TASK.rehab_service_code = ‘S022’).
					*/
					IF ll_count < 1 OR ISNULL(ll_count)  THEN
					//	MessageBox('Invalid Provider', "The service provider (type & number) should be entered if the task is for a Physiotherapy Service (i.e. REHAB_TASK.rehab_service_code = ‘S022’)." , Exclamation!)
					END IF
					
					SELECT 	provider_sub_type_code
					INTO	 		:ls_provider_sub_type_code
	               FROM 		PROVIDER
	               WHERE 	provider_no 				= :ll_provider_no
	               AND 			provider_type_code 	= :ls_provider_type_code
	               USING 		SQLCA;
	               SQLCA.nf_handle_error("n_task","nf_change_item()","SELECT count(*) FROM PROVIDER.(3)") 
	
					/* 3.510 The service provider (type & number) should be a Physiotherapy Clinic  or a Hospital if the Rehab Task is for a Physiotherapy Service 
									 (i.e provider_type_code ‘M’ and provider_sub_type_code ‘06’ or ‘35’ for REHAB_TASK.rehab_service_code = ‘S022’). */
					
					IF ls_provider_sub_type_code <> '35' AND ls_provider_sub_type_code <> '06'THEN
					//	MessageBox('Invalid Provider', "The service provider (type & number) should be a Physiotherapy Clinic  or a Hospital if the Rehab Task is for a Physiotherapy Service " , Exclamation!)
					END IF 	
				END IF 		
				
				// if there is only one value in the subtype dropdown based on the user selection -- select it for them!
				idw_dw[1].GetChild('task_xref_display',ldwc_child)
				IF ldwc_child.ROWCOUNT() = 1 THEN 
					
					ls_display_text = ldwc_child.getitemstring(1,'rehab_task_sub_specific_computed' )
					idw_dw[1].setitem(1,'task_xref_display', ls_display_text)	
					
					//fake out the itemchanged event
					nf_change_item(1,1,'task_xref_display',ls_display_text)
				END IF 
				
			CASE 'task_xref_display'
				
				ls_task_type_code_orig 			= idw_dw[1].getitemstring(al_row,'task_type_code')
				ls_task_sub_type_code_orig	= idw_dw[1].getitemstring(al_row,'task_sub_type_code')
				ls_task_specific_code_orig		= idw_dw[1].getitemstring(al_row,'task_specific_code')
				
				IF ISNULL(ls_task_type_code_orig) 		THEN ls_task_type_code_orig 		= ''
				IF ISNULL(ls_task_sub_type_code_orig) 	THEN ls_task_sub_type_code_orig 	= ''
				IF ISNULL(ls_task_specific_code_orig) 	THEN ls_task_specific_code_orig 	= ''
				
				idw_dw[1].GetChild('task_xref_display',ldwc_child)
				ll_rowcount 	= ldwc_child.RowCount()
				ll_no 				= ldwc_child.getrow()
				
				//grab the values from the datawindow child
				ls_task_type_code 		=	ldwc_child.getitemstring(ll_no,'task_type_code')
				ls_task_sub_type_code	= 	ldwc_child.getitemstring(ll_no,'task_sub_type_code')
				ls_task_specific_code		=	ldwc_child.getitemstring(ll_no,'task_specific_code')
				
				IF ls_task_type_code <> ls_task_type_code_orig THEN
					
					// code from previous column 'task_type_code'
					SELECT 	success_code_required
					INTO 		:ls_success_code_required
					FROM 		Task_Type
					WHERE 	task_type_code = :ls_task_type_code
					USING 		SQLCA;
					SQLCA.nf_handle_error("n_task","nf_change_item()","SELECT success_code_required") 
				
					IF ls_success_code_required = 'Y' THEN
						idw_dw[1].Object.task_success_code.Visible = 1
						idw_dw[1].SetItem(1, 'task_success_code', 'X') /* ensures set to default value of 'Not Yet Determined'*/
					ELSE															  /* if success code is required */
						idw_dw[1].Object.task_success_code.Visible = 0
						idw_dw[1].SetItem(1, 'task_success_code', 'I') /*set to 'Inapplicable' if success code not req'd.*/
					END IF
					
					idw_dw[1].SetItem(1,'task_type_code',ls_task_type_code)
					
				END IF 
				
				IF ls_task_sub_type_code <> ls_task_sub_type_code_orig THEN
					idw_dw[1].SetItem(1,'task_sub_type_code',ls_task_sub_type_code)
				END IF 
				
				IF ls_task_specific_code <> ls_task_specific_code_orig THEN
					idw_dw[1].SetItem(1,'task_specific_code',ls_task_specific_code)
				END IF 
				
				/* if communication and a new record then do the following. As per meeting
						planned_start_date 			= todays date
						planned_completion date 	= todays date
						actual start date 				= todays date
						task_status 						= in progress
				*/
				IF ls_task_type_code = 'CO' AND  idw_dw[1].getitemstatus(1,0, primary!) = NEWMODIFIED!  THEN 
					idw_dw[1].SetItem(1,"actual_start_date", ldt_current_date)
					idw_dw[1].SetItem(1,"planned_start_date", ldt_current_date)
					idw_dw[1].SetItem(1,"planned_completion_date", ldt_current_date)
					idw_dw[1].SetItem(1, "task_status_code", '02')	
				END IF 
					
			CASE  'task_type_code'
				//task_type_code is now controlled by task_xref_display (above) 	
				
			CASE  'task_sub_type_code'
				//task_sub_type_code is now controlled by task_xref_display (above) 

			CASE 'task_specific_code'
				//task specific is now controlled by task_xref_display (above) 
					
			CASE 'provider_no'
				
				ls_string 	= idw_dw[1].GetItemString(1, 'provider_type_code')
				ll_no 			= nf_provider_address(ls_string, Long(as_data), 1)

				/* 3.660	The service provider (type and number) should not be modified to another provider if there are any rehab authorizations 
								 under that task that are authorized to the same provider as the task.	Warn the user if there are any authorizations under that task 
								 that have the same provider as the provider that was originally assigned to the task so that they can change the authorizations, if needed.
						MODIFIED MOVRD TO BR (MUST)
					3.660	The service provider (type and number) MUST not be modified to a different service provider if there are any rehab authorizations 
							     under that task that are authorized to the same provider as the task and the authorized billable_item has been partially or fully paid  
						        ( the paid amount is greater than zero )	
					
					REVISED: 20120125
					3.660	The service provider (type and number) on an existing task must not be modified to a different service provider if there is a rehab authorization
								under that task that is authorized to the same provider as the task and the rehab authorization has been partially or fully paid 			  
					
				*/

			CASE 'provider_type_code'
				
				ll_claim_no 								= idw_dw[1].GetItemNumber(1,'claim_no')
				ll_task_no									= idw_dw[1].GetItemNumber(1,'task_no')
				ll_provider_no_origional 				= idw_dw[1].GetItemnumber(1, 'provider_no', PRIMARY!,TRUE)
				ls_provider_type_code_original 	= idw_dw[1].GetItemString(1, 'provider_type_code', PRIMARY!,TRUE)
				
				idw_dw[1].SetItem(1,'provider_no',0)
				idw_dw[1].SetItem(1,'address_line1','')
				idw_dw[1].SetItem(1,'address_line2','')
				idw_dw[1].SetItem(1,'name','')
				idw_dw[1].SetItem(1,'city','')
				idw_dw[1].SetItem(1,'prov_state_code','')
				idw_dw[1].SetItem(1,'postal_code','')
				idw_dw[1].SetItem(1,'country','')
				idw_dw[1].SetItem(1,"telephone_no",'')
				
					
			CASE 'task_status_code'
			/* If status becomes 'Completed' or 'Cancelled', default the actual completion date and
				the Actual Start Date to blank
				When I cancel a task, the application is defaulting the Actual Start & End Dates to the current date but the actual dates cannot be entered when the task is cancelled.
				It should not default the dates to the current date when cancelling a task.
				
				task_status_code task_status_desc
				---------------- ----------------------------------------
				01               planned
				02               in progress
				03               cancelled
				04               closed
				05               reset
			*/
				IF as_data = '03' OR as_data = '04' THEN
					
					/* default the actual closed date to the current date if the service is NOT for Physio.  For Physio, we are making them enter the date.
					 For all others, the date will default to the current date. */
					ls_service_code 		= idw_dw[1].getitemstring(idw_dw[1].getrow(),'rehab_service_code')
					
					setnull(ld_null_date)
									
					IF as_data = '04' AND ls_service_code <> 'S022'  THEN
						IF IsNull(idw_dw[1].GetItemDateTime(1,"actual_completion_date")) THEN
							idw_dw[1].SetItem(1,"actual_completion_date", Date(f_server_datetime()))
						END IF
						
						IF IsNull(idw_dw[1].GetItemDateTime(1,"actual_start_date")) THEN
							idw_dw[1].SetItem(1,"actual_start_date", ldt_current_date)
						END IF
					
					END IF 
				ELSE
				/* If the status becomes 'In Progress', default the actual start date to the
					current date if the start date is not yet completed
				*/
					IF as_data = '02' THEN
						IF IsNull(idw_dw[1].GetItemDateTime(1,"actual_start_date")) THEN
							idw_dw[1].SetItem(1,"actual_start_date", ldt_current_date)
						END IF
					ELSE
					/* If Status becomes 'Planned', set Actual Start & Actual Completion dates to blank */
						SetNull(ldtm_date)
						idw_dw[1].SetItem(1,"actual_start_date", ldtm_date)
						idw_dw[1].SetItem(1,"actual_completion_date", ldtm_date)
					END IF
				END IF
				
				/* Validate TASK SUCCESS vs. TASK RESULT for Planned or In Progress tasks */
				ls_success_code_required 	= idw_dw[1].GetItemString(1,'success_code_required')
				ls_task_status_code 			= idw_dw[1].GetItemString(1,'task_status_code')

				/* If Task PLANNED or IN PROGRESS and Success Code is required, result must be NOT YET DETERMINED
			   (i.e. cannot be successful/not successful if task not yet finished) */
				IF (idw_dw[1].GetItemString(1,"task_success_code") <> 'X') AND ls_success_code_required = "Y" AND &
				(ls_task_status_code = '01' OR ls_task_status_code = '02') THEN
					MessageBox('Invalid Task Result',"A 'planned' or 'in progress' task must have a result of 'Not Yet Determined'.")
					RETURN -1
				END IF
						
			CASE 'planned_start_date'

				ld_date = Date(Left(as_data,10))
				IF ld_date < Date('1900-01-01') OR ld_date > Date('2079-06-06') THEN		// PR 1279
					MessageBox('Invalid Date', "The planned start date is invalid.  Please enter a date between 01/01/1900 and 06/06/2079.", Exclamation!)
					RETURN -1
				END IF

			CASE 'planned_completion_date'
				
				ld_date = Date(Left(as_data,10))
				IF ld_date < Date('1900-01-01') OR ld_date > Date('2079-06-06') THEN		// PR 1279
					MessageBox('Invalid Date', "The planned completion date is invalid.  Please enter a date between 01/01/1900 and 06/06/2079.", Exclamation!)
					RETURN -1
				END IF
				
				/* 3.145 	The planned completion date should not be greater than 5 years in the future.  */
				// planned_completion_date - Reasonable check would be 5 years PR12848 - to be added to test plan (should - just message the users)
				IF daysafter(date(f_server_datetime()),ld_date ) > (365 * 5 ) THEN 
					MessageBox("Validation Error","The planned completion date should not be greater than 5 years in the future.")
				END IF 

			CASE 'actual_start_date'

				ld_date = Date(Left(as_data,10))
				IF ld_date < Date('1900-01-01') OR ld_date > Date('2079-06-06') THEN		// PR 1279
					MessageBox('Invalid Date', "The actual start date is invalid.  Please enter a date between 01/01/1900 and 06/06/2079.", Exclamation!)
					RETURN -1
				END IF
												
			/* If actual start date entered and actual completion date is not entered, set task status to '02' ('In Progress')
			*/
				IF NOT isnull(as_data) AND &
				   isnull(idw_dw[1].GetItemDateTime(1, "actual_completion_date"))  THEN
					idw_dw[1].SetItem(1, "task_status_code", '02')
				END IF
				
			CASE 'actual_completion_date'

				ld_date = Date(Left(as_data,10))
				IF ld_date < Date('1900-01-01') OR ld_date > Date('2079-06-06') THEN		// PR 1279
					MessageBox('Invalid Date', "The actual completion date is invalid.  Please enter a date between 01/01/1900 and 06/06/2079.", Exclamation!)
					RETURN -1
				END IF

			/* Validate - actual completion date cannot be in the future
			*/
				IF Date(Left(as_data,10)) > Date(f_server_datetime()) THEN
					MessageBox('Invalid Actual Completion Date', "The Actual Completion Date cannot be in the future")
					RETURN -1
				END IF
								
			/* If actual completed date is entered then default Task Status to '04' ('Completed')
			*/
				IF NOT IsNull(as_data)  THEN
					idw_dw[1].SetItem(1, "task_status_code", '04')
				END IF	
						
		END CHOOSE
		
	CASE 2//REHAB_RERRAL
     	CHOOSE CASE as_dw_column
			CASE 'referring_provider_no'
				
				ls_string 	= idw_dw[2].GetItemString(1,'referring_provider_type_code')
				ll_no 			= nf_provider_address(ls_string,Long(as_data),2)
					
			CASE 'referring_provider_type_code'
				
				idw_dw[2].SetItem(1,'referring_provider_no',0)
				idw_dw[2].SetItem(1,'address_line1','')
				idw_dw[2].SetItem(1,'address_line2','')
				idw_dw[2].SetItem(1,'name','')
				idw_dw[2].SetItem(1,'city','')
				idw_dw[2].SetItem(1,'prov_state_code','')
				idw_dw[2].SetItem(1,'postal_code','')
				idw_dw[2].SetItem(1,'country','')

			CASE 'requested_date'  // Added for PR1279 - Validate date entered to ensure within database smalldatetime range

				ld_date = Date(Left(as_data,10))

				IF ld_date < Date('1900-01-01') OR ld_date > Date('2079-06-06') THEN
					MessageBox('Invalid Date', "The requested date is invalid.  Please enter a date between 01/01/1900 and 06/06/2079.", Exclamation!)				
					RETURN -1					
				END IF	// End of PR1279 new code
				
		END CHOOSE
		
END CHOOSE

RETURN 0
end function

public subroutine nf_set_new_task (boolean ab_new_task);
ib_new_task = ab_new_task
end subroutine

public function integer nf_set_task_sub_type_filter (string as_task_type_code);DATAWINDOWCHILD		ldwc_child
LONG							ll_row
STRING							ls_filter


ll_row = idw_dw[1].GetChild('task_sub_type_code', ldwc_child)
	
IF ll_row > 0 THEN
	IF ib_new_task = TRUE THEN
		ls_filter = "task_type_code = '" + as_task_type_code + " ' AND active_flag = 'Y'"
	ELSE
		ls_filter = "task_type_code = '" + as_task_type_code + " '"
	END IF
	ldwc_child.SetFilter(ls_filter)
	ldwc_child.Filter()
ELSE
	RETURN -1
END IF

RETURN 0

end function

public function integer nf_update ();/*************************************************************************/
INT	li_cntr = 1, li_bound

li_bound = UpperBound(idw_dw)
DO WHILE li_cntr <= li_bound
	idw_dw[li_cntr].Update()
    inv_transobj.nf_handle_error("n_task","nf_update()","	idw_dw[li_cntr].Update()")
	// Error Handling
	li_cntr ++
LOOP

// PR13352 - can't apply foreign key on rehab task authorization -- need to update in the opposite direction (if this doesn't work hardcode the update as done below )
//li_cntr = UpperBound(idw_dw)
//DO WHILE li_cntr > 0 
//	idw_dw[li_cntr].Update()
//    inv_transobj.nf_handle_error("n_task","nf_update()","	idw_dw[li_cntr].Update()")
//	li_cntr --
//LOOP 

If IsValid(inv_action_item) THen
	inv_action_item.nf_update()
End if

RETURN 0
end function

public function integer nf_cancel_related_follow_ups ();LONG			ll_task_no
INTEGER		li_rtn

ll_task_no = idw_dw[1].GetItemNumber(1,'task_no')

li_rtn = inv_action_item.nf_cancel_related_action_items(0,ll_task_no,False)

//return the number of follow-ups canceled
RETURN li_rtn
end function

public function integer nf_set_claim_no (long al_claim_no);
IF al_claim_no = 0 THEN SignalError(-666,'Claim number cannot be zero.')

il_claim_no = al_claim_no

inv_action_item.nf_set_claim_no(il_claim_no)
 
RETURN 0
end function

public function integer nf_check_bus_rule ();STRING			ls_task_status_code, ls_success_code_required, ls_expedited_service_flag, ls_provider_sub_type_code, ls_task_status_code_origional
STRING			ls_string2, ls_string, ls_task_success_code,  ls_provider_type_code, ls_provider_type_code_original, ls_referring_provider_type_code
STRING			ls_rehab_service_code, ls_rehab_program_code, ls_task_type_code, ls_task_sub_type_code, ls_task_specific_code, ls_program_description, ls_physio_contract_flag
STRING			ls_ephysio_flag
DATETIME 	ldtm_accident_date, ldtm_min_service_date, ldtm_referred_on_date, ldtm_max_service_date
DATE    		ld_start_date_test, ld_completion_date_test, ld_treatment_discharge_date, ld_initial_assessment_date
DATE			ld_planned_start_date,  ld_planned_completion_date, ld_actual_start_date, ld_actual_completion_date, ld_requested_date
LONG			ll_canceled_count, ll_task_no, ll_count, ll_no, ll_provider_no, ll_claim_no, ll_provider_no_origional, ll_referring_provider_no, ll_count_auth_check
INTEGER		li_days, li_rtn, li_message, li_check_count
BOOLEAN 		lb_client_appropriate_for_program
dwItemStatus 	l_status_row, l_rehab_service_code_status, l_rehab_program_code_status, l_task_type_code_status, l_task_sub_type_code_status, l_task_specific_code_status 
dwItemStatus	l_provider_no_status, l_provider_type_code_status, l_task_status_code_status


idw_dw[1].ACCEPTTEXT()

ls_rehab_service_code  					= idw_dw[1].GetItemString(1, 'rehab_service_code')
ls_rehab_program_code 				= idw_dw[1].GetItemString(1, 'rehab_program_code')
ls_task_type_code  						= idw_dw[1].GetItemString(1, 'task_type_code')
ls_task_sub_type_code     				= idw_dw[1].GetItemString(1, 'task_sub_type_code')
ls_task_specific_code 					= idw_dw[1].GetItemString(1, 'task_specific_code')
ls_provider_type_code					= idw_dw[1].GetItemString(1, 'provider_type_code')
ll_provider_no								= idw_dw[1].GetItemnumber(1, 'provider_no')
ll_claim_no   									= idw_dw[1].GetItemnumber(1, 'claim_no')
ls_expedited_service_flag				= idw_dw[1].GetItemString(1, 'expedited_service_flag')
ld_actual_start_date 						= Date(idw_dw[1].GetItemDateTime(1, 'actual_start_date'))
ld_actual_completion_date 			= Date(idw_dw[1].GetItemDateTime(1, 'actual_completion_date'))
ll_task_no										= idw_dw[1].GetItemnumber(1,'task_no')
ld_planned_start_date 					= Date(idw_dw[1].GetItemDateTime(1, 'planned_start_date'))
ld_planned_completion_date 			= Date(idw_dw[1].GetItemDateTime(1, 'planned_completion_date'))
ld_requested_date 						= Date(idw_dw[2].GetItemDateTime(1, 'referred_on_date'))
ls_task_status_code 						= idw_dw[1].GetItemString(1,'task_status_code')
ls_task_success_code       				= idw_dw[1].GetItemString(1,"task_success_code")

//original data
ll_provider_no_origional 					= idw_dw[1].GetItemnumber(1, 'provider_no', PRIMARY!,TRUE)
ls_provider_type_code_original 		= idw_dw[1].GetItemString(1, 'provider_type_code',  PRIMARY!,TRUE)
ls_task_status_code_origional        = idw_dw[1].GetItemString(1, 'task_status_code',  PRIMARY!,TRUE)

//rehab_referral
ll_referring_provider_no 					= idw_dw[2].GetItemNumber(1,'referring_provider_no')
ls_referring_provider_type_code 	= idw_dw[2].GetItemString(1,'referring_provider_type_code')
ldtm_referred_on_date					= idw_dw[2].GetItemDateTime(1, 'referred_on_date')

// grab the itemstatus for certain columns these are columns that may not have been modified  and are inactive
l_rehab_service_code_status 			=  	idw_dw[1].GetItemStatus(1,'rehab_service_code', Primary!)
l_rehab_program_code_status 		=  	idw_dw[1].GetItemStatus(1,'rehab_program_code', Primary!)
l_task_type_code_status 				=  	idw_dw[1].GetItemStatus(1,'task_type_code', Primary!)
l_task_sub_type_code_status 		= 	 	idw_dw[1].GetItemStatus(1,'task_sub_type_code', Primary!)
l_task_specific_code_status 			=  	idw_dw[1].GetItemStatus(1,'task_specific_code', Primary!)
l_status_row 									=		idw_dw[1].GetItemStatus(1,0, Primary!)
l_provider_no_status						=  	idw_dw[1].GetItemStatus(1,'provider_no', Primary!)
l_provider_type_code_status       	=  	idw_dw[1].GetItemStatus(1,'provider_type_code', Primary!)
l_task_status_code_status				=  	idw_dw[1].GetItemStatus(1,'task_status_code', Primary!)

/*
Start Date for a Task 
– planned start date for a task that is Planned 
– actual start date for a task that is In Progress or Closed  

Closed Date for a Task 
– planned completed date for a task that is Planned  
– actual completed date for a task that is In Progress or Closed 
*/

//do some required defaults for NULL's
IF ISNULL(ls_rehab_program_code) 			THEN ls_rehab_program_code 			= ''
IF ISNULL(ll_claim_no) 								THEN ll_claim_no 								= il_claim_no
IF ISNULL(ll_task_no) 								THEN ll_task_no 								= 0
IF ISNULL(ls_task_status_code_origional) 	THEN ls_task_status_code_origional 	= ''

/* 3.440	A rehab service must be entered. */
IF trim(ls_rehab_service_code) = '' OR ISNULL(ls_rehab_service_code) THEN
	MessageBox('Invalid Rehab Service', "A rehab service must be entered." , Exclamation!)
	idw_dw[1].SetFocus()
	idw_dw[1].SetColumn('rehab_service_code')
	RETURN -1
END IF 

IF l_rehab_service_code_status <> NOTMODIFIED!  THEN 
	/* 3.450	A rehab service must be an active service.	 */
	SELECT count(*)
	INTO 	:ll_count
	FROM 	Rehab_Service
	WHERE rehab_service_code 	= :ls_rehab_service_code
	AND 		active_flag 				= 'Y'
	USING 	SQLCA;		
	SQLCA.nf_handle_error("n_task","nf_check_bus_rule()","A rehab service must be an active service.") 
	
	IF ll_count <> 1 THEN
		MessageBox('Invalid Rehab Service', "A rehab service must be an active service." , Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('rehab_service_code')
		RETURN -1
	END IF 
END IF 

/* 3.460	[ I think the code below will cover this one (3.470)]A rehab program must be entered if the associated rehab service has a rehab program.	
              Remove the selected rehab program. The rehab service has associated rehab programs	 */
IF l_rehab_program_code_status <> NOTMODIFIED!  THEN 
	/* 3.470	A rehab program, if entered, must be an active program for the rehab service.*/
	SELECT count(*)
	INTO 	:ll_count
	FROM 	Rehab_Service_Program_Xref
	WHERE rehab_service_code 		= :ls_rehab_service_code
	AND     rehab_program_code 	= :ls_rehab_program_code
	AND 		active_flag 					= 'Y'
	USING 	SQLCA;				
	SQLCA.nf_handle_error("n_task","nf_check_bus_rule()","SELECT count(*) FROM 	Rehab_Service_Program_Xref.") 
			
	IF ll_count < 1 OR ISNULL(ll_count)  THEN
		MessageBox('Invalid Rehab Program', "A rehab program, if entered, must be an active program for the rehab service." , Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('rehab_program_code')
		RETURN -1
	END IF 	
END IF 

/* The type of task must be active for the rehab service and the rehab program 
     (i.e. Rehab_Service_Program_Task_Xref.active_flag = ‘Yes’ for task type/sub-type/specific type 
     and the rehab service code/rehab program code)

	 A task type must be an active task type for the rehab service and rehab program.
*/	
IF l_task_type_code_status <> NOTMODIFIED!  THEN 
	SELECT count(*)
	INTO 	:ll_count
	FROM 	Rehab_Service_Program_Task_Xref
	WHERE rehab_service_code 		= :ls_rehab_service_code
	AND     rehab_program_code 	= :ls_rehab_program_code
	AND     task_type_code            = :ls_task_type_code
	AND 		active_flag 					= 'Y'
	USING 	SQLCA;			
	SQLCA.nf_handle_error("n_task","nf_check_bus_rule()","SELECT count(*) FROM 	Rehab_Service_Program_Task_Xref.") 
		
	IF ll_count < 1 OR ISNULL(ll_count)  THEN
		MessageBox('Invalid Rehab Program', "A task type must be an active task type for the rehab service and rehab program." , Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('task_type_code')
		RETURN -1
	END IF 	
END IF 

/* 3.35	The type of task (task type/sub-type/specific type) must be active for the rehab service 
			and the rehab program (i.e. Rehab_Service_Program_Task_Xref.active_flag = ‘Yes’)	
	
	A task subtype must be an active task subtype for the rehab service, rehab program and task type. 
*/	
IF l_task_sub_type_code_status <> NOTMODIFIED!  THEN 
	SELECT count(*)
	INTO 	:ll_count
	FROM 	Rehab_Service_Program_Task_Xref
	WHERE rehab_service_code 		= :ls_rehab_service_code
	AND     rehab_program_code 	= :ls_rehab_program_code
	AND     task_type_code            = :ls_task_type_code
	AND     task_sub_type_code 		= :ls_task_sub_type_code
	AND 		active_flag 					= 'Y'
	USING 	SQLCA;				
	SQLCA.nf_handle_error("n_task","nf_check_bus_rule()","SELECT count(*) FROM 	Rehab_Service_Program_Task_Xref. (2)") 
		
	IF ll_count < 1 OR ISNULL(ll_count)  THEN
		MessageBox('Invalid Rehab Program', "A task subtype must be an active task subtype for the rehab service, rehab program and task type." , Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('task_sub_type_code')
		RETURN -1
	END IF 	
END IF 

/* 3.35	The type of task (task type/sub-type/specific type) must be active for the rehab service 
			and the rehab program (i.e. Rehab_Service_Program_Task_Xref.active_flag = ‘Yes’)	
	
	A task specific type must be an active task specific type for the rehab service, rehab program , task type and task subtype. 
*/	
IF l_task_specific_code_status <> NOTMODIFIED!  THEN 
	SELECT count(*)
	INTO 	:ll_count
	FROM 	Rehab_Service_Program_Task_Xref
	WHERE rehab_service_code 		= :ls_rehab_service_code
	AND     rehab_program_code 	= :ls_rehab_program_code
	AND     task_type_code            = :ls_task_type_code
	AND     task_sub_type_code 		= :ls_task_sub_type_code
	AND     task_specific_code        	= :ls_task_specific_code
	AND 		active_flag 					= 'Y'
	USING 	SQLCA;			
	SQLCA.nf_handle_error("n_task","nf_check_bus_rule()","SELECT count(*) FROM 	Rehab_Service_Program_Task_Xref. (3)") 
		
	IF ll_count < 1 OR ISNULL(ll_count)  THEN
		MessageBox('Invalid Rehab Program', "A task specific type must be an active task specific type for the rehab service, rehab program , task type and task subtype." , Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('task_specific_code')
		RETURN -1
	END IF 
END IF 

/* 3.40	The type of task (task type/sub-type/specific type) must be allowed for entry on the Task tab (i.e. Task_Specific.task_entry_flag = ‘yes’ ).*/
IF  l_task_specific_code_status <> NOTMODIFIED! AND l_task_sub_type_code_status <> NOTMODIFIED! AND  l_task_type_code_status <> NOTMODIFIED! THEN 
	SELECT count(*)
	INTO 	:ll_count
	FROM 	Task_Specific
	WHERE  task_type_code           = :ls_task_type_code
	AND     	task_sub_type_code 		= :ls_task_sub_type_code
	AND     task_specific_code        	= :ls_task_specific_code
	AND 		active_flag 					= 'Y'
	AND     task_entry_flag           	= 'Y'
	USING 	SQLCA;				
	SQLCA.nf_handle_error("n_task","nf_check_bus_rule()","SELECT count(*) FROM 	Rehab_Service_Program_Task_Xref. (xx)") 
		
	IF ll_count < 1 OR ISNULL(ll_count)  THEN
		MessageBox('Invalid Task', "The type of task (task type/sub-type/specific type) must be allowed for entry on the Task tab" , Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('task_type_code')
		RETURN -1
	END IF
END IF 

/* 3.480	The service provider type, if entered, must be an active provider type (from Provider_Type table) */
IF trim(ls_provider_type_code) > '' AND NOT ISNULL (ls_provider_type_code) AND l_provider_type_code_status <> NOTMODIFIED! THEN
	
	SELECT count(*)
	INTO 	:ll_count
	FROM 	Provider_Type
	WHERE provider_type_code 		= :ls_provider_type_code
	AND 		active_flag 					= 'Y'
	USING 	SQLCA;				
	SQLCA.nf_handle_error("n_task","nf_check_bus_rule()","SELECT count(*) FROM 	Rehab_Service_Program_Xref.") 
		
	IF ll_count < 1 OR ISNULL(ll_count)  THEN
		MessageBox('Invalid Provider Type', "The service provider type, if entered, must be an active provider type" , Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('provider_type_code')
		RETURN -1
	END IF 
END IF

/* 3.490	The service provider number, if entered, must be an active service provider (from PROVIDER table) */
IF ll_provider_no > 0 AND  l_provider_no_status <> NOTMODIFIED! THEN	
	
	IF trim(ls_provider_type_code) = '' OR isnull(ls_provider_type_code)  THEN 
		MessageBox('Invalid Provider type', "Please enter a valid Provider Type" , Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('provider_type_code')
		RETURN -1
	END IF 
			
	SELECT 	count(*)
	INTO	 		:ll_count
	FROM 		PROVIDER
	WHERE 	provider_no 				= :ll_provider_no
	AND 			provider_type_code 	= :ls_provider_type_code
	AND 			active_flag 				= 'Y'
	USING 		SQLCA;
	SQLCA.nf_handle_error("n_task","nf_check_bus_rule()","SELECT count(*) FROM 	PROVIDER.") 
			
	IF ll_count < 1 OR ISNULL(ll_count)  THEN
		MessageBox('Invalid Provider', "The service provider number, if entered, must be an active service provider" , Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('provider_no')
		RETURN -1
	END IF 
	
	IF ls_rehab_service_code = 'S022' THEN
	
		SELECT 	provider_sub_type_code
		INTO	 		:ls_provider_sub_type_code
		FROM 		PROVIDER
		WHERE 	provider_no 				= :ll_provider_no
		AND 			provider_type_code 	= :ls_provider_type_code
		USING 		SQLCA;
		SQLCA.nf_handle_error("n_task","nf_check_bus_rule()","SELECT provider_sub_type_code") 
				
		/* 3.510 The service provider (type & number) should be a Physiotherapy Clinic  or a Hospital if the Rehab Task is for a Physiotherapy Service 
					 (i.e provider_type_code ‘M’ and provider_sub_type_code ‘06’ or ‘35’ for REHAB_TASK.rehab_service_code = ‘S022’). */
		//REVISED
		/* 3.510	The service provider (type & number), if entered, should be a Physiotherapy Clinic or a Hospital if the Rehab Task is for a Physiotherapy Service
		            (i.e provider_type_code ‘M’ and provider_sub_type_code ‘06’ or ‘35’ for REHAB_TASK.rehab_service_code = ‘S022’).
		*/			 
		IF ( ls_provider_type_code <> 'M' OR  (ls_provider_sub_type_code <> '06' and ls_provider_sub_type_code <> '35')) THEN
			li_message = MessageBox('Invalid Provider', "The service provider (type & number) should be a Physiotherapy Clinic or a Hospital if the Rehab Task is for a Physiotherapy Service" +&
																	"~rContinue with Save?",Question!,OKCancel!, 2)
			IF li_message = 2 THEN RETURN -1 																												
		 END IF 	
	  END IF	  
END IF 

IF ls_rehab_service_code = 'S022' THEN
		
	SELECT 	count(*)
	INTO	 		:ll_count
	FROM 		PROVIDER
	WHERE 	provider_no 				= :ll_provider_no
	AND 			provider_type_code 	= :ls_provider_type_code
	USING 		SQLCA;
	SQLCA.nf_handle_error("n_task","nf_check_bus_rule()","SELECT count(*) FROM 	PROVIDER.(2)") 
		
	/*   3.500 The service provider (type & number) should be entered if the task is for a Physiotherapy Service (i.e. REHAB_TASK.rehab_service_code = ‘S022’). */
	IF ll_count < 1 OR ISNULL(ll_count)  THEN
		li_message = MessageBox('Service Provider', "The service provider (type & number) should be entered if the task is for a Physiotherapy Service." +&
														"~rContinue with Save?",Question!,OKCancel!, 2)
		IF li_message = 2 THEN RETURN -1 
	
	END IF 
		  
	 /*  3.515	The service provider (type & number), if entered, for a Physiotherapy Service task must be approved as a provider of that physiotherapy program 
                                                                 – for all physiotherapy programs excluding  the Pain & Activity Questionnaire program 

			 (if REHAB_TASK.rehab_service_code = ‘S022’ and the REHAB_TASK.rehab_program_code not = ‘P004’,
			 then the provider must exist in  PROVIDER_XREF_REHAB_PROGRAM.rehab_program_code for the REHAB_TASK.rehab_program_code )
			 
			 Can you change the wording of this message from:
			---------------------------
			The service provider (type & number) for a Physiotherapy Service task must be approved as a provider for the physiotherapy program
			---------------------------
			To:
			This provider is not an approved physiotherapy clinic for the ‘rehab_program_description’	 
			
			REVISION: 2013-04-03
			
		3.515   The service provider (type & number) for a Physiotherapy Service task must be approved as a provider of that physiotherapy program 
		– for all physiotherapy programs excluding  the Pain & Activity Questionnaire program – if that provider is under a physio contract with WorkSafeNB 
		(if REHAB_TASK.rehab_service_code = ‘S022’ and the REHAB_TASK.rehab_program_code not = ‘P004’, and PROVIDER.physio_contract_flag =’Y’,  
		then the provider must exist in  Rehab_Program_Xref_Clinic.rehab_program_code for the REHAB_TASK.rehab_program_code )
				The rule should only be enforced if the provider is also under the Physio contract.  Currently, it enforces the rule even if the clinic is NOT under contract.
		
	*/
	ls_physio_contract_flag = nf_get_physio_contract_flag(ll_provider_no, ls_provider_type_code)
	
	 IF  ls_physio_contract_flag = 'Y' AND ll_provider_no > 0 AND trim(ls_rehab_program_code) > '' AND  ( l_provider_no_status <> NOTMODIFIED! OR l_provider_type_code_status <> NOTMODIFIED!  ) AND ls_rehab_program_code <> 'P004' AND nf_provider_approved_for_program(ll_provider_no , ls_provider_type_code , ls_rehab_program_code) = FALSE THEN		
		ls_program_description = nf_get_program_description(ls_rehab_program_code)
		MessageBox('Rehab Task Error', "This provider is not an approved physiotherapy clinic for the " + ls_program_description , Exclamation!)
		RETURN -1
	END IF 
	
	/*
	3.517	The service provider for a Physiotherapy Service task must be approved for that physiotherapy program if that provider is a Physiotherapy provider and the provider
	     is set up for electronic physiotherapy billing and reporting  (if REHAB_TASK.rehab_service_code = ‘S022’ and PROVIDER.provider_type_code = ‘M’ and PROVIDER.provider_sub_type_code = ‘35’ 
		and PROVIDER.ephysio_flag =’Y’,  then the provider must exist in  Rehab_Program_Xref_Clinic.rehab_program_code for the REHAB_TASK.rehab_program_code ).
	*/
	ls_ephysio_flag = nf_get_ephysio_flag(ll_provider_no, ls_provider_type_code)
	IF  ls_ephysio_flag = 'Y' AND ls_provider_type_code = 'M' AND  ls_provider_sub_type_code = '35' AND nf_provider_approved_for_program(ll_provider_no , ls_provider_type_code , ls_rehab_program_code) = FALSE THEN		
		ls_program_description = nf_get_program_description(ls_rehab_program_code)
		MessageBox('Rehab Task Error', "This provider is not an approved physiotherapy clinic for the " + ls_program_description , Exclamation!)
		RETURN -1
	END IF 

	/* BR 3.275 - The Actual Start Date must not be before then planned start date if the rehab task is for a
	                     Physiotherapy Service (i.e. REHAB_TASK.rehab_service_code = ‘S022’).
	*/
	IF ld_planned_start_date > ld_actual_start_date THEN 
		MessageBox('Rehab Task Error', "The actual start date must not be before the planned start date for a physio service"  , Exclamation!)
		RETURN -1
	END IF 
	
	 /* 3.690	The user should be warned that there is a Physio Report that is not yet submitted under the following conditions:
		•	the rehab service is for a Physiotherapy service (REHAB_TASK.rehab_service_code = ‘S022’)
		•	the rehab program is any physiotherapy program other than the P&A Questionnaire (REHAB_TASK.rehab_program_code  not = ‘P004’)
		•	the task status is being changed to Closed (REHAB_TASK.task_status_code = ‘04’)
		•	there is a physio report for this task that is in draft mode (i.e. a PHYSIO_REPORT exists for this claim and task and the PHYSIO_REPORT.report_status_code = ‘D’)
	 */
	 IF trim(ls_rehab_program_code) > '' AND ls_rehab_program_code <> 'P004' THEN
		
		IF ls_task_status_code = '04' AND  ls_task_status_code_origional <> '04' THEN
			IF nf_check_if_report_type_exists(ll_claim_no,   ll_task_no, 'D' ) = TRUE THEN 
				
				li_message = MessageBox('Task Status Warning', "The Physio Clinic still has a Report that has not yet been submitted to WorkSafeNB;" +&
															'~rThe clinic will not be able to submit the report if you close this task. Continue with Save?',Question!,OKCancel!, 2)
				IF li_message = 2 THEN RETURN -1 
			END IF 
				
		END IF 
		
		// CLOSED STATUS CHECKS
		IF ls_task_status_code <> '04' THEN
								
			/* 3.700	  The task status must be ‘Closed’ under the following conditions:
				•	the rehab service is for a Physiotherapy service (REHAB_TASK.rehab_service_code = ‘S022’)
				•	the rehab program is any physiotherapy program other than the P&A Questionnaire (REHAB_TASK.rehab_program_code  not = ‘P004’)
				•	the client has been discharged from the treatment program (i.e. a PHYSIO_REPORT exists for this claim and task and the 
				PHYSIO_REPORT.report_type_code = ‘D’ and the PHYSIO_REPORT.report_status_code = ‘S’)
			*/	
			IF nf_check_client_discharged(ll_claim_no, ll_task_no) = TRUE THEN 
				
				MessageBox('Injured Worker is Discharged', "The injured worker has already been discharged from this treatment program. "+&
									'If additional treatment is required under this physio program, you must create a new task.', Exclamation!)
				RETURN -1
			END IF 
			
			/* 3.720    The task status must be ‘Closed’ if the client is not appropriate for the physiotherapy program under the following conditions:
				•	the rehab service is for a Physiotherapy service (REHAB_TASK.rehab_service_code = ‘S022’)
				•	the rehab program is any physiotherapy program other than the P&A Questionnaire program (REHAB_TASK.rehab_program_code  not = ‘P004’)
				•	the client is Not Appropriate for the program  (REPORT_PHYSIO_MASTER.report_type_code = ‘I’ 
					and the REPORT_PHYSIO_MASTER.report_status_code = ‘S’ and the REPORT_PHYSIO_MASTER.appropriate_for_program_code = ‘N’)
			*/
			lb_client_appropriate_for_program = nf_client_appropriate_for_program(ll_claim_no, ll_task_no)
			IF lb_client_appropriate_for_program = FALSE THEN 
				MessageBox('Client Not Appropriate', "The task status must be ‘Closed’ if the client is not appropriate for the physiotherapy program", Exclamation!)
				RETURN -1	
			END IF 
		END IF 
		
		// task status is close - checks
		IF  ls_task_status_code = '04' THEN 
		
			/* 3.710	  The actual completion date must not be greater than the treatment discharge date, under the following conditions:
					•	the rehab service is for a Physiotherapy service (REHAB_TASK.rehab_service_code = ‘S022’)
					•	the rehab program is any physiotherapy program other than the P&A Questionnaire (REHAB_TASK.rehab_program_code  not = ‘P004’)
					•	the client has been discharged from the treatment program (i.e. a PHYSIO_REPORT exists for this claim and task and the 
						 PHYSIO_REPORT.report_type_code = ‘D’ and the PHYSIO_REPORT.report_status_code = ‘S’)
					•	task status is ‘Closed’  (04)
			*/
			ld_treatment_discharge_date = nf_get_treatment_discharge_date(ll_claim_no, ll_task_no)
			IF NOT isnull(ld_treatment_discharge_date)THEN 
				IF ld_actual_completion_date > ld_treatment_discharge_date THEN 
					
					MessageBox('Injured Worker is Discharged', "The injured worker was discharged from this treatment program on " + string(ld_treatment_discharge_date,"yyyy-mm-dd") + ". The treatment completion date must not be extended beyond this date. "+&
										'If additional treatment is required, you should create a new task.', Exclamation!)
					RETURN -1
				END IF 
			END IF 
				
			/* 3.730    The actual completion date of the task must be equal to the assessment date of the 
							Initial Assessment report if the client is not appropriate for the physiotherapy program under the following conditions:
					•	the rehab service is for a Physiotherapy service (REHAB_TASK.rehab_service_code = ‘S022’)
					•	the rehab program is any physiotherapy program other than the P&A Questionnaire program (REHAB_TASK.rehab_program_code  not = ‘P004’)
					•	the client is Not Appropriate for the program  (REPORT_PHYSIO_MASTER.report_type_code = ‘I’ 
						and the REPORT_PHYSIO_MASTER.report_status_code = ‘S’ and the REPORT_PHYSIO_MASTER.appropriate_for_program_code = ‘N’)
					•	task status is ‘Closed’  (04)
					
					The Clinic reported that the injured worker was Not Appropriate for this physio program - the actual closed date cannot be changed. 
					If the injured worker requires further treatment under this same physio program, you should create a new task.
			*/ 
			ld_initial_assessment_date = nf_get_initial_assessment_date(ll_claim_no, ll_task_no)
			IF 	 nf_client_appropriate_for_program(ll_claim_no, ll_task_no) = FALSE AND ld_actual_completion_date <> ld_initial_assessment_date  THEN
					MessageBox('Client Not Appropriate', "The Clinic reported that the injured worker was Not Appropriate for this physio program - the actual closed date cannot be changed. "+&
										'If the injured worker requires further treatment under this same physio program, you should create a new task.', Exclamation!)
					RETURN -1
			
			END IF 
		END IF 	
	END IF 		
END IF 

/* 3.520	The expedited service flag must be Yes or No.	*/
IF ls_expedited_service_flag <> 'Y' AND ls_expedited_service_flag <> 'N'  THEN
	MessageBox('Invalid Expedited Service Selection', "The expedited service flag must be Yes or No." , Exclamation!)
	idw_dw[1].SetFocus()
	idw_dw[1].SetColumn('expedited_service_flag')
	RETURN -1
END IF 	

/* 3.530	A claim should not be undergoing treatment concurrently for certain types of treatment while they are undergoing Primary physiotherapy treatment. 
				The user should receive a warning if they manually setup treatment for one of the following services or programs while receiving treatment for Primary Physio.
		•	Acupuncture  					(rehab service code ‘S001’) -- task_type_code (AS, TR)  task_sub_type_code (011)
		•	Athletic Therapy 				(rehab service code ‘S004’) -- task_type_code (AS, TR)  task_sub_type_code (159)
		•	Back management				(task sub_type_code ‘088’) -- task_type_code (TR)  task_sub_type_code (088)
		•	Chiropractic						(rehab service code ‘S006’) -- task_type_code (AS, TR)  task_sub_type_code (015)
		•	Massage Therapy				(rehab service code ‘S010’) -- task_type_code (AS, TR)  task_sub_type_code (154)
		•	Occ Rehab Level 1				(task_sub_type_code ‘146’) -- task_type_code (TR)  task_sub_type_code (146)
		•	Occ Rehab Level 2				(task_sub_type_code ‘147’) -- task_type_code (TR)  task_sub_type_code (147)
		•	Work Hardening					(task sub_type_code ‘096’’) -- task_type_code (TR)  task_sub_type_code (096)
		•	Work Recovery					(task sub_type_code ‘097’’) -- task_type_code (AS, TR)  task_sub_type_code (097)
		•	Spinal Cord Rehabilitation	(task sub_type_code ‘094’’) -- task_type_code (AS, TR)  task_sub_type_code (094)
			
		Can you please Change to
		The claim should not be undergoing treatment for this service while they are undergoing treatment for Primary Physiotherapy. Continue with save ?
*/
IF l_task_type_code_status <> NOTMODIFIED!  OR  l_task_sub_type_code_status <> NOTMODIFIED! THEN 
	
	IF ((ls_task_type_code = 'TR' OR ls_task_type_code = 'AS' ) AND ls_task_sub_type_code = '011' ) OR &
		((ls_task_type_code = 'TR'  OR ls_task_type_code = 'AS' ) AND ls_task_sub_type_code = '159' ) OR &
		(ls_task_type_code = 'TR' AND ls_task_sub_type_code = '088' ) OR &
		((ls_task_type_code = 'TR' OR ls_task_type_code = 'AS' ) AND ls_task_sub_type_code = '015' ) OR &
		((ls_task_type_code = 'TR' OR ls_task_type_code = 'AS' ) AND ls_task_sub_type_code = '154' ) OR &
		(ls_task_type_code = 'TR' AND ls_task_sub_type_code = '146' ) OR &
		(ls_task_type_code = 'TR' AND ls_task_sub_type_code = '147' ) OR &
		((ls_task_type_code = 'TR'  OR ls_task_type_code = 'AS' ) AND ls_task_sub_type_code = '094' ) OR &
		(ls_task_type_code = 'TR' AND ls_task_sub_type_code = '096' ) OR &
		((ls_task_type_code = 'TR'  OR ls_task_type_code = 'AS' ) AND ls_task_sub_type_code = '097' )  THEN
						
		SELECT 	count(*) 
		INTO			:ll_count
		FROM 		REHAB_TASK
		WHERE 	claim_no 				   	 = 	:ll_claim_no
		AND     		task_no 				     <> 	:ll_task_no
		AND     		rehab_program_code  = 	'P001'
		AND     		rehab_service_code	 = 	'S022'
		AND     		task_status_code  NOT IN ('03','04','05') //closed, cancelled, reset
		USING		SQLCA;
		SQLCA.nf_handle_error("n_task","nf_check_bus_rules()","SELECT count(*) FROM REHAB_TASK") 
								
		IF ll_count > 0 THEN 
			li_message = MessageBox('Warning', "The claim should not be undergoing treatment for this service while they are undergoing treatment for Primary Physiotherapy." +&
																	 "~rContinue with Save?",Question!,OKCancel!, 2)	
			IF li_message = 2 THEN RETURN -1	
		END IF 
	END IF 
END IF 

/*
This rule only applies to tasks for the rehab service of Physio (S022)

3.535	A claim must not have more than one task that is Planned or in Progress for the same Physiotherapy program.

Primary Physio – there can only be one task for that claim for Primary Physio that is Planned or In Progress
Work Conditioning  – there can only be one task for that claim for Work Conditioning  that is Planned or In Progress
Shoulder program – there can only be one task for that claim for Shoulder Program that is Planned or In Progress

 task_status_code task_status_desc
---------------- 			----------------------------------------
01               			planned
02               			in progress
03               			cancelled
04               			closed
05               			reset
*/
IF ls_rehab_service_code = 'S022' AND ( ls_task_status_code	= '01'  OR  ls_task_status_code	= '02') THEN
					
	SELECT 	count(*) 
	INTO			:ll_count
	FROM 		REHAB_TASK
	WHERE 	claim_no 				   	 = 	:ll_claim_no
	AND     		task_no 				     <> 	:ll_task_no
	AND     		rehab_program_code  = 	:ls_rehab_program_code
	AND     		rehab_service_code	 = 	'S022'
	AND     		task_status_code       IN ('01','02') //Planned or In Progress
	USING		SQLCA;
	SQLCA.nf_handle_error("n_task","nf_check_bus_rules()","SELECT count(*) FROM REHAB_TASK B") 
								
	IF ll_count > 0 THEN 
		MessageBox('Rehab Task Error', "A claim must not have more than one task that is Planned or in Progress for the same Physiotherapy program."  , Exclamation!)
		RETURN -1
	END IF	
END IF 

/*  3.540	A claim must not have a task for the primary physiotherapy program that has a start date and closed date that overlaps 
    with another task for primary physiotherapy program for that claim; tasks that have a status of cancelled are excluded. 
	 Business wording: A course of treatment for primary physio for a claim must not have an effective period that overlaps with another course 
	 of primary physio for that claim.	
	 
	 3.540 – commented out and then removed if we determine through testing that 3.560 covers it
	 
	 REVISED:
	 3.540	A claim must not have a task for the Primary Physiotherapy program that has a start date and closed date that overlaps with another task for
	           primary physiotherapy program for that claim; exclude the primary physiotherapy tasks that have a status of Cancelled as well as the 
				primary physiotherapy tasks that were created prior to the date of the first Physio Deployment changes.
				
	NEW:			
	3.542	A claim must not have a task for the Physiotherapy Work Conditioning program that has a start date and closed date that overlaps with another task for 
				Work Conditioning program for that claim; exclude the Work Conditioning tasks that have a status of Cancelled as well as the Work Conditioning tasks
				that were created prior to the date of the first Physio Deployment changes.

	NEW:
	3.544	A claim must not have a task for the Physiotherapy Shoulder program that has a start date and closed date that overlaps with another task for
				Physiotherapy Shoulder program for that claim; exclude the Physiotherapy Shoulder tasks that have a status of Cancelled as well as the Physiotherapy Shoulder 
				tasks that were created prior to the date of the first Physio Deployment changes.

	NEW:
	3.546	A claim must not have a task for the Pain & Activity Questionnaire program that has a start date and closed date that overlaps with another task for 
				Pain & Activity Questionnaire program for that claim; exclude the Pain & Activity Questionnaire tasks that have a status of Cancelled as well as the Pain & Activity Questionnaire
				tasks that were created prior to the date of the first Physio Deployment changes. 
				
				rehab_program_code rehab_program_desc_e
				------------------ ----------------------------------------    
				P001               Primary Physio
				P002               Shoulder Program
				P003               Work Conditioning
				P004               P & A Questionnaire
								
		Start Date for a Task 
		– planned start date for a task that is Planned 
		– actual start date for a task that is In Progress or Closed  
		Closed Date for a Task 
		– planned completed date for a task that is Planned  
		– actual completed date for a task that is In Progress or Closed 

*/

IF  ls_task_status_code	<> '03' THEN 
				
		CHOOSE CASE ls_rehab_program_code
			CASE 'P001', 'P002', 'P003', 'P004'
				
					CHOOSE CASE ls_task_status_code
						CASE '01'				
							ld_start_date_test 		 	= ld_planned_start_date
							ld_completion_date_test 	= ld_planned_completion_date
						CASE ELSE 			
							ld_start_date_test 			= ld_actual_start_date
							ld_completion_date_test 	= ld_actual_completion_date
					 END CHOOSE 
				
					//planned
					SELECT		count(*) 
					INTO			:ll_count
					FROM 		REHAB_TASK
					WHERE		claim_no 						= :ll_claim_no
					AND    		task_no 						<> :ll_task_no
					AND     		task_status_code 			= '01'
					AND     		rehab_program_code  	= :ls_rehab_program_code	
					AND			create_date 					>= :idt_ephysio_br_check_date 
					AND 	  ((	planned_start_date 				 BETWEEN :ld_start_date_test   AND :ld_completion_date_test )
						OR 	(	planned_completion_date    	 BETWEEN :ld_start_date_test   AND :ld_completion_date_test )
						OR 	(	planned_start_date 				> :ld_start_date_test  				 AND planned_completion_date < :ld_completion_date_test )
						OR 	(	:ld_start_date_test			     BETWEEN planned_start_date   AND planned_completion_date ))
					USING SQLCA;
					SQLCA.nf_handle_error("n_task","nf_check_bus_rule()","SELECT count(*) FROM 	REHAB_TASK (4)") 
			
					IF ll_count > 0  THEN 
						MessageBox('Overlapping Program', "This claim currently has a Program of the same type that is not cancelled and has overlapping dates." , Exclamation!)
						idw_dw[1].SetFocus()
						idw_dw[1].SetColumn('rehab_program_code')
						RETURN -1
					END IF 
					
					IF isnull(ld_completion_date_test) THEN ld_completion_date_test = DATE('2079-06-06')// whatever in here
			
					//everything else
					SELECT	count(*) 
					INTO		:ll_count
					FROM 	REHAB_TASK
					WHERE	claim_no 						= 		:ll_claim_no
					AND    	task_no 						<> 	:ll_task_no
					AND     task_status_code			NOT IN ( '01', '03')
					AND     rehab_program_code  	= 		:ls_rehab_program_code
					AND		create_date 					>= 	:idt_ephysio_br_check_date 
					AND 	  ((	actual_start_date 				 BETWEEN :ld_start_date_test  AND :ld_completion_date_test )
						OR 	(	actual_completion_date    	 BETWEEN :ld_start_date_test  AND :ld_completion_date_test )
						OR 	(	actual_start_date 				> :ld_start_date_test  				AND actual_completion_date <= :ld_completion_date_test )
						OR 	(	:ld_start_date_test			 BETWEEN actual_start_date   	AND actual_completion_date )
						OR    (  :ld_start_date_test			 > actual_start_date   	           AND actual_completion_date is null))
						
						
					USING SQLCA;
					SQLCA.nf_handle_error("n_task","nf_check_bus_rule()","SELECT count(*) FROM 	REHAB_TASK (4)") 
						
					IF ll_count > 0  THEN 
						MessageBox('Overlapping Program', "This claim currently has a Program of the same type that is not cancelled and has overlapping dates." , Exclamation!)
						idw_dw[1].SetFocus()
						idw_dw[1].SetColumn('rehab_program_code')
						RETURN -1
					END IF 
					
		END CHOOSE	
END IF 

/* 3.560	A claim must not have more than one rehab task of the same type that has an overlapping period for that claim. 
               Rationale: Cannot have more than one Physio treatments for the same time period. Cannot have more than one task for total hip surgery 
				for a claim occurring at the same time.	
					
		NOTE: based on check date contained in the .ini file.  -[ePhysio]
		New_Task_Rules_Date = '2012/02/05'		
		
        the rule is ALWAYS checked but we exclude the tasks from the set of tasks to check if the task is cancelled or if the task is closed and the 
		  actual completed date is less than the INI file parameter
		  
		  task_status_code task_status_desc
---------------- ----------------------------------------
01               planned
02               in progress
03               cancelled
04               closed
05               reset

Modified: 2012-0215
3.560	A claim must not have more than one rehab task of the same type for rehab services other than Physiotherapy and Communication that 
has an overlapping period for that claim, provided the task is not Cancelled  and the task was not closed prior to the date of the first Physio Deployment changes. 

Diana wants to exclude the Communication tasks from this rule and we already have specific rules for the tasks for the 4 Physio programs (rules 3.540 – 3.546)
: Removed 20120402

*/		

 
/* Need a rule to prevent the user from modifying the task (especially the task status). 
	3.570	A task for the physiotherapy service must not be modified if the task was created prior to the date of the first Physio Deployment changes.
	
	Disabled datawindow most likely on
*/

/* 3.580	A rehab task must not be Cancelled if there are any rehab authorizations for that task that have been paid 
(i.e. all related rehab authorizations must have a paid amount that is zero if the status is cancelled) */
IF ls_task_status_code = '03' AND   l_task_status_code_status <> NOTMODIFIED! THEN
	
	SELECT count(*) 
	INTO		:ll_count
	FROM 	REHAB_TASK_AUTHORIZATION
	WHERE task_no 		= :ll_task_no
	AND		claim_no 		= :ll_claim_no
	AND     paid_amount > 0
	USING  SQLCA;
	SQLCA.nf_handle_error("n_task","nf_check_bus_rule()","SELECT count(*) FROM 	REHAB_TASK_AUTHORIZATION(2)") 

	IF ll_count > 0  THEN 
		MessageBox('Invalid Task Status', "A rehab task must not be Cancelled if there are any rehab authorizations for that task that have been paid" , Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('task_status_code')
		RETURN -1
	END IF
	
	/* 3.590	The Actual Start Date and the Actual Completion Date must not be entered if the task is Cancelled.*/
	IF NOT ISNULL(ld_actual_completion_date) AND NOT ISNULL(ld_actual_start_date) THEN
		MessageBox('Invalid Task Status', "The Actual Start Date and the Actual Completion Date must not be entered if the task is Cancelled." , Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('task_status_code')
		RETURN -1
	END IF		
END IF 

/* 3.585 A rehab task must not be Planned if there are any rehab authorizations for that task that have been paid
   (i.e. all related rehab authorizations must have a paid amount that is zero if the status is planned) */
IF ls_task_status_code = '01' AND   l_task_status_code_status <> NOTMODIFIED! THEN
	
	SELECT count(*) 
	INTO		:ll_count
	FROM 	REHAB_TASK_AUTHORIZATION
	WHERE task_no 		= :ll_task_no
	AND		claim_no 		= :ll_claim_no
	AND     paid_amount > 0
	USING SQLCA;
	SQLCA.nf_handle_error("n_task","nf_check_bus_rule()","SELECT count(*) FROM 	REHAB_TASK_AUTHORIZATION(3)") 

	IF ll_count > 0  THEN 
		MessageBox('Invalid Task Status', "A rehab task must not be Planned if there are any rehab authorizations for that task that have been paid" , Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('task_status_code')
		RETURN -1
	END IF
END IF 

/* 3.600	A rehab task must not be deleted.	 NOT SURE THERE IS THE FUNCTIONALLITY TO DELETE */
SELECT count(*) 
INTO		:ll_count
FROM 	REHAB_TASK_AUTHORIZATION
WHERE task_no 		= :ll_task_no
AND		claim_no 		= :ll_claim_no
USING 	SQLCA;
SQLCA.nf_handle_error("n_task","nf_check_bus_rule()","SELECT count(*) FROM 	REHAB_TASK_AUTHORIZATION(4)") 

/* 3.610	The rehab service must not be modified if there are any rehab authorizations for that task 
      (rationale – the authorizations that are created are based on the rehab service, program, task type/subtype/specific type). */
IF ll_count > 0 THEN
	IF  idw_dw[1].GetItemStatus(1,'rehab_service_code',primary!) = DATAMODIFIED!  THEN 
		IF ls_rehab_service_code <>  idw_dw[1].GetItemString(1, 'rehab_service_code', PRIMARY!,TRUE) THEN
			MessageBox('Rehab Service', "The rehab service must not be modified if there are any rehab authorizations for that task " , Exclamation!)
			idw_dw[1].SetFocus()
			idw_dw[1].SetColumn('rehab_service_code')
			RETURN -1
		END IF 
	END IF 
	
	/* 3.620	The rehab program must not be modified if there are any rehab authorizations for that task.. */
	IF  idw_dw[1].GetItemStatus(1,'rehab_program_code',primary!) = DATAMODIFIED!  THEN 
		IF ls_rehab_program_code <>  idw_dw[1].GetItemString(1, 'rehab_program_code', PRIMARY!,TRUE) THEN
			MessageBox('Rehab Program', "The rehab program must not be modified if there are any rehab authorizations for that task." , Exclamation!)
			idw_dw[1].SetFocus()
			idw_dw[1].SetColumn('rehab_program_code')
			RETURN -1
		END IF 
	END IF 
	
	/* 3.630	The task type must not be modified if there are any rehab authorizations for that task. */
	IF  idw_dw[1].GetItemStatus(1,'task_type_code',primary!) = DATAMODIFIED!  THEN 
		IF ls_task_type_code <>  idw_dw[1].GetItemString(1, 'task_type_code', PRIMARY!,TRUE) THEN
			MessageBox('Task Type', "The task type must not be modified if there are any rehab authorizations for that task." , Exclamation!)
			idw_dw[1].SetFocus()
			idw_dw[1].SetColumn('task_type_code')
			RETURN -1
		END IF 
	END IF 
	
	/* 3.640	The task sub type must not be modified if there are any rehab authorizations for that task. */
	IF  idw_dw[1].GetItemStatus(1,'task_sub_type_code',primary!) = DATAMODIFIED!  THEN 
		IF ls_task_sub_type_code <>  idw_dw[1].GetItemString(1, 'task_sub_type_code', PRIMARY!,TRUE) THEN
			MessageBox('Task Sub Type', "The task sub type must not be modified if there are any rehab authorizations for that task." , Exclamation!)
			idw_dw[1].SetFocus()
			idw_dw[1].SetColumn('task_sub_type_code')
			RETURN -1
		END IF 
	END IF 
	
	/* 3.650	The task specific type must not be modified if there are any rehab authorizations for that task.	 */
	IF  idw_dw[1].GetItemStatus(1,'task_specific_code',primary!) = DATAMODIFIED!  THEN 
		IF ls_task_specific_code <>  idw_dw[1].GetItemString(1, 'task_specific_code', PRIMARY!,TRUE) THEN
			MessageBox('Task Specific Code', "The task specific type must not be modified if there are any rehab authorizations for that task." , Exclamation!)
			idw_dw[1].SetFocus()
			idw_dw[1].SetColumn('task_specific_code')
			RETURN -1
		END IF 
	END IF 
	
	/* 3.660 The service provider (type and number) should not be modified to another provider if there are any rehab authorizations 
				  under that task that are authorized to the same provider as the task.	Warn the user if there are any authorizations under that task 
				  that have the same provider as the provider that was originally assigned to the task so that they can change the authorizations, if needed. 
		MODIFIED TO	
		3.660  The service provider (type and number) MUST not be modified to a different service provider if there are any rehab authorizations 
				   under that task that are authorized to the same provider as the task and the authorized billable_item has been partially or fully paid  
					( the paid amount is greater than zero )	
					
		REVISED: 20120125
		3.660	The service provider (type and number) on an existing task must not be modified to a different service provider if there is a rehab authorization 
		            under that task that is authorized to the same provider as the task and the rehab authorization has been partially or fully paid 
	*/
	SELECT count(*) 
	INTO		:ll_count_auth_check
	FROM 	REHAB_TASK_AUTHORIZATION
	WHERE task_no 									= :ll_task_no
	AND		claim_no 									= :ll_claim_no
	AND     authorized_provider_no 				= :ll_provider_no_origional
	AND     authorized_provider_type_code 	= :ls_provider_type_code_original
	AND     paid_amount                           > 0
	USING 	SQLCA;
	SQLCA.nf_handle_error("n_task","nf_check_bus_rule()","SELECT count(*) FROM 	REHAB_TASK_AUTHORIZATION(99)") 
	
	IF  ll_count_auth_check > 0 THEN 
	
		IF  idw_dw[1].GetItemStatus(1,'provider_no', primary!) = DATAMODIFIED!  AND ( NOT ISNULL(ll_provider_no_origional) AND ll_provider_no_origional <> 0) THEN 
			IF ll_provider_no <>  ll_provider_no_origional THEN
					MessageBox('Provider', "The service provider number must not be modified to another provider if there are any rehab authorizations"+&
													  "~runder that task that are authorized to the same provider as the task and the paid amount is greater than zero.", Exclamation!)
					RETURN -1 																	 														 
			END IF 
		END IF 
		
		IF  idw_dw[1].GetItemStatus(1,'provider_type_code', primary!) = DATAMODIFIED! AND trim(ls_provider_type_code_original) <> '' THEN 
			IF ls_provider_type_code <> ls_provider_type_code_original THEN
				MessageBox('Provider', "The service provider type must not be modified to another provider if there are any rehab authorizations"+&
													"~runder that task that are authorized to the same provider as the task and the paid amount is greater than zero.", Exclamation!)											
				 RETURN -1																									
			END IF 
		END IF 
	END IF 
	
	/* NEW:
	3.667	The user must be warned if the service provider can be modified on the task (see rule 3.660) and there is a rehab authorization that will not have 
				the authorized provider automatically updated due to the following conditions:
				•	the billable item on the rehab authorization is an item that is automatically created when the task is created  
				•	the authorized billable item has not been partially or fully paid (REHAB_TASK_AUTHORIZATION.paid_amount > 0)
				•	the authorized provider on the rehab authorization is not the same as the original provider on the task			
	*/
	IF  (( idw_dw[1].GetItemStatus(1,'provider_no', primary!) = DATAMODIFIED! AND  ll_provider_no <>  ll_provider_no_origional ) OR +&
		( idw_dw[1].GetItemStatus(1,'provider_type_code', primary!) = DATAMODIFIED! AND  ls_provider_type_code <> ls_provider_type_code_original )) THEN
						
			SELECT 	COUNT(*) 
			INTO			:li_check_count 
			FROM 		REHAB_TASK_AUTHORIZATION a
								join Billable_Item_Rehab_Task_Xref b ON a.billable_xref_no = b.billable_xref_no
			WHERE   	b.auto_create_authorization_flag 	= 'Y' 
			AND			a.paid_amount 								= 0
			AND			a.task_no        								= :ll_task_no
			AND     		a.claim_no       								= :ll_claim_no
			AND         a.authorized_provider_no             > 0
			AND        ( a.authorized_provider_type_code 	<> :ls_provider_type_code_original
				OR           a.authorized_provider_no          <> :ll_provider_no_origional )
			USING      sqlca;
					
			SQLCA.nf_handle_error("n_task","nf_check_bus_rule()","SELECT COUNT(*) INTO :li_check_count ")
			
			IF li_check_count > 0 THEN 
				MessageBox('Warning', "You are about to Modify the Provider. There are associated Authorizations where the Provider is not eligible to be updated."+&
			    								   "~rThe update will continue but you should review the associated Authorizations.", Exclamation!)				
			END IF 							
	END IF 		
END IF 

/* 3.670	The actual start date for a physiotherapy service task must be less than or equal to the earliest date of service
               for an invoice item that has been billed under that task (i.e.  the minimum Rehab_Invoice_Line_Item.service_date).*/
SELECT min(service_date) 
INTO    :ldtm_min_service_date
FROM 	REHAB_INVOICE_LINE_ITEM 
WHERE claim_no 	= :ll_claim_no
AND 		task_no 	= :ll_task_no
AND    	payment_no > 0
USING 	SQLCA;
SQLCA.nf_handle_error("n_task","nf_check_bus_rule()","SELECT FROM REHAB_INVOICE_LINE_ITEM") 

IF ld_actual_start_date > DATE(ldtm_min_service_date) THEN
	MessageBox('Actual Start Date', "The actual start date for a physiotherapy service task must be less than or equal to the earliest date of service" +&
	                                             "~rfor an invoice item that has been billed under that task." , Exclamation!)
	idw_dw[1].SetFocus()
	idw_dw[1].SetColumn('actual_start_date')
	RETURN -1
END IF 

/* 3.160	The planned start date must not be after the planned completion date. */
IF ld_planned_start_date > ld_planned_completion_date THEN 
	MessageBox('Invalid planned start Date', "The planned start date must not be after the planned completion date. ", Exclamation!)
	idw_dw[1].SetFocus()
	idw_dw[1].SetColumn('planned_start_date')
	RETURN -1
END IF 

/* 3.210	The actual start date, if entered, must be a valid date. */
IF NOT ISNULL(ld_actual_start_date) THEN
	/* 3.220	The Actual Start date, if entered, must not be prior to the accident date. */
	IF NOT nf_check_accident_date(idw_dw[1].GetItemDateTime(1,'actual_start_date')) THEN
		MessageBox('Invalid Actual Start Date','The Actual Start date, if entered, must not be prior to the accident date.')
		RETURN -1
	END IF
	
	/* 3.230	The Actual Start Date, if entered, must not be in the future. */
	IF ld_actual_start_date > Date(f_server_datetime()) THEN
		MessageBox('Invalid Actual Start Date', "The Actual Start Date, if entered, must not be in the future")
		RETURN -1
	END IF	
END IF 

/*	validate ACTUAL COMPLETION DATE  */
IF NOT ISNULL(ld_actual_completion_date) THEN
	/* 3.250	The Actual Completion Date, if entered, must not be in the future */
	IF ld_actual_completion_date > Date(f_server_datetime()) THEN
		MessageBox('Invalid Actual Completion Date', "The Actual Completion Date cannot be in the future")
		RETURN -1
	END IF
	
	/* 3.260	The Actual Completion date, if entered, must not be before the Actual Start date.	*/
	IF ld_actual_completion_date < ld_actual_start_date THEN
		MessageBox('Invalid Actual Completion Date','The actual completion date cannot be before the actual start date.')
		RETURN -1
	END IF
	
	/* 3.265   The Actual Completion date, if entered, must be greater than or equal to the maximum service date for all billable items that have been invoiced under the task.
					Actual completion date must be >=  max(REHAB_INVOICE_LINE_ITEM.service_date) for the REHAB_INVOICE_LINE_ITEM.task_no */
	SELECT max(service_date) 
	INTO    :ldtm_max_service_date
	FROM 	REHAB_INVOICE_LINE_ITEM 
	WHERE claim_no 		= :ll_claim_no
	AND 		task_no 		= :ll_task_no
	USING 	SQLCA;
	SQLCA.nf_handle_error("n_task","nf_check_bus_rule()","SELECT max(service_date)") 
	
	IF ld_actual_completion_date < date(ldtm_max_service_date) THEN
		MessageBox('Invalid Actual Completion Date','The Actual Completion date must be greater than or equal to the maximum service date for all billable items that have been invoiced under the task.')
		RETURN -1
	END IF
	
	/* 3.270	The Actual Start Date must be entered if an Actual Completion Date is entered.) */
	IF isnull(ld_actual_start_date) THEN 
		MessageBox('Invalid Actual Start','An Actual Start Date must be entered when an Actual Completion Date is entered')
		RETURN -1
	END IF 	
END IF

// COMBO MUST BE VALID
IF l_task_type_code_status <> NOTMODIFIED! OR l_task_sub_type_code_status <> NOTMODIFIED! OR l_task_specific_code_status <> NOTMODIFIED! THEN 

	/*	validate the TASK COMBO */
	SELECT 	count(*)
	INTO 		:ll_count
	FROM 		Task_Specific
	WHERE 	task_type_code 		= :ls_task_type_code
	AND 			task_sub_type_code 	= :ls_task_sub_type_code
	AND 			task_specific_code 	= :ls_task_specific_code
	USING 		SQLCA;
	SQLCA.nf_handle_error("Embedded SQL: SELECT from Task Specific","n_task","nf_check_bus_rule") 
		
	IF ll_count = 0 THEN
		MessageBox('Selection Error','The selected task type / task sub type / task specific combination is invalid.')
		RETURN -1
	END IF
END IF 

/*	validate INTERNAL PROVIDER */
IF NOT IsNull(ll_provider_no) THEN
	IF ll_provider_no = 0 THEN
		idw_dw[1].SetItem(1,'provider_type_code',' ')
	ELSE
		
		SetNull(ls_string2)
		SELECT 	provider_sub_type_code
		INTO 		:ls_string2
		FROM 		PROVIDER
		WHERE 	provider_no 				= :ll_provider_no
		AND 			provider_type_code 	= :ls_provider_type_code
		USING 		SQLCA; 
		SQLCA.nf_handle_error('Embedded SQL: Select from PROVIDER','n_task','nf_check_bus_rule') 
			
		IF IsNull(ls_string2) THEN
			MessageBox('Invalid Provider Information','The provider number is invalid for the type chosen.')
			idw_dw[1].SetColumn('provider_no')
			idw_dw[1].SetFocus()
			RETURN -1
		END IF
	END IF
END IF
	
/*	3.150	The planned start date must not be before the accident date. */
IF NOT IsNull(ld_planned_start_date) THEN
	IF	NOT nf_check_accident_date(idw_dw[1].GetItemDateTime(1,'planned_start_date')) THEN
		MessageBox('Invalid Planned Start Date','The planned start date cannot be before the accident date.')
		RETURN -1
	END IF
END IF

/* Ensure the planned start date is not after the planned completed date  */
IF ld_planned_completion_date < ld_planned_start_date THEN
	MessageBox('Invalid Planned Dates','The planned start date cannot be after the planned completion date.')
	RETURN -1
END IF

/* 3.200	The Planned Completion date must not be before the revised Planned Start date.  [ REMOVED]  */
/*	3.180	The revised Planned Start date must not be before the accident date.  [ REMOVED] */
/* 3.200	The Planned Completion date must not be before the revised Planned Start date.	 [ REMOVED]  */
/* 3.190	The revised Planned Start date should not be the same date as the Planned Start date.	 [ REMOVED]  */

/* 3.170	If the Planned Start date has been revised, then the planned date must be revised for all related action items.	*/
IF idw_dw[1].GetItemStatus(1,'planned_start_date',Primary!) = DataModified! THEN
	IF Not IsNull(idw_dw[1].GetItemDateTime(1,'planned_start_date',Primary!,TRUE)) THEN
		li_days = DaysAfter(Date(idw_dw[1].GetItemDateTime(1,'planned_start_date',Primary!,True)),&
					Date(idw_dw[1].GetItemDateTime(1,'planned_start_date',Primary!,FALSE)))
	ELSE
		li_days = 0
	END IF
	
	IF li_days <> 0 THEN 
		li_rtn = inv_action_item.nf_reschedule_related_action_items(0, ll_task_no, li_days, FALSE)
		IF li_rtn < 0 THEN RETURN -1
		
		//li_rtn is the number of records affected
		IF li_rtn > 0 THEN
			ib_rescheduled_related_action_items = TRUE
		END IF
	END IF 
END IF

SELECT 	success_code_required
INTO 		:ls_success_code_required
FROM 		Task_Type
WHERE 	task_type_code = :ls_task_type_code
USING 		SQLCA;
SQLCA.nf_handle_error("Embedded SQL: SELECT from Task Type table","n_task","nf_check_bus_rule") 

/* 3.330	The task success status must be Inapplicable if the task success status is not required.	*/
IF ls_success_code_required = 'N' AND ls_task_success_code <> 'I' THEN 
	MessageBox('Invalid task success','The task success status must be Inapplicable if the task success status is not required.')
	RETURN -1
END IF 
		
/*	Validate TASK STATUS vs. ACTUAL DATES
 	If Status "Planned"								- Actual Start & Actual Completion dates should be blank
	If Status "In Progress"					 	- an Actual Start date is required but Actual Completion must be blank
	If Status "Cancelled" or "Completed" 	- both Actual Start & Actual Completion dates are required 
	
	task_status_code task_status_desc
	---------------- ----------------------------------------
	01               planned
	02               in progress
	03               cancelled
	04               completed
	05               reset
	
	
	task_success_code task_success_desc
	----------------- ------------------------------
	I                 INAPPLICABLE
	N                 NOT SUCCESSFUL
	X                 NOT YET DETERMINED
	Y                 SUCCCESSFUL
*/
CHOOSE CASE ls_task_status_code 
	CASE '01' 	/* Task Status = PLANNED */
		
		/*  3.280	The Actual Start date must not be entered for a task that is planned.	*/
		/*  3.340	If the task is 'Planned' or ‘Cancelled’, the Actual Start & Actual Completion dates must not be entered (must be null). */
		IF NOT IsNull(ld_actual_start_date)THEN
			MessageBox('Invalid Actual Dates','The Actual Start date must remain blank for a Planned task')
			RETURN -1
		END IF	
		
		/* 3.285	The Actual Completion date must not be entered for a task that is planned.	*/
		IF NOT IsNull(ld_actual_completion_date)THEN
			MessageBox('Invalid Actual Dates','The Actual Completion date must remain blank for a Planned task')
			RETURN -1
		END IF	
		
		/* 3.320	If the task is 'Planned' or 'In Progress' and the success status is required,  the task success status must be Not Yet Determined.	  */
		IF ls_success_code_required = 'Y' AND ls_task_success_code <> 'X' THEN 
				MessageBox('Invalid Task Success','A Planned task must have a result of NOT YET DETERMINED')
				RETURN -1
		END IF	
		
	CASE '02' 	/* Task Status = IN PROGRESS */
		
		/* 3.290	The Actual Start date must be entered for a task that is In Progress. */
		IF IsNull(ld_actual_start_date)  THEN
			MessageBox('Invalid Actual Dates','An Actual Start date must be entered for an In Progress task')
			RETURN -1
		END IF	
		
		/* 3.300	The Actual Completion date must not be entered for a task that is In Progress. */
		IF NOT IsNull(ld_actual_completion_date) THEN
			MessageBox('Invalid Actual Dates','The Actual Completion date must remain blank for an In Progress task')
			RETURN -1
		END IF
		
		/* 3.320	If the task is 'Planned' or 'In Progress' and the success status is required, the task success status must be Not Yet Determined.	 */
		IF ls_success_code_required = 'Y' AND ls_task_success_code <> 'X' THEN 
				MessageBox('Invalid Task Success','A In Progress task must have a result of NOT YET DETERMINED')
				RETURN -1
		END IF	
		
	CASE '03'	/* Task Status = cancelled */
		
		/* 3.340	If the task is  ‘Cancelled’, the Actual Start date must not be entered (must be null). */
		IF NOT IsNull(ld_actual_start_date)THEN
			MessageBox('Invalid Actual Start Date','If the task is  ‘Cancelled’, the Actual Start date must not be entered (must be null)')
			RETURN -1
		END IF
		
		/* 3.340	If the task is ‘Cancelled’, the Actual Completion dates must not be entered (must be null). */
		IF NOT IsNull(ld_actual_completion_date) THEN
			MessageBox('Invalid Actual Completion Date','If the task is ‘Cancelled’, the Actual Completion dates must not be entered (must be null).')
			RETURN -1
		END IF
		
		/* 3.325	If the task is ‘Cancelled’ and the success status is required, the task success status must be Not Yet Determined.  */ 
		IF ls_success_code_required = 'Y' AND ls_task_success_code <> 'X' THEN 
			MessageBox('Invalid task success','The task success status must not be Not Yet Determined if the task is cancelled and the success code is required.')
			RETURN -1
		END IF 
		
		/* 3.420 The task status must be set to Cancelled (task status ‘03’) for all of the related Follow-up action items if the task is cancelled or completed (task status ‘03’ or ‘04’). */
		/* 3.425	The actual start date and actual completed date must be set to the current date for all of the related Follow-up action items if the task is cancelled or completed (task status ‘03’ or ‘04’). */
		ll_canceled_count = nf_cancel_related_follow_ups()
		IF ll_canceled_count > 0 THEN
			ib_canceled_related_action_items = True
		ELSEIF ll_canceled_count = -1 THEN
			RETURN -1
		END IF
		
	CASE '04' /* Task Status = closed */
		
		/* 3.310	The Actual Completion date must be entered for a task that is Closed.	 */
		IF IsNull(ld_actual_completion_date)THEN
			MessageBox('Invalid Actual Dates','The Actual Completion date must be entered for a task that is Closed.	')
			RETURN -1
		END IF
		
		/* 3.310	The Actual Start date must be entered for a task that is Closed.	 */
		IF IsNull(ld_actual_start_date)THEN
			MessageBox('Invalid Actual Dates','The Actual Start date must be entered for a task that is Closed')
			RETURN -1
		END IF
		
		/* 3.327	If the task is ‘Closed’ and the success status is required, the task success status must be Not Yet Determined, Not Successful or Successful. */
		IF ls_success_code_required = 'Y' AND ls_task_success_code = 'I' THEN 
			MessageBox('Invalid task success','The task success status cant be Inapplicable if the task success status is required and the task is closed.')
			RETURN -1
		END IF 
		
		/* 3.420 The task status must be set to Cancelled (task status ‘03’) for all of the related Follow-up action items if the task is cancelled or completed (task status ‘03’ or ‘04’).	 */
		/* 3.425	The actual start date and actual completed date must be set to the current date for all of the related Follow-up action items if the task is cancelled or completed (task status ‘03’ or ‘04’). */
		ll_canceled_count = nf_cancel_related_follow_ups()
		IF ll_canceled_count > 0 THEN
			ib_canceled_related_action_items = True
		ELSEIF ll_canceled_count = -1 THEN
			RETURN -1
		END IF
			
	CASE ELSE //reset  '05'
END CHOOSE

/* Check to make sure that a claim exists.  Rob Head 98/09/14. */
IF w_rehab_sheet.dw_basic_claim.RowCount() = 0 THEN
	MessageBox('Error', 'Unable to perform process when a claim is not active in basic claim window.')
	RETURN -1
END IF 

/* determine whether or not an external requestor is to be saved */
ldtm_accident_date = w_rehab_sheet.dw_basic_claim.GetItemDateTime(1, 'accident_date')

/* validate the rehab referral information idw_dw[2] */
IF idw_dw[2].RowCount() > 0 THEN
	IF idw_dw[2].GetItemStatus(1,0,Primary!) = New! THEN
		/* new row but no data */
		idw_dw[2].DeleteRow(1)
	ELSE
		/* validate provider info for external */
		IF IsNull(ls_referring_provider_type_code) and (IsNull(ll_referring_provider_no) OR ll_referring_provider_no = 0) THEN
			/* delete it since no data */	
			idw_dw[2].DeleteRow(1)
		ELSE
			
			/* 3.350	The rehab referral Provider Type must be entered if the external requestor Provider Number is entered.	
			                Can you change the wording in this message ‘The rehab referral Provider Number must be entered if the rehab referral Provider Type is entered’*/
			IF (NOT IsNull(ll_referring_provider_no) AND ll_referring_provider_no > 0) AND  (IsNull(ls_referring_provider_type_code) OR TRIM(ls_referring_provider_type_code) = '') THEN
				MessageBox('Missing  rehab referral Provider Type','The rehab referral Provider Type must be entered if the rehab referral Provider Number is entered..')
				idw_dw[2].SetFocus()
				RETURN -1
			END IF 

			/* 3.360	The rehab referral Provider Number must be entered if the external requestor Provider Type is entered.	
			               Can you change the wording in this message ‘The rehab referral Provider Number must be entered if the rehab referral Provider Type is entered’
							Can you change the wording in this message ‘The rehab referral Provider Number must be entered if the rehab referral Provider Type is entered’
			*/
			IF ( IsNull(ll_referring_provider_no) OR ll_referring_provider_no <= 0) AND  (NOT IsNull(ls_referring_provider_type_code) AND TRIM (ls_referring_provider_type_code) <> '' )THEN
				MessageBox('Missing  rehab referral Provider No','The rehab referral Provider Number must be entered if the rehab referral Provider Type is entered.	')
				idw_dw[2].SetFocus()
				RETURN -1
			END IF 
			
			/* 3.370	The rehab referral Provider Number must be a Provider for the Provider Type entered.	*/
			SetNull(ls_string2)
			SELECT 	provider_sub_type_code
			INTO 		:ls_string2
			FROM 		PROVIDER
			WHERE 	provider_no 				= :ll_referring_provider_no
			AND 			provider_type_code 	= :ls_referring_provider_type_code
			USING 		SQLCA;
			SQLCA.nf_handle_error('Embedded SQL: Select from PROVIDER ( for external)','n_task','nf_check_bus_rule') 
			
			IF IsNull(ls_string2) THEN
				MessageBox('Invalid Provider Information','The rehab referral Provider Number must be a Provider for the Provider Type entered.')
				idw_dw[2].SetColumn('referring_provider_no')
				idw_dw[2].SetFocus()
				RETURN -1
			END IF 
				
			/* 3.380	The rehab referral Requested Date must be entered if the external requestor (provider type and number) is entered.	*/
			IF IsNull(ldtm_referred_on_date) THEN
				MessageBox('Missing referred on Date','The referred on date must be filled in for an external requestor.')
				idw_dw[2].SetColumn('referred_on_date')
				idw_dw[2].SetFocus()
				RETURN -1
			END IF 
			
			/* 3.390	The rehab referral Requested Date must be greater than or equal to the accident date.*/
			IF ldtm_referred_on_date < ldtm_accident_date THEN
				MessageBox('Invalid referred on Date','The rehab referral Requested Date must be greater than or equal to the accident date.')
				idw_dw[2].SetFocus()
				RETURN -1
			END IF
					
			/* if there is a valid date then must enter type and numbber */
			IF NOT isnull(ldtm_referred_on_date) AND  (IsNull(ll_referring_provider_no) OR ll_referring_provider_no <= 0)  THEN
				MessageBox('Invalid referred on Date','The referred on date cannot entered if the provider is not.')
				idw_dw[2].SetFocus()
				RETURN -1
			END IF 	
			
			/* 3.395	The rehab referral Request Date must not be in the future. */
				IF date(ldtm_referred_on_date) > date(f_server_datetime()) THEN
				MessageBox('Invalid referred on Date','The rehab referral Request Date must not be in the future.')
				idw_dw[2].SetFocus()
				RETURN -1
			END IF
		END IF 
	END IF
END IF

/* if all of the BR checks pass check to see if we need to update the provider on the authorization record 
	The code is called in nf_set_identifiers() next to the updates and commits */

/* 3.663 (NEW) The service provider on the rehab authorization(s) for the task must be automatically updated to the same provider as the service provider
              on the task if the task service provider is entered or modified and there is no authorized provider on the rehab authorization and the 
			   authorized billable item is an item that is automatically created when the task is created.
				
	REVISED: 20120125
	3.663	The authorized provider on the rehab authorization must be automatically updated to the service provider on the task if there was no service provider on
	            the existing task and the following conditions apply to the rehab authorization:
				•	the billable item on the rehab authorization is an item that is automatically created when the task is created 
				•	there was no authorized provider on the rehab authorization 
			
				coded in nf_update_provider_on_authorization()				
*/

/* 3.665 (NEW) The authorized provider on the rehab authorizations(s) for the task must be automatically updated to the service provider on the task
              			if the service provider on the task is modified to a different provider and the following conditions are met:
							  
			a. The authorized provider on the rehab authorization was the same as then service provider on the task prior to the task being modified
			b. The authorized billable item has not been partially or fully paid (the paid amount is zero)
			
	REVISED: 20120125	
	3.665	The authorized provider on the rehab authorization must be automatically updated to the service provider on the task if  the service provider on the task can be modified 
				(see rule 3.660) and the following conditions apply to the rehab authorization: 
				•	the billable item on the rehab authorization is an item that is automatically created when the task is created  
				•	there was no authorized provider on the rehab authorization 
				•	the authorized billable item has not been partially or fully paid (the paid amount is zero)
						  
				coded in nf_update_provider_on_authorization()
*/

/*	fill in the authorization information for new entries only */
IF idw_dw[3].RowCount() = 1 THEN
	IF ib_authorization THEN
	ELSE
		idw_dw[3].DeleteRow(1)
	END IF
END IF

RETURN 0


end function

public function integer nf_check_mandatory ();INTEGER		li_result, li_row
STRING			ls_type_code, ls_sub_type_code, ls_specific_code, ls_comment_required_flag
STRING			ls_rehab_service_code, ls_rehab_program_code, ls_comment, ls_responsible_user_id
LONG			ll_count
DATE			ld_planned_start_date,  ld_planned_completion_date, ld_actual_start_date, ld_actual_completion_date, ld_requested_date
DATAWINDOWCHILD		ldwc_child


/* accept the data windows */
IF idw_dw[1].AcceptText() < 0 THEN RETURN -1
IF idw_dw[2].AcceptText() < 0 THEN RETURN -1
	
ib_authorization = TRUE
idec_qty 			= 1

//If related_follow_ups were canceled during the save process then
//the action item list will need to be refreshed to reflect this.
//This variable will be set to TRUE if some were canceled. 
ib_canceled_related_action_items 		= FALSE	
ib_rescheduled_related_action_items 	= FALSE

ls_type_code 						= idw_dw[1].GetItemString(1,'task_type_code')
ls_sub_type_code 					= idw_dw[1].GetItemString(1,'task_sub_type_code')
ls_specific_code 					= idw_dw[1].GetItemString(1,'task_specific_code')
ls_rehab_service_code			= idw_dw[1].GetItemString(1,'rehab_service_code')
ls_rehab_program_code			= idw_dw[1].GetItemString(1,'rehab_program_code')
ls_comment							= idw_dw[1].GetItemString(1,'comment')
ls_responsible_user_id			= idw_dw[1].GetItemString(1,'responsible_user_id')
ld_actual_start_date 				= Date(idw_dw[1].GetItemDateTime(1, 'actual_start_date'))
ld_actual_completion_date 	= Date(idw_dw[1].GetItemDateTime(1, 'actual_completion_date'))
ld_planned_start_date 			= Date(idw_dw[1].GetItemDateTime(1, 'planned_start_date'))
ld_planned_completion_date 	= Date(idw_dw[1].GetItemDateTime(1, 'planned_completion_date'))
ld_requested_date 				= Date(idw_dw[2].GetItemDateTime(1, 'referred_on_date'))


/* ck_REHAB_TASK_05 (datalength([comment])=datalength(rtrim(ltrim([comment])))) */
IF idw_dw[1].GetItemStatus(1,'comment', Primary!) <> notmodified! THEN
	idw_dw[1].setitem(1,'comment',trim(ls_comment))
END IF 

/* 3.10	The type of task must be entered (i.e. task type/sub-type/specific type). */
IF IsNull(ls_rehab_service_code) OR Trim(ls_rehab_service_code) = '' THEN
	MessageBox('Invalid Rehab Service','The Rehab Service must be entered.')
	idw_dw[1].SetFocus()
	RETURN -1
END IF

/* 3.110	The planned start date must be a valid date.	*/
IF ld_planned_start_date < Date('1900-01-01') OR ld_planned_start_date > Date('2079-06-06') THEN
	MessageBox('Invalid Date', "The planned start date is invalid.  Please enter a date between 01/01/1900 and 06/06/2079.", Exclamation!)
	idw_dw[1].SetFocus()
	idw_dw[1].SetColumn('planned_start_date')
	RETURN -1
END IF

/*	3.120	The revised planned start date, if entered, must be a valid date. [ REMOVED] */
/* 3.130	The planned completion date must be entered. */
/* 3.140	The planned completion date must be a valid date.	*/
IF ld_planned_completion_date < Date('1900-01-01') OR ld_planned_completion_date > Date('2079-06-06') THEN
	MessageBox('Invalid Date', "The planned completion date is invalid.  Please enter a date between 01/01/1900 and 06/06/2079.", Exclamation!)
	idw_dw[1].SetFocus()
	idw_dw[1].SetColumn('planned_completion_date')
	RETURN -1
END IF

/* 3.210	The actual start date, if entered, must be a valid date. */
IF NOT ISNULL(ld_actual_start_date) THEN
	IF ld_actual_start_date < Date('1900-01-01') OR ld_actual_start_date > Date('2079-06-06') THEN
		MessageBox('Invalid Date', "The actual start date is invalid.  Please enter a date between 01/01/1900 and 06/06/2079.", Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('actual_start_date')
		RETURN -1
	END IF
END IF 

/*	validate ACTUAL COMPLETION DATE  */
IF NOT ISNULL(ld_actual_completion_date) THEN
	/* 3.240	The actual completion date, if entered, must be a valid date. */
	IF ld_actual_completion_date < Date('1900-01-01') OR ld_actual_completion_date > Date('2079-06-06') THEN
		MessageBox('Invalid Date', "The actual completion date is invalid.  Please enter a date between 01/01/1900 and 06/06/2079.", Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('actual_completion_date')
		RETURN -1
	END IF
END IF 

/* requested date  --  PR 1279 */
IF ld_requested_date < Date('1900-01-01') OR ld_requested_date > Date('2079-06-06') THEN
	MessageBox('Invalid Date', "The requested date is invalid.  Please enter a date between 01/01/1900 and 06/06/2079.", Exclamation!)
	idw_dw[2].SetFocus()
	idw_dw[2].SetColumn('referred_on_date')
	RETURN -1
END IF

/* 3.35	The type of task (task type/sub-type/specific type) must be active for the rehab service 
            and the rehab program (i.e. Rehab_Service_Program_Task_Xref.active_flag = ‘Yes’)	
*/	
SELECT count(*) INTO :ll_count
FROM 	Task_Specific
WHERE task_type_code 		= :ls_type_code
AND 		task_sub_type_code 	= :ls_sub_type_code
AND 		task_specific_code 	= :ls_specific_code
AND 		active_flag 				= 'Y'
USING 	SQLCA;
SQLCA.nf_handle_error('n_task','nf_check_mandatory()','select task combinations')

IF ll_count = 0 or IsNull(ll_count) THEN
	IF ls_sub_type_code = '.' THEN
		MessageBox('Invalid Task Specific Type','The task sub type is required.')
		idw_dw[1].SetColumn('task_sub_type_code')
	ELSEIF ls_specific_code = '.' THEN
		MessageBox('Invalid Task Sub Type','The task specific type is required.')
		idw_dw[1].SetColumn('task_specific_code')
	END IF
END IF

/* 3.50	The Comment must be entered, if the type of task (task type/sub-type/specific type) 
	requires a comment (i.e. Task_Specific.comment_required_flag = ‘yes’ ). */
SELECT comment_required_flag
INTO 	:ls_comment_required_flag
FROM 	Task_Specific
WHERE task_type_code 		= :ls_type_code
AND 		task_sub_type_code 	= :ls_sub_type_code
AND 		task_specific_code 	= :ls_specific_code
USING 	SQLCA;

SQLCA.nf_handle_error('n_task','nf_check_mandatory()','select task combinations')
		
IF ls_comment_required_flag = 'Y' AND (Trim(ls_comment) = '' OR ISNULL(ls_comment)) THEN
	MessageBox('Invalid Comment','The comment is required for the task entered.')
	idw_dw[1].SetColumn('comment')
	idw_dw[1].SetFocus()
	RETURN -1
END IF

/* 3.100	The planned start date must be entered. */
IF IsNull(idw_dw[1].GetItemDateTime(1,'planned_start_date')) THEN
	MessageBox('Missing Planned Start Date','The planned start date is a required column and must be entered.')
	idw_dw[1].SetColumn('planned_start_date')
	idw_dw[1].SetFocus()
	Return -1
END IF

IF IsNull(idw_dw[1].GetItemDateTime(1,'planned_completion_date')) THEN
	MessageBox('Missing Planned Completion Date','The planned completion date is a required column and must be entered.')
	idw_dw[1].SetColumn('planned_completion_date')
	idw_dw[1].SetFocus()
	RETURN -1
END IF
	
/* 3.70	The task priority must be entered. [REMOVED]*/
/* 3.80	The task priority must be High or Low. [REMOVED]*/
	
/* 3.90	The task status must be entered. */
IF IsNull(idw_dw[1].GetItemString(1,'task_status_code')) OR Trim(idw_dw[1].GetItemString(1,'task_status_code')) = '' THEN
	MessageBox('Missing Task Status Code','The task status code is required.')
	idw_dw[1].SetColumn('task_status_code')
	idw_dw[1].SetFocus()
	RETURN -1
END IF

/* 3.60	The responsible person must be entered. */
IF IsNull(ls_responsible_user_id) OR Trim(ls_responsible_user_id) = '' THEN
	MessageBox('Missing Responsible Person','The responsible person is required.')
	idw_dw[1].SetColumn('responsible_user_id')
	idw_dw[1].SetFocus()
	RETURN -1
END IF

RETURN 0
end function

public function integer nf_insert_rehab_task_authorization ();/* 
	Automatically Created (Default) Authorizations
	When a task is created, the application will continue to automatically set up some default authorizations for billable items. 
	The authorizations will be automatically created based on the billable items that apply to the rehab task 
	– these are identified in the  Billable_Goods_And_Services_Rehab_Task table with an auto_create_authorization_flag = ‘Y’. 

	The Claim Manager must manually create the authorizations that are not automatically generated when creating a task. 	
	
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
	
*/
u_ds				lds_values_from_xref_table
INTEGER		li_xref_count, li_row, li_counter, li_countcheck
STRING			ls_service_code, ls_program_code, ls_task_type_code, ls_task_subtype_code, ls_task_specific_code
STRING			ls_fixed_fee_flag, ls_auto_create_authorization_flag, ls_provider_type_code, ls_user_id
LONG			ll_billable_xref_no, ll_provider_no, ll_billable_item_no, ll_no, ll_claim_no, ll_task_no
DATE			ldt_current_date
DECIMAL		ldec_default_authorized_qty, ldec_max_authorized_amount, ldec_authorized_amount, ldec_authorized_quantity


// THIS DATAWINDOW WILL BE USED TO SUPPLY THE DATA FOR OUR REHAB_TASK_AUTHORIZATION table.
IF  idw_dw[1].rowcount() < 1  THEN RETURN 0

/* grab the values we need from idw_dw[1] [ rehab task ] makes it easier for testing this way */
ls_service_code 				= idw_dw[1].getitemstring(1,'rehab_service_code' )
ls_program_code 			= idw_dw[1].getitemstring(1,'rehab_program_code' )
ls_task_type_code 		= idw_dw[1].getitemstring(1,'task_type_code' )
ls_task_subtype_code 	= idw_dw[1].getitemstring(1,'task_sub_type_code' )
ls_task_specific_code 	= idw_dw[1].getitemstring(1,'task_specific_code' )
ll_provider_no				= idw_dw[1].getitemnumber(1,'provider_no' )	
ls_provider_type_code	= idw_dw[1].getitemstring(1,'provider_type_code' )
ll_claim_no 					= idw_dw[1].getitemnumber(1,'claim_no' )	
ll_task_no 						= idw_dw[1].getitemnumber(1,'task_no' )	


/* there was a PR problem in which the user was able to create authorizations there were not allowed to */
//SELECT 	IsNull(count(*),0)
SELECT 	count(*)
INTO 		:li_countcheck
FROM 		REHAB_TASK_AUTHORIZATION
WHERE 	claim_no 	= :ll_claim_no
AND			task_no 	= :ll_task_no
USING 		SQLCA;
SQLCA.nf_handle_error('n_task','nf_insert_rehab_task_authorization()','SELECT count(*) ') 

IF li_countcheck > 0 THEN RETURN -1


/* retrieve the various authorizations we require */
lds_values_from_xref_table 					= CREATE u_ds
lds_values_from_xref_table.DataObject 	= 'd_billable_item_rehab_task_xref_insert'
lds_values_from_xref_table.SetTransObject(SQLCA)
li_xref_count = lds_values_from_xref_table.Retrieve(ls_service_code, ls_program_code, ls_task_type_code, ls_task_subtype_code, ls_task_specific_code  )
SQLCA.nf_handle_error('n_task','nf_insert_rehab_task_authorization()',' lds_values_from_xref_table.Retrieve') 


ldt_current_date = DATE(f_server_datetime())
f_user_id(ls_user_id)

IF ISNULL(li_xref_count) OR li_xref_count < 1  THEN RETURN 0

FOR li_counter = 1 TO li_xref_count
	
	ls_fixed_fee_flag 								= lds_values_from_xref_table.getitemstring(li_counter,'fixed_fee_flag')
	ll_billable_xref_no 								= lds_values_from_xref_table.getitemnumber(li_counter,'billable_xref_no')
	ls_auto_create_authorization_flag 		= 'Y' //as per document
	ldec_default_authorized_qty				= lds_values_from_xref_table.getitemdecimal(li_counter,'default_authorized_qty')
	ldec_max_authorized_amount				= lds_values_from_xref_table.getitemdecimal(li_counter,'max_authorized_amount')
	ll_billable_item_no								= lds_values_from_xref_table.getitemnumber(li_counter,'billable_item_no')
                         
	li_row = idw_dw[3].insertrow(0)
	idw_dw[3].SetItem(li_row,'billable_xref_no', ll_billable_xref_no)
	 
	// always set it to this..... as per discussion
	ldec_authorized_amount = ldec_max_authorized_amount
	
	idw_dw[3].SetItem(li_row,'claim_no', ll_claim_no)
	idw_dw[3].SetItem(li_row,'task_no', ll_task_no)
	
	ll_no = nf_get_next_authorization_no()
	IF ll_no > 0 THEN
		idw_dw[3].SetItem(li_row,'authorization_no',ll_no)
	ELSE
		RETURN -1
	END IF
	
	//This is checked by the Business rules as the values come the user selection
	IF ISNULL(ls_provider_type_code) 	THEN ls_provider_type_code 	= ''
	IF ISNULL(ll_provider_no) 				THEN ll_provider_no 				= 0
	
	idw_dw[3].SetItem(li_row,'paid_quantity',0)
	idw_dw[3].SetItem(li_row,'paid_amount',0)
	idw_dw[3].SetItem(li_row,'authorized_provider_type_code', ls_provider_type_code)
	idw_dw[3].SetItem(li_row,'authorized_provider_no', ll_provider_no)
	idw_dw[3].SetItem(li_row,'authorized_date',ldt_current_date)
	idw_dw[3].SetItem(li_row,'authorized_by_user_id',ls_user_id)
	idw_dw[3].SetItem(li_row,'authorization_comment','')
	idw_dw[3].SetItem(li_row,'expedited_billing_flag','N')
	idw_dw[3].SetItem(li_row,'fixed_fee_flag', ls_fixed_fee_flag)
	idw_dw[3].SetItem(li_row,'auto_created_flag', ls_auto_create_authorization_flag)
//	idw_dw[3].SetItem(li_row,'web_create_id',0)
//	idw_dw[3].SetItem(li_row,'web_modify_id',0)
	idw_dw[3].SetItem(li_row,'authorized_amount',ldec_authorized_amount)
	
//  (JP) Hi In the WorkBench, you should be able to rely on the database defaults to set the values for the web_create_id, web_modify_id, 
//    web_create_date and web_modify_date.
		
	/*
	Authorized quantity	If billable item is 173 (Physiotherapy Treatment) or 279 (Physiotherapy Treatment (in the home)
		Billable_Item_Rehab_Task.default_authorized_qty minus 1
	Else
		Billable_Item_Rehab_Task.default_authorized_qty

	** The reason for this is that the assessment includes the assessment and treatment so on the initial treatment set, 
	the number of treatments should be one less than the default number of treatments per set.
	
		173              S022               P001               TR             031 
		
		NOTE: Addition - 2012-01-04 remove the -1 until after the implementation date for ephysio
				
	*/
	IF (ls_program_code = 'P001' AND ls_task_type_code = 'TR' AND ls_task_subtype_code = '031' AND ls_service_code = 'S022'  AND (ll_billable_item_no = 173 OR ll_billable_item_no = 279)) AND (idt_ephysio_implentation_date < date(f_server_datetime() ) ) THEN
		idw_dw[3].SetItem(li_row,'authorized_quantity',ldec_default_authorized_qty - 1)
	ELSE
		idw_dw[3].SetItem(li_row,'authorized_quantity',ldec_default_authorized_qty)
	END IF 
	
NEXT

RETURN 1




end function

public function integer nf_insert_rehab_referral ();/*
	For a Rehab Referral set up for a Task:
	Create a Rehab Referral record with the following values:
	
	Task number							REHAB_TASK.task_no for the task
	Claim number							REHAB_TASK.claim_no for the task
	Rehab referral number			system generated
	Rehab service code				REHAB_TASK.rehab_service_code
	Referring Provider Type			value from the screen
	Referring Provider Number		value from the screen
	Document identifier				0 (zero)
	
*/
INTEGER			li_rowcount, li_row
LONG				ll_task_no, ll_claim_no, ll_rehab_referral_no, ll_provider_no
STRING				ls_service_code, ls_provider_type_code
DATE				ldt_current_date


ids_rehab_referral_insert 					= CREATE u_ds
ids_rehab_referral_insert.DataObject 	= 'd_rehab_referral_insert'
ids_rehab_referral_insert.SetTransObject(SQLCA)

li_row = ids_rehab_referral_insert.insertrow(0)

IF  idw_dw[1].rowcount() < 1  THEN RETURN 0

//grab the current date
ldt_current_date = DATE(f_server_datetime())

/* grab the values we need from idw_dw[1] [ rehab task ] makes it easier for testing this way */
ll_task_no 							= idw_dw[1].getitemnumber(1,'task_no' )
ll_claim_no 						= idw_dw[1].getitemnumber(1,'claim_no' )
ls_service_code 					= idw_dw[1].getitemstring(1,'rehab_service_code' )
ll_provider_no					= idw_dw[1].getitemnumber(1,'provider_no' )	
ls_provider_type_code		= idw_dw[1].getitemstring(1,'provider_type_code' )

/* We may want to switch this to use a datawindow so that we can add it to the datawindow array for updat purposes */
ids_rehab_referral_insert.SetItem(li_row,'rehab_referral_no',ll_rehab_referral_no)//RETRIEVE AND UPDATE LASTNOTABLE
ids_rehab_referral_insert.SetItem(li_row,'referred_on_date',ldt_current_date)
ids_rehab_referral_insert.SetItem(li_row,'rehab_service_code',ls_service_code)
ids_rehab_referral_insert.SetItem(li_row,'referring_provider_no', ls_provider_type_code)
ids_rehab_referral_insert.SetItem(li_row,'referring_provider_type_code', ll_provider_no)
ids_rehab_referral_insert.SetItem(li_row,'claim_no',ll_claim_no)
ids_rehab_referral_insert.SetItem(li_row,'task_no',ll_task_no)
ids_rehab_referral_insert.SetItem(li_row,'doc_id',0)

RETURN 1
end function

public function long nf_get_next_rehab_referral_no ();LONG	ll_referral_no

UPDATE Last_Rehab_Referral_No SET last_rehab_referral_no = last_rehab_referral_no + 1 USING SQLCA;
SQLCA.nf_handle_error("Embedded SQL: Update Last_Rehab_Referral_No","n_tasks","nf_get_next_rehab_referral_no")

CHOOSE CASE SQLCA.SQLNRows
	/*	If update was successful (ie. SQLNRows would equal 1), read back the identifier */	
	CASE 1
	
		SELECT Last_Rehab_Referral_No.last_rehab_referral_no INTO :ll_referral_no FROM Last_Rehab_Referral_No USING SQLCA;
		 SQLCA.nf_handle_error("Embedded SQL: Update Last_Rehab_Referral_No","n_tasks","nf_get_next_rehab_referral_no")
			
	CASE ELSE
		/*		if anything other than 1 record found, display error*/
		SQLCA.nf_rollback_transaction()
		IF SQLCA.SQLCode <> 0 THEN
			Error.Text 				= "Error during rollback of Last_Rehab_Referral_No in function nf_get_next_rehab_referral_no"
			Error.WindowMenu	=	"w_rehab"
			Error.Object				=	""
			Error.ObjectEvent		=	"nf_get_next_rehab_referral_no()"
			SignalError()
		END IF		
		MessageBox("Rehab Module - Data Integrity Error", string(SQLCA.SQLNRows) + " record(s) found in Last_Rehab_Referral_No~r~nPlease call the help desk",Exclamation!)
		RETURN -1

	END CHOOSE
		
RETURN ll_referral_no




end function

public function integer nf_set_task_sub_specific_filter (string as_service_code, string as_program_code);DATAWINDOWCHILD		ldwc_child
LONG							ll_row
STRING							ls_filter

ll_row = idw_dw[1].GetChild('task_xref_display', ldwc_child)
	
IF ll_row > 0 THEN
	
	ldwc_child.SetFilter("")
	ldwc_child.Filter()
	
	IF ib_new_task = TRUE THEN
		ls_filter = "rehab_service_code = '" + as_service_code + "' AND rehab_program_code = '" + as_program_code +	"' AND active_flag = 'Y'"
	ELSE
		ls_filter = 'rehab_service_code = "' + as_service_code + '" and rehab_program_code = "' + as_program_code + '"'
	END IF
ELSE
	RETURN 1
END IF

ldwc_child.SetFilter(ls_filter)
ldwc_child.SetSort(" task_type_desc A ,  task_sub_type_desc A, task_specific_desc A")
ldwc_child.Filter()
ldwc_child.Sort()

RETURN 0

end function

protected function integer nf_update_provider_on_authorization (long al_task_no, long al_claim_no, long al_provider_no, string as_provider_type_code, long al_old_provider_no, string as_old_provider_type);/*
	3.660	The service provider (type and number) on an existing task must not be modified to a different service provider if there is a rehab authorization 
	under that task that is authorized to the same provider as the task and the rehab authorization has been partially or fully paid (REHAB_TASK_AUTHORIZATION.paid_amount > 0).
	(i.e.  If any of the rehab authorizations for that provider are partially or fully paid, the service provider on the task must not be modified to a different provider.) 
	
	(A)
 	3.663 (NEW) The service provider on the rehab authorization(s) for the task must be automatically updated to the same provider as the service provider
              on the task if the task service provider is entered or modified and there is no authorized provider on the rehab authorization and the 
			   authorized billable item is an item that is automatically created when the task is created.
				
	REVISED: 20120125
	3.663	The authorized provider on the rehab authorization must be automatically updated to the service provider on the task if there was no service provider 
				on the existing task and the following conditions apply to the rehab authorization:
				•	the billable item on the rehab authorization is an item that is automatically created when the task is created 
				•	there was no authorized provider on the rehab authorization 

	
				
	(B)		
 	 3.665 (NEW) The authorized provider on the rehab authorizations(s) for the task must be automatically updated to the service provider on the task
              			if the service provider on the task is modified to a different provider and the following conditions are met:
							  
			a. The authorized provider on the rehab authorization was the same as the service provider on the task prior to the task provider being modified
			b. The authorized billable item has not been partially or fully paid (the paid amount is zero)
			
	REVISED: 20120125
	3.665	The authorized provider on the rehab authorization must be automatically updated to the service provider on the task if  the service provider on the task can be modified
				(see rule 3.660) and the following conditions apply to the rehab authorization: 
				•	the billable item on the rehab authorization is an item that is automatically created when the task is created  
				•	The authorized provider on the rehab authorization was the same as the service provider on the task prior to the task provider being modified
				•	the authorized billable item has not been partially or fully paid (the paid amount is zero)	
	
/**************************************************************************************************************************/	
	(A)/(B)/(C)
	REVISED: 20120404
			Add "and the authorized amount on the RTA = Max authorized amount for the RTA (from Billable_Item_Rehab_Task_Xref )
				
				
			Task 			Provider			Change Task Provider ?	Auth				Type					Provider			Paid		Change Auth Provider ?    COVERED BY
			-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
1			Physio		None				Yes								1					Assessment		None				No		= Task 						                  	A
																					2					Treatment			None				No		= Task											A 
																					3					Report				None				No		= Task											A											
																					4					Obus Form			Not = Task	No		No												N/A auto create flag = N
								
2			Physio		None				Yes								1					Assessment		None				No		= Task											A - same as a1
																					2					Treatment			Not = Task	No		No; warning message displayed		N/A  - should never get into this code
																					3					Report				None				No		= Task											A - same as a1
																					4					Obus Form			Not = Task	No		No												N/A auto create flag = N
								
3			Physio		Assigned	 	Yes								1					Assessment		= Task			No		= Task (revised)							B
																					2					Treatment			= Task			No		= Task (revised)							B	
																					3					Report				= Task			No		= Task (revised)							B
																					4					Obus Form			Not 	= Task	No		No												N/A auto create flag = N
								
4			Physio		Assigned	 	NO								1					Assessment		= Task			No		No												N/A - should never get into this code
																					2					Treatment			= Task			Paid		No												N/A  - should never get into this code
																					3					Report				= Task			No		No												N/A  - should never get into this code
																					4					Obus Form			Not = Task	No		No												N/A  - should never get into this code
								
5			Physio		Assigned	 	Yes								1					Assessment		= Task			No		= Task (revised)							B
																					2					Treatment			= Task			No		= Task (revised)							B	
																					3					Report				= Task			No		= Task (revised)							B
																					4					Obus Form			= Task			No		No												N/A auto create flag = N
								
6			Physio		None	 			Yes								1					Assessment		None				Paid		= Task (revised)							C
																					2					Treatment			None				No		= Task (revised)							A
																					3					Report				None				No		= Task (revised)							A
																					4					Obus Form			None				No		No												N/A auto create flag = N																																																																
*/

IF ((ISNULL(al_old_provider_no) OR al_old_provider_no = 0) AND al_provider_no > 0)  THEN 
	
	/* (A) */
	UPDATE  	a
	SET     		authorized_provider_type_code 	= :as_provider_type_code, 
					authorized_provider_no        		= :al_provider_no
	FROM 		REHAB_TASK_AUTHORIZATION a
						join Billable_Item_Rehab_Task_Xref b ON a.billable_xref_no = b.billable_xref_no
	WHERE   	b.auto_create_authorization_flag 	= 'Y'
	AND         a.authorized_provider_no 	        	= 0 
	AND			a.paid_amount 								= 0 //not really applcable just done for case study
	AND         a.authorized_amount				 		= b.max_authorized_amount //REVISED: 20120404
	AND			a.task_no        								= :al_task_no
	AND     		a.claim_no       								= :al_claim_no
	USING      sqlca;
	
	SQLCA.nf_handle_error("n_task","nf_update_provider_on_authorization()","UPDATE REHAB_TASK_AUTHORIZATION (A)")
	
	//    AND         a.authorized_amount				 = b.max_authorized_amount
	
	
	/* (C) */
	UPDATE  	a
	SET     		authorized_provider_type_code 	= :as_provider_type_code, 
					authorized_provider_no        		= :al_provider_no
	FROM 		REHAB_TASK_AUTHORIZATION a
						join Billable_Item_Rehab_Task_Xref b ON a.billable_xref_no = b.billable_xref_no
	WHERE   	b.auto_create_authorization_flag 	= 'Y'
	AND         a.authorized_provider_no 	        	= 0 
	AND			a.paid_amount 								> 0   //not really applcable just done for case study
	AND         a.authorized_amount				 		= b.max_authorized_amount //REVISED: 20120404
	AND			a.task_no        								= :al_task_no
	AND     		a.claim_no       								= :al_claim_no
	USING      sqlca;
	
	SQLCA.nf_handle_error("n_task","nf_update_provider_on_authorization()","UPDATE REHAB_TASK_AUTHORIZATION (B)")
	
ELSE // modification of existing provider_no
	
	/* (B) */
	UPDATE  	a
	SET     		authorized_provider_type_code 	= :as_provider_type_code, 
					authorized_provider_no        		= :al_provider_no
	FROM 		REHAB_TASK_AUTHORIZATION a
						join Billable_Item_Rehab_Task_Xref b ON a.billable_xref_no = b.billable_xref_no
	WHERE   	b.auto_create_authorization_flag 	= 'Y'
	AND         a.authorized_provider_no 	        	> 0 
	AND			a.paid_amount 								= 0
	AND         a.authorized_amount				 		= b.max_authorized_amount //REVISED: 20120404
	AND			a.task_no        								= :al_task_no
	AND     		a.claim_no       								= :al_claim_no
	AND         a.authorized_provider_type_code  	= :as_old_provider_type
	AND         a.authorized_provider_no             	= :al_old_provider_no
	USING      sqlca;
	
	SQLCA.nf_handle_error("n_task","nf_update_provider_on_authorization()","UPDATE REHAB_TASK_AUTHORIZATION (C)")
		
END IF 

RETURN 1
end function

public function boolean nf_check_if_report_type_exists (long al_claim_no, long al_task_no, string as_report_status_code);/*
(1) Scenario #1 •	there is a physio report for this task that is in draft mode (i.e. a PHYSIO_REPORT exists for this claim and task and the PHYSIO_REPORT.report_status_code = ‘D’)

return true if report type exists for claim and task and false if it doesn't 

*/

INTEGER		li_count

	
SELECT count(*) 
INTO 	:li_count
FROM 	REPORT_PHYSIO_MASTER 
WHERE claim_no 								= :al_claim_no
AND      task_no 								= :al_task_no
AND     physio_report_status_code 	= :as_report_status_code
USING SQLCA;
SQLCA.nf_handle_error("n_task","nf_check_if_report_type_exists()","SELECT count(*) ")

	
IF isnull(li_count) THEN li_count = 0

//record exists
IF li_count > 0 THEN RETURN TRUE

//nothing exists
RETURN FALSE
end function

public function boolean nf_check_client_discharged (long al_claim_no, long al_task_no);/*
(1) Scenario #1 •	the client has been discharged from the treatment program (i.e. a PHYSIO_REPORT exists for this claim and task and the PHYSIO_REPORT.report_type_code = ‘D’ 
                          and the PHYSIO_REPORT.report_status_code = ‘S’)

return true if the client has been discharged

*/

INTEGER		li_count

SELECT 	count(*) 
INTO 		:li_count
FROM 		REPORT_PHYSIO_MASTER  
WHERE 	claim_no 								= :al_claim_no
AND      	task_no 								= :al_task_no
AND     		physio_report_status_code 	= 'S'
AND			physio_report_type_code 		= 'D'
USING SQLCA;
SQLCA.nf_handle_error("n_task","nf_check_client_discharged()","SELECT count(*) ")
			
IF isnull(li_count) THEN li_count = 0

//record exists
IF li_count > 0 THEN RETURN TRUE

//nothing exists
RETURN FALSE



end function

public function date nf_get_treatment_discharge_date (long al_claim_no, long al_task_no);/*

*/

DATETIME		ldtm_treatment_discharge_date

SELECT 	treatment_discharge_date
INTO 		:ldtm_treatment_discharge_date
FROM 		REPORT_PHYSIO_MASTER  
WHERE 	claim_no 								= :al_claim_no
AND      	task_no 								= :al_task_no
AND     		physio_report_status_code 	= 'S'
AND			physio_report_type_code 		= 'D'
USING SQLCA;
SQLCA.nf_handle_error("n_task", "nf_get_treatment_discharge_date()", "SELECT treatment_discharge_date")
			
IF string(ldtm_treatment_discharge_date,'yyyy-mm-dd') = '1900-01-01' THEN 
	SETNULL(ldtm_treatment_discharge_date)
END IF 

//nothing exists
RETURN DATE(ldtm_treatment_discharge_date)
end function

public function boolean nf_client_appropriate_for_program (long al_claim_no, long al_task_no);/*
	the client is Not Appropriate for the program  (REPORT_PHYSIO_MASTER.report_type_code = ‘I’ 
	and the REPORT_PHYSIO_MASTER.report_status_code = ‘S’ 
	and the REPORT_PHYSIO_MASTER.appropriate_for_program_code = ‘N’)
*/
INTEGER li_count

SELECT 	count(*) 
INTO 		:li_count
FROM 		REPORT_PHYSIO_MASTER 
WHERE 	claim_no 									= :al_claim_no
AND      	task_no 									= :al_task_no
AND     		physio_report_status_code 		= 'S'
AND     		physio_report_type_code         	= 'I'
AND    		appropriate_for_program_code 	= 'N'
USING 		SQLCA;
SQLCA.nf_handle_error("n_task","nf_client_appropriate_for_program()","SELECT count(*) ")


//not appropriate
IF li_count > 0 THEN RETURN FALSE

//not applicable
RETURN TRUE
end function

public function date nf_get_initial_assessment_date (long al_claim_no, long al_task_no);/*
•	the client is Not Appropriate for the program  (REPORT_PHYSIO_MASTER.report_type_code = ‘I’ 
	and the REPORT_PHYSIO_MASTER.report_status_code = ‘S’ 
	and the REPORT_PHYSIO_MASTER.appropriate_for_program_code = ‘N’)
*/
DATE 		ldt_initial_assessment_date

SELECT 	reporting_date
INTO 		:ldt_initial_assessment_date
FROM 		REPORT_PHYSIO_MASTER 
WHERE 	claim_no 									= :al_claim_no
AND      	task_no 									= :al_task_no
AND     		physio_report_type_code         	= 'I'
USING 		SQLCA;
SQLCA.nf_handle_error("n_task","nf_get_initial_assessment_date","SELECT count(*) ")


//not appropriate
IF isnull(ldt_initial_assessment_date) THEN ldt_initial_assessment_date = date('1900-01-01')

//return the date
RETURN ldt_initial_assessment_date
end function

public function boolean nf_provider_approved_for_program (long al_provider_no, string as_provider_type, string as_program_code);/*
	the client is Not Appropriate for the program  (REPORT_PHYSIO_MASTER.report_type_code = ‘I’ 
	and the REPORT_PHYSIO_MASTER.report_status_code = ‘S’ 
	and the REPORT_PHYSIO_MASTER.appropriate_for_program_code = ‘N’)
	
	
			(if REHAB_TASK.rehab_service_code = ‘S022’ and the REHAB_TASK.rehab_program_code not = ‘P004’, and PROVIDER.physio_contract_flag =’Y’,  
		then the provider must exist in  Rehab_Program_Xref_Clinic.rehab_program_code for the REHAB_TASK.rehab_program_code )
				The rule should only be enforced if the provider is also under the Physio contract.  Currently, it enforces the rule even if the clinic is NOT under contract.
*/
INTEGER li_count

SELECT 	count(*)
INTO 		:li_count
FROM 		REHAB_PROGRAM_xref_PROVIDER 
WHERE 	rehab_program_code 	= :as_program_code
AND 			provider_no 					= :al_provider_no
AND 			active_flag 					= 'Y'
USING 		SQLCA;
SQLCA.nf_handle_error("n_task","nf_provider_approved_for_program()","SELECT count(*) ")


//not appropriate
IF li_count > 0 THEN RETURN TRUE

//not applicable
RETURN FALSE
end function

public function string nf_get_program_description (string as_program_code);/* returns the program descriotion from the program code */

STRING ls_program_description

SELECT 	rehab_program_desc_e
INTO 		:ls_program_description
FROM 		Rehab_Program
WHERE 	rehab_program_code = :as_program_code
USING 		SQLCA;
SQLCA.nf_handle_error("n_task","nf_get_program_description()","SELECT 	rehab_program_desc_e")


//not appropriate
IF isnull(ls_program_description) THEN ls_program_description = ''

//not applicable
RETURN ls_program_description
end function

public function string nf_get_physio_contract_flag (long al_provider_no, string as_provider_type);/*
			(if REHAB_TASK.rehab_service_code = ‘S022’ and the REHAB_TASK.rehab_program_code not = ‘P004’, and PROVIDER.physio_contract_flag =’Y’,  
		then the provider must exist in  Rehab_Program_Xref_Clinic.rehab_program_code for the REHAB_TASK.rehab_program_code )
				The rule should only be enforced if the provider is also under the Physio contract.  Currently, it enforces the rule even if the clinic is NOT under contract.
*/
STRING		ls_physio_contract_flag

SELECT 		physio_contract_flag
INTO 			:ls_physio_contract_flag
FROM 		PROVIDER
WHERE 		provider_no 			= :al_provider_no 
AND 			provider_type_code 	=:as_provider_type
USING 		SQLCA;
SQLCA.nf_handle_error("n_task","nf_get_physio_contract_flag()","SELECT physio_contract_flag ")

//not appropriate
IF isnull(ls_physio_contract_flag) THEN ls_physio_contract_flag = '' 

RETURN ls_physio_contract_flag
end function

public function string nf_get_ephysio_flag (long al_no, string as_type);
STRING			ls_ephysio_flag

SELECT 		ephysio_flag
INTO 			:ls_ephysio_flag
FROM 		PROVIDER
WHERE 		provider_no 				= :al_no
AND 			provider_type_code 		= :as_type
USING 		SQLCA;
SQLCA.nf_handle_error('n_task', 'nf_get_ephysio_flag()', 'SELECT ephysio_flag...') 

IF ISNULL(ls_ephysio_flag) THEN ls_ephysio_flag = ''

RETURN ls_ephysio_flag
end function

on n_task.create
call super::create
end on

on n_task.destroy
call super::destroy
end on

event constructor;call super::constructor;inv_action_item = CREATE n_action_item


idt_ephysio_implentation_date = Date(gdtm_ephysio_implementation_date)

if idt_ephysio_implentation_date = date('1900-01-01') THEN
	messagebox('Implementation Error - cmwb.ini file', 'The ephysio implementation date could not be resolved. Please contact the helpdesk before proceeding.')
	return -1
end if 


idt_ephysio_br_check_date = date(ProfileString(vgs_ini_filename,"ePhysio","New_Task_Rules_Date ",""))

if idt_ephysio_br_check_date = date('1900-01-01') THEN
	messagebox('Implementation Error -  cmwb.ini file', 'The ephysio Buisness rule date check could not be resolved. Please contact the helpdesk before proceeding.')
	return -1
end if 



 
end event

