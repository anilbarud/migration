$PBExportHeader$n_case_monitor.sru
$PBExportComments$Case Monitoring Business Rules
forward
global type n_case_monitor from n_pdc
end type
end forward

global type n_case_monitor from n_pdc
end type
global n_case_monitor n_case_monitor

type variables
LONG         	il_claim_no
u_dwa		idw_complication_list


end variables

forward prototypes
public function integer nf_check_bus_rule ()
public function integer nf_check_mandatory ()
public function integer nf_set_claim_no (long al_claim_no)
public function integer nf_set_unused_fields ()
public function integer nf_add_complication ()
public function integer nf_add_case_monitoring ()
public function integer nf_set_defaults (integer ai_datawindow, long al_current_row)
public function integer nf_protect_complication (boolean ab_protect)
public function integer nf_reset_complication ()
public function boolean nf_check_complication_exists ()
public function integer nf_set_complication_list (datawindow adw_complication_list)
public function integer nf_set_complication_trans ()
public function integer nf_log_events ()
public function integer nf_retrieve_case_monitor ()
public function integer nf_change_item (long al_datawindow, long al_row, string as_dw_column, string as_data)
end prototypes

public function integer nf_check_bus_rule ();
/* ----  VALIDATE if data entered for Case Monitoring ----
*/
/* Check if	New row but no data was entered
*/
	IF idw_dw[1].GetItemStatus(1,0,Primary!) = New! THEN
		idw_dw[1].DeleteRow(1)
		
	END IF


/* ----  VALIDATE if data entered for Complicating Factors ----
*/
/*	Check if	New row but no data was entered
*/
	IF idw_dw[2].GetItemStatus(1,0,Primary!) = New! THEN
		idw_dw[2].DeleteRow(1)
	ELSE
		/* check to see if the Complicating factor was entered; delete the datawindow
			if none was entered */
		IF idw_dw[2].GetRow() > 0 THEN
			IF isnull(idw_dw[2].GetItemString(1,'complicating_factor_code')) or &
		   	idw_dw[2].GetItemString(1,'complicating_factor_code') = '' THEN
				idw_dw[2].DeleteRow(1)
			ELSE
				/* Verify if the Complicating Factor already exists for the claim
				*/
				IF nf_check_complication_exists() THEN
					MEssageBox ("Complicating Factor", 'Complicating Factor entered already exists', StopSign!)
					Return -1
				END IF
			END IF
		END IF
	END IF


Return 0


end function

public function integer nf_check_mandatory ();STRING ls_case_mgmt_intensity,	ls_working_status,	ls_med_functional_status
DateTime ldtm_new_date,  ldtm_old_date

/* accept the data windows */
IF idw_dw[1].AcceptText() < 0 THEN Return -1

/* The following columns are mandatory:
	Case Management Intensity 
	Working Status
	Medical Functional Status
*/

ls_case_mgmt_intensity = idw_dw[1].GetItemString(1,'case_mgmt_intensity_code')
IF IsNull(ls_case_mgmt_intensity) OR Trim(ls_case_mgmt_intensity) = '' THEN
	MessageBox('Invalid Case Management Intensity Code','The Case Management Intensity is required.')
	idw_dw[1].SetColumn('case_mgmt_intensity_code')
	idw_dw[1].SetFocus()
	RETURN -1
END IF

ls_working_status = idw_dw[1].GetItemString(1,'working_status_code')
IF IsNull(ls_working_status) OR Trim(ls_working_status) = '' THEN
	MessageBox('Invalid Working Status Code','The Working Status is required.')
	idw_dw[1].SetColumn('working_status_code')
	idw_dw[1].SetFocus()
	RETURN -1
END IF

/* EDL  Begin Changes   Jun 16,1999
   checking if new_working_status_date is valid */

ldtm_new_date = idw_dw[1].GetItemDateTime(1,'work_status_date')
IF idw_dw[3].RowCount() > 0 then
	ldtm_old_date = idw_dw[3].GetItemDateTime(idw_dw[3].RowCount(),'new_working_status_date')
ELSE 
	ldtm_old_date = ldtm_new_date
END IF

IF IsNull(ldtm_new_date) or ldtm_new_date < ldtm_old_date THEN
	MessageBox('Invalid Work Status Date','Work Status Date is required and can not be < previous date')
	idw_dw[1].SetColumn('work_status_date')
	idw_dw[1].SetFocus()
	RETURN -1
END IF

/* EDL  End Changes   Jun 16,1999 */

ls_med_functional_status = idw_dw[1].GetItemString(1,'medical_functional_status_code')
IF IsNull(ls_med_functional_status) OR Trim(ls_med_functional_status) = '' THEN
	MessageBox('Invalid Medical Functional Status Code','The Medical Functional Status is required.')
	idw_dw[1].SetColumn('medical_functional_status_code')
	idw_dw[1].SetFocus()
	RETURN -1
END IF

/* EDL  Begin Changes   Jun 16,1999
   checking if new_medical_functional_status_date  is valid */

ldtm_new_date = idw_dw[1].GetItemDateTime(1,'medical_functional_status_date')
IF idw_dw[4].RowCount() > 0 then
	ldtm_old_date = idw_dw[4].GetItemDateTime(idw_dw[4].RowCount(),'new_medical_func_status_date')
ELSE 
	ldtm_old_date = ldtm_new_date
END IF
IF IsNull(ldtm_new_date) or ldtm_new_date < ldtm_old_date THEN
	MessageBox('Invalid Medical Functional Status Date','Medical Functional Status Date is required and can not be < previous date')
	idw_dw[1].SetColumn('medical_functional_status_date')
	idw_dw[1].SetFocus()
	RETURN -1
END IF

/* EDL  End Changes   Jun 16,1999 */


Return 0
end function

public function integer nf_set_claim_no (long al_claim_no);/* Set the claim number retrieved for the rehab plan */
	
il_claim_no = al_claim_no
 
Return 0
end function

public function integer nf_set_unused_fields ();
LONG  ll_row

/* Determine which fields are null and fill them in with a default value
 	at some point in time any of the following fields may be unused
*/


ll_row = idw_dw[1].GetRow()
IF ll_row > 0 THEN
   IF Isnull(idw_dw[1].GetItemString(ll_row,'employer_contact'))  or &
	   idw_dw[1].GetItemString(ll_row,'employer_contact') = '' THEN
		idw_dw[1].SetItem(ll_row, 'employer_contact', ' ')
	END IF
	
	string ls_it
	ls_it = idw_dw[1].GetItemString(ll_row,'case_mgmt_intensity_code')
	IF Isnull(idw_dw[1].GetItemString(ll_row,'case_mgmt_intensity_code')) or &
	   idw_dw[1].GetItemString(ll_row,'case_mgmt_intensity_code') = '' THEN
		idw_dw[1].SetItem(ll_row, 'case_mgmt_intensity_code', ' ')
	END IF
	
	IF Isnull(idw_dw[1].GetItemString(ll_row,'working_status_code')) or &
	   idw_dw[1].GetItemString(ll_row,'working_status_code') = '' THEN
		idw_dw[1].SetItem(ll_row, 'working_status_code', ' ')
	END IF
	
	IF Isnull(idw_dw[1].GetItemString(ll_row,'medical_functional_status_code')) or &
	   idw_dw[1].GetItemString(ll_row,'medical_functional_status_code') = '' THEN
		idw_dw[1].SetItem(ll_row, 'medical_functional_status_code', ' ')
	END IF
	
	
END IF

Return 0
end function

public function integer nf_add_complication ();long		ll_current_row

/* This function is used to add new complicating factors
*/
	idw_dw[2].Reset()
	ll_current_row = idw_dw[2].InsertRow(0)
	IF ll_current_row < 0 THEN
		Return -1
	END IF

/* Unprotect the Complicating Factor column
*/
	nf_protect_complication(false)
			
/* Set the defaults for a new Complicating Factor */
	IF nf_set_defaults(2,ll_current_row) < 0 THEN
		Return -1
	END IF

	return 0
end function

public function integer nf_add_case_monitoring ();long		ll_current_row

/* This function is used to add a Case Monitoring record for the claim
*/

	idw_dw[1].Reset()
	ll_current_row = idw_dw[1].InsertRow(0)
	IF ll_current_row < 0 THEN
		Return -1
	END IF
	
/* Set the defaults for a new Case Monitoring row */
	IF nf_set_defaults(1,ll_current_row) < 0 THEN
		Return -1
	END IF

	return 0
end function

public function integer nf_set_defaults (integer ai_datawindow, long al_current_row);

CHOOSE CASE ai_datawindow
		
	Case 1
	
	/* Set the Claim Number to the current claim for a new Case Monitoring 
	*/
	IF al_current_row > 0 THEN
		idw_dw[1].SetItem(al_current_row, "claim_no", il_claim_no)	
	END IF

	Case 2
	/* Set the claim number to the current claim for a new complicating factor
	*/
	IF al_current_row > 0 THEN
		idw_dw[2].SetItem(al_current_row, "claim_no", il_claim_no)	
	END IF

END CHOOSE

Return 0
end function

public function integer nf_protect_complication (boolean ab_protect);/* 
	This function protects/unprotects the Complicating Factor columns. Used to control whether the
	user may update the columns
*/

IF ab_protect THEN
	idw_dw[2].Modify("complicating_factor_code.protect=1")
ELSE
	idw_dw[2].Modify("complicating_factor_code.protect=0")
END IF


return 0
end function

public function integer nf_reset_complication ();/* On Cancel or SAVE,
	clear the new complicating factor datawindow 
*/
	IF idw_dw[2].GetRow() = 1 THEN
		idw_dw[2].DeleteRow(1)
	END IF
	
	return 0
		
end function

public function boolean nf_check_complication_exists ();Integer	li_row,	li_total_factors
STRING	ls_new_complicating_factor


/* Verify the new complicating factor entered does not already exists for the claim
*/

li_total_factors = idw_complication_list.RowCount()
IF li_total_factors > 0 THEN
	ls_new_complicating_Factor = idw_dw[2].GetItemString(1, 'complicating_factor_code')
	For li_row = 1 to li_total_factors 
		IF idw_complication_list.GetItemString(li_row, 'complicating_factor_code') = ls_new_complicating_factor THEN
			return true
		END IF
	NEXT	
END IF		

return false



end function

public function integer nf_set_complication_list (datawindow adw_complication_list);/* Set the instance for the Complicating Factor List */
	
idw_complication_list = adw_complication_list
 
Return 0
end function

public function integer nf_set_complication_trans ();/* Set the transaction object for the Complicating Factor datawindow */
	
idw_complication_list.SetTransObject(SQLCA)
 
Return 0
end function

public function integer nf_log_events ();N_EVENT_LOG	lnv_event_log     
LONG			ll_event_no,  ll_seq_no, ll_claim_no
STRING		ls_old_code, ls_new_code, ls_event_comment
DATETIME		ldtm_old_status_date, ldtm_new_status_date

lnv_event_log = Create n_event_log
ll_event_no = 0

/*	If the CASE MANAGEMENT INTENSITY has changed, log changes 
*/
	ls_old_code = idw_dw[1].GetItemString(1,'case_mgmt_intensity_code', Primary!, TRUE)
	ls_new_code = idw_dw[1].GetItemString(1,'case_mgmt_intensity_code')
	IF ls_old_code <> ls_new_code THEN
	   /*	Call the function to get the event number */
		ll_event_no = lnv_event_log.nf_next_claim_event_no(il_claim_no)
		IF ll_event_no > 0 THEN
	      /* Determine the Comments for the Event */
			CHOOSE CASE ls_old_code
				Case "H"  
					ls_event_comment = "Changed from High Intensity"
				Case "L"
					ls_event_comment = "Changed from Low Intensity"
				Case "M"
					ls_event_comment = "Changed from Medium Intensity"
			END CHOOSE
			
			IF lnv_event_log.nf_create_auto_event(il_claim_no, ll_event_no,'018', ls_event_comment,	ls_new_code) < 0 THEN 
				Return -1
   			END IF
		END IF
	END IF

/*	If the WORKING STATUS CODE has changed, log changes 
*/
	ls_old_code = idw_dw[1].GetItemString(1,'working_status_code', Primary!, TRUE)
	ls_new_code = idw_dw[1].GetItemString(1,'working_status_code')
	IF Trim(ls_old_code) <> Trim(ls_new_code) THEN
		/*	Call the function to get the event number */
  		ll_event_no = lnv_event_log.nf_next_claim_event_no(il_claim_no)
		/* create log entry in CLAIM EVENT table */
		IF ll_event_no > 0 THEN
			IF lnv_event_log.nf_create_auto_event(il_claim_no, ll_event_no,'019','changed from ' 	+ ls_old_code ,' ') < 0 THEN 
				Return -1
	   		END IF
		END IF
		/*
		Create history entry in WORKING_STATUS_CHANGE table
		*/                          
		ldtm_old_status_date = idw_dw[1].GetItemDateTime(1, 'work_status_date', Primary!, TRUE)
		ldtm_new_status_date = idw_dw[1].GetItemDateTime(1, 'work_status_date')
		
		INSERT INTO WORKING_STATUS_CHANGE
			(claim_no, event_no, old_working_status_date, new_working_status_date,
			 old_working_status_code, new_working_status_code)
		VALUES (:il_claim_no, :ll_event_no, :ldtm_old_status_date, :ldtm_new_status_date, :ls_old_code, :ls_new_code)
		USING SQLCA;
	
		SQLCA.nf_handle_error('n_case_monitor', 'Embedded SQL:  Error inserting into', 'nf_log_events')
				
		IF SQLCA.SQLNRows <> 1 THEN
			SQLCA.nf_rollback_transaction()
		END IF
	END IF
		
/* If the MEDICAL FUNCTIONAL STATUS CODE has changed, log changes 
*/
	ls_old_code 					= idw_dw[1].GetItemString(1,'medical_functional_status_code', Primary!, TRUE)
	ls_new_code 				= idw_dw[1].GetItemString(1,'medical_functional_status_code')
	ldtm_old_status_date 	= idw_dw[1].GetItemDateTime(1, 'medical_functional_status_date', Primary!, TRUE)
	ldtm_new_status_date 	= idw_dw[1].GetItemDateTime(1, 'medical_functional_status_date')
	/* Retrieve highest sequence no. currently in MEDICAL_FUNC_STATUS_CHANGE table
	*/
	ll_claim_no = idw_dw[1].GetItemNumber(1, 'claim_no')
	
	SELECT Max(seq_no) INTO :ll_seq_no FROM MEDICAL_FUNC_STATUS_CHANGE WHERE claim_no = :ll_claim_no;

	SQLCA.nf_handle_error('n_case_monitor', 'Embedded SQL:  Error retrieving sequence number', 'nf_log_events')
		
	/*  Assign value to history table sequence no field
	*/
	IF IsNull(ll_seq_no) THEN
		ll_seq_no = 1  //if no pre-existing records -> set initial seq_no value to 1
	ELSE
		ll_seq_no = ll_seq_no + 1	//pre-existing records present (ie seq_no<>NULL) -> increase seq_no by 1
	END IF
	
	/*	NOTE:  Medical Status Code changes are not recorded in the CLAIM EVENT table
	*/
	IF Trim(ls_old_code) <> Trim(ls_new_code) THEN
	
		INSERT INTO MEDICAL_FUNC_STATUS_CHANGE(claim_no, seq_no, old_medical_func_status_date,
		new_medical_func_status_date, old_medical_func_status_code, new_medical_func_status_code)
		VALUES (:il_claim_no, :ll_seq_no, :ldtm_old_status_date, :ldtm_new_status_date, :ls_old_code, :ls_new_code)
		USING SQLCA;
	
		SQLCA.nf_handle_error('n_case_monitor', 'Embedded SQL:  Error inserting into MEDICAL_FUNC_STATUS_CHANGE','nf_log_events')
		
		IF SQLCA.SQLNRows <> 1 THEN
			SQLCA.nf_rollback_transaction()
		END IF		
	END IF

Return 0


  
end function

public function integer nf_retrieve_case_monitor ();/* Re-Retrieve the Case Monitoring Details */
idw_dw[1].Retrieve(il_claim_no)
SQLCA.nf_handle_error('Retrieve of dw','n_case_monitor','nf_retrieve_case_monitor') 

return 0
end function

public function integer nf_change_item (long al_datawindow, long al_row, string as_dw_column, string as_data);/*	This function runs the code for the itemchanged event for the appropriate datawindow.
*/

CHOOSE CASE	al_datawindow
	CASE	1
  		CHOOSE CASE as_dw_column			
			CASE 'working_status_code'
				idw_dw[1].SetItem(1, 'work_status_date', Date(f_server_datetime()))
				/* enable work_status_date so user can modify
				*/
				idw_dw[1].Object.work_status_date.Protect = FALSE
				idw_dw[1].Object.work_status_date.Background.Color = RGB(255,255,255) /* White */
				
			CASE 'medical_functional_status_code'
				idw_dw[1].SetItem(1, 'medical_functional_status_date', Date(f_server_datetime()))
				/* enable medical_functional_status_date so user can modify
				*/
				idw_dw[1].Object.medical_functional_status_date.Protect = FALSE
				idw_dw[1].Object.medical_functional_status_date.Background.Color = RGB(255,255,255) /* White */
		END CHOOSE
END CHOOSE
	
RETURN 0
end function

on n_case_monitor.create
call super::create
end on

on n_case_monitor.destroy
call super::destroy
end on

