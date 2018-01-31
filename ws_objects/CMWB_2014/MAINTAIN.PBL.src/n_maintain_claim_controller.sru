$PBExportHeader$n_maintain_claim_controller.sru
$PBExportComments$controller for maintaining claims - links n_claims and n_openings
forward
global type n_maintain_claim_controller from n_pdc
end type
end forward

global type n_maintain_claim_controller from n_pdc
end type
global n_maintain_claim_controller n_maintain_claim_controller

type variables
N_CLAIMS inv_claims
N_OPENINGS inv_openings
n_action_item		inv_action_item
N_BR_BLUECROSS		inv_bc
N_REMINDERS			inv_reminders
N_REFERRAL       inv_referral
n_cst_remote_print    inv_remote_print
BOOLEAN ib_admin_region_change
STRING is_column_name

STRING is_app = 'CMWB'
INTEGER ii_module = 008
INTEGER ii_reason[]
N_MAIL inv_email
s_email_setup istr_email_setup
Datawindowchild idwc_sob

end variables

forward prototypes
public function integer nf_check_mandatory ()
public function integer nf_change_item (long al_datawindow)
public function integer nf_log_events ()
public function long nf_get_claim_no ()
public function integer nf_insert (long al_row)
public subroutine nf_status_changed (long al_datawindow)
public function integer nf_retrieve (long al_claim_no)
public function integer nf_set_unused_fields ()
public function long nf_set_identifiers ()
public subroutine nf_init ()
public function integer nf_check_bus_rule ()
public function string nf_get_claim_role_code ()
public function integer nf_create_firefighter (long al_rejected_wca_claim_no, long al_individual_no, ref long al_new_fca_claim_no)
public function integer nf_12week_letter (long al_claim_no)
end prototypes

public function integer nf_check_mandatory ();/*	call the check mandatory functions in all the children
*/

	IF inv_openings.nf_check_open_type('VR') > 0 THEN
   	inv_claims.ib_rehab_officer_needed = TRUE
	ELSE
   	inv_claims.ib_rehab_officer_needed = FALSE
	END IF
	IF inv_openings.nf_check_open_type('RLOE') > 0 THEN
   	inv_claims.ib_rloe_opening = TRUE
	ELSE
   	inv_claims.ib_rloe_opening = FALSE
	END IF


	IF inv_claims.nf_check_mandatory() < 0 THEN Return -1

/*	set the openings status variable
*/
	inv_openings.is_status_code = idw_dw[1].GetItemstring(1,'claim_status_code')
	inv_openings.is_status_type_code = idw_dw[1].GetItemstring(1,'claim_status_type_code')

	IF inv_openings.nf_check_mandatory() < 0 THEN Return -1
	inv_claims.is_opening_type_code = inv_openings.nf_pen_or_sv_open()
/*	if opening of sv or pen then there must be a legislation code
*/
	IF inv_claims.is_opening_type_code <> '' and Trim(idw_dw[1].GetItemString(1,'legislation_code')) = '' THEN
		MessageBox('Missing Legislation Code','The legislation code must be filled in for pen or sv  or s1 or s2 openings')
		Return -1
	END IF

Return 0
end function

public function integer nf_change_item (long al_datawindow);LONG	ll_return

/*	PURPOSE :
	get the datawindow that triggered the event  - al_datawindow
	this contains the datawindow as it is known by this object
	call the function in the appropriate child and pass 
	it the appropriate datawindow number
*/

	CHOOSE CASE al_datawindow
	   CASE 1
/*		claim create dw
*/
   	   ll_return = inv_claims.nf_change_item(1)
			Return ll_return
	   CASE 2
/*		individual main dw
*/
   	   inv_claims.nf_change_item(2)
	   CASE 3
/*		individual main name dw
		set the accident date from the claim dw
*/
   	   inv_openings.idt_accident_date = idw_dw[1].GetItemDateTime(1, 'accident_date')
      	inv_openings.nf_change_item(1, is_column_name)
	   CASE 9
/*		accident stats shared datawindow with claim create dw
*/
   	   ll_return = inv_claims.nf_change_item(9)
			Return ll_return

	   CASE 10
/*		accident stats shared datawindow with claim create dw
*/
   	   ll_return = inv_claims.nf_change_item(10)
			Return ll_return
			
	   CASE 11
/*     claim reminder
*/
		ll_return = inv_reminders.nf_change_item(11)
		Return ll_return
		
	  CASE ELSE 
   	   Return -1		
	END CHOOSE

Return 0
end function

public function integer nf_log_events ();/*	log the creation of the claim event
*/

	IF idw_dw[1].GetItemString(idw_dw[1].GetRow(), 'admin_region_code', Primary!, TRUE) <> idw_dw[1].GetItemString(idw_dw[1].GetRow(), 'admin_region_code') THEN
/*	get this first since the update resets the status flags - check to see if original value changed
*/
   	ib_admin_region_change = TRUE
	ELSE
   	ib_admin_region_change = FALSE
	END IF

Return inv_claims.nf_log_events()

end function

public function long nf_get_claim_no ();LONG  ll_row

/*	this function gets the claim number from the first data window
	it is only to be used after nf_get_next_identifier has been called
	it will be called by the parent window to pass the claim numbe back to 
	its parent
*/

	ll_row = idw_dw[1].GetRow()
Return idw_dw[1].GetItemNumber(ll_row,'claim_no')
end function

public function integer nf_insert (long al_row);/*
	insert a row  - in the maintain only openings can be inserted
	claims can only be created in the claim create
*/


/*	cannot insert into the openings unless the status  and type are the following:
	Active   any type
	Finalled  Final
	Finalled First and Final
	send the status to openings so all the checks can be done there
*/
	inv_openings.il_claim_no = idw_dw[1].GetItemNumber(1, 'claim_no')
	inv_openings.is_status_code = idw_dw[1].GetItemString(1, 'claim_status_code')
	inv_openings.is_status_type_code = idw_dw[1].GetItemString(1, 'claim_status_type_code')
	inv_openings.idt_accident_date = idw_dw[1].GetItemDateTime(1,'accident_date')

Return inv_openings.nf_insert(al_row) 

end function

public subroutine nf_status_changed (long al_datawindow);inv_claims.nf_status_changed(al_datawindow)
end subroutine

public function integer nf_retrieve (long al_claim_no);LONG					ll_rows, ll_rtn, ll_num_rows, ll_rowcount, ll_filter_count
STRING				ls_type, ls_pob, ls_sob_filter
DATAWINDOWCHILD	ldwc_child

/*	dw 1 = claim - arguement claim no
	dw 3 = openings - claim no
	dw 4 = difficult claim
	dw 11 = accident stat
*/
	inv_claims.nf_retrieve(al_claim_no)
//	inv_reminders.nf_retrieve(al_claim_no)
	inv_openings.nf_retrieve(al_claim_no)
	inv_openings.idt_accident_date = idw_dw[1].GetItemDateTime(1,'accident_date')
	inv_openings.il_claim_no = al_claim_no

/*	set the filter on legislation code on the claim dw to the ones valid for the opening type
*/
	ls_type = inv_openings.nf_pen_or_sv_open()
	idw_dw[1].GetChild('legislation_code', ldwc_child)
	IF ls_type = 'SV' OR ls_type = 'PEN' OR ls_type = 'S1' OR ls_type = 'S2' THEN
		ldwc_child.SetFilter('opening_type_code = "'+ ls_type + '"')
	END IF
	ldwc_child.Filter()
	
/* Retrieve Reminders
*/
	idw_dw[14].Retrieve(al_claim_no)
	SQLCA.nf_handle_error('retrieve child','n_claims', 'nf_retrieve')

/*	Retrieve difficult claim
*/
	ll_rows = idw_dw[4].Retrieve(al_claim_no)
	SQLCA.nf_handle_error('Retrieve difficult claim', 'n_maintain_claim_controller', 'nf_retrieve')
	
	IF ll_rows = 0 THEN
/*	no difficult claim - insert a row for entry
*/
		IF idw_dw[4].InsertRow(0) < 1 THEN
			Return -1 
		ELSE
			idw_dw[4].SetItem(1,'claim_no', al_claim_no)
		END IF
	ELSE
		if idw_dw[4].getitemstring(1,"fatality_flag") = 'Y' then
			Select Count(*) 
			INTO :ll_rows
			FROM PAYMENT 
			WHERE claim_no = :al_claim_no
			AND payment_type_code = '18';
			SQLCA.nf_handle_error('n_maintain_claim_controller','embedded SQL: Select Count(*) FROM PAYMENT...','nf_retrieve(al_claim_no)')
			
			IF ll_rows > 0 THEN
				idw_dw[4].Modify("fatality_flag.Protect='1'")
			END IF
		end if
	END IF
	
    /* Side of Body */
	//Set the side

	idw_dw[11].Retrieve(al_claim_no)
	SQLCA.nf_handle_error('n_claim_controller', 'nf_retrieve','idw_dw[11].Retrieve(al_claim_no)')
				
	//filter the side of body drop down.
	ll_rtn = idw_dw[11].GetChild("side_of_body_code",idwc_sob)
	ll_num_rows = idwc_sob.RowCount()
	
	ls_pob = idw_dw[11].Getitemstring(1,"part_of_body_code")
	ls_sob_filter = 'part_of_body_code = "' + ls_pob + '" and active_flag = "Y"'
	idwc_sob.SetFilter(ls_sob_filter)
	idwc_sob.SetRedraw(false)
	idwc_sob.Filter()
	idwc_sob.SetRedraw(true)
	idwc_sob.SetSort("#2 A")
	idwc_sob.Sort()
	ll_filter_count = idwc_sob.FilteredCount()

ll_num_rows = idwc_sob.RowCount()

Return 1
end function

public function integer nf_set_unused_fields ();
	IF inv_claims.nf_set_unused_fields() < 0 THEN Return -1
	IF inv_openings.nf_set_unused_fields() < 0 THEN Return -1

Return 0

end function

public function long nf_set_identifiers ();LONG 			ll_claim_no, ll_opening_no, ll_row
STRING		ls_recurrence_type_code
STRING		ls_opening_type_code
STRING		ls_responsible_user_id
DATETIME		ldt_benefit_end_date 
DATETIME		ldt_disablement_date
LONG			ll_opening_row
INTEGER		li_rtn

	ll_claim_no = inv_claims.nf_get_next_identifier()   // this will return the claim number on the dw if one
	inv_openings.il_claim_no = ll_claim_no

	IF ll_claim_no > 0 THEN
/*	set the claim number on the main claim dw
*/
/* Removed for PR  11435 - Claim No Update this is not required
		ll_row = idw_dw[1].GetRow()
	   	IF ll_row > 0 THEN
   	   		idw_dw[1].SetItem(ll_row, 'claim_no', ll_claim_no)
	   	ELSE
   	   		MessageBox("Error", "Error setting claim number.")
      		Return -1
	   	END IF
End of PR 
*/

/*		the openings - need to loop  - also set the opening no
*/
		ll_row = idw_dw[3].RowCount()
		ll_opening_no = 0					// initialize incase there is more than one opening entered
   	ll_opening_row = 1
		ls_responsible_user_id = idw_dw[1].GetItemString(1,'claim_manager_user_id')
	   DO WHILE ll_opening_row <= ll_row
			IF idw_dw[3].GetItemStatus(ll_opening_row,0,PRIMARY!) <> NotModified! THEN
				
				idw_dw[3].SetItem(ll_opening_row, 'claim_no', ll_claim_no)
	/*			determine if a new entry and if so call function to get next number
	*/
				IF idw_dw[3].GetItemStatus(ll_opening_row,0,PRIMARY!) = NewModified! THEN
	/*			only call this function once per save - if more than one opening entered at a time
				the max will always be the same, so increment the ll_opening_no
	*/
					IF ll_opening_no = 0 THEN
						ll_opening_no = inv_openings.nf_get_next_identifier(ll_claim_no)
					ELSE
						ll_opening_no = ll_opening_no + 1
					END IF
		         IF ll_opening_no > 0 THEN
	   	         idw_dw[3].SetItem(ll_opening_row, 'opening_no', ll_opening_no)
	      	   ELSE
	         	   MessageBox('Error', 'Error getting the next opening number.')
	            	Return -1
		         END IF
					
					//items should be added.	
					If idw_dw[1].GetItemString(1,'case_managed_flag') = 'Y' Then	
						//The claim number for inv_action_item has already been set in the 
						//nf_check_bus_rule() function
						
						ldt_disablement_date = idw_dw[3].GetItemDateTime(ll_opening_row,'benefit_start_date')					
						ls_opening_type_code = idw_dw[3].GetItemString(ll_opening_row ,'opening_type_code')
						ldt_benefit_end_date = idw_dw[3].GetItemDateTime(ll_opening_row,'benefit_end_date')
						ls_recurrence_type_code = idw_dw[3].GetItemString(ll_opening_row,'recurrence_type_code')
						
						//Make sure this is a valid type of Opening to add the intial 3 action item too.
						If ls_opening_type_code = 'RLOE' and ls_recurrence_type_code = 'R' and IsNull(ldt_benefit_end_date) Then
							//This is a new opening so create the action items for this opening
						
							li_rtn = inv_action_item.nf_create_initial_action_items(ll_opening_no,ldt_disablement_date,ls_responsible_user_id ) 
							
						End if
					eND IF
				END IF
			END IF
      	ll_opening_row = ll_opening_row + 1
	   LOOP
		
		If idw_dw[1].GetItemString(1,'case_managed_flag') = 'Y' Then
			if idw_dw[1].GetITemStatus(1,'case_managed_flag',Primary!) = DataModified! Then
				SetNull(ldt_disablement_date)
								
				li_rtn = inv_action_item.nf_create_initial_action_items(0,ldt_disablement_date,ls_responsible_user_id ) 
			End if
		End if		
	ELSE
   	MessageBox("Error", "Error setting identifiers number.")
	   Return -1
	END IF
	
	//I'm calling the update of the action item object here because it can create claim events
	//if the update is not called before the claim logs event (status change) then
	//the tow objects will select the same event no and a pk contraint error will occur.
	inv_action_item.nf_update()
	
Return 0
end function

public subroutine nf_init ();U_DWA ldw_dw[]


/* create all the objects that need controlling from here
*/
	inv_claims = 		Create n_claims
	inv_action_item = CREATE n_action_item
	inv_openings = 	Create n_openings
	inv_bc		 =		Create n_br_bluecross
	inv_reminders =	Create n_reminders
	inv_referral    =    Create n_referral
	
/*	register the parent and the datawindows
*/
	ldw_dw[1] = idw_dw[1]															// claim dw
	ldw_dw[2] = idw_dw[2]															// claim no dw
	
	ldw_dw[3] = idw_dw[5]															//dw_eligible_report_fee
	ldw_dw[4] = idw_dw[6]															//dw_payment
	ldw_dw[5] = idw_dw[7]															//dw_unapplied_claim_txn
	ldw_dw[6] = idw_dw[8]															//dw_deleted_payment
	ldw_dw[7] = idw_dw[9]															//dw_deleted_claim
	
	/* New P10151-7 */
	ldw_dw[8] = idw_dw[10]    //dw_override_main
	ldw_dw[9] = idw_dw[11]    //dw_accident_stat
	ldw_dw[10] = idw_dw[12]  //dw_pension
	ldw_dw[11] = idw_dw[14]  //dw_claim_reminder
	
	/* New P10281 - Firefighters' Compensation Act */
	ldw_dw[12] = idw_dw[16]  //dw_firefighter_claim
	ldw_dw[13] = idw_dw[17]  //dw_firefighter_claim_participant
	ldw_dw[14] = idw_dw[18]  //dw_firefighter_claim_accident
	ldw_dw[15] = idw_dw[19]  //dw_rejected_WCA_claim_events
	ldw_dw[16] = idw_dw[20]  //dw_rejected_WCA_difficult_claim
	
	inv_claims.nf_init(ldw_dw[], SQLCA, iwi_window_parent)
//	inv_reminders.nf_init(ldw_dw[], SQLCA, iwi_window_parent)				// reminders dw

	ldw_dw[1] = idw_dw[3]															// openings dw
	inv_openings.nf_init(ldw_dw[], SQLCA, iwi_window_parent)
	
/*	set up the commit flag to only allow this controller to commit
*/
	nf_set_commit(TRUE)
	inv_claims.nf_set_commit(FALSE)
	inv_openings.nf_set_commit(FALSE)
	inv_action_item.nf_set_commit(False)
//	inv_reminders.nf_set_commit(FALSE)
	
Return
end subroutine

public function integer nf_check_bus_rule ();LONG			ll_return_value, ll_no, ll_opened, ll_future, ll_max, ll_related_action_items
LONG			ll_claim_no, ll_opening_no, ll_opening_row, ll_new_opening_no, ll_cntr, ll_referral_no, ll_number, ll_docid, ll_direct_referral_no
STRING 		ls_string, ls_string2, ls_opening_type_code, ls_message, ls_recurrence_type_code
DATETIME		ldtm_date, ldtm_check, ldtm_next_review_due, ldtm_review_due
DATETIME    ldt_benefit_end_date, ldt_disablement_date, ldtm_acc_rec_date, ldtm_today
DATE			ldt_annual_ben_review_cmpltd, ldt_annual_ben_review_due
INTEGER		li_rtn, li_action_items_canceled, li_action_items_reassigned, li_cntr, li_array, li_reason[],  li_task_no, li_check_request, li_return, li_existing_physio_task
BOOLEAN		lb_related_action_items_exist, lb_save, lb_active
//EPAY II
LONG		ll_override_row, ll_return, ll_row, ll_foe_count, ll_fb_count, ll_rtn, ll_loop
DATETIME	ldtm_override_term_date, ldtm_accident_date, ldtm_accdate_comp
DATE 		ldt_accident_date	
BOOLEAN  lb_eligible, lb_statuschanged, lb_display_message = FALSE, lb_accident_date_change, lb_foe_sent, lb_foe_chg
STRING   ls_claim_status_code, ls_claim_status_type, ls_orig_claim_status_code, ls_orig_claim_status_type_code, ls_flag

STRING   ls_service_code, ls_program_code, ls_task_type_code, ls_task_subtype_code

ii_reason = li_reason

ldtm_today = f_server_datetime()

//Fill some variables
ll_claim_no = idw_dw[1].GetITemNumber(1,'claim_no')
inv_action_item.nf_set_claim_no(ll_claim_no)

/*	need to return what the lower functions returned - since the check for 
	individual returns
	other numbers besides -1 for an error
*/


   inv_openings.idt_accident_date = idw_dw[1].GetItemDateTime(1, 'accident_date')

	ll_max = idw_dw[3].RowCount()
	ll_no = 1
	IF ll_max > 0 THEN
		inv_claims.idtm_min_accident_recurrence = idw_dw[3].GetItemDateTime(1,'accident_recurrence_date')
	ELSE
		SetNull(inv_claims.idtm_min_accident_recurrence)
	END IF
	DO WHILE ll_no <= ll_max
		IF idw_dw[3].GetItemDateTime(ll_no,'accident_recurrence_date') < inv_claims.idtm_min_accident_recurrence THEN
			inv_claims.idtm_min_accident_recurrence = idw_dw[3].GetItemDateTime(ll_no,'accident_recurrence_date')
		END IF
		ll_no = ll_no + 1
	LOOP

	ll_return_value = inv_claims.nf_check_bus_rule()
	IF ll_return_value < 0 THEN Return ll_return_value

	ll_return_value = inv_openings.nf_check_bus_rule()
	IF  ll_return_value < 0 THEN Return ll_return_value

	
/***************************** START EPAY II ***************************************************/  	
	
//EPAY II check the accident date against the override termination date.
	
//Get Accident Date
ldtm_accident_date = idw_dw[1].getitemdatetime(idw_dw[1].getrow(),'accident_date')

// Check for an override & validate

ll_override_row = idw_dw[10].getrow()
IF ll_override_row > 0 THEN
	ldtm_override_term_date = idw_dw[10].getitemdatetime(ll_override_row,"eligibility_end_date")
END IF	

IF ll_override_row > 0 THEN
	IF DATE(ldtm_override_term_date) < DATE(ldtm_accident_date) THEN
		MessageBox('Invalid Date','The Override Termination Date cannot be before the Accident Date.',information!)
		idw_dw[1].SetFocus()
		RETURN -1
	END IF
END IF

// Check to see if this claim is registered.
ll_row = idw_dw[1].getrow()

ls_claim_status_code 			 = idw_dw[1].GetItemString(ll_row,'claim_status_code')
ls_claim_status_type 			 = idw_dw[1].GetItemString(ll_row,'claim_status_type_code')
ls_orig_claim_status_code		 = idw_dw[1].GetItemString(ll_row, "claim_status_code", Primary!, TRUE)
ls_orig_claim_status_type_code = idw_dw[1].GetItemString(ll_row, "claim_status_type_code", Primary!, TRUE)
IF ls_claim_status_code <> ls_orig_claim_status_code OR ls_claim_status_type <> ls_orig_claim_status_type_code THEN
	lb_statuschanged = TRUE
END IF

ldtm_accdate_comp 			= idw_dw[1].GetItemDateTime(1,'accident_date', Primary!, TRUE)
IF ldtm_accident_date <> ldtm_accdate_comp THEN lb_accident_date_change = TRUE

IF inv_bc.uf_is_registered(ll_claim_no) = 1 THEN  //Claim is registered
	IF lb_statuschanged = TRUE THEN
		ll_return = inv_bc.uf_update_termdate_for_registered_claims(ll_claim_no,ls_claim_status_code,ls_claim_status_type)
		IF ll_return >= 0 THEN
			//Check to see if a claim event needs to be logged
			inv_bc.uf_log_claim_event_for_noi_change(ll_claim_no,ls_claim_status_code,ls_claim_status_type)
			IF ll_return = 1 THEN lb_display_message = TRUE
		END IF
		ll_return = inv_bc.uf_update_cec_on_adjudication(ll_claim_no,ls_claim_status_code,ls_claim_status_type)
		IF ll_return = 1 THEN lb_display_message = TRUE
	END IF	
	IF lb_accident_date_change = TRUE THEN 
		//Check to see if a claim event should be logged
		inv_bc.uf_log_claim_event_for_accdate_change(ll_claim_no,ldtm_accident_date)
	END IF	
		
ELSE //Claim is not registered
		//Check claim status to see if the claim is now eligible to be registered.
		lb_eligible = inv_bc.uf_check_for_eligibility(ll_claim_no,ls_claim_status_code,ls_claim_status_type)
		IF lb_eligible = TRUE THEN
			MessageBox('Rx Coverage Information','This Claim is now eligible for Rx Coverage.~n'+ &
			'The new coverage can be viewed in the Rx Coverage Indicator Module.',Information!,OK!)
		END IF
		IF lb_eligible = FALSE AND ll_override_row > 0 THEN
			MessageBox('Rx Coverage Information','This Claim is no longer eligible for Rx Coverage.~n'+ &
			'The override termination date for Rx Coverage is being removed as it is no applicable.',Information!,OK!)
			idw_dw[10].DeleteRow(idw_dw[10].GetRow())
			idw_dw[10].Update()
			SQLCA.nf_handle_error("w_claim", "", "cb_save - dw_override_main.Update()")
		END IF
	
END IF	


/********************************* END OF EPAY II *************************************************/	
	

/*	check to see if annual review date needs to be modified
	set up the next review due date
*/
	IF idw_dw[3].RowCount() > 0 THEN
		ldtm_date = f_server_datetime()
		SetNull(ldtm_check)
		SetNull(ldtm_next_review_due)
		ldtm_review_due = idw_dw[1].GetItemDateTime(1,'annual_ben_review_due_date')
		
		IF NOT IsNull( ldtm_review_due ) THEN
		
			IF inv_openings.nf_all_openings_ended() THEN
				IF inv_openings.nf_end_before_due(ldtm_review_due) THEN 		// all ben end dates less than review date
					idw_dw[1].SetItem(1,'annual_ben_review_due_date', ldtm_check)
				END IF
			ELSE				
				IF ldtm_review_due < inv_openings.idt_accident_date THEN
					MessageBox('Review Due Date Error', 'The annual review date cannot be before the accident date.')
					Return -1
				END IF
				IF Date(ldtm_review_due) > RelativeDate(Date(ldtm_date),517) THEN
					MessageBox('Review Due Date Error', 'The annual review date cannot be more than 17 months in the future.')
					Return -1
				END IF
			END IF
		ELSEIF inv_openings.nf_more_than_one_opened() THEN			// more than one opened opening
			MessageBox('Missing Review Date', 'The annual benefit review date must be entered.')
			Return -1
		ELSE
			IF NOT inv_openings.nf_all_openings_ended() THEN	
/*				PR4529 - J. Hawker - Annual Review Date must be entered. The Annual Review Date 
   			must only be automatically set when an opening is first created. 
*/
				IF NOT IsNull(idw_dw[1].GetItemDateTime(1,'annual_ben_review_due_date', PRIMARY!, TRUE)) THEN
					MessageBox('Missing Review Date', 'The annual benefit review date must be entered.')
					idw_dw[1].SetFocus()
					idw_dw[1].Setcolumn('annual_ben_review_due_date')					
					Return -1
				END IF
				
/*				if RLOE opened then use this opening to calculate the
				review due date
				these functions will return the row number of the opened entry 
				or the one with the end date in the future
*/				
				ll_opened = inv_openings.nf_check_open_type('RLOE')
				IF ll_opened > 0 THEN
					ldtm_check = idw_dw[3].GetItemDateTime(ll_opened,'accident_recurrence_date')
					IF Month(Date(ldtm_check)) <= Month(Date(ldtm_date)) THEN					// month of recurrence <= today
						ldt_annual_ben_review_due = Date(Year(Date(ldtm_date)) + 1, Month(Date(ldtm_check)), 1)
						idw_dw[1].SetItem(1,'annual_ben_review_due_date',DateTime(ldt_annual_ben_review_due))
					ELSE
						ldt_annual_ben_review_due = Date(Year(Date(ldtm_date)), Month(Date(ldtm_check)), 1)
						idw_dw[1].SetItem(1,'annual_ben_review_due_date',DateTime(ldt_annual_ben_review_due))
					END IF
				END IF
				IF IsNull(ldtm_check) THEN
/*					continue on and look for ltd
*/					ll_opened = inv_openings.nf_check_open_type('LTD')
					IF ll_opened > 0 THEN
						ldtm_check = idw_dw[1].GetItemDateTime(1,'accident_date')
						IF Month(Date(ldtm_check)) <= Month(Date(ldtm_date)) THEN			// month of accident <= today
							ldt_annual_ben_review_due = Date(Year(Date(ldtm_date)) + 1, Month(Date(ldtm_check)), 1)
							idw_dw[1].SetItem(1,'annual_ben_review_due_date',DateTime(ldt_annual_ben_review_due))
						ELSE
							ldt_annual_ben_review_due = Date(Year(Date(ldtm_date)), Month(Date(ldtm_check)), 1)
							idw_dw[1].SetItem(1,'annual_ben_review_due_date',DateTime(ldt_annual_ben_review_due))
						END IF
					END IF
				END IF
				IF IsNull(ldtm_check) THEN
/*					continue on and look for PEN
*/					ll_opened = inv_openings.nf_check_open_type('PEN')
					IF ll_opened > 0 THEN
						ldtm_check = idw_dw[1].GetItemDateTime(1,'accident_date')
						IF (Month(Date(ldtm_check)) -1 ) <= Month(Date(ldtm_date)) THEN			// month of accident - 1 <= today
							IF Month(Date(ldtm_check)) - 1 = 0 THEN
								ldt_annual_ben_review_due = Date(Year(Date(ldtm_date)), 12, 1)
							ELSE
								ldt_annual_ben_review_due = Date(Year(Date(ldtm_date)) + 1, Month(Date(ldtm_check)) - 1, 1)
							END IF
							idw_dw[1].SetItem(1,'annual_ben_review_due_date',DateTime(ldt_annual_ben_review_due))
						ELSE
							ldt_annual_ben_review_due = Date(Year(Date(ldtm_date)), Month(Date(ldtm_check)) - 1, 1)
								idw_dw[1].SetItem(1,'annual_ben_review_due_date',DateTime(ldt_annual_ben_review_due))
						END IF
					END IF
				END IF
				IF IsNull(ldtm_check) THEN
/*					continue on and look for SV
*/					ll_opened = inv_openings.nf_check_open_type('SV')
					IF ll_opened > 0 THEN
/*						Get the death date
*/		
						ll_no = idw_dw[1].GetItemNumber(1,'individual_no')
						SELECT death_date
						  INTO :ldtm_check
						  FROM INDIVIDUAL
						  WHERE individual_no = :ll_no;
						IF SQLCA.nf_handle_error('Embedded SQL: select INDIVIDUAL', 'n_maintain_claim_controller', 'nf_check_bus_rule') < 0 THEN
							Return -1
						END IF
						IF (Month(Date(ldtm_check)) -1 ) <= Month(Date(ldtm_date)) THEN			// month of death date - 1 <= today
							IF Month(Date(ldtm_check)) - 1 = 0 THEN
								ldt_annual_ben_review_due = Date(Year(Date(ldtm_date)) , 12, 1)
							ELSE
								ldt_annual_ben_review_due = Date(Year(Date(ldtm_date)) + 1, Month(Date(ldtm_check)) - 1, 1)
							END IF
							idw_dw[1].SetItem(1,'annual_ben_review_due_date',DateTime(ldt_annual_ben_review_due))
						ELSE
							ldt_annual_ben_review_due = Date(Year(Date(ldtm_date)), Month(Date(ldtm_check)) - 1, 1)
							idw_dw[1].SetItem(1,'annual_ben_review_due_date',DateTime(ldt_annual_ben_review_due))
						END IF
					END IF
				END IF
				IF IsNull(ldtm_check) THEN
/*					continue on and look for S1
*/					ll_opened = inv_openings.nf_check_open_type('S1')
					IF ll_opened > 0 THEN
/*						Get the death date
*/		
						ll_no = idw_dw[1].GetItemNumber(1,'individual_no')
						SELECT death_date
						  INTO :ldtm_check
						  FROM INDIVIDUAL
						  WHERE individual_no = :ll_no;
						IF SQLCA.nf_handle_error('Embedded SQL: select INDIVIDUAL', 'n_maintain_claim_controller', 'nf_check_bus_rule') < 0 THEN
							Return -1
						END IF
						IF (Month(Date(ldtm_check)) -1 ) <= Month(Date(ldtm_date)) THEN			// month of death date - 1 <= today
							IF Month(Date(ldtm_check)) - 1 = 0 THEN
								ldt_annual_ben_review_due = Date(Year(Date(ldtm_date)) , 12, 1)
							ELSE
								ldt_annual_ben_review_due = Date(Year(Date(ldtm_date)) + 1, Month(Date(ldtm_check)) - 1, 1)
							END IF
							idw_dw[1].SetItem(1,'annual_ben_review_due_date',DateTime(ldt_annual_ben_review_due))
						ELSE
							ldt_annual_ben_review_due = Date(Year(Date(ldtm_date)), Month(Date(ldtm_check)) - 1, 1)
							idw_dw[1].SetItem(1,'annual_ben_review_due_date',DateTime(ldt_annual_ben_review_due))
						END IF
					END IF
				END IF
				IF IsNull(ldtm_check) THEN
/*					continue on and look for S2
*/					ll_opened = inv_openings.nf_check_open_type('S2')
					IF ll_opened > 0 THEN
/*						Get the death date
*/		
						ll_no = idw_dw[1].GetItemNumber(1,'individual_no')
						SELECT death_date
						  INTO :ldtm_check
						  FROM INDIVIDUAL
						  WHERE individual_no = :ll_no;
						IF SQLCA.nf_handle_error('Embedded SQL: select INDIVIDUAL', 'n_maintain_claim_controller', 'nf_check_bus_rule') < 0 THEN
							Return -1
						END IF
						IF (Month(Date(ldtm_check)) -1 ) <= Month(Date(ldtm_date)) THEN			// month of death date - 1 <= today
							IF Month(Date(ldtm_check)) - 1 = 0 THEN
								ldt_annual_ben_review_due = Date(Year(Date(ldtm_date)) , 12, 1)
							ELSE
								ldt_annual_ben_review_due = Date(Year(Date(ldtm_date)) + 1, Month(Date(ldtm_check)) - 1, 1)
							END IF
							idw_dw[1].SetItem(1,'annual_ben_review_due_date',DateTime(ldt_annual_ben_review_due))
						ELSE
							ldt_annual_ben_review_due = Date(Year(Date(ldtm_date)), Month(Date(ldtm_check)) - 1, 1)
							idw_dw[1].SetItem(1,'annual_ben_review_due_date',DateTime(ldt_annual_ben_review_due))
						END IF
					END IF
				END IF
			END IF		
		END IF
		

		//Check for action items related to this claim.		
		lb_related_action_items_exist = inv_action_item.nf_related_action_items_exist(0,0,True)
		If lb_related_action_items_exist Then
			//RULE: Display a message to the user if the claim_manager_user_id changed and there
			//are outstanding action items for this claim
			IF idw_dw[1].GetItemString(1,'claim_manager_user_id') <> idw_dw[1].GetItemString(1,'claim_manager_user_id',Primary!,True) Then
				MessageBox('Outstanding action items',"There are outstanding action items related to this claim. You may want to change the responsible user for these action items, but it's not required.")				
			End if
			
			//RULE:  Display a message to the user if the case_managed_flag is changed from "Y"
			//to 'N' and there are outstanding action items for this claim
			If idw_dw[1].GetItemString(1,'case_managed_flag') = 'N' and idw_dw[1].GetItemString(1,'case_managed_flag',Primary!,True) = 'Y' Then
				MessageBox('Oustanding action items',"There are outstanding action items related to this claim.  You may want to cancel these items, but it's not necessary")
			End if
		End if
		
				
		//Loop through the openings
		For ll_opening_row = 1 to idw_dw[3].RowCount()
			//ll_opening_row = idw_dw[3].GetRow()
			ll_opening_no = idw_dw[3].GetItemNumber(ll_opening_row,'opening_no')
			ldt_disablement_date = idw_dw[3].GetItemDateTime(ll_opening_row,'benefit_start_date')
			
			//Check for action items related to this claim/ opening, and don't retrict the search 
			//to outstanding action items only (that is what the false parameter does)
			lb_related_action_items_exist = inv_action_item.nf_related_action_items_exist(ll_opening_no,0,False)
			IF lb_related_action_items_exist  Then
				//RULE: Action items must be related to an RLOE opening
				If idw_dw[3].GetItemString(ll_opening_row,'opening_type_code') <> 'RLOE' Then
					MessageBox('Validation error','The opening type must be RLOE because it has related action items.')
					RETURN -1
				END IF
				
				//RULE: Action items must be associated with a recurrence type opening (not administrative)
				/*PR5108 - J.Hawker, 2005.08.19 - If the recurrence type is changed from Recurrence to Administrative, 
				  check for related action items and if they exist, assign them to the previous RLOE Recurrence opening.
				*/
				IF idw_dw[3].GetItemString(ll_opening_row,'recurrence_type_code') <> 'R' Then
					IF idw_dw[3].GetItemString(ll_opening_row,'recurrence_type_code', Primary!, TRUE) = 'R' AND &
						idw_dw[3].GetItemString(ll_opening_row,'recurrence_type_code') = 'A' THEN
						ldtm_date = DATETIME(DATE('1900-01-01'))
						FOR ll_Cntr = idw_dw[3].RowCount() to 1 STEP -1
							ls_recurrence_type_code = idw_dw[3].GetItemString(ll_cntr,'recurrence_type_code')
							ls_opening_type_code    = idw_dw[3].GetItemString(ll_cntr,'opening_type_code')
							ldtm_acc_rec_date       = idw_dw[3].GetItemDateTime(ll_cntr,'accident_recurrence_date')
							IF ls_recurrence_type_code = 'R' AND ls_opening_type_code = 'RLOE' THEN
								IF ldtm_acc_rec_date <= idw_dw[3].GetItemDateTime(ll_opening_row,'accident_recurrence_date') THEN
									IF ldtm_acc_rec_date > ldtm_date THEN
										ldtm_date = ldtm_acc_rec_date 
										ll_new_opening_no = idw_dw[3].GetItemNumber(ll_cntr,'opening_no')
									END IF								
								END IF
							END IF
						NEXT 
						
						IF ll_new_opening_no > 0 THEN							
							IF MessageBox("Action Items Exist","Changing the current opening from Recurrence to Administrative" + &
						         "~r~nwill move the opening's Action Items to Opening # " + STRING(ll_new_opening_no) + &
									".  Do you wish to continue?",question!, okcancel!) = 1 THEN
							
								li_action_items_reassigned = inv_action_item.nf_reassign_related_action_items(ll_opening_no,ll_new_opening_no)
								IF li_action_items_reassigned < 0 THEN
									MessageBox('Error','There was a problem re-assigning action items. Please call Help Desk.', information!)
									RETURN -1
								END IF	
							ELSE
								RETURN -1
							END IF
							ls_recurrence_type_code = ''
							ls_opening_type_code    = ''
						ELSE
							MessageBox('No Matching Recurrence','Action Items cannot be moved as there are no matching Recurrence openings.' + &
							       '~r~nRecurrence type cannot be changed.',information!)
							RETURN -1
						END IF
					END IF
				END IF
			End if
			
			//Check for action items related to this claim/ opening
			lb_related_action_items_exist = inv_action_item.nf_related_action_items_exist(ll_opening_no,0,True)
			IF lb_related_action_items_exist Then
				//If the benefit_start_date is modified and there are oustanding related action_items
				//warn the user and tell them to notify the responsible user so the action_items
				//dates can be changed if necessary
				If idw_dw[3].GetItemStatus(ll_opening_row ,'benefit_start_date',Primary!) = DataModified! Then
					
					ls_message = 'There are outstanding action items related to this opening.  You should revise their dates or notify the responsible user.'
					//Check if the new benefit_start_date is after the earliest 
					//actions items planned_start_date
					If inv_action_item.nf_get_min_outstanding_planned_date(ll_opening_no,0)	< ldt_disablement_date Then
						MessageBox('Invalid date','This benefit start date is after the planned date for atleast one of the action items.  ~r~nThis is not allowed. Please revise the planned start date.')
						RETURN -1
					End if
					MessageBox('Information',ls_message,Information!,Ok!)					
				End if
				
				//If the openings benefit end date is filled in, provide the user with a list of 
				//oustanding action items related to this opening and allow them to choose
				//which ones to cancel as well.
				If idw_dw[3].GetItemStatus(ll_opening_row,'benefit_end_date',Primary!) = DataModified! Then
					IF not IsNull(idw_dw[3].GetItemDateTime(ll_opening_row,'benefit_end_date')) Then
						li_action_items_canceled = inv_action_item.nf_cancel_related_action_items(ll_opening_no,0,True)
						IF li_action_items_canceled = -1 Then 
							Return -1	
						ELSE
							// Need to update the CLAIM_EVENT table so that when the next claim event number is grabbed 
							// we don't grab the same one, as there is no last Claim event table.
							
						END IF	
					End if
				End if
			End if						
		Next		
	END IF
	
	//Loop through the deleted openings and verify that they can be deleted
	For ll_opening_row = 1 to idw_dw[3].DeletedCount()
		//ll_opening_row = idw_dw[3].GetRow()
		ll_opening_no = idw_dw[3].GetItemNumber(ll_opening_row,'opening_no',Delete!,False)
		//ldt_disablement_date = idw_dw[3].GetItemDateTime(ll_opening_row,'benefit_start_date',Delete!,False)
		
		//Check for action items related to this claim/ opening
		lb_related_action_items_exist = inv_action_item.nf_related_action_items_exist(ll_opening_no,0,False)
		IF lb_related_action_items_exist Then
			MessageBox('Delete error','This opening cannot be deleted because it has related action items.')
			RETURN -1
		END IF
	Next


// ---- CREATE AN AUTO REHAB PLAN, TASKS AND AUTHORIZATIONS -----

ll_claim_no = idw_dw[1].GetITemNumber(1,'claim_no')
ldtm_accident_date = idw_dw[11].GetItemDateTime (1,'accident_date')
ls_claim_status_code = idw_dw[1].GetItemString(1,'claim_status_code')
ls_claim_status_type = idw_dw[1].GetItemString(1,'claim_status_type_code')
lb_active = FALSE

IF ls_claim_status_code = 'A' THEN //check for an RLOE opening for a claim status of Active
	//loop through the openings to ensure there is an open-ended RLOE opening.
	ll_max = idw_dw[3].RowCount()
	ll_loop = 1
	DO WHILE ll_loop <= ll_max
      IF idw_dw[3].GetItemString(ll_loop, 'opening_type_code') = 'RLOE' AND IsNull(idw_dw[3].GetItemDateTime(ll_loop, 'benefit_end_date')) THEN
		 ll_opening_no = idw_dw[3].GetItemNumber(ll_loop, 'opening_no')
		 IF IsNull(ll_opening_no) then ll_opening_no = 1
       	 lb_active = TRUE
      END IF
	   ll_loop = ll_loop + 1
	LOOP
END IF

//check that the claim has a referral for physio services and retrieve that referral no
ll_rtn = inv_referral.nf_check_for_a_valid_referral(ll_claim_no, 'S022',ll_referral_no)

IF ll_rtn > 0 THEN //  1 - there is a referral on file

	//  T015677 - 2015-07-11 We now need to determine if a direct physio referral needs to be created instead of the primary referral as it did before, when the claim is being adjudicated
	//  To do this, we check for the existence of a document in the doc list with type code 'SP' AND a physiotherapy referral flag on. 
	//   if one exists, and the claim is being adjudicated, create the direct physio referral , tasks and auths, otherwise create primary physio  
	
	
	SELECT isnull(max(di.docid), 0), rr.rehab_referral_no
	INTO   :ll_docid, :ll_direct_referral_no
	FROM   DOCUMENT_INDEX di
	JOIN    REHAB_REFERRAL rr on di.claim_no = rr.claim_no and di.docid = rr.doc_id
	WHERE  di.type_code = 'SP'
	AND     rr.rehab_service_code = 'S022'
	AND     di.claim_no = :ll_claim_no
	group  by rr.rehab_referral_no
	USING  SQLCA;
	
	SQLCA.nf_handle_error("claim_maintenance - ","nf_check_bus_rules","Select docid, referral_no.")
	
	// set the data up for a direct physio referral; Diana said the conditions are that the claim is being adjudicated, meaning it is moving from a state of being a 'J' to an 'A'. or 'F - 01 to F - 04
	IF ls_orig_claim_status_code = 'J' AND ll_docid > 0 THEN   
		 ls_service_code = 'S022'
		 ls_program_code = 'P006'
		 ls_task_type_code = 'AS'
		 ls_task_subtype_code  = '168'
		 
		 IF ll_direct_referral_no > 0 THEN ll_referral_no  = ll_direct_referral_no   // reassign the new direct referal number to this variable so the proper referral will have its task_no updated later
	ELSE
		 ls_service_code = 'S022'
		 ls_program_code = 'P001'
		 ls_task_type_code  = 'TR'
		 ls_task_subtype_code = '031'
	END IF
	
	//the claim is a new accident within 6 weeks (42 days) of the current date
	
	ll_number =  DaysAfter(Date(ldtm_accident_date),Date(ldtm_today))
	IF DaysAfter(Date(ldtm_accident_date),Date(ldtm_today)) <= 42 THEN // 2  
	    //check the claim status
        IF (ls_claim_status_code = 'F' AND (ls_claim_status_type = '01' OR ls_claim_status_type = '02' OR ls_claim_status_type = '03' OR ls_claim_status_type = '04' )) OR (lb_active = TRUE) THEN  // 3 
			IF IsNull(ll_opening_no) then ll_opening_no = 1
         	//check for a task
	        		IF inv_referral.nf_check_rehab_task(ll_claim_no,ls_service_code,ls_program_code) = 1 THEN // 4
	     	   //check the referral date
				IF inv_referral.nf_check_referral_date(ll_claim_no,ls_service_code,Date(ldtm_accident_date))	= 1 THEN	  // 5
			        
					IF inv_referral.nf_check_other_services(ll_claim_no ) = 1 THEN  // 6
					 
					 	//create a Rehab plan, task and authorizations ?
					 	IF inv_referral.nf_create_rehab_plan(ll_claim_no,ls_claim_status_code,ls_claim_status_type ) = 1 THEN
							IF nf_12week_letter(ll_claim_no) = -1 THEN  //Ask about the 12 month pre-accident earnings request
								MessageBox('Error','Cannot create the 12 week review letter for Remote Print.',Exclamation!)
							END IF
					  	END IF

					  	IF inv_referral.nf_create_rehab_tasks(ll_claim_no,ls_service_code,ls_program_code,ls_task_type_code,ls_task_subtype_code,ll_opening_no,Date(ldtm_accident_date),li_task_no) = 1 THEN
							  IF inv_referral.nf_create_rehab_task_auths(li_task_no, ll_claim_no,ls_service_code, ls_program_code) = 1 THEN 
						   	 		MessageBox('Rehab Tasks and Authorizations','Rehab Tasks and Authorizations have been automatically created for the claim.',Information!)
							   	 			
									//if a primary physio task exists, and we are creating a direct referral instead, then cancel the primary physio task
									li_existing_physio_task = inv_referral.nf_check_physio_task_not_assigned(ll_claim_no)
									IF li_existing_physio_task > 0 and ls_program_code = 'P006'  THEN
										UPDATE REHAB_TASK
										SET      task_status_code = '03'
										WHERE  claim_no = :ll_claim_no
										and      task_no = :li_existing_physio_task
										USING SQLCA;
			
										SQLCA.nf_handle_error("claim_maintenance - ","nf_check_bus_rules","UPDATE REHAB_TASK..set task_status_code..")
									END IF
										 
									IF inv_referral.nf_update_task_on_referral(ll_referral_no, li_task_no) < 0 THEN 
							    			Return -1
							    		END IF 
							END IF  
					   	END IF 				
					 END IF  //6
			 	 END IF  //5
	      	END IF  //4
		END IF  //3
	 END IF  //2
  END IF  //1

/*	check to see if the difficult claim info is filled in 
	if so then set the difficult claim flag on the main claim dw (1)
	else delete the row
*/
/*	seems to be a problem with no rows being there this is temporary until I find the problem
*/
	IF idw_dw[4].RowCount() > 0 THEN

/*	must be a fatality for P81 legislation code
*/
		IF idw_dw[1].GetItemString(1,'legislation_code') = 'P81' AND (idw_dw[4].GetItemString(1,'fatality_flag') = 'N' OR IsNull(idw_dw[4].GetItemString(1,'fatality_flag'))) THEN
			MessageBox('Error on Legislation Code', 'A Post-1981 code can only be used if there is a fatality.')
			Return -1
		END IF
				
		//P10151-66 - J. Hawker, 2007.10.15
		
		SELECT form_of_election_sent_flag
		INTO     :ls_flag
		FROM    DIFFICULT_CLAIM 
		WHERE  claim_no = :ll_claim_no 
		USING   SQLCA;
		
		IF SQLCA.nf_handle_error("n_maintain_claim_controller","nf_check_bus_rule()","SELECT form_of_election_sent_flag") < 0 THEN
			RETURN -1
		END IF

		IF NOT IsNull(ls_flag) THEN
			IF ls_flag <>  idw_dw[4].GetItemString(1,'form_of_election_sent_flag') THEN
				lb_foe_chg = TRUE
			END IF
		END IF
				
		// Check if any ELECTION letters have been sent for the claim
		SELECT Count(*)
		INTO     :ll_foe_count
		FROM   CORRESPONDENCE 
		WHERE claim_no                      = :ll_claim_no
		AND     template_code              IN ('ELECTION')
		AND     correspond_status_code = 'S'
		USING   SQLCA;
	
		IF SQLCA.nf_handle_error("n_maintain_claim_controller","nf_check_bus_rule()","SELECT Count(*)") < 0 THEN
			RETURN -1
		END IF				
		
		// Check if any FBLETTERS letters have been sent for the claim
		SELECT Count(*)
		INTO     :ll_fb_count
		FROM   CORRESPONDENCE 
		WHERE claim_no                      = :ll_claim_no
		AND     template_code              IN ('FBLETTER')
		AND     correspond_status_code = 'S'
		USING   SQLCA;
	
		IF SQLCA.nf_handle_error("n_maintain_claim_controller","nf_check_bus_rule()","SELECT Count(*)") < 0 THEN
			RETURN -1
		END IF	
	
		IF ll_foe_count > 0 THEN	
			lb_foe_sent = TRUE
			//If claim status is changed and the FOE flag = 'Y', send an email
			IF idw_dw[4].GetItemString(1,'form_of_election_sent_flag') = 'Y' THEN	
				//check claim status - if one of the ones listed, send email
				IF lb_statuschanged = TRUE THEN
					IF (ls_claim_status_code = 'A' OR &
						 (ls_claim_status_code = 'F' AND (ls_claim_status_type = '01' OR ls_claim_status_type = '02' OR &
																						ls_claim_status_type = '03'  OR ls_claim_status_type = '04' )) OR &
						 (ls_claim_status_code = 'R' AND (ls_claim_status_type = '06' OR ls_claim_status_type = '07' OR &
																						ls_claim_status_type = '08' OR ls_claim_status_type = '09' OR ls_claim_status_type = '18'))) THEN
						//FOE flag on and claim status change
						li_array = Upperbound(ii_reason)
						ii_reason[li_array + 1] = 004
					END IF
				END IF
			END IF
			
			//If flag is changed, send email
			IF lb_foe_chg THEN
				IF idw_dw[4].GetItemString(1,'form_of_election_sent_flag') = 'Y' THEN
					//FOE flag turned on
					li_array = Upperbound(ii_reason)
					ii_reason[li_array + 1] = 003		
				ELSE
					//FOE flag turned off
					li_array = Upperbound(ii_reason)
					ii_reason[li_array + 1] = 002
				END IF	
			END IF		
		ELSE 
			lb_foe_sent = FALSE
		END IF
		
		IF Upperbound(ii_reason) > 0 THEN
			istr_email_setup.as_application_code = is_app
			istr_email_setup.ai_app_module_code = ii_module
			istr_email_setup.as_subject_type = 'FOE'
		END IF
		
		//BR 4.41 - If FOE flag is set 'Off' and FOE has been sent, give warning.
		IF (lb_foe_sent OR ll_fb_count > 0) AND (idw_dw[4].GetItemString(1,'form_of_election_sent_flag') <>  'Y' AND lb_foe_chg) THEN
			IF MessageBox('Warning','The Form of Election flag is being turned "Off", but a Form of Election Letter has already been sent. Would you like to continue?',Question!,YesNo!) = 2 THEN
				RETURN -1
			END IF
		END IF
		//BR 4.42 - If FOE flag is set 'On' and FOE has not been sent, give warning.
		IF NOT (lb_foe_sent OR ll_fb_count > 0) AND (idw_dw[4].GetItemString(1,'form_of_election_sent_flag') = 'Y' AND lb_foe_chg) THEN
			IF MessageBox('Warning','The Form of Election flag is being turned "On", but a Form of Election Letter has not been sent. Would you like to continue?',Question!,YesNo!) = 2 THEN
				RETURN -1
			END IF
		END IF					

		IF (IsNull(idw_dw[4].GetItemString(1,'form_of_election_sent_flag')) OR idw_dw[4].GetItemString(1,'form_of_election_sent_flag') = 'N') AND &
		(IsNull(idw_dw[4].GetItemString(1,'appeal_decision_flag')) OR idw_dw[4].GetItemString(1,'appeal_decision_flag') = 'N') AND &
		(IsNull(idw_dw[4].GetItemString(1,'fatality_flag')) OR idw_dw[4].GetItemString(1,'fatality_flag') = 'N') AND &
		(IsNull(idw_dw[4].GetItemString(1,'complex_adjudication_code')) OR Trim(idw_dw[4].GetItemString(1,'complex_adjudication_code')) = '') THEN
			idw_dw[4].DeleteRow(1)
			idw_dw[1].SetItem(idw_dw[1].GetRow(),'difficult_claim_flag', 'N')
		ELSE
/*		validate the adjudication code
*/
			ls_string = idw_dw[4].GetItemString(1,'complex_adjudication_code')
			IF NOT (IsNull(ls_string) OR ls_string = '') THEN
				SELECT complex_adjudication_code
				  INTO :ls_string2
				  FROM Complex_Adjudication
				 WHERE complex_adjudication_code = :ls_string;

				IF SQLCA.nf_handle_error('Embedded SQL: select Complex_Adjudication', 'n_maintain_claim_controller', 'nf_check_bus_rule') < 0 THEN
					Return -1
				END IF
				IF IsNull(ls_string2) THEN
					MessageBox('Error', 'Invalid complex adjudication code.' )
					Return -1
				END IF
			END IF
			
/*		validate the flags
*/		
			IF idw_dw[4].GetItemString(1,'form_of_election_sent_flag') <> 'N' AND idw_dw[4].GetItemString(1,'form_of_election_sent_flag') <> 'Y' THEN	
				MessageBox('Invalid Form of Election Sent', 'The Form of Election Sent flag can only have a value of Y or N.')
				Return -1
			END IF		
			IF idw_dw[4].GetItemString(1,'fatality_flag') <> 'N' AND idw_dw[4].GetItemString(1,'fatality_flag') <> 'Y' THEN	
				MessageBox('Invalid Fatality', 'The fatality flag can only have a value of Y or N.')
				Return -1
			ELSE
				IF idw_dw[4].GetItemString(1,'fatality_flag') = 'Y' THEN
/*				check the death date
*/					
					ll_no = idw_dw[1].GetItemNumber(1,'individual_no')
					SELECT death_date
					  INTO :ldtm_date
					  FROM INDIVIDUAL
					 WHERE individual_no = :ll_no;
					IF SQLCA.nf_handle_error('Embedded SQL: select INDIVIDUAL', 'n_maintain_claim_controller', 'nf_check_bus_rule') < 0 THEN
						Return -1
					END IF
					IF IsNull(ldtm_date) THEN
						MessageBox('Invalid Fatality Setting', 'The fatality flag cannot be set on unless there is a death date entered on the individual.')
						Return -1
					END IF
				END IF
			END IF
			IF idw_dw[4].GetItemString(1,'appeal_decision_flag') <> 'N' AND idw_dw[4].GetItemString(1,'appeal_decision_flag') <> 'Y' THEN	
				MessageBox('Invalid Appeal Decision', 'The appeal decision flag can only have a value of Y or N.')
				Return -1
			END IF
			idw_dw[1].SetItem(idw_dw[1].GetRow(), 'difficult_claim_flag', 'Y')					
			
		END IF		
	ELSE
		IF idw_dw[1].GetItemString(1,'legislation_code') = 'P81' THEN
			MessageBox('Error on Legislation Code', 'A Post-1981 code can only be used if there is a fatality.')
			Return -1
		END IF
	END IF	

IF lb_display_message = TRUE THEN
	MessageBox('COVERAGE HAS BEEN AFFECTED','The Coverage has been affected, to view the Coverage Information please see the Rx Coverage Indicator Screen.',Information!,OK!)
END IF

Return 0
end function

public function string nf_get_claim_role_code ();LONG  ll_row

	ll_row = idw_dw[1].GetRow()
Return idw_dw[1].GetItemString(ll_row,'claim_role_code')
end function

public function integer nf_create_firefighter (long al_rejected_wca_claim_no, long al_individual_no, ref long al_new_fca_claim_no);INTEGER li_rtn
LONG    ll_new_FCA_claim_no

li_rtn = inv_claims.nf_create_firefighter(al_rejected_WCA_claim_no,al_individual_no,al_new_FCA_claim_no)

RETURN li_rtn
end function

public function integer nf_12week_letter (long al_claim_no);// Define Variables
LONG			ll_claim_no
INTEGER		li_check_request, li_return = 1
						
// Is "12 month pre-accident earnings request" required (Yes/No). 

li_check_request = MessageBox('Automate','Is 12 month Pre-Accident earnings request required', Question!, YesNo!)

// IF "Yes" THEN
IF li_check_request = 1 THEN

	ll_claim_no = al_claim_no

	//	Create the nonvisual user object for Remote Print.
	If NOT (isvalid(inv_remote_print)) Then 
		inv_remote_print = Create n_cst_remote_print
		If NOT (isvalid(inv_remote_print)) Then
			li_return = -1
		End If
	End If
	
	If li_Return = 1 Then
		IF inv_remote_print.of_init() <> 1 THEN
			MessageBox("Error in of_init()","Cannot Create Letter for Remote Print.")
			li_RETURN  = -1
		END IF
	End If
	
	If li_Return = 1 Then
		IF inv_remote_print.of_Set_Mail_Package_Values(ll_claim_no, 'O', 'PE', 'LC', 'Pre-Accident Earning', 'B') <> 1 THEN
			MessageBox("Error in of_Set_Mail_Package_Values()","Cannot Create Letter for Remote Print.")
			li_return = -1
		END IF
	End If
	
	If li_Return = 1 Then
		IF inv_remote_print.of_Insert_into_mail_package_table() <> 1 THEN
			MessageBox("Error in of_Insert_into_mail_package_table()","Cannot Create Letter for Remote Print.")
			li_return = -1
		END IF
	End If
END IF
	
If isValid(inv_remote_print) Then DESTROY inv_remote_print

RETURN li_Return

end function

on destructor;call n_pdc::destructor;	IF IsValid(inv_claims) THEN
		Destroy(inv_claims)
	END IF
	IF IsValid(inv_openings) THEN
		Destroy(inv_openings)
	END IF

end on

on n_maintain_claim_controller.create
call super::create
end on

on n_maintain_claim_controller.destroy
call super::destroy
end on

