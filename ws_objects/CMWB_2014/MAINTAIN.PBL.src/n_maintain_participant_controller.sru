$PBExportHeader$n_maintain_participant_controller.sru
$PBExportComments$controller object for n_claim_participant & n_individual
forward
global type n_maintain_participant_controller from n_pdc
end type
end forward

global type n_maintain_participant_controller from n_pdc
end type
global n_maintain_participant_controller n_maintain_participant_controller

type variables
N_INDIVIDUAL inv_individual
N_CLAIM_PARTICIPANT inv_participant
LONG il_edit_row
LONG il_claim_no, il_individual_no
DATETIME idtm_annuity_start_date
STRING is_claim_role_code
end variables

forward prototypes
public function integer nf_check_mandatory ()
public function integer nf_set_unused_fields ()
public function integer nf_retrieve_individual (long al_individual_no)
public function long nf_get_claim_no ()
public function integer nf_log_events ()
public function integer nf_check_for_payments (long al_individual_no, long al_claim_no)
public function integer nf_insert (long al_claim_no)
public function integer nf_change_item (long al_datawindow)
public subroutine nf_init ()
public function integer nf_can_delete (long al_individual_no, long al_claim_no)
public function long nf_set_identifiers ()
public function integer nf_check_bus_rule ()
public function integer nf_retrieve (long al_claim_no, long al_individual_no)
end prototypes

public function integer nf_check_mandatory ();/*	call the check mandatory functions in all the children
*/

IF inv_participant.nf_check_mandatory() < 0 THEN Return -1
IF inv_individual.nf_check_mandatory() < 0 THEN Return -1
Return 0
end function

public function integer nf_set_unused_fields ();	IF inv_participant.nf_set_unused_fields() < 0 THEN Return -1
	IF inv_individual.nf_set_unused_fields() < 0 THEN Return -1

Return 0

end function

public function integer nf_retrieve_individual (long al_individual_no);LONG ll_row

/*	this function retrieve the individual information only
	it would be called when the user wants to change the individual on the claim
	it must change the individual number on the claim participant row also
*/

	ll_row = inv_individual.nf_retrieve(al_individual_no)
	IF ll_row > 0 THEN
   	idw_dw[1].SetItem(il_edit_row, 'individual_no', al_individual_no)
	END IF
Return ll_row
end function

public function long nf_get_claim_no ();LONG  ll_row

	ll_row = idw_dw[1].GetRow()
Return idw_dw[1].GetItemNumber(ll_row,'claim_no')
end function

public function integer nf_log_events ();	inv_individual.nf_log_events()
Return inv_participant.nf_log_events()
end function

public function integer nf_check_for_payments (long al_individual_no, long al_claim_no);LONG	ll_count

	SELECT Count(*)
	  INTO :ll_count
	  FROM PAYMENT, APPLIED_CLAIM_TXN
	 WHERE PAYMENT.claim_no = :al_claim_no
	   AND recipient_type_code = 'I'
		AND recipient_no = :al_individual_no
		AND PAYMENT.payment_no = APPLIED_CLAIM_TXN.payment_no;
   IF SQLCA.nf_handle_error('Embedded SQL: Select from PAYMENTS', "n_maintain_participant_controller","nf_check_bus_rule") < 0 THEN
  	   Return -1
   ELSE
  	   IF ll_count > 0 THEN
     	   MessageBox('Deletion Error', 'Individual cannot be deleted.  Payments exist.')
			Return -1
      END IF
  	END IF

/*	unapplied payments
*/
	SELECT Count(*)
	  INTO :ll_count
	  FROM PAYMENT, UNAPPLIED_CLAIM_TXN
	 WHERE PAYMENT.claim_no = :al_claim_no
	   AND recipient_type_code = 'I'
		AND recipient_no = :al_individual_no
		AND PAYMENT.payment_no = UNAPPLIED_CLAIM_TXN.payment_no;
	IF SQLCA.nf_handle_error('Embedded SQL: Select from PAYMENTS', "n_maintain_participant_controller","nf_check_bus_rule") < 0 THEN
      Return -1
	ELSE
      IF ll_count > 0 THEN
     	   MessageBox('Deletion Error', 'Individual cannot be deleted.  Payments exist.')
			Return -1
	   END IF
   END IF

Return 0

end function

public function integer nf_insert (long al_claim_no);	inv_participant.nf_retrieve(al_claim_no)
	inv_participant.nf_insert(1)
	idw_dw[1].ScrollToRow(1)
	idw_dw[1].SetItem(1, 'claim_no', al_claim_no)

/*	retrieve data into claim individual incase claimant role changes
*/
	idw_dw[6].Retrieve(al_claim_no)

	inv_individual.nf_insert(0)
	il_edit_row = 1
Return 0

end function

public function integer nf_change_item (long al_datawindow);
CHOOSE CASE al_datawindow
   CASE 1
  	   inv_participant.nf_change_item(1)
   CASE 2
/*	individual main dw
*/
		il_claim_no      = nf_get_claim_no()
		inv_individual.nf_set_current_claim(il_claim_no)

		IF inv_individual.nf_change_item(1) < 0 THEN
			Return -1
		END IF
	CASE 3
      inv_individual.nf_change_item(2)
	CASE ELSE 
   	Return -1
END CHOOSE

Return 0
end function

public subroutine nf_init ();U_DWA ldw_dw[]

/*	idw_dw[6] - this contains the claim datawindow
	          - it is to update the individual number if the claimant role changes
*/

/*	create all the objects that need controlling from here
*/

	inv_participant = Create n_claim_participant
/*	register the parent and the datawindows
*/
	ldw_dw[1] = idw_dw[1]															// participant
	ldw_dw[2] = idw_dw[10]														   // annuity eligibility info
	inv_participant.nf_init(ldw_dw[], SQLCA, iwi_window_parent)

	inv_individual = Create n_individual
	ldw_dw[1] = idw_dw[2]															// individual dw
	ldw_dw[2] = idw_dw[3]															// individual main name dw
	ldw_dw[3] = idw_dw[4]															// individual name (alias)
	ldw_dw[4] = idw_dw[5]															// next individual no dw
	ldw_dw[5] = idw_dw[7]															// claim list for change in individual name
	ldw_dw[6] = idw_dw[6]															// claim individual
	ldw_dw[7] = idw_dw[10]														// annuity_eligibility
	ldw_dw[8] = idw_dw[12]                                                               // participant dw  (new for PR7034)
	inv_individual.nf_init(ldw_dw[], SQLCA, iwi_window_parent)

/*	set up the commit flag to only allow this controller to commit
*/
	nf_set_commit(TRUE)
	inv_participant.nf_set_commit(FALSE)
	inv_individual.nf_set_commit(FALSE)

Return
end subroutine

public function integer nf_can_delete (long al_individual_no, long al_claim_no);STRING ls_role, ls_active
LONG   ll_applied, ll_unapplied, ll_awards

//Claimant cannot be deleted

ls_role   = idw_dw[12].GetItemString(idw_dw[12].GetRow(),'claim_role_code')
ls_active = idw_dw[12].GetItemString(idw_dw[12].GetRow(),'claim_participant_claimant_active_flag')

IF ls_role = 'C' AND ls_active = 'Y' THEN
	MessageBox('Error','There must be an active Claimant. The Claimant cannot be deleted.',information!)
	RETURN -1
END IF

//Check for Txns, prevent delete if any exist
SELECT Count(*)
INTO   :ll_applied
FROM   APPLIED_CLAIM_TXN a, PAYMENT b
WHERE  a.recipient_type_code = 'I' 
AND    a.claim_no = b.claim_no
AND    a.recipient_no = :al_individual_no
AND    a.claim_no = :al_claim_no
UNION ALL
SELECT Count(*)
FROM   UNAPPLIED_CLAIM_TXN a, PAYMENT b 
WHERE  a.recipient_type_code = 'I' 
AND    a.claim_no = b.claim_no
AND    a.recipient_no = :al_individual_no
AND    a.claim_no = :al_claim_no
USING SQLCA;

IF SQLCA.nf_handle_error('SELECT Count(*) INTO :ll_applied', "w_add_participant_individual","wf_can_delete") < 0 THEN
	RETURN -1
END IF

//Check for Awards, prevent delete if any exist
SELECT Count(*)
INTO   :ll_awards
FROM   PERIODIC_AWARD a, 
       PERIODIC_RECIPIENT b
WHERE  a.claim_no            = b.claim_no
AND    a.award_no            = b.award_no
AND    b.recipient_type_code = 'I'
AND    b.recipient_no        = :al_individual_no
AND    b.claim_no            = :al_claim_no
USING  SQLCA;

IF SQLCA.nf_handle_error('SELECT Count(*) INTO :ll_awards', "w_add_participant_individual","wf_can_delete") < 0 THEN
	RETURN -1
END IF

IF ll_applied > 0 OR ll_unapplied > 0 OR ll_awards > 0 THEN
	MessageBox('Payments Exist','Payments exist, participant cannot be deleted.',information!)
	RETURN -1
END IF

RETURN 0
end function

public function long nf_set_identifiers ();LONG ll_sub_claim_no, ll_individual_no, ll_row, ll_loop, ll_old_individual_no, ll_rtn

/*	get all the keys and set on all the datawindows
*/
	IF idw_dw[2].RowCount() > 0 THEN			// the individual
/*	otherwise the row has been deleted.
*/
		IF IsNull(idw_dw[2].GetItemNumber(1,'individual_no') ) OR idw_dw[2].GetItemNumber(1,'individual_no') = 0 THEN
	   	ll_individual_no = inv_individual.nf_set_identifiers()
		ELSE
			ll_individual_no = idw_dw[2].GetItemNumber(1,'individual_no')
			ll_row = idw_dw[4].GetRow()   // alias name  - may  not be one
			IF ll_row > 0 THEN
				idw_dw[4].SetItem(ll_row, 'individual_no', ll_individual_no)
		   END IF
		END IF
	   IF ll_individual_no > 0 THEN
/*			set the individual no on the participant
*/
   	   IF idw_dw[1].DeletedCount() < 1 THEN
/*			do this as long as the row was not deleted		the individual no may have changed
*/
         	idw_dw[1].SetItem(il_edit_row, 'individual_no', ll_individual_no)
				IF IsNull(idw_dw[1].GetItemNumber(il_edit_row,'sub_claim_no')) OR &
					( idw_dw[1].GetItemString(il_edit_row, 'claim_role_code') <> 'C' AND &
					  idw_dw[1].GetItemNumber(il_edit_row,'sub_claim_no') = 0) THEN
      		   ll_sub_claim_no = inv_participant.nf_get_next_sub_claim_no()
				ELSE
					ll_sub_claim_no = idw_dw[1].GetItemNumber(il_edit_row,'sub_claim_no')
				END IF
	         IF idw_dw[1].GetItemString(il_edit_row, 'claim_role_code') <> 'C' THEN
/*					C roles are always 0 sub claim numbers
*/	      	   
					idw_dw[1].SetItem(il_edit_row, 'sub_claim_no', ll_sub_claim_no)
	     	   ELSE
					IF idw_dw[1].GetItemString(il_edit_row, 'claimant_active_flag') = 'Y' THEN
						
						ll_old_individual_no = idw_dw[6].GetItemNumber(1,'individual_no')
         	   	
						IF ll_old_individual_no <> ll_individual_no THEN							
/*						   the claimant roles individual number has changed - need to update the main claim table
*/	          	 	   idw_dw[6].SetItem(1,'individual_no', ll_individual_no)

/*		        			also need to update the annuity eligibility record and injury record
*/						
//							idw_dw[10].Retrieve(il_claim_no, ll_old_individual_no)
//							IF idw_dw[10].RowCount() > 0 THEN
//								MessageBox('Annuity Exists','An Annuity Eligibility record exists and must be reviewed.',exclamation!)
//								idw_dw[10].SetItem(1,'individual_no', ll_individual_no)
//							END IF
											
							idw_dw[11].Retrieve(ll_old_individual_no, il_claim_no)
							IF idw_dw[11].RowCount() > 0 THEN
								idw_dw[11].SetItem(1, 'individual_no', ll_individual_no)
							END IF
						END IF				
	  	         END IF
      		END IF
      	END IF
	   ELSE
   	   MessageBox('Error', 'Error setting identifiers')
      	Return -1
	   END IF

	END IF


Return 0
end function

public function integer nf_check_bus_rule ();LONG  		 ll_return_value, ll_count, ll_claim_no, ll_individual_no, ll_loop, ll_pct,ll_claimant_indiviudual_no 
INTEGER   	 li_year, li_month, li_day,	 li_res, li_rc
STRING		 ls_legislation, ls_comment
DATETIME		 ldtm_date, ldtm_birth_date,ldtm_death_date,ldtm_age_65,ldtm_annuity_end_date_old,ldtm_qualify_date
DATETIME 	 ldtm_annuity_start_date, ldtm_annuity_end_date,ldtm_birth_date_new, ldtm_annuity_start_date_old
BOOLEAN  	 lb_past_pmts
DECIMAL {2}  ldec_pct, ldec_pct_old
dwItemStatus l_status
STRING 			ls_filter
INTEGER			li_rtn
LONG				ll_claim_to_retrieve
LONG				ll_individual_to_retrieve[], ll_day, ll_old_individual, ll_new_individual

/*	need to check and see if it is ok to perform deletes if any
	if not necessary to delete the individual (they exist on another claim) then
	reset the update flag on the datawindow
*/

IF idw_dw[2].DeletedCount() > 0 THEN
/*	if the individual is deleted then so must one of the participants
*/
	IF idw_dw[1].GetItemString(1, 'converted_flag', DELETE!, FALSE) = 'Y' THEN
		MessageBox('Delete Error','This individual was converted from the old system and cannot be deleted.')
		Return -1
	END IF

	ll_claim_no = idw_dw[1].GetItemNumber(1, 'claim_no', DELETE!, FALSE)

/*	if the legislation code exists then delete not allowed
*/
	SELECT legislation_code
	INTO   :ls_legislation
	FROM   CLAIM
	WHERE  claim_no = :ll_claim_no;

	IF SQLCA.nf_handle_error('Embedded SQL: Select from CLAIM',"n_maintain_participant_controller","nf_check_bus_rule") < 0 THEN
		Return -1
	END IF

	IF Trim(ls_legislation) > '' THEN
		MessageBox('Deletion Error', 'The participant cannot be deleted from the claim since a legislation code exists on the claim (Data integrity must be maintain between Workbench and WCB.')
		Return -1
	END IF

/*	get the claim and individual no
*/
  	ll_individual_no = idw_dw[2].GetItemNumber(1,'individual_no', DELETE!, FALSE)
	  
/*	first check to see if payments exist
*/
	IF nf_check_for_payments(ll_individual_no, ll_claim_no) < 0 THEN
		Return -1
	END IF

	SELECT Count(*)
   INTO   :ll_count
	FROM   CLAIM_PARTICIPANT
	WHERE  individual_no = :ll_individual_no
	AND    claim_no     <> :ll_claim_no;
		
/*	check if select ok
*/
	IF SQLCA.nf_handle_error('Embedded SQL: Select from CLAIM_PARTICIPANT',"n_maintain_participant_controller","nf_check_bus_rule") < 0 THEN
		Return -1
	ELSE
		IF ll_count > 0 THEN
			idw_dw[2].ResetUpdate()
		END IF
	END IF
END IF

ll_individual_no = idw_dw[1].GetItemNumber(il_edit_row,'individual_no',Primary!, TRUE)
ll_claim_no = idw_dw[1].GetItemNumber(1, 'claim_no')
  
IF idw_dw[2].RowCount() > 0 THEN
	
	ldtm_birth_date     = idw_dw[2].GetItemDateTime(1, 'birth_date',Primary!, TRUE)
	ldtm_birth_date_new = idw_dw[2].GetItemDateTime(1, 'birth_date')
	
	IF IsNull(ldtm_birth_date_new) THEN ldtm_birth_date_new = ldtm_birth_date
	
	IF idw_dw[1].GetItemNumber(il_edit_row,'individual_no',Primary!,TRUE) <> idw_dw[2].GetItemNumber(1,'individual_no' ) &
		AND NOT IsNull(idw_dw[1].GetItemNumber(il_edit_row,'individual_no',Primary!,TRUE) ) THEN

/*		get the claim and individual no
*/
		ll_return_value = idw_dw[1].GetRow()
		IF ll_return_value < 1 THEN
			MessageBox('Error','Unable to determine participant changed.  See system administrator.')
			Return -1
		END IF
		
		ll_individual_no = idw_dw[1].GetItemNumber(il_edit_row,'individual_no',Primary!, TRUE)
		ll_claim_no = idw_dw[1].GetItemNumber(1, 'claim_no')

/*		if the legislation code exists then delete not allowed
*/
		SELECT legislation_code
		INTO :ls_legislation
		FROM CLAIM
		WHERE claim_no = :ll_claim_no;

		IF SQLCA.nf_handle_error('Embedded SQL: Select from CLAIM',"n_maintain_participant_controller","nf_check_bus_rule") < 0 THEN
			Return -1
		END IF

		IF Trim(ls_legislation) > '' THEN
			MessageBox('Modification Error', 'The participant cannot be changed since a legislation code exists on the claim (Data integrity must be maintain between Workbench and WCB.')
			Return -1
		END IF
		
/*		first check to see if payments exist
*/
		IF nf_check_for_payments(ll_individual_no, ll_claim_no) < 0 THEN
			Return -1
		END IF

		SELECT Count(*)
		INTO  :ll_count
		FROM   CLAIM_PARTICIPANT
		WHERE  individual_no = :ll_individual_no
		AND    claim_no <> :ll_claim_no;
		
/*		check if select ok
*/
		IF SQLCA.nf_handle_error('Embedded SQL: Select from CLAIM_PARTICIPANT',"n_maintain_participant_controller","nf_check_bus_rule") < 0 THEN
			Return -1
		ELSE
			IF ll_count = 0 THEN
				
/*				delete the original individual and the names
*/
				ll_count = idw_dw[8].Retrieve(ll_individual_no)
				
				IF SQLCA.nf_handle_error('Retrieve','n_maintain_participant_controller','nf_check_bus_rule') < 0 THEN RETURN -1
				
				IF ll_count = 1 THEN
					idw_dw[8].DeleteRow(1)
				ELSE
					MessageBox('Error', 'Error deleting old individual.  Contact System Administrator.')
					Return -1
				END IF
				
				ll_count = idw_dw[9].Retrieve(ll_individual_no)
				ll_loop = 1
				
				DO WHILE ll_loop <= ll_count				
					idw_dw[9].DeleteRow(1)
					ll_loop = ll_loop + 1
				LOOP
			END IF
		END IF
	END IF
END IF

ll_return_value = inv_participant.nf_check_bus_rule()
IF ll_return_value < 0 THEN Return ll_return_value

//Before we check the individual business rules we must first determine
//which claims to validate the individual against and weather or not we
//have to force cost allocation validation. We have to force cost allocation
//validation when the sin number has not change but the active claimant has
//changed.  In that case we want to force the cost allocation validation
//and only validate the current claim (reguardless of how many the individual has)

ll_old_individual = idw_dw[1].GetItemNumber(idw_dw[1].GetRow(), 'individual_no',Primary!,TRUE)
ll_new_individual = idw_dw[1].GetItemNumber(idw_dw[1].GetRow(), 'individual_no')

IF (idw_dw[1].GetItemStatus(idw_dw[1].GetRow(),'claimant_active_flag',Primary!) = DataModified! AND &
	idw_dw[1].GetItemString(idw_dw[1].GetRow(),'claimant_active_flag') = 'Y') OR &
	(ll_old_individual <> ll_new_individual AND idw_dw[1].GetItemString(idw_dw[1].GetRow(),'claimant_active_flag') = 'Y') THEN
	//This forces inv_individual to validate cost allocation.  Normally this is only done
	//when the sin no is modified.
	inv_individual.ib_validate_cost_allocation = True
	
	ll_claim_to_retrieve = ll_claim_no
End if

IF idw_dw[2].GetItemStatus(1,'sin_no',Primary!) = DataModified! Then
	ll_individual_to_retrieve[1] = ll_individual_no
Else
	ll_individual_to_retrieve[1] = 0
End if	

//idw_dw[7] is the claim list for the individual.  There are two parameters
//so you can retrieve an additional claim if the individual is being changed
//to the active claimant for that claim.  We must validate that individual against
//the claim before we allow the change.
li_rtn = idw_dw[7].Retrieve(ll_individual_to_retrieve,ll_claim_to_retrieve)

IF li_rtn < 0 Then
	SignalError(-666,'An error occured retrieving the claim list for individual ' + String(ll_individual_no))
End if

SQLCA.nf_handle_error('w_individual','OPEN','Retrieve claim list')
	
il_claim_no = ll_claim_no

ll_return_value = inv_individual.nf_check_bus_rule()
IF  ll_return_value < 0 THEN Return ll_return_value

inv_individual.ib_validate_cost_allocation = False
idw_dw[7].SetFilter('')
idw_dw[7].Filter()

Return 0
end function

public function integer nf_retrieve (long al_claim_no, long al_individual_no);DATAWINDOWCHILD	ldwc_child
LONG					ll_row
	inv_participant.nf_retrieve(al_claim_no)
/*	scroll to the row with the correct individual number
*/	
	il_edit_row = idw_dw[1].Find('individual_no = ' + String(al_individual_no), 0, idw_dw[1].RowCount())
	IF il_edit_row > 0 THEN
   	idw_dw[1].ScrollToRow(il_edit_row)
		IF idw_dw[1].GetItemString(il_edit_row,'dependent_flag') <> 'Y' THEN
			idw_dw[1].GetChild('dependent_reason_code',ldwc_child)
			ldwc_child.SetFilter("dependent_reason_code = ' '")
			ldwc_child.Filter()
		END IF
	END IF
	inv_individual.nf_retrieve(al_individual_no)
/*	Retrieve data into claim_individual 
*/
	idw_dw[6].Retrieve(al_claim_no)
	
	ll_row = idw_dw[10].Retrieve(al_individual_no)
	IF SQLCA.nf_handle_error('Retrieve of Annuity Eligibility','n_maintain_participant_controller','nf_retrieve') < 0 THEN
		Return -1
	END IF		
	IF ll_row = 0 THEN
		idw_dw[10].InsertRow(0)
	END IF		

Return il_edit_row
end function

on destructor;call n_pdc::destructor;	IF IsValid(inv_participant) THEN
		Destroy(inv_participant)
	END IF
	IF IsValid(inv_individual) THEN
		Destroy(inv_individual)
	END IF
end on

on n_maintain_participant_controller.create
call super::create
end on

on n_maintain_participant_controller.destroy
call super::destroy
end on

