$PBExportHeader$n_claim_participant.sru
$PBExportComments$user object containing business rules for claim participant
forward
global type n_claim_participant from n_pdc
end type
end forward

global type n_claim_participant from n_pdc
end type
global n_claim_participant n_claim_participant

type variables
LONG	il_changed_row, il_claim_no
BOOLEAN ib_delete_annuity
STRING is_comment


end variables

forward prototypes
public function integer nf_set_defaults ()
public function integer nf_insert (long al_row)
public function integer nf_check_mandatory ()
public function long nf_get_next_sub_claim_no ()
public function integer nf_inactivate_claimant ()
public function integer nf_retrieve (long al_claim_no)
public function integer nf_change_item (integer al_datawindow)
public function integer nf_check_bus_rule ()
public function integer nf_log_events ()
end prototypes

public function integer nf_set_defaults ();LONG  ll_row, ll_max

	ll_row = 1
	ll_max = idw_dw[1].RowCount()
	DO WHILE ll_row <= ll_max
   	IF IsNull(idw_dw[1].GetItemNumber(ll_row,'claim_no')) THEN
      	idw_dw[1].SetItem(ll_row, 'claim_role_code', 'C')
	      idw_dw[1].SetItem(ll_row, 'sub_claim_no',0)
   	   idw_dw[1].SetItem(ll_row, 'converted_flag', 'N')
/*			the active flag must to set to yes so as to pass the save check on the creation
			of a new claim - it is assumed that the role is active on the new claim creation
*/
      	idw_dw[1].SetItem(ll_row, 'claimant_active_flag', 'Y')
			idw_dw[1].SetItem(ll_row, 'dependent_flag', 'N')
			idw_dw[1].SetItem(ll_row, 'dependent_reason_code', ' ')
			idw_dw[1].SetItem(ll_row, 'pen_survivor_eligibility_flag', 'N')
	   END IF
   	ll_row = ll_row + 1
	LOOP

Return 0
end function

public function integer nf_insert (long al_row);LONG  ll_row

	ll_row = idw_dw[1].InsertRow(al_row)

	IF ll_row < 0 THEN Return -1
	IF nf_set_defaults() < 0 THEN Return -1

Return ll_row
end function

public function integer nf_check_mandatory ();LONG  	ll_loop, ll_max
STRING	ls_string

	IF idw_dw[1].AcceptText() < 0 THEN Return -1

	ll_max = idw_dw[1].RowCount()
	ll_loop = 1
	DO WHILE ll_loop <= ll_max
		ls_string = idw_dw[1].GetItemString(ll_loop, 'claim_role_code')
		IF IsNull(ls_string) OR Trim(ls_string) = '' THEN
			MessageBox('Missing Role','The claim role code must be specified.')
			Return -1
		END IF
   	ll_loop = ll_loop + 1
	LOOP

Return 0
end function

public function long nf_get_next_sub_claim_no ();LONG ll_max

/*	there can be at most 9 sub claim numbers
	if there are any gaps they must be used
	look for the lowest sub claim number not used between 1 and 9
	since the datawindow may not be sorted and we don't want to sort it since it will change
	the order on the screen look for each number and stop when one not found
*/

ll_max = idw_dw[1].RowCount()
IF idw_dw[1].Find('sub_claim_no = 1', 0, ll_max) = 0 THEN
   Return 1
END IF
IF idw_dw[1].Find('sub_claim_no = 2', 0, ll_max) = 0 THEN
   Return 2
END IF
IF idw_dw[1].Find('sub_claim_no = 3', 0, ll_max) = 0 THEN
   Return 3
END IF
IF idw_dw[1].Find('sub_claim_no = 4', 0, ll_max) = 0 THEN
   Return 4
END IF
IF idw_dw[1].Find('sub_claim_no = 5', 0, ll_max) = 0 THEN
   Return 5
END IF
IF idw_dw[1].Find('sub_claim_no = 6', 0, ll_max) = 0 THEN
   Return 6
END IF
IF idw_dw[1].Find('sub_claim_no = 7', 0, ll_max) = 0 THEN
   Return 7
END IF
IF idw_dw[1].Find('sub_claim_no = 8', 0, ll_max) = 0 THEN
   Return 8
END IF
IF idw_dw[1].Find('sub_claim_no = 9', 0, ll_max) = 0 THEN
   Return 9
END IF

Return 0
end function

public function integer nf_inactivate_claimant ();LONG  ll_loop, ll_max, ll_unapplied_no, ll_applied_no, ll_recipient, ll_claim_no

/*	this routine checks to see that the role being marked as inactive has no outstanding
	payments then 
	finds the claimant row that is not the current one being editted
	and mark it as inactive
*/
	ll_max = idw_dw[1].RowCount()
	ll_loop = 1
	DO WHILE ll_loop <= ll_max
   	IF idw_dw[1].GetItemString(ll_loop, 'claim_role_code') = 'C' AND NOT idw_dw[1].GetItemStatus(ll_loop,0, Primary!) = NewModified! THEN
/*			if there is no payments out there then the individual number should be changed and instead of
			creating a new participant
*/	
			ll_recipient = idw_dw[1].GetItemNumber(ll_loop, 'individual_no')
			ll_claim_no = idw_dw[1].GetItemNumber(ll_loop, 'claim_no')
			SELECT Count(*)
			  INTO :ll_unapplied_no
			  FROM UNAPPLIED_CLAIM_TXN
			 WHERE recipient_type_code = 'I'
			   AND recipient_no = :ll_recipient
				AND claim_no = :ll_claim_no;
			IF SQLCA.nf_handle_error('Embedded SQL: select from PAYMENTS', 'n_claim_participant', 'nf_inactivate_claimant') < 0 THEN
				Return -1
			END IF

			SELECT Count(*)
			  INTO :ll_applied_no
			  FROM APPLIED_CLAIM_TXN
			 WHERE recipient_type_code = 'I'
			   AND recipient_no = :ll_recipient
				AND claim_no = :ll_claim_no;
			IF SQLCA.nf_handle_error('Embedded SQL: select from PAYMENTS', 'n_claim_participant', 'nf_inactivate_claimant') < 0 THEN
				Return -1
			END IF

				IF ll_unapplied_no > 0 AND ll_applied_no = 0 THEN
					MessageBox('New Claimant Role Disallowd','Only scheduled payments exists, delete them and then change the individual.  Delete the payments')
					Return -1
				ELSE
					IF ll_unapplied_no > 0 AND ll_applied_no > 0 THEN
						MessageBox('New Claimant Role Disallowd','Delete the scheduled payments, and then add a new claimant.')
						Return -1
					ELSE
/*						only applied claim txns exist so inactivate - need to keep the individual around for history
*/
						IF ll_loop <> il_changed_row THEN
				      	idw_dw[1].SetItem(ll_loop, 'claimant_active_flag', 'N')
						END IF

					END IF
				END IF
	   END IF
   	ll_loop = ll_loop + 1
	LOOP
Return 0
end function

public function integer nf_retrieve (long al_claim_no);LONG  ll_rows


	ll_rows = idw_dw[1].Retrieve(al_claim_no)
  
Return ll_rows

end function

public function integer nf_change_item (integer al_datawindow);DATAWINDOWCHILD	ldwc_child
LONG					ll_current_row
STRING				ls_string

/*	the datawindow which triggered the item changed event can be found in
	the argument - al_datawindow
	In the itemchanged event of the window this function is called passing
	the datawindow corresponding to the array passed in to the nf_init() 
	function
*/

	CHOOSE CASE al_datawindow
   	CASE 1
   	   ll_current_row = idw_dw[1].GetRow()
	    	CHOOSE CASE idw_dw[1].GetColumnName()
         	CASE 'dependent_flag'
					IF idw_dw[1].GetText() = 'N' THEN
						idw_dw[1].GetChild('dependent_reason_code',ldwc_child)
						ldwc_child.SetFilter("dependent_reason_code = ' '")
						ldwc_child.Filter()
						idw_dw[1].SetItem(ll_current_row,'dependent_reason_code',' ')
					ELSE
						idw_dw[1].GetChild('dependent_reason_code',ldwc_child)
						ls_string = idw_dw[1].GetItemString(ll_current_row,'claim_role_code') 
						IF IsNull(ls_string) THEN
							ls_string = ' '
						END IF
						ldwc_child.SetFilter("claim_code = '" + ls_string + "'")
						ldwc_child.Filter()
						idw_dw[1].SetItem(ll_current_row,'dependent_reason_code',' ')
					END IF

				CASE 'claim_role_code'
						idw_dw[1].GetChild('dependent_reason_code',ldwc_child)
						ldwc_child.SetFilter("claim_code = '" +idw_dw[1].GetText() + "'")
						ldwc_child.Filter()
						idw_dw[1].SetItem(ll_current_row,'dependent_reason_code',' ')

			END CHOOSE
	END CHOOSE

Return 0
end function

public function integer nf_check_bus_rule ();LONG  			   ll_individual_no, ll_count, ll_current_row, ll_claim_no, ll_max_row, ll_no, ll_activeawardcount
LONG					ll_activeopeningcount
STRING  				ls_string, ls_string2, ls_legislation_code, ls_role_code, ls_role_code_old
BOOLEAN  			lb_claimant_role
DATAWINDOWCHILD	ldwc_child

	lb_claimant_role = FALSE

	ll_max_row = idw_dw[1].RowCount()
	ll_current_row = 1

/*	there can only be 9 roles defined - active or inactive
	since they have to be transferred to another system with this limitation
*/
	IF ll_max_row > 9 THEN
   	MessageBox('Error - Too many roles', 'There is only a maximum of 9 roles that can be defined for a claim.')
	   Return -1
	END IF

	ls_legislation_code = ''				// set this so it can be checked and only looked up once
	
	DO WHILE ll_current_row <= ll_max_row
/*		an individual can only have one link to a claim
		if an individual exists then validate that they aren't already on the claim
		in another role
		the participant dw contains all the roles and individual numbers for the claim
*/
	   ll_individual_no = idw_dw[1].GetItemNumber(ll_current_row,'individual_no')
       ll_claim_no = idw_dw[1].GetItemNumber(ll_current_row,'claim_no')
	   IF ll_individual_no > 0 THEN
   	      IF ll_current_row > 1 THEN   				// search backwards for a match
         	ll_count = idw_dw[1].Find('individual_no = ' + String(ll_individual_no), 0, ll_current_row - 1)
	          IF ll_count > 0 THEN				       // individual must exist - disallow
      	       MessageBox('Invididual Already Exists','The individual ' + String(ll_individual_no) + ' already plays another role on the claim.~r~n Please correct.')
         	    Return -1
	          END IF
   	      END IF
      	
	      IF ll_current_row < ll_max_row THEN		// search forwards
   	      ll_count = idw_dw[1].Find('individual_no = ' + String(ll_individual_no), ll_current_row + 1, ll_max_row)
      	   IF ll_count > 0 THEN				   	// individual must exist - disallow
         	   MessageBox('Invididual Already Exists','The individual ' + String(ll_individual_no) + ' already plays another role on the claim.~r~n Please correct.')
		         Return -1
      	   END IF
	      END IF
	   END IF

		IF idw_dw[1].GetItemString(ll_current_row, 'claim_role_code') <> 'C' AND idw_dw[1].GetItemString(ll_current_row,'claimant_active_flag') = 'Y' THEN
/*		change the default on the claimant_active_flag
*/			idw_dw[1].SetItem(ll_current_row,'claimant_active_flag','N')
		END IF
/*		can't add an inactive claimant
*/
		IF idw_dw[1].GetItemStatus(ll_current_row,0, Primary!) = NewModified! AND idw_dw[1].GetItemString(ll_current_row, 'claim_role_code') = 'C' AND idw_dw[1].GetItemString(ll_current_row, 'claimant_active_flag') = 'N' THEN
			MessageBox('Error',"A new claimant cannot be added as 'inactive'.")
			Return -1
		END IF

/*		need only one guy as C   - the claimant
*/
  		IF idw_dw[1].GetItemString(ll_current_row, 'claim_role_code') = 'C' AND idw_dw[1].GetItemString(ll_current_row, 'claimant_active_flag') = 'Y' THEN
/*			the claimant always has a sub claim of 0 - this is a link to an older system 
*/
  	   		IF idw_dw[1].GetItemStatus(ll_current_row,0, Primary!) = NewModified! OR idw_dw[1].GetItemStatus(ll_current_row,0, Primary!) = DataModified! THEN
         		idw_dw[1].SetItem(ll_current_row, 'sub_claim_no', 0)
   	   		END IF
			
			IF idw_dw[1].GetItemString(ll_current_row, 'claimant_active_flag') <> idw_dw[1].GetItemString( ll_current_row, 'claimant_active_flag', Primary!, TRUE ) THEN	// pr1794
				il_changed_row = ll_current_row
			END IF

			
      		IF lb_claimant_role THEN
/*				error there can only be one claimant role
*/
  		      	ll_count = MessageBox('Error', 'The role of claimant can only be assigned to one individual at a time.~r~n    Do you wish to inactivate the other claimant?', Question!, YesNo!)
      	   		IF ll_count = 1 THEN
						IF nf_inactivate_claimant() < 0 THEN
							Return -1
						END IF
	         	ELSE
   	         		Return -1
      	   		END IF
	      	ELSE
   	      		lb_claimant_role = TRUE
      		END IF
	   END IF

	//BR 1.160 A Surving Spouse with an annuity account must not change their role code.
		ls_role_code = idw_dw[1].GetItemString(ll_current_row,'claim_role_code')
		
		Select claim_role_code
		Into :ls_role_code_old
		From CLAIM_PARTICIPANT
		Where individual_no = :ll_individual_no
		And  claim_no = :ll_claim_no
		Using SQLCA;
		
		SQLCA.nf_handle_error('n_claim_participant','dw_participant','nf_check_bus_rule')
		
		IF ls_role_code <> ls_role_code_old and ls_role_code_old = 'SS' THEN
			
			Select Count(*)
			Into   :ll_count
			From ANNUITY_ACCOUNT
		    Where individual_no = :ll_individual_no
			Using SQLCA;
		
			SQLCA.nf_handle_error('n_claim_participant','dw_participant','nf_check_bus_rule')
		
			IF ll_count > 0  THEN
				Messagebox('Error','The role code can not change from Surving Spouse when there is an annuity account setup.',Exclamation!)
				Return -1
			END IF

		END IF


/*		active flag can only be changed if no scheduled payments
*/	
		IF idw_dw[1].GetItemString(ll_current_row,'claimant_active_flag') = 'Y' AND idw_dw[1].GetItemString(ll_current_row,'claimant_active_flag',Primary!, TRUE) = 'N' THEN
/*			check for unapplied claim transactions - must remain active if any - (these are payments that have not
			been processed)
*/	
			SELECT Count(*)
			  INTO :ll_no
			  FROM UNAPPLIED_CLAIM_TXN	
			 WHERE recipient_type_code = 'I' 
				AND recipient_no = :ll_individual_no
				AND claim_no = :ll_claim_no;
			IF SQLCA.nf_handle_error('Embedded SQL: select from UNAPPLIED_CLAIM_TXN', 'n_claim_participant', 'nf_check_bus_rule') < 0 THEN
				Return -1
			END IF
			IF ll_no > 0 THEN
				MessageBox('Payments Exist','Cannot inactivate individual while payments are still scheduled.')
				Return -1
			END IF
		END IF
		
		ls_string = idw_dw[1].GetItemString(ll_current_row,'claim_role_code')
		ls_string2 = Trim(idw_dw[1].GetItemString(ll_current_row, 'dependent_reason_code'))
		IF ls_string2 <> '' THEN
			SELECT Count(*)
			  INTO :ll_count
			  FROM Dependent_Reason_Combination
			 WHERE dependent_reason_code = :ls_string2
			   AND claim_role_code = :ls_string;

			IF SQLCA.nf_handle_error('Embedded SQL: select from Dependent_Reason_Combination','n_claim_participant','nf_check_bus_rule') < 0 THEN
				Return -1
			END IF
			IF ll_count < 1 THEN
				MessageBox('Invalid Dependent Reason', 'The dependent reason is not valid for the claim role code chosen.')
				Return -1
			END IF
		END IF
 		IF idw_dw[1].GetItemString(ll_current_row,'dependent_flag') = 'Y' THEN
			IF ls_string = 'DC' OR ls_string = 'OD' THEN
				IF ls_string2 = '' THEN 
					MessageBox('Invalid Dependent Reason','A Dependent Reason must be selected for Dependent Child or Other Dependent',information!)
					RETURN -1
				END IF
			ELSE
				MessageBox('Invalid Dependent Flag', 'The dependent flag can only be set on for the roles "DC" and "OD".')
				Return -1
			END IF
		END IF

		IF idw_dw[1].GetItemString(ll_current_row,'claim_role_code') = 'DC' OR idw_dw[1].GetItemString(ll_current_row,'claim_role_code') = 'OD' THEN
			IF idw_dw[1].GetItemString(ll_current_row,'dependent_flag') <> 'Y' THEN
				IF idw_dw[1].GetItemString(ll_current_row,'pen_survivor_eligibility_flag') = 'Y' THEN
					MessageBox('Invalid Eligibility Flag', 'The eligibility flag can only be set when dependent flag is checked for role code of dependent child or other dependent.')
					Return -1
				END IF
			END IF
		END IF

/*	Check to see the participant is a PERIODIC_RECIPIENT on an active PERIODIC_AWARD of type 'PEN'. If so, then
	can not change pen_survivor_eligibility_flag.
*/
		IF idw_dw[1].GetItemString(ll_current_row,'pen_survivor_eligibility_flag',Primary!,TRUE) = 'Y' and idw_dw[1].GetItemString(ll_current_row,'pen_survivor_eligibility_flag') = 'N' THEN

			SELECT Count(PERIODIC_RECIPIENT.claim_no)
			  INTO :ll_activeawardcount
			  FROM PERIODIC_AWARD,   
					 PERIODIC_RECIPIENT,
					 PERIODIC_AWARD_CONTROL 
			 WHERE PERIODIC_AWARD.claim_no = PERIODIC_RECIPIENT.claim_no
				AND PERIODIC_AWARD.award_no = PERIODIC_RECIPIENT.award_no
				AND PERIODIC_AWARD_CONTROL.award_type_code = PERIODIC_AWARD.award_type_code
				AND PERIODIC_RECIPIENT.recipient_no = :ll_individual_no
				AND PERIODIC_RECIPIENT.recipient_type_code = 'I' 
				AND PERIODIC_AWARD.award_type_code = 'PEN'
				AND PERIODIC_AWARD.award_end_date >= getdate()
				AND PERIODIC_AWARD_CONTROL.processed_date is NULL
				AND PERIODIC_AWARD_CONTROL.period_from_date < PERIODIC_AWARD.award_end_date
			USING SQLCA;


			IF SQLCA.nf_handle_error('Embedded SQL: select from PERIODIC_AWARD', 'n_claim_participant', 'nf_check_bus_rule') < 0 THEN
				Return -1	
			END IF
			IF ll_activeawardcount > 0 THEN
				MessageBox("Invalid Pen Survivor Eligibility Flag","The Pen/Survivor Eligibility can not be unselected for an individual who is attached to an active PERIODIC AWARD.")
				Return -1
			END IF
		END IF

		IF ls_string <> 'C' THEN
/*			If there is a legislation code on the claim then it must be valid for the role chosen
			there is no need to validate this for the claimant, since all codes are valid for roles of type 'C'
*/
			IF ls_legislation_code = '' THEN					//avoid looking it up more than once
				SELECT legislation_code 
				  INTO :ls_legislation_code
				  FROM CLAIM
			    WHERE claim_no = :ll_claim_no
				 USING SQLCA;
				IF SQLCA.nf_handle_error('Embedded SQL: select from CLAIM', 'n_claim_participant', 'nf_check_bus_rule') < 0 THEN
					Return -1	
				END IF
			END IF
/*			now look for the values in the combination table
*/
			SELECT Count(*)
			  INTO :ll_count
			  FROM Claim_Role_Combination
			 WHERE claim_role_code = :ls_string 
			   AND legislation_code = :ls_legislation_code
			 USING SQLCA;
			IF SQLCA.nf_handle_error('Embedded SQL: select from Claim_Role_Code', 'n_claim_participant', 'nf_check_bus_rule') < 0 THEN
				Return -1	
			END IF
			IF ll_count < 1 THEN
				MessageBox('Invalid Claim Role/Legislation Combo', 'The claim role code of "' + ls_string  + '" is not valid for the legislation code of "' + ls_legislation_code + '".')
				Return -1
			END IF
		END IF
		IF idw_dw[1].GetItemString(ll_current_row,'pen_survivor_eligibility_flag') = 'Y' THEN
			SELECT legislation_code
			  INTO :ls_string
			  FROM CLAIM
			 WHERE claim_no = :ll_claim_no;

		   IF SQLCA.nf_handle_error('Embedded SQL: Select from CLAIM',"n_claim_participant","nf_check_bus_rule") < 0 THEN
   		   Return -1
	   	END IF

			IF Trim(ls_string) = '' THEN
				MessageBox('Pension/Survisor Eligibility', 'The eligibility flag can only be set if a legislation code is entered on the claim.')
				Return -1
			END IF

			SELECT Count(*)
			  INTO :ll_activeopeningcount
			  FROM OPENING
			 WHERE claim_no = :ll_claim_no
				AND (opening_type_code = 'PEN'
				 OR  opening_type_code = 'SV' 
				 OR  opening_type_code = 'S1' 
				 OR  opening_type_code = 'S2' )
				AND (benefit_end_date is NULL
				 OR  benefit_end_date > getdate())
			USING SQLCA;
			IF SQLCA.nf_handle_error('Embedded SQL: select count(*) from OPENING', 'n_claim_participant', 'nf_check_bus_rule') < 0 THEN
				Return -1	
			END IF
			IF ll_activeopeningcount = 0 THEN
				MessageBox("Invalid Pen Survivor Eligibility Flag","The Pen/Survivor Eligibility can not be selected for an individual unless there is an active OPENING of type 'PEN' or 'SV' or 'S1' or 'S2'.")
				Return -1
			END IF
		END IF

   	ll_current_row = ll_current_row + 1
	LOOP

	IF NOT lb_claimant_role THEN
   	MessageBox('Error', 'There must be one individual assigned as the claimant.  Please Correct.')
	   Return -1
	END IF

	IF idw_dw[1].DeletedCount() > 0 THEN
/*		this check is performed in the controller module since it may involve
		deleting the individual
		see the check bus rule of n_maintain_participant_controller
		can't delete a role if payments exits
*/
	END IF
	
	il_changed_row = 0
	
Return 0

end function

public function integer nf_log_events ();/* PR4073 - J. Hawker, 2005.02.07 
   Create an event when the Annuity Eligibility record has been deleted.
*/

DATETIME ldtm_annuity_start, ldtm_annuity_end
LONG ll_event_no
n_event_log lnv_event_log

lnv_event_log = CREATE n_event_log

IF ib_delete_annuity THEN
	ll_event_no = lnv_event_log.nf_next_claim_event_no(il_claim_no)
	lnv_event_log.nf_create_auto_event(il_claim_no, ll_event_no,'030', is_comment , 'AR')	
	ib_delete_annuity = FALSE
END IF

DESTROY n_event_log

Return 0
end function

on n_claim_participant.create
call super::create
end on

on n_claim_participant.destroy
call super::destroy
end on

