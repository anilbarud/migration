$PBExportHeader$n_annuity_participant.sru
forward
global type n_annuity_participant from n_pdc
end type
end forward

global type n_annuity_participant from n_pdc
end type
global n_annuity_participant n_annuity_participant

type variables
N_INDIVIDUAL inv_individual
LONG   il_claim_no, il_participant_no, il_annuity_account_no, il_annuity_payout_no
LONG   il_benefit_holder_no, il_represented_by_recipient_no, il_deleted_participant_no
LONG   il_edit_row
STRING is_annuity_role_code, is_recipient_type_code, is_represented_by_recipient_type_code
STRING is_individual_status, is_participant_status
U_DATAWINDOW idw_datawindow[]
end variables

forward prototypes
public function integer nf_insert (long al_row)
public subroutine nf_set_datawindow (u_datawindow adw_dw[], n_transaction anv_transobject)
public function integer nf_retrieve_individual (long al_individual_no)
public function integer nf_update ()
public function integer nf_check_bus_rule ()
public function integer nf_can_delete ()
public function integer nf_save ()
public function integer nf_change_item (long al_datawindow)
public subroutine nf_reset ()
public function integer nf_garnishment_br_1_320 (string as_court_order_flag, string as_garnish_flag, long al_individual_no)
public function integer nf_garnishment_br_1_310 (string as_court_order_flag, string as_garnish_flag, long al_individual_no)
end prototypes

public function integer nf_insert (long al_row);
il_edit_row = idw_datawindow[2].getRow()
inv_individual.nf_insert(0)
Return il_edit_row
end function

public subroutine nf_set_datawindow (u_datawindow adw_dw[], n_transaction anv_transobject);Int li_cntr = 1

Do While li_cntr 				<=	UpperBound(adw_dw)
	idw_datawindow[li_cntr] 			=	adw_dw[li_cntr]
	idw_datawindow[li_cntr].SetTransObject(anv_transobject)
	li_cntr ++
Loop

inv_transobj 	=	anv_transobject
end subroutine

public function integer nf_retrieve_individual (long al_individual_no);
LONG ll_row, ll_row2

/*	this function retrieve the individual information only
	it would be called when the user wants to change the individual on the claim
	it must change the individual number on the claim participant row also
*/

	ll_row = inv_individual.nf_retrieve(al_individual_no)
//	IF ll_row > 0 THEN
//		ll_row2 =	idw_datawindow[1].getRow()
//   		idw_datawindow[1].SetItem(ll_row2, 'recipient_no', al_individual_no)
//	END IF
Return ll_row
end function

public function integer nf_update ();/*************************************************************************
	Description:		This function updates the datawindows in the array.
		
						Then this script updates array of u_datawindows
*************************************************************************/

LONG ll_error
INT	li_cntr = 1, li_bound

li_bound = UpperBound(idw_datawindow)
DO WHILE li_cntr <= li_bound
	idw_datawindow[li_cntr].Update()
   ll_error = inv_transobj.nf_handle_error("n_pdc","nf_update","updateing dw")
   IF ll_error < 0 THEN
      Return ll_error
   END IF
	// Error Handling
	li_cntr ++
LOOP
Return 0

end function

public function integer nf_check_bus_rule ();
INT li_rtn, li_found_row, li_row_count, li_current_row, li_detail_row, li_count, li_age, li_cntr, li_count2
LONG  ll_recipient_no, ll_represented_by_recipient_no, ll_represented_by_recipient_no_for_bh, ll_temp_recip_no
STRING  ls_found_exp, ls_recipient_type_code,ls_annuity_role_code, ls_annuity_payout_writeoff_flag, ls_value, ls_value2
STRING  ls_annuity_payout_writeoff_flag_for_bh,  ls_represented_by_annuity_role_code, ls_garnish_flag, ls_garnish_flag_for_bh
STRING  ls_annuity_entitlement_flag, ls_annuity_eligibility_flag, ls_annuity_entitlement_flag_temp, ls_annuity_eligibility_flag_temp
STRING  ls_court_order_flag
DATETIME ldt_death_date, ldt_concluded_date, ldt_death_date_recipient, ldt_65th_birth_day_date_bh, ldt_death_date_temp
DWItemStatus   ldw_status1, ldw_status2

// check for a deleted row, and verify the business rules for deletions
IF idw_datawindow[1].deletedCount() > 0 THEN
	li_rtn = nf_can_delete()
	// this is a deletion so after the check, return something, no requirement to do further BR checks
	IF li_rtn < 0 THEN 
		RETURN -1
	ELSE
		RETURN 0
	END IF
END IF

li_row_count = idw_datawindow[1].rowCount()
li_current_row = idw_datawindow[1].getRow()

li_detail_row = idw_datawindow[2].getRow()   // current row of the details data window


//1.10		The individual, either injured worker or surviving spouse, who is eligible for the annuity benefits must be an annuity participant for the annuity payout. (Refer to Rationale)
ls_found_exp = "recipient_no = " + STRING(il_benefit_holder_no)
li_found_row = idw_datawindow[1].find(ls_found_exp, 1, li_row_count)
IF li_found_row <= 0 THEN
	MESSAGEBOX("BR1.10","The individual, either injured worker or surviving spouse, who is eligible for the annuity benefits " &
									+ "~rmust be an annuity participant for the annuity payout.", INFORMATION!)
	RETURN -1
END IF

IF li_detail_row > 0 THEN
	ls_recipient_type_code          = idw_datawindow[2].getItemString(li_detail_row, 'recipient_type_code')
	ls_annuity_role_code            = idw_datawindow[2].getItemString(li_detail_row, 'annuity_role_code')
	ll_recipient_no                 = idw_datawindow[2].getItemNumber(li_detail_row, 'recipient_no')
	ll_represented_by_recipient_no  = idw_datawindow[2].getItemNumber(li_detail_row, 'represented_by_recipient_no')
	ls_annuity_payout_writeoff_flag = idw_datawindow[2].getItemString(li_detail_row, 'annuity_payout_writeoff_flag')
	ls_garnish_flag                 = idw_datawindow[2].getItemString(li_detail_row, 'annuity_payout_garnish_flag')


	//used later on
	IF ll_recipient_no = il_benefit_holder_no THEN
		ll_represented_by_recipient_no_for_bh  = idw_datawindow[2].getItemNumber(li_detail_row, 'represented_by_recipient_no')
		ls_annuity_payout_writeoff_flag_for_bh = idw_datawindow[2].getItemString(li_detail_row, 'annuity_payout_writeoff_flag')
		ls_garnish_flag_for_bh                 = idw_datawindow[2].getItemString(li_detail_row, 'annuity_payout_garnish_flag')
	ELSE
		ll_represented_by_recipient_no_for_bh  = idw_datawindow[1].getItemNumber(li_found_row, 'represented_by_recipient_no')
		ls_annuity_payout_writeoff_flag_for_bh = idw_datawindow[1].getItemString(li_found_row, 'annuity_payout_writeoff_flag')
		ls_garnish_flag_for_bh                 = idw_datawindow[1].getItemString(li_found_row, 'annuity_payout_garnish_flag')
	END IF	
	
	//BR1.70	 An annuity participant must have an active annuity role.
	//  controlled by the dropdown list, it contains only active anuity roles during an ADD function
	IF isnull(ls_annuity_role_code) OR ls_annuity_role_code = "" THEN
		MESSAGEBOX("BR1.70", "An annuity participant must have an active annuity role.", EXCLAMATION!)
		RETURN -1
	END IF
	
	
	//1.20		The injured worker who is eligible for the annuity benefits must have an annuity role of injured worker, if the annuity payout is for an injured worker.
	IF ll_recipient_no = il_benefit_holder_no THEN
		ls_value = idw_datawindow[1].getItemString(li_found_row, 'annuity_role_code')
		IF ls_value = 'C' and ls_annuity_role_code <> 'C' THEN
			MESSAGEBOX("BR1.20", "The injured worker who is eligible for the annuity benefits must have an annuity role of injured worker, ~rif the annuity payout is for an injured worker.", EXCLAMATION!)
			RETURN -1
		END IF	
	
	//1.30    The surviving spouse who is eligible for the annuity benefits must have an annuity role of surviving spouse, if the annuity payout is for a surviving spouse. 
		IF ls_value = 'SS' and ls_annuity_role_code <> 'SS' THEN
			MESSAGEBOX("BR1.30", "The surviving spouse who is eligible for the annuity benefits must have an annuity role of surviving spouse, ~rif the annuity payout is for a surviving spouse. ", EXCLAMATION!)
			RETURN -1
		END IF	
	END IF
	
	//1.40		Annuity participants, other than the injured worker, who is eligibile for the annuity benefits, must not be assigned the annuity role of injured worker.  
	IF ll_recipient_no <> il_benefit_holder_no THEN
		IF  ls_annuity_role_code = 'C' THEN
			MESSAGEBOX("BR1.40", "Annuity participants, other than the injured worker, who is eligibile for the annuity benefits, ~rmust not be assigned the annuity role of injured worker.", EXCLAMATION!)
			RETURN -1
		END IF
		
		//1.50		Annuity participants, other than the surviving spouse, who is eligibile for the annuity benefits, must not be assigned the annuity role of  surviving spouse	
		IF  ls_annuity_role_code = 'SS' THEN
			MESSAGEBOX("BR1.50", "Annuity participants, other than the surviving spouse, who is eligibile for the annuity benefits, ~rmust not be assigned the annuity role of surviving spouse.", EXCLAMATION!)
			RETURN -1
		END IF		
	END IF
	
	
	//BR1.60	  There may be, at most, one annuity participant in the annuity role of dependent spouse for an annuity payout. 
	ldw_status1 = idw_datawindow[2].getItemStatus(li_detail_row, 0, PRIMARY!)

	IF ldw_status1 = New! or ldw_status1 = NewModified! THEN
		ls_found_exp = "annuity_role_code = '01'"
		li_found_row = idw_datawindow[1].find(ls_found_exp, 1, li_row_count)
		IF ls_annuity_role_code = '01' and li_found_row > 0 THEN
			MESSAGEBOX("BR1.60", "There may be, at most, one annuity participant in the annuity role of dependent spouse for an annuity payout.", EXCLAMATION!)
			RETURN -1
		END IF	
	END IF
	
	//BR1.80    An annuity participant must have a recipient type of individual or other payee. 
	IF ls_recipient_type_code <> 'I' and ls_recipient_type_code <> 'O' OR isnull(ls_recipient_type_code) THEN
		MESSAGEBOX("BR1.80", "An annuity participant must have a recipient type of individual or other payee.", EXCLAMATION!)
		RETURN -1
	END IF
	
	//BR 1.90	An annuity participant must have a recipient type that corresponds to their annuity role. 
	SELECT COUNT(*)
	INTO   :li_count
	FROM   Annuity_Role_Recipient_Type_Xref
	WHERE  annuity_role_code = :ls_annuity_role_code
	AND    recipient_type_code = :ls_recipient_type_code;
	SQLCA.nf_handle_error("n_annuity_participant", "nf_check_bus_rule", "Select count from Annuity_Role_Recipient_Type_Xref")
	
	IF li_count <= 0 THEN
		MESSAGEBOX("BR1.90", "An annuity participant must have a recipient type that corresponds to their annuity role.", EXCLAMATION!)
		RETURN -1
	END IF
	
	// BR 1.100	The annuity role and recipient type combination must be active.
	SELECT COUNT(*)
	INTO   :li_count
	FROM   Annuity_Role_Recipient_Type_Xref
	WHERE  annuity_role_code = :ls_annuity_role_code
	AND    recipient_type_code = :ls_recipient_type_code
	AND    active_flag = 'Y';
	SQLCA.nf_handle_error("n_annuity_participant", "nf_check_bus_rule", "Select count from Annuity_Role_Recipient_Type_Xref")
	
	IF li_count <= 0 THEN
		MESSAGEBOX("BR1.00", "An annuity participant must have a recipient type that corresponds to their annuity role.", EXCLAMATION!)
		RETURN -1
	END IF

  //BR1.110	An annuity participant must be an individual or provider. 
	IF ls_recipient_type_code = 'I' THEN
		
		Select count(*)
		INTO  :li_count
		FROM  INDIVIDUAL
		WHERE individual_no = :ll_recipient_no;		
		SQLCA.nf_handle_error("n_annuity_participant", "nf_check_bus_rule", "Select from INDIVIDUAL")
		
		IF li_count <= 0 THEN
			MESSAGEBOX("BR1.110", "The recipient number is invalid or does not exist for the selected recipient type code of 'Individual'", EXCLAMATION!)
			RETURN -1
		END IF
		
	ELSEIF ls_recipient_type_code = 'O' THEN
		Select count(*)
		INTO  :li_count
		FROM  PROVIDER
		WHERE provider_no = :ll_recipient_no
		AND   provider_type_code = 'O';		
		SQLCA.nf_handle_error("n_annuity_participant", "nf_check_bus_rule", "Select count(*) from PROVIDER")
		
		IF li_count <= 0 THEN 
			MESSAGEBOX("BR1.110", "The recipient number is invalid or does not exist for the selected recipient type code of 'Other Payee'.", EXCLAMATION!)
			RETURN -1
		END IF
	END IF
	
	//BR1.120	The provider must be active, if an annuity participant is a provider. 	
	IF ls_recipient_type_code = 'O' THEN
		SELECT active_flag
		INTO   :ls_value
		FROM   PROVIDER
		WHERE  provider_no = :ll_recipient_no;
		SQLCA.nf_handle_error("n_annuity_participant", "nf_check_bus_rule", "Select active_flag from PROVIDER")
		
		IF ls_value <> 'Y' THEN
			MESSAGEBOX("BR1.120", "The provider must be active, if an annuity participant is a provider.", EXCLAMATION!)
			RETURN -1
		END IF
	END IF

	//BR 1.130   The provider must be of type ‘Other’, if the annuity participant is of recipient type other payee. 
	//          THIS IS CHECKED IN THE buttonclicked event of dw_participant_details on w_prepare_annuity_account. 

	
	
	//BR 1.132	A dependent annuity participant, if deceased, must have died:
	//				•	after the death date of the individual who is eligible for annuity benefits, if deceased prior to the age of 65 or
	//				•	after the 65th birthday of the individual who is eligible for annuity benefits
	
	//first get some data - used here and in other rules later on
	SELECT annuity_entitlement_flag, annuity_eligibility_flag
	INTO   :ls_annuity_entitlement_flag, :ls_annuity_eligibility_flag
	FROM   Annuity_Role
	WHERE  annuity_role_code = :ls_annuity_role_code;
	SQLCA.nf_handle_error("n_annuity_participant", "nf_check_bus_rule", "Select annuity_entitlement_flag from Annuity_Role")

	SELECT CASE
				WHEN death_date is Null 
				THEN FLOOR(DATEDIFF(day, birth_date, getdate()) / 365.25)
				ELSE  FLOOR(DATEDIFF(day, birth_date, death_date) / 365.25)
				END,
				death_date,
				DATEADD(year,65, birth_date)
	INTO   :li_age, :ldt_death_date, :ldt_65th_birth_day_date_bh
	FROM   INDIVIDUAL
	WHERE  individual_no = :il_benefit_holder_no;
	SQLCA.nf_handle_error("n_annuity_participant", "nf_check_bus_rule", "Select death_date, age from INDIVIDUAL for benefit holder")

	SELECT death_date
	INTO   :ldt_death_date_recipient
	FROM   INDIVIDUAL
	WHERE  individual_no = :ll_recipient_no;
	SQLCA.nf_handle_error("n_annuity_participant", "nf_check_bus_rule", "Select death_date from INDIVIDUAL for recipient")
		
	IF ll_recipient_no <> il_benefit_holder_no THEN		
		//a participant is considered a 'dependant' when their annuity role has its annuity_eligibility_flag = 'N' AND annuity_entitlement_flag = 'Y'
		IF ls_annuity_eligibility_flag = 'N' AND ls_annuity_entitlement_flag = 'Y'  AND NOT isnull(ldt_death_date_recipient) THEN	// this line evaluates to a recipient who is a 'dependant' and has died.
		    
			 // check if  participant died on or before the death date of the benefit holder (who died before reaching 65) OR the participant died on or before benefit holder's 65'th birthday
			IF (NOT isnull(ldt_death_date) AND li_age < 65 AND ldt_death_date_recipient <= ldt_death_date)   &   
			    OR (li_age >= 65 AND ldt_death_date_recipient <= ldt_65th_birth_day_date_bh)  THEN  
				MESSAGEBOX("BR1.132", "A dependent annuity participant, if deceased, must have died:" & 
												+ "~r~n   • after the death date of the individual who is eligible for annuity benefits, if deceased prior to the age of 65 or" &
											    + "~r~n   • after the 65th birthday of the individual who is eligible for annuity benefits" , EXCLAMATION!)
				RETURN -1
			END IF
		END IF	
	END IF
	
	
	//BR1.134	An estate may only represent a deceased dependent (or themselves). 
	SELECT annuity_role_code
	INTO   :ls_value
	FROM   ANNUITY_PAYOUT_PARTICIPANT
	WHERE  recipient_no = :ll_represented_by_recipient_no
	AND    annuity_payout_no = :il_annuity_payout_no;
	SQLCA.nf_handle_error("n_annuity_participant", "nf_check_bus_rule", "Select annuity_role_code from ANNUITY_PAYOUT_PARTICIPANT WHERE recipient_no = :ll_represented_by_recipient_no")

	IF ls_value = '18' AND ll_represented_by_recipient_no <> ll_recipient_no THEN              // the represented_by_recipient is an 'estate', and they are representing someone other than themselves
		IF ls_annuity_eligibility_flag = 'N' AND ls_annuity_entitlement_flag = 'Y'  AND NOT isnull(ldt_death_date_recipient) THEN         // this equates to a recipient that is a dependant  and is deceased
			// OK, this is allowed, as this is an estate representing a deceased dependant
		ELSE
			// Any thing else is not allowed
			MESSAGEBOX("BR1.134", "An estate may only represent a deceased dependent (or themselves).", EXCLAMATION!)
			RETURN -1
		END IF
	END IF
	

	
	/*BR1.140    The provider name must be ‘Estate of’ or ‘Succession de’ or ‘Succession d’ ‘ plus the given names and last name 
                       of one of the deceased dependents, if the annuity role of the annuity participant is estate.

			i.e:		If recipient being added has an annuity role of estate, then 2 things must exist:
					• At least one deceased dependant must exist and
					• The provider name of the estate must be 'Estate of', or 'Success de', or 'Succession d'   plus the 
					   given names and last name of the individual who is entitled to the annuity benefits (the deceased dependant)
				   
					IF either condion is false, exit with an error message
				   IF both conditions are true, set this annuity participant (estate) as the representative of the decesaed dependant that was located	
	*/	
	
	IF ldw_status1 = New! or ldw_status1 = NewModified! THEN
		ls_value =  idw_datawindow[2].getItemString(li_detail_row,'recipient_name_and_address')
		ls_value = UPPER(LEFT(ls_value, POS(ls_value, CHAR(13)) - 1 ))
		IF ls_annuity_role_code = '18' THEN   // code for annuity role of 'Estate'	
			li_count = 0
			
			FOR li_cntr = 1 to li_row_count
				ls_annuity_eligibility_flag_temp     = idw_datawindow[1].getItemString(li_cntr,'annuity_eligibility_flag')
				ls_annuity_entitlement_flag_temp = idw_datawindow[1].getItemString(li_cntr,'annuity_entitlement_flag')
				IF ls_annuity_eligibility_flag_temp = 'N' AND ls_annuity_entitlement_flag_temp = 'Y' THEN  // found a 'dependant' in the annuity participant list, now check for a deceased date
					ll_temp_recip_no = idw_datawindow[1].getItemNumber(li_cntr,'recipient_no')
					ls_value2           = idw_datawindow[1].getItemString(li_cntr,'recipient_name')
					
					SELECT death_date 
					INTO   :ldt_death_date_temp
					FROM   INDIVIDUAL
					WHERE individual_no = :ll_temp_recip_no;
					SQLCA.nf_handle_error("n_annuity_participant", "nf_check_bus_rule", "Select death_date from INDIVIDUAL for recipient")
					
					IF NOT isnull(ldt_death_date_temp) THEN   // we now have a 'deceased dependant'
						IF UPPER(ls_value) = "ESTATE OF "+ ls_value2  OR UPPER(ls_value) = "SUCCESSION DE "+ ls_value2 OR UPPER(ls_value) = "SUCCESSION D'"+ ls_value2  THEN
							li_count = 1							
							exit  //  exit the loop
						END IF
					END IF
				END IF
			NEXT
			// no deceased dependants with ESTATE OF or SUCCESSION DE or SUCCESSION D plus given names and last name, were found
			IF li_count = 0 THEN
				MESSAGEBOX("BR1.140", "BR1.140 - The provider name must be ‘Estate of’ or ‘Succession de’ or ‘Succession d’ ‘ plus the given names and  last name " &
											+    "~r~nof one of the deceased dependents, if the annuity role of the annuity participant is 'Estate'." &
											+    "~r~n~r~nCheck that the name of the estate matches the name of a dependant, and that the dependant is deceased.", EXCLAMATION!)														
				RETURN -1
			END IF
		END IF
	END IF
	
	IF ls_annuity_role_code <> '18' THEN
	//1.145    The provider name must not contain ‘Estate’ or ‘Succession‘, if the annuity role of the annuity participant is not estate.
		
		IF MATCH(UPPER(ls_value),"ESTATE") OR MATCH(UPPER(ls_value),"SUCCESSION") THEN
			MESSAGEBOX("BR1.145", "The provider name must not contain '‘Estate' or 'Succession', if the role of the annuity participant is other than estate.", EXCLAMATION!)
			RETURN -1
		END IF	
	END IF
	
				
	//BR1.150    An annuity participant must participate in only one annuity role for the annuity payout.
	// this really means that no individual can exist as a participant for a given annuity payout more than once, regardless of what roles they play
	ls_found_exp = "recipient_no = " + STRING(ll_recipient_no) 
	
	ldw_status1 = idw_datawindow[2].getItemStatus(li_detail_row, 0, PRIMARY!)
	//ldw_status2 = idw_datawindow[2].getItemStatus(li_detail_row, 'represented_by_recipient_no', PRIMARY!)
	li_count = 0
	
	// If this is a new row, check for the existence of this individual, if exists, error out
	IF ldw_status1 = New! OR ldw_status1 = NewModified! THEN
		li_found_row = idw_datawindow[1].find(ls_found_exp, 1, li_row_count)

		IF  li_found_row >= 1 THEN
			MESSAGEBOX("BR1.150","An annuity participant must participate in only one annuity role for the annuity payout.", INFORMATION!)
			RETURN -1
		END IF
	END IF
	
	//BR1.160   An annuity participant must have a representative; either her/himself or another annuity participant (Refer to Rationale)
	// look in the participant list, for a recipient_no matching the represented_by_recipient_no
	ls_found_exp = "recipient_no = " + STRING(ll_represented_by_recipient_no) 
	li_found_row = idw_datawindow[1].find(ls_found_exp, 1, li_row_count)
	
	IF li_found_row <= 0 THEN
		//no recipient_no found in existing participant list; now check the current entry dw (details data window)
		li_found_row = idw_datawindow[2].find(ls_found_exp, li_detail_row, li_detail_row)
		IF li_found_row <= 0 THEN
			MESSAGEBOX("BR1.160"," An annuity participant must have a representative; either him/herself or another annuity participant.", INFORMATION!)
			RETURN -1
		END IF
	END IF
	
	//BR1.170	A representative must be an annuity participant of the annuity payout.	
	/*  Enforced by the fact that represented by recipient_no must be selected from the dropdown list, 
	 	 which only includes existing annuity participants... AND, in the case where a participant is being deleted
	 	 and is currently a representative for another participant, this rule is checked in nf_can_delete()
	*/
	
	//BR1.180    There must not be annuity participants other than the individual who is eligible for the annuity   
	//               benefits and her/his representative, if the individual has reached the age of 65 and is not deceased.
	//              BUT! We will allow them to add a Guardian or Trustee if they are >= 65 and not deceased 
	//               In this case, this new particpant will be set as the benefit holder's representative automatically
           
	
	IF ldw_status1 = New! OR ldw_status1 = NewModified! AND li_age >= 65 AND isnull(ldt_death_date) THEN	
		IF ls_annuity_role_code <> 'GU' and ls_annuity_role_code <> 'TR'  THEN										   
			
			MESSAGEBOX("BR1.180", "There must not be annuity participants other than the individual who is eligible for the annuity benefits " &
										 + "~rand his/her representative, if the individual has reached the age of 65 and is not deceased." &
										 + "~r~r(You can add a Guardian or Trustee if the benefit holder has reached the age of 65 and is not deceased.)")
			RETURN -1
		END IF
		
		// Next check to see if there are any participants with one of these annuity roles, that already exst. 
		// They have to use one of those, or delete them all before adding a new one
		ls_found_exp = "annuity_role_code = 'GU' OR annuity_role_code = 'TR'" 
		li_found_row = idw_datawindow[1].find(ls_found_exp, 1, li_row_count)
		IF li_found_row > 0 THEN
			MESSAGEBOX("BR1.180","Please delete any existing annuity participants of type Guardian or Trustee before adding a new one.", INFORMATION!)
			RETURN -1
		END IF		
	END IF
	
	//BR1.190   The representative of the individual who is eligible for the annuity benefits must be an annuity participant in an 
	//               annuity role of Trustee or Guardian, if the individual reached the age of 65, is not deceased and is not representing her/himself.
	IF  isnull(ldt_death_date) AND li_age >= 65 THEN
		// are we working on the benefit holder?
		IF ll_recipient_no = il_benefit_holder_no THEN		
			SELECT annuity_role_code
			INTO   :ls_value
			FROM   ANNUITY_PAYOUT_PARTICIPANT
			WHERE  recipient_no = :ll_represented_by_recipient_no_for_bh
			AND    annuity_payout_no = :il_annuity_payout_no;
			SQLCA.nf_handle_error("n_annuity_participant", "nf_check_bus_rule", "Select annuity_role_code from ANNUITY_PAYOUT_PARTICIPANT")
		
		//are we working on the participant that represents the benefit holder ?
		ELSEIF ll_recipient_no = ll_represented_by_recipient_no_for_bh THEN
			 ls_value = ls_annuity_role_code
		ELSE
			// neither - must be working on someone other than the benefit holder or the representative of the benefit holder
			ls_value = ""
		END IF
		

		IF ls_value > "" AND ls_value <> 'TR' AND ls_value <> 'GU' AND ll_represented_by_recipient_no_for_bh <> il_benefit_holder_no THEN
			MESSAGEBOX("BR1.190", "The representative of the individual who is eligible for the annuity benefits must be an annuity participant in an annuity " &
											+ "~rrole of Trustee or Guardian, if the individual reached the age of 65, is not deceased and is not representing her/himself.")
			RETURN -1
		END IF		
	END IF
	

	//BR1.200   The representative of the individual who is eligible for the annuity benefits must be her/himself, if the individual is deceased. 
	IF  NOT isnull(ldt_death_date) AND  ll_recipient_no = il_benefit_holder_no THEN
		IF ll_recipient_no <> ll_represented_by_recipient_no THEN
			MESSAGEBOX("BR1.200", "The representative of the individual who is eligible for the annuity benefits must be her/himself, if the individual is deceased.")
			RETURN -1
		END IF
	END IF
	
	
	// BR1.210	 A deceased dependant must be represented by his/her estate or her/himself.
		
	IF ldw_status1 = DataModified! THEN
		li_found_row =  idw_datawindow[1].find('recipient_no = ' + STRING(ll_represented_by_recipient_no), 1, li_row_count)
		IF li_found_row <= 0 THEN RETURN -1
		ls_represented_by_annuity_role_code =  idw_datawindow[1].getItemString(li_found_row,'represented_by_annuity_role_code')
		li_count = 0
		IF ls_annuity_eligibility_flag = 'N' AND ls_annuity_entitlement_flag = 'Y' THEN  // this is a 'dependant', now check for a deceased date
							
			SELECT death_date 
			INTO   :ldt_death_date_temp
			FROM   INDIVIDUAL
			WHERE  individual_no = :ll_recipient_no;
			SQLCA.nf_handle_error("n_annuity_participant", "nf_check_bus_rule", "Select death_date from INDIVIDUAL for recipient")
			
			IF NOT isnull(ldt_death_date_temp) THEN  
	  		// this is a deceased 'dependant'  - check the existing representative, to see if its the deceased dependants estate
				IF ls_represented_by_annuity_role_code = '18' THEN // yep , its an estate, 
					// now check if this estate is for the deceased dependant (estate name contains the dependant's name)
					ls_value = idw_datawindow[2].getItemString(li_detail_row,'represented_by_name_and_address')
					ls_value = UPPER(LEFT(ls_value, POS(ls_value, CHAR(13)) - 1 ))	
					
					ls_value2 = idw_datawindow[2].getItemString(li_detail_row,'recipient_name_and_address')
					ls_value2 = UPPER(LEFT(ls_value2, POS(ls_value2, CHAR(13)) - 1 ))	
					
					IF UPPER(ls_value) <> "ESTATE OF "+ UPPER(ls_value2)  AND  UPPER(ls_value) <> "SUCCESSION DE "+ UPPER(ls_value2)  AND UPPER(ls_value) <> "SUCCESSION D'"+ UPPER(ls_value2)  THEN
						MESSAGEBOX("BR1.210",  "A deceased dependant must be represented by his/her estate or her/himself. The name of the estate is not consistent with the name of the dependant." , EXCLAMATION!) 
						Return -1
					END IF
				ELSE
					// BR1.210	A deceased dependent must be represented by an estate or her/himself. (Refer to Rationale)
					// We have determined that the current participant is a deceased dependant, so if their representative is not an
					// estate, then they must represent themselves, orelse BR1.215 fails
					IF  ll_represented_by_recipient_no <> ll_recipient_no THEN
						MESSAGEBOX("BR1.210",  "A deceased dependant must be represented by his/her estate or her/himself." , EXCLAMATION!) 
						Return -1						
					END IF
				END IF
			END IF
		END IF
	END IF


	//BR 1.220   The annuity benefits of the individual who is eligible for the annuity benefits must not be written off, 
	//                if the individual is deceased and there are dependents

	IF  NOT isnull(ldt_death_date)  THEN
		ls_found_exp = "annuity_eligibility_flag = 'N' AND annuity_entitlement_flag = 'Y'"
		li_found_row = idw_datawindow[1].find(ls_found_exp, 1, li_row_count)
		
		// if there are dependants currently existing in the list, (li_found_row > 0) OR...
		// the individual being added now is not a dependant, AND annuity_payout_writeoff_flag for benefit holder is 'Y', then create a message
		IF (li_found_row > 0  OR  (ls_annuity_eligibility_flag = 'N' AND ls_annuity_entitlement_flag = 'Y')) AND ls_annuity_payout_writeoff_flag_for_bh = 'Y' THEN
			MESSAGEBOX("BR1.220", "The annuity benefits of the individual who is eligible for the annuity benefits must not be written off," &
										 +   "~r~nif the individual is deceased and there are dependants.")
			RETURN -1
		END IF
	END IF	
	

	//BR1.230   The annuity benefits of a dependent who is entitled to annuity benefits must not be written off, 
	//                if the dependent is deceased and s/he has a representative in the annuity role of estate.
	IF ls_annuity_eligibility_flag = 'N' AND ls_annuity_entitlement_flag = 'Y'  THEN
		IF NOT isnull(ldt_death_date_recipient) AND ls_represented_by_annuity_role_code = '18' AND ls_annuity_payout_writeoff_flag = 'Y' THEN
			IF ldw_status1 = DataModified! THEN 
				MESSAGEBOX("BR1.230", "The annuity benefits of a dependant who is entitled to annuity benefits must not be written off, " &
											+   "~r~nif the dependant is deceased and s/he has a representative in the annuity role of estate.", INFORMATION!)
				RETURN -1
			END IF
		END IF		
	END IF
	
	//BR 1.235 The annuity benefits of an annuity payout participant must not be written off, if s/he represents 
	//another annuity payout participant whose annuity benefits are not written off. (Refer to Rationale)
	
	IF ls_annuity_payout_writeoff_flag = 'Y' THEN
		// prevent checking of writeoff, if person represents someone who is not written off
		SELECT Count(*)
		INTO   :li_count
		FROM   ANNUITY_PAYOUT_PARTICIPANT a
		WHERE  a.annuity_payout_no            = :il_annuity_payout_no
		AND    a.recipient_no                 = :ll_recipient_no
		AND    a.recipient_type_code          = :ls_recipient_type_code
		AND EXISTS ( SELECT *
						 FROM   ANNUITY_PAYOUT_PARTICIPANT b
						 WHERE  b.annuity_payout_no                   = a.annuity_payout_no
						 AND    b.represented_by_recipient_no         = a.recipient_no
						 AND    b.represented_by_recipient_type_code  = a.recipient_type_code
						 AND  ( b.represented_by_recipient_no        <> b.recipient_no
						    OR  b.represented_by_recipient_type_code <> b.recipient_type_code )
						 AND    b.annuity_payout_writeoff_flag        = 'N')
		USING SQLCA;
		SQLCA.nf_handle_error("n_annuity_participant", "nf_check_bus_rule", "Select count(*) from ANNUITY_PAYOUT_PARTICIPANT (BR 1.235-1)")
	ELSE
		// prevent unchecking of writeoff, if represented by someone who is written off
		SELECT Count(*)
		INTO   :li_count
		FROM   ANNUITY_PAYOUT_PARTICIPANT a
		WHERE  a.annuity_payout_no            = :il_annuity_payout_no
		AND    a.recipient_no                 = :ll_recipient_no
		AND    a.recipient_type_code          = :ls_recipient_type_code
		AND  ( a.recipient_no                <> a.represented_by_recipient_no
          OR a.recipient_type_code         <> a.represented_by_recipient_type_code )
		AND EXISTS ( SELECT *
						 FROM   ANNUITY_PAYOUT_PARTICIPANT b
						 WHERE  b.annuity_payout_no            = a.annuity_payout_no
						 AND    b.recipient_no                 = a.represented_by_recipient_no
						 AND    b.recipient_type_code          = a.represented_by_recipient_type_code
						 AND    b.annuity_payout_writeoff_flag = 'Y')
		USING SQLCA;
		SQLCA.nf_handle_error("n_annuity_participant", "nf_check_bus_rule", "Select count(*) from ANNUITY_PAYOUT_PARTICIPANT (BR 1.235-2)")		
	END IF
	
	IF li_count > 0 THEN
		MESSAGEBOX("BR1.235", "The annuity benefits of an annuity payout participant must not be written off, if s/he represents "&
									+"another annuity payout participant whose annuity benefits are not written off.", INFORMATION!)
		RETURN -1
	END IF
	
	
	//BR1.240  	The individual who is eligible for the annuity benefits must not represent other annuity participants. (Refer to Rationale) – Annuity_Role annuity_eligibility_flag=Y
	IF ldw_status1 = DataModified! AND ll_recipient_no <> il_benefit_holder_no  AND ll_represented_by_recipient_no = il_benefit_holder_no THEN
		MESSAGEBOX("BR1.240", " The individual who is eligible for the annuity benefits must not represent other annuity participants.", INFORMATION!)
		RETURN -1 
	END IF  
 
		
	//BR1.250	An annuity participant must represent her/himself, if s/he is not entitled to annuity benefits.
	IF ldw_status1 = DataModified! AND ll_recipient_no <> il_benefit_holder_no THEN
		IF ls_annuity_entitlement_flag = 'N' AND ll_represented_by_recipient_no <>  ll_recipient_no THEN
			MESSAGEBOX("BR1.250", "An annuity participant must represent her/himself, if s/he is not entitled to annuity benefits.", INFORMATION!)
			RETURN -1
		END IF
	END IF
	
	//BR1.260	Annuity participants must not be maintained, if the annuity eligibility has not been confirmed for payout purposes 
	//          Rationale: Participant maintenance can only be done after confirm annuity eligibility has been completed and before 
	//          confirm overpayment recovery has been completed. (i.e. ANNUITY_PAYOUT annuity_eligibility_confirmed_flag = ‘Y’ 
	//          AND   ANNUITY_PAYOUT overpayment_recovery_confirmed_flag = ‘N’)
	
	SELECT annuity_eligibility_confirmed_flag, overpayment_recovery_confirmed_flag 
	INTO   :ls_value, :ls_value2
	FROM   ANNUITY_PAYOUT
	WHERE  annuity_payout_no = :il_annuity_payout_no;
	SQLCA.nf_handle_error("n_annuity_participant", "nf_check_bus_rule", "Select annuity_eligibility_confirmed_flag from ANNUITY_PAYOUT")
			
	IF ls_value = 'N'  THEN
		MESSAGEBOX("BR1.260", "Annuity participants must not be maintained, if the annuity eligibility has NOT been confirmed for payout purposes", INFORMATION!)
		RETURN -1
	END IF
	
	//BR1.270	Annuity participants must not be maintained, if overpayment recovery has been confirmed.
	IF ls_value2 = 'Y'  THEN
		MESSAGEBOX("BR1.270", "Annuity participants must not be maintained, if overpayment recovery has been confirmed.", INFORMATION!)
		RETURN -1
	END IF
	
	
	// BR1.280	Individuals must be available to be linked as annuity participants, if all of the following are true:
	//					•	The individual must not be the individual who is eligible for the annuity benefits
	//					•	The individual must be a claim participant of a claim in which the individual who is eligible for annuity benefits is a claim participant
	//					•	The claim role of the claim participant must not be claimant.
	//					•	The individual must not be an annuity participant for the annuity payout   (i.e., don't allow those individuals that are already set up as participants)
	
	// Note: BR1.280  is technicall part of the functionality, implemented by the datawindow in 'Add Claim Participants' button that pops up a list of claim participants
		

	//1.290	Annuity benefits may only be written off for annuity participants who are entitled to annuity benefits.
	IF ls_annuity_payout_writeoff_flag = 'Y' AND ls_annuity_entitlement_flag = 'N' THEN
		MESSAGEBOX("BR1.290", "Annuity benefits may only be written off for annuity participants who are entitled to annuity benefits.", INFORMATION!)
		RETURN -1
	END IF
	
	// BR1.300	Annuity benefits may only be garnished for annuity payout participants who are entitled to annuity benefits.
	IF  ls_garnish_flag = 'Y' AND ls_annuity_entitlement_flag = 'N' THEN
		MESSAGEBOX("BR1.300", "Annuity benefits may only be garnished for annuity participants who are entitled to annuity benefits.", INFORMATION!)
		RETURN -1
	END IF
	
	//BR1.305	Annuity benefits must not be garnished for the individual who is eligible for annuity benefits, if the individual died prior to the age of 65. 
	IF NOT isnull(ldt_death_date) and li_age < 65 AND ls_garnish_flag_for_bh = 'Y' THEN
		MESSAGEBOX("BR1.305", "Annuity benefits must not be garnished for the individual who is eligible for annuity benefits, if the individual died prior to the age of 65.", INFORMATION!)
		RETURN -1
	END IF
	
	IF ls_recipient_type_code = 'I' THEN
		
		// used for BRs 1.310, 1.320
		SELECT court_order_flag
		INTO   :ls_court_order_flag
		FROM   INDIVIDUAL
		WHERE individual_no = :ll_recipient_no
		USING SQLCA;
		SQLCA.nf_handle_error("n_annuity_participant", "nf_check_bus_rule", "Select court_order_flag from INDIVIDUAL")
		
		//BR1.310	The annuity payout participant must have a court order, if her/his annuity benefits require garnishment.
		li_rtn = nf_garnishment_BR_1_310(ls_court_order_flag,ls_garnish_flag,ll_recipient_no)
		IF li_rtn = -1 THEN
			RETURN -1
		END IF
	
		//BR1.320	The annuity payout participant should not have a court order, if her/his annuity benefits do not require garnishment.
		li_rtn = nf_garnishment_BR_1_320(ls_court_order_flag,ls_garnish_flag,ll_recipient_no)
		IF li_rtn = -1 THEN
			RETURN -1
		END IF
	END IF
	
END IF	
Return 0
end function

public function integer nf_can_delete ();
STRING ls_find_exp
LONG ll_found, ll_rowCount

ll_rowCount = idw_datawindow[1].rowCount()
//1.10		The individual, either injured worker or surviving spouse, who is eligible for the annuity benefits must be an annuity participant for the annuity payout. (Refer to Rationale)
ls_find_exp = "recipient_no = " + STRING(il_benefit_holder_no)
ll_found = idw_datawindow[1].find(ls_find_exp, 1,ll_rowCount)
IF ll_found <= 0 THEN
	MESSAGEBOX("DELETE DENIED - BR1.10","The individual, either injured worker or surviving spouse, who is eligible for the annuity benefits " &
											+ "~rmust be an annuity participant for the annuity payout.", INFORMATION!)
	RETURN -1
END IF


// BR1.170 A representative must be an annuity participant of the annuity payout. (Cannot delete a participant, if this participant is representing another participant 
ls_find_exp = "represented_by_recipient_no = " + STRING(il_deleted_participant_no) 
ll_found = idw_datawindow[1].find( ls_find_exp, 1, ll_rowCount)
IF ll_found > 0 THEN
	MESSAGEBOX("DELETE DENIED - BR1.170","A representative must be an annuity participant of the annuity payout." &
	                                     		+ "~r(Cannot delete a participant, if this participant is representing another participant)." &
											+ "~rYou must first de-register this recipient as the 'represented by' recipient of any other participants")
	RETURN -1
END IF

RETURN 0
end function

public function integer nf_save ();INT li_rtn, li_cntr
LONG ll_individual_no, ll_row
S_WINDOW_MESSAGE lstr_message
U_DWA ldwa_dw
DWItemStatus ldw_status
w_add_annuity_participant       lw_add_annuity_participant

// the individual needs to be saved first, if the n_individual object exists and the itemstatus of any one of the 
// individual's DWs is new or has been modified ( dw_individual, dw_main_name, dw_name ) (set in nf_change_item)
IF isvalid(inv_individual) THEN
	IF is_individual_status = 'DATAMODIFIED' OR is_individual_status = 'NEW' THEN 
		IF is_individual_status = 'NEW' THEN
			// delay any commit untill the participant commits, if this is a new individual, 
			// (new individual it may fail to be added as an annuity participant, therfore we don't want an orphaned INDIVIDUAL record)
			inv_individual.nf_set_commit(false)
		ELSE
			inv_individual.nf_set_commit(true)
		END IF
		
		li_rtn = inv_individual.nf_save()
		
		IF li_rtn < 0   THEN 
			is_individual_status = 'NOTMODIFIED'
			RETURN li_rtn   
		ELSEIF is_individual_status = 'NEW' THEN
			// now get the individual no for the new individual just added to the INDIVIDUAL table
			ll_individual_no = inv_individual.nf_get_next_identifier()
			il_participant_no = ll_individual_no
		END IF
	END IF
END IF


// if the individual saved successfully or the individual did not need saving, now save the annuity participant
// first set the (remaining) values if this is a new participant being added
IF is_participant_status = 'NEW' THEN
	ll_row = idw_datawindow[2].getRow()
	idw_datawindow[2].setItem(ll_row,'recipient_no', il_participant_no)
	idw_datawindow[2].setItem(ll_row,'annuity_account_no', il_annuity_account_no)
	idw_datawindow[2].setItem(ll_row,'annuity_payout_no', il_annuity_payout_no)
	idw_datawindow[2].setItem(ll_row,'annuity_role_code', is_annuity_role_code)
	idw_datawindow[2].setItem(ll_row,'recipient_type_code', is_recipient_type_code)
	idw_datawindow[2].setItem(ll_row,'represented_by_recipient_no', il_participant_no)
	idw_datawindow[2].setItem(ll_row,'represented_by_recipient_type_code', is_recipient_type_code)
	idw_datawindow[2].setItem(ll_row,'annuity_payout_writeoff_flag', 'N')
	idw_datawindow[2].setItem(ll_row,'annuity_payout_garnish_flag', 'N')

ELSEIF is_participant_status = 'DATAMODIFIED' THEN
	// nothing to do, move on with the save
END IF

li_rtn = super:: FUNCTION nf_save()

this.nf_reset()

IF li_rtn < 0 THEN 
	RETURN -1
ELSE
	RETURN 0
END IF

end function

public function integer nf_change_item (long al_datawindow);CHOOSE CASE al_datawindow
   CASE 1

   CASE 2
/*	dw_individual
*/
		IF inv_individual.nf_change_item(1) < 0 THEN
			Return -1
		END IF
		is_individual_status = 'DATAMODIFIED'
	
	CASE 3
		// dw_main_name for individual
     	inv_individual.nf_change_item(2)
		is_individual_status = 'DATAMODIFIED'
	
	CASE ELSE 
   		Return -1
END CHOOSE

Return 0
end function

public subroutine nf_reset ();

is_individual_status = 'NOTMODIFIED'
is_participant_status = 'NOTMODIFIED'
return
end subroutine

public function integer nf_garnishment_br_1_320 (string as_court_order_flag, string as_garnish_flag, long al_individual_no);// enforces annuity payout participant BR 1.320
STRING ls_fullname

IF as_garnish_flag = 'N' AND as_court_order_flag = 'Y' THEN
	
	SELECT given_names + ' ' + last_name
	INTO   :ls_fullname
	FROM   INDIVIDUAL
	WHERE  individual_no = :al_individual_no
	USING SQLCA;
	SQLCA.nf_handle_error('n_annuity_participant','embedded SQL: SELECT given_names + last_name FROM INDIVIDUAL...','nf_garnishment_br_1_320')
	
	
	IF MESSAGEBOX("BR1.320", "The annuity payout participant ("+String(ls_fullname)+") should not have a court order, if her/his annuity benefits do not require garnishment. " &
		                      + "This participant has a court order. Do you want to continue?",  QUESTION!, YesNo!, 2) = 2 THEN
		RETURN -1
	END IF
END IF

RETURN 0
end function

public function integer nf_garnishment_br_1_310 (string as_court_order_flag, string as_garnish_flag, long al_individual_no);// enforces annuity payout participant BR 1.310
STRING ls_fullname


IF as_garnish_flag = 'Y' AND as_court_order_flag = 'N' THEN
		
	SELECT given_names + ' ' + last_name
	INTO   :ls_fullname
	FROM   INDIVIDUAL
	WHERE  individual_no = :al_individual_no
	USING SQLCA;
	SQLCA.nf_handle_error('n_annuity_participant','embedded SQL: SELECT given_names + last_name FROM INDIVIDUAL...','nf_garnishment_br_1_310')
	
	
	MESSAGEBOX("BR1.310", "The annuity payout participant ("+String(ls_fullname)+") must have a court order, if her/his annuity benefits require garnishment.", INFORMATION!)
	RETURN -1	
END IF

RETURN 0
end function

on n_annuity_participant.create
call super::create
end on

on n_annuity_participant.destroy
call super::destroy
end on

