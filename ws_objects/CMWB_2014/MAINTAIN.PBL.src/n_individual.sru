$PBExportHeader$n_individual.sru
$PBExportComments$user object contain all the business rules for creating and maintaining an individual
forward
global type n_individual from n_pdc
end type
end forward

global type n_individual from n_pdc
end type
global n_individual n_individual

type variables
N_VALIDATE_ADDRESS 	inv_address
LONG        il_claim_no, il_new_claim_no
BOOLEAN     ib_new_claim 
BOOLEAN		ib_validate_cost_allocation = False
BOOLEAN		ib_validate_sin_uniqueness = True
BOOLEAN		ib_validate_medicare_uniqueness = True
u_ds        ids_annuity_eligibility
STRING      is_x001_override_elig_delete
DATETIME    idtm_death_date
LONG        il_individual_no			
DATETIME    idtm_old_annuity_start_date
String 		is_claim_role_code

end variables

forward prototypes
public function integer nf_retrieve (long al_individual_no)
public function integer nf_set_defaults ()
public function integer nf_check_digit (long al_number, string as_type)
public function integer nf_insert_individual_name ()
public function integer nf_set_unused_fields ()
public function long nf_set_identifiers ()
public function string nf_reduce_blanks (string as_string)
public function integer nf_check_for_numeric (string as_string)
public function integer nf_calculate_age (date adt_birth_date, date adt_death_date)
public function long nf_get_next_individual ()
public function integer nf_check_payments (long al_individual_no, boolean ab_direct_deposit)
public function integer nf_check_payments_and_awards (long al_individual_no, ref string as_award_type, ref string as_message)
public function long nf_get_next_identifier ()
public subroutine nf_init (u_dwa adw_dw[], n_transaction anv_transobj, w_ancestor awi_window_parent)
public subroutine nf_set_current_claim (long al_claim_no)
public subroutine nf_set_new_claim_no (long al_claim_no)
public function integer nf_change_item (long al_datawindow)
public function integer nf_validate_claim_cost_alloc ()
public function integer nf_log_events ()
public function integer nf_validate_birth_death_date (string as_action)
public function integer nf_check_bus_rule ()
public function integer nf_update_eligibility_coverage (long al_individual_no)
public function integer nf_check_mandatory ()
public function long nf_get_annuity_account_no (long al_individual_no, long al_claim_no, string as_claim_role_code)
public subroutine nf_set_claim_role_code (string as_claim_role_code)
public function integer nf_modify_create_annuity_eligibility (integer ai_case, date adt_birth, date adt_death, long al_individual_no)
public function integer nf_check_benefit_payments (datetime adtm_annuity_end_date, ref boolean ab_past_pmts, long al_claim_no, long al_individual_no, string as_claim_role_code)
public function integer nf_validate_annuity_dates (datetime adtm_annuity_start_date, datetime adtm_annuity_end_date, long al_claim_no, long al_individual_no, decimal adec_pct, string as_claim_role)
public function integer nf_check_annuity_end_date_brs (integer ai_check_rule, long al_individual_no, date adt_birth_date, date adt_death_date, datetime adtm_ann_start_date, string as_claim_role)
public function integer nf_check_iw_death_date (long al_individual_no, date adt_new_death_date)
public function integer nf_check_ss_death_date (long al_individual_no, date adt_new_death_date)
public function integer nf_check_annuity_death_date (long al_individual_no, datetime adt_original_death_date, datetime adt_new_death_date)
public function integer nf_check_annuity_birth_date (long al_individual_no, datetime adt_original_birth_date, datetime adt_new_birth_date)
public function integer nf_check_annuity_dependant_death_date (long al_individual_no, datetime adt_original_death_date, datetime adt_new_death_date)
end prototypes

public function integer nf_retrieve (long al_individual_no);LONG  				ll_rows,					ll_child_row
STRING				ls_location_code,		ls_location_type_code,	ls_find_expression
DATAWINDOWCHILD	ldwc_child

/* dw 1 = main individual info including alias name
 	dw 2 = main name of individual
	dw 3 = alias name
*/
	ll_rows = idw_dw[1].Retrieve(al_individual_no)
	idw_dw[2].Retrieve(al_individual_no)
	idw_dw[3].Retrieve(al_individual_no)

/* determine if the individual is historic - display error message and set flag
*/
	IF ll_rows = 1 THEN
	   IF idw_dw[1].GetItemString(ll_rows,'history_flag') = 'Y' THEN
	      MessageBox("Warning", "This individual is being reactivated.  All data will be verified on the save.")
	   END IF
		IF NOT IsNull(idw_dw[1].GetItemDateTime(ll_rows,'birth_date')) THEN
			idw_dw[1].SetItem(ll_rows,'age',nf_calculate_age(Date(idw_dw[1].GetItemDateTime(ll_rows,'birth_date')),Date(idw_dw[1].GetItemDateTime(ll_rows,'death_date'))))
		END IF

	/* Get the location type code to set the location description field so that the appropriate municipality code
		is displayed.
	*/
		ls_location_code = idw_dw[1].GetItemString(ll_rows,'location_code')
		ls_location_type_code = idw_dw[1].GetItemString(ll_rows,'location_type_code')
		IF ls_location_type_code = 'M' THEN
		/*	Get a reference to the city drop down list
		*/
			IF idw_dw[1].GetChild("location_desc2",ldwc_child) < 0 THEN
				MessageBox("Error","Could not reference list of city codes.  Please call the Help Desk.")
				Return -1
			END IF
			ls_find_expression = 'location_code= "' + ls_location_code + '"'
			ll_child_row = ldwc_child.Find(ls_find_expression,1,ldwc_child.RowCount())
			IF ll_child_row > 0 THEN
				idw_dw[1].SetItem(ll_rows,'location_desc2',ldwc_child.GetItemString(ll_child_row,"location_desc2"))
			ELSE
				idw_dw[1].SetItem(ll_rows,'location_desc2',idw_dw[1].GetItemString(ll_rows,'city'))
			END IF
		ELSE
			idw_dw[1].SetItem(ll_rows,'location_desc2',idw_dw[1].GetItemString(ll_rows,'city'))
		END IF
	END IF
   
	Return ll_rows

end function

public function integer nf_set_defaults ();LONG  ll_row

ll_row = idw_dw[1].GetRow()
IF ll_row > 0 THEN
   //set the default values for the individual insert
   idw_dw[1].SetItem(ll_row,'sex', 'U')
   idw_dw[1].SetItem(ll_row,'language_code', 'U')
   idw_dw[1].SetItem(ll_row,'address_line2', ' ')
   idw_dw[1].SetItem(ll_row,'prov_state_code', 'NB')
   idw_dw[1].SetItem(ll_row,'country_code', 'CAN')
   idw_dw[1].SetItem(ll_row,'history_flag', 'N')
	idw_dw[1].SetItem(ll_row,'individual_caution_flag','N')
   idw_dw[2].SetItem(ll_row,'name_type_code', 'M')
END IF

Return 0
end function

public function integer nf_check_digit (long al_number, string as_type);// this routine can be used to validate the SIN or the Medicare Number
// the same logic applies
STRING   ls_sin, ls_temp
LONG     ll_digits
INTEGER  li_check_digit, li_loop, li_sum

ls_sin = String(al_number,'000000000')
li_check_digit = Integer(Right(ls_sin,1))
ls_sin = Left(ls_sin,8)

// make a number from the units position and every other position to the left
ls_temp = Right(ls_sin,1)
ls_temp = Mid(ls_sin,6,1) + ls_temp
ls_temp = Mid(ls_sin,4,1) + ls_temp
ls_temp = Mid(ls_sin,2,1) + ls_temp
// add the number to itself
ll_digits = Long(ls_temp) * 2
ls_temp = String(ll_digits)
// cross add the digits in the sum
li_loop = 1
ll_digits = 0
DO WHILE li_loop <= Len(ls_temp)
   ll_digits = ll_digits + Integer(Mid(ls_temp,li_loop,1))
   li_loop = li_loop + 1
LOOP
li_sum = ll_digits
// cross add the other digits in the sin
ll_digits = Integer(Mid(ls_sin,7,1)) + Integer(Mid(ls_sin,5,1)) + Integer(Mid(ls_sin,3,1)) + Integer(Mid(ls_sin,1,1))
// sum both sums together
li_sum = li_sum + ll_digits
// check digit is 10 - last digit of sum if last digit not zero else check digit = 0
ls_temp = String(li_sum)
IF Right(Trim(ls_temp),1) = '0' THEN
	ll_digits = 0
ELSE
	ll_digits = 10 - Integer(Right(Trim(ls_temp),1))
END IF
IF li_check_digit <> ll_digits THEN
// error - invalid number
   IF as_type = 'S' THEN
      MessageBox('Invalid SIN','The SIN entered is invalid. Please correct.')
   ELSE
      MessageBox('Invalid Medicare Number','The Medicare number entered is invalid. Please correct.')
   END IF
   RETURN -1
END IF

RETURN 0
end function

public function integer nf_insert_individual_name ();LONG  ll_row

IF NOT IsNull(idw_dw[1].GetItemString(1,'individual_name_last_name')) AND Trim(idw_dw[1].GetItemString(1,'individual_name_last_name')) <> '' THEN
   ll_row = idw_dw[3].RowCount()
   IF ll_row = 0 THEN
   // insert a row if one was not retrieved
      ll_row = idw_dw[3].InsertRow(0)
   END IF
   idw_dw[3].SetItem(ll_row,'last_name', idw_dw[1].GetItemString(1,'individual_name_last_name'))
   idw_dw[3].SetItem(ll_row,'given_names', idw_dw[1].GetItemString(1,'individual_name_given_names'))
   idw_dw[3].SetItem(ll_row,'name_type_code', 'A')
   idw_dw[3].AcceptText()
ELSE
// if row delete it
   IF idw_dw[3].RowCount() > 0 THEN
      idw_dw[3].DeleteRow(1)
   END IF
END IF

Return 0
end function

public function integer nf_set_unused_fields ();LONG  ll_row

// determine which fields are null and fill them in with a default value
// at some point in time any of the following fields may be unused

ll_row = idw_dw[1].GetRow()
IF ll_row > 0 THEN
   IF IsNull(idw_dw[1].GetItemString(ll_row, 'address_line1')) OR idw_dw[1].GetItemString(ll_row, 'address_line1') = '' THEN
      idw_dw[1].SetItem(ll_row, 'address_line1', ' ')
   END IF
   IF IsNull(idw_dw[1].GetItemString(ll_row, 'address_line2')) THEN
      idw_dw[1].SetItem(ll_row, 'address_line2', ' ')
   END IF
   IF IsNull(idw_dw[1].GetItemString(ll_row, 'sex')) OR idw_dw[1].GetItemString(ll_row, 'sex') = '' THEN
      idw_dw[1].SetItem(ll_row, 'sex', 'U')
   END IF
   IF IsNull(idw_dw[1].GetItemString(ll_row, 'language_code')) OR idw_dw[1].GetItemString(ll_row, 'language_code') = '' THEN
      idw_dw[1].SetItem(ll_row, 'language_code', 'U')
   END IF
   IF IsNull(idw_dw[1].GetItemString(ll_row, 'city'))  THEN
      idw_dw[1].SetItem(ll_row, 'city', ' ')
    END IF
   IF IsNull(idw_dw[1].GetItemString(ll_row, 'prov_state_code'))  OR idw_dw[1].GetItemString(ll_row, 'prov_state_code') = '' THEN
      idw_dw[1].SetItem(ll_row, 'prov_state_code', ' ')
    END IF
   IF IsNull(idw_dw[1].GetItemString(ll_row, 'country_code')) OR idw_dw[1].GetItemString(ll_row, 'country_code') = '' THEN
      idw_dw[1].SetItem(ll_row, 'country_code', ' ')
   END IF
   IF IsNull(idw_dw[1].GetItemString(ll_row, 'postal_code'))  THEN
      idw_dw[1].SetItem(ll_row, 'postal_code', ' ')
   END IF
   IF IsNull(idw_dw[1].GetItemString(ll_row, 'telephone_no')) THEN
      idw_dw[1].SetItem(ll_row, 'telephone_no', ' ')
   END IF

   IF IsNull(idw_dw[1].GetItemString(ll_row, 'bank_no'))  THEN
      idw_dw[1].SetItem(ll_row, 'bank_no', ' ')
   END IF
   IF IsNull(idw_dw[1].GetItemString(ll_row, 'bank_transit_no')) THEN
      idw_dw[1].SetItem(ll_row, 'bank_transit_no', ' ')
   END IF
   IF IsNull(idw_dw[1].GetItemString(ll_row, 'bank_account_no')) THEN
      idw_dw[1].SetItem(ll_row, 'bank_account_no', ' ')
   END IF
   IF IsNull(idw_dw[1].GetItemString(ll_row, 'history_flag')) OR idw_dw[1].GetItemString(ll_row, 'history_flag') = '' THEN
      idw_dw[1].SetItem(ll_row, 'history_flag', 'N')
   END IF
END IF

Return 0
end function

public function long nf_set_identifiers ();LONG  ll_individual_no, ll_row

ll_individual_no = nf_get_next_identifier()

IF ll_individual_no > 0 THEN
   // need to check if new row
   
   ll_row = idw_dw[1].GetRow()
   IF ll_row > 0 THEN
      idw_dw[1].SetItem(ll_row, 'individual_no', ll_individual_no)
   ELSE
      MessageBox("Error", "Error setting individual number.")
      Return -1
   END IF
   ll_row = idw_dw[2].GetRow()
   IF ll_row > 0 THEN
      idw_dw[2].SetItem(ll_row, 'individual_no', ll_individual_no)
   ELSE
      MessageBox("Error", "Error setting individual number.")
      Return -1
   END IF
   ll_row = idw_dw[3].GetRow()   // alias name  - may  not be one
   IF ll_row > 0 THEN
      idw_dw[3].SetItem(ll_row, 'individual_no', ll_individual_no)
   END IF
ELSE
   MessageBox("Error","Unable to generate an new individual number.~r~nSee system administrator.")
END IF


Return ll_individual_no
end function

public function string nf_reduce_blanks (string as_string);LONG	ll_pos

/*	get rid of double blanks in the string passed
*/
	ll_pos = Pos(as_string,'  ')
	DO WHILE  ll_pos > 0 
		as_string = Replace(as_string,ll_pos,2,' ')
		ll_pos = Pos(as_string,'  ',ll_pos )
	LOOP
	
	//remove blanks around the dash
	ll_pos = Pos(as_string,'- ')
	DO WHILE  ll_pos > 0 
		as_string = Replace(as_string,ll_pos,2,'-')
		ll_pos = Pos(as_string,'- ',ll_pos )
	LOOP
	
	ll_pos = Pos(as_string,' -')
	DO WHILE  ll_pos > 0 
		as_string = Replace(as_string,ll_pos,2,'-')
		ll_pos = Pos(as_string,' -',ll_pos )
	LOOP


Return as_string
end function

public function integer nf_check_for_numeric (string as_string);	IF Pos(as_string,'0') > 0 THEN
		Return -1
	ELSEIF Pos(as_string,'1') > 0 THEN
		Return -1
	ELSEIF Pos(as_string,'2') > 0 THEN
		Return -1
	ELSEIF Pos(as_string,'3') > 0 THEN
		Return -1
	ELSEIF Pos(as_string,'4') > 0 THEN
		Return -1
	ELSEIF Pos(as_string,'5') > 0 THEN
		Return -1
	ELSEIF Pos(as_string,'6') > 0 THEN
		Return -1
	ELSEIF Pos(as_string,'7') > 0 THEN
		Return -1
	ELSEIF Pos(as_string,'8') > 0 THEN
		Return -1
	ELSEIF Pos(as_string,'9') > 0 THEN
		Return -1
	END IF
Return 0
end function

public function integer nf_calculate_age (date adt_birth_date, date adt_death_date);LONG		ll_Today_Date, ll_birth_date
STRING	ls_Age

/*	Difference in years between the Birth Date and the Other Date (i.e. Today)
	Makes sure the string is 8 characters long so that the first 4 characters reflects the age
*/

	ll_Birth_Date = Long(String(adt_birth_date,"YYYYMMDD"))

	IF IsNull(adt_death_date) OR String(adt_death_date,'yyyy/mm/dd') = '1900/01/01' THEN
		ll_Today_Date = Long(String(Date(f_server_datetime()),"YYYYMMDD"))
	ELSE
		ll_Today_Date = Long(String(adt_death_date,'YYYYMMDD'))
	END IF

	ls_Age = String(ll_Today_Date - ll_Birth_Date)
	ls_Age = Space(8 - Len(ls_Age)) + ls_Age
	ls_Age = Left(ls_Age, 4)

Return Integer(ls_Age)
end function

public function long nf_get_next_individual ();LONG ll_individual_no, ll_error


SQLCA.nf_begin_transaction()

UPDATE Last_Individual_No
SET individual_no = individual_no +1
USING SQLCA;

ll_error = inv_transobj.nf_handle_error("Embedded SQL","n_individual","nf_get_next_individual")

SELECT individual_no
INTO :ll_individual_no
FROM Last_Individual_No
USING SQLCA;

ll_error = inv_transobj.nf_handle_error("Embedded SQL","n_individual","nf_get_next_individual")

IF ll_error < 0 THEN
	SQLCA.nf_rollback_transaction()
	RETURN - 1
ELSE
	SQLCA.nf_commit_transaction()
END IF

RETURN ll_individual_no
end function

public function integer nf_check_payments (long al_individual_no, boolean ab_direct_deposit);LONG		ll_count, ll_return, ll_cntr, ll_claim, ll_claim_temp
DATETIME	ldtm_today
STRING	ls_award_type, ls_temp, ls_awards
DATASTORE lds_awards

ldtm_today = f_server_datetime()

IF IsNull(al_individual_no) THEN
	Return 0
END IF
ll_return = 0

lds_awards = CREATE DATASTORE
lds_awards.DataObject = 'd_award_types'
lds_awards.SetTransObject(SQLCA)

IF ab_direct_deposit THEN
	SELECT Count(*)
	  INTO :ll_count
	  FROM UNAPPLIED_CLAIM_TXN
	 WHERE recipient_no = :al_individual_no
		AND payment_method_code = 'D'
	 USING SQLCA;

	IF SQLCA.nf_handle_error('Embedded SQL: select from UNAPPLIED_CLAIM_TXN','n_payments','nf_check_payments') < 0 THEN
		Return -1
	END IF

	IF ll_count > 0 THEN
		MessageBox('Scheduled Payments Exist','Scheduled direct deposit PAYMENTS exist for this individual.  The banking information~r~n cannot be removed until the payment method has been changed to Automated Cheque.')
		ll_return = -1
	END IF

	ll_count = lds_awards.Retrieve(al_individual_no)

	IF ll_count > 0 THEN
		FOR ll_cntr = 1 to ll_count
			ls_temp = lds_awards.GetItemString(ll_cntr, 'award_type_code')
			ll_claim = lds_awards.GetItemNumber(ll_cntr,'claim_no')
			IF ll_claim = ll_claim_temp THEN
				IF ls_temp = ls_award_type THEN
					CONTINUE
				ELSE
					IF ls_award_type = '' THEN
						ls_award_type = ls_temp
					ELSE
						ls_award_type = ls_award_type + ', ' + ls_temp
					END IF
				END IF
				ls_awards = 'Individual #: ' + string(al_individual_no) + " has Award Types: " + ls_award_type + ' for Claim #: ' + string(ll_claim) + '~r~n'
			ELSE
				ls_award_type = ls_temp
				ls_awards = ls_awards + 'Individual #: ' + string(al_individual_no) + " has Award Types: "  + ls_award_type + ' for Claim #: ' + string(ll_claim) + '~r~n'
			END IF
			ll_claim_temp = ll_claim
		NEXT
		MessageBox('Scheduled Awards Exist','Scheduled direct deposit AWARDS exist for this individual.  The banking information ~r~ncannot be removed until the payment method has been changed.~r~n~r~n' &
		+ 'To replace the payment method for Awards, terminate~r~nthe award and enter a new Award record for the remaining period with a payment~r~nmethod of Automated Cheque.' &
		+ '~r~n~r~n' + ls_awards)
		ll_return = -1
	END IF

	Return ll_return
ELSE
	SELECT Count(*)
	  INTO :ll_count
	  FROM UNAPPLIED_CLAIM_TXN
	 WHERE recipient_no = :al_individual_no
		AND payment_method_code <> 'D'
	 USING SQLCA;

	IF SQLCA.nf_handle_error('Embedded SQL: select from UNAPPLIED_CLAIM_TXN','n_payments','nf_check_payments') < 0 THEN
		Return -1
	END IF

	IF ll_count > 0 THEN
		MessageBox('Warning - Scheduled Payments Exist','Scheduled payments exist for this individual that are not direct deposit.  You may want to change the payment method to direct deposit.')
	END IF

	SELECT Count(*)
	  INTO :ll_count
	  FROM PERIODIC_AWARD,   
			 PERIODIC_RECIPIENT,
			 PERIODIC_AWARD_CONTROL 
	 WHERE PERIODIC_AWARD.claim_no = PERIODIC_RECIPIENT.claim_no
		AND PERIODIC_AWARD.award_no = PERIODIC_RECIPIENT.award_no
		AND PERIODIC_AWARD_CONTROL.award_type_code = PERIODIC_AWARD.award_type_code
		AND PERIODIC_RECIPIENT.payment_method_code <> 'D'
		AND PERIODIC_RECIPIENT.recipient_no = :al_individual_no
		AND PERIODIC_RECIPIENT.recipient_type_code = 'I' 
		AND PERIODIC_AWARD_CONTROL.processed_date is NULL
		AND PERIODIC_AWARD.award_end_date > PERIODIC_AWARD_CONTROL.period_from_date  
	USING SQLCA;

	IF SQLCA.nf_handle_error('Embedded SQL: select from PERIODIC AWARD','n_payments','nf_check_payments') < 0 THEN
		Return -1
	END IF

	IF ll_count > 0 THEN
		MessageBox('Warning - Scheduled Awards Exist','Scheduled payments exist for this individual that are not direct deposit.  You may want to change the payment method to direct deposit.')
	END IF

END IF

Return 0
end function

public function integer nf_check_payments_and_awards (long al_individual_no, ref string as_award_type, ref string as_message);/*Check to see if there are any unprocessed txns or awards, if there are, do not allow
  users to remove bank information from the Individual.*/

LONG		ll_count, ll_return, ll_cntr, ll_claim, ll_claim_temp
STRING	ls_temp, ls_claim,ls_award_type,ls_awards
DATETIME	ldtm_today
DATASTORE lds_awards

ldtm_today = f_server_datetime()

lds_awards = create DATASTORE

lds_awards.dataobject = 'd_award_types'
lds_awards.SetTransObject(SQLCA)

IF IsNull(al_individual_no) THEN
	Return 0
END IF

ll_return = 0

SELECT Count(*)
  INTO :ll_count
  FROM UNAPPLIED_CLAIM_TXN
 WHERE recipient_no = :al_individual_no
	AND payment_method_code = 'D'
 USING SQLCA;

IF SQLCA.nf_handle_error('Embedded SQL: select from UNAPPLIED_CLAIM_TXN','n_payments','nf_check_payments') < 0 THEN
	Return -1
END IF

IF ll_count > 0 THEN
	ll_return = -1
END IF

ll_count = lds_awards.Retrieve(al_individual_no)

IF ll_count > 0 THEN
	FOR ll_cntr = 1 to ll_count
		ls_temp = lds_awards.GetItemString(ll_cntr, 'award_type_code')
		ll_claim = lds_awards.GetItemNumber(ll_cntr, 'claim_no')
		IF ll_claim = ll_claim_temp THEN
			IF ls_temp = ls_award_type THEN
				CONTINUE
			ELSE
				IF ls_award_type = '' THEN
					ls_award_type = ls_temp
				ELSE
					ls_award_type = ls_award_type + ', ' + ls_temp
				END IF
			END IF
			ls_awards = 'Individual #: ' + string(al_individual_no) + " has Award Types: " + ls_award_type + ' for Claim #: ' + string(ll_claim) + '~r~n'
		ELSE
			ls_award_type = ls_temp
			ls_awards = ls_awards + 'Individual #: ' + string(al_individual_no) + " has Award Types: "  + ls_award_type + ' for Claim #: ' + string(ll_claim) + '~r~n'
		END IF
		ll_claim_temp = ll_claim
	NEXT
	as_award_type = 'AWARD'
	
	as_message = 'Scheduled direct deposit AWARDS exist for this individual.  The banking information ~r~ncannot be removed until the payment method has been changed.~r~n~r~n' &
	+ 'To replace the payment method for Awards, terminate~r~nthe award and enter a new Award record for the remaining period with a payment~r~nmethod of Automated Cheque.' &
	+ '~r~n~r~n' + ls_awards
	
	ll_return = -1
END IF

Return ll_return

		

















end function

public function long nf_get_next_identifier ();LONG  ll_no, ll_row

// get the next individual number
ll_row = idw_dw[1].GetRow()
IF ll_row > 0 THEN
   IF idw_dw[1].GetItemStatus(ll_row,0,Primary!) = NewModified! THEN 
		ll_no = nf_get_next_individual()
	ELSE
      ll_no = idw_dw[1].GetItemNumber(ll_row,'individual_no')
   END IF
ELSE
   Return 0
END IF
RETURN ll_no

end function

public subroutine nf_init (u_dwa adw_dw[], n_transaction anv_transobj, w_ancestor awi_window_parent);/*	The following datawindows are passed:
	adw_dw[1] = dw_individual
	adw_dw[2] = dw_main_name
	adw_dw[3] = dw_individual_name
	adw_dw[4] = dw_next_individual_no
	adw_dw[5] = dw_claim_list
	adw_dw[6] = dw_basic_claim
*/

	nf_set_datawindow(adw_dw[],anv_transobj)
	nf_set_window_parent(awi_window_parent)

/* Create and initialize all the objects that are used by this object
*/
	inv_address = Create n_validate_address
	inv_address.nf_set_datawindow(adw_dw[1])

	ids_annuity_eligibility = CREATE u_ds
	ids_annuity_eligibility.dataobject = 'd_annuity_eligibility'
	ids_annuity_eligibility.SetTransObject(SQLCA)
	

end subroutine

public subroutine nf_set_current_claim (long al_claim_no);//Set the current claim number

il_claim_no = al_claim_no
end subroutine

public subroutine nf_set_new_claim_no (long al_claim_no);//Set the newly created claim_no

il_new_claim_no = al_claim_no
end subroutine

public function integer nf_change_item (long al_datawindow);LONG            	ll_row, ll_no,  ll_individual_no,ll_claim_no, ll_ann_rows
STRING         	ls_claim_role_code, ls_claim_role_desc, ls_message
DATE            	ld_date,ld_date2
DATETIME		ldtm_annuity_end_date, ldtm_old_bday, ldtm_annuity_start_date, ldtm_annuity_end_date_calculated
DATETIME		ldtm_annuity_eligibility_end_date_used
INT			   	li_counter
BOOLEAN		lb_past_pmts

CHOOSE CASE al_datawindow
   CASE 1	// dw_individual 
		
      	ll_row 				= idw_dw[1].GetRow()
	 	ll_individual_no 	= idw_dw[1].GetItemNumber(1,'individual_no')
     	ll_ann_rows 		= ids_annuity_eligibility.Retrieve(ll_individual_no)	
		
      CHOOSE CASE idw_dw[1].GetColumnName()
         CASE 'individual_name_given_names'
	         	idw_dw[1].SetItem(ll_row,'individual_name_given_names',Trim(idw_dw[1].GetText()))
         CASE 'individual_name_last_name'
	            	idw_dw[1].SetItem(ll_row,'individual_name_last_name',Trim(idw_dw[1].GetText()))
         CASE 'sin_no'
         // do the check digit check
            ll_no = Long(idw_dw[1].GetText())
            IF nf_check_digit(ll_no, 'S') < 0 THEN
					idw_dw[1].SetFocus()
					idw_dw[1].SetColumn('sin_no')
					Return -1
            END IF
         CASE 'medicare_no'
         // do the check digit check
            ll_no = Long(idw_dw[1].GetText())
            IF nf_check_digit(ll_no, 'M') < 0 THEN
					idw_dw[1].SetFocus()
					idw_dw[1].SetColumn('medicare_no')
					Return -1
            END IF
         CASE 'birth_date'
         	// must be less than today
		   ldtm_old_bday = idw_dw[1].GetItemDateTime(idw_dw[1].GetRow(), 'birth_date',Primary!,TRUE)
			
            IF idw_dw[1].GetText() <> "" THEN
               	ld_date 	= Date(Left(idw_dw[1].GetText(),10))
			  	ld_date2 	= Date(idw_dw[1].GetItemDateTime(1,'death_date'))
			 
				IF ld_date > Date(f_server_datetime()) THEN
					 MessageBox('Warning', 'Birth date cannot be in the future!', Exclamation!)
					 idw_dw[1].SetFocus()
					 idw_dw[1].SetColumn('birth_date')
					 Return -1
				END IF
					
				// give warning if under 16 or over 65
				ll_no = nf_calculate_age(ld_date, ld_date2)				
										
				idw_dw[1].SetItem(ll_row,'age',ll_no)
									
				/* PR3899 - J.M. 2004/06/25 - If the birthdate has changed and the annuity end date
				   is entered, the annuity end date needs to be updated to be 65 years past the birthdate.
				*/		
				IF ll_ann_rows > 0 THEN
					
					n_common_annuity lnv_common_annuity
					lnv_common_annuity = create n_common_annuity
					
					
					FOR li_counter = 1 TO ll_ann_rows	
						
						//grab the information we need for the annuity datawindow
						ll_claim_no 							= ids_annuity_eligibility.GetItemnumber(li_counter,'claim_no')
						ls_claim_role_code 				= ids_annuity_eligibility.GetItemstring(li_counter,'claim_role_code')
						ldtm_annuity_end_date 			= ids_annuity_eligibility.GetItemDateTime(li_counter,'annuity_end_date')
						ldtm_annuity_start_date 			= ids_annuity_eligibility.GetItemDateTime(li_counter,'annuity_start_date')						
							
						/*
						2.250 	If an active annuity eligibility exists for the annuity account, and the birthdate is changed causing the annuity end date to be set in the future, 
									and scheduled payments/awards exists after the new annuity end date, a message is displayed indicating the payments should be reviewed.
	
						2.260 	If an active annuity eligibility exists for the annuity account, and the birthdate is changed causing the annuity end date to be set in the past, 
									and payments/awards exists after the new annuity end date, a message is displayed indicating the claims/payments should be reviewed.
	
						2.270		If an active annuity eligibility exists for the annuity account, and the birth date is changed, the annuity eligibility end date must not be prior to the 
									annuity eligibility start date.	
						*/
							
						IF nf_check_annuity_end_date_brs(3, ll_individual_no, ld_date, ld_date2, ldtm_annuity_start_date, ls_claim_role_code) = -1 THEN
							MessageBox('Validation Error', 'The birth date has changed, the annuity eligibility end date must not be prior to the annuity eligibility start date!', Exclamation!)
							idw_dw[1].SetFocus()
							idw_dw[1].SetColumn('birth_date')
							RETURN -1
						END IF 
												
						// ldtm_annuity_end_date_calculated returned by reference
						lnv_common_annuity.nf_get_annuity_end_date(ll_individual_no,ls_claim_role_desc , ls_message, ldtm_annuity_end_date_calculated, ldtm_annuity_eligibility_end_date_used, DATETIME(ld_date), DATETIME(ld_date2))
								
						IF DATE(ldtm_annuity_end_date_calculated) <> DATE(ldtm_annuity_end_date) THEN
														
							IF DATE(ldtm_annuity_end_date_calculated) < DATE(f_server_datetime()) THEN
								lb_past_pmts = TRUE
							END IF
								
							IF li_counter = 1 THEN //ONLY DO IT THE FIRST TIME								
								MessageBox('Annuity End Date','The Annuity End Date has been re-calculated. The new date will be ' + string(date(ldtm_annuity_end_date_calculated)),information!)							
							END IF 
								
							//BR 2.250 & BR 2.260	
							nf_check_benefit_payments(ldtm_annuity_end_date_calculated, lb_past_pmts,ll_claim_no,ll_individual_no, ls_claim_role_code)

						END IF
					NEXT
				END IF
			END IF
						
         CASE 'death_date'
           ld_date = Date(Left(idw_dw[1].GetText(),10))
           IF idw_dw[1].GetText() <> "" THEN
				
				/*
				2.280	If an active annuity eligibility exists for the annuity account, and the death date is changed, the annuity eligibility end date must not be prior to the 
						annuity eligibility start date.	
						** renumbered to ** 
				2.275	The death date of the individual must not be changed if active annuity eligibility exists for the annuity account of the individual 
						and the death date will cause the annuity eligibility end date to be on or prior to the annuity eligibility start date. 
						(Note: There may be more than one annuity account for the individual)
				*/
				IF ll_ann_rows > 0 THEN		
					
					//grab the death date
					ld_date2 	= Date(idw_dw[1].GetItemDateTime(1,'birth_date'))
					
					FOR li_counter = 1 TO ll_ann_rows
						
						//GRAB THE VALUES REQUIRED FROM THE DATASTORE
 						ll_claim_no 					= ids_annuity_eligibility.GetItemnumber(li_counter,'claim_no')
						ls_claim_role_code 		= ids_annuity_eligibility.GetItemstring(li_counter,'claim_role_code')
						ldtm_annuity_start_date 	= ids_annuity_eligibility.GetItemDateTime(li_counter,'annuity_start_date')	
						
						IF nf_check_annuity_end_date_brs(4,ll_individual_no,ld_date2, ld_date, ldtm_annuity_start_date, ls_claim_role_code) = -1 THEN
							MessageBox('Validation Error', 'The Death Date has changed, the annuity eligibility end date must not be prior to the annuity eligibility start date!', Exclamation!)
							idw_dw[1].SetFocus()
							idw_dw[1].SetColumn('death_date')
							RETURN -1
						END IF 
					NEXT
				END IF 
				
               IF ld_date > Date(f_server_datetime()) THEN
                  MessageBox('Validation Error', 'Death date cannot be in the future.', Exclamation!)
				idw_dw[1].SetFocus()
				idw_dw[1].SetColumn('death_date')
                  Return -1
               END IF
               IF Not IsNull(idw_dw[1].GetItemDateTime(ll_row,'birth_date')) THEN
                  ld_date2 = Date(idw_dw[1].GetItemDateTime(ll_row,'birth_date'))
                  IF ld_date2 > ld_date THEN
                     MessageBox("Error", "Death date cannot be before birth date.")
							idw_dw[1].SetFocus()
							idw_dw[1].SetColumn('death_date')
							RETURN -1
						END IF
               END IF

            END IF
				ld_date2 = Date(idw_dw[1].GetItemDateTime(1,'birth_date'))
				ll_no = nf_calculate_age(ld_date2, ld_date)
				idw_dw[1].SetItem(ll_row,'age',ll_no)
    
         CASE 'bank_no'
            IF NOT IsNumber(idw_dw[1].GetText()) THEN
               MessageBox('Error', 'The bank number must be numeric.')
					idw_dw[1].SetFocus()
					idw_dw[1].SetColumn('bank_no')
					RETURN -1
            END IF
				IF (Trim(idw_dw[1].GetItemString(ll_row,'bank_no')) = '' OR idw_dw[1].GetItemString(ll_row,'bank_no') = '000' ) AND &
					idw_dw[1].GetText() > '' THEN
					IF NOT IsNull(idw_dw[1].GetItemNumber(ll_row,'individual_no')) THEN
						nf_check_payments(idw_dw[1].GetItemNumber(ll_row,'individual_no'), FALSE)
					END IF
				END IF

         CASE 'bank_transit_no'
            IF NOT IsNumber(idw_dw[1].GetText()) THEN
               MessageBox('Error', 'The bank transit number must be numeric.')
					idw_dw[1].SetFocus()
					idw_dw[1].SetColumn('bank_transit_no')
					RETURN -1
            END IF

			CASE ELSE
				inv_address.nf_change_item()
      END CHOOSE

	CASE 2	// dw_main_name


END CHOOSE

Return 0
end function

public function integer nf_validate_claim_cost_alloc ();//Loop through all the individuals claims idw_dw[5] when the
//sin_no has changed or when ib_force_cost_alloc_validation = True
LONG			ll_cost_alloc_no
LONG			ll_cost_alloc_operation_no
LONG			ll_sin_no
LONG			ll_claim_no
STRING		ls_given_names
STRING		ls_last_name
STRING		ls_claimant_full_name
DATETIME		ldt_accident_date
INTEGER		li_x
n_claim_employer		lnv_claim_employer


lnv_claim_employer = CREATE n_claim_employer

IF idw_dw[5].RowCount() = 0 Then
	RETURN 1
END IF

//Validate cost allocation if the sin number has changed
IF idw_dw[1].GetItemStatus(1,'sin_no',Primary!) = DataModified!  Then
	ib_validate_cost_allocation = True
END IF

IF ib_validate_cost_allocation Then
	ll_sin_no 					= idw_dw[1].GetItemNumber(1,'sin_no') 
	ls_given_names			 	= idw_dw[1].GetItemString(1,'given_names')
	ls_last_name				= idw_dw[1].GetItemString(1,'last_name')
	ls_claimant_full_name 	= ls_given_names + ' ' + ls_last_name
	
	For li_x = 1 To idw_dw[5].RowCount()
		ll_cost_alloc_no 				= idw_dw[5].GetItemNumber(li_x,'cost_alloc_no')
		ll_cost_alloc_operation_no = idw_dw[5].GetItemNumber(li_x,'cost_alloc_operation_no')
		ll_claim_no						= idw_dw[5].GetItemNumber(li_x,'claim_no')
		ldt_accident_date				= idw_dw[5].GetItemDateTime(li_x,'accident_date')
		
		IF lnv_claim_employer.nf_set_claim_parameters(ll_claim_no,ldt_accident_date) <> 1 THEN RETURN -1
		IF lnv_claim_employer.nf_set_employer_parameters(ll_cost_alloc_no,ll_cost_alloc_operation_no) <> 1 THEN RETURN -1
		IF lnv_claim_employer.nf_set_claimant_parameters(ls_claimant_full_name,ll_sin_no) <> 1 THEN RETURN -1
		
		/* Only the SIN number can be changed from any individual modules so we only need to
		   validate the personal and volunteer coverage for claims with accidents after 2005-01-01.
			That is the only time that SIN is used as part of cost allocation validation.
			
			idw_dw[5] contains claims that are cost allocated and have an accident date >= 2005-01-01
		*/
		
		IF lnv_claim_employer.nf_validate_SIN_no() <> 1 THEN RETURN -1 
		
		IF lnv_claim_employer.is_volunteer_flag = 'N' Then
			IF lnv_claim_employer.nf_validate_personal_coverage() <> 1 THEN RETURN -1
		ELSE
			IF lnv_claim_employer.nf_validate_volunteer_coverage(lnv_claim_employer.CLAIM) <> 1 THEN RETURN -1
		END IF
		
		
	NEXT
END IF

RETURN 1

end function

public function integer nf_log_events ();N_EVENT_LOG  lnv_event_log
DATASTORE    lds_claims
LONG         ll_loop, ll_rows,ll_individual_no,ll_row,ll_event_no,ll_claim_no
LONG         ll_original_SIN, ll_new_SIN, ll_original_Medicare_number, ll_new_Medicare_number
STRING       ls_string,ls_code, ls_event_text

STRING       ls_original_address_line1,ls_new_address_line1,ls_original_address_line2,ls_new_address_line2
STRING       ls_original_city,ls_new_city,ls_original_prov_state_code,ls_new_prov_state_code
STRING       ls_original_country_code,ls_new_country_code,ls_original_postal_code,ls_new_postal_code


lnv_event_log = create N_EVENT_LOG


ll_row = idw_dw[1].GetRow()

ls_code = idw_dw[1].GetItemString(ll_row, 'individual_court_order_flag')
ls_string = idw_dw[1].GetItemString(ll_row, 'event_desc')

ll_individual_no = idw_dw[1].GetItemNumber(ll_row, 'individual_no')

lds_claims = create datastore
lds_claims.dataobject = 'd_individual_claims'
lds_claims.settransobject(sqlca)
ll_rows = lds_claims.retrieve(ll_individual_no)

/* PR4274 - Order Note Event Not Created - J. Hawker, 2004/10/19
   Log event if the court order flag has been changed.  If there is a new claim 
   being created, log an event for that one as well as all other related claims. 
	If the court order has not been changed and it is checked('Y'), but a new claim 
	is being created, create an event for the newly created claim only.
*/

IF idw_dw[1].GetItemString(ll_row, 'individual_court_order_flag', Primary!, TRUE) <> idw_dw[1].GetItemString(ll_row, 'individual_court_order_flag') THEN
	IF ll_rows > 0 THEN
		FOR ll_loop = 1 TO ll_rows
			ll_claim_no = lds_claims.getitemnumber(ll_loop,'claim_no')
			ll_event_no = lnv_event_log.nf_next_claim_event_no(ll_claim_no)
			lnv_event_log.nf_create_auto_event(ll_claim_no, ll_event_no,'020', ls_string , ls_code)
		NEXT 		
	END IF
	IF il_new_claim_no > 0 THEN
		ll_event_no = lnv_event_log.nf_next_claim_event_no(il_new_claim_no)
		lnv_event_log.nf_create_auto_event(il_new_claim_no, ll_event_no,'020',ls_string, ls_code)
	END IF
ELSEIF il_new_claim_no > 0 AND ls_code = 'Y' THEN
	ll_event_no = lnv_event_log.nf_next_claim_event_no(il_new_claim_no)
	lnv_event_log.nf_create_auto_event(il_new_claim_no, ll_event_no,'020',ls_string, ls_code)
END IF


/*
P10151-264 SIN & Medicare Auto Events
if the SIN or Medicare has changed from a non-zero value, then log an automatic event.
*/


// test if SIN has changed
ll_original_SIN = idw_dw[1].GetItemNumber(ll_row, 'sin_no', Primary!, TRUE)
ll_new_SIN      = idw_dw[1].GetItemNumber(ll_row, 'sin_no')
IF ll_original_SIN <> 0 THEN
	IF ll_original_SIN <> ll_new_SIN THEN
		IF ll_new_SIN <> 0 THEN
			ls_event_text = 'SIN has been changed from ' +String(ll_original_SIN,'###-###-###')+ ' to ' +String(ll_new_SIN,'###-###-###')+ ' for this individual.'
		ELSE
			ls_event_text = 'SIN has been removed from this individual.'
		END IF
		
		ll_event_no = lnv_event_log.nf_next_individual_event_no(il_individual_no)
		lnv_event_log.nf_create_auto_individual_event(ll_individual_no, ll_event_no,'063',ls_event_text,'CG')
	END IF
END IF

// test if Medicare number has changed
ll_original_Medicare_number = idw_dw[1].GetItemNumber(ll_row, 'medicare_no', Primary!, TRUE)
ll_new_Medicare_number      = idw_dw[1].GetItemNumber(ll_row, 'medicare_no')
IF ll_original_Medicare_number <> 0 THEN
	IF ll_original_Medicare_number <> ll_new_Medicare_number THEN
		IF ll_new_Medicare_number <> 0 THEN
			ls_event_text = 'Medicare number has been changed from ' +String(ll_original_Medicare_number,'###-###-###')+ ' to ' +String(ll_new_Medicare_number,'###-###-###')+ ' for this individual.'
		ELSE
			ls_event_text = 'Medicare number has been removed from this individual.'
		END IF
		
		ll_event_no = lnv_event_log.nf_next_individual_event_no(il_individual_no)
		lnv_event_log.nf_create_auto_individual_event(ll_individual_no, ll_event_no,'064',ls_event_text,'CG')
	END IF
END IF

// test if some part of the individual's address has changed
ls_original_address_line1   = Trim(idw_dw[1].GetItemString(ll_row,'address_line1',Primary!,TRUE))
ls_new_address_line1        = Trim(idw_dw[1].GetItemString(ll_row,'address_line1'))

ls_original_address_line2   = Trim(idw_dw[1].GetItemString(ll_row,'address_line2',Primary!,TRUE))
ls_new_address_line2        = Trim(idw_dw[1].GetItemString(ll_row,'address_line2'))


ls_original_city            = Trim(idw_dw[1].GetItemString(ll_row,'city',Primary!,TRUE))
ls_new_city                 = Trim(idw_dw[1].GetItemString(ll_row,'city'))

ls_original_prov_state_code = Trim(idw_dw[1].GetItemString(ll_row,'prov_state_code',Primary!,TRUE))
ls_new_prov_state_code      = Trim(idw_dw[1].GetItemString(ll_row,'prov_state_code'))

ls_original_country_code    = Trim(idw_dw[1].GetItemString(ll_row,'country_code',Primary!,TRUE))
ls_new_country_code         = Trim(idw_dw[1].GetItemString(ll_row,'country_code'))

ls_original_postal_code     = Trim(idw_dw[1].GetItemString(ll_row,'postal_code',Primary!,TRUE))
ls_new_postal_code          = Trim(idw_dw[1].GetItemString(ll_row,'postal_code'))

IF ls_original_address_line1   <> ls_new_address_line1   OR &
   ls_original_address_line2   <> ls_new_address_line2   OR &
   ls_original_city            <> ls_new_city            OR &
   ls_original_prov_state_code <> ls_new_prov_state_code OR &
   ls_original_country_code    <> ls_new_country_code    OR &
   ls_original_postal_code     <> ls_new_postal_code THEN
	
	ls_event_text = "This individual's address has changed from: " &
	              + '~r~n' &
					  + '~r~n~t' + ls_original_address_line1 +' '+ ls_original_address_line2 +' '+ ls_original_city &
					  + ' '      + ls_original_prov_state_code +' '+ ls_original_country_code +' '+ ls_original_postal_code &
					  + '~r~nto' &
					  + '~r~n~t' + ls_new_address_line1 +' '+ ls_new_address_line2 +' '+ ls_new_city &
					  + ' '      + ls_new_prov_state_code +' '+ ls_new_country_code +' '+ ls_new_postal_code
	ll_event_no = lnv_event_log.nf_next_individual_event_no(il_individual_no)
	lnv_event_log.nf_create_auto_individual_event(ll_individual_no, ll_event_no,'065',ls_event_text,'CG')
END IF


/*
end P10151-264 changes
*/


DESTROY lds_claims

RETURN 0
end function

public function integer nf_validate_birth_death_date (string as_action);/* These are existing rules but do not work in production or in the Epay project. 
   We need them fixed as it can cause an Application Error in the Eligibility Export.

1.18	Birth date must be prior to the earliest Accident Date for all 
      Individuals being merged

1.23	Death Date must be after most recent Accident Date for all 
      Individuals being merged
*/

Long        ll_individual_no
DATETIME    ldtm_birth_date, ldtm_death_date,ldt_min_accident_date,ldt_max_accident_date
Datetime    ldt_min_accident_recurrence_date, ldt_date, ldt_max_accident_recurrence_date
STRING    ls_individual, ls_sql, ls_temp
INTEGER    li_rowcount,li_counter

CHOOSE CASE as_action
	CASE "BIRTH"
		
		ldtm_birth_date = idw_dw[1].GetItemDateTime(1,'birth_date')
		IF isnull(ldtm_birth_date) THEN RETURN 1
		
		li_rowcount = w_dup_fix_individual.dw_individual_list.RowCount()
		IF isnull(li_rowcount) OR li_rowcount < 1 THEN RETURN 1
		
		/* this will be our where clause for all of the individual numbers */
		ls_individual = ""
		SetNull(ldt_min_accident_recurrence_date)
		SetNull(ldt_min_accident_date)
		FOR li_counter = 1 TO li_rowcount
			ll_individual_no = w_dup_fix_individual.dw_individual_list.GetItemNumber(li_counter,'individual_no')

			SELECT MIN(O.accident_recurrence_date) 
			  INTO :ldt_date 
			  FROM OPENING O, 
			       CLAIM C, 
					 CLAIM_PARTICIPANT CP 
			 WHERE O.claim_no = C.claim_no 
			   AND C.claim_no = CP.claim_no 
			   AND CP.claim_role_code = "C" 
			   AND C.individual_no = :ll_individual_no ; 

			SQLCA.nf_handle_error('n_individual',"", "nf_validate_birth_death_date - SELECT MIN(O.accident_recurrence_date) FROM OPENING O, CLAIM C, CLAIM_PARTICIPANT CP")

			IF IsNull(ldt_date) = FALSE THEN
				IF IsNull(ldt_min_accident_recurrence_date) = TRUE THEN
					ldt_min_accident_recurrence_date = ldt_date
				ELSE
					IF ldt_date < ldt_min_accident_recurrence_date THEN
						ldt_min_accident_recurrence_date = ldt_date
					END IF
				END IF
			END IF

			SELECT MIN(C.accident_date) 
			  INTO :ldt_date 
			  FROM CLAIM C, 
			       INDIVIDUAL I, 
					 CLAIM_PARTICIPANT CP 
			 WHERE C.claim_no = CP.claim_no 
			   AND CP.claim_role_code = "C" 
			   AND I.individual_no = CP.individual_no 
			   AND I.individual_no = :ll_individual_no ;

			SQLCA.nf_handle_error('n_individual',"", "nf_validate_birth_death_date - SELECT MIN(C.accident_date) FROM OPENING O, CLAIM C, CLAIM_PARTICIPANT CP")

			IF IsNull(ldt_date) = FALSE THEN
				IF IsNull(ldt_min_accident_date) = TRUE THEN
					ldt_min_accident_date = ldt_date
				ELSE
					IF ldt_date < ldt_min_accident_date THEN
						ldt_min_accident_date = ldt_date
					END IF
				END IF
			END IF
		NEXT

		IF IsNull(ldt_min_accident_recurrence_date) = FALSE THEN
			IF IsNull(ldt_min_accident_date) = TRUE THEN
				ldt_min_accident_date = ldt_min_accident_recurrence_date
			ELSE
				IF ldt_min_accident_recurrence_date < ldt_min_accident_date THEN
					ldt_min_accident_date = ldt_min_accident_recurrence_date
				END IF
			END IF
		END IF
		
		IF NOT IsNull(ldt_min_accident_date) THEN
			IF ldt_min_accident_date < ldtm_birth_date THEN
				Return -1
			END IF
		END IF
		

	CASE "DEATH"
		ldtm_death_date = idw_dw[1].GetItemDateTime(1,'death_date')
		IF isnull(ldtm_death_date) THEN RETURN 1
			
		li_rowcount = w_dup_fix_individual.dw_individual_list.RowCount()
		IF isnull(li_rowcount) OR li_rowcount < 1 THEN RETURN 1
		
		/* this will be our where clause for all of the individual numbers */
		ls_individual = ""
		SetNull(ldt_max_accident_recurrence_date)
		SetNull(ldt_max_accident_date)
		FOR li_counter = 1 TO li_rowcount
			ll_individual_no = w_dup_fix_individual.dw_individual_list.GetItemNumber(li_counter,'individual_no')

			SELECT MAX(O.accident_recurrence_date) 
			  INTO :ldt_date 
			  FROM OPENING O, 
			       CLAIM C, 
					 CLAIM_PARTICIPANT CP 
			 WHERE O.claim_no = C.claim_no 
			   AND C.claim_no = CP.claim_no 
			   AND CP.claim_role_code = "C" 
			   AND C.individual_no = :ll_individual_no ; 
				
			SQLCA.nf_handle_error('n_individual',"", "nf_validate_birth_death_date - SELECT MAX(O.accident_recurrence_date) FROM OPENING O, CLAIM C, CLAIM_PARTICIPANT CP")

			IF IsNull(ldt_date) = FALSE THEN
				IF IsNull(ldt_max_accident_recurrence_date) = TRUE THEN
					ldt_max_accident_recurrence_date = ldt_date
				ELSE
					IF ldt_date > ldt_max_accident_recurrence_date THEN
						ldt_max_accident_recurrence_date = ldt_date
					END IF
				END IF
			END IF

			SELECT MAX(C.accident_date) 
			  INTO :ldt_date
			  FROM CLAIM C, 
			       INDIVIDUAL I, 
					 CLAIM_PARTICIPANT CP 
			 WHERE C.claim_no = CP.claim_no 
			   AND CP.claim_role_code = "C" 
			   AND I.individual_no = CP.individual_no 
			   AND I.individual_no = :ll_individual_no ; 

			SQLCA.nf_handle_error('n_individual',"", "nf_validate_birth_death_date - SELECT MAX(C.accident_date) FROM OPENING O, CLAIM C, CLAIM_PARTICIPANT CP")

			IF IsNull(ldt_date) = FALSE THEN
				IF IsNull(ldt_max_accident_date) = TRUE THEN
					ldt_max_accident_date = ldt_date
				ELSE
					IF ldt_date > ldt_max_accident_date THEN
						ldt_max_accident_date = ldt_date
					END IF
				END IF
			END IF
	   NEXT
		
		IF IsNull(ldt_max_accident_recurrence_date) = FALSE THEN
			IF IsNull(ldt_max_accident_date) = TRUE THEN
				ldt_max_accident_date = ldt_max_accident_recurrence_date
			ELSE
				IF ldt_max_accident_recurrence_date > ldt_max_accident_date THEN
					ldt_max_accident_date = ldt_max_accident_recurrence_date
				END IF
			END IF
		END IF
			
		IF NOT IsNull(ldt_max_accident_date) THEN
			IF ldt_max_accident_date > ldtm_death_date THEN
				Return -1
			END IF
		END IF
END CHOOSE

RETURN 1
end function

public function integer nf_check_bus_rule ();// nf_check_bus_rule
//
//	Return -1 if an error occurs
//	Return -201 if an error and it is a duplicate SIN
//	Return -202 if an error and it is a duplicate medicare
//	Return -203 if the main and alias names are the same (Added for PR 2011, July 2001)

//Changing Business Rules may impact the Client Tab in CMS as rules are duplicated there. 
//Rules in CMS should be revised. 

LONG     		ll_no, ll_row, ll_cnt, ll_individual_no, ll_cntr,ll_indiv_no, ll_bank_info, ll_pmts, ll_count
LONG     		ll_result, ll_age, ll_ann_rows, ll_claim_no, ll_annuity_account_no, ll_current_year, ll_annuity_eligibility_no
STRING   	ls_string, ls_string2, ls_award_type, ls_bank_info,ls_pmts,ls_message
STRING   	ls_sex, ls_orig_sex, ls_address_line1, ls_address_line2, ls_day_month, ls_role, ls_claim_role_code
STRING   	ls_last_name, ls_first_name, ls_claimant_role, ls_claim_role_desc
DATETIME 	ldt_max_accident_date, ldt_min_accident_date, ldt_orig_birth_date, ldt_birth_date
DATETIME 	ldt_death_date, ldt_orig_death_date, ldtm_validate_bday, ldtm_today, ldtm_annuity_end_date_calculated
DATETIME 	ldt_min_accident_recurrence_date, ldt_max_accident_recurrence_date, ldtm_annuity_start_date
DATETIME	ldtm_annuity_end_date, ldtm_old_bday, ldtm_null
INTEGER  	li_rtn, li_counter, li_count, li_pos
BOOLEAN	lb_past_pmts
U_DS 			lds_individual_accident_dates

n_common_annuity lnv_common_annuity
lnv_common_annuity = create n_common_annuity

ldtm_today = f_server_datetime()
	
ll_row = idw_dw[1].GetRow()
IF ll_row = 1 THEN
	// validate the last and given names are less than 30 also check for alias
	ll_individual_no = idw_dw[1].GetItemNumber(ll_row,'individual_no')
	
	ll_ann_rows 		= ids_annuity_eligibility.Retrieve(ll_individual_no)	
	SQLCA.nf_handle_error('n_individual','nf_check_bus_rule()', 'ids_annuity_eligibility.Retrieve(ll_individual_no)')
	
	ls_last_name  = idw_dw[2].GetItemString(1,'last_name')
	ls_first_name = idw_dw[2].GetItemString(1,'given_names')
	
	IF IsNull(ls_last_name) OR TRIM(ls_last_name) = '' THEN
		MessageBox("Missing Name", "The Last Name must be supplied.  Please enter.")
		idw_dw[2].SetColumn('last_name')
		idw_dw[2].SetFocus()
		Return -1
	END IF
	
	// Individual Maintenance BR 1.120
	li_pos = Pos(ls_last_name,'ESTATE')
	IF li_pos > 0 THEN
		MessageBox("Given Name Error", "The Last Name must not contain 'ESTATE'.  Please correct.")
		idw_dw[2].SetColumn('last_name')
		idw_dw[2].SetFocus()
		Return -1
	END IF
	
	// Individual Maintenance BR 1.120
	li_pos = Pos(ls_last_name,'SUCCESSION')
	IF li_pos > 0 THEN
		MessageBox("Given Name Error", "The Last Name must not contain 'SUCCESSION'.  Please correct.")
		idw_dw[2].SetColumn('last_name')
		idw_dw[2].SetFocus()
		Return -1
	END IF
	
	
	IF IsNull(ls_first_name) OR TRIM(ls_first_name) = '' THEN
		MessageBox("Missing Name", "The First Name must be supplied.  Please enter.")
		idw_dw[2].SetColumn('given_names')
		idw_dw[2].SetFocus()
		Return -1
	END IF
	
	// Individual Maintenance BR 1.120
	li_pos = Pos(ls_first_name,'ESTATE')
	IF li_pos > 0 THEN
		MessageBox("Given Name Error", "The First Name must not contain 'ESTATE'.  Please correct.")
		idw_dw[2].SetColumn('given_names')
		idw_dw[2].SetFocus()
		Return -1
	END IF
	
	// Individual Maintenance BR 1.120
	li_pos = Pos(ls_first_name,'SUCCESSION')
	IF li_pos > 0 THEN
		MessageBox("Given Name Error", "The First Name must not contain 'SUCCESSION'.  Please correct.")
		idw_dw[2].SetColumn('given_names')
		idw_dw[2].SetFocus()
		Return -1
	END IF
		

	IF Len(idw_dw[2].GetItemString(1,'last_name') + ' ' + idw_dw[2].GetItemString(1,'given_names')) > 30 THEN
		MessageBox("Warning","The full name exceeds the maximum length allowed.  Please correct.")
		idw_dw[2].SetColumn('last_name')
		idw_dw[2].SetFocus()
		Return -1
	ELSE 
		ls_string = nf_reduce_blanks(idw_dw[2].GetItemString(1,'last_name'))
		//BR 1.80
		IF len(ls_string) > 0 AND len(ls_string) < 2 THEN
			MessageBox("Error","The last name must contain a minumum of 2 characters.  Please correct.")
			idw_dw[2].SetColumn('last_name')
			idw_dw[2].SetFocus()
			Return -1
		END IF
		
		//BR 1.85
		IF f_invalid_character_test1(ls_string) AND ll_individual_no <> 2023 THEN 
			MessageBox("Error","The last name must not contain any special characters.  Please correct.")
			idw_dw[2].SetColumn('last_name')
			idw_dw[2].SetFocus()
			Return -1
		END IF
		
		//BR 1.87
		IF f_3_consecutive_character_test(ls_string) THEN
			IF MessageBox('Warning','The last name contains 3 or more consecutive cases of the same character.' &
				 + '  Would you like to continue with the save?', QUESTION!, OKCANCEL!, 2) = 2 THEN
				idw_dw[2].SetColumn('last_name')
				idw_dw[2].SetFocus()
				Return -1
			END IF
		END IF
		
		
		//BR 1.95  
		IF MATCH(left(ls_string,1) , '[^a-z ^A-Z]') AND ll_individual_no <> 2023 AND ll_individual_no <> 5657 THEN   // exceptions are individual 2023 and 5657
			MESSAGEBOX("", "The last name must begin with a letter. Please correct")
			idw_dw[2].SetColumn('last_name')
			idw_dw[2].SetFocus()
			Return -1
		END IF
		
		IF ls_string > '' THEN
			idw_dw[2].SetItem(1,'last_name', ls_string)
		END IF
		
		ls_string = nf_reduce_blanks(idw_dw[2].GetItemString(1,'given_names'))
		
		//BR 1.80
		IF  len(ls_string) > 0 AND len(ls_string) < 2 THEN
			MessageBox("Error","The given name must contain a minumum of 2 characters.  Please correct.")
			idw_dw[2].SetColumn('given_names')
			idw_dw[2].SetFocus()
			Return -1
		END IF
		
		//BR 1.85
		IF f_invalid_character_test1(ls_string) AND ll_individual_no <> 2023 THEN   //one exception to the special character rule, idividual 2023
			MessageBox("Error","The given name must not contain any special characters.  Please correct.")
			idw_dw[2].SetColumn('given_names')
			idw_dw[2].SetFocus()
			Return -1
		END IF		
			
		//BR 1.87
		IF f_3_consecutive_character_test(ls_string) THEN
			IF MessageBox('Warning','The given name contains 3 or more consecutive cases of the same character.' &
				 	+ '  Would you like to continue with the save?', QUESTION!, OKCANCEL!, 2) = 2 THEN
				idw_dw[2].SetColumn('given_names')
				idw_dw[2].SetFocus()
				Return -1	 
			END IF 
		END IF
		
		//BR 1.95  
		IF MATCH(left(ls_string,1) , '[^a-z ^A-Z]') AND ll_individual_no <> 2023 AND ll_individual_no <> 5657 THEN   // exceptions are individual 2023 and 5657
			MESSAGEBOX("", "The given name must begin with a letter. Please correct.")
			idw_dw[2].SetColumn('given_names')
			idw_dw[2].SetFocus()
			Return -1	
		END IF
		
		IF ls_string > '' THEN
			idw_dw[2].SetItem(1,'given_names', ls_string)
		END IF
		idw_dw[1].SetItem(ll_row,'last_name',idw_dw[2].GetItemString(1,'last_name'))
		idw_dw[1].SetItem(ll_row,'given_names',idw_dw[2].GetItemString(1,'given_names'))
	END IF	

	//BR 1.60
	IF ll_individual_no <>  419205 and ll_individual_no <>  2023 and ll_individual_no <>  5657  THEN // three exceptions to the rule:  these are permitted to have numbers
		IF nf_check_for_numeric(ls_last_name ) < 0 THEN			
			MessageBox('Error','The last name must not contain any numbers. Please correct.')
			Return -1
		END IF
		
		IF nf_check_for_numeric(ls_first_name ) < 0 THEN			
			MessageBox('Error','The given name must not contain any numbers. Please correct.')
			Return -1
		END IF
	END IF	

	
	//	check the alias
	IF NOT (IsNull(idw_dw[1].GetItemString(ll_row,'individual_name_last_name'))) AND idw_dw[1].GetItemString(ll_row,'individual_name_last_name') > '' THEN
		// must supply given names
		IF IsNull(idw_dw[1].GetItemString(ll_row,'individual_name_given_names')) OR idw_dw[1].GetItemString(ll_row,'individual_name_given_names') = '' THEN
			MessageBox("Error", "You must supply an alias given name if an alias last name is entered.")
			idw_dw[1].SetColumn('individual_name_given_names')
			idw_dw[1].SetFocus()
				Return -1
		END IF

		IF Len(idw_dw[1].GetItemString(ll_row,'individual_name_last_name') + ' ' + idw_dw[1].GetItemString(ll_row,'individual_name_given_names')) > 30 THEN
			MessageBox("Error","The alias full name exceeds the maximum length	 allowed.  Please edit to 30 characters or less.")
			idw_dw[1].SetColumn('individual_name_last_name')
			idw_dw[1].SetFocus()
				Return -1
		END IF

		ls_string = nf_reduce_blanks(idw_dw[1].GetItemString(1,'individual_name_last_name'))
		
		//BR 1.80
		IF  len(ls_string) > 0 AND len(ls_string) < 2 THEN
			MessageBox("Error","The alias last name must contain a minumum of 2 characters.  Please correct.")
			idw_dw[1].SetColumn('individual_name_last_name')
			idw_dw[1].SetFocus()
			Return -1
		END IF
		
		//BR 1.85
		IF f_invalid_character_test1(ls_string) AND ll_individual_no <> 2023 THEN 
			MessageBox("Error","The alias last name must not contain any special characters.  Please correct.")
			idw_dw[1].SetColumn('individual_name_last_name')
			idw_dw[1].SetFocus()
			Return -1
		END IF
		
		//BR 1.87
		IF f_3_consecutive_character_test(ls_string) THEN
			IF MessageBox('Warning','The alias last name contains 3 or more consecutive cases of the same character.' &
				 	+ '  Would you like to continue with the save?', QUESTION!, OKCANCEL!, 2) = 2 THEN
				idw_dw[1].SetColumn('individual_name_last_name')
				idw_dw[1].SetFocus()
				Return -1
			END IF
		END IF
		
		//BR 1.95  
		IF MATCH(left(ls_string,1) , '[^a-z ^A-Z]') AND ll_individual_no <> 2023 AND ll_individual_no <> 5657 THEN   // exceptions are individual 2023 and 5657
			MESSAGEBOX("", "The alias last name must begin with a letter. Please correct.")
			idw_dw[1].SetColumn('individual_name_last_name')
			idw_dw[1].SetFocus()
			Return -1
		END IF
		
		IF ls_string > '' THEN
			idw_dw[1].SetItem(1,'individual_name_last_name', ls_string)
		END IF

		ls_string = nf_reduce_blanks(idw_dw[1].GetItemString(1,'individual_name_given_names'))
		//BR 1.80
		IF  len(ls_string) > 0 AND len(ls_string) < 2 THEN
			MessageBox("Error","The alias given name must contain a minumum of 2 characters.  Please correct.")
			idw_dw[1].SetColumn('individual_name_given_names')
			idw_dw[1].SetFocus()
			Return -1
		END IF	
		
		//BR 1.85
		IF f_invalid_character_test1(ls_string)  AND ll_individual_no <> 2023 THEN 
			MessageBox("Error","The alias given name must not contain any special characters.  Please correct.")
			idw_dw[1].SetColumn('individual_name_given_names')
			idw_dw[1].SetFocus()
			Return -1
		END IF
		
		//BR 1.87
		IF f_3_consecutive_character_test(ls_string) THEN
			IF MessageBox('Warning','The alias given name contains 3 or more consecutive cases of the same character.' &
				 	+ '  Would you like to continue with the save?', QUESTION!, OKCANCEL!, 2) = 2 THEN
				idw_dw[1].SetColumn('individual_name_given_names')
				idw_dw[1].SetFocus()
				Return -1
			END IF	
		END IF
		
		//BR 1.60
		IF nf_check_for_numeric(idw_dw[1].GetItemString(1,'individual_name_last_name') ) < 0 THEN			
			MessageBox('Error','The alias last name must not contain any numbers. Please correct')
			Return -1
		END IF
		
		IF nf_check_for_numeric( idw_dw[1].GetItemString(1,'individual_name_given_names')) < 0 THEN			
			MessageBox('Error','The alias given name must not contain any numbers. Please correct')
			Return -1
		END IF
		
		//BR 1.95  
		IF MATCH(left(ls_string,1) , '[^a-z ^A-Z]') AND ll_individual_no <> 2023 AND ll_individual_no <> 5657 THEN   // exceptions are individual 2023 and 5657
			MESSAGEBOX("", "The alias given name must begin with a letter. Please correct.")
			idw_dw[1].SetColumn('individual_name_last_name')
			idw_dw[1].SetFocus()
			Return -1
		END IF
		
		IF ls_string > '' THEN
			idw_dw[1].SetItem(1,'individual_name_given_names', ls_string)
		END IF
		
	ELSEIF NOT (IsNull(idw_dw[1].GetItemString(ll_row,'individual_name_given_names'))) AND idw_dw[1].GetItemString(ll_row,'individual_name_given_names') > '' THEN
		MessageBox("Error", "You must supply an alias last name if an alias given name is entered.")
		idw_dw[1].SetColumn('individual_name_last_name')
		idw_dw[1].SetFocus()
		Return -1
	END IF

	// Check that the alias is not the same as the main name
	IF NOT (IsNull(idw_dw[1].GetItemString(ll_row,'individual_name_last_name'))) AND idw_dw[1].GetItemString(ll_row,'individual_name_last_name') > '' THEN
		IF idw_dw[1].GetItemString(ll_row,'individual_name_last_name') + ' ' + idw_dw[1].GetItemString(ll_row,'individual_name_given_names') = &
			idw_dw[2].GetItemString(1,'last_name') + ' ' + idw_dw[2].GetItemString(1,'given_names') THEN
			MessageBox("Error", "The alias cannot be the same as the main name, please delete or change it.")
			Return -203
		END IF
	END IF

	// set up the data window to update the alias name into the individual name table
	IF nf_insert_individual_name() < 0 THEN
		Return -1
	END IF

	// Get the Soundex values
	ls_string = idw_dw[2].GetItemString(1,'last_name')	
	SELECT top 1 Soundex(:ls_string)
	INTO :ls_string2  
	FROM sysobjects;

	SQLCA.nf_handle_error('Select soundex','n_individual', 'nf_check_bus_rule') 
	
	idw_dw[2].SetItem(1, 'soundex_last_name', ls_string2)
	
	ls_string = idw_dw[2].GetItemString(1,'given_names')	
	SELECT top 1 Soundex(:ls_string)
	INTO :ls_string2
	FROM sysobjects;
	
	SQLCA.nf_handle_error('Select soundex','n_individual', 'nf_check_bus_rule') 
	
	idw_dw[2].SetItem(1, 'soundex_given_names', ls_string2)

	IF idw_dw[3].RowCount() = 1 THEN
		ls_string = idw_dw[3].GetItemString(1,'last_name')	
		SELECT top 1 Soundex(:ls_string)
		INTO :ls_string2
		FROM sysobjects;
		
		SQLCA.nf_handle_error('Select soundex','n_individual', 'nf_check_bus_rule') 
		
		idw_dw[3].SetItem(1, 'soundex_last_name', ls_string2)

		ls_string = idw_dw[3].GetItemString(1,'given_names')	
		SELECT top 1 Soundex(:ls_string)
		INTO :ls_string2
		FROM sysobjects;
		
		SQLCA.nf_handle_error('Select soundex','n_individual', 'nf_check_bus_rule') 
		
		idw_dw[3].SetItem(1, 'soundex_given_names', ls_string2)
	END IF

	// check medicare 
	ll_no = idw_dw[1].GetItemNumber(ll_row, 'medicare_no')
	IF NOT (IsNull(ll_no) OR ll_no = 0) THEN
			IF nf_check_digit(ll_no, 'M') = -1 THEN
				idw_dw[1].SetColumn('medicare_no')
				idw_dw[1].SetFocus()
				Return -1
			END IF

		IF ib_validate_medicare_uniqueness = TRUE THEN
			// validate uniqueness 
			IF idw_dw[1].GetItemStatus(ll_row,0,Primary!) = NewModified! THEN
				SELECT Count(*) 
				INTO :ll_cnt
				FROM INDIVIDUAL
				WHERE medicare_no = :ll_no;
				SQLCA.nf_handle_error('Embedded SQL: select from INDIVIDUAL', 'n_individual', 'nf_check_bus_rule' ) 
				
			ELSE
				// exclude the current individual
				ll_individual_no = idw_dw[1].GetItemNumber(ll_row,'individual_no')
					SELECT Count(*) 
						INTO :ll_cnt
					FROM INDIVIDUAL
					WHERE medicare_no = :ll_no
						AND individual_no <> :ll_individual_no;
					SQLCA.nf_handle_error('Embedded SQL: select from INDIVIDUAL', 'n_individual', 'nf_check_bus_rule' ) 
					
			END IF
			IF ll_cnt > 0 THEN
				MessageBox("Error", "Another individual exists with the same medicare number.")
				idw_dw[1].SetColumn('medicare_no')
				idw_dw[1].SetFocus()
				Return -202
			END IF 
		END IF
	END IF

	//	make sure the sin # is valid
	ll_no = idw_dw[1].GetItemNumber(ll_row, 'sin_no')
	IF NOT (IsNull(ll_no) OR ll_no = 0) THEN
		IF nf_check_digit(ll_no, 'S') = -1 THEN
			idw_dw[1].SetColumn('sin_no')
			idw_dw[1].SetFocus()
			Return -1
		END IF

		IF ib_validate_sin_uniqueness = TRUE THEN
			//	validate uniqueness
			IF idw_dw[1].GetItemStatus(ll_row,0,Primary!) = NewModified! THEN
				SELECT Count(*) 
				INTO :ll_cnt
				FROM INDIVIDUAL
				WHERE sin_no = :ll_no
				AND history_flag = 'N';
				SQLCA.nf_handle_error('Embedded SQL: select from INDIVIDUAL', 'n_individual', 'nf_check_bus_rule' ) 
				
			ELSE
				//	exclude the current individual
				ll_individual_no = idw_dw[1].GetItemNumber(ll_row,'individual_no')
				SELECT Count(*) 
				INTO :ll_cnt
				FROM INDIVIDUAL
				WHERE sin_no = :ll_no
				AND individual_no <> :ll_individual_no
				AND history_flag = 'N';
				SQLCA.nf_handle_error('Embedded SQL: select from INDIVIDUAL', 'n_individual', 'nf_check_bus_rule' ) 
				
			END IF
			IF ll_cnt > 0 THEN
				MessageBox("Error", "Another individual exists with the same SIN.")
				idw_dw[1].SetColumn('sin_no')
				idw_dw[1].SetFocus()
				Return -201
			END IF 
		END IF
	END IF

	IF NOT (IsNull(idw_dw[1].GetItemDateTime(ll_row,'birth_date')) OR IsNull(idw_dw[1].GetItemDateTime(ll_row,'death_date')))  THEN
		// check that birth date before death
		IF idw_dw[1].GetItemDateTime(ll_row,'birth_date') >= idw_dw[1].GetItemDateTime(ll_row,'death_date') THEN
			MessageBox("Error", "Birth date must be before death date.")
			idw_dw[1].SetColumn('birth_date')
			idw_dw[1].SetFocus()
			Return -1
		END IF
	END IF
	
	/*
	2.280	If an active annuity eligibility exists for the annuity account, and the death date is changed, the annuity eligibility end date must not be prior to the 
			annuity eligibility start date.	
	** renumbered to ** 
	2.275	The death date of the individual must not be changed if active annuity eligibility exists for the annuity account of the individual 
			and the death date will cause the annuity eligibility end date to be on or prior to the annuity eligibility start date. 
			(Note: There may be more than one annuity account for the individual)
	*/
	
			

	ldt_orig_death_date 	= idw_dw[1].GetItemDateTime(ll_row, "death_date", Primary!, TRUE)
	ldt_death_date 		= idw_dw[1].GetItemDateTime(ll_row, "death_date")
	ldt_orig_birth_date  = idw_dw[1].GetItemDateTime(ll_row, "birth_date", Primary!, TRUE)
	ldt_birth_date 		= idw_dw[1].GetItemDateTime(ll_row, "birth_date")

	/*	
	 BR2.271	The birth date of the individual must not be changed if all of the following are true:		
		•	The individual has an annuity account
		•	The change affects the year and/or month of the birth date
		•	One of the following is true:
			o	There is an outstanding Confirm Annuity Eligibility potential checklist for the annuity account with the ‘Confirm Birth Date’ step completed 
			o	There is an outstanding Confirm Annuity Eligibility payout checklist for the annuity account with the ‘Confirm Birth Date’ step completed (i.e. the annuity payout is in progress)
			o	There is a completed Confirm Annuity Eligibility payout checklist for the annuity account and the annuity payout is in progress
			
	*/
	li_rtn = nf_check_annuity_birth_date(ll_individual_no, ldt_orig_birth_date, ldt_birth_date)
	IF li_rtn < 0 THEN 
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('birth_date')
		RETURN -1	
	END IF 
	
	/*	
	BR2.279: The death date of the individual must not be changed if all of the following are true:
				•	The individual has an annuity account
				•	The change affects the year and/or month of the death date
				•	One of the following is true:
					o	There is an outstanding Confirm Annuity Eligibility potential checklist for the annuity account with the ‘Confirm Annuity Eligibility’ step completed 
					o	There is an outstanding Confirm Annuity Eligibility payout checklist for the annuity account with the ‘Confirm Annuity Eligibility’ step completed (i.e. the annuity payout is in progress)
					o	There is a completed Confirm Annuity Eligibility payout checklist for the annuity account and the annuity payout is in progress
	*/
	li_rtn = nf_check_annuity_death_date(ll_individual_no, ldt_orig_death_date, ldt_death_date)
	IF li_rtn < 0 THEN 
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('death_date')
		RETURN -1	
	END IF
	
	/*
	BR2.280	The death date of the individual must not be changed if all of the following are true:
				•	The individual is involved in an annuity payout that is in progress
				•	The individual is a dependent annuity payout participant
				•	The change affects the year and/or month and/or day of the death date 
				•	One of the following is true:
					o	overpayment recovery has been confirmed for the annuity payout
					o	if deceased, the dependent’s death date is on or before the death date of the individual who is eligible for annuity benefits, if the eligible individual died prior to the age of 65
					o	if deceased, the dependent’s death date is on or before the 65th birthday of the individual who is eligible for annuity benefits
					o	if deceased, the dependent is represented by another annuity payout participant who is not an estate
					o	if not deceased, the dependent is represented by another annuity payout participant who is an estate
	*/
	
	li_rtn = nf_check_annuity_dependant_death_date(ll_individual_no, ldt_orig_death_date, ldt_death_date)
	IF li_rtn < 0 THEN 
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('death_date')
		RETURN -1	
	END IF
	

	IF ldt_orig_death_date <> ldt_death_date OR (isnull(ldt_orig_death_date) AND NOT ISNULL(ldt_death_date)) OR (isnull(ldt_death_date) AND NOT isnull(ldt_orig_death_date)) THEN		
		IF ll_ann_rows > 0 THEN		
			FOR li_counter = 1 TO ll_ann_rows
						
				//GRAB THE VALUES REQUIRED FROM THE DATASTORE
				ldtm_annuity_start_date 		= ids_annuity_eligibility.GetItemDateTime(li_counter,'annuity_start_date')	
				ls_claim_role_code         	= ids_annuity_eligibility.getitemstring(li_counter,'claim_role_code')
				ll_annuity_account_no     	= ids_annuity_eligibility.getitemnumber(li_counter,'annuity_account_no')  
				ll_annuity_eligibility_no   	 	= ids_annuity_eligibility.getitemnumber(li_counter,'annuity_eligibility_no')  
						
				IF nf_check_annuity_end_date_brs(4,ll_individual_no,date(ldt_birth_date), date(ldt_death_date), ldtm_annuity_start_date, ls_claim_role_code) = -1 THEN
					MessageBox('Error', 'The Death Date has changed, the annuity eligibility end date must not be prior to the annuity eligibility start date!', Exclamation!)
					idw_dw[1].SetFocus()
					idw_dw[1].SetColumn('death_date')
					RETURN -1
				END IF	
			NEXT
		ELSE
			IF nf_check_IW_death_date(ll_individual_no,date(ldt_death_date)) = -1 THEN
				idw_dw[1].SetFocus()
				idw_dw[1].SetColumn('death_date')
				RETURN -1
			END IF
		END IF 
	END IF 

	IF nf_check_SS_death_date(ll_individual_no,date(ldt_death_date)) = -1 THEN
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('death_date')
		RETURN -1
	END IF

	// Birth date must not be removed once it is set	
	ldt_birth_date = idw_dw[1].GetItemDateTime(ll_row, "birth_date", Primary!, FALSE)
	IF IsDate(String(ldt_orig_birth_date, "yyyy-mm-dd")) = TRUE AND IsDate(String(ldt_birth_date, "yyyy-mm-dd")) = FALSE THEN
		MessageBox("Error", "Birth date must not be removed once it is set.")
		idw_dw[1].SetItem(ll_row, "birth_date", ldt_orig_birth_date)
		idw_dw[1].SetColumn('birth_date')
		idw_dw[1].SetFocus()
		Return -1
	END IF

	
	IF NOT IsNull(idw_dw[1].GetItemDateTime(ll_row,'birth_date')) THEN
		
		//IF ldt_orig_birth_date <> ldt_birth_date THEN
		IF ldt_orig_birth_date <> ldt_birth_date OR (isnull(ldt_orig_birth_date) AND NOT ISNULL(ldt_birth_date)) OR (isnull(ldt_birth_date) AND NOT isnull(ldt_orig_birth_date)) THEN

			/*
			2.250 	If an active annuity eligibility exists for the annuity account, and the birthdate is changed causing the annuity end date to be set in the future, 
						and scheduled payments/awards exists after the new annuity end date, a message is displayed indicating the payments should be reviewed.
			2.260 	If an active annuity eligibility exists for the annuity account, and the birthdate is changed causing the annuity end date to be set in the past, 
						and payments/awards exists after the new annuity end date, a message is displayed indicating the claims/payments should be reviewed.
			2.270		If an active annuity eligibility exists for the annuity account, and the birth date is changed, the annuity eligibility end date must not be prior to the 
						annuity eligibility start date.	
			*/					
			IF ll_ann_rows > 0 THEN
				FOR li_counter = 1 TO ll_ann_rows	
						
					//grab the information we need for the annuity datawindow
					ll_claim_no 						= ids_annuity_eligibility.GetItemnumber(li_counter,'claim_no')
					ls_claim_role_code 			= ids_annuity_eligibility.GetItemstring(li_counter,'claim_role_code')
					ldtm_annuity_end_date 		= ids_annuity_eligibility.GetItemDateTime(li_counter,'annuity_end_date')
					ldtm_annuity_start_date 		= ids_annuity_eligibility.GetItemDateTime(li_counter,'annuity_start_date')
					ll_annuity_account_no     	= ids_annuity_eligibility.getitemnumber(li_counter,'annuity_account_no') 
					ll_annuity_eligibility_no   		= ids_annuity_eligibility.getitemnumber(li_counter,'annuity_eligibility_no')  
							
					IF nf_check_annuity_end_date_brs(3, ll_individual_no, date(ldt_birth_date), date(ldt_death_date), ldtm_annuity_start_date, ls_claim_role_code) = -1 THEN
						MessageBox('Validation Error', 'The birth date has changed, the annuity eligibility end date must not be prior to the annuity eligibility start date!', Exclamation!)
						idw_dw[1].SetFocus()
						idw_dw[1].SetColumn('birth_date')
						RETURN -1
					END IF 
					
				// ldtm_annuity_end_date_calculated returned by reference
				lnv_common_annuity.nf_get_annuity_end_date(ll_individual_no,ls_claim_role_desc , ls_message, ldtm_annuity_end_date_calculated, ldtm_null, ldt_birth_date, ldt_death_date)
																	
				IF NOT IsNull(ldtm_annuity_end_date) THEN
													
					IF DATE(ldtm_annuity_end_date_calculated) <> DATE(ldtm_annuity_end_date) THEN
													
						IF DATE(ldtm_annuity_end_date_calculated) < DATE(f_server_datetime()) THEN
							lb_past_pmts = TRUE
						END IF
																
						//BR 2.250 & BR 2.260	
						IF nf_check_benefit_payments(ldtm_annuity_end_date_calculated, lb_past_pmts,ll_claim_no,ll_individual_no, ls_claim_role_code) < 0 THEN
							idw_dw[1].SetItem(1,'birth_date',ldtm_old_bday)
							idw_dw[1].SetFocus()
							idw_dw[1].SetColumn('birth_date')
							RETURN -1
						END IF
					END IF
				END IF
			NEXT
		END IF
	END IF				
		
		ll_current_year = Year(Date(ldtm_today))
		ls_day_month	 = '-' + STRING(Month(Date(ldtm_today))) + '-' + STRING(Day(Date(ldtm_today)))
		ldtm_validate_bday = DateTime(Date(STRING(ll_current_year - 150) + ls_day_month))
		IF Date(idw_dw[1].GetItemDateTime(ll_row,'birth_date')) < Date(ldtm_validate_bday) THEN
			MessageBox('Invalid Birth Date','The Birthdate is more than 150 years in the past. Please review.')
			idw_dw[1].SetColumn('birth_date')
			idw_dw[1].SetFocus()
			Return -1
		END IF
		
		// SR 151 - birth date cannot be after earliest accident date for the claim
		SELECT MIN(O.accident_recurrence_date) 
		  INTO :ldt_min_accident_recurrence_date 
		  FROM OPENING O, 
		       CLAIM C, 
				 CLAIM_PARTICIPANT CP  
		 WHERE O.claim_no = C.claim_no 
		   AND C.claim_no = CP.claim_no 
		   AND CP.claim_role_code = "C" 
		   AND C.individual_no = :ll_individual_no ; 
		
		SQLCA.nf_handle_error('n_individual',"", "nf_check_bus_rule - SELECT MIN(O.accident_recurrence_date) FROM OPENING O, CLAIM C, CLAIM_PARTICIPANT CP")

		SELECT MIN(C.accident_date) 
		  INTO :ldt_min_accident_date 
		  FROM CLAIM C, 
		       INDIVIDUAL I, 
				 CLAIM_PARTICIPANT CP  
		 WHERE C.claim_no = CP.claim_no 
		   AND CP.claim_role_code = "C" 
		   AND I.individual_no = CP.individual_no 
		   AND I.individual_no = :ll_individual_no ; 

		SQLCA.nf_handle_error('n_individual',"", "nf_check_bus_rule - SELECT MIN(C.accident_date) FROM OPENING O, CLAIM C, CLAIM_PARTICIPANT CP")

		IF IsNull(ldt_min_accident_recurrence_date) = FALSE THEN
			IF IsNull(ldt_min_accident_date) = TRUE THEN
				ldt_min_accident_date = ldt_min_accident_recurrence_date
			ELSE
				IF ldt_min_accident_recurrence_date < ldt_min_accident_date THEN
					ldt_min_accident_date = ldt_min_accident_recurrence_date
				END IF
			END IF
		END IF
				
		IF NOT IsNull(ldt_min_accident_date) THEN
			IF ldt_min_accident_date < idw_dw[1].GetItemDateTime(ll_row,'birth_date') THEN
				MessageBox("Error", "Birth date must be prior to earliest accident date for all claims!")
				idw_dw[1].SetColumn('birth_date')
				idw_dw[1].SetFocus()
				Return -1
			END IF
		END IF
	END IF
	
	IF NOT (IsNull(idw_dw[1].GetItemDateTime(ll_row,'birth_date'))) AND idw_dw[1].GetItemDateTime(ll_row,'birth_date') > f_server_datetime() THEN
		MessageBox("Error", "Birth date must be less than today!")
		idw_dw[1].SetColumn('birth_date')
		idw_dw[1].SetFocus()
		Return -1
	END IF
	
	/* PR4275 - After validations have passed, warn users if birthdate is out of normal range
	*/
	IF NOT (IsNull(idw_dw[1].GetItemDateTime(ll_row,'birth_date'))) THEN
		/* PR7034 this one line is part of PR below; fixes problem if this code is being run by the fix duplicate individual merge module, then il_individual_no is the default individual's #
		*/
		IF ll_individual_no = 0 and il_individual_no > 0  then ll_individual_no = il_individual_no
		
		SELECT	claim_role_code
		INTO		:ls_role
		FROM		CLAIM_PARTICIPANT
		WHERE	   individual_no = :ll_individual_no
		USING    SQLCA;
							
		SQLCA.nf_handle_error("n_individual","Embedded SQL: SELECT CLAIM_PARTICIPANT","nf_check_bus_rule")
				
		ll_age = idw_dw[1].GetItemNumber(1,'age')
	
	// PR7034 2008-02-25 R.S. ls_role was being populated for existing participants only. For new claimants, it wasn't; we now set 
	// ib_new_claim in n_create_claim.nf_init(),  so it can be verified here to check this new claimants age. Also get and check the participant claim_role_code from 
	// the 'Add Participant' window; the user might have decided to make the new participant a 'Claimant' by inactivating the original one.
	
	IF upperbound(idw_dw) >= 8   THEN             // first check if  participant dw exists, this will depend on what module called this function, i.e, 'Add Participant'
		IF idw_dw[08].dataobject = 'd_claim_participant_claim_maint' THEN              // check if its the right dw object
			ls_claimant_role = idw_dw[08].getitemstring(idw_dw[08].getrow(),'claim_role_code')
		END IF
	END IF
	
		IF (ll_age < 16 OR ll_age > 65) AND (ls_role = 'C' OR THIS.ib_new_claim = TRUE OR ls_claimant_role = 'C') THEN
			IF MessageBox('Warning!', "The birth date indicates the individual's age is not within the normal range of 16 to 65 years." +&
								"~rDo you wish to continue with the save?",QUESTION!,YESNO!,2 ) = 2 THEN
				idw_dw[1].SetColumn('birth_date')
				idw_dw[1].SetFocus()
				Return -1
			END IF
		END IF
	END IF

	// A warning message is displayed if individual's date of death is removed and the 
	// individual has atleast one claim that is registered for drug coverage
	ldt_orig_death_date = idw_dw[1].GetItemDateTime(ll_row, "death_date", Primary!, TRUE)
	ldt_death_date = idw_dw[1].GetItemDateTime(ll_row, "death_date")


	IF IsNull(ldt_orig_death_date) = FALSE AND IsNull(ldt_death_date) = TRUE THEN
		SELECT COUNT(*) 
		  INTO :ll_count 
		  FROM CLAIM_PARTICIPANT CP, 
				 X001_REGISTRATION XR 
		 WHERE CP.individual_no = :ll_individual_no 
		   AND CP.claim_role_code = "C" 
			AND CP.claim_no = XR.claim_no ; 

		SQLCA.nf_handle_error('n_individual',"", "nf_check_bus_rule - SELECT COUNT(*) FROM CLAIM_PARTICIPANT CP, X001_REGISTRATION XR")

		IF ll_count > 0 THEN
			MessageBox("Warning", "Individual's date of death is being removed and the individual has at least one claim that is registered for drug coverage", Information!)
		END IF
	END IF
	
	IF NOT IsNull(idw_dw[1].GetItemDateTime(ll_row,'death_date'))  THEN
		// check that death date after birth
		IF idw_dw[1].GetItemDateTime(ll_row,'birth_date') > idw_dw[1].GetItemDateTime(ll_row,'death_date') THEN
			MessageBox("Error", "Death date must be after birth date.")
			idw_dw[1].SetColumn('death_date')
			idw_dw[1].SetFocus()
			Return -1
		END IF

		IF idw_dw[1].GetItemDateTime(ll_row,'death_date') > f_server_datetime() THEN
			MessageBox("Error", "Death date must be less than today!")
			idw_dw[1].SetColumn('death_date')
			idw_dw[1].SetFocus()
			Return -1
		END IF
		
		SELECT MAX(O.accident_recurrence_date) 
		  INTO :ldt_max_accident_recurrence_date 
		  FROM OPENING O, 
				 CLAIM C, 
				 CLAIM_PARTICIPANT CP 
		 WHERE O.claim_no = C.claim_no 
			AND C.claim_no = CP.claim_no 
			AND CP.claim_role_code = "C" 
			AND C.individual_no = :ll_individual_no ;
			
		SQLCA.nf_handle_error('n_individual',"", "nf_check_bus_rule - SELECT MAX(O.accident_recurrence_date) FROM OPENING O, CLAIM C, CLAIM_PARTICIPANT CP")
		
		SELECT MAX(C.accident_date) 
		  INTO :ldt_max_accident_date 
		  FROM CLAIM C, 
		       INDIVIDUAL I, 
				 CLAIM_PARTICIPANT CP 
		 WHERE C.claim_no = CP.claim_no 
		   AND CP.claim_role_code = "C" 
		   AND I.individual_no = CP.individual_no 
		   AND I.individual_no = :ll_individual_no ; 
			
		SQLCA.nf_handle_error('n_individual',"", "nf_check_bus_rule - SELECT MAX(C.accident_date) FROM CLAIM C, INDIVIDUAL I, CLAIM_PARTICIPANT CP")

		IF IsNull(ldt_max_accident_recurrence_date) = FALSE THEN
			IF IsNull(ldt_max_accident_date) = TRUE THEN
				ldt_max_accident_date = ldt_max_accident_recurrence_date
			ELSE
				IF ldt_max_accident_recurrence_date > ldt_max_accident_date THEN
					ldt_max_accident_date = ldt_max_accident_recurrence_date
				END IF
			END IF
		END IF

		IF NOT IsNull(ldt_max_accident_date) THEN
			IF ldt_max_accident_date > idw_dw[1].GetItemDateTime(ll_row,'death_date') THEN
				MessageBox("Error", "Death date must be after most recent accident date/recurrence date for all claims!")
				idw_dw[1].SetColumn('death_date')
				idw_dw[1].SetFocus()
				Return -1
			END IF
		END IF
	END IF

	IF idw_dw[1].GetItemString(ll_row,'sex') <> 'M' AND idw_dw[1].GetItemString(ll_row,'sex') <> 'F' AND idw_dw[1].GetItemString(ll_row,'sex') <> 'U' THEN
		MessageBox('Error', 'Invalid sex code specified.')
		idw_dw[1].SetColumn('sex')
		idw_dw[1].SetFocus()
		Return -1
	END IF

	// The sex of an individual must not go from Male or Female to Not Specified
	ls_orig_sex = idw_dw[1].GetItemString(ll_row,'sex', Primary!, TRUE)
	ls_sex = idw_dw[1].GetItemString(ll_row, 'sex', Primary!, FALSE)
	IF (ls_orig_sex = "M" OR ls_orig_sex = "F") AND ls_sex = "U" THEN
		MessageBox('Error', 'The sex of an individual must not go from Male or Female to Not Specified.')
		idw_dw[1].SetItem(ll_row, 'sex', ls_orig_sex)
		idw_dw[1].SetColumn('sex')
		idw_dw[1].SetFocus()
		Return -1
	END IF

	IF idw_dw[1].GetItemString(ll_row,'language_code') <> 'E' AND idw_dw[1].GetItemString(ll_row,'language_code') <> 'F' AND idw_dw[1].GetItemString(ll_row,'language_code') <> 'U' THEN
		MessageBox('Error', 'Invalid language code specified.')
		idw_dw[1].SetColumn('language_code')
		idw_dw[1].SetFocus()
		Return -1
	END IF

	IF inv_address.nf_check_bus_rule() < 0 THEN
		Return -1
	END IF
	
	// The address lines must contain atleast 2 alphabetic characters when they are non blank
	ls_address_line1 = Trim(idw_dw[1].GetItemString(ll_row, 'address_line1'))
	IF ls_address_line1 <> "" AND IsNull(ls_address_line1) = FALSE AND Len(ls_address_line1) = 1 THEN
		MessageBox('Error', 'Address line 1 must contain at least 2 alphabetic characters when they are non blank.')
		idw_dw[1].SetColumn('address_line1')
		idw_dw[1].SetFocus()
		Return -1
	END IF

	ls_address_line2 = Trim(idw_dw[1].GetItemString(ll_row, 'address_line2'))
	IF ls_address_line2 <> "" AND IsNull(ls_address_line2) = FALSE AND Len(ls_address_line2) = 1 THEN
		MessageBox('Error', 'Address line 2 must contain at least 2 alphabetic characters when they are non blank.')
		idw_dw[1].SetColumn('address_line2')
		idw_dw[1].SetFocus()
		Return -1
	END IF

	// set the history flag to No since the record is now verified and is no longer history
	idw_dw[1].SetItem(ll_row,'history_flag', 'N')

	//	bank no
	ls_string = Trim(idw_dw[1].GetItemString(ll_row,'bank_no'))
	IF ls_string > '' THEN  
		// need to check to see if leading zeros are needed
		ll_no = Len(ls_string)
		IF ll_no < 3 THEN
			ll_no = 3 - ll_no
			ls_string2 = Fill('0',ll_no)
			ls_string = ls_string2 + ls_string
			idw_dw[1].SetItem(ll_row,'bank_no', ls_string)
		END IF
		IF ls_string = '000' THEN
			idw_dw[1].SetItem(ll_row,'bank_no', '')
		END IF
	END IF
	
	// bank transit no
	ls_string = Trim(idw_dw[1].GetItemString(ll_row,'bank_transit_no'))
	IF ls_string > '' THEN
		// need to check to see if leading zeros are needed
		ll_no = Len(ls_string)
		IF ll_no < 5 THEN
			ll_no = 5 - ll_no
			ls_string2 = Fill('0',ll_no)
			ls_string = ls_string2 + ls_string
			idw_dw[1].SetItem(ll_row,'bank_transit_no', ls_string)
		END IF
		IF ls_string = '00000' THEN 
			idw_dw[1].SetItem(ll_row,'bank_transit_no', '')
		END IF
	END IF
	
	//Jill M. PR3479 - July 9/03 - Check each individual for bank info, payments/awards
	IF w_frame.GetActiveSheet() = w_dup_fix_individual THEN
		IF (idw_dw[1].GetItemString(ll_row,'bank_no') = '0' OR idw_dw[1].GetItemString(ll_row,'bank_no') = '' OR &
			LONG(idw_dw[1].GetItemString(ll_row,'bank_no')) = 0) THEN
			FOR ll_cntr = 1 to w_dup_fix_individual.dw_individual_list.RowCount()
				ll_indiv_no = w_dup_fix_individual.dw_individual_list.GetItemNumber(ll_cntr,'individual_no')
				ls_bank_info = w_dup_fix_individual.dw_individual_list.GetItemString(ll_cntr,'compute_4')
				IF ls_bank_info > '--' THEN
					ll_bank_info = ll_bank_info + 1
				END IF
				
				// This function checks to see how many Awards(if any) are greater than the period_from_date for the  
				// earliest NULL processed_date in PERIODIC_AWARD_CONTROL for each award.  If there is one or more, 
				// users cannot remove bank information for that individual until the Award has been ended(with an 
				// end date the same as the period_from_date for the earliest NULL processed_date)
				IF nf_check_payments_and_awards(ll_indiv_no,ls_award_type,ls_message) < 0 THEN
					ll_pmts = ll_pmts + 1
					ls_pmts = ls_pmts + 'Individual #: ' + string(ll_indiv_no) + " has Award Types: " + ls_award_type + '~r~n'
				END IF
			NEXT
			IF ll_pmts >= ll_bank_info and ll_pmts > 0 THEN
				IF ls_award_type = 'AWARD' THEN
					messagebox('Missing Bank Information','Atleast one of the Individuals has banking information.~r~n' + ls_message , information!) 
					Return -1
				ELSE
					messagebox('Missing Bank Information','Atleast one of the Individuals has banking information.~r~nBank Information is required unless the payment method is ~r~nreplaced with Automated Cheque. ~r~n~r~n', information!) 
					Return -1
				END IF
			ELSEIF ll_bank_info > 0 and ll_pmts = 0 THEN
				IF messagebox('Warning!','Atleast one of the Individuals has banking information.~r~nWould you like to Continue? ',question!,yesno!) = 2 THEN
					Return -1
				END IF	
			END IF
		END IF
	ELSE
		IF idw_dw[1].GetItemString(ll_row,'bank_no',Primary!,TRUE) > '' AND &
		(idw_dw[1].GetItemString(ll_row,'bank_no') = '0' OR idw_dw[1].GetItemString(ll_row,'bank_no') = '' OR &
		LONG(idw_dw[1].GetItemString(ll_row,'bank_no')) = 0) THEN
			IF NOT IsNull(idw_dw[1].GetItemNumber(ll_row,'individual_no')) THEN
				// This function checks to see how many Awards(if any) are greater than the period_from_date for the  
				// earliest NULL processed_date in PERIODIC_AWARD_CONTROL for each award.  If there is one or more, 
				// users cannot remove bank information for that individual until the Award has been ended(with an 
				// end date the same as the period_from_date for the earliest NULL processed_date)*/
				IF nf_check_payments(idw_dw[1].GetItemNumber(ll_row,'individual_no'),TRUE) < 0 THEN
					Return -1
				END IF
			END IF
		END IF	
	END IF	
END IF

IF w_frame.GetActiveSheet() = w_dup_fix_individual THEN	
	IF nf_validate_birth_death_date("BIRTH") = -1 THEN 
		MessageBox("Error", "Birth date must be prior to earliest accident date for all claims!")
		idw_dw[1].SetColumn('birth_date')
		idw_dw[1].SetFocus()
		RETURN -1
	END IF 
	
	IF nf_validate_birth_death_date("DEATH") = -1 THEN 
		MessageBox("Error", "Death date must be after most recent accident date/recurrence date for all claims!")
		idw_dw[1].SetColumn('death_date')
		idw_dw[1].SetFocus()
		RETURN -1
	END IF 
END IF

IF nf_validate_claim_cost_alloc() <> 1 Then
	RETURN -1
END IF

IF idw_dw[1].DeletedCount() > 0 THEN
	//	check to make sure delete allowed and delete from the other datawindows
	// just delete no need to make checks - check will be done in another place.  The individual cannot
	// be deleted from the individual maintenance.  If it is deleted then it came from Claim Participant
	// Claim Participant will check to make sure the delete of the individual is allowed 
	idw_dw[2].DeleteRow(1)
	idw_dw[3].DeleteRow(1)    
END IF

// P10236 Epay - This Function DOES UPDATES!!!  Keep it after the rule checking.
// Determines if the death date has been set and updates the coverage accordingly
IF w_frame.GetActiveSheet() <> w_dup_fix_individual THEN
	is_x001_override_elig_delete = ""
	IF idw_dw[1].GetItemStatus(ll_row, 'death_date', Primary!) <> NotModified! THEN
		If  ll_individual_no > 0 Then    // PR9519
			li_rtn = nf_update_eligibility_coverage(ll_individual_no)
			IF li_rtn < 0 THEN
				RETURN -1
			END IF
		END IF 
	END IF 
	
	// P10237 Annuities - This Function DOES UPDATES!!!  Keep it after the rule checking.
	/*P10273 could be multiple annuity accounts for individual - check function for description and directions*/
	IF nf_modify_create_annuity_eligibility(1, DATE(ldt_birth_date), DATE(ldt_death_date), ll_individual_no) = -1 THEN 
		RETURN -1// rollback done inside function due to msgboxes
	END IF 
END IF

Return 0
end function

public function integer nf_update_eligibility_coverage (long al_individual_no);// function nf_update_eligibility_coverage. 
/* this function is called from the 'maintain individual' window's save button. It
	checks all claims associated with an individual who has had his or her death date set.
	The eligibility coverage must be updated according to the rules set out in the BR document for E-Pay, Phase 2.  
*/

N_BR_BLUECROSS lnv_br_bluecross

datetime ldt_death_date,ldt_next_bluecross_processing_day
long ll_row, ll_claim_no,ll_count_update
string ls_sql, ls_syntax, ls_event_comment
string error_syntaxfromSQL, error_create
date ldt_today 
u_ds lds_claims

lnv_br_bluecross = create N_BR_BLUECROSS

ll_row = idw_dw[1].getRow()

if not al_individual_no > 0 THEN
	return -1
END IF

ldt_death_date = idw_dw[1].GetItemDateTime(ll_row, "death_date")
IF NOT IsNull(ldt_death_date)  THEN	
	ldt_today = DATE(f_server_datetime())
	
	// get a list of registered claims associated with this individual. All claims must be checked
	ls_sql = 'select claim_no from CLAIM where individual_no = ' + String(al_individual_no)			 
	ls_syntax = SQLCA.SyntaxFromSQL(ls_sql, "", error_syntaxfromSQL)

	IF Len(error_syntaxfromSQL) > 0 THEN
			// Display errors					
		MessageBox("Syntax Error", "unable to process eligibility update~rErrorCode: "+error_syntaxfromSQL)
		return -1
	ELSE
		// Generate new DataWindow object for the u_ds
		lds_claims = create u_ds			
		lds_claims.Create(ls_syntax, error_create)
			
		IF Len(error_create) > 0 THEN
			MessageBox("Syntax Error", "unable to process eligibility update~rError Code: "+error_create)
			return -1
		END IF
	END IF		
	lds_claims.settransobject(SQLCA)
	lds_claims.Retrieve()
	SQLCA.nf_handle_error('n_individual','nf_update_eligibility_coverage','lds_claims.Retrieve()')
	
	// get the date to be used for termination date. It is the next bluecross processing date
	ldt_next_bluecross_processing_day = lnv_br_bluecross.uf_get_bluecross_next_day()

	FOR ll_row = 1 to lds_claims.rowcount()	
		ll_claim_no = lds_claims.getItemNumber(ll_row, "claim_no")
						
		// check the claim is registered, it must be a registered claim as per BR's, before updating
		IF lnv_br_bluecross.uf_is_registered(ll_claim_no) = 1 THEN		
			/* now check the eligibility end date for each claim associated with individual: 
				(there could be more than one record in the CLAIM_ELIGIBILITY table for this claim, each with different eligibility_end_dates)
				conditions: if an eligibility end date is null, use that record as 'current'
				else, get the record with maximum eligibilty_end_date. 
			*/	
			
			/* run the stored procedure for the eligibility update */
			IF lnv_br_bluecross.nf_create_eligibility_history(ll_claim_no) < 1 THEN RETURN -1
			
			UPDATE CLAIM_ELIGIBILITY
			   SET eligibility_end_date = :ldt_next_bluecross_processing_day, 
				    comment              = 'Death of the Individual', 
					 manual_entry_flag    = "N"
			 WHERE eligibility_record_no in(
			 SELECT  distinct 
 				CASE WHEN EXISTS(SELECT b.eligibility_record_no FROM CLAIM_ELIGIBILITY b 
                              WHERE b.eligibility_record_no = a.eligibility_record_no                                       
                                AND b.eligibility_end_date IS NULL )
                       THEN (SELECT c.eligibility_record_no FROM CLAIM_ELIGIBILITY c 
                              WHERE c.eligibility_record_no = a.eligibility_record_no                                      
                                AND c.eligibility_end_date IS NULL)
                       ELSE (SELECT d.eligibility_record_no FROM CLAIM_ELIGIBILITY d 
                              WHERE d.claim_no = a.claim_no
                                AND d.eligibility_end_date = 
                                   (SELECT max(eligibility_end_date)
                                      FROM CLAIM_ELIGIBILITY e
                                     WHERE e.claim_no = a.claim_no )) 
             END
                     
			 FROM CLAIM_ELIGIBILITY a
			WHERE a.claim_no = :ll_claim_no
			  AND (a.eligibility_end_date is null or a.eligibility_end_date > :ldt_today));

			ll_count_update = SQLCA.SQLNRows
			SQLCA.nf_handle_error('n_individual','nf_update_eligibility_coverage','UPDATE eligibility_end_date') 
		 	
			IF ll_count_update > 0 THEN
			   
				//Log an event for an Eligibility date change
				ls_event_comment = 'The Eligibility End Date has changed to '+STRING(ldt_next_bluecross_processing_day,"yyyy-mm-dd")+'.'
				IF lnv_br_bluecross.uf_log_auto_event(ll_claim_no,'035','',ls_event_comment) < 0 THEN RETURN -1
			END IF 
			
			/* run the stored procedure for the formulary update */
			IF lnv_br_bluecross.nf_create_formulary_history(ll_claim_no) < 1 THEN RETURN -1
			
			// now do the same for claim formulary table
			// BR 6.20 set termination date = next bluecross processing day when formulary effective date 
			// is less than or equal to the next bluecross processing day
			UPDATE CLAIM_FORMULARY
			   SET formulary_end_date = :ldt_next_bluecross_processing_day, 
				    comment            = 'Death of the Individual', 
					 manual_entry_flag  = "N"
			 WHERE formulary_record_no in(
					 SELECT 
				          CASE WHEN EXISTS(SELECT b.formulary_record_no FROM CLAIM_FORMULARY b 
                                       WHERE b.claim_no = a.claim_no                                                                       
                                       AND b.formulary_end_date IS NULL )
                                       
                            THEN (SELECT c.formulary_record_no FROM CLAIM_FORMULARY c 
                                       WHERE c.claim_no = a.claim_no                                        
                                       AND c.formulary_end_date IS NULL
                                       AND c.formulary_record_no = a.formulary_record_no )
                                       
                            ELSE (SELECT d.formulary_record_no FROM CLAIM_FORMULARY d 
                                       WHERE d.claim_no = a.claim_no                                       
                                       AND d.formulary_end_date = a.formulary_end_date
                                       AND d.formulary_record_no = a.formulary_record_no )                                        
                       END
							 
			  FROM CLAIM_FORMULARY a
		  	 WHERE a.claim_no            = :ll_claim_no
				AND a.formulary_type_code = 'P' 
  			 	AND a.primary_active_flag = 'Y' 
				AND a.formulary_start_date <= :ldt_next_bluecross_processing_day
				AND (a.formulary_end_date IS NULL OR a.formulary_end_date > :ldt_today)) ;
		
			ll_count_update = SQLCA.SQLNRows
			SQLCA.nf_handle_error('n_individual','nf_update_eligibility_coverage','UPDATE primary formulary_end_date') 

			// BR 6.30 set termination date = formulary effective date when effective date is greater than  
			// the next bluecross processing day
			UPDATE CLAIM_FORMULARY
			   SET formulary_end_date = formulary_start_date, 
				    comment            = 'Death of the Individual',
					 manual_entry_flag  = "N"
			 WHERE formulary_record_no in(
					 SELECT 
				         CASE WHEN EXISTS(SELECT b.formulary_record_no FROM CLAIM_FORMULARY b 
                                       WHERE b.claim_no = a.claim_no                                                                       
                                       AND b.formulary_end_date IS NULL )
                                       
                            THEN (SELECT c.formulary_record_no FROM CLAIM_FORMULARY c 
                                       WHERE c.claim_no = a.claim_no                                        
                                       AND c.formulary_end_date IS NULL
                                       AND c.formulary_record_no = a.formulary_record_no )
                                       
                            ELSE (SELECT d.formulary_record_no FROM CLAIM_FORMULARY d 
                                       WHERE d.claim_no = a.claim_no                                       
                                       AND d.formulary_end_date = a.formulary_end_date
                                       AND d.formulary_record_no = a.formulary_record_no )                                        
                      END
							 
			  FROM CLAIM_FORMULARY a
		  	 WHERE a.claim_no            = :ll_claim_no
				AND a.formulary_type_code = 'P' 
  			 	AND a.primary_active_flag = 'Y' 
				AND a.formulary_start_date > :ldt_next_bluecross_processing_day 
  			   AND (a.formulary_end_date IS NULL OR a.formulary_end_date > :ldt_today)) ;
		
			ll_count_update = ll_count_update + SQLCA.SQLNRows
			SQLCA.nf_handle_error('n_individual','nf_update_eligibility_coverage','UPDATE primary formulary_end_date') 
				
			IF ll_count_update > 0 THEN
			 
				//Log an event for an Formulary date change
				ls_event_comment = 'The Primary Formulary End Date has changed to '+STRING(ldt_next_bluecross_processing_day,"yyyy-mm-dd")+'.'
				IF lnv_br_bluecross.uf_log_auto_event(ll_claim_no,'036','',ls_event_comment) < 0 THEN RETURN -1
			END IF 				

			// now do all the Secondary formularies
			// BR 6.20 set termination date = next bluecross processing day when formulary effective date 
			// is less than or equal to the next bluecross processing day
			UPDATE CLAIM_FORMULARY
				SET formulary_end_date = :ldt_next_bluecross_processing_day, 
				    comment            = 'Death of the Individual',
					 manual_entry_flag  = "N"
			 WHERE formulary_record_no IN (
      		SELECT formulary_record_no 
			  	  FROM CLAIM_FORMULARY 
      	 	 WHERE claim_no            = :ll_claim_no
      	   	AND formulary_type_code = 'S' 
					AND formulary_start_date <= :ldt_next_bluecross_processing_day
      			AND (formulary_end_date IS NULL OR formulary_end_date > :ldt_today)) ;

			ll_count_update = SQLCA.SQLNRows
			SQLCA.nf_handle_error('n_individual','nf_update_eligibility_coverage','UPDATE secondary formulary_end_date') 

			// BR 6.30 set termination date = formulary effective date when effective date is greater than  
			// the next bluecross processing day
			UPDATE CLAIM_FORMULARY
				SET formulary_end_date = formulary_start_date, 
				    comment            = 'Death of the Individual' ,
					 manual_entry_flag  = "N"
			 WHERE formulary_record_no IN (
      		SELECT formulary_record_no 
			  	  FROM CLAIM_FORMULARY 
      	 	 WHERE claim_no            = :ll_claim_no
      	   	AND formulary_type_code = 'S' 
					AND formulary_start_date > :ldt_next_bluecross_processing_day 
      			AND (formulary_end_date IS NULL OR formulary_end_date > :ldt_today)) ;

			ll_count_update = ll_count_update + SQLCA.SQLNRows
			SQLCA.nf_handle_error('n_individual','nf_update_eligibility_coverage','UPDATE secondary formulary_end_date') 

			IF ll_count_update > 0 THEN
			   		
				//Log an event for an Formulary date change
				ls_event_comment = 'The Secondary Formulary End Date(s) have changed to '+STRING(ldt_next_bluecross_processing_day, "yyyy-mm-dd")+'.'
				IF lnv_br_bluecross.uf_log_auto_event(ll_claim_no,'036','',ls_event_comment) < 0 THEN RETURN -1
			END IF 				

		END IF
		
		// now check all claims , registered or not, for entry in X001_OVERRIDE_ELIGIBILITY, delete if it is unprocessed
			DELETE FROM X001_OVERRIDE_ELIGIBILITY
			WHERE (claim_no = :ll_claim_no AND record_no = 0)
			USING SQLCA;
			SQLCA.nf_handle_error('n_individual','nf_update_eligibility_coverage','DELETE FROM X001_OVERRIDE_ELIGIBILITY') 
				
			IF SQLCA.sqlnrows > 0 THEN
				IF TRIM(is_x001_override_elig_delete) = "" THEN
					is_x001_override_elig_delete = "A Claim(s) related to this individual has an entry in the manual override termination table. This/these will be deleted."
				END IF 
			END IF

	END FOR
END IF

return 0
end function

public function integer nf_check_mandatory ();LONG  ll_row, ll_count, ll_individual_no
DATETIME ldtm_death_date
// accept the data windows
	
	IF idw_dw[1].AcceptText() < 0 THEN Return -1
	IF idw_dw[2].AcceptText() < 0 THEN Return -1
	IF idw_dw[3].AcceptText() < 0 THEN Return -1
	IF idw_dw[4].AcceptText() < 0 THEN Return -1
/*	Trim all the fields
*/

	idw_dw[2].SetItem(1,'last_name', Trim(idw_dw[2].GetItemString(1,'last_name')))
	idw_dw[2].SetItem(1,'given_names', Trim(idw_dw[2].GetItemString(1,'given_names')))


	IF IsNull(idw_dw[2].GetItemString(1,'last_name')) OR Trim(idw_dw[2].GetItemString(1,'last_name')) = '' THEN
   	MessageBox("Missing Name", "The last name of the individual must be supplied.  Please enter.")
		idw_dw[2].SetColumn('last_name')
		idw_dw[2].SetFocus()
	   Return -1
	END IF
	IF IsNull(idw_dw[2].GetItemString(1,'given_names')) OR idw_dw[2].GetItemString(1,'given_names') = ''THEN
   	MessageBox("Missing Name", "The given names of the individual must be supplied.  Please enter.")
		idw_dw[2].SetColumn('given_names')
		idw_dw[2].SetFocus()
	   Return -1
	END IF

	IF IsNull(idw_dw[2].GetItemString(1,'name_type_code')) THEN
   	idw_dw[2].SetItem(1,'name_type_code', 'M')
	END IF

	ll_row = idw_dw[1].GetRow()
	IF ll_row > 0 THEN
		idw_dw[1].SetItem(1,'individual_name_last_name', Trim(idw_dw[1].GetItemString(1,'individual_name_last_name')))
		idw_dw[1].SetItem(1,'individual_name_given_names', Trim(idw_dw[1].GetItemString(1,'individual_name_given_names')))
		ll_individual_no = idw_dw[1].GetItemNumber(1,'individual_no')
		IF IsNull(idw_dw[1].GetItemDateTime(1,'death_date')) AND NOT IsNull(idw_dw[1].GetItemNumber(1,'individual_no')) THEN
			IF (IsNull(idw_dw[1].GetItemDateTime(1,'death_date', Primary!, TRUE)) AND NOT IsNull(idtm_death_date)) THEN
				ldtm_death_date = idtm_death_date
				
				IF IsNull(ll_individual_no) OR ll_individual_no = 0 THEN 
					ll_individual_no = il_individual_no
				END IF
				
			ELSEIF NOT IsNull(idw_dw[1].GetItemDateTime(1,'death_date', Primary!, TRUE)) THEN
				ldtm_death_date = idw_dw[1].GetItemDateTime(1,'death_date', Primary!, TRUE)
			END IF
			IF NOT IsNull(ldtm_death_date) THEN
/*				check that the individual is not a claimant on a fatality claim
*/				
				SELECT Count(*)
				  INTO :ll_count
				  FROM CLAIM_PARTICIPANT, CLAIM, DIFFICULT_CLAIM
				 WHERE CLAIM_PARTICIPANT.claim_no = CLAIM.claim_no
			   	AND CLAIM_PARTICIPANT.claim_role_code = 'C'
					AND CLAIM_PARTICIPANT.individual_no = :ll_individual_no
						AND CLAIM.claim_no = DIFFICULT_CLAIM.claim_no
					AND DIFFICULT_CLAIM.fatality_flag = 'Y'
				 USING SQLCA;

				IF SQLCA.nf_handle_error('Embedded SQL: select from CLAIM_PARTICIPANT, CLAIM, DIFFICULT_CLAIM', 'n_individual', 'nf_check_madatory') < 0 THEN
					Return -1
				END IF
				IF ll_count > 0 THEN
					MessageBox('Invalid Death Date', 'The death date cannot be removed because the individual is a claimant on a fatality claim.')
					Return -1
				END IF
			END IF
		END IF

		IF Len(idw_dw[1].GetItemString(ll_row, 'bank_transit_no')) > 1 THEN
			idw_dw[1].SetItem(ll_row,'bank_transit_no', Trim(idw_dw[1].GetItemString(ll_row, 'bank_transit_no')))
		END IF
		IF Len(idw_dw[1].GetItemString(ll_row, 'bank_account_no')) > 1 THEN
			idw_dw[1].SetItem(ll_row,'bank_account_no', Trim(idw_dw[1].GetItemString(ll_row, 'bank_account_no')))
		END IF
   // if some banking info there then all must be there

	   IF IsNull(idw_dw[1].GetItemString(ll_row,'bank_no')) OR Trim(idw_dw[1].GetItemString(ll_row,'bank_no')) = '' OR Integer(idw_dw[1].GetItemString(ll_row,'bank_no')) = 0 THEN
   	   IF NOT(IsNull(idw_dw[1].GetItemString(ll_row,'bank_transit_no')) OR Trim(idw_dw[1].GetItemString(ll_row,'bank_transit_no')) = '' OR Integer(idw_dw[1].GetItemString(ll_row,'bank_transit_no')) = 0) OR &
	         NOT(IsNull(idw_dw[1].GetItemString(ll_row,'bank_account_no')) OR Trim(idw_dw[1].GetItemString(ll_row,'bank_account_no')) = '') THEN
   	      MessageBox('Missing Banking Information', 'If any banking information is supplied then it all must be supplied.')
				idw_dw[1].SetColumn('bank_no')
				idw_dw[1].SetFocus()
      	   Return -1
	      END IF
   	ELSE
      	IF (IsNull(idw_dw[1].GetItemString(ll_row,'bank_transit_no')) OR Trim(idw_dw[1].GetItemString(ll_row,'bank_transit_no')) = '' OR Integer(idw_dw[1].GetItemString(ll_row,'bank_transit_no')) = 0) OR &
	         (IsNull(idw_dw[1].GetItemString(ll_row,'bank_account_no')) OR Trim(idw_dw[1].GetItemString(ll_row,'bank_account_no')) = '') THEN
   	      MessageBox('Missing Banking Information', 'If any banking information is supplied then it all must be supplied.')
				idw_dw[1].SetColumn('bank_no')
				idw_dw[1].SetFocus()
      	   Return -1
	      END IF
   
   	END IF

		IF inv_address.nf_check_mandatory() < 0 THEN Return -1

	END IF

Return 0
end function

public function long nf_get_annuity_account_no (long al_individual_no, long al_claim_no, string as_claim_role_code);LONG ll_annuity_account_no

CHOOSE CASE as_claim_role_code
	CASE 'C'
		
		al_claim_no = 0
		
	CASE 'SS'	
END CHOOSE

//this assumes we don't have an account number TBD
SELECT 	annuity_account_no 
INTO 		:ll_annuity_account_no 
FROM 	ANNUITY_ACCOUNT
WHERE 	individual_no 		= :al_individual_no 
AND 		claim_no 			= :al_claim_no
AND 		claim_role_code 	= :as_claim_role_code
USING    SQLCA;

SQLCA.nf_handle_error('n_individual','nf_get_annuity_account_no()','SELECT annuity_account_no ')

IF ISNULL(ll_annuity_account_no) OR ll_annuity_account_no < 0 THEN
	ll_annuity_account_no = 0
END IF 

RETURN ll_annuity_account_no


end function

public subroutine nf_set_claim_role_code (string as_claim_role_code);
is_claim_role_code = as_claim_role_code
end subroutine

public function integer nf_modify_create_annuity_eligibility (integer ai_case, date adt_birth, date adt_death, long al_individual_no);/*
note: THIS IS THE ORIGINAL - UNCHANGED
A change to a birth date for an injured worker or surviving spouse who is eligible for annuity benefits, 
where that change alters the value of the month or year of the birth date, requires that the annuity eligibility end date be ‘changed’.  
As Annuity Eligibility data is NOT overwritten, but rather made inactive and a new records created, 
a change of birth date year or month will require the following data updates:
o	The existing active ANNUITY_ELIBILITY record for the injured worker will be set to be inactive
o	A new ANNUITY _ELIGIBILITY record for the injured worker will be created with the same start date, corrected end date 
(1st of the month following their 65th birthday) and a reason code of ‘BDC’ for ‘Birth Date change’

An addition or change to a death date for an injured worker or surviving spouse who is eligible for annuity benefits, 
where the individual has not yet turned 65 years of age, requires that the annuity eligibility end date be ‘changed’. 
As Annuity Eligibility data is NOT overwritten, but rather made inactive and a new records created, a change of death date year or month 
will require the following data updates:
o	The existing active ANNUITY_ELIBILITY record for the individual will be set to be inactive
o	A new ANNUITY _ELIGIBILITY record for the individual will be created with the same start date, corrected end date 
(1st of the month following their death date) and a reason code of ‘DDC’ for ‘Death Date change’
o	Take the confirm date too?  Hopefully code is doing that

2.1.3	Business Rules 

The following business rules will be added to the BR_CMWB_Maintain_Individual document:

2.250 If an active annuity eligibility exists for the annuity account, and the birthdate is changed causing the annuity end date to be set in the future, 
and scheduled payments/awards exists after the new annuity end date, a message is displayed indicating the payments should be reviewed.

2.260 If an active annuity eligibility exists for the annuity account, and the birthdate is changed causing the annuity end date to be set in the past, 
and payments/awards exists after the new annuity end date, a message is displayed indicating the claims/payments should be reviewed.

2.270	If an active annuity eligibility exists for the annuity account, and the birth date is changed, the annuity eligibility end date must not be prior 
to the annuity eligibility start date.

2.280	If an active annuity eligibility exists for the annuity account, and the death date is changed, the annuity eligibility end date must not be prior 
to the annuity eligibility start date.	
** renumbered to ** 
2.275	The death date of the individual must not be changed if active annuity eligibility exists for the annuity account of the individual 
and the death date will cause the annuity eligibility end date to be on or prior to the annuity eligibility start date. 
(Note: There may be more than one annuity account for the individual)

note: function does update and Insert

RETURNS:
1 	- update/insert completed
0 	- no action required
-1 	- Error

NEW: 20090811
set aside percent no and percentage must be obtained from the Set_Aside_Percentage look-up table 
(Kevin has code that can be shared, if not accessed).  This happens because the method of determining 5% vs 10% is based 
on the end date value being 2009-01-01 or greater and the BDC/DDC change the end date (new)

NEW: 20090812
3.60		The annuity eligibility must end on the last day of the month in which the individual turns sixty-five years of age.
3.70		The annuity eligibility must end on the last day of the month in which the individual dies if death occurs before age sixty-five.

*/

LONG     ll_annuity_eligibility_no, ll_annuity_account_no, ll_confirm_annuity_elig_checklist_no
LONG     ll_year, ll_month, ll_day, ll_no,  ll_verify_ben_entitlement_checklist_no
STRING   ls_annuity_eligibility_status_code, ls_benefit_option_code, ls_annuity_eligibility_reason_code, ls_annuity_eligibility_comment
STRING   ls_pre_1993, ls_converted_flag, as_type, ls_message, ls_claim_role_desc, ls_claim_role, ls_confirmed_by_user_id
DATETIME ldtm_annuity_start_date, ldtm_annuity_end_date, ldtm_birth_check, ldtm_death_check, ldtm_new_annuity
DATETIME	ldtm_annuity_end_date_calculated, ldtm_confirmed_date, adtm_annuity_end_date
DATETIME	ldtm_annuity_eligibility_end_date_changed, ldtm_annuity_eligibility_end_date_used
DECIMAL  ldec_annuity_set_aside_percent
INTEGER  li_cnt, li_counter, li_annuity_set_aside_percent_no
BOOLEAN  lb_check_birth, lb_check_death
DATE     ldt_calculated

DATASTORE lds_annuity

lds_annuity 					= CREATE DATASTORE
lds_annuity.DataObject 	= 'd_annuity_eligibility'
lds_annuity.SetTransObject(SQLCA)
li_cnt = lds_annuity.Retrieve(al_individual_no)
SQLCA.nf_handle_error('n_individual', 'nf_modify_create_annuity_eligibility()', 'li_cnt = lds_annuity.Retrieve(al_individual_no)')


n_common_annuity lnv_common_annuity
lnv_common_annuity = create n_common_annuity

//No Valid annuity so just return out
IF li_cnt <= 0  THEN RETURN 1

// Grab the Birth Date
SELECT 	 birth_date, death_date
INTO 		:ldtm_birth_check, :ldtm_death_check
FROM 	INDIVIDUAL
WHERE 	individual_no = :al_individual_no
USING 	SQLCA;
	
SQLCA.nf_handle_error('n_individual', 'nf_modify_create_annuity_eligibility()', 'SELECT  birth_date')

// Determine what has been changed
IF DATE(ldtm_birth_check) 	<> adt_birth 						THEN lb_check_birth 	= TRUE
IF DATE(ldtm_death_check) <> adt_death 						THEN lb_check_death = TRUE
IF ISNULL(ldtm_death_check) AND NOT ISNULL(adt_death) 	THEN lb_check_death = TRUE
IF ISNULL(adt_death) AND NOT ISNULL(ldtm_death_check) 	THEN lb_check_death = TRUE
//IF isnull(adt_death) 													THEN RETURN 0

//if nothing changed return out
IF lb_check_birth = FALSE AND lb_check_death = FALSE THEN RETURN 0

//loop for each record
FOR li_counter = 1 TO li_cnt

	//grab the data we need from the datawindow
 	ldtm_annuity_start_date 				= lds_annuity.getitemdatetime(li_counter, "annuity_start_date")
	
 	ldtm_annuity_end_date 				= lds_annuity.getitemdatetime(li_counter, 'annuity_end_date')
   	ldec_annuity_set_aside_percent	= lds_annuity.getitemdecimal(li_counter, 'annuity_set_aside_percent')
	ls_benefit_option_code 				= lds_annuity.getitemstring(li_counter, 'benefit_option_code')
	ll_annuity_eligibility_no 				= lds_annuity.getitemnumber(li_counter, 'annuity_eligibility_no')
	ll_annuity_account_no 				= lds_annuity.getitemnumber(li_counter, 'annuity_account_no')
	ls_claim_role_desc                 		= lds_annuity.getitemstring(li_counter, 'claim_role_desc')
	ls_claim_role                 				= lds_annuity.getitemstring(li_counter, 'claim_role_code')
	
	//IF NO ANNUITY_ACCOUNT  return out.
	IF ll_annuity_account_no = 0 THEN RETURN 0
	
	IF lb_check_birth = TRUE THEN 
				
		/*If the person is dead then the annuity end date is the last date of the death date
		3.60		The annuity eligibility must end on the last day of the month in which the individual turns sixty-five years of age.
		3.70		The annuity eligibility must end on the last day of the month in which the individual dies if death occurs before age sixty-five.
		
		Only death before the age of 65 will impact Annuity dates
		If the change of the birth date means the person died after the age 65 the annuity end date becomes 
		the last of the month (got it right this time) when they turned 65.
		If the change of the birth date means they still died before reaching 65 there is no change required 
		(you can leave all annuity data as it is).
		*/
		
		// ldtm_annuity_end_date_calculated returned by 
		lnv_common_annuity.nf_get_annuity_end_date(al_individual_no, ls_claim_role_desc, ls_message, ldtm_annuity_end_date_calculated, ldtm_annuity_eligibility_end_date_changed, DATETIME(adt_birth), DATETIME(adt_death))
							
		IF ISNULL(ldtm_annuity_end_date_calculated) THEN RETURN 0
	
		IF DATE(ldtm_annuity_end_date_calculated) < DATE(ldtm_annuity_start_date) THEN 
			SQLCA.nf_rollback_transaction()
			MessageBox('Validation Error', 'The birth date has changed, the annuity eligibility end date must not be prior to the annuity eligibility start date!', Exclamation!)
			RETURN -1
		END IF 

		//  reason code of ‘BDC’ for ‘Birth Date change’
		ls_annuity_eligibility_reason_code = 'BDC'
		
	END IF 

	IF lb_check_death = TRUE THEN 
		
		// ldtm_annuity_end_date_calculated returned by reference
		lnv_common_annuity.nf_get_annuity_end_date(al_individual_no,ls_claim_role_desc , ls_message, ldtm_annuity_end_date_calculated, ldtm_annuity_eligibility_end_date_changed, DATETIME(adt_birth),DATETIME(adt_death))
		
		IF ISNULL(ldtm_annuity_end_date_calculated) THEN RETURN 0

		//  reason code of ‘DDC’ for ‘Death Date change’
		ls_annuity_eligibility_reason_code = 'DDC'
			
		IF DATE(ldtm_annuity_end_date_calculated) < DATE(ldtm_annuity_start_date) THEN 
			SQLCA.nf_rollback_transaction()
			MessageBox('Validation Error', 'The Death Date has changed, the annuity eligibility end date must not be prior to the annuity eligibility start date!', Exclamation!)
			RETURN -1
		END IF 
	END IF
	
	//Problem
	IF ISNULL(ldtm_annuity_end_date_calculated) OR DATE(ldtm_annuity_end_date_calculated) = date('1900-01-01') THEN RETURN -1

	SELECT annuity_account_no,
          annuity_eligibility_no,
          annuity_eligibility_status_code,
          confirmed_date,
          confirmed_by_user_id,
          annuity_start_date,
			 annuity_eligibility_end_date_used,
          annuity_end_date,
          benefit_option_code,
          annuity_eligibility_comment,
          confirm_annuity_eligibility_checklist_no,
          verify_benefit_entitlement_checklist_no,
          pre_1993_annuity_eligibility_flag,
          converted_flag
	INTO   :ll_annuity_account_no,
          :ll_annuity_eligibility_no,
          :ls_annuity_eligibility_status_code,
          :ldtm_confirmed_date,
          :ls_confirmed_by_user_id,
          :ldtm_annuity_start_date,
			 :ldtm_annuity_eligibility_end_date_used,
          :ldtm_annuity_end_date,
          :ls_benefit_option_code,
          :ls_annuity_eligibility_comment,
          :ll_confirm_annuity_elig_checklist_no,
          :ll_verify_ben_entitlement_checklist_no,
          :ls_pre_1993,
          :ls_converted_flag 
	FROM 	 ANNUITY_ELIGIBILITY
	WHERE  annuity_eligibility_status_code = 'A'
	AND    annuity_account_no              = :ll_annuity_account_no
	USING SQLCA;		
	SQLCA.nf_handle_error('n_individual', 'nf_modify_create_annuity_eligibility()', 'SELECT annuity_account_no - B')
	
	
	IF IsNull(ldtm_annuity_eligibility_end_date_changed) THEN ldtm_annuity_eligibility_end_date_changed = ldtm_annuity_eligibility_end_date_used
	
	// get the annuity pct for the role code, annuity end date, & benefit option
	lnv_common_annuity.nf_get_annuity_percentage(ls_claim_role, ldtm_annuity_end_date_calculated, ls_benefit_option_code, ldec_annuity_set_aside_percent , li_annuity_set_aside_percent_no)
	
	//SET ANNUITY_ELIBILITY TO inactive (what if it is pending?)
	UPDATE 	ANNUITY_ELIGIBILITY 
	SET  		annuity_eligibility_status_code 	= 'I' 
	WHERE 	annuity_eligibility_status_code 	= 'A'
	AND 		annuity_account_no 				= :ll_annuity_account_no
	USING 	SQLCA;
			
	SQLCA.nf_handle_error('n_individual', 'nf_modify_create_annuity_eligibility()', 'UPDATE 	ANNUITY_ELIGIBILITY')
	
	//create the new record based on the old values and new values as determined from the DD
	lnv_common_annuity.nf_insert_annuity_eligibility(ll_annuity_account_no, ls_annuity_eligibility_status_code, ldtm_confirmed_date, ls_confirmed_by_user_id, ldtm_annuity_start_date, ldtm_annuity_eligibility_end_date_changed, ldtm_annuity_end_date_calculated, ls_benefit_option_code, li_annuity_set_aside_percent_no, ldec_annuity_set_aside_percent,ls_annuity_eligibility_reason_code, ls_annuity_eligibility_comment, ll_confirm_annuity_elig_checklist_no, ll_verify_ben_entitlement_checklist_no, ls_pre_1993, 'N')
		
NEXT      
			
RETURN 1
end function

public function integer nf_check_benefit_payments (datetime adtm_annuity_end_date, ref boolean ab_past_pmts, long al_claim_no, long al_individual_no, string as_claim_role_code);LONG ll_awards, ll_pmts

IF ab_past_pmts THEN
	
	ab_past_pmts = FALSE
	
	SELECT Count(*)
	INTO   :ll_pmts
	FROM   PAYMENT a, 
			 Payment_Type d,
			 OPENING b, 
			 Opening_Type c
 	WHERE  a.claim_no 		   = b.claim_no
	AND    a.opening_no        = b.opening_no
	AND    b.opening_type_code = c.opening_type_code
	AND    a.payment_type_code = d.payment_type_code
	AND    a.paid_to_date      > :adtm_annuity_end_date
	AND    a.claim_no          in ( SELECT  	a.claim_no
										 FROM	CLAIM_PARTICIPANT a
										 JOIN     	CLAIM b on a.claim_no 	= b.claim_no 
										 WHERE	a.individual_no 				= :al_individual_no
										 AND 		a.claim_role_code 		= :as_claim_role_code)
	AND    a.processed_date is not null
 	AND    c.opening_type_code in('RLOE','LTD','PEN','SV')
	AND    d.annuity_flag      = 'Y'
	
	USING  SQLCA;
		
	SQLCA.nf_handle_error("n_individual","Embedded SQL: SELECT Count(*)","nf_check_benefit_payments")
			
	SELECT Count(*) 
	INTO   :ll_awards
	FROM   PERIODIC_AWARD a, 
			 PERIODIC_RECIPIENT b,		
			 OPENING d,
			 Opening_Type e,
			 Payment_Type f
	WHERE  a.claim_no            = b.claim_no	
 	AND    a.award_no            = b.award_no
 	AND    b.recipient_type_code = 'I'
	AND    a.award_end_date      > :adtm_annuity_end_date
	AND    a.claim_no              in ( SELECT a.claim_no
										 FROM	CLAIM_PARTICIPANT a
										 JOIN     	CLAIM b on a.claim_no 	= b.claim_no 
										 WHERE	a.individual_no 				= :al_individual_no
										 AND 		a.claim_role_code 		= :as_claim_role_code)
	AND    a.opening_no          = d.opening_no
	AND    a.claim_no            = d.claim_no
	AND    d.opening_type_code   = e.opening_type_code
	AND    a.payment_type_code   = f.payment_type_code
	AND    f.annuity_flag        = 'Y'
	AND    e.opening_type_code  in ('RLOE','LTD','PEN','SV')
	AND    a.award_end_date      < (SELECT Max(c.period_to_date)
									        FROM   PERIODIC_AWARD_CONTROL c
											  WHERE  a.award_type_code = c.award_type_code
											  AND    c.processed_date IS NOT NULL)
   USING  SQLCA;
	
	SQLCA.nf_handle_error("n_individual","Embedded SQL: SELECT Count(*)","nf_check_benefit_payments")

	IF ll_pmts > 0 OR ll_awards > 0 THEN
		ab_past_pmts = TRUE
		MessageBox('Warning: Processed Payments','Payments have been processed after the new Annuity End Date.~r~n' + &
		           'Please review the claim for possible Benefit or Annuity impact.',information!)
	END IF	
	
ELSE	
	
	ab_past_pmts = FALSE
	
	SELECT Count(*)
	INTO   :ll_pmts
	FROM   PAYMENT a, 
			 Payment_Type d,
			 OPENING b, 
			 Opening_Type c
 	WHERE  a.claim_no 		   = b.claim_no
	AND    a.opening_no        = b.opening_no
	AND    b.opening_type_code = c.opening_type_code
	AND    a.payment_type_code = d.payment_type_code
	AND    a.paid_to_date      > :adtm_annuity_end_date
	AND    a.claim_no           in ( SELECT a.claim_no
										 FROM	CLAIM_PARTICIPANT a
										 JOIN     	CLAIM b on a.claim_no 	= b.claim_no 
										 WHERE	a.individual_no 				= :al_individual_no
										 AND 		a.claim_role_code 		= :as_claim_role_code)
	AND    a.processed_date is null
 	AND    c.opening_type_code in('RLOE','LTD','PEN','SV')
	AND    d.annuity_flag      = 'Y'
	USING  SQLCA;
			
	SQLCA.nf_handle_error("n_individual","Embedded SQL: SELECT Count(*)","nf_check_benefit_payments")
	
	SELECT Count(*) 
	INTO   :ll_awards
	FROM   PERIODIC_AWARD a, 
			 PERIODIC_RECIPIENT b,		
			 OPENING d,
			 Opening_Type e,
			 Payment_Type f
	WHERE  a.claim_no            = b.claim_no	
 	AND    a.award_no            = b.award_no
 	AND    b.recipient_type_code = 'I'
	AND    a.award_end_date      > :adtm_annuity_end_date
	AND    a.claim_no            in ( SELECT a.claim_no
										 FROM	CLAIM_PARTICIPANT a
										 JOIN     	CLAIM b on a.claim_no 	= b.claim_no 
										 WHERE	a.individual_no 				= :al_individual_no
										 AND 		a.claim_role_code 		= :as_claim_role_code)
	AND    a.opening_no          = d.opening_no
	AND    a.claim_no            = d.claim_no
	AND    d.opening_type_code   = e.opening_type_code
	AND    a.payment_type_code   = f.payment_type_code
	AND    f.annuity_flag        = 'Y'
	AND    e.opening_type_code  in ('RLOE','LTD','PEN','SV')
	AND    a.award_end_date      > (SELECT Min(c.period_from_date)
									        FROM   PERIODIC_AWARD_CONTROL c
											  WHERE  a.award_type_code = c.award_type_code
											  AND    c.processed_date IS NULL)
   USING  SQLCA;
	
	SQLCA.nf_handle_error("n_individual","Embedded SQL: SELECT Count(*)","nf_check_benefit_payments")
		
	IF ll_pmts > 0 OR ll_awards > 0 THEN
		MessageBox('Warning: Scheduled Payments Exist','Scheduled payments exist after the Annuity End Date. The Birthdate and Annuity Dates should not~r~n' + &
					  'be changed until those payments are ended/removed.Please review.',information!)
	END IF
END IF

RETURN 0
end function

public function integer nf_validate_annuity_dates (datetime adtm_annuity_start_date, datetime adtm_annuity_end_date, long al_claim_no, long al_individual_no, decimal adec_pct, string as_claim_role);DATETIME  ldtm_date, ldtm_qualify_date

IF IsNull(al_claim_no) or IsNull(al_individual_no) THEN RETURN 0
		
IF as_claim_role = 'SS' OR as_claim_role = 'C' THEN
		
	SELECT 	accident_date
	INTO 		:ldtm_date
	FROM 	CLAIM
	WHERE 	claim_no = :al_claim_no
	USING 	SQLCA;
		 
	SQLCA.nf_handle_error('Embedded SQL: select from CLAIM', 'n_individual','nf_validate_annuity_dates') 
	
	IF ISNULL(ldtm_date) THEN RETURN 0 //huh? 
		
	IF as_claim_role = 'C' THEN		
		IF NOT IsNull(adtm_annuity_start_date) THEN
			/* PR6024 - J. Hawker, 2006-10-17 - Only validate annuity start date if it has been entered/modified and allow a date less than
			     1993-01-01 only if previous date was less than 1993-01-01
			*/
			IF adtm_annuity_start_date <>  idtm_old_annuity_start_date THEN
				IF DATE(adtm_annuity_start_date) < DATE('1993,01,01') THEN
					IF IsNull(idtm_old_annuity_start_date) OR DATE(idtm_old_annuity_start_date) >= DATE('1993-01-01')  THEN RETURN -1
				END IF
								
				/*PR4180 - J. Hawker - The Annuity Start Date must be the first day of the qualifying month. */
				ldtm_qualify_date = datetime(date(year(relativedate(date(ldtm_date),730)),month(relativedate(date(ldtm_date),730)),1))
				
				//IF start date is less than 2 years after accident date, return -1
				IF DATE(adtm_annuity_start_date) < DATE(ldtm_qualify_date) THEN RETURN -1
					
				//IF start date is greater than 2 years after the accident date and it isn't the first of the month, return -1 
				IF DATE(adtm_annuity_start_date) > DATE(ldtm_qualify_date) AND DAY(DATE(adtm_annuity_start_date)) <> 1 THEN RETURN -1
				
			END IF
		END IF				
	END IF 	
	
	// Comparison with accident date...how does this work with type 'C'???
	IF NOT IsNull(adtm_annuity_start_date) THEN
		IF DATE(adtm_annuity_start_date) < DATE(ldtm_date) THEN RETURN -1
		
		IF DATE(adtm_annuity_end_date) < DATE(adtm_annuity_start_date) THEN RETURN -1
	END IF		
END IF

RETURN 0

end function

public function integer nf_check_annuity_end_date_brs (integer ai_check_rule, long al_individual_no, date adt_birth_date, date adt_death_date, datetime adtm_ann_start_date, string as_claim_role);/*
(RULE - 1)
2.250 If an active annuity eligibility exists for the annuity account, and the birthdate is changed causing the annuity end date to be set in the future, 
and scheduled payments/awards exists after the new annuity end date, a message is displayed indicating the payments should be reviewed.

(RULE - 2)
2.260 If an active annuity eligibility exists for the annuity account, and the birthdate is changed causing the annuity end date to be set in the past, 
and payments/awards exists after the new annuity end date, a message is displayed indicating the claims/payments should be reviewed.

(RULE - 3)
2.270	If an active annuity eligibility exists for the annuity account, and the birth date is changed, the annuity eligibility end date must not be prior to the 
annuity eligibility start date.

(RULE - 4)
2.280	If an active annuity eligibility exists for the annuity account, and the death date is changed, the annuity eligibility end date must not be prior to the
annuity eligibility start date.	

------- NOTE to james
2.275	The death date of the individual must not be changed if active annuity eligibility exists for 
         the annuity account of the individual and the death date will cause the annuity eligibility end date to be on or prior 
		to the annuity eligibility start date.  (Note: There may be more than one annuity account for the individual)

RETURN
1  		- NO PROBLEM
-1  	- VALIDATION ERROR
*/
DATETIME	   ldtm_annuity_end_date_calculated, ldtm_birth_check, ldtm_death_check, ldtm_annuity_eligibility_end_date_used
STRING		ls_message

n_common_annuity	lnv_common_annuity
lnv_common_annuity = create n_common_annuity

IF IsNull(al_individual_no) THEN RETURN 1

// Grab the Birth Date
SELECT 	 birth_date, death_date
INTO 		:ldtm_birth_check, :ldtm_death_check
FROM 	INDIVIDUAL
WHERE 	individual_no = :al_individual_no
USING 	SQLCA;
	
SQLCA.nf_handle_error('n_individual', 'nf_check_annuity_end_date_brs()', 'SELECT  birth_date')

CHOOSE CASE ai_check_rule
	CASE 1, 2
		
		/* (RULE - 1)
		2.250 If an active annuity eligibility exists for the annuity account, and the birthdate is changed causing the annuity end date to be set in the future, 
		and scheduled payments/awards exists after the new annuity end date, a message is displayed indicating the payments should be reviewed.
		*/
		
		/*(RULE - 2)
		2.260 If an active annuity eligibility exists for the annuity account, and the birthdate is changed causing the annuity end date to be set in the past, 
		and payments/awards exists after the new annuity end date, a message is displayed indicating the claims/payments should be reviewed.
		*/
		
		//CHECK FOR SCHEDULED PAYMENTS AND AWARDS
		//handled 	by nf_check_benefit_payments()
	
		
	CASE 3 
		/*(RULE - 3)
		2.270	If an active annuity eligibility exists for the annuity account, and the birth date is changed, the annuity eligibility end date must not be prior to the 
		annuity eligibility start date.
		*/
		IF  year(adt_birth_date) + month(adt_birth_date) = year(DATE(ldtm_birth_check)) + month(DATE(ldtm_birth_check))  THEN RETURN 0
		
		// ldtm_annuity_end_date_calculated returned by reference
		lnv_common_annuity.nf_get_annuity_end_date(al_individual_no, as_claim_role , ls_message, ldtm_annuity_end_date_calculated, ldtm_annuity_eligibility_end_date_used, DATETIME(adt_birth_date), DATETIME(adt_death_date))
		
		//Problem
		IF ISNULL(ldtm_annuity_end_date_calculated) THEN RETURN -1
		
		IF date(adtm_ann_start_date) > date(ldtm_annuity_end_date_calculated) THEN RETURN -1
		
	CASE 4
	
		/*(RULE - 4)
		2.280	If an active annuity eligibility exists for the annuity account, and the death date is changed, the annuity eligibility end date must not be prior to the
		annuity eligibility start date.	
		*/
		
		// ldtm_annuity_end_date_calculated returned by reference
		lnv_common_annuity.nf_get_annuity_end_date(al_individual_no, as_claim_role , ls_message, ldtm_annuity_end_date_calculated, ldtm_annuity_eligibility_end_date_used, DATETIME(adt_birth_date), DATETIME(adt_death_date))
		
		//Problem
		IF ISNULL(ldtm_annuity_end_date_calculated) THEN RETURN -1
		
		//now do the check
		IF date(ldtm_annuity_end_date_calculated) < date(adtm_ann_start_date) THEN RETURN -1
		
END CHOOSE
		
RETURN 1

		

















end function

public function integer nf_check_iw_death_date (long al_individual_no, date adt_new_death_date);/*

2.276	The death date of the individual must not be changed if all of the following are true:
·	The individual is an injured worker
·	There is a surviving spouse associated with the claim for the injured worker
·	The surviving spouse has an active annuity eligibility for the claim
·	The change will cause the death date to be after the annuity start date for the surviving spouse

*/

DATETIME		ldtm_SS_annuity_start_date


SELECT	d.annuity_start_date
INTO		:ldtm_SS_annuity_start_date
FROM		CLAIM a
JOIN		CLAIM_PARTICIPANT b on a.claim_no = b.claim_no
JOIN		ANNUITY_ACCOUNT c on b.individual_no = c.individual_no
									and b.claim_no = c.claim_no
									and b.claim_role_code = c.claim_role_code
JOIN		ANNUITY_ELIGIBILITY d on c.annuity_account_no = d.annuity_account_no
WHERE		b.claim_role_code = 'SS'
AND		d.annuity_eligibility_status_code = 'A'
AND		a.individual_no = :al_individual_no
AND		d.annuity_start_date < :adt_new_death_date
USING SQLCA;
SQLCA.nf_handle_error('n_individual', 'nf_check_iw_death_date', 'embedded SQL: select count(*) from CLAIM...')

IF NOT IsNull(ldtm_SS_annuity_start_date) AND ldtm_SS_annuity_start_date <> DateTime('1900-01-01') THEN
	MessageBox('Death Date Error','The date of death for this injured worker cannot be changed.'&
										+'~r~nThe new date ('+String(adt_new_death_date,'yyyy-mm-dd')+') would be after the annuity start date ('+String(ldtm_SS_annuity_start_date,'yyyy-mm-dd')+') of the surviving spouse.',Exclamation!)
	RETURN -1
END IF

RETURN 0
end function

public function integer nf_check_ss_death_date (long al_individual_no, date adt_new_death_date);/*

2.277	The death date of the individual must not be changed if all of the following are true:
·	The individual plays the role of surviving spouse on the claim
·	The change will cause the death date to be before the death date of the injured worker associated with the claim

------------------------------------------------------------------------------------------------------------------------------------

2.278	The death date of the individual must not be changed if all of the following are true:
·	The individual is an injured worker
·	There is a surviving spouse associated with the claim for the injured worker
·	The surviving spouse has an active annuity eligibility for the claim
·	The change will cause the death date to move such that the surviving spouse’s annuity benefits are affected (See Rationale)

Rationale
2.278	If the injured worker’s death date changes from on or after January 1, 1998 to prior to January 1, 1998 or vice versa, the death date change must not be permitted due to the effect on the surviving spouse’s annuity benefits (i.e. different levels of benefits per legislation that is in effect for the respective dates)

	If the injured worker’s death date changes from on or after January 1, 1982 to prior to January 1, 1982, the death date change must not be permitted due to the effect on the surviving spouse’s annuity benefits (i.e. not eligible prior to January 1, 1982).

	In both cases above, the surviving spouse must be eligible for annuity benefits at the time of the death date change.

	In the case of a change to the injured worker’s death date from prior to January 1, 1982 to on or after January 1, 1982, the surviving spouse would not be eligible for annuity benefits. S/he would eventually appear on a confirm annuity eligibility checklist when they reach the age of 65.


*/

DATETIME		ldtm_IW_death_date
INTEGER		li_count


SELECT	c.death_date
INTO		:ldtm_IW_death_date
FROM		CLAIM a
JOIN		CLAIM_PARTICIPANT b on a.claim_no = b.claim_no
JOIN		INDIVIDUAL c on a.individual_no = c.individual_no
WHERE		b.claim_role_code = 'SS'
AND		b.individual_no = :al_individual_no
AND		c.death_date > :adt_new_death_date
USING SQLCA;
SQLCA.nf_handle_error('n_individual', 'nf_check_SS_death_date', 'embedded SQL: select death_date from CLAIM...')


// BR 2.277
IF NOT IsNull(ldtm_IW_death_date) AND ldtm_IW_death_date <> DateTime('1900-01-01') THEN
	MessageBox('Death Date Error','The date of death for this surviving spouse cannot be changed.'&
										+'~r~nThe new date ('+String(adt_new_death_date,'yyyy-mm-dd')+') would be prior to the death date ('+String(ldtm_IW_death_date,'yyyy-mm-dd')+') of the injured worker.',Exclamation!)
	RETURN -1
END IF



/*
BR 2.278

IW death date was > 1981, now before
*/

SELECT	Count(*)
INTO		:li_count
FROM		CLAIM a
JOIN		CLAIM_PARTICIPANT b on a.claim_no = b.claim_no
JOIN		INDIVIDUAL c on a.individual_no = c.individual_no
JOIN     ANNUITY_ACCOUNT d on b.claim_no = d.claim_no
                          and b.individual_no = d.individual_no
JOIN     ANNUITY_ELIGIBILITY e on d.annuity_account_no = e.annuity_account_no
WHERE		b.claim_role_code = 'SS'
AND      e.annuity_eligibility_status_code = 'A'
AND		a.individual_no = :al_individual_no
AND		c.death_date >= '1982-01-01'
and     :adt_new_death_date < '1982-01-01'
USING SQLCA;
SQLCA.nf_handle_error('n_individual', 'nf_check_SS_death_date', 'embedded SQL: select Count(*) from CLAIM...(1)')

IF li_count > 0 THEN
	MessageBox('Death Date Error','The date of death for this injured worker cannot be changed.' &
										+'~r~nThe new date ('+String(adt_new_death_date,'yyyy-mm-dd')+') would affect the annuity benefits of the surviving spouse.' &
										+'~r~nPlease contact an annuity administrator for surviving spouses.',Exclamation!)
	RETURN -1
END IF


/*
BR 2.278

IW death date was < 1982, now after
*/
SELECT	Count(*)
INTO		:li_count
FROM		CLAIM a
JOIN		CLAIM_PARTICIPANT b on a.claim_no = b.claim_no
JOIN		INDIVIDUAL c on a.individual_no = c.individual_no
JOIN     ANNUITY_ACCOUNT d on b.claim_no = d.claim_no
                          and b.individual_no = d.individual_no
JOIN     ANNUITY_ELIGIBILITY e on d.annuity_account_no = e.annuity_account_no
WHERE		b.claim_role_code = 'SS'
and      e.annuity_eligibility_status_code = 'A'
AND		a.individual_no = :al_individual_no
AND		c.death_date < '1982-01-01'
and     :adt_new_death_date >= '1982-01-01'
USING SQLCA;
SQLCA.nf_handle_error('n_individual', 'nf_check_SS_death_date', 'embedded SQL: select Count(*) from CLAIM...(2)')

IF li_count > 0 THEN
	MessageBox('Death Date Error','The date of death for this injured worker cannot be changed.' &
										+'~r~nThe new date ('+String(adt_new_death_date,'yyyy-mm-dd')+') would affect the annuity benefits of the surviving spouse.' &
										+'~r~nPlease contact an annuity administrator for surviving spouses.',Exclamation!)
	RETURN -1
END IF


/*
BR 2.278

IW death date was > 1997, now before
*/
SELECT	Count(*)
INTO		:li_count
FROM		CLAIM a
JOIN		CLAIM_PARTICIPANT b on a.claim_no = b.claim_no
JOIN		INDIVIDUAL c on a.individual_no = c.individual_no
JOIN     ANNUITY_ACCOUNT d on b.claim_no = d.claim_no
                          and b.individual_no = d.individual_no
JOIN     ANNUITY_ELIGIBILITY e on d.annuity_account_no = e.annuity_account_no
WHERE		b.claim_role_code = 'SS'
and      e.annuity_eligibility_status_code = 'A'
AND		a.individual_no = :al_individual_no
AND		c.death_date >= '1998-01-01'
and     :adt_new_death_date < '1998-01-01'
USING SQLCA;
SQLCA.nf_handle_error('n_individual', 'nf_check_SS_death_date', 'embedded SQL: select Count(*) from CLAIM...(3)')

IF li_count > 0 THEN
	MessageBox('Death Date Error','The date of death for this injured worker cannot be changed.' &
										+'~r~nThe new date ('+String(adt_new_death_date,'yyyy-mm-dd')+') would affect the annuity benefits of the surviving spouse.' &
										+'~r~nPlease contact an annuity administrator for surviving spouses.',Exclamation!)
	RETURN -1
END IF


/*
BR 2.278

IW death date was < 1998, now after
*/
SELECT	Count(*)
INTO		:li_count
FROM		CLAIM a
JOIN		CLAIM_PARTICIPANT b on a.claim_no = b.claim_no
JOIN		INDIVIDUAL c on a.individual_no = c.individual_no
JOIN     ANNUITY_ACCOUNT d on b.claim_no = d.claim_no
                          and b.individual_no = d.individual_no
JOIN     ANNUITY_ELIGIBILITY e on d.annuity_account_no = e.annuity_account_no
WHERE		b.claim_role_code = 'SS'
and         e.annuity_eligibility_status_code = 'A'
AND		a.individual_no = :al_individual_no
AND		c.death_date < '1998-01-01'
and     :adt_new_death_date >= '1998-01-01'
USING SQLCA;
SQLCA.nf_handle_error('n_individual', 'nf_check_SS_death_date', 'embedded SQL: select Count(*) from CLAIM...(4)')

IF li_count > 0 THEN
	MessageBox('Death Date Error','The date of death for this injured worker cannot be changed.' &
										+'~r~nThe new date ('+String(adt_new_death_date,'yyyy-mm-dd')+') would affect the annuity benefits of the surviving spouse.' &
										+'~r~nPlease contact an annuity administrator for surviving spouses.',Exclamation!)
	RETURN -1
END IF


RETURN 0
end function

public function integer nf_check_annuity_death_date (long al_individual_no, datetime adt_original_death_date, datetime adt_new_death_date);/* 
BR2.279:The death date of the individual must not be changed if all of the following are true:

		•	The individual has an annuity account
		•	The change affects the year and/or month of the death date
		•	One of the following is true:
			o	There is an outstanding Confirm Annuity Eligibility potential checklist for the annuity account with the ‘Confirm Annuity Eligibility’ step completed 
			o	There is an outstanding Confirm Annuity Eligibility payout checklist for the annuity account with the ‘Confirm Annuity Eligibility’ step completed (i.e. the annuity payout is in progress)
			o	There is a completed Confirm Annuity Eligibility payout checklist for the annuity account and the annuity payout is in progress
*/

INTEGER     li_count

IF year(DATE(adt_original_death_date)) <> year(DATE(adt_new_death_date)) or month(DATE(adt_original_death_date)) <> month(DATE(adt_new_death_date)) OR &
	     (isnull(adt_original_death_date) AND NOT ISNULL(adt_new_death_date)) OR (isnull(adt_new_death_date) AND NOT isnull(adt_original_death_date)) THEN
		
	// claimant
	// confirm annuity eligibility checklist not completed, but Confirm Annuity Eligibility is completed
	SELECT Count(*)
	INTO   :li_count
	FROM   ANNUITY_ACCOUNT     a
	JOIN   ANNUITY_ELIGIBILITY b  ON a.annuity_account_no                       = b.annuity_account_no
	JOIN   CHECKLIST           c  ON b.confirm_annuity_eligibility_checklist_no = c.checklist_no
	JOIN   CHECKLIST_STEP      d  ON c.checklist_no                             = d.checklist_no
	WHERE  a.claim_role_code             = 'C'
	AND    c.checklist_status_code       = 'IA'
	AND    d.checklist_step_status_code IN ('COM','COA')
	AND    d.checklist_step_type_code    = '007'  //Confirm Annuity Eligibility checklist step
	AND    a.individual_no               = :al_individual_no
	USING SQLCA;
	SQLCA.nf_handle_error('n_individual','nf_check_annuity_death_date()','SELECT Count(*)INTO :li_count (A)')
	
	IF li_count > 0 THEN
		MESSAGEBOX("Individual death date error", "The death date on this injured worker cannot be changed because there is an outstanding Confirm Annuity Eligibility " &
														+ "potential or payout checklist for the annuity account with the ‘Confirm Annuity Eligibility’ step completed.", Exclamation!)
		Return -1
	END IF
	
	// claimant or surviving spouse
	// confirm annuity eligibility payout checklist is completed, but associated annuity payout that created the eligibility is not completed
	SELECT Count(*)
	INTO   :li_count
	FROM   ANNUITY_ACCOUNT     a
	JOIN   ANNUITY_ELIGIBILITY b  ON a.annuity_account_no                       = b.annuity_account_no
	JOIN   CHECKLIST           c  ON b.confirm_annuity_eligibility_checklist_no = c.checklist_no
	JOIN   ANNUITY_PAYOUT      e  ON b.annuity_eligibility_no                   = e.annuity_eligibility_no
	WHERE  a.claim_role_code             = 'C'
	AND    c.checklist_status_code       = 'CA'
	AND    c.checklist_type_code         = 'CAEP'  // Confirm Annuity Eligibility - Payout 
	AND    e.annuity_payout_status_code  = 'I'
	AND    a.individual_no               = :al_individual_no
	USING SQLCA;
	SQLCA.nf_handle_error('n_individual','nf_check_annuity_death_date()','SELECT Count(*)INTO :li_count (B)')
	
	IF li_count > 0 THEN
		MESSAGEBOX("Individual death date error", "The death date on this individual cannot be changed because the Confirm Annuity Eligibility - Payout checklist " &
		                                                   + "is completed, but the associated Annuity Payout that created the eligibility is still in progress.", Exclamation!)
		Return -1
	END IF
	
	// surviving spouse
	// confirm annuity eligibility checklist not completed, but Confirm Annuity Eligibility step is completed
	SELECT Count(*) 
	INTO   :li_count
	FROM   ANNUITY_ACCOUNT     a
	JOIN   ANNUITY_ELIGIBILITY b  ON a.annuity_account_no                       = b.annuity_account_no
	JOIN   CHECKLIST           c  ON b.confirm_annuity_eligibility_checklist_no = c.checklist_no
	JOIN   CHECKLIST_STEP      d  ON c.checklist_no                             = d.checklist_no
	WHERE  a.claim_role_code            = 'SS'
	AND    c.checklist_status_code      = 'IA'
	AND    d.checklist_step_status_code IN ('COM','COA')
	AND    d.checklist_step_type_code   = '007'
	AND    a.individual_no              = :al_individual_no
	USING SQLCA;
	SQLCA.nf_handle_error('n_individual','nf_check_annuity_death_date()','SELECT Count(*)INTO :li_count (C)')
	
	IF li_count > 0 THEN
		MESSAGEBOX("Individual death date error", "The death date on this surviving spouse cannot be changed because there is an outstanding Confirm Annuity Eligibility " &
														+ "potential or payout checklist for the annuity account with the ‘Confirm Annuity Eligibility’ step completed.", Exclamation!)
		Return -1
	END IF

END IF

RETURN 0

end function

public function integer nf_check_annuity_birth_date (long al_individual_no, datetime adt_original_birth_date, datetime adt_new_birth_date);/* 
BR2.271	The birth date of the individual must not be changed if all of the following are true:
		
		•	The individual has an annuity account
		•	The change affects the year and/or month of the birth date
		•	One of the following is true:
			o	There is an outstanding Confirm Annuity Eligibility potential checklist for the annuity account with the ‘Confirm Birth Date’ step completed 
			o	There is an outstanding Confirm Annuity Eligibility payout checklist for the annuity account with the ‘Confirm Birth Date’ step completed (i.e. the annuity payout is in progress)
			o	There is a completed Confirm Annuity Eligibility payout checklist for the annuity account and the annuity payout is in progress
*/

INTEGER    li_count

IF year(DATE(adt_original_birth_date)) <> year(DATE(adt_new_birth_date)) or month(DATE(adt_original_birth_date)) <> month(DATE(adt_new_birth_date)) OR &
		     (isnull(adt_original_birth_date) AND NOT ISNULL(adt_new_birth_date)) OR (isnull(adt_new_birth_date) AND NOT isnull(adt_original_birth_date)) THEN
	
	// claimant
	// confirm annuity eligibility checklist not completed, but Confirm Birth Date checklist step is completed
	SELECT Count(*)
	INTO   :li_count
	FROM   ANNUITY_ACCOUNT     a
	JOIN   ANNUITY_ELIGIBILITY b  ON a.annuity_account_no                       = b.annuity_account_no
	JOIN   CHECKLIST           c  ON b.confirm_annuity_eligibility_checklist_no = c.checklist_no
	JOIN   CHECKLIST_STEP      d  ON c.checklist_no                             = d.checklist_no
	WHERE  a.claim_role_code             = 'C'
	AND    c.checklist_status_code       = 'IA'
	AND    d.checklist_step_status_code  = 'COM'
	AND    d.checklist_step_type_code    = '005'  //Confirm Birth Date
	AND    a.individual_no               = :al_individual_no
	USING SQLCA;
	SQLCA.nf_handle_error('n_individual','nf_check_annuity_birth_date()','SELECT Count(*)INTO :li_count (A)')
	
	IF li_count > 0 THEN
		MESSAGEBOX("Individual birth date error", "The birth date on this injured worker cannot be changed because there is an outstanding Confirm Annuity Eligibility " &
														+ "potential or payout checklist for the annuity account with the ‘Confirm Birth Date’ step completed.", Exclamation!)
		Return -1
	END IF
	
	// claimant or surviving spouse
	// confirm annuity eligibility checklist is completed, but associated annuity payout that created the eligibility is not completed
	SELECT Count(*)
	INTO   :li_count
	FROM   ANNUITY_ACCOUNT     a
	JOIN   ANNUITY_ELIGIBILITY b  ON a.annuity_account_no                       = b.annuity_account_no
	JOIN   CHECKLIST           c  ON b.confirm_annuity_eligibility_checklist_no = c.checklist_no
	JOIN   ANNUITY_PAYOUT      e  ON b.annuity_eligibility_no                   = e.annuity_eligibility_no
	WHERE  a.claim_role_code             = 'C'
	AND    c.checklist_status_code       = 'CA'
	AND    c.checklist_type_code         = 'CAEP'  // Confirm Annuity Eligibility - Payout 
	AND    e.annuity_payout_status_code  = 'I'
	AND    a.individual_no               = :al_individual_no
	USING SQLCA;
	SQLCA.nf_handle_error('n_individual','nf_check_annuity_birth_date()','SELECT Count(*)INTO :li_count (B)')
	
	IF li_count > 0 THEN
		MESSAGEBOX("Individual birth date error", "The birth date on this individual cannot be changed because the Confirm Annuity Eligibility - Payout checklist is completed, " &
		                                                   + "but the associated Annuity Payout that created the eligibility is still in progress.", Exclamation!)
		Return -1
	END IF
	
	// surviving spouse
	// confirm annuity eligibility checklist not completed, but Confirm Birth Date checklist step is completed
	SELECT Count(*) 
	INTO   :li_count
	FROM   ANNUITY_ACCOUNT     a
	JOIN   ANNUITY_ELIGIBILITY b  ON a.annuity_account_no                       = b.annuity_account_no
	JOIN   CHECKLIST           c  ON b.confirm_annuity_eligibility_checklist_no = c.checklist_no
	JOIN   CHECKLIST_STEP      d  ON c.checklist_no                             = d.checklist_no
	WHERE  a.claim_role_code            = 'SS'
	AND    c.checklist_status_code      = 'IA'
	AND    d.checklist_step_status_code = 'COM'
	AND    d.checklist_step_type_code   = '005'
	AND    a.individual_no              = :al_individual_no
	USING SQLCA;
	SQLCA.nf_handle_error('n_individual','nf_check_annuity_birth_date()','SELECT Count(*)INTO :li_count (C)')
	
	IF li_count > 0 THEN
		MESSAGEBOX("Individual birth date error", "The birth date on this surviving spouse cannot be changed because there is an outstanding Confirm Annuity Eligibility " &
														+ "potential or payout checklist for the annuity account with the ‘Confirm Birth Date’ step completed.", Exclamation!)
		Return -1
	END IF

END IF

RETURN 0

end function

public function integer nf_check_annuity_dependant_death_date (long al_individual_no, datetime adt_original_death_date, datetime adt_new_death_date);
/*
BR2.280 The death date of the individual must not be changed if all of the following are true:
			•	The individual is involved in an annuity payout that is in progress
			•	The individual is a dependent annuity payout participant
			•	The change affects the year and/or month and/or day of the death date 
			•	One of the following is true:
				o	overpayment recovery has been confirmed for the annuity payout
				o	if deceased, the dependent’s death date is on or before the death date of the individual who is eligible for annuity benefits, 
						if the eligible individual died prior to the age of 65
				o	if deceased, the dependent’s death date is on or before the 65th birthday of the individual who is eligible for annuity benefits
				o	if deceased, the dependent is represented by another annuity payout participant who is not an estate
				o	if not deceased, the dependent is represented by another annuity payout participant who is an estate

Note: The individual could be a participant, and even a dependant, on more than one annuity account, therefore we need to examine all 
benefit holder's accounts that this individual is a participant of and determine on a case by case basis if this individual's death date can change.
If any one account satifies the above conditions, then the recipient's birthdate cannot change i.e., return a -1
*/

INTEGER  li_count, li_age, li_rows, li_rtn
LONG     ll_eligible_individual_no, ll_represented_by_individual_no, ll_annuity_payout_no
DATETIME ldt_death_date_eligible_individual, ldt_65th_birth_date_eligible_indiv
STRING   ls_overpayment_recovery_flag, ls_represented_by_annuity_role_code, ls_message_details

U_DS     lds_annuity_dependants

IF DATE(adt_original_death_date) <> DATE(adt_new_death_date)            OR &
   (isNull(adt_original_death_date) AND Not isNull(adt_new_death_date)) OR &
	(isNull(adt_new_death_date) AND Not isNull(adt_original_death_date)) THEN
	
	// Use the datastore to retrieve all annuity payouts where this individual is a dependant, and the annuity payout is in progress
	lds_annuity_dependants = create U_DS
	lds_annuity_dependants.dataobject = 'ds_dependant_annuity_accounts'
	lds_annuity_dependants.setTransObject(SQLCA)
	
	li_rows = lds_annuity_dependants.retrieve(al_individual_no)
	SQLCA.nf_handle_error("n_individual", "nf_check_annuity_dependant_death_date", "lds_annuity_dependants.retrieve()")
	
	IF li_rows > 0 THEN
		FOR li_count = 1 to li_rows
			ll_eligible_individual_no          = lds_annuity_dependants.getItemNumber(li_count,  'benefit_holder_no')
			ldt_death_date_eligible_individual = lds_annuity_dependants.getItemDateTime(li_count,'benefit_holder_death_date')
			li_age                             = lds_annuity_dependants.getItemNumber(li_count,  'benefit_holder_age')
			ldt_65th_birth_date_eligible_indiv = lds_annuity_dependants.getItemDateTime(li_count,'benefit_holder_65th_birthday')
			ls_overpayment_recovery_flag       = lds_annuity_dependants.getItemString(li_count,  'overpayment_recovery_confirmed_flag')
			ll_represented_by_individual_no    = lds_annuity_dependants.getItemNumber(li_count,  'represented_by_recipient_no')
			ll_annuity_payout_no               = lds_annuity_dependants.getItemNumber(li_count,  'annuity_payout_no')
			
			// if the al_individual_no is the person who is eligible for benefits, then skip this record, its not a dependant
			IF al_individual_no = ll_eligible_individual_no THEN continue 

			SELECT CHAR(9)+ 'Eligible Individual'+CHAR(9)+'- '  + i.given_names + ' ' + i.last_name + ' ('
       				+ CASE app.annuity_role_code 
         				WHEN 'C' 
         				THEN 'Injured Worker' 
         				Else 'Surviving Spouse' END + ')'       
       				+ CHAR(13) + CHAR(9) + 'Individual No.'+CHAR(9)+'- '  + convert(varchar(10),:ll_eligible_individual_no)
       				+ CHAR(13) + CHAR(9) + 'Annuity Payout No.'+CHAR(9)+'- '     + convert(varchar(10),:ll_annuity_payout_no)
			INTO  :ls_message_details
			FROM  INDIVIDUAL i 
			JOIN  ANNUITY_PAYOUT_PARTICIPANT app on i.individual_no = app.recipient_no
			WHERE app.annuity_payout_no = :ll_annuity_payout_no
			AND   i.individual_no = :ll_eligible_individual_no
			USING SQLCA;
			SQLCA.nf_handle_error("n_individual", "nf_check_annuity_dependant_death_date","SELECT Annuity payout message details from ANNUITY_PAYOUT_PARTICIPANT")

			ls_message_details += "~r~n~nPlease make note of the eligible individual and annuity payout numbers and contact the person " &
			                                        + "dealing with the annuity payout to resolve this issue."
																 
			ls_message_details += "~r~n~nNote: To get a snapshot of the error message, ensure you are focussed on the error message, hold down the " &
			                                        + "Ctrl, Alt & Print Screen keys, then paste into an e-mail via Ctrl & C keys and send it."
																 

			// if the overpayment recovery has been confirmed for the annuity payout, then the death date cannot be changed, so return -1
			IF ls_overpayment_recovery_flag = 'Y' THEN
				Messagebox("Death Date Error", "The death date cannot be added or changed for this individual because he/she is a dependant annuity participant " &
				                             + "involved in an annuity payout that is in progress, where the overpayment recovery step has been confirmed." &
													  + "~r~n~n" + ls_message_details, Exclamation!)
				Return -1
			END IF
			
			// for cases where this individual IS deceased,(death date changed, or death date being added) 
			// we check the following 3 sets of conditions:
			IF NOT ISNULL(adt_new_death_date) THEN 
				
				// under the following conditions, a change to the individual's death date cannot be made, so we are finished here, so return a -1
				IF NOT isnull(ldt_death_date_eligible_individual) AND li_age < 65 AND adt_new_death_date <= ldt_death_date_eligible_individual THEN
					Messagebox("Death Date Error", "The death date cannot be added or changed for this individual because he/she is a dependant annuity participant " &
				                              + "involved in an annuity payout that is in progress, where the dependent’s death date is on or before the "&
														+ "death date of the individual who is eligible for annuity benefits, who died prior to the age of 65." &
													   + "~r~n~n" + ls_message_details, Exclamation!)
					RETURN -1
				END IF
		
				// under these conditions also, a change to the individual's death date cannot be made, so we are finished here, so return a -1
				IF li_age >= 65 AND adt_new_death_date <= ldt_65th_birth_date_eligible_indiv THEN
					Messagebox("Death Date Error", "The death date cannot be added or changed for this individual because he/she is a dependant annuity participant " &
				                              + "involved in an annuity payout that is in progress where the dependent’s death date is on or before "&
														+ "the 65th birthday of the individual who is eligible for annuity benefits, and the individual who is " &
														+ "eligible for annuity benefits attained 65 years of age" &
													   + "~r~n~n" + ls_message_details, Exclamation!)					
					RETURN -1		
				END IF
				
				//If there is at least one case where the dependent is represented by another annuity payout participant who is not an estate, return -1
				SELECT count(*)
				INTO   :li_rtn
				FROM   ANNUITY_PAYOUT_PARTICIPANT app1
				WHERE  app1.recipient_no = :al_individual_no   // where recipient is the individual passed in, the one being worked on 
				AND    app1.recipient_no <> app1.represented_by_recipient_no // where they are not representing themselves
				AND EXISTS( SELECT * 
           					FROM   ANNUITY_PAYOUT_PARTICIPANT app2 
           					WHERE  app2.annuity_payout_no = app1.annuity_payout_no
           					AND    app2.recipient_no      = app1.represented_by_recipient_no
           					AND    app2.annuity_role_code <> '18')	// AND where this person's representative is not an estate		
				USING SQLCA;
				SQLCA.nf_handle_error("n_individual", "nf_check_annuity_dependant_death_date","SELECT count(*)FROM   ANNUITY_PAYOUT_PARTICIPANT")
				
				IF li_rtn > 0 THEN 
					Messagebox("Death Date Error", "The death date cannot be added or changed for this individual because he/she is a dependant annuity participant involved in " &
				                                + "an annuity payout that is in progress, and is represented by another annuity payout participant who is not an estate." &
													     + "~r~n~n" + ls_message_details, Exclamation!)
					RETURN -1 // can't change (add) the death date because the individual has a representative other than an Estate
				END IF
				
			// and for the case where the individual being checked is not deceased: (as in when the death date is being removed)
			// If the dependent is represented by another annuity payout participant who is an estate, the date cannot be changed
			ELSEIF ISNULL(adt_new_death_date) THEN
				SELECT count(*)
				INTO   :li_rtn
				FROM   ANNUITY_PAYOUT_PARTICIPANT app1
				WHERE  app1.recipient_no = :al_individual_no   // where recipient is the individual passed in, the one being worked on 
				AND    app1.recipient_no <> app1.represented_by_recipient_no // where they are not representing themselves
				AND EXISTS( SELECT * 
           					FROM   ANNUITY_PAYOUT_PARTICIPANT app2 
           					WHERE  app2.annuity_payout_no = app1.annuity_payout_no
           					AND    app2.recipient_no      = app1.represented_by_recipient_no
           					AND    app2.annuity_role_code = '18')	// AND where this person's representative is an estate		
				USING SQLCA;
				SQLCA.nf_handle_error("n_individual", "nf_check_annuity_dependant_death_date","SELECT count(*)FROM   ANNUITY_PAYOUT_PARTICIPANT")
				
				IF li_rtn > 0 THEN 
					Messagebox("Death Date Error", "The death date cannot be removed for this individual because he/she is a dependant annuity participant involved in " &
				                               + "an annuity payout that is in progress, and is represented by another annuity payout participant who is an estate." &
													    + "~r~n~n" + ls_message_details, Exclamation!)
					RETURN -1 // can't change (remove) the death date because the individual has a representative that is an Estate
				END IF
				
			END IF
			
		NEXT
		
	END IF
END IF

RETURN 0
end function

on destructor;call n_pdc::destructor;	IF IsValid(inv_address) THEN
		Destroy(inv_address)
	END IF
end on

on n_individual.create
call super::create
end on

on n_individual.destroy
call super::destroy
end on

