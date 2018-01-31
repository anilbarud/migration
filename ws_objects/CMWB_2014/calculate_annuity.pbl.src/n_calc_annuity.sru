$PBExportHeader$n_calc_annuity.sru
forward
global type n_calc_annuity from nonvisualobject
end type
end forward

global type n_calc_annuity from nonvisualobject
end type
global n_calc_annuity n_calc_annuity

forward prototypes
public function boolean nf_last_quarter_interest_rate_exists (date adt_annuity_end_date)
public function integer nf_validate_calculation_reason (string as_annuity_calc_reason_code, s_calc_reason_data astr_calc_reason_data, ref string as_set_annuity_calc_reason_code)
end prototypes

public function boolean nf_last_quarter_interest_rate_exists (date adt_annuity_end_date);BOOLEAN  lb_last_quarter_annuity_rate_exists
DATETIME ldtm_quarter_end_date
INTEGER  li_annuity_end_date_year, li_annuity_end_date_quarter, li_end_quarter_count



// determine year of annuity end date
li_annuity_end_date_year = Year(adt_annuity_end_date)

// determine quarter of annuity end date
CHOOSE CASE Month(adt_annuity_end_date)
	CASE 1,2,3
		li_annuity_end_date_quarter = 1
		ldtm_quarter_end_date = DateTime(String(li_annuity_end_date_year)+'-04-01')
	CASE 4,5,6
		li_annuity_end_date_quarter = 2
		ldtm_quarter_end_date = DateTime(String(li_annuity_end_date_year)+'-07-01')				
	CASE 7,8,9
		li_annuity_end_date_quarter = 3
		ldtm_quarter_end_date = DateTime(String(li_annuity_end_date_year)+'-10-01')
	CASE 10,11,12
		li_annuity_end_date_quarter = 4
		ldtm_quarter_end_date = DateTime(String(li_annuity_end_date_year+1)+'-01-01')
END CHOOSE


// determine if "last quarter's" annuity set aside interest record exists
SELECT Count(*)
INTO   :li_end_quarter_count
FROM   Annuity_Interest_Rate
WHERE  year = :li_annuity_end_date_year
AND    quarter_no = :li_annuity_end_date_quarter
AND    active_flag = 'Y'
AND    interest_applied_date is not null
USING SQLCA;
SQLCA.nf_handle_error('w_select_calc_reason_response', 'embedded SQL: select Count(*) FROM Annuity_Interest_Rate...','nf_last_quarter_interest_rate_exists')


IF li_end_quarter_count = 1 THEN
	lb_last_quarter_annuity_rate_exists = TRUE
END IF

RETURN lb_last_quarter_annuity_rate_exists
end function

public function integer nf_validate_calculation_reason (string as_annuity_calc_reason_code, s_calc_reason_data astr_calc_reason_data, ref string as_set_annuity_calc_reason_code);BOOLEAN    lb_is_65,lb_deceased_before_65th_bday
BOOLEAN    lb_greater_than_annuity_end_date, lb_last_quarter_annuity_rate_exists
BOOLEAN    lb_pre_annuity_payout_rollout_payment, lb_birth_date_change, lb_death_date_change
BOOLEAN    lb_continue
DATE       ldt_annuity_end_date
DATETIME   ldtm_65_birthday, ldtm_death_date, ldtm_current, ldtm_annuity_end_date
DATETIME   ldtm_65_birthday_end_of_month, ldtm_death_date_end_of_month
DATETIME   ldtm_original_annuity_eligibility_create_date
INTEGER    li_annuity_payment_count, li_birth_date_change_count, li_death_date_change_count
STRING     ls_message_end, ls_annuity_eligibility_status_code


/*

The calculation reason chosen by the user must be validated
If there is BR violation where the user is required to choose a specific reason, these should be tested & displayed first.

*/


ldtm_current = DateTime(Date(f_server_datetime()))

// 65th b-day & death date
SELECT DateAdd(yy,65,birth_date), 
       death_date, 
		 dbo.udf_Last_Date_Of_Current_Month(DateAdd(yy,65,birth_date)), 
		 dbo.udf_Last_Date_Of_Current_Month(death_date)
INTO   :ldtm_65_birthday, 
       :ldtm_death_date,
		 :ldtm_65_birthday_end_of_month,
		 :ldtm_death_date_end_of_month
FROM   INDIVIDUAL
WHERE  individual_no = :astr_calc_reason_data.individual_no
USING SQLCA;
SQLCA.nf_handle_error('w_select_calc_reason_response', 'embedded SQL', 'select DateAdd(yy,65,birth_date), death_date FROM INDIVIDUAL...')

IF IsNull(ldtm_death_date) THEN
	IF ldtm_65_birthday <= ldtm_current THEN
		lb_is_65 = TRUE
	ELSE
		lb_is_65 = FALSE
	END IF
ELSE
	// deceased
	IF ldtm_death_date < ldtm_65_birthday THEN
		lb_deceased_before_65th_bday = TRUE
	ELSE
		lb_deceased_before_65th_bday = FALSE
		IF ldtm_65_birthday <= ldtm_current THEN
			lb_is_65 = TRUE
		ELSE
			lb_is_65 = FALSE
		END IF
	END IF
END IF


// last quarter
IF NOT IsNull(astr_calc_reason_data.annuity_end_date) THEN
	ldt_annuity_end_date = Date(astr_calc_reason_data.annuity_end_date)
	
	// find out if the last quarter for the given end date exists
	lb_last_quarter_annuity_rate_exists = nf_last_quarter_interest_rate_exists(ldt_annuity_end_date)
	
	IF Date(ldtm_current) > Date(astr_calc_reason_data.annuity_end_date) THEN
		lb_greater_than_annuity_end_date = TRUE
	END IF
ELSE
	SetNull(ldt_annuity_end_date)
END IF


// annuity eligibility status
SELECT a.annuity_eligibility_status_code,
       b.create_date
INTO   :ls_annuity_eligibility_status_code,
       :ldtm_original_annuity_eligibility_create_date
FROM   ANNUITY_ELIGIBILITY a
JOIN   CHECKLIST           b ON a.confirm_annuity_eligibility_checklist_no = b.checklist_no
WHERE  annuity_eligibility_no = :astr_calc_reason_data.annuity_eligibility_no
USING SQLCA;
SQLCA.nf_handle_error('n_calc_annuity','embedded SQL: SELECT annuity_eligibility_status_code FROM ANNUITY_ELIGIBILITY ...','nf_validate_calculation_reason')


// annuity payout exists prior to Prepare Annuity Payout rollout
IF astr_calc_reason_data.claim_role_code = 'C' THEN
	SELECT COUNT(*)
	INTO   :li_annuity_payment_count
	FROM   PAYMENT           a
	JOIN   APPLIED_CLAIM_TXN b ON a.payment_no = b.payment_no
	WHERE  a.payment_type_code     = '97'
	AND    a.payment_sub_type_code = 'CM'
	AND    b.recipient_type_code   = 'I'
	AND    b.recipient_no          = :astr_calc_reason_data.individual_no
	AND NOT EXISTS ( SELECT *
	                 FROM   ANNUITY_PAYOUT_PRE_1993 c
						  WHERE  c.payout_payment_no = a.payment_no )
	AND    b.txn_entry_date < ( SELECT use_prepare_annuity_payout_module_date 
	                            FROM   Annuity_Parameter
										 WHERE  active_flag = 'Y')
	USING SQLCA;
	SQLCA.nf_handle_error('n_calc_annuity','embedded SQL: SELECT COUNT(*) FROM PAYMENT, APPLIED_CLAIM_TXN (IW)...','nf_validate_calculation_reason')
ELSE
	SELECT COUNT(*)
	INTO   :li_annuity_payment_count
	FROM   PAYMENT           a
	JOIN   APPLIED_CLAIM_TXN b ON a.payment_no = b.payment_no
	WHERE  a.payment_type_code     = '97'
	AND    a.payment_sub_type_code = 'CM'
	AND    b.recipient_type_code   = 'I'
	AND    b.recipient_no          = :astr_calc_reason_data.individual_no
	AND    b.claim_no              = :astr_calc_reason_data.claim_no
	AND    b.txn_entry_date < ( SELECT use_prepare_annuity_payout_module_date 
	                            FROM   Annuity_Parameter
										 WHERE  active_flag = 'Y')
	USING SQLCA;
	SQLCA.nf_handle_error('n_calc_annuity','embedded SQL: SELECT COUNT(*) FROM PAYMENT, APPLIED_CLAIM_TXN (IW)...','nf_validate_calculation_reason')
END IF
	
IF li_annuity_payment_count > 0 THEN
	lb_pre_annuity_payout_rollout_payment = TRUE
END IF


// birth date change after placement on potential confirm annuity eligibility list
IF astr_calc_reason_data.annuity_payout_no = 0 AND lb_pre_annuity_payout_rollout_payment THEN
	
	SELECT COUNT(*)
	INTO   :li_birth_date_change_count
	FROM   INDIVIDUAL_BIRTH_DATE_CHANGE
	WHERE  create_date   > :ldtm_original_annuity_eligibility_create_date
	AND    individual_no = :astr_calc_reason_data.individual_no
	USING SQLCA;
	SQLCA.nf_handle_error('n_calc_annuity','embedded SQL: SELECT COUNT(*) FROM INDIVIDUAL_BIRTH_DATE_CHANGE (1)...','nf_validate_calculation_reason')
	
	IF li_birth_date_change_count > 0 THEN			
		// birth date change occurred after being added to annuity eligibility list
		lb_birth_date_change = TRUE
	END IF
	
	SELECT COUNT(*)
	INTO   :li_death_date_change_count
	FROM   INDIVIDUAL_DEATH_DATE_CHANGE
	WHERE  create_date   > :ldtm_original_annuity_eligibility_create_date
	AND    individual_no = :astr_calc_reason_data.individual_no
	USING SQLCA;
	SQLCA.nf_handle_error('n_calc_annuity','embedded SQL: SELECT COUNT(*) FROM INDIVIDUAL_DEATH_DATE_CHANGE (1)...','nf_validate_calculation_reason')
	
	IF li_death_date_change_count > 0 THEN
		// death date change occurred after being added to annuity eligibility list
		lb_death_date_change = TRUE
	END IF
	
END IF

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
// "must choose" error messages

/*--------------------------*/
// Eligibility Change (payout)

IF as_annuity_calc_reason_code <> '13' THEN
	// For payout
	IF astr_calc_reason_data.annuity_payout_no > 0 THEN
	
		/*--------------------------*/
		// Payout - No Longer Eligible
		IF ls_annuity_eligibility_status_code = 'I' THEN
			IF as_annuity_calc_reason_code = '' THEN
				as_set_annuity_calc_reason_code = '13'
				RETURN 0
			ELSE
				MessageBox('Calculation Error','The calculation reason must be "Eligibility Change (payout)", if the following is true:' &
													+'~r~n·	the calculation has been initiated by the annuity payout' &
													+'~r~n·	the individual on the annuity account is not eligible for annuity benefits.' &
													+'~r~n~r~nPlease select "Eligibility Change (payout)".',StopSign!)
				RETURN -1
			END IF
		END IF
	
		/*--------------------------*/
		// Payout - annuity end date now in future
		IF ls_annuity_eligibility_status_code = 'A' THEN
			IF ldtm_current < astr_calc_reason_data.annuity_end_date THEN
				IF as_annuity_calc_reason_code = '' THEN
					as_set_annuity_calc_reason_code = '13'
					RETURN 0
				ELSE
					MessageBox('Calculation Error','The calculation reason must be "Eligibility Change (payout)", if the following is true:' &
														+'~r~n·	the calculation has been initiated by the annuity payout' &
														+'~r~n·	the individual on the annuity account is eligible for annuity benefits' &
														+'~r~n·	the annuity end date is in the future.' &
														+'~r~n~r~nPlease select "Eligibility Change (payout)".',StopSign!)
					RETURN -1
				END IF
			END IF
		END IF
	END IF
END IF

/*--------------------------*/
// Payout - Attained Age 65

IF as_annuity_calc_reason_code <> '04' THEN
   /*
	BR 1.150
		The calculation must have a calculation reason of ‘Payout – Attained Age 65’, only if all of the following are true:
		·	the individual on the annuity account is at least 65 years old
		·	the interest rate for the last quarter of the annuity eligibility period is available
		·	one of the following is true:
			§	the annuity calculation is being done for payout purposes via Confirm Annuity Eligibility – Payout checklist
			§	there is a  post-1992 annuity payment for the injured worker on the annuity account that was entered before 
			§	there is a  post-1992 annuity payment for the injured worker on the annuity account that was entered before 
			   the Prepare Annuity Payout module became effective and the individual attained age 65 prior to being placed 
				on the potential confirm annuity eligibility list and their birth date and death date were not changed after 
				being placed on the potential confirm annuity eligibility list
         §	there is an annuity payment for the claim and surviving spouse on the annuity account that was entered before 
			   the Prepare Annuity Payout module became effective and the individual attained age 65 prior to being placed 
				on the potential confirm annuity eligibility list and their birth date and death date were not changed after 
				being placed on the potential confirm annuity eligibility list.

			Refer to Rationale

	*/
	IF lb_is_65 AND lb_deceased_before_65th_bday = FALSE AND lb_last_quarter_annuity_rate_exists THEN
		IF astr_calc_reason_data.annuity_payout_no > 0 THEN
			// reason must be 04
			lb_continue = TRUE
		ELSEIF lb_pre_annuity_payout_rollout_payment AND lb_birth_date_change = FALSE AND lb_death_date_change = FALSE THEN
			IF ldtm_original_annuity_eligibility_create_date > ldtm_65_birthday THEN
				// reason must be 04
				lb_continue = TRUE
			END IF
		END IF
		
		IF lb_continue THEN
			lb_continue = FALSE
			IF as_annuity_calc_reason_code = '' THEN
				as_set_annuity_calc_reason_code = '04'
				RETURN 0
			ELSE
				MessageBox('Calculation Error','The calculation reason must be "Payout - Attained Age 65", if all of the following are true:' &
													+'~r~n· the individual on the annuity account is at least 65 years old' &
													+'~r~n· the interest rate for the last quarter of the annuity eligibility period is available.' &
													+'~r~n· one of the following is true:' &
													+'~r~n      · the annuity calculation is being done for payout purposes via Confirm Annuity Eligibility – Payout checklist' &
													+'~r~n      · there is a  post-1992 annuity payment for the injured worker on the annuity account that was entered before ' &
													       +'the Prepare Annuity Payout module became effective and the individual attained age 65 prior to being placed on ' &
													       +'the potential confirm annuity eligibility list and their birth date and death date was not changed after being placed on ' &
													       +'the potential confirm annuity eligibility list' &
													+'~r~n      · there is an annuity payment for the claim and surviving spouse on the annuity account that was entered before ' &
													      +'the Prepare Annuity Payout module became effective and the individual attained age 65 prior to being placed on ' &
													      +'the potential confirm annuity eligibility list and their birth date and death date was not changed after being placed on ' &
													      +'the potential confirm annuity eligibility list.' &
													+'~r~n~r~nPlease select "Payout - Attained Age 65".',StopSign!)
				RETURN -1
			END IF
		END IF
	END IF
END IF


/*-------------------*/
// Payout - Deceased Prior to Age 65

IF as_annuity_calc_reason_code <> '05' THEN
	/*
	BR 1.160
		The calculation must have a calculation reason of ‘Payout – Deceased Prior to Age 65’, only if all of the following are true:
		·	the individual on the annuity account died prior to attaining age 65
		·	the interest rate for the last quarter of the annuity eligibility period is available
		·	one of the following is true:
			§	the annuity calculation is being done for payout purposes via Confirm Annuity Eligibility – Payout checklist
			§	there is a post-1992 annuity payment for the injured worker on the annuity account that was entered before 
			   the Prepare Annuity Payout module became effective and the individual died prior to attaining age 65 and 
				prior to being placed on the potential confirm annuity eligibility list and their birth date and death date 
				were not changed after being placed on the potential confirm annuity eligibility list
         §	there is an annuity payment for the claim and surviving spouse on the annuity account that was entered before 
			   the Prepare Annuity Payout module became effective and the individual died prior to attaining age 65 and 
				prior to being placed on the potential confirm annuity eligibility list and their birth date and death date 
				were not changed after being placed on the potential confirm annuity eligibility list.

		Refer to Rationale

	*/		
	IF lb_deceased_before_65th_bday AND lb_last_quarter_annuity_rate_exists THEN
		IF astr_calc_reason_data.annuity_payout_no > 0 THEN
			// reason must be 05
			lb_continue = TRUE
		ELSEIF lb_pre_annuity_payout_rollout_payment AND lb_birth_date_change = FALSE AND lb_death_date_change = FALSE THEN
			IF ldtm_original_annuity_eligibility_create_date > ldtm_death_date THEN
				// reason must be 05
				lb_continue = TRUE
			END IF
		END IF
		
		IF lb_continue THEN
			lb_continue = FALSE
		
			IF as_annuity_calc_reason_code = '' THEN
				as_set_annuity_calc_reason_code = '05'
				RETURN 0
			ELSE
				MessageBox('Calculation Error','The calculation reason must be "Payout - Deceased Prior to Age 65", if all of the following are true:' &
													+'~r~n· the individual on the annuity account died prior to attaining age 65' &
													+'~r~n· the interest rate for the last quarter of the annuity eligibility period is available.' &
													+'~r~n· one of the following is true:' &
													+'~r~n   · the annuity calculation is being done for payout purposes via Confirm Annuity Eligibility – Payout checklist' &
													+'~r~n   · there is a post-1992 annuity payment for the injured worker on the annuity account that was entered before ' &
													         +'the Prepare Annuity Payout module became effective and the individual died prior to attaining age 65 and ' &
													         +'prior to being placed on the potential confirm annuity eligibility list and their birth and death date was not changed ' &
													         +'after being placed on the potential confirm annuity eligibility list' &
													+'~r~n   ·   there is an annuity payment for the claim and surviving spouse on the annuity account that was entered before ' &
													         +'the Prepare Annuity Payout module became effective and the individual died prior to attaining age 65 and ' &
													         +'prior to being placed on the potential confirm annuity eligibility list and their birth and death date was not changed ' &
													         +'after being placed on the potential confirm annuity eligibility list.' &
													+'~r~n~r~nPlease select "Payout - Deceased Prior to Age 65".',StopSign!)
				RETURN -1
			END IF
		END IF
	END IF
END IF



/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
// "choose another reason" error messages


/*--------------------------*/
// Payout - Attained Age 65

IF as_annuity_calc_reason_code = '04' THEN
	
	/*
	BR 1.155
	The calculation must not have a calculation reason of ‘Payout – Attained Age 65’, if any one of the following is true:
	·	the individual on the annuity account is less than  65 years old and did not die prior to attaining age 65
	·	the interest rate for the last quarter of the annuity eligibility period is not available
	
	·	there is not a post-1992 annuity payment for the injured worker on the annuity account that was entered 
	   before the Prepare Annuity Payout module became effective and the calculation is not being done for payout purposes via Confirm Annuity Eligibility – Payout checklist		
	·	there is a post-1992 annuity payment for the injured worker on the annuity account that was entered 
	   before the Prepare Annuity Payout module became effective and the individual attained age 65 since being placed on 
		the potential confirm annuity eligibility list and the individual’s birth date and death date were not changed since being placed on the potential confirm annuity eligibility list		
	·	there is a post-1992 annuity payment for the injured worker on the annuity account that was entered 
	   before the Prepare Annuity Payout module became effective and the individual’s birth date or death date were changed since being placed on 
		the potential confirm annuity eligibility list
		
	·	there is not an annuity payment for the claim and surviving spouse on the annuity account that was entered before the Prepare Annuity Payout module 
	   became effective and the calculation is not being done for payout purposes
		
	·	there is an annuity payment for the claim and surviving spouse on the annuity account that was entered before the Prepare Annuity Payout module 
	   became effective and the individual attained age 65 since being placed on the potential confirm annuity eligibility list and the individual’s birth date 
		and death date were not changed since being placed on the potential confirm annuity eligibility list
		
	·	there is an annuity payment for the claim and surviving spouse on the annuity account that was entered before the Prepare Annuity Payout module 
	   became effective and the individual’s birth date or death date were changed since being placed on the potential confirm annuity eligibility list. 
	Refer to Rationale

	*/
		
	IF NOT lb_is_65 OR lb_deceased_before_65th_bday THEN
		MessageBox('Calculation Error','The calculation must not have a calculation reason of ‘Payout – Attained Age 65’, if the individual ' &
		                             + 'on the annuity account is less than  65 years old and did not die prior to attaining age 65. ' &
											  + 'Select another reason or cancel the calculation.', StopSign!)
		RETURN -1
	END IF
	
	IF lb_last_quarter_annuity_rate_exists = FALSE THEN
		IF IsNull(ldt_annuity_end_date) THEN
			// do not refer to the annuity eligibility period if the individual is not eligble
			MessageBox('Calculation Error','The calculation must not have a calculation reason of ‘Payout – Attained Age 65’. ' &
												  + 'Select another reason or cancel the calculation.', StopSign!)
			RETURN -1
		ELSE
			MessageBox('Calculation Error','The calculation must not have a calculation reason of ‘Payout – Attained Age 65’, if ' &
												  + 'the interest rate for the last quarter of the annuity eligibility period is not available. ' &
												  + 'Select another reason or cancel the calculation.', StopSign!)
			RETURN -1
		END IF
	END IF
	
	IF astr_calc_reason_data.claim_role_code = 'C' AND lb_pre_annuity_payout_rollout_payment = FALSE AND astr_calc_reason_data.annuity_payout_no = 0 THEN
		MessageBox('Calculation Error','The calculation must not have a calculation reason of ‘Payout – Attained Age 65’, if ' &
		                             + 'there is not a post-1992 annuity payment for the injured worker on the annuity account that was entered ' &
		                             + 'before the Prepare Annuity Payout module became effective and the calculation is not being done for payout ' &
											  + 'purposes via Confirm Annuity Eligibility – Payout checklist. '&
											  + 'Select another reason or cancel the calculation.', StopSign!)
		RETURN -1
	END IF
	
	IF astr_calc_reason_data.claim_role_code = 'C' AND &
	     lb_pre_annuity_payout_rollout_payment = TRUE AND &
		  ldtm_65_birthday > ldtm_original_annuity_eligibility_create_date AND &
		  lb_birth_date_change = FALSE AND &
		  lb_death_date_change = FALSE THEN
		MessageBox('Calculation Error','The calculation must not have a calculation reason of ‘Payout – Attained Age 65’, if ' &
		                             + 'there is a post-1992 annuity payment for the injured worker on the annuity account that was entered ' &
		                             + 'before the Prepare Annuity Payout module became effective and the individual attained age 65 since being placed on ' &	   
		                             + 'the potential confirm annuity eligibility list and the individual’s birth date and death date were not changed since being placed on ' &
											  + 'the potential confirm annuity eligibility list. '&
											  + 'Select another reason or cancel the calculation.', StopSign!)
		RETURN -1
	END IF
	
	IF astr_calc_reason_data.claim_role_code = 'C' AND lb_pre_annuity_payout_rollout_payment = TRUE AND ( lb_birth_date_change = TRUE OR lb_death_date_change = TRUE ) THEN
		MessageBox('Calculation Error','The calculation must not have a calculation reason of ‘Payout – Attained Age 65’, if ' &
		                             + 'there is a post-1992 annuity payment for the injured worker on the annuity account that was entered ' &
	                                + 'before the Prepare Annuity Payout module became effective and the individual’s birth date or death date was changed since being placed on ' &
		                             + 'the potential confirm annuity eligibility. ' &
											  + 'Select another reason or cancel the calculation.', StopSign!)
		RETURN -1
	END IF
	
	IF astr_calc_reason_data.claim_role_code = 'SS' AND lb_pre_annuity_payout_rollout_payment = FALSE AND astr_calc_reason_data.annuity_payout_no = 0 THEN
		MessageBox('Calculation Error','The calculation must not have a calculation reason of ‘Payout – Attained Age 65’, if ' &
		                             + 'there is not an annuity payment for the claim and surviving spouse on the annuity account that was entered ' &
											  + 'before the Prepare Annuity Payout module became effective and the calculation is not being done for payout ' &
											  + 'purposes via Confirm Annuity Eligibility – Payout checklist. ' &
											  + 'Select another reason or cancel the calculation.', StopSign!)
		RETURN -1
	END IF

	IF astr_calc_reason_data.claim_role_code = 'SS' AND &
	     lb_pre_annuity_payout_rollout_payment = TRUE AND &
		  ldtm_65_birthday > ldtm_original_annuity_eligibility_create_date AND &
		  lb_birth_date_change = FALSE AND &
		  lb_death_date_change = FALSE THEN
		MessageBox('Calculation Error','The calculation must not have a calculation reason of ‘Payout – Attained Age 65’, if ' &
		                             + 'there is an annuity payment for the claim and surviving spouse on the annuity account that was entered ' &
											  + 'before the Prepare Annuity Payout module became effective and the individual attained age 65 since being placed on ' &
											  + 'the potential confirm annuity eligibility list and the individual’s birth date and death date were not changed since being placed on ' &
											  + 'the potential confirm annuity eligibility list. ' &
											  + 'Select another reason or cancel the calculation.', StopSign!)
		RETURN -1
	END IF

	IF astr_calc_reason_data.claim_role_code = 'SS' AND lb_pre_annuity_payout_rollout_payment = TRUE AND ( lb_birth_date_change = TRUE OR lb_death_date_change = TRUE ) THEN
		MessageBox('Calculation Error','The calculation must not have a calculation reason of ‘Payout – Attained Age 65’, if ' &
		                             + 'there is an annuity payment for the claim and surviving spouse on the annuity account that was entered ' &
											  + 'before the Prepare Annuity Payout module became effective and the individual’s birth date or death date was changed since being placed on ' &
											  + 'the potential confirm annuity eligibility list so the individual is at least 65 years old. ' &
											  + 'Select another reason or cancel the calculation.', StopSign!)
		RETURN -1
	END IF
END IF


/*-------------------*/
// Payout - Deceased

IF as_annuity_calc_reason_code = '05' THEN
	/*
	
	BR 1.163
	The calculation must not have a calculation reason of ‘Payout – Deceased Prior to Age 65’, if any one of the following is true:
	·	the individual on the annuity account did not die prior to attaining age 65
	·	the interest rate for the last quarter of the annuity eligibility period is not available
	
	·	there is not a post-1992 annuity payment for the injured worker on the annuity account that was entered before the Prepare Annuity Payout module 
	   became effective and the calculation is not being done for payout purposes via Confirm Annuity Eligibility – Payout checklist
	·	there is a post-1992 annuity payment for the injured worker on the annuity account that was entered before the Prepare Annuity Payout module 
	   became effective and the individual’s birth or death date was changed since being placed on the potential confirm annuity eligibility list 
				
	·	there is not an annuity payment for the claim and surviving spouse on the annuity account that was entered before the Prepare Annuity Payout module 
	   became effective and the calculation is not being done for payout purposes via Confirm Annuity Eligibility – Payout checklist
	·	there is an annuity payment for the claim and surviving spouse on the annuity account that was entered before the Prepare Annuity Payout module 
	   became effective and the individual’s birth or death date was changed since being placed on the potential confirm annuity eligibility list 
		
	Refer to Rationale

	*/
	
	IF IsNull(ldtm_death_date) OR NOT lb_deceased_before_65th_bday THEN
		MessageBox('Calculation Error','The calculation must not have a calculation reason of ‘Payout – Deceased Prior to Age 65’, if ' &
		                             + 'the individual on the annuity account did not die prior to attaining age 65.',StopSign!)
		RETURN -1
	END IF
	
	IF lb_last_quarter_annuity_rate_exists = FALSE THEN
		IF IsNull(ldt_annuity_end_date) THEN
			// do not refer to the annuity eligibility period if the individual is not eligble
			MessageBox('Calculation Error','The calculation must not have a calculation reason of ‘Payout – Deceased Prior to Age 65’. ' &
												 + 'Select another reason or cancel the calculation.',StopSign!)
			RETURN -1
		ELSE
			MessageBox('Calculation Error','The calculation must not have a calculation reason of ‘Payout – Deceased Prior to Age 65’, if ' &
												 + 'the interest rate for the last quarter of the annuity eligibility period is not available. ' &
												 + 'Select another reason or cancel the calculation.',StopSign!)
			RETURN -1
		END IF
	END IF
	
	IF astr_calc_reason_data.claim_role_code = 'C' AND lb_pre_annuity_payout_rollout_payment = FALSE AND astr_calc_reason_data.annuity_payout_no = 0 THEN
		MessageBox('Calculation Error','The calculation must not have a calculation reason of ‘Payout – Deceased Prior to Age 65’, if ' &
		                             + 'there is not a post-1992 annuity payment for the injured worker on the annuity account that was entered ' &
		                             + 'before the Prepare Annuity Payout module became effective and the calculation is not being done for payout ' &
											  + 'purposes via Confirm Annuity Eligibility – Payout checklist. '&
											  + 'Select another reason or cancel the calculation.', StopSign!)
		RETURN -1
	END IF
	
	IF astr_calc_reason_data.claim_role_code = 'C' AND lb_pre_annuity_payout_rollout_payment = TRUE AND ( lb_birth_date_change = TRUE OR lb_death_date_change = TRUE ) THEN
		MessageBox('Calculation Error','The calculation must not have a calculation reason of ‘Payout – Deceased Prior to Age 65’, if ' &
		                             + 'there is a post-1992 annuity payment for the injured worker on the annuity account that was entered ' &
											  + 'before the Prepare Annuity Payout module became effective and the individual’s birth or death date was changed ' &
											  + 'since being placed on the potential confirm annuity eligibility list. '&
											  + 'Select another reason or cancel the calculation.', StopSign!)
		RETURN -1
	END IF
		
	IF astr_calc_reason_data.claim_role_code = 'SS' AND lb_pre_annuity_payout_rollout_payment = FALSE AND astr_calc_reason_data.annuity_payout_no = 0 THEN
		MessageBox('Calculation Error','The calculation must not have a calculation reason of ‘Payout – Deceased Prior to Age 65’, if ' &
		                             + 'there is not an annuity payment for the claim and surviving spouse on the annuity account that was entered ' &
											  + 'before the Prepare Annuity Payout module became effective and the calculation is not being done for payout ' &
											  + 'purposes via Confirm Annuity Eligibility – Payout checklist. '&
											  + 'Select another reason or cancel the calculation.', StopSign!)
		RETURN -1
	END IF
		
	IF astr_calc_reason_data.claim_role_code = 'SS' AND lb_pre_annuity_payout_rollout_payment = TRUE AND ( lb_birth_date_change = TRUE OR lb_death_date_change = TRUE ) THEN
		MessageBox('Calculation Error','The calculation must not have a calculation reason of ‘Payout – Deceased Prior to Age 65’, if ' &
		                             + 'there is an annuity payment for the claim and surviving spouse on the annuity account that was entered ' &
											  + 'before the Prepare Annuity Payout module became effective and the individual’s birth or death date was changed ' &
											  + 'since being placed on the potential confirm annuity eligibility list. '&
											  + 'Select another reason or cancel the calculation.', StopSign!)	
		RETURN -1
	END IF
	
END IF



/*-----------------------------------------*/
// Surviving Spouse - 1st Year of Benefits

IF as_annuity_calc_reason_code = '08' THEN
	/*
	BR 1.170
	The individual on the annuity account must be a surviving spouse, 
	if the calculation reason is "Surviving Spouse - 1st Year of Benefits".
	*/
	IF astr_calc_reason_data.claim_role_code <> 'SS' THEN
		MessageBox('Calculation Error','This individual is not a surviving spouse.' &
										+ '~r~n~r~nSelect another reason or cancel the calculation.',StopSign!)
		RETURN -1
	END IF
END IF

/*--------------------------*/
//Eligibility Change (payout)

IF as_annuity_calc_reason_code = '13' THEN
	// NOT For payout
	IF astr_calc_reason_data.annuity_payout_no = 0 THEN
		MessageBox('Calculation Error','This calculation is not for annuity payout.' &
										+ '~r~n~r~nSelect another reason or cancel the calculation.',StopSign!)
		RETURN -1
	ELSE
		IF ls_annuity_eligibility_status_code = 'A' AND ldtm_current >= astr_calc_reason_data.annuity_end_date THEN
			MessageBox('Calculation Error','This calculation is for annuity payout, but the individual is eligible for payout and the annutiy end date is not in the future.' &
											+ '~r~n~r~nSelect another reason or cancel the calculation.',StopSign!)
			RETURN -1
		END IF
	END IF
	
END IF

RETURN 0
end function

on n_calc_annuity.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_calc_annuity.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

