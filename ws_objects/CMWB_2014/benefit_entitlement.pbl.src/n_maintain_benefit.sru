$PBExportHeader$n_maintain_benefit.sru
$PBExportComments$used to hold functionality for the maintain/verify benefit module
forward
global type n_maintain_benefit from n_pdc
end type
end forward

global type n_maintain_benefit from n_pdc
end type
global n_maintain_benefit n_maintain_benefit

type variables

end variables

forward prototypes
public function integer nf_check_benefit_applicable (string as_claim_role_code)
public function integer nf_check_for_ss_number (long al_individual_no, string as_claim_role_code)
public function integer nf_check_payment_fully_adjusted (long al_payment_no)
public function integer nf_check_associated_benefit_entitlement (long al_individual_no, long al_claim_no, string as_case)
public function integer nf_check_benefit_attracts_annuity (string as_claim_role_code, string as_opening_type)
public function integer nf_check_opening_for_claim (long al_claim_no, integer ai_opening_no)
public function integer nf_check_valid_payment_type (string as_payment_type)
public function integer nf_check_valid_date (datetime adtm_date_to_check)
public function integer nf_check_benefit_period_days (datetime adtm_from_date, datetime adtm_to_date, long ai_number_of_days)
public function integer nf_check_annuity_one_day_rule (datetime adtm_period_to, datetime adtm_annuity_end)
public function decimal nf_calculate_amount (decimal adc_total_amount, decimal adc_calculate_amount, integer ai_type)
public function long nf_get_next_entitlement_no (long al_number)
public function integer nf_count_selected_rows (datawindow adw_dw)
public function integer nf_check_claim_participant (long al_individual_no, long al_claim_no, string as_case)
public function string nf_get_opening_type (long al_claim_no, long al_opening_no)
public function integer nf_check_payment_type (string as_payment_type, u_dw_online au_dw_online)
public function integer nf_examine_benefit_entitlement (long al_annuity_account_no, string as_claim_role_code, date adt_be_from, date adt_be_to, integer ai_case)
public function integer nf_check_calculation_for_payment (long al_benefit_calculation_no, u_dw_online adw_payment)
public function integer nf_check_if_receiving_salary_payment (long al_payment_no, long al_claim_no)
public function string nf_get_ben_freq_code (long al_claim_no, long al_ben_calc_no, long al_opening_no)
public function integer nf_check_for_opening (integer ai_opening_no, long al_claim_no)
public function integer nf_check_for_benefit_calculation (long al_calculation_no, long al_claim_no, long al_opening_no)
public function integer nf_check_payment_type_with_opening (string as_payment_type, string as_opening_type)
public function integer nf_payment_type_attracts_annuity (string as_claim_role_code, string as_payment_type)
public function integer nf_check_dates_assorted (date adt_from_date, date adt_to_date, integer ai_case)
public function integer nf_check_benefit_calculation_dates (date adt_check_date, long al_benefit_calculation_no, long al_claim_no, long al_opening_no, integer ai_case)
public function boolean nf_entitlement_xref_exists (long al_account_no, long al_benefit_entitlement_no)
public function integer nf_check_opening_with_bencalc (long al_benefit_calculation_no, integer ai_opening_no, long al_claim_no)
public function datetime nf_get_bencalc_eff_date (long al_calculation_no, long al_claim_no, long al_opening_no)
public function string nf_get_benefit_category_code (string as_payment_type)
public function integer nf_check_span_annuity_interest_period (datetime adtm_from, datetime adtm_to)
public function integer nf_check_partial_span_annuity_interest (datetime adtm_from, datetime adtm_to)
public function integer nf_check_benefit_entitlement_dates (date adt_check_date, long al_opening_no, long al_claim_no, string as_opening_type, string as_payment_type, integer ai_case)
public function boolean nf_record_exists_in_xref (long al_annuity_account_no, long al_annuity_eligibility_no, long al_benefit_entitlement_no)
public function integer nf_get_claimant_active_flag (long al_individual_no, string as_claim_role_code)
public function boolean nf_check_payment_type_claim_role (string as_payment_type, string as_claim_role)
public function date nf_get_claim_accident_date (long al_claim_no)
public function decimal nf_get_bc_award_amount (long al_claim_no, long al_benefit_calculation_no, long al_opening_no)
public function decimal nf_calculate_3_fifths (decimal adec_check_value)
public function datetime nf_get_effective_date (long al_claim_no, long al_ben_calc_no, long al_opening_no)
public function string nf_get_freq_from_payment_type (string as_payment_type)
public function datastore nf_split_payment_for_interest_period (date adtm_from, date adtm_to, string as_frequency)
public function string nf_check_span_start_date (date adt_from_date, date adt_to_date, datetime adtm_annuity_start_date, datetime adtm_annuity_end_date, integer ai_case)
public function integer nf_calculate_relative_days (datetime adtm_date_from, datetime adtm_date_to)
public subroutine nf_get_opening_dates (long al_claim_no, long al_opening_no, string as_opening_type, ref datetime adt_check_start, ref datetime adt_check_end)
public function integer nf_check_entitlement_overlap (datetime adtm_from_date, datetime adtm_to_date, long al_claim_no, integer al_row_no, u_dw_online a_udw_online, string as_payment_type_code, string as_opening_type, long al_opening_no, string as_claim_role_code)
public function integer nf_get_annuity_dates (long al_annuity_account_no, long al_claim_no, long al_individual_no, string as_type, ref datetime adtm_annuity_start_date, ref datetime adtm_annuity_end_date)
public function integer nf_check_opening_covers_period (long al_claim_no, integer ai_openings[], datetime adtm_benefit_start, datetime adtm_benefit_end)
public function string nf_get_opening_type_from_combination (string as_payment_type)
public function integer nf_check_benefit_calculations_days (integer ai_days, integer ai_hours, integer ai_weeks, datetime adtm_payment_from, datetime adtm_payment_to, long al_claim_no, long al_opening_no, long al_bencalc_no)
public function boolean nf_check_benefit_level_percent (string as_payment_type, long al_benefit_calculation_no, long al_claim_no)
public function integer nf_check_paytype_requires_days (string as_payment_type)
end prototypes

public function integer nf_check_benefit_applicable (string as_claim_role_code);/* Ø	Benefit entitlement must only be applicable to injured workers and surviving spouses.
*/

CHOOSE CASE as_claim_role_code
	CASE 'SS','C'
		//okay
	CASE ELSE
		RETURN -1
END CHOOSE

RETURN 1
end function

public function integer nf_check_for_ss_number (long al_individual_no, string as_claim_role_code);/*
An injured worker must have a Social Insurance Number.
A surviving spouse should have a Social Insurance Number.  

RETURNS 
1 	- 	ALL IS GOOD
0 	- 	SS No Sin Number - is a should not a must
-1 	-	BR violation Message and Error 

*/
LONG  ll_sin_no

SELECT 	sin_no
INTO 		:ll_sin_no
FROM 	INDIVIDUAL
WHERE 	individual_no = :al_individual_no
USING 	SQLCA;

SQLCA.nf_handle_error('N_maintain_benefit','nf_check_for_ss_number()','SELECT sin_no ')


IF ISNULL(ll_sin_no) OR ll_sin_no < 1 THEN
	
	CHOOSE CASE as_claim_role_code
		CASE 'SS'
			RETURN 0
		CASE 'C'
			RETURN -1
		CASE ELSE
	END CHOOSE
END IF 

RETURN 1
end function

public function integer nf_check_payment_fully_adjusted (long al_payment_no);/*
An entitlement should not be created from a payment that has been fully adjusted to zero.
any payment????????

returns 1 -- all is good
return 0 -- error 

*/
LONG ll_num

SELECT COUNT(*) 
INTO:ll_num
FROM PAYMENT
WHERE zeroed_flag 	= 'Y'
AND payment_no  		= :al_payment_no
USING SQLCA;

SQLCA.nf_handle_error('n_maintain_benefit','nf_check_payment_fully_adjusted()','SELECT COUNT(*)')

IF ll_num <= 0 THEN RETURN 0

RETURN 1
end function

public function integer nf_check_associated_benefit_entitlement (long al_individual_no, long al_claim_no, string as_case);/*
A claim on which the individual is a claimant must be associated with a benefit entitlement 
if the annuity account is for an injured worker.

The claim on which the individual is a surviving spouse must be associated with a 
benefit entitlement if the annuity account is for a surviving spouse.

calling code will determine if the individual is a an account for an injured worker
if they are then this function will be called

RETURNS -1 if nothing is found

ai_case for future use calling code will determine SS or C

*/
LONG ll_num

SELECT COUNT(*) INTO :ll_num FROM CLAIM_PARTICIPANT 
WHERE individual_no 	= :al_individual_no
AND claim_no 			= :al_claim_no
USING 	SQLCA;

SQLCA.nf_handle_error('n_maintain_benefit','nf_check_associated_benefit_entitlement()','SELECT')

//CODE IF NEEDED
CHOOSE CASE as_case
	CASE 'SS'
	CASE 'C'
	CASE ELSE
END CHOOSE

IF ll_num <= 0 THEN 	RETURN -1
		
RETURN 1
end function

public function integer nf_check_benefit_attracts_annuity (string as_claim_role_code, string as_opening_type);/*
Only openings of types associated with benefits that attract annuities must be available for all claims 
of the injured worker if the annuity account is for a claimant.
Only openings of survivor benefit types must be available for the claim on which the individual is 
a surviving spouse if the annuity account is for a surviving spouse.

RETURNS -1 if nothing is found

as_case for future use calling code will determine SS or C

*/

LONG ll_num

//CODE IF NEEDED
CHOOSE CASE as_claim_role_code
	CASE 'SS'
		
		//Only openings of survivor benefit types must be available for the claim on which the individual is 
         //a surviving spouse if the annuity account is for a surviving spouse.
		SELECT count(*) 
		INTO :ll_num 
		FROM  Claim_Role_Opening_Type_Xref 
		WHERE claim_role_code 			= 'SS'
		AND   opening_type_code 		= :as_opening_type
		AND   annuity_eligibility_flag 	= 'Y'
		USING 	SQLCA;	
			
		SQLCA.nf_handle_error('n_maintain_benefit','nf_check_benefit_attracts_annuity()','SELECT count(*) For SS')
		
	CASE 'C'
		
		//Only openings of types associated with benefits that attract annuities must be available for all claims 
         //of the injured worker if the annuity account is for a claimant.
			
		SELECT count(*) 
		INTO :ll_num 
		FROM  Claim_Role_Opening_Type_Xref 
		WHERE claim_role_code 			= 'C'
		AND   opening_type_code 		= :as_opening_type
		AND   annuity_eligibility_flag 	= 'Y'
		USING 	SQLCA;

		SQLCA.nf_handle_error('n_maintain_benefit','nf_check_benefit_attracts_annuity()','SELECT count(*) - For C')
		
	CASE ELSE
END CHOOSE

IF ll_num <= 0 THEN 	RETURN -1
		
RETURN 1
end function

public function integer nf_check_opening_for_claim (long al_claim_no, integer ai_opening_no);/*
The opening must be an opening for the claim associated with the benefit entitlement.

RETURNS -1 if nothing is found
*/

LONG ll_num

SELECT 	count(*) 
INTO 		:ll_num 
FROM 	OPENING 
WHERE 	claim_no 		= :al_claim_no
AND 		opening_no 		= :ai_opening_no
USING 	SQLCA;
		
SQLCA.nf_handle_error('n_maintain_benefit','nf_check_opening_for_claim()','SELECT count(*) ')
		
IF ll_num <= 0 THEN 	RETURN -1
		
RETURN 1
end function

public function integer nf_check_valid_payment_type (string as_payment_type);/*
Check that the payment type is  avalid payment type.

RETURNS -1 if nothing is found
*/

LONG ll_num

SELECT 	count(*) 
INTO 		:ll_num 
FROM 	Payment_Type 
WHERE 	payment_type_code 		= :as_payment_type
USING 	SQLCA;
		
SQLCA.nf_handle_error('n_maintain_benefit','nf_check_valid_payment_type()','SELECT count(*) ')
		
IF ll_num <= 0 THEN 	RETURN -1
		
RETURN 1
end function

public function integer nf_check_valid_date (datetime adtm_date_to_check);/*
These will be common date checks handled in calling code

A benefit entitlement must have a period from date.
A benefit entitlement must have a period to date.

RETURNS -1 if not valid
*/

IF IsNull(adtm_date_to_check) OR STRING(adtm_date_to_check,'YYYY-MM-DD') = '1900-01-01'  OR DATE(adtm_date_to_check) > Date('2079-06-06') OR DATE(adtm_date_to_check) < Date('1900-01-01') THEN 
	RETURN -1
END IF 	

RETURN 1
end function

public function integer nf_check_benefit_period_days (datetime adtm_from_date, datetime adtm_to_date, long ai_number_of_days);/*
The benefit entitlement period must include all days from the period from date up to but not including the period to date.

This statement returns 4:

DaysAfter(2002-12-20, 2002-12-24)

RETURNS -1 if not valid
*/

IF DaysAfter(date(adtm_from_date), date(adtm_to_date)) <> (ai_number_of_days - 1) THEN RETURN -1
		
RETURN 1
end function

public function integer nf_check_annuity_one_day_rule (datetime adtm_period_to, datetime adtm_annuity_end);/*
A benefit entitlement with a period to date one day after the annuity end date must be included in the annuity set aside.

RETURNS -1 if not valid - in this case needs to be included in annuity set aside

what is an annuity set aside?
*/

IF DaysAfter(date(adtm_period_to), date(adtm_annuity_end)) = -1 THEN RETURN -1
		
RETURN 1
end function

public function decimal nf_calculate_amount (decimal adc_total_amount, decimal adc_calculate_amount, integer ai_type);/*
*/
decimal {2} ldc_return

CHOOSE CASE ai_type
	CASE 1//DAILY
		ldc_return = adc_total_amount / adc_calculate_amount
		
	CASE 2//WEEKLY
		
	CASE 3//MONTHLY
		
	CASE 4//YEARLY
		
	CASE ELSE
END CHOOSE
		
RETURN 1
end function

public function long nf_get_next_entitlement_no (long al_number);LONG			ll_benefit_entitlement_no

/*	To ensure that we get the next number without, Update the Last_Benefit_Entitlement_No table incrementing the  
	last_no by 1  (This will lock it so no one else can get in). Then, read it back                 
*/
UPDATE	Last_Benefit_Entitlement_No
SET		last_benefit_entitlement_no = last_benefit_entitlement_no + :al_number
USING 	SQLCA;
SQLCA.nf_handle_error("Embedded SQL: Update Last_Annuity_Account_No","w_confirm_annuity_eligibility","wf_get_next_annuity_account_no")
	
CHOOSE CASE SQLCA.SQLNRows
/*		If update was successful (ie. SQLNRows would equal 1), read back the identifier
*/
	CASE 1
			
		SELECT	last_benefit_entitlement_no
		INTO		:ll_benefit_entitlement_no
		FROM		Last_Benefit_Entitlement_No
		USING 	SQLCA;
		SQLCA.nf_handle_error("Embedded SQL: SELECT Last_Annuity_Account_No","w_confirm_annuity_eligibility","wf_get_next_annuity_account_no")
			
	CASE ELSE
/*		If anything other than 1 record found, display error
*/
		MessageBox("Data Integrity Error", string(SQLCA.SQLNRows) + " record(s) found in Last_Benefit_Entitlement_No~r~nPlease call the help desk",Exclamation!)
		SQLCA.nf_rollback_transaction()
		IF SQLCA.SQLCode <> 0 THEN
			Error.Text 				= "Error during rollback of Last_Benefit_Entitlement_No in function nf_get_next_entitlement_no"
			Error.WindowMenu	=""
			Error.Object				=""
			Error.ObjectEvent		="nf_get_next_entitlement_no"
			SignalError()
		END IF		
		RETURN -1

	END CHOOSE

RETURN ll_benefit_entitlement_no
end function

public function integer nf_count_selected_rows (datawindow adw_dw);//#===================================================================
//# public function long nf_count_selected_rows ( datawindow adw_dw)
//# 
//# Description:
//#	count the total selected rows in datawindow
//#
//# Example: 
//#	ll_count = nf_count_selected_rows(dw_list)
//#
//#===================================================================

IF NOT isvalid(adw_dw) THEN RETURN -1
RETURN long(adw_dw.DESCRIBE("Evaluate('sum(if(isselected(),1,0))',0)"))

end function

public function integer nf_check_claim_participant (long al_individual_no, long al_claim_no, string as_case);/*
A claim on which the individual is a claimant must be associated with a benefit entitlement 
if the annuity account is for an injured worker.

The claim on which the individual is a surviving spouse must be associated with a 
benefit entitlement if the annuity account is for a surviving spouse.

calling code will determine if the individual is a an account for an injured worker
if they are then this function will be called

RETURNS -1 if nothing is found

ai_case for future use calling code will determine SS or C
*/
LONG ll_num

SELECT 	COUNT(*) INTO :ll_num FROM CLAIM_PARTICIPANT
WHERE 	individual_no 			= :al_individual_no
AND 		claim_no 				= :al_claim_no
AND 		claimant_active_flag 	= 'Y'
AND       claim_role_code         = :as_case
USING 	SQLCA;

SQLCA.nf_handle_error('n_maintain_benefit','nf_check_claim_participant()','SELECT')

IF ll_num <= 0 THEN 	RETURN -1
		
RETURN 1
end function

public function string nf_get_opening_type (long al_claim_no, long al_opening_no);/* RETURN THE opening_type_code if one does not exist for this record */
STRING	ls_type_code

SELECT opening_type_code  INTO :ls_type_code
FROM   OPENING 
WHERE claim_no 		= :al_claim_no
AND     opening_no 	= :al_opening_no
USING  SQLCA;

SQLCA.nf_handle_error('n_maintain_benefit','nf_get_opening_type()','SELECT opening_type_code  INTO :ls_type_code')

IF ISNULL(ls_type_code) THEN ls_type_code = ''

RETURN ls_type_code
end function

public function integer nf_check_payment_type (string as_payment_type, u_dw_online au_dw_online);/*
A payment of the same type as the benefit entitlement should exist. 
any payment

returns 1 -- all is good
return -1 -- error (not applicable)
return 0 - is a should so no payment exists but is not an error
*/

INTEGER li_count
STRING 	DWfilter2

DWfilter2 = "payment_type_code = '" + as_payment_type + "'"

IF ISNULL(DWfilter2) THEN RETURN 0//SHOULD we have a check for valid values?

au_dw_online.SetFilter(DWfilter2)

au_dw_online.Filter()
li_count = au_dw_online.rowcount()

//reset the filter
au_dw_online.SetFilter('')
au_dw_online.Filter()

IF li_count <= 0 THEN RETURN 0

RETURN 1
end function

public function integer nf_examine_benefit_entitlement (long al_annuity_account_no, string as_claim_role_code, date adt_be_from, date adt_be_to, integer ai_case);/* Kevin's function - going to be used for the following

	Ø	A benefit entitlement period must not span the annuity start date. 
	Ø	A benefit entitlement period must not span the annuity end date.
	
	CASE 1 = from date
	CASE 2 = to date
		
	// benefit_entitlement_from_date, benefit_entitlement_to_date (from dataobjects)

*/

U_DS		lds_reexamine_benefit_entitlement
LONG		ll_benefit_entitlement_rowcount, ll_annuity_eligibility_rowcount, ll_num
STRING	ls_annuity_eligibility_reason_code, DWfilter2, ls_date_from, ls_date_to
INTEGER li_count
	
	
lds_reexamine_benefit_entitlement = Create U_DS
	
IF as_claim_role_code = 'C' THEN
		
	lds_reexamine_benefit_entitlement.DataObject = 'ds_reexamine_benefit_entitlement_IW'
	lds_reexamine_benefit_entitlement.SetTransObject(SQLCA)
		
	ll_benefit_entitlement_rowcount = lds_reexamine_benefit_entitlement.Retrieve(al_annuity_account_no)
	SQLCA.nf_handle_error('n_maintain_benefit', 'nf_examine_benefit_entitlement', 'lds_reexamine_benefit_entitlement.Retrieve')
		
ELSEIF as_claim_role_code = 'SS' THEN
	
	lds_reexamine_benefit_entitlement.DataObject = 'ds_examine_benefit_entitlement_SS'
	lds_reexamine_benefit_entitlement.SetTransObject(SQLCA)
		
	ll_benefit_entitlement_rowcount = lds_reexamine_benefit_entitlement.Retrieve(al_annuity_account_no)
	SQLCA.nf_handle_error('n_maintain_benefit', 'nf_examine_benefit_entitlement', 'ids_reexamine_benefit_entitlement.Retrieve')
	
END IF

//IF NO ROWS DON'T GO ANY FURTHUR
IF ll_benefit_entitlement_rowcount = 0 THEN RETURN 0

//grab the dates to check	
ls_date_from = String(adt_be_from,'yyyy-mm-dd') 
ls_date_to 	  = String(adt_be_to,'yyyy-mm-dd') 

CHOOSE CASE  ai_case 
	CASE 1 // A benefit entitlement period must not span the annuity start date. 
		
			DWfilter2 = "benefit_entitlement_from_date >= " + ls_date_from  +  " and  benefit_entitlement_from_date <= " + ls_date_to 

	CASE 2 // A benefit entitlement period must not span the annuity end date.
		
			DWfilter2 = "benefit_entitlement_to_date >= " + ls_date_from  +  " and  benefit_entitlement_to_date <= " + ls_date_to 
		
END CHOOSE

//RUN THE FILTER - calling code will know what to do with the results	
IF ISNULL(DWfilter2) THEN RETURN 0
	
lds_reexamine_benefit_entitlement.SetFilter(DWfilter2)
	
li_count = lds_reexamine_benefit_entitlement.Filter()
	
IF li_count <= 0 THEN RETURN 0
IF li_count > 0   THEN RETURN -1
	
RETURN 1
end function

public function integer nf_check_calculation_for_payment (long al_benefit_calculation_no, u_dw_online adw_payment);/*
The benefit calculation associated with the benefit entitlement should also be associated with a payment. 

RETURNS -1 if nothing is found

*/

LONG 		ll_benefit_calculation_no
INTEGER li_rowcount, li_counter

// filter out all of the payments for ones that have a benefit_calculation_no = the one entered
li_rowcount = adw_payment.rowcount()

IF li_rowcount = 0  THEN RETURN 0

FOR li_counter = 1 TO li_rowcount
	ll_benefit_calculation_no = adw_payment.getitemnumber(li_counter,'benefit_calculation_no')
	
	IF ll_benefit_calculation_no = al_benefit_calculation_no  THEN
		RETURN 1
	END IF 
NEXT
		
RETURN 0
end function

public function integer nf_check_if_receiving_salary_payment (long al_payment_no, long al_claim_no);/*
Benefit entitlement must not be created from a receiving salary payment.

What is a receiving salaray payment????????? -- based on the CLAIM?

SHOULD THIS BE SELECTABLE THEN???
I have no link back to the payment

RETURNS -1 if nothing is found

*/

LONG ll_num

SELECT 	count(*) 
INTO 		:ll_num 
FROM 	PAYMENT a
JOIN       APPLIED_CLAIM_TXN b ON a.payment_no = b.payment_no AND a.claim_no  = b.claim_no
WHERE 	b.payment_method_code 	= 'R'
AND 		a.payment_no 					= :al_payment_no
AND       a.claim_no                         = :al_claim_no
USING 	SQLCA;
		
SQLCA.nf_handle_error('n_maintain_benefit','nf_check_if_receiving_salary_payment()','SELECT count(*) ')
		
IF ll_num > 0 THEN 	RETURN -1
		
RETURN 1
end function

public function string nf_get_ben_freq_code (long al_claim_no, long al_ben_calc_no, long al_opening_no);//grab the payment freq code from the provided information.
STRING ls_freq_code

SELECT 	award_freq_code 
INTO 		:ls_freq_code 
FROM 	BENEFIT_CALCULATION
WHERE 	claim_no 					= :al_claim_no
AND 		benefit_calculation_no 	= :al_ben_calc_no
AND 		opening_no 					= :al_opening_no
USING 	SQLCA;

SQLCA.nf_handle_error("n_maintain_benefit","nf_get_ben_freq_code()","SELECT award_freq_code ")

IF ISNULL(ls_freq_code) THEN ls_freq_code = ''

RETURN ls_freq_code
end function

public function integer nf_check_for_opening (integer ai_opening_no, long al_claim_no);/*
An Opening must be associated with a benefit entitlement.

RETURNS -1 if nothing is found

*/

LONG ll_num

SELECT 	count(*) 
INTO 		:ll_num 
FROM 	OPENING 
WHERE 	opening_no 	= :ai_opening_no
AND       claim_no 		= :al_claim_no
USING 	SQLCA;
		
SQLCA.nf_handle_error('n_maintain_benefit','nf_check_for_opening()','SELECT count(*) ')
		
IF ll_num <= 0 THEN 	RETURN -1
		
RETURN 1
end function

public function integer nf_check_for_benefit_calculation (long al_calculation_no, long al_claim_no, long al_opening_no);/*
A Benefit Calculation must be associated with a benefit entitlement.

RETURNS -1 if nothing is found
*/

LONG ll_num

SELECT 	count(*) 
INTO 		:ll_num 
FROM 	BENEFIT_CALCULATION 
WHERE 	benefit_calculation_no 	= :al_calculation_no
AND       claim_no						= :al_claim_no
AND       opening_no					= :al_opening_no
USING 	SQLCA;
		
SQLCA.nf_handle_error('n_maintain_benefit','nf_check_for_benefit_calculation()','SELECT count(*) ')
		
IF ll_num <= 0 THEN 	RETURN -1
		
RETURN 1
end function

public function integer nf_check_payment_type_with_opening (string as_payment_type, string as_opening_type);/*
The payment type must be valid for the associated opening.

RETURNS -1 if nothing is found
*/

LONG ll_num

SELECT 	count(*) 
INTO 		:ll_num 
FROM 	OPENING a 
INNER JOIN Payment_Combination b ON a.opening_type_code = b.opening_type_code
WHERE 	payment_type_code 	= :as_payment_type
AND		a.opening_type_code = :as_opening_type
USING 	SQLCA;
		
SQLCA.nf_handle_error('n_maintain_benefit','nf_check_payment_type_with_opening()','SELECT count(*) ')
		
IF ll_num <= 0 THEN 	RETURN -1
		
RETURN 1
end function

public function integer nf_payment_type_attracts_annuity (string as_claim_role_code, string as_payment_type);/*
as_claim_role_code = 'C'
The benefit entitlement payment type must be a payment type that attracts annuity for the injured worker 
if the individual on the annuity account is an injured worker.

as_claim_role_code = 'SS'
The benefit entitlement payment type must be a payment type that attracts annuity for a surviving spouse 
if the individual on the annuity account is a surviving spouse.

RETURNS -1 if nothing is found

as_claim_role_code will determine SS or C
*/

LONG ll_num

SELECT count(*) INTO :ll_num FROM Payment_Type 
JOIN Payment_Combination ON Payment_Combination.payment_type_code = Payment_Type.payment_type_code
JOIN Claim_Role_Opening_Type_Xref ON Claim_Role_Opening_Type_Xref.opening_type_code = Payment_Combination.opening_type_code
WHERE 	claim_role_code 									= :as_claim_role_code
AND 		annuity_flag 										= 'Y'
AND		Payment_Combination.payment_type_code 	= :as_payment_type
USING 	SQLCA;
		
SQLCA.nf_handle_error('n_maintain_benefit','nf_payment_type_attracts_annuity()','SELECT count(*)')
	
//for future concideration
CHOOSE CASE as_claim_role_code
	CASE 'C'
	CASE 'SS'	
	CASE ELSE
END CHOOSE

IF ll_num <= 0 THEN 	RETURN -1
		
RETURN 1
end function

public function integer nf_check_dates_assorted (date adt_from_date, date adt_to_date, integer ai_case);/*
(ai_case = 1)
7.120	The benefit entitlement period to date must be greater than the benefit entitlement period from date.
1	“Period” to date < “Period” from date.	Receive an error message indicating the Period to date cannot be earlier 
     than the period from date.		
2	 “Period” to date that > “Period” from date.	Payment created/update with the Period to/from dates entered.		
3	“Period” to date = “Period” from date.	Receive an error message indicating that the period end date cannot
     be less than or equal to the start date.		

(ai_case = 4)
7.150	A benefit entitlement must start on or after January 1st, 1991 if the annuity account is for an injured worker. 
REVISED TO
7.170	A benefit entitlement must start on or after January 1st, 1991 if the annuity account is for an injured worker 
        and the injured worker was not eligible for annuity benefits prior to January 1st, 1993. 

	Scenario	Expected Results	Pass/Fail	Note
1	Create benefit entitlement for an injured worker Set the benefit start date to be Jan 1/91	Allowed		
2	Create benefit entitlement for an injured worker Set the benefit start date to be before Jan 1/91	Not allowed		
3	Create benefit entitlement for an injured worker Set the benefit start date to be after Jan 1/91	Allowed		

(ai_case = 5)
7.180	A benefit entitlement must start on or after January 1st, 1982 if the annuity account is for a surviving spouse.
	Scenario	Expected Results	Pass/Fail	Note
1	Create benefit entitlement for a surviving spouse Set the benefit start date to Jan 1/82	Allowed		May require a proper opening
2	Create benefit entitlement for a surviving spouse Set the benefit start date be prior to Jan 1/82	Not allowed		
3	Create benefit entitlement for a surviving spouse Set the benefit start date to be after Jan 1/82	Allowed	


(AI_CASE = 6)
7.175	A benefit entitlement must start on or after January 1st, 1993 if the annuity account is for an injured worker and 
  the injured worker was eligible for annuity benefits prior to January 1st, 1993.  (See Rationale)

returns 1 -- all is good
return -1 -- error

 'to cover or extend over an area or time period' 
 
 FOR non coded values use '1900-01-01'
*/

CHOOSE CASE ai_case
	CASE 1
		
		IF DATE(adt_from_date) >= DATE(adt_to_date) THEN RETURN -1

	CASE 2
	//	IF DATE(adt_from_date) > DATE(adt_to_date) THEN RETURN -1
		
	CASE 3
		
	CASE 4
		IF DATE(adt_from_date) < date('1991-01-01') THEN RETURN -1
		
	CASE 5
		IF DATE(adt_from_date) < date('1982-01-01') THEN RETURN -1
		
	CASE 6
	IF DATE(adt_from_date) < date('1993-01-01') THEN RETURN -1
		
END CHOOSE
		
RETURN 1
end function

public function integer nf_check_benefit_calculation_dates (date adt_check_date, long al_benefit_calculation_no, long al_claim_no, long al_opening_no, integer ai_case);/*
(ai_case =  1)

 7.90	The benefit entitlement period from date must not be less than the effective date of the associated benefit calculation 
 	for the benefit entitlement.
		
	Scenario	Expected Results	Pass/Fail	Note
1	“Period” from date < effective date of the benefit.	Receive an error emssage indicating the ‘Period’ from date 
	can not be earlier than the effective date of the benefit		
2	“Period” from date = effective date of the benefit.	The payment is created/updated with the Period from  -- date entered		
3	“Period” from date that is > effective date of the benefit.	The payment is created/updated with the Period from -- date entered		

returns 1 -- all is good
return -1 -- error
*/
DATE ldt_check_effective

SELECT 	effective_from_date
INTO   	:ldt_check_effective
FROM 	BENEFIT_CALCULATION
WHERE 	claim_no 					= :al_claim_no
AND     	opening_no 					= :al_opening_no
AND     	benefit_calculation_no 	= :al_benefit_calculation_no
USING  	SQLCA;

SQLCA.nf_handle_error("n_maintain_benefit","nf_check_benefit_calculation_dates()","SELECT benefit_start_date, benefit_end_date")

CHOOSE CASE ai_case
	CASE 1
		//The benefit entitlement period from date must not be less than the effective date of the associated benefit calculation for the benefit entitlement.
		IF 	DATE(adt_check_date) < DATE(ldt_check_effective) THEN RETURN -1	
END CHOOSE
		
RETURN 1
end function

public function boolean nf_entitlement_xref_exists (long al_account_no, long al_benefit_entitlement_no);/* Used for the entitlement delete function 
   	return TRUE if record exists in the xref table
  	and FALSE if the record does not exist
 */
 
 INTEGER li_count_1, li_count_2, li_count
 
 li_count = 0

SELECT 	COUNT(*) INTO :li_count_1
FROM 	ANNUITY_ELIGIBILITY_BENEFIT_ENTITLEMENT_XREF
WHERE 	annuity_account_no 		= :al_account_no
AND	 	benefit_entitlement_no 	= :al_benefit_entitlement_no
USING	SQLCA;

SQLCA.nf_handle_error("n_maintain_benefit","nf_entitlement_xref_exists()","SELECT COUNT(*) FROM ANNUITY_ELIGIBILITY_BENEFIT_ENTITLEMENT_XREF...")

IF ISNULL(li_count_1) THEN  li_count_1 = 0

SELECT 	COUNT(*) INTO :li_count_2
FROM 	ANNUITY_CALC_BENEFIT_ENTITLEMENT_XREF
WHERE 	annuity_account_no 		= :al_account_no
AND	 	benefit_entitlement_no 	= :al_benefit_entitlement_no
USING	SQLCA;

SQLCA.nf_handle_error("n_maintain_benefit","nf_entitlement_xref_exists()","SELECT COUNT(*) FROM ANNUITY_CALC_BENEFIT_ENTITLEMENT_XREF...")

IF ISNULL(li_count_2) THEN  li_count_2 = 0

li_count = li_count_1 + li_count_2

IF li_count > 0  THEN RETURN TRUE

RETURN FALSE
end function

public function integer nf_check_opening_with_bencalc (long al_benefit_calculation_no, integer ai_opening_no, long al_claim_no);/*
Only benefit calculations associated with the selected opening must be available for selection for the benefit entitlement.

RETURNS -1 if nothing is found

*/

LONG ll_num

SELECT 	count(*) 
INTO 		:ll_num 
FROM 	BENEFIT_CALCULATION 
WHERE 	opening_no 					= :ai_opening_no
AND       benefit_calculation_no  	= :al_benefit_calculation_no
AND       claim_no 						= :al_claim_no
USING 	SQLCA;
		
SQLCA.nf_handle_error('n_maintain_benefit','nf_check_opening_with_entitlement()','SELECT count(*) ')
		
IF ll_num <= 0 THEN 	RETURN -1
		
RETURN 1
end function

public function datetime nf_get_bencalc_eff_date (long al_calculation_no, long al_claim_no, long al_opening_no);/* function retruns the Ø	Benefit Calculation Effective Date 
*/

DATETIME ldtm_effective_date

SELECT 	effective_from_date
INTO 		:ldtm_effective_date 
FROM 	BENEFIT_CALCULATION 
WHERE 	benefit_calculation_no 	= :al_calculation_no
AND       claim_no						= :al_claim_no
AND       opening_no					= :al_opening_no
USING 	SQLCA;
		
SQLCA.nf_handle_error('n_maintain_benefit','nf_get_bencalc_eff_date()','SELECT effective_date')
		
RETURN ldtm_effective_date
end function

public function string nf_get_benefit_category_code (string as_payment_type);//get the benefit category code
STRING	ls_benefit_category_code

SELECT 	benefit_category_code
INTO 		:ls_benefit_category_code
FROM 	Payment_Type
WHERE 	payment_type_code	= :as_payment_type
USING 	SQLCA;

SQLCA.nf_handle_error("n_maintain_benefit","nf_get_benefit_category_code()","SELECT benefit_category_code")

IF ISNULL(ls_benefit_category_code) THEN ls_benefit_category_code = ''

RETURN ls_benefit_category_code
end function

public function integer nf_check_span_annuity_interest_period (datetime adtm_from, datetime adtm_to);/*
A benefit entitlement that is created from a weekly benefit calculation must 
not have a period that spans one of the annuity interest periods (ie quarter). 

MARCH 			31 (3)
JUNE    			30 (6)
STEPTEMBER 	30 (9)
DECEMBER		31 (12)
	
RETURNS 1 if DOES NOT span
RETURNS -1 IF it spans the period			
*/
INTEGER 	li_year_from, li_year_to, li_month_from, li_month_to

//populate the required information...
li_year_from	= year(date(adtm_from))
li_year_to		= year(date(adtm_to))
li_month_from 	= month(date(adtm_from))
li_month_to		= month(date(adtm_to))

//if the from_year = to_year and month = month continue on
IF li_year_from = li_year_to AND li_month_from = li_month_to  THEN 
	RETURN 1 //NO ACTION REQUIRED
END IF 

//now do the easy ones - all in the same year
IF li_year_from = li_year_to THEN
	CHOOSE CASE li_month_from
		CASE 1,2,3
			
			IF li_month_to = 4 AND day(date(adtm_to)) = 1 THEN RETURN 1//ALRIGHT
			IF li_month_to > 3 THEN RETURN -1//SPANS THE PERIOD
				
		CASE 4,5,6
			IF li_month_to = 7 AND day(date(adtm_to)) = 1 THEN RETURN 1//ALRIGHT
			IF li_month_to > 6 THEN RETURN -1//SPANS THE PERIOD
				
		CASE 7,8,9
			IF li_month_to = 10 AND day(date(adtm_to)) = 1 THEN RETURN 1//ALRIGHT
			IF li_month_to > 9 THEN RETURN -1//SPANS THE PERIOD
					
		CASE 10,11,12 //NO ACTION REQUIRED year is the same - does not span interest period
	END CHOOSE
ELSE //different years
	IF li_month_to = 1 AND day(date(adtm_to)) = 1 THEN RETURN 1//ALRIGHT
	RETURN -1 //SPANS THE PERIOD
END IF 
		
RETURN 1
end function

public function integer nf_check_partial_span_annuity_interest (datetime adtm_from, datetime adtm_to);/*
7.20	A benefit entitlement that is based on a monthly benefit calculation must not have a partial month that spans 
        an annuity interest period.
	
Scenario &	Expected Results	
1	Create a benefit entitlement Set the ben calc to a monthly award 
	Set the benefit entitlement period from date to prior to an annuity interest period
	Set the benefit entitlement period to date to a date after an annuity interest period
	The benefit entitlement is for a complete month (the period to date is the start of a month)	 Allowed		
2	Create a benefit entitlement Set the ben calc to a monthly award 
	Set the benefit entitlement period from date to prior to an annuity interest period
	Set the benefit entitlement period to date to a date after an annuity interest period
	The benefit entitlement is NOT for a complete month (the period to date is NOT the start of a month)	
	Not allowedReceive an error message indicating that the benefit entitlement must be split 		
3	Create a benefit entitlement Set the ben calc to a monthly award 
	Set the benefit entitlement period from date to prior to an annuity interest period
	Set the benefit entitlement period to date to a date prior an annuity interest period	
	Allowed – no error message received		
4	Create a benefit entitlement Set the ben calc to a monthly award 	
	Set the benefit entitlement period from date to after an annuity interest period
	Set the benefit entitlement period to date to a date after an annuity interest period	
	Allowed – no error message received		
*/

/*
A benefit entitlement that is created from a weekly benefit calculation must 
not have a period that spans one of the annuity interest periods (ie quarter). 

MARCH 			31 (3)
JUNE    			30 (6)
STEPTEMBER 	30 (9)
DECEMBER		31 (12)
	
RETURNS 1 if DOES NOT span
RETURNS -1 IF it spans the period			
*/
INTEGER 	li_year_from, li_year_to, li_month_from, li_month_to, li_day_from, li_day_to

//populate the required information...
li_year_from	= year(date(adtm_from))
li_year_to		= year(date(adtm_to))
li_month_from 	= month(date(adtm_from))
li_month_to		= month(date(adtm_to))
li_day_from		= day(date(adtm_from))
li_day_to			= day(date(adtm_to))

//if the from_year = to_year and month = month continue on
IF li_year_from = li_year_to AND li_month_from = li_month_to  THEN 
	RETURN 1 //NO ACTION REQUIRED
END IF

// no matter how many months apart, it's OK as long as it's the first of months
IF li_day_from = 1 AND li_day_to = 1 THEN
	RETURN 1 //NO ACTION REQUIRED
END IF
	

// all in the same year
IF li_year_from = li_year_to THEN
	CHOOSE CASE li_month_from
		CASE 1,2,3
			CHOOSE CASE li_month_to
				CASE 1,2,3
					// OK
					RETURN 1
				CASE 4
					IF li_day_to = 1 THEN
						// one day into next period, OK
						RETURN 1
					ELSE
						//SPANS THE PERIOD & HAS PARTIAL MONTH
						RETURN -1
					END IF
				CASE 5 TO 12
					//SPANS THE PERIOD & HAS PARTIAL MONTH
					RETURN -1
			END CHOOSE
				
		CASE 4,5,6
			CHOOSE CASE li_month_to
				CASE 4,5,6
					// OK
					RETURN 1
				CASE 7
					IF li_day_to = 1 THEN
						// one day into next period, OK
						RETURN 1
					ELSE						
						//SPANS THE PERIOD & HAS PARTIAL MONTH
						RETURN -1
					END IF
				CASE 8 TO 12
					//SPANS THE PERIOD & HAS PARTIAL MONTH
					RETURN -1
			END CHOOSE
				
		CASE 7,8,9			
			CHOOSE CASE li_month_to
				CASE 7,8,9
					// OK
					RETURN 1
				CASE 10
					IF li_day_to = 1 THEN
						// one day into next period, OK
						RETURN 1
					ELSE
						//SPANS THE PERIOD & HAS PARTIAL MONTH
						RETURN -1
					END IF
				CASE 11,12
					//SPANS THE PERIOD & HAS PARTIAL MONTH
					RETURN -1
			END CHOOSE
					
		CASE 10,11,12
			//NO ACTION REQUIRED year is the same - does not span interest period
			RETURN 1
	END CHOOSE
	
ELSE //different years
	IF li_year_from	= li_year_to - 1 THEN
		CHOOSE CASE li_month_from
			CASE IS < 10
				//SPANS THE PERIOD & HAS PARTIAL MONTH
				RETURN -1
			CASE ELSE
				IF li_month_to = 1 AND li_day_to = 1 THEN
					// OK
					RETURN 1
				ELSE
					//SPANS THE PERIOD & HAS PARTIAL MONTH
					RETURN -1					
				END IF
		END CHOOSE
	ELSE
		//SPANS THE PERIOD & HAS PARTIAL MONTH
		RETURN -1
	END IF
END IF 
		
RETURN 1

end function

public function integer nf_check_benefit_entitlement_dates (date adt_check_date, long al_opening_no, long al_claim_no, string as_opening_type, string as_payment_type, integer ai_case);/*
(ai_case =  1)
7.80	The benefit entitlement period from date must not be less than the benefit start date for the associated opening on the benefit entitlement.
	Scenario	Expected Results	Pass/Fail	Note
1	Enter a “Period” from date that is < than the benefit start date for the opening		-- 	Not allowed		
2	Enter a “Period” from date that is = than the benefit start date for the opening		--	Allowed.		
3	Enter a “Period” from date that is > than the benefit start date for the opening		--	Allowed.		

(ai_case = 2)
7.100	The benefit entitlement period from date must not be greater than the benefit end date for the associated opening on the benefit entitlement.
	Scenario	Expected Results	Pass/Fail	Note
1	“Period” from date > benefit end date for the opening.	Receive an error message indicating the ‘Period’ from date 
	must be before the benefit end date.		
2	“Period” from date that is = benefit end date for the opening.	Payment not created,Error that the from and to dates cannot be equal		
3	“Period” from date that is < the benefit end date for the opening.	The payment is created/updated with the Period from date entered		

(ai_case = 3)
7.110	The benefit entitlement period from date must not be before the accident recurrence date of the associated opening 
          on the benefit entitlement.
	Scenario	Expected Results	Pass/Fail	Note
1	“Period” from date <  accident recurrence date.	Receive an error message indicating the ‘Period’ from date cannot be before the recurrence date.		
2	“Period” from date  > accident recurrence date.	The payment is created/updated with the Period from date entered		
3	 “Period” from date  = accident recurrence date.	The payment is created/updated with the Period from date entered		

(ai_case = 4)
7.130	The benefit entitlement period to date must not be greater than the benefit end date of the associated opening on the benefit entitlement.
	Scenario	Expected Results	Pass/Fail	Note
1	“Period” to date > benefit end date for the opening. Payment type not RTW Job search	
     Receive an error message indicating the Period to date cannot be later than the benefit end date.		
2	“Period” to date = benefit end date for the opening. Payment created/update with the Period to date entered.		
3	“Period” to date < benefit end date for the opening. Payment created/update with the Period to date entered.		
4	“Period” to date > benefit end date for the opening. Payment type is at RTW Job search	
     Payment created/update with the Period to date entered.		

returns 1 -- all is good
return -1 -- error

 FOR non coded values use '1900-01-01'	
*/
DATETIME ldt_check_start, ldt_check_end, ldt_check_accident

SELECT 	benefit_start_date,  benefit_end_date, accident_recurrence_date
INTO   	:ldt_check_start, :ldt_check_end, :ldt_check_accident
FROM 	OPENING
WHERE 	claim_no 				= :al_claim_no
AND     	opening_no 				= :al_opening_no
AND     	opening_type_code 	= :as_opening_type
USING  	SQLCA;

SQLCA.nf_handle_error("n_maintain_benefit","nf_check_benefit_entitlement_dates()","SELECT benefit_start_date, benefit_end_date")

CHOOSE CASE ai_case
	CASE 1
		//The benefit entitlement period from date must not be less than the benefit start date for the selected opening on the benefit entitlement.	
		IF 	DATE(adt_check_date) < DATE(ldt_check_start) THEN RETURN -1
	
	CASE 2
		//The benefit entitlement period from date must not be greater than the benefit end date for the associated opening on the benefit entitlement.
		IF 	DATE(adt_check_date) > DATE(ldt_check_end) THEN RETURN -1
		
	CASE 3
		//The benefit entitlement period from date must not be before the accident recurrence date of the associated opening on the benefit entitlement.
		IF 	DATE(adt_check_date) < DATE(ldt_check_accident) THEN RETURN -1
		
	CASE 4
		
		IF isnull(as_payment_type) OR trim(as_payment_type) = '' THEN RETURN -1// we need this
		
		IF as_payment_type = 'R1' THEN 
			//NO PROBLEMS
		ELSE
			//1	“Period” to date > benefit end date for the opening. Payment type not RTW Job search	
			IF 	DATE(adt_check_date) > DATE(ldt_check_end) THEN RETURN -1
		END IF 
	CASE 6
		/*7.190	The period from date of a benefit entitlement must be set to the benefit start date of the selected opening 
					if the 60% option lump sum indicator is set to ‘Yes’.
		*/
		IF 	DATE(adt_check_date) <> DATE(ldt_check_start) THEN RETURN -1
		
	CASE 7
		/*7.200	The period to date of a benefit entitlement must be set to one day after the benefit start date 
					of the selected opening if the 60% option lump sum indicator is set to ‘Yes’.
		*/
		IF 	DAYSAFTER(DATE(ldt_check_start), DATE(adt_check_date)) <> 1 THEN RETURN -1
		
END CHOOSE
		
RETURN 1
end function

public function boolean nf_record_exists_in_xref (long al_annuity_account_no, long al_annuity_eligibility_no, long al_benefit_entitlement_no);LONG		ll_count

/* Function determines if the Benefit Entitlement exists in the ANNUITY_ELIGIBILITY_BENEFIT_ENTITLEMENT_XREF table

	returns true if it does, false if not.
*/

SELECT count(*) 
INTO 		:ll_count 
FROM 	ANNUITY_ELIGIBILITY_BENEFIT_ENTITLEMENT_XREF
WHERE 	annuity_account_no 		= :al_annuity_account_no
AND		annuity_eligibility_no 		= :al_annuity_eligibility_no
AND 		benefit_entitlement_no 	= :al_benefit_entitlement_no
USING SQLCA;

SQLCA.nf_handle_error("n_maintain_benefit","nf_record_exists_in_xref","SELECT COUNT(*) FROM ANNUITY...")

IF ll_count > 0 THEN RETURN TRUE

RETURN FALSE
end function

public function integer nf_get_claimant_active_flag (long al_individual_no, string as_claim_role_code);/*
	A Verify Benefit Entitlement checklist step must not be completed if active benefit entitlement data 
    	exists for a claim where the claimant is inactive.
		 
	The claimant can be made inactive on a claim if they are associated with the wrong claim.  
	If benefit entitlement is saved for a claim where the injured worker is no longer the active 
	claimant then this will need to be removed.  This will be check before the checklist can be completed.  
	The claimant could be made inactive for a variety of reasons.  For example the father and son may 
	have the same name and both are injured workers.  The incorrect individual could be selected for 
	the claim and then later corrected.  If benefit entitlement is saved before the correction this will need 
	to be corrected.
	
	note: This function returns the claimant active flag from the CLAIM_PARTICIPANT table
	
	This check should only be for an injured worker.
	For the surviving spouse the claimant_active_flag will always be 'N' (WG)
	
*/

STRING		ls_claimant_active_flag	
INTEGER		li_count

li_count = 0

SELECT  		count(*)
INTO			:li_count
FROM 		CLAIM_PARTICIPANT a
JOIN         	BENEFIT_ENTITLEMENT b ON a.claim_no = b.claim_no
WHERE 		individual_no 				= :al_individual_no
AND 			claim_role_code 			= :as_claim_role_code
AND            b.active_flag         		= 'Y'
AND            a.claimant_active_flag 	= 'N'
USING 		SQLCA;

SQLCA.nf_handle_error("n_maintain_benefit","nf_get_claimant_active_flag()","SELECT claimant_active_flag")

IF isnull(li_count) THEN  li_count = -1

RETURN li_count

end function

public function boolean nf_check_payment_type_claim_role (string as_payment_type, string as_claim_role);/*
	The payment type must be a payment type that attracts annuities for an injured worker 
	if the benefit entitlement is for an injured worker and the entitlement is not associated with an opening.
	The payment type must be a payment type that attracts annuities for a surviving spouse 
	if the benefit entitlement is for a surviving spouse and the entitlement is not associated with an opening.
*/

LONG ll_num

SELECT  	count(*)
INTO 		:ll_num
FROM 	Payment_Type            	a 
JOIN Payment_Combination        	b 		ON b.payment_type_code 	= a.payment_type_code
JOIN Claim_Role_Opening_Type_Xref c 	ON c.opening_type_code 	= b.opening_type_code
WHERE 	annuity_eligibility_flag 	= 'Y'
AND 		annuity_flag             		= 'Y'
AND 		c.claim_role_code          	= :as_claim_role
USING 	SQLCA;
		
SQLCA.nf_handle_error('n_maintain_benefit','nf_check_payment_type_claim_role()','SELECT count(*) ')

IF ISNULL(ll_num) THEN  RETURN FALSE

IF ll_num <= 0 THEN RETURN FALSE

RETURN TRUE
end function

public function date nf_get_claim_accident_date (long al_claim_no);/*
	The payment type must be a payment type that attracts annuities for an injured worker 
	if the benefit entitlement is for an injured worker and the entitlement is not associated with an opening.
	The payment type must be a payment type that attracts annuities for a surviving spouse 
	if the benefit entitlement is for a surviving spouse and the entitlement is not associated with an opening.
*/

DATETIME			ldtm_accident_date

SELECT  	accident_date
INTO 		:ldtm_accident_date
FROM 	CLAIM            
WHERE 	claim_no = :al_claim_no
USING 	SQLCA;
		
SQLCA.nf_handle_error('n_maintain_benefit','nf_get_claim_accident_date()','SELECT accident_date ')

RETURN DATE(ldtm_accident_date)
end function

public function decimal nf_get_bc_award_amount (long al_claim_no, long al_benefit_calculation_no, long al_opening_no);/*
grabs the award amount from the BENEFIT_CALCULATION table

RETURNS decimal amount or 0
*/

DECIMAL ldec_award_amount

SELECT 	award_amount
INTO 		:ldec_award_amount
FROM 	BENEFIT_CALCULATION 
WHERE 	claim_no 					= :al_claim_no
AND 		benefit_calculation_no 	= :al_benefit_calculation_no
AND 		opening_no 					= :al_opening_no
USING 	SQLCA;
		
SQLCA.nf_handle_error('n_maintain_benefit','nf_get_bc_award_amount()','SELECT award_amount ')
		
IF ISNULL(ldec_award_amount) THEN ldec_award_amount = 0
		
RETURN ldec_award_amount
end function

public function decimal nf_calculate_3_fifths (decimal adec_check_value);/*
returns 3 fiths of a value - used in calculations and checks

THIS WILL BE A positive NUMBER -- will have to converted to a negative number
*/

DECIMAL 	ldec_return_value

//returns a negative 
ldec_return_value = ((adec_check_value / 5) * 3) 

RETURN ldec_return_value
end function

public function datetime nf_get_effective_date (long al_claim_no, long al_ben_calc_no, long al_opening_no);//grab the payment freq code from the provided information.
DATETIME	ldtm_effective_from_date

SELECT 	effective_from_date 
INTO 		:ldtm_effective_from_date
FROM 	BENEFIT_CALCULATION
WHERE 	claim_no 					= :al_claim_no
AND 		benefit_calculation_no 	= :al_ben_calc_no
AND 		opening_no 					= :al_opening_no
USING 	SQLCA;

SQLCA.nf_handle_error("n_maintain_benefit","nf_get_effective_date()","SELECT 	effective_from_date")


RETURN ldtm_effective_from_date
end function

public function string nf_get_freq_from_payment_type (string as_payment_type);//grab the payment freq code from the provided information.
STRING ls_freq_code

SELECT 	award_freq_code 
INTO 		:ls_freq_code
 FROM 	Payment_Type
WHERE 	payment_type_code = :as_payment_type
USING 	SQLCA;

SQLCA.nf_handle_error("n_maintain_benefit","nf_get_freq_from_payment_type()","SELECT award_freq_code ")

IF ISNULL(ls_freq_code) THEN ls_freq_code = ''

RETURN ls_freq_code
end function

public function datastore nf_split_payment_for_interest_period (date adtm_from, date adtm_to, string as_frequency);/*
	Also a weekly payment that spans an annuity interest period will need to be split so 
	that it is clear which portion of the entitlement is prior to the interest period and 
	which portion is in the next interest period.  For monthly payments the entitlement 
	will only need to be split if the entitlement is represented by a partial month and the 
	partial month is in the month of an annuity interest period.  For example if the award 
	goes from March 15 to December 31 the award will need to be split into March 15 to 
	March 31 and April 1 to December 31. The splitting of the entitlement should be done 
	automatically for any payment dropped on the entitlement tab that spans the period described above.
	
	REVISION: If split date is last day of month go to first day of next month 
	                and next date starts with same date
	 (Addition) If the period to date is the same as the annuity interest period date then there is no need to split the payment.
					I will have to delete the entitlement that is created with the dates 2005-07-01 to 2005-07-01 
					since I can't enter any days on this entitlement.
	
	note: working from the example - it may be incorrect
	need the annuity interest quarters
	
	MARCH 			31 (3)
	JUNE    			30 (6)
	STEPTEMBER 	30 (9)
	DECEMBER		31 (12)
	
	dbo.udf_First_Date_Of_Next_Month
	dbo.udf_First_Date_Of_Current_Month
	dbo.udf_Last_Date_Of_Current_Month
		
	as_frequency = 'M' monthly OR 'W' weekly	
	
	"This has PR written all over it!!!!!!!!!"
	
	EXAMPLES:
	#1 1997-02-27 to 1997-04-01		
		(1997-02-27 to 1997-04-01)
	
	#2	1997-09-30 to 1997-10-14
		(1997-09-30 to 1997-10-01 &  1997-10-01 to 1997-10-14) 
	
	#3	1997-06-24 to 1997-07-08
	1997-06-24 to 1997-07-01  1997-07-01 to 1997-07-08 
	
	#4	1997-09-16 to 1997-09-30
		(1997-09-16 to 1997-09-30)
	
	#5	1997-12-23 to 1998-01-06
	1997-12-23 to 1998-01-01  & 1998-01-01 to 1998-01-06 
	
	#6	2007-12-01 to 2008-01-01
		(2007-12-01 to 2008-01-01)
		
	2001-01-01 to 2002-01-01		the award has a weekly Frequency
	
	2001-01-01 to 2001-04-01
	2001-04-01 to 2001-07-01
	2001-07-01 to 2001-10-01
	2001-10-01 to 2002-01-01
	
*/

BOOLEAN	lb_first_quarter, lb_last_year

DATE			ldt_start_date , ldt_end_date, ldt_calculated_to, ldt_calculated_start, ldt_calculated_end

DATETIME	ldtm_end_date

INTEGER		li_min_partial_year, li_max_partial_year
INTEGER		li_original_start_month, li_original_end_month, li_start_month, li_end_month
INTEGER		li_original_start_day, li_original_end_day, li_start_day, li_end_day
INTEGER		li_year_counter, li_quarter_counter, li_last_item
INTEGER		li_year_array[], li_upperbound, li_year, li_row, li_rowcount_ds

INTEGER		li_year_from, li_year_to, li_month_from, li_month_to, li_counter

STRING		ls_quarter, ls_start_month,	ls_start_day
STRING		ls_end_month,	ls_end_day, ls_split

BOOLEAN	ib_split

s_quarters 	lstr_quarters[]
datastore 	lds_interest_period

//populate the array
lstr_quarters[1].quarter_no 					= 1
lstr_quarters[1].start_quarter_month 	= '01'
lstr_quarters[1].end_quarter_month 		= '03'
lstr_quarters[1].end_quarter_day 			= '31'

lstr_quarters[2].quarter_no 					= 2
lstr_quarters[2].start_quarter_month 	= '04'
lstr_quarters[2].end_quarter_month 		= '06'
lstr_quarters[2].end_quarter_day 			= '30'

lstr_quarters[3].quarter_no 					= 3
lstr_quarters[3].start_quarter_month 	= '07'
lstr_quarters[3].end_quarter_month 		= '09'
lstr_quarters[3].end_quarter_day 			= '30'

lstr_quarters[4].quarter_no 					= 4
lstr_quarters[4].start_quarter_month 	= '10'
lstr_quarters[4].end_quarter_month 		= '12'
lstr_quarters[4].end_quarter_day 			= '31'

//create the datastore
lds_interest_period 				= CREATE DATASTORE
lds_interest_period.dataobject 	= 'ds_span_annuity_interest'
lds_interest_period.settransobject(sqlca)

//if the dates are null return out
IF ISNULL(adtm_from) OR ISNULL(adtm_to) THEN
	li_row = lds_interest_period.insertrow(0)
	lds_interest_period.setitem(li_row, 'annuity_date_from',adtm_from)
	lds_interest_period.setitem(li_row, 'annuity_date_to',adtm_to)
	lds_interest_period.setitem(li_row, 'for_split','N')
	RETURN lds_interest_period //NO ACTION REQUIRED
END IF

//set the initial state of the variables
ldt_start_date 				= adtm_from
ldt_end_date		 		= adtm_to

li_min_partial_year 		= Year(ldt_start_date)
li_original_start_month 	= Month(ldt_start_date)
li_start_month				= li_original_start_month
li_original_start_day		= Day(ldt_start_date)
li_start_day 					= li_original_start_day

//populate the required information...
li_year_from	= year(adtm_from)
li_year_to		= year(adtm_to)
li_month_from 	= month(adtm_from)
li_month_to		= month(adtm_to)

//default ib_split to true
ib_split = TRUE

//if the from_year = to_year and month = month continue on
IF li_year_from = li_year_to AND li_month_from = li_month_to  THEN ib_split = FALSE

IF as_frequency = 'W' THEN
	
	//now do the easy ones - all in the same year
	IF li_year_from = li_year_to THEN			
		IF li_month_from = 3  AND li_month_to = 4 AND day(adtm_to) = 1 THEN 
			ib_split = FALSE
		ELSEIF li_month_from = 6  AND li_month_to = 7 	AND day(adtm_to) = 1 THEN 
			ib_split = FALSE
		ELSEIF li_month_from = 9  AND li_month_to = 10 AND day(adtm_to) = 1 THEN 
			ib_split = FALSE
		ELSEIF li_month_from >= 10 THEN 
			ib_split = FALSE
		END IF
	ELSE //different years
		
		IF li_month_to = 1 AND day(adtm_to) = 1 AND li_month_from = 12  AND li_year_from + 1 = li_year_to THEN
			ib_split = FALSE
		END IF
	END IF 


	IF ib_split = FALSE THEN
		li_row = lds_interest_period.insertrow(0)
		lds_interest_period.setitem(li_row, 'annuity_date_from',adtm_from)
		lds_interest_period.setitem(li_row, 'annuity_date_to',adtm_to)
		lds_interest_period.setitem(li_row, 'for_split','N')
		RETURN lds_interest_period //NO ACTION REQUIRED
	END IF 
	

	
	li_max_partial_year 		= Year(ldt_end_date)
	li_original_end_month 	= Month(ldt_end_date)
	li_end_month 				= li_original_end_month
	li_original_end_day		= Day(ldt_end_date)
	li_end_day 					= li_original_end_day
	
	
	li_upperbound 						= 1
	li_year_array[li_upperbound] 	= li_min_partial_year
	
	DO WHILE li_year_array[li_upperbound] < li_max_partial_year
		li_upperbound 						= li_upperbound + 1
		li_year_array[li_upperbound] 	= li_year_array[li_upperbound - 1] + 1	
	LOOP
	
	FOR li_year_counter = 1 TO li_upperbound
		li_year = li_year_array[li_year_counter]
		
		// set up start month/day
		IF li_year_counter = 1 THEN
			li_start_month 	= li_original_start_month
			li_start_day 		= Day(ldt_start_date)
			
			IF li_start_month < 10 THEN
				ls_start_month = '0' + String(li_start_month)
			ELSE
				ls_start_month = String(li_start_month)
			END IF			
			IF li_start_day < 10 THEN
				ls_start_day = '0' + String(li_start_day)
			ELSE
				ls_start_day = String(li_start_day)
			END IF
			lb_first_quarter = TRUE
		ELSE
			li_start_month 			= 1
			li_start_day 				= 1
			ls_start_month 			= '01'
			ls_start_day 			= '01'		
		END IF
		
		
		// set up end month/day
		IF li_year_counter < li_upperbound THEN
			li_end_month 			= 12
			li_end_day 				= 31
			ls_end_month 			= String(li_end_month)
			ls_end_day 				= String(li_end_day)
			
		ELSE
			li_end_month 			= li_original_end_month
			li_end_day 				= li_original_end_day
			
			IF li_end_month < 10 THEN
				ls_end_month = '0' + String(li_end_month)
			ELSE
				ls_end_month = String(li_end_month)
			END IF
			
			IF li_end_day < 10 THEN
				ls_end_day = '0' + String(li_end_day)
			ELSE
				ls_end_day = String(li_end_day)
			END IF
			lb_last_year = TRUE
		END IF
		
		IF li_year_counter = li_upperbound THEN lb_last_year = TRUE
				
		FOR li_quarter_counter = 1 TO 4
			// starts within quarter
			IF li_start_month >= Integer(lstr_quarters[li_quarter_counter].start_quarter_month) and li_start_month <= Integer(lstr_quarters[li_quarter_counter].end_quarter_month) THEN
				// starts & ends within quarter
				IF lb_last_year = TRUE THEN
					IF li_original_end_month >= Integer(lstr_quarters[li_quarter_counter].start_quarter_month) AND li_original_end_month <= Integer(lstr_quarters[li_quarter_counter].end_quarter_month) THEN
						IF ls_end_month = lstr_quarters[li_quarter_counter].start_quarter_month AND ls_end_day = '01' THEN
							// if the orig end month/day is start of quarter, do not go to next loop
							EXIT
						ELSE
							li_row = lds_interest_period.insertrow(0)
							ldt_calculated_start = Date(String(li_year) + '-' + ls_start_month + '-'+ ls_start_day)
							ldt_calculated_end = Date(String(li_year) + '-' + ls_end_month + '-' + ls_end_day)
							lds_interest_period.setitem(li_row, 'annuity_date_from',ldt_calculated_start)
							lds_interest_period.setitem(li_row, 'annuity_date_to',ldt_calculated_end)
						END IF
						EXIT
					ELSEIF li_end_month > Integer(lstr_quarters[li_quarter_counter].end_quarter_month) THEN
						li_row = lds_interest_period.insertrow(0)
						ldt_calculated_start = Date(String(li_year) + '-' + ls_start_month + '-'+ ls_start_day)
						ldt_calculated_end = RelativeDate(Date(String(li_year) + '-' + lstr_quarters[li_quarter_counter].end_quarter_month + '-' + lstr_quarters[li_quarter_counter].end_quarter_day),1)
						lds_interest_period.setitem(li_row, 'annuity_date_from',ldt_calculated_start)
						lds_interest_period.setitem(li_row, 'annuity_date_to',ldt_calculated_end)
						
						li_start_month = Integer(lstr_quarters[li_quarter_counter].end_quarter_month) + 1
						IF li_quarter_counter < 4 THEN 
							ls_start_month = lstr_quarters[li_quarter_counter+1].start_quarter_month
							li_start_month = Integer(ls_start_month)
						ELSE					
							EXIT
						END IF
						
						ls_start_day = '01'	
						li_start_day = 1
					END IF
				ELSEIF lb_first_quarter = TRUE THEN
					li_row = lds_interest_period.insertrow(0)
					ldt_calculated_start = Date(String(li_year) + '-' + ls_start_month + '-'+ ls_start_day)
					ldt_calculated_end = RelativeDate(Date(String(li_year) + '-' + lstr_quarters[li_quarter_counter].end_quarter_month + '-' + lstr_quarters[li_quarter_counter].end_quarter_day),1)
					lds_interest_period.setitem(li_row, 'annuity_date_from',ldt_calculated_start)
					lds_interest_period.setitem(li_row, 'annuity_date_to',ldt_calculated_end)
					
					li_start_month = Integer(lstr_quarters[li_quarter_counter].end_quarter_month) + 1
					IF li_quarter_counter < 4 THEN 
						ls_start_month = lstr_quarters[li_quarter_counter+1].start_quarter_month
					ELSE					
						EXIT
					END IF
						
					ls_start_day = '01'
					lb_first_quarter = FALSE
				ELSE
					// Not last year in loop
					li_row = lds_interest_period.insertrow(0)
					ldt_calculated_start = Date(String(li_year) + '-' + ls_start_month + '-'+ ls_start_day)
					ldt_calculated_end = RelativeDate(Date(String(li_year) + '-' + lstr_quarters[li_quarter_counter].end_quarter_month + '-' + lstr_quarters[li_quarter_counter].end_quarter_day),1)
					lds_interest_period.setitem(li_row, 'annuity_date_from',ldt_calculated_start)
					lds_interest_period.setitem(li_row, 'annuity_date_to',ldt_calculated_end)
					
					IF li_quarter_counter < 4 THEN 
						ls_start_month = lstr_quarters[li_quarter_counter+1].start_quarter_month
						li_start_month = Integer(ls_start_month)
					ELSE
						EXIT
					END IF
				END IF
			END IF
		NEXT
	NEXT
			
	li_rowcount_ds = lds_interest_period.rowcount()
	IF li_rowcount_ds = 1 THEN 
		ls_split= 'N'
	ELSE
		ls_split= 'Y'
	END IF
	
	FOR li_counter = 1 TO li_rowcount_ds
		lds_interest_period.setitem(li_counter, 'for_split',ls_split)
	NEXT
	
ELSEIF  as_frequency = 'M' THEN //MONTHLY

	IF DAY(DATE(adtm_from)) = 1 THEN 			
		li_row = lds_interest_period.insertrow(0)
		lds_interest_period.setitem(li_row, 'annuity_date_from',adtm_from)
		lds_interest_period.setitem(li_row, 'annuity_date_to',adtm_to)
		lds_interest_period.setitem(li_row, 'for_split','N')
		RETURN lds_interest_period //NO ACTION REQUIRED
	END IF 
	
	CHOOSE CASE li_month_from
		CASE 3,6,9,12
			IF ( li_month_from = 3 	    AND ( li_month_to = 4 	AND day(adtm_to) = 1 )) THEN 
				li_row = lds_interest_period.insertrow(0)
				lds_interest_period.setitem(li_row, 'annuity_date_from',adtm_from)
				lds_interest_period.setitem(li_row, 'annuity_date_to',adtm_to)
				lds_interest_period.setitem(li_row, 'for_split','N')
				RETURN lds_interest_period //NO ACTION REQUIRED
			END IF
				
		    IF  ( li_month_from = 6 		AND ( li_month_to = 7 	AND day(adtm_to) = 1 )) THEN 	
				li_row = lds_interest_period.insertrow(0)
				lds_interest_period.setitem(li_row, 'annuity_date_from',adtm_from)
				lds_interest_period.setitem(li_row, 'annuity_date_to',adtm_to)
				lds_interest_period.setitem(li_row, 'for_split','N')
				RETURN lds_interest_period //NO ACTION REQUIRED
			END IF
			
			IF ( li_month_from = 9 		AND ( li_month_to = 10 	AND day(adtm_to) = 1 )) THEN
				li_row = lds_interest_period.insertrow(0)
				lds_interest_period.setitem(li_row, 'annuity_date_from',adtm_from)
				lds_interest_period.setitem(li_row, 'annuity_date_to',adtm_to)
				lds_interest_period.setitem(li_row, 'for_split','N')
				RETURN lds_interest_period //NO ACTION REQUIRED
			END IF
			
			IF ( li_month_from = 12 		AND ( li_month_to = 1 	AND day(adtm_to) = 1 ) AND li_year_to = li_year_from + 1 ) THEN 
					li_row = lds_interest_period.insertrow(0)
					lds_interest_period.setitem(li_row, 'annuity_date_from',adtm_from)
					lds_interest_period.setitem(li_row, 'annuity_date_to',adtm_to)
					lds_interest_period.setitem(li_row, 'for_split','N')
					RETURN lds_interest_period //NO ACTION REQUIRED
			END IF
		
			IF li_month_from = 3 THEN
		
					//make the end date for the first payment = to last day of interest period
					li_row = lds_interest_period.insertrow(0)
					lds_interest_period.setitem(li_row, 'annuity_date_from',adtm_from)
					
					ldt_calculated_to = DATE(STRING(li_year_from) + '-' + '03-01' )
					
					SELECT 	dbo.udf_Last_Date_Of_Current_Month(:ldt_calculated_to)
					INTO		:ldtm_end_date
					FROM 	sysobjects
					USING 	sqlca;
					SQLCA.nf_handle_error('n_maintain_benefit', 'nf_split_payment_for_interest_period()', '	Section E')
					
					lds_interest_period.setitem(li_row, 'annuity_date_to',relativedate(date(ldtm_end_date),1))
					lds_interest_period.setitem(li_row, 'for_split','Y')
					
					//NOW DO THE NEXT PERIOD
					li_row = lds_interest_period.insertrow(0)
					lds_interest_period.setitem(li_row, 'annuity_date_from',relativedate(date(ldtm_end_date),1))
					lds_interest_period.setitem(li_row, 'annuity_date_to',adtm_to)
					lds_interest_period.setitem(li_row, 'for_split','Y')
					RETURN lds_interest_period //NO ACTION REQUIRED
					
			ELSEIF 	li_month_from = 6 THEN
					//make the end date for the first payment = to last day of interest period
					li_row = lds_interest_period.insertrow(0)
					lds_interest_period.setitem(li_row, 'annuity_date_from',adtm_from)
					
					ldt_calculated_to = DATE(STRING(li_year_from) + '-' + '06-01' )
					
					SELECT 	dbo.udf_Last_Date_Of_Current_Month(:ldt_calculated_to)
					INTO		:ldtm_end_date
					FROM 	sysobjects
					USING 	sqlca;
					SQLCA.nf_handle_error('n_maintain_benefit', 'nf_split_payment_for_interest_period()', '	Section F')
					
					lds_interest_period.setitem(li_row, 'annuity_date_to',relativedate(date(ldtm_end_date),1))
					lds_interest_period.setitem(li_row, 'for_split','Y')
					
					//NOW DO THE NEXT PERIOD
					li_row = lds_interest_period.insertrow(0)
					lds_interest_period.setitem(li_row, 'annuity_date_from',relativedate(date(ldtm_end_date),1))
					lds_interest_period.setitem(li_row, 'annuity_date_to',adtm_to)
					lds_interest_period.setitem(li_row, 'for_split','Y')
					RETURN lds_interest_period //NO ACTION REQUIRED
		
			ELSEIF 	li_month_from = 9 THEN
					//make the end date for the first payment = to last day of interest period
					li_row = lds_interest_period.insertrow(0)
					lds_interest_period.setitem(li_row, 'annuity_date_from',adtm_from)
					
					ldt_calculated_to = DATE(STRING(li_year_from) + '-' + '09-01' )
					
					SELECT 	dbo.udf_Last_Date_Of_Current_Month(:ldt_calculated_to)
					INTO		:ldtm_end_date
					FROM 	sysobjects
					USING 	sqlca;
					SQLCA.nf_handle_error('n_maintain_benefit', 'nf_split_payment_for_interest_period()', '	Section G')
					
					lds_interest_period.setitem(li_row, 'annuity_date_to',relativedate(date(ldtm_end_date),1))
					lds_interest_period.setitem(li_row, 'for_split','Y')
					
					//NOW DO THE NEXT PERIOD
					li_row = lds_interest_period.insertrow(0)
					lds_interest_period.setitem(li_row, 'annuity_date_from',relativedate(date(ldtm_end_date),1))
					lds_interest_period.setitem(li_row, 'annuity_date_to',adtm_to)
					lds_interest_period.setitem(li_row, 'for_split','Y')
					RETURN lds_interest_period //NO ACTION REQUIRED
			
			ELSEIF 	li_month_from = 12 THEN
				
					//make the end date for the first payment = to last day of interest period
					li_row = lds_interest_period.insertrow(0)
					lds_interest_period.setitem(li_row, 'annuity_date_from',adtm_from)
					
					ldt_calculated_to = DATE(STRING(li_year_from) + '-' + '12-01' )
					
					SELECT 	dbo.udf_Last_Date_Of_Current_Month(:ldt_calculated_to)
					INTO		:ldtm_end_date
					FROM 	sysobjects
					USING 	sqlca;
					SQLCA.nf_handle_error('n_maintain_benefit', 'nf_split_payment_for_interest_period()', '	Section H')
					
					lds_interest_period.setitem(li_row, 'annuity_date_to',relativedate(date(ldtm_end_date),1))
					lds_interest_period.setitem(li_row, 'for_split','Y')
					
					//NOW DO THE NEXT PERIOD
					li_row = lds_interest_period.insertrow(0)
					lds_interest_period.setitem(li_row, 'annuity_date_from',relativedate(date(ldtm_end_date),1))
					lds_interest_period.setitem(li_row, 'annuity_date_to',adtm_to)
					lds_interest_period.setitem(li_row, 'for_split','Y')
					RETURN lds_interest_period //NO ACTION REQUIRED
			END IF 
				 		
		CASE ELSE//don't care
					li_row = lds_interest_period.insertrow(0)
					lds_interest_period.setitem(li_row, 'annuity_date_from',adtm_from)
					lds_interest_period.setitem(li_row, 'annuity_date_to',adtm_to)
					lds_interest_period.setitem(li_row, 'for_split','N')
					RETURN lds_interest_period //NO ACTION REQUIRED
	END CHOOSE
ELSE
	li_row = lds_interest_period.insertrow(0)
	lds_interest_period.setitem(li_row, 'annuity_date_from',adtm_from)
	lds_interest_period.setitem(li_row, 'annuity_date_to',adtm_to)
	lds_interest_period.setitem(li_row, 'for_split','N')
	RETURN lds_interest_period //NO ACTION REQUIRED
END IF

RETURN lds_interest_period 

end function

public function string nf_check_span_start_date (date adt_from_date, date adt_to_date, datetime adtm_annuity_start_date, datetime adtm_annuity_end_date, integer ai_case);/*
A benefit entitlement period must not span the annuity start date. 
A benefit entitlement period must not span the annuity end date.

returns 1 -- all is good
return -1 -- error

ai_case
1 = spans annuity_start_date
2 = spans annuity_end_date

ai_type
1 = SS
2 = IW

(START DATE)
1	Create a benefit entitlement The annuity start date is not set	No message about the annuity start date	X	
2	Create benefit entitlement The annuity start date is set The benefit entitlement period to date 
	is prior to the annuity start date	No message about the annuity start date		
3	Create benefit entitlement The annuity start date is set The benefit entitlement period to date 
	is after the annuity start date The benefit entitlement period from date is after the annuity start date	
	No message about the annuity start date		
4	Create benefit entitlement The annuity start date is set The benefit entitlement period to date
	is after the annuity start date The benefit entitlement period from date is before the annuity start date	
	Receive an error indicating that the benefit entitlement spans the annuity start date		
	(NOTE**** ONLY CODE THIS ONE **************************************)
	
(END DATE)
6.90	A benefit entitlement period must not span the annuity end date.
1	Create a benefit entitlement The annuity end date is not set	No message about the annuity end date		
2	Create benefit entitlement The annuity end date is set The benefit entitlement period to date is prior to the annuity end date	
	No message about the annuity end date		
3	Create benefit entitlement The annuity end date is set The benefit entitlement period to date is after the annuity end date 
	The benefit entitlement period from date is after the annuity end date	No message about the annuity end date		
4	Create benefit entitlement The annuity end date is set The benefit entitlement period to date is after the annuity end date 
	The benefit entitlement period from date is before the annuity end date	
	Receive an error indicating that the benefit entitlement spans the annuity end date
	(NOTE**** ONLY CODE THIS ONE **************************************)
	
*/

CHOOSE CASE ai_case
	CASE 1//START DATE --	'A benefit entitlement period must not span the annuity start date.'
		
		//dont care about the return value only care if date is set
		IF isnull(adtm_annuity_start_date) THEN RETURN ''//NO ERROR
		
		IF adt_from_date < DATE(adtm_annuity_start_date) AND adt_to_date > DATE(adtm_annuity_start_date) THEN
			RETURN 'A benefit entitlement period must not span the annuity start date: ' + string(DATE(adtm_annuity_start_date),'YYYY-MM-DD')
		END IF 
	CASE 2//END DATE -- 'A benefit entitlement period must not span the annuity end date.'	
		
		//dont care about the return value only care if date is set
		IF isnull(adtm_annuity_end_date) THEN RETURN ''//NO ERROR
		
		IF adt_from_date < DATE(adtm_annuity_end_date) AND adt_to_date > RelativeDate(DATE(adtm_annuity_end_date),1) THEN
			RETURN  'A benefit entitlement period must not span the annuity end date: ' + string(DATE(adtm_annuity_end_date),'YYYY-MM-DD')
		END IF	
END CHOOSE
		
RETURN ''
end function

public function integer nf_calculate_relative_days (datetime adtm_date_from, datetime adtm_date_to);/* A ‘month’ must consist of 30 days for calculation purposes if the benefit entitlement is associated with a monthly award.   See Rationale
	A monthly award is always based on 30 days.  For the month of February where there is less than 30 days, 
	if the entire month is part of the period then assume the month has 30 days.  
	If only a partial month is entered then the number of days will be less than 30 but the days could be 29 – 
	this would still be acceptable for February even though the month may only have 28 days.
*/
INTEGER     li_total_days, li_months_after, li_calculated_day_from , li_calculated_day_to, li_day_from
DATETIME	ldtm_input_from, ldtm_input_to, ldtm_end_date_of_month_from, ldtm_end_date_of_month_to, ldtm_first_date_of_month_to

//FROM DATE CALCULATION
ldtm_input_from = adtm_date_from
ldtm_input_to     = adtm_date_to

SELECT 	DATEDIFF(mm, :ldtm_input_from, :ldtm_input_to)
INTO 		:li_months_after
FROM 	sysobjects
USING 	SQLCA;	
SQLCA.nf_handle_error('n_maintain_benefit','nf_calculate_relative_days()','SELECT  DATEDIFF(mm, adtm_date_from, adtm_date_to)')

 SELECT 	dbo.udf_Last_Date_Of_Current_Month(:ldtm_input_from)
INTO		:ldtm_end_date_of_month_from
FROM 	sysobjects
USING	sqlca;
SQLCA.nf_handle_error('n_maintain_benefit', 'nf_calculate_relative_days()', 'dbo.udf_Last_Date_Of_Current_Month(:ldtm_input_from)')

SELECT 	dbo.udf_Last_Date_Of_Current_Month(:ldtm_input_to)
INTO		:ldtm_end_date_of_month_to
FROM 	sysobjects
USING	sqlca;
SQLCA.nf_handle_error('n_maintain_benefit', 'nf_calculate_relative_days()', 'SELECT dbo.udf_Last_Date_Of_Current_Month(:ldtm_to_date_of_month).')

SELECT 	dbo.udf_First_Date_Of_Next_Month(:ldtm_input_from)
INTO		:ldtm_first_date_of_month_to
FROM 	sysobjects
USING	sqlca;
SQLCA.nf_handle_error('n_maintain_benefit', 'nf_calculate_relative_days()', 'dbo.udf_First_Date_Of_Next_Month(:ldtm_to_date_of_month).')

//if first date of one month and 1st of next month = 1 month
IF DAY(date(ldtm_input_from)) = 1 AND date(ldtm_input_to) = date(ldtm_first_date_of_month_to) THEN 
	li_total_days = 30
	RETURN li_total_days
END IF

//if it's within the same month year just do a minus
IF year(date(ldtm_input_from)) = year(date(ldtm_input_to)) AND month(date(ldtm_input_from)) = month(date(ldtm_input_to)) THEN 
	//IF day = last day of month - against 30 else minus against end date
	IF  day(date(ldtm_input_to)) = day(date(ldtm_end_date_of_month_from)) THEN
		li_total_days = 30 - DAY(date(ldtm_input_from))
		IF li_total_days < 0 THEN li_total_days = 0
		RETURN li_total_days
	ELSE
		li_total_days = DAY(date(ldtm_input_to)) - DAY(date(ldtm_input_from))
		IF li_total_days > 30 THEN li_total_days = 30
		RETURN li_total_days	
	END IF 
END IF 

//calculate total months - first and last
li_months_after = li_months_after - 1
IF li_months_after < 0 THEN li_months_after = 0

li_day_from=   DAY(date(ldtm_input_from)) 
IF li_day_from > 30 THEN li_day_from = 30
//Everything is based on 30 sooo.
li_calculated_day_from = 30 - li_day_from

li_calculated_day_to = day(date(ldtm_input_to))

//MAKE SURE IT'S NOT > 30
IF li_calculated_day_to > 30 THEN li_calculated_day_to = 30

//calculate the total number of days that could be
li_total_days = (li_months_after * 30) + li_calculated_day_from + li_calculated_day_to

//make sure we have a valid value
IF ISNULL(li_total_days) THEN  li_total_days = 0

//return out
RETURN li_total_days

end function

public subroutine nf_get_opening_dates (long al_claim_no, long al_opening_no, string as_opening_type, ref datetime adt_check_start, ref datetime adt_check_end);/*
this function is used to grab the values for the following
7.190	The period from date of a benefit entitlement must be set to the benefit start date of the selected
		opening if the 60% option lump sum indicator is set to ‘Yes’.

7.200	The period to date of a benefit entitlement must be set to one day after the benefit start date of the 
		selected opening if the 60% option lump sum indicator is set to ‘Yes’.
*/
SELECT 	benefit_start_date,  benefit_end_date
INTO   	:adt_check_start, :adt_check_end
FROM 	OPENING
WHERE 	claim_no 				= :al_claim_no
AND     	opening_no 				= :al_opening_no
AND     	opening_type_code 	= :as_opening_type
USING  	SQLCA;

SQLCA.nf_handle_error("n_maintain_benefit","nf_get_opening_dates()","SELECT benefit_start_date, benefit_end_date")
end subroutine

public function integer nf_check_entitlement_overlap (datetime adtm_from_date, datetime adtm_to_date, long al_claim_no, integer al_row_no, u_dw_online a_udw_online, string as_payment_type_code, string as_opening_type, long al_opening_no, string as_claim_role_code);/*
7.70	A benefit entitlement must not overlap with any other benefit entitlement of the same opening type 
         and	 claim for the specified annuity account. 
-- revised to			
7.70	A benefit entitlement must not overlap with any other benefit entitlement of the same opening type 
		and	 claim for the specified annuity account if the annuity account is for an injured worker.
				
		no opening type -
		For those ones - go with the opening type associated with the payment type - found in the payment combination table. 
		ls_opening_type    = idw_benefit_entitlement.getitemstring(li_counter,'opening_type_code')

RETURNS -1 		FAILS BR
RETURN    0 		- VALID
RETURN   -2 		UNKNOWN ERROR

ADDED - If an opening type = '' THEN assume it is an 'SV'

*/

INTEGER 	li_counter, li_rowcount
LONG			ll_target_claim_no, ll_opening_no_target
DATETIME 	ldtm_target_from_date, ldtm_target_to_date
STRING		ls_payment_type_code, ls_opening_type_source, ls_opening_type_target, ls_target_percent_thing
STRING		ls_deleted_flag

li_rowcount =  a_udw_online.rowcount()

IF ISNULL(li_rowcount) OR li_rowcount <= 0 THEN RETURN 0

IF al_opening_no > 0 THEN
	 ls_opening_type_source = nf_get_opening_type(al_claim_no, al_opening_no) 
ELSE
	// grab the opening_type based on the payment_type_code only  
	ls_opening_type_source = nf_get_opening_type_from_combination(as_payment_type_code)
END IF 

IF  ls_opening_type_source = '' THEN //WE HAVE AN ERROR
	//RETURN -2
	ls_opening_type_target = 'SV'
END IF 


FOR li_counter = 1 TO li_rowcount
	
	IF li_counter = al_row_no THEN CONTINUE //SAME RECORD
	
	ldtm_target_from_date 		= a_udw_online.getitemdatetime(li_counter, 'benefit_entitlement_from_date')
	ldtm_target_to_date  			= a_udw_online.getitemdatetime(li_counter, 'benefit_entitlement_to_date')
	ll_target_claim_no      		= a_udw_online.getitemnumber(li_counter, 'claim_no')
	ls_payment_type_code		= a_udw_online.getitemstring(li_counter, 'payment_type_code')
	ll_opening_no_target 			= a_udw_online.getitemnumber(li_counter, 'opening_no')
	ls_target_percent_thing		= a_udw_online.getitemstring(li_counter, 'sixty_percent_flag')
	ls_deleted_flag              		= a_udw_online.getitemstring(li_counter, 'deleted_flag')
	
	IF ls_deleted_flag = 'Y'  THEN CONTINUE
	
	IF ll_opening_no_target > 0 THEN
		ls_opening_type_target = nf_get_opening_type(ll_target_claim_no, ll_opening_no_target) 
	ELSE
	 	ls_opening_type_target = nf_get_opening_type_from_combination(ls_payment_type_code)
	END IF 
	
	IF  ls_opening_type_target = '' THEN //WE HAVE AN ERROR
		ls_opening_type_target = 'SV'
		//RETURN -2
	END IF 
	
	// now do the checks but only if not 'SS'
	IF as_claim_role_code <> 'SS' THEN
		IF ll_target_claim_no 		  <> al_claim_no 					THEN CONTINUE//NOT APPLICABLE
		IF ls_opening_type_target <> ls_opening_type_source  	THEN CONTINUE//NOT APPLICABLE
	ELSE//SS
		IF ls_target_percent_thing = 'Y' AND ls_opening_type_target = 'S1' 	THEN CONTINUE//NOT APPLICABLE - PICK UP EXIST S1/Y
	END IF
	
	//IN BETWEEN DATES
	IF date(adtm_from_date) >= date(ldtm_target_from_date) AND date(adtm_from_date) < date(ldtm_target_to_date) THEN
		RETURN -1
	END IF 
	
	//in between to date
	IF date(adtm_to_date) > date(ldtm_target_from_date) AND date(adtm_to_date) <= date(ldtm_target_to_date) THEN
		RETURN -1
	END IF 
	
	//span both dates
	IF date(adtm_from_date) <= date(ldtm_target_from_date) AND date(adtm_to_date) >= date(ldtm_target_to_date) THEN
		RETURN -1
	END IF 	
NEXT

RETURN 1
end function

public function integer nf_get_annuity_dates (long al_annuity_account_no, long al_claim_no, long al_individual_no, string as_type, ref datetime adtm_annuity_start_date, ref datetime adtm_annuity_end_date);DATETIME	ldtm_null
n_common_annuity lnv_common_annuity
INTEGER		li_rtn, li_benefit_entitlement_rowcount
STRING		ls_message
U_DS			lds_reexamine_benefit_entitlement


lds_reexamine_benefit_entitlement = Create U_DS

setnull(ldtm_null)


IF as_type = 'SS' THEN
	lds_reexamine_benefit_entitlement.DataObject = 'ds_reexamine_benefit_entitlement_SS'
	lds_reexamine_benefit_entitlement.SetTransObject(SQLCA)
	li_benefit_entitlement_rowcount = lds_reexamine_benefit_entitlement.Retrieve(al_annuity_account_no,al_claim_no)
ELSE
	lds_reexamine_benefit_entitlement.DataObject = 'ds_reexamine_benefit_entitlement_IW'
	lds_reexamine_benefit_entitlement.SetTransObject(SQLCA)
	li_benefit_entitlement_rowcount = lds_reexamine_benefit_entitlement.Retrieve(al_annuity_account_no)
END IF 

SQLCA.nf_handle_error('n_maintain_benefit_entitlement', 'nf_get_annuity_dates()', 'lds_reexamine_benefit_entitlement.Retrieve')

IF lds_reexamine_benefit_entitlement.rowcount() <= 0 THEN 
	adtm_annuity_start_date = ldtm_null
	adtm_annuity_end_date  = ldtm_null
	
	DESTROY(lnv_common_annuity)
	DESTROY(lds_reexamine_benefit_entitlement)
	RETURN -1
END IF

IF as_type = 'SS' THEN
	adtm_annuity_start_date = lds_reexamine_benefit_entitlement.getitemdatetime(1,'benefit_start_date')
ELSE
	adtm_annuity_start_date = lds_reexamine_benefit_entitlement.getitemdatetime(1,'benefit_entitlement_25th_month')
END IF 

//dates taken from Kevin's stuff.
lnv_common_annuity = CREATE n_common_annuity
lnv_common_annuity.nf_get_annuity_end_date(al_individual_no, as_type, ls_message, adtm_annuity_end_date, ldtm_null, ldtm_null, ldtm_null)

RETURN 0
end function

public function integer nf_check_opening_covers_period (long al_claim_no, integer ai_openings[], datetime adtm_benefit_start, datetime adtm_benefit_end);/*
	A benefit entitlement must be associated with an opening if an opening exists that covers the benefit entitlement period.
*/
DATE	ldt_start, ldt_end
LONG ll_num
U_DS		lds_openings

ldt_start 		= DATE(adtm_benefit_start)
ldt_end 		= DATE(adtm_benefit_end)

lds_openings = Create U_DS
lds_openings.DataObject = 'ds_openings'
lds_openings.SetTransObject(SQLCA)

ll_num = lds_openings.Retrieve(al_claim_no,ai_openings,ldt_start,ldt_end)		
SQLCA.nf_handle_error('n_maintain_benefit','nf_check_opening_covers_period()','SELECT count(*) ')
	
IF ISNULL(ll_num) THEN  ll_num = 0	
			
RETURN ll_num
end function

public function string nf_get_opening_type_from_combination (string as_payment_type);/* RETURN THE opening_type_code if one does not exist for this record */
STRING	ls_type_code
INTEGER	li_count


SELECT count(*)  INTO :li_count
FROM   Payment_Combination 
WHERE payment_type_code = :as_payment_type
USING  SQLCA;

SQLCA.nf_handle_error('n_maintain_benefit','nf_get_opening_type_from_combination()','SELECT count(*)  INTO :li_count')

// NOTHING  this is a problem
IF li_count <> 1 THEN
	ls_type_code = ''
	RETURN ls_type_code
END IF 

SELECT opening_type_code  INTO :ls_type_code
FROM   Payment_Combination 
WHERE payment_type_code = :as_payment_type
USING  SQLCA;

SQLCA.nf_handle_error('n_maintain_benefit','nf_get_opening_type_from_combination()','SELECT opening_type_code  INTO :ls_type_code')

IF ISNULL(ls_type_code) THEN ls_type_code = ''

RETURN ls_type_code
end function

public function integer nf_check_benefit_calculations_days (integer ai_days, integer ai_hours, integer ai_weeks, datetime adtm_payment_from, datetime adtm_payment_to, long al_claim_no, long al_opening_no, long al_bencalc_no);/*
8.10	The sum of the days, hours and weeks for a benefit entitlement must not exceed the number of days 
          in the benefit entitlement period if the benefit entitlement is associated with a weekly award.
					
Scenario	Expected Results	Pass/Fail	Note
1	“Days” > total number of days in the payment period.	Receive an error message that the Days + Hours cannot exceed 
	the payment period		
2	 “Hours” > total number of days in the payment period.	Receive an error message that the Days + Hours cannot 
	exceed the payment period.		
	Days <> 0 Hours <> 0 “Days” +“Hours” > total number of days in the payment period. Receive an error message that the Days + Hours cannot exceed the payment period		
	Days <> 0 Hours <> 0 “Days” +“Hours” < total number of days in the payment period. Payment created/updated for Days and Hours entered		
	Days =  0 Hours <> 0 “Days” +“Hours” < total number of days in the payment period. Payment created/updated for Hours entered		
	Days <> 0 Hours =  0 “Days” +“Hours” < total number of days in the payment period. Payment created/updated for Days entered	
	
	NOTE: Revision -- For Post Implementation Issue.
	8.15	The sum of the days, hours and weeks for a benefit entitlement should not exceed the number of days in the
	         benefit entitlement period based on a twelve-hour day if the benefit entitlement is associated with a weekly award
			and no benefit calculation information is available.

	Rationale for 8.10 and 8.15

8.10	Use the benefit calculation associated with the entitlement to convert the hours in to days. 
         Use 7 days per week to convert the weeks into days. If no benefit calculation exists then convert the hours into days using 24 hours per day.

8.15	This is a warning to the user.  This is only necessary if a benefit calculation does not exist.  
         Use 7 days per week to convert the weeks into days and 12 hours per day to convert the hours into days.
	
returns: 	1 	-- 	ALL CLEAR
			-1 	- 	ERROR
			-2 	-- 	WARNING

*/

INTEGER 	li_days_after
DECIMAL		ldec_preacc_work_days_per_week, ldec_preacc_work_hours_per_day

//use days after for this one
li_days_after = DaysAfter(date(adtm_payment_from), date(adtm_payment_to))

IF isnull(li_days_after) THEN RETURN -1//NOT GOOD

IF ISNULL(ai_days) 	THEN ai_days 	= 0
IF ISNULL(ai_hours) 	THEN ai_hours 	= 0

/* grab some defaults from the original BenCalc -- if bencalc information doesn't exist default to 7-8*/
SELECT 	preacc_work_days_per_week, preacc_work_hours_per_day
INTO 		:ldec_preacc_work_days_per_week, :ldec_preacc_work_hours_per_day
FROM 	BENEFIT_CALCULATION 
WHERE 	claim_no 					= :al_claim_no
AND 		benefit_calculation_no 	= :al_bencalc_no
AND 		opening_no 					= :al_opening_no
USING 	SQLCA;
		
SQLCA.nf_handle_error('n_maintain_benefit','nf_check_benefit_calculation_days()','SELECT preacc_work_days_per_week, preacc_work_hours_per_day')
	
// default the Days -- subject to change
IF ISNULL(ldec_preacc_work_days_per_week) OR ldec_preacc_work_days_per_week = 0  THEN 
	ldec_preacc_work_days_per_week = 7.00
END IF 

//default the hours -- subject to change
IF ISNULL(ldec_preacc_work_hours_per_day) OR ldec_preacc_work_hours_per_day = 0  THEN 
	ldec_preacc_work_hours_per_day = 24
END IF
		

/*1	“Days” > total number of days in the payment period.	Receive an error message that the Days + Hours cannot exceed 
	the payment period	
*/
IF ai_days > li_days_after THEN RETURN -1

/*
2	 “Hours” > total number of days in the payment period.	Receive an error message that the Days + Hours cannot 
	exceed the payment period.		
	Days <> 0 Hours <> 0 “Days” +“Hours” > total number of days in the payment period. Receive an error message that the Days + Hours cannot exceed the payment period		
	Days <> 0 Hours <> 0 “Days” +“Hours” < total number of days in the payment period. Payment created/updated for Days and Hours entered		
	Days =  0 Hours <> 0 “Days” +“Hours” < total number of days in the payment period.	Payment created/updated for Hours entered		
	Days <> 0 Hours =  0 “Days” +“Hours” < total number of days in the payment period.	Payment created/updated for Days entered		
*/
IF (ai_hours/ldec_preacc_work_hours_per_day) > li_days_after THEN RETURN -1

//Days <> 0 Hours <> 0 “Days” +“Hours” > total number of days in the payment period. Receive an error message that the Days + Hours cannot exceed the payment period		
IF ((ai_hours/ ldec_preacc_work_hours_per_day) + ai_days) + (ai_weeks * ldec_preacc_work_days_per_week)   > li_days_after THEN RETURN -1

//case where there is no benefit calculation and no previous error based on 24 hours
IF ISNULL(al_bencalc_no) OR al_bencalc_no = 0 THEN 
	//if no bencalc check if we need to warn the user
	ldec_preacc_work_hours_per_day = 12
	IF ((ai_hours/ ldec_preacc_work_hours_per_day) + ai_days) + (ai_weeks * ldec_preacc_work_days_per_week)   > li_days_after THEN RETURN -2
END IF 


RETURN 1

end function

public function boolean nf_check_benefit_level_percent (string as_payment_type, long al_benefit_calculation_no, long al_claim_no);/* FROM BENEFIT_ENTITLEMENT a 
LEFT JOIN BENEFIT_CALCULATION b ON a.benefit_calculation_no = b.benefit_calculation_no AND a.claim_no = b.claim_no 

This seems to be partially enforced by the filtering on the payment type drop down, but not always. 
Because there is no BR defined, it is not checked on the Save or Validate.
For example, if I drag a payment over to the LOE Entitlement tab & it is the first one, there seems to be no filtering on the payment type. 
But if I drag another one over, there seems to be filtering on the payment type for the correct benefit level %, when I drop the list down. 
If some B/E already exist and I drag another payment over, there also seems to be filtering on the payment type when I drop the list down. 
However, if the original payment type was for example 90% and an 85% ben calc is selected for that B/E,
the module does not force you to change the payment type to match the benefit level % of the benefit calculation.

Add a new BR & datafix for existing records:
The payment type must have the same benefit level as the benefit calculation, if there is a benefit calculation associated with the benefit entitlement.

RETURNS 
TRUE 		- MATCH
FALSE 	- DON'T MATCH

*/

DECIMAL		ldec_blp_payment, ldec_blp_calculation

SELECT 	benefit_level_percentage
INTO 		:ldec_blp_payment
FROM  	Payment_Type
WHERE 	payment_type_code = :as_payment_type
USING 	SQLCA;	
			
SQLCA.nf_handle_error('n_maintain_benefit','nf_check_benefit_level_percent()','SELECT benefit_level_percent(1)')


SELECT 	benefit_level_percentage
INTO 		:ldec_blp_calculation
FROM  	BENEFIT_CALCULATION
WHERE 	benefit_calculation_no  	= :al_benefit_calculation_no
AND		claim_no						= :al_claim_no
USING 	SQLCA;	
			
SQLCA.nf_handle_error('n_maintain_benefit','nf_check_benefit_level_percent()','SELECT benefit_level_percent(2)')

//don't match
IF ldec_blp_payment <> ldec_blp_calculation THEN
	RETURN FALSE
END IF 

//all good
RETURN TRUE
end function

public function integer nf_check_paytype_requires_days (string as_payment_type);/*
Based on the following concept
The benefit entitlement payment type, for an injured worker, must be a payment type that requires days. 

returns 1 - requires days
return  0 - does not require days
RETURN -1 - ERROR SOMEWHERE - WRITE IT AS AN ERROR (needed in case there is a problem
*/
STRING		ls_check

SELECT 	days_hours_flag 
INTO   	:ls_check
FROM 	Payment_Type
WHERE 	payment_type_code = :as_payment_type 
USING 	SQLCA;
SQLCA.nf_handle_error('n_maintain_benefit', 'nf_check_paytype_requires_days()', 'SELECT days_hours_flag.... ')

CHOOSE CASE ls_check
	CASE 'Y'
		RETURN 1
	CASE 'N'
		RETURN 0
	CASE ELSE
		//WILL RETURN -1
END CHOOSE

RETURN -1//PROBLEM
end function

on n_maintain_benefit.create
call super::create
end on

on n_maintain_benefit.destroy
call super::destroy
end on

