$PBExportHeader$n_manual_financial.sru
forward
global type n_manual_financial from nonvisualobject
end type
end forward

global type n_manual_financial from nonvisualobject
end type
global n_manual_financial n_manual_financial

type variables

end variables

forward prototypes
public function integer nf_validate_setaside_pymt_sub_type (string as_claim_role_code, string as_payment_sub_type_code)
public function boolean nf_validate_cost_allocation_required (string as_payment_sub_type_code)
public function integer nf_validate_cost_alloc_operation_no (long al_cost_alloc_no, long al_cost_alloc_operation_no)
public function integer nf_validate_cost_alloc_no (long al_cost_alloc_no, ref string as_cost_alloc_type_code)
public function integer nf_cost_allocation_warnings (long al_claim_no, long al_cost_alloc_no)
public function integer nf_validate_explanation (string as_explanation)
public subroutine nf_display_cost_allocation_data (long al_cost_alloc_no, long al_cost_alloc_operation_no, ref string as_employer_name, ref string as_operation_name)
public function integer nf_validate_cost_allocation (string as_payment_sub_type_code, long al_claim_no, long al_cost_alloc_no, long al_cost_alloc_operation_no, string as_cost_alloc_type_code, ref boolean ab_set_cost_allocation, ref long al_default_cost_alloc_no, ref long al_default_cost_alloc_operation_no, ref string as_default_cost_alloc_type_code)
public function integer nf_retrieve_annuity_dates (long al_individual_no, long al_claim_no, string as_claim_role_code, ref datetime adtm_annuity_start_date, ref datetime adtm_annuity_end_date)
public function integer nf_validate_payment (long al_individual_no, long al_claim_no, datetime adtm_paid_from_date, datetime adtm_paid_to_date, string as_payment_sub_type_code, string as_claim_role_code)
public function integer nf_validate_paid_from_paid_to_date (long al_individual_no, long al_claim_no, string as_claim_role_code, datetime adtm_paid_from_date, datetime adtm_paid_to_date)
public function integer nf_get_default_cost_allocation (long al_claim_no, ref long al_cost_alloc_no, ref long al_cost_alloc_operation_no, ref string as_cost_alloc_type_code)
public subroutine nf_get_subledger_balance (long al_claim_no, long al_recipient_no, ref decimal adc_subledger_balance)
public subroutine nf_get_post92_subledger_balance (long al_claim_no, long al_recipient_no, ref decimal adc_subledger_balance)
public subroutine nf_get_pre93_subledger_balance (long al_claim_no, ref decimal adc_subledger_balance)
public function boolean nf_unapplied_cm_exists (long al_claim_no, long al_recipient_no, string as_mode)
public function integer nf_find_applied_annuity_payments (string as_payment_sub_type_code, date adt_dm_paid_from, date adt_dm_paid_to, long al_dm_cheque_no, decimal adc_dm_amount, long al_dm_recipient_no)
public function integer nf_find_unapplied_annuity_payments (string as_payment_sub_type_code, date adt_dm_paid_from, date adt_dm_paid_to, long al_dm_cheque_no, decimal adc_dm_amount, long al_dm_recipient_no)
public function integer nf_validation (string as_payment_sub_type_code, long al_claim_no, long al_cost_alloc_no, long al_cost_alloc_operation_no, string as_cost_alloc_type_code, ref boolean ab_set_cost_allocation, ref long al_default_cost_alloc_no, ref long al_default_cost_alloc_operation_no, ref string as_default_cost_alloc_type_code, string as_explanation, long al_cheque_no)
public function integer nf_validate_transaction (string as_payment_sub_type_code, string as_explanation, long al_cheque_no, long al_original_cheque_no, long al_recipient_no)
public function integer nf_validate_cheque_no (string as_payment_sub_type_code, long al_cheque_no, long al_original_cheque_no, long al_recipient_no)
end prototypes

public function integer nf_validate_setaside_pymt_sub_type (string as_claim_role_code, string as_payment_sub_type_code);INTEGER   li_count

// validate setaside payment sub types for the given claim role
// similar function used in add manual financial module

// 1.220 An annuity adjustment transaction for an injured worker must only be for a payment type that attracts annuities 
// for an injured worker.
// 1.230 An annuity adjustment transaction for a surviving spouse must only be for a payment type that attracts annuities 
// for a surviving spouse. 

CHOOSE CASE as_payment_sub_type_code
	CASE 'CM','DM','CO','DO','CW','DW','IN'
		// These payment sub-types are OK for injured workers & surviving spouses
	CASE ELSE
		// if count > 0 then it is a valid sub type for claim role
		SELECT Count(*)
		INTO   :li_count
		FROM   Claim_Role_Opening_Type_Xref a
		JOIN   Payment_Combination          b ON a.opening_type_code = b.opening_type_code
		JOIN   Payment_Type                 c ON b.payment_type_code = c.payment_type_code
		WHERE  a.claim_role_code          = :as_claim_role_code
		AND    a.annuity_eligibility_flag = 'Y'
		AND    b.payment_type_code        = :as_payment_sub_type_code
		AND    c.annuity_flag             = 'Y'
		USING SQLCA;
		SQLCA.nf_handle_error( 'w_maintain_manual_financial_trns', 'wf_validate_setaside_payment_type', 'embedded SQL: select count(*) from Claim_Role_Opening_Type_Xref...')
		
		// exception
		IF as_payment_sub_type_code = '.5' AND as_claim_role_code = 'C' THEN
			// annuity 90% Short-Term Irregular LOE adjustments are OK for claimants only
			RETURN 0
		END IF
		
		IF li_count = 0 THEN
			MessageBox("Annuity Validation Error","The selected annuity payment sub type is not applicable for this recipient.",Exclamation!)
			RETURN -1
		END IF
END CHOOSE

RETURN 0
end function

public function boolean nf_validate_cost_allocation_required (string as_payment_sub_type_code);STRING   ls_cost_alloc_required_flag

SELECT cost_alloc_required_flag
INTO   :ls_cost_alloc_required_flag
FROM   Payment_Sub_Type
WHERE  payment_type_code = '97'
AND    payment_sub_type_code = :as_payment_sub_type_code
USING SQLCA;
SQLCA.NF_Handle_error('n_manual_financial', 'nf_validate_cost_allocation_required', 'embedded SQL: SELECT cost_alloc_required_flag FROM Payment_Sub_Type...')

IF ls_cost_alloc_required_flag = 'N' THEN
	RETURN FALSE
ELSE
	RETURN TRUE
END IF
end function

public function integer nf_validate_cost_alloc_operation_no (long al_cost_alloc_no, long al_cost_alloc_operation_no);INTEGER  li_count

SELECT Count(*)
INTO   :li_count
FROM   OPERATION
WHERE	 employer_no = :al_cost_alloc_no
AND    operation_no = :al_cost_alloc_operation_no
USING SQLCA;
SQLCA.nf_handle_error('n_manual_financial', 'nf_validate_cost_alloc_operation_no', 'SELECT Count(*) FROM OPERATION...')

IF li_count = 0 THEN
	MessageBox('Cost Allocation Error','The employer/operation combination is not valid. Please enter a different operation or cancel the annuity payment.')
	RETURN -1
ELSE
	RETURN 0
END IF

end function

public function integer nf_validate_cost_alloc_no (long al_cost_alloc_no, ref string as_cost_alloc_type_code);INTEGER  li_count

SELECT employer_type_code
INTO   :as_cost_alloc_type_code
FROM   EMPLOYER
WHERE	 employer_no = :al_cost_alloc_no
USING SQLCA ;
SQLCA.nf_handle_error('n_manual_financial', 'nf_validate_cost_alloc_no', 'wf_validate_cost_allocation - SELECT Count(*) FROM EMPLOYER...')

IF IsNull(as_cost_alloc_type_code) OR as_cost_alloc_type_code = '' THEN
	MessageBox('Cost Allocation Error','The cost allocation employer entered is not valid. Please enter a different employer or cancel the annuity payment.')
	RETURN -1
ELSE
	RETURN 0
END IF

end function

public function integer nf_cost_allocation_warnings (long al_claim_no, long al_cost_alloc_no);// nf_cost_allocation_warnings
// display warnings to user where appropriate
String  ls_receiving_salary_flag
Integer li_reply


// Display a warning message if the employer number is the Pension Account.
IF al_cost_alloc_no = 10002 THEN
	li_reply = MessageBox("Warning!","This is the Pension Account! Do you wish to proceed?", Question!, YesNo!)
	IF li_reply = 2 THEN
		RETURN -1
	END IF
END IF


// Get the receiving salary flag.
SELECT receiving_salary_flag
INTO   :ls_receiving_salary_flag
FROM   CLAIM
WHERE  claim_no = :al_claim_no
USING SQLCA;
SQLCA.nf_handle_error('w_add_manual_financial_trns', '', 'wf_validate_cost_alloc_no - Embedded SQL: select from CLAIM')

//	If the Claim is a receiving salary claim and the cost allocation is NOT a deposit account employer, display
// a warning message.
IF ls_receiving_salary_flag = 'Y' AND (al_cost_alloc_no < 50000 OR al_cost_alloc_no > 100000) THEN
	li_reply = MessageBox("Warning!", "This transaction is for a receiving salary claim using a non deposit account employer. Do you wish to continue?",Question!,YesNo!,1) 
	IF li_reply = 2 THEN
		RETURN -1
	END IF
END IF

RETURN 0
	
end function

public function integer nf_validate_explanation (string as_explanation);IF IsNull(as_explanation) THEN 
	MessageBox("Annuity Validation Error","An explanation must be entered for all payments.",Exclamation!)
	RETURN -1
END IF

IF Len(as_explanation) = 0 THEN 
	MessageBox("Annuity Validation Error","An explanation must be entered for all payments.",Exclamation!)
	RETURN -1
END IF

RETURN 0
end function

public subroutine nf_display_cost_allocation_data (long al_cost_alloc_no, long al_cost_alloc_operation_no, ref string as_employer_name, ref string as_operation_name);// This function is to be called after cost allocation has been validated (for add module) or
// on rowfocuschanged (for maintain module)
// it will return cost allocation data

SELECT employer_name
INTO   :as_employer_name
FROM   EMPLOYER_NAME
WHERE  employer_no             = :al_cost_alloc_no 
AND    employer_name_type_code = 'L'
USING SQLCA;
SQLCA.nf_handle_error('w_add_manual_financial_trns', 'wf_display_cost_allocation_data', 'Embedded SQL: select employer_name from EMPLOYER_NAME...')

SELECT operation_name 
INTO   :as_operation_name
FROM   OPERATION 
WHERE  employer_no      = :al_cost_alloc_no
AND    operation_no = :al_cost_alloc_operation_no
USING SQLCA;
SQLCA.nf_handle_error('w_add_manual_financial_trns', 'wf_display_cost_allocation_data','SELECT operation_name FROM OPERATION...')

end subroutine

public function integer nf_validate_cost_allocation (string as_payment_sub_type_code, long al_claim_no, long al_cost_alloc_no, long al_cost_alloc_operation_no, string as_cost_alloc_type_code, ref boolean ab_set_cost_allocation, ref long al_default_cost_alloc_no, ref long al_default_cost_alloc_operation_no, ref string as_default_cost_alloc_type_code);BOOLEAN   lb_cost_allocation_required
INTEGER   li_rtn, li_msg_rtn
LONG      ll_cost_alloc_no, ll_cost_alloc_operation_no
STRING    ls_cost_alloc_type_code


lb_cost_allocation_required = nf_validate_cost_allocation_required(as_payment_sub_type_code)
IF lb_cost_allocation_required = TRUE THEN
	IF al_cost_alloc_no = 0 THEN
		nf_get_default_cost_allocation(al_claim_no,al_default_cost_alloc_no,al_default_cost_alloc_operation_no,as_default_cost_alloc_type_code)
		MessageBox('Cost Allocation Error','Cost allocation is required for annuity payment sub type ' + as_payment_sub_type_code + '.' &
		                                 + '~r~nPlease enter cost allocation.', Exclamation!)
		ab_set_cost_allocation = TRUE
		al_default_cost_alloc_no = 0
		al_default_cost_alloc_operation_no = 0
		as_default_cost_alloc_type_code = ''
		RETURN -1
	END IF
ELSE
	IF al_cost_alloc_no <> 0 OR al_cost_alloc_operation_no <> 0 OR as_cost_alloc_type_code <> '' THEN
		MessageBox('Cost Allocation Error','Cost allocation is not required for annuity payment sub type ' + as_payment_sub_type_code + '.' &
		                                 + '~r~nThe cost allocation will be cleared.', Exclamation!)
		ab_set_cost_allocation = TRUE
		al_default_cost_alloc_no = 0
		al_default_cost_alloc_operation_no = 0
		as_default_cost_alloc_type_code = ''
		RETURN -1
	END IF
END IF

IF lb_cost_allocation_required THEN
	li_rtn = nf_validate_cost_alloc_no(al_cost_alloc_no, as_cost_alloc_type_code)
	IF li_rtn < 0 THEN
		MessageBox('Cost Allocation Error','There is no employer that matches the cost allocation entered.' &
														+ '~r~nPlease enter a valid cost allocation.', Exclamation!)
		RETURN li_rtn
	END IF
	
	li_rtn = nf_validate_cost_alloc_operation_no(al_cost_alloc_no,al_cost_alloc_operation_no)
	IF li_rtn < 0 THEN
		MessageBox('Cost Allocation Error','There is no operation that matches the cost allocation entered.' &
														+ '~r~nPlease enter a valid cost allocation operation.', Exclamation!)
		RETURN li_rtn
	END IF
	
	SELECT cost_alloc_no,
			 cost_alloc_operation_no,
			 cost_alloc_type_code
	INTO   :ll_cost_alloc_no,
			 :ll_cost_alloc_operation_no,
			 :ls_cost_alloc_type_code
	FROM   CLAIM
	WHERE  claim_no = :al_claim_no
	USING SQLCA;
	SQLCA.nf_handle_error('n_manual_financial', 'nf_validate_cost_allocation', 'embedded SQL: SELECT cost_alloc_no,cost_alloc_operation_no,cost_alloc_type_code FROM CLAIM...')
	
	IF ( al_cost_alloc_no <> ll_cost_alloc_no ) OR &
		( al_cost_alloc_operation_no <> ll_cost_alloc_operation_no ) OR &
		( as_cost_alloc_type_code <> ls_cost_alloc_type_code ) THEN
		li_msg_rtn = MessageBox('Restore Cost Allocation?','The cost allocation for this payment does not currently match the claim cost allocation.'&
																  + '~r~nWould you like to restore the cost allocation to match the claim data and attempt to save again?',Question!,YesNo!,1)
		IF li_msg_rtn = 1 THEN
			// restore to claim values
			ab_set_cost_allocation = TRUE
			al_default_cost_alloc_no = ll_cost_alloc_no
			al_default_cost_alloc_operation_no = ll_cost_alloc_operation_no
			as_default_cost_alloc_type_code = ls_cost_alloc_type_code
			RETURN -1
		END IF
	END IF
ELSE
	// no need to validate cost alloc if it's not required
END IF

RETURN 0
end function

public function integer nf_retrieve_annuity_dates (long al_individual_no, long al_claim_no, string as_claim_role_code, ref datetime adtm_annuity_start_date, ref datetime adtm_annuity_end_date);INTEGER   li_rowcount
U_DS      lds_annuity

lds_annuity = Create U_DS
lds_annuity.DataObject = 'ds_annuity'
lds_annuity.SetTransObject(SQLCA)

IF as_claim_role_code = 'C' THEN
	li_rowcount = lds_annuity.Retrieve(al_individual_no,0)
	SQLCA.nf_handle_error('w_add_manual_financial_trns', 'wf_eligible_for_annuity', 'retrieve lds_annuity (1)')	
ELSE	
	li_rowcount = lds_annuity.Retrieve(al_individual_no,al_claim_no)
	SQLCA.nf_handle_error('w_add_manual_financial_trns', 'wf_eligible_for_annuity', 'retrieve lds_annuity (2)')
END IF

IF li_rowcount > 0 THEN
	adtm_annuity_start_date = lds_annuity.GetItemDateTime(1,'annuity_start_date')
	adtm_annuity_end_date = lds_annuity.GetItemDateTime(1,'annuity_end_date')
ELSE
	IF as_claim_role_code = 'C' THEN
		MessageBox('No Annuity Eligibility','This claimant (individual# '+String(al_individual_no)+') either has no active annuity eligibility or has no annuity eligibility confirmed.',Exclamation!)
		RETURN -1
	ELSE
		MessageBox('No Annuity Eligibility','This surviving spouse (claim# '+String(al_claim_no)+') either has no active annuity eligibility or has no annuity eligibility confirmed.',Exclamation!)
		RETURN -1
	END IF
END IF

RETURN 0
end function

public function integer nf_validate_payment (long al_individual_no, long al_claim_no, datetime adtm_paid_from_date, datetime adtm_paid_to_date, string as_payment_sub_type_code, string as_claim_role_code);INTEGER li_rtn

// function to validate PAYMENT.paid_from_date for the Add & Maintain modules
IF as_payment_sub_type_code <> '' THEN
	li_rtn = nf_validate_setaside_pymt_sub_type(as_claim_role_code,as_payment_sub_type_code)
	IF li_rtn < 0 THEN RETURN li_rtn
END IF

// function to validate PAYMENT.paid_from_date for the Add & Maintain modules
li_rtn = nf_validate_paid_from_paid_to_date(al_individual_no,al_claim_no,as_claim_role_code,adtm_paid_from_date,adtm_paid_to_date)
IF li_rtn < 0 THEN RETURN li_rtn

RETURN 0
end function

public function integer nf_validate_paid_from_paid_to_date (long al_individual_no, long al_claim_no, string as_claim_role_code, datetime adtm_paid_from_date, datetime adtm_paid_to_date);DATETIME   ldtm_annuity_start_date, ldtm_annuity_end_date
INTEGER    li_rtn

// validate BRs associated with the paid from date and the paid to date
// return error where applicable


li_rtn = nf_retrieve_annuity_dates(al_individual_no, al_claim_no, as_claim_role_code, ldtm_annuity_start_date, ldtm_annuity_end_date)
IF li_rtn < 0 THEN RETURN li_rtn

// 2.90 Payment from date must not be before the annuity start date, if the individual is a surviving spouse. 
IF as_claim_role_code = 'SS' THEN
	IF adtm_paid_from_date < ldtm_annuity_start_date THEN
		MessageBox('Paid From Date Error','The paid from date cannot be less than the annuity start date ('+String(ldtm_annuity_start_date,"yyyy-mm-dd")+').',Exclamation!)
		RETURN -1
	END IF
ELSE
	// 2.50 The payment from date must not be before the annuity start date, if the individual is 
	// an injured worker and the payment involves post-1992 annuity benefits.
	IF Date(ldtm_annuity_start_date) > 1993-01-01 THEN
		// post-1993 claimant
		IF adtm_paid_from_date < ldtm_annuity_start_date THEN
			MessageBox('Paid From Date Error','The payment from date must not be before the annuity start date ('+String(ldtm_annuity_start_date,"yyyy-mm-dd")+')' &
													+'~r~nif the individual is an injured worker and the payment involves post-1992 annuity benefits.',Exclamation!)
			RETURN -1
		END IF
	END IF
END IF

// 1.150 The “Pd To” date must be after the “Pd From” date.
IF adtm_paid_from_date >= adtm_paid_to_date THEN
	MessageBox('Paid Date Error','The paid to date must be after the paid from date.',Exclamation!)
	RETURN -1
END IF


// 1.160 The “Pd To” must not be more than one day in the future.
IF Date(adtm_paid_to_date) > RelativeDate(Date(f_server_datetime()),1) THEN
	MessageBox('Paid To Date Error','The paid to date must not be more than one day in the future.',Exclamation!)
	RETURN -1
END IF

// 2.60	The payment period to date must be less than or equal to the first day of the month following the annuity end date.
IF Date(adtm_paid_to_date) > RelativeDate(Date(ldtm_annuity_end_date),1) THEN
	MessageBox('Paid To Date Error','The payment period to date must be less than or equal to'&
									+'~r~nthe first day of the month following the annuity end date.',Exclamation!)
	RETURN -1
END IF



RETURN 0
end function

public function integer nf_get_default_cost_allocation (long al_claim_no, ref long al_cost_alloc_no, ref long al_cost_alloc_operation_no, ref string as_cost_alloc_type_code);SELECT cost_alloc_no,
       cost_alloc_operation_no,
       cost_alloc_type_code
INTO   :al_cost_alloc_no,
       :al_cost_alloc_operation_no,
       :as_cost_alloc_type_code
FROM   CLAIM
WHERE  claim_no = :al_claim_no
USING SQLCA;
SQLCA.nf_handle_error('n_manual_financial', 'nf_get_default_cost_allocation', 'embedded SQL: SELECT cost_alloc_no, cost_alloc_operation_no, cost_alloc_type_code FROM CLAIM...')

IF IsNull(al_cost_alloc_no) THEN
	al_cost_alloc_no = 0
   al_cost_alloc_operation_no = 0
   as_cost_alloc_type_code = ''
	RETURN -1
ELSE
	RETURN 0
END IF

end function

public subroutine nf_get_subledger_balance (long al_claim_no, long al_recipient_no, ref decimal adc_subledger_balance);DECIMAL {4}   ldec_applied_subledger_sum, ldec_unapplied_subledger_sum


//Get the subledger balance
SELECT IsNull(Sum(a.net_payment_amount),0)
INTO   :ldec_applied_subledger_sum
FROM   PAYMENT a
JOIN   APPLIED_CLAIM_TXN b ON a.payment_no = b.payment_no
WHERE  a.payment_type_code = '97' 
AND    a.claim_no = :al_claim_no
AND    b.recipient_no = :al_recipient_no
Using SQLCA;
SQLCA.nf_handle_error('n_manual_financial','nf_get_subledger_balance','Select sum(a.net_payment_amount) From PAYMENT a, APPLIED_CLAIM_TXN b')

SELECT IsNull(Sum(a.net_payment_amount),0)
INTO   :ldec_unapplied_subledger_sum
FROM   PAYMENT a
JOIN   UNAPPLIED_CLAIM_TXN b ON a.payment_no = b.payment_no
WHERE  a.payment_type_code = '97' 
AND    a.claim_no = :al_claim_no
AND    b.recipient_no = :al_recipient_no
Using SQLCA;
SQLCA.nf_handle_error('n_manual_financial','nf_get_subledger_balance','Select sum(a.net_payment_amount) From PAYMENT a, UNAPPLIED_CLAIM_TXN b')

adc_subledger_balance = ldec_applied_subledger_sum + ldec_unapplied_subledger_sum
end subroutine

public subroutine nf_get_post92_subledger_balance (long al_claim_no, long al_recipient_no, ref decimal adc_subledger_balance);DECIMAL {4}   ldec_applied_subledger_sum, ldec_unapplied_subledger_sum


//Get the subledger balance
SELECT IsNull(Sum(a.net_payment_amount),0)
INTO   :ldec_applied_subledger_sum
FROM   PAYMENT a
JOIN   APPLIED_CLAIM_TXN b ON a.payment_no = b.payment_no
WHERE  a.payment_type_code = '97' 
AND    a.claim_no     = :al_claim_no
AND    b.recipient_no = :al_recipient_no
AND NOT EXISTS (SELECT *
                FROM   ANNUITY_PAYOUT_PRE_1993 c
                WHERE  c.claim_no = a.claim_no
                AND    c.payout_payment_no = a.payment_no)
AND NOT EXISTS (SELECT *
                FROM   ANNUITY_SETASIDE_PRE_1993 d
                WHERE  d.claim_no = a.claim_no
                AND    d.setaside_payment_no = a.payment_no)
Using SQLCA;
SQLCA.nf_handle_error('n_manual_financial','nf_get_post92_subledger_balance','Select sum(a.net_payment_amount) From PAYMENT a, APPLIED_CLAIM_TXN b')


SELECT IsNull(Sum(a.net_payment_amount),0)
INTO   :ldec_unapplied_subledger_sum
FROM   PAYMENT a
JOIN   UNAPPLIED_CLAIM_TXN b ON a.payment_no = b.payment_no
WHERE  a.payment_type_code = '97' 
AND    a.claim_no     = :al_claim_no
AND    b.recipient_no = :al_recipient_no
AND NOT EXISTS (SELECT *
                FROM   ANNUITY_PAYOUT_PRE_1993 c
                WHERE  c.claim_no = a.claim_no
                AND    c.payout_payment_no = a.payment_no)
AND NOT EXISTS (SELECT *
                FROM   ANNUITY_SETASIDE_PRE_1993 d
                WHERE  d.claim_no = a.claim_no
                AND    d.setaside_payment_no = a.payment_no)
Using SQLCA;
SQLCA.nf_handle_error('n_manual_financial','nf_get_post92_subledger_balance','Select sum(a.net_payment_amount) From PAYMENT a, UNAPPLIED_CLAIM_TXN b')


adc_subledger_balance = ldec_applied_subledger_sum + ldec_unapplied_subledger_sum
end subroutine

public subroutine nf_get_pre93_subledger_balance (long al_claim_no, ref decimal adc_subledger_balance);DECIMAL {4}   ldec_subledger_payout_sum, ldec_subledger_setaside_sum



//Get the subledger balance
SELECT IsNull(Sum(a.net_payment_amount),0)
INTO   :ldec_subledger_payout_sum
FROM   PAYMENT a
JOIN   ANNUITY_PAYOUT_PRE_1993 b ON a.payment_no = b.payout_payment_no
Where  a.claim_no = b.claim_no
AND    a.claim_no = :al_claim_no
USING SQLCA;
SQLCA.nf_handle_error('n_manual_financial','wf_setup_pre93_txns','Select sum(a.net_payment_amount) 1')

SELECT IsNull(Sum(a.net_payment_amount),0)
INTO   :ldec_subledger_setaside_sum
FROM   PAYMENT a
JOIN   ANNUITY_SETASIDE_PRE_1993 b ON a.payment_no = b.setaside_payment_no
Where  a.claim_no = b.claim_no
AND    a.claim_no = :al_claim_no
USING SQLCA;
SQLCA.nf_handle_error('n_manual_financial','wf_setup_pre93_txns','Select sum(a.net_payment_amount) 2')

adc_subledger_balance = ldec_subledger_payout_sum + ldec_subledger_setaside_sum

end subroutine

public function boolean nf_unapplied_cm_exists (long al_claim_no, long al_recipient_no, string as_mode);INTEGER   li_count

// this function is used to determine the existence of an Annuity Payment (97/CM) for a specific claim and recipient
// if the user is trying to add a sub-ledger payment, and there the existing subledger balance is zero, and there 
// are unapplied Annuity Payments for this individual and claim, then the user must be prevented from adding the new sub-ledger payment.
// they are supposed to deleted the Annuity Payment, add this new sub-ledger payment and re-add the Annuity Pyament

CHOOSE CASE as_mode
	CASE 'post92'
		SELECT Count(*)
		INTO   :li_count
		FROM   PAYMENT a
		JOIN   UNAPPLIED_CLAIM_TXN b ON a.payment_no = b.payment_no
		WHERE  a.payment_type_code = '97'
		AND    a.payment_sub_type_code in ('CM', 'CO', 'CW')
		AND    b.claim_no = :al_claim_no
		AND    b.recipient_no = :al_recipient_no
		AND    b.recipient_type_code = 'I'
		AND NOT EXISTS ( SELECT *
		                 FROM   ANNUITY_PAYOUT_PRE_1993 c
							  WHERE  c.payout_payment_no = a.payment_no )
		USING SQLCA;
		SQLCA.nf_handle_error('n_manual_financial','nf_unapplied_CM_exists','embedded SQL: SELECT Count(*) FROM PAYMENT,UNAPPLIED_CLAIM_TXN (1)...')
		
	CASE 'pre93'
		SELECT Count(*)
		INTO   :li_count
		FROM   PAYMENT a
		JOIN   UNAPPLIED_CLAIM_TXN b ON a.payment_no = b.payment_no
		JOIN   ANNUITY_PAYOUT_PRE_1993 c ON a.payment_no = c.payout_payment_no
		WHERE  a.payment_type_code = '97'
		AND    a.payment_sub_type_code in ('CM', 'CO', 'CW')
		AND    b.claim_no = :al_claim_no
		AND    b.recipient_no = :al_recipient_no
		AND    b.recipient_type_code = 'I'
		USING SQLCA;
		SQLCA.nf_handle_error('n_manual_financial','nf_unapplied_CM_exists','embedded SQL: SELECT Count(*) FROM PAYMENT,UNAPPLIED_CLAIM_TXN (2)...')
		
	CASE 'SS'
		SELECT Count(*)
		INTO   :li_count
		FROM   PAYMENT a
		JOIN   UNAPPLIED_CLAIM_TXN b ON a.payment_no = b.payment_no
		WHERE  a.payment_type_code = '97'
		AND    a.payment_sub_type_code in ('CM', 'CO', 'CW')
		AND    b.claim_no = :al_claim_no
		AND    b.recipient_no = :al_recipient_no
		AND    b.recipient_type_code = 'I'
		USING SQLCA;
		SQLCA.nf_handle_error('n_manual_financial','nf_unapplied_CM_exists','embedded SQL: SELECT Count(*) FROM PAYMENT,UNAPPLIED_CLAIM_TXN (3)...')
		
END CHOOSE

IF li_count = 0 THEN
	RETURN FALSE
ELSE
	RETURN TRUE
END IF
end function

public function integer nf_find_applied_annuity_payments (string as_payment_sub_type_code, date adt_dm_paid_from, date adt_dm_paid_to, long al_dm_cheque_no, decimal adc_dm_amount, long al_dm_recipient_no);INTEGER    li_count
STRING     ls_matching_sub_type_code

IF as_payment_sub_type_code = 'DM' then ls_matching_sub_type_code = 'CM'
IF as_payment_sub_type_code = 'DO' then ls_matching_sub_type_code = 'CO'
IF as_payment_sub_type_code = 'DW' then ls_matching_sub_type_code = 'CW'

SELECT Count(*)
INTO   :li_count
FROM   PAYMENT a
JOIN   APPLIED_CLAIM_TXN b ON a.payment_no = b.payment_no
WHERE  a.paid_from_date = :adt_DM_paid_from
AND    a.paid_to_date   = :adt_DM_paid_to
AND    b.cheque_no      = :al_DM_cheque_no
AND    b.txn_amount     = :adc_DM_amount * -1
AND    b.recipient_no   = :al_DM_recipient_no
AND    a.payment_type_code = '97'
AND    a.payment_sub_type_code = :ls_matching_sub_type_code
USING SQLCA;
SQLCA.nf_handle_error('n_manual_financial','nf_find_applied_annuity_payments','embedded SQL: SELECT Count(*) FROM PAYMENT, UNAPPLIED_CLAIM_TXN...')

IF li_count = 0 THEN
	MessageBox('BR2.170, 2.172, 2.174','A Void Annuity Payment (i.e. 97/' + as_payment_sub_type_code + ') must have all of the following:' &
                  +'~r~n·	The same amount as the Annuity Payment (i.e. 97/' + ls_matching_sub_type_code + ') but of the opposite sign' &
                  +'~r~n·	The same cheque number as the Annuity Payment' &
                  +'~r~n·	The same payment period as the Annuity Payment.' &
                  +'~r~n·	The same recipient as the Annuity Payment.',Exclamation!)
	RETURN li_count
END IF
end function

public function integer nf_find_unapplied_annuity_payments (string as_payment_sub_type_code, date adt_dm_paid_from, date adt_dm_paid_to, long al_dm_cheque_no, decimal adc_dm_amount, long al_dm_recipient_no);INTEGER    li_count
STRING     ls_matching_sub_type_code

IF as_payment_sub_type_code = 'DM' then ls_matching_sub_type_code = 'CM'
IF as_payment_sub_type_code = 'DO' then ls_matching_sub_type_code = 'CO'
IF as_payment_sub_type_code = 'DW' then ls_matching_sub_type_code = 'CW'

SELECT Count(*)
INTO   :li_count
FROM   PAYMENT a
JOIN   UNAPPLIED_CLAIM_TXN b ON a.payment_no = b.payment_no
WHERE  a.paid_from_date = :adt_DM_paid_from
AND    a.paid_to_date   = :adt_DM_paid_to
AND    b.cheque_no      = :al_DM_cheque_no
AND    b.txn_amount     = :adc_DM_amount * -1
AND    b.recipient_no   = :al_DM_recipient_no
AND    a.payment_type_code = '97'
AND    a.payment_sub_type_code = :ls_matching_sub_type_code
USING SQLCA;
SQLCA.nf_handle_error('n_manual_financial','nf_find_unapplied_annuity_payments','embedded SQL: SELECT Count(*) FROM PAYMENT, UNAPPLIED_CLAIM_TXN...')

IF li_count > 0 THEN
	MessageBox('BR 2.180, 2.182, 2.184','A Void Annuity Payment (i.e. 97/DM, DO, DW) must not be saved, if there is a matching scheduled Annuity Payment (i.e. 97/CM, CO, CW).',Exclamation!)
	RETURN li_count
END IF
end function

public function integer nf_validation (string as_payment_sub_type_code, long al_claim_no, long al_cost_alloc_no, long al_cost_alloc_operation_no, string as_cost_alloc_type_code, ref boolean ab_set_cost_allocation, ref long al_default_cost_alloc_no, ref long al_default_cost_alloc_operation_no, ref string as_default_cost_alloc_type_code, string as_explanation, long al_cheque_no);BOOLEAN   lb_cost_allocation_required
INTEGER   li_rtn, li_msg_rtn
LONG      ll_cost_alloc_no, ll_cost_alloc_operation_no
STRING    ls_cost_alloc_type_code

li_rtn = nf_validate_cost_allocation(as_payment_sub_type_code, al_claim_no, al_cost_alloc_no, al_cost_alloc_operation_no, as_cost_alloc_type_code, &
                                                          ab_set_cost_allocation, al_default_cost_alloc_no, al_default_cost_alloc_operation_no, as_default_cost_alloc_type_code)
IF li_rtn < 0 THEN RETURN li_rtn

//li_rtn = nf_validate_transaction(as_payment_sub_type_code, as_explanation, al_cheque_no)
IF li_rtn < 0 THEN RETURN li_rtn

RETURN 0
end function

public function integer nf_validate_transaction (string as_payment_sub_type_code, string as_explanation, long al_cheque_no, long al_original_cheque_no, long al_recipient_no);INTEGER li_rtn

li_rtn = nf_validate_explanation(as_explanation)
IF li_rtn < 0 THEN RETURN li_rtn

IF IsNull(al_cheque_no) THEN al_cheque_no = 0
li_rtn = nf_validate_cheque_no(as_payment_sub_type_code,al_cheque_no, al_original_cheque_no, al_recipient_no )
IF li_rtn < 0 THEN RETURN li_rtn

RETURN 0
end function

public function integer nf_validate_cheque_no (string as_payment_sub_type_code, long al_cheque_no, long al_original_cheque_no, long al_recipient_no);long ll_min_cheque_no, ll_max_cheque_no, ll_count


IF as_payment_sub_type_code = 'CM' OR as_payment_sub_type_code = 'CO'  THEN
	IF al_cheque_no = 0 AND al_original_cheque_no  = 0 THEN
		MessageBox("Annuity Validation Error","For Annuity Payments with payment sub-type of 'CM' and 'CO', the cheque number must be a valid cheque number and cannot be blank.",Exclamation!)
		RETURN -1
	ELSEIF al_cheque_no = 0 AND al_original_cheque_no  <> 0 THEN
		// This is OK, user made a mistake and is still able to reverse, or reset the cheque_no to 0
		// Do nothing
	ELSEIF al_cheque_no < 0 THEN
		MessageBox("Annuity Validation Error","For Annuity Payments with payment sub-type of 'CM' and 'CO', the cheque number must be a valid cheque number and cannot be negative.",Exclamation!)
		RETURN -1
	END IF
	
	IF al_cheque_no <> 0 THEN
		
		Select min_handwritten_cheque_no,max_handwritten_cheque_no
		Into :ll_min_cheque_no, :ll_max_cheque_no
		From Handwritten_Cheque_No_Range
		Using SQLCA;
			
		SQLCA.nf_handle_error('n_manual_financial','nf_validate_cheque_no','Select from Handwritten_Cheque_No_Range')
					
		IF (al_cheque_no < ll_min_cheque_no) OR (al_cheque_no > ll_max_cheque_no)  THEN
				MessageBox('Error','The Cheque No must be within the allowable range of handwritten cheque numbers between '+String(ll_min_cheque_no)+' and '+String(ll_max_cheque_no)+' .',exclamation!)
				Return -1
		END IF
		
			// The new or modified cheque number for a manual annuity sub-ledger payment must not exist on other scheduled payment/transactions for other recipients. 
	
		Select count(*)
		Into    :ll_count
		From  UNAPPLIED_CLAIM_TXN
		Where cheque_no = :al_cheque_no
		and   recipient_no <> :al_recipient_no  
		Using SQLCA;
	
		SQLCA.nf_handle_error('n_validate_cheque_no','nf_validate_cheque_no','Select from UNAPPLIED_CLAIM_TXN')
	
		IF ll_count > 0 THEN 
			MessageBox('Error','The cheque no must not exist on other sceduled payment/transactions for other recipients.',Exclamation!)
			 Return -1
		END IF
		
	END IF
	
	
ELSEIF as_payment_sub_type_code = 'DM' OR as_payment_sub_type_code = 'DO' THEN
	// OK as zero or non-zero, as long as it matches the 97/CM
ELSE
	IF al_cheque_no <> 0 THEN
		MessageBox("Annuity Validation Error","For subledger payments other than Annuity Payments and Void Annuity Payments, the cheque number must be blank.",Exclamation!)
		RETURN -1
	END IF
END IF
end function

on n_manual_financial.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_manual_financial.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

