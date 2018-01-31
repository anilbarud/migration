$PBExportHeader$w_maintain_manual_financial_trns.srw
$PBExportComments$Window to handle maintaining manual transactions before they are posted.
forward
global type w_maintain_manual_financial_trns from w_ancestor
end type
type cb_extract from commandbutton within w_maintain_manual_financial_trns
end type
type rb_auto_writeoff from radiobutton within w_maintain_manual_financial_trns
end type
type rb_auto_payment from radiobutton within w_maintain_manual_financial_trns
end type
type rb_all from radiobutton within w_maintain_manual_financial_trns
end type
type rb_auto from radiobutton within w_maintain_manual_financial_trns
end type
type rb_manual from radiobutton within w_maintain_manual_financial_trns
end type
type dw_manual_cost_of_claims_allocated from u_dw_online within w_maintain_manual_financial_trns
end type
type cb_delete from commandbutton within w_maintain_manual_financial_trns
end type
type cb_cancel from commandbutton within w_maintain_manual_financial_trns
end type
type cb_close from commandbutton within w_maintain_manual_financial_trns
end type
type cb_save from commandbutton within w_maintain_manual_financial_trns
end type
type dw_manual_payment_details from u_dw_online within w_maintain_manual_financial_trns
end type
type gb_2 from groupbox within w_maintain_manual_financial_trns
end type
type dw_manual_transaction_details from u_dw_online within w_maintain_manual_financial_trns
end type
type dw_cheque_details from u_dw_online within w_maintain_manual_financial_trns
end type
type gb_1 from groupbox within w_maintain_manual_financial_trns
end type
type dw_ready_to_process from datawindow within w_maintain_manual_financial_trns
end type
type dw_unapplied_man_txns_list from u_dw_online within w_maintain_manual_financial_trns
end type
end forward

global type w_maintain_manual_financial_trns from w_ancestor
integer width = 3269
integer height = 2792
string title = "Maintain Manual Transactions"
string menuname = "m_cmwb_notools"
windowtype windowtype = main!
long backcolor = 67108864
event ue_print ( )
cb_extract cb_extract
rb_auto_writeoff rb_auto_writeoff
rb_auto_payment rb_auto_payment
rb_all rb_all
rb_auto rb_auto
rb_manual rb_manual
dw_manual_cost_of_claims_allocated dw_manual_cost_of_claims_allocated
cb_delete cb_delete
cb_cancel cb_cancel
cb_close cb_close
cb_save cb_save
dw_manual_payment_details dw_manual_payment_details
gb_2 gb_2
dw_manual_transaction_details dw_manual_transaction_details
dw_cheque_details dw_cheque_details
gb_1 gb_1
dw_ready_to_process dw_ready_to_process
dw_unapplied_man_txns_list dw_unapplied_man_txns_list
end type
global w_maintain_manual_financial_trns w_maintain_manual_financial_trns

type variables
STRING	is_claim_role_code , is_radio_button
Boolean ib_post92
n_manual_financial inv_manual_financial

Long il_recipient_no, il_txn_unit_of_work, il_claim_no

Boolean  ib_auto_payment , ib_auto_writeoff
end variables

forward prototypes
public subroutine wf_retrieve_details (long al_current_row)
public function integer wf_validation ()
public function string wf_validate_recipient (long al_recipient_no, long al_claim_no)
public function integer wf_validate_role_code (long al_recipient_no, long al_claim_no)
public subroutine wf_setup_datawindows ()
public function integer wf_setup_post92_txns ()
public function integer wf_setup_pre93_txns ()
public function integer wf_validate_dm (decimal adc_amount, long al_cheque_no, date adt_paid_from, date adt_paid_to, long al_claim_no)
public subroutine wf_enable_controls (boolean ab_enable)
public function long wf_retrieve_list_datawindow ()
public function integer wf_validate_cost_allocation ()
public function integer wf_filter_payment_sub_type_code (date adt_paid_from_date, date adt_paid_to_date)
public function integer wf_validate_paid_date_range_pymt_type (string as_payment_sub_type_code, date adt_paid_from_date, date adt_paid_to_date)
public subroutine wf_protect_cheque_no (boolean ab_protect)
public function integer wf_check_subledger_ss ()
public function integer wf_find_post92_cm (long al_claim_no, long al_recipient_no, string as_message)
public function integer wf_find_pre93_cm (long al_claim_no, long al_recipient_no, string as_message)
public function integer wf_find_ss_credit_memo (long al_claim_no, long al_recipient_no, string as_message)
public function integer wf_process_annuity_payout_cheque ()
end prototypes

event ue_print();
unsignedlong ll_print_Job

ll_print_Job = PrintOpen()

This.Print(ll_print_Job,0,0)

PrintClose(ll_print_Job)
end event

public subroutine wf_retrieve_details (long al_current_row);// wf_retrieve_details - loads the detail dw's.
//

DATE     ldt_paid_from_date, ldt_paid_to_date
String  ls_recipient_name, ls_employer_name, ls_operation_name
Long    ll_recipient_no, ll_payment_no, ll_claim_no, ll_txn_no, ll_num_rows
LONG    ll_cost_alloc_no, ll_cost_alloc_operation_no, ll_cheque_no
Integer li_rtn

DataWindowChild ldwc_claim_participants, ldwc_child

ll_payment_no = dw_unapplied_man_txns_list.GetItemNumber(al_current_row, "payment_payment_no")
ll_claim_no = dw_unapplied_man_txns_list.GetItemNumber(al_current_row, "payment_claim_no")
ll_txn_no = dw_unapplied_man_txns_list.GetItemNumber(al_current_row, "txn_no")

//	Clear out the claim participants dddw so that you wont see the wrong claim participants for the claim # just retreived.
li_rtn = dw_manual_transaction_details.GetChild("recipient_no", ldwc_claim_participants)
ldwc_claim_participants.SetTransObject(SQLCA)
ldwc_claim_participants.Reset()	

// Retrieve the drop down data window of claim participants, based on the claim number.
// Note: ALL claimants are listed, not just the active ones, as you may need to transfer costs 
//       from a participant that was set up incorrectly.
ll_num_rows = ldwc_claim_participants.Retrieve(ll_claim_no)
SQLCA.nf_handle_error("w_maintain_manual_financial_trns", "wf_retrieve_details", "ldwc_claim_participants.Retrieve(ll_claim_no)")

//	Now retrieve the detail dw's.

// PAYMENT
ll_num_rows = dw_manual_payment_details.Retrieve(ll_payment_no)
SQLCA.nf_handle_error("w_maintain_manual_financial_trns", "wf_retrieve_details", "dw_manual_payment_details.Retrieve(ll_payment_no)")

dw_manual_payment_details.SetItem(1, "payment_type_description", dw_manual_payment_details.GetItemString(1, "payment_type_code"))
dw_manual_payment_details.SetItem(1, "payment_sub_type_description", dw_manual_payment_details.GetItemString(1, "payment_sub_type_code"))
dw_manual_payment_details.SetItemStatus(1, 0, Primary!, NotModified!)

IF dw_manual_payment_details.GetChild('payment_sub_type_code', ldwc_child) < 0 THEN
	MessageBox('Error', 'Not a DW child')
END IF

// filter, sort payment sub type
ldt_paid_from_date = Date(dw_manual_payment_details.GetItemDateTime(dw_manual_payment_details.GetRow(),'paid_from_date'))
ldt_paid_to_date = Date(dw_manual_payment_details.GetItemDateTime(dw_manual_payment_details.GetRow(),'paid_to_date'))

wf_filter_payment_sub_type_code(ldt_paid_from_date,ldt_paid_to_date)

ldwc_child.SetFilter('payment_type_code = "97"')   // can only be 97 - set-aside
ldwc_child.Filter()
ldwc_child.SetSort('payment_sub_type_code A')
ldwc_child.Sort()


// TXN
ll_num_rows = dw_manual_transaction_details.Retrieve(ll_txn_no)
SQLCA.nf_handle_error("w_maintain_manual_financial_trns", "wf_retrieve_details", "dw_manual_transaction_details.Retrieve(ll_txn_no)")

ll_recipient_no = dw_manual_transaction_details.GetItemNumber(1, "recipient_no")
ls_recipient_name = wf_validate_recipient(ll_recipient_no, ll_claim_no)

//CHEQUE_DETAILS
IF rb_manual.checked or rb_auto_payment.checked or rb_all.checked THEN
	ll_num_rows = dw_cheque_details.retrieve(ll_txn_no)
	SQLCA.nf_handle_error("w_maintain_manual_financial_trns", "wf_retrieve_details", "dw_cheque_details.Retrieve(ll_txn_no)")
ELSE
	dw_cheque_details.Reset()
END IF

//READY TO PROCESS
IF rb_auto_writeoff.checked or rb_auto_payment.checked or rb_all.checked THEN
	dw_ready_to_process.retrieve(ll_txn_no)
	SQLCA.nf_handle_error("w_maintain_manual_financial_trns", "wf_retrieve_details", "dw_ready_to_process.Retrieve(ll_txn_no)")
END IF

// COST ALLOC
ll_num_rows = dw_manual_cost_of_claims_allocated.Retrieve(ll_txn_no)
SQLCA.nf_handle_error("w_maintain_manual_financial_trns", "wf_retrieve_details", "dw_manual_cost_of_claims_allocated.Retrieve(ll_txn_no)")

// set cost alloc names
IF dw_manual_cost_of_claims_allocated.GetRow() = 1 THEN
	ll_cost_alloc_no = dw_manual_cost_of_claims_allocated.GetItemNumber(1,'cost_alloc_no')
	ll_cost_alloc_operation_no = dw_manual_cost_of_claims_allocated.GetItemNumber(1,'cost_alloc_operation_no')
	
	inv_manual_financial.nf_display_cost_allocation_data(ll_cost_alloc_no,ll_cost_alloc_operation_no,ls_employer_name,ls_operation_name)
	
	dw_manual_cost_of_claims_allocated.SetItem(1,'cost_alloc_name',ls_employer_name)
	dw_manual_cost_of_claims_allocated.SetItem(1,'cost_alloc_group_name',ls_operation_name)
	
	dw_manual_cost_of_claims_allocated.SetItemStatus(1, 0, Primary!, NotModified!)
END IF
end subroutine

public function integer wf_validation ();// wf_validation - validates the changes made to the manual txn.
//
//	The validations are:
//		1. Cost of Claims period must be valid (current period)
//		5. Individual type recipients must be related to the claim
//		6.	Payees must have a valid payee # 
//		7.	Payees are only valid when payment_type_code is a MA benefit category
//		6.	Employer # (cost allocation) must be valid
//		8. If it is a set aside, the individual (recipient) must be eligible
//
//			
//	This function returns: -1 if any errors were found
//									0 if no errors were found.
//
LONG        ll_claim_no, ll_individual_no, ll_cheque_no, ll_recipient_no, ll_original_cheque_no, ll_row
LONG        ll_min_cheque_no, ll_max_cheque_no, ll_old_cheque_no, ll_count, ll_tuow, ll_count2
INTEGER		li_reply, li_rtn, li_count, li_unprocessed_CMs
STRING      ls_recipient_name, ls_payment_sub_type_code, ls_explanation, ls_recipient_type_code 
DATETIME    ldtm_accident_date, ldt_cheque_deposit_date, ldt_transmit_date, ldt_reconciled_date
DECIMAL{4}  ldec_amount
DATE			ldt_paid_from, ldt_paid_to


// Make sure you have last data entered to validate
IF dw_manual_transaction_details.AcceptText() = -1 THEN RETURN -1
IF dw_manual_payment_details.AcceptText() = -1 THEN RETURN -1
IF dw_manual_cost_of_claims_allocated.AcceptText() = -1 THEN RETURN -1

ldec_amount = dw_manual_transaction_details.GetItemDecimal(dw_manual_transaction_details.GetRow(),'txn_amount')
IF ldec_amount = 0 THEN
	MessageBox('Cheque Validation','Please enter a non-zero amount.')
	RETURN -1
END IF

ll_claim_no = dw_manual_transaction_details.GetItemNumber(1,"claim_no")
ll_individual_no = dw_manual_transaction_details.GetItemNumber(1,"recipient_no")
ls_payment_sub_type_code = dw_manual_payment_details.GetItemString(1,'payment_sub_type_code')
ll_cheque_no = dw_manual_transaction_details.GetItemNumber(1,'cheque_no')
ls_explanation = dw_manual_transaction_details.GetItemString(1,"explanation")
ll_original_cheque_no = dw_manual_transaction_details.GetItemNumber(1,'cheque_no',PRIMARY!,TRUE)
ll_recipient_no = dw_manual_transaction_details.GetItemNumber(1,'recipient_no')

// validate transaction data
li_rtn = inv_manual_financial.nf_validate_transaction(ls_payment_sub_type_code,ls_explanation,ll_cheque_no, ll_original_cheque_no,ll_recipient_no) 
IF li_rtn < 0 THEN RETURN li_rtn


ldt_paid_from = Date(dw_manual_payment_details.GetItemDateTime(1,'paid_from_date'))
ldt_paid_to   = Date(dw_manual_payment_details.GetItemDateTime(1,'paid_to_date'))

// Validate that if the paid from and to dates are required that they are there. Also make sure that the
// paid from date is not greater than the paid to date and that the paid to date is not in the future.

// If either the "Pd From" or "Paid To" date is entered or present (i.e. previously entered), the other date 	
//	must also be entered or present.
IF IsNull(ldt_paid_from) OR IsNull(ldt_paid_to) THEN
	MessageBox("No Dates Entered", "You have not entered both the from and to dates. Annuity set-aside payments require both from and to dates.", Exclamation!)
	RETURN -1
END IF

SELECT	accident_date
INTO		:ldtm_accident_date
FROM		CLAIM
WHERE	claim_no = :ll_claim_no
USING SQLCA ;
SQLCA.nf_handle_error("w_maintain_manual_financial_trns", "","wf_validation - Embedded SQL:SELECT accident_date FROM CLAIM") 

IF NOT ISNULL(ldtm_accident_date) THEN
	IF ldt_paid_from < date(ldtm_accident_date) THEN
		MessageBox("Compensation Validation Error","The paid from date must be on or after the accident date of the claim!",Exclamation!)
		RETURN -1
	END IF
END IF 


// when the "Pd From" date is entered/changed and is before January 1, 1993, a warning message is displayed    
//	for the user to confirm the date.
IF ldt_paid_from < date("1993-01-01") THEN
	li_reply = MessageBox("Compensation Validation Warning","You have entered a paid from date that is " + & 
								  "before'1993-01-01'. Do you want to continue with this date?",Question!,YesNo!,2)
	IF li_reply = 2 THEN RETURN -1			
END IF

// When the "Pd To" date is entered/changed and is before January 1, 1993, a warning message is displayed for the
// user to confirm the date.
IF ldt_paid_from < date("1993-01-01") AND ldt_paid_to <= date("1993-01-01") THEN
	li_reply = MessageBox("Compensation Validation Warning","You have entered a paid to date that is " +&
		"on or before '1993-01-01'. Do you want to continue with this date?",Question!,YesNo!,2)
	IF li_reply = 2 THEN RETURN -1		
END IF

li_rtn = wf_validate_role_code(ll_individual_no, ll_claim_no)
IF li_rtn < 0 THEN
	RETURN li_rtn
END IF




//****************************

//Validate for a Debit Memo or Credit Memo - C or SS
ll_claim_no = dw_manual_transaction_details.getitemnumber(1,'claim_no')
ldec_amount = dw_manual_transaction_details.getitemdecimal(1,'txn_amount')
ldt_paid_from = Date(dw_manual_payment_details.getitemdatetime(1,'paid_from_date'))
ldt_paid_to = Date(dw_manual_payment_details.getitemdatetime(1,'paid_to_date'))
ll_recipient_no = dw_manual_transaction_details.GetItemNumber(1,'recipient_no')

IF ls_payment_sub_type_code = 'DM' OR ls_payment_sub_type_code = 'DO' OR ls_payment_sub_type_code = 'DW' THEN
	// look for unapplied CMs
	li_count = inv_manual_financial.nf_find_unapplied_annuity_payments(ls_payment_sub_type_code, ldt_paid_from, ldt_paid_to, ll_cheque_no,ldec_amount, ll_recipient_no)
	IF li_count > 0 THEN
		RETURN -1
	END IF
	
	// look for applied CMs
	li_count = inv_manual_financial.nf_find_applied_annuity_payments(ls_payment_sub_type_code,ldt_paid_from,ldt_paid_to,ll_cheque_no,ldec_amount,ll_recipient_no)
	IF li_count = 0 THEN
		RETURN -1
	END IF
	
ELSEIF ls_payment_sub_type_code = 'CM' OR ls_payment_sub_type_code = 'CO' OR ls_payment_sub_type_code = 'CW' THEN
	IF is_claim_role_code = 'C' THEN
		IF ib_post92 = TRUE THEN
			IF wf_setup_post92_txns() < 0 THEN RETURN -1
		ELSE
			IF wf_setup_pre93_txns() < 0 THEN RETURN -1
		END IF
	ELSEIF is_claim_role_code = 'SS' THEN
		IF wf_check_subledger_ss() < 0 THEN RETURN -1
	END IF
ELSE
	// not CM or DM
	// 2.105 The annuity sub-ledger balance for the surviving spouse must equal the amount of the scheduled Annuity Payment(s)
	//       (i.e. 97/CM), if the individual selected is the surviving spouse.
	// 2.120	The pre-1993 annuity sub-ledger balance for the injured worker must equal the amount of the scheduled 
	//       Annuity Payment(s) (i.e. 97/CM), if the individual selected is an injured worker, and the payment period 
	//       is prior to 1993.
	// 2.140	The post-1992 annuity sub-ledger balance for the injured worker must equal the amount of the scheduled 
	//       Annuity Payment(s) (i.e. 97/CM), if the individual selected is an injured worker, and the payment period 
	//       is on or after January 1, 1993.
	
	// So, if there are any applicable unprocessed credit memos, then obviously the subledger balance will no longer be zero
	// so, prevent the deletion/modification of subledger payments	
	IF dw_manual_transaction_details.GetItemStatus(1,'txn_amount',Primary!) = DataModified! THEN
		// the txn amt was modifed on non-97/CM
		IF is_claim_role_code = 'C' THEN
			IF ib_post92 = TRUE THEN
				li_rtn = wf_find_post92_CM(ll_claim_no,ll_individual_no,'You cannot alter the transaction amount for a post-1992 manual set-aside payment.'&
																				+'~r~nThere is an existing unprocessed post-1992 manual Annuity Payment for this claim.')
				IF li_rtn < 0 THEN RETURN -1
			ELSE
				li_rtn = wf_find_pre93_CM(ll_claim_no,ll_individual_no,'You cannot alter the transaction amount for a pre-1993 manual set-aside payment.'&
																				+'~r~nThere is an existing unprocessed pre-1993 manual Annuity Payment for this claim.')
				IF li_rtn < 0 THEN RETURN -1
			END IF
		ELSEIF is_claim_role_code = 'SS' THEN
			li_rtn = wf_find_ss_credit_memo(ll_claim_no,ll_individual_no,'You cannot alter the transaction amount for a surviving spouse manual set-aside payment.'&
																					+'~r~nThere is an existing unprocessed manual Annuity Payment for this claim and individual.')
			IF li_rtn < 0 THEN RETURN -1
		END IF		
	END IF
END IF


// validate payment data
li_rtn = inv_manual_financial.nf_validate_payment(ll_individual_no,ll_claim_no,Datetime(ldt_paid_from),Datetime(ldt_paid_to),ls_payment_sub_type_code,is_claim_role_code)
IF li_rtn < 0 THEN
	RETURN li_rtn
END IF

//****************************

		
// Use the function wf_validate_recipient to validate the recipient (obviously). It returns the recipient name
// if every thing went ok, otherwise it returns null.

ls_recipient_name = wf_validate_recipient(ll_individual_no, ll_claim_no)
IF IsNull(ls_recipient_name) THEN
	RETURN -1
ELSE
	dw_manual_transaction_details.SetItem(1,"recipient_name",ls_recipient_name)
END IF


// Well, time to validate the cost allocation and get the cost allocation type code. We always need to
// get the type code and update it in the dw, incase the cost allocation (employer) type has changed.
IF wf_validate_cost_allocation() < 0 THEN
	RETURN -1 
END IF



ls_payment_sub_type_code = dw_manual_payment_details.GetItemString(1,"payment_sub_type_code")
IF (ls_payment_sub_type_code = 'CM' OR ls_payment_sub_type_code = 'CO' OR ls_payment_sub_type_code = 'CW') and ldec_amount >= 0 then
	MessageBox("Annuity Validation Warning","An Annuity Payment must be a negative amount " + string(ldec_amount,'[currency]') )
	RETURN -1
END IF

IF (ls_payment_sub_type_code = 'DM' OR ls_payment_sub_type_code = 'DO' OR ls_payment_sub_type_code = 'DW') and ldec_amount <= 0 then
	MessageBox("Annuity Validation Warning","A Void Annuity Payment must be a positive amount " + string(ldec_amount,'[currency]') )
	RETURN -1
END IF

IF is_claim_role_code = 'C' THEN
	ll_claim_no = 0
END IF

SELECT	count(*)
INTO		:li_count
FROM		ANNUITY_ELIGIBILITY a
JOIN		ANNUITY_ACCOUNT b ON a.annuity_account_no = b.annuity_account_no
WHERE	   a.annuity_eligibility_status_code = 'A'
AND      b.claim_no = :ll_claim_no
AND		b.individual_no = :ll_individual_no
USING SQLCA ;
SQLCA.nf_handle_error('w_maintain_manual_financial_trns', '', 'wf_validation - Embedded SQL: select from ANNUITY_ELIGIBILITY')

IF li_count < 1 THEN
	MessageBox("Validation Error","Recipient is not eligible to receive annuity set aside!",Exclamation!)
	RETURN -1
END IF

// If there's a cheque number make sure there is a cheque deposit date
ll_cheque_no = dw_manual_transaction_details.GetItemNumber(1,"cheque_no")
ldt_cheque_deposit_date = dw_manual_transaction_details.GetItemDatetime(1,"cheque_deposit_date")
IF ll_cheque_no > 0 AND IsNull(ldt_cheque_deposit_date) THEN
	ldt_cheque_deposit_date = Datetime(Date(f_server_datetime()))
	dw_manual_transaction_details.SetItem(1,"cheque_deposit_date", ldt_cheque_deposit_date)
	IF dw_manual_transaction_details.AcceptText() = -1 THEN RETURN -1
END IF

//The cheque no must fall between the range of cheque no's in the Handwritten_Cheque_No_Range table

IF ls_payment_sub_type_code = 'CM' OR ls_payment_sub_type_code = 'CO' THEN
	IF  dw_manual_transaction_details.GetItemstatus(1,"cheque_no",PRIMARY!) = DataModified! THEN
		
		IF dw_manual_transaction_details.GetItemNumber(1,"cheque_no") <> 0 THEN
		
			Select min_handwritten_cheque_no,max_handwritten_cheque_no
			Into :ll_min_cheque_no, :ll_max_cheque_no
			From Handwritten_Cheque_No_Range
			Using SQLCA;
			
			SQLCA.nf_handle_error('w_maintain_manual_financial_trns','wf_validation','Select from Handwritten_Cheque_No_Range')
				
			IF (ll_cheque_no < ll_min_cheque_no) OR (ll_cheque_no > ll_max_cheque_no) THEN
					MessageBox('Error','The Cheque No must be within the allowable range of handwritten cheque numbers between '+String(ll_min_cheque_no)+' and '+String(ll_max_cheque_no)+' .',exclamation!)
					Return -1
			END IF
		END IF
		
		ll_old_cheque_no = dw_manual_transaction_details.GetItemNumber(1,"cheque_no",Primary!,true)
		
		//The cheque no must not be removed or changed  if the CHEQUE_HEADER.transmit_date is not null or the reconciled_date is not null.
		Select transmit_date, reconciled_date
		Into   :ldt_transmit_date, :ldt_reconciled_date
		From  CHEQUE_HEADER
		Where cheque_no = :ll_old_cheque_no
		Using SQLCA;

		SQLCA.nf_handle_error('w_maintain_manual_financial_trns','wf_validation','Select from CHEQUE_HEADER')
		
		IF ll_old_cheque_no <> 0 THEN
			IF NOT IsNull(ldt_transmit_date) OR NOT IsNull(ldt_reconciled_date) THEN
				MessageBox('Error','The Cheque No must not be changed or removed as the cheque has been reconciled or transmitted to the bank.',exclamation!)
				Return -1
			END IF
		END IF
		
		//automatic annuity payment
		
		IF rb_auto_payment.checked = True THEN

			//The cheque no must not already exist in the CHEQUE_HEADER table
			Select count(*)
			Into   :ll_count
			From  CHEQUE_HEADER
			Where cheque_no = :ll_cheque_no
			Using SQLCA;
	
			SQLCA.nf_handle_error('w_maintain_manual_financial_trns','wf_validation','Select from CHEQUE_HEADER 2')
			
			IF ll_count > 0 THEN
				MessageBox('Error','The Cheque No already exists and can not be re-used.',exclamation!)
				Return -1
			END IF
			
			/*The new or modified cheque number for an automatic annuity sub-ledger payment must not exist on other scheduled payments/transactions that are for either of the following:
				•	other annuity payout recipients involved in the same annuity payout
				•	other payment/transactions that are not associated with the annuity payout.
			*/

			ll_row =  dw_manual_transaction_details.GetRow()
           ll_tuow = dw_manual_transaction_details.GetItemNumber(ll_row,"txn_unit_of_work_no")
           ll_cheque_no  = dw_manual_transaction_details.GetItemNumber(ll_row,"cheque_no")
			
			IF ll_cheque_no > 0 THEN
				// checks for other annuity payout recipients involved in the same annuity payout
				
				Select count(*)
				Into   :ll_count
				From   UNAPPLIED_CLAIM_TXN 
				WHERE txn_unit_of_work_no = :ll_tuow
				AND cheque_no = :ll_cheque_no
				Using SQLCA;
				
				SQLCA.nf_handle_error('w_maintain_manual_financial_trns','wf_validation','Select from UNAPPLIED_CLAIM_TXN1')
				
				IF ll_count > 1  THEN 
					MessageBox('Error','The cheque no already exists on other scheduled payments/transactions for other annuity payout recipients.',Exclamation!)
					 Return -1
				END IF
				
	
				// checks for other payment/transactions that are not associated with the annuity payout.
				
				Select count(*)
				Into   :ll_count2
				From   UNAPPLIED_CLAIM_TXN 
				WHERE cheque_no = :ll_cheque_no
				Using SQLCA;
				
				SQLCA.nf_handle_error('w_maintain_manual_financial_trns','wf_validation','Select from UNAPPLIED_CLAIM_TXN2')
	
				IF ll_count2 > 1 THEN 
					MessageBox('Error','The cheque no already exists on other scheduled payments/transactions that are not associated with the annuity payout.',Exclamation!)
					 Return -1
				END IF			
			END IF
			
		END IF
			
		IF rb_manual.checked = True THEN
			
	       	ll_cheque_no  = dw_manual_transaction_details.GetItemNumber(1,"cheque_no")
			ll_count = 0 
				  
			// The new or modified cheque number for a manual annuity sub-ledger payment must exist (i.e. CHEQUE_HEADER).	  
				  
			Select count(*)
			Into    :ll_count
			From  CHEQUE_HEADER
			Where cheque_no = :ll_cheque_no
			Using SQLCA;

			SQLCA.nf_handle_error('w_maintain_manual_financial_trns','wf_validation','Select from CHEQUE_HEADER1')

			IF ll_count > 1 THEN 
				MessageBox('Error','The cheque no must exist for a manual annuity sub-ledger payment.',Exclamation!)
			    Return -1
			END IF			

			// The new or modified cheque number for a manual annuity sub-ledger payment must not exist on other scheduled payment/transactions for other recipients. 

			ll_recipient_no = dw_manual_transaction_details.GetItemNumber(1,"recipient_no")
			ls_recipient_type_code = dw_manual_transaction_details.GetItemString(1,"recipient_type_code")
			ll_count = 0 
			
			Select count(*)
			Into    :ll_count
			From  UNAPPLIED_CLAIM_TXN
			Where cheque_no = :ll_cheque_no
			and   recipient_no = :ll_recipient_no  
			and   recipient_type_code = :ls_recipient_type_code
			Using SQLCA;

			SQLCA.nf_handle_error('w_maintain_manual_financial_trns','wf_validation','Select from CHEQUE_HEADER1')

			IF ll_count > 1 THEN 
				MessageBox('Error','The cheque no must exist for a manual annuity sub-ledger payment.',Exclamation!)
			    Return -1
			END IF

		END IF			
			
	 END IF	 
END IF

RETURN 0
end function

public function string wf_validate_recipient (long al_recipient_no, long al_claim_no);// wf_validate_recipient - validates the recipient and returns the recipient name. If the recipient number and type are
//                         not valid, then it returns a null string.
//
String ls_null_string, ls_last_name, ls_given_names, ls_claim_role_code, ls_provider_name

SetNull(ls_null_string)

IF IsNull(al_recipient_no) OR &
	al_recipient_no < 1 THEN
	MessageBox("Invalid Recipient Number","You must select a recipient for the claim!",Exclamation!)
	RETURN ls_null_string
END IF


// Retrieve the individual's name
SELECT	last_name, given_names
INTO		:ls_last_name, :ls_given_names
FROM		INDIVIDUAL
WHERE	individual_no = :al_recipient_no
USING SQLCA;
SQLCA.nf_handle_error('w_maintain_manual_financial_trns', '', 'wf_validate_recipient - select from INDIVIDUAL')

IF Len(ls_last_name) > 0 OR Len(ls_given_names) > 0 THEN
	RETURN Trim(ls_given_names) + " " + Trim(ls_last_name)
ELSE
	MessageBox("Error","Could not locate a name for individual!",Exclamation!)
	RETURN ls_null_string
END IF
end function

public function integer wf_validate_role_code (long al_recipient_no, long al_claim_no);// wf_validate_role_code - validates the recipient role code, sets is_claim_role_code variable
//

// Validate that the individual is valid for the claim and get the claim role code.
SELECT	claim_role_code
INTO		:is_claim_role_code
FROM		CLAIM_PARTICIPANT
WHERE	claim_no = :al_claim_no
AND		individual_no = :al_recipient_no
USING SQLCA;
SQLCA.nf_handle_error('w_maintain_manual_financial_trns', '', 'wf_validate_recipient - Embedded SQL: select from CLAIM PARTICIPANT')

IF IsNull(is_claim_role_code) OR is_claim_role_code <= ' ' THEN
	MessageBox("Invalid Individual","The individual is not valid for the claim!",Exclamation!)
	RETURN -1
END IF

RETURN 0
end function

public subroutine wf_setup_datawindows ();Long    ll_row_count, ll_row


IF IsValid(dw_unapplied_man_txns_list) and dw_unapplied_man_txns_list.RowCount() > 0 THEN
	ll_row = dw_unapplied_man_txns_list.getrow()
	il_claim_no = dw_unapplied_man_txns_list.getitemnumber(ll_row,"payment_claim_no")
	il_recipient_no = dw_unapplied_man_txns_list.getitemnumber(ll_row,"unapplied_claim_txn_recipient_no")
	il_txn_unit_of_work = dw_unapplied_man_txns_list.getitemnumber(ll_row,"txn_unit_of_work_no")
END IF

ll_row_count = wf_retrieve_list_datawindow()

ll_row = dw_unapplied_man_txns_list.getrow()
IF ll_row = 0 THEN ll_row = 1

cb_extract.enabled = FALSE

IF rb_manual.checked = TRUE THEN
	// manually created through add manual financial txn module
	wf_enable_controls(TRUE)
	is_radio_button = "rb_manual"
ELSEIF rb_auto.checked = TRUE THEN
	// automatically created through annuity calculation module
	wf_enable_controls(FALSE)
	is_radio_button = "rb_auto"
	cb_extract.enabled =FALSE
ELSEIF rb_all.checked = TRUE THEN
	// all txn list	
	IF ll_row_count > 0 THEN
		// IF first row has txn_unit_of_work_no, it was created automatically, so disable
		IF dw_unapplied_man_txns_list.GetItemNumber(1, 'txn_unit_of_work_no') > 0 THEN
			wf_enable_controls(FALSE)
		ELSE
			// otherwise...
			wf_enable_controls(TRUE)
		END IF
		is_radio_button = "rb_all"
	END IF
ELSEIF rb_auto_payment.checked = TRUE THEN
	//automatically created through annuity payout module	
	IF ll_row_count > 0 THEN
		cb_extract.enabled = TRUE
	ELSE
		wf_enable_controls(FALSE)
	END IF
	is_radio_button = "rb_auto_payment"
ELSEIF rb_auto_writeoff.checked = TRUE THEN
	//automatically created through annuity payout module
	//	wf_enable_controls(FALSE)
	is_radio_button = "rb_auto_writeoff"
END IF

IF ll_row_count > 0 THEN
	IF Date(dw_unapplied_man_txns_list.GetItemDateTime(1,'payment_paid_from_date')) < 1993-01-01 THEN
		ib_post92 = FALSE
	ELSE
		ib_post92 = TRUE
	END IF
	wf_retrieve_details(ll_row)
ELSE
	//MessageBox("No Txn's Found", "There are no unposted manual transactions!", Information!)
	cb_extract.enabled = FALSE
END IF

// Disable the save button until some data has been changed
cb_save.Enabled = False

end subroutine

public function integer wf_setup_post92_txns ();// This function checks all the post 92 txns against the sub-ledger
int li_row, li_rtn
long ll_cheque_no, ll_claim_no, ll_payment_no
decimal{4} ldec_net_payment, ldec_amount, ldec_sum, ldec_sum_CM, ldec_net_payment_2


li_row = dw_manual_transaction_details.getrow()

ll_claim_no = dw_manual_transaction_details.getitemdecimal(li_row,'claim_no')
ldec_amount = dw_manual_transaction_details.getitemdecimal(li_row,'txn_amount')
ll_payment_no =  dw_manual_transaction_details.getitemdecimal(li_row,'payment_no')


//Get the subledger balance

Select sum(a.net_payment_amount)
Into :ldec_net_payment
From PAYMENT a
Where a.payment_type_code = '97' 
AND NOT EXISTS (Select b.*
                From ANNUITY_PAYOUT_PRE_1993 b
                Where b.claim_no = a.claim_no
                And  b.payout_payment_no = a.payment_no)
AND NOT EXISTS (Select c.*
                From ANNUITY_SETASIDE_PRE_1993 c
                Where c.claim_no = a.claim_no
                And  c.setaside_payment_no = a.payment_no)
AND a.claim_no = :ll_claim_no
Using SQLCA;

SQLCA.nf_handle_error('w_add_manual_financial_trns','wf_setup_post92_txns','Select sum(a.total_payment_amount) From PAYMENT a')

// Get the net payment for the current txn and subtract it from the total and then check if the txn amounts equal
Select net_payment_amount
Into :ldec_net_payment_2
From PAYMENT 
Where payment_no = :ll_payment_no
Using SQLCA;

SQLCA.nf_handle_error('w_add_manual_financial_trns','wf_setup_post92_txns','Select net_payment_amount) From PAYMENT')

IF (ldec_amount * -1)  <> (ldec_net_payment - ldec_net_payment_2) THEN
	MessageBox('Error',' The Annuity Payment amount must equal the post-1992 annuity sub-ledger balance for the injured worker.', Exclamation!)
	Return -1
END IF

Return 1
end function

public function integer wf_setup_pre93_txns ();// This function ensures that all the txns that were adjusted are pre 93 txns and checks them against the Pre93 XFER tables and sub-ledger

int li_row, li_rtn
long ll_claim_no, ll_payment_no
decimal{4} ldec_net_payment_1, ldec_net_payment_2, ldec_amount, ldec_net_payment_3, ldec_scheduled_payments

li_row = dw_manual_transaction_details.getrow()
ll_claim_no = dw_manual_transaction_details.getitemdecimal(li_row,'claim_no')
ldec_amount = dw_manual_transaction_details.getitemdecimal(li_row,'txn_amount')
ll_payment_no =  dw_manual_transaction_details.getitemdecimal(li_row,'payment_no')

// Get the net payment for the current txn and subtract it from the total and then check if the txn amounts equal
Select net_payment_amount
Into :ldec_net_payment_3
From PAYMENT 
Where payment_no = :ll_payment_no
Using SQLCA;

SQLCA.nf_handle_error('w_add_manual_financial_trns','wf_setup_post92_txns','Select net_payment_amount) From PAYMENT')
//
//// Get the other scheduled payments
//Select sum(net_payment_amount)
//Into :ldec_scheduled_payments
//From PAYMENT 
//Where claim_no = :ll_claim_no
//and processed_date IS NULL
//Using SQLCA;
//
//SQLCA.nf_handle_error('w_add_manual_financial_trns','wf_setup_post92_txns','Select net_payment_amount) From PAYMENT')
//

//Get the subledger balance

Select sum(a.net_payment_amount)
Into :ldec_net_payment_1
From PAYMENT a, ANNUITY_PAYOUT_PRE_1993 b
Where a.payment_no = b.payout_payment_no
And a.claim_no = b.claim_no
And a.claim_no = :ll_claim_no
Using SQLCA;

SQLCA.nf_handle_error('w_add_manual_financial_trns','wf_setup_post92_txns','Select  sum(a.net_payment_amount)')

IF IsNull(ldec_net_payment_1) then ldec_net_payment_1 = 0


Select sum(a.net_payment_amount)
Into :ldec_net_payment_2
From PAYMENT a, ANNUITY_SETASIDE_PRE_1993 b
Where a.payment_no = b.setaside_payment_no
And a.claim_no = b.claim_no
And a.claim_no = :ll_claim_no
Using SQLCA;

SQLCA.nf_handle_error('w_add_manual_financial_trns','wf_setup_post92_txns','Select From PAYMENT a, ANNUITY_SETASIDE_PRE_1993 b')

IF IsNull(ldec_net_payment_2) then ldec_net_payment_1 = 0

IF (ldec_amount + ldec_scheduled_payments - ldec_net_payment_3) - (ldec_net_payment_1 + ldec_net_payment_2) <> 0 THEN
// IF ldec_amount  <> (ldec_net_payment_1 + ldec_net_payment_2 - ldec_net_payment_3) THEN
	MessageBox('Error',' The Annuity Payment amount must equal the pre-1993 annuity sub-ledger balance for the injured worker.', Exclamation!)
	Return -1
END IF

Return 1
end function

public function integer wf_validate_dm (decimal adc_amount, long al_cheque_no, date adt_paid_from, date adt_paid_to, long al_claim_no);// This function validates the DM - debit memo txn.

// The DM must have the same amount as a credit memo but with the opposite sign
// The same cheque number as the credit memo
// The same period as the credit memo

Long ll_count, ll_count2
Decimal ldc_amount

ldc_amount = adc_amount * -1


Select count(*)
Into: ll_count
From PAYMENT a, APPLIED_CLAIM_TXN b
Where a.payment_no = b.payment_no
And a.claim_no = b.claim_no
And a.payment_type_code = '97'
And a.payment_sub_type_code = 'CM'
And a.claim_no = :al_claim_no
And b.cheque_no = :al_cheque_no
And a.total_payment_amount = :ldc_amount
And a.paid_from_date = :adt_paid_from	
And a.paid_to_date = :adt_paid_to
Using SQLCA;

SQLCA.nf_handle_error('w_add_manual_financial_trns','wf_validate_dm','Select count(*) From PAYMENT a, APPLIED_CLAIM_TXN b')


Select count(*)
Into: ll_count2
From PAYMENT a, UNAPPLIED_CLAIM_TXN b
Where a.payment_no = b.payment_no
And a.claim_no = b.claim_no
And a.payment_type_code = '97'
And a.payment_sub_type_code = 'CM'
And a.claim_no = :al_claim_no
And b.cheque_no = :al_cheque_no
And a.total_payment_amount = :ldc_amount
And a.paid_from_date = :adt_paid_from	
And a.paid_to_date = :adt_paid_to
Using SQLCA;

SQLCA.nf_handle_error('w_add_manual_financial_trns','wf_validate_dm','Select count(*) From PAYMENT a, UNAPPLIED_CLAIM_TXN b')

IF ll_count + ll_count2 = 0 THEN
	MessageBox('Error','An Annuity Payment does not exist for this Void Annuity Payment. ~nThe Void Annuity Payment must have the same cheque no, period, and amount as the Annuity Payment.',Exclamation!)
	RETURN -1
END IF

RETURN 1
end function

public subroutine wf_enable_controls (boolean ab_enable);long ll_current_row

dw_manual_payment_details.enabled          = ab_enable
dw_manual_transaction_details.enabled      = ab_enable
dw_manual_cost_of_claims_allocated.enabled = ab_enable
cb_delete.enabled                          = ab_enable
cb_cancel.enabled                          = ab_enable

// need to enable the cheque no entry field (only) under certain conditions
ll_current_row = dw_unapplied_man_txns_list.getRow()
	IF (rb_manual.checked OR rb_auto_payment.checked) AND ll_current_row > 0 THEN  
		IF ab_enable = FALSE THEN
			// for subtypes CM,CO enable the datawindow but protect all displayed colums except cheque_no
      		IF  dw_unapplied_man_txns_list.GetItemString(ll_current_row,'payment_sub_type_code') = 'CM'  &
				OR  dw_unapplied_man_txns_list.GetItemString(ll_current_row,'payment_sub_type_code') = 'CO' THEN
				dw_manual_transaction_details.enabled = TRUE
				dw_manual_transaction_details.Object.coc_period.Protect = 1
				dw_manual_transaction_details.Object.txn_amount.Protect = 1
				dw_manual_transaction_details.Object.explanation.Protect = 1
				dw_manual_transaction_details.Object.recipient_no.Protect = 1	
			END IF
		ELSE
				dw_manual_transaction_details.Object.coc_period.Protect = 0
				dw_manual_transaction_details.Object.txn_amount.Protect = 0
				dw_manual_transaction_details.Object.explanation.Protect = 0
				dw_manual_transaction_details.Object.recipient_no.Protect = 0	
		END IF
	END IF

end subroutine

public function long wf_retrieve_list_datawindow ();    Long    ll_row_count, ll_rcount, ll_row, ll_claim, ll_recipient, ll_txn_unit_of_work_no
Integer li_rtn, li_z, li_x


IF IsValid(w_maintain_manual_financial_trns.dw_unapplied_man_txns_list) and w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.RowCount() > 0 THEN
	ll_row = w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.getrow()
	il_claim_no = w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.getitemnumber(ll_row,"payment_claim_no")
	il_recipient_no = w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.getitemnumber(ll_row,"unapplied_claim_txn_recipient_no")
	il_txn_unit_of_work = w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.getitemnumber(ll_row,"txn_unit_of_work_no")
END IF

dw_unapplied_man_txns_list.Reset()
dw_manual_payment_details.Reset()
dw_manual_transaction_details.Reset()
dw_manual_cost_of_claims_allocated.Reset()
dw_cheque_details.Reset()
dw_ready_to_process.Reset()

dw_ready_to_process.visible = false
dw_ready_to_process.enabled = false

IF rb_manual.checked = TRUE THEN
	//manual txn list
	w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.dataobject = 'd_unapplied_man_txns_list_manual'
	dw_cheque_details.visible = true
ELSEIF rb_auto.checked = TRUE THEN
	//automatic txn list
	w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.dataobject = 'd_unapplied_man_txns_list'
	dw_cheque_details.visible = false
ELSEIF rb_all.checked = TRUE THEN
	// all txn list
	w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.dataobject = 'd_unapplied_man_txns_list_all'
	dw_cheque_details.visible = true
ELSEIF rb_auto_payment.checked = TRUE THEN
	// txn list for annuity payment module, auto payment , payment subtype code 'CM', CO'
	dw_unapplied_man_txns_list.dataobject = 'd_unapplied_man_auto_payout_txns_list'
	dw_cheque_details.visible = true
	dw_ready_to_process.visible = true
	ib_auto_payment  = true
ELSEIF rb_auto_writeoff.checked = TRUE THEN
	// txn list for annuity payment module, auto payment , payment subtype code 'CW'
	dw_unapplied_man_txns_list.dataobject = 'd_unapplied_man_auto_writeoff_txns_list'
	dw_cheque_details.visible = false
	dw_ready_to_process.visible = true
	dw_ready_to_process.enabled = true
    ib_auto_writeoff = true
END IF


// Connect the dw's to SQLCA.
dw_unapplied_man_txns_list.SetTransObject(SQLCA)
dw_manual_payment_details.SetTransObject(SQLCA)
dw_manual_transaction_details.SetTransObject(SQLCA)
dw_manual_cost_of_claims_allocated.SetTransObject(SQLCA)
dw_ready_to_process.SetTransObject(SQLCA)

// Load the list of unposted manual transactions.
ll_row_count = w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.Retrieve()
li_rtn = SQLCA.nf_handle_error("w_maintain_manual_financial_trns","","open - dw_unapplied_man_txns_list.Retrieve()")
ll_rcount = w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.RowCount()


//IF rb_auto_payment.checked = True and ib_auto_writeoff = True THEN
//	//set the row to the same row that was selected in the auto write off datawindow
//	
//	For li_z = 1 to ll_rcount
//		
//		ll_recipient = w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.GetitemNumber(li_z,"unapplied_claim_txn_recipient_no")
//		ll_txn_unit_of_work_no = w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.GetitemNumber(li_z,"txn_unit_of_work_no")
//		//IF w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.GetitemNumber(li_z,"payment_claim_no") = il_claim_no 
//		IF ll_recipient = il_recipient_no and ll_txn_unit_of_work_no = il_txn_unit_of_work THEN
//			w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.selectrow(0,false)
//			w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.setrow(li_z)
//			w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.selectrow(li_z,true)
//			w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.setfocus()
//			ll_row = w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.getrow()
//			 w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.ScrollToRow(ll_row)
//			exit
//		END IF
//	Next
//	ib_auto_writeoff = false
//	
//END IF
//IF rb_auto_writeoff.checked = True and ib_auto_payment = True THEN
//	//set the row to the same row that was selected in the auto payment datawindow
//	
//   For li_x = 1 to ll_rcount
//		ll_recipient = w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.GetitemNumber(li_x,"unapplied_claim_txn_recipient_no")
//		ll_txn_unit_of_work_no = w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.GetitemNumber(li_x,"txn_unit_of_work_no")
//		//IF w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.GetitemNumber(li_x,"payment_claim_no") = il_claim_no THEN
//		IF ll_recipient = il_recipient_no and ll_txn_unit_of_work_no = il_txn_unit_of_work THEN	
//			w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.selectrow(0,false)
//			w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.setrow(li_x)
//			w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.selectrow(li_x,true)
//			w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.setfocus()
//			ll_row = w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.getrow()
//		   w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.ScrollToRow(ll_row)
//		END IF
//	Next
//	ib_auto_payment = false
//END IF

For li_z = 1 to ll_rcount
		
	ll_recipient = w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.GetitemNumber(li_z,"unapplied_claim_txn_recipient_no")
	ll_txn_unit_of_work_no = w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.GetitemNumber(li_z,"txn_unit_of_work_no")
	//IF w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.GetitemNumber(li_z,"payment_claim_no") = il_claim_no 
	IF ll_recipient = il_recipient_no and ll_txn_unit_of_work_no = il_txn_unit_of_work THEN
		w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.selectrow(0,false)
		w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.setrow(li_z)
		w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.selectrow(li_z,true)
		w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.setfocus()
		ll_row = w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.getrow()
		 w_maintain_manual_financial_trns.dw_unapplied_man_txns_list.ScrollToRow(ll_row)
		exit
	END IF
Next

RETURN ll_row_count
end function

public function integer wf_validate_cost_allocation ();// wf_validate_cost_allocation - validates the cost allocation and displays the name(s) on the dw.
//
// Parameter: as_calling_pgm - determines if error msgs should be displayed. They should only be displayed 
//                             when wf_validations is running, which only occurs when the data is being validated before saving.
//
BOOLEAN lb_restore_default_cost_allocation
Long    ll_cost_alloc_no, ll_cost_alloc_operation_no, ll_claim_no
Long    ll_default_cost_alloc_no, ll_default_cost_alloc_operation_no
Integer li_rtn
String  ls_cost_alloc_type_code, ls_default_cost_alloc_type_code, ls_payment_sub_type_code, ls_employer_name,ls_operation_name


IF dw_manual_cost_of_claims_allocated.RowCount() > 0 THEN
	ll_cost_alloc_no           = dw_manual_cost_of_claims_allocated.GetItemNumber(1, "cost_alloc_no")
	ll_cost_alloc_operation_no = dw_manual_cost_of_claims_allocated.GetItemNumber(1, "cost_alloc_operation_no")
	ls_cost_alloc_type_code    = dw_manual_cost_of_claims_allocated.GetItemString(1, "cost_alloc_type_code")
ELSE
	ll_cost_alloc_no           = 0
	ll_cost_alloc_operation_no = 0
	ls_cost_alloc_type_code    = ''
END IF

ls_payment_sub_type_code = dw_manual_payment_details.GetItemString(1,'payment_sub_type_code')
ll_claim_no = dw_manual_transaction_details.GetItemNumber(1, 'claim_no')

li_rtn = inv_manual_financial.nf_validate_cost_allocation(ls_payment_sub_type_code, ll_claim_no, ll_cost_alloc_no, ll_cost_alloc_operation_no, ls_cost_alloc_type_code,&
                                                          lb_restore_default_cost_allocation, ll_default_cost_alloc_no, ll_default_cost_alloc_operation_no, ls_default_cost_alloc_type_code)

IF li_rtn < 0 THEN 
	IF lb_restore_default_cost_allocation THEN
		dw_manual_cost_of_claims_allocated.SetItem(1, "cost_alloc_no", ll_default_cost_alloc_no)
		dw_manual_cost_of_claims_allocated.SetItem(1, "cost_alloc_operation_no", ll_default_cost_alloc_operation_no)
		dw_manual_cost_of_claims_allocated.SetItem(1, "cost_alloc_type_code", ls_default_cost_alloc_type_code)
		
		// set cost alloc names
		IF dw_manual_cost_of_claims_allocated.GetRow() = 1 THEN
			ll_cost_alloc_no = dw_manual_cost_of_claims_allocated.GetItemNumber(1,'cost_alloc_no')
			ll_cost_alloc_operation_no = dw_manual_cost_of_claims_allocated.GetItemNumber(1,'cost_alloc_operation_no')
			
			inv_manual_financial.nf_display_cost_allocation_data(ll_cost_alloc_no,ll_cost_alloc_operation_no,ls_employer_name,ls_operation_name)
			
			dw_manual_cost_of_claims_allocated.SetItem(1,'cost_alloc_name',ls_employer_name)
			dw_manual_cost_of_claims_allocated.SetItem(1,'cost_alloc_group_name',ls_operation_name)
			
			// give user a chance to review the newly set cost allocation
			RETURN -1
		END IF
		
	ELSE
		RETURN li_rtn
	END IF
END IF

//dw_manual_cost_of_claims_allocated.SetItemStatus(1, 0, Primary!, NotModified!)

RETURN 1
end function

public function integer wf_filter_payment_sub_type_code (date adt_paid_from_date, date adt_paid_to_date);INTEGER            li_rtn, li_rows
STRING             ls_from_date, ls_to_date, ls_filter
DATAWINDOWCHILD    ldwc_annuity_payment_sub_type_code


ls_from_date = String(adt_paid_from_date,'YYYY-MM-DD')
ls_to_date = String(adt_paid_to_date,'YYYY-MM-DD')

IF ls_to_date = '1993-01-01' THEN
	// if the user has entered 1993-01-01 as the paid to date
	// then we will filter for the pre-93 effective dates
	ls_to_date = '1992-12-31'
END IF

ls_filter = 'effective_from_date <= ' + ls_from_date  + ' and (effective_to_date >= ' + ls_to_date  +  ' or ISNULL(effective_to_date))'

dw_manual_payment_details.GetChild('payment_sub_type_code',ldwc_annuity_payment_sub_type_code)
ldwc_annuity_payment_sub_type_code.SetTransObject(SQLCA)
li_rows = ldwc_annuity_payment_sub_type_code.Retrieve()
SQLCA.nf_handle_error('w_maintain_manual_financial_trns', 'dw_manual_payment_details', 'ItemFocusChanged')
	
li_rtn= ldwc_annuity_payment_sub_type_code.SetFilter( ls_filter) 
li_rtn= ldwc_annuity_payment_sub_type_code.Filter()

RETURN 0

end function

public function integer wf_validate_paid_date_range_pymt_type (string as_payment_sub_type_code, date adt_paid_from_date, date adt_paid_to_date);INTEGER    li_count
DATE       ldt_day_before

ldt_day_before = RelativeDate (adt_paid_to_date, -1)

SELECT Count(*)
INTO   :li_count
FROM   Payment_Sub_Type a
JOIN   Module_Payment_Sub_Type b ON a.payment_type_code     = b.payment_type_code 
                                AND a.payment_sub_type_code = b.payment_sub_type_code
JOIN   Work_Group_Payment_Sub_Type c ON a.payment_type_code     = c.payment_type_code
                                    AND a.payment_sub_type_code = c.payment_sub_type_code 
WHERE  b.module_code       = '003'
AND    c.work_group_code   = 'FS' 
AND    c.active_flag       = 'Y' 
AND    c.payment_type_code = '97'
AND    c.payment_sub_type_code = :as_payment_sub_type_code
AND  ( c.effective_from_date <= :adt_paid_from_date
AND  ( c.effective_to_date   >= :ldt_day_before
 OR    c.effective_to_date   IS NULL ))
USING SQLCA;
SQLCA.nf_handle_error('w_maintain_manual_financial_trns', 'embedded SQL: SELECT Count(*) FROM Payment_Sub_Type JOIN Module_Payment_Sub_Type JOIN Work_Group_Payment_Sub_Type...', 'wf_validate_paid_date_range_pymt_type')
 
RETURN li_count
end function

public subroutine wf_protect_cheque_no (boolean ab_protect);
IF ab_protect THEN
	dw_manual_transaction_details.Object.cheque_no.Background.Mode  = '1' // transparent
	dw_manual_transaction_details.Object.cheque_no.Border           = '0' // None
	dw_manual_transaction_details.Object.cheque_no.Protect          = '1' // FALSE
ELSE
	dw_manual_transaction_details.Object.cheque_no.Background.Mode  = '0' // opaque
	dw_manual_transaction_details.Object.cheque_no.Background.Color = RGB(255,255,255) // white
	dw_manual_transaction_details.Object.cheque_no.Border           = '5' // 3D Lowered
	dw_manual_transaction_details.Object.cheque_no.Protect          = '0' // TRUE
END IF
end subroutine

public function integer wf_check_subledger_ss ();//The credit memo  amount must equal the annuity sub-ledger balance for the surviving spouse
boolean lb_unapplied_CM_exists
long ll_claim_no, ll_payment_no, ll_recipient_no
decimal{4} ldec_net_payment, ldec_amount, ldec_net_payment_2
string ls_payment_sub_type_code

ll_claim_no = dw_manual_transaction_details.getitemnumber(1,'claim_no')
ldec_amount = dw_manual_transaction_details.getitemdecimal(1,'txn_amount')
ll_payment_no = dw_manual_transaction_details.getitemnumber(1,'payment_no')
ll_recipient_no = dw_manual_transaction_details.getitemnumber(1,'recipient_no')

ls_payment_sub_type_code = dw_manual_payment_details.getitemstring(1,'payment_sub_type_code')


// Get the net payment for the current txn and subtract it from the total and then check if the txn amounts equal
Select net_payment_amount
Into :ldec_net_payment_2
From PAYMENT 
Where payment_no = :ll_payment_no
Using SQLCA;

SQLCA.nf_handle_error('w_add_manual_financial_trns','wf_setup_post92_txns','Select net_payment_amount) From PAYMENT')

//Get the subledger balance
SELECT sum(a.net_payment_amount)
INTO   :ldec_net_payment
FROM   PAYMENT a
WHERE  a.payment_type_code = '97' 
AND    a.claim_no          = :ll_claim_no
AND ( EXISTS ( SELECT *
               FROM   APPLIED_CLAIM_TXN b
					WHERE  b.payment_no          = a.payment_no
					AND    b.recipient_no        = :ll_recipient_no
               AND    b.recipient_type_code = 'I' )
OR    EXISTS ( SELECT *
               FROM   UNAPPLIED_CLAIM_TXN c
					WHERE  c.payment_no          = a.payment_no
					AND    c.recipient_no        = :ll_recipient_no
               AND    c.recipient_type_code = 'I' ) )
Using SQLCA;

SQLCA.nf_handle_error('w_add_manual_financial_trns','wf_check_subledger_ss','Select sum(a.total_payment_amount) From PAYMENT a')

IF IsNull(ldec_net_payment) THEN ldec_net_payment = 0

IF ls_payment_sub_type_code = 'CM' OR ls_payment_sub_type_code = 'CO' OR ls_payment_sub_type_code = 'CW' THEN ldec_amount = (ldec_amount * -1)

IF ls_payment_sub_type_code = 'CM' OR ls_payment_sub_type_code = 'CO' OR ls_payment_sub_type_code = 'CW' THEN
	IF ldec_amount <> (ldec_net_payment - ldec_net_payment_2) THEN
		MessageBox('Error',' The Annuity Payment amount must equal the annuity sub-ledger balance for the surviving spouse.', Exclamation!)
		Return -1
	END IF
END IF


Return 1
end function

public function integer wf_find_post92_cm (long al_claim_no, long al_recipient_no, string as_message);INTEGER li_unprocessed_CMs

// get the unprocessed CM sum
SELECT Count(*)
INTO   :li_unprocessed_CMs
FROM   PAYMENT a
JOIN   UNAPPLIED_CLAIM_TXN b ON a.payment_no = b.payment_no
WHERE  a.payment_type_code     = '97'
AND    a.payment_sub_type_code = 'CM'
AND NOT EXISTS (SELECT c.*
                FROM   ANNUITY_PAYOUT_PRE_1993 c
                WHERE  c.claim_no          = a.claim_no
                AND    c.payout_payment_no = a.payment_no)
AND NOT EXISTS (SELECT d.*
                FROM   ANNUITY_SETASIDE_PRE_1993 d
                WHERE  d.claim_no            = a.claim_no
                AND    d.setaside_payment_no = a.payment_no)
AND    a.claim_no     = :al_claim_no
AND    b.recipient_no = :al_recipient_no
AND    b.recipient_type_code = 'I'
USING SQLCA;

SQLCA.nf_handle_error('w_maintain_manual_financial', 'embedded SQL: select sum(net_payment_amount) from PAYMENT', 'wf_delete_post92_credit_memo')

// If there are any unprocessed credit memos, then obviously the subledger balance will no longer be zero
// so, prevent the deletion/modification of subledger payments

IF li_unprocessed_CMs > 0 THEN
	MessageBox('Annuity Validation Warning', as_message)
	RETURN -1
ELSE
	RETURN 0
END IF
end function

public function integer wf_find_pre93_cm (long al_claim_no, long al_recipient_no, string as_message);INTEGER li_unprocessed_CMs

// get the unprocessed post-92 CM sum
SELECT Count(*)
INTO   :li_unprocessed_CMs
FROM   PAYMENT a
JOIN   UNAPPLIED_CLAIM_TXN b on a.payment_no = b.payment_no
WHERE  a.payment_type_code     = '97'
AND    a.payment_sub_type_code = 'CM'
AND  EXISTS (SELECT c.*
             FROM   ANNUITY_PAYOUT_PRE_1993 c
             WHERE  c.claim_no = a.claim_no
             AND    c.payout_payment_no = a.payment_no)
AND    a.claim_no            = :al_claim_no
AND    b.recipient_no        = :al_recipient_no
AND    b.recipient_type_code = 'I'
USING SQLCA;

SQLCA.nf_handle_error('w_maintain_manual_financial', 'embedded SQL: select sum(net_payment_amount) from PAYMENT', 'wf_delete_post92_credit_memo')

// If there are any other credit memos, then obviously the subledger balance will no longer be zero
// so, prevent the deletion

IF li_unprocessed_CMs > 0 THEN
	MessageBox('Annuity Validation Warning', as_message)
	RETURN -1
ELSE
	RETURN 1
END IF
end function

public function integer wf_find_ss_credit_memo (long al_claim_no, long al_recipient_no, string as_message);INTEGER li_unprocessed_CMs

// get the unprocessed CM sum
SELECT Count(*)
INTO   :li_unprocessed_CMs
FROM   PAYMENT a
JOIN   UNAPPLIED_CLAIM_TXN b on a.payment_no = b.payment_no
WHERE  a.payment_type_code     = '97'
AND    a.payment_sub_type_code = 'CM'
AND    a.claim_no            = :al_claim_no
AND    b.recipient_no        = :al_recipient_no
AND    b.recipient_type_code = 'I'
USING SQLCA;

SQLCA.nf_handle_error('w_maintain_manual_financial', 'embedded SQL: select sum(net_payment_amount) from PAYMENT', 'wf_delete_post92_credit_memo')

// If there are any other credit memos, then obviously the subledger balance will no longer be zero
// so, prevent the deletion/modification of subledger payments

IF li_unprocessed_CMs > 0 THEN
	MessageBox('Annuity Validation Warning', as_message)
	RETURN -1
ELSE
	RETURN 1
END IF
end function

public function integer wf_process_annuity_payout_cheque ();
long  ll_recipient_no, ll_current_details_row, ll_current_list_row,  ll_cheque_no,  &
       ll_original_cheque_no,  ll_txn_unit_of_work_no, ll_annuity_payout_no

INT li_count, li_count2
STRING ls_new_or_revised
datetime ldt_date_time

setNull(ldt_date_time)

dw_manual_transaction_details.accepttext()
ll_current_details_row = dw_manual_transaction_details.getRow()
ll_current_list_row    = dw_unapplied_man_txns_list.getRow()

ll_cheque_no          = dw_manual_transaction_details.getItemNumber(ll_current_details_row,  'cheque_no')
ll_original_cheque_no = dw_manual_transaction_details.getItemNumber(ll_current_details_row,  'cheque_no', PRIMARY!,TRUE)

ll_recipient_no          = dw_unapplied_man_txns_list.getItemNumber(ll_current_list_row,  'annuity_payout_recipient_no')
ll_txn_unit_of_work_no   = dw_unapplied_man_txns_list.getItemNumber(ll_current_list_row,  'txn_unit_of_work_no')
ll_annuity_payout_no     = dw_unapplied_man_txns_list.getItemNumber(ll_current_list_row,  'annuity_payout_no')

IF  ll_original_cheque_no = 0  THEN
	ls_new_or_revised = 'new'
ELSEIF ll_cheque_no = 0 AND ll_original_cheque_no <> 0 THEN
	ls_new_or_revised = 'remove'
ELSEIF  ll_cheque_no <> ll_original_cheque_no  THEN
	ls_new_or_revised = 'revised'
ELSE
	ls_new_or_revised = ''
END IF


IF 	ls_new_or_revised = 'remove' OR ls_new_or_revised = 'revised' THEN
	SELECT  transmit_date
	INTO    :ldt_date_time
	FROM    CHEQUE_HEADER
	WHERE   cheque_no = :ll_original_cheque_no
	USING   SQLCA;
	SQLCA.nf_handle_error("w_maintain_manual_financial_trns","wf_process_annuity_payout_cheque","SELECT transmit_date from CHEQUE_HEADER")
	
	IF not isnull(ldt_date_time) THEN
		MESSAGEBOX("INVALID CHEQUE No", "This cheque has been transmitted. You cannot make a change to the cheque no.")
		RETURN -1
	END IF
END IF

IF ls_new_or_revised = 'new' OR ls_new_or_revised = 'revised' THEN
	
	SELECT  count(cheque_no) 
   INTO    :li_count
	FROM    CHEQUE_HEADER
	WHERE   cheque_no = :ll_cheque_no
	USING   SQLCA;
	SQLCA.nf_handle_error("w_maintain_manual_financial_trns","wf_process_annuity_payout_cheque","Select count from CHEQUE_HEADER")
	
	SELECT  count(cheque_no) 
   INTO    :li_count2
	FROM    UNAPPLIED_CLAIM_TXN
	WHERE   cheque_no = :ll_cheque_no
	AND     txn_no > 0
	USING   SQLCA;
	SQLCA.nf_handle_error("w_maintain_manual_financial_trns","wf_process_annuity_payout_cheque","Select count from UNAPPLIED_CLAIM_TXN")
	
	IF (li_count + li_count2) > 0 THEN
		MESSAGEBOX("INVALID CHEQUE No", "A cheque with this cheque number already exists. Please enter a valid non-used cheque number.")
		RETURN -1
	END IF	
END IF

IF ls_new_or_revised <> '' THEN
	
	DECLARE lp_update_cheque PROCEDURE FOR p_process_annuity_payout_cheque
			@annuity_payout_no    = :ll_annuity_payout_no,
			@txn_unit_of_work_no  = :ll_txn_unit_of_work_no,
			@recipient_no         = :ll_recipient_no,
			@cheque_no            = :ll_cheque_no,
			@original_cheque_no   = :ll_original_cheque_no,
			@new_or_revised       = :ls_new_or_revised 
	USING SQLCA;
	EXECUTE  lp_update_cheque;
	SQLCA.nf_handle_error("w_maintain_manual_financial_trns","wf_process_annuity_payout_cheque","execute p_process_annuity_payout_cheque")
	
	// Reset the update flag, to prevent the details datawindow (UNAPPLIED_CLAIM_TXN) from updating 
	// during the save, and causing an error. All updating is done in the stored procedure
	dw_manual_transaction_details.ResetUpdate()
	
	// Do an informational check: If all txn's have had a cheque_no entered, AND there is at least one write-off ('CW')
	// for the txn unit of work, then inform the user that they need to check the ready to process flag manuall on the writeoff tab
	SELECT  count(*)
	INTO    :li_count
   FROM    ANNUITY_PAYOUT_TXN_DETAIL a 
   JOIN    UNAPPLIED_CLAIM_TXN b on a.txn_no = b.txn_no
   JOIN    TXN_UNIT_OF_WORK    c on b.txn_unit_of_work_no = c.txn_unit_of_work_no
   WHERE   a.payment_sub_type_code in ('CM', 'CO')
   AND     b.txn_unit_of_work_no =  :ll_txn_unit_of_work_no
   AND     b.cheque_no = 0 
	USING   SQLCA;
	SQLCA.nf_handle_error("w_maintain_manual_financial_trns","wf_process_annuity_payout_cheque","Select count from ANNUITY_PAYOUT_TXN_DETAIL")
	
	// IF li_count = 0 then all txn's in this unit of work have had a cheque number entered. 
	//  Now check for any CW's, and if one or more exists, inform the user.
	IF li_count = 0 THEN
		
		SELECT count(*)
		INTO   :li_count2
		FROM   ANNUITY_PAYOUT_TXN_DETAIL a 
		JOIN   UNAPPLIED_CLAIM_TXN b on a.txn_no = b.txn_no
		JOIN   TXN_UNIT_OF_WORK    c on b.txn_unit_of_work_no = c.txn_unit_of_work_no
		WHERE  a.payment_sub_type_code in ('CW')
		AND    b.txn_unit_of_work_no =  :ll_txn_unit_of_work_no
		AND    c.ready_to_process_flag = 'N'
		USING  SQLCA;
		SQLCA.nf_handle_error("w_maintain_manual_financial_trns","wf_process_annuity_payout_cheque","Select count from ANNUITY_PAYOUT_TXN_DETAIL")
	
		IF li_count2 > 0 THEN
			MESSAGEBOX("Information", "Please be aware that there is at least one write-off transaction waiting to be processed." &
		                       + "~r~nYou must manually check its ready-to-process flag and save, in order for this batch to be processed.", INFORMATION!) 
		END IF
	END IF
	
END IF

RETURN 1
end function

on w_maintain_manual_financial_trns.create
int iCurrent
call super::create
if IsValid(this.MenuID) then destroy(this.MenuID)
if this.MenuName = "m_cmwb_notools" then this.MenuID = create m_cmwb_notools
this.cb_extract=create cb_extract
this.rb_auto_writeoff=create rb_auto_writeoff
this.rb_auto_payment=create rb_auto_payment
this.rb_all=create rb_all
this.rb_auto=create rb_auto
this.rb_manual=create rb_manual
this.dw_manual_cost_of_claims_allocated=create dw_manual_cost_of_claims_allocated
this.cb_delete=create cb_delete
this.cb_cancel=create cb_cancel
this.cb_close=create cb_close
this.cb_save=create cb_save
this.dw_manual_payment_details=create dw_manual_payment_details
this.gb_2=create gb_2
this.dw_manual_transaction_details=create dw_manual_transaction_details
this.dw_cheque_details=create dw_cheque_details
this.gb_1=create gb_1
this.dw_ready_to_process=create dw_ready_to_process
this.dw_unapplied_man_txns_list=create dw_unapplied_man_txns_list
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_extract
this.Control[iCurrent+2]=this.rb_auto_writeoff
this.Control[iCurrent+3]=this.rb_auto_payment
this.Control[iCurrent+4]=this.rb_all
this.Control[iCurrent+5]=this.rb_auto
this.Control[iCurrent+6]=this.rb_manual
this.Control[iCurrent+7]=this.dw_manual_cost_of_claims_allocated
this.Control[iCurrent+8]=this.cb_delete
this.Control[iCurrent+9]=this.cb_cancel
this.Control[iCurrent+10]=this.cb_close
this.Control[iCurrent+11]=this.cb_save
this.Control[iCurrent+12]=this.dw_manual_payment_details
this.Control[iCurrent+13]=this.gb_2
this.Control[iCurrent+14]=this.dw_manual_transaction_details
this.Control[iCurrent+15]=this.dw_cheque_details
this.Control[iCurrent+16]=this.gb_1
this.Control[iCurrent+17]=this.dw_ready_to_process
this.Control[iCurrent+18]=this.dw_unapplied_man_txns_list
end on

on w_maintain_manual_financial_trns.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_extract)
destroy(this.rb_auto_writeoff)
destroy(this.rb_auto_payment)
destroy(this.rb_all)
destroy(this.rb_auto)
destroy(this.rb_manual)
destroy(this.dw_manual_cost_of_claims_allocated)
destroy(this.cb_delete)
destroy(this.cb_cancel)
destroy(this.cb_close)
destroy(this.cb_save)
destroy(this.dw_manual_payment_details)
destroy(this.gb_2)
destroy(this.dw_manual_transaction_details)
destroy(this.dw_cheque_details)
destroy(this.gb_1)
destroy(this.dw_ready_to_process)
destroy(this.dw_unapplied_man_txns_list)
end on

event open;call super::open;inv_manual_financial = Create n_manual_financial

wf_setup_datawindows()

// Disable the save button until some data has been changed
cb_save.Enabled = False
end event

event closequery;call super::closequery;IF cb_save.enabled THEN
   IF MessageBox('Warning', 'Data not saved.  Save now?', Question!, YesNo!) = 1 THEN
      cb_save.TriggerEvent(Clicked!)
      IF cb_save.enabled THEN
         Message.ReturnValue = 1
      END IF
   END IF
END IF

end event

type cb_extract from commandbutton within w_maintain_manual_financial_trns
integer x = 1294
integer y = 2400
integer width = 283
integer height = 100
integer taborder = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Extract"
end type

event clicked;String ls_directoryname, ls_pathname, ls_filename
STRING ls_current_date
integer li_rtn
LONG ll_txn_unit_of_work_no, ll_rows, ll_insert_row, ll_dw_row, ll_annuity_payout_no
u_ds lds_extract, lds_extract_data

ll_txn_unit_of_work_no = dw_unapplied_man_txns_list.getItemNumber(dw_unapplied_man_txns_list.getRow(),'txn_unit_of_work_no')
IF ll_txn_unit_of_work_no <= 0 THEN
	MESSAGEBOX("Data Error", "No transaction unit of work number found for the selected transaction")
	RETURN
END IF

ll_annuity_payout_no = dw_unapplied_man_txns_list.getItemNumber(dw_unapplied_man_txns_list.getRow(),'annuity_payout_no')

SELECT count(*)
INTO   :li_rtn
FROM   ANNUITY_PAYOUT_RECIPIENT
WHERE annuity_payout_no = :ll_annuity_payout_no
AND     cheque_no > 0;
SQLCA.nf_handle_error("w_maintain_manual_financial_trns", "cb_extract", "Select count from ANNUITY_PAYOUT_RECIPIENT")

IF li_rtn > 0 THEN
	MESSAGEBOX("Extract Error", "At least one cheque number has been entered for this transaction-unit-of-work. Extract cannot done.",INFORMATION!)
	RETURN
END IF

//Get the extract file name
ls_pathname = ProfileString(vgs_ini_filename, 'ANNUITY CHEQUE EXTRACT', 'AnnuityChequeExtractPathName', "")

//Make sure the file name exists
IF ls_pathname = ""  THEN
	RETURN SignalError(-666, "Error locating the CMWB.INI value for 'AnnuityChequeExtractPathName'. Call The Help Desk.")
END IF

//Make sure the directory exists
ls_directoryname = left(ls_pathname, LASTPOS( ls_pathname,'\'))

IF NOT DirectoryExists(ls_directoryname) THEN
	Return SignalError(-666, "The directory '"+ ls_directoryname +"' cannot be found. Call The Help Desk.")
END IF	

li_rtn = GetFileSaveName( "Extract To:",  ls_pathname, ls_filename, 'XLS', "EXCEL FILES (*.XLS*),*.xls*" )

IF li_rtn = 1 then
	IF upper(right(ls_pathname,4)) <> '.XLS' then 
		    Messagebox("File Name Error", "Wrong file extension!. Please specify a valid file name.")
			Return
	END IF
	
	ls_current_date = STRING(DATE(f_server_datetime()),'yyyy-mm-dd')

    ls_current_date = LEFT(ls_current_date,4)   + ' - '   + &
	                         MID(ls_current_date,6,2) + ' - '   + &
							MID(ls_current_date,9,2) 
 
		
	lds_extract = create u_ds
	lds_extract.dataobject = 'd_annuity_payout_extract'
	lds_extract.settransobject(SQLCA)
	
	lds_extract_data = create u_ds
	lds_extract_data.dataobject = 'd_annuity_payout_extract_data'
	lds_extract_data.settransobject(SQLCA)
	
		
	IF ll_txn_unit_of_work_no > 0 THEN
		ll_rows = lds_extract_data.retrieve(ll_txn_unit_of_work_no)

		FOR ll_dw_row = 1 to ll_rows
			ll_insert_row = lds_extract.insertRow(0)
			
			lds_extract.setItem(ll_insert_row, 'checkstub_individual_no', lds_extract_data.getItemNumber(ll_dw_row, 'recipient_no'))
			lds_extract.setItem(ll_insert_row, 'checkstub_yyyymmdd', ls_current_date)
			lds_extract.setItem(ll_insert_row, 'checkstub_recipient', lds_extract_data.getItemString(ll_dw_row,'checkstub_recipient'))
			lds_extract.setItem(ll_insert_row, 'checkstub_payment_type',  lds_extract_data.getItemString(ll_dw_row,'payment_sub_type_desc'))
			lds_extract.setItem(ll_insert_row, 'checkstub_amount', STRING(lds_extract_data.getItemDecimal(ll_dw_row, 'net_annuity_payout_amount'),'$#,##0.00'))
			lds_extract.setItem(ll_insert_row, 'yyyymmdd',  ls_current_date)
			lds_extract.setItem(ll_insert_row, 'cheque_amount', STRING(lds_extract_data.getItemDecimal(ll_dw_row, 'net_annuity_payout_amount'),'#,##0.00'))
			lds_extract.setItem(ll_insert_row, 'payable_to', lds_extract_data.getItemString(ll_dw_row, 'name_on_cheque'))
			lds_extract.setItem(ll_insert_row, 'address1',   lds_extract_data.getItemString(ll_dw_row, 'address_line1'))
			lds_extract.setItem(ll_insert_row, 'address2',   lds_extract_data.getItemString(ll_dw_row, 'address_line2'))
			lds_extract.setItem(ll_insert_row, 'address3',   lds_extract_data.getItemString(ll_dw_row, 'address_line3'))
			lds_extract.setItem(ll_insert_row, 'address4',   lds_extract_data.getItemString(ll_dw_row, 'address_line4'))
			lds_extract.setItem(ll_insert_row, 'address5',   lds_extract_data.getItemString(ll_dw_row, 'address_line5'))

		NEXT
		
		li_rtn = lds_extract.SaveAs(ls_pathname, EXCEL8!, TRUE)
		IF li_rtn <> 1 THEN
			MESSAGEBOX("File Error", "The file was not extracted. Check to see if a file by the same name exists and is already open.")
			RETURN
		END IF
	END IF
END IF


end event

type rb_auto_writeoff from radiobutton within w_maintain_manual_financial_trns
integer x = 1568
integer y = 88
integer width = 475
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Auto Write-Off"
end type

event clicked;IF cb_save.enabled = True THEN 
	IF is_radio_button = "rb_auto_payment" THEN 
		rb_auto_payment.checked = true
		Messagebox('Error','Please Save or Cancel before changing to the auto writeoff window.',exclamation!)
		Return
	ELSEIF is_radio_button = "rb_manual" THEN 
		rb_manual.checked = true
		Messagebox('Error','Please Save or Cancel before changing to the auto writeoff window.',exclamation!)
		Return
	ELSEIF is_radio_button = "rb_auto" THEN 
		rb_auto.checked = true
		Messagebox('Error','Please Save or Cancel before changing to the auto writeoff window.',exclamation!)
		Return
	ELSEIF is_radio_button = "rb_all" THEN 
		rb_all.checked = true
		Messagebox('Error','Please Save or Cancel before changing to the auto writeoff window.',exclamation!)
		Return
	END IF
ELSE
	wf_setup_datawindows()
END IF

end event

type rb_auto_payment from radiobutton within w_maintain_manual_financial_trns
integer x = 1038
integer y = 88
integer width = 457
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Auto Payment"
end type

event clicked;IF cb_save.enabled = True THEN 
	IF is_radio_button = "rb_manual" THEN 
		rb_auto_payment.checked = true
		Messagebox('Error','Please Save or Cancel before changing to the auto payment window.',exclamation!)
		Return
	ELSEIF is_radio_button = "rb_auto_writeoff" THEN 
		rb_auto_writeoff.checked = true
		Messagebox('Error','Please Save or Cancel before changing to the auto payment window.',exclamation!)
		Return
	ELSEIF is_radio_button = "rb_auto" THEN 
		rb_auto.checked = true
		Messagebox('Error','Please Save or Cancel before changing to the auto payment window.',exclamation!)
		Return
	ELSEIF is_radio_button = "rb_all" THEN 
		rb_all.checked = true
		Messagebox('Error','Please Save or Cancel before changing to the auto payment window.',exclamation!)
		Return
	END IF
ELSE
	wf_setup_datawindows()
END IF

end event

event losefocus;
//is_radio_button = "rb_auto_payment"
end event

type rb_all from radiobutton within w_maintain_manual_financial_trns
integer x = 2135
integer y = 88
integer width = 215
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "All"
end type

event clicked;
IF cb_save.enabled = True THEN 
	IF is_radio_button = "rb_auto_payment" THEN
		Messagebox('Error','Please Save or Cancel before changing to the All window.',exclamation!)
		rb_auto_payment.checked = true
		Return
	ELSEIF is_radio_button = "rb_auto_writeoff" THEN 
		rb_auto_writeoff.checked = true
		Messagebox('Error','Please Save or Cancel before changing to the All window.',exclamation!)
		Return
	ELSEIF is_radio_button = "rb_auto" THEN 
		rb_auto.checked = true
		Messagebox('Error','Please Save or Cancel before changing to the All window.',exclamation!)
		Return
	ELSEIF is_radio_button = "rb_manual" THEN 
		rb_manual.checked = true
		Messagebox('Error','Please Save or Cancel before changing to the All window.',exclamation!)
		Return
	END IF
ELSE
	wf_setup_datawindows()
END IF
end event

type rb_auto from radiobutton within w_maintain_manual_financial_trns
integer x = 430
integer y = 88
integer width = 512
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Auto Adjustment"
end type

event clicked;
IF cb_save.enabled = True THEN 
	IF is_radio_button = "rb_auto_payment" THEN 
		rb_auto_payment.checked = true
		Messagebox('Error','Please Save or Cancel before changing to the Auto Adjustment window.',exclamation!)
		Return
	ELSEIF is_radio_button = "rb_auto_writeoff" THEN 
		rb_auto_writeoff.checked = true
		Messagebox('Error','Please Save or Cancel before changing to the Auto Adjustment window.',exclamation!)
		Return
	ELSEIF is_radio_button = "rb_manual" THEN 
		rb_manual.checked = true
		Messagebox('Error','Please Save or Cancel before changing to the Auto Adjustment window.',exclamation!)
		Return
	ELSEIF is_radio_button = "rb_all" THEN 
		rb_all.checked = true
		Messagebox('Error','Please Save or Cancel before changing to the Auto Adjustment window.',exclamation!)
		Return
	END IF
ELSE
	wf_setup_datawindows()
END IF

end event

type rb_manual from radiobutton within w_maintain_manual_financial_trns
integer x = 64
integer y = 88
integer width = 306
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Manual"
boolean checked = true
end type

event clicked;
IF cb_save.enabled = True THEN 
	IF is_radio_button = "rb_auto_payment" THEN 
		rb_auto_payment.checked = true
		Messagebox('Error','Please Save or Cancel before changing to the manual window.',exclamation!)
		Return
	ELSEIF is_radio_button = "rb_auto_writeoff" THEN 
		rb_auto_writeoff.checked = true
		Messagebox('Error','Please Save or Cancel before changing to the manual window.',exclamation!)
		Return
	ELSEIF is_radio_button = "rb_auto" THEN 
		rb_auto.checked = true
		Messagebox('Error','Please Save or Cancel before changing to the manual window.',exclamation!)
		Return
	ELSEIF is_radio_button = "rb_all" THEN 
		rb_all.checked = true
		Messagebox('Error','Please Save or Cancel before changing to the manual window.',exclamation!)
		Return
	END IF
ELSE
	wf_setup_datawindows()
END IF

end event

type dw_manual_cost_of_claims_allocated from u_dw_online within w_maintain_manual_financial_trns
integer x = 18
integer y = 2144
integer width = 2903
integer height = 204
integer taborder = 30
string title = "none"
string dataobject = "d_manual_cost_of_claims_allocated"
boolean border = false
borderstyle borderstyle = stylelowered!
end type

event itemchanged;INTEGER     li_rtn
LONG        ll_cost_alloc_no, ll_cost_alloc_operation_no, ll_claim_no
STRING      ls_cost_alloc_type_code, ls_employer_name, ls_operation_name

cb_save.Enabled = True

CHOOSE CASE dwo.name
	CASE 'cost_alloc_no'
		ll_cost_alloc_no = Long(data)
		ll_claim_no = THIS.GetItemNumber(1,'claim_no')
		IF IsNull(ll_cost_alloc_no) = TRUE THEN
			ll_cost_alloc_no = 0
		END IF

		// If a value has been entered for Cost Allocation, then verify the number and get the name.
		IF ll_cost_alloc_no < 1 THEN
			THIS.SetItem(1, 'cost_alloc_name', '')
			THIS.SetItem(1, 'cost_alloc_group_name', '')
			THIS.SetItem(1, 'cost_alloc_operation_no', 0)
			THIS.SetItem(1, 'cost_alloc_group_name', '')
			RETURN 0
		ELSE
			li_rtn = inv_manual_financial.nf_validate_cost_alloc_no(ll_cost_alloc_no,ls_cost_alloc_type_code)
			IF li_rtn < 0 THEN
				THIS.SetItem(1, 'cost_alloc_name', '')
				THIS.SetItem(1, 'cost_alloc_group_name', '')
				THIS.SetItem(1, 'cost_alloc_operation_no', 0)
				THIS.SetItem(1, 'cost_alloc_group_name', '')
				RETURN 1
			END IF
			li_rtn = inv_manual_financial.nf_cost_allocation_warnings(ll_claim_no,ll_cost_alloc_no)
			IF li_rtn < 0 THEN
				THIS.SetItem(1, 'cost_alloc_name', '')
				THIS.SetItem(1, 'cost_alloc_group_name', '')
				THIS.SetItem(1, 'cost_alloc_operation_no', 0)
				THIS.SetItem(1, 'cost_alloc_group_name', '')
				RETURN 1
			END IF
		END IF
		
		// If a value has been entered for both Cost Allocation and Operation No, then revalidate operation number.
		ll_cost_alloc_operation_no = THIS.GetItemNumber(1,'cost_alloc_operation_no')
		IF ll_cost_alloc_no > 0 AND ll_cost_alloc_operation_no > 0 THEN
			li_rtn = inv_manual_financial.nf_validate_cost_alloc_operation_no(ll_cost_alloc_no,ll_cost_alloc_operation_no)
			IF li_rtn < 0 THEN
				THIS.SetItem(1, 'cost_alloc_name', '')
				THIS.SetItem(1, 'cost_alloc_group_name', '')
				THIS.SetItem(1, 'cost_alloc_operation_no', 0)
				THIS.SetItem(1, 'cost_alloc_group_name', '')
				RETURN 1
			ELSE
				inv_manual_financial.nf_display_cost_allocation_data(ll_cost_alloc_no,ll_cost_alloc_operation_no,ls_employer_name,ls_operation_name)
				THIS.SetItem(1, 'cost_alloc_name', ls_employer_name)
				THIS.SetItem(1, 'cost_alloc_group_name', ls_operation_name)
			END IF
		END IF		
		
	CASE 'cost_alloc_operation_no'
		ll_cost_alloc_no = THIS.GetItemNumber(1, 'cost_alloc_no')
		ll_cost_alloc_operation_no = Long(data)
		IF data = '' OR IsNull(data) = TRUE THEN
			ll_cost_alloc_operation_no = 0
		END IF

		// If a value has been entered for both Cost Allocation and Operation No, then retrieve the
		// operation name by calling the function to validate and retrieve operation number.
		IF ll_cost_alloc_no > 0 AND ll_cost_alloc_operation_no > 0 THEN
			li_rtn = inv_manual_financial.nf_validate_cost_alloc_operation_no(ll_cost_alloc_no,ll_cost_alloc_operation_no)
			IF li_rtn < 0 THEN
				THIS.SetItem(1, 'cost_alloc_group_name', '')
				RETURN 1
			END IF
			inv_manual_financial.nf_display_cost_allocation_data(ll_cost_alloc_no,ll_cost_alloc_operation_no,ls_employer_name,ls_operation_name)
			THIS.SetItem(1,'cost_alloc_name',ls_employer_name)
			THIS.SetItem(1,'cost_alloc_group_name',ls_operation_name)
		END IF		
END CHOOSE

end event

event dberror;SQLCA.SQLDBCode = sqldbcode
SQLCA.SQLErrText = sqlerrtext
RETURN 1

end event

event editchanged;call super::editchanged;IF dwo.name = 'cost_alloc_no' THEN
	THIS.SetItem(1,'cost_alloc_operation_no',0)
	THIS.SetItem(1,'cost_alloc_type_code','')
END IF
end event

type cb_delete from commandbutton within w_maintain_manual_financial_trns
integer x = 978
integer y = 2400
integer width = 274
integer height = 100
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Delete"
end type

event clicked;LONG     ll_payment_no, ll_payment_array[], ll_claim_no, ll_num_rows, ll_listrow, ll_recipient_no
INTEGER  li_rtn, li_counter, li_find, li_payment_rows
INTEGER  li_num_payment_rows, li_num_UCT_rows, li_num_MTL_rows, li_num_COCA_rows
INTEGER  li_num_pre93_payout_rows, li_num_pre93_setaside_rows
U_DS     lds_CM_payments_to_delete, lds_payments_to_delete, lds_unapplied_claim_txn_to_delete
U_DS     lds_manual_txn_link_to_delete, lds_coca_to_delete, lds_pre93_payout_to_delete, lds_pre93_setaside_to_delete
STRING   ls_message, ls_payment_sub_type_code
BOOLEAN  lb_unapplied_CM_exists, lb_unsaved

N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '003' refers to the Manual Financial Txn Maintenance module, '044' refers to the Payment Processing module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('003','044','delete',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF
/******************************************************************************************/

IF dw_manual_transaction_details.AcceptText() = -1 THEN RETURN -1
IF dw_manual_payment_details.AcceptText() = -1 THEN RETURN -1
IF dw_manual_cost_of_claims_allocated.AcceptText() = -1 THEN RETURN -1
	
IF dw_manual_transaction_details.GetItemStatus(1,0,Primary!) <> NotModified! THEN lb_unsaved = TRUE
IF dw_manual_payment_details.GetItemStatus(1,0,Primary!) <> NotModified! AND lb_unsaved = FALSE THEN lb_unsaved = TRUE
IF dw_manual_cost_of_claims_allocated.GetItemStatus(1,0,Primary!) <> NotModified! AND lb_unsaved = FALSE THEN lb_unsaved = TRUE
	
IF lb_unsaved THEN
	MessageBox('Unsaved Record','Please save or cancel the current record.',Exclamation!)
	RETURN -1
END IF

IF dw_manual_payment_details.RowCount() > 0 THEN
	ll_payment_no = dw_manual_payment_details.GetItemNumber(1,'payment_no')
	ll_claim_no = dw_manual_payment_details.GetItemNumber(1,'claim_no')
	ll_recipient_no = dw_manual_transaction_details.GetItemNumber(1,'recipient_no')
	ls_payment_sub_type_code = dw_manual_payment_details.getitemstring(1,'payment_sub_type_code')
	
	// Populate is_claim_role_code
	li_rtn = wf_validate_role_code(ll_recipient_no, ll_claim_no)
	IF li_rtn < 0 THEN RETURN li_rtn
	
	IF  ls_payment_sub_type_code <> 'CM' AND ls_payment_sub_type_code <> 'CO' AND ls_payment_sub_type_code <> 'CW' &
	AND ls_payment_sub_type_code <> 'DM' AND ls_payment_sub_type_code <> 'DO' AND ls_payment_sub_type_code <> 'DW'THEN
		IF is_claim_role_code = 'C' THEN
			IF ib_post92 THEN
				lb_unapplied_CM_exists = inv_manual_financial.nf_unapplied_CM_exists(ll_claim_no,ll_recipient_no,'post92')
				IF lb_unapplied_CM_exists THEN
					MessageBox('Cannot Delete','You cannot delete this post-1992 manual set-aside payment.'&
												 +'~r~nThere is an existing unprocessed post-1992 manual Annuity Payment for this claim.')
					RETURN -1
				END IF
			ELSE
				lb_unapplied_CM_exists = inv_manual_financial.nf_unapplied_CM_exists(ll_claim_no,ll_recipient_no,'pre93')
				IF lb_unapplied_CM_exists THEN
					MessageBox('Cannot Delete','You cannot delete this pre-1993 manual set-aside payment.'&
												 +'~r~nThere is an existing unprocessed pre-1993 manual Annuity Payment for this claim.')
					RETURN -1
				END IF
			END IF
		ELSEIF is_claim_role_code = 'SS' THEN
			lb_unapplied_CM_exists = inv_manual_financial.nf_unapplied_CM_exists(ll_claim_no,ll_recipient_no,'SS')
			IF lb_unapplied_CM_exists THEN
				MessageBox('Cannot Delete','You cannot delete this surviving spouse annuity manual set-aside payment.'&
		                           +'~r~nThere is an existing unprocessed manual Annuity Payment for this claim and surviving spouse.')
				RETURN -1
			END IF
		END IF
		
		ls_message = 'Do you want to delete sub-ledger Payment ' +String(ll_payment_no)+ '?'
		ll_payment_array[1] = ll_payment_no

	ELSE
		// deleting 97/CMs
		lds_CM_payments_to_delete = Create U_DS
		
		IF is_claim_role_code = 'C' THEN
			IF ib_post92 = TRUE THEN
				lds_CM_payments_to_delete.DataObject = 'ds_post92_97cm_to_delete'
				lds_CM_payments_to_delete.SetTransObject(SQLCA)
				li_payment_rows = lds_CM_payments_to_delete.Retrieve(ll_claim_no,ll_recipient_no)
				
			ELSE
				lds_CM_payments_to_delete.DataObject = 'ds_pre93_97cm_to_delete'
				lds_CM_payments_to_delete.SetTransObject(SQLCA)
				li_payment_rows = lds_CM_payments_to_delete.Retrieve(ll_claim_no,ll_recipient_no)
			END IF
		ELSEIF is_claim_role_code = 'SS' THEN
			lds_CM_payments_to_delete.DataObject = 'ds_SS_97cm_to_delete'
			lds_CM_payments_to_delete.SetTransObject(SQLCA)
			li_payment_rows = lds_CM_payments_to_delete.Retrieve(ll_claim_no,ll_recipient_no)
		END IF
		IF li_payment_rows > 1 THEN
			ls_message = 'There are multiple Annuity Payments for this claim and recipient.' &
			            + '~r~nAll unprocessed Annuity Payments must be deleted at the same time.' &
							+ '~r~nDo you want to delete all the Annuity Payments for claim ' +String(ll_claim_no)+ '?'
			FOR li_counter = 1 TO li_payment_rows
				ll_payment_array[li_counter] = lds_CM_payments_to_delete.GetItemNumber(li_counter, 'payment_no')
			NEXT

		ELSE
			ls_message = 'Do you want to delete Annuity Payment ' +String(ll_payment_no)+ '?'
			ll_payment_array[1] = ll_payment_no
		END IF
	END IF

	IF MessageBox('Delete Annuity Payment(s)',ls_message, Question!, YesNo!, 2) = 2 THEN
		// no deletion
		RETURN
	END IF
	
	
	// DELETIONS
	SQLCA.nf_begin_transaction()
	
	// delete records in ANNUITY_PAYOUT_PRE_1993 table
	lds_pre93_payout_to_delete = Create U_DS
	lds_pre93_payout_to_delete.DataObject = 'ds_pre93_payout_to_delete'
	lds_pre93_payout_to_delete.SetTransObject(SQLCA)
	li_num_pre93_payout_rows = lds_pre93_payout_to_delete.Retrieve(ll_payment_array)
	SQLCA.nf_handle_error('w_maintain_manual_financial_trns', 'cb_delete', 'lds_pre93_payout_to_delete.retrieve')
	IF li_num_pre93_payout_rows > 0 THEN
		FOR li_counter = 1 TO li_num_pre93_payout_rows
			li_rtn = lds_pre93_payout_to_delete.DeleteRow(0)
			SQLCA.nf_handle_error('w_maintain_manual_financial_trns', 'cb_delete', 'lds_pre93_payout_to_delete.DeleteRow(0)')
		NEXT
		li_rtn = lds_pre93_payout_to_delete.Update()
		SQLCA.nf_handle_error('w_maintain_manual_financial_trns', 'cb_delete', 'lds_pre93_payout_to_delete.Update()')
	END IF
	
	// delete records in ANNUITY_SETASIDE_PRE_1993 table
	lds_pre93_setaside_to_delete = Create U_DS
	lds_pre93_setaside_to_delete.DataObject = 'ds_pre93_setaside_to_delete'
	lds_pre93_setaside_to_delete.SetTransObject(SQLCA)
	li_num_pre93_setaside_rows = lds_pre93_setaside_to_delete.Retrieve(ll_payment_array)
	SQLCA.nf_handle_error('w_maintain_manual_financial_trns', 'cb_delete', 'lds_pre93_setaside_to_delete.retrieve')
	IF li_num_pre93_setaside_rows > 0 THEN
		FOR li_counter = 1 TO li_num_pre93_setaside_rows
			li_rtn = lds_pre93_setaside_to_delete.DeleteRow(0)
			SQLCA.nf_handle_error('w_maintain_manual_financial_trns', 'cb_delete', 'lds_pre93_setaside_to_delete.DeleteRow(0)')
		NEXT
		li_rtn = lds_pre93_setaside_to_delete.Update()
		SQLCA.nf_handle_error('w_maintain_manual_financial_trns', 'cb_delete', 'lds_pre93_setaside_to_delete.Update()')
	END IF
	
	// delete records in PAYMENT table
	lds_payments_to_delete = Create U_DS
	lds_payments_to_delete.DataObject = 'ds_payments_to_delete'
	lds_payments_to_delete.SetTransObject(SQLCA)
	li_num_payment_rows = lds_payments_to_delete.Retrieve(ll_payment_array)
	SQLCA.nf_handle_error('w_maintain_manual_financial_trns', 'cb_delete', 'lds_payments_to_delete.retrieve')
	IF li_num_payment_rows > 0 THEN
		FOR li_counter = 1 TO li_num_payment_rows
			li_rtn = lds_payments_to_delete.DeleteRow(0)
			SQLCA.nf_handle_error('w_maintain_manual_financial_trns', 'cb_delete', 'lds_payments_to_delete.DeleteRow(0)')
		NEXT
		li_rtn = lds_payments_to_delete.Update()
		SQLCA.nf_handle_error('w_maintain_manual_financial_trns', 'cb_delete', 'lds_payments_to_delete.Update()')
	END IF
	
	// delete records in UNAPPLIED_CLAIM_TXN table
	lds_unapplied_claim_txn_to_delete = Create U_DS
	lds_unapplied_claim_txn_to_delete.DataObject = 'ds_unapplied_claim_txn_to_delete'
	lds_unapplied_claim_txn_to_delete.SetTransObject(SQLCA)
	li_num_UCT_rows = lds_unapplied_claim_txn_to_delete.Retrieve(ll_payment_array)
	SQLCA.nf_handle_error('w_maintain_manual_financial_trns', 'cb_delete', 'lds_unapplied_claim_txn_to_delete.retrieve')
	IF li_num_UCT_rows > 0 THEN
		FOR li_counter = 1 TO li_num_UCT_rows
			li_rtn = lds_unapplied_claim_txn_to_delete.DeleteRow(0)
			SQLCA.nf_handle_error('w_maintain_manual_financial_trns', 'cb_delete', 'lds_unapplied_claim_txn_to_delete.DeleteRow(0)')
		NEXT
		li_rtn = lds_unapplied_claim_txn_to_delete.Update()
		SQLCA.nf_handle_error('w_maintain_manual_financial_trns', 'cb_delete', 'lds_unapplied_claim_txn_to_delete.Update()')
	END IF
	
	// delete records in MANUAL_TXN_LINK table
	lds_manual_txn_link_to_delete = Create U_DS
	lds_manual_txn_link_to_delete.DataObject = 'ds_manual_txn_link_to_delete'
	lds_manual_txn_link_to_delete.SetTransObject(SQLCA)
	li_num_MTL_rows = lds_manual_txn_link_to_delete.Retrieve(ll_payment_array)
	SQLCA.nf_handle_error('w_maintain_manual_financial_trns', 'cb_delete', 'lds_manual_txn_link_to_delete.retrieve')
	IF li_num_MTL_rows > 0 THEN
		FOR li_counter = 1 TO li_num_MTL_rows
			li_rtn = lds_manual_txn_link_to_delete.DeleteRow(0)
			SQLCA.nf_handle_error('w_maintain_manual_financial_trns', 'cb_delete', 'lds_manual_txn_link_to_delete.DeleteRow(0)')
		NEXT
		li_rtn = lds_manual_txn_link_to_delete.Update()
		SQLCA.nf_handle_error('w_maintain_manual_financial_trns', 'cb_delete', 'lds_manual_txn_link_to_delete.Update()')
	END IF
	
	// delete records in COST_OF_CLAIMS_ALLOCATED table
	lds_COCA_to_delete = Create U_DS
	lds_COCA_to_delete.DataObject = 'ds_COCA_to_delete'
	lds_COCA_to_delete.SetTransObject(SQLCA)
	li_num_COCA_rows = lds_COCA_to_delete.Retrieve(ll_payment_array)
	SQLCA.nf_handle_error('w_maintain_manual_financial_trns', 'cb_delete', 'lds_COCA_to_delete.retrieve')
	IF li_num_COCA_rows > 0 THEN
		FOR li_counter = 1 TO li_num_COCA_rows
			li_rtn = lds_COCA_to_delete.DeleteRow(0)
			SQLCA.nf_handle_error('w_maintain_manual_financial_trns', 'cb_delete', 'lds_COCA_to_delete.DeleteRow(0)')
		NEXT
		li_rtn = lds_COCA_to_delete.Update()
		SQLCA.nf_handle_error('w_maintain_manual_financial_trns', 'cb_delete', 'lds_COCA_to_delete.Update()')
	END IF
	
	SQLCA.nf_commit_transaction()

	
	dw_unapplied_man_txns_list.Reset()
	ll_num_rows = dw_unapplied_man_txns_list.Retrieve()
	SQLCA.nf_handle_error("w_maintain_manual_financial_trns", "", "cb_delete - dw_unapplied_man_txns_list.Retrieve()")

	IF ll_num_rows = 0 THEN
		dw_manual_transaction_details.Reset()
		dw_manual_payment_details.Reset()
		dw_manual_cost_of_claims_allocated.Reset()
	ELSE
		ll_listrow = dw_unapplied_man_txns_list.GetRow()
		dw_unapplied_man_txns_list.SelectRow(0,false)
		dw_unapplied_man_txns_list.SelectRow(ll_listrow,true)
		
		//	Retrieve the details
		wf_retrieve_details(ll_listrow)
	END IF

END IF
end event

type cb_cancel from commandbutton within w_maintain_manual_financial_trns
integer x = 366
integer y = 2400
integer width = 274
integer height = 100
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
end type

event clicked;// Reset the details back to the way they were.
IF dw_unapplied_man_txns_list.GetRow() > 0 THEN
	wf_retrieve_details(dw_unapplied_man_txns_list.GetRow())
	cb_save.Enabled = False
END IF

end event

type cb_close from commandbutton within w_maintain_manual_financial_trns
integer x = 672
integer y = 2400
integer width = 274
integer height = 100
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;Close(Parent)
	
end event

type cb_save from commandbutton within w_maintain_manual_financial_trns
integer x = 59
integer y = 2400
integer width = 274
integer height = 100
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Save"
end type

event clicked;Long     ll_current_row, ll_payment_no, ll_txn_no, ll_num_rows, ll_cost_alloc_no, ll_cost_alloc_operation_no
Long     ll_coc_allocated_no, ll_claim_no, ll_coc_period, ll_cocperiod, ll_cocperiod2
Integer  li_rtn, li_trancount
String   ls_payment_adjustment_flag, ls_final_payment_flag, ls_payment_type, ls_cost_alloc_type_code
Decimal  ldc_new_amount, ldc_sum, ldc_deductions, ld_cost_amount
Datetime ldt_current

N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '003' refers to the Manual Financial Txn Maintenance module, '044' refers to the Payment Processing module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('003','044','save',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF
/******************************************************************************************/

// The COC Period must be the current cost of claims period that is open.

ll_cocperiod = dw_manual_transaction_details.GetItemNumber(1,"coc_period")

Select current_coc_period
Into    :ll_cocperiod2
From Coc_Control
Where previous_coc_closed_flag = 'N' 
Using SQLCA;

SQLCA.nf_handle_error("w_maintain_manual_financial_trns","cb_save","SELECT * FROM Coc_Control")

IF ll_cocperiod < ll_cocperiod2 THEN
	dw_manual_transaction_details.SetItem(1,"coc_period",ll_cocperiod2)
END IF

//	Validate the changes before updating.
IF wf_validation() < 0 THEN
	RETURN
END IF

// Get the current row and save to use later
ll_current_row = dw_unapplied_man_txns_list.GetRow()

IF dw_manual_payment_details.GetItemString(1, 'payment_type_code') <> dw_manual_payment_details.GetItemString(1, 'payment_type_code', Primary!, TRUE) THEN
	ls_payment_adjustment_flag = 'N'
	ls_final_payment_flag = 'N'
	ls_payment_type = dw_manual_payment_details.GetItemString(1, 'payment_type_code')
	
	dw_manual_payment_details.SetItem(1,"final_payment_flag",ls_final_payment_flag)
	dw_manual_payment_details.SetItem(1,"payment_adjustment_flag",ls_payment_adjustment_flag)
	dw_manual_payment_details.SetItem(1,"payment_type_code",ls_payment_type)
END IF

ldc_new_amount = dw_manual_transaction_details.GetItemDecimal(1, 'txn_amount')
IF dw_manual_transaction_details.GetItemDecimal(1, 'txn_amount', Primary!, TRUE) <> ldc_new_amount THEN
	IF dw_manual_transaction_details.GetItemString(1, 'txn_type_code', Primary!, TRUE) = '2' THEN
		MessageBox('Transfer Payment Error', 'You cannot alter the transaction amount for a transfer. If you have made an error in the amount, delete this transaction and add a new one.')
		Parent.cb_cancel.trigger event clicked()
		RETURN
	END IF
	ll_payment_no = dw_manual_payment_details.GetItemNumber(1, 'payment_no')
	ll_txn_no = dw_manual_transaction_details.GetItemNumber(1, 'txn_no')

	IF MessageBox('Update Payment?', 'You are about to change the amount for the following payment: ' + String(ll_payment_no) + '. Do you wish to continue?',Question!, YesNo!, 2 ) = 2 THEN
		RETURN
	END IF

	// update total_payment_amount in PAYMENT table
	SELECT sum(txn_amount)
	  INTO :ldc_sum
	  FROM UNAPPLIED_CLAIM_TXN
	 WHERE payment_no = :ll_payment_no
	   AND txn_no <> :ll_txn_no
	 USING SQLCA ;

	li_rtn = SQLCA.nf_handle_error("w_maintain_manual_financial_trns","","cb_save - SELECT SUM(txn_amount) FROM UNAPPLIED_CLAIM_TXN")

	ldc_deductions = dw_manual_payment_details.GetItemDecimal(1, 'total_deductions')
	IF NOT IsNull(ldc_sum) THEN
		ldc_sum = ldc_sum + ldc_new_amount //add in new amount instead of old
	ELSE
		ldc_sum = ldc_new_amount
	END IF

	dw_manual_payment_details.SetItem(1, 'total_award_amount', ldc_sum + ldc_deductions)
	dw_manual_payment_details.SetItem(1, 'total_payment_amount', ldc_sum)
END IF


SQLCA.nf_begin_transaction()

// PR15532 - RS - 2011-12-21 - removed the lines of code that were updating the 'authorized_by_code' and 'authorized_date'

// Check Cost of Claims Allocated Record
IF dw_manual_cost_of_claims_allocated.RowCount() > 0 THEN
	ll_cost_alloc_no = dw_manual_cost_of_claims_allocated.GetItemNumber(1, "cost_alloc_no")
	ll_cost_alloc_operation_no = dw_manual_cost_of_claims_allocated.GetItemNumber(1, "cost_alloc_operation_no")
	IF IsNull(ll_cost_alloc_no) = TRUE THEN ll_cost_alloc_no = 0
	IF IsNull(ll_cost_alloc_operation_no) = TRUE THEN ll_cost_alloc_operation_no = 0
	
	IF ll_cost_alloc_no = 0 AND ll_cost_alloc_operation_no = 0 THEN
		dw_manual_cost_of_claims_allocated.DeleteRow(1)
	ELSE
		// coc_allocated_no
		ll_coc_allocated_no = dw_manual_cost_of_claims_allocated.GetItemNumber(1, "coc_allocated_no")
		IF ll_coc_allocated_no = 0 OR IsNull(ll_coc_allocated_no) = TRUE THEN
			UPDATE Last_Coc_Allocated_No 
				SET last_coc_allocated_no = last_coc_allocated_no + 1 
			 USING SQLCA ;
			
			li_rtn = SQLCA.nf_handle_error("w_maintain_manual_financial_trns", "", "cb_save - UPDATE Last_Coc_Allocated_No SET last_coc_allocated_no = last_coc_allocated_no + 1 ")
			IF li_rtn < 0 THEN
				RETURN -1
			END IF
			
			SELECT last_coc_allocated_no 
			  INTO :ll_coc_allocated_no 
			  FROM Last_Coc_Allocated_No 
			 USING SQLCA ; 
			
			li_rtn = SQLCA.nf_handle_error("w_maintain_manual_financial_trns", "", "cb_save - SELECT last_coc_allocated_no FROM Last_Coc_Allocated_No")	
			IF li_rtn < 0 THEN
				RETURN -1
			END IF
			dw_manual_cost_of_claims_allocated.SetItem(1, "coc_allocated_no", ll_coc_allocated_no)
		END IF		

		// cost_alloc_type_code
		ls_cost_alloc_type_code = dw_manual_cost_of_claims_allocated.GetItemString(1, "cost_alloc_type_code")
		IF ls_cost_alloc_type_code = "" OR IsNull(ls_cost_alloc_type_code) = TRUE THEN
			SELECT employer_type_code 
			  INTO :ls_cost_alloc_type_code 
			  FROM EMPLOYER 
			 WHERE employer_no = :ll_cost_alloc_no ;

			li_rtn = SQLCA.nf_handle_error("w_maintain_manual_financial_trns", "", "cb_save - SELECT employer_type_code FROM EMPLOYER")
			IF li_rtn < 0 THEN
				RETURN -1
			END IF
			
			dw_manual_cost_of_claims_allocated.SetItem(1, "cost_alloc_type_code", ls_cost_alloc_type_code)
		END IF

		// claim_no
		ll_claim_no = dw_manual_cost_of_claims_allocated.GetItemNumber(1, "claim_no")
		IF ll_claim_no = 0 OR IsNull(ll_claim_no) = TRUE THEN
			ll_claim_no = dw_manual_transaction_details.GetItemNumber(1, "claim_no")
			dw_manual_cost_of_claims_allocated.SetItem(1, "claim_no", ll_claim_no)
		END IF		

		// payment_no
		ll_payment_no = dw_manual_cost_of_claims_allocated.GetItemNumber(1, "payment_no")
		IF ll_payment_no = 0 OR IsNull(ll_payment_no) = TRUE THEN
			ll_payment_no = dw_manual_transaction_details.GetItemNumber(1, "payment_no")
			dw_manual_cost_of_claims_allocated.SetItem(1, "payment_no", ll_payment_no)
		END IF		

		// txn_no
		ll_txn_no = dw_manual_cost_of_claims_allocated.GetItemNumber(1, "txn_no")
		IF ll_txn_no = 0 OR IsNull(ll_txn_no) = TRUE THEN
			ll_txn_no = dw_manual_transaction_details.GetItemNumber(1, "txn_no")
			dw_manual_cost_of_claims_allocated.SetItem(1, "txn_no", ll_txn_no)
		END IF		

		// cost_amount
		ld_cost_amount = dw_manual_transaction_details.GetItemDecimal(1, "txn_amount")
		dw_manual_cost_of_claims_allocated.SetItem(1, "cost_amount", ld_cost_amount)		

		// coc_period
		ll_coc_period = dw_manual_transaction_details.GetItemNumber(1, "coc_period")
	IF ll_coc_period <> dw_manual_transaction_details.GetItemNumber(1,"coc_period",primary!,true) THEN
		IF messagebox("Coc Period Change", "The Coc Posting Period has been modified from the original record. Continue?",question!,yesno!) <> 1 then RETURN
				
			dw_manual_cost_of_claims_allocated.SetItem(1, "coc_period", ll_coc_period)
		end if 
		IF dw_manual_cost_of_claims_allocated.AcceptText() = -1 THEN RETURN -1
	END IF
END IF

IF rb_auto_payment.checked THEN
	//if we're in auto payment, we do the updates to UNAPPLIED_CLAIM_TXN and CHEQUE_HEADER, CHEQUE_STUB, and ANNUITY tables
	IF wf_process_annuity_payout_cheque() < 0 THEN
		SQLCA.nf_transaction_count(li_trancount,1,'','','',FALSE)
		IF li_trancount > 0 THEN
			SQLCA.nf_rollback_transaction()
		END IF
		RETURN
	END IF
END IF

// Update and commit ...
li_rtn = dw_manual_payment_details.Update()
li_rtn = SQLCA.nf_handle_error("w_maintain_manual_financial_trns", "", "cb_save - dw_manual_payment_details.Update()")

li_rtn = dw_manual_transaction_details.Update()
li_rtn = SQLCA.nf_handle_error("w_maintain_manual_financial_trns","","cb_save - dw_manual_transaction_details.Update()")

li_rtn = dw_manual_cost_of_claims_allocated.Update()
li_rtn = SQLCA.nf_handle_error("w_maintain_manual_financial_trns","","cb_save - dw_manual_cost_of_claims_allocated.Update()")

li_rtn = dw_ready_to_process.Update()
li_rtn = SQLCA.nf_handle_error("w_maintain_manual_financial_trns","","cb_save - dw_ready_to_process.Update()")

SQLCA.nf_commit_transaction()


//	Refresh the data. Save the txn no in order to find it and make sure that that is the current row.
ll_num_rows = dw_unapplied_man_txns_list.Retrieve()
li_rtn = SQLCA.nf_handle_error("w_maintain_manual_financial_trns","","cb_save - dw_unapplied_man_txns_list.Retrieve()")

dw_unapplied_man_txns_list.SelectRow(0,false)
dw_unapplied_man_txns_list.SelectRow(ll_current_row,true)
dw_unapplied_man_txns_list.ScrollToRow(ll_current_row)

cb_cancel.TriggerEvent(Clicked!)

end event

type dw_manual_payment_details from u_dw_online within w_maintain_manual_financial_trns
integer x = 18
integer y = 1376
integer width = 2907
integer height = 204
integer taborder = 30
boolean bringtotop = true
string dataobject = "d_manual_payment_details"
boolean border = false
end type

event itemchanged;INTEGER       li_rtn, li_count
long          ll_claim_no, ll_recipient_no, ll_cost_alloc_no,ll_cost_alloc_operation_no, ll_coc_period
string        ls_claim_role_code, ls_payment_sub_type_code, ls_original_payment_sub_type_code
string        ls_cost_alloc_required_flag,ls_cost_alloc_type_code,ls_employer_name,ls_operation_name
Datetime      ldt_annuity_end_date
Date          ldt_end_date, ldt_paid_from_date, ldt_paid_to_date
dwItemStatus  l_status
BOOLEAN       lb_payment_sub_type_error

cb_save.Enabled = TRUE

IF dwo.name = 'payment_sub_type_code' THEN
	This.SetItem(1,"payment_sub_type_description",data)
END IF

ll_claim_no = this.getitemnumber(row,'claim_no')
ll_recipient_no = dw_manual_transaction_details.getitemnumber(1,'recipient_no')

Select claim_role_code 
into   :ls_claim_role_code
From   CLAIM_PARTICIPANT
Where  claim_no = :ll_claim_no
And    individual_no = :ll_recipient_no
Using SQLCA;

SQLCA.nf_handle_error('w_maintain_manaul_financial_trns','itemchanged','Select From CLAIM_PARTICIPANT')


//Check to see if this is for an injured worker or a surviving spouse
//If this is for a IW then the paid from and to can not span over 1993.
// 1993-01-01 is exclusive and can be included in both the Post 92 and Pre 93 dates. 

IF ls_claim_role_code = 'C' THEN
	//This is for the Injured Worker Only

	IF dwo.name = 'paid_from_date' THEN
		IF Date(this.getitemdatetime(1,'paid_from_date')) < 1993-01-01 OR (Date(data) <= 1993-01-01 and Date(this.getitemdatetime(1,'paid_to_date')) <= 1993-01-01) THEN
			//Pre93 rules apply
			ib_post92 = FALSE
			IF  Date(data) >= 1993-01-01 THEN
				MessageBox('Error','The paid from and paid to dates can not span 1993. Please re-enter the paid from date.',Exclamation!)
				Return 1
			END IF
		ELSE
			ib_post92 = TRUE		
			IF  Date(data) < 1993-01-01 AND Date(this.getitemdatetime(1,'paid_to_date')) > 1993-01-01 THEN
				MessageBox('Error','The paid from and paid to dates can not span 1993. Please re-enter the paid from date.',Exclamation!)
				Return 1
			END IF
		END IF
		
		// BR 3.100	An annuity sub-ledger payment must not have the period changed from a post-1992 period to a pre-1993 period, if the recipient is a claimant.
		IF Date(this.getitemdatetime(1,'paid_from_date', Primary!, TRUE)) > 1993-01-01 AND Date(data) < 1993-01-01 THEN
			MessageBox('Error','You cannot change a post 92 period payment to a pre 93 payment . Please re-enter the paid from date.',Exclamation!)
			Return 1
		END IF
	END IF
	
	IF dwo.name = 'paid_to_date' THEN
		IF Date(this.getitemdatetime(1,'paid_from_date')) < 1993-01-01 AND Date(data) > 1993-01-01 THEN
			MessageBox('Error','The paid from and paid to dates can not span 1993. Please re-enter the paid to date.',Exclamation!)
			Return 1
		END IF
		
		// BR 3.100	An annuity sub-ledger payment must not have the period changed from a post-1992 period to a pre-1993 period, if the recipient is a claimant.
		IF Date(this.getitemdatetime(1,'paid_to_date', Primary!, TRUE)) > 1993-01-01 AND Date(data) < 1993-01-01 THEN
			MessageBox('Error','You cannot change a post 92 period payment to a pre 93 payment . Please re-enter the paid to date.',Exclamation!)
			Return 1
		END IF
	END IF
END IF


ls_payment_sub_type_code = THIS.GetItemString(row,'payment_sub_type_code')

CHOOSE CASE dwo.name
		
	CASE 'paid_from_date'
		
		IF Date(data) < 1982-01-01 THEN
			MessageBox('Validation Error','There are no payment types that exist prior to 1982-01-01. Please enter a new date.',Exclamation!)
			Return 1
		END IF		
		
		ldt_paid_from_date = Date(data)
		ldt_paid_to_date = Date(THIS.GetItemDateTime(row,'paid_to_date'))
		
		wf_filter_payment_sub_type_code(ldt_paid_from_date,ldt_paid_to_date)
		
		IF IsNull(ls_payment_sub_type_code) OR ls_payment_sub_type_code = '' THEN
			// user has not chosen payment sub type yet
		ELSE
			// user has chosen payment sub type
			li_count = wf_validate_paid_date_range_pymt_type(ls_payment_sub_type_code,ldt_paid_from_date,ldt_paid_to_date)
			IF li_count = 0 THEN
				// no sub type for date range
				MessageBox('Annuity Adjustment Error','The chosen payment sub type is not valid for the paid from date.'&
				                                  + '~r~nPlease correct the paid from date or cancel the change.',Exclamation!)
				RETURN 1
			END IF
		END IF
		
	CASE 'paid_to_date'
		
		ldt_paid_from_date = Date(THIS.GetItemDateTime(row,'paid_from_date'))
		ldt_paid_to_date = Date(data)
				
		wf_filter_payment_sub_type_code(ldt_paid_from_date,ldt_paid_to_date)
		
		IF IsNull(ls_payment_sub_type_code) OR ls_payment_sub_type_code = '' THEN
			// user has not chosen payment sub type yet
		ELSE
			// user has chosen payment sub type
			li_count = wf_validate_paid_date_range_pymt_type(ls_payment_sub_type_code,ldt_paid_from_date,ldt_paid_to_date)
			IF li_count = 0 THEN
				// no sub type for date range
				MessageBox('Annuity Adjustment Error','The chosen payment sub type is not valid for the paid to date.'&
				                                  + '~r~nPlease correct the paid to date or cancel the change.',Exclamation!)
				RETURN 1
			END IF
		END IF

		l_status = dw_manual_payment_details.GetItemStatus( 1, 'paid_from_date', Primary!)
		IF l_status = NotModified! and Date(this.getitemdatetime(1,'paid_from_date')) >= 1993-01-01 THEN
			ib_post92 = TRUE
		END IF
		
		Select b.annuity_end_date
		Into :ldt_annuity_end_date
		From ANNUITY_ACCOUNT a, ANNUITY_ELIGIBILITY b, CLAIM_PARTICIPANT c
		Where a.annuity_account_no = b.annuity_account_no
		And  a.individual_no = c.individual_no
		And  a.claim_role_code = c.claim_role_code 
		And  c.claim_no = :ll_claim_no
		And  c.claim_role_code = :ls_claim_role_code
		And b.annuity_eligibility_status_code = 'A'
		Using SQLCA;
		
		SQLCA.nf_handle_error('w_add_manaul_financial_trns','cb_save.clicked','Select b.annuity_end_date From ANNUNITY_ACCOUNT a, ANNUITY_ELIGIBILITY b, CLAIM_PARTICIPANT c')
	
		IF IsNull(ldt_annuity_end_date)  OR Date(ldt_annuity_end_date) = 1900-01-01 THEN
			// There isn't an active eligibility so use the max Inactive enddate
			Select max(b.annuity_end_date)
			Into :ldt_annuity_end_date
			From ANNUITY_ACCOUNT a, ANNUITY_ELIGIBILITY b, CLAIM_PARTICIPANT c
			Where a.annuity_account_no = b.annuity_account_no
			And  a.individual_no = c.individual_no
			And  a.claim_role_code = c.claim_role_code 
			And  c.claim_no = :ll_claim_no
			And  c.claim_role_code = :ls_claim_role_code
			And b.annuity_eligibility_status_code = 'I'
			Using SQLCA;
			
			SQLCA.nf_handle_error('w_add_manaul_financial_trns','cb_save.clicked','Select b.annuity_end_date From ANNUNITY_ACCOUNT a, ANNUITY_ELIGIBILITY b, CLAIM_PARTICIPANT c')
		
		END IF
		
		ldt_end_date = RelativeDate(Date(ldt_annuity_end_date), 1)
		
		//The payment period to date must be less than or equal to the first day of the month following the annuity end date
		IF Date(data) > ldt_end_date THEN
			MessageBox('Error','The paid to date must be less than or equal to the first day of the month following the annuity end date of '+String(ldt_annuity_end_date,'YYYY-MM-DD')+'.',Exclamation!)
			Return 1
		END IF
		
	CASE 'payment_sub_type_code'
		ls_original_payment_sub_type_code = THIS.GetItemString(row,'payment_sub_type_code', PRIMARY!, TRUE)
	
	/* 3.40  An annuity sub-ledger payment of the following types must not have its payment sub-type changed.
					•	Annuity Payment (i.e. 97/CM)
					•	Annuity Overpayment Recovery (i.e .97/CO)
					•	Annuity Write-off (i.e. 97/CW) 
					•	Void Annuity Payment (i.e. 97/DM)
					•	Void Annuity Overpayment Recovery (i.e. 97/DO).
					•	Reverse Annuity Write-off  (i.e. 97/DW). */
		
		CHOOSE CASE ls_original_payment_sub_type_code
			CASE 'CM', 'CO', 'CW', 'DM', 'DO', 'DW'
				IF ls_original_payment_sub_type_code <> data THEN
					MessageBox('Error',"An Annuity sub-ledger payment of sub-type: 'CM', 'CO', 'CW', 'DM', 'DO', or 'DW' must not be changed to another sub-type." &
						+ "~r~n~nIf you need the payment sub type to be different, then delete this payment and add another one with the correct sub type.",Exclamation!)
						Return 1
				END IF
		END CHOOSE
	
	/*3.60	An annuity sub-ledger payment must not have its payment sub-type changed to any of the following annuity payment sub-types:
					•	Annuity Payment (i.e. 97/CM)
					•	Annuity Overpayment Recovery (i.e .97/CO)
					•	Annuity Write-off (i.e. 97/CW)
					•	Void Annuity Payment (i.e. 97/DM)
					•	Void Annuity Overpayment Recovery (i.e. 97/DO).
					•	Reverse Annuity Write-off  (i.e. 97/DW).*/
	
		CHOOSE CASE data
			CASE 'CM', 'CO', 'CW', 'DM', 'DO', 'DW'
				IF data <> ls_original_payment_sub_type_code THEN
					MessageBox('Error',"An Annuity sub-ledger payment must not have its payment sub-type changed to: 'CM', 'CO', 'CW', 'DM', 'DO', or 'DW'." &
						+ "~r~n~nIf you need the payment sub type to be different, then delete this payment and add another one with the correct sub type.",Exclamation!)
					Return 1	
				END IF
		END CHOOSE
		
		SELECT cost_alloc_required_flag
		INTO   :ls_cost_alloc_required_flag
		FROM   Payment_Sub_Type
		WHERE  payment_type_code = '97'
		AND    payment_sub_type_code = :data
		USING SQLCA;
		SQLCA.nf_handle_error('w_maintain_manual_financial_trns', 'dw_manual_payment_details', 'embedded SQL: SELECT cost_alloc_required_flag FROM Payment_Sub_Type...')
		
		IF ls_cost_alloc_required_flag = 'Y' THEN
			IF dw_manual_cost_of_claims_allocated.RowCount() = 0 THEN
				dw_manual_cost_of_claims_allocated.InsertRow(0)
				inv_manual_financial.nf_get_default_cost_allocation(ll_claim_no,ll_cost_alloc_no,ll_cost_alloc_operation_no,ls_cost_alloc_type_code)
				dw_manual_cost_of_claims_allocated.SetItem(1,'cost_alloc_no',ll_cost_alloc_no)
				dw_manual_cost_of_claims_allocated.SetItem(1,'cost_alloc_operation_no',ll_cost_alloc_operation_no)
				dw_manual_cost_of_claims_allocated.SetItem(1,'cost_alloc_type_code',ls_cost_alloc_type_code)
				
				inv_manual_financial.nf_display_cost_allocation_data(ll_cost_alloc_no,ll_cost_alloc_operation_no,ls_employer_name,ls_operation_name)
				
				dw_manual_cost_of_claims_allocated.SetItem(1,'cost_alloc_name',ls_employer_name)
				dw_manual_cost_of_claims_allocated.SetItem(1,'cost_alloc_group_name',ls_operation_name)
				
				ll_coc_period = dw_manual_transaction_details.GetItemNumber(1,'coc_period')
	 			dw_manual_cost_of_claims_allocated.SetItem(1,'coc_period',ll_coc_period)
			END IF
		ELSE
			IF dw_manual_cost_of_claims_allocated.RowCount() <> 0 THEN
				dw_manual_cost_of_claims_allocated.DeleteRow(1)
			END IF
		END IF
	
END CHOOSE

end event

event editchanged;cb_save.Enabled = True

end event

event dberror;SQLCA.SQLDBCode = sqldbcode
SQLCA.SQLErrText = sqlerrtext
RETURN 1

end event

type gb_2 from groupbox within w_maintain_manual_financial_trns
integer x = 5
integer y = 12
integer width = 3186
integer height = 192
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Annuity Transaction Type"
end type

type dw_manual_transaction_details from u_dw_online within w_maintain_manual_financial_trns
integer x = 18
integer y = 1604
integer width = 1431
integer height = 496
integer taborder = 20
boolean bringtotop = true
string dataobject = "d_manual_transaction_details"
boolean border = false
end type

event itemchanged;// If the cost allocation has changed , then call the function that will retrieve and display the name.
//	
//	If the recipient type code has changed, then refresh the list of recipients. (Only get the list for Claimants/Individuals)
//
String  ls_recipient_type_code, ls_recipient_sub_type_code, ls_data
String  ls_recipient_name, ls_address_line1, ls_address_line2, ls_city, ls_payment_type_code
String  ls_prov_state_code, ls_country_code, ls_postal_code, ls_country_desc, ls_payment_sub_type_code
Long   ll_claim_no, ll_return_code, ll_null_value, ll_recipient_no, ll_count, ll_rtn, ll_row, ll_cheque_no
Long   ll_min_cheque_no, ll_max_cheque_no
Integer li_rtn
DataWindowChild ldwc_claim_participants

cb_save.Enabled = TRUE
IF dw_manual_transaction_details.AcceptText() = -1 THEN RETURN 1

//ll_row = dw_manual_payment_details.GetSelectedRow(0)
ll_row = 1
ls_payment_sub_type_code = dw_manual_payment_details.getitemstring(ll_row,'payment_sub_type_code')

IF dwo.name = 'recipient_type_code' THEN
	// Clear out the claim participants dddw so that you wont see the wrong claim participants for the claim # just retrieved.
	ll_return_code = dw_manual_transaction_details.GetChild("recipient_no", ldwc_claim_participants)
	ldwc_claim_participants.SetTransObject(SQLCA)
	ldwc_claim_participants.Reset()	
	SetNull(ll_null_value)
	dw_manual_transaction_details.SetItem(1, "recipient_no", ll_null_value)

	IF dw_manual_transaction_details.GetItemString(1, "recipient_type_code") = "I" THEN
		//	Retrieve the drop down data window of claim participants, based on the claim number.
		// Note: ALL claimants are listed, not just the active ones, as you may need to transfer costs 
		// from a participant that was set up incorrectly.
		ll_claim_no = dw_manual_transaction_details.GetItemNumber(1, "claim_no")
		ll_return_code = ldwc_claim_participants.Retrieve(ll_claim_no)
		li_rtn = SQLCA.nf_handle_error("w_maintain_manual_financial_trns", "dw_manual_transaction_details", "itemchanged - ldwc_claim_participants.Retrieve(ll_claim_no)")
	END IF
ELSEIF dwo.name = 'recipient_no' THEN
	// Verify that the recipient number is valid for the recipient type.
	ls_recipient_type_code = dw_manual_transaction_details.GetItemString(1, "recipient_type_code")
	ll_recipient_no = dw_manual_transaction_details.GetItemNumber(1, "recipient_no")
	ll_claim_no = dw_manual_transaction_details.GetItemNumber(1, "claim_no")

	IF ls_recipient_type_code = 'I' THEN
		ls_recipient_sub_type_code = ""

		// look in CLAIM_PARTICIPANT to ensure that the individual # is related to claim.
		SELECT COUNT(individual_no)
		  INTO :ll_count
		  FROM CLAIM_PARTICIPANT
		 WHERE claim_no = :ll_claim_no 
			AND individual_no = :ll_recipient_no ;
				
		li_rtn = SQLCA.nf_handle_error('w_maintain_manual_financial_trns', 'dw_manual_transaction_details', 'Item Changed Event - Embedded SQL:Select from CLAIM_PARTICIPANT')
		IF li_rtn < 0 THEN
			RETURN -1
		END IF
		
		IF ll_count <> 1 THEN
			MessageBox("Recipient Validation Error!","Recipient number is not a valid individual for this claim!")
			RETURN -1
		END IF

		SELECT given_names + " " + last_name, address_line1, address_line2, city, 
				 prov_state_code, country_code, postal_code
		  INTO :ls_recipient_name, :ls_address_line1, :ls_address_line2, :ls_city, 
				 :ls_prov_state_code, :ls_country_code, :ls_postal_code 
		  FROM INDIVIDUAL 
		 WHERE individual_no = :ll_recipient_no ;
	
		li_rtn = SQLCA.nf_handle_error("w_maintain_manual_financial_trns", "", "itemchanged - SELECT given_names + ' ' + last_name FROM INDIVIDUAL")
	ELSE
		// look in PROVIDER to ensure that the PROVIDER is valid and to get the recipient code and sub_type_code.
		SELECT count(*) 
		  INTO :ll_count
		  FROM PROVIDER
		 WHERE provider_no = :ll_recipient_no 
			AND provider_type_code = :ls_recipient_type_code USING SQLCA;

		li_rtn = SQLCA.nf_handle_error('w_maintain_manual_financial_trns', 'dw_manual_transaction_details', 'item changed event - Embedded SQL:Select from PROVIDER') 
		IF li_rtn < 0 THEN
			RETURN -1
		END IF

		IF ll_count <> 1 THEN
			MessageBox("Recipient Validation Error!","Error validating Recipient on the Provider table!",Exclamation!)
			RETURN -1
		END IF
	
		SELECT provider_sub_type_code, name, address_line1, address_line2, city, 
		       prov_state_code, country_code, postal_code
		  INTO :ls_recipient_sub_type_code, :ls_recipient_name, :ls_address_line1, :ls_address_line2, :ls_city, 
				 :ls_prov_state_code, :ls_country_code, :ls_postal_code 
		  FROM PROVIDER
		 WHERE provider_no = :ll_recipient_no
			AND provider_type_code = :ls_recipient_type_code USING SQLCA;

		li_rtn = SQLCA.nf_handle_error('w_maintain_manual_financial_trns', 'dw_manual_transaction_details', 'itemchanged event - Embedded SQL:Select from PROVIDER') 
		IF li_rtn < 0 THEN
			RETURN -1
		END IF
		
		// If type is Other Payee or VocRehab Provider and address_line2 is blank then add individuals name to address_line1
		IF ls_recipient_type_code = 'O' OR ls_recipient_type_code = 'V'THEN
			IF ls_address_line2 = "" OR IsNull(ls_address_line2) = TRUE THEN
				ls_address_line2 = ls_address_line1

				SELECT 'RE: ' + I.given_names + ' ' + I.last_name 
				  INTO :ls_address_line1 
				  FROM CLAIM C, 
				  		 INDIVIDUAL I 
				 WHERE C.claim_no = :ll_claim_no 
				   AND C.individual_no = I.individual_no ; 
					
				li_rtn = SQLCA.nf_handle_error("w_maintain_manual_financial_trns", "dw_manual_transaction_details", "itemchanged event - SELECT 'RE: ' + I.given_names + ' ' + I.last_name FROM CLAIM C, INDIVIDUAL I")
				IF li_rtn < 0 THEN
					RETURN -1
				END IF
			END IF
		END IF
	END IF

	SELECT ISNULL(location_desc1, :ls_country_code) 
	  INTO :ls_country_desc 
	  FROM Location 
	 WHERE location_type_code = "C"  
		AND location_code = :ls_country_code ; 
	
	li_rtn = SQLCA.nf_handle_error("w_maintain_manual_financial_trns", "", "itemchanged - SELECT location_desc1 FROM Location")
	
	dw_manual_transaction_details.SetItem(1, "recipient_type_code", ls_recipient_type_code)
	dw_manual_transaction_details.SetItem(1, "recipient_sub_type_code", ls_recipient_sub_type_code)
	dw_manual_transaction_details.SetItem(1, "address_line1", ls_address_line1)
	dw_manual_transaction_details.SetItem(1, "address_line2", ls_address_line2)
	dw_manual_transaction_details.SetItem(1, "city", ls_city)
	dw_manual_transaction_details.SetItem(1, "prov_state_code", ls_prov_state_code)
	dw_manual_transaction_details.SetItem(1, "country", ls_country_desc)
	dw_manual_transaction_details.SetItem(1, "postal_code", ls_postal_code)
	
END IF

// A DM and DO must have a cheque no 
// A CM and CO may have a cheque_no of zero entered, for example, if the user makes a mistake and needs to reset the cheque_no back to zero
IF dwo.name = 'cheque_no' THEN
	IF (IsNull(data) OR data = '' ) AND (ls_payment_sub_type_code = 'CM' OR ls_payment_sub_type_code = 'DM' OR ls_payment_sub_type_code = 'CO') THEN
		MessageBox('Error','A valid Cheque no must be entered.',Exclamation!)
		Return 1
	END IF
	IF (ls_payment_sub_type_code = 'DM' OR ls_payment_sub_type_code = 'DO') AND  LONG(data) = 0 THEN
		MessageBox('Error','A valid Cheque no, greater than 0, must be entered for payment types of DM, and DO.',Exclamation!)
		Return 1
	END IF

	ll_cheque_no = Long(data)
	//The cheque no must fall between the range of cheque no's in the Handwritten_Cheque_No_Range table
	ls_payment_type_code = dw_manual_payment_details.GetItemString(1, "payment_type_code")
	ls_payment_sub_type_code = dw_manual_payment_details.GetItemString(1, "payment_sub_type_code")	
	IF ls_payment_type_code = '97' and (ls_payment_sub_type_code = 'CM' OR ls_payment_sub_type_code = 'CO') THEN
	    Select min_handwritten_cheque_no,max_handwritten_cheque_no
		Into :ll_min_cheque_no, :ll_max_cheque_no
		From Handwritten_Cheque_No_Range
		Using SQLCA;
		
		SQLCA.nf_handle_error('w_maintain_manual_financial_trns','dw_manual_transaction_details.itemchanged','Select from Handwritten_Cheque_No_Range')
		
//		IF (ll_cheque_no < ll_min_cheque_no) OR (ll_cheque_no > ll_max_cheque_no) THEN
//			MessageBox('Error','The Cheque No must be within the allowable range of handwritten cheque numbers.',exclamation!)
//			Return 1
//		END IF
	 END IF

END IF

IF dwo.name = 'txn_amount' THEN
	IF (ls_payment_sub_type_code = 'CM' OR ls_payment_sub_type_code = 'CO' OR ls_payment_sub_type_code = 'CW') and Dec(data) >= 0 THEN
		MessageBox('Error','A CM, CO, or CW payment must have a negative amount.',Exclamation!)
		Return 1
	END IF
	IF (ls_payment_sub_type_code = 'DM' OR ls_payment_sub_type_code = 'DO' OR ls_payment_sub_type_code = 'DW') and Dec(data) <= 0 THEN
		MessageBox('Error','A DM, DO, or DW payment must have a positive amount.',Exclamation!)
		Return 1
	END IF	
END IF

end event

event editchanged;cb_save.Enabled = TRUE
cb_cancel.Enabled = TRUE
end event

event dberror;SQLCA.SQLDBCode = sqldbcode
SQLCA.SQLErrText = sqlerrtext
RETURN 1

end event

type dw_cheque_details from u_dw_online within w_maintain_manual_financial_trns
integer x = 1445
integer y = 1600
integer width = 1746
integer height = 536
integer taborder = 30
string dataobject = "d_cheque_details_man_txn"
boolean border = false
end type

event constructor;call super::constructor;This.setTransObject(SQLCA)
end event

type gb_1 from groupbox within w_maintain_manual_financial_trns
integer x = 5
integer y = 1312
integer width = 3195
integer height = 1068
integer taborder = 11
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Txn Details"
end type

type dw_ready_to_process from datawindow within w_maintain_manual_financial_trns
integer x = 2487
integer y = 1380
integer width = 649
integer height = 84
integer taborder = 40
boolean bringtotop = true
string dataobject = "d_annuity_ready_to_process"
boolean border = false
boolean livescroll = true
end type

event itemchanged;
INT li_count
long ll_txn_unit_of_work_no

IF data = 'Y' THEN
	ll_txn_unit_of_work_no = dw_unapplied_man_txns_list.getItemNumber(dw_unapplied_man_txns_list.getRow(), 'txn_unit_of_work_no')
	
	SELECT Count(*)
	INTO   :li_count
    FROM   ANNUITY_PAYOUT_TXN_DETAIL a 
    JOIN    UNAPPLIED_CLAIM_TXN b on a.txn_no = b.txn_no
    JOIN    TXN_UNIT_OF_WORK    c on b.txn_unit_of_work_no = c.txn_unit_of_work_no
    WHERE a.payment_sub_type_code in ('CM', 'CO')
    AND     b.txn_unit_of_work_no =  :ll_txn_unit_of_work_no
    AND     b.cheque_no = 0 
	USING SQLCA;
	SQLCA.nf_handle_error('w_maintain_manual_financial_trns', 'dw_ready_to_proecess.itemChanged', 'Select Count from ANNUITY_PAYOUT_RECIPIENT')
	
	IF li_count > 0 THEN
		Messagebox("Data Error", "There is at least one Annuity Payment ('CM' or 'CO') for this unit of work that has not had a cheque number entered." &
		                                    + "~r~nYou must enter cheque numbers for all recipients in this unit of work before you can check the ready to process checkbox.", Information!)
				
		RETURN 2 //don't accept the change
	END IF
END IF

cb_save.enabled = true
cb_cancel.enabled = true
end event

type dw_unapplied_man_txns_list from u_dw_online within w_maintain_manual_financial_trns
integer x = 5
integer y = 220
integer width = 3191
integer height = 1080
integer taborder = 10
string dataobject = "d_unapplied_man_txns_list_manual"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;call super::rowfocuschanged;INTEGER  li_rtn
LONG     ll_listrow, ll_cost_alloc_no, ll_cost_alloc_operation_no
STRING   ls_employer_name, ls_operation_name, ls_filter
DATAWINDOWCHILD ldwc_child


// If no rows, then exit.
IF This.RowCount() = 0 THEN
	RETURN
END IF

// Initialize
ll_listrow = dw_unapplied_man_txns_list.GetRow()
dw_unapplied_man_txns_list.SelectRow(0,false)
dw_unapplied_man_txns_list.SelectRow(ll_listrow,true)

IF ll_listrow = 0 THEN
	dw_manual_transaction_details.Reset()
	dw_manual_payment_details.Reset()
END IF

//	Retrieve the details
wf_retrieve_details(ll_listrow)

dw_manual_payment_details.Enabled = TRUE
dw_manual_transaction_details.Enabled = TRUE

IF Date(dw_unapplied_man_txns_list.GetItemDateTime(currentrow,'payment_paid_from_date')) < 1993-01-01 THEN
	ib_post92 = FALSE
ELSE
	ib_post92 = TRUE
END IF

// re-apply restrictions to data entry, i.e., for Automatic records
IF dw_unapplied_man_txns_list.GetItemNumber(currentrow, 'txn_unit_of_work_no') > 0 THEN
	wf_enable_controls(FALSE)
ELSE
	wf_enable_controls(TRUE)
END IF

IF THIS.GetItemString(currentrow,'payment_sub_type_code') = 'CM' &
OR THIS.GetItemString(currentrow,'payment_sub_type_code') = 'CO' THEN
	// Allow editing of cheque number
	wf_protect_cheque_no(FALSE)
ELSE
	// Do not allow editing of cheque number
	wf_protect_cheque_no(TRUE)
END IF

// filter so that only claimant & surviving spouse will be displayed
dw_manual_transaction_details.GetChild('recipient_no', ldwc_child)
ls_filter = 'claim_role_code = "C" or claim_role_code = "SS"'
li_rtn = ldwc_child.SetFilter(ls_filter)
li_rtn = ldwc_child.Filter()


// set cost alloc names
IF dw_manual_cost_of_claims_allocated.GetRow() = 1 THEN
	ll_cost_alloc_no = dw_manual_cost_of_claims_allocated.GetItemNumber(1,'cost_alloc_no')
	ll_cost_alloc_operation_no = dw_manual_cost_of_claims_allocated.GetItemNumber(1,'cost_alloc_operation_no')
	
	inv_manual_financial.nf_display_cost_allocation_data(ll_cost_alloc_no,ll_cost_alloc_operation_no,ls_employer_name,ls_operation_name)
	
	dw_manual_cost_of_claims_allocated.SetItem(1,'cost_alloc_name',ls_employer_name)
	dw_manual_cost_of_claims_allocated.SetItem(1,'cost_alloc_group_name',ls_operation_name)
	
	dw_manual_cost_of_claims_allocated.SetItemStatus(1,0,Primary!,NotModified!)
END IF


end event

event rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup
//	
//	Create the menu -	Note that this only gives default options.  If 
//							you want additional options, you should override
//							the ancestor and visible the options you desire.
//
lm_popup = Create m_dw_online_rmb_popup
lm_popup.mf_set_datawindow(This)
lm_popup.m_options.m_sort.Visible = TRUE
lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))

Destroy lm_popup
end event

event rowfocuschanging;call super::rowfocuschanging;DWItemStatus ldwis_payment, ldwis_txn, ldwis_coca, ldwis_flag

dw_manual_payment_details.AcceptText()
dw_manual_transaction_details.AcceptText()
dw_manual_cost_of_claims_allocated.AcceptText()

ldwis_payment = dw_manual_payment_details.GetItemStatus(1, 0, Primary!)
ldwis_txn     = dw_manual_transaction_details.GetItemStatus(1, 0, Primary!)
ldwis_coca    = dw_manual_cost_of_claims_allocated.GetItemStatus(1, 0, Primary!)
ldwis_flag = dw_ready_to_process.GetItemStatus(1, 0, Primary!)

IF ldwis_payment <> NotModified! OR ldwis_txn <> NotModified! OR ldwis_coca <> NotModified! OR ldwis_flag <> NotModified! THEN
	MessageBox('Save or Cancel','Please save or cancel the current record before going to another record.',Exclamation!)
	RETURN 1
END IF
end event

