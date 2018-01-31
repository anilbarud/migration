$PBExportHeader$w_add_manual_financial_trns.srw
forward
global type w_add_manual_financial_trns from w_ancestor
end type
type cb_cancel from commandbutton within w_add_manual_financial_trns
end type
type dw_total_manual_trns from u_dw_online within w_add_manual_financial_trns
end type
type cb_clear from commandbutton within w_add_manual_financial_trns
end type
type cb_close from commandbutton within w_add_manual_financial_trns
end type
type dw_financial_transaction from u_dw_online within w_add_manual_financial_trns
end type
type cb_save from commandbutton within w_add_manual_financial_trns
end type
type tab_manual_transactions from tab within w_add_manual_financial_trns
end type
type tabpage_annuity_payments from userobject within tab_manual_transactions
end type
type dw_comp_manual_transactions_annuity from u_dw_online within tabpage_annuity_payments
end type
type cb_add_annuity from commandbutton within tabpage_annuity_payments
end type
type tabpage_annuity_payments from userobject within tab_manual_transactions
dw_comp_manual_transactions_annuity dw_comp_manual_transactions_annuity
cb_add_annuity cb_add_annuity
end type
type tab_manual_transactions from tab within w_add_manual_financial_trns
tabpage_annuity_payments tabpage_annuity_payments
end type
type dw_annuity_setaside_pre93 from u_datawindow within w_add_manual_financial_trns
end type
type dw_annuity_payout_pre93 from u_datawindow within w_add_manual_financial_trns
end type
type dw_manual_txn_link from u_dw_online within w_add_manual_financial_trns
end type
type dw_man_txn_unapplied_claim_txn from u_dw_online within w_add_manual_financial_trns
end type
type dw_man_txn_payment from u_dw_online within w_add_manual_financial_trns
end type
type dw_man_txn_cost_of_claims_allocated from u_dw_online within w_add_manual_financial_trns
end type
end forward

global type w_add_manual_financial_trns from w_ancestor
integer width = 2770
integer height = 3216
string title = "Manual Transactions"
string menuname = "m_cmwb_notools"
boolean maxbox = false
boolean resizable = false
windowtype windowtype = main!
long backcolor = 67108864
cb_cancel cb_cancel
dw_total_manual_trns dw_total_manual_trns
cb_clear cb_clear
cb_close cb_close
dw_financial_transaction dw_financial_transaction
cb_save cb_save
tab_manual_transactions tab_manual_transactions
dw_annuity_setaside_pre93 dw_annuity_setaside_pre93
dw_annuity_payout_pre93 dw_annuity_payout_pre93
dw_manual_txn_link dw_manual_txn_link
dw_man_txn_unapplied_claim_txn dw_man_txn_unapplied_claim_txn
dw_man_txn_payment dw_man_txn_payment
dw_man_txn_cost_of_claims_allocated dw_man_txn_cost_of_claims_allocated
end type
global w_add_manual_financial_trns w_add_manual_financial_trns

type variables
BOOLEAN	ib_negate_amount = FALSE, ib_accept_claim_no, ib_post92 = true

DECIMAL{2} idc_amount, &
		idc_paid_days_lost

STRING is_claim_role_code, &
		is_legislation_code, &
		is_previous_coc_closed_flag, &
		is_payment_type, &
		is_explanation, &
		is_employer_type_code, &
		is_admin_region_code, &
		is_receiving_salary_flag, &
		is_recipient_type_code, &
		is_recipient_sub_type_code, &
		is_update_wcb_stats, &
      is_payment_sub_type_code, &
      is_cost_alloc_type_code

LONG	il_previous_coc_period, &
		il_current_coc_period, &
		il_claim_no, &
		il_individual_no, &
		il_cost_alloc_no, &
		il_cost_alloc_operation_no, &
		il_coc_period, &
		il_recipient_no, &
		il_cheque_no

INTEGER	ii_comp_rows_found_annuity

DATETIME	idt_paid_from_date, &
		idt_paid_to_date, &
		idt_scheduled_processing_date, idt_scheduled_processing_datetime, &
                                idtm_server_datetime

DataWindowChild idwc_annuity_payment_sub_type_code

n_manual_financial inv_manual_financial
end variables

forward prototypes
public function long wf_validate_claim_no (long al_claim_no)
public subroutine wf_set_dw_item_status (ref datawindow adw_dw, integer ai_row)
public function long wf_get_link_no ()
public function long wf_get_txn_no (long al_number_required)
public subroutine wf_initialize_window ()
public function long wf_get_payment_no (long al_number_required)
public function long wf_get_coc_allocated_no ()
public function integer wf_update_numbers ()
public function integer wf_validate_comp_details_annuity ()
public function integer wf_validate_individual (long al_individual_no, long al_claim_no)
public function integer wf_setup_post92_txns ()
public function integer wf_check_subledger_ss ()
public function integer wf_populate_instance_variables ()
public subroutine wf_default_cost_allocation (long al_claim_no)
public function integer wf_setup_annuity_payments (integer ai_row)
public subroutine wf_create_man_txns (string as_stage, string as_type, integer ai_row, long al_cost_alloc_no, long al_cost_alloc_operation_no, string as_cost_alloc_type)
private function integer wf_setup_pre93_txns (ref boolean ab_pre1993_payout_payments)
end prototypes

public function long wf_validate_claim_no (long al_claim_no);Long    ll_count
Integer li_reply
String  ls_admin_region_code, ls_receiving_salary_flag

// This function validates that the Claim number exists in the Claim db. It returns the value of the count,
// which would be '1' if the claim was found, '0' if it wasn't found or -1 if the claim number is not numeric.
IF IsNull(al_claim_no) OR al_claim_no < 1 THEN
	RETURN -1
END IF

SELECT count(*)
INTO   :ll_count
FROM   CLAIM
WHERE  claim_no = :al_claim_no
USING SQLCA;
SQLCA.nf_handle_error('w_add_manual_financial_trns', 'wf_validate_claim_no', 'Embedded SQL: select from CLAIM')

// If the claim was not found, return
IF ll_count < 0 THEN
	RETURN -1
ELSEIF ll_count = 0 OR ll_count = 100 THEN
	RETURN 0
END IF


// Acquire the legislation code and admin_region_code for the claim
SELECT legislation_code, admin_region_code, receiving_salary_flag
INTO   :is_legislation_code, :ls_admin_region_code, :ls_receiving_salary_flag
FROM   CLAIM
WHERE  claim_no = :al_claim_no
USING SQLCA;
SQLCA.nf_handle_error('w_add_manual_financial_trns', 'wf_validate_claim_no', 'Embedded SQL: select legislation_code from CLAIM')


// If the claim does not have an admin_region_code, then the Cost of Claims transaction cannot be processed.
// Company policy is that a claim must have an admin_region_code to be processed on the Workbench. The region
// that requested the transaction(s) must be notified to set up the admin region.
IF IsNull(ls_admin_region_code) OR ls_admin_region_code = "" OR ls_admin_region_code = " " THEN
	MessageBox("Error","Unable to process claim. This claim does not have an admin region. Please notify Claims dept to set up the region!",Exclamation!)
	RETURN -1
END IF


//	Display warning message if Claim is receiving salary.
IF ls_receiving_salary_flag = 'Y' THEN
	li_reply = MessageBox("Warning!","Claim " + String(al_claim_no) + " is a receiving salary claim. Do you wish to proceed?",Question!,YesNo!)
	IF li_reply = 2 THEN 
		RETURN -1
	END IF
END IF

RETURN ll_count
end function

public subroutine wf_set_dw_item_status (ref datawindow adw_dw, integer ai_row);DWITEMSTATUS	ldwis_status

ldwis_status = adw_dw.GetItemStatus( ai_row , 0 , Primary! )

CHOOSE CASE ldwis_status
	CASE New!
		adw_dw.SetItemStatus(ai_row, 0, Primary!, NewModified!)
	CASE NotModified!
		adw_dw.SetItemStatus(ai_row, 0, Primary!, DataModified!)
	CASE ELSE
		
END CHOOSE
end subroutine

public function long wf_get_link_no ();/* This function gets the link number required for the Manual Transactions being created.
	NOTE: The table is updated before reading to handle locking issues.
*/

LONG	ll_error, ll_last_link_no

	UPDATE Last_Link_No SET last_link_no = last_link_no + 1 using SQLCA;
	ll_error = SQLCA.nf_handle_error("w_add_manual_financial_trns","","wf_get_link_no - Embedded SQL 'UPDATE Last_Link_No SET last_link_no = last_link_no + 1 using SQLCA'")	
	IF ll_error < 0 THEN
		RETURN -1
	END IF

	SELECT Last_Link_No.last_link_no INTO :ll_last_link_no FROM Last_Link_No using SQLCA;
	ll_error = SQLCA.nf_handle_error("w_add_manual_financial_trns","","wf_get_link_no - Embedded SQL 'SELECT Last_Link_No.last_link_no INTO :ll_link_no FROM Last_Link_No using SQLCA'")	
	IF ll_error < 0 THEN
		RETURN -1
	END IF

/* If everything is OK, return the link number to be used. 
*/
	RETURN (ll_last_link_no)
end function

public function long wf_get_txn_no (long al_number_required);/* This function gets the txn numbers required for the Care Payments being created.
	NOTE: The table is updated before reading to handle locking issues.
*/

LONG	ll_error, ll_last_txn_no

	UPDATE Last_Claim_Txn_No SET last_txn_no = last_txn_no + :al_number_required using SQLCA;
	ll_error = SQLCA.nf_handle_error("w_add_manual_financial_trns","","wf_get_txn_no: Embedded SQL 'UPDATE Last_Claim_Txn_No SET last_txn_no = last_txn_no + :al_number_required using SQLCA'")	
	IF ll_error < 0 THEN
		RETURN -1
	END IF

	SELECT Last_Claim_Txn_No.last_txn_no INTO :ll_last_txn_no FROM Last_Claim_Txn_No using SQLCA;
	ll_error = SQLCA.nf_handle_error("w_add_manaual_financial_trns","","wf_get_txn_no: Embedded SQL 'SELECT Last_Claim_Txn_No.last_payment_no INTO :ll_last_txn_no FROM Last_Claim_Txn_No using SQLCA'")	
	IF ll_error < 0 THEN
		RETURN -1
	END IF

/* If everything is OK, return the first txn number to be used. This is the current value of last_txn_no minus
	the number of payments(txn's) to create that was added to it in the UPDATE statement. I then add 1 to this value to get the 
	first number to use.
*/
	ll_last_txn_no = (ll_last_txn_no - al_number_required) + 1
	RETURN (ll_last_txn_no)
end function

public subroutine wf_initialize_window ();// wf_initialize_window
Date    ldt_today
Long    ll_current_period, ll_row, ll_num_rows, ll_find_row
Integer li_rtn, li_count
DataWindowChild ldwc_comp_payment_type, ldwc_ma_payment_type, ldwc_claim_role_code, ldwc_transaction_type
//DataWindowChild ldwc_annuity_payment_sub_type_code, ldwc_transaction_type

//	Reset the datawindows, as this function is called from places other that the initial open of the window.
tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.Reset()

dw_total_manual_trns.Reset()
dw_man_txn_payment.Reset()
dw_man_txn_unapplied_claim_txn.Reset()
dw_man_txn_cost_of_claims_allocated.Reset()
dw_manual_txn_link.Reset()
dw_financial_transaction.Reset()
dw_annuity_payout_pre93.Reset()
dw_annuity_setaside_pre93.Reset()

//	Set up the 'header' information for the manual transaction(s). This is considered header information
// because it is the same for every PAYMENT record generated from this window.
dw_financial_transaction.InsertRow(0)

ldt_today = Date(f_server_datetime())
ll_current_period = Long(String(Year(ldt_today),"0000") + String(Month(ldt_today),"00"))

SELECT Count(*)
INTO   :li_count
FROM   Coc_Control
WHERE current_coc_period = :ll_current_period
USING SQLCA ;
li_rtn = SQLCA.nf_handle_error('w_add_manual_financial_trns', '', 'wf_initialize_window - Embedded SQL:Select from Coc_Control')
IF  li_rtn < 0 THEN
	RETURN 
END IF

IF li_rtn = 100 THEN
	MessageBox("Missing Cost of Claims Control Record","Unable to locate a Coc_Control record with current_coc_period = " + String(ll_current_period) + ". Contact Systems!",StopSign!)
	CLOSE (This)
	RETURN
END IF

dw_financial_transaction.SetItem(1,"posting_period",ll_current_period)

//	Set up the dw that shows the grand total of all dw's.
dw_total_manual_trns.InsertRow(0)

// Set up the Annuity dw
tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.InsertRow(0)

tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.GetChild("payment_sub_type_code",idwc_annuity_payment_sub_type_code)
idwc_annuity_payment_sub_type_code.SetTransObject(SQLCA)
ll_row = idwc_annuity_payment_sub_type_code.Retrieve()
SQLCA.nf_handle_error("w_add_manual_financial_trns","","wf_initialize_window - idwc_annuity_payment_sub_type_code.Retrieve()")


//	Make sure the save button is enabled...
cb_save.Enabled = TRUE

end subroutine

public function long wf_get_payment_no (long al_number_required);// wf_get_payment_no - gets the payment numbers required for the Care Payments being created.
//                     NOTE: The table is updated before reading to handle locking issues.
//
Long    ll_last_payment_no
Integer li_rtn

UPDATE Last_Payment_No SET last_payment_no = last_payment_no + :al_number_required using SQLCA;
li_rtn = SQLCA.nf_handle_error("w_add_manual_financial_trns","","wf_get_payment_no - Embedded SQL 'UPDATE Last_Payment_No SET last_payment_no = last_payment_no + :al_number_required using SQLCA'")	
IF li_rtn < 0 THEN
	RETURN -1
END IF

SELECT Last_Payment_No.last_payment_no INTO :ll_last_payment_no FROM Last_Payment_No using SQLCA;
li_rtn = SQLCA.nf_handle_error("w_add_manual_financial_trns","","wf_get_payment_no - Embedded SQL 'SELECT Last_Payment_No.last_payment_no INTO :ll_last_payment_no FROM Last_Payment_No using SQLCA'")	
IF li_rtn < 0 THEN
	RETURN -1
END IF

// Return the first payment number to be used. This is the current value of last_payment_no minus the 
// number of payments to create that was added to it in the UPDATE statement. I then add 1 to it to get the first
// number to use.
ll_last_payment_no = ((ll_last_payment_no - al_number_required) + 1)

RETURN ll_last_payment_no

end function

public function long wf_get_coc_allocated_no ();// wf_get_coc_allocated_no - Gets the next number from the Last_Coc_Allocated_No table.
//                           NOTE: The table is updated before reading to handle locking issues.
//
Long    ll_last_coc_allocated_no
Integer li_rtn

UPDATE Last_Coc_Allocated_No 
   SET last_coc_allocated_no = last_coc_allocated_no + 1 
 USING SQLCA ;

li_rtn = SQLCA.nf_handle_error("w_add_manual_financial_trns", "", "wf_get_coc_allocated_no - Embedded SQL 'UPDATE Last_Coc_Allocated_No SET last_coc_allocated_no = last_coc_allocated_no + 1'")
IF li_rtn < 0 THEN
	RETURN -1
END IF

SELECT last_coc_allocated_no 
  INTO :ll_last_coc_allocated_no 
  FROM Last_Coc_Allocated_No 
 USING SQLCA ; 

li_rtn = SQLCA.nf_handle_error("w_add_manual_financial_trns", "", "wf_get_coc_allocated_no - Embedded SQL 'SELECT last_coc_allocated_no INTO :ll_last_coc_allocated_no FROM Last_Coc_Allocated_No'")	
IF li_rtn < 0 THEN
	RETURN -1
END IF

RETURN ll_last_coc_allocated_no

end function

public function integer wf_update_numbers ();// wf_update_numbers - controls updating the payment_no on MANUAL_TXN_LINK, UNAPPLIED_CLAIM_TXN
//                     and PAYMENT, the txn_no on UNAPPLIED_CLAIM_TXN and the link_no on MANUAL_TXN_LINK.
//
Long    ll_row_count, ll_counter, ll_txn_no, ll_link_no, ll_payment_no, ll_link_row
Long    ll_related_payment_no_row, ll_related_payment_no, ll_coc_allocated_no, ll_coca_counter
String  ls_payment_type_code, ls_payment_sub_type_code, ls_cost_alloc_required_flag
Integer li_rtn, li_new_row

ll_row_count = dw_man_txn_payment.RowCount()

ll_payment_no = wf_get_payment_no(ll_row_count)
IF ll_payment_no <= 0 THEN
	RETURN -1
END IF

ll_txn_no = wf_get_txn_no(ll_row_count)
IF ll_txn_no <= 0 THEN
	RETURN -1
END IF

ll_link_no = wf_get_link_no()
IF ll_link_no <= 0 THEN
	RETURN -1
END IF

ll_counter = 1
ll_coca_counter = 1
DO	WHILE ll_counter <= ll_row_count
	dw_man_txn_payment.SetItem(ll_counter, "payment_no", ll_payment_no)
	dw_man_txn_unapplied_claim_txn.SetItem(ll_counter, "payment_no", ll_payment_no)
	dw_man_txn_unapplied_claim_txn.SetItem(ll_counter, "txn_no", ll_txn_no)

	// See if COST_OF_CLAIMS_ALLOCATED row is required for payment
	ls_payment_type_code = dw_man_txn_payment.GetItemString(ll_counter, "payment_type_code")
	ls_payment_sub_type_code = dw_man_txn_payment.GetItemString(ll_counter, "payment_sub_type_code")

	SELECT cost_alloc_required_flag 
	  INTO :ls_cost_alloc_required_flag 
	  FROM Payment_Sub_Type 
	 WHERE payment_type_code = :ls_payment_type_code 
		AND payment_sub_type_code = :ls_payment_sub_type_code ;
	
	li_rtn = SQLCA.nf_handle_error("w_add_manual_financial_trns", "", "wf_update_numbers - SELECT cost_alloc_required_flag FROM Payment_Sub_Type")

	IF ls_cost_alloc_required_flag = "Y" THEN
		ll_coc_allocated_no = wf_get_coc_allocated_no()
		
		dw_man_txn_cost_of_claims_allocated.SetItem(ll_coca_counter, "coc_allocated_no", ll_coc_allocated_no)
		dw_man_txn_cost_of_claims_allocated.SetItem(ll_coca_counter, "payment_no", ll_payment_no)
		dw_man_txn_cost_of_claims_allocated.SetItem(ll_coca_counter, "txn_no", ll_txn_no)
		ll_coca_counter = ll_coca_counter + 1
	END IF

	ll_link_row = dw_manual_txn_link.InsertRow(0)
	dw_manual_txn_link.SetItem(ll_link_row,"link_no",ll_link_no)
	dw_manual_txn_link.SetItem(ll_link_row,"payment_no",ll_payment_no)

	ll_related_payment_no_row = dw_man_txn_payment.GetItemNumber(ll_counter,'related_payment')

	IF ll_related_payment_no_row <> 0 THEN  // this is the second part of a transfer
		ll_related_payment_no = dw_man_txn_payment.GetItemNumber(ll_related_payment_no_row,'payment_no')
	ELSE
		ll_related_payment_no = 0
	END IF

	dw_manual_txn_link.SetItem(ll_link_row, "related_payment_no", ll_related_payment_no)
	
	
	// P10273-2
	// create conversion records if users are entering pre93 manual payments for injured workers
	IF ib_post92 THEN
		// do not insert into conversion tables
	ELSE
		IF ls_payment_sub_type_code = 'CM' OR ls_payment_sub_type_code = 'CO' OR ls_payment_sub_type_code = 'CW' THEN
			li_new_row = dw_annuity_payout_pre93.Insertrow(0)
			IF li_new_row > 0 THEN
				dw_annuity_payout_pre93.SetItem(li_new_row, 'payout_payment_no', ll_payment_no)
				dw_annuity_payout_pre93.SetItem(li_new_row, 'claim_no', il_claim_no)
			ELSE
				MessageBox('Error','A new pre-1993 conversion payout record could not be created.', Exclamation!)
				RETURN -1
			END IF
		ELSE
			li_new_row = dw_annuity_setaside_pre93.Insertrow(0)
			IF li_new_row > 0 THEN
				dw_annuity_setaside_pre93.SetItem(li_new_row, 'setaside_payment_no', ll_payment_no)
				dw_annuity_setaside_pre93.SetItem(li_new_row, 'claim_no', il_claim_no)
			ELSE
				MessageBox('Error','A new pre-1993 conversion set-aside record could not be created.', Exclamation!)
				RETURN -1
			END IF
		END IF
	END IF
		
	ll_payment_no = ll_payment_no + 1
	ll_txn_no = ll_txn_no + 1
	ll_counter = ll_counter + 1
LOOP

RETURN 0
end function

public function integer wf_validate_comp_details_annuity ();// wf_validate_comp_details_annuity - validates the comp details.  It loops through and processes every row 
//                                    in the comp dw.
//
Decimal{2} ldc_amount
Integer    li_counter, li_reply, li_rtn, li_rowcount, li_found, li_count
String     ls_fromto_dates_flag, ls_payment_type, ls_payment_sub_type, ls_explanation
STRING     ls_matching_payment_sub_type, ls_find, ls_recipient_type_code
Long       ll_result, ll_cheque_no, ll_original_cheque_no, ll_recipient_no
Datetime   ldtm_accident_date,ldtm_null, ldtm_annuity_start_date, ldtm_annuity_end_date, ldt_paid_from,ldt_paid_to
DATE       ldt_paid_from_date, ldt_paid_to_date
U_DS       lds_annuity

SetNull(ldtm_null)

ll_recipient_no = dw_financial_transaction.getitemnumber(1,"individual_no")

FOR li_counter = 1 to  ii_comp_rows_found_annuity
	// Only do the validations if the amount is not zero.
	ldc_amount = tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.GetItemDecimal(li_counter,"amount")
	IF ldc_amount <> 0 THEN
		// Make sure that a payment type has been selected.
		ls_payment_type = '97'
		
		// Make sure that a payment_sub_type has been selected.
		IF IsNull(tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.GetItemString(li_counter,"payment_sub_type_code")) THEN
			MessageBox("Annuity Validation Error","The annuity payment type must be entered!",Exclamation!)
			tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.ScrollToRow(li_counter)
			tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.SetFocus()
			RETURN -1
		END IF

		SELECT fromto_dates_flag
		  INTO :ls_fromto_dates_flag
		  FROM Payment_Type
		 WHERE payment_type_code = :ls_payment_type
		 USING SQLCA;
	
		SQLCA.nf_handle_error('w_add_manual_financial_trns', '', 'wf_validate_comp_details_annuity - Embedded SQL: Select from Payment_Type')
	
		//	Validate that if the paid from and to dates are required that they are there. Also make sure that the
		// paid from date is not greater than the paid to date and that the paid to date is not in the future.
		//	IF ls_fromto_dates_flag = 'Y' THEN
		idt_paid_from_date = tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.GetItemDateTime(li_counter,"date_paid_from")
		idt_paid_to_date = tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.GetItemDateTime(li_counter,"date_paid_to")
		ls_payment_sub_type	= tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.GetItemString(li_counter,"payment_sub_type_code")
		
		IF (IsNull(idt_paid_from_date) OR IsNull(idt_paid_to_date)) AND ls_fromto_dates_flag = 'Y' THEN
			MessageBox("No Paid Dates Entered", "You have not entered from and to dates. The payment type '" + ls_payment_type + "' requires both from and to dates.", Exclamation!)
			tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.ScrollToRow(li_counter)
			tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.SetFocus()
			RETURN -1
		ELSE
			// When the "Pd From" date is entered/changed, it must be on or after the accident date of the claim.
			// When the "Pd From" and "Paid To" dates are entered, they must be valid dates.
			IF NOT ISNULL(idt_paid_from_date) THEN
				SELECT accident_date INTO :ldtm_accident_date FROM CLAIM  WHERE claim_no = :il_claim_no
				 USING SQLCA;
	
				SQLCA.nf_handle_error("w_add_manual_transactions","","wf_validate_comp_details_annuity - Embedded SQL: SELECT accident_date") 
					IF NOT ISNULL(ldtm_accident_date) THEN
						IF idt_paid_from_date < ldtm_accident_date THEN
							MessageBox("Compensation Validation Error","The paid from date must be on or after the accident date of the claim!",Exclamation!)
							tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.ScrollToRow(li_counter)
							tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.SetFocus()
							RETURN -1
						END IF
				 END IF 
			END IF 
	

	
			// If either the "Pd From" or "Paid To" date is entered or present (i.e. previously entered), the other date 	
			//	must also be entered or present.
			IF NOT ISNULL(idt_paid_from_date ) AND ISNULL(idt_paid_to_date ) THEN
				MessageBox("Compensation Validation Error","The paid to date(s) must be entered if there is a valid paid from date(s)",Exclamation!)
				tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.ScrollToRow(li_counter)
				tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.SetFocus()
				RETURN -1
			END IF
			
			IF NOT ISNULL(idt_paid_to_date) AND ISNULL(idt_paid_from_date ) THEN
				MessageBox("Compensation Validation Error","The paid from date(s) must be entered if there is a valid paid to date(s)",Exclamation!)
				tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.ScrollToRow(li_counter)
				tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.SetFocus()
				RETURN -1
			END IF
			
			// validate payment data
			li_rtn = inv_manual_financial.nf_validate_payment(il_individual_no,il_claim_no,idt_paid_from_date,idt_paid_to_date,ls_payment_sub_type,is_claim_role_code)
			IF li_rtn < 0 THEN
				tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.ScrollToRow(li_counter)
				tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.SetFocus()
				RETURN li_rtn
			END IF
	
			// when the "Pd From" date is entered/changed and is before January 1, 1993, a warning message is displayed    
			//	for the user to confirm the date.
			IF NOT ISNULL(idt_paid_from_date ) AND date(idt_paid_from_date) < date("1993-01-01") THEN
				li_reply = MessageBox("Compensation Validation Warning","You have entered a paid from date that is " + &
												"before '1993-01-01'. Do you want to continue with this date?",Question!,YesNo!,2)
				IF li_reply = 2 THEN 
					tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.ScrollToRow(li_counter)
					tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.SetFocus()
					RETURN -1			
				END IF
			END IF
	
			// When the "Pd To" date is entered/changed and is before January 1, 1993, a warning message is 
			// displayed for the user to confirm the date.
			IF NOT ISNULL(idt_paid_from_date ) AND Date(idt_paid_from_date) < Date('1993-01-01') AND date(idt_paid_to_date) <= date("1993-01-01") THEN
				li_reply = MessageBox("Compensation Validation Warning","You have entered a paid to date that is " +&
								"on or before '1993-01-01'. Do you want to continue with this date?",Question!,YesNo!,2)
				IF li_reply = 2 THEN 
					tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.ScrollToRow(li_counter)
					tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.SetFocus()
					RETURN -1		
				END IF
			END IF
		END IF
	
		//	Check to ensure that they are eligible for Annuity passed by ref
		ldt_paid_from = idt_paid_from_date
		ldt_paid_to = idt_paid_to_date
		IF inv_manual_financial.nf_retrieve_annuity_dates(il_individual_no,il_claim_no,is_claim_role_code,ldt_paid_from,ldt_paid_to) < 0 THEN
			tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.ScrollToRow(li_counter)
			tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.SetFocus()
			RETURN -1
		END IF
		
		IF ls_payment_sub_type = '' OR IsNull(ls_payment_sub_type) THEN
			MessageBox("Annuity Validation Warning",'You must select a payment sub type.' )
			tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.ScrollToRow(li_counter)
			tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.SetFocus()
			RETURN -1
		END IF
		
		IF (ls_payment_sub_type = 'CM' OR ls_payment_sub_type = 'CO' OR ls_payment_sub_type = 'CW') and ldc_amount > 0 then
			MessageBox("Annuity Validation Warning","An Annuity Payment must be a negative amount " + string(ldc_amount,'[currency]') )
			tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.ScrollToRow(li_counter)
			tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.SetFocus()
			RETURN -1
		END IF
	
		IF (ls_payment_sub_type = 'DM' OR ls_payment_sub_type = 'DO' OR ls_payment_sub_type = 'DW') and ldc_amount < 0 then
			MessageBox("Annuity Validation Warning","A Void Annuity Payment must be a positive amount " +string(ldc_amount,'[currency]')  )
			tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.ScrollToRow(li_counter)
			tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.SetFocus()
			RETURN -1
		END IF
		
		// validate transaction data
		ll_cheque_no = tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.GetItemNumber(li_counter,"cheque_no")
		ll_original_cheque_no = tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.GetItemNumber(li_counter,"cheque_no",PRIMARY!,TRUE)
		ls_explanation = tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.GetItemString(li_counter,'explanation')
		
		IF IsNull(ll_original_cheque_no) THEN ll_original_cheque_no = 0
		IF IsNull(ll_cheque_no) THEN ll_cheque_no = 0
		
       
		
		li_rtn = inv_manual_financial.nf_validate_transaction(ls_payment_sub_type,ls_explanation,ll_cheque_no, ll_original_cheque_no,ll_recipient_no)
		IF li_rtn < 0 THEN
			tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.ScrollToRow(li_counter)
			tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.SetFocus()
			RETURN -1
		END IF
		
		// For Void Annuity Payments (i.e., DM, DO , DW) don't allow a matching Annuity payment (i.e., CM, CO, CW) to be entered at the same time
		// Possible to have multiples, so loop thru all DM, DO, DW and find matching void transactions
		// Also run checks for BR's 2.170, 2.172, 2.174, 2.180, 2.182, 2.184
		IF ls_payment_sub_type = 'DM' OR ls_payment_sub_type = 'DO' OR ls_payment_sub_type = 'DW' THEN
			ldt_paid_from_date = DATE(idt_paid_from_date)
			ldt_paid_to_date   = DATE(idt_paid_to_date)
			
			//now check to see if a matching payment (CM, CO, CW) exists in the entry datawindow
			IF ls_payment_sub_type = 'DM' THEN ls_matching_payment_sub_type = 'CM'
			IF ls_payment_sub_type = 'DO' THEN ls_matching_payment_sub_type = 'CO'
			IF ls_payment_sub_type = 'DW' THEN ls_matching_payment_sub_type = 'CW'
			
			IF IsNull(ll_cheque_no) THEN
				ls_find =  'payment_sub_type_code = "' + ls_matching_payment_sub_type + '"'  &
						  +  ' AND amount = ' + STRING(ldc_amount * -1) &
						  +  ' AND String(Date(date_paid_from),"yyyy-mm-dd") = "' + STRING(Date(ldt_paid_from_date),"yyyy-mm-dd") + '"' &
						  +  ' AND String(Date(date_paid_to),"yyyy-mm-dd") = "' + STRING(Date(ldt_paid_to_date),"yyyy-mm-dd") + '"'
			ELSE
				ls_find =  'payment_sub_type_code = "' + ls_matching_payment_sub_type + '"'  &
						  +  ' AND amount = ' + STRING(ldc_amount * -1) &
						  +  ' AND cheque_no = ' + STRING(ll_cheque_no)  &
						  +  ' AND String(Date(date_paid_from),"yyyy-mm-dd") = "' + STRING(Date(ldt_paid_from_date),"yyyy-mm-dd") + '"' &
						  +  ' AND String(Date(date_paid_to),"yyyy-mm-dd") = "' + STRING(Date(ldt_paid_to_date),"yyyy-mm-dd") + '"'
			END IF
			
			li_found = tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.Find(ls_find,0,ii_comp_rows_found_annuity)
			IF li_found > 0 THEN
				// Don't allow a CM and DM to be entered together if they are exactly the same (but opposite sign on the amount).
				MessageBox('Error',"A Void Annuity Payment (i.e. 97/DM, DO, DW) must not be entered,if there is also" &
				                 + "~r~na matching Annuity Payment (i.e. 97/CM, CO, CW) entered at the same time.",Exclamation!)
				RETURN -1
			END IF
			
			
			IF IsNull(ll_cheque_no) THEN ll_cheque_no = 0
			IF IsNull(ll_original_cheque_no) THEN ll_original_cheque_no = 0
			
			// look for unapplied CMs
			// BR 2.180, 2.182, 2.184  - A Void Annuity Payment (i.e. 97/DM,DO,DW) must not be entered, if there is a matching scheduled Annuity Payment (i.e. 97/CM,'CO,CW). 
			li_count = inv_manual_financial.nf_find_unapplied_annuity_payments(ls_payment_sub_type,ldt_paid_from_date,ldt_paid_to_date, ll_cheque_no, ldc_amount, il_individual_no)
			IF li_count > 0 THEN
				RETURN -1
			END IF
			
			// now look for applied CMs
			// 2.170, 2.172, 2.174 	A Void Annuity Payment (i.e. 97/DM) must have all of the following:
			//	·	the same amount as the Annuity Payment (i.e. 97/CM) but of the opposite sign 
			//	·	the same cheque number as the Annuity Payment
			//	·	the same payment period as the Annuity Payment.
			//	·	the same recipient as the Annuity Payment.  
			li_count = inv_manual_financial.nf_find_applied_annuity_payments(ls_payment_sub_type,ldt_paid_from_date,ldt_paid_to_date,ll_cheque_no,ldc_amount,il_individual_no)
			IF li_count = 0 THEN
				RETURN -1
			END IF			
		END IF
			
	ELSEIF IsNull(ldc_amount) THEN
		// not entered
	ELSE
		//pr 2850 - txn amount = 0
		MessageBox("Annuity Validation","Cheque amount cannot be zero.")
		tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.ScrollToRow(li_counter)
		tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.SetFocus()
		RETURN -1
	END IF
NEXT

RETURN 1
end function

public function integer wf_validate_individual (long al_individual_no, long al_claim_no);// wf_validate_individual
//
Long ll_count

// This function validates that the individual number entered is a valid individual for the 
//	claim. It also retrieves the claim role code and stores in an instance variable to be used 
//	in the Dep Acct Pension tab page.
IF IsNull(al_individual_no) OR &
	al_individual_no < 1 THEN
	MessageBox("Invalid Individual Number","You must select an individual for the claim!",Exclamation!)
	RETURN -1
END IF

// Validate that the individual number is valid.
SELECT count(*)
  INTO :ll_count
  FROM INDIVIDUAL
 WHERE individual_no = :al_individual_no
 USING SQLCA;
SQLCA.nf_handle_error('w_add_manual_financial_trns', '', 'wf_validate_individual - Embedded SQL: select from INDIVIDUAL')

IF ll_count <> 1 THEN
	MessageBox("Invalid Individual Number","The individual is not a valid individual!",Exclamation!)
	RETURN -1
END IF

//	Validate that the individual is valid for the claim and get the claim role code.
SELECT claim_role_code
  INTO :is_claim_role_code
  FROM CLAIM_PARTICIPANT
 WHERE claim_no = :al_claim_no 
	AND individual_no = :al_individual_no
 USING SQLCA;
SQLCA.nf_handle_error('w_add_manual_financial_trns', '', 'wf_validate_individual - Embedded SQL: select from CLAIM PARTICIPANT')

CHOOSE CASE TRUE
	CASE IsNull(is_claim_role_code)
		MessageBox("Invalid Individual","The individual is not valid for the claim!",Exclamation!)
		RETURN -1
		
	CASE is_claim_role_code <= ' '
		MessageBox("Invalid Individual","The individual is not valid for the claim!",Exclamation!)
		RETURN -1
		
	CASE is_claim_role_code = 'DC'
		MessageBox("Invalid Individual","Dependant children must not be chosen for manual transactions!",Exclamation!)
		RETURN -1
		
	CASE is_claim_role_code = 'GU'
		MessageBox("Invalid Individual","Guardians must not be chosen for manual transactions!",Exclamation!)
		RETURN -1
		
	CASE is_claim_role_code = 'OD'
		MessageBox("Invalid Individual","Other dependants must not be chosen for manual transactions!",Exclamation!)
		RETURN -1
		
	CASE is_claim_role_code = 'TR'
		MessageBox("Invalid Individual","Trustees must not be chosen for manual transactions!",Exclamation!)
		RETURN -1

	CASE ELSE
		
END CHOOSE


RETURN ll_count
end function

public function integer wf_setup_post92_txns ();// This function ensures that all the txns that were adjusted are post 92 txns and checks them against the sub-ledger
INTEGER     li_row
DECIMAL{4}  ldec_entered_amount, ldec_sum_CM, ldec_sum_SL, ldec_subledger_balance, ldec_total_subledger_balance
BOOLEAN     lb_entered_CM = FALSE, lb_unapplied_CM_exists
STRING      ls_payment_sub_type_code

FOR li_row = 1 to ii_comp_rows_found_annuity
	ls_payment_sub_type_code = tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.getitemstring(li_row,'payment_sub_type_code')
	ldec_entered_amount = tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.getitemnumber(li_row,'amount')

	IF ls_payment_sub_type_code = 'CM' OR ls_payment_sub_type_code = 'CO' OR ls_payment_sub_type_code = 'CW' THEN
		ldec_sum_CM = ldec_sum_CM + ldec_entered_amount
		lb_entered_CM = TRUE  // Set cm to true when a credit memo is found

	ELSEIF ls_payment_sub_type_code <> 'DM'  THEN
		// not CM or DM, so entered sub-ledger txns
		ldec_sum_SL = ldec_sum_SL + ldec_entered_amount
	END IF
NEXT

//Get the subledger balance
inv_manual_financial.nf_get_post92_subledger_balance(il_claim_no,il_individual_no,ldec_subledger_balance)

IF IsNull(ldec_subledger_balance) THEN ldec_subledger_balance = 0

ldec_total_subledger_balance = ldec_subledger_balance + ldec_sum_SL

IF lb_entered_CM = TRUE THEN
	IF ldec_total_subledger_balance <> 0 THEN
		IF (ldec_sum_CM * -1) <> ldec_total_subledger_balance THEN
			MessageBox('Error','The Annuity Payment amount must equal the post-1992 annuity sub-ledger balance for the injured worker ('+String(ldec_total_subledger_balance,'$#,##0.00;[RED]($#,##0.00)')+').', Exclamation!)
			Return -1
		END IF
	ELSE
		MessageBox('Error','The current post-1992 annuity sub-ledger balance is zero for this injured worker, so an Annuity Payment cannot be saved.', Exclamation!)
		Return -1
	END IF
ELSE
	lb_unapplied_CM_exists = inv_manual_financial.nf_unapplied_CM_exists(il_claim_no,il_individual_no,'post92')
	IF ldec_subledger_balance = 0 AND lb_unapplied_CM_exists THEN
		MessageBox('Error','The current sub-ledger balance is $0, including an unprocessed Annuity Payment.' &
		              +'~r~nYou must delete the unprocessed Annuity Payment before adding a sub-ledger payment.', Exclamation!)
		Return -1
	ELSE
		// all 97/CMs processed, so you can add new sub-ledger payments
	END IF
END IF

Return 1
end function

public function integer wf_check_subledger_ss ();//The credit memo  amount must equal the annuity sub-ledger balance for the surviving spouse
INTEGER     li_row
DECIMAL{4}  ldec_entered_amount, ldec_sum_CM, ldec_sum_SL, ldec_subledger_balance, ldec_total_subledger_balance
BOOLEAN     lb_entered_CM = FALSE, lb_unapplied_CM_exists
STRING      ls_payment_sub_type_code


FOR li_row = 1 to ii_comp_rows_found_annuity
	ls_payment_sub_type_code = tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.getitemstring(li_row,'payment_sub_type_code')
	ldec_entered_amount = tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.getitemnumber(li_row,'amount')

	IF ls_payment_sub_type_code = 'CM' OR ls_payment_sub_type_code = 'CO' OR ls_payment_sub_type_code = 'CW' THEN
		ldec_sum_CM = ldec_sum_CM + ldec_entered_amount
		lb_entered_CM = TRUE  // Set cm to true when a credit memo is found

	ELSEIF ls_payment_sub_type_code <> 'DM' THEN
		// not CM or DM, so entered sub-ledger txns
		ldec_sum_SL = ldec_sum_SL + ldec_entered_amount
	END IF
NEXT


//Get the subledger balance
inv_manual_financial.nf_get_subledger_balance(il_claim_no,il_individual_no,ldec_subledger_balance)
IF IsNull(ldec_subledger_balance) THEN ldec_subledger_balance = 0

ldec_total_subledger_balance = ldec_subledger_balance + ldec_sum_SL

IF lb_entered_CM = TRUE THEN 
	IF ldec_total_subledger_balance <> 0 THEN
		IF (ldec_sum_CM * -1) <> ldec_total_subledger_balance THEN
			MessageBox('Error','The Annuity Payment amount must equal the annuity sub-ledger balance for the surviving spouse ('+String(ldec_subledger_balance,'$#,##0.00;[RED]($#,##0.00)')+').', Exclamation!)
			Return -1
		END IF
	ELSE
		MessageBox('Validation Error','The current annuity sub-ledger balance is zero for this surviving spouse, so an Annuity Payment cannot be saved.',Exclamation!)
		Return -1
	END IF
ELSE
	lb_unapplied_CM_exists = inv_manual_financial.nf_unapplied_CM_exists(il_claim_no,il_individual_no,'SS')
	IF ldec_subledger_balance = 0 AND lb_unapplied_CM_exists THEN
		MessageBox('Error','The current sub-ledger balance is $0, including an unprocessed Annuity Payment.' &
		              +'~r~nYou must delete the unprocessed Annuity Payment before adding a sub-ledger payment.', Exclamation!)
		Return -1
	END IF
END IF

Return 1
end function

public function integer wf_populate_instance_variables ();// wf_retrieve_from_info - sets up the values using the 'from' side of the header information.
//
il_claim_no = dw_financial_transaction.GetItemNumber(1,"claim_no")
il_individual_no = dw_financial_transaction.GetItemNumber(1,"individual_no")
il_cost_alloc_no = dw_financial_transaction.GetItemNumber(1,"cost_alloc_no")
IF IsNull(il_cost_alloc_no) THEN il_cost_alloc_no = 0
il_cost_alloc_operation_no = dw_financial_transaction.GetItemNumber(1,"cost_alloc_operation_no")
IF IsNull(il_cost_alloc_operation_no) THEN il_cost_alloc_operation_no = 0

// Check to see if there is a value for the claim number and cost alloc number. If not, then don't try to read
// the database to get values. Return 1, as the calling program does validations to ensure that there are values.
IF IsNull(il_claim_no) OR IsNull(il_cost_alloc_no) THEN
	is_admin_region_code = ''
	is_receiving_salary_flag = ''
	is_employer_type_code = ''
	RETURN 1
END IF

SELECT admin_region_code, receiving_salary_flag
INTO   :is_admin_region_code, :is_receiving_salary_flag
FROM   CLAIM
WHERE  claim_no = :il_claim_no 
USING SQLCA ;
SQLCA.nf_handle_error('w_add_manual_financial_trns', 'wf_retrieve_from_info', 'Embedded SQL:Select from CLAIM')

SELECT employer_type_code
INTO   :is_employer_type_code
FROM   EMPLOYER
WHERE  employer_no = :il_cost_alloc_no 
USING SQLCA;
SQLCA.nf_handle_error('w_add_manual_financial_trns', 'wf_retrieve_from_info', 'Embedded SQL:Select from EMPLOYER')

RETURN 1

end function

public subroutine wf_default_cost_allocation (long al_claim_no);//when user enters claim number, default all the cost alloc data.
INTEGER   li_rtn
STRING    ls_employer_name,ls_operation_name


li_rtn = inv_manual_financial.nf_get_default_cost_allocation(al_claim_no,il_cost_alloc_no,il_cost_alloc_operation_no,is_cost_alloc_type_code)

dw_financial_transaction.SetItem(1,'cost_alloc_no',il_cost_alloc_no)
dw_financial_transaction.SetItem(1,'cost_alloc_operation_no',il_cost_alloc_operation_no)

IF li_rtn = 0 THEN
	inv_manual_financial.nf_display_cost_allocation_data(il_cost_alloc_no,il_cost_alloc_operation_no,ls_employer_name,ls_operation_name)
	dw_financial_transaction.SetItem(1,'employer_name',ls_employer_name)
	dw_financial_transaction.SetItem(1,'operation_name',ls_operation_name)
END IF

end subroutine

public function integer wf_setup_annuity_payments (integer ai_row);/* This function sets up the data required for each line of Comp Detail.
*/

	is_payment_type = '97'
	is_payment_sub_type_code = tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.GetItemString(ai_row,"payment_sub_type_code")
	is_explanation       = tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.GetItemString(ai_row,"explanation")
	is_update_wcb_stats  = tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.GetItemString(ai_row,"update_wcb_stats")
	idc_amount           = tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.GetItemDecimal(ai_row,"amount")

	il_recipient_no            = il_individual_no
	is_recipient_type_code     = 'I'
	is_recipient_sub_type_code = ''
	
	
		
/* If an amount has been entered, make sure that the other required fields have a value. If the amount is null,
	then we ignore that detail line (the whole purpose of this is to make $'s balance, so if no $'s are entered,
	there is no change in the Cost of Claims.
*/
	IF IsNull(idc_amount) THEN
		RETURN 0
	END IF
	
/*	Set up the paid from and to dates. If there are multiple txn's to be created at once, all the from payments are 
	created, then all the to payments are created, so the paid from and to dates might not be correct.
*/
	idt_paid_from_date = tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.GetItemDateTime(ai_row,"date_paid_from")
	idt_paid_to_date   = tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.GetItemDateTime(ai_row,"date_paid_to")
   idc_paid_days_lost = tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.GetItemDecimal(ai_row,"days_lost")
	IF IsNull(idc_paid_days_lost) THEN
		idc_paid_days_lost = 0
	END IF

	/*	Set up the Cheque Information (this is really only applicable for 'Refunds/Canceled Cheques')*/
	
	il_cheque_no = tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.GetItemNumber(ai_row,"cheque_no")

	if isnull(il_cheque_no) then
		il_cheque_no = 0
	end if
	
	RETURN 1
	
	


/**********************************************************
Revision History:
Developer		Doug Filmore
Date				1999.02.25
Comment			PR 613

					A change was made because the cheque_no where no being saved for Annuity Transactions.


**********************************************************/


end function

public subroutine wf_create_man_txns (string as_stage, string as_type, integer ai_row, long al_cost_alloc_no, long al_cost_alloc_operation_no, string as_cost_alloc_type);// wf_create_man_txns - sets up the UNAPPLIED_CLAIM_TXN, PAYMENT and MANUAL_TXN_LINK records required to 
//                      create the manual transaction(s).
//
Integer li_rtn
Long    ll_rows, ll_found_row, ll_row
String  ls_final_payment_flag, ls_payment_adjustment_flag , ls_find, ls_cost_alloc_oper_no
String  ls_recipient_name, ls_address_line1, ls_address_line2, ls_city , ls_country_desc 
String  ls_prov_state_code, ls_country_code, ls_postal_code, ls_cost_alloc_required_flag, ls_tax_flag
Decimal{2} ld_tax_rate
Datetime ldt_cheque_deposit_date
n_tax_functions lnv_tax_functions

IF IsNull(is_explanation) = TRUE THEN
	is_explanation = ""
END IF

// Get the Recipient Name
IF is_recipient_type_code = "I" THEN
	SELECT given_names + " " + last_name, address_line1, address_line2, city, 
	       prov_state_code, country_code, postal_code
	  INTO :ls_recipient_name, :ls_address_line1, :ls_address_line2, :ls_city, 
	       :ls_prov_state_code, :ls_country_code, :ls_postal_code 
	  FROM INDIVIDUAL 
	 WHERE individual_no = :il_recipient_no ;

	li_rtn = SQLCA.nf_handle_error("w_add_manual_financial_trns", "", "wf_create_man_txns - SELECT given_names + ' ' + last_name FROM INDIVIDUAL")
ELSE 
	SELECT name, address_line1, address_line2, city, prov_state_code, country_code, postal_code
	  INTO :ls_recipient_name, :ls_address_line1, :ls_address_line2, :ls_city, 
	       :ls_prov_state_code, :ls_country_code, :ls_postal_code 
	  FROM PROVIDER 
	 WHERE provider_no = :il_recipient_no 
	   AND provider_type_code = :is_recipient_type_code ;

	li_rtn = SQLCA.nf_handle_error("w_add_manual_financial_trns", "", "wf_create_man_txns - SELECT name FROM PROVIDER")
END IF

SELECT ISNULL(location_desc1, :ls_country_code) 
  INTO :ls_country_desc 
  FROM Location 
 WHERE location_type_code = "C"  
   AND location_code = :ls_country_code ; 

li_rtn = SQLCA.nf_handle_error("w_add_manual_financial_trns", "", "wf_create_man_txns - SELECT location_desc1 FROM Location")

// Make sure cheque_no is not Null
IF IsNull(il_cheque_no) = TRUE OR il_cheque_no = 0 THEN
	il_cheque_no = 0
	SetNull(ldt_cheque_deposit_date)
ELSE
	ldt_cheque_deposit_date = Datetime(Date(f_server_datetime()))
END IF

ll_row = dw_man_txn_unapplied_claim_txn.InsertRow(0)
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"claim_no",il_claim_no)
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"txn_type_code",'8')
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"batch_no",0)
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"recipient_no",il_recipient_no)
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"recipient_type_code",is_recipient_type_code)
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"recipient_sub_type_code",is_recipient_sub_type_code)
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"coc_period",il_coc_period)
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"manual_cheque_req_no",0)
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"cheque_no",il_cheque_no)
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"cheque_deposit_date",ldt_cheque_deposit_date)
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"payment_method_code",'I')
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"txn_amount",idc_amount)
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"admin_region_code",is_admin_region_code)
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"scheduled_processing_date",idt_scheduled_processing_date)
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"explanation",is_explanation)
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"related_txn_no",0)
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"recipient_name", ls_recipient_name)
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"address_line1", ls_address_line1)
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"address_line2", ls_address_line2)
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"city", ls_city)
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"prov_state_code", ls_prov_state_code)
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"country", ls_country_desc)
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"postal_code", ls_postal_code)
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"use_default_address_flag",'Y')
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"cheque_print_group_code",' ')
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"txn_sub_type_code",'')
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"tax_amount",0)
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"txn_unit_of_work_no",0)
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"maintain_allowed_flag","N")
dw_man_txn_unapplied_claim_txn.SetItem(ll_row,"direct_deposit_xmit_no",0)

// See if COST_OF_CLAIMS_ALLOCATED row needs to be created.  coc_allocated_no, payment_no, txn_no are set in wf_update_numbers()
SELECT cost_alloc_required_flag, tax_flag
  INTO :ls_cost_alloc_required_flag, :ls_tax_flag
  FROM Payment_Sub_Type 
 WHERE payment_type_code = :is_payment_type 
   AND payment_sub_type_code = :is_payment_sub_type_code ;

li_rtn = SQLCA.nf_handle_error("w_add_manual_financial_trns", "", "wf_create_man_txns - SELECT cost_alloc_required_flag FROM Payment_Sub_Type")

IF ls_cost_alloc_required_flag = "Y" THEN
	ll_row = dw_man_txn_cost_of_claims_allocated.InsertRow(0)
	dw_man_txn_cost_of_claims_allocated.SetItem(ll_row, "claim_no", il_claim_no)
	dw_man_txn_cost_of_claims_allocated.SetItem(ll_row, "cost_alloc_no", al_cost_alloc_no)
	dw_man_txn_cost_of_claims_allocated.SetItem(ll_row, "cost_alloc_operation_no", al_cost_alloc_operation_no)
	dw_man_txn_cost_of_claims_allocated.SetItem(ll_row, "cost_alloc_type_code", as_cost_alloc_type)
	dw_man_txn_cost_of_claims_allocated.SetItem(ll_row, "cost_amount", idc_amount) 
	dw_man_txn_cost_of_claims_allocated.SetItem(ll_row, "coc_period", il_coc_period)
END IF

// Set the tax rate
IF ls_tax_flag = "N" THEN
	ld_tax_rate = 0.00
ELSE
	lnv_tax_functions = CREATE n_tax_functions
	
	li_rtn = lnv_tax_functions.nf_get_valid_tax_rate(Date(idt_paid_from_date), Date(idt_paid_to_date), ld_tax_rate)
	IF li_rtn = -1 THEN
		ld_tax_rate = 0.00
	END IF
	
	DESTROY lnv_tax_functions
END IF

ls_payment_adjustment_flag = 'N'
ls_final_payment_flag = 'N'

// The zeroed_flag is set to 'Y' to prevent these transactions from being included in the list of
// payments for the cancel/adjust payments module and from being included in several counts that
// only want to include payments (exclude cancelled and fully adjusted payments)
ll_row = dw_man_txn_payment.InsertRow(0)
dw_man_txn_payment.SetItem(ll_row,"claim_no",il_claim_no)
dw_man_txn_payment.SetItem(ll_row,"opening_no",0)
dw_man_txn_payment.SetItem(ll_row,"benefit_calculation_no",0)
dw_man_txn_payment.SetItem(ll_row,"payment_type_code",is_payment_type)
dw_man_txn_payment.SetItem(ll_row,"final_payment_flag",ls_final_payment_flag)
dw_man_txn_payment.SetItem(ll_row,"paid_days_lost",idc_paid_days_lost)
dw_man_txn_payment.SetItem(ll_row,"paid_hours_lost",0)
dw_man_txn_payment.SetItem(ll_row,"paid_from_date",idt_paid_from_date)
dw_man_txn_payment.SetItem(ll_row,"paid_to_date",idt_paid_to_date)
dw_man_txn_payment.SetItem(ll_row,"total_award_amount",idc_amount)	
dw_man_txn_payment.SetItem(ll_row,"total_deductions",0)
//dw_man_txn_payment.SetItem(ll_row,"total_payment_amount",idc_amount)
dw_man_txn_payment.SetItem(ll_row,"tax_amount",0)
dw_man_txn_payment.SetItem(ll_row,"adjustment_days_lost",0)
dw_man_txn_payment.SetItem(ll_row,"adjustment_hours_lost",0)
dw_man_txn_payment.SetItem(ll_row,"adjustment_payment_amount",0)
dw_man_txn_payment.SetItem(ll_row,"payment_sub_type_code",is_payment_sub_type_code)
dw_man_txn_payment.SetItem(ll_row,"processed_date",'')
dw_man_txn_payment.SetItem(ll_row,"loe_explanation",'')
dw_man_txn_payment.SetItem(ll_row,"payment_adjustment_flag",ls_payment_adjustment_flag)
dw_man_txn_payment.SetItem(ll_row,"authorized_by_code",vgst_user_profile.user_id)
dw_man_txn_payment.SetItem(ll_row,"authorized_date",idt_scheduled_processing_datetime)
dw_man_txn_payment.SetItem(ll_row,"submitted_amount",0.00)
dw_man_txn_payment.SetItem(ll_row,"award_no",0)
dw_man_txn_payment.SetItem(ll_row,"paid_quantity",0)
dw_man_txn_payment.SetItem(ll_row,"authorization_no",0)
dw_man_txn_payment.SetItem(ll_row,"adjustment_tax_amount",0.00)
dw_man_txn_payment.SetItem(ll_row,"tax_rate", ld_tax_rate)

// PR 2850 - This ensures that the related_payment_no column
// in MANUAL_TXN_LINK gets populated properly for transfers (txn_type_code = '2')
IF as_stage = '1' THEN
	dw_man_txn_payment.SetItem(ll_row,"selected_txn_type",as_type)
	dw_man_txn_payment.SetItem(ll_row,"row_in_loop",ai_row)
ELSEIF as_stage = '2' THEN
	ll_rows=dw_man_txn_payment.RowCount()
	ls_find = 'row_in_loop = ' + String(ai_row) + ' and selected_txn_type = "' + as_type + '"'
	ll_found_row = dw_man_txn_payment.Find(ls_find,0,ll_rows)
	dw_man_txn_payment.SetItem(ll_row,"related_payment", ll_found_row) // use row number temporarily
	dw_man_txn_payment.SetItem(ll_row,"selected_txn_type",as_type)
	dw_man_txn_payment.SetItem(ll_row,"row_in_loop",ai_row)
ELSEIF as_stage = '3' THEN
	// this is a one-sided transfer: find the credit, 
	// all the debits chould have related_payment set to credit's payment_no 
	IF idc_amount < 0 THEN
		dw_man_txn_payment.SetItem(ll_row,"related_payment", 0)
	ELSE
		ll_rows=dw_man_txn_payment.RowCount()
		ls_find = 'total_payment_amount < 0'
		ll_found_row = dw_man_txn_payment.Find(ls_find,0,ll_rows)
		dw_man_txn_payment.SetItem(ll_row,"related_payment", ll_found_row) // use row number temporarily
	END IF
END IF
end subroutine

private function integer wf_setup_pre93_txns (ref boolean ab_pre1993_payout_payments);// This function ensures that all the txns that were adjusted are pre 93 txns and checks them against the Pre93 XFER tables and sub-ledger

INTEGER     li_row
DECIMAL{4}  ldec_subledger_balance, ldec_total_subledger_balance, ldec_sum_SL, ldec_sum_CM , ldec_total_scheduled_payments
BOOLEAN     lb_entered_CM = false, lb_unapplied_CM_exists
STRING      ls_payment_sub_type_code 
LONG 		ll_claim_no

FOR li_row = 1 to ii_comp_rows_found_annuity
	ls_payment_sub_type_code = tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.getitemstring(li_row,'payment_sub_type_code')
	IF ls_payment_sub_type_code = 'CM' OR ls_payment_sub_type_code = 'CO' OR ls_payment_sub_type_code = 'CW' THEN
		ldec_sum_CM = ldec_sum_CM + tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.getitemnumber(li_row,'amount')
		ab_pre1993_payout_payments = true
	ELSE
		ldec_sum_SL = ldec_sum_SL + tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity.getitemnumber(li_row,'amount')
	END IF
NEXT

//Get the subledger balance
inv_manual_financial.nf_get_pre93_subledger_balance (il_claim_no,ldec_subledger_balance)

ldec_total_subledger_balance = ldec_sum_SL + ldec_subledger_balance

IF ab_pre1993_payout_payments = TRUE THEN
	IF ldec_total_subledger_balance <> 0 THEN
		IF (ldec_sum_CM * -1) <> ldec_total_subledger_balance THEN
			MessageBox('Error','The Annuity Payment amount must equal the pre-1993 annuity sub-ledger balance for the injured worker ('+String((ldec_total_subledger_balance) ,'$#,##0.00;[RED]($#,##0.00)')+').', Exclamation!)
			Return -1
		END IF
	ELSE
		MessageBox('Error','The current pre-1993 annuity sub-ledger balance is zero for this injured worker, so an Annuity Payment cannot be saved.', Exclamation!)
		Return -1
	END IF
ELSE
	lb_unapplied_CM_exists = inv_manual_financial.nf_unapplied_CM_exists(il_claim_no,il_individual_no,'pre93')
	IF ldec_subledger_balance = 0 AND lb_unapplied_CM_exists THEN
		MessageBox('Error','The current sub-ledger balance is $0, including an unprocessed Annuity Payment.' &
		              +'~r~nYou must delete the unprocessed Annuity Payment before adding a sub-ledger payment.', Exclamation!)
		Return -1
	END IF
END IF

RETURN 1
end function

event open;call super::open;//	Connect the dw's that update PAYMENT, UNAPPLIED_CLAIM_TXN and MANUAL_TXN_LINK to SQLCA.
dw_man_txn_unapplied_claim_txn.SetTransObject(SQLCA)
dw_man_txn_payment.SetTransObject(SQLCA)
dw_manual_txn_link.SetTransObject(SQLCA)
dw_man_txn_cost_of_claims_allocated.SetTransObject(SQLCA)
dw_annuity_payout_pre93.SetTransObject(SQLCA)
dw_annuity_setaside_pre93.SetTransObject(SQLCA)

//	Call the function to set up the window with drop downs.
wf_initialize_window()

dw_financial_transaction.SetFocus()

inv_manual_financial = Create n_manual_financial
end event

on w_add_manual_financial_trns.create
int iCurrent
call super::create
if IsValid(this.MenuID) then destroy(this.MenuID)
if this.MenuName = "m_cmwb_notools" then this.MenuID = create m_cmwb_notools
this.cb_cancel=create cb_cancel
this.dw_total_manual_trns=create dw_total_manual_trns
this.cb_clear=create cb_clear
this.cb_close=create cb_close
this.dw_financial_transaction=create dw_financial_transaction
this.cb_save=create cb_save
this.tab_manual_transactions=create tab_manual_transactions
this.dw_annuity_setaside_pre93=create dw_annuity_setaside_pre93
this.dw_annuity_payout_pre93=create dw_annuity_payout_pre93
this.dw_manual_txn_link=create dw_manual_txn_link
this.dw_man_txn_unapplied_claim_txn=create dw_man_txn_unapplied_claim_txn
this.dw_man_txn_payment=create dw_man_txn_payment
this.dw_man_txn_cost_of_claims_allocated=create dw_man_txn_cost_of_claims_allocated
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_cancel
this.Control[iCurrent+2]=this.dw_total_manual_trns
this.Control[iCurrent+3]=this.cb_clear
this.Control[iCurrent+4]=this.cb_close
this.Control[iCurrent+5]=this.dw_financial_transaction
this.Control[iCurrent+6]=this.cb_save
this.Control[iCurrent+7]=this.tab_manual_transactions
this.Control[iCurrent+8]=this.dw_annuity_setaside_pre93
this.Control[iCurrent+9]=this.dw_annuity_payout_pre93
this.Control[iCurrent+10]=this.dw_manual_txn_link
this.Control[iCurrent+11]=this.dw_man_txn_unapplied_claim_txn
this.Control[iCurrent+12]=this.dw_man_txn_payment
this.Control[iCurrent+13]=this.dw_man_txn_cost_of_claims_allocated
end on

on w_add_manual_financial_trns.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_cancel)
destroy(this.dw_total_manual_trns)
destroy(this.cb_clear)
destroy(this.cb_close)
destroy(this.dw_financial_transaction)
destroy(this.cb_save)
destroy(this.tab_manual_transactions)
destroy(this.dw_annuity_setaside_pre93)
destroy(this.dw_annuity_payout_pre93)
destroy(this.dw_manual_txn_link)
destroy(this.dw_man_txn_unapplied_claim_txn)
destroy(this.dw_man_txn_payment)
destroy(this.dw_man_txn_cost_of_claims_allocated)
end on

type cb_cancel from commandbutton within w_add_manual_financial_trns
integer x = 357
integer y = 2340
integer width = 274
integer height = 100
integer taborder = 90
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
end type

event clicked;wf_initialize_window()
	
end event

type dw_total_manual_trns from u_dw_online within w_add_manual_financial_trns
integer x = 1778
integer y = 2308
integer width = 923
integer height = 132
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_total_manual_trns"
borderstyle borderstyle = styleraised!
end type

type cb_clear from commandbutton within w_add_manual_financial_trns
integer x = 654
integer y = 2340
integer width = 274
integer height = 100
integer taborder = 100
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cl&ear"
end type

event clicked;/*	Call the function to reset the dw's and drop downs.
*/
	wf_initialize_window()
	
end event

type cb_close from commandbutton within w_add_manual_financial_trns
integer x = 951
integer y = 2340
integer width = 274
integer height = 100
integer taborder = 110
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Close"
end type

event clicked;/*	Reset the dw's, then call the function to reset the dw's and drop downs.
*/
	Close(Parent)
	
end event

type dw_financial_transaction from u_dw_online within w_add_manual_financial_trns
integer x = 9
integer y = 40
integer width = 2688
integer height = 344
integer taborder = 20
boolean bringtotop = true
string dataobject = "d_financial_transaction"
borderstyle borderstyle = styleraised!
end type

event itemchanged;call super::itemchanged;DataWindowChild ldwc_child, ldwc_claim_participants
Integer         li_rtn
Long            ll_claim_no, ll_return_code, ll_cost_alloc_no, ll_count, ll_individual_no, ll_posting_period, ll_cost_alloc_operation_no
String          ls_column_name , ls_filter, ls_individual_name, ls_employer_name, ls_operation_name


//	Validate the current field by calling the common function wf_validate_financial_transactions. If it
// returns 1, then there was an error with the field.
ls_column_name = THIS.GetColumnName()

IF IsNull(data) THEN
	data = THIS.GetText()
END IF

CHOOSE CASE ls_column_name
		
	// Verify the Claim Number.
	CASE 'claim_no'		
		il_claim_no = Long(data)
		
		//	Clear out the claim participants dddw so that you wont see the wrong claim participants for the 
		// claim # just entered.
		ll_return_code = dw_financial_transaction.GetChild('individual_no',ldwc_claim_participants)
		ldwc_claim_participants.SetTransObject(SQLCA)
		ldwc_claim_participants.Reset()	
		dw_financial_transaction.SetItem(1, 'individual_no', 0)
						
		// Validate that the claim is a valid claim number.
		ll_count = wf_validate_claim_no(il_claim_no)
		IF ll_count < 0 THEN
			RETURN 1
		ELSEIF ll_count = 0 THEN
			MessageBox('Invalid Claim','The claim number you entered is not valid!')
			RETURN 1
		END IF
		
		// Retrieve the drop down data window of claim participants, based on the claim number.
		// Note: ALL claimants are listed, not just the active ones, as you may need to transfer costs 
		// from a participant that was set up incorrectly.
		ll_return_code = ldwc_claim_participants.Retrieve(il_claim_no)
		SQLCA.nf_handle_error('w_add_manual_financial_trns', '', 'wf_validate_financial_transactions - ldwc_claim_participants.Retrieve(ll_claim_no)')
		
		// default the individual into the dropdown
		IF ll_return_code = 1 THEN
			ls_individual_name = ldwc_claim_participants.getitemstring(1,'name')
			dw_financial_transaction.setcolumn('individual_no')
			dw_financial_transaction.settext(ls_individual_name)	
			IF dw_financial_transaction.accepttext() = -1 THEN RETURN 1
			ll_individual_no = dw_financial_transaction.GetItemNumber(1,'individual_no')
			IF wf_validate_individual(ll_individual_no,il_claim_no) < 0 THEN RETURN 1
		END IF 
		
		// filter drop down to only display claimant & surviving spouse
		THIS.GetChild('individual_no', ldwc_child)
		ls_filter = 'claim_role_code = "C" or claim_role_code = "SS"'
		li_rtn = ldwc_child.SetFilter(ls_filter)
		li_rtn = ldwc_child.Filter()
		
		wf_default_cost_allocation(il_claim_no)
		
	// Verify the from individual number and get the claim role code.
	CASE 'individual_no'
		ll_individual_no = Long(data)
		ll_claim_no = dw_financial_transaction.GetItemNumber(1,'claim_no')
		IF wf_validate_individual(ll_individual_no,ll_claim_no) < 0 THEN RETURN 1
		
	// Verify the From Cost Allocation Number
	CASE 'cost_alloc_no'
		ll_cost_alloc_no = Long(data)
		ll_claim_no = dw_financial_transaction.GetItemNumber(1,'claim_no')
		IF IsNull(ll_cost_alloc_no) THEN
			ll_cost_alloc_no = 0
		END IF

		// If a value has been entered for Cost Allocation, then verify the number and get the name.
		IF ll_cost_alloc_no < 1 THEN
			dw_financial_transaction.SetItem(1, 'employer_name', '')
			dw_financial_transaction.SetItem(1, 'operation_name', '')
			dw_financial_transaction.SetItem(1, 'cost_alloc_operation_no', 0)
			is_cost_alloc_type_code = ''
		ELSE
			li_rtn = inv_manual_financial.nf_validate_cost_alloc_no(ll_cost_alloc_no,is_cost_alloc_type_code)
			IF li_rtn < 0 THEN
				dw_financial_transaction.SetItem(1, 'employer_name', '')
				dw_financial_transaction.SetItem(1, 'operation_name', '')
				dw_financial_transaction.SetItem(1, 'cost_alloc_operation_no', 0)
				is_cost_alloc_type_code = ''
				RETURN 1
			END IF
			
			li_rtn = inv_manual_financial.nf_cost_allocation_warnings(ll_claim_no,ll_cost_alloc_no)
			IF li_rtn < 0 THEN
				dw_financial_transaction.SetItem(1, 'employer_name', '')
				dw_financial_transaction.SetItem(1, 'operation_name', '')
				dw_financial_transaction.SetItem(1, 'cost_alloc_operation_no', 0)
				is_cost_alloc_type_code = ''
				RETURN 1
			END IF
		END IF
		
		// If a value has been entered for both Cost Allocation and Operation No, then revalidate operation number.
		ll_cost_alloc_operation_no = dw_financial_transaction.GetItemNumber(1,'cost_alloc_operation_no')
		IF ll_cost_alloc_no > 0 AND ll_cost_alloc_operation_no > 0 THEN
			li_rtn = inv_manual_financial.nf_validate_cost_alloc_operation_no(ll_cost_alloc_no,ll_cost_alloc_operation_no)
			IF li_rtn < 0 THEN
				dw_financial_transaction.SetItem(1, 'employer_name', '')
				dw_financial_transaction.SetItem(1, 'operation_name', '')
				dw_financial_transaction.SetItem(1, 'cost_alloc_operation_no', 0)
				is_cost_alloc_type_code = ''
				RETURN 1
			ELSE
				inv_manual_financial.nf_display_cost_allocation_data(ll_cost_alloc_no,ll_cost_alloc_operation_no,ls_employer_name,ls_operation_name)
				dw_financial_transaction.SetItem(1,'employer_name',ls_employer_name)
				dw_financial_transaction.SetItem(1,'operation_name',ls_operation_name)
			END IF
		END IF		
	
	//	Verify the From Cost Allocation operation number.
	CASE 'cost_alloc_operation_no'
		ll_cost_alloc_no = dw_financial_transaction.GetItemNumber(1, 'cost_alloc_no')
		ll_cost_alloc_operation_no = Long(data)
		IF IsNull(ll_cost_alloc_operation_no) = TRUE THEN
			ll_cost_alloc_operation_no = 0
		END IF

		// If a value has been entered for both Cost Allocation and Operation No, then retrieve the
		// operation name by calling the function to validate and retrieve operation number.
		IF ll_cost_alloc_no > 0 AND ll_cost_alloc_operation_no > 0 THEN
			li_rtn = inv_manual_financial.nf_validate_cost_alloc_operation_no(ll_cost_alloc_no,ll_cost_alloc_operation_no)
			IF li_rtn < 0 THEN
				dw_financial_transaction.SetItem(1, 'operation_name', '')
				RETURN 1
			END IF
			inv_manual_financial.nf_display_cost_allocation_data(ll_cost_alloc_no,ll_cost_alloc_operation_no,ls_employer_name,ls_operation_name)
			dw_financial_transaction.SetItem(1,'employer_name',ls_employer_name)
			dw_financial_transaction.SetItem(1,'operation_name',ls_operation_name)
		ELSE
			dw_financial_transaction.SetItem(1, 'operation_name', '')
		END IF
		
END CHOOSE

RETURN 0
end event

event itemfocuschanged;/* Override ancestor script to have edits on edit mask fields work correctly.
*/
end event

type cb_save from commandbutton within w_add_manual_financial_trns
integer x = 59
integer y = 2340
integer width = 274
integer height = 100
integer taborder = 80
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Save"
end type

event clicked;LONG     ll_default_cost_alloc_no, ll_default_cost_alloc_operation_no
LONG     ll_CM_recipient_no, ll_CM_cheque_no, ll_DM_recipient_no, ll_DM_cheque_no
INTEGER  li_row, li_count, li_rtn, li_found
DECIMAL  ldc_annuity, ldc_CM_amount, ldc_DM_amount
STRING   ls_default_cost_alloc_type_code, ls_payment_sub_type_code, ls_find, ls_matching_payment_sub_type_code
BOOLEAN  lb_pre1993_payout_payments, lb_DMs_exist, lb_restore_default_cost_allocation
DATE     ldt_CM_paid_from, ldt_CM_paid_to, ldt_DM_paid_from, ldt_DM_paid_to
DWObject l_dwo
U_DW_ONLINE ldw_annuity_transactions

N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '012' refers to the Add Manual Financial Txns module, '044' refers to the Payment Processing module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('012','044','save',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF
/******************************************************************************************/

SetPointer(HourGlass!)

ldw_annuity_transactions = tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity

//	Force accept of data in all dw's, then force the validation of all fields in dw_financial_transaction.
IF dw_financial_transaction.AcceptText() = -1 THEN RETURN -1
IF ldw_annuity_transactions.AcceptText() = -1 THEN RETURN -1

//	Populate instance variables
li_rtn = wf_populate_instance_variables()
IF li_rtn < 0 THEN
	RETURN
END IF

//	Ensure that at least the from claim number is entered.
IF IsNull(il_claim_no) OR il_claim_no < 0 THEN
	MessageBox("Missing Claim Number","The 'From' Claim number must be specified!",Exclamation!)
	RETURN
END IF

//	Ensure that the individual has been selected.
li_rtn = wf_validate_individual(il_individual_no,il_claim_no)
IF li_rtn = -1 THEN
	RETURN
END IF

// CM & non-CM cannot be part of same batch, so validate using first record's sub type
ls_payment_sub_type_code = ldw_annuity_transactions.GetItemString(1,'payment_sub_type_code')

//	Ensure that the from cost allocation is entered.
IF ls_payment_sub_type_code <> '' THEN
	li_rtn = inv_manual_financial.nf_validate_cost_allocation(ls_payment_sub_type_code, il_claim_no, il_cost_alloc_no, il_cost_alloc_operation_no, is_cost_alloc_type_code, &
																				 lb_restore_default_cost_allocation, ll_default_cost_alloc_no, ll_default_cost_alloc_operation_no, ls_default_cost_alloc_type_code)
	IF li_rtn < 0 THEN
		IF lb_restore_default_cost_allocation THEN
			
			dw_financial_transaction.SetColumn('cost_alloc_no')
			dw_financial_transaction.SetItem(1,'cost_alloc_no',ll_default_cost_alloc_no)
			l_dwo = dw_financial_transaction.Object.cost_alloc_no
			dw_financial_transaction.EVENT TRIGGER ItemChanged(1,l_dwo,String(ll_default_cost_alloc_no))
			
			dw_financial_transaction.SetColumn('cost_alloc_operation_no')
			dw_financial_transaction.SetItem(1,'cost_alloc_operation_no',ll_default_cost_alloc_operation_no)
			l_dwo = dw_financial_transaction.Object.cost_alloc_operation_no
			dw_financial_transaction.EVENT TRIGGER ItemChanged(1,l_dwo,String(ll_default_cost_alloc_operation_no))
			
			is_cost_alloc_type_code = ls_default_cost_alloc_type_code
		END IF
		RETURN
	END IF
END IF

//	Ensure that there is at least one line of detail entered. (Can't create a payment record without an amount!)
ldc_annuity = ldw_annuity_transactions.GetItemDecimal(1,"amount")
            
IF IsNull(ldc_annuity) THEN
	MessageBox("Missing Details","Cannot create manual transactions without an amount. Please enter at least one complete set of details.",Exclamation!)
	RETURN
ELSEIF ldc_annuity = 0 THEN
	MessageBox("Missing Details","Cannot create manual transactions with zero amounts. Please enter a proper amount.",Exclamation!)
	RETURN
END IF

// Get the number of detail rows, the Cost of Claims period, transaction type and processing date.
ii_comp_rows_found_annuity = ldw_annuity_transactions.RowCount()
il_coc_period = dw_financial_transaction.GetItemNumber(1,"posting_period")

//	Set the scheduled processing date to today at 12:00 AM
idt_scheduled_processing_datetime = f_server_datetime()
idt_scheduled_processing_date = DateTime(Date(idt_scheduled_processing_datetime))

// Validate the transaction details
li_rtn  = wf_validate_comp_details_annuity()
IF li_rtn < 0 THEN
	RETURN
END IF


// For Void Annuity Payments (i.e., DM, DO , DW) don't allow a matching Annuity payment (i.e., CM, CO, CW) to be entered at the same time
// Possible to have multiples, so loop thru all DM, DO, DW and find matching void transactions

//FOR li_row = 1 to ii_comp_rows_found_annuity
//	ls_payment_sub_type_code = ldw_annuity_transactions.GetItemString(li_row,'payment_sub_type_code')
//	IF ls_payment_sub_type_code = 'DM' OR ls_payment_sub_type_code = 'DO' OR ls_payment_sub_type_code = 'DW' THEN
//		ll_DM_recipient_no = dw_financial_transaction.GetItemNumber(1,'individual_no')
//		ll_DM_cheque_no    = ldw_annuity_transactions.getitemnumber(li_row,'cheque_no')
//		ldc_DM_amount      = ldw_annuity_transactions.getitemdecimal(li_row,'amount') 
//		ldt_DM_paid_from   = Date(ldw_annuity_transactions.getitemdatetime(li_row,'date_paid_from'))
//		ldt_DM_paid_to     = Date(ldw_annuity_transactions.getitemdatetime(li_row,'date_paid_to'))
//		
//		//now check to see if a matching payment (CM, CO, CW) exists in the entry datawindow
//		IF ls_payment_sub_type_code = 'DM' THEN ls_matching_payment_sub_type_code = 'CM'
//		IF ls_payment_sub_type_code = 'DO' THEN ls_matching_payment_sub_type_code = 'CO'
//		IF ls_payment_sub_type_code = 'DW' THEN ls_matching_payment_sub_type_code = 'CW'
//		
//		ls_find =  "payment_sub_type_code = '" + ls_matching_payment_sub_type_code + "'"  &
//		        +  " AND amount = " + STRING(ldc_DM_amount *  -1) &
//				  +  " AND cheque_no = " + STRING(ll_DM_cheque_no)  &
//				  +  " AND individual_no = " + STRING(ll_DM_recipient_no) &
//				  +  " AND date_paid_from = " + STRING(ldt_DM_paid_from)  &
//				  +  " AND date_paid_to = " + STRING(ldt_DM_paid_to)
//		
//		li_found = ldw_annuity_transactions.Find(ls_find,0,ldw_annuity_transactions.RowCount())
//		IF li_found > 0 THEN
//			// Don't allow a CM and DM to be entered together if they are exactly the same.
//			MessageBox('Error','A Void Annuity Payment (i.e. 97/DM, DO, DW) must not be entered, if there is also a matching Annuity Payment (i.e. 97/CM,CO,CW) entered at the same time.',Exclamation!)
//			RETURN
//		END IF
//		
//		// look for unapplied CMs
//		// BR 2.180 - A Void Annuity Payment (i.e. 97/DM,Do,DW) must not be entered, if there is a matching scheduled Annuity Payment (i.e. 97/CM,'CO,CW). 
//		li_count = inv_manual_financial.nf_find_unapplied_annuity_payments(ls_payment_sub_type_code,ldt_DM_paid_from,ldt_DM_paid_to, ll_DM_cheque_no, ldc_dm_amount, ll_dm_recipient_no)
//		IF li_count > 0 THEN
//			RETURN
//		END IF
//		
//		// now look for applied CMs
//		//2.170	A Void Annuity Payment (i.e. 97/DM) must have all of the following:
//		//	·	the same amount as the Annuity Payment (i.e. 97/CM) but of the opposite sign 
//		//	·	the same cheque number as the Annuity Payment
//		//	·	the same payment period as the Annuity Payment.
//		//	·	the same recipient as the Annuity Payment.
//		li_count = inv_manual_financial.nf_find_applied_annuity_payments(ls_payment_sub_type_code,ldt_DM_paid_from,ldt_DM_paid_to,ll_DM_cheque_no,ldc_dm_amount,ll_dm_recipient_no)
//		IF li_count = 0 THEN
//			RETURN
//		END IF
//		
//	END IF	
//NEXT
//


//If the claimant is an Injured Worker then proceed into the correct transaction posting period, Post 92 or Pre 93 for each row entered.
IF is_claim_role_code = 'C' THEN
	IF ib_post92 = TRUE THEN
		li_rtn = wf_setup_post92_txns()
		IF li_rtn < 0 THEN RETURN 
	ELSE
		li_rtn = wf_setup_pre93_txns(lb_pre1993_payout_payments)
		IF li_rtn < 0 THEN RETURN 
	END IF
ELSEIF is_claim_role_code = 'SS' THEN
	IF wf_check_subledger_ss() < 0 THEN RETURN
END IF


FOR li_row = 1 to ii_comp_rows_found_annuity
	
	li_rtn = wf_setup_annuity_payments(li_row)
	IF li_rtn < 0 THEN
		RETURN
	ELSEIF li_rtn = 1 THEN
		wf_create_man_txns('0','ann',li_row,il_cost_alloc_no, il_cost_alloc_operation_no,is_cost_alloc_type_code)
	END IF

NEXT


SQLCA.nf_begin_transaction()

//	Set up the payment, txn and link numbers...
li_rtn = wf_update_numbers()
IF li_rtn < 0 THEN
	SQLCA.nf_rollback_transaction()
	MessageBox("Error","Error saving manual transaction(s)!",Exclamation!)
	RETURN
END IF

//	Update and commit ...
li_rtn = dw_man_txn_payment.Update()
li_rtn = SQLCA.nf_handle_error("w_add_manual_financial_trns", "", "cb_save - dw_man_txn_payment.Update()")

li_rtn = dw_man_txn_unapplied_claim_txn.Update()
li_rtn = SQLCA.nf_handle_error("w_add_manual_financial_trns", "", "cb_save - dw_man_txn_unapplied_claim_txn.Update()")

li_rtn = dw_man_txn_cost_of_claims_allocated.Update()
li_rtn = SQLCA.nf_handle_error("w_add_manual_financial_trns", "", "cb_save - dw_man_txn_cost_of_claims_allocated.Update()")

li_rtn = dw_manual_txn_link.Update()
li_rtn = SQLCA.nf_handle_error("w_add_manual_financial_trns", "", "cb_save - dw_manual_txn_link.Update()")

// P10273-2
// update conversion records if users are entering pre93 manual payments for injured workers
IF is_claim_role_code = 'C' THEN
	IF ib_post92 THEN
		// do not update conversion tables
	ELSE
		IF lb_pre1993_payout_payments THEN
			li_rtn = dw_annuity_payout_pre93.Update()
			li_rtn = SQLCA.nf_handle_error("w_add_manual_financial_trns", "", "cb_save - dw_annuity_payout_pre93.Update()")
		ELSE
			li_rtn = dw_annuity_setaside_pre93.Update()
			li_rtn = SQLCA.nf_handle_error("w_add_manual_financial_trns", "", "cb_save - dw_annuity_setaside_pre93.Update()")
		END IF
	END IF
END IF

// commit transaction
SQLCA.nf_commit_transaction()


//	Remove the data to prevent update of the data twice
cb_clear.TriggerEvent(Clicked!)
ib_accept_claim_no = False

end event

type tab_manual_transactions from tab within w_add_manual_financial_trns
integer x = 18
integer y = 428
integer width = 2688
integer height = 1840
integer taborder = 30
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
boolean raggedright = true
boolean boldselectedtext = true
alignment alignment = center!
integer selectedtab = 1
tabpage_annuity_payments tabpage_annuity_payments
end type

on tab_manual_transactions.create
this.tabpage_annuity_payments=create tabpage_annuity_payments
this.Control[]={this.tabpage_annuity_payments}
end on

on tab_manual_transactions.destroy
destroy(this.tabpage_annuity_payments)
end on

type tabpage_annuity_payments from userobject within tab_manual_transactions
event create ( )
event destroy ( )
integer x = 18
integer y = 108
integer width = 2651
integer height = 1716
long backcolor = 67108864
string text = "Annuity Payments"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_comp_manual_transactions_annuity dw_comp_manual_transactions_annuity
cb_add_annuity cb_add_annuity
end type

on tabpage_annuity_payments.create
this.dw_comp_manual_transactions_annuity=create dw_comp_manual_transactions_annuity
this.cb_add_annuity=create cb_add_annuity
this.Control[]={this.dw_comp_manual_transactions_annuity,&
this.cb_add_annuity}
end on

on tabpage_annuity_payments.destroy
destroy(this.dw_comp_manual_transactions_annuity)
destroy(this.cb_add_annuity)
end on

type dw_comp_manual_transactions_annuity from u_dw_online within tabpage_annuity_payments
integer x = 41
integer y = 32
integer width = 2578
integer height = 1460
integer taborder = 100
string dataobject = "d_comp_manual_transactions_annuity_2"
boolean vscrollbar = true
end type

event editchanged;call super::editchanged;wf_set_dw_item_status ( tab_manual_transactions.tabpage_annuity_payments.dw_comp_manual_transactions_annuity , row )
end event

event itemfocuschanged;call super::itemfocuschanged;int li_rtn, ll_row
date ldt_effective_from_date, ldt_effective_to_date
string ls_filter, ls_filter2, ls_from_date, ls_to_date


IF dwo.name = 'payment_sub_type_code' THEN
 
	ldt_effective_from_date = Date(this.getitemdatetime(this.getrow(),"date_paid_from"))
	ldt_effective_to_date = Date(this.getitemdatetime(this.getrow(),"date_paid_to"))
	
	ls_from_date = String(ldt_effective_from_date,'YYYY-MM-DD')
	ls_to_date = String(ldt_effective_to_date,'YYYY-MM-DD')
	
	IF ls_to_date = '1993-01-01' THEN
		// if the user has entered 1993-01-01 as the paid to date
		// then we will filter for the pre-93 effective dates
		ls_to_date = '1992-12-31'
	END IF
	
	ls_filter = 'effective_from_date <= ' + ls_from_date  + ' and (effective_to_date >= ' + ls_to_date  +  ' or ISNULL(effective_to_date))'
	
	li_rtn= idwc_annuity_payment_sub_type_code.SetFilter( ls_filter) 
	li_rtn= idwc_annuity_payment_sub_type_code.Filter()

END IF
end event

event itemchanged;call super::itemchanged;String ls_payment_sub_type_code, ls_other_payment_sub_type_code
Long ll_count
date ldt_paid_from
BOOLEAN    lb_payment_type_error
INTEGER    li_counter, li_rtn
DWObject   l_dwo


CHOOSE CASE dwo.name
	CASE 'date_paid_from'
		
		IF Date(data) < 1982-01-01 THEN
			MessageBox('Validation Error','There are no payment types that exist prior to 1982-01-01. Please enter a new date.',Exclamation!)
			Return 1
		END IF
		
		//Check to see if this is for an injured worker or a surviving spouse
		//If this is for a IW then the paid from and to can not span over 1993. The user must enter all Post 92 or all Pre 93 transactions at once and save. 
		// 1993-01-01 is exclusive and can be included in both the Post 92 and Pre 93 dates. 
		IF is_claim_role_code = 'C' THEN
			ldt_paid_from = Date(data)
			
			IF row = 1 THEN
				IF ldt_paid_from < 1993-01-01 THEN
					IF Date(this.getitemdatetime(1,'date_paid_to')) > 1993-01-01 THEN
						MessageBox('Error','The pre-1993 paid from and paid to dates cannot span 1993-01-01. Please re-enter the paid dates.',Exclamation!)
						Return 1
					END IF
					
					//Pre93 rules apply
					ib_post92 = FALSE
				ELSE
					ib_post92 = TRUE
				END IF			
			END IF
			
			IF row <> 1 THEN 
				IF ( Date(this.getitemdatetime(row,'date_paid_from')) < 1993-01-01 OR ldt_paid_from < 1993-01-01) and ib_post92 = True THEN
					Messagebox('Error','The sequence of payments is for Post 92 transactions. Please enter all Post 92 transactions and then save.',Exclamation!)
					Return 1
				END IF
				IF ( Date(this.getitemdatetime(row,'date_paid_from')) >= 1993-01-01 OR ldt_paid_from > 1993-01-01)  and ib_post92 = False THEN
					Messagebox('Error','The sequence of payments is for Pre 93 transactions. Please enter all Pre 93 transactions and then save.',Exclamation!)
					Return 1
				END IF			
			  END IF
		END IF
		
		// for either injured workers or surviving spouses
		// the list of available payment sub type codes is determined by
		// the paid date range
		// so if the range is changed, then the payment sub type code should be cleared.
		IF THIS.GetItemString(row,'payment_sub_type_code') > '' THEN
			MessageBox('Error','The paid from date should not be entered after the payment sub type. Please select a payment sub type again.',Exclamation!)
			THIS.SetItem(row,'payment_sub_type_code','')
			l_dwo = THIS.Object.payment_sub_type_code
			THIS.EVENT TRIGGER ItemChanged(1,l_dwo,'')
		END IF
		
	CASE 'date_paid_to'
		IF is_claim_role_code = 'C' THEN
			IF Date(this.getitemdatetime(1,'date_paid_from')) < 1993-01-01 AND Date(data) > 1993-01-01 THEN
				MessageBox('Error','The pre-1993 paid from and paid to dates cannot span 1993-01-01. Please re-enter the paid dates.',Exclamation!)
				Return 1
			END IF
		END IF
		
		// for either injured workers or surviving spouses
		// the list of available payment sub type codes is determined by
		// the paid date range
		// so if the range is changed, then the payment sub type code should be cleared.
		IF THIS.GetItemString(row,'payment_sub_type_code') > '' THEN
			MessageBox('Error','The paid from date should not be entered after the payment sub type. Please select a payment sub type again.',Exclamation!)
			THIS.SetItem(row,'payment_sub_type_code','')
			l_dwo = THIS.Object.payment_sub_type_code
			THIS.EVENT TRIGGER ItemChanged(1,l_dwo,'')
		END IF

	CASE 'cheque_no'
		IF (IsNull(data) OR data = '' ) AND (this.getitemstring(row,'payment_sub_type_code') = 'CM' OR this.getitemstring(row,'payment_sub_type_code') = 'DM') THEN
			MessageBox('Error','A valid Cheque no must be entered.',Exclamation!)
			Return 1
		END IF
		
	CASE 'amount'
		IF this.getitemstring(row,'payment_sub_type_code') = 'CM' and Dec(data) >= 0 THEN
			MessageBox('Error','A CM payment must have a negative amount.',Exclamation!)
			Return 1
		END IF
		IF this.getitemstring(row,'payment_sub_type_code') = 'DM' and Dec(data) <= 0 THEN
			MessageBox('Error','A DM payment must have a positive amount.',Exclamation!)
			Return 1
		END IF
		
	CASE 'payment_sub_type_code'
		ls_payment_sub_type_code = String(data)
		IF ls_payment_sub_type_code = '' THEN
			RETURN 1
		END IF
				
		FOR li_counter = 1 TO THIS.RowCount() 
			// if entering CM or DM, verify that non-CM or non-DM has not already been entered
			// if entering non-CM or non-DM, verify that CM or DM has not already been entered

			IF ls_payment_sub_type_code = 'CM' OR ls_payment_sub_type_code = 'CO' OR ls_payment_sub_type_code = 'CW' &
			OR ls_payment_sub_type_code = 'DM' OR ls_payment_sub_type_code = 'DO' OR ls_payment_sub_type_code = 'DW' THEN
				ls_other_payment_sub_type_code = THIS.GetItemString(li_counter,'payment_sub_type_code')
				IF ls_other_payment_sub_type_code <> 'CM' AND ls_other_payment_sub_type_code <> 'CO' AND ls_other_payment_sub_type_code <> 'CW' AND &
					ls_other_payment_sub_type_code <> 'DM' AND ls_other_payment_sub_type_code <> 'DO' AND ls_other_payment_sub_type_code <> 'DW' and li_counter <> row THEN
					MessageBox('Payment Sub Type Error','You cannot enter an Annuity Payment or Void Annuity Payment at the same time as another payment of a different subtype.',Exclamation!)
					lb_payment_type_error = TRUE
					EXIT
				END IF
				dw_financial_transaction.SetItem(1,'cost_alloc_no',0)
				dw_financial_transaction.SetItem(1,'cost_alloc_operation_no',0)
				dw_financial_transaction.SetItem(1,'employer_name','')
				dw_financial_transaction.SetItem(1,'operation_name','')
				il_cost_alloc_no= 0
				il_cost_alloc_operation_no = 0
				is_cost_alloc_type_code = ''
			ELSE
				ls_other_payment_sub_type_code = THIS.GetItemString(li_counter,'payment_sub_type_code')
				IF ls_other_payment_sub_type_code = 'CM' OR ls_other_payment_sub_type_code = 'CO' OR ls_other_payment_sub_type_code = 'CW' OR &
					ls_other_payment_sub_type_code = 'DM' OR ls_other_payment_sub_type_code = 'DO' OR ls_other_payment_sub_type_code = 'DW' and li_counter <> row THEN
					MessageBox('Payment Sub Type Error','You cannot enter another payment at same time as an Annuity Payment or Void Annuity Payment .',Exclamation!)
					lb_payment_type_error = TRUE
					EXIT
				END IF
				
				IF ls_payment_sub_type_code = 'IN' THEN
					dw_financial_transaction.SetItem(1,'cost_alloc_no',0)
					dw_financial_transaction.SetItem(1,'cost_alloc_operation_no',0)
					dw_financial_transaction.SetItem(1,'employer_name','')
					dw_financial_transaction.SetItem(1,'operation_name','')
					il_cost_alloc_no= 0
					il_cost_alloc_operation_no = 0
					is_cost_alloc_type_code = ''
				END IF
				
			END IF			
		NEXT
		IF lb_payment_type_error THEN
			RETURN 1
		END IF
			
		
END CHOOSE
	
	

end event

type cb_add_annuity from commandbutton within tabpage_annuity_payments
integer x = 105
integer y = 1564
integer width = 402
integer height = 108
integer taborder = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Add &Annuity"
end type

event clicked;dw_comp_manual_transactions_annuity.InsertRow(0)

end event

type dw_annuity_setaside_pre93 from u_datawindow within w_add_manual_financial_trns
integer x = 722
integer y = 1196
integer taborder = 90
string dataobject = "d_annuity_setaside_pre93"
borderstyle borderstyle = styleraised!
end type

type dw_annuity_payout_pre93 from u_datawindow within w_add_manual_financial_trns
integer x = 146
integer y = 1196
integer width = 443
integer taborder = 80
string dataobject = "d_annuity_payout_pre93"
borderstyle borderstyle = styleraised!
end type

type dw_manual_txn_link from u_dw_online within w_add_manual_financial_trns
event itemchanged pbm_dwnitemchange
event losefocus pbm_dwnkillfocus
boolean visible = false
integer x = 137
integer y = 656
integer height = 360
integer taborder = 40
string dataobject = "d_manual_txn_link"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
end type

type dw_man_txn_unapplied_claim_txn from u_dw_online within w_add_manual_financial_trns
event itemchanged pbm_dwnitemchange
event losefocus pbm_dwnkillfocus
boolean visible = false
integer x = 713
integer y = 656
integer height = 360
integer taborder = 50
string dataobject = "d_man_txn_unapplied_claim_txn"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
end type

type dw_man_txn_payment from u_dw_online within w_add_manual_financial_trns
event itemchanged pbm_dwnitemchange
event losefocus pbm_dwnkillfocus
boolean visible = false
integer x = 1289
integer y = 656
integer height = 360
integer taborder = 60
string dataobject = "d_man_txn_payment"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
end type

type dw_man_txn_cost_of_claims_allocated from u_dw_online within w_add_manual_financial_trns
boolean visible = false
integer x = 1851
integer y = 656
integer height = 360
integer taborder = 70
string dataobject = "d_man_txn_cost_of_claims_allocated"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
end type

