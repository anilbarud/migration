$PBExportHeader$w_payments.srw
forward
global type w_payments from w_a_tool
end type
type dw_payment_list from u_dw_online within w_payments
end type
type cb_save from commandbutton within w_payments
end type
type cb_add_recipient from commandbutton within w_payments
end type
type cb_next from commandbutton within w_payments
end type
type cb_prev from commandbutton within w_payments
end type
type st_cancelled from statictext within w_payments
end type
type cb_add from commandbutton within w_payments
end type
type cb_delete from commandbutton within w_payments
end type
type cb_cancel from commandbutton within w_payments
end type
type uo_payments from u_payment within w_payments
end type
type sle_reminder from singlelineedit within w_payments
end type
type sle_overpayment from singlelineedit within w_payments
end type
type st_save from statictext within w_payments
end type
type cb_reminders from commandbutton within w_payments
end type
type cb_delete_recipient from commandbutton within w_payments
end type
type cb_group from commandbutton within w_payments
end type
end forward

global type w_payments from w_a_tool
integer width = 2953
integer height = 1872
boolean resizable = false
event display_message ( )
event ue_display_open_message ( )
event ue_postopen ( )
dw_payment_list dw_payment_list
cb_save cb_save
cb_add_recipient cb_add_recipient
cb_next cb_next
cb_prev cb_prev
st_cancelled st_cancelled
cb_add cb_add
cb_delete cb_delete
cb_cancel cb_cancel
uo_payments uo_payments
sle_reminder sle_reminder
sle_overpayment sle_overpayment
st_save st_save
cb_reminders cb_reminders
cb_delete_recipient cb_delete_recipient
cb_group cb_group
end type
global w_payments w_payments

type variables
S_WINDOW_MESSAGE istr_message
N_PAYMENT inv_payments
N_REMINDERS inv_reminders
LONG il_payment_no, il_claim_no, il_individual_no
STRING is_display_message


boolean ib_birth_date

datetime  idtm_date_from[],idtm_date_to[] , idtm_birth_date

decimal  idec_tot[],idec_adjust[]		
string is_zeroed[]		
	
datastore ids_annuity_eligibilty_validate,  ids_annuity_payment_validation
Datastore ids_reminders, ids_auto_reminder
end variables

forward prototypes
public function integer wf_enable_disable_dw ()
public function integer wf_retrieve_list ()
public function integer wf_claim_reminders ()
public function integer wf_create_auto_reminders ()
public function integer wf_refresh_recipient ()
public function integer wf_verify_bank_info (long al_recipient_no, string as_recipient_type_code)
end prototypes

event display_message();IF is_display_message > '' THEN
	MessageBox('Warning', is_display_message)
	is_display_message = ''
END IF
end event

event ue_display_open_message();S_WINDOW_MESSAGE	lstr_message

	SetPointer(HourGlass!)

	lstr_message.as_stringparm[1] = is_display_message
	lstr_message.as_stringparm[2] = 'I'
	lstr_message.al_doubleparm[1] = il_individual_no
	lstr_message.al_doubleparm[2] = il_claim_no
	
	OpenWithParm(w_payment_message,lstr_message)
	
	is_display_message = ''
end event

event ue_postopen();DATAWINDOWCHILD	ldwc_child

DATE	 	ldt_benefit_start_date, ldt_benefit_end_date, ldt_review_12week_date, ldt_review_date, ldt_accident_recurrence_date,ldt_server_date
STRING 	ls_three_day_paid_flag, ls_comp85_percent_flag, ls_desc,ls_opening_type_code, ls_courtorder
LONG 		ll_rownum, ll_opening_no, ll_result, ll_cnt,  ll_working_days, ll_rc, ll_up
DECIMAL	ldec_balance_amount
INTEGER	li_rc, li_rtn, li_rows
U_DS     lds_overpayment_warning_sum




ldt_server_date = Date(f_server_datetime())

/* let the object know about the basic claim info to avoid having to re-retrieve
*/
	inv_payments.nf_set_basic_claim(uo_payments.dw_basic_claim)

/* send the claim number to the object
*/
	inv_payments.nf_set_claim_no(il_claim_no)

	IF inv_payments.nf_is_valid_status_act() = FALSE THEN
		MessageBox('No Payments Allowed', 'The claim status does not allow for payments.')
		Close(This)
		Return
	END IF


/* validate that user is allowed here
*/
	IF inv_payments.nf_validate_claim() < 0 THEN
   	Close(This)
		Return
	END IF
/*	retrieve payments
*/
	is_display_message = ''

	ll_cnt = wf_retrieve_list()
	
/* Check for Claim Reminder messages - auto create and display 
*/	
IF gnv_user_authorizations.nf_reminder_authorization() >= 0  THEN
// User has authorization to create automatic reminders
	wf_create_auto_reminders()
END IF

//Display all planned claim reminders
wf_claim_reminders()
	

/*	check the opening for messages
*/
	uo_payments.dw_payment_details.GetChild('opening_no', ldwc_child)
	IF ldwc_child.RowCount() > 0 THEN
		ll_opening_no = ldwc_child.GetItemNumber(1,"max_opening_no")
		ll_rownum = ldwc_child.Find("opening_no = " + String(ll_opening_no),1,ldwc_child.RowCount())
		IF ll_rownum <= 0 or IsNull(ll_rownum) THEN
			Error.Text = "Error finding max OPENING number in ldwc_child"
			Error.WindowMenu="w_payments"
			Error.Object="w_payments"
			Error.ObjectEvent="open"
			SignalError()
			Return
		END IF	
		ls_opening_type_code = ldwc_child.GetItemString(ll_rownum,"opening_type_code")
		ls_three_day_paid_flag = ldwc_child.GetItemString(ll_rownum,"three_day_paid_flag")
		ls_comp85_percent_flag = ldwc_child.GetItemString(ll_rownum,"comp_85percent_flag")
		ldt_benefit_start_date = Date(ldwc_child.GetItemDateTime(ll_rownum,"benefit_start_date"))
		ldt_benefit_end_date = Date(ldwc_child.GetItemDateTime(ll_rownum,"benefit_end_date"))
		ldt_review_12week_date = Date(ldwc_child.GetItemDateTime(ll_rownum,"review_12_week_date"))
		ldt_accident_recurrence_date = Date(ldwc_child.GetItemDateTime(ll_rownum,"accident_recurrence_date"))

	
		
		IF IsNull(ldt_benefit_end_date) THEN
			IF ls_three_day_paid_flag = "N" AND ls_opening_type_code = 'RLOE' THEN
				IF ldt_accident_recurrence_date >= date ("1998/01/01") THEN
					ll_working_days = 28    /* Three Day ReImbursement after 20 Working Days for Accidents on or after Jan 01, 1998*/
				ELSE
					ll_working_days = 42    /* Three Day ReImbursement after 30 Working Days for Accidents prior to Jan 01, 1998*/
				END IF
				ldt_review_date = RelativeDate(ldt_benefit_start_date,ll_working_days)   
				IF ldt_server_date > ldt_review_date THEN
					is_display_message = is_display_message + "3 Day re-imbursement is overdue~r~nDue Date: " + String(ldt_review_date,"yyyy-mm-dd") + "~r~n"
				ELSEIF ldt_server_date = ldt_review_date THEN
						is_display_message = is_display_message + "3 Day re-imbursement is due today~r~n"
				ELSEIF DaysAfter(ldt_server_date,ldt_review_date) <= 14 THEN
						is_display_message = is_display_message + "3 Day re-imbursement is due within two weeks~r~nDue Date: " + String(ldt_review_date,"yyyy-mm-dd")+ "~r~n"
				END IF
			END IF
			IF ls_comp85_percent_flag = "N" THEN
				ldt_review_date = RelativeDate(ldt_benefit_start_date,273)
				IF ldt_server_date > ldt_review_date THEN
					is_display_message = is_display_message + "Adjustment to 85% is overdue~r~nDue Date: " + String(ldt_review_date,"yyyy-mm-dd") + "~r~n"
				ELSEIF ldt_server_date = ldt_review_date THEN
					is_display_message = is_display_message + "Adjustment to 85% is due today~r~n"
				ELSEIF DaysAfter(ldt_server_date,ldt_review_date) <= 14 THEN
					is_display_message = is_display_message + "Adjustment to 85% is due within two weeks~r~nDue Date: " + String(ldt_review_date,"yyyy-mm-dd") + "~r~n"
				END IF
			END IF
			IF IsNull(ldwc_child.GetItemDateTime(ll_rownum,"review_12_week_date")) AND ls_opening_type_code = 'RLOE' THEN
				ldt_review_date = RelativeDate(ldt_benefit_start_date,84)
				IF ldt_server_date > ldt_review_date THEN
					is_display_message = is_display_message + "12 week review is overdue~r~nDue Date: " + String(ldt_review_date,"yyyy-mm-dd") + "~r~n"
				ELSEIF ldt_server_date = ldt_review_date THEN
					is_display_message = is_display_message + "12 week review is due today~r~n"
				ELSEIF DaysAfter(ldt_server_date,ldt_review_date) <= 14 Then
					is_display_message = is_display_message + "12 week review is due within two weeks~r~nDue Date: " + String(ldt_review_date,"yyyy-mm-dd") + "~r~n"
				END IF
			END IF
			IF NOT IsNull(uo_payments.dw_basic_claim.GetItemDateTime(1,'annual_ben_review_due_date')) THEN
				ldt_review_date = Date(uo_payments.dw_basic_claim.GetItemDateTime(1,'annual_ben_review_due_date'))
				IF ldt_server_date > ldt_review_date THEN
					is_display_message = is_display_message + "Annual review is overdue~r~nDue Date: " + String(ldt_review_date,"yyyy-mm-dd") + "~r~n"
				ELSEIF ldt_server_date = ldt_review_date THEN
					is_display_message = is_display_message + "Annual review is due today~r~n"
				ELSEIF DaysAfter(ldt_server_date,ldt_review_date) <= 14 Then
					is_display_message = is_display_message + "Annual review is due within two weeks~r~nDue Date: " + String(ldt_review_date,"yyyy-mm-dd") + "~r~n"
				END IF
			END IF
			
		END IF
							
end if


/*Check for Court Order
*/
Select court_order_flag 
INTO :ls_CourtOrder
FROM INDIVIDUAL
Where individual_no = :il_individual_no
USING SQLCA;

IF ls_CourtOrder = 'Y' then
	is_display_message = is_display_message + '~r~n*********************************~r~n~Court Order Situation Exists.'
	is_display_message = is_display_message + '~r~n*********************************~r~n~r~n'
END IF


/*	check for overpayment to the claimant
*/
	lds_overpayment_warning_sum = Create U_DS
	lds_overpayment_warning_sum.DataObject = 'ds_overpayment_warning_sum'
	lds_overpayment_warning_sum.SetTransObject(SQLCA)
	li_rows = lds_overpayment_warning_sum.Retrieve(il_claim_no)
	SQLCA.nf_handle_error('w_account_payment','ds_overpayment_warning_sum','lds_overpayment_warning_sum.Retrieve')
		   
	IF li_rows > 0 THEN
		is_display_message = is_display_message + '~r~n~r~n OVERPAYMENT situation exists.'
		sle_overpayment.Text = 'Overpayment'
	END IF
	
IF is_display_message <> '' and not isnull(is_display_message) THEN
	is_display_message = "NOTE:~r~n~r~n" + is_display_message
	this.Event ue_display_open_message()
END IF
	
	
	
	SELECT payment_reminder_no 
	  INTO :ll_result
	  FROM PAYMENT_REMINDER 
	 WHERE claim_no = :il_claim_no 
   	AND reminder_type_code = 'pay' using SQLCA;
	ll_result = SQLCA.nf_handle_error("Embedded SQL: Retrieve PAYMENT_REMINDER","w_payments","open")
	IF ll_result < 0 THEN
		Close(This)
		Return
	END IF

	IF ll_result =  0 THEN
		sle_reminder.text = ls_desc + "Reminder"
		beep(2)
	ELSE
		sle_reminder.text = ""
	END IF

	ls_desc = inv_payments.nf_get_comp_day()
	IF Trim(ls_desc) <> '' THEN
		IF sle_reminder.text > '' THEN
			sle_reminder.text  = sle_reminder.text + ls_desc
		ELSE
			sle_reminder.text = ls_desc
		END IF
	END IF
end event

public function integer wf_enable_disable_dw ();DATETIME	ldtm_processed_date
LONG		ll_batch_no, ll_row
STRING	ls_txn_type

	ll_row = uo_payments.dw_payment_details.GetRow()
	IF ll_row > 0 THEN
		ldtm_processed_date = uo_payments.dw_payment_details.GetItemDateTime(ll_row,'processed_date')
		IF uo_payments.dw_transaction_details.RowCount() > 0 THEN
			ll_batch_no =	uo_payments.dw_transaction_details.GetItemNumber(1,"batch_no")	
			ls_txn_type = uo_payments.dw_transaction_details.GetItemString(1,'txn_type_code')
			IF IsNull(ls_txn_type) THEN
				ls_txn_type = ' '
			END IF
		ELSE 
			MessageBox('Error','No transactions found for payment.  See system administrator.')
			cb_delete.Enabled  = FALSE
	   	cb_add_recipient.Enabled     = FALSE
			cb_delete_recipient.Enabled = False
			uo_payments.dw_payment_details.Enabled = FALSE
			uo_payments.dw_transaction_details.Enabled = FALSE
			uo_payments.cb_search.Enabled = FALSE
			Return -1
		END IF
		IF IsNull(ldtm_processed_date) and ll_batch_no = 0 AND ls_txn_type <> '2' AND ls_txn_type <> '3' AND &
		ls_txn_type <> '4' AND ls_txn_type <> '5' AND ls_txn_type <> '8' THEN
			cb_delete.Enabled = TRUE
			uo_payments.dw_payment_details.Enabled = TRUE
			uo_payments.dw_transaction_details.Enabled = TRUE
			uo_payments.cb_search.Enabled = TRUE
			cb_add_recipient.Enabled    = TRUE
			cb_delete_recipient.Enabled = True
		ELSE
			cb_delete.Enabled  = FALSE
	   	cb_add_recipient.Enabled     = FALSE
			cb_delete_recipient.Enabled = False
			uo_payments.dw_payment_details.Enabled = FALSE
			uo_payments.dw_transaction_details.Enabled = FALSE
			uo_payments.cb_search.Enabled = FALSE
			IF ll_batch_no > 0 and IsNull(ldtm_processed_date) THEN
				is_display_message = "This transaction is currently being processed -- Modifications are not allowed"
				This.Event display_message()
			END IF
			
		END IF
	ELSE
		uo_payments.cb_search.Enabled = FALSE
		cb_delete.Enabled  = FALSE
   	cb_add_recipient.Enabled     = FALSE
		cb_delete_recipient.Enabled = False
		cb_next.enabled = FALSE
		cb_prev.enabled = FALSE
	END IF
Return 0
end function

public function integer wf_retrieve_list ();	dw_payment_list.Reset()
	inv_payments.il_list_row_count = dw_payment_list.Retrieve(il_claim_no)
	SQLCA.nf_handle_error('w_payments', 'wf_retrieve_list()','dw_payment_list.Retrieve') 
	IF inv_payments.il_list_row_count > 0 THEN 

		inv_payments.idt_max_paid_to_date = Date(dw_payment_list.GetItemDateTime(1,'max_paid_to_date'))
	END IF

Return inv_payments.il_list_row_count
end function

public function integer wf_claim_reminders ();Long ll_count, ll_row
Int		li_reminder_no
String ls_reminder_stat_desc, ls_reminder_type, ls_reminder_subtype_desc
Date	ldt_reminder_due_date

ll_count = ids_reminders.Retrieve(il_claim_no)
SQLCA.nf_handle_error('w_payments','ue_postopen','ids_reminders.Retrieve')

FOR ll_row = 1 to ll_count
	ls_reminder_stat_desc = ids_reminders.GetItemString(ll_row,"reminder_status_desc")
	ls_reminder_type = ids_reminders.GetItemString(ll_row,"reminder_type_code")
	ls_reminder_subtype_desc = ids_reminders.GetItemString(ll_row,"reminder_sub_type_desc")
	ldt_reminder_due_date = Date(ids_reminders.GetItemDatetime(ll_row,"due_date"))
	
	is_display_message = is_display_message + ls_reminder_subtype_desc+" due "+String(ldt_reminder_due_date,"YYYY-MM-DD")+".~r~n~r~n"

NEXT 	

Return 0


end function

public function integer wf_create_auto_reminders ();String	ls_claim_status_code, ls_cppd_status_code, ls_opening_type, ls_reminder_sub_type_code
DateTime	ldtm_accident_date, ldtm_today
Date		ldt_accident_date, ldt_today
Long		ll_count
Int			li_month = 0

ldtm_today = f_server_datetime()
ldt_today = Date(ldtm_today)

// Check for 24 month reminder
Select Count(*)
Into    :ll_count
From   CLAIM, OPENING
Where CLAIM.claim_no = OPENING.claim_no
And	 CLAIM.claim_no = :il_claim_no
And	 CLAIM.claim_status_code = 'A'
And	(CLAIM.cppd_status_code <> 'R' AND CLAIM.cppd_status_code <> 'D' AND CLAIM.cppd_status_code <> 'N')
And	 OPENING.accident_recurrence_date <= ( DATEADD(MONTH, -24,:ldtm_today ))
And	 OPENING.opening_type_code = 'RLOE'
And	 (OPENING.benefit_end_date IS Null OR OPENING.benefit_end_date > :ldt_today)
Using  SQLCA;

SQLCA.nf_handle_error("ERROR - w_payments","wf_create_auto_reminders","Select on OPENING")

IF ll_count < 1 THEN //Try 6 months

	// 6 month auto reminder
	Select Count(*)
	Into    :ll_count
	From   CLAIM, OPENING
	Where CLAIM.claim_no = OPENING.claim_no
	And	 CLAIM.claim_no = :il_claim_no
	And	 CLAIM.claim_status_code = 'A'
	And	(CLAIM.cppd_status_code <> 'R' AND CLAIM.cppd_status_code <> 'D' AND CLAIM.cppd_status_code <> 'N')
	And	 OPENING.accident_recurrence_date <= ( DATEADD(MONTH, -6,:ldtm_today ))
	And	 OPENING.opening_type_code = 'RLOE'
	And	 (OPENING.benefit_end_date IS Null OR OPENING.benefit_end_date > :ldt_today)
	Using  SQLCA;
	
	SQLCA.nf_handle_error("ERROR - w_payments","wf_create_auto_reminders","Select on OPENING")

	IF ll_count > 0 THEN
		li_month = 6
	END IF	

ELSEIF ll_count > 0 THEN
	li_month = 24
END IF	


IF ll_count > 0 THEN
	
	IF li_month = 6 THEN
		ls_reminder_sub_type_code = '6M' 	// 6 month auto reminder
	ELSE
		ls_reminder_sub_type_code = '24M' 	// 24 month auto reminder
	END IF	

	Select count(*)
	Into    :ll_count
	From CLAIM_REMINDER
	Where CLAIM_REMINDER.claim_no = :il_claim_no
	And	 reminder_sub_type_code = :ls_reminder_sub_type_code
	Using SQLCA;

	SQLCA.nf_handle_error("ERROR - w_payments","wf_create_auto_reminders","Select on CLAIM, CLAIM_REMINDERS")

	IF ll_count < 1 THEN
		SQLCA.nf_begin_transaction()

		// A 6 or 24 month reminder does not exist create one.
		inv_reminders.nf_create_auto_reminder(il_claim_no, "CPPD",ls_reminder_sub_type_code)
		
		Select count(*)
		Into 	:ll_count
		From  CLAIM
		Where claim_no = :il_claim_no
		And     cppd_status_code = 'I'
		Using  SQLCA;
		
		SQLCA.nf_handle_error("ERROR - w_payments","wf_create_auto_reminders","Select on CLAIM")
		
		IF ll_count > 0 THEN
			//Reset the CPPD status to Unknown when it was Inapplicable and a 6 or 24 month reminder is created.
			Update	CLAIM
			Set		cppd_status_code = 'U'
			Where	claim_no = :il_claim_no
			Using		SQLCA;
			
			SQLCA.nf_handle_error("ERROR - w_payments","wf_create_auto_reminders","Update CLAIM")
		END IF

		SQLCA.nf_commit_transaction()
	END IF	
	
END IF


Return 0
end function

public function integer wf_refresh_recipient ();/*  PR5435 ans PR5208 2006-02-03 r.s.
	 For concurrency reasons, during a save we need to do 3 things:
	1: If this is a direct deposit payment method, check the INDIVIDUAL table
	   to determine if the bank information has been removed since this window was opened
	2: If there is some banking information, repopulate with fresh information from
	   the INDIVIDUAL table
	3: If the 'use default address' option is checked, re-populate basic address information 
	   with fresh information from the INDIVIDUAL table because it might have changed since the payment window was opened. 
		The datawindow in this window uses address information from UNAPPLIED_CLAIM_TXN 
		so if the INDIVIDUAL has ben changed by another user in another module, thats the info we want. 
*/
/* P10151-40 - June 21, 2010 - JH - Changed function to be generic as it is called for Service Providers as well,
   and now that they will have the option of DD, this will need to be changed to accomodate
*/
STRING ls_address_line1, ls_address_line2, ls_city, ls_prov_state_code, ls_postal_code, ls_country_code, ls_country_name, ls_use_default_address
STRING ls_bank_account_no, ls_bank_no, ls_bank_transit_no, ls_payment_method_code, ls_recipient_type_code
STRING ls_cheque_print_group_code
LONG   ll_recipient_no, ll_row, ll_row_count

ll_row_count = uo_payments.dw_transaction_details.RowCount()

FOR ll_row = 1 TO ll_row_count
	ls_recipient_type_code = uo_payments.dw_transaction_details.getItemString(ll_row, 'recipient_type_code')
	//First get the recipient no, this is the same as the individual_no in INDIVIDUAL table 
	ll_recipient_no = uo_payments.dw_transaction_details.getItemNumber(ll_row,'recipient_no')
	if ll_recipient_no < 1 then
		uo_payments.dw_transaction_details.ScrollToRow(ll_row)
		Messagebox("Save Error", "The recipient number is not a valid number") 
		return -1
	END IF
	
	// Next, if this is a Direct Deposit payment, check that the bank information has not been removed 
	// (another user might have removed it since this window was opened)
	ls_payment_method_code = uo_payments.dw_transaction_details.getItemString(ll_row, 'payment_method_code')
	IF ls_payment_method_code = 'D' THEN // yes, its a Direct Deposit
		IF wf_verify_bank_info(ll_recipient_no,  ls_recipient_type_code) < 0 THEN
			uo_payments.dw_transaction_details.ScrollToRow(ll_row)
			Messagebox("Save Error", "This recipients banking information has been removed. You cannot save a Direct Deposit payment at this time. Choose another payment method type.") 
			RETURN -1
		END IF
		
		// Update the bank information, just in case it has been modified by another user 
		uo_payments.dw_transaction_details.setitem (ll_row,'bank_no', ls_bank_no)
		uo_payments.dw_transaction_details.setitem (ll_row,'bank_transit_no', ls_bank_transit_no)
		uo_payments.dw_transaction_details.setitem (ll_row,'bank_account_no', ls_bank_account_no)
	END IF
	
	// Now refresh address information
	// check the 'use_default_address_flag, proceed if it is a 'Y'
	ls_use_default_address = uo_payments.dw_transaction_details.getItemString(ll_row, 'use_default_address_flag')
	
	IF ls_use_default_address = 'Y' THEN
		ls_cheque_print_group_code = uo_payments.dw_transaction_details.getItemString(ll_row, 'cheque_print_group_code')
				
		// function sets up adress information for either individual or other types of providers
		IF inv_payments.nf_setup_address(ls_recipient_type_code, ll_recipient_no, ll_row, ls_cheque_print_group_code) <0 THEN
			uo_payments.dw_transaction_details.ScrollToRow(ll_row)
			return -1
		END IF
	END IF
NEXT

RETURN 0

end function

public function integer wf_verify_bank_info (long al_recipient_no, string as_recipient_type_code);STRING ls_bank_no, ls_bank_transit_no, ls_bank_account_no

IF as_recipient_type_code = 'I' THEN
	
	SELECT bank_no, bank_transit_no, bank_account_no 
	INTO        :ls_bank_no, :ls_bank_transit_no, :ls_bank_account_no
	FROM      INDIVIDUAL
	WHERE  individual_no = :al_recipient_no;

	SQLCA.nf_handle_error("w_payments","wf_verify_bank_info","SELECT bank_no, bank_transit_no, bank_account_no") 
	
ELSE
		
	SELECT bank_no, bank_transit_no, bank_account_no
	INTO       :ls_bank_no, :ls_bank_transit_no, :ls_bank_account_no
	FROM     BANK_INFO a 
	RIGHT OUTER JOIN PROVIDER b ON a.recipient_no = b.provider_no 
														AND a.recipient_type_code = b.provider_type_code
	WHERE  b.provider_no = :al_recipient_no
	AND         b.provider_type_code = :as_recipient_type_code
	USING   SQLCA;

	IF SQLCA.nf_handle_error("w_payments","wf_verify_bank_info","SELECT bank_no, bank_transit_no, bank_account_no") < 0 THEN
		RETURN -1
	END IF

END IF

IF (IsNull(ls_bank_no) OR ls_bank_no = '' ) OR (IsNull(ls_bank_transit_no) OR ls_bank_transit_no = '') OR (IsNull(ls_bank_account_no) OR ls_bank_account_no = '') THEN
	RETURN -1
END IF
	
RETURN 0
end function

event open;call super::open;U_DWA   	ldw_dw[]
STRING		ls_claim_admin_region

ids_reminders = create datastore
ids_reminders.dataobject = 'ds_planned_claim_reminders'
ids_reminders.settransobject(sqlca)

ids_auto_reminder = create datastore
ids_auto_reminder.dataobject = 'ds_claim_reminder'
ids_auto_reminder.settransobject(sqlca)

dw_payment_list.SetTransObject(SQLCA)
dw_payment_list.uf_setselect(1)
uo_payments.dw_basic_claim.SetTransObject(SQLCA)

il_claim_no = iw_active_sheet.dw_basic_claim.GetItemNumber(1,'claim_no')
il_individual_no = iw_active_sheet.dw_basic_claim.GetItemNumber(1,'individual_no')
ls_claim_admin_region = iw_active_sheet.dw_basic_claim.GetItemString(1,'admin_region_code')

//Check authorizations
If gnv_user_authorizations.nf_user_has_module_authorizations(ls_claim_admin_region ,'001') = False THen
	MessageBox('No Authorizations','You do not have the necessary authorizations to use this module.')
	CLOSE(tHIS)
	RETURN
END IF

uo_payments.dw_basic_claim.Retrieve(il_claim_no)


/* change this to get claim number from the dw on the sheet */
	istr_message = Message.PowerObjectParm

	inv_payments = Create n_payment
	inv_payments.nf_set_window_parent(THIS)
	
	ldw_dw[1] = uo_payments.dw_payment_details
	ldw_dw[2] = uo_payments.dw_transaction_details
	ldw_dw[3] = uo_payments.dw_rtw_incentive_payment_xref // not visible, used as datastore
	ldw_dw[4] = uo_payments.dw_authorization_groups

	inv_payments.nf_init(ldw_dw[],SQLCA,THIS)
	inv_payments.nf_set_commit(TRUE)

	//reminders will check/create any outstanding claim reminders
	inv_reminders = create n_reminders
	

	THIS.PostEvent('ue_postopen')
	
end event

on w_payments.create
int iCurrent
call super::create
this.dw_payment_list=create dw_payment_list
this.cb_save=create cb_save
this.cb_add_recipient=create cb_add_recipient
this.cb_next=create cb_next
this.cb_prev=create cb_prev
this.st_cancelled=create st_cancelled
this.cb_add=create cb_add
this.cb_delete=create cb_delete
this.cb_cancel=create cb_cancel
this.uo_payments=create uo_payments
this.sle_reminder=create sle_reminder
this.sle_overpayment=create sle_overpayment
this.st_save=create st_save
this.cb_reminders=create cb_reminders
this.cb_delete_recipient=create cb_delete_recipient
this.cb_group=create cb_group
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_payment_list
this.Control[iCurrent+2]=this.cb_save
this.Control[iCurrent+3]=this.cb_add_recipient
this.Control[iCurrent+4]=this.cb_next
this.Control[iCurrent+5]=this.cb_prev
this.Control[iCurrent+6]=this.st_cancelled
this.Control[iCurrent+7]=this.cb_add
this.Control[iCurrent+8]=this.cb_delete
this.Control[iCurrent+9]=this.cb_cancel
this.Control[iCurrent+10]=this.uo_payments
this.Control[iCurrent+11]=this.sle_reminder
this.Control[iCurrent+12]=this.sle_overpayment
this.Control[iCurrent+13]=this.st_save
this.Control[iCurrent+14]=this.cb_reminders
this.Control[iCurrent+15]=this.cb_delete_recipient
this.Control[iCurrent+16]=this.cb_group
end on

on w_payments.destroy
call super::destroy
destroy(this.dw_payment_list)
destroy(this.cb_save)
destroy(this.cb_add_recipient)
destroy(this.cb_next)
destroy(this.cb_prev)
destroy(this.st_cancelled)
destroy(this.cb_add)
destroy(this.cb_delete)
destroy(this.cb_cancel)
destroy(this.uo_payments)
destroy(this.sle_reminder)
destroy(this.sle_overpayment)
destroy(this.st_save)
destroy(this.cb_reminders)
destroy(this.cb_delete_recipient)
destroy(this.cb_group)
end on

event closequery;call super::closequery;long ll_up
int li_rtn

IF NOT isvalid(inv_payments) THEN return 0

ll_up = inv_payments.nf_flagged_ungrouped(il_claim_no)

IF ll_up =1  THEN
	
	MessageBox('Ungrouped Payments','A payment has been flagged for Authorization Grouping, ' &
	                    + 'but is not assigned to an Authorization Group. ~r~n~r~nThe payment is not authorized and ' &
					   +  'will not be processed until it has been Grouped for Authorization.', Exclamation!)
ELSEIF ll_up > 1  THEN
	li_rtn = MessageBox('Ungrouped Payments','There are payments that have been flagged for Authorization Grouping, ' &
	                           + 'but have not yet been assigned to an Authorization Group.' &
							  + '~r~n~r~nWould you like to Group them now?', Question!, YesNo!, 1)
							  
	IF li_rtn = 1 THEN
		cb_group.postevent(Clicked!) 
		return 1
	ELSE
		return 0
	END IF
END IF	

end event

type st_title from w_a_tool`st_title within w_payments
integer y = 0
integer width = 2839
integer height = 80
string text = "Payments "
end type

type cb_close from w_a_tool`cb_close within w_payments
integer x = 2258
integer y = 1732
integer taborder = 10
end type

event cb_close::clicked;/*  EdL  begin changes   1999/06/17 
	    display 'save needed' text    */

SetPointer(HourGlass!)
IF parent.cb_save.enabled = TRUE THEN
	messagebox("New Payment","You have been working on a payment - Save or Cancel before proceeding")
ELSE
	Close(parent)
END IF

/*  EdL   end changes   1999/06/17    */
end event

type dw_payment_list from u_dw_online within w_payments
integer x = 32
integer y = 84
integer width = 2825
integer height = 300
integer taborder = 20
string dataobject = "d_payment_list_unapplied"
boolean vscrollbar = true
boolean livescroll = false
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;call super::rowfocuschanged;Long    ll_listrow, li_opening_no, 	ll_batch_no
Integer li_benefit_calculation_no, li_rtn
Decimal lc_daily_rate, lc_paid_hours_lost, lc_day_amount, lc_hour_amount
Decimal lc_hourly_rate, lc_paid_days_lost,lc_benefit_level_percentage
String  ls_payment_type_code, ls_payment_method_code, ls_recipient_type_code
String  ls_payment_type_desc, ls_payment_sub_type_desc
Date    ld_processed_date, ld_order_date
LONG		ll_award_no


IF this.RowCount() = 0 THEN
	cb_delete.enabled = FALSE
	Return
END IF

ll_listrow = this.GetRow()
this.SelectRow(0,false)
this.SelectRow(ll_listrow,true)

li_opening_no					= this.GetItemNumber(ll_listrow,"opening_no")
li_benefit_calculation_no 	= this.GetItemNumber(ll_listrow,"benefit_calculation_no")
ld_processed_date         	= Date(this.GetItemDateTime(ll_listrow,"processed_date"))
ld_order_date					= Date(this.GetItemDateTime(ll_listrow,"scheduled_processing_date"))
ll_award_no                = this.GetItemNumber(ll_listrow,'award_no')

IF isNull(ld_processed_date ) and ll_award_no <> 0 Then
	MessageBox('Warning','This payment originated from an Award. You should only be modifying this record to correct whatever caused it to be rejected.',Exclamation!,Ok!)
End if


//	Retrieve and display payment details                                                                 
il_payment_no = this.GetItemNumber(ll_listrow,"payment_no")
inv_payments.nf_retrieve(il_payment_no)

IF inv_payments.nf_setup_benefit_info(li_benefit_calculation_no,lc_daily_rate,lc_hourly_rate,lc_benefit_level_percentage) < 0 THEN
	//	changed this to signal an error.  If you try and close the parent some of the other scripts
	// still run and it causes a null object reference
	// this error should not happen because you cannot delete bencals if there are payments, however in the old system
	// this was possible so there is still some data that is bad
	Error.Text = 'Missing bencalc for: ' + string(il_claim_no) + ' opening/bencalc : ' + String(li_opening_no) + '/' + String(li_benefit_calculation_no)
	Error.WindowMenu="cmwb"
	Error.Object="w_payments"
	Error.ObjectEvent="rowfocuschanged for dw_payment_list"
	SignalError()
	//	Close(parent)
	Return
END IF

lc_paid_days_lost    = this.GetItemDecimal(ll_listrow,"paid_days_lost")
lc_paid_hours_lost   = this.GetItemDecimal(ll_listrow,"paid_hours_lost")
lc_day_amount        = lc_daily_rate * lc_paid_days_lost
lc_hour_amount       = lc_hourly_rate * lc_paid_hours_lost

// Load the display only fields
SELECT PT.payment_type_desc, PST.payment_sub_type_desc  
  INTO :ls_payment_type_desc, :ls_payment_sub_type_desc 
  FROM PAYMENT P,
		 Payment_Type PT, 
		 Payment_Sub_Type PST 
 WHERE payment_no = :il_payment_no 
	AND P.payment_type_code = PT.payment_type_code 
	AND P.payment_type_code = PST.payment_type_code 
	AND P.payment_sub_type_code = PST.payment_sub_type_code ;

li_rtn = SQLCA.nf_handle_error('','w_payments', 'dw_payment_list - rowfocuschanged: SELECT PT.payment_type_desc, PST.payment_sub_type_desc')

IF ls_payment_sub_type_desc = "" OR IsNull(ls_payment_sub_type_desc) THEN
	uo_payments.dw_payment_details.SetItem(1,"payment_type_description", ls_payment_type_desc)
ELSE
	uo_payments.dw_payment_details.SetItem(1,"payment_type_description", ls_payment_type_desc + " - " + ls_payment_sub_type_desc)
END IF
uo_payments.dw_payment_details.SetItem(1,"nmbr_cycles",1)
uo_payments.dw_payment_details.SetItem(1,"day_amount",lc_day_amount)
uo_payments.dw_payment_details.SetItem(1,"hour_amount",lc_hour_amount)
uo_payments.dw_payment_details.SetItem(1,"scheduled_processing_date",ld_order_date)

// Check to see if this is a receiving salary transaction										
IF uo_payments.dw_transaction_details.GetItemString(1,"payment_method_code") = "R" THEN

	cb_next.Hide()
	cb_prev.Hide()
	cb_add_recipient.Hide()
ELSE
	
	cb_next.Show()
	cb_prev.Show()
	cb_add_recipient.Show()
END IF

// Check to see if transaction has been canceled																		  
IF uo_payments.dw_transaction_details.RowCount() > 0 THEN
	IF uo_payments.dw_transaction_details.GetItemString(1,"canceled_txn_flag") = "Y" THEN
		st_cancelled.Visible = TRUE
	ELSE
		st_cancelled.Visible =FALSE
	END IF
ELSE
	st_cancelled.Visible =FALSE
	Return
END IF

ls_payment_type_code   = this.GetItemString(ll_listrow,"payment_type_code")
ls_payment_method_code = uo_payments.dw_transaction_details.GetItemString(1,"payment_method_code")
ls_recipient_type_code = uo_payments.dw_transaction_details.GetItemString(1,"recipient_type_code")

inv_payments.nf_tab_order("payment_type_code",ls_payment_type_code)
inv_payments.nf_tab_order("recipient_type_code",ls_recipient_type_code)
inv_payments.nf_tab_order("payment_method_code",ls_payment_method_code)
ll_batch_no      = uo_payments.dw_transaction_details.GetItemNumber(1,"batch_no")

wf_enable_disable_dw()

end event

event constructor;/* Overridden for performance reasons.  No column level security needed. */
end event

type cb_save from commandbutton within w_payments
integer x = 1143
integer y = 1732
integer width = 366
integer height = 100
integer taborder = 130
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
end type

event clicked;INTEGER		li_rtn, li_trancount
string			ls_sCode,ls_sType
LONG          ll_group_no
N_PROCESS_RUN_STATUS	ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '001' refers to the Payment Maintenance module, '044' refers to the Payment Processing module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('001','044','save',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF
/******************************************************************************************/


SetPointer(HourGlass!)

uo_payments.dw_payment_details.accepttext()

/* PR 1414 Tyler Craft Sept 10/2001
	Added If statement to check database for a change in status from instance datastore.*/
IF inv_payments.nf_check_claim_status(il_claim_no) THEN
	ls_sCode = iw_active_sheet.dw_basic_claim.getitemstring(iw_active_sheet.dw_basic_claim.getrow(),"claim_status_code")
	ls_stype = iw_active_sheet.dw_basic_claim.getitemstring(iw_active_sheet.dw_basic_claim.getrow(),"claim_status_type_code")
	IF inv_payments.nf_refresh_claim_status(il_claim_no,ls_sCode,ls_sType) then
		/* PR 5434 and 5208, 2006/02/03. 
		If statement calls new function wf_refresh_individual() to check individual information. */
		/*P10151-140 - Function name has been changed from wf_refresh_individual() to wf_refresh_recipient()*/
		IF wf_refresh_recipient() >= 0 THEN

			
			SQLCA.nf_begin_transaction()
			
			IF inv_payments.nf_save() >= 0 THEN								
				SQLCA.nf_commit_transaction()

				cb_save.enabled = FALSE
				cb_cancel.enabled = FALSE
				cb_group.Enabled = TRUE
				cb_add.enabled = TRUE
				wf_retrieve_list()
				dw_payment_list.enabled = TRUE
				dw_payment_list.SetFocus()
				
				/*  EdL  begin changes   1999/06/17 
					 remove  'save needed' text      */
				st_save.visible = FALSE
				/*  EdL   end changes   1999/06/17  */
			ELSE
				SQLCA.nf_transaction_count(li_trancount,0,this.classname(),'','',FALSE)
				IF li_trancount > 0 THEN
					SQLCA.nf_rollback_transaction()
				END IF
			END IF
		END IF
	ELSE
		messagebox("Claim Status","The status for this claim has changed. Please refresh data and try again.")
	END IF
ELSE
	messagebox("Claim Status","The status for this claim has changed. Please refresh data and try again.")
END IF
uo_payments.dw_payment_details.SetFocus()
end event

type cb_add_recipient from commandbutton within w_payments
integer x = 1431
integer y = 1648
integer width = 421
integer height = 76
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Add Recipient"
end type

event clicked;IF uo_payments.dw_payment_details.GetItemString(1,'payment_type_code')='S1' THEN
	cb_add_recipient.Enabled = FALSE
	cb_next.Enabled = FALSE
	cb_prev.Enabled = FALSE
	MessageBox("WARNING:","Survivor's Special Payments can only have one recipient",Information!)
ELSE	
	IF inv_payments.nf_insert_recipient(0) > 0 THEN
		cb_cancel.Enabled = TRUE
		cb_next.Enabled = TRUE
		cb_prev.Enabled = TRUE
	END IF	
END IF
end event

type cb_next from commandbutton within w_payments
integer x = 2336
integer y = 1648
integer width = 270
integer height = 76
integer taborder = 70
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Next >>"
end type

on clicked;LONG    ll_tranrow
STRING  ls_recipient_type_code,  ls_payment_method_code

/*	Check to see if we are already on the last row
*/

	ll_tranrow = uo_payments.dw_transaction_details.GetRow()
	IF ll_tranrow = uo_payments.dw_transaction_details.RowCount() THEN
		Return
	END IF

	IF uo_payments.dw_transaction_details.AcceptText() < 0 THEN
		uo_payments.dw_transaction_details.SetFocus()
		Return
	END IF

long ll_temp
ll_temp = uo_payments.dw_transaction_details.RowCount()
ll_temp =	uo_payments.dw_transaction_details.ScrollToRow(ll_tranrow+1 )
ll_temp = uo_payments.dw_transaction_details.GetRow()
/*	Check to see if transaction has been canceled
*/

	IF uo_payments.dw_transaction_details.GetItemString(ll_tranrow ,"canceled_txn_flag") = "Y" THEN
		st_cancelled.Visible = TRUE
	ELSE
		st_cancelled.Visible = FALSE
	END IF

	ls_recipient_type_code = uo_payments.dw_transaction_details.GetItemString(ll_tranrow+1,"recipient_type_code")
	inv_payments.nf_tab_order("recipient_type_code",ls_recipient_type_code)

	ls_payment_method_code = uo_payments.dw_transaction_details.GetItemString(ll_tranrow+1,"payment_method_code")
	inv_payments.nf_tab_order("payment_method_code",ls_payment_method_code)

end on

type cb_prev from commandbutton within w_payments
integer x = 1157
integer y = 1648
integer width = 270
integer height = 76
integer taborder = 80
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "<< &Prev "
end type

on clicked;LONG    ll_tranrow
STRING  ls_recipient_type_code, ls_payment_method_code


	ll_tranrow=uo_payments.dw_transaction_details.GetRow()
	IF ll_tranrow = 1 THEN
		return
	END IF

	IF uo_payments.dw_transaction_details.AcceptText() < 0 THEN
		uo_payments.dw_transaction_details.SetFocus()
		Return
	END IF

/*	Move to the previous row and set up the screen accordingly
*/
	uo_payments.dw_transaction_details.ScrollToRow(ll_tranrow - 1)

/*	Check to see if transaction has been canceled
*/
	IF uo_payments.dw_transaction_details.GetItemString(ll_tranrow - 1,"canceled_txn_flag") = "Y" THEN
		st_cancelled.Visible = TRUE
	ELSE
		st_cancelled.Visible =FALSE
	END IF

	ls_recipient_type_code = uo_payments.dw_transaction_details.GetItemString(ll_tranrow - 1,"recipient_type_code")
	inv_payments.nf_tab_order("recipient_type_code",ls_recipient_type_code)

	ls_payment_method_code = uo_payments.dw_transaction_details.GetItemString(ll_tranrow - 1,"payment_method_code")
	inv_payments.nf_tab_order("payment_method_code",ls_payment_method_code)

end on

type st_cancelled from statictext within w_payments
boolean visible = false
integer x = 905
integer y = 1548
integer width = 379
integer height = 84
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16711680
long backcolor = 67108864
boolean enabled = false
string text = "Cancelled"
alignment alignment = center!
boolean focusrectangle = false
end type

type cb_add from commandbutton within w_payments
integer x = 32
integer y = 1732
integer width = 366
integer height = 100
integer taborder = 110
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Add"
end type

event clicked;	SetPointer(HourGlass!)
	
	IF inv_payments.nf_insert(0) < 0 THEN
		Return
	END IF

	dw_payment_list.enabled = FALSE
	dw_payment_list.SelectRow(0,FALSE)
	cb_cancel.enabled = TRUE
	cb_save.enabled = TRUE
	cb_group.Enabled = FALSE
	cb_add.enabled = FALSE
	wf_enable_disable_dw()

	cb_delete.enabled = FALSE
   st_cancelled.Visible =FALSE

	/*  EdL  begin changes   1999/06/17 
	    display 'save needed' text       */
	st_save.visible = TRUE
	/*  EdL   end changes   1999/06/17    */
	
/* Check to see if this is a receiving salary transaction										
*/
	IF uo_payments.dw_transaction_details.GetItemString(1,"payment_method_code") = "R" THEN
		cb_next.Hide()
		cb_prev.Hide()
		cb_add_recipient.Hide()
	ELSE
		cb_next.Show()
		cb_prev.Show()
		cb_add_recipient.Show()
	END IF

	uo_payments.dw_payment_details.SetFocus()
end event

type cb_delete from commandbutton within w_payments
integer x = 402
integer y = 1732
integer width = 366
integer height = 100
integer taborder = 100
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Delete"
end type

event clicked;INTEGER	li_rtn
LONG		ll_rows
N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '001' refers to the Payment Maintenance module, '044' refers to the Payment Processing module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('001','044','delete',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF
/******************************************************************************************/


// confirm deletion???

	SetPointer(HourGlass!)
	IF MessageBox('Confirm Deletion','Are you sure you want to delete this payment?',Question!,YesNo!) = 1 THEN
		IF inv_payments.nf_delete_payment() < 0 THEN
		ELSE
			ll_rows = wf_retrieve_list()
			IF ll_rows <= 0 THEN
				cb_delete.enabled = FALSE
			END IF
			cb_cancel.Enabled = FALSE
			cb_save.Enabled = FALSE
			cb_group.Enabled = TRUE
		END IF
	END IF
end event

type cb_cancel from commandbutton within w_payments
integer x = 1513
integer y = 1732
integer width = 366
integer height = 100
integer taborder = 90
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Cance&l"
end type

event clicked;	SetPointer(HourGlass!)
/*	retrieve the last payment worked on
*/
/*	if the list is now empty then the payment number should be 0
	This is because if you delete the payments it still had the old payment number
	hanging around and it made it retrieve incorrectly
*/
	IF dw_payment_list.RowCount() = 0 THEN
		il_payment_no = 0
		cb_add_recipient.Enabled = FALSE
		cb_delete_recipient.Enabled = False
		cb_next.Enabled = FALSE
		cb_prev.Enabled = FALSE
	END IF
	inv_payments.nf_retrieve(il_payment_no)
	dw_payment_list.enabled = TRUE
	IF wf_retrieve_list() > 0 THEN
		dw_payment_list.uf_processselect(1,'KeyBoard')
		wf_enable_disable_dw()
	END IF
	cb_save.enabled = FALSE
	cb_cancel.enabled = FALSE
	cb_group.Enabled = TRUE
	cb_add.enabled = TRUE
	
	/*  EdL  begin changes   1999/06/17 
	    remove  'save needed' text      */
	st_save.visible = FALSE
	/*  EdL   end changes   1999/06/17   */
	
Return
end event

type uo_payments from u_payment within w_payments
integer x = 14
integer y = 380
integer height = 1348
integer taborder = 50
end type

event ue_payment_changes;call super::ue_payment_changes;	long		ll_return
	
	ll_return = inv_payments.nf_change_item(1)
	cb_save.enabled = TRUE
	cb_cancel.enabled = TRUE
	cb_group.Enabled = FALSE
	IF inv_payments.is_claim_receiving_salary_flag = 'N' THEN
		cb_add_recipient.visible = TRUE
		cb_next.visible = TRUE
		cb_prev.visible = TRUE
	ELSE
		cb_add_recipient.visible = FALSE
		cb_next.visible = FALSE
		cb_prev.visible = FALSE
	END IF

return ll_return
end event

event ue_txn_changes;call super::ue_txn_changes;integer li_return


cb_save.enabled = TRUE
cb_cancel.enabled = TRUE
cb_group.Enabled = FALSE

return inv_payments.nf_change_item(2)
end event

on uo_payments.destroy
call u_payment::destroy
end on

type sle_reminder from singlelineedit within w_payments
integer x = 23
integer y = 4
integer width = 1019
integer height = 64
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16711680
long backcolor = 67108864
boolean border = false
boolean autohscroll = false
end type

type sle_overpayment from singlelineedit within w_payments
integer x = 2199
integer y = 4
integer width = 443
integer height = 64
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 255
long backcolor = 67108864
boolean border = false
boolean autohscroll = false
boolean hideselection = false
end type

type st_save from statictext within w_payments
boolean visible = false
integer x = 1765
integer y = 4
integer width = 667
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 255
long backcolor = 67108864
boolean enabled = false
string text = "Save needed"
alignment alignment = center!
long bordercolor = 16777215
boolean focusrectangle = false
end type

type cb_reminders from commandbutton within w_payments
integer x = 773
integer y = 1732
integer width = 366
integer height = 100
integer taborder = 120
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Reminders"
end type

on clicked;	SetPointer(HourGlass!)
	OpenWithParm(w_reminder,il_claim_no)
end on

type cb_delete_recipient from commandbutton within w_payments
integer x = 1856
integer y = 1648
integer width = 475
integer height = 76
integer taborder = 100
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Delete Recipient"
end type

event clicked;INTEGER		li_rtn

li_rtn = inv_payments.nf_delete_recipient()

IF li_rtn < 1 THEN RETURN

cb_save.enabled = TRUE
cb_cancel.enabled = True
cb_group.Enabled = FALSE
cb_add.enabled = False
	



end event

type cb_group from commandbutton within w_payments
integer x = 1883
integer y = 1732
integer width = 366
integer height = 100
integer taborder = 100
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Group"
end type

event clicked;LONG ll_wait

OpenWithParm(w_authorization_grouping, il_claim_no, PARENT)

ll_wait = Message.DoubleParm

IF ll_wait = 1 THEN
	wf_retrieve_list()
END IF

end event

