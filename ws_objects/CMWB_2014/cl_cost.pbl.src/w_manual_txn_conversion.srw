$PBExportHeader$w_manual_txn_conversion.srw
forward
global type w_manual_txn_conversion from w_ancestor
end type
type st_conversion_no from statictext within w_manual_txn_conversion
end type
type cb_match from commandbutton within w_manual_txn_conversion
end type
type st_conversion_details from statictext within w_manual_txn_conversion
end type
type dw_conversion from u_dw_online within w_manual_txn_conversion
end type
type st_splitbar_2 from u_splitbar_vertical within w_manual_txn_conversion
end type
type st_splitbar_1 from u_splitbar_horizontal within w_manual_txn_conversion
end type
type pb_remove from picturebutton within w_manual_txn_conversion
end type
type pb_add from picturebutton within w_manual_txn_conversion
end type
type dw_selected_payments from u_dw_online within w_manual_txn_conversion
end type
type uo_filter_payments from u_filter_control within w_manual_txn_conversion
end type
type dw_manual from u_dw_online within w_manual_txn_conversion
end type
type dw_payment_list from u_dw_online within w_manual_txn_conversion
end type
type st_conversion_list from statictext within w_manual_txn_conversion
end type
end forward

global type w_manual_txn_conversion from w_ancestor
integer width = 4507
integer height = 2616
string title = "Manual Transaction Conversion"
string menuname = "m_convert_manual"
windowtype windowtype = main!
long backcolor = 67108864
string icon = "AppIcon!"
long il_design_time_height = 2616
long il_design_time_width = 4507
event ue_postopen ( )
st_conversion_no st_conversion_no
cb_match cb_match
st_conversion_details st_conversion_details
dw_conversion dw_conversion
st_splitbar_2 st_splitbar_2
st_splitbar_1 st_splitbar_1
pb_remove pb_remove
pb_add pb_add
dw_selected_payments dw_selected_payments
uo_filter_payments uo_filter_payments
dw_manual dw_manual
dw_payment_list dw_payment_list
st_conversion_list st_conversion_list
end type
global w_manual_txn_conversion w_manual_txn_conversion

type variables

u_ds				ids_ultimate_payment
u_ds				ids_payment_list_selected

windowstate		iws_frame_open_state


//conversion status
CONSTANT		INTEGER		CONV_EMPTY		= 0
CONSTANT    INTEGER		CONV_NEW			= 1
CONSTANT    INTEGER		CONV_MODIFIED  = 2
CONSTANT    INTEGER		CONV_NOT_MODIFIED = 3

INTEGER		ii_CONVERSION_STATUS
INTEGER		ii_conversion_no

LONG			il_mousemove

BOOLEAN		ib_payment_list_drag
BOOLEAN		ib_selected_payment_drag



BOOLEAN			ib_allow_rowfocuschange
end variables

forward prototypes
public function integer wf_get_next_conversion_no ()
public function integer wf_removed_selected_from_payment_list ()
public function integer wf_retrieve_payment_list ()
public function integer wf_retrieve_ultimate_payment (long al_payment_no)
public function integer wf_check_conversion_status ()
public function integer wf_suggest_a_match ()
public subroutine wf_clear_conversion ()
public function integer wf_retrieve_manual_txn ()
public function integer wf_retrieve_conversion_list ()
public function integer wf_delete_conversion ()
public function integer wf_cancel ()
public function integer wf_new_conversion (long al_ultimate_payment_no)
public function integer wf_check_module_rules ()
public function integer wf_refresh_selected_grouping ()
public function integer wf_load_conversion (long al_conversion_no)
public function integer wf_remove_selected_payments ()
public function integer wf_retrieve_selected_payments (long al_conversion_no)
public function integer wf_convert ()
public function integer wf_add_selected_payments ()
public function integer wf_save (boolean ab_show_message)
end prototypes

event ue_postopen();
wf_retrieve_conversion_list()

dw_conversion.uf_protectcolumn('new_txn_sub_type_code',"new_txn_type_code = 'J' and new_txn_sub_type_code in('','2','3')")


wf_new_conversion(0)



end event

public function integer wf_get_next_conversion_no ();LONG	ll_next_conversion_no
	
UPDATE Last_Ntr_Conversion_No
SET    last_ntr_conversion_no = last_ntr_conversion_no + 1
USING SQLCA;

SQLCA.nf_handle_error('w_manual_txn_conversion','wf_get_next_conversion_no','UPDATE Last_Manual_Conversion_No')


SELECT last_ntr_conversion_no INTO :ll_next_conversion_no
FROM Last_Ntr_Conversion_No
USING SQLCA;

SQLCA.nf_handle_error('w_manual_txn_conversion','wf_get_next_conversion_no','SELECT last_manual_conversion_no INTO :ll_next_conversion_no')


RETURN ll_next_conversion_no
end function

public function integer wf_removed_selected_from_payment_list ();LONG		ll_txn_no
LONG		ll_found_row
INTEGER	li_x


For li_x = 1 to dw_selected_payments.RowCount()
	ll_txn_no = dw_selected_payments.GetItemNumber(li_x,'txn_no')
	ll_found_row = dw_payment_list.Find('txn_no = ' + String(ll_txn_no),1,dw_payment_list.RowCount())
	
	IF ll_found_row <= 0 Then
		SignalError(-666,'Error finding selected payment to remove from list.')
	End if
	
	
	//I had to use the RowsCopy and RowsDiscard combination because RowsMove
	//cause the rowfocuschanged event to fire twice and it reported the wrong currentrow 
	//when zero rows were left in the datawindow
	dw_payment_list.RowsCopy(ll_found_row,ll_found_row,Primary!,ids_payment_list_selected,1,Primary!)
	dw_payment_list.RowsDiscard(ll_found_row,ll_found_row,Primary!)

	
Next

RETURN 1
end function

public function integer wf_retrieve_payment_list ();LONG		ll_rows
LONG		ll_claim_no
DATETIME	ldt_manual_processed_date
STRING	ls_payment_message_group_code


dw_payment_list.SetFilter('')
dw_payment_list.Filter()

ll_claim_no 						= ids_ultimate_payment.GetItemNumber(1,'claim_no')
ls_payment_message_group_code = ids_ultimate_payment.GetItemString(1,'payment_message_group_code')
ldt_manual_processed_date     = dw_manual.GetItemDateTime(dw_manual.GetRow(),'processed_date')


ll_rows = dw_payment_list.retrieve(ll_claim_no,ldt_manual_processed_date,ls_payment_message_group_code)
SQLCA.nf_handle_error("dw_list_payments","w_manual_txn_conversion","w_manual_txn_conversion")


IF ll_rows <= 0 Then
	SignalError(-666,'Error retrieving payment list')
END IF

//This must be reset because it may contain records from another conversion.
ids_payment_list_selected.Reset()

RETURN ll_rows
end function

public function integer wf_retrieve_ultimate_payment (long al_payment_no);long		ll_rows
LONG		ll_count


SELECT count(*) into :ll_count
FROM PAYMENT
WHERE payment_no = :al_payment_no
USING SQLCA;

SQLCA.nf_handle_error('w_manual_txn_conversion','wf_search','Retrieve ultimate payment info.')

IF SQLCA.SQLNRows = 0 THEN
	MessageBox('Invalid payment','The payment number you entered does not exist.')
	RETURN -1
END IF


//Retrieve information about the ultimate payment so we can retrieve only the
//manual txns that need to be converted.
ll_rows = ids_ultimate_payment.Retrieve(al_payment_no)
SQLCA.nf_handle_error('w_manual_txn_conversion','wf_search','Retrieve ultimate payment info.')

If ll_rows = 0 Then		
	MessageBox('Invalid payment','The payment you entered cannot be maintained in this module.')
	RETURN -1
elseif ll_rows < 0 Then
	SignalError(-666,'Error retrieving payment information.')
Elseif ll_rows > 1 Then
	SignalError(-666,'One row was not returned when retrieving payment.')
End if

RETURN 1
end function

public function integer wf_check_conversion_status ();
dw_conversion.AcceptText()
dw_selected_payments.AcceptText()

IF ii_CONVERSION_STATUS = CONV_NEW OR ii_CONVERSION_STATUS = CONV_MODIFIED THEN
	MessageBox('Save','Save or Cancel changes to the current conversion before continuing.',StopSign!,Ok!)
	RETURN -1

END IF


return 1
end function

public function integer wf_suggest_a_match ();LONG			ll_payment_no[]
DECIMAL{2}     ldec_txn_amount[]
INTEGER		li_x
LONG			ll_found_row
n_payment_matching		lnv_payment_matching
DECIMAL{2}  ll_target_amount
string		ls_current_filter

SetRedraw(False)

//ls_current_filter = dw_payment_list.inv_filter.is_filter[dw_payment_list.inv_filter.ii_current_filter_position]

ls_current_filter = dw_payment_list.describe('datawindow.table.filter')
IF ls_current_filter = '?' THen
	ls_current_filter = ''
END IF

IF ls_current_filter <> '' THEN	
	dw_payment_list.SetFilter(ls_current_filter + " and selectable_flag = 'Y'")
ELSE	
	dw_payment_list.SetFilter("selectable_flag = 'Y'")
END IF
	
dw_payment_list.Filter()

//Populate the arrays
For li_x = 1 to dw_payment_list.RowCount()
	
	ll_payment_no[li_x]   = dw_payment_list.GetItemNumber(li_x,'payment_no')
	ldec_txn_amount[li_x] = dw_payment_list.GetItemDecimal(li_x,'txn_amount')

Next

ll_target_amount = Abs(dw_manual.GetItemDecimal(dw_manual.GetRow(),'txn_amount'))

//Call recursive function
lnv_payment_matching = create n_payment_matching

//lnv_payment_matching.il_source_payment_list = ll_payment_no
lnv_payment_matching.idec_source_payment_amount = ldec_txn_amount
lnv_payment_matching.idec_target_amount = ll_target_amount
lnv_payment_matching.idec_target_variance = 0

dw_payment_list.SelectRow(0,False)

SetPointer(hourglass!)
IF lnv_payment_matching.of_start() = 1 Then
	
	For li_x = 1 To UpperBound(lnv_payment_matching.il_match_payment_list)		
		dw_payment_list.SelectRow(lnv_payment_matching.il_match_payment_list[li_x],True)
	Next
	MessageBox('Match','Your suggested match is displayed.')
Else
	MessageBox('No match','No match could be found.')
End if

dw_payment_list.SetFilter(ls_current_filter)
dw_payment_list.Filter()

SetRedraw(True)

RETURN 1
end function

public subroutine wf_clear_conversion ();dw_manual.Reset()
dw_payment_list.inv_filter.of_SetFilter('')
dw_payment_list.Reset()
dw_selected_payments.Reset()

ii_conversion_no = 0
ii_CONVERSION_STATUS = 0
st_conversion_no.text = ''
end subroutine

public function integer wf_retrieve_manual_txn ();LONG			ll_rows
LONG			ll_claim_no
LONG			ll_from_claim_no
STRING		ls_from_claim_no
LONG			ll_ultimate_payment_no
LONG			ll_positive_to_payment_no
DATETIME		ldt_manual_processed_date
DATETIME		ldt_processed_date //ultimate payment  processed date
STRING		ls_payment_message_group_code


ldt_processed_date 				= ids_ultimate_payment.GetItemDateTime(1,'processed_date')
ll_claim_no 						= ids_ultimate_payment.GetItemNumber(1,'claim_no')
ls_payment_message_group_code = ids_ultimate_payment.GetItemString(1,'payment_message_group_code')
ll_ultimate_payment_no        = ids_ultimate_payment.GetItemNumber(1,'payment_no')

ll_rows = dw_manual.Retrieve(ldt_processed_date,ll_claim_No,ls_payment_message_group_code)
SQLCA.nf_handle_error('w_manual_txn_conversion','wf_search','Retrieve manual transactions to convert.')

IF ll_rows = 0 Then
	//Find any claim transfers with a positive amount. These transaction will block the user
	//from maintaining the ultimate payment but they must convert them anyway.
	
	SELECT top 1 a.payment_no INTO :ll_positive_to_payment_no
	FROM APPLIED_CLAIM_TXN a,
	     PAYMENT b,
		  Module_Payment_Sub_Type c
	WHERE  a.payment_no = b.payment_no
	  and b.payment_type_code = c.payment_type_code 
	  and b.payment_sub_type_code = c.payment_sub_type_code
	  and c.module_code = '006'
	  and a.claim_no = :ll_claim_no
	  AND a.processed_date > :ldt_processed_date
	  AND c.payment_message_group_code = :ls_payment_message_group_code
	  AND a.txn_type_code = '2'
	USING SQLCA;
	
	SQLCA.nf_handle_error('w_manual_txn_conversion','wf_search','Retrieve manual transactions to convert.')	
	
	//Search for the "FROM" claim in the NTR_MANUAL_TRANSFER_PAIR table.
	SELECT from_claim_no INTO :ll_from_claim_no
	FROM NTR_MANUAL_TRANSFER_PAIR
	WHERE to_payment_no = :ll_positive_to_payment_no
	USING SQLCA;
	
	SQLCA.nf_handle_error('w_manual_txn_conversion','wf_search','SELECT from_claim_no INTO :ll_from_claim_no')	
	
	IF NOT ISNULL(ll_from_claim_no) THEN
		ls_from_claim_no = '#' +String(ll_from_claim_no)
	Else
		ls_from_claim_no = 'Unknown'
	END IF
	
	
	IF ll_positive_to_payment_no <> 0 AND NOT ISNULL(ll_positive_to_payment_no) THEN
		messagebox('Positive Transfer','There are no manual transactions to covert from within' &
		            + ' this claim but a "Transfer In" exists (payment_no #' &
						+ String(ll_positive_to_payment_no) + ') which must be converted in the' &
						+ ' "From Claim" (' + ls_from_claim_no + ') before the ultimate payment will be maintainable.')
		RETURN -1
	ELSE
		
		MessageBox('Conversion','There are no manual transactions that need to be converted. You should be able to maintain Payment #' + String(ll_ultimate_payment_no) + ' .' )
		RETURN -1
	END IF
elseif ll_rows < 0 Then
	SignalError(-666,'Error retrieving manual transactions.')
End if

//Select the manual transaction that was processed the ealiest
dw_manual.SetRow(ll_rows)
dw_manual.ScrollToRow(ll_rows)


RETURN ll_rows
end function

public function integer wf_retrieve_conversion_list ();
ib_allow_rowfocuschange = True

dw_conversion.Reset()

dw_conversion.Retrieve()
SQLCA.nf_handle_error('w_manual_txn_conversion','OPEN','dw_conversion.Retrieve()')

ib_allow_rowfocuschange = False

return 1
end function

public function integer wf_delete_conversion ();LONG			ll_rows
LONG			ll_selected_row
LONG			ll_conversion_no
INTEGER		li_index
INTEGER		li_dw_row
INTEGER		li_rtn


IF dw_conversion.RowCount() = 0 Then
	MessageBox('Cannot delete','There are no conversions to delete.')
	RETURN -1
END IF

ll_selected_row = dw_conversion.GetRow()


IF ii_CONVERSION_STATUS = CONV_NEW then
	wf_cancel()
	RETURN 1
END IF


li_rtn = MessageBox('Delete','Are you sure you want to delete conversion # ' + String(ii_conversion_no),Question!,YesNo!)
IF li_rtn = 2 Then
	RETURN 1
END IF


ll_conversion_no = dw_conversion.GetItemNumber(ll_selected_row,'ntr_conversion_no')

//IF the conversion is not new then delete it from the database
IF ll_conversion_no <> 0 Then

	SQLCA.nf_begin_transaction()

	//DELETE MANUAL_TXN_CONVERSION
	DELETE NTR_MANUAL_TXN_CONVERSION
	WHERE ntr_conversion_no = :ll_conversion_no
	USING SQLCA;
	
	SQLCA.nf_handle_error('w_manual_txn_conversion','wf_delete_conversion','DELETE MANUAL_TXN_CONVERSION')
	
	IF SQLCA.SQLNRows <> 1 Then
		SignalError(-666,'The wrong number of rows was deleted.')
	END IF
	
	
	
	//DELETE OLD_MANUAL_TXN
	DELETE NTR_OLD_MANUAL_TXN
	WHERE ntr_conversion_no = :ll_conversion_no
	USING SQLCA;
	
	SQLCA.nf_handle_error('w_manual_txn_conversion','wf_delete_conversion','DELETE OLD_MANUAL_TXN')
	
	IF SQLCA.SQLNRows < 1 OR SQLCA.SQLNRows > 2 Then
		SignalError(-666,'The wrong number of rows was deleted.')
	END IF
	
	
	//DELETE AFFECTED_TXN
	DELETE NTR_AFFECTED_TXN
	WHERE ntr_conversion_no = :ll_conversion_no
	USING SQLCA;
	
	SQLCA.nf_handle_error('w_manual_txn_conversion','wf_delete_conversion','DELETE OLD_MANUAL_TXN')
	
	IF SQLCA.SQLNRows < 1 Then
		SignalError(-666,'The wrong number of rows was deleted.')
	END IF
	
	SQLCA.nf_commit_transaction()

END IF

ii_CONVERSION_STATUS = CONV_EMPTY
ii_conversion_no = 0

wf_clear_conversion()

//Reset the window
wf_retrieve_conversion_list()



RETURN 1
end function

public function integer wf_cancel ();LONG			ll_conversion_row


ll_conversion_row = dw_conversion.GetRow()

IF ll_conversion_row = 0 THEN
	RETURN 0
END IF


wf_clear_conversion()

wf_retrieve_conversion_list()


RETURN 1
end function

public function integer wf_new_conversion (long al_ultimate_payment_no);LONG		ll_payment_no
LONG		ll_manual_row
LONG		ll_conversion_row
LONG		ll_claim_no
LONG		ll_found_row
LONG		ll_rows
LONG		ll_found_conversion_no 
LONG		ll_conversion_no
INTEGER	li_rtn
INTEGER	li_x
STRING	ls_new_txn_type_code
STRING	ls_new_txn_sub_type_code
STRING	ls_payment_message_group_code
STRING	ls_find
STRING	ls_explanation


IF wf_check_conversion_status() <> 1 THEN
	RETURN -1
END IF

//Prompt the user for a new ultimate_payment_no if zero is passed in.
IF al_ultimate_payment_no = 0 Then

	
	//Open the input window so the user can enter the payment number they can't maintain.
	Open(w_payment_no)
	al_ultimate_payment_no = Message.DoubleParm
	
	IF al_ultimate_payment_no = 0  or  IsNull(al_ultimate_payment_no) Then
		RETURN -1
	END IF
End if




//Get the ulimate payment information
IF wf_retrieve_ultimate_payment(al_ultimate_payment_no) <> 1 Then
	RETURN li_rtn
End if


ll_claim_no 						= ids_ultimate_payment.GetItemNumber(1,'claim_no')
ls_payment_message_group_code = ids_ultimate_payment.GetItemString(1,'payment_message_group_code')


//Make sure no un-converted conversions exists for the same claim and payment_message_group_code


//USE THIS ls_find once the payment_message_group_code is added to the table
ls_find = "claim_no = " + String(ll_claim_no) + " and ntr_payment_group_code = '" + ls_payment_message_group_code + "'"
 

ll_found_row = dw_conversion.Find(ls_find,1,dw_conversion.RowCount())

IF ll_found_row < 0 Then
	SignalError(-666,'Error finding row')
Elseif ll_found_row > 0 Then
	ll_found_conversion_no = dw_conversion.GetItemNumber(ll_found_row,'ntr_conversion_no')
	MessageBox('Conversion','Conversion #' + String(ll_found_conversion_no ) + ' already exists for the same claim and payment grouping. A second cannot be added until the first is converted.')
	RETURN -1
END IF

wf_clear_conversion()


//Retrieve all the manual transactions that must be converted.
ll_manual_row = wf_retrieve_manual_txn()
IF ll_manual_row <= 0 Then
	ll_conversion_row = dw_conversion.GetRow()
	
	IF ll_conversion_row <> 0 Then
		ll_conversion_no = dw_conversion.GetItemNumber(ll_conversion_row,'ntr_conversion_no')
		wf_load_conversion(ll_conversion_no)
	End if
	
	RETURN ll_manual_row
End if



//Retrieve all the payments that the chosen manual could affect.
ll_rows = wf_retrieve_payment_list()
IF ll_rows <= 0 Then
	RETURN ll_rows
End if


ib_allow_rowfocuschange = TRUE

ll_conversion_row = dw_conversion.InsertRow(0)


dw_conversion.SetRow(ll_conversion_row)
dw_conversion.ScrollToRow(ll_conversion_row)

ib_allow_rowfocuschange = False

//Mark the conversion as NEW
ii_CONVERSION_STATUS = CONV_NEW
ii_Conversion_No = 0

//Get some info from the manual transaction and set it on the conversion record.
ls_new_txn_type_code 			= dw_manual.GetItemString(ll_manual_row,'new_txn_type_code')
ls_new_txn_sub_type_code 		= dw_manual.GetItemString(ll_manual_row,'new_txn_sub_type_code')
ls_explanation						= dw_manual.GetITemString(ll_manual_row,'explanation')
ls_payment_message_group_code = ids_ultimate_payment.GetItemString(1,'payment_message_group_code')



dw_conversion.SetItem(ll_conversion_row,'ultimate_payment_no',al_ultimate_payment_no)
dw_conversion.SetItem(ll_conversion_row,'new_txn_type_code',ls_new_txn_type_code)
dw_conversion.SetItem(ll_conversion_row,'new_txn_sub_type_code',ls_new_txn_sub_type_code)
dw_conversion.SetItem(ll_conversion_row,'converted_flag','N')
dw_conversion.SetItem(ll_conversion_row,'ntr_payment_group_code',ls_payment_message_group_code)
dw_conversion.SetITem(ll_conversion_row,'explanation',ls_explanation)
dw_conversion.SetItem(ll_conversion_row,'claim_no',ll_claim_no)



RETURN 1
end function

public function integer wf_check_module_rules ();LONG			ll_manual_row
LONG			ll_target_rows
LONG			ll_manual_cost_alloc_operation_no
LONG			ll_manual_cost_alloc_no
LONG			ll_payment_no
LONG			ll_employer_no
LONG			ll_legacy_operation_no
LONG			ll_to_claim_no
LONG			ll_conversion_row
STRING		ls_manual_payment_type_code
STRING		ls_manual_payment_sub_type_code
STRING		ls_employer_operation
STRING		ls_employer_type_code
STRING		ls_new_txn_type_code
STRING		ls_new_txn_sub_type_code
INTEGER		li_x
INTEGER		li_rtn
DATETIME	ldt_operation_inactive_date, ldt_operation_start_date, ldt_accident_date
DECIMAL{2}	ldec_manual_days_lost
DECIMAL{2}	ldec_manual_hours_lost
DECIMAL{2}	ldec_total_adjustment_days_lost
DECIMAL{2}	ldec_total_adjustment_hours_lost


ll_manual_row = dw_manual.GetRow()
ll_target_rows = dw_selected_payments.RowCount()


ls_manual_payment_type_code = dw_manual.GetITemString(ll_manual_row,'from_payment_type_code')
ls_manual_payment_sub_type_code = dw_manual.GetITemString(ll_manual_row,'from_payment_sub_type_code')

ll_manual_cost_alloc_no = dw_manual.GetItemNumber(ll_manual_row,'from_cost_alloc_no')
ll_manual_cost_alloc_operation_no = dw_manual.GetItemNumber(ll_manual_row,'from_cost_alloc_operation_no')

ldec_manual_days_lost = dw_manual.GetItemNumber(ll_manual_row,'from_paid_days_lost')
ldec_manual_hours_lost  = dw_manual.GetItemNumber(ll_manual_row,'from_paid_hours_lost')


ldec_total_adjustment_days_lost = dw_selected_payments.GetItemDecimal(1,'c_sum_adjustment_days_lost')
ldec_total_adjustment_hours_lost = dw_selected_payments.GetItemDecimal(1,'c_sum_adjustment_hours_lost')



FOR li_x = 1 To ll_target_rows

	ll_payment_no = dw_selected_payments.GetItemNumber(li_x,'payment_no')
	
	//The paytype/subtype should be the same as the manual
	IF dw_selected_payments.GetItemString(li_x,'payment_type_code') <> ls_manual_payment_type_code &
	 OR dw_selected_payments.GetItemString(li_x,'payment_sub_type_code') <> ls_manual_payment_sub_type_code THEN
		li_rtn = MessageBox('Payment type','All payments should have the same payment type as the "Manual" transaction. ' &
		         + 'Payment #' + String(ll_payment_no) + ' has a different payment type. ' &
					+ 'Do you want to continue with the conversion?',Question!,YesNo!)
		IF li_rtn = 2 THEN
			RETURN -1
		END IF
	END IF
	
	
	//The cost alloc should be the same as the manual
	IF dw_selected_payments.GetItemNumber(li_x,'cost_alloc_no') <> ll_manual_cost_alloc_no &
	 OR dw_selected_payments.GetItemNumber(li_x,'cost_alloc_operation_no') <> ll_manual_cost_alloc_operation_no THEN
		li_rtn = MessageBox('Cost Allocation','All payments should have the same Cost Allocation as the "Manual" transaction. ' &
		         + 'Payment #' + String(ll_payment_no) + ' has a different Cost Allocation. ' &
					+ 'Do you want to continue with the conversion?',Question!,YesNo!)
		IF li_rtn = 2 THEN
			RETURN -1
		END IF
	END IF
	
	// check for cost transfer where Cost Allocation operation date range excludes the claim accident date
	// The following is an edited version of script in n_claim_employer.nf_validate_operation_coverage()
	// That function prevents user from saving data where operation date range excludes claim accident date
	// This function only warns of that situation.
	
	
	
	ll_conversion_row = dw_conversion.GetRow()
	ls_new_txn_type_code = dw_conversion.GetItemString(ll_conversion_row,'new_txn_type_code')
	ls_new_txn_sub_type_code = dw_conversion.GetItemString(ll_conversion_row,'new_txn_sub_type_code')
		
	IF ls_new_txn_type_code = 'T' AND ( ls_new_txn_sub_type_code = '7' OR ls_new_txn_sub_type_code = '8') THEN
		ll_to_claim_no				= dw_manual.GetItemNumber(ll_manual_row,'to_claim_no')
		ll_employer_no				= dw_manual.GetItemNumber(ll_manual_row,'to_cost_alloc_no')
		ll_legacy_operation_no	= dw_manual.GetItemNumber(ll_manual_row,'to_cost_alloc_operation_no')
		ls_employer_operation	= String(ll_employer_no) + '/' + String(ll_legacy_operation_no)
		
		//Only "Assessed" and "Self Insured" employers have a coverage period
		//other Employer Accounts have continuous coverage from "The Dawn of Time" until "Judgment Day".
		SELECT	b.operation_start_date,
					b.operation_inactive_date,
					a.employer_type_code
		INTO		:ldt_operation_start_date,
					:ldt_operation_inactive_date,
					:ls_employer_type_code
		FROM		EMPLOYER		a,
					OPERATION		b
		WHERE	a.employer_no				= b.employer_no
		AND		a.employer_no				= :ll_employer_no
		AND		b.legacy_operation_no	= :ll_legacy_operation_no
		USING SQLCA;
		
		SQLCA.nf_handle_error('w_manual_txn_conversion','wf_check_module_rules','select ... from EMPLOYER, OPERATION...')
		
		IF ls_employer_type_code = 'A' or ls_employer_type_code = 'S' Then
			
		
			SELECT	accident_date
			INTO		:ldt_accident_date
			FROM		CLAIM
			WHERE	claim_no = :ll_to_claim_no
			USING SQLCA;
			
			SQLCA.nf_handle_error('w_manual_txn_conversion','wf_check_module_rules','select accident_date from CLAIM...')
			
			
			//Employer/Operation cannot be negative
			IF ll_employer_no < 0 or ll_legacy_operation_no < 0 Then
				SignalError(-666,'Employer/operation cannot be negative.')
			END IF
			
			//Null values are not allowed
			If IsNull(ll_employer_no)  or IsNull(ll_legacy_operation_no) Then
				SignalError(-666,'Employer/operation is null.')
			END IF
			
			//Zeros are valid as parameters. If cost allocation is required then zeros are not allowed
			IF ll_employer_no = 0 and ll_legacy_operation_no = 0 Then
				SignalError(-666,'0/0 is not an Employer/operation.')
			End if
			
			//BR_1.70 START   **********
			// A cost allocation transfer SHOULD not be completed if the accident date is after the last operated date of the 'TO' cost allocation operation.
			IF NOT IsNull(ldt_operation_inactive_date) Then
				
				IF ldt_operation_inactive_date < ldt_accident_date Then
					//The accident occured after the operation became inactive. We will prevent this but 
					//first we must determine which error message to give the user.		
					
					li_rtn = MessageBox('Cost Allocation Employer Warning', 'Cost Allocation Employer Operation (' + ls_employer_operation + ') became inactive on ' + String(ldt_operation_inactive_date,'yyyy-mm-dd')+&
						', and the accident did not occur until ' + String(ldt_accident_date,'yyyy-mm-dd') + '. Do you want to continue with the conversion?',Question!,YesNo!)
					IF li_rtn = 2 THEN
						RETURN -1
					END IF
				END IF
			End if
			//BR_1.70 START   **********
			
			//BR_1.60 START **********
			// A cost allocation transfer SHOULD not be completed if the accident date of the claim prior to the 'TO' cost allocation operation start date.
			IF ldt_operation_start_date > ldt_accident_date Then
				li_rtn = MessageBox('Cost Allocation Employer Warning','Cost Allocation Employer Operation (' + ls_employer_operation + ') did not start operations until ' + String(ldt_operation_start_date,'yyyy-mm-dd')+&
									', and the accident occured on ' + String(ldt_accident_date,'yyyy-mm-dd') + '. Do you want to continue with the conversion?',Question!,YesNo!)
					
				IF li_rtn = 2 THEN
					RETURN -1
				END IF
			END IF
			//BR_1.60 END **********
		END IF
	End if
		
Next

//Perform batch rules.

IF ldec_total_adjustment_days_lost <> ldec_manual_days_lost Then
	li_rtn = MessageBox('Warning','The total adjustment days for the selected payments does not equal the days lost for the manual transaction. Do you want to continue with the conversion?',Question!,YesNo!,1)
	IF li_rtn = 2 Then
		RETURN -1
	END IF
END IF

RETURN 1
end function

public function integer wf_refresh_selected_grouping ();
dw_selected_payments.Sort()
dw_selected_payments.GroupCalc()

return 1
end function

public function integer wf_load_conversion (long al_conversion_no);LONG			ll_ultimate_payment_no
LONG			ll_conversion_row
LONG			ll_found_row
LONG			ll_rows
INTEGER		li_rtn



IF al_conversion_no = 0 or IsNull(al_conversion_no) THEN
	RETURN 0
END IF

This.SetRedraw(False)

ll_found_row = dw_conversion.Find('ntr_conversion_no = ' + String(al_conversion_no),1,dw_conversion.RowCount())
IF ll_found_row <= 0 Then
	SignalError(-666,'Error finding conversion number ' + String(al_conversion_no))
End if

wf_clear_conversion()

ii_CONVERSION_STATUS = CONV_NOT_MODIFIED
ii_conversion_no = al_conversion_no



st_conversion_no.text = '# ' + String(ii_conversion_no)

ll_ultimate_payment_no = dw_conversion.GetItemNumber(ll_found_row,'ultimate_payment_no')

wf_retrieve_ultimate_payment(ll_ultimate_payment_no)

wf_retrieve_manual_txn()

wf_retrieve_payment_list()

wf_retrieve_selected_payments(al_conversion_no)

wf_removed_selected_from_payment_list()

wf_refresh_selected_grouping()

This.SetRedraw(True)

//Get information about the ultimate payment
RETURN 1
end function

public function integer wf_remove_selected_payments ();LONG			ll_selected_rows[]
INTEGER		li_x
LONG			ll_index
LONG			ll_found_row
LONG			ll_payment_no
LONG			ll_selected_row_count
STRING		ls_find
INTEGER		li_rtn


IF ii_CONVERSION_STATUS = CONV_NOT_MODIFIED THEN
	ii_CONVERSION_STATUS = CONV_MODIFIED
END IF


//Get the list of all selected payments

FOR li_x = 1 to dw_selected_payments.RowCount()
	IF dw_selected_payments.IsSelected(li_x) Then
		ll_index += 1
		ll_selected_rows[ll_index] = li_x
	End if
Next

IF ll_index = 0 Then
	MessageBox('Nothing selected','There are no payments selected to remove. Try again.')
	RETURN 0
END IF



ll_selected_row_count = UpperBound(ll_selected_rows)
FOR li_x = ll_selected_row_count to 1 STEP -1
	ll_payment_no = dw_selected_payments.GetItemNumber(ll_selected_rows[li_x],'payment_no')
	
	ls_find = "payment_no = " + String(ll_payment_no)
	
	ll_found_row = ids_payment_list_selected.Find(ls_find,1,ids_payment_list_selected.RowCount())
	If ll_found_row <= 0 Then
		SignalError(-666,'Error finding payment')
	End if
	
	li_rtn = ids_payment_list_selected.RowsMove(ll_found_row,ll_found_row,Primary!,dw_payment_list,1,Primary!)
	IF li_rtn  = -1 Then
		SignalError(-666,'Error moving payment back')
	End if
	
	
	
	li_rtn = dw_selected_payments.DeleteRow(ll_selected_rows[li_x])
	IF li_rtn = -1 Then
		SignalError(-666,'Error deleting payment from selected payment list.')
	End if
Next


wf_refresh_selected_grouping()


RETURN 1
end function

public function integer wf_retrieve_selected_payments (long al_conversion_no);LONG		ll_rows

ll_rows = dw_selected_payments.Retrieve(al_conversion_no)
SQLCA.nf_handle_error('w_manual_txn_conversion','wf_retrieve_selected_payments','ll_rows = dw_selected_payments.Retrieve(al_conversion_no)')

IF ll_rows <= 0 Then 
	SignalError(-666,'Error occured retrieving selected payments for conversion #' + STring(al_conversion_no))
End if

wf_refresh_selected_grouping()


RETURN 1
end function

public function integer wf_convert ();n_claim_cost_maintenance	lnv_ccm
INTEGER							li_x, li_rtn
LONG								ll_row
LONG								ll_conversion_row
LONG								ll_manual_row
LONG								ll_ultimate_payment_no
N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '006' refers to the Convert Manual Transaction module, '044' refers to the Payment Processing module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('006','044','conversion',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return -1
END IF

/******************************************************************************************/


lnv_ccm = create n_claim_cost_maintenance

lnv_ccm.is_module_code = '006'

dw_conversion.AcceptText()
dw_selected_payments.AcceptText()


IF ii_CONVERSION_STATUS = CONV_NEW OR ii_CONVERSION_STATUS = CONV_MODIFIED THEN
	IF wf_save(False) <> 1 THEN
		RETURN -1	
	End if
END IF

IF ii_CONVERSION_STATUS = CONV_EMPTY THEN
	MessageBox('Nothing to convert','You have not selected anything to convert.')
	RETURN -1
END IF

SetPointer(Hourglass!)

IF wf_check_module_rules() = -1 THEN
	RETURN -1
END IF


ll_conversion_row = dw_conversion.Find('ntr_conversion_no = ' + String(ii_conversion_no),1,dw_conversion.RowCount())
IF ll_conversion_row <= 0 THEN
	SignalError(-666,'Error finding conversion #' + String(ii_conversion_no))
END IF


ll_ultimate_payment_no = dw_conversion.GetItemNumber(ll_conversion_row,'ultimate_payment_no')

ll_manual_row 		= dw_manual.GetRow()

lnv_ccm.ids_batch_control.InsertRow(0)

//Data from the conversion record
lnv_ccm.ids_batch_control.SetItem(1,'new_txn_type_code',dw_conversion.GetItemString(ll_conversion_row,'new_txn_type_code'))
lnv_ccm.ids_batch_control.SetItem(1,'new_txn_sub_type_code',dw_conversion.GetItemString(ll_conversion_row,'new_txn_sub_type_code'))
lnv_ccm.ids_batch_control.SetItem(1,'explanation',dw_conversion.GetItemString(ll_conversion_row,'explanation'))


//Data from the manual record
lnv_ccm.ids_batch_control.SetItem(1,'total_adjustment_txn_amount',dw_manual.GetItemDecimal(ll_manual_row,'txn_amount'))
lnv_ccm.ids_batch_control.SetItem(1,'to_claim_no',dw_manual.GetItemNumber(ll_manual_row,'to_claim_no'))
lnv_ccm.ids_batch_control.SetItem(1,'to_cost_alloc_no',dw_manual.GetItemNumber(ll_manual_row,'to_cost_alloc_no'))
lnv_ccm.ids_batch_control.SetItem(1,'to_cost_alloc_operation_no',dw_manual.GetItemNumber(ll_manual_row,'to_cost_alloc_operation_no'))
lnv_ccm.ids_batch_control.SetItem(1,'to_payment_type_code',dw_manual.GetItemString(ll_manual_row,'to_payment_type_code'))
lnv_ccm.ids_batch_control.SetItem(1,'to_payment_sub_type_code',dw_manual.GetItemString(ll_manual_row,'to_payment_sub_type_code'))
lnv_ccm.ids_batch_control.SetItem(1,'from_claim_no',dw_manual.GetItemNumber(ll_manual_row,'from_claim_no'))
lnv_ccm.ids_batch_control.SetItem(1,'from_cost_alloc_no',dw_manual.GetItemNumber(ll_manual_row,'from_cost_alloc_no'))
lnv_ccm.ids_batch_control.SetItem(1,'from_cost_alloc_operation_no',dw_manual.GetItemNumber(ll_manual_row,'from_cost_alloc_operation_no'))
lnv_ccm.ids_batch_control.SetItem(1,'from_payment_type_code',dw_manual.GetItemString(ll_manual_row,'from_payment_type_code'))
lnv_ccm.ids_batch_control.SetItem(1,'from_payment_sub_type_code',dw_manual.GetItemString(ll_manual_row,'from_payment_sub_type_code'))	
lnv_ccm.ids_batch_control.SetItem(1,'cheque_no',dw_manual.GetItemNumber(ll_manual_row,'cheque_no'))	
lnv_ccm.ids_batch_control.SetItem(1,'direct_deposit_xmit_no',dw_manual.GetItemNumber(ll_manual_row,'direct_deposit_xmit_no'))	
lnv_ccm.ids_batch_control.SetItem(1,'cheque_deposit_date',dw_manual.GetItemDateTime(ll_manual_row,'cheque_deposit_date'))	
lnv_ccm.ids_batch_control.SetItem(1,'from_payment_method_code',dw_manual.GetItemString(ll_manual_row,'from_payment_method_code'))	
lnv_ccm.ids_batch_control.SetItem(1,'to_payment_method_code',dw_manual.GetItemString(ll_manual_row,'to_payment_method_code'))	
	


FOR li_x = 1 to dw_selected_payments.RowCount()
	ll_row = lnv_ccm.ids_maintenance.InsertRow(0)
	
	
	//Data from the target txn	
	lnv_ccm.ids_maintenance.SetItem(ll_row,'target_payment_no',dw_selected_payments.GetItemNumber(li_x,'payment_no'))
	lnv_ccm.ids_maintenance.SetItem(ll_row,'target_txn_no',dw_selected_payments.GetItemNumber(li_x,'txn_no'))
	lnv_ccm.ids_maintenance.SetItem(ll_row,'adjustment_txn_amount',dw_selected_payments.GetItemDecimal(li_x,'adjustment_txn_amount'))
	lnv_ccm.ids_maintenance.SetItem(ll_row,'adjustment_quantity',dw_selected_payments.GetItemNumber(li_x,'adjustment_quantity'))
	lnv_ccm.ids_maintenance.SetItem(ll_row,'adjustment_days',dw_selected_payments.GetItemNumber(li_x,'adjustment_days_lost'))	
	lnv_ccm.ids_maintenance.SetItem(ll_row,'adjustment_hours',dw_selected_payments.GetItemNumber(li_x,'adjustment_hours_lost'))
Next


TRY
	lnv_ccm.of_check_business_rules()
CATCH (uo_br_exception luo_br_exception)
	
	if luo_br_exception.GetMessage() <> '' Then
		MessageBox('Error occured',luo_br_exception.GetMessage())
	End if
	
	RETURN -1
END TRY


DECLARE p_Convert_Manual_Txn PROCEDURE FOR p_Convert_Manual_Txn  
        @conversion_no = :ii_conversion_no  
USING SQLCA;
SQLCA.nf_handle_error("dw_list_payments","w_manual_txn_conversion","DECLARE p_Convert_Manaul_Txn")

EXECUTE p_Convert_Manual_Txn;
SQLCA.nf_handle_error("dw_list_payments","w_manual_txn_conversion","EXECUTE p_Convert_Manaul_Txn")

CLOSE p_Convert_Manual_Txn;
SQLCA.nf_handle_error("dw_list_payments","w_manual_txn_conversion","CLOSE p_Convert_Manaul_Txn")



wf_retrieve_conversion_list()

wf_new_conversion(ll_ultimate_payment_no)

return 1

end function

public function integer wf_add_selected_payments ();LONG			ll_selected_rows[]
INTEGER		li_x
LONG			ll_index
LONG			ll_new_row


IF ii_CONVERSION_STATUS = CONV_NOT_MODIFIED THEN
	ii_CONVERSION_STATUS = CONV_MODIFIED
END IF



//Get the list of all selected payments

FOR li_x = 1 to dw_payment_list.RowCount()
	IF dw_payment_list.IsSelected(li_x) Then
		ll_index += 1
		ll_selected_rows[ll_index] = li_x
	End if
Next

if ll_index = 0 Then
	MessageBox('None selected','There are no payments selected.')
	RETURN 0
END IF

//Loop through the selected payments
// - copy selected payment data to dw_selected_payment
// - move selected rows into the deleted buffer



FOR li_x = UpperBound(ll_selected_rows) TO 1 STEP -1
	ll_new_row = dw_selected_payments.InsertRow(0)
	dw_selected_payments.SetItem(ll_new_row,'payment_type_code', dw_payment_list.GetItemString(ll_selected_rows[li_x],'payment_type_code'))
	dw_selected_payments.SetItem(ll_new_row,'payment_sub_type_code', dw_payment_list.GetItemString(ll_selected_rows[li_x],'payment_sub_type_code'))
	dw_selected_payments.SetItem(ll_new_row,'paid_from_date', dw_payment_list.GetItemDateTime(ll_selected_rows[li_x],'paid_from_date'))
	dw_selected_payments.SetItem(ll_new_row,'paid_to_date', dw_payment_list.GetItemDateTime(ll_selected_rows[li_x],'paid_to_date'))
	dw_selected_payments.SetItem(ll_new_row,'payment_no', dw_payment_list.GetItemNumber(ll_selected_rows[li_x],'payment_no'))
	dw_selected_payments.SetItem(ll_new_row,'txn_no', dw_payment_list.GetItemNumber(ll_selected_rows[li_x],'txn_no'))	
	dw_selected_payments.SetItem(ll_new_row,'claim_no', dw_payment_list.GetItemNumber(ll_selected_rows[li_x],'claim_no'))	
	dw_selected_payments.SetItem(ll_new_row,'payment_no', dw_payment_list.GetItemNumber(ll_selected_rows[li_x],'payment_no'))	
	dw_selected_payments.SetItem(ll_new_row,'net_days_lost', dw_payment_list.GetItemNumber(ll_selected_rows[li_x],'net_days_lost'))
	dw_selected_payments.SetItem(ll_new_row,'net_quantity', dw_payment_list.GetItemNumber(ll_selected_rows[li_x],'net_quantity'))
	dw_selected_payments.SetItem(ll_new_row,'net_hours_lost', dw_payment_list.GetItemNumber(ll_selected_rows[li_x],'net_hours_lost'))
	dw_selected_payments.SetItem(ll_new_row,'txn_amount', dw_payment_list.GetItemNumber(ll_selected_rows[li_x],'txn_amount'))
	dw_selected_payments.SetItem(ll_new_row,'adjustment_txn_amount', dw_payment_list.GetItemNumber(ll_selected_rows[li_x],'txn_amount') * -1)
	dw_selected_payments.SetItem(ll_new_row,'cost_alloc_no', dw_payment_list.GetItemNumber(ll_selected_rows[li_x],'cost_alloc_no'))
	dw_selected_payments.SetItem(ll_new_row,'cost_alloc_operation_no', dw_payment_list.GetItemNumber(ll_selected_rows[li_x],'cost_alloc_operation_no'))

	IF dw_payment_list.GetItemString(ll_selected_rows[li_x],'split_payment_flag') = 'N' Then
		dw_selected_payments.SetItem(ll_new_row,'adjustment_days_lost', dw_payment_list.GetItemNumber(ll_selected_rows[li_x],'paid_days_lost') * -1)
		dw_selected_payments.SetItem(ll_new_row,'adjustment_quantity', dw_payment_list.GetItemNumber(ll_selected_rows[li_x],'paid_quantity') * -1)	
		dw_selected_payments.SetItem(ll_new_row,'adjustment_hours_lost', dw_payment_list.GetItemNumber(ll_selected_rows[li_x],'paid_hours_lost') * -1)	
	Else
		dw_selected_payments.SetItem(ll_new_row,'adjustment_days_lost', 0)
		dw_selected_payments.SetItem(ll_new_row,'adjustment_quantity', 0)	
		dw_selected_payments.SetItem(ll_new_row,'adjustment_hours_lost', 0)	
	END IF
	
	dw_selected_payments.SelectRow(ll_new_row,True)
	//Move the selected payment to the deleted buffer.
	
	//I had to use the RowsCopy and RowsDiscard combination because RowsMove
	//cause the rowfocuschanged event to fire twice and it reported the wrong currentrow 
	//when zero rows were left in the datawindow
	dw_payment_list.RowsCopy(ll_selected_rows[li_x],ll_selected_rows[li_x],Primary!,ids_payment_list_selected,1,Primary!)
	dw_payment_list.RowsDiscard(ll_selected_rows[li_x],ll_selected_rows[li_x],Primary!)
Next

wf_refresh_selected_grouping()




RETURN 1
end function

public function integer wf_save (boolean ab_show_message);LONG		ll_new_row
LONG		ll_manual_row
LONG		ll_from_txn_no
LONG		ll_from_claim_no
LONG		ll_from_payment_no
LONG		ll_to_txn_no
LONG		ll_to_claim_no
LONG		ll_to_payment_no
LONG		ll_next_conversion_no
LONG		ll_conversion_row
STRING	ls_new_txn_type_code
STRING	ls_new_txn_sub_type_code
STRING	ls_payment_message_group_code
INTEGER	li_x
INTEGER	li_rtn
u_ds		lds_old_manual_txn

dw_manual.AcceptText()
dw_selected_payments.AcceptText()
dw_conversion.AcceptText()

IF ii_CONVERSION_STATUS <> CONV_NEW AND ii_CONVERSION_STATUS <> CONV_MODIFIED THEN
	MessageBox('No changes','There is nothing to save.')
	RETURN -1
END IF

IF dw_selected_payments.RowCount() = 0 Then
	MessageBox('No Payments','No payments have been selected. There is nothing to save.')
	return -1
end if

SetPointer(Hourglass!)

lds_old_manual_txn = create u_ds
lds_old_manual_txn.DataObject = 'd_old_manual_txn'
lds_old_manual_txn.SetTransObject(SQLCA)



SQLCA.nf_begin_transaction()

IF ii_CONVERSION_STATUS = CONV_NEW THEN
	
	ll_next_conversion_no = wf_get_next_conversion_no()
	ii_conversion_no =  ll_next_conversion_no	
	
	ll_manual_row = dw_manual.GetRow()	
	ll_from_txn_no 		= dw_manual.GetItemNumber(ll_manual_row,'from_txn_no')
	ll_from_claim_no  	= dw_manual.GetItemNumber(ll_manual_row,'from_claim_no')
	ll_from_payment_no  	= dw_manual.GetItemNumber(ll_manual_row,'from_payment_no')
	ll_to_txn_no 			= dw_manual.GetItemNumber(ll_manual_row,'to_txn_no')
	ll_to_claim_no  		= dw_manual.GetItemNumber(ll_manual_row,'to_claim_no')
	ll_to_payment_no  	= dw_manual.GetItemNumber(ll_manual_row,'to_payment_no')
	

	/* MANUAL_TXN_CONVERSION */
	ll_conversion_row = dw_conversion.GetRow()
	li_rtn = dw_conversion.SetItem(ll_conversion_row,'ntr_conversion_no',ll_next_conversion_no)


	/* OLD_MANUAL_TXN */
	ll_new_row = lds_old_manual_txn.InsertRow(0)
	lds_old_manual_txn.SetItem(ll_new_row,'ntr_conversion_no',ll_next_conversion_no)
	lds_old_manual_txn.SetItem(ll_new_row,'txn_no'           ,ll_from_txn_no)
	lds_old_manual_txn.SetItem(ll_new_row,'claim_no'         ,ll_from_claim_no)
	lds_old_manual_txn.SetItem(ll_new_row,'payment_no'       ,ll_from_payment_no)
	
	IF ll_to_txn_no <> 0 Then
		ll_new_row = lds_old_manual_txn.InsertRow(0)
		lds_old_manual_txn.SetItem(ll_new_row,'ntr_conversion_no',ll_next_conversion_no)
		lds_old_manual_txn.SetItem(ll_new_row,'txn_no'           ,ll_to_txn_no)
		lds_old_manual_txn.SetItem(ll_new_row,'claim_no'         ,ll_to_claim_no)
		lds_old_manual_txn.SetItem(ll_new_row,'payment_no'       ,ll_to_payment_no)
	End if
	
End if

/* AFFECTED_TXN */
FOR li_x = 1 to dw_selected_payments.RowCount()
	//Set the conversion number for new records. This could be for a new
	//conversion or a modified conversion.
	IF dw_selected_payments.GetItemStatus(li_x,0,Primary!) = NewModified! Then
		dw_selected_payments.SetItem(li_x,'ntr_conversion_no',ii_conversion_no)
	End if
NEXT


//Set the conversion status to not modified because we just saved all the changes.
ii_CONVERSION_STATUS = CONV_NOT_MODIFIED




//UPDATE
li_rtn = dw_conversion.UPDATE()
SQLCA.nf_handle_error('w_manual_txn_conversion','wf_save','lds_manual_txn_conversion.UPDATE()')
IF li_rtn <> 1 Then
	SignalError(-666,'MANUAL_TXN_CONVERSION UPDATE FAILED.')
END IF

li_rtn = lds_old_manual_txn.UPDATE()
SQLCA.nf_handle_error('w_manual_txn_conversion','wf_save','lds_old_manual_txn.UPDATE()')
IF li_rtn <> 1 Then
	SignalError(-666,'OLD_MANUAL_TXN UPDATE FAILED.')
END IF

li_rtn = dw_selected_payments.UPDATE()
SQLCA.nf_handle_error('w_manual_txn_conversion','wf_save','dw_selected_payments.UPDATE()')
IF li_rtn <> 1 Then
	SignalError(-666,'AFFECTED_TXN UPDATE FAILED.')
END IF

SQLCA.nf_commit_transaction()


RETURN 1
end function

on w_manual_txn_conversion.create
int iCurrent
call super::create
if IsValid(this.MenuID) then destroy(this.MenuID)
if this.MenuName = "m_convert_manual" then this.MenuID = create m_convert_manual
this.st_conversion_no=create st_conversion_no
this.cb_match=create cb_match
this.st_conversion_details=create st_conversion_details
this.dw_conversion=create dw_conversion
this.st_splitbar_2=create st_splitbar_2
this.st_splitbar_1=create st_splitbar_1
this.pb_remove=create pb_remove
this.pb_add=create pb_add
this.dw_selected_payments=create dw_selected_payments
this.uo_filter_payments=create uo_filter_payments
this.dw_manual=create dw_manual
this.dw_payment_list=create dw_payment_list
this.st_conversion_list=create st_conversion_list
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_conversion_no
this.Control[iCurrent+2]=this.cb_match
this.Control[iCurrent+3]=this.st_conversion_details
this.Control[iCurrent+4]=this.dw_conversion
this.Control[iCurrent+5]=this.st_splitbar_2
this.Control[iCurrent+6]=this.st_splitbar_1
this.Control[iCurrent+7]=this.pb_remove
this.Control[iCurrent+8]=this.pb_add
this.Control[iCurrent+9]=this.dw_selected_payments
this.Control[iCurrent+10]=this.uo_filter_payments
this.Control[iCurrent+11]=this.dw_manual
this.Control[iCurrent+12]=this.dw_payment_list
this.Control[iCurrent+13]=this.st_conversion_list
end on

on w_manual_txn_conversion.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.st_conversion_no)
destroy(this.cb_match)
destroy(this.st_conversion_details)
destroy(this.dw_conversion)
destroy(this.st_splitbar_2)
destroy(this.st_splitbar_1)
destroy(this.pb_remove)
destroy(this.pb_add)
destroy(this.dw_selected_payments)
destroy(this.uo_filter_payments)
destroy(this.dw_manual)
destroy(this.dw_payment_list)
destroy(this.st_conversion_list)
end on

event open;LONG		ll_row

ids_ultimate_payment = CREATE u_ds
ids_payment_list_selected = CREATE u_ds


//SERVICES

/* DATAWINDOW */
dw_payment_list.uf_SetFilter(True)
dw_payment_list.uf_SetSort(True)
dw_selected_payments.uf_SetSort(True)




//dw_manual.inv_sort.
dw_payment_list.uf_SetSelect(dw_payment_list.RS_EXTENDED)
dw_selected_payments.uf_SetSelect(dw_selected_payments.RS_EXTENDED)

uo_filter_payments.uf_Set_Requestor(dw_payment_list)

/* SPLITBAR */
st_splitbar_1.of_Register(dw_payment_list)
st_splitbar_1.of_Register(dw_manual)
st_splitbar_1.of_Register(dw_selected_payments)
st_splitbar_1.of_Register(st_splitbar_2)

st_splitbar_2.of_Register(dw_payment_list)
st_splitbar_2.of_Register(dw_selected_payments)
st_splitbar_2.of_Register(pb_add)
st_splitbar_2.of_Register(pb_remove)



/* WINDOW */

This.wf_SetResize(True)


//(Move H,Move V,Grow H, Grow V)

inv_resize.of_register(dw_conversion,0,0,100,0)
inv_resize.of_register(dw_manual,0,0,100,0)
inv_resize.of_register(dw_payment_list,0,0,50,100)
inv_resize.of_register(dw_selected_payments,50,0,50,100)

inv_resize.of_register(st_splitbar_1,0,0,100,0)
inv_resize.of_register(st_splitbar_2,50,0,0,100)

inv_resize.of_register(st_conversion_list,0,0,100,0)
inv_resize.of_register(st_conversion_details,0,0,100,0)

inv_resize.of_register(uo_filter_payments,0,100,0,0)
inv_resize.of_register(pb_add,50,100,0,0)
inv_resize.of_register(pb_remove,50,100,0,0)

inv_resize.of_register(cb_match,100,100,0,0)




ids_ultimate_payment.dataobject = 'd_ultimate_payment'
ids_ultimate_payment.SetTransObject(SQLCA)

ids_payment_list_selected.DataObject = 'd_apply_manual_payment_list'
ids_payment_list_selected.SetTransObject(SQLCA)

dw_manual.SetTransObject(SQLCA)
dw_payment_list.SetTransObject(sqlca)
dw_selected_payments.SetTransObject(SQLCA)
dw_conversion.SetTransObject(SQLCA)

iws_frame_open_state = w_frame.WindowState
w_frame.WindowState = Maximized!




PostEvent("ue_postopen")










end event

event closequery;INTEGER		li_rtn

dw_Conversion.AcceptText()
dw_selected_payments.AcceptText()

IF wf_check_conversion_status() = -1 THEN
	RETURN 1
END IF
end event

event resize;call super::resize;
st_splitbar_2.of_SetRequestor(This)


end event

type st_conversion_no from statictext within w_manual_txn_conversion
integer x = 521
integer y = 444
integer width = 343
integer height = 60
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 268435456
boolean focusrectangle = false
end type

type cb_match from commandbutton within w_manual_txn_conversion
boolean visible = false
integer x = 3909
integer y = 2316
integer width = 494
integer height = 104
integer taborder = 80
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Suggest a match"
end type

event clicked;
wf_suggest_a_match()


end event

type st_conversion_details from statictext within w_manual_txn_conversion
integer y = 444
integer width = 4453
integer height = 64
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 268435456
string text = " Conversion details:"
boolean focusrectangle = false
end type

type dw_conversion from u_dw_online within w_manual_txn_conversion
event ue_key pbm_dwnkey
integer x = 14
integer y = 76
integer width = 4439
integer height = 348
integer taborder = 40
string dataobject = "d_manual_txn_conversion"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event ue_key;if KeyDown(KeyDelete!) Then
	wf_delete_conversion()
End if
end event

event doubleclicked;call super::doubleclicked;INTEGER		li_rtn
LONG			ll_conversion_no

IF row <> 0 Then
	//If the conversion is NEW, there is nothing to load.
	IF dw_conversion.GetItemStatus(row,0,Primary!) <> NewModified! Then
		
		if wf_check_conversion_status() = 1 THEN
			ib_allow_rowfocuschange = TRUE
			dw_conversion.SetRow(row)
			ib_allow_rowfocuschange = False
		END IF
	End if
End if
end event

event rbuttondown;m_convert_manual		im_menu

im_menu = CREATE m_convert_manual

im_menu.m_conversion.Popmenu(w_frame.Pointerx(),w_frame.Pointery())
end event

event rowfocuschanged;call super::rowfocuschanged;LONG			ll_conversion_no

IF currentrow <> 0 Then
	
	ll_conversion_no = dw_conversion.GetItemNumber(currentrow,'ntr_conversion_no')

	wf_load_conversion(ll_conversion_no)
	
END IF
end event

event itemchanged;call super::itemchanged;IF ii_CONVERSION_STATUS = CONV_NOT_MODIFIED THEN
	ii_CONVERSION_STATUS = CONV_MODIFIED
END IF
end event

event clicked;call super::clicked;IF row > 0 Then
	SetRow(row)
End if
end event

event rowfocuschanging;call super::rowfocuschanging;long			ll_conversion_no


IF NOT ib_allow_rowfocuschange Then
	
	RETURN 1

eND IF


RETURN 0
	
end event

type st_splitbar_2 from u_splitbar_vertical within w_manual_txn_conversion
integer x = 2405
integer y = 968
integer width = 32
integer height = 1324
borderstyle borderstyle = styleraised!
long il_min_units_from_left = 750
long il_min_units_from_right = 700
end type

event ue_moved;call super::ue_moved;INTEGER		li_rtn


Parent.inv_resize.of_unregister(dw_payment_list)
Parent.inv_resize.of_unregister(dw_selected_payments)


Parent.inv_resize.of_register(dw_payment_list,0,0,50,100)
Parent.inv_resize.of_register(dw_selected_payments,50,0,50,100)
end event

type st_splitbar_1 from u_splitbar_horizontal within w_manual_txn_conversion
integer x = 18
integer y = 940
integer width = 4434
integer height = 24
long il_min_units_from_top = 900
end type

type pb_remove from picturebutton within w_manual_txn_conversion
integer x = 2446
integer y = 2308
integer width = 101
integer height = 88
integer taborder = 50
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean originalsize = true
string picturename = "Custom036!"
alignment htextalign = left!
end type

event clicked;wf_remove_selected_payments()
end event

type pb_add from picturebutton within w_manual_txn_conversion
integer x = 2295
integer y = 2308
integer width = 101
integer height = 88
integer taborder = 40
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean originalsize = true
string picturename = "Custom035!"
alignment htextalign = left!
end type

event clicked;wf_add_selected_payments()
end event

type dw_selected_payments from u_dw_online within w_manual_txn_conversion
event ue_lbuttonup pbm_dwnlbuttonup
integer x = 2437
integer y = 968
integer width = 2016
integer height = 1328
integer taborder = 60
string dragicon = "Query5!"
string dataobject = "d_selected_payments"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event ue_lbuttonup;if ib_selected_payment_drag = True Then
	ib_selected_payment_drag = False
	This.Drag(End!)
End if
end event

event dragdrop;call super::dragdrop;datawindow		ldw_source


IF ib_payment_list_drag = True Then
	IF source.TypeOf() = DataWindow! THEN
			ldw_Source = source
			if ldw_source = dw_payment_list THEN
				
				wf_add_selected_payments()
				
				ib_payment_list_drag = false
			end if
	END IF
End if
end event

event clicked;call super::clicked;LONG		ll_tabsequence

IF row <> 0 Then
	this.setrow(row)
	IF  dwo.type = 'column' tHEN
		if Long(dwo.tabsequence) > 0 Then
			RETURN
		END IF
	END IF
	
	ib_selected_payment_drag = TRUE
	il_mousemove = 0
	
End if

end event

event dwmousemove;call super::dwmousemove;IF KeyDown(KeyLeftButton!) Then
	IF ib_selected_payment_drag = TRUE Then
		IF il_mousemove >= 1 THEN
			This.Drag(Begin!)
		End if		
		il_mousemove ++
	End if
End if



end event

event itemchanged;call super::itemchanged;IF ii_CONVERSION_STATUS = CONV_NOT_MODIFIED THEN
	ii_CONVERSION_STATUS = CONV_MODIFIED
END IF
end event

type uo_filter_payments from u_filter_control within w_manual_txn_conversion
integer x = 37
integer y = 2320
integer taborder = 70
end type

on uo_filter_payments.destroy
call u_filter_control::destroy
end on

type dw_manual from u_dw_online within w_manual_txn_conversion
integer x = 14
integer y = 516
integer width = 4439
integer height = 424
integer taborder = 100
string dataobject = "d_manual_summary_2"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanging;call super::rowfocuschanging;IF newrow <> RowCount() Then
	RETURN 1
ELSE
	RETURN 0
eND IF
end event

type dw_payment_list from u_dw_online within w_manual_txn_conversion
event ue_lbuttondown pbm_lbuttondown
event ue_lbuttonup pbm_dwnlbuttonup
event lbuttonup pbm_lbuttonup
integer x = 14
integer y = 968
integer width = 2391
integer height = 1328
integer taborder = 30
string dragicon = "Query5!"
string dataobject = "d_apply_manual_payment_list"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event ue_lbuttonup;IF ib_payment_list_drag = True Then
	ib_payment_list_drag = False
	This.Drag(End!)
End if


end event

event dwmousemove;call super::dwmousemove;

IF KeyDown(KeyLeftButton!) Then
	IF ib_payment_list_drag = TRUE Then		
		IF il_mousemove >= 1 THEN
			This.Drag(Begin!)
		End if		
		il_mousemove ++
	End if
End if
end event

event clicked;call super::clicked;
if row <> 0 Then
	ib_payment_list_drag = TRUE
	il_mousemove = 0
End If



end event

event dragdrop;call super::dragdrop;datawindow		ldw_source

IF ib_selected_payment_drag = True THen
	IF source.TypeOf() = DataWindow! THEN
			ldw_Source = source
			
			wf_remove_selected_payments()
			
			ib_selected_payment_drag = false
	
	END IF
End if
end event

event ue_rowselecting;call super::ue_rowselecting;IF GetItemString(al_row,'selectable_flag') = 'Y' Then
	return 1
Else
	return -1
End if
end event

type st_conversion_list from statictext within w_manual_txn_conversion
integer y = 4
integer width = 4453
integer height = 64
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 268435456
string text = " Conversion list:"
boolean focusrectangle = false
end type

