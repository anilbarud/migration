$PBExportHeader$n_overpayment.sru
forward
global type n_overpayment from n_pdc
end type
end forward

global type n_overpayment from n_pdc autoinstantiate
end type

type variables
LONG il_claim_no

end variables

forward prototypes
public function integer nf_check_bus_rule ()
public function integer nf_check_mandatory ()
public function integer nf_set_defaults ()
public function integer nf_set_unused_fields ()
public function integer nf_change_item (long al_datawindow)
public function long nf_set_identifiers ()
public function long nf_get_next_identifier ()
public function integer nf_insert (long al_row, long al_claim_no)
public function integer nf_validate_overpayment_balance (integer ai_find, decimal adec_detail_amount)
public function datetime nf_get_max_op_adj_txn_date (long al_recipient_no, string as_recipient_type_code)
public function integer nf_set_last_action_date ()
public function datetime nf_get_max_overpayment_detail_date (string as_overpayment_type_code, long al_recipient_no, string as_recipient_type_code)
public subroutine nf_set_instance (long al_claim_no)
public subroutine nf_set_credit_amount (ref decimal adec_detail_amount, string as_db_cr, string as_mode)
public function integer nf_validate_op_status (decimal adec_new_balance, string as_balance_mode)
end prototypes

public function integer nf_check_bus_rule ();DATETIME	ldtm_overpayment, ldtm_scheduled_repayment_date, ldtm_null
DECIMAL	ldec_balance_amount, ldec_detail_amount, ldec_new_balance
INTEGER  li_msg, li_rowcount, li_find, li_inserted_row, li_rtn
LONG		ll_recipient_no, ll_rows
STRING	ls_db_cr, ls_recipient_type_code, ls_message, ls_claim_role_code, ls_comment, ls_overpayment_type_code, ls_find
STRING	ls_error_msg, ls_op_status_code, ls_status_comment, ls_zero_balance_flag, ls_op_status_desc
DWItemStatus   ldwis_overpayment_status

IF idw_dw[1].GetRow() > 0 THEN
	
	/*
	1.30	The date must be entered. 
	*/
	ldtm_overpayment = idw_dw[1].GetItemDateTime(1,'overpayment_date')
	IF Date(ldtm_overpayment) < Date('1900/01/01') THEN
		MessageBox('Invalid Overpayment Date', 'The overpayment date is invalid.')
		idw_dw[1].SetColumn('overpayment_date')
		idw_dw[1].SetFocus()
		Return -1
	END IF
	
	ldec_detail_amount = idw_dw[1].GetItemDecimal(1,'detail_amount')
	IF IsNull(ldec_detail_amount) THEN
		ldec_detail_amount = 0.00
	END IF
		
	IF ldec_detail_amount < 0 THEN
	/*		
		1.50	The overpayment detail amount entered must be greater than or equal to zero.
		
		only allow entries of positive number because the system will negate the number depending
		upon the setting of the debit/credit flag
	*/
		MessageBox('Invalid Amount', 'The amount entered must not be negative.')
		idw_dw[1].SetColumn('detail_amount')
		idw_dw[1].SetFocus()
		Return -1
	END IF
	
	
	IF ldec_detail_amount = 0.00 THEN
		IF MessageBox('Zero Amount?','You have entered $0 for the overpayment amount. Do you want to continue?',Question!,YesNo!,2) = 2 THEN
			idw_dw[1].SetColumn('detail_amount')
			idw_dw[1].SetFocus()
			Return -1
		END IF
	END IF
	
	
	ll_recipient_no        = idw_dw[1].GetItemNumber(1,'recipient_no')
	ls_recipient_type_code = idw_dw[1].GetItemString(1,'recipient_type_code')
	ls_db_cr               = idw_dw[1].GetItemString(1,'db_cr')
	
	// set credit amount to be negative, if necessary
	nf_set_credit_amount(ldec_detail_amount,ls_db_cr,'set')
	
	
	IF idw_dw[1].GetItemString(1,'overpayment_type_code') = 'A' AND ls_db_cr = 'D' THEN
		ldtm_scheduled_repayment_date = idw_dw[1].GetItemDateTime(1,'scheduled_repayment_date')
	END IF
	
	
	// Validate overpayment comment
	SELECT claim_role_code
	INTO   :ls_claim_role_code
	FROM   CLAIM_PARTICIPANT
	WHERE  claim_no = :il_claim_no
	AND    individual_no = :ll_recipient_no
	USING SQLCA;
	SQLCA.nf_handle_error('n_overpayment','embedded SQL: SELECT claim_role_code FROM CLAIM_PARTICIPANT...', 'nf_check_bus_rule')
	
	ls_comment = Trim(idw_dw[1].GetItemString(1,'comment'))
	IF IsNull(ls_comment) THEN
		ls_comment = ''
	END IF
	
	IF ls_claim_role_code = 'C' THEN
		// BR 1.80	A comment must contain at least five characters.
		IF Len(ls_comment) < 5 THEN
			MessageBox('Enter Comment', 'You must enter a comment at least five characters long.')
			idw_dw[1].SetColumn('comment')
			idw_dw[1].SetFocus()
			
			// reset credit amount to be positive, if necessary
			nf_set_credit_amount(ldec_detail_amount,ls_db_cr,'reset')
				
			RETURN -1
		END IF
		
	ELSE
		// BR 1.80	A comment must contain at least five characters.
		IF Len(ls_comment) < 5 THEN
			MessageBox('Enter Comment', 'You must enter a comment at least five characters long.')
			idw_dw[1].SetColumn('comment')
			idw_dw[1].SetFocus()
			
			// reset credit amount to be positive, if necessary
			nf_set_credit_amount(ldec_detail_amount,ls_db_cr,'reset')
	
			RETURN -1
		END IF
		
		IF ldec_detail_amount > 0 THEN
			// BR 1.90 The comment should indicate who was overpaid or who is being advanced monies, 
			// if the claim participant is other than the claimant and the amount is greater than zero.
			li_msg = MessageBox('Save Comment?', ' For a non-claimant recipient, the comment should indicate ' &
													  + '~r~nwho was overpaid or who is being advanced monies. Do you' &
													  + '~r~nwish to continue this save?',Question!,YesNo!,2)
			IF li_msg = 2 THEN
				idw_dw[1].SetColumn('comment')
				idw_dw[1].SetFocus()
			
				// reset credit amount to be positive, if necessary
				nf_set_credit_amount(ldec_detail_amount,ls_db_cr,'reset')
				
				RETURN -1
			END IF
		END IF
	END IF
	
	
	
	/*	check for the balance information - this must be done here since we do not know
	what the overpayment type is until the end
	*/
	li_rowcount = idw_dw[3].RowCount()
	
	ls_overpayment_type_code = idw_dw[1].GetItemString(1,'overpayment_type_code')
	ls_find = 'overpayment_type_code = "' +ls_overpayment_type_code+ '" and recipient_no = ' +String(ll_recipient_no)+ ' and recipient_type_code = "' +ls_recipient_type_code+ '"'
	
	IF li_rowcount > 0 THEN li_find = idw_dw[3].Find(ls_find,1,li_rowcount)
	
	IF ls_db_cr = 'C' THEN
		IF li_find > 0 THEN
			li_rtn = nf_validate_overpayment_balance(li_find,ldec_detail_amount)
			IF li_rtn < 0 THEN
				/*
				1.120	The overpayment balance for a recipient and overpayment type should not be less than zero.
				*/
				IF MessageBox('Negative Balance', 'An overpayment credit should not be more than the overpayment balance for the recipient and the overpayment type.' &
													  +'~r~n~r~nThis overpayment MUST not be saved unless you have over-collected an overpayment for this recipient and overpayment type for this claim.'&
													  +'~r~n~r~nThe negative balance must be written off or issued to the recipient.'&
													  +'~r~n~r~nDo you want to continue with the saving of this overpayment?',Exclamation!,YesNo!,2) = 2 THEN
					idw_dw[1].SetColumn('detail_amount')
					idw_dw[1].SetFocus()
						
					// reset credit amount to be positive, if necessary
					nf_set_credit_amount(ldec_detail_amount,ls_db_cr,'reset')
		
					
					Return -1
				END IF
			END IF
		ELSE
			IF MessageBox('Negative Balance', 'An overpayment credit should not be more than the overpayment balance for the recipient and the overpayment type.' &
												  +'~r~n~r~nThis overpayment MUST not be saved unless you have over-collected an overpayment for this recipient and overpayment type for this claim.'&
												  +'~r~n~r~nThe negative balance must be written off or issued to the recipient.'&
												  +'~r~n~r~nDo you want to continue with the saving of this overpayment?',Exclamation!,YesNo!,2) = 2 THEN
				idw_dw[1].SetColumn('detail_amount')
				idw_dw[1].SetFocus()
					
				// reset credit amount to be positive, if necessary
				nf_set_credit_amount(ldec_detail_amount,ls_db_cr,'reset')
	
				
				Return -1
			END IF
		END IF
	END IF
	
	
	
	IF li_find = 0 THEN
		li_rtn = nf_validate_op_status(ldec_detail_amount,'ADD')
		IF li_rtn < 0 THEN
			// reset credit amount to be positive, if necessary
			nf_set_credit_amount(ldec_detail_amount,ls_db_cr,'reset')
			RETURN -1
		END IF
		
		// insert OVERPAYMENT_BALANCE record
		ls_op_status_code = idw_dw[2].GetItemString(1,'op_status_code')
		IF IsNull(ls_op_status_code) THEN ls_op_status_code = ''
		ls_status_comment = idw_dw[2].GetItemString(1,'comment')
		IF IsNull(ls_status_comment) THEN ls_status_comment = ''
		
		idw_dw[2].Reset()
		li_inserted_row = idw_dw[2].InsertRow(0)
		idw_dw[2].SetItem(li_inserted_row,'claim_no',il_claim_no)
		idw_dw[2].SetItem(li_inserted_row,'recipient_no',ll_recipient_no)
		idw_dw[2].SetItem(li_inserted_row,'recipient_type_code','I')
		idw_dw[2].SetItem(li_inserted_row,'recipient_sub_type_code',' ')
		idw_dw[2].SetItem(li_inserted_row,'overpayment_type_code',ls_overpayment_type_code)
		IF ls_overpayment_type_code = 'O' THEN
			SetNull(ldtm_scheduled_repayment_date)
		END IF		
		idw_dw[2].SetItem(li_inserted_row,'scheduled_repayment_date',ldtm_scheduled_repayment_date)
		idw_dw[2].SetItem(li_inserted_row,'balance_amount', ldec_detail_amount)
		idw_dw[2].SetItem(li_inserted_row,'op_status_code', ls_op_status_code)
		idw_dw[2].SetItem(li_inserted_row,'comment', ls_status_comment)
		
		
	ELSE
		
		// update OVERPAYMENT_BALANCE record found		
		ldec_balance_amount = idw_dw[2].GetItemDecimal(1,'balance_amount')
		IF IsNull(ldec_balance_amount) THEN
			ldec_balance_amount = 0
		END IF
		
		ldec_new_balance = ldec_balance_amount + ldec_detail_amount
		
		li_rtn = nf_validate_op_status(ldec_new_balance,'ADD')
		IF li_rtn < 0 THEN
			// reset credit amount to be positive, if necessary
			nf_set_credit_amount(ldec_detail_amount,ls_db_cr,'reset')
			RETURN -1
		END IF		
		
		idw_dw[2].SetItem(1,'balance_amount', ldec_balance_amount + ldec_detail_amount)
		// do not reset the sched repayment dt unless its a debit advance.
		IF idw_dw[1].GetItemString(1,'overpayment_type_code') = 'A' THEN
			IF ls_db_cr = 'D' THEN
				idw_dw[2].SetItem(1,'scheduled_repayment_date',ldtm_scheduled_repayment_date)
			ELSE
				// do nothing
			END IF
		ELSE
			SetNull(ldtm_null)
			idw_dw[2].SetItem(1,'scheduled_repayment_date',ldtm_null)
		END IF
	END IF
ELSE
	// only updating the OVERPAYMENT_BALANCE record
	ldec_balance_amount = idw_dw[2].GetItemDecimal(1,'balance_amount')
	li_rtn = nf_validate_op_status(ldec_balance_amount,'STATUS')
	IF li_rtn < 0 THEN
		RETURN -1
	END IF
END IF



Return 0
end function

public function integer nf_check_mandatory ();STRING   ls_op_status_code


IF idw_dw[1].GetRow() > 0 THEN
	IF idw_dw[1].AcceptText() < 0 THEN Return -1
//	IF idw_dw[2].AcceptText() < 0 THEN Return -1

	IF IsNull(idw_dw[1].GetItemDateTime(1,'overpayment_date')) THEN
		MessageBox('Missing Overpayment Date','The date of the overpayment must be entered.  Please enter.')
		idw_dw[1].SetColumn('overpayment_date')
		idw_dw[1].SetFocus()
		Return -1
	END IF

	IF IsNull(idw_dw[1].GetItemNumber(1,'recipient_no')) THEN
		MessageBox('Missing Recipient','The recipient is missing.  Please close out, select a claimant and try again.')
		Return -1
	END IF

	IF IsNull(idw_dw[1].GetItemString(1,'recipient_type_code')) THEN
		idw_dw[1].SetItem(1,'recipient_type_code','I')
	END IF

	IF IsNull(idw_dw[1].GetItemString(1,'overpayment_type_code')) THEN
		MessageBox('Missing Overpayment Type','The overpayment type must be specified.')
		Return -1
	END IF

	IF IsNull(idw_dw[1].GetItemNumber(1,'claim_no')) THEN
		MessageBox('Missing Claim','The claim number is missing.  Please close out, select a claim and try again.')
		Return -1
	END IF
	IF idw_dw[1].GetItemString(1,'overpayment_type_code') = 'A' AND idw_dw[1].GetItemString(1,'db_cr') = 'D' AND &
		IsNull(idw_dw[1].GetItemDateTime(1,'scheduled_repayment_date')) THEN
		MessageBox('Missing Repayment Date','The scheduled repayment date must be entered for debits of ADVANCE overpayments.')
		Return -1
	END IF
END IF

ls_op_status_code = idw_dw[2].GetItemString(1,'op_status_code')
IF ls_op_status_code = '' OR IsNull(ls_op_status_code) THEN
	MessageBox('No O/P Status','An overpayment status must be selected.')
	RETURN -1
END IF
	
Return 0
end function

public function integer nf_set_defaults ();DATE	ldt_date


IF idw_dw[1].GetRow() > 0 THEN

/*	for now the recipient will always be the claimant
*/
	idw_dw[1].SetItem(1,'recipient_type_code','I')
	idw_dw[1].SetItem(1,'recipient_sub_type_code',' ')
	idw_dw[1].SetItem(1,'claim_no', il_claim_no)
	idw_dw[1].SetItem(1,'overpayment_type_code', 'O')
	ldt_date = Date(f_server_datetime())
	idw_dw[1].SetItem(1,'overpayment_date',ldt_date)

END IF

Return 0
end function

public function integer nf_set_unused_fields ();
IF idw_dw[1].GetRow() > 0 THEN
	IF IsNull(idw_dw[1].GetItemString(1,'comment')) THEN
		idw_dw[1].SetItem(1,'comment',' ')
	END IF
	IF IsNull(idw_dw[1].GetItemString(1,'recipient_sub_type_code')) THEN
		idw_dw[1].SetItem(1,'recipient_sub_type_code',' ')
	END IF
END IF
	
Return 0
end function

public function integer nf_change_item (long al_datawindow);DWItemStatus     ldwis_status
INTEGER          li_current_row, li_master_list_find, li_master_rows, li_status_rows
LONG             ll_new_recipient_no, ll_old_recipient_no, ll_current_recipient_no
STRING           ls_column_name, ls_column_desc, ls_old_overpayment_type_code, ls_new_overpayment_type_code, ls_master_list_find
STRING           ls_overpayment_type_code, ls_recipient_type_code




CHOOSE CASE al_datawindow
	CASE 1
		li_current_row = idw_dw[1].GetRow()
		ls_column_name = idw_dw[1].GetColumnName()
		
		CHOOSE CASE ls_column_name
			CASE "overpayment_date"   
				IF Left(idw_dw[1].GetText(),10) < '1900/01/01' THEN
					MessageBox('Invalid Overpayment Date','The overpayment date is invalid.')

					// this should check that the overpayment date is not before the first payment!!
					idw_dw[1].SetColumn('overpayment_date')
					idw_dw[1].SetFocus()
					Return -1
				END IF
				
			CASE 'recipient_no', 'overpayment_type_code'
				// changing these two columns can affect the OVERPAYMENT_BALANCE record				
				ll_old_recipient_no = idw_dw[1].GetItemNumber(1,'recipient_no')
				ls_old_overpayment_type_code = idw_dw[1].GetItemString(1,'overpayment_type_code')
				
				li_master_rows = idw_dw[3].RowCount()
				
				IF li_master_rows = 0 THEN
					// new record, skip this
				ELSE
					
					IF ls_column_name = 'recipient_no' THEN
						ls_column_desc = 'recipient'
						ll_new_recipient_no          = LONG(idw_dw[1].GetText())
						ls_old_overpayment_type_code = idw_dw[1].GetItemString(li_current_row,'overpayment_type_code')
						ls_master_list_find = 'recipient_no = ' + String(ll_new_recipient_no) + ' and overpayment_type_code = "' +ls_old_overpayment_type_code+ '"'
					ELSE
						ls_column_desc = 'overpayment type'
						ll_old_recipient_no          = idw_dw[1].GetItemNumber(li_current_row,'recipient_no')
						ls_new_overpayment_type_code = idw_dw[1].GetText()
						ls_master_list_find = 'recipient_no = ' + String(ll_old_recipient_no) + ' and overpayment_type_code = "' +ls_new_overpayment_type_code+ '"'
					END IF
					
					ldwis_status = idw_dw[2].GetItemStatus(1,0,Primary!)
					
					li_master_list_find = idw_dw[3].Find(ls_master_list_find,1,li_master_rows)
					IF li_master_list_find > 0 THEN
						// OVERPAYMENT_BALANCE record in master list was found
						CHOOSE CASE ldwis_status
							CASE New!, NotModified!
								// status/comment was new or not modified
								idw_dw[2].Reset()
								li_master_rows = idw_dw[3].Retrieve(il_claim_no)
								SQLCA.nf_handle_error('n_overpayment','idw_dw[3].Retrieve','nf_change_item')
								
								idw_dw[3].SetRow(li_master_list_find)
								idw_dw[3].ScrollToRow(li_master_list_find)
								idw_dw[3].SelectRow(li_master_list_find,TRUE)
								
								ls_overpayment_type_code = idw_dw[3].GetItemString(li_master_list_find,'overpayment_type_code')
								ll_current_recipient_no  = idw_dw[3].GetItemNumber(li_master_list_find,'recipient_no')
								ls_recipient_type_code   = idw_dw[3].GetItemString(li_master_list_find,'recipient_type_code')
								
								li_status_rows = idw_dw[2].Retrieve(il_claim_no,ls_overpayment_type_code,ll_current_recipient_no,ls_recipient_type_code)
								
							CASE DataModified!, NewModified!
								// if there is a change to either column, then determine if there is an existing record in the master
								// that matches on these two columns
								// if there is a match, and the current 'status' row is new or new modified, the remove & retrieve
								
								IF ls_column_name = 'recipient_no' THEN								
									idw_dw[1].SetItem(1,'recipient_no',ll_old_recipient_no)							
								ELSE
									idw_dw[1].SetItem(1,'overpayment_type_code',ls_old_overpayment_type_code)
								END IF
								
								MessageBox('O/P Status Modified','The overpayment status has been modified. Please save or cancel changes before modifying the '+ls_column_desc+'.')
								RETURN -1
								
						END CHOOSE
					ELSE
						// OVERPAYMENT_BALANCE record in master list was not found
						CHOOSE CASE ldwis_status
							CASE DataModified!, NewModified!
								// if there is no match, and the current 'status' row is data modified or new modified, then prevent change, because
								// user should save 'status' changes first
								MessageBox('O/P Status Modified','The overpayment status has been modified. Please save or cancel changes before modifying the '+ls_column_desc+'.')
								IF ls_column_name = 'recipient_no' THEN
									idw_dw[1].SetItem(1,'recipient_no',ll_old_recipient_no)							
								ELSE
									idw_dw[1].SetItem(1,'overpayment_type_code',ls_old_overpayment_type_code)
								END IF
								
								RETURN -1
							CASE ELSE
								// if there is no match, and and the current 'status' row is new or not modified
								// then insert row into status dw
								idw_dw[2].Reset()
								idw_dw[2].InsertRow(0)
						END CHOOSE
					END IF
				END IF
		END CHOOSE
				
END CHOOSE

// THIS FUNCTION HAS BEEN KEPT FOR FUTURE USE
//nf_set_last_action_date()

Return 0
end function

public function long nf_set_identifiers ();LONG	ll_overpayment_no

IF idw_dw[1].GetRow() > 0 THEN
	ll_overpayment_no = nf_get_next_identifier()

	IF ll_overpayment_no <= 0 THEN
		MessageBox('Error','Unable to determine next identifier.')
		Return -1
	END IF
		
	idw_dw[1].SetItem(1,'overpayment_no',ll_overpayment_no)
END IF

Return 0
	
end function

public function long nf_get_next_identifier ();LONG	ll_result, ll_no

// To ensure that we get the next number, Update the Last_Overpayment_no table incrementing the  
// last_overpayment_no by 1  (This will lock it so no one else can get in). Then, read it back                 
UPDATE Last_Overpayment_No SET last_overpayment_no = last_overpayment_no + 1 using SQLCA;
ll_result = SQLCA.nf_handle_error("Embedded SQL: Update Last_Overpayment_No","n_overpayment","nf_next_identifier")
IF ll_result < 0 THEN
	Return -1
END IF

IF SQLCA.SQLNRows = 1 THEN  // successful (ie. SQLNRows would equal 1), read back the identifier
	SELECT last_overpayment_no 
	  INTO :ll_no 
	  FROM Last_Overpayment_No 
	 using SQLCA;
	ll_result = SQLCA.nf_handle_error("Embedded SQL: Update Last_Overpayment_No","n_overpayments","nf_next_identifier")
	IF ll_result < 0 THEN
		Return -1
	END IF
ELSE // display error
	SQLCA.nf_rollback_transaction()
	IF SQLCA.SQLCode <> 0 THEN
		Error.Text = "Error during rollback of Last_Overpayment_No in function nf_next_identifier"
		Error.WindowMenu=""
		Error.Object="n_overpayment"
		Error.ObjectEvent="nf_next_identifier"
		SignalError()
	END IF		
	MessageBox("Data Integrity Error", String(SQLCA.SQLNRows) + " record(s) found in Last_Overpayment_No~r~nPlease call the help desk",Exclamation!)
	RETURN -1
END IF

RETURN ll_no

end function

public function integer nf_insert (long al_row, long al_claim_no);INTEGER   li_detail_insert_row, li_balance_insert_row

	idw_dw[1].Reset()
	li_detail_insert_row = idw_dw[1].InsertRow(0)
	SQLCA.nf_handle_error('Insert of overpayment details','n_overpayment','nf_insert')

	IF li_detail_insert_row <> 1 THEN
		MessageBox('Insert Error','Unable to insert overpayment detail.  Contact Helpdesk.')
		RETURN -1
	END IF
	
	IF idw_dw[2].RowCount() = 0 THEN
		// insert OVERPAYMENT_BALANCE
		li_balance_insert_row = idw_dw[2].InsertRow(0)
		IF li_balance_insert_row <> 1 THEN
			MessageBox('Insert Error','Unable to insert overpayment balance.  Contact Helpdesk.')
			RETURN -1
		END IF
	END IF

	nf_set_defaults()
	idw_dw[1].SetFocus()
Return 0


end function

public function integer nf_validate_overpayment_balance (integer ai_find, decimal adec_detail_amount);DECIMAL    ldec_overpayment_balance

/* 
1.120	An overpayment credit cannot be more than the overpayment balance for the recipient and the overpayment type.
*/

IF ai_find = 0 THEN
	// no OP balance yet, so BR violated
	RETURN -1
ELSE
	ldec_overpayment_balance = idw_dw[3].GetItemDecimal(ai_find,'balance_amount')
	IF ldec_overpayment_balance + adec_detail_amount < 0 THEN
		// new OP balance would be negative, so BR violated
		RETURN -1
	END IF
END IF

return 0
end function

public function datetime nf_get_max_op_adj_txn_date (long al_recipient_no, string as_recipient_type_code);DATETIME     ldtm_max_op_adj_txn_date

/*

Returns the maximum processed date of an overpayment recovery (J3) transaction
that is associated with the current claim, recipient and recipient type.

*/

SELECT MAX(processed_date)
INTO   :ldtm_max_op_adj_txn_date
FROM   APPLIED_CLAIM_TXN
WHERE  txn_type_code       = 'J'
AND    txn_sub_type_code   = '3'
AND    claim_no            = :il_claim_no
AND    recipient_no        = :al_recipient_no
AND    recipient_type_code = :as_recipient_type_code
USING SQLCA;
SQLCA.nf_handle_error('n_overpayment','embedded SQL: SELECT MAX(processed_date) FROM APPLIED_CLAIM_TXN...','nf_get_max_op_adj_txn_date')


RETURN ldtm_max_op_adj_txn_date
end function

public function integer nf_set_last_action_date ();DATETIME   ldtm_current_last_action_date, ldtm_max_op_adj_txn_date, ldtm_max_overpayment_detail_date, ldtm_max_date
INTEGER    li_master_row
LONG       ll_recipient_no
STRING     ls_overpayment_type_code, ls_recipient_type_code



li_master_row = idw_dw[2].GetRow()

IF li_master_row > 0 THEN
	
	ll_recipient_no               = idw_dw[2].GetItemNumber(li_master_row,'recipient_no')
	ls_recipient_type_code        = idw_dw[2].GetItemString(li_master_row,'recipient_type_code')
	ls_overpayment_type_code      = idw_dw[2].GetItemString(li_master_row,'overpayment_type_code')
	ldtm_current_last_action_date = idw_dw[2].GetItemDateTime(li_master_row,'last_action_date')
	
	// get the max overpayment recovery (J3) transaction associated with the claim, recipient and recipient type
	// that is same as overpayment currently selected
	ldtm_max_op_adj_txn_date = nf_get_max_op_adj_txn_date(ll_recipient_no,ls_recipient_type_code)
	
	// get the max overpayment date associated with the claim, O/P type, recipient and recipient type
	// that is same as overpayment currently selected	
	ldtm_max_overpayment_detail_date = nf_get_max_overpayment_detail_date(ls_overpayment_type_code,ll_recipient_no,ls_recipient_type_code)
	
	// compare these two dates
	IF ( ldtm_max_op_adj_txn_date > ldtm_max_overpayment_detail_date ) OR IsNull(ldtm_max_overpayment_detail_date) THEN
		ldtm_max_date = ldtm_max_op_adj_txn_date
	ELSEIF ( ldtm_max_overpayment_detail_date > ldtm_max_op_adj_txn_date ) OR IsNull(ldtm_max_op_adj_txn_date) THEN
		ldtm_max_date = ldtm_max_overpayment_detail_date
	END IF
	
	// only update value if it will be changed
	IF IsNull(ldtm_current_last_action_date) THEN
		idw_dw[2].SetItem(li_master_row,'last_action_date',ldtm_max_date)
	ELSE
		IF ldtm_max_date <> ldtm_current_last_action_date THEN
			idw_dw[2].SetItem(li_master_row,'last_action_date',ldtm_max_date)
		END IF
	END IF
	
END IF

RETURN 0
end function

public function datetime nf_get_max_overpayment_detail_date (string as_overpayment_type_code, long al_recipient_no, string as_recipient_type_code);DATETIME     ldtm_max_overpayment_date, ldtm_current_overpayment_date, ldtm_max_date

/*

Returns the maximum overpayment date between existing OVERPAYMENT_DETAIL records
that are associated with the current claim, O/P type, recipient and recipient type,
and an OVERPAYMENT_DETAIL record that is currently being saved (if one exists).

*/

SELECT MAX(overpayment_date)
INTO   :ldtm_max_overpayment_date
FROM   OVERPAYMENT_DETAIL
WHERE  claim_no              = :il_claim_no
AND    overpayment_type_code = :as_overpayment_type_code
AND    recipient_no          = :al_recipient_no
AND    recipient_type_code   = :as_recipient_type_code
USING SQLCA;
SQLCA.nf_handle_error('n_overpayment','embedded SQL: SELECT MAX(overpayment_date) FROM OVERPAYMENT_DETAIL...','nf_max_overpayment_detail_date')

IF idw_dw[1].GetRow() > 0 THEN
	IF idw_dw[1].GetItemStatus(1,'overpayment_date',Primary!) <> New! THEN
		ldtm_current_overpayment_date = idw_dw[1].GetItemDateTime(1,'overpayment_date')
		
		IF IsNull(ldtm_current_overpayment_date) THEN
			ldtm_max_date = ldtm_max_overpayment_date
		ELSE
			IF ldtm_current_overpayment_date > ldtm_max_overpayment_date THEN
				ldtm_max_date = ldtm_current_overpayment_date
			ELSE
				ldtm_max_date = ldtm_max_overpayment_date
			END IF
		END IF
	END IF
ELSE
	ldtm_max_date = ldtm_max_overpayment_date
END IF



RETURN ldtm_max_date
end function

public subroutine nf_set_instance (long al_claim_no);il_claim_no = al_claim_no
end subroutine

public subroutine nf_set_credit_amount (ref decimal adec_detail_amount, string as_db_cr, string as_mode);STRING   ls_db_cr



//	a credit is recorded as a negative amount
IF as_db_cr = 'C' THEN
	IF as_mode = 'set' OR adec_detail_amount < 0.00 THEN
		// if the amount is being set for the first time, or 
		// it is being reset to be positive due to an error
		adec_detail_amount = adec_detail_amount * -1
	END IF	
END IF
idw_dw[1].SetItem(1,'detail_amount', adec_detail_amount)
end subroutine

public function integer nf_validate_op_status (decimal adec_new_balance, string as_balance_mode);STRING	ls_op_status_code, ls_zero_balance_flag, ls_pos_balance_flag, ls_neg_balance_flag, ls_op_status_desc


/*
2.30		A non-zero balance overpayment status must be selected if the overpayment has a non-zero balance.
See Rationale

2.40		A zero balance overpayment status must be selected if the overpayment has a zero balance.
See Rationale
*/
ls_op_status_code   = idw_dw[2].GetItemString(1,'op_status_code')

SELECT op_status_desc,
       zero_balance_flag,
		 pos_balance_flag,
		 neg_balance_flag
INTO   :ls_op_status_desc,
       :ls_zero_balance_flag,
		 :ls_pos_balance_flag,
		 :ls_neg_balance_flag
FROM   Op_Status
WHERE  op_status_code = :ls_op_status_code
USING SQLCA;
SQLCA.nf_handle_error('n_overpayment','embedded SQL: SELECT zero_balance_flag FROM Op_Status...','nf_check_bus_rule')

IF as_balance_mode = 'ADD' THEN
	// if the user is adding an OVERPAYMENT_DETAIL record
	IF adec_new_balance > 0 AND ls_pos_balance_flag = 'N' THEN
		MessageBox('O/P Status Error','"'+ls_op_status_desc+'" can only be selected as an overpayment status if the overpayment balance is zero.' &
		                            + '~r~n~r~n' &
											 + 'Please correct the overpayment detail amount or the overpayment status.' ,Exclamation!)
		RETURN -1
	END IF
	
	IF adec_new_balance = 0 AND ls_zero_balance_flag = 'N' THEN
		MessageBox('O/P Status Error','"'+ls_op_status_desc+'" can only be selected as an overpayment status if the overpayment balance is not zero.' &
		                            + '~r~n~r~n' &
											 + 'Please correct the overpayment detail amount or the overpayment status.' ,Exclamation!)		
		RETURN -1
	END IF
	
	IF adec_new_balance < 0 AND ls_neg_balance_flag = 'N' THEN
		MessageBox('O/P Status Error','If the overpayment balance is less than zero, then only "Recovered" can be selected as an overpayment status.' &
		                            + '~r~n~r~n' &
											 + 'Please correct the overpayment detail amount or the overpayment status.' ,Exclamation!)
		RETURN -1
	END IF
	
ELSE
	// if the user is only modifying the status
	IF adec_new_balance > 0 AND ls_pos_balance_flag = 'N' THEN
		MessageBox('O/P Status Error','"'+ls_op_status_desc+'" can only be selected as an overpayment status if the overpayment balance is zero.' &
		                            + '~r~n~r~n' &
											 + 'Please correct the overpayment status.' ,Exclamation!)
		RETURN -1
	END IF
	
	IF adec_new_balance = 0 AND ls_zero_balance_flag = 'N' THEN
		MessageBox('O/P Status Error','"'+ls_op_status_desc+'" can only be selected as an overpayment status if the overpayment balance is not zero.' &
		                            + '~r~n~r~n' &
											 + 'Please correct the overpayment status.' ,Exclamation!)
		RETURN -1
	END IF
	
	IF adec_new_balance < 0 AND ls_neg_balance_flag = 'N' THEN
		MessageBox('O/P Status Error','If the overpayment balance is less than zero, then only "Recovered" can be selected as an overpayment status.' &
		                            + '~r~n~r~n' &
											 + 'Please correct the overpayment status.' ,Exclamation!)
		RETURN -1
	END IF
END IF

RETURN 0
end function

on n_overpayment.create
call super::create
end on

on n_overpayment.destroy
call super::destroy
end on

