$PBExportHeader$w_account_payment_maintenance.srw
$PBExportComments$Account Payments - Window used to display & maintain account type payments
forward
global type w_account_payment_maintenance from w_a_tool
end type
type dw_account_period_overlap from u_dw_online within w_account_payment_maintenance
end type
type cb_add_inv from commandbutton within w_account_payment_maintenance
end type
type cb_next from commandbutton within w_account_payment_maintenance
end type
type cb_prev from commandbutton within w_account_payment_maintenance
end type
type cb_ok from commandbutton within w_account_payment_maintenance
end type
type cb_cancel from commandbutton within w_account_payment_maintenance
end type
type cb_authorization from commandbutton within w_account_payment_maintenance
end type
type dw_account_authorizations from u_dw_online within w_account_payment_maintenance
end type
type dw_account_authorizations_2 from u_dw_online within w_account_payment_maintenance
end type
type dw_account_payment_2 from u_dw_online within w_account_payment_maintenance
end type
type dw_account_transaction_2 from u_dw_online within w_account_payment_maintenance
end type
type uo_payment from u_payment within w_account_payment_maintenance
end type
type dw_payment_document from u_dw_online within w_account_payment_maintenance
end type
type dw_efb_payment_to_delete from u_dw_online within w_account_payment_maintenance
end type
type dw_efb_txn_to_delete from u_dw_online within w_account_payment_maintenance
end type
type dw_efb_payment_document_to_delete from u_dw_online within w_account_payment_maintenance
end type
type dw_payment_document_2 from u_dw_online within w_account_payment_maintenance
end type
type dw_iw_travel_expense from u_dw_online within w_account_payment_maintenance
end type
type cb_travel from commandbutton within w_account_payment_maintenance
end type
type dw_iw_travel_expense_delete from u_dw_online within w_account_payment_maintenance
end type
end forward

global type w_account_payment_maintenance from w_a_tool
integer y = 380
integer width = 2921
integer height = 1828
boolean controlmenu = true
boolean resizable = false
event ue_postopen ( )
dw_account_period_overlap dw_account_period_overlap
cb_add_inv cb_add_inv
cb_next cb_next
cb_prev cb_prev
cb_ok cb_ok
cb_cancel cb_cancel
cb_authorization cb_authorization
dw_account_authorizations dw_account_authorizations
dw_account_authorizations_2 dw_account_authorizations_2
dw_account_payment_2 dw_account_payment_2
dw_account_transaction_2 dw_account_transaction_2
uo_payment uo_payment
dw_payment_document dw_payment_document
dw_efb_payment_to_delete dw_efb_payment_to_delete
dw_efb_txn_to_delete dw_efb_txn_to_delete
dw_efb_payment_document_to_delete dw_efb_payment_document_to_delete
dw_payment_document_2 dw_payment_document_2
dw_iw_travel_expense dw_iw_travel_expense
cb_travel cb_travel
dw_iw_travel_expense_delete dw_iw_travel_expense_delete
end type
global w_account_payment_maintenance w_account_payment_maintenance

type variables
N_ACCOUNT_PAYMENT_CONTROLLER inv_controller
n_tax_functions   inv_tax_functions
S_WINDOW_MESSAGE istr_message

W_ACCOUNT_PAYMENT	iwi_parent
w_inbasket                          iwi_parent_window


end variables

forward prototypes
public subroutine wf_set_payment_object (n_payment anv_payment)
public subroutine wf_set_parent_window (window awi_parent)
public function integer wf_refresh_recipient ()
public function integer wf_verify_bank_info (long al_recipient_no, string as_recipient_type_code)
public function integer wf_cancel_travel_expense (integer ar_iw_travel_expense_no)
end prototypes

event ue_postopen();Long     ll_docid, ll_wrc_invoice_no, ll_recipient_no, ll_payment_no, ll_row, ll_row_count
Long     ll_individual_no, ll_service_provider_no, ll_claim_no, ll_txn_no, ll_rehab_invoice_no
STRING ls_recipient_type_code, ls_bank_no, ls_bank_account_no, ls_bank_transit_no
Integer  li_batch_no, li_error, li_rtn
Decimal  ld_balance_owing
String   ls_document_type, ls_paid_status, ls_pymt_recpt_type, ls_pymt_type, ls_pymt_sub_type, ls_pymt_tax_flag
Datetime ldt_processed_date, ldt_paid_from_date, ldt_paid_to_date, ldt_epay_implementation_date, ldt_date_on_document, ldtm_today
Boolean  lb_protect = TRUE
STRING	ls_module_source_code, ls_paid_status_code
datawindowchild ldwc_child2
n_wrc_invoice lnv_wrc_invoice

ldtm_today = f_server_datetime()

ls_document_type = istr_message.as_stringparm[1]
ll_payment_no = istr_message.al_doubleparm[1]
ll_claim_no = istr_message.al_doubleparm[2]
ll_docid = istr_message.al_doubleparm[3]

IF 	istr_message.as_mode = "display" AND istr_message.as_stringparm[2] = "ADVANCE" THEN
	ll_rehab_invoice_no =  istr_message.al_doubleparm[6]
ELSE
	ll_rehab_invoice_no = 0
END IF

IF ll_docid > 0 THEN
	SELECT service_provider_no, date_on_document 
	  INTO :ll_service_provider_no, :ldt_date_on_document 
	  FROM DOCUMENT_INDEX 
	 WHERE docid = :ll_docid 
	USING imagetrans ;

	li_rtn = imagetrans.nf_handle_error("w_account_payment_maintenance", "", "ue_postopen - SELECT service_provider_no, date_on_document FROM DOCUMENT_INDEX")
ELSE
	ll_service_provider_no = 0
	SetNull(ldt_date_on_document)
END IF

lnv_wrc_invoice  = CREATE n_wrc_invoice

uo_payment.dw_basic_claim.SetTransObject(SQLCA)
uo_payment.dw_basic_claim.Retrieve(istr_message.al_doubleparm[2])  // claim_no

ldt_epay_implementation_date = Datetime(Date(ProfileString(vgs_ini_filename, "EPay", "EpayImplementationDate", "")))

inv_controller.inv_payment.nf_set_basic_claim(uo_payment.dw_basic_claim)


//	determine the mode this window is open for and retrieve or insert payments accordingly
CHOOSE CASE istr_message.as_mode
	CASE 'reject', 'inbasketreject'	// Note - rejecting documents, not payments
		st_title.text = "Reject document # " + string(istr_message.al_doubleparm[3])
		inv_controller.nf_reject()
		cb_add_inv.visible = FALSE
		cb_next.visible    = FALSE
		cb_prev.visible    = FALSE
		cb_ok.text = "&Save"
		cb_travel.visible = FALSE
	CASE 'add', 'advance', 'inbasketadd'
		st_title.text = "Add payment for document # " + string(istr_message.al_doubleparm[3])
		inv_controller.nf_insert(istr_message.al_doubleparm[2])
		IF istr_message.al_doubleparm[5] > 0 THEN
			uo_payment.dw_account_payment.SetItem(1,'authorization_no', istr_message.al_doubleparm[5])
		END IF
		IF inv_controller.inv_payment.idw_basic_claim.GetItemNumber(1,'cost_alloc_no') = 0 OR &
			inv_controller.inv_payment.idw_basic_claim.GetItemNumber(1,'cost_alloc_operation_no') = 0  THEN
			MessageBox('Warning','No cost allocation information.  Payment cannot be made.')
			Close(This)
			Return
		END IF

		cb_ok.text = "&Save"
		
	// If document_type_code = "Prescription Drugs Via ABCC"
		IF istr_message.as_stringparm[1] = "ARX" THEN
			uo_payment.dw_account_payment.SetItem(1, "payment_type_code", "22")
			uo_payment.dw_account_payment.SetItem(1, "payment_sub_type_code", "RC")

			SELECT paid_from_date, paid_to_date 
			  INTO :ldt_paid_from_date, :ldt_paid_to_date 
			  FROM PAYMENT 
			 WHERE payment_no = :ll_payment_no ; 
			
			li_rtn = SQLCA.nf_handle_error("w_account_payment_maintenance", "", "ue_postopen - SELECT paid_from_date, paid_to_date FROM PAYMENT")
			
			SELECT ISNULL(individual_no, 0)  
			  INTO :ll_individual_no 
			  FROM CLAIM  
			 WHERE claim_no = :ll_claim_no ;

			li_rtn = SQLCA.nf_handle_error("w_account_payment_maintenance", "", "ue_postopen - SELECT ISNULL(individual_no, 0) FROM CLAIM")
			
			uo_payment.dw_account_payment.SetItem(1, "paid_from_date", ldt_paid_from_date)
			uo_payment.dw_account_payment.SetItem(1, "paid_to_date", ldt_paid_to_date)
			uo_payment.dw_account_transactions.SetItem(1, "recipient_type_code", "I")
			uo_payment.dw_account_transactions.SetItem(1, "recipient_no", ll_individual_no)

			// Disable columns
			uo_payment.dw_account_payment.Modify("payment_type_code.protect=1     ")
			uo_payment.dw_account_payment.Modify("payment_sub_type_code.protect=1 ")
			uo_payment.dw_account_payment.Modify("paid_from_date.protect=1        ")
			uo_payment.dw_account_payment.Modify("paid_to_date.protect=1          ")

			IF ldt_date_on_document < ldt_epay_implementation_date THEN
				uo_payment.dw_account_payment.Modify("authorization_no.protect=0   ")
				cb_authorization.Enabled = TRUE
			ELSE
				uo_payment.dw_account_payment.Modify("authorization_no.protect=1   ")
				cb_authorization.Enabled = FALSE
			END IF

			uo_payment.cb_search2.Enabled = FALSE
			uo_payment.dw_account_transactions.Modify("recipient_type_code.protect=1     ")
			uo_payment.dw_account_transactions.Modify("recipient_no.protect=1            ")
			
			// Set the Related Txn number
			SELECT MAX(P.payment_no)  
			  INTO :ll_payment_no 
			  FROM PAYMENT P, 
			       PAYMENT_DOCUMENT PD 
			 WHERE PD.doc_id = :ll_docid 
				AND PD.payment_no = P.payment_no 
				AND P.payment_type_code = "22" 
				AND P.payment_sub_type_code = "BC" ;

			li_rtn = SQLCA.nf_handle_error("w_account_payment_maintenance", "", "ue_postopen - SELECT payment_no FROM PAYMENT")
			
			SELECT txn_no 
			  INTO :ll_txn_no 
			  FROM APPLIED_CLAIM_TXN 
			 WHERE payment_no = :ll_payment_no 
			   AND txn_type_code = "1" ; 
			
			li_rtn = SQLCA.nf_handle_error("w_account_payment_maintenance", "", "ue_postopen - SELECT txn_no FROM APPLIED_CLAIM_TXN")

			IF li_rtn = 100 THEN
				SELECT txn_no 
				  INTO :ll_txn_no 
				  FROM UNAPPLIED_CLAIM_TXN 
				 WHERE payment_no = :ll_payment_no 
				   AND txn_type_code = "1" ; 
				
				li_rtn = SQLCA.nf_handle_error("w_account_payment_maintenance", "", "ue_postopen - SELECT txn_no FROM UNAPPLIED_CLAIM_TXN")
			END IF

			IF li_rtn = 0 THEN
				uo_payment.dw_account_transactions.SetItem(1, "related_txn_no", ll_txn_no)
			END IF
		ELSEIF istr_message.as_stringparm[1] = "AR" THEN
			// If document_type_code = "AR" = "Prescription Accounts" 
			
			uo_payment.dw_account_payment.SetItem(1, "payment_type_code", "22")
			uo_payment.dw_account_payment.SetItem(1, "payment_sub_type_code", " ")
			uo_payment.dw_account_transactions.SetItem(1, "recipient_type_code", "M")
			uo_payment.dw_account_transactions.SetItem(1, "recipient_no", ll_service_provider_no)
			
			// Disable columns
			uo_payment.dw_account_payment.Modify("payment_type_code.protect=1        ")
			uo_payment.dw_account_payment.Modify("payment_sub_type_code.protect=1    ")
			uo_payment.dw_account_transactions.Modify("recipient_type_code.protect=1 ")
			uo_payment.dw_account_transactions.Modify("recipient_no.protect=1        ")
			uo_payment.cb_search2.Enabled = FALSE
			
		ELSE
			uo_payment.dw_account_payment.Modify("payment_type_code.protect=0     ")
			uo_payment.dw_account_payment.Modify("payment_sub_type_code.protect=0 ")
			uo_payment.dw_account_payment.Modify("paid_from_date.protect=0        ")
			uo_payment.dw_account_payment.Modify("paid_to_date.protect=0          ")
			uo_payment.dw_account_payment.Modify("authorization_no.protect=0      ")
			cb_authorization.Enabled = TRUE

			uo_payment.cb_search2.Enabled = TRUE
			uo_payment.dw_account_transactions.Modify("recipient_type_code.protect=0     ")
			uo_payment.dw_account_transactions.Modify("recipient_no.protect=0            ")
			
			END IF
	CASE 'delete'
		
		cb_add_inv.visible 		= FALSE
		cb_next.visible    		= FALSE
		cb_prev.visible    		= FALSE
		cb_travel.visible 			= FALSE
		
		IF inv_controller.nf_retrieve_for_delete(istr_message.al_doubleparm[1]) < 0 THEN
			iwi_parent.cb_refresh.PostEvent("ue_update_cancelled")
			Close(This)
			Return
		ELSE
			IF istr_message.al_doubleparm[3] > 0 THEN
				st_title.text = "Confirm delete of entry for document # " + string(istr_message.al_doubleparm[3])
			ELSE
				st_title.text = "Confirm delete of payment # " + string(istr_message.al_doubleparm[1])
			END IF
		END IF
		cb_ok.text = "&Delete"
	CASE 'display'		// Note - this is 'Update' mode
		IF inv_controller.nf_retrieve(istr_message.al_doubleparm[1]) < 0 THEN
			Close(This)
			Return
		END IF
		IF istr_message.as_stringparm[2] = "ADVANCE" THEN
			// no associated document therefore there will be no document paid status
		ELSE
			cb_ok.text = "&Save"
		END IF

		// Imported Prescription payments cannot be updated.  Disable Save button
		ll_row = uo_payment.dw_account_transactions.GetRow()
		IF ll_row > 0 THEN
			ls_pymt_type = uo_payment.dw_account_payment.GetItemString(ll_row, 'payment_type_code')
			ls_pymt_sub_type = uo_payment.dw_account_payment.GetItemString(ll_row, 'payment_sub_type_code')
			IF ls_pymt_type = "22" AND ls_pymt_sub_type = "BC" THEN
				cb_ok.Enabled = FALSE
			END IF
		END IF
		
		// If document_type_code = "Prescription Drugs Via ABCC"
		IF istr_message.as_stringparm[1] = "ARX" THEN
			// Disable columns
			uo_payment.dw_account_payment.Modify("payment_type_code.protect=1     ")
			uo_payment.dw_account_payment.Modify("payment_sub_type_code.protect=1 ")
			uo_payment.dw_account_payment.Modify("paid_from_date.protect=1        ")
			uo_payment.dw_account_payment.Modify("paid_to_date.protect=1          ")

			IF ldt_date_on_document < ldt_epay_implementation_date THEN
				uo_payment.dw_account_payment.Modify("authorization_no.protect=0   ")
				cb_authorization.Enabled = TRUE
			ELSE
				uo_payment.dw_account_payment.Modify("authorization_no.protect=1   ")
				cb_authorization.Enabled = FALSE
			END IF

			uo_payment.cb_search2.Enabled = FALSE
			uo_payment.dw_account_transactions.Modify("recipient_type_code.protect=1     ")
			uo_payment.dw_account_transactions.Modify("recipient_no.protect=1            ")
			
		ELSEIF istr_message.as_stringparm[1] = "AR" THEN
			// Disable columns
			uo_payment.dw_account_payment.Modify("payment_type_code.protect=1        ")
			uo_payment.dw_account_payment.Modify("payment_sub_type_code.protect=1    ")
			uo_payment.dw_account_transactions.Modify("recipient_type_code.protect=1 ")
			uo_payment.dw_account_transactions.Modify("recipient_no.protect=1        ")
			uo_payment.cb_search2.Enabled = FALSE
		
		ELSE
			uo_payment.dw_account_payment.Modify("payment_type_code.protect=0     ")
			uo_payment.dw_account_payment.Modify("payment_sub_type_code.protect=0 ")
			uo_payment.dw_account_payment.Modify("paid_from_date.protect=0        ")
			uo_payment.dw_account_payment.Modify("paid_to_date.protect=0          ")
			uo_payment.dw_account_payment.Modify("authorization_no.protect=0      ")
			cb_authorization.Enabled = TRUE

			uo_payment.cb_search2.Enabled = TRUE
			uo_payment.dw_account_transactions.Modify("recipient_type_code.protect=0     ")
			uo_payment.dw_account_transactions.Modify("recipient_no.protect=0            ")
			
		END IF
END CHOOSE
uo_payment.uf_set_account_on()

IF istr_message.as_mode <> 'reject'	THEN
	ll_row = uo_payment.dw_account_transactions.GetRow()
	IF ll_row > 0 THEN
		ls_pymt_recpt_type = uo_payment.dw_account_transactions.GetItemString ( ll_row, 'recipient_type_code' )
		ls_pymt_type       = uo_payment.dw_account_payment.GetItemString ( ll_row, 'payment_type_code' )
		ls_pymt_sub_type   = uo_payment.dw_account_payment.GetItemString ( ll_row, 'payment_sub_type_code' )
		li_batch_no        = uo_payment.dw_account_transactions.GetItemNumber( ll_row, 'batch_no' )
		ldt_processed_date = uo_payment.dw_account_transactions.GetItemDateTime( ll_row, 'processed_date' )
	END IF
END IF

IF uo_payment.dw_account_payment.RowCount() > 0 AND istr_message.as_mode = 'display' AND uo_payment.dw_account_transactions.RowCount() > 0 THEN
	IF NOT IsNull(uo_payment.dw_account_payment.GetItemDateTime(1,'processed_date')) OR uo_payment.dw_account_transactions.GetItemNumber(1,'batch_no') > 0 THEN
		uo_payment.dw_account_payment.enabled = FALSE
		uo_payment.dw_account_transactions.enabled = FALSE
		cb_add_inv.enabled = FALSE
		cb_ok.enabled = FALSE
		uo_payment.dw_invoice.enabled = FALSE
		uo_payment.cb_search.enabled = FALSE
		uo_payment.cb_search2.enabled = FALSE
	END IF	
	IF (ls_pymt_type = "21" AND ( ls_pymt_sub_type = "01" OR ls_pymt_sub_type = "09" ) ) OR (ls_pymt_type = "25" AND ls_pymt_sub_type = "01" and ll_rehab_invoice_no <> 0) OR ( ls_pymt_type = "50" and ll_rehab_invoice_no <> 0) THEN
			uo_payment.dw_account_payment.enabled      = FALSE
			uo_payment.dw_account_transactions.enabled = FALSE
			cb_add_inv.enabled                         = FALSE
			cb_ok.enabled                              = FALSE
			uo_payment.dw_invoice.enabled              = FALSE
			uo_payment.cb_search.enabled               = FALSE
			uo_payment.cb_search2.enabled              = FALSE	
	END IF
	
ELSEIF istr_message.as_mode = 'reject' OR istr_message.as_mode = 'delete' OR istr_message.as_mode = 'inbasketreject' THEN
	uo_payment.dw_account_payment.enabled      = FALSE
	uo_payment.dw_account_transactions.enabled = FALSE
	cb_add_inv.enabled                         = FALSE
	uo_payment.dw_invoice.enabled              = FALSE
	uo_payment.cb_search.enabled               = FALSE
	uo_payment.cb_search2.enabled              = FALSE
	uo_payment.cb_search.visible               = FALSE
	uo_payment.cb_search2.visible              = FALSE
	cb_authorization.enabled                   = FALSE
ELSEIF dw_payment_document.RowCount() > 0  AND istr_message.as_mode = 'display' THEN
	uo_payment.dw_invoice.visible = FALSE
	cb_add_inv.visible            = FALSE
	uo_payment.cb_search.visible  = FALSE
	uo_payment.cb_search2.visible = FALSE
	cb_next.visible               = FALSE
	cb_prev.visible               = FALSE
END IF

//************************************************************************************
//wrc_invoicing
//************************************************************************************
IF ls_document_type = "AH" THEN
	ll_wrc_invoice_no = lnv_wrc_invoice.nf_get_invoice_number(ll_docid)
	If ll_wrc_invoice_no > 0 THEN
		inv_controller.nf_protect_columns('')
		cb_add_inv.enabled = False
		uo_payment.cb_search2.enabled = False
			
		CHOOSE CASE istr_message.as_mode
			CASE 'add', 'advance', 'inbasketadd','display'
				ld_balance_owing = lnv_wrc_invoice.nf_get_invoice_amount(ll_wrc_invoice_no)
				
				ll_row_count = uo_payment.dw_account_payment.RowCount()
				
				IF ll_row_count > 0 THEN 
					uo_payment.dw_account_payment.SetItem(1,"submitted_amount",ld_balance_owing)
		
					IF uo_payment.dw_account_payment.GetItemdecimal(1,'total_award_amount') = 0 THEN
						//check to see if there is a balance owing
						IF lnv_wrc_invoice.nf_get_balance_owing(ll_wrc_invoice_no) > 0 THEN
				
							// set the payment_method_code = "A" automated cheque and than let the business rules do the rest
							uo_payment.dw_account_transactions.object.payment_method_code.protect = 0
							
							ll_recipient_no = uo_payment.dw_account_transactions.GetItemNumber(1,'recipient_no') 
							ls_recipient_type_code = uo_payment.dw_account_transactions.GetItemString(1,'recipient_type_code')
							IF ls_recipient_type_code <> 'I' THEN
								SELECT bank_no, bank_account_no, bank_transit_no
								INTO      :ls_bank_no, :ls_bank_account_no, :ls_bank_transit_no
								FROM    BANK_INFO 		
								WHERE recipient_type_code = :ls_recipient_type_code
								AND       recipient_no = :ll_recipient_no
								USING    SQLCA;
								
								li_rtn = SQLCA.nf_handle_error("w_account_payment_maintenance", "", "ue_postopen - SELECT bank_no, bank_account_no, bank_transit_no FROM BANK_INFO")
								
								IF ls_bank_no > '' THEN 
									uo_payment.dw_account_transactions.SetItem(1,'payment_method_code','D')
									uo_payment.dw_account_transactions.SetItem(1,'bank_no',ls_bank_no)
									uo_payment.dw_account_transactions.SetItem(1,'bank_account_no',ls_bank_account_no)
									uo_payment.dw_account_transactions.SetItem(1,'bank_transit_no',ls_bank_transit_no)
								ELSE
									uo_payment.dw_account_transactions.SetItem(1,"payment_method_code","A")
								END IF
							END IF
							uo_payment.dw_account_transactions.object.payment_method_code.protect = 1
						END IF 
					END IF 
				END IF 
		END CHOOSE
	end if
end if

// PR9225 - The Paid Quantity field is enterable when the payment status is 'Entered'
ll_wrc_invoice_no = lnv_wrc_invoice.nf_get_invoice_number(ll_docid)
If ll_wrc_invoice_no > 0 THEN
	uo_payment.dw_account_payment.object.paid_quantity.protect = 0
END IF

//************************************************************************************
//Hospital e-invoice
//************************************************************************************

SELECT module_source_code
INTO :ls_module_source_code
FROM DOC
WHERE docid = :ll_docid
USING SQLCA;

SQLCA.nf_handle_error("w_account_payment_maintenance", "ue_postopen","SELECT module_source_code")

IF ls_module_source_code = '12' Then
	inv_controller.nf_protect_columns(ls_module_source_code)
	cb_add_inv.enabled = False
	uo_payment.cb_search2.enabled = False
END IF



// check the tax flag to see if the column needs to be protected
ll_row = uo_payment.dw_account_transactions.GetRow()
IF ll_row > 0 THEN
	ls_pymt_recpt_type = uo_payment.dw_account_transactions.GetItemString(ll_row, 'recipient_type_code')
	ls_pymt_type       = uo_payment.dw_account_payment.GetItemString(ll_row, 'payment_type_code')
	ls_pymt_sub_type   = uo_payment.dw_account_payment.GetItemString(ll_row, 'payment_sub_type_code')
	ll_recipient_no    = uo_payment.dw_account_transactions.GetItemnumber(ll_row, 'recipient_no')

	// if needed fiter the sub type on the payment to start with
	IF ls_pymt_type = "21" THEN
		ldwc_child2.SetFilter('')
		uo_payment.dw_account_payment.GetChild('payment_sub_type_code',ldwc_child2)
		IF istr_message.as_stringparm[3] = 'WCA' THEN
			li_rtn = ldwc_child2.SetFilter( "payment_type_code = '21' and payment_sub_type_code in ('02','10','12') ")
		ELSEIF istr_message.as_stringparm[3] = 'FCA' THEN
			li_rtn = ldwc_child2.SetFilter( "payment_type_code = '21' and payment_sub_type_code in ('10','11') ")
		END IF
		ldwc_child2.Filter()
	END IF
			
	li_error = inv_tax_functions.nf_check_tax_flag(ls_pymt_type,ls_pymt_sub_type,ls_pymt_recpt_type,ll_recipient_no,lb_protect)
	IF lb_protect = TRUE THEN uo_payment.dw_account_payment.object.tax_amount.protect = 1
END IF 

ll_row = uo_payment.dw_account_transactions.GetRow()
IF ll_row > 0 THEN
	IF uo_payment.dw_account_payment.GetItemString(1, "payment_type_code") = "23"  and Date(ldtm_today) >= Date(gdtm_ephysio_implementation_date) THEN
		cb_travel.enabled = True
	END IF
END IF
	
uo_payment.dw_account_payment.SetFocus()
end event

public subroutine wf_set_payment_object (n_payment anv_payment);
end subroutine

public subroutine wf_set_parent_window (window awi_parent);IF istr_message.as_mode = 'inbasketadd' or istr_message.as_mode = 'inbasketreject' THEN
	iwi_parent_window = awi_parent
ELSE
	iwi_parent = awi_parent
END IF
end subroutine

public function integer wf_refresh_recipient ();/*  PR5435 and PR5208 2006-02-03 r.s.
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
LONG ll_recipient_no, ll_row

ll_row = uo_payment.dw_account_transactions.getRow()
IF ll_row > 0 THEN
	ls_recipient_type_code = uo_payment.dw_account_transactions.getItemString(ll_row, 'recipient_type_code')
	//First get the recipient no, this is the same as the individual_no in INDIVIDUAL table 
	
	ll_recipient_no = uo_payment.dw_account_transactions.getItemNumber(ll_row,'recipient_no')
	if ll_recipient_no < 1 then
		Messagebox("Save Error", "The recipient number is not a valid number") 
		return -1
	END IF
	
	// Next, if this is a Direct Deposit payment, check that the bank information has not been removed 
	// (another user might have removed it since this window was opened)	
	ls_payment_method_code = uo_payment.dw_account_transactions.getItemString(ll_row, 'payment_method_code')
	IF ls_payment_method_code = 'D' THEN // yes, its a Direct Deposit
		IF wf_verify_bank_info(ll_recipient_no,  ls_recipient_type_code) < 0 THEN
			uo_payment.dw_transaction_details.ScrollToRow(ll_row)
			Messagebox("Save Error", "This recipients banking information has been removed. You cannot save a Direct Deposit payment at this time. Choose another payment method type.") 
			RETURN -1
		END IF
		
		// Update the bank information, just in case it has been modified by another user 
		uo_payment.dw_account_transactions.setitem (ll_row,'bank_no', ls_bank_no)
		uo_payment.dw_account_transactions.setitem (ll_row,'bank_transit_no', ls_bank_transit_no)
		uo_payment.dw_account_transactions.setitem (ll_row,'bank_account_no', ls_bank_account_no)
	
	END IF
	
	// Now refresh address information
	// check the 'use_default_address_flag, proceed if it is a 'Y'
	ls_use_default_address = uo_payment.dw_account_transactions.getItemString(ll_row, 'use_default_address_flag')
	
	IF ls_use_default_address = 'Y' THEN
		/* PR5669 - J.Hawker, 2006.04.06 - Address was being overwritten. Called function to refresh
			the information, but keep the address the same.
		*/
		ls_cheque_print_group_code = uo_payment.dw_account_transactions.GetItemString(ll_row, 'cheque_print_group_code')
		inv_controller.inv_payment.nf_setup_address(ls_recipient_type_code, ll_recipient_no,ll_row,ls_cheque_print_group_code)
	END IF
END IF

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

public function integer wf_cancel_travel_expense (integer ar_iw_travel_expense_no);

Delete 
From IW_TRAVEL_EXPENSE
Where iw_travel_expense_no = :ar_iw_travel_expense_no
Using SQLCA;

SQLCA.nf_handle_error('w_account_payment_maintenance','wf_cancel_travel_expense','delete')

Return 1
end function

event open;call super::open;Date ldt_ephysio_implementation_date, ldt_today
U_DWA ldw_dw[]


ldt_today = Date(f_server_datetime())
ldt_ephysio_implementation_date = Date(gdtm_ephysio_implementation_date)

	istr_message= Message.PowerObjectParm
	inv_controller = Create n_account_payment_controller
	inv_tax_functions  = create n_tax_functions
/*	pass all the info to the object
*/
	inv_controller.istr_message = istr_message

	ldw_dw[1] = uo_payment.dw_account_payment
	ldw_dw[2] = uo_payment.dw_account_transactions
	ldw_dw[3] = dw_EFB_payment_to_delete
	ldw_dw[4] = dw_EFB_txn_to_delete
	ldw_dw[5] = uo_payment.dw_invoice
	ldw_dw[6] = dw_payment_document
	ldw_dw[7] = dw_account_authorizations
	ldw_dw[8] = dw_account_authorizations_2
/* project 10127 medical society agreement two new records need to be created
	depending if all BR's are met these two new records are for PAYMENT and
   UNAPPLIED_CLAIM_TXN
*/
	ldw_dw[9]  = dw_account_payment_2
	ldw_dw[10] = dw_account_transaction_2
	ldw_dw[11] = dw_payment_document_2
	ldw_dw[12] = dw_EFB_payment_document_to_delete
	ldw_dw[13] = dw_iw_travel_expense
	ldw_dw[14] = dw_iw_travel_expense_delete

	inv_controller.nf_set_window_parent(THIS)
	inv_controller.nf_set_datawindow(ldw_dw[],SQLCA)
	inv_controller.nf_init()

	inv_controller.is_authorization_type = 'act'
	
	If ldt_today >= ldt_ephysio_implementation_date THEN
		cb_travel.visible = TRUE
	ELSE
	//	cb_travel.visible = FALSE
	END IF
	
	THIS.PostEvent('ue_postopen')
end event

on w_account_payment_maintenance.create
int iCurrent
call super::create
this.dw_account_period_overlap=create dw_account_period_overlap
this.cb_add_inv=create cb_add_inv
this.cb_next=create cb_next
this.cb_prev=create cb_prev
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.cb_authorization=create cb_authorization
this.dw_account_authorizations=create dw_account_authorizations
this.dw_account_authorizations_2=create dw_account_authorizations_2
this.dw_account_payment_2=create dw_account_payment_2
this.dw_account_transaction_2=create dw_account_transaction_2
this.uo_payment=create uo_payment
this.dw_payment_document=create dw_payment_document
this.dw_efb_payment_to_delete=create dw_efb_payment_to_delete
this.dw_efb_txn_to_delete=create dw_efb_txn_to_delete
this.dw_efb_payment_document_to_delete=create dw_efb_payment_document_to_delete
this.dw_payment_document_2=create dw_payment_document_2
this.dw_iw_travel_expense=create dw_iw_travel_expense
this.cb_travel=create cb_travel
this.dw_iw_travel_expense_delete=create dw_iw_travel_expense_delete
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_account_period_overlap
this.Control[iCurrent+2]=this.cb_add_inv
this.Control[iCurrent+3]=this.cb_next
this.Control[iCurrent+4]=this.cb_prev
this.Control[iCurrent+5]=this.cb_ok
this.Control[iCurrent+6]=this.cb_cancel
this.Control[iCurrent+7]=this.cb_authorization
this.Control[iCurrent+8]=this.dw_account_authorizations
this.Control[iCurrent+9]=this.dw_account_authorizations_2
this.Control[iCurrent+10]=this.dw_account_payment_2
this.Control[iCurrent+11]=this.dw_account_transaction_2
this.Control[iCurrent+12]=this.uo_payment
this.Control[iCurrent+13]=this.dw_payment_document
this.Control[iCurrent+14]=this.dw_efb_payment_to_delete
this.Control[iCurrent+15]=this.dw_efb_txn_to_delete
this.Control[iCurrent+16]=this.dw_efb_payment_document_to_delete
this.Control[iCurrent+17]=this.dw_payment_document_2
this.Control[iCurrent+18]=this.dw_iw_travel_expense
this.Control[iCurrent+19]=this.cb_travel
this.Control[iCurrent+20]=this.dw_iw_travel_expense_delete
end on

on w_account_payment_maintenance.destroy
call super::destroy
destroy(this.dw_account_period_overlap)
destroy(this.cb_add_inv)
destroy(this.cb_next)
destroy(this.cb_prev)
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.cb_authorization)
destroy(this.dw_account_authorizations)
destroy(this.dw_account_authorizations_2)
destroy(this.dw_account_payment_2)
destroy(this.dw_account_transaction_2)
destroy(this.uo_payment)
destroy(this.dw_payment_document)
destroy(this.dw_efb_payment_to_delete)
destroy(this.dw_efb_txn_to_delete)
destroy(this.dw_efb_payment_document_to_delete)
destroy(this.dw_payment_document_2)
destroy(this.dw_iw_travel_expense)
destroy(this.cb_travel)
destroy(this.dw_iw_travel_expense_delete)
end on

type st_title from w_a_tool`st_title within w_account_payment_maintenance
integer x = 14
integer y = 12
integer width = 2880
string text = "Account Payment Maintenance"
end type

type cb_close from w_a_tool`cb_close within w_account_payment_maintenance
boolean visible = false
integer x = 3566
integer y = 1660
integer taborder = 60
boolean enabled = false
end type

type dw_account_period_overlap from u_dw_online within w_account_payment_maintenance
boolean visible = false
integer x = 3479
integer y = 248
integer height = 364
integer taborder = 140
string dataobject = "d_account_period_overlap"
end type

event constructor;/* Overridden for performance reasons.  No column level security needed. */
end event

type cb_add_inv from commandbutton within w_account_payment_maintenance
integer x = 2555
integer y = 312
integer width = 279
integer height = 76
integer taborder = 90
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Add Inv#"
end type

on clicked;uo_payment.dw_invoice.InsertRow(0)
uo_payment.dw_invoice.SetFocus()
end on

type cb_next from commandbutton within w_account_payment_maintenance
integer x = 2158
integer y = 1500
integer width = 251
integer height = 76
integer taborder = 110
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Next>>"
end type

on clicked;IF uo_payment.dw_account_transactions.GetRow() < uo_payment.dw_account_transactions.RowCount() THEN
	uo_payment.dw_account_transactions.ScrollToRow(uo_payment.dw_account_transactions.GetRow() + 1)
ELSE
	beep(2)
END IF
end on

type cb_prev from commandbutton within w_account_payment_maintenance
integer x = 1623
integer y = 1500
integer width = 251
integer height = 76
integer taborder = 100
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "<< &Prev"
end type

on clicked;IF uo_payment.dw_account_transactions.GetRow() > 1 THEN
	uo_payment.dw_account_transactions.ScrollToRow(uo_payment.dw_account_transactions.GetRow() - 1)
ELSE
	beep(2)
END IF
end on

type cb_ok from commandbutton within w_account_payment_maintenance
integer x = 983
integer y = 1680
integer width = 379
integer height = 100
integer taborder = 130
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Save"
end type

event clicked;
Long     ll_row, ll_docid, ll_service_provider_no, ll_rows, ll_recip_row, ll_recipient_no, ll_max, ll_rehab_invoice_no, ll_claim_no, ll_payment_no
Integer  li_rtn, row, li_travel, li_travel_expense_no, li_line_no, li_trancount
String   ls_paid_status, ls_temp, ls_payment_type_code, ls_payment_sub_type_code, ls_physio_recipient_type, ls_physio_recipient_sub_type_code
String   ls_document_type, ls_ephysio_flag
Boolean  lb_rejected
Datetime ldt_epay_implementation_date, ldt_date_on_document, ldt_paid_from_date, ldt_paid_to_date, ldtm_ephysio_implementation_date
Date     ldt_today 

string ls_mode
long ll_count

ldt_today = date(f_server_datetime())


N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '002' refers to the Account Payment Maintenance module, '044' refers to the Payment Processing module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('002','044','save',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF
/******************************************************************************************/

SetPointer(HourGlass!)
li_rtn = uo_payment.dw_account_payment.AcceptText()
//li_rtn = uo_payment.dw_account_transactions.AcceptText()

ll_row = uo_payment.dw_account_payment.GetRow()
IF ll_row > 0 THEN
	IF istr_message.as_stringparm[2] = "ADVANCE" THEN
		// no doc therefore no doc paid status, so don't check for rejected status
	ELSE
		ls_paid_status = dw_payment_document.GetItemString(dw_payment_document.GetRow(), 'paid_status_code')
		IF ls_paid_status = 'R' OR ls_paid_status = 'H' OR ls_paid_status = 'O' THEN
			lb_rejected = TRUE
		END IF
	END IF
END IF

// Get some info on document
ll_docid = istr_message.al_doubleparm[3]
IF ll_docid > 0 THEN
	SELECT service_provider_no, date_on_document
	  INTO :ll_service_provider_no, :ldt_date_on_document 
	  FROM DOCUMENT_INDEX 
	 WHERE docid = :ll_docid 
	 USING imagetrans;

	li_rtn = imagetrans.nf_handle_error("w_account_payment_maintenance", "", "cb_ok - SELECT service_provider_no, date_on_document FROM IMARA_DB..DOCUMENT_INDEX")
ELSE
	ll_service_provider_no = 0
	SetNull(ldt_date_on_document)
END IF

ldt_epay_implementation_date = Datetime(Date(ProfileString(vgs_ini_filename, "EPay", "EpayImplementationDate", "")))
ldtm_ephysio_implementation_date = gdtm_ephysio_implementation_date


// Do some validations if in 'add', 'advance', 'inbasketadd', or 'display' mode
IF istr_message.as_mode = 'display' OR istr_message.as_mode = 'add' OR istr_message.as_mode = 'advance' OR istr_message.as_mode = 'inbasketadd' THEN
	// Prescriptions submitted by the pharmacy with a service date prior to the 
	// Epay implementation date will be paid via the Account Payment module
	ls_document_type = istr_message.as_stringparm[1]
	
	IF ll_row > 0 THEN

		// Check Drugs Payment Type
		// Make sure user's aren't creating 22 - BC's
		ls_payment_type_code = uo_payment.dw_account_payment.GetItemString(ll_row, "payment_type_code")
		ls_payment_sub_type_code = Trim(uo_payment.dw_account_payment.GetItemString(ll_row, "payment_sub_type_code"))
		IF IsNull(ls_payment_sub_type_code) = TRUE THEN ls_payment_sub_type_code = ""

		ldt_paid_from_date = uo_payment.dw_account_payment.GetItemDatetime(ll_row, "paid_from_date")
		ldt_paid_to_date = uo_payment.dw_account_payment.GetItemDatetime(ll_row, "paid_to_date")

		IF ls_payment_type_code = "22" AND ls_payment_sub_type_code = "BC" THEN
			MessageBox("Invalid Payment type/subtype", "Payment Type = 'Drugs' and Payment Sub Type = 'ABCC Prescription Payment or Adjustment' " +&
						  "can only be created by ABCC.~r~rChoose a differenct combination and try again.", Information!)
			RETURN
		END IF

		// Advances on Drug payments are not allowed
		IF istr_message.as_mode = 'advance' AND ls_payment_type_code = "22" THEN
			MessageBox("No Advance Drug Payments", "Advances on Drug payments are not allowed.", Information!)
			RETURN
		END IF

		// Make sure user only create's 22 - RC's on ARX documents
		IF ls_document_type <> "ARX" AND ls_payment_type_code = "22" AND ls_payment_sub_type_code = "RC" THEN
			MessageBox("Invalid Document Type for Payment type/subtype", "Payment Type = 'Drugs' and Payment Sub Type = 'ABCC Claim Reimbursement Correction' " +&
						  "can only be created for ARX - 'Prescription Drugs Via ABCC' document types.~r~rChoose a differenct combination and try again.", Information!)
			RETURN
		END IF

		// A Drug Payment is only allowed if service date is before the EPay Implementation date 
		// and recipient is a service provider
		IF ll_docid > 0 THEN
			IF ls_payment_type_code = "22" AND ls_payment_sub_type_code = "" THEN
				IF ldt_date_on_document >= ldt_epay_implementation_date THEN
					//If the date on document is after the epay implementation date and the document is "AR"
					//then we know the service provider must be outside Canada  or Inside Canada but outside
					//the atlantic provinces. We are prevented from entering the module otherwise.
					IF ls_document_type <> "AR" Then				
										
						MessageBox("Drug Payment Disallowed", "A Drug Payment is not allowed when service date: " +&
									  String(ldt_date_on_document, "mmm dd, yyyy") + " is on or after the ABCC Epay implementation date:" +&
									  String(ldt_epay_implementation_date, "mmm dd, yyyy") + ".", Exclamation!)
						RETURN
					End if
				Elseif ldt_date_on_document < ldt_epay_implementation_date  Then
					//If the date on document is before the epay date then the paid period
					//must also be before the epay date. We do not validate paid period when
					//the date on document is on or after the epay date.
					IF (ldt_paid_from_date >= ldt_epay_implementation_date) OR (ldt_paid_to_date >= ldt_epay_implementation_date)  THEN		
						Messagebox("Invalid Dates", "The payment period must be less than the Epay Implemenation date: " +&
							String(ldt_epay_implementation_date, "mmm dd, yyyy") + " for invoices submitted by a pharmacy.", Exclamation!)
						RETURN
					End if
				END IF

				IF ll_service_provider_no <= 0 OR IsNull(ll_service_provider_no) THEN
					MessageBox("Drug Payment Disallowed", "A Drug Payment is only allowed when the recipient is a Service Provider.", Exclamation!)
					RETURN
				END IF
			END IF
		END IF
		
		// Check Physio Payment Type	
		
		ll_rows = uo_payment.dw_account_transactions.RowCount()
		ll_recip_row = uo_payment.dw_account_transactions.getRow()

		IF ll_docid > 0 THEN
			IF (ls_payment_type_code = "50" AND ls_payment_sub_type_code = "")THEN
				MessageBox('Error','Please choose a vaild payment subtype.',Exclamation!)
				Return
			END IF			
		END IF
		
		IF istr_message.as_stringparm[2] = "ADVANCE" and ((ls_payment_type_code = "50") OR (ls_payment_type_code = "25" AND ls_payment_sub_type_code = "01")) THEN
			MessageBox('Error','An advance payment cannot be made for Physio.',Exclamation!)
			Return
		END IF
		
		
		IF ldt_today >= Date(ldtm_ephysio_implementation_date) THEN
			
		FOR ll_recip_row = 1 to ll_rows   //Check each recipient
			
		ll_recipient_no = uo_payment.dw_account_transactions.getitemnumber(ll_recip_row,"recipient_no")
		ls_physio_recipient_type = uo_payment.dw_account_transactions.getitemstring(ll_recip_row,"recipient_type_code")
			
		Select ephysio_flag, provider_sub_type_code
		Into :ls_ephysio_flag, :ls_physio_recipient_sub_type_code
		From PROVIDER
		Where provider_no = :ll_recipient_no 
		and    provider_type_code = :ls_physio_recipient_type
		Using SQLCA;
			
		SQLCA.nf_handle_error('w_account_payment_maintenance','clicked - cb_ok','Select count from PROVIDER')
			
		IF ll_docid > 0 THEN
		
     		IF (ls_payment_type_code = "25" AND ls_payment_sub_type_code = "01") OR (ls_payment_type_code = "50")THEN	
			//	IF ldt_date_on_document >= ldtm_ephysio_implementation_date OR (ldt_paid_from_date >= ldtm_ephysio_implementation_date) OR (ldt_paid_to_date >= ldtm_ephysio_implementation_date)THEN
//				IF (ldt_paid_from_date >= ldtm_ephysio_implementation_date) OR (ldt_paid_to_date >= ldtm_ephysio_implementation_date)THEN
					//IF the recipient type = M and provider type = M and provider sub type = 35 // error msg
					IF ls_physio_recipient_type = 'M' and ls_physio_recipient_sub_type_code = '35' and ls_ephysio_flag = 'Y' THEN
						MessageBox('Error','This invoice must be returned to the Physio Clinic for billing through the Physio website.',Exclamation!)
						Return
					END IF
					IF ls_physio_recipient_type = 'M' and ls_physio_recipient_sub_type_code = '35' THEN
						MessageBox('Error','This invoice must be reimbursed through the Rehab Invoice module.',Exclamation!)
						Return
					END IF
					//IF the recipient type = I //error msg
					IF ls_physio_recipient_type = 'I' THEN
						MessageBox('Error','The invoice must be reimbursed through the Rehab Invoice module.',Exclamation!)
						Return					
					END IF
				END IF
//			END IF
		END IF
		
		ll_recip_row = ll_recip_row + 1
	NEXT
		
		
	ELSE
		ls_ephysio_flag = ''
	END IF
	
		
		
					
	END IF
END IF

// If making payments from the In Basket
IF istr_message.as_mode = 'inbasketadd' OR istr_message.as_mode = 'inbasketreject' THEN
	ls_temp = iwi_parent_window.wf_validate_list_data()
ELSE
	ls_temp = iwi_parent.wf_validate_list_data()
END IF

IF len(trim(ls_temp)) > 0 THEN
	Messagebox ("Data Error",ls_temp)
	RETURN
END IF

/* PR 5434 and 5208, 2006/02/03. 
	If statement checks that there is a valid transaction row,
	then calls new function wf_refresh_individual() to check individual information. */
/* P10151-140 - June 21, 2010 - JH
    Changed funtion name to wf_refresh_recipient() as it refreshes both individual and sp info.
*/

IF uo_payment.dw_account_transactions.getRow() > 0 THEN
	IF wf_refresh_recipient() < 0 THEN RETURN
END IF

IF istr_message.as_mode = 'inbasketadd' THEN
	
	SQLCA.nf_begin_transaction()

	IF inv_controller.nf_save() = 0 THEN
		SQLCA.nf_commit_transaction()

		IF Isvalid(w_travel_expense) then Close(w_travel_expense)
		Close(Parent)
		RETURN
	ELSE
		SQLCA.nf_transaction_count(li_trancount,0,this.classname(),'','',FALSE)
		IF li_trancount > 0 THEN
			SQLCA.nf_rollback_transaction()
		END IF

		RETURN
	END IF
END IF

// If rejecting payments from the In Basket
IF istr_message.as_mode = 'inbasketreject' THEN
	SQLCA.nf_begin_transaction()
		
	IF inv_controller.nf_save_reject() = 0 THEN
		SQLCA.nf_commit_transaction()
		
		IF Isvalid(w_travel_expense) then Close(w_travel_expense)
		Close(Parent)
		RETURN
	ELSE
		SQLCA.nf_transaction_count(li_trancount,0,this.classname(),'','',FALSE)
		IF li_trancount > 0 THEN
			SQLCA.nf_rollback_transaction()
		END IF
		RETURN
	END IF
END IF

// If payments are maintained from the Account Payments window
IF IsValid(w_travel_expense) THEN 
	IF istr_message.as_mode = 'display' AND dw_payment_document.ModifiedCount() <= 0  AND &
		uo_payment.dw_account_payment.ModifiedCount() <= 0 AND &
		uo_payment.dw_account_transactions.ModifiedCount() <= 0  AND &
		w_travel_expense.dw_travel_expense.ModifiedCount() <= 0 THEN
		iwi_parent.cb_refresh.PostEvent("ue_update_complete")
		IF Isvalid(w_travel_expense) then Close(w_travel_expense)
		Close(Parent)
		RETURN
	END IF
ELSE
	IF istr_message.as_mode = 'display' AND dw_payment_document.ModifiedCount() <= 0  AND &
		uo_payment.dw_account_payment.ModifiedCount() <= 0 AND &
		uo_payment.dw_account_transactions.ModifiedCount() <= 0  THEN
		iwi_parent.cb_refresh.PostEvent("ue_update_complete")
		IF Isvalid(w_travel_expense) then Close(w_travel_expense)
		Close(Parent)
		RETURN
	END IF
END IF


SQLCA.nf_begin_transaction()

IF istr_message.as_mode = 'delete' THEN
	IF inv_controller.nf_delete() = 0 THEN
		SQLCA.nf_commit_transaction()
		
		iwi_parent.cb_refresh.PostEvent("ue_update_complete")
		IF Isvalid(w_travel_expense) then Close(w_travel_expense)
		Close(Parent)
		RETURN
	ELSE
		SQLCA.nf_rollback_transaction()
	END IF
ELSEIF (dw_payment_document.ModifiedCount() > 0 AND istr_message.as_mode <> 'add') OR istr_message.as_mode = 'reject' THEN
	IF inv_controller.nf_save_reject() = 0 THEN
		SQLCA.nf_commit_transaction()
		
		iwi_parent.cb_refresh.PostEvent("ue_update_complete")
		IF Isvalid(w_travel_expense) then Close(w_travel_expense)
		Close(Parent)
		RETURN
	ELSE
		SQLCA.nf_rollback_transaction()
	END IF
ELSEIF inv_controller.nf_save() = 0 THEN
	SQLCA.nf_commit_transaction()

	iwi_parent.cb_refresh.PostEvent("ue_update_complete")

	// Display a nofication message when payment_type = "Drugs" and payment_sub_type = "ABCC Claim Reimbursement Correctn"
	IF ll_row > 0 THEN
		ls_payment_type_code = uo_payment.dw_account_payment.GetItemString(ll_row, "payment_type_code")
		ls_payment_sub_type_code = uo_payment.dw_account_payment.GetItemString(ll_row, "payment_sub_type_code")
		IF ls_payment_type_code = "22" AND ls_payment_sub_type_code = "RC" THEN
			Messagebox("Notify ABCC", "Notify ABCC about the correction so they can make the adjustment!!", Information!)
		END IF
	END IF
	
	IF Isvalid(w_travel_expense) then Close(w_travel_expense)
	Close(Parent)
	RETURN
ELSE
	SQLCA.nf_rollback_transaction()
END IF

// Display a nofication message when payment_type = "Drugs" and payment_sub_type = "ABCC Claim Reimbursement Correctn"
IF ll_row > 0 THEN
	ls_payment_type_code = uo_payment.dw_account_payment.GetItemString(ll_row, "payment_type_code")
	ls_payment_sub_type_code = uo_payment.dw_account_payment.GetItemString(ll_row, "payment_sub_type_code")
	IF ls_payment_type_code = "22" AND ls_payment_sub_type_code = "RC" THEN
		Messagebox("Notify ABCC", "Notify ABCC about the correction so they can make the adjustment!!", Information!)
	END IF
END IF

end event

type cb_cancel from commandbutton within w_account_payment_maintenance
integer x = 1367
integer y = 1680
integer width = 379
integer height = 100
integer taborder = 150
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
end type

event clicked;	SetPointer(HourGlass!)

	IF istr_message.as_mode <> "inbasketadd" AND istr_message.as_mode <> "inbasketreject" THEN
		iwi_parent.cb_refresh.PostEvent("ue_update_cancelled")
	END IF
	
	IF IsValid(w_travel_expense) THEN
		//get the iw_travel_expense_no and then delete it from the IW_TRAVEL_EXPENSE table if they saved it and then cancelled.
		Close(w_travel_expense)
	END IF

	Close(Parent)

end event

type cb_authorization from commandbutton within w_account_payment_maintenance
integer x = 1499
integer y = 576
integer width = 384
integer height = 64
integer taborder = 120
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Authorization"
end type

event clicked;S_WINDOW_MESSAGE	lstr_message


	lstr_message.al_doubleparm[1] = istr_message.al_doubleparm[2]
	OpenWithParm(w_account_authorizations,lstr_message)
	
	lstr_message = Message.PowerObjectParm
	
	IF lstr_message.as_mode = 'OK' THEN
		uo_payment.dw_account_payment.SetItem(1,'authorization_no', lstr_message.al_doubleparm[1])
	END IF
end event

type dw_account_authorizations from u_dw_online within w_account_payment_maintenance
boolean visible = false
integer x = 27
integer y = 1600
integer width = 114
integer height = 116
integer taborder = 50
string dataobject = "d_account_authorizations_update"
end type

event constructor;/* Overridden for performance reasons.  No column level security needed. */
end event

type dw_account_authorizations_2 from u_dw_online within w_account_payment_maintenance
boolean visible = false
integer x = 151
integer y = 1600
integer width = 114
integer height = 116
integer taborder = 10
string dataobject = "d_account_authorizations_update"
end type

event constructor;/* Overridden for performance reasons.  No column level security needed. */
end event

type dw_account_payment_2 from u_dw_online within w_account_payment_maintenance
boolean visible = false
integer x = 283
integer y = 1600
integer width = 114
integer height = 116
integer taborder = 40
string dataobject = "d_account_payment"
end type

event constructor;/* Overridden for performance reasons.  No column level security needed. */
end event

type dw_account_transaction_2 from u_dw_online within w_account_payment_maintenance
boolean visible = false
integer x = 407
integer y = 1600
integer width = 114
integer height = 116
integer taborder = 30
string dataobject = "d_claim_txn"
end type

event constructor;/* Overridden for performance reasons.  No column level security needed. */
end event

type uo_payment from u_payment within w_account_payment_maintenance
event ue_payment_type_changed pbm_custom13
integer y = 64
integer height = 1568
integer taborder = 70
end type

event ue_payment_type_changed;long ll_row
string ls_pymt_type, ls_pymt_sub_type
datetime ldtm_date

ldtm_date = f_server_datetime()

inv_controller.nf_change_payment_type(1)

//travel for physio
If Date(ldtm_date) >= Date(gdtm_ephysio_implementation_date) THEN

	ll_row = uo_payment.dw_account_transactions.GetRow()
	IF ll_row > 0 THEN
		ls_pymt_type = uo_payment.dw_account_payment.GetItemString(ll_row, 'payment_type_code')
		ls_pymt_sub_type = uo_payment.dw_account_payment.GetItemString(ll_row, 'payment_sub_type_code')
		IF ls_pymt_type = "23"  THEN
			cb_travel.Enabled = True
		Else
			cb_travel.Enabled = False
		END IF
	END IF
		
END IF
		
Return 
end event

on ue_itemfocuschanged;call u_payment::ue_itemfocuschanged;inv_controller.nf_trans_itemfocuschanged()
end on

event ue_txn_changes;call super::ue_txn_changes;Return inv_controller.nf_change_item(2)
end event

on uo_payment.destroy
call u_payment::destroy
end on

event ue_payment_changes;call super::ue_payment_changes;Return inv_controller.nf_change_item(1)
end event

type dw_payment_document from u_dw_online within w_account_payment_maintenance
boolean visible = false
integer x = 567
integer y = 464
integer width = 1390
integer height = 340
integer taborder = 80
string dataobject = "d_payment_document"
boolean border = false
end type

on itemchanged;call u_dw_online::itemchanged;This.uf_set_pbmessage(TRUE)
inv_controller.nf_change_item(8)
end on

event constructor;/* Overridden for performance reasons.  No column level security needed. */
end event

type dw_efb_payment_to_delete from u_dw_online within w_account_payment_maintenance
boolean visible = false
integer x = 846
integer y = 1600
integer width = 110
integer height = 128
integer taborder = 11
boolean bringtotop = true
string dataobject = "d_efb_payment_to_delete"
end type

type dw_efb_txn_to_delete from u_dw_online within w_account_payment_maintenance
boolean visible = false
integer x = 146
integer y = 1728
integer width = 293
integer height = 64
integer taborder = 21
boolean bringtotop = true
string dataobject = "d_efb_txn_to_delete"
end type

type dw_efb_payment_document_to_delete from u_dw_online within w_account_payment_maintenance
boolean visible = false
integer x = 475
integer y = 1728
integer width = 475
integer height = 64
integer taborder = 31
boolean bringtotop = true
string dataobject = "d_efb_payment_document_to_delete"
end type

type dw_payment_document_2 from u_dw_online within w_account_payment_maintenance
boolean visible = false
integer x = 672
integer y = 1552
integer width = 155
integer height = 172
integer taborder = 20
string dataobject = "d_payment_document"
boolean livescroll = false
end type

event constructor;/* Overridden for performance reasons.  No column level security needed. */
end event

type dw_iw_travel_expense from u_dw_online within w_account_payment_maintenance
boolean visible = false
integer x = 530
integer y = 1600
integer width = 114
integer height = 116
integer taborder = 11
boolean bringtotop = true
string dataobject = "d_iw_travel_expense"
end type

type cb_travel from commandbutton within w_account_payment_maintenance
integer x = 1467
integer y = 320
integer width = 293
integer height = 76
integer taborder = 100
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Travel Log"
end type

event clicked;long ll_iw_travel_expense_no, ll_count

s_window_message lstr_message

//passing in the claim_no
lstr_message.al_doubleparm[1] = istr_message.al_doubleparm[2] // claim_no
lstr_message.al_doubleparm[2] = uo_payment.dw_account_payment.getitemnumber(uo_payment.dw_account_payment.getrow(),"payment_no")  // payment_no   <----- find out what the payment no is ?????


IF ISvalid(w_travel_expense) Then w_travel_expense.visible = True 

Select count(*)
Into    :ll_count
From   REHAB_INVOICE_LINE_ITEM a
JOIN   Billable_Item_Rehab_Task_Xref b on a.billable_xref_no = b.billable_xref_no
JOIN   Billable_Item c on b.billable_item_no = c.billable_item_no
Where  b.billable_item_no IN (172,173, 256, 252)
and    a.claim_no = :lstr_message.al_doubleparm[1] 
USING SQLCA;

SQLCA.nf_handle_error('w_account_payment_maintenance','cb_travel.clicked','Select count from REHAB_INVOICE_LINE_ITEM')

IF ll_count > 0 THEN 
	OpenWithParm (w_travel_expense,lstr_message)
ELSE
	MessageBox('Warning','There are no ePhysio treatment travel reimbursement records to display.',Information!)
	Return
END IF



end event

type dw_iw_travel_expense_delete from u_dw_online within w_account_payment_maintenance
boolean visible = false
integer x = 37
integer y = 1724
integer width = 96
integer height = 88
integer taborder = 11
boolean bringtotop = true
string dataobject = "d_iw_travel_expense_delete"
end type

