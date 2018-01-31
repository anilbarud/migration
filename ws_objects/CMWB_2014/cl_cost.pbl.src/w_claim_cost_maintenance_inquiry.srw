$PBExportHeader$w_claim_cost_maintenance_inquiry.srw
forward
global type w_claim_cost_maintenance_inquiry from w_ancestor
end type
type dw_document_path from u_dw_document_path within w_claim_cost_maintenance_inquiry
end type
type cb_close from commandbutton within w_claim_cost_maintenance_inquiry
end type
type cb_viewaccounts from commandbutton within w_claim_cost_maintenance_inquiry
end type
type uo_image_append from u_image_append within w_claim_cost_maintenance_inquiry
end type
type dw_txn_list from u_dw_online within w_claim_cost_maintenance_inquiry
end type
type dw_txn_list_details from u_dw_online within w_claim_cost_maintenance_inquiry
end type
type dw_pay_txn_details from u_dw_online within w_claim_cost_maintenance_inquiry
end type
end forward

global type w_claim_cost_maintenance_inquiry from w_ancestor
integer width = 2775
integer height = 3104
string title = "Claim Cost Maintenance - Payment Inquiry"
string menuname = ""
boolean maxbox = false
boolean resizable = false
windowtype windowtype = popup!
long backcolor = 67108864
boolean toolbarvisible = false
dw_document_path dw_document_path
cb_close cb_close
cb_viewaccounts cb_viewaccounts
uo_image_append uo_image_append
dw_txn_list dw_txn_list
dw_txn_list_details dw_txn_list_details
dw_pay_txn_details dw_pay_txn_details
end type
global w_claim_cost_maintenance_inquiry w_claim_cost_maintenance_inquiry

type variables
boolean lb_toggle

U_DW_DOCUMENT_PATH iu_dw_document_path
M_DW_ONLINE_RMB_POPUP im_popup

Datastore  ids_docs_for_payment_no


long il_txn_no

string   is_window_name

boolean ib_refresh_required = true
end variables

forward prototypes
public subroutine wf_set_parm (integer ai_action)
end prototypes

public subroutine wf_set_parm (integer ai_action);
end subroutine

on w_claim_cost_maintenance_inquiry.create
int iCurrent
call super::create
this.dw_document_path=create dw_document_path
this.cb_close=create cb_close
this.cb_viewaccounts=create cb_viewaccounts
this.uo_image_append=create uo_image_append
this.dw_txn_list=create dw_txn_list
this.dw_txn_list_details=create dw_txn_list_details
this.dw_pay_txn_details=create dw_pay_txn_details
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_document_path
this.Control[iCurrent+2]=this.cb_close
this.Control[iCurrent+3]=this.cb_viewaccounts
this.Control[iCurrent+4]=this.uo_image_append
this.Control[iCurrent+5]=this.dw_txn_list
this.Control[iCurrent+6]=this.dw_txn_list_details
this.Control[iCurrent+7]=this.dw_pay_txn_details
end on

on w_claim_cost_maintenance_inquiry.destroy
call super::destroy
destroy(this.dw_document_path)
destroy(this.cb_close)
destroy(this.cb_viewaccounts)
destroy(this.uo_image_append)
destroy(this.dw_txn_list)
destroy(this.dw_txn_list_details)
destroy(this.dw_pay_txn_details)
end on

event open;call super::open;LONG 		ll_claim_no,ll_rows,ll_rc
long		ll_row,ll_payment_no,ll_txn_no
long		ll_posting_period,ll_find
string 	ls_script
string	ls_find
long		ll_rowcount


is_window_name = this.Classname( )
ls_script = "open"


/*	Create an instance of the user object for the view function
*/


iu_dw_document_path = dw_document_path



//	iu_dw_document_path = dw_document_path
	
/* datastore to load documents for a payment */
ids_docs_for_payment_no = create datastore
ids_docs_for_payment_no.Dataobject = 'd_docs_for_payment_no'

/* set trans object for datawindows's and datastores */
dw_txn_list.SetTransObject(SQLCA)
dw_pay_txn_details.SetTransObject(SQLCA)
dw_txn_list_details.SetTransObject(SQLCA)
ids_docs_for_payment_no.SetTransObject(SQLCA)

/* the txn no is passed from the calling window */
ll_txn_no = Message.DoubleParm	

dw_txn_list.setredraw(false)

/* txn details */
ll_rows = dw_txn_list_details.retrieve(ll_txn_no)
SQLCA.nf_handle_error('Retrieve on dw_txn_list_details()',is_window_name,ls_script)

/* payment related to the txn that is selected */
ll_payment_no = dw_txn_list_details.getitemnumber(1,'payment_no')
/* retrieve the payment information */
dw_pay_txn_details.retrieve(ll_payment_no)
SQLCA.nf_handle_error('Retrieve on dw_pay_txn_details()',is_window_name,ls_script)

/* doc's for a payment */
ll_rowcount = ids_docs_for_payment_no.retrieve(ll_payment_no)
SQLCA.nf_handle_error('Retrieve on ids_docs_for_payment_no()',is_window_name,ls_script)

if ll_rowcount <= 0 then
	cb_viewaccounts.enabled = false
else
	cb_viewaccounts.enabled = true
end if


/* all txn for payment */
dw_txn_list.retrieve(ll_payment_no)
SQLCA.nf_handle_error('Retrieve on dw_txn_list()',is_window_name,ls_script)

/* detemine which row that needs to be selected in the txn list */
ls_find = "txn_no = " + string(ll_txn_no)
ll_find = dw_txn_list.find(ls_find,1, dw_txn_list.rowcount())
dw_txn_list.selectrow(0,false)
/* select the txn in the txn list corresponding to the txn being passed */
if ll_find > 0 then
	dw_txn_list.selectrow(ll_find,true)
	dw_txn_list.setrow(ll_find)
	dw_txn_list.scrolltorow(ll_find)
end if


ll_rowcount = ids_docs_for_payment_no.RowCount()



dw_txn_list.setredraw(true)




end event

type dw_document_path from u_dw_document_path within w_claim_cost_maintenance_inquiry
boolean visible = false
integer x = 695
integer y = 2688
integer taborder = 40
end type

type cb_close from commandbutton within w_claim_cost_maintenance_inquiry
integer x = 2304
integer y = 2664
integer width = 402
integer height = 104
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
boolean default = true
end type

event clicked;close(parent)
end event

type cb_viewaccounts from commandbutton within w_claim_cost_maintenance_inquiry
integer x = 1815
integer y = 2664
integer width = 485
integer height = 104
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&View Accounts"
end type

event clicked;LONG ll_cntr, ll_rowcount, ll_doc_id
string ls_doc_type
integer li_rtn
string ls_script = "itemfocuschanged"


ll_cntr = 1

ll_rowcount = ids_docs_for_payment_no.RowCount()


DO WHILE ll_cntr <= ll_rowcount
		
	ll_doc_id =  ids_docs_for_payment_no.GetItemNumber(ll_cntr,"doc_id")	
		
	if uo_image_append.of_init(ll_doc_id)	<= 0 then
		RETURN
	end if
		
	ls_doc_type =  uo_image_append.of_get_file_type()
		
	
	CHOOSE CASE ls_doc_type
		/*  Imaged document */ 
		CASE 'TIF' , 'IMA'
			li_rtn = uo_image_append.of_append_image(ll_doc_id)
			if li_rtn < 0 then
				RETURN
			end if
		case else	
			iu_dw_document_path.f_manage_document(ll_doc_id,"V","NORMAL")
			
	end choose
			
	ll_cntr ++
LOOP
end event

type uo_image_append from u_image_append within w_claim_cost_maintenance_inquiry
boolean visible = false
integer x = 2231
integer y = 1568
integer width = 110
integer height = 192
end type

on uo_image_append.destroy
call u_image_append::destroy
end on

type dw_txn_list from u_dw_online within w_claim_cost_maintenance_inquiry
event ue_show_transfer_details ( string as_detail_type )
integer y = 768
integer width = 2743
integer height = 512
integer taborder = 30
string dataobject = "d_txn_list"
boolean vscrollbar = true
boolean livescroll = false
end type

event ue_show_transfer_details(string as_detail_type);/*

Any changes made to this script should also be made in 
w_claim_cost_maintenance.dw_transaction_list.ue_show_transfer_details and
w_payment_inquiry.dw_list_payments.ue_show_transfer_details

*/

LONG			ll_selected_txn_no
LONG			ll_selected_payment_no
LONG			ll_payment_no, ll_max_payment_no
LONG			ll_claim_no
LONG			ll_max_applied_payment_no, ll_max_applied_txn_no
LONG			ll_max_unapplied_payment_no , ll_max_unapplied_txn_no
LONG			ll_cost_alloc_no , ll_cost_alloc_operation_no
STRING		ls_message
STRING		ls_txn_amount_desc
STRING		ls_txn_sub_type_code
STRING		ls_payment_type_code
STRING		ls_payment_sub_type_code
DECIMAL{2}	ldec_txn_amount
DECIMAL{2}  ldec_selected_txn_amount
LONG			ll_rows, ll_rows_unapplied
u_ds			lds_max_payment_applied, lds_max_payment_unapplied
u_ds			lds_transfer_details_applied, lds_transfer_details_unapplied

lds_transfer_details_applied	= CREATE u_ds
lds_transfer_details_unapplied	= CREATE u_ds
lds_max_payment_applied		= CREATE u_ds
lds_max_payment_unapplied	= CREATE u_ds

ll_selected_txn_no = GetItemNumber(GetRow(),'txn_no')
ll_selected_payment_no = GetItemNumber(GetRow(),'payment_no')
ldec_selected_txn_amount = GetItemDecimal(GetRow(),'txn_amount')

CHOOSE CASE as_detail_type
	CASE 'TO'
		//The user wants to know where this transaction was transfered "to"
		//Get the maximum payment_no that is related to the selected transaction 
		
		lds_max_payment_applied.DataObject = 'd_xfer_to_applied_max_payment'
		lds_max_payment_applied.SetTransObject(SQLCA)
		ll_rows = lds_max_payment_applied.Retrieve(ll_selected_txn_no)
		IF ll_rows < 0 Then
			SignalError(-666,'Error retrieving applied transfer details.')
		End if
		SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','retrieve')
		
		lds_max_payment_unapplied.DataObject = 'd_xfer_to_unapplied_max_payment'
		lds_max_payment_unapplied.SetTransObject(SQLCA)
		ll_rows_unapplied = lds_max_payment_unapplied.Retrieve(ll_selected_txn_no)
		IF ll_rows_unapplied < 0 Then
			SignalError(-666,'Error retrieving unapplied transfer details.')
		End if
		SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','retrieve')
				
		IF ll_rows = 0 AND ll_rows_unapplied = 0 Then
			MessageBox('Not transferred','This transaction has not been transferred.')
			
		ELSEIF ll_rows <> 0 AND ll_rows_unapplied = 0 THEN // only applied txns transferred
			ll_max_applied_payment_no = lds_max_payment_applied.GetItemNumber(1,'max_transfer_payment_no')
			
			//Gather the information and create the message string to pass into the message window.
			
			// the txn sub type will determine the data passed to the messagebox
			SELECT	txn_sub_type_code , Max(txn_no)
			INTO		:ls_txn_sub_type_code , :ll_max_applied_txn_no
			FROM		APPLIED_CLAIM_TXN
			WHERE	payment_no	= :ll_max_applied_payment_no
			AND		txn_type_code = 'T'
			GROUP BY txn_sub_type_code
			USING	SQLCA;
			SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','select max txn from APPLIED_CLAIM_TXN')
			
			CHOOSE CASE ls_txn_sub_type_code
				CASE '7','8'
					SELECT	b.cost_alloc_no , b.cost_alloc_operation_no , a.txn_amount
					INTO		:ll_cost_alloc_no , :ll_cost_alloc_operation_no, :ldec_txn_amount
					FROM		APPLIED_CLAIM_TXN					a,
								COST_OF_CLAIMS_ALLOCATED	b
					WHERE	a.txn_no = b.txn_no
					AND		a.txn_no	= :ll_max_applied_txn_no
					USING	SQLCA;
					SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','select data for max txn from APPLIED_CLAIM_TXN')
					
					ls_message = ' (' + String(ldec_txn_amount,'$#,##0.00') + ') of the selected transaction was transferred to '&
						+ 'cost alloc ' + String(ll_cost_alloc_no) +'/'+ String(ll_cost_alloc_operation_no) + '.  See payment #' + String(ll_max_applied_payment_no) + '.'
						
				CASE '6'
					SELECT	claim_no , txn_amount
					INTO		:ll_claim_no , :ldec_txn_amount
					FROM		APPLIED_CLAIM_TXN
					WHERE	txn_no	= :ll_max_applied_txn_no
					USING	SQLCA;
					SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','select data for max txn from APPLIED_CLAIM_TXN')
					
					ls_message = ' (' + String(ldec_txn_amount,'$#,##0.00') + ') of the selected transaction was transferred to '&
						+ 'claim #' + String(ll_claim_no) + '.  See payment #' + String(ll_max_applied_payment_no) + '.'
						
				CASE '9'
					SELECT	b.payment_type_code , b.payment_sub_type_code , a.txn_amount
					INTO		:ls_payment_type_code , :ls_payment_sub_type_code , :ldec_txn_amount
					FROM		APPLIED_CLAIM_TXN	a ,
								PAYMENT				b
					WHERE	a.payment_no	= b.payment_no
					AND		a.txn_no			= :ll_max_applied_txn_no
					USING	SQLCA;
					SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','select data for max txn from APPLIED_CLAIM_TXN')
					
					IF ls_payment_sub_type_code <> '' THEN
						ls_message = ' (' + String(ldec_txn_amount,'$#,##0.00') + ') of the selected transaction was transferred to '&
							+ 'payment type/sub type ' + ls_payment_type_code +'/'+ ls_payment_sub_type_code  + '.  See payment #' + String(ll_max_applied_payment_no) + '.'
					ELSE
						ls_message = ' (' + String(ldec_txn_amount,'$#,##0.00') + ') of the selected transaction was transferred to '&
							+ 'payment type ' + ls_payment_type_code + '.  See payment #' + String(ll_max_applied_payment_no) + '.'
					END IF
					
			END CHOOSE
					
			IF ldec_selected_txn_amount = ldec_txn_amount Then
				ls_message = 'The full amount' + ls_message
			Else
				ls_message = 'A portion' + ls_message
			End if
			
			MessageBox('Transfer',ls_message)
			
		ELSEIF ll_rows = 0 AND ll_rows_unapplied <> 0 THEN // only unapplied txns transferred
			ll_max_unapplied_payment_no = lds_max_payment_unapplied.GetItemNumber(1,'max_transfer_payment_no')
			
			//Gather the information and create the message string to pass into the message window.
			
			// the txn sub type will determine the data passed to the messagebox
			SELECT	txn_sub_type_code , Max(txn_no)
			INTO		:ls_txn_sub_type_code , :ll_max_unapplied_txn_no
			FROM		UNAPPLIED_CLAIM_TXN
			WHERE	payment_no	= :ll_max_unapplied_payment_no
			AND		txn_type_code = 'T'
			GROUP BY txn_sub_type_code
			USING	SQLCA;
			SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','select max txn from UNAPPLIED_CLAIM_TXN')
			
			CHOOSE CASE ls_txn_sub_type_code
				CASE '7','8'
					SELECT	b.cost_alloc_no , b.cost_alloc_operation_no , a.txn_amount
					INTO		:ll_cost_alloc_no , :ll_cost_alloc_operation_no, :ldec_txn_amount
					FROM		UNAPPLIED_CLAIM_TXN				a,
								COST_OF_CLAIMS_ALLOCATED	b
					WHERE	a.txn_no = b.txn_no
					AND		a.txn_no	= :ll_max_unapplied_txn_no
					USING	SQLCA;
					SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','select data for max txn from UNAPPLIED_CLAIM_TXN')
					
					ls_message = ' (' + String(ldec_txn_amount,'$#,##0.00') + ') of the selected transaction is scheduled to be transferred to '&
						+ 'cost alloc ' + String(ll_cost_alloc_no) +'/'+ String(ll_cost_alloc_operation_no) + '.  See payment #' + String(ll_max_unapplied_payment_no) + '.'
						
				CASE '6'
					SELECT	claim_no , txn_amount
					INTO		:ll_claim_no , :ldec_txn_amount
					FROM		UNAPPLIED_CLAIM_TXN
					WHERE	txn_no	= :ll_max_unapplied_txn_no
					USING	SQLCA;
					SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','select data for max txn from UNAPPLIED_CLAIM_TXN')
					
					ls_message = ' (' + String(ldec_txn_amount,'$#,##0.00') + ') of the selected transaction is scheduled to be transferred to '&
						+ 'claim #' + String(ll_claim_no) + '.  See payment #' + String(ll_max_unapplied_payment_no) + '.'
						
				CASE '9'
					SELECT	b.payment_type_code , b.payment_sub_type_code , a.txn_amount
					INTO		:ls_payment_type_code , :ls_payment_sub_type_code , :ldec_txn_amount
					FROM		UNAPPLIED_CLAIM_TXN	a ,
								PAYMENT					b
					WHERE	a.payment_no	= b.payment_no
					AND		a.txn_no			= :ll_max_unapplied_txn_no
					USING	SQLCA;
					SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','select data for max txn from UNAPPLIED_CLAIM_TXN')
					
					IF ls_payment_sub_type_code <> '' THEN
						ls_message = ' (' + String(ldec_txn_amount,'$#,##0.00') + ') of the selected transaction is scheduled to be transferred to '&
							+ 'payment type/sub type ' + ls_payment_type_code +'/'+ ls_payment_sub_type_code  + '.  See payment #' + String(ll_max_unapplied_payment_no) + '.'
					ELSE
						ls_message = ' (' + String(ldec_txn_amount,'$#,##0.00') + ') of the selected transaction was transferred to '&
							+ 'payment type ' + ls_payment_type_code + '.  See payment #' + String(ll_max_unapplied_payment_no) + '.'
					END IF
					
			END CHOOSE
			IF ldec_selected_txn_amount = ldec_txn_amount Then
				ls_message = 'The full amount' + ls_message
			Else
				ls_message = 'A portion' + ls_message
			End if
			
			MessageBox('Transfer', ls_message)
			
		End if
		
	CASE 'FROM'
		//The user wants to know where this transactions was transfered "from"
		lds_transfer_details_applied.DataObject = 'd_transfer_from'
		lds_transfer_details_applied.SetTransObject(SQLCA)
		ll_rows = lds_transfer_details_applied.Retrieve(ll_selected_txn_no)
		IF ll_rows < 0 Then
			SignalError(-666,'Error retrieving transfer details.')
		End if
		SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','retrieve')
		
		lds_transfer_details_unapplied.DataObject = 'd_transfer_from_unapplied'
		lds_transfer_details_unapplied.SetTransObject(SQLCA)
		ll_rows_unapplied = lds_transfer_details_unapplied.Retrieve(ll_selected_txn_no)
		IF ll_rows_unapplied < 0 Then
			SignalError(-666,'Error retrieving unapplied transfer details.')
		End if
		SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','retrieve')
		
		IF ll_rows = 0 and ll_rows_unapplied = 0 Then
			MessageBox('Not transferred','This transaction has not been transferred.')
			
		ELSEIF ll_rows <> 0 AND ll_rows_unapplied = 0 THEN // only applied txns transferred
			//Gather the information and create the message string to pass into 
			//the message window.
			ll_payment_no  			= lds_transfer_details_applied.GetItemNumber(1,'payment_no')
			ll_max_applied_txn_no	= lds_transfer_details_applied.GetItemNumber(1,'max_transfer_txn_no')
			ls_txn_sub_type_code	= lds_transfer_details_applied.GetItemString(1,'txn_sub_type_code')

			CHOOSE CASE ls_txn_sub_type_code
				CASE '7','8'
					ll_cost_alloc_no = lds_transfer_details_applied.GetItemNumber(1,'cost_alloc_no')
					ll_cost_alloc_operation_no = lds_transfer_details_applied.GetItemNumber(1,'cost_alloc_operation_no')
					ls_message =  'The selected transaction was transferred from ' &
						+ 'cost alloc ' + String(ll_cost_alloc_no) +'/'+ String(ll_cost_alloc_operation_no) + '.  See payment #' + String(ll_payment_no) + '.'
						
				CASE '6'
					ll_claim_no = lds_transfer_details_applied.GetItemNumber(1,'claim_no')
					ls_message = 'The selected transaction was transferred from '&
						+ 'claim #' + String(ll_claim_no) + '.  See payment #' + String(ll_payment_no) + '.'
					
				CASE '9'
					ls_payment_type_code = lds_transfer_details_applied.GetItemString(1,'payment_type_code')
					ls_payment_sub_type_code = lds_transfer_details_applied.GetItemString(1,'payment_sub_type_code')
					IF ls_payment_sub_type_code <> '' THEN
						ls_message = 'The selected transaction was transferred from '&
							+ 'payment type/sub type ' + ls_payment_type_code +'/'+ ls_payment_sub_type_code  + '.  See payment #' + String(ll_payment_no) + '.'
					ELSE
						ls_message = 'The selected transaction was transferred from '&
						+ 'payment type ' + ls_payment_type_code + '.  See payment #' + String(ll_payment_no) + '.'
					END IF
					
			END CHOOSE
					
			IF ll_max_applied_txn_no = ll_selected_txn_no THEN
				MessageBox('Transfer',ls_message)
			ELSE
				MessageBox('Not transferred','This transaction has not been transferred.')
			END IF
			
		ELSEIF ll_rows = 0 AND ll_rows_unapplied <> 0 THEN // only unapplied txns transferred
			
			//Gather the information and create the message string to pass into 
			//the message window.
			ll_payment_no  				= lds_transfer_details_unapplied.GetItemNumber(1,'payment_no')
			ll_max_unapplied_txn_no	= lds_transfer_details_unapplied.GetItemNumber(1,'max_transfer_txn_no')
			ls_txn_sub_type_code		= lds_transfer_details_unapplied.GetItemString(1,'txn_sub_type_code')
			
			CHOOSE CASE ls_txn_sub_type_code
				CASE '7','8'
					ll_cost_alloc_no = lds_transfer_details_unapplied.GetItemNumber(1,'cost_alloc_no')
					ll_cost_alloc_operation_no = lds_transfer_details_unapplied.GetItemNumber(1,'cost_alloc_operation_no')
					ls_message =  'The selected transaction is scheduled to be transferred from ' &
						+ 'cost alloc ' + String(ll_cost_alloc_no) +'/'+ String(ll_cost_alloc_operation_no) + '.  See payment #' + String(ll_payment_no) + '.'
						
				CASE '6'
					ll_claim_no = lds_transfer_details_unapplied.GetItemNumber(1,'claim_no')
					ls_message = 'The selected transaction was transferred from '&
						+ 'claim #' + String(ll_claim_no) + '.  See payment #' + String(ll_payment_no) + '.'
					
				CASE '9'
					ls_payment_type_code = lds_transfer_details_unapplied.GetItemString(1,'payment_type_code')
					ls_payment_sub_type_code = lds_transfer_details_unapplied.GetItemString(1,'payment_sub_type_code')
					IF ls_payment_sub_type_code <> '' THEN
						ls_message = 'The selected transaction is scheduled to be transferred from '&
							+ 'payment type/sub type ' + ls_payment_type_code +'/'+ ls_payment_sub_type_code  + '.  See payment #' + String(ll_payment_no) + '.'
					ELSE
						ls_message = 'The selected transaction is scheduled to be transferred from '&
						+ 'payment type ' + ls_payment_type_code + '.  See payment #' + String(ll_payment_no) + '.'
					END IF
					
			END CHOOSE
			
			IF ll_max_unapplied_txn_no	= ll_selected_txn_no THEN
				MessageBox('Transfer',ls_message)
			ELSE
				MessageBox('Not transferred','This transaction has not been transferred.')
			END IF
			
		End if
END CHOOSE
end event

event rowfocuschanged;call super::rowfocuschanged;LONG ll_sel_row, ll_new_row, ll_rowcount, ll_cntr,ll_multi
BOOLEAN lb_selected
long ll_txn_no
string ls_script = "rowfocuschaged - dw_txn_list"



ll_rowcount = this.RowCount()

FOR ll_cntr = 1 to ll_rowcount
	lb_selected = IsSelected(ll_cntr)
	IF lb_selected = TRUE THEN
		ll_multi = ll_multi + 1
	END IF
NEXT

IF ll_multi = 1 THEN
	ll_sel_row = this.GetSelectedRow(0)

	IF ll_sel_row > 0 THEN
		ll_new_row = currentrow
		THIS.SelectRow(ll_sel_row,FALSE)
		IF ll_new_row > 0 THEN
			THIS.SelectRow(ll_new_row, TRUE)
			ll_txn_no = this.getitemnumber(ll_new_row,'txn_no')
			
		
			/* txn details */
			dw_txn_list_details.retrieve(ll_txn_no)
			SQLCA.nf_handle_error('Retrieve on dw_txn_list_details()',is_window_name,ls_script)


		END IF
	END IF
END IF
end event

event rbuttondown;If not isvalid(im_popup) Then

	im_popup = Create m_dw_online_rmb_popup
	im_popup.mf_set_datawindow(This)
//	im_popup.m_options.m_filterlist.visible = TRUE	
//	im_popup.m_options.m_sort.visible = TRUE
//	im_popup.m_options.m_manualtxn.visible = TRUE
	im_popup.m_options.m_transferdetails.visible = TRUE
//	im_popup.m_options.m_tooltips.visible = TRUE	
End if

im_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))


end event

type dw_txn_list_details from u_dw_online within w_claim_cost_maintenance_inquiry
integer y = 1312
integer width = 2743
integer height = 1312
integer taborder = 20
string dataobject = "d_txn_list_details"
end type

type dw_pay_txn_details from u_dw_online within w_claim_cost_maintenance_inquiry
integer y = 32
integer width = 2743
integer height = 672
integer taborder = 0
string dataobject = "d_pay_txn_details"
boolean livescroll = false
borderstyle borderstyle = styleraised!
end type

