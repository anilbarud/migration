$PBExportHeader$w_breakpay.srw
forward
global type w_breakpay from w_ancestor
end type
type dw_breakpay_doc_list from u_dw_online within w_breakpay
end type
type dw_breakpay_pay_doc_list from u_dw_online within w_breakpay
end type
type dw_breakpay_pay_list from u_dw_online within w_breakpay
end type
type cb_remlink from commandbutton within w_breakpay
end type
type cb_close from commandbutton within w_breakpay
end type
type gb_2 from groupbox within w_breakpay
end type
end forward

global type w_breakpay from w_ancestor
integer x = 384
integer y = 92
integer height = 2820
string title = "Remove Payment/Document Link"
string menuname = ""
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
long backcolor = 67108864
dw_breakpay_doc_list dw_breakpay_doc_list
dw_breakpay_pay_doc_list dw_breakpay_pay_doc_list
dw_breakpay_pay_list dw_breakpay_pay_list
cb_remlink cb_remlink
cb_close cb_close
gb_2 gb_2
end type
global w_breakpay w_breakpay

type variables
S_WINDOW_MESSAGE           istr_window_message
end variables

on open;call w_ancestor::open;LONG	ll_docid, ll_payno

/*	Initialize needed datawindows.
*/
	dw_breakpay_doc_list.SetTransObject(ImageTrans)
	dw_breakpay_pay_doc_list.SetTransObject(SQLCA)
	dw_breakpay_pay_list.SetTransObject(SQLCA)

/*	Get the document number from the calling object.
*/
	istr_window_message = Message.PowerObjectParm
	ll_docid = istr_window_message.al_doubleparm[1]

/*	Retrieve records for dw_breakpay_doc_list & dw_breakpay_pay_doc_list
	and continue if any.
*/
	IF dw_breakpay_doc_list.Retrieve(ll_docid) <= 0 THEN
		ImageTrans.nf_handle_error("dw_breakpay_doc_list","w_breakpay","open")
		Close(THIS)
		RETURN
	END IF

	IF dw_breakpay_pay_doc_list.Retrieve(ll_docid) <= 0 THEN
		SQLCA.nf_handle_error("dw_breakpay_pay_doc_list","w_breakpay","open")
		Close(THIS)
		RETURN
	END IF

	ll_payno = dw_breakpay_pay_doc_list.GetItemNumber(1,"payment_no")

	IF dw_breakpay_pay_list.Retrieve(ll_payno) = -1 THEN
		SQLCA.nf_handle_error("dw_breakpay_pay_list","w_breakpay","open")
		Close(THIS)
		RETURN
	END IF

	dw_breakpay_pay_doc_list.uf_setselect(1)
	dw_breakpay_pay_doc_list.SetFocus()
	dw_breakpay_pay_doc_list.SetRow(1)

end on

on w_breakpay.create
int iCurrent
call super::create
this.dw_breakpay_doc_list=create dw_breakpay_doc_list
this.dw_breakpay_pay_doc_list=create dw_breakpay_pay_doc_list
this.dw_breakpay_pay_list=create dw_breakpay_pay_list
this.cb_remlink=create cb_remlink
this.cb_close=create cb_close
this.gb_2=create gb_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_breakpay_doc_list
this.Control[iCurrent+2]=this.dw_breakpay_pay_doc_list
this.Control[iCurrent+3]=this.dw_breakpay_pay_list
this.Control[iCurrent+4]=this.cb_remlink
this.Control[iCurrent+5]=this.cb_close
this.Control[iCurrent+6]=this.gb_2
end on

on w_breakpay.destroy
call super::destroy
destroy(this.dw_breakpay_doc_list)
destroy(this.dw_breakpay_pay_doc_list)
destroy(this.dw_breakpay_pay_list)
destroy(this.cb_remlink)
destroy(this.cb_close)
destroy(this.gb_2)
end on

type dw_breakpay_doc_list from u_dw_online within w_breakpay
integer x = 50
integer y = 36
integer width = 2651
integer height = 568
integer taborder = 50
string dataobject = "d_breakpay_doc_list"
boolean border = false
end type

type dw_breakpay_pay_doc_list from u_dw_online within w_breakpay
integer x = 105
integer y = 764
integer width = 2569
integer height = 424
integer taborder = 30
string dataobject = "d_breakpay_pay_doc_list"
end type

event clicked;call super::clicked;// This event script marks a clicked row as selected 
Long ll_payno

IF row <= 0 THEN
	RETURN
END IF

SetRow(row)

ll_payno = dw_breakpay_pay_doc_list.GetItemNumber(row, "payment_no")
IF dw_breakpay_pay_list.Retrieve(ll_payno) = -1 THEN 
	SQLCA.nf_handle_error("dw_breakpay_pay_doc_list", "w_breakpay", "clicked for dw_breakpay_pay_doc_list")
	RETURN
END IF

end event

type dw_breakpay_pay_list from u_dw_online within w_breakpay
integer x = 50
integer y = 1496
integer width = 2661
integer height = 1012
integer taborder = 10
string dataobject = "d_breakpay_pay_list"
boolean border = false
end type

type cb_remlink from commandbutton within w_breakpay
integer x = 105
integer y = 1232
integer width = 384
integer height = 108
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Remove Link"
end type

event clicked;INTEGER	li_rtn
LONG		ll_selected_row, ll_payno, ll_row
STRING	ls_paid_status_code,	ls_zeroed
N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '019' refers to the Document Indexing module, '044' refers to the Payment Processing module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('019','044','removal of the link',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF
/******************************************************************************************/


/*	Ask user if they really want to delete the link.
*/
	IF MessageBox("Linkage Removal","Do you wish to delete the current payment / document link?",Exclamation!,YesNo!) = 2 THEN
		RETURN
	END IF

/* Determine the selected row on dw_breakpay_pay_doc_list.
*/
	ll_selected_row = dw_breakpay_pay_doc_list.GetSelectedRow(0)
	IF ll_selected_row = 0 THEN
		MessageBox("Linkage Removal","You must select one record so as to specify the link to be removed")
		RETURN
	END IF

/* Check to see if any 'scheduled' payments exist. If so, not allowed to remove link.
*/
	ls_paid_status_code = dw_breakpay_pay_doc_list.GetItemString(ll_selected_row,"paid_status_code")
	IF ls_paid_status_code = 'S' THEN
		MessageBox("Linkage Removal","Linkage removal is not allowed on 'scheduled' payments. Please delete this payment from the account payment screen first.")
		RETURN
	END IF

/* Determine whether a payment row exists.
*/
	ll_row = dw_breakpay_pay_list.GetRow()

/* Check to see if payment is 'zeroed' (fully cancelled and adjusted).
*/
	IF ll_row > 0 THEN
		ls_zeroed = dw_breakpay_pay_list.GetItemString(ll_row,"zeroed_flag")
		IF ls_zeroed <> 'Y' OR IsNull(ls_zeroed) THEN
			IF MessageBox("Linkage Removal", "This payment has not been fully 'zeroed' (cancelled and adjusted). Do you wish to delete the current payment / document link?",Exclamation!,YesNo!) = 2 THEN
				RETURN
			END IF
		END IF
	END IF

/* Allowed to remove link so remove the row from dw_breakpay_pay_doc_list
	and update table.
*/
	dw_breakpay_pay_doc_list.DeleteRow(GetRow(dw_breakpay_pay_doc_list))
	
	SQLCA.nf_begin_transaction()
	
	dw_breakpay_pay_doc_list.Update()
	IF SQLCA.nf_handle_error("dw_breakpay_pay_doc_list","w_breakpay","clicked for cb_remlink") < 0 THEN
		MessageBox("Error Saving Changes","Error saving changes to database. Please exit and try again.")
		Close(PARENT)
		RETURN
	END IF

	SQLCA.nf_commit_transaction()
	

/* Record successfully removed, now fill datawindows with the next record.
*/
	IF dw_breakpay_pay_list.RowCount() > 0 THEN
		dw_breakpay_pay_list.DeleteRow(GetRow(dw_breakpay_pay_list))
	END IF

	IF dw_breakpay_pay_doc_list.RowCount() > 0 THEN
		dw_breakpay_pay_doc_list.uf_setselect(1)
		dw_breakpay_pay_doc_list.SetFocus()
		dw_breakpay_pay_doc_list.SetRow(1)

		ll_payno = dw_breakpay_pay_doc_list.GetItemNumber(1,"payment_no")

		IF dw_breakpay_pay_list.Retrieve(ll_payno) = -1 THEN
			SQLCA.nf_handle_error("dw_breakpay_pay_list","w_breakpay","clicked for cb_remlink")
			Close(PARENT)
			RETURN
		END IF
	ELSE
		cb_remlink.Enabled = FALSE
	END IF

end event

type cb_close from commandbutton within w_breakpay
integer x = 2327
integer y = 2592
integer width = 375
integer height = 108
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

on clicked;Close(PARENT)
end on

type gb_2 from groupbox within w_breakpay
integer x = 50
integer y = 672
integer width = 2656
integer height = 708
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Payment / Document Data"
borderstyle borderstyle = styleraised!
end type

