$PBExportHeader$w_account_link_maintenance.srw
$PBExportComments$module for linking up payments to documents
forward
global type w_account_link_maintenance from w_ancestor
end type
type dw_payment_document from u_dw_online within w_account_link_maintenance
end type
type rb_unlinked_only from radiobutton within w_account_link_maintenance
end type
type rb_all_payments from radiobutton within w_account_link_maintenance
end type
type gb_list_options from groupbox within w_account_link_maintenance
end type
type st_1 from statictext within w_account_link_maintenance
end type
type cb_ok from commandbutton within w_account_link_maintenance
end type
type cb_cancel from commandbutton within w_account_link_maintenance
end type
type pb_document from picturebutton within w_account_link_maintenance
end type
type dw_account_type_payments_list from u_dw_online within w_account_link_maintenance
end type
end forward

global type w_account_link_maintenance from w_ancestor
integer x = 73
integer y = 120
integer width = 2592
integer height = 1520
string title = "Link Imaged Document to Advance"
string menuname = ""
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = child!
long backcolor = 67108864
dw_payment_document dw_payment_document
rb_unlinked_only rb_unlinked_only
rb_all_payments rb_all_payments
gb_list_options gb_list_options
st_1 st_1
cb_ok cb_ok
cb_cancel cb_cancel
pb_document pb_document
dw_account_type_payments_list dw_account_type_payments_list
end type
global w_account_link_maintenance w_account_link_maintenance

type variables
S_WINDOW_MESSAGE		istr_message
Long				il_claim_no
n_payment			inv_payment
n_account_payment_controller inv_controller
w_account_payment		iwi_parent
boolean				ib_already_unpaid
end variables

forward prototypes
public subroutine wf_set_parent_window (window awi_parent)
public subroutine wf_set_payment_object (n_payment anv_payment)
end prototypes

public subroutine wf_set_parent_window (window awi_parent);iwi_parent = awi_parent
end subroutine

public subroutine wf_set_payment_object (n_payment anv_payment);inv_payment = anv_payment
end subroutine

event open;call super::open;LONG	ll_find_rownum

	istr_message = message.powerobjectparm

	dw_account_type_payments_list.SetTransObject(SQLCA)
	dw_payment_document.SetTransObject(SQLCA)
	

	rb_unlinked_only.TriggerEvent(clicked!)

	dw_account_type_payments_list.Retrieve(istr_message.al_doubleparm[2])
	IF SQLCA.nf_handle_error("dw_account_type_payments","w_account_link_maintenance","wf_init") < 0 THEN
		Close(this)
		Return
	END IF

 	this.Title = 		"Link payments to doc # " + string(istr_message.al_doubleparm[3]) + &
							"  (" + istr_message.as_stringparm[1] + " " + string(istr_message.adtm_datetimeparm[1],"yyyy-mm-dd") + ")"
	
	dw_account_type_payments_list.uf_setselect(1)

/*	Check to see if there is already an unpaid payment for this account.  We do this
	here so we can give the user a better idea of why they can't link another unpaid
	payment to this document (ie. if we wait and only check on save we don't know if
	there was an unpaid payment when the user started or if another user had made an
	unpaid payment
*/
	SetNull(ll_find_rownum)

	SELECT	count(*)
	INTO		:ll_find_rownum
	FROM		PAYMENT_DOCUMENT
	WHERE		PAYMENT_DOCUMENT.paid_status_code = "S" and PAYMENT_DOCUMENT.doc_id = :istr_message.al_doubleparm[3]
	USING		SQLCA;

	IF SQLCA.nf_handle_error("Embedded SQL: Select from PAYMENT_DOCUMENT","w_account_payment_maintenance","open") < 0 THEN
		MessageBox("Account Link Maintenance","No updates have been made",Exclamation!)
		Close(this)
		Return
	END IF

	IF ll_find_rownum > 0 THEN
		ib_already_unpaid = TRUE
	ELSE
		ib_already_unpaid = FALSE
	END IF

inv_controller = Create n_account_payment_controller

	Return
end event

on w_account_link_maintenance.create
int iCurrent
call super::create
this.dw_payment_document=create dw_payment_document
this.rb_unlinked_only=create rb_unlinked_only
this.rb_all_payments=create rb_all_payments
this.gb_list_options=create gb_list_options
this.st_1=create st_1
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.pb_document=create pb_document
this.dw_account_type_payments_list=create dw_account_type_payments_list
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_payment_document
this.Control[iCurrent+2]=this.rb_unlinked_only
this.Control[iCurrent+3]=this.rb_all_payments
this.Control[iCurrent+4]=this.gb_list_options
this.Control[iCurrent+5]=this.st_1
this.Control[iCurrent+6]=this.cb_ok
this.Control[iCurrent+7]=this.cb_cancel
this.Control[iCurrent+8]=this.pb_document
this.Control[iCurrent+9]=this.dw_account_type_payments_list
end on

on w_account_link_maintenance.destroy
call super::destroy
destroy(this.dw_payment_document)
destroy(this.rb_unlinked_only)
destroy(this.rb_all_payments)
destroy(this.gb_list_options)
destroy(this.st_1)
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.pb_document)
destroy(this.dw_account_type_payments_list)
end on

type dw_payment_document from u_dw_online within w_account_link_maintenance
integer x = 2720
integer y = 68
integer width = 1143
integer height = 212
integer taborder = 30
string dataobject = "d_payment_document"
end type

type rb_unlinked_only from radiobutton within w_account_link_maintenance
integer x = 197
integer y = 120
integer width = 695
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Unlinked payments only"
boolean checked = true
end type

event clicked;dw_account_type_payments_list.SetFilter('compute_1 = ""')
dw_account_type_payments_list.Filter()

end event

type rb_all_payments from radiobutton within w_account_link_maintenance
integer x = 197
integer y = 196
integer width = 640
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "All account payments"
end type

event clicked;dw_account_type_payments_list.SetFilter("")
dw_account_type_payments_list.Filter()



end event

type gb_list_options from groupbox within w_account_link_maintenance
integer x = 78
integer y = 36
integer width = 1120
integer height = 288
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "List Options"
end type

type st_1 from statictext within w_account_link_maintenance
integer x = 1221
integer y = 1264
integer width = 846
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Drag payment to  link to account"
alignment alignment = right!
boolean focusrectangle = false
end type

type cb_ok from commandbutton within w_account_link_maintenance
integer x = 2258
integer y = 92
integer width = 274
integer height = 108
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&OK"
end type

event clicked;INTEGER	li_rtn
LONG		ll_rownum, ll_reccount
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

//	If the user is trying to link an unpaid payment, we have to make sure no other 
//	user created an entry for this document since we first came in here

ll_rownum = dw_payment_document.Find("paid_status_code = 'S'",1,dw_payment_document.RowCount())

IF ll_rownum > 0 THEN

	SELECT	count(*)
	INTO		:ll_reccount
	FROM		PAYMENT_DOCUMENT
	WHERE		(doc_id = :istr_message.al_doubleparm[3]) and paid_status_code <> "P" using SQLCA;

	IF SQLCA.nf_handle_error("w_account_link_maintenance", "", "cb_ok - SELECT * FROM PAYMENT_DOCUMENT") < 0 THEN Goto Error_Handling
	IF ll_reccount > 0 THEN
		MessageBox("Account Payment Maintenance","Another user has made a payment on this account since you first started",Exclamation!)
		Goto Error_Handling
	END IF
END IF

SQLCA.nf_begin_transaction()

dw_payment_document.Update()
IF SQLCA.nf_handle_error("w_account_link_maintenance", "", "cb_ok - dw_payment_document.Update()") < 0 THEN Goto Error_Handling

SQLCA.nf_commit_transaction()

iwi_parent.cb_refresh.PostEvent("ue_update_complete")

Close(PARENT)
Return

Error_Handling:
	MessageBox("Account Link Maintenance","No updates have been made",Exclamation!)
	iwi_parent.cb_refresh.PostEvent("ue_update_cancelled")
	Close(PARENT)
	Return
end event

type cb_cancel from commandbutton within w_account_link_maintenance
integer x = 2258
integer y = 204
integer width = 274
integer height = 108
integer taborder = 50
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
end type

on clicked;//	Since we cancelled, just call the function to reset the buttons on the 
//	calling screen

iwi_parent.cb_refresh.PostEvent("ue_update_cancelled")


close(parent)

end on

type pb_document from picturebutton within w_account_link_maintenance
integer x = 2171
integer y = 1232
integer width = 389
integer height = 176
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "LINKACCT.BMP"
alignment htextalign = right!
end type

event dragdrop;Long    ll_rownum, ll_find_rownum, ll_payment_no
Integer li_rtn
String  ls_payment_type_code, ls_payment_sub_type_code, ls_recipient_type_code

ll_rownum = dw_account_type_payments_list.GetRow()
IF ll_rownum > 0 THEN
	ll_payment_no = dw_account_type_payments_list.GetItemNumber(ll_rownum,"payment_no")

	//	Check the datawindow to ensure we don't link twice
	ll_find_rownum = dw_payment_document.Find("payment_no = " + String(ll_payment_no) + " and doc_id = " + String(istr_message.al_doubleparm[3]),1,dw_account_type_payments_list.RowCount())
	IF ll_find_rownum > 0 THEN
		MessageBox("Account Link Maintenance", "The payment you selected is already linked to the current document",Exclamation!)
		dw_account_type_payments_list.Drag(Cancel!)
		RETURN
	END IF

	// Now, check the database to ensure that we don't link twice
	SetNull(ll_find_rownum)

	SELECT COUNT(*)
	  INTO :ll_find_rownum
	  FROM PAYMENT_DOCUMENT
	 WHERE payment_no = :ll_payment_no 
	   AND doc_id = :istr_message.al_doubleparm[3] 
	 USING SQLCA ;

	li_rtn = SQLCA.nf_handle_error("w_account_link_maintenance", "", "pb_document - SELECT COUNT(*) FROM PAYMENT_DOCUMENT")

	IF ll_find_rownum > 0 THEN
		MessageBox("Account Link Maintenance", "The payment you selected is already linked to the current document",Exclamation!)
		dw_account_type_payments_list.Drag(Cancel!)
		RETURN
	END IF

	// Don't allow a payment_type = "Drugs" and payment_sub_type = "ABCC Prescript or Adj." or "ABCC Claim reimbursement"
	ls_payment_type_code = dw_account_type_payments_list.GetItemString(ll_rownum, "payment_payment_type_code")
	
	SELECT payment_sub_type_code 
	  INTO :ls_payment_sub_type_code 
	  FROM PAYMENT 
	 WHERE payment_no = :ll_payment_no ;

	li_rtn = SQLCA.nf_handle_error("w_account_link_maintenance", "", "pb_document - SELECT payment_sub_type_code FROM PAYMENT")

	IF ls_payment_type_code = "22" AND (ls_payment_sub_type_code = "BC" OR ls_payment_sub_type_code = "RC") THEN
		MessageBox("Account Link Maintenance", "The payment you selected can't be linked to the current document.  " +&
					  "Payments with type = 'Drugs' sub_type = 'ABCC Prescript or Adj.' or 'ABCC Claim " +&
					  "reimbursement' can't be linked to documents.", Exclamation!)
		dw_account_type_payments_list.Drag(Cancel!)
		RETURN
	END IF
	
	// doubleparm[2] = claim_no; doubleparm[3] = doc_id
	
	IF istr_message.as_stringparm[2] = 'FCA' THEN
		ls_recipient_type_code = dw_account_type_payments_list.GetItemString(ll_rownum, 'recipient_type_code')
		li_rtn = inv_controller.nf_check_fca_payment(istr_message.al_doubleparm[2],istr_message.al_doubleparm[3],ls_payment_type_code, ls_payment_sub_type_code, ls_recipient_type_code)
		IF li_rtn < 0 THEN
			dw_account_type_payments_list.Drag(Cancel!)
			RETURN
		END IF
	END IF
	
   ll_find_rownum = dw_payment_document.InsertRow(0)
	dw_payment_document.SetItem(ll_find_rownum, "doc_id", istr_message.al_doubleparm[3])
	dw_payment_document.SetItem(ll_find_rownum, "payment_no", dw_account_type_payments_list.GetItemNumber(ll_rownum,"payment_no"))
   dw_account_type_payments_list.SetItem(ll_rownum, "related_document", 1)

	IF IsNull(dw_account_type_payments_list.GetItemDateTime(ll_rownum, 'processed_date')) THEN
		dw_payment_document.SetItem(ll_find_rownum, "paid_status_code", "S")
	ELSE
		dw_payment_document.SetItem(ll_find_rownum, "paid_status_code", "P")
	END IF
	dw_payment_document.SetItem(ll_find_rownum, "paid_status_explanation_code", " ")
	
	cb_ok.Enabled = TRUE
	

END IF

dw_account_type_payments_list.Drag(Cancel!)

end event

type dw_account_type_payments_list from u_dw_online within w_account_link_maintenance
integer y = 352
integer width = 2569
integer height = 864
integer taborder = 10
string dragicon = "DROPITEM.ICO"
boolean bringtotop = true
string dataobject = "d_account_type_payments_list"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

on clicked;call u_dw_online::clicked;This.Drag(Begin!)
end on

event rowfocuschanged;call super::rowfocuschanged;

uf_processselect(this.GetRow(),"Mouse")
end event

on dragdrop;call u_dw_online::dragdrop;This.Drag(Cancel!)
end on

