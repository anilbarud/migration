$PBExportHeader$w_filter_transaction_list.srw
$PBExportComments$Payment Inquiry (Select Filter Option) -Popup window for selecting filter options on payment inquiry screen
forward
global type w_filter_transaction_list from w_ancestor
end type
type dw_payment_type from u_dw_online within w_filter_transaction_list
end type
type dw_filter_recipients from u_dw_online within w_filter_transaction_list
end type
type dw_screen_type from u_dw_online within w_filter_transaction_list
end type
type dw_filter_dates from u_dw_online within w_filter_transaction_list
end type
type rb_all_payments from radiobutton within w_filter_transaction_list
end type
type rb_paid from radiobutton within w_filter_transaction_list
end type
type rb_unpaid from radiobutton within w_filter_transaction_list
end type
type gb_select_cheque_no from groupbox within w_filter_transaction_list
end type
type gb_select_payments from groupbox within w_filter_transaction_list
end type
type cb_ok from commandbutton within w_filter_transaction_list
end type
type cb_cancel from commandbutton within w_filter_transaction_list
end type
type st_1 from statictext within w_filter_transaction_list
end type
type sle_cheque_no from singlelineedit within w_filter_transaction_list
end type
end forward

global type w_filter_transaction_list from w_ancestor
integer width = 1714
integer height = 1988
string title = "Filter Transaction List"
string menuname = ""
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
long backcolor = 67108864
dw_payment_type dw_payment_type
dw_filter_recipients dw_filter_recipients
dw_screen_type dw_screen_type
dw_filter_dates dw_filter_dates
rb_all_payments rb_all_payments
rb_paid rb_paid
rb_unpaid rb_unpaid
gb_select_cheque_no gb_select_cheque_no
gb_select_payments gb_select_payments
cb_ok cb_ok
cb_cancel cb_cancel
st_1 st_1
sle_cheque_no sle_cheque_no
end type
global w_filter_transaction_list w_filter_transaction_list

event open;call super::open;DATAWINDOWCHILD	ldwc_child
DATAWINDOWCHILD	ldwc_recipient_child
LONG					ll_row

dw_filter_recipients.SetTransObject(Sqlca)

	dw_filter_recipients.InsertRow(0)
	dw_filter_dates.InsertRow(0)
	dw_screen_type.InsertRow(0)
	dw_payment_type.InsertRow(0)

	dw_filter_recipients.GetChild("recipient_type_code",ldwc_recipient_child)
	ll_row = ldwc_recipient_child.InsertRow(1)
	ldwc_recipient_child.SetItem(ll_row,"recipient_type_code",'')
	ldwc_recipient_child.SetItem(ll_row,"recipient_type_desc",'All recipients')
	
	dw_filter_recipients.SetItem(1,"recipient_type_code","")
	
	dw_filter_recipients.SetTabOrder("recipient_no",0)

	dw_screen_type.GetChild("screen_type",ldwc_child)
	ldwc_child.SetTransObject(SQLCA)
	ldwc_child.Retrieve()
	IF SQLCA.nf_handle_error("ldwc_child","w_filter_transaction_list","open for w_filter_transaction_list") < 0 THEN
		cb_cancel.TriggerEvent(Clicked!)
		Return
	END IF
	
	dw_screen_type.SetItem(1,"screen_type","all")

	dw_payment_type.GetChild("c_type_and_sub_type",ldwc_child)
	ldwc_child.SetTransObject(SQLCA)
	ldwc_child.Retrieve()
	IF SQLCA.nf_handle_error("ldwc_child","w_filter_transaction_list","open for w_filter_transaction_list") < 0 THEN
		cb_cancel.TriggerEvent(Clicked!)
		Return
	END IF

end event

on w_filter_transaction_list.create
int iCurrent
call super::create
this.dw_payment_type=create dw_payment_type
this.dw_filter_recipients=create dw_filter_recipients
this.dw_screen_type=create dw_screen_type
this.dw_filter_dates=create dw_filter_dates
this.rb_all_payments=create rb_all_payments
this.rb_paid=create rb_paid
this.rb_unpaid=create rb_unpaid
this.gb_select_cheque_no=create gb_select_cheque_no
this.gb_select_payments=create gb_select_payments
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.st_1=create st_1
this.sle_cheque_no=create sle_cheque_no
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_payment_type
this.Control[iCurrent+2]=this.dw_filter_recipients
this.Control[iCurrent+3]=this.dw_screen_type
this.Control[iCurrent+4]=this.dw_filter_dates
this.Control[iCurrent+5]=this.rb_all_payments
this.Control[iCurrent+6]=this.rb_paid
this.Control[iCurrent+7]=this.rb_unpaid
this.Control[iCurrent+8]=this.gb_select_cheque_no
this.Control[iCurrent+9]=this.gb_select_payments
this.Control[iCurrent+10]=this.cb_ok
this.Control[iCurrent+11]=this.cb_cancel
this.Control[iCurrent+12]=this.st_1
this.Control[iCurrent+13]=this.sle_cheque_no
end on

on w_filter_transaction_list.destroy
call super::destroy
destroy(this.dw_payment_type)
destroy(this.dw_filter_recipients)
destroy(this.dw_screen_type)
destroy(this.dw_filter_dates)
destroy(this.rb_all_payments)
destroy(this.rb_paid)
destroy(this.rb_unpaid)
destroy(this.gb_select_cheque_no)
destroy(this.gb_select_payments)
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.st_1)
destroy(this.sle_cheque_no)
end on

type dw_payment_type from u_dw_online within w_filter_transaction_list
integer x = 46
integer y = 860
integer width = 1609
integer height = 212
integer taborder = 60
string title = "none"
string dataobject = "d_filter_pymt_types"
boolean border = false
boolean livescroll = true
end type

event itemchanged;DATAWINDOWCHILD	ldwc_child
STRING ls_description , ls_payment_type_desc , ls_payment_sub_type_desc
LONG ll_row

THIS.GetChild('c_type_and_sub_type',ldwc_child)
ll_row = ldwc_child.GetRow()

ls_payment_type_desc = ldwc_child.GetItemString(ll_row, "payment_type_desc")
ls_payment_sub_type_desc = ldwc_child.GetItemString(ll_row, "payment_sub_type_desc")
IF ls_payment_sub_type_desc = "" OR IsNull(ls_payment_sub_type_desc) THEN
	ll_row = THIS.GetRow()
	THIS.Object.payment_description.Text = ls_payment_type_desc
ELSE
	THIS.Object.payment_description.Text =  ls_payment_type_desc + " - " + ls_payment_sub_type_desc
END IF

end event

type dw_filter_recipients from u_dw_online within w_filter_transaction_list
integer x = 50
integer y = 1096
integer width = 1609
integer height = 368
integer taborder = 30
string dataobject = "d_filter_recipients"
boolean border = false
end type

event itemchanged;call super::itemchanged;
CHOOSE CASE This.GetColumnName()
	CASE "recipient_type_code"
		IF This.GetText() = "" THEN
			dw_filter_recipients.SetItem(1,"recipient_no",0)
			dw_filter_recipients.SetTabOrder("recipient_no",0)
		ELSE
			dw_filter_recipients.SetTabOrder("recipient_no",20)
		END IF
END CHOOSE
end event

type dw_screen_type from u_dw_online within w_filter_transaction_list
integer x = 41
integer y = 568
integer width = 1609
integer height = 300
integer taborder = 50
string dataobject = "d_screen_type"
boolean border = false
end type

type dw_filter_dates from u_dw_online within w_filter_transaction_list
integer x = 101
integer y = 424
integer width = 1088
integer height = 120
integer taborder = 20
string dataobject = "d_filter_dates"
boolean border = false
end type

on getfocus;call u_dw_online::getfocus;rb_paid.checked = TRUE
end on

type rb_all_payments from radiobutton within w_filter_transaction_list
integer x = 137
integer y = 120
integer width = 421
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "All Payments"
boolean checked = true
end type

on clicked;DATETIME	lt_null

SetNull(lt_null)

dw_filter_dates.SetItem(1,"paid_from",lt_null)
dw_filter_dates.SetItem(1,"paid_to",lt_null)
end on

type rb_paid from radiobutton within w_filter_transaction_list
integer x = 137
integer y = 296
integer width = 421
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Paid"
end type

type rb_unpaid from radiobutton within w_filter_transaction_list
integer x = 137
integer y = 208
integer width = 421
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "UnPaid"
end type

on clicked;DATETIME	lt_null

SetNull(lt_null)

dw_filter_dates.SetItem(1,"paid_from",lt_null)
dw_filter_dates.SetItem(1,"paid_to",lt_null)
end on

type gb_select_cheque_no from groupbox within w_filter_transaction_list
integer x = 69
integer y = 1476
integer width = 1573
integer height = 244
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Select Cheque No"
end type

type gb_select_payments from groupbox within w_filter_transaction_list
integer x = 59
integer y = 28
integer width = 1573
integer height = 524
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Select Payments"
end type

type cb_ok from commandbutton within w_filter_transaction_list
integer x = 558
integer y = 1760
integer width = 274
integer height = 108
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;DATETIME ldtm_from, ldtm_to, ldtm_null
STRING	ls_payment_filter, ls_recipient_filter, ls_recipient_type_code, ls_return_filter, ls_screen_filter, ls_screen_type_code, ls_cheque_filter, ls_joiner
LONG		ll_recipient_no
STRING	ls_payment_type , ls_payment_sub_type , ls_payment_type_and_sub_type , ls_payment_type_filter

	SetPointer(HourGlass!)
	SetNull(ldtm_null)

	IF dw_filter_dates.AcceptText() < 0 THEN
		Return
	END IF

	IF dw_filter_recipients.AcceptText() < 0 THEN
		Return
	END IF

	IF dw_screen_type.AcceptText() < 0 THEN
		Return
	END IF

/*	Check the selected option from the "select payments" group box and create the filter
*/

	IF rb_all_payments.checked = TRUE THEN
		ls_payment_filter = ""
	ELSEIF rb_unpaid.checked = TRUE THEN
		dw_filter_dates.SetItem(1,"paid_from",ldtm_null)
		dw_filter_dates.SetItem(1,"paid_to",ldtm_null)
		ls_payment_filter = "(IsNull(applied_processed_date))"
	ELSE

		ldtm_from = dw_filter_dates.GetItemDateTime(1,"paid_from")
		ldtm_to   = dw_filter_dates.GetItemDateTime(1,"paid_to")

		// Note: We have to be careful with the dates as processed_date contains the time

		IF (IsNull(ldtm_from) OR String(ldtm_from) = "0000 01 01 00:00:00") AND &
			(IsNull(ldtm_to)   OR String(ldtm_to)   = "0000 01 01 00:00:00") THEN
			ls_payment_filter = "(not IsNull(applied_processed_date))"

		ELSEIF IsNull(ldtm_from) OR string(ldtm_from) = "0000 01 01 00:00:00" THEN
			ldtm_to = DateTime(RelativeDate(Date(ldtm_to),1))
			ls_payment_filter = "(applied_processed_date < " + String(ldtm_to,'yyyy-mm-dd') + ")"

		ELSEIF IsNull(ldtm_to)   OR string(ldtm_to) = "0000 01 01 00:00:00"  THEN
			ls_payment_filter = "(applied_processed_date >= " + String(ldtm_from,'yyyy-mm-dd') + ")"
		ELSE
			IF ldtm_from > ldtm_to THEN
				MessageBox("Payment Inquiry - Validation Error","The from date cannot be after the to date",Exclamation!)
				Return
			END IF
			ldtm_to = DateTime(RelativeDate(Date(ldtm_to),1))
			ls_payment_filter = "(applied_processed_date >= " + String(ldtm_from,'yyyy-mm-dd') + " and applied_processed_date < " + string(ldtm_to,'yyyy-mm-dd') + ")"
		END IF
	END IF

// Check the selected option from the "select type" group box and create the filter
	ls_screen_type_code = dw_screen_type.GetItemString(1,"screen_type")

	IF ls_screen_type_code <> "all" THEN
		ls_screen_filter = "authorization_type_code = '" + ls_screen_type_code + "'"
	ELSE
		ls_screen_filter = ""
	END IF
	
// Check the selected option from the "select payment type and sub type" group box and create the filter
	ls_payment_type_and_sub_type = dw_payment_type.GetItemString(1,"c_type_and_sub_type")
	IF ls_payment_type_and_sub_type <> "" THEN
		ls_payment_type = Left(ls_payment_type_and_sub_type,2)
		IF Len(ls_payment_type_and_sub_type) > 2 THEN
			ls_payment_sub_type = Right(ls_payment_type_and_sub_type,2)
			ls_payment_type_filter = "payment_type_code = '" + ls_payment_type + &
			"' AND payment_sub_type_code = '" + ls_payment_sub_type + "'"
		ELSE
			ls_payment_type_filter = "payment_type_code = '" + ls_payment_type + "'"
		END IF

	ELSE
		ls_payment_type_filter = ""
	END IF

// Check the selected option from the "select recipients" group box and create the filter
	ls_recipient_type_code = dw_filter_recipients.GetItemString(1,"recipient_type_code")
	ll_recipient_no   = dw_filter_recipients.GetItemNumber(1,"recipient_no")
	IF ls_recipient_type_code <> "" THEN
		ls_recipient_filter = "recipient_type_code = '" + ls_recipient_type_code + "'"
		IF ll_recipient_no > 0 THEN
			 ls_recipient_filter = ls_recipient_filter + " and recipient_no = " + string(ll_recipient_no)
		END IF
	ELSE
		ls_recipient_filter = ""
	END IF

// Check for a cheque no and create the filter
	ls_cheque_filter = ""
	IF IsNumber(sle_cheque_no.Text) THEN
		ls_cheque_filter = "cheque_no = " + sle_cheque_no.Text
	END IF

// Build the return filter
	ls_return_filter = ""
	ls_joiner = ""
	IF ls_payment_filter <> "" THEN
		ls_return_filter = ls_return_filter + ls_joiner + ls_payment_filter
		ls_joiner = " and "
	END IF
	IF ls_recipient_filter <> "" THEN
		ls_return_filter = ls_return_filter + ls_joiner + ls_recipient_filter
		ls_joiner = " and "
	END IF
	IF ls_screen_filter <> "" THEN
		ls_return_filter = ls_return_filter + ls_joiner + ls_screen_filter
		ls_joiner = " and "
	END IF
	IF ls_payment_type_filter <> "" THEN
		ls_return_filter = ls_return_filter + ls_joiner + ls_payment_type_filter
		ls_joiner = " and "
	END IF	
	IF ls_cheque_filter <> "" THEN
		ls_return_filter = ls_return_filter + ls_joiner + ls_cheque_filter
		ls_joiner = " and "
	END IF
	
	CloseWithReturn(w_filter_transaction_list,ls_return_filter)
end event

type cb_cancel from commandbutton within w_filter_transaction_list
integer x = 841
integer y = 1760
integer width = 274
integer height = 108
integer taborder = 70
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
boolean cancel = true
end type

on clicked;	
	SetPointer(HourGlass!)
	CloseWithReturn(w_filter_transaction_list,"Cancel")
end on

type st_1 from statictext within w_filter_transaction_list
integer x = 128
integer y = 1600
integer width = 320
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Cheque No.:"
boolean focusrectangle = false
end type

type sle_cheque_no from singlelineedit within w_filter_transaction_list
integer x = 466
integer y = 1588
integer width = 293
integer height = 88
integer taborder = 32
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type

