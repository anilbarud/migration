$PBExportHeader$w_filter_claim_cost_transaction_list.srw
$PBExportComments$Payment Inquiry (Select Filter Option) -Popup window for selecting filter options on payment inquiry screen
forward
global type w_filter_claim_cost_transaction_list from w_ancestor
end type
type dw_filter_recipients from u_dw_online within w_filter_claim_cost_transaction_list
end type
type cbx_maintainable from checkbox within w_filter_claim_cost_transaction_list
end type
type st_2 from statictext within w_filter_claim_cost_transaction_list
end type
type dw_filter_dates from u_dw_online within w_filter_claim_cost_transaction_list
end type
type gb_select_cheque_no from groupbox within w_filter_claim_cost_transaction_list
end type
type gb_select_payments from groupbox within w_filter_claim_cost_transaction_list
end type
type cb_ok from commandbutton within w_filter_claim_cost_transaction_list
end type
type cb_cancel from commandbutton within w_filter_claim_cost_transaction_list
end type
type st_1 from statictext within w_filter_claim_cost_transaction_list
end type
type sle_cheque_no from singlelineedit within w_filter_claim_cost_transaction_list
end type
type gb_1 from groupbox within w_filter_claim_cost_transaction_list
end type
type dw_filter_payment_type_codes from u_dw_online within w_filter_claim_cost_transaction_list
end type
type gb_2 from groupbox within w_filter_claim_cost_transaction_list
end type
end forward

global type w_filter_claim_cost_transaction_list from w_ancestor
integer width = 1714
integer height = 1952
string title = "Filter Transaction List"
string menuname = ""
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
long backcolor = 67108864
dw_filter_recipients dw_filter_recipients
cbx_maintainable cbx_maintainable
st_2 st_2
dw_filter_dates dw_filter_dates
gb_select_cheque_no gb_select_cheque_no
gb_select_payments gb_select_payments
cb_ok cb_ok
cb_cancel cb_cancel
st_1 st_1
sle_cheque_no sle_cheque_no
gb_1 gb_1
dw_filter_payment_type_codes dw_filter_payment_type_codes
gb_2 gb_2
end type
global w_filter_claim_cost_transaction_list w_filter_claim_cost_transaction_list

type variables

end variables

event open;call super::open;DATAWINDOWCHILD	ldwc_filter_payment_type

DATAWINDOWCHILD	ldwc_filter_payment_sub_type

DATAWINDOWCHILD	ldwc_recipient_child
LONG					ll_row


dw_filter_recipients.SetTransObject(Sqlca)

dw_filter_recipients.InsertRow(0)
dw_filter_dates.InsertRow(0)

dw_filter_payment_type_codes.InsertRow(0)

dw_filter_recipients.GetChild("recipient_type_code",ldwc_recipient_child)
ll_row = ldwc_recipient_child.InsertRow(1)
ldwc_recipient_child.SetItem(ll_row,"recipient_type_code",'')
ldwc_recipient_child.SetItem(ll_row,"recipient_type_desc",'All recipients')

dw_filter_recipients.SetItem(1,"recipient_type_code","")

dw_filter_recipients.SetTabOrder("recipient_no",0)



dw_filter_payment_type_codes.GetChild("payment_type_code",ldwc_filter_payment_type)

ldwc_filter_payment_type.SetTransObject(SQLCA)
ldwc_filter_payment_type.Retrieve()
IF SQLCA.nf_handle_error("ldwc_filter_payment_type","w_filter_transaction_list","open for w_filter_transaction_list") < 0 THEN
	cb_cancel.TriggerEvent(Clicked!)
	Return
END IF

dw_filter_payment_type_codes.GetChild("payment_sub_type_code",ldwc_filter_payment_sub_type)
ldwc_filter_payment_sub_type.SetTransObject(SQLCA)

end event

on w_filter_claim_cost_transaction_list.create
int iCurrent
call super::create
this.dw_filter_recipients=create dw_filter_recipients
this.cbx_maintainable=create cbx_maintainable
this.st_2=create st_2
this.dw_filter_dates=create dw_filter_dates
this.gb_select_cheque_no=create gb_select_cheque_no
this.gb_select_payments=create gb_select_payments
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.st_1=create st_1
this.sle_cheque_no=create sle_cheque_no
this.gb_1=create gb_1
this.dw_filter_payment_type_codes=create dw_filter_payment_type_codes
this.gb_2=create gb_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_filter_recipients
this.Control[iCurrent+2]=this.cbx_maintainable
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.dw_filter_dates
this.Control[iCurrent+5]=this.gb_select_cheque_no
this.Control[iCurrent+6]=this.gb_select_payments
this.Control[iCurrent+7]=this.cb_ok
this.Control[iCurrent+8]=this.cb_cancel
this.Control[iCurrent+9]=this.st_1
this.Control[iCurrent+10]=this.sle_cheque_no
this.Control[iCurrent+11]=this.gb_1
this.Control[iCurrent+12]=this.dw_filter_payment_type_codes
this.Control[iCurrent+13]=this.gb_2
end on

on w_filter_claim_cost_transaction_list.destroy
call super::destroy
destroy(this.dw_filter_recipients)
destroy(this.cbx_maintainable)
destroy(this.st_2)
destroy(this.dw_filter_dates)
destroy(this.gb_select_cheque_no)
destroy(this.gb_select_payments)
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.st_1)
destroy(this.sle_cheque_no)
destroy(this.gb_1)
destroy(this.dw_filter_payment_type_codes)
destroy(this.gb_2)
end on

type dw_filter_recipients from u_dw_online within w_filter_claim_cost_transaction_list
integer y = 1056
integer width = 1609
integer height = 368
integer taborder = 40
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

type cbx_maintainable from checkbox within w_filter_claim_cost_transaction_list
integer x = 366
integer y = 640
integer width = 699
integer height = 64
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Show Only Maintainable"
end type

type st_2 from statictext within w_filter_claim_cost_transaction_list
integer x = 73
integer y = 128
integer width = 453
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Date Range:"
boolean focusrectangle = false
end type

type dw_filter_dates from u_dw_online within w_filter_claim_cost_transaction_list
integer x = 101
integer y = 232
integer width = 1088
integer height = 120
integer taborder = 20
string dataobject = "d_filter_dates"
boolean border = false
end type

event getfocus;call super::getfocus;//rb_paid.checked = TRUE
end event

type gb_select_cheque_no from groupbox within w_filter_claim_cost_transaction_list
integer x = 32
integer y = 1412
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

type gb_select_payments from groupbox within w_filter_claim_cost_transaction_list
integer x = 37
integer y = 32
integer width = 1573
integer height = 448
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

type cb_ok from commandbutton within w_filter_claim_cost_transaction_list
integer x = 521
integer y = 1696
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
STRING	ls_payment_type , ls_payment_sub_type , ls_payment_type_and_sub_type , ls_payment_type_filter,ls_maintainable_filter

	SetPointer(HourGlass!)
	SetNull(ldtm_null)

	IF dw_filter_dates.AcceptText() < 0 THEN
		Return
	END IF

	IF dw_filter_recipients.AcceptText() < 0 THEN
		Return
	END IF


	IF dw_filter_payment_type_codes.AcceptText() < 0 THEN
		Return
	END IF
	

/*	Check the selected option from the "select payments" group box and create the filter
*/

	ls_payment_filter = ""

	ldtm_from = dw_filter_dates.GetItemDateTime(1,"paid_from")
	ldtm_to   = dw_filter_dates.GetItemDateTime(1,"paid_to")

	// Note: We have to be careful with the dates as processed_date contains the time

	IF (IsNull(ldtm_from) OR String(ldtm_from) = "0000 01 01 00:00:00") AND &
		(IsNull(ldtm_to)   OR String(ldtm_to)   = "0000 01 01 00:00:00") THEN
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


// Check the selected option from the "select payment type and sub type" group box and create the filter

	ls_payment_type = dw_filter_payment_type_codes.GetItemString(1,"payment_type_code")
	ls_payment_type_and_sub_type = dw_filter_payment_type_codes.GetItemString(1,"payment_sub_type_code")


	if isnull(ls_payment_type_and_sub_type) then
		ls_payment_type_filter = "payment_type_code = '" + ls_payment_type + "'" 
	elseif ls_payment_type_and_sub_type <> "" then
		ls_payment_type_filter = "payment_type_code = '" + ls_payment_type + "'" + &
		" AND payment_sub_type_code = '" + ls_payment_type_and_sub_type + "'"
	else
		ls_payment_type_filter = "payment_type_code = '" + ls_payment_type + "'" + &
		" AND payment_sub_type_code = ' '"	
	End if
				
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

	if cbx_maintainable.checked = true THEN
		ls_maintainable_filter = "maintain_allowed_flag = 'Y'"
	end if

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
	
	IF ls_maintainable_filter <> "" THEN
		ls_return_filter = ls_return_filter + ls_joiner + ls_maintainable_filter
		ls_joiner = " and "
	END IF
		
	CloseWithReturn(w_filter_claim_cost_transaction_list,ls_return_filter)
end event

type cb_cancel from commandbutton within w_filter_claim_cost_transaction_list
integer x = 805
integer y = 1696
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

event clicked;	
	SetPointer(HourGlass!)
	CloseWithReturn(w_filter_claim_cost_transaction_list,"Cancel")
end event

type st_1 from statictext within w_filter_claim_cost_transaction_list
integer x = 91
integer y = 1536
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

type sle_cheque_no from singlelineedit within w_filter_claim_cost_transaction_list
integer x = 430
integer y = 1524
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

type gb_1 from groupbox within w_filter_claim_cost_transaction_list
integer x = 37
integer y = 544
integer width = 1573
integer height = 224
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Select Maintainable"
end type

type dw_filter_payment_type_codes from u_dw_online within w_filter_claim_cost_transaction_list
integer x = 73
integer y = 896
integer width = 1280
integer height = 128
integer taborder = 30
string dataobject = "d_filter_payment_type_code"
boolean border = false
end type

event itemchanged;call super::itemchanged;DATAWINDOWCHILD	ldwc_filter_payment_sub_type
long ll_rows
		
choose case this.GetColumnName()	
	/* filter the sub type dropdown based on the payment type */
	case 'payment_type_code'
			ll_rows = this.GetChild("payment_sub_type_code",ldwc_filter_payment_sub_type)
	//		dw_filter_payment_type_codes.setitem(1,"payment_sub_type_code",'')
			ll_rows = ldwc_filter_payment_sub_type.Retrieve(DATA)
			
			
			if ll_rows = 0 then
				
				dw_filter_payment_type_codes.setitem(1,"payment_sub_type_code",'')
				
			end if
			
END CHOOSE
end event

type gb_2 from groupbox within w_filter_claim_cost_transaction_list
integer x = 37
integer y = 800
integer width = 1573
integer height = 256
integer taborder = 40
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Select Payment Type  and Sub Type"
end type

