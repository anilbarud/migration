$PBExportHeader$w_cheque_inquiry.srw
forward
global type w_cheque_inquiry from w_a_report
end type
type dw_enter_date_range from u_dw_online within w_cheque_inquiry
end type
type cb_ok from commandbutton within w_cheque_inquiry
end type
type gb_1 from groupbox within w_cheque_inquiry
end type
type st_1 from statictext within w_cheque_inquiry
end type
type sle_cheque_no from singlelineedit within w_cheque_inquiry
end type
type st_3 from statictext within w_cheque_inquiry
end type
type st_2 from statictext within w_cheque_inquiry
end type
type sle_amount from singlelineedit within w_cheque_inquiry
end type
type cb_clear_criteria from commandbutton within w_cheque_inquiry
end type
end forward

global type w_cheque_inquiry from w_a_report
string title = "Cheque Inquiry Report"
dw_enter_date_range dw_enter_date_range
cb_ok cb_ok
gb_1 gb_1
st_1 st_1
sle_cheque_no sle_cheque_no
st_3 st_3
st_2 st_2
sle_amount sle_amount
cb_clear_criteria cb_clear_criteria
end type
global w_cheque_inquiry w_cheque_inquiry

type variables

end variables

event open;call super::open;	LONG ll_result

/*	Database Connections 
*/

	dw_report.SetTransObject(SQLCA)
	dw_enter_date_range.SetTransObject(SQLCA)

/*	Insert a row for the date range.
*/
	dw_enter_date_range.InsertRow(0)

end event

on w_cheque_inquiry.create
int iCurrent
call super::create
this.dw_enter_date_range=create dw_enter_date_range
this.cb_ok=create cb_ok
this.gb_1=create gb_1
this.st_1=create st_1
this.sle_cheque_no=create sle_cheque_no
this.st_3=create st_3
this.st_2=create st_2
this.sle_amount=create sle_amount
this.cb_clear_criteria=create cb_clear_criteria
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_enter_date_range
this.Control[iCurrent+2]=this.cb_ok
this.Control[iCurrent+3]=this.gb_1
this.Control[iCurrent+4]=this.st_1
this.Control[iCurrent+5]=this.sle_cheque_no
this.Control[iCurrent+6]=this.st_3
this.Control[iCurrent+7]=this.st_2
this.Control[iCurrent+8]=this.sle_amount
this.Control[iCurrent+9]=this.cb_clear_criteria
end on

on w_cheque_inquiry.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.dw_enter_date_range)
destroy(this.cb_ok)
destroy(this.gb_1)
destroy(this.st_1)
destroy(this.sle_cheque_no)
destroy(this.st_3)
destroy(this.st_2)
destroy(this.sle_amount)
destroy(this.cb_clear_criteria)
end on

type dw_report from w_a_report`dw_report within w_cheque_inquiry
integer taborder = 60
string dataobject = "d_cheque_inquiry"
boolean hscrollbar = true
boolean border = false
boolean hsplitscroll = true
end type

type dw_enter_date_range from u_dw_online within w_cheque_inquiry
integer x = 233
integer y = 264
integer width = 1298
integer height = 92
integer taborder = 40
boolean bringtotop = true
string dataobject = "d_enter_date_range"
boolean border = false
boolean livescroll = true
end type

type cb_ok from commandbutton within w_cheque_inquiry
integer x = 2153
integer y = 168
integer width = 389
integer height = 108
integer taborder = 50
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;/*	Initialization
*/
	LONG 		ll_numrows
	STRING	as_cheque_no, as_cheque_amount, ls_date_filter, ls_cheque_filter, ls_amount_filter
	STRING	ls_filter, ls_original_select, ls_mod_string
	DATETIME	adt_start_date, adt_end_date, adt_today
	
	dw_enter_date_range.AcceptText()

/*	Get the start and end dates and validate them.
*/
	adt_start_date = dw_enter_date_range.GetItemDateTime(1,"from_date")
	adt_end_date = datetime(relativedate(date(dw_enter_date_range.GetItemDateTime(1,"to_date")),1))
	
	IF (IsNull(adt_start_date) AND NOT IsNull(adt_end_date)) OR &
		(IsNull(adt_end_date) AND NOT IsNull(adt_start_date))THEN
		MessageBox("Validation Error","You must provide both the start and end dates.",Exclamation!)
		Return
	END IF

	IF adt_start_date < DateTime(Date(1900,01,01)) OR &
		adt_end_date < DateTime(Date(1900,01,01)) OR &
		adt_start_date > DateTime(Date(2079,06,06)) OR &
		adt_end_date > DateTime(Date(2079,06,06)) THEN
		MessageBox("Validation Error","Dates cannot be earlier than 1900-01-01 or later than 2079-06-06!",Exclamation!)
		Return
	END IF

	IF adt_start_date > datetime(relativedate(date(adt_end_date), -1)) THEN
		MessageBox("Validation Error","The from date cannot be after the to date",Exclamation!)
		Return
	END IF

	adt_today = f_server_datetime()
	adt_today = DateTime(RelativeDate(Date(adt_today), 1))

	IF adt_end_date > adt_today THEN
		MessageBox("Validation Error","The to date must not be greater than the current date",Exclamation!)
		Return
	END IF

	IF IsNumber(sle_amount.Text) AND IsNull(adt_start_date) THEN
		MessageBox("Validation Error","A date range must be entered when searching for an amount",Exclamation!)
		Return
	END IF
		
// create date filter
	ls_date_filter = ""
	IF NOT IsNull(adt_start_date) THEN
		ls_date_filter = "(CHEQUE_HEADER.cheque_date >= ~"" + String(adt_start_date,'yyyy-mm-dd') + "~")"
		ls_date_filter += " and (CHEQUE_HEADER.cheque_date < ~"" + String(adt_end_date,'yyyy-mm-dd') + "~")"
	END IF

// Check for a cheque no and create the filter
	ls_cheque_filter = ""
	IF IsNumber(sle_cheque_no.Text) THEN
		ls_cheque_filter = "CHEQUE_HEADER.cheque_no = " + sle_cheque_no.Text
	END IF

// Check for a cheque amount and create the filter
	ls_amount_filter = ""
	IF IsNumber(sle_amount.Text) THEN
		ls_amount_filter = "CHEQUE_HEADER.cheque_amount = " + sle_amount.Text
	END IF

	IF ls_date_filter = "" AND ls_cheque_filter = "" AND ls_amount_filter = "" THEN
		MessageBox("Validation Error","You must provide some kind of criteria.",Exclamation!)
		Return
	END IF
	
// Build the return filter
	ls_filter = ""
	IF ls_date_filter <> "" THEN
		ls_filter = ls_filter + " where " + ls_date_filter
	END IF
	IF ls_cheque_filter <> "" THEN
		IF ls_filter = '' THEN
			ls_filter = ls_filter + " where " + ls_cheque_filter
		ELSE
			ls_filter = ls_filter + " and " + ls_cheque_filter
		END IF
	END IF
	IF ls_amount_filter <> "" THEN
		IF ls_filter = '' THEN
			ls_filter = ls_filter + " where " + ls_amount_filter
		ELSE
			ls_filter = ls_filter + " and " + ls_amount_filter
		END IF
	END IF

//	Modify the select and retrieve the report.

	ls_original_select = dw_report.Describe("DataWindow.Table.Select")

	ls_mod_string = "DataWindow.Table.Select='" + ls_original_select + ls_filter + "'"

	dw_report.Modify(ls_mod_string)
	
	ll_numrows = dw_report.Retrieve(MID(ls_filter, 5, 255))
	IF SQLCA.nf_handle_error("w_cheque_inquiry","dw_report","cb_ok") < 0 THEN
		Return
	END IF

	IF ll_numrows <= 0 then
		MessageBox("Cheque Inquiry Report","No data found to satisfy request.")
	END IF

// Set the select back to original
	ls_mod_string = "DataWindow.Table.Select='" + ls_original_select + "'"
	dw_report.Modify(ls_mod_string)
end event

type gb_1 from groupbox within w_cheque_inquiry
integer x = 50
integer y = 40
integer width = 1518
integer height = 352
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Select Criteria"
end type

type st_1 from statictext within w_cheque_inquiry
integer x = 73
integer y = 136
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

type sle_cheque_no from singlelineedit within w_cheque_inquiry
integer x = 421
integer y = 132
integer width = 329
integer height = 72
integer taborder = 20
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

type st_3 from statictext within w_cheque_inquiry
integer x = 841
integer y = 136
integer width = 247
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
string text = "Amount:"
boolean focusrectangle = false
end type

type st_2 from statictext within w_cheque_inquiry
integer x = 841
integer y = 80
integer width = 247
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
string text = "Cheque"
boolean focusrectangle = false
end type

type sle_amount from singlelineedit within w_cheque_inquiry
integer x = 1115
integer y = 132
integer width = 329
integer height = 72
integer taborder = 30
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

type cb_clear_criteria from commandbutton within w_cheque_inquiry
integer x = 2153
integer y = 336
integer width = 389
integer height = 108
integer taborder = 41
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Clear Criteria"
end type

event clicked;sle_cheque_no.Text = ''
sle_amount.Text = ''
dw_enter_date_range.Reset()
dw_enter_date_range.InsertRow(0)
end event

