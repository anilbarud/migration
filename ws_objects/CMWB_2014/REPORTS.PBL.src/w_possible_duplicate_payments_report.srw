$PBExportHeader$w_possible_duplicate_payments_report.srw
$PBExportComments$Report listing all records in User_Msg_Log for a given date range and region
forward
global type w_possible_duplicate_payments_report from w_a_report
end type
type cb_ok from commandbutton within w_possible_duplicate_payments_report
end type
type dw_enter_date_range from u_dw_online within w_possible_duplicate_payments_report
end type
type gb_1 from groupbox within w_possible_duplicate_payments_report
end type
type st_2 from statictext within w_possible_duplicate_payments_report
end type
type dw_region_code from u_dw_online within w_possible_duplicate_payments_report
end type
end forward

global type w_possible_duplicate_payments_report from w_a_report
integer width = 2747
string title = "Possible Duplicate Payments Report"
cb_ok cb_ok
dw_enter_date_range dw_enter_date_range
gb_1 gb_1
st_2 st_2
dw_region_code dw_region_code
end type
global w_possible_duplicate_payments_report w_possible_duplicate_payments_report

type variables
datawindowchild	viw_region_list
end variables

event open;call super::open;String	ls_user_region_code
Long		ll_row
datawindowchild	ldwc_region_code
Integer	li_return
Date		ld_today


ld_today = today()

dw_report.settransobject(sqlca)
dw_region_code.settransobject(sqlca)
li_return = dw_region_code.getchild("admin_region_code", ldwc_region_code)
//dw_report2.settransobject(sqlca)
dw_report.uf_SetSelect(3)
//dw_report2.uf_SetSelect(3)

dw_report.visible = True
ll_row = dw_region_code.Insertrow(0)
dw_enter_date_range.Insertrow(0)
dw_enter_date_range.SetFocus()

ll_row = ldwc_region_code.insertrow(0)
ldwc_region_code.setitem(ll_row,"admin_region_code","ALL")
ldwc_region_code.setitem(ll_row,"admin_region_desc","ALL REGIONS")

//ma
ld_today = date(year(ld_today),month(ld_today),1)
ld_today = relativedate(ld_today,-2)
ld_today = date(year(ld_today),month(ld_today),1)

dw_enter_date_range.setitem(1,"from_date",date(year(ld_today), month(ld_today),1))
dw_enter_date_range.triggerevent(itemchanged!)


ls_user_region_code = vgst_user_profile.default_admin_region_code
if ls_user_region_code <> "" then
	ll_row = ldwc_region_code.find("admin_region_code = " + "'" + ls_user_region_code + "'", 1, ldwc_region_code.rowcount())
	if ll_row > 0 then
		ldwc_region_code.scrolltorow(ll_row)
		dw_region_code.setItem(1,'admin_region_code',  ls_user_region_code)
	end if
End if

end event

event ue_print;//If rb_date.Checked Then
//	dw_report2.Print()
//Else
	dw_report.object.datawindow.print.orientation = 1
	dw_report.Print()
//End IF
end event

on w_possible_duplicate_payments_report.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.dw_enter_date_range=create dw_enter_date_range
this.gb_1=create gb_1
this.st_2=create st_2
this.dw_region_code=create dw_region_code
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.dw_enter_date_range
this.Control[iCurrent+3]=this.gb_1
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.dw_region_code
end on

on w_possible_duplicate_payments_report.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.dw_enter_date_range)
destroy(this.gb_1)
destroy(this.st_2)
destroy(this.dw_region_code)
end on

event resize;call super::resize;dw_report.width = this.width - 100
dw_report.height = this.height - 600
end event

type dw_report from w_a_report`dw_report within w_possible_duplicate_payments_report
integer x = 32
integer y = 440
integer height = 2092
integer taborder = 0
string dataobject = "d_possible_duplicate_payments"
boolean hscrollbar = true
end type

type cb_ok from commandbutton within w_possible_duplicate_payments_report
integer x = 2331
integer y = 88
integer width = 334
integer height = 108
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;LONG 		ll_numrows
DATETIME	ldt_from_date, ldt_to_date
STRING	ls_region_code

/*	Validate dates entered
*/
	If dw_enter_date_range.AcceptText() < 0 Then
		Return
	End IF

	ldt_from_date = dw_enter_date_range.GetItemDateTime(1,"from_date")
	ldt_to_date = dw_enter_date_range.GetItemDateTime(1,"to_date")

	If IsNull(ldt_from_date) or ldt_from_date < DateTime(Date(1900,01,01)) Then
		MessageBox("Validation Error","The 'From' date cannot be before 1900-01-01",Exclamation!)
		Return
	End If

	If IsNull(ldt_to_date) or ldt_to_date < DateTime(Date(1900,01,01)) Then
		MessageBox("Validation Error","The 'To' date cannot be before 1900-01-01",Exclamation!)
		Return
	End If

	IF ldt_from_date > DateTime(Date(2079,06,06)) OR &
		ldt_to_date > DateTime(Date(2079,06,06)) THEN
		MessageBox("Validation Error","Dates cannot be later than 2079-06-06!",Exclamation!)
		Return
	END IF
	
	If ldt_from_date > ldt_to_date then
		MessageBox("Validation Error","The 'From' date cannot be after the 'To' date",Exclamation!)
		Return
	End If

//	If DaysAfter(date(ldt_from_date),date(ldt_to_date)) > 7 Then
//		MessageBox("Validation Error","This is a weekly report.~r~nThe to date cannot be more than 7 days after the from date",Exclamation!)
//		Return
//	End If
	If DaysAfter(date(ldt_from_date),date(ldt_to_date)) > 31 Then
		MessageBox("Validation Error","This is a monthly report.~r~nThe to date cannot be more than 31 days after the from date",Exclamation!)
		Return
	End If

/* Add 1 to the 'to date' since the query looks for less than (it can't use equal since it's a date & time
	and we don't know what the time is)
*/

	ldt_to_date = DateTime(Relativedate(date(ldt_to_date),1))

/*	Retrieve the report (Note: for the invoice number report, set autocommit to true as it
	runs a stored procedure which creates a temporary table)
*/

	ls_region_code = dw_region_code.getitemstring(1,"admin_region_code")
	If ls_region_code = "" or ISNULL(ls_region_code) Then
		Messagebox("Validation Error", "Admin region is a required field.", Exclamation!)
		Return
	End If
	
	
	If dw_region_code.getitemstring(1,"admin_region_code") = "ALL" Then 
		dw_report.object.c_admin_region_code.expression = "'ALL'"
	else
		dw_report.object.c_admin_region_code.expression = "admin_region_code"
	End if
	ll_numrows = dw_report.Retrieve(ldt_from_date,ldt_to_date,ls_region_code)
	SQLCA.nf_handle_error("w_possible_duplicate_payments_report","dw_report","cb_ok")

	If ll_numrows <= 0 then
		MessageBox("Duplicate Payments Report","No data found to satisfy request")
	End If


end event

type dw_enter_date_range from u_dw_online within w_possible_duplicate_payments_report
integer x = 736
integer y = 264
integer width = 1253
integer height = 92
integer taborder = 30
boolean bringtotop = true
string dataobject = "d_enter_date_range"
boolean border = false
end type

event itemchanged;Date		ldtm_to_date, ldtm_from_date
String	ls_date
Date		ldt_date
Integer	li_month, li_days = 31, li_x = 1, li_year


If this.GetColumnName() = "from_date" Then
	ls_date 			= this.GetText()
	ls_date 			= Left(ls_date,10)
	ldt_date			= date(ls_date)
	IF month(ldt_date) = 12 THEN
		ldt_date = date(year(ldt_date) + 1, 1,1)
	else
		ldt_date = date(year(ldt_date),month(ldt_date) + 1,1)
	end if
	this.setitem(1,"to_date",ldt_date)
end if
end event

type gb_1 from groupbox within w_possible_duplicate_payments_report
integer x = 50
integer y = 56
integer width = 2222
integer height = 340
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Report Parameters"
end type

type st_2 from statictext within w_possible_duplicate_payments_report
integer x = 105
integer y = 276
integer width = 622
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
string text = "Payment Created Date:"
boolean focusrectangle = false
end type

type dw_region_code from u_dw_online within w_possible_duplicate_payments_report
integer x = 91
integer y = 136
integer width = 1088
integer height = 92
integer taborder = 20
boolean bringtotop = true
string dataobject = "d_display_active_admin_regions"
boolean border = false
end type

