$PBExportHeader$w_mail_package_error_report.srw
$PBExportComments$Listing of Errors From The MAil Package run
forward
global type w_mail_package_error_report from w_a_report
end type
type dw_claim_managers from u_dw_online within w_mail_package_error_report
end type
type st_1 from statictext within w_mail_package_error_report
end type
type cb_ok from commandbutton within w_mail_package_error_report
end type
type gb_1 from groupbox within w_mail_package_error_report
end type
type dw_enter_date_range from u_dw_online within w_mail_package_error_report
end type
end forward

global type w_mail_package_error_report from w_a_report
string title = "Mail Package Error Report"
dw_claim_managers dw_claim_managers
st_1 st_1
cb_ok cb_ok
gb_1 gb_1
dw_enter_date_range dw_enter_date_range
end type
global w_mail_package_error_report w_mail_package_error_report

type variables

end variables

event open;call super::open;DATAWINDOWCHILD ldwc_child
DATETIME ldtm_date

dw_report.SetTransObject (SQLCA)

IF dw_claim_managers.GetChild('claim_manager', ldwc_child) < 0 THEN
	MessageBox('Error', 'claim_manager is not a child datawindow')
	Return 0
END IF

ldwc_child.SetTransObject(SQLCA)
ldwc_child.Retrieve()
ldwc_child.InsertRow(1)
ldwc_child.SetItem(1,'user_id', '')
IF ldwc_child.Find('user_id = "' + vgst_user_profile.user_id + '"', 1, ldwc_child.RowCount()) > 0 THEN
	dw_claim_managers.SetItem(1,'claim_manager', vgst_user_profile.user_id)
END IF

ldwc_child.SelectRow(0, FALSE)

dw_enter_date_range.InsertRow(0)
ldtm_date = f_server_datetime()

dw_enter_date_range.SetRow(1)
//dw_enter_date_range.SetItem(1, 'from_date', RelativeDate(Date(ldtm_date),-30))
//dw_enter_date_range.SetItem(1, 'to_date', DateTime(Date(ldtm_date)))
dw_enter_date_range.SetColumn("from_date")
end event

on w_mail_package_error_report.create
int iCurrent
call super::create
this.dw_claim_managers=create dw_claim_managers
this.st_1=create st_1
this.cb_ok=create cb_ok
this.gb_1=create gb_1
this.dw_enter_date_range=create dw_enter_date_range
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_claim_managers
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.cb_ok
this.Control[iCurrent+4]=this.gb_1
this.Control[iCurrent+5]=this.dw_enter_date_range
end on

on w_mail_package_error_report.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.dw_claim_managers)
destroy(this.st_1)
destroy(this.cb_ok)
destroy(this.gb_1)
destroy(this.dw_enter_date_range)
end on

type dw_report from w_a_report`dw_report within w_mail_package_error_report
integer y = 460
integer height = 2020
string dataobject = "d_mail_package_error_report"
boolean hscrollbar = true
end type

type dw_claim_managers from u_dw_online within w_mail_package_error_report
integer x = 553
integer y = 80
integer width = 832
integer height = 92
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_claim_managers"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type st_1 from statictext within w_mail_package_error_report
integer x = 110
integer y = 88
integer width = 416
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Claim Manager:"
boolean focusrectangle = false
end type

type cb_ok from commandbutton within w_mail_package_error_report
integer x = 2153
integer y = 216
integer width = 389
integer height = 108
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "OK"
boolean default = true
end type

event clicked;/*	Variables and initialization.
*/
LONG 		ll_numrows
DATETIME	adt_from_date, adt_to_date, adt_today

dw_enter_date_range.AcceptText()

/*	Validate the dates and set the to date to midnight of the next day to ensure that all transactions
from the requested day are included.
*/

adt_from_date = dw_enter_date_range.GetItemDateTime(1,"from_date")
adt_to_date = DateTime(RelativeDate(Date(dw_enter_date_range.GetItemDateTime(1,"to_date")),1))

//IF IsNull(adt_from_date) or &
//	IsNull(adt_to_date) THEN
//	MessageBox("Validation Error","Both the from and to dates must have a value.",Exclamation!)
//	Return
//END IF

IF adt_from_date < DateTime(Date(1900,01,01)) OR &
	adt_to_date < DateTime(Date(1900,01,01)) THEN
	MessageBox("Validation Error","Dates cannot be earlier than 1900-01-01!",Exclamation!)
	Return
END IF

IF adt_from_date > adt_to_date then
	MessageBox("Validation Error","The from date must not be after the to date.",Exclamation!)
	Return
END IF

adt_today = f_server_datetime()
adt_today = DateTime(RelativeDate(Date(adt_today), 1))

If adt_to_date > adt_today THEN
	MessageBox("Validation Error","The to date must not be after the current date",Exclamation!)
	Return
END IF

If adt_from_date > adt_today THEN
	MessageBox("Validation Error","The from date must not be after the current date",Exclamation!)
	Return
END IF

/*	Retrieve the report.
*/
IF dw_claim_managers.GetItemString(1, 'claim_manager') = '' THEN
	dw_report.Retrieve('',adt_from_date,adt_to_date)
ELSE
	IF dw_report.Retrieve(dw_claim_managers.GetItemString(1, 'claim_manager'),adt_from_date,adt_to_date) = 0 THEN
		MessageBox('Claim Status Change Report', 'No records found for specified claim manager')
	END IF
END IF
SQLCA.nf_handle_error("w_mail_package_error_report","dw_report","cb_ok")

end event

type gb_1 from groupbox within w_mail_package_error_report
integer x = 50
integer y = 24
integer width = 1518
integer height = 352
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Parameters"
end type

type dw_enter_date_range from u_dw_online within w_mail_package_error_report
integer x = 105
integer y = 204
integer width = 1385
integer height = 92
integer taborder = 30
boolean bringtotop = true
string dataobject = "d_enter_date_range"
boolean border = false
boolean livescroll = true
end type

