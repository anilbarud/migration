$PBExportHeader$w_scheduled_payments.srw
$PBExportComments$Report that lists all payments scheduled for processing with in a user specified time frame for a specified user id and benefit payment.
forward
global type w_scheduled_payments from w_a_report
end type
type cb_ok from commandbutton within w_scheduled_payments
end type
type dw_enter_date_range from u_dw_online within w_scheduled_payments
end type
type st_1 from statictext within w_scheduled_payments
end type
type gb_select_criteria from groupbox within w_scheduled_payments
end type
type dw_display_user_list from u_dw_online within w_scheduled_payments
end type
type st_2 from statictext within w_scheduled_payments
end type
type dw_benefit_type_list from u_dw_online within w_scheduled_payments
end type
end forward

global type w_scheduled_payments from w_a_report
integer y = 49
integer height = 2761
string title = "Scheduled Payments Report"
cb_ok cb_ok
dw_enter_date_range dw_enter_date_range
st_1 st_1
gb_select_criteria gb_select_criteria
dw_display_user_list dw_display_user_list
st_2 st_2
dw_benefit_type_list dw_benefit_type_list
end type
global w_scheduled_payments w_scheduled_payments

type variables
DATAWINDOWCHILD	iw_user_list
DATAWINDOWCHILD	iw_benefit_type_list
end variables

on open;call w_a_report::open;/*	This script controls setting up and defaulting the search criteria
	used to retrieve scheduled payments.
*/

	LONG			ll_result

/* Populate the list of user names and default to the current user's id.
*/
	dw_display_user_list.GetChild("user_id",iw_user_list)
	iw_user_list.SetTransObject(SQLCA)
	ll_result = iw_user_list.Retrieve()
	SQLCA.nf_handle_error("w_scheduled_payments","dw_display_user_list","open of w_scheduled_payments")
	iw_user_list.SetSort("computed_user_full_name A")
	iw_user_list.Sort()
	dw_display_user_list.InsertRow(0)
	ll_result = dw_display_user_list.SetItem(1,"user_id",vgst_user_profile.user_id)
	IF ll_result <= 0 then
		MessageBox("Report Module - Scheduled Payments","You have not been set up as a valid user in the Authorization table~r~nYou cannot access this report",Exclamation!)
		Close(this)
		Return
	END IF

/* Populate the list of Benefit Types.
*/
	dw_benefit_type_list.GetChild("benefit_category_code",iw_benefit_type_list)
	iw_benefit_type_list.SetTransObject(SQLCA)
	ll_result = iw_benefit_type_list.Retrieve()
	SQLCA.nf_handle_error("w_scheduled_payments","dw_display_user_list","open of w_scheduled_payments")

	dw_benefit_type_list.InsertRow(0)
	dw_benefit_type_list.SetItem(1,"benefit_category_code","LTD")

/* Set up the dw to accept the date range.
*/
	dw_enter_date_range.InsertRow(0)

/*	Set up the report datawindow.
*/
	dw_report.SetTransObject(SQLCA)
	//dw_report.f_SetSelect(3)

end on

on w_scheduled_payments.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.dw_enter_date_range=create dw_enter_date_range
this.st_1=create st_1
this.gb_select_criteria=create gb_select_criteria
this.dw_display_user_list=create dw_display_user_list
this.st_2=create st_2
this.dw_benefit_type_list=create dw_benefit_type_list
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.dw_enter_date_range
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.gb_select_criteria
this.Control[iCurrent+5]=this.dw_display_user_list
this.Control[iCurrent+6]=this.st_2
this.Control[iCurrent+7]=this.dw_benefit_type_list
end on

on w_scheduled_payments.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.dw_enter_date_range)
destroy(this.st_1)
destroy(this.gb_select_criteria)
destroy(this.dw_display_user_list)
destroy(this.st_2)
destroy(this.dw_benefit_type_list)
end on

type dw_report from w_a_report`dw_report within w_scheduled_payments
integer taborder = 50
string dataobject = "d_scheduled_payments"
boolean hscrollbar = true
end type

type cb_ok from commandbutton within w_scheduled_payments
integer x = 2153
integer y = 212
integer width = 389
integer height = 108
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;/*	Variables
*/

/* RULES: For the Scheduled Payment Report - One month in the past, 
          and One year in the future. 
*/

STRING	ls_user_id, ls_benefit_category_code
LONG 		ll_numrows
DATETIME	ldt_from_date, ldt_to_date, ldt_current
INTEGER  li_month_number,li_month_days
BOOLEAN  lb_leap

dw_enter_date_range.AcceptText()

/*	Acquire the user_id and requested benefit type.
*/
	ls_user_id = dw_display_user_list.GetItemString(1,"user_id")
	ls_benefit_category_code = dw_benefit_type_list.GetItemString(1,"benefit_category_code")

/* Validate date range. To date must be greater than the from date. As well, add one to the to date
	to ensure getting all items created that day, regardless of what time they were created.
*/ 
	ldt_from_date = dw_enter_date_range.GetItemDateTime(1,"from_date")
	ldt_to_date = datetime(relativedate(date(dw_enter_date_range.GetItemDateTime(1,"to_date")),1))
	ldt_current = f_server_datetime()

	IF IsNull(ldt_from_date) or &
		IsNull(ldt_to_date) THEN
		MessageBox("Validation Error","Both the from and to dates must have a value.",Exclamation!)
		Return
	END IF
	
	IF ldt_from_date >= ldt_to_date then
		MessageBox("Validation Error","The to date must be later than the from date",Exclamation!)
		Return
	END IF

//P10229
	li_month_number = Month(Date(ldt_current))
	
	CHOOSE CASE li_month_number
		CASE 1,3,5,7,8,10,12
			li_month_days = 31
		CASE 4,6,9,11
			li_month_days = 30
		CASE 2
			IF Mod(Year(Date(ldt_current)),4) = 0 THEN
				lb_leap = TRUE
				li_month_days = 29
			ELSE
				li_month_days = 28
			END IF
	END CHOOSE
			
	IF ldt_from_date < DateTime(RelativeDate ( Date(ldt_current) , li_month_days*-1 )) THEN
		MessageBox("Validation Error","From date cannot be more than a month in the past.",Exclamation!)
		Return
	END IF

	IF lb_leap THEN
		IF ldt_to_date > DateTime(RelativeDate ( Date(ldt_current) , 367 )) THEN
			MessageBox("Validation Error","To date cannot be more than a year in the future.",Exclamation!)
			Return
		END IF
	ELSE
		IF ldt_to_date > DateTime(RelativeDate ( Date(ldt_current) , 366 )) THEN
			MessageBox("Validation Error","To date cannot be more than a year in the future.",Exclamation!)
			Return
		END IF		
	END IF

/*	Retrieve the report. It requires the user id, benefit type and from and to dates for parameters
*/
	ll_numrows = dw_report.Retrieve(ls_user_id,ls_benefit_category_code,ldt_from_date,ldt_to_date)
	SQLCA.nf_handle_error("w_scheduled_payments","dw_report","cb_ok")
	If ll_numrows <= 0 then
		MessageBox("Scheduled Payments Report","No data found to satisfy request.")
	End If

end event

type dw_enter_date_range from u_dw_online within w_scheduled_payments
integer x = 96
integer y = 352
integer width = 1385
integer height = 92
integer taborder = 30
boolean bringtotop = true
string dataobject = "d_enter_date_range"
boolean border = false
boolean livescroll = true
end type

type st_1 from statictext within w_scheduled_payments
integer x = 87
integer y = 232
integer width = 347
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
string text = "Benefit Type:"
alignment alignment = center!
boolean focusrectangle = false
end type

type gb_select_criteria from groupbox within w_scheduled_payments
integer x = 50
integer y = 40
integer width = 1518
integer height = 444
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Payment Selection Criteria"
end type

type dw_display_user_list from u_dw_online within w_scheduled_payments
integer x = 443
integer y = 116
integer width = 1097
integer height = 92
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_select_user"
boolean border = false
boolean livescroll = true
end type

type st_2 from statictext within w_scheduled_payments
integer x = 87
integer y = 124
integer width = 311
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
string text = "Created By:"
alignment alignment = center!
boolean focusrectangle = false
end type

type dw_benefit_type_list from u_dw_online within w_scheduled_payments
integer x = 448
integer y = 220
integer width = 891
integer height = 92
integer taborder = 20
boolean bringtotop = true
string dataobject = "d_display_benefit_categories"
boolean border = false
boolean livescroll = true
end type

