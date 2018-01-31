$PBExportHeader$w_pension_reports.srw
$PBExportComments$Window used to produce several pension reports. The datawindow object is changed on the fly based on parameter passed in through Message.StringParm.
forward
global type w_pension_reports from w_a_report
end type
type cb_ok from commandbutton within w_pension_reports
end type
type dw_pension_enter_date_range from u_dw_online within w_pension_reports
end type
type gb_select_criteria from groupbox within w_pension_reports
end type
end forward

global type w_pension_reports from w_a_report
cb_ok cb_ok
dw_pension_enter_date_range dw_pension_enter_date_range
gb_select_criteria gb_select_criteria
end type
global w_pension_reports w_pension_reports

type variables
STRING	is_dw_object
end variables

event open;call super::open;Long     ll_row, ll_from_year, ll_from_month, ll_to_month, ll_to_year
Datetime ldt_server_datetime, ldt_from_date, ldt_to_date

//	This script sets the report data window object based on the 
// parameter passed in when the report was chosen from the menu.
is_dw_object = Message.StringParm
dw_report.DataObject = is_dw_object

// Set up the dw to accept the date range.
ll_row = dw_pension_enter_date_range.InsertRow(0)

//	Set up the report dw
dw_report.SetTransObject(SQLCA)

// Default the Report Parameters
ldt_server_datetime = f_server_datetime()
ll_from_year = Year(Date(ldt_server_datetime))
ll_from_month = Month(Date(ldt_server_datetime))

IF ll_from_month = 12 THEN
	ll_to_month = 01
	ll_to_year = ll_from_year + 1
ELSE
	ll_to_month = ll_from_month + 1
	ll_to_year = ll_from_year
END IF

ldt_from_date = DateTime(Date(String(ll_from_year) + "/" + String(ll_from_month) + "/01"))
ldt_to_date = DateTime(Date(String(ll_to_year) + "/" + String(ll_to_month) + "/01"))
ldt_to_date = Datetime(RelativeDate(Date(ldt_to_date), -1))

dw_pension_enter_date_range.SetItem(ll_row, "from_date", ldt_from_date)
dw_pension_enter_date_range.SetItem(ll_row, "to_date", ldt_to_date)
dw_pension_enter_date_range.SetItem(ll_row, "anniv_start_month", Month(Date(ldt_from_date)))
dw_pension_enter_date_range.SetItem(ll_row, "anniv_end_month", Month(Date(ldt_to_date)))
dw_pension_enter_date_range.SetItem(ll_row, "anniv_start_day", 1)
dw_pension_enter_date_range.SetItem(ll_row, "anniv_end_day", Day(Date(ldt_to_date)))
dw_pension_enter_date_range.SetItem(ll_row, "start_year", Year(Date(ldt_server_datetime)))

CHOOSE CASE is_dw_object
	CASE "d_pension_anniversary_report"
		This.Title = "Pension Anniversay Report"
		dw_pension_enter_date_range.Modify("paid_to_t.visible=0")
		dw_pension_enter_date_range.Modify("to_date.visible=0")
		dw_pension_enter_date_range.Modify("paid_from_t.visible=0")
		dw_pension_enter_date_range.Modify("from_date.visible=0")
	CASE "d_survivors_anniversary_report"
		This.Title = "Survivors Anniversay Report"
		dw_pension_enter_date_range.Modify("paid_to_t.visible=0")
		dw_pension_enter_date_range.Modify("to_date.visible=0")
		dw_pension_enter_date_range.Modify("paid_from_t.visible=0")
		dw_pension_enter_date_range.Modify("from_date.visible=0")
	CASE "d_dependent_child_by_birthdate_report"
		This.Title = "Dependent Children by Birthdate Report"
		dw_pension_enter_date_range.Modify("paid_to_t.visible=0")
		dw_pension_enter_date_range.Modify("to_date.visible=0")
		dw_pension_enter_date_range.Modify("paid_from_t.visible=0")
		dw_pension_enter_date_range.Modify("from_date.visible=0")
		dw_pension_enter_date_range.Modify("anniv_start_t.Text='Birth Date Start:'")
		dw_pension_enter_date_range.Modify("anniv_end_t.Text='Birth Date End:'")
	CASE "d_p81_ss_attaining_65_report"
		This.Title = "P81 Surviving Spouses Attaining 65 Report"
		dw_pension_enter_date_range.Modify("start_in_year_t.visible=0")
		dw_pension_enter_date_range.Modify("start_year.visible=0")
		dw_pension_enter_date_range.Modify("anniv_start_t.visible=0")
		dw_pension_enter_date_range.Modify("anniv_end_t.visible=0")
		dw_pension_enter_date_range.Modify("anniv_start_month.visible=0")
		dw_pension_enter_date_range.Modify("anniv_end_month.visible=0")
		dw_pension_enter_date_range.Modify("anniv_start_day.visible=0")
		dw_pension_enter_date_range.Modify("anniv_end_day.visible=0")
	CASE "d_all_leg_ss_attaining_65_report"
		This.Title = "Surviving Spouses Attaining 65 Report - All Legislation"
		dw_pension_enter_date_range.Modify("start_in_year_t.visible=0")
		dw_pension_enter_date_range.Modify("start_year.visible=0")
		dw_pension_enter_date_range.Modify("anniv_start_t.visible=0")
		dw_pension_enter_date_range.Modify("anniv_end_t.visible=0")
		dw_pension_enter_date_range.Modify("anniv_start_month.visible=0")
		dw_pension_enter_date_range.Modify("anniv_end_month.visible=0")
		dw_pension_enter_date_range.Modify("anniv_start_day.visible=0")
		dw_pension_enter_date_range.Modify("anniv_end_day.visible=0")
	CASE "d_dependent_child_over_18_report"
		This.Title = "Dependent Children Over 18 Report"
		dw_pension_enter_date_range.Modify("start_in_year_t.visible=0")
		dw_pension_enter_date_range.Modify("start_year.visible=0")
		dw_pension_enter_date_range.Modify("anniv_start_t.visible=0")
		dw_pension_enter_date_range.Modify("anniv_end_t.visible=0")
		dw_pension_enter_date_range.Modify("anniv_start_month.visible=0")
		dw_pension_enter_date_range.Modify("anniv_end_month.visible=0")
		dw_pension_enter_date_range.Modify("anniv_start_day.visible=0")
		dw_pension_enter_date_range.Modify("anniv_end_day.visible=0")
END CHOOSE

dw_pension_enter_date_range.SetColumn("from_date")
dw_pension_enter_date_range.SetFocus()

end event

on w_pension_reports.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.dw_pension_enter_date_range=create dw_pension_enter_date_range
this.gb_select_criteria=create gb_select_criteria
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.dw_pension_enter_date_range
this.Control[iCurrent+3]=this.gb_select_criteria
end on

on w_pension_reports.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.dw_pension_enter_date_range)
destroy(this.gb_select_criteria)
end on

event resize;call super::resize;dw_report.width = This.width - 137
end event

type dw_report from w_a_report`dw_report within w_pension_reports
integer taborder = 40
boolean hscrollbar = true
end type

type cb_ok from commandbutton within w_pension_reports
integer x = 2153
integer y = 96
integer width = 389
integer height = 108
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;Long     ll_num_rows, ll_start_month, ll_start_day, ll_start_month_day
Long     ll_end_month, ll_end_day, ll_end_month_day, ll_temp
Long     ll_start_in_year
Datetime ldt_from_date, ldt_to_date, ldt_earliest_birth_date
Datetime ldt_birth_from_date, ldt_birth_to_date, ldt_latest_birth_date
String   ls_start_month_day, ls_end_month_day, ls_title, ls_month

SetPointer(HourGlass!)

dw_pension_enter_date_range.AcceptText()

// Validate input parameters
IF is_dw_object = "d_p81_ss_attaining_65_report" OR is_dw_object = "d_dependent_child_over_18_report" OR is_dw_object = "d_all_leg_ss_attaining_65_report" THEN
	// Validate From Date
	ldt_from_date = dw_pension_enter_date_range.GetItemDateTime(1, "from_date")
	IF IsNull(ldt_from_date) THEN
		MessageBox("Validation Error","The From Date must have a value.",Information!)
		dw_pension_enter_date_range.SetColumn("from_date")
		dw_pension_enter_date_range.SetFocus()
		RETURN
	END IF
	
	IF ldt_from_date < Datetime(Date("1900,01,01")) OR ldt_from_date > Datetime(Date(2079,06,06)) THEN
		MessageBox("Validation Error","Dates must be between 1900-01-01 and 2079-06-06.",Information!)
		dw_pension_enter_date_range.SetColumn("from_date")
		dw_pension_enter_date_range.SetFocus()
		RETURN
	END IF
	
	// Validate To Date
	ldt_to_date = dw_pension_enter_date_range.GetItemDateTime(1, "to_date")
	IF IsNull(ldt_to_date) THEN
		MessageBox("Validation Error","Both the To date must have a value.",Information!)
		dw_pension_enter_date_range.SetColumn("to_date")
		dw_pension_enter_date_range.SetFocus()
		RETURN
	END IF
	
	IF ldt_to_date < Datetime(Date(1900,01,01)) OR ldt_to_date > Datetime(Date(2079,06,06)) THEN
		MessageBox("Validation Error","Dates must be between 1900-01-01 and 2079-06-06.",Information!)
		dw_pension_enter_date_range.SetColumn("to_date")
		dw_pension_enter_date_range.SetFocus()
		RETURN
	END IF
	
	IF ldt_from_date >= ldt_to_date THEN
		MessageBox("Validation Error","The to date must be later than the from date",Information!)
		dw_pension_enter_date_range.SetColumn("to_date")
		dw_pension_enter_date_range.SetFocus()
		RETURN
	END IF
ELSE
	// Get the Anniversary Starting and Ending month/day
	ll_start_month = dw_pension_enter_date_range.GetItemNumber(1, "anniv_start_month")
	ll_start_day = dw_pension_enter_date_range.GetItemNumber(1, "anniv_start_day")
	ll_start_month_day = ll_start_month * 100 + ll_start_day
	ls_start_month_day = String(Date(String("2000/" + String(ll_start_month) + "/" + String(ll_start_day))), "mmmm d")
	IF IsDate("2000/" + String(ll_start_month) + "/" + String(ll_start_day)) = FALSE THEN
		ls_month = String(Date("2000/" + String(ll_start_month) + "/01"), "mmmm")
		MessageBox("Validation Error", "The Anniversary Start Date (" + ls_month + " " + String(ll_start_day) +&
					  ") is not valid.  Please re-enter.", Information!)
		dw_pension_enter_date_range.SetColumn("anniv_start_day")
		dw_pension_enter_date_range.SetFocus()
		RETURN
	END IF
	
	ll_end_month = dw_pension_enter_date_range.GetItemNumber(1, "anniv_end_month")
	ll_end_day = dw_pension_enter_date_range.GetItemNumber(1, "anniv_end_day")
	ll_end_month_day = ll_end_month * 100 + ll_end_day
	ls_end_month_day = String(Date(String("2000/" + String(ll_end_month) + "/" + String(ll_end_day))), "mmmm d")
	IF IsDate("2000/" + String(ll_end_month) + "/" + String(ll_end_day)) = FALSE THEN
		ls_month = String(Date("2000/" + String(ll_end_month) + "/01"), "mmmm")
		MessageBox("Validation Error", "The Anniversary End Date (" + ls_month + " " + String(ll_end_day) +&
					  ") is not valid.  Please re-enter.", Information!)
		dw_pension_enter_date_range.SetColumn("anniv_end_day")
		dw_pension_enter_date_range.SetFocus()
		RETURN
	END IF
	
	IF ll_start_month_day > ll_end_month_day THEN
		MessageBox("Validation Error", "The Anniversary End Date (" + ls_end_month_day + ") must be later than the " +&
					  "Anniversary Start Date (" + ls_start_month_day + ").", Information!) 
		dw_pension_enter_date_range.SetColumn("anniv_end_month")
		dw_pension_enter_date_range.SetFocus()
		RETURN
	END IF
	
	ll_start_in_year = dw_pension_enter_date_range.GetItemNumber(1, "start_year")
END IF

//	Retrieve the requested report. Each report requested may require different options, so...
CHOOSE CASE is_dw_object
	CASE "d_pension_anniversary_report"
		//	This option requires the parameters: 'earliest birth date' (to exclude pensioners
		//	more than 65 years old), 'from date', anniversary from month/day, 'anniversary to month/day'		
		ldt_from_date = DateTime(Date(String(ll_start_in_year) + "/" + String(ll_start_month) + "/" + String(ll_start_day)))
		
		//T012347  2016/02/18 - Pension Anniversary Report changed 66 to 65 in the SELECT below. David Worboys
		SELECT dateadd(year, -65, :ldt_from_date), last_batch_no  
		  INTO :ldt_earliest_birth_date, :ll_temp 
		  FROM Last_Batch_No ; 

		SQLCA.nf_handle_error("w_pension_reports","SELECT dateadd(year, -66, :ldt_from_date), last_batch_no","cb_ok")

		ls_title = "Pension's with accident anniversary between " + ls_start_month_day + " and " + ls_end_month_day +&
					  " starting in " + String(ll_start_in_year)
		ll_num_rows = dw_report.Retrieve(ldt_earliest_birth_date, ldt_from_date, ll_start_month_day, ll_end_month_day, ls_title)
		SQLCA.nf_handle_error("w_pension_reports","dw_report (d_pension_anniversary_report)","cb_ok")
		IF ll_num_rows <= 0 THEN
			MessageBox("Pension Anniversary Report","No data found to satisfy request.")
		END IF

	CASE "d_survivors_anniversary_report"
		ls_title = "Survivor Claims with death anniversary between " + ls_start_month_day + " and " + ls_end_month_day +&
					  " starting in " + String(ll_start_in_year)
		ldt_from_date = DateTime(Date(String(ll_start_in_year) + "/" + String(ll_start_month) + "/" + String(ll_start_day)))

		ll_num_rows = dw_report.Retrieve(ldt_from_date, ll_start_month_day, ll_end_month_day, ls_title)
		SQLCA.nf_handle_error("w_pension_reports","dw_report (d_survivors_anniversary_report)","cb_ok")
		IF ll_num_rows <= 0 THEN
			MessageBox("Survivors Anniversary Report","No data found to satisfy request.")
		END IF

	CASE "d_dependent_child_by_birthdate_report"
		ldt_from_date = DateTime(Date(String(ll_start_in_year) + "/" + String(ll_start_month) + "/" + String(ll_start_day)))
		ldt_to_date = DateTime(Date(String(ll_start_in_year) + "/" + String(ll_end_month) + "/" + String(ll_end_day)))
		ls_title = "From " + ls_start_month_day + " to " + ls_end_month_day + " starting in " + String(ll_start_in_year)
		ll_num_rows = dw_report.Retrieve(ldt_from_date, ll_start_month_day, ll_end_month_day, ls_title, ldt_to_date)
		SQLCA.nf_handle_error("w_pension_reports","dw_report (d_dependent_child_by_birthdate_report)","cb_ok")
		IF ll_num_rows <= 0 THEN
			MessageBox("Dependent Children by Birthdate Report","No data found to satisfy request.")
		END IF

	CASE "d_p81_ss_attaining_65_report"
		SELECT dateadd(year, -65, :ldt_from_date), dateadd(year, -65, :ldt_to_date), last_batch_no  
		  INTO :ldt_birth_from_date, :ldt_birth_to_date, :ll_temp 
		  FROM Last_Batch_No ; 

		SQLCA.nf_handle_error("w_pension_reports","SELECT dateadd(year, -65, :ldt_from_date), dateadd(year, -65, :ldt_to_date), last_batch_no","cb_ok")

		ll_num_rows = dw_report.Retrieve(ldt_from_date, ldt_to_date, ldt_birth_from_date, ldt_birth_to_date)
		SQLCA.nf_handle_error("w_pension_reports","dw_report (d_p81_ss_attaining_65_report)","cb_ok")
		IF ll_num_rows <= 0 THEN
			MessageBox("P81 Surviving Spouses Attaining 65 Report","No data found to satisfy request.")
		END IF
		

	CASE "d_all_leg_ss_attaining_65_report"
		SELECT dateadd(year, -65, :ldt_from_date), dateadd(year, -65, :ldt_to_date), last_batch_no
		  INTO :ldt_birth_from_date, :ldt_birth_to_date, :ll_temp
		  FROM Last_Batch_No ;

		SQLCA.nf_handle_error("w_pension_reports","SELECT dateadd(year, -65, :ldt_from_date), dateadd(year, -65, :ldt_to_date), last_batch_no","cb_ok")

		ll_num_rows = dw_report.Retrieve(ldt_from_date, ldt_to_date, ldt_birth_from_date, ldt_birth_to_date)
		SQLCA.nf_handle_error("w_pension_reports","dw_report (d_all_leg_ss_attaining_65_report)","cb_ok")
		IF ll_num_rows <= 0 THEN
			MessageBox("All Surviving Spouses Attaining 65 Report","No data found to satisfy request.")
		END IF


	CASE "d_dependent_child_over_18_report"
		SELECT dateadd(year, -18, :ldt_to_date), last_batch_no
		  INTO :ldt_latest_birth_date, :ll_temp
		  FROM Last_Batch_No ; 

		SQLCA.nf_handle_error("w_pension_reports","SELECT dateadd(year, -18, :ldt_to_date), last_batch_no","cb_ok")

		ll_num_rows = dw_report.Retrieve(ldt_from_date, ldt_to_date, ldt_latest_birth_date)
		SQLCA.nf_handle_error("w_pension_reports","dw_report (d_dependent_child_over_18_report)","cb_ok")
		IF ll_num_rows <= 0 THEN
			MessageBox("Dependent Children Over 18 Report","No data found to satisfy request.")
		END IF
END CHOOSE

SetPointer(Arrow!)

end event

type dw_pension_enter_date_range from u_dw_online within w_pension_reports
integer x = 119
integer y = 108
integer width = 1531
integer height = 308
integer taborder = 30
boolean bringtotop = true
string dataobject = "d_pension_enter_date_range"
boolean border = false
end type

type gb_select_criteria from groupbox within w_pension_reports
integer x = 50
integer y = 36
integer width = 1669
integer height = 420
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Selection Criteria"
end type

