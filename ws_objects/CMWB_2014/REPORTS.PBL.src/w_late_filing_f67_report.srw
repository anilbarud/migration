$PBExportHeader$w_late_filing_f67_report.srw
$PBExportComments$Report on employers with history of late filing of F67 reports
forward
global type w_late_filing_f67_report from w_a_report
end type
type cb_ok from commandbutton within w_late_filing_f67_report
end type
type dw_enter_date_range from u_dw_online within w_late_filing_f67_report
end type
type gb_select_criteria from groupbox within w_late_filing_f67_report
end type
type dw_no_days_overdue from u_dw_online within w_late_filing_f67_report
end type
end forward

global type w_late_filing_f67_report from w_a_report
string title = "Employers Late Filing F67 Report"
cb_ok cb_ok
dw_enter_date_range dw_enter_date_range
gb_select_criteria gb_select_criteria
dw_no_days_overdue dw_no_days_overdue
end type
global w_late_filing_f67_report w_late_filing_f67_report

type variables

end variables

event open;call super::open;LONG			ll_result

/* Set up the date range dw to accept the dates.
*/
	dw_enter_date_range.InsertRow(0)
	
/*	Set up the no. days dw to accept user-entered no. of days overdue
*/
	dw_no_days_overdue.InsertRow(0)

/*	Set up the report datawindow.
*/
	dw_report.SetTransObject(SQLCA)

end event

on w_late_filing_f67_report.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.dw_enter_date_range=create dw_enter_date_range
this.gb_select_criteria=create gb_select_criteria
this.dw_no_days_overdue=create dw_no_days_overdue
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.dw_enter_date_range
this.Control[iCurrent+3]=this.gb_select_criteria
this.Control[iCurrent+4]=this.dw_no_days_overdue
end on

on w_late_filing_f67_report.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.dw_enter_date_range)
destroy(this.gb_select_criteria)
destroy(this.dw_no_days_overdue)
end on

type dw_report from w_a_report`dw_report within w_late_filing_f67_report
integer taborder = 40
string dataobject = "d_late_filing_f67_report"
boolean hscrollbar = true
end type

type cb_ok from commandbutton within w_late_filing_f67_report
integer x = 2286
integer y = 212
integer width = 389
integer height = 108
integer taborder = 30
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
	LONG 		ll_numrows, al_no_days_overdue, ll_return
	DATETIME	adt_from_date, adt_to_date

	SetPointer(HourGlass!)
	
	ll_return = MessageBox("Information", "This report may take several minutes to process. " + &
	+ "Do you still want to run the report?", Information!, YesNo!, 2)
	IF ll_return = 1 THEN
		dw_no_days_overdue.AcceptText()
		dw_enter_date_range.AcceptText()

	/*	Assign the data values to the variables
	*/
		al_no_days_overdue = dw_no_days_overdue.GetItemNumber(1,'no_days_overdue')
		adt_from_date = dw_enter_date_range.GetItemDateTime(1,'from_date')
		adt_to_date = dw_enter_date_range.GetItemDateTime(1,'to_date')
		
	IF isnull(al_no_days_overdue) THEN al_no_days_overdue = 0
		
	/*	Validate no. days - must be a positive number
	*/
		IF al_no_days_overdue <=0 THEN
			MessageBox("Invalid Number of Days", "The number of days overdue must be greater than 0.")
			dw_no_days_overdue.SetFocus()
			Return
		END IF

	/* Validate date range. 'To' date must be greater than the 'from' date. As well, add one to the 'to' date
		to ensure getting all items created that day, regardless of what time they were created.
	*/ 
		IF IsNull(adt_from_date) OR IsNull(adt_to_date) THEN
			MessageBox("Validation Error","Both the from and to dates must have a value.",Exclamation!)
			Return
		END IF

		IF adt_from_date < DateTime(Date(1900,01,01)) OR &
			adt_to_date < DateTime(Date(1900,01,01)) THEN
			MessageBox("Validation Error","Dates cannot be earlier than 1900-01-01!",Exclamation!)
			Return
		END IF

		IF adt_from_date > DateTime(Date(2079,06,06)) OR &
			adt_to_date > DateTime(Date(2079,06,06)) THEN
			MessageBox("Validation Error","Dates cannot be later than 2079-06-06!",Exclamation!)
			Return
		END IF

		IF adt_from_date >= adt_to_date then
			MessageBox("Validation Error","The 'to' date must be later than the 'from' date",Exclamation!)
			Return
		END IF

	/*	Retrieve the report. It requires the minimum number of days overdue and the from and to dates for parameters
	*/
		ll_numrows = dw_report.Retrieve(al_no_days_overdue, adt_from_date, adt_to_date)
		SQLCA.nf_handle_error("w_late_filing_f67_report","dw_report","cb_ok")
		IF ll_numrows = 0 THEN
			MessageBox("Completed Awards Report","No data found to satisfy request.")
		ELSE
			IF ll_numrows < 0 THEN
				MessageBox("Retrieval Error", "Error retrieving information for report.")
				Return -1
			END IF
		END IF
	ELSE
		Return -1
	END IF
	
	SetPointer(Arrow!)

end event

type dw_enter_date_range from u_dw_online within w_late_filing_f67_report
integer x = 119
integer y = 324
integer width = 1289
integer height = 96
integer taborder = 20
boolean bringtotop = true
string dataobject = "d_enter_date_range"
boolean border = false
end type

type gb_select_criteria from groupbox within w_late_filing_f67_report
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
string text = "Selection Criteria"
end type

type dw_no_days_overdue from u_dw_online within w_late_filing_f67_report
integer x = 105
integer y = 176
integer width = 1015
integer height = 92
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_no_days_overdue"
boolean border = false
end type

