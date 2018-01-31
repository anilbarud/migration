$PBExportHeader$w_ltd_payments.srw
$PBExportComments$LTD Payment Report main window
forward
global type w_ltd_payments from w_a_report
end type
type cb_ok from commandbutton within w_ltd_payments
end type
type dw_filter_dates from u_dw_online within w_ltd_payments
end type
type gb_1 from groupbox within w_ltd_payments
end type
end forward

global type w_ltd_payments from w_a_report
string title = "LTD Payments Report"
cb_ok cb_ok
dw_filter_dates dw_filter_dates
gb_1 gb_1
end type
global w_ltd_payments w_ltd_payments

on open;call w_a_report::open;/* ---------------------------------------------------------------------------------------------------- */
/* Declare Variables                                                                                    */
/* ---------------------------------------------------------------------------------------------------- */

Long     vll_result

/* ---------------------------------------------------------------------------------------------------- */
/* Database Connections                                                                                 */
/* ---------------------------------------------------------------------------------------------------- */

dw_report.settransobject (sqlca)

dw_filter_dates.InsertRow(0)
dw_filter_dates.SetColumn("from_date")


dw_report.uf_SetSelect(3)
end on

on w_ltd_payments.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.dw_filter_dates=create dw_filter_dates
this.gb_1=create gb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.dw_filter_dates
this.Control[iCurrent+3]=this.gb_1
end on

on w_ltd_payments.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.dw_filter_dates)
destroy(this.gb_1)
end on

type dw_report from w_a_report`dw_report within w_ltd_payments
integer y = 372
integer height = 2108
integer taborder = 40
string dataobject = "d_ltd_payments"
boolean hscrollbar = true
end type

type cb_ok from commandbutton within w_ltd_payments
integer x = 2249
integer y = 144
integer width = 434
integer height = 108
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;	LONG ll_numrows
	DATETIME	ldt_from_date,	ldt_to_date, ldt_current
	INTEGER li_month_number, li_month_days
	BOOLEAN lb_leap

	dw_filter_dates.AcceptText()

/* Acquire and validate the dates.
*/	
	ldt_from_date = dw_filter_dates.GetItemDateTime(1,"from_date")
	ldt_to_date   = DateTime(RelativeDate(Date(dw_filter_dates.GetItemDateTime(1,"to_date")), 1))
	ldt_current = f_server_datetime()

	IF IsNull(ldt_from_date) or isNull(ldt_to_date) THEN
		MessageBox("Validation Error","Dates must have a value!",Exclamation!)
		RETURN
	END IF
	
	IF ldt_from_date > ldt_to_date THEN
		MessageBox("Validation Error","The from date cannot be after the to date",Exclamation!)
		RETURN
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
		IF ldt_to_date > DateTime(RelativeDate ( Date(ldt_current) , 366 )) THEN
			MessageBox("Validation Error","To date cannot be more than a year in the future.",Exclamation!)
			Return
		END IF
	ELSE
		IF ldt_to_date > DateTime(RelativeDate ( Date(ldt_current) , 365 )) THEN
			MessageBox("Validation Error","To date cannot be more than a year in the future.",Exclamation!)
			Return
		END IF		
	END IF

/* Retrieve the report.
*/ 
	ll_numrows = dw_report.Retrieve(ldt_from_date,ldt_to_date)
	IF ll_numrows <= 0 THEN
		MessageBox("LTD Payment","No data found to satisfy request")
	END IF

end event

type dw_filter_dates from u_dw_online within w_ltd_payments
integer x = 128
integer y = 152
integer width = 1463
integer height = 80
integer taborder = 30
string dataobject = "d_enter_date_range"
boolean border = false
boolean livescroll = true
end type

type gb_1 from groupbox within w_ltd_payments
integer x = 73
integer y = 52
integer width = 1723
integer height = 268
integer taborder = 10
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Processing Period"
end type

