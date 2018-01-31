$PBExportHeader$w_invoice_reconciliation_report.srw
forward
global type w_invoice_reconciliation_report from w_a_report
end type
type cb_retrieve from commandbutton within w_invoice_reconciliation_report
end type
type em_from_date from editmask within w_invoice_reconciliation_report
end type
type em_to_date from editmask within w_invoice_reconciliation_report
end type
type cb_fit_to_window from commandbutton within w_invoice_reconciliation_report
end type
type st_1 from statictext within w_invoice_reconciliation_report
end type
type st_2 from statictext within w_invoice_reconciliation_report
end type
type gb_search_criteria from groupbox within w_invoice_reconciliation_report
end type
end forward

global type w_invoice_reconciliation_report from w_a_report
string title = "Invoice Reconciliation Report"
cb_retrieve cb_retrieve
em_from_date em_from_date
em_to_date em_to_date
cb_fit_to_window cb_fit_to_window
st_1 st_1
st_2 st_2
gb_search_criteria gb_search_criteria
end type
global w_invoice_reconciliation_report w_invoice_reconciliation_report

type variables
n_resize inv_resize
end variables

on w_invoice_reconciliation_report.create
int iCurrent
call super::create
this.cb_retrieve=create cb_retrieve
this.em_from_date=create em_from_date
this.em_to_date=create em_to_date
this.cb_fit_to_window=create cb_fit_to_window
this.st_1=create st_1
this.st_2=create st_2
this.gb_search_criteria=create gb_search_criteria
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_retrieve
this.Control[iCurrent+2]=this.em_from_date
this.Control[iCurrent+3]=this.em_to_date
this.Control[iCurrent+4]=this.cb_fit_to_window
this.Control[iCurrent+5]=this.st_1
this.Control[iCurrent+6]=this.st_2
this.Control[iCurrent+7]=this.gb_search_criteria
end on

on w_invoice_reconciliation_report.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_retrieve)
destroy(this.em_from_date)
destroy(this.em_to_date)
destroy(this.cb_fit_to_window)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.gb_search_criteria)
end on

event open;call super::open;Integer li_rtn, li_day
date ltd_date

li_rtn = dw_report.SetTransObject(SQLCA)

// get todays 'day'
li_day = Day(today())

// back up to last day of previous month and set the 'to_date' edit field
ltd_date = RelativeDate(today(), -li_day)
em_to_date.Text = String(ltd_date,"yyyy-mm-dd")

// set the 'from_date' field to the first of the month
em_from_date.text = String(date(year(ltd_date),month(ltd_date), 1),"yyyy-mm-dd")



end event

event resize;call super::resize;dw_report.width = This.width - 100
dw_report.height = This.height - 400
end event

type dw_report from w_a_report`dw_report within w_invoice_reconciliation_report
integer y = 312
integer height = 2168
integer taborder = 40
string dataobject = "d_composite_abcc_invoice"
boolean hscrollbar = true
boolean resizable = true
end type

type cb_retrieve from commandbutton within w_invoice_reconciliation_report
integer x = 1339
integer y = 96
integer width = 416
integer height = 100
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Retrieve"
end type

event clicked;Long       ll_rows, ll_tax_rows, ll_cntr, ll_count, ll_tax_record, ll_num_rows, ll_year_to_date_trans_count, ll_num_totals
Integer    li_year, li_rtn 
String     ls_express, ls_sql, ls_rtn, ls_year_to_date
Decimal{2} ldec_tax_rate
Date       ldt_from, ldt_to, ldt_begin_year, ldt_tax_date, ldt_current_tax_date, ldt_next_tax_date
Datastore  lds_admin_fee_schedule, lds_tax_rate
DataWindowChild ldwc_report, ldwc_totals

SetPointer(HourGlass!) 
dw_report.Reset()

// Validate Report Arguments
ldt_from = Date(em_from_date.Text)
ldt_to = Date(em_to_date.Text)

IF IsDate(em_from_date.Text) = FALSE OR IsDate(em_to_date.Text) = FALSE THEN 
	MessageBox("Invalid Criteria", "You must enter a valid date for both 'To date' and the 'From date'", Exclamation!)
	em_from_date.SetFocus( )
	RETURN
END IF

IF ldt_to < ldt_from THEN
	MessageBox("Invalid Criteria", "You must enter a 'To date' that is greater than the 'From date'", Exclamation!)
	em_to_date.SetFocus( )
	RETURN
END IF

IF ldt_from > Date(f_server_datetime())THEN
	MessageBox("Invalid Criteria", "The 'From date' must not be in the future", Exclamation!)
	em_from_date.SetFocus( )
	RETURN
END IF

IF Year(ldt_from) <> Year(ldt_to) THEN
	MessageBox("Invalid Criteria", "The 'Year' value must be the same for both dates", Exclamation!)
	em_from_date.SetFocus( )
	RETURN
END IF

li_year = Year(ldt_from)
ldt_begin_year = Date(String(li_year)+'-01-01')

// Get the tax rate(s) 
lds_tax_rate = CREATE DATASTORE
lds_tax_rate.DataObject = "d_tax_rates"
lds_tax_rate.SetTransObject(SQLCA)
ll_tax_rows = lds_tax_rate.Retrieve()
li_rtn = SQLCA.nf_handle_error("w_invoice_reconciliation_report", "", "cb_retrieve - lds_tax_rate.Retrieve()")

// Make sure there is Admin Fees for the year 
lds_admin_fee_schedule = CREATE DATASTORE 
lds_admin_fee_schedule.dataobject = "d_abcc_admin_fee"
lds_admin_fee_schedule.SetTransObject(SQLCA) 
ll_rows = lds_admin_fee_schedule.Retrieve(li_year) 
li_rtn = SQLCA.nf_handle_error("w_invoice_reconciliation_report", "", "cb_retrieve - lds_admin_fee_schedule.retrieve(li_year)")

IF ll_rows = 0 THEN
	MessageBox("Insufficient Data", "Unable to retrieve the proper Admin fee schedule data for " + String(li_year) + " from the Abcc_Admin_Fee_Schedule table. Call the help desk.")
	RETURN
END IF

// PR5988 - J. Hawker, 2006.08.17 - Check to see how many tax periods this date range covers. If it is more than one, do not allow. 
//             They must only retrieve the report for one tax period at a time due to the possibility of different tax rates.
FOR ll_cntr = 1 to ll_tax_rows
	ldt_tax_date = Date(lds_tax_rate.GetItemDateTime(ll_cntr,"effective_date"))
	IF ll_tax_rows > ll_cntr THEN
		IF ldt_from >= ldt_tax_date AND ldt_from < Date(lds_tax_rate.GetItemDateTime(ll_cntr + 1, "effective_date")) THEN
			IF ldt_to < Date(lds_tax_rate.GetItemDateTime(ll_cntr + 1, "effective_date")) THEN
				ldt_current_tax_date = ldt_tax_date
				ll_count = 1
				EXIT
			ELSE
				ldt_next_tax_date = Date(lds_tax_rate.GetItemDateTime(ll_cntr + 1, "effective_date"))
				ll_count = ll_count + 1
			END IF
		END IF
	ELSE
		ldt_current_tax_date = ldt_tax_date
		IF ldt_from >= ldt_tax_date AND ldt_to >= ldt_tax_date THEN
			ll_count = ll_count + 1
		END IF
	END IF
	IF ldt_from < ldt_tax_date AND ldt_to >= ldt_tax_date THEN
		ll_count = ll_count + 1
	END IF				
NEXT

IF ll_count > 1 THEN
	Messagebox('Invalid Date Range','The date range you have entered expands over multiple tax periods. ' + &
							 '~r~nYou must retrieve for one period only. A new tax period starts  ' + String(ldt_next_tax_date, 'yyyy-mm-dd'), Information!)
	RETURN
ELSEIF ll_count = 1 THEN
	ls_express = "effective_date=" + String(ldt_current_tax_date, 'yyyy-mm-dd')
	ll_tax_record  = lds_tax_rate.Find(ls_express, 1,lds_tax_rate.RowCount()) 
	ldec_tax_rate = lds_tax_rate.GetItemNumber(ll_tax_record, "tax_rate")
END IF

// 
ls_sql =	"CREATE TABLE #abcc_report (rownum int IDENTITY(1,1), plan_pays money not null, adjudication_date datetime not null, transaction_code char(2) not null, " +&
         "                           week_no int not null, effective_date datetime null, effective_to_date datetime null, admin_fee money null, tax_rate money null ) " 
EXECUTE IMMEDIATE :ls_sql ;
li_rtn = SQLCA.nf_handle_error("w_invoice_reconciliation_report", "", "cb_retrieve - CREATE TABLE #abcc_report") 


li_rtn = SQLCA.nf_handle_error("w_invoice_reconciliation_report", "", "cb_retrieve - COMMIT USING SQLCA ") 

// Load Temp table
INSERT #abcc_report (plan_pays, adjudication_date, transaction_code, week_no, effective_date, effective_to_date, admin_fee, tax_rate) 
SELECT plan_pays, adjudication_date, transaction_code, 
       CASE WHEN datepart(dw,adjudication_date)>= 5 THEN datepart(wk,adjudication_date) + 1 ELSE datepart(wk,adjudication_date) END week, 
       NULL, NULL, 0.00, :ldec_tax_rate 
  FROM PAYMENT_PRESCRIPTION 
 WHERE adjudication_date >= :ldt_begin_year 
   AND adjudication_date <= :ldt_to 
 ORDER BY adjudication_date ASC ; 

li_rtn = SQLCA.nf_handle_error("w_invoice_reconciliation_report", "", "cb_retrieve - INSERT #abcc_report ") 

// Set effective_date and admin_fee
UPDATE #abcc_report  
   SET #abcc_report.effective_date = AAFS.effective_date,  
       #abcc_report.admin_fee = AAFS.admin_fee 
  FROM Abcc_Admin_Fee_Schedule AAFS  
 WHERE AAFS.effective_date = (SELECT MAX(Abcc_Admin_Fee_Schedule.effective_date) 
                                FROM Abcc_Admin_Fee_Schedule 
                               WHERE Abcc_Admin_Fee_Schedule.effective_date <= #abcc_report.adjudication_date)  
   AND #abcc_report.rownum > AAFS.exceeding_level 
   AND #abcc_report.rownum <= AAFS.not_exceeding_level ; 

li_rtn = SQLCA.nf_handle_error("w_invoice_reconciliation_report", "", "cb_retrieve - UPDATE #abcc_report SET effective_date and admin_fee ") 

UPDATE #abcc_report 
   SET effective_date = :ldt_from 
 WHERE effective_date < :ldt_from ; 

li_rtn = SQLCA.nf_handle_error("w_invoice_reconciliation_report", "", "cb_retrieve - UPDATE #abcc_report SET effective_date = :ldt_from ") 

// Set effective_to_date 
UPDATE #abcc_report  
   SET #abcc_report.effective_to_date = DATEADD(day, -1, AAFS.effective_date)  
  FROM Abcc_Admin_Fee_Schedule AAFS  
 WHERE AAFS.effective_date = (SELECT MIN(Abcc_Admin_Fee_Schedule.effective_date) 
                                FROM Abcc_Admin_Fee_Schedule 
                               WHERE Abcc_Admin_Fee_Schedule.effective_date > #abcc_report.adjudication_date)  
   AND #abcc_report.rownum > AAFS.exceeding_level 
   AND #abcc_report.rownum <= AAFS.not_exceeding_level ; 

li_rtn = SQLCA.nf_handle_error("w_invoice_reconciliation_report", "", "cb_retrieve - UPDATE #abcc_report SET effective_to_date ") 

UPDATE #abcc_report 
   SET effective_to_date = :ldt_to 
 WHERE effective_to_date IS NULL ; 

li_rtn = SQLCA.nf_handle_error("w_invoice_reconciliation_report", "", "cb_retrieve - UPDATE #abcc_report SET effective_to_date = :ldt_to WHERE effective_to_date IS NULL ") 

// Count Year to date records 
SELECT COUNT(*) 
  INTO :ll_year_to_date_trans_count 
  FROM #abcc_report ; 

li_rtn = SQLCA.nf_handle_error("w_invoice_reconciliation_report", "", "cb_retrieve - INSERT #abcc_report ") 

IF IsNull(ll_year_to_date_trans_count) = TRUE THEN 
	ll_year_to_date_trans_count = 0 
END IF 

// Get top datawindow in the composite datawindow and change SQL and then retrieve
li_rtn = dw_report.GetChild("dw_1", ldwc_report)
IF li_rtn = -1 THEN
	Messagebox("An Error has occurred", "Error getting dw_1 datawindowchild for dw_report.  Phone the helpdesk.")
	RETURN
END IF

ls_sql =	"SELECT plan_pays, adjudication_date, transaction_code, week_no, " + String(ll_year_to_date_trans_count) + " " +& 
         "  FROM #abcc_report " +& 
         " WHERE adjudication_date >= ~"" + String(ldt_from, "yyyy-mm-dd") + "~" " +& 
         "   AND adjudication_date <= ~"" + String(ldt_to, "yyyy-mm-dd") + "~" " +& 
         " ORDER BY adjudication_date " 
ls_rtn = ldwc_report.Modify("DataWindow.Table.Select='" + ls_sql + "'")
li_rtn = ldwc_report.SetTransObject(SQLCA)
ll_num_rows = ldwc_report.Retrieve()
li_rtn = SQLCA.nf_handle_error("w_invoice_reconciliation_report", "", "cb_retrieve - ldwc_report.Retrieve()")

ls_rtn = ldwc_report.Modify("t_title.Text = 'To reconcile invoice for Claim Administration Fee for the period of " + String(ldt_from, "mmmm d, yyyy") + " to " + String(ldt_to, "mmmm d, yyyy") + "'")

// Get bottom / totals datawindow in the composite datawindow and change SQL and then retrieve
li_rtn = dw_report.GetChild("dw_invoice_reconciliation_totals", ldwc_totals)
IF li_rtn = -1 THEN
	Messagebox("An Error has occurred", "Error getting dw_invoice_reconciliation_totals datawindowchild for dw_report.  Phone the helpdesk.")
	RETURN
END IF

ls_sql =	"SELECT effective_date, effective_to_date, COUNT(*), admin_fee, tax_rate, " + String(ll_year_to_date_trans_count) +& 
         "  FROM #abcc_report " +& 
         " WHERE adjudication_date >= ~"" + String(ldt_from, "yyyy-mm-dd") + "~" " +& 
         "   AND adjudication_date <= ~"" + String(ldt_to, "yyyy-mm-dd") + "~" " +& 
         " GROUP BY effective_date, effective_to_date, admin_fee, tax_rate " +& 
         " ORDER BY effective_date, effective_to_date, admin_fee DESC, tax_rate " 
ls_rtn = ldwc_totals.Modify("DataWindow.Table.Select='" + ls_sql + "'")
li_rtn = ldwc_totals.SetTransObject(SQLCA)
ll_num_totals = ldwc_totals.Retrieve()
li_rtn = SQLCA.nf_handle_error("w_invoice_reconciliation_report", "", "cb_retrieve - ldwc_totals.Retrieve()")

ls_rtn = ldwc_totals.Modify("t_year_to_date_total.Text = '" + String(Year(ldt_begin_year)) + " Year to date volume up to " + String(ldt_to, "mmm d, yyyy") + ": " + String(ll_year_to_date_trans_count, "#,###,##0") + "'")

// Drop temp table
ls_sql =	"DROP TABLE #abcc_report "
EXECUTE IMMEDIATE :ls_sql ;
li_rtn = SQLCA.nf_handle_error("w_invoice_reconciliation_report", "", "cb_retrieve - DROP TABLE #abcc_report") 


IF ll_num_rows <= 0 THEN
	Messagebox("No Data Found", "There was no data that matches your search criteria.", Information!)
END IF

end event

type em_from_date from editmask within w_invoice_reconciliation_report
integer x = 137
integer y = 156
integer width = 421
integer height = 80
integer taborder = 10
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "0000-00-00"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "yyyy-mm-dd"
boolean dropdowncalendar = true
end type

type em_to_date from editmask within w_invoice_reconciliation_report
integer x = 667
integer y = 156
integer width = 421
integer height = 80
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "0000-00-00"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "yyyy-mm-dd"
boolean dropdowncalendar = true
end type

type cb_fit_to_window from commandbutton within w_invoice_reconciliation_report
integer x = 1915
integer y = 96
integer width = 416
integer height = 100
integer taborder = 50
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Fit to Window"
end type

event clicked;IF THIS.text = 'Fit to Window' THEN
	dw_report.Object.DataWindow.Zoom = 75
	THIS.text = 'Full Size'
ELSE
	dw_report.Object.DataWindow.Zoom = 100
	THIS.text = 'Fit to Window'
END IF
end event

type st_1 from statictext within w_invoice_reconciliation_report
integer x = 142
integer y = 100
integer width = 402
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "From date"
boolean focusrectangle = false
end type

type st_2 from statictext within w_invoice_reconciliation_report
integer x = 663
integer y = 92
integer width = 402
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "To date"
boolean focusrectangle = false
end type

type gb_search_criteria from groupbox within w_invoice_reconciliation_report
integer x = 82
integer y = 28
integer width = 1097
integer height = 236
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Search criteria"
end type

