$PBExportHeader$w_gl_transfer_summary.srw
forward
global type w_gl_transfer_summary from w_a_report
end type
type cb_ok from commandbutton within w_gl_transfer_summary
end type
type dw_report_parameters from u_dw_online within w_gl_transfer_summary
end type
type cb_clear from commandbutton within w_gl_transfer_summary
end type
type cb_summary from commandbutton within w_gl_transfer_summary
end type
end forward

global type w_gl_transfer_summary from w_a_report
string title = "GL Transfer Summary Report"
cb_ok cb_ok
dw_report_parameters dw_report_parameters
cb_clear cb_clear
cb_summary cb_summary
end type
global w_gl_transfer_summary w_gl_transfer_summary

type variables

end variables

event open;call super::open;
LONG ll_result
DATAWINDOWCHILD ldwc_cheque_type

/*	Database Connections 
*/

		
dw_report_parameters.SetTransObject(SQLCA)
dw_report_parameters.insertrow(0)
dw_report_parameters.SetItem(1, 'report_type', 'D')

end event

on w_gl_transfer_summary.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.dw_report_parameters=create dw_report_parameters
this.cb_clear=create cb_clear
this.cb_summary=create cb_summary
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.dw_report_parameters
this.Control[iCurrent+3]=this.cb_clear
this.Control[iCurrent+4]=this.cb_summary
end on

on w_gl_transfer_summary.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.dw_report_parameters)
destroy(this.cb_clear)
destroy(this.cb_summary)
end on

type dw_report from w_a_report`dw_report within w_gl_transfer_summary
integer y = 460
integer height = 2076
integer taborder = 40
string dataobject = "d_gl_debit_account_tfer_summary"
boolean hscrollbar = true
end type

type cb_ok from commandbutton within w_gl_transfer_summary
integer x = 2359
integer y = 52
integer width = 293
integer height = 108
integer taborder = 10
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;/*	Initialization*/

LONG		ll_numrows
DATETIME ldtm_from_date, ldtm_to_date
INTEGER	li_rc
STRING	ls_report_type



dw_report_parameters.AcceptText()


	ldtm_from_date = dw_report_parameters.GetItemDateTime(1, 'from_date')
	ldtm_to_date = DateTime(RelativeDate(Date(dw_report_parameters.GetItemDateTime(1, 'to_date')), 1))

/* Validate date range. End date must be greater than the start date. As well check the SQL Server limits.
*/ 
	IF IsNull(ldtm_from_date) or &
		IsNull(ldtm_to_date) THEN
		MessageBox("Validation Error","Both the from and to dates must have a value.",Exclamation!)
		Return
	END IF

	IF ldtm_from_date < DateTime(Date(1900,01,01)) OR &
		ldtm_to_date < DateTime(Date(1900,01,01)) THEN
		MessageBox("Validation Error","Dates cannot be earlier than 1900-01-01!",Exclamation!)
		Return
	END IF

	IF ldtm_from_date < DateTime(Date(1900,01,01)) OR &
		ldtm_to_date < DateTime(Date(1900,01,01)) THEN
		MessageBox("Validation Error","Dates cannot be earlier than 1900-01-01!",Exclamation!)
		Return
	END IF

	IF ldtm_from_date > DateTime(Date(2079,06,06)) OR &
		ldtm_to_date > DateTime(Date(2079,06,06)) THEN
		MessageBox("Validation Error","Dates cannot be later than 2079-06-06!",Exclamation!)
		Return
	END IF
	
	IF ldtm_from_date >= ldtm_to_date then
		MessageBox("Validation Error","The to date must not be less than the from date",Exclamation!)
		Return
	END IF

	ls_report_type = dw_report_parameters.GetItemString(1, 'report_type')

	IF ls_report_type = 'C' THEN
		dw_report.DataObject = 'd_gl_credit_account_tfer_summary'
		dw_report.SetTransObject(SQLCA)
	ELSE
		dw_report.DataObject = 'd_gl_debit_account_tfer_summary'
		dw_report.SetTransObject(SQLCA)
	END IF
	
	cb_summary.Text = 'Summary'
	
	ll_numrows = dw_report.Retrieve(ldtm_from_date, ldtm_to_date)
	IF SQLCA.nf_handle_error("w_gl_transfer_summary","","cb_ok - dw_report.Retrieve(ldtm_from_date, ldtm_to_date)") < 0 THEN
		Return
	END IF
	IF ll_numrows <= 0 THEN
		MessageBox("GL Summary Transfer Report","No data found to satisfy request")
		return
	END IF

end event

type dw_report_parameters from u_dw_online within w_gl_transfer_summary
integer x = 78
integer y = 68
integer width = 1797
integer height = 364
integer taborder = 20
boolean bringtotop = true
string dataobject = "d_gl_transfer_dates"
boolean border = false
boolean livescroll = true
end type

type cb_clear from commandbutton within w_gl_transfer_summary
integer x = 2359
integer y = 200
integer width = 293
integer height = 108
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Clear"
end type

event clicked;dw_report_parameters.reset()
dw_report_parameters.insertrow(0)
dw_report_parameters.SetItem(1, 'report_type', 'D')
end event

type cb_summary from commandbutton within w_gl_transfer_summary
integer x = 2359
integer y = 344
integer width = 293
integer height = 108
integer taborder = 21
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Summary"
end type

event clicked;If this.Text = 'Summary' THEN
	dw_report.Modify("datawindow.detail.height = 0")
	dw_report.Modify("datawindow.header.height = 289")
	dw_report.Modify("datawindow.header.1.height = 0")
	this.Text = 'Detail'
Else
	dw_report.Modify("datawindow.detail.height = 89")
	dw_report.Modify("datawindow.header.height = 437")
	dw_report.Modify("datawindow.header.1.height = 77")
	this.text = 'Summary'
END IF
end event

