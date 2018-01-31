$PBExportHeader$w_cheque_reprint_audit_report.srw
forward
global type w_cheque_reprint_audit_report from w_a_report
end type
type cb_ok from commandbutton within w_cheque_reprint_audit_report
end type
type cb_clear from commandbutton within w_cheque_reprint_audit_report
end type
type dw_report_parameters from u_dw_online within w_cheque_reprint_audit_report
end type
end forward

global type w_cheque_reprint_audit_report from w_a_report
string title = "Cheque Reprint Audit Report"
cb_ok cb_ok
cb_clear cb_clear
dw_report_parameters dw_report_parameters
end type
global w_cheque_reprint_audit_report w_cheque_reprint_audit_report

type variables

end variables

event open;call super::open;
/*	Database Connections 
*/

dw_report.SetTransObject(SQLCA)

dw_report_parameters.SetTransObject(SQLCA)
dw_report_parameters.InsertRow(0)
end event

on w_cheque_reprint_audit_report.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.cb_clear=create cb_clear
this.dw_report_parameters=create dw_report_parameters
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.cb_clear
this.Control[iCurrent+3]=this.dw_report_parameters
end on

on w_cheque_reprint_audit_report.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.cb_clear)
destroy(this.dw_report_parameters)
end on

type dw_report from w_a_report`dw_report within w_cheque_reprint_audit_report
integer y = 376
integer height = 2160
integer taborder = 40
string dataobject = "d_cheque_reprint_audit_report"
boolean hscrollbar = true
end type

type cb_ok from commandbutton within w_cheque_reprint_audit_report
integer x = 2359
integer y = 56
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
DATETIME adtm_processed_date 
INTEGER	li_rc

dw_report_parameters.AcceptText()


	adtm_processed_date = dw_report_parameters.GetItemDateTime(1, 'cheque_reprint_date')
	ll_numrows = dw_report.Retrieve(adtm_processed_date)
	IF SQLCA.nf_handle_error("w_cheque_reprint_audit_report","","cb_ok - dw_report.Retrieve(adtm_processed_date)") < 0 THEN
		Return
	END IF
	IF ll_numrows <= 0 THEN
		MessageBox("Cheque Reprint Audit Report","No data found to satisfy request")
		return
	END IF

end event

type cb_clear from commandbutton within w_cheque_reprint_audit_report
integer x = 2359
integer y = 212
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

event clicked;dw_report_parameters.Reset()
dw_report_parameters.InsertRow(0)
end event

type dw_report_parameters from u_dw_online within w_cheque_reprint_audit_report
integer x = 78
integer y = 32
integer width = 1797
integer height = 308
integer taborder = 20
boolean bringtotop = true
string dataobject = "d_cheque_reprint_dates"
boolean border = false
boolean livescroll = true
end type

