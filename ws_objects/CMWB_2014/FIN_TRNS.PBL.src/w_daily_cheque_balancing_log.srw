$PBExportHeader$w_daily_cheque_balancing_log.srw
forward
global type w_daily_cheque_balancing_log from w_a_report
end type
type cb_ok from commandbutton within w_daily_cheque_balancing_log
end type
type dw_report_parameters from u_dw_online within w_daily_cheque_balancing_log
end type
type cb_clear from commandbutton within w_daily_cheque_balancing_log
end type
end forward

global type w_daily_cheque_balancing_log from w_a_report
string title = "Daily Cheque Balancing Log"
cb_ok cb_ok
dw_report_parameters dw_report_parameters
cb_clear cb_clear
end type
global w_daily_cheque_balancing_log w_daily_cheque_balancing_log

type variables

end variables

event open;call super::open;
LONG ll_result
DATAWINDOWCHILD ldwc_cheque_type

/*	Database Connections 
*/

dw_report.SetTransObject(SQLCA)

			
dw_report_parameters.SetTransObject(SQLCA)
dw_report_parameters.insertrow(0)

end event

on w_daily_cheque_balancing_log.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.dw_report_parameters=create dw_report_parameters
this.cb_clear=create cb_clear
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.dw_report_parameters
this.Control[iCurrent+3]=this.cb_clear
end on

on w_daily_cheque_balancing_log.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.dw_report_parameters)
destroy(this.cb_clear)
end on

type dw_report from w_a_report`dw_report within w_daily_cheque_balancing_log
integer y = 460
integer height = 2076
integer taborder = 40
string dataobject = "d_daily_cheque_balancing_log"
boolean hscrollbar = true
end type

type cb_ok from commandbutton within w_daily_cheque_balancing_log
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


	adtm_processed_date = dw_report_parameters.GetItemDateTime(1, 'cbh_processed_date')
	ll_numrows = dw_report.Retrieve(adtm_processed_date)
	IF SQLCA.nf_handle_error("w_daily_cheque_balancing_log","","cb_ok - dw_report.Retrieve(adtm_processed_date)") < 0 THEN
		Return
	END IF
	IF ll_numrows <= 0 THEN
		MessageBox("Daily Cheque Balancing Log","No data found to satisfy request")
		return
	END IF

end event

type dw_report_parameters from u_dw_online within w_daily_cheque_balancing_log
integer x = 78
integer y = 68
integer width = 1797
integer height = 364
integer taborder = 20
boolean bringtotop = true
string dataobject = "d_cheque_batch_history_dates"
boolean border = false
boolean livescroll = true
end type

type cb_clear from commandbutton within w_daily_cheque_balancing_log
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

event clicked;dw_report_parameters.reset()
dw_report_parameters.insertrow(0)
end event

