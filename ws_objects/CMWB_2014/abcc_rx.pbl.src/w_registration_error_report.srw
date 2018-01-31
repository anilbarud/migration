$PBExportHeader$w_registration_error_report.srw
forward
global type w_registration_error_report from window
end type
type mle_1 from multilineedit within w_registration_error_report
end type
type uo_filter from u_filter_control within w_registration_error_report
end type
type cb_print from commandbutton within w_registration_error_report
end type
type cb_close from commandbutton within w_registration_error_report
end type
type dw_report from u_dw_online within w_registration_error_report
end type
end forward

global type w_registration_error_report from window
integer width = 2802
integer height = 2808
boolean titlebar = true
string title = "ABCC Export - Error Report"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 67108864
string icon = "AppIcon!"
mle_1 mle_1
uo_filter uo_filter
cb_print cb_print
cb_close cb_close
dw_report dw_report
end type
global w_registration_error_report w_registration_error_report

event open;INTEGER		li_rtn

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


dw_report.SetTransObject(SQLCA)
li_rtn = dw_report.Retrieve()
IF li_rtn < 0 Then
	SignalError(-666,'Error retrieving report')
Elseif li_rtn = 0 Then
	MessageBox('No rows','No claims with errors to report.')
	cb_print.enabled = False
End if

dw_report.uf_SetFilter(True)
uo_filter.uf_set_Requestor(dw_report)
end event

on w_registration_error_report.create
this.mle_1=create mle_1
this.uo_filter=create uo_filter
this.cb_print=create cb_print
this.cb_close=create cb_close
this.dw_report=create dw_report
this.Control[]={this.mle_1,&
this.uo_filter,&
this.cb_print,&
this.cb_close,&
this.dw_report}
end on

on w_registration_error_report.destroy
destroy(this.mle_1)
destroy(this.uo_filter)
destroy(this.cb_print)
destroy(this.cb_close)
destroy(this.dw_report)
end on

type mle_1 from multilineedit within w_registration_error_report
integer x = 667
integer y = 2536
integer width = 1298
integer height = 148
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean autovscroll = true
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type uo_filter from u_filter_control within w_registration_error_report
integer x = 50
integer y = 2560
integer taborder = 20
end type

on uo_filter.destroy
call u_filter_control::destroy
end on

event ue_filter_changed;call super::ue_filter_changed;mle_1.text = ls_new_filter
end event

type cb_print from commandbutton within w_registration_error_report
integer x = 1993
integer y = 2580
integer width = 297
integer height = 92
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Print"
end type

event clicked;IF dw_report.RowCount() > 0 Then
	dw_report.Print()
else
	messagebox('No rows','There are no errors to print.')
End if
end event

type cb_close from commandbutton within w_registration_error_report
integer x = 2318
integer y = 2580
integer width = 297
integer height = 92
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;Close(parent)
end event

type dw_report from u_dw_online within w_registration_error_report
integer y = 12
integer width = 2697
integer height = 2512
integer taborder = 10
string dataobject = "d_registration_error_report"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

