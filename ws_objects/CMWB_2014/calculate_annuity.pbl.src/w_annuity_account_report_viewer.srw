$PBExportHeader$w_annuity_account_report_viewer.srw
$PBExportComments$A response window showing the formulary history
forward
global type w_annuity_account_report_viewer from window
end type
type cb_export from commandbutton within w_annuity_account_report_viewer
end type
type cb_print from commandbutton within w_annuity_account_report_viewer
end type
type dw_report from u_dw_online within w_annuity_account_report_viewer
end type
type cb_1 from commandbutton within w_annuity_account_report_viewer
end type
end forward

global type w_annuity_account_report_viewer from window
integer width = 3429
integer height = 2436
boolean titlebar = true
string title = "Annuity Calculation Report"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
windowtype windowtype = popup!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
cb_export cb_export
cb_print cb_print
dw_report dw_report
cb_1 cb_1
end type
global w_annuity_account_report_viewer w_annuity_account_report_viewer

on w_annuity_account_report_viewer.create
this.cb_export=create cb_export
this.cb_print=create cb_print
this.dw_report=create dw_report
this.cb_1=create cb_1
this.Control[]={this.cb_export,&
this.cb_print,&
this.dw_report,&
this.cb_1}
end on

on w_annuity_account_report_viewer.destroy
destroy(this.cb_export)
destroy(this.cb_print)
destroy(this.dw_report)
destroy(this.cb_1)
end on

event open;LONG 							ll_calc_no, ll_account_no
datastore					lds_check
s_window_message 		lstr_message

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


//populate the local structure
lstr_message = Message.powerobjectparm

ll_calc_no 		= lstr_message.al_doubleparm[1]
ll_account_no 	= lstr_message.al_doubleparm[2]

// retrieve the datawindow
dw_report.SetTransObject(SQLCA)
dw_report.retrieve(ll_calc_no, ll_account_no)

sqlca.nf_handle_error("w_annuity_account_report_viewer","open","dw_report.retrieve(ll_calc_no, ll_account_no)")

/* if you dont do this eligibility will not show up correctly */
dw_report.Object.DataWindow.Print.preview = "YES"








end event

event resize;dw_report.width = This.width - 90
dw_report.height = This.height - 280
end event

type cb_export from commandbutton within w_annuity_account_report_viewer
boolean visible = false
integer x = 544
integer y = 12
integer width = 265
integer height = 84
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Export"
end type

event clicked;string ls_richtext

//ls_richtext = dw_report.CopyRTF(true)
//ll_numchars = rte_report.text = ls_richtext

//messagebox("",ls_richtext)
dw_report.saveas()


//dw_report.saveas("",EXCEL!,TRUE)
//dw_report.saveas("",EXCEL!,FALSE)
//dw_report.saveas("",TEXT!,TRUE)
//dw_report.saveas("",TEXT!,FALSE)

/* none of these work */
end event

type cb_print from commandbutton within w_annuity_account_report_viewer
integer x = 27
integer y = 12
integer width = 256
integer height = 84
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Print"
end type

event clicked;PrintSetup( )

dw_report.Print()

end event

type dw_report from u_dw_online within w_annuity_account_report_viewer
integer x = 14
integer y = 116
integer width = 3337
integer height = 2104
integer taborder = 10
string dataobject = "d_annuity_account_header_comp"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean border = false
boolean livescroll = false
end type

type cb_1 from commandbutton within w_annuity_account_report_viewer
integer x = 288
integer y = 12
integer width = 256
integer height = 84
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;CLOSE(PARENT)
end event

