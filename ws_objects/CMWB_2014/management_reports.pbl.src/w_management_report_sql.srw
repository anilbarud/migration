$PBExportHeader$w_management_report_sql.srw
$PBExportComments$Used to give basic information for authorizations
forward
global type w_management_report_sql from window
end type
type mle_sql from multilineedit within w_management_report_sql
end type
type cb_close from commandbutton within w_management_report_sql
end type
end forward

global type w_management_report_sql from window
integer x = 2002
integer y = 1000
integer width = 3237
integer height = 1556
boolean titlebar = true
string title = "Management Report Sql"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean clientedge = true
mle_sql mle_sql
cb_close cb_close
end type
global w_management_report_sql w_management_report_sql

type variables
s_window_message istr_window_message

ULONG		iul_handle
end variables

event open;STRING			ls_sql
INT 				li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')

/*	grab the passed value into the datastore */
istr_window_message 	= Message.PowerObjectParm

ls_sql 		= istr_window_message.as_stringparm[1]
mle_sql.text = ls_sql


end event

on w_management_report_sql.create
this.mle_sql=create mle_sql
this.cb_close=create cb_close
this.Control[]={this.mle_sql,&
this.cb_close}
end on

on w_management_report_sql.destroy
destroy(this.mle_sql)
destroy(this.cb_close)
end on

type mle_sql from multilineedit within w_management_report_sql
integer y = 12
integer width = 3150
integer height = 1320
integer taborder = 11
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean hscrollbar = true
boolean vscrollbar = true
end type

type cb_close from commandbutton within w_management_report_sql
integer x = 2898
integer y = 1364
integer width = 283
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

event clicked;close(parent)


end event

event rowfocuschanged;call super::rowfocuschanged;//MAKE SURE IT IS A valid row.

end event

event rbuttondown;return 1
end event

