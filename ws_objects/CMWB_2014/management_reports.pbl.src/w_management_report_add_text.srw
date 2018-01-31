$PBExportHeader$w_management_report_add_text.srw
$PBExportComments$Used to give basic information for authorizations
forward
global type w_management_report_add_text from window
end type
type cb_save from commandbutton within w_management_report_add_text
end type
type mle_add_note from multilineedit within w_management_report_add_text
end type
type cb_close from commandbutton within w_management_report_add_text
end type
end forward

global type w_management_report_add_text from window
integer x = 2002
integer y = 1000
integer width = 3067
integer height = 740
boolean titlebar = true
string title = "Management Report - Add Note"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean clientedge = true
cb_save cb_save
mle_add_note mle_add_note
cb_close cb_close
end type
global w_management_report_add_text w_management_report_add_text

type variables
s_window_message istr_window_message

ULONG		iul_handle
end variables

event open;STRING			ls_message
INT 				li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')

/*	grab the passed value into the datastore */
ls_message 	= Message.STRINGParm

mle_add_note.text = ls_message



end event

on w_management_report_add_text.create
this.cb_save=create cb_save
this.mle_add_note=create mle_add_note
this.cb_close=create cb_close
this.Control[]={this.cb_save,&
this.mle_add_note,&
this.cb_close}
end on

on w_management_report_add_text.destroy
destroy(this.cb_save)
destroy(this.mle_add_note)
destroy(this.cb_close)
end on

type cb_save from commandbutton within w_management_report_add_text
integer x = 2153
integer y = 520
integer width = 485
integer height = 84
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Save and Close"
end type

event clicked;STRING			ls_message


ls_message = TRIM(mle_add_note.text)

IF isnull(ls_message) THEN ls_message = ''

closewithreturn(parent, ls_message)
end event

type mle_add_note from multilineedit within w_management_report_add_text
integer y = 12
integer width = 3031
integer height = 476
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
integer limit = 200
end type

type cb_close from commandbutton within w_management_report_add_text
integer x = 2679
integer y = 520
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

event clicked;STRING			ls_message


ls_message = TRIM(mle_add_note.text)

IF isnull(ls_message) THEN ls_message = ''

closewithreturn(parent, ls_message)



end event

event rowfocuschanged;call super::rowfocuschanged;//MAKE SURE IT IS A valid row.

end event

event rbuttondown;return 1
end event

