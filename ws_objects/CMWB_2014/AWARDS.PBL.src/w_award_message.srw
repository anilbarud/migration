$PBExportHeader$w_award_message.srw
forward
global type w_award_message from window
end type
type mle_message from multilineedit within w_award_message
end type
type cb_ok from commandbutton within w_award_message
end type
end forward

global type w_award_message from window
integer x = 1650
integer y = 636
integer width = 1399
integer height = 1600
boolean titlebar = true
string title = "Award Messages"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
mle_message mle_message
cb_ok cb_ok
end type
global w_award_message w_award_message

on w_award_message.create
this.mle_message=create mle_message
this.cb_ok=create cb_ok
this.Control[]={this.mle_message,&
this.cb_ok}
end on

on w_award_message.destroy
destroy(this.mle_message)
destroy(this.cb_ok)
end on

event open;S_WINDOW_MESSAGE	lstr_message
LONG					ll_row

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


lstr_message = Message.PowerObjectParm

mle_message.text = lstr_message.as_stringparm[1]

end event

type mle_message from multilineedit within w_award_message
integer x = 101
integer y = 36
integer width = 1216
integer height = 1328
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean border = false
end type

type cb_ok from commandbutton within w_award_message
integer x = 535
integer y = 1392
integer width = 247
integer height = 108
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "O&K"
end type

event clicked;SetPointer(HourGlass!)
Close(Parent)
end event

