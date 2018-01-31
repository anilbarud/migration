$PBExportHeader$w_popup_messagebox.srw
forward
global type w_popup_messagebox from window
end type
type st_1 from statictext within w_popup_messagebox
end type
type cb_yestoall from commandbutton within w_popup_messagebox
end type
type cb_no from commandbutton within w_popup_messagebox
end type
type st_message from statictext within w_popup_messagebox
end type
type cb_yes from commandbutton within w_popup_messagebox
end type
end forward

global type w_popup_messagebox from window
integer width = 3017
integer height = 656
boolean titlebar = true
string title = "Warning"
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
st_1 st_1
cb_yestoall cb_yestoall
cb_no cb_no
st_message st_message
cb_yes cb_yes
end type
global w_popup_messagebox w_popup_messagebox

type variables
s_window_message istr_window_message
end variables

on w_popup_messagebox.create
this.st_1=create st_1
this.cb_yestoall=create cb_yestoall
this.cb_no=create cb_no
this.st_message=create st_message
this.cb_yes=create cb_yes
this.Control[]={this.st_1,&
this.cb_yestoall,&
this.cb_no,&
this.st_message,&
this.cb_yes}
end on

on w_popup_messagebox.destroy
destroy(this.st_1)
destroy(this.cb_yestoall)
destroy(this.cb_no)
destroy(this.st_message)
destroy(this.cb_yes)
end on

event open;string ls_message

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')



istr_window_message = Message.PowerObjectParm

ls_message = istr_window_message.as_stringparm[1]

st_message.text = ls_message
end event

type st_1 from statictext within w_popup_messagebox
integer x = 128
integer y = 64
integer width = 402
integer height = 96
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
string text = "Continue ?"
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type cb_yestoall from commandbutton within w_popup_messagebox
integer x = 1737
integer y = 432
integer width = 402
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Yes to &All"
end type

event clicked;istr_window_message.al_doubleparm[1] = 3
 
closewithreturn(parent,istr_window_message)
end event

type cb_no from commandbutton within w_popup_messagebox
integer x = 1298
integer y = 432
integer width = 402
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&No"
end type

event clicked;istr_window_message.al_doubleparm[1] = 2
 
closewithreturn(parent,istr_window_message)
end event

type st_message from statictext within w_popup_messagebox
integer x = 165
integer y = 172
integer width = 2775
integer height = 220
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type cb_yes from commandbutton within w_popup_messagebox
integer x = 859
integer y = 432
integer width = 402
integer height = 104
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Yes"
end type

event clicked;
istr_window_message.al_doubleparm[1] = 1
 
closewithreturn(parent,istr_window_message)
end event

