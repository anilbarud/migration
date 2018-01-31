$PBExportHeader$w_paper_mail_package_complete.srw
forward
global type w_paper_mail_package_complete from window
end type
type st_3 from statictext within w_paper_mail_package_complete
end type
type st_8 from statictext within w_paper_mail_package_complete
end type
type st_7 from statictext within w_paper_mail_package_complete
end type
type st_6 from statictext within w_paper_mail_package_complete
end type
type st_5 from statictext within w_paper_mail_package_complete
end type
type st_4 from statictext within w_paper_mail_package_complete
end type
type cb_1 from commandbutton within w_paper_mail_package_complete
end type
type st_2 from statictext within w_paper_mail_package_complete
end type
type st_1 from statictext within w_paper_mail_package_complete
end type
end forward

global type w_paper_mail_package_complete from window
integer width = 3099
integer height = 1444
boolean titlebar = true
string title = "Registration Letter Produced"
boolean controlmenu = true
boolean minbox = true
windowtype windowtype = popup!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
st_3 st_3
st_8 st_8
st_7 st_7
st_6 st_6
st_5 st_5
st_4 st_4
cb_1 cb_1
st_2 st_2
st_1 st_1
end type
global w_paper_mail_package_complete w_paper_mail_package_complete

on w_paper_mail_package_complete.create
this.st_3=create st_3
this.st_8=create st_8
this.st_7=create st_7
this.st_6=create st_6
this.st_5=create st_5
this.st_4=create st_4
this.cb_1=create cb_1
this.st_2=create st_2
this.st_1=create st_1
this.Control[]={this.st_3,&
this.st_8,&
this.st_7,&
this.st_6,&
this.st_5,&
this.st_4,&
this.cb_1,&
this.st_2,&
this.st_1}
end on

on w_paper_mail_package_complete.destroy
destroy(this.st_3)
destroy(this.st_8)
destroy(this.st_7)
destroy(this.st_6)
destroy(this.st_5)
destroy(this.st_4)
destroy(this.cb_1)
destroy(this.st_2)
destroy(this.st_1)
end on

event open;S_WINDOW_MESSAGE lstr_message

 lstr_message= Message.PowerObjectParm

st_1.text = lstr_message.as_stringparm[3]
st_2.text = string(lstr_message.adtm_datetimeparm[1])
end event

type st_3 from statictext within w_paper_mail_package_complete
integer x = 41
integer y = 716
integer width = 1248
integer height = 104
integer textsize = -11
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "The Registration Code for the provider is: "
boolean focusrectangle = false
end type

type st_8 from statictext within w_paper_mail_package_complete
integer x = 55
integer y = 512
integer width = 2615
integer height = 172
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "2. The Registration code must be shared with the Provider as they will be required to enter it during the registration process."
boolean focusrectangle = false
end type

type st_7 from statictext within w_paper_mail_package_complete
integer x = 46
integer y = 416
integer width = 2606
integer height = 136
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "1. You will not be able to view the Registration Code once you close this window.  "
boolean focusrectangle = false
end type

type st_6 from statictext within w_paper_mail_package_complete
integer x = 69
integer y = 288
integer width = 457
integer height = 92
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean underline = true
long backcolor = 67108864
string text = "Reminders"
boolean focusrectangle = false
end type

type st_5 from statictext within w_paper_mail_package_complete
integer x = 59
integer y = 148
integer width = 2871
integer height = 92
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "You have successfully produced the Registration letter for the Entity Administrator"
boolean focusrectangle = false
end type

type st_4 from statictext within w_paper_mail_package_complete
integer x = 41
integer y = 848
integer width = 1234
integer height = 112
integer textsize = -11
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "This Registration Invitation will expire on :"
boolean focusrectangle = false
end type

type cb_1 from commandbutton within w_paper_mail_package_complete
integer x = 1193
integer y = 1136
integer width = 402
integer height = 104
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "OK"
end type

event clicked;close(w_paper_mail_package_complete)
end event

type st_2 from statictext within w_paper_mail_package_complete
integer x = 1312
integer y = 848
integer width = 663
integer height = 112
integer textsize = -11
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

type st_1 from statictext within w_paper_mail_package_complete
integer x = 1312
integer y = 716
integer width = 663
integer height = 104
integer textsize = -11
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

