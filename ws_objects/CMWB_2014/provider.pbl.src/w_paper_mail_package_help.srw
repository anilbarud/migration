$PBExportHeader$w_paper_mail_package_help.srw
forward
global type w_paper_mail_package_help from window
end type
type st_9 from statictext within w_paper_mail_package_help
end type
type st_7 from statictext within w_paper_mail_package_help
end type
type st_8 from statictext within w_paper_mail_package_help
end type
type st_6 from statictext within w_paper_mail_package_help
end type
type st_5 from statictext within w_paper_mail_package_help
end type
type st_4 from statictext within w_paper_mail_package_help
end type
type st_3 from statictext within w_paper_mail_package_help
end type
type st_2 from statictext within w_paper_mail_package_help
end type
type cb_close from commandbutton within w_paper_mail_package_help
end type
end forward

global type w_paper_mail_package_help from window
integer width = 2994
integer height = 2160
boolean titlebar = true
string title = "Registration Letter Help"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 15793151
string icon = "AppIcon!"
boolean center = true
st_9 st_9
st_7 st_7
st_8 st_8
st_6 st_6
st_5 st_5
st_4 st_4
st_3 st_3
st_2 st_2
cb_close cb_close
end type
global w_paper_mail_package_help w_paper_mail_package_help

on w_paper_mail_package_help.create
this.st_9=create st_9
this.st_7=create st_7
this.st_8=create st_8
this.st_6=create st_6
this.st_5=create st_5
this.st_4=create st_4
this.st_3=create st_3
this.st_2=create st_2
this.cb_close=create cb_close
this.Control[]={this.st_9,&
this.st_7,&
this.st_8,&
this.st_6,&
this.st_5,&
this.st_4,&
this.st_3,&
this.st_2,&
this.cb_close}
end on

on w_paper_mail_package_help.destroy
destroy(this.st_9)
destroy(this.st_7)
destroy(this.st_8)
destroy(this.st_6)
destroy(this.st_5)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.st_2)
destroy(this.cb_close)
end on

type st_9 from statictext within w_paper_mail_package_help
integer x = 119
integer y = 92
integer width = 2734
integer height = 360
integer textsize = -11
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "A Registration letter must be produced, printed and mailed to the Provider when:     1. A new provider is created or 2. if the provider no longer has an Entity Administrator and the provider has requested that a new Entity admininistrator be set up."
boolean border = true
boolean focusrectangle = false
end type

type st_7 from statictext within w_paper_mail_package_help
integer x = 46
integer y = 960
integer width = 343
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean underline = true
long textcolor = 33554432
long backcolor = 553648127
string text = "Please Note:"
boolean focusrectangle = false
end type

type st_8 from statictext within w_paper_mail_package_help
integer x = 119
integer y = 1040
integer width = 2734
integer height = 160
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "The Registration code can be a phrase, a word or a sentence. It is restricted to minimum of 5 characters and a maximum of 25 characters."
boolean focusrectangle = false
end type

type st_6 from statictext within w_paper_mail_package_help
integer x = 119
integer y = 480
integer width = 2734
integer height = 332
integer textsize = -11
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "As part of the registration process, the Provider and WorkSafeNB will be required to share a registration code. Either WorkSafeNB or the Provider can choose what the registration code will be. The code will NOT be printed on the Registration letter."
boolean border = true
boolean focusrectangle = false
end type

type st_5 from statictext within w_paper_mail_package_help
integer x = 119
integer y = 1584
integer width = 2734
integer height = 144
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "To re-issue an invitation, a new Registration code will be required, communicated to the Provider and a new Registration Letter printed and sent to the provider. Any previous invitations will be automatically cancelled."
boolean focusrectangle = false
end type

type st_4 from statictext within w_paper_mail_package_help
integer x = 119
integer y = 1460
integer width = 2734
integer height = 152
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "An invitation may be re-issued by WorkSafeNB by producing another Registration letter."
boolean focusrectangle = false
end type

type st_3 from statictext within w_paper_mail_package_help
integer x = 119
integer y = 1300
integer width = 2734
integer height = 112
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "A pending invitation may be manually cancelled for any reason by WorkSafeNB. You must enter a reason for the cancellation."
boolean focusrectangle = false
end type

type st_2 from statictext within w_paper_mail_package_help
integer x = 119
integer y = 1208
integer width = 2734
integer height = 60
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "An invitation will automatically expire after 14 days if Provider has not accepted the invitation."
boolean focusrectangle = false
end type

type cb_close from commandbutton within w_paper_mail_package_help
integer x = 1221
integer y = 1928
integer width = 402
integer height = 104
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Close"
boolean default = true
end type

event clicked;close(w_paper_mail_package_help)
end event

