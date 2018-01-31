$PBExportHeader$w_group_for_authorizing_help.srw
forward
global type w_group_for_authorizing_help from window
end type
type st_5 from statictext within w_group_for_authorizing_help
end type
type st_4 from statictext within w_group_for_authorizing_help
end type
type st_3 from statictext within w_group_for_authorizing_help
end type
type st_2 from statictext within w_group_for_authorizing_help
end type
type st_1 from statictext within w_group_for_authorizing_help
end type
type cb_ok from commandbutton within w_group_for_authorizing_help
end type
type r_1 from rectangle within w_group_for_authorizing_help
end type
type r_2 from rectangle within w_group_for_authorizing_help
end type
end forward

global type w_group_for_authorizing_help from window
integer width = 2363
integer height = 1184
boolean titlebar = true
string title = "Help - group for Authorization"
boolean controlmenu = true
boolean minbox = true
windowtype windowtype = popup!
long backcolor = 134217731
string icon = "AppIcon!"
boolean center = true
st_5 st_5
st_4 st_4
st_3 st_3
st_2 st_2
st_1 st_1
cb_ok cb_ok
r_1 r_1
r_2 r_2
end type
global w_group_for_authorizing_help w_group_for_authorizing_help

on w_group_for_authorizing_help.create
this.st_5=create st_5
this.st_4=create st_4
this.st_3=create st_3
this.st_2=create st_2
this.st_1=create st_1
this.cb_ok=create cb_ok
this.r_1=create r_1
this.r_2=create r_2
this.Control[]={this.st_5,&
this.st_4,&
this.st_3,&
this.st_2,&
this.st_1,&
this.cb_ok,&
this.r_1,&
this.r_2}
end on

on w_group_for_authorizing_help.destroy
destroy(this.st_5)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.cb_ok)
destroy(this.r_1)
destroy(this.r_2)
end on

type st_5 from statictext within w_group_for_authorizing_help
integer x = 315
integer y = 536
integer width = 178
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "OR"
boolean focusrectangle = false
end type

type st_4 from statictext within w_group_for_authorizing_help
integer x = 165
integer y = 616
integer width = 2085
integer height = 144
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "•     The ‘Group for Authorization’ FLAG must be unchecked, so the payment can be authorized and processed as a single payment"
boolean focusrectangle = false
end type

type st_3 from statictext within w_group_for_authorizing_help
integer x = 165
integer y = 436
integer width = 2098
integer height = 104
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "•    The payments must be added to an Authorization Group via the ‘Group’ Button"
boolean focusrectangle = false
end type

type st_2 from statictext within w_group_for_authorizing_help
integer x = 101
integer y = 308
integer width = 2094
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "To ensure these payments are authorized appropriately:"
boolean focusrectangle = false
end type

type st_1 from statictext within w_group_for_authorizing_help
integer x = 137
integer y = 104
integer width = 1989
integer height = 144
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "Payments that have been flagged to be grouped for authorization, but have not yet been assigned to a group, are not authorized payments.  "
boolean focusrectangle = false
end type

type cb_ok from commandbutton within w_group_for_authorizing_help
integer x = 910
integer y = 940
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

event clicked;CLOSE(PARENT)
end event

type r_1 from rectangle within w_group_for_authorizing_help
long linecolor = 33554432
integer linethickness = 4
long fillcolor = 16777215
integer x = 55
integer y = 64
integer width = 2245
integer height = 200
end type

type r_2 from rectangle within w_group_for_authorizing_help
long linecolor = 33554432
integer linethickness = 4
long fillcolor = 16777215
integer x = 46
integer y = 284
integer width = 2258
integer height = 604
end type

