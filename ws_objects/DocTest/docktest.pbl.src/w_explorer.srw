$PBExportHeader$w_explorer.srw
forward
global type w_explorer from window
end type
type st_1 from statictext within w_explorer
end type
end forward

global type w_explorer from window
integer width = 923
integer height = 2200
boolean titlebar = true
string title = "Explorer"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
st_1 st_1
end type
global w_explorer w_explorer

on w_explorer.create
this.st_1=create st_1
this.Control[]={this.st_1}
end on

on w_explorer.destroy
destroy(this.st_1)
end on

type st_1 from statictext within w_explorer
integer x = 165
integer y = 352
integer width = 402
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 67108864
string text = "Explorer"
boolean focusrectangle = false
end type

