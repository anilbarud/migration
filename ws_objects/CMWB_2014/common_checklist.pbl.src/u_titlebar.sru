$PBExportHeader$u_titlebar.sru
forward
global type u_titlebar from userobject
end type
type st_checklist_name from statictext within u_titlebar
end type
end forward

global type u_titlebar from userobject
integer width = 1957
integer height = 100
long backcolor = 134217730
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
st_checklist_name st_checklist_name
end type
global u_titlebar u_titlebar

type variables


end variables

on u_titlebar.create
this.st_checklist_name=create st_checklist_name
this.Control[]={this.st_checklist_name}
end on

on u_titlebar.destroy
destroy(this.st_checklist_name)
end on

type st_checklist_name from statictext within u_titlebar
integer x = 5
integer width = 1947
integer height = 84
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 134217730
boolean focusrectangle = false
end type

