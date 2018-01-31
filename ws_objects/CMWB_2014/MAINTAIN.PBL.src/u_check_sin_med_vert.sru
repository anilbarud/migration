$PBExportHeader$u_check_sin_med_vert.sru
$PBExportComments$object containing datawindow to list out SIN or Medicare # and individual name for those with the given number
forward
global type u_check_sin_med_vert from userobject
end type
type sle_message from singlelineedit within u_check_sin_med_vert
end type
type cb_ok from commandbutton within u_check_sin_med_vert
end type
type dw_list from u_dw_online within u_check_sin_med_vert
end type
end forward

global type u_check_sin_med_vert from userobject
integer width = 1655
integer height = 848
boolean border = true
long backcolor = 67108864
sle_message sle_message
cb_ok cb_ok
dw_list dw_list
end type
global u_check_sin_med_vert u_check_sin_med_vert

forward prototypes
public subroutine uf_set_dw (string as_dw)
end prototypes

public subroutine uf_set_dw (string as_dw);IF as_dw = 'SIN' THEN
   dw_list.DataObject = 'd_sin_check'
ELSE
   dw_list.DataObject = 'd_medicare_check'
END IF
 
dw_list.SetTransObject(SQLCA)
Return
end subroutine

on u_check_sin_med_vert.create
this.sle_message=create sle_message
this.cb_ok=create cb_ok
this.dw_list=create dw_list
this.Control[]={this.sle_message,&
this.cb_ok,&
this.dw_list}
end on

on u_check_sin_med_vert.destroy
destroy(this.sle_message)
destroy(this.cb_ok)
destroy(this.dw_list)
end on

type sle_message from singlelineedit within u_check_sin_med_vert
integer x = 5
integer y = 596
integer width = 1637
integer height = 88
integer taborder = 20
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 11250091
boolean border = false
boolean autohscroll = false
end type

type cb_ok from commandbutton within u_check_sin_med_vert
integer x = 690
integer y = 688
integer width = 247
integer height = 108
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
boolean default = true
end type

on clicked;Parent.Visible = FALSE
end on

type dw_list from u_dw_online within u_check_sin_med_vert
integer x = 9
integer y = 12
integer width = 1627
integer height = 572
integer taborder = 10
string dataobject = "d_sin_check"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

