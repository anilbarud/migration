$PBExportHeader$w_a_tool.srw
$PBExportComments$Common - ancestor window inherited from for claim modules (ie. child windows on w_sheet)
forward
global type w_a_tool from w_ancestor
end type
type st_title from statictext within w_a_tool
end type
type cb_close from commandbutton within w_a_tool
end type
end forward

global type w_a_tool from w_ancestor
integer x = 23
integer y = 376
integer width = 3237
integer height = 1824
boolean titlebar = false
string menuname = ""
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
windowtype windowtype = child!
long backcolor = 67108864
st_title st_title
cb_close cb_close
end type
global w_a_tool w_a_tool

type variables
w_sheet		iw_active_sheet
end variables

event open;call super::open;iw_active_sheet = w_frame.GetActiveSheet()
If not IsValid(iw_active_sheet) Then
	MessageBox("Closing " + st_title.text,"Could not determine the active sheet.  You may be low on resources",Exclamation!)
	
		Close(This)
	Return
End If

iw_active_sheet.dynamic wf_register_child(This)
end event

on w_a_tool.create
int iCurrent
call super::create
this.st_title=create st_title
this.cb_close=create cb_close
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_title
this.Control[iCurrent+2]=this.cb_close
end on

on w_a_tool.destroy
call super::destroy
destroy(this.st_title)
destroy(this.cb_close)
end on

type st_title from statictext within w_a_tool
integer x = 9
integer y = 8
integer width = 3145
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
boolean enabled = false
string text = "Put Title Here"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cb_close from commandbutton within w_a_tool
integer x = 2752
integer y = 1680
integer width = 379
integer height = 100
integer taborder = 1
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

on clicked;SetPointer(HourGlass!)
Close(parent)
end on

