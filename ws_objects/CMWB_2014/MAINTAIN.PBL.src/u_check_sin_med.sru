$PBExportHeader$u_check_sin_med.sru
$PBExportComments$object containing datawindow to list out SIN or Medicare # and individual name for those with the given number
forward
global type u_check_sin_med from UserObject
end type
type sle_message from singlelineedit within u_check_sin_med
end type
type cb_ok from commandbutton within u_check_sin_med
end type
type dw_list from u_dw_online within u_check_sin_med
end type
end forward

global type u_check_sin_med from UserObject
int Width=2181
int Height=849
boolean Border=true
long BackColor=67108864
sle_message sle_message
cb_ok cb_ok
dw_list dw_list
end type
global u_check_sin_med u_check_sin_med

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

on u_check_sin_med.create
this.sle_message=create sle_message
this.cb_ok=create cb_ok
this.dw_list=create dw_list
this.Control[]={ this.sle_message,&
this.cb_ok,&
this.dw_list}
end on

on u_check_sin_med.destroy
destroy(this.sle_message)
destroy(this.cb_ok)
destroy(this.dw_list)
end on

type sle_message from singlelineedit within u_check_sin_med
int X=5
int Y=597
int Width=2149
int Height=89
int TabOrder=20
boolean Border=false
boolean AutoHScroll=false
long TextColor=33554432
long BackColor=11250091
int TextSize=-9
int Weight=400
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

type cb_ok from commandbutton within u_check_sin_med
int X=910
int Y=689
int Width=247
int Height=109
int TabOrder=30
string Text="&OK"
boolean Default=true
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

on clicked;Parent.Visible = FALSE
end on

type dw_list from u_dw_online within u_check_sin_med
int X=10
int Y=13
int Width=2149
int Height=573
int TabOrder=10
string DataObject="d_sin_check"
BorderStyle BorderStyle=StyleLowered!
boolean VScrollBar=true
end type

