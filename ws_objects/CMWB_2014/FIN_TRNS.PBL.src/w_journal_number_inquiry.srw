$PBExportHeader$w_journal_number_inquiry.srw
forward
global type w_journal_number_inquiry from w_a_report
end type
type cb_ok from commandbutton within w_journal_number_inquiry
end type
type gb_select_criteria from groupbox within w_journal_number_inquiry
end type
type st_1 from statictext within w_journal_number_inquiry
end type
type sle_journal_control_no from singlelineedit within w_journal_number_inquiry
end type
end forward

global type w_journal_number_inquiry from w_a_report
string title = "GL Journal Number Inquiry"
cb_ok cb_ok
gb_select_criteria gb_select_criteria
st_1 st_1
sle_journal_control_no sle_journal_control_no
end type
global w_journal_number_inquiry w_journal_number_inquiry

type variables
DATAWINDOWCHILD	iw_select_region
DATAWINDOWCHILD	iw_select_award
end variables

event open;call super::open;/*	This script controls setting up and defaulting the search criteria
*/


/*	Set up the report datawindow.
*/
	dw_report.SetTransObject(SQLCA)

end event

on w_journal_number_inquiry.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.gb_select_criteria=create gb_select_criteria
this.st_1=create st_1
this.sle_journal_control_no=create sle_journal_control_no
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.gb_select_criteria
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.sle_journal_control_no
end on

on w_journal_number_inquiry.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.gb_select_criteria)
destroy(this.st_1)
destroy(this.sle_journal_control_no)
end on

type dw_report from w_a_report`dw_report within w_journal_number_inquiry
string dataobject = "d_journal_number_inquiry"
boolean hscrollbar = true
end type

type cb_ok from commandbutton within w_journal_number_inquiry
integer x = 2153
integer y = 212
integer width = 389
integer height = 108
integer taborder = 10
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;/*	Variables
*/
	LONG 		ll_numrows
	STRING	ls_journal_control_no

	SetPointer(HourGlass!)

	ls_journal_control_no = sle_journal_control_no.Text
	
/*	Retrieve the report.
*/
	ll_numrows = dw_report.Retrieve(ls_journal_control_no)
	SQLCA.nf_handle_error("w_gl_journal_number_inquiry","","cb_ok - dw_report.Retrieve(ls_journal_control_no)")
	If ll_numrows <= 0 then
		MessageBox("GL Journal Number Inquiry","No data found to satisfy request.")
	End If

	SetPointer(Arrow!)

end event

type gb_select_criteria from groupbox within w_journal_number_inquiry
integer x = 50
integer y = 112
integer width = 1518
integer height = 200
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Selection Criteria"
end type

type st_1 from statictext within w_journal_number_inquiry
integer x = 96
integer y = 196
integer width = 727
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "GL Journal Control Number:"
boolean focusrectangle = false
end type

type sle_journal_control_no from singlelineedit within w_journal_number_inquiry
integer x = 850
integer y = 180
integer width = 640
integer height = 88
integer taborder = 1
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean autohscroll = false
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
end type

