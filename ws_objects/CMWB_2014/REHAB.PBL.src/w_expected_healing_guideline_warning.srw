$PBExportHeader$w_expected_healing_guideline_warning.srw
forward
global type w_expected_healing_guideline_warning from window
end type
type mle_2 from multilineedit within w_expected_healing_guideline_warning
end type
type p_1 from picture within w_expected_healing_guideline_warning
end type
type st_1 from statictext within w_expected_healing_guideline_warning
end type
type cb_abort from commandbutton within w_expected_healing_guideline_warning
end type
type cb_continue from commandbutton within w_expected_healing_guideline_warning
end type
type mle_warning_message from multilineedit within w_expected_healing_guideline_warning
end type
type dw_guideline from u_dw_online within w_expected_healing_guideline_warning
end type
type rr_1 from roundrectangle within w_expected_healing_guideline_warning
end type
end forward

global type w_expected_healing_guideline_warning from window
integer width = 2551
integer height = 1292
boolean titlebar = true
string title = "Expected Healing Guidelines"
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
mle_2 mle_2
p_1 p_1
st_1 st_1
cb_abort cb_abort
cb_continue cb_continue
mle_warning_message mle_warning_message
dw_guideline dw_guideline
rr_1 rr_1
end type
global w_expected_healing_guideline_warning w_expected_healing_guideline_warning

event open;STRING		ls_warning_message

// this window does not have nf_transaction_count called to determine if
// there is an open transaction. This is because this window will always be
// opened between a begin & commit transaction.

ls_warning_message = Message.StringParm

mle_warning_message.text = ls_warning_message


dw_guideline.SetTransObject(SQLCA)
dw_guideline.Retrieve()

SQLCA.nf_handle_error('w_healing_guideline','open','retrieve()')

end event

on w_expected_healing_guideline_warning.create
this.mle_2=create mle_2
this.p_1=create p_1
this.st_1=create st_1
this.cb_abort=create cb_abort
this.cb_continue=create cb_continue
this.mle_warning_message=create mle_warning_message
this.dw_guideline=create dw_guideline
this.rr_1=create rr_1
this.Control[]={this.mle_2,&
this.p_1,&
this.st_1,&
this.cb_abort,&
this.cb_continue,&
this.mle_warning_message,&
this.dw_guideline,&
this.rr_1}
end on

on w_expected_healing_guideline_warning.destroy
destroy(this.mle_2)
destroy(this.p_1)
destroy(this.st_1)
destroy(this.cb_abort)
destroy(this.cb_continue)
destroy(this.mle_warning_message)
destroy(this.dw_guideline)
destroy(this.rr_1)
end on

type mle_2 from multilineedit within w_expected_healing_guideline_warning
integer x = 69
integer y = 324
integer width = 809
integer height = 120
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "Please review these guidelines carefully."
boolean border = false
end type

type p_1 from picture within w_expected_healing_guideline_warning
integer x = 247
integer y = 456
integer width = 457
integer height = 740
string picturename = "doctor.gif"
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
boolean map3dcolors = true
end type

type st_1 from statictext within w_expected_healing_guideline_warning
integer x = 855
integer y = 1020
integer width = 713
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Continue with the save?"
boolean focusrectangle = false
end type

type cb_abort from commandbutton within w_expected_healing_guideline_warning
integer x = 2066
integer y = 992
integer width = 402
integer height = 112
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "C&ancel"
end type

event clicked;CloseWithReturn(Parent,-1)
end event

type cb_continue from commandbutton within w_expected_healing_guideline_warning
integer x = 1641
integer y = 992
integer width = 402
integer height = 112
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Continue"
end type

event clicked;CloseWithReturn(Parent,1)
end event

type mle_warning_message from multilineedit within w_expected_healing_guideline_warning
integer x = 69
integer y = 32
integer width = 805
integer height = 276
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "You have entered a reset date that is passed the suggested default date."
boolean border = false
end type

type dw_guideline from u_dw_online within w_expected_healing_guideline_warning
integer x = 1001
integer y = 236
integer width = 1458
integer height = 664
integer taborder = 10
string dataobject = "d_expected_healing_guideline_paper"
boolean vscrollbar = true
boolean border = false
borderstyle borderstyle = stylelowered!
end type

type rr_1 from roundrectangle within w_expected_healing_guideline_warning
long linecolor = 33554432
integer linethickness = 4
long fillcolor = 16777215
integer x = 50
integer y = 20
integer width = 850
integer height = 436
integer cornerheight = 40
integer cornerwidth = 46
end type

