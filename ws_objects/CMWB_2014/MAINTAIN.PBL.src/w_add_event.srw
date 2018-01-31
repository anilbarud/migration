$PBExportHeader$w_add_event.srw
forward
global type w_add_event from window
end type
type cb_cancel from commandbutton within w_add_event
end type
type cb_ok from commandbutton within w_add_event
end type
type st_comment from statictext within w_add_event
end type
type mle_comment from multilineedit within w_add_event
end type
type gb_1 from groupbox within w_add_event
end type
end forward

global type w_add_event from window
integer width = 2057
integer height = 700
boolean titlebar = true
string title = "Event Comment"
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
cb_cancel cb_cancel
cb_ok cb_ok
st_comment st_comment
mle_comment mle_comment
gb_1 gb_1
end type
global w_add_event w_add_event

on w_add_event.create
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.st_comment=create st_comment
this.mle_comment=create mle_comment
this.gb_1=create gb_1
this.Control[]={this.cb_cancel,&
this.cb_ok,&
this.st_comment,&
this.mle_comment,&
this.gb_1}
end on

on w_add_event.destroy
destroy(this.cb_cancel)
destroy(this.cb_ok)
destroy(this.st_comment)
destroy(this.mle_comment)
destroy(this.gb_1)
end on

event open;INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


This.height = 700
This.width = 2057
end event

type cb_cancel from commandbutton within w_add_event
integer x = 1659
integer y = 424
integer width = 256
integer height = 104
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cancel"
end type

event clicked;CloseWithReturn(Parent, '')
end event

type cb_ok from commandbutton within w_add_event
integer x = 1376
integer y = 424
integer width = 256
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "OK"
end type

event clicked;STRING ls_comment

ls_comment = mle_comment.Text

IF TRIM(ls_comment) > '' THEN 		
	CloseWithReturn(Parent, ls_comment)
ELSE
	MessageBox('Invalid Comment','You must enter a comment.',information!)
END IF
end event

type st_comment from statictext within w_add_event
integer x = 64
integer y = 72
integer width = 855
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Please enter an Event Comment:"
boolean focusrectangle = false
end type

type mle_comment from multilineedit within w_add_event
integer x = 87
integer y = 148
integer width = 1838
integer height = 244
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
integer limit = 215
borderstyle borderstyle = stylelowered!
end type

type gb_1 from groupbox within w_add_event
integer x = 14
integer y = 20
integer width = 1989
integer height = 540
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
end type

