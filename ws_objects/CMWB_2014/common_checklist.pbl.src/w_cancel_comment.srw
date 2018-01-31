$PBExportHeader$w_cancel_comment.srw
forward
global type w_cancel_comment from window
end type
type st_1 from statictext within w_cancel_comment
end type
type mle_cancel_comment from multilineedit within w_cancel_comment
end type
type cb_cancel from commandbutton within w_cancel_comment
end type
type cb_save from commandbutton within w_cancel_comment
end type
end forward

global type w_cancel_comment from window
integer width = 2021
integer height = 776
boolean titlebar = true
string title = "Please enter a Checklist Cancelled Comment:"
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
st_1 st_1
mle_cancel_comment mle_cancel_comment
cb_cancel cb_cancel
cb_save cb_save
end type
global w_cancel_comment w_cancel_comment

type variables
Integer  ii_step_no
Long il_checklist_no
String is_user_id, is_status
Datetime idtm_datetime

n_transaction itr_trans_object
end variables

on w_cancel_comment.create
this.st_1=create st_1
this.mle_cancel_comment=create mle_cancel_comment
this.cb_cancel=create cb_cancel
this.cb_save=create cb_save
this.Control[]={this.st_1,&
this.mle_cancel_comment,&
this.cb_cancel,&
this.cb_save}
end on

on w_cancel_comment.destroy
destroy(this.st_1)
destroy(this.mle_cancel_comment)
destroy(this.cb_cancel)
destroy(this.cb_save)
end on

event open;// this window does not have nf_transaction_count called to determine if
// there is an open transaction. This is because this window will always be
// opened between a begin & commit transaction.
end event

type st_1 from statictext within w_cancel_comment
integer x = 69
integer y = 32
integer width = 430
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Enter Comment:"
boolean focusrectangle = false
end type

type mle_cancel_comment from multilineedit within w_cancel_comment
integer x = 73
integer y = 100
integer width = 1888
integer height = 404
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type cb_cancel from commandbutton within w_cancel_comment
integer x = 1381
integer y = 580
integer width = 306
integer height = 104
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Cancel"
end type

event clicked;S_MESSAGE   lstr_message

MessageBox('Information','Cancelling the comment will also cancel the status change.',Information!)

lstr_message.as_stringparm[1] = 'cancel'
lstr_message.as_stringparm[2] = ''

CloseWithReturn(w_cancel_comment,lstr_message)

end event

type cb_save from commandbutton within w_cancel_comment
integer x = 1696
integer y = 580
integer width = 306
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Save"
boolean default = true
end type

event clicked;STRING      ls_comment
S_MESSAGE   lstr_message


ls_comment = f_clean_string_1(Trim(mle_cancel_comment.Text))

IF ISNULL(ls_comment) OR ls_comment = '' THEN
	Messagebox('Error','A cancelled comment must be entered.',Exclamation!)
	RETURN
END IF

IF LEN(ls_comment) < 5 OR LEN(ls_comment) > 256 THEN
	Messagebox('Error','The cancelled comment must be more than 5 characters and less than 256.~nPlease revise.',Exclamation!)
	RETURN
END IF 

lstr_message.as_stringparm[1] = 'save'
lstr_message.as_stringparm[2] = ls_comment

CloseWithReturn(w_cancel_comment,lstr_message)
end event

