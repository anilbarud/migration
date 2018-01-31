$PBExportHeader$w_annuity_comment.srw
forward
global type w_annuity_comment from window
end type
type st_comment from statictext within w_annuity_comment
end type
type cb_ok from commandbutton within w_annuity_comment
end type
type mle_comment from multilineedit within w_annuity_comment
end type
end forward

global type w_annuity_comment from window
integer width = 2304
integer height = 1672
boolean titlebar = true
string title = "Enter Annuity Eligibility Comment"
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
st_comment st_comment
cb_ok cb_ok
mle_comment mle_comment
end type
global w_annuity_comment w_annuity_comment

type variables
BOOLEAN  ib_required
INTEGER  ii_min_number_of_chars
end variables

on w_annuity_comment.create
this.st_comment=create st_comment
this.cb_ok=create cb_ok
this.mle_comment=create mle_comment
this.Control[]={this.st_comment,&
this.cb_ok,&
this.mle_comment}
end on

on w_annuity_comment.destroy
destroy(this.st_comment)
destroy(this.cb_ok)
destroy(this.mle_comment)
end on

event open;S_WINDOW_MESSAGE   lstr_message

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


lstr_message = Message.PowerObjectParm

st_comment.Text = lstr_message.as_stringparm[1]

IF lstr_message.as_mode = 'REQUIRED' THEN
	ib_required = TRUE
END IF

IF UpperBound(lstr_message.al_doubleparm) > 0 THEN
	ii_min_number_of_chars = lstr_message.al_doubleparm[1]
END IF
end event

type st_comment from statictext within w_annuity_comment
integer x = 46
integer y = 20
integer width = 2190
integer height = 600
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Please enter a comment below"
boolean focusrectangle = false
end type

type cb_ok from commandbutton within w_annuity_comment
integer x = 1833
integer y = 1428
integer width = 402
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;STRING		ls_comment, ls_message, ls_clean_comment

ls_comment = mle_comment.Text

ls_clean_comment = f_Clean_String_1(ls_comment)

IF ls_clean_comment = '' AND ib_required THEN
	MessageBox('','You are required to enter a comment.')
	RETURN
END IF


// clear MLE just in case
mle_comment.Text = ''
mle_comment.Text = ls_clean_comment

IF ls_clean_comment <> ls_comment THEN
	IF MessageBox('Restricted Characters','The entered comment contains restricted characters. Each restricted '&
													+ 'character has been replaced. Do you want to see the resulting '&
													+ 'comment before it is saved?',Question!,YesNo!,1) = 1 THEN
		RETURN
	END IF
END IF

IF ii_min_number_of_chars > 0 THEN
	IF Len(ls_clean_comment) < ii_min_number_of_chars AND ls_clean_comment <> '' THEN
		MessageBox('Too Short','The comment must be at least five characters long. Please enter a longer comment.',Exclamation!)
		RETURN
	END IF
END IF

CloseWithReturn(Parent,ls_clean_comment)


end event

type mle_comment from multilineedit within w_annuity_comment
integer x = 46
integer y = 700
integer width = 2190
integer height = 656
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
integer limit = 256
borderstyle borderstyle = stylelowered!
end type

