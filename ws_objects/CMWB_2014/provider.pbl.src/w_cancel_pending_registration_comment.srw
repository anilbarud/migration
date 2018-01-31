$PBExportHeader$w_cancel_pending_registration_comment.srw
forward
global type w_cancel_pending_registration_comment from window
end type
type st_3 from statictext within w_cancel_pending_registration_comment
end type
type dw_token_print from datawindow within w_cancel_pending_registration_comment
end type
type st_2 from statictext within w_cancel_pending_registration_comment
end type
type dw_cancel_comment from datawindow within w_cancel_pending_registration_comment
end type
type cb_2 from commandbutton within w_cancel_pending_registration_comment
end type
type cb_1 from commandbutton within w_cancel_pending_registration_comment
end type
end forward

global type w_cancel_pending_registration_comment from window
integer width = 2811
integer height = 1212
boolean titlebar = true
string title = "Cancel Entity Administrator Registration"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
st_3 st_3
dw_token_print dw_token_print
st_2 st_2
dw_cancel_comment dw_cancel_comment
cb_2 cb_2
cb_1 cb_1
end type
global w_cancel_pending_registration_comment w_cancel_pending_registration_comment

type variables
S_WINDOW_MESSAGE istr_message
end variables

on w_cancel_pending_registration_comment.create
this.st_3=create st_3
this.dw_token_print=create dw_token_print
this.st_2=create st_2
this.dw_cancel_comment=create dw_cancel_comment
this.cb_2=create cb_2
this.cb_1=create cb_1
this.Control[]={this.st_3,&
this.dw_token_print,&
this.st_2,&
this.dw_cancel_comment,&
this.cb_2,&
this.cb_1}
end on

on w_cancel_pending_registration_comment.destroy
destroy(this.st_3)
destroy(this.dw_token_print)
destroy(this.st_2)
destroy(this.dw_cancel_comment)
destroy(this.cb_2)
destroy(this.cb_1)
end on

event open;long ll_entity_no
string  ls_wif_entity_subtype_code,ls_entity_name


//grab the information that will be displayed on the screen from the structure
istr_message= Message.PowerObjectParm


ll_entity_no = istr_message.al_doubleparm[1]
ls_entity_name  = istr_message.as_stringparm[2]

//show the text on the screen
st_2.text =  istr_message.as_stringparm[2] + " - Provider Number  " + string(istr_message.al_doubleparm[1])
st_3.text = ' Registration letter was produced on ' + istr_message.as_stringparm[3]

//insert an mepty row to collect the cancelled comment
dw_cancel_comment.insertrow(0)
end event

type st_3 from statictext within w_cancel_pending_registration_comment
integer x = 41
integer y = 180
integer width = 2651
integer height = 84
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

type dw_token_print from datawindow within w_cancel_pending_registration_comment
boolean visible = false
integer x = 2322
integer y = 1288
integer width = 686
integer height = 400
integer taborder = 20
string title = "none"
string dataobject = "d_token_print"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type st_2 from statictext within w_cancel_pending_registration_comment
integer x = 41
integer y = 52
integer width = 2651
integer height = 84
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
alignment alignment = center!
boolean focusrectangle = false
end type

type dw_cancel_comment from datawindow within w_cancel_pending_registration_comment
integer x = 64
integer y = 452
integer width = 2569
integer height = 380
integer taborder = 10
string title = "none"
string dataobject = "d_cancel_comment"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type cb_2 from commandbutton within w_cancel_pending_registration_comment
integer x = 1650
integer y = 944
integer width = 402
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = " Close"
end type

event clicked;close(PARENT)
end event

type cb_1 from commandbutton within w_cancel_pending_registration_comment
integer x = 567
integer y = 940
integer width = 955
integer height = 104
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cancel Pending Registration"
end type

event clicked;string ls_token_id
integer li_rc
datetime ldtm_current_datetime
string ls_cancelled_comment
long ll_len



//7.10	 An invite token must be pending to be cancelled.
//7.20	 A cancelled comment must be supplied to cancel a token.


dw_cancel_comment.accepttext()

IF dw_cancel_comment.rowcount() < 1 THEN RETURN

ls_cancelled_comment = dw_cancel_comment.getitemstring(1,'cancelled_comment')
if (isnull(ls_cancelled_comment)) then
	messagebox('Information', 'You must provide a Cancelled Comment when cancelling an Invite token',EXCLAMATION!)
	RETURN -1
end if


ll_len = LEN(ls_cancelled_comment)
if(ll_len < 5) then
	messagebox('Information', 'You must enter at least 5 characters into the Cancelled Comment!',EXCLAMATION!)
	RETURN -1
end if

if(ll_len >200)  then
	messagebox('Information', 'You can not enter more than 200 chracters into a Cancelled Comment !',EXCLAMATION!)
	RETURN -1
end if

ls_token_id = istr_message.as_stringparm[1]
ldtm_current_datetime = f_server_datetime()


sqlca.nf_begin_transaction()

Update WORKFLOW_TOKEN
set cancelled_datetime = :ldtm_current_datetime,
	cancelled_comment = :ls_cancelled_comment
where token_id = :ls_token_id
USING sqlca;

sqlca.nf_commit_transaction()

 SQLCA.nf_handle_error('w_cancel_comment','cb_cancel_invite.clicked','Update WORKFLOW_TOKEN')


CloseWithReturn(Parent,1)


end event

