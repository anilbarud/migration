$PBExportHeader$w_full_event_comments.srw
$PBExportComments$A response window showing the FULL event comments
forward
global type w_full_event_comments from window
end type
type cb_print from commandbutton within w_full_event_comments
end type
type cb_close from commandbutton within w_full_event_comments
end type
type dw_event_comment from u_dw_online within w_full_event_comments
end type
end forward

global type w_full_event_comments from window
integer width = 3246
integer height = 1312
boolean titlebar = true
string title = "Full Event Comments"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
cb_print cb_print
cb_close cb_close
dw_event_comment dw_event_comment
end type
global w_full_event_comments w_full_event_comments

on w_full_event_comments.create
this.cb_print=create cb_print
this.cb_close=create cb_close
this.dw_event_comment=create dw_event_comment
this.Control[]={this.cb_print,&
this.cb_close,&
this.dw_event_comment}
end on

on w_full_event_comments.destroy
destroy(this.cb_print)
destroy(this.cb_close)
destroy(this.dw_event_comment)
end on

event open;LONG   ll_claim_no, ll_event_no, ll_rtn
S_WINDOW_MESSAGE	lstr_message
STRING ls_type
BOOLEAN lb_drug_alert

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


/*	get the type to search for */
lstr_message = Message.PowerObjectParm
ls_type = lstr_message.as_stringparm[1]
ll_claim_no = lstr_message.al_doubleparm[1]
ll_event_no = lstr_message.al_doubleparm[2]
	
IF ISNULL(ll_claim_no) OR ll_claim_no < 1 THEN RETURN
IF ISNULL(ll_event_no) OR ll_event_no < 1 THEN RETURN

IF ls_type = 'T' THEN
	dw_event_comment.DataObject = 'd_terminated_comments'
	lb_drug_alert = TRUE
ELSEIF ls_type = 'N' THEN
	dw_event_comment.DataObject = 'd_drug_alert_comments'
	lb_drug_alert = TRUE
END IF

/* RETRIEVE THE COMMENT INFORMATION INTO THE WINDOW */
dw_event_comment.settransobject(sqlca)
ll_rtn = dw_event_comment.retrieve(ll_claim_no,ll_event_no)

IF ll_rtn < 1 THEN
	IF lb_drug_alert THEN
		MESSAGEBOX("No Alert Information", "No Drug Alert Information was returned for Drug Alert #: " + STRING(ll_event_no))
	ELSE
		MESSAGEBOX("No Event Information", "No Event Information was returned for Claim #: " + string(ll_claim_no) + " And Event #: " + string(ll_event_no))
	END IF
END IF 



end event

type cb_print from commandbutton within w_full_event_comments
integer x = 2661
integer y = 1128
integer width = 256
integer height = 96
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Print"
end type

event clicked;dw_event_comment.Object.DataWindow.Print.orientation = 1
dw_event_comment.print()
end event

type cb_close from commandbutton within w_full_event_comments
integer x = 2958
integer y = 1128
integer width = 256
integer height = 96
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;CLOSE(PARENT)
end event

type dw_event_comment from u_dw_online within w_full_event_comments
integer x = 5
integer y = 4
integer width = 3209
integer height = 1108
integer taborder = 10
string dataobject = "d_full_event_comments"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = false
end type

