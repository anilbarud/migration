$PBExportHeader$w_restriction_history.srw
$PBExportComments$A response window showing the formulary history
forward
global type w_restriction_history from window
end type
type dw_1 from u_dw_online within w_restriction_history
end type
type cb_1 from commandbutton within w_restriction_history
end type
end forward

global type w_restriction_history from window
integer width = 3337
integer height = 1208
boolean titlebar = true
string title = "Rx Special Authorization Restriction History"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
dw_1 dw_1
cb_1 cb_1
end type
global w_restriction_history w_restriction_history

on w_restriction_history.create
this.dw_1=create dw_1
this.cb_1=create cb_1
this.Control[]={this.dw_1,&
this.cb_1}
end on

on w_restriction_history.destroy
destroy(this.dw_1)
destroy(this.cb_1)
end on

event open;LONG ll_claim_no

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


ll_claim_no = Message.doubleParm

/* CHECK THAT THE CLAIM NUMBER IS VALID */
IF ISNULL(ll_claim_no) OR ll_claim_no < 1  THEN
	CLOSE(THIS)
	RETURN
END IF

/* RETRIEVE THE CLAIM FORMULARY HISTORY INFORMATION */
dw_1.settransobject(sqlca)
dw_1.Retrieve(ll_claim_no)
SQLCA.nf_handle_error('w_restriction_history','open','dw_1.Retrieve(ll_claim_no)') 










		



end event

type dw_1 from u_dw_online within w_restriction_history
integer x = 5
integer width = 3310
integer height = 1016
integer taborder = 30
string dataobject = "d_restriction_history"
boolean hscrollbar = true
boolean vscrollbar = true
boolean border = false
end type

type cb_1 from commandbutton within w_restriction_history
integer x = 3049
integer y = 1024
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

