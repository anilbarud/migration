$PBExportHeader$w_payment_message.srw
forward
global type w_payment_message from window
end type
type mle_message from multilineedit within w_payment_message
end type
type dw_op_list from u_dw_online within w_payment_message
end type
type cb_ok from commandbutton within w_payment_message
end type
end forward

global type w_payment_message from window
integer x = 1650
integer y = 636
integer width = 2213
integer height = 2152
boolean titlebar = true
string title = "Payment Messages"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
mle_message mle_message
dw_op_list dw_op_list
cb_ok cb_ok
end type
global w_payment_message w_payment_message

on w_payment_message.create
this.mle_message=create mle_message
this.dw_op_list=create dw_op_list
this.cb_ok=create cb_ok
this.Control[]={this.mle_message,&
this.dw_op_list,&
this.cb_ok}
end on

on w_payment_message.destroy
destroy(this.mle_message)
destroy(this.dw_op_list)
destroy(this.cb_ok)
end on

event open;LONG              ll_claim_no, ll_row
S_WINDOW_MESSAGE	lstr_message

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


lstr_message = Message.PowerObjectParm

mle_message.text = lstr_message.as_stringparm[1]

dw_op_list.SetTransObject(SQLCA)


ll_claim_no = lstr_message.al_doubleparm[2]
ll_row = dw_op_list.Retrieve(ll_claim_no)
	
IF ll_row > 0 THEN
	dw_op_list.visible = TRUE
END IF

end event

type mle_message from multilineedit within w_payment_message
integer x = 27
integer y = 36
integer width = 2149
integer height = 1328
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean border = false
boolean vscrollbar = true
end type

type dw_op_list from u_dw_online within w_payment_message
boolean visible = false
integer x = 27
integer y = 1396
integer width = 2149
integer height = 500
integer taborder = 10
string dataobject = "d_overpayment_message_list"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = false
borderstyle borderstyle = stylelowered!
end type

type cb_ok from commandbutton within w_payment_message
integer x = 1929
integer y = 1932
integer width = 247
integer height = 108
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "O&K"
end type

event clicked;SetPointer(HourGlass!)
Close(Parent)
end event

