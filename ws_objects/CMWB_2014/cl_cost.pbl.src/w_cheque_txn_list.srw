$PBExportHeader$w_cheque_txn_list.srw
forward
global type w_cheque_txn_list from window
end type
type mle_message from multilineedit within w_cheque_txn_list
end type
type p_1 from picture within w_cheque_txn_list
end type
type cb_print from commandbutton within w_cheque_txn_list
end type
type dw_cheque_txn_list from u_dw_online within w_cheque_txn_list
end type
end forward

global type w_cheque_txn_list from window
integer width = 3168
integer height = 1624
boolean titlebar = true
string title = "Cheque transactions"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
mle_message mle_message
p_1 p_1
cb_print cb_print
dw_cheque_txn_list dw_cheque_txn_list
end type
global w_cheque_txn_list w_cheque_txn_list

on w_cheque_txn_list.create
this.mle_message=create mle_message
this.p_1=create p_1
this.cb_print=create cb_print
this.dw_cheque_txn_list=create dw_cheque_txn_list
this.Control[]={this.mle_message,&
this.p_1,&
this.cb_print,&
this.dw_cheque_txn_list}
end on

on w_cheque_txn_list.destroy
destroy(this.mle_message)
destroy(this.p_1)
destroy(this.cb_print)
destroy(this.dw_cheque_txn_list)
end on

event open;S_WINDOW_MESSAGE		ls_window_message
u_ds						lds_cheque_txn_list
STRING					ls_message_text

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


ls_window_message = MESSAGE.PowerObjectParm

ls_message_text = ls_Window_Message.as_StringParm[1]

lds_cheque_txn_list = ls_Window_Message.apo_PowerObjectParm[1]

mle_message.Text = ls_message_text

lds_cheque_txn_list.sharedata(dw_cheque_txn_list)
end event

type mle_message from multilineedit within w_cheque_txn_list
integer x = 59
integer y = 536
integer width = 974
integer height = 844
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
end type

type p_1 from picture within w_cheque_txn_list
integer x = 59
integer y = 64
integer width = 978
integer height = 400
string picturename = "cheque.gif"
boolean focusrectangle = false
end type

type cb_print from commandbutton within w_cheque_txn_list
integer x = 2533
integer y = 1412
integer width = 521
integer height = 96
integer taborder = 20
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Print and  &Close"
end type

event clicked;dw_cheque_txn_list.Print()
Close(Parent)
end event

type dw_cheque_txn_list from u_dw_online within w_cheque_txn_list
integer x = 1083
integer y = 68
integer width = 2011
integer height = 1316
integer taborder = 10
string dataobject = "d_cheque_txn_list"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

