$PBExportHeader$w_auth_info_popup.srw
$PBExportComments$Used to give basic information for authorizations
forward
global type w_auth_info_popup from window
end type
type cb_print from commandbutton within w_auth_info_popup
end type
type cb_close from commandbutton within w_auth_info_popup
end type
type dw_auth_info from u_dw_online within w_auth_info_popup
end type
end forward

global type w_auth_info_popup from window
integer x = 2002
integer y = 1000
integer width = 3182
integer height = 1148
boolean titlebar = true
string title = "Authorization Information"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean clientedge = true
cb_print cb_print
cb_close cb_close
dw_auth_info dw_auth_info
end type
global w_auth_info_popup w_auth_info_popup

type variables
s_window_message istr_window_message

ULONG		iul_handle
end variables

event open;LONG			ll_claim_no, ll_task_no

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


/*	grab the passed value into the datastore */
istr_window_message 	= Message.PowerObjectParm

ll_claim_no 		= istr_window_message.al_doubleparm[1]
ll_task_no			= istr_window_message.al_doubleparm[2]

dw_auth_info.retrieve(ll_claim_no, ll_task_no)
SQLCA.nf_handle_error('w_auth_info_popup','Open','dw_auth_info.retrieve(ll_claim_no, ll_task_no)')

//select the first row
dw_auth_info.selectrow(0,FALSE)

//NOW MAKE SURE THIS ROW IS SELECTED
dw_auth_info.SelectRow(1, TRUE)

end event

on w_auth_info_popup.create
this.cb_print=create cb_print
this.cb_close=create cb_close
this.dw_auth_info=create dw_auth_info
this.Control[]={this.cb_print,&
this.cb_close,&
this.dw_auth_info}
end on

on w_auth_info_popup.destroy
destroy(this.cb_print)
destroy(this.cb_close)
destroy(this.dw_auth_info)
end on

type cb_print from commandbutton within w_auth_info_popup
integer x = 2514
integer y = 956
integer width = 283
integer height = 84
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Print"
end type

event clicked;IF dw_auth_info.rowcount() > 0 THEN 
	PRINT(dw_auth_info)
END IF 
end event

type cb_close from commandbutton within w_auth_info_popup
integer x = 2802
integer y = 956
integer width = 283
integer height = 84
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;close(parent)


end event

type dw_auth_info from u_dw_online within w_auth_info_popup
integer y = 4
integer width = 3136
integer height = 944
integer taborder = 10
string dataobject = "d_auth_info_popup"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;THIS.settransobject(sqlca)
THIS.uf_setselect(1)

end event

event rowfocuschanged;call super::rowfocuschanged;//MAKE SURE IT IS A valid row.
IF currentrow <= 0 THEN RETURN

//remove select from all other rows
THIS.selectrow(0,FALSE)

//NOW MAKE SURE THIS ROW IS SELECTED
This.SelectRow(currentrow, true)


end event

event rbuttondown;return 1
end event

