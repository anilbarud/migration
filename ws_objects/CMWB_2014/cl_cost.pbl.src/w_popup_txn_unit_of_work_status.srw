$PBExportHeader$w_popup_txn_unit_of_work_status.srw
forward
global type w_popup_txn_unit_of_work_status from window
end type
type cb_save from commandbutton within w_popup_txn_unit_of_work_status
end type
type cb_close from commandbutton within w_popup_txn_unit_of_work_status
end type
type dw_txn_unit_of_work_status from u_dw_online within w_popup_txn_unit_of_work_status
end type
end forward

global type w_popup_txn_unit_of_work_status from window
integer x = 2002
integer y = 1000
integer width = 2455
integer height = 1120
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
cb_save cb_save
cb_close cb_close
dw_txn_unit_of_work_status dw_txn_unit_of_work_status
end type
global w_popup_txn_unit_of_work_status w_popup_txn_unit_of_work_status

type variables
s_window_message istr_window_message

string is_window_name = "w_popup_add_txn_unit_of_work_no"
end variables

event open;long  ll_rows
string ls_script = "open"

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


dw_txn_unit_of_work_status.settransobject(SQLCA)
		
		
ll_rows = dw_txn_unit_of_work_status.RETRIEVE(vgst_user_profile.work_group_code)
SQLCA.nf_handle_error("ldw_txn_unit_of_work_status.retrieve()",is_window_name,ls_script)




end event

on w_popup_txn_unit_of_work_status.create
this.cb_save=create cb_save
this.cb_close=create cb_close
this.dw_txn_unit_of_work_status=create dw_txn_unit_of_work_status
this.Control[]={this.cb_save,&
this.cb_close,&
this.dw_txn_unit_of_work_status}
end on

on w_popup_txn_unit_of_work_status.destroy
destroy(this.cb_save)
destroy(this.cb_close)
destroy(this.dw_txn_unit_of_work_status)
end on

type cb_save from commandbutton within w_popup_txn_unit_of_work_status
integer x = 1499
integer y = 896
integer width = 402
integer height = 104
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Save"
end type

event clicked;string ls_script = 'cb_save'
long ll_rows


SQLCA.nf_begin_transaction()

dw_txn_unit_of_work_status.update()
SQLCA.nf_handle_error("ds_rehab_task_authorization.update()",is_window_name,ls_script)	

SQLCA.nf_commit_transaction()

		
ll_rows = dw_txn_unit_of_work_status.RETRIEVE(vgst_user_profile.work_group_code)

SQLCA.nf_handle_error("ldw_txn_unit_of_work_status.retrieve()",is_window_name,ls_script)




end event

type cb_close from commandbutton within w_popup_txn_unit_of_work_status
integer x = 1938
integer y = 896
integer width = 402
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;close(w_popup_txn_unit_of_work_status)
end event

type dw_txn_unit_of_work_status from u_dw_online within w_popup_txn_unit_of_work_status
integer x = 37
integer y = 64
integer width = 2341
integer height = 768
integer taborder = 10
string dataobject = "d_txn_unit_of_work_status"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

