$PBExportHeader$w_rehab_task_auth_info_change.srw
$PBExportComments$A response window showing the formulary history
forward
global type w_rehab_task_auth_info_change from window
end type
type dw_report from u_dw_online within w_rehab_task_auth_info_change
end type
type cb_1 from commandbutton within w_rehab_task_auth_info_change
end type
end forward

global type w_rehab_task_auth_info_change from window
integer width = 3497
integer height = 1044
boolean titlebar = true
string title = "Authorization Revision History"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
dw_report dw_report
cb_1 cb_1
end type
global w_rehab_task_auth_info_change w_rehab_task_auth_info_change

on w_rehab_task_auth_info_change.create
this.dw_report=create dw_report
this.cb_1=create cb_1
this.Control[]={this.dw_report,&
this.cb_1}
end on

on w_rehab_task_auth_info_change.destroy
destroy(this.dw_report)
destroy(this.cb_1)
end on

event open;LONG ll_authorization_no

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


ll_authorization_no = Message.doubleParm

/* CHECK THAT THE CLAIM NUMBER IS VALID */
IF ISNULL(ll_authorization_no) OR ll_authorization_no < 1  THEN
	CLOSE(THIS)
	RETURN
END IF

/* RETRIEVE THE CLAIM FORMULARY HISTORY INFORMATION */
dw_report.settransobject(sqlca)
dw_report.Retrieve(ll_authorization_no)
SQLCA.nf_handle_error('w_rehab_task_auth_info_change','open','dw_report.Retrieve(ll_authorization_no)') 










		



end event

type dw_report from u_dw_online within w_rehab_task_auth_info_change
integer x = 5
integer width = 3470
integer height = 840
integer taborder = 30
string dataobject = "d_rehab_task_auth_info_change"
boolean hscrollbar = true
boolean vscrollbar = true
boolean border = false
end type

event rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup

/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	
	lm_popup.m_options.PopMenu(parent.PointerX( ), parent.PointerY( ))

	Destroy lm_popup
end event

type cb_1 from commandbutton within w_rehab_task_auth_info_change
integer x = 3218
integer y = 848
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

