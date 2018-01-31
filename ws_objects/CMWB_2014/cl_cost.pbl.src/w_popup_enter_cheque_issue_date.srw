$PBExportHeader$w_popup_enter_cheque_issue_date.srw
forward
global type w_popup_enter_cheque_issue_date from window
end type
type st_1 from statictext within w_popup_enter_cheque_issue_date
end type
type dw_cheque_issue_date from u_dw_online within w_popup_enter_cheque_issue_date
end type
type cb_accept from commandbutton within w_popup_enter_cheque_issue_date
end type
end forward

global type w_popup_enter_cheque_issue_date from window
integer x = 2002
integer y = 1000
integer width = 1367
integer height = 812
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
st_1 st_1
dw_cheque_issue_date dw_cheque_issue_date
cb_accept cb_accept
end type
global w_popup_enter_cheque_issue_date w_popup_enter_cheque_issue_date

type variables
s_window_message istr_window_message

string is_window_name = "w_popup_add_txn_unit_of_work_no"


end variables

event open;long 		ll_rows
long		ll_rc
long 		ll_cheque_no 

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


istr_window_message = Message.PowerObjectParm
ll_cheque_no =  istr_window_message.al_doubleparm[1] 

dw_cheque_issue_date.SetTransObject(SQLCA)

ll_rows = dw_cheque_issue_date.retrieve(ll_cheque_no)
dw_cheque_issue_date.selectrow(1,true)


end event

on w_popup_enter_cheque_issue_date.create
this.st_1=create st_1
this.dw_cheque_issue_date=create dw_cheque_issue_date
this.cb_accept=create cb_accept
this.Control[]={this.st_1,&
this.dw_cheque_issue_date,&
this.cb_accept}
end on

on w_popup_enter_cheque_issue_date.destroy
destroy(this.st_1)
destroy(this.dw_cheque_issue_date)
destroy(this.cb_accept)
end on

type st_1 from statictext within w_popup_enter_cheque_issue_date
integer x = 37
integer y = 64
integer width = 1321
integer height = 64
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "The following issue date~'s have been detected"
boolean focusrectangle = false
end type

type dw_cheque_issue_date from u_dw_online within w_popup_enter_cheque_issue_date
integer x = 110
integer y = 192
integer width = 658
integer height = 384
integer taborder = 10
string title = "none"
string dataobject = "ds_cheque_issue_date"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;selectrow(0,false)
selectrow(currentrow,true)

end event

type cb_accept from commandbutton within w_popup_enter_cheque_issue_date
integer x = 878
integer y = 640
integer width = 402
integer height = 104
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Accept"
end type

event clicked;long ll_row
datetime ldtm_cheque_issue_date



ll_row = dw_cheque_issue_date.getrow()

ldtm_cheque_issue_date = dw_cheque_issue_date.getitemdatetime(ll_row,'cheque_deposit_date')
	
istr_window_message.adtm_datetimeparm[1] = ldtm_cheque_issue_date 
closewithreturn(parent,istr_window_message)
end event

