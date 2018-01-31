$PBExportHeader$w_popup_add_txn_unit_work_no.srw
forward
global type w_popup_add_txn_unit_work_no from window
end type
type st_2 from statictext within w_popup_add_txn_unit_work_no
end type
type cb_new from commandbutton within w_popup_add_txn_unit_work_no
end type
type st_1 from statictext within w_popup_add_txn_unit_work_no
end type
type dw_unapplied_txn_unit_of_work from u_dw_online within w_popup_add_txn_unit_work_no
end type
type cb_accept from commandbutton within w_popup_add_txn_unit_work_no
end type
end forward

global type w_popup_add_txn_unit_work_no from window
integer x = 2002
integer y = 1000
integer width = 2405
integer height = 812
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
st_2 st_2
cb_new cb_new
st_1 st_1
dw_unapplied_txn_unit_of_work dw_unapplied_txn_unit_of_work
cb_accept cb_accept
end type
global w_popup_add_txn_unit_work_no w_popup_add_txn_unit_work_no

type variables
s_window_message istr_window_message

string is_window_name = "w_popup_add_txn_unit_of_work_no"
end variables

event open;string ls_txn_type_code
string ls_txn_sub_type_code
long ll_row, ll_rows
string ls_script = "open"

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


istr_window_message = Message.PowerObjectParm

ls_txn_type_code = istr_window_message.as_stringparm[1]
ls_txn_sub_type_code = istr_window_message.as_stringparm[2]
	
dw_unapplied_txn_unit_of_work.settransobject(SQLCA)
		
	
st_1.text = st_1.text + " for " + ls_txn_type_code + "-" + ls_txn_sub_type_code
		
		
ll_rows = dw_unapplied_txn_unit_of_work.RETRIEVE(ls_txn_type_code,ls_txn_sub_type_code,vgst_user_profile.work_group_code)
SQLCA.nf_handle_error("ldwc_txn_type_code.Retrieve()",is_window_name,ls_script)




end event

on w_popup_add_txn_unit_work_no.create
this.st_2=create st_2
this.cb_new=create cb_new
this.st_1=create st_1
this.dw_unapplied_txn_unit_of_work=create dw_unapplied_txn_unit_of_work
this.cb_accept=create cb_accept
this.Control[]={this.st_2,&
this.cb_new,&
this.st_1,&
this.dw_unapplied_txn_unit_of_work,&
this.cb_accept}
end on

on w_popup_add_txn_unit_work_no.destroy
destroy(this.st_2)
destroy(this.cb_new)
destroy(this.st_1)
destroy(this.dw_unapplied_txn_unit_of_work)
destroy(this.cb_accept)
end on

type st_2 from statictext within w_popup_add_txn_unit_work_no
integer x = 37
integer y = 256
integer width = 402
integer height = 256
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long textcolor = 255
long backcolor = 67108864
string text = "Please click the ~"Accept~" or  ~"Add New~" ."
boolean focusrectangle = false
end type

type cb_new from commandbutton within w_popup_add_txn_unit_work_no
integer x = 1792
integer y = 672
integer width = 402
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Add &New"
boolean default = true
end type

event clicked;istr_window_message.as_stringparm[3] = "0"

closewithreturn(parent,istr_window_message)
end event

type st_1 from statictext within w_popup_add_txn_unit_work_no
integer y = 32
integer width = 2158
integer height = 64
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "The following ~'unit of work~' have been detected."
boolean focusrectangle = false
end type

type dw_unapplied_txn_unit_of_work from u_dw_online within w_popup_add_txn_unit_work_no
integer x = 549
integer y = 160
integer width = 1719
integer height = 480
integer taborder = 10
string title = "none"
string dataobject = "ds_unapplied_txn_unit_of_work"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;this.selectrow(0,false)
this.selectrow(currentrow,true)
end event

type cb_accept from commandbutton within w_popup_add_txn_unit_work_no
integer x = 1317
integer y = 672
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
long ll_txn_unit_of_work_no

ll_row = dw_unapplied_txn_unit_of_work.getrow()

ll_txn_unit_of_work_no = dw_unapplied_txn_unit_of_work.getitemnumber(ll_row,'txn_unit_of_work_no')
istr_window_message.as_stringparm[3] = string(ll_txn_unit_of_work_no)

closewithreturn(parent,istr_window_message)
end event

