$PBExportHeader$w_rejected_license_info.srw
forward
global type w_rejected_license_info from window
end type
type dw_info from datawindow within w_rejected_license_info
end type
type cb_close from commandbutton within w_rejected_license_info
end type
end forward

global type w_rejected_license_info from window
integer x = 2002
integer y = 1000
integer width = 3936
integer height = 724
boolean titlebar = true
string title = "Rejected License Info"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean clientedge = true
dw_info dw_info
cb_close cb_close
end type
global w_rejected_license_info w_rejected_license_info

type variables
s_window_message istr_window_message

ULONG		iul_handle
end variables

event open;STRING			ls_message
INT 				li_trancount
LONG				ll_principal_id

s_window_message 	lstr_message 



// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')

/*	grab the passed value into the datastore */
lstr_message= Message.PowerObjectParm

ll_principal_id 	= lstr_message.al_doubleparm[1]


dw_info.retrieve(ll_principal_id)
SQLCA.nf_handle_error("w_rejected_license_info","open","dw_info.retrieve(ll_principal_id)")





end event

on w_rejected_license_info.create
this.dw_info=create dw_info
this.cb_close=create cb_close
this.Control[]={this.dw_info,&
this.cb_close}
end on

on w_rejected_license_info.destroy
destroy(this.dw_info)
destroy(this.cb_close)
end on

type dw_info from datawindow within w_rejected_license_info
integer x = 14
integer y = 16
integer width = 3877
integer height = 492
integer taborder = 10
string title = "none"
string dataobject = "d_rejected_licenses"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
end type

event constructor;this.settransobject(sqlca)
end event

type cb_close from commandbutton within w_rejected_license_info
integer x = 3598
integer y = 528
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
boolean default = true
end type

event clicked;close(parent)



end event

event rowfocuschanged;call super::rowfocuschanged;//MAKE SURE IT IS A valid row.

end event

event rbuttondown;return 1
end event

