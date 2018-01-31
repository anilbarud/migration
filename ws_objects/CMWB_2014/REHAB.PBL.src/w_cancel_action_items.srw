$PBExportHeader$w_cancel_action_items.srw
forward
global type w_cancel_action_items from w_ancestor
end type
type st_info_message from statictext within w_cancel_action_items
end type
type cb_2 from commandbutton within w_cancel_action_items
end type
type cb_1 from commandbutton within w_cancel_action_items
end type
type dw_cancel from u_dw_online within w_cancel_action_items
end type
end forward

global type w_cancel_action_items from w_ancestor
integer width = 3118
integer height = 1360
string title = "Cancel related action items"
string menuname = ""
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
long backcolor = 67108864
string icon = "Asterisk!"
boolean center = true
st_info_message st_info_message
cb_2 cb_2
cb_1 cb_1
dw_cancel dw_cancel
end type
global w_cancel_action_items w_cancel_action_items

event open;STRING								ls_info_message, ls_window_title, ls_title
S_WINDOW_MESSAGE		lstr_message
u_ds									lds_cancel


// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')

lstr_message 			= Message.PowerObjectParm
lds_cancel 				= lstr_message.apo_PowerObjectParm[1]
ls_info_message 	= lstr_message.as_stringparm[1]
ls_window_title 		= lstr_message.as_stringparm[2]

st_info_message.text 	= ls_info_message
this.title 						= ls_window_title

lds_cancel.ShareData(dw_cancel)





end event

on w_cancel_action_items.create
int iCurrent
call super::create
this.st_info_message=create st_info_message
this.cb_2=create cb_2
this.cb_1=create cb_1
this.dw_cancel=create dw_cancel
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_info_message
this.Control[iCurrent+2]=this.cb_2
this.Control[iCurrent+3]=this.cb_1
this.Control[iCurrent+4]=this.dw_cancel
end on

on w_cancel_action_items.destroy
call super::destroy
destroy(this.st_info_message)
destroy(this.cb_2)
destroy(this.cb_1)
destroy(this.dw_cancel)
end on

type st_info_message from statictext within w_cancel_action_items
integer x = 23
integer y = 24
integer width = 3058
integer height = 136
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Select items to be updated."
boolean focusrectangle = false
end type

type cb_2 from commandbutton within w_cancel_action_items
integer x = 2181
integer y = 1112
integer width = 402
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Continue"
end type

event clicked;CloseWithReturn(parent,1)
end event

type cb_1 from commandbutton within w_cancel_action_items
integer x = 2624
integer y = 1112
integer width = 402
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Cancel"
end type

event clicked;CloseWithReturn(parent,-1)
end event

type dw_cancel from u_dw_online within w_cancel_action_items
integer x = 9
integer y = 168
integer width = 3072
integer height = 904
integer taborder = 10
string dataobject = "d_rehab_related_outstanding_action_items"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event clicked;call super::clicked;STRING		ls_cancel_all_flag
STRING		ls_flag
INTEGER		li_x

SetRedraw(False)

If dwo.name = 'cancel_all_flag' Then
	ls_cancel_all_flag = this.GetITemString(1,'cancel_all_flag')
	
	If ls_cancel_all_flag = 'Y' THen
		ls_flag = 'N'
	Else
		ls_flag = 'Y'
	End if
	
	this.object.cancel_all_flag[1] = ls_Flag
	
	For li_x = 1 To this.RowCount()
		this.SetItem(li_x,'selection_flag',ls_flag)
	Next
	
End if


SetRedraw(True)
end event

event rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup
/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	
	lm_popup.m_options.PopMenu(w_cancel_action_items.PointerX(), w_cancel_action_items.PointerY())
	
	Destroy lm_popup
end event

