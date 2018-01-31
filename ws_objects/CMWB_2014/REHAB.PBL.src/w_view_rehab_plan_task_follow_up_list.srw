$PBExportHeader$w_view_rehab_plan_task_follow_up_list.srw
forward
global type w_view_rehab_plan_task_follow_up_list from w_ancestor
end type
type dw_view_rehab_plan_task_follow_up_list from u_dw_online within w_view_rehab_plan_task_follow_up_list
end type
type cb_ok from commandbutton within w_view_rehab_plan_task_follow_up_list
end type
end forward

global type w_view_rehab_plan_task_follow_up_list from w_ancestor
integer x = 1289
integer y = 312
integer width = 2715
integer height = 1252
string title = "Rehab Plan Task Follow-up List"
string menuname = ""
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
long backcolor = 67108864
string icon = "Information!"
boolean center = true
dw_view_rehab_plan_task_follow_up_list dw_view_rehab_plan_task_follow_up_list
cb_ok cb_ok
end type
global w_view_rehab_plan_task_follow_up_list w_view_rehab_plan_task_follow_up_list

event open;call super::open;S_WINDOW_MESSAGE	lstr_message
	LONG		ll_claim_no, ll_task_no
	STRING 	ls_filter_string 
	LONG li_return
	
	dw_view_rehab_plan_task_follow_up_list.SetTransObject(SQLCA)
	dw_view_rehab_plan_task_follow_up_list.uf_setselect(1)
	
	lstr_message = Message.PowerObjectParm
	ll_claim_no = lstr_message.al_doubleparm[1]
	ll_task_no = lstr_message.al_doubleparm[2]
	ls_filter_string=lstr_message.as_stringparm[1]
	
	SetPointer(HourGlass!)
	
	li_return = dw_view_rehab_plan_task_follow_up_list.Retrieve(ll_claim_no,ll_task_no)
	
	IF SQLCA.nf_Handle_Error("w_view_rehab_plan_attachments","dw_rehab_plan_view_task_follow_up_list.Retrieve(ll_claim_no,ll_task_no)","Open Event")	< 0 THEN
		lstr_message.al_doubleparm[3] = -1
		CloseWithReturn(THIS,lstr_message)
	END IF
	
	//If there rows retrieved thenfilter the data displayed using the filter string 
	//passed in through the message paramater
	if li_return>0 then
		dw_view_rehab_plan_task_follow_up_list.SetFilter(ls_filter_string)
		dw_view_rehab_plan_task_follow_up_list.Filter()
	end if
end event

on w_view_rehab_plan_task_follow_up_list.create
int iCurrent
call super::create
this.dw_view_rehab_plan_task_follow_up_list=create dw_view_rehab_plan_task_follow_up_list
this.cb_ok=create cb_ok
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_view_rehab_plan_task_follow_up_list
this.Control[iCurrent+2]=this.cb_ok
end on

on w_view_rehab_plan_task_follow_up_list.destroy
call super::destroy
destroy(this.dw_view_rehab_plan_task_follow_up_list)
destroy(this.cb_ok)
end on

type dw_view_rehab_plan_task_follow_up_list from u_dw_online within w_view_rehab_plan_task_follow_up_list
integer x = 27
integer y = 28
integer width = 2633
integer height = 968
integer taborder = 10
string dataobject = "d_view_rehab_plan_task_follow_up_list"
boolean vscrollbar = true
string icon = "Information!"
borderstyle borderstyle = stylelowered!
end type

type cb_ok from commandbutton within w_view_rehab_plan_task_follow_up_list
integer x = 2249
integer y = 1028
integer width = 334
integer height = 100
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

event clicked;	S_WINDOW_MESSAGE	lstr_message
	lstr_message.al_doubleparm[3] = 1
	CloseWithReturn(w_view_rehab_plan_task_follow_up_list,lstr_message)
end event

