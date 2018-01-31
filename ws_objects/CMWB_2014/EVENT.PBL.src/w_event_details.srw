$PBExportHeader$w_event_details.srw
$PBExportComments$Window displaying event details such as all claim status changes
forward
global type w_event_details from window
end type
type cb_close from commandbutton within w_event_details
end type
type dw_event_details from u_dw_online within w_event_details
end type
end forward

global type w_event_details from window
integer x = 1335
integer y = 688
integer width = 2537
integer height = 1556
boolean titlebar = true
string title = "Event Details"
windowtype windowtype = response!
long backcolor = 67108864
cb_close cb_close
dw_event_details dw_event_details
end type
global w_event_details w_event_details

type variables
s_window_message istr_window_message
end variables

event open;	STRING ls_event_type
	LONG   ll_claim_no,	ll_event_no,	ll_rownum

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


/* Get the claim number and event number
*/
	istr_window_message = Message.PowerObjectParm
	ls_event_type = istr_window_message.as_stringparm[1]
	ll_claim_no   = istr_window_message.al_doubleparm[1]
	ll_event_no   = istr_window_message.al_doubleparm[2]

	IF ls_event_type = "010" THEN
		dw_event_details.DataObject = 'd_event_status_change'
		dw_event_details.uf_setselect(1)
	ELSEIF ls_event_type = "014" THEN
		dw_event_details.DataObject = 'd_event_cost_alloc_change'
		dw_event_details.uf_setselect(1)
	END IF

	dw_event_details.SetTransObject(SQLCA)
	dw_event_details.Retrieve(ll_claim_no)
	IF SQLCA.nf_handle_error("w_event_details","dw_event_details","Open") < 0 THEN
		Close(This)
		Return
	END IF

/* Highlight the specific event in the datawindow
*/
	ll_rownum = dw_event_details.Find("event_no = " + String(ll_event_no),1,dw_event_details.RowCount())
	If ll_rownum > 0 Then
		dw_event_details.ScrollToRow(ll_rownum)
		dw_event_details.uf_processselect(ll_rownum,"Mouse")
	End If
end event

on w_event_details.create
this.cb_close=create cb_close
this.dw_event_details=create dw_event_details
this.Control[]={this.cb_close,&
this.dw_event_details}
end on

on w_event_details.destroy
destroy(this.cb_close)
destroy(this.dw_event_details)
end on

type cb_close from commandbutton within w_event_details
integer x = 1047
integer y = 1312
integer width = 311
integer height = 108
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
boolean default = true
end type

on clicked;Close(Parent)
end on

type dw_event_details from u_dw_online within w_event_details
integer x = 23
integer y = 52
integer width = 2437
integer height = 1236
integer taborder = 10
string dataobject = "d_event_status_change"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

on rbuttondown;/*	
	create the menu - override the ancestor here as the parent is a response window
	and a different pointer value needs to be passed to the PopMenu function
*/
	M_DW_ONLINE_RMB_POPUP lm_popup


	lm_popup = Create m_dw_online_rmb_popup

	lm_popup.mf_set_datawindow(This)
	lm_popup.m_options.PopMenu(Parent.PointerX( ), Parent.PointerY( ))

	Destroy lm_popup



end on

