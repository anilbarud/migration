$PBExportHeader$w_event_log_response.srw
$PBExportComments$Window to Display and Add Claim Events
forward
global type w_event_log_response from window
end type
type cb_add_event from commandbutton within w_event_log_response
end type
type cb_send from commandbutton within w_event_log_response
end type
type cb_save from commandbutton within w_event_log_response
end type
type cb_cancel from commandbutton within w_event_log_response
end type
type uo_event_log from u_event_log within w_event_log_response
end type
type st_1 from statictext within w_event_log_response
end type
type cb_close from commandbutton within w_event_log_response
end type
end forward

global type w_event_log_response from window
integer x = 23
integer y = 376
integer width = 3282
integer height = 1900
boolean titlebar = true
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
cb_add_event cb_add_event
cb_send cb_send
cb_save cb_save
cb_cancel cb_cancel
uo_event_log uo_event_log
st_1 st_1
cb_close cb_close
end type
global w_event_log_response w_event_log_response

type variables
STRING		is_event_category_code
end variables

event open;LONG							ll_claim_no
S_WINDOW_MESSAGE	lstr_message
INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


lstr_message = Message.PowerObjectParm

/*
lstr_message.al_doubleparm[1] = al_claim_no
lstr_message.al_doubleparm[2] = al_individual_no
lstr_message.al_doubleparm[3] = event_no

lstr_message.as_StringParm[1] = as_claim_role_code
lstr_message.as_StringParm[2] = as_last_name
lstr_message.as_StringParm[3] = as_given_names
lstr_message.as_StringParm[4] = as_event_category_code
lstr_message.as_StringParm[5] = as_event_type_code
lstr_message.as_StringParm[6] = as_event_specific_code
lstr_message.as_StringParm[7] = as_allow_parameter_change
lstr_message.as_StringParm[8] = as_add_new_event
lstr_message.as_StringParm[9] = message
lstr_message.as_StringParm[10] = find event
lstr_message.as_StringParm[11] = as_search_event_category_code
*/

uo_event_log.uf_populate_variables(lstr_message)

IF lstr_message.as_StringParm[8] = 'Y' THEN // add new event
	IF lstr_message.as_StringParm[4] = 'I' THEN
		// individual event
		cb_add_event.TriggerEvent(Clicked!)
		uo_event_log.uf_set_event_category_code('I')
	ELSEIF lstr_message.as_StringParm[4] = 'C' THEN
		// claim event
		cb_add_event.TriggerEvent(Clicked!)
		uo_event_log.uf_set_event_category_code('C')
	END IF
END IF

ll_claim_no = lstr_message.al_DoubleParm[1]
IF ll_claim_no = 0 THEN
	cb_add_event.Enabled = FALSE
ELSE
	THIS.title = 'Claim No: ' + String(ll_claim_no)
END IF

uo_event_log.uf_set_window(THIS)

end event

on w_event_log_response.create
this.cb_add_event=create cb_add_event
this.cb_send=create cb_send
this.cb_save=create cb_save
this.cb_cancel=create cb_cancel
this.uo_event_log=create uo_event_log
this.st_1=create st_1
this.cb_close=create cb_close
this.Control[]={this.cb_add_event,&
this.cb_send,&
this.cb_save,&
this.cb_cancel,&
this.uo_event_log,&
this.st_1,&
this.cb_close}
end on

on w_event_log_response.destroy
destroy(this.cb_add_event)
destroy(this.cb_send)
destroy(this.cb_save)
destroy(this.cb_cancel)
destroy(this.uo_event_log)
destroy(this.st_1)
destroy(this.cb_close)
end on

type cb_add_event from commandbutton within w_event_log_response
integer x = 82
integer y = 1704
integer width = 402
integer height = 96
integer taborder = 70
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Add Event"
end type

event clicked;//  claim event 
Enabled = False

		
IF gb_additional_logging = TRUE THEN	
	N_OBJECTHELPER lnv_object_helper
	// write to the application log
	f_populate_app_log(gs_appname_and_handle ,lnv_object_helper.get_app_component_type(THIS), lnv_object_helper.getpath(THIS), 'cb_add_event-clicked' )
END IF 

THIS.SetRedraw(FALSE)
uo_event_log.f_add_event('')
THIS.SetRedraw(TRUE)

cb_cancel.Enabled = TRUE
cb_save.Enabled = TRUE
cb_send.Enabled = FALSE
cb_close.Enabled = FALSE

uo_event_log.cb_refresh.enabled = FALSE
uo_event_log.cbx_comments.enabled = FALSE
uo_event_log.cb_open_filter_window.enabled = FALSE
uo_event_log.cb_refresh_event_list.enabled = FALSE
uo_event_log.cbx_my_events.enabled = FALSE
uo_event_log.cbx_include_incoming_corresp.enabled = FALSE
uo_event_log.cb_sort.enabled = FALSE
uo_event_log.st_1.enabled = FALSE
uo_event_log.st_2.enabled = FALSE

uo_event_log.dw_claim_event.SetColumn("event_type_code")
uo_event_log.dw_individual_event.SetColumn("event_type_code")
end event

type cb_send from commandbutton within w_event_log_response
integer x = 1591
integer y = 1704
integer width = 357
integer height = 96
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Send &Msg"
end type

on clicked;uo_event_log.f_send_event()
end on

type cb_save from commandbutton within w_event_log_response
integer x = 1211
integer y = 1704
integer width = 357
integer height = 96
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
end type

event clicked;THIS.SetRedraw(FALSE)
IF uo_event_log.f_save_event() THEN
	cb_add_event.Enabled = True
	cb_save.Enabled = False
	cb_cancel.Enabled = False
	cb_send.Enabled = TRUE
	cb_close.Enabled = True
	
	uo_event_log.cb_refresh.enabled = TRUE
	uo_event_log.cbx_comments.enabled = TRUE
	uo_event_log.cb_open_filter_window.enabled = TRUE
	uo_event_log.cb_refresh_event_list.enabled = TRUE
	uo_event_log.cbx_my_events.enabled = TRUE
	uo_event_log.cbx_include_incoming_corresp.enabled = TRUE
	uo_event_log.cb_sort.enabled = TRUE
	uo_event_log.st_1.enabled = TRUE
    uo_event_log.st_2.enabled = TRUE
	 
END IF
THIS.SetRedraw(TRUE)
end event

type cb_cancel from commandbutton within w_event_log_response
integer x = 832
integer y = 1704
integer width = 357
integer height = 96
integer taborder = 50
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Cance&l"
end type

event clicked;THIS.SetRedraw(FALSE)
uo_event_log.f_cancel_event('cancel')

cb_add_event.Enabled = True
cb_cancel.Enabled = False
cb_save.Enabled = False
cb_send.Enabled = TRUE
cb_close.Enabled = True

uo_event_log.cb_refresh.enabled = TRUE
uo_event_log.cbx_comments.enabled = TRUE
uo_event_log.cb_open_filter_window.enabled = TRUE
uo_event_log.cb_refresh_event_list.enabled = TRUE
uo_event_log.cbx_my_events.enabled = TRUE
uo_event_log.cbx_include_incoming_corresp.enabled = TRUE
uo_event_log.cb_sort.enabled = TRUE
uo_event_log.st_1.enabled = TRUE
uo_event_log.st_2.enabled = TRUE


THIS.SetRedraw(TRUE)
end event

type uo_event_log from u_event_log within w_event_log_response
integer y = 96
integer taborder = 10
boolean border = false
end type

on uo_event_log.destroy
call u_event_log::destroy
end on

type st_1 from statictext within w_event_log_response
integer x = 5
integer y = 4
integer width = 2665
integer height = 76
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Maintain Events"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cb_close from commandbutton within w_event_log_response
integer x = 2295
integer y = 1704
integer width = 357
integer height = 96
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cl&ose"
end type

on clicked;Close (Parent)
end on

