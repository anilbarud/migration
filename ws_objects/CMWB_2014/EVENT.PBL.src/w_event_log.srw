$PBExportHeader$w_event_log.srw
$PBExportComments$Window to Display and Add Claim Events
forward
global type w_event_log from w_a_tool
end type
type cb_send from commandbutton within w_event_log
end type
type cb_save from commandbutton within w_event_log
end type
type cb_cancel from commandbutton within w_event_log
end type
type cb_add_event from commandbutton within w_event_log
end type
type uo_event_log from u_event_log within w_event_log
end type
end forward

global type w_event_log from w_a_tool
integer width = 3269
integer height = 1848
string title = ""
boolean resizable = false
cb_send cb_send
cb_save cb_save
cb_cancel cb_cancel
cb_add_event cb_add_event
uo_event_log uo_event_log
end type
global w_event_log w_event_log

type variables
w_sheet   iw_sheet
end variables

event open;call super::open;S_WINDOW_MESSAGE	lstr_message

lstr_message = Message.PowerObjectParm

iw_sheet = lstr_message.apo_powerobjectparm[1]

uo_event_log.uf_populate_variables(lstr_message)
uo_event_log.uf_set_window(w_frame)
uo_event_log.uf_set_event_log_window(this)
end event

on w_event_log.create
int iCurrent
call super::create
this.cb_send=create cb_send
this.cb_save=create cb_save
this.cb_cancel=create cb_cancel
this.cb_add_event=create cb_add_event
this.uo_event_log=create uo_event_log
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_send
this.Control[iCurrent+2]=this.cb_save
this.Control[iCurrent+3]=this.cb_cancel
this.Control[iCurrent+4]=this.cb_add_event
this.Control[iCurrent+5]=this.uo_event_log
end on

on w_event_log.destroy
call super::destroy
destroy(this.cb_send)
destroy(this.cb_save)
destroy(this.cb_cancel)
destroy(this.cb_add_event)
destroy(this.uo_event_log)
end on

event closequery;call super::closequery;IF gb_additional_logging = TRUE THEN	
	N_OBJECTHELPER lnv_object_helper
	// write to the application log
	f_populate_app_log(gs_appname_and_handle ,lnv_object_helper.get_app_component_type(THIS), lnv_object_helper.getpath(THIS), 'closequery' )
END IF 
end event

type st_title from w_a_tool`st_title within w_event_log
integer width = 3218
string text = "Maintain Events"
end type

type cb_close from w_a_tool`cb_close within w_event_log
integer y = 1700
end type

type cb_send from commandbutton within w_event_log
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

event clicked;IF gb_additional_logging = TRUE THEN	
	N_OBJECTHELPER lnv_object_helper
	// write to the application log
	f_populate_app_log(gs_appname_and_handle ,lnv_object_helper.get_app_component_type(THIS), lnv_object_helper.getpath(THIS), 'cb_send-clicked' )
END IF 

uo_event_log.f_send_event()
end event

type cb_save from commandbutton within w_event_log
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

event clicked;DataWindowChild  ldwc_event_type_code
INTEGER          li_rtn, li_row
LONG             ll_tombstone_claim_no, ll_event_log_claim_no
LONG             ll_tombstone_individual_no, ll_event_log_individual_no
N_OBJECTHELPER   lnv_object_helper
STRING           ls_event_category_code


THIS.SetRedraw(FALSE)

IF gb_additional_logging = TRUE THEN
	// write to the application log
	f_populate_app_log(gs_appname_and_handle ,lnv_object_helper.get_app_component_type(THIS), lnv_object_helper.getpath(THIS), 'cb_save-clicked')
END IF



li_rtn = uo_event_log.dw_add_event.GetChild('event_type_code',ldwc_event_type_code) 
li_row = ldwc_event_type_code.GetRow()

ls_event_category_code = ldwc_event_type_code.GetItemString(li_row,'event_category_code')

//ls_event_category_code = uo_event_log.dw_add_event.GetItemString(1,7) // column 7 is the event category column
IF ls_event_category_code = 'C' THEN
	ll_tombstone_claim_no = iw_sheet.dw_basic_claim.GetItemNumber(1,'claim_no')
	ll_event_log_claim_no = uo_event_log.dw_add_event.GetItemNumber(1,'claim_no')
	
	IF ll_tombstone_claim_no <> ll_event_log_claim_no THEN
		// PROBLEM!
		// tombstone claim does not match event log claim
		// write to the application log
		f_populate_app_log(gs_appname_and_handle ,lnv_object_helper.get_app_component_type(THIS), lnv_object_helper.getpath(THIS), 'cb_save-clicked ERROR claim number mismatch. Event Log ('+String(ll_event_log_claim_no)+'); Tombstone ('+String(ll_tombstone_claim_no)+')')
		MessageBox('Save Error','The Event Log claim ('+String(ll_event_log_claim_no)+') does not match the WorkBench tombstone claim ('+String(ll_tombstone_claim_no)+').' &
					+ ' This will prevent the current event log from being saved.' &
					+ '~r~n~r~n' &
					+ 'Please cancel the save. Close the Event Log and try again.',Exclamation!)
		RETURN
	END IF
ELSE
	ll_tombstone_individual_no = iw_sheet.dw_basic_claim.GetItemNumber(1,'individual_no')
	ll_event_log_individual_no = uo_event_log.dw_add_event.GetItemNumber(1,'individual_no')
	
	IF ll_tombstone_individual_no <> ll_event_log_individual_no THEN
		// PROBLEM!
		// tombstone individual does not match event log claim
		// write to the application log
		f_populate_app_log(gs_appname_and_handle ,lnv_object_helper.get_app_component_type(THIS), lnv_object_helper.getpath(THIS), 'cb_save-clicked ERROR individual number mismatch. Event Log ('+String(ll_event_log_individual_no)+'); Tombstone ('+String(ll_tombstone_individual_no)+')')
		MessageBox('Save Error','The Event Log individual ('+String(ll_event_log_individual_no)+') does not match the WorkBench tombstone individual ('+String(ll_tombstone_individual_no)+').' &
					+ ' This will prevent the current event log from being saved.' &
					+ '~r~n~r~n' &
					+ 'Please cancel the save. Close the Event Log and try again.',Exclamation!)
		RETURN
	END IF
END IF



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

type cb_cancel from commandbutton within w_event_log
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


IF gb_additional_logging = TRUE THEN	
	N_OBJECTHELPER lnv_object_helper
	// write to the application log
	f_populate_app_log(gs_appname_and_handle ,lnv_object_helper.get_app_component_type(THIS), lnv_object_helper.getpath(THIS), 'cb_cancel-clicked' )
END IF 


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

uo_event_log.dw_add_event.uf_protectcolumn("event_specific_code", TRUE)
uo_event_log.dw_add_event.uf_protectcolumn("individual_no", TRUE)


THIS.SetRedraw(TRUE)

end event

type cb_add_event from commandbutton within w_event_log
integer x = 50
integer y = 1700
integer width = 402
integer height = 104
integer taborder = 50
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Add Event"
end type

event clicked;
//  claim event 
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

type uo_event_log from u_event_log within w_event_log
integer y = 88
integer width = 3237
integer height = 1604
integer taborder = 10
boolean bringtotop = true
boolean border = false
end type

on uo_event_log.destroy
call u_event_log::destroy
end on

