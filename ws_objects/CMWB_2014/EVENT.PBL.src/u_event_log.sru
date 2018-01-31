$PBExportHeader$u_event_log.sru
$PBExportComments$Visual event log
forward
global type u_event_log from userobject
end type
type cb_refresh_event_list from commandbutton within u_event_log
end type
type st_2 from statictext within u_event_log
end type
type cb_sort from commandbutton within u_event_log
end type
type cb_open_filter_window from commandbutton within u_event_log
end type
type cbx_comments from checkbox within u_event_log
end type
type st_1 from statictext within u_event_log
end type
type cbx_include_incoming_corresp from checkbox within u_event_log
end type
type cbx_my_events from checkbox within u_event_log
end type
type dw_add_event from u_datawindow within u_event_log
end type
type cb_refresh from commandbutton within u_event_log
end type
type dw_event_comments from u_datawindow within u_event_log
end type
type dw_events from u_datawindow within u_event_log
end type
type uo_image_append from u_image_append within u_event_log
end type
type uo_dw_document_path from u_dw_document_path within u_event_log
end type
type dw_claim_event from u_datawindow within u_event_log
end type
type dw_individual_event from u_datawindow within u_event_log
end type
end forward

global type u_event_log from userobject
integer width = 3241
integer height = 1600
boolean border = true
long backcolor = 67108864
long tabtextcolor = 33554432
cb_refresh_event_list cb_refresh_event_list
st_2 st_2
cb_sort cb_sort
cb_open_filter_window cb_open_filter_window
cbx_comments cbx_comments
st_1 st_1
cbx_include_incoming_corresp cbx_include_incoming_corresp
cbx_my_events cbx_my_events
dw_add_event dw_add_event
cb_refresh cb_refresh
dw_event_comments dw_event_comments
dw_events dw_events
uo_image_append uo_image_append
uo_dw_document_path uo_dw_document_path
dw_claim_event dw_claim_event
dw_individual_event dw_individual_event
end type
global u_event_log u_event_log

type variables
Protected:

DATE							idt_claim_accident_date, idt_individual_accident_date
LONG							il_claim_no, il_individual_no, il_find_event_no
N_EVENT_LOG				inv_event_log
STRING						is_event_category_code, is_event_type_code, is_event_specific_code
STRING						is_last_name, is_given_names, is_add_new_event, is_allow_parameter_change
STRING						is_comment_one, is_filter_string, is_find_event_flag, is_search_event_category_code

WINDOW						iw_win, iw_event_log

S_WINDOW_MESSAGE	istr_window_message


end variables

forward prototypes
public subroutine f_send_event ()
public subroutine uf_comments (boolean ab_set_on)
public function boolean uf_archived_document (long al_docid)
public function boolean f_save_event ()
public subroutine f_cancel_event (string as_caller)
public function integer f_switch_datawindow (boolean ab_add)
public subroutine uf_set_claim_no (long al_claim_no)
public subroutine uf_set_individual_no (long al_individual_no)
public subroutine uf_set_accident_date (date adt_accident_date)
public subroutine uf_set_last_name (string as_last_name)
public subroutine uf_set_given_names (string as_given_names)
public subroutine uf_populate_variables (s_window_message astr_message)
public subroutine uf_set_event_category_code (string as_event_category_code)
public subroutine uf_set_add_new_event (string as_add_new_event)
public subroutine uf_set_allow_parameter_change (string as_allow_parameter_change)
public subroutine uf_set_event_type_code (string as_event_type_code)
public subroutine uf_set_event_specific_code (string as_event_specific_code)
public subroutine uf_set_event_parameters ()
public subroutine uf_set_window (window aw_win)
public subroutine uf_set_comment_one (string as_comment)
public subroutine uf_cancel_character_filter ()
public subroutine f_adjust_text ()
public function integer uf_filter_dw (u_datawindow a_dw)
public subroutine uf_set_event_log_window (window aw_window)
public subroutine f_add_event (string as_event_category_code)
public subroutine uf_set_find_event_flag (string as_find_event_flag)
public subroutine uf_set_event_no (long al_find_event_no)
public subroutine uf_find_event ()
public subroutine uf_set_search_event_category_code (string as_search_event_category_code)
end prototypes

public subroutine f_send_event ();S_SEND_DOC_PARAMETERS	ls_send_doc_parameters

ls_send_doc_parameters.msg_mode = TRUE
ls_send_doc_parameters.claim_no = il_claim_no
IF dw_event_comments.visible THEN
	IF dw_event_comments.RowCount() <= 0 THEN
		MessageBox('Warning','There are no events listed to send.')
		Return
	ELSE
		ls_send_doc_parameters.document_list = dw_event_comments
	END IF
ELSE
	IF dw_events.RowCount() <= 0 THEN
		MessageBox('Warning','There are no events listed to send.')
		Return
	ELSE
		ls_send_doc_parameters.document_list = dw_events
	END IF
END IF
OpenWithParm(w_send_folder,ls_send_doc_parameters)
end subroutine

public subroutine uf_comments (boolean ab_set_on);LONG	ll_row

	IF ab_set_on THEN
		ll_row = dw_events.GetRow()
		IF ll_row > 0 THEN
			dw_event_comments.ScrollToRow(ll_row)
			dw_event_comments.SelectRow(ll_row,TRUE)
		END IF
		dw_event_comments.SetFocus()
	ELSE
		ll_row = dw_event_comments.GetRow()
		IF ll_row > 0 THEN
			dw_events.ScrollToRow(ll_row)
		END IF
	END IF		
	dw_event_comments.visible = ab_set_on
	cb_refresh.visible = TRUE
end subroutine

public function boolean uf_archived_document (long al_docid);INTEGER		li_count

SELECT Count(*)
INTO     :li_count
FROM    DOCUMENT_INDEX_ARCHIVE
WHERE  docid = :al_docid
USING   ImageTrans;

IF li_count > 0 THEN
	MessageBox('Archived', 'This document has been archived.',Information!)
	RETURN TRUE
ELSE
	RETURN FALSE
END IF
end function

public function boolean f_save_event ();LONG		ll_num_rows, ll_result, ll_event_no, ll_row, ll_individual_no, ll_counter, ll_num_caution_events
STRING	ls_event_type_code, ls_event_specific_code, ls_caution_flag, ls_event_comment, ls_event_category_code

INTEGER		li_count, li_rtn
DATETIME		ldt_event_date
STRING			ls_active_flag
STRING			ls_generated_method_code
STRING			ls_clean_comment
N_PROCESS_RUN_STATUS ln_process_run_status
DataWindowChild   ldwc_event_type_code

u_ds     ldw_add_claim_event

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '026' refers to the Event Log Maintenance module, '044' refers to the Payment Processing module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('026','044','save',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return false
END IF
/******************************************************************************************/


If dw_add_event.AcceptText() = -1 THEN
	Return False
END IF

ll_row = dw_add_event.GetRow()
ls_event_type_code = dw_add_event.GetItemString(ll_row, 'event_type_code')
ls_event_specific_code = dw_add_event.GetItemString(ll_row, 'event_specific_code')
ls_event_comment = dw_add_event.GetItemString(ll_row, 'event_comment')
ldt_event_date = dw_add_event.GetItemDateTime(ll_row, 'event_date')

IF ls_event_type_code = '' or IsNull(ls_event_type_code) THEN
	MessageBox('Incorrect Event Type', 'Please choose an event type', StopSign!)
	dw_add_event.SetFocus()
	dw_add_event.SetColumn('event_type_code')
	RETURN FALSE
End if

// double check that we have a valid event_category_code or we cant proceed
// if its blank, get the event_category_code from the current value of the event type code drop down
if is_event_category_code = '' THEN
	li_rtn = dw_add_event.GetChild("event_type_code", ldwc_event_type_code)
	ls_event_category_code = ldwc_event_type_code.getItemString(ldwc_event_type_code.getRow(), 'event_category_code')
	is_event_category_code  = ls_event_category_code
END IF

li_rtn = inv_event_log.nf_validate_event_category(is_event_category_code)
IF li_rtn = 0 THEN
	SignalError(-666,'Invalid Event Category: "' + is_event_category_code + '"')
END IF

li_rtn = inv_event_log.nf_validate_event_type(is_event_category_code,ls_event_type_code,ls_active_flag)
IF li_rtn = 0 THEN
	SignalError(-666,'Invalid Event Category/Type: "' + is_event_category_code + '/' + ls_event_type_code + '"')
END IF
//If the active flag is "N" then tell the user give them a chance to choose an active one.
IF ls_active_flag = 'N' THEN
	MessageBox('Incorrect Event Type', 'Please choose an "active" event type', StopSign!)
	dw_add_event.SetFocus()
	dw_add_event.SetColumn('event_type_code')
	RETURN FALSE
END IF

li_rtn = inv_event_log.nf_validate_event_specific_code(ls_event_type_code,ls_event_specific_code,ls_generated_method_code,ls_active_flag)
IF li_rtn = 0 THEN
	SignalError(-666,'Invalid Event Type/Specific Code: "' + ls_event_type_code + '/' + ls_event_specific_code + '"')
END IF
//If the generated method is "A" then crash, the user should not have acess to automated event types in this module
IF ls_generated_method_code = 'A' THEN
	SignalError(-666,'Automatic event generation is not allowed through this module. Invalid event_type_code "' + ls_event_type_code + '"')
END IF
//If the active flag is "N" then tell the user give them a chance to choose an active one.
IF ls_active_flag = 'N' THEN
	MessageBox('Incorrect Event Specific Type', 'Please choose an "active" event specific type', StopSign!)
	dw_add_event.SetFocus()
	dw_add_event.SetColumn('event_specific_code')
	RETURN FALSE
END IF

// Check if doing an Individual Event 
IF is_event_category_code = 'I' THEN
	ll_individual_no = dw_add_event.GetItemNumber(ll_row, 1)
	IF ll_individual_no = 0 THEN
		MessageBox('Event Individual Error', 'Please choose a claim participant.')
		RETURN FALSE
	END IF

	SELECT caution_flag
	  INTO :ls_caution_flag
	  FROM INDIVIDUAL 
	 WHERE individual_no = :ll_individual_no 
	 USING SQLCA ; 

	li_rtn = SQLCA.nf_handle_error("u_event_log", "", "f_save_event - Select caution_flag from INDIVIDUAL") 

	IF ls_caution_flag = 'Y' THEN
		// If entering a Caution Removal, set caution flag to 'N'
		IF ls_event_type_code = '034' AND ls_event_specific_code = 'C5' THEN 
			UPDATE INDIVIDUAL
			SET       caution_flag = 'N'
			WHERE  individual_no = :ll_individual_no
			USING   SQLCA;
			
			SQLCA.nf_handle_error("u_event_log", "", "f_save_event - UPDATE INDIVIDUAL SET caution_flag = 'N'") 
		END IF
	ELSEIF ls_caution_flag = 'N' THEN
		// See if user is trying to create a Caution / Removal event when there is no record of individual having a caution event
		IF ls_event_type_code = '034' AND ls_event_specific_code = 'C5' THEN 
			SELECT COUNT(*) 
			  INTO :ll_num_caution_events
			  FROM INDIVIDUAL_EVENT
			 WHERE individual_no = :ll_individual_no 
			   AND event_type_code = '034' 
				AND event_specific_code <> 'C5' 
			 USING SQLCA ; 

			li_rtn = SQLCA.nf_handle_error("u_event_log", "", "f_save_event - Select count(*) from INDIVIDUAL_EVENT") 

			IF ll_num_caution_events = 0 THEN 
				MessageBox('Caution Event Error', 'This Claim Participant cannot be removed from having a Caution. There is no record of the individual having a Caution Event.')
				RETURN FALSE
			END IF
		ELSEIF ls_event_type_code = '034' AND ls_event_specific_code <> 'C5' THEN
			//If adding a caution flag event, check if on already, if not, turn on.
			IF ls_caution_flag = 'N' THEN
				UPDATE INDIVIDUAL
				SET       caution_flag = 'Y'
				WHERE  individual_no = :ll_individual_no
				USING   SQLCA;
				
				SQLCA.nf_handle_error("u_event_log", "", "f_save_event - UPDATE INDIVIDUAL SET caution_flag = 'Y'") 
			END IF
			
		END IF
	ELSE
		MessageBox('Event Individual Error', 'There is a problem with this claim participant. Please contact the Help Desk.')
		RETURN FALSE
	END IF
	
	// If there is a death date, make sure event date is not greater than death date	
	SELECT	Count(*)
	INTO		:li_count
	FROM		INDIVIDUAL
	WHERE	individual_no	= :ll_individual_no
	AND		death_date is not null
	AND		death_date < :ldt_event_date
	USING	SQLCA;
	
	IF SQLCA.nf_handle_error("u_event_log","Select count(*) from INDIVIDUAL","f_save_event")< 0 THEN
		RETURN FALSE
	END IF
	
	IF li_count > 0 AND ls_event_type_code = '034' AND ls_event_specific_code <> 'C5' THEN
		MessageBox('Individual Event Error', 'The Caution Event date cannot be greater than the date of death of an individual.')
		dw_add_event.SetFocus()
		dw_add_event.SetColumn('event_date')
		RETURN FALSE
	END IF
END IF

// clean up the comments
ls_clean_comment = f_Clean_String_4(ls_event_comment)
IF ls_clean_comment <> ls_event_comment THEN
	// no warning to users
	dw_add_event.SetItem(ll_row, 'event_comment', TRIM(ls_clean_comment))
END IF
	
IF dw_add_event.RowCount() <> 1 THEN
	MessageBox("Event","Unable to add new event. Please exit and try again.")
	Return False
END IF

SQLCA.nf_begin_transaction()

ldw_add_claim_event = create u_ds   //dw_online

IF is_event_category_code = 'C' THEN
	// set the datastore for adding claim events
	ll_event_no = inv_event_log.nf_next_claim_event_no(il_claim_no)
	IF ll_event_no = -1 THEN
		MessageBox("Event No","Unable to determine next event no. Please try again")
		Return False
	END IF
	ldw_add_claim_event.dataobject = 'ds_add_claim_event'
	ldw_add_claim_event.settransObject(SQLCA)
	ll_row = ldw_add_claim_event.insertRow(0)	
	ldw_add_claim_event.SetItem(ll_row,"event_no", ll_event_no)	
	ldw_add_claim_event.SetItem(ll_row,"claim_no", il_claim_no)	
	ldw_add_claim_event.SetItem(ll_row,"event_type_code", ls_event_type_code)	
	ldw_add_claim_event.SetItem(ll_row,"event_specific_code", ls_event_specific_code)	
	ldw_add_claim_event.SetItem(ll_row,"event_comment",ls_event_comment)	
	ldw_add_claim_event.SetItem(ll_row,"event_date", ldt_event_date)	
ELSE
	ll_individual_no = dw_add_event.GetItemNumber(ll_row,1)
	ll_event_no = inv_event_log.nf_next_individual_event_no(ll_individual_no)
	dw_add_event.SetItem(1,"individual_event_no",ll_event_no)
	IF ll_event_no = -1 THEN
		MessageBox("Event No","Unable to determine next event no. Please try again")
		Return False
	END IF
END IF

IF is_allow_parameter_change = 'N' THEN			
	dw_add_event.uf_protectcolumn('event_type_code', FALSE)
	dw_add_event.uf_protectcolumn('event_specific_code', FALSE)
	dw_add_event.uf_protectcolumn('individual_no', FALSE)
	dw_add_event.uf_protectcolumn('event_date', FALSE)	
END IF

IF is_event_category_code = 'C' THEN
	ldw_add_claim_event.Update()
ELSE
	dw_add_event.Update()
END IF

ll_result = SQLCA.nf_handle_error("u_event_log","dw_add_event","f_save_event")
IF ll_result < 0 THEN
	Return  False
END IF

SQLCA.nf_commit_transaction()


// any auto-added event has been saved
is_add_new_event = 'N'
dw_events.Enabled = True

dw_events.Reset()	//	To ensure rowfocuschanged fires off
ll_result = dw_events.Retrieve(il_claim_no)
IF SQLCA.nf_handle_error("u_event_log","dw_events","f_save_event") < 0 THEN
	Return False
END IF

cb_refresh.triggerEvent(CLICKED!)

dw_event_comments.enabled = TRUE
IF dw_event_comments.visible THEN
	dw_event_comments.TriggerEvent(RowFocusChanged!)
END IF
	
Return True
end function

public subroutine f_cancel_event (string as_caller);LONG		ll_result, ll_row
INT		li_error_status
STRING	ls_type_filter

	
		
	IF as_caller = 'refresh' THEN
		ll_result = dw_events.Retrieve(il_claim_no)
		SQLCA.nf_handle_error("u_event_log","dw_events","f_cancel_event")
		
		IF is_add_new_event = 'N' THEN
			
			dw_events.Enabled = True
			dw_event_comments.enabled = TRUE
			
			IF dw_event_comments.visible THEN
				IF dw_event_comments.RowCount() > 0 THEN dw_event_comments.SelectRow(1,TRUE)
			ELSE
				IF dw_events.RowCount() > 0 THEN dw_events.SelectRow(1,TRUE)
			END IF
			
			ll_row = dw_events.GetRow()
			IF ll_row > 0 THEN
				is_event_category_code = dw_events.GetItemString( ll_row, 'event_category_code')
			ELSE
				is_event_category_code = 'C'
			END IF
			
		/*	so the filter will be applied
		*/
			f_switch_datawindow(FALSE)
			
		END IF
		
	ELSEIF as_caller = 'cancel' THEN
		// an event is being canceled
		IF is_allow_parameter_change = 'N' THEN
			// reset variables to allow changes
			// cancel add & display existing event data
			dw_add_event.Reset()
			dw_add_event.uf_protectcolumn('event_type_code', FALSE)
			dw_add_event.uf_protectcolumn('event_specific_code', FALSE)
			dw_add_event.uf_protectcolumn('individual_no', FALSE)
			dw_add_event.uf_protectcolumn('event_date', FALSE)	
			
		END IF
		
		ll_result = dw_events.Retrieve(il_claim_no)
		SQLCA.nf_handle_error("u_event_log","dw_events","f_cancel_event")		
	
		ll_row = dw_events.GetRow()
		IF ll_row > 0 THEN
			is_event_category_code = dw_events.GetItemString( ll_row, 'event_category_code')
		ELSE
			is_event_category_code = 'C'
		END IF
		
		/*	so the filter will be applied
		*/
		f_switch_datawindow(FALSE)
		
		// Reset variable
		is_add_new_event = 'N'
		
		IF dw_event_comments.visible THEN
			IF dw_event_comments.RowCount() > 0 THEN
				dw_event_comments.SelectRow(1,TRUE)
				dw_event_comments.event trigger rowfocuschanged(1)
			END IF
		ELSEIF dw_events.visible THEN
			IF dw_events.RowCount() > 0 THEN 
				dw_events.SelectRow(1,TRUE)
				dw_events.event trigger rowfocuschanged(1)
			END IF
		END IF
		
		
	END IF


	

end subroutine

public function integer f_switch_datawindow (boolean ab_add);STRING		ls_type_filter, ls_specific_filter

IF ab_add THEN
	dw_event_comments.Enabled = FALSE
	dw_events.Enabled = FALSE
	
	IF is_event_category_code = 'C' THEN
		dw_claim_event.Visible = FALSE
		dw_individual_event.Visible = FALSE
		
	ELSEIF is_event_category_code = 'I' THEN
		dw_claim_event.Visible = FALSE
		dw_individual_event.Visible = FALSE	
	END IF
	dw_add_event.Visible = TRUE
	
ELSE
	dw_event_comments.Enabled = TRUE
	dw_events.Enabled = TRUE
	
	IF is_event_category_code = 'C' THEN
		dw_claim_event.Visible = TRUE
		dw_individual_event.Visible = FALSE
		
	ELSEIF is_event_category_code = 'I' THEN
		dw_claim_event.Visible = FALSE
		dw_individual_event.Visible = TRUE		
	END IF
	
	dw_add_event.Visible = FALSE
END IF
	
RETURN 0
end function

public subroutine uf_set_claim_no (long al_claim_no);	il_claim_no = al_claim_no
end subroutine

public subroutine uf_set_individual_no (long al_individual_no);il_individual_no = al_individual_no

SELECT	min(accident_date)
INTO		:idt_individual_accident_date
FROM		CLAIM
WHERE	individual_no = :il_individual_no
USING SQLCA;
SQLCA.nf_handle_error('u_event_log', 'uf_set_individual_no', 'select min(accident_date) from CLAIM')
end subroutine

public subroutine uf_set_accident_date (date adt_accident_date);idt_claim_accident_date = adt_accident_date
end subroutine

public subroutine uf_set_last_name (string as_last_name);is_last_name = as_last_name
end subroutine

public subroutine uf_set_given_names (string as_given_names);is_given_names = as_given_names
end subroutine

public subroutine uf_populate_variables (s_window_message astr_message);DATE							ldt_accident_date
LONG							ll_claim_no, ll_individual_no, ll_event_no
STRING						ls_last_name, ls_given_names
STRING						ls_event_category_code, ls_event_type_code, ls_event_specific_code
STRING						ls_claim_role_code, ls_allow_parameter_change, ls_add_new_event
STRING						ls_message, ls_find_event_flag, ls_search_event_category_code


ll_claim_no 						= astr_message.al_DoubleParm[1]
ll_individual_no 					= astr_message.al_DoubleParm[2]
ll_event_no                   = astr_message.al_DoubleParm[3]

ldt_accident_date 				= astr_message.adt_DateParm[1]

ls_claim_role_code 				= astr_message.as_StringParm[1]
ls_last_name 						= astr_message.as_StringParm[2]
ls_given_names 					= astr_message.as_StringParm[3]
ls_event_category_code 			= astr_message.as_StringParm[4]
ls_event_type_code 				= astr_message.as_StringParm[5]
ls_event_specific_code 			= astr_message.as_StringParm[6]
ls_allow_parameter_change	 	= astr_message.as_StringParm[7]
ls_add_new_event 					= astr_message.as_StringParm[8]
ls_message                    = astr_message.as_StringParm[9]
ls_find_event_flag            = astr_message.as_StringParm[10]
ls_search_event_category_code = astr_message.as_StringParm[11]


uf_set_claim_no(ll_claim_no)
uf_set_individual_no(ll_individual_no)
uf_set_event_no(ll_event_no)
uf_set_accident_date(ldt_accident_date)
uf_set_last_name(ls_last_name)
uf_set_given_names(ls_given_names)
uf_set_event_category_code(ls_event_category_code)
uf_set_event_type_code(ls_event_type_code)
uf_set_event_specific_code(ls_event_specific_code)
uf_set_allow_parameter_change(ls_allow_parameter_change)
uf_set_add_new_event(ls_add_new_event)
uf_set_find_event_flag(ls_find_event_flag)
uf_set_comment_one(ls_message)
uf_set_search_event_category_code(ls_search_event_category_code)

IF ls_add_new_event = 'Y' THEN
	// set up a new claim or individual event
	post function uf_set_event_parameters()
END IF

cb_refresh.PostEvent(Clicked!)
end subroutine

public subroutine uf_set_event_category_code (string as_event_category_code);is_event_category_code = as_event_category_code
end subroutine

public subroutine uf_set_add_new_event (string as_add_new_event);is_add_new_event = as_add_new_event
end subroutine

public subroutine uf_set_allow_parameter_change (string as_allow_parameter_change);is_allow_parameter_change = as_allow_parameter_change
end subroutine

public subroutine uf_set_event_type_code (string as_event_type_code);is_event_type_code = as_event_type_code

end subroutine

public subroutine uf_set_event_specific_code (string as_event_specific_code);is_event_specific_code = as_event_specific_code
end subroutine

public subroutine uf_set_event_parameters ();

is_comment_one = f_clean_string_4(is_comment_one)

IF is_add_new_event = 'Y' THEN
	dw_add_event.SetItem(1, 'event_type_code', is_event_type_code)
	dw_add_event.SetItem(1, 'event_specific_code', is_event_specific_code)
	IF is_event_category_code = 'I' THEN
		dw_add_event.SetItem(1, 'individual_no', il_individual_no)
	END IF
	dw_add_event.SetItem(1, 'event_comment', TRIM(is_comment_one))
	
	IF is_allow_parameter_change = 'N' THEN	
		dw_add_event.uf_protectcolumn('event_type_code', TRUE)
		dw_add_event.uf_protectcolumn('event_specific_code', TRUE)
		dw_add_event.uf_protectcolumn('individual_no', TRUE)
		dw_add_event.uf_protectcolumn('event_date', TRUE)	
		
	ELSE
		// do nothing	
	END IF
END IF

end subroutine

public subroutine uf_set_window (window aw_win);iw_win = aw_win
end subroutine

public subroutine uf_set_comment_one (string as_comment);is_comment_one = as_comment
end subroutine

public subroutine uf_cancel_character_filter ();dw_add_event.uf_set_character_filter('event_comment',0)

end subroutine

public subroutine f_adjust_text ();STRING ls_text, ls_new_text, ls_text2, ls_new_text2, ls_tab, ls_double, ls_double2
LONG ll_pos, ll_pos2,ll_string1, ll_string2


ls_text = dw_add_event.GetItemString(dw_add_event.GetRow(), "event_comment")

//replace carriage returns in the first Comment Area
DO
	ll_pos = Pos(ls_text,"~r~n",1)
	ll_string1 = Len("~r~n")
	IF ll_pos <> 0 THEN
		ls_new_text = Replace (ls_text,ll_pos,ll_string1, " ")
		ls_text = ls_new_text
	END IF
LOOP WHILE ll_pos <> 0
//replace double spaces in the first Comment Area
DO
	ll_pos2 = Pos(ls_new_text,"  ", 1)
	ll_string2 = Len("  ")
	IF ll_pos2 <> 0 THEN
		ls_text = Replace (ls_new_text,ll_pos2,ll_string2, " ")
		ls_new_text = ls_text
	END IF
LOOP WHILE ll_pos2 <> 0
ll_pos=0;ll_pos2=0;ll_string1=0;ll_string2=0;

dw_add_event.SetItem(dw_add_event.GetRow(), "event_comment",TRIM(ls_text))

end subroutine

public function integer uf_filter_dw (u_datawindow a_dw);
STRING  ls_new_filter

IF cbx_my_events.checked THEN
	ls_new_filter = "create_user_id = '" + vgst_user_profile.user_id + "'"
END IF


IF ls_new_filter > '' AND is_filter_string > '' THEN
	ls_new_filter = is_filter_string + ' and ' + ls_new_filter
ELSEIF is_filter_string > '' THEN
	ls_new_filter = is_filter_string
END IF

		// T032789 - RS  -  if the user has specifically selected one of the incoming correspondence filters in the separate filter window, 
		//  then do nothing here,  let the filter string being returned from the filter selection window remain what it is;
		//  they seleted 'incoming correspondence' plus maybe a subtype, so filter on the specific criteria exclusively, 
		//  regardless of wether the check box is ticked off or not. (as per Diana's recomendation to me RS on 2017/02/02)	
		// otherwise, if there is not a match, carry on like before to check the status of the 'include all incoming correspondence' checkbox
IF  NOT MATCH(ls_new_filter, "event_type_code = '004'") THEN
	
	IF cbx_include_incoming_corresp.checked THEN
	  	IF ls_new_filter > "" THEN
			ls_new_filter = ls_new_filter +  " or event_type_code = '004'"
		END IF
	ELSE
		IF ls_new_filter > "" THEN
				ls_new_filter = ls_new_filter +  " and event_type_code <> '004'"			
		ELSE
			ls_new_filter = "event_type_code <> '004'"
		END IF
	END IF	
END IF

a_dw.setFilter(ls_new_filter)
a_dw.filter()

IF a_dw.rowcount() > 0  THEN
	a_dw.selectRow(0, FALSE)
	a_dw.selectRow(1, TRUE)
END IF

cb_refresh.enabled = TRUE
Return 0


end function

public subroutine uf_set_event_log_window (window aw_window);iw_event_log = aw_window
end subroutine

public subroutine f_add_event (string as_event_category_code);LONG        ll_current_row, ll_return, ll_rows
DATE        ld_current_date
STRING     ls_name
u_DataWindow	ldw_add_event
DataWindowChild 	ldwc_event_type
DataWindowChild 	ldwc_individual


is_event_category_code = as_event_category_code

// T023109 - category code no longer used for selecting an add claim event datawindow,  vs an
//  add individual event datawindow, event category determined on the fly, by the event type selected


ldw_add_event = dw_add_event


// Unhighlight the event list
dw_events.Enabled = False
dw_events.SelectRow(0,False)

dw_event_comments.Enabled = False
dw_event_comments.SelectRow(0,False)


ld_current_date = Date(f_server_datetime())

dw_add_event.Reset()

// datawindow should match chosen category type
f_switch_datawindow(TRUE)

// Add a row
ll_current_row = ldw_add_event.InsertRow(0)
// set claim_no & event_date
dw_add_event.SetItem(ll_current_row, "claim_no", il_claim_no)
dw_add_event.SetItem(ll_current_row, "event_date", ld_current_date)

// Only show manual event types for the chosen event category
dw_add_event.GetChild("event_type_code",ldwc_event_type)
ll_return = ldwc_event_type.SetFilter("generated_method_code = 'M' and event_log_entry_flag = 'Y' and active_flag = 'Y' ")
ll_return = ldwc_event_type.Filter()


dw_add_event.GetChild("individual_no",ldwc_individual)
ldwc_individual.SetTransObject(SQLCA)
ll_rows = ldwc_individual.Retrieve(il_claim_no)
	
ls_name = ldwc_individual.GetItemString(1,"name")
dw_add_event.SetItem(1,"individual_no",il_individual_no)
ldwc_individual.SetText(ls_name)


dw_add_event.SetFocus()
dw_add_event.SetColumn("event_type_code")

end subroutine

public subroutine uf_set_find_event_flag (string as_find_event_flag);is_find_event_flag = as_find_event_flag
end subroutine

public subroutine uf_set_event_no (long al_find_event_no);il_find_event_no = al_find_event_no

end subroutine

public subroutine uf_find_event ();INTEGER                 li_rowcount, li_find
STRING						ls_find


IF is_search_event_category_code = 'C' THEN
	// find claim event
	ls_find = 'claim_no = ' +String(il_claim_no)+ ' and event_no = ' +String(il_find_event_no)
ELSE
	// find individual event
	ls_find = 'event_category_code = "' +is_search_event_category_code+ '" and event_no = ' +String(il_find_event_no)
END IF

li_rowcount = dw_events.RowCount()

li_find = dw_events.Find(ls_find,1,li_rowcount)
IF li_find > 0 THEN
	dw_events.SetRow(li_find)
	dw_events.SelectRow(li_find,TRUE)
	dw_events.ScrollToRow(li_find)
END IF
end subroutine

public subroutine uf_set_search_event_category_code (string as_search_event_category_code);is_search_event_category_code = as_search_event_category_code
end subroutine

on destructor;If IsValid(inv_event_log) Then
	Destroy inv_event_log
End If
end on

event constructor;/*	Purpose:	Retrieves and displays all CLAIM_EVENTs for a claim
*/
	inv_event_log = CREATE n_event_log

/*	Set transaction objects for data windows
*/
	dw_events.SetTransObject(SQLCA)
	dw_claim_event.SetTransObject(SQLCA)
	dw_individual_event.SetTransObject(SQLCA)
	dw_add_event.SetTransObject(SQLCA)
	
	dw_event_comments.SetTransObject(SQLCA)
	
	dw_events.ShareData(dw_event_comments)

	dw_events.uf_setselect(1)
	dw_event_comments.uf_setselect(1)

post function uf_cancel_character_filter()
end event

on u_event_log.create
this.cb_refresh_event_list=create cb_refresh_event_list
this.st_2=create st_2
this.cb_sort=create cb_sort
this.cb_open_filter_window=create cb_open_filter_window
this.cbx_comments=create cbx_comments
this.st_1=create st_1
this.cbx_include_incoming_corresp=create cbx_include_incoming_corresp
this.cbx_my_events=create cbx_my_events
this.dw_add_event=create dw_add_event
this.cb_refresh=create cb_refresh
this.dw_event_comments=create dw_event_comments
this.dw_events=create dw_events
this.uo_image_append=create uo_image_append
this.uo_dw_document_path=create uo_dw_document_path
this.dw_claim_event=create dw_claim_event
this.dw_individual_event=create dw_individual_event
this.Control[]={this.cb_refresh_event_list,&
this.st_2,&
this.cb_sort,&
this.cb_open_filter_window,&
this.cbx_comments,&
this.st_1,&
this.cbx_include_incoming_corresp,&
this.cbx_my_events,&
this.dw_add_event,&
this.cb_refresh,&
this.dw_event_comments,&
this.dw_events,&
this.uo_image_append,&
this.uo_dw_document_path,&
this.dw_claim_event,&
this.dw_individual_event}
end on

on u_event_log.destroy
destroy(this.cb_refresh_event_list)
destroy(this.st_2)
destroy(this.cb_sort)
destroy(this.cb_open_filter_window)
destroy(this.cbx_comments)
destroy(this.st_1)
destroy(this.cbx_include_incoming_corresp)
destroy(this.cbx_my_events)
destroy(this.dw_add_event)
destroy(this.cb_refresh)
destroy(this.dw_event_comments)
destroy(this.dw_events)
destroy(this.uo_image_append)
destroy(this.uo_dw_document_path)
destroy(this.dw_claim_event)
destroy(this.dw_individual_event)
end on

type cb_refresh_event_list from commandbutton within u_event_log
integer x = 2665
integer y = 932
integer width = 475
integer height = 92
integer taborder = 110
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Refresh"
end type

event clicked;f_cancel_event('refresh')
end event

type st_2 from statictext within u_event_log
integer x = 2752
integer y = 524
integer width = 448
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Correspondence"
boolean focusrectangle = false
boolean disabledlook = true
end type

type cb_sort from commandbutton within u_event_log
integer x = 2665
integer y = 776
integer width = 475
integer height = 92
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Sort"
end type

event clicked;/*	Call powerbuilders sort dialog box to let the user select the sort they
	want.  Then sort the list and bring the current row into view.
*/
STRING	     ls_null
LONG		ll_selectedrow

u_datawindow  ldw_datawindow

if dw_events.visible then 
	ldw_datawindow = dw_events
ELSEIF dw_event_comments.visible THEN
	ldw_datawindow = dw_event_comments
END IF

SetNull(ls_null)
IF ldw_datawindow.SetSort(ls_null) < 0 THEN
	Return
END IF

ldw_datawindow.Sort()
ll_selectedrow = ldw_datawindow.GetSelectedRow(0)
ldw_datawindow.GroupCalc()
IF ll_selectedrow > 0 THEN
	ldw_datawindow.ScrollToRow(ll_selectedrow)
END IF
end event

type cb_open_filter_window from commandbutton within u_event_log
integer x = 2665
integer y = 636
integer width = 475
integer height = 92
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Filter"
end type

event clicked;
String 	ls_filter
w_event_log    lw_event_Log

/*	Open the filter popup window
*/

Open(w_filter_event_type_list)

/*	If its a 'cancel', do nothing and return, otherwise capture the filter, store it in an instance variable
    then pass control back to the u_event_log function to do the final filter application, which may or may not
    include other parts like the my events filter, include incoming correspondence etc
*/	
ls_filter = Message.StringParm

IF ls_filter = "Cancel" or ls_filter = "" THEN
	Return
END IF

is_filter_string = ls_filter
uf_filter_dw(dw_events)
uf_filter_dw(dw_event_comments)

cb_refresh.enabled = true

// this is to clear any 'new events just being created, if and when the user opens the filter window
IF isvalid(iw_event_log) THEN
	IF  iw_event_log.Classname() = "w_event_log" THEN
		lw_event_Log = iw_event_log
		lw_event_Log.cb_cancel.triggerevent (CLICKED!)
	END IF
END IF
end event

type cbx_comments from checkbox within u_event_log
integer x = 2661
integer y = 164
integer width = 553
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "  Show Comments"
end type

event clicked;IF this.checked  THEN
	uf_comments(TRUE)
	cb_refresh.visible = TRUE	
ELSE
	uf_comments(FALSE)
END IF
end event

type st_1 from statictext within u_event_log
integer x = 2757
integer y = 456
integer width = 398
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "All Incoming"
boolean focusrectangle = false
boolean disabledlook = true
end type

type cbx_include_incoming_corresp from checkbox within u_event_log
integer x = 2661
integer y = 380
integer width = 457
integer height = 80
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "  Also Include"
end type

event clicked;
//force a filter action, so the status of this checkbox will get picked up and applied correctly
uf_filter_dw(dw_events)
uf_filter_dw(dw_event_comments)
	 
 if checked then 
	cb_refresh.enabled = true
Else    // only disable the refresh button if there is no filter string in the instance variable - still need the button  available if there is a filter in place
	IF is_filter_string = "" THEN
		cb_refresh.enabled = false
	END IF
END IF

end event

type cbx_my_events from checkbox within u_event_log
integer x = 2661
integer y = 272
integer width = 489
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "  My Events only"
end type

event clicked;
//force a filter action, so the status of this checkbox will get picked up and applied correctly
uf_filter_dw(dw_events)
uf_filter_dw(dw_event_comments)

if checked then 
	cb_refresh.enabled = true
Else
	IF is_filter_string = "" THEN
		cb_refresh.enabled = false
	END IF
END IF

end event

type dw_add_event from u_datawindow within u_event_log
boolean visible = false
integer y = 904
integer width = 2638
integer height = 692
integer taborder = 100
string dataobject = "d_add_event"
borderstyle borderstyle = styleraised!
end type

event itemchanged;call super::itemchanged;Long		ll_row, ll_return, ll_individual_no, ll_row_count
int			li_count, li_rtn
String	ls_text, ls_event_category_code, ls_event_specific_code, ls_rtn_msg
DATAWINDOWCHILD ldwc_event_specific, ldwc_event_type_code


ls_text = GetText()
ll_row  = GetRow()

Choose Case GetColumnName()
CASE "event_date"
	IF Date(Left(GetText(),10)) < idt_individual_accident_date THEN
		MessageBox("Event Log - Validation Error","The individual event date cannot be less than the minimum accident date for the individual ("+String(idt_individual_accident_date,'yyyy-mm-dd')+").",Exclamation!)
		Return 1
	END IF
/* validation added by Rob Head 98/09/15 as per PR137 */
	IF Date(Left(GetText(),10)) > Date(f_server_datetime()) THEN
		MessageBox("Event Log - Validation Error","The event date cannot be greater than the current date", Exclamation!)
		Return 1
	END IF

CASE "event_type_code"
	IF ls_text = '' THEN
		MessageBox('Need Event Type','You must choose an event type.', StopSign!)
		RETURN 1
	END IF
	
	is_event_type_code = data
	
	li_rtn = THIS.GetChild("event_type_code", ldwc_event_type_code)
	ls_event_category_code = ldwc_event_type_code.getItemString(ldwc_event_type_code.getRow(), 'event_category_code')
	is_event_category_code  = ls_event_category_code

	IF ls_event_category_code = 'C' THEN
		ls_rtn_msg = this.modify("event_specific_code.dddw.name = 'dddw_claim_event_specific_list'")
		THIS.uf_protectcolumn('individual_no', TRUE)
	ELSE
		ls_rtn_msg = this.modify("event_specific_code.dddw.name  ='dddw_individual_event_specific_list'")
		THIS.uf_protectcolumn('individual_no', FALSE)
	END IF
	
	li_rtn = THIS.GetChild("event_specific_code", ldwc_event_specific)
	ldwc_event_specific.setTransObject(SQLCA)
	ldwc_event_specific.retrieve()
	ll_return = ldwc_event_specific.SetFilter("event_type_code = '" + ls_text + "' and active_flag = 'Y'")
	ll_return = ldwc_event_specific.Filter()

	If ldwc_event_specific.RowCount() > 0 Then
		THIS.uf_protectcolumn('event_specific_code', FALSE)
		ls_event_specific_code = ldwc_event_specific.GetItemString(1,"event_specific_code")
		THIS.SetItem(1,"event_specific_code",ls_event_specific_code)
	Else
		THIS.uf_protectcolumn('event_specific_code', TRUE)
		THIS.SetItem(1,"event_specific_code","")
	End If
	
CASE "event_specific_code"
		IF ls_text = '' THEN
			MessageBox('Need Event Type','You must choose an event specific type.', StopSign!)
			RETURN 1
		END IF
		
		is_event_specific_code = data
		
	CASE "individual_no"
		
		il_individual_no = long(data)
End Choose


end event

event rbuttondown;call super::rbuttondown;M_DW_RMB_POPUP lm_popup

IF IsValid(inv_rowselection) Then
	inv_rowselection.event pfc_rbuttondown(xpos,ypos,row,dwo)
End if


/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
lm_popup = Create m_dw_rmb_popup
lm_popup.mf_set_datawindow(This)

// don't display lines
lm_popup.m_options.m_1.Visible = FALSE
lm_popup.m_options.m_2.Visible = FALSE
lm_popup.m_options.m_sort.visible = TRUE
lm_popup.m_options.m_moredetails.visible = TRUE
lm_popup.m_options.m_filterlist.visible = TRUE


lm_popup.m_options.PopMenu(iw_win.PointerX( ), iw_win.PointerY( ))

Destroy lm_popup
end event

type cb_refresh from commandbutton within u_event_log
integer x = 2665
integer y = 32
integer width = 475
integer height = 92
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Clea&r Filter"
end type

event clicked;
is_filter_string = ''
cbx_my_events.checked = false
cbx_include_incoming_corresp.checked = false

f_cancel_event('refresh')

uf_filter_dw(dw_events)
uf_filter_dw(dw_event_comments)
this.enabled = False

end event

type dw_event_comments from u_datawindow within u_event_log
boolean visible = false
integer width = 2633
integer height = 904
integer taborder = 40
string dataobject = "d_events_comments"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event ue_print;STRING	ls_modstring

/*	Note: we override the ancestor here since we want the 
	printed datawindow to be dw_event_comments which
	has the same data as dw_events but includes comments
*/

	ls_modstring = 	't_claim_info.text="' + &
							'Claim: ' + String(il_claim_no) + &
							'    ' + &
							is_last_name + &
							', ' + &
							is_given_names + '"'
	ls_modstring = 	This.Modify(ls_modstring)
	
	This.Print()
	ls_modstring = 	"t_claim_info.text=''"
	ls_modstring = 	This.Modify(ls_modstring)


end event

event rowfocuschanged;call super::rowfocuschanged;LONG			ll_listrow,	ll_nmbr_records, ll_event_no
STRING		ls_type_filter
U_DATAWINDOW	ldw_event

/*	Get out if we're not suppose to be here
*/

	If RowCount() = 0 then
		Return
	End If


/*	Initialize
*/
	ll_listrow = GetRow()
	If ll_listrow = 0 Then
		Return
	End If

/* Highlight the event in the list
*/
	uf_processselect(ll_listrow,"Keyboard")

	// do not continue with script if an auto-add is in progress
	IF is_add_new_event = 'Y' THEN
		RETURN
	END IF	
	
	is_event_category_code = THIS.GetItemString(ll_listrow, 'event_category_code')

	IF is_event_category_code = 'C' THEN
		ldw_event = dw_claim_event
	ELSEIF is_event_category_code = 'I' THEN
		ldw_event = dw_individual_event
	END IF

/*	Retrieve and display event details
*/
	f_switch_datawindow(FALSE) // do not use "add" datawindow
	ll_event_no = GetItemNumber(ll_listrow, 2 )  // event_no
	ll_nmbr_records = ldw_event.Retrieve(ll_event_no, il_claim_no)
	
	IF SQLCA.nf_handle_error("u_event_log","dw_add_event","rowfocuschanged") < 0 THEN
		Return

	ELSEIF ll_nmbr_records = 0 or ll_nmbr_records <> 1 THEN
		MessageBox('Event Module - Data Integrity Error','Error processing event ' + String(ll_event_no) + &
					  '~r~nEvent Details not found in CLAIM_EVENT table~r~nPlease call the Help Desk',StopSign!)
		Return
	END IF

end event

on doubleclicked;call u_datawindow::doubleclicked;	this.PostEvent("ue_more_details")
end on

event rbuttondown;M_DW_RMB_POPUP lm_popup

IF IsValid(inv_rowselection) Then
	inv_rowselection.event pfc_rbuttondown(xpos,ypos,row,dwo)
End if


/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
lm_popup = Create m_dw_rmb_popup
lm_popup.mf_set_datawindow(This)

// don't display lines
lm_popup.m_options.m_1.Visible = FALSE
lm_popup.m_options.m_2.Visible = FALSE
lm_popup.m_options.m_sort.visible = TRUE
lm_popup.m_options.m_moredetails.visible = TRUE
lm_popup.m_options.m_filterlist.visible = TRUE

lm_popup.m_options.PopMenu(iw_win.PointerX( ), iw_win.PointerY( ))

Destroy lm_popup
end event

event ue_more_details;LONG				ll_event_no, ll_docid, ll_fldid
STRING			ls_event_type_code, ls_select, ls_generated_method_code, ls_doc_type
integer			li_rtn
datawindow		ldw_event

	THIS.TriggerEvent(RowFocusChanged!)

	IF is_event_category_code = 'C' THEN
		ldw_event = dw_claim_event
	ELSE
		ldw_event = dw_individual_event
	END IF

	IF ldw_event.RowCount() = 0 THEN
		Return
	END IF

	ls_event_type_code = ldw_event.GetItemString(1,"event_type_code")
	ll_event_no   = ldw_event.GetItemNumber(1,2) // 2nd column is either CLAIM_EVENT.event_no or INDIVIDUAL_EVENT.individual_event_no

/* First we have to find out if the event is manual or automatic
*/
	ls_event_type_code = ldw_event.GetItemString(ldw_event.GetRow(), 'event_type_code')
			
	SELECT	generated_method_code
	INTO		:ls_generated_method_code
	FROM		Event_Type
	WHERE	event_type_code = :ls_event_type_code
	USING	SQLCA;
	
	IF SQLCA.nf_handle_error("u_event_log","Embedded SQL: select generated_method_code from Event_Type","ue_more_details event") < 0 THEN
		Return
	END IF
	
/* If the event is manual, we can exit this script as there is not details to display
*/
	IF ls_generated_method_code = "M" THEN
		MessageBox("Details","There are no more details.")
		Return
	END IF

/* If the event is automatic THEN display the right datawindow to show the event data
	if there are more details to see
*/
	CHOOSE CASE ls_event_type_code
		CASE '004' // Incoming Correspondence
/*		Get the docid and view the document
*/
			SELECT doc_id 
		 	  INTO :ll_docid
			  FROM CORRESP_EVENT_DETAIL
			 WHERE event_no = :ll_event_no
			 USING SQLCA;

				IF SQLCA.nf_handle_error("u_event_log","Embedded SQL: Find docid in CORRESP_EVENT_DETAIL","clicked for cb_details") < 0 THEN
					Return
				END IF
				If ll_docid <= 0 or IsNull(ll_docid) THEN
					MessageBox("View Document","Unable to view document from event log.  Try document list below.")
				ELSE
				
				IF uf_archived_document(ll_docid) THEN
					RETURN
				END IF
				
				if uo_image_append.of_init(ll_docid)	<= 0 then
					RETURN
				end if
					
				ls_doc_type =  uo_image_append.of_get_file_type()
				
				CHOOSE CASE ls_doc_type
					/*  Imaged document */ 
					CASE 'IMA', 'TIF'
						li_rtn = uo_image_append.of_append_image(ll_docid)
						if li_rtn < 0 then
							RETURN
						end if
					case else
						uo_dw_document_path.f_manage_document(ll_docid,"V","NORMAL")
				end choose
			
			END IF

		CASE '005' // Outgoing Correspondence
/*		Get the docid and view the document
*/
			SELECT doc_id 
			  INTO :ll_docid
			  FROM CORRESPONDENCE 
			 WHERE event_no = :ll_event_no
			   AND   claim_no = :il_claim_no
			 USING SQLCA;

			IF SQLCA.nf_handle_error("u_event_log","Embedded SQL: Find docid in CORRESPONDENCE","clicked for cb_details")< 0 THEN
				Return
			END IF

			IF ll_docid <= 0 or IsNull(ll_docid) THEN
				MessageBox("View Document","Unable to view document from event log.  Try document list below.")
				Return
			END IF
			
			IF uf_archived_document(ll_docid) THEN
				RETURN
			END IF
															  
			if uo_image_append.of_init(ll_docid)	<= 0 then
				RETURN
			end if
				
			ls_doc_type =  uo_image_append.of_get_file_type()
			
			CHOOSE CASE ls_doc_type
				/*  Imaged document */ 
				CASE 'IMA', 'TIF'
					li_rtn = uo_image_append.of_append_image(ll_docid)
					if li_rtn < 0 then
						RETURN
					end if
				case else
					uo_dw_document_path.f_manage_document(ll_docid,"V","NORMAL")
			end choose
					

		CASE '010' // Claim Status Change
	
			istr_window_message.as_stringparm[1] = ls_event_type_code
			istr_window_message.al_doubleparm[1] = il_claim_no
			istr_window_message.al_doubleparm[2] = ll_event_no

			OpenWithParm(w_event_details,istr_window_message)

		CASE '014' // Cost Allocation Change
			istr_window_message.as_stringparm[1] = ls_event_type_code
			istr_window_message.al_doubleparm[1] = il_claim_no
			istr_window_message.al_doubleparm[2] = ll_event_no

			OpenWithParm(w_event_details,istr_window_message)

		CASE ELSE
			MessageBox("Event Type","There are no further details for this event type")

END CHOOSE

end event

event ue_filter;call super::ue_filter;
cb_open_filter_window.triggerEvent(Clicked!)
end event

type dw_events from u_datawindow within u_event_log
integer width = 2633
integer height = 904
integer taborder = 10
string dataobject = "d_events"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event ue_filter;call super::ue_filter;
cb_open_filter_window.triggerEvent(Clicked!)

end event

on ue_print;dw_event_comments.TriggerEvent("ue_print")
end on

event rowfocuschanged;call super::rowfocuschanged;LONG  ll_listrow,	ll_nmbr_records, ll_event_no
INT	li_row
U_DATAWINDOW ldw_event

/*	Get out if we're not suppose to be here
*/
	If RowCount() = 0 then
		Return
	End If

/*	Initialize
*/
	ll_listrow = GetRow()
	If ll_listrow = 0 Then
		Return
	End If

/* Highlight the event in the list
*/
	uf_processselect(ll_listrow,"Keyboard")
	
	// do not continue with script if an auto-add is in progress
	IF is_add_new_event = 'Y' THEN
		RETURN
	END IF
	
	is_event_category_code = THIS.GetItemString(ll_listrow, 'event_category_code')

	IF is_event_category_code = 'C' THEN
		ldw_event = dw_claim_event
	ELSEIF is_event_category_code = 'I' THEN
		ldw_event = dw_individual_event
	END IF

/*	Retrieve and display event details
*/
	f_switch_datawindow(FALSE) // do not use "add" datawindow
	ll_event_no = GetItemNumber(ll_listrow, 2 )  // event_no
	ll_nmbr_records = ldw_event.Retrieve(ll_event_no, il_claim_no)

	IF SQLCA.nf_handle_error("u_event_log","dw_add_event","rowfocuschanged") < 0 THEN
		Return

	ELSEIF ll_nmbr_records = 0 or ll_nmbr_records <> 1 THEN
		MessageBox('Event Module - Data Integrity Error','Error processing event ' + String(ll_event_no) + &
					  '~r~nEvent Details not found in CLAIM_EVENT table~r~nPlease call the Help Desk',StopSign!)
		Return
	END IF
end event

event rbuttondown;M_DW_RMB_POPUP lm_popup

IF IsValid(inv_rowselection) Then
	inv_rowselection.event pfc_rbuttondown(xpos,ypos,row,dwo)
End if


/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
lm_popup = Create m_dw_rmb_popup
lm_popup.mf_set_datawindow(This)

// don't display lines
lm_popup.m_options.m_1.Visible = FALSE
lm_popup.m_options.m_2.Visible = FALSE
lm_popup.m_options.m_sort.visible = TRUE
lm_popup.m_options.m_moredetails.visible = TRUE
lm_popup.m_options.m_filterlist.visible = TRUE

lm_popup.m_options.PopMenu(iw_win.PointerX(), iw_win.PointerY())

Destroy lm_popup
end event

event ue_more_details;LONG					ll_event_no, ll_docid, ll_fldid
STRING				ls_select, ls_event_type_code, ls_generated_method_code, ls_doc_type
integer				li_rtn
DataWindow		ldw_event

	THIS.TriggerEvent(RowFocusChanged!)
	
	IF is_event_category_code = 'C' THEN
		ldw_event = dw_claim_event
	ELSE
		ldw_event = dw_individual_event
	END IF
	
	//no records return out.
	IF ldw_event.RowCount() = 0 THEN Return

	ls_event_type_code = ldw_event.GetItemString(1,"event_type_code")
	ll_event_no   = ldw_event.GetItemNumber(1,2) // 2nd column is either CLAIM_EVENT.event_no or INDIVIDUAL_EVENT.individual_event_no

/* First we have to find out if the event is manual or automatic
*/
	ls_event_type_code = ldw_event.GetItemString(ldw_event.GetRow(), 'event_type_code')
			
	SELECT	generated_method_code
	INTO		:ls_generated_method_code
	FROM		Event_Type
	WHERE	event_type_code = :ls_event_type_code
	USING	SQLCA;
	
	IF SQLCA.nf_handle_error("u_event_log","Embedded SQL: select generated_method_code from Event_Type","ue_more_details event") < 0 THEN
		Return
	END IF
	
/* If the event is manual, we can exit this script as there is not details to display
*/
	IF ls_generated_method_code = "M" THEN
		MessageBox("Details","There are no more details.")
		Return
	END IF

/* If the event is automatic THEN display the right datawindow to show the event data
	if there are more details to see
*/
	CHOOSE CASE ls_event_type_code
		CASE '004' // Incoming Correspondence
/*		Get the docid and view the document
*/
			SELECT doc_id 
		 	  INTO :ll_docid
			  FROM CORRESP_EVENT_DETAIL
			 WHERE event_no = :ll_event_no
			 	AND claim_no = :il_claim_no
			 USING SQLCA;

				IF SQLCA.nf_handle_error("u_event_log","Embedded SQL: Find docid in CORRESP_EVENT_DETAIL","clicked for cb_details") < 0 THEN
					Return
				END IF
				If ll_docid <= 0 or IsNull(ll_docid) THEN
					MessageBox("View Document","Unable to view document from event log.  Try document list below.")
				ELSE
				
				IF uf_archived_document(ll_docid) THEN
					RETURN
				END IF
					
				if uo_image_append.of_init(ll_docid)	<= 0 then
					RETURN
				end if
					
					
				ls_doc_type =  uo_image_append.of_get_file_type()
					
				
				CHOOSE CASE ls_doc_type
					/*  Imaged document */ 
					CASE 'IMA', 'TIF'
						li_rtn = uo_image_append.of_append_image(ll_docid)
						if li_rtn < 0 then
							RETURN
						end if
					case else
						uo_dw_document_path.f_manage_document(ll_docid,"V","NORMAL")
				end choose
						
				
			
		
			END IF

		CASE '005' // Outgoing Correspondence
/*		Get the docid and view the document
*/
			SELECT doc_id 
			  INTO :ll_docid
			  FROM CORRESPONDENCE 
			 WHERE event_no = :ll_event_no
			   AND   claim_no = :il_claim_no
			 USING SQLCA;

			IF SQLCA.nf_handle_error("u_event_log","Embedded SQL: Find docid in CORRESPONDENCE","clicked for cb_details")< 0 THEN
				Return
			END IF

			IF ll_docid <= 0 or IsNull(ll_docid) THEN
				MessageBox("View Document","Unable to view document from event log.  Try document list below.")
				Return
			END IF
			
			IF uf_archived_document(ll_docid) THEN
				RETURN
			END IF
					
			if uo_image_append.of_init(ll_docid)	<= 0 then
				RETURN
			end if
							
			ls_doc_type =  uo_image_append.of_get_file_type()
				
			
			CHOOSE CASE ls_doc_type
				/*  Imaged document */ 
				CASE 'IMA', 'TIF'
					li_rtn = uo_image_append.of_append_image(ll_docid)
					if li_rtn < 0 then
						RETURN
					end if
				case else
					uo_dw_document_path.f_manage_document(ll_docid,"V","NORMAL")
			end choose
						

		CASE '010' // Claim Status Change
	
			istr_window_message.as_stringparm[1] = ls_event_type_code
			istr_window_message.al_doubleparm[1] = il_claim_no
			istr_window_message.al_doubleparm[2] = ll_event_no

			OpenWithParm(w_event_details,istr_window_message)

		CASE '014' // Cost Allocation Change
			istr_window_message.as_stringparm[1] = ls_event_type_code
			istr_window_message.al_doubleparm[1] = il_claim_no
			istr_window_message.al_doubleparm[2] = ll_event_no

			OpenWithParm(w_event_details,istr_window_message)

		CASE ELSE
			MessageBox("Event Type","There are no further details for this event type")

END CHOOSE
end event

on doubleclicked;call u_datawindow::doubleclicked;	this.PostEvent("ue_more_details")
end on

event retrieveend;call super::retrieveend;IF is_find_event_flag = 'Y' THEN
	POST uf_find_event()
END IF
end event

type uo_image_append from u_image_append within u_event_log
integer x = 553
integer y = 600
integer taborder = 40
end type

on uo_image_append.destroy
call u_image_append::destroy
end on

type uo_dw_document_path from u_dw_document_path within u_event_log
boolean visible = false
integer x = 1518
integer y = 1212
integer taborder = 100
end type

type dw_claim_event from u_datawindow within u_event_log
integer y = 904
integer width = 2638
integer height = 676
integer taborder = 90
boolean bringtotop = true
string dataobject = "d_claim_event2"
boolean border = false
end type

event rbuttondown;M_DW_RMB_POPUP lm_popup

IF IsValid(inv_rowselection) Then
	inv_rowselection.event pfc_rbuttondown(xpos,ypos,row,dwo)
End if


/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
lm_popup = Create m_dw_rmb_popup
lm_popup.mf_set_datawindow(This)

// don't display lines
lm_popup.m_options.m_1.Visible = FALSE
lm_popup.m_options.m_2.Visible = FALSE
lm_popup.m_options.m_sort.visible = TRUE
lm_popup.m_options.m_moredetails.visible = TRUE
lm_popup.m_options.m_filterlist.visible = TRUE


lm_popup.m_options.PopMenu(iw_win.PointerX( ), iw_win.PointerY( ))

Destroy lm_popup
end event

event ue_print;DATASTORE lds_print_event
LONG ll_claim_no, ll_event_no

lds_print_event = CREATE DATASTORE
lds_print_event.DataObject = 'd_claim_event_print'
lds_print_event.SetTransObject(SQLCA)

IF THIS.GetRow() > 0 THEN
	ll_claim_no = THIS.GetItemNumber(THIS.GetRow(), "claim_no")
	ll_event_no = THIS.GetItemNumber(THIS.GetRow(), "event_no")
	lds_print_event.Retrieve(ll_event_no, ll_claim_no)

	IF lds_print_event.RowCount() > 0 THEN
		lds_print_event.Print()
	END IF
END IF
end event

type dw_individual_event from u_datawindow within u_event_log
integer y = 904
integer width = 2647
integer height = 692
integer taborder = 80
boolean bringtotop = true
string dataobject = "d_individual_event"
boolean border = false
end type

event rbuttondown;M_DW_RMB_POPUP lm_popup

IF IsValid(inv_rowselection) Then
	inv_rowselection.event pfc_rbuttondown(xpos,ypos,row,dwo)
End if


/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
lm_popup = Create m_dw_rmb_popup
lm_popup.mf_set_datawindow(This)

// don't display lines
lm_popup.m_options.m_1.Visible = FALSE
lm_popup.m_options.m_2.Visible = FALSE
lm_popup.m_options.m_sort.visible = TRUE
lm_popup.m_options.m_moredetails.visible = TRUE
lm_popup.m_options.m_filterlist.visible = TRUE


lm_popup.m_options.PopMenu(iw_win.PointerX( ), iw_win.PointerY( ))

Destroy lm_popup
end event

event ue_print;DATASTORE lds_print_event
LONG  ll_claim_no, ll_event_no

lds_print_event = CREATE DATASTORE
lds_print_event.DataObject = 'd_individual_event_print'
lds_print_event.SetTransObject(SQLCA)

IF THIS.GetRow() > 0 THEN
	ll_claim_no = THIS.GetItemNumber(THIS.GetRow(), "claim_no")
	ll_event_no = THIS.GetItemNumber(THIS.GetRow(), "individual_event_no")
	lds_print_event.Retrieve(ll_event_no, ll_claim_no)

	IF lds_print_event.RowCount() > 0 THEN
		lds_print_event.Print()
	END IF
END IF
end event

