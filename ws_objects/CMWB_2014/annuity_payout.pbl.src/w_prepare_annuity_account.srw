$PBExportHeader$w_prepare_annuity_account.srw
$PBExportComments$Common - Ancestor window with security code, one datawindow and a standard print event.  Used primarily for reports.
forward
global type w_prepare_annuity_account from window
end type
type st_select_annuity_payout from statictext within w_prepare_annuity_account
end type
type cb_manual_annuity_payout from commandbutton within w_prepare_annuity_account
end type
type cb_reset_sort from commandbutton within w_prepare_annuity_account
end type
type dw_payout_region from u_datawindow within w_prepare_annuity_account
end type
type tab_payout from tab within w_prepare_annuity_account
end type
type tabpage_participants from userobject within tab_payout
end type
type cb_cancel from commandbutton within tabpage_participants
end type
type cb_save from commandbutton within tabpage_participants
end type
type cb_delete from commandbutton within tabpage_participants
end type
type cb_new from commandbutton within tabpage_participants
end type
type cb_add from commandbutton within tabpage_participants
end type
type cb_details from commandbutton within tabpage_participants
end type
type dw_participant_details from u_datawindow within tabpage_participants
end type
type dw_participants from u_datawindow within tabpage_participants
end type
type tabpage_participants from userobject within tab_payout
cb_cancel cb_cancel
cb_save cb_save
cb_delete cb_delete
cb_new cb_new
cb_add cb_add
cb_details cb_details
dw_participant_details dw_participant_details
dw_participants dw_participants
end type
type tabpage_payout_summary from userobject within tab_payout
end type
type dw_annuity_payout_summary from u_datawindow within tabpage_payout_summary
end type
type tabpage_payout_summary from userobject within tab_payout
dw_annuity_payout_summary dw_annuity_payout_summary
end type
type tabpage_payout_details from userobject within tab_payout
end type
type dw_annuity_payout_detail from u_datawindow within tabpage_payout_details
end type
type tabpage_payout_details from userobject within tab_payout
dw_annuity_payout_detail dw_annuity_payout_detail
end type
type tabpage_annuity_payout_txn_detail from userobject within tab_payout
end type
type dw_annuity_payout_txn_detail from u_datawindow within tabpage_annuity_payout_txn_detail
end type
type tabpage_annuity_payout_txn_detail from userobject within tab_payout
dw_annuity_payout_txn_detail dw_annuity_payout_txn_detail
end type
type tabpage_overpayment from userobject within tab_payout
end type
type dw_annuity_overpayment from u_datawindow within tabpage_overpayment
end type
type tabpage_overpayment from userobject within tab_payout
dw_annuity_overpayment dw_annuity_overpayment
end type
type tabpage_payout_recipients from userobject within tab_payout
end type
type st_recipient_save_needed from statictext within tabpage_payout_recipients
end type
type dw_payout_recipient_details from u_datawindow within tabpage_payout_recipients
end type
type cb_cancel_annuity_payout_recipient from commandbutton within tabpage_payout_recipients
end type
type cb_save_annuity_payout_recipient from commandbutton within tabpage_payout_recipients
end type
type dw_payout_recipients from u_datawindow within tabpage_payout_recipients
end type
type dw_recipient_report from u_datawindow within tabpage_payout_recipients
end type
type tabpage_payout_recipients from userobject within tab_payout
st_recipient_save_needed st_recipient_save_needed
dw_payout_recipient_details dw_payout_recipient_details
cb_cancel_annuity_payout_recipient cb_cancel_annuity_payout_recipient
cb_save_annuity_payout_recipient cb_save_annuity_payout_recipient
dw_payout_recipients dw_payout_recipients
dw_recipient_report dw_recipient_report
end type
type tabpage_contract_and_beneficiaries from userobject within tab_payout
end type
type st_contract_save_needed from statictext within tabpage_contract_and_beneficiaries
end type
type cb_cancel_contract from commandbutton within tabpage_contract_and_beneficiaries
end type
type cb_delete_beneficiary from commandbutton within tabpage_contract_and_beneficiaries
end type
type cb_add_beneficiary from commandbutton within tabpage_contract_and_beneficiaries
end type
type cb_delete_contract from commandbutton within tabpage_contract_and_beneficiaries
end type
type cb_add_contract from commandbutton within tabpage_contract_and_beneficiaries
end type
type cb_save_contract from commandbutton within tabpage_contract_and_beneficiaries
end type
type dw_contract_beneficiaries_details from u_datawindow within tabpage_contract_and_beneficiaries
end type
type dw_contract_beneficiaries from u_datawindow within tabpage_contract_and_beneficiaries
end type
type tabpage_contract_and_beneficiaries from userobject within tab_payout
st_contract_save_needed st_contract_save_needed
cb_cancel_contract cb_cancel_contract
cb_delete_beneficiary cb_delete_beneficiary
cb_add_beneficiary cb_add_beneficiary
cb_delete_contract cb_delete_contract
cb_add_contract cb_add_contract
cb_save_contract cb_save_contract
dw_contract_beneficiaries_details dw_contract_beneficiaries_details
dw_contract_beneficiaries dw_contract_beneficiaries
end type
type tab_payout from tab within w_prepare_annuity_account
tabpage_participants tabpage_participants
tabpage_payout_summary tabpage_payout_summary
tabpage_payout_details tabpage_payout_details
tabpage_annuity_payout_txn_detail tabpage_annuity_payout_txn_detail
tabpage_overpayment tabpage_overpayment
tabpage_payout_recipients tabpage_payout_recipients
tabpage_contract_and_beneficiaries tabpage_contract_and_beneficiaries
end type
type rb_region from radiobutton within w_prepare_annuity_account
end type
type rb_individual from radiobutton within w_prepare_annuity_account
end type
type cb_save_region from commandbutton within w_prepare_annuity_account
end type
type uo_filter_control from u_dw_filter_control within w_prepare_annuity_account
end type
type st_splitbar_1 from u_splitbar_horizontal within w_prepare_annuity_account
end type
type dw_payout_accounts from u_datawindow within w_prepare_annuity_account
end type
type uo_prepare_checklist from u_checklist within w_prepare_annuity_account
end type
type uo_confirm_payout_checklist from u_checklist within w_prepare_annuity_account
end type
type dw_annuity_history from u_datawindow within w_prepare_annuity_account
end type
type st_multiple_accounts from statictext within w_prepare_annuity_account
end type
end forward

global type w_prepare_annuity_account from window
integer width = 5952
integer height = 2860
boolean titlebar = true
string title = "Prepare Annuity Payout"
string menuname = "m_annuity"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 67108864
string icon = "AppIcon!"
boolean clientedge = true
boolean center = true
event ue_post_open ( )
st_select_annuity_payout st_select_annuity_payout
cb_manual_annuity_payout cb_manual_annuity_payout
cb_reset_sort cb_reset_sort
dw_payout_region dw_payout_region
tab_payout tab_payout
rb_region rb_region
rb_individual rb_individual
cb_save_region cb_save_region
uo_filter_control uo_filter_control
st_splitbar_1 st_splitbar_1
dw_payout_accounts dw_payout_accounts
uo_prepare_checklist uo_prepare_checklist
uo_confirm_payout_checklist uo_confirm_payout_checklist
dw_annuity_history dw_annuity_history
st_multiple_accounts st_multiple_accounts
end type
global w_prepare_annuity_account w_prepare_annuity_account

type variables
BOOLEAN           I_Authorized_Access  //flag to check window security

LONG              il_design_time_height
LONG              il_design_time_width
LONG              il_workspace_width_diff
LONG              il_workspace_height_diff

n_resize          inv_resize

n_common_annuity  inv_common_annuity
n_annuity_participant     inv_annuity_participant

n_checklist       inv_prepare_checklist, inv_confirm_payout_checklist

BOOLEAN           ib_checklist_posted, ib_checklist_cancelled, ib_save
BOOLEAN           ib_payout_with_dependants, ib_no_payout, ib_checklist_completed
BOOLEAN           ib_more_than_forty_chars, ib_total_writeoff, ib_auto_authorization
BOOLEAN           ib_garnishment, ib_different_participant_list, ib_can_change_participants
BOOLEAN           ib_optional_user_payout_comment, ib_required_user_payout_comment
BOOLEAN           ib_not_eligible, ib_future_end_date, ib_no_benefit_entitlement
BOOLEAN           ib_annuity_previously_paid, ib_reduction_of_claim_annuity_benefits
BOOLEAN           ib_opening, ib_previous_manual_intervention, ib_non_zero_subledger_warning
BOOLEAN           ib_reverse_confirm_OP_recovery, ib_reverse_request_payout
BOOLEAN           ib_modified_annuity_calc_method

ULONG				   iul_handle

s_window_message	istr_CAE_message

LONG              il_annuity_payout_no
LONG              il_annuity_account_no
LONG              il_annuity_eligibility_no
LONG              il_annuity_calc_no
LONG              il_individual_no, il_forced_individual_no
LONG              il_annuity_account_claim_no
LONG              il_checklist_no
LONG              il_payout_list_column
LONG              il_previously_completed_payout_no

INTEGER           ii_dependant_txn_count

STRING            is_admin_region_code, is_claim_role_code, is_checklist_type_code, is_pap_open_mode
STRING            is_annuity_payout_reason_message
STRING            is_purchase_name_on_cheque
STRING            is_prev_annuity_annuity_payout_calc_method_code

U_DS              ids_annuity_payout_details_SP
U_DS              ids_non_whscc_recipients
U_DS              ids_populate_initial_annuity_payout_balances

U_DATAWINDOW      idw_participants, idw_participant_details
U_DATAWINDOW      idw_annuity_payout_summary, idw_annuity_payout_detail
U_DATAWINDOW      idw_annuity_payout_txn_detail
U_DATAWINDOW      idw_payout_recipients, idw_payout_recipient_details, idw_payout_recipient_report
U_DATAWINDOW      idw_annuity_overpayment
U_DATAWINDOW      idw_contract_beneficiaries, idw_contract_beneficiaries_details

UserObject iuo_preview_tab

W_PREPARE_ANNUITY_ACCOUNT iw_win

CONSTANT INTEGER	PARTICIPANT    = 1
CONSTANT INTEGER	PAYOUT_SUMMARY = 2
CONSTANT INTEGER  PAYOUT_DETAILS = 3
CONSTANT INTEGER  TXN_DETAILS    = 4
CONSTANT INTEGER  OP_RECOVERY    = 5
CONSTANT INTEGER  RECIPIENTS     = 6
CONSTANT INTEGER  CONTRACT       = 7


end variables

forward prototypes
public subroutine wf_setresize (boolean ab_switch)
public subroutine wf_post_resize_checklist_position ()
public function integer wf_create_cap_checklist ()
public function integer wf_get_active_checklist_count ()
public subroutine wf_clear_filter ()
public function integer wf_set_claim (long al_claim_no)
public subroutine wf_create_request_authorization_event (string as_window_to_open)
public function integer wf_request_authorization_event_count ()
public function boolean wf_enable_overpayment_buttons (long al_checklist_no)
public function integer wf_annuity_payout_corresp_count ()
public subroutine wf_create_request_payout_event (string as_window_to_open)
public function w_prepare_annuity_account wf_get_window_reference ()
public function integer wf_confirm_annuity_eligibility ()
public subroutine wf_retrieve_tab (integer ai_selected_tab)
public subroutine wf_enable_participant_buttons (boolean ab_enable)
public function long wf_get_claim_no ()
public function string wf_get_claim_role_code ()
public function long wf_get_individual_no ()
public function string wf_get_module_code ()
public subroutine wf_enable_controls (boolean ab_enabled)
public subroutine wf_scroll_to_individual (long al_individual_no)
public function integer wf_verify_payout_authorization ()
public function integer wf_verify_annuity_payout_letter ()
public subroutine wf_payout_has_dependants ()
public function boolean wf_display_claim_details_tab (long al_paa_checklist_no)
public function integer wf_request_payout ()
public subroutine wf_filter_participant_dddws (boolean ab_filter)
public subroutine wf_clear_datawindows ()
public function integer wf_check_bus_rule_annuity_contract ()
public function boolean wf_check_maintain_annuity_contract (long al_annuity_payout_no)
public subroutine wf_enable_contract_buttons (boolean ab_state)
public function long wf_get_next_contract_no ()
public function boolean wf_display_overpayment_tab (long al_paa_checklist_no)
public subroutine wf_update_annuity_payout_status (string as_annuity_payout_status_code, string as_annuity_payout_status_reason_code, string as_comment)
public subroutine wf_reject_checklist_status_change (boolean ab_message, string as_message_title, string as_message, long al_checklist_no, string as_checklist_type_code, n_checklist anv_checklist, ref u_checklist_datawindow adw_checklist)
public function integer wf_auto_complete_next_step (string as_checklist_step_type_code, string as_checklist_step_status_code, ref u_checklist_datawindow adw_dw, string as_data, string as_status_assigned_method_code, ref u_checklist a_checklist, ref n_checklist anv_checklist, ref boolean ab_commit)
public subroutine wf_post_hscrollbar_position ()
public function integer wf_verify_recipient_address ()
public function integer wf_verify_contract_confirmed ()
public subroutine wf_confirm_payout_amount ()
public function integer wf_unprocessed_payout_txn_count ()
public function integer wf_unprocessed_subledger_txn_count ()
public function integer wf_verify_subledger (ref string as_error_message)
public subroutine wf_post_commit_retrieval ()
public function string wf_get_annuity_payout_type ()
public subroutine wf_get_current_name_address (long al_annuity_payout_recipient_no, string as_annuity_payout_recipient_type_code, ref string as_current_name, ref string as_current_address_line1, ref string as_current_address_line2, ref string as_current_address_line3, ref string as_current_address_line4, ref string as_current_address_line5)
public function integer wf_determine_name_address_change ()
public subroutine wf_get_current_name (long al_annuity_payout_recipient_no, string as_annuity_payout_recipient_type_code, ref string as_current_name)
public function integer wf_determine_lump_sum_recipient (ref string as_error_message)
public subroutine wf_set_address_line_display_only (integer ai_recipient_row)
public function boolean wf_determine_total_writeoff ()
public function integer wf_confirm_overpayment_recovery ()
public function boolean wf_display_recipients_tab ()
public function boolean wf_display_contracts_tab ()
public subroutine wf_reverse_confirm_op_recovery ()
public subroutine wf_reverse_request_payout ()
public subroutine wf_annuity_payout_comment (string as_payout)
public function boolean wf_display_txn_details_tab ()
public function boolean wf_display_payout_summary_tab ()
public function integer wf_orphan_participant (ref string as_orphan_message)
public function integer wf_determine_participant_garnishment ()
public function integer wf_validate_writeoff_flag ()
public subroutine wf_determine_writeoff_on_last_payout ()
public function boolean wf_enable_manual_payout_btn ()
public subroutine wf_enable_annuity_admin_region ()
public function integer wf_subledger_payout_match (ref decimal adec_sub_ledger, ref decimal adec_payout)
public subroutine wf_report ()
public subroutine wf_print ()
public function integer wf_no_payout_subledger_warning (string as_annuity_payout_status_reason_code)
public subroutine wf_build_annuity_payout_message (string as_annuity_payout_status_code, string as_annuity_payout_status_reason_code)
public function integer wf_post_cae_no_payout ()
public subroutine wf_initial_annuity_payout_population ()
public function integer wf_request_payout_event_count (ref string as_request_type_code)
public subroutine wf_determine_cap_checklist_type (ref string as_checklist_type_code)
public subroutine wf_determine_prev_manual_intervention ()
public subroutine wf_determine_eligibility (long al_annuity_eligibility_no, ref string as_annuity_eligibility_status_code, ref datetime adtm_annuity_start_date, ref datetime adtm_annuity_end_date)
public subroutine wf_get_payout_annuity_eligibility_no ()
public subroutine wf_determine_prev_completed_with_payout ()
public function long wf_determine_no_payout_reason (ref u_checklist_datawindow adw_dw)
public function integer wf_build_annuity_purchase_name_on_chq (ref string as_purchase_name_on_cheque, ref boolean ab_more_than_forty_chars, boolean ab_display_messagebox, string as_checklist_step_desc, string as_new_annuity_carrier_name)
public subroutine wf_auto_request_authorization (string as_checklist_type_code)
public function integer wf_reinsert_onto_annuity_payout_list ()
public subroutine wf_determine_annuity_calc_method_change (ref string as_msg)
end prototypes

event ue_post_open();LONG           ll_cap_checklist_no, ll_history_annuity_payout_no
STRING         ls_name, ls_sql
INT            li_find_row, li_rows, li_inserted_row, li_rtn
U_CHECKLIST    l_checklist
U_DATAWINDOW   ldw_dw[]
DatawindowChild ldw_child


IF dw_payout_accounts.RowCount() > 0 THEN
	dw_payout_accounts.ScrollToRow(1)
	dw_payout_accounts.SelectRow(1,TRUE)
	
	// set checklist bar to display name and individual number
	ls_name = dw_payout_accounts.GetItemString(1,'full_name') + ' - ' + String(il_individual_no)
	
	ll_cap_checklist_no = dw_payout_accounts.GetItemNumber(1,'confirm_annuity_payout_checklist_no')
	IF ll_cap_checklist_no = 0 THEN
		// incomplete PAA checklist
		l_checklist = uo_prepare_checklist
	ELSE
		l_checklist = uo_confirm_payout_checklist
	END IF
	
	l_checklist.uf_set_checklistbar_text(ls_name)
	l_checklist.uf_set_historybar_text(ls_name)
	l_checklist.uf_set_notesbar_text(ls_name)
END IF

ldw_dw[1] =  idw_participants    // participant list
ldw_dw[2] =  idw_participant_details
inv_annuity_participant.nf_set_datawindow(ldw_dw, SQLCA)
inv_annuity_participant.nf_set_commit(true) // allow this object to commit changes

/* maximize frame while module is envoked */
THIS.WindowState = Maximized!

//  For 'report' mode, check in the dw-child of dw_annuity_history, find the row that contains a payout that is  
// currently in-progress, if one exists, and make that the default row and trigger the item change to do the filtering
IF is_pap_open_mode = 'inquiry' THEN
	dw_annuity_history.visible = true
	dw_annuity_history.enabled = true

   
	
	dw_annuity_history.getChild('display', ldw_child)
	ldw_child.setTransObject(SQLCA)
	ldw_child.retrieve(il_annuity_account_no)
	SQLCA.nf_handle_error('w_prepare_annuity_account','ldw_child.retrieve','ue_post_open')
		
	/*	Insert a row */
	li_inserted_row = dw_annuity_history.InsertRow(0)
	SQLCA.nf_handle_error('w_prepare_annuity_account','dw_annuity_history.InsertRow','ue_post_open event')

	ls_sql = "annuity_payout_status_code = 'I'"
	
	li_find_row = ldw_child.find(ls_sql, 1, ldw_child.rowCount())
	IF li_find_row <= 0 THEN
		li_find_row = 1
	END IF	
	
	ldw_child.setRow(li_find_row)
	ldw_child.scrollToRow(li_find_row)
	
	ll_history_annuity_payout_no = ldw_child.GetItemNumber(li_find_row,'annuity_payout_no')
	
	dw_annuity_history.SetItem(li_inserted_row,'annuity_payout_no',ll_history_annuity_payout_no)
	dw_annuity_history.SetItem(li_inserted_row,'display',ldw_child.GetItemString(li_find_row,'drop_down_display'))
	
	dw_annuity_history.triggerevent(itemchanged!) //forces the dw_payout_accounts to be filtered etc
	
ELSE
	// set up this datastore for use in completing the CAE checklist step
	ids_populate_initial_annuity_payout_balances = Create U_DS
	ids_populate_initial_annuity_payout_balances.DataObject = 'ds_populate_initial_annuity_payout_balances'
	li_rtn = ids_populate_initial_annuity_payout_balances.SetTransObject(SQLCA)
END IF



dw_payout_accounts.setredraw(true)
end event

public subroutine wf_setresize (boolean ab_switch);

IF ab_switch = True Then
	IF il_design_time_height = 0 or il_design_time_width = 0 THEN
		SignalError(-666,'The resize service requires that both the il_design_time_height and il_design_time_width be filled in.')
	End if
	
	/* default instance of the resize object */
	IF IsNull(inv_resize) OR NOT IsValid (inv_resize) THEN
		inv_resize = create n_resize
		If this.WindowType = Child! Then
			inv_resize.of_SetOrigSize (il_design_time_width , il_design_time_height)
			inv_resize.of_SetMinSize (il_design_time_width , il_design_time_height)
		Else
			inv_resize.of_SetOrigSize (il_design_time_width - il_workspace_width_diff, il_design_time_height - il_workspace_height_diff)
			inv_resize.of_SetMinSize (il_design_time_width - il_workspace_width_diff, il_design_time_height - il_workspace_height_diff)
		End if
	END IF 
Else
	Destroy inv_resize
End if
end subroutine

public subroutine wf_post_resize_checklist_position ();LONG		ll_x , ll_width


IF ib_checklist_posted THEN
	
	ll_x = THIS.x
	ll_width = THIS.width
	
	// prepare account checklist
	IF uo_prepare_checklist.ib_maximized THEN
		// minimize the checklist before resize
		uo_prepare_checklist.p_right_arrow.triggerevent(clicked!)
	END IF

	uo_prepare_checklist.il_maximized_x = (ll_width - 200) - 2076
	uo_prepare_checklist.il_minimized_x = ll_width - 200
		
	uo_prepare_checklist.x = ll_width - 200
	
	
	// confirm payout checklist
	IF uo_confirm_payout_checklist.ib_maximized THEN
		// minimize the checklist before resize
		uo_confirm_payout_checklist.p_right_arrow.triggerevent(clicked!)
	END IF

	uo_confirm_payout_checklist.il_maximized_x = (ll_width - 200) - 2076
	uo_confirm_payout_checklist.il_minimized_x = ll_width - 200
		
	uo_confirm_payout_checklist.x = ll_width - 200
	
	ib_checklist_posted = FALSE	
	
END IF
end subroutine

public function integer wf_create_cap_checklist ();DATETIME    ldtm_current
INTEGER     li_count, li_rtn
LONG        ll_related_checklist_no, ll_new_checklist_no, ll_checklist_subscriber_no
STRING      ls_annuity_payout_type_code
w_prepare_annuity_account   lw_win

/*
after PAA checklist is done, create a new CAP (CAPLS/CAPAP) checklist
*/

IF is_checklist_type_code = 'CAPAP' THEN
	ls_annuity_payout_type_code = 'P'
ELSE
	ls_annuity_payout_type_code = 'LS'
END IF


// get related checklist number, if any exists
ll_related_checklist_no = inv_common_annuity.nf_get_related_checklist_no(il_annuity_account_no, is_checklist_type_code)

li_count = wf_get_active_checklist_count()
IF li_count > 0 THEN
	MessageBox('Data Integrity problem','The module was trying to create a new checklist, but there is already an incomplete checklist for this individual '+String(il_individual_no)+'. Call the HELPDESK.')
	RETURN -1
END IF

// insert records into CHECKLIST, CHECKLIST_STEP
ll_new_checklist_no = inv_confirm_payout_checklist.nf_create_checklist('050', is_checklist_type_code, 'IA','A', 'INA','A', ll_related_checklist_no) // Prepare Annuity Account & Request Payout module = 050
IF ll_new_checklist_no <= 0 OR IsNull(ll_new_checklist_no) THEN
	MessageBox('Data Integrity Problem','The checklist was not created.')
	RETURN -1
END IF

// get subscriber_no and insert subscriber_checklist_xref
ll_checklist_subscriber_no = inv_common_annuity.nf_get_ann_acct_checklist_subscriber_no(il_annuity_account_no)
IF ll_checklist_subscriber_no <= 0 OR IsNull(ll_checklist_subscriber_no) THEN
	MessageBox('Data Integrity Problem','The checklist subscriber does not exist. Call HELPDESK.')
	RETURN -1
END IF
inv_confirm_payout_checklist.nf_insert_subscriber_checklist_xref(ll_checklist_subscriber_no,ll_new_checklist_no)


// set the first step of the CAP checklist to be completed
ldtm_current = f_server_datetime()
inv_confirm_payout_checklist.nf_save_checklist_step(ll_new_checklist_no, '001', 'COA', ldtm_current, vgst_user_profile.user_id, '','050',is_checklist_type_code,'A')

// set up checklist object
uo_prepare_checklist.Visible = FALSE

inv_confirm_payout_checklist.nf_retrieve_checklists(is_checklist_type_code, ll_new_checklist_no)

wf_post_resize_checklist_position()

uo_confirm_payout_checklist.Visible = TRUE

// will be committed through PAA checklist
//dw_payout_accounts.SetItem(dw_payout_accounts.GetRow(),'confirm_annuity_payout_checklist_no',ll_new_checklist_no)
//dw_payout_accounts.Update()

UPDATE ANNUITY_PAYOUT
SET    prepared_by_user_id      = :vgst_user_profile.user_id,
		 prepared_date            = :ldtm_current,
		 annuity_payout_type_code = :ls_annuity_payout_type_code,
		 confirm_annuity_payout_checklist_no = :ll_new_checklist_no
WHERE  annuity_payout_no = :il_annuity_payout_no
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account', 'embedded SQL: UPDATE ANNUITY_PAYOUT...', 'wf_create_cap_checklist')

IF ls_annuity_payout_type_code = 'P' THEN
	// if the user has decided to complete this payout as an annuity purchase
	// then the annuity payout recipient must be set to the recipient number & type of the eligible individual (IW/SS)
	
	// also the recipient name on record for annuity account individual must be updated to the eligible individual's current name
	// also, blank out the address data, because we force users to enter this later in CAPAP checklist.
	UPDATE a
	SET    a.name_on_cheque = e.given_names + ' ' + e.last_name + ' & ANNUITY CARRIER',
			 a.annuity_payout_recipient_no = c.recipient_no,
			 a.annuity_payout_recipient_type_code = c.recipient_type_code,
			 a.address_line1  = '',
			 a.address_line2  = '',
			 a.address_line3  = '',
			 a.address_line4  = '',
			 a.address_line5  = ''
	FROM   ANNUITY_PAYOUT_RECIPIENT   a
	JOIN   ANNUITY_PAYOUT_TXN_DETAIL  b ON a.annuity_payout_no   = b.annuity_payout_no
	                                   AND a.annuity_payout_recipient_no = b.annuity_payout_recipient_no
	                                   AND a.annuity_payout_recipient_type_code = b.annuity_payout_recipient_type_code
	JOIN   ANNUITY_PAYOUT_PARTICIPANT c ON b.annuity_account_no  = c.annuity_account_no
	                                   AND b.annuity_payout_no   = c.annuity_payout_no
												  AND b.recipient_no        = c.recipient_no
												  AND b.recipient_type_code = c.recipient_type_code
	JOIN   Annuity_Role               d ON c.annuity_role_code   = d.annuity_role_code
	JOIN   INDIVIDUAL                 e ON c.recipient_no        = e.individual_no
   WHERE  b.recipient_type_code   = 'I'
	AND    b.recipient_no          = :il_individual_no
   AND    b.annuity_payout_no     = :il_annuity_payout_no
	AND    b.payment_sub_type_code = 'CM'
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_account', 'embedded SQL: UPDATE ANNUITY_PAYOUT_RECIPIENT...', 'wf_create_cap_checklist')
	
	
	/*
	
	6.70	The annuity payout recipient must be the individual who is eligible for the annuity benefits, if the annuity payout transaction 
	is for an annuity payment (i.e. payment type/sub-type 97/CM) and the annuity payout type is annuity purchase. (Refer to Rationale)
	
	note that the individual on the annuity account record is always the eligible individual
	
	*/
	UPDATE a
	SET    a.annuity_payout_recipient_no        = a.recipient_no,
	       a.annuity_payout_recipient_type_code = a.recipient_type_code
	FROM   ANNUITY_PAYOUT_TXN_DETAIL  a
	JOIN   ANNUITY_PAYOUT_PARTICIPANT b ON a.annuity_account_no  = b.annuity_account_no
	                                   AND  a.annuity_payout_no  = b.annuity_payout_no
	                                   AND a.recipient_no        = b.recipient_no
												  AND a.recipient_type_code = b.recipient_type_code
	JOIN   Annuity_Role               c ON b.annuity_role_code = c.annuity_role_code
	WHERE  a.payment_sub_type_code    = 'CM'
	AND (  a.annuity_payout_recipient_no        <> a.recipient_no
	    OR a.annuity_payout_recipient_type_code <> a.recipient_type_code )
	AND    c.annuity_eligibility_flag = 'Y'
	AND    a.annuity_payout_no        = :il_annuity_payout_no
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_account', 'embedded SQL: UPDATE ANNUITY_PAYOUT_RECIPIENT...', 'wf_create_cap_checklist')
	
	
END IF

RETURN 0
end function

public function integer wf_get_active_checklist_count ();INTEGER		li_count

SELECT	IsNull(Count(*),0)
INTO		:li_count
FROM		ANNUITY_ACCOUNT					a
JOIN		CHECKLIST_SUBSCRIBER			b ON a.checklist_subscriber_no = b.checklist_subscriber_no
JOIN		SUBSCRIBER_CHECKLIST_XREF	c ON b.checklist_subscriber_no = c.checklist_subscriber_no
JOIN		CHECKLIST								d ON c.checklist_no = d.checklist_no
WHERE	d.checklist_status_code = 'IA'
AND		b.checklist_subscriber_type_code = 'ANN'
AND		d.checklist_type_code like 'CAP%'
AND		a.annuity_account_no = :il_annuity_account_no
USING SQLCA;

RETURN li_count

end function

public subroutine wf_clear_filter ();STRING   ls_filter, ls_filter_return

//ls_filter_return = uo_filter_control.idw_datawindow.inv_filter.of_SetFilter(ls_filter)

uo_filter_control.idw_datawindow.inv_filter.is_filter[] = {''}
uo_filter_control.idw_datawindow.inv_filter.ii_current_filter_position = 1

uo_filter_control.event ue_filter_changed(ls_filter)
uo_filter_control.idw_datawindow.inv_filter.ib_filter_on = FALSE
end subroutine

public function integer wf_set_claim (long al_claim_no);RETURN 0
end function

public subroutine wf_create_request_authorization_event (string as_window_to_open);// Request Authorization
DATE              ldt_accident_date
INTEGER           li_opening_no
LONG              ll_claim_no
STRING            ls_given_names, ls_last_name, ls_event_category_code, ls_event_type_code, ls_event_specific_code
STRING            ls_message

SELECT	last_name, given_names
INTO		:ls_last_name, :ls_given_names
FROM		INDIVIDUAL
WHERE	individual_no = :il_individual_no
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account', 'wf_create_request_authorization_event', 'select last_name, given_names from INDIVIDUAL..')

IF is_claim_role_code = 'C' THEN
	ls_event_category_code = 'I'     // Individual Based Events
	ls_event_type_code = '046'       // Annuity Authorization - IW
	ls_event_specific_code = 'REQ'   // request
	SetNull(ldt_accident_date)
	
	// even though the claimant event is individual based
	// we need non-zero claim_no to retrieve the 'name' dddw
	// in the u_event_log object properly. Otherwise, just the name shows.
	inv_common_annuity.nf_get_aq_claim(il_individual_no,ll_claim_no,li_opening_no)
	
ELSE
	ll_claim_no = il_annuity_account_claim_no
	
	ls_event_category_code = 'C'     // Claim Based Events
	ls_event_type_code = '051'       // Annuity Authorization - SS
	ls_event_specific_code = 'REQ'   // request
	
	SELECT accident_date
	INTO   :ldt_accident_date
	FROM   CLAIM
	WHERE  claim_no = :il_annuity_account_claim_no
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_account', 'wf_create_request_authorization_event', 'select accident_date from CLAIM..')
END IF

ls_message = 'Requesting annuity payout authorization as detailed in annuity payout# ' +String(il_annuity_payout_no)+ ' for individual# ' +String(il_individual_no)+ ', ' + ls_given_names +' '+ ls_last_name +'.'

inv_common_annuity.nf_open_event_log(as_window_to_open, is_claim_role_code, ll_claim_no, il_individual_no, ldt_accident_date, ls_last_name, ls_given_names, ls_event_category_code, ls_event_type_code, ls_event_specific_code, 'N', 'Y',ls_message)
end subroutine

public function integer wf_request_authorization_event_count ();DATETIME    ldtm_payout_confirmed
INTEGER		li_count
LONG        ll_confirm_annuity_payout_checklist_no

// returns the num of annuity payout auth request events that have been created since the confirm payout step (018) was completed
// in the case of claimant, its an individual event; in the case of SS, its a claim event.

ll_confirm_annuity_payout_checklist_no = dw_payout_accounts.GetItemNumber(dw_payout_accounts.GetRow(),'confirm_annuity_payout_checklist_no')

SELECT concluded_date
INTO   :ldtm_payout_confirmed
FROM   CHECKLIST_STEP
WHERE  checklist_no             = :ll_confirm_annuity_payout_checklist_no
AND    checklist_step_type_code = '018'
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account', 'wf_request_authorization_event_count', 'SELECT concluded_date FROM CHECKLIST')


IF is_claim_role_code = 'C' THEN
	
	SELECT Count(*)
	INTO   :li_count
	FROM   INDIVIDUAL_EVENT
	WHERE  individual_no       = :il_individual_no
	AND    event_type_code     = '046'
	AND    event_specific_code = 'REQ'
	AND    create_date        >= :ldtm_payout_confirmed
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_account', 'wf_request_authorization_event_count', 'SELECT Count(*) FROM INDIVIDUAL_EVENT')
ELSE
	SELECT Count(*)
	INTO   :li_count
	FROM   CLAIM_EVENT
	WHERE  claim_no            = :il_annuity_account_claim_no
	AND    event_type_code     = '051'
	AND    event_specific_code = 'REQ'
	AND    create_date        >= :ldtm_payout_confirmed
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_account', 'wf_request_authorization_event_count', 'SELECT Count(*) FROM CLAIM_EVENT')
END IF

RETURN li_count
end function

public function boolean wf_enable_overpayment_buttons (long al_checklist_no);INTEGER    li_overpayment_rows
STRING     ls_checklist_step_status_code
U_DS       lds_entitlement_overpayment


// If the Confirm Overpayment Recovery is complete, then you cannot modify overpayment amount
SELECT checklist_step_status_code
INTO   :ls_checklist_step_status_code
FROM   CHECKLIST_STEP
WHERE  checklist_step_type_code = '016'
AND    checklist_no = :al_checklist_no
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_for_annuity_account','SELECT checklist_step_status_code FROM CHECKLIST_STEP...','wf_enable_overpayment_buttons')

IF ls_checklist_step_status_code = 'INA' THEN
	RETURN FALSE
END IF

// enable or disable overpayment buttons
lds_entitlement_overpayment = Create U_DS
lds_entitlement_overpayment.DataObject = 'd_entitlement_overpayment'
lds_entitlement_overpayment.SetTransObject(SQLCA)
li_overpayment_rows = lds_entitlement_overpayment.Retrieve(il_individual_no)
SQLCA.nf_handle_error('w_prepare_for_annuity_account','lds_entitlement_overpayment','retrieve')


IF li_overpayment_rows > 0 THEN
	RETURN TRUE
ELSE
	RETURN FALSE
END IF

end function

public function integer wf_annuity_payout_corresp_count ();DATE        ldt_prior_step_completed
DATETIME    ldtm_prior_step_completed
INTEGER		li_count
LONG        ll_confirm_annuity_payout_checklist_no
STRING      ls_confirm_annuity_payout_type_code, ls_template_code, ls_prior_checklist_step_type_code

// returns the num of annuity payout auth request events that have been created since the confirm payout step (018) was completed
// in the case of claimant, its an individual event; in the case of SS, its a claim event.

ll_confirm_annuity_payout_checklist_no = dw_payout_accounts.GetItemNumber(dw_payout_accounts.GetRow(),'confirm_annuity_payout_checklist_no')

ls_confirm_annuity_payout_type_code = uo_confirm_payout_checklist.tab_checklist.tabpage_checklist.dw_checklist_master.GetItemString(1,'checklist_type_code')	


IF ls_confirm_annuity_payout_type_code = 'CAPAP' THEN
	// contract received step completed
	ls_prior_checklist_step_type_code = '022'
	ls_template_code = 'ANNPAY01'
ELSEIF ls_confirm_annuity_payout_type_code = 'CAPLS' THEN
	// notification step completed
	ls_prior_checklist_step_type_code = '020'
	ls_template_code = 'ANNPAY02'
END IF

SELECT concluded_date
INTO   :ldtm_prior_step_completed
FROM   CHECKLIST_STEP
WHERE  checklist_no      = :ll_confirm_annuity_payout_checklist_no
AND    checklist_step_type_code = :ls_prior_checklist_step_type_code
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account', 'wf_annuity_payout_corresp_count', 'SELECT concluded_date FROM CHECKLIST_STEP')

ldt_prior_step_completed = Date(ldtm_prior_step_completed)	
	
SELECT Count(*)
INTO   :li_count
FROM   CORRESPONDENCE a
JOIN   CLAIM b ON a.claim_no = b.claim_no
WHERE	 a.template_code          = :ls_template_code
AND    a.create_date           >= :ldt_prior_step_completed
AND    a.correspond_status_code = 'S'
AND    b.individual_no          = :il_individual_no
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account', 'wf_annuity_payout_corresp_count', 'SELECT Count(*) FROM CORRESPONDENCE')


RETURN li_count
end function

public subroutine wf_create_request_payout_event (string as_window_to_open);// Request Payout
DATE              ldt_accident_date
INTEGER           li_opening_no, li_non_writeoff_payout_count
LONG              ll_claim_no
STRING            ls_given_names, ls_last_name, ls_event_category_code, ls_event_type_code, ls_event_specific_code
STRING            ls_message



SELECT Count(*)
INTO   :li_non_writeoff_payout_count
FROM   ANNUITY_PAYOUT
WHERE  annuity_payout_no     = :il_annuity_payout_no
AND    net_annuity_payout_amount - writeoff_amount > 0.00
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account', 'wf_request_payout_event_count', 'SELECT Count(*) FROM ANNUITY_PAYOUT_TXN_DETAIL...')


SELECT	last_name, given_names
INTO		:ls_last_name, :ls_given_names
FROM		INDIVIDUAL
WHERE	individual_no = :il_individual_no
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account', 'wf_create_request_payout_event', 'select last_name, given_names from INDIVIDUAL..')

IF li_non_writeoff_payout_count > 0 THEN
	IF is_claim_role_code = 'C' THEN
		ls_event_category_code = 'I'     // Individual Based Events
		ls_event_type_code = '048'       // Annuity Payout - IW
		ls_event_specific_code = 'REQ'   // request
		SetNull(ldt_accident_date)
		
		// even though the claimant event is individual based
		// we need non-zero claim_no to retrieve the 'name' dddw
		// in the u_event_log object properly. Otherwise, just the name shows.
		inv_common_annuity.nf_get_aq_claim(il_individual_no,ll_claim_no,li_opening_no)
		
	ELSE
		ll_claim_no = il_annuity_account_claim_no
		
		ls_event_category_code = 'C'     // Claim Based Events
		ls_event_type_code = '053'       // Annuity Payout - SS
		ls_event_specific_code = 'REQ'   // request
		
		SELECT accident_date
		INTO   :ldt_accident_date
		FROM   CLAIM
		WHERE  claim_no = :il_annuity_account_claim_no
		USING SQLCA;
		SQLCA.nf_handle_error('w_prepare_annuity_account', 'wf_create_request_payout_event', 'select accident_date from CLAIM..')
	END IF
ELSE
	IF is_claim_role_code = 'C' THEN
		ls_event_category_code = 'I'     // Individual Based Events
		ls_event_type_code = '050'       // Annuity Write-off - IW
		ls_event_specific_code = ''
		SetNull(ldt_accident_date)
		
		// even though the claimant event is individual based
		// we need non-zero claim_no to retrieve the 'name' dddw
		// in the u_event_log object properly. Otherwise, just the name shows.
		inv_common_annuity.nf_get_aq_claim(il_individual_no,ll_claim_no,li_opening_no)
		
	ELSE
		ll_claim_no = il_annuity_account_claim_no
		
		ls_event_category_code = 'C'     // Claim Based Events
		ls_event_type_code = '055'       // Annuity Write-off - SS
		ls_event_specific_code = ''
		
		SELECT accident_date
		INTO   :ldt_accident_date
		FROM   CLAIM
		WHERE  claim_no = :il_annuity_account_claim_no
		USING SQLCA;
		SQLCA.nf_handle_error('w_prepare_annuity_account', 'wf_create_request_payout_event', 'select accident_date from CLAIM..')
	END IF
END IF

ls_message = 'Requesting annuity payout as detailed in annuity payout# ' +String(il_annuity_payout_no)+ ' for individual# ' +String(il_individual_no)+ ', ' + ls_given_names +' '+ ls_last_name +'.'

inv_common_annuity.nf_open_event_log(as_window_to_open, is_claim_role_code, ll_claim_no, il_individual_no, ldt_accident_date, ls_last_name, ls_given_names, ls_event_category_code, ls_event_type_code, ls_event_specific_code, 'N', 'Y', ls_message)
end subroutine

public function w_prepare_annuity_account wf_get_window_reference ();RETURN THIS
end function

public function integer wf_confirm_annuity_eligibility ();DATETIME            ldtm_annuity_end_date
INTEGER             li_count_non_BH_participants
LONG                ll_previously_completed_payout
STRING              ls_annuity_payout_status_code, ls_annuity_payout_status_reason_code, ls_payout_comment


// determine if individual was found not eligible, if so set to D/NEA - no payout, user enters comment
IF ib_not_eligible THEN
	//reset variable
	ib_not_eligible = FALSE
	
	ib_optional_user_payout_comment = TRUE
	ls_annuity_payout_status_code = 'D'
	ls_annuity_payout_status_reason_code = 'NEA'
	wf_build_annuity_payout_message(ls_annuity_payout_status_code,ls_annuity_payout_status_reason_code)
	wf_update_annuity_payout_status(ls_annuity_payout_status_code,ls_annuity_payout_status_reason_code,'Not Eligible for Annuity Benefits.') // Not Eligible for Annuity Benefits
	RETURN -1
END IF

// if the annuity end date is in future, set to D/AEE - no payout, user enters comment
IF ib_future_end_date THEN
	SELECT annuity_end_date
	INTO   :ldtm_annuity_end_date
	FROM   ANNUITY_ELIGIBILITY
	WHERE  annuity_eligibility_no = :il_annuity_eligibility_no
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_eligibility','embedded SQL: SELECT annuity_end_date FROM ANNUITY_ELIGIBILITY...','wf_confirm_annuity_eligibility')
	
	//reset variable
	ib_future_end_date = FALSE
	
	ib_optional_user_payout_comment = TRUE
	ls_annuity_payout_status_code = 'D'
	ls_annuity_payout_status_reason_code = 'AEE'
	wf_build_annuity_payout_message(ls_annuity_payout_status_code,ls_annuity_payout_status_reason_code)
	wf_update_annuity_payout_status(ls_annuity_payout_status_code,ls_annuity_payout_status_reason_code,'Annuity Eligibility Ends in Future ('+String(ldtm_annuity_end_date,'yyyy-mm-dd')+').') // Annuity Eligibility Ends in Future
	RETURN -1
END IF

// no post qualification entitlement, set to D/NBE - no payout, user enters comment
IF ib_no_benefit_entitlement THEN
	//reset variable
	ib_no_benefit_entitlement = FALSE
	
	ib_optional_user_payout_comment = TRUE
	ls_annuity_payout_status_code = 'D'
	ls_annuity_payout_status_reason_code = 'NBE'
	wf_build_annuity_payout_message(ls_annuity_payout_status_code,ls_annuity_payout_status_reason_code)
	wf_update_annuity_payout_status(ls_annuity_payout_status_code,ls_annuity_payout_status_reason_code,'No Benefit Entitlement After Qualification.') // No Benefit Entitlement After Qualification
	RETURN -1
END IF

// annuity previously paid, set to D/APP - no payout, user enters comment
IF ib_annuity_previously_paid THEN	
	ib_optional_user_payout_comment = TRUE
	ls_annuity_payout_status_code = 'D'
	ls_annuity_payout_status_reason_code = 'APP'
	ls_payout_comment = ids_populate_initial_annuity_payout_balances.GetItemString(1,'payout_comment')
	wf_build_annuity_payout_message(ls_annuity_payout_status_code,ls_annuity_payout_status_reason_code)
	wf_update_annuity_payout_status(ls_annuity_payout_status_code,ls_annuity_payout_status_reason_code,ls_payout_comment)
END IF

// reduction of claim annuity benefits, set to D/RCA - no payout, user enters no comment
IF ib_reduction_of_claim_annuity_benefits THEN
	ib_optional_user_payout_comment = TRUE
	ls_annuity_payout_status_code = 'D'
	ls_annuity_payout_status_reason_code = 'RCA'
	ls_payout_comment = ids_populate_initial_annuity_payout_balances.GetItemString(1,'payout_comment')
	wf_build_annuity_payout_message(ls_annuity_payout_status_code,ls_annuity_payout_status_reason_code)
	wf_update_annuity_payout_status(ls_annuity_payout_status_code,ls_annuity_payout_status_reason_code,ls_payout_comment)
	
	// display messagebox to indicate that reduction in ann.benefits will result in removal for list etc.
	SELECT IsNull(MAX(annuity_payout_no),0)
	INTO   :ll_previously_completed_payout
	FROM   ANNUITY_PAYOUT 
	WHERE  annuity_account_no = ( SELECT annuity_account_no 
											FROM   ANNUITY_PAYOUT 
											WHERE  annuity_payout_no = :il_annuity_payout_no
											GROUP BY annuity_account_no )
	AND    annuity_payout_status_code = 'C'
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_eligibility','embedded SQL: SELECT IsNull(MAX(annuity_payout_no),0) FROM ANNUITY_PAYOUT...','wf_confirm_annuity_eligibility')

END IF

IF ib_previous_manual_intervention THEN
	// reset variable
	ib_previous_manual_intervention = FALSE
	
	ls_annuity_payout_status_code = 'D'
	ls_annuity_payout_status_reason_code = 'PMI'
	wf_build_annuity_payout_message(ls_annuity_payout_status_code,ls_annuity_payout_status_reason_code)
	wf_update_annuity_payout_status(ls_annuity_payout_status_code,ls_annuity_payout_status_reason_code,'Manual intervention required.')
	
	RETURN -1
END IF

IF ib_modified_annuity_calc_method THEN
	// reset variable
	ib_modified_annuity_calc_method = FALSE
	
	ls_annuity_payout_status_code = 'D'
	ls_annuity_payout_status_reason_code = 'MAC'
	wf_build_annuity_payout_message(ls_annuity_payout_status_code,ls_annuity_payout_status_reason_code)
	wf_update_annuity_payout_status(ls_annuity_payout_status_code,ls_annuity_payout_status_reason_code,'Manual intervention required.')
	
	RETURN -1
END IF


IF ib_annuity_previously_paid OR ib_reduction_of_claim_annuity_benefits THEN
	//reset variables
	ib_annuity_previously_paid = FALSE
	ib_reduction_of_claim_annuity_benefits = FALSE
	
	// inserts records into ANNUITY_PAYOUT_CLAIM_SUMMARY for benefit holder, updates ANNUITY_PAYOUT totals
	wf_initial_annuity_payout_population()
	RETURN -1
END IF


IF ib_different_participant_list THEN
	// reset variable
	ib_different_participant_list = FALSE
	
	// inserts records into ANNUITY_PAYOUT_CLAIM_SUMMARY for benefit holder, updates ANNUITY_PAYOUT totals
	wf_initial_annuity_payout_population()
	
	// the user requires a different participant list, so this payout (and any future payout) must be completed manually
	ib_required_user_payout_comment = TRUE
	ls_annuity_payout_status_code = 'D'
	ls_annuity_payout_status_reason_code = 'DPP'
	wf_build_annuity_payout_message(ls_annuity_payout_status_code,ls_annuity_payout_status_reason_code)
	wf_update_annuity_payout_status(ls_annuity_payout_status_code,ls_annuity_payout_status_reason_code,'Manual intervention required.')
	RETURN -1
ELSE
	IF ib_can_change_participants = FALSE AND il_previously_completed_payout_no > 0 THEN
		
		// are there any non-IW or non-SS participants?
		SELECT Count(*)
		INTO   :li_count_non_BH_participants
		FROM   ANNUITY_PAYOUT_PARTICIPANT a
		JOIN   Annuity_Role               b ON a.annuity_role_code = b.annuity_role_code
		WHERE  a.annuity_payout_no = :il_previously_completed_payout_no
		AND NOT (b.annuity_eligibility_flag = 'Y'
           AND b.annuity_entitlement_flag = 'Y')
		USING SQLCA;
		SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT Count(*) FROM ANNUITY_PAYOUT_PARTICIPANT...', 'wf_confirm_annuity_eligibility')
		
		IF li_count_non_BH_participants > 0 THEN
			// for subsequent payouts, must have same participant list
				
			DECLARE lp_copy_previous_participant_list PROCEDURE FOR p_copy_previous_participant_list
				@current_annuity_payout_no = :il_annuity_payout_no, @last_annuity_payout_no = :il_previously_completed_payout_no
			USING SQLCA;
			
			EXECUTE lp_copy_previous_participant_list;
			SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: EXECUTE lp_copy_previous_participant_list','wf_confirm_annuity_eligibility')
			
		END IF
	END IF
	
	//reset variables
	il_previously_completed_payout_no = 0
	ib_can_change_participants = FALSE
END IF




// at this point it has been determined that there will be a payout
// inserts records into ANNUITY_PAYOUT_CLAIM_SUMMARY for benefit holder, updates ANNUITY_PAYOUT totals
wf_initial_annuity_payout_population()


RETURN 0


end function

public subroutine wf_retrieve_tab (integer ai_selected_tab);INTEGER  li_rows, li_current_row, li_rowcount, li_recipient_row, li_beneficiary_row
LONG     ll_participant_no, ll_annuity_payout_recipient_no, ll_annuity_contract_no
STRING   ls_annuity_payout_recipient_type_code
STRING   ls_annuity_eligibility_confirmed_flag, ls_overpayment_recovery_confirmed_flag
STRING   ls_contract_confirmed_flag


li_rowcount = dw_payout_accounts.RowCount()
IF li_rowcount > 0 THEN
	CHOOSE CASE ai_selected_tab
		CASE PARTICIPANT
			li_rows = idw_participants.retrieve(il_annuity_payout_no)
			SQLCA.nf_handle_error('w_prepare_annuity_account','idw_participants','retrieve')
			IF li_rows > 0 THEN
				ll_participant_no = idw_participants.getItemNumber(1,'recipient_no')
				IF ll_participant_no > 0 THEN
					idw_participants.setRow(1)
					idw_participants.selectRow(1,TRUE)
					idw_participants.event dynamic rowfocuschanged(1)
				END IF
			END IF
			
			IF is_checklist_type_code = 'PAA' THEN
				// is eligibility confirmed, and is overpayment still not confirmed
				// if so, enable controls on tab
				ls_annuity_eligibility_confirmed_flag = dw_payout_accounts.GetItemString(dw_payout_accounts.GetRow(),'annuity_eligibility_confirmed_flag')
				ls_overpayment_recovery_confirmed_flag = dw_payout_accounts.GetItemString(dw_payout_accounts.GetRow(),'overpayment_recovery_confirmed_flag')
				
				IF ls_annuity_eligibility_confirmed_flag = 'Y' AND ls_overpayment_recovery_confirmed_flag = 'N' THEN
					
					// if there was a previously completed payout, then user cannot alter the participant list
					wf_determine_writeoff_on_last_payout()
					IF ib_can_change_participants THEN
						idw_participants.Enabled = TRUE
						idw_participant_details.Enabled = TRUE
						idw_participant_details.Object.b_participant_search.Enabled = 'Yes'
						
						wf_enable_participant_buttons(TRUE)
						tab_payout.tabpage_participants.cb_details.Enabled = TRUE
					ELSE
						wf_enable_participant_buttons(FALSE)
						idw_participants.Enabled = TRUE
						idw_participant_details.Enabled = TRUE
						idw_participant_details.Object.b_participant_search.Enabled = 'No'
												
						tab_payout.tabpage_participants.cb_cancel.Enabled = FALSE
						tab_payout.tabpage_participants.cb_save.Enabled = FALSE
						tab_payout.tabpage_participants.cb_details.Enabled = TRUE
					END IF
				ELSE
					wf_enable_participant_buttons(FALSE)
					idw_participants.Enabled = TRUE
					idw_participant_details.Enabled = FALSE
					idw_participant_details.Object.b_participant_search.Enabled = 'Yes'
					
					tab_payout.tabpage_participants.cb_cancel.Enabled = FALSE
					tab_payout.tabpage_participants.cb_save.Enabled = FALSE
					tab_payout.tabpage_participants.cb_details.Enabled = TRUE
					
				END IF			
			ELSE
				// disable controls on tab
				wf_enable_participant_buttons(FALSE)
				idw_participants.Enabled = TRUE
				idw_participant_details.Enabled = FALSE
				idw_participant_details.Object.b_participant_search.Enabled = 'Yes'
						
				tab_payout.tabpage_participants.cb_cancel.Enabled = FALSE
				tab_payout.tabpage_participants.cb_save.Enabled = FALSE
				tab_payout.tabpage_participants.cb_details.Enabled = TRUE
			END IF
			
			IF is_pap_open_mode = 'inquiry' THEN
				uo_prepare_checklist.Visible = FALSE
				uo_confirm_payout_checklist.Visible = FALSE
				rb_individual.Visible = false
				rb_region.Visible    = false
				wf_enable_controls(false)
				wf_enable_participant_buttons(false)
				wf_enable_contract_buttons(false)
				
				// need to disable these two buttons individually
				tab_payout.tabpage_participants.cb_save.enabled = false
				tab_payout.tabpage_participants.cb_cancel.enabled = false
				
				//re-enable this datawindow
				idw_participants.enabled = true
				dw_payout_accounts.enabled = true
			END IF
			
		CASE PAYOUT_SUMMARY
			li_rows = idw_annuity_payout_summary.Retrieve(il_annuity_payout_no)
			SQLCA.nf_handle_error('w_prepare_annuity_account','idw_annuity_payout_summary','retrieve')
			
		CASE PAYOUT_DETAILS
			li_rows = idw_annuity_payout_detail.Retrieve(il_annuity_payout_no)
			SQLCA.nf_handle_error('w_prepare_annuity_account','idw_annuity_payout_detail','retrieve')
			idw_annuity_payout_detail.GroupCalc()
			
		CASE TXN_DETAILS
			IF ib_payout_with_dependants AND ii_dependant_txn_count > 0 THEN
				idw_annuity_payout_txn_detail.DataObject = 'd_ap_txn_detail_w_dependants_comp'
			ELSE
				idw_annuity_payout_txn_detail.DataObject = 'd_annuity_payout_txn_detail_no_dependants'
			END IF
			idw_annuity_payout_txn_detail.SetTransObject(SQLCA)
			li_rows = idw_annuity_payout_txn_detail.Retrieve(il_annuity_payout_no)
			SQLCA.nf_handle_error('w_prepare_annuity_account','idw_annuity_payout_txn_detail','retrieve')
			idw_annuity_payout_txn_detail.GroupCalc()
			
		CASE OP_RECOVERY
			li_rows = idw_annuity_overpayment.Retrieve(il_annuity_payout_no)
			SQLCA.nf_handle_error('w_prepare_annuity_account','idw_annuity_overpayment','retrieve')
			
		CASE RECIPIENTS
			li_rows = idw_payout_recipients.Retrieve(il_annuity_payout_no)
			SQLCA.nf_handle_error('w_prepare_annuity_account','idw_payout_recipients','retrieve')
			idw_payout_recipients.GroupCalc()
			
			li_current_row = idw_payout_recipients.GetRow()
			IF li_current_row > 0 THEN			
				idw_payout_recipients.event RowFocusChanged(li_current_row)
			END IF
			
			li_rows = idw_payout_recipient_report.Retrieve(il_annuity_payout_no)
			SQLCA.nf_handle_error('w_prepare_annuity_account','idw_payout_recipient_report','retrieve')
			
			IF li_rows > 0 THEN
				idw_payout_recipient_report.GroupCalc()
				
				idw_payout_recipients.SetColumn('address_line1')
				idw_payout_recipients.post function SetFocus()
	
				li_recipient_row = idw_payout_recipients.GetRow()
				wf_set_address_line_display_only(li_recipient_row)
			END IF
			
		CASE CONTRACT
			idw_contract_beneficiaries_details.Reset()
			li_rows = idw_contract_beneficiaries.retrieve(il_annuity_payout_no)
			SQLCA.nf_handle_error('w_prepare_annuity_account','idw_contract_beneficiaries','retrieve')
			IF li_rows > 0 THEN			
				ll_annuity_contract_no = idw_contract_beneficiaries.getItemNumber(1, 'annuity_contract_no')
										
				idw_contract_beneficiaries.setFocus()
				idw_contract_beneficiaries.setRow(1)
				idw_contract_beneficiaries.setColumn('annuity_carrier_name')   //'annuity_carrier_name'
				
				li_rows = idw_contract_beneficiaries_details.retrieve(ll_annuity_contract_no)
				SQLCA.nf_handle_error('w_prepare_annuity_account','idw_contract_beneficiaries_details','retrieve')
				
				IF li_rows > 0 THEN
					ls_contract_confirmed_flag = idw_contract_beneficiaries.getItemString(1, 'contract_confirmed_flag')
					IF ls_contract_confirmed_flag = 'Y' THEN
						idw_contract_beneficiaries_details.Object.beneficiary_name.Edit.DisplayOnly = 'Yes'
						li_current_row = idw_contract_beneficiaries_details.GetRow()
						IF li_current_row > 0 THEN
							
							THIS.SetRedraw(FALSE)
							FOR li_beneficiary_row = 1 TO li_rows
								idw_contract_beneficiaries_details.setRow(li_beneficiary_row)
								idw_contract_beneficiaries_details.scrollToRow(li_beneficiary_row)
								idw_contract_beneficiaries_details.SetColumn('beneficiary_name')
								idw_contract_beneficiaries_details.SetFocus()
							NEXT							
							THIS.SetRedraw(TRUE)
							
						END IF
						
						idw_contract_beneficiaries_details.Object.annuity_beneficiary_code.TabSequence='0'
						idw_contract_beneficiaries_details.Object.primary_beneficiary_flag.TabSequence='0'
					END IF
					idw_contract_beneficiaries_details.setRow(1)
					idw_contract_beneficiaries_details.scrollToRow(1)
				ELSE
					idw_contract_beneficiaries_details.Object.beneficiary_name.Edit.DisplayOnly = 'No'
					
					idw_contract_beneficiaries_details.Object.annuity_beneficiary_code.TabSequence='10'
					idw_contract_beneficiaries_details.Object.primary_beneficiary_flag.TabSequence='20'
				END IF
			ELSE
				idw_contract_beneficiaries_details.Object.beneficiary_name.Edit.DisplayOnly = 'No'
				
				idw_contract_beneficiaries_details.Object.annuity_beneficiary_code.TabSequence='10'
				idw_contract_beneficiaries_details.Object.primary_beneficiary_flag.TabSequence='20'
			END IF	
			
			// BR's 8.10 and 8.20 
			//8.10 Annuity contract and beneficiary information must not be maintained, if the 
			//       Confirm Annuity Payout – Annuity Purchase, Contract Received checklist step is Incomplete
			//8.20 Annuity contract and beneficiary information must not be maintained, if the 
			//       Confirm Annuity Payout – Annuity Purchase, Contract Confirmed checklist step is Completed
			IF wf_check_maintain_annuity_contract(il_annuity_payout_no) = false THEN
				idw_contract_beneficiaries.enabled = false
				wf_enable_contract_buttons(FALSE)
			ELSE
				idw_contract_beneficiaries.enabled = TRUE
				wf_enable_contract_buttons(TRUE)
				IF idw_contract_beneficiaries.RowCount() > 0 THEN
					tab_payout.tabpage_contract_and_beneficiaries.cb_add_contract.enabled = FALSE	
				ELSE
					tab_payout.tabpage_contract_and_beneficiaries.cb_delete_contract.enabled = FALSE
				END IF
				tab_payout.tabpage_contract_and_beneficiaries.cb_save_contract.Enabled = FALSE
				tab_payout.tabpage_contract_and_beneficiaries.cb_cancel_contract.Enabled = FALSE
			END IF
			
	END CHOOSE
END IF
end subroutine

public subroutine wf_enable_participant_buttons (boolean ab_enable);idw_participants.enabled = ab_enable

tab_payout.tabpage_participants.cb_new.enabled = ab_enable
tab_payout.tabpage_participants.cb_add.enabled = ab_enable
tab_payout.tabpage_participants.cb_delete.enabled = ab_enable
//tab_payout.tabpage_participants.cb_details.enabled = ab_enable
tab_payout.tabpage_participants.cb_save.enabled = not ab_enable
tab_payout.tabpage_participants.cb_cancel.enabled =  not ab_enable

end subroutine

public function long wf_get_claim_no ();INTEGER				li_row
LONG					ll_claim_no

li_row = dw_payout_accounts.GetRow()
IF li_row > 0 THEN
	ll_claim_no = dw_payout_accounts.GetItemNumber(li_row,'claim_no')
ELSE
	MessageBox('No Records','There are no annuity payout records.',Exclamation!)
	RETURN -1
END IF


RETURN ll_claim_no
end function

public function string wf_get_claim_role_code ();RETURN is_claim_role_code
end function

public function long wf_get_individual_no ();RETURN il_individual_no
end function

public function string wf_get_module_code ();RETURN '050'
end function

public subroutine wf_enable_controls (boolean ab_enabled);
dw_payout_accounts.enabled = ab_enabled
uo_filter_control.enabled = ab_enabled

IF dw_payout_region.Enabled = TRUE THEN
	dw_payout_region.enabled = ab_enabled
END IF



end subroutine

public subroutine wf_scroll_to_individual (long al_individual_no);INTEGER    li_find



li_find = dw_payout_accounts.uf_find_row('individual_no',String(al_individual_no))

IF li_find > 0 THEN
//	dw_payout_accounts.ScrollToRow(li_find)
	dw_payout_accounts.SelectRow(li_find,TRUE)
ELSE
	dw_payout_accounts.SetRow(1)
	dw_payout_accounts.SelectRow(1,TRUE)
	dw_payout_accounts.event RowFocusChanged(1)
END IF
end subroutine

public function integer wf_verify_payout_authorization ();INTEGER     li_count

SELECT Count(*)
INTO   :li_count
FROM   ANNUITY_PAYOUT
WHERE  authorized_date IS NOT NULL
AND    annuity_payout_no = :il_annuity_payout_no
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT Count(*) FROM ANNUITY_PAYOUT...', 'wf_verify_payout_authorization')

RETURN li_count
end function

public function integer wf_verify_annuity_payout_letter ();DATETIME    ldtm_authorized_date
INTEGER     li_count, li_row
LONG        ll_claim_no
STRING      ls_template_code


// get the claim_no that should have been used to create AP notification
IF is_claim_role_code = 'C' THEN
	inv_common_annuity.nf_get_AP_claim(il_annuity_payout_no,il_individual_no,ll_claim_no)
ELSE
	ll_claim_no = il_annuity_account_claim_no
END IF

IF is_checklist_type_code = 'CAPAP' THEN
	ls_template_code = 'ANNPAY01'
ELSE
	ls_template_code = 'ANNPAY02'
END IF

li_row = dw_payout_accounts.GetRow()

SELECT authorized_date
INTO   :ldtm_authorized_date
FROM   ANNUITY_PAYOUT
WHERE  annuity_payout_no = :il_annuity_payout_no
USING SQLCA;
SQLCA.nf_Handle_error('w_prepare_annuity_account','embedded SQL: SELECT authorized_date FROM ANNUITY_PAYOUT...', 'wf_verify_annuity_payout_letter')


SELECT Count(*)
INTO   :li_count
FROM   CORRESPONDENCE
WHERE  correspond_status_code = 'S'
AND    template_code          = :ls_template_code
AND    claim_no               = :ll_claim_no
AND    create_date           >= :ldtm_authorized_date
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT Count(*) FROM CORRESPONDENCE...', 'wf_verify_annuity_payout_letter')

RETURN li_count
end function

public subroutine wf_payout_has_dependants ();INTEGER li_count

// check if there are dependants, set instance variable


SELECT COUNT(*)
INTO   :li_count
FROM   ANNUITY_PAYOUT_PARTICIPANT a
JOIN   Annuity_Role               b ON a.annuity_role_code = b.annuity_role_code
WHERE  a.annuity_payout_no        = :il_annuity_payout_no
AND    b.annuity_eligibility_flag = 'N'
AND    b.annuity_entitlement_flag = 'Y'
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT COUNT(*) FROM ANNUITY_PAYOUT_PARTICIPANT...','wf_payout_has_dependants')
	
IF li_count = 0 THEN
	ib_payout_with_dependants = FALSE
ELSE
	ib_payout_with_dependants = TRUE
	
	SELECT Count(*)
	INTO   :ii_dependant_txn_count
	FROM   ANNUITY_PAYOUT_TXN_DETAIL
	WHERE  recipient_no in ( SELECT a.recipient_no
	                         FROM   ANNUITY_PAYOUT_PARTICIPANT a
	                         JOIN   Annuity_Role               b ON a.annuity_role_code = b.annuity_role_code
	                         WHERE  a.annuity_payout_no        = :il_annuity_payout_no
	                         AND    b.annuity_eligibility_flag = 'N'
	                         AND    b.annuity_entitlement_flag = 'Y' )
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT Count(*)	FROM ANNUITY_PAYOUT_TXN_DETAIL...','wf_payout_has_dependants')
END IF

end subroutine

public function boolean wf_display_claim_details_tab (long al_paa_checklist_no);BOOLEAN lb_payout_detail_tab_visible
INTEGER li_count

// if there are no dependants or the confirm overpayment checklist step is not completed, then do not display payout details tab
// otherwise, display the tab
SELECT COUNT(*)
INTO   :li_count
FROM   CHECKLIST_STEP
WHERE  checklist_no             = :al_paa_checklist_no
AND    checklist_step_no        = 3
AND    checklist_step_type_code = '016'
AND    concluded_date IS NOT NULL
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT COUNT(*) FROM CHECKLIST_STEP...','wf_display_claim_details_tab')

wf_payout_has_dependants()

IF li_count = 0 THEN
	lb_payout_detail_tab_visible = FALSE
ELSE
	IF ib_payout_with_dependants = FALSE THEN
		lb_payout_detail_tab_visible = FALSE
	ELSE
		lb_payout_detail_tab_visible = TRUE
	END IF
END IF

tab_payout.tabpage_payout_details.Visible = lb_payout_detail_tab_visible

RETURN lb_payout_detail_tab_visible
end function

public function integer wf_request_payout ();// insert PAYMENT, UNAPPLIED_CLAIM_TXN, update ANNUITY_PAYOUT_TXN_DETAIL

DECLARE lp_request_annuity_payout PROCEDURE FOR p_request_annuity_payout
	@annuity_payout_no = :il_annuity_payout_no
USING SQLCA;

EXECUTE lp_request_annuity_payout;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: EXECUTE lp_request_annuity_payout','wf_request_payout')


RETURN 0

end function

public subroutine wf_filter_participant_dddws (boolean ab_filter);
datawindowchild ldw_child, ldw_child2

idw_participant_details.getChild('annuity_role_code', ldw_child)
idw_participant_details.getChild('recipient_type_code', ldw_child2)

IF ab_filter = TRUE THEN	
	ldw_child.setFilter("active_flag = 'Y'")
	ldw_child2.setFilter("active_flag = 'Y'")
ELSE
	ldw_child.setFilter("")
	ldw_child2.setFilter("")
END IF

ldw_child.filter()
ldw_child2.filter()




end subroutine

public subroutine wf_clear_datawindows ();/*
function called if the checklist has been concluded and there are no more records in list
typically would occur after completing checklist for a single individual on list
*/

dw_payout_region.Reset()

idw_participants.Reset()
idw_participant_details.Reset()

idw_annuity_payout_summary.Reset()

idw_annuity_payout_detail.Reset()

idw_annuity_payout_txn_detail.Reset()

idw_annuity_overpayment.Reset()

idw_payout_recipients.Reset()
idw_payout_recipient_details.Reset()

idw_contract_beneficiaries.Reset()

st_multiple_accounts.Visible = FALSE
cb_manual_annuity_payout.Enabled = FALSE

IF is_checklist_type_code = 'PAA' THEN
	uo_prepare_checklist.uf_clear_datawindows()
ELSE
	uo_confirm_payout_checklist.uf_clear_datawindows()
END IF

end subroutine

public function integer wf_check_bus_rule_annuity_contract ();
BOOLEAN      lb_has_beneficiary_not_guaranteed
INTEGER      li_current_row, li_details_row, li_find_row, li_counter, li_details_rowcount, li_rtn
INTEGER      li_msg, li_found
STRING       ls_annuity_carrier_name, ls_beneficiary_name, ls_whscc_beneficiary_name, ls_annuity_term_code, ls_annuity_term_active_flag, ls_annuity_beneficiary_code
STRING       ls_primary_beneficiary_flag, ls_beneficiary_required_flag, ls_find_sql
STRING       ls_full_name, ls_last_name


// BR's 8.10 and 8.20
IF wf_check_maintain_annuity_contract(il_annuity_payout_no) = FALSE THEN
	MESSAGEBOX("Check Bus Rules", "Annuity contract and beneficiary information must not be maintained, if the " &
	 + "~rConfirm Annuity Payout – Annuity Purchase, Contract Received checklist step is Incomplete OR the" &
	  +"~rConfirm Annuity Payout – Annuity Purchase, Contract Confirmed checklist step is Completed.",INFORMATION!) 
	RETURN -1
END IF 

li_current_row = idw_contract_beneficiaries.getRow()
li_details_row = idw_contract_beneficiaries_details.getRow()

IF li_current_row < 1 then RETURN 0

//8.30	 The annuity carrier’s name must be provided
ls_annuity_carrier_name = idw_contract_beneficiaries.getItemString(li_current_row, 'annuity_carrier_name')
IF ls_annuity_carrier_name = "" or isnull(ls_annuity_carrier_name) or len(ls_annuity_carrier_name) < 3 THEN
	MESSAGEBOX("BR 8.30", "The annuity carrier’s name must be provided.")
	idw_contract_beneficiaries.setColumn('annuity_carrier_name')
	RETURN -1
END IF 


//8.40	 The annuity contract must have an active annuity term. 
ls_annuity_term_code = idw_contract_beneficiaries.getItemString(li_current_row, 'annuity_term_code')
SELECT  active_flag,
        beneficiary_required_flag
INTO    :ls_annuity_term_active_flag,
        :ls_beneficiary_required_flag
FROM    Annuity_Term
WHERE   annuity_term_code = :ls_annuity_term_code
USING   SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','SELECT active_flag,beneficiary_required_flag FROM Annuity_Term...','wf_check_bus_rule_annuity_contract')

IF isNull(ls_annuity_term_code) OR ls_annuity_term_code = "" OR ls_annuity_term_active_flag = 'N' THEN
	MESSAGEBOX("BR 8.40", "The annuity contract must have an active annuity term.")
	idw_contract_beneficiaries.setColumn('annuity_term_code')
	RETURN -1
END IF 


li_details_rowcount = idw_contract_beneficiaries_details.rowCount()

// BR 8.50  There must be at least one primary annuity beneficiary associated with the annuity contract, if the annuity term is guaranteed.
IF ls_beneficiary_required_flag = 'Y' THEN
	IF li_details_rowcount <= 0 THEN
		MESSAGEBOX("BR 8.50", "There must be at least one Primary annuity beneficiary associated with the annuity contract.")
		RETURN -1
	ELSE
		ls_find_sql = "primary_beneficiary_flag = 'Y'"
		li_find_row = idw_contract_beneficiaries_details.find(ls_find_sql, 1, li_details_rowcount)
		IF li_find_row <= 0 THEN
			MESSAGEBOX("BR 8.50", "There must be at least one primary annuity beneficiary associated with the annuity contract, if the annuity term is guaranteed.")
			RETURN -1
		END IF
	END IF
ELSEIF ls_beneficiary_required_flag = 'N' THEN
	// BR 8.60  There must not be annuity beneficiaries, if the annuity term is not guaranteed. (Refer to Rationale)
	IF li_details_rowcount > 0 THEN
		FOR li_counter = 1 TO li_details_rowcount
			// if the user has not selected any values, then the row is 'new'
			ls_annuity_beneficiary_code = idw_contract_beneficiaries_details.GetItemString(li_counter,'annuity_beneficiary_code')
			ls_primary_beneficiary_flag = idw_contract_beneficiaries_details.GetItemString(li_counter,'primary_beneficiary_flag')
			ls_beneficiary_name = idw_contract_beneficiaries_details.GetItemString(li_counter,'beneficiary_name')
			
			IF IsNull(ls_annuity_beneficiary_code) THEN ls_annuity_beneficiary_code = ''
			IF IsNull(ls_primary_beneficiary_flag) THEN ls_primary_beneficiary_flag = ''
			IF IsNull(ls_beneficiary_name) THEN ls_beneficiary_name = ''
			
			IF ls_annuity_beneficiary_code + ls_primary_beneficiary_flag + ls_beneficiary_name = '' THEN
				// ignore
			ELSE
				lb_has_beneficiary_not_guaranteed = TRUE
				MESSAGEBOX("BR 8.60", "There must not be annuity beneficiaries, if the annuity term is not guaranteed.")
				EXIT
			END IF
		NEXT
		IF lb_has_beneficiary_not_guaranteed THEN
			RETURN -1
		END IF
	END IF
END IF


// warn user if the last or full name of the eligible individual has been included in the annuity carrier name
SELECT given_names + ' ' + last_name,
       last_name
INTO   :ls_full_name,
       :ls_last_name
FROM   INDIVIDUAL
WHERE  individual_no = :il_individual_no
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT given_names,last_name FROM INDIVIDUAL...','wf_check_bus_rule_annuity_contract')

li_found = Pos(ls_annuity_carrier_name,ls_full_name)
IF li_found > 0 THEN
	li_msg = MessageBox('','The annuity carrier name should not include the name of the eligible individual. Do you want to cancel the save?',Question!,YesNo!,1)
	IF li_msg = 1 THEN
		RETURN -1
	END IF
ELSE
	li_found = Pos(ls_annuity_carrier_name,ls_last_name)
	IF li_found > 0 THEN
		li_msg = MessageBox('','The annuity carrier name should not include the last name of the eligible individual. Do you want to cancel the save?',Question!,YesNo!,1)
		IF li_msg = 1 THEN
			RETURN -1
		END IF
	END IF
END IF


// if there are beneficiaries, then test the BRs below
IF li_details_row > 0  THEN
	
	IF ls_beneficiary_required_flag = 'Y' THEN
	
		//BR 8.70	The annuity beneficiary must have an active annuity beneficiary type. 
		ls_annuity_beneficiary_code =  idw_contract_beneficiaries_details.getItemString(li_details_row, 'annuity_beneficiary_code')
		IF ls_annuity_beneficiary_code = '' or isnull(ls_annuity_beneficiary_code) THEN
			MESSAGEBOX("BR 8.70", "The annuity beneficiary must have an active annuity beneficiary type.")
			idw_contract_beneficiaries_details.setColumn('annuity_beneficiary_code')
			RETURN -1
		END IF	
		
		//BR 8.80	The annuity beneficiary’s name must be provided, if the beneficiary is a dependent.
		ls_beneficiary_name =  idw_contract_beneficiaries_details.getItemString(li_details_row, 'beneficiary_name')
		IF ls_beneficiary_name = '' or isnull(ls_beneficiary_name) THEN
			MESSAGEBOX("BR 8.80", "The annuity beneficiary’s name must be provided, if the beneficiary is a dependent.")
			idw_contract_beneficiaries_details.setColumn('beneficiary_name')
			RETURN -1
		END IF
	
		//BR 8.90	The annuity beneficiary’s name must be the annuity beneficiary description, if the beneficiary is WHSCC. (WHSCC might change to WorkSafe NB later.So be prepared)
		SELECT annuity_beneficiary_desc_e
		INTO   :ls_whscc_beneficiary_name
		FROM   Annuity_Beneficiary
		WHERE  annuity_beneficiary_code = 'W'
		USING SQLCA;
		SQLCA.nf_handle_error("w_prepare_annuity_account","wf_check_bus_rule_annuity_contract","select from Annuity_Beneficiary") 
		
		IF ls_annuity_beneficiary_code = 'W' THEN
			IF upper(ls_whscc_beneficiary_name) <> idw_contract_beneficiaries_details.getItemString(li_details_row, 'beneficiary_name') THEN
				MESSAGEBOX("BR 8.90", "The annuity beneficiary’s name must be the annuity beneficiary description, if the beneficiary is " + ls_whscc_beneficiary_name + ".")
				idw_contract_beneficiaries_details.setColumn('beneficiary_name')
				RETURN -1
			END IF
		END IF
		
		// BR 8.80	The annuity beneficiary must be designated as either the primary beneficiary or not the primary beneficiary. 
		ls_primary_beneficiary_flag =  idw_contract_beneficiaries_details.getItemString(li_details_row, 'primary_beneficiary_flag')
		IF isNull(ls_primary_beneficiary_flag) OR (ls_primary_beneficiary_flag <> 'Y' and ls_primary_beneficiary_flag <> 'N') THEN
			MESSAGEBOX("BR 8.80", "The annuity beneficiary must be designated as either the primary beneficiary or not the primary beneficiary. ")
			idw_contract_beneficiaries_details.setColumn('primary_beneficiary_flag')
			RETURN -1
		END IF 
	
		//BR 8.140	An annuity beneficiary must not be associated with an annuity contract more than once. (Refer to Rationale)
		IF li_details_rowcount > 1 THEN
			FOR li_counter = 1 to li_details_rowcount
				ls_beneficiary_name = idw_contract_beneficiaries_details.getItemString(li_counter, 'beneficiary_name')
				ls_find_sql = "beneficiary_name = '" + ls_beneficiary_name + "'"
				li_find_row = idw_contract_beneficiaries_details.find(ls_find_sql, li_counter + 1, li_details_rowcount + 1)
				IF li_find_row > 1 THEN
					MESSAGEBOX("BR 8.140", "An annuity beneficiary must not be associated with an annuity contract more than once.")
					RETURN -1
				END IF
			NEXT
		END IF
		
		// BR 8.90	WHSCC must be the primary beneficiary, if there are no dependent primary beneficiaries associated with the annuity contract. 
		ls_find_sql = "annuity_beneficiary_code = 'D' and primary_beneficiary_flag = 'Y'"
		li_find_row =  idw_contract_beneficiaries_details.find(ls_find_sql, 1, li_details_rowcount)
		IF li_find_row = 0 THEN
			// found no rows where beneficiary is a 'D'ependant and is Primary. Now check for a row that is a WHSCC and is primary
			ls_find_sql = "annuity_beneficiary_code = 'W' and primary_beneficiary_flag = 'Y'"
			li_find_row =  idw_contract_beneficiaries_details.find(ls_find_sql, 1, li_details_rowcount)
			IF li_find_row = 0 THEN
				MESSAGEBOX("BR 8.90", ls_whscc_beneficiary_name + " must be the primary beneficiary, if there are no dependent primary beneficiaries associated with the annuity contract.")
				RETURN -1
			END IF
		ELSEIF li_find_row > 0 THEN	
		
		//BR 8.120	There must be a secondary or contingent beneficiary of WHSCC, if there are any dependent primary beneficiaries associated with the annuity contract.
			
			// found some records where beneficiary is a 'D'ependant and is Primary. Now check that  WHSCC exists as a secondary, but only check if current row is > first row
			ls_find_sql = "annuity_beneficiary_code = 'W' and primary_beneficiary_flag = 'N'"
			li_find_row =  idw_contract_beneficiaries_details.find(ls_find_sql, 1, li_details_rowcount)
			IF li_find_row <= 0 THEN
				MESSAGEBOX("BR 8.120", "There must be a secondary or contingent beneficiary of " + ls_whscc_beneficiary_name + ", if there are any dependent primary beneficiaries associated with the annuity contract.")
				RETURN -1
			END IF
		END IF
		
		//BR 8.130	There must not be a secondary or contingent Dependant beneficiary.
		ls_find_sql = "annuity_beneficiary_code = 'D' and primary_beneficiary_flag = 'N'"
		li_find_row =  idw_contract_beneficiaries_details.find(ls_find_sql, 1, li_details_rowcount)
		IF li_find_row > 0 THEN
			MESSAGEBOX("BR 8.130", "There must not be a secondary or contingent Dependant beneficiary")
			RETURN -1
		END IF
	ELSE
		// ls_beneficiary_required_flag = 'N', so delete beneficiary rows
		li_details_row = idw_contract_beneficiaries_details.RowCount()
		
		FOR li_counter = 1 TO li_details_row
			li_rtn = idw_contract_beneficiaries_details.DeleteRow(0)
			SQLCA.nf_handle_error('w_prepare_annuity_account','idw_contract_beneficiaries_details.DeleteRow','wf_check_bus_rule_annuity_contract')
			IF li_rtn < 0 THEN
				MessageBox('Deletion Error','A problem was encountered during the deletion of the annuity beneficiaries.' &
								  +'~r~n' &
								  +'~r~nPlease contact the HELPDESK',Exclamation!)
				RETURN 1
			END IF
		NEXT
		
		li_rtn = idw_contract_beneficiaries_details.Update()
			SQLCA.nf_handle_error('w_prepare_annuity_account','idw_contract_beneficiaries_details.Update','wf_check_bus_rule_annuity_contract')
		IF li_rtn < 0 THEN
			MessageBox('Update Error','A problem was encountered during the deletion of the annuity beneficiaries.' &
							  +'~r~n' &
							  +'~r~nPlease contact the HELPDESK',Exclamation!)
			RETURN 1
		END IF
		
	END IF

END IF


// BR 8.150	The annuity contract for the annuity payout must be confirmed, if the Confirm Annuity Payout, Contract Confirmed checklist step is completed.

// This module is not maintainable when Contract Confirmed checklist step is completed, so any code placed here to check this will not be executed.
// It (annuity contract flag)  will be set by the checklist code.



RETURN  0
end function

public function boolean wf_check_maintain_annuity_contract (long al_annuity_payout_no);
//8.10 Annuity contract and beneficiary information must not be maintained, if the 
//       Confirm Annuity Payout – Annuity Purchase, Contract Received checklist step is Incomplete
//8.20 Annuity contract and beneficiary information must not be maintained, if the 
//       Confirm Annuity Payout – Annuity Purchase, Contract Confirmed checklist step is Completed

STRING ls_contract_received, ls_contract_confirmed, ls_message
Boolean lb_rtn


SELECT cs.checklist_step_status_code , cs2.checklist_step_status_code 
INTO   :ls_contract_received, :ls_contract_confirmed
FROM   CHECKLIST_STEP             cs
JOIN   CHECKLIST_STEP            cs2 on cs.checklist_no = cs2.checklist_no
JOIN   CHECKLIST                   c on c.checklist_no = cs.checklist_no 
                                    and c.checklist_no = cs2.checklist_no 
JOIN   SUBSCRIBER_CHECKLIST_XREF scx on scx.checklist_no = c.checklist_no
JOIN   ANNUITY_ACCOUNT            aa on aa.checklist_subscriber_no = scx.checklist_subscriber_no
JOIN   ANNUITY_PAYOUT             ap on ap.annuity_account_no = aa.annuity_account_no
                                    and ap.confirm_annuity_payout_checklist_no = c.checklist_no
WHERE  ap.annuity_payout_no         = :al_annuity_payout_no
AND    c.checklist_type_code        = 'CAPAP'
AND    cs.checklist_step_type_code  = '021'
AND    cs2.checklist_step_type_code = '022'
USING SQLCA;

SQLCA.nf_handle_error('w_prepare_annuity_contract', 'wf_check_maintain_contract', 'SELECT from CHECKLIST_STEP')

IF ls_contract_received = 'INA' or ls_contract_confirmed = 'COM' THEN
	return false
ELSE
	return true
END IF

end function

public subroutine wf_enable_contract_buttons (boolean ab_state);
windowobject l_lwindow_controls[]
int li_cntr
commandbutton l_button

l_lwindow_controls = tab_payout.tabpage_contract_and_beneficiaries.control

FOR li_cntr = 1 to upperbound(l_lwindow_controls)
	if l_lwindow_controls[li_cntr].typeof() = commandbutton! THEN
		l_button = l_lwindow_controls[li_cntr]
		l_button.enabled = ab_state
	END IF				 
NEXT
end subroutine

public function long wf_get_next_contract_no ();
long ll_annuity_contract_no


UPDATE Last_Annuity_Contract_No 
SET       last_annuity_contract_no = last_annuity_contract_no + 1
FROM    Last_Annuity_Contract_No ;
SQLCA.nf_handle_error('w_prepare_annuity_account','cb_save_contract', 'UPDATE Last_Annuity_Contract_No')
		
SELECT last_annuity_contract_no
INTO   :ll_annuity_contract_no
FROM   Last_Annuity_Contract_No ;
SQLCA.nf_handle_error('w_prepare_annuity_account','cb_save_contract', 'SELECT last_annuity_contract_no')

RETURN ll_annuity_contract_no
end function

public function boolean wf_display_overpayment_tab (long al_paa_checklist_no);BOOLEAN lb_overpayment_tab_visible
INTEGER li_checklist_step_count, li_overpayment_count

// if the confirm overpayment checklist step is not completed, then do not display overpayment tab
// if the confirm overpayment checklist step is completed, but there are no overpayment details then do not display overpayment tab
// otherwise, display the tab
SELECT COUNT(*)
INTO   :li_checklist_step_count
FROM   CHECKLIST_STEP
WHERE  checklist_no             = :al_paa_checklist_no
AND    checklist_step_no        = 3
AND    checklist_step_type_code = '016'
AND    concluded_date IS NOT NULL
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT COUNT(*) FROM CHECKLIST_STEP...','wf_display_overpayment_tab')

SELECT COUNT(*)
INTO   :li_overpayment_count
FROM   ANNUITY_PAYOUT_TXN_DETAIL
WHERE  annuity_payout_no     = :il_annuity_payout_no
AND    payment_sub_type_code = 'CO'
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT COUNT(*) FROM ANNUITY_PAYOUT_TXN_DETAIL...','wf_display_overpayment_tab')


IF li_checklist_step_count = 0 THEN
	lb_overpayment_tab_visible = FALSE
ELSE
	IF li_overpayment_count = 0 THEN
		lb_overpayment_tab_visible = FALSE
	ELSE
		lb_overpayment_tab_visible = TRUE
	END IF
END IF

tab_payout.tabpage_overpayment.Visible = lb_overpayment_tab_visible

RETURN lb_overpayment_tab_visible
end function

public subroutine wf_update_annuity_payout_status (string as_annuity_payout_status_code, string as_annuity_payout_status_reason_code, string as_comment);
// if the status/reason is not validated, an app error
inv_common_annuity.nf_validate_annuity_status(il_annuity_payout_no,as_annuity_payout_status_code,as_annuity_payout_status_reason_code)

// update ANNUITY_PAYOUT
UPDATE ANNUITY_PAYOUT
SET    annuity_payout_status_code         = :as_annuity_payout_status_code,
       annuity_payout_status_reason_code  = :as_annuity_payout_status_reason_code,
		 annuity_eligibility_confirmed_flag = 'Y',       
		 payout_comment                     = :as_comment
WHERE  annuity_payout_no = :il_annuity_payout_no
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded sql: UPDATE ANNUITY_PAYOUT...','wf_cancel_paa_checklist')

end subroutine

public subroutine wf_reject_checklist_status_change (boolean ab_message, string as_message_title, string as_message, long al_checklist_no, string as_checklist_type_code, n_checklist anv_checklist, ref u_checklist_datawindow adw_checklist);INTEGER    li_trancount


IF ab_message THEN
	MessageBox(as_message_title,as_message,Exclamation!)
END IF

// only rollback if there is a txn open
SQLCA.nf_transaction_count(li_trancount,1,'','','',FALSE)
IF li_trancount = 1 THEN
	SQLCA.nf_rollback_transaction()
END IF

					
// the retrieval will set the dw item status to not modified & prevent firing of ue_itemchangeaccepted
anv_checklist.nf_retrieve_checklists(as_checklist_type_code, al_checklist_no)
inv_common_annuity.nf_select_next_available_checklist_step(anv_checklist,adw_checklist,al_checklist_no,as_checklist_type_code)
SetRedraw(TRUE)
end subroutine

public function integer wf_auto_complete_next_step (string as_checklist_step_type_code, string as_checklist_step_status_code, ref u_checklist_datawindow adw_dw, string as_data, string as_status_assigned_method_code, ref u_checklist a_checklist, ref n_checklist anv_checklist, ref boolean ab_commit);DWObject   l_dwo
INTEGER    li_find
LONG       ll_rtn

/* completes the next checklist step as indicated by arguments */

li_find = adw_dw.uf_find_row('checklist_step_type_code', '"'+as_checklist_step_type_code+'"')
IF li_find > 0 THEN
	adw_dw.SetItem(li_find, 'checklist_step_status_code',as_checklist_step_status_code)
	adw_dw.SetItem(li_find, 'step_checked', 'Y')
	ab_commit = FALSE
			
	l_dwo = adw_dw.object.checklist_step_status_code
	
	a_checklist.TRIGGER Event ue_checklist_step_status_changed(adw_dw,li_find,l_dwo,as_data,as_status_assigned_method_code,ll_rtn)
			
	adw_dw.POST Event ItemChanged(li_find,l_dwo,as_checklist_step_status_code)
	RETURN 0
ELSE
	wf_reject_checklist_status_change(TRUE,'Missing Step','Missing step '+as_checklist_step_type_code+' - call helpdesk.',il_checklist_no,'PAA',anv_checklist,adw_dw)
	RETURN -1
END IF
end function

public subroutine wf_post_hscrollbar_position ();INTEGER  li_column
LONG     ll_max_hscrollpos


ll_max_hscrollpos = Long(dw_payout_accounts.object.datawindow.horizontalscrollmaximum)

IF ll_max_hscrollpos > 0 THEN
	
	li_column = dw_payout_accounts.GetColumn()
	
	CHOOSE CASE li_column
		CASE 20
			// admin region
			dw_payout_accounts.object.datawindow.horizontalscrollposition = '0'
		CASE 4
			// individual
			dw_payout_accounts.object.datawindow.horizontalscrollposition = String(Int((507/6185)*ll_max_hscrollpos))
			
		CASE 11
			// SIN
			dw_payout_accounts.object.datawindow.horizontalscrollposition = String(Int((850/6185)*ll_max_hscrollpos))
			
		CASE 5
			// name
			dw_payout_accounts.object.datawindow.horizontalscrollposition = String(Int((1317/6185)*ll_max_hscrollpos))
			
		CASE 8
			// role
			dw_payout_accounts.object.datawindow.horizontalscrollposition = String(Int((2245/6185)*ll_max_hscrollpos))
			
		CASE 9
			// claim
			dw_payout_accounts.object.datawindow.horizontalscrollposition = String(Int((2414/6185)*ll_max_hscrollpos))
			
		CASE 10
			// age
			dw_payout_accounts.object.datawindow.horizontalscrollposition = String(Int((2752/6185)*ll_max_hscrollpos))
			
		CASE 13
			// birth date
			dw_payout_accounts.object.datawindow.horizontalscrollposition = String(Int((2889/6185)*ll_max_hscrollpos))
			
		CASE 14
			// death date
			dw_payout_accounts.object.datawindow.horizontalscrollposition = String(Int((3246/6185)*ll_max_hscrollpos))
			
		CASE 15
			// annuity start date
			dw_payout_accounts.object.datawindow.horizontalscrollposition = String(Int((3602/6185)*ll_max_hscrollpos))
					
		CASE 16
			// annuity end date
			dw_payout_accounts.object.datawindow.horizontalscrollposition = String(Int((3959/6185)*ll_max_hscrollpos))
					
		CASE 23
			// checklist type
			dw_payout_accounts.object.datawindow.horizontalscrollposition = String(Int((4315/6185)*ll_max_hscrollpos))
					
		CASE 24
			// step completed
			dw_payout_accounts.object.datawindow.horizontalscrollposition = String(Int((4649/6185)*ll_max_hscrollpos))
				
		CASE 17
			// benefit_option_code
			dw_payout_accounts.object.datawindow.horizontalscrollposition = String(Int((5664/6185)*ll_max_hscrollpos))
			
		CASE 18
			// set aside %
			dw_payout_accounts.object.datawindow.horizontalscrollposition = String(Int((5902/6185)*ll_max_hscrollpos))
			
		CASE 31
			// sub ledger balance
			dw_payout_accounts.object.datawindow.horizontalscrollposition = string(ll_max_hscrollpos)
			
	END CHOOSE

END IF
SetRedraw(TRUE)

end subroutine

public function integer wf_verify_recipient_address ();INTEGER    li_count

IF ib_more_than_forty_chars THEN
	SELECT COUNT(*)
	INTO   :li_count
	FROM   ANNUITY_PAYOUT_RECIPIENT
	WHERE  annuity_payout_no = :il_annuity_payout_no
	AND  ( address_line2 = '' 
		 OR address_line3 = '' )
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT COUNT(*) FROM ANNUITY_PAYOUT_RECIPIENT...','wf_verify_recipient_address (1)')
ELSE
	SELECT COUNT(*)
	INTO   :li_count
	FROM   ANNUITY_PAYOUT_RECIPIENT
	WHERE  annuity_payout_no = :il_annuity_payout_no
	AND  ( address_line1 = '' 
		 OR address_line2 = '' )
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT COUNT(*) FROM ANNUITY_PAYOUT_RECIPIENT...','wf_verify_recipient_address (2)')
END IF

RETURN li_count
end function

public function integer wf_verify_contract_confirmed ();INTEGER  li_beneficiary_count
LONG     ll_annuity_contract_no
STRING   ls_beneficiary_required_flag


SELECT IsNull(a.annuity_contract_no,0),
       b.beneficiary_required_flag
INTO   :ll_annuity_contract_no,
       :ls_beneficiary_required_flag
FROM   ANNUITY_CONTRACT a
JOIN   Annuity_Term     b ON a.annuity_term_code = b.annuity_term_code
WHERE  annuity_payout_no = :il_annuity_payout_no
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT IsNull(annuity_contract_no,0) FROM ANNUITY_CONTRACT...','wf_verify_contract_confirmed')


IF ll_annuity_contract_no > 0 THEN
	IF ls_beneficiary_required_flag = 'Y' THEN
		SELECT Count(*)
		INTO   :li_beneficiary_count
		FROM   ANNUITY_CONTRACT_BENEFICIARY
		WHERE  annuity_contract_no = :ll_annuity_contract_no
		USING SQLCA;
		SQLCA.nf_Handle_error('w_prepare_annuity_account','embedded SQL: SELECT Count(*) FROM ANNUITY_CONTRACT_BENEFICIARY...','wf_verify_contract_confirmed')
		
		RETURN li_beneficiary_count
	ELSE
		// one contract retrieved
		RETURN 1
	END IF
ELSE
	RETURN 0
END IF
end function

public subroutine wf_confirm_payout_amount ();UPDATE ANNUITY_PAYOUT
SET    payout_amount_confirmed_by_user_id = :vgst_user_profile.user_id,
       payout_amount_confirmed_date       = getDate()
WHERE  annuity_payout_no = :il_annuity_payout_no
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: UPDATE ANNUITY_PAYOUT...','wf_confirm_payout_amount')
end subroutine

public function integer wf_unprocessed_payout_txn_count ();INTEGER    li_count

SELECT COUNT(*)
INTO   :li_count
FROM   ANNUITY_PAYOUT_TXN_DETAIL a
JOIN   UNAPPLIED_CLAIM_TXN       b ON a.txn_no = b.txn_no
WHERE  annuity_payout_no = :il_annuity_payout_no
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT COUNT(*) FROM ANNUITY_PAYOUT_TXN_DETAIL, UNAPPLIED_CLAIM_TXN...','wf_unprocessed_txn_count')

RETURN li_count
end function

public function integer wf_unprocessed_subledger_txn_count ();INTEGER   li_unprocessed_count



IF is_claim_role_code = 'C' THEN
	SELECT Count(*)
	INTO   :li_unprocessed_count
	FROM   PAYMENT             a
	JOIN   UNAPPLIED_CLAIM_TXN b ON a.payment_no = b.payment_no
	JOIN   CLAIM_PARTICIPANT   c ON b.claim_no     = c.claim_no
	                            AND b.recipient_no = c.individual_no
	WHERE  a.payment_type_code = '97'
	AND    b.recipient_no      = :il_individual_no
	AND    c.claim_role_code   = 'C'
	AND NOT EXISTS ( SELECT *
	                 FROM   ANNUITY_SETASIDE_PRE_1993 c
						  WHERE  c.setaside_payment_no = a.payment_no )
	AND NOT EXISTS ( SELECT *
	                 FROM   ANNUITY_PAYOUT_PRE_1993 d
						  WHERE  d.payout_payment_no = a.payment_no )
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT Count(*) FROM PAYMENT, UNAPPLIED_CLAIM_TXN','wf_verify_subledger')
	
ELSEIF is_claim_role_code = 'SS' THEN
	SELECT Count(*)
	INTO   :li_unprocessed_count
	FROM   PAYMENT a
	JOIN   UNAPPLIED_CLAIM_TXN b ON a.payment_no = b.payment_no
	JOIN   CLAIM_PARTICIPANT   c ON b.claim_no     = c.claim_no
	                            AND b.recipient_no = c.individual_no
	WHERE  a.payment_type_code = '97'
	AND    b.recipient_no      = :il_individual_no
	AND    b.claim_no          = :il_annuity_account_claim_no
	AND    c.claim_role_code   = 'SS'
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT Count(*) FROM PAYMENT, UNAPPLIED_CLAIM_TXN','wf_verify_subledger')

END IF

RETURN li_unprocessed_count

end function

public function integer wf_verify_subledger (ref string as_error_message);
DECIMAL   ldec_sub_ledger, ldec_payout
INTEGER   li_unprocessed_count, li_rtn


// verify that there are no unprocessed sub-ledger txns prior to creating payout txns
li_unprocessed_count = wf_unprocessed_subledger_txn_count()
IF li_unprocessed_count > 0 THEN
	IF is_claim_role_code = 'C' THEN
		as_error_message = "There are unprocessed sub-ledger transactions for the injured worker's annuity account."
	ELSEIF is_claim_role_code = 'SS' THEN
		as_error_message = "There are unprocessed sub-ledger transactions for the surviving spouse's annuity account."
	END IF
	RETURN -1
END IF

// verify that subledger is offset by payout amount
li_rtn = wf_subledger_payout_match(ldec_sub_ledger, ldec_payout)

IF li_rtn < 0 THEN
	IF is_claim_role_code = 'C' THEN
		as_error_message = "The annuity payout will not offset the existing sub-ledger transactions for the injured worker's annuity account."
	ELSEIF is_claim_role_code = 'SS' THEN
		as_error_message = "The annuity payout will not offset the existing sub-ledger transactions for the surviving spouse's annuity account."
	END IF
	as_error_message = as_error_message + '~r~n~t The annuity sub-ledger balance is: ' + String(ldec_sub_ledger,'$#,##0.00;($#,##0.00)') &
                   		               + '~r~n~t The annuity payout amount is: ' + String(ldec_payout,'$#,##0.00;($#,##0.00)')
	RETURN -1
END IF


RETURN 0

end function

public subroutine wf_post_commit_retrieval ();INTEGER    li_rows, li_find
LONG       ll_hold_individual_no, ll_paa_checklist_no

// after a checklist step has been completed, a re-retrieval of payout list, etc


// do not re-retrieve list until commit
IF il_forced_individual_no = 0 OR rb_region.Checked THEN
	ll_hold_individual_no = il_individual_no
	li_rows = dw_payout_accounts.Retrieve(0,vgst_user_profile.default_admin_region_code,'entry')
ELSE
	ll_hold_individual_no = il_forced_individual_no
	li_rows = dw_payout_accounts.Retrieve(il_forced_individual_no,vgst_user_profile.default_admin_region_code,'entry')
END IF
SQLCA.nf_handle_error('w_prepare_for_annuity_account','dw_payout_accounts.retrieve','uo_prepare_checklist.ue_checklist_itemchangeaccepted')

IF li_rows = 1 THEN	
	// retrieve confirm datawindows
	// Only one row in payout list datawindow, so must trigger rowfocuschanged
	dw_payout_accounts.Trigger Event RowFocusChanged(1)
	
	ll_paa_checklist_no = dw_payout_accounts.GetItemNumber(1,'prepare_annuity_payout_checklist_no')
	wf_display_claim_details_tab(ll_paa_checklist_no)
ELSEIF li_rows = 0 THEN
	wf_clear_datawindows()
	wf_clear_filter()
	IF is_claim_role_code = 'C' THEN
		MessageBox('No records','The checklist for the injured worker has been completed. There are no records left.')
	ELSE
		MessageBox('No records','The checklist for the surviving spouse has been completed. There are no records left.')
	END IF
ELSE
	li_find = dw_payout_accounts.uf_find_row('individual_no',String(ll_hold_individual_no))

	IF li_find > 0 THEN
		IF li_find = 1 THEN
			// if the first row was originally chosen, then the RFC will not have fired
			// so, we have to trigger it so that all the datawindows (on tabs, in checklist object, etc) get retrieved.
			dw_payout_accounts.Trigger Event RowFocusChanged(1)
		END IF
		dw_payout_accounts.ScrollToRow(li_find)
		dw_payout_accounts.SelectRow(li_find,TRUE)
		ll_paa_checklist_no = dw_payout_accounts.GetItemNumber(li_find,'prepare_annuity_payout_checklist_no')
		wf_display_claim_details_tab(ll_paa_checklist_no)
	ELSE
		// could not find the individual in the annuity payout list
		// the person might have been removed from the list due to completion of payout
		// or might be filtered out.
		// so - re-retrieve datawindows for first row in list
		dw_payout_accounts.Trigger Event RowFocusChanged(1)
	END IF
	
	
END IF

end subroutine

public function string wf_get_annuity_payout_type ();STRING  ls_annuity_payout_type_code


SELECT annuity_payout_type_code
INTO   :ls_annuity_payout_type_code
FROM   ANNUITY_PAYOUT
WHERE  annuity_payout_no = :il_annuity_payout_no
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT annuity_payout_type_code FROM ANNUITY_PAYOUT...','wf_get_annuity_payout_type')

RETURN ls_annuity_payout_type_code
end function

public subroutine wf_get_current_name_address (long al_annuity_payout_recipient_no, string as_annuity_payout_recipient_type_code, ref string as_current_name, ref string as_current_address_line1, ref string as_current_address_line2, ref string as_current_address_line3, ref string as_current_address_line4, ref string as_current_address_line5);STRING   ls_current_city,ls_current_prov_state_code,ls_current_postal_code,ls_current_country_desc


IF as_annuity_payout_recipient_type_code = 'I' THEN
	// individual
	SELECT a.given_names + ' ' + a.last_name,
			 a.address_line1,
			 a.address_line2,
			 a.city,
			 a.prov_state_code,
			 a.postal_code,
			 b.country_desc
	INTO   :as_current_name,
			 :as_current_address_line1,
			 :as_current_address_line2,
			 :ls_current_city, 
			 :ls_current_prov_state_code, 
			 :ls_current_postal_code, 
			 :ls_current_country_desc
	FROM   INDIVIDUAL a
	JOIN   Country    b ON a.country_code = b.old_country_code
	WHERE  individual_no = :al_annuity_payout_recipient_no
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT given_names + last_name FROM INDIVIDUAL...','wf_get_current_name_address')
ELSE
	// provider
	SELECT a.name,
			 a.address_line1,
			 a.address_line2,
			 a.city,
			 a.prov_state_code,
			 a.postal_code,
			 b.country_desc
	INTO   :as_current_name,
			 :as_current_address_line1,
			 :as_current_address_line2,
			 :ls_current_city, 
			 :ls_current_prov_state_code, 
			 :ls_current_postal_code, 
			 :ls_current_country_desc
	FROM   PROVIDER a
	JOIN   Country  b ON a.country_code = b.old_country_code
	WHERE  a.provider_no = :al_annuity_payout_recipient_no
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT name FROM PROVIDER...','wf_get_current_name_address')
END IF

IF as_current_address_line2 = '' THEN
	as_current_address_line2 = Trim(ls_current_city + ' ' + ls_current_prov_state_code + ' ' + ls_current_postal_code)
	as_current_address_line3 = ls_current_country_desc
	as_current_address_line4 = ''
ELSE
	as_current_address_line3 = Trim(ls_current_city + ' ' + ls_current_prov_state_code + ' ' + ls_current_postal_code)
	as_current_address_line4 = ls_current_country_desc
END IF
end subroutine

public function integer wf_determine_name_address_change ();BOOLEAN    lb_name_change, lb_address_change, lb_change, lb_old_more_than_forty_chars, lb_new_more_than_forty_chars
INTEGER    li_counter, li_rowcount, li_upperbound, li_pos_ampersand, li_pos
LONG       ll_annuity_payout_recipient_no
STRING     ls_recipient_name, ls_old_purchase_name, ls_new_purchase_name, ls_annuity_carrier_name
STRING     ls_recipient_address_line1, ls_recipient_address_line2, ls_recipient_address_line3, ls_recipient_address_line4, ls_recipient_address_line5
STRING     ls_annuity_payout_recipient_type_code, ls_annuity_payout_type_code
INTEGER    li_max_completed_annuity_purchase_step
STRING     ls_current_name, ls_current_address_line1, ls_current_address_line2, ls_current_address_line3, ls_current_address_line4, ls_current_address_line5
STRING     ls_recipient_type_text, ls_rtn, ls_change_type, ls_eligible_individual_name
S_NAME_ADDRESS_CHANGE   lstr_name_address_change

ls_annuity_payout_type_code = wf_get_annuity_payout_type()

// recipient tab may not be selected, so retrieve the dw's
wf_retrieve_tab(6)

li_rowcount = idw_payout_recipients.RowCount()

FOR li_counter = 1 TO li_rowcount
	ll_annuity_payout_recipient_no = idw_payout_recipients.GetItemNumber(li_counter,'annuity_payout_recipient_no')
	ls_annuity_payout_recipient_type_code = idw_payout_recipients.GetItemString(li_counter,'annuity_payout_recipient_type_code')
	
	IF ls_annuity_payout_recipient_type_code = 'I' THEN
		ls_recipient_type_text = 'individual'
	ELSE
		ls_recipient_type_text = 'provider'
	END IF
	
	IF ls_annuity_payout_type_code = 'P' THEN
		IF ls_annuity_payout_recipient_type_code <> 'I' THEN
			// we are not interested in changes to WHSCC - this should never happen
			CONTINUE
		END IF
		
		li_max_completed_annuity_purchase_step = inv_confirm_payout_checklist.nf_max_completed_step(il_checklist_no)
		
		ls_recipient_name = idw_payout_recipients.GetItemString(li_counter,'name_on_cheque')
		
		IF li_max_completed_annuity_purchase_step = 6 THEN
			// the confirm contract checklist step is being completed
			// and an attempt is being made to complete the request payout checklist step
			lstr_name_address_change.annuity_purchase_step = 'REQUEST'
			
			// include the address because the user has entered one
			
			// this is the address of the annuity recipient
			ls_recipient_address_line1 = idw_payout_recipients.GetItemString(li_counter,'address_line1')
			ls_recipient_address_line2 = idw_payout_recipients.GetItemString(li_counter,'address_line2')
			ls_recipient_address_line3 = idw_payout_recipients.GetItemString(li_counter,'address_line3')
			ls_recipient_address_line4 = idw_payout_recipients.GetItemString(li_counter,'address_line4')
			ls_recipient_address_line5 = idw_payout_recipients.GetItemString(li_counter,'address_line5')
		
		
			SELECT annuity_carrier_name
			INTO   :ls_annuity_carrier_name
			FROM   ANNUITY_CONTRACT
			WHERE  annuity_payout_no = :il_annuity_payout_no
			USING SQLCA;
			SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT annuity_carrier_name FROM ANNUITY_CONTRACT...','wf_determine_name_address_change')
			
			// if this function finds the annuity carrier name in the recipient name, then the 
			// individual name appended to annuity carrier was 40 chars or less
			li_pos = Pos (ls_recipient_name,ls_annuity_carrier_name)
			IF li_pos > 0 THEN
				lb_old_more_than_forty_chars = FALSE
			ELSE
				lb_old_more_than_forty_chars = TRUE
			END IF
			
			wf_build_annuity_purchase_name_on_chq(ls_new_purchase_name,lb_new_more_than_forty_chars,FALSE,'','')
			// if the changed name is more than 40 characters when appended to annuity carrier
			IF lb_new_more_than_forty_chars THEN
				// if the old name was more than 40 characters when appended to annuity carrier
				IF lb_old_more_than_forty_chars THEN
					// determine if current name can be be found in name_on_cheque,
					// and that rebuilt name_on_cheque with appended carrier 
					// is same length
					wf_get_current_name(ll_annuity_payout_recipient_no,ls_annuity_payout_recipient_type_code,ls_current_name)
					
					li_pos = Pos (ls_recipient_name,ls_current_name)
					IF li_pos > 0 THEN
						IF Len(ls_recipient_name + ls_recipient_address_line1) = Len(ls_current_name + ' & ' + ls_annuity_carrier_name) THEN
							lb_address_change = FALSE
							GOTO RETURN_RESULT
						ELSE
							lb_address_change = TRUE
						END IF
					ELSE
						lb_address_change = TRUE
					END IF
					
					ls_current_name = Left(ls_new_purchase_name,40)
					
					ls_current_address_line1 = Right(ls_new_purchase_name,Len(ls_new_purchase_name) - 40)
					ls_current_address_line2 = ls_recipient_address_line2
					ls_current_address_line3 = ls_recipient_address_line3
					ls_current_address_line4 = ls_recipient_address_line4
					ls_current_address_line5 = ls_recipient_address_line5
					lb_address_change = TRUE
				ELSE
					// old name was not more than 40 characters when appended to annuity carrier
					ls_current_name = Left(ls_new_purchase_name,40)
					
					ls_current_address_line1 = Right(ls_new_purchase_name,Len(ls_new_purchase_name) - 40)
					ls_current_address_line2 = ls_recipient_address_line1
					ls_current_address_line3 = ls_recipient_address_line2
					ls_current_address_line4 = ls_recipient_address_line3
					ls_current_address_line5 = ls_recipient_address_line4
					lb_address_change = TRUE
				END IF
			ELSE
				IF lb_old_more_than_forty_chars THEN
					// old name was more than 40 characters when appended to annuity carrier
					ls_current_name = ls_new_purchase_name
					
					ls_current_address_line1 = ls_recipient_address_line2
					ls_current_address_line2 = ls_recipient_address_line3
					ls_current_address_line3 = ls_recipient_address_line4
					ls_current_address_line4 = ls_recipient_address_line5
					ls_current_address_line5 = ''
									
					lb_address_change = TRUE
				ELSE
					// old name was not more than 40 characters when appended to annuity carrier
					ls_current_name = ls_new_purchase_name
					
					ls_current_address_line1 = ls_recipient_address_line1
					ls_current_address_line2 = ls_recipient_address_line2
					ls_current_address_line3 = ls_recipient_address_line3
					ls_current_address_line4 = ls_recipient_address_line4
					ls_current_address_line5 = ls_recipient_address_line5
									
					lb_address_change = FALSE
				END IF
			END IF
			
		ELSEIF li_max_completed_annuity_purchase_step = 5 THEN
			// the contract received checklist step is being completed
			// and an attempt is being made to complete the confirm contract checklist step
			lstr_name_address_change.annuity_purchase_step = 'CONTRACT'
			
			// so, DO NOT include the full "name_on_cheque", because it only has " & ANNUITY CARRIER" appended to the end
			li_pos_ampersand = Pos(ls_recipient_name,'&')
			IF li_pos_ampersand = 0 THEN
				MessageBox('Name on Cheque Error','There was an error setting the name on the cheque for the annuity purchase. Please contact the HELPDESK.')
				RETURN -1
			END IF
			
			// this is the name that is on the annuity recipient
			ls_recipient_name = Left(ls_recipient_name, li_pos_ampersand - 2)
			
			// get the current name
			wf_get_current_name(ll_annuity_payout_recipient_no,ls_annuity_payout_recipient_type_code,ls_current_name)
			
			// do not included address because there isn't one yet
			lb_address_change = FALSE
		END IF
	ELSE
		// it's a lump sum, not an annuity purchase
		ls_recipient_name = idw_payout_recipients.GetItemString(li_counter,'name_on_cheque')
		
		// this is the address of the annuity recipient
		ls_recipient_address_line1 = idw_payout_recipients.GetItemString(li_counter,'address_line1')
		ls_recipient_address_line2 = idw_payout_recipients.GetItemString(li_counter,'address_line2')
		ls_recipient_address_line3 = idw_payout_recipients.GetItemString(li_counter,'address_line3')
		ls_recipient_address_line4 = idw_payout_recipients.GetItemString(li_counter,'address_line4')
		ls_recipient_address_line5 = idw_payout_recipients.GetItemString(li_counter,'address_line5')
		
		// get the name & address from the individual's current record
		wf_get_current_name_address(ll_annuity_payout_recipient_no,ls_annuity_payout_recipient_type_code,ls_current_name,ls_current_address_line1,ls_current_address_line2,ls_current_address_line3,ls_current_address_line4,ls_current_address_line5)
		
		IF  ls_recipient_address_line1 <> ls_current_address_line1 &
		 OR ls_recipient_address_line2 <> ls_current_address_line2 & 
		 OR ls_recipient_address_line3 <> ls_current_address_line3 & 
		 OR ls_recipient_address_line4 <> ls_current_address_line4 & 
		 OR ls_recipient_address_line5 <> ls_current_address_line5 THEN
			lb_address_change = TRUE
		END IF
	END IF


// ** are there changes?	
	IF ls_current_name <> ls_recipient_name THEN
		lb_name_change = TRUE
	END IF
	
	IF lb_address_change OR lb_name_change THEN
		IF lb_change = FALSE THEN
			lb_change = TRUE
		END IF
		
		li_upperbound = UpperBound(lstr_name_address_change.change_message) + 1
				
		// build message
		IF lb_name_change = TRUE and lb_address_change = FALSE THEN
			ls_change_type = 'name'
		ELSEIF lb_name_change = TRUE and lb_address_change = TRUE THEN
			ls_change_type = 'name and address'
		ELSEIF lb_name_change = FALSE and lb_address_change = TRUE THEN
			ls_change_type = 'address'
		END IF
		
		// get IW/SS current name
		wf_get_current_name(il_individual_no,'I',ls_eligible_individual_name)
		
		// build message
		IF ls_annuity_payout_type_code <> 'P' THEN
			lstr_name_address_change.change_header = &
                       'Annuity Payout# '+String(il_annuity_payout_no)+' - Lump Sum'
			lstr_name_address_change.change_message[li_upperbound] = &
							  'The '+ls_recipient_type_text+' recipient '+ls_change_type+' for this payout has changed since the overpayment recovery was confirmed.'&
							 +'~r~n~r~n'&
							 +'Note: The '+ls_change_type+' of an annuity payout recipient cannot be changed after an annuity payout has been requested.' &
							 +'~r~n~r~n'
				lstr_name_address_change.change_footer = &
				           '   To CONTINUE with the NEW information, update the Annuity Payout Recipient and complete this checklist step, click Continue.' &
							 +'~r~n~r~n'&
							 +'   To CANCEL and make necessary corrections, click Cancel.'
		ELSE
			// it is an annuity purchase			
			lstr_name_address_change.change_header = &
                       'Annuity Payout# '+String(il_annuity_payout_no)+' - Annuity Purchase'
			IF li_max_completed_annuity_purchase_step = 5 THEN
				// attempting to confirm contract for purchase
				lstr_name_address_change.change_message[li_upperbound] = &
							  'The '+ls_recipient_type_text+' recipient '+ls_change_type+' for this payout has changed since the overpayment recovery was confirmed.'&
							 +'~r~n~r~n'&
							 +'Note: The '+ls_change_type+' of an annuity payout recipient cannot be changed after an annuity contract has been confirmed.' &
							 +'~r~n~r~n'
				lstr_name_address_change.change_footer = &
				           '   To CONTINUE with the NEW information, update the Annuity Payout Recipient and complete this checklist step, click Continue.' &
							 +'~r~n~r~n'&
							 +'   To CANCEL and make necessary corrections, click Cancel.'
			ELSEIF li_max_completed_annuity_purchase_step = 6 THEN
				// attempting to request payout for purchase
				lstr_name_address_change.change_message[li_upperbound] =  &
							  'The '+ls_recipient_type_text+' recipient '+ls_change_type+' for this payout has changed since the annuity contract was confirmed.'&
							 +'~r~n~r~n'&
							 +'Note: The '+ls_change_type+' of an annuity payout recipient cannot be changed after an annuity contract has been confirmed.' &
							 +'~r~n~r~n'
				lstr_name_address_change.change_footer = &
				           '   To CONTINUE with the OLD information and complete this checklist step, click Continue.' &
							 +'~r~n~r~n'&
							 +'   To CANCEL and make necessary corrections, click Cancel.'
			END IF
		END IF
		
		IF is_claim_role_code = 'C' THEN
			lstr_name_address_change.change_header = lstr_name_address_change.change_header + '~t~t~tClaimant: ('+ls_eligible_individual_name+') Individual# '+String(il_individual_no)
		ELSE
			lstr_name_address_change.change_header = lstr_name_address_change.change_header + '~t~t~tSurviving Spouse: ('+ls_eligible_individual_name+') Individual# '+String(il_individual_no)+ '~tClaim# '+String(il_annuity_account_claim_no)
		END IF
		

		lstr_name_address_change.old_name[li_upperbound] = ls_recipient_name
		lstr_name_address_change.new_name[li_upperbound] = ls_current_name
		
		lstr_name_address_change.old_address_line1[li_upperbound] = ls_recipient_address_line1
		lstr_name_address_change.new_address_line1[li_upperbound] = ls_current_address_line1
		
		lstr_name_address_change.old_address_line2[li_upperbound] = ls_recipient_address_line2
		lstr_name_address_change.new_address_line2[li_upperbound] = ls_current_address_line2
		
		lstr_name_address_change.old_address_line3[li_upperbound] = ls_recipient_address_line3
		lstr_name_address_change.new_address_line3[li_upperbound] = ls_current_address_line3
		
		lstr_name_address_change.old_address_line4[li_upperbound] = ls_recipient_address_line4
		lstr_name_address_change.new_address_line4[li_upperbound] = ls_current_address_line4
		
		lstr_name_address_change.old_address_line5[li_upperbound] = ls_recipient_address_line5
		lstr_name_address_change.new_address_line5[li_upperbound] = ls_current_address_line5		
		
		// reset variables
		lb_address_change = FALSE
		lb_name_change = FALSE
		
		ls_recipient_name = ''
		ls_recipient_address_line1 = ''
		ls_recipient_address_line2 = ''
		ls_recipient_address_line3 = ''
		ls_recipient_address_line4 = ''
		ls_recipient_address_line5 = ''
		ls_current_name = ''
		ls_current_address_line1 = ''
		ls_current_address_line2 = ''
		ls_current_address_line3 = ''
		ls_current_address_line4 = ''
		ls_current_address_line5 = ''
		
	END IF
NEXT

RETURN_RESULT:

IF lb_change THEN
	OpenWithParm(w_name_address_change_warning,lstr_name_address_change)
	
	ls_rtn = Message.StringParm
	
	IF ls_rtn = 'YES' THEN
		RETURN 1
	ELSE
		RETURN -1
	END IF
ELSE
	RETURN 0
END IF


end function

public subroutine wf_get_current_name (long al_annuity_payout_recipient_no, string as_annuity_payout_recipient_type_code, ref string as_current_name);

IF as_annuity_payout_recipient_type_code = 'I' THEN
	// individual
	SELECT given_names + ' ' + last_name
	INTO   :as_current_name
	FROM   INDIVIDUAL
	WHERE  individual_no = :al_annuity_payout_recipient_no
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT given_names + last_name FROM INDIVIDUAL...','wf_get_current_name')
ELSE
	// provider
	SELECT name
	INTO   :as_current_name
	FROM   PROVIDER
	WHERE  provider_no = :al_annuity_payout_recipient_no
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT name FROM PROVIDER...','wf_get_current_name')
END IF

end subroutine

public function integer wf_determine_lump_sum_recipient (ref string as_error_message);INTEGER               li_outer_counter, li_inner_counter, li_upper, li_datastore_counter, li_datastore_rows, li_update_rtn
LONG                  ll_annuity_payout_recipient_no
STRING                ls_address_line1, ls_address_line2, ls_address_line3, ls_address_line4, ls_address_line5
STRING                ls_original_address_line1, ls_original_address_line2, ls_original_city, ls_original_prov_state_code, ls_original_postal_code, ls_original_country_code
STRING                ls_annuity_payout_recipient_type_code, ls_annuity_payout_recipient_type_desc, ls_lump_sum_recipient_name
STRING                ls_address_array[], ls_null_array[], ls_hold_array[]


// updates non-WHSCC recipients for lump sum payouts, uses INDIVIDUAL or PROVIDER table columns, removes any blank lines
// address_line5 is never populated 

IF IsValid(ids_non_whscc_recipients) THEN
	DESTROY ids_non_whscc_recipients
END IF

ids_non_whscc_recipients = Create U_DS
ids_non_whscc_recipients.DataObject = 'ds_non_whscc_recipients'
ids_non_whscc_recipients.SetTransObject(SQLCA)

li_datastore_rows = ids_non_whscc_recipients.Retrieve(il_annuity_payout_no)
SQLCA.nf_handle_error('w_prepare_annuity_account','lds_non_whscc_recipients.Retrieve','wf_set_lump_sum_recipient')

FOR li_datastore_counter = 1 TO li_datastore_rows
	
	ll_annuity_payout_recipient_no        = ids_non_whscc_recipients.GetItemNumber(li_datastore_counter,'annuity_payout_recipient_no')
	ls_annuity_payout_recipient_type_code = ids_non_whscc_recipients.GetItemString(li_datastore_counter,'annuity_payout_recipient_type_code')
	
	IF ls_annuity_payout_recipient_type_code = 'I' THEN
		SELECT given_names +' '+ last_name,
				 address_line1, 
				 address_line2, 
				 city,
				 prov_state_code,
				 postal_code,
				 country_code
		INTO   :ls_lump_sum_recipient_name,
				 :ls_original_address_line1, 
				 :ls_original_address_line2,
				 :ls_original_city,
				 :ls_original_prov_state_code,
				 :ls_original_postal_code,
				 :ls_original_country_code
		FROM   INDIVIDUAL
		WHERE  individual_no = :ll_annuity_payout_recipient_no
		USING SQLCA;
		SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT address_line1,address_line2,city,prov_state_code,country FROM INDIVIDUAL...','wf_set_lump_sum_recipient')
		
	ELSEIF ls_annuity_payout_recipient_type_code = 'O' THEN
		SELECT name,
				 address_line1, 
				 address_line2, 
				 city,
				 prov_state_code,
				 postal_code,			 
				 country_code
		INTO   :ls_lump_sum_recipient_name,
				 :ls_original_address_line1, 
				 :ls_original_address_line2,
				 :ls_original_city,
				 :ls_original_prov_state_code,
				 :ls_original_postal_code,
				 :ls_original_country_code
		FROM   PROVIDER
		WHERE  provider_no = :ll_annuity_payout_recipient_no
		USING SQLCA;
		SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT address_line1,address_line2,city,prov_state_code,country FROM PROVIDER...','wf_set_lump_sum_recipient')
		
	ELSE
		RETURN -1
	END IF
	
	ls_address_line1 = ls_original_address_line1
	ls_address_line2 = ls_original_address_line2
	
	IF ls_original_prov_state_code <> '' THEN
		ls_address_line3 = ls_original_city +' '+ ls_original_prov_state_code +' '+ ls_original_postal_code
	ELSE
		ls_address_line3 = ls_original_city +' '+ ls_original_postal_code
	END IF
	
	ls_address_line4 = ls_original_country_code
	
	ls_address_array[1] = Trim(ls_address_line1)
	ls_address_array[2] = Trim(ls_address_line2)
	ls_address_array[3] = Trim(ls_address_line3)
	ls_address_array[4] = Trim(ls_address_line4)
	
	li_upper = 4
	FOR li_outer_counter = 1 TO li_upper
		IF ls_address_array[li_outer_counter] = '' THEN
			FOR li_inner_counter = 1 TO li_upper - 1
				
				IF li_inner_counter < li_outer_counter THEN
					ls_hold_array[li_inner_counter] = ls_address_array[li_inner_counter]
				ELSE
					ls_hold_array[li_inner_counter] = ls_address_array[li_inner_counter+1]
				END IF
			NEXT
			
			ls_address_array = ls_null_array
			ls_address_array = ls_hold_array
			ls_hold_array = ls_null_array
			
			li_upper = li_upper - 1
		END IF
	NEXT
	
	li_upper = UpperBound(ls_address_array)
	
	IF li_upper = 1 THEN
		// must have at least two address lines
		// error
		IF ls_annuity_payout_recipient_type_code = 'I' THEN
			ls_annuity_payout_recipient_type_desc = 'individual'
		ELSE
			ls_annuity_payout_recipient_type_desc = 'other provider'
		END IF
		
		as_error_message = 'An annuity payout recipient for this payout has less than two address lines:'&
		              +'~r~n~t recipient_no = ' + String(ll_annuity_payout_recipient_no) &
						  +'~r~n~t recipient type = ' + ls_annuity_payout_recipient_type_desc &
						  +'~r~nPlease contact the HELPDESK.'
		RETURN -1
		
	ELSEIF li_upper = 2 THEN
		ls_address_array[3] = ''
		ls_address_array[4] = ''
		
	ELSEIF li_upper = 3 THEN
		ls_address_array[4] = ''
		
	END IF
	
	ids_non_whscc_recipients.SetItem(li_datastore_counter,'new_name_on_cheque',ls_lump_sum_recipient_name)
	ids_non_whscc_recipients.SetItem(li_datastore_counter,'new_address_line1',ls_address_array[1])
	ids_non_whscc_recipients.SetItem(li_datastore_counter,'new_address_line2',ls_address_array[2])
	ids_non_whscc_recipients.SetItem(li_datastore_counter,'new_address_line3',ls_address_array[3])
	ids_non_whscc_recipients.SetItem(li_datastore_counter,'new_address_line4',ls_address_array[4])

NEXT

RETURN 0
end function

public subroutine wf_set_address_line_display_only (integer ai_recipient_row);IF idw_payout_recipients.GetItemString(ai_recipient_row,'enable_address_line1_flag') = 'N' THEN
	idw_payout_recipients.Object.address_line1.Edit.DisplayOnly = 'Yes'
ELSE
	idw_payout_recipients.Object.address_line1.Edit.DisplayOnly = 'No'
END IF

IF idw_payout_recipients.GetItemString(ai_recipient_row,'enable_address_line2_flag') = 'N' THEN
	idw_payout_recipients.Object.address_line2.Edit.DisplayOnly = 'Yes'
ELSE
	idw_payout_recipients.Object.address_line2.Edit.DisplayOnly = 'No'
END IF

IF idw_payout_recipients.GetItemString(ai_recipient_row,'enable_address_line3_flag') = 'N' THEN
	idw_payout_recipients.Object.address_line3.Edit.DisplayOnly = 'Yes'
ELSE
	idw_payout_recipients.Object.address_line3.Edit.DisplayOnly = 'No'
END IF

IF idw_payout_recipients.GetItemString(ai_recipient_row,'enable_address_line4_flag') = 'N' THEN
	idw_payout_recipients.Object.address_line4.Edit.DisplayOnly = 'Yes'
ELSE
	idw_payout_recipients.Object.address_line4.Edit.DisplayOnly = 'No'
END IF

IF idw_payout_recipients.GetItemString(ai_recipient_row,'enable_address_line5_flag') = 'N' THEN
	idw_payout_recipients.Object.address_line5.Edit.DisplayOnly = 'Yes'
ELSE
	idw_payout_recipients.Object.address_line5.Edit.DisplayOnly = 'No'
END IF


end subroutine

public function boolean wf_determine_total_writeoff ();BOOLEAN     lb_total_writeoff
DECIMAL     ldec_writeoff, ldec_net_less_writeoff


SELECT writeoff_amount,
       net_annuity_payout_amount - writeoff_amount
INTO   :ldec_writeoff,
       :ldec_net_less_writeoff
FROM   ANNUITY_PAYOUT
WHERE  annuity_payout_no = :il_annuity_payout_no
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT Count(*) FROM ANNUITY_PAYOUT_PARTICIPANT...','wf_determine_total_writeoff')

IF ldec_writeoff <> 0.00 THEN
	IF ldec_net_less_writeoff = 0 THEN
		// if the write-off amount <> $0, and net amount = write-off amount, then total write-off
		lb_total_writeoff = TRUE
	ELSE
		// if the write-off amount <> $0, and net amount <> write-off amount, then partial write-off, not total
		lb_total_writeoff = FALSE
	END IF
ELSE
	// if the write-off amount = $0, then no write-off
	lb_total_writeoff = FALSE
END IF

RETURN lb_total_writeoff
end function

public function integer wf_confirm_overpayment_recovery ();
// updates records in ANNUITY_PAYOUT, ANNUITY_PAYOUT_CLAIM_SUMMARY
// inserts records into ANNUITY_PAYOUT_CLAIM_DETAIL, ANNUITY_PAYOUT_TXN_DETAIL, ANNUITY_PAYOUT_RECIPIENT for both benefit holder & dependants
// may insert records into OVERPAYMENT_DETAIL, ANNUITY_PAYOUT_OVERPAYMENT_XREF & update OVERPAYMENT_BALANCE

DECLARE lp_confirm_annuity_payout_overpayment_recovery PROCEDURE FOR p_confirm_annuity_payout_overpayment_recovery
	@annuity_payout_no = :il_annuity_payout_no, @mode = 'INSERT'
USING SQLCA;

EXECUTE lp_confirm_annuity_payout_overpayment_recovery;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: EXECUTE lp_confirm_annuity_payout_overpayment_recovery','wf_confirm_overpayment_recovery')


RETURN 0
end function

public function boolean wf_display_recipients_tab ();BOOLEAN lb_recipient_tab_visible
INTEGER li_count

// if there are no annuity payout recipients, then do not recipients tab
// otherwise, display the tab
SELECT COUNT(*)
INTO   :li_count
FROM   ANNUITY_PAYOUT_RECIPIENT
WHERE  annuity_payout_no = :il_annuity_payout_no
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT COUNT(*) FROM ANNUITY_PAYOUT_RECIPIENT...','wf_display_recipients_tab')

IF li_count > 0 THEN
	lb_recipient_tab_visible = TRUE	
ELSE
	lb_recipient_tab_visible = FALSE
END IF

tab_payout.tabpage_payout_recipients.Visible = lb_recipient_tab_visible

RETURN lb_recipient_tab_visible
end function

public function boolean wf_display_contracts_tab ();BOOLEAN lb_contract_tab_visible
INTEGER li_count

// if there are no annuity payout recipients, then do not recipients tab
// otherwise, display the tab
SELECT COUNT(*)
INTO   :li_count
FROM   ANNUITY_CONTRACT
WHERE  annuity_payout_no = :il_annuity_payout_no
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT COUNT(*) FROM ANNUITY_PAYOUT_RECIPIENT...','wf_display_recipients_tab')

IF li_count > 0 THEN
	lb_contract_tab_visible = TRUE	
ELSE
	lb_contract_tab_visible = FALSE
END IF

tab_payout.tabpage_contract_and_beneficiaries.Visible = lb_contract_tab_visible

RETURN lb_contract_tab_visible
end function

public subroutine wf_reverse_confirm_op_recovery ();INTEGER     li_annuity_overpayment_xref_count

// reverse inserts/updates to OVERPAYMENT_DETAIL, OVERPAYMENT_BALANCE, ANNUITY_PAYOUT_OVERPAYMENT_XREF


// if the status/reason is not validated, an app error
inv_common_annuity.nf_validate_annuity_status(il_annuity_payout_no,'X','')


SELECT Count(*)
INTO   :li_annuity_overpayment_xref_count
FROM   ANNUITY_PAYOUT_OVERPAYMENT_XREF
WHERE  annuity_payout_no = :il_annuity_payout_no
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT Count(*) FROM ANNUITY_PAYOUT_OVERPAYMENT_XREF...','wf_cancel_confirm_OP_recovery')


IF li_annuity_overpayment_xref_count > 0 THEN
			
	DECLARE lp_cancel_overpayment_recovery PROCEDURE FOR p_cancel_overpayment_recovery
		@annuity_payout_no = :il_annuity_payout_no
	USING SQLCA;
	
	EXECUTE lp_cancel_overpayment_recovery;
	SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: EXECUTE lp_cancel_overpayment_recovery','wf_cancel_confirm_OP_recovery')

END IF
end subroutine

public subroutine wf_reverse_request_payout ();
// insert PAYMENT, UNAPPLIED_CLAIM_TXN, update ANNUITY_PAYOUT_TXN_DETAIL


// if the status/reason is not validated, an app error
inv_common_annuity.nf_validate_annuity_status(il_annuity_payout_no,'X','')

DECLARE lp_cancel_annuity_payout PROCEDURE FOR p_cancel_annuity_payout
	@annuity_payout_no = :il_annuity_payout_no
USING SQLCA;

EXECUTE lp_cancel_annuity_payout;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: EXECUTE lp_cancel_annuity_payout','wf_cancel_cap_checklist')

end subroutine

public subroutine wf_annuity_payout_comment (string as_payout);DATETIME   ldtm_current_date
STRING     ls_comment, ls_trimmed_comment, ls_clean_comment
S_WINDOW_MESSAGE lstr_message

// allows users to enter a comment when the payout is concluding

// open comment window
IF ib_required_user_payout_comment THEN
	lstr_message.as_stringparm[1] = is_annuity_payout_reason_message+'~r~nYou must enter a comment.'
	lstr_message.al_doubleparm[1] = 5  // if you are going to enter an additional comment, it must be at least 5 chars
	lstr_message.as_mode = 'REQUIRED'
ELSEIF ib_optional_user_payout_comment THEN
	lstr_message.as_stringparm[1] = is_annuity_payout_reason_message+'~r~nPlease enter a comment.'
	lstr_message.al_doubleparm[1] = 5  // if you are going to enter an additional comment, it must be at least 5 chars
	lstr_message.as_mode = 'OPTIONAL'
ELSE
	MessageBox('','An error has occurred while trying to open the annuity comment window. Please report this error to the HELPDESK.',Exclamation!)
	RETURN
END IF
	

OpenWithParm(w_annuity_comment,lstr_message)


ls_comment = Message.StringParm
ls_trimmed_comment = trim(ls_comment)
ls_clean_comment = f_clean_string_1(ls_trimmed_comment)

SQLCA.nf_begin_transaction()

IF trim(ls_comment) <> '' THEN
	IF is_checklist_type_code = 'PAA' THEN
		UPDATE ANNUITY_PAYOUT
		SET    payout_comment    = payout_comment + ' ' + :ls_clean_comment
		WHERE  annuity_payout_no = :il_annuity_payout_no
		USING SQLCA;
		SQLCA.nf_handle_error('w_prepare_annuity_account','UPDATE ANNUITY_PAYOUT...(1)','wf_annuity_payout_comment')
	ELSE
		
		IF as_payout = 'yes' THEN
			
			ldtm_current_date = f_server_datetime()
			
			UPDATE ANNUITY_PAYOUT
			SET    payout_comment              = payout_comment + ' ' + :ls_clean_comment,
					 payout_requested_by_user_id = :vgst_user_profile.user_id,
					 payout_requested_date       = :ldtm_current_date
			WHERE  annuity_payout_no = :il_annuity_payout_no
			USING SQLCA;
			SQLCA.nf_handle_error('w_prepare_annuity_account','UPDATE ANNUITY_PAYOUT...(2)','wf_annuity_payout_comment')
		ELSE
			// no payout
			
			UPDATE ANNUITY_PAYOUT
			SET    payout_comment = payout_comment + ' ' + :ls_clean_comment
			WHERE  annuity_payout_no = :il_annuity_payout_no
			USING SQLCA;
			SQLCA.nf_handle_error('w_prepare_annuity_account','UPDATE ANNUITY_PAYOUT...(3)','wf_annuity_payout_comment')
		END IF
	END IF
	
ELSE
	// even though the user has not entered further comment,
	// still must update the 'payout request' columns in ANNUITY_PAYOUT
	IF is_checklist_type_code <> 'PAA' THEN
		IF as_payout = 'yes' THEN
			IF trim(ls_comment) <> '' THEN
				ls_clean_comment = ' ' + ls_clean_comment
			END IF
			
			ldtm_current_date = f_server_datetime()
			
			UPDATE ANNUITY_PAYOUT
			SET    payout_comment              = payout_comment + :ls_clean_comment,
					 payout_requested_by_user_id = :vgst_user_profile.user_id,
					 payout_requested_date       = :ldtm_current_date
			WHERE  annuity_payout_no = :il_annuity_payout_no
			USING SQLCA;
			SQLCA.nf_handle_error('w_prepare_annuity_account','UPDATE ANNUITY_PAYOUT...(4)','wf_annuity_payout_comment')
			
		END IF
	END IF
END IF

SQLCA.nf_commit_transaction()

//reset variable
ib_optional_user_payout_comment = FALSE
ib_required_user_payout_comment = FALSE
is_annuity_payout_reason_message = ''
end subroutine

public function boolean wf_display_txn_details_tab ();BOOLEAN lb_payout_txn_detail_tab_visible
INTEGER li_count

// if there are no records, then do not display payout txn details tab
// otherwise, display the tab
SELECT COUNT(*)
INTO   :li_count
FROM   ANNUITY_PAYOUT_TXN_DETAIL
WHERE  annuity_payout_no = :il_annuity_payout_no
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT COUNT(*) FROM ANNUITY_PAYOUT_TXN_DETAIL...','wf_display_txn_details_tab')

IF li_count = 0 THEN
	lb_payout_txn_detail_tab_visible = FALSE
ELSE
	lb_payout_txn_detail_tab_visible = TRUE
END IF

tab_payout.tabpage_annuity_payout_txn_detail.Visible = lb_payout_txn_detail_tab_visible

RETURN lb_payout_txn_detail_tab_visible
end function

public function boolean wf_display_payout_summary_tab ();BOOLEAN lb_payout_summary_tab_visible
INTEGER li_count

// if there are no records, then do not display payout txn details tab
// otherwise, display the tab
SELECT COUNT(*)
INTO   :li_count
FROM   ANNUITY_PAYOUT_CLAIM_SUMMARY
WHERE  annuity_payout_no = :il_annuity_payout_no
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT COUNT(*) FROM ANNUITY_PAYOUT_CLAIM_SUMMARY...','wf_display_payout_summary_tab')

IF li_count = 0 THEN
	lb_payout_summary_tab_visible = FALSE
ELSE
	lb_payout_summary_tab_visible = TRUE
END IF

tab_payout.tabpage_payout_summary.Visible = lb_payout_summary_tab_visible

RETURN lb_payout_summary_tab_visible
end function

public function integer wf_orphan_participant (ref string as_orphan_message);BOOLEAN   lb_found
INTEGER   li_orphan_count, li_counter, li_upperbound, li_description_counter
LONG      ll_orphan_recipient_no
STRING    ls_orphan_recipient_type_code, ls_orphan_name, ls_orphan_name_string, ls_orphan_roles, ls_orphan_description
STRING    ls_description_string
STRING    ls_orphan_descriptions[]
U_DS      lds_orphan_payout_recipients

/*

called when the Confirm Overpayment Recovery checklist step in PAA checklist is being completed

  you cannot complete this step if there are any trustees, guardians or estates that do
  not represent an entitled individual, such as injured worker, surviving spouse or dependant

*/

lds_orphan_payout_recipients = Create U_DS
lds_orphan_payout_recipients.DataObject = 'ds_orphan_payout_recipients'
lds_orphan_payout_recipients.SetTransObject(SQLCA)

li_orphan_count = lds_orphan_payout_recipients.Retrieve(il_annuity_payout_no)
SQLCA.nf_handle_error('w_prepare_annuity_account','lds_orphan_payout_recipients.Retrieve','wf_orphan_participant_count')

IF li_orphan_count > 0 THEN
	
	// gather orphan names in loop
	FOR li_counter = 1 TO li_orphan_count
		ls_orphan_recipient_type_code = lds_orphan_payout_recipients.GetItemString(li_counter,'recipient_type_code')
		ls_orphan_description         = lds_orphan_payout_recipients.GetItemString(li_counter,'annuity_role_desc_e')
		ll_orphan_recipient_no        = lds_orphan_payout_recipients.GetItemNumber(li_counter,'recipient_no')
		
		IF ls_orphan_recipient_type_code = 'I' THEN			
			SELECT given_names + ' ' + last_name
			INTO   :ls_orphan_name
			FROM   INDIVIDUAL
			WHERE  individual_no = :ll_orphan_recipient_no
			USING SQLCA;
			SQLCA.nf_handle_error('w_prepare_annuity_account','SELECT given_names,last_name FROM INDIVIDUAL...','wf_orphan_participant_count')
			
		ELSE
			SELECT name
			INTO   :ls_orphan_name
			FROM   PROVIDER
			WHERE  provider_type_code = 'O'
			AND    provider_no        = :ll_orphan_recipient_no
			USING SQLCA;
			SQLCA.nf_handle_error('w_prepare_annuity_account','SELECT name FROM PROVIDER...','wf_orphan_participant_count')
			
		END IF
		
		ls_orphan_name = ls_orphan_name + ' (' + ls_orphan_description + ')'
		
		IF li_counter = 1 THEN
			ls_orphan_name_string = '~r~n~t' + ls_orphan_name
		ELSE
			ls_orphan_name_string = ls_orphan_name_string + '~r~n~t' + ls_orphan_name
		END IF
		
		// gather descriptions
		li_upperbound = UpperBound(ls_orphan_descriptions)
		FOR li_description_counter = 1 TO li_upperbound
			IF ls_orphan_descriptions[li_description_counter] = ls_orphan_description THEN
				lb_found = TRUE
				EXIT
			END IF			
		NEXT
		
		IF lb_found THEN
			// do not add to description array if it already exists
			// reset variable
			lb_found = FALSE
		ELSE
			// add new description
			ls_orphan_descriptions[li_upperbound+1] = ls_orphan_description
		END IF
		
	NEXT
	
	// build error message
	li_upperbound = UpperBound(ls_orphan_descriptions)
	
	IF li_upperbound = 1 THEN
		as_orphan_message = 'There is an annuity participant ('+ls_orphan_descriptions[1]+') that does not represent another annuity participant in this annuity payout. '&
								+ 'This participant must be deleted before the Confirm Overpayment Recovery checklist step can be completed. '&
								+ 'The annuity payout participant to be deleted is:'&
								+ '~r~n' + ls_orphan_name_string
	ELSEIF li_upperbound > 1 THEN
		
		FOR li_description_counter = 1 TO li_upperbound
			IF li_description_counter = 1 THEN
				ls_description_string = ls_orphan_descriptions[1]
			ELSE
				ls_description_string = ls_description_string + ', ' + ls_orphan_descriptions[li_description_counter]
			END IF
		NEXT
			
		as_orphan_message = 'There are annuity participants ('+ls_description_string+') that do not represent another annuity participant in this annuity payout. '&
								+ 'They must be deleted before the Confirm Overpayment Recovery checklist step can be completed. '&
								+ 'The annuity payout participants to be deleted are:'&
								+ '~r~n' + ls_orphan_name_string
	END IF
	
END IF


RETURN li_orphan_count

end function

public function integer wf_determine_participant_garnishment ();INTEGER      li_rows
U_DS         lds_garnished_annuity_participants


lds_garnished_annuity_participants = Create U_DS
lds_garnished_annuity_participants.DataObject = 'ds_garnished_annuity_participants'
lds_garnished_annuity_participants.SetTransObject(SQLCA)

li_rows = lds_garnished_annuity_participants.Retrieve(il_annuity_payout_no)
SQLCA.nf_handle_error('w_prepare_annuity_account','lds_garnished_annuity_participants.retrieve','wf_determine_participant_garnishment')

RETURN li_rows
end function

public function integer wf_validate_writeoff_flag ();INTEGER     li_benefit_holder_not_written_off_count, li_dependant_not_written_off_count, li_dependant_with_estate_written_off_count
STRING      ls_claim_role_desc

/*

1.230	The annuity benefits of the individual who is eligible for the annuity benefits must be written off, if all of the following are true:
o	the individual is deceased
o	there are no dependents 
o	the Prepare Annuity Payout, Confirm Overpayment Recovery checklist step is completed.
(Refer to Rationale)

*/


SELECT Count(*)
INTO   :li_benefit_holder_not_written_off_count
FROM   ANNUITY_PAYOUT_PARTICIPANT a
JOIN   Annuity_Role               b ON a.annuity_role_code = b.annuity_role_code
JOIN   INDIVIDUAL                 c ON a.recipient_no      = c.individual_no
WHERE  a.annuity_payout_no            = :il_annuity_payout_no
AND    a.annuity_payout_writeoff_flag = 'N'
AND    b.annuity_eligibility_flag     = 'Y'
AND    c.death_date                   IS NOT NULL
AND NOT EXISTS ( SELECT *
                 FROM   ANNUITY_PAYOUT_PARTICIPANT d
                 JOIN   Annuity_Role               e ON d.annuity_role_code = e.annuity_role_code
                 WHERE  d.annuity_payout_no        = :il_annuity_payout_no
                 AND    e.annuity_eligibility_flag = 'N'
                 AND    e.annuity_entitlement_flag = 'Y' )
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT Count(*) FROM ANNUITY_PARTICIPANT,Annuity_Role,INDIVIDUAL...(1)','wf_validate_writeoff_flag')

IF li_benefit_holder_not_written_off_count > 0 THEN
	IF is_claim_role_code = 'C' THEN
		ls_claim_role_desc = 'injured worker'
	ELSE
		ls_claim_role_desc = 'surviving spouse'
	END IF
		
	MessageBox('Annuity Participant Error - BR 1.230','The annuity benefits of the '+ls_claim_role_desc+' must be written off, if all of the following are true:' &
                                              +'~r~n	the '+ls_claim_role_desc+' is deceased' &
                                              +'~r~n	there are no dependants' &
                                              +'~r~n	the Prepare Annuity Payout, Confirm Overpayment Recovery checklist step is completed.',StopSign!)
	RETURN -1
END IF

/*

1.235	The annuity benefits of a dependent who is entitled to annuity benefits must be written off, if all of the following are true
o	dependent is deceased 
o	s/he does not have a representative in the annuity role of estate
o	the Prepare Annuity Payout, Confirm Overpayment Recovery checklist step is completed.
(Refer to Rationale)

*/

SELECT Count(*)
INTO   :li_dependant_not_written_off_count
FROM   ANNUITY_PAYOUT_PARTICIPANT a
JOIN   Annuity_Role               b ON a.annuity_role_code = b.annuity_role_code
JOIN   INDIVIDUAL                 c ON a.recipient_no      = c.individual_no
WHERE  a.annuity_payout_no            = :il_annuity_payout_no
AND    a.annuity_payout_writeoff_flag = 'N'
AND    b.annuity_eligibility_flag     = 'N'
AND    b.annuity_entitlement_flag     = 'Y'
AND    c.death_date                   IS NOT NULL
AND NOT EXISTS ( SELECT *
                 FROM   ANNUITY_PAYOUT_PARTICIPANT d
                 WHERE  d.recipient_no        = a.represented_by_recipient_no
                 AND    d.recipient_type_code = a.represented_by_recipient_type_code
                 AND    d.annuity_payout_no   = a.annuity_payout_no
                 AND    d.annuity_role_code   = '18')           //estate
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT Count(*) FROM ANNUITY_PARTICIPANT,Annuity_Role,INDIVIDUAL...(2)','wf_validate_writeoff_flag')


IF li_dependant_not_written_off_count > 0 THEN
	MessageBox('Annuity Participant Error - BR 1.235','The annuity benefits of a dependant who is entitled to annuity benefits must be written off, if all of the following are true:' &
                                              +'~r~n	dependant is deceased' &
                                              +'~r~n	s/he does not have a representative in the annuity role of estate' &
                                              +'~r~n	the Prepare Annuity Payout, Confirm Overpayment Recovery checklist step is completed.',StopSign!)
	RETURN -1
END IF


end function

public subroutine wf_determine_writeoff_on_last_payout ();STRING   ls_annuity_writeoff_flag


/*

3.83	The annuity payout status reason must be ‘Different Payout Participants’, if, in the most recent previous annuity payout 
      that was completed with payout, the following are true:
		·	the annuity benefits of  the individual who is eligible for the annuity benefits were not written off
		·	it has been indicated that a different set of annuity participants should have been paid

		Rationale
		
		If the previous annuity payout that was completed with payout involved the injured worker's or surviving spouse's annuity benefits 
		having been written off, it is possible that the eligible individual may now require payout, or if deceased, some dependents now 
		require payout. Therefore, a message will not be provided, if the eligible individual's annuity benefits had been written off.  
		
		If the eligible individual's annuity benefits were not written off,  a message will always be provided. If a dependent's annuity benefits 
		were written off, the message will be provided to allow a different payout participant list, in the case where another dependent came forward 
		since the initial payout. If there are additional dependents, this is a different payout participants list, so these dependents can't be added.

*/

IF il_previously_completed_payout_no > 0 THEN
	SELECT a.annuity_payout_writeoff_flag
	INTO   :ls_annuity_writeoff_flag
	FROM   ANNUITY_PAYOUT_PARTICIPANT a
	JOIN   Annuity_Role        b ON a.annuity_role_code = b.annuity_role_code
	WHERE  a.annuity_payout_no = :il_previously_completed_payout_no
	AND    b.annuity_eligibility_flag = 'Y'
	AND    b.annuity_entitlement_flag = 'Y'
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT annuity_writeoff_flag FROM ANNUITY_PARTICIPANT...', 'wf_determine_last_payout')
	
	
	// had previous payout, but BH was written off, so allowed to add new participants
	IF ls_annuity_writeoff_flag = 'Y' THEN
		ib_can_change_participants = TRUE
	ELSE
		ib_can_change_participants = FALSE
	END IF
ELSE
	// if there is no previously completed annuity payout,
	// then you must allow users to add new annuity payout participants
	ib_can_change_participants = TRUE
END IF
end subroutine

public function boolean wf_enable_manual_payout_btn ();BOOLEAN    lb_payout_can_be_reversed
INTEGER    li_rtn, li_cheque_count
STRING     ls_annuity_eligibility_confirmed_flag, ls_checklist_step_status_code


// if annuity eligibility has not been confirmed for this annuity payout, then button is not available
ls_annuity_eligibility_confirmed_flag = dw_payout_accounts.GetItemString(dw_payout_accounts.GetRow(),'annuity_eligibility_confirmed_flag')

IF ls_annuity_eligibility_confirmed_flag = 'N' THEN
	RETURN FALSE
END IF


/* 
verify that user is not trying to manually payout after the CAP request payout step 
has been completed, and cheque number entered/or payment processed
*/
IF is_checklist_type_code = 'CAPAP' OR is_checklist_type_code = 'CAPLS' THEN
	lb_payout_can_be_reversed = inv_common_annuity.nf_determine_request_payout_status(il_annuity_payout_no)
	IF lb_payout_can_be_reversed = TRUE THEN
		RETURN TRUE
	ELSE
		RETURN FALSE
	END IF
ELSE
	RETURN TRUE
END IF
end function

public subroutine wf_enable_annuity_admin_region ();STRING     ls_annuity_eligibility_confirmed_flag

ls_annuity_eligibility_confirmed_flag = dw_payout_accounts.GetItemString(dw_payout_accounts.GetRow(),'annuity_eligibility_confirmed_flag')

IF ls_annuity_eligibility_confirmed_flag = 'N' THEN
	IF is_claim_role_code = 'C' THEN
		dw_payout_region.Enabled = TRUE
		cb_save_region.Enabled = TRUE
	ELSE
		dw_payout_region.Enabled = FALSE
		cb_save_region.Enabled = FALSE
	END IF
ELSE
	dw_payout_region.Enabled = FALSE
	cb_save_region.Enabled = FALSE
END IF



end subroutine

public function integer wf_subledger_payout_match (ref decimal adec_sub_ledger, ref decimal adec_payout);IF is_claim_role_code = 'C' THEN
	
	SELECT SUM(a.net_payment_amount)
	INTO   :adec_sub_ledger
	FROM   PAYMENT a
	WHERE  a.payment_type_code   = '97'
	AND EXISTS ( SELECT *
                FROM   APPLIED_CLAIM_TXN b
                JOIN   ANNUITY_ACCOUNT   c ON b.recipient_no = c.individual_no
                JOIN   ANNUITY_PAYOUT    d ON c.annuity_account_no = d.annuity_account_no
                JOIN   CLAIM_PARTICIPANT e ON b.recipient_no = e.individual_no
                                          AND b.claim_no = e.claim_no
                                          AND c.claim_role_code = e.claim_role_code
                WHERE  b.claim_no   = a.claim_no
                AND    b.payment_no = a.payment_no
                AND    b.recipient_type_code = 'I'
                AND    d.annuity_payout_no   = :il_annuity_payout_no )
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT SUM(net_payment_amount) FROM PAYMENT, APPLIED_CLAIM_TXN, ANNUITY_ACCOUNT, ANNUITY_PAYOUT...','wf_subledger_payout_match(1)')
	
ELSE
	// surviving spouse
	SELECT SUM(a.net_payment_amount)
	INTO   :adec_sub_ledger
	FROM   PAYMENT a
	WHERE  a.payment_type_code   = '97'
	AND EXISTS ( SELECT *
                FROM   APPLIED_CLAIM_TXN b
                JOIN   ANNUITY_ACCOUNT   c ON b.recipient_no = c.individual_no
					                           AND b.claim_no     = c.claim_no
                JOIN   ANNUITY_PAYOUT    d ON c.annuity_account_no = d.annuity_account_no
                JOIN   CLAIM_PARTICIPANT e ON b.recipient_no = e.individual_no
                                          AND b.claim_no = e.claim_no
                                          AND c.claim_role_code = e.claim_role_code
                WHERE  b.claim_no   = a.claim_no
                AND    b.payment_no = a.payment_no
                AND    b.recipient_type_code = 'I'
                AND    d.annuity_payout_no   = :il_annuity_payout_no )
	USING SQLCA;	
	SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT SUM(net_payment_amount) FROM PAYMENT, APPLIED_CLAIM_TXN, ANNUITY_ACCOUNT, ANNUITY_PAYOUT...','wf_subledger_payout_match(2)')
END IF

SELECT sub_total_annuity_payout_amount
INTO   :adec_payout
FROM   ANNUITY_PAYOUT
WHERE  annuity_payout_no = :il_annuity_payout_no
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT sub_total_annuity_payout_amount FROM ANNUITY_PAYOUT...','wf_subledger_payout_match')

IF adec_sub_ledger <> adec_payout THEN
	RETURN -1
END IF

RETURN 0
end function

public subroutine wf_report ();S_WINDOW_MESSAGE          lstr_message

lstr_message.al_doubleparm[1] = 0                     //claim_no
lstr_message.al_doubleparm[2] = il_individual_no
lstr_message.al_doubleparm[3] = il_annuity_payout_no

//call funtion to set up instance variables ib_payout_with_dependants, and ii_dependant_txn_count 
wf_payout_has_dependants()

//set a value in the stingparm, so report window knows which datawindow to use for claim txn details
IF ib_payout_with_dependants AND ii_dependant_txn_count > 0 THEN
	lstr_message.as_stringparm[1] = 'has dependants'
ELSE
	lstr_message.as_stringparm[1] = 'no dependants'
END IF

OpenWithParm(w_annuity_payout_report_viewer, lstr_message, iw_win)
end subroutine

public subroutine wf_print ();dw_payout_accounts.Object.DataWindow.Print.Orientation = 1
dw_payout_accounts.Print()
end subroutine

public function integer wf_no_payout_subledger_warning (string as_annuity_payout_status_reason_code);DECIMAL     ldec_subledger_balance
INTEGER     li_msg_rtn
STRING      ls_annuity_payout_status_reason_desc_e, ls_message, ls_reduction_reason_code, ls_reduction_reason_desc



// get no-payout reason description
SELECT annuity_payout_status_reason_desc_e
INTO   :ls_annuity_payout_status_reason_desc_e
FROM   Annuity_Payout_Status_Reason
WHERE  annuity_payout_status_reason_code = :as_annuity_payout_status_reason_code
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_payout','embedded SQL: SELECT annuity_payout_status_reason_desc_e FROM Annuity_Payout_Status_Reaon...','wf_no_payout_subledger_warning')


IF is_claim_role_code = 'C' THEN
	inv_common_annuity.nf_injured_worker_sub_ledger_balance(il_individual_no,ldec_subledger_balance)
ELSE
	inv_common_annuity.nf_surviving_spouse_sub_ledger_balance(il_individual_no,il_annuity_account_claim_no,ldec_subledger_balance)
END IF

IF ldec_subledger_balance <> 0 THEN
	IF as_annuity_payout_status_reason_code = 'RCA' THEN
		// if claim annuity benefits have been reduced
		// there is a sub-reason: future date, ineligibility, or reduction of at least one claims benefits
		// only future date and ineligibility need further text added.
		ls_reduction_reason_code = ids_populate_initial_annuity_payout_balances.GetItemString(1,'annuity_payout_reduction_reason')
		IF ls_reduction_reason_code = 'future' THEN
			ls_reduction_reason_desc = ' due to annuity end date in future. '
		ELSEIF ls_reduction_reason_code = 'ineligible' THEN
			ls_reduction_reason_desc = ' due to ineligibility. '
		ELSEIF ls_reduction_reason_code = 'reduction' OR ls_reduction_reason_code = 'previous' THEN
			ls_reduction_reason_desc = '. '			
		END IF
		
		ls_message = 'This individual has a non-zero sub-ledger balance. '&
						+'This annuity payout will be Completed No Payout with reason, '+ls_annuity_payout_status_reason_desc_e + ls_reduction_reason_desc &
						+'~r~n~r~n'&
						+'This payout should remain on the payout list until the annuity benefits have either been paid out manually '&
						+'or the annuity sub-ledger has been reduced to $0, which ever is required depending on the situation. '&
						+'~r~n~r~n'&
						+'Only then should the annuity payout be completed.'		
	ELSE	
		ls_message = 'This individual has a non-zero sub-ledger balance. '&
						+'This annuity payout will be Completed No Payout with reason, '+ls_annuity_payout_status_reason_desc_e+'. '&
						+'~r~n~r~n'&
						+'This payout should remain on the payout list until the annuity benefits have either been paid out manually '&
						+'or the annuity sub-ledger has been reduced to $0, which ever is required depending on the situation. '&
						+'~r~n~r~n'&
						+'Only then should the annuity payout be completed.'
	END IF
	
	IF as_annuity_payout_status_reason_code = 'NEA' OR as_annuity_payout_status_reason_code = 'NBE' THEN
		// do not add note to message box if NEA or NBE
	ELSE
		ls_message = ls_message + '~r~n~r~nNote:'&
						+'~r~nIf you complete the payout without dealing with the non-zero annuity sub-ledger, the individual will automatically '&
						+'get placed back on the annuity payout list.'
	END IF
	
	ls_message = ls_message + '~r~n~r~nDo you want to Complete the payout?'
	
	li_msg_rtn = MessageBox('BR 3.00 - Annuity Sub-Ledger Balance not equal $0.00',ls_message,Question!,YesNo!,2)
	IF li_msg_rtn = 1	 THEN
		// subledger <> 0, user has been warned, but wants to continue with completion of payout
		ib_non_zero_subledger_warning = TRUE
		RETURN 0
	ELSE
		// subledger <> 0, user has been warned and does not want to continue with completion of payout
		RETURN -1
	END IF
ELSE
	// subledger = 0, so no warning, continue with completion of payout
	RETURN 0
END IF
end function

public subroutine wf_build_annuity_payout_message (string as_annuity_payout_status_code, string as_annuity_payout_status_reason_code);INTEGER  li_rows
STRING   ls_annuity_payout_status_desc, ls_annuity_payout_status_reason_desc
U_DS     lds_annuity_status


lds_annuity_status = Create U_DS
lds_annuity_status.DataObject = 'ds_annuity_status'
lds_annuity_status.SetTransObject(SQLCA)
li_rows = lds_annuity_status.Retrieve(as_annuity_payout_status_code,as_annuity_payout_status_reason_code)
SQLCA.nf_handle_error('w_prepare_annuity_account','lds_annuity_status.Retrieve','wf_build_annuity_payout_message')

IF li_rows <> 1 THEN
	MessageBox('Annuity Payout Reason Error','More than one payout reason was returned. Call the HELPDESK.',Exclamation!)
	RETURN
END IF

ls_annuity_payout_status_desc = lds_annuity_status.GetItemString(1,'annuity_payout_status_desc_e')
ls_annuity_payout_status_reason_desc = lds_annuity_status.GetItemString(1,'annuity_payout_status_reason_desc_e')


IF ib_different_participant_list AND as_annuity_payout_status_reason_code <> 'DPP' THEN
	ib_different_participant_list = FALSE
	is_annuity_payout_reason_message = 'You have chosen to complete this annuity payout manually because the payout needs a different participant list, ' &
	                                  +'but the payout will be completed with a different reason: '&
												 +'~r~n~r~n'
END IF


is_annuity_payout_reason_message = is_annuity_payout_reason_message &
	                             + 'This annuity payout will be completed as' &
   	                          + '~r~n~t' + ls_annuity_payout_status_desc &
										  + '~r~n~t' + ls_annuity_payout_status_reason_desc + '.' 

end subroutine

public function integer wf_post_cae_no_payout ();BOOLEAN    lb_no_payout_warning
DATETIME   ldtm_annuity_start_date, ldtm_annuity_end_date, ldtm_current_date
INTEGER    li_count, li_rtn, li_msg_rtn, li_rows
STRING     ls_annuity_eligibility_status_code, ls_annuity_payout_status_code, ls_annuity_payout_status_reason_code

// after the CAE checklist is complete, determine if there will be no payout
// this allows warning of user if there's a non-zero subledger balance - wf_no_payout_subledger_warning call at end
// also the status is saved as instance var so that it can be used in ue_itemchangeaccepted to update ANNUITY_PAYOUT

// determine if individual was found not eligible, if so set to D/NEA 
wf_determine_eligibility(il_annuity_eligibility_no,ls_annuity_eligibility_status_code,ldtm_annuity_start_date,ldtm_annuity_end_date)
IF ls_annuity_eligibility_status_code = 'I' THEN
	// not eligible
	ib_no_payout = TRUE
	ib_not_eligible = TRUE
	ls_annuity_payout_status_reason_code = 'NEA'
	lb_no_payout_warning = TRUE
END IF


IF ls_annuity_eligibility_status_code = 'A' THEN
	
	// test that, if eligible, the annuity end date is not in future - otherwise, no payout
	ldtm_current_date = f_server_datetime()
	
	IF DaysAfter(Date(ldtm_current_date), Date(ldtm_annuity_end_date)) > 0 THEN
		// annuity end date is now in the future
		ib_no_payout = TRUE
		ib_future_end_date = TRUE
		ls_annuity_payout_status_reason_code = 'AEE'
		// do not warn the user to zero out the subledger if the annuity ends in future
		lb_no_payout_warning = FALSE
	END IF

	
	// test that, if eligible, the individual had post qualification entitlement - if none, then no payout
	SELECT Count(*)
	INTO   :li_count
	FROM   BENEFIT_ENTITLEMENT
	WHERE  annuity_account_no                          = :il_annuity_account_no
	AND    benefit_entitlement_from_date              >= :ldtm_annuity_start_date
	AND    DateAdd(dd,-1,benefit_entitlement_to_date) <= :ldtm_annuity_end_date
	AND    deleted_flag                                = 'N'
	AND    active_flag                                 = 'Y'
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT Count(*) FROM BENEFIT_ENTITLEMENT...','wf_post_cae_no_payout')
	
	IF li_count = 0 THEN
		// no post qualification entitlement
		ib_no_payout = TRUE
		ib_no_benefit_entitlement = TRUE
		ls_annuity_payout_status_reason_code = 'NBE'
		lb_no_payout_warning = TRUE
	END IF
END IF


IF lb_no_payout_warning THEN
	li_rtn = wf_no_payout_subledger_warning(ls_annuity_payout_status_reason_code)
	RETURN li_rtn
ELSE
	RETURN 0
END IF

end function

public subroutine wf_initial_annuity_payout_population ();DECIMAL             ldec_total_annuity_benefit_amount, ldec_previous_annuity_payout_amount
DECIMAL             ldec_AP_total_annuity_benefit_amount, ldec_AP_prev_annuity_payout_amount
INTEGER             li_rows, li_rtn, li_inserted_row, li_summary_counter
LONG                ll_claim_no, ll_annuity_payout_no, ll_annuity_account_no, ll_annuity_calc_no
U_DS                lds_annuity_payout_claim_summary



// inserts records into ANNUITY_PAYOUT_CLAIM_SUMMARY for benefit holder
lds_annuity_payout_claim_summary = Create U_DS
lds_annuity_payout_claim_summary.DataObject = 'ds_annuity_payout_claim_summary'
lds_annuity_payout_claim_summary.SetTransObject(SQLCA)

li_rows = ids_populate_initial_annuity_payout_balances.RowCount()

FOR li_summary_counter = 1 TO li_rows
	li_inserted_row = lds_annuity_payout_claim_summary.InsertRow(0)
	SQLCA.nf_handle_error('w_prepare_annuity_account','lds_annuity_payout_claim_summary.InsertRow','wf_initial_annuity_payout_population')
	
	ll_claim_no = ids_populate_initial_annuity_payout_balances.GetItemNumber(li_summary_counter,'claim_no')
	ll_annuity_payout_no = ids_populate_initial_annuity_payout_balances.GetItemNumber(li_summary_counter,'annuity_payout_no')
	ll_annuity_account_no = ids_populate_initial_annuity_payout_balances.GetItemNumber(li_summary_counter,'annuity_account_no')
	ll_annuity_calc_no = ids_populate_initial_annuity_payout_balances.GetItemNumber(li_summary_counter,'annuity_calc_no')
	ldec_total_annuity_benefit_amount = ids_populate_initial_annuity_payout_balances.GetItemDecimal(li_summary_counter,'total_annuity_benefit_amount')
	ldec_previous_annuity_payout_amount = ids_populate_initial_annuity_payout_balances.GetItemDecimal(li_summary_counter,'previous_annuity_payout_amount')
	
	lds_annuity_payout_claim_summary.SetItem(li_inserted_row,'claim_no',ll_claim_no)
	lds_annuity_payout_claim_summary.SetItem(li_inserted_row,'annuity_payout_no',ll_annuity_payout_no)
	lds_annuity_payout_claim_summary.SetItem(li_inserted_row,'annuity_account_no',ll_annuity_account_no)
	lds_annuity_payout_claim_summary.SetItem(li_inserted_row,'annuity_calc_no',ll_annuity_calc_no)
	lds_annuity_payout_claim_summary.SetItem(li_inserted_row,'total_annuity_benefit_amount',ldec_total_annuity_benefit_amount)
	lds_annuity_payout_claim_summary.SetItem(li_inserted_row,'previous_annuity_payout_amount',ldec_previous_annuity_payout_amount)
	lds_annuity_payout_claim_summary.SetItem(li_inserted_row,'overpayment_recovery_amount',0.00)
	lds_annuity_payout_claim_summary.SetItem(li_inserted_row,'writeoff_amount',0.00)
NEXT

li_rtn = lds_annuity_payout_claim_summary.Update()
SQLCA.nf_handle_error('w_prepare_annuity_account','lds_annuity_payout_claim_summary.Update()','wf_initial_annuity_payout_population')

// update initial ANNUITY_PAYOUT totals
ldec_AP_total_annuity_benefit_amount = ids_populate_initial_annuity_payout_balances.GetItemDecimal(1,'ap_total_annuity_benefit_amount')
ldec_AP_prev_annuity_payout_amount = ids_populate_initial_annuity_payout_balances.GetItemDecimal(1,'ap_prev_annuity_payout_amount')

UPDATE ANNUITY_PAYOUT
SET    annuity_eligibility_confirmed_flag = 'Y',
       total_annuity_benefit_amount       = :ldec_AP_total_annuity_benefit_amount,
       prev_annuity_payout_amount         = :ldec_AP_prev_annuity_payout_amount
WHERE  annuity_payout_no = :il_annuity_payout_no
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: UPDATE ANNUITY_PAYOUT...','wf_initial_annuity_payout_population')


end subroutine

public function integer wf_request_payout_event_count (ref string as_request_type_code);DATETIME    ldtm_prior_to_payout_request
INTEGER		li_event_count, li_non_writeoff_payout_count
STRING      ls_previous_checklist_step_type_code, ls_event_type_code, ls_event_specific_code


// returns the num of annuity payout auth request events that have been created since the confirm payout step (018) was completed
// in the case of claimant, its an individual event; in the case of SS, its a claim event.


IF is_checklist_type_code = 'CAPAP' THEN
	ls_previous_checklist_step_type_code = '022'
ELSEIF is_checklist_type_code = 'CAPLS' THEN
	ls_previous_checklist_step_type_code = '020'
END IF

SELECT concluded_date
INTO   :ldtm_prior_to_payout_request
FROM   CHECKLIST_STEP
WHERE  checklist_no             = :il_checklist_no
AND    checklist_step_type_code = :ls_previous_checklist_step_type_code
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account', 'wf_request_payout_event_count', 'SELECT concluded_date FROM CHECKLIST...')


SELECT Count(*)
INTO   :li_non_writeoff_payout_count
FROM   ANNUITY_PAYOUT
WHERE  annuity_payout_no     = :il_annuity_payout_no
AND    net_annuity_payout_amount - writeoff_amount > 0.00
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account', 'wf_request_payout_event_count', 'SELECT Count(*) FROM ANNUITY_PAYOUT_TXN_DETAIL...')

IF li_non_writeoff_payout_count = 0 THEN
	as_request_type_code = 'writeoff'
ELSE
	as_request_type_code = 'request'
END IF


IF is_claim_role_code = 'C' THEN
	IF li_non_writeoff_payout_count > 0 THEN
		ls_event_type_code     = '048'
		ls_event_specific_code = 'REQ'
	ELSE
		// claimant payout has write-off only
		ls_event_type_code     = '050'
		ls_event_specific_code = ''
	END IF
	
	SELECT Count(*)
	INTO   :li_event_count
	FROM   INDIVIDUAL_EVENT
	WHERE  individual_no        = :il_individual_no
	AND    event_type_code      = :ls_event_type_code
	AND    event_specific_code  = :ls_event_specific_code
	AND    create_date         >= :ldtm_prior_to_payout_request
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_account', 'wf_request_payout_event_count', 'SELECT Count(*) FROM INDIVIDUAL_EVENT')

ELSE	
	IF li_non_writeoff_payout_count > 0 THEN
		ls_event_type_code     = '053'
		ls_event_specific_code = 'REQ'
	ELSE
		ls_event_type_code     = '055'
		ls_event_specific_code = ''
	END IF
	
	SELECT Count(*)
	INTO   :li_event_count
	FROM   CLAIM_EVENT
	WHERE  claim_no             = :il_annuity_account_claim_no
	AND    event_type_code      = :ls_event_type_code
	AND    event_specific_code  = :ls_event_specific_code
	AND    create_date         >= :ldtm_prior_to_payout_request
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_account', 'wf_request_payout_event_count', 'SELECT Count(*) FROM INDIVIDUAL_EVENT')
END IF


RETURN li_event_count
end function

public subroutine wf_determine_cap_checklist_type (ref string as_checklist_type_code);DATETIME  ldtm_death
INTEGER   li_threshold_count, li_completed_count
// sets the new checklist type to either CAPAP (purchase) or CAPLS (lump sum)

SELECT Count(*)
INTO   :li_threshold_count
FROM   ANNUITY_PAYOUT
WHERE  annuity_payout_no = :il_annuity_payout_no
AND    net_annuity_payout_amount < ( SELECT purchase_annuity_amount_threshold 
                                     FROM   Annuity_Parameter
												 WHERE  active_flag = 'Y')
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT Count(*) FROM ANNUITY_PAYOUT...','wf_determine_cap_checklist_type')


ldtm_death = dw_payout_accounts.GetItemDateTime(dw_payout_accounts.GetRow(),'death_date')

// determine if there has been a completed payout
// for the eligible individual (the individual in ANNUITY_ACCOUNT)
// where the IW/SS was not written off
SELECT Count(*)
INTO   :li_completed_count
FROM   ANNUITY_PAYOUT a
WHERE  a.annuity_payout_no = :il_annuity_payout_no
AND    EXISTS ( SELECT *
                FROM   ANNUITY_PAYOUT              b
					 JOIN   ANNUITY_PAYOUT_PARTICIPANT  c ON b.annuity_payout_no = c.annuity_payout_no
					 JOIN   Annuity_Role                d ON c.annuity_role_code = d.annuity_role_code
					 WHERE  b.annuity_account_no           = a.annuity_account_no
					 AND    b.annuity_payout_no           <> :il_annuity_payout_no
					 AND    b.annuity_payout_status_code   = 'C'
					 AND    c.annuity_payout_writeoff_flag = 'N'
					 AND    d.annuity_eligibility_flag     = 'Y')
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT Count(*) FROM ANNUITY_PAYOUT...','wf_determine_cap_checklist_type')


// its a purchase payout if >= threshold, not deceased & no previously completed payout with payout
IF li_threshold_count = 0 AND IsNull(ldtm_death) AND li_completed_count = 0 THEN
	is_checklist_type_code = 'CAPAP'
	as_checklist_type_code = 'CAPAP'
ELSE
	is_checklist_type_code = 'CAPLS'
	as_checklist_type_code = 'CAPLS'	
END IF

end subroutine

public subroutine wf_determine_prev_manual_intervention ();INTEGER    li_count

// if the individual has a previous manual intervention associated with the annuity account, then no payout
SELECT Count(*)
INTO   :li_count
FROM   ANNUITY_PAYOUT                    a
JOIN   Annuity_Payout_Status_Reason      b ON a.annuity_payout_status_reason_code = b.annuity_payout_status_reason_code
JOIN   Annuity_Payout_Status_Reason_Xref c ON a.annuity_payout_status_code = c.annuity_payout_status_code
														AND a.annuity_payout_status_reason_code = c.annuity_payout_status_reason_code
WHERE  a.annuity_account_no         = :il_annuity_account_no
AND    a.annuity_payout_status_code = 'D'
AND    b.manual_intervention_flag   = 'Y'
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_payout', 'embedded SQL: SELECT Count(*) FROM ANNUITY_PAYOUT...', 'wf_post_cae_no_payout')

IF li_count > 0 THEN
	ib_no_payout = TRUE
	ib_previous_manual_intervention = TRUE
END IF
end subroutine

public subroutine wf_determine_eligibility (long al_annuity_eligibility_no, ref string as_annuity_eligibility_status_code, ref datetime adtm_annuity_start_date, ref datetime adtm_annuity_end_date);

// determine if individual was eligible
SELECT annuity_eligibility_status_code,
       annuity_start_date,
		 annuity_end_date
INTO   :as_annuity_eligibility_status_code,
       :adtm_annuity_start_date,
       :adtm_annuity_end_date
FROM   ANNUITY_ELIGIBILITY 
WHERE  annuity_eligibility_no = :al_annuity_eligibility_no
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT annuity_eligibility_status_code FROM ANNUITY_ELIGIBILITY ...','wf_determine_eligibility')
end subroutine

public subroutine wf_get_payout_annuity_eligibility_no ();SELECT annuity_eligibility_no
INTO   :il_annuity_eligibility_no
FROM   ANNUITY_PAYOUT
WHERE  annuity_payout_no = :il_annuity_payout_no
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT annuity_eligibility_no FROM ANNUITY_PAYOUT...','wf_get_payout_annuity_eligibility_no')

end subroutine

public subroutine wf_determine_prev_completed_with_payout ();
// determine last successfully completed payout
SELECT IsNull(Max(annuity_payout_no),0)
INTO   :il_previously_completed_payout_no
FROM   ANNUITY_PAYOUT
WHERE  annuity_account_no = :il_annuity_account_no
AND    annuity_payout_no <> :il_annuity_payout_no
AND    annuity_payout_status_code = 'C'
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT Max(annuity_payout_no) FROM ANNUITY_PAYOUT...', 'wf_determine_prev_completed_with_payout')

IF il_previously_completed_payout_no > 0 THEN
	SELECT annuity_payout_calc_method_code
	INTO   :is_prev_annuity_annuity_payout_calc_method_code
	FROM   ANNUITY_PAYOUT
	WHERE  annuity_payout_no = :il_previously_completed_payout_no
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT Max(annuity_payout_no) FROM ANNUITY_PAYOUT...', 'wf_determine_prev_completed_with_payout')
END IF
end subroutine

public function long wf_determine_no_payout_reason (ref u_checklist_datawindow adw_dw);// this function will populate the ANNUITY_PAYOUT.annuity_payout_status_reason_code if it is determined that the 
// annuity payout will be paid out with no payout

BOOLEAN      lb_annuity_ends_in_future
INTEGER      li_rtn, li_msg_rtn, li_rows
DATETIME     ldtm_annuity_start_date, ldtm_annuity_end_date, ldtm_current_date
N_CHECKLIST  lnv_checklist
STRING       ls_annuity_eligibility_status_code, ls_annuity_payout_status_reason_code, ls_annuity_calc_method_msg


lnv_checklist = uo_prepare_checklist.inv_checklist


// find out if this annuity account had had a previous payout that was completed "with payout"
// this populates instance variable il_previously_completed_payout_no
wf_determine_prev_completed_with_payout()

// find out if this annuity account has had a previous payout that required manual interventions
wf_determine_prev_manual_intervention()


IF ib_previous_manual_intervention THEN
	// do not indicate that the participant list cannot be changed if the payout is going to be manual anyway
	li_rtn = wf_no_payout_subledger_warning('PMI')
	IF li_rtn < 0 THEN
		ib_previous_manual_intervention = FALSE
		ib_no_payout = FALSE
		// user chose to cancel the completion of checklist step when warned about non-zero subledger balance
		wf_reject_checklist_status_change(FALSE,'','',il_checklist_no,'PAA',lnv_checklist,adw_dw)
		
		RETURN li_rtn
	END IF
	
END IF

IF ib_previous_manual_intervention THEN
	// no need to determine a different 'no payout' reason
	
	// warn users
	li_msg_rtn = MessageBox('BR 9.150,9.160 - Previous Manual Intervention','The associated annuity account has had a previous annuity payout that required manual intervention. ' &
																										+ 'You can reject the completion of this checklist step and wait until the manual work is completed, '&
																										+ 'or you can have the individual removed from the Annuity Payout list now.' &
																										+ '~r~n'&
																										+ '~r~nWould you like to remove this individual from the Annuity Payout List now?',Question!,YesNo!,2)
	IF li_msg_rtn = 2 THEN
		ib_previous_manual_intervention = FALSE
		ib_no_payout = FALSE
		wf_reject_checklist_status_change(FALSE,'','',il_checklist_no,'PAA',lnv_checklist,adw_dw)
		
		RETURN -1
	END IF

ELSE	
	IF il_previously_completed_payout_no > 0 THEN
		
		// determine if the calculation method for the annuity payout has changed
		// and a manual intervention will be required.
		wf_determine_annuity_calc_method_change(ls_annuity_calc_method_msg)
		
		IF ib_modified_annuity_calc_method THEN
			li_rtn = wf_no_payout_subledger_warning('MAC')
			IF li_rtn < 0 THEN
				ib_no_payout = FALSE
				ib_modified_annuity_calc_method = FALSE
				
				wf_reject_checklist_status_change(FALSE,'','',il_checklist_no,'PAA',lnv_checklist,adw_dw)
				
				RETURN li_rtn
			ELSE
				IF ib_non_zero_subledger_warning THEN
					// non-zero sub-ledger balance
					RETURN 0
				ELSE
					// zero sub-ledger balance
					// no need to determine a different 'no payout' reason
					
					// warn users
					li_msg_rtn = MessageBox('BR 9.150,9.160 - Modified Annuity Payout Calculation',ls_annuity_calc_method_msg &
																											+ '~r~n'&
																											+ '~r~nWould you like to remove this individual from the Annuity Payout List now?',Question!,YesNo!,2)
					IF li_msg_rtn = 2 THEN
						ib_no_payout = FALSE
						ib_modified_annuity_calc_method = FALSE
						
						wf_reject_checklist_status_change(FALSE,'','',il_checklist_no,'PAA',lnv_checklist,adw_dw)
						
						RETURN -1							
					ELSE
						RETURN 0
					END IF
				END IF

			END IF
		END IF

		// look at 'no payout' reasons that result from determining annuity benefit claim summaries
		// APP (Annuity Previously Paid) 
		// RCA (Reduction in Claim Annuity Benefits)
		
		// this stored proc will determine if annuity payout status reason should be set
		li_rows = ids_populate_initial_annuity_payout_balances.Retrieve(il_annuity_payout_no,il_previously_completed_payout_no)
		SQLCA.nf_handle_error('w_prepare_annuity_account','ids_populate_initial_annuity_payout_balances.retrieve (2)','wf_determine_no_payout_reason')
				
		IF li_rows <= 0 THEN
			wf_reject_checklist_status_change(TRUE,'Annuity Problem','The completion of this checklist step failed - CALL HELPDESK.',il_checklist_no,'PAA',lnv_checklist,adw_dw)
			
			RETURN li_rows
		ELSE
			ls_annuity_payout_status_reason_code = ids_populate_initial_annuity_payout_balances.GetItemString(1,'annuity_payout_status_reason_code')
			IF ls_annuity_payout_status_reason_code = 'APP' OR ls_annuity_payout_status_reason_code = 'RCA' THEN
				li_rtn = wf_no_payout_subledger_warning(ls_annuity_payout_status_reason_code)
				IF li_rtn < 0 THEN
					ib_no_payout = FALSE
					wf_reject_checklist_status_change(FALSE,'','',il_checklist_no,'PAA',lnv_checklist,adw_dw)
					
					RETURN li_rtn
				ELSE
					IF ls_annuity_payout_status_reason_code = 'APP' THEN
						ib_no_payout = TRUE
						ib_annuity_previously_paid = TRUE
					ELSEIF ls_annuity_payout_status_reason_code = 'RCA' THEN
						ib_no_payout = TRUE
						ib_reduction_of_claim_annuity_benefits = TRUE
					END IF
				END IF
			END IF
		END IF
		
		ldtm_current_date = f_server_datetime()
		
		// find out if annuity ends in future or if individual is no longer eligible
		// need to know this for 'different participant list' question below
		wf_determine_eligibility(il_annuity_eligibility_no,ls_annuity_eligibility_status_code,ldtm_annuity_start_date,ldtm_annuity_end_date)
		IF DaysAfter(Date(ldtm_current_date), Date(ldtm_annuity_end_date)) > 0 THEN
			lb_annuity_ends_in_future = TRUE
		END IF
		
		IF lb_annuity_ends_in_future OR ls_annuity_eligibility_status_code = 'I' OR ib_no_payout THEN
			// do not ask if different participant list will be required if
			// the annuity ends in the future or
			// the individual is not eligible for benefits or
			// it has already been determined that the annuity payout will be completed as 'no payout'		
		ELSE
			wf_determine_writeoff_on_last_payout()
			IF ib_can_change_participants = FALSE THEN
				// warn users that participant list cannot be changed.
				li_msg_rtn = MessageBox('Previous Annuity Payout','This individual has had a previously completed payout. The participant list for this payout cannot be changed in this module. If a change is required, then the payout must be completed manually.' &
				                                                + '~r~n~r~nDo you want to complete this payout manually?',Question!,YesNo!,2)
				IF li_msg_rtn = 1 THEN
					li_msg_rtn = MessageBox('Complete Payout Manually?','WARNING - Do you want to complete this payout MANUALLY?',Question!, YesNo!,2)
					
					IF li_msg_rtn = 1 THEN
						// indicate that ANNUITY_PAYOUT is to be set to D/DPP, Different Payout Participants, set remaining steps to NRA
						ib_different_participant_list = TRUE
						
						li_rtn = wf_no_payout_subledger_warning('DPP')
						IF li_rtn < 0 THEN
							ib_no_payout = FALSE
							ib_different_participant_list = FALSE
							// user chose to cancel the completion of checklist step when warned about non-zero subledger balance
							wf_reject_checklist_status_change(FALSE,'','',il_checklist_no,'PAA',lnv_checklist,adw_dw)
							
							RETURN li_rtn
						END IF
					ELSE
						// set the status back to 'Incomplete'
						lnv_checklist.nf_retrieve_checklists(is_checklist_type_code,il_checklist_no)
						lnv_checklist.nf_get_next_checklist_step(il_checklist_no)
						RETURN -1
					END IF
				END IF
			END IF
		END IF
		
	ELSE
		// look at 'no payout' reasons that can be determined from the recently confirmed annuity eligibility
		// these reasons are only valid if there has NOT been a previously completed annuity payout 'with payout'
		
		// AEE (Annuity Eligibility Ends in Future)
		// NBE (No Benefit Entitlement After Qualification Period)
		// NEA (Not Eligible for Annuity Benefits)
		li_rtn = wf_post_CAE_no_payout()
		IF li_rtn < 0 THEN
			ib_no_payout = FALSE
			// user chose to cancel the completion of checklist step
			wf_reject_checklist_status_change(FALSE,'','',il_checklist_no,'PAA',lnv_checklist,adw_dw)
			
			RETURN li_rtn
		END IF
	END IF
	
	// checklist step & annuity payout will continue
	IF NOT ib_no_payout THEN
		li_rows = ids_populate_initial_annuity_payout_balances.Retrieve(il_annuity_payout_no,il_previously_completed_payout_no)
		SQLCA.nf_handle_error('w_prepare_annuity_account','ids_populate_initial_annuity_payout_balances.retrieve (3)','wf_determine_no_payout_reason')
	END IF
	
	
END IF
end function

public function integer wf_build_annuity_purchase_name_on_chq (ref string as_purchase_name_on_cheque, ref boolean ab_more_than_forty_chars, boolean ab_display_messagebox, string as_checklist_step_desc, string as_new_annuity_carrier_name);INTEGER   li_msg_rtn
STRING    ls_recipient_name, ls_annuity_carrier_name, ls_name_on_cheque

// when ab_display_messagebox = true, then function is used to build the string and ask user whether to complete step & later perform update
// when ab_display_messagebox = false, then function is used to build the string to determine if ANNUITY_PAYOUT_RECIPIENT.address_line1 
// should be protected, because it contains truncated remainder of string after 40 characters fills ANNUITY_PAYOUT_RECIPIENT.name_on_cheque


SELECT given_names + ' ' + last_name
INTO   :ls_recipient_name
FROM   INDIVIDUAL 
WHERE  individual_no = :il_individual_no
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT given_names, last_name FROM INDIVIDUAL...','wf_build_annuity_purchase_name_on_chq')

IF as_new_annuity_carrier_name = '' THEN
	SELECT annuity_carrier_name
	INTO   :ls_annuity_carrier_name
	FROM   ANNUITY_CONTRACT
	WHERE  annuity_payout_no = :il_annuity_payout_no
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT annuity_carrier_name FROM ANNUITY_CONTRACT...','wf_build_annuity_purchase_name_on_chq')
END IF

as_purchase_name_on_cheque = ls_recipient_name + ' & ' + ls_annuity_carrier_name

IF Len(as_purchase_name_on_cheque) > 40 THEN
	ib_more_than_forty_chars = TRUE
	ab_more_than_forty_chars = TRUE
	IF ab_display_messagebox THEN
		li_msg_rtn = MessageBox('Annuity Payout Recipient - Length of Name','The annuity payout recipient name that appears on the cheque is' &
                                                                     +'~r~nlonger than 40 characters. The name will be extended onto the first' &
                                                                     +'~r~nline of address, as follows:'&
																							+'~r~n   ' + Left(as_purchase_name_on_cheque,40) &
                                                                     +'~r~n   ' + Right(as_purchase_name_on_cheque,Len(as_purchase_name_on_cheque) - 40) &
                                                                     +'~r~nTo continue '+as_checklist_step_desc+', click Yes.' &
                                                                     +'~r~nTo cancel the save, and make necessary corrections, click No.',Question!,YesNo!,2)
		IF li_msg_rtn = 1 THEN
			RETURN 0
		ELSE
			RETURN -1
		END IF
	ELSE
		RETURN 0
	END IF
ELSE
	ib_more_than_forty_chars = FALSE
	ab_more_than_forty_chars = FALSE
END IF


RETURN 0
end function

public subroutine wf_auto_request_authorization (string as_checklist_type_code);/*

if current user has adequate authorization level to payout amount for annuity then indicate that 
next step (request authorization) can be automatically completed

*/

DATETIME ldtm_current
DECIMAL  ldec_sub_total_annuity_payout_amount, ldec_authorization_level
LONG     ll_counter
STRING   ls_annuity_admin_region_code


SELECT annuity_admin_region_code,
       sub_total_annuity_payout_amount
INTO   :ls_annuity_admin_region_code,
       :ldec_sub_total_annuity_payout_amount
FROM   ANNUITY_PAYOUT
WHERE  annuity_payout_no = :il_annuity_payout_no
USING SQLCA;
SQLCA.nf_Handle_error('w_prepare_annuity_account','embedded SQL: SELECT annuity_admin_region_code,net_annuity_payout_amount FROM ANNUITY_PAYOUT...','wf_auto_request_authorization')

// retrieve authorization level for admin region of payout
ll_counter = 1
DO UNTIL Trim(ls_annuity_admin_region_code) = Trim(vgst_user_profile.annuity_authorizations[ll_counter,1]) OR ll_counter = Upperbound(vgst_user_profile.annuity_authorizations)	
	ll_counter++
LOOP
IF Trim(ls_annuity_admin_region_code) <> Trim(vgst_user_profile.annuity_authorizations[ll_counter,1]) THEN
	// no authorization, no auto-complete for step
	ib_auto_authorization = FALSE
ELSE
	ldec_authorization_level = Dec(vgst_user_profile.annuity_authorizations[ll_counter,2])
END IF

IF ldec_sub_total_annuity_payout_amount <= ldec_authorization_level THEN
	// adequate authorization, auto-complete for step
	
	ldtm_current = f_server_datetime()
	
	UPDATE ANNUITY_PAYOUT
	SET    authorized_by_user_id = :vgst_user_profile.user_id ,
          authorized_date       = :ldtm_current
	WHERE  annuity_payout_no = :il_annuity_payout_no
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: UPDATE ANNUITY_PAYOUT...','wf_auto_request_authorization')
	
	ib_auto_authorization = TRUE
ELSE
	// inadequate authorization, no auto-complete for step
	ib_auto_authorization = FALSE
END IF
end subroutine

public function integer wf_reinsert_onto_annuity_payout_list ();INTEGER    li_rows
U_DS       lds_force_onto_payout


// reinsert individual into ANNUITY_PAYOUT
lds_force_onto_payout = Create U_DS
lds_force_onto_payout.DataObject = 'ds_force_onto_payout'
lds_force_onto_payout.SetTransObject(SQLCA)

// run stored proc to insert data

li_rows = lds_force_onto_payout.Retrieve(il_annuity_account_claim_no,il_individual_no,'CANCEL')
SQLCA.nf_handle_error('w_prepare_annuity_account','lds_force_onto_payout.retrieve','wf_reinsert_onto_annuity_payout_list')


IF li_rows < 1 THEN
	// problem!
	MessageBox('Payout List Error','The retrieval of the payout list for this individual encountered an error. Contact HELPDESK.')
	RETURN -1
END IF

RETURN 0
end function

public subroutine wf_determine_annuity_calc_method_change (ref string as_msg);// determine if this annuity payout should be completed manually
STRING ls_prev_desc, ls_curr_desc, ls_curr_annuity_annuity_payout_calc_method_code
U_DS   lds_ann_pay_calc_method


lds_ann_pay_calc_method            = Create U_DS
lds_ann_pay_calc_method.DataObject = 'ds_ann_pay_calc_method'
lds_ann_pay_calc_method.SetTransObject(SQLCA)





// use embedded SQL rather than getitemstring, so as to avoid re-retrieval of dw_payout_accounts
SELECT annuity_payout_calc_method_code
INTO   :ls_curr_annuity_annuity_payout_calc_method_code
FROM   ANNUITY_PAYOUT
WHERE  annuity_payout_no = :il_annuity_payout_no
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT annuity_payout_calc_method_code FROM ANNUITY_PAYOUT...','wf_determine_no_payout_reason')

IF is_prev_annuity_annuity_payout_calc_method_code = 'NNI' THEN
	lds_ann_pay_calc_method.Retrieve(is_prev_annuity_annuity_payout_calc_method_code)
	ls_prev_desc = lds_ann_pay_calc_method.GetItemString(1,'annuity_payout_calc_method_desc')
	
	as_msg = 'The associated annuity account has had a previous annuity payout with calculation method: "'+ls_prev_desc+'". ' &
			 + '~r~n~r~n' &
	       + 'If this checklist step is completed, then the individual will be removed from the Annuity Payout list now, and ' &
			 + 'any future annuity payout will have to be completed manually.'
	ib_modified_annuity_calc_method = TRUE
	ib_no_payout = TRUE
ELSEIF ls_curr_annuity_annuity_payout_calc_method_code = 'NNI' THEN
	lds_ann_pay_calc_method.Retrieve(is_prev_annuity_annuity_payout_calc_method_code)
	ls_prev_desc = lds_ann_pay_calc_method.GetItemString(1,'annuity_payout_calc_method_desc')
	
	lds_ann_pay_calc_method.Retrieve(ls_curr_annuity_annuity_payout_calc_method_code)
	ls_curr_desc = lds_ann_pay_calc_method.GetItemString(1,'annuity_payout_calc_method_desc')
		
	as_msg = 'The associated annuity account has had a previous annuity payout with calculation method: "'+ls_prev_desc+'". ' &
	       + 'The current annuity payout is calculation method: "'+ls_curr_desc+'." ' &
			 + '~r~n~r~n' &
	       + 'If this checklist step is completed, then the individual will be removed from the Annuity Payout list now, and ' &
			 + 'any future annuity payout will have to be completed manually.'
	ib_modified_annuity_calc_method = TRUE
	ib_no_payout = TRUE
END IF

end subroutine

on w_prepare_annuity_account.create
if this.MenuName = "m_annuity" then this.MenuID = create m_annuity
this.st_select_annuity_payout=create st_select_annuity_payout
this.cb_manual_annuity_payout=create cb_manual_annuity_payout
this.cb_reset_sort=create cb_reset_sort
this.dw_payout_region=create dw_payout_region
this.tab_payout=create tab_payout
this.rb_region=create rb_region
this.rb_individual=create rb_individual
this.cb_save_region=create cb_save_region
this.uo_filter_control=create uo_filter_control
this.st_splitbar_1=create st_splitbar_1
this.dw_payout_accounts=create dw_payout_accounts
this.uo_prepare_checklist=create uo_prepare_checklist
this.uo_confirm_payout_checklist=create uo_confirm_payout_checklist
this.dw_annuity_history=create dw_annuity_history
this.st_multiple_accounts=create st_multiple_accounts
this.Control[]={this.st_select_annuity_payout,&
this.cb_manual_annuity_payout,&
this.cb_reset_sort,&
this.dw_payout_region,&
this.tab_payout,&
this.rb_region,&
this.rb_individual,&
this.cb_save_region,&
this.uo_filter_control,&
this.st_splitbar_1,&
this.dw_payout_accounts,&
this.uo_prepare_checklist,&
this.uo_confirm_payout_checklist,&
this.dw_annuity_history,&
this.st_multiple_accounts}
end on

on w_prepare_annuity_account.destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.st_select_annuity_payout)
destroy(this.cb_manual_annuity_payout)
destroy(this.cb_reset_sort)
destroy(this.dw_payout_region)
destroy(this.tab_payout)
destroy(this.rb_region)
destroy(this.rb_individual)
destroy(this.cb_save_region)
destroy(this.uo_filter_control)
destroy(this.st_splitbar_1)
destroy(this.dw_payout_accounts)
destroy(this.uo_prepare_checklist)
destroy(this.uo_confirm_payout_checklist)
destroy(this.dw_annuity_history)
destroy(this.st_multiple_accounts)
end on

event resize;
LONG ll_workspacewidth,ll_workspaceheight

IF IsValid(inv_resize) THEN
	// Notify the resize service that the window size has changed.
	ll_workspacewidth  = This.WorkSpaceWidth()
	ll_workspaceheight = This.WorkSpaceHeight()

	IF IsValid (inv_resize) THEN
		inv_resize.Event pfc_Resize (sizetype,ll_workspacewidth  , ll_workspaceheight )
	END IF 
END IF 

// must post function calls to allow individual constructor events to fire
st_splitbar_1.post of_SetRequestor(This)

function post wf_post_resize_checklist_position()
ib_checklist_posted = TRUE


function post wf_post_hscrollbar_position()

end event

event open;INTEGER     li_rows, li_upperbound, li_inserted_row, li_count
LONG        ll_paa_checklist_no
STRING		ls_filter, ls_filter_return, ls_modify, ls_rtn, ls_checklist_status_code
S_WINDOW_MESSAGE          lstr_message
WINDOW      lw_win

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


//	APPLICATION SECURITY CODE
//
G_PFSecurity.UOF_Check_Access(This)
I_Authorized_Access = True              //declared as an instance variable

lw_win = THIS

lstr_message = Message.PowerObjectParm

iw_win = wf_get_window_reference()

is_pap_open_mode = lstr_message.as_stringparm[2]

iul_handle = Handle(THIS)

// sets up to close this window if frame is closed

li_upperbound = UpperBound(gstr_window_array) + 1
gstr_window_array[li_upperbound].window_element = THIS
gstr_window_array[li_upperbound].handle_element = iul_handle

IF is_pap_open_mode = 'entry' THEN
	IF isnull(vgst_user_profile.default_admin_region_code) OR vgst_user_profile.default_admin_region_code = '' THEN
		MESSAGEBOX('User Profile Error','Your User profile is missing a valid Default Admin region Code. Please contact the helpdesk.')
		RETURN
	END IF
	st_select_annuity_payout.Visible = FALSE
	THIS.Title = 'Prepare Annuity Payout'
ELSE	
	st_select_annuity_payout.Visible = TRUE
	THIS.Title = 'Annuity Payout Inquiry'
END IF

inv_common_annuity = Create n_common_annuity
inv_annuity_participant = CREATE n_annuity_participant


idw_participants = tab_payout.tabpage_participants.dw_participants
idw_participant_details = tab_payout.tabpage_participants.dw_participant_details

idw_annuity_payout_summary = tab_payout.tabpage_payout_summary.dw_annuity_payout_summary

idw_annuity_payout_detail = tab_payout.tabpage_payout_details.dw_annuity_payout_detail

idw_annuity_payout_txn_detail = tab_payout.tabpage_annuity_payout_txn_detail.dw_annuity_payout_txn_detail

idw_payout_recipients = tab_payout.tabpage_payout_recipients.dw_payout_recipients
idw_payout_recipient_details = tab_payout.tabpage_payout_recipients.dw_payout_recipient_details
idw_payout_recipient_report = tab_payout.tabpage_payout_recipients.dw_recipient_report

idw_annuity_overpayment = tab_payout.tabpage_overpayment.dw_annuity_overpayment

idw_contract_beneficiaries = tab_payout.tabpage_contract_and_beneficiaries.dw_contract_beneficiaries
idw_contract_beneficiaries_details = tab_payout.tabpage_contract_and_beneficiaries.dw_contract_beneficiaries_details


il_design_time_height = 3136
il_design_time_width = 5093

st_splitbar_1.of_Register(dw_payout_accounts)

st_splitbar_1.of_Register(tab_payout)

// set up split bar & resizing
This.wf_SetResize(True)

//(Move H,Move V,Grow H, Grow V)
inv_resize.of_register(dw_payout_accounts,'ScaleToRight')
inv_resize.of_register(st_splitbar_1,0,0,100,0)
inv_resize.of_register(tab_payout,'ScaleToRight&Bottom')



dw_payout_accounts.uf_setselect(1)
dw_payout_accounts.uf_setfilter(True)
uo_filter_control.uf_set_Requestor(dw_payout_accounts)


// set up checklist object for PAA checklists
uo_prepare_checklist.uf_set_module_code('050')
uo_prepare_checklist.uf_set_checklist_type_code('PAA')	
uo_prepare_checklist.uf_set_parent_window(iw_win)	
uo_prepare_checklist.uf_get_checklist_nvo(inv_prepare_checklist)	
uo_prepare_checklist.itr_trans_object = SQLCA	

inv_prepare_checklist.nf_set_datawindow(uo_prepare_checklist.idw_dw[],SQLCA)
inv_prepare_checklist.nf_set_checklist_object(uo_prepare_checklist)
inv_prepare_checklist.nf_set_checklist_subscriber_type('ANN')

// set up checklist object for CAP checklists
uo_confirm_payout_checklist.uf_set_module_code('050')
uo_confirm_payout_checklist.uf_set_parent_window(iw_win)	
uo_confirm_payout_checklist.uf_get_checklist_nvo(inv_confirm_payout_checklist)
uo_confirm_payout_checklist.itr_trans_object = SQLCA	

inv_confirm_payout_checklist.nf_set_datawindow(uo_confirm_payout_checklist.idw_dw[],SQLCA)
inv_confirm_payout_checklist.nf_set_checklist_object(uo_confirm_payout_checklist)
inv_confirm_payout_checklist.nf_set_checklist_subscriber_type('ANN')

il_annuity_account_claim_no = lstr_message.al_doubleparm[1]
il_individual_no = lstr_message.al_doubleparm[2]
il_forced_individual_no = il_individual_no
is_admin_region_code = lstr_message.as_stringparm[1]


IF il_forced_individual_no = 0 THEN
	// prevent rowfocuschange code from firing until final list individual is selected
	ib_opening = TRUE
END IF


IF is_pap_open_mode = 'inquiry' THEN
	// open with provincial region, allows users to inquire on any payout
	li_rows = dw_payout_accounts.Retrieve(il_individual_no,'PRV','inquiry')
	cb_manual_annuity_payout.Visible = FALSE
	IF li_rows = 0 THEN 
		MessageBox('No Annuity Payouts','There was a problem retrieving the Annuity Payout record for this individual. Please contact the HELPDESK.')
		RETURN
	END IF
ELSE
	IF il_forced_individual_no > 0 THEN
		// retrieve list for 'tombstone' individual
		li_rows = dw_payout_accounts.Retrieve(il_forced_individual_no,vgst_user_profile.default_admin_region_code,'entry')
		rb_individual.POST EVENT CLICKED()
	ELSE
		// retrieve unfiltered list
		li_rows = dw_payout_accounts.Retrieve(0,vgst_user_profile.default_admin_region_code,'entry')
	END IF	
END IF
SQLCA.nf_handle_error('w_prepare_for_annuity_account','dw_payout_accounts','retrieve')

dw_payout_accounts.setredraw(false)  // we'll redraw after the filtering takes place

IF li_rows = 0 THEN 
	MessageBox('No Annuity Payouts','There are no Annuity Payout records.')
ELSE

	IF il_forced_individual_no = 0 THEN
		il_individual_no = 0
		rb_individual.Visible = FALSE
		rb_region.Visible = FALSE
		// Get instance variables
		
		il_annuity_account_no = dw_payout_accounts.GetItemNumber(dw_payout_accounts.GetRow(),'annuity_account_no')
		
		il_individual_no = dw_payout_accounts.GetItemNumber(dw_payout_accounts.GetRow(),'individual_no')
	
		// filter specific admin region for module
		ls_filter = "annuity_admin_region_code = '"+ vgst_user_profile.default_admin_region_code +"'"
		ls_filter_return = uo_filter_control.idw_datawindow.inv_filter.of_SetFilter(ls_filter)
		uo_filter_control.idw_datawindow.inv_filter.ib_filter_on = TRUE
		
		ls_modify = 'st_filter.Text="'+ls_filter+ '"'
		
		ls_rtn = dw_payout_accounts.Modify(ls_modify)
		
	ELSE
		rb_individual.Checked = TRUE
		// Get instance variables
		IF is_pap_open_mode = 'inquiry' THEN
			il_annuity_account_no = lstr_message.al_doubleparm[3]
		ELSE
			il_annuity_account_no = dw_payout_accounts.GetItemNumber(dw_payout_accounts.GetRow(),'annuity_account_no')
		END IF		
	END IF
END IF

// after filtering, trigger RFC for first row
// this enables, disables admin region dddw, o/p button, sets checklist type, retrieves checklist
IF dw_payout_accounts.RowCount() > 0 THEN
	dw_payout_accounts.SetRow(1)
	dw_payout_accounts.SelectRow(1,TRUE)
	il_payout_list_column = 1
	dw_payout_accounts.SetColumn(il_payout_list_column)
	
	ib_opening = FALSE
	dw_payout_accounts.event RowFocusChanged(1)

	// determine whether to display claim details tab, or not
	ll_paa_checklist_no = dw_payout_accounts.GetItemNumber(1,'prepare_annuity_payout_checklist_no')
	wf_display_claim_details_tab(ll_paa_checklist_no)
ELSE
	ib_opening = FALSE
	wf_clear_datawindows()
	MessageBox('No Annuity Payouts','There are no Annuity Payouts in your default region. There may be payouts in other regions.')
END IF

// do not call posted event below if an error has occurred
IF IsValid(w_error) THEN RETURN

post event ue_post_open()


// set size of window
IF IsValid(inv_resize) THEN
ELSE
	inv_resize = Create n_resize
END IF
inv_resize.of_SetOrigSize (this.width,this.height - 250)


IF is_pap_open_mode = 'inquiry' THEN
	// make print menu item not visible
	inv_common_annuity.nf_make_menu_item_invisible(lw_win,'m_file','m_print')
	inv_common_annuity.nf_make_menu_item_invisible(lw_win,'m_file','m_printsetup')
END IF



end event

event close;INTEGER		li_counter, li_upper

inv_common_annuity.nf_close_handle_array(iul_handle)

li_upper = UpperBound(gstr_window_array)

FOR li_counter = 1 TO li_upper
	IF gstr_window_array[li_counter].window_element.ClassName() = 'w_confirm_annuity_eligibility' THEN
		gstr_window_array[li_counter].window_element.SetFocus()
	END IF
NEXT
end event

event closequery;IF ib_save = TRUE THEN
	MESSAGEBOX("SAVE NEEDED","You have changes pending. Please Save or Cancel before proceding.")
	RETURN 1
END IF
end event

type st_select_annuity_payout from statictext within w_prepare_annuity_account
integer x = 46
integer y = 16
integer width = 590
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Select Annuity Payout:"
boolean focusrectangle = false
end type

type cb_manual_annuity_payout from commandbutton within w_prepare_annuity_account
integer x = 1669
integer y = 740
integer width = 434
integer height = 104
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Manual Payout"
end type

event clicked;BOOLEAN                lb_commit, lb_button_should_be_enabled
DECIMAL                ldec_subledger_balance
DWObject               l_dwo
INTEGER                li_msg_rtn, li_next_checklist_step_no, li_rtn, li_cheque_count, li_rows, li_non_writeoffs_found, li_writeoffs_found
N_CHECKLIST            lnv_checklist
STRING                 ls_checklist_step_status_code, ls_checklist_step_type_code, ls_payout_requested_by_user_id, ls_find, ls_payment_sub_type_phrase
U_CHECKLIST            luo_checklist
U_CHECKLIST_DATAWINDOW ldw_dw
U_DS                   lds_annuity_payout_payment_sub_types


// it is possible that the user has completed the annuity payout request
// without navigating off the record and returning to it later (which would leave the button possibly disabled, see BR 9.180)
// so, run the function that would normally determine whether it should be enabled or not.
lb_button_should_be_enabled = wf_enable_manual_payout_btn()
IF lb_button_should_be_enabled THEN
	// OK
ELSE
	// tell user that a manual payout cannot be selected, and disable button
	MessageBox('Manual Annuity Payout Selected','You cannot complete this annuity payout manually because one of the following is true:' &
														+ '~r~n· The annuity eligibility has not been confirmed yet or ' &
														+ '~r~n· A cheque number has been assigned to at least one associated annuity payment or ' &
                                          + '~r~n· The associated payments have been processed.',Exclamation!)
	THIS.Enabled = FALSE
	RETURN
END IF


// if applicable, warn user about completing payout with non-zero sub-ledger balance.
li_rtn = wf_no_payout_subledger_warning('MPS')
IF li_rtn < 0 THEN
	// user chose to cancel the manual payout
	RETURN
END IF


// determine which checklist is still incomplete
// find out the maximum incomplete checklist step within that checklist
// set boolean ib_complete_payout_manually to true
// trigger itemchanged for the maximum incomplete checklist step


ls_payout_requested_by_user_id = dw_payout_accounts.GetItemString(dw_payout_accounts.GetRow(),'requested_by_user_id')
IF IsNull(ls_payout_requested_by_user_id) THEN ls_payout_requested_by_user_id = ''

IF ls_payout_requested_by_user_id = '' THEN
	li_msg_rtn = MessageBox('Manual Annuity Payout Selected','You have chosen to complete this annuity payout manually.' &
																		+ '~r~nIf you choose to continue, then any subsequent annuity review ' &
                                                      + 'will have to be completed manually (i.e. cannot use Annuity modules).' &
																		+ '~r~n' &
																		+ '~r~nWould you like to continue with the manual completion of this payout?',Question!,YesNo!,2)
ELSE
	// determine the nature of payout request, so that message is specific to the situation
	// i.e., no write-off; some write-off, some non-write-off; all write-off
	lds_annuity_payout_payment_sub_types = Create U_DS
	lds_annuity_payout_payment_sub_types.DataObject = 'ds_annuity_payout_payment_sub_types'
	lds_annuity_payout_payment_sub_types.SetTransObject(SQLCA)
	
	li_rows = lds_annuity_payout_payment_sub_types.Retrieve(il_annuity_payout_no)
	SQLCA.nf_handle_error('w_prepare_annuity_account','lds_annuity_payout_payment_sub_types.Retrieve','cb_manual_annuity_payout.clicked')
	
	ls_find = 'payment_sub_type_code = "CW"'
	li_writeoffs_found = lds_annuity_payout_payment_sub_types.Find(ls_find,1,li_rows)
		
	ls_find = 'payment_sub_type_code <> "CW"'
	li_non_writeoffs_found = lds_annuity_payout_payment_sub_types.Find(ls_find,1,li_rows)
	
	IF li_non_writeoffs_found > 0 THEN
		IF li_writeoffs_found > 0 THEN
			ls_payment_sub_type_phrase = 'the cheque number has not yet been assigned and the associated payments have not been processed'
		ELSE
			ls_payment_sub_type_phrase = 'the cheque number has not yet been assigned'
		END IF
	ELSE
		// li_writeoffs_found > 0, but li_non_writeoffs_found > 0
		ls_payment_sub_type_phrase = 'the associated payments have not been processed'
	END IF
	
	li_msg_rtn = MessageBox('Manual Annuity Payout Selected','You have chosen to complete this annuity payout manually. ' &
                                                          + 'If you choose to continue, then any subsequent annuity review ' &
                                                          + 'will have to be completed manually (i.e. cannot use Annuity modules).' &
                                                          + '~r~n' &
                                                          + '~r~nThe annuity payout has been requested BUT '+ls_payment_sub_type_phrase+'. '&
                                                          + 'If you choose to complete this annuity payout manually, you MUST contact Financial Services '&
                                                          + 'to prevent the issuance of the requested payout (for example, do not issue cheque, ' &
																			 + 'stop payment, cancel cheque, general ledger postings, etc).' &
                                                          + '~r~n' &
                                                          + '~r~nWould you like to continue with the manual completion of this payout?',Question!,YesNo!,2)
END IF

IF li_msg_rtn = 1 THEN
	
	SQLCA.nf_begin_transaction()

	// update ANNUITY_PAYOUT
	UPDATE ANNUITY_PAYOUT
	SET    annuity_payout_status_code        = 'D',
			 annuity_payout_status_reason_code = 'MPS',
			 payout_comment                    = 'Manual intervention required.'
	WHERE  annuity_payout_no = :il_annuity_payout_no
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_payout', 'embedded SQL: UPDATE ANNUITY_PAYOUT...', 'cb_manual_annuity_payout.clicked')
	
	// set remainder of checklist to NRA
	ib_required_user_payout_comment = TRUE
	ib_no_payout = TRUE
	
	is_annuity_payout_reason_message = 'You have chosen to complete this annuity payout manually.'
	
	lnv_checklist = Create N_CHECKLIST
	
	IF is_checklist_type_code = 'PAA' THEN
		lnv_checklist = inv_prepare_checklist
		luo_checklist = uo_prepare_checklist
	ELSE
		lnv_checklist = inv_confirm_payout_checklist
		luo_checklist = uo_confirm_payout_checklist
	END IF
	
	li_next_checklist_step_no = lnv_checklist.nf_get_next_checklist_step(il_checklist_no)
	
	SELECT checklist_step_type_code
	INTO   :ls_checklist_step_type_code
	FROM   Checklist_Step_Type_Xref
	WHERE  checklist_step_no   = :li_next_checklist_step_no
	AND    checklist_type_code = :is_checklist_type_code
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_payout', 'embedded SQL: SELECT checklist_step_status_code FROM Checklist_Step_Type_Xref...', 'cb_manual_annuity_payout.clicked')
	
	IF is_checklist_type_code = 'PAA' THEN
		wf_reverse_confirm_OP_recovery()
		IF ls_checklist_step_type_code = '024' THEN
			ls_checklist_step_status_code = 'COA'
		ELSE
			ls_checklist_step_status_code = 'NRA'
		END IF
	ELSE
		IF ls_checklist_step_type_code = '024' THEN
			// request payout completed, it must be reversed
			// note that this includes reversal of OP recovery

			wf_reverse_request_payout()
			ls_checklist_step_status_code = 'COA'
		ELSE
			// must reverse OP recovery
			wf_reverse_confirm_OP_recovery()
			ls_checklist_step_status_code = 'NRA'
		END IF
	END IF
	
	ldw_dw = luo_checklist.tab_checklist.tabpage_checklist.dw_checklist
	
	l_dwo = ldw_dw.object.checklist_step_status_code
	
	// there will ultimately be a commit in this function call
	li_rtn = wf_auto_complete_next_step(ls_checklist_step_type_code,ls_checklist_step_status_code,ldw_dw,ls_checklist_step_status_code,'A',luo_checklist,lnv_checklist,lb_commit)
ELSE
	RETURN
END IF



end event

type cb_reset_sort from commandbutton within w_prepare_annuity_account
integer x = 1097
integer y = 740
integer width = 494
integer height = 104
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Reset Sort Order"
end type

event clicked;INTEGER  li_find
LONG     ll_current_individual_no
STRING   ls_sort


ll_current_individual_no = il_individual_no

ls_sort = 'annuity_admin_region_code ASC, annuity_end_date ASC, last_name ASC, given_names ASC, individual_no ASC'

dw_payout_accounts.SetSort(ls_sort)
dw_payout_accounts.Sort()


dw_payout_accounts.GroupCalc()

li_find = dw_payout_accounts.uf_find_row('individual_no',String(ll_current_individual_no))

IF li_find > 0 THEN
	IF li_find = 1 THEN
		// if the first row was originally chosen, then the RFC will not have fired
		// so, we have to trigger it so that all the datawindows (on tabs, in checklist object, etc) get retrieved.
		dw_payout_accounts.Trigger Event RowFocusChanged(1)
	END IF
	dw_payout_accounts.ScrollToRow(li_find)
	dw_payout_accounts.SelectRow(li_find,TRUE)
ELSE
	// could not find the individual in the annuity payout list
	// the person might have been removed from the list due to completion of payout
	// or might be filtered out.
	// so - re-retrieve datawindows for first row in list
	dw_payout_accounts.Trigger Event RowFocusChanged(1)
END IF

POST wf_post_hscrollbar_position()
end event

type dw_payout_region from u_datawindow within w_prepare_annuity_account
integer x = 55
integer y = 844
integer width = 4215
integer height = 120
integer taborder = 70
string dataobject = "d_annuity_admin_region"
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;settransobject(sqlca)
end event

type tab_payout from tab within w_prepare_annuity_account
event create ( )
event destroy ( )
event ue_tab_resize pbm_size
integer x = 46
integer y = 1012
integer width = 5646
integer height = 1600
integer taborder = 80
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
boolean raggedright = true
boolean focusonbuttondown = true
boolean boldselectedtext = true
integer selectedtab = 1
tabpage_participants tabpage_participants
tabpage_payout_summary tabpage_payout_summary
tabpage_payout_details tabpage_payout_details
tabpage_annuity_payout_txn_detail tabpage_annuity_payout_txn_detail
tabpage_overpayment tabpage_overpayment
tabpage_payout_recipients tabpage_payout_recipients
tabpage_contract_and_beneficiaries tabpage_contract_and_beneficiaries
end type

on tab_payout.create
this.tabpage_participants=create tabpage_participants
this.tabpage_payout_summary=create tabpage_payout_summary
this.tabpage_payout_details=create tabpage_payout_details
this.tabpage_annuity_payout_txn_detail=create tabpage_annuity_payout_txn_detail
this.tabpage_overpayment=create tabpage_overpayment
this.tabpage_payout_recipients=create tabpage_payout_recipients
this.tabpage_contract_and_beneficiaries=create tabpage_contract_and_beneficiaries
this.Control[]={this.tabpage_participants,&
this.tabpage_payout_summary,&
this.tabpage_payout_details,&
this.tabpage_annuity_payout_txn_detail,&
this.tabpage_overpayment,&
this.tabpage_payout_recipients,&
this.tabpage_contract_and_beneficiaries}
end on

on tab_payout.destroy
destroy(this.tabpage_participants)
destroy(this.tabpage_payout_summary)
destroy(this.tabpage_payout_details)
destroy(this.tabpage_annuity_payout_txn_detail)
destroy(this.tabpage_overpayment)
destroy(this.tabpage_payout_recipients)
destroy(this.tabpage_contract_and_beneficiaries)
end on

event ue_tab_resize;tabpage_participants.dw_participants.Width  = THIS.Width - 200
tabpage_participants.dw_participants.Height = THIS.Height - tab_payout.tabpage_participants.dw_participant_details.Height - 500

tabpage_participants.dw_participant_details.Width  = THIS.Width - 200


idw_annuity_payout_summary.Height = THIS.Height - 400
idw_annuity_payout_summary.Width  = THIS.Width - 200


idw_annuity_payout_detail.Height = THIS.Height - 400
idw_annuity_payout_detail.Width  = THIS.Width - 200


idw_annuity_payout_txn_detail.Height = THIS.Height - 400 
idw_annuity_payout_txn_detail.Width  = tab_payout.Width - 200


idw_payout_recipients.Height = (THIS.Height - 300)*(.7)  // about three quarters of tab height should be for recipient tab
idw_payout_recipients.Width  = THIS.Width - 200
idw_payout_recipient_details.Width  = THIS.Width - 200


idw_annuity_overpayment.Height = THIS.Height - 400
idw_annuity_overpayment.Width  = THIS.Width - 200


idw_contract_beneficiaries.Height = THIS.Height - 700
idw_contract_beneficiaries.Width  = THIS.Width - 200

idw_contract_beneficiaries_details.Height = idw_contract_beneficiaries.Height - 330
idw_contract_beneficiaries_details.Width  = THIS.Width - 200

tabpage_contract_and_beneficiaries.cb_save_contract.y = THIS.Height - 400
tabpage_contract_and_beneficiaries.cb_add_contract.y = THIS.Height - 400
tabpage_contract_and_beneficiaries.cb_add_beneficiary.y = THIS.Height - 400
tabpage_contract_and_beneficiaries.cb_delete_contract.y = THIS.Height - 400
tabpage_contract_and_beneficiaries.cb_delete_beneficiary.y = THIS.Height - 400
tabpage_contract_and_beneficiaries.cb_cancel_contract.y = THIS.Height - 400
tabpage_contract_and_beneficiaries.st_contract_save_needed.y = THIS.Height - 400

tab_payout.tabpage_payout_recipients.st_recipient_save_needed.y = tab_payout.tabpage_payout_recipients.cb_cancel_annuity_payout_recipient.y
end event

event selectionchanged;wf_retrieve_tab(newindex)

// if this is an annuity purchase, and the selected tab is 'RECIPIENTS' 
// and the selected record is for WHSCC (overpayment), then select next record if there is one.

IF is_checklist_type_code = 'CAPAP' AND newindex = RECIPIENTS THEN	
	IF idw_payout_recipients.RowCount() > 1 THEN
		// more than one recipient
		IF idw_payout_recipients.GetItemString(1,'annuity_payout_recipient_type_code') = 'O' AND idw_payout_recipients.GetItemNumber(1,'annuity_payout_recipient_no') = 7 THEN
			// first record is overpayment recovery for WHSCC
			// so select second record
			idw_payout_recipients.ScrollToRow(2)
			idw_payout_recipients.SelectRow(2,FALSE)
		END IF
	END IF	
END IF
end event

event selectionchanging;IF ib_save THEN
	MessageBox("Save Required","You have pending changes that need to be saved. Please Save or Cancel")
	RETURN 1
END IF
end event

type tabpage_participants from userobject within tab_payout
integer x = 18
integer y = 108
integer width = 5609
integer height = 1476
long backcolor = 67108864
string text = "Participants"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
cb_cancel cb_cancel
cb_save cb_save
cb_delete cb_delete
cb_new cb_new
cb_add cb_add
cb_details cb_details
dw_participant_details dw_participant_details
dw_participants dw_participants
end type

on tabpage_participants.create
this.cb_cancel=create cb_cancel
this.cb_save=create cb_save
this.cb_delete=create cb_delete
this.cb_new=create cb_new
this.cb_add=create cb_add
this.cb_details=create cb_details
this.dw_participant_details=create dw_participant_details
this.dw_participants=create dw_participants
this.Control[]={this.cb_cancel,&
this.cb_save,&
this.cb_delete,&
this.cb_new,&
this.cb_add,&
this.cb_details,&
this.dw_participant_details,&
this.dw_participants}
end on

on tabpage_participants.destroy
destroy(this.cb_cancel)
destroy(this.cb_save)
destroy(this.cb_delete)
destroy(this.cb_new)
destroy(this.cb_add)
destroy(this.cb_details)
destroy(this.dw_participant_details)
destroy(this.dw_participants)
end on

type cb_cancel from commandbutton within tabpage_participants
integer x = 2917
integer y = 1308
integer width = 343
integer height = 104
integer taborder = 150
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Cancel"
end type

event clicked;datawindowchild ldw_child

IF ib_save = TRUE THEN
	dw_payout_accounts.Enabled = TRUE
	ib_save = FALSE
END IF

wf_enable_controls(true)
wf_filter_participant_dddws(FALSE)

dw_participant_details.reset()
dw_participants.reset()
inv_annuity_participant.nf_reset()
dw_participants.retrieve(il_annuity_payout_no)
SQLCA.nf_handle_error('w_prepare_annuity_account','dw_participants.retrieve','cb_cancel.clicked')
dw_participants.event dynamic rowfocuschanged (dw_participants.getRow()) // re-retrieve the data for the details
dw_participant_details.object.recipient_type_code.protect = 1
dw_participant_details.object.annuity_role_code.protect = 1
dw_participant_details.object.represented_by_recipient_no.protect = 0

// cancel filter on the recipient type code 
dw_participant_details.getChild("recipient_type_code", ldw_child)
ldw_child.setFilter("")
ldw_child.filter()

wf_enable_participant_buttons(true)
cb_details.enabled = TRUE

idw_participant_details.Object.t_save_needed.Visible = FALSE
idw_participants.Enabled = TRUE
end event

type cb_save from commandbutton within tabpage_participants
integer x = 2542
integer y = 1308
integer width = 343
integer height = 104
integer taborder = 150
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
end type

event clicked;	
INT li_rtn, li_trancount
LONG ll_row

ll_row = idw_participants.getrow()	

// call inv_annuity_participant.nf_save(), which first calls the validation, check business rule,  
// and update functions and then the commit function if everything passes

IF ib_save = TRUE THEN		
	SQLCA.nf_begin_transaction()
	
	li_rtn = inv_annuity_participant.nf_save()
	IF li_rtn >= 0 THEN
		SQLCA.nf_commit_transaction()
		
		dw_payout_accounts.Enabled = TRUE
		ib_save = FALSE
	ELSE
		SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
		IF li_trancount > 0 THEN
			SQLCA.nf_rollback_transaction()
		END IF
	END IF
END IF	

//trigger the cancel button, as this will do the rest of the cleanup and restoration to make window ready for next entry
cb_cancel.TriggerEvent(CLICKED!)

end event

type cb_delete from commandbutton within tabpage_participants
integer x = 2167
integer y = 1308
integer width = 343
integer height = 104
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Delete"
end type

event clicked;LONG ll_row, ll_delete_count
INT li_rtn, li_trancount


IF ib_save = true then
	MESSAGEBOX("Save Required","You have pending changes that need to be saved. Please Save or Cancel")
	Return
END IF

ll_row = dw_participants.getRow()

IF ll_row < 1 THEN Return

IF MESSAGEBOX("Confirm Delete","Are you sure you want to delete this annuity participant?", Information!, YesNo!,2) = 1 THEN
	// set up the deleted il participant no, so it can be checked in nf check bus rules 
	// because after the call to deleterow(), the DW refocuses on the one of the remaining rows, and il_participant_no changes to that participant
	inv_annuity_participant.il_deleted_participant_no = inv_annuity_participant.il_participant_no
	
	dw_participants.deleterow(ll_row)
	SQLCA.nf_handle_error('w_prepare_annuity_account','dw_participants.deleterow','cb_delete.clicked')
	
	SQLCA.nf_begin_transaction()
	
	li_rtn = inv_annuity_participant.nf_save()
	
	// if save not successful, trigger the cancel which will rollback and refresh
	IF li_rtn < 0 THEN
		SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
		IF li_trancount > 0 THEN
			SQLCA.nf_rollback_transaction()
		END IF
		
		cb_cancel.triggerEvent(Clicked!)
	ELSE
		SQLCA.nf_commit_transaction()
	END IF
	
	tab_payout.event dynamic selectionChanged(1,1)   // refresh the datawindow
	
END IF

end event

type cb_new from commandbutton within tabpage_participants
integer x = 1294
integer y = 1308
integer width = 782
integer height = 104
integer taborder = 140
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Add Annuity Participant"
end type

event clicked;long ll_current_row

IF ib_save = true then
	MESSAGEBOX("Save Required","You have pending changes that need to be saved. Please Save or Cancel")
	Return
END IF

ll_current_row = dw_participant_details.insertrow(0)
SQLCA.nf_handle_error('w_prepare_annuity_account','dw_participant_details.insertrow','cb_new.clicked')
		
dw_participant_details.setRow(ll_current_row)
dw_participant_details.scrollToRow(ll_current_row)


dw_participant_details.object.annuity_role_code.protect = 0
dw_participant_details.object.represented_by_recipient_no.protect = 1

dw_payout_accounts.Enabled = FALSE
ib_save = true

wf_enable_controls(false)
wf_filter_participant_dddws(TRUE)
wf_enable_participant_buttons(false)
end event

type cb_add from commandbutton within tabpage_participants
integer x = 521
integer y = 1308
integer width = 741
integer height = 104
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Link Claim Participant"
end type

event clicked;
s_window_message	lstr_message
INT li_cntr, li_cntr2, li_rows, li_current_row, li_rtn, li_found, li_row, li_trancount
LONG ll_individual_no
STRING ls_annuity_role_code, ls_recipient_type_code, ls_benefit_holder_name


IF ib_save = true then
	MESSAGEBOX("Save Required","You have pending changes that need to be saved. Please Save or Cancel.")
	Return
END IF

wf_enable_participant_buttons(false)

// get the name of the benefit holder, (who is identified by the value in il_individual_no), not the name of the currently selected individual in the participant list
li_found = dw_participants.find('recipient_no = ' +STRING( il_individual_no), 1, dw_participants.rowCount() )

istr_CAE_message.as_stringparm[1] = dw_participants.getItemString(li_found,'recipient_name')

istr_CAE_message.al_doubleparm[1] =il_individual_no
istr_CAE_message.al_doubleparm[2] =il_annuity_account_no
istr_CAE_message.al_doubleparm[3] =il_annuity_payout_no


OpenWithParm(w_claim_participant_list, istr_CAE_message, iw_win)

//retrieve the returned message object into a local structure object, and check for values
lstr_message = Message.PowerObjectParm
IF IsValid(lstr_message) then
	li_rows = UpperBound(lstr_message.al_doubleparm)
	
	IF li_rows > 0 THEN
		IF MESSAGEBOX("Confirm Add Participants", "Are you sure you want to add the selected annuity participants?", Information!, YesNo!,  2) = 1 THEN 
			
			SQLCA.nf_begin_transaction()
			
			FOR li_cntr = 1 to li_rows
				inv_annuity_participant.is_participant_status = 'NEW' 
				inv_annuity_participant.is_individual_status   = 'NOTMODIFIED'
				li_row = inv_annuity_participant.idw_datawindow[2].insertRow(0)
				SQLCA.nf_handle_error('w_prepare_annuity_account','inv_annuity_participant.idw_datawindow[2].insertRow(0)','cb_add.clicked')
				
				inv_annuity_participant.idw_datawindow[2].setRow(li_row)
				
				ll_individual_no = lstr_message.al_doubleparm[li_cntr]
				
				li_cntr2++
				ls_annuity_role_code = lstr_message.as_stringparm[li_cntr2 ]
				
				li_cntr2++
				ls_recipient_type_code = lstr_message.as_stringparm[li_cntr2]
				
				inv_annuity_participant.il_participant_no = ll_individual_no
				inv_annuity_participant.il_annuity_account_no = il_annuity_account_no
				inv_annuity_participant.il_annuity_payout_no = il_annuity_payout_no
				inv_annuity_participant.is_annuity_role_code = ls_annuity_role_code
				inv_annuity_participant.is_recipient_type_code =ls_recipient_type_code
				inv_annuity_participant.il_represented_by_recipient_no = ll_individual_no
				inv_annuity_participant.is_represented_by_recipient_type_code = ls_recipient_type_code
				
				// save each one as we go
				li_rtn = inv_annuity_participant.nf_save()
				
				IF li_rtn < 0 and li_rows > 1 THEN
					SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
					IF li_trancount > 0 THEN
						SQLCA.nf_rollback_transaction()
					END IF
					MESSAGEBOX("ERROR", "There was an error adding one of the Claim Participants as an annuity participant. ~rSome or All of the selected participants have NOT been saved.",EXCLAMATION!)
					cb_cancel.triggerEvent(Clicked!)
					EXIT
				END IF 	
			NEXT
			
			SQLCA.nf_commit_transaction()
			
			ib_save = false
			
			tab_payout.event dynamic selectionChanged(1,1)   // refresh the datawindow
			dw_participants.setrow(dw_participants.rowcount())
			dw_participants.scrolltorow(dw_participants.rowcount())   // scroll to last row (the one just added)		
				
		END IF
	END IF
END IF

inv_annuity_participant.nf_reset()
wf_enable_participant_buttons(true)



end event

type cb_details from commandbutton within tabpage_participants
integer x = 119
integer y = 1308
integer width = 347
integer height = 104
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Details..."
end type

event clicked;LONG   ll_recipient_no, ll_claim_no, ll_row, ll_count
STRING ls_recipient_type_code, ls_provider_type_code, ls_provider_sub_type_code, ls_annuity_role_code
STRING ls_provider_name, ls_provider_city, ls_provider_prov_state_code
STRING ls_annuity_eligibility_confirmed_flag, ls_overpayment_recovery_confirmed_flag

IF idw_participant_details.rowcount() < 1 THEN RETURN

ls_recipient_type_code = inv_annuity_participant.is_recipient_type_code


IF ls_recipient_type_code = 'I' THEN

	istr_CAE_message.al_doubleparm[1] = il_annuity_account_claim_no
	istr_CAE_message.al_doubleparm[2] = inv_annuity_participant.il_participant_no
	istr_CAE_message.as_stringparm[1] = 'MOD'
	
	ls_annuity_eligibility_confirmed_flag = dw_payout_accounts.getItemString( dw_payout_accounts.getRow(), 'annuity_eligibility_confirmed_flag')
	ls_overpayment_recovery_confirmed_flag = dw_payout_accounts.getItemString( dw_payout_accounts.getRow(), 'overpayment_recovery_confirmed_flag')

	SELECT  count(*)
	INTO    :ll_count
	FROM    CLAIM_PARTICIPANT
	WHERE  individual_no = :inv_annuity_participant.il_participant_no
	USING   SQLCA;
	SQLCA.nf_handle_error("w_prepare_annuity_account","cb_details.clicked", "Select count from CLAIM_PARTICIPANT")

	
	IF ll_count > 0 OR  ls_annuity_eligibility_confirmed_flag = 'N' OR ls_overpayment_recovery_confirmed_flag = 'Y'  THEN
		istr_CAE_message.as_mode = "READ"
	ELSE
		istr_CAE_message.as_mode = ""
	END IF
	
	// all the rest of the necessary data is in inv_annuity_participant, populated on rowfocuschanged event of dw_participants
	istr_CAE_message.apo_powerobjectparm[1] = inv_annuity_participant
	OpenWithParm(w_add_annuity_participant, istr_CAE_message, iw_win)	
	
ELSEIF  ls_recipient_type_code  = 'O'  THEN
	ll_recipient_no = inv_annuity_participant.il_participant_no
	istr_CAE_message.al_doubleparm[1] = ll_recipient_no
	istr_CAE_message.al_doubleparm[2] = 1   // exact match on name
	istr_CAE_message.al_doubleparm[3] = ll_recipient_no
	
	SELECT provider_type_code,
	       provider_sub_type_code,
			 name,
			 city
	INTO   :ls_provider_type_code,
	       :ls_provider_sub_type_code,
			 :ls_provider_name,
			 :ls_provider_city
	FROM   PROVIDER
	WHERE  provider_no        = :ll_recipient_no
	AND    provider_type_code = 'O'
	USING SQLCA;
	
	SQLCA.nf_handle_error("w_prepare_annuity_account", "tabpage_participants.cb_details","SELECT FROM PROVIDER")
	
	istr_CAE_message.as_stringparm[1] = 'B'
	istr_CAE_message.as_stringparm[2] = ls_provider_type_code
	istr_CAE_message.as_stringparm[3] = ls_provider_sub_type_code
	istr_CAE_message.as_stringparm[4] = ''//ls_provider_name
	istr_CAE_message.as_stringparm[5] = ''//ls_provider_city
	istr_CAE_message.as_stringparm[6] = 'M'   // main name
	istr_CAE_message.as_mode = 'READ'
	
	OpenSheetWithParm(w_service_provider,istr_CAE_message,w_frame,0,Layered!)
END IF

inv_annuity_participant.nf_reset()

IF message.stringParm <> 'CANCEL' THEN
	//refresh
	ll_row = idw_participants.getrow()		
	idw_participants.retrieve(il_annuity_payout_no)	
	SQLCA.nf_handle_error('w_prepare_annuity_account','idw_participants.retrieve','cb_details.clicked')
	idw_participants.event dynamic rowfocuschanged(ll_row)
	idw_participants.scrolltorow(ll_row)
END IF


end event

type dw_participant_details from u_datawindow within tabpage_participants
integer x = 41
integer y = 240
integer width = 4343
integer height = 1044
integer taborder = 11
string dataobject = "d_annuity_participant_detail"
borderstyle borderstyle = stylelowered!
end type

event itemchanged;call super::itemchanged;
LONG    ll_row_found, ll_row, ll_rows, ll_represented_by_recipient_no
INT      li_protect_value
STRING ls_recipient_type_code, ls_annuity_role_code, ls_found_exp, ls_represented_by_name_and_address
DWItemStatus ldw_status
Datawindowchild ldw_child

if row > 0 THEN
	this.accepttext()
	wf_enable_controls(false)
	wf_filter_participant_dddws(TRUE)
	CHOOSE CASE dwo.name		
		CASE 'represented_by_recipient_no'				
			ldw_status = this.getItemStatus(row,'represented_by_recipient_no',PRIMARY!)			
			ll_row_found = idw_participants.find('recipient_no = ' + data, 1,idw_participants.rowcount() )
			IF ll_row_found < 1 then 
				messagebox("no data","Could not locate the value for the recipient_type_code")
				return
			END IF
			ls_recipient_type_code = idw_participants.getItemString(ll_row_found,'recipient_type_code')
			this.setItem(row, 'represented_by_recipient_type_code', ls_recipient_type_code)
			
			ll_represented_by_recipient_no = LONG(data)
			
			IF ls_recipient_type_code = 'I' THEN
				SELECT given_names + ' ' + last_name + CHAR(13) + 
                   address_line1   + CHAR(13) + 
                   case when len(address_line2) >  0 then address_line2 + CHAR(13) else '' end +
						 city            + CHAR(13) + 
                   prov_state_code + '  ' + postal_code
				INTO :ls_represented_by_name_and_address
				FROM  INDIVIDUAL
				WHERE individual_no = :ll_represented_by_recipient_no
				USING SQLCA;
				SQLCA.nf_handle_error('w_prepare_annuity_account','SELECT...FROM INDIVIDUAL','dw_participant_details.itemchanged')
			ELSE
				SELECT name            + CHAR(13) + 
                   address_line1   + CHAR(13) + 
                   case when len(address_line2) >  0 then address_line2 + CHAR(13) else '' end +
                   city            + CHAR(13) + 
                   prov_state_code + '  ' + postal_code   
				INTO  :ls_represented_by_name_and_address
				FROM   PROVIDER
				WHERE  provider_no        = :ll_represented_by_recipient_no
				AND    provider_type_code = 'O'
				USING SQLCA;
				SQLCA.nf_handle_error('w_prepare_annuity_account','SELECT...FROM INDIVIDUAL','dw_participant_details.itemchanged')
			END IF
			
			this.setItem(row, 'represented_by_name_and_address', ls_represented_by_name_and_address)
			
			// do an update (no commit yet) and force a rowfocuschanged event on dw_participants which will re-retrieve 
			// the data again to display the represented-by name and address
//			this.update()
//			SQLCA.nf_handle_error('w_prepare_annuity_account','dw_participant_details.update()','dw_participant_details.itemchanged')
//			ll_row = dw_participants.getrow()
//			
//			dw_participants.retrieve(il_annuity_payout_no)	
//			SQLCA.NF_Handle_error('w_prepare_annuity_account','dw_participants.retrieve','dw_participants.itemchanged')
//			dw_participants.event dynamic rowfocuschanged(ll_row)
//			dw_participants.scrolltorow(ll_row)
			
			THIS.Object.t_save_needed.Visible = TRUE
			idw_participants.Enabled = FALSE
			
			//reset the item status, because it gets set to NotModified by the update statement above, 
			// but we need to check it in nf_check_bus_rule() during a save
			this.setItemStatus(row, 'represented_by_recipient_no', PRIMARY!, ldw_status)
		
	CASE 'annuity_role_code'
		// set up a filter on the recipient type code based on the annuity role code selected
		ls_annuity_role_code = this.getItemString(row, 'annuity_role_code')
		IF ls_annuity_role_code > "" THEN
			this.getChild("recipient_type_code", ldw_child)
			ldw_child.setFilter("annuity_role_code = '" + ls_annuity_role_code + "' AND active_flag = 'Y'")
			ldw_child.filter()
			ll_row = ldw_child.getRow()
			IF ll_row > 0 THEN
				ls_recipient_type_code = ldw_child.getItemString(ll_row, 'recipient_type_code')
			END IF
			
			IF ldw_child.rowCount() = 1 THEN
				this.setItem(row,'recipient_type_code', ls_recipient_type_code)
			ELSE
				this.setItem(row,'recipient_type_code', "")
			END IF
			
			dw_participant_details.object.recipient_type_code.protect = 0
			

			//NEED to exercise this rule on the way in, to prevent user from going thru all the data entry and getting the error on save
			//BR1.60	  There must be, at most, one annuity participant in the annuity role of dependent spouse for an annuity payout. 
			ldw_status = this.getItemStatus(this.getRow(), 0, PRIMARY!)
			IF ldw_status = New! or ldw_status = NewModified! THEN
				ls_found_exp = "annuity_role_code = '01'"
				ll_row_found = idw_participants.find(ls_found_exp, 1, idw_participants.rowCount())
				IF ls_annuity_role_code = '01' and ll_row_found > 0 THEN
					MESSAGEBOX("BR1.60", "There must be, at most, one annuity participant in the annuity role of dependent spouse for an annuity payout.", EXCLAMATION!)
					RETURN -1
				END IF	
			END IF

		END IF
		
	CASE 'annuity_payout_writeoff_flag'
		THIS.Object.t_save_needed.Visible = TRUE
		idw_participants.Enabled = FALSE
		
	CASE 'annuity_payout_garnish_flag'
		THIS.Object.t_save_needed.Visible = TRUE
		idw_participants.Enabled = FALSE
		
	END CHOOSE
	
	ib_save = TRUE

	wf_enable_participant_buttons(FALSE)
	cb_details.Enabled = FALSE
	
END IF
		
	
end event

event constructor;call super::constructor;settransobject(sqlca)

end event

event buttonclicked;call super::buttonclicked;
STRING ls_recipient_type_code, ls_annuity_role_code, ls_provider_type_code, ls_name
Long     ll_provider_no, ll_recipient_no
INT      li_current_row, li_rtn

ib_save = FALSE

IF dwo.name = 'b_participant_search' and this.getItemStatus(row,0,Primary!) = NewModified!  THEN
	ls_recipient_type_code = this.getItemString(row,'recipient_type_code')
	ls_annuity_role_code = this.getItemString(row,'annuity_role_code')
	
	IF ls_annuity_role_code = ''     OR isNull(ls_annuity_role_code) &
	OR ls_recipient_type_code = ''  OR isNull(ls_recipient_type_code) THEN RETURN
	
	inv_annuity_participant.il_participant_no = 0 // we're adding a new one, so set this to 0, as the new particpant hasn't been selected yet
	inv_annuity_participant.is_annuity_role_code = ls_annuity_role_code
	inv_annuity_participant.is_recipient_type_code = ls_recipient_type_code
		
	IF ls_recipient_type_code = 'I' THEN
		istr_CAE_message.al_doubleparm[1] = 0  // claim_no    - not needed
		istr_CAE_message.al_doubleparm[2] = 0  // individual no, preset to 0 for now
		istr_CAE_message.as_stringparm[1] = 'ADD'
		istr_CAE_message.as_mode = ''
		//the rest of the necessary data is in inv_annuity_participant, updated on each dw_participants.row focuschanged event
		istr_CAE_message.apo_powerobjectparm[1] = inv_annuity_participant
		
		
		OpenWithParm(w_add_annuity_participant, istr_CAE_message, iw_win)
		IF message.StringParm = "ROLLBACK"	THEN
			cb_cancel.triggerEvent(CLICKED!)
		END IF
		tab_payout.event dynamic selectionChanged(1,1)   // refresh the datawindow
		
	ELSEIF ls_recipient_type_code = 'O' THEN
		istr_CAE_message.as_stringparm[1] = ''
		istr_CAE_message.as_stringparm[2] = ''
		istr_CAE_message.as_stringparm[3] = ''
		istr_CAE_message.as_mode = ''		

		OPENWithPARM(w_service_provider_search,'O', iw_win)
		istr_CAE_message = MESSAGE.PowerObjectParm
		ll_provider_no = istr_CAE_message.al_doubleparm[1]
		ls_provider_type_code = istr_CAE_message.as_stringparm[1]
		
		IF ll_provider_no > 0 THEN
			
			SELECT name
			INTO   :ls_name
			FROM   PROVIDER
			WHERE  provider_no        = :ll_provider_no
			AND    provider_type_code = :ls_provider_type_code
			USING SQLCA;
			SQLCA.nf_handle_error("w_prepare_annuity_account","dw_participant_details.buttonclicked event","Select name from PROVIDER")
				
			IF MESSAGEBOX("Add Provider Participant","Do you wish to add provider: " + ls_name + " as an annuity participant?", Information!,YesNo!,2) = 1 THEN
				
				
				
				//check BR1.130    The provider must be of type ‘Other’, if the participant is of recipient type other payee. (Refer to Rationale)
				// but in reallity, it would be impossible to have any provider type other than 'O' since the provider search window was opened with a parameter of 'O' which locks in that provider type
				IF ls_provider_type_code <> 'O' THEN
					MESSAGEBOX("BR1.130", "The provider must be of type ‘Other’, if the participant's recipient type is 'Other Payee'")
					cb_cancel.triggerEvent(CLICKED!)
					RETURN
				END IF	
				
				li_current_row = idw_participant_details.getRow()
				
				inv_annuity_participant.il_participant_no = ll_provider_no
				inv_annuity_participant.is_annuity_role_code = ls_annuity_role_code
				inv_annuity_participant.is_recipient_type_code = ls_recipient_type_code
				inv_annuity_participant.is_participant_status = 'NEW'
				
				// temporary set this item, so it can be checked in nf_check_bus_rule when participant being added is 'Estate'
				idw_participant_details.setItem(li_current_row,'recipient_name_and_address',ls_name + CHAR(13)) 
				
				SQLCA.nf_begin_transaction()
				
				li_rtn = inv_annuity_participant.nf_save()
				IF li_rtn < 0 THEN
					SQLCA.nf_rollback_transaction()
					cb_cancel.triggerEvent(CLICKED!)
				ELSE
					SQLCA.nf_commit_transaction()
				END IF
			END IF
		END IF
	END IF
	
	ib_save = false
	
	tab_payout.event dynamic selectionChanged(1,1)   // refresh the datawindow
	dw_participants.setrow(dw_participants.rowcount())
	dw_participants.scrolltorow(dw_participants.rowcount())   // scroll to last row (the one just added)
	
	wf_enable_participant_buttons(true)
	wf_enable_controls(true)
	wf_filter_participant_dddws(FALSE)
	inv_annuity_participant.nf_reset()
	dw_participant_details.object.recipient_type_code.protect = 1
	dw_participant_details.object.annuity_role_code.protect = 1
	dw_participant_details.object.represented_by_recipient_no.protect = 0
	
END IF



end event

type dw_participants from u_datawindow within tabpage_participants
integer x = 41
integer y = 20
integer width = 4343
integer height = 208
integer taborder = 30
string dataobject = "d_annuity_participant_list"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
boolean righttoleft = true
end type

event constructor;call super::constructor;settransobject(sqlca)
this.uf_setSelect(1)


end event

event rowfocuschanged;call super::rowfocuschanged;
long   ll_participant_no, ll_row, ll_individual_no, ll_represented_by_recipient_no
string ls_annuity_role_code, ls_recipient_type_code, ls_represented_by_recipient_type_code
INT   li_rtn
datawindowchild ldwc_represented_by
dwobject ldwo

IF ib_save THEN
	// do not retrieve
ELSE
	IF currentrow > 0 THEN
		ll_participant_no = this.getItemNumber(currentrow,'recipient_no')
		ll_individual_no = this.getItemNumber(currentrow,'individual_no')
		ls_annuity_role_code = this.getitemString(currentrow,'annuity_role_code')
		ls_recipient_type_code = this.getitemString(currentrow,'recipient_type_code')
		ll_represented_by_recipient_no = this.getitemNumber(currentrow,'represented_by_recipient_no')
		ls_represented_by_recipient_type_code = this.getitemString(currentrow,'represented_by_recipient_type_code')
	
		li_rtn = idw_participant_details.getChild('represented_by_recipient_no', ldwc_represented_by)
		IF li_rtn <> 1 THEN
			MESSAGEBOX("error","Not a datawindowchild, rowfocuschanged in dw_participants")
			return
		END IF
		
		ll_row = idw_participant_details.retrieve(il_annuity_payout_no, ll_participant_no, ls_annuity_role_code, ls_recipient_type_code)
		SQLCA.nf_handle_error('w_prepare_for_annuity_account','tab_payout.tabpage_participants.rowfocuschanged','dw_participant_details.retrieve')
		
		ldwc_represented_by.setTransObject(SQLCA)
		
		ldwc_represented_by.retrieve(il_annuity_payout_no)
		SQLCA.nf_handle_error('w_prepare_for_annuity_account','tab_payout.tabpage_participants.rowfocuschanged','dw_participant_details_datawindowchild.retrieve')
		
		
		inv_annuity_participant.il_annuity_account_no = il_annuity_account_no
		inv_annuity_participant.il_annuity_payout_no = il_annuity_payout_no
		inv_annuity_participant.il_benefit_holder_no = ll_individual_no
		inv_annuity_participant.il_participant_no = ll_participant_no
		inv_annuity_participant.is_annuity_role_code = ls_annuity_role_code
		inv_annuity_participant.is_recipient_type_code = ls_recipient_type_code
		inv_annuity_participant.il_represented_by_recipient_no = ll_represented_by_recipient_no
		inv_annuity_participant.is_represented_by_recipient_type_code = ls_represented_by_recipient_type_code
		
	END IF
END IF
end event

event resize;call super::resize;//idw_participant_details.Height = THIS.Height*2.5
idw_participant_details.y = THIS.y + THIS.Height + 20

tab_payout.tabpage_participants.cb_details.y = idw_participant_details.y + idw_participant_details.Height + 20
tab_payout.tabpage_participants.cb_add.y     = idw_participant_details.y + idw_participant_details.Height + 20
tab_payout.tabpage_participants.cb_new.y     = idw_participant_details.y + idw_participant_details.Height + 20
tab_payout.tabpage_participants.cb_delete.y  = idw_participant_details.y + idw_participant_details.Height + 20
tab_payout.tabpage_participants.cb_save.y    = idw_participant_details.y + idw_participant_details.Height + 20
tab_payout.tabpage_participants.cb_cancel.y  = idw_participant_details.y + idw_participant_details.Height + 20
end event

event rowfocuschanging;call super::rowfocuschanging;
IF ib_save THEN
	MESSAGEBOX("Save Required", "You have pending changes. Please Save or Cancel first.")
	SelectRow(newrow, FALSE)
   SelectRow(currentrow, TRUE)
	// prevent row change
	RETURN 1
END IF

end event

type tabpage_payout_summary from userobject within tab_payout
event create ( )
event destroy ( )
integer x = 18
integer y = 108
integer width = 5609
integer height = 1476
long backcolor = 67108864
string text = "Payout Summary"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_annuity_payout_summary dw_annuity_payout_summary
end type

on tabpage_payout_summary.create
this.dw_annuity_payout_summary=create dw_annuity_payout_summary
this.Control[]={this.dw_annuity_payout_summary}
end on

on tabpage_payout_summary.destroy
destroy(this.dw_annuity_payout_summary)
end on

type dw_annuity_payout_summary from u_datawindow within tabpage_payout_summary
integer x = 32
integer y = 52
integer width = 4562
integer height = 1960
integer taborder = 30
string dataobject = "d_annuity_payout_summary"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;settransobject(sqlca)
end event

event editchanged;call super::editchanged;DECIMAL ldec_overpayment_recovery_amount, ldec_sub_total_annuity_payout_amount, ldec_diff

IF dwo.name = 'overpayment_recovery_amount' THEN
	// change value of net annuity payment amount
	ldec_overpayment_recovery_amount = Dec(data)
	ldec_sub_total_annuity_payout_amount = THIS.GetItemDecimal(row,'sub_total_annuity_payout_amount')
	
	ldec_diff = ldec_sub_total_annuity_payout_amount - ldec_overpayment_recovery_amount
	
	THIS.SetItem(row,'net_annuity_payment_amount',ldec_diff)
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


lm_popup.m_options.PopMenu(iw_win.PointerX( ), iw_win.PointerY( ))

Destroy lm_popup
end event

event ue_print;THIS.Object.DataWindow.Print.Orientation = 1 //landscape
THIS.Print()
end event

type tabpage_payout_details from userobject within tab_payout
integer x = 18
integer y = 108
integer width = 5609
integer height = 1476
long backcolor = 67108864
string text = "Payout Details"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_annuity_payout_detail dw_annuity_payout_detail
end type

on tabpage_payout_details.create
this.dw_annuity_payout_detail=create dw_annuity_payout_detail
this.Control[]={this.dw_annuity_payout_detail}
end on

on tabpage_payout_details.destroy
destroy(this.dw_annuity_payout_detail)
end on

type dw_annuity_payout_detail from u_datawindow within tabpage_payout_details
integer x = 50
integer y = 52
integer width = 4713
integer height = 1960
integer taborder = 11
string dataobject = "d_annuity_payout_claim_detail"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;settransobject(sqlca)
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

lm_popup.m_options.PopMenu(iw_win.PointerX( ), iw_win.PointerY( ))

Destroy lm_popup
end event

event ue_print;THIS.Object.DataWindow.Print.Orientation = 1 //landscape
THIS.Print()
end event

type tabpage_annuity_payout_txn_detail from userobject within tab_payout
event create ( )
event destroy ( )
integer x = 18
integer y = 108
integer width = 5609
integer height = 1476
long backcolor = 67108864
string text = "Claim Transaction Details"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_annuity_payout_txn_detail dw_annuity_payout_txn_detail
end type

on tabpage_annuity_payout_txn_detail.create
this.dw_annuity_payout_txn_detail=create dw_annuity_payout_txn_detail
this.Control[]={this.dw_annuity_payout_txn_detail}
end on

on tabpage_annuity_payout_txn_detail.destroy
destroy(this.dw_annuity_payout_txn_detail)
end on

type dw_annuity_payout_txn_detail from u_datawindow within tabpage_annuity_payout_txn_detail
integer x = 46
integer y = 52
integer width = 4681
integer height = 1960
integer taborder = 11
string dataobject = "d_annuity_payout_txn_detail_no_dependants"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;settransobject(sqlca)
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

lm_popup.m_options.PopMenu(iw_win.PointerX( ), iw_win.PointerY( ))

Destroy lm_popup
end event

event ue_print;THIS.Object.DataWindow.Print.Orientation = 1 //landscape
THIS.Print()
end event

type tabpage_overpayment from userobject within tab_payout
integer x = 18
integer y = 108
integer width = 5609
integer height = 1476
long backcolor = 67108864
string text = "Overpayment Recovery"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_annuity_overpayment dw_annuity_overpayment
end type

on tabpage_overpayment.create
this.dw_annuity_overpayment=create dw_annuity_overpayment
this.Control[]={this.dw_annuity_overpayment}
end on

on tabpage_overpayment.destroy
destroy(this.dw_annuity_overpayment)
end on

type dw_annuity_overpayment from u_datawindow within tabpage_overpayment
integer x = 46
integer y = 52
integer width = 4750
integer height = 1960
integer taborder = 11
string dataobject = "d_annuity_overpayment"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;settransobject(sqlca)
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

lm_popup.m_options.PopMenu(iw_win.PointerX( ), iw_win.PointerY( ))

Destroy lm_popup
end event

event ue_print;THIS.Object.DataWindow.Print.Orientation = 1 //landscape
THIS.Print()
end event

type tabpage_payout_recipients from userobject within tab_payout
integer x = 18
integer y = 108
integer width = 5609
integer height = 1476
long backcolor = 67108864
string text = "Payout Recipients"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
st_recipient_save_needed st_recipient_save_needed
dw_payout_recipient_details dw_payout_recipient_details
cb_cancel_annuity_payout_recipient cb_cancel_annuity_payout_recipient
cb_save_annuity_payout_recipient cb_save_annuity_payout_recipient
dw_payout_recipients dw_payout_recipients
dw_recipient_report dw_recipient_report
end type

on tabpage_payout_recipients.create
this.st_recipient_save_needed=create st_recipient_save_needed
this.dw_payout_recipient_details=create dw_payout_recipient_details
this.cb_cancel_annuity_payout_recipient=create cb_cancel_annuity_payout_recipient
this.cb_save_annuity_payout_recipient=create cb_save_annuity_payout_recipient
this.dw_payout_recipients=create dw_payout_recipients
this.dw_recipient_report=create dw_recipient_report
this.Control[]={this.st_recipient_save_needed,&
this.dw_payout_recipient_details,&
this.cb_cancel_annuity_payout_recipient,&
this.cb_save_annuity_payout_recipient,&
this.dw_payout_recipients,&
this.dw_recipient_report}
end on

on tabpage_payout_recipients.destroy
destroy(this.st_recipient_save_needed)
destroy(this.dw_payout_recipient_details)
destroy(this.cb_cancel_annuity_payout_recipient)
destroy(this.cb_save_annuity_payout_recipient)
destroy(this.dw_payout_recipients)
destroy(this.dw_recipient_report)
end on

type st_recipient_save_needed from statictext within tabpage_payout_recipients
boolean visible = false
integer x = 2409
integer y = 1384
integer width = 402
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 255
long backcolor = 67108864
string text = "Save Needed"
boolean focusrectangle = false
end type

type dw_payout_recipient_details from u_datawindow within tabpage_payout_recipients
integer x = 32
integer y = 888
integer width = 4096
integer height = 464
integer taborder = 11
string dataobject = "d_annuity_payout_recipient_detail"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;SetTransObject(SQLCA)
end event

event ue_print;idw_payout_recipient_report.TriggerEvent('ue_print')

end event

type cb_cancel_annuity_payout_recipient from commandbutton within tabpage_payout_recipients
integer x = 3301
integer y = 1360
integer width = 402
integer height = 104
integer taborder = 160
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Cancel"
end type

event clicked;INTEGER   li_rows

li_rows = idw_payout_recipients.RowCount()
IF li_rows > 0 THEN
	tab_payout.tabpage_payout_recipients.st_recipient_save_needed.Visible = FALSE
	dw_payout_accounts.Enabled = TRUE
	ib_save = FALSE
	THIS.Enabled = FALSE
	tab_payout.tabpage_payout_recipients.cb_save_annuity_payout_recipient.Enabled = FALSE
	
	wf_enable_controls(TRUE)
	tab_payout.tabpage_payout_recipients.cb_save_annuity_payout_recipient.Enabled = FALSE
	wf_retrieve_tab(6)
END IF
end event

type cb_save_annuity_payout_recipient from commandbutton within tabpage_payout_recipients
integer x = 2889
integer y = 1360
integer width = 402
integer height = 104
integer taborder = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
end type

event clicked;INTEGER   li_rtn
LONG      ll_annuity_payout_no, ll_annuity_payout_recipient_no
STRING    ls_first_address_line, ls_second_address_line
STRING    ls_address_line1, ls_address_line2, ls_address_line3, ls_address_line4, ls_address_line5
STRING    ls_annuity_payout_recipient_type_code, ls_message

idw_payout_recipients.AcceptText()

ll_annuity_payout_no = idw_payout_recipients.GetItemNumber(idw_payout_recipients.GetRow(),'annuity_payout_no')
ll_annuity_payout_recipient_no = idw_payout_recipients.GetItemNumber(idw_payout_recipients.GetRow(),'annuity_payout_recipient_no')
ls_annuity_payout_recipient_type_code = idw_payout_recipients.GetItemString(idw_payout_recipients.GetRow(),'annuity_payout_recipient_type_code')

ls_address_line1 = idw_payout_recipients.GetItemString(idw_payout_recipients.GetRow(),'address_line1')
ls_address_line2 = idw_payout_recipients.GetItemString(idw_payout_recipients.GetRow(),'address_line2')
ls_address_line3 = idw_payout_recipients.GetItemString(idw_payout_recipients.GetRow(),'address_line3')
ls_address_line4 = idw_payout_recipients.GetItemString(idw_payout_recipients.GetRow(),'address_line4')
ls_address_line5 = idw_payout_recipients.GetItemString(idw_payout_recipients.GetRow(),'address_line5')

IF ib_more_than_forty_chars THEN
	ls_first_address_line = idw_payout_recipients.GetItemString(idw_payout_recipients.GetRow(),'address_line2')
	ls_second_address_line = idw_payout_recipients.GetItemString(idw_payout_recipients.GetRow(),'address_line3')
	ls_message = 'The first two address lines below the name must be populated.'
ELSE
	ls_first_address_line = idw_payout_recipients.GetItemString(idw_payout_recipients.GetRow(),'address_line1')
	ls_second_address_line = idw_payout_recipients.GetItemString(idw_payout_recipients.GetRow(),'address_line2')
	ls_message = 'The first two address lines must be populated.'
END IF

IF Trim(ls_first_address_line) = '' OR Trim(ls_second_address_line) = '' THEN
	MessageBox('Annuity Payout Recipient Error',ls_message,StopSign!)
	RETURN
END IF


SQLCA.nf_begin_transaction()

IF ib_more_than_forty_chars THEN
	// only update 4 address lines
	UPDATE ANNUITY_PAYOUT_RECIPIENT
	SET    address_line2 = :ls_address_line2,
	       address_line3 = :ls_address_line3,
	       address_line4 = :ls_address_line4,
	       address_line5 = :ls_address_line5	       
	WHERE  annuity_payout_no = :ll_annuity_payout_no
	AND    annuity_payout_recipient_no = :ll_annuity_payout_recipient_no
	AND    annuity_payout_recipient_type_code = :ls_annuity_payout_recipient_type_code
	USING SQLCA;
ELSE
	// only update 4 address lines
	UPDATE ANNUITY_PAYOUT_RECIPIENT
	SET    address_line1 = :ls_address_line1,
	       address_line2 = :ls_address_line2,
	       address_line3 = :ls_address_line3,
	       address_line4 = :ls_address_line4,
	       address_line5 = :ls_address_line5	       
	WHERE  annuity_payout_no = :ll_annuity_payout_no
	AND    annuity_payout_recipient_no = :ll_annuity_payout_recipient_no
	AND    annuity_payout_recipient_type_code = :ls_annuity_payout_recipient_type_code
	USING SQLCA;
END IF
SQLCA.nf_handle_error('w_prepare_annuity_account','UPDATE ANNUITY_PAYOUT_RECIPIENT','cb_save_annuity_payout_recipient.clicked')

SQLCA.nf_commit_transaction()


THIS.Enabled = FALSE
tab_payout.tabpage_payout_recipients.cb_cancel_annuity_payout_recipient.Enabled = FALSE

tab_payout.tabpage_payout_recipients.st_recipient_save_needed.Visible = FALSE
dw_payout_accounts.Enabled = TRUE
ib_save = FALSE
wf_enable_controls(TRUE)
end event

type dw_payout_recipients from u_datawindow within tabpage_payout_recipients
integer x = 32
integer y = 64
integer width = 4096
integer height = 804
integer taborder = 11
string dataobject = "d_annuity_payout_recipient"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
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

lm_popup.m_options.PopMenu(iw_win.PointerX( ), iw_win.PointerY( ))

Destroy lm_popup
end event

event ue_print;idw_payout_recipient_report.TriggerEvent('ue_print')

end event

event constructor;call super::constructor;settransobject(sqlca)
end event

event itemchanged;call super::itemchanged;ib_save = TRUE
wf_enable_controls(FALSE)
tab_payout.tabpage_payout_recipients.cb_save_annuity_payout_recipient.Enabled = TRUE
tab_payout.tabpage_payout_recipients.cb_cancel_annuity_payout_recipient.Enabled = TRUE
end event

event editchanged;call super::editchanged;IF ib_save = FALSE THEN
	tab_payout.tabpage_payout_recipients.st_recipient_save_needed.Visible = TRUE
	dw_payout_accounts.Enabled = FALSE
	ib_save = true
	wf_enable_controls(FALSE)
   tab_payout.tabpage_payout_recipients.cb_save_annuity_payout_recipient.Enabled = TRUE
   tab_payout.tabpage_payout_recipients.cb_cancel_annuity_payout_recipient.Enabled = TRUE
END IF

end event

event rowfocuschanged;call super::rowfocuschanged;LONG            ll_annuity_payout_recipient_no, ll_rtn, ll_rows
STRING          ls_annuity_payout_recipient_type_code, ls_filter
DWItemStatus    ldwis


IF currentrow = 0 THEN 
	idw_payout_recipient_details.Reset()
ELSE
	ll_rows = idw_payout_recipient_details.Retrieve(il_annuity_payout_no)
	SQLCA.nf_handle_error('w_prepare_annuity_account','idw_payout_recipient_details.retrieve','dw_payout_recipients.rowfocuschanged')
	
	ldwis = THIS.GetItemStatus(currentrow,0,Primary!)
	
	IF ldwis <> New! THEN
		
		ll_annuity_payout_recipient_no = THIS.GetItemNumber(currentrow, 'annuity_payout_recipient_no')
		ls_annuity_payout_recipient_type_code = THIS.GetItemString(currentrow, 'annuity_payout_recipient_type_code')
		
		// clear existing filter
		ll_rtn = idw_payout_recipient_details.SetFilter(ls_filter)
		
		ls_filter = 'annuity_payout_recipient_no = ' + String(ll_annuity_payout_recipient_no) +' and annuity_payout_recipient_type_code = "'+ls_annuity_payout_recipient_type_code +'"'
		ll_rtn = idw_payout_recipient_details.SetFilter(ls_filter)
		ll_rtn = idw_payout_recipient_details.Filter()
		
	END IF
	
	wf_set_address_line_display_only(currentrow)
			
END IF

THIS.SetRowFocusIndicator(FocusRect!)
end event

event itemfocuschanged;call super::itemfocuschanged;wf_set_address_line_display_only(row)
end event

event resize;call super::resize;idw_payout_recipient_details.y = THIS.y + THIS.Height + 30

idw_payout_recipient_details.Height = THIS.Height/3

tab_payout.tabpage_payout_recipients.cb_cancel_annuity_payout_recipient.y = THIS.y + THIS.Height + idw_payout_recipient_details.Height + 80
tab_payout.tabpage_payout_recipients.cb_save_annuity_payout_recipient.y = tab_payout.tabpage_payout_recipients.cb_cancel_annuity_payout_recipient.y 

tab_payout.tabpage_payout_recipients.cb_cancel_annuity_payout_recipient.x = THIS.width/2 - tab_payout.tabpage_payout_recipients.cb_cancel_annuity_payout_recipient.Width
tab_payout.tabpage_payout_recipients.cb_save_annuity_payout_recipient.x = tab_payout.tabpage_payout_recipients.cb_cancel_annuity_payout_recipient.x - 412
end event

type dw_recipient_report from u_datawindow within tabpage_payout_recipients
boolean visible = false
integer x = 32
integer y = 64
integer width = 4096
integer height = 804
integer taborder = 11
string dataobject = "d_annuity_payout_recipient_report"
end type

event constructor;call super::constructor;SetTransObject(SQLCA)
end event

event ue_print;

THIS.Object.DataWindow.Print.Orientation = 1 //landscape
THIS.Print()
end event

type tabpage_contract_and_beneficiaries from userobject within tab_payout
event create ( )
event destroy ( )
integer x = 18
integer y = 108
integer width = 5609
integer height = 1476
long backcolor = 67108864
string text = "Contract and Beneficiaries"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
st_contract_save_needed st_contract_save_needed
cb_cancel_contract cb_cancel_contract
cb_delete_beneficiary cb_delete_beneficiary
cb_add_beneficiary cb_add_beneficiary
cb_delete_contract cb_delete_contract
cb_add_contract cb_add_contract
cb_save_contract cb_save_contract
dw_contract_beneficiaries_details dw_contract_beneficiaries_details
dw_contract_beneficiaries dw_contract_beneficiaries
end type

on tabpage_contract_and_beneficiaries.create
this.st_contract_save_needed=create st_contract_save_needed
this.cb_cancel_contract=create cb_cancel_contract
this.cb_delete_beneficiary=create cb_delete_beneficiary
this.cb_add_beneficiary=create cb_add_beneficiary
this.cb_delete_contract=create cb_delete_contract
this.cb_add_contract=create cb_add_contract
this.cb_save_contract=create cb_save_contract
this.dw_contract_beneficiaries_details=create dw_contract_beneficiaries_details
this.dw_contract_beneficiaries=create dw_contract_beneficiaries
this.Control[]={this.st_contract_save_needed,&
this.cb_cancel_contract,&
this.cb_delete_beneficiary,&
this.cb_add_beneficiary,&
this.cb_delete_contract,&
this.cb_add_contract,&
this.cb_save_contract,&
this.dw_contract_beneficiaries_details,&
this.dw_contract_beneficiaries}
end on

on tabpage_contract_and_beneficiaries.destroy
destroy(this.st_contract_save_needed)
destroy(this.cb_cancel_contract)
destroy(this.cb_delete_beneficiary)
destroy(this.cb_add_beneficiary)
destroy(this.cb_delete_contract)
destroy(this.cb_add_contract)
destroy(this.cb_save_contract)
destroy(this.dw_contract_beneficiaries_details)
destroy(this.dw_contract_beneficiaries)
end on

type st_contract_save_needed from statictext within tabpage_contract_and_beneficiaries
boolean visible = false
integer x = 3269
integer y = 1324
integer width = 402
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 255
long backcolor = 67108864
string text = "Save Needed"
boolean focusrectangle = false
end type

type cb_cancel_contract from commandbutton within tabpage_contract_and_beneficiaries
integer x = 2779
integer y = 1300
integer width = 402
integer height = 104
integer taborder = 110
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Cancel"
end type

event clicked;
tab_payout.tabpage_contract_and_beneficiaries.cb_save_contract.Enabled = FALSE
tab_payout.tabpage_contract_and_beneficiaries.cb_cancel_contract.Enabled = FALSE

dw_contract_beneficiaries.Reset()
dw_contract_beneficiaries_details.Reset()
wf_retrieve_tab(7)

tab_payout.tabpage_contract_and_beneficiaries.st_contract_save_needed.Visible = FALSE
dw_payout_accounts.Enabled = TRUE
ib_save = FALSE
end event

type cb_delete_beneficiary from commandbutton within tabpage_contract_and_beneficiaries
integer x = 1696
integer y = 1300
integer width = 567
integer height = 104
integer taborder = 90
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "De&lete Beneficiary"
end type

event clicked;INTEGER       li_rtn, li_contract_row
LONG          ll_contract_row, ll_annuity_contract_no, ll_beneficiaries_row, ll_seq_no
STRING        ls_beneficiary_name
DWItemStatus  ldwis_contract_status, ldwis_beneficiary_status


li_contract_row = dw_contract_beneficiaries.getRow()
IF li_contract_row = 0 THEN
	MessageBox('Cannot Delete','You must have an annuity contract before you can delete beneficiaries.',Exclamation!)
	RETURN
END IF


ll_beneficiaries_row = dw_contract_beneficiaries_details.getRow()

ldwis_beneficiary_status = idw_contract_beneficiaries_details.GetItemStatus(ll_beneficiaries_row, 0, Primary!)
IF ldwis_beneficiary_status = NewModified! OR ldwis_beneficiary_status = New! THEN
	
	
	ldwis_contract_status = idw_contract_beneficiaries.getItemStatus(li_contract_row, 0, PRIMARY!)
	
	IF ldwis_contract_status = New! OR ldwis_contract_status = NewModified! THEN
		// just delete the beneficiary without trying to save contract & other beneficiaries
		li_rtn = idw_contract_beneficiaries_details.DeleteRow(ll_beneficiaries_row)
		RETURN
	END IF
END IF

IF ll_beneficiaries_row > 0 THEN
	ls_beneficiary_name = dw_contract_beneficiaries_details.GetItemString(ll_beneficiaries_row, 'beneficiary_name')
	
	IF MESSAGEBOX("DELETE", "Are you sure you want to delete contract beneficiary: " + ls_beneficiary_name, Exclamation!, YesNoCancel!,  2) = 1 THEN
		li_rtn = dw_contract_beneficiaries_details.deleteRow(ll_beneficiaries_row)
		SQLCA.nf_handle_error('w_prepare_annuity_account','dw_contract_beneficiaries_details.deleterow','cb_delete_beneficiary.clicked')
		
		li_contract_row = dw_contract_beneficiaries.getRow()
		ldwis_contract_status = idw_contract_beneficiaries.getItemStatus(li_contract_row, 0, PRIMARY!)
		
		IF ldwis_contract_status <> New! OR ldwis_contract_status <> NewModified! THEN
			//now run the business rule checks
			SQLCA.nf_begin_transaction()
			
			IF wf_check_bus_rule_annuity_contract() >= 0 THEN
				li_rtn = dw_contract_beneficiaries_details.UPDATE()
				SQLCA.nf_handle_error('w_prepare_annuity_account', 'dw_contract_beneficiaries_details.UPDATE()', 'cb_delete_beneficiary.clicked')
				
				IF idw_contract_beneficiaries.GetItemStatus(idw_contract_beneficiaries.GetRow(),0,Primary!) = DataModified! THEN
					li_rtn = idw_contract_beneficiaries.Update()
					SQLCA.nf_handle_error('w_prepare_annuity_account','idw_contract_beneficiaries.Update','cb_delete_beneficiary.clicked')
				END IF			
			END IF
			
			SQLCA.nf_commit_transaction()

			
			tab_payout.tabpage_contract_and_beneficiaries.st_contract_save_needed.Visible = FALSE
			dw_payout_accounts.Enabled = TRUE
			ib_save = FALSE
						
			dw_contract_beneficiaries.Reset()
			dw_contract_beneficiaries_details.Reset()
			wf_retrieve_tab(7)
			
		END IF
	END IF
END IF

end event

type cb_add_beneficiary from commandbutton within tabpage_contract_and_beneficiaries
integer x = 1189
integer y = 1300
integer width = 503
integer height = 104
integer taborder = 80
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Add &Beneficiary"
end type

event clicked;
int li_row, li_seq_no, li_contract_beneficiary_rowcount, li_contract_row 
long ll_annuity_contract_no



li_contract_row = dw_contract_beneficiaries.getRow()
IF li_contract_row = 0 THEN
	MessageBox('Cannot Add','You must have an annuity contract before you can add beneficiaries.',Exclamation!)
	RETURN
END IF


//
//IF ib_save AND idw_contract_beneficiaries_details.getItemStatus(idw_contract_beneficiaries_details.getRow(), 0,Primary!) = DATAMODIFIED! THEN    
//	MESSAGEBOX("Save Required", "You have pending changes. Please Save or Cancel fist")
//	RETURN 0
//END IF

IF dw_contract_beneficiaries.getRow() = 0 THEN RETURN

li_contract_beneficiary_rowcount = dw_contract_beneficiaries_details.rowCount()

IF li_contract_beneficiary_rowcount = 0 THEN
	li_seq_no = 0
ELSE
	li_seq_no = dw_contract_beneficiaries_details.getItemNumber(li_contract_beneficiary_rowcount,'seq_no')
END IF

li_row = dw_contract_beneficiaries_details.insertRow(0)
SQLCA.nf_handle_error('w_prepare_annuity_account','dw_contract_beneficiaries_details.insertRow(0)','cb_add_beneficiary.clicked')

ll_annuity_contract_no = dw_contract_beneficiaries.getItemNumber(dw_contract_beneficiaries.getRow(), 'annuity_contract_no')

dw_contract_beneficiaries_details.setItem(li_row, 'annuity_payout_no', il_annuity_payout_no)
dw_contract_beneficiaries_details.setItem(li_row, 'annuity_contract_no', ll_annuity_contract_no)

li_seq_no++  // increment
dw_contract_beneficiaries_details.setItem(li_row, 'seq_no', li_seq_no)
dw_contract_beneficiaries_details.setRow(li_row)
dw_contract_beneficiaries_details.scrollToRow(li_row)

cb_save_contract.Enabled = TRUE
cb_cancel_contract.Enabled = TRUE

tab_payout.tabpage_contract_and_beneficiaries.st_contract_save_needed.Visible = TRUE
dw_payout_accounts.Enabled = FALSE

ib_save = TRUE


end event

type cb_delete_contract from commandbutton within tabpage_contract_and_beneficiaries
integer x = 581
integer y = 1300
integer width = 498
integer height = 104
integer taborder = 80
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Delete Contract"
end type

event clicked;long ll_contract_row, ll_contract_beneficiary_rowcount, ll_annuity_contract_no
STRING   ls_msg


IF ib_save THEN
	MESSAGEBOX("Save Required", "You have pending changes. Please Save or Cancel first.")
	RETURN 0
END IF

ll_contract_row = idw_contract_beneficiaries.GetRow()

IF ll_contract_row > 0 THEN
	
	ll_contract_beneficiary_rowcount = idw_contract_beneficiaries_details.RowCount()
	
	IF ll_contract_beneficiary_rowcount > 0 THEN
		ls_msg = 'Are you sure you want to delete this annuity contract and ALL associated beneficiaries?'
	ELSE
		ls_msg = 'Are you sure you want to delete this annuity contract?'
	END IF

	IF MESSAGEBOX("DELETE", ls_msg, Exclamation!, YesNoCancel!,  2) = 1 THEN
		ll_annuity_contract_no = dw_contract_beneficiaries.getItemNumber(ll_contract_row, 'annuity_contract_no')

		SQLCA.nf_begin_transaction()
		
		IF ll_contract_beneficiary_rowcount > 0 THEN
			DELETE  ANNUITY_CONTRACT_BENEFICIARY
			WHERE  annuity_contract_no = :ll_annuity_contract_no;
			SQLCA.nf_handle_error('w_prepare_annuity_account', 'cb_delete_contract', 'DELETE  FROM ANNUITY_CONTRACT_BENEFICIARIES')
		END IF
		
		DELETE  ANNUITY_CONTRACT
		WHERE  annuity_contract_no = :ll_annuity_contract_no;
		SQLCA.nf_handle_error('w_prepare_annuity_account', 'cb_delete_contract', 'DELETE  FROM ANNUITY_CONTRACT')
			
		SQLCA.nf_commit_transaction()

		
		dw_contract_beneficiaries.Reset()
		dw_contract_beneficiaries_details.Reset()
		wf_retrieve_tab(7)
		
	END IF
END IF
end event

type cb_add_contract from commandbutton within tabpage_contract_and_beneficiaries
integer x = 133
integer y = 1300
integer width = 434
integer height = 104
integer taborder = 70
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Add Contract"
end type

event clicked;
int ll_row

IF ib_save THEN
	MESSAGEBOX("Save Required", "You have pending changes. Please Save or Cancel first.")
	RETURN 0
END IF

//can only have one contract, so if row count > 0,  return
IF dw_contract_beneficiaries.rowCount() > 0 THEN RETURN

ll_row = dw_contract_beneficiaries.insertRow(0)
SQLCA.nf_handle_error('w_prepare_annuity_account','dw_contract_beneficiaries.insertRow(0)','cb_add_contract.clicked')

dw_contract_beneficiaries.setItem(ll_row, 'annuity_payout_no', il_annuity_payout_no)

ll_row = dw_contract_beneficiaries_details.insertRow(0)
SQLCA.nf_handle_error('w_prepare_annuity_account','dw_contract_beneficiaries_details.insertRow(0)','cb_add_contract.clicked')

dw_contract_beneficiaries_details.setItem(ll_row, 'annuity_payout_no', il_annuity_payout_no)
dw_contract_beneficiaries_details.setItem(ll_row, 'seq_no', 1)
dw_contract_beneficiaries_details.selectRow(1, false)

dw_contract_beneficiaries.setFocus()
dw_contract_beneficiaries.setColumn('annuity_term_code')

tab_payout.tabpage_contract_and_beneficiaries.cb_save_contract.Enabled = TRUE
tab_payout.tabpage_contract_and_beneficiaries.cb_cancel_contract.Enabled = TRUE
tab_payout.tabpage_contract_and_beneficiaries.st_contract_save_needed.Visible = TRUE
dw_payout_accounts.Enabled = FALSE
ib_save = TRUE
end event

type cb_save_contract from commandbutton within tabpage_contract_and_beneficiaries
integer x = 2327
integer y = 1300
integer width = 434
integer height = 104
integer taborder = 100
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Save"
end type

event clicked;BOOLEAN lb_modified
INT     li_rtn, li_contract_row, li_beneficiary_row, li_seq_no, li_cntr
LONG    ll_annuity_contract_no
STRING  ls_new_annuity_carrier_name, ls_beneficiary_name

DWItemStatus ldw_status

dw_contract_beneficiaries.acceptText()
dw_contract_beneficiaries_details.acceptText()

IF ib_save =  FALSE then return

li_contract_row = dw_contract_beneficiaries.getRow()
li_beneficiary_row = dw_contract_beneficiaries_details.getRow()
ldw_status = dw_contract_beneficiaries.getItemStatus(li_contract_row, 0, PRIMARY!)

SQLCA.nf_begin_transaction()

//test the business rules
IF wf_check_bus_rule_annuity_contract() = -1 THEN
	SQLCA.nf_rollback_transaction()

	// do a 'cancel' to rollback and reset, but only if we're modifying a row.  
	// Otherwise we'll let the user decide to fix the new row or manually cancel out on a new row.
	IF ldw_status = DATAMODIFIED! THEN
		cb_cancel_contract.triggerevent(CLICKED!)
	END IF
	RETURN
END IF

			
// concatenate benefit holder name and annuity carrier
// if length > 40 characters, warn user
// if warning is ignored, then truncate name_on_cheque
// and populate address_line1 with truncated string
// push existing address lines down one
// BR 8.160 The combined name of the individual who is eligible for the annuity benefits and the annuity carrier should not be more than 40 characters. 
ls_new_annuity_carrier_name = Trim(idw_contract_beneficiaries.GetItemString(1,'annuity_carrier_name',Primary!,FALSE))
li_rtn = wf_build_annuity_purchase_name_on_chq(is_purchase_name_on_cheque,ib_more_than_forty_chars,TRUE,'the save of the Annuity Contract',ls_new_annuity_carrier_name)
IF li_rtn < 0 THEN
	RETURN
END IF


IF  ldw_status <> NotModified! THEN
	IF ldw_status = NewModified! THEN   
		ll_annuity_contract_no = wf_get_next_contract_no()	
		dw_contract_beneficiaries.setItem(li_contract_row, 'annuity_contract_no', ll_annuity_contract_no)
		// if the contract is a new row, then populate annuity_contract_no for all new details rows as well (user might have added more than one details row)
		FOR li_cntr = 1 to dw_contract_beneficiaries_details.rowCount()
			dw_contract_beneficiaries_details.setItem(li_cntr, 'annuity_contract_no', ll_annuity_contract_no)
		NEXT
	END IF
	dw_contract_beneficiaries.SetItem(li_contract_row,'annuity_carrier_name',ls_new_annuity_carrier_name)
	
	li_rtn= dw_contract_beneficiaries.UPDATE()
	SQLCA.nf_handle_error('w_prepare_annuity_account','cb_save_contract', 'dw_contract_beneficiaries.update()')
	IF li_rtn < 1 THEN
		SignalError(-666, "Update failed during save on dw_contract_beneficiaries"  )
	END IF	
END IF

ldw_status = dw_contract_beneficiaries_details.getItemStatus(dw_contract_beneficiaries_details.getRow(),0,PRIMARY!)


FOR li_cntr = 1 to dw_contract_beneficiaries_details.rowCount()
	DWItemStatus ldwis
	
	ldwis = dw_contract_beneficiaries_details.GetItemStatus(li_cntr,'annuity_beneficiary_code',Primary!)
	ldwis = dw_contract_beneficiaries_details.GetItemStatus(li_cntr,'primary_beneficiary_flag',Primary!)
	ldwis = dw_contract_beneficiaries_details.GetItemStatus(li_cntr,'beneficiary_name',Primary!)
	
	ls_beneficiary_name = Trim(dw_contract_beneficiaries_details.GetItemString(li_cntr,'beneficiary_name'))
	
	IF    dw_contract_beneficiaries_details.GetItemStatus(li_cntr,'annuity_beneficiary_code',Primary!) = NotModified! &
	  AND dw_contract_beneficiaries_details.GetItemStatus(li_cntr,'primary_beneficiary_flag',Primary!) = NotModified! &
	  AND ( ls_beneficiary_name = '' OR  IsNull(ls_beneficiary_name)) THEN
	  
		IF dw_contract_beneficiaries_details.GetItemStatus(li_cntr,'beneficiary_name',Primary!) = NotModified! THEN
			dw_contract_beneficiaries_details.DeleteRow(li_cntr)
			SQLCA.nf_handle_error('w_prepare_annuity_account','dw_contract_beneficiaries_details.DeleteRow','cb_save_contract.clicked')
		ELSE
			MessageBox('Unsaved Beneficiary','There is a contract beneficiary that needs to have its Beneficiary Type and Primary Flag selected.',Exclamation!)
			lb_modified = TRUE
		END IF
	ELSE
		// data has been entered
		dw_contract_beneficiaries_details.SetItem(li_cntr,'beneficiary_name',ls_beneficiary_name)
	END IF
NEXT
IF lb_modified THEN
	// do not save
	SQLCA.nf_rollback_transaction()

	RETURN
END IF

li_rtn= dw_contract_beneficiaries_details.UPDATE()
SQLCA.nf_handle_error('w_prepare_annuity_account','cb_save_contract', 'dw_contract_beneficiaries_details.update()')
IF li_rtn < 1 THEN
	SignalError(-666, "Update failed during save on dw_contract_beneficiaries_details"  )
END IF

SQLCA.nf_commit_transaction()


tab_payout.tabpage_contract_and_beneficiaries.cb_save_contract.Enabled = FALSE
tab_payout.tabpage_contract_and_beneficiaries.cb_cancel_contract.Enabled = FALSE

tab_payout.tabpage_contract_and_beneficiaries.st_contract_save_needed.Visible = FALSE
dw_payout_accounts.Enabled = TRUE
ib_save = FALSE

wf_retrieve_tab(7)

end event

type dw_contract_beneficiaries_details from u_datawindow within tabpage_contract_and_beneficiaries
integer x = 32
integer y = 272
integer width = 4731
integer height = 1000
integer taborder = 12
string dataobject = "d_contract_beneficiaries_details"
borderstyle borderstyle = stylelowered!
end type

event itemchanged;call super::itemchanged;
STRING ls_beneficiary_name
Int li_find
datawindowchild ldw_child

IF dwo.name = 'annuity_beneficiary_code' THEN
	IF data = 'W' THEN
		this.getChild('annuity_beneficiary_code', ldw_child)
		ls_beneficiary_name =  ldw_child.getItemString(ldw_child.getRow(),'annuity_beneficiary_desc_e')
		idw_contract_beneficiaries_details.setItem(row, 'beneficiary_name', upper(ls_beneficiary_name))
	ELSE
		idw_contract_beneficiaries_details.setItem(row, 'beneficiary_name', '')
	END IF
END IF

tab_payout.tabpage_contract_and_beneficiaries.cb_save_contract.Enabled = TRUE
tab_payout.tabpage_contract_and_beneficiaries.cb_cancel_contract.Enabled = TRUE

tab_payout.tabpage_contract_and_beneficiaries.st_contract_save_needed.Visible = TRUE
dw_payout_accounts.Enabled = FALSE

ib_save = TRUE
end event

event editchanged;call super::editchanged;dw_payout_accounts.Enabled = FALSE

tab_payout.tabpage_contract_and_beneficiaries.cb_save_contract.Enabled = TRUE
tab_payout.tabpage_contract_and_beneficiaries.cb_cancel_contract.Enabled = TRUE

tab_payout.tabpage_contract_and_beneficiaries.st_contract_save_needed.Visible = TRUE
ib_save = TRUE
end event

event constructor;call super::constructor;settransobject(sqlca)
this.uf_setSelect(1)
end event

type dw_contract_beneficiaries from u_datawindow within tabpage_contract_and_beneficiaries
integer x = 32
integer y = 52
integer width = 4731
integer height = 1000
integer taborder = 11
string dataobject = "d_contract_beneficiaries"
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;settransobject(sqlca)

//this.uf_setSelect(0)

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

lm_popup.m_options.PopMenu(iw_win.PointerX( ), iw_win.PointerY( ))

Destroy lm_popup
end event

event ue_print;THIS.Object.DataWindow.Print.Orientation = 1 //landscape
dw_contract_beneficiaries_details.Object.DataWindow.Print.Orientation = 1
THIS.Print()

dw_contract_beneficiaries_details.Object.r_1.visible = 0
dw_contract_beneficiaries_details.Print()

dw_contract_beneficiaries_details.Object.r_1.visible = 1
end event

event editchanged;call super::editchanged;dw_payout_accounts.Enabled = FALSE

tab_payout.tabpage_contract_and_beneficiaries.cb_save_contract.Enabled = TRUE
tab_payout.tabpage_contract_and_beneficiaries.cb_cancel_contract.Enabled = TRUE

tab_payout.tabpage_contract_and_beneficiaries.st_contract_save_needed.Visible = TRUE
ib_save = TRUE
end event

event itemchanged;call super::itemchanged;// If the user has selected 'life with no guarantee', then we must warn the user that all beneficiaries will be deleted

INTEGER        li_message, li_annuity_beneficiary_rowcount, li_counter, li_rtn
STRING         ls_dwo_name, ls_annuity_term_code, ls_beneficiary_required_flag, ls_annuity_term_desc_e
dwitemstatus   ldwis

THIS.AcceptText()

IF row > 0 THEN
	ldwis = THIS.GetItemStatus(row,0,Primary!)
	
	ls_dwo_name = dwo.name
	IF ls_dwo_name = 'annuity_term_code' THEN
		
		tab_payout.tabpage_contract_and_beneficiaries.cb_save_contract.Enabled = TRUE
		tab_payout.tabpage_contract_and_beneficiaries.cb_cancel_contract.Enabled = TRUE
		tab_payout.tabpage_contract_and_beneficiaries.st_contract_save_needed.Visible = TRUE
		dw_payout_accounts.Enabled = FALSE
		ib_save = TRUE
		
		IF ldwis = DataModified! THEN			
			ls_annuity_term_code = String(data)
			
			SELECT beneficiary_required_flag,
					 annuity_term_desc_e
			INTO   :ls_beneficiary_required_flag,
					 :ls_annuity_term_desc_e
			FROM   Annuity_Term
			WHERE  annuity_term_code = :ls_annuity_term_code
			USING SQLCA;
			SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT beneficiary_required_flag, annuity_term_desc_e FROM Annuity_Term...','idw_contract_beneficiaries.itemchanged')
			
			IF ls_beneficiary_required_flag = 'N' THEN
				li_message = MessageBox('No Annuity Beneficiaries','No annuity beneficiaries are required for the selected annuity term type ('+ls_annuity_term_desc_e+'). ' &
											  +'If you choose to save the change to the annuity contract, the annuity beneficiaries will be deleted.' &
											  +'~r~n' &
											  +'~r~nDo you wish to delete the beneficiaries?',Question!,YesNo!,2)
				IF li_message = 1 THEN
					li_annuity_beneficiary_rowcount = idw_contract_beneficiaries_details.RowCount()
	
					FOR li_counter = 1 TO li_annuity_beneficiary_rowcount
						li_rtn = idw_contract_beneficiaries_details.DeleteRow(0)
						SQLCA.nf_handle_error('w_prepare_annuity_account','idw_contract_beneficiaries_details.DeleteRow','dw_contract_beneficiaries.itemChanged')
						IF li_rtn < 0 THEN
							MessageBox('Deletion Error','A problem was encountered during the deletion of the annuity beneficiaries.' &
											  +'~r~n' &
											  +'~r~nPlease contact the HELPDESK',Exclamation!)
							RETURN 1
						END IF
						li_rtn = idw_contract_beneficiaries_details.Update()
						IF li_rtn < 0 THEN
							MessageBox('Update Error','A problem was encountered during the deletion of the annuity contract beneficiaries.' &
											  +'~r~n' &
											  +'~r~nPlease contact the HELPDESK',Exclamation!)
							RETURN 1
						END IF
						
						li_rtn = idw_contract_beneficiaries.Update()
						IF li_rtn < 0 THEN
							MessageBox('Update Error','A problem was encountered during the updating of the annuity contract.' &
											  +'~r~n' &
											  +'~r~nPlease contact the HELPDESK',Exclamation!)
							RETURN 1
						END IF
					NEXT
					
					tab_payout.tabpage_contract_and_beneficiaries.cb_save_contract.Enabled = FALSE
					tab_payout.tabpage_contract_and_beneficiaries.cb_cancel_contract.Enabled = FALSE
					tab_payout.tabpage_contract_and_beneficiaries.st_contract_save_needed.Visible = FALSE
					dw_payout_accounts.Enabled = TRUE
					ib_save = FALSE
				END IF
			END IF			
		END IF
	END IF
END IF


end event

event keydown;call super::keydown;IF THIS.GetRow() > 0 THEN
	IF key = KeyTab! THEN
		IF THIS.GetColumnName() = 'annuity_carrier_name' THEN
			IF tab_payout.tabpage_contract_and_beneficiaries.dw_contract_beneficiaries_details.RowCount() > 0 THEN
				tab_payout.tabpage_contract_and_beneficiaries.dw_contract_beneficiaries_details.SetFocus()
				tab_payout.tabpage_contract_and_beneficiaries.dw_contract_beneficiaries_details.SetColumn('annuity_beneficiary_code')
			END IF
		END IF
	END IF
END IF
end event

type rb_region from radiobutton within w_prepare_annuity_account
integer x = 2917
integer y = 756
integer width = 352
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Region"
end type

event clicked;INTEGER      li_rows
STRING       ls_filter, ls_filter_return, ls_modify, ls_rtn


SetRedraw(FALSE)

dw_payout_accounts.Reset()
dw_payout_accounts.DataObject = 'd_retrieve_annuity_payout_list'
dw_payout_accounts.SetTransObject(SQLCA)
wf_clear_filter()

li_rows = dw_payout_accounts.Retrieve(0,vgst_user_profile.default_admin_region_code,'entry')
SQLCA.nf_handle_error('w_prepare_for_annuity_account','dw_payout_accounts','retrieve')

IF li_rows = 0 THEN 
	MessageBox('No Payouts Pending','There are no Annuity Accounts to be Paid Out.')
ELSE
	// Get instance variables
	il_annuity_account_no = dw_payout_accounts.GetItemNumber(dw_payout_accounts.GetRow(),'annuity_account_no')
	il_individual_no = dw_payout_accounts.GetItemNumber(dw_payout_accounts.GetRow(),'individual_no')

	// filter specific admin region for module
	ls_filter = "annuity_admin_region_code = '"+ vgst_user_profile.default_admin_region_code +"'"
	ls_filter_return = uo_filter_control.idw_datawindow.inv_filter.of_SetFilter(ls_filter)
	uo_filter_control.idw_datawindow.inv_filter.ib_filter_on = TRUE
	
	uo_filter_control.event ue_filter_changed(ls_filter)
END IF

SetRedraw(TRUE)
end event

type rb_individual from radiobutton within w_prepare_annuity_account
integer x = 2537
integer y = 756
integer width = 352
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Individual"
end type

event clicked;INTEGER       li_rows
STRING        ls_filter, ls_filter_return, ls_modify, ls_rtn


SetRedraw(FALSE)

dw_payout_accounts.Reset()
dw_payout_accounts.DataObject = 'd_retrieve_annuity_payout_list'
dw_payout_accounts.SetTransObject(SQLCA)
wf_clear_filter()


li_rows = dw_payout_accounts.Retrieve(il_forced_individual_no,vgst_user_profile.default_admin_region_code,'entry')
SQLCA.nf_handle_error('w_prepare_for_annuity_account','dw_payout_accounts','retrieve')

IF li_rows = 0 THEN
	wf_clear_datawindows()
	wf_clear_filter()
	MessageBox('No Payouts Pending','There are no Annuity Accounts to be Paid Out for this individual.')
ELSE
	rb_individual.Checked = TRUE
	// Get instance variables
	il_annuity_account_no = dw_payout_accounts.GetItemNumber(dw_payout_accounts.GetRow(),'annuity_account_no')

	// filter specific individual for module
	ls_filter = "individual_no = "+ String(il_forced_individual_no)
	ls_filter_return = uo_filter_control.idw_datawindow.inv_filter.of_SetFilter(ls_filter)
	uo_filter_control.idw_datawindow.inv_filter.ib_filter_on = TRUE
	
	uo_filter_control.event ue_filter_changed(ls_filter)

END IF

SetRedraw(TRUE)

PARENT.BringToTop = TRUE
end event

type cb_save_region from commandbutton within w_prepare_annuity_account
integer x = 4279
integer y = 852
integer width = 402
integer height = 104
integer taborder = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Save Region"
end type

event clicked;
SQLCA.nf_begin_transaction()

dw_payout_region.Update()
SQLCA.nf_handle_error('w_prepare_annuity_account','dw_payout_region.update','cb_save_region.clicked')

SQLCA.nf_commit_transaction()

end event

type uo_filter_control from u_dw_filter_control within w_prepare_annuity_account
integer x = 50
integer y = 740
integer taborder = 110
end type

on uo_filter_control.destroy
call u_dw_filter_control::destroy
end on

event ue_filter_changed;call super::ue_filter_changed;BOOLEAN  lb_modified
STRING	ls_rtn, ls_modify


PARENT.SetRedraw(FALSE)

IF dw_payout_accounts.RowCount() > 0 THEN
	// add to filter string
	ls_modify = 'st_filter.Text="'+ls_new_filter+ '"'
	
	ls_rtn = dw_payout_accounts.Modify(ls_modify)
	
	IF dw_payout_accounts.RowCount() > 0 THEN
		dw_payout_accounts.GroupCalc()
		dw_payout_accounts.SetRow(1)
		dw_payout_accounts.SelectRow(1,TRUE)
		dw_payout_accounts.SetColumn(il_payout_list_column)
		dw_payout_accounts.EVENT RowFocusChanged(1)
	END IF	
ELSE
	IF dw_payout_accounts.FilteredCount() <> 0 THEN
		// add to filter string
		ls_modify = 'st_filter.Text="'+ls_new_filter+ '"'
		
		ls_rtn = dw_payout_accounts.Modify(ls_modify)
	END IF
	wf_clear_datawindows()
	IF ls_new_filter <> ''  THEN
		MessageBox('','All annuity payouts have been filtered.',Information!)
	END IF
	iw_win.BringToTop = TRUE
END IF

POST wf_post_hscrollbar_position()
end event

type st_splitbar_1 from u_splitbar_horizontal within w_prepare_annuity_account
integer x = 18
integer y = 976
integer width = 5673
integer height = 32
integer textsize = -18
integer weight = 400
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
long il_min_units_from_top = 400
long il_min_units_from_bottom = 600
end type

event lbuttonup;
long ll_workspacewidth,ll_workspaceheight
ulong		sizetype

// do not extend ancestor script
// this allows prevention of splitbar_2 from overtaking splitbar_1 or 3 etc.

ib_lbuttondown = False
this.backcolor = iw_parent_window.backcolor

// Notify the resize service that the window size has changed.
ll_workspacewidth = iw_parent_window.WorkSpaceWidth()
ll_workspaceheight = This.y

inv_resize.Event pfc_Resize (sizetype,ll_workspacewidth  , ll_workspaceheight )

end event

type dw_payout_accounts from u_datawindow within w_prepare_annuity_account
integer x = 46
integer y = 92
integer width = 5659
integer height = 636
integer taborder = 10
string dataobject = "d_retrieve_annuity_payout_list"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
boolean righttoleft = true
end type

event constructor;call super::constructor;THIS.SetTransObject(SQLCA)

THIS.uf_setselect(1)
THIS.uf_SetSort(True)
end event

event rowfocuschanged;call super::rowfocuschanged;BOOLEAN      lb_payout_summary_tab_visible, lb_payout_detail_tab_visible
BOOLEAN      lb_claim_txn_detail_tab_visible, lb_overpayment_recovery_tab_visible
BOOLEAN      lb_payout_recipients_tab_visible, lb_contract_tab_visible
BOOLEAN      lb_enable_manual_payout_button
INTEGER      li_rows, li_selected_tab, li_rtn, li_max_completed_checklist_step_no
LONG         ll_paa_checklist_no, ll_cap_checklist_no
STRING       ls_annuity_admin_region_code, ls_name
U_CHECKLIST  l_checklist

IF ib_opening THEN
	// do not execute if open event is running for all individuals
ELSE
	IF currentrow > 0 THEN
		// determine which checklist is incomplete for the current row, set it up
		
		ll_cap_checklist_no = dw_payout_accounts.GetItemNumber(currentrow,'confirm_annuity_payout_checklist_no')
		IF ll_cap_checklist_no = 0 THEN
			// incomplete PAA checklist
			is_checklist_type_code = 'PAA'
			
			IF is_pap_open_mode <> 'inquiry' THEN
				uo_prepare_checklist.ib_checklist_visible = TRUE
				uo_prepare_checklist.Visible = TRUE
			END IF
			uo_confirm_payout_checklist.ib_checklist_visible = FALSE
			uo_confirm_payout_checklist.Visible = FALSE
			
			ll_paa_checklist_no = dw_payout_accounts.GetItemNumber(currentrow,'prepare_annuity_payout_checklist_no')
			il_checklist_no = ll_paa_checklist_no
			
			uo_prepare_checklist.uf_set_checklist_type_code('PAA')
			inv_prepare_checklist.nf_retrieve_checklists('PAA', ll_paa_checklist_no)
			
			li_max_completed_checklist_step_no = inv_prepare_checklist.nf_max_completed_step(ll_paa_checklist_no)
			
			l_checklist = uo_prepare_checklist
		ELSE
			// incomplete CAPLS/CAPAP checklist
			il_checklist_no = ll_cap_checklist_no
			
			uo_prepare_checklist.ib_checklist_visible = FALSE
			uo_prepare_checklist.Visible = FALSE
			IF is_pap_open_mode <> 'inquiry' THEN
				uo_confirm_payout_checklist.ib_checklist_visible = TRUE
				uo_confirm_payout_checklist.Visible = TRUE
			END IF
			
			SELECT checklist_type_code
			INTO   :is_checklist_type_code
			FROM   CHECKLIST
			WHERE  checklist_no = :ll_cap_checklist_no
			USING SQLCA;
			SQLCA.nf_handle_error('w_prepare_annuity_account', 'embedded SQL: SELECT checklist_type_code FROM CHECKLIST..', 'dw_payout_accounts.rfc')
			
			uo_confirm_payout_checklist.uf_set_checklist_type_code(is_checklist_type_code)
			
			inv_confirm_payout_checklist.nf_retrieve_checklists(is_checklist_type_code, il_checklist_no)
			
			li_max_completed_checklist_step_no = inv_confirm_payout_checklist.nf_max_completed_step(ll_cap_checklist_no)
			
			l_checklist = uo_confirm_payout_checklist
			
			// need PAA checklist for claim detail display
			ll_paa_checklist_no = dw_payout_accounts.GetItemNumber(currentrow,'prepare_annuity_payout_checklist_no')
		END IF

		il_annuity_payout_no        = THIS.GetItemNumber(CurrentRow,'annuity_payout_no')
		IF is_pap_open_mode = 'entry' THEN
			// do not change ann.acct for inquiry
			il_annuity_account_no       = THIS.GetItemNumber(CurrentRow,'annuity_account_no')
		END IF
		il_annuity_eligibility_no   = THIS.GetItemNumber(CurrentRow,'annuity_eligibility_no')
		il_annuity_calc_no          = THIS.GetItemNumber(CurrentRow,'annuity_calc_no')
		il_individual_no            = THIS.GetItemNumber(CurrentRow,'individual_no')
		il_annuity_account_claim_no = THIS.GetItemNumber(CurrentRow,'claim_no')
		is_claim_role_code          = THIS.GetItemString(CurrentRow,'claim_role_code')
		
		
		// make the 'multiple accounts' text appear if it's appropriate
		IF inv_common_annuity.nf_multiple_annuity_accounts(il_annuity_account_no) > 0 THEN
			st_multiple_accounts.visible = TRUE
		ELSE
			st_multiple_accounts.visible = FALSE
		END IF
		
		// retrieve admin region dddw
		li_rows = dw_payout_region.Retrieve(il_annuity_payout_no)
		SQLCA.nf_handle_error('w_prepare_annuity_account','dw_payout_region','retrieve')
			
		// enable or disable admin region dddw
		wf_enable_annuity_admin_region()
		
		
		// enable or disable 'Manual Payout' button
		lb_enable_manual_payout_button = wf_enable_manual_payout_btn()
		cb_manual_annuity_payout.Enabled = lb_enable_manual_payout_button
		
		// set checklist bar to display name and individual number
		ls_name = THIS.GetItemString(CurrentRow,'full_name') + ' - ' + String(il_individual_no)
		
		l_checklist.uf_set_checklistbar_text(ls_name)
		l_checklist.uf_set_historybar_text(ls_name)
		l_checklist.uf_set_notesbar_text(ls_name)

		// make tabs visible or not, depending on CAP checklist type
		li_selected_tab = tab_payout.Selectedtab
		IF is_checklist_type_code = 'PAA' THEN
			tab_payout.tabpage_contract_and_beneficiaries.Visible = FALSE
			lb_contract_tab_visible = FALSE
			
			CHOOSE CASE li_max_completed_checklist_step_no
				CASE 1
					// identified
					tab_payout.tabpage_payout_summary.Visible = FALSE
					tab_payout.tabpage_payout_details.Visible = FALSE
					tab_payout.tabpage_annuity_payout_txn_detail.Visible = FALSE
					tab_payout.tabpage_overpayment.Visible = FALSE
					tab_payout.tabpage_payout_recipients.Visible = FALSE
					
					lb_payout_summary_tab_visible = FALSE
					lb_payout_detail_tab_visible = FALSE
					lb_claim_txn_detail_tab_visible = FALSE
					lb_overpayment_recovery_tab_visible = FALSE
					lb_payout_recipients_tab_visible = FALSE
					
									
				CASE 2
					// confirm annuity eligibility
					tab_payout.tabpage_payout_summary.Visible = TRUE
					tab_payout.tabpage_payout_details.Visible = FALSE
					tab_payout.tabpage_annuity_payout_txn_detail.Visible = FALSE
					tab_payout.tabpage_overpayment.Visible = FALSE
					tab_payout.tabpage_payout_recipients.Visible = FALSE
					
					lb_payout_summary_tab_visible = TRUE
					lb_payout_detail_tab_visible = FALSE
					lb_claim_txn_detail_tab_visible = FALSE
					lb_overpayment_recovery_tab_visible = FALSE
					lb_payout_recipients_tab_visible = FALSE
					
				CASE 3,4,5
					// confirm overpayment recovery, establish payout
					
					// determine whether to display claim summary tab, or not
					lb_payout_summary_tab_visible = wf_display_payout_summary_tab()
					
					// determine whether to display claim details tab, or not
					lb_payout_detail_tab_visible = wf_display_claim_details_tab(ll_paa_checklist_no)
					
					// determine whether to display txn details tab, or not
					lb_claim_txn_detail_tab_visible = wf_display_txn_details_tab()
					
					// determine whether to display overpayment tab, or not
					lb_overpayment_recovery_tab_visible = wf_display_overpayment_tab(ll_paa_checklist_no)
					
					// determine whether to display recipient tab, or not
					lb_payout_recipients_tab_visible = wf_display_recipients_tab()
			END CHOOSE
			
		ELSE
			// determine whether to display claim summary tab, or not
			lb_payout_summary_tab_visible = wf_display_payout_summary_tab()
											
			// determine whether to display claim details tab, or not
			lb_payout_detail_tab_visible = wf_display_claim_details_tab(ll_paa_checklist_no)
			
			// determine whether to display txn details tab, or not
			lb_claim_txn_detail_tab_visible = wf_display_txn_details_tab()
			
			// determine whether to display overpayment tab, or not
			lb_overpayment_recovery_tab_visible = wf_display_overpayment_tab(ll_paa_checklist_no)
			
			// determine whether to display recipient tab, or not
			lb_payout_recipients_tab_visible = wf_display_recipients_tab()
			
			IF is_checklist_type_code = 'CAPAP' THEN
				CHOOSE CASE li_max_completed_checklist_step_no
					CASE 1,2,3,4
						tab_payout.tabpage_contract_and_beneficiaries.Visible = FALSE
						lb_contract_tab_visible = FALSE
					CASE 5
						// always display if 'contract received' is the max completed step
						tab_payout.tabpage_contract_and_beneficiaries.Visible = TRUE
						lb_contract_tab_visible = TRUE
					CASE 6,7,8
						// determine whether to display contract tab, or not
						lb_contract_tab_visible = wf_display_contracts_tab()
				END CHOOSE
			ELSEIF is_checklist_type_code = 'CAPLS' THEN
				tab_payout.tabpage_contract_and_beneficiaries.Visible = FALSE
				lb_contract_tab_visible = FALSE
			END IF
		END IF

		PARENT.SetRedraw(FALSE)
		
		// if the previously selected individual had a tabpage display that 
		// is not going to be displayed for the current individual
		// then display the first tabpage, which will always be visible
		IF (li_selected_tab = PAYOUT_SUMMARY AND lb_payout_summary_tab_visible       = FALSE) &
		OR (li_selected_tab = PAYOUT_DETAILS AND lb_payout_detail_tab_visible        = FALSE) &
		OR (li_selected_tab = TXN_DETAILS    AND lb_claim_txn_detail_tab_visible     = FALSE) &
		OR (li_selected_tab = OP_RECOVERY    AND lb_overpayment_recovery_tab_visible = FALSE) &
		OR (li_selected_tab = RECIPIENTS     AND lb_payout_recipients_tab_visible    = FALSE) &
		OR (li_selected_tab = CONTRACT       AND lb_contract_tab_visible             = FALSE)  THEN
			tab_payout.SelectTab(PARTICIPANT)
			li_selected_tab = PARTICIPANT
		END IF
		wf_retrieve_tab(li_selected_tab)
	
		POST wf_post_hscrollbar_position()
		
	END IF
END IF

end event

event resize;call super::resize;

dw_payout_region.y = THIS.y + THIS.Height + 128
dw_payout_region.Width = THIS.Width

// Max width for dw
IF dw_payout_region.Width > 4215 THEN dw_payout_region.Width = 4215

cb_save_region.y = dw_payout_region.y + 8
uo_filter_control.y = dw_payout_region.y - 120
cb_reset_sort.y = dw_payout_region.y - 120
cb_manual_annuity_payout.y = dw_payout_region.y - 120
st_multiple_accounts.y = dw_payout_region.y - 110
rb_individual.y = dw_payout_region.y - 120
rb_region.y = dw_payout_region.y - 120


end event

event clicked;call super::clicked;// this script helps handle the problem of the 'rightToLeft' scrollbar
// that automatically sends the horizontal scrollbar to the right
// which is where the user probably doesn't want it.
// the script will help move the scrollbar to approximately
// the last selected column 

CHOOSE CASE dwo.name
	CASE 'annuity_admin_region_desc'
		il_payout_list_column = 20
		
	CASE 'individual_no'
		il_payout_list_column = 4
		
	CASE 'individual_sin_no'
		il_payout_list_column = 11
		
	CASE 'full_name'
		il_payout_list_column = 5
		
	CASE 'claim_role_code'
		il_payout_list_column = 8
		
	CASE 'claim_no'
		il_payout_list_column = 9
		
	CASE 'age'
		il_payout_list_column = 10
		
	CASE 'birth_date'
		il_payout_list_column = 13
		
	CASE 'death_date'
		il_payout_list_column = 14
		
	CASE 'annuity_start_date'
		il_payout_list_column = 15
				
	CASE 'annuity_end_date'
		il_payout_list_column = 16
				
	CASE 'checklist_type_code'
		il_payout_list_column = 23
				
	CASE 'step_completed'
		il_payout_list_column = 24
				
	CASE 'benefit_option_code'
		il_payout_list_column = 17
		
	CASE 'annuity_set_aside_percent'
		il_payout_list_column = 18
		
	CASE 'sub_ledger_balance'
		il_payout_list_column = 31
		
END CHOOSE



IF Right(dwo.name,2) = '_t' THEN
	// if you've clicked one of the column headers, then a sort will take place
	// group calc after sort
	THIS.POST FUNCTION GroupCalc()
END IF

end event

event rowfocuschanging;call super::rowfocuschanging;
IF currentrow = 0 THEN
	il_payout_list_column = 1
ELSE
	il_payout_list_column = THIS.GetColumn()
END IF

end event

type uo_prepare_checklist from u_checklist within w_prepare_annuity_account
integer x = 5751
integer width = 2185
integer taborder = 40
boolean bringtotop = true
boolean ib_cannot_change_status = true
end type

on uo_prepare_checklist.destroy
call u_checklist::destroy
end on

event ue_checklist_buttonclicking;call super::ue_checklist_buttonclicking;/*
Confirm Annuity Eligibility
There will be a button provided to open the Confirm Annuity Eligibility module 
for the select annuity account.  This will allow the user to do a final confirmation 
of the annuity before the payout.  The module will pass in the annuity account
*/

BOOLEAN                  lb_commit
DECIMAL                  ldec_subledger_balance
DWObject                 l_dwo
INTEGER	                li_row, li_rtn, li_count, li_msg_rtn
STRING	                ls_checklist_step_type_code, ls_checklist_step_status_code



IF adw_dw.ClassName() = 'dw_checklist' THEN
	IF al_return = 1 THEN
		GOTO Label_End
	END IF

	SetPointer(HourGlass!)
	
	li_row = dw_payout_accounts.getrow()
	IF li_row <= 0 THEN 
		al_return = 1
		GOTO Label_End
	END IF
	
		
	ls_checklist_step_type_code = adw_dw.GetItemString(row,'checklist_step_type_code')
	
	CHOOSE CASE ls_checklist_step_type_code
		CASE '001'
			// Identified
			
		CASE '025'
			// Confirm Annuity Eligibility - Payout
			// do not open window if it is already open
			li_rtn = inv_common_annuity.nf_check_for_open_window( 'w_confirm_annuity_eligibility', 'Confirm Annuity Eligibility')
			IF li_rtn = 1 THEN
				al_return = 1
				GOTO Label_End
			END IF
						
			// if the individual still has an incomplete CAEIC or CAESC checklist (changed IW or SS checklist), then this module cannot be opened
			SELECT Count(*)
			INTO   :li_count
			FROM   ANNUITY_ELIGIBILITY a
			JOIN   CHECKLIST           b ON a.confirm_annuity_eligibility_checklist_no = b.checklist_no
			WHERE  b.checklist_status_code = 'IA'
			AND    b.checklist_type_code in ('CAEIN','CAEIC','CAESN','CAESC')
			AND    a.annuity_account_no = :il_annuity_account_no
			USING SQLCA;
			SQLCA.nf_handle_error('w_prepare_annuity_payout', 'embedded SQL: SELECT Count(*) FROM ANNUITY_ELIGIBILITY a, CHECKLIST...', 'uo_prepare_checklist.ue_checklist_buttonclicking')
			
			IF li_count > 0 THEN
				MessageBox('BR 9.100 - Complete Checklist','You cannot open the Confirm Annuity Eligibility module because there ' &
									           + 'is already an incomplete potential annuity eligibility checklist that must be completed.',Exclamation!)
				al_return = 1
				GOTO Label_End
			END IF
			
			
			// if the individual has a complete CAEIP or CAESP checklist associated with this payout, then this module cannot be opened
			SELECT Count(*)
			INTO   :li_count
			FROM   ANNUITY_PAYOUT a
			JOIN   CHECKLIST      b ON a.confirm_annuity_eligibility_checklist_no = b.checklist_no
			WHERE  a.annuity_payout_no = :il_annuity_payout_no
			AND    b.checklist_status_code = 'CA'
			USING SQLCA;
			SQLCA.nf_handle_error('w_prepare_annuity_payout', 'embedded SQL: SELECT Count(*) FROM ANNUITY_ELIGIBILITY, ANNUITY_PAYOUT...', 'uo_prepare_checklist.ue_checklist_buttonclicking')
			
			IF li_count > 0 THEN
				MessageBox('BR 9.110 - Complete Checklist','You cannot open the Confirm Annuity Eligibility module because ' &
									+ 'the associated confirm annuity eligibility checklist is already completed. ' &
									+ 'If you must re-confirm this individual, then please cancel this annuity payout checklist',Exclamation!)
				al_return = 1
				GOTO Label_End
			END IF
			
			li_rtn = inv_common_annuity.nf_check_individual_exists(il_individual_no)
			IF li_rtn = 0 THEN
				MessageBox('','The individual no longer exists, probably due to a merge. Please close this module and re-open it.',Exclamation!)
				al_return = 1
				GOTO Label_End
			END IF
			
		CASE '016'
			// Confirm Overpayment Recovery
			
		CASE '017'
			// Establish Payout
			
		CASE '024'
			// Checklist Completed
			
	END CHOOSE
	
END IF

//messagebox('','clicking - end')

al_return = 0

Label_End:

end event

event ue_checklist_buttonclicked;call super::ue_checklist_buttonclicked;INTEGER    li_row
LONG       ll_claim_no
STRING     ls_checklist_step_type_code, ls_claim_role_code, ls_window_to_open


ls_checklist_step_type_code = adw_dw.GetItemString(row,'checklist_step_type_code')


IF adw_dw.ClassName() = 'dw_checklist' THEN
	
	ls_window_to_open = adw_dw.GetItemString(row,'open_window_name')
	IF ls_window_to_open = '' THEN
		MessageBox('No window','There is no window to be opened by clicking this button.',Exclamation!)
	END IF
	
	CHOOSE CASE ls_checklist_step_type_code
		CASE '001'
			// Identified
			
		CASE '025'
			// Confirm Annuity Eligibility - Payout
			li_row = dw_payout_accounts.getrow()
			
			ll_claim_no          = dw_payout_accounts.GetItemNumber(li_row,'claim_no')
			ls_claim_role_code   = dw_payout_accounts.GetItemString(li_row,'claim_role_code')
			
			inv_common_annuity.nf_open_CAE_for_payout(ls_window_to_open,ll_claim_no,il_individual_no,il_annuity_payout_no,ls_claim_role_code)
			
		CASE '016'
			// Confirm Overpayment Recovery
			inv_common_annuity.nf_open_overpayment_list(ls_window_to_open, il_annuity_payout_no, il_individual_no)
			
		CASE '017'
			// Establish Payout
			
		CASE '024'
			// Checklist Completed
			
	END CHOOSE
	
//	idw_dw[1].SelectRow(0, false)
//	idw_dw[1].SelectRow(row, true)
	
END IF

end event

event ue_checklist_itemchanged;call super::ue_checklist_itemchanged;DATETIME  ldtm_current
INTEGER   li_rtn, li_count, li_msg_rtn, li_garnished_participants, li_orphan_count
INTEGER   li_annuity_participant_count, li_annuity_participant_counter, li_rows, li_msg, li_trancount
LONG      ll_related_checklist_no, ll_new_checklist_no, ll_recipient_no
STRING    ls_checklist_step_type_code, ls_dwo_name, ls_orphan_message, ls_benefit_holder_writeoff_flag
STRING    ls_annuity_entitlement_flag, ls_court_order_flag,ls_annuity_payout_garnish_flag
STRING    ls_annuity_payout_status_reason_code, ls_checklist_type_code
S_WINDOW_MESSAGE lstr_message

dwitemstatus ldwis
ldwis = adw_dw.GetItemStatus(row,0,Primary!)

ls_dwo_name = dwo.name
IF ls_dwo_name = 'checklist_step_status_code' THEN
	
	IF ib_save THEN
		wf_reject_checklist_status_change(TRUE,'Save Required','You have pending changes that need to be saved. Please Save or Cancel',il_checklist_no,'PAA',inv_checklist,adw_dw)
		al_return = 1
		GOTO label_end
	ELSE
		
		ls_checklist_step_type_code = adw_dw.GetItemString(row,'checklist_step_type_code')
		
		CHOOSE CASE ls_checklist_step_type_code
			CASE '001'
				// Identified
				
			CASE '025'
				// Confirm Annuity Eligibility - Payout
				
				IF data = 'COM' THEN
					
					// populate il_annuity_eligibility_no
					wf_get_payout_annuity_eligibility_no()				
					
					IF il_annuity_eligibility_no > 0 THEN
						SELECT Count(*)
						INTO   :li_count
						FROM   ANNUITY_ELIGIBILITY a
						JOIN   CHECKLIST           b ON a.confirm_annuity_eligibility_checklist_no = b.checklist_no
						WHERE  a.annuity_eligibility_no = :il_annuity_eligibility_no
						AND    b.checklist_status_code = 'IA'
						USING SQLCA;
						SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT Count(*) FROM ANNUITY_ELIGIBILITY, CHECKLIST...', 'uo_prepare_checklist.ue_checklist_itemchanged')
						
						IF li_count > 0 THEN
							wf_reject_checklist_status_change(TRUE,'BR 9.130 - Incomplete Checklist','You must complete the "Confirm Annuity Eligibility for Payout" checklist before this step can be set to "completed".',il_checklist_no,'PAA',inv_checklist,adw_dw)
							al_return = 1
							GOTO label_end
							
						ELSE
							SELECT annuity_calc_no
							INTO   :il_annuity_calc_no
							FROM   ANNUITY_PAYOUT
							WHERE  annuity_payout_no = :il_annuity_payout_no
							USING SQLCA;
							SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT FROM ANNUITY_PAYOUT...', 'uo_prepare_checklist.ue_checklist_itemchanged')
						END IF
					ELSE
						wf_reject_checklist_status_change(TRUE,'BR 9.90 - Open Module','You must open the Confirm Annuity Eligibility module and complete the'&
												 +'~r~n"Confirm Annuity Eligibility for Payout" checklist before this step can be set to "completed".',il_checklist_no,'PAA',inv_checklist,adw_dw)
						al_return = 1					
						GOTO label_end
					END IF
				END IF
	
				
				li_rtn = wf_determine_no_payout_reason(adw_dw)
				IF li_rtn < 0 THEN
					al_return = 1					
					GOTO label_end
				END IF
	
				
			CASE '016'
				SetPointer(HourGlass!)
							
				// Confirm Overpayment Recovery
				IF data = 'COM' THEN
					
					// Loop through all participants, check all those that are entitled to benefits for garnishment flag BRs
					li_annuity_participant_count = idw_participants.RowCount()
					FOR li_annuity_participant_counter = 1 TO li_annuity_participant_count
						
						ls_annuity_entitlement_flag = idw_participants.GetItemString(li_annuity_participant_counter,'annuity_entitlement_flag')
						
						IF ls_annuity_entitlement_flag = 'Y' THEN
							
							ls_annuity_payout_garnish_flag = idw_participants.GetItemString(li_annuity_participant_counter,'annuity_payout_garnish_flag')
							ll_recipient_no = idw_participants.GetItemNumber(li_annuity_participant_counter,'recipient_no')
							
							SELECT court_order_flag
							INTO   :ls_court_order_flag
							FROM   INDIVIDUAL
							WHERE  individual_no = :ll_recipient_no
							USING SQLCA;
							SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT court_order_flag FROM INDIVIDUAL...','uo_prepare_checklist.ue_checklist_itemchanged')
							
							
							// enforce part of Annnuity Payout Checklist BR 9.190, which is similar to Annnuity Payout Participant BR 1.310
							li_rtn = inv_annuity_participant.nf_garnishment_br_1_310(ls_court_order_flag,ls_annuity_payout_garnish_flag,ll_recipient_no)
							IF li_rtn = -1 THEN
								// error message was displayed by function call above
								wf_reject_checklist_status_change(FALSE,'','',il_checklist_no,'PAA',inv_checklist,adw_dw)
								al_return = 1
								GOTO label_end
							END IF
							
							// enforce Annnuity Payout Checklist BR 9.200, which is similar to Annnuity Payout Participant BR 1.320
							li_rtn = inv_annuity_participant.nf_garnishment_br_1_320(ls_court_order_flag,ls_annuity_payout_garnish_flag,ll_recipient_no)
							IF li_rtn = -1 THEN
								// error message was displayed by function call above
								wf_reject_checklist_status_change(FALSE,'','',il_checklist_no,'PAA',inv_checklist,adw_dw)
								al_return = 1
								GOTO label_end
							END IF
						END IF
					NEXT
					
	
					// non-entitled participants must represent an entitled participant
					
					/* 
					BR 9.190	The Prepare Annuity Payout, Confirm Overpayment Recovery checklist step must not be ‘Completed’, if any of the following is true:
					·	There is an annuity payout participant whose annuity benefits require garnishment but s/he does not have a court order
					·	There is an annuity participant in the annuity role of estate who does not represent a deceased dependent annuity participant who is entitled to annuity benefits
					·	There is an annuity participant in the annuity role of guardian or trustee who does not represent another annuity participant who is eligible for annuity benefits and/or entitled to annuity benefits.
					·	The annuity benefits of the individual who is eligible for the annuity benefits are not written off and all of the following are true:
						o	the individual is deceased
						o	there are no dependants
					·	The annuity benefits of a dependant who is entitled to annuity benefits are not written off and all of the following are true
						o	dependant is deceased 
						o	s/he does not have a representative in the annuity role of estate
					(Refer to Rationale)
	
					*/
					
					li_orphan_count = wf_orphan_participant(ls_orphan_message)
					IF li_orphan_count > 0 THEN					
						wf_reject_checklist_status_change(TRUE,'BR 9.190 - Annuity Payout Participant Error',ls_orphan_message,il_checklist_no,'PAA',inv_checklist,adw_dw)
						al_return = 1
						GOTO label_end
					END IF
					
					// validate writeoff for IW, SS or dependants
					li_rtn = wf_validate_writeoff_flag()
					IF li_rtn = -1 THEN
						// error message was displayed by function call above
						wf_reject_checklist_status_change(FALSE,'','',il_checklist_no,'PAA',inv_checklist,adw_dw)
						al_return = 1
						GOTO label_end
					END IF
					
					li_garnished_participants = wf_determine_participant_garnishment()
					IF li_garnished_participants > 0 THEN
																
						li_msg_rtn = MessageBox('Annuity Payout Participant Warning','The current status of at least one of the annuity participants indicates that '&
																								 + 'garnishment is required. If the garnish flag is left checked, then this payout '&
																								 + 'will require manual intervention, and any future payout modifications cannot '&
																								 + 'be made in this module.'&
																								 + '~r~n'&
																								 + '~r~nWould you like to continue completing this checklist step?',Exclamation!,YesNo!,2)
						IF li_msg_rtn = 1 THEN
							// warn user about non-zero sub-ledger if applicable
							li_rtn = wf_no_payout_subledger_warning('GAB')
							IF li_rtn < 0 THEN
								// user chose to cancel the completion of checklist step
								wf_reject_checklist_status_change(FALSE,'','',il_checklist_no,'PAA',inv_checklist,adw_dw)
								al_return = 1
								GOTO label_end
							END IF
							ib_garnishment = TRUE
						ELSE
							wf_reject_checklist_status_change(FALSE,'','',il_checklist_no,'PAA',inv_checklist,adw_dw)
							al_return = 1
							GOTO label_end
						END IF
					END IF
					
					// do not display message if this checklist step 
					// is being completed as 'not required - automatic', i.e., NRA
					li_msg_rtn = MessageBox('Annuity Payout Participant Warning','All annuity participants must be saved before this checklist step is'&
																							 + '~r~ncompleted. After this step is completed, you will not be able to add'&
																							 + '~r~nnew participants for this annuity payout.'&
																							 + '~r~nWould you like to continue completing this checklist step?',Exclamation!,YesNo!,2)
					IF li_msg_rtn <> 1 THEN
						wf_reject_checklist_status_change(FALSE,'','',il_checklist_no,'PAA',inv_checklist,adw_dw)
						al_return = 1
						GOTO label_end
					END IF
					
				END IF
				
			CASE '017'
				// Establish Payout
				
			CASE '024'
				// Checklist Completed
				IF String(data) = 'XCM' OR String(data) = 'XCA' THEN
					// if user has cancelled last step of PAA checklist, then
					// create a new PAA checklist
					
					ib_checklist_cancelled = TRUE
					
					// if the status/reason is not validated, an app error
					inv_common_annuity.nf_validate_annuity_status(il_annuity_payout_no,'X','')
				ELSE
					// determine status of payout - no need to create CAP checklist if 'no payout'
					IF ib_no_payout THEN
						// do not create CAP checklist
					ELSE
						// determines whether type is CAPAP or CAPLS
						wf_determine_cap_checklist_type(ls_checklist_type_code)
						
						IF ls_checklist_type_code = 'CAPAP' THEN
							
							lstr_message.apo_powerobjectparm[1] = PARENT
							
							
							OpenWithParm(w_select_annuity_payout_type,lstr_message)
							lstr_message = Message.PowerObjectParm
							IF lstr_message.as_stringparm[1] = 'LUMP' THEN
								is_checklist_type_code = 'CAPLS'
							ELSEIF lstr_message.as_stringparm[1] = 'PURCHASE' THEN
								is_checklist_type_code = 'CAPAP'
							ELSEIF lstr_message.as_stringparm[1] = 'CANCEL' THEN
								wf_reject_checklist_status_change(FALSE,'','',il_checklist_no,'PAA',inv_checklist,adw_dw)
								al_return = 1
								GOTO label_end
							END IF
								
							// opening of window triggered the posted itemchangeaccepted
							// so need to re-post
							post function uf_trigger_itemchangeaccepted(adw_dw,row,ls_dwo_name,data)
						ELSE
							is_checklist_type_code = 'CAPLS'
						END IF
					END IF
				END IF
				
		END CHOOSE
	END IF
		
ELSEIF dwo.Name = 'step_checked' THEN
	SetRedraw(TRUE)
	al_return = 2
	GOTO label_end

END IF

ldwis = adw_dw.GetItemStatus(row,ls_dwo_name,Primary!)
//messagebox('descendant','itemchanged - end')

adw_dw.SetItemStatus(row,ls_dwo_name,Primary!,DataModified!)

ldwis = adw_dw.GetItemStatus(row,ls_dwo_name,Primary!)


SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
IF li_trancount = 0 THEN
	SQLCA.nf_begin_transaction()
END IF

	
al_return = 0

label_end:
end event

event ue_checklist_itemchangeaccepted;call super::ue_checklist_itemchangeaccepted;BOOLEAN     lb_commit, lb_overpayment_tab_visible
INTEGER     li_rtn, li_find, li_rows, li_selected_tab
LONG        ll_hold_individual_no, ll_rtn
STRING      ls_checklist_step_type_code, ls_comment, ls_cancelled_comment
U_DS        lds_force_onto_payout


IF ib_checklist_cancelled THEN
	ls_checklist_step_type_code = '024'
ELSE
	ls_checklist_step_type_code = adw_dw.GetItemString(adw_dw.GetRow(),'checklist_step_type_code')
END IF

CHOOSE CASE ls_checklist_step_type_code
	CASE '001'
		// Identified
		
	CASE '025'
		// Confirm Annuity Eligibility - Payout

		li_rtn = wf_confirm_annuity_eligibility()
		
		// if PAA checklist can be completed by user
		IF li_rtn >= 0 THEN
			inv_common_annuity.nf_select_next_available_checklist_step(inv_checklist,adw_dw,il_checklist_no,is_checklist_type_code)			
			MessageBox('Overpayment','Overpayment data must be up to date for the payout to be correct.',Information!)
			
			lb_commit = TRUE
		ELSE
			// no payout
			// rest of checklist steps not required
			// complete the checklist itself with no payout
			ib_no_payout = TRUE
			
			// complete this step, but set next step to 'NRA'
			li_rtn = wf_auto_complete_next_step('016','NRA',adw_dw,as_data,'A',uo_prepare_checklist,inv_prepare_checklist,lb_commit)
			IF li_rtn < 0 THEN				
				RETURN
			END IF
			
			lb_commit = FALSE
		END IF
		
		
	CASE '016'
		// Confirm Overpayment Recovery
		
		// if user is completing this step manually, then validate OP recovery
		IF as_data = 'NRA' THEN
			
			IF ib_no_payout THEN
				// complete this step, but set next step to 'NRA'
				li_rtn = wf_auto_complete_next_step('017','NRA',adw_dw,as_data,'A',uo_prepare_checklist,inv_prepare_checklist,lb_commit)
				IF li_rtn < 0 THEN				
					RETURN
				END IF
			END IF
			
		ELSE
			IF ib_garnishment THEN

				ib_garnishment = FALSE
				ls_comment = 'Manual intervention required.'
				ib_optional_user_payout_comment = TRUE
				wf_build_annuity_payout_message('D','GAB')
				wf_update_annuity_payout_status('D','GAB',ls_comment)
				
				
				// update flag to indicate that O/P recovery was confirmed
				UPDATE ANNUITY_PAYOUT
				SET    overpayment_recovery_confirmed_flag = 'Y'
				WHERE  annuity_payout_no = :il_annuity_payout_no
				USING SQLCA;
				SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: UPDATE ANNUITY_PAYOUT SET overpayment_recovery_confirmed_flag = Y','uo_prepare_checklist.ue_checklist_itemchangeaccepted')
				
				
				// no payout
				// rest of checklist steps not required
				// complete the checklist itself with no payout
				ib_no_payout = TRUE
				
				// complete this step, but set next step to 'NRA'
				li_rtn = wf_auto_complete_next_step('017','NRA',adw_dw,as_data,'A',uo_prepare_checklist,inv_prepare_checklist,lb_commit)
				IF li_rtn < 0 THEN				
					RETURN
				END IF
				
				lb_commit = FALSE
			ELSE
				wf_confirm_overpayment_recovery()
				lb_commit = TRUE
			END IF
						
			li_selected_tab = tab_payout.Selectedtab
			lb_overpayment_tab_visible = wf_display_overpayment_tab(il_checklist_no)
			IF (li_selected_tab = PAYOUT_DETAILS AND tab_payout.tabpage_payout_details.Visible              = FALSE) &
			OR (li_selected_tab = OP_RECOVERY    AND lb_overpayment_tab_visible                             = FALSE) &
			OR (li_selected_tab = CONTRACT       AND tab_payout.tabpage_contract_and_beneficiaries.Visible  = FALSE)  THEN
				tab_payout.SelectTab(PARTICIPANT)
				li_selected_tab = PARTICIPANT
			END IF
			wf_retrieve_tab(li_selected_tab)
		END IF
				
	CASE '017'
		// Establish Payout
		IF as_data = 'NRA' THEN
			
			IF ib_no_payout THEN
				// complete this step, but set next step (last one) to 'COA'
				li_rtn = wf_auto_complete_next_step('024','COA',adw_dw,as_data,'A',uo_prepare_checklist,inv_prepare_checklist,lb_commit)
				IF li_rtn < 0 THEN				
					RETURN
				END IF
			END IF
		ELSE
			lb_commit = TRUE
		END IF
		
	CASE '024'
		// Checklist Completed
		
		IF ib_checklist_cancelled THEN
			ls_cancelled_comment = THIS.uf_get_cancelled_comment()			
		
			UPDATE ANNUITY_PAYOUT
			SET    annuity_payout_status_code = 'X',
				    payout_comment    = :ls_cancelled_comment
			WHERE  annuity_payout_no = :il_annuity_payout_no
			USING SQLCA;
			
			SQLCA.nf_handle_error('w_prepare_annuity_account', 'embedded SQL: UPDATE ANNUITY_PAYOUT...', 'uo_prepare_checklist.ue_checklist_itemchanged')
			
			// re-insert annuity payout
			wf_reinsert_onto_annuity_payout_list()
		ELSE
			IF ib_no_payout THEN
				// do not create a CAPAP/CAPLS checklist!
				IF ib_non_zero_subledger_warning THEN
					ib_non_zero_subledger_warning = FALSE
					wf_reinsert_onto_annuity_payout_list()
				END IF
			ELSE
				// do create a CAPAP/CAPLS checklist!
				wf_create_cap_checklist()
			END IF
		END IF	

		lb_commit = TRUE
				
END CHOOSE


IF lb_commit = TRUE THEN

	SQLCA.nf_commit_transaction()

	IF ib_checklist_cancelled THEN
		MessageBox('Annuity Payout List','The individual has been added back onto the Prepare Annuity Payout list.')
		ib_checklist_cancelled = FALSE
	END IF
END IF

// if the checklist has been committed
IF lb_commit AND ib_no_payout THEN
	// reset variable for next person
	ib_no_payout = FALSE

	// if we are allowing the user to enter a comment
	IF ib_optional_user_payout_comment OR ib_required_user_payout_comment THEN
		wf_annuity_payout_comment('no')
	END IF

END IF


IF lb_commit THEN
	wf_post_commit_retrieval()
END IF


end event

event ue_checklist_step_status_changed;call super::ue_checklist_step_status_changed;INTEGER         li_rtn
DATAWINDOWCHILD ldwc_child

IF as_status_assigned_method_code = '' THEN
	
	li_rtn = uo_prepare_checklist.tab_checklist.tabpage_checklist.dw_checklist.GetChild('checklist_step_status_code',ldwc_child)
	
	as_status_assigned_method_code = ldwc_child.GetItemString(ldwc_child.GetRow(),'status_assigned_method_code')

END IF
end event

event ue_checklist_master_itemchanged;call super::ue_checklist_master_itemchanged;DATAWINDOWCHILD ldwc_child
DATETIME        ldtm_annuity_start_date,ldtm_annuity_end_date,ldtm_current_date
INTEGER         li_rtn, li_trancount
LONG            ll_cae_checklist_no, ll_paa_checklist_no
STRING          ls_checklist_step_status_code, ls_cae_checklist_status_code, ls_annuity_eligibility_status_code, ls_claim_role_desc


IF ib_save THEN
	MessageBox('Save Required','You have pending changes that need to be saved. Please Save or Cancel',Exclamation!)
	al_return = 1
	RETURN
END IF

// verify that user is not trying to cancel checklist after the PAA Confirm Overpayment Recovery step has been completed
IF data = 'XM' THEN
	
	SELECT confirm_annuity_eligibility_checklist_no, prepare_annuity_payout_checklist_no
	INTO   :ll_cae_checklist_no, :ll_paa_checklist_no
	FROM   ANNUITY_PAYOUT
	WHERE  annuity_payout_no = :il_annuity_payout_no
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT confirm_annuity_eligibility_checklist_no FROM ANNUITY_PAYOUT...','uo_prepare_annuity_checklist.ue_checklist_master_itemchanged')
	
	IF ll_cae_checklist_no > 0 THEN
		
		SELECT checklist_status_code
		INTO   :ls_cae_checklist_status_code
		FROM   CHECKLIST
		WHERE  checklist_no = :ll_cae_checklist_no
		USING SQLCA;
		SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT checklist_status_code FROM CHECKLIST...','uo_prepare_annuity_checklist.ue_checklist_master_itemchanged')
		
		IF ls_cae_checklist_status_code = 'IA' THEN
			MessageBox('BR 9.50 - Annuity Payout Error','A Prepare Annuity Payout checklist must not be "Cancelled", if '&
			                            +'the associated Confirm Annuity Eligibility - Payout checklist is "In Progress".',StopSign!)
			uo_prepare_checklist.tab_checklist.tabpage_checklist.dw_checklist_master.post function Retrieve(ll_paa_checklist_no)
			SQLCA.nf_handle_error('w_prepare_annuity_account','uo_prepare_checklist.tab_checklist.tabpage_checklist.dw_checklist_master.post function Retrieve','uo_prepare_checklist.ue_checklist_master_itemchanged')
			al_return = 1
			RETURN
		ELSE
			// populate il_annuity_eligibility_no
			wf_get_payout_annuity_eligibility_no()
			
			IF is_claim_role_code = 'C' THEN
				ls_claim_role_desc = 'injured worker'
			ELSE
				ls_claim_role_desc = 'surviving spouse'
			END IF
			
			wf_determine_eligibility(il_annuity_eligibility_no,ls_annuity_eligibility_status_code,ldtm_annuity_start_date,ldtm_annuity_end_date)
			IF ls_annuity_eligibility_status_code = 'I' THEN
				// not eligible, error message
				MessageBox('BR 9.50 - Annuity Payout Error','A Prepare Annuity Payout checklist must not be "Cancelled", if '&
			                            +'the confirmation of annuity eligibility for payout purposes resulted in the '+ls_claim_role_desc+' not '&
												 +'being eligible for annuity benefits.' &
												 +'~r~n~r~n'&
												 +'You must complete the "Confirm Annuity Eligibility" checklist step. This will '&
												 +'remove the '+ls_claim_role_desc+' from the Prepare Annuity Payout list.'&
												 +'~r~n~r~n'&
												 +'If this '+ls_claim_role_desc+' needs to be added to the Prepare Annuity Payout list, '&
												 +'you must first re-establish her/his annuity eligibility via the Confirm Annuity Eligibility module. '&
												 +'You can then manually add the '+ls_claim_role_desc+' to the Prepare Annuity Payout list.',StopSign!)
				al_return = 1
				RETURN
			END IF
			
			IF ls_annuity_eligibility_status_code = 'A' THEN
				
				// test that, if eligible, the annuity end date is not in future - otherwise, error message
				ldtm_current_date = f_server_datetime()
				
				IF DaysAfter(Date(ldtm_current_date), Date(ldtm_annuity_end_date)) > 0 THEN
					// annuity end date is now in the future
					MessageBox('BR 9.50 - Annuity Payout Error','A Prepare Annuity Payout checklist must not be ‘Cancelled’, if '&
													 +'the confirmation of annuity eligibility for payout purposes resulted in the '+ls_claim_role_desc+'’s '&
													 +'annuity end date now being in the future.' &
													 +'~r~n~r~n'&
													 +'You must complete the "Confirm Annuity Eligibility" checklist step. This will '&
													 +'remove the '+ls_claim_role_desc+' from the Prepare Annuity Payout list.'&
													 +'~r~n~r~n'&
													 +'If this '+ls_claim_role_desc+' needs to be added to the Prepare Annuity Payout list, '&
													 +'you must correct the birth or death date.',StopSign!)
					al_return = 1
					RETURN
				END IF
			END IF
			
		END IF
	END IF
	
	SELECT checklist_step_status_code
	INTO   :ls_checklist_step_status_code
	FROM   CHECKLIST_STEP
	WHERE  checklist_step_type_code = '016'
	AND    checklist_no = :il_checklist_no
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT checklist_step_status_code FROM CHECKLIST_STEP...','uo_prepare_annuity_checklist.ue_checklist_master_itemchanged')
	
	
	IF ls_checklist_step_status_code = 'INA' THEN
		// OK - no OP records for this annuity payout yet
	ELSE
		// if the PAA checklist 'Confirm Overpayment Recovery ' step has been completed
		// then determine if OP records can be reversed if they exist
		// wf_reverse_confirm_OP_recovery()
		ib_reverse_confirm_OP_recovery = TRUE
	END IF
	
END IF

IF as_status_assigned_method_code = '' THEN
	
	li_rtn = uo_prepare_checklist.tab_checklist.tabpage_checklist.dw_checklist_master.GetChild('checklist_status_code',ldwc_child)
	
	as_status_assigned_method_code = ldwc_child.GetItemString(ldwc_child.GetRow(),'status_assigned_method_code')

END IF


SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
IF li_trancount = 0 THEN
	SQLCA.nf_begin_transaction()
END IF

end event

event ue_checklist_master_itemchangeaccepted;call super::ue_checklist_master_itemchangeaccepted;IF ib_reverse_confirm_OP_recovery THEN
	// reset variable
	ib_reverse_confirm_OP_recovery = FALSE
	
	// and perform reversal of recovery
	wf_reverse_confirm_OP_recovery()
END IF
end event

type uo_confirm_payout_checklist from u_checklist within w_prepare_annuity_account
boolean visible = false
integer x = 5751
integer y = 384
integer taborder = 100
boolean bringtotop = true
boolean ib_cannot_change_status = true
end type

on uo_confirm_payout_checklist.destroy
call u_checklist::destroy
end on

event ue_checklist_buttonclicked;call super::ue_checklist_buttonclicked;DATETIME   ldtm_null
INTEGER    li_opening_no, li_authorization_count
LONG       ll_claim_no
STRING     ls_checklist_step_type_code, ls_window_to_open, ls_correspondence_type



IF IsValid(w_error) THEN RETURN

ls_checklist_step_type_code = adw_dw.GetItemString(row,'checklist_step_type_code')

ls_window_to_open = adw_dw.GetItemString(row,'open_window_name')
IF ls_window_to_open = '' THEN
	MessageBox('No window','There is no window to be opened by clicking this button.',Exclamation!)
END IF


IF adw_dw.ClassName() = 'dw_checklist' THEN
	
	CHOOSE CASE ls_checklist_step_type_code
		CASE '001'
			// Identified
			
		CASE '018'
			// Confirm Payout Amount
			
		CASE '019'
			// Request Authorization				
			wf_create_request_authorization_event(ls_window_to_open)
			
		CASE '020'
			// Notification
			li_authorization_count = wf_verify_payout_authorization()
			IF li_authorization_count = 1 THEN
				IF is_checklist_type_code = 'CAPAP' THEN
					ls_correspondence_type = 'annuity purchase'
				ELSE
					ls_correspondence_type = 'lump sum payout'
				END IF
			
				MessageBox('Opening Correspondence for Payout','Please ensure that you select the '+ ls_correspondence_type + ' template in the Correspondence module.',Information!)
				
				IF is_claim_role_code = 'C' THEN
					inv_common_annuity.nf_get_AP_claim(il_annuity_payout_no,il_individual_no,ll_claim_no)								
				ELSE
					// Surviving Spouse has specific claim
					ll_claim_no = il_annuity_account_claim_no
				END IF
								
				IF ll_claim_no > 0 THEN
					SetNull(ldtm_null)
					inv_common_annuity.nf_open_correspondence('d_claim_number_search', 'ENABLE', '', '', ldtm_null,0,0,ll_claim_no)
				ELSE
					SignalError(-1,'No claim number to open correspondence in Prepare Annuity Payout')
				END IF
			ELSE
				MessageBox('BR 9.300 - Annuity Payout NOT Authorized','This annuity payout is not authorized. You cannot send a notification until the payout is authorized.',Exclamation!)
			END IF
			
		CASE '021'
			// Contract Received (does not apply for CAPLS)
			IF is_checklist_type_code = 'CAPAP' THEN
				
			END IF
			
		CASE '022'
			// Contract Confirmed (does not apply for CAPLS)
			IF is_checklist_type_code = 'CAPAP' THEN
				inv_common_annuity.nf_open_confirm_contract_reminder(il_annuity_payout_no,il_individual_no,il_annuity_account_claim_no,is_claim_role_code)
			END IF
			
		CASE '023'
			// Request Payout
			li_authorization_count = wf_verify_payout_authorization()
			IF li_authorization_count = 1 THEN
				// open the event log
				wf_create_request_payout_event(ls_window_to_open)
			ELSE
				MessageBox('BR 9.403 - Annuity Payout NOT Authorized','This annuity payout is not authorized. You cannot create a Request Payout event until the payout is authorized.',Exclamation!)
			END IF
			
							
		CASE '024'
			// Checklist Completed
			
	END CHOOSE
			
END IF
end event

event ue_checklist_buttonclicking;call super::ue_checklist_buttonclicking;STRING     ls_checklist_step_type_code


ls_checklist_step_type_code = adw_dw.GetItemString(row,'checklist_step_type_code')

IF adw_dw.ClassName() = 'dw_checklist' THEN
	CHOOSE CASE ls_checklist_step_type_code
		CASE '001'
			// Identified
			
		CASE '018'
			// Confirm Payout Amount
			
		CASE '019'
			// Confirm Overpayment Recovery
			
		CASE '020'
			// Notification
			
		CASE '021'
			// Contract Received (does not apply for CAPLS)
			IF is_checklist_type_code = 'CAPAP' THEN
				
			END IF
			
		CASE '022'
			// Contract Confirmed (does not apply for CAPLS)
			IF is_checklist_type_code = 'CAPAP' THEN
				
			END IF
			
		CASE '023'
			// Request Payout
			
		CASE '024'
			// Checklist Completed
			
	END CHOOSE
	
END IF
end event

event ue_checklist_itemchangeaccepted;call super::ue_checklist_itemchangeaccepted;BOOLEAN       lb_commit, lb_contract_received
INTEGER       li_find, li_rtn
LONG          ll_rows, ll_rtn, ll_hold_individual_no
STRING        ls_checklist_step_type_code, ls_status_assigned_method_code, ls_cancelled_comment
STRING        ls_name_on_cheque, ls_first_forty_chars, ls_remainder, ls_error_message
DWItemStatus  ldwis
DWObject      l_dwo



IF ib_checklist_cancelled THEN
	ls_checklist_step_type_code = '024'
ELSE
	ls_checklist_step_type_code = adw_dw.GetItemString(adw_dw.GetRow(),'checklist_step_type_code')
END IF

IF adw_dw.ClassName() = 'dw_checklist' THEN

	CHOOSE CASE ls_checklist_step_type_code
		CASE '001'
			// Identified
			
		CASE '018'
			// Confirm Payout Amount
			IF as_data = 'NRA' THEN
				IF ib_no_payout THEN
					li_rtn = wf_auto_complete_next_step('019','NRA',adw_dw,as_data,'A',uo_confirm_payout_checklist,inv_confirm_payout_checklist,lb_commit)
					IF li_rtn < 0 THEN				
						RETURN
					END IF
				END IF
			ELSE
				wf_confirm_payout_amount()
				/*
				
				For either the CAPAP or CAPLS checklists, the 'Request Authorization' step will be automatically completed 
				as not required (NRA), if the user has a high enough auth level. 
				For CAPAP, the next step, 'Authorization Confirmed' is only required if the prior step was required. So, if 
				'Request Authorization' was NRA, then 'Authorization Confirmed' must be NRA also.
				
				*/
				
				wf_auto_request_authorization(is_checklist_type_code)
				
				IF ib_auto_authorization THEN
					// auto-complete next step (019, Request Authorization)
					li_rtn = wf_auto_complete_next_step('019','NRA',adw_dw,as_data,'A',uo_confirm_payout_checklist,inv_confirm_payout_checklist,lb_commit)
					IF li_rtn < 0 THEN				
						RETURN
					END IF
				ELSE
					lb_commit = TRUE
				END IF
			END IF
			
			
		CASE '019'
			// Request Authorization
			IF as_data = 'NRA' THEN
				IF ib_auto_authorization THEN
					lb_commit = TRUE
				END IF
				
				IF ib_no_payout THEN
					li_rtn = wf_auto_complete_next_step('020','NRA',adw_dw,as_data,'A',uo_confirm_payout_checklist,inv_confirm_payout_checklist,lb_commit)
					IF li_rtn < 0 THEN				
						RETURN
					END IF
				END IF
			END IF
			
			ib_total_writeoff = wf_determine_total_writeoff()
			IF ib_total_writeoff THEN
				// BR 9.340 - The Confirm Annuity Payout - Lump Sum, Notification checklist step must be automatically set to Not Required,
				//            if the annuity benefits must be written off for all recipients of the annuity payout.
				// auto-complete next step (020, Notification)
				li_rtn = wf_auto_complete_next_step('020','NRA',adw_dw,as_data,'A',uo_confirm_payout_checklist,inv_confirm_payout_checklist,lb_commit)
				IF li_rtn < 0 THEN				
					RETURN
				END IF
				lb_commit = FALSE
			ELSE
				IF ib_no_payout = FALSE THEN
					lb_commit = TRUE
				ELSE
					lb_commit = FALSE
				END IF
			END IF
			
			
		CASE '020'
			// Notification
			IF as_data = 'NRA' THEN
				
				IF is_checklist_type_code = 'CAPAP' THEN
					IF ib_total_writeoff OR ib_no_payout THEN
						// BR 9.370	The Confirm Annuity Payout - Annuity Purchase, Contract Received checklist step must be automatically set to Not Required,
						//          if the annuity benefits must be written off for the individual who is eligible for the annuity benefits.
						// auto-complete next step (021, Contract Received)
						li_rtn = wf_auto_complete_next_step('021','NRA',adw_dw,as_data,'A',uo_confirm_payout_checklist,inv_confirm_payout_checklist,lb_commit)
						IF li_rtn < 0 THEN				
							RETURN
						END IF
					ELSE
						lb_commit = TRUE						
					END IF
				ELSE
					// CAPLS, so auto-complete a different checklist step
					IF ib_no_payout THEN
						// auto-complete next step (023, Request Payout)
						li_rtn = wf_auto_complete_next_step('023','NRA',adw_dw,as_data,'A',uo_confirm_payout_checklist,inv_confirm_payout_checklist,lb_commit)
						IF li_rtn < 0 THEN				
							RETURN
						END IF
					ELSE
						lb_commit = TRUE
					END IF
				END IF
			ELSE
				lb_commit = TRUE
			END IF
			
			
		CASE '021'
			// Contract Received (does not apply for CAPLS)
			IF as_data = 'NRA' THEN
				IF ib_total_writeoff OR ib_no_payout THEN
					// BR 9.380	The Confirm Annuity Payout - Annuity Purchase, Contract Confirmed checklist step must be automatically set to Not Required,
					//          if the annuity benefits must be written off for the individual who is eligible for the annuity benefits.
					// auto-complete next step (022, Contract Confirmed)
					li_rtn = wf_auto_complete_next_step('022','NRA',adw_dw,as_data,'A',uo_confirm_payout_checklist,inv_confirm_payout_checklist,lb_commit)
					IF li_rtn < 0 THEN				
						RETURN
					END IF
				ELSE
					lb_commit = TRUE
				END IF
			ELSE
				tab_payout.POST FUNCTION SelectTab(CONTRACT)
				lb_contract_received = TRUE
				lb_commit = TRUE
			END IF
			
			
		CASE '022'
			// Contract Confirmed (does not apply for CAPLS)
			IF as_data = 'NRA' THEN
				IF ib_total_writeoff THEN
					// for a total writeoff, this is the last "not required" checklist step, so commit
					ib_total_writeoff = FALSE
					lb_commit = TRUE
				END IF
				
				IF ib_no_payout THEN
					// auto-complete next step (023, Request Payout)
					li_rtn = wf_auto_complete_next_step('023','NRA',adw_dw,as_data,'A',uo_confirm_payout_checklist,inv_confirm_payout_checklist,lb_commit)
					IF li_rtn < 0 THEN				
						RETURN
					END IF
				ELSE
					lb_commit = TRUE
				END IF
			ELSE
				UPDATE ANNUITY_CONTRACT
				SET    contract_confirmed_flag = 'Y'
				WHERE  annuity_payout_no = :il_annuity_payout_no
				USING SQLCA;
				SQLCA.nf_handle_error('w_prepare_annuity_account', 'embedded SQL: UPDATE ANNUITY_CONTRACT SET contract_confirmed_flag = Y...', 'uo_confirm_payout_checklist.ue_checklist_itemchangeaccepted')
				
				IF ib_more_than_forty_chars THEN
					
					ls_first_forty_chars = Left(is_purchase_name_on_cheque,40)
					ls_remainder = Right(is_purchase_name_on_cheque,Len(is_purchase_name_on_cheque)-40)
					
					UPDATE a
					SET    a.name_on_cheque = :ls_first_forty_chars,
							 a.address_line1  = :ls_remainder,
							 a.address_line2  = a.address_line1,
							 a.address_line3  = a.address_line2,
							 a.address_line4  = a.address_line3,
							 a.address_line5  = a.address_line4
					FROM   ANNUITY_PAYOUT_RECIPIENT  a
					JOIN   ANNUITY_PAYOUT_TXN_DETAIL b ON a.annuity_payout_no                  = b.annuity_payout_no
	            						                AND a.annuity_payout_recipient_no        = b.annuity_payout_recipient_no
												             AND a.annuity_payout_recipient_type_code = b.annuity_payout_recipient_type_code
					WHERE  b.annuity_payout_recipient_type_code = 'I'
					AND    b.recipient_no                       = :il_individual_no
					AND    b.annuity_payout_no                  = :il_annuity_payout_no
					USING SQLCA;
					SQLCA.nf_handle_error('w_prepare_annuity_account', 'embedded SQL: UPDATE ANNUITY_PAYOUT_RECIPIENT...(1)', 'uo_confirm_payout_checklist.ue_checklist_itemchangeaccepted')
				ELSE
					UPDATE a
					SET    a.name_on_cheque = :is_purchase_name_on_cheque
					FROM   ANNUITY_PAYOUT_RECIPIENT  a
					JOIN   ANNUITY_PAYOUT_TXN_DETAIL b ON a.annuity_payout_no                  = b.annuity_payout_no
	            						                AND a.annuity_payout_recipient_no        = b.annuity_payout_recipient_no
												             AND a.annuity_payout_recipient_type_code = b.annuity_payout_recipient_type_code
					WHERE  b.annuity_payout_recipient_type_code = 'I'
					AND    b.recipient_no                       = :il_individual_no
					AND    b.annuity_payout_no                  = :il_annuity_payout_no
					USING SQLCA;
					SQLCA.nf_handle_error('w_prepare_annuity_account', 'embedded SQL: UPDATE ANNUITY_PAYOUT_RECIPIENT...(2)', 'uo_confirm_payout_checklist.ue_checklist_itemchangeaccepted')
				END IF
				
				// reset variables
				is_purchase_name_on_cheque = ''
				ib_total_writeoff = FALSE
				
				tab_payout.POST FUNCTION SelectTab(RECIPIENTS)
				
				lb_commit = TRUE
			END IF
			
			
		CASE '023'
			// Request Payout
			IF as_data = 'NRA' THEN
				IF ib_no_payout THEN
					// auto-complete next step (024, Checklist Completed)
					li_rtn = wf_auto_complete_next_step('024','COA',adw_dw,as_data,'A',uo_confirm_payout_checklist,inv_confirm_payout_checklist,lb_commit)
					IF li_rtn < 0 THEN				
						RETURN
					END IF
				ELSE
					lb_commit = TRUE
				END IF
			ELSE
				wf_request_payout()
				
				ib_optional_user_payout_comment = TRUE
				is_annuity_payout_reason_message = 'You have requested a payout for this individual.'
				
				lb_commit = TRUE
			END IF
			
			
		CASE '024'
			// Checklist Completed
			// if module opened for a single IW or SS & that person has had checklist completed, then no rows left
			IF ib_checklist_cancelled THEN	
				
				ls_cancelled_comment = THIS.uf_get_cancelled_comment()
			
				UPDATE ANNUITY_PAYOUT
				SET    annuity_payout_status_code = 'X',
						 payout_comment    = :ls_cancelled_comment
				WHERE  annuity_payout_no = :il_annuity_payout_no
				USING SQLCA;
				SQLCA.nf_handle_error('w_prepare_annuity_account', 'embedded SQL: UPDATE ANNUITY_PAYOUT SET annuity_payout_status_code = X...', 'uo_confirm_payout_checklist.ue_checklist_itemchangeaccepted')
				
				// re-insert annuity payout
				li_rtn = wf_reinsert_onto_annuity_payout_list()
				

			ELSEIF ib_checklist_completed THEN
				ib_checklist_completed = FALSE
				
				UPDATE ANNUITY_PAYOUT
				SET    annuity_payout_status_code = 'C'
				WHERE  annuity_payout_no = :il_annuity_payout_no
				USING SQLCA;
				SQLCA.nf_handle_error('w_prepare_annuity_account', 'embedded SQL: UPDATE ANNUITY_PAYOUT SET annuity_payout_status_code = C...', 'uo_confirm_payout_checklist.ue_checklist_itemchangeaccepted')
			ELSE
				IF ib_no_payout THEN
					IF ib_non_zero_subledger_warning THEN
						ib_non_zero_subledger_warning = FALSE
						wf_reinsert_onto_annuity_payout_list()
					END IF
				END IF
			END IF
		
			lb_commit = TRUE
			
	END CHOOSE
		
END IF

IF lb_commit = TRUE THEN
	SQLCA.nf_commit_transaction()
	
	IF ib_checklist_cancelled THEN
		MessageBox('Annuity Payout List','The individual has been added back onto the Prepare Annuity Payout list.')
		ib_checklist_cancelled = FALSE
	END IF
	
	//reset variable
	ib_auto_authorization = FALSE
	
END IF

// if the checklist has been committed
IF lb_commit THEN
	
	IF ib_no_payout THEN
		// reset variable for next person
		ib_no_payout = FALSE
		
		// if we are allowing the user to enter a comment
		IF ib_optional_user_payout_comment OR ib_required_user_payout_comment THEN
			wf_annuity_payout_comment('no')
		END IF
		
	ELSEIF ib_optional_user_payout_comment THEN
		wf_annuity_payout_comment('yes')
	ELSEIF ib_required_user_payout_comment THEN
		wf_annuity_payout_comment('no')
	END IF
END IF


IF lb_contract_received THEN
	inv_common_annuity.nf_open_confirm_contract_reminder(il_annuity_payout_no,il_individual_no,il_annuity_account_claim_no,is_claim_role_code)
END IF

IF lb_commit THEN
	wf_post_commit_retrieval()
END IF
end event

event ue_checklist_rowfocuschanged;call super::ue_checklist_rowfocuschanged;STRING     ls_checklist_step_type_code


IF THIS.tab_checklist.tabpage_checklist.dw_checklist_master.RowCount() > 0 THEN
	
	ls_checklist_step_type_code = adw_dw.GetItemString(CurrentRow,'checklist_step_type_code')
	
	IF adw_dw.ClassName() = 'dw_checklist' THEN
		
		IF is_checklist_type_code = 'CAPLS' THEN
			CHOOSE CASE ls_checklist_step_type_code
				CASE '001'
					// Identified
					
				CASE '018'
					// Confirm Payout Amount
					
				CASE '019'
					// Request Authorization
					
				CASE '020'
					// Notification
					
				CASE '023'
					// Request Payout
					
				CASE '024'
					// Checklist Completed
					
			END CHOOSE
			
		ELSEIF is_checklist_type_code = 'CAPAP' THEN
			CHOOSE CASE ls_checklist_step_type_code
				CASE '001'
					// Identified
					
				CASE '018'
					// Confirm Payout Amount
					
				CASE '019'
					// Confirm Overpayment Recovery
					
				CASE '020'
					// Notification
					
				CASE '021'
					// Contract Received
					
				CASE '022'
					// Contract Confirmed
					
				CASE '023'
					// Request Payout
					
				CASE '024'
					// Checklist Completed
					
			END CHOOSE
		END IF
			
	END IF
	
END IF
end event

event ue_checklist_itemchanged;call super::ue_checklist_itemchanged;BOOLEAN        lb_all_payments_processed
INTEGER        li_count, li_authorization_count, li_annuity_payout_letter_count, li_message_rtn, li_contract_count, li_rtn, li_trancount
INTEGER        li_current_length_name_on_cheque
STRING         ls_dwo_name, ls_checklist_step_type_code, ls_error_message, ls_request_type_code
DWItemStatus   ldwis

ls_dwo_name = dwo.name
IF ls_dwo_name = 'checklist_step_status_code' THEN
	
	IF ib_save THEN
		wf_reject_checklist_status_change(TRUE,'Save Required','You have pending changes that need to be saved. Please Save or Cancel',il_checklist_no,'PAA',inv_checklist,adw_dw)
		al_return = 1
		GOTO label_end
	ELSE
		
		ls_checklist_step_type_code = adw_dw.GetItemString(row,'checklist_step_type_code')
		
		CHOOSE CASE ls_checklist_step_type_code
			CASE '001'
				// Identified
				
			CASE '018'
				// Confirm Payout Amount
				
			CASE '019'
				// Request Authorization
				IF data = 'COM' THEN
					li_count = wf_request_authorization_event_count()
					IF li_count = 0 THEN					
						wf_reject_checklist_status_change(TRUE,'BR 9.280, 9.290 - Annuity Problem','You must create a "request for authorization" event before this step can be set to "completed".',il_checklist_no,is_checklist_type_code,inv_checklist,adw_dw)
						al_return = 1				
						GOTO label_end
					END IF
				END IF
				
				
			CASE '020'
				// Notification
				IF data = 'COM' THEN
					li_authorization_count = wf_verify_payout_authorization()
					IF li_authorization_count = 1 THEN
						// OK
					ELSE
						wf_reject_checklist_status_change(TRUE,'BR 9.310 - Annuity Payout NOT Authorized','This annuity payout is not authorized. You cannot complete the notification checklist step until the payout is authorized.',il_checklist_no,is_checklist_type_code,inv_checklist,adw_dw)
						al_return = 1
						GOTO label_end
					END IF
					
					li_annuity_payout_letter_count = wf_verify_annuity_payout_letter()
					IF li_annuity_payout_letter_count > 0 THEN
						// OK - an appropriate letter has been created and sent
					ELSE
						wf_reject_checklist_status_change(TRUE,'BR 9.320, 9.330 - No Annuity Notification','An appropriate annuity payout notification has either not been created or has not been sent.',il_checklist_no,is_checklist_type_code,inv_checklist,adw_dw)
						al_return = 1
						GOTO label_end
					END IF
				ELSE
					// do not perform the checks above if being automatically set to NRA (not required)
				END IF
				
			CASE '021'
				// Contract Received (does not apply for CAPLS)
				IF data = 'COM' THEN
					// do not display messagebox if step is being completed
					// as 'not required - automatic', i.e., NRA
					
					// display a warning
					li_message_rtn = MessageBox('Annuity Contract Warning','Please ensure that the annuity contract has been received. Would you like to continue completing this checklist step?',Exclamation!,YesNo!,2)
					IF li_message_rtn <> 1 THEN
						wf_reject_checklist_status_change(FALSE,'','',il_checklist_no,is_checklist_type_code,inv_checklist,adw_dw)
						al_return = 1
						GOTO label_end
					END IF
				ELSE
					// do not perform the checks above if being automatically set to NRA (not required)
				END IF
				
			CASE '022'
				// Contract Confirmed (does not apply for CAPLS)
				IF data = 'COM' THEN
					
					// BR 9.400	The Confirm Annuity Payout, Contract Confirmed checklist step must not be completed,
					//          if the annuity contract and beneficiaries have not been entered.
					// verify that contract & beneficiaries exist
					li_contract_count = wf_verify_contract_confirmed()
					IF li_contract_count = 0 THEN
						wf_reject_checklist_status_change(TRUE,'BR 8.50, 8.60 - Save Annuity Contract','You cannot complete this checklist step until an annuity contract and beneficiary(ies) has(have) been saved.',il_checklist_no,is_checklist_type_code,inv_checklist,adw_dw)
						al_return = 1
						GOTO label_end
					END IF
					
					// determine if name has changed since Confirm OP recovery checklist step
					// and allow users to stop completion of checklist step
					li_rtn = wf_determine_name_address_change()
					IF li_rtn < 0 THEN
						wf_reject_checklist_status_change(FALSE,'','',il_checklist_no,is_checklist_type_code,inv_checklist,adw_dw)
						al_return = 1				
						GOTO label_end
					ELSEIF li_rtn = 1 THEN
						// opening of window triggered the posted itemchangeaccepted
						// so need to re-post
						post function uf_trigger_itemchangeaccepted(adw_dw,row,ls_dwo_name,data)
					ELSE
						// li_rtn = 0 - no change, do nothing
					END IF
									
					// give user another chance to change name/carrier
					li_rtn = wf_build_annuity_purchase_name_on_chq(is_purchase_name_on_cheque,ib_more_than_forty_chars,TRUE,'and Confirm Contract','')
					IF li_rtn < 0 THEN
						wf_reject_checklist_status_change(FALSE,'','',il_checklist_no,is_checklist_type_code,inv_checklist,adw_dw)
						al_return = 1				
						GOTO label_end
					END IF	
				ELSE
					// do not perform the checks above if being automatically set to NRA (not required)
				END IF
				
			CASE '023'
				// Request Payout
				
				IF data = 'COM' THEN
					
					// verify that request payout event has been created
					li_count = wf_request_payout_event_count(ls_request_type_code)
					IF li_count = 0 THEN
						IF ls_request_type_code = 'request' THEN
							wf_reject_checklist_status_change(TRUE,'BR 9.440, 9.460 - Annuity Event Problem','You must create a "request for payout" event before this step can be set to "completed".',il_checklist_no,is_checklist_type_code,inv_checklist,adw_dw)
						ELSE
							wf_reject_checklist_status_change(TRUE,'BR 9.450, 9.470 - Annuity Event Problem','You must create a "annuity write-off" event before this step can be set to "completed".',il_checklist_no,is_checklist_type_code,inv_checklist,adw_dw)
						END IF
						
						al_return = 1				
						GOTO label_end
					END IF
					
					li_authorization_count = wf_verify_payout_authorization()
					IF li_authorization_count = 1 THEN
						// OK
					ELSE
						wf_reject_checklist_status_change(TRUE,'BR 9.405 - Annuity Payout NOT Authorized','This annuity payout is not authorized. You cannot complete the Request Payout checklist step until the payout is authorized.',il_checklist_no,is_checklist_type_code,inv_checklist,adw_dw)
						al_return = 1
						GOTO label_end
					END IF
					
					// verify that annuity payout recipient address contains at least two lines
					
					wf_build_annuity_purchase_name_on_chq(is_purchase_name_on_cheque,ib_more_than_forty_chars,FALSE,'','')
					
					// verify that annuity payout recipient address contains at least two lines
					li_count = wf_verify_recipient_address()
					IF li_count > 0 THEN
						wf_reject_checklist_status_change(TRUE,'Annuity Problem','The first two address lines below the name must be populated for each annuity payout recipient before this step can be set to "completed".',il_checklist_no,is_checklist_type_code,inv_checklist,adw_dw)
						tab_payout.SelectTab(RECIPIENTS)
						wf_retrieve_tab(RECIPIENTS)
						al_return = 1
						GOTO label_end
					END IF
					
					IF is_checklist_type_code = 'CAPLS' THEN
						// determine if name/address has changed since Confirm OP recovery checklist step
						// and allow users to stop completion of checklist step
						li_rtn = wf_determine_name_address_change()
						IF li_rtn < 0 THEN
							wf_reject_checklist_status_change(FALSE,'','',il_checklist_no,is_checklist_type_code,inv_checklist,adw_dw)
							al_return = 1				
							GOTO label_end
						ELSEIF li_rtn = 1 THEN
							// opening of window triggered the posted itemchangeaccepted
							// so need to re-post
							post function uf_trigger_itemchangeaccepted(adw_dw,row,ls_dwo_name,data)
						ELSE
							// li_rtn = 0 - no change, do nothing
						END IF
						
						// lump sum payout will have its addres s& name updated from INDIVIDUAL and PROVIDER
						li_rtn = wf_determine_lump_sum_recipient(ls_error_message)
						IF li_rtn < 0 THEN
							wf_reject_checklist_status_change(TRUE,'Annuity Payout Recipient Error',ls_error_message,il_checklist_no,is_checklist_type_code,inv_checklist,adw_dw)
							al_return = 1
							GOTO label_end
						END IF
					ELSE
						// annuity purchase
						// determine if name has changed since the last time that the annuity recipient was updated.
						li_rtn = wf_determine_name_address_change()
						IF li_rtn < 0 THEN
							wf_reject_checklist_status_change(FALSE,'','',il_checklist_no,is_checklist_type_code,inv_checklist,adw_dw)
							al_return = 1				
							GOTO label_end
						ELSEIF li_rtn = 1 THEN						
							// opening of window triggered the posted itemchangeaccepted
							// so need to re-post
							post function uf_trigger_itemchangeaccepted(adw_dw,row,ls_dwo_name,data)
						ELSE
							// li_rtn = 0, window display not necessary
						END IF
	
					END IF
					
					
					// BR 9.420 The Confirm Annuity Payout, Request Payout checklist step must not be completed,
					//          if there are unprocessed post-1992 annuity sub-ledger payment transactions associated with the annuity account for an injured worker.
					
					// BR 9.430 The Confirm Annuity Payout, Request Payout checklist step must not be completed,
					//          if there are unprocessed annuity sub-ledger payment transactions associated with the annuity account for a surviving spouse.
					li_rtn = wf_verify_subledger(ls_error_message)
					IF li_rtn < 0 THEN
						wf_reject_checklist_status_change(TRUE,'Annuity Problem',ls_error_message,il_checklist_no,is_checklist_type_code,inv_checklist,adw_dw)
						al_return = 1				
						GOTO label_end
					END IF
				END IF
				
							
			CASE '024'
				// Checklist Completed
				IF String(data) = 'XCM' OR String(data) = 'XCA' THEN
					
					ib_checklist_cancelled = TRUE
					ib_checklist_completed = FALSE
					
					// if the status/reason is not validated, an app error
					inv_common_annuity.nf_validate_annuity_status(il_annuity_payout_no,'X','')
					
				ELSE
					ib_checklist_cancelled = FALSE
					
					IF ib_no_payout THEN
						// no payout, so no need to check for unprocessed txn count
					ELSE
						ib_checklist_completed = TRUE
						li_count = wf_unprocessed_payout_txn_count()
						IF li_count = 0 THEN
							// if the status/reason is not validated, an app error
							inv_common_annuity.nf_validate_annuity_status(il_annuity_payout_no,'C','')
						
						ELSE
							// there are UNAPPLIED_CLAIM_TXN records for ANNUITY_PAYOUT_TXN_DETAIL
							wf_reject_checklist_status_change(TRUE,'BR 9.480 - Annuity Problem','There are transactions associated with this payout ('+String(il_annuity_payout_no)+') that are unprocessed.'&
														  +'~r~nAll these transactions must be processed before this step can be completed.',il_checklist_no,is_checklist_type_code,inv_checklist,adw_dw)
							al_return = 1				
							GOTO label_end
						END IF
					END IF
				END IF
		END CHOOSE
	END IF
	
ELSEIF dwo.Name = 'step_checked' THEN
	SetRedraw(TRUE)
	al_return = 2
	GOTO label_end
END IF

ldwis = adw_dw.GetItemStatus(row,ls_dwo_name,Primary!)
//messagebox('descendant','itemchanged - end')

adw_dw.SetItemStatus(row,ls_dwo_name,Primary!,DataModified!)

ldwis = adw_dw.GetItemStatus(row,ls_dwo_name,Primary!)


SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
IF li_trancount = 0 THEN
	SQLCA.nf_begin_transaction()
END IF


al_return = 0

label_end:
end event

event ue_checklist_step_status_changed;call super::ue_checklist_step_status_changed;INTEGER         li_rtn
DATAWINDOWCHILD ldwc_child

IF as_status_assigned_method_code = '' THEN
	
	li_rtn = uo_confirm_payout_checklist.tab_checklist.tabpage_checklist.dw_checklist.GetChild('checklist_step_status_code',ldwc_child)
	
	as_status_assigned_method_code = ldwc_child.GetItemString(ldwc_child.GetRow(),'status_assigned_method_code')

END IF
end event

event ue_checklist_master_itemchanged;call super::ue_checklist_master_itemchanged;BOOLEAN         lb_payout_can_be_reversed
INTEGER         li_rtn, li_msg_rtn, li_rows, li_writeoffs_found, li_non_writeoffs_found, li_trancount
DATAWINDOWCHILD ldwc_child
STRING          ls_checklist_step_status_code, ls_payout_requested_by_user_id, ls_find, ls_payment_sub_type_phrase
U_DS            lds_annuity_payout_payment_sub_types

	
IF ib_save THEN
	MessageBox('Save Required','You have pending changes that need to be saved. Please Save or Cancel',Exclamation!)
	al_return = 1
	RETURN
END IF

// verify that user is not trying to cancel checklist after the CAP request payout step has been completed
IF data = 'XM' THEN
	
	SELECT checklist_step_status_code
	INTO   :ls_checklist_step_status_code
	FROM   CHECKLIST_STEP
	WHERE  checklist_step_type_code = '023'
	AND    checklist_no = :il_checklist_no
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT checklist_step_status_code FROM CHECKLIST_STEP...','uo_confirm_payout_checklist')
	
	
	IF ls_checklist_step_status_code = 'INA' THEN
		// if the CAPAP/CAPLS checklist has been created,
		// but the 'Request Payout' step has not been completed
		// then determine if OP needs to be reversed
//		wf_reverse_confirm_OP_recovery()
		ib_reverse_confirm_OP_recovery = TRUE
	ELSE
		// if the CAPAP/CAPLS checklist has been created,
		// AND the 'Request Payout' step has been completed
		// then determine if payment,txn & txn unit of work can be deleted
		// (& OP records reversed if they exist)
		lb_payout_can_be_reversed = inv_common_annuity.nf_determine_request_payout_status(il_annuity_payout_no)
		
		
		
		
		IF lb_payout_can_be_reversed = TRUE THEN
			ls_payout_requested_by_user_id = dw_payout_accounts.GetItemString(dw_payout_accounts.GetRow(),'requested_by_user_id')
			
			IF IsNull(ls_payout_requested_by_user_id) THEN ls_payout_requested_by_user_id = ''
			
			
			// determine the nature of payout request, so that message is specific to the situation
			// i.e., no write-off; some write-off, some non-write-off; all write-off
			lds_annuity_payout_payment_sub_types = Create U_DS
			lds_annuity_payout_payment_sub_types.DataObject = 'ds_annuity_payout_payment_sub_types'
			lds_annuity_payout_payment_sub_types.SetTransObject(SQLCA)
			
			li_rows = lds_annuity_payout_payment_sub_types.Retrieve(il_annuity_payout_no)
			SQLCA.nf_handle_error('w_prepare_annuity_account','lds_annuity_payout_payment_sub_types.Retrieve','uo_confirm_payout_checklist.ue_checklist_master_itemchanged')
			
			ls_find = 'payment_sub_type_code = "CW"'
			li_writeoffs_found = lds_annuity_payout_payment_sub_types.Find(ls_find,1,li_rows)
				
			ls_find = 'payment_sub_type_code <> "CW"'
			li_non_writeoffs_found = lds_annuity_payout_payment_sub_types.Find(ls_find,1,li_rows)
			
			IF li_non_writeoffs_found > 0 THEN
				IF li_writeoffs_found > 0 THEN
					ls_payment_sub_type_phrase = 'the cheque number has not yet been assigned and the associated payments have not been processed'
				ELSE
					ls_payment_sub_type_phrase = 'the cheque number has not yet been assigned'
				END IF
			ELSE
				// li_writeoffs_found > 0, but li_non_writeoffs_found > 0
				ls_payment_sub_type_phrase = 'the associated payments have not been processed'
			END IF
			
			
			IF ls_payout_requested_by_user_id <> '' THEN
				li_msg_rtn = MessageBox('BR 9.70 - Cancel Annuity Payout','You have chosen to cancel this annuity payout. ' &
																					  + '~r~n' &
																					  + 'The annuity payout has been requested BUT '+ls_payment_sub_type_phrase+'. '&
																					  + 'If you choose to cancel this annuity payout, you MUST contact Financial Services '&
																					  + 'to prevent the issuance of the requested payout (for example, do not issue cheque,' &
																					  + 'stop payment, cancel cheque, general ledger postings, etc)'&
																					  +'~r~n' &
																					  +'~r~nWould you like to continue with the cancellation of this annuity payout?', Question!,YesNo!,2)
				IF li_msg_rtn = 1 THEN
//					wf_reverse_request_payout()
               ib_reverse_request_payout = TRUE
					al_return = 0
				ELSE
					al_return = 1
					RETURN
				END IF
			ELSE
				// payout not requested yet
				al_return = 0
			END IF
		ELSE
			// payout cannot be reversed because the request has proceeded too far
			MessageBox('BR 9.80 - Cancel Annuity Payout','You have chosen to cancel this annuity payout. ' &
																	+ '~r~n' &
																	+ 'The annuity payout has been requested and a cheque number has been assigned or associated payments have been processed.'&
																	+ '~r~nThis payout can no longer be cancelled. You MUST contact Financial Services '&
																	+ 'to prevent the issuance of the requested payout, if possible (for example, do not issue cheque,' &
																	+ 'stop payment, cancel cheque, general ledger postings, etc).'&																	
																	+'~r~nAfter you have coordinated with Finance, the payout must be completed.',Exclamation!)
			al_return = 1
			RETURN
		END IF
	END IF
	
END IF


IF as_status_assigned_method_code = '' THEN
	
	li_rtn = uo_confirm_payout_checklist.tab_checklist.tabpage_checklist.dw_checklist_master.GetChild('checklist_status_code',ldwc_child)
	
	as_status_assigned_method_code = ldwc_child.GetItemString(ldwc_child.GetRow(),'status_assigned_method_code')

END IF

SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
IF li_trancount = 0 THEN
	SQLCA.nf_begin_transaction()
END IF



end event

event ue_checklist_master_itemchangeaccepted;call super::ue_checklist_master_itemchangeaccepted;IF ib_reverse_confirm_OP_recovery THEN
	// reset variable
	ib_reverse_confirm_OP_recovery = FALSE
	
	// and perform reversal of recovery
	wf_reverse_confirm_OP_recovery()
END IF

IF ib_reverse_request_payout THEN
	// reset variable
	ib_reverse_request_payout = FALSE
	
	// and perform reversal of recovery
	wf_reverse_request_payout()
END IF



end event

type dw_annuity_history from u_datawindow within w_prepare_annuity_account
boolean visible = false
integer x = 645
integer y = 8
integer width = 5321
integer height = 84
integer taborder = 20
string dataobject = "d_annuity_payout_history"
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;this.settransobject(SQLCA)


end event

event itemchanged;call super::itemchanged;long ll_annuity_payout_no, ll_row
datawindowchild ldw_child

this.getChild('display', ldw_child)

ll_row = ldw_child.getrow()

IF ll_row <= 0 THEN SignalError(-666,"No data retrieved in the annuity drop down list. Call the help desk.")

ll_annuity_payout_no = ldw_child.getItemNumber(ldw_child.getrow(),'annuity_payout_no')

//// need to re-retrieve each time or the filtering will not work properly on subsequent item changes
//this.retrieve(il_individual_no)
//this.setFilter("annuity_payout_no =" + string(ll_annuity_payout_no))
//this.filter()

dw_payout_accounts.setFilter('annuity_payout_no = ' + string(ll_annuity_payout_no))
dw_payout_accounts.filter()

dw_payout_accounts.event rowFocusChanged(1)
dw_payout_accounts.setFocus()
dw_payout_accounts.selectRow(1, TRUE)



end event

event clicked;call super::clicked;DATAWINDOWCHILD     ldwc_child
INTEGER             li_inserted_row


dw_annuity_history.getChild('display', ldwc_child)



//ldwc_child.Object.drop_down_display.Visible = '0'
//
//ldwc_child.Object.annuity_payout_no_t.Visible = '1'
//ldwc_child.Object.annuity_payout_status_desc_e_t.Visible = '1'
//ldwc_child.Object.annuity_payout_status_reason_desc_e_t.Visible = '1'
//ldwc_child.Object.last_stage_completed_t.Visible = '1'
//ldwc_child.Object.annuity_payout_no.Visible = '1'
//ldwc_child.Object.annuity_payout_status_desc_e.Visible = '1'
//ldwc_child.Object.annuity_payout_status_reason_desc_e.Visible = '1'
//ldwc_child.Object.last_stage_completed.Visible = '1'
//

end event

type st_multiple_accounts from statictext within w_prepare_annuity_account
integer x = 3639
integer y = 760
integer width = 1275
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 134217857
long backcolor = 67108864
string text = "Multiple annuity accounts exist for this Individual"
boolean focusrectangle = false
end type

