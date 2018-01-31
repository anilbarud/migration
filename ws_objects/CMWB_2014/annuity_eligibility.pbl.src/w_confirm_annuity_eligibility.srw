$PBExportHeader$w_confirm_annuity_eligibility.srw
forward
global type w_confirm_annuity_eligibility from w_ancestor
end type
type cb_annuity_benefit_inquiry from commandbutton within w_confirm_annuity_eligibility
end type
type st_last_generated from statictext within w_confirm_annuity_eligibility
end type
type st_last_generated_dtm from statictext within w_confirm_annuity_eligibility
end type
type cb_generate_list from commandbutton within w_confirm_annuity_eligibility
end type
type st_run_option from statictext within w_confirm_annuity_eligibility
end type
type dw_cae_tombstone from u_datawindow within w_confirm_annuity_eligibility
end type
type st_multiple_annuity_accounts from statictext within w_confirm_annuity_eligibility
end type
type dw_annuity_eligibility_run_option from u_datawindow within w_confirm_annuity_eligibility
end type
type tab_confirm_eligibility from tab within w_confirm_annuity_eligibility
end type
type tabpage_new from userobject within tab_confirm_eligibility
end type
type dw_new_ss_eligibility from u_datawindow within tabpage_new
end type
type dw_new_iw_eligibility from u_datawindow within tabpage_new
end type
type tabpage_new from userobject within tab_confirm_eligibility
dw_new_ss_eligibility dw_new_ss_eligibility
dw_new_iw_eligibility dw_new_iw_eligibility
end type
type tabpage_changed from userobject within tab_confirm_eligibility
end type
type dw_changed_ss_eligibility from u_datawindow within tabpage_changed
end type
type dw_changed_iw_eligibility from u_datawindow within tabpage_changed
end type
type tabpage_changed from userobject within tab_confirm_eligibility
dw_changed_ss_eligibility dw_changed_ss_eligibility
dw_changed_iw_eligibility dw_changed_iw_eligibility
end type
type tabpage_payout from userobject within tab_confirm_eligibility
end type
type dw_iw_payout from u_datawindow within tabpage_payout
end type
type dw_ss_payout from u_datawindow within tabpage_payout
end type
type tabpage_payout from userobject within tab_confirm_eligibility
dw_iw_payout dw_iw_payout
dw_ss_payout dw_ss_payout
end type
type tab_confirm_eligibility from tab within w_confirm_annuity_eligibility
tabpage_new tabpage_new
tabpage_changed tabpage_changed
tabpage_payout tabpage_payout
end type
type tab_eligibility from tab within w_confirm_annuity_eligibility
end type
type tabpage_current_eligibility from userobject within tab_eligibility
end type
type dw_current_eligibility from u_datawindow within tabpage_current_eligibility
end type
type tabpage_current_eligibility from userobject within tab_eligibility
dw_current_eligibility dw_current_eligibility
end type
type tabpage_history_eligibility from userobject within tab_eligibility
end type
type dw_history_eligibility from u_datawindow within tabpage_history_eligibility
end type
type tabpage_history_eligibility from userobject within tab_eligibility
dw_history_eligibility dw_history_eligibility
end type
type tab_eligibility from tab within w_confirm_annuity_eligibility
tabpage_current_eligibility tabpage_current_eligibility
tabpage_history_eligibility tabpage_history_eligibility
end type
type uo_checklist from u_checklist within w_confirm_annuity_eligibility
end type
end forward

global type w_confirm_annuity_eligibility from w_ancestor
integer width = 5385
integer height = 2936
string title = "Confirm Annuity Eligibility"
string menuname = "m_annuity"
windowtype windowtype = main!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
cb_annuity_benefit_inquiry cb_annuity_benefit_inquiry
st_last_generated st_last_generated
st_last_generated_dtm st_last_generated_dtm
cb_generate_list cb_generate_list
st_run_option st_run_option
dw_cae_tombstone dw_cae_tombstone
st_multiple_annuity_accounts st_multiple_annuity_accounts
dw_annuity_eligibility_run_option dw_annuity_eligibility_run_option
tab_confirm_eligibility tab_confirm_eligibility
tab_eligibility tab_eligibility
uo_checklist uo_checklist
end type
global w_confirm_annuity_eligibility w_confirm_annuity_eligibility

type variables
BOOLEAN				ib_checklist_posted, ib_set_eligibility_comment, ib_auto_not_allow_calc_step, ib_rfc_warning
BOOLEAN           ib_individual_does_not_exist, ib_PAA_individual, ib_prevent_VBE_step_warning

LONG					il_claim_no, il_individual_no, il_annuity_eligibility_for_comment, il_annuity_payout_no
STRING				is_annuity_eligibility_run_option_code, is_claim_role_code, is_list_mode
STRING				is_eligibility_comment_message, is_auto_calculation_step_status

n_common_annuity	inv_common_annuity
n_checklist			inv_checklist

ULONG					iul_handle

U_DS					ids_reexamine_benefit_entitlement, ids_annuity_payout

U_DATAWINDOW      idw_iw_payout, idw_ss_payout

CONSTANT INTEGER	NEW_LIST = 1
CONSTANT INTEGER	CHANGED_LIST = 2
CONSTANT INTEGER  PAYOUT_LIST = 3
end variables

forward prototypes
public subroutine wf_clear_datawindows ()
public function integer wf_create_mail_package (long al_claim_no)
public function integer wf_get_active_checklist_count (long al_annuity_account_no)
public function integer wf_get_annuity_eligibility_count (long al_individual_no, long al_claim_no, string as_annuity_eligibility_status_code)
public function integer wf_get_selected_confirm_eligibility_tab ()
public subroutine wf_insert_annuity_eligibility_run (string as_annuity_eligibility_run_option_code)
public subroutine wf_print ()
public function integer wf_select_ae_run_option ()
public subroutine wf_switch_dw_type (string as_annuity_eligibility_run_option)
public function integer wf_set_current_ae_filter (string as_filter)
public function integer wf_scroll_to_individual (u_datawindow adw_eligibility, long al_individual_no, long al_checklist_no, string as_checklist_type_code)
public subroutine wf_select_next_available_checklist_step (long al_checklist_no, string as_checklist_type_code)
public subroutine wf_select_tab (integer ai_new_rows, integer ai_changed_rows)
public function long wf_retrieve_confirm_eligibility (long al_individual_no, long al_claim_no, string as_checklist_type_code)
public subroutine wf_retrieve_annuity_eligilbility (long al_annuity_account_no)
public function long wf_retrieve_all_surviving_spouses ()
public function long wf_retrieve_all_injured_workers ()
public function long wf_generate_all_surviving_spouses ()
public function integer wf_generate_all_injured_workers ()
public subroutine wf_confirm_dw_rfc (string as_checklist_type_code, long al_annuity_account_no, long al_checklist_no, u_datawindow adw_dw, integer ai_row)
public subroutine wf_set_eligibility_datawindow (ref u_datawindow adw_eligibility)
public function integer wf_set_entitled_data (integer ai_pending_row, integer ai_active_row, integer ai_selected_tab, long al_individual_no, long al_claim_no)
public function integer wf_populate_checklist_and_annuity (long al_individual_no, long al_claim_no, long al_annuity_account_no, string as_checklist_type_code)
public function long wf_get_claim_no ()
public function long wf_get_individual_no ()
public function string wf_get_claim_role_code ()
public function string wf_get_module_code ()
public function w_confirm_annuity_eligibility wf_get_window_reference ()
public function integer wf_verify_new_list (long al_individual_no, long al_claim_no)
public function long wf_generate_injured_worker ()
public function long wf_active_annuity_eligibility_count (long al_claim_no, long al_individual_no)
public function long wf_generate_surviving_spouse ()
public function long wf_retrieve_surviving_spouse (long al_claim_no, long al_individual_no)
public function long wf_retrieve_injured_worker (long al_individual_no)
public function integer wf_set_not_entitled_data (integer ai_pending_row, integer ai_selected_tab, string as_extra_message)
public subroutine wf_post_resize_checklist_position ()
public subroutine wf_update_annuity_eligibility_run (string as_annuity_eligibility_run_option_code)
public function integer wf_get_ae_run_option_count (string as_annuity_eligibility_run_option_code)
public function integer wf_get_last_confirmed_data (ref datetime adtm_last_confirmed, ref datetime adtm_annuity_start_date, ref datetime adtm_annuity_end_date, ref string as_benefit_option_code, ref decimal adec_annuity_set_aside_percent)
public subroutine wf_reject_checklist_status_change (boolean ab_message, string as_message_title, string as_message, long al_checklist_no, string as_checklist_type_code)
public function integer wf_incomplete_prepare_acct_vbe_checklist (long al_verify_be_checklist_no, long al_annuity_account_no)
public subroutine wf_set_process_run_status (string as_in_progress_flag)
public function datetime wf_get_ae_last_run_date (string as_annuity_eligibility_run_option_code)
public subroutine wf_set_last_generated_datetime (boolean ab_visible, datetime adtm_last_generated)
public function integer wf_confirm_annuity_eligibility (long al_annuity_account_no, long al_individual_no, long al_claim_no)
public function integer wf_get_last_confirmed_data (ref long al_last_inactive_annuity_eligibility_no)
public function integer wf_check_missing_annuity_interest_rate (long al_annuity_eligibility_no)
protected function boolean wf_auto_not_require_calc_step (string as_claim_role_code, long al_annuity_account_no, long al_annuity_eligibility_no, long al_individual_no, long al_claim_no, datetime adtm_new_annuity_start_date, datetime adtm_new_annuity_end_date, boolean ab_eligible)
public function integer wf_check_unapplied_manual_set_asides (long al_annuity_eligibility_no, string as_claim_role_code)
protected function boolean wf_auto_not_allow_calc_step (string as_claim_role_code, long al_annuity_account_no, long al_annuity_eligibility_no, long al_individual_no, long al_claim_no, datetime adtm_new_annuity_start_date, datetime adtm_new_annuity_end_date, boolean ab_eligible)
public subroutine wf_individual_error (string as_message)
public function integer wf_eligibility_dw_rowfocuschanging (u_datawindow adw_eligibility, long al_newrow, long al_currentrow)
public function integer wf_get_pre_1993_annuity_eligibility_flag (long al_annuity_account_no, ref string as_pre_1993_annuity_eligibility_flag)
public function integer wf_benefit_entitlement_checklist (long al_annuity_account_no, long al_annuity_eligibility_no, boolean ab_display_error_msgs)
public subroutine wf_setup_payout_controls ()
public subroutine wf_resize_module ()
protected function long wf_post_checklist_generation (long al_individual_no, long al_annuity_account_no, long al_claim_no, long al_checklist_subscriber_no, long al_checklist_no, string as_checklist_type_code, boolean ab_new_checklist_subscriber)
public function integer wf_merge_event_count (long al_individual_no, datetime adtm_last_confirmed, string as_event_specific_code)
public subroutine wf_determine_calc_step_auto_completion (string as_claim_role_code, long al_annuity_account_no, long al_annuity_eligibility_no, long al_individual_no, long al_claim_no, datetime adtm_annuity_start_date, datetime adtm_annuity_end_date, boolean ab_eligible, long al_checklist_no, string as_checklist_type_code, ref u_checklist_datawindow adw_dw, boolean ab_warning, ref boolean ab_prevent_warning, ref string as_auto_calculation_step_status, ref boolean ab_commit)
protected subroutine wf_enable_annuity_inquiry_btn ()
public function integer wf_get_last_confirmed_eligibility (ref long al_last_inactive_annuity_eligibility_no, long al_annuity_account_no)
public function integer wf_get_entitled_data (u_ds ads_reexamine_benefit_entitlement, long al_annuity_account_no, long al_individual_no, long al_claim_no, ref datetime adtm_annuity_start_date, ref datetime adtm_annuity_eligibility_end_date_used, ref datetime adtm_annuity_end_date, ref decimal adec_annuity_set_aside_percent)
end prototypes

public subroutine wf_clear_datawindows ();/*
function called if the checklist has been concluded
*/
INTEGER			li_selected_tab
STRING			ls_message

li_selected_tab = wf_get_selected_confirm_eligibility_tab()

IF is_annuity_eligibility_run_option_code = 'IW' THEN
	ls_message = 'injured worker'
	IF li_selected_tab = NEW_LIST THEN
		tab_confirm_eligibility.tabpage_new.dw_new_iw_eligibility.Reset()
	ELSEIF li_selected_tab = CHANGED_LIST THEN
		tab_confirm_eligibility.tabpage_changed.dw_changed_iw_eligibility.Reset()
	ELSEIF li_selected_tab = PAYOUT_LIST THEN
		idw_iw_payout.Reset()
	END IF
ELSE
	ls_message = 'surviving spouse'
	IF li_selected_tab = NEW_LIST THEN
		tab_confirm_eligibility.tabpage_new.dw_new_ss_eligibility.Reset()
	ELSEIF li_selected_tab = CHANGED_LIST THEN
		tab_confirm_eligibility.tabpage_changed.dw_changed_ss_eligibility.Reset()
	ELSEIF li_selected_tab = PAYOUT_LIST THEN
		idw_ss_payout.Reset()
	END IF
END IF
tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.Reset()
tab_eligibility.tabpage_history_eligibility.dw_history_eligibility.Reset()

dw_cae_tombstone.Reset()

uo_checklist.uf_clear_datawindows()

tab_confirm_eligibility.tabpage_new.Enabled = FALSE
tab_confirm_eligibility.tabpage_changed.Enabled = FALSE

tab_eligibility.tabpage_current_eligibility.Enabled = FALSE
tab_eligibility.tabpage_history_eligibility.Enabled = FALSE

end subroutine

public function integer wf_create_mail_package (long al_claim_no);/*
creates mail packages for 'new' injured workers who have qualified for an annuity
argument - al_claim_no
return		= 0 if OK
			= -1 if not OK
			
n_cst_remote_print is in rehab.pbl
*/
INTEGER					li_rtn
n_cst_remote_print	lnv_remote_print

//	Create the nonvisual user object for Remote Print.
lnv_remote_print = Create n_cst_remote_print
li_rtn = lnv_remote_print.of_init()
IF li_rtn <> 1 THEN
	Error.ObjectEvent='wf_create_mail_package calling of_init'
	Error.Text = 'Cannot create letter for Remote Print '
	RETURN li_rtn
END IF

li_rtn = lnv_remote_print.of_Set_Mail_Package_Values(al_claim_no, 'O', 'AQ', 'LC', "Annuity Qualificat'n", 'B')
IF li_rtn <> 1 THEN
	Error.ObjectEvent='wf_create_mail_package calling of_Set_Mail_Package_Values'
	Error.Text = 'Cannot create letter for Remote Print '
	RETURN li_rtn
END IF

li_rtn = lnv_remote_print.of_Insert_into_mail_package_table()
IF li_rtn <> 1 THEN
	Error.ObjectEvent='wf_create_mail_package calling of_Insert_into_mail_package_table'
	Error.Text = 'Cannot create letter for Remote Print '
	RETURN li_rtn
END IF

RETURN 1
end function

public function integer wf_get_active_checklist_count (long al_annuity_account_no);INTEGER		li_count

SELECT	IsNull(Count(*),0)
INTO		:li_count
FROM		ANNUITY_ACCOUNT					a
JOIN		CHECKLIST_SUBSCRIBER			b ON a.checklist_subscriber_no = b.checklist_subscriber_no
JOIN		SUBSCRIBER_CHECKLIST_XREF	c ON b.checklist_subscriber_no = c.checklist_subscriber_no
JOIN		CHECKLIST								d ON c.checklist_no = d.checklist_no
WHERE	d.checklist_status_code = 'IA'
AND		b.checklist_subscriber_type_code = 'ANN'
AND		d.checklist_type_code like 'CAE%'
AND		a.annuity_account_no = :al_annuity_account_no
USING SQLCA;

RETURN li_count

end function

public function integer wf_get_annuity_eligibility_count (long al_individual_no, long al_claim_no, string as_annuity_eligibility_status_code);INTEGER		li_count

/*
if there are pending annuity eligibility records for the situation below, then prevent inserting of ANNUITY_ELIGIBILITY records
*/

SELECT	Count(*)
INTO		:li_count
FROM		ANNUITY_ACCOUNT a
JOIN		ANNUITY_ELIGIBILITY b ON a.annuity_account_no = b.annuity_account_no
WHERE	a.individual_no = :al_individual_no
AND		a.claim_no = :al_claim_no
AND		b.annuity_eligibility_status_code = :as_annuity_eligibility_status_code
USING SQLCA;

RETURN li_count
end function

public function integer wf_get_selected_confirm_eligibility_tab ();INTEGER		li_upperBound, li_counter

// return selected tab as '1' only if the first tab is enabled
IF tab_confirm_eligibility.SelectedTab = 1 THEN
	
	li_upperBound = UpperBound(tab_confirm_eligibility.Control)
	
	FOR li_counter = 1 TO li_upperBound
		IF tab_confirm_eligibility.Control[li_counter].ClassName() = 'tabpage_new' THEN
			IF tab_confirm_eligibility.Control[li_counter].Enabled = TRUE AND is_list_mode <> 'payout' THEN
				RETURN 1
			END IF
		ELSEIF tab_confirm_eligibility.Control[li_counter].ClassName() = 'tabpage_changed' AND is_list_mode <> 'payout' THEN
			IF tab_confirm_eligibility.Control[li_counter].Enabled = TRUE THEN
				RETURN 2
			END IF
		ELSEIF tab_confirm_eligibility.Control[li_counter].ClassName() = 'tabpage_payout' THEN
			IF tab_confirm_eligibility.Control[li_counter].Enabled = TRUE THEN
				RETURN 3
			END IF
		END IF	
	NEXT
ELSE
	RETURN tab_confirm_eligibility.SelectedTab
END IF

RETURN 0
end function

public subroutine wf_insert_annuity_eligibility_run (string as_annuity_eligibility_run_option_code);
SQLCA.nf_begin_transaction()

INSERT	ANNUITY_ELIGIBILITY_RUN
(			annuity_eligibility_run_option_code )
SELECT	:as_annuity_eligibility_run_option_code
USING SQLCA;
SQLCA.nf_handle_error( 'w_confirm_annuity_eligibility', 'INSERT ANNUITY_ELIGIBILITY_RUN', 'wf_insert_annuity_eligibility_run')

SQLCA.nf_commit_transaction()

end subroutine

public subroutine wf_print ();u_datawindow	ldw_eligibility

wf_set_eligibility_datawindow(ldw_eligibility)
ldw_eligibility.Object.DataWindow.Print.Orientation = 1 //landscape
ldw_eligibility.Print()

end subroutine

public function integer wf_select_ae_run_option ();DataWindowChild	ldwc_run_option
LONG					ll_found
STRING				ls_annuity_eligibility_run_option_desc

dw_annuity_eligibility_run_option.GetChild('annuity_eligibility_run_option_code',ldwc_run_option)
ldwc_run_option.SetTransObject(SQLCA)
ldwc_run_option.Retrieve(vgst_user_profile.default_admin_region_code)
SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_select_ae_run_option', 'ldwc_run_option.Retrieve')

ll_found = ldwc_run_option.Find('annuity_eligibility_run_option_code= "' +is_annuity_eligibility_run_option_code+ '"',1,ldwc_run_option.RowCount())
IF ll_found = 0 THEN
	// problem
	MessageBox('No Admin Region','You do not have a default admin region set up in your User Profile.~r~n' &
										+ 'Please contact the help desk to have this set up.', Exclamation!)
	RETURN -1
ELSE
	ldwc_run_option.SetRow(ll_found)
	ldwc_run_option.ScrollToRow(ll_found)
	ls_annuity_eligibility_run_option_desc = ldwc_run_option.GetItemString(ll_found,'annuity_eligibility_run_option_desc')
	dw_annuity_eligibility_run_option.SetText(ls_annuity_eligibility_run_option_desc)
END IF

wf_switch_dw_type(is_annuity_eligibility_run_option_code)

RETURN 0
end function

public subroutine wf_switch_dw_type (string as_annuity_eligibility_run_option);CHOOSE CASE as_annuity_eligibility_run_option
	CASE 'IW','IA'
		tab_confirm_eligibility.tabpage_new.dw_new_iw_eligibility.Visible = TRUE
		tab_confirm_eligibility.tabpage_changed.dw_changed_iw_eligibility.Visible = TRUE
		tab_confirm_eligibility.tabpage_new.dw_new_ss_eligibility.Visible = FALSE
		tab_confirm_eligibility.tabpage_changed.dw_changed_ss_eligibility.Visible = FALSE
	CASE 'SS','SA'
		tab_confirm_eligibility.tabpage_new.dw_new_iw_eligibility.Visible = FALSE
		tab_confirm_eligibility.tabpage_changed.dw_changed_iw_eligibility.Visible = FALSE
		tab_confirm_eligibility.tabpage_new.dw_new_ss_eligibility.Visible = TRUE
		tab_confirm_eligibility.tabpage_changed.dw_changed_ss_eligibility.Visible = TRUE
END CHOOSE

end subroutine

public function integer wf_set_current_ae_filter (string as_filter);INTEGER		li_rtn
STRING		ls_no_filter


IF as_filter <> '' THEN
	// clear any existing filter
	li_rtn = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.SetFilter(ls_no_filter)
	li_rtn = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.Filter()
	
	// apply filter
	li_rtn = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.SetFilter(as_filter)
	li_rtn = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.Filter()
ELSE
	// clear filter
	li_rtn = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.SetFilter(as_filter)
	li_rtn = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.Filter()
END IF

RETURN li_rtn
end function

public function integer wf_scroll_to_individual (u_datawindow adw_eligibility, long al_individual_no, long al_checklist_no, string as_checklist_type_code);INTEGER	li_find, li_count
STRING   ls_message

// find the individual that had originally been selected, & re-select
li_find = adw_eligibility.uf_find_row('individual_no',String(al_individual_no))

IF li_find > 0 THEN
	adw_eligibility.ScrollToRow(li_find)
	adw_eligibility.SelectRow(li_find,TRUE)
	
	// find the next checklist step that can be selected & select it.
	wf_select_next_available_checklist_step(al_checklist_no,as_checklist_type_code)
ELSE
	// 2.370	Annuity eligibility work must not continue for an individual who has been merged 
	// since the last time the potential eligibility lists were retrieved.
	li_count = inv_common_annuity.nf_check_individual_exists(al_individual_no)
	IF li_count = 0 THEN
		ls_message = 'The individual number that you are working on no longer exists.'&
						+'~r~nThe individual may have been merged. The checklist step will not be concluded.'
		wf_individual_error(ls_message)
		RETURN -1
	END IF
END IF

RETURN 0
end function

public subroutine wf_select_next_available_checklist_step (long al_checklist_no, string as_checklist_type_code);INTEGER			li_find, li_next_incomplete_checklist_step_no


// find the next checklist step that can be selected & select it.
inv_checklist.nf_retrieve_checklists(as_checklist_type_code,al_checklist_no)
li_next_incomplete_checklist_step_no = inv_checklist.nf_get_next_checklist_step(al_checklist_no)

uo_checklist.tab_checklist.tabpage_checklist.dw_checklist.uf_find_row('checklist_step_no', String(li_next_incomplete_checklist_step_no))


end subroutine

public subroutine wf_select_tab (integer ai_new_rows, integer ai_changed_rows);IF ai_changed_rows = 0 THEN
	// no changed records
	tab_confirm_eligibility.tabpage_changed.Enabled = FALSE
ELSE
	tab_confirm_eligibility.tabpage_changed.Enabled = TRUE
END IF

IF ai_new_rows = 0 THEN
	// no new records
	tab_confirm_eligibility.tabpage_new.Enabled = FALSE
ELSE
	tab_confirm_eligibility.tabpage_new.Enabled = TRUE
END IF

IF ai_new_rows > 0 THEN
	tab_confirm_eligibility.post SelectTab(1)	
ELSE
	IF ai_changed_rows = 0 THEN
		tab_confirm_eligibility.post SelectTab(1)
	ELSE
		tab_confirm_eligibility.post SelectTab(2)
	END IF			
END IF

end subroutine

public function long wf_retrieve_confirm_eligibility (long al_individual_no, long al_claim_no, string as_checklist_type_code);INTEGER        li_current_row
LONG	   	   ll_rows
U_DATAWINDOW   ldw_eligibility


// retrieve confirm datawindows
CHOOSE CASE is_annuity_eligibility_run_option_code
	CASE 'IW'
		IF as_checklist_type_code = 'CAEIN' THEN
			ldw_eligibility = tab_confirm_eligibility.tabpage_new.dw_new_iw_eligibility
			li_current_row = ldw_eligibility.GetRow()
			ll_rows = tab_confirm_eligibility.tabpage_new.dw_new_iw_eligibility.Retrieve(al_individual_no,as_checklist_type_code)
			SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_retrieve_confirm_eligibility', 'IW dw retrieve')
		ELSEIF as_checklist_type_code = 'CAEIC' THEN
			ldw_eligibility = tab_confirm_eligibility.tabpage_changed.dw_changed_iw_eligibility
			li_current_row = ldw_eligibility.GetRow()
			ll_rows = tab_confirm_eligibility.tabpage_changed.dw_changed_iw_eligibility.Retrieve(al_individual_no,as_checklist_type_code)
			SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_retrieve_confirm_eligibility', 'IW dw retrieve')
		ELSEIF as_checklist_type_code = 'CAEP' THEN
			ldw_eligibility = idw_iw_payout
			li_current_row = ldw_eligibility.GetRow()
			ll_rows = idw_iw_payout.Retrieve(al_individual_no,as_checklist_type_code)
			SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_retrieve_confirm_eligibility', 'IW dw retrieve')
		END IF
		
	CASE 'IA'
		IF as_checklist_type_code = 'CAEIN' THEN
			ldw_eligibility = tab_confirm_eligibility.tabpage_new.dw_new_iw_eligibility
			li_current_row = ldw_eligibility.GetRow()
			ll_rows = tab_confirm_eligibility.tabpage_new.dw_new_iw_eligibility.Retrieve(vgst_user_profile.default_admin_region_code,as_checklist_type_code)
			SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_retrieve_confirm_eligibility', 'IA dw retrieve')
		ELSE
			ldw_eligibility = tab_confirm_eligibility.tabpage_changed.dw_changed_iw_eligibility
			li_current_row = ldw_eligibility.GetRow()
			ll_rows = tab_confirm_eligibility.tabpage_changed.dw_changed_iw_eligibility.Retrieve(vgst_user_profile.default_admin_region_code,as_checklist_type_code)
			SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_retrieve_confirm_eligibility', 'IA dw retrieve')
		END IF
		
	CASE 'SS'
		IF as_checklist_type_code = 'CAESN' THEN
			ldw_eligibility = tab_confirm_eligibility.tabpage_new.dw_new_ss_eligibility
			li_current_row = ldw_eligibility.GetRow()
			ll_rows = tab_confirm_eligibility.tabpage_new.dw_new_ss_eligibility.Retrieve(al_claim_no,al_individual_no,as_checklist_type_code)
			SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_retrieve_confirm_eligibility', 'SS dw retrieve')
		ELSEIF as_checklist_type_code = 'CAESC' THEN
			ldw_eligibility = tab_confirm_eligibility.tabpage_changed.dw_changed_ss_eligibility
			li_current_row = ldw_eligibility.GetRow()
			ll_rows = tab_confirm_eligibility.tabpage_changed.dw_changed_ss_eligibility.Retrieve(al_claim_no,al_individual_no,as_checklist_type_code)
			SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_retrieve_confirm_eligibility', 'SS dw retrieve')
		ELSEIF as_checklist_type_code = 'CAEP' THEN
			ldw_eligibility = idw_ss_payout
			li_current_row = ldw_eligibility.GetRow()
			ll_rows = idw_ss_payout.Retrieve(al_claim_no,al_individual_no,as_checklist_type_code)
			SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_retrieve_confirm_eligibility', 'SS dw retrieve')
		END IF
		
	CASE 'SA'
		IF as_checklist_type_code = 'CAESN' THEN
			ldw_eligibility = tab_confirm_eligibility.tabpage_new.dw_new_ss_eligibility
			li_current_row = ldw_eligibility.GetRow()
			ll_rows = tab_confirm_eligibility.tabpage_new.dw_new_ss_eligibility.Retrieve(as_checklist_type_code)
			SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_retrieve_confirm_eligibility', 'SA dw retrieve')
		ELSE
			ldw_eligibility = tab_confirm_eligibility.tabpage_changed.dw_changed_ss_eligibility
			li_current_row = ldw_eligibility.GetRow()
			ll_rows = tab_confirm_eligibility.tabpage_changed.dw_changed_ss_eligibility.Retrieve(as_checklist_type_code)
			SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_retrieve_confirm_eligibility', 'SA dw retrieve')
		END IF
		
END CHOOSE

IF li_current_row = 1 THEN
	// if current row is first row, then force the rowfocuschanged
	// this will force re-retrieval of all other datawindows
	ldw_eligibility.TRIGGER EVENT RowFocusChanged(1)
	
END IF

RETURN ll_rows
end function

public subroutine wf_retrieve_annuity_eligilbility (long al_annuity_account_no);INTEGER		li_rows

// current & history eligibility datawindows
li_rows = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.Retrieve(al_annuity_account_no)
SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_setup_rfc', 'dw_current_eligibility.retrieve')
li_rows = tab_eligibility.tabpage_history_eligibility.dw_history_eligibility.Retrieve(al_annuity_account_no)
SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_setup_rfc', 'dw_history_eligibility.retrieve ')

end subroutine

public function long wf_retrieve_all_surviving_spouses ();INTEGER		li_rows, li_new_rows, li_changed_rows
	
// retrieve new eligibilities
tab_confirm_eligibility.tabpage_new.dw_new_ss_eligibility.DataObject = 'd_ss_all_eligibility'
tab_confirm_eligibility.tabpage_new.dw_new_ss_eligibility.SetTransObject(SQLCA)
li_new_rows = tab_confirm_eligibility.tabpage_new.dw_new_ss_eligibility.Retrieve('CAESN')
SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_retrieve_all_surviving_spouses', 'new dw retrieve')

// retrieve changed eligibility
tab_confirm_eligibility.tabpage_changed.dw_changed_ss_eligibility.DataObject = 'd_ss_all_eligibility'
tab_confirm_eligibility.tabpage_changed.dw_changed_ss_eligibility.SetTransObject(SQLCA)
li_changed_rows = tab_confirm_eligibility.tabpage_changed.dw_changed_ss_eligibility.Retrieve('CAESC')
SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_retrieve_all_surviving_spouses', 'changed dw retrieve')

wf_select_tab(li_new_rows,li_changed_rows)

li_rows = li_new_rows + li_changed_rows

IF li_rows = 0 THEN
	wf_clear_datawindows()
	MessageBox('No records','The checklists for all the surviving spouses have been completed. There are no records left.')
END IF

RETURN li_rows
end function

public function long wf_retrieve_all_injured_workers ();INTEGER							li_rows, li_new_rows, li_changed_rows
	
// retrieve new eligibilities
tab_confirm_eligibility.tabpage_new.dw_new_iw_eligibility.DataObject = 'd_iw_all_eligibility'
tab_confirm_eligibility.tabpage_new.dw_new_iw_eligibility.SetTransObject(SQLCA)
li_new_rows = tab_confirm_eligibility.tabpage_new.dw_new_iw_eligibility.Retrieve(vgst_user_profile.default_admin_region_code,'CAEIN')

tab_confirm_eligibility.tabpage_new.dw_new_iw_eligibility.post SetColumn('individual_no')
tab_confirm_eligibility.tabpage_new.dw_new_iw_eligibility.post SelectText(1,1)

SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_retrieve_all_injured_workers', 'new dw retrieve')

// retrieve changed eligibility
tab_confirm_eligibility.tabpage_changed.dw_changed_iw_eligibility.DataObject = 'd_iw_all_eligibility'
tab_confirm_eligibility.tabpage_changed.dw_changed_iw_eligibility.SetTransObject(SQLCA)
li_changed_rows = tab_confirm_eligibility.tabpage_changed.dw_changed_iw_eligibility.Retrieve(vgst_user_profile.default_admin_region_code,'CAEIC')
SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_retrieve_all_injured_workers', 'changed dw retrieve')

wf_select_tab(li_new_rows,li_changed_rows)

li_rows = li_new_rows + li_changed_rows

IF li_rows = 0 THEN
	wf_clear_datawindows()
	MessageBox('No records','The checklists for all the injured workers have been completed. There are no records left.')
END IF

RETURN li_rows
end function

public function long wf_generate_all_surviving_spouses ();/*
Generates checklist data for all surviving spouses
no arguments
returns	= 0 if OK
			= -1 if not OK
*/

BOOLEAN			lb_new_checklist_subscriber
DATETIME			ldtm_null
INTEGER			li_rtn, li_count, li_PAA_rows, li_PAA_upperbound, li_PAA_counter
LONG				ll_rows, ll_new_rows, ll_changed_rows, ll_individual_no, ll_counter, ll_claim_no, ll_annuity_account_no
LONG				ll_new_checklist_no, ll_related_checklist_no, ll_annuity_eligibility_no, ll_checklist_subscriber_no
LONG				ll_PAA_individual_no, ll_PAA_claim_no
STRING         ls_checklist_type_code
U_DS				lds_identify_new_ss_annuity_eligibilities, lds_identify_changed_ss_annuity_eligibilities, lds_force_onto_payout
s_payout_individuals  lstr_payout_individuals[]


SetNull(ldtm_null)

// this function handles its own transaction
wf_set_process_run_status('Y')


// generate new surviving spouses
lds_identify_new_ss_annuity_eligibilities = create u_ds
lds_identify_new_ss_annuity_eligibilities.DataObject = 'ds_identify_new_ss_annuity_eligibilities'
lds_identify_new_ss_annuity_eligibilities.SetTransObject(SQLCA)


// this function handles its own transaction
wf_insert_annuity_eligibility_run('SA')


// identify potential annuity elig if it doesn't exist
ll_new_rows = lds_identify_new_ss_annuity_eligibilities.Retrieve(0)
SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_generate_all_surviving_spouses', 'retrieve p_identify_new_iw_annuity_eligibilities for one claim')

IF ll_new_rows < 0 THEN
	// this function handles its own transaction
	wf_set_process_run_status('N')
	SignalError(-1,'The generation of a new list of surviving spouse checklists failed unexpectedly. Call the HELPDESK.')
END IF

IF ll_new_rows > 0 THEN
	SQLCA.nf_begin_transaction()

	FOR ll_counter = 1 TO ll_new_rows
		ll_annuity_account_no = lds_identify_new_ss_annuity_eligibilities.GetItemNumber(ll_counter,'annuity_account_no')
		ll_individual_no = lds_identify_new_ss_annuity_eligibilities.GetItemNumber(ll_counter,'individual_no')
		ll_claim_no = lds_identify_new_ss_annuity_eligibilities.GetItemNumber(ll_counter,'claim_no')
				
		li_count = wf_verify_new_list(ll_individual_no,ll_claim_no)
		IF li_count > 0 THEN
			// this function handles its own transaction
			wf_set_process_run_status('N')
			MessageBox('Data Integrity problem','The module was trying to add individual number'+String(ll_individual_no)+' to the new list, but this person is already on a list. Call the HELPDESK.')
			RETURN -1
		END IF
		
		IF ll_annuity_account_no = 0 THEN
			// insert record into ANNUITY_ACCOUNT
			ll_annuity_account_no = inv_common_annuity.nf_insert_annuity_account(ll_individual_no, ll_claim_no, 'SS',0,'N')
		END IF
		
		// get related checklist number, if any exists
		ll_related_checklist_no = inv_common_annuity.nf_get_related_checklist_no(ll_annuity_account_no, 'CAESN')
		
		li_count = wf_get_active_checklist_count(ll_annuity_account_no)
		IF li_count > 0 THEN
			// this function handles its own transaction
			wf_set_process_run_status('N')
			MessageBox('Data Integrity problem','The module was trying to create a new checklist, but there is already an incomplete checklist for this individual '+String(ll_individual_no)+'. Call the HELPDESK.')
			RETURN -1
		END IF
		
		// insert records into CHECKLIST, CHECKLIST_STEP
		ll_new_checklist_no = inv_checklist.nf_create_checklist( '048', 'CAESN', 'IA','A','INA','A',ll_related_checklist_no) // confirm annuity eligibility module = 048
		IF ll_new_checklist_no <= 0 OR IsNull(ll_new_checklist_no) THEN
			// this function handles its own transaction
			wf_set_process_run_status('N')
			MessageBox('Data Integrity Problem','The checklist was not created.')
			RETURN -1
		END IF
		
		ll_checklist_subscriber_no = inv_common_annuity.nf_get_ann_acct_checklist_subscriber_no(ll_annuity_account_no)
		IF ll_checklist_subscriber_no <= 0 OR IsNull(ll_checklist_subscriber_no) THEN
			ll_checklist_subscriber_no = inv_checklist.nf_create_checklist_subscriber(ll_new_checklist_no, 'ANN')
			lb_new_checklist_subscriber = TRUE
		END IF
		
		inv_checklist.nf_insert_subscriber_checklist_xref(ll_checklist_subscriber_no, ll_new_checklist_no)
		
		// perform common tasks after checklist has been created
		ll_annuity_eligibility_no = wf_post_checklist_generation(ll_individual_no,ll_annuity_account_no,ll_claim_no,ll_checklist_subscriber_no,ll_new_checklist_no,'CAESN',lb_new_checklist_subscriber)
		IF ll_annuity_eligibility_no < 0 THEN
			// this function handles its own transaction
			wf_set_process_run_status('N')
			RETURN ll_annuity_eligibility_no
		END IF
		
		lb_new_checklist_subscriber = FALSE
	NEXT
	
	SQLCA.nf_commit_transaction()

ELSE
	// no new eligibilities
	tab_confirm_eligibility.tabpage_new.Enabled = FALSE
END IF


// generate changed surviving spouses
lds_identify_changed_ss_annuity_eligibilities = create u_ds
lds_identify_changed_ss_annuity_eligibilities.DataObject = 'ds_identify_changed_ss_annuity_eligibilities'
lds_identify_changed_ss_annuity_eligibilities.SetTransObject(SQLCA)


// identify potential annuity elig if it doesn't exist
ll_changed_rows = lds_identify_changed_ss_annuity_eligibilities.Retrieve(0)
SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_generate_all_surviving_spouses', 'retrieve p_identify_changed_ss_annuity_eligibilities for one claim')


IF ll_changed_rows < 0 THEN
	// this function handles its own transaction
	wf_set_process_run_status('N')
	SignalError(-1,'The generation of a changed list of surviving spouse checklists failed unexpectedly. Call the HELPDESK.')
END IF

IF ll_changed_rows > 0 THEN
	SQLCA.nf_begin_transaction()

	FOR ll_counter = 1 TO ll_changed_rows
		ll_individual_no = lds_identify_changed_ss_annuity_eligibilities.GetItemNumber(ll_counter,'individual_no')
		ll_claim_no = lds_identify_changed_ss_annuity_eligibilities.GetItemNumber(ll_counter,'claim_no')
		ll_annuity_account_no = lds_identify_changed_ss_annuity_eligibilities.GetItemNumber(ll_counter,'annuity_account_no')
		ls_checklist_type_code = lds_identify_changed_ss_annuity_eligibilities.GetItemString(ll_counter,'checklist_type_code')

		// get related checklist number, if any exists
		ll_related_checklist_no = inv_common_annuity.nf_get_related_checklist_no(ll_annuity_account_no, ls_checklist_type_code)
		
		// insert records into CHECKLIST, CHECKLIST_STEP
		IF ls_checklist_type_code = 'CAESC' THEN
			ll_new_checklist_no = inv_checklist.nf_create_checklist( '048',ls_checklist_type_code, 'IA','A','INA','A', ll_related_checklist_no) // confirm annuity eligibility module = 048
		ELSEIF ls_checklist_type_code = 'PAA' THEN
			li_PAA_upperbound = UpperBound(lstr_payout_individuals)
			
			lstr_payout_individuals[li_PAA_upperbound + 1].payout_individual_no = ll_individual_no
			lstr_payout_individuals[li_PAA_upperbound + 1].payout_claim_no      = ll_claim_no
			
			// skip the remainder of this loop for a PAA individual
			CONTINUE
		END IF
		
		IF ll_new_checklist_no <= 0 OR IsNull(ll_new_checklist_no) THEN
			// this function handles its own transaction
			wf_set_process_run_status('N')
			MessageBox('Data Integrity Problem','The checklist was not created.')
			RETURN -1
		END IF
		
		ll_checklist_subscriber_no = inv_common_annuity.nf_get_ann_acct_checklist_subscriber_no(ll_annuity_account_no)
		IF ll_checklist_subscriber_no <= 0 OR IsNull(ll_checklist_subscriber_no) THEN
			ll_checklist_subscriber_no = inv_checklist.nf_create_checklist_subscriber(ll_new_checklist_no, 'ANN')
			lb_new_checklist_subscriber = TRUE
		END IF
		
		inv_checklist.nf_insert_subscriber_checklist_xref(ll_checklist_subscriber_no, ll_new_checklist_no)
		
		// perform common tasks after checklist has been created
		ll_annuity_eligibility_no = wf_post_checklist_generation(ll_individual_no,ll_annuity_account_no,ll_claim_no,ll_checklist_subscriber_no,ll_new_checklist_no,ls_checklist_type_code,lb_new_checklist_subscriber)
		IF ll_annuity_eligibility_no < 0 THEN
			// this function handles its own transaction
			wf_set_process_run_status('N')
			RETURN ll_annuity_eligibility_no
		END IF
		
		lb_new_checklist_subscriber = FALSE
		
	NEXT
	
	SQLCA.nf_commit_transaction()
	
ELSE
	// not potentially a changed eligibility
	tab_confirm_eligibility.tabpage_changed.Enabled = FALSE
END IF


// this function handles its own transaction
wf_update_annuity_eligibility_run('SA')

li_PAA_upperbound = UpperBound(lstr_payout_individuals)
IF li_PAA_upperbound > 0 THEN

	lds_force_onto_payout = Create U_DS
	lds_force_onto_payout.DataObject = 'ds_force_onto_payout'
	lds_force_onto_payout.SetTransObject(SQLCA)
	
	FOR li_PAA_counter = 1 TO li_PAA_upperbound 
		
		ll_PAA_individual_no = lstr_payout_individuals[li_PAA_counter].payout_individual_no
		ll_PAA_claim_no      = lstr_payout_individuals[li_PAA_counter].payout_claim_no
		
		// run stored proc to insert data
		// the stored procedure that serves as a datasource for lds_force_onto_payout handles its own transaction
		li_PAA_rows = lds_force_onto_payout.Retrieve(ll_PAA_claim_no,ll_PAA_individual_no,'FORCE')
		SQLCA.nf_handle_error('w_confirm_annuity_eligibility','lds_force_onto_payout.retrieve','wf_generate_all_surviving_spouses')
		
		
		IF li_PAA_rows < 1 THEN
			// problem!
			MessageBox('Payout List Error','The retrieval of the payout list for this individual encountered an error. Contact HELPDESK.')
			RETURN li_PAA_rows
		END IF
	NEXT
ELSE
	// if no new, changed or PAA records created
	IF ll_new_rows = 0 AND ll_changed_rows = 0 AND li_PAA_upperbound = 0 THEN
		IF tab_confirm_eligibility.tabpage_new.dw_new_ss_eligibility.RowCount() > 0 THEN
			tab_confirm_eligibility.tabpage_new.Enabled     = TRUE
		END IF
		IF tab_confirm_eligibility.tabpage_changed.dw_changed_ss_eligibility.RowCount() > 0 THEN
			tab_confirm_eligibility.tabpage_changed.Enabled = TRUE
		END IF
	END IF
END IF


ll_rows = ll_new_rows + ll_changed_rows

// this function handles its own transaction
wf_set_process_run_status('N')

RETURN ll_rows
end function

public function integer wf_generate_all_injured_workers ();/*
Generates checklist data for all injured workers
no arguments
returns	= 0 if OK
			= -1 if not OK
*/

BOOLEAN			lb_new_checklist_subscriber, lb_insert_records_for_payout
DATETIME			ldtm_null
INTEGER			li_rtn, li_count, li_PAA_rows, li_PAA_upperbound, li_PAA_counter
LONG				ll_rows, ll_new_rows, ll_changed_rows, ll_individual_no, ll_counter, ll_annuity_account_no, ll_PAA_individual_no
LONG				ll_related_checklist_no, ll_annuity_eligibility_no, ll_checklist_subscriber_no, ll_new_checklist_no
STRING         ls_checklist_type_code
U_DS				lds_identify_new_iw_annuity_eligibilities, lds_identify_changed_iw_annuity_eligibilities, lds_force_onto_payout
s_payout_individuals  lstr_payout_individuals[]


SetNull(ldtm_null)

// this function handles its own transaction
wf_set_process_run_status('Y')


// retrieve new injured worker
lds_identify_new_iw_annuity_eligibilities = create u_ds
lds_identify_new_iw_annuity_eligibilities.DataObject = 'ds_identify_new_iw_annuity_eligibilities'
lds_identify_new_iw_annuity_eligibilities.SetTransObject(SQLCA)


// this function handles its own transaction
wf_insert_annuity_eligibility_run('IA')


// identify potential annuity elig if it doesn't exist
ll_new_rows = lds_identify_new_iw_annuity_eligibilities.Retrieve(0,vgst_user_profile.default_admin_region_code)
SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_generate_all_injured_workers', 'retrieve p_identify_new_iw_annuity_eligibilities for one claim')

IF ll_new_rows < 0 THEN
	// this function handles its own transaction
	wf_set_process_run_status('N')
	SignalError(-1,'The generation of a new list of injured worker checklists failed unexpectedly. Call the HELPDESK.')
END IF

IF ll_new_rows > 0 THEN
	SQLCA.nf_begin_transaction()
	
	FOR ll_counter = 1 TO ll_new_rows
		ll_annuity_account_no = lds_identify_new_iw_annuity_eligibilities.GetItemNumber(ll_counter,'annuity_account_no')
		ll_individual_no = lds_identify_new_iw_annuity_eligibilities.GetItemNumber(ll_counter,'individual_no')
		
		li_count = wf_verify_new_list(ll_individual_no,0)
		IF li_count > 0 THEN
			// this function handles its own transaction
			wf_set_process_run_status('N')
			MessageBox('Data Integrity problem','The module was trying to add individual number'+String(ll_individual_no)+' to the new list, but this person is already on a list. Call the HELPDESK.')
			RETURN -1
		END IF
				
		IF ll_annuity_account_no = 0 THEN
			// insert record into ANNUITY_ACCOUNT
			ll_annuity_account_no = inv_common_annuity.nf_insert_annuity_account(ll_individual_no, 0, 'C',0,'N')
		END IF
		
		// get related checklist number, if any exists
		ll_related_checklist_no = inv_common_annuity.nf_get_related_checklist_no(ll_annuity_account_no, 'CAEIN')
		
		li_count = wf_get_active_checklist_count(ll_annuity_account_no)
		IF li_count > 0 THEN
			// this function handles its own transaction
			wf_set_process_run_status('N')
			MessageBox('Data Integrity problem','The module was trying to create a new checklist, but there is already an incomplete checklist for this individual '+String(ll_individual_no)+'. Call the HELPDESK.')
			RETURN -1
		END IF
		
		// insert records into CHECKLIST, CHECKLIST_STEP
		ll_new_checklist_no = inv_checklist.nf_create_checklist( '048', 'CAEIN', 'IA','A','INA','A', ll_related_checklist_no) // confirm annuity eligibility module = 048
		IF ll_new_checklist_no <= 0 OR IsNull(ll_new_checklist_no) THEN
			// this function handles its own transaction
			wf_set_process_run_status('N')
			MessageBox('Data Integrity Problem','The checklist was not created.')
			RETURN -1
		END IF
		
		ll_checklist_subscriber_no = inv_common_annuity.nf_get_ann_acct_checklist_subscriber_no(ll_annuity_account_no)
		IF ll_checklist_subscriber_no <= 0 OR IsNull(ll_checklist_subscriber_no) THEN
			ll_checklist_subscriber_no = inv_checklist.nf_create_checklist_subscriber(ll_new_checklist_no, 'ANN')
			lb_new_checklist_subscriber = TRUE
		END IF
		
		inv_checklist.nf_insert_subscriber_checklist_xref(ll_checklist_subscriber_no, ll_new_checklist_no)
				
		// perform common tasks after checklist has been created
		ll_annuity_eligibility_no = wf_post_checklist_generation(ll_individual_no,ll_annuity_account_no,0,ll_checklist_subscriber_no,ll_new_checklist_no,'CAEIN',lb_new_checklist_subscriber)
		IF ll_annuity_eligibility_no < 0 THEN
			// this function handles its own transaction
			wf_set_process_run_status('N')
			RETURN ll_annuity_eligibility_no
		END IF
		
		lb_new_checklist_subscriber = FALSE
	NEXT
	
	SQLCA.nf_commit_transaction()
	
ELSE
	// no new eligibilities
	tab_confirm_eligibility.tabpage_new.Enabled = FALSE
END IF


// retrieve changed injured worker
lds_identify_changed_iw_annuity_eligibilities = create u_ds
lds_identify_changed_iw_annuity_eligibilities.DataObject = 'ds_identify_changed_iw_annuity_eligibilities'
lds_identify_changed_iw_annuity_eligibilities.SetTransObject(SQLCA)


// identify potential annuity elig if it doesn't exist
ll_changed_rows = lds_identify_changed_iw_annuity_eligibilities.Retrieve(0,vgst_user_profile.default_admin_region_code)
SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_generate_all_injured_workers', 'retrieve p_identify_changed_iw_annuity_eligibilities for one claim')


IF ll_changed_rows < 0 THEN
	// this function handles its own transaction
	wf_set_process_run_status('N')
	SignalError(-1,'The generation of a changed list of injured worker checklists failed unexpectedly. Call the HELPDESK.')
END IF

IF ll_changed_rows > 0 THEN
	SQLCA.nf_begin_transaction()

	FOR ll_counter = 1 TO ll_changed_rows
		ll_individual_no = lds_identify_changed_iw_annuity_eligibilities.GetItemNumber(ll_counter,'individual_no')
		ll_annuity_account_no = lds_identify_changed_iw_annuity_eligibilities.GetItemNumber(ll_counter,'annuity_account_no')
		ls_checklist_type_code = lds_identify_changed_iw_annuity_eligibilities.GetItemString(ll_counter,'checklist_type_code')
		
		// get related checklist number, if any exists
		ll_related_checklist_no = inv_common_annuity.nf_get_related_checklist_no(ll_annuity_account_no,ls_checklist_type_code)
		
		// insert records into CHECKLIST, CHECKLIST_STEP
		IF ls_checklist_type_code = 'CAEIC' THEN
			ll_new_checklist_no = inv_checklist.nf_create_checklist( '048',ls_checklist_type_code,'IA','A','INA','A', ll_related_checklist_no) // confirm annuity eligibility module = 048
		ELSEIF ls_checklist_type_code = 'PAA' THEN
			li_PAA_upperbound = UpperBound(lstr_payout_individuals)
			
			lstr_payout_individuals[li_PAA_upperbound + 1].payout_individual_no = ll_individual_no
			
			// skip the remainder of this loop for a PAA individual
			CONTINUE
		END IF
		
		IF ll_new_checklist_no <= 0 OR IsNull(ll_new_checklist_no) THEN
			// this function handles its own transaction
			wf_set_process_run_status('N')
			MessageBox('Data Integrity Problem','The checklist was not created.')
			RETURN -1
		END IF
		
		ll_checklist_subscriber_no = inv_common_annuity.nf_get_ann_acct_checklist_subscriber_no(ll_annuity_account_no)
		IF ll_checklist_subscriber_no <= 0 OR IsNull(ll_checklist_subscriber_no) THEN
			ll_checklist_subscriber_no = inv_checklist.nf_create_checklist_subscriber(ll_new_checklist_no, 'ANN')
			lb_new_checklist_subscriber = TRUE
		END IF
		
		inv_checklist.nf_insert_subscriber_checklist_xref(ll_checklist_subscriber_no, ll_new_checklist_no)
		
		// perform common tasks after checklist has been created
		ll_annuity_eligibility_no = wf_post_checklist_generation(ll_individual_no,ll_annuity_account_no,0,ll_checklist_subscriber_no,ll_new_checklist_no,ls_checklist_type_code,lb_new_checklist_subscriber)
		IF ll_annuity_eligibility_no < 0 THEN
			// this function handles its own transaction
			wf_set_process_run_status('N')
			RETURN ll_annuity_eligibility_no
		END IF
		
		lb_new_checklist_subscriber = FALSE		
	NEXT
	
	SQLCA.nf_commit_transaction()
	
ELSE
	// not potentially a changed eligibility
	tab_confirm_eligibility.tabpage_changed.Enabled = FALSE
END IF


// this function handles its own transaction
wf_update_annuity_eligibility_run('IA')


li_PAA_upperbound = UpperBound(lstr_payout_individuals)
IF li_PAA_upperbound > 0 THEN

	lds_force_onto_payout = Create U_DS
	lds_force_onto_payout.DataObject = 'ds_force_onto_payout'
	lds_force_onto_payout.SetTransObject(SQLCA)
	
	FOR li_PAA_counter = 1 TO li_PAA_upperbound 
		
		ll_PAA_individual_no = lstr_payout_individuals[li_PAA_counter].payout_individual_no
		
		// run stored proc to insert data
		// the stored procedure that serves as a datasource for lds_force_onto_payout handles its own transaction
		li_PAA_rows = lds_force_onto_payout.Retrieve(0,ll_PAA_individual_no,'FORCE')
		SQLCA.nf_handle_error('w_confirm_annuity_eligibility','lds_force_onto_payout.retrieve','wf_generate_all_injured_workers')
		
		
		IF li_PAA_rows < 1 THEN
			// problem!
			MessageBox('Payout List Error','The retrieval of the payout list for this individual encountered an error. Contact HELPDESK.')
			RETURN li_PAA_rows
		END IF
	NEXT
ELSE
	// if no new, changed or PAA records created
	IF ll_new_rows = 0 AND ll_changed_rows = 0 AND li_PAA_upperbound = 0 THEN
		IF tab_confirm_eligibility.tabpage_new.dw_new_iw_eligibility.RowCount() > 0 THEN
			tab_confirm_eligibility.tabpage_new.Enabled     = TRUE
		END IF
		IF tab_confirm_eligibility.tabpage_changed.dw_changed_iw_eligibility.RowCount() > 0 THEN
			tab_confirm_eligibility.tabpage_changed.Enabled = TRUE
		END IF
	END IF
END IF

ll_rows = ll_new_rows + ll_changed_rows

// this function handles its own transaction
wf_set_process_run_status('N')

RETURN ll_rows
end function

public subroutine wf_confirm_dw_rfc (string as_checklist_type_code, long al_annuity_account_no, long al_checklist_no, u_datawindow adw_dw, integer ai_row);/*
function is called if the one of the confirm datawindows (dw_new_iw_eligibility,dw_changed_iw_eligibility,dw_new_ss_eligibility,dw_changed_ss_eligibility)
is having the RowFocusChanged event called

arguments:
as_checklist_type_code
al_annuity_account_no
al_checklist_no
adw_dw - the DW that had RFC event
ai_row - current row

*/

INTEGER			li_multiple_annuity_accounts, li_max_completed_step_no, li_rtn
LONG				ll_individual_no
STRING			ls_name, ls_checklist_step_status_code

ll_individual_no = adw_dw.GetItemNumber(ai_row,'individual_no')
ls_name = adw_dw.GetItemString(ai_row,'name') + ' - ' + String(ll_individual_no)

wf_retrieve_annuity_eligilbility(al_annuity_account_no)

IF ib_individual_does_not_exist = FALSE THEN
	// Do not re-retrieve checklists when individual no longer exists
	// the CAE module is just going to re-retrieve all the IWs or SSs anyway
	wf_select_next_available_checklist_step(al_checklist_no,as_checklist_type_code)
	uo_checklist.uf_set_checklist_type_code(as_checklist_type_code)
	uo_checklist.uf_set_module_code('048')
END IF

uo_checklist.uf_set_checklistbar_text(ls_name)
uo_checklist.uf_set_historybar_text(ls_name)
uo_checklist.uf_set_notesbar_text(ls_name)

li_multiple_annuity_accounts = inv_common_annuity.nf_multiple_annuity_accounts(al_annuity_account_no)

IF li_multiple_annuity_accounts > 0 THEN
	st_multiple_annuity_accounts.Visible = TRUE
ELSE
	st_multiple_annuity_accounts.Visible = FALSE
END IF

// enable or disable btn
wf_enable_annuity_inquiry_btn()

end subroutine

public subroutine wf_set_eligibility_datawindow (ref u_datawindow adw_eligibility);INTEGER			li_selected_tabpage

li_selected_tabpage = wf_get_selected_confirm_eligibility_tab ()

IF is_claim_role_code = 'C' THEN
	IF li_selected_tabpage = NEW_LIST THEN
		adw_eligibility = tab_confirm_eligibility.tabpage_new.dw_new_iw_eligibility
	ELSEIF li_selected_tabpage = CHANGED_LIST THEN
		adw_eligibility = tab_confirm_eligibility.tabpage_changed.dw_changed_iw_eligibility
	ELSEIF li_selected_tabpage = PAYOUT_LIST THEN
		adw_eligibility = idw_iw_payout
	END IF
ELSEIF is_claim_role_code = 'SS' THEN
	IF li_selected_tabpage = NEW_LIST THEN
		adw_eligibility = tab_confirm_eligibility.tabpage_new.dw_new_ss_eligibility
	ELSEIF li_selected_tabpage = CHANGED_LIST THEN
		adw_eligibility = tab_confirm_eligibility.tabpage_changed.dw_changed_ss_eligibility
	ELSEIF li_selected_tabpage = PAYOUT_LIST THEN
		adw_eligibility = idw_ss_payout
	END IF
END IF
end subroutine

public function integer wf_set_entitled_data (integer ai_pending_row, integer ai_active_row, integer ai_selected_tab, long al_individual_no, long al_claim_no);DATETIME	   ldtm_annuity_start_date, ldtm_annuity_end_date, ldtm_null, ldtm_benefit_option_date
DATETIME	   ldtm_annuity_eligibility_end_date_used
DECIMAL		ldec_annuity_set_aside_percent
INTEGER		li_rtn, li_opening_no, li_annuity_set_aside_percent_no
LONG			ll_claim_no, ll_annuity_account_no
STRING		ls_benefit_option_code, ls_message, ls_annuity_eligibility_status_code, ls_type, ls_date
STRING		ls_benefit_option_desc, ls_annuity_eligibility_status_desc, ls_comment


ll_annuity_account_no = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.GetItemNumber(ai_pending_row,'annuity_account_no')

// determine annuity start date
// get claim, opening & date needed to determine benefit option
IF is_claim_role_code = 'C' THEN	
	// ls_benefit_option_code = 'N/A'
	
	inv_common_annuity.nf_get_aq_claim(al_individual_no, ll_claim_no, li_opening_no)
	ldtm_annuity_start_date = DateTime(Date(ids_reexamine_benefit_entitlement.GetItemDateTime(1,'benefit_entitlement_25th_month')))
	
	IF li_opening_no > 0 THEN
		SELECT	accident_recurrence_date
		INTO		:ldtm_benefit_option_date
		FROM		OPENING
		WHERE	   claim_no = :ll_claim_no
		AND		opening_no = :li_opening_no
		USING SQLCA;
		SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_set_entitled_data', 'SELECT accident_recurrence_date...')
	ELSE
		ls_benefit_option_code = 'N/A'
	END IF
		
	ls_type = 'injured worker'
	ls_date = 'accident recurrence date'
	
ELSEIF is_claim_role_code = 'SS' THEN
	
   SELECT a.death_date 
	INTO   :ldtm_benefit_option_date
	FROM   INDIVIDUAL a 
	JOIN   CLAIM      b ON a.individual_no = b.individual_no
	WHERE  b.claim_no = :al_claim_no
	USING SQLCA;
   SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_set_entitled_data', 'SELECT death_date...')
	
	ldtm_annuity_start_date = ids_reexamine_benefit_entitlement.GetItemDateTime(1,'benefit_start_date')
	li_opening_no = ids_reexamine_benefit_entitlement.GetItemNumber(1,'opening_no')
	
	ll_claim_no = al_claim_no
	ls_type = 'surviving spouse'
	ls_date = 'injured worker date of death'
END IF


// get benefit option
IF ls_benefit_option_code = 'N/A' THEN
	// already set for no-opening IWs above
ELSE
	SELECT b.benefit_option_code
	INTO   :ls_benefit_option_code
	FROM   OPENING a
	JOIN   Opening_Type_Benefit_Option_Xref b ON a.opening_type_code = b.opening_type_code
	WHERE  a.claim_no = :ll_claim_no
	AND    a.opening_no = :li_opening_no
	AND    b.effective_from_date <= :ldtm_benefit_option_date
	AND    IsNull(b.effective_to_date,GetDate()) >= :ldtm_benefit_option_date
	USING SQLCA;
	SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_set_entitled_data', 'SELECT benefit_option_code...')
END IF

// If nothing retrieved, then trigger error
IF ls_benefit_option_code = '' THEN
	Error.Text = 'No benefit option retrieved for the '+ls_type+' from the Opening_Type_Benefit_Option_Xref table. ' &
					+'Claim: ' + String(ll_claim_no) &
					+'; Individual: ' + String(al_individual_no) &
					+'; Annuity Account: ' + String(ll_annuity_account_no) &
					+'; Opening: ' + String(li_opening_no) &
					+'; ' + ls_date + ': ' + String(ldtm_benefit_option_date,'yyyy-mm-dd')
	Error.WindowMenu="cmwb"
	Error.Object="w_confirm_annuity_eligibility"
	SignalError()
ELSE
	SELECT	benefit_option_desc
	INTO		:ls_benefit_option_desc
	FROM		Benefit_Option
	WHERE	   benefit_option_code = :ls_benefit_option_code
	USING SQLCA;
	SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_set_entitled_data', 'SELECT benefit_option_desc')
END IF


// get annuity end date
SetNull(ldtm_null)
li_rtn = inv_common_annuity.nf_get_annuity_end_date(al_individual_no,ls_type,ls_message,ldtm_annuity_end_date,ldtm_annuity_eligibility_end_date_used,ldtm_null,ldtm_null)
IF li_rtn < 0 THEN
	MessageBox('Annuity Problem',ls_message,Exclamation!)
	RETURN li_rtn
END IF

	
IF ldtm_annuity_start_date > ldtm_annuity_end_date THEN
	// the only reason that this happens is that the IW or SS turned 65 prior to start date
	li_rtn = wf_set_not_entitled_data(ai_pending_row, ai_selected_tab, 'The '+ls_type+' turned 65 or was deceased prior to the qualification start date.')
	RETURN 0
END IF

ls_annuity_eligibility_status_code = 'A'

tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.SetItem(ai_pending_row,'annuity_start_date',ldtm_annuity_start_date)
tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.SetItem(ai_pending_row,'annuity_eligibility_end_date_used',ldtm_annuity_eligibility_end_date_used)
tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.SetItem(ai_pending_row,'annuity_end_date',ldtm_annuity_end_date)
tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.SetItem(ai_pending_row,'benefit_option_code',ls_benefit_option_code)

// confirmed_date is null for now
tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.SetItem(ai_pending_row,'annuity_eligibility_status_code',ls_annuity_eligibility_status_code)

// get the annuity pct for the role code, annuity end date, & benefit option
inv_common_annuity.nf_get_annuity_percentage(is_claim_role_code, ldtm_annuity_end_date, ls_benefit_option_code, ldec_annuity_set_aside_percent , li_annuity_set_aside_percent_no)

// If nothing retrieved, then trigger error
IF ldec_annuity_set_aside_percent = 0.00 OR li_annuity_set_aside_percent_no = 0 THEN
	Error.Text = 'No percentage retrieved for the '+ls_type+' from the Annuity_Set_Aside_Percent table.' &
					+'Claim: ' + String(ll_claim_no) &
					+'; Individual: ' + String(al_individual_no) &
					+'; Annuity Account: ' + String(ll_annuity_account_no) &
					+'; Benefit Option: ' + ls_benefit_option_desc
	Error.WindowMenu="cmwb"
	Error.Object="w_confirm_annuity_eligibility"
	SignalError()
END IF

// set percentage values
tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.SetItem(ai_pending_row,'annuity_set_aside_percent',ldec_annuity_set_aside_percent)
tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.SetItem(ai_pending_row,'annuity_set_aside_percent_no',li_annuity_set_aside_percent_no)

SELECT	annuity_eligibility_status_desc
INTO		:ls_annuity_eligibility_status_desc
FROM		Annuity_Eligibility_Status
WHERE	annuity_eligibility_status_code = :ls_annuity_eligibility_status_code
USING SQLCA;
SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_set_entitled_data', 'SELECT annuity_eligibility_status_desc')


// there will only be a value for li_active_row if this is a changed eligibility
IF ai_active_row > 0 THEN
	// changed eligibility
	tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.SetItem(ai_active_row,'annuity_eligibility_status_code','I')
END IF


// Open eligibility comment window
is_eligibility_comment_message = 'Based on the Benefit Entitlement that has been verified, Annuity Eligibility'&
					+'~nhas been established as eligible from '+String(ldtm_annuity_start_date,'yyyy-mm-dd')+' to '+String(ldtm_annuity_end_date,'yyyy-mm-dd') +'.'&
					+'~nAlso,'&
					+'~n                ·    the benefit option was ' +ls_benefit_option_desc &
					+'~n                ·    the annuity eligibility status was ' +ls_annuity_eligibility_status_desc+ '.'&
					+'~nPlease enter a Comment, if you wish and press OK to save the comment.'

// set up the values needed to save the users comments, after the commit of the steps
ib_set_eligibility_comment = TRUE
il_annuity_eligibility_for_comment = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.GetItemNumber(ai_pending_row,'annuity_eligibility_no')

RETURN 0

end function

public function integer wf_populate_checklist_and_annuity (long al_individual_no, long al_claim_no, long al_annuity_account_no, string as_checklist_type_code);BOOLEAN		lb_new_checklist_subscriber
LONG			ll_checklist_subscriber_no, ll_related_checklist_no, ll_new_checklist_no, ll_annuity_eligibility_no

IF al_annuity_account_no = 0 THEN
	// insert record into ANNUITY_ACCOUNT
	al_annuity_account_no = inv_common_annuity.nf_insert_annuity_account(al_individual_no, al_claim_no, is_claim_role_code,0,'N')
END IF

// get related checklist number, if any exists
ll_related_checklist_no = inv_common_annuity.nf_get_related_checklist_no(al_annuity_account_no, as_checklist_type_code)

// insert records into CHECKLIST, CHECKLIST_STEP
ll_new_checklist_no = inv_checklist.nf_create_checklist('048',as_checklist_type_code,'IA','A','INA','A', ll_related_checklist_no) // confirm annuity eligibility module = 048
IF ll_new_checklist_no <= 0 OR IsNull(ll_new_checklist_no) THEN
	MessageBox('Data Integrity Problem','The checklist was not created.')
	RETURN -1
END IF

ll_checklist_subscriber_no = inv_common_annuity.nf_get_ann_acct_checklist_subscriber_no(al_annuity_account_no)
IF ll_checklist_subscriber_no <= 0 OR IsNull(ll_checklist_subscriber_no) THEN
	ll_checklist_subscriber_no = inv_checklist.nf_create_checklist_subscriber(ll_new_checklist_no, 'ANN')
	lb_new_checklist_subscriber = TRUE
END IF
IF ll_checklist_subscriber_no <= 0 OR IsNull(ll_checklist_subscriber_no) THEN
	MessageBox('Data Integrity Problem','The checklist subscriber was not created. Call HELPDESK.')
	RETURN -1
END IF

inv_checklist.nf_insert_subscriber_checklist_xref(ll_checklist_subscriber_no,ll_new_checklist_no)

// perform common tasks after checklist has been created
ll_annuity_eligibility_no = wf_post_checklist_generation(al_individual_no,al_annuity_account_no,al_claim_no,ll_checklist_subscriber_no,ll_new_checklist_no,as_checklist_type_code,lb_new_checklist_subscriber)
IF ll_annuity_eligibility_no < 0 THEN RETURN ll_annuity_eligibility_no

IF il_annuity_payout_no > 0 THEN
	//update ANNUITY_PAYOUT with new CAEP checklist_no, new annuity_eligibility_no
	ids_annuity_payout.SetItem(1,'confirm_annuity_eligibility_checklist_no',ll_new_checklist_no)
	ids_annuity_payout.SetItem(1,'annuity_eligibility_no',ll_annuity_eligibility_no)
	ids_annuity_payout.Update()
END IF

RETURN 0
end function

public function long wf_get_claim_no ();INTEGER				li_row
LONG					ll_claim_no
U_DATAWINDOW	ldw_eligibility
	
wf_set_eligibility_datawindow(ldw_eligibility)

IF IsValid(ldw_eligibility) THEN
	li_row = ldw_eligibility.GetRow()
	IF li_row > 0 THEN
		ll_claim_no = ldw_eligibility.GetItemNumber(li_row,'claim_no')
	ELSE
		MessageBox('No Records','There are no records. Please retrieve and select a record.',Exclamation!)
		RETURN -1
	END IF
ELSE
	MessageBox('No Records','There are no records. Please retrieve and select a record.',Exclamation!)
	RETURN -1
END IF

RETURN ll_claim_no
end function

public function long wf_get_individual_no ();INTEGER				li_row
LONG					ll_individual_no
U_DATAWINDOW	ldw_eligibility
	
wf_set_eligibility_datawindow(ldw_eligibility)
li_row = ldw_eligibility.GetRow()

IF li_row > 0 THEN
	ll_individual_no = ldw_eligibility.GetItemNumber(li_row,'individual_no')
END IF

RETURN ll_individual_no
end function

public function string wf_get_claim_role_code ();RETURN is_claim_role_code
end function

public function string wf_get_module_code ();RETURN '048'
end function

public function w_confirm_annuity_eligibility wf_get_window_reference ();RETURN THIS
end function

public function integer wf_verify_new_list (long al_individual_no, long al_claim_no);/*
Function checks for situations where someone has been identified as needing to be added to 'new' list, but shouldn't be added

arguments
al_individual_no
al_claim_no
*/

INTEGER		li_count

SELECT	Count(*)
INTO		:li_count
FROM		ANNUITY_ACCOUNT	a
JOIN		ANNUITY_ELIGIBILITY	b ON a.annuity_account_no = b.annuity_account_no
WHERE	a.individual_no = :al_individual_no
AND		a.claim_no = :al_claim_no
AND		b.annuity_eligibility_status_code in ('P')
USING SQLCA;
SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_get_active_annuity_account_count', 'SELECT count(*) FROM ANNUITY_ACCOUNT (1) ...')


RETURN li_count
end function

public function long wf_generate_injured_worker ();/*
Generates checklist data for a single injured worker
argument - al_claim_no
returns	= 0 if OK
			= -1 if not OK
*/

INTEGER				li_rtn, li_count, li_msg, li_PAA_rows, li_trancount
LONG					ll_rows, ll_new_rows, ll_changed_rows, ll_annuity_account_no, ll_active_annuity_eligibility_no
STRING            ls_checklist_type_code, ls_exclude_reason, ls_exclude_message
U_DS					lds_identify_new_iw_annuity_eligibilities, lds_identify_changed_iw_annuity_eligibilities, lds_force_onto_payout


ll_active_annuity_eligibility_no = wf_active_annuity_eligibility_count(0,il_individual_no)

// get annuity account
SELECT	annuity_account_no
INTO		:ll_annuity_account_no
FROM		ANNUITY_ACCOUNT
WHERE	individual_no = :il_individual_no
AND		claim_role_code = 'C'
USING SQLCA;
SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_generate_injured_worker', 'SELECT annuity_account_no FROM ANNUITY_ACCOUNT')

// retrieve new injured worker
lds_identify_new_iw_annuity_eligibilities = create u_ds
lds_identify_new_iw_annuity_eligibilities.DataObject = 'ds_identify_new_iw_annuity_eligibilities'
lds_identify_new_iw_annuity_eligibilities.SetTransObject(SQLCA)

// identify potential annuity elig if it doesn't exist
ll_new_rows = lds_identify_new_iw_annuity_eligibilities.Retrieve(il_claim_no,'')
SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_generate_injured_worker', 'retrieve p_identify_new_iw_annuity_eligibilities for one claim')

IF ll_new_rows = 1 THEN
	li_count = wf_verify_new_list(il_individual_no,0)
	IF li_count > 0 THEN
		MessageBox('Data Integrity problem','The module was trying to add individual number'+String(il_individual_no)+' to the new list, but this person is already on a list. Call the HELPDESK.')
		RETURN -1
	END IF

	li_count = wf_get_active_checklist_count(ll_annuity_account_no)
	IF li_count > 0 THEN
		MessageBox('Data Integrity problem','The module was trying to create a new checklist, but there is already an incomplete checklist for this individual '+String(il_individual_no)+'. Call the HELPDESK.')
		RETURN -1
	END IF
	
	SQLCA.nf_begin_transaction()
							
	li_rtn = wf_populate_checklist_and_annuity(il_individual_no,0,ll_annuity_account_no,'CAEIN')	
	IF li_rtn = -1 THEN
		SQLCA.nf_transaction_count(li_trancount,0,this.classname(),'','',FALSE)
		IF li_trancount > 0 THEN
			SQLCA.nf_rollback_transaction()
		END IF
		RETURN -1
	END IF

	SQLCA.nf_commit_transaction()
	
	// successfully inserted into list
	RETURN ll_new_rows

ELSEIF ll_new_rows = 0 THEN
	// does not qualify for 'new' list
	IF ll_active_annuity_eligibility_no = 0 THEN
		li_msg = MessageBox('Add Claimant','This claimant (individual no: '+String(il_individual_no)+') did not meet the criteria for the "new" potential annuity eligibility list.'&
													+'~r~n~r~nDo you want to add the claimant to the list anyway?',Question!,YesNo!,2)
		IF li_msg = 1 THEN
			SQLCA.nf_begin_transaction()
			
			li_rtn = wf_populate_checklist_and_annuity(il_individual_no,0,ll_annuity_account_no,'CAEIN')
			IF li_rtn = -1 THEN
				SQLCA.nf_transaction_count(li_trancount,0,this.classname(),'','',FALSE)
				IF li_trancount > 0 THEN
					SQLCA.nf_rollback_transaction()
				END IF
				RETURN -1
			END IF
			
			SQLCA.nf_commit_transaction()
		ELSE
			wf_clear_datawindows()
			tab_confirm_eligibility.tabpage_new.Enabled = FALSE
			RETURN -99
		END IF
				
		// successfully inserted into list
		RETURN 1
	END IF
ELSEIF ll_new_rows < 0 THEN
	SignalError(-1,'The generation of the new claimant checklist failed unexpectedly. Call the HELPDESK.')
END IF


// retrieve changed injured worker
lds_identify_changed_iw_annuity_eligibilities = create u_ds
lds_identify_changed_iw_annuity_eligibilities.DataObject = 'ds_identify_changed_iw_annuity_eligibilities'
lds_identify_changed_iw_annuity_eligibilities.SetTransObject(SQLCA)


// identify potential annuity elig if it doesn't exist
ll_changed_rows = lds_identify_changed_iw_annuity_eligibilities.Retrieve(il_claim_no,'')
SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_generate_injured_worker', 'retrieve p_identify_changed_iw_annuity_eligibilities for one claim')


IF ll_changed_rows = 1 THEN
	ls_checklist_type_code = lds_identify_changed_iw_annuity_eligibilities.GetItemString(1,'checklist_type_code')
	
	IF ls_checklist_type_code <> 'PAA' THEN
		SQLCA.nf_begin_transaction()
		
		li_rtn = wf_populate_checklist_and_annuity(il_individual_no,0,ll_annuity_account_no,ls_checklist_type_code)		
		IF li_rtn = -1 THEN
			SQLCA.nf_transaction_count(li_trancount,0,this.classname(),'','',FALSE)
			IF li_trancount > 0 THEN
				SQLCA.nf_rollback_transaction()
			END IF
			RETURN -1
		END IF

		SQLCA.nf_commit_transaction()
	ELSE
		lds_force_onto_payout = Create U_DS
		lds_force_onto_payout.DataObject = 'ds_force_onto_payout'
		lds_force_onto_payout.SetTransObject(SQLCA)
		
		// the stored procedure that is the data source for lds_force_onto_payout handles its own transaction		
		li_PAA_rows = lds_force_onto_payout.Retrieve(0,il_individual_no,'FORCE')
		SQLCA.nf_handle_error('w_confirm_annuity_eligibility','lds_force_onto_payout.retrieve','wf_generate_injured_worker')
		
		IF li_PAA_rows < 1 THEN
			// problem!
			MessageBox('Payout List Error','The retrieval of the payout list for this individual encountered an error. Contact HELPDESK.')
			RETURN li_PAA_rows
		ELSE
			ib_PAA_individual = TRUE
			
			// determine exclusion
			ls_exclude_reason = lds_force_onto_payout.GetItemString(1,'exclude_reason')
			
			CHOOSE CASE ls_exclude_reason
				CASE ''
					// not excluded, so forced onto annuity payout list
					MessageBox('Eligible for Annuity Payout List','This injured worker has been added to the Annuity Payout list. ' &
					                                             +'You must open the Prepare Annuity Payout module to access the injured worker.')
				CASE 'paid out'
					ls_exclude_message = 'The injured worker has a post-1992 Annuity Payout created before the Annuity Payout module became effective.'
				CASE 'no subledger'
					ls_exclude_message = 'The injured worker has no post-1992 sub-ledger transactions.'
				CASE 'zero amount subledger'
					ls_exclude_message = 'The injured worker has a post-1992 sub-ledger amount of $0.'
				CASE 'no annuity account'
					ls_exclude_message = 'The injured worker does not have an annuity account.'
				CASE 'no active eligibility'
					ls_exclude_message = 'The injured worker is not eligible for annuity benefits.'
				CASE 'future end date'
					ls_exclude_message = 'The annuity end date for this injured worker is in the future.'
			END CHOOSE
			
			IF ls_exclude_reason <> '' THEN
				MessageBox('Not Eligible for Payout List','This injured worker cannot be forced onto the Payout Annuity Account list for the following reason:~r~n' + ls_exclude_message)
			END IF
			
			RETURN li_PAA_rows
		END IF
	END IF

ELSEIF ll_changed_rows = 0 THEN
	// does not qualify for 'changed' list
	IF ll_active_annuity_eligibility_no > 0 THEN
		li_msg = MessageBox('Add Claimant','This claimant (individual no: '+String(il_individual_no)+') did not meet the criteria for the "changed" potential annuity eligibility list.'&
													+'~r~n~r~nDo you want to add the claimant to the list anyway?',Question!,YesNo!,2)
		IF li_msg = 1 THEN
			SQLCA.nf_begin_transaction()
			
			li_rtn = wf_populate_checklist_and_annuity(il_individual_no,0,ll_annuity_account_no,'CAEIC')
			IF li_rtn = -1 THEN
				SQLCA.nf_transaction_count(li_trancount,0,this.classname(),'','',FALSE)
				IF li_trancount > 0 THEN
					SQLCA.nf_rollback_transaction()
				END IF
				RETURN -1
			END IF
			
			SQLCA.nf_commit_transaction()
		ELSE
			wf_clear_datawindows()
			tab_confirm_eligibility.tabpage_new.Enabled = FALSE
			RETURN -99
		END IF		
	END IF
ELSEIF ll_changed_rows < 0 THEN
	SignalError(-1,'The generation of the changed claimant checklist failed unexpectedly. Call the HELPDESK.')
END IF

ll_rows = ll_new_rows + ll_changed_rows

RETURN ll_rows
end function

public function long wf_active_annuity_eligibility_count (long al_claim_no, long al_individual_no);LONG		ll_annuity_eligibility_no


// new (no active AE) or changed (has active AE)
SELECT	IsNull(a.annuity_eligibility_no,0)
INTO		:ll_annuity_eligibility_no
FROM		ANNUITY_ELIGIBILITY a
JOIN		ANNUITY_ACCOUNT b ON a.annuity_account_no = b.annuity_account_no
WHERE	b.claim_no = :al_claim_no
AND		b.individual_no = :al_individual_no
AND		a.annuity_eligibility_status_code = 'A'
USING SQLCA;
SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_active_annuity_eligibility_count', 'SELECT IsNull(annuity_eligibility_no,0) FROM ANNUITY_ELIGIBILITY...')

RETURN ll_annuity_eligibility_no
end function

public function long wf_generate_surviving_spouse ();/*
Generates checklist data for a single surviving spouse
argument - il_claim_no
returns	= row count (1) if SS qualifies as potential, 0 if not
*/

INTEGER				li_rtn, li_count, li_msg, li_PAA_rows, li_trancount
LONG					ll_rows, ll_new_rows, ll_changed_rows, ll_annuity_account_no, ll_active_annuity_eligibility_no
STRING            ls_checklist_type_code, ls_exclude_reason, ls_exclude_message
U_DS					lds_identify_new_ss_annuity_eligibilities, lds_identify_changed_ss_annuity_eligibilities, lds_force_onto_payout


ll_active_annuity_eligibility_no = wf_active_annuity_eligibility_count(il_claim_no,il_individual_no)

// get annuity account
SELECT	annuity_account_no
INTO		:ll_annuity_account_no
FROM		ANNUITY_ACCOUNT
WHERE	individual_no = :il_individual_no
AND		claim_no = :il_claim_no
AND		claim_role_code = 'SS'
USING SQLCA;
SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_generate_surviving_spouse', 'SELECT annuity_account_no FROM ANNUITY_ACCOUNT')


// generate new surviving spouse
lds_identify_new_ss_annuity_eligibilities = create u_ds
lds_identify_new_ss_annuity_eligibilities.DataObject = 'ds_identify_new_ss_annuity_eligibilities'
lds_identify_new_ss_annuity_eligibilities.SetTransObject(SQLCA)


// identify potential annuity elig if it doesn't exist
ll_new_rows = lds_identify_new_ss_annuity_eligibilities.Retrieve(il_claim_no)
SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_generate_surviving_spouse', 'retrieve p_identify_new_ss_annuity_eligibilities for one claim')


IF ll_new_rows = 1 THEN
	
	li_count = wf_verify_new_list(il_individual_no,il_claim_no)
	IF li_count > 0 THEN
		MessageBox('Data Integrity problem','The module was trying to add individual number'+String(il_individual_no)+' to the new list, but this person is already on a list. Call the HELPDESK.')
		RETURN -1
	END IF

	li_count = wf_get_active_checklist_count(ll_annuity_account_no)
	IF li_count > 0 THEN
		MessageBox('Data Integrity problem','The module was trying to create a new checklist, but there is already an incomplete checklist for this individual '+String(il_individual_no)+'. Call the HELPDESK.')
		RETURN -1
	END IF
	
	SQLCA.nf_begin_transaction()
	
	li_rtn = wf_populate_checklist_and_annuity(il_individual_no,il_claim_no,ll_annuity_account_no,'CAESN')
	IF li_rtn = -1 THEN
		SQLCA.nf_transaction_count(li_trancount,0,this.classname(),'','',FALSE)
		IF li_trancount > 0 THEN
			SQLCA.nf_rollback_transaction()
		END IF
		RETURN -1
	END IF

	SQLCA.nf_commit_transaction()
					
ELSEIF ll_new_rows = 0 THEN
	// does not qualify for 'new' list
	IF ll_active_annuity_eligibility_no = 0 THEN
		li_msg = MessageBox('Add Surviving Spouse','This surviving spouse (claim no: '+String(il_claim_no)+', individual no: '+String(il_individual_no)+') did not meet the criteria for the "new" potential annuity eligibility list.'&
													+'~r~n~r~nDo you want to add the surviving spouse to the list anyway?',Question!,YesNo!,2)
		IF li_msg = 1 THEN
			SQLCA.nf_begin_transaction()
			
			li_rtn = wf_populate_checklist_and_annuity(il_individual_no,il_claim_no,ll_annuity_account_no,'CAESN')
			IF li_rtn = -1 THEN
				SQLCA.nf_transaction_count(li_trancount,0,this.classname(),'','',FALSE)
				IF li_trancount > 0 THEN
					SQLCA.nf_rollback_transaction()
				END IF
				RETURN -1
			END IF
			
			SQLCA.nf_commit_transaction()
		ELSE
			wf_clear_datawindows()
			tab_confirm_eligibility.tabpage_new.Enabled = FALSE
			RETURN -99
		END IF
		
	END IF
ELSEIF ll_new_rows < 0 THEN
	SignalError(-1,'The generation of the new surviving spouse checklist failed unexpectedly. Call the HELPDESK.')
END IF


// generate changed surviving spouse

lds_identify_changed_ss_annuity_eligibilities = create u_ds
lds_identify_changed_ss_annuity_eligibilities.DataObject = 'ds_identify_changed_ss_annuity_eligibilities'
lds_identify_changed_ss_annuity_eligibilities.SetTransObject(SQLCA)


// identify potential annuity elig if it doesn't exist
ll_changed_rows = lds_identify_changed_ss_annuity_eligibilities.Retrieve(il_claim_no)
SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_generate_surviving_spouse', 'retrieve p_identify_changed_ss_annuity_eligibilities for one claim')


IF ll_changed_rows = 1 THEN
	ls_checklist_type_code = lds_identify_changed_ss_annuity_eligibilities.GetItemString(1,'checklist_type_code')
	
	IF ls_checklist_type_code <> 'PAA' THEN
		SQLCA.nf_begin_transaction()
		
		li_rtn = wf_populate_checklist_and_annuity(il_individual_no,il_claim_no,ll_annuity_account_no,ls_checklist_type_code)
		IF li_rtn = -1 THEN
			SQLCA.nf_transaction_count(li_trancount,0,this.classname(),'','',FALSE)
			IF li_trancount > 0 THEN
				SQLCA.nf_rollback_transaction()
			END IF
			RETURN -1
		END IF

		SQLCA.nf_commit_transaction()
	ELSE
		lds_force_onto_payout = Create U_DS
		lds_force_onto_payout.DataObject = 'ds_force_onto_payout'
		lds_force_onto_payout.SetTransObject(SQLCA)
		
		// run stored proc to insert data
		// stored procedure that serves as data source for lds_force_onto_payout handles its own transaction
		li_PAA_rows = lds_force_onto_payout.Retrieve(il_claim_no,il_individual_no,'FORCE')
		SQLCA.nf_handle_error('w_confirm_annuity_eligibility','lds_force_onto_payout.retrieve','wf_generate_surviving_spouse')
		
		
		IF li_PAA_rows < 1 THEN
			// problem!
			MessageBox('Payout List Error','The retrieval of the payout list for this individual encountered an error. Contact HELPDESK.')
			RETURN li_PAA_rows
		ELSE
			ib_PAA_individual = TRUE
			
			// determine exclusion
			ls_exclude_reason = lds_force_onto_payout.GetItemString(1,'exclude_reason')
			
			CHOOSE CASE ls_exclude_reason
				CASE ''
					// not excluded, so forced onto annuity payout list
					MessageBox('Eligible for Annuity Payout List','This surviving spouse has been added to the Annuity Payout list. ' &
					                                             +'You must open the Prepare Annuity Payout module to access the surviving spouse.')
				CASE 'paid out'
					ls_exclude_message = 'The surviving spouse has an Annuity Payout created before the Annuity Payout module became effective.'
				CASE 'no subledger'
					ls_exclude_message = 'The surviving spouse has no sub-ledger transactions.'
				CASE 'zero amount subledger'
					ls_exclude_message = 'The surviving spouse has a sub-ledger amount of $0.'
				CASE 'no annuity account'
					ls_exclude_message = 'The surviving spouse does not have an annuity account.'
				CASE 'no active eligibility'
					ls_exclude_message = 'The surviving spouse is not eligible for annuity benefits.'
				CASE 'future end date'
					ls_exclude_message = 'The annuity end date for this surviving spouse is in the future.'
			END CHOOSE
			
			IF ls_exclude_reason <> '' THEN
				MessageBox('Not Eligible for Payout List','This surviving spouse cannot be forced onto the Payout Annuity Account list for the following reason:~r~n' + ls_exclude_message)
			END IF
			
			RETURN li_PAA_rows
		END IF
	END IF

ELSEIF ll_changed_rows = 0 THEN
	// does not qualify for 'changed' list
	IF ll_active_annuity_eligibility_no > 0 THEN
		li_msg = MessageBox('Add Surviving Spouse','This surviving spouse (claim no: '+String(il_claim_no)+', individual no: '+String(il_individual_no)+') did not meet the criteria for the "changed" potential annuity eligibility list.'&
													+'~r~n~r~nDo you want to add the surviving spouse to the list anyway?',Question!,YesNo!,2)
		IF li_msg = 1 THEN
			SQLCA.nf_begin_transaction()
			
			li_rtn = wf_populate_checklist_and_annuity(il_individual_no,il_claim_no,ll_annuity_account_no,'CAESC')
			IF li_rtn = -1 THEN
				SQLCA.nf_transaction_count(li_trancount,0,this.classname(),'','',FALSE)
				IF li_trancount > 0 THEN
					SQLCA.nf_rollback_transaction()
				END IF
				RETURN -1
			END IF
			
			SQLCA.nf_commit_transaction()
		ELSE
			wf_clear_datawindows()
			tab_confirm_eligibility.tabpage_new.Enabled = FALSE
			RETURN -99
		END IF
		
	END IF
ELSEIF ll_changed_rows < 0 THEN
	SignalError(-1,'The generation of the changed surviving spouse checklist failed unexpectedly. Call the HELPDESK.')
END IF

ll_rows = ll_new_rows + ll_changed_rows

RETURN ll_rows
end function

public function long wf_retrieve_surviving_spouse (long al_claim_no, long al_individual_no);INTEGER		li_rows, li_new_rows, li_changed_rows, li_payout_rows


IF is_list_mode = 'menu' THEN
	// retrieve new eligibilities
	tab_confirm_eligibility.tabpage_new.dw_new_ss_eligibility.DataObject = 'd_ss_eligibility'
	tab_confirm_eligibility.tabpage_new.dw_new_ss_eligibility.SetTransObject(SQLCA)
	li_new_rows = tab_confirm_eligibility.tabpage_new.dw_new_ss_eligibility.Retrieve(al_claim_no,al_individual_no,'CAESN')
	SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_retrieve_surviving_spouse', 'new dw retrieve')
	
	// retrieve changed eligibility
	tab_confirm_eligibility.tabpage_changed.dw_changed_ss_eligibility.DataObject = 'd_ss_eligibility'
	tab_confirm_eligibility.tabpage_changed.dw_changed_ss_eligibility.SetTransObject(SQLCA)
	li_changed_rows = tab_confirm_eligibility.tabpage_changed.dw_changed_ss_eligibility.Retrieve(al_claim_no,al_individual_no,'CAESC')
	SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_retrieve_surviving_spouse', 'changed dw retrieve')
ELSEIF is_list_mode = 'payout' THEN
	// retrieve payout eligibility
	idw_ss_payout.DataObject = 'd_ss_eligibility'
	idw_ss_payout.SetTransObject(SQLCA)
	li_payout_rows = idw_ss_payout.Retrieve(al_claim_no,al_individual_no,'CAEP')
	idw_ss_payout.SetColumn('individual_no')
	SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_retrieve_injured_worker', 'payout dw retrieve')	
END IF

IF li_payout_rows = 0 THEN
	wf_select_tab(li_new_rows,li_changed_rows)
ELSE
	tab_confirm_eligibility.tabpage_changed.Enabled = TRUE
	tab_confirm_eligibility.post SelectTab(3)
END IF

li_rows = li_new_rows + li_changed_rows + li_payout_rows

RETURN li_rows
end function

public function long wf_retrieve_injured_worker (long al_individual_no);INTEGER			li_rows, li_new_rows, li_changed_rows, li_payout_rows

IF is_list_mode = 'menu' THEN
	// retrieve new eligibilities
	tab_confirm_eligibility.tabpage_new.dw_new_iw_eligibility.DataObject = 'd_iw_eligibility'
	tab_confirm_eligibility.tabpage_new.dw_new_iw_eligibility.SetTransObject(SQLCA)
	li_new_rows = tab_confirm_eligibility.tabpage_new.dw_new_iw_eligibility.Retrieve(al_individual_no,'CAEIN')
	tab_confirm_eligibility.tabpage_new.dw_new_iw_eligibility.SetColumn('individual_no')
	SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_retrieve_injured_worker', 'new dw retrieve')
	
	// retrieve changed eligibility
	tab_confirm_eligibility.tabpage_changed.dw_changed_iw_eligibility.DataObject = 'd_iw_eligibility'
	tab_confirm_eligibility.tabpage_changed.dw_changed_iw_eligibility.SetTransObject(SQLCA)
	li_changed_rows = tab_confirm_eligibility.tabpage_changed.dw_changed_iw_eligibility.Retrieve(al_individual_no,'CAEIC')
	tab_confirm_eligibility.tabpage_changed.dw_changed_iw_eligibility.SetColumn('individual_no')
	SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_retrieve_injured_worker', 'changed dw retrieve')
	
ELSEIF is_list_mode = 'payout' THEN
	// retrieve payout eligibility
	idw_iw_payout.DataObject = 'd_iw_eligibility'
	idw_iw_payout.SetTransObject(SQLCA)
	li_payout_rows = idw_iw_payout.Retrieve(al_individual_no,'CAEP')
	idw_iw_payout.SetColumn('individual_no')
	SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_retrieve_injured_worker', 'payout dw retrieve')
END IF

IF li_payout_rows = 0 THEN
	wf_select_tab(li_new_rows,li_changed_rows)
ELSE
	tab_confirm_eligibility.tabpage_changed.Enabled = TRUE
	tab_confirm_eligibility.post SelectTab(3)
END IF

li_rows = li_new_rows + li_changed_rows + li_payout_rows

RETURN li_rows
end function

public function integer wf_set_not_entitled_data (integer ai_pending_row, integer ai_selected_tab, string as_extra_message);STRING						ls_benefit_option_code, ls_annuity_eligibility_status_code
STRING						ls_message, ls_comment

// Open eligibility comment window
IF ai_selected_tab = 1 OR is_claim_role_code = 'SS' THEN
	is_eligibility_comment_message = 'Based on the Benefit Entitlement that has been verified, Annuity Eligibility has been established as not eligible.' &
						+'~r~n' + as_extra_message &
						+'~r~nPlease enter a Comment, if you wish and press OK to save the comment.'
ELSE
	is_eligibility_comment_message = 'Based on the Benefit Entitlement that has been verified, Annuity Eligibility has been established as not eligible.' &
						+'~r~n' + as_extra_message &
						+'~r~nAll set aside amounts should be reversed.'&
						+'~r~nPlease enter a Comment, if you wish and press OK to save the comment.'
END IF

// set up the values needed to save the users comments, after the commit of the steps
ib_set_eligibility_comment = TRUE
il_annuity_eligibility_for_comment = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.GetItemNumber(ai_pending_row,'annuity_eligibility_no')

ls_benefit_option_code = 'N/A'
ls_annuity_eligibility_status_code = 'I'

tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.SetItem(ai_pending_row,'benefit_option_code',ls_benefit_option_code)
tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.SetItem(ai_pending_row,'annuity_eligibility_status_code',ls_annuity_eligibility_status_code)

RETURN 0


end function

public subroutine wf_post_resize_checklist_position ();LONG		ll_x , ll_width

IF ib_checklist_posted THEN
	
	ll_x = THIS.x
	ll_width = THIS.width
	
	IF uo_checklist.ib_maximized THEN
		// minimize the checklist before resize
		uo_checklist.p_right_arrow.triggerevent(clicked!)
	END IF

	uo_checklist.il_maximized_x = (ll_width - 160) - 2076
	uo_checklist.il_minimized_x = ll_width - 160
		
	uo_checklist.x = ll_width - 160
	
	ib_checklist_posted = FALSE
END IF

end subroutine

public subroutine wf_update_annuity_eligibility_run (string as_annuity_eligibility_run_option_code);SQLCA.nf_begin_transaction()

UPDATE	ANNUITY_ELIGIBILITY_RUN
SET		annuity_eligibility_run_end_date = GetDate()
WHERE	annuity_eligibility_run_option_code = :as_annuity_eligibility_run_option_code
AND		annuity_eligibility_run_end_date IS NULL
AND		abnormal_termination_flag = 'N'
USING SQLCA;
SQLCA.nf_handle_error( 'w_confirm_annuity_eligibility', 'UPDATE ANNUITY_ELIGIBILITY_RUN', 'wf_update_annuity_eligibility_run')

SQLCA.nf_commit_transaction()

end subroutine

public function integer wf_get_ae_run_option_count (string as_annuity_eligibility_run_option_code);INT	li_count

SELECT	Count(*)
INTO		:li_count
FROM		ANNUITY_ELIGIBILITY_RUN
WHERE	annuity_eligibility_run_end_date is null
AND		abnormal_termination_flag = 'N'
AND		annuity_eligibility_run_option_code = :as_annuity_eligibility_run_option_code
USING SQLCA;
SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_get_AE_run_option_count', 'SELECT	Count(*) FROM ANNUITY_ELIGIBILITY_RUN...')

RETURN li_count
end function

public function integer wf_get_last_confirmed_data (ref datetime adtm_last_confirmed, ref datetime adtm_annuity_start_date, ref datetime adtm_annuity_end_date, ref string as_benefit_option_code, ref decimal adec_annuity_set_aside_percent);INTEGER		li_counter, li_upper, li_last_confirmed_row
DATETIME	ldtm_new_confirmed_date, ldtm_last_confirmed_date

SetNull(ldtm_last_confirmed_date)

li_upper = tab_eligibility.tabpage_history_eligibility.dw_history_eligibility.RowCount()

FOR li_counter = 1 TO li_upper
	ldtm_new_confirmed_date = tab_eligibility.tabpage_history_eligibility.dw_history_eligibility.GetItemDateTime(li_counter, 'confirmed_date')
	IF IsNull(ldtm_new_confirmed_date) THEN
		CONTINUE
	ELSE
		IF IsNull(ldtm_last_confirmed_date) THEN ldtm_last_confirmed_date = DateTime('1901-01-01')
		IF ldtm_new_confirmed_date > ldtm_last_confirmed_date THEN
			li_last_confirmed_row = li_counter
			ldtm_last_confirmed_date = ldtm_new_confirmed_date
		END IF
	END IF
NEXT

IF NOT IsNull(ldtm_last_confirmed_date) THEN
	adtm_last_confirmed = ldtm_last_confirmed_date
	adtm_annuity_start_date = tab_eligibility.tabpage_history_eligibility.dw_history_eligibility.GetItemDateTime(li_last_confirmed_row, 'annuity_start_date')
	adtm_annuity_end_date = tab_eligibility.tabpage_history_eligibility.dw_history_eligibility.GetItemDateTime(li_last_confirmed_row, 'annuity_end_date')
	as_benefit_option_code = tab_eligibility.tabpage_history_eligibility.dw_history_eligibility.GetItemString(li_last_confirmed_row, 'benefit_option_code')
	adec_annuity_set_aside_percent = tab_eligibility.tabpage_history_eligibility.dw_history_eligibility.GetItemDecimal(li_last_confirmed_row, 'annuity_set_aside_percent')
	RETURN 1
ELSE
	RETURN -1
END IF
end function

public subroutine wf_reject_checklist_status_change (boolean ab_message, string as_message_title, string as_message, long al_checklist_no, string as_checklist_type_code);IF ab_message THEN
	MessageBox(as_message_title,as_message,Exclamation!)
END IF

SQLCA.nf_rollback_transaction()
					
// the retrieval will set the dw item status to not modified & prevent firing of ue_itemchangeaccepted
inv_checklist.nf_retrieve_checklists(as_checklist_type_code, al_checklist_no)
wf_select_next_available_checklist_step(al_checklist_no,as_checklist_type_code)
SetRedraw(TRUE)
end subroutine

public function integer wf_incomplete_prepare_acct_vbe_checklist (long al_verify_be_checklist_no, long al_annuity_account_no);INTEGER		li_vbe_checklist_count

SELECT	Count(*)
INTO		:li_vbe_checklist_count
FROM		CHECKLIST a
JOIN		SUBSCRIBER_CHECKLIST_XREF b ON a.checklist_no = b.checklist_no
JOIN		CHECKLIST_SUBSCRIBER c ON b.checklist_subscriber_no = c.checklist_subscriber_no
JOIN		ANNUITY_ACCOUNT d ON c.checklist_subscriber_no = d.checklist_subscriber_no
WHERE	a.checklist_status_code = 'IA'
AND		a.checklist_type_code = 'VBE'
AND		a.checklist_no <> :al_verify_be_checklist_no
AND		d.annuity_account_no = :al_annuity_account_no
USING SQLCA;
SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_incomplete_prepare_acct_VBE_checklist', 'SELECT	Count(*) FROM CHECKLIST...')

IF li_vbe_checklist_count > 0 THEN
	MessageBox('Prepare Annuity Account','There is a Verify Benefit Entitlement checklist that has been started from the Prepare Annuity Account module.'&
													+'~r~nIt must be completed before a new checklist is started from the Confirm Annuity Eligibility module.',Exclamation!)
	RETURN -1
ELSE
	RETURN 0
END IF
end function

public subroutine wf_set_process_run_status (string as_in_progress_flag);SQLCA.nf_begin_transaction()

UPDATE	PROCESS_RUN_STATUS
SET		in_progress_flag = :as_in_progress_flag
WHERE	module_code = '048'
USING SQLCA;
SQLCA.nf_handle_error('w_confirm_annuity_eligibility','UPDATE PROCESS_RUN_STATUS','wf_set_process_run_status')

SQLCA.nf_commit_transaction()

end subroutine

public function datetime wf_get_ae_last_run_date (string as_annuity_eligibility_run_option_code);DATETIME		ldtm_last_run

SELECT	max(annuity_eligibility_run_end_date)
INTO		:ldtm_last_run
FROM		ANNUITY_ELIGIBILITY_RUN
WHERE	annuity_eligibility_run_option_code = :as_annuity_eligibility_run_option_code
AND		abnormal_termination_flag = 'N'
USING SQLCA;
SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_get_ae_last_run_date', 'SELECT max(create_date) FROM ANNUITY_ELIGIBILITY_RUN...')

RETURN ldtm_last_run
end function

public subroutine wf_set_last_generated_datetime (boolean ab_visible, datetime adtm_last_generated);cb_generate_list.Visible = ab_visible
st_last_generated.Visible = ab_visible
st_last_generated_dtm.Visible = ab_visible

IF ab_visible THEN
	st_last_generated_dtm.Text = String(adtm_last_generated,'yyyy-mm-dd h:mm:ss am/pm')
END IF
end subroutine

public function integer wf_confirm_annuity_eligibility (long al_annuity_account_no, long al_individual_no, long al_claim_no);/*
function updates AE record with confirmed date, updates annuity start & end dates if eligible, changes AE status, AE reason where applicable

arguments
adtm_65th_birthday, adtm_death_date - used to determine AE end date
al_annuity_account_no
al_individual_no
al_claim_no

*/

INTEGER				li_selected_tab, li_pending_row, li_active_row, li_rtn, li_counter, li_benefit_entitlement_rowcount, li_rows
LONG					ll_annuity_eligibility_no, ll_benefit_entitlement_no, ll_last_confirmed_annuity_eligibility_no
STRING				ls_pre_1993_annuity_eligibility_flag, ls_individual_type
DATAWINDOWCHILD	ldwc_child
DATETIME				ldtm_server_datetime


li_selected_tab = wf_get_selected_confirm_eligibility_tab()

// Re-examine annuity eligibility
li_benefit_entitlement_rowcount = ids_reexamine_benefit_entitlement.RowCount()

// remove filter
li_rtn = wf_set_current_AE_filter('')
					
li_rows = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.Retrieve(al_annuity_account_no)

IF is_claim_role_code = 'C' THEN
	ls_individual_type = 'claimant'
ELSE
	ls_individual_type = 'surviving spouse'
END IF

// determine active & pending A/E records for 'changed' eligibilities
IF li_selected_tab = CHANGED_LIST OR li_selected_tab = PAYOUT_LIST THEN
	// find pending record
	li_pending_row = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.uf_find_row('annuity_eligibility_status_code', '"P"')
	
	// find active record
	li_active_row = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.uf_find_row('annuity_eligibility_status_code','"A"')
	
	IF li_selected_tab = CHANGED_LIST THEN
		// there must always be an active eligibility for the changed list
		IF li_active_row = 0 THEN
			Error.Text = 'There is no active annuity eligibility associated with this '+ls_individual_type+' ('+String(al_individual_no)+') on the "changed" list'
			RETURN -1
		END IF
		
		tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.SetItem(li_active_row,'annuity_eligibility_status_code','I')
	ELSEIF li_selected_tab = PAYOUT_LIST THEN
		IF li_active_row = 0 THEN
			// there might not be an active row if the individual was found inactive during confirmation of the potential change eligibility.
			SELECT a.annuity_eligibility_no
			INTO   :ll_last_confirmed_annuity_eligibility_no
			FROM   ANNUITY_ELIGIBILITY a
			WHERE  a.annuity_account_no = :al_annuity_account_no
			AND    a.confirmed_date     IS NOT NULL
			AND    NOT EXISTS ( SELECT * 
									  FROM   ANNUITY_ELIGIBILITY b
									  WHERE  b.annuity_account_no = a.annuity_account_no
									  AND    b.confirmed_date     > a.confirmed_date )
			USING SQLCA;
			SQLCA.nf_handle_error('w_confirm_annuity_eligibility','embedded SQL: SELECT annuity_eligibility_no FROM ANNUITY_ELIGIBILITY','wf_confirm_annuity_eligibility')
			
			UPDATE ANNUITY_ELIGIBILITY
			SET    annuity_eligibility_status_code = 'I'
			WHERE  annuity_eligibility_no = :ll_last_confirmed_annuity_eligibility_no
			USING SQLCA;
			SQLCA.nf_handle_error('w_confirm_annuity_eligibility','embedded SQL: UPDATE ANNUITY_ELIGIBILITY','wf_confirm_annuity_eligibility')
		ELSE
			tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.SetItem(li_active_row,'annuity_eligibility_status_code','I')
		END IF
		
	END IF
	

	
	IF li_pending_row = 0 THEN
		Error.ObjectEvent="wf_confirm_annuity_eligibility"
		Error.Text = 'There is no pending annuity eligibility associated with this '+ls_individual_type+' ('+String(al_individual_no)+') on the "changed" list'
		RETURN -1
	END IF

ELSE
	li_pending_row = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.uf_find_row('annuity_eligibility_status_code', '"P"')
END IF

IF li_pending_row = 0 THEN
	Error.ObjectEvent="wf_confirm_annuity_eligibility"
	Error.Text = 'There is no pending annuity eligibility associated with this '+ls_individual_type+' ('+String(al_individual_no)+') on the "new" list'
	RETURN -1
END IF
		
// if benefit entitlement exists
IF li_benefit_entitlement_rowcount > 0 THEN
	// the IW or SS was determined to be entitled
	li_rtn = wf_set_entitled_data(li_pending_row, li_active_row, li_selected_tab,al_individual_no, al_claim_no)
	
ELSE
	// the IW or SS was determined to be not entitled	
	li_rtn = wf_set_not_entitled_data(li_pending_row, li_selected_tab,'')
	
END IF

IF li_rtn < 0 THEN RETURN li_rtn

ldtm_server_datetime = f_server_datetime()
li_rtn = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.SetItem(li_pending_row,'confirmed_date',ldtm_server_datetime)
li_rtn = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.SetItem(li_pending_row,'confirmed_by_user_id',vgst_user_profile.user_id)

tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.AcceptText()

ll_annuity_eligibility_no = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.GetItemNumber(li_pending_row,'annuity_eligibility_no')
	
IF is_claim_role_code = 'C' THEN
//	injured workers
	
	SELECT	pre_1993_annuity_eligibility_flag
	INTO		:ls_pre_1993_annuity_eligibility_flag
	FROM		ANNUITY_ELIGIBILITY
	WHERE	annuity_eligibility_no = :ll_annuity_eligibility_no
	USING SQLCA;
	SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_confirm_annuity_eligibility', 'embedded SQL: SELECT pre_1993_annuity_eligibility_flag FROM ANNUITY_ELIGIBILITY...')
	
	IF ls_pre_1993_annuity_eligibility_flag = 'N' THEN
		// create ANNUITY_ELIGIBILITY_BENEFIT_ENTITLEMENT_XREF	record for each benefit entitlement
		FOR li_counter = 1 TO li_benefit_entitlement_rowcount
			ll_benefit_entitlement_no = ids_reexamine_benefit_entitlement.GetItemNumber(li_counter,'benefit_entitlement_no')
			
			INSERT	ANNUITY_ELIGIBILITY_BENEFIT_ENTITLEMENT_XREF
						(	annuity_account_no,
							annuity_eligibility_no,
							benefit_entitlement_no )
			VALUES	(	:al_annuity_account_no ,
							:ll_annuity_eligibility_no ,
							:ll_benefit_entitlement_no )
			USING SQLCA;
			SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_confirm_annuity_eligibility', 'INSERT ANNUITY_ELIGIBILITY_BENEFIT_ENTITLEMENT_XREF')
				
		NEXT
	END IF
END IF


RETURN 0
end function

public function integer wf_get_last_confirmed_data (ref long al_last_inactive_annuity_eligibility_no);INTEGER         li_counter, li_upper, li_last_confirmed_row, li_rtn
DATETIME        ldtm_new_confirmed_date, ldtm_last_confirmed_date
U_DATAWINDOW    ldw_eligibility


/*

returns annuity_eligibility_no for the annuity eligibility that was last confirmed

*/

SetNull(ldtm_last_confirmed_date)

tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.AcceptText()

li_rtn = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.SetFilter('')
li_rtn = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.Filter()

// this DW potentially has uncommitted data that we need
// if the IW was eligible, then the latest confirmed date will be assoc with the newly updated active A/E record
// if the IW was ineligible, then the latest confirmed date will potentially be assoc with an existing A/E record on the history DW

li_upper = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.RowCount()

// If li_upper = 0 then IW was not eligible, because of where clause on current elig DW, so
// use the history elig DW instead
IF li_upper > 0 THEN
	ldw_eligibility = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility
ELSE
	ldw_eligibility = tab_eligibility.tabpage_history_eligibility.dw_history_eligibility
	li_upper = ldw_eligibility.RowCount()
END IF

FOR li_counter = 1 TO li_upper
	ldtm_new_confirmed_date = ldw_eligibility.GetItemDateTime(li_counter, 'confirmed_date')
	IF IsNull(ldtm_new_confirmed_date) THEN
		CONTINUE
	ELSE
		IF IsNull(ldtm_last_confirmed_date) THEN ldtm_last_confirmed_date = DateTime('1900-01-01')
		IF ldtm_new_confirmed_date > ldtm_last_confirmed_date THEN
			li_last_confirmed_row = li_counter
			ldtm_last_confirmed_date = ldtm_new_confirmed_date
		END IF
	END IF
NEXT

IF NOT IsNull(ldtm_last_confirmed_date) THEN
	al_last_inactive_annuity_eligibility_no = ldw_eligibility.GetItemNumber(li_last_confirmed_row, 'annuity_eligibility_no')
	RETURN 1
ELSE
	RETURN -1
END IF
end function

public function integer wf_check_missing_annuity_interest_rate (long al_annuity_eligibility_no);/*

If there is a missing interest rate in the middle of annuity eligibility period ---- return -1
If there is any quarter in the annuity eligibility period where the only rate is either unapplied or inactive ---- return -1

*/

INTEGER				li_rows, li_counter, li_year, li_quarter_no
INTEGER				li_inactive_unapplied_annuity_rate_count, li_no_annuity_rate_count
u_ds					lds_inactive_unapplied_annuity_interest_rates, lds_nonexistent_annuity_interest_rates


lds_inactive_unapplied_annuity_interest_rates = create u_ds
lds_inactive_unapplied_annuity_interest_rates.DataObject = 'ds_inactive_unapplied_annuity_interest_rates'
lds_inactive_unapplied_annuity_interest_rates.SetTransObject(SQLCA)

// Retrieve all quarters that have unapplied or inactive rates
// and there is no applied or active rate for that quarter
li_rows = lds_inactive_unapplied_annuity_interest_rates.Retrieve()

IF li_rows > 0 THEN
	FOR li_counter = 1 TO li_rows
		// Get year & quarter number assoc with for last quarter of eligibility
		li_year = lds_inactive_unapplied_annuity_interest_rates.GetItemNumber(li_counter,'year')
		li_quarter_no = lds_inactive_unapplied_annuity_interest_rates.GetItemNumber(li_counter,'quarter_no')
		
		SELECT Count(*)
		INTO   :li_inactive_unapplied_annuity_rate_count
		FROM   BENEFIT_ENTITLEMENT a
		JOIN   ANNUITY_ELIGIBILITY b  ON a.annuity_account_no             = b.annuity_account_no
											  AND a.benefit_entitlement_from_date >= b.annuity_start_date
		WHERE  a.active_flag = 'Y'
		AND    year(a.benefit_entitlement_from_date) = :li_year
		AND    CASE when month(a.benefit_entitlement_from_date) < 4 THEN 1
						when month(a.benefit_entitlement_from_date) > 3 AND month(a.benefit_entitlement_from_date) < 7  THEN 2
						when month(a.benefit_entitlement_from_date) > 6 AND month(a.benefit_entitlement_from_date) < 10 THEN 3
						ELSE 4 
				 END = :li_quarter_no
		AND    b.annuity_eligibility_no = :al_annuity_eligibility_no
		USING SQLCA;
		SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'uo_checklist.ue_checklist_buttonclicked', 'SELECT Count(*) FROM BENEFIT_ENTITLEMENT JOIN ANNUITY_ELIGIBILITY...')
		
		// if there is post-qualification benefit entitlement for the quarter that is unapplied or inactive
		// and there is no applied or active rate for that quarter, then exit & display error
		IF li_inactive_unapplied_annuity_rate_count > 0 THEN
			EXIT
		END IF
	NEXT
	
	IF li_inactive_unapplied_annuity_rate_count > 0 THEN
		MessageBox('Annuity Interest Rate Error','There are inactive or unapplied annuity interest rates (year = '+String(li_year)+ ', quarter = ' +String(li_quarter_no)+ ') for the post-qualification benefit entitlement period.'&
													  + '~r~nThe Annuity Calculation module cannot be opened for this individual until this problem is resolved.', Exclamation!)
		RETURN -1
	END IF
	
END IF

lds_nonexistent_annuity_interest_rates = create u_ds
lds_nonexistent_annuity_interest_rates.DataObject = 'ds_nonexistent_annuity_interest_rates'
lds_nonexistent_annuity_interest_rates.SetTransObject(SQLCA)


// Retrieve all quarters that have no rate for that quarter
li_rows = lds_nonexistent_annuity_interest_rates.Retrieve()

IF li_rows > 0 THEN
	FOR li_counter = 1 TO li_rows
		// Get year & quarter number assoc with for last quarter of eligibility
		li_year = lds_nonexistent_annuity_interest_rates.GetItemNumber(li_counter,'year')
		li_quarter_no = lds_nonexistent_annuity_interest_rates.GetItemNumber(li_counter,'missing_quarter')
		
		SELECT Count(*)
		INTO   :li_no_annuity_rate_count
		FROM   BENEFIT_ENTITLEMENT a
		JOIN   ANNUITY_ELIGIBILITY b  ON a.annuity_account_no             = b.annuity_account_no
											  AND a.benefit_entitlement_from_date >= b.annuity_start_date
		WHERE  a.active_flag = 'Y'
		AND    year(a.benefit_entitlement_from_date) = :li_year
		AND    CASE when month(a.benefit_entitlement_from_date) < 4 THEN 1
						when month(a.benefit_entitlement_from_date) > 3 AND month(a.benefit_entitlement_from_date) < 7  THEN 2
						when month(a.benefit_entitlement_from_date) > 6 AND month(a.benefit_entitlement_from_date) < 10 THEN 3
						ELSE 4 
				 END = :li_quarter_no
		AND    b.annuity_eligibility_no = :al_annuity_eligibility_no
		USING SQLCA;
		SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'uo_checklist.ue_checklist_buttonclicked', 'SELECT Count(*) FROM BENEFIT_ENTITLEMENT JOIN ANNUITY_ELIGIBILITY...')
		
		// if there is post-qualification benefit entitlement for the quarter that is unapplied or inactive
		// and there is no applied or active rate for that quarter, then exit & display error
		IF li_no_annuity_rate_count > 0 THEN
			EXIT
		END IF
	NEXT
	
	// if there is post-qualification benefit entitlement for the quarter that has no annuity interest rate at all
	IF li_no_annuity_rate_count > 0 THEN
		MessageBox('Annuity Interest Rate Error','There is a missing annuity interest rate for the post-qualification benefit entitlement period.'&
													  + '~r~nThe Annuity Calculation module cannot be opened for this individual until this problem is resolved.', Exclamation!)
		RETURN -1
	END IF
	
END IF


RETURN 0
end function

protected function boolean wf_auto_not_require_calc_step (string as_claim_role_code, long al_annuity_account_no, long al_annuity_eligibility_no, long al_individual_no, long al_claim_no, datetime adtm_new_annuity_start_date, datetime adtm_new_annuity_end_date, boolean ab_eligible);DECIMAL   ldec_total_net_payment_amount
LONG      ll_txn_count, ll_benefit_entitlement_count


/*

wf_auto_not_require_calc_step

returns TRUE - if the Calculation step in the Confirm Annuity Eligibility checklist
should automatically be set to 'Not Required'. For injured workers BRs 2.230 & 2.70
indicate when TRUE should be returned. For surviving spouses, BRs 2.240 & 2.280 apply

returns FALSE - if the Calculation step in the Confirm Annuity Eligibility checklist
should NOT automatically be set to 'Not Required', i.e., when the calculation is required.

*/


IF as_claim_role_code = 'C' THEN
	
	// returns post-92 setaside count & balance
	inv_common_annuity.nf_post_92_setasides_for_IW(al_individual_no,ll_txn_count,ldec_total_net_payment_amount)

	IF NOT ab_eligible THEN
		// INELIGIBLE CLAIMANT
		/*
		BR 2.230	
		   The calculate account balance checklist step must be automatically set to ‘Not Required’ if all of 
		   the following are true:
			·	The individual is an injured worker
			·	The individual is confirmed as not eligible for annuity 
			·	There are no sub-ledger transactions, for payment types that attract annuities for the injured worker,
				and must only include annuity payments that are not part of the pre-1993 annuity sub-ledger.

		*/
		
		IF ll_txn_count = 0 THEN
			RETURN TRUE
		END IF
	ELSE
		// ELIGIBLE CLAIMANT
		/*
		BR 2.270
		   The Calculate Account Balance checklist step must be automatically set to ‘Not Required’, if all of the following are true:
			·	The individual is an injured worker
			·	The injured worker is confirmed as eligible for annuity benefits
			·	There is no benefit entitlement within the annuity eligibility period
			·	The annuity sub-ledger balance is zero, for payment types that attract annuities for the injured worker,
			   and must only include annuity payments that are not part of the pre-1993 annuity sub-ledger.
		*/
		
		// retrieve count of post qualification BE for individual
		inv_common_annuity.nf_post_qualification_ben_entitlement(al_annuity_eligibility_no,adtm_new_annuity_start_date,&
		                                                         adtm_new_annuity_end_date,ll_benefit_entitlement_count)
		
		// setaside sums to zero, no post qualification benefit entitlement
		IF ldec_total_net_payment_amount = 0 AND ll_benefit_entitlement_count = 0 THEN
			RETURN TRUE
		END IF
	END IF
	
ELSEIF as_claim_role_code = 'SS' THEN
	
	// returns post-81 setaside count & balance
	inv_common_annuity.nf_post_81_setasides_for_ss(al_individual_no,al_claim_no,ll_txn_count,ldec_total_net_payment_amount)

	IF NOT ab_eligible THEN
		// INELIGIBLE SURVIVING SPOUSE
		/*
		BR 2.240
		   The calculate account balance checklist step must be automatically set to ‘Not Required’ if all of the following are true:
			·	The individual is a surviving spouse
			·	The individual is confirmed as not eligible for annuity 
			·	There are no sub-ledger transactions, for payment types that attract annuities for the surviving spouse, 
			   for a period since January 1, 1982.

		*/
		IF ll_txn_count = 0 THEN
			RETURN TRUE
		END IF
	END IF
END IF

RETURN FALSE

end function

public function integer wf_check_unapplied_manual_set_asides (long al_annuity_eligibility_no, string as_claim_role_code);/*

If there are unapplied manual set-asides (txn type 8) for the given annuity eligibility, then return -1

*/

INTEGER    li_count

IF as_claim_role_code = 'C' THEN
	
   SELECT Count(*)
   INTO   :li_count
   FROM   UNAPPLIED_CLAIM_TXN a
   JOIN   PAYMENT             b  ON a.payment_no = b.payment_no
   JOIN   CLAIM_PARTICIPANT   c  ON b.claim_no = c.claim_no
   JOIN   ANNUITY_ACCOUNT     d  ON c.individual_no   = d.individual_no
                                AND c.claim_role_code = d.claim_role_code
   JOIN   ANNUITY_ELIGIBILITY e  ON d.annuity_account_no = e.annuity_account_no
   WHERE  a.txn_type_code          = '8'
   AND    a.recipient_type_code    = 'I'
   AND    a.txn_unit_of_work_no    = 0
   AND    b.payment_type_code      = '97'
   AND    c.claim_role_code        = 'C'
   AND    e.annuity_eligibility_no = :al_annuity_eligibility_no
   AND NOT EXISTS ( SELECT *
                    FROM   ANNUITY_PAYOUT_PRE_1993 x
                    WHERE  x.payout_payment_no = b.payment_no )
   USING SQLCA;
   SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'embedded SQL: select count from UNAPPLIED_CLAIM_TXN,CLAIM_PARTICIPANT,ANNUITY_ACCOUNT,ANNUITY_ELIGIBILITY...', 'wf_check_unapplied_manual_set_asides (1)')
	
ELSEIF as_claim_role_code = 'SS' THEN
	
   SELECT Count(*)
   INTO   :li_count
   FROM   UNAPPLIED_CLAIM_TXN a
   JOIN   PAYMENT             b  ON a.payment_no = b.payment_no
   JOIN   CLAIM_PARTICIPANT   c  ON a.claim_no     = c.claim_no
   JOIN   ANNUITY_ACCOUNT     d  ON c.individual_no   = d.individual_no
                                AND c.claim_role_code = d.claim_role_code
                                AND c.claim_no        = d.claim_no
   JOIN   ANNUITY_ELIGIBILITY e  ON d.annuity_account_no = e.annuity_account_no
   WHERE  a.txn_type_code          = '8'
   AND    a.recipient_type_code    = 'I'
   AND    a.txn_unit_of_work_no    = 0
   AND    b.payment_type_code      = '97'
   AND    c.claim_role_code        = 'SS'
   AND    e.annuity_eligibility_no = :al_annuity_eligibility_no
   AND NOT EXISTS ( SELECT *
                    FROM   ANNUITY_PAYOUT_PRE_1993 x
                    WHERE  x.payout_payment_no = b.payment_no )
   USING SQLCA;
   SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'embedded SQL: select count from UNAPPLIED_CLAIM_TXN,CLAIM_PARTICIPANT,ANNUITY_ACCOUNT,ANNUITY_ELIGIBILITY...', 'wf_check_unapplied_manual_set_asides (2)')
	
ELSE
	MessageBox('Annuity Calculation Error','Incorrect claim role code - call helpdesk.',StopSign!)
	RETURN -1
END IF

IF li_count > 0 THEN
	IF as_claim_role_code = 'C' THEN
		MessageBox('Annuity Calculation Error','There are unprocessed manual annuity benefit transactions for a claim associated with this injured worker. '&
		                                     + 'An annuity calculation cannot be created until these transactions are processed.',StopSign!)
	ELSE
		MessageBox('Annuity Calculation Error',"The surviving spouse's claim has unprocessed manual annuity benefit transactions. "&
		                                     + 'An annuity calculation cannot be created until these transactions are processed.',StopSign!)
	END IF
END IF

RETURN li_count
end function

protected function boolean wf_auto_not_allow_calc_step (string as_claim_role_code, long al_annuity_account_no, long al_annuity_eligibility_no, long al_individual_no, long al_claim_no, datetime adtm_new_annuity_start_date, datetime adtm_new_annuity_end_date, boolean ab_eligible);DECIMAL   ldec_total_net_payment_amount
INTEGER   li_CM_count
LONG      ll_txn_count, ll_benefit_entitlement_count


/*

wf_auto_not_allow_calc_step

returns TRUE - if the Calculation step in the Confirm Annuity Eligibility checklist
should automatically be set to 'Not Allowed'.

returns FALSE - if the Calculation step in the Confirm Annuity Eligibility checklist
should NOT automatically be set to 'Not Allowed', i.e., when the calculation is required.

covers the business rules listed below

*/

IF as_claim_role_code = 'C' THEN
	// count of annuity credit memos for injured worker in all claims (as claimant)
	// where payment processed prior to parameter date
	SELECT Count(*)
	INTO   :li_CM_count
	FROM   PAYMENT           a
	JOIN   APPLIED_CLAIM_TXN b ON a.payment_no     = b.payment_no
	JOIN   CLAIM_PARTICIPANT c ON b.claim_no       = c.claim_no
	JOIN   Annuity_Parameter d ON a.processed_date < d.use_calculate_annuity_module_date
	WHERE  a.payment_type_code     = '97'
	AND    a.payment_sub_type_code = 'CM'
	AND    b.recipient_type_code   = 'I'
	AND    b.recipient_no          = :al_individual_no
	AND    c.claimant_active_flag  = 'Y'
	AND    c.claim_role_code       = 'C'
	AND    d.active_flag           = 'Y'
	AND NOT EXISTS ( SELECT *
	                 FROM   ANNUITY_PAYOUT_PRE_1993 e
						  WHERE  e.payout_payment_no = a.payment_no )
	USING SQLCA;
	SQLCA.nf_handle_error('w_confirm_annuity_eligibility','wf_auto_not_allow_calc_step', 'embedded SQL for (IW): SELECT * FROM PAYMENT,APPLIED_CLAIM_TXN,CLAIM_PARTICIPANT,Annuity_Parameter...')

ELSE
	// count of annuity credit memos for surviving spouse in specific claim
	// where payment processed prior to parameter date
	SELECT Count(*)
	INTO   :li_CM_count
	FROM   PAYMENT           a
	JOIN   APPLIED_CLAIM_TXN b ON a.payment_no     = b.payment_no
	JOIN   CLAIM_PARTICIPANT c ON b.claim_no       = c.claim_no
	JOIN   Annuity_Parameter d ON a.processed_date < d.use_calculate_annuity_module_date
	WHERE  a.payment_type_code     = '97'
	AND    a.payment_sub_type_code = 'CM'
	AND    b.recipient_type_code   = 'I'
	AND    b.recipient_no          = :al_individual_no
	AND    c.claim_no              = :al_claim_no
	AND    c.claim_role_code       = 'SS'
	AND    d.active_flag           = 'Y'
	USING SQLCA;
	SQLCA.nf_handle_error('w_confirm_annuity_eligibility','wf_auto_not_allow_calc_step', 'embedded SQL for (SS): SELECT * FROM PAYMENT,APPLIED_CLAIM_TXN,CLAIM_PARTICIPANT,Annuity_Parameter...')

END IF


// if there was a pre-implementation 97/CM then possible 'NOT ALLOWED' status for checklist step
IF li_CM_count > 0 THEN
	
	IF as_claim_role_code = 'C' THEN
		
		// returns post-92 setaside count & balance
		inv_common_annuity.nf_post_92_setasides_for_IW(al_individual_no,ll_txn_count,ldec_total_net_payment_amount)
	
		IF NOT ab_eligible THEN
			// INELIGIBLE CLAIMANT
			/*
				BR 2.300
				The Calculate Annuity checklist step must be automatically set to ‘Not Allowed’, if all of the following are true:
				·	The individual is an injured worker
				·	The individual is confirmed as not eligible for annuity benefits
				·	There was a post-1992 annuity payout processed before the Calculate Annuity module became effective
				·	There are post-1992 annuity sub-ledger transactions for payment types that attract annuities for the injured worker.
			*/
			
			IF ll_txn_count > 0 THEN
				RETURN TRUE
			END IF
		ELSE
			// ELIGIBLE CLAIMANT
			/*
				BR 2.280	
				The Calculate Annuity checklist step must be automatically set to ‘Not Allowed’, if all of the following are true:
				·	The individual is an injured worker
				·	The injured worker is confirmed as eligible for annuity benefits
				·	There was a post-1992 annuity payout processed before the Calculate Annuity module became effective
				·	There is either benefit entitlement within the annuity eligibility period or the post-1992 annuity sub-ledger balance is non-zero for payment types that attract annuities for an injured worker.		
			*/
			
			// retrieve count of post qualification BE for individual
			inv_common_annuity.nf_post_qualification_ben_entitlement(al_annuity_eligibility_no,adtm_new_annuity_start_date,&
																						adtm_new_annuity_end_date,ll_benefit_entitlement_count)
			
			// setaside sums to zero, no post qualification benefit entitlement
			IF ldec_total_net_payment_amount <> 0 OR ll_benefit_entitlement_count <> 0 THEN
				RETURN TRUE
			END IF
		END IF
		
	ELSEIF as_claim_role_code = 'SS' THEN
		
		// returns post-81 setaside count & balance
		inv_common_annuity.nf_post_81_setasides_for_ss(al_individual_no,al_claim_no,ll_txn_count,ldec_total_net_payment_amount)
	
		IF NOT ab_eligible THEN
			// INELIGIBLE SURVIVING SPOUSE
			/*
				BR 2.310
				The Calculate Annuity checklist step must be automatically set to ‘Not Allowed’, if all of the following are true:
				·	The individual is an surviving spouse
				·	The surviving spouse is confirmed as not eligible for annuity benefits for the claim
				·	There was an annuity payout processed before the Calculate Annuity module became effective
				·	There are annuity sub-ledger transactions for the claim for payment types that attract annuities for the surviving spouse for a period since January 1, 1982. 
			*/
			
			IF ll_txn_count <> 0 THEN
				RETURN TRUE
			END IF
		ELSE
			// ELIGIBLE SURVIVING SPOUSE
			/*
				BR 2.290
				The Calculate Annuity checklist step must be automatically set to ‘Not Allowed’, if all of the following are true:
				·	The individual is an surviving spouse
				·	The surviving spouse is confirmed as eligible for annuity benefits for the claim
				·	There was an annuity payout processed before the Calculate Annuity module became effective
				·	The annuity sub-ledger balance is non-zero for the claim for payment types that attract annuities for a surviving spouse, for a period since January 1, 1982.
			*/
			
			// setaside sums to zero, no post qualification benefit entitlement
			IF ldec_total_net_payment_amount <> 0 THEN
				RETURN TRUE
			END IF		
		END IF
	END IF
END IF

RETURN FALSE

end function

public subroutine wf_individual_error (string as_message);STRING       ls_checklist_type_code
U_DATAWINDOW ldw_checklist

ldw_checklist = uo_checklist.tab_checklist.tabpage_checklist.dw_checklist

ls_checklist_type_code = ldw_checklist.GetItemString(ldw_checklist.GetRow(),'checklist_type_code')

IF is_claim_role_code = 'C' THEN
	IF ls_checklist_type_code = 'CAEP' THEN
		MessageBox('Individual Not Found',as_message + '~r~nPlease close this module and re-open.', Exclamation!)
	ELSE
		IF vgst_user_profile.default_admin_region_code <> 'PRV' THEN
			is_annuity_eligibility_run_option_code = 'IA'
			wf_select_ae_run_option()
		
			MessageBox('Individual Not Found',as_message + '~r~nThe "All Injured Workers" list will be retrieved.', Exclamation!)
			wf_retrieve_all_injured_workers()
		ELSE
			is_annuity_eligibility_run_option_code = 'SA'
			wf_select_ae_run_option()
						
			MessageBox('Individual Not Found',as_message + '~r~nThe "All Surviving Spouses" list will be retrieved.', Exclamation!)
			wf_retrieve_all_surviving_spouses()
		END IF
	END IF
ELSE
	IF ls_checklist_type_code = 'CAEP' THEN
		MessageBox('Individual Not Found',as_message + '~r~nPlease close this module and re-open.', Exclamation!)
	ELSE
		is_annuity_eligibility_run_option_code = 'SA'
		wf_select_ae_run_option()
		
		MessageBox('Individual Not Found',as_message + '~r~nThe "All Surviving Spouses" list will be retrieved.', Exclamation!)
		wf_retrieve_all_surviving_spouses()
	END IF	
END IF
end subroutine

public function integer wf_eligibility_dw_rowfocuschanging (u_datawindow adw_eligibility, long al_newrow, long al_currentrow);INTEGER   li_rtn


IF ib_individual_does_not_exist THEN
	// this boolean is set when the user has attempted 
	// to save a checklist note for an individual who no longer
	// exists & probably was merged. The intent is to prevent 
	// a checklist note error message from being needlessly displayed
	RETURN 0
END IF

// the clicked event of datawindow has row selection code that is common. Unfortunately, it
// triggers this event, so code was added to prevent the display of the warning more than once
IF ib_rfc_warning = FALSE THEN
	li_rtn = inv_checklist.nf_check_note_modified_status()
	IF li_rtn < 0 THEN ib_rfc_warning = TRUE
ELSE
	li_rtn = -1
	ib_rfc_warning = FALSE
END IF

IF li_rtn < 0 THEN
	adw_eligibility.SelectRow(al_newrow, FALSE)
	adw_eligibility.SelectRow(al_currentrow, TRUE)
	// prevent row change
	RETURN 1
ELSE
	RETURN 0
END IF
end function

public function integer wf_get_pre_1993_annuity_eligibility_flag (long al_annuity_account_no, ref string as_pre_1993_annuity_eligibility_flag);/*

returns the 'pre-1993 annuity eligibility status' for a given annuity account

*/

// use flag from last confirmed A/E if it exists
SELECT a.pre_1993_annuity_eligibility_flag
INTO   :as_pre_1993_annuity_eligibility_flag
FROM   ANNUITY_ELIGIBILITY a
WHERE  a.annuity_account_no = :al_annuity_account_no
AND    a.confirmed_date IS NOT NULL
AND NOT EXISTS ( SELECT *
                 FROM   ANNUITY_ELIGIBILITY b
                 WHERE  b.annuity_account_no      = :al_annuity_account_no
					  AND    b.annuity_eligibility_no <> a.annuity_eligibility_no
					  AND    b.confirmed_date          > a.confirmed_date )
AND NOT EXISTS ( SELECT *
                 FROM   ANNUITY_ELIGIBILITY b
                 WHERE  b.annuity_account_no     = :al_annuity_account_no
					  AND    b.annuity_eligibility_no > a.annuity_eligibility_no
					  AND    b.confirmed_date         = a.confirmed_date )
USING SQLCA;
SQLCA.nf_handle_error('w_confirm_annuity_eligibility','wf_get_pre_1993_annuity_eligibility_flag','SELECT pre_1993_annuity_eligibility_flag FROM ANNUITY_ELIGIBILITY...(2)')

// if a flag was found, return
IF NOT IsNull(as_pre_1993_annuity_eligibility_flag) AND as_pre_1993_annuity_eligibility_flag <> '' THEN
	RETURN 1
END IF


// use flag from max A/E if it exists
SELECT a.pre_1993_annuity_eligibility_flag
INTO   :as_pre_1993_annuity_eligibility_flag
FROM   ANNUITY_ELIGIBILITY a
WHERE  a.annuity_account_no = :al_annuity_account_no
AND    a.confirmed_date IS NULL
AND NOT EXISTS ( SELECT *
                 FROM   ANNUITY_ELIGIBILITY b
                 WHERE  b.annuity_account_no     = :al_annuity_account_no
					  AND    b.annuity_eligibility_no > a.annuity_eligibility_no )
AND NOT EXISTS ( SELECT *
                 FROM   ANNUITY_ELIGIBILITY b
                 WHERE  b.annuity_account_no     = :al_annuity_account_no
					  AND    b.confirmed_date IS NOT NULL )
USING SQLCA;
SQLCA.nf_handle_error('w_confirm_annuity_eligibility','wf_get_pre_1993_annuity_eligibility_flag','SELECT pre_1993_annuity_eligibility_flag FROM ANNUITY_ELIGIBILITY...(2)')

// if a flag was found, return
IF NOT IsNull(as_pre_1993_annuity_eligibility_flag) AND as_pre_1993_annuity_eligibility_flag <> '' THEN
	RETURN 1
ELSE
	RETURN 0
END IF


end function

public function integer wf_benefit_entitlement_checklist (long al_annuity_account_no, long al_annuity_eligibility_no, boolean ab_display_error_msgs);/*
function verifies that associated VBE checklist has been completed and that there are no other incomplete VBE checklists
if function rtns zero, then associated VBE checklist is complete
otherwise, not complete, doesnt exist etc
*/

INTEGER		li_count
LONG			ll_verify_ben_entitlement_checklist_no


SELECT	verify_benefit_entitlement_checklist_no
INTO		:ll_verify_ben_entitlement_checklist_no
FROM		ANNUITY_ELIGIBILITY
WHERE		annuity_account_no = :al_annuity_account_no
AND		annuity_eligibility_no = :al_annuity_eligibility_no
USING SQLCA;
SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_benefit_entitlement_checklist', 'SELECT verify_benefit_entitlement_checklist_no FROM ANNUITY_ELIGIBILITY...')

IF ll_verify_ben_entitlement_checklist_no > 0 THEN
	// there is an associated VBE checklist
	// is the checklist for the A/E finished
	SELECT	Count(*)
	INTO		:li_count
	FROM		ANNUITY_ELIGIBILITY	a
	JOIN		ANNUITY_ACCOUNT		b ON a.annuity_account_no = b.annuity_account_no
	JOIN		CHECKLIST				c ON a.verify_benefit_entitlement_checklist_no = c.checklist_no
	WHERE		a.annuity_eligibility_no = :al_annuity_eligibility_no
	AND		a.verify_benefit_entitlement_checklist_no = :ll_verify_ben_entitlement_checklist_no
	AND		b.annuity_account_no = :al_annuity_account_no
	AND		c.checklist_status_code = 'IA'
	USING SQLCA;
	SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_benefit_entitlement_checklist', 'SELECT count(*) FROM ANNUITY_ELIGIBILITY...')
	
	IF li_count > 0 THEN
		// VBE checklist not completed for A/E
		IF ab_display_error_msgs THEN
			MessageBox('Benefit Entitlement Problem','The Benefit Entitlement checklist for this annuity eligibility was not completed. Please complete the checklist before completing this step.',Exclamation!)
		END IF
		RETURN -1
	END IF

ELSE
	// there is not an associated VBE checklist
	// is there an incomplete VBE checklist not associated with this A/E, but still associated with the account?

	SELECT	Count(*)
	INTO		:li_count
	FROM		ANNUITY_ACCOUNT				a
	JOIN		CHECKLIST_SUBSCRIBER			b ON a.checklist_subscriber_no = b.checklist_subscriber_no
	JOIN		SUBSCRIBER_CHECKLIST_XREF	c ON b.checklist_subscriber_no = c.checklist_subscriber_no
	JOIN		CHECKLIST						d ON c.checklist_no = d.checklist_no
	WHERE		a.annuity_account_no = :al_annuity_account_no
	AND		b.checklist_subscriber_type_code = 'ANN'
	AND		d.checklist_status_code = 'IA'
	AND		d.checklist_type_code = 'VBE'
	USING SQLCA;
	SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_benefit_entitlement_checklist', 'SELECT checklist_no FROM ANNUITY_ELIGIBILITY...')
	
	IF li_count > 0 THEN
		// VBE checklist not completed for something other than A/E
		IF ab_display_error_msgs THEN
		MessageBox('Benefit Entitlement Problem','There is an incomplete Benefit Entitlement checklist for another module. Please complete the checklist before completing this step.',Exclamation!)
	END IF
		RETURN -1
	ELSE
		// no VBE checklist, SO module not even opened
		IF ab_display_error_msgs THEN 
			MessageBox('Benefit Entitlement Problem','There is no Benefit Entitlement checklist associated with this annuity eligibility record.'&
															+'~r~nPlease open the VBE module and complete the checklist before completing this step.',Exclamation!)
		END IF
		RETURN -1
	END IF
END IF

RETURN 0
end function

public subroutine wf_setup_payout_controls ();idw_iw_payout = tab_confirm_eligibility.tabpage_payout.dw_iw_payout
idw_ss_payout = tab_confirm_eligibility.tabpage_payout.dw_ss_payout

IF is_claim_role_code = 'C' THEN
	idw_iw_payout.Visible = TRUE
	idw_ss_payout.Visible = FALSE
ELSE
	idw_iw_payout.Visible = FALSE
	idw_ss_payout.Visible = TRUE
END IF

tab_confirm_eligibility.tabpage_new.Visible = FALSE
tab_confirm_eligibility.tabpage_changed.Visible = FALSE
tab_confirm_eligibility.tabpage_payout.Visible = TRUE
	
st_run_option.Visible = FALSE
dw_annuity_eligibility_run_option.Visible = FALSE

end subroutine

public subroutine wf_resize_module ();IF IsValid(inv_resize) THEN
ELSE
	inv_resize = Create n_resize
END IF
inv_resize.of_SetOrigSize (this.width,this.height - 250)

/* maximize frame while module is envoked */
THIS.WindowState = Maximized!

//'ScaleToRight'
inv_resize.of_register(tab_confirm_eligibility,'ScaleToRight')
inv_resize.of_register(tab_confirm_eligibility.tabpage_new.dw_new_iw_eligibility,'ScaleToRight')
inv_resize.of_register(tab_confirm_eligibility.tabpage_new.dw_new_ss_eligibility,'ScaleToRight')
inv_resize.of_register(tab_confirm_eligibility.tabpage_changed.dw_changed_iw_eligibility,'ScaleToRight')
inv_resize.of_register(tab_confirm_eligibility.tabpage_changed.dw_changed_ss_eligibility,'ScaleToRight')
inv_resize.of_register(tab_confirm_eligibility.tabpage_payout.dw_iw_payout,'ScaleToRight')
inv_resize.of_register(tab_confirm_eligibility.tabpage_payout.dw_ss_payout,'ScaleToRight')

//'ScaleToRight&Bottom'
inv_resize.of_register(tab_eligibility,'ScaleToRight&Bottom')
inv_resize.of_register(tab_eligibility.tabpage_current_eligibility.dw_current_eligibility,'ScaleToRight&Bottom')
inv_resize.of_register(tab_eligibility.tabpage_history_eligibility.dw_history_eligibility,'ScaleToRight&Bottom')
end subroutine

protected function long wf_post_checklist_generation (long al_individual_no, long al_annuity_account_no, long al_claim_no, long al_checklist_subscriber_no, long al_checklist_no, string as_checklist_type_code, boolean ab_new_checklist_subscriber);INTEGER       li_pending_count, li_active_count, li_ineligible_count, li_rtn, li_row
DATETIME      ldtm_null, ldtm_current
LONG          ll_annuity_eligibility_no, ll_rtn
STRING        ls_error_message, ls_pre_1993_annuity_eligibility_flag, ls_annuity_eligibility_reason_code, ls_module_code



SetNull(ldtm_null)
ldtm_current = f_server_datetime()


// update CHECKLIST_STEP for identified step
li_rtn = inv_checklist.nf_save_checklist_step(al_checklist_no, '001', 'COA', ldtm_current, vgst_user_profile.user_id, '','048',as_checklist_type_code,'A')
IF li_rtn < 0 THEN
	RETURN -1
END IF
		
// update ANNUITY_ACCOUNT
IF ab_new_checklist_subscriber = TRUE THEN
	inv_common_annuity.nf_update_subscriber_no(al_annuity_account_no,al_checklist_subscriber_no)
END IF
		
li_pending_count = wf_get_annuity_eligibility_count(al_individual_no, al_claim_no, 'P')
IF li_pending_count > 0 THEN
	IF al_claim_no = 0 THEN
		ls_error_message = 'The module was trying to create a pending annuity eligibility record, but one already existed for this individual '+String(al_individual_no)+'. Call the HELPDESK.'
	ELSE
		ls_error_message = 'The module was trying to create a pending annuity eligibility record, but one already existed for this claim '+String(al_claim_no)+' and this individual '+String(al_individual_no)+'. Call the HELPDESK.'
	END IF
	MessageBox('Pending Eligibility Error',ls_error_message)
	RETURN -1
END IF

// set pre_1993_annuity_eligibility_flag & annuity_eligibility_reason_code
CHOOSE CASE as_checklist_type_code
	CASE 'CAEIC'
		li_rtn = wf_get_pre_1993_annuity_eligibility_flag(al_annuity_account_no,ls_pre_1993_annuity_eligibility_flag)
		IF li_rtn = 0 THEN
			MessageBox('Error','The pre-1993 annuity eligibility could not be determined. Call the HELPDESK.')
			RETURN -1
		END IF
		ls_annuity_eligibility_reason_code = 'BEC'
		
	CASE 'CAEIN'
		li_rtn = wf_get_pre_1993_annuity_eligibility_flag(al_annuity_account_no,ls_pre_1993_annuity_eligibility_flag)
		IF li_rtn = 0 THEN
			// if there are active and/or ineligible ann.elig records, then the flag should
			// be able to be determined from them
			li_active_count = wf_get_annuity_eligibility_count(al_individual_no, al_claim_no, 'A')
			li_ineligible_count = wf_get_annuity_eligibility_count(al_individual_no, al_claim_no, 'I')
			IF li_active_count + li_ineligible_count <> 0 THEN
				MessageBox('Error','The pre-1993 annuity eligibility could not be determined. Call the HELPDESK.')
				RETURN -1
			ELSE
				ls_pre_1993_annuity_eligibility_flag = 'N'
			END IF
		END IF
		ls_annuity_eligibility_reason_code = 'INI'
		
	CASE 'CAEP'
		IF is_claim_role_code = 'C' THEN
			li_rtn = wf_get_pre_1993_annuity_eligibility_flag(al_annuity_account_no,ls_pre_1993_annuity_eligibility_flag)
			IF li_rtn = 0 THEN
				MessageBox('Error','The pre-1993 annuity eligibility could not be determined. Call the HELPDESK.')
				RETURN -1
			END IF
		ELSE
			ls_pre_1993_annuity_eligibility_flag = 'N'
		END IF
		ls_annuity_eligibility_reason_code = 'PAY'
		
	CASE 'CAESC'
		ls_pre_1993_annuity_eligibility_flag = 'N'
		ls_annuity_eligibility_reason_code = 'BEC'
		
	CASE 'CAESN'
		ls_pre_1993_annuity_eligibility_flag = 'N'
		ls_annuity_eligibility_reason_code = 'INI'
		
END CHOOSE

// insert pending record into ANNUITY_ELIGIBILITY
ll_annuity_eligibility_no = inv_common_annuity.nf_insert_annuity_eligibility(al_annuity_account_no, 'P' , ldtm_null , '', ldtm_null, ldtm_null, ldtm_null, 'N/A', 0, 0.00, ls_annuity_eligibility_reason_code, '', al_checklist_no, 0, ls_pre_1993_annuity_eligibility_flag, 'N')

RETURN ll_annuity_eligibility_no
end function

public function integer wf_merge_event_count (long al_individual_no, datetime adtm_last_confirmed, string as_event_specific_code);INTEGER		li_count

SELECT	Count(*)
INTO		:li_count
FROM		INDIVIDUAL_EVENT
WHERE	individual_no = :al_individual_no
AND		event_type_code = '042'
AND		event_specific_code = :as_event_specific_code
AND		create_date > :adtm_last_confirmed
USING SQLCA;
SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_merge_event_count', 'SELECT Count(*) FROM INDIVIDUAL_EVENT')

RETURN li_count
end function

public subroutine wf_determine_calc_step_auto_completion (string as_claim_role_code, long al_annuity_account_no, long al_annuity_eligibility_no, long al_individual_no, long al_claim_no, datetime adtm_annuity_start_date, datetime adtm_annuity_end_date, boolean ab_eligible, long al_checklist_no, string as_checklist_type_code, ref u_checklist_datawindow adw_dw, boolean ab_warning, ref boolean ab_prevent_warning, ref string as_auto_calculation_step_status, ref boolean ab_commit);BOOLEAN        lb_auto_not_require_calc_step
DWItemStatus   ldwis
DWObject       l_dwo
DATETIME       ldtm_null
INTEGER        li_eligibility_find, li_rtn, li_find
LONG           ll_rtn
STRING         ls_status_assigned_method_code


/*

This function is used:
(1) to determine if the Calculate Annuity checklist step (009) should be automatically completed, either
because the calculation of annuities is not allowed or is not required. If the step should be automatically 
completed, then the completion of the step is triggered.
(2) to prevent the post-confirmation warning window from being displayed if the calculation of annuities will 
ultimately not be allowed or not be required. This deters users from using the information on this window to 
pay out the annuity account with amounts that are not likely to be correct.

*/

setNull(ldtm_null)

// if this function call is associated with a warning, do not look for a/e dates on the current eligibility tabpage
IF ab_warning THEN
	IF ab_eligible THEN
		// use the annuity_eligibility_no & start/end date passed in as arguments
	ELSE
		li_rtn = wf_get_last_confirmed_eligibility(al_annuity_eligibility_no,al_annuity_account_no)
		IF li_rtn <= 0 THEN
			SetRedraw(TRUE)
			Error.Text       = 'Could not find last confirmed ineligible annuity_eligiblity_no for an injured worker.'
			Error.WindowMenu = 'cmwb'
			Error.Object     = 'w_confirm_annuity_eligibility'
			SignalError()
			RETURN
		END IF
	END IF
ELSE
	li_eligibility_find = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.uf_find_row('annuity_eligibility_status_code','"A"')

	IF li_eligibility_find > 0 THEN
		// eligible
		ab_eligible = TRUE
		al_annuity_eligibility_no = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.GetItemNumber(li_eligibility_find,'annuity_eligibility_no')
		adtm_annuity_start_date   = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.GetItemDateTime(li_eligibility_find,'annuity_start_date')
		adtm_annuity_end_date     = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.GetItemDateTime(li_eligibility_find,'annuity_end_date')
	ELSE
		// not eligible
		ab_eligible = FALSE
		li_rtn = wf_get_last_confirmed_eligibility(al_annuity_eligibility_no,al_annuity_account_no)
		IF li_rtn <= 0 THEN
			SetRedraw(TRUE)
			Error.Text       = 'Could not find last confirmed ineligible annuity_eligiblity_no for an injured worker.'
			Error.WindowMenu = 'cmwb'
			Error.Object     = 'w_confirm_annuity_eligibility'
			SignalError()
			RETURN
		END IF
	END IF
END IF

IF ab_eligible THEN
	IF as_claim_role_code = 'C' THEN
		lb_auto_not_require_calc_step = wf_auto_not_require_calc_step(is_claim_role_code,al_annuity_account_no,al_annuity_eligibility_no, &
																						  al_individual_no,al_claim_no,adtm_annuity_start_date,adtm_annuity_end_date,ab_eligible)
	END IF
ELSE
	lb_auto_not_require_calc_step = wf_auto_not_require_calc_step(is_claim_role_code,al_annuity_account_no,al_annuity_eligibility_no, &
																					  al_individual_no,al_claim_no,adtm_annuity_start_date,adtm_annuity_end_date,ab_eligible)
END IF



IF lb_auto_not_require_calc_step = TRUE THEN
	// if this function has been called to determine whether the 'confirm a/e' warning should be displayed,
	// then prevent warning, or trigger completion of other steps
	IF ab_warning THEN
		ab_prevent_warning = TRUE
	END IF
	
	// the annuity calculation step is not required.
	li_find = adw_dw.uf_find_row('checklist_step_type_code', '"009"')
	IF li_find > 0 THEN		
		as_auto_calculation_step_status = 'NRA'
		RETURN
	ELSE
		wf_reject_checklist_status_change(TRUE,'Missing Step','Missing step 009 - call helpdesk.',al_checklist_no,as_checklist_type_code)
		RETURN
	END IF
	
ELSE
	// do not run 'not allow' function unless 'not required' function has returned FALSE
	
	//determine if auto-completion of annuity calculation checklist step is allowed, or not allowed
	ib_auto_not_allow_calc_step = wf_auto_not_allow_calc_step(is_claim_role_code,al_annuity_account_no,al_annuity_eligibility_no, &
																						  al_individual_no,al_claim_no,adtm_annuity_start_date,adtm_annuity_end_date,ab_eligible)
	IF ib_auto_not_allow_calc_step THEN
		IF ab_warning THEN
			ab_prevent_warning = TRUE
		END IF
	
		// the annuity calculation step is not allowed.
		li_find = adw_dw.uf_find_row('checklist_step_type_code', '"009"')
		IF li_find > 0 THEN
			as_auto_calculation_step_status = 'NAA'
			RETURN
		ELSE
			wf_reject_checklist_status_change(TRUE,'Missing Step','Missing step 009 - call helpdesk.',al_checklist_no,as_checklist_type_code)
			RETURN
		END IF
	ELSE
		ab_commit = TRUE
		RETURN
	END IF					
END IF
end subroutine

protected subroutine wf_enable_annuity_inquiry_btn ();DATETIME       ldtm_confirmed_date
LONG           ll_checklist_no,ll_annuity_eligibility_no
U_DATAWINDOW   ldw_current_list_dw

//******************************************************************************************************************************
// in order to enable the Annuity Benefit Inquiry btn, the annuity eligibility must be confirmed

ldw_current_list_dw     = Create u_datawindow

wf_set_eligibility_datawindow(ldw_current_list_dw)

IF ldw_current_list_dw.GetRow() > 0 THEN
	
	ll_checklist_no = ldw_current_list_dw.GetItemNumber(ldw_current_list_dw.GetRow(),'checklist_no')
	
	SELECT annuity_eligibility_no,
			 confirmed_date
	INTO   :ll_annuity_eligibility_no,
			 :ldtm_confirmed_date
	FROM   ANNUITY_ELIGIBILITY
	WHERE  confirm_annuity_eligibility_checklist_no = :ll_checklist_no
	USING SQLCA;
	SQLCA.nf_handle_error('w_confirm_annuity_eligibility','SELECT annuity_eligibility_no FROM ANNUITY_ELIGIBILITY...','wf_enable_annuity_inquiry_btn')
	
	
	IF IsNull(ldtm_confirmed_date) THEN	
		// unconfirmed, so...
		cb_annuity_benefit_inquiry.Enabled = FALSE
	ELSE
		cb_annuity_benefit_inquiry.Enabled = TRUE
	END IF
END IF


end subroutine

public function integer wf_get_last_confirmed_eligibility (ref long al_last_inactive_annuity_eligibility_no, long al_annuity_account_no);INTEGER         li_counter, li_upper, li_last_confirmed_row, li_rtn
DATETIME        ldtm_new_confirmed_date, ldtm_last_confirmed_date, ldtm_max_confirmed_date


/*

returns annuity_eligibility_no for the inactive annuity eligibility that was last confirmed

*/

SELECT Max(confirmed_date)
INTO   :ldtm_max_confirmed_date
FROM   ANNUITY_ELIGIBILITY
WHERE  annuity_eligibility_status_code = 'I'
AND    annuity_account_no = :al_annuity_account_no
USING SQLCA;
SQLCA.nf_handle_error('w_confirm_annuity_eligibility','embedded SQL: SELECT Max(confirmed_date) FROM ANNUITY_ELIGIBILITY', 'wf_get_last_confirmed_eligibility')

IF IsNull(ldtm_max_confirmed_date) THEN
	SELECT Max(annuity_eligibility_no)
	INTO   :al_last_inactive_annuity_eligibility_no
	FROM   ANNUITY_ELIGIBILITY
	WHERE  annuity_eligibility_status_code = 'I'
	AND    annuity_account_no = :al_annuity_account_no
	USING SQLCA;
	SQLCA.nf_handle_error('w_confirm_annuity_eligibility','embedded SQL: SELECT Max(confirmed_date) FROM ANNUITY_ELIGIBILITY', 'wf_get_last_confirmed_eligibility')
END IF


SELECT annuity_eligibility_no
INTO   :al_last_inactive_annuity_eligibility_no
FROM   ANNUITY_ELIGIBILITY
WHERE  confirmed_date = :ldtm_max_confirmed_date
USING SQLCA;
SQLCA.nf_handle_error('w_confirm_annuity_eligibility','embedded SQL: SELECT annuity_eligibility_no FROM ANNUITY_ELIGIBILITY', 'wf_get_last_confirmed_eligibility')

RETURN 1
end function

public function integer wf_get_entitled_data (u_ds ads_reexamine_benefit_entitlement, long al_annuity_account_no, long al_individual_no, long al_claim_no, ref datetime adtm_annuity_start_date, ref datetime adtm_annuity_eligibility_end_date_used, ref datetime adtm_annuity_end_date, ref decimal adec_annuity_set_aside_percent);DATETIME	   ldtm_null, ldtm_benefit_option_date, ldtm_annuity_eligibility_end_date_used
INTEGER     li_rtn, li_opening_no, li_annuity_set_aside_percent_no
LONG			ll_claim_no
STRING		ls_benefit_option_code, ls_message, ls_type, ls_date, ls_benefit_option_desc

// determine annuity start date
// get claim, opening & date needed to determine benefit option
IF is_claim_role_code = 'C' THEN	
	// ls_benefit_option_code = 'N/A'
	
	inv_common_annuity.nf_get_aq_claim(al_individual_no, ll_claim_no, li_opening_no)
	adtm_annuity_start_date = DateTime(Date(ads_reexamine_benefit_entitlement.GetItemDateTime(1,'benefit_entitlement_25th_month')))
	
	IF li_opening_no > 0 THEN
		SELECT	accident_recurrence_date
		INTO		:ldtm_benefit_option_date
		FROM		OPENING
		WHERE	   claim_no = :ll_claim_no
		AND		opening_no = :li_opening_no
		USING SQLCA;
		SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_set_entitled_data', 'SELECT accident_recurrence_date...')
	ELSE
		ls_benefit_option_code = 'N/A'
	END IF
		
	ls_type = 'injured worker'
	ls_date = 'accident recurrence date'
	
ELSEIF is_claim_role_code = 'SS' THEN
	
   SELECT a.death_date 
	INTO   :ldtm_benefit_option_date
	FROM   INDIVIDUAL a 
	JOIN   CLAIM      b ON a.individual_no = b.individual_no
	WHERE  b.claim_no = :al_claim_no
	USING SQLCA;
   SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_set_entitled_data', 'SELECT death_date...')
	
	adtm_annuity_start_date = ads_reexamine_benefit_entitlement.GetItemDateTime(1,'benefit_start_date')
	li_opening_no = ads_reexamine_benefit_entitlement.GetItemNumber(1,'opening_no')
	
	ll_claim_no = al_claim_no
	ls_type = 'surviving spouse'
	ls_date = 'injured worker date of death'
END IF


// get benefit option
IF ls_benefit_option_code = 'N/A' THEN
	// already set for no-opening IWs above
ELSE
	SELECT b.benefit_option_code
	INTO   :ls_benefit_option_code
	FROM   OPENING a
	JOIN   Opening_Type_Benefit_Option_Xref b ON a.opening_type_code = b.opening_type_code
	WHERE  a.claim_no = :ll_claim_no
	AND    a.opening_no = :li_opening_no
	AND    b.effective_from_date <= :ldtm_benefit_option_date
	AND    IsNull(b.effective_to_date,GetDate()) >= :ldtm_benefit_option_date
	USING SQLCA;
	SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_set_entitled_data', 'SELECT benefit_option_code...')
END IF

// If nothing retrieved, then trigger error
IF ls_benefit_option_code = '' THEN
	Error.Text = 'No benefit option retrieved for the '+ls_type+' from the Opening_Type_Benefit_Option_Xref table. ' &
					+'Claim: ' + String(ll_claim_no) &
					+'; Individual: ' + String(al_individual_no) &
					+'; Annuity Account: ' + String(al_annuity_account_no) &
					+'; Opening: ' + String(li_opening_no) &
					+'; ' + ls_date + ': ' + String(ldtm_benefit_option_date,'yyyy-mm-dd')
	Error.WindowMenu="cmwb"
	Error.Object="w_confirm_annuity_eligibility"
	SignalError()
ELSE
	SELECT	benefit_option_desc
	INTO		:ls_benefit_option_desc
	FROM		Benefit_Option
	WHERE	   benefit_option_code = :ls_benefit_option_code
	USING SQLCA;
	SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'wf_set_entitled_data', 'SELECT benefit_option_desc')
END IF


// get annuity end date
SetNull(ldtm_null)
li_rtn = inv_common_annuity.nf_get_annuity_end_date(al_individual_no,ls_type,ls_message,adtm_annuity_end_date,adtm_annuity_eligibility_end_date_used,ldtm_null,ldtm_null)
IF li_rtn < 0 THEN
	MessageBox('Annuity Problem',ls_message,Exclamation!)
	RETURN li_rtn
END IF

// get the annuity pct for the role code, annuity end date, & benefit option
inv_common_annuity.nf_get_annuity_percentage(is_claim_role_code, adtm_annuity_end_date, ls_benefit_option_code, adec_annuity_set_aside_percent , li_annuity_set_aside_percent_no)

// If nothing retrieved, then trigger error
IF adec_annuity_set_aside_percent = 0.00 OR li_annuity_set_aside_percent_no = 0 THEN
	Error.Text = 'No percentage retrieved for the '+ls_type+' from the Annuity_Set_Aside_Percent table.' &
					+'Claim: ' + String(ll_claim_no) &
					+'; Individual: ' + String(al_individual_no) &
					+'; Annuity Account: ' + String(al_annuity_account_no) &
					+'; Benefit Option: ' + ls_benefit_option_desc
	Error.WindowMenu="cmwb"
	Error.Object="w_confirm_annuity_eligibility"
	SignalError()
END IF

RETURN 0

end function

on w_confirm_annuity_eligibility.create
int iCurrent
call super::create
if IsValid(this.MenuID) then destroy(this.MenuID)
if this.MenuName = "m_annuity" then this.MenuID = create m_annuity
this.cb_annuity_benefit_inquiry=create cb_annuity_benefit_inquiry
this.st_last_generated=create st_last_generated
this.st_last_generated_dtm=create st_last_generated_dtm
this.cb_generate_list=create cb_generate_list
this.st_run_option=create st_run_option
this.dw_cae_tombstone=create dw_cae_tombstone
this.st_multiple_annuity_accounts=create st_multiple_annuity_accounts
this.dw_annuity_eligibility_run_option=create dw_annuity_eligibility_run_option
this.tab_confirm_eligibility=create tab_confirm_eligibility
this.tab_eligibility=create tab_eligibility
this.uo_checklist=create uo_checklist
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_annuity_benefit_inquiry
this.Control[iCurrent+2]=this.st_last_generated
this.Control[iCurrent+3]=this.st_last_generated_dtm
this.Control[iCurrent+4]=this.cb_generate_list
this.Control[iCurrent+5]=this.st_run_option
this.Control[iCurrent+6]=this.dw_cae_tombstone
this.Control[iCurrent+7]=this.st_multiple_annuity_accounts
this.Control[iCurrent+8]=this.dw_annuity_eligibility_run_option
this.Control[iCurrent+9]=this.tab_confirm_eligibility
this.Control[iCurrent+10]=this.tab_eligibility
this.Control[iCurrent+11]=this.uo_checklist
end on

on w_confirm_annuity_eligibility.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_annuity_benefit_inquiry)
destroy(this.st_last_generated)
destroy(this.st_last_generated_dtm)
destroy(this.cb_generate_list)
destroy(this.st_run_option)
destroy(this.dw_cae_tombstone)
destroy(this.st_multiple_annuity_accounts)
destroy(this.dw_annuity_eligibility_run_option)
destroy(this.tab_confirm_eligibility)
destroy(this.tab_eligibility)
destroy(this.uo_checklist)
end on

event open;DataWindowChild                ldwc_run_option
INTEGER                        li_rtn, li_upperbound, li_message, li_incomplete_checklist_count, li_count, li_row, li_trancount
LONG                           ll_rows, ll_annuity_account_no, ll_claim_no, ll_active_eligibility_no, ll_checklist_no
STRING                         ls_annuity_eligibility_run_option_desc, ls_checklist_type_code, ls_checklist_type_list[]
STRING                         ls_checklist_flag, ls_surviving_spouse_message, ls_run_date_type, ls_message
U_DS                           lds_incomplete_checklists
u_datawindow   	             ldw_eligibility
s_window_message               lstr_message
w_confirm_annuity_eligibility	 lw_win


lstr_message = Message.PowerObjectParm

iul_handle = Handle(THIS)

// sets up to close this window if frame is closed

li_upperbound = UpperBound(gstr_window_array) + 1
gstr_window_array[li_upperbound].window_element = THIS
gstr_window_array[li_upperbound].handle_element = iul_handle

lw_win = this

ls_surviving_spouse_message = lstr_message.as_stringParm[1]
is_list_mode = lstr_message.as_stringParm[2]

il_claim_no = lstr_message.al_doubleParm[1]
il_individual_no = lstr_message.al_doubleParm[2]
il_annuity_payout_no = lstr_message.al_doubleParm[3]

IF il_annuity_payout_no > 0 THEN
	ids_annuity_payout = Create U_DS
	ids_annuity_payout.DataObject = 'ds_annuity_payout'
	ids_annuity_payout.SetTransObject(SQLCA)
	ll_rows = ids_annuity_payout.Retrieve(il_annuity_payout_no)
	SQLCA.nf_handle_error('w_confirm_annuity_eligibility','ids_annuity_payout','retrieve')
	IF ll_rows <> 1 THEN
		// should only retrieve one row
		MessageBox('Payout Error','Error - Should be a single annuity payout record retrieved. Actually retrieved '+String(ll_rows)+'. Call HELPDESK.',Exclamation!)
		THIS.WindowState = Maximized!
		RETURN		
	END IF
END IF




//	APPLICATION SECURITY CODE
//
G_PFSecurity.UOF_Check_Access(This)
I_Authorized_Access = True              //declared as an instance variable



uo_checklist.uf_set_parent_window(lw_win)

uo_checklist.uf_get_checklist_nvo(inv_checklist)

uo_checklist.itr_trans_object = SQLCA	
inv_checklist.nf_set_datawindow(uo_checklist.idw_dw[],SQLCA)
inv_checklist.nf_set_checklist_object(uo_checklist)
inv_checklist.nf_set_checklist_subscriber_type('ANN')

inv_common_annuity = Create n_common_annuity
inv_common_annuity.nf_set_window_parent(THIS)


IF IsNull(il_claim_no) THEN il_claim_no = 0
IF IsNull(il_individual_no) THEN il_individual_no = 0

// if il_claim_no & il_individual_no are zero and admin region is not PRV, then display all injured workers 
dw_annuity_eligibility_run_option.InsertRow(0)

IF ls_surviving_spouse_message = 'IW' OR ls_surviving_spouse_message = 'NA' THEN
	// user chose to look at injured worker if surviving spouse existed, OR there was no surviving spouse
	// or chose to generate (potentially) for all surviving spouses
	is_annuity_eligibility_run_option_code = 'IW'
	is_claim_role_code = 'C'
	ls_run_date_type = 'IA'
	ls_message = 'injured worker'
	ls_checklist_type_list[1] = "CAEIN"
	ls_checklist_type_list[2] = "CAEIC"
	ls_checklist_type_list[3] = "CAEP"
	wf_select_ae_run_option()
ELSE
	// surviving spouse for specific claim
	// user chose to look at surviving spouse		
	is_annuity_eligibility_run_option_code = 'SS'
	is_claim_role_code = 'SS'
	ls_run_date_type = 'SA'
	ls_message = 'surviving spouse'
	ls_checklist_type_list[1] = "CAESN"
	ls_checklist_type_list[2] = "CAESC"
	ls_checklist_type_list[3] = "CAEP"
	wf_select_ae_run_option()
END IF

// does the individual have an incomplete checklist?
IF il_individual_no > 0 THEN
	lds_incomplete_checklists = CREATE U_DS
	lds_incomplete_checklists.DataObject = 'ds_incomplete_checklists'
	lds_incomplete_checklists.SetTransObject(SQLCA)
	IF is_claim_role_code = 'C' THEN
		ll_claim_no = 0
	ELSE
		ll_claim_no = il_claim_no
	END IF
	li_incomplete_checklist_count = lds_incomplete_checklists.Retrieve(ls_checklist_type_list,il_individual_no,ll_claim_no,is_claim_role_code)
	SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'open', 'retrieve ds_incomplete_checklists')
END IF


// do not open window for payout purposes directly
// i.e., through the CAE window menu item - you should open it through
// the PAA window & PAA checklist.
IF is_claim_role_code = 'C' THEN
	SELECT Count(*)
	INTO   :li_count
	FROM   CHECKLIST           a
	JOIN   ANNUITY_ELIGIBILITY b ON a.checklist_no = b.confirm_annuity_eligibility_checklist_no
	JOIN   ANNUITY_ACCOUNT     c ON b.annuity_account_no = c.annuity_account_no
	WHERE  c.individual_no         = :il_individual_no
	AND    c.claim_no              = :ll_claim_no
	AND    a.checklist_type_code IN ('PAA','CAPLS','CAPAP')
	AND    a.checklist_status_code = 'IA'
	USING SQLCA;
	SQLCA.nf_handle_error('w_confirm_annuity_eligibility','embedded SQL: SELECT Count(*) FROM CHECKLIST a, ANNUITY_ELIGIBILITY b, ANNUITY_ACCOUNT c...', 'open event (IW)')
ELSE
	SELECT Count(*)
	INTO   :li_count
	FROM   CHECKLIST           a
	JOIN   ANNUITY_ELIGIBILITY b ON a.checklist_no = b.confirm_annuity_eligibility_checklist_no
	JOIN   ANNUITY_ACCOUNT     c ON b.annuity_account_no = c.annuity_account_no
	WHERE  c.individual_no         = :il_individual_no
	AND    c.claim_no              = :ll_claim_no
	AND    a.checklist_type_code IN ('PAA','CAPLS','CAPAP')
	AND    a.checklist_status_code = 'IA'
	USING SQLCA;
	SQLCA.nf_handle_error('w_confirm_annuity_eligibility','embedded SQL: SELECT Count(*) FROM CHECKLIST a, ANNUITY_ELIGIBILITY b, ANNUITY_ACCOUNT c...', 'open event (SS)')
END IF

IF li_count > 0 THEN
	MessageBox('Incorrect Module','This individual has an incomplete Payout checklist. Please contact the person responsible for paying out the annuity for this individual.',Exclamation!)
	RETURN
END IF


IF is_list_mode = 'payout' THEN
	// opened from payout module
	wf_setup_payout_controls()
	
	IF li_incomplete_checklist_count > 0 THEN
		// already have incomplete checklist, so retrieve it
		IF is_claim_role_code = 'C' THEN
			ll_rows = wf_retrieve_injured_worker(il_individual_no)
		ELSE
			ll_rows = wf_retrieve_surviving_spouse(il_claim_no,il_individual_no)
		END IF
	ELSE
		// insert records from individual from payout module
		SELECT annuity_account_no
		INTO   :ll_annuity_account_no
		FROM   ANNUITY_ACCOUNT
		WHERE  individual_no   = :il_individual_no
		AND    claim_no        = :il_claim_no
		AND    claim_role_code = :is_claim_role_code
		USING SQLCA;
		SQLCA.nf_handle_error("w_confirm_annuity_eligibility", "Embedded SQL: SELECT annuity_account_no FROM ANNUITY_ACCOUNT...", "open")
		
		SQLCA.nf_begin_transaction()
		
		li_rtn = wf_populate_checklist_and_annuity(il_individual_no,0,ll_annuity_account_no,'CAEP')
		IF li_rtn < 0 THEN
			SQLCA.nf_transaction_count(li_trancount,0,this.classname(),'','',FALSE)
			IF li_trancount > 0 THEN
				SQLCA.nf_rollback_transaction()
			END IF
		END IF

		SQLCA.nf_commit_transaction()
		
		IF is_claim_role_code = 'C' THEN
			ll_rows = wf_retrieve_injured_worker(il_individual_no)
		ELSE
			ll_rows = wf_retrieve_surviving_spouse(il_claim_no,il_individual_no)
		END IF
	END IF

	
	// enable/disable annuity inquiry btn as needed
	wf_set_eligibility_datawindow(ldw_eligibility)	
	li_row = ldw_eligibility.getrow()	
	IF li_row > 0 THEN
		ll_checklist_no = ldw_eligibility.GetItemNumber(li_row,'checklist_no')
		ll_annuity_account_no = ldw_eligibility.GetItemNumber(li_row,'annuity_account_no')
		wf_enable_annuity_inquiry_btn()
	END IF
	
	
ELSEIF is_list_mode = 'menu' THEN
	// opened from menu
	IF il_claim_no <> 0 OR il_individual_no <> 0 THEN
		IF li_incomplete_checklist_count > 0 THEN
			// already have incomplete checklist, so retrieve it
			IF is_claim_role_code = 'C' THEN
				ll_rows = wf_retrieve_injured_worker(il_individual_no)
			ELSE				
				ll_rows = wf_retrieve_surviving_spouse(il_claim_no,il_individual_no)
			END IF
			
		ELSE
			// No incomplete checklists
			li_message = MessageBox('Generate List?','A checklist for this '+ls_message+' will be generated.'&
																+'~r~n~r~nDo you wish to continue?',Question!,YesNo!,2)
			IF li_message = 1 THEN

				IF is_claim_role_code = 'C' THEN
					tab_confirm_eligibility.tabpage_payout.Visible = FALSE
					ll_rows = wf_generate_injured_worker()
					IF ll_rows < 0 THEN
						tab_confirm_eligibility.tabpage_new.Enabled = TRUE
						tab_confirm_eligibility.tabpage_changed.Enabled = TRUE
						wf_resize_module()
						THIS.WindowState = Maximized!
						RETURN
					END IF
					
					IF ib_PAA_individual THEN
						ib_PAA_individual = FALSE
					END IF
					
					ll_rows = wf_retrieve_injured_worker(il_individual_no)
				ELSE
					tab_confirm_eligibility.tabpage_payout.Visible = FALSE
					ll_rows = wf_generate_surviving_spouse()
					IF ll_rows < 0 THEN
						tab_confirm_eligibility.tabpage_new.Enabled = TRUE
						tab_confirm_eligibility.tabpage_changed.Enabled = TRUE
						wf_resize_module()
						THIS.WindowState = Maximized!
						RETURN
					END IF
					
					IF ib_PAA_individual THEN
						ib_PAA_individual = FALSE
					END IF
					
					ll_rows = wf_retrieve_surviving_spouse(il_claim_no,il_individual_no)
				END IF
			
				IF ll_rows = 0 THEN
					// not currently eligible
					MessageBox('None Currently Eligible','This '+ls_message+' is not potentially eligible for an annuity.',Exclamation!)
					THIS.WindowState = Maximized!
					RETURN
				END IF
								
			ELSE
				// chose not to generate
				
			END IF
			
			// enable/disable annuity inquiry btn as needed
			wf_set_eligibility_datawindow(ldw_eligibility)	
			li_row = ldw_eligibility.getrow()	
			IF li_row > 0 THEN
				ll_checklist_no = ldw_eligibility.GetItemNumber(li_row,'checklist_no')
				ll_annuity_account_no = ldw_eligibility.GetItemNumber(li_row,'annuity_account_no')
				wf_enable_annuity_inquiry_btn()
			END IF
		END IF		
		
	ELSE
		// no claim number, no individual number indicates that the user opened the window
		// directly without searching for a claim (and therefore a claimant), so filter the run options
		// if the user wants to generate the list, then an option will have to be chosen
		dw_annuity_eligibility_run_option.Reset()
		dw_annuity_eligibility_run_option.InsertRow(0)
		
		dw_annuity_eligibility_run_option.GetChild('annuity_eligibility_run_option_code',ldwc_run_option)
		ldwc_run_option.SetTransObject(SQLCA)
		ldwc_run_option.Retrieve(vgst_user_profile.default_admin_region_code)
		SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'open', 'ldwc_run_option.Retrieve')
		ldwc_run_option.SetFilter('annuity_eligibility_run_option_code <> "IW" AND annuity_eligibility_run_option_code <> "SS"')
		ldwc_run_option.Filter()
	END IF
	
	tab_confirm_eligibility.tabpage_new.Visible = TRUE
	tab_confirm_eligibility.tabpage_changed.Visible = TRUE
	tab_confirm_eligibility.tabpage_payout.Visible = FALSE
	
	st_run_option.Visible = TRUE
	dw_annuity_eligibility_run_option.Visible = TRUE
	
ELSE
	MessageBox('No list mode','Error - no list mode was set up. Call the HELPDESK.',Exclamation!)
	THIS.WindowState = Maximized!
	RETURN
END IF

wf_resize_module()

//make the checklist move
//uo_checklist.visible = TRUE

dw_annuity_eligibility_run_option.post SetFocus()

// make report menu item not visible
inv_common_annuity.nf_make_menu_item_invisible(lw_win,'m_file','m_report')
end event

event close;inv_common_annuity.nf_close_handle_array(iul_handle)

end event

event closequery;INTEGER li_counter, li_count, li_rowcount, li_message
DWITEMSTATUS     	ldwis_rowstatus

tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.AcceptText()

li_rowcount = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.RowCount()

li_count = 0
FOR li_counter = 1 TO li_rowcount
	//CHECK THE ROW STATUS
	ldwis_rowstatus = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.GetItemStatus(li_counter,0,Primary!)
	IF ldwis_rowstatus <> NotModified! THEN
		li_count ++
	END IF 	
NEXT		
 
IF li_count > 0  THEN 
	li_message = MessageBox('Pending Changes', 'There are unsaved changes. Do you want to save them before you close?',Exclamation!,YesNo!,1)
	IF li_message = 1 THEN
		RETURN 1
	END IF
END IF 	
end event

event resize;call super::resize;function post wf_post_resize_checklist_position()
ib_checklist_posted = TRUE

end event

type cb_annuity_benefit_inquiry from commandbutton within w_confirm_annuity_eligibility
integer x = 3963
integer y = 1852
integer width = 631
integer height = 104
integer taborder = 70
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Annuity Benefit Inquiry"
end type

event clicked;
BOOLEAN            lb_warning, lb_eligible, lb_display_warning_messagebox, lb_commit
DATETIME           ldtm_new_annuity_start_date,ldtm_new_annuity_end_date, ldtm_annuity_eligibility_end_date_used
DECIMAL            ldec_annuity_set_aside_percent
INTEGER            li_annuity_calc_account_header_count
LONG               ll_claim_no, ll_individual_no, ll_annuity_account_no, ll_annuity_eligibility_no, ll_confirm_eligibility_row, ll_checklist_no
STRING             ls_checklist_step_status_code, ls_checklist_type_code, ls_auto_calculation_step_status
u_datawindow       ldw_eligibility
u_checklist_datawindow ldw_dw
s_verify_warning   lstr_verify_warning

ldw_eligibility = Create u_datawindow

wf_set_eligibility_datawindow(ldw_eligibility)
ll_confirm_eligibility_row = ldw_eligibility.GetRow()

IF ll_confirm_eligibility_row > 0 THEN
	
	ll_checklist_no = ldw_eligibility.GetItemNumber(ll_confirm_eligibility_row,'checklist_no')
	ll_annuity_account_no = ldw_eligibility.GetItemNumber(ll_confirm_eligibility_row,'annuity_account_no')
	ll_claim_no = ldw_eligibility.GetItemNumber(ll_confirm_eligibility_row,'claim_no')
	ll_individual_no = ldw_eligibility.GetItemNumber(ll_confirm_eligibility_row,'individual_no')
	

	SELECT annuity_eligibility_no,
	       annuity_start_date,
			 annuity_eligibility_end_date_used,
			 annuity_end_date,
			 annuity_set_aside_percent
	INTO   :ll_annuity_eligibility_no,
          :ldtm_new_annuity_start_date,
			 :ldtm_annuity_eligibility_end_date_used,
			 :ldtm_new_annuity_end_date,
			 :ldec_annuity_set_aside_percent
	FROM   ANNUITY_ELIGIBILITY
	WHERE  confirm_annuity_eligibility_checklist_no = :ll_checklist_no
	USING SQLCA;
	SQLCA.nf_handle_error('w_confirm_annuity_eligibility','embedded SQL: SELECT annuity_eligibility_no FROM ANNUITY_ELIGIBILITY...','cb_annuity_benefit_inquiry.clicked')
	
	IF IsNull(ldtm_new_annuity_start_date) THEN
		lb_eligible = FALSE
	ELSE
		lb_eligible = TRUE
	END IF
	
	// the intent is to prevent a warning, not to possibly trigger the completion of the Calculate Annuity checklist step
	lb_warning = TRUE
	
	wf_enable_annuity_inquiry_btn()
	IF cb_annuity_benefit_inquiry.Enabled THEN
		// IF the button is enabled, then determine the status of the Calculate Annuity checklist step
		SELECT IsNull(checklist_step_status_code,'')
		INTO   :ls_checklist_step_status_code
		FROM   CHECKLIST_STEP
		WHERE  checklist_no = :ll_checklist_no
		AND    checklist_step_type_code = '009'
		USING SQLCA;
		SQLCA.nf_handle_error('w_confirm_annuity_eligibility','embedded SQL: SELECT checklist_step_status_code FROM CHECKLIST_STEP...','cb_annuity_benefit_inquiry.clicked')
		
		IF ls_checklist_step_status_code <> 'INA' THEN
			// the Calculate Annuity checklist step has been concluded, so...
			// DO NOT display a popup window with the potential annuity account calculation data
			// display a messagebox instead
			IF     ls_checklist_step_status_code = 'NAA' THEN
				MessageBox('Annuity Benefit Inquiry Warning','The Calculate Annuity checklist step was concluded as "Not Allowed", so there are no annuity calculation details to view.',Exclamation!)
			ELSEIF ls_checklist_step_status_code = 'NRA' THEN
				MessageBox('Annuity Benefit Inquiry Warning','The Calculate Annuity checklist step was concluded as "Not Required", so there are no annuity calculation details to view.',Exclamation!)
			ELSEIF ls_checklist_step_status_code = 'COM' THEN
				MessageBox('Annuity Benefit Inquiry Warning','The Calculate Annuity checklist step was concluded as "Completed". If you wish to view the details of the calculation, open the Calculate Annuity Inquiry module for this annuity account.',Exclamation!)
			END IF
			RETURN
		ELSE
			// the Calculate Annuity checklist step has NOT been concluded, so...
			// determine if there has been a calculation created
			SELECT COUNT(*)
			INTO   :li_annuity_calc_account_header_count
			FROM   ANNUITY_CALC_ACCOUNT_HEADER
			WHERE  annuity_eligibility_no = :ll_annuity_eligibility_no
			USING SQLCA;
			SQLCA.nf_handle_error('w_confirm_annuity_eligibility','embedded SQL: SELECT COUNT(*) FROM ANNUITY_CALC_ACCOUNT_HEADER...','cb_annuity_benefit_inquiry.clicked')
			
			IF li_annuity_calc_account_header_count = 0 THEN
				// no annuity calculated so...
				
				ldw_dw = uo_checklist.tab_checklist.tabpage_checklist.dw_checklist
				ls_checklist_type_code = ldw_dw.GetItemString(ldw_dw.GetRow(),'checklist_type_code')

				// determine if the annuity calculation checklist step should be automatically completed
				// if it should, then DO NOT display the 'VBE warning'. This deters users from using an
				// 'annuity calculation' that will never be made as the basis of a manual (and erroneous) annuity payout
				wf_determine_calc_step_auto_completion(is_claim_role_code,ll_annuity_account_no,ll_annuity_eligibility_no,ll_individual_no,ll_claim_no, &
							                     ldtm_new_annuity_start_date,ldtm_new_annuity_end_date,lb_eligible,ll_checklist_no,ls_checklist_type_code, &
														ldw_dw,lb_warning, lb_display_warning_messagebox,ls_auto_calculation_step_status,lb_commit)
					
				IF lb_display_warning_messagebox = TRUE THEN
					// NRA or NAA - not required or not allowed, so display messagebox
					IF ls_auto_calculation_step_status = 'NAA' THEN
						MessageBox('Annuity Benefit Inquiry Warning','The Calculate Annuity checklist step will be concluded as "Not Allowed", so there will be no annuity calculation details to view.',Exclamation!)
					ELSEIF ls_auto_calculation_step_status = 'NRA' THEN
						MessageBox('Annuity Benefit Inquiry Warning','The Calculate Annuity checklist step will be concluded as "Not Required", so there will be annuity calculation details to view.',Exclamation!)
					END IF
					RETURN
				END IF
			
				//do not return out of event, allow data to be displayed in popup
				// the Calculate Annuity has not been concluded, so...
				// display a popup window with the potential annuity account calculation data
			
				// popup pre-calculation summary to warn users
				lstr_verify_warning.annuity_account_no = ll_annuity_account_no
				lstr_verify_warning.annuity_eligibility_no = ll_annuity_eligibility_no
				lstr_verify_warning.annuity_payout_no      = il_annuity_payout_no
				lstr_verify_warning.claim_no = ll_claim_no
				lstr_verify_warning.individual_no = ll_individual_no
				lstr_verify_warning.claim_role_code = is_claim_role_code
				lstr_verify_warning.annuity_start_date = ldtm_new_annuity_start_date
				lstr_verify_warning.annuity_eligibility_end_date_used = ldtm_annuity_eligibility_end_date_used
				lstr_verify_warning.annuity_end_date = ldtm_new_annuity_end_date
				lstr_verify_warning.annuity_percentage = ldec_annuity_set_aside_percent
				lstr_verify_warning.window_mode = 'inquiry'
				
				
				// if the a/e start date is after the a/e end date, then use null dates
				// because the confirmation process will ultimately set the a/e dates to null
				IF (ldtm_new_annuity_start_date > ldtm_new_annuity_end_date) THEN
					SetNull(lstr_verify_warning.annuity_start_date)
					SetNull(lstr_verify_warning.annuity_end_date)
				END IF
				
				
				// open popup to warn users to complete the individual's benefit entitlement
				OpenWithParm(w_verify_step_warning,lstr_verify_warning)
				
				
			ELSE
				// annuity calculated so...
				MessageBox('Annuity Benefit Inquiry Warning','An Annuity Calculation has been completed. If you wish to view the details of the calculation, click the button beside the Calculate Annuity checklist step.',Exclamation!)
				RETURN
			END IF

		END IF
	ELSE
		// can this happen? i.e., can you click the button, have the function disable the button?
		// Probably not, because the function is run on the open of the module, & on each RFC of the 'list' datawindow.
		RETURN
	END IF

	
END IF
end event

type st_last_generated from statictext within w_confirm_annuity_eligibility
boolean visible = false
integer x = 4247
integer width = 425
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
string text = "Last Generated:"
boolean focusrectangle = false
end type

type st_last_generated_dtm from statictext within w_confirm_annuity_eligibility
boolean visible = false
integer x = 4247
integer y = 68
integer width = 631
integer height = 60
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
string text = "2009-10-31 11:59:59 pm"
boolean focusrectangle = false
end type

type cb_generate_list from commandbutton within w_confirm_annuity_eligibility
boolean visible = false
integer x = 3776
integer y = 8
integer width = 443
integer height = 104
integer taborder = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Generate List"
end type

event clicked;DATETIME		ldtm_last_run
INTEGER		li_message, li_rtn, li_count, li_trancount
LONG			ll_rows
N_PROCESS_RUN_STATUS ln_process_run_status


CHOOSE CASE is_annuity_eligibility_run_option_code
	CASE 'IA'
		
		
		li_message = MessageBox('Generate New List?','Generating this injured worker list will take several minutes.'&
		                                            +'~r~n'&
		                                            +'Once the process is started, you must allow it to complete.'&
		                                            +'~r~n'&
																  +'Do not terminate the process until it is finished.'&
		                                            +'~r~n'&
		                                            +'~r~n'&
		                                            +'Do you want to generate a new list of all potentially eligible injured workers?',Question!,OKCancel!,2)
						
		IF li_message = 1 THEN
			
			/******************************************************************************************
			- function call prevents generating the potential annuity eligibility list while
			- someone else has already started generating the list
			- N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
			- '048' refers to the Confirm Annuity Eligibility module
			******************************************************************************************/
			ln_process_run_status = Create N_PROCESS_RUN_STATUS
			li_rtn = ln_process_run_status.nf_in_progress(SQLCA,'048','Confirm Annuity Eligibility - All Injured Worker Potential List Generation - In Progress',&
																+'Generation of the Potential Lists cannot be done at this time. They are currently In Progress.~r~n'&
																+'~r~n'&
																+'You may not need to generate the lists again so soon. But, if required, please try again in a few minutes.')
			
			IF li_rtn = 1 THEN
				// module is blocked
				return
			END IF
			
			/******************************************************************************************/
			
			li_count = wf_get_AE_run_option_count(is_annuity_eligibility_run_option_code)
			IF li_count > 0 THEN
				MessageBox('Outstanding Error','The last time the list was generated for the "All Injured Workers" option, there was an error.'&
													+'~r~nThis error has not yet been resolved, so the lists cannot be generated.'&
													+'~r~nYou can continue to work on the injured worker potential lists.'&
													+'~r~nPlease contact the HELPDESK.',Exclamation!)
			ELSE
				SetPointer(HourGlass!)
				SetRedraw(FALSE)
				ll_rows = wf_generate_all_injured_workers()
				
				IF ll_rows < 0 THEN
					SQLCA.nf_transaction_count(li_trancount,0,this.classname(),'','',FALSE)
					IF li_trancount > 0 THEN
						SQLCA.nf_rollback_transaction()
					END IF

					SetRedraw(TRUE)
					SetPointer(Arrow!)
					RETURN
				ELSE
					ll_rows = wf_retrieve_all_injured_workers()
					SetRedraw(TRUE)
					SetPointer(Arrow!)
				END IF
			END IF
		ELSE
			// Did not generate list
		END IF
	CASE 'SA'
		ldtm_last_run = wf_get_ae_last_run_date('SA')
		
		li_message = MessageBox('Generate New List?','Once the surviving spouse process is started, you must allow it to complete.'&
		                                            +'~r~n'&
																  +'Do not terminate the process until it is finished.'&
		                                            +'~r~n'&
		                                            +'~r~n'&
																  +'Do you want to generate a new list of all potentially eligible surviving spouses?',Question!,OKCancel!,2)
		
		SetRedraw(FALSE)
		IF li_message = 1 THEN
						
			/******************************************************************************************
			- function call prevents generating the potential annuity eligibility list while
			- someone else has already started generating the list
			- N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
			- '048' refers to the Confirm Annuity Eligibility module
			******************************************************************************************/
			ln_process_run_status = Create N_PROCESS_RUN_STATUS
			li_rtn = ln_process_run_status.nf_in_progress(SQLCA,'048','Confirm Annuity Eligibility - All Surviving Spouse Potential List Generation - In Progress',&
																+'Generation of the Potential Lists cannot be done at this time. They are currently In Progress.~r~n'&
																+'~r~n'&
																+'You may not need to generate the lists again so soon. But, if required, please try again in a few minutes.')
			
			IF li_rtn = 1 THEN
				// module is blocked
				return
			END IF
			
			/******************************************************************************************/
			
			li_count = wf_get_AE_run_option_count(is_annuity_eligibility_run_option_code)
			IF li_count > 0 THEN
				MessageBox('Outstanding Error','The last time the list was generated for the "All Surviving Spouses" option, there was an error.'&
													+'~r~nThis error has not yet been resolved, so the lists cannot be generated.'&
													+'~r~nYou can continue to work on the surviving spouse potential lists.'&
													+'~r~nPlease contact the HELPDESK.',Exclamation!)
			ELSE
				SetPointer(HourGlass!)
				SetRedraw(FALSE)
				ll_rows = wf_generate_all_surviving_spouses()
				IF ll_rows < 0 THEN
					SQLCA.nf_transaction_count(li_trancount,0,this.classname(),'','',FALSE)
					IF li_trancount > 0 THEN
						SQLCA.nf_rollback_transaction()
					END IF

					SetRedraw(TRUE)
					SetPointer(Arrow!)
					RETURN
				ELSE
					ll_rows = wf_retrieve_all_surviving_spouses()
					SetRedraw(TRUE)
					SetPointer(Arrow!)
				END IF
			END IF			
		ELSE
			// Did not generate list
		END IF
END CHOOSE

ldtm_last_run = wf_get_ae_last_run_date(is_annuity_eligibility_run_option_code)
wf_set_last_generated_datetime(TRUE,ldtm_last_run)
end event

type st_run_option from statictext within w_confirm_annuity_eligibility
integer x = 2523
integer y = 72
integer width = 343
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
string text = "Run Option:"
alignment alignment = right!
boolean focusrectangle = false
end type

type dw_cae_tombstone from u_datawindow within w_confirm_annuity_eligibility
integer x = 23
integer y = 28
integer width = 2432
integer height = 148
integer taborder = 30
string dataobject = "d_CAE_tombstone"
boolean border = false
borderstyle borderstyle = styleraised!
end type

event constructor;call super::constructor;THIS.SetTransObject(SQLCA)
end event

type st_multiple_annuity_accounts from statictext within w_confirm_annuity_eligibility
boolean visible = false
integer x = 1586
integer y = 196
integer width = 1280
integer height = 80
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 255
long backcolor = 67108864
string text = "Multiple annuity accounts exist for this Individual"
boolean focusrectangle = false
end type

type dw_annuity_eligibility_run_option from u_datawindow within w_confirm_annuity_eligibility
integer x = 2871
integer y = 52
integer width = 855
integer height = 100
integer taborder = 50
string dataobject = "d_annuity_eligibility_run_option"
boolean border = false
end type

event itemchanged;call super::itemchanged;DATETIME		ldtm_last_run, ldtm_null
INTEGER		li_row, li_selected_tab
LONG			ll_rows, ll_iw_individual_no, ll_ss_individual_no, ll_ss_claim_no


is_annuity_eligibility_run_option_code = THIS.GetText()

wf_switch_dw_type(is_annuity_eligibility_run_option_code)

li_selected_tab = wf_get_selected_confirm_eligibility_tab()

SetNull(ldtm_null)

CHOOSE CASE is_annuity_eligibility_run_option_code
	CASE 'IA'
		ldtm_last_run = wf_get_ae_last_run_date('IA')
		wf_set_last_generated_datetime(TRUE,ldtm_last_run)
		ll_rows = wf_retrieve_all_injured_workers()
				
		IF ll_rows = 0 THEN
			// not currently eligible
			MessageBox('None Currently Eligible','There are currently no "new" injured workers that are potentially eligible for an annuity.',Exclamation!)
			RETURN 2
		END IF
		
		is_claim_role_code = 'C'
				
	CASE 'SA'
		ldtm_last_run = wf_get_ae_last_run_date('SA')
		wf_set_last_generated_datetime(TRUE,ldtm_last_run)
		ll_rows = wf_retrieve_all_surviving_spouses()
		
		IF ll_rows = 0 THEN
			// not currently eligible
			MessageBox('None Currently Eligible','There are currently no "new" surviving spouses that are potentially eligible for an annuity.',Exclamation!)
			RETURN 2
		END IF
		
		is_claim_role_code = 'SS'
					
	CASE 'IW'
		wf_set_last_generated_datetime(FALSE,ldtm_null)
				
		IF li_selected_tab = NEW_LIST THEN
			li_row = tab_confirm_eligibility.tabpage_new.dw_new_iw_eligibility.GetRow()
			IF li_row > 0 THEN
				ll_iw_individual_no = tab_confirm_eligibility.tabpage_new.dw_new_iw_eligibility.GetItemNumber(li_row,'individual_no')
			END IF
		ELSEIF li_selected_tab = CHANGED_LIST THEN
			li_row = tab_confirm_eligibility.tabpage_changed.dw_changed_iw_eligibility.GetRow()
			IF li_row > 0 THEN
				ll_iw_individual_no = tab_confirm_eligibility.tabpage_changed.dw_changed_iw_eligibility.GetItemNumber(li_row,'individual_no')
			END IF
		END IF
		ll_rows = wf_retrieve_injured_worker(ll_iw_individual_no)
		
		IF ll_rows = 0 THEN
			// not currently eligible
			MessageBox('None Currently Eligible','This injured worker is not potentially eligible for an annuity.',Exclamation!)
			tab_confirm_eligibility.tabpage_new.Enabled = TRUE
			tab_confirm_eligibility.tabpage_changed.Enabled = TRUE
		END IF
		
		is_claim_role_code = 'C'
		
				
	CASE 'SS'
		wf_set_last_generated_datetime(FALSE,ldtm_null)
		
		IF li_selected_tab = NEW_LIST THEN
			li_row = tab_confirm_eligibility.tabpage_new.dw_new_ss_eligibility.GetRow()
			IF li_row > 0 THEN
				ll_ss_claim_no = tab_confirm_eligibility.tabpage_new.dw_new_ss_eligibility.GetItemNumber(li_row,'claim_no')
				ll_ss_individual_no = tab_confirm_eligibility.tabpage_new.dw_new_ss_eligibility.GetItemNumber(li_row,'individual_no')
			END IF
		ELSEIF li_selected_tab = CHANGED_LIST THEN
			li_row = tab_confirm_eligibility.tabpage_changed.dw_changed_ss_eligibility.GetRow()
			IF li_row > 0 THEN
				ll_ss_claim_no = tab_confirm_eligibility.tabpage_changed.dw_changed_ss_eligibility.GetItemNumber(li_row,'claim_no')
				ll_ss_individual_no = tab_confirm_eligibility.tabpage_changed.dw_changed_ss_eligibility.GetItemNumber(li_row,'individual_no')
			END IF
		END IF
		ll_rows = wf_retrieve_surviving_spouse(ll_ss_claim_no, ll_ss_individual_no)
		
		IF ll_rows = 0 THEN
			// not currently eligible
			MessageBox('None Currently Eligible','This surviving spouse is not potentially eligible for an annuity.',Exclamation!)
			tab_confirm_eligibility.tabpage_new.Enabled = TRUE
			tab_confirm_eligibility.tabpage_changed.Enabled = TRUE
		END IF
		
		is_claim_role_code = 'SS'
		
	CASE ELSE
		
END CHOOSE

end event

type tab_confirm_eligibility from tab within w_confirm_annuity_eligibility
integer x = 9
integer y = 276
integer width = 4585
integer height = 1540
integer taborder = 70
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
boolean raggedright = true
boolean focusonbuttondown = true
boolean boldselectedtext = true
integer selectedtab = 1
tabpage_new tabpage_new
tabpage_changed tabpage_changed
tabpage_payout tabpage_payout
end type

on tab_confirm_eligibility.create
this.tabpage_new=create tabpage_new
this.tabpage_changed=create tabpage_changed
this.tabpage_payout=create tabpage_payout
this.Control[]={this.tabpage_new,&
this.tabpage_changed,&
this.tabpage_payout}
end on

on tab_confirm_eligibility.destroy
destroy(this.tabpage_new)
destroy(this.tabpage_changed)
destroy(this.tabpage_payout)
end on

event constructor;tab_confirm_eligibility.tabpage_new.dw_new_iw_eligibility.SetTransObject(SQLCA)
tab_confirm_eligibility.tabpage_changed.dw_changed_iw_eligibility.SetTransObject(SQLCA)
tab_confirm_eligibility.tabpage_new.dw_new_ss_eligibility.SetTransObject(SQLCA)
tab_confirm_eligibility.tabpage_changed.dw_changed_ss_eligibility.SetTransObject(SQLCA)
end event

event selectionchanged;INTEGER			li_row
u_datawindow	ldw_eligibility

ldw_eligibility = Create u_datawindow

IF oldindex > 0 THEN
	IF newindex = 1 THEN
		IF is_claim_role_code = 'C' THEN
			ldw_eligibility = tab_confirm_eligibility.tabpage_new.dw_new_iw_eligibility
		ELSEIF is_claim_role_code = 'SS' THEN
			ldw_eligibility = tab_confirm_eligibility.tabpage_new.dw_new_ss_eligibility
		END IF
	ELSEIF newindex = 2 THEN
		IF is_claim_role_code = 'C' THEN
			ldw_eligibility = tab_confirm_eligibility.tabpage_changed.dw_changed_iw_eligibility
		ELSEIF is_claim_role_code = 'SS' THEN
			ldw_eligibility = tab_confirm_eligibility.tabpage_changed.dw_changed_ss_eligibility
		END IF
	ELSEIF newindex = 3 THEN
		IF is_claim_role_code = 'C' THEN
			ldw_eligibility = idw_iw_payout
		ELSEIF is_claim_role_code = 'SS' THEN
			ldw_eligibility = idw_ss_payout
		END IF
	END IF
	
	li_row = ldw_eligibility.GetRow()
	IF li_row > 0 THEN
		ldw_eligibility.Event RowFocusChanged(li_row)
	END IF
	
END IF


end event

type tabpage_new from userobject within tab_confirm_eligibility
integer x = 18
integer y = 108
integer width = 4549
integer height = 1416
long backcolor = 67108864
string text = "New Eligibility"
long tabtextcolor = 33554432
long picturemaskcolor = 553648127
dw_new_ss_eligibility dw_new_ss_eligibility
dw_new_iw_eligibility dw_new_iw_eligibility
end type

on tabpage_new.create
this.dw_new_ss_eligibility=create dw_new_ss_eligibility
this.dw_new_iw_eligibility=create dw_new_iw_eligibility
this.Control[]={this.dw_new_ss_eligibility,&
this.dw_new_iw_eligibility}
end on

on tabpage_new.destroy
destroy(this.dw_new_ss_eligibility)
destroy(this.dw_new_iw_eligibility)
end on

type dw_new_ss_eligibility from u_datawindow within tabpage_new
boolean visible = false
integer x = 41
integer y = 48
integer width = 4443
integer height = 1324
integer taborder = 30
string dataobject = "d_ss_eligibility"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
boolean righttoleft = true
end type

event rowfocuschanged;call super::rowfocuschanged;DATETIME		ldtm_65th_birthday, ldtm_death_date
INTEGER			li_max_completed_step_no, li_find, li_rtn, li_rows
LONG				ll_checklist_no, ll_annuity_account_no, ll_annuity_eligibility_no, ll_individual_no
STRING			ls_checklist_type_code, ls_checklist_step_type_code

IF currentrow <> 0 THEN
	ls_checklist_type_code = 'CAESN'
	ll_checklist_no = THIS.GetItemNumber(currentrow,'checklist_no')
	ll_annuity_account_no = THIS.GetItemNumber(currentrow,'annuity_account_no')
	ll_individual_no = THIS.GetItemNumber(currentrow,'individual_no')
	
	wf_confirm_dw_rfc(ls_checklist_type_code,ll_annuity_account_no,ll_checklist_no,THIS, currentrow)
	li_max_completed_step_no = inv_checklist.nf_max_completed_step(ll_checklist_no)
	
	li_find = uo_checklist.tab_checklist.tabpage_checklist.dw_checklist.uf_find_row('checklist_step_no', String(li_max_completed_step_no))
	IF li_find > 0 THEN
		ls_checklist_step_type_code = uo_checklist.tab_checklist.tabpage_checklist.dw_checklist.GetItemString(li_find, 'checklist_step_type_code')
		IF ls_checklist_step_type_code = '006' THEN
			//remove filter
			li_rtn = wf_set_current_AE_filter('')
			
			// filter the dw to display uncommitted active A/E record			
			li_find = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.uf_find_row('annuity_eligibility_status_code','"P"')
			
			IF li_find > 0 THEN
				ll_annuity_eligibility_no = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.GetItemNumber(li_find,'annuity_eligibility_no')
				li_rtn = wf_set_current_AE_filter('annuity_eligibility_no = ' + String(ll_annuity_eligibility_no))
			ELSE
				SignalError(-1,'Cannot find pending AE record')
			END IF
			
		ELSE
			// filter the dw to display committed active A/E record
			li_rtn = wf_set_current_AE_filter('annuity_eligibility_status_code = "A"')
		END IF
	END IF
	
	li_rows = dw_cae_tombstone.Retrieve(ll_individual_no)
	SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'dw_new_ss_eligibility', 'dw_cae_tombstone.Retrieve')
	
	THIS.SetColumn('claim_no')
	THIS.SelectText(1,256)
	THIS.POST SetFocus()

END IF
end event

event constructor;call super::constructor;THIS.uf_setselect(1)
THIS.uf_SetSort(True)
end event

event rbuttondown;M_DW_RMB_POPUP lm_popup
window		lw_window

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

lw_window = wf_get_window_reference()	

lm_popup.m_options.PopMenu(lw_window.PointerX( ), lw_window.PointerY( ))

Destroy lm_popup
end event

event ue_print;THIS.Object.DataWindow.Print.Orientation = 1 //landscape
THIS.Print()
end event

event rowfocuschanging;call super::rowfocuschanging;INTEGER   li_rtn


li_rtn = wf_eligibility_dw_rowfocuschanging(THIS, newrow, currentrow)
RETURN li_rtn
end event

type dw_new_iw_eligibility from u_datawindow within tabpage_new
integer x = 41
integer y = 48
integer width = 4439
integer height = 1328
integer taborder = 40
string dataobject = "d_iw_eligibility"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
boolean righttoleft = true
end type

event constructor;call super::constructor;THIS.uf_setselect(1)
THIS.uf_SetSort(True)
end event

event rowfocuschanged;call super::rowfocuschanged;DATETIME		ldtm_65th_birthday, ldtm_death_date
INTEGER			li_max_completed_step_no, li_find, li_rtn, li_rows
LONG				ll_checklist_no, ll_annuity_account_no, ll_annuity_eligibility_no, ll_individual_no
STRING			ls_checklist_type_code, ls_checklist_step_type_code

IF currentrow <> 0 THEN
	ls_checklist_type_code = 'CAEIN'
	ll_checklist_no = THIS.GetItemNumber(currentrow,'checklist_no')
	ll_annuity_account_no = THIS.GetItemNumber(currentrow,'annuity_account_no')
	ll_individual_no = THIS.GetItemNumber(currentrow,'individual_no')
	
	wf_confirm_dw_rfc(ls_checklist_type_code,ll_annuity_account_no,ll_checklist_no,THIS, currentrow)
	li_max_completed_step_no = inv_checklist.nf_max_completed_step(ll_checklist_no)
	
	li_find = uo_checklist.tab_checklist.tabpage_checklist.dw_checklist.uf_find_row('checklist_step_no',String(li_max_completed_step_no))
	IF li_find > 0 THEN
		ls_checklist_step_type_code = uo_checklist.tab_checklist.tabpage_checklist.dw_checklist.GetItemString(li_find, 'checklist_step_type_code')
		IF ls_checklist_step_type_code = '006' THEN
			//remove filter
			li_rtn = wf_set_current_AE_filter('')
			
			// filter the dw to display uncommitted active A/E record
			li_find = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.uf_find_row('annuity_eligibility_status_code','"P"')
			
			IF li_find > 0 THEN
				ll_annuity_eligibility_no = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.GetItemNumber(li_find,'annuity_eligibility_no')
				li_rtn = wf_set_current_AE_filter('annuity_eligibility_no = ' + String(ll_annuity_eligibility_no))
			ELSE
				SignalError(-1,'Cannot find pending AE record')
			END IF
			
		ELSE
			// filter the dw to display committed active A/E record
			li_rtn = wf_set_current_AE_filter('annuity_eligibility_status_code = "A"')
		END IF
	END IF
	
	li_rows = dw_cae_tombstone.Retrieve(ll_individual_no)
	SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'dw_new_iw_eligibility', 'dw_cae_tombstone.Retrieve')
	
	THIS.SetColumn('individual_no')
	THIS.SelectText(1,256)
	THIS.POST SetFocus()

END IF

end event

event rbuttondown;M_DW_RMB_POPUP lm_popup
window		lw_window

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

lw_window = wf_get_window_reference()	

lm_popup.m_options.PopMenu(lw_window.PointerX( ), lw_window.PointerY( ))

Destroy lm_popup
end event

event ue_print;THIS.Object.DataWindow.Print.Orientation = 1 //landscape
THIS.Print()
end event

event rowfocuschanging;call super::rowfocuschanging;INTEGER   li_rtn


li_rtn = wf_eligibility_dw_rowfocuschanging(THIS, newrow, currentrow)
RETURN li_rtn
end event

type tabpage_changed from userobject within tab_confirm_eligibility
integer x = 18
integer y = 108
integer width = 4549
integer height = 1416
long backcolor = 67108864
string text = "Changed Eligibility"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_changed_ss_eligibility dw_changed_ss_eligibility
dw_changed_iw_eligibility dw_changed_iw_eligibility
end type

on tabpage_changed.create
this.dw_changed_ss_eligibility=create dw_changed_ss_eligibility
this.dw_changed_iw_eligibility=create dw_changed_iw_eligibility
this.Control[]={this.dw_changed_ss_eligibility,&
this.dw_changed_iw_eligibility}
end on

on tabpage_changed.destroy
destroy(this.dw_changed_ss_eligibility)
destroy(this.dw_changed_iw_eligibility)
end on

type dw_changed_ss_eligibility from u_datawindow within tabpage_changed
boolean visible = false
integer x = 32
integer y = 48
integer width = 4457
integer height = 1316
integer taborder = 40
string dataobject = "d_ss_eligibility"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
boolean righttoleft = true
end type

event constructor;call super::constructor;THIS.uf_setselect(1)
THIS.uf_SetSort(True)
end event

event rowfocuschanged;call super::rowfocuschanged;DATETIME		ldtm_65th_birthday, ldtm_death_date
INTEGER			li_max_completed_step_no, li_find, li_rtn, li_rows
LONG				ll_checklist_no, ll_annuity_account_no, ll_annuity_eligibility_no, ll_individual_no
STRING			ls_checklist_type_code, ls_checklist_step_type_code

IF currentrow <> 0 THEN
	ls_checklist_type_code = 'CAESC'
	ll_checklist_no = THIS.GetItemNumber(currentrow,'checklist_no')
	ll_annuity_account_no = THIS.GetItemNumber(currentrow,'annuity_account_no')
	ll_individual_no = THIS.GetItemNumber(currentrow,'individual_no')
	
	wf_confirm_dw_rfc(ls_checklist_type_code,ll_annuity_account_no,ll_checklist_no,THIS, currentrow)
	li_max_completed_step_no = inv_checklist.nf_max_completed_step(ll_checklist_no)
	
	li_find = uo_checklist.tab_checklist.tabpage_checklist.dw_checklist.uf_find_row('checklist_step_no', String(li_max_completed_step_no))
	IF li_find > 0 THEN
		ls_checklist_step_type_code = uo_checklist.tab_checklist.tabpage_checklist.dw_checklist.GetItemString(li_find, 'checklist_step_type_code')
		IF ls_checklist_step_type_code = '006' THEN
			//remove filter
			li_rtn = wf_set_current_AE_filter('')
			
			// filter the dw to display uncommitted active A/E record
			li_find = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.uf_find_row('annuity_eligibility_status_code','"P"')
			
			IF li_find > 0 THEN
				ll_annuity_eligibility_no = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.GetItemNumber(li_find,'annuity_eligibility_no')
				li_rtn = wf_set_current_AE_filter('annuity_eligibility_no = ' + String(ll_annuity_eligibility_no))
			ELSE
				SignalError(-1,'Cannot find pending AE record')
			END IF
			
		ELSE
			// filter the dw to display committed active A/E record
			li_rtn = wf_set_current_AE_filter('annuity_eligibility_status_code = "A"')
		END IF
	END IF
	
	li_rows = dw_cae_tombstone.Retrieve(ll_individual_no)
	SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'dw_changed_ss_eligibility', 'dw_cae_tombstone.Retrieve')
	
	THIS.SetColumn('claim_no')
	THIS.SelectText(1,256)
	THIS.POST SetFocus()

END IF
end event

event rbuttondown;M_DW_RMB_POPUP lm_popup
window		lw_window

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

lw_window = wf_get_window_reference()	

lm_popup.m_options.PopMenu(lw_window.PointerX( ), lw_window.PointerY( ))

Destroy lm_popup
end event

event ue_print;THIS.Object.DataWindow.Print.Orientation = 1 //landscape
THIS.Print()
end event

event rowfocuschanging;call super::rowfocuschanging;INTEGER   li_rtn


li_rtn = wf_eligibility_dw_rowfocuschanging(THIS, newrow, currentrow)
RETURN li_rtn
end event

type dw_changed_iw_eligibility from u_datawindow within tabpage_changed
integer x = 32
integer y = 48
integer width = 4443
integer height = 1324
integer taborder = 60
string dataobject = "d_iw_eligibility"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
boolean righttoleft = true
end type

event constructor;call super::constructor;THIS.uf_setselect(1)
THIS.uf_SetSort(True)
end event

event rowfocuschanged;call super::rowfocuschanged;DATETIME		ldtm_65th_birthday, ldtm_death_date
INTEGER			li_max_completed_step_no, li_find, li_rtn, li_rows
LONG				ll_checklist_no, ll_annuity_account_no, ll_annuity_eligibility_no, ll_individual_no
STRING			ls_checklist_type_code, ls_checklist_step_type_code, ls_filter

IF currentrow <> 0 THEN
	ls_checklist_type_code = 'CAEIC'
	ll_checklist_no = THIS.GetItemNumber(currentrow,'checklist_no')
	ll_annuity_account_no = THIS.GetItemNumber(currentrow,'annuity_account_no')
	ll_individual_no = THIS.GetItemNumber(currentrow,'individual_no')
	
	wf_confirm_dw_rfc(ls_checklist_type_code,ll_annuity_account_no,ll_checklist_no,THIS, currentrow)
	
	li_max_completed_step_no = inv_checklist.nf_max_completed_step(ll_checklist_no)
	
	li_find = uo_checklist.tab_checklist.tabpage_checklist.dw_checklist.uf_find_row('checklist_step_no',String(li_max_completed_step_no))
	IF li_find > 0 THEN
		ls_checklist_step_type_code = uo_checklist.tab_checklist.tabpage_checklist.dw_checklist.GetItemString(li_find, 'checklist_step_type_code')
		IF ls_checklist_step_type_code = '006' THEN
			//remove filter
			li_rtn = wf_set_current_AE_filter('')
			
			// filter the dw to display pending A/E record (this will be set to uncommitted active)
			li_find = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.uf_find_row('annuity_eligibility_status_code','"P"')
			
			IF li_find > 0 THEN
				ll_annuity_eligibility_no = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.GetItemNumber(li_find,'annuity_eligibility_no')
				li_rtn = wf_set_current_AE_filter('annuity_eligibility_no = ' + String(ll_annuity_eligibility_no))
			ELSE
				SignalError(-1,'Cannot find pending AE record')
			END IF
			
		ELSE
			// filter the dw to display committed active A/E record
			li_rtn = wf_set_current_AE_filter('annuity_eligibility_status_code = "A"')
		END IF
	END IF
	
	li_rows = dw_cae_tombstone.Retrieve(ll_individual_no)
	SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'dw_changed_iw_eligibility', 'dw_cae_tombstone.Retrieve')

	THIS.SetColumn('individual_no')
	THIS.SelectText(1,256)
	THIS.POST SetFocus()
	
END IF
end event

event rbuttondown;M_DW_RMB_POPUP lm_popup
window		lw_window

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

lw_window = wf_get_window_reference()	

lm_popup.m_options.PopMenu(lw_window.PointerX( ), lw_window.PointerY( ))

Destroy lm_popup
end event

event ue_print;THIS.Object.DataWindow.Print.Orientation = 1 //landscape
THIS.Print()
end event

event rowfocuschanging;call super::rowfocuschanging;INTEGER   li_rtn


li_rtn = wf_eligibility_dw_rowfocuschanging(THIS, newrow, currentrow)
RETURN li_rtn
end event

type tabpage_payout from userobject within tab_confirm_eligibility
integer x = 18
integer y = 108
integer width = 4549
integer height = 1416
long backcolor = 67108864
string text = "Payout"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_iw_payout dw_iw_payout
dw_ss_payout dw_ss_payout
end type

on tabpage_payout.create
this.dw_iw_payout=create dw_iw_payout
this.dw_ss_payout=create dw_ss_payout
this.Control[]={this.dw_iw_payout,&
this.dw_ss_payout}
end on

on tabpage_payout.destroy
destroy(this.dw_iw_payout)
destroy(this.dw_ss_payout)
end on

type dw_iw_payout from u_datawindow within tabpage_payout
integer x = 32
integer y = 48
integer width = 4457
integer height = 1316
integer taborder = 11
string dataobject = "d_iw_eligibility"
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;THIS.uf_setselect(1)
THIS.uf_SetSort(True)
end event

event rbuttondown;call super::rbuttondown;M_DW_RMB_POPUP lm_popup
window		lw_window

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

lw_window = wf_get_window_reference()	

lm_popup.m_options.PopMenu(lw_window.PointerX( ), lw_window.PointerY( ))

Destroy lm_popup
end event

event rowfocuschanged;call super::rowfocuschanged;DATETIME		ldtm_65th_birthday, ldtm_death_date
INTEGER			li_max_completed_step_no, li_find, li_rtn, li_rows
LONG				ll_checklist_no, ll_annuity_account_no, ll_annuity_eligibility_no, ll_individual_no
STRING			ls_checklist_type_code, ls_checklist_step_type_code

IF currentrow <> 0 THEN
	ls_checklist_type_code = 'CAEP'
	ll_checklist_no = THIS.GetItemNumber(currentrow,'checklist_no')
	ll_annuity_account_no = THIS.GetItemNumber(currentrow,'annuity_account_no')
	ll_individual_no = THIS.GetItemNumber(currentrow,'individual_no')
	
	wf_confirm_dw_rfc(ls_checklist_type_code,ll_annuity_account_no,ll_checklist_no,THIS, currentrow)
	li_max_completed_step_no = inv_checklist.nf_max_completed_step(ll_checklist_no)
	
	li_find = uo_checklist.tab_checklist.tabpage_checklist.dw_checklist.uf_find_row('checklist_step_no', String(li_max_completed_step_no))
	IF li_find > 0 THEN
		ls_checklist_step_type_code = uo_checklist.tab_checklist.tabpage_checklist.dw_checklist.GetItemString(li_find, 'checklist_step_type_code')
		IF ls_checklist_step_type_code = '006' THEN
			//remove filter
			li_rtn = wf_set_current_AE_filter('')
			
			// filter the dw to display uncommitted active A/E record
			li_find = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.uf_find_row('annuity_eligibility_status_code','"P"')
			
			IF li_find > 0 THEN
				ll_annuity_eligibility_no = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.GetItemNumber(li_find,'annuity_eligibility_no')
				li_rtn = wf_set_current_AE_filter('annuity_eligibility_no = ' + String(ll_annuity_eligibility_no))
			ELSE
				SignalError(-1,'Cannot find pending AE record')
			END IF
			
		ELSE
			// filter the dw to display committed active A/E record
			li_rtn = wf_set_current_AE_filter('annuity_eligibility_status_code = "A"')
		END IF
	END IF
	
	li_rows = dw_cae_tombstone.Retrieve(ll_individual_no)
	SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'dw_changed_ss_eligibility', 'dw_cae_tombstone.Retrieve')
	
	THIS.SetColumn('claim_no')
	THIS.SelectText(1,256)
	THIS.POST SetFocus()

END IF
end event

event rowfocuschanging;call super::rowfocuschanging;INTEGER   li_rtn


li_rtn = wf_eligibility_dw_rowfocuschanging(THIS, newrow, currentrow)
RETURN li_rtn
end event

event ue_print;THIS.Object.DataWindow.Print.Orientation = 1 //landscape
THIS.Print()
end event

type dw_ss_payout from u_datawindow within tabpage_payout
integer x = 32
integer y = 48
integer width = 4457
integer height = 1316
integer taborder = 11
string dataobject = "d_ss_eligibility"
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;THIS.uf_setselect(1)
THIS.uf_SetSort(True)
end event

event rbuttondown;call super::rbuttondown;M_DW_RMB_POPUP lm_popup
window		lw_window

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

lw_window = wf_get_window_reference()	

lm_popup.m_options.PopMenu(lw_window.PointerX( ), lw_window.PointerY( ))

Destroy lm_popup
end event

event rowfocuschanged;call super::rowfocuschanged;DATETIME		ldtm_65th_birthday, ldtm_death_date
INTEGER			li_max_completed_step_no, li_find, li_rtn, li_rows
LONG				ll_checklist_no, ll_annuity_account_no, ll_annuity_eligibility_no, ll_individual_no
STRING			ls_checklist_type_code, ls_checklist_step_type_code

IF currentrow <> 0 THEN
	ls_checklist_type_code = 'CAEP'
	ll_checklist_no = THIS.GetItemNumber(currentrow,'checklist_no')
	ll_annuity_account_no = THIS.GetItemNumber(currentrow,'annuity_account_no')
	ll_individual_no = THIS.GetItemNumber(currentrow,'individual_no')
	
	wf_confirm_dw_rfc(ls_checklist_type_code,ll_annuity_account_no,ll_checklist_no,THIS, currentrow)
	li_max_completed_step_no = inv_checklist.nf_max_completed_step(ll_checklist_no)
	
	li_find = uo_checklist.tab_checklist.tabpage_checklist.dw_checklist.uf_find_row('checklist_step_no', String(li_max_completed_step_no))
	IF li_find > 0 THEN
		ls_checklist_step_type_code = uo_checklist.tab_checklist.tabpage_checklist.dw_checklist.GetItemString(li_find, 'checklist_step_type_code')
		IF ls_checklist_step_type_code = '006' THEN
			//remove filter
			li_rtn = wf_set_current_AE_filter('')
			
			// filter the dw to display uncommitted active A/E record
			li_find = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.uf_find_row('annuity_eligibility_status_code','"P"')
			
			IF li_find > 0 THEN
				ll_annuity_eligibility_no = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.GetItemNumber(li_find,'annuity_eligibility_no')
				li_rtn = wf_set_current_AE_filter('annuity_eligibility_no = ' + String(ll_annuity_eligibility_no))
			ELSE
				SignalError(-1,'Cannot find pending AE record')
			END IF
			
		ELSE
			// filter the dw to display committed active A/E record
			li_rtn = wf_set_current_AE_filter('annuity_eligibility_status_code = "A"')
		END IF
	END IF
	
	li_rows = dw_cae_tombstone.Retrieve(ll_individual_no)
	SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'dw_changed_ss_eligibility', 'dw_cae_tombstone.Retrieve')
	
	THIS.SetColumn('claim_no')
	THIS.SelectText(1,256)
	THIS.POST SetFocus()

END IF
end event

event rowfocuschanging;call super::rowfocuschanging;INTEGER   li_rtn


li_rtn = wf_eligibility_dw_rowfocuschanging(THIS, newrow, currentrow)
RETURN li_rtn
end event

event ue_print;THIS.Object.DataWindow.Print.Orientation = 1 //landscape
THIS.Print()
end event

type tab_eligibility from tab within w_confirm_annuity_eligibility
integer x = 9
integer y = 1904
integer width = 4585
integer height = 828
integer taborder = 60
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
boolean raggedright = true
boolean focusonbuttondown = true
boolean boldselectedtext = true
integer selectedtab = 1
tabpage_current_eligibility tabpage_current_eligibility
tabpage_history_eligibility tabpage_history_eligibility
end type

on tab_eligibility.create
this.tabpage_current_eligibility=create tabpage_current_eligibility
this.tabpage_history_eligibility=create tabpage_history_eligibility
this.Control[]={this.tabpage_current_eligibility,&
this.tabpage_history_eligibility}
end on

on tab_eligibility.destroy
destroy(this.tabpage_current_eligibility)
destroy(this.tabpage_history_eligibility)
end on

event constructor;tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.SetTransObject(SQLCA)
tab_eligibility.tabpage_history_eligibility.dw_history_eligibility.SetTransObject(SQLCA)
end event

type tabpage_current_eligibility from userobject within tab_eligibility
integer x = 18
integer y = 108
integer width = 4549
integer height = 704
long backcolor = 67108864
string text = "Current Eligibility"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_current_eligibility dw_current_eligibility
end type

on tabpage_current_eligibility.create
this.dw_current_eligibility=create dw_current_eligibility
this.Control[]={this.dw_current_eligibility}
end on

on tabpage_current_eligibility.destroy
destroy(this.dw_current_eligibility)
end on

type dw_current_eligibility from u_datawindow within tabpage_current_eligibility
integer x = 5
integer y = 20
integer width = 4503
integer height = 644
integer taborder = 20
string dataobject = "d_current_eligibility"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;//THIS.uf_setselect(0)
THIS.uf_setfilter(TRUE)
end event

event rbuttondown;M_DW_RMB_POPUP lm_popup
window		lw_window

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

lw_window = wf_get_window_reference()	

lm_popup.m_options.PopMenu(lw_window.PointerX( ), lw_window.PointerY( ))

Destroy lm_popup
end event

event ue_print;THIS.Object.DataWindow.Print.Orientation = 1 //landscape
THIS.Print()
end event

type tabpage_history_eligibility from userobject within tab_eligibility
integer x = 18
integer y = 108
integer width = 4549
integer height = 704
long backcolor = 67108864
string text = "History Eligibility"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_history_eligibility dw_history_eligibility
end type

on tabpage_history_eligibility.create
this.dw_history_eligibility=create dw_history_eligibility
this.Control[]={this.dw_history_eligibility}
end on

on tabpage_history_eligibility.destroy
destroy(this.dw_history_eligibility)
end on

type dw_history_eligibility from u_datawindow within tabpage_history_eligibility
integer x = 5
integer y = 20
integer width = 4507
integer height = 640
integer taborder = 11
string dataobject = "d_history_eligibility"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;THIS.uf_setselect(0)
end event

event rbuttondown;M_DW_RMB_POPUP lm_popup
window		lw_window

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

lw_window = wf_get_window_reference()	

lm_popup.m_options.PopMenu(lw_window.PointerX( ), lw_window.PointerY( ))

Destroy lm_popup
end event

event ue_print;THIS.Object.DataWindow.Print.Orientation = 1 //landscape
THIS.Print()
end event

type uo_checklist from u_checklist within w_confirm_annuity_eligibility
integer x = 5079
integer y = 128
integer width = 119
integer taborder = 60
boolean bringtotop = true
boolean ib_checklist_visible = true
boolean ib_cannot_change_status = true
long il_resize_steps = 25
end type

on uo_checklist.destroy
call u_checklist::destroy
end on

event ue_checklist_buttonclicked;call super::ue_checklist_buttonclicked;BOOLEAN              lb_annuity_calculation_needed
DATETIME					ldtm_null, ldtm_accident_date, ldtm_annuity_start_date, ldtm_annuity_end_date, ldtm_annuity_eligibility_end_date_used
INTEGER					li_row, li_rtn, li_rows, li_find
INTEGER					li_opening_no, li_benefit_entitlement_rowcount, li_eligibility_row, li_selected_tab, li_max_completed_step
INTEGER					li_annuity_set_aside_percent_no, li_annuity_calculation_needed
LONG						ll_individual_no, ll_action_code, ll_claim_no, ll_verify_benefit_entitlement_checklist_no
LONG						ll_annuity_account_no, ll_annuity_eligibility_no, ll_checklist_no
STRING					ls_window_to_open , ls_checklist_step_type_code, ls_given_names, ls_last_name, ls_vbe_checklist_status_code
STRING					ls_null, ls_message
u_datawindow			ldw_eligibility
u_ds						lds_select_claim
N_PROCESS_RUN_STATUS ln_process_run_status

//ldw_eligibility = Create u_datawindow


SetNull(ldtm_null)
SetNull(ls_null)

IF adw_dw.ClassName() = 'dw_checklist' THEN
	
	ls_window_to_open = adw_dw.GetItemString(row,'open_window_name')
	IF ls_window_to_open = '' THEN
		MessageBox('No window','There is no window to be opened by clicking this button.',Exclamation!)
	END IF
	
	ls_checklist_step_type_code = adw_dw.GetItemString(row,'checklist_step_type_code')
	
	wf_set_eligibility_datawindow(ldw_eligibility)
	li_row = ldw_eligibility.GetRow()
	
	ll_claim_no = ldw_eligibility.GetItemNumber(li_row,'claim_no')
	ll_individual_no = ldw_eligibility.GetItemNumber(li_row,'individual_no')
	ll_annuity_account_no = ldw_eligibility.GetItemNumber(li_row,'annuity_account_no')
	
	ls_given_names = ldw_eligibility.GetItemString(li_row,'given_names')
	ls_last_name = ldw_eligibility.GetItemString(li_row,'last_name')
	ll_checklist_no = ldw_eligibility.GetItemNumber(li_row,'checklist_no')
	
	CHOOSE CASE ls_checklist_step_type_code
		CASE '001'
			// Identified
			
		CASE '002'
			// Search Individual
			inv_common_annuity.nf_open_individual_search('d_basic_claim_search', 'ENABLE', ls_given_names, ls_last_name, ldtm_null,0,0,0)
			
		CASE '003'
			// Request Merge Individuals
			IF is_claim_role_code = 'C' THEN
				lds_select_claim = create u_ds
				lds_select_claim.DataObject = 'ds_select_claim'
				lds_select_claim.SetTransObject(SQLCA)
				
				li_rows = lds_select_claim.Retrieve(ll_individual_no)
				SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'uo_checklist.ue_checklist_buttonclicked', 'lds_select_claim.Retrieve')
				
				IF li_rows < 0 THEN
					SignalError(-1,'The retrieval of a claim for the opening of the event log failed unexpectedly. Call the HELPDESK.')
				END IF
	
				IF li_rows = 1 THEN
					ll_claim_no = lds_select_claim.GetItemNumber(1,'claim_no')
					ldtm_accident_date = lds_select_claim.GetItemDateTime(1,'accident_date')
				ELSEIF li_rows = 0 THEN
					SignalError(-1,'Request Merge Individuals stored procedure returned no claims.')
				ELSE
					SignalError(-1,'Request Merge Individuals stored procedure returned more than one claim.')
				END IF
			ELSE
				ll_claim_no = il_claim_no
				
				SELECT accident_date
				INTO   :ldtm_accident_date
				FROM   CLAIM
				WHERE  claim_no = :ll_claim_no
				USING SQLCA;
				SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'uo_checklist.ue_checklist_buttonclicked', 'SELECT accident_date FROM CLAIM...')
			END IF
					
			inv_common_annuity.nf_open_event_log(ls_window_to_open, is_claim_role_code, ll_claim_no, ll_individual_no, Date(ldtm_accident_date), ls_last_name, ls_given_names, 'I', '042', 'REQ', 'N', 'Y','')
			
		CASE '004'
			// Merge Individuals Completed
			
		CASE '005'
			// Confirm Birth Date
			inv_common_annuity.nf_open_document_list(ls_window_to_open,is_claim_role_code, '048', ll_claim_no,ll_individual_no)
			
		CASE '006'
			// Verify Benefit Entitlement
			
			wf_retrieve_annuity_eligilbility(ll_annuity_account_no)
			
			li_eligibility_row = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.GetRow()
			
			ll_annuity_eligibility_no = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.GetItemNumber(li_eligibility_row,'annuity_eligibility_no')			
			
			li_find = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.uf_find_row('annuity_eligibility_status_code', '"P"')
			IF li_find > 0 THEN
				ll_verify_benefit_entitlement_checklist_no = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.GetItemNumber(li_find,'verify_benefit_entitlement_checklist_no')
				
				IF ll_verify_benefit_entitlement_checklist_no > 0 THEN
					// Indicates that there is a checklist
					
					SELECT	checklist_status_code
					INTO		:ls_vbe_checklist_status_code
					FROM		CHECKLIST
					WHERE	checklist_no = :ll_verify_benefit_entitlement_checklist_no
					USING SQLCA;
					SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'uo_checklist.ue_checklist_buttonclicked', 'SELECT checklist_status_code FROM CHECKLIST')
					
					CHOOSE CASE ls_vbe_checklist_status_code
						CASE '', ls_null
							li_rtn = wf_incomplete_prepare_acct_VBE_checklist(ll_verify_benefit_entitlement_checklist_no,ll_annuity_account_no)
							IF li_rtn < 0 THEN
								// has incomplete prepare annuity acct VBE checklist
								RETURN
							END IF
							

						CASE 'CA','XA','XM'
							IF MessageBox('Open VBE?','The Verify Benefit Entitlement checklist has been completed at least once. Do you want to open this module and start a new checklist?',Question!,YesNo!,2) = 2 THEN
								RETURN
							END IF
							
						CASE 'IA'
							// OK - open that checklist
							
						CASE ELSE
							// WARNING !!!  
							// this must be a new status for the checklist
							
					END CHOOSE
							
//					IF ls_vbe_checklist_status_code <> 'IA' THEN
//						// only complete or cancelled
//						IF MessageBox('Open VBE?','The Verify Benefit Entitlement checklist has been completed at least once. Do you want to open this module and start a new checklist?',Question!,YesNo!,2) = 2 THEN
//							RETURN
//						END IF
//					END IF
				ELSE
					// no VBE checklist assoc with a/e
					li_rtn = wf_incomplete_prepare_acct_VBE_checklist(ll_verify_benefit_entitlement_checklist_no,ll_annuity_account_no)
					IF li_rtn < 0 THEN
						// has incomplete prepare annuity acct VBE checklist
						RETURN
					END IF
					
				END IF
				
			ELSE
				MessageBox('No Pending AE','There is no pending annuity eligibility record. Call the HELPDESK.')
			END IF
			
			// go to prior row. this forces user to select 6 after completing VBE checklist
			li_max_completed_step = uo_checklist.inv_checklist.nf_max_completed_step(ll_checklist_no)
			uo_checklist.inv_checklist.nf_find_row('checklist_step_no', String(li_max_completed_step), adw_dw)
			
			ll_action_code = 1 // verify benefit entitlement
			inv_common_annuity.nf_open_verify_benefit_entitlement(ls_window_to_open,ll_annuity_account_no,ll_annuity_eligibility_no,ll_action_code,ll_claim_no,ll_individual_no,il_annuity_payout_no,is_claim_role_code)
			
			li_rtn = wf_set_current_AE_filter('annuity_eligibility_status_code = "A"')
			
			// In this particular case, we do not want to select 'row' in code below
			RETURN
			
		CASE '007'
			// Confirm Annuity Eligibility
			
		CASE '008'
			// Notify Injured Worker
			IF is_claim_role_code = 'C' THEN
				li_selected_tab = wf_get_selected_confirm_eligibility_tab()
				IF li_selected_tab = CHANGED_LIST THEN
					// The annuity qualification letter, if being sent, must be associated with the claim that has 
					// the most recent accident recurrence date for openings that attract annuities for the injured worker, 
					// if the injured worker no longer qualifies for annuity benefits. 
					inv_common_annuity.nf_get_AQ_claim(ll_individual_no,ll_claim_no,li_opening_no)								
				END IF
				
				IF ll_claim_no > 0 THEN
					inv_common_annuity.nf_open_correspondence('d_claim_number_search', 'ENABLE', '', '', ldtm_null,0,0,ll_claim_no)
				ELSE
					SignalError(-1,'No claim number to open correspondence in Confirm Annuity Eligibility')
				END IF
			END IF
			
		CASE '009'
			// Calculate Annuity
			
			/******************************************************************************************
			Daytime Payment Processing
			- prevents updating of tables used by PRODSVCS Payment Processing
			- N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
			- '051' refers to the Calculate Annuity module, '044' refers to the Payment Processing module
			******************************************************************************************/
			ln_process_run_status = Create N_PROCESS_RUN_STATUS
			
			li_rtn = ln_process_run_status.nf_in_progress('051','044','open',SQLCA)
			
			IF li_rtn = 1 THEN
				// module is blocked
				return
			END IF
			/******************************************************************************************/
			
			li_rtn = wf_get_last_confirmed_data(ll_annuity_eligibility_no)
			IF li_rtn > 0 THEN
				li_eligibility_row = tab_eligibility.tabpage_history_eligibility.dw_history_eligibility.uf_find_row('annuity_eligibility_no', String(ll_annuity_eligibility_no))
				
				ldtm_annuity_start_date                = tab_eligibility.tabpage_history_eligibility.dw_history_eligibility.GetItemDateTime(li_eligibility_row,'annuity_start_date')
				ldtm_annuity_end_date                  = tab_eligibility.tabpage_history_eligibility.dw_history_eligibility.GetItemDateTime(li_eligibility_row,'annuity_end_date')
				ldtm_annuity_eligibility_end_date_used = tab_eligibility.tabpage_history_eligibility.dw_history_eligibility.GetItemDateTime(li_eligibility_row,'annuity_eligibility_end_date_used')
				li_annuity_set_aside_percent_no        = tab_eligibility.tabpage_history_eligibility.dw_history_eligibility.GetItemNumber(li_eligibility_row,'annuity_set_aside_percent_no')
				
							
				li_rtn = wf_check_missing_annuity_interest_rate(ll_annuity_eligibility_no)
				IF li_rtn < 0 THEN
					// do not open window, an error msg was displayed by function already
					RETURN
				END IF
				
				lb_annuity_calculation_needed = inv_common_annuity.nf_check_for_annuity_calculation(ll_annuity_account_no,ll_annuity_eligibility_no)
							
				IF lb_annuity_calculation_needed THEN
					li_annuity_calculation_needed = 1 // 1 = TRUE
					li_rtn = wf_check_unapplied_manual_set_asides(ll_annuity_eligibility_no,is_claim_role_code)
					IF li_rtn > 0 THEN
						// do not open window, an error msg was displayed by function already
						RETURN
					END IF
					
					// warn users
					IF is_claim_role_code = 'C' THEN
						ls_message = 'Are there any Loss of Earnings benefit entitlement adjustments'&
									+'~r~nowed to the Injured Worker for any claims of the individual'&
									+'~r~nthat have not yet been issued and/or processed?'
					ELSE
						ls_message = 'Are there any Survivor benefit entitlement adjustments'&
									+'~r~nowed to the Surviving Spouse for this claim'&
									+'~r~nthat have not yet been issued and/or processed?'
					END IF
					IF MessageBox('Continue Annuity Calculation?',ls_message,Question!,YesNo!,1) = 1 THEN
						MessageBox('Annuity Calculation Aborted','You cannot proceed with Calculate Annuity benefits until'&
									+'~r~nall benefit entitlement adjustment payments are issued and processed.',Information!)
						RETURN
					END IF
						
				ELSE				
					li_annuity_calculation_needed = 0 // 0 = FALSE
				END IF
				
				// open the Annuity Calculation module
				inv_common_annuity.nf_open_calculate_annuity(ls_window_to_open,ll_annuity_account_no,ll_annuity_eligibility_no,il_annuity_payout_no,ldtm_annuity_start_date, ldtm_annuity_end_date, ldtm_annuity_eligibility_end_date_used, li_annuity_set_aside_percent_no,li_annuity_calculation_needed)
			ELSE
				SignalError(-1,'No eligibility number to open the Annuity Calculation module from Confirm Annuity Eligibility.')
			END IF
			
		CASE '024'
			// Checklist Completed
			
		CASE ELSE
			
	END CHOOSE
	
	idw_dw[1].SelectRow(0, false)
	idw_dw[1].SelectRow(row, true)
	
END IF

end event

event ue_checklist_itemchanged;call super::ue_checklist_itemchanged;BOOLEAN            lb_calculate_problem, lb_display_popup, lb_eligible, lb_warning, lb_prevent_warning, lb_commit
INTEGER            li_selected_tab, li_rtn, li_rtn2, li_rows, li_find, li_count, li_benefit_entitlement_rowcount, li_opening_no, li_trancount
DATE               ldt_last_confirmed_date
DATETIME           ldtm_last_confirmed_date, ldtm_last_annuity_start_date,ldtm_last_annuity_end_date
DATETIME           ldtm_new_annuity_start_date,ldtm_new_annuity_end_date, ldtm_request_merge_concluded, ldtm_null
DATETIME           ldtm_new_annuity_eligibility_end_date_used
DECIMAL            ldec_annuity_set_aside_percent, ldec_total_net_payment_amount
LONG               ll_confirm_eligibility_row, ll_rowcount, ll_row, ll_rows, ll_txn_count, ll_benefit_entitlement_count
LONG               ll_claim_no, ll_individual_no, ll_annuity_account_no, ll_checklist_no, ll_benefit_entitlement_no, ll_annuity_eligibility_no
STRING             ls_checklist_type_code, ls_checklist_step_type_code, ls_dwo_name
STRING             ls_datawindow_control, ls_last_benefit_option_code, ls_calculate_message, ls_message, ls_return, ls_data
DWObject           l_dwo
u_datawindow       ldw_eligibility
U_DS               lds_reexamine_benefit_entitlement
s_verify_warning   lstr_verify_warning



setNull(ldtm_null)

/*
NOTE that the committing of the items that have been changed will occur in ue_checklist_itemchanged
which is triggered in the ancestor u_datawindow.
*/

ldw_eligibility = Create u_datawindow

ls_datawindow_control = adw_dw.ClassName()

IF ls_datawindow_control = 'dw_checklist' THEN
	
	//messagebox('descendant','itemchanged - begin')
	dwitemstatus ldwis
	ldwis = adw_dw.GetItemStatus(row,0,Primary!)
	
	wf_set_eligibility_datawindow(ldw_eligibility)
	ll_confirm_eligibility_row = ldw_eligibility.GetRow()
	
	If al_return <> 0 THEN
		// only rollback if there is a txn open
		SQLCA.nf_transaction_count(li_trancount,1,'','','',FALSE)
		IF li_trancount = 1 THEN
			SQLCA.nf_rollback_transaction()
		END IF
		IF ll_confirm_eligibility_row > 0 THEN
			ll_checklist_no = adw_dw.GetItemNumber(row,'checklist_no')
			ls_checklist_type_code = adw_dw.GetItemString(row,'checklist_type_code')
			wf_select_next_available_checklist_step(ll_checklist_no,ls_checklist_type_code)
		END IF
		GOTO Label_end
	END IF
	
	
	SetRedraw(FALSE)
	
	
	ldwis = adw_dw.GetItemStatus(row,0,Primary!)
	
	
	IF ll_confirm_eligibility_row > 0 THEN
		
		ls_dwo_name = dwo.name
		
		IF ls_dwo_name = 'checklist_step_status_code' THEN
				
			ll_claim_no = ldw_eligibility.GetItemNumber(ll_confirm_eligibility_row,'claim_no')
			ll_individual_no = ldw_eligibility.GetItemNumber(ll_confirm_eligibility_row,'individual_no')
			ll_annuity_account_no = ldw_eligibility.GetItemNumber(ll_confirm_eligibility_row,'annuity_account_no')
			
			ll_checklist_no = adw_dw.GetItemNumber(row,'checklist_no')
			ls_checklist_type_code = adw_dw.GetItemString(row,'checklist_type_code')
			ls_checklist_step_type_code = adw_dw.GetItemString(row,'checklist_step_type_code')
						
			li_rtn = wf_get_last_confirmed_data(ldtm_last_confirmed_date,ldtm_last_annuity_start_date,ldtm_last_annuity_end_date,ls_last_benefit_option_code,ldec_annuity_set_aside_percent)
						
			ldt_last_confirmed_date = Date(ldtm_last_confirmed_date)
			
			// 2.370	Annuity eligibility work must not continue for an individual who has been merged 
			// since the last time the potential eligibility lists were retrieved.
			li_count = inv_common_annuity.nf_check_individual_exists(ll_individual_no)
			IF li_count = 0 THEN
				SetRedraw(TRUE)
				ls_message = 'The individual number that you are working on no longer exists.'&
								+'~r~nThe individual may have been merged. The checklist step will not be concluded.'
				wf_individual_error(ls_message)
				al_return = 2
				GOTO label_end
			END IF
			
			CHOOSE CASE ls_checklist_step_type_code
				CASE '001'
					// Identified				
				
				CASE '003'
					// Request Merge Individuals
					// verify that a 'merge individuals' event has been created (event_type_code = '042')
					IF data = 'COM' THEN
						// look for 'completed' sub type
						li_count = wf_merge_event_count(ll_individual_no,ldtm_last_confirmed_date,'REQ')
						IF li_count = 0 THEN
							// only rollback if there is a txn open
							SQLCA.nf_transaction_count(li_trancount,1,'','','',FALSE)
							IF li_trancount = 1 THEN
								SQLCA.nf_rollback_transaction()
							END IF
							
							// the retrieval will set the dw item status to not modified & prevent firing of ue_itemchangeaccepted
							inv_checklist.nf_retrieve_checklists(ls_checklist_type_code, ll_checklist_no)
							wf_select_next_available_checklist_step(ll_checklist_no,ls_checklist_type_code)
							
							SetRedraw(TRUE)
							MessageBox('Annuity Problem','You must create a "merge individual request" event before this step can be set to "completed".',Exclamation!)
							al_return = 1				
							GOTO label_end
						END IF
					END IF
									
				CASE '004'
					// Merge Individuals Completed
					IF data = 'COM' THEN
						li_count = wf_merge_event_count(ll_individual_no,ldtm_last_confirmed_date,'COM')
						IF li_count = 0 THEN
							// only rollback if there is a txn open
							SQLCA.nf_transaction_count(li_trancount,1,'','','',FALSE)
							IF li_trancount = 1 THEN
								SQLCA.nf_rollback_transaction()
							END IF
							
							// the retrieval will set the dw item status to not modified & prevent firing of ue_itemchangeaccepted
							inv_checklist.nf_retrieve_checklists(ls_checklist_type_code, ll_checklist_no)
							
							wf_select_next_available_checklist_step(ll_checklist_no,ls_checklist_type_code)
							
							SetRedraw(TRUE)
							MessageBox('Annuity Problem','You must create a "merge individual completed" event before this step can be set to "completed".',Exclamation!)
							al_return = 1
							GOTO label_end
						END IF
					ELSEIF data = 'NRM' THEN
						
						SELECT concluded_date
						INTO   :ldtm_request_merge_concluded
						FROM   CHECKLIST_STEP
						WHERE  checklist_no = :ll_checklist_no
						AND    checklist_step_type_code = '003'
						USING SQLCA;
						SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'embedded SQL: SELECT concluded_date FROM CHECKLIST_STEP...','ue_checklist_itemchanged')
						
						li_count = wf_merge_event_count(ll_individual_no,ldtm_request_merge_concluded,'NRE')
						IF li_count = 0 THEN
							// only rollback if there is a txn open
							SQLCA.nf_transaction_count(li_trancount,1,'','','',FALSE)
							IF li_trancount = 1 THEN
								SQLCA.nf_rollback_transaction()
							END IF
							
							// the retrieval will set the dw item status to not modified & prevent firing of ue_itemchangeaccepted
							inv_checklist.nf_retrieve_checklists(ls_checklist_type_code, ll_checklist_no)
							wf_select_next_available_checklist_step(ll_checklist_no,ls_checklist_type_code)
							
							SetRedraw(TRUE)
							MessageBox('Annuity Problem','You must create a "merge individual not required" event before this step can be set to "not required".',Exclamation!)
							al_return = 1
							GOTO label_end
						END IF
					END IF
					
									
				CASE '005'
					// Confirm Birth Date
					SELECT	Count(*)
					INTO		:li_count
					FROM		INDIVIDUAL
					WHERE		individual_no = :ll_individual_no
					AND		birth_date is null
					USING SQLCA;
					SQLCA.nf_handle_error(ls_datawindow_control, 'ue_checklist_ItemChanged', 'SELECT Count(*) from INDIVIDUAL...')
					
					IF li_count > 0 THEN
						// No birth date
						// the retrieval will set the dw item status to not modified & prevent firing of ue_itemchangeaccepted
						inv_checklist.nf_retrieve_checklists(ls_checklist_type_code, ll_checklist_no)
						wf_select_next_available_checklist_step(ll_checklist_no,ls_checklist_type_code)
						
						SetRedraw(TRUE)
						MessageBox('Birth Date Problem','This individual has no birth date. It is required for annuity eligibility purposes.')
						al_return = 1
						GOTO label_end						
					END IF
					
					
				CASE '006'
	//				Verify Benefit Entitlement
					
					IF IsValid(ids_annuity_payout) AND il_annuity_payout_no > 0 THEN
						// re-retrieve this A/P record. It has been updated in the VBE since last retrieved
						ll_rows = ids_annuity_payout.Retrieve(il_annuity_payout_no)
						SQLCA.nf_handle_error('w_confirm_annuity_eligibility','ids_annuity_payout','retrieve')
					END IF
					
					// filter the dw to display pending A/E record
					li_rtn = wf_set_current_AE_filter('')
					
					li_find = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.uf_find_row('annuity_eligibility_status_code','"P"')
					
					IF li_find > 0 THEN
						ll_annuity_eligibility_no = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.GetItemNumber(li_find,'annuity_eligibility_no')
						li_rtn = wf_set_current_AE_filter('annuity_eligibility_no = ' + String(ll_annuity_eligibility_no))
					ELSE
						MessageBox('PROBLEM','Cannot find pending AE record')
					END IF
					tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.SelectRow (li_find,FALSE)
					 
					// verify benefit entitlement checklist
					// if not complete, prevent checklist item from being checked				
					IF data = 'COM' THEN
						li_rtn = wf_benefit_entitlement_checklist(ll_annuity_account_no,ll_annuity_eligibility_no,TRUE) // true = display error msgs
						
						IF li_rtn < 0 THEN
							// only rollback if there is a txn open
							SQLCA.nf_transaction_count(li_trancount,1,'','','',FALSE)
							IF li_trancount = 1 THEN
								SQLCA.nf_rollback_transaction()
							END IF
												
							// the retrieval will set the dw item status to not modified & prevent firing of ue_itemchangeaccepted
							inv_checklist.nf_retrieve_checklists(ls_checklist_type_code, ll_checklist_no)
							wf_select_next_available_checklist_step(ll_checklist_no,ls_checklist_type_code)
							
							li_rtn = wf_set_current_AE_filter('annuity_eligibility_status_code = "A"')
							
							SetRedraw(TRUE)
							al_return = 1
							GOTO label_end
						END IF
					END IF
	
	
					// display a popup window with the potential annuity account calculation data
					// this is to give users a chance to stop the completion of step 006/007, 
					// so that they can update their benefit entitlement data to prevent
					// unnecessarily backing out the subledger.
					
					lds_reexamine_benefit_entitlement = Create U_DS
					
					// need to get the annuity start & end dates
					IF is_claim_role_code = 'C' THEN
						lds_reexamine_benefit_entitlement.DataObject = 'ds_reexamine_benefit_entitlement_IW'
						inv_common_annuity.nf_post_92_setasides_for_iw(ll_individual_no,ll_txn_count,ldec_total_net_payment_amount)
					ELSE
						lds_reexamine_benefit_entitlement.DataObject = 'ds_reexamine_benefit_entitlement_SS'
						inv_common_annuity.nf_post_81_setasides_for_ss(ll_individual_no,ll_claim_no,ll_txn_count,ldec_total_net_payment_amount)
					END IF
					
					// determine eligibility
					li_rtn = lds_reexamine_benefit_entitlement.SetTransObject(SQLCA)
					
					li_benefit_entitlement_rowcount = lds_reexamine_benefit_entitlement.Retrieve(ll_annuity_account_no,ll_claim_no)
					SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'ds_reexamine_benefit_entitlement.Retrieve', 'ue_checklist_itemchanged')
				
					IF li_benefit_entitlement_rowcount > 0 THEN
						li_rtn = wf_get_entitled_data(lds_reexamine_benefit_entitlement,ll_annuity_account_no,ll_individual_no,ll_claim_no,&
																ldtm_new_annuity_start_date,ldtm_new_annuity_eligibility_end_date_used,ldtm_new_annuity_end_date,ldec_annuity_set_aside_percent)
					ELSE
						
					END IF

					// popup pre-calculation summary to warn users
					lstr_verify_warning.annuity_account_no                = ll_annuity_account_no
					lstr_verify_warning.annuity_eligibility_no            = ll_annuity_eligibility_no
					lstr_verify_warning.annuity_payout_no                 = il_annuity_payout_no
					lstr_verify_warning.claim_no                          = ll_claim_no
					lstr_verify_warning.individual_no                     = ll_individual_no
					lstr_verify_warning.claim_role_code                   = is_claim_role_code
					lstr_verify_warning.annuity_start_date                = ldtm_new_annuity_start_date
					lstr_verify_warning.annuity_eligibility_end_date_used = ldtm_new_annuity_eligibility_end_date_used
					lstr_verify_warning.annuity_end_date                  = ldtm_new_annuity_end_date
					lstr_verify_warning.annuity_percentage                = ldec_annuity_set_aside_percent
					lstr_verify_warning.window_mode                       = 'warning'
					
					
					// if the a/e start date is after the a/e end date, then use null dates
					// because the confirmation process will ultimately set the a/e dates to null
					IF (ldtm_new_annuity_start_date > ldtm_new_annuity_end_date) THEN
						SetNull(lstr_verify_warning.annuity_start_date)
						SetNull(lstr_verify_warning.annuity_end_date)
					END IF
					
					// the intent is to prevent a warning, not to possibly trigger the completion of the Calculate Annuity checklist step
					lb_warning = TRUE
					
					IF IsNull(ldtm_new_annuity_start_date) OR String(ldtm_new_annuity_start_date,'yyyy-mm-dd') = '1900-01-01' THEN
						// if there is going to be no annuity start date, then IW/SS is not eligible
						lb_eligible = FALSE
					ELSE
						lb_eligible = TRUE
					END IF
					
					// determine if the annuity calculation checklist step should be automatically completed
					// if it should, then DO NOT display the 'VBE warning'. This deters users from using an
					// 'annuity calculation' that will never be made as the basis of a manual (and erroneous) annuity payout
					wf_determine_calc_step_auto_completion(is_claim_role_code,ll_annuity_account_no,ll_annuity_eligibility_no,ll_individual_no,ll_claim_no, &
							                     ldtm_new_annuity_start_date,ldtm_new_annuity_end_date,lb_eligible,ll_checklist_no,ls_checklist_type_code, &
														adw_dw,lb_warning, ib_prevent_VBE_step_warning,is_auto_calculation_step_status,lb_commit)
					
					IF ib_prevent_VBE_step_warning = TRUE THEN
						// NRA or NAA - not required or not allowed, so display a different kind of warning
						lstr_verify_warning.window_mode = is_auto_calculation_step_status
					END IF
					
					// open popup to warn users to complete the individual's benefit entitlement
					OpenWithParm(w_verify_step_warning,lstr_verify_warning)
				
					ls_return = Message.StringParm
					
					IF ls_return = 'cancel' THEN
						// only rollback if there is a txn open
						SQLCA.nf_transaction_count(li_trancount,1,'','','',FALSE)
						IF li_trancount = 1 THEN
							SQLCA.nf_rollback_transaction()
						END IF
							
						// the retrieval will set the dw item status to not modified & prevent firing of ue_itemchangeaccepted
						inv_checklist.nf_retrieve_checklists(ls_checklist_type_code, ll_checklist_no)
						wf_select_next_available_checklist_step(ll_checklist_no,ls_checklist_type_code)
						
						li_rtn = wf_set_current_AE_filter('annuity_eligibility_status_code = "A"')
						
						SetRedraw(TRUE)
						al_return = 1
						GOTO label_end
					ELSEIF ls_return = 'calculate' THEN
						post function uf_trigger_itemchangeaccepted(adw_dw,row,ls_dwo_name,data)
					END IF		
							
				CASE '007'
					// Confirm Annuity Eligibility
			
				CASE '008'
					// Notify Injured Worker
					// if the IW is on the 'changed' list, then correspondence for 'ANNQUAL' template
					// must exist to allow changing status of checklist step in order for this step
					// to be updated as 'completed'
					
					/*
					2.60		For each potential changed injured worker annuity eligibility, the Notify Injured Worker step may be completed, if correspondence 
								has been produced for the injured worker from the Annuity Qualification template since the last confirmation of eligibility.   (See Rationale.)
								-- warn user if they choose completed & there is no AQ correspondence since last confirmation of eligibility
								
								also - may NOT be set to Not Required if there is AQ correspondence since last confirmation of eligibility
					*/
					
					
					li_selected_tab = wf_get_selected_confirm_eligibility_tab()
					IF is_claim_role_code = 'C' THEN
						IF li_selected_tab = CHANGED_LIST THEN
							SELECT	Count(*)
							INTO		:li_count
							FROM		CORRESPONDENCE a
							JOIN		CLAIM b ON a.claim_no = b.claim_no
							WHERE	a.template_code = 'ANNQUAL'
							AND		a.create_date > :ldt_last_confirmed_date
							AND		a.correspond_status_code = 'S'
							AND		b.individual_no = :ll_individual_no
							USING SQLCA;
							SQLCA.nf_handle_error(ls_datawindow_control, 'ue_checklist_ItemChanged', 'SELECT Count(*) CORRESPONDENCE')
							
							IF data = 'COM' THEN
								IF li_count = 0 THEN
									//Problem
									li_rtn = MessageBox('No Correspondence','No Annuity Qualification correspondence letter has been sent since the last confirmed date ('+String(ldt_last_confirmed_date,'yyyy-mm-dd')+').'&
																			+'~r~nDo you still want to mark the Notify Injured Worker step as Complete?',Exclamation!,YesNo!,2)
								ELSE
									// ignore
								END IF
							ELSEIF data = 'NRM' THEN
								IF li_count > 0 THEN
									// an error - rollback
									MessageBox('Annuity Correspondence','An Annuity Qualification correspondence letter has been sent since the last confirmed date ('+String(ldt_last_confirmed_date,'yyyy-mm-dd')+'),'&
																	+'~r~nso the Notify Injured Worker step cannot be marked as Not Required.',Exclamation!)
									li_rtn = 2
								END IF
							END IF
													
							// if not required & AQ exists, rollback
							// or if completed & no AQ, but user chose to rollback
							IF li_rtn = 2 THEN
								// only rollback if there is a txn open
								SQLCA.nf_transaction_count(li_trancount,1,'','','',FALSE)
								IF li_trancount = 1 THEN
									SQLCA.nf_rollback_transaction()
								END IF
								
								// the retrieval will set the dw item status to not modified & prevent firing of ue_itemchangeaccepted
								inv_checklist.nf_retrieve_checklists(ls_checklist_type_code, ll_checklist_no)
								wf_select_next_available_checklist_step(ll_checklist_no,ls_checklist_type_code)
					
								li_rtn = wf_set_current_AE_filter('annuity_eligibility_status_code = "A"')
								
								SetRedraw(TRUE)
								al_return = 1
								GOTO label_end
							END IF
						ELSE
							IF data = 'COA' THEN
								inv_common_annuity.nf_get_AQ_claim(ll_individual_no, ll_claim_no, li_opening_no)
								IF ll_claim_no > 0 THEN
									li_rtn = wf_create_mail_package(ll_claim_no)
								ELSE
									li_rtn = -1
								END IF
								IF li_rtn <> 1 THEN
									// only rollback if there is a txn open
									SQLCA.nf_transaction_count(li_trancount,1,'','','',FALSE)
									IF li_trancount = 1 THEN
										SQLCA.nf_rollback_transaction()
									END IF
									
									// the retrieval will set the dw item status to not modified & prevent firing of ue_itemchangeaccepted
									inv_checklist.nf_retrieve_checklists(ls_checklist_type_code, ll_checklist_no)
									wf_select_next_available_checklist_step(ll_checklist_no,ls_checklist_type_code)
						
									li_rtn = wf_set_current_AE_filter('annuity_eligibility_status_code = "A"')
									
									SetRedraw(TRUE)
									
									Error.Text = Error.Text + ' - error during completion of Notify Injured Worker step'
									Error.WindowMenu="cmwb"
									Error.Object="w_confirm_annuity_eligibility"
									SignalError()
								END IF
							END IF
						END IF
					ELSE
						//
					END IF
					
				CASE '009'
					ll_annuity_eligibility_no = inv_common_annuity.nf_get_checklist_annuity_eligibility_no(ll_checklist_no)
					
					// Calculate Account Balance
					SELECT Count(*)
					INTO   :li_count
					FROM   ANNUITY_CALC_ACCOUNT_HEADER a
					WHERE  a.annuity_account_no = :ll_annuity_account_no
					AND    a.annuity_eligibility_no = :ll_annuity_eligibility_no
					USING SQLCA;
					SQLCA.nf_handle_error('w_confirm_annuity_eligibility', 'uo_checklist.ue_checklist_itemChanged', 'SELECT Count(*) FROM ANNUITY_CALC_ACCOUNT_HEADER...')
					
					IF data = 'COM' THEN
						IF li_count = 0 THEN
							lb_calculate_problem = TRUE
							ls_calculate_message = 'You must save an annuity calculation before this step can be set to "completed".'
						END IF
						
					ELSEIF data = 'NRM' THEN
						IF li_count <> 0 THEN
							lb_calculate_problem = TRUE
							ls_calculate_message = 'This annuity eligibility has an associated annuity calculation. The status cannot be set to "Not Required".'
						END IF						
						
					END IF
					
					IF lb_calculate_problem THEN
						lb_calculate_problem = FALSE
						// the retrieval will set the dw item status to not modified & prevent firing of ue_itemchangeaccepted
						inv_checklist.nf_retrieve_checklists(ls_checklist_type_code, ll_checklist_no)
						wf_select_next_available_checklist_step(ll_checklist_no,ls_checklist_type_code)
						
						SetRedraw(TRUE)
						MessageBox('Annuity Problem',ls_calculate_message,Exclamation!)
						al_return = 1
						GOTO label_end
					END IF

			END CHOOSE
	
			
		ELSEIF dwo.Name = 'step_checked' THEN
			SetRedraw(TRUE)
			al_return = 2
			GOTO label_end
		END IF
	END IF
	
	SetRedraw(TRUE)
	
	
	// there are scenarios where multiple checklist steps are committed together,
	// so there is no need to open another transaction if one has already been begun
	SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
	IF li_trancount = 0 THEN
		// the ancestor ue_checklist_itemchangeaccepted will execute next, so must begin transaction before updates are called
		SQLCA.nf_begin_transaction()
	END IF
	
	ldwis = adw_dw.GetItemStatus(row,ls_dwo_name,Primary!)
	//messagebox('descendant','itemchanged - end')
	
	adw_dw.SetItemStatus(row,ls_dwo_name,Primary!,DataModified!)
	
	ldwis = adw_dw.GetItemStatus(row,ls_dwo_name,Primary!)

END IF
	
al_return = 0

label_end:
end event

event ue_checklist_itemchangeaccepted;call super::ue_checklist_itemchangeaccepted;BOOLEAN		lb_commit, lb_auto_not_require_calc_step, lb_eligible, lb_warning, lb_prevent_warning
DECIMAL			ldec_last_annuity_set_aside_percent, ldec_new_annuity_set_aside_percent
DATE			ldt_current
DATETIME		ldtm_death_date, ldtm_last_confirmed_date,ldtm_last_annuity_start_date,ldtm_last_annuity_end_date
DATETIME		ldtm_new_annuity_start_date,ldtm_new_annuity_end_date,ldtm_null
INTEGER			li_rtn, li_selected_tab, li_find, li_rows, li_eligibility_find, li_status_find, li_benefit_entitlement_rowcount
LONG				ll_annuity_account_no, ll_individual_no, ll_claim_no, ll_checklist_no, ll_annuity_eligibility_no
LONG				ll_confirm_eligibility_row, ll_rows, ll_rtn
STRING			ls_checklist_type_code, ls_checklist_step_type_code, ls_datawindow_control, ls_last_benefit_option_code, ls_new_benefit_option_code
STRING			ls_pre_1993_annuity_eligibility_flag, ls_message, ls_annuity_eligibility_status_code, ls_status_assigned_method_code
DataWindowChild ldwc_child
DWObject		l_dwo
u_datawindow	ldw_eligibility
S_WINDOW_MESSAGE lstr_message


IF IsValid(w_error) THEN RETURN

SETREDRAW(FALSE)

SetNull(ldtm_null)

ls_datawindow_control = adw_dw.ClassName()

IF ls_datawindow_control = 'dw_checklist' THEN
	
	//messagebox('descendant','itemchanged accepted - begin')
	dwitemstatus ldwis
	ldwis = adw_dw.GetItemStatus(al_row,0,Primary!)
	
	
	wf_set_eligibility_datawindow(ldw_eligibility)
	
	ll_confirm_eligibility_row = ldw_eligibility.GetRow()
	
	li_selected_tab = wf_get_selected_confirm_eligibility_tab()
	
	IF ll_confirm_eligibility_row > 0 AND as_column_name = 'checklist_step_status_code' THEN		
		ll_claim_no = ldw_eligibility.GetItemNumber(ll_confirm_eligibility_row,'claim_no')
		ll_individual_no = ldw_eligibility.GetItemNumber(ll_confirm_eligibility_row,'individual_no')
		ll_annuity_account_no = ldw_eligibility.GetItemNumber(ll_confirm_eligibility_row,'annuity_account_no')
		
		ll_checklist_no = adw_dw.GetItemNumber(al_row,'checklist_no')
		ls_checklist_type_code = adw_dw.GetItemString(al_row,'checklist_type_code')
		ls_checklist_step_type_code = adw_dw.GetItemString(al_row,'checklist_step_type_code')
		
		// Perform work after update
		CHOOSE CASE ls_checklist_step_type_code
			CASE '001'
				// Identified
				lb_commit = TRUE
				
			CASE '002'
				// Search Individual
				lb_commit = TRUE
				
			CASE '003'
				// Request Merge Individuals
				IF as_data = 'NRM' THEN
					// if this step is not required, then next step is also not required.
					li_find = adw_dw.uf_find_row('checklist_step_type_code', '"004"')
					IF li_find > 0 THEN
						adw_dw.SetItem(li_find, 'checklist_step_status_code', 'NRA')
						adw_dw.SetItem(li_find, 'step_checked', 'Y')
						lb_commit = FALSE
						
						l_dwo = adw_dw.object.checklist_step_status_code
						
						ls_status_assigned_method_code = 'A'
						uo_checklist.TRIGGER Event ue_checklist_step_status_changed(adw_dw,li_find,l_dwo,as_data,ls_status_assigned_method_code,ll_rtn)
						
						adw_dw.POST Event ItemChanged(li_find,l_dwo,'NRA')
					ELSE
						wf_reject_checklist_status_change(TRUE,'Missing Step','Missing step 004 - call helpdesk.',ll_checklist_no,ls_checklist_type_code)
						RETURN
					END IF
				ELSE
					lb_commit = TRUE
				END IF
						
			CASE '004'
				// Merge Individuals Completed
				lb_commit = TRUE
				
			CASE '005'
				// Confirm Birth Date
				lb_commit = TRUE
				
			CASE '006'
				// Verify Benefit Entitlement
				
				// if step 006 was completed, then auto-complete step 007
				IF as_data = 'COM' THEN
				
					// if this step is completed, then next step is also not required.
					li_find = adw_dw.uf_find_row('checklist_step_type_code', '"007"')
					IF li_find > 0 THEN
						
						adw_dw.SetItem(li_find, 'checklist_step_status_code', 'COA')
						adw_dw.SetItem(li_find, 'step_checked', 'Y')
						
						ldwis = adw_dw.GetItemStatus(li_find,0,Primary!)
																		
						adw_dw.SetItemStatus(li_find,0,Primary!,DataModified!)
	
						lb_commit = FALSE
						l_dwo = adw_dw.object.checklist_step_status_code
						
						ls_status_assigned_method_code = 'A'
						uo_checklist.TRIGGER Event ue_checklist_step_status_changed(adw_dw,li_find,l_dwo,as_data,ls_status_assigned_method_code,ll_rtn)
						
						adw_dw.POST Event ItemChanged(li_find,l_dwo,'COA')
					ELSE
						wf_reject_checklist_status_change(TRUE,'Missing Step','Missing step 007 - call helpdesk.',ll_checklist_no,ls_checklist_type_code)
						RETURN
					END IF
				ELSE
					lb_commit = TRUE
				END IF
				
			CASE '007'
				// Confirm Annuity Eligibility				
				li_rtn = wf_confirm_annuity_eligibility(ll_annuity_account_no,ll_individual_no,ll_claim_no)
				IF li_rtn < 0 THEN
					SQLCA.nf_rollback_transaction()
					
					// the retrieval will set the dw item status to not modified & prevent firing of ue_checklist_itemchangeaccepted
					inv_checklist.nf_retrieve_checklists(ls_checklist_type_code, ll_checklist_no)
					wf_select_next_available_checklist_step(ll_checklist_no,ls_checklist_type_code)
					SetRedraw(TRUE)
					Error.Text = Error.Text + ' - error during completion of Confirm Annuity Eligibility step'
					Error.WindowMenu="cmwb"
					Error.Object="w_confirm_annuity_eligibility"
					SignalError()
					RETURN
				END IF
								
				li_rtn = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.Update()
				SQLCA.nf_handle_error('dw_current_eligibility', 'ue_checklist_itemchangeaccepted', 'update')
				// if step 007 was completed, and injured worker was on 'new' list 
				// then auto-complete step 008 
				
				/*
					2.50		For each potential new injured worker annuity eligibility, the Notify Injured Worker step of checklist must be automatically completed, 
								if the Annuity Qualification Mail package has been created to notify the injured worker of their eligibility.
								
					2.70		The Notify Injured Worker step for a potential new injured worker annuity eligibility must be automatically set to Not Required, 
								if the individual has been confirmed as not eligible for annuity benefits.
	
					2.80		The Notify Injured Worker step for a change in annuity for an injured worker eligibility must be automatically set to Not Required,
								if the individual has been confirmed as eligible for annuity benefits but there has been no change since annuity was last confirmed.
				*/
								
				IF is_claim_role_code = 'C' THEN
					li_find = adw_dw.uf_find_row('checklist_step_type_code', '"008"')
					IF li_selected_tab = NEW_LIST THEN
						// new list
						IF as_data = 'COA' THEN
							IF li_find > 0 THEN							
								// since step 007 is complete, if there is an active eligibility
								// then the individual was eligible
								li_eligibility_find = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.uf_find_row('annuity_eligibility_status_code','"A"')
								
								SELECT	death_date
								INTO		:ldtm_death_date
								FROM		INDIVIDUAL
								WHERE	individual_no = :ll_individual_no
								USING SQLCA;
								SQLCA.nf_handle_error('dw_current_eligibility', 'ue_checklist_itemchangeaccepted', 'select death_date from INDIVIDUAL')
														
								IF li_eligibility_find > 0 AND IsNull(ldtm_death_date) THEN
									IF tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.GetItemString(li_eligibility_find,'annuity_eligibility_reason_code') = 'INI' THEN
										// insert mail package
										IF IsValid(ids_reexamine_benefit_entitlement) THEN
											li_benefit_entitlement_rowcount = ids_reexamine_benefit_entitlement.RowCount()
										ELSE
											wf_reject_checklist_status_change(TRUE,'Eligibility Problem','Re-examination failed - call helpdesk',ll_checklist_no,ls_checklist_type_code)
											RETURN
										END IF
										
										ls_pre_1993_annuity_eligibility_flag = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.GetItemString(li_eligibility_find,'pre_1993_annuity_eligibility_flag')
										
										IF li_benefit_entitlement_rowcount = 0 and ls_pre_1993_annuity_eligibility_flag = 'N' THEN
											wf_reject_checklist_status_change(TRUE,'Eligibility Problem','Re-examination failed - call helpdesk',ll_checklist_no,ls_checklist_type_code)
											RETURN
										END IF
										
										// if eligible, then auto-complete
										adw_dw.SetItem(li_find, 'checklist_step_status_code', 'COA')
										adw_dw.SetItem(li_find, 'step_checked', 'Y')
										
										lb_commit = FALSE
										l_dwo = adw_dw.Object.checklist_step_status_code
										
										ls_status_assigned_method_code = 'A'
										uo_checklist.TRIGGER Event ue_checklist_step_status_changed(adw_dw,li_find,l_dwo,as_data,ls_status_assigned_method_code,ll_rtn)
										
										adw_dw.POST Event ItemChanged(li_find,l_dwo,'COA')
									ELSE
										wf_reject_checklist_status_change(TRUE,'Eligibility Problem','Active Eligibility does not have "initial" status - call helpdesk.',ll_checklist_no,ls_checklist_type_code)
										RETURN
									END IF
								ELSE
									// if not eligible, then auto-complete is not required
									adw_dw.SetItem(li_find, 'checklist_step_status_code', 'NRA')
									adw_dw.SetItem(li_find, 'step_checked', 'Y')
									
									lb_commit = FALSE
									
									l_dwo = adw_dw.Object.checklist_step_status_code
																		
									ls_status_assigned_method_code = 'A'
									uo_checklist.TRIGGER Event ue_checklist_step_status_changed(adw_dw,li_find,l_dwo,as_data,ls_status_assigned_method_code,ll_rtn)
										
									adw_dw.POST Event ItemChanged(li_find,l_dwo,'NRA')
								END IF
							ELSE
								// not found?
								wf_reject_checklist_status_change(TRUE,'Missing Step','Missing step 008 - call helpdesk.',ll_checklist_no,ls_checklist_type_code)
								RETURN
							END IF
						ELSE
							wf_reject_checklist_status_change(TRUE,'Eligibility Problem','Step "Confirm Eligibility" not completed automatically for "new list" injured worker - call helpdesk.',ll_checklist_no,ls_checklist_type_code)
							RETURN
						END IF
					ELSEIF li_selected_tab = CHANGED_LIST THEN
						// changed list
						IF li_find > 0 THEN
							IF as_data = 'COA' THEN
								li_eligibility_find = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.uf_find_row('annuity_eligibility_status_code','"A"')
								
								IF li_eligibility_find > 0 THEN
									// eligible
									li_rtn = wf_get_last_confirmed_data(ldtm_last_confirmed_date,ldtm_last_annuity_start_date,ldtm_last_annuity_end_date,ls_last_benefit_option_code,ldec_last_annuity_set_aside_percent)
									
									ldec_new_annuity_set_aside_percent = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.GetItemDecimal(li_eligibility_find,'annuity_set_aside_percent')
									ldtm_new_annuity_start_date = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.GetItemDateTime(li_eligibility_find,'annuity_start_date')
									ldtm_new_annuity_end_date = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.GetItemDateTime(li_eligibility_find,'annuity_end_date')
									ls_new_benefit_option_code = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.GetItemString(li_eligibility_find,'benefit_option_code')
									
									// if A/E is not changed by criteria below, then auto-complete step 8 as not required
									IF 	ldtm_last_annuity_start_date = ldtm_new_annuity_start_date &
										AND ldtm_last_annuity_end_date = ldtm_new_annuity_end_date &
										AND ls_last_benefit_option_code = ls_new_benefit_option_code &
										AND ldec_last_annuity_set_aside_percent = ldec_new_annuity_set_aside_percent THEN
										
										adw_dw.SetItem(li_find, 'checklist_step_status_code', 'NRA')
										adw_dw.SetItem(li_find, 'step_checked', 'Y')
										
										lb_commit = FALSE
																			
										l_dwo = adw_dw.Object.checklist_step_status_code
										
										ls_status_assigned_method_code = 'A'
										uo_checklist.TRIGGER Event ue_checklist_step_status_changed(adw_dw,li_find,l_dwo,as_data,ls_status_assigned_method_code,ll_rtn)
										
										adw_dw.POST Event ItemChanged(li_find,l_dwo,'NRA')
									ELSE
										// otherwise, commit step 7 only
										
										// reset some variables
										is_auto_calculation_step_status = ''
										ib_prevent_VBE_step_warning     = FALSE
										
										lb_commit = TRUE
									END IF
								ELSE
									// not active A/E
														
									// reset some variables
									is_auto_calculation_step_status = ''
									ib_prevent_VBE_step_warning     = FALSE
									
									lb_commit = TRUE
								END IF
							ELSE
								wf_reject_checklist_status_change(TRUE,'Eligibility Problem','Step "Confirm Eligibility" not completed automatically for "changed list" injured worker - call helpdesk.',ll_checklist_no,ls_checklist_type_code)
								RETURN
							END IF
						ELSE
							// not found?
							wf_reject_checklist_status_change(TRUE,'Missing Step','Missing step 008 - call helpdesk.',ll_checklist_no,ls_checklist_type_code)
							RETURN
						END IF
					ELSEIF li_selected_tab = PAYOUT_LIST THEN
						
						// annuity payout CAEP checklist does not have checklist step 008
						li_find = adw_dw.uf_find_row('checklist_step_type_code', '"009"')
						
						IF ib_prevent_VBE_step_warning THEN
							// if the 'VBE warning' window has been prevented from
							// being displayed, then all steps, up to and including the Annuity Calc step will be NRA
							adw_dw.SetItem(li_find, 'checklist_step_status_code',is_auto_calculation_step_status)
							adw_dw.SetItem(li_find, 'step_checked', 'Y')
							
							lb_commit = FALSE
																
							l_dwo = adw_dw.Object.checklist_step_status_code
							
							ls_status_assigned_method_code = 'A'
							uo_checklist.TRIGGER Event ue_checklist_step_status_changed(adw_dw,li_find,l_dwo,as_data,ls_status_assigned_method_code,ll_rtn)
							
							adw_dw.POST Event ItemChanged(li_find,l_dwo,is_auto_calculation_step_status)
						ELSE
							// the intent is not to prevent a warning, but to possibly trigger the completion of the Calculate Annuity checklist step
							lb_warning = FALSE
							wf_determine_calc_step_auto_completion(is_claim_role_code,ll_annuity_account_no,ll_annuity_eligibility_no,ll_individual_no,ll_claim_no, &
																ldtm_null,ldtm_null,lb_eligible,ll_checklist_no,ls_checklist_type_code,adw_dw,lb_warning, &
																lb_prevent_warning,is_auto_calculation_step_status,lb_commit)
						END IF
					END IF
				ELSE
					// surviving spouse
					// SS does not have checklist step 008
					li_find = adw_dw.uf_find_row('checklist_step_type_code', '"009"')
					
					IF ib_prevent_VBE_step_warning THEN
						// if the 'VBE warning' window has been prevented from
						// being displayed, then all steps, up to and including the Annuity Calc step will be NRA
						adw_dw.SetItem(li_find, 'checklist_step_status_code',is_auto_calculation_step_status)
						adw_dw.SetItem(li_find, 'step_checked', 'Y')
						
						lb_commit = FALSE
															
						l_dwo = adw_dw.Object.checklist_step_status_code
						
						ls_status_assigned_method_code = 'A'
						uo_checklist.TRIGGER Event ue_checklist_step_status_changed(adw_dw,li_find,l_dwo,as_data,ls_status_assigned_method_code,ll_rtn)
						
						adw_dw.POST Event ItemChanged(li_find,l_dwo,is_auto_calculation_step_status)
					ELSE					
						// the intent is not to prevent a warning, but to possibly trigger the completion of the Calculate Annuity checklist step
						lb_warning = FALSE
						wf_determine_calc_step_auto_completion(is_claim_role_code,ll_annuity_account_no,ll_annuity_eligibility_no,ll_individual_no,ll_claim_no, &
															ldtm_null,ldtm_null,lb_eligible,ll_checklist_no,ls_checklist_type_code,adw_dw,lb_warning, &
															lb_prevent_warning,is_auto_calculation_step_status,lb_commit)
						IF is_auto_calculation_step_status <> '' THEN
							// must trigger next step with step status indicated
							adw_dw.SetItem(li_find, 'checklist_step_status_code',is_auto_calculation_step_status)
							adw_dw.SetItem(li_find, 'step_checked', 'Y')
							
							lb_commit = FALSE
																
							l_dwo = adw_dw.Object.checklist_step_status_code
							
							ls_status_assigned_method_code = 'A'
							uo_checklist.TRIGGER Event ue_checklist_step_status_changed(adw_dw,li_find,l_dwo,as_data,ls_status_assigned_method_code,ll_rtn)
							
							adw_dw.POST Event ItemChanged(li_find,l_dwo,is_auto_calculation_step_status)
						END IF
							
					END IF
				END IF
				
			CASE '008'
				// Notify Injured Worker -- not for surviving spouses, so don't need to check!
				
				li_find = adw_dw.uf_find_row('checklist_step_type_code', '"009"')

				IF ib_prevent_VBE_step_warning THEN
					// if the 'VBE warning' window has been prevented from
					// being displayed, then the Annuity Calc step will be NRA
					adw_dw.SetItem(li_find, 'checklist_step_status_code', is_auto_calculation_step_status)
					adw_dw.SetItem(li_find, 'step_checked', 'Y')
					
					lb_commit = FALSE
														
					l_dwo = adw_dw.Object.checklist_step_status_code
					
					ls_status_assigned_method_code = 'A'
					uo_checklist.TRIGGER Event ue_checklist_step_status_changed(adw_dw,li_find,l_dwo,as_data,ls_status_assigned_method_code,ll_rtn)
					
					adw_dw.POST Event ItemChanged(li_find,l_dwo,is_auto_calculation_step_status)
				ELSE
					// the intent is not to prevent a warning, but to possibly trigger the completion of the Calculate Annuity checklist step
					lb_warning = FALSE
					wf_determine_calc_step_auto_completion(is_claim_role_code,ll_annuity_account_no,ll_annuity_eligibility_no,ll_individual_no,ll_claim_no, &
														ldtm_null,ldtm_null,lb_eligible,ll_checklist_no,ls_checklist_type_code,adw_dw,lb_warning, &
														lb_prevent_warning,is_auto_calculation_step_status,lb_commit)
					
					IF is_auto_calculation_step_status <> '' THEN
						// must trigger next step with step status indicated
						adw_dw.SetItem(li_find, 'checklist_step_status_code',is_auto_calculation_step_status)
						adw_dw.SetItem(li_find, 'step_checked', 'Y')
						
						lb_commit = FALSE
															
						l_dwo = adw_dw.Object.checklist_step_status_code
						
						ls_status_assigned_method_code = 'A'
						uo_checklist.TRIGGER Event ue_checklist_step_status_changed(adw_dw,li_find,l_dwo,as_data,ls_status_assigned_method_code,ll_rtn)
						
						adw_dw.POST Event ItemChanged(li_find,l_dwo,is_auto_calculation_step_status)
					END IF
				END IF
				
			CASE '009'
				// Calculate Account Balance
				
				// reset some variables
				is_auto_calculation_step_status = ''
				ib_prevent_VBE_step_warning     = FALSE
				
				// button not available after annuity has been calculated
				cb_annuity_benefit_inquiry.Enabled = FALSE
				
				lb_commit = TRUE
				
				
			CASE '024'
				// Checklist Completed
				
				// the SS or IW has had it's checklist completed
				// so re-retrieve appropriate checklist
				
				// if a single IW or SS has had checklist completed, then no rows left
				CHOOSE CASE is_annuity_eligibility_run_option_code
					CASE 'IW', 'SS'
						wf_clear_datawindows()
						IF il_annuity_payout_no = 0 THEN
							IF is_annuity_eligibility_run_option_code = 'IW' THEN
								MessageBox('No records','The checklist for the injured worker has been completed. There are no records left.')
							ELSE
								MessageBox('No records','The checklist for the surviving spouse has been completed. There are no records left.')
							END IF
						END IF
						uo_checklist.p_checklist.TriggerEvent(Clicked!)
					CASE 'IA'
						wf_retrieve_all_injured_workers ()
					CASE 'SA'
						wf_retrieve_all_surviving_spouses ()
				END CHOOSE
				idw_dw[7].object.checklist_status_code.protect = False  // idw_dw[7] is dw_checklist_master
				lb_commit = TRUE
				
		END CHOOSE
		
		// if there are no itemchanged events to post, so commit
		IF lb_commit = TRUE THEN
			// set filter to active status
			li_rtn = wf_set_current_AE_filter('annuity_eligibility_status_code = "A"')
						
			SQLCA.nf_commit_transaction()						
		END IF
		
	ELSE
		// not dw_checklist?
	END IF
		
	IF lb_commit THEN
		
		// if user is being asked to enter a comment
		IF ib_set_eligibility_comment THEN
			lstr_message.as_stringparm[1] = is_eligibility_comment_message
			lstr_message.as_mode = 'OPTIONAL'
			
			OpenWithParm(w_annuity_comment,lstr_message)
			
			ls_message = Message.StringParm
			IF trim(ls_message) <> '' THEN
				SQLCA.nf_begin_transaction()

				UPDATE ANNUITY_ELIGIBILITY
				SET    annuity_eligibility_comment = :ls_message
				WHERE  annuity_eligibility_no = :il_annuity_eligibility_for_comment
				USING SQLCA;
				SQLCA.nf_handle_error(ls_datawindow_control, 'ue_checklist_itemchangeaccepted', 'UPDATE ANNUITY_ELIGIBILITY...')
				
				SQLCA.nf_commit_transaction()
			END IF
		
			
			// reset instance variables
			is_eligibility_comment_message = ''
			ib_set_eligibility_comment = FALSE
		END IF
		
		// display message for automatic completion of calculation step as 'not allowed'
		IF ls_checklist_step_type_code = '009' THEN
			IF ib_auto_not_allow_calc_step THEN
				IF is_claim_role_code = 'C' THEN
					MessageBox('Checklist Step Completed','The "Calculate Annuity" checklist step has been automatically completed as "Not Allowed" for this injured worker.'&
															  + '~r~nThere is a post-1992 Annuity Payout that was processed prior to the implementation of the Annuity Calculation module.'&
															  + '~r~nYou must use the pre-established annuity calculation spreadsheet to calculate annuity benefits and determine subledger adjustments.',Information!)
				ELSE
					MessageBox('Checklist Step Completed','The "Calculate Annuity" checklist step has been automatically completed as "Not Allowed" for this surviving spouse.'&
															  + '~r~nThere is an Annuity Payout that was processed prior to the implementation of the Annuity Calculation module.'&
															  + '~r~nYou must use the pre-established annuity calculation spreadsheet to calculate annuity benefits and determine subledger adjustments.',Information!)
				END IF
				ib_auto_not_allow_calc_step = FALSE
			END IF
			
			IF as_data = 'COM' THEN
				cb_annuity_benefit_inquiry.TriggerEvent(Clicked!)
			END IF
		END IF
		
		ll_rows = wf_retrieve_confirm_eligibility(ll_individual_no,ll_claim_no,ls_checklist_type_code)
		
		IF ll_rows = 1 THEN
			// retrieve confirm datawindows
			// Only one row in 'confirm' a/e datawindow, so must trigger rowfocuschanged
			ldw_eligibility.Trigger Event RowFocusChanged(1)
		ELSE
			wf_scroll_to_individual(ldw_eligibility, ll_individual_no, ll_checklist_no, ls_checklist_type_code)
		END IF
	END IF
		
	SetRedraw(TRUE)
		
	ldwis = adw_dw.GetItemStatus(al_row,0,Primary!)

END IF
end event

event ue_checklist_buttonclicking;call super::ue_checklist_buttonclicking;BOOLEAN        lb_last_quarter_annuity_rate_exists
DATE           ldt_annuity_end_date
DATETIME       ldtm_last_confirmed,ldtm_annuity_start_date,ldtm_annuity_end_date, ldtm_current
DECIMAL        ldec_annuity_set_aside_percent
INTEGER			li_row, li_eligibility_row, li_rtn, li_count
LONG           ll_individual_no, ll_annuity_eligibility_no
STRING			ls_checklist_step_type_code, ls_message, ls_benefit_option_code
u_datawindow	ldw_eligibility
ULONG          lul_handle
W_SHEET			lw_sheet, lw_search_sheet
WINDOW			lw_active_sheet
u_ds				lds_select_claim, lds_inactive_unapplied_annuity_interest_rates
n_calc_annuity       lnv_calc_annuity
s_calc_reason_data   lstr_calc_reason_data



IF adw_dw.ClassName() = 'dw_checklist' THEN
	IF al_return = 1 THEN
		GOTO Label_End
	END IF
	
	wf_set_eligibility_datawindow(ldw_eligibility)
	
	// 2.370	Annuity eligibility work must not continue for an individual who has been merged 
	// since the last time the potential eligibility lists were retrieved.
	li_row = ldw_eligibility.GetRow()
	IF li_row > 0 THEN
		ll_individual_no = ldw_eligibility.GetItemNumber(li_row,'individual_no')
		li_count = inv_common_annuity.nf_check_individual_exists(ll_individual_no)
		IF li_count = 0 THEN
			ls_message = 'The individual number that you are working on no longer exists.'&
							+'~r~nThe individual may have been merged. The module will not be opened.'
			wf_individual_error(ls_message)
			al_return = 1
			GOTO Label_End
		END IF
			
	ELSE	
		MessageBox('No potential eligibility','There is no potential eligibility selected.',Exclamation!)
		al_return = 1
		GOTO Label_End
	END IF
	
	ls_checklist_step_type_code = adw_dw.GetItemString(row,'checklist_step_type_code')
	
	CHOOSE CASE ls_checklist_step_type_code
		CASE '001'
			// Identified
		CASE '002'
			// Search Individual		
		CASE '003'
			// Request Merge Individuals
		CASE '004'
			// Merge Individuals Completed
		CASE '005'
			// Confirm Birth Date
			
			// check that window is not already open
			li_rtn = inv_common_annuity.nf_check_for_open_window('w_individual_doclist','Individual Document List',lul_handle)
			IF li_rtn = 1 THEN
				BringWindowToTop(lul_handle)
				al_return = 1
				GOTO Label_End
			END IF
			
		CASE '006'
			// Verify Benefit Entitlement
			
			// check that window is not already open
			li_rtn = inv_common_annuity.nf_check_for_open_window('w_maintain_benefit_entitlement','Benefit Entitlement')
			IF li_rtn = 1 THEN
				al_return = 1
				GOTO Label_End
			END IF
			
			li_rtn = wf_set_current_AE_filter('annuity_eligibility_status_code = "P"')
			li_eligibility_row = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.GetRow()
			IF li_eligibility_row > 0 THEN
			ELSE
				MessageBox('No current eligibility','There is no current eligibility.',Exclamation!)
				al_return = 1
				GOTO Label_End
			END IF
			
		CASE '007'
			// Confirm Annuity Eligibility
		CASE '008'
			// Notify Injured Worker
		CASE '009'
			// Calculate Account Balance
			
			// check that window is not already open
			li_rtn = inv_common_annuity.nf_check_for_open_window('w_calc_annuity','Calculate Annuity')
			IF li_rtn = 1 THEN
				al_return = 1
				GOTO Label_End
			END IF
			
			IF il_annuity_payout_no > 0 THEN
				// do not allow calc annuity window to open if (1) the calculation is for payout and (2) last quarter's interest rate is not yet available
				lnv_calc_annuity = Create n_calc_annuity
				
				SELECT annuity_end_date
				INTO   :ldtm_annuity_end_date
				FROM   ANNUITY_PAYOUT      a
				JOIN   ANNUITY_ELIGIBILITY b ON a.annuity_eligibility_no = b.annuity_eligibility_no
				WHERE  a.annuity_payout_no = :il_annuity_payout_no
				USING SQLCA;
				SQLCA.nf_handle_error('w_confirm_annuity_eligibility','embedded SQL: SELECT annuity_end_date FROM ANNUITY_PAYOUT, ANNUITY_ELIGIBILITY...','uo_checklist.ue_checklist_buttonclicking')
				
				ldt_annuity_end_date = Date(ldtm_annuity_end_date)
				
				IF IsNull(ldt_annuity_end_date) THEN
					// do nothing, the person has been found ineligible
				ELSE
					ldtm_current = f_server_datetime()
					IF ldtm_annuity_end_date <= ldtm_current THEN
						lb_last_quarter_annuity_rate_exists = lnv_calc_annuity.nf_last_quarter_interest_rate_exists(ldt_annuity_end_date)
						IF lb_last_quarter_annuity_rate_exists = FALSE THEN
							MessageBox('Calculation for Annuity Payout','The annuity interest rate for the last quarter is not yet available.'&
																				+'~r~nThis annuity account cannot have the payout calculation completed until the interest rate is available.',Exclamation!)
							al_return = 1
							GOTO Label_End
						END IF
					ELSE
						// if annuity end date is in future, 
						// then allow calculation module to be opened
						// it will force the selection of 'eligibility change - payout'
					END IF
				END IF
			END IF
			
			
		CASE '024'
			// Checklist Completed		
		CASE ELSE
			
	END CHOOSE
END IF

al_return = 0

Label_End:
end event

event ue_checklist_rowfocuschanged;call super::ue_checklist_rowfocuschanged;INTEGER					li_rtn, li_benefit_entitlement_rowcount
LONG						ll_confirm_eligibility_row, ll_annuity_account_no, ll_claim_no
STRING					ls_datawindow_control, ls_checklist_step_type_code
U_DATAWINDOW		ldw_eligibility


ldw_eligibility = Create u_datawindow

ls_datawindow_control = adw_dw.ClassName()

IF ls_datawindow_control = 'dw_checklist' THEN
	
	ls_checklist_step_type_code = adw_dw.GetItemString(currentrow,'checklist_step_type_code')	

	CHOOSE CASE ls_checklist_step_type_code
		CASE '006'
			
			wf_set_eligibility_datawindow(ldw_eligibility)
			ll_confirm_eligibility_row = ldw_eligibility.GetRow()
			
			IF ll_confirm_eligibility_row > 0 THEN
				DESTROY ids_reexamine_benefit_entitlement
				
				ll_annuity_account_no = ldw_eligibility.GetItemNumber(ll_confirm_eligibility_row,'annuity_account_no')
				
				ids_reexamine_benefit_entitlement = Create U_DS
				
				IF is_claim_role_code = 'C' THEN					
					ids_reexamine_benefit_entitlement.DataObject = 'ds_reexamine_benefit_entitlement_IW'
					li_rtn = ids_reexamine_benefit_entitlement.SetTransObject(SQLCA)
					
					li_benefit_entitlement_rowcount = ids_reexamine_benefit_entitlement.Retrieve(ll_annuity_account_no)
					SQLCA.nf_handle_error('dw_checklist', 'ue_checklist_rowfocuschanged', 'ids_reexamine_benefit_entitlement.Retrieve')
										
				ELSE
					ll_claim_no = ldw_eligibility.GetItemNumber(ll_confirm_eligibility_row,'claim_no')
					
					ids_reexamine_benefit_entitlement.DataObject = 'ds_reexamine_benefit_entitlement_SS'
					li_rtn = ids_reexamine_benefit_entitlement.SetTransObject(SQLCA)
					
					li_benefit_entitlement_rowcount = ids_reexamine_benefit_entitlement.Retrieve(ll_annuity_account_no,ll_claim_no)
					SQLCA.nf_handle_error('dw_checklist', 'ue_checklist_rowfocuschanged', 'ids_reexamine_benefit_entitlement.Retrieve')
					
				END IF
				
				IF li_benefit_entitlement_rowcount < 0 THEN
					SignalError(-1,'The re-examination of benefit entitlement failed unexpectedly. Call the HELPDESK.')
				END IF
			END IF
	END CHOOSE
END IF


end event

event ue_checklist_note_save_btn_clicking;call super::ue_checklist_note_save_btn_clicking;INTEGER        li_count, li_row
LONG           ll_individual_no
STRING         ls_message
U_DATAWINDOW   ldw_eligibility

wf_set_eligibility_datawindow(ldw_eligibility)

// 2.370	Annuity eligibility work must not continue for an individual who has been merged 
// since the last time the potential eligibility lists were retrieved.
li_row = ldw_eligibility.GetRow()
IF li_row > 0 THEN
	ll_individual_no = ldw_eligibility.GetItemNumber(li_row,'individual_no')
	
	li_count = inv_common_annuity.nf_check_individual_exists(ll_individual_no)
	IF li_count = 0 THEN
		ib_individual_does_not_exist = TRUE
		ls_message = 'The individual number that you are working on no longer exists.'&
						+'~r~nThe individual may have been merged. The change to the note will be canceled.'
		wf_individual_error(ls_message)
		
		// reset instance variable
		ib_individual_does_not_exist = FALSE
		
		// cancel note save
		al_return = -1
	END IF
ELSE
	ib_individual_does_not_exist = TRUE
	ls_message = 'The individual number that you are working on no longer exists.'&
					+'~r~nThe individual may have been merged. The change to the note will be canceled.'
	wf_individual_error(ls_message)
	
	// reset instance variable
	ib_individual_does_not_exist = FALSE

	// cancel note save
	al_return = -1
END IF
end event

event ue_checklist_step_status_changed;call super::ue_checklist_step_status_changed;INTEGER         li_rtn
DATAWINDOWCHILD ldwc_child

IF as_status_assigned_method_code = '' THEN
	
	li_rtn = uo_checklist.tab_checklist.tabpage_checklist.dw_checklist.GetChild('checklist_step_status_code',ldwc_child)
	
	as_status_assigned_method_code = ldwc_child.GetItemString(ldwc_child.GetRow(),'status_assigned_method_code')

END IF
end event

