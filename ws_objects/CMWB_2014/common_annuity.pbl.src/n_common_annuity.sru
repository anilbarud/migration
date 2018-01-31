$PBExportHeader$n_common_annuity.sru
forward
global type n_common_annuity from nonvisualobject
end type
end forward

global type n_common_annuity from nonvisualobject
end type
global n_common_annuity n_common_annuity

type variables
window iwi_window_parent

end variables

forward prototypes
public function date nf_first_day_of_next_month (date adt_date)
public subroutine nf_set_window_parent (window awi_window_parent)
public function long nf_get_next_annuity_account_no (long al_number)
public function long nf_get_next_annuity_eligibility_no (long al_number)
public function long nf_get_related_checklist_no (long al_annuity_account_no, string as_checklist_type_code)
public function integer nf_multiple_annuity_accounts (long al_annuity_account_no)
public subroutine nf_resize_window_array (integer ai_array_element)
public subroutine nf_update_subscriber_no (long al_annuity_account_no, long al_checklist_subscriber_no)
public function long nf_get_new_checklist_no (long al_checklist_subscriber_no)
public function long nf_insert_annuity_account (long al_individual_no, long al_claim_no, string as_claim_role_code, long al_checklist_subscriber_no, string as_converted_flag)
public subroutine nf_open_correspondence (string as_search_type, string as_enable, string as_given_names, string as_last_name, datetime adtm_birth_date, long al_sin_no, long al_medicare_no, long al_claim_no)
public subroutine nf_open_individual_search (string as_search_type, string as_enable, string as_given_names, string as_last_name, datetime adtm_birth_date, long al_sin_no, long al_medicare_no, long al_claim_no)
public function long nf_get_ann_acct_checklist_subscriber_no (long al_annuity_account_no)
public subroutine nf_open_document_list (string as_open_window_name, string as_claim_role_code, string as_module_code, long al_claim_no, long al_individual_no)
public subroutine nf_close_handle_array (long al_window_handle)
public subroutine nf_get_annuity_percentage (string as_claim_role_code, datetime adtm_annuity_end_date, string as_benefit_option_code, ref decimal adec_annuity_set_aside_percent, ref integer ai_annuity_set_aside_percent_no)
public subroutine nf_get_aq_claim (long al_individual_no, ref long al_claim_no, ref integer al_opening_no)
public subroutine nf_post_qualification_ben_entitlement (long al_annuity_eligibility_no, datetime adtm_new_annuity_start_date, datetime adtm_new_annuity_end_date, ref long al_benefit_entitlement_count)
public subroutine nf_post_81_setasides_for_ss (long al_individual_no, long al_claim_no, ref long al_txn_count, ref decimal adec_total_net_payment_amount)
public subroutine nf_post_92_setasides_for_iw (long al_individual_no, ref long al_txn_count, ref decimal adec_total_net_payment_amount)
public function boolean nf_check_for_annuity_calculation (long al_annuity_account_no, long al_annuity_eligibility_no)
public function integer nf_check_individual_exists (long al_individual_no)
public function integer nf_check_for_open_window (string as_window_name, string as_window_message)
public function integer nf_check_for_open_window (string as_window_name, string as_window_message, ref unsignedlong aul_handle)
public subroutine nf_open_verify_benefit_entitlement (string as_open_window_name, long al_annuity_account_no, long al_annuity_eligibility_no, long al_action_code, long al_claim_no, long al_individual_no, long al_annuity_payout_no, string as_claim_role_code)
public subroutine nf_select_next_available_checklist_step (ref n_checklist anv_checklist, ref u_checklist_datawindow adw_checklist, long al_checklist_no, string as_checklist_type_code)
public subroutine nf_get_annuity_payout_claim (long al_individual_no, long al_annuity_payout_no, ref long al_claim_no)
public subroutine nf_open_cae_for_payout (string as_open_window_name, long al_claim_no, long al_individual_no, long al_annuity_payout_no, string as_claim_role_code)
public subroutine nf_get_ap_claim (long al_annuity_payout_no, long al_individual_no, ref long al_claim_no)
public function integer nf_validate_annuity_status (long al_annuity_payout_no, string as_annuity_status_code, string as_annuity_status_reason_code)
public subroutine nf_open_event_log (string as_open_window_name, string as_claim_role_code, long al_claim_no, long al_individual_no, date adt_accident_date, string as_last_name, string as_given_names, string as_event_category_code, string as_event_type_code, string as_event_specific_code, string as_allow_parameter_change, string as_add_new_event, string as_message)
public subroutine nf_open_overpayment_list (string as_open_window_name, long al_annuity_payout_no, long al_individual_no)
public function integer nf_validate_annuity_payout_for_cae (string as_annuity_eligibility_run_option_code, long al_individual_no, long al_claim_no)
public function integer nf_individual_has_other_annuity_accounts (long al_individual_no, long al_claim_no)
public function boolean nf_determine_request_payout_status (long al_annuity_payout_no)
public subroutine nf_make_menu_item_invisible (ref window aw_window, string as_menu_item, string as_sub_menu_item)
public subroutine nf_injured_worker_sub_ledger_balance (long al_individual_no, ref decimal adec_total_net_payment_amount)
public subroutine nf_surviving_spouse_sub_ledger_balance (long al_individual_no, long al_claim_no, ref decimal adec_total_net_payment_amount)
public function long nf_get_annuity_account (long al_claim_no, long al_individual_no)
public subroutine nf_open_confirm_contract_reminder (long al_annuity_payout_no, long al_individual_no, long al_claim_no, string as_annuity_role_code)
public function long nf_get_checklist_annuity_eligibility_no (long al_checklist_no)
public function integer nf_get_annuity_end_date (long al_individual_no, string as_type, ref string as_message, ref datetime adtm_annuity_end_date, ref datetime adtm_annuity_eligibility_end_date_used, datetime adtm_birth_date, datetime adtm_death_date)
public subroutine nf_open_calculate_annuity (string as_open_window_name, long al_annuity_account_no, long al_annuity_eligibility_no, long al_payout_no, datetime adtm_annuity_start_date, datetime adtm_annuity_end_date, datetime adtm_annuity_eligibility_end_date_used, long ai_annuity_set_aside_percent_no, integer ai_annuity_calculation_needed)
public function long nf_insert_annuity_eligibility (long al_annuity_account_no, string as_annuity_eligibility_status_code, datetime adtm_confirmed_date, string as_confirmed_by_user_id, datetime adtm_annuity_start_date, datetime adtm_annuity_eligibility_end_date_used, datetime adtm_annuity_end_date, string as_benefit_option_code, integer al_annuity_set_aside_percent_no, decimal adec_annuity_set_aside_percent, string as_annuity_eligibility_reason_code, string as_annuity_eligibility_comment, long al_confirm_annuity_elig_checklist_no, integer al_verify_ben_entitlement_checklist_no, string as_pre_1993_annuity_eligibility_flag, string as_converted_flag)
end prototypes

public function date nf_first_day_of_next_month (date adt_date);// nf_first_day_of_next_month 
// 
Long   ll_current_year, ll_current_month, ll_next_year, ll_next_month 
String ls_next_month, ls_first_day_of_next_month 
Date   ldt_first_day_of_next_month 

ll_current_year = Year(Date(adt_date))
ll_current_month = Month(Date(adt_date))

IF ll_current_month = 12 THEN
	ll_next_year = ll_current_year + 1
	ll_next_month = 1
ELSE
	ll_next_year = ll_current_year
	ll_next_month = ll_current_month + 1
END IF

IF ll_next_month < 10 THEN
	ls_next_month = '0' + String(ll_next_month)
ELSE
	ls_next_month = String(ll_next_month)
END IF

ls_first_day_of_next_month = String(ll_next_year) +'-' + ls_next_month + '-01'

ldt_first_day_of_next_month = Date(ls_first_day_of_next_month)

RETURN ldt_first_day_of_next_month

end function

public subroutine nf_set_window_parent (window awi_window_parent);// nf_set_window_parent - stores a reference to the parent window in an instance variable
//
iwi_window_parent = awi_window_parent

end subroutine

public function long nf_get_next_annuity_account_no (long al_number);// nf_get_next_annuity_account_no - To ensure that we get the next number without, Update the Last_Claim_Txn_no table incrementing the  
//                                  last_txn_no by 1  (This will lock it so no one else can get in). Then, read it back 
//
Long ll_annuity_account_no 

UPDATE Last_Annuity_Account_No 
   SET last_annuity_account_no = last_annuity_account_no + :al_number 
 USING SQLCA ; 

SQLCA.nf_handle_error("n_common_annuity", "", "nf_get_next_annuity_account_no - Update Last_Annuity_Account_No")

IF SQLCA.SQLNRows = 1 THEN 
	// If update was successful (ie. SQLNRows would equal 1), read back the identifier
	SELECT last_annuity_account_no 
	  INTO :ll_annuity_account_no 
	  FROM Last_Annuity_Account_No 
	 USING SQLCA ; 
	
	SQLCA.nf_handle_error("n_common_annuity", "", "nf_get_next_annuity_account_no - SELECT last_annuity_account_no FROM Last_Annuity_Account_No")
ELSE
	// If anything other than 1 record found, display error
	MessageBox("Data Integrity Error", string(SQLCA.SQLNRows) + " record(s) found in Last_Annuity_Account_No~r~nPlease call the help desk",Exclamation!)
	SQLCA.nf_rollback_transaction()
	IF SQLCA.SQLCode <> 0 THEN
		Error.Text = "Error during rollback of Last_Annuity_Account_No in function nf_get_next_annuity_account_no"
		Error.WindowMenu=""
		Error.Object=""
		Error.ObjectEvent="nf_get_next_annuity_account_no"
		SignalError()
	END IF		
	RETURN -1
END IF

RETURN ll_annuity_account_no

end function

public function long nf_get_next_annuity_eligibility_no (long al_number);// nf_get_next_annuity_eligibility_no 
//
Long ll_annuity_eligibility_no

// To ensure that we get the next number without, Update the Last_Claim_Txn_no table incrementing the  
// last_txn_no by 1  (This will lock it so no one else can get in). Then, read it back                 
UPDATE Last_Annuity_Eligibility_No
   SET last_annuity_eligibility_no = last_annuity_eligibility_no + :al_number 
 USING SQLCA ; 

SQLCA.nf_handle_error("n_common_annuity", "", "nf_get_next_annuity_eligibility_no - Update Last_Annuity_Eligibility_No")

IF SQLCA.SQLNRows = 1 THEN 
	// If update was successful (ie. SQLNRows would equal 1), read back the identifier
	SELECT last_annuity_eligibility_no
	  INTO :ll_annuity_eligibility_no
	  FROM Last_Annuity_Eligibility_No 
	 USING SQLCA ; 
	
	SQLCA.nf_handle_error("n_common_annuity", "", "nf_get_next_annuity_eligibility_no - SELECT Last_Annuity_Eligibility_No") 
ELSE
	// If anything other than 1 record found, display error
	MessageBox("Data Integrity Error", string(SQLCA.SQLNRows) + " record(s) found in Last_Annuity_Eligibility_No~r~nPlease call the help desk",Exclamation!)
	SQLCA.nf_rollback_transaction()
	IF SQLCA.SQLCode <> 0 THEN
		Error.Text = "Error during rollback of Last_Annuity_Eligibility_No in function nf_get_next_annuity_eligibility_no"
		Error.WindowMenu = ""
		Error.Object = ""
		Error.ObjectEvent = "nf_get_next_annuity_eligibility_no"
		SignalError()
	END IF		
	RETURN -1
END IF 

RETURN ll_annuity_eligibility_no

end function

public function long nf_get_related_checklist_no (long al_annuity_account_no, string as_checklist_type_code);// nf_get_related_checklist_no 
// 
Long ll_related_checklist_no 

SELECT IsNull(e.related_checklist_no, 0) 
  INTO :ll_related_checklist_no 
  FROM      ANNUITY_ACCOUNT b 
       JOIN SUBSCRIBER_CHECKLIST_XREF c ON b.checklist_subscriber_no = c.checklist_subscriber_no 
       JOIN CHECKLIST_SUBSCRIBER d      ON c.checklist_subscriber_no = d.checklist_subscriber_no 
       JOIN CHECKLIST e                 ON e.checklist_no = c.checklist_no 
 WHERE d.checklist_subscriber_type_code = 'ANN' 
   AND e.checklist_no = e.related_checklist_no 
   AND b.annuity_account_no = :al_annuity_account_no  
 USING SQLCA ; 

SQLCA.nf_handle_error("n_common_annuity", "", "nf_get_related_checklist_no - SELECT IsNull(e.related_checklist_no, 0) FROM ANNUITY_ACCOUNT b JOIN SUBSCRIBER_CHECKLIST_XREF c...")

RETURN ll_related_checklist_no 

end function

public function integer nf_multiple_annuity_accounts (long al_annuity_account_no);// nf_multiple_annuity_accounts - if function returns > 0, then multiple annuity accounts exist
//
Integer li_multiple_annuity_accounts
Long    ll_individual_no

SELECT individual_no 
  INTO :ll_individual_no 
  FROM ANNUITY_ACCOUNT 
 WHERE annuity_account_no = :al_annuity_account_no 
 USING SQLCA ; 

SQLCA.nf_handle_error("n_common_annuity", "", "nf_multiple_annuity_accounts - SELECT individual_no FROM ANNUITY_ACCOUNT")

SELECT Count(*) 
  INTO :li_multiple_annuity_accounts 
  FROM ANNUITY_ACCOUNT 
 WHERE individual_no = :ll_individual_no 
   AND annuity_account_no <> :al_annuity_account_no 
 USING SQLCA ; 

SQLCA.nf_handle_error("n_common_annuity", "", "nf_multiple_annuity_accounts - SELECT Count(*) FROM ANNUITY_ACCOUNT") 

RETURN li_multiple_annuity_accounts

end function

public subroutine nf_resize_window_array (integer ai_array_element);// nf_resize_window_array 
// 
Boolean lb_reached_skipped_element 
Integer li_upperbound, li_counter 
s_window_array lstr_hold_array[], lstr_array[]

// hold value of 
lstr_hold_array = gstr_window_array

// clear global array
gstr_window_array = lstr_array

li_upperbound = UpperBound(lstr_hold_array)

FOR li_counter = 1 TO li_upperbound
	IF li_counter = ai_array_element THEN
		lb_reached_skipped_element = TRUE
	END IF
	IF lb_reached_skipped_element THEN
		IF li_counter <> li_upperbound THEN
			gstr_window_array[li_counter].window_element = lstr_hold_array[li_counter+1].window_element
			gstr_window_array[li_counter].handle_element = lstr_hold_array[li_counter+1].handle_element
		ELSE
			EXIT
		END IF
	ELSE
		gstr_window_array[li_counter].window_element = lstr_hold_array[li_counter].window_element
		gstr_window_array[li_counter].handle_element = lstr_hold_array[li_counter].handle_element
	END IF
NEXT

end subroutine

public subroutine nf_update_subscriber_no (long al_annuity_account_no, long al_checklist_subscriber_no);// nf_update_subscriber_no 
// 
UPDATE ANNUITY_ACCOUNT 
   SET checklist_subscriber_no = :al_checklist_subscriber_no 
 WHERE annuity_account_no = :al_annuity_account_no 
 USING SQLCA ; 

SQLCA.nf_handle_error('n_common_annuity', '', 'nf_update_subscriber_no - UPDATE ANNUITY_ACCOUNT...') 

end subroutine

public function long nf_get_new_checklist_no (long al_checklist_subscriber_no);// nf_get_new_checklist_no 
// 
Long ll_new_checklist_no

SELECT checklist_no 
  INTO :ll_new_checklist_no 
  FROM      CHECKLIST_SUBSCRIBER a 
       JOIN SUBSCRIBER_CHECKLIST_XREF b ON a.checklist_subscriber_no = b.checklist_subscriber_no 
 WHERE a.checklist_subscriber_type_code = 'ANN' 
   AND b.checklist_subscriber_no = :al_checklist_subscriber_no 
 USING SQLCA ; 

SQLCA.nf_handle_error('n_common_annuity', '', 'nf_get_new_checklist_no - SELECT checklist_no FROM CHECKLIST_SUBSCRIBER a')

RETURN ll_new_checklist_no

end function

public function long nf_insert_annuity_account (long al_individual_no, long al_claim_no, string as_claim_role_code, long al_checklist_subscriber_no, string as_converted_flag);// nf_insert_annuity_account 
// 
Long ll_annuity_account_no

SELECT Count(*) 
  INTO :ll_annuity_account_no 
  FROM ANNUITY_ACCOUNT 
 WHERE individual_no = :al_individual_no 
   AND claim_no = :al_claim_no 
 USING SQLCA ; 

SQLCA.nf_handle_error("n_common_annuity", "", "nf_insert_annuity_account - SELECT Count(*) FROM ANNUITY_ACCOUNT") 

IF ll_annuity_account_no > 0 THEN
	SignalError(-1, 'The module was trying to create an annuity account, but there is already one for this individual ' + String(al_individual_no) + '.')
END IF

// add one ANNUITY_ACCOUNT record
// NOTE that this might be altered to handle multiple records to accommodate 'all injured workers' run option
ll_annuity_account_no = nf_get_next_annuity_account_no(1)

INSERT ANNUITY_ACCOUNT (annuity_account_no, individual_no, claim_no, claim_role_code, checklist_subscriber_no, converted_flag) 
                SELECT :ll_annuity_account_no, :al_individual_no, :al_claim_no, :as_claim_role_code, :al_checklist_subscriber_no, :as_converted_flag 
 USING SQLCA ; 

SQLCA.nf_handle_error("n_common_annuity", "", "nf_insert_annuity_account - INSERT ANNUITY_ACCOUNT")

RETURN ll_annuity_account_no

end function

public subroutine nf_open_correspondence (string as_search_type, string as_enable, string as_given_names, string as_last_name, datetime adtm_birth_date, long al_sin_no, long al_medicare_no, long al_claim_no);// nf_open_correspondence 
// 
s_window_message lstr_message
s_claim_search_defaults	lstr_claim_search_defaults
w_sheet lw_search_sheet
window  lw_to_open
m_frame lm_frame
m_cmwb  lm_cmwb

lstr_claim_search_defaults.given_names = as_given_names
lstr_claim_search_defaults.last_name = as_last_name
lstr_claim_search_defaults.birth_date = adtm_birth_date
lstr_claim_search_defaults.sin_no = al_sin_no
lstr_claim_search_defaults.medicare_no = al_medicare_no
lstr_claim_search_defaults.claim_no = al_claim_no

lstr_message.as_stringparm[1] = as_search_type
lstr_message.as_stringparm[2] = as_enable
lstr_message.apo_powerobjectparm[1] = lstr_claim_search_defaults

lm_frame = w_frame.MenuID
lm_frame.m_workbench.m_worksheet.TriggerEvent(Clicked!)

//m_cmwb.m_workbench.m_worksheet.TriggerEvent(Clicked!)
	
lw_search_sheet = w_frame.GetFirstSheet()

IF IsValid(lw_search_sheet) THEN
	lw_search_sheet.uo_claim_search.uf_set_search_type(as_search_type)
	lw_search_sheet.uo_claim_search.uf_protect_searchtype(as_enable)
	lw_search_sheet.uo_claim_search.uf_set_search_defaults(lstr_claim_search_defaults, as_search_type)
	//	m_cmwb.mf_open_window('m_correspondence')
	lm_cmwb = lw_search_sheet.MenuID

	lm_cmwb.m_tools.m_correspondence.TriggerEvent(Clicked!)
END IF

end subroutine

public subroutine nf_open_individual_search (string as_search_type, string as_enable, string as_given_names, string as_last_name, datetime adtm_birth_date, long al_sin_no, long al_medicare_no, long al_claim_no);// nf_open_individual_search 
// 
s_claim_search_defaults lstr_claim_search_defaults
s_window_message lstr_message
window  lw_to_open
w_sheet lw_search_sheet
m_frame lm_frame

// Search Individual
lstr_claim_search_defaults.given_names = as_given_names
lstr_claim_search_defaults.last_name = as_last_name
lstr_claim_search_defaults.birth_date = adtm_birth_date
lstr_claim_search_defaults.sin_no = al_sin_no
lstr_claim_search_defaults.medicare_no = al_medicare_no
lstr_claim_search_defaults.claim_no = al_claim_no
	
lstr_message.as_stringparm[1] = as_search_type
lstr_message.as_stringparm[2] = as_enable
lstr_message.apo_powerobjectparm[1] = lstr_claim_search_defaults
lstr_message.awi_parent_window = iwi_window_parent

lm_frame = w_frame.MenuId
lm_frame.m_workbench.m_worksheet.TriggerEvent(Clicked!)

lw_search_sheet = w_frame.GetFirstSheet()

IF IsValid(lw_search_sheet) THEN
	lw_search_sheet.uo_claim_search.uf_set_search_type(as_search_type)
	lw_search_sheet.uo_claim_search.uf_protect_searchtype(as_enable)
	lw_search_sheet.uo_claim_search.uf_set_search_defaults(lstr_claim_search_defaults, as_search_type)
END IF

end subroutine

public function long nf_get_ann_acct_checklist_subscriber_no (long al_annuity_account_no);// nf_get_ann_acct_checklist_subscriber 
// 
Long ll_checklist_subscriber_no

SELECT IsNull(checklist_subscriber_no, 0) 
  INTO :ll_checklist_subscriber_no 
  FROM ANNUITY_ACCOUNT 
 WHERE annuity_account_no = :al_annuity_account_no 
 USING SQLCA ; 

SQLCA.nf_handle_error("n_common_annuity", "", 'nf_get_ann_acct_checklist_subscriber_no - select checklist_subscriber_no from ANNUITY_ACCOUNT') 

RETURN ll_checklist_subscriber_no 

end function

public subroutine nf_open_document_list (string as_open_window_name, string as_claim_role_code, string as_module_code, long al_claim_no, long al_individual_no);// nf_open_document_list 
// 
s_window_message lstr_message 
window lw_to_open 

// open - individual document list - confirm birth date 
lstr_message.as_stringparm[1] = as_claim_role_code 
lstr_message.as_stringparm[2] = as_module_code 
lstr_message.al_doubleparm[1] = al_claim_no 
lstr_message.al_doubleparm[2] = al_individual_no 

OpenWithParm(lw_to_open, lstr_message, as_open_window_name) 

end subroutine

public subroutine nf_close_handle_array (long al_window_handle);// nf_close_handle_array 
// 
Integer li_window_element, li_counter, li_upperbound

li_upperbound = UpperBound(gstr_window_array)

FOR li_counter = 1 TO li_upperbound
	IF gstr_window_array[li_counter].handle_element = al_window_handle THEN
		li_window_element = li_counter
		EXIT
	END IF
NEXT

nf_resize_window_array(li_window_element)

end subroutine

public subroutine nf_get_annuity_percentage (string as_claim_role_code, datetime adtm_annuity_end_date, string as_benefit_option_code, ref decimal adec_annuity_set_aside_percent, ref integer ai_annuity_set_aside_percent_no);// nf_get_annuity_percentage
// 
//The value of the annuity_set_aside_percent will be determined by referencing the Annuity_Set_Aside_Percent look-up table on the CLAIM database.
//
//For an injured worker eligible for annuity benefits (i.e. claim_role_code = 'C'), the annuity_end_date will be compared to
//the annuity_payout_eligibility date fields to find the correct percentage value (for the active record and claimant role code). 
//There are two ways that the date values may exist in the look-up table that must be used to determine the correct percentage:
//annuity_payout_eligible_from_date <= end date >= annuity_payout_eligible_to_date OR
//annuity_payout_eligible_from_date <= end date and annuity_payout_eligible_to_date is null (effective into the future)
//
//Note that because other business rules do not allow the annuity end date to be before the annuity end date and the annuity start date 
//cannot be before January 1, 1993, records will NOT be included in the look-up table to pre-date payout eligibility of January 1, 1993.  The 
//effective_from_date and effective_to_date are included for reference purposes only and will not be used in the application to determine 
//the percentage set aside value.
//
//For a Surviving Spouse eligibile for annuity benefits (i.e. claim_role_code = 'SS'), the Benefit Option will be used to determine 
//the correct percentage value for the annuity_set_aside_percent field.  The benefit option value for the Eligibility record will be used to 
//select the active Annuity_Set_Aside_Percent record for claim_role_code = 'SS' to determine the correct percentage value for 
//the annuity_set_aside_percent to be updated on the ANNUITY_ELIGIBILITY record.

// initialize variables
adec_annuity_set_aside_percent = 0.00
ai_annuity_set_aside_percent_no = 0

IF as_claim_role_code = 'C' THEN
	SELECT annuity_set_aside_percent, annuity_set_aside_percent_no
	  INTO :adec_annuity_set_aside_percent, :ai_annuity_set_aside_percent_no
	  FROM Annuity_Set_Aside_Percent 
    WHERE benefit_option_code = :as_benefit_option_code
      AND :adtm_annuity_end_date >= annuity_payout_eligible_from_date
      AND :adtm_annuity_end_date <= annuity_payout_eligible_to_date
      AND claim_role_code = :as_claim_role_code
      AND annuity_payout_eligible_to_date IS NOT NULL
    USING SQLCA ; 
	
	SQLCA.nf_handle_error('n_common_annuity', '', 'nf_get_annuity_percentage - SELECT annuity_set_aside_percent (1)')
	
	IF ai_annuity_set_aside_percent_no = 0 OR IsNull(ai_annuity_set_aside_percent_no) THEN
      SELECT annuity_set_aside_percent, annuity_set_aside_percent_no
        INTO :adec_annuity_set_aside_percent , :ai_annuity_set_aside_percent_no
        FROM Annuity_Set_Aside_Percent
       WHERE benefit_option_code = :as_benefit_option_code 
         AND :adtm_annuity_end_date >= annuity_payout_eligible_from_date
         AND claim_role_code = :as_claim_role_code
         AND annuity_payout_eligible_to_date IS NULL 
       USING SQLCA ; 
		
		SQLCA.nf_handle_error('n_common_annuity', '', 'nf_get_annuity_percentage - SELECT annuity_set_aside_percent (2)')
	END IF
ELSE
   SELECT annuity_set_aside_percent, annuity_set_aside_percent_no
     INTO :adec_annuity_set_aside_percent , :ai_annuity_set_aside_percent_no
     FROM Annuity_Set_Aside_Percent
    WHERE benefit_option_code = :as_benefit_option_code
      AND claim_role_code = :as_claim_role_code
    USING SQLCA ; 
	
	SQLCA.nf_handle_error('n_common_annuity', '', 'nf_get_annuity_percentage - SELECT annuity_set_aside_percent (3)')
END IF

end subroutine

public subroutine nf_get_aq_claim (long al_individual_no, ref long al_claim_no, ref integer al_opening_no);// nf_get_aq_claim - retrieves the claim with the most recent opening accident recurrence date
//                   this claim is used to open the correspondence window (for ANNQUAL letters)
//                   or is used to automatically create AQ mail packages
// 
SELECT	TOP 1 a.claim_no , b.opening_no
INTO		:al_claim_no , :al_opening_no
FROM		CLAIM a
JOIN		OPENING b ON a.claim_no = b.claim_no
JOIN        Claim_Role_Opening_Type_Xref c on b.opening_type_code = c.opening_type_code
JOIN		(	SELECT	Max(e.accident_recurrence_date) 'max_accident_recurrence_date'
				FROM		CLAIM   d
				JOIN		OPENING e ON d.claim_no = e.claim_no
				JOIN		Claim_Role_Opening_Type_Xref f ON e.opening_type_code = f.opening_type_code
				WHERE		d.individual_no = :al_individual_no
				AND		f.claim_role_code = 'C'
				AND		f.active_flag = 'Y'
				AND		f.annuity_eligibility_flag = 'Y'
				AND ( d.claim_status_code = 'A'
             OR ( d.claim_status_code = 'F' and d.claim_status_type_code = '01')
             OR ( d.claim_status_code = 'F' and d.claim_status_type_code = '02')
             OR ( d.claim_status_code = 'F' and d.claim_status_type_code = '03')
             OR ( d.claim_status_code = 'F' and d.claim_status_type_code = '04')
             OR ( d.claim_status_code = 'F' and d.claim_status_type_code = '17'))) x ON b.accident_recurrence_date = x.max_accident_recurrence_date
WHERE	   a.individual_no = :al_individual_no
AND      c.claim_role_code = 'C'
and      c.annuity_eligibility_flag = 'Y'
AND	   c.active_flag = 'Y'
AND  ( a.claim_status_code = 'A'
  OR ( a.claim_status_code = 'F' and a.claim_status_type_code = '01')
  OR ( a.claim_status_code = 'F' and a.claim_status_type_code = '02')
  OR ( a.claim_status_code = 'F' and a.claim_status_type_code = '03')
  OR ( a.claim_status_code = 'F' and a.claim_status_type_code = '04')
  OR ( a.claim_status_code = 'F' and a.claim_status_type_code = '17'))
USING SQLCA;

SQLCA.nf_handle_error('n_common_annuity', '', 'nf_get_aq_claim - SELECT top 1 claim_no, opening_no FROM CLAIM')

// al_claim_no = 0 because no openings for individual
IF al_claim_no = 0 THEN
	SELECT	TOP 1 a.claim_no , 0
	INTO		:al_claim_no , :al_opening_no
	FROM		CLAIM a
	JOIN		(	SELECT	Max(c.accident_date) 'max_accident_date'
					FROM		CLAIM   c
					WHERE		c.individual_no = :al_individual_no
   				AND ( c.claim_status_code = 'A'
                OR ( c.claim_status_code = 'F' and c.claim_status_type_code = '01')
                OR ( c.claim_status_code = 'F' and c.claim_status_type_code = '02')
                OR ( c.claim_status_code = 'F' and c.claim_status_type_code = '03')
                OR ( c.claim_status_code = 'F' and c.claim_status_type_code = '04')
                OR ( c.claim_status_code = 'F' and c.claim_status_type_code = '17'))) x ON a.accident_date = x.max_accident_date
	WHERE	a.individual_no = :al_individual_no
	AND  ( a.claim_status_code = 'A'
	  OR ( a.claim_status_code = 'F' and a.claim_status_type_code = '01')
	  OR ( a.claim_status_code = 'F' and a.claim_status_type_code = '02')
	  OR ( a.claim_status_code = 'F' and a.claim_status_type_code = '03')
	  OR ( a.claim_status_code = 'F' and a.claim_status_type_code = '04')
	  OR ( a.claim_status_code = 'F' and a.claim_status_type_code = '17'))
	USING SQLCA;
	
	SQLCA.nf_handle_error('n_common_annuity', '', 'nf_get_aq_claim -  select top 1 claim_no, 0 FROM CLAIM')
END IF

IF al_claim_no = 0 THEN
	Error.ObjectEvent='nf_get_aq_claim'
	Error.Text = 'Cannot determine claim number '
END IF
end subroutine

public subroutine nf_post_qualification_ben_entitlement (long al_annuity_eligibility_no, datetime adtm_new_annuity_start_date, datetime adtm_new_annuity_end_date, ref long al_benefit_entitlement_count);// nf_post_qualificatino_ben_entitlement - count of post-qualification benefit entitlement that is associated with the annuity eligibility.
// 
SELECT Count(*)
  INTO :al_benefit_entitlement_count
  FROM      BENEFIT_ENTITLEMENT a
       JOIN ANNUITY_ELIGIBILITY b ON     a.annuity_account_no = b.annuity_account_no
									          AND a.benefit_entitlement_from_date >= :adtm_new_annuity_start_date
									          AND a.benefit_entitlement_to_date <= dateadd(day,1,:adtm_new_annuity_end_date) 
 WHERE a.deleted_flag = 'N'
   AND a.active_flag  = 'Y'
   AND b.annuity_eligibility_no = :al_annuity_eligibility_no 
 USING SQLCA ; 

SQLCA.nf_handle_error('n_common_annuity', '', 'nf_post_qualification_ben_entitlement - SELECT Count(*) FROM BENEFIT_ENTITLEMENT, ANNUITY_ELIGIBILITY...')

end subroutine

public subroutine nf_post_81_setasides_for_ss (long al_individual_no, long al_claim_no, ref long al_txn_count, ref decimal adec_total_net_payment_amount);// nf_post_81_setasides_for_ss 
// 
Long    ll_count_applied, ll_count_unapplied, ll_processed_txn_count 
Decimal ldec_applied_net_payment_amount, ldec_unapplied_net_payment_amount, ldec_processed_net_payment_amount 

// for surviving spouse: sums & counts of set-asides with sub-types that attract annuities

// gather applied data
SELECT Count(*), IsNull(Sum(a.net_payment_amount),0)
  INTO :ll_count_applied, :ldec_applied_net_payment_amount
  FROM PAYMENT a 
       JOIN APPLIED_CLAIM_TXN b            ON a.payment_no = b.payment_no 
       JOIN Payment_Type c                 ON a.payment_sub_type_code = c.payment_type_code 
       JOIN Payment_Combination d          ON c.payment_type_code = d.payment_type_code 
       JOIN Claim_Role_Opening_Type_Xref e ON d.opening_type_code = e.opening_type_code 
 WHERE a.payment_type_code = '97'
   AND a.paid_to_date > '1982-01-01'  // if 'to' is greater than date, then 'from' could be equal to 1982-01-01
   AND b.recipient_type_code = 'I'
   AND e.annuity_eligibility_flag = 'Y'
   AND c.annuity_flag = 'Y'
   AND a.claim_no = :al_claim_no
   AND b.recipient_no = :al_individual_no 
 USING SQLCA ; 

SQLCA.nf_handle_error('n_common_annuity', '', 'nf_post_81_setasides_for_ss - SELECT Count(*) FROM PAYMENT (1 - for SS)...') 

// gather unapplied data
SELECT Count(*), IsNull(Sum(a.net_payment_amount),0)
  INTO :ll_count_unapplied, :ldec_unapplied_net_payment_amount
  FROM      PAYMENT a
       JOIN UNAPPLIED_CLAIM_TXN b          ON a.payment_no            = b.payment_no
       JOIN Payment_Type c                 ON a.payment_sub_type_code = c.payment_type_code
       JOIN Payment_Combination d          ON c.payment_type_code     = d.payment_type_code
       JOIN Claim_Role_Opening_Type_Xref e ON d.opening_type_code     = e.opening_type_code
 WHERE a.payment_type_code = '97'
   AND a.paid_to_date > '1982-01-01'  // if 'to' is greater than date, then 'from' could be equal to 1982-01-01
   AND b.recipient_type_code = 'I'
   AND e.annuity_eligibility_flag = 'Y'
   AND c.annuity_flag = 'Y'
   AND a.claim_no = :al_claim_no
   AND b.recipient_no = :al_individual_no 
 USING SQLCA ; 

SQLCA.nf_handle_error('n_common_annuity', '', 'nf_post_81_setasides_for_ss - SELECT Count(*) FROM PAYMENT (2 - for SS)...')

// CHECK PROCESSED DATE, sometimes there is no paid_to_date
SELECT Count(*), IsNull(Sum(a.net_payment_amount),0)
  INTO :ll_processed_txn_count, :ldec_processed_net_payment_amount
  FROM PAYMENT a
       JOIN APPLIED_CLAIM_TXN b            ON a.payment_no = b.payment_no
       JOIN Payment_Type c                 ON a.payment_sub_type_code = c.payment_type_code
       JOIN Payment_Combination d          ON c.payment_type_code = d.payment_type_code
       JOIN Claim_Role_Opening_Type_Xref e ON d.opening_type_code = e.opening_type_code
 WHERE a.payment_type_code = '97'
   AND a.paid_to_date is null
   AND a.processed_date >= '1982-01-01' 
   AND b.recipient_type_code = 'I'
   AND c.annuity_flag = 'Y'
   AND e.annuity_eligibility_flag = 'Y'
   AND a.claim_no = :al_claim_no
   AND b.recipient_no = :al_individual_no 
 USING SQLCA ; 

SQLCA.nf_handle_error('n_common_annuity', '', 'nf_post_81_setasides_for_ss - SELECT Count(*) FROM PAYMENT (3 - for SS)...') 

// populate reference values
al_txn_count = ll_count_applied + ll_count_unapplied + ll_processed_txn_count
adec_total_net_payment_amount = ldec_applied_net_payment_amount + ldec_unapplied_net_payment_amount + ldec_processed_net_payment_amount

end subroutine

public subroutine nf_post_92_setasides_for_iw (long al_individual_no, ref long al_txn_count, ref decimal adec_total_net_payment_amount);// nf_post_92_setasides_for_iw
// 
Long    ll_count_applied, ll_count_unapplied 
Decimal ldec_applied_net_payment_amount, ldec_unapplied_net_payment_amount 

// for claimant: sums & counts of set-asides with sub-types that attract annuities

// gather applied data
SELECT Count(*), IsNull(Sum(a.net_payment_amount),0)
  INTO :ll_count_applied, :ldec_applied_net_payment_amount
  FROM      PAYMENT a 
       JOIN APPLIED_CLAIM_TXN b            ON a.payment_no = b.payment_no
       JOIN Payment_Type c                 ON a.payment_sub_type_code = c.payment_type_code
       JOIN Payment_Combination d          ON c.payment_type_code = d.payment_type_code
       JOIN Claim_Role_Opening_Type_Xref e ON d.opening_type_code = e.opening_type_code 
 WHERE a.payment_type_code = '97'
   AND b.recipient_type_code = 'I'
   AND c.annuity_flag = 'Y'
   AND e.annuity_eligibility_flag = 'Y'
   AND e.claim_role_code = 'C'
   AND b.recipient_no = :al_individual_no 
   AND NOT EXISTS (SELECT *
                     FROM ANNUITY_SETASIDE_PRE_1993 f 
                    WHERE f.setaside_payment_no = a.payment_no) 
   AND NOT EXISTS (SELECT * 
                     FROM ANNUITY_PAYOUT_PRE_1993 g 
                    WHERE g.payout_payment_no = a.payment_no) 
 USING SQLCA ; 

SQLCA.nf_handle_error('n_common_annuity', '', 'nf_post_92_setasides_for_iw - SELECT Count(*) FROM PAYMENT (1 - for IW)...')

// gather unapplied data
SELECT Count(*), IsNull(Sum(a.net_payment_amount), 0) 
  INTO :ll_count_unapplied, :ldec_unapplied_net_payment_amount
  FROM      PAYMENT a 
       JOIN UNAPPLIED_CLAIM_TXN b          ON a.payment_no = b.payment_no
       JOIN Payment_Type c                 ON a.payment_sub_type_code = c.payment_type_code
       JOIN Payment_Combination d          ON c.payment_type_code = d.payment_type_code
       JOIN Claim_Role_Opening_Type_Xref e ON d.opening_type_code = e.opening_type_code 
 WHERE a.payment_type_code = '97'
   AND b.recipient_type_code = 'I'
   AND c.annuity_flag = 'Y'
   AND e.annuity_eligibility_flag = 'Y'
   AND e.claim_role_code = 'C'
   AND b.recipient_no = :al_individual_no 
   AND NOT EXISTS (SELECT *
                     FROM ANNUITY_SETASIDE_PRE_1993 f
                    WHERE f.setaside_payment_no = a.payment_no) 
   AND NOT EXISTS (SELECT *
                     FROM ANNUITY_PAYOUT_PRE_1993 g 
                    WHERE g.payout_payment_no = a.payment_no) 
 USING SQLCA ; 

SQLCA.nf_handle_error('n_common_annuity', '', 'nf_post_92_setasides_for_iw - SELECT Count(*) FROM PAYMENT (3 - for IW)...')

IF IsNull(ll_count_applied) THEN ll_count_applied = 0
IF IsNull(ll_count_unapplied) THEN ll_count_unapplied = 0

// populate reference values
al_txn_count = ll_count_applied + ll_count_unapplied

IF IsNull(ldec_applied_net_payment_amount) THEN ldec_applied_net_payment_amount = 0.00
IF IsNull(ldec_unapplied_net_payment_amount) THEN ldec_unapplied_net_payment_amount = 0.00

adec_total_net_payment_amount = ldec_applied_net_payment_amount + ldec_unapplied_net_payment_amount

end subroutine

public function boolean nf_check_for_annuity_calculation (long al_annuity_account_no, long al_annuity_eligibility_no);// nf_check_for_annuity_calculation 
// 
Long ll_check

SELECT count(*)
  INTO :ll_check
  FROM ANNUITY_CALC_ACCOUNT_HEADER
 WHERE annuity_account_no     = :al_annuity_account_no
   AND annuity_eligibility_no = :al_annuity_eligibility_no
 USING SQLCA ; 

SQLCA.nf_handle_error('n_common_annuity', '', 'nf_check_for_annuity_calculation - SELECT count(*) from ANNUITY_CALC_ACCOUNT_HEADER...') 

IF ll_check > 0 THEN RETURN FALSE

RETURN TRUE
end function

public function integer nf_check_individual_exists (long al_individual_no);// nf_check_individual_exists - check for the existence of individual
//
Integer li_count

SELECT Count(*)
  INTO :li_count
  FROM INDIVIDUAL
 WHERE individual_no = :al_individual_no
 USING SQLCA ; 

SQLCA.nf_handle_error('n_common_annuity', '', 'nf_check_individual_exists - SELECT Count(*) FROM INDIVIDUAL...')

RETURN li_count 

end function

public function integer nf_check_for_open_window (string as_window_name, string as_window_message);// nf_check_for_open_window
// 
Integer li_counter, li_upperbound 

// check that window is not already open
li_upperbound = UpperBound(gstr_window_array)
FOR li_counter = 1 to li_upperbound
	IF gstr_window_array[li_counter].window_element.ClassName() = as_window_name THEN
		MessageBox('Already Open','The '+as_window_message+' window is already open. Please close it before clicking this button.', Exclamation!)
		RETURN 1
	END IF
NEXT

end function

public function integer nf_check_for_open_window (string as_window_name, string as_window_message, ref unsignedlong aul_handle);// nf_check_for_open_window
//
Integer li_counter, li_upperbound 

// check that window is not already open
li_upperbound = UpperBound(gstr_window_array)
FOR li_counter = 1 to li_upperbound
	IF gstr_window_array[li_counter].window_element.ClassName() = as_window_name THEN
		MessageBox('Already Open','The '+as_window_message+' window is already open. Please close it before clicking this button.', Exclamation!)
		aul_handle = gstr_window_array[li_counter].Handle_element
		RETURN 1
	END IF
NEXT

end function

public subroutine nf_open_verify_benefit_entitlement (string as_open_window_name, long al_annuity_account_no, long al_annuity_eligibility_no, long al_action_code, long al_claim_no, long al_individual_no, long al_annuity_payout_no, string as_claim_role_code);// nf_open_verify_benefit_entitlement 
// 
s_window_message lstr_message 
window lw_to_open 

// Verify Benefit Entitlement
lstr_message.al_doubleparm[1] = al_annuity_account_no
lstr_message.al_doubleparm[2] = al_annuity_eligibility_no
lstr_message.al_doubleparm[3] = al_action_code
lstr_message.al_doubleparm[4] = al_claim_no
lstr_message.al_doubleparm[5] = al_individual_no
lstr_message.al_doubleparm[6] = al_annuity_payout_no

lstr_message.as_stringparm[1] = as_claim_role_code

OpenWithParm(lw_to_open, lstr_message, as_open_window_name) 

end subroutine

public subroutine nf_select_next_available_checklist_step (ref n_checklist anv_checklist, ref u_checklist_datawindow adw_checklist, long al_checklist_no, string as_checklist_type_code);// nf_select_next_available_checklist_step 
// 
Integer li_find, li_next_incomplete_checklist_step_no

// find the next checklist step that can be selected & select it.
anv_checklist.nf_retrieve_checklists(as_checklist_type_code,al_checklist_no)
li_next_incomplete_checklist_step_no = anv_checklist.nf_get_next_checklist_step(al_checklist_no)

adw_checklist.uf_find_row('checklist_step_no', String(li_next_incomplete_checklist_step_no))

end subroutine

public subroutine nf_get_annuity_payout_claim (long al_individual_no, long al_annuity_payout_no, ref long al_claim_no);// nf_get_annuity_payout_claim - retrieves the claim with the most recent opening accident recurrence date
//                               this claim is used to open the correspondence window (for ANNPAY01 or ANNPAY02 letters)
//                               the claim must also have been involved in a specified annuity payout
// 
SELECT TOP 1 a.claim_no 
  INTO :al_claim_no
  FROM      CLAIM a
       JOIN OPENING b                      ON a.claim_no = b.claim_no
       JOIN Claim_Role_Opening_Type_Xref c ON b.opening_type_code = c.opening_type_code 
       JOIN (SELECT Max(e.accident_recurrence_date) 'max_accident_recurrence_date' 
               FROM      CLAIM d 
                    JOIN OPENING e ON d.claim_no = e.claim_no
                    JOIN Claim_Role_Opening_Type_Xref f ON e.opening_type_code = f.opening_type_code 
              WHERE d.individual_no = :al_individual_no 
                AND f.claim_role_code = 'C' 
                AND f.active_flag = 'Y'
                AND f.annuity_eligibility_flag = 'Y'
                AND ( d.claim_status_code = 'A'
                      OR ( d.claim_status_code = 'F' and d.claim_status_type_code = '01')
                      OR ( d.claim_status_code = 'F' and d.claim_status_type_code = '02')
                      OR ( d.claim_status_code = 'F' and d.claim_status_type_code = '03')
                      OR ( d.claim_status_code = 'F' and d.claim_status_type_code = '04')
                      OR ( d.claim_status_code = 'F' and d.claim_status_type_code = '17'))) x ON b.accident_recurrence_date = x.max_accident_recurrence_date 
 WHERE a.individual_no = :al_individual_no 
   AND c.claim_role_code = 'C' 
   AND c.annuity_eligibility_flag = 'Y' 
   AND c.active_flag = 'Y'
   AND ( a.claim_status_code = 'A'
         OR ( a.claim_status_code = 'F' and a.claim_status_type_code = '01')
         OR ( a.claim_status_code = 'F' and a.claim_status_type_code = '02')
         OR ( a.claim_status_code = 'F' and a.claim_status_type_code = '03')
         OR ( a.claim_status_code = 'F' and a.claim_status_type_code = '04')
         OR ( a.claim_status_code = 'F' and a.claim_status_type_code = '17'))
   AND a.claim_no IN (SELECT g.claim_no 
                        FROM ANNUITY_PAYOUT_CLAIM_SUMMARY g 
                       WHERE g.annuity_payout_no = :al_annuity_payout_no ) 
 USING SQLCA ; 

SQLCA.nf_handle_error('n_common_annuity', '', 'nf_get_annuity_payout_claim - select top 1 claim_no (1)')

// al_claim_no = 0 because no openings for individual
IF al_claim_no = 0 THEN
	SELECT TOP 1 a.claim_no, 0 
     INTO :al_claim_no 
     FROM      CLAIM a 
          JOIN (SELECT Max(c.accident_date) 'max_accident_date' 
                  FROM CLAIM c 
                 WHERE c.individual_no = :al_individual_no 
                   AND (c.claim_status_code = 'A' 
                        OR ( c.claim_status_code = 'F' and c.claim_status_type_code = '01')
                        OR ( c.claim_status_code = 'F' and c.claim_status_type_code = '02')
                        OR ( c.claim_status_code = 'F' and c.claim_status_type_code = '03')
                        OR ( c.claim_status_code = 'F' and c.claim_status_type_code = '04')
                        OR ( c.claim_status_code = 'F' and c.claim_status_type_code = '17'))) x ON a.accident_date = x.max_accident_date 
    WHERE a.individual_no = :al_individual_no 
      AND (a.claim_status_code = 'A' 
           OR (a.claim_status_code = 'F' and a.claim_status_type_code = '01') 
           OR (a.claim_status_code = 'F' and a.claim_status_type_code = '02') 
           OR (a.claim_status_code = 'F' and a.claim_status_type_code = '03') 
           OR (a.claim_status_code = 'F' and a.claim_status_type_code = '04') 
           OR (a.claim_status_code = 'F' and a.claim_status_type_code = '17')) 
      AND a.claim_no IN (SELECT g.claim_no 
                           FROM ANNUITY_PAYOUT_CLAIM_SUMMARY g 
                          WHERE g.annuity_payout_no = :al_annuity_payout_no) 
    USING SQLCA ; 

	SQLCA.nf_handle_error('n_common_annuity', '', 'nf_get_annuity_payout_claim - select top 1 claim_no (2)')
END IF

IF al_claim_no = 0 THEN
	Error.ObjectEvent='nf_get_annuity_payout_claim'
	Error.Text = 'Cannot determine claim number '
END IF

end subroutine

public subroutine nf_open_cae_for_payout (string as_open_window_name, long al_claim_no, long al_individual_no, long al_annuity_payout_no, string as_claim_role_code);// nf_open_cae_for_payout
//
s_window_message lstr_CAE_message
window lw_to_open 

IF IsNull(al_claim_no) THEN al_claim_no = 0

IF as_claim_role_code = 'C' THEN
	lstr_CAE_message.as_stringparm[1] = 'IW'
ELSE
	lstr_CAE_message.as_stringparm[1] = as_claim_role_code
END IF

lstr_CAE_message.as_stringparm[2] = 'payout'

lstr_CAE_message.al_doubleparm[1] = al_claim_no
lstr_CAE_message.al_doubleparm[2] = al_individual_no
lstr_CAE_message.al_doubleparm[3] = al_annuity_payout_no

OpenWithParm(lw_to_open, lstr_CAE_message, as_open_window_name) 

end subroutine

public subroutine nf_get_ap_claim (long al_annuity_payout_no, long al_individual_no, ref long al_claim_no);// nf_get_ap_claim - retrieves the claim with the most recent opening accident recurrence date
//                   amongst those claims that are associated with the annutiy payout (ANNUITY_PAYOUT_CLAIM_SUMMARY
//                   this claim is used to open the correspondence window (for ANNPAY01/ANNPAY02 letters) 
// 
SELECT	TOP 1 a.claim_no
INTO		:al_claim_no
FROM		CLAIM a
JOIN		OPENING b ON a.claim_no = b.claim_no
JOIN     Claim_Role_Opening_Type_Xref c on b.opening_type_code = c.opening_type_code
JOIN		(	SELECT	Max(e.accident_recurrence_date) 'max_accident_recurrence_date'
				FROM		CLAIM   d
				JOIN		OPENING e ON d.claim_no = e.claim_no
				JOIN		Claim_Role_Opening_Type_Xref f ON e.opening_type_code = f.opening_type_code
				JOIN     ANNUITY_PAYOUT_CLAIM_SUMMARY g ON d.claim_no = g.claim_no
				JOIN     ANNUITY_ACCOUNT              h ON g.annuity_account_no = h.annuity_account_no
				                                       AND d.individual_no      = h.individual_no 
				WHERE		d.individual_no            = :al_individual_no
				AND		f.claim_role_code          = 'C'
				AND		f.active_flag              = 'Y'
				AND		f.annuity_eligibility_flag = 'Y'
				AND      g.annuity_payout_no        = :al_annuity_payout_no ) x ON b.accident_recurrence_date = x.max_accident_recurrence_date
WHERE	   a.individual_no            = :al_individual_no
AND      c.claim_role_code          = 'C'
and      c.annuity_eligibility_flag = 'Y'
AND	   c.active_flag              = 'Y'
USING SQLCA;

SQLCA.nf_handle_error('n_common_annuity', '', 'nf_get_ap_claim - select top 1 claim_no FROM CLAIM (1)')


// al_claim_no = 0 because no openings for individual
IF al_claim_no = 0 THEN
	SELECT	TOP 1 a.claim_no
	INTO		:al_claim_no
	FROM		CLAIM a
	JOIN		(	SELECT	Max(d.accident_date) 'max_accident_date'
					FROM		CLAIM                        d
					JOIN     ANNUITY_PAYOUT_CLAIM_SUMMARY g ON d.claim_no           = g.claim_no
    				JOIN     ANNUITY_ACCOUNT              h ON g.annuity_account_no = h.annuity_account_no
		    		                                       AND d.individual_no      = h.individual_no 
					WHERE		d.individual_no     = :al_individual_no
					AND      g.annuity_payout_no = :al_annuity_payout_no ) x ON a.accident_date = x.max_accident_date
	WHERE	a.individual_no = :al_individual_no
	USING SQLCA;
	
	SQLCA.nf_handle_error('n_common_annuity', '', 'nf_get_ap_claim - select top 1 claim_no FROM CLAIM (2)')
END IF

IF al_claim_no = 0 THEN
	Error.ObjectEvent='nf_get_ap_claim'
	Error.Text = 'Cannot determine claim number '
END IF

end subroutine

public function integer nf_validate_annuity_status (long al_annuity_payout_no, string as_annuity_status_code, string as_annuity_status_reason_code);// nf_validate_annuity_status
// 
Integer li_combo_count, li_status_count, li_reason_count, li_total_count

SELECT Count(*)
  INTO :li_combo_count 
  FROM      Annuity_Payout_Status a
       JOIN Annuity_Payout_Status_Reason_Xref b ON a.annuity_payout_status_code = b.annuity_payout_status_code
       JOIN   Annuity_Payout_Status_Reason c    ON b.annuity_payout_status_reason_code = c.annuity_payout_status_reason_code
 WHERE b.annuity_payout_status_code = :as_annuity_status_code
   AND b.annuity_payout_status_reason_code = :as_annuity_status_reason_code
   AND b.active_flag = 'Y' 
 USING SQLCA ; 

SQLCA.nf_handle_error('n_common_annuity', '', 'nf_validate_annuity_status - SELECT Count(*) FROM Annuity_Payout_Status, Annuity_Payout_Status_Reason_Xref...')

IF li_combo_count = 0 THEN	
	Error.Text = 'The attempt to change the annuity payout status on annuity_payout_no '+String(al_annuity_payout_no)+' has failed. The following combination is not valid/active:' +&
                '~r~n~t Annuity Payout Status: '+ as_annuity_status_code + '~r~n~t Annuity Payout Status Reason: '+ as_annuity_status_reason_code 
	Error.WindowMenu = "cmwb"
	Error.Object = "n_common_annuity.nf_validate_annuity_status"
	SignalError()
END IF

SELECT Count(*)
  INTO :li_status_count
  FROM Annuity_Payout_Status
 WHERE annuity_payout_status_code = :as_annuity_status_code
   AND active_flag = 'Y' 
 USING SQLCA ; 

SQLCA.nf_handle_error('n_common_annuity', '', 'nf_validate_annuity_status - SELECT Count(*) FROM Annuity_Payout_Status...')

IF li_status_count = 0 THEN	
	Error.Text = 'The attempt to change the annuity payout status on annuity_payout_no '+String(al_annuity_payout_no)+' has failed. The following annuity payout status is not valid/active:' +&
                '~r~n~t Annuity Payout Status: '+ as_annuity_status_code 
	Error.WindowMenu = "cmwb"
	Error.Object = "n_common_annuity.nf_validate_annuity_status" 
	SignalError()
END IF

SELECT Count(*) 
  INTO :li_reason_count 
  FROM Annuity_Payout_Status_Reason 
 WHERE annuity_payout_status_reason_code = :as_annuity_status_reason_code 
   AND active_flag = 'Y' 
 USING SQLCA ; 

SQLCA.nf_handle_error('n_common_annuity', '', 'nf_validate_annuity_status - SELECT Count(*) FROM Annuity_Payout_Status_Reason...')

IF li_reason_count = 0 THEN	
	Error.Text = 'The attempt to change the annuity payout status reason on annuity_payout_no '+String(al_annuity_payout_no)+' has failed. The following annuity payout status reason is not valid/active:' +&
                '~r~n~t Annuity Payout Status Reason: ' + as_annuity_status_reason_code 
	Error.WindowMenu = "cmwb"
	Error.Object = "n_common_annuity.nf_validate_annuity_status" 
	SignalError()
END IF

li_total_count = li_combo_count + li_status_count + li_reason_count

RETURN li_total_count
end function

public subroutine nf_open_event_log (string as_open_window_name, string as_claim_role_code, long al_claim_no, long al_individual_no, date adt_accident_date, string as_last_name, string as_given_names, string as_event_category_code, string as_event_type_code, string as_event_specific_code, string as_allow_parameter_change, string as_add_new_event, string as_message);// nf_open_event_log 
// 
Date ldt_null
s_window_message lstr_message
window lw_to_open

SetNull(ldt_null)

// open - select claims for event log - request merge individuals
lstr_message.al_doubleparm[1] = al_claim_no 
lstr_message.al_doubleparm[2] = al_individual_no 
lstr_message.al_doubleparm[3] = 0

lstr_message.adt_dateparm[1] = adt_accident_date 

lstr_message.as_StringParm[1] = as_claim_role_code 
lstr_message.as_StringParm[2] = as_last_name 
lstr_message.as_StringParm[3] = as_given_names 
lstr_message.as_StringParm[4] = as_event_category_code
lstr_message.as_StringParm[5] = as_event_type_code
lstr_message.as_StringParm[6] = as_event_specific_code
lstr_message.as_StringParm[7] = as_allow_parameter_change
lstr_message.as_StringParm[8] = as_add_new_event
lstr_message.as_StringParm[9] = as_message
lstr_message.as_StringParm[10] = ''
lstr_message.as_StringParm[11] = ''

OpenWithParm(lw_to_open, lstr_message, as_open_window_name) 

end subroutine

public subroutine nf_open_overpayment_list (string as_open_window_name, long al_annuity_payout_no, long al_individual_no);// nf_open_overpayment_list
// 
s_window_message lstr_message
window lw_to_open

lstr_message.al_doubleparm[1] = al_annuity_payout_no
lstr_message.al_doubleparm[2] = al_individual_no

OpenWithParm(lw_to_open, lstr_message, as_open_window_name) 

end subroutine

public function integer nf_validate_annuity_payout_for_cae (string as_annuity_eligibility_run_option_code, long al_individual_no, long al_claim_no);// nf_validate_annuity_payout_for_cae 
// 
Boolean lb_violation 
Integer li_rtn, li_incomplete_payout_count, li_complete_payout_count, li_no_payout_count 
String  ls_message
s_window_message lstr_message, lstr_ss_message

IF as_annuity_eligibility_run_option_code <> 'SS' THEN
	// do not open this module for IW if there is an incomplete ANNUITY_PAYOUT
   SELECT Count(*)
     INTO :li_incomplete_payout_count
     FROM      ANNUITY_ACCOUNT a
          JOIN ANNUITY_PAYOUT b ON a.annuity_account_no = b.annuity_account_no 
    WHERE a.individual_no = :al_individual_no
      AND a.claim_role_code = 'C'
      AND b.annuity_payout_status_code = 'I'
      AND NOT EXISTS (SELECT * 
                        FROM      ANNUITY_ELIGIBILITY c 
                             JOIN CHECKLIST d           ON c.confirm_annuity_eligibility_checklist_no = d.checklist_no
                       WHERE c.annuity_account_no = a.annuity_account_no
                         AND d.checklist_status_code = 'IA'  
                         AND d.checklist_type_code IN ('CAEIN','CAEIC') )
    USING SQLCA ; 

	SQLCA.nf_handle_error('n_common_annuity', '', 'nf_validate_annuity_payout_for_cae - select count(*) from ANNUITY_ACCOUNT, ANNUITY_PAYOUT(1)...')
	
	IF li_incomplete_payout_count > 0 THEN
		lb_violation = TRUE
		ls_message = 'This injured worker has an incomplete annuity payout and can only be accessed through the Prepare Annuity Payout ' + &
						 'module. The injured worker cannot be added to any potential eligibility list.'
	END IF
ELSE
	// do not open this module for SS if there is an incomplete ANNUITY_PAYOUT
   SELECT Count(*)
     INTO :li_incomplete_payout_count 
     FROM      ANNUITY_ACCOUNT a 
          JOIN ANNUITY_PAYOUT b ON a.annuity_account_no = b.annuity_account_no
    WHERE a.individual_no = :al_individual_no 
      AND a.claim_no = :al_claim_no
      AND a.claim_role_code = 'SS' 
      AND b.annuity_payout_status_code = 'I' 
      AND NOT EXISTS (SELECT * 
                        FROM      ANNUITY_ELIGIBILITY c 
                             JOIN CHECKLIST d           ON c.confirm_annuity_eligibility_checklist_no = d.checklist_no 
                       WHERE c.annuity_account_no = a.annuity_account_no 
                         AND d.checklist_status_code = 'IA'  
                         AND d.checklist_type_code IN ('CAESN','CAESC') ) 
    USING SQLCA ; 

	SQLCA.nf_handle_error('n_common_annuity', '', 'nf_validate_annuity_payout_for_cae - select count(*) from ANNUITY_ACCOUNT,ANNUITY_PAYOUT(2)...')

	IF li_incomplete_payout_count > 0 THEN
		lb_violation = TRUE
		ls_message = 'This surviving spouse has an incomplete annuity payout for this claim and can only be accessed through the Prepare Annuity Payout ' +&
						 'module. The surviving spouse cannot be added to any potential eligibility list.'
	END IF
END IF

IF lb_violation THEN
	MessageBox('Confirm Annuity List Error', ls_message,Exclamation!) 
	lstr_message.as_stringparm[1] = as_annuity_eligibility_run_option_code
	lstr_message.as_stringparm[2] = 'menu'
	
	lstr_message.al_doubleparm[1] = 0
	lstr_message.al_doubleparm[2] = 0
	lstr_message.al_doubleparm[3] = 0 // annuity_payout_no
	OpenWithParm(w_confirm_annuity_eligibility, lstr_message) 
	RETURN -1
ELSE
	RETURN 0
END IF

end function

public function integer nf_individual_has_other_annuity_accounts (long al_individual_no, long al_claim_no);// nf_individual_has_other_annuity_accounts - report on any other annuity accounts that the individual might have 
//
Integer li_annuity_account_count 

SELECT Count(*) 
  INTO :li_annuity_account_count 
  FROM      ANNUITY_ACCOUNT a 
       JOIN ANNUITY_ELIGIBILITY b ON a.annuity_account_no = b.annuity_account_no 
 WHERE b.annuity_eligibility_status_code = 'A' 
   AND a.individual_no = :al_individual_no 
   AND a.claim_no <> :al_claim_no 
 USING SQLCA ; 

SQLCA.nf_handle_error('n_common_annuity', '', 'nf_individual_has_other_annuity_accounts - SELECT Count(*) FROM ANNUITY_ACCOUNT, ANNUITY_ELIGIBILITY...')

IF IsNull(li_annuity_account_count) THEN li_annuity_account_count = 0

RETURN li_annuity_account_count

end function

public function boolean nf_determine_request_payout_status (long al_annuity_payout_no);// nf_determine_request_payout_status - this function determines if the payout request has proceeded too far to be reversed
//
Boolean lb_payout_can_be_reversed 
Integer li_cheque_count, li_processed_count 

SELECT Count(*) 
  INTO :li_cheque_count 
  FROM ANNUITY_PAYOUT_RECIPIENT 
 WHERE cheque_no > 0 
   AND annuity_payout_no = :al_annuity_payout_no 
 USING SQLCA ; 

SQLCA.nf_handle_error('n_common_annuity', '', 'nf_determine_request_payout_status - SELECT Count(*) FROM ANNUITY_PAYOUT_RECIPIENT...')

IF IsNull(li_cheque_count) THEN li_cheque_count = 0

SELECT Count(*)
  INTO :li_processed_count
  FROM      ANNUITY_PAYOUT_TXN_DETAIL a
       JOIN PAYMENT                   b ON a.payment_no = b.payment_no
 WHERE a.payment_no > 0 
   AND a.annuity_payout_no = :al_annuity_payout_no 
   AND b.processed_date IS NOT NULL 
 USING SQLCA ; 

SQLCA.nf_handle_error('n_common_annuity', '', 'nf_determine_request_payout_status - SELECT Count(*) FROM ANNUITY_PAYOUT_TXN_DETAIL, PAYMENT...')

IF IsNull(li_processed_count) THEN li_processed_count = 0

IF li_cheque_count > 0 OR li_processed_count > 0 THEN
	lb_payout_can_be_reversed = FALSE
ELSE
	lb_payout_can_be_reversed = TRUE
END IF

RETURN lb_payout_can_be_reversed

end function

public subroutine nf_make_menu_item_invisible (ref window aw_window, string as_menu_item, string as_sub_menu_item);// nf_make_menu_item_visible - aw_window func makes either the report or the print menu item not visible in menu bar or toolbar 
//
Integer li_menu_counter, li_upperbound_menu, li_menu_inner_counter, li_upperbound_inner_menu

li_upperbound_menu = UpperBound(aw_window.MenuId.Item[])
FOR li_menu_counter = 1 TO li_upperbound_menu	
	IF Lower(aw_window.MenuId.Item[li_menu_counter].ClassName()) = as_menu_item THEN		
		IF as_sub_menu_item <> '' THEN
			// if sub menu item is specified, then make sub menu item not visible
			li_upperbound_inner_menu = UpperBound(aw_window.MenuId.Item[li_menu_counter].Item[])
			FOR li_menu_inner_counter = 1 TO li_upperbound_inner_menu			
				IF Lower(aw_window.MenuId.Item[li_menu_counter].Item[li_menu_inner_counter].ClassName()) = as_sub_menu_item THEN
					aw_window.MenuId.Item[li_menu_counter].Item[li_menu_inner_counter].Visible = FALSE
					aw_window.MenuId.Item[li_menu_counter].Item[li_menu_inner_counter].ToolBarItemVisible = FALSE
					EXIT
				END IF
			NEXT
		ELSE
			// if sub menu item is NOT specified, then make menu item and all associated sub menu items not visible
			aw_window.MenuId.Item[li_menu_counter].Visible = FALSE
			aw_window.MenuId.Item[li_menu_counter].ToolBarItemVisible = FALSE
			li_upperbound_inner_menu = UpperBound(aw_window.MenuId.Item[li_menu_counter].Item[])
			FOR li_menu_inner_counter = 1 TO li_upperbound_inner_menu			
				aw_window.MenuId.Item[li_menu_counter].Item[li_menu_inner_counter].Visible = FALSE
				aw_window.MenuId.Item[li_menu_counter].Item[li_menu_inner_counter].ToolBarItemVisible = FALSE
			NEXT			
		END IF
	END IF
NEXT

end subroutine

public subroutine nf_injured_worker_sub_ledger_balance (long al_individual_no, ref decimal adec_total_net_payment_amount);// nf_injured_worker_sub_ledger_balance 
// 
Decimal ldec_applied_net_payment_amount, ldec_unapplied_net_payment_amount

// gather applied data 
SELECT IsNull(Sum(b.txn_amount), 0) 
  INTO :ldec_applied_net_payment_amount 
  FROM      PAYMENT a 
       JOIN APPLIED_CLAIM_TXN b ON a.payment_no = b.payment_no 
 WHERE a.payment_type_code = '97' 
   AND b.recipient_type_code = 'I' 
   AND b.recipient_no = :al_individual_no 
 USING SQLCA ; 

SQLCA.nf_handle_error('n_common_annuity', '', 'nf_injured_worker_sub_ledger_balance - SELECT IsNull(Sum(a.net_payment_amount),0) FROM PAYMENT, APPLIED_CLAIM_TXN...')

// gather unapplied data
SELECT IsNull(Sum(b.txn_amount), 0) 
  INTO :ldec_unapplied_net_payment_amount 
  FROM      PAYMENT a 
       JOIN UNAPPLIED_CLAIM_TXN b ON a.payment_no = b.payment_no 
 WHERE a.payment_type_code = '97'
   AND b.recipient_type_code = 'I'
   AND b.recipient_no = :al_individual_no
 USING SQLCA ; 

SQLCA.nf_handle_error('n_common_annuity', '', 'nf_injured_worker_sub_ledger_balance - SELECT IsNull(Sum(a.net_payment_amount),0) FROM PAYMENT, UNAPPLIED_CLAIM_TXN...')

IF IsNull(ldec_applied_net_payment_amount) THEN ldec_applied_net_payment_amount = 0.00
IF IsNull(ldec_unapplied_net_payment_amount) THEN ldec_unapplied_net_payment_amount = 0.00

adec_total_net_payment_amount = ldec_applied_net_payment_amount + ldec_unapplied_net_payment_amount
end subroutine

public subroutine nf_surviving_spouse_sub_ledger_balance (long al_individual_no, long al_claim_no, ref decimal adec_total_net_payment_amount);// nf_surviving_spouse_sub_ledger_balance
// 
Decimal ldec_applied_net_payment_amount, ldec_unapplied_net_payment_amount

// gather applied data
SELECT ISNULL(SUM(b.txn_amount), 0) 
  INTO :ldec_applied_net_payment_amount 
  FROM      PAYMENT a
       JOIN APPLIED_CLAIM_TXN b ON a.payment_no = b.payment_no 
 WHERE a.payment_type_code = '97' 
   AND b.recipient_type_code = 'I'
   AND b.recipient_no = :al_individual_no
   AND b.claim_no = :al_claim_no
 USING SQLCA ; 

SQLCA.nf_handle_error('w_common_annuity', '', 'nf_surviving_spouse_sub_ledger_balance - SELECT IsNull(Sum(a.net_payment_amount),0) FROM PAYMENT, APPLIED_CLAIM_TXN...')

// gather unapplied data
SELECT ISNULL(SUM(b.txn_amount), 0) 
  INTO :ldec_unapplied_net_payment_amount 
  FROM      PAYMENT a 
       JOIN UNAPPLIED_CLAIM_TXN b ON a.payment_no = b.payment_no 
 WHERE a.payment_type_code = '97'
   AND b.recipient_type_code = 'I'
   AND b.recipient_no = :al_individual_no
   AND b.claim_no = :al_claim_no 
 USING SQLCA ; 

SQLCA.nf_handle_error('n_common_annuity', '', 'nf_surviving_spouse_sub_ledger_balance - SELECT IsNull(Sum(a.net_payment_amount),0) FROM PAYMENT, UNAPPLIED_CLAIM_TXN...')

IF IsNull(ldec_applied_net_payment_amount) THEN ldec_applied_net_payment_amount = 0.00
IF IsNull(ldec_unapplied_net_payment_amount) THEN ldec_unapplied_net_payment_amount = 0.00

adec_total_net_payment_amount = ldec_applied_net_payment_amount + ldec_unapplied_net_payment_amount 

end subroutine

public function long nf_get_annuity_account (long al_claim_no, long al_individual_no);// nf_get_annuity_account 
// 
Long ll_annuity_account_no 

SELECT annuity_account_no 
  INTO :ll_annuity_account_no 
  FROM ANNUITY_ACCOUNT 
 WHERE claim_no = :al_claim_no 
   AND individual_no = :al_individual_no 
 USING SQLCA ; 

SQLCA.nf_handle_error('n_common_annuity', '', 'nf_get_annuity_account - SELECT annuity_account_no FROM ANNUITY_ACCOUNT...')

IF IsNull(ll_annuity_account_no) THEN
	RETURN 0
ELSE
	RETURN ll_annuity_account_no
END IF

end function

public subroutine nf_open_confirm_contract_reminder (long al_annuity_payout_no, long al_individual_no, long al_claim_no, string as_annuity_role_code);// nf_open_confirm_contract_reminder 
// 
String ls_name, ls_annuity_role_desc_e 
s_window_message lstr_message 

SELECT last_name + ', ' + given_names
  INTO :ls_name
  FROM INDIVIDUAL
 WHERE individual_no = :al_individual_no
 USING SQLCA ; 

SQLCA.nf_handle_error('n_common_annuity', '', 'nf_open_confirm_contract_reminder - SELECT last_name, given_names FROM INDIVIDUAL...')

SELECT annuity_role_desc_e 
  INTO :ls_annuity_role_desc_e 
  FROM Annuity_Role 
 WHERE annuity_role_code = :as_annuity_role_code 
 USING SQLCA ; 

SQLCA.nf_handle_error('n_common_annuity', '', 'nf_open_confirm_contract_reminder - SELECT annuity_role_desc_e FROM Annuity_Role...')

lstr_message.as_stringparm[1] = ls_name
lstr_message.as_stringparm[2] = ls_annuity_role_desc_e
lstr_message.al_doubleparm[1] = al_annuity_payout_no
lstr_message.al_doubleparm[2] = al_individual_no
lstr_message.al_doubleparm[3] = al_claim_no

OpenWithParm(w_confirm_contract_reminder, lstr_message) 

end subroutine

public function long nf_get_checklist_annuity_eligibility_no (long al_checklist_no);// nf_get_checklist_annuity_eligibility_no 
// 
Long ll_annuity_eligibility_no 

SELECT annuity_eligibility_no
  INTO :ll_annuity_eligibility_no
  FROM ANNUITY_ELIGIBILITY
 WHERE confirm_annuity_eligibility_checklist_no = :al_checklist_no
 USING SQLCA ; 

SQLCA.nf_handle_error('n_common_annuity', '', 'nf_get_checklist_annuity_eligibility_no - SELECT annuity_eligibility_no FROM ANNUITY_ELIGIBILITY...')

RETURN ll_annuity_eligibility_no

end function

public function integer nf_get_annuity_end_date (long al_individual_no, string as_type, ref string as_message, ref datetime adtm_annuity_end_date, ref datetime adtm_annuity_eligibility_end_date_used, datetime adtm_birth_date, datetime adtm_death_date);// nf_get_annuity_end_date
// 
Datetime ldtm_65th_birthday, ldtm_death_date, ldtm_end_date 

IF IsNull(adtm_birth_date) and IsNull(adtm_death_date) THEN
	// If the birth and/or death dates are not being changed, then select existing values
	SELECT DateAdd(yy,65,birth_date) , death_date
	  INTO :ldtm_65th_birthday, :ldtm_death_date
	  FROM INDIVIDUAL
	 WHERE individual_no = :al_individual_no 
	 USING SQLCA ; 

	SQLCA.nf_handle_error('n_common_annuity', '', 'nf_get_annuity_end_date - SELECT DateAdd(yy,65,birth_date) , death_date FROM INDIVIDUAL...')
ELSE
	// another module is modifying dates
	SELECT DateAdd(yy,65,:adtm_birth_date)
	  INTO :ldtm_65th_birthday 
	  FROM INDIVIDUAL
	 WHERE individual_no = :al_individual_no 
	 USING SQLCA ;

	SQLCA.nf_handle_error('n_common_annuity', '', 'nf_get_annuity_end_date - SELECT DateAdd(yy,65,birth_date) FROM INDIVIDUAL...')
	
	ldtm_death_date = adtm_death_date
END IF

IF IsNull(ldtm_65th_birthday) THEN
	as_message = 'The ' + as_type + ' must have a birth date in order for the annuity eligibility to be processed. Contact the HELPDESK.'
	RETURN -1
END IF

// determine annuity start date
IF IsNull(ldtm_death_date) OR ldtm_death_date >= ldtm_65th_birthday THEN
	ldtm_end_date = ldtm_65th_birthday
	adtm_annuity_eligibility_end_date_used = ldtm_65th_birthday
ELSE
	ldtm_end_date = ldtm_death_date
	adtm_annuity_eligibility_end_date_used = ldtm_death_date
END IF

SELECT top 1 dbo.udf_Last_Date_Of_Current_Month(:ldtm_end_date)
  INTO :adtm_annuity_end_date
  FROM sysobjects
 USING SQLCA ; 

SQLCA.nf_handle_error('n_common_annuity', '', 'nf_get_annuity_end_date - SELECT top 1 dbo.udf_Last_Date_Of_Current_Month(:ldtm_end_date) FROM sysobjects') 

RETURN 0

end function

public subroutine nf_open_calculate_annuity (string as_open_window_name, long al_annuity_account_no, long al_annuity_eligibility_no, long al_payout_no, datetime adtm_annuity_start_date, datetime adtm_annuity_end_date, datetime adtm_annuity_eligibility_end_date_used, long ai_annuity_set_aside_percent_no, integer ai_annuity_calculation_needed);// nf_open_calculate_annuity
// 
s_window_message lstr_message 
window lw_to_open 

// open - calculate annuity
lstr_message.al_doubleparm[1] = al_annuity_account_no
lstr_message.al_doubleparm[2] = al_annuity_eligibility_no
lstr_message.al_doubleparm[3] = al_payout_no
lstr_message.al_doubleparm[4] = ai_annuity_set_aside_percent_no
lstr_message.al_doubleparm[5] = 2                             // used to open in calculate or no calculate (viewing) mode. 1 -(viewing) **  calculate  - 2 
lstr_message.al_doubleparm[6] = ai_annuity_calculation_needed // do we need to create a new calculation. 0 = NO; 1 = YES;
lstr_message.adtm_datetimeparm[1] = adtm_annuity_start_date
lstr_message.adtm_datetimeparm[2] = adtm_annuity_end_date
lstr_message.adtm_datetimeparm[3] = adtm_annuity_eligibility_end_date_used


OpenWithParm(lw_to_open,lstr_message,as_open_window_name)

end subroutine

public function long nf_insert_annuity_eligibility (long al_annuity_account_no, string as_annuity_eligibility_status_code, datetime adtm_confirmed_date, string as_confirmed_by_user_id, datetime adtm_annuity_start_date, datetime adtm_annuity_eligibility_end_date_used, datetime adtm_annuity_end_date, string as_benefit_option_code, integer al_annuity_set_aside_percent_no, decimal adec_annuity_set_aside_percent, string as_annuity_eligibility_reason_code, string as_annuity_eligibility_comment, long al_confirm_annuity_elig_checklist_no, integer al_verify_ben_entitlement_checklist_no, string as_pre_1993_annuity_eligibility_flag, string as_converted_flag);// nf_insert_annuity_eligibility 
// 
Long ll_annuity_eligibility_no 

ll_annuity_eligibility_no = nf_get_next_annuity_eligibility_no(1) 

INSERT ANNUITY_ELIGIBILITY
(      annuity_account_no,
       annuity_eligibility_no,
		 annuity_eligibility_status_code,
		 confirmed_date, 
       confirmed_by_user_id,
		 annuity_start_date,
		 annuity_eligibility_end_date_used,
		 annuity_end_date,
		 benefit_option_code, 
       annuity_set_aside_percent_no,
		 annuity_set_aside_percent,
		 annuity_eligibility_reason_code,
		 annuity_eligibility_comment, 
		 confirm_annuity_eligibility_checklist_no,
		 verify_benefit_entitlement_checklist_no,
		 pre_1993_annuity_eligibility_flag,
		 converted_flag )
SELECT :al_annuity_account_no,
       :ll_annuity_eligibility_no,
		 :as_annuity_eligibility_status_code,
		 :adtm_confirmed_date, 
		 :as_confirmed_by_user_id,
		 :adtm_annuity_start_date,
		 :adtm_annuity_eligibility_end_date_used,
		 :adtm_annuity_end_date,
		 :as_benefit_option_code, 
		 :al_annuity_set_aside_percent_no,
		 :adec_annuity_set_aside_percent,
		 :as_annuity_eligibility_reason_code,
		 :as_annuity_eligibility_comment,
		 :al_confirm_annuity_elig_checklist_no,
		 :al_verify_ben_entitlement_checklist_no,
		 :as_pre_1993_annuity_eligibility_flag,
		 :as_converted_flag 
 USING SQLCA ; 

SQLCA.nf_handle_error('n_common_annuity', '', 'nf_insert_annuity_eligilibity - INSERT ANNUITY_ELIGIBILITY') 

RETURN ll_annuity_eligibility_no

end function

on n_common_annuity.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_common_annuity.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

