$PBExportHeader$w_dup_fix_individual.srw
forward
global type w_dup_fix_individual from w_ancestor
end type
type cb_open_event_log from commandbutton within w_dup_fix_individual
end type
type st_2 from statictext within w_dup_fix_individual
end type
type st_total from statictext within w_dup_fix_individual
end type
type cb_search from commandbutton within w_dup_fix_individual
end type
type dw_main_name from u_dw_online within w_dup_fix_individual
end type
type dw_medicare from u_dw_online within w_dup_fix_individual
end type
type dw_sin from u_dw_online within w_dup_fix_individual
end type
type dw_individual_list from u_dw_online within w_dup_fix_individual
end type
type cb_save from commandbutton within w_dup_fix_individual
end type
type cb_build_individual from commandbutton within w_dup_fix_individual
end type
type cb_cancel from commandbutton within w_dup_fix_individual
end type
type dw_search_criteria from u_dw_online within w_dup_fix_individual
end type
type dw_claim_list from u_dw_online within w_dup_fix_individual
end type
type dw_next_individual_no from u_dw_online within w_dup_fix_individual
end type
type dw_individual_name from u_dw_online within w_dup_fix_individual
end type
type dw_individual from u_dw_online within w_dup_fix_individual
end type
end forward

global type w_dup_fix_individual from w_ancestor
integer y = 48
integer width = 2770
integer height = 2788
string title = "Duplicate Individual Repair"
string menuname = "m_cmwb_notools"
windowtype windowtype = main!
long backcolor = 67108864
cb_open_event_log cb_open_event_log
st_2 st_2
st_total st_total
cb_search cb_search
dw_main_name dw_main_name
dw_medicare dw_medicare
dw_sin dw_sin
dw_individual_list dw_individual_list
cb_save cb_save
cb_build_individual cb_build_individual
cb_cancel cb_cancel
dw_search_criteria dw_search_criteria
dw_claim_list dw_claim_list
dw_next_individual_no dw_next_individual_no
dw_individual_name dw_individual_name
dw_individual dw_individual
end type
global w_dup_fix_individual w_dup_fix_individual

type prototypes

end prototypes

type variables
String		  is_SearchType		// Contains the current search type value
String		  is_BasicClaimSyntax
String                      is_stats_syntax
String		  is_SQLSyntax		// Contains the SQL used to modify the select statement on d_claim_search
String		  is_WhereClause		// Contains standard where clause appended to end of SQL statement
String		  is_SQLSyntaxI		// Contains the SQL used to modify the select statement on d_claim_search
String		  is_WhereClauseI		// Contains standard where clause appended to end of SQL statement
String                      is_or_condition

// Drop Down Data Windows

DataWindowChild	  idwc_claim_status_type_code
DataWindowChild	  idwc_claim_manager

// Search criteria values

String		  is_last_name		// Contains last name search value 
String		  is_first_name		// Contains first name search value
String		  is_name_search_option	// Contains name type search indicator ('B'egins with, 'E'quals or 'L'ike)
String		  is_soundex_search_option
	
Long		  il_claim_no		// Contains current selected claim number
Long		  il_sin			// Contains SIN search value
Long		  il_medicare		// Contains Medicare search value
Date		  id_birth_date		// Contains Birth date search value

DateTime		  idt_from_date		// Contains from date used to determine due/overdue
DateTime		  idt_to_date		// Contains to date used to determine due/overdue
String		  is_review_type		// Contains type of due/overdue search to be performed
String		  is_overdue_ind		// Contains ind used to determine if only overdue claims are to be listed
STRING is_first_entry		// determines where the bracket which begins the additional where clause is placed

BOOLEAN ib_sin, ib_medicare, ib_birthdate // These values have been validated for search
BOOLEAN ib_force_plan
N_INDIVIDUAL inv_individual
LONG il_selected_cnt = 0
LONG                         il_individuals_to_archive[]

STRING    is_court_order
DATASTORE ids_indiv_list

LONG		il_new_individual_no
DATETIME 	idtm_death_date

end variables

forward prototypes
public function integer wf_retrieve ()
public function integer wf_validate_sin (long al_sin)
public function integer wf_validate_medicare (long al_medicare)
public function string wf_get_old_individual_nos ()
public function integer wf_t5007_update (s_old_individual_array astr_old_individuals, long al_new_individual)
public function integer wf_t5007_y2k (s_old_individual_array astr_old_individuals, long al_new_individual)
public function integer wf_t5007_annuity (s_old_individual_array astr_old_individuals, long al_new_individual)
public function integer wf_t5007_history (s_old_individual_array astr_old_individuals, long al_new_individual)
public function integer wf_check_trustee_recipient (s_old_individual_array astr_old_individs)
public function integer wf_t5007_trustee_recipient_history (s_old_individual_array astr_old_individuals, long al_new_individual)
public function integer wf_t5007_trustee_history (s_old_individual_array astr_old_individuals, long al_new_individual)
public function integer wf_set_query ()
public function integer wf_validate_criteria ()
public function integer wf_set_defaults (datastore ads_indiv_list)
public function integer wf_log_court_order_event ()
public function integer wf_set_individual ()
public function integer wf_compare (datastore ads_indiv_list, boolean ab_save)
public function integer wf_update_save_annuity_eligibility (long al_individual_no, date adt_birth_date, date adt_death_date)
public function integer wf_add_event (boolean ab_merge_completed)
public function integer wf_save ()
public function integer wf_compare_individuals (datastore ads_indiv_list, boolean ab_save)
public function long wf_get_default_individual ()
public function long wf_update_bank_events (datastore ads_indiv_list)
public function integer wf_insert_entity (long al_individual_no)
public function boolean wf_check_wif_entity (long al_individual_no[], ref string as_package_type_code)
end prototypes

public function integer wf_retrieve ();LONG	ll_ReturnCode

/*	This function retrieves d_individual_list.
	It returns the number of rows retrieved.
	Set the .Redraw option to false while the datawindow is being
	loaded.
*/


/*	Reset the search list datawindow so that a rowfocuschange will always
	fire off -- then retrieve
*/
	dw_individual_list.Reset()

	IF ib_force_plan THEN
		EXECUTE IMMEDIATE 'set forceplan on' USING SQLCA;
		IF SQLCA.nf_handle_error('Setting force plan on', 'u_claim_search', 'uf_retrieve') < 0 THEN
			Return -1
		END IF
	END IF
	ll_ReturnCode = dw_individual_list.Retrieve()
	IF ib_force_plan THEN
		EXECUTE IMMEDIATE 'set forceplan off' USING SQLCA;
		IF SQLCA.nf_handle_error('Setting force plan off', 'u_claim_search', 'uf_retrieve') < 0 THEN
			Return -1
		END IF
	END IF
	IF SQLCA.nf_handle_error("u_claim_search","dw_individual_list","uf_retrieve") < 0 Then
		Return -1
	END IF

	dw_individual_list.SetFocus()


	Return ll_ReturnCode
end function

public function integer wf_validate_sin (long al_sin);LONG	ll_max, ll_loop, ll_check_rows, ll_individual_no, ll_loop2

/*	retrieve any individuals with the same SIN as the new individual
	and make sure they all appear in the list to be replaced
*/
	IF inv_individual.nf_check_digit(al_sin, 'S') < 0 THEN
		Return -1
	END IF

	IF al_sin > 0 THEN
		ll_max = dw_sin.Retrieve(al_sin)
/*		ignore all history individuals
*/
		dw_sin.SetFilter('history_flag = "N"')
		dw_sin.Filter()
		ll_max = dw_sin.RowCount()
		ll_loop = 1
		DO WHILE ll_loop <= ll_max
/* 		make sure that all entries retrieved are included in the list to merge
			- if one is not there then an individual exists with the same SIN and need to be merged
*/
			ll_individual_no = dw_sin.GetItemNumber(ll_loop,'individual_no')
			ll_check_rows = dw_individual_list.RowCount()
			ll_loop2 = 1
			DO WHILE ll_loop2 <= ll_check_rows
				IF dw_individual_list.GetItemNumber(ll_loop2,'individual_no') = ll_individual_no THEN
					EXIT
				END IF
				ll_loop2 = ll_loop2 + 1
			LOOP
			IF ll_loop2 > ll_check_rows THEN
				MessageBox('Duplicate SIN','Another individual, that is currently not selected, exists with the same SIN.')
				Return -1
			END IF
			ll_loop = ll_loop + 1
		LOOP
	END IF
Return 0
end function

public function integer wf_validate_medicare (long al_medicare);LONG	ll_max, ll_loop, ll_check_rows, ll_individual_no, ll_loop2

/*	retrieve any individuals with the same medicare number as the new individual
	and make sure they all appear in the list to be replaced
*/
	IF inv_individual.nf_check_digit(al_medicare, 'M') < 0 THEN
		Return -1
	END IF

	IF al_medicare > 0 THEN
		ll_max = dw_medicare.Retrieve(al_medicare)
/*		ignore all history individuals
*/
		dw_medicare.SetFilter('history_flag = "N"')
		dw_medicare.Filter()
		ll_max = dw_medicare.RowCount()
		ll_loop = 1
		DO WHILE ll_loop <= ll_max
/* 		make sure that all entries retrieved are included in the list to merge
			- if one is not there then an individual exists with the same medicare number and needs to be merged
*/
			ll_individual_no = dw_medicare.GetItemNumber(ll_loop,'individual_no')
			ll_check_rows = dw_individual_list.RowCount()
			ll_loop2 = 1
			DO WHILE ll_loop2 <= ll_check_rows
				IF dw_individual_list.GetItemNumber(ll_loop2,'individual_no') = ll_individual_no THEN
					EXIT
				END IF
				ll_loop2 = ll_loop2 + 1
			LOOP
			IF ll_loop2 > ll_check_rows THEN
				MessageBox('Duplicate Medicare Number','Another individual, that is currently not selected, exists with the same medicare number.')
				Return -1
			END IF
			ll_loop = ll_loop + 1
		LOOP
	END IF
Return 0
end function

public function string wf_get_old_individual_nos ();STRING     ls_individual_nos
INTEGER   li_counter, li_max

li_max = dw_individual_list.Rowcount()

FOR li_counter = 1 to li_max
	IF dw_individual_list.Object.selected_individual[li_counter] = 'Y'  THEN // include if it was chosen to be merged.
		ls_individual_nos = ls_individual_nos + String(dw_individual_list.Object.individual_no[li_counter]) + ', '
	END IF
NEXT

ls_individual_nos = ' ' + Left(ls_individual_nos, (Len(ls_individual_nos)-2))  // add space at beginning, remove comma & space at end

RETURN ls_individual_nos
end function

public function integer wf_t5007_update (s_old_individual_array astr_old_individuals, long al_new_individual);IF wf_t5007_y2k(astr_old_individuals, al_new_individual) < 0 THEN RETURN -1
IF wf_t5007_annuity(astr_old_individuals, al_new_individual) < 0 THEN RETURN -1
IF wf_t5007_history(astr_old_individuals, al_new_individual) < 0 THEN RETURN -1

IF wf_check_trustee_recipient(astr_old_individuals) < 0 THEN RETURN -1

IF wf_t5007_trustee_history(astr_old_individuals, al_new_individual) < 0 THEN RETURN -1
IF wf_t5007_trustee_recipient_history(astr_old_individuals, al_new_individual) < 0 THEN RETURN -1

RETURN 0
end function

public function integer wf_t5007_y2k (s_old_individual_array astr_old_individuals, long al_new_individual);STRING		ls_old_flag, ls_current_flag
INTEGER		li_upper, li_counter, li_rows, li_new_row, li_delete_rows
U_DS			lds_y2k_old, lds_y2k_new, lds_y2k_delete
BOOLEAN	lb_merged

// compare annuity flags amongst all records to be merged
// if any are 'Y' then flag will be 'Y', else 'N'

lds_y2k_old = Create u_ds
lds_y2k_old.DataObject = 'ds_t5_y2k_old'
lds_y2k_old.SetTransObject(SQLCA)

li_upper = UpperBound(astr_old_individuals.ll_old_individual)

IF li_upper > 0 THEN
	FOR li_counter = 1 TO li_upper
		li_rows = lds_y2k_old.Retrieve(astr_old_individuals.ll_old_individual[li_counter])
		
		IF li_rows = 1 THEN
			lb_merged = TRUE
			ls_current_flag = lds_y2k_old.Object.annuity_payout_flag[1]
			
			IF ls_current_flag = 'Y' OR ls_old_flag = 'Y' THEN
				ls_old_flag = 'Y'
			ELSE
				ls_old_flag = 'N'
			END IF
		ELSEIF li_rows > 1 THEN
			Error.is_Type = 'N'    // not a database error
			Error.Text = "WorkBench had more than one record for old recipient number: " + String(astr_old_individuals.ll_old_individual[li_counter]) + "."
			Error.windowmenu = "w_dup_fix_individual"
			Error.objectevent = "wf_t5007_y2k"
			Error.Line = 17
			SQLCA.nf_rollback_transaction()
			SignalError()
		ELSEIF li_rows < 0 THEN
			Error.is_Type = 'N'    // not a database error
			Error.Text = "WorkBench had a retrieval error for old recipient number: " + String(astr_old_individuals.ll_old_individual[li_counter]) + "."
			Error.windowmenu = "w_dup_fix_individual"
			Error.objectevent = "wf_t5007_y2k"
			Error.Line = 17
			SQLCA.nf_rollback_transaction()
			SignalError()
		ELSE
			// no individuals to be merged by that old number
		END IF
	NEXT
ELSE
	MessageBox("","No individuals to be merged.")

	RETURN -1
END IF

IF lb_merged THEN // insert new row and delete all the old ones
	lds_y2k_new = Create u_ds
	lds_y2k_new.DataObject = 'ds_t5_y2k_new'
	lds_y2k_new.SetTransObject(SQLCA)
	
	li_new_row = lds_y2k_new.InsertRow(0)
	lds_y2k_new.Object.t5007_recipient_no[li_new_row] = al_new_individual
	lds_y2k_new.Object.annuity_payout_flag[li_new_row] = ls_old_flag
	
	lds_y2k_new.Update()
	IF SQLCA.nf_handle_error('Embedded SQL: Retrieve T5007_HISTORY','w_dup_fix_individual','wf_t5007_y2k') < 0 THEN
		RETURN -1
	END IF
	
	lds_y2k_delete = Create u_ds
	lds_y2k_delete.DataObject = 'ds_t5_y2k_delete'
	lds_y2k_delete.SetTransObject(SQLCA)
	
	li_delete_rows = lds_y2k_delete.Retrieve(astr_old_individuals.ll_old_individual)
	
	IF li_delete_rows > 0 THEN
		FOR li_counter = 1 TO li_delete_rows
			lds_y2k_delete.DeleteRow(0)
		NEXT
		
		lds_y2k_delete.Update()
		IF SQLCA.nf_handle_error('Embedded SQL: Retrieve T5007_HISTORY','w_dup_fix_individual','wf_t5007_y2k') < 0 THEN
			RETURN -1
		END IF
	END IF
END IF

RETURN 0
end function

public function integer wf_t5007_annuity (s_old_individual_array astr_old_individuals, long al_new_individual);STRING		ls_old_flag, ls_current_flag
INTEGER		li_upper, li_counter, li_rows, li_new_row, li_delete_rows
U_DS			lds_annuity_old, lds_annuity_new, lds_annuity_delete
BOOLEAN	lb_merged

// compare y2k flags amongst all records to be merged
// if any are 'Y' then flag will be 'Y', else 'N'

lds_annuity_old = Create u_ds
lds_annuity_old.DataObject = 'ds_t5_annuity_old'
lds_annuity_old.SetTransObject(SQLCA)

li_upper = UpperBound(astr_old_individuals.ll_old_individual)

IF li_upper > 0 THEN
	FOR li_counter = 1 TO li_upper
		li_rows = lds_annuity_old.Retrieve(astr_old_individuals.ll_old_individual[li_counter])
		
		IF li_rows = 1 THEN
			lb_merged = TRUE
			ls_current_flag = lds_annuity_old.Object.y2k_payment_flag[1]
			
			IF ls_current_flag = 'Y' OR ls_old_flag = 'Y' THEN
				ls_old_flag = 'Y'
			ELSE
				ls_old_flag = 'N'
			END IF
		ELSEIF li_rows > 1 THEN
			Error.is_Type = 'N'    // not a database error
			Error.Text = "WorkBench had more than one record for old recipient number: " + String(astr_old_individuals.ll_old_individual[li_counter]) + "."
			Error.windowmenu = "w_dup_fix_individual"
			Error.objectevent = "wf_t5007_annuity"
			Error.Line = 17
			SQLCA.nf_rollback_transaction()
			SignalError()
		ELSEIF li_rows < 0 THEN
			Error.is_Type = 'N'    // not a database error
			Error.Text = "WorkBench had a retrieval error for old recipient number: " + String(astr_old_individuals.ll_old_individual[li_counter]) + "."
			Error.windowmenu = "w_dup_fix_individual"
			Error.objectevent = "wf_t5007_annuity"
			Error.Line = 17
			SQLCA.nf_rollback_transaction()
			SignalError()
		ELSE
			// no individuals to be merged by that old number
		END IF
	NEXT
ELSE
	MessageBox("","No individuals to be merged.")
	RETURN -1
END IF

IF lb_merged THEN // insert new row and delete all the old ones
	lds_annuity_new = Create u_ds
	lds_annuity_new.DataObject = 'ds_t5_annuity_new'
	lds_annuity_new.SetTransObject(SQLCA)
	
	li_new_row = lds_annuity_new.InsertRow(0)
	lds_annuity_new.Object.t5007_recipient_no[li_new_row] = al_new_individual
	lds_annuity_new.Object.y2k_payment_flag[li_new_row] = ls_old_flag
	
	lds_annuity_new.Update()
	IF SQLCA.nf_handle_error('Embedded SQL: Retrieve T5007_HISTORY','w_dup_fix_individual','wf_t5007_annuity') < 0 THEN
		RETURN -1
	END IF
	
	lds_annuity_delete = Create u_ds
	lds_annuity_delete.DataObject = 'ds_t5_annuity_delete'
	lds_annuity_delete.SetTransObject(SQLCA)
	
	li_delete_rows = lds_annuity_delete.Retrieve(astr_old_individuals.ll_old_individual)
	
	IF li_delete_rows > 0 THEN
		FOR li_counter = 1 TO li_delete_rows
			lds_annuity_delete.DeleteRow(0)
		NEXT
		
		lds_annuity_delete.Update()
		IF SQLCA.nf_handle_error('Embedded SQL: Retrieve T5007_HISTORY','w_dup_fix_individual','wf_t5007_annuity') < 0 THEN
			RETURN -1
		END IF
		
	END IF
END IF

RETURN 0
end function

public function integer wf_t5007_history (s_old_individual_array astr_old_individuals, long al_new_individual);u_ds				lds_t5_history_year, lds_t5_history_old[], lds_t5_history_new[], lds_arch_t5_history_new[], lds_t5_history_delete
STRING			ls_current_printed_flag, ls_old_printed_flag, ls_printed_flag_array[]
STRING         ls_current_cra_file_extract_flag, ls_old_cra_file_extract_flag, ls_cra_file_extract_flag_array[]
STRING         ls_current_amended_flag, ls_old_amended_flag, ls_amended_flag_array[], ls_current_retro_flag, ls_old_retro_flag, ls_retro_flag_array[]
DOUBLE			ldbl_history_old_amt, ldbl_history_current_amt, ldbl_history_amt_array[]
DATETIME		ldt_current_printed, ldt_old_printed, ldt_printed_array[]
DATETIME		ldt_current_cra_file_extract_date, ldt_old_cra_file_extract_date, ldt_cra_file_extract_date_array[]
BOOLEAN		lb_merged
INTEGER 		li_rows, li_t5_rows, li_counter, li_inner_counter, li_year, li_history_rows, li_upper, li_return
INTEGER			li_row, li_current_individual, li_individ_counter, li_new_row
LONG				ll_recipient_no

boolean lb_test


lds_t5_history_year = Create u_ds
lds_t5_history_year.DataObject = 'ds_t5_history_year'
li_return = lds_t5_history_year.SetTransObject(SQLCA)
	
li_rows = lds_t5_history_year.Retrieve()

FOR li_counter = 1 to li_rows // year by year
	li_year = lds_t5_history_year.Object.taxation_year[li_counter]
	
	li_upper = UpperBound(astr_old_individuals.ll_old_individual)
	
	lds_t5_history_old[li_counter] = Create u_ds
	lds_t5_history_old[li_counter].DataObject = 'ds_t5_history_old'
	lds_t5_history_old[li_counter].SetTransObject(SQLCA)
	
	FOR li_individ_counter = 1 TO li_upper  // individual by individual
		
		li_t5_rows = lds_t5_history_old[li_counter].Retrieve(astr_old_individuals.ll_old_individual[li_individ_counter], li_year)
			
		IF SQLCA.nf_handle_error('Embedded SQL: Retrieve T5007_HISTORY','w_dup_fix_individual','wf_t5007_history') < 0 THEN
			RETURN -1
		END IF
		
		IF li_t5_rows = 1 THEN
			lb_merged = TRUE
			
			ldbl_history_current_amt = lds_t5_history_old[li_counter].Object.total_benefit_amount[1]
			ldbl_history_old_amt += ldbl_history_current_amt
			
			// printed data
			ls_current_printed_flag = lds_t5_history_old[li_counter].Object.printed_flag[1]
			IF ls_current_printed_flag = 'Y' OR ls_old_printed_flag = 'Y' THEN
				ls_old_printed_flag = 'Y'
			ELSE
				ls_old_printed_flag = 'N'
			END IF
				
			ldt_current_printed = lds_t5_history_old[li_counter].Object.printed_date[1]
			IF (ldt_current_printed > ldt_old_printed AND ls_old_printed_flag = 'Y') OR IsNull(ldt_old_printed) THEN
				ldt_old_printed = ldt_current_printed
			END IF
			
			// amended flag data
			ls_current_amended_flag = lds_t5_history_old[li_counter].Object.amended_flag[1]
			IF ls_current_amended_flag = 'Y' OR ls_old_amended_flag = 'Y' THEN
				ls_old_amended_flag = 'Y'
			ELSE
				ls_old_amended_flag = 'N'
			END IF
			
			// retro flag data
			ls_current_retro_flag = lds_t5_history_old[li_counter].Object.retro_flag[1]
			IF ls_current_retro_flag = 'Y' OR ls_old_retro_flag = 'Y' THEN
				ls_old_retro_flag = 'Y'
			ELSE
				ls_old_retro_flag = 'N'
			END IF
						
			// extracted data
			ls_current_cra_file_extract_flag = lds_t5_history_old[li_counter].Object.cra_file_extract_flag[1]
			IF ls_current_cra_file_extract_flag = 'Y' OR ls_old_cra_file_extract_flag = 'Y' THEN
				ls_old_cra_file_extract_flag = 'Y'
			ELSE
				ls_old_cra_file_extract_flag = 'N'
			END IF
				
			ldt_current_cra_file_extract_date = lds_t5_history_old[li_counter].Object.cra_file_extract_date[1]
			IF (ldt_current_cra_file_extract_date > ldt_old_cra_file_extract_date AND ls_old_cra_file_extract_flag = 'Y') OR IsNull(ldt_old_cra_file_extract_date) THEN
				ldt_old_cra_file_extract_date = ldt_current_cra_file_extract_date
			END IF
			
		ELSEIF li_t5_rows > 1 THEN
			Error.is_Type = 'N'    // not a database error
			Error.Text = "WorkBench had more than one record for old recipient number: " + String(astr_old_individuals.ll_old_individual[li_counter]) + "."
			Error.windowmenu = "w_dup_fix_individual"
			Error.objectevent = "wf_t5007_history"
			Error.Line = 30
			SQLCA.nf_rollback_transaction()
			SignalError()
		ELSEIF li_t5_rows < 0 THEN
			Error.is_Type = 'N'    // not a database error
			Error.Text = "WorkBench had a retrieval error for old recipient number: " + String(astr_old_individuals.ll_old_individual[li_counter]) + "."
			Error.windowmenu = "w_dup_fix_individual"
			Error.objectevent = "wf_t5007_history"
			Error.Line = 30
			SQLCA.nf_rollback_transaction()
			SignalError()
		ELSE
			// no individuals to be merged
		END IF
	NEXT // individual for that year
	
	// set values for new individual for the year
	ldbl_history_amt_array[li_counter]          = ldbl_history_old_amt
	ls_amended_flag_array[li_counter]           = ls_old_amended_flag
	ls_retro_flag_array[li_counter]             = ls_old_retro_flag
	ls_printed_flag_array[li_counter]           = ls_old_printed_flag
	ldt_printed_array[li_counter]               = ldt_old_printed
	ls_cra_file_extract_flag_array[li_counter]  = ls_old_cra_file_extract_flag
	ldt_cra_file_extract_date_array[li_counter] = ldt_old_cra_file_extract_date
	
	IF lb_merged THEN
	
		lds_t5_history_new[li_counter] = Create u_ds
		lds_t5_history_new[li_counter].DataObject = 'ds_t5_history_new'
		lds_t5_history_new[li_counter].SetTransObject(SQLCA)
		
		li_new_row = lds_t5_history_new[li_counter].InsertRow(0)
		lds_t5_history_new[li_counter].Object.t5007_recipient_no[li_new_row]		= al_new_individual
		lds_t5_history_new[li_counter].Object.taxation_year[li_new_row]			= lds_t5_history_year.Object.taxation_year[li_counter]
		lds_t5_history_new[li_counter].Object.last_name[li_new_row]					= dw_main_name.Object.last_name[1]
		lds_t5_history_new[li_counter].Object.given_names[li_new_row]				= dw_main_name.Object.given_names[1]
		lds_t5_history_new[li_counter].Object.address_line1[li_new_row]			= dw_individual.Object.address_line1[1]
		lds_t5_history_new[li_counter].Object.address_line2[li_new_row]			= dw_individual.Object.address_line2[1]
		lds_t5_history_new[li_counter].Object.city[li_new_row]						= dw_individual.Object.city[1]
		lds_t5_history_new[li_counter].Object.prov_state_code[li_new_row]			= dw_individual.Object.prov_state_code[1]
		lds_t5_history_new[li_counter].Object.country_code[li_new_row]				= dw_individual.Object.country_code[1]
		lds_t5_history_new[li_counter].Object.postal_code[li_new_row]				= dw_individual.Object.postal_code[1]
		lds_t5_history_new[li_counter].Object.location_code[li_new_row]			= dw_individual.Object.location_code[1]
		lds_t5_history_new[li_counter].Object.location_type_code[li_new_row]		= dw_individual.Object.location_type_code[1]
		lds_t5_history_new[li_counter].Object.total_benefit_amount[li_new_row]	= ldbl_history_amt_array[li_counter]
		lds_t5_history_new[li_counter].Object.printed_flag[li_new_row]				= ls_printed_flag_array[li_counter]
		lds_t5_history_new[li_counter].Object.printed_date[li_new_row]				= ldt_printed_array[li_counter]
		lds_t5_history_new[li_counter].Object.amended_flag[li_new_row]				= ls_amended_flag_array[li_counter]
		lds_t5_history_new[li_counter].Object.retro_flag[li_new_row]				= ls_retro_flag_array[li_counter]
		lds_t5_history_new[li_counter].Object.cra_file_extract_flag[li_new_row]	= ls_cra_file_extract_flag_array[li_counter]
		lds_t5_history_new[li_counter].Object.cra_file_extract_date[li_new_row]	= ldt_cra_file_extract_date_array[li_counter]
		
		lds_t5_history_new[li_counter].Update()
		SQLCA.nf_handle_error('Embedded SQL: Retrieve T5007_HISTORY','w_dup_fix_individual','wf_t5007_history')
		
	END IF
	lb_merged = FALSE  // reset
	ldbl_history_old_amt = 0
	ls_old_printed_flag = ''
	SetNull(ldt_old_printed)
	
	ls_old_cra_file_extract_flag = ''
	SetNull(ldt_old_cra_file_extract_date)
	
	ls_old_amended_flag = ''
	ls_old_retro_flag   = ''
	
NEXT // year

// archive and delete, by individuals and then by year
FOR li_individ_counter = 1 TO li_upper
	ll_recipient_no = astr_old_individuals.ll_old_individual[li_individ_counter]
	
	FOR li_counter = 1 TO li_rows
		li_year = lds_t5_history_year.Object.taxation_year[li_counter]
		
		INSERT ARCHIVE_T5007_HISTORY
		(      new_t5007_recipient_no,  taxation_year,        t5007_recipient_no,      last_name,               given_names,
		       address_line1,           address_line2,        city,                    prov_state_code,         country_code,
		       postal_code,             location_code,        location_type_code,      total_benefit_amount,    printed_flag,
		       printed_date,            amended_flag,         retro_flag,              cra_file_extract_flag,   cra_file_extract_date,
		       original_create_date,    original_modify_date, original_create_user_id, original_modify_user_id )
		SELECT :al_new_individual,      taxation_year,        t5007_recipient_no,      last_name,               given_names,
		       address_line1,           address_line2,        city,                    prov_state_code,         country_code,
		       postal_code,             location_code,        location_type_code,      total_benefit_amount,    printed_flag,
		       printed_date,            amended_flag,         retro_flag,              cra_file_extract_flag,   cra_file_extract_date,
		       create_date,             modify_date,          create_user_id,          modify_user_id
		FROM  T5007_HISTORY
		WHERE t5007_recipient_no = :ll_recipient_no
		AND   taxation_year      = :li_year
		USING SQLCA;		
		SQLCA.nf_handle_error('Embedded SQL: Retrieve T5007_HISTORY','w_dup_fix_individual','wf_t5007_history')
		
		
		DELETE T5007_HISTORY
		WHERE  t5007_recipient_no = :ll_recipient_no
		AND    taxation_year = :li_year
		USING SQLCA;
		SQLCA.nf_handle_error('Embedded SQL: Retrieve T5007_HISTORY','w_dup_fix_individual','wf_t5007_history')
	NEXT
NEXT

RETURN 0
end function

public function integer wf_check_trustee_recipient (s_old_individual_array astr_old_individs);INTEGER		li_upper, li_counter, li_inner_counter, li_count, li_rows
LONG			ll_individ_check, ll_individ_check_against
STRING		ls_individs
U_DS			lds_trustee, lds_recip

li_upper = UpperBound(astr_old_individs.ll_old_individual)

lds_trustee = Create U_DS
lds_trustee.DataObject = 'ds_t5_t_h_trustee_checker'
lds_trustee.SetTransObject(SQLCA)
li_rows = lds_trustee.Retrieve(astr_old_individs.ll_old_individual)

IF li_rows > 1 THEN
	ls_individs = wf_get_old_individual_nos()
	Error.is_Type = 'N'    // not a database error
	Error.Text = "WorkBench is attempting to merge two T5007 trustees for the following individuals:" + ls_individs +"."
	Error.windowmenu = "w_dup_fix_individual"
	Error.objectevent = "wf_check_trustee_recipient"
	Error.Line = 11
	SQLCA.nf_rollback_transaction()
	SignalError()
END IF

lds_recip = Create U_DS
lds_recip.DataObject = 'ds_t5_t_h_recip_checker'
lds_recip.SetTransObject(SQLCA)
li_rows = lds_recip.Retrieve(astr_old_individs.ll_old_individual)

IF li_rows > 1 THEN
	ls_individs = wf_get_old_individual_nos()
	Error.is_Type = 'N'    // not a database error
	Error.Text = "WorkBench is attempting to merge two T5007 trustee recipients for the following individuals:" + ls_individs +"."
	Error.windowmenu = "w_dup_fix_individual"
	Error.objectevent = "wf_check_trustee_recipient"
	Error.Line = 28
	SQLCA.nf_rollback_transaction()
	SignalError()
END IF

FOR li_counter = 1 TO li_upper
	FOR li_inner_counter = li_counter+1 TO li_upper
		ll_individ_check             = astr_old_individs.ll_old_individual[li_counter]
		ll_individ_check_against = astr_old_individs.ll_old_individual[li_inner_counter]
		
		SELECT	Count(*)
		INTO		:li_count
		FROM		T5007_TRUSTEE_HISTORY a, T5007_TRUSTEE_HISTORY b
		WHERE	(a.trustee_individual_no = :ll_individ_check
					AND b.t5007_recipient_no = :ll_individ_check_against)
		OR			(a.t5007_recipient_no = :ll_individ_check
					AND b.trustee_individual_no = :ll_individ_check_against)
		USING SQLCA;
		
		IF SQLCA.nf_handle_error('Embedded SQL: Retrieve T5007_TRUSTEE_HISTORY','w_dup_fix_individual','wf_check_trustee_recipient') < 0 THEN
			RETURN -1
		END IF
		
		IF li_count > 0 THEN
			ls_individs = wf_get_old_individual_nos()
			Error.is_Type = 'N'    // not a database error
			Error.Text = "WorkBench is attempting to merge a T5007 trustee with a T5007 recipient for the following individuals:" + ls_individs +"."
			Error.windowmenu = "w_dup_fix_individual"
			Error.objectevent = "wf_check_trustee_recipient"
			Error.Line = 47
			SQLCA.nf_rollback_transaction()
			SignalError()
			EXIT
		END IF
	NEXT
NEXT

RETURN 0
end function

public function integer wf_t5007_trustee_recipient_history (s_old_individual_array astr_old_individuals, long al_new_individual);u_ds				lds_t5_trustee_history_year, lds_t5_trustee_history_claim
u_ds				lds_t5_trustee_history_old[], lds_t5_trustee_history_new[], lds_arch_t5_trustee_history_new[], lds_t5_history_delete
STRING			ls_current_comment, ls_old_comment, ls_comment_array[]
DOUBLE			ldbl_history_old_amt, ldbl_history_current_amt, ldbl_history_amt_array[]
DATETIME		ldt_current_create, ldt_old_create, ldt_create_array[]
BOOLEAN		lb_merged
INTEGER 		li_rows, li_t5_rows, li_counter, li_inner_counter, li_year, li_upper, li_return
INTEGER			li_row, li_individ_counter, li_new_row
LONG				ll_recipient_no

// For T5007_TRUSTEE_HISTORY
// We will allow a single record to be replaced with a new one as long as the following business rules apply:
// If it is either a trustee or a recipient being replaced, not both.
// If it is a single trustee recipient being replaced, not multiple trustee recipients being merged into one recipient.


lds_t5_trustee_history_year = Create u_ds
lds_t5_trustee_history_year.DataObject = 'ds_t5_trustee_history_year'
li_return = lds_t5_trustee_history_year.SetTransObject(SQLCA)
	
li_rows = lds_t5_trustee_history_year.Retrieve()

FOR li_counter = 1 to li_rows // year by year
	li_year = lds_t5_trustee_history_year.Object.taxation_year[li_counter]
	
	li_upper = UpperBound(astr_old_individuals.ll_old_individual)
	
	lds_t5_trustee_history_old[li_counter] = Create u_ds
	lds_t5_trustee_history_old[li_counter].DataObject = 'ds_t5_trustee_history_recipient_old'
	lds_t5_trustee_history_old[li_counter].SetTransObject(SQLCA)
	
	FOR li_individ_counter = 1 TO li_upper // duplicate individual by individual
		
		li_t5_rows = lds_t5_trustee_history_old[li_counter].Retrieve(li_year, astr_old_individuals.ll_old_individual[li_individ_counter])
			
		IF SQLCA.nf_handle_error('Embedded SQL: Retrieve T5007_TRUSTEE_HISTORY','w_dup_fix_individual','wf_t5007_trustee_recipient_history') < 0 THEN
			RETURN -1
		END IF

		IF li_t5_rows = 1 THEN
			lb_merged = TRUE
			
			ldbl_history_current_amt = lds_t5_trustee_history_old[li_counter].Object.total_benefit_amount[1]
			ldbl_history_old_amt += ldbl_history_current_amt
			
			ldt_current_create = lds_t5_trustee_history_old[li_counter].Object.create_date[1]
			ls_current_comment = lds_t5_trustee_history_old[li_counter].Object.comment[1]
			
			IF (ldt_current_create > ldt_old_create) OR IsNull(ldt_old_create) THEN
				ls_old_comment = ls_current_comment
				ldt_old_create = ldt_current_create
			END IF

		ELSEIF li_t5_rows > 1 THEN
			Error.is_Type = 'N'    // not a database error
			Error.Text = "WorkBench had more than one record for old T5007 recipient number: " + String(astr_old_individuals.ll_old_individual[li_counter]) + "." &
			+ "Trustee recipient may have more than one claim this year."
			Error.windowmenu = "w_dup_fix_individual"
			Error.objectevent = "wf_t5007_trustee_recipient_history"
			Error.Line = 36
			SQLCA.nf_rollback_transaction()
			SignalError()
		ELSEIF li_t5_rows < 0 THEN
			Error.is_Type = 'N'    // not a database error
			Error.Text = "WorkBench had a retrieval error for old T5007 recipient number: " + String(astr_old_individuals.ll_old_individual[li_counter]) + "."
			Error.windowmenu = "w_dup_fix_individual"
			Error.objectevent = "wf_t5007_trustee_recipient_history"
			Error.Line = 36
			SQLCA.nf_rollback_transaction()
			SignalError()
		ELSE
			// no individuals to be merged
		END IF

	NEXT // individual for that year
	
	// set values for new individual for the year
	ldbl_history_amt_array[li_counter] = ldbl_history_old_amt
	ls_comment_array[li_counter] = ls_old_comment
	
	IF lb_merged AND li_t5_rows = 1 THEN
	
		lds_t5_trustee_history_new[li_counter] = Create u_ds
		lds_t5_trustee_history_new[li_counter].DataObject = 'ds_t5_trustee_history_new'
		lds_t5_trustee_history_new[li_counter].SetTransObject(SQLCA)
		
		li_new_row = lds_t5_trustee_history_new[li_counter].InsertRow(0)
		
		lds_t5_trustee_history_new[li_counter].Object.trustee_individual_no[li_new_row]	= lds_t5_trustee_history_old[li_counter].Object.trustee_individual_no[1]
		lds_t5_trustee_history_new[li_counter].Object.t5007_recipient_no[li_new_row]	= al_new_individual
		lds_t5_trustee_history_new[li_counter].Object.taxation_year[li_new_row]			= li_year
		lds_t5_trustee_history_new[li_counter].Object.total_benefit_amount[li_new_row]	= ldbl_history_amt_array[li_counter]
		lds_t5_trustee_history_new[li_counter].Object.comment[li_new_row]					= ls_comment_array[li_counter]
		
		lds_t5_trustee_history_new[li_counter].Update()
		IF SQLCA.nf_handle_error('Embedded SQL: Retrieve T5007_HISTORY','w_dup_fix_individual','wf_t5007_trustee_recipient_history') < 0 THEN
			RETURN -1
		END IF
		
	END IF
	
NEXT // year

// archive and delete, by individuals and then by year
FOR li_individ_counter = 1 TO li_upper
	ll_recipient_no = astr_old_individuals.ll_old_individual[li_individ_counter]
	
	FOR li_counter = 1 TO li_rows
		li_year = lds_t5_trustee_history_year.Object.taxation_year[li_counter]
		
		INSERT ARCHIVE_T5007_TRUSTEE_HISTORY
		(new_t5007_recipient_no,		new_trustee_individual_no,		taxation_year,				t5007_recipient_no,
		trustee_individual_no,			claim_no,							total_benefit_amount,		comment,
		original_create_date,				original_modify_date,				original_create_user_id,	original_modify_user_id)
		SELECT
		:al_new_individual,				trustee_individual_no,			taxation_year,				t5007_recipient_no,
		trustee_individual_no,      		claim_no,                				total_benefit_amount,		comment,
		create_date,						modify_date,						create_user_id,				modify_user_id
		FROM T5007_TRUSTEE_HISTORY
		WHERE t5007_recipient_no = :ll_recipient_no
		AND     taxation_year         = :li_year
		USING SQLCA;
		
		IF SQLCA.nf_handle_error('Embedded SQL: Retrieve T5007_HISTORY','w_dup_fix_individual','wf_t5007_trustee_recipient_history') < 0 THEN
			RETURN -1
		END IF
		
		UPDATE T5007_TRUSTEE_HISTORY
		SET		t5007_recipient_no = :al_new_individual
		WHERE	t5007_recipient_no = :ll_recipient_no
		AND		taxation_year         = :li_year
		USING	SQLCA;
		
		IF SQLCA.nf_handle_error('Embedded SQL: Retrieve T5007_HISTORY','w_dup_fix_individual','wf_t5007_trustee_recipient_history') < 0 THEN
			RETURN -1
		END IF
	NEXT
NEXT

RETURN 0
end function

public function integer wf_t5007_trustee_history (s_old_individual_array astr_old_individuals, long al_new_individual);u_ds				lds_t5_trustee_history_year, lds_t5_trustee_history_claim
u_ds				lds_t5_trustee_history_old[], lds_t5_trustee_history_new[], lds_arch_t5_trustee_history_new[], lds_t5_history_delete
STRING			ls_current_comment, ls_old_comment, ls_comment_array[], ls_individs
DOUBLE			ldbl_history_old_amt, ldbl_history_current_amt, ldbl_history_amt_array[]
DATETIME		ldt_current_create, ldt_old_create, ldt_create_array[]
BOOLEAN		lb_merged
INTEGER 		li_rows, li_t5_rows, li_counter, li_inner_counter, li_year, li_upper, li_return
INTEGER			li_row, li_individ_counter, li_new_row,li_dup_trustee_counter, li_upper_trustee
LONG				ll_trustee_no

boolean lb_test

// For T5007_TRUSTEE_HISTORY
// We will allow a single record to be replaced with a new one as long as the following business rules apply:
// If it is either a trustee or a recipient being replaced, not both.
// If it is a single trustee being replaced, not multiple trustees being merged into one trustee.
// each trustee can only be involved in a single claim per year.
	
lds_t5_trustee_history_year = Create u_ds
lds_t5_trustee_history_year.DataObject = 'ds_t5_trustee_history_year'
li_return = lds_t5_trustee_history_year.SetTransObject(SQLCA)
	
li_rows = lds_t5_trustee_history_year.Retrieve()

FOR li_counter = 1 to li_rows // year by year
	li_year = lds_t5_trustee_history_year.Object.taxation_year[li_counter]
	
	li_upper = UpperBound(astr_old_individuals.ll_old_individual) // individuals being replaced
	
	lds_t5_trustee_history_old[li_counter] = Create u_ds
	lds_t5_trustee_history_old[li_counter].DataObject = 'ds_t5_trustee_history_old'
	lds_t5_trustee_history_old[li_counter].SetTransObject(SQLCA)
	
	FOR li_individ_counter = 1 TO li_upper // duplicate individual by individual
		
		li_t5_rows = lds_t5_trustee_history_old[li_counter].Retrieve(li_year, astr_old_individuals.ll_old_individual[li_individ_counter]) // by year & individ
			
		IF SQLCA.nf_handle_error('Embedded SQL: Retrieve T5007_TRUSTEE_HISTORY','w_dup_fix_individual','wf_t5_individual_duplicate') < 0 THEN
			RETURN -1
		END IF
		
		IF li_t5_rows = 1 THEN
			//li_upper_trustee = UpperBound(lb_merged)
		
			lb_merged = TRUE
			
			ldbl_history_current_amt = lds_t5_trustee_history_old[li_counter].Object.total_benefit_amount[1]
			ldbl_history_old_amt += ldbl_history_current_amt
			
			ldt_current_create = lds_t5_trustee_history_old[li_counter].Object.create_date[1]
			ls_current_comment = lds_t5_trustee_history_old[li_counter].Object.comment[1]
			
			IF (ldt_current_create > ldt_old_create) OR IsNull(ldt_old_create) THEN
				ls_old_comment = ls_current_comment
				ldt_old_create = ldt_current_create
			END IF

		ELSEIF li_t5_rows > 1 THEN
			Error.is_Type = 'N'    // not a database error
			Error.Text = "WorkBench had more than one record for old recipient number: " + String(astr_old_individuals.ll_old_individual[li_counter]) + "." &
			+ "Trustee may have more than one claim this year."
			Error.windowmenu = "w_dup_fix_individual"
			Error.objectevent = "wf_t5007_trustee_history"
			Error.Line = 37
			SQLCA.nf_rollback_transaction()
			SignalError()
		ELSEIF li_t5_rows < 0 THEN
			Error.is_Type = 'N'    // not a database error
			Error.Text = "WorkBench had a retrieval error for old recipient number: " + String(astr_old_individuals.ll_old_individual[li_counter]) + "."
			Error.windowmenu = "w_dup_fix_individual"
			Error.objectevent = "wf_t5007_trustee_history"
			Error.Line = 37
			SQLCA.nf_rollback_transaction()
			SignalError()
		ELSE
			// no individuals to be merged
		END IF

	NEXT // individual for that year
	
	// set values for new individual for the year
	ldbl_history_amt_array[li_counter] = ldbl_history_old_amt
	ls_comment_array[li_counter] = ls_old_comment
	
	IF lb_merged AND li_t5_rows = 1 THEN
	
		lds_t5_trustee_history_new[li_counter] = Create u_ds
		lds_t5_trustee_history_new[li_counter].DataObject = 'ds_t5_trustee_history_new'
		lds_t5_trustee_history_new[li_counter].SetTransObject(SQLCA)
		
		li_new_row = lds_t5_trustee_history_new[li_counter].InsertRow(0)
		lds_t5_trustee_history_new[li_counter].Object.trustee_individual_no[li_new_row]	= al_new_individual
		lds_t5_trustee_history_new[li_counter].Object.t5007_recipient_no[li_new_row]    = lds_t5_trustee_history_old[li_counter].Object.t5007_recipient_no[1]
		lds_t5_trustee_history_new[li_counter].Object.taxation_year[li_new_row]			= li_year
		lds_t5_trustee_history_new[li_counter].Object.total_benefit_amount[li_new_row]	= ldbl_history_amt_array[li_counter]
		lds_t5_trustee_history_new[li_counter].Object.comment[li_new_row]					= ls_comment_array[li_counter]
		
		lds_t5_trustee_history_new[li_counter].Update()
		IF SQLCA.nf_handle_error('Embedded SQL: Retrieve T5007_HISTORY','w_dup_fix_individual','wf_t5007_trustee_history') < 0 THEN
			RETURN -1
		END IF
	END IF
	
NEXT // year

// archive and update, by individuals and then by year
FOR li_individ_counter = 1 TO li_upper
	ll_trustee_no = astr_old_individuals.ll_old_individual[li_individ_counter]
	
	FOR li_counter = 1 TO li_rows
		li_year = lds_t5_trustee_history_year.Object.taxation_year[li_counter]
		
		INSERT ARCHIVE_T5007_TRUSTEE_HISTORY
		(new_t5007_recipient_no,		new_trustee_individual_no,		taxation_year,				t5007_recipient_no,
		trustee_individual_no,			claim_no,							total_benefit_amount,		comment,
		original_create_date,				original_modify_date,				original_create_user_id,	original_modify_user_id)
		SELECT
		t5007_recipient_no,				:al_new_individual,				taxation_year,				t5007_recipient_no,
		trustee_individual_no,      		claim_no,                				total_benefit_amount,		comment,
		create_date,						modify_date,						create_user_id,				modify_user_id
		FROM T5007_TRUSTEE_HISTORY
		WHERE trustee_individual_no = :ll_trustee_no
		AND     taxation_year         = :li_year
		USING SQLCA;
		
		IF SQLCA.nf_handle_error('Embedded SQL: Retrieve T5007_HISTORY','w_dup_fix_individual','wf_t5007_trustee_history') < 0 THEN
			RETURN -1
		END IF
		
		UPDATE T5007_TRUSTEE_HISTORY
		SET		trustee_individual_no = :al_new_individual
		WHERE	trustee_individual_no = :ll_trustee_no
		AND		taxation_year         = :li_year
		USING	SQLCA;
		
		IF SQLCA.nf_handle_error('Embedded SQL: Retrieve T5007_HISTORY','w_dup_fix_individual','wf_t5007_trustee_history') < 0 THEN
			RETURN -1
		END IF
	NEXT
NEXT

RETURN 0
end function

public function integer wf_set_query ();STRING	ls_additional_whereclause, ls_modstring, ls_returncode, ls_and
BOOLEAN lb_no_sin, lb_no_medicare

/*	Build the WHERE clause for the basic claim search......
*/
	IF is_last_name > "" THEN
		IF is_name_search_option = "B" THEN
			ls_Additional_WhereClause = ls_Additional_WhereClause + " and (INDIVIDUAL_NAME.last_name like ~~~"" + is_last_name + "%~~~")"
		ELSEIF is_soundex_search_option = "0" THEN
			ls_Additional_WhereClause = ls_Additional_WhereClause + " and (INDIVIDUAL_NAME.last_name = ~~~"" + is_last_name + "~~~")"
		ELSE
			ls_Additional_WhereClause = ls_Additional_WhereClause + " and (INDIVIDUAL_NAME.soundex_last_name = Soundex(~~~"" + is_last_name + "~~~"))"
		END IF
	END IF
	
	IF is_first_name > "" THEN
		IF is_name_search_option = "B" THEN
			ls_Additional_WhereClause = ls_Additional_WhereClause + " and (INDIVIDUAL_NAME.given_names like ~~~"" + is_first_name + "%~~~")"
		ELSEIF is_soundex_search_option = "0" THEN
			ls_Additional_WhereClause = ls_Additional_WhereClause + " and (INDIVIDUAL_NAME.given_names = ~~~"" + is_first_name + "~~~")"
		ELSE
			ls_Additional_WhereClause = ls_Additional_WhereClause + " and (INDIVIDUAL_NAME.soundex_given_names = Soundex(~~~"" + is_first_name + "~~~"))"
		END IF
	END IF

	IF il_sin > 0 THEN
		ls_Additional_WhereClause = ls_Additional_WhereClause + " and ( (INDIVIDUAL.sin_no = " + string(il_sin) + ")"
	ELSE
		lb_no_sin = TRUE
	END IF
	
	IF il_medicare > 0 THEN
		IF is_first_entry = 'med' THEN
			ls_and = ' and ('
		ELSE				
			IF lb_no_sin THEN // first entry after names
				ls_and = ' and ('
			ELSE
				IF is_or_condition = 'Y' THEN
					ls_and = ' or '
				ELSE
					ls_and = ' and '
				END IF
			END IF
		END IF
		
		ls_Additional_WhereClause = ls_Additional_WhereClause + ls_and + " (INDIVIDUAL.medicare_no = " + string(il_medicare) + ")"
	ELSE
		lb_no_medicare = TRUE
	END IF
	
	IF String(id_birth_date) > "" THEN
		IF is_first_entry = 'date' THEN
			ls_and = ' and ('
		ELSE				
			IF lb_no_sin AND lb_no_medicare THEN // first entry after names
				ls_and = ' and ('
			ELSE
				IF is_or_condition = 'Y' THEN
					ls_and = ' or '
				ELSE
					ls_and = ' and '
				END IF
			END IF
		END IF
		
		ls_Additional_WhereClause = ls_Additional_WhereClause + ls_and + " (INDIVIDUAL.birth_date = ~~~"" + String(id_birth_date,"yyyy-mm-dd") + "~~~")"
	END IF

/*	Modify the data window syntax of both the search list datawindow and the search list individual.....   
*/

	IF Len(ls_Additional_WhereClause) > 0 THEN
		IF ib_sin or ib_medicare or ib_birthdate THEN // add the extra enclosing bracket
			ls_ModString = "DataWindow.Table.Select=~"" + is_SQLSyntax + is_WhereClause + ls_Additional_WhereClause + ")~""
		ELSE
			ls_ModString = "DataWindow.Table.Select=~"" + is_SQLSyntax + is_WhereClause + ls_Additional_WhereClause + "~""
		END IF
	ELSE
		ls_ModString = "DataWindow.Table.Select=~"" + is_SQLSyntax + is_WhereClause + "~""
	END IF
	
	ls_ReturnCode = dw_individual_list.Modify(ls_ModString)
	IF Len(ls_ReturnCode) > 0 THEN
		Return -1
	ELSE
		Return 1
	END IF

Return 0

end function

public function integer wf_validate_criteria ();
	dw_search_criteria.AcceptText()

	is_last_name          = dw_search_criteria.GetItemString(1,"last_name")
	is_first_name         = dw_search_criteria.GetItemString(1,"first_name")
	is_name_search_option = dw_search_criteria.GetItemString(1,"name_search_option")
	is_soundex_search_option = dw_search_criteria.GetItemString(1,"soundex_search_option")
	il_sin                = Long(dw_search_criteria.GetItemString(1,"sin"))
	il_medicare           = Long(dw_search_criteria.GetItemString(1,"medicare"))
	id_birth_date         = dw_search_criteria.GetItemdate(1,"birth_date")
	is_or_condition		 = dw_search_criteria.GetItemString(1,'condition_or')

/*	Aug20/96 -error if soundex is on then name search must be exact
*/	
	IF is_soundex_search_option = '1' THEN
		is_name_search_option = 'E'
	END IF
	IF (IsNull(is_last_name) OR is_last_name = "") AND &
		(IsNull(is_first_name) OR is_first_name = "") AND &
		(IsNull(il_sin) OR il_sin = 0) AND &
		(IsNull(il_medicare) OR il_medicare = 0) AND &
		IsNull(id_birth_date) THEN
		MessageBox("Invalid Search Criteria","No search values have been entered!",Exclamation!)
		Return -1
	END IF

	IF Pos(is_last_name,'"') > 0 THEN
		MessageBox("Invalid Search Criteria","Last Name cannot contain double quotation marks!",Exclamation!)
		Return -1
	END IF

	IF Pos(is_last_name,'%') > 0 THEN
		MessageBox("Invalid Search Criteria","Last Name cannot contain percent sign!",Exclamation!)
		Return -1
	END IF

	IF Pos(is_last_name,'_') > 0 THEN
		MessageBox("Invalid Search Criteria","Last Name cannot contain an underscore!",Exclamation!)
		Return -1
	END IF
	
	IF is_last_name > "" THEN
		IF Len(is_last_name) < 3 THEN
			MessageBox("Invalid Search Criteria","Last Name must contain at least 3 characters!",Exclamation!)
			Return -1
		ELSE // a valid last name
			is_first_entry = 'last_name'
		END IF
	END IF

	IF Pos(is_first_name,'"') > 0 THEN
		MessageBox("Invalid Search Criteria","First Name cannot contain double quotation marks!",Exclamation!)
		Return -1
	END IF

	IF Pos(is_first_name,'%') > 0 THEN
		MessageBox("Invalid Search Criteria","First Name cannot contain percent sign!",Exclamation!)
		Return -1
	END IF

	IF Pos(is_first_name,'_') > 0 THEN
		MessageBox("Invalid Search Criteria","First Name cannot contain an underscore!",Exclamation!)
		Return -1
	END IF

	IF is_first_name > "" AND is_first_entry = "" THEN
		is_first_entry = 'first_name'
	END IF
		

	IF il_sin > 0 THEN
		IF f_CheckDigit(String(il_sin),8) < 0 THEN
			MessageBox("Invalid Search Criteria","Invalid Social Insurance Number!",Exclamation!)
			Return -1
		ELSE
			ib_sin = TRUE
			IF is_first_entry = "" THEN
				is_first_entry = 'sin'
			END IF			
		END IF
	END IF

	IF il_medicare > 0 THEN
		IF Len(String(il_medicare)) <> 9 THEN
			MessageBox("Invalid Search Criteria","Medicare Number must contain 9 digits.",Exclamation!)
			Return -1
		ELSE
			ib_medicare = TRUE
			IF is_first_entry = "" THEN
				is_first_entry = 'med'
			END IF
		END IF
		IF f_CheckDigit(String(il_medicare),8) < 0 THEN
			ib_medicare = FALSE
			is_first_entry = ""
			MessageBox("Invalid Search Criteria","Invalid Medicare Number!",Exclamation!)
			Return -1
		ELSE
			ib_medicare = TRUE
		END IF
	END IF

	IF id_birth_date <> 1900-01-01 THEN
		ib_birthdate = TRUE
		IF is_first_entry = "" THEN
			is_first_entry = 'date'
		END IF
	END IF

	IF is_or_condition = 'Y' THEN
		// PR 2210		
		IF ib_medicare OR ib_sin OR ib_birthdate THEN
			// this is OK, at least one of them has a proper value.
		ELSE
			MessageBox('Query Error', 'At least one of the SIN number, Medicare number or birth date must be filled in when the OR box is checked.')
			Return -1
		END IF
		
	END IF
Return 0
end function

public function integer wf_set_defaults (datastore ads_indiv_list); /******************************If merge is ALLOWED*******************************/

LONG    ll_med, ll_sin
LONG    ll_cntr, ll_rows
DATETIME ldtm_birth_date
STRING   ls_sex

ll_rows = ads_indiv_list.RowCount()

SetNull(ldtm_birth_date)

dw_individual.SetItem(1,'individual_no',0)
dw_main_name.SetItem(1,'last_name','')
dw_main_name.SetItem(1,'given_names','')
dw_individual.SetItem(1,'sex','')
dw_individual.SetItem(1,'language_code','')
dw_individual.SetItem(1,'sin_no',0)
dw_individual.SetItem(1,'medicare_no',0)
dw_individual.SetItem(1,'birth_date',ldtm_birth_date)

FOR ll_cntr = 1 to ll_rows

	ldtm_birth_date = ads_indiv_list.GetItemDateTime(ll_cntr, 'birth_date')
	ll_sin          = ads_indiv_list.GetItemNumber(ll_cntr, 'sin_no')
	ll_med          = ads_indiv_list.GetItemNumber(ll_cntr, 'medicare')
	ls_sex			 = ads_indiv_list.GetItemString(ll_cntr, 'gender')
	
	IF NOT ISNULL(ldtm_birth_date) AND ISNULL(dw_individual.GetItemDateTime(1,'birth_date')) THEN
		dw_individual.SetItem(1,'birth_date',ldtm_birth_date)
	END IF
	
	IF (NOT ISNULL(ll_sin) AND ll_sin > 0) AND (ISNULL(dw_individual.GetItemNumber(1,'sin_no')) OR dw_individual.GetItemNumber(1,'sin_no') = 0) THEN
		dw_individual.SetItem(1,'sin_no',ll_sin)
	END IF
	
	IF (NOT ISNULL(ll_med) AND ll_med > 0) AND (ISNULL(dw_individual.GetItemNumber(1,'medicare_no')) OR dw_individual.GetItemNumber(1,'medicare_no') = 0) THEN
		dw_individual.SetItem(1,'medicare_no',ll_med)
	END IF
	
	IF (ls_sex > '' AND ls_sex <> 'U') AND (ISNULL(dw_individual.GetItemString(1,'sex')) OR TRIM(dw_individual.GetItemString(1,'sex')) = '') THEN
		dw_individual.SetItem(1,'sex',ls_sex)
	END IF

NEXT

IF NOT ISNULL(dw_individual.GetItemDateTime(1,'birth_date')) THEN
	dw_individual.Modify("birth_date.Protect=1")
	dw_individual.Modify("birth_date.Background.Color='553648127'")
ELSE
	dw_individual.Modify("birth_date.Protect=0")
	dw_individual.Modify("birth_date.Background.Color='16777215'")	
END IF

IF NOT ISNULL(dw_individual.GetItemNumber(1,'sin_no')) AND dw_individual.GetItemNumber(1,'sin_no') > 0 THEN
	dw_individual.Modify("sin_no.Protect=1")
	dw_individual.Modify("sin_no.Background.Color='553648127'")
ELSE
	dw_individual.Modify("sin_no.Protect=0")
	dw_individual.Modify("sin_no.Background.Color='16777215'")
END IF

IF NOT ISNULL(dw_individual.GetItemNumber(1,'medicare_no')) AND dw_individual.GetItemNumber(1,'medicare_no') > 0 THEN
	dw_individual.Modify("medicare_no.Protect=1")
	dw_individual.Modify("medicare_no.Background.Color='553648127'")
ELSE
	dw_individual.Modify("medicare_no.Protect=0")
	dw_individual.Modify("medicare_no.Background.Color='16777215'")
END IF

IF TRIM(dw_individual.GetItemString(1,'sex')) > '' AND dw_individual.GetItemString(1,'sex') <> 'U' THEN
	dw_individual.Modify("sex.Protect=1")
ELSE
	dw_individual.Modify("sex.Protect=0")
END IF

RETURN 0
end function

public function integer wf_log_court_order_event ();LONG		ll_loop, ll_id, ll_claim_no, ll_event_no, ll_getrow, ll_claims, ll_loop2
STRING   ls_code, ls_desc, ls_new_code, ls_flag
DATASTORE lds_claims
n_event_log lnv_event_log

lnv_event_log = CREATE n_event_log

lds_claims = CREATE DATASTORE
lds_claims.DataObject = 'd_individual_claims'
lds_claims.SetTransObject(SQLCA)

ll_getrow = dw_individual.GetRow()

IF ll_getrow > 0 THEN 
	ls_new_code = dw_individual.GetItemString(ll_getrow, 'individual_court_order_flag')
	ll_id       = dw_individual.GetItemNumber(ll_getrow, 'individual_no')
	IF ls_new_code = 'Y' THEN
		ls_desc = dw_individual.GetItemString(ll_getrow, 'event_desc') 
		IF IsNull(ls_desc) THEN ls_desc = ''			

		ll_claims = lds_claims.Retrieve(ll_id)
		FOR ll_loop2 = 1 to ll_claims
			ll_claim_no = lds_claims.getitemnumber(ll_loop2,'claim_no')
			
			SELECT a.event_specific_code
			INTO   :ls_flag
			FROM   CLAIM_EVENT a
			WHERE  a.claim_no = :ll_claim_no
			AND    a.event_no = (SELECT MAX(b.event_no)  
										FROM   CLAIM_EVENT b
										WHERE  a.claim_no = b.claim_no 
										AND    b.event_type_code = '020' )
			USING SQLCA;
			
			SQLCA.nf_handle_error("w_dup_fix_individual", "", "wf_log_court_order_event, SELECT a.event_specific_code")
			
			IF ls_flag = 'Y' THEN
				ls_flag = 'N'
				CONTINUE
			END IF
			
			ll_event_no = lnv_event_log.nf_next_claim_event_no(ll_claim_no)
			lnv_event_log.nf_create_auto_event(ll_claim_no, ll_event_no,'020', ls_desc , ls_new_code)
		NEXT
	END IF
ELSE
	RETURN -1
END IF


RETURN 0
end function

public function integer wf_set_individual ();/*****************************************************************************/
/* PR4125 - J. Hawker 2004-10-15    										           											*/ 
/* This function has been mostly re-written. The default individual is now   											*/
/* selected by the user. This function now gets information                  												*/
/* (gender and birthdate) from all of the 'selected' individuals(individuals 											*/
/* being merged) and uses that to determine if they can be merged or not, by 									*/
/* calling the function wf_compare_individuals()									  										*/
/*****************************************************************************/
//PR19887 2014-11-17 David Worboys : Cleaned up variables & comments
LONG			ll_default_individual_no //PR19887 2014-11-17 David Worboys : Renamed to make clearer
LONG			ll_loop
LONG			ll_max
LONG			ll_row
//DATETIME 	ldtm_birthdate //PR19887 2014-11-17 David Worboys : Not used
STRING   	ls_court_order = 'N'
STRING   	ls_desc

ids_indiv_list = CREATE DATASTORE
ids_indiv_list.dataobject = 'd_indiv_list'

/*	
** Loop through the list and find the Default individual selected
*/
ll_max 						= dw_individual_list.RowCount()
ll_loop 						= 1
ll_default_individual_no 	= 0

DO WHILE ll_max >= ll_loop	
	/* 
	** If the individual is a 'selected' individual, get it's information
	*/
	IF dw_individual_list.GetItemString(ll_loop,'selected_individual') = 'Y' THEN
		
		ll_row = ids_indiv_list.InsertRow(0)
		
		ids_indiv_list.SetItem(ll_row, 'individual_no',dw_individual_list.GetItemNumber(ll_loop,'individual_no'))
		ids_indiv_list.SetItem(ll_row, 'birth_date',dw_individual_list.GetItemDateTime(ll_loop,'individual_birth_date'))
		ids_indiv_list.SetItem(ll_row, 'gender',dw_individual_list.GetItemString(ll_loop,'sex'))
		ids_indiv_list.SetItem(ll_row, 'last_name',dw_individual_list.GetItemString(ll_loop, 'last_name'))
		ids_indiv_list.SetItem(ll_row, 'given_name',dw_individual_list.GetItemString(ll_loop,'given_names'))
		ids_indiv_list.SetItem(ll_row, 'sin_no',dw_individual_list.GetItemNumber(ll_loop,'individual_sin_no'))
		ids_indiv_list.SetItem(ll_row, 'medicare',dw_individual_list.GetItemNumber(ll_loop,'individual_medicare_no'))
		ids_indiv_list.SetItem(ll_row, 'court_order',dw_individual_list.GetItemString(ll_loop,'court_order_flag'))
		ids_indiv_list.SetItem(ll_row, 'death_date',dw_individual_list.GetItemDateTime(ll_loop, 'individual_death_date'))
		
		ids_indiv_list.OBJECT.bank_no[ll_row] 				= dw_individual_list.OBJECT.individual_bank_no[ll_loop] 			//PR19887 2014-11-17 David Worboys - New
		ids_indiv_list.OBJECT.bank_transit_no[ll_row]	 	= dw_individual_list.OBJECT.individual_bank_transit_no[ll_loop] 	//PR19887 2014-11-17 David Worboys - New
		ids_indiv_list.OBJECT.bank_account_no[ll_row] 	= dw_individual_list.OBJECT.individual_bank_account_no[ll_loop]	 //PR19887 2014-11-17 David Worboys - New
		
		IF dw_individual_list.GetItemString(ll_loop,'court_order_flag') = 'Y' THEN
			ls_court_order = 'Y'			
			dw_individual.Object.individual_court_order_flag.Protect = 1
		END IF 
		
		/* 
		** If the individual is the 'default' individual, get it's individual number
		*/
		IF dw_individual_list.GetItemString(ll_loop,'default_individual') = 'Y' THEN
			ll_default_individual_no 	= dw_individual_list.GetItemNumber(ll_loop,'individual_no')
			il_sin 							= dw_individual_list.GetItemNumber(ll_loop,'individual_sin_no')
			il_medicare 					= dw_individual_list.GetItemNumber(ll_loop,'individual_medicare_no')
		END IF			
	END IF	
	ll_loop = ll_loop + 1	
LOOP

IF ll_default_individual_no > 0 THEN
	/* If there is an individual selected as the default individual,
		call wf_compare_individuals to make sure they are 'mergeable'.
		i.e.  Birthdates are the same(if entered), Genders are the 
		same(if entered). If not, do not allow to merge.
	*/
	//2014-11-14 David Worboys : I think the next line should be calling wf_compare_individual but will leave alone for now as not a reported problem
	IF wf_compare(ids_indiv_list, FALSE) < 0 THEN
		RETURN -1
	END IF
	
	dw_individual.Reset()
	
	IF inv_individual.nf_retrieve(ll_default_individual_no)	< 0 THEN
		RETURN -1
	END IF
	
	IF wf_set_defaults(ids_indiv_list) < 0 THEN
		RETURN -1
	END IF
	
	dw_individual.SetItem(1,'individual_court_order_flag', ls_court_order)
	dw_individual.SetItem(1,'event_desc',ls_desc)			
	dw_individual.SetItemStatus(1,0,Primary!,NewModified!)

	inv_individual.idtm_death_date  	= dw_individual.GetItemDateTime(1,'death_date')
	inv_individual.il_individual_no 		= ll_default_individual_no
ELSE
	MessageBox('No Individual','An individual must be selected as the default.',information!)
	RETURN -1
END IF

RETURN 0
end function

public function integer wf_compare (datastore ads_indiv_list, boolean ab_save);//2014-11-14 David Worboys : I think this function is old and wf_compare_individual should supercede this.
STRING  ls_msg, ls_gender, ls_gender_compare, ls_court_order, ls_court_order_comp
STRING  ls_given, ls_given_compare, ls_last, ls_last_compare
LONG    ll_med, ll_med_compare, ll_sin, ll_sin_compare, ll_start,ll_start2
LONG    ll_cntr, ll_rows, ll_cntr2, ll_last, ll_first, ll_court_order
LONG    ll_sin_cntr, ll_med_cntr, ll_death
DATETIME ldtm_birth_date, ldtm_bday_compare, ldtm_death_date, ldtm_death_compare

ll_rows = ads_indiv_list.RowCount()

ll_start = 1

IF ab_save THEN
	dw_individual.Accepttext()
	dw_main_name.Accepttext()
	ldtm_birth_date = dw_individual.GetItemDateTime(1, 'birth_date')
	ldtm_death_date = dw_individual.GetItemDateTime(1, 'death_date')
	ls_gender       = dw_individual.GetItemString(1, 'sex')
	ll_sin          = dw_individual.GetItemNumber(1, 'sin_no')
	ll_med          = dw_individual.GetItemNumber(1, 'medicare_no')
	ls_last         = dw_main_name.GetItemString(1, 'last_name')
	ls_given        = dw_main_name.GetItemString(1, 'given_names')
	ls_court_order  = dw_individual.GetItemString(1, 'individual_court_order_flag')
	ll_start  = ll_rows
	ll_start2 = 0	

END IF

FOR ll_cntr = ll_start to ll_rows
	
	IF ab_save = FALSE THEN
		ldtm_birth_date = ads_indiv_list.GetItemDateTime(ll_cntr, 'birth_date')
		ldtm_death_date = ads_indiv_list.GetItemDateTime(ll_cntr, 'death_date')
		ls_gender       = ads_indiv_list.GetItemString(ll_cntr, 'gender')
		ll_sin          = ads_indiv_list.GetItemNumber(ll_cntr, 'sin_no')
		ll_med          = ads_indiv_list.GetItemNumber(ll_cntr, 'medicare')
		ls_last         = ads_indiv_list.GetItemString(ll_cntr, 'last_name')
		ls_given        = ads_indiv_list.GetItemString(ll_cntr, 'given_name')
		ls_court_order  = ads_indiv_list.GetItemString(ll_cntr, 'court_order')
		ll_start = ll_cntr
	END IF
	
	FOR ll_cntr2 = ll_start2 + 1 to ll_rows
		ldtm_bday_compare   = ads_indiv_list.GetItemDateTime(ll_cntr2, 'birth_date')
		ldtm_death_compare  = ads_indiv_list.GetItemDateTime(ll_cntr2, 'death_date')
		ls_gender_compare   = ads_indiv_list.GetItemString(ll_cntr2, 'gender')
		ll_sin_compare      = ads_indiv_list.GetItemNumber(ll_cntr2, 'sin_no')
		ll_med_compare      = ads_indiv_list.GetItemNumber(ll_cntr2, 'medicare')
		ls_last_compare     = ads_indiv_list.GetItemString(ll_cntr2, 'last_name')
		ls_given_compare    = ads_indiv_list.GetItemString(ll_cntr2, 'given_name')
		ls_court_order_comp = ads_indiv_list.GetItemString(ll_cntr2, 'court_order')
		
		//Birthdate
		IF NOT ISNULL(ldtm_birth_date) AND NOT ISNULL(ldtm_bday_compare) THEN
			IF ldtm_birth_date <> ldtm_bday_compare THEN
				MessageBox('Cannot Merge','These individuals cannot be merged. The birthdates must be the same.',information!)
				RETURN -1
			END IF
		END IF
				
		//Gender
		IF (ls_gender = '' OR ls_gender = 'U')  AND ab_save THEN
			MessageBox('Invalid Gender','You must select Male or Female as the Sex before merging individuals.',information!)
			RETURN -1
		ELSEIF ls_gender <> 'U' AND ls_gender_compare <> 'U' THEN
			IF ls_gender <> ls_gender_compare THEN
				MessageBox('Cannot Merge','These individuals cannot be merged. The individuals have different genders.',information!)
				RETURN -1
			END IF
		END IF
		
		//SIN
		IF NOT ISNULL(ll_sin) AND NOT ISNULL(ll_sin_compare) THEN
			IF ll_sin <> 0 AND ll_sin_compare <> 0 THEN
				IF ll_sin <> ll_sin_compare THEN
					MessageBox('Cannot Merge','These individuals cannot be merged. The individuals have different Social Insurance Numbers.',information!)
					RETURN -1
				END IF
			END IF
		END IF
		
		//Medicare
		IF NOT ISNULL(ll_med) AND NOT ISNULL(ll_med_compare) THEN
			IF ll_med <> 0 AND ll_med_compare <> 0 THEN
				IF ll_med <> ll_med_compare THEN
					MessageBox('Cannot Merge','These individuals cannot be merged. The individuals have different Medicare Numbers.',information!)
					RETURN -1
				END IF
			END IF
		END IF
		
		//Last Name
		IF NOT ISNULL(ls_last) AND NOT ISNULL(ls_last_compare) THEN
			IF TRIM(ls_last) <> '' AND TRIM(ls_last_compare) <> '' THEN
				IF TRIM(ls_last) <> TRIM(ls_last_compare) THEN
					ll_last = ll_last + 1
				END IF
			END IF
		END IF
		
		//First Name
		IF NOT ISNULL(ls_given) AND NOT ISNULL(ls_given_compare) THEN
			IF TRIM(ls_given) <> '' AND TRIM(ls_given_compare) <> '' THEN
				IF TRIM(ls_given) <> TRIM(ls_given_compare) THEN
					ll_first = ll_first + 1
				END IF
			END IF
		END IF
		
		//Deathdate
		IF NOT ISNULL(ldtm_death_date) AND NOT ISNULL(ldtm_death_compare) THEN
			IF ldtm_death_date <> ldtm_death_compare THEN
				MessageBox('Cannot Merge','These individuals cannot be merged. The deathdates must be the same.',information!)
				RETURN -1
			END IF
		END IF
		
		IF (ISNULL(ldtm_death_date) AND NOT ISNULL(ldtm_death_compare)) OR (NOT ISNULL(ldtm_death_date) AND ISNULL(ldtm_death_compare)) THEN
			MessageBox('Cannot Merge','These individuals cannot be merged. You cannot merge deceased individuals with individuals who are not deceased.',information!)
			RETURN -1			
		END IF

	NEXT
NEXT

IF ll_last > 0 THEN 
	IF MessageBox('Warning!','These individuals have different Last Names. Would you like to continue with the merge?',question!,yesno!) = 2 THEN
		RETURN -1
	END IF
END IF

IF ll_first > 0 THEN
	IF MessageBox('Warning!','These individuals have different First Names. Would you like to continue with the merge?',question!,yesno!) = 2 THEN
		RETURN -1
	END IF
END IF

//IF ll_death > 0 THEN
//	IF MessageBox('Warning!','These individuals have different Death Dates. Would you like to continue with the merge?',question!,yesno!) = 2 THEN
//		RETURN -1
//	END IF
//END IF
	
RETURN 0
end function

public function integer wf_update_save_annuity_eligibility (long al_individual_no, date adt_birth_date, date adt_death_date);/*
note: THIS IS THE ORIGINAL - UNCHANGED
A change to a birth date for an injured worker or surviving spouse who is eligible for annuity benefits, 
where that change alters the value of the month or year of the birth date, requires that the annuity eligibility end date be ‘changed’.  
As Annuity Eligibility data is NOT overwritten, but rather made inactive and a new records created, 
a change of birth date year or month will require the following data updates:
o	The existing active ANNUITY_ELIBILITY record for the injured worker will be set to be inactive
o	A new ANNUITY _ELIGIBILITY record for the injured worker will be created with the same start date, corrected end date 
(1st of the month following their 65th birthday) and a reason code of ‘BDC’ for ‘Birth Date change’

An addition or change to a death date for an injured worker or surviving spouse who is eligible for annuity benefits, 
where the individual has not yet turned 65 years of age, requires that the annuity eligibility end date be ‘changed’. 
As Annuity Eligibility data is NOT overwritten, but rather made inactive and a new records created, a change of death date year or month 
will require the following data updates:
o	The existing active ANNUITY_ELIBILITY record for the individual will be set to be inactive
o	A new ANNUITY _ELIGIBILITY record for the individual will be created with the same start date, corrected end date 
(1st of the month following their death date) and a reason code of ‘DDC’ for ‘Death Date change’
o	Take the confirm date too?  Hopefully code is doing that

2.1.3	Business Rules 

The following business rules will be added to the BR_CMWB_Maintain_Individual document:

2.250 If an active annuity eligibility exists for the annuity account, and the birthdate is changed causing the annuity end date to be set in the future, 
and scheduled payments/awards exists after the new annuity end date, a message is displayed indicating the payments should be reviewed.

2.260 If an active annuity eligibility exists for the annuity account, and the birthdate is changed causing the annuity end date to be set in the past, 
and payments/awards exists after the new annuity end date, a message is displayed indicating the claims/payments should be reviewed.

2.270	If an active annuity eligibility exists for the annuity account, and the birth date is changed, the annuity eligibility end date must not be prior 
to the annuity eligibility start date.

2.280	If an active annuity eligibility exists for the annuity account, and the death date is changed, the annuity eligibility end date must not be prior 
to the annuity eligibility start date.	

note: function does update and Insert

RETURNS:
1 	- update/insert completed
0 	- no action required
-1 	- Error

NEW: 20090811
set aside percent no and percentage must be obtained from the Set_Aside_Percentage look-up table 
(Kevin has code that can be shared, if not accessed).  This happens because the method of determining 5% vs 10% is based 
on the end date value being 2009-01-01 or greater and the BDC/DDC change the end date (new)

NEW: 20090812
3.60		The annuity eligibility must end on the last day of the month in which the individual turns sixty-five years of age.
3.70		The annuity eligibility must end on the last day of the month in which the individual dies if death occurs before age sixty-five.

*/

LONG			ll_annuity_eligibility_no, ll_annuity_account_no, ll_confirm_annuity_elig_checklist_no
LONG		 	ll_verify_ben_entitlement_checklist_no
STRING		ls_annuity_eligibility_status_code, ls_benefit_option_code, ls_annuity_eligibility_reason_code, ls_annuity_eligibility_comment
STRING		ls_pre_1993, ls_converted_flag, as_type, ls_message, ls_claim_role_desc, ls_claim_role,ls_confirmed_by_user_id
DATETIME	   ldtm_annuity_start_date, ldtm_annuity_end_date
DATETIME	   ldtm_annuity_end_date_calculated, ldtm_confirmed_date
DATETIME	   ldtm_annuity_eligibility_end_date_changed, ldtm_annuity_eligibility_end_date_used
DECIMAL		ldec_annuity_set_aside_percent
INTEGER		li_cnt,  li_annuity_set_aside_percent_no
BOOLEAN     lb_check_death
DATE			ldt_birth_date_from_screen, ldt_death_date_from_screen

datastore  lds_annuity

lds_annuity 					= CREATE DATASTORE
lds_annuity.dataobject 	= 'd_annuity_eligibility'
lds_annuity.settransobject(sqlca)
li_cnt = lds_annuity.retrieve(al_individual_no)

SQLCA.nf_handle_error('w_dup_fix_individual', 'wf_update_save_annuity_eligibility()', 'li_cnt = lds_annuity.Retrieve(al_individual_no)')

IF li_cnt <> 1  THEN RETURN 0

//grab the birthdate/ deathdate entered on the screen
ldt_birth_date_from_screen 	= DATE(dw_individual.GetItemDatetime(1, 'birth_date') )
ldt_death_date_from_screen 	= DATE(dw_individual.GetItemDatetime(1, 'death_date') )

// Determine what has been changed
IF DATE(adt_death_date) <> ldt_death_date_from_screen 							THEN lb_check_death = TRUE
IF ISNULL(adt_death_date) AND NOT ISNULL(ldt_death_date_from_screen) 	THEN lb_check_death = TRUE
IF ISNULL(ldt_death_date_from_screen) AND NOT ISNULL(adt_death_date) 	THEN lb_check_death = TRUE

//if nothing changed return out
IF lb_check_death = FALSE THEN RETURN 0

n_common_annuity lnv_common_annuity
lnv_common_annuity = CREATE n_common_annuity

//grab the data we need from the datawindow
ldtm_annuity_start_date 				= lds_annuity.getitemdatetime(1, "annuity_start_date")
ldtm_annuity_end_date 				= lds_annuity.getitemdatetime(1, 'annuity_end_date')
ldec_annuity_set_aside_percent	= lds_annuity.getitemdecimal(1, 'annuity_set_aside_percent')
ls_benefit_option_code 				= lds_annuity.getitemstring(1, 'benefit_option_code')
ll_annuity_eligibility_no 				= lds_annuity.getitemnumber(1, 'annuity_eligibility_no')
ll_annuity_account_no 				= lds_annuity.getitemnumber(1, 'annuity_account_no')
ls_claim_role_desc                 		= lds_annuity.getitemstring(1, 'claim_role_desc')
ls_claim_role                 				= lds_annuity.getitemstring(1, 'claim_role_code')
	
//IF NO ANNUITY_ACCOUNT  return out.
IF ll_annuity_account_no = 0 THEN RETURN 0
	
IF  year(ldt_death_date_from_screen) + month(ldt_death_date_from_screen) = year(DATE(adt_death_date)) + month(DATE(adt_death_date))  THEN 
	RETURN 0
END IF 
		
// ldtm_annuity_end_date_calculated returned by reference
IF lnv_common_annuity.nf_get_annuity_end_date(al_individual_no, ls_claim_role_desc , ls_message, ldtm_annuity_end_date_calculated, ldtm_annuity_eligibility_end_date_changed, DATETIME(ldt_birth_date_from_screen), DATETIME(ldt_death_date_from_screen)) < 0 THEN
	SQLCA.nf_rollback_transaction()
	MessageBox('Validation Error', ls_message , Exclamation!)
	RETURN -1
END IF 
		
IF ISNULL(ldtm_annuity_end_date_calculated) THEN RETURN 0
//  reason code of ‘DDC’ for ‘Death Date change’
ls_annuity_eligibility_reason_code = 'DDC'
		
IF DATE(ldtm_annuity_end_date_calculated) < DATE(ldtm_annuity_start_date) THEN 
	SQLCA.nf_rollback_transaction()
	MessageBox('Validation Error', 'The Death Date has changed, the annuity eligibility end date must not be prior to the annuity eligibility start date!', Exclamation!)
	RETURN -1
END IF 

//Problem
IF ISNULL(ldtm_annuity_end_date_calculated) OR DATE(ldtm_annuity_end_date_calculated) = date('1900-01-01') THEN RETURN -1

SELECT annuity_account_no,
       annuity_eligibility_no,
		 annuity_eligibility_status_code,
		 confirmed_date,
		 confirmed_by_user_id,
		 annuity_start_date,
		 annuity_eligibility_end_date_used,
		 annuity_end_date,
		 benefit_option_code,
		 annuity_eligibility_comment,
		 confirm_annuity_eligibility_checklist_no,
		 verify_benefit_entitlement_checklist_no,
		 pre_1993_annuity_eligibility_flag,
       converted_flag
INTO   :ll_annuity_account_no,
       :ll_annuity_eligibility_no,
		 :ls_annuity_eligibility_status_code,
		 :ldtm_confirmed_date,
		 :ls_confirmed_by_user_id,
		 :ldtm_annuity_start_date,
		 :ldtm_annuity_eligibility_end_date_used,
		 :ldtm_annuity_end_date,
		 :ls_benefit_option_code,
		 :ls_annuity_eligibility_comment,
		 :ll_confirm_annuity_elig_checklist_no,
		 :ll_verify_ben_entitlement_checklist_no,
		 :ls_pre_1993,
		 :ls_converted_flag 
FROM 	ANNUITY_ELIGIBILITY
WHERE 	annuity_eligibility_status_code 	= 'A'
AND 		annuity_account_no 				= :ll_annuity_account_no
USING 	SQLCA;
		
SQLCA.nf_handle_error('n_individual', 'nf_modify_create_annuity_eligibility()', 'SELECT annuity_account_no - B')
	
// get the annuity pct for the role code, annuity end date, & benefit option
lnv_common_annuity.nf_get_annuity_percentage(ls_claim_role, ldtm_annuity_end_date_calculated, ls_benefit_option_code, ldec_annuity_set_aside_percent , li_annuity_set_aside_percent_no)
	
//SET ANNUITY_ELIBILITY TO inactive (what if it is pending?)
UPDATE 	ANNUITY_ELIGIBILITY 
SET  		annuity_eligibility_status_code 	= 'I' 
WHERE 	annuity_eligibility_status_code 	= 'A'
AND 		annuity_account_no 				= :ll_annuity_account_no
USING 	SQLCA;			
SQLCA.nf_handle_error('w_dup_fix_individual', 'wf_update_save_annuity_eligibility()', 'UPDATE ANNUITY_ELIGIBILITY')


IF IsNull(ldtm_annuity_eligibility_end_date_changed) THEN ldtm_annuity_eligibility_end_date_changed = ldtm_annuity_eligibility_end_date_used

//create the new record based on the old values and new values as determined from the DD
lnv_common_annuity.nf_insert_annuity_eligibility(	ll_annuity_account_no, ls_annuity_eligibility_status_code, ldtm_confirmed_date,  ls_confirmed_by_user_id, ldtm_annuity_start_date, ldtm_annuity_eligibility_end_date_used, ldtm_annuity_end_date_calculated, ls_benefit_option_code, li_annuity_set_aside_percent_no, ldec_annuity_set_aside_percent,ls_annuity_eligibility_reason_code, ls_annuity_eligibility_comment, ll_confirm_annuity_elig_checklist_no, ll_verify_ben_entitlement_checklist_no, ls_pre_1993, 'N' )
	
//default the annuity end date instance variable to be used ouside of script to inform the user
idtm_death_date = ldtm_annuity_end_date_calculated
	
			
RETURN 1
end function

public function integer wf_add_event (boolean ab_merge_completed);DATE			ldt_accident_date
INTEGER		li_counter, li_rowcount
LONG			ll_claim_no, ll_individual_no, ll_source_individual, ll_row
STRING		ls_last_name, ls_given_names, ls_message, ls_event_specific_code

S_WINDOW_MESSAGE lstr_message


/*
lstr_message.al_doubleparm[1] = al_claim_no
lstr_message.al_doubleparm[2] = al_individual_no

lstr_message.adt_DateParm[1] = adt_accident_date

lstr_message.as_StringParm[1] = as_claim_role_code
lstr_message.as_StringParm[2] = as_last_name
lstr_message.as_StringParm[3] = as_given_names
lstr_message.as_StringParm[4] = as_event_category_code
lstr_message.as_StringParm[5] = as_event_type_code
lstr_message.as_StringParm[6] = as_event_specific_code
lstr_message.as_StringParm[7] = as_allow_parameter_change
lstr_message.as_StringParm[8] = as_add_new_event
*/

// grab the individual from nvo and do the defaults
IF ab_merge_completed THEN
	ls_event_specific_code = 'COM'
	
	ll_individual_no = il_new_individual_no
	ls_message      =  'Individuals '
ELSE
	ls_event_specific_code = 'NRE'
	
	ll_row =  dw_individual_list.GetSelectedRow(0 )
	IF ISNULL(ll_row) OR ll_row = 0 THEN RETURN -1
	
	// grab the individual from nvo and do the defaults
	ll_individual_no = dw_individual_list.getitemnumber(ll_row, 'individual_no')
	
END IF

IF ISNULL(ll_individual_no) OR ll_individual_no = 0 THEN RETURN -1

// get names
SELECT	last_name, given_names
INTO		:ls_last_name, :ls_given_names
FROM		INDIVIDUAL
WHERE	individual_no = :ll_individual_no
USING SQLCA;
SQLCA.nf_handle_error('w_dup_fix_individual','wf_add_event()','SELECT last_name, given_names')


// now grab a claim_no
//   (1) - the selected individual must be associated with the claim
//   (2) - the claim must have the most recent accident date & be non-history amongst those claims above (1)
//   (3) - if no claims meet (2) then claim can be history amongst those claims above

SELECT 	Max(a.claim_no)
INTO 		:ll_claim_no
FROM 	   CLAIM_PARTICIPANT a
JOIN     CLAIM b ON a.claim_no = b.claim_no
WHERE 	a.individual_no = :ll_individual_no
AND      b.accident_date in ( select max(d.accident_date)
                              from   CLAIM_PARTICIPANT c
										join   CLAIM d on c.claim_no = d.claim_no
										where  c.individual_no = a.individual_no
										and    d.history_flag = 'N' )
USING 	SQLCA;		
SQLCA.nf_handle_error('w_dup_fix_individual','cb_open_event_log.clicked','SELECT max(claim_no) - 1 ')

IF ISNULL(ll_claim_no) OR ll_claim_no = 0 THEN
	SELECT 	Max(a.claim_no)
	INTO 		:ll_claim_no
	FROM 	   CLAIM_PARTICIPANT a
	JOIN     CLAIM b ON a.claim_no = b.claim_no
	WHERE 	a.individual_no = :ll_individual_no
	AND      b.accident_date in ( select max(d.accident_date)
											from   CLAIM_PARTICIPANT c
											join   CLAIM d on c.claim_no = d.claim_no
											where  c.individual_no = a.individual_no )
	USING 	SQLCA;		
	SQLCA.nf_handle_error('w_dup_fix_individual','cb_open_event_log.clicked','SELECT max(claim_no) - 2 ')
END IF
		
IF ISNULL(ll_claim_no) OR ll_claim_no = 0 THEN 
	messagebox('Claim Role',' The selected Individual is not a participant in any claims. Please review the selected Individual.')
	RETURN -1
END IF 

SELECT 	accident_date 
INTO 		:ldt_accident_date
FROM 	CLAIM 
WHERE 	claim_no = :ll_claim_no
USING 	SQLCA;
		
SQLCA.nf_handle_error('w_dup_fix_individual','wf_add_event()','SELECT accident_date ')


IF ab_merge_completed THEN
	// grab the Individual rowcount
	li_rowcount =  upperbound(il_individuals_to_archive)
	
	IF ISNULL(li_rowcount) OR li_rowcount = 0 THEN  RETURN -1
	
	
	// CREATE THE TEXT 
	FOR li_counter = 1 TO li_rowcount
			ll_source_individual = il_individuals_to_archive[li_counter]
			
			IF li_counter = li_rowcount THEN
				ls_message = ls_message + STRING(ll_source_individual)
			ELSE
				ls_message = ls_message + STRING(ll_source_individual) + ', '
			END IF 
			
	NEXT
	
	IF TRIM(ls_message) = '' THEN RETURN -1
	ls_message = ls_message + ' were merged into the new Individual ' + string (ll_individual_no) + " - " + ls_given_names + " " + ls_last_name
	
ELSE
	//set the message to nothing
	ls_message = ''
END IF

lstr_message.al_doubleparm[1] 	= ll_claim_no
lstr_message.al_doubleparm[2] 	= ll_individual_no
lstr_message.al_doubleparm[3] 	= 0
lstr_message.adt_DateParm[1] 	= ldt_accident_date
lstr_message.as_StringParm[1] 	= 'C'
lstr_message.as_StringParm[2] 	= ls_last_name
lstr_message.as_StringParm[3] 	= ls_given_names
lstr_message.as_StringParm[4] 	= 'I' //taken from Event_Category - Claim vs Individual
lstr_message.as_StringParm[5] 	= '042' //taken from Event_Type - Merge Individual
lstr_message.as_StringParm[6] 	= ls_event_specific_code
lstr_message.as_StringParm[7] 	= 'Y' //?????
lstr_message.as_StringParm[8] 	= 'Y' //?????
lstr_message.as_StringParm[9] 	= ls_message //put message here?????
lstr_message.as_StringParm[10]   = ''
lstr_message.as_StringParm[11]   = ''

OpenWithParm(w_event_log_response, lstr_message)

/*
added by Craig Gibb, July 2011
PR13522 - bug with il_individuals_to_archive[] array.
if two consecutive merges occured during a single run-time of the module, the 2nd merge's event text may show incorrect individual numbers.
all we have to do is reinitialize the array once its done with after each merge.
*/
 long ll_empty[]
 il_individuals_to_archive = ll_empty

RETURN 1
end function

public function integer wf_save ();// wf_save
//
//  Changes originaly made by Doug Filmore on Nov 16, 1999  re:  CMS consideration
//  Reaplied by Ed Lenarczyk  Jul 18, 2000  -  PR #1085   (done prev on Feb 22, 2000)
//
// 2014-11-19 PR19887 David Worboys : Addfunction functino call wf_update_bank_events
Long     ll_sin, ll_medicare, ll_loop, ll_max, ll_individual_no_delete, ll_individual_no_new, ll_xfer_cnt, ll_loop2
Long     ll_claim_no, ll_sub_claim_no, n, ll_no_of_same_indivduals_in_cms, ll_count = 0, ll_individual_no
Long     ll_upi[], ll_cnt, ll_upi_no, ll_no_of_same_ind_in_appeals, ll_ind_no[], ll_counter, ll_row
Integer  li_rows, li_rtn, li_find, li_rowcount
String   ls_errormessage, ls_last_name, ls_given_name, ls_sex, ls_address_line1, ls_address_line2, ls_find
String   ls_city, ls_prov_state_code, ls_country, ls_postal_code, ls_orig_sex, ls_message, ls_given_names, ls_package_type_code
STRING   ls_token_id
Datetime ldt_birth_date, ldt_death_date, ldt_orig_birth_date
Boolean  lb_birth_date_exists, lb_wif_entity_to_delete
DATASTORE  lds_annuity_account
INTEGER  li_x, li_inserted_row
LONG     ll_individual_list[], ll_records, ll_fatality_count, ll_individual_with_account, ll_no_minutes_to_expire
DATETIME	ldtm_account_birth, ldtm_account_death
U_DS     lds_annuity_payout_participant_for_individual
U_DS     lds_delete_wif_entity, lds_create_token, lds_workflow_token_related_mail_package

s_old_individual_array lstr_old_individuals

lds_annuity_account = CREATE DATASTORE
lds_annuity_account.DataObject = 'd_annuity_account_for_individual'
lds_annuity_account.SetTransObject(SQLCA)


SetPointer(HourGlass!)

//	- call the functions in the user object to validate the data
//	- generate a new individual number for this person
//	- remove the SIN and Medicare number before the validation, so it will not be validated
//	- validate the SIN and Medicare with window function
IF	dw_individual.AcceptText() < 0 THEN
	Return -1
END IF


dw_individual_list.SetFilter('selected_individual = "Y"')
dw_individual_list.Filter()
ll_max = dw_individual_list.RowCount()

// error check
IF dw_individual.RowCount() < 1 THEN
	MessageBox('No Individual','There is no individual information to save.')
	GOTO ErrorExit
END IF

ll_sin = dw_individual.GetItemNumber(1,'sin_no')
ll_medicare = dw_individual.GetItemNumber(1,'medicare_no')


//Set a flag that tells inv_individual not to validate the sin numbers uniqueness
//because it will find that it is not unique.  It doesn't know that we are archiving
//the individual that make the sin not unique
inv_individual.ib_validate_sin_uniqueness = False
inv_individual.ib_validate_medicare_uniqueness = False

IF inv_individual.nf_check_mandatory() < 0 THEN
	GOTO ErrorExit
END IF

//inv_individual needs a list of claim for the individual so it can validate
//each claims cost allocation.  We must retrieve the list of claims for 
//all selected individuals because they will become one.

For li_x = 1 to dw_individual_list.RowCount()
	ll_individual_list[li_x] = dw_individual_list.GetItemNumber(li_x,'individual_no')
Next

// use array ll_individual_list to find WIF_ENTITY
lb_wif_entity_to_delete = wf_check_wif_entity(ll_individual_list, ls_package_type_code)
IF lb_wif_entity_to_delete THEN	
	// datastore to delete current WIF_ENTITY & related records for the 'old' individual number
	lds_delete_wif_entity = Create U_DS
	lds_delete_wif_entity.DataObject = 'd_delete_wif_entity'
	lds_delete_wif_entity.SetTransObject(SQLCA)
	
	// datastore to create WIF_ENTITY & related records for the 'new' individual number
	lds_create_token = CREATE u_ds
	lds_create_token.DataObject =  "d_create_token"
	lds_create_token.SetTransObject(SQLCA)
END IF

li_rtn = dw_claim_list.Retrieve(ll_individual_list,0)
IF li_rtn  < 0 Then SignalError(-666,'Error retrieving claim list for selected individuals')
	
SQLCA.nf_handle_error('w_dup_fix_individual','wf_save','dw_claim_list.retrieve')

IF inv_individual.nf_check_bus_rule() < 0 THEN
	GOTO ErrorExit
END IF

/* PR4125 - J. Hawker - Birthdate, Gender, SIN, MED must match. Last/Given Names SHOULD match.
*/
IF wf_compare_individuals(ids_indiv_list,TRUE) < 0 THEN
	GOTO ErrorExit
END IF

// The birth date of an individual must not be removed once it is set.
lb_birth_date_exists = FALSE
ls_orig_sex = "U"

SetPointer(HourGlass!)

FOR n = 1 TO ll_max
	ldt_birth_date = dw_individual_list.GetItemDatetime(n, 'individual_birth_date')
	IF IsNull(ldt_birth_date) = FALSE OR IsDate(String(ldt_birth_date, "yyyy-mm-dd")) = TRUE THEN
		lb_birth_date_exists = TRUE
		ldt_orig_birth_date = ldt_birth_date
	END IF

	ls_sex = dw_individual_list.GetItemString(n, 'sex')
	IF ls_sex = "M" or ls_sex = "F" THEN
		ls_orig_sex = ls_sex
	END IF
NEXT

SetPointer(HourGlass!)

ll_row = dw_individual.GetRow()
IF ll_row > 0 THEN
	// Birth date must not be removed once it is set
	ldt_birth_date = dw_individual.GetItemDatetime(ll_row, "birth_date")
	IF IsDate(String(ldt_birth_date, "yyyy-mm-dd")) = FALSE AND lb_birth_date_exists = TRUE THEN
		MessageBox("Error", "Birth date must not be removed once it is set.")
		dw_individual.SetItem(ll_row, "birth_date", ldt_orig_birth_date)
		dw_individual.SetColumn('birth_date')
		dw_individual.SetFocus()
		RETURN -1
	END IF
	
	// The sex of an individual must not go from Male or Female to Not Specified
	ls_sex = dw_individual.GetItemString(ll_row, "sex")
	IF (ls_orig_sex = "M" OR ls_orig_sex = "F") AND ls_sex = "U" THEN
		MessageBox('Error', 'The sex of an individual must not go from Male or Female to Not Specified.')
		dw_individual.SetItem(ll_row, 'sex', ls_orig_sex)
		dw_individual.SetColumn('sex')
		dw_individual.SetFocus()
		RETURN -1
	END IF
END IF

SetPointer(HourGlass!)

//	if all is ok so far then validate the SIN and Medicare
IF wf_validate_sin(ll_sin) < 0 THEN
	GOTO ErrorExit
END IF

IF wf_validate_medicare(ll_medicare) < 0 THEN
	GOTO ErrorExit
END IF

IF inv_individual.nf_set_unused_fields() < 0 THEN
	GOTO ErrorExit
END IF

SetPointer(HourGlass!)

//  Changes originaly made by Doug Filmore on Nov 16, 1999  re:  CMS consideration
//  Reaplied by Ed Lenarczyk  Jul 18, 2000  -  PR # 1085   done prev on Feb 22, 2000
//  Minor changes done:  Do not blow out the whole application when can't merge due to CMS duplication
//                       Report individual numbers rather than UPIs (UPIs are not on the screen..)
//
// Added by D.F.  1999/11/16 for CMS - problem if individual_no's that are being re-assigned exist  in CMS tables
ll_loop  = 1
ll_no_of_same_indivduals_in_cms = 0 
DO WHILE ll_loop <= ll_max
	ll_individual_no_delete = dw_individual_list.GetItemNumber(ll_loop,'individual_no')
	ll_count = 0
	SELECT upi_no INTO :ll_upi_no 
	FROM PATIENT_DETAILS 
	where individual_no = :ll_individual_no_delete
	using SQLCA;
	
	IF SQLCA.nf_handle_error('Embedded SQL: SELECTING FROM PATIENT_DETAILS','w_dup_fix_individual','wf_save') < 0 THEN
		GOTO ErrorExit
	END If
		
	if SQLCA.SQLCode = 100  then
	else
	ll_no_of_same_indivduals_in_cms ++
		ll_upi[ll_no_of_same_indivduals_in_cms] = ll_upi_no
		ll_ind_no[ll_no_of_same_indivduals_in_cms] = ll_individual_no_delete
	END IF
	ll_loop = ll_loop + 1
LOOP

SetPointer(HourGlass!)
	
if ll_no_of_same_indivduals_in_cms > 1 then
	ls_errormessage = 'Cannot reassign - more than 1 WRC client exists for the individual being re-assigned.'
	if upperbound(ll_upi[]) > 0 then
		ls_errormessage = ls_errormessage +  '~nIndividual numbers / UPI numbers: '
	end if
	FOR ll_cnt = 1 TO upperbound(ll_upi[])
		if ll_cnt = upperbound(ll_upi[]) then
			ls_errormessage = ls_errormessage + ' ' + string(ll_ind_no[ll_cnt])	+ 				&
			        ' / ' + string(ll_upi[ll_cnt]) + '~nPlease note the UPI numbers' +		&
					  ' and contact WRC Admitting and Health Records to resolve'
		else
			ls_errormessage = ls_errormessage + ' ' + string(ll_ind_no[ll_cnt]) + ' / ' + string(ll_upi[ll_cnt]) + ';   '
		end if
	next
		
// Ed Lenarczyk - Change made not to terminate the whole application if selected
//                individuals can not be merged due to duplicate in the CMS
//                Original code (next 1 line) commented out
//
//if SQLCA.nf_handle_error('w_dup_fix_individual','wf_save',ls_errormessage) < 0 THEN
	messagebox("Problems with merging",ls_errormessage)
	cb_cancel.TriggerEvent(Clicked!)
	
	GOTO ErrorExit																	
end if
		
//1.220	At most, one (1) annuity account may be involved in a duplicate individual fix/merge.
lds_annuity_account.Retrieve(ll_individual_list)
SQLCA.nf_handle_error('w_dup_fix_individual','wf_save()','lds_annuity_account.Retrieve')


ls_find = 'count_in_one_annuity_payout <> 1'

IF lds_annuity_account.RowCount() > 1 THEN
	MessageBox('Error!','At most, one (1) annuity account may be involved in a duplicate individual fix/merge.~r~n' &
	         + 'Please contact HelpDesk, a datafix is required to correct Annuity Eligibility.',exclamation!)
	GOTO ErrorExit
END IF

IF lds_annuity_account.RowCount() = 1 THEN
	
	//grab the Individual with the annuity_account
	ll_individual_with_account = lds_annuity_account.getitemnumber(1,'individual_no')
	
	SELECT 	birth_date, death_date 
	INTO 		:ldtm_account_birth, :ldtm_account_death
	FROM 	INDIVIDUAL 
	WHERE 	individual_no = :ll_individual_with_account
	USING 	SQLCA;
	
	SQLCA.nf_handle_error('w_dup_fix_individual','wf_save()','select birth_date, death_date ')
END IF



// BR. 1.300  Individuals must not be merged, if they are involved in the same annuity payout.
lds_annuity_payout_participant_for_individual = Create U_DS
lds_annuity_payout_participant_for_individual.DataObject = 'd_annuity_payout_participant_for_individual'
lds_annuity_payout_participant_for_individual.SetTransObject(SQLCA)

li_rowcount = lds_annuity_payout_participant_for_individual.Retrieve(ll_individual_list)
SQLCA.nf_handle_error('w_dup_fix_individual','wf_save()','lds_annuity_payout_participant_for_individual.Retrieve')

ls_find = 'count_in_annuity_payout <> 1'
li_find = lds_annuity_payout_participant_for_individual.Find(ls_find,1,li_rowcount)

IF li_find <> 0 THEN
	MessageBox('Error!','Individuals must not be merged, if they are annuity payout participants involved in the same annuity payout. ' &
	         + '~r~n~r~nPlease refer this error message to the person who requested the merge. They may need to contact the HelpDesk, as they may need assistance from the IT Department.',exclamation!)
	GOTO ErrorExit
END IF


//2014-11-19 PR19887 David Worboys : Moved SQLCA.nf_begin_transaction()  Now in function wf_update_bank_events as a messagebox is located there 
//                                                        that can not be placed gere
//SQLCA.nf_begin_transaction()  
//
// 2014-11-19 PR19887 David Worboys : Added function below.
IF (wf_update_Bank_Events(ids_indiv_list) = -1) THEN
	GOTO ErrorExit //2014-11-19 PR19887 David Worboys - Darwin, I feel so dirty I have never used a GOTO in over 20 years!
END IF

//	now get the new individual number and update the dw to start the transaction
IF inv_individual.nf_set_identifiers() < 0 THEN
	GOTO ErrorExit
END IF

dw_next_individual_no.Update()
IF SQLCA.nf_handle_error('update dw individual no','w_dup_fix_individual','wf_save') < 0 THEN
	GOTO ErrorExit
END IF

dw_individual.Update()
IF SQLCA.nf_handle_error('update dw individual ','w_dup_fix_individual','wf_save') < 0 THEN
	GOTO ErrorExit
END IF

dw_individual_name.Update()
IF SQLCA.nf_handle_error('update dw individual name','w_dup_fix_individual','wf_save') < 0 THEN
	GOTO ErrorExit
END IF

dw_main_name.Update()
IF SQLCA.nf_handle_error('update dw main name','w_dup_fix_individual','wf_save') < 0 THEN
	GOTO ErrorExit
END IF

SetPointer(HourGlass!)

// New Individual already created !!!
ll_loop = 1
ll_individual_no_new = dw_individual.GetItemNumber(1,'individual_no')
ls_last_name         = dw_main_name.getitemstring(1,"last_name")
ls_given_name        = dw_main_name.getitemstring(1,"given_names")
ls_address_line1     = dw_individual.getitemstring(1,"address_line1")
ls_address_line2     = dw_individual.getitemstring(1,"address_line2")
ls_city              = dw_individual.getitemstring(1,"city")
ls_prov_state_code   = dw_individual.getitemstring(1,"prov_state_code")
ls_country           = dw_individual.getitemstring(1,"country_code")
ls_postal_code       = dw_individual.getitemstring(1,"postal_code")

//default an instance of the new individual_no to be used outside of this script
il_new_individual_no = ll_individual_no_new

IF ISNULL(ls_last_name)  THEN ls_last_name = ""
IF ISNULL(ls_given_name) THEN ls_last_name = ""

DO WHILE ll_loop <= ll_max
	ll_individual_no_delete = dw_individual_list.GetItemNumber(ll_loop,'individual_no')

	//	update all tables with the new individual number first check the Applied payment 
	// transactions to see if the individual was a recipient on any payments
	INSERT INTO APPLIED_CLAIM_TXN_AUDIT  
		( txn_no, claim_no, recipient_no,  new_recipient_no)
	SELECT txn_no, claim_no, recipient_no, :ll_individual_no_new
	  FROM APPLIED_CLAIM_TXN
	 WHERE recipient_no = :ll_individual_no_delete
		AND recipient_type_code = 'I'
	 USING SQLCA;

	IF SQLCA.nf_handle_error('Embedded SQL: Insert APPLIED_CLAIM_TXN_AUDIT','w_dup_fix_individual','wf_save') < 0 THEN
		GOTO ErrorExit
	END IF

	UPDATE APPLIED_CLAIM_TXN 
		SET recipient_no = :ll_individual_no_new
	 WHERE recipient_no = :ll_individual_no_delete
		AND recipient_type_code = 'I'
	 USING SQLCA;

	IF SQLCA.nf_handle_error('Embedded SQL: Update APPLIED_CLAIM_TXN','w_dup_fix_individual','wf_save') < 0 THEN
		GOTO ErrorExit
	END IF
	
	SetPointer(HourGlass!)
	
	// 3. recipient address line 1 -  when the recipient_type_code <> 'I' and the 
	//    recipient_address_line1 contains "RE"  (e.g. 'RE: John Doe'), update address line 1 
	//	   with the Individual's name --
	UPDATE UNAPPLIED_CLAIM_TXN
		SET address_line1             = "RE: " + :ls_given_name + " " + :ls_last_name	   
	  FROM UNAPPLIED_CLAIM_TXN a, CLAIM  b
	 WHERE a.recipient_type_code       <> 'I'
		AND a.address_line1             like ("RE:%")
		AND a.claim_no 		            = b.claim_no
		AND b.individual_no		         = :ll_individual_no_delete
		and a.recipient_type_code 		   in ('O', 'V')
		and a.use_default_address_flag 	= 'Y'
		and a.txn_type_code 		         in ( '1', '2', '4', '5', '8')
	 USING SQLCA;
			 
	SQLCA.nf_handle_error('Embedded SQL: Update UNAPPLIED_CLAIM_TXN','w_dup_fix_individual','wf_save') 

	// update the claim with the new individual
	UPDATE CLAIM 
		SET individual_no = :ll_individual_no_new
	 WHERE individual_no = :ll_individual_no_delete
	 USING SQLCA;

	IF SQLCA.nf_handle_error('Embedded SQL: Update CLAIM','w_dup_fix_individual','wf_save') < 0 THEN
		GOTO ErrorExit
	END IF

	UPDATE CLAIM_PARTICIPANT
		SET individual_no = :ll_individual_no_new
	 WHERE individual_no = :ll_individual_no_delete
	 USING SQLCA;

	IF SQLCA.nf_handle_error('Embedded SQL: Update CLAIM','w_dup_fix_individual','wf_save') < 0 THEN
		GOTO ErrorExit
	END IF
	
	SetPointer(HourGlass!)
	
	//	recipient address when the recipient_type_code = ‘I’ and 
	//	the recipient_no is one of the individuals being merged  and the use_default_address_flag = ‘Y’
	//	address_line1,address_line2,city,prov_state_code,country,postal_code 
	//	we need the country and not the country code grab the text from the datawindow
	
	dw_individual.SetColumn("country_code")
	 ls_country = dw_individual.gettext()
	 
	 SELECT location_desc1 
		INTO :ls_country
		FROM Location      
	  WHERE location_type_code = 'C'  
		 and active_flag        = 'Y'  
		 and location_code      <> '' 
		 and location_code      = :ls_country  ; 
	SQLCA.nf_handle_error('Embedded SQL:  SELECT location_desc1','w_dup_fix_individual','wf_save') 

	UPDATE UNAPPLIED_CLAIM_TXN 
		SET address_line1            = :ls_address_line1,
			 address_line2            = :ls_address_line2,
			 city                     = :ls_city,
			 prov_state_code          = :ls_prov_state_code,
			 country                  = :ls_country,
			 postal_code              = :ls_postal_code
	 WHERE recipient_no             = :ll_individual_no_delete
		AND recipient_type_code      = 'I'
		AND use_default_address_flag = "Y"
	 USING SQLCA;

	SQLCA.nf_handle_error('Embedded SQL: Update UNAPPLIED_CLAIM_TXN','w_dup_fix_individual','wf_save') 
	
	SetPointer(HourGlass!)

	//	check for any unapplied payment transactions that may need to be changed
	//	recipient name when the recipient_type_code = ‘I’ and the recipient_no is one of 
	//	the individuals being merged (p10229)
	//	
	//	The module must be enhanced as follows:
	//	For each Individual Merged, UPDATE the corresponding UNAPLLIED_CLAIM_TXN as follows:
	//	1. recipient name - when the recipient_type_code = 'I' and the recipient_no is 
	//		one of the individuals being merged 
	//	2. recipient address - when the recipient_type_code = 'I' and the recipient_no 
	//		is one of the individuals being merged  and the use_default_address_flag = 'Y'
	//	3. recipient address line 1-  when the recipient_type_code <> 'I' and the 
	//		recipient_address_line1 contains "RE"  (e.g. 'RE: John Doe'), update address line 1 
	//		with the Individual's name
	//
	//	** Note -  the Recipient Address contains the claimant name for Payments made 
	//		through the Payment Maintenance module that have a Provider recipient 
	//		that has no address line 2 in the recipient's address information

	// this takes care of case 1 & 2 above
	UPDATE UNAPPLIED_CLAIM_TXN 
		SET recipient_no        = :ll_individual_no_new,
			 recipient_name      = :ls_given_name + " " + :ls_last_name
	 WHERE recipient_no        = :ll_individual_no_delete
		AND recipient_type_code = 'I'
	 USING SQLCA;

	SQLCA.nf_handle_error('Embedded SQL: Update UNAPPLIED_CLAIM_TXN','w_dup_fix_individual','wf_save') 

	//	check the overpayments
	UPDATE OVERPAYMENT_DETAIL
		SET recipient_no = :ll_individual_no_new
	 WHERE recipient_no = :ll_individual_no_delete
		AND recipient_type_code = 'I'
	 USING SQLCA;

	IF SQLCA.nf_handle_error('Embedded SQL: Update OVERPAYMENT_DETAIL','w_dup_fix_individual','wf_save') < 0 THEN
		GOTO ErrorExit
	END IF
	
	SetPointer(HourGlass!)

	UPDATE OVERPAYMENT_BALANCE
		SET recipient_no = :ll_individual_no_new
	 WHERE recipient_no = :ll_individual_no_delete
		AND recipient_type_code = 'I'
	 USING SQLCA;

	IF SQLCA.nf_handle_error('Embedded SQL: Update OVERPAYMENT_BALANCE','w_dup_fix_individual','wf_save') < 0 THEN
		GOTO ErrorExit
	END IF
	
	//	check for awards
	UPDATE PERIODIC_RECIPIENT
		SET recipient_no = :ll_individual_no_new
	 WHERE recipient_no = :ll_individual_no_delete
		AND recipient_type_code = 'I'
	 USING SQLCA;

	IF SQLCA.nf_handle_error('Embedded SQL: Update PERIODIC_RECIPIENT','w_dup_fix_individual','wf_save') < 0 THEN
		GOTO ErrorExit
	END IF
	
	SetPointer(HourGlass!)
	
	/*
	If only one (1) annuity account is to be involved in the merge of individuals, 
	the individual number on that annuity account data will be changed to be the new individual identifier 
	(replaces the individual number of an archived individual). 
	
	NOTE: ANNUITY_ELIGIBILITY  accessed through ANNUITY_ACCOUNT - SEE BR 1.220 (BR_CMWB_Individual_Duplicate_Fix )
	*/
	
//	UPDATE ANNUITY_ELIGIBILITY
//		SET individual_no = :ll_individual_no_new
//	WHERE individual_no = :ll_individual_no_delete
//	USING SQLCA;
//
//	IF SQLCA.nf_handle_error('Embedded SQL: Update ANNUITY_ELIGIBILITY','w_dup_fix_individual','wf_save') < 0 THEN
//		GOTO ErrorExit
//	END IF

//replaced with 

	UPDATE ANNUITY_ACCOUNT
		SET individual_no = :ll_individual_no_new
	WHERE individual_no = :ll_individual_no_delete
	USING SQLCA;

	IF SQLCA.nf_handle_error('Embedded SQL: Update ANNUITY_ACCOUNT','w_dup_fix_individual','wf_save') < 0 THEN
		GOTO ErrorExit
	END IF

   //********************************************************
   // BEGIN new tables for P10273-2-3 - PAYOUT
	
	
	// ANNUITY_PAYOUT_PARTICIPANT
	UPDATE ANNUITY_PAYOUT_PARTICIPANT
	SET    recipient_no = :ll_individual_no_new
	WHERE  recipient_no = :ll_individual_no_delete
	AND    recipient_type_code = 'I'
	USING SQLCA;
	SQLCA.nf_handle_error('Embedded SQL: Update ANNUITY_PAYOUT_PARTICIPANT(1)','w_dup_fix_individual','wf_save')

	UPDATE ANNUITY_PAYOUT_PARTICIPANT
	SET    represented_by_recipient_no = :ll_individual_no_new
	WHERE  represented_by_recipient_no = :ll_individual_no_delete
	AND    represented_by_recipient_type_code = 'I'
	USING SQLCA;
	SQLCA.nf_handle_error('Embedded SQL: Update ANNUITY_PAYOUT_PARTICIPANT(2)','w_dup_fix_individual','wf_save')
	
	
	// ANNUITY_PAYOUT_CLAIM_DETAIL
	UPDATE ANNUITY_PAYOUT_CLAIM_DETAIL
	SET    recipient_no = :ll_individual_no_new
	WHERE  recipient_no = :ll_individual_no_delete
	AND    recipient_type_code = 'I'
	USING SQLCA;
	SQLCA.nf_handle_error('Embedded SQL: Update ANNUITY_PAYOUT_CLAIM_DETAIL(1)','w_dup_fix_individual','wf_save')

	UPDATE ANNUITY_PAYOUT_CLAIM_DETAIL
	SET    represented_by_recipient_no = :ll_individual_no_new
	WHERE  represented_by_recipient_no = :ll_individual_no_delete
	AND    represented_by_recipient_type_code = 'I'
	USING SQLCA;
	SQLCA.nf_handle_error('Embedded SQL: Update ANNUITY_PAYOUT_CLAIM_DETAIL(2)','w_dup_fix_individual','wf_save')
	
	
	// ANNUITY_PAYOUT_TXN_DETAIL
	UPDATE ANNUITY_PAYOUT_TXN_DETAIL
	SET    recipient_no = :ll_individual_no_new
	WHERE  recipient_no = :ll_individual_no_delete
	AND    recipient_type_code = 'I'
	USING SQLCA;
	SQLCA.nf_handle_error('Embedded SQL: Update ANNUITY_PAYOUT_TXN_DETAIL(1)','w_dup_fix_individual','wf_save')

	UPDATE ANNUITY_PAYOUT_TXN_DETAIL
	SET    annuity_payout_recipient_no = :ll_individual_no_new
	WHERE  annuity_payout_recipient_no = :ll_individual_no_delete
	AND    annuity_payout_recipient_type_code = 'I'
	USING SQLCA;
	SQLCA.nf_handle_error('Embedded SQL: Update ANNUITY_PAYOUT_TXN_DETAIL(2)','w_dup_fix_individual','wf_save')

	
	// ANNUITY_PAYOUT_OVERPAYMENT_XREF
	// NOTE that this update statement depends on the recipents being the same within the record
	UPDATE ANNUITY_PAYOUT_OVERPAYMENT_XREF
	SET    recipient_no             = :ll_individual_no_new,
	       overpayment_recipient_no = :ll_individual_no_new
	WHERE  recipient_no                    = :ll_individual_no_delete
	AND    overpayment_recipient_no        = :ll_individual_no_delete
	AND    recipient_type_code             = 'I'
	AND    overpayment_recipient_type_code = 'I'
	USING SQLCA;
	SQLCA.nf_handle_error('Embedded SQL: Update ANNUITY_PAYOUT_OVERPAYMENT_XREF(1)','w_dup_fix_individual','wf_save')
	
		
	// ANNUITY_PAYOUT_RECIPIENT
	UPDATE ANNUITY_PAYOUT_RECIPIENT
	SET    annuity_payout_recipient_no = :ll_individual_no_new
	WHERE  annuity_payout_recipient_no = :ll_individual_no_delete
	AND    annuity_payout_recipient_type_code = 'I'
	USING SQLCA;
	SQLCA.nf_handle_error('Embedded SQL: Update ANNUITY_PAYOUT_RECIPIENT','w_dup_fix_individual','wf_save')
	

	// END new tables for P10273-2-3 - PAYOUT
   //********************************************************
   	
	UPDATE INVOICE
		SET recipient_no = :ll_individual_no_new
	WHERE recipient_no = :ll_individual_no_delete
		AND recipient_type_code = 'I'
	USING SQLCA;

	IF SQLCA.nf_handle_error('Embedded SQL: Update INVOICE','w_dup_fix_individual','wf_save') < 0 THEN
		GOTO ErrorExit
	END IF
	
	SetPointer(HourGlass!)
		
//  Changes originaly made by Doug Filmore on Nov 16, 1999  re:  CMS consideration
//  Reaplied by Ed Lenarczyk  Jul 18, 2000  -  PR # 1085   done prev on Feb 22, 2000
	UPDATE PATIENT_CONTACTS
	SET individual_no = :ll_individual_no_new
	WHERE individual_no = :ll_individual_no_delete
	USING SQLCA;
	IF SQLCA.nf_handle_error('Embedded SQL: Update PATIENT_CONTACTS','w_dup_fix_individual','wf_save') < 0 THEN
		GOTO ErrorExit
	END IF
		
	UPDATE PATIENT_DETAILS
	SET individual_no = :ll_individual_no_new
	WHERE individual_no = :ll_individual_no_delete
	USING SQLCA;
	IF SQLCA.nf_handle_error('Embedded SQL: Update PATIENT_DETAILS','w_dup_fix_individual','wf_save') < 0 THEN
		GOTO ErrorExit
	END IF
	
	SetPointer(HourGlass!)
		
	UPDATE INJURY
	SET individual_no = :ll_individual_no_new
	WHERE individual_no = :ll_individual_no_delete
	USING SQLCA;
	IF SQLCA.nf_handle_error('Embedded SQL: Update INJURY','w_dup_fix_individual','wf_save') < 0 THEN
		GOTO ErrorExit
	END IF
		
	UPDATE REFERRAL
	SET individual_no = :ll_individual_no_new
	WHERE individual_no = :ll_individual_no_delete
	USING SQLCA;
	IF SQLCA.nf_handle_error('Embedded SQL: Update REFERRAL','w_dup_fix_individual','wf_save') < 0 THEN
		GOTO ErrorExit
	END IF
	
	SetPointer(HourGlass!)
		
	UPDATE TREATMENT_RECORD
	SET individual_no = :ll_individual_no_new
	WHERE individual_no = :ll_individual_no_delete
	USING SQLCA;
	IF SQLCA.nf_handle_error('Embedded SQL: Update TREATMENT_RECORD','w_dup_fix_individual','wf_save') < 0 THEN
		GOTO ErrorExit
	END IF
		
	UPDATE DISCHARGE_DETAILS
	SET individual_no = :ll_individual_no_new
	WHERE individual_no = :ll_individual_no_delete
	USING SQLCA;
	IF SQLCA.nf_handle_error('Embedded SQL: Update DISCHARGE_DETAILS','w_dup_fix_individual','wf_save') < 0 THEN
		GOTO ErrorExit
	END IF
	
	SetPointer(HourGlass!)
		
	UPDATE TREATMENT_FOLLOW_UP
	SET individual_no = :ll_individual_no_new
	WHERE individual_no = :ll_individual_no_delete
	USING SQLCA;
	IF SQLCA.nf_handle_error('Embedded SQL: Update TREATMENT_FOLLOW_UP','w_dup_fix_individual','wf_save') < 0 THEN
		GOTO ErrorExit
	END IF
	
	// Added by M.A. for Wrc_Invoicing project
	UPDATE WRC_TXN
	SET individual_no = :ll_individual_no_new
	WHERE individual_no = :ll_individual_no_delete
	USING SQLCA;
	IF SQLCA.nf_handle_error('Embedded SQL: Update WRC_TXN','w_dup_fix_individual','wf_save') < 0 THEN
		GOTO ErrorExit
	END IF
	
	SetPointer(HourGlass!)
	
	UPDATE WRC_INVOICE_HEADER
	SET individual_no = :ll_individual_no_new
	WHERE individual_no = :ll_individual_no_delete
	USING SQLCA;
	IF SQLCA.nf_handle_error('Embedded SQL: Update WRC_INVOICE_HEADER','w_dup_fix_individual','wf_save') < 0 THEN
		GOTO ErrorExit
	END IF
	
	UPDATE MATERIAL_PACKAGE
	SET individual_no = :ll_individual_no_new
	WHERE individual_no = :ll_individual_no_delete
	USING SQLCA;
	IF SQLCA.nf_handle_error('Embedded SQL: Update MATERIAL_PACKAGE','w_dup_fix_individual','wf_save') < 0 THEN
		GOTO ErrorExit
	END IF
	
	SetPointer(HourGlass!)	
		
	UPDATE PRINT_ITEM
	SET recipient_no = :ll_individual_no_new
	WHERE recipient_no = :ll_individual_no_delete
	AND recipient_type_code = 'I'
	USING SQLCA;
	IF SQLCA.nf_handle_error('Embedded SQL: Update PRINT_ITEM','w_dup_fix_individual','wf_save') < 0 THEN
		GOTO ErrorExit
	END IF

	UPDATE	INDIVIDUAL_EVENT
	SET		individual_no	= :ll_individual_no_new
	WHERE	individual_no	= :ll_individual_no_delete
	USING SQLCA;
	IF SQLCA.nf_handle_error('Embedded SQL: Update INDIVIDUAL_EVENT','w_dup_fix_individual','wf_save') < 0 THEN
		GOTO ErrorExit
	END IF
	
	// Update the Drug Alerts with new individual number
	UPDATE DRUG_ALERT
	SET          individual_no =  :ll_individual_no_new
	WHERE   individual_no = :ll_individual_no_delete
	USING SQLCA;
	
	IF SQLCA.nf_handle_error('Embedded SQL: Update DRUG_ALERT','w_dup_fix_individual','wf_save') < 0 THEN
		GOTO ErrorExit
	END IF
	
	//P10277 - Added for Income Verification Project.
	UPDATE IV_REQUEST
	SET       individual_no = :ll_individual_no_new
	WHERE  individual_no = :ll_individual_no_delete
	USING    SQLCA;
	
	IF SQLCA.nf_handle_error('Embedded SQL: UPDATE IV_REQUEST','w_dup_fix_individual','wf_save') < 0 THEN
		GOTO ErrorExit
	END IF
	
	//P10151-291 - Added for Single DD for Individuals
	UPDATE DIRECT_DEPOSIT_DETAIL
	SET       recipient_no = :ll_individual_no_new
	WHERE  recipient_no = :ll_individual_no_delete
	AND       recipient_type_code = 'I'
	USING    SQLCA;
	
	IF SQLCA.nf_handle_error('Embedded SQL: DIRECT_DEPOSIT_DETAIL','w_dup_fix_individual','wf_save') < 0 THEN
		GOTO ErrorExit
	END IF
	
	SetPointer(HourGlass!)

	// hold individual_no's to be inserted into archive tables by wf_archive_t5_individuals()
	il_individuals_to_archive[ll_loop] = dw_individual_list.Object.individual_no[ll_loop]

	// save the old data in archive
	INSERT INTO ARCHIVE_INDIVIDUAL
	( new_individual_no, individual_no, sin_no,             medicare_no,     birth_date,   
     death_date,        sex,           language_code,      last_name,       given_names,
	  address_line1,     address_line2, city,               prov_state_code, country_code,
	  postal_code,       location_code, location_type_code, telephone_no,    cellphone_no,
	  pager_no,          bank_no,       bank_transit_no,    bank_account_no,          
	  caution_flag,      history_flag,  court_order_flag )
	SELECT 
	  :ll_individual_no_new, individual_no, sin_no,             medicare_no,     birth_date,   
     death_date,            sex,           language_code,      last_name,       given_names,
	  address_line1,         address_line2, city,               prov_state_code, country_code,
	  postal_code,           location_code, location_type_code, telephone_no,    cellphone_no,
	  pager_no,              bank_no,       bank_transit_no,    bank_account_no,         
	  caution_flag,          history_flag,	 court_order_flag
   FROM INDIVIDUAL  
	WHERE individual_no = :ll_individual_no_delete
	USING SQLCA;

	IF SQLCA.nf_handle_error('Embedded SQL: Insert into INDIVIDUAL ARCHIVE','w_dup_fix_individual','wf_save') < 0 THEN
		GOTO ErrorExit
	END IF

	//	delete the old individual
	DELETE 
	  FROM INDIVIDUAL
	 WHERE individual_no = :ll_individual_no_delete
	 USING SQLCA;

	IF SQLCA.nf_handle_error('Embedded SQL: Delete from INDIVIDUAL','w_dup_fix_individual','wf_save') < 0 THEN
		GOTO ErrorExit
	END IF

	DELETE 
	  FROM INDIVIDUAL_NAME
	 WHERE individual_no = :ll_individual_no_delete
	 USING SQLCA;

	IF SQLCA.nf_handle_error('Embedded SQL: Delete from INDIVIDUAL_NAME','w_dup_fix_individual','wf_save') < 0 THEN
		GOTO ErrorExit
	END IF
	
	IF lb_wif_entity_to_delete THEN
		lds_delete_wif_entity.Retrieve(ll_individual_no_delete,'I','cancelled for individual merge')
		SQLCA.nf_handle_error('w_dup_fix_individual','lds_delete_wif_entity.Retrieve','wf_save')		
	END IF
	
	
	SetPointer(HourGlass!)

	ll_loop = ll_loop + 1
LOOP

IF wf_log_court_order_event() < 0 THEN
	MessageBox('Error','Error logging event. Please call HelpDesk.',information!)
	GOTO ErrorExit
END IF

dw_individual.Object.individual_court_order_flag.Protect = 0

lstr_old_individuals.ll_old_individual = il_individuals_to_archive
IF wf_t5007_update(lstr_old_individuals, ll_individual_no_new) < 0 THEN RETURN -1

/* P10236 - e-Pay, new business rule BR 4.10 in BR_CMWB_Individual_Duplication_Fix_v6.doc (2005-10-03) 
*/	 
IF inv_individual.nf_update_eligibility_coverage(ll_individual_no_new) < 0 THEN
	GOTO ErrorExit
END IF
/* end P10236 - new business rule BR 4.10
*/

/* P10237 Annuities - This Function DOES UPDATES!!!  Keep it after the rule checking.
    could be multiple annuity accounts for individual - check function for description and directions*/
IF wf_update_save_annuity_eligibility( ll_individual_no_new, DATE(ldtm_account_birth), DATE(ldtm_account_death)) = -1 THEN 
	GOTO ErrorExit
END IF

// T024503/WO003155 - Injured Worker Portal
IF lb_wif_entity_to_delete THEN		
	// set the no minutes for workflow token to expire
	ll_no_minutes_to_expire = Long(ProfileString(vgs_ini_filename, "workflow token", "no_minutes_to_expire_invite_iw", '0'))
	
	// this stored procedure inserts into 'WIF' & 'WORKFLOW' tables
	li_rtn = lds_create_token.retrieve(ll_individual_no_new, '', '', ll_no_minutes_to_expire, 'I', 4)   // two blanks are 'wif_entity_subtype_code' (no subtype for individual) and 'registration_code' which is not required, 'I' is for Individual, as a entity type code, the '4' is harcoded for the secret question identifier (dynamic question)
	SQLCA.nf_handle_error('w_dup_fix_individual','lds_create_token.retrieve','wf_save')
	
	ls_token_id = lds_create_token.GetItemString(1,'GUID_RETURN')

	lds_workflow_token_related_mail_package = Create U_DS
	lds_workflow_token_related_mail_package.DataObject =  "ds_workflow_token_related_mail_package"
	lds_workflow_token_related_mail_package.SetTransObject(SQLCA)
	
	li_inserted_row = lds_workflow_token_related_mail_package.InsertRow(0)
	
	lds_workflow_token_related_mail_package.SetItem(li_inserted_row,'token_id',         ls_token_id)
	lds_workflow_token_related_mail_package.SetItem(li_inserted_row,'package_no',       0)
	lds_workflow_token_related_mail_package.SetItem(li_inserted_row,'package_type_code',ls_package_type_code)
	
	li_rtn = lds_workflow_token_related_mail_package.Update()
	SQLCA.nf_Handle_error('w_dup_fix_individual','lds_workflow_token_related_mail_package','wf_save')
END IF


SQLCA.nf_commit_transaction()

st_total.Text = '0'
		
dw_individual_list.SetFilter('')
dw_individual_list.Filter()	

Return 0		

ErrorExit: 
	dw_individual_list.SetFilter('')
	dw_individual_list.Filter()	
	Return -1	
end function

public function integer wf_compare_individuals (datastore ads_indiv_list, boolean ab_save);//PR19887 2014-11-14 David Worboys cleaned up variables.  I think this function supercedes wf_compare used in cb_build_individual
DATETIME ldtm_birth_date
DATETIME ldtm_bday_compare
DATETIME ldtm_death_date
DATETIME ldtm_death_compare
LONG        ll_med
LONG        ll_med_compare
LONG        ll_sin
LONG        ll_sin_compare
LONG        ll_start
LONG        ll_start2
LONG        ll_cntr
LONG        ll_rows
LONG        ll_cntr2
LONG        ll_last
LONG        ll_first
LONG        ll_court_order
LONG        ll_sin_cntr
LONG        ll_med_cntr
LONG        ll_death
STRING     ls_given
STRING     ls_given_compare
STRING     ls_last
STRING     ls_last_compare
STRING     ls_msg
STRING     ls_gender
STRING     ls_gender_compare
STRING     ls_court_order
STRING     ls_court_order_comp

ll_rows 						 = ads_indiv_list.RowCount()

ll_start = 1

IF ab_save THEN
	dw_individual.Accepttext()
	dw_main_name.Accepttext()
	
	ldtm_birth_date 	= dw_individual.GetItemDateTime(1, 'birth_date')
	ldtm_death_date 	= dw_individual.GetItemDateTime(1, 'death_date')
	ls_gender       		= dw_individual.GetItemString(1, 'sex')
	ll_sin          			= dw_individual.GetItemNumber(1, 'sin_no')
	ll_med          		= dw_individual.GetItemNumber(1, 'medicare_no')
	ls_last         		= dw_main_name.GetItemString(1, 'last_name')
	ls_given        		= dw_main_name.GetItemString(1, 'given_names')
	ls_court_order  	= dw_individual.GetItemString(1, 'individual_court_order_flag')
	
	ll_start  = ll_rows
//	ll_start2 = 0	 //2014-11-17 David Worboys - Deleted
	
	IF il_sin > 0 AND (ISNULL(ll_sin) OR ll_sin = 0) THEN
		MessageBox('Invalid SIN','The default individual has a SIN. Please enter before continuing.',information!)
		RETURN -1
	END IF
	
	IF il_medicare > 0 AND (ISNULL(ll_med) OR ll_med = 0) THEN
		MessageBox('Invalid Medicare','The default individual has a Medicare Number. Please enter before continuing.',information!)
		RETURN -1
	END IF
END IF

FOR ll_cntr = ll_start to ll_rows
	
	IF ab_save = FALSE THEN
		ldtm_birth_date 		= ads_indiv_list.GetItemDateTime(ll_cntr, 'birth_date')
		ldtm_death_date 		= ads_indiv_list.GetItemDateTime(1, 'death_date')
		ls_gender       			= ads_indiv_list.GetItemString(ll_cntr, 'gender')
		ll_sin          				= ads_indiv_list.GetItemNumber(ll_cntr, 'sin_no')
		ll_med          			= ads_indiv_list.GetItemNumber(ll_cntr, 'medicare')
		ls_last         			= ads_indiv_list.GetItemString(ll_cntr, 'last_name')
		ls_given        			= ads_indiv_list.GetItemString(ll_cntr, 'given_name')
		ls_court_order  		= ads_indiv_list.GetItemString(ll_cntr, 'court_order')
				
		ll_start = ll_cntr
	END IF
	
//	FOR ll_cntr2 = ll_start2 + 1 to ll_rows //2014-11-17 David Worboys - Removed as ll_start2 was always set to 0
	FOR ll_cntr2 = 1 to ll_rows 					
		ldtm_bday_compare   			= ads_indiv_list.GetItemDateTime(ll_cntr2, 'birth_date')
		ldtm_death_compare  			= ads_indiv_list.GetItemDateTime(ll_cntr2, 'death_date')
		ls_gender_compare   				= ads_indiv_list.GetItemString(ll_cntr2, 'gender')
		ll_sin_compare      				= ads_indiv_list.GetItemNumber(ll_cntr2, 'sin_no')
		ll_med_compare      				= ads_indiv_list.GetItemNumber(ll_cntr2, 'medicare')
		ls_last_compare     				= ads_indiv_list.GetItemString(ll_cntr2, 'last_name')
		ls_given_compare    				= ads_indiv_list.GetItemString(ll_cntr2, 'given_name')
		ls_court_order_comp 				= ads_indiv_list.GetItemString(ll_cntr2, 'court_order')
				
		//Birthdate
		IF NOT ISNULL(ldtm_birth_date) AND NOT ISNULL(ldtm_bday_compare) THEN
			IF ldtm_birth_date <> ldtm_bday_compare THEN
				MessageBox('Cannot Merge','These individuals cannot be merged. The birthdates must be the same.',information!)
				RETURN -1
			END IF
		END IF
			
	//IF NOT ISNULL(ldtm_bday_compare) THEN
			
		IF ISNULL(ldtm_birth_date) AND NOT ISNULL(ldtm_bday_compare) AND ab_save THEN
			MessageBox('No Birthdate','Atleast one of the individuals being merged has a birthdate entered.' &
			           + '~r~nYou must enter a Birthdate before merging individuals.',information!)
			RETURN -1
		END IF
	
		//Gender
		IF (ls_gender = '' OR ls_gender = 'U')  AND ab_save THEN
			MessageBox('Invalid Gender','You must select Male or Female as the Sex before merging individuals.',information!)
			RETURN -1
		ELSEIF ls_gender <> 'U' AND ls_gender_compare <> 'U' THEN
			IF ls_gender <> ls_gender_compare THEN
				MessageBox('Cannot Merge','These individuals cannot be merged. The individuals have different genders.',information!)
				RETURN -1
			END IF
		END IF
		
		//SIN
		IF NOT ISNULL(ll_sin) AND NOT ISNULL(ll_sin_compare) THEN
			IF ll_sin <> 0 AND ll_sin_compare <> 0 THEN
				IF ll_sin <> ll_sin_compare THEN
					MessageBox('Cannot Merge','These individuals cannot be merged. The individuals have different Social Insurance Numbers.',information!)
					RETURN -1
				END IF
			END IF
		END IF
		
		IF (NOT ll_sin > 0) AND (ll_sin_compare > 0) AND ab_save THEN
			ll_sin_cntr = ll_sin_cntr + 1
		END IF
		
		//Medicare
		IF NOT ISNULL(ll_med) AND NOT ISNULL(ll_med_compare) THEN
			IF ll_med <> 0 AND ll_med_compare <> 0 THEN
				IF ll_med <> ll_med_compare THEN
					MessageBox('Cannot Merge','These individuals cannot be merged. The individuals have different Medicare Numbers.',information!)
					RETURN -1
				END IF
			END IF
		END IF
	
		IF (NOT ll_med > 0) AND (ll_med_compare > 0) AND ab_save THEN
			ll_med_cntr = ll_med_cntr + 1
		END IF
		
		//Last Name
		IF NOT ISNULL(ls_last) AND NOT ISNULL(ls_last_compare) THEN
			IF TRIM(ls_last) <> '' AND TRIM(ls_last_compare) <> '' THEN
				IF TRIM(ls_last) <> TRIM(ls_last_compare) THEN
					ll_last = ll_last + 1
				END IF
			END IF
		END IF
		
		//First Name
		IF NOT ISNULL(ls_given) AND NOT ISNULL(ls_given_compare) THEN
			IF TRIM(ls_given) <> '' AND TRIM(ls_given_compare) <> '' THEN
				IF TRIM(ls_given) <> TRIM(ls_given_compare) THEN
					ll_first = ll_first + 1
				END IF
			END IF
		END IF
		
		//Deathdate
		// one could be null while the other Not - Death Date allows Nulls
		//IF NOT ISNULL(ldtm_death_date) AND NOT ISNULL(ldtm_death_compare) THEN
			IF ldtm_death_date <> ldtm_death_compare THEN
				ll_death = ll_death + 1
			END IF
		//END IF

	NEXT
NEXT

IF ll_sin_cntr > 0 THEN
	IF MessageBox('Warning!','One of the individuals being merged has a Social Insurance Number. Would you like to continue with the merge?',question!,yesno!) = 2 THEN
		RETURN -1
	END IF
END IF

IF ll_med_cntr > 0 THEN
	IF MessageBox('Warning!','One of the individuals being merged has a Medicare Number. Would you like to continue with the merge?',question!,yesno!) = 2 THEN
		RETURN -1
	END IF
END IF

IF ll_last > 0 THEN 
	IF MessageBox('Warning!','These individuals have different Last Names. Would you like to continue with the merge?',question!,yesno!) = 2 THEN
		RETURN -1
	END IF
END IF

IF ll_first > 0 THEN
	IF MessageBox('Warning!','These individuals have different First Names. Would you like to continue with the merge?',question!,yesno!) = 2 THEN
		RETURN -1
	END IF
END IF

IF ll_death > 0 THEN
//	IF MessageBox('Warning!','These individuals have different Death Dates. Would you like to continue with the merge?',question!,yesno!) = 2 THEN
//		RETURN -1
//	END IF
	MessageBox('Cannot Merge','These individuals cannot be merged. The death dates must be the same.'+&
	                 '~rPlease modify Individuals using the Individual Module before proceeding',information!)
	RETURN -1
END IF
	
	
RETURN 0
end function

public function long wf_get_default_individual ();/*
**		Type  		: Function
**		Name 		: wf_get_default_individual
**		Arguments 	:
**		Returns 		:
**		Purpose		: Returns the default individual number
**		Date			: 2014/11/17
**		Author		: David Worboys
**	
**		Modifications
*/
LONG ll_Row					  =  0
LONG ll_Default_Individual_No = -1

//Only 1 default individual can be selected at once
ll_Row =  dw_individual_list.Find ( "default_individual = 'Y'", 1, dw_individual_list.ROWCOUNT() + 1 )

IF (ll_Row > 0) THEN
	ll_Default_Individual_No = dw_individual_list.GetItemNumber(ll_Row,'individual_no')
END IF

RETURN ll_Default_Individual_No
end function

public function long wf_update_bank_events (datastore ads_indiv_list);/* 
** PR19887
** 
IF the ID that is identified as the ‘default’ ID does not have bank information AND 
    at least one of the other ID’s has bank information THEN	 
	 
	Warn the user that continuing will remove bank information and ask if they wish to continue. 
	
	IF they wish to proceed, THEN 
		create an INDIVIDUAL_EVENT bank info removed event. 
	ENDIF
ENDIF		
	
IF all ID’s have bank information THEN
    		IF the bank information fields are identical in all ID’s THEN 
    			A new individual bank info event is not required. 
		ELSE		 
			create an INDIVIDUAL_EVENT bank info changed event
		 ENDIF
ENDIF

IF none of the ID’s being merged have bank information THEN
	no new INDIVIDUAL_EVENT for bank information is tcreated. 
ENDIF

All existing INDIVIDUAL_EVENT bank info events are to have the individual ID changed to the new individual ID (Note: The new bank info saved will be the bank info in the default ID.). 
	 
*/

/*
**		Type  		: Windows Fundtion
**		Name 		: wf_update_bank_events
**		Arguments 	: datastore : ads_indiv_list
**		Returns 		:
**		Purpose		: Merges Individual BanK Events
**		Date			: 2014/11/19
**		Author		: David Worboys
**	
**		Modifications
*/

/*
** NOTE: SQLCA.nf_begin_transaction() is in this fuction because we have a messagebox and under explict transaction handling we want to do that before the transaction
**           commentces.  This also affect the arcitecture of this function as ads_indiv_list is looped through twice - once to gather bank information and later to perform
**           a SQL update pass that must be within the transaction statement.  Normally both could be done in the same pass.  wf_save closes the transaction.
*/
LONG 		ll_Default_Individual_No				= 0
LONG 		ll_Default_Individual_Row			= 0
LONG 		ll_Max 									= 0
LONG 		ll_Bank_Info_Identical_Count		= 0
LONG 		ll_Bank_Info_Count					= 0
LONG		ll_Event_No								= 0
LONG 		ll_Individual_No							= 0
LONG 		ll_Row 									= 0
LONG		ll_Status									= 1
STRING   ls_Default_Bank_No					= ""
STRING   ls_Bank_No_Compare				= ""
STRING   ls_Default_Bank_Transit_No			= ""
STRING   ls_Bank_Transit_No_Compare		= ""
STRING   ls_Default_Bank_Account_No		= ""
STRING   ls_Bank_Account_No_Compare		= ""
STRING	ls_Merged_Individuals				= ""					
n_Event_Log lnv_Event_Log

lnv_Event_Log = CREATE n_Event_Log

ll_Default_Individual_No 	= wf_Get_Default_Individual()
ll_Max 						= ads_Indiv_List.ROWCOUNT()

IF (ll_Default_Individual_No > 0) THEN //All is good, have a default individual like we should

	ll_Default_Individual_Row =  ads_Indiv_List.Find ( "individual_no = "+STRING(ll_Default_Individual_No), 1, ll_Max + 1 )

	IF (ll_Default_Individual_Row > 0) THEN //All is good,  default individual is in the datastore
	
		ls_Default_Bank_No				= TRIM(ads_indiv_list.OBJECT.Bank_No[ll_Default_Individual_Row])
		ls_Default_Bank_Transit_No	 	= TRIM(ads_indiv_list.OBJECT.Bank_Transit_No[ll_Default_Individual_Row])
		ls_Default_Bank_Account_No 	= TRIM(ads_indiv_list.OBJECT.Bank_Account_No[ll_Default_Individual_Row])

		//Now process all the selected individuals to determine banking status - Note: can not do updates here as messagebox comes further down.
		FOR ll_Row = 1 TO ll_Max
			IF (ll_Row = ll_Default_Individual_Row) THEN //We can ignore as we are only checking the individuals to be merged.
				CONTINUE
			END IF
			
			ll_Individual_No = ads_indiv_list.OBJECT.Individual_No[ll_Row]
			
			ls_Merged_Individuals += STRING(ll_Individual_No)
			
			IF (ll_Row < ll_Max) THEN
				ls_Merged_Individuals += ","
			END IF
			
			ls_Bank_No_Compare				= TRIM(ads_indiv_list.OBJECT.Bank_No[ll_Row])
			ls_Bank_Transit_No_Compare	= TRIM(ads_indiv_list.OBJECT.Bank_Transit_No[ll_Row])
			ls_Bank_Account_No_Compare	= TRIM(ads_indiv_list.OBJECT.Bank_Account_No[ll_Row])
			
			//If the merged individual has bank info need to keep track of this
			IF (ls_Bank_No_Compare <> "" AND ls_Bank_Transit_No_Compare <> "" AND ls_Bank_Account_No_Compare <> "" ) THEN							
				
				IF (ls_Default_Bank_No = ls_Bank_No_Compare AND ls_Default_Bank_Transit_No = ls_Bank_Transit_No_Compare AND &
					ls_Default_Bank_Account_No = ls_Bank_Account_No_Compare) THEN //Then Identical Bank Info to Default Individual
					
					ll_Bank_Info_Identical_Count ++
				ELSE  //Not The Same Bank Info as the Default Individual
					
					ll_Bank_Info_Count++
				END IF			
				
			END IF
		NEXT

		/*
		** When the ID that is identified as the ‘default’ ID does not have bank information and at least one of the other ID’s has bank information then 
		** warn the user that continuing will remove bank information and ask if they wish to continue.
		*/
		IF (ls_Default_Bank_No = ""  AND ls_Default_Bank_Transit_No = ""  AND &
			 ls_Default_Bank_Account_No = "" ) THEN //If Default Individual Has NO Bank Info
			
			IF (ll_Bank_Info_Count > 0) THEN //And At least one merge individual has Bank Info   	
			
				IF (MESSAGEBOX("Warning", "Continuing This Operation Will Result In The Loss of  Bank Information.  Proceed?", &
										 QUESTION!,YESNO!,2) = 1) THEN //And the User Has Chosen To Continue
										 
					SQLCA.nf_begin_transaction()	//Note: Located here because of explicit transaction handling	 									 
										 
				 	//create an INDIVIDUAL_EVENT bank info removed event. 
			 		ll_Event_No = lnv_event_log.nf_next_individual_event_no(ll_Default_Individual_No)
					 
				 	IF (lnv_event_log.nf_create_auto_individual_event( ll_Default_Individual_No, ll_Event_No, "068","Bank Information Removed - "+ls_Merged_Individuals+ &
					 	" Merged With "+STRING(ll_Default_Individual_No), "RV") = 0) THEN
						ll_Status = 1
					ELSE
						ll_Status = -1
					END IF
				ELSE //User has chosen to bail out
					ll_Status = -1
				END IF	
			ELSE //No Bank info found or the merged and default individual bank ino is the same	 
				ll_Status = 1
				SQLCA.nf_begin_transaction()	//Note: Located here because of explicit transaction handling	 									 
			END IF	 
		ELSEIF (TRIM(ls_Default_Bank_No) <> ""  AND  TRIM(ls_Default_Bank_Transit_No) <> "" AND &
			 	   TRIM(ls_Default_Bank_Account_No) <> "" ) THEN //If Default Individual Has Bank Info
					 
			SQLCA.nf_begin_transaction()	//Note: Located here because of explicit transaction handling	 
			
			/*
			** If all ID’s have bank information and the bank information fields are identical in all ID’s, then a new individual bank info event is not required. 
			** If all ID’s being merged have bank information and the bank information is not the same in all ID’s then create an INDIVIDUAL_EVENT bank info changed event. 
			*/					 
			IF (ll_Bank_Info_Identical_Count = ll_Max - 1) THEN //And all individuals have idenitical bank info (ll_Max - 1 because we are ignoring default individual)
				//A new individual bank info event is not required. 
			ELSE
				//create an INDIVIDUAL_EVENT bank info changed event using the default user id
				ll_Event_No = lnv_event_log.nf_next_individual_event_no(ll_Default_Individual_No)

				IF (lnv_event_log.nf_create_auto_individual_event( ll_Default_Individual_No, ll_Event_No, "068","Bank Information Changed - " + ls_Merged_Individuals + &
					 " Merged With "+STRING(ll_Default_Individual_No), "CG") = 0) THEN
					ll_Status = 1
				ELSE
					ll_Status = -1
				END IF
			END IF
		ELSE //Just in case catch
			ll_Status = 1
			SQLCA.nf_begin_transaction()	//Note: Located here because of explicit transaction handling	 
		END IF

		IF (ll_Status = 1) THEN //Good, all existing INDIVIDUAL_EVENT bank info events can now have the individual ID changed to the default individual ID									
		
			//Now process all the selected individuals updating INDIVIDUAL_EVANTS - Note: can not do updates in first pass as messagebox would block the transaction.
			FOR ll_Row = 1 TO ll_Max
				IF (ll_Row = ll_Default_Individual_Row) THEN //We can ignore as we are only checking the individuals to be merged.
					CONTINUE
				END IF
				
				ll_Individual_No = ads_indiv_list.OBJECT.Individual_No[ll_Row]

				UPDATE	INDIVIDUAL_EVENT
				SET		individual_no	= :ll_Default_Individual_No, event_type_code = '068'
				WHERE	individual_no	=:ll_Individual_No AND (event_type_code = '024' OR event_type_code = '068')
				USING SQLCA;
				
				IF SQLCA.nf_handle_error('Embedded SQL: Update INDIVIDUAL_EVENT','w_dup_fix_individual','wf_update_bank_events') = 0 THEN
					ll_Status = 1
				ELSE
					ll_Status = -1
					EXIT
				END IF	
			NEXT
		END IF 
	ELSE //Must have a default individual row
		ll_Status = -1
	END IF 
ELSE //Must have a default individual number
	ll_Status = -1
END IF

RETURN ll_Status

end function

public function integer wf_insert_entity (long al_individual_no);

// insert replacement WIF/WORKFLOW records
INTEGER   li_rtn
LONG      ll_no_minutes_to_expire_invite_iw
U_DS      lds_create_token


lds_create_token = CREATE u_ds
lds_create_token.DataObject =  "d_create_token"
lds_create_token.SetTransObject(SQLCA)


ll_no_minutes_to_expire_invite_iw = ProfileInt(vgs_ini_filename, "workflow token", "no_minutes_to_expire_invite_iw", 0)


SQLCA.nf_begin_transaction()

// this stored procedure inserts into 'WIF' & 'WORKFLOW' tables
li_rtn = lds_create_token.retrieve(al_individual_no, '', '', ll_no_minutes_to_expire_invite_iw, 'I', 4)   // two blanks are 'wif_entity_subtype_code' (no subtype for individual) and 'registration_code' which is not required, 'I' is for Individual, as a entity type code, the '4' is harcoded for the secret question identifier (dynamic question)
SQLCA.nf_handle_error('u_tabpg_registration','lds_create_token.retrieve','wf_process_injured_worker_package')

SQLCA.nf_commit_transaction()


destroy lds_create_token

RETURN 0
end function

public function boolean wf_check_wif_entity (long al_individual_no[], ref string as_package_type_code);// wf_check_wif_entity

BOOLEAN     lb_wif_entity_to_delete
INTEGER     li_rowcount, li_Find
LONG        ll_wif_group_id
STRING      ls_find
U_DS        lds_entity_individual


lds_entity_individual = Create U_DS
lds_entity_individual.DataObject = 'ds_entity_individual'
lds_entity_individual.SetTransObject(SQLCA)

li_rowcount = lds_entity_individual.Retrieve(al_individual_no)
SQLCA.nf_handle_error('w_dup_fix_individual','lds_entity_individual.retrieve','wf_check_wif_entity')


IF li_rowcount > 0 THEN
   ls_find = 'wif_group_id <> 0'
	li_find = lds_entity_individual.Find(ls_find,1,li_rowcount)
	IF li_find > 0 THEN
		as_package_type_code = 'MI'
	ELSE
		as_package_type_code = 'IR'
	END IF
	
	lb_wif_entity_to_delete = TRUE
ELSE
	lb_wif_entity_to_delete = FALSE
END IF

RETURN lb_wif_entity_to_delete
end function

event open;call super::open;U_DWA	ldw_dw[]

	dw_search_criteria.SetTransObject(SQLCA)
	dw_individual_list.SetTransObject(SQLCA)
	dw_individual.SetTransObject(SQLCA)
	
	dw_search_criteria.InsertRow(0)
	dw_individual_list.uf_SetSelect(1)

/*  Set up the basic SQL Syntax that will be modified for the requested search.    
*/
	is_SQLSyntax = dw_individual_list.Describe("DataWindow.Table.Select")
	is_SQLSyntax = left(is_SQLSyntax,pos(is_SQLSyntax,"WHERE",1)-1)
	is_WhereClause = "WHERE (INDIVIDUAL.individual_no = INDIVIDUAL_NAME.individual_no)  AND  ( INDIVIDUAL_NAME.name_type_code = 'M' ) " 


	inv_individual = Create n_individual

/*	initialize the object
*/
	ldw_dw[1] = dw_individual
	ldw_dw[2] = dw_main_name
	ldw_dw[3] = dw_individual_name
	ldw_dw[4] = dw_next_individual_no
	ldw_dw[5] = dw_claim_list

	// prevent modification of death date during merge
	dw_individual.Object.death_date.Protect = '1'
	dw_individual.Object.death_date.Background.Color = '553648127'

	inv_individual.nf_init(ldw_dw[],SQLCA,THIS)

	dw_sin.SetTransObject(SQLCA)
	dw_medicare.SetTransObject(SQLCA)
	dw_search_criteria.SetFocus()
	

end event

on w_dup_fix_individual.create
int iCurrent
call super::create
if IsValid(this.MenuID) then destroy(this.MenuID)
if this.MenuName = "m_cmwb_notools" then this.MenuID = create m_cmwb_notools
this.cb_open_event_log=create cb_open_event_log
this.st_2=create st_2
this.st_total=create st_total
this.cb_search=create cb_search
this.dw_main_name=create dw_main_name
this.dw_medicare=create dw_medicare
this.dw_sin=create dw_sin
this.dw_individual_list=create dw_individual_list
this.cb_save=create cb_save
this.cb_build_individual=create cb_build_individual
this.cb_cancel=create cb_cancel
this.dw_search_criteria=create dw_search_criteria
this.dw_claim_list=create dw_claim_list
this.dw_next_individual_no=create dw_next_individual_no
this.dw_individual_name=create dw_individual_name
this.dw_individual=create dw_individual
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_open_event_log
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.st_total
this.Control[iCurrent+4]=this.cb_search
this.Control[iCurrent+5]=this.dw_main_name
this.Control[iCurrent+6]=this.dw_medicare
this.Control[iCurrent+7]=this.dw_sin
this.Control[iCurrent+8]=this.dw_individual_list
this.Control[iCurrent+9]=this.cb_save
this.Control[iCurrent+10]=this.cb_build_individual
this.Control[iCurrent+11]=this.cb_cancel
this.Control[iCurrent+12]=this.dw_search_criteria
this.Control[iCurrent+13]=this.dw_claim_list
this.Control[iCurrent+14]=this.dw_next_individual_no
this.Control[iCurrent+15]=this.dw_individual_name
this.Control[iCurrent+16]=this.dw_individual
end on

on w_dup_fix_individual.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_open_event_log)
destroy(this.st_2)
destroy(this.st_total)
destroy(this.cb_search)
destroy(this.dw_main_name)
destroy(this.dw_medicare)
destroy(this.dw_sin)
destroy(this.dw_individual_list)
destroy(this.cb_save)
destroy(this.cb_build_individual)
destroy(this.cb_cancel)
destroy(this.dw_search_criteria)
destroy(this.dw_claim_list)
destroy(this.dw_next_individual_no)
destroy(this.dw_individual_name)
destroy(this.dw_individual)
end on

type cb_open_event_log from commandbutton within w_dup_fix_individual
integer x = 2286
integer y = 2388
integer width = 311
integer height = 100
integer taborder = 130
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Event Log"
end type

event clicked;wf_add_event(FALSE) // merge not completed
end event

type st_2 from statictext within w_dup_fix_individual
integer x = 2382
integer y = 1204
integer width = 155
integer height = 72
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Total:"
boolean focusrectangle = false
end type

type st_total from statictext within w_dup_fix_individual
integer x = 2533
integer y = 1200
integer width = 187
integer height = 72
integer taborder = 140
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "0"
alignment alignment = right!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type cb_search from commandbutton within w_dup_fix_individual
integer x = 2203
integer y = 80
integer width = 311
integer height = 108
integer taborder = 100
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Search"
boolean default = true
end type

event clicked;	SetPointer(HourGlass!)
	cb_save.enabled = FALSE
	cb_cancel.enabled = FALSE
	ib_birthdate = FALSE
	ib_medicare = FALSE
	ib_sin = FALSE
	is_first_entry = ''	
	st_total.text   = '0'
	il_selected_cnt = 0
	
	IF wf_validate_criteria() >= 0 THEN
		IF wf_set_query() >= 0 THEN
			wf_retrieve() 
			
			// reset the dw's 
			dw_individual.Reset()
			dw_main_name.Reset()
			cb_build_individual.enabled = TRUE //2014-11-14 David Worboys : Renamed button & Cleaned up comment
			
			dw_individual_list.Object.default_individual.Protect = 0
			dw_individual_list.Object.selected_individual.Protect = 0
		END IF
	END IF
	
	//only enable the buttons if we have something
	IF dw_individual_list.rowcount() > 0 THEN 
		cb_build_individual.enabled 	= TRUE //2014-11-14 David Worboys : Renamed button
		cb_open_event_log.enabled = TRUE
	ELSE
		cb_build_individual.enabled 	= FALSE //2014-11-14 David Worboys : Renamed button
		cb_open_event_log.enabled = FALSE 
	END IF 
	
end event

type dw_main_name from u_dw_online within w_dup_fix_individual
integer x = 78
integer y = 1308
integer width = 2286
integer height = 92
integer taborder = 80
string dataobject = "d_individual_name_main"
boolean border = false
end type

event itemchanged;call super::itemchanged;Integer li_rtn

//cb_save.enabled = TRUE
//cb_cancel.enabled = TRUE
uf_set_pbmessage(TRUE)
li_rtn = inv_individual.nf_change_item(2)
IF li_rtn = -1 THEN
	RETURN 1
END IF

end event

event editchanged;call super::editchanged;IF len(data) = 1  OR (  len(data) > 1 AND MATCH( MID(data, len(data) - 1, 1) , "[ '-]" ) ) THEN   //looks for a space or a hyphen or a apostrophy
	// this.settext(WordCap(data))  WordCap function works for the space but not the hyphen or apostrophy so manually upper case the last character
	this.settext (  left (data,len(data)-1) + upper(right(data,1)  ))	
	this.SelectText(this.selectedstart() + len(data) + 1, 0)	
END IF

// this removes a leading edge space and any space that follows a space (so there is no double spaces)
IF MATCH( MID(data, len(data) - 1, 1) , "[ ]" )   AND right(data,1) = ' ' THEN
	this.settext (  left (data,len(data)-1) )	
	this.SelectText(this.selectedstart() + len(data) + 1, 0)	
END IF
end event

type dw_medicare from u_dw_online within w_dup_fix_individual
boolean visible = false
integer x = 3040
integer y = 1400
integer height = 360
integer taborder = 20
string dataobject = "d_medicare_check"
end type

type dw_sin from u_dw_online within w_dup_fix_individual
boolean visible = false
integer x = 3058
integer y = 1816
integer height = 360
integer taborder = 10
string dataobject = "d_sin_check"
end type

type dw_individual_list from u_dw_online within w_dup_fix_individual
integer x = 5
integer y = 308
integer width = 2715
integer height = 880
integer taborder = 110
string dataobject = "d_dup_fix_individual_list"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

on rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup
/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	lm_popup.m_options.m_sort.visible = TRUE
	lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))

	Destroy lm_popup
end on

event retrieveend;call super::retrieveend;st_total.text = STRING(this.RowCount())
end event

event itemchanged;call super::itemchanged;STRING ls_check, ls_dwo_name, ls_selected
LONG   ll_cntr

IF dw_individual_list.RowCount() > 0 THEN
	IF row > 0 THEN
		ls_check    = data
		ls_dwo_name = dwo.name
		CHOOSE CASE ls_dwo_name
			CASE 'selected_individual'
				IF ls_check = 'Y' THEN
					dw_individual_list.SetItem(row,'selected_individual',ls_check)
					il_selected_cnt = il_selected_cnt + 1
				ELSE
					dw_individual_list.SetItem(row,'selected_individual',ls_check)
					il_selected_cnt = il_selected_cnt - 1
					IF dw_individual_list.GetItemString(row,'default_individual') = 'Y' THEN
						dw_individual_list.SetItem(row,'default_individual','N')
					END IF
				END IF
			CASE 'default_individual'
				ls_selected = dw_individual_list.GetItemString(row,'selected_individual')
				IF ls_selected = 'Y' THEN
					IF ls_check = 'Y' THEN
						FOR ll_cntr = 1 to THIS.RowCount()
							dw_individual_list.SetItem(ll_cntr,'default_individual','N')
						NEXT
					END IF
					dw_individual_list.SetItem(row, 'default_individual',ls_check)
					ls_check = dw_individual_list.GetItemString(row,'default_individual')
				ELSE
					MessageBox('Error','The individual must be selected to merge before choosing them as the default.',information!)
					dw_individual_list.SetItem(row,'default_individual','N')
					RETURN 2
				END IF
		END CHOOSE
	END IF
END IF

end event

type cb_save from commandbutton within w_dup_fix_individual
integer x = 1019
integer y = 2388
integer width = 274
integer height = 100
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
end type

event clicked;	INTEGER		li_rtn
//	long ll_row, ll_individual_no	//2014-11-17 David Worboys : Variables not used
	N_PROCESS_RUN_STATUS ln_process_run_status
	
	/******************************************************************************************
	P10275 - Daytime Payment Processing
	- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
	- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
	- '009' refers to the Individual Merge module, '044' refers to the Payment Processing module
	- likewise, '046' refers to the ABCC Eligibility Export module
	- likewise, '047' refers to the T5 Printing module
	******************************************************************************************/
	ln_process_run_status = Create N_PROCESS_RUN_STATUS
	
	li_rtn = ln_process_run_status.nf_in_progress('009','044','save',SQLCA)
	
	IF li_rtn = 1 THEN
		// module is blocked
		return
	END IF
	
	//clear variable
	li_rtn = 0
	li_rtn = ln_process_run_status.nf_in_progress('009','046','save',SQLCA)
	
	IF li_rtn = 1 THEN
		// module is blocked
		return
	END IF
		
	//clear variable
	li_rtn= 0
	li_rtn = ln_process_run_status.nf_in_progress('009','047','save',SQLCA)
	
	IF li_rtn = 1 THEN
		// module is blocked
		return
	END IF
	
	/******************************************************************************************/


	IF il_selected_cnt < 2 THEN
		MessageBox('Error','More than one individual must be selected.')
		Return 
	END IF
	
	//used for annuity msg box
	SetNull(idtm_death_date)
	
	
	IF wf_save() >= 0 THEN
		cb_save.enabled 				= FALSE
		cb_cancel.enabled 			= FALSE
		cb_build_individual.enabled = FALSE //2014-11-14 David Worboys : Renamed button
		dw_individual_list.Reset()
		dw_individual.Reset()
		dw_main_name.Reset()
		dw_individual_list.Object.default_individual.Protect = 0
		dw_individual_list.Object.selected_individual.Protect = 0	
		
		// open the Event log and default the values
		wf_add_event(TRUE) // merge completed
		
		IF NOT ISNULL(idtm_death_date) THEN
			//check and see if we need to message the users about a change in the annuity end date
			MessageBox('Annuity End Date','The Annuity End Date has been re-calculated. The new date will be ' + string(date(idtm_death_date)),information!)							
		END IF 
		
	END IF
	
end event

type cb_build_individual from commandbutton within w_dup_fix_individual
integer x = 1797
integer y = 2388
integer width = 480
integer height = 100
integer taborder = 120
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Build Individual"
end type

event clicked;
	SetPointer(HourGlass!)

	IF il_selected_cnt < 2 THEN
		MessageBox('Warning','More than one individual must be selected.')
		Return 
	END IF
	IF wf_set_individual() >= 0 THEN
		cb_save.enabled = TRUE
		cb_cancel.enabled = TRUE
		dw_individual_list.Object.default_individual.Protect = 1
		dw_individual_list.Object.selected_individual.Protect = 1
	END IF
end event

type cb_cancel from commandbutton within w_dup_fix_individual
integer x = 1303
integer y = 2388
integer width = 274
integer height = 100
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Cance&l"
end type

event clicked;/***************************************************/ 
/*	PR4125 - J.Hawker 2004-10-15							*/
/* Reset the window to be like before the Build	   */ 
/*	Individual button was clicked.						*/
/***************************************************/

dw_individual.Reset()
dw_main_name.Reset()

cb_cancel.Enabled 			= FALSE
cb_save.Enabled 				= FALSE
cb_build_individual.Enabled = TRUE //2014-11-14 David Worboys : Renamed button

dw_individual_list.Object.default_individual.Protect = 0
dw_individual_list.Object.selected_individual.Protect = 0
dw_individual.Object.individual_court_order_flag.Protect = 0
dw_individual.Object.event_desc.Protect = 0

end event

type dw_search_criteria from u_dw_online within w_dup_fix_individual
integer x = 5
integer y = 16
integer width = 2715
integer height = 284
integer taborder = 90
string dataobject = "d_individual_search"
borderstyle borderstyle = styleraised!
end type

event editchanged;call super::editchanged;IF dwo.name = 'last_name' OR dwo.name ='first_name' THEN
	IF len(data) = 1  OR (  len(data) > 1 AND MATCH( MID(data, len(data) - 1, 1) , "[ '-]" ) ) THEN   //looks for a space or a hyphen or a apostrophy
		// this.settext(WordCap(data))  WordCap function works for the space but not the hyphen or apostrophy so manually upper case the last character
		this.settext (  left (data,len(data)-1) + upper(right(data,1)  ))	
		this.SelectText(this.selectedstart() + len(data) + 1, 0)	
	END IF
END IF

// this removes a leading edge space and any space that follows a space (so there is no double spaces)
IF dwo.name = 'last_name' OR dwo.name ='first_name' THEN
	IF MATCH( MID(data, len(data) - 1, 1) , "[ ]" )   AND right(data,1) = ' ' THEN
		this.settext (  left (data,len(data)-1) )	
		this.SelectText(this.selectedstart() + len(data) + 1, 0)	
	END IF
END IF
end event

type dw_claim_list from u_dw_online within w_dup_fix_individual
boolean visible = false
integer x = 585
integer y = 2424
integer width = 183
integer height = 160
integer taborder = 40
string dataobject = "d_claims_for_individual"
end type

type dw_next_individual_no from u_dw_online within w_dup_fix_individual
boolean visible = false
integer x = 343
integer y = 2424
integer width = 183
integer height = 160
integer taborder = 30
string dataobject = "d_next_individual_no"
end type

type dw_individual_name from u_dw_online within w_dup_fix_individual
boolean visible = false
integer x = 101
integer y = 2424
integer width = 183
integer height = 160
integer taborder = 70
string dataobject = "d_individual_name"
end type

type dw_individual from u_dw_online within w_dup_fix_individual
integer x = 50
integer y = 1248
integer width = 2647
integer height = 1148
integer taborder = 130
string dataobject = "d_individual"
boolean border = false
end type

event itemchanged;call super::itemchanged;Integer li_rtn

//cb_save.enabled = TRUE
//cb_cancel.enabled = TRUE
IF dwo.name = "individual_court_order_flag" THEN
	this.Modify("event_desc.Visible='1'")
	this.Modify("court_order_t.Visible='1'")
END IF
uf_set_pbmessage(TRUE)
li_rtn = inv_individual.nf_change_item(1)
IF li_rtn = -1 THEN
	RETURN 1
END IF

end event

event itemfocuschanged;/*	override
*/
IF dwo.name = "individual_court_order_flag" THEN
	this.setitem(row,'event_desc','')
END IF
end event

event rowfocuschanged;call super::rowfocuschanged;STRING ls_eventMessage,ls_eventSpecfic
long ll_individual
if this.rowcount() > 0 then
	ll_individual = this.getitemnumber(currentrow,'individual_no')
	SELECT TOP 1 CLAIM_EVENT.event_comment ,event_specific_code  
	INTO :ls_eventMessage,:ls_eventSpecfic 
	FROM CLAIM_EVENT  
	WHERE 	CLAIM_EVENT.event_type_code = '020' AND
		CLAIM_EVENT.claim_no in (  SELECT CLAIM.claim_no  
												FROM CLAIM  
												WHERE CLAIM.individual_no = :ll_individual )   
	Order By CLAIM_EVENT.event_date desc,
				CLAIM_EVENT.event_no desc;
	
	IF len(trim(ls_eventMessage)) > 0 then
		this.setitem(currentrow,'event_desc',ls_eventMessage)
	end if
end if
end event

event retrieveend;call super::retrieveend;STRING ls_eventMessage,ls_eventSpecfic
long ll_individual
if rowcount > 0 then
	ll_individual = this.getitemnumber(this.getrow(),'individual_no')
	SELECT TOP 1 CLAIM_EVENT.event_comment ,event_specific_code  
	INTO :ls_eventMessage,:ls_eventSpecfic 
	FROM CLAIM_EVENT  
	WHERE 	CLAIM_EVENT.event_type_code = '020' AND
		CLAIM_EVENT.claim_no in (  SELECT CLAIM.claim_no  
												FROM CLAIM  
												WHERE CLAIM.individual_no = :ll_individual )   
	Order By CLAIM_EVENT.event_date desc,
				CLAIM_EVENT.event_no desc;
	
	IF len(trim(ls_eventMessage)) > 0 then
		this.setitem(this.getrow(),'event_desc',ls_eventMessage)
	end if
end if
end event

event editchanged;call super::editchanged;IF dwo.name = 'individual_name_last_name' OR dwo.name ='individual_name_given_names' THEN
	IF len(data) = 1  OR (  len(data) > 1 AND MATCH( MID(data, len(data) - 1, 1) , "[ '-]" ) ) THEN   //looks for a space or a hyphen or a apostrophy
		// this.settext(WordCap(data))  WordCap function works for the space but not the hyphen or apostrophy so manually upper case the last character
		this.settext (  left (data,len(data)-1) + upper(right(data,1)  ))	
		this.SelectText(this.selectedstart() + len(data) + 1, 0)	
	END IF
END IF

// this removes a leading edge space and any space that follows a space (so there is no double spaces)
IF dwo.name = 'individual_name_last_name' OR dwo.name ='individual_name_given_names' THEN
	IF MATCH( MID(data, len(data) - 1, 1) , "[ ]" )   AND right(data,1) = ' ' THEN
		this.settext (  left (data,len(data)-1) )	
		this.SelectText(this.selectedstart() + len(data) + 1, 0)	
	END IF
END IF
end event

