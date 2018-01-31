$PBExportHeader$w_cost_analysis.srw
forward
global type w_cost_analysis from w_a_tool
end type
type cb_cancel from commandbutton within w_cost_analysis
end type
type cb_save from commandbutton within w_cost_analysis
end type
type tab_cost_analysis from tab within w_cost_analysis
end type
type tabpage_master from userobject within tab_cost_analysis
end type
type dw_cost_analysis_list from u_dw_online within tabpage_master
end type
type dw_cost_analysis_master from u_dw_online within tabpage_master
end type
type dw_basic_claim_data from u_dw_online within tabpage_master
end type
type cb_add_master from commandbutton within tabpage_master
end type
type tabpage_master from userobject within tab_cost_analysis
dw_cost_analysis_list dw_cost_analysis_list
dw_cost_analysis_master dw_cost_analysis_master
dw_basic_claim_data dw_basic_claim_data
cb_add_master cb_add_master
end type
type tabpage_options from userobject within tab_cost_analysis
end type
type dw_options_tasks from u_dw_online within tabpage_options
end type
type cb_delete_option from commandbutton within tabpage_options
end type
type cb_add_option from commandbutton within tabpage_options
end type
type dw_options_list from u_dw_online within tabpage_options
end type
type dw_options_costs from u_dw_online within tabpage_options
end type
type sle_total_costs from singlelineedit within tabpage_options
end type
type st_1 from statictext within tabpage_options
end type
type tabpage_options from userobject within tab_cost_analysis
dw_options_tasks dw_options_tasks
cb_delete_option cb_delete_option
cb_add_option cb_add_option
dw_options_list dw_options_list
dw_options_costs dw_options_costs
sle_total_costs sle_total_costs
st_1 st_1
end type
type tabpage_cost_comparison from userobject within tab_cost_analysis
end type
type dw_compare_options from u_dw_online within tabpage_cost_comparison
end type
type cb_printtab3 from commandbutton within tabpage_cost_comparison
end type
type tabpage_cost_comparison from userobject within tab_cost_analysis
dw_compare_options dw_compare_options
cb_printtab3 cb_printtab3
end type
type tabpage_summary from userobject within tab_cost_analysis
end type
type uo_summary_report from uo_tabpage_summary_report within tabpage_summary
end type
type tabpage_summary from userobject within tab_cost_analysis
uo_summary_report uo_summary_report
end type
type tab_cost_analysis from tab within w_cost_analysis
tabpage_master tabpage_master
tabpage_options tabpage_options
tabpage_cost_comparison tabpage_cost_comparison
tabpage_summary tabpage_summary
end type
end forward

global type w_cost_analysis from w_a_tool
boolean resizable = false
cb_cancel cb_cancel
cb_save cb_save
tab_cost_analysis tab_cost_analysis
end type
global w_cost_analysis w_cost_analysis

type variables
N_COST_ANALYSIS	inv_cost_analysis
LONG			il_costs_records_for_claim
LONG			il_claim_no
LONG			il_clickedrow
STRING			is_admin_region_code
STRING			is_sex
INTEGER			ii_cost_no
DATETIME		idtm_birth_date
DATETIME		idtm_accident_date
DECIMAL                            idec_authorization_limit
DECIMAL                            idec_recom_option_total_costs
BOOLEAN		ib_fire_window_ftn

end variables

forward prototypes
public function integer wf_retrieve_master_list ()
public subroutine wf_set_state (string as_state)
public function integer wf_delete_blank_tasks ()
public function integer wf_get_openings_for_ben_calcs ()
public subroutine wf_disable_after_entry (u_dw_online adw_enable_this_one)
public subroutine wf_check_disable ()
public subroutine wf_verify_capitalizations ()
public subroutine wf_retrieve_recom_options_list (long al_list_row)
end prototypes

public function integer wf_retrieve_master_list ();/*	This function is used to fill the master list on the first tab.
*/
	INTEGER	li_result

	il_costs_records_for_claim = tab_cost_analysis.tabpage_master.dw_cost_analysis_list.Retrieve(il_claim_no)

	IF SQLCA.nf_handle_error("tab_cost_analysis.tabpage_master.dw_cost_analysis_list.Retrieve(il_claim_no)","w_cost_analysis","wf_retrieve_lists()") < 0 THEN
		RETURN -1
	END IF

/*	Call function to check and see that all option records pass capitalization verification.
*/
	wf_verify_capitalizations()

	cb_cancel.Enabled = FALSE
	cb_save.Enabled = FALSE
	ib_fire_window_ftn = TRUE

	RETURN 0

end function

public subroutine wf_set_state (string as_state);/*	This function is used to set the screen state (Enabled / Disabled appropriate controls)
	based on the action (Adding / Saving) being done.

	Argument :	as_state - The action currently being done in the screen.
*/	
	CHOOSE CASE	as_state
		CASE	'ADD COST'
			inv_cost_analysis.nf_insert()
			inv_cost_analysis.nf_insert_option()
			tab_cost_analysis.tabpage_options.dw_options_list.Reset()
			tab_cost_analysis.tabpage_options.sle_total_costs.Text = String(0,'$###,###.00')
			tab_cost_analysis.tabpage_master.cb_add_master.Enabled = FALSE
			tab_cost_analysis.tabpage_options.cb_add_option.Enabled = FALSE
			tab_cost_analysis.tabpage_options.cb_delete_option.Enabled = FALSE
			cb_save.Enabled = FALSE
			cb_cancel.Enabled = TRUE
			tab_cost_analysis.tabpage_master.dw_cost_analysis_list.Enabled = FALSE
			tab_cost_analysis.tabpage_master.dw_cost_analysis_master.Enabled = FALSE
			tab_cost_analysis.tabpage_options.dw_options_list.Enabled = FALSE
			tab_cost_analysis.tabpage_options.dw_options_tasks.Enabled = TRUE
			tab_cost_analysis.tabpage_options.dw_options_costs.Enabled = TRUE
			tab_cost_analysis.tabpage_cost_comparison.Enabled = FALSE
			tab_cost_analysis.tabpage_summary.Enabled = FALSE
			tab_cost_analysis.tabpage_master.Enabled = FALSE
			tab_cost_analysis.SelectTab(2)
			tab_cost_analysis.tabpage_options.dw_options_costs.SetFocus()
			tab_cost_analysis.tabpage_options.dw_options_costs.SetColumn("option_name")

		CASE	'ADD OPTION'
			inv_cost_analysis.nf_insert_option()
			tab_cost_analysis.tabpage_options.sle_total_costs.Text = String(0,'$###,###.00')
			tab_cost_analysis.tabpage_options.cb_add_option.Enabled = FALSE
			tab_cost_analysis.tabpage_options.cb_delete_option.Enabled = FALSE
			tab_cost_analysis.tabpage_master.cb_add_master.Enabled = FALSE
			cb_save.Enabled = FALSE
			cb_cancel.Enabled = TRUE
			tab_cost_analysis.tabpage_master.dw_cost_analysis_list.Enabled = FALSE
			tab_cost_analysis.tabpage_master.dw_cost_analysis_master.Enabled = FALSE
			tab_cost_analysis.tabpage_options.dw_options_list.Enabled = FALSE
			tab_cost_analysis.tabpage_options.dw_options_tasks.Enabled = TRUE
			tab_cost_analysis.tabpage_options.dw_options_costs.Enabled = TRUE
			tab_cost_analysis.tabpage_cost_comparison.Enabled = FALSE
			tab_cost_analysis.tabpage_summary.Enabled = FALSE
			tab_cost_analysis.tabpage_master.Enabled = FALSE
			tab_cost_analysis.tabpage_options.dw_options_costs.SetFocus()
			tab_cost_analysis.tabpage_options.dw_options_costs.SetColumn("option_name")
			
		CASE	'SAVE'
			ib_fire_window_ftn = TRUE
			tab_cost_analysis.tabpage_master.cb_add_master.Enabled = TRUE
			tab_cost_analysis.tabpage_options.cb_add_option.Enabled = TRUE
			tab_cost_analysis.tabpage_options.cb_delete_option.Enabled = TRUE
			tab_cost_analysis.tabpage_master.dw_cost_analysis_list.Enabled = TRUE
			tab_cost_analysis.tabpage_master.dw_cost_analysis_master.Enabled = TRUE
			tab_cost_analysis.tabpage_options.dw_options_list.Enabled = TRUE
			tab_cost_analysis.tabpage_options.dw_options_costs.Enabled = TRUE
			tab_cost_analysis.tabpage_options.dw_options_tasks.Enabled = TRUE
			tab_cost_analysis.tabpage_cost_comparison.Enabled = TRUE
			tab_cost_analysis.tabpage_summary.Enabled = TRUE
			tab_cost_analysis.tabpage_master.Enabled = TRUE
			
		CASE	'SAVE FAILED'
			IF tab_cost_analysis.tabpage_options.dw_options_tasks.RowCount() = 0 THEN
				tab_cost_analysis.tabpage_options.dw_options_tasks.InsertRow(0)
			END IF
			
		CASE	'CANCEL'
			tab_cost_analysis.tabpage_master.cb_add_master.Enabled = TRUE
			tab_cost_analysis.tabpage_options.cb_add_option.Enabled = TRUE
			tab_cost_analysis.tabpage_options.cb_delete_option.Enabled = TRUE
			tab_cost_analysis.tabpage_master.dw_cost_analysis_list.Enabled = TRUE
			tab_cost_analysis.tabpage_master.dw_cost_analysis_master.Enabled = TRUE
			tab_cost_analysis.tabpage_options.dw_options_list.Enabled = TRUE
			tab_cost_analysis.tabpage_options.dw_options_costs.Enabled = TRUE
			tab_cost_analysis.tabpage_options.dw_options_tasks.Enabled = TRUE
			tab_cost_analysis.tabpage_cost_comparison.Enabled = TRUE
			tab_cost_analysis.tabpage_summary.Enabled = TRUE
			tab_cost_analysis.tabpage_master.Enabled = TRUE
			
	END CHOOSE
	
	RETURN
	
end subroutine

public function integer wf_delete_blank_tasks ();/*	This function is used to delete any blank task records within the datawindow
	dw_options_tasks. There are two ways that a record is eligible. First, records
	where no information is entered and secondly, when the del flag is checked.
*/
	LONG		ll_loopcount, ll_rowsindw
	STRING	ls_delete_record, ls_type_code, ls_sub_type_code, ls_specific_type_code, ls_comment
	DECIMAL	ldec_task_amount
	INTEGER	li_result
	
	ll_loopcount = 1
	ll_rowsindw = tab_cost_analysis.tabpage_options.dw_options_tasks.RowCount()
	DO WHILE ll_loopcount <= ll_rowsindw
		ls_delete_record		 = tab_cost_analysis.tabpage_options.dw_options_tasks.GetItemString(ll_loopcount,'delete_flag')
		ls_type_code			 = tab_cost_analysis.tabpage_options.dw_options_tasks.GetItemString(ll_loopcount,'task_type_code')
		ls_sub_type_code		 = tab_cost_analysis.tabpage_options.dw_options_tasks.GetItemString(ll_loopcount,'task_sub_type_code')
		ls_specific_type_code = tab_cost_analysis.tabpage_options.dw_options_tasks.GetItemString(ll_loopcount,'task_specific_code')
		ls_comment				 = tab_cost_analysis.tabpage_options.dw_options_tasks.GetItemString(ll_loopcount,'comment')
		ldec_task_amount		 = tab_cost_analysis.tabpage_options.dw_options_tasks.GetItemNumber(ll_loopcount,'task_amount')
		IF ls_delete_record = '1' OR (ls_type_code = "" AND ls_sub_type_code = "" AND ls_specific_type_code = "." AND &
			ls_comment = "" AND ldec_task_amount = 0) THEN
			li_result = tab_cost_analysis.tabpage_options.dw_options_tasks.DeleteRow(ll_loopcount)
			IF li_result = 1 THEN
				ll_rowsindw --
			ELSE
				RETURN -1
			END IF
		ELSE
			ll_loopcount ++
		END IF
	LOOP

	RETURN 0
	
end function

public function integer wf_get_openings_for_ben_calcs ();///*	This function is used to determine the opening numbers for any of the entered
//	benefit calcs.
//*/
//	INTEGER	li_rloe_ben_calc, li_ltd_ben_calc, li_capitalization_ben_calc, li_rloe_opening
//	INTEGER	li_ltd_opening, li_capitalization_opening
//
//	li_rloe_ben_calc = tab_cost_analysis.tabpage_options.dw_options_costs.GetItemNumber(1,'rloe_benefit_calc_no')
//	li_ltd_ben_calc = tab_cost_analysis.tabpage_options.dw_options_costs.GetItemNumber(1,'ltd_benefit_calc_no')
//	li_capitalization_ben_calc = tab_cost_analysis.tabpage_options.dw_options_costs.GetItemNumber(1,'capitalization_benefit_calc_no')
//
//	IF li_rloe_ben_calc > 0 THEN
//		SELECT opening_no  
//		  INTO :li_rloe_opening
//		  FROM BENEFIT_CALCULATION  
//		 WHERE claim_no = :il_claim_no
//			AND benefit_calculation_no = :li_rloe_ben_calc
//		 USING SQLCA;
//
//		IF SQLCA.nf_handle_error("Embedded SQL: SELECT opening_no INTO ...","w_cost_analysis","wf_get_openings_for_ben_calcs()") < 0 THEN
//			RETURN -1
//		END IF
//		tab_cost_analysis.tabpage_options.dw_options_costs.SetItem(1,'rloe_opening',li_rloe_opening)
//	END IF
//
//	IF li_ltd_ben_calc > 0 THEN
//		SELECT opening_no  
//		  INTO :li_ltd_opening
//		  FROM BENEFIT_CALCULATION  
//		 WHERE claim_no = :il_claim_no
//			AND benefit_calculation_no = :li_ltd_ben_calc
//		 USING SQLCA;
//
//		IF SQLCA.nf_handle_error("Embedded SQL: SELECT opening_no INTO ...","w_cost_analysis","wf_get_openings_for_ben_calcs()") < 0 THEN
//			RETURN -1
//		END IF
//		tab_cost_analysis.tabpage_options.dw_options_costs.SetItem(1,'ltd_opening',li_ltd_opening)
//	END IF
//
//	IF li_capitalization_ben_calc > 0 THEN
//		SELECT opening_no  
//		  INTO :li_capitalization_opening
//		  FROM BENEFIT_CALCULATION  
//		 WHERE claim_no = :il_claim_no
//			AND benefit_calculation_no = :li_capitalization_ben_calc
//		 USING SQLCA;
//
//		IF SQLCA.nf_handle_error("Embedded SQL: SELECT opening_no INTO ...","w_cost_analysis","wf_get_openings_for_ben_calcs()") < 0 THEN
//			RETURN -1
//		END IF
//		tab_cost_analysis.tabpage_options.dw_options_costs.SetItem(1,'capitalization_opening',li_capitalization_opening)
//	END IF

	RETURN 0
	
end function

public subroutine wf_disable_after_entry (u_dw_online adw_enable_this_one);tab_cost_analysis.tabpage_master.dw_cost_analysis_list.Enabled = FALSE
tab_cost_analysis.tabpage_master.dw_cost_analysis_master.Enabled = FALSE
tab_cost_analysis.tabpage_options.dw_options_list.Enabled = FALSE
tab_cost_analysis.tabpage_options.dw_options_costs.Enabled = FALSE
tab_cost_analysis.tabpage_options.dw_options_tasks.Enabled = FALSE
tab_cost_analysis.tabpage_master.cb_add_master.Enabled = FALSE
tab_cost_analysis.tabpage_options.cb_add_option.Enabled = FALSE
tab_cost_analysis.tabpage_options.cb_delete_option.Enabled = FALSE
adw_enable_this_one.Enabled = TRUE
CHOOSE CASE	adw_enable_this_one.DataObject
	CASE	'd_options_costs'
		tab_cost_analysis.tabpage_options.dw_options_tasks.Enabled = TRUE
	CASE	'd_options_tasks'
		tab_cost_analysis.tabpage_options.dw_options_costs.Enabled = TRUE
END CHOOSE
cb_save.Enabled = TRUE
cb_cancel.Enabled = TRUE
adw_enable_this_one.SetFocus()
end subroutine

public subroutine wf_check_disable ();/*	If the person has access to the Cost Analysis module and the cost analysis is not authorized, anything can be changed/saved.
	If the cost analysis is authorized and the person has adequate authorization limit for the current total costs for the recommended option,
	they can change anything. 	Note that in the latter case, the person must have adequate authorization limit for the changes to save and 
	re-authorize it.  If not, they won't be able to save/re-authorize the revised cost analysis.
*/
	LONG		ll_row, ll_rowcount
	STRING	ls_authorized_by, ls_admin_region_code
	DATETIME	ldtm_today
	INTEGER	li_recommended_option_no
	
	tab_cost_analysis.tabpage_options.cb_delete_option.enabled = FALSE
	tab_cost_analysis.tabpage_options.cb_add_option.enabled = FALSE
	tab_cost_analysis.tabpage_master.dw_cost_analysis_master.Enabled = FALSE
	tab_cost_analysis.tabpage_options.dw_options_costs.Enabled = FALSE
	tab_cost_analysis.tabpage_options.dw_options_tasks.Enabled = FALSE

	ls_authorized_by = tab_cost_analysis.tabpage_master.dw_cost_analysis_master.GetItemString(1,'authorized_user_id')
	
	IF ls_authorized_by > '' THEN
	
/*	If the limit is adequate for the recommended option's total costs, then unprotect all.
*/
		li_recommended_option_no = tab_cost_analysis.tabpage_master.dw_cost_analysis_master.GetItemNumber(1,"recommended_option_no")
		idec_recom_option_total_costs = inv_cost_analysis.nf_get_recom_option_total_costs(ii_cost_no,li_recommended_option_no)
		IF	idec_recom_option_total_costs < 0 THEN
				
/*	If couldn't determine total costs, leave everything protected and return.
*/
			RETURN
		END IF
			
		IF idec_authorization_limit >= idec_recom_option_total_costs THEN
				
/*	Unprotect if authorization limit is adequate.
*/
			tab_cost_analysis.tabpage_master.dw_cost_analysis_master.Enabled = TRUE
			tab_cost_analysis.tabpage_options.dw_options_costs.Enabled = TRUE
			tab_cost_analysis.tabpage_options.dw_options_tasks.Enabled = TRUE
			tab_cost_analysis.tabpage_options.cb_delete_option.enabled = TRUE
			tab_cost_analysis.tabpage_options.cb_add_option.enabled = TRUE
				
/*	Set the authorize flag on so that the entry can be authorized again.
*/
			tab_cost_analysis.tabpage_master.dw_cost_analysis_master.SetItem(1,"authorize_flag",'Y')
		END IF
	ELSE
		
/*	Don't protect anything if cost analysis is not authorized.
*/
		tab_cost_analysis.tabpage_master.dw_cost_analysis_master.Enabled = TRUE
		tab_cost_analysis.tabpage_options.dw_options_costs.Enabled = TRUE
		tab_cost_analysis.tabpage_options.dw_options_tasks.Enabled = TRUE
		tab_cost_analysis.tabpage_options.cb_delete_option.enabled = TRUE
		tab_cost_analysis.tabpage_options.cb_add_option.enabled = TRUE
	END IF

	RETURN
	
end subroutine

public subroutine wf_verify_capitalizations ();/*	This function loops through all the cost records for a claim and returns their option
	records. These option records are then checked to see if they meet the capitalization
	verification rules.
*/
	LONG 					ll_rowcount, ll_row, ll_row_master, ll_capitalization_factor_yyyymm
	INTEGER				li_cost_no, li_option_no, li_capitalization_bene_calc_no, li_result, li_saved_age, li_calc_age
	DECIMAL				ldec_saved_capitalized_amount, ldec_calc_capitalized_amount, ldec_award_amount
	DATE					ldt_capitalization_date
	DATETIME				ldtm_end_date
	STRING				ls_message

	IF il_costs_records_for_claim = 0 THEN
		RETURN
	END IF

	ll_row_master = 1
	DO WHILE ll_row_master <= il_costs_records_for_claim
		li_cost_no = tab_cost_analysis.tabpage_master.dw_cost_analysis_list.GetItemNumber(ll_row_master,"cost_no")
		ll_rowcount = inv_cost_analysis.nf_retrieve_option_list(li_cost_no,tab_cost_analysis.tabpage_options.dw_options_list)
		ll_row =	1
		DO WHILE ll_row <= ll_rowcount
			tab_cost_analysis.tabpage_options.dw_options_list.uf_processselect(ll_row,'Mouse')
			li_capitalization_bene_calc_no = tab_cost_analysis.tabpage_options.dw_options_list.GetItemNumber(ll_row,'capitalization_benefit_calc_no')
			IF li_capitalization_bene_calc_no > 0 THEN
				li_option_no = tab_cost_analysis.tabpage_options.dw_options_list.GetItemNumber(ll_row,'option_no')
				ldec_saved_capitalized_amount = tab_cost_analysis.tabpage_options.dw_options_list.GetItemDecimal(ll_row,"capitalized_amount")
				li_saved_age = tab_cost_analysis.tabpage_options.dw_options_list.GetItemNumber(ll_row,"age")
				ldtm_end_date = tab_cost_analysis.tabpage_options.dw_options_list.GetItemDateTime(ll_row,"end_date")
				li_calc_age = f_calculate_age(Date(idtm_birth_date),Date(ldtm_end_date))
				IF li_calc_age > 64 THEN
					inv_cost_analysis.ib_capitalization_difference_flag = TRUE
					MessageBox("Age Greater Than 64", "Age must be less than or equal to 64 years.~r~n~r~nVerify/Correct Individual Data and Correct all Cost Analysis with Capitalizations",Information!)
					GOTO GETRECORD
				END IF
				ldec_award_amount = tab_cost_analysis.tabpage_options.dw_options_list.GetItemDecimal(ll_row,'award_amount')
				ldt_capitalization_date = Date(ldtm_end_date)
				ll_capitalization_factor_yyyymm = Long(String(ldt_capitalization_date,'YYYYMMDD'))/100
				ldec_calc_capitalized_amount = 0
				li_result = inv_cost_analysis.nf_calc_capitalized_amt(ldec_award_amount,li_calc_age,ll_capitalization_factor_yyyymm,ldec_calc_capitalized_amount)

				IF li_result < 0 THEN
					inv_cost_analysis.ib_capitalization_difference_flag = TRUE
					MessageBox("Capitalized Amount Error","Unable to calculate the capitalized amount.")
					GOTO GETRECORD
				END IF
				IF Round(ldec_calc_capitalized_amount,2) <> Round(ldec_saved_capitalized_amount,2) OR li_calc_age <> li_saved_age THEN
					inv_cost_analysis.ib_capitalization_difference_flag = TRUE
					ls_message = "Cost Analysis: " + String(li_cost_no)
					ls_message = ls_message + "~r~n~r~nAges for Option: " + String(li_option_no)
					ls_message = ls_message + "~r~nOld/New Ages: " + String(li_saved_age) + " / " + String(li_calc_age)
					ls_message = ls_message + "~r~n~r~nCapitalized Amounts for Option: " + String(li_option_no)
					ls_message = ls_message + "~r~nOld/New Amounts: " + String(ldec_saved_capitalized_amount,'$###,###.00') + " / " + String(ldec_calc_capitalized_amount,'$###,###.00')
					ls_message = ls_message + "~r~n~r~nVerify/Correct Individual Data and Correct all Cost Analysis with Capitalizations"
					MessageBox ("Capitalized Amount/Age Different", ls_message,Information!)
					tab_cost_analysis.tabpage_options.dw_options_list.TriggerEvent(Clicked!)
					GOTO GETRECORD
				END IF
			END IF
			ll_row ++
		LOOP
		ll_row_master ++
	LOOP

	ll_row = 1	
	ll_row_master = 1
GETRECORD:	
	
	
	li_cost_no = tab_cost_analysis.tabpage_master.dw_cost_analysis_list.GetItemNumber(ll_row_master,"cost_no")
	ll_rowcount = inv_cost_analysis.nf_retrieve_option_list(li_cost_no,tab_cost_analysis.tabpage_options.dw_options_list)
	tab_cost_analysis.tabpage_options.dw_options_list.uf_processselect(ll_row,'Mouse')
	tab_cost_analysis.tabpage_options.dw_options_list.TriggerEvent(Clicked!)
	wf_retrieve_recom_options_list(ll_row_master)

	RETURN
end subroutine

public subroutine wf_retrieve_recom_options_list (long al_list_row);/*	This function is used to populate the drop down datawindow on the cost analysis
	master record's 'recommended_option_no' column.
*/
	INTEGER 				li_result, li_option_no
	LONG 				 	ll_child_row, ll_row, ll_row_count
	STRING				ls_option_name
	DATAWINDOWCHILD 	ldwc_child
	DATE					ldt_start_date, ldt_end_date

	
	li_result = tab_cost_analysis.tabpage_master.dw_cost_analysis_master.GetChild('recommended_option_no',ldwc_child)
	li_result = ldwc_child.Reset()
	ll_row_count = tab_cost_analysis.tabpage_options.dw_options_list.RowCount()
	ll_row = 1
	
	DO WHILE ll_row <= ll_row_count
		li_option_no = tab_cost_analysis.tabpage_options.dw_options_list.GetItemNumber(ll_row,"option_no")
		ls_option_name = tab_cost_analysis.tabpage_options.dw_options_list.GetItemString(ll_row,"option_name")
		ldt_start_date = Date(tab_cost_analysis.tabpage_options.dw_options_list.GetItemDatetime(ll_row,"start_date"))
		ldt_end_date = Date(tab_cost_analysis.tabpage_options.dw_options_list.GetItemDatetime(ll_row,"end_date"))
		li_result = ldwc_child.InsertRow(0)
		li_result = ldwc_child.SetItem(ll_row,"option_no",li_option_no)
		li_result = ldwc_child.SetItem(ll_row,"option_name",ls_option_name)
		li_result = ldwc_child.SetItem(ll_row,"start_date",ldt_start_date)
		li_result = ldwc_child.SetItem(ll_row,"end_date",ldt_end_date)
		ll_row ++
	LOOP
	IF tab_cost_analysis.tabpage_master.dw_cost_analysis_master.RowCount() > 0 THEN
		tab_cost_analysis.tabpage_master.dw_cost_analysis_master.SetItem(1,"recommended_option_no",tab_cost_analysis.tabpage_master.dw_cost_analysis_list.GetItemNumber(al_list_row,"recommended_option_no"))
	END IF	
	

end subroutine

on w_cost_analysis.create
int iCurrent
call super::create
this.cb_cancel=create cb_cancel
this.cb_save=create cb_save
this.tab_cost_analysis=create tab_cost_analysis
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_cancel
this.Control[iCurrent+2]=this.cb_save
this.Control[iCurrent+3]=this.tab_cost_analysis
end on

on w_cost_analysis.destroy
call super::destroy
destroy(this.cb_cancel)
destroy(this.cb_save)
destroy(this.tab_cost_analysis)
end on

event open;call super::open;	U_DW_ONLINE	ldw_dw[]
	INTEGER		li_result

/*	Set the instance variable with the current claim number from the tombstone.
*/
	il_claim_no				= iw_active_sheet.dw_basic_claim.GetItemNumber(1,'claim_no')
	is_admin_region_code	= iw_active_sheet.dw_basic_claim.GetItemString(1,'admin_region_code')
	is_sex					= iw_active_sheet.dw_basic_claim.GetItemString(1,'sex')
	idtm_birth_date		= iw_active_sheet.dw_basic_claim.GetItemDatetime(1,'birth_date')
	idtm_accident_date	= iw_active_sheet.dw_basic_claim.GetItemDatetime(1,'accident_date')

/*	Create the nonvisual user object inv_cost_analysis.
*/
	inv_cost_analysis = Create n_cost_analysis

/*	Initialize the array with updateable datawindows and call the function to initialize them.
*/
	ldw_dw[1] = tab_cost_analysis.tabpage_master.dw_cost_analysis_master
	ldw_dw[2] = tab_cost_analysis.tabpage_options.dw_options_costs
	ldw_dw[3] = tab_cost_analysis.tabpage_options.dw_options_tasks

	inv_cost_analysis.nf_init(ldw_dw[],SQLCA,THIS)
	
/*	Call function to set the instance variables within the user object.
*/
	inv_cost_analysis.nf_set_instance_variables(il_claim_no,is_sex,idtm_birth_date,is_admin_region_code,FALSE,idtm_accident_date)
	
/* Check to see if the person has authorization for this region */
	idec_authorization_limit = gnv_user_authorizations.nf_get_authorization_limit(is_admin_region_code,"cst")
	IF idec_authorization_limit < 0 OR ISNULL(idec_authorization_limit) THEN
   	MessageBox('Invalid Authorization', "You do not have a valid Authorization amount for this region")
		Close(THIS)
		RETURN
	END IF
	
/* set the authorization level on the business rule object */
	inv_cost_analysis.idec_authorization_limit = idec_authorization_limit 
	
/*	Call the user object function to set the committing of the records.
*/
	inv_cost_analysis.nf_set_commit(TRUE)

/*	Initialize all othe non-updateable datawindows.
*/
	tab_cost_analysis.tabpage_master.dw_cost_analysis_list.SetTransObject(SQLCA)
	tab_cost_analysis.tabpage_options.dw_options_list.SetTransObject(SQLCA)
	tab_cost_analysis.tabpage_cost_comparison.dw_compare_options.SetTransObject(SQLCA)
	tab_cost_analysis.tabpage_summary.uo_summary_report.dw_summary_report.SetTransObject(SQLCA)
	tab_cost_analysis.tabpage_summary.uo_summary_report.dw_print_one_option.SetTransObject(SQLCA)
	tab_cost_analysis.tabpage_summary.uo_summary_report.dw_print_version.SetTransObject(SQLCA)
	
/*	Set the lists on each tab so that 1 row is selected at a time.
*/
	tab_cost_analysis.tabpage_master.dw_cost_analysis_list.uf_setselect(1)
	tab_cost_analysis.tabpage_options.dw_options_list.uf_setselect(1)

/*	Call function to do initial retrieve into master list (dw_cost_analysis_list).
*/
	li_result = wf_retrieve_master_list()
	IF li_result < 0 THEN
		Close(THIS)
		RETURN
	END IF

end event

event close;call super::close;destroy n_cost_analysis
end event

event closequery;call super::closequery;IF cb_save.Enabled THEN
   IF MessageBox('Warning','Data not saved. Save now?',Question!,YesNo!) = 1 THEN
      cb_save.TriggerEvent(Clicked!)
      IF cb_save.Enabled THEN
         RETURN 1
      END IF
   END IF
END IF

end event

type st_title from w_a_tool`st_title within w_cost_analysis
string text = "Cost Analysis "
end type

type cb_close from w_a_tool`cb_close within w_cost_analysis
integer x = 2258
integer y = 1724
integer height = 92
integer taborder = 20
end type

type cb_cancel from commandbutton within w_cost_analysis
integer x = 1733
integer y = 1724
integer width = 379
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Cance&l"
end type

event clicked;	LONG					ll_return
	DATAWINDOWCHILD	ldwc_tasktype
	
	wf_set_state('CANCEL')

/*	If canceling a new option add, set active task type filter off
*/
	ll_return = tab_cost_analysis.tabpage_options.dw_options_tasks.GetChild('task_type_code', ldwc_tasktype)
	IF ll_return > 0 THEN
		ldwc_tasktype.SetFilter("")
		ldwc_tasktype.Filter()
	END IF

/*	Call function to do initial retrieve again.
*/
	wf_retrieve_master_list()

end event

type cb_save from commandbutton within w_cost_analysis
integer x = 1339
integer y = 1724
integer width = 379
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
end type

event clicked;	INTEGER	li_result, li_trancount
	LONG		ll_return
	DATAWINDOWCHILD	ldwc_tasktype

/*	Call function to save the 'updateable' datawindows on the first two tabs. Need to delete
	any unwanted tasks from dw_options_tasks.
*/
	li_result = wf_delete_blank_tasks()
	IF li_result = 0 THEN
		SQLCA.nf_begin_transaction()

		li_result = inv_cost_analysis.nf_save()
		IF li_result = 0 THEN
			SQLCA.nf_commit_transaction()

			wf_set_state('SAVE')

/*	Call function to do re-retrieval into master list (dw_cost_analysis_list).
*/
			wf_retrieve_master_list()
		ELSE
			SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
			IF li_trancount > 0 THEN
				SQLCA.nf_rollback_transaction()
			END IF

			wf_set_state('SAVE FAILED')
		END IF
	ELSE
		MessageBox("Save Error","An error occured while trying to delete unused tasks, please try to save again.")
	END IF

/*	If saving new or changed option tasks, set active task type filter off
*/
	ll_return = tab_cost_analysis.tabpage_options.dw_options_tasks.GetChild('task_type_code', ldwc_tasktype)
	IF ll_return > 0 THEN
		ldwc_tasktype.SetFilter("")
		ldwc_tasktype.Filter()
	END IF

end event

type tab_cost_analysis from tab within w_cost_analysis
event selectionchanged pbm_tcnselchanged
event create ( )
event destroy ( )
integer y = 80
integer width = 2638
integer height = 1648
integer taborder = 10
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
boolean raggedright = true
integer selectedtab = 1
tabpage_master tabpage_master
tabpage_options tabpage_options
tabpage_cost_comparison tabpage_cost_comparison
tabpage_summary tabpage_summary
end type

event selectionchanged;LONG		ll_rows, ll_current_row
STRING 	ls_claim_name
INTEGER	li_option_no


CHOOSE CASE newindex
	CASE	1
		ll_rows = tab_cost_analysis.tabpage_master.dw_cost_analysis_list.GetRow()
		wf_retrieve_recom_options_list(ll_rows)
		tab_cost_analysis.tabpage_options.dw_options_costs.AcceptText()
		tab_cost_analysis.tabpage_options.dw_options_tasks.AcceptText()
	CASE	2	
		tab_cost_analysis.tabpage_master.dw_cost_analysis_master.AcceptText()
	CASE	3	
		ls_claim_name = iw_active_sheet.dw_basic_claim.GetItemString(1,'given_names') + " " + iw_active_sheet.dw_basic_claim.GetItemString(1,'last_name')
		tab_cost_analysis.tabpage_cost_comparison.dw_compare_options.Retrieve(il_claim_no,ls_claim_name,is_sex,ii_cost_no)
		tab_cost_analysis.tabpage_cost_comparison.dw_compare_options.Object.ltd_calculation.Visible = TRUE
		tab_cost_analysis.tabpage_cost_comparison.dw_compare_options.Object.ltd_calculation_print.Visible = FALSE
	CASE	4
		tabpage_summary.uo_summary_report.dw_summary_report.Retrieve(il_claim_no,ii_cost_no)
		li_option_no = tab_cost_analysis.tabpage_master.dw_cost_analysis_master.GetItemNumber(1,'recommended_option_no')
		tabpage_summary.uo_summary_report.dw_print_one_option.Retrieve(il_claim_no,ii_cost_no,li_option_no)
		tabpage_summary.uo_summary_report.dw_print_version.Retrieve(il_claim_no,ii_cost_no)
END CHOOSE
	
end event

on tab_cost_analysis.create
this.tabpage_master=create tabpage_master
this.tabpage_options=create tabpage_options
this.tabpage_cost_comparison=create tabpage_cost_comparison
this.tabpage_summary=create tabpage_summary
this.Control[]={this.tabpage_master,&
this.tabpage_options,&
this.tabpage_cost_comparison,&
this.tabpage_summary}
end on

on tab_cost_analysis.destroy
destroy(this.tabpage_master)
destroy(this.tabpage_options)
destroy(this.tabpage_cost_comparison)
destroy(this.tabpage_summary)
end on

type tabpage_master from userobject within tab_cost_analysis
event create ( )
event destroy ( )
integer x = 18
integer y = 112
integer width = 2601
integer height = 1520
long backcolor = 67108864
string text = "Cost Analysis"
long tabtextcolor = 33554432
string picturename = "Custom045!"
long picturemaskcolor = 553648127
dw_cost_analysis_list dw_cost_analysis_list
dw_cost_analysis_master dw_cost_analysis_master
dw_basic_claim_data dw_basic_claim_data
cb_add_master cb_add_master
end type

on tabpage_master.create
this.dw_cost_analysis_list=create dw_cost_analysis_list
this.dw_cost_analysis_master=create dw_cost_analysis_master
this.dw_basic_claim_data=create dw_basic_claim_data
this.cb_add_master=create cb_add_master
this.Control[]={this.dw_cost_analysis_list,&
this.dw_cost_analysis_master,&
this.dw_basic_claim_data,&
this.cb_add_master}
end on

on tabpage_master.destroy
destroy(this.dw_cost_analysis_list)
destroy(this.dw_cost_analysis_master)
destroy(this.dw_basic_claim_data)
destroy(this.cb_add_master)
end on

type dw_cost_analysis_list from u_dw_online within tabpage_master
integer x = 14
integer y = 12
integer width = 2574
integer height = 400
integer taborder = 10
string dataobject = "d_cost_analysis_list"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event editchanged;call super::editchanged;cb_cancel.Enabled = TRUE
cb_save.Enabled = TRUE

end event

event clicked;call super::clicked;/*	Call user object function to retrieve the master details.
*/

/*	Need code to call the authorization function.
*/	

	INTEGER	li_result, li_option_no
	LONG		ll_rowsindw

	IF row > 0 THEN
		THIS.uf_processselect(row,'Mouse')
		ii_cost_no = THIS.GetItemNumber(row,'cost_no')
		inv_cost_analysis.nf_retrieve_master_details(ii_cost_no)
		inv_cost_analysis.nf_retrieve_option_list(ii_cost_no,tab_cost_analysis.tabpage_options.dw_options_list)
		IF tab_cost_analysis.tabpage_options.dw_options_list.RowCount() > 0 THEN
			tab_cost_analysis.tabpage_options.dw_options_list.TriggerEvent(Clicked!)
		END IF
		wf_retrieve_recom_options_list(row)
		wf_check_disable()
	END IF

end event

event retrieveend;call super::retrieveend;	INTEGER	li_result, li_option_no
	LONG		ll_rowsindw

	IF rowcount > 0 THEN
		THIS.uf_processselect(1,'Mouse')
		ii_cost_no = THIS.GetItemNumber(1,'cost_no')
		inv_cost_analysis.nf_retrieve_master_details(ii_cost_no)
		wf_check_disable()
	ELSE

/*	Set up the screen so that the user can enter a new option.
*/
		tab_cost_analysis.tabpage_master.cb_add_master.PostEvent(Clicked!)
	END IF

end event

event rowfocuschanged;call super::rowfocuschanged;//LONG		ll_row
//INTEGER	li_cost_no, li_option_no
//
//ll_row = THIS.GetRow()
//IF ll_row = 0 THEN
//	RETURN
//END IF
//
//uf_processselect(ll_row,"Mouse")
//THIS.TriggerEvent(Clicked!)

end event

type dw_cost_analysis_master from u_dw_online within tabpage_master
integer x = 9
integer y = 416
integer width = 2601
integer height = 844
integer taborder = 20
boolean bringtotop = true
string dataobject = "d_cost_analysis_master"
boolean border = false
end type

event itemchanged;call super::itemchanged;inv_cost_analysis.nf_change_item(1)

IF ib_fire_window_ftn THEN
	wf_disable_after_entry(THIS)
	ib_fire_window_ftn = FALSE
END IF

end event

event retrieveend;call super::retrieveend;IF THIS.Visible = FALSE THEN THIS.Visible = TRUE
		
end event

event editchanged;call super::editchanged;IF ib_fire_window_ftn THEN
	wf_disable_after_entry(THIS)
	ib_fire_window_ftn = FALSE
END IF

end event

type dw_basic_claim_data from u_dw_online within tabpage_master
boolean visible = false
integer x = 1970
integer y = 1288
integer height = 116
integer taborder = 20
boolean bringtotop = true
string dataobject = "d_basic_claim_data"
end type

type cb_add_master from commandbutton within tabpage_master
integer x = 9
integer y = 1380
integer width = 379
integer height = 100
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Add Cost"
end type

event clicked;wf_set_state('ADD COST')

end event

type tabpage_options from userobject within tab_cost_analysis
event create ( )
event destroy ( )
integer x = 18
integer y = 112
integer width = 2601
integer height = 1520
long backcolor = 67108864
string text = "Options    "
long tabtextcolor = 33554432
string picturename = "Custom006!"
long picturemaskcolor = 553648127
dw_options_tasks dw_options_tasks
cb_delete_option cb_delete_option
cb_add_option cb_add_option
dw_options_list dw_options_list
dw_options_costs dw_options_costs
sle_total_costs sle_total_costs
st_1 st_1
end type

on tabpage_options.create
this.dw_options_tasks=create dw_options_tasks
this.cb_delete_option=create cb_delete_option
this.cb_add_option=create cb_add_option
this.dw_options_list=create dw_options_list
this.dw_options_costs=create dw_options_costs
this.sle_total_costs=create sle_total_costs
this.st_1=create st_1
this.Control[]={this.dw_options_tasks,&
this.cb_delete_option,&
this.cb_add_option,&
this.dw_options_list,&
this.dw_options_costs,&
this.sle_total_costs,&
this.st_1}
end on

on tabpage_options.destroy
destroy(this.dw_options_tasks)
destroy(this.cb_delete_option)
destroy(this.cb_add_option)
destroy(this.dw_options_list)
destroy(this.dw_options_costs)
destroy(this.sle_total_costs)
destroy(this.st_1)
end on

type dw_options_tasks from u_dw_online within tabpage_options
integer x = 18
integer y = 896
integer width = 2574
integer height = 476
integer taborder = 30
boolean bringtotop = true
string dataobject = "d_options_tasks"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;call super::itemchanged;/*	Call function to run itemchanged code. Set the value on sle_total_costs.
*/
	IF ib_fire_window_ftn THEN
		wf_disable_after_entry(THIS)
		ib_fire_window_ftn = FALSE
	END IF
	inv_cost_analysis.nf_change_item(3)
	tab_cost_analysis.tabpage_options.sle_total_costs.Text = String(inv_cost_analysis.idec_total_costs,'$###,###.00')

end event

event itemfocuschanged;call super::itemfocuschanged;	DATAWINDOWCHILD	ldwc_child
	INTEGER				li_result

	IF row = THIS.RowCount() AND dwo.Name = 'comment' THEN
		THIS.InsertRow(0)
		inv_cost_analysis.nf_set_defaults(THIS)
		RETURN
	END IF

/* Call function to handle the filtering of the three drop down data windows.
*/
	li_result = inv_cost_analysis.nf_item_focus_change_task(row,dwo.Name)
	RETURN li_result
end event

event losefocus;call super::losefocus;THIS.PostEvent('ue_post_losefocus')
end event

event ue_post_losefocus;call super::ue_post_losefocus;/*	Call function to run itemchanged code. Set the value on sle_total_costs.
*/
	inv_cost_analysis.nf_change_item(3)
	tab_cost_analysis.tabpage_options.sle_total_costs.Text = String(inv_cost_analysis.idec_total_costs,'$###,###.00')

end event

event retrieveend;call super::retrieveend;inv_cost_analysis.nf_determine_total_costs()
tab_cost_analysis.tabpage_options.sle_total_costs.Text = String(inv_cost_analysis.idec_total_costs,'$###,###.00')

end event

event getfocus;call super::getfocus;DATAWINDOWCHILD	ldwc_tasktype
STRING				ls_filter
LONG					ll_return

/* Filter task types dddw so only active task types are displayed (i.e. user can only select
	an active task type when creating a new option).
*/
ls_filter = "active_flag = 'Y'"
ll_return = dw_options_tasks.GetChild('task_type_code', ldwc_tasktype)
IF ll_return > 0 THEN
	ldwc_tasktype.SetFilter(ls_filter)
	ldwc_tasktype.SetSort("task_type_code A")
	ldwc_tasktype.Filter()
	ldwc_tasktype.Sort()
ELSE
	Return -1
END IF

Return 0
end event

event editchanged;call super::editchanged;IF ib_fire_window_ftn THEN
	wf_disable_after_entry(THIS)
	ib_fire_window_ftn = FALSE
END IF

end event

type cb_delete_option from commandbutton within tabpage_options
integer x = 411
integer y = 1380
integer width = 407
integer height = 100
integer taborder = 50
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Delete Option"
end type

event clicked;	LONG	ll_result

	ll_result = inv_cost_analysis.nf_delete_option_and_tasks(tab_cost_analysis.tabpage_options.dw_options_list)
	IF ll_result = 1 THEN

/*	Call function to do re-retrieveal into master list (dw_cost_analysis_list).
*/
		wf_retrieve_master_list()	
	END IF

end event

type cb_add_option from commandbutton within tabpage_options
integer x = 9
integer y = 1380
integer width = 379
integer height = 100
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Add Option"
end type

event clicked;STRING				ls_filter
LONG					ll_return, ll_returnfilter, ll_returnsetfilter
DATAWINDOWCHILD	ldwc_tasktype

wf_set_state('ADD OPTION')

/* Filter task types dddw so only active task types are displayed (i.e. user can only select
	an active task type when creating a new option).
*/
ls_filter = "active_flag = 'Y'"
ll_return = dw_options_tasks.GetChild('task_type_code', ldwc_tasktype)
IF ll_return > 0 THEN
	ll_returnsetfilter = ldwc_tasktype.SetFilter(ls_filter)
	ll_returnfilter = ldwc_tasktype.Filter()
END IF

end event

type dw_options_list from u_dw_online within tabpage_options
integer x = 23
integer y = 4
integer width = 2574
integer height = 288
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_options_list"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event clicked;call super::clicked;INTEGER	li_cost_no, li_option_no

IF row > 0 THEN
	li_cost_no = THIS.GetItemNumber(row,'cost_no')
	li_option_no = THIS.GetItemNumber(row,'option_no')
	il_clickedrow = row
	inv_cost_analysis.nf_retrieve_option_details(li_cost_no,li_option_no)
ELSE
	IF THIS.RowCount() > 0 THEN
		li_cost_no = THIS.GetItemNumber(1,'cost_no')
		li_option_no = THIS.GetItemNumber(1,'option_no')
		il_clickedrow = 1
		inv_cost_analysis.nf_retrieve_option_details(li_cost_no,li_option_no)
	END IF
END IF

end event

event rowfocuschanged;call super::rowfocuschanged;LONG		ll_row
INTEGER	li_cost_no, li_option_no

ll_row = THIS.GetRow()
IF ll_row = 0 THEN
	RETURN
END IF

uf_processselect(ll_row,"Mouse")
il_clickedrow = ll_row
li_cost_no = THIS.GetItemNumber(ll_row,'cost_no')
li_option_no = THIS.GetItemNumber(ll_row,'option_no')
inv_cost_analysis.nf_retrieve_option_details(li_cost_no,li_option_no)
	
end event

type dw_options_costs from u_dw_online within tabpage_options
integer x = 5
integer y = 312
integer width = 2601
integer height = 560
integer taborder = 20
string dataobject = "d_options_costs"
boolean border = false
end type

event itemchanged;call super::itemchanged;/*	Call function to run the itemchanged code. Also set the value for sle_total_costs.
*/
	IF ib_fire_window_ftn THEN
		wf_disable_after_entry(THIS)
		ib_fire_window_ftn = FALSE
	END IF
	inv_cost_analysis.nf_change_item(2)
	tab_cost_analysis.tabpage_options.sle_total_costs.Text = String(inv_cost_analysis.idec_total_costs,'$###,###.00')
	
end event

event retrieveend;call super::retrieveend;	LONG					ll_capitalization_factor_yyyymm, ll_row
	INTEGER				li_cost_no, li_option_no, li_result, li_saved_age, li_calc_age, li_capitalization_benefit_calc_no
	DATE					ldt_capitalization_date
	DECIMAL				ldec_saved_capitalized_amount, ldec_calc_capitalized_amount, ldec_award_amount
	STRING				ls_message

	IF	rowcount > 0 AND NOT inv_cost_analysis.ib_capitalization_difference_flag THEN
		li_cost_no = THIS.GetItemNumber(1,'cost_no')
		li_option_no = THIS.GetItemNumber(1,'option_no')
		
/*	Determine if the capitalized amount & age have to be recalculated.
*/		
		ldec_saved_capitalized_amount = THIS.GetItemNumber(1,"capitalized_amount")
		li_capitalization_benefit_calc_no = THIS.GetItemNumber(1,"capitalization_benefit_calc_no")
		IF li_capitalization_benefit_calc_no > 0 THEN
			li_saved_age = THIS.GetItemNumber(1,"age")
			li_calc_age = f_calculate_age(Date(idtm_birth_date),Date(THIS.GetItemDateTime(1,"end_date")))
			IF li_calc_age > 64 THEN
				inv_cost_analysis.ib_capitalization_difference_flag = TRUE
				MessageBox("Age Greater Than 64", "Age must be less than or equal to 64 years.~r~n~r~nVerify/Correct Individual Data and Correct all Cost Analysis with Capitalizations", Information!)
				RETURN
			END IF
//old		ldec_award_amount = tab_cost_analysis.tabpage_options.dw_options_list.GetItemDecimal(tab_cost_analysis.tabpage_options.dw_options_list.GetRow(),'award_amount')
			ll_row = il_clickedrow
			ldec_award_amount = tab_cost_analysis.tabpage_options.dw_options_list.GetItemDecimal(il_clickedrow,'award_amount')
			ldt_capitalization_date = Date(THIS.GetItemDateTime(1,"end_date"))
			ll_capitalization_factor_yyyymm = Long(String(ldt_capitalization_date,'YYYYMMDD'))/100
			ldec_calc_capitalized_amount = 0
			li_result = inv_cost_analysis.nf_calc_capitalized_amt(ldec_award_amount,li_calc_age,ll_capitalization_factor_yyyymm,ldec_calc_capitalized_amount)
			IF li_result < 0 THEN
				inv_cost_analysis.ib_capitalization_difference_flag = TRUE
				MessageBox("Capitalized Amount Error","Unable to calculate the capitalized amount.")
				RETURN
			END IF
			IF Round(ldec_calc_capitalized_amount,2) <> Round(ldec_saved_capitalized_amount,2) OR li_calc_age <> li_saved_age THEN
				inv_cost_analysis.ib_capitalization_difference_flag = TRUE
				ls_message = "Cost Analysis: " + String(li_cost_no)
				ls_message = ls_message + "~r~n~r~nAges for Option: " + String(li_option_no)
				ls_message = ls_message + "~r~nOld/New Ages: " + String(li_saved_age) + " / " + String(li_calc_age)
				ls_message = ls_message + "~r~n~r~nCapitalized Amounts for Option: " + String(li_option_no)
				ls_message = ls_message + "~r~nOld/New Amounts: " + String(ldec_saved_capitalized_amount,'$###,###.00') + " / " + String(ldec_calc_capitalized_amount,'$###,###.00')
				ls_message = ls_message + "~r~n~r~nVerify/Correct Individual Data and Correct all Cost Analysis with Capitalizations"
				MessageBox ("Capitalized Amount/Age Different", ls_message, Information!)
				dw_options_costs.SetItem(1,'capitalized_amount',ldec_calc_capitalized_amount)
				dw_options_costs.SetItem(1,'capitalization_factor_yyyymm',ll_capitalization_factor_yyyymm)
				THIS.SetItem(1,'age',li_calc_age)
				RETURN
			END IF
		END IF
	END IF
					
	inv_cost_analysis.ib_capitalization_difference_flag = FALSE
	
end event

event editchanged;call super::editchanged;IF ib_fire_window_ftn THEN
	wf_disable_after_entry(THIS)
	ib_fire_window_ftn = FALSE
END IF

end event

type sle_total_costs from singlelineedit within tabpage_options
integer x = 2135
integer y = 1396
integer width = 453
integer height = 80
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type

type st_1 from statictext within tabpage_options
integer x = 1787
integer y = 1412
integer width = 334
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Total Costs :"
boolean focusrectangle = false
end type

type tabpage_cost_comparison from userobject within tab_cost_analysis
integer x = 18
integer y = 112
integer width = 2601
integer height = 1520
long backcolor = 67108864
string text = "Cost Comparison"
long tabtextcolor = 33554432
string picturename = "Compute!"
long picturemaskcolor = 553648127
dw_compare_options dw_compare_options
cb_printtab3 cb_printtab3
end type

on tabpage_cost_comparison.create
this.dw_compare_options=create dw_compare_options
this.cb_printtab3=create cb_printtab3
this.Control[]={this.dw_compare_options,&
this.cb_printtab3}
end on

on tabpage_cost_comparison.destroy
destroy(this.dw_compare_options)
destroy(this.cb_printtab3)
end on

type dw_compare_options from u_dw_online within tabpage_cost_comparison
integer x = 27
integer y = 16
integer width = 2514
integer height = 1344
integer taborder = 2
string dataobject = "d_compare_options"
boolean hscrollbar = true
boolean vscrollbar = true
end type

type cb_printtab3 from commandbutton within tabpage_cost_comparison
integer x = 9
integer y = 1380
integer width = 379
integer height = 100
integer taborder = 51
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Print"
end type

event clicked;SetPointer(HourGlass!)

IF dw_compare_options.RowCount() > 0 THEN
	dw_compare_options.Object.ltd_calculation.Visible = FALSE
	dw_compare_options.Object.ltd_calculation_print.Visible = TRUE
	dw_compare_options.Print()
ELSE
	MessageBox('No data to report','There is no data available to report on.')
END IF

end event

type tabpage_summary from userobject within tab_cost_analysis
event create ( )
event destroy ( )
integer x = 18
integer y = 112
integer width = 2601
integer height = 1520
long backcolor = 67108864
string text = "Summary Report"
long tabtextcolor = 33554432
string picturename = "PictureDropDownListBox!"
long picturemaskcolor = 553648127
uo_summary_report uo_summary_report
end type

on tabpage_summary.create
this.uo_summary_report=create uo_summary_report
this.Control[]={this.uo_summary_report}
end on

on tabpage_summary.destroy
destroy(this.uo_summary_report)
end on

type uo_summary_report from uo_tabpage_summary_report within tabpage_summary
event destroy ( )
integer width = 2583
integer height = 1532
integer taborder = 2
end type

on uo_summary_report.destroy
call uo_tabpage_summary_report::destroy
end on

