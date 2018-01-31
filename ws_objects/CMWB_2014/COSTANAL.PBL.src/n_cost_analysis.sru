$PBExportHeader$n_cost_analysis.sru
forward
global type n_cost_analysis from n_pdc
end type
end forward

global type n_cost_analysis from n_pdc
end type
global n_cost_analysis n_cost_analysis

type variables
LONG		il_master_list_count
LONG		il_option_list_count
LONG		il_claim_no
BOOLEAN	ib_capitalization_difference_flag
STRING		is_sex
STRING		is_admin_region_code
DATETIME	idtm_birth_date
DATETIME	idtm_accident_date
DECIMAL		idec_total_costs
DECIMAL               idec_authorization_limit
DECIMAL		idec_recom_option_total_costs
BOOLEAN               ib_new_option
end variables

forward prototypes
public function integer nf_check_mandatory ()
public function integer nf_set_unused_fields ()
public function integer nf_get_next_cost_no ()
public function integer nf_insert ()
public function integer nf_insert_option ()
public function integer nf_retrieve_capitalization ()
public function integer nf_retrieve_ltd ()
public function integer nf_retrieve_master_details (integer ai_cost_no)
public function integer nf_retrieve_option_details (integer ai_costno, integer ai_optionno)
public function integer nf_retrieve_rloe ()
public function integer nf_retrieve_option_list (integer ai_cost_no, ref u_dw_online adw_optionlist)
public function integer nf_set_defaults (u_dw_online adw_datawindowtoset)
public function long nf_set_identifiers ()
public function integer nf_change_item (long al_datawindow)
public function integer nf_set_ben_calc_filters (string as_filter_column, integer ai_value)
public subroutine nf_determine_total_costs ()
public function integer nf_item_focus_change_task (long al_row, string as_column_name)
public function integer nf_delete_option_and_tasks (ref u_dw_online adw_option_list)
public function integer nf_get_next_task_no (integer ai_costno, integer ai_optionno)
public function integer nf_get_next_option_no (integer ai_costno)
public function integer nf_set_opening_no (string as_ben_calc_column, string as_opening_column)
public function decimal nf_get_recom_option_total_costs (integer ai_cost_no, integer ai_recommended_option_no)
public subroutine nf_calc_total_benefit_amt (date adt_start_date, date adt_end_date, integer ai_rloe_benefit_calc_no, integer ai_ltd_benefit_calc_no)
public function integer nf_get_capitalization_factor (integer ai_age, ref long al_capitalization_factor_yyyymm, ref decimal adec_capitalization_factor)
public function integer nf_calc_capitalized_amt (decimal adec_award_amount, integer ai_age, ref long al_capitalization_factor_yyyymm, ref decimal adec_capitalized_amount)
public function integer nf_check_bus_rule ()
public subroutine nf_set_instance_variables (long al_claim_no, string as_sex, datetime adtm_birth_date, string as_admin_region_code, boolean ab_capitalization_difference_flag, datetime adtm_accident_date)
end prototypes

public function integer nf_check_mandatory ();/*	This function is used to check and see if all the madatory columns within each
	updateable datawindow is filled in. It first will calculate all the needed new
	cost_no's, option_no's, and task_no's.
*/
	LONG					ll_rowsindw, ll_loopcount, ll_claimno, ll_count
	INTEGER				li_costno, li_optionno, li_taskno, li_result, li_row, li_exist_count
	DATAWINDOWCHILD	ldwc_child
	STRING				ls_type_code, ls_sub_type_code, ls_specific_code
	
	IF idw_dw[1].AcceptText() = -1 THEN RETURN -1
	IF idw_dw[2].AcceptText() = -1 THEN RETURN -1
	IF idw_dw[3].AcceptText() = -1 THEN RETURN -1
	
/*	First check all the columns in idw_dw[1] (the master detail datawindow).
*/
	IF IsNull(idw_dw[1].GetItemNumber(1,'recommended_option_no'))THEN
		MessageBox('Invalid Recommended Option Number','The recommended option number must not be NULL.')
		RETURN -1
	END IF

	IF IsNull(idw_dw[1].GetItemString(1,'rationale')) THEN
		MessageBox('Invalid Rationale','The rationale must not be NULL.')
		RETURN -1
	END IF

	IF IsNull(idw_dw[1].GetItemString(1,'authorized_user_id')) THEN
		MessageBox('Invalid Authorized User ID','The authorized user id must not be NULL.')
		RETURN -1
	END IF


/*	Next, check all the columns in idw_dw[2] (the option detail datawindow).
*/
	IF (NOT IsNull(idw_dw[2].GetItemNumber(1,'capitalization_benefit_calc_no'))) AND idw_dw[2].GetItemNumber(1,'capitalization_benefit_calc_no') > 0 THEN
		IF (IsNull(idw_dw[2].GetItemNumber(1,'age')) OR idw_dw[2].GetItemNumber(1,'age') = 0) THEN
			MessageBox('Invalid Age','The application was unable to set the age for the new option record.')
			RETURN -1
		END IF
	END IF

	IF IsNull(idw_dw[2].GetItemString(1,'option_name')) OR Trim(idw_dw[2].GetItemString(1,'option_name')) = '' THEN
		MessageBox('Invalid Option Name','The option name must be entered for an option record.')
		RETURN -1
	END IF

	IF IsNull(idw_dw[2].GetItemString(1,'option_desc')) OR idw_dw[2].GetItemString(1,'option_desc') = '' THEN
		MessageBox('Invalid Option Description','The option description must be entered for an option record.')
		RETURN -1
	END IF

	IF IsNull(idw_dw[2].GetItemDatetime(1,'start_date')) OR String(idw_dw[2].GetItemDatetime(1,'start_date'),'YYYY-MM-DD') = '1900-01-01' THEN
		MessageBox('Invalid Start Date','The start date must be entered for an option record.')
		RETURN -1
	END IF

	IF IsNull(idw_dw[2].GetItemDatetime(1,'end_date')) OR String(idw_dw[2].GetItemDatetime(1,'end_date'),'YYYY-MM-DD') = '1900-01-01' THEN
		MessageBox('Invalid End Date','The end date must be entered for an option record.')
		RETURN -1
	END IF

/*	Finally, check all the columns in idw_dw[3] (the tasks datawindow).
*/
	ll_loopcount = 1
	ll_rowsindw = idw_dw[3].RowCount()
	IF ll_rowsindw > 0 THEN
		DO WHILE ll_loopcount <= ll_rowsindw
			ls_type_code = idw_dw[3].GetItemString(ll_loopcount,'task_type_code')
			ls_sub_type_code = idw_dw[3].GetItemString(ll_loopcount,'task_sub_type_code')
			ls_specific_code = idw_dw[3].GetItemString(ll_loopcount,'task_specific_code')
			IF IsNull(ls_type_code) OR Trim(ls_type_code) = '' THEN
				MessageBox('Invalid Task Type Code','The type for task record ' + String(ll_loopcount) + ' is required.')
				RETURN -1
			END IF
			IF IsNull(ls_sub_type_code) OR Trim(ls_sub_type_code) = '' THEN
				MessageBox('Invalid Task Sub Type Code','The sub type for task record ' + String(ll_loopcount) + ' is required.')
				RETURN -1
			END IF
			IF IsNull(ls_specific_code) OR Trim(ls_specific_code) = '' THEN
				MessageBox('Invalid Task Specific Code','The task specific code for task record ' + String(ll_loopcount) + ' is required.')
				RETURN -1
			END IF
			
			li_result = idw_dw[3].GetChild('task_specific_code',ldwc_child)
			IF li_result = 1 THEN
				ldwc_child.SetFilter('')
				ldwc_child.Filter()
				li_row = ldwc_child.Find("task_specific_code = '" + idw_dw[3].GetItemString(ll_loopcount,'task_specific_code') + "'",1,ldwc_child.RowCount())
				IF li_row > 0 THEN
					IF ldwc_child.GetItemString(li_row,'comment_required_flag') = 'Y' AND Trim(idw_dw[3].GetItemString(ll_loopcount,'comment')) = '' THEN
						MessageBox('Invalid Comment','The comment for task record ' + String(ll_loopcount) + ' is required.')
						RETURN -1
					END IF
				ELSE
					MessageBox('Comment Error','Unable to determine if the comment is required for task number ' + String(ll_loopcount) + '.')
					RETURN -1
				END IF
			ELSE
				MessageBox('Comment Error','Unable to determine if the comment is required for task number ' + String(ll_loopcount) + '.')
				RETURN -1
			END IF
			
			SELECT count(*)
			  INTO :li_exist_count
			  FROM Cost_Analysis_Option_Task_Specific
			 WHERE task_type_code = :ls_type_code
			   AND task_sub_type_code = :ls_sub_type_code
				AND task_specific_code = :ls_specific_code
			 USING SQLCA;
			
			IF SQLCA.nf_handle_error("Embedded SQL: SELECT count(*) INTO :li_exist_count","n_cost_analysis","nf_check_mandatory()") < 0 THEN
				RETURN -1
			END IF

			IF li_exist_count = 0 THEN
				MessageBox('Selection Error','The selected task type / task sub type / task specific combination in invalid.')
				RETURN -1
			END IF
			ll_loopcount ++
		LOOP
	END IF

	RETURN 0

	
end function

public function integer nf_set_unused_fields ();/*	This function is used to set the defaults on any of the columns where no value
	has been entered by the user. This is done for all three of the updateable 
	datawindows.
*/
	LONG	ll_row, ll_loopcount, ll_rowsindw
	DATAWINDOWCHILD	ldwc_child

	ll_row = idw_dw[1].GetRow()

	IF ll_row > 0 THEN
		IF IsNull(idw_dw[1].GetItemNumber(ll_row,'recommended_option_no')) THEN
			idw_dw[1].SetItem(ll_row,'recommended_option_no',0)
		END IF
		IF IsNull(idw_dw[1].GetItemString(ll_row,'rationale')) THEN
			idw_dw[1].SetItem(ll_row,'rationale',' ')
		END IF
		IF IsNull(idw_dw[1].GetItemString(ll_row,'authorized_user_id')) THEN
			idw_dw[1].SetItem(ll_row,'authorized_user_id',' ')
		END IF
	END IF

	ll_row = idw_dw[2].GetRow()

	IF ll_row > 0 AND idw_dw[2].Enabled = TRUE THEN
		IF IsNull(idw_dw[2].GetItemNumber(ll_row,'capitalization_benefit_calc_no')) THEN
			idw_dw[2].SetItem(ll_row,'capitalization_benefit_calc_no',0)
		END IF
		IF IsNull(idw_dw[2].GetItemNumber(ll_row,'ltd_benefit_calc_no')) THEN
			idw_dw[2].SetItem(ll_row,'ltd_benefit_calc_no',0)
		END IF
		IF IsNull(idw_dw[2].GetItemNumber(ll_row,'rloe_benefit_calc_no')) THEN
			idw_dw[2].SetItem(ll_row,'rloe_benefit_calc_no',0)
		END IF
		IF IsNull(idw_dw[2].GetItemDecimal(ll_row,'capitalized_amount')) THEN
			idw_dw[2].SetItem(ll_row,'capitalized_amount',0)
		END IF
		IF IsNull(idw_dw[2].GetItemDecimal(ll_row,'total_benefit_amount')) THEN
			idw_dw[2].SetItem(ll_row,'total_benefit_amount',0)
		END IF
	END IF

	ll_rowsindw = idw_dw[3].RowCount()
	ll_loopcount = 1
	DO WHILE ll_loopcount <= ll_rowsindw AND idw_dw[3].Enabled = TRUE
		IF IsNull(idw_dw[3].GetItemString(ll_loopcount,'task_specific_code')) THEN
			idw_dw[3].SetItem(ll_loopcount,'task_specific_code', ' ')
		END IF
		IF IsNull(idw_dw[3].GetItemString(ll_loopcount,'comment')) THEN
			idw_dw[3].SetItem(ll_loopcount,'comment', ' ')
		END IF
		IF IsNull(idw_dw[3].GetItemDecimal(ll_loopcount,'task_amount')) THEN
			idw_dw[3].SetItem(ll_loopcount,'task_amount',0)
		END IF
		ll_loopcount ++
	LOOP

	RETURN 0  
	
end function

public function integer nf_get_next_cost_no ();/* This function is used to determine the next cost number for a claim. It does it by
	determining the current maximum cost number and then adds 1 to it. This function
	should only be called once per update.
*/
	INTEGER	li_nextcostno

	SELECT Max(cost_no)
	  INTO :li_nextcostno
	  FROM COST_ANALYSIS_MASTER
	 WHERE claim_no = :il_claim_no
	 USING SQLCA;
	
	IF SQLCA.nf_handle_error("Embedded SQL: SELECT Max(cost_no)","n_cost_analysis","nf_get_next_cost_no().") < 0 THEN
		RETURN -1
	END IF

	IF IsNull(li_nextcostno) THEN
		li_nextcostno = 1
	ELSE
		li_nextcostno ++
	END IF

	RETURN li_nextcostno

end function

public function integer nf_insert ();LONG  				ll_row
DATAWINDOWCHILD 	ldwc_child

	idw_dw[1].Reset()

	ll_row = idw_dw[1].InsertRow(0)
	

	IF ll_row < 0 THEN Return -1

	idw_dw[1].GetChild('recommended_option_no',ldwc_child)
	ldwc_child.Reset()

	IF nf_set_defaults(idw_dw[1]) < 0 THEN Return -1
	
Return ll_row 
end function

public function integer nf_insert_option ();/*	This function is called when a COST_ANALYSIS_OPTION record needs to be added. It will 
	insert a record into dw_options_costs and a record into dw_options_tasks.
*/
	INTEGER	li_result, li_row
	DATAWINDOWCHILD	ldwc_child

	idw_dw[2].Reset()
	idw_dw[3].Reset()

/*	Retrieve the three drop down datawindows for idw_dw[2].
*/
	li_result = nf_retrieve_capitalization()
	IF li_result < 0 THEN
		RETURN -1
	END IF
	
	li_result = nf_retrieve_rloe()
	IF li_result < 0 THEN
		RETURN -1
	END IF

	li_result = nf_retrieve_ltd()
	IF li_result < 0 THEN
		RETURN -1
	END IF

/*	Into dw_options_costs and set the defaults.
*/
	li_row = idw_dw[2].InsertRow(0)
	IF li_row < 0 THEN RETURN -1

	IF nf_set_defaults(idw_dw[2]) < 0 THEN RETURN -1
	
	nf_set_opening_no('rloe_benefit_calc_no','rloe_opening')
	nf_set_opening_no('ltd_benefit_calc_no','ltd_opening')
	nf_set_opening_no('capitalization_benefit_calc_no','capitalization_opening')

/*	Into dw_options_tasks and set the defaults.
*/
	li_row = idw_dw[3].InsertRow(0)
	IF li_row < 0 THEN RETURN -1
	
	IF nf_set_defaults(idw_dw[3]) < 0 THEN RETURN -1
	
	RETURN li_row
	
end function

public function integer nf_retrieve_capitalization ();/*	This function is used to fill the drop down datawindows for the columns
	'capitalization_opening' and 'capitalization_benefit_calc_no' on idw_dw[2].
*/
	DATAWINDOWCHILD	ldwc_child

	idw_dw[2].GetChild('capitalization_opening',ldwc_child)
	ldwc_child.SetTransObject(SQLCA)
	ldwc_child.Retrieve(il_claim_no)

	IF SQLCA.nf_handle_error("idw_dw[2].ldwc_child.Retrieve(il_claim_no)","n_cost_analysis","nf_retrieve_capitalization() - 'capitalization_opening'") < 0 THEN
		RETURN -1
	END IF

	ldwc_child.InsertRow(1)
	ldwc_child.SetItem(1,'opening_no',0)
	
	idw_dw[2].GetChild('capitalization_benefit_calc_no',ldwc_child)
	ldwc_child.SetTransObject(SQLCA)
	ldwc_child.Retrieve(il_claim_no)

	IF SQLCA.nf_handle_error("idw_dw[2].ldwc_child.Retrieve(il_claim_no)","n_cost_analysis","nf_retrieve_capitalization() - 'capitalization'") < 0 THEN
		RETURN -1
	END IF

	ldwc_child.InsertRow(1)
	ldwc_child.SetItem(1,'benefit_calculation_no',0)
	ldwc_child.SetItem(1,'opening_no',0)
	
	RETURN 0

end function

public function integer nf_retrieve_ltd ();/*	This function is used to fill the drop down datawindows for the columns 'ltd_opening'
	and 'ltd_benefit_calc_no' on idw_dw[2].
*/
	DATAWINDOWCHILD	ldwc_child

	idw_dw[2].GetChild('ltd_opening',ldwc_child)
	ldwc_child.SetTransObject(SQLCA)
	ldwc_child.Retrieve(il_claim_no)

	IF SQLCA.nf_handle_error("idw_dw[2].ldwc_child.Retrieve(il_claim_no)","n_cost_analysis","nf_retrieve_ltd() - 'ltd opening'") < 0 THEN
		RETURN -1
	END IF
	
	ldwc_child.InsertRow(1)
	ldwc_child.SetItem(1,'opening_no',0)
	
	idw_dw[2].GetChild('ltd_benefit_calc_no',ldwc_child)
	ldwc_child.SetTransObject(SQLCA)
	ldwc_child.Retrieve(il_claim_no)

	IF SQLCA.nf_handle_error("idw_dw[2].ldwc_child.Retrieve(il_claim_no)","n_cost_analysis","nf_retrieve_ltd() - 'ltd'") < 0 THEN
		RETURN -1
	END IF
	
	ldwc_child.InsertRow(1)
	ldwc_child.SetItem(1,'benefit_calculation_no',0)
	ldwc_child.SetItem(1,'opening_no',0)
	
	RETURN 0

end function

public function integer nf_retrieve_master_details (integer ai_cost_no);// nf_retrieve_master_details - retrieves info into the master details datawindow.
//
Long     ll_rowsindw
Integer  li_rtn 
Datetime ldt_authorized_date 
String   ls_authorized_user_id

ll_rowsindw = idw_dw[1].Retrieve(il_claim_no, ai_cost_no)

li_rtn = SQLCA.nf_handle_error("n_cost_analysis", "", "nf_retrieve_master_details - idw_dw[1].Retrieve(il_claim_no, ai_cost_no)")
IF li_rtn < 0 THEN
	RETURN -1
END IF

IF ll_rowsindw > 0 THEN
	ldt_authorized_date = idw_dw[1].GetItemDatetime(ll_rowsindw, "authorized_date") 
	ls_authorized_user_id = idw_dw[1].GetItemString(ll_rowsindw, "authorized_user_id") 
	IF IsNull(ls_authorized_user_id) = TRUE THEN ls_authorized_user_id = ""
	
	IF IsNull(ldt_authorized_date) = FALSE OR ls_authorized_user_id <> "" THEN
		idw_dw[1].Modify("recommended_option_no.protect=1  recommended_option_no.Background.Color='67108864'  recommended_option_no.border='0'  recommended_option_no.dddw.UseAsBorder=No")
		idw_dw[1].Modify("rationale.protect=1              rationale.Background.Color='67108864'              rationale.border='0'")
	ELSE
		idw_dw[1].Modify("recommended_option_no.protect=0  recommended_option_no.Background.Color='16777215'  recommended_option_no.border='5'  recommended_option_no.dddw.UseAsBorder=Yes")
		idw_dw[1].Modify("rationale.protect=0              rationale.Background.Color='16777215'              rationale.border='5'")
	END IF 
END IF

RETURN ll_rowsindw

end function

public function integer nf_retrieve_option_details (integer ai_costno, integer ai_optionno);// nf_retrieve_option_details - This function is used to retrieve into the option details datawindow.
//
// Arguments: ai_costno 
//            ai_optionno 
//
Integer  li_rtn, li_recommended_option
Long     ll_rowsindw, li_row 
Datetime ldt_authorized_date 
String   ls_authorized_user_id 
DataWindowChild ldwc_child 

li_rtn = nf_retrieve_capitalization()
IF li_rtn < 0 THEN
	RETURN -1
END IF

li_rtn = nf_retrieve_rloe()
IF li_rtn < 0 THEN
	RETURN -1
END IF

li_rtn = nf_retrieve_ltd()
IF li_rtn < 0 THEN
	RETURN -1
END IF

// idw_dw[2] is dw_options_costs 
ll_rowsindw = idw_dw[2].Retrieve(il_claim_no, ai_costno, ai_optionno)
li_rtn = SQLCA.nf_handle_error("idw_dw[2].Retrieve(il_claim_no,ai_costno,ai_optionno)","n_cost_analysis","nf_retrieve_option_details()")
IF li_rtn < 0 THEN
	RETURN -1
END IF

IF ll_rowsindw = 0 THEN
	RETURN -1
END IF

nf_set_opening_no('rloe_benefit_calc_no', 'rloe_opening')
nf_set_opening_no('ltd_benefit_calc_no', 'ltd_opening')
nf_set_opening_no('capitalization_benefit_calc_no', 'capitalization_opening')

// idw_dw[3] is dw_options_tasks
ll_rowsindw = idw_dw[3].Retrieve(il_claim_no, ai_costno, ai_optionno)
li_rtn = SQLCA.nf_handle_error("idw_dw[3].Retrieve(il_claim_no,ai_costno,ai_optionno)", "n_cost_analysis", "nf_retrieve_option()")
IF li_rtn < 0 THEN
	RETURN -1
END IF

IF ll_rowsindw = 0 THEN
	// If no records were retrieved then enter a blank record.
	li_row = idw_dw[3].InsertRow(0)
	IF li_row < 0 THEN RETURN -1

	IF nf_set_defaults(idw_dw[3]) < 0 THEN RETURN -1
END IF

// BR 2.4 The Recommend Option cannot be modified if the cost analysis is authorized
ls_authorized_user_id = idw_dw[1].GetItemString(1, 'authorized_user_id')
ldt_authorized_date = idw_dw[1].GetItemDatetime(1, 'authorized_date')
li_recommended_option = idw_dw[1].GetItemNumber(1,'recommended_option_no')
IF ls_authorized_user_id <> '' AND IsNull(ls_authorized_user_id) = FALSE AND IsNull(ldt_authorized_date) = FALSE THEN
		idw_dw[2].Modify("option_name.protect=1                    option_name.Background.Color='67108864'                    option_name.border='0'")
		idw_dw[2].Modify("option_desc.protect=1                    option_desc.Background.Color='67108864'                    option_desc.border='0'")
		idw_dw[2].Modify("start_date.protect=1                     start_date.Background.Color='67108864'                     start_date.border='0'")
		idw_dw[2].Modify("end_date.protect=1                       end_date.Background.Color='67108864'                       end_date.border='0'")
		idw_dw[2].Modify("rloe_opening.protect=1                   rloe_opening.Background.Color='67108864'                   rloe_opening.border='0'                   rloe_opening.dddw.UseAsBorder=No")
		idw_dw[2].Modify("rloe_benefit_calc_no.protect=1           rloe_benefit_calc_no.Background.Color='67108864'           rloe_benefit_calc_no.border='0'           rloe_benefit_calc_no.dddw.UseAsBorder=No")
		idw_dw[2].Modify("ltd_opening.protect=1                    ltd_opening.Background.Color='67108864'                    ltd_opening.border='0'                    ltd_opening.dddw.UseAsBorder=No")
		idw_dw[2].Modify("ltd_benefit_calc_no.protect=1            ltd_benefit_calc_no.Background.Color='67108864'            ltd_benefit_calc_no.border='0'            ltd_benefit_calc_no.dddw.UseAsBorder=No")
		idw_dw[2].Modify("capitalization_opening.protect=1         capitalization_opening.Background.Color='67108864'         capitalization_opening.border='0'         capitalization_opening.dddw.UseAsBorder=No")
		idw_dw[2].Modify("capitalization_benefit_calc_no.protect=1 capitalization_benefit_calc_no.Background.Color='67108864' capitalization_benefit_calc_no.border='0' capitalization_benefit_calc_no.dddw.UseAsBorder=No")

		idw_dw[3].Modify("task_type_code.protect=1                 task_type_code.Background.Color='67108864'                 task_type_code.border='0'                 task_type_code.dddw.UseAsBorder=No")
		idw_dw[3].Modify("task_sub_type_code.protect=1             task_sub_type_code.Background.Color='67108864'             task_sub_type_code.border='0'             task_sub_type_code.dddw.UseAsBorder=No")
		idw_dw[3].Modify("task_specific_code.protect=1             task_specific_code.Background.Color='67108864'             task_specific_code.border='0'             task_specific_code.dddw.UseAsBorder=No")
		idw_dw[3].Modify("task_amount.protect=1                    task_amount.Background.Color='67108864'                    task_amount.border='0'")
		idw_dw[3].Modify("comment.protect=1                        comment.Background.Color='67108864'                        comment.border='0'")
		idw_dw[3].Modify("delete_flag.protect=1                    delete_flag.Background.Color='67108864'                    delete_flag.border='0'")
	ELSE
		idw_dw[2].Modify("option_name.protect=0                    option_name.Background.Color='16777215'                    option_name.border='5'")
		idw_dw[2].Modify("option_desc.protect=0                    option_desc.Background.Color='16777215'                    option_desc.border='5'")
		idw_dw[2].Modify("start_date.protect=0                     start_date.Background.Color='16777215'                     start_date.border='5'")
		idw_dw[2].Modify("end_date.protect=0                       end_date.Background.Color='16777215'                       end_date.border='5'")
		idw_dw[2].Modify("rloe_opening.protect=0                   rloe_opening.Background.Color='16777215'                   rloe_opening.border='5'                   rloe_opening.dddw.UseAsBorder=Yes")
		idw_dw[2].Modify("rloe_benefit_calc_no.protect=0           rloe_benefit_calc_no.Background.Color='16777215'           rloe_benefit_calc_no.border='5'           rloe_benefit_calc_no.dddw.UseAsBorder=Yes")
		idw_dw[2].Modify("ltd_opening.protect=0                    ltd_opening.Background.Color='16777215'                    ltd_opening.border='5'                    ltd_opening.dddw.UseAsBorder=Yes")
		idw_dw[2].Modify("ltd_benefit_calc_no.protect=0            ltd_benefit_calc_no.Background.Color='16777215'            ltd_benefit_calc_no.border='5'            ltd_benefit_calc_no.dddw.UseAsBorder=Yes")
		idw_dw[2].Modify("capitalization_opening.protect=0         capitalization_opening.Background.Color='16777215'         capitalization_opening.border='5'         capitalization_opening.dddw.UseAsBorder=Yes")
		idw_dw[2].Modify("capitalization_benefit_calc_no.protect=0 capitalization_benefit_calc_no.Background.Color='16777215' capitalization_benefit_calc_no.border='5' capitalization_benefit_calc_no.dddw.UseAsBorder=Yes")

		idw_dw[3].Modify("task_type_code.protect=0                 task_type_code.Background.Color='16777215'                 task_type_code.border='5'                 task_type_code.dddw.UseAsBorder=Yes")
		idw_dw[3].Modify("task_sub_type_code.protect=0             task_sub_type_code.Background.Color='16777215'             task_sub_type_code.border='5'             task_sub_type_code.dddw.UseAsBorder=Yes")
		idw_dw[3].Modify("task_specific_code.protect=0             task_specific_code.Background.Color='16777215'             task_specific_code.border='5'             task_specific_code.dddw.UseAsBorder=Yes")
		idw_dw[3].Modify("task_amount.protect=0                    task_amount.Background.Color='16777215'                    task_amount.border='5'")
		idw_dw[3].Modify("comment.protect=0                        comment.Background.Color='16777215'                        comment.border='5'")
		idw_dw[3].Modify("delete_flag.protect=0                    delete_flag.Background.Color='16777215'                    delete_flag.border='5'")
END IF

RETURN ll_rowsindw
end function

public function integer nf_retrieve_rloe ();/*	This function is used to fill the drop down datawindows for the columns 'rloe_opening'
	and 'rloe_benefit_calc_no' on idw_dw[2].
*/
	DATAWINDOWCHILD	ldwc_child

	idw_dw[2].GetChild('rloe_opening',ldwc_child)
	ldwc_child.SetTransObject(SQLCA)
	ldwc_child.Retrieve(il_claim_no)

	IF SQLCA.nf_handle_error("idw_dw[2].ldwc_child.Retrieve(il_claim_no)","n_cost_analysis","nf_retrieve_rloe() - 'rloe opening'") < 0 THEN
		RETURN -1
	END IF

	ldwc_child.InsertRow(1)
	ldwc_child.SetItem(1,'opening_no',0)

	idw_dw[2].GetChild('rloe_benefit_calc_no',ldwc_child)
	ldwc_child.SetTransObject(SQLCA)
	ldwc_child.Retrieve(il_claim_no)

	IF SQLCA.nf_handle_error("idw_dw[2].ldwc_child.Retrieve(il_claim_no)","n_cost_analysis","nf_retrieve_rloe() - 'rloe'") < 0 THEN
		RETURN -1
	END IF

	ldwc_child.InsertRow(1)
	ldwc_child.SetItem(1,'benefit_calculation_no',0)
	ldwc_child.SetItem(1,'opening_no',0)

	RETURN 0

end function

public function integer nf_retrieve_option_list (integer ai_cost_no, ref u_dw_online adw_optionlist);/*	This function is used to retrieve into the option details datawindow.

	Argument:	ai_cost_no - 
					adw_optionlist - 
*/
	LONG ll_rowsindw
	
	ll_rowsindw = adw_optionlist.Retrieve(il_claim_no,ai_cost_no)

	IF SQLCA.nf_handle_error("adw_optionlist.Retrieve(il_claim_no,ai_cost_no)","n_cost_analysis","nf_retrieve_option_list()") < 0 THEN
		RETURN -1
	END IF

	IF ll_rowsindw > 0 THEN 
		adw_optionlist.SetRow(1)
		adw_optionlist.uf_processselect(1,'KeyBoard')
	END IF

	RETURN ll_rowsindw


end function

public function integer nf_set_defaults (u_dw_online adw_datawindowtoset);/*	This function is used to set the defaults for a new record for each of the three
	updateable datawindows. For the datawindow idw_dw[3], because there will probably
	be more than 1 new record, the function nf_check_madatory() will set the defaults
	for all records greater than 1.
*/
	DATETIME	ldtm_null_date
	LONG		ll_rowsindw
	
	SetNull(ldtm_null_date)
	CHOOSE CASE adw_datawindowtoset
		CASE idw_dw[1]
			adw_datawindowtoset.Setitem(1,'recommended_option_no',0)
			adw_datawindowtoset.Setitem(1,'rationale','')
			adw_datawindowtoset.Setitem(1,'authorized_date',ldtm_null_date)
			adw_datawindowtoset.Setitem(1,'authorized_user_id','')
			
		CASE idw_dw[2]
			adw_datawindowtoset.Setitem(1,'capitalization_benefit_calc_no',0)
			adw_datawindowtoset.Setitem(1,'capitalized_amount',0)
			adw_datawindowtoset.Setitem(1,'option_no',0)
			adw_datawindowtoset.Setitem(1,'age',0)
			adw_datawindowtoset.Setitem(1,'option_name','')
			adw_datawindowtoset.Setitem(1,'option_desc','')
			adw_datawindowtoset.Setitem(1,'start_date',ldtm_null_date)
			adw_datawindowtoset.Setitem(1,'end_date',ldtm_null_date)
			adw_datawindowtoset.Setitem(1,'capitalization_factor_yyyymm',0)
			adw_datawindowtoset.Setitem(1,'rloe_benefit_calc_no',0)
			adw_datawindowtoset.Setitem(1,'ltd_benefit_calc_no',0)
			adw_datawindowtoset.Setitem(1,'total_benefit_amount',0)

		CASE idw_dw[3]
			ll_rowsindw = adw_datawindowtoset.RowCount()
			adw_datawindowtoset.Setitem(ll_rowsindw,'task_type_code','')
			adw_datawindowtoset.Setitem(ll_rowsindw,'task_sub_type_code','')
			adw_datawindowtoset.Setitem(ll_rowsindw,'task_specific_code','.')
			adw_datawindowtoset.Setitem(ll_rowsindw,'comment','')
			adw_datawindowtoset.Setitem(ll_rowsindw,'task_amount',0)

	END CHOOSE

	RETURN 0

end function

public function long nf_set_identifiers ();/*	This function is used to set all the identifiers for the three updateable datawindows.
*/
	LONG		ll_rowsindw, ll_loopcount, ll_claimno, ll_count
	INTEGER	li_costno, li_optionno, li_taskno
	
/*	Call function to determine next cost number if the COST_ANALYSIS_MASTER record is new.
*/
	IF	idw_dw[1].RowCount() > 0 THEN
		IF idw_dw[1].GetItemNumber(1,'cost_no') = 0 OR IsNull(idw_dw[1].GetItemNumber(1,'cost_no')) THEN
			li_costno = nf_get_next_cost_no()
			IF li_costno < 0 THEN
				RETURN -1
			ELSE
				idw_dw[1].SetItem(1,'cost_no',li_costno)
				idw_dw[1].Setitem(1,'claim_no',il_claim_no)
			END IF
		ELSE
			li_costno = idw_dw[1].GetItemNumber(1,'cost_no')
		END IF
	END IF

/*	Call function to determine next option number if the COST_ANALYSIS_OPTION record is new.
*/
	IF	idw_dw[2].RowCount() > 0 THEN
		IF idw_dw[2].GetItemNumber(1,'option_no') = 0 OR IsNull(idw_dw[2].GetItemNumber(1,'option_no')) THEN
			li_optionno = nf_get_next_option_no(li_costno)
			IF li_optionno < 0 THEN
				RETURN -1
			ELSE
				idw_dw[2].SetItem(1,'option_no',li_optionno)
				
/*	Also set the cost number for the new record.
*/
				idw_dw[2].SetItem(1,'cost_no',li_costno)
				idw_dw[2].Setitem(1,'claim_no',il_claim_no)
			END IF
		ELSE
			li_optionno = idw_dw[2].GetItemNumber(1,'option_no')
		END IF
	END IF

/*	Call function to determine next task number if the COST_ANALYSIS_OPTION_TASK record(s) is new.
*/
	ll_rowsindw = idw_dw[3].RowCount()
	IF	ll_rowsindw > 0 THEN
		ll_count = 1
		DO WHILE ll_count <= ll_rowsindw
			IF idw_dw[3].GetItemNumber(ll_count,'task_no') = 0 OR IsNull(idw_dw[3].GetItemNumber(ll_count,'task_no')) THEN
				li_taskno = nf_get_next_task_no(li_costno,li_optionno)
				IF li_optionno < 0 THEN
					RETURN -1
				ELSE
					idw_dw[3].SetItem(ll_count,'task_no',li_taskno)
				
/*	Also set the cost number and option number for the new record.
*/
					idw_dw[3].SetItem(ll_count,'claim_no',il_claim_no)
					idw_dw[3].SetItem(ll_count,'cost_no',li_costno)
					idw_dw[3].SetItem(ll_count,'option_no',li_optionno)
					ll_count ++
					EXIT
				END IF
			END IF
			ll_count ++
		LOOP
		DO WHILE ll_count <= ll_rowsindw
			li_taskno ++
			idw_dw[3].SetItem(ll_count,'task_no',li_taskno)
			idw_dw[3].SetItem(ll_count,'cost_no',li_costno)
			idw_dw[3].SetItem(ll_count,'option_no',li_optionno)
			idw_dw[3].SetItem(ll_count,'claim_no',il_claim_no)
			ll_count ++
		LOOP
	END IF

	RETURN 0

end function

public function integer nf_change_item (long al_datawindow);/*	This function runs the code for the itemchanged event for the appropriate datawindow.
*/
	LONG					ll_current_row, ll_child_row, ll_capitalization_factor_yyyymm, ll_row
	STRING				ls_tasktype, ls_tasksubtype, ls_filter
	DATAWINDOWCHILD	ldwc_child
	INTEGER				li_age, li_result, li_capitalization_benefit_calc_no
	DATE					ldt_capitalization_date
	DATETIME				ldtm_effective_date, ldtm_date
	DECIMAL				ldec_award_amount, ldec_calc_capitalized_amount
		  
	ll_current_row = idw_dw[al_datawindow].GetRow()
	CHOOSE CASE	al_datawindow
		CASE	1

/*	This is for the datawindow idw_dw[1] - which is the cost analysis master.
*/
	  	 	CHOOSE CASE idw_dw[al_datawindow].GetColumnName()				
				CASE	'authorize_flag'
					IF idw_dw[1].GetText() ='Y' AND  &
						(idw_dw[1].GetItemNumber(1,"recommended_option_no") = 0 OR IsNull(idw_dw[1].GetItemNumber(1,"recommended_option_no")) OR &
						Trim(idw_dw[1].GetItemString(1,"rationale")) = '') THEN
						MessageBox("Authorization Error","Must choose both Recommended Option and Rationale for Authorization", Information!)
						RETURN 2
					END IF
				CASE	''
				
			END CHOOSE

		CASE	2
/*	This is for the datawindow idw_dw[2] - which is the options details.
*/
   	 	CHOOSE CASE idw_dw[al_datawindow].GetColumnName()				
				CASE	'start_date'	
					ldtm_date = Datetime(Date(Left(idw_dw[2].GetText(),10)))
					IF ldtm_date < idtm_accident_date THEN
						MessageBox("Invalid Start Date","The start date for an option must be equal to or greater than the accident date for the claim.")
						RETURN 1					
					END IF
					nf_calc_total_benefit_amt(Date(ldtm_date),Date(idw_dw[2].GetItemDatetime(ll_current_row,'end_date')),idw_dw[2].GetItemNumber(ll_current_row,'rloe_benefit_calc_no'),idw_dw[2].GetItemNumber(ll_current_row,'ltd_benefit_calc_no'))

				CASE	'end_date'
					ldtm_date = Datetime(Date(Left(idw_dw[2].GetText(),10)))
					IF idw_dw[2].GetItemDatetime(ll_current_row,'start_date') > ldtm_date THEN
						MessageBox("Invalid End Date","The end date for an option must be equal to or greater than the start date for an option.")
						RETURN 1					
					END IF
					nf_calc_total_benefit_amt(Date(idw_dw[2].GetItemDatetime(ll_current_row,'start_date')),Date(ldtm_date),idw_dw[2].GetItemNumber(ll_current_row,'rloe_benefit_calc_no'),idw_dw[2].GetItemNumber(ll_current_row,'ltd_benefit_calc_no'))
					li_capitalization_benefit_calc_no = idw_dw[2].GetItemNumber(ll_current_row,'capitalization_benefit_calc_no')
					IF li_capitalization_benefit_calc_no > 0 THEN

/*	Individual's sex and birth date are required.
*/
						IF is_sex = 'U' OR IsNull(idtm_birth_date) THEN
							MessageBox("Cost Analysis Module Error","Cannot calculate capitalization amount as either the individual's sex and/or birth date is unknown. Please close module and fix individual data.")
							RETURN -1
						END IF
						ldt_capitalization_date = Date(ldtm_date)
						li_age = f_calculate_age(Date(idtm_birth_date),ldt_capitalization_date)
						IF li_age < 0 THEN
							MessageBox("Age Error", "Couldn't determine age - is less than 0", Information!)
							RETURN 1
						END IF
						IF IsNull(li_age) OR li_age = 0 THEN
							MessageBox("Age Error","Couldn't determine age - is null or 0", Information!)
							RETURN 1
						END IF
						IF li_age > 64 THEN
							ib_capitalization_difference_flag = TRUE
							MessageBox("Age Greater Than 64", "Age must be less than or equal to 64 years.~r~n~r~nVerify/Correct Individual Data and Correct all Cost Analysis with Capitalizations", Information!)
							RETURN 1
						END IF
						idw_dw[2].SetItem(ll_current_row,'age',li_age)
						li_result = idw_dw[2].GetChild('capitalization_benefit_calc_no',ldwc_child)
						IF li_result <> 1 THEN
							MessageBox("Capitalization Benefit Calc Number Error","Unable to determine the benefit calc award amount.")
							RETURN 1
						END IF
						ll_child_row = ldwc_child.Find("benefit_calculation_no = " + String(li_capitalization_benefit_calc_no),1,ldwc_child.RowCount())
						IF ll_child_row <= 0 THEN
							MessageBox("Capitalization Benefit Calc Number Error","Unable to determine the benefit calc award amount.")
							RETURN 1
						END IF
						ldec_award_amount = ldwc_child.GetItemDecimal(ll_child_row,'award_amount')
						ll_capitalization_factor_yyyymm = Long(String(ldt_capitalization_date,'YYYYMMDD'))/100
						ldec_calc_capitalized_amount = 0
						li_result = nf_calc_capitalized_amt(ldec_award_amount,li_age,ll_capitalization_factor_yyyymm,ldec_calc_capitalized_amount)
						IF li_result < 0 THEN
							RETURN 1
						ELSE
							idw_dw[2].SetItem(ll_current_row,'capitalized_amount',ldec_calc_capitalized_amount)
							idw_dw[2].SetItem(ll_current_row,'capitalization_factor_yyyymm',ll_capitalization_factor_yyyymm)
						END IF
					END IF
		
				CASE	'capitalization_benefit_calc_no'
					IF Integer(idw_dw[2].GetText()) > 0 THEN

/*	Individual's sex and birth date are required.
*/
						IF is_sex = 'U' OR IsNull(idtm_birth_date) THEN
							MessageBox("Cost Analysis Module Error","Cannot calculate capitalization amount as either the individual's sex and/or birth date is unknown. Please close module and fix individual data.")
							RETURN -1
						END IF
						li_result = idw_dw[2].GetChild('capitalization_benefit_calc_no',ldwc_child)
						IF li_result <> 1 THEN
							MessageBox("Capitalization Benefit Calc Number Error","Unable to determine the benefit calc selected.")
							RETURN 1
						END IF
						ll_child_row = ldwc_child.GetRow()
						IF ll_child_row <= 0 THEN
							MessageBox("Capitalization Benefit Calc Number Error","Unable to determine the benefit calc selected.")
							RETURN 1
						END IF
						ldec_award_amount = ldwc_child.GetItemDecimal(ll_child_row,'award_amount')
						ldtm_effective_date = ldwc_child.GetItemDatetime(ll_child_row,'effective_from_date')
						ldt_capitalization_date = Date(idw_dw[2].GetItemDatetime(ll_current_row,'end_date'))					
						IF IsNull(ldt_capitalization_date) THEN
							MessageBox("Invalid End Date", "The end date for an option must be entered.", Information!)
							RETURN 2
						END IF
						IF ldtm_effective_date > idw_dw[2].GetItemDatetime(ll_current_row,'end_date') THEN
							MessageBox("Invalid Capitalization Bene Calc Number", "The effective date for the chosen CAPITALIZATION benefit calc number cannot be after the end date.", Information!)
							RETURN 2
						END IF
						li_age = f_calculate_age(Date(idtm_birth_date),ldt_capitalization_date)
						IF IsNull(li_age) OR li_age <= 0 THEN
							MessageBox("Age Error", "Couldn't determine age", Information!)
							RETURN 1
						END IF
						IF li_age > 64 THEN
							ib_capitalization_difference_flag = TRUE
							MessageBox("Age Greater Than 64", "Age must be less than or equal to 64 years.~r~n~r~nVerify/Correct Individual Data and Correct all Cost Analysis with Capitalizations", Information!)
							RETURN 1
						END IF
						ll_capitalization_factor_yyyymm = Long(String(ldt_capitalization_date,'YYYYMMDD'))/100
						ldec_calc_capitalized_amount = 0
						li_result = nf_calc_capitalized_amt(ldec_award_amount,li_age,ll_capitalization_factor_yyyymm,ldec_calc_capitalized_amount)
						IF li_result < 0 THEN
							MessageBox("Capitalized Amount Error","Unable to calculate the capitalized amount.")
							RETURN 1
						ELSE
							idw_dw[2].SetItem(ll_current_row,'capitalized_amount',ldec_calc_capitalized_amount)
							idw_dw[2].SetItem(ll_current_row,'capitalization_factor_yyyymm',ll_capitalization_factor_yyyymm)
							idw_dw[2].SetItem(ll_current_row,'age',li_age)
						END IF
					ELSE
						idw_dw[2].SetItem(ll_current_row,'capitalized_amount',0.0)
						idw_dw[2].SetItem(ll_current_row,'capitalization_factor_yyyymm',0)
						idw_dw[2].SetItem(ll_current_row,'age',0)
					END IF
	
				CASE	'ltd_benefit_calc_no'
					li_result = idw_dw[2].GetChild('ltd_benefit_calc_no',ldwc_child)
					IF li_result <> 1 THEN
						MessageBox("LTD Benefit Calc Number Error","Unable to determine the ltd benefit calc no selected.")
						RETURN 1
					END IF
					ll_child_row = ldwc_child.GetRow()
					IF ll_child_row <= 0 THEN
						MessageBox("LTD Benefit Calc Number Error","Unable to determine the ltd benefit calc no selected.")
						RETURN 1
					END IF
					ldtm_effective_date = ldwc_child.GetItemDatetime(ll_child_row,'effective_from_date')
					IF ldtm_effective_date > idw_dw[2].GetItemDatetime(ll_current_row,'end_date') THEN
						MessageBox("Invalid LTD Benefit Calc No", "The effective date for the chosen LTD benefit calc number cannot be after the end date.", Information!)
						RETURN 2
					END IF
					nf_calc_total_benefit_amt(Date(idw_dw[2].GetItemDatetime(ll_current_row,'start_date')),Date(idw_dw[2].GetItemDatetime(ll_current_row,'end_date')),idw_dw[2].GetItemNumber(ll_current_row,'rloe_benefit_calc_no'),Integer(idw_dw[2].GetText()))
			
				CASE	'rloe_benefit_calc_no'
					li_result = idw_dw[2].GetChild('rloe_benefit_calc_no',ldwc_child)
					IF li_result <> 1 THEN
						MessageBox("RLOE Benefit Calc Number Error","Unable to determine the rloe benefit calc no selected.")
						RETURN 1
					END IF
					ll_child_row = ldwc_child.GetRow()
					IF ll_child_row <= 0 THEN
						MessageBox("RLOE Benefit Calc Number Error","Unable to determine the rloe benefit calc no selected.")
						RETURN 1
					END IF
					ldtm_effective_date = ldwc_child.GetItemDatetime(ll_child_row,'effective_from_date')
					IF ldtm_effective_date > idw_dw[2].GetItemDatetime(ll_current_row,'end_date') THEN
						MessageBox("Invalid RLOE Benefit Calc No", "The effective date for the chosen RLOE benefit calc number cannot be after the end date.", Information!)
						RETURN 2
					END IF
					nf_calc_total_benefit_amt(Date(idw_dw[2].GetItemDatetime(ll_current_row,'start_date')),Date(idw_dw[2].GetItemDatetime(ll_current_row,'end_date')),Integer(idw_dw[2].GetText()),idw_dw[2].GetItemNumber(ll_current_row,'ltd_benefit_calc_no'))

				CASE	'rloe_opening'
/*	Set filter from selected opening onto the ben_calc column.
*/
					nf_set_ben_calc_filters('rloe_benefit_calc_no',Integer(idw_dw[2].GetText()))
					idw_dw[2].SetItem(1,'rloe_benefit_calc_no',0)
					nf_calc_total_benefit_amt(Date(idw_dw[2].GetItemDatetime(ll_current_row,'start_date')),Date(idw_dw[2].GetItemDatetime(ll_current_row,'end_date')),idw_dw[2].GetItemNumber(ll_current_row,'rloe_benefit_calc_no'),idw_dw[2].GetItemNumber(ll_current_row,'ltd_benefit_calc_no'))
			
				CASE	'ltd_opening'
/*	Set filter from selected opening onto the ben_calc column.
*/
					nf_set_ben_calc_filters('ltd_benefit_calc_no',Integer(idw_dw[2].GetText()))
					idw_dw[2].SetItem(1,'ltd_benefit_calc_no',0)
					nf_calc_total_benefit_amt(Date(idw_dw[2].GetItemDatetime(ll_current_row,'start_date')),Date(idw_dw[2].GetItemDatetime(ll_current_row,'end_date')),idw_dw[2].GetItemNumber(ll_current_row,'rloe_benefit_calc_no'),idw_dw[2].GetItemNumber(ll_current_row,'ltd_benefit_calc_no'))
			
				CASE	'capitalization_opening'
/*	Set filter from selected opening onto the ben_calc column.
*/
					nf_set_ben_calc_filters('capitalization_benefit_calc_no',Integer(idw_dw[2].GetText()))
					idw_dw[2].SetItem(1,'capitalization_benefit_calc_no',0)
					idw_dw[2].SetItem(1,'capitalized_amount',0)
					
			END CHOOSE
			nf_determine_total_costs()

	CASE	3

/*	This is for the datawindow idw_dw[3] - which is the tasks.
*/
		CHOOSE CASE idw_dw[al_datawindow].GetColumnName()				
   		CASE "task_type_code"
			ll_row = idw_dw[3].GetChild('task_sub_type_code',ldwc_child)
			ls_filter = "active_flag = 'Y'"
			IF ll_row > 0 THEN
				ldwc_child.SetFilter(ls_filter)
				ldwc_child.SetSort("task_sub_type_desc A")
				ldwc_child.Filter()
				ldwc_child.Sort()
				idw_dw[3].SetItem(idw_dw[3].GetRow(),'task_sub_type_code','')
				idw_dw[3].SetItem(idw_dw[3].GetRow(),'task_specific_code','.')
			ELSE
				Return -1
			END IF
					
		   CASE "task_sub_type_code"
				ll_row = idw_dw[3].GetChild('task_specific_code',ldwc_child)
				ls_filter = "active_flag = 'Y'"
				IF ll_row > 0 THEN
					ldwc_child.SetFilter(ls_filter)
					ldwc_child.SetSort("task_specific_desc A")
					ldwc_child.Filter()
					ldwc_child.Sort()
					idw_dw[3].SetItem(idw_dw[3].GetRow(),'task_specific_code','.')
				ELSE
					Return -1
				END IF

			CASE "task_amount"
				nf_determine_total_costs()
		END CHOOSE
	END CHOOSE

	RETURN 0

end function

public function integer nf_set_ben_calc_filters (string as_filter_column, integer ai_value);/*	This function is used to set the filters on the three ben_calc columns in idw_dw[2].

	Argument:	as_filter_column - The column to apply the filter to.
					as_value - The opening no to used in the filter.
*/
	DATAWINDOWCHILD  	ldwc_child
	STRING          	ls_filter
	INTEGER				li_result

	idw_dw[2].GetChild(as_filter_column,ldwc_child)
	IF li_result < 0 THEN
		MessageBox("","")
		RETURN -1
	END IF
	
	li_result = ldwc_child.SetFilter("opening_no = " + String(ai_value))
	li_result = ldwc_child.Filter()

	RETURN 0

end function

public subroutine nf_determine_total_costs ();/*	This function is used to calculate and display the total costs for the option.
*/
	DECIMAL	ldec_total_benefit_amount, ldec_capitalized_amount, ldec_total_task_amount
	
	ldec_capitalized_amount = 0.0
	ldec_total_benefit_amount = 0.0
	ldec_total_task_amount = 0.0
	idw_dw[2].AcceptText()
	idw_dw[3].AcceptText()
	IF idw_dw[2].RowCount() > 0 THEN
		ldec_total_benefit_amount = idw_dw[2].GetItemDecimal(1,'total_benefit_amount')
		ldec_capitalized_amount = idw_dw[2].GetItemDecimal(1,'capitalized_amount')
		IF IsNull(ldec_capitalized_amount) THEN
			ldec_capitalized_amount = 0.0
		END IF
		IF IsNull(ldec_total_benefit_amount) THEN
			ldec_total_benefit_amount = 0.0
		END IF
	ELSE
		ldec_total_benefit_amount = 0.0
		ldec_capitalized_amount = 0.0
	END IF

	IF idw_dw[3].RowCount() > 0 THEN
		ldec_total_task_amount = idw_dw[3].GetItemDecimal(idw_dw[3].RowCount(),'tasks_total')
		IF IsNull(ldec_total_task_amount) THEN
			ldec_total_task_amount = 0.0
		END IF
	ELSE
		ldec_total_task_amount = 0.0
	END IF

	idec_total_costs = Round(ldec_total_benefit_amount + ldec_capitalized_amount + ldec_total_task_amount,2)

end subroutine

public function integer nf_item_focus_change_task (long al_row, string as_column_name);DATAWINDOWCHILD	ldwc_child
STRING				ls_tasktype, ls_filter, ls_tasksubtype

CHOOSE CASE as_column_name
	CASE "task_sub_type_code"
		ls_tasktype = idw_dw[3].GetItemString(al_row,'task_type_code')
		IF IsNull(ls_tasktype) OR ls_tasktype = '' THEN
			MessageBox("Task Type Error","A task type must be selected prior to selecting a task sub type.")
			RETURN 1
		END IF
		idw_dw[3].GetChild('task_sub_type_code',ldwc_child)
		ls_filter = "task_type_code = '" + ls_tasktype + "' AND active_flag = 'Y'"
		ldwc_child.SetFilter(ls_filter)
		ldwc_child.SetSort('task_sub_type_desc A')
		ldwc_child.Filter()
		ldwc_child.Sort()

	CASE "task_specific_code"
		ls_tasktype = idw_dw[3].GetItemString(al_row,'task_type_code')
		ls_tasksubtype = idw_dw[3].GetItemString(al_row,'task_sub_type_code')
		IF IsNull(ls_tasktype) OR ls_tasktype = '' THEN
			MessageBox("Task Type Error","A task type must be selected prior to a task specific.")
			RETURN 1
		END IF
		IF IsNull(ls_tasksubtype) OR ls_tasksubtype = '' THEN
			MessageBox("Task Sub Type Error","A task sub type must be selected prior to a task specific.")
			RETURN 1
		END IF
		idw_dw[3].GetChild('task_specific_code',ldwc_child)
		ls_filter = "task_type_code = '" + ls_tasktype + "' and task_sub_type_code = '" + ls_tasksubtype + "' AND active_flag = 'Y'"
		ldwc_child.SetFilter(ls_filter)
		ldwc_child.SetSort('task_specific_desc A')
		ldwc_child.Filter()
		ldwc_child.Sort()
					
		IF ldwc_child.RowCount() = ldwc_child.FilteredCount() THEN
			ldwc_child.InsertRow(0)
			ldwc_child.SetItem(1,'task_type_code',ls_tasktype)
			ldwc_child.SetItem(1,'task_sub_type_code',ls_tasksubtype)
			ldwc_child.SetItem(1,'task_specific_desc','.')
			ldwc_child.SetItem(1,'task_specific_code','.')
			ldwc_child.AcceptText()
		END IF

	CASE ELSE
		idw_dw[3].GetChild('task_sub_type_code',ldwc_child)
		ls_filter = ''
		ldwc_child.SetFilter(ls_filter)
		ldwc_child.Filter()
		idw_dw[3].GetChild('task_specific_code',ldwc_child)
		ls_filter = ''
		ldwc_child.SetFilter(ls_filter)
		ldwc_child.Filter()

END CHOOSE

RETURN 0

end function

public function integer nf_delete_option_and_tasks (ref u_dw_online adw_option_list);// nf_delete_option_and_tasks
// 
Long     ll_current_row, ll_options_count, ll_loopcount, ll_tasks_count, ll_tasks_count2, ll_claim_no, ll_cost_no
Integer  li_answer, li_result, li_option_no, li_recommended_option, li_rtn
String   ls_message, ls_authorized_user_id
Boolean  lb_last_option, lb_recommended_option
Datetime ldtm_null_date, ldt_authorized_date

// Get the seleted row from dw_options_list
ll_loopcount = 1
lb_last_option = FALSE
lb_recommended_option = FALSE
ll_options_count = adw_option_list.RowCount()
ll_tasks_count = idw_dw[3].RowCount()
ll_current_row = adw_option_list.GetSelectedRow(0)


// BR 4.4 - A cost analysis option can be deleted if there are no associated tasks 
IF ll_current_row > 0 THEN 
	ll_claim_no = adw_option_list.GetItemNumber(ll_current_row, 'claim_no')
	ll_cost_no = adw_option_list.GetItemNumber(ll_current_row, 'cost_no')
	li_option_no = adw_option_list.GetItemNumber(ll_current_row, 'option_no')

	SELECT COUNT(*) 
	  INTO :ll_tasks_count2  
	  FROM COST_ANALYSIS_OPTION_TASK 
	 WHERE claim_no = :ll_claim_no 
		AND cost_no = :ll_cost_no  
		AND option_no = :li_option_no ; 

	li_rtn = SQLCA.nf_handle_error("n_cost_analysis", "", "nf_delete_option_and_tasks - 	SELECT COUNT(*) FROM COST_ANALYSIS_OPTION_TASK WHERE claim_no = :ll_claim_no AND cost_no = :ll_cost_no AND option_no = :li_option_no") 

	IF ll_tasks_count2 > 0 THEN
		Messagebox("Delete Denied", "Unable to delete option because it has associated tasks.~r~rIf you want to delete this option, you have to remove the tasks first and then try again.")
		RETURN 0
	END IF
END IF

ls_message = "Do you wish to continue with the deletion of the selected option."
IF ll_tasks_count2 > 0 THEN
	ls_message = ls_message + " All associated tasks will also be deleted."
END IF

IF ll_current_row > 0 THEN
	// Determine if this is the last option for the cost analysis.
	IF ll_options_count = 1 AND ll_current_row = 1 THEN
		lb_last_option = TRUE
	END IF

	// Determine if this is the recommended option for the cost analysis
	li_option_no = adw_option_list.GetItemNumber(ll_current_row,'option_no')
	li_recommended_option = idw_dw[1].GetItemNumber(1,'recommended_option_no')
	IF li_option_no = li_recommended_option THEN
		lb_recommended_option = TRUE
	END IF

	// BR 1.4 A Cost Analysis option can be deleted if the cost anlysis is not authorized and the option is not the recommended option
	IF lb_recommended_option = TRUE THEN
		Messagebox("Delete Denied", "This option is not allowed to be deleted as this is the recommended option.")
		RETURN 0
	END IF

	ls_authorized_user_id = idw_dw[1].GetItemString(1, 'authorized_user_id')
	ldt_authorized_date = idw_dw[1].GetItemDatetime(1, 'authorized_date')
	IF (ls_authorized_user_id <> '' AND IsNull(ls_authorized_user_id) = FALSE) OR IsNull(ldt_authorized_date) = FALSE THEN 
		Messagebox("Delete Denied", "The option is not allowed to be deleted because the Cost Analysis has been authorized.")
		RETURN 0
	END IF

	IF lb_last_option THEN
		ls_message = ls_message + " Since this is the last option for the cost analysis, the cost analysis will also be deleted."
	ELSE
		IF lb_recommended_option THEN
			ls_message = ls_message + " Since this is the recommended option for the cost analysis, the recommendation will be cleared. If authorized, the cost analysis will be unauthorized."
		END IF
	END IF
	li_answer = MessageBox("Delete Of A Option",ls_message,Question!,YesNo!)
	IF li_answer = 1 THEN
		SQLCA.nf_begin_transaction()
		
		// Delete all the associated tasks, if any
		IF ll_tasks_count > 0 THEN
			DO WHILE ll_loopcount <= ll_tasks_count
				li_result = idw_dw[3].DeleteRow(1)
				IF li_result <> 1 THEN
					RETURN -1
				END IF
				ll_tasks_count --
			LOOP

			idw_dw[3].Update()
			IF SQLCA.nf_handle_error("idw_dw[3].Update()","n_cost_analysis","nf_delete_option_and_tasks()") < 0 THEN
				RETURN -1
			END IF
		END IF

		//	Delete the record from the dw_options_costs datawindow.
		li_result = idw_dw[2].DeleteRow(1)
		IF li_result <> 1 THEN
			RETURN -1
		END IF

		idw_dw[2].Update()
		IF SQLCA.nf_handle_error("idw_dw[2].Update()","n_cost_analysis","nf_delete_option_and_tasks()") < 0 THEN
			RETURN -1
		END IF

		//	Delete the cost record if the delete is for the last option record or, wipe out the authorized
		// information if the delete is for the recommended option.
		IF lb_last_option OR lb_recommended_option THEN
			IF NOT lb_last_option AND lb_recommended_option THEN
				SetNull(ldtm_null_date)
				idw_dw[1].SetItem(1,'recommended_option_no',0)
				idw_dw[1].SetItem(1,'rationale','')
				idw_dw[1].SetItem(1,'authorized_user_id','')
				idw_dw[1].SetItem(1,'authorized_date',ldtm_null_date)
				idw_dw[1].AcceptText()
			ELSE
				li_result = idw_dw[1].DeleteRow(1)
				IF li_result <> 1 THEN
					RETURN -1
				END IF
			END IF	

			idw_dw[1].Update()
			IF SQLCA.nf_handle_error("idw_dw[1].Update()","n_cost_analysis","nf_delete_option_and_tasks()") < 0 THEN
				RETURN -1
			END IF
		END IF

		SQLCA.nf_commit_transaction()
		
	ELSE
		RETURN 0
	END IF
ELSE
	MessageBox("Delete Options Error","Can not delete an option unless one is chosen.")
END IF

RETURN 1

end function

public function integer nf_get_next_task_no (integer ai_costno, integer ai_optionno);/* This function is used to determine the next task number for an option. It does it by
	determining the current maximum task number and then adds 1 to it. This function
	should only be called once per update.
	
	Argument:	ai_costno - The cost number for the claim for which to calculate the next task number for.
					ai_optionno - The option number for the claim & cost for which to calculate the next task number for.
*/
	INTEGER	li_nexttaskno

	SELECT Max(task_no)
	  INTO :li_nexttaskno
	  FROM COST_ANALYSIS_OPTION_TASK
	 WHERE claim_no = :il_claim_no
	 	AND cost_no = :ai_costno
	 	AND option_no = :ai_optionno
	 USING SQLCA;
	
	IF SQLCA.nf_handle_error("Embedded SQL: SELECT Max(task_no)","n_cost_analysis","nf_get_next_task_no().") < 0 THEN
		RETURN -1
	END IF

	IF IsNull(li_nexttaskno) THEN
		li_nexttaskno = 1
	ELSE
		li_nexttaskno ++
	END IF

	RETURN li_nexttaskno

end function

public function integer nf_get_next_option_no (integer ai_costno);/* This function is used to determine the next option number for a claim. It does it by
	determining the current maximum option number and then adds 1 to it. This function
	should only be called once per update.
	
	Argument:	ai_costno - The cost number for the claim for which to calculate the next option number for.
*/
	INTEGER	li_nextoptionno

	SELECT Max(option_no)
	  INTO :li_nextoptionno
	  FROM COST_ANALYSIS_OPTION
	 WHERE claim_no = :il_claim_no
	 	AND cost_no = :ai_costno
	 USING SQLCA;
	
	IF SQLCA.nf_handle_error("Embedded SQL: SELECT Max(option_no)","n_cost_analysis","nf_get_next_option_no().") < 0 THEN
		RETURN -1
	END IF

	IF IsNull(li_nextoptionno) THEN
		li_nextoptionno = 1
	ELSE
		li_nextoptionno ++
	END IF

	RETURN li_nextoptionno

end function

public function integer nf_set_opening_no (string as_ben_calc_column, string as_opening_column);/*	
*/
	INTEGER	li_ben_calc_no, li_opening_no
	
	li_ben_calc_no = idw_dw[2].GetItemNumber(1,as_ben_calc_column)
			
	IF IsNull(li_ben_calc_no) THEN
		li_ben_calc_no = -1
	END IF
			
	SELECT opening_no  
	  INTO :li_opening_no
	  FROM BENEFIT_CALCULATION  
	 WHERE claim_no = :il_claim_no
		AND benefit_calculation_no = :li_ben_calc_no
	 USING SQLCA;

	IF SQLCA.nf_handle_error("Embedded SQL: SELECT opening_no INTO ...","n_cost_analysis","nf_set_opening_no()") < 0 THEN
		RETURN -1
	END IF
		
	idw_dw[2].SetItem(1,as_opening_column,li_opening_no)
		
	nf_set_ben_calc_filters(as_ben_calc_column,li_opening_no)
			
	RETURN 0

end function

public function decimal nf_get_recom_option_total_costs (integer ai_cost_no, integer ai_recommended_option_no);/*	Get the total costs for the recommended option
*/

			
	SELECT	b.capitalized_amount+
				b.total_benefit_amount +
				ISNULL((SELECT sum(c.task_amount) 
				        FROM COST_ANALYSIS_OPTION_TASK c 
						  WHERE c.claim_no=:il_claim_no  
						  AND c.cost_no=:ai_cost_no
						  AND c.option_no=:ai_recommended_option_no),0)
	INTO		:idec_recom_option_total_costs
	FROM 	 	COST_ANALYSIS_MASTER a, COST_ANALYSIS_OPTION b
	WHERE	 	a.claim_no=:il_claim_no 
	AND	 	a.cost_no=:ai_cost_no 
	AND	 	b.claim_no=a.claim_no
	AND	 	b.cost_no=a.cost_no
	AND	 	b.option_no=:ai_recommended_option_no
	USING 	SQLCA;
				
	IF SQLCA.nf_handle_error('Embedded SQL: SELECT from COST_ANALYSIS_MASTER/OPTION','w_cost_analysis','wf_check_disable') < 0 THEN
		RETURN -1
	END IF
	IF SQLCA.SQLCode = 100 THEN
		MessageBox('Recommended Option Error',"Recommended option doesn't exist",StopSign!)
		RETURN -1
	END IF
	
	RETURN (idec_recom_option_total_costs)
	
end function

public subroutine nf_calc_total_benefit_amt (date adt_start_date, date adt_end_date, integer ai_rloe_benefit_calc_no, integer ai_ltd_benefit_calc_no);/* Calculate total benefit amount for duration of rehabilitation period 
	Arguments:	None
	Return 		None
*/
	LONG 					ll_no_of_days, ll_child_row, ll_row_count
	DECIMAL 				ldec_no_of_weeks, ldec_no_of_months, ldec_rloe_award_amount, ldec_ltd_award_amount, ldec_total_benefit_amount
	INTEGER 				li_result
	DATAWINDOWCHILD 	ldwc_child

	IF IsNull(adt_start_date) OR IsNull(adt_end_date) OR &
		((IsNull(ai_rloe_benefit_calc_no) OR ai_rloe_benefit_calc_no = 0) AND &
		(IsNull(ai_ltd_benefit_calc_no) OR ai_ltd_benefit_calc_no = 0)) THEN
		idw_dw[2].SetItem(1,'total_benefit_amount',0)
		RETURN
	END IF

/*	Get the RLOE award amount
*/
	IF ai_rloe_benefit_calc_no > 0 THEN
		li_result = idw_dw[2].GetChild('rloe_benefit_calc_no',ldwc_child)
		IF li_result <> 1 THEN
			ib_capitalization_difference_flag = TRUE
			MessageBox("Capitalization Benefit Calc Number Error","Unable to determine the benefit calc award amount.")
			RETURN
		END IF	
		ll_row_count = ldwc_child.RowCount()
		ll_child_row = ldwc_child.Find('benefit_calculation_no='+String(ai_rloe_benefit_calc_no),1,ll_row_count)
		ldec_rloe_award_amount = ldwc_child.GetItemDecimal(ll_child_row,'award_amount')
		li_result = ldwc_child.SetRow(ll_child_row)
	END IF

/*	Get the LTD award amount
*/
	IF ai_ltd_benefit_calc_no > 0 THEN
		li_result = idw_dw[2].GetChild('ltd_benefit_calc_no',ldwc_child)
		IF li_result <> 1 THEN
			ib_capitalization_difference_flag = TRUE
			MessageBox("Capitalization Benefit Calc Number Error","Unable to determine the benefit calc award amount.")
			RETURN
		END IF	
		ll_row_count = ldwc_child.RowCount()
		ll_child_row = ldwc_child.Find('benefit_calculation_no='+String(ai_ltd_benefit_calc_no),1,ll_row_count)
		ldec_ltd_award_amount = ldwc_child.GetItemDecimal(ll_child_row,'award_amount')
		li_result = ldwc_child.SetRow(ll_child_row)
	END IF
	
/* Calculate the # days; divide by 7 to get # weeks and divide by 30 to get # months
	Calculate total benefit amount
*/

	ll_no_of_days = DaysAfter(adt_start_date,adt_end_date)
	ldec_no_of_weeks = ll_no_of_days/7
	ldec_no_of_months = ll_no_of_days/30
	ldec_total_benefit_amount = Round((ldec_rloe_award_amount * ldec_no_of_weeks) + (ldec_ltd_award_amount * ldec_no_of_months),2)
	idw_dw[2].SetItem(1,'total_benefit_amount',ldec_total_benefit_amount)
	
	RETURN 



end subroutine

public function integer nf_get_capitalization_factor (integer ai_age, ref long al_capitalization_factor_yyyymm, ref decimal adec_capitalization_factor);/* This function is used to get the capitalization factor.
	
	Arguments:	ai_age									(pass by Value)
					al_capitalization_factor_yyyymm	(pass by Reference)
					adec_capitalization_factor			(pass by Reference)
					
	Returns 		integer
*/
	LONG	 	ll_result, ll_capitalization_factor_yyyymm
	DECIMAL	ldec_capitalization_factor

	SELECT capitalization_factor_yyyymm, capitalization_factor
	  INTO :ll_capitalization_factor_yyyymm, :ldec_capitalization_factor
	  FROM Capitalization_Factor
	 WHERE sex = :is_sex
		AND age = :ai_age
		AND capitalization_factor_yyyymm <=  
				(SELECT max(capitalization_factor_yyyymm)
				   FROM Capitalization_Factor
				  WHERE capitalization_factor_yyyymm <= :al_capitalization_factor_yyyymm)
	 USING SQLCA;

	ll_result = SQLCA.nf_handle_error('Embedded SQL: Select from Capitalization_Factor','n_cost_analysis','nf_get_capitalization_factor')
	IF ll_result  < 0 THEN
		MessageBox("Capitalization Factor","Couldn't read capitalization factor",Information!)
		RETURN -1
	END IF	
		
	IF ll_result = 100 THEN
		MessageBox("Capitalization Factor","There is no capitalization factor for the parameters.~r~n~r~nContact I.T. Dept to verify/correct capitalization factors.",Information!)
		RETURN -1
	END IF
		
	al_capitalization_factor_yyyymm = ll_capitalization_factor_yyyymm
	adec_capitalization_factor = ldec_capitalization_factor
	
	RETURN 0

end function

public function integer nf_calc_capitalized_amt (decimal adec_award_amount, integer ai_age, ref long al_capitalization_factor_yyyymm, ref decimal adec_capitalized_amount);/* Calculate the capitalized_amount based on parameters

	Arguments -	adec_award_amount						(pass by Value)
					ai_age									(pass by Value)
					al_capitalization_factor_yyyymm 	(pass by Reference)
					adec_capitalized_amount 			(pass by Reference)
					
	Returns	 -	integer
*/

DECIMAL	ldec_capitalization_factor
LONG		li_result
STRING 	ls_message

	ldec_capitalization_factor = 0
	li_result = nf_get_capitalization_factor(ai_age,al_capitalization_factor_yyyymm,ldec_capitalization_factor)

	adec_capitalized_amount = Round(adec_award_amount * ldec_capitalization_factor,2)

	RETURN li_result
	
end function

public function integer nf_check_bus_rule ();// nf_check_bus_rule - This function is used to check and see that all the business rules are being satisified by any new and/or modified records.
// 
Long     ll_child_row, ll_capitalization_factor_yyyymm, ll_capitalization_benefit_calc_no    
Integer  li_result, li_recommended_option_no, li_cost_no, li_age
String   ls_rationale
Datetime ldtm_server_datetime, ldtm_null_datetime, ldtm_effective_date, ldtm_start_date, ldtm_end_date
Decimal  ldec_award_amount, ldec_calc_capitalized_amount
DataWindowChild ldwc_child

ldtm_server_datetime = f_server_datetime()
li_cost_no = idw_dw[1].GetItemNumber(1,'cost_no')
li_recommended_option_no = idw_dw[1].GetItemNumber(1,'recommended_option_no')
ls_rationale = Trim(idw_dw[1].GetItemString(1,'rationale'))
ldtm_start_date = idw_dw[2].GetItemDatetime(1,'start_date')
ldtm_end_date = idw_dw[2].GetItemDatetime(1,'end_date')
SetNull(ldtm_null_datetime)	

// BR 3.2 A capitalization cannot be calculated unless the claimant's sex and date of birth are known
IF is_sex = 'U' OR (is_sex <> 'M' AND is_sex <> 'F') THEN 
	MessageBox("Cost Analysis Module Error", "Cannot save data as the individual's sex is unknown.", Exclamation!)
	RETURN -1
END IF

IF IsNull(idtm_birth_date) = TRUE THEN 
	MessageBox("Cost Analysis Module Error", "Cannot save data as the individual's birth date is unknown.", Exclamation!)
	RETURN -1
END IF

// If either entered/changed, must have both the recommended option # and rationale.
IF (NOT IsNull(li_recommended_option_no) AND li_recommended_option_no > 0 AND ls_rationale = '') OR ((IsNull(li_recommended_option_no) OR li_recommended_option_no = 0) AND ls_rationale <> '') THEN
	MessageBox('Recommended Option Error', "Must choose both the Recommended Option and enter the Rationale",Information!)
	RETURN -1
END IF

IF idw_dw[1].GetItemString(1,'authorize_flag') = 'Y' AND ((IsNull(li_recommended_option_no) OR li_recommended_option_no = 0) AND ls_rationale = '') THEN
	MessageBox('Recommended Option Error', "Must choose both the Recommended Option and enter the Rationale",Information!)
	RETURN -1
END IF

IF li_recommended_option_no > 0 AND ls_rationale <> '' THEN
	// If on recommended option in option list (ie. modifying it), get the total costs.	Otherwise, acquire the total from the database. This will be used to determine if authorization limit is adequate.
	IF idw_dw[2].GetItemNumber(1,"option_no") = li_recommended_option_no THEN
		idec_recom_option_total_costs = idec_total_costs
	ELSE
		nf_get_recom_option_total_costs(li_cost_no,li_recommended_option_no)
		IF	idec_recom_option_total_costs < 0 THEN
			// If couldn't determine total costs, return.
			RETURN -1
		END IF
	END IF
	IF idw_dw[1].GetItemString(1,'authorize_flag') = 'Y' THEN
		// Authorize or re-authorize if the user has an adequate authorization limit.			
		IF idec_authorization_limit >= idec_recom_option_total_costs THEN
			idw_dw[1].SetItem(1,'authorized_user_id',vgst_user_profile.user_id)
			idw_dw[1].SetItem(1,'authorized_date',ldtm_server_datetime)
		ELSE
			idw_dw[1].SetItem(1,'authorized_user_id','')
			idw_dw[1].SetItem(1,'authorized_date',ldtm_null_datetime)
			MessageBox('Authorization Limit',"Your authorization limit is not adequate.~r~n Notify someone with adequate limit to authorize.",Information!)
		END IF
	ELSE
		// Unauthorize
		idw_dw[1].SetItem(1,'authorized_user_id','')
		idw_dw[1].SetItem(1,'authorized_date',ldtm_null_datetime)
	END IF		
END IF

IF ldtm_start_date < idtm_accident_date THEN
	MessageBox("Invalid Start Date","The start date for an option must be equal to or greater than the accident date for the claim.")
	RETURN -1
END IF

IF ldtm_start_date > ldtm_end_date THEN
	MessageBox("Invalid End Date","The end date for an option must be equal to or greater than the start date for an option.")
	RETURN -1
END IF

ll_capitalization_benefit_calc_no = idw_dw[2].GetItemNumber(1,'capitalization_benefit_calc_no')
IF ll_capitalization_benefit_calc_no > 0 THEN
	li_result = idw_dw[2].GetChild('capitalization_benefit_calc_no',ldwc_child)
	IF li_result <> 1 THEN
		MessageBox("Capitalization Benefit Calculation Number Error","Unable to determine the benefit calculation number selected.")
		RETURN -1
	END IF
	ll_child_row = ldwc_child.Find("benefit_calculation_no = " + String(ll_capitalization_benefit_calc_no),1,ldwc_child.RowCount())
	IF ll_child_row <= 0 THEN
		MessageBox("Capitalization Benefit Calculation Number Error","Unable to determine the benefit calculation number selected.")
		RETURN -1
	END IF
	ldtm_effective_date = ldwc_child.GetItemDatetime(ll_child_row,'effective_from_date')
	IF ldtm_effective_date > ldtm_end_date THEN
		MessageBox("Invalid Capitalization Bene Calc Number", "The effective date for the chosen CAPITALIZATION benefit calculation number cannot be after the end date.", Information!)
		RETURN -1
	END IF

	// Ensure that the capitalized amount can be calculated
	li_age = f_calculate_age(Date(idtm_birth_date),Date(ldtm_end_date))
	IF li_age < 0 THEN
		MessageBox("Age Error", "Couldn't determine age - is less than 0", Information!)
		RETURN -1
	END IF
	IF IsNull(li_age) OR li_age = 0 THEN
		MessageBox("Age Error","Couldn't determine age - is null or 0", Information!)
		RETURN -1
	END IF
	IF li_age > 64 THEN
		ib_capitalization_difference_flag = TRUE
		MessageBox("Age Greater Than 64", "Age must be less than or equal to 64 years.~r~n~r~nVerify/Correct Individual Data and Correct all Cost Analysis with Capitalizations", Information!)
		RETURN -1
	END IF
	li_result = idw_dw[2].SetItem(1,'age',li_age)
	ldec_award_amount = ldwc_child.GetItemDecimal(ll_child_row,'award_amount')
	ll_capitalization_factor_yyyymm = Long(String(Date(ldtm_end_date),'YYYYMMDD'))/100
	ldec_calc_capitalized_amount = 0
	li_result = nf_calc_capitalized_amt(ldec_award_amount,li_age,ll_capitalization_factor_yyyymm,ldec_calc_capitalized_amount)
	IF li_result < 0 THEN
		RETURN -1
	END IF		
END IF		
	
ll_capitalization_benefit_calc_no = idw_dw[2].GetItemNumber(1,'ltd_benefit_calc_no')
IF ll_capitalization_benefit_calc_no > 0 THEN
	li_result = idw_dw[2].GetChild('ltd_benefit_calc_no',ldwc_child)
	IF li_result <> 1 THEN
		MessageBox("LTD Benefit Calculation Number Error","Unable to determine the benefit calculation number selected.")
		RETURN -1
	END IF
	ll_child_row = ldwc_child.Find("benefit_calculation_no = " + String(ll_capitalization_benefit_calc_no),1,ldwc_child.RowCount())
	IF ll_child_row <= 0 THEN
		MessageBox("LTD Benefit Calculation Number Error","Unable to determine the benefit calculation number selected.")
		RETURN -1
	END IF
	ldtm_effective_date = ldwc_child.GetItemDatetime(ll_child_row,'effective_from_date')
	IF ldtm_effective_date > ldtm_end_date THEN
		MessageBox("Invalid LTD Benefit Calculation Number", "The effective date for the chosen LTD benefit calculation number cannot be after the end date.", Information!)
		RETURN -1
	END IF
END IF

ll_capitalization_benefit_calc_no = idw_dw[2].GetItemNumber(1,'rloe_benefit_calc_no')
IF ll_capitalization_benefit_calc_no > 0 THEN
	li_result = idw_dw[2].GetChild('rloe_benefit_calc_no',ldwc_child)
	IF li_result <> 1 THEN
		MessageBox("RLOE Benefit Calculation Number Error","Unable to determine the benefit calculation number selected.")
		RETURN -1
	END IF
	ll_child_row = ldwc_child.Find("benefit_calculation_no = " + String(ll_capitalization_benefit_calc_no),1,ldwc_child.RowCount())
	IF ll_child_row <= 0 THEN
		MessageBox("RLOE Benefit Calculation Number Error","Unable to determine the benefit calculation number selected.")
		RETURN -1
	END IF
	ldtm_effective_date = ldwc_child.GetItemDatetime(ll_child_row,'effective_from_date')
	IF ldtm_effective_date > ldtm_end_date THEN
		MessageBox("Invalid RLOE Benefit Calculation Number", "The effective date for the chosen RLOE benefit calculation number cannot be after the end date.", Information!)
		RETURN -1
	END IF
END IF

RETURN 0
	
end function

public subroutine nf_set_instance_variables (long al_claim_no, string as_sex, datetime adtm_birth_date, string as_admin_region_code, boolean ab_capitalization_difference_flag, datetime adtm_accident_date);/*	This function is used to set the instance variables for the user object.

	Argument:	al_claim_no - The claim number for the claim.
					as_sex - The sex type of the individual for the claim.
					adtm_birth_date - The birth date of the individual for the claim.
					as_admin_region_code - The admin region for the claim.
					ab_capitalization_difference_flag - A flag used to control when to check for a 
																	difference in capitalization amount.
					adtm_accident_date - The accident date of a claim.
*/
	il_claim_no = al_claim_no
	is_sex = as_sex
	idtm_birth_date = adtm_birth_date
	is_admin_region_code = as_admin_region_code
	idtm_accident_date = adtm_accident_date
	ib_capitalization_difference_flag = ab_capitalization_difference_flag
end subroutine

on n_cost_analysis.create
call super::create
end on

on n_cost_analysis.destroy
call super::destroy
end on

