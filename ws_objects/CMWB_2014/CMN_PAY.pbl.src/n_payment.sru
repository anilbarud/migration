$PBExportHeader$n_payment.sru
forward
global type n_payment from n_pdc
end type
end forward

global type n_payment from n_pdc
end type
global n_payment n_payment

type variables
LONG il_claim_no, il_list_row_count
U_DW_ONLINE idw_basic_claim
DECIMAL ic_authorization_limit
DATE idt_max_paid_to_date
BOOLEAN ib_check_recipient_tax_flag = FALSE
string is_claim_receiving_salary_flag
INTEGER		ii_tier_no
end variables

forward prototypes
public function integer nf_setup_benefit_info (integer ai_benefit_calculation_no, ref decimal ac_daily_rate, ref decimal ac_hourly_rate, ref decimal ac_benefit_level_percentage)
public subroutine nf_set_claim_no (long al_claim_no)
public function integer nf_retrieve_openings ()
public function integer nf_retrieve_benefit ()
public function long nf_set_identifiers ()
public function long nf_get_next_txn_identifier ()
public function long nf_get_next_payment_identifier ()
public function integer nf_get_advances ()
public function string nf_get_comp_day ()
public function integer nf_calculate_age (date adt_birth_date)
public function integer nf_check_openings (datawindowchild adwc_openings, string as_type)
public function boolean nf_is_valid_status_act ()
public function integer nf_set_benefit_filter ()
public function integer nf_retrieve_recipients ()
public function integer nf_set_recipient_filter (boolean as_filter_on)
public function integer nf_insert (long al_row)
public function integer nf_set_surv_recipient_filter ()
public function boolean nf_check_claim_status (long al_claimno)
public function boolean nf_refresh_claim_status (long al_claim_no, string as_sCode, string as_sType)
public function long nf_check_account (long al_docid, string as_type)
public function integer nf_check_recipient_hst (string as_recipient_type_code)
public function integer nf_check_recipient_tax_flag (boolean ab_check_recipient_tax_flag)
public subroutine nf_set_payment_filter (long al_opening_no, long al_benefit_calculation_no)
public function long nf_check_dates (datetime ad_paid_to_date, datetime ad_paid_from_date, datetime ad_benefit_end_date, long al_cycle, string as_award)
public function integer nf_validate_cost_alloc ()
public function integer nf_tab_order (string as_field_name, string as_value)
public function integer nf_set_basic_claim (datawindow adw_basic_claim)
public function integer nf_check_hand_cheque_range (long al_cheque_no, ref boolean ab_check)
public function decimal nf_get_authorization_limit_act ()
public function decimal nf_get_authorization_limit (string as_type)
public function integer nf_validate_claim ()
public function integer nf_set_recipient_defaults ()
public function integer nf_set_unused_fields ()
public function integer nf_retrieve (long al_payment_no)
public function integer nf_validate_recipient (string as_recipient_type_code, long al_recipient_no, long al_tranrow)
public function integer nf_delete_recipient ()
public function integer nf_delete_payment ()
public function integer nf_validate_payment_type (string as_payment_type_code, ref integer ai_error_level, ref string as_error_message, ref string as_award_freq_code, ref string as_days_required, ref string as_dates_required)
public function integer nf_validate_address ()
public function integer nf_setup_address (string as_recipient_type_code, long al_recipient_no, long al_txn_row, string as_cheque_print_group_code)
public function string nf_get_authorization_filter ()
public function integer nf_check_mandatory ()
public function integer nf_change_item_2 ()
public function integer nf_check_dup_recipients ()
public function integer nf_set_defaults ()
public function integer nf_insert_recipient (integer al_row)
public function integer nf_setup_default (decimal adec_daily_rate, integer ai_benefit_calculation_no, decimal adec_benefit_level_percentage, integer ai_opening_no, boolean ab_insert)
public function integer nf_setup_survivor ()
public function integer nf_check_bus_rule ()
public function integer nf_change_item (long al_datawindow)
public function integer nf_retrieve_rtw_tier ()
public subroutine nf_rtw_defaulting (string as_payment_type_code)
public function integer nf_check_rtw_bus_rules (string as_payment_type_code, decimal adec_payment_amount, decimal adec_deduction_amount, decimal adec_bencalc_award_amount, datetime adtm_paid_from_date, datetime adtm_paid_to_date, datetime adtm_effective_date, integer ai_nmbr_cycles)
public function integer nf_setup_rtw_incentive_payment_xref (long al_payment_no, string as_payment_type_code)
public subroutine nf_default_js_rtw_date_range (ref integer ai_opening_no)
public subroutine nf_default_js_rtw_amount (ref integer ai_opening_no)
public function integer nf_get_bank (long al_recipient_no, string as_recipient_type_code, ref string as_bank_no, ref string as_bank_transit_no, ref string as_bank_account_no, long al_payment_no, long al_txn_no)
public function integer nf_authorize_group (ref datawindow adw_dw, string as_action, boolean ab_remove)
public function integer nf_flagged_ungrouped (long al_claim_no)
public function boolean nf_is_valid_status_act_pymt ()
public function boolean nf_is_valid_doctype_act_pymt (string as_doctype)
end prototypes

public function integer nf_setup_benefit_info (integer ai_benefit_calculation_no, ref decimal ac_daily_rate, ref decimal ac_hourly_rate, ref decimal ac_benefit_level_percentage);DATAWINDOWCHILD ldwc_child
DECIMAL  lc_wages_based_on, lc_work_days_per_week, lc_work_hours_per_day
LONG     ll_benefit_rownum
STRING   ls_effective_date, ls_award_freq_code

/* Function Name: nf_setup_benefit_info                                                              
                                                                                                  
	 Purpose:       The purpose of this module is to setup the benefit information for the benefit        
   	             calculation number passed                                                             
                                                                                                      
	 Arguments:     ai_benefit_calculation_no                                                            
   		          ac_daily_rate                                                                        
         	   	 ac_hourly_rate                                                                       
            	    ac_benefit_level_percentage                                                          
                                                                                                      
	 Return Values: 0 Successful completion                                                               
   	            -1 An error has occurred                                                               
*/

/*	Find the benefit calculation number in the child datawindow (viw_child_benefit_calculation_no) and load the values into the
	external datawindows so the user can see them
*/
	ll_benefit_rownum = idw_dw[1].GetChild('benefit_calculation_no', ldwc_child)
	ll_benefit_rownum = ldwc_child.Find("benefit_calculation_no = " + string(ai_benefit_calculation_no),1, ldwc_child.RowCount())
	IF ll_benefit_rownum <= 0 THEN
		MessageBox('Payment Module - Data Integrity Error','Error loading benefit calculation number: ' + String(ai_benefit_calculation_no,"000") + &
   	           '~r~nBenefit Calculation Details not found in BENEFIT_CALCULATION table~r~nPlease call the Help Desk',StopSign!)
		Return -1
	END IF
/*	If the benefit calculation number passed is zero, we just want to initialize everything
*/
	IF ai_benefit_calculation_no = 0 THEN
		idw_dw[1].SetItem(1,"benefit_level_percentage",0)
		idw_dw[1].SetItem(1,"effective_from_date",'')
		idw_dw[1].Modify("t_week_or_month.text='/week'")
	   ldwc_child.SetItem(ll_benefit_rownum,"award_freq_code","W")
		ac_benefit_level_percentage = 0
		ac_daily_rate               = 0
		ac_hourly_rate              = 0
	ELSE
		ac_benefit_level_percentage = ldwc_child.GetItemDecimal(ll_benefit_rownum,"benefit_level_percentage")
		lc_wages_based_on = ldwc_child.GetItemDecimal(ll_benefit_rownum,"award_amount")
		ls_award_freq_code = ldwc_child.GetItemString(ll_benefit_rownum,"award_freq_code")
		lc_work_days_per_week = ldwc_child.GetItemDecimal(ll_benefit_rownum,"preacc_work_days_per_week")
		lc_work_hours_per_day = ldwc_child.GetItemDecimal(ll_benefit_rownum,"preacc_work_hours_per_day")
		IF ls_award_freq_code = "W" THEN
			idw_dw[1].Modify("t_week_or_month.text='/week'")
			IF lc_work_days_per_week > 0 THEN
				ac_daily_rate = lc_wages_based_on / lc_work_days_per_week
			ELSE
				ac_daily_rate = 0
			END IF
			IF lc_work_hours_per_day > 0 THEN
				ac_hourly_rate = (lc_wages_based_on / lc_work_days_per_week) / lc_work_hours_per_day
			ELSE
				ac_hourly_rate = 0
			END IF
		ELSE
			idw_dw[1].Modify("t_week_or_month.text='/month'")
			ac_daily_rate = lc_wages_based_on / 30
			IF lc_work_hours_per_day > 0 THEN
				ac_hourly_rate = (lc_wages_based_on / 30) / lc_work_hours_per_day
			ELSE
				ac_hourly_rate = 0
			END IF
		END IF

		IF IsNull(ldwc_child.GetItemDateTime(ll_benefit_rownum,"effective_from_date")) THEN
			ls_effective_date = ""
		ELSE
			ls_effective_date = "Effective Date: " + string(ldwc_child.GetItemDateTime(ll_benefit_rownum,"effective_from_date"),'yyyy-mm-dd')
		END IF
		idw_dw[1].SetItem(1,"effective_from_date",ls_effective_date)
		idw_dw[1].SetItem(1,"benefit_level_percentage",ac_benefit_level_percentage)
	END IF

	idw_dw[1].SetItem(1,"daily_rate",ac_daily_rate)
	idw_dw[1].SetItem(1,"hourly_rate",ac_hourly_rate)

Return 0
end function

public subroutine nf_set_claim_no (long al_claim_no);	il_claim_no = al_claim_no
Return
end subroutine

public function integer nf_retrieve_openings ();DATAWINDOWCHILD	ldwc_child
LONG					ll_row


	idw_dw[1].GetChild('opening_no', ldwc_child)
	ldwc_child.SetTransObject(SQLCA)
	ldwc_child.SetFilter('')
	ldwc_child.Filter()
	ll_row = ldwc_child.Retrieve(il_claim_no)
/*		always set a zeri no openings 
*/
	ll_row = ldwc_child.InsertRow(1)
	IF ll_row > 0 THEN
		ldwc_child.SetItem(1,'opening_no',0)
	//	ldwc_child.SetItem(1,'max_opening_no',0) Application crashes when doing a setitem on any computed field. In PB 9.0 it returns a -1 as it should. 

	END IF
Return 0








end function

public function integer nf_retrieve_benefit ();DATAWINDOWCHILD  ldwc_child
LONG             ll_row
	
	
	idw_dw[1].GetChild('benefit_calculation_no', ldwc_child)
	ldwc_child.SetTransObject(SQLCA)
	ldwc_child.SetFilter('')
	ldwc_child.Filter()
	ll_row =  ldwc_child.Retrieve(il_claim_no)
	
	ldwc_child.SetSort('benefit_calculation_no A')
	ldwc_child.Sort()
	
	ll_row = ldwc_child.InsertRow(1)
	IF ll_row > 0 THEN
/*	this is for payment types that do not need a benefit calc no
*/
		ldwc_child.SetItem(1,'benefit_calculation_no', 0)
	END IF
Return ll_row






end function

public function long nf_set_identifiers ();
Return 0
end function

public function long nf_get_next_txn_identifier ();LONG	ll_result, ll_txn_no


/*	To ensure that we get the next number without, Update the Last_Claim_Txn_no table incrementing the  
	last_txn_no by 1  (This will lock it so no one else can get in). Then, read it back                 
*/
	UPDATE Last_Claim_Txn_No SET last_txn_no = last_txn_no + 1 using SQLCA;
	ll_result = SQLCA.nf_handle_error("Embedded SQL: Update Last_Claim_Txn_No","n_payment","nf_next_txn_identifier")
	IF ll_result < 0 THEN
		Return -1
	END IF

	CHOOSE CASE SQLCA.SQLNRows
/*		If update was successful (ie. SQLNRows would equal 1), read back the identifier
*/
		CASE 1
			SELECT Last_Claim_Txn_No.last_txn_no INTO :ll_txn_no FROM Last_Claim_Txn_No using SQLCA;
			ll_result = SQLCA.nf_handle_error("Embedded SQL: Update Last_Claim_Txn_No","n_payment","nf_next_txn_identifier")
			IF ll_result < 0 THEN
				Return -1
			END IF
		CASE ELSE
/*		If anything other than 1 record found, display error
*/
			MessageBox("Data Integrity Error", string(SQLCA.SQLNRows) + " record(s) found in Last_Claim_Txn_No~r~nPlease call the help desk",Exclamation!)
			SQLCA.nf_rollback_transaction()
			IF SQLCA.SQLCode <> 0 THEN
				Error.Text = "Error during rollback of Last_Claim_Txn_No in function nf_next_txn_identifier"
				Error.WindowMenu=""
				Error.Object=""
				Error.ObjectEvent="nf_next_txn_identifier"
				SignalError()
			END IF		
			Return -1

	END CHOOSE

Return ll_txn_no


end function

public function long nf_get_next_payment_identifier ();LONG	ll_result, ll_payment_no

/*	To ensure that we get the next number without, Update the Last_Payment_No table incrementing the 
	last_payment_no by 1  (This will lock it so no one else can get in). Then, read it back          
*/
	UPDATE Last_Payment_No SET last_payment_no = last_payment_no + 1 using SQLCA;

	ll_result = SQLCA.nf_handle_error("Embedded SQL: Update Last_Payment_No","n_payments","nf_next_payment_identifier")
	IF ll_result < 0 THEN
		Return -1
	END IF

	CHOOSE CASE SQLCA.SQLNRows
/*	If update was successful (ie. SQLNRows would equal 1), read back the identifier
*/	
		CASE 1
			SELECT Last_Payment_No.last_payment_no INTO :ll_payment_no FROM Last_Payment_No using SQLCA;
			ll_result = SQLCA.nf_handle_error("Embedded SQL: Update Last_Payment_No","n_payments","nf_next_payment_identifier")
			IF ll_result < 0 THEN
				Return -1
			END IF
		CASE ELSE
/*		if anything other than 1 record found, display error
*/
		SQLCA.nf_rollback_transaction()
		IF SQLCA.SQLCode <> 0 THEN
			Error.Text = "Error during rollback of Last_Payment_No in function wf_next_payment_no"
			Error.WindowMenu="w_payment"
			Error.Object=""
			Error.ObjectEvent="uf_next_payment_no"
			SignalError()
		END IF		
		MessageBox("Payment Module - Data Integrity Error", string(SQLCA.SQLNRows) + " record(s) found in Last_Payment_No~r~nPlease call the help desk",Exclamation!)
		Return -1

	END CHOOSE
		
Return ll_payment_no


end function

public function integer nf_get_advances ();LONG	ll_count = 0

/*	This code has been modified for P10229 we need to exclude the history
   records added for this project so that they do not get picked up
	in the Account Payment Module  as they were not paid through that module
*/
IF idw_basic_claim.GetItemString(1,'imaged_flag') = 'Y' THEN
	 
      select  count(*) into :ll_count from PAYMENT a,  Module_Payment_Sub_Type b
       where a.payment_type_code = b.payment_type_code
         and module_code  = '002'
         and payment_no > 0 
         and not exists (select * from PAYMENT_DOCUMENT c
                                 where a.payment_no = c.payment_no)
         and a.claim_no = :il_claim_no
	  USING SQLCA;

	SQLCA.nf_handle_error("Embedded SQL: Retrieve from PAYMENT","n_payment","nf_get_advances")
			
END IF

Return ll_count
end function

public function string nf_get_comp_day ();STRING	ls_week, ls_day

	SetNull(ls_week)
	SetNull(ls_day)

	ls_day = idw_basic_claim.GetItemString(1,'comp_day_code')
	ls_week = idw_basic_claim.GetItemString(1,'comp_week_code')

	SELECT comp_day_desc
	  INTO :ls_day
	  FROM Comp_Day
	 WHERE comp_day_code = :ls_day
	 USING SQLCA;
	SQLCA.nf_handle_error('Embedded SQL: Select from Comp_Day', 'n_payment', 'nf_get_comp_day') 

	SELECT comp_week_code
	  INTO :ls_week
	  FROM Comp_Week
	 WHERE comp_week_code = :ls_week
	 USING SQLCA;
	SQLCA.nf_handle_error('Embedded SQL: Select from Comp_Week', 'n_payment', 'nf_get_comp_day') 

Return ls_day + ' -  '+ ls_week

end function

public function integer nf_calculate_age (date adt_birth_date);LONG		ll_Today_Date, ll_birth_date
STRING	ls_Age

/*	Difference in years between the Birth Date and the Other Date (i.e. Today)
	Makes sure the string is 8 characters long so that the first 4 characters reflects the age
*/

	ll_Birth_Date = Long(String(adt_birth_date,"YYYYMMDD"))
	ll_Today_Date = Long(String(Date(f_server_datetime()),"YYYYMMDD"))

	ls_Age = String(ll_Today_Date - ll_Birth_Date)
	ls_Age = Space(8 - Len(ls_Age)) + ls_Age
	ls_Age = Left(ls_Age, 4)

Return Integer(ls_Age)
end function

public function integer nf_check_openings (datawindowchild adwc_openings, string as_type);	Return adwc_openings.Find('opening_type_code = "' + as_type + '" AND IsNull(benefit_end_date)', 0, adwc_openings.RowCount())
end function

public function boolean nf_is_valid_status_act ();// check final status
	IF idw_basic_claim.GetItemString(1,'claim_status_code') = 'A' OR &
	(idw_basic_claim.GetItemString(1,'claim_status_code') = 'F' AND &
	(idw_basic_claim.GetItemString(1,'claim_status_type_code') = '01' OR &
	idw_basic_claim.GetItemString(1,'claim_status_type_code') = '02' OR &
	idw_basic_claim.GetItemString(1,'claim_status_type_code') = '03' OR &
	idw_basic_claim.GetItemString(1,'claim_status_type_code') = '04')) THEN
		Return TRUE
	ELSE
		Return FALSE
	END IF

end function

public function integer nf_set_benefit_filter ();DATAWINDOWCHILD	ldwc_child
LONG					ll_opening_no

	ll_opening_no = idw_dw[1].GetItemNumber(1,'opening_no')

   idw_dw[1].GetChild('benefit_calculation_no', ldwc_child)
	IF ll_opening_no = 0 OR ldwc_child.RowCount() = 1 THEN			//if rowcount = 1 then only benefit calc of 0
		ldwc_child.SetFilter(" benefit_calculation_no = 0")
	ELSE
		ldwc_child.SetFilter("opening_no = " + string(ll_opening_no) + " or benefit_calculation_no = 0")
	END IF
	ldwc_child.Filter()

Return 0

end function

public function integer nf_retrieve_recipients ();DATAWINDOWCHILD  ldwc_child
LONG             ll_row
	
	
	idw_dw[2].GetChild('recipient_no', ldwc_child)
	ldwc_child.SetTransObject(SQLCA)
	ldwc_child.SetFilter('')
	ldwc_child.Filter()
	ll_row =  ldwc_child.Retrieve(il_claim_no)

Return ll_row






end function

public function integer nf_set_recipient_filter (boolean as_filter_on);DATAWINDOWCHILD	ldwc_child

	idw_dw[2].GetChild('recipient_no',ldwc_child)

	IF as_filter_on THEN
		ldwc_child.SetFilter('individual_no = 0')
		ldwc_child.Filter()
	ELSE
		ldwc_child.SetFilter('')
		ldwc_child.Filter()

	END IF
	


Return 0
		
end function

public function integer nf_insert (long al_row);// Return -2 if no openings - can't add payment
STRING		ls_payment_type_code
INTEGER		li_tier_rows

	nf_retrieve_openings()
	idw_dw[1].Reset()
	nf_retrieve_benefit()
		
	idw_dw[1].InsertRow(al_row)
	
	idw_dw[2].Reset()
	nf_retrieve_recipients()
	idw_dw[2].InsertRow(al_row)
	
	nf_set_defaults()
	ls_payment_type_code = idw_dw[1].GetItemString(1,'payment_type_code')

	li_tier_rows = nf_retrieve_rtw_tier()
	nf_rtw_defaulting(ls_payment_type_code)
	IF li_tier_rows = 0 AND ls_payment_type_code = 'R1' THEN
		MessageBox('No Tiers Available','For this claim opening, the eligible tiers have' + &
											'~nbeen fully paid or there are no eligible tiers.',StopSign!)
	END IF
	
Return 0
end function

public function integer nf_set_surv_recipient_filter ();DATAWINDOWCHILD	ldwc_child

	idw_dw[2].GetChild('recipient_no',ldwc_child)

	ldwc_child.SetFilter("claim_no = " + String(il_claim_no))
	ldwc_child.Filter()
	

Return 0

end function

public function boolean nf_check_claim_status (long al_claimno);string ls_statuscode,ls_statustype

SELECT claim_status_code,claim_status_type_code 
INTO :ls_statuscode ,:ls_statustype
FROM CLAIM 
WHERE claim_no = :al_claimno;
IF SQLCA.nf_handle_error("Embedded SQL: Retrieve from check_claim_status ","n_payment","nf_check_claim_status") < 0 THEN
		Return false
END IF
IF ls_statuscode = idw_basic_claim.GetItemString(1,'claim_status_code') and ls_statustype = idw_basic_claim.GetItemString(1,'claim_status_type_code') THEN
	Return TRUE
ELSE
	Return FALSE
END IF
end function

public function boolean nf_refresh_claim_status (long al_claim_no, string as_sCode, string as_sType);/*
Pr 1414 September 10/2001
Tyler Craft Added function 
to check current status of claim against database not instance datastore for concurrency issues.
*/
string ls_statuscode,ls_statustype
IF isValid(w_sheet) THEN
	IF as_sCode = idw_basic_claim.GetItemString(1,'claim_status_code') THEN
	ELSE
		return false
	END IF
	IF as_sType = idw_basic_claim.GetItemString(1,'claim_status_type_code') THEN
		Return TRUE
	ELSE
		Return FALSE
	END IF
END IF
return true
end function

public function long nf_check_account (long al_docid, string as_type);long ll_count 

CHOOSE Case as_type
	Case 'P'
		SELECT Count(*)
		INTO :ll_count
		FROM PAYMENT_DOCUMENT
		WHERE doc_id = :al_docid
			AND paid_status_code = 'P';
	Case ELSE
		SELECT Count(*)
		INTO :ll_count
		FROM PAYMENT_DOCUMENT
		WHERE doc_id = :al_docid;
END CHOOSE


IF SQLCA.nf_handle_error("Embedded SQL: Retrieve from Check_account ","n_payment","nf_check_account") < 0 THEN
		Return -1
END IF

return ll_count

end function

public function integer nf_check_recipient_hst (string as_recipient_type_code);STRING	ls_hst_flag, ls_rcpt_tax_flag
LONG		ll_result


	SELECT	tax_flag
	INTO		:ls_rcpt_tax_flag
	FROM		Recipient_Type
	WHERE		recipient_type_code = :as_recipient_type_code
	USING SQLCA;
		
	ll_result = SQLCA.nf_handle_error("Embedded SQL: SELECT tax_flag FROM Recipient_Type","n_payment","nf_check_recipient_hst")
	IF ll_result < 0 THEN
		Return -1
	END IF

	IF ls_rcpt_tax_flag = 'N' THEN
		MessageBox('Invalid Recipient/HST Flag','HST Recovery is not allowed for this recipient type.', Exclamation!)
		Return -1
	END IF


Return 1
end function

public function integer nf_check_recipient_tax_flag (boolean ab_check_recipient_tax_flag);ib_check_recipient_tax_flag = ab_check_recipient_tax_flag

Return 0
end function

public subroutine nf_set_payment_filter (long al_opening_no, long al_benefit_calculation_no);DATAWINDOWCHILD  	ldwc_child
STRING          	ls_filter, ls_opening_type_code, ls_rtw_incentive_flag, ls_payment_type_code
LONG					ll_row, ll_no
DECIMAL				ldec_benefit_level
DATE					ldt_accident_recurrence_date
Integer li_rtn

/*	add a filter to get only payment types valid for this screen
*/

	IF IsNull(al_opening_no) THEN
		al_opening_no = 0
	END IF

	IF IsNull(al_benefit_calculation_no) THEN
		al_benefit_calculation_no = 0
	END IF

	ll_row = idw_dw[1].GetRow()
	IF ll_row > 0 THEN
		ls_filter = nf_get_authorization_filter()
				
/*		find the row of the currently selected number
*/
		idw_dw[1].GetChild('opening_no',ldwc_child)
		IF al_opening_no > 0 THEN
			ll_row = ldwc_child.Find('opening_no = ' + string(al_opening_no),0,ldwc_child.RowCount())
		ELSE
			ll_row = 0
		END IF
		IF ll_row > 0 THEN
   			ls_opening_type_code = ldwc_child.GetItemString(ll_row,'opening_type_code')
			ldt_accident_recurrence_date = date(ldwc_child.GetItemDateTime(ll_row,'accident_recurrence_date'))
		ELSE
			ls_opening_type_code = 'I'
		END IF
		IF ls_opening_type_code > '' THEN
			ls_filter = ls_filter + ' AND (opening_type_code = "' + ls_opening_type_code + '"'
		END IF
		
		
		
		IF al_benefit_calculation_no = 0 THEN
/*			see pay types where openings match and benefit level = 0
*/		
			ldec_benefit_level = 0
		ELSE
			idw_dw[1].GetChild('benefit_calculation_no', ldwc_child)
			ll_row = ldwc_child.Find('benefit_calculation_no = ' + string(al_benefit_calculation_no),0,ldwc_child.RowCount())
			IF ll_row > 0 THEN
				ldec_benefit_level = ldwc_child.GetItemNumber(ll_row,'benefit_level_percentage')
				ls_rtw_incentive_flag = ldwc_child.GetItemString(ll_row,'rtw_incentive_flag')
			END IF
		END IF
		
				

/*		
		Prior to January 01, 1998, the 3 Day payment is a valid payment type for RLOE openings having a
		benefit level of 80%. For accidents or recurrences of injury after December 31, 1997, all RLOE benefits 
		start at 85%. The valid payments are determined through the payment type table which is based on the 
		benefit category and the benefit level. Since the 3D payment in this table is at 80% and another payment
		of type "3D" cannot be added, the filter was forced to include 3D payments for RLOE openings after 
		December 31, 1997. NOTE: at some point, the use of the payment_combination and the payment_type tables
		should be reviewed to eliminate the reliance on the benefit level in determining the valid payment types.	
*/
		
		IF ls_opening_type_code = "RLOE"   							AND  &
			ldt_accident_recurrence_date > Date ("1997/12/31") AND &
			al_benefit_calculation_no > 0 							THEN
			ls_filter = ls_filter + ' AND benefit_level_percentage = ' + String(ldec_benefit_level) + ') OR  payment_type_code = "3D"'
		ELSE
			IF ls_opening_type_code > '' THEN
				ls_filter = ls_filter + ' AND benefit_level_percentage = ' + String(ldec_benefit_level) + ')'
			ELSE
				ls_filter = ls_filter + ' AND benefit_level_percentage = ' + String(ldec_benefit_level)
			END IF
		END IF
		
		
		/*
		P10261 - RTW Incentive
		*/
		ls_payment_type_code = idw_dw[1].GetItemString(1,'payment_type_code')
		IF ls_rtw_incentive_flag ='Y' THEN
			IF ls_payment_type_code <> 'R2' AND ls_payment_type_code <> 'R3' THEN
				idw_dw[1].SetItem(1,'payment_type_code','')
			END IF
			ls_filter = ls_filter + ' AND (payment_type_code = "R2" OR payment_type_code = "R3" )'
		ELSE
			IF ls_payment_type_code = 'R2' OR ls_payment_type_code = 'R3' THEN
				idw_dw[1].SetItem(1,'payment_type_code','')
			END IF
			ls_filter = ls_filter + ' AND (payment_type_code <> "R2" AND payment_type_code <> "R3" )'
		END IF
		
		//P10151-305 - Ability to Issue PPI to NLT Claim - Only allow F/03/04 to issue .4 pmt
		ls_payment_type_code = idw_dw[1].GetItemString(1,'payment_type_code')
		IF idw_basic_claim.GetItemString(1,'claim_status_code') = 'F' AND (idw_basic_claim.GetItemString(1,'claim_status_type_code') = '03' OR idw_basic_claim.GetItemString(1,'claim_status_type_code') = '04')THEN
			ls_filter = ls_filter + ' AND payment_type_code = ".4"'
		END IF
		

		IF idw_dw[1].dataobject = 'd_payment_details' THEN
			li_rtn = idw_dw[1].GetChild('payment_type_and_sub_type', ldwc_child)
		ELSE
			li_rtn = idw_dw[1].GetChild('payment_type_code', ldwc_child)
		END IF
		li_rtn = ldwc_child.SetFilter(ls_filter)
		If li_rtn = -1 Then
			SignalError(-666,'Error setting filter criteria for payment_type_code. Expression = "' + ls_filter + '"')
		End if
		ldwc_child.Filter()
				
	END IF

Return
end subroutine

public function long nf_check_dates (datetime ad_paid_to_date, datetime ad_paid_from_date, datetime ad_benefit_end_date, long al_cycle, string as_award);DATETIME	ldtm_benefit_end
DATE		ld_paid_from_date,ld_paid_to_date, ld_effective_date


ld_paid_from_date = Date(ad_paid_from_date)
ld_paid_to_date   = Date(ad_paid_to_date)
ldtm_benefit_end  = ad_benefit_end_date

IF as_award = 'M' THEN
	IF Day(ld_paid_from_date) <> Day(ld_paid_to_date) or Day(ld_paid_from_date) <> 1 or Day(ld_paid_to_date) <> 1 THEN
	    MessageBox("Payment Module - Validation Error","For monthly cyclical payments, the period dates must be the first of 1 month to the first of the next month"+"~r~nThe day must be set to 1 or change the number of cyclical payments to 1",Exclamation!)
	     Return -1
	END IF
	IF ((Month(ld_paid_from_date)+1) <> Month(ld_paid_to_date)) and (Month(ld_paid_from_date) <> 12 and Month(ld_paid_to_date) <> 1) THEN
	    MessageBox("Payment Module - Validation Error","For monthly cyclical payments, the period end date must be 1 month after the start date"+"~r~nThe month must be set to "+string(Month(ld_paid_from_date)+1),Exclamation!)
	    Return -1
	END IF
ELSEIF as_award = 'W' THEN
	IF ld_paid_to_date <> RelativeDate(ld_paid_from_date,14) THEN
		MessageBox("Payment Module - Validation Error","For cyclical payments, the period end date, must be 14 days after the start date.",Exclamation!)
		Return -1
	END IF
END IF

IF NOT IsNull(ldtm_benefit_end) THEN
	IF ld_paid_from_date > Date(ldtm_benefit_end) THEN
		MessageBox('Invalid Paid From Date','The from date of Payment Cycle ' + string(al_cycle) + ' (' + string(ld_paid_from_date, 'mmm dd, yyyy') + '),~nmust be before the benefit end date for the opening, ' + ' (' + string(ad_benefit_end_date, 'mmm dd, yyyy') + ')' )
		Return -1
	END IF

	IF ld_paid_to_date > Date(ldtm_benefit_end) THEN
		MessageBox('Invalid Paid To Date','The to date of Payment Cycle ' + string(al_cycle) + ' (' + string(ld_paid_to_date, 'mmm dd, yyyy') + '),~nmust be before the benefit end date for the opening, ' + ' (' + string(ad_benefit_end_date, 'mmm dd, yyyy') + ')' )
		Return -1
	END IF
END IF

Return 1


end function

public function integer nf_validate_cost_alloc ();Long ll_result, ll_cost_alloc_no, ll_cost_alloc_operation_no

ll_cost_alloc_no           = idw_basic_claim.GetItemNumber(1, 'cost_alloc_no')
ll_cost_alloc_operation_no = idw_basic_claim.GetItemNumber(1, 'cost_alloc_operation_no')

SELECT employer_no
  INTO :ll_cost_alloc_no
  FROM OPERATION 
 WHERE employer_no = :ll_cost_alloc_no 
   AND operation_no = :ll_cost_alloc_operation_no
 USING SQLCA ;

ll_result = SQLCA.nf_handle_error("Embedded SQL: Retrieve OPERATION","w_payments","Nf_validate_claim for w_payments")
IF ll_result < 0 THEN
	Return -1
END IF

IF ll_result = 100 THEN
	MessageBox('Warning No Payments Allowed', "Claim has an invalid Employer/Operation ( " + String(ll_cost_alloc_no) + "/" + String(ll_cost_alloc_operation_no) + " )" )
	Return -1
END IF

Return 0
end function

public function integer nf_tab_order (string as_field_name, string as_value);DATE     ld_null_date
DECIMAL  lc_benefit_level_percentage
LONG     ll_rownum, ll_no
Integer li_rtn
STRING   ls_fromto_dates_required_flag, ls_repeat_payment_allowed_flag, ls_opening_type_code
STRING   ls_days_required_flag, ls_adjustment_flag, ls_payment_type_code
DATAWINDOWCHILD ldwc_child

/* Function Name: wf_tab_order                                                                         
	Purpose:       The purpose of this function is to enable/disable fields and controls as well as to 
   	            hide them when they are not required                                                 
      	         Why? because tab orders were being set and reset everywhere and it was getting very  
                  hard to maintain.  With this, if the actual tab order changes on the datawindow 
                  only this function needs to be modified.                                      
             		Note: sometimes we're changing the tab order when it doesn't need to be changed, but  
            	   the alternative is to compare the new value of vas_field_name with the old value and  
      	         then only set tab orders when required -- the problem is that this kind of code gets  
   	   	      very hard to maintain!                                                                
	 Arguments:    as_field_name - the name of the field or 'section' being evaluated                    
   	            as_value      - the current value of the field                                        
	 Return Values:n/a                                                                                   
*/

	SetNull(ld_null_date)


	CHOOSE CASE as_field_name
		CASE "recipient_type_code"
//SR70 - Special Survivor Payment - JAN 2001
			ls_payment_type_code = idw_dw[1].GetItemString(1,'payment_type_code')
		   IF as_value = "I" AND ls_payment_type_code = 'S1' THEN
				nf_set_surv_recipient_filter()
				idw_dw[2].SetTabOrder("recipient_no",20)
			ELSE	
 			 	IF as_value = "I" or as_value = "" THEN					
					ll_no = idw_dw[1].GetItemNumber(1,'opening_no')
					idw_dw[1].GetChild('opening_no',ldwc_child)
					ll_no = ldwc_child.Find('opening_no = ' + String(ll_no),1,ldwc_child.RowCount())
					IF ll_no > 0 THEN
						IF ldwc_child.GetItemString(ll_no,'opening_type_code') = 'PEN' &
						OR ldwc_child.GetItemString(ll_no,'opening_type_code') = 'SV' &
						OR ldwc_child.GetItemString(ll_no,'opening_type_code') = 'S1' &
						OR ldwc_child.GetItemString(ll_no,'opening_type_code') = 'S2' THEN
							nf_set_recipient_filter(FALSE)
						ELSE
							nf_set_recipient_filter(TRUE)
						END IF
						idw_dw[2].SetTabOrder("recipient_no",20)
					ELSE
						idw_dw[2].SetTabOrder("recipient_no",0)
					END IF
			 	ELSE
					nf_set_recipient_filter(TRUE)
					idw_dw[2].SetTabOrder("recipient_no",20)
			 	END IF
			END IF

		CASE "payment_method_code"
			idw_dw[2].SetRedraw(false)
/*			this is now done with the attributes on the data window
*/
			idw_dw[2].SetRedraw(true)

		CASE "payment_type_code"  
			// Note: the payment type code fields are not touched, however, the value of payment type code affects all other fields on the screen     
			idw_dw[1].SetRedraw(false)
			// Retrieve the flags for the payment type
			IF idw_dw[1].dataobject = 'd_payment_details' THEN
				li_rtn = idw_dw[1].GetChild('payment_type_and_sub_type', ldwc_child)
			ELSE
				li_rtn = idw_dw[1].GetChild('payment_type_code', ldwc_child)
			END IF
			ll_rownum = ldwc_child.Find("payment_type_code = '" + as_value + "'",1, ldwc_child.RowCount())
			// If the payment type code was not found, turn off the tab order on everything except payment type code fields
			// Otherwise, Determine (based on payment type) which fields need to be enabled/disabled
			IF ll_rownum <= 0  or IsNull(ll_rownum) THEN
				idw_dw[1].SetTabOrder("nmbr_cycles",0)
				idw_dw[1].SetTabOrder("paid_days_lost",0)                    
				idw_dw[1].SetTabOrder("paid_hours_lost",0)   
	 			idw_dw[1].SetTabOrder("total_award_amount",0)
				idw_dw[1].SetTabOrder("paid_from_date",0)
				idw_dw[1].SetTabOrder("paid_to_date",0)
				idw_dw[1].SetTabOrder("scheduled_processing_date",0)
				idw_dw[1].SetTabOrder("total_deductions",0)
				idw_dw[1].SetTabOrder("final_payment_flag",0)
			ELSE
				ls_fromto_dates_required_flag = ldwc_child.GetItemString(ll_rownum,"fromto_dates_flag") 
				ls_opening_type_code          = ldwc_child.GetItemString(ll_rownum,"benefit_category_code")
				ls_days_required_flag         = ldwc_child.GetItemString(ll_rownum,"days_hours_flag")
				lc_benefit_level_percentage   = ldwc_child.GetItemDecimal(ll_rownum,"benefit_level_percentage")
				ls_repeat_payment_allowed_flag= ldwc_child.GetItemString(ll_rownum,"repeat_payment_allowed_flag")
/*				Number of cycles:
*/

				IF (idw_dw[1].GetItemStatus(1,0,Primary!) = NewModified! OR idw_dw[1].GetItemStatus(1,0,Primary!) = New!) AND ls_repeat_payment_allowed_flag = "Y" THEN
					idw_dw[1].SetTabOrder("nmbr_cycles",40)
				ELSE
					idw_dw[1].SetTabOrder("nmbr_cycles",0)
				END IF
/*				For LTD adjustments the days and hours lost are not required
*/
				ls_adjustment_flag = idw_dw[1].GetItemString(1,"payment_adjustment_flag")
				IF ls_adjustment_flag = "Y" THEN ls_days_required_flag = "N"

/*		Days lost, hours lost, and the total award amount:
*/
				IF ls_days_required_flag = "Y" THEN
					idw_dw[1].SetTabOrder("paid_days_lost",100)                    
					idw_dw[1].SetTabOrder("paid_hours_lost",110) 
				 	idw_dw[1].SetTabOrder("total_award_amount",0)
				ELSE
					idw_dw[1].SetTabOrder("paid_days_lost",0)                    
					idw_dw[1].SetTabOrder("paid_hours_lost",0)   
				 	idw_dw[1].SetTabOrder("total_award_amount",120)
				END IF
/*		From and to dates:
*/
				IF ls_fromto_dates_required_flag = "Y" THEN
					idw_dw[1].SetTabOrder("paid_from_date",50)
					idw_dw[1].SetTabOrder("paid_to_date",55)
				ELSE
					idw_dw[1].SetItem(1,"paid_from_date",ld_null_date)
					idw_dw[1].SetItem(1,"paid_to_date",ld_null_date)
					idw_dw[1].SetTabOrder("paid_from_date",0)
					idw_dw[1].SetTabOrder("paid_to_date",0)
				END IF
/*				If the payment type has changed from null, these may be disabled - so lets set them just in case
*/
				idw_dw[1].SetTabOrder("final_payment_flag",70)
				idw_dw[1].SetTabOrder("scheduled_processing_date",60)
				idw_dw[1].SetTabOrder("total_deductions",130)
			END IF
		idw_dw[1].SetRedraw(true)

	END CHOOSE
Return 0
end function

public function integer nf_set_basic_claim (datawindow adw_basic_claim);	idw_basic_claim = adw_basic_claim	
	IF idw_basic_claim.rowcount() > 0 THEN
		is_claim_receiving_salary_flag = idw_basic_claim.GetItemString(1,'receiving_salary_flag')
	END IF 
Return 0
end function

public function integer nf_check_hand_cheque_range (long al_cheque_no, ref boolean ab_check);/*	
...'When creating a handwritten cheque transaction, the cheque number
		must be
		(1) - greater than the minimum handwritten cheque number range and less
				than the maximum handwritten cheque number range
		(2) - the handwritten cheque number must not already exist on the 
				CHEQUE_HEADER table or on any UNAPPLIED_CLAIM_TXN records - 
				The validation should be done when the user enters the handwritten cheque.'

ARGS:
	al_cheque_no			LONG			handwritten cheque number to check
	ab_check					boolean		BY REFreturns false if it fails validation

RETURNS:
	1				Pass
	-1 			fail
	-2				Fail on check min max range
	-3				record found on cheque header
	-4				record found on UNAPPLIED_CLAIM_TXN 
*/
DECIMAL		ld_min_amount
DECIMAL		ld_max_amount

INT			li_error

LONG			ll_count


////validation number 1
  SELECT min_handwritten_cheque_no,   
         max_handwritten_cheque_no  
    INTO :ld_min_amount,   
         :ld_max_amount  
    FROM Handwritten_Cheque_No_Range  
   USING SQLCA;

IF SQLCA.nf_handle_error("Embedded SQL: Retrieve from Handwritten_Cheque_No_Range ","n_payment","nf_check_hand_cheque_range_") < 0 THEN
			Return -1
END IF

IF al_cheque_no < ld_min_amount OR al_cheque_no > ld_max_amount THEN
	ab_check = FALSE
	RETURN -2
ELSE
	ab_check = TRUE
END IF

//validation number 2
  SELECT count(cheque_no)  
    INTO :ll_count  
    FROM CHEQUE_HEADER 
	 WHERE cheque_no = :al_cheque_no
	 using sqlca;

IF SQLCA.nf_handle_error("Embedded SQL: Retrieve from CHEQUE_HEADER","n_payment","nf_check_hand_cheque_range_") < 0 THEN
		Return -1
END IF

IF ll_count > 0 THEN
	ab_check = FALSE
	RETURN -3
ELSE
	ab_check = TRUE
END IF

  SELECT count(cheque_no)  
    INTO :ll_count  
    FROM UNAPPLIED_CLAIM_TXN  
	WHERE cheque_no = :al_cheque_no
	 using sqlca;
	 
	 
IF SQLCA.nf_handle_error("Embedded SQL: Retrieve from UNAPPLIED_CLAIM_TXN","n_payment","nf_check_hand_cheque_range_") < 0 THEN
		Return -1
END IF

IF ll_count > 0 THEN
	ab_check = FALSE
	RETURN -4
ELSE
	ab_check = TRUE
END IF

return 1

end function

public function decimal nf_get_authorization_limit_act ();STRING		ls_admin_region

ls_admin_region = idw_basic_claim.GetItemString(1,'admin_region_code')

	
If	gnv_user_authorizations.nf_authorizations_exist(ls_admin_region,'act') = False Then
	MessageBox('Warning',"You don't have any Account Authorizations for this claims region.")
	RETURN -1
ELSE
	RETURN gnv_user_authorizations.nf_get_authorization_limit(ls_admin_region,'act')
END IF

RETURN -1

end function

public function decimal nf_get_authorization_limit (string as_type);STRING		ls_admin_region

ls_admin_region = idw_basic_claim.GetItemString(1,'admin_region_code')


If gnv_user_authorizations.nf_authorizations_exist(ls_admin_region,as_type) = False Then
	MessageBox('Warning',"You don't have the proper Authorization authority for this claims region." )
	RETURN -1
END IF

RETURN gnv_user_authorizations.nf_get_authorization_limit(ls_admin_region,as_type)

RETURN -1


end function

public function integer nf_validate_claim ();LONG     ll_result, ll_counter

/* Function Name: nf_validate_claim                                                        
	Purpose:       To determine if the claimant can receive benefits                              
	Arguments:     vai_error_message -  the error message is returned to the calling routine      
	Return Values: -1 error                                                                       
*/

/*	Check the user profile structure to see if the user can make payments for this claim
	and if so, what their authorization limit is
*/

	STRING		ls_admin_region

	ls_admin_region = idw_basic_claim.GetItemString(1,'admin_region_code')
	IF gnv_user_authorizations.nf_authorizations_exist(ls_admin_region,'') = False THen
		MessageBox('Warning',"You don't have any Authorizations for this claims admin region.")
   	Return -1
	END IF

 	IF nf_validate_cost_alloc() < 0 THEN
		Return -1
	END IF

Return 0
end function

public function integer nf_set_recipient_defaults ();DATE		ldt_null
LONG     ll_transaction_rowcount, ll_tranrow, ll_counter
DECIMAL  ldec_sum_txn, ldec_total_payment_amount

	SetNull(ldt_null)
	ll_transaction_rowcount = idw_dw[2].RowCount()
	ll_tranrow              = idw_dw[2].GetRow()

/* Setup Defaults                                                                      
*/
	idw_dw[2].SetItem(ll_tranrow,"recipient_type_code","V")
	idw_dw[2].SetItem(ll_tranrow,"payment_method_code","A")
	idw_dw[2].SetItem(ll_tranrow,"cheque_deposit_date",ldt_null)
	idw_dw[2].SetItem(ll_tranrow,"cheque_no",0)
	idw_dw[2].SetItem(ll_tranrow,"manual_cheque_req_no",0)
	idw_dw[2].SetItem(ll_tranrow,"use_default_address_flag","Y")
	idw_dw[2].SetItem(ll_tranrow,'cheque_print_group_code', ' ')
	ll_counter = 1
	ldec_sum_txn = 0
/*	Compute the balance of the transaction amount
*/
	DO WHILE ll_counter <= ll_transaction_rowcount
		ldec_sum_txn = ldec_sum_txn + idw_dw[2].GetItemDecimal(ll_counter,"txn_amount")
		ll_counter++
	LOOP

	ldec_total_payment_amount = idw_dw[1].GetItemDecimal(1,"total_payment_amount")
	IF (ldec_total_payment_amount - ldec_sum_txn) > 0 THEN
		idw_dw[2].SetItem(ll_tranrow,"txn_amount",ldec_total_payment_amount - ldec_sum_txn)
	END IF
	nf_tab_order("recipient_type_code","V")

Return 0
end function

public function integer nf_set_unused_fields ();LONG  ll_row, ll_loop, ll_max

	ll_row = idw_dw[1].GetRow()

	IF ll_row > 0 THEN
		IF IsNull(idw_dw[1].GetItemNumber(ll_row,'paid_days_lost')) THEN
			idw_dw[1].SetItem(ll_row,'paid_days_lost', 0)
		END IF
		IF IsNull(idw_dw[1].GetItemNumber(ll_row,'paid_hours_lost')) THEN
			idw_dw[1].SetItem(ll_row,'paid_hours_lost', 0)
		END IF

	END IF
	ll_max = idw_dw[2].RowCount()
	ll_loop = 1
	DO WHILE ll_loop <= ll_max
		IF IsNull(idw_dw[2].GetItemString(ll_loop,'prov_state_code')) THEN
			idw_dw[2].SetItem(ll_loop,'prov_state_code', ' ')
		END IF
		/* August 31, 1999 - New validation for default address flag. Cannot be No for payment methods D and R or non-individual */
		IF idw_dw[2].GetItemString(ll_loop, "payment_method_code") = "D" OR &
		  idw_dw[2].GetItemString(ll_loop, "payment_method_code") = "R" OR &
		  idw_dw[2].GetItemString(ll_loop,"recipient_type_code") <> 'I' THEN
		  idw_dw[2].SetItem(ll_loop,"use_default_address_flag", 'Y')
		  idw_dw[2].SetItem(ll_loop,"cheque_print_group_code" , ' ')
		END IF

		ll_loop = ll_loop + 1
	LOOP
Return 0  
end function

public function integer nf_retrieve (long al_payment_no);LONG		ll_rows, ll_tran_row, ll_cnt, ll_txn_no, ll_tier_rows
STRING	ls_b_no, ls_t_no, ls_a_no, ls_payment_type_code

	nf_retrieve_openings()
	nf_retrieve_benefit()
	ll_rows = idw_dw[1].Retrieve(al_payment_no)
	ll_tier_rows = nf_retrieve_rtw_tier()
	nf_retrieve_recipients()
	ll_tran_row = idw_dw[2].Retrieve(al_payment_no)
	
	IF ll_rows > 0 THEN	
		nf_set_benefit_filter()
		nf_set_payment_filter(idw_dw[1].GetItemNumber(1,'opening_no'), idw_dw[1].GetItemNumber(1,'benefit_calculation_no'))
		
		ls_payment_type_code = idw_dw[1].GetItemString(1,'payment_type_code')
		IF ls_payment_type_code = '' OR IsNull(ls_payment_type_code) THEN
		ELSE			
			nf_rtw_defaulting(ls_payment_type_code)
		END IF
		
/*		retrieve the transaction details
*/
		IF ll_tran_row > 0 THEN
			ll_cnt = 1
			DO WHILE ll_cnt <= ll_tran_row
				IF idw_dw[2].GetItemString(ll_cnt,'recipient_type_code') = 'I' THEN
					nf_set_recipient_filter(FALSE)
				ELSE
					nf_set_recipient_filter(TRUE)
				END IF
				
				IF idw_dw[2].GetItemString(ll_cnt,'payment_method_code') = 'D' THEN
					ll_txn_no = idw_dw[2].GetItemNumber(ll_cnt,'txn_no')
					nf_get_bank(idw_dw[2].GetItemNumber(ll_cnt,'recipient_no'),idw_dw[2].GetItemString(ll_cnt,'recipient_type_code'),ls_b_no, ls_t_no, ls_a_no, al_payment_no,ll_txn_no)
					idw_dw[2].SetItem(ll_cnt,'bank_no', ls_b_no)
					idw_dw[2].SetItem(ll_cnt,'bank_transit_no', ls_t_no)
					idw_dw[2].SetItem(ll_cnt,'bank_account_no', ls_a_no)
				END IF

				ll_cnt++
			LOOP
		END IF	
	ELSE
		Return -1
	END IF

Return ll_rows
end function

public function integer nf_validate_recipient (string as_recipient_type_code, long al_recipient_no, long al_tranrow);LONG             ll_result
STRING           ls_recipient_sub_type, ls_role_code, ls_flag, ls_opening_type_code, ls_payment_type_code
DATAWINDOWCHILD  ldwc_child


	CHOOSE CASE as_recipient_type_code
		CASE ''
			Return -1
			
	   		
	   CASE "I"
			idw_dw[2].SetItem(al_tranrow, 'recipient_sub_type_code', ' ') /* March 27, 1998: initialize recipient sub-type code */
			ls_payment_type_code = idw_dw[1].GetItemString(1, 'payment_type_code')
			// SR70 - Special Survivor Payment JAN 2001		
			IF ls_payment_type_code = 'S1' THEN
				SELECT claim_role_code
				  INTO :ls_role_code
				  FROM CLAIM_PARTICIPANT	
				 WHERE individual_no = :al_recipient_no
			   	AND claim_no = :il_claim_no
				 USING SQLCA;
	
			   ll_result = SQLCA.nf_handle_error("Embedded SQL: Retrieve on CLAIM_PARTICIPANT","n_payment","nf_validate_recipient")
			   IF ll_result < 0 THEN
	   			return -1
		   	ELSEIF ll_result = 100 THEN
					MessageBox('Error','Invalid individual number.')
				   Return -1
				ELSEIF  ls_role_code <> 'SS' AND ls_role_code <> 'TR' THEN
					MessageBox('Invalid Individual','This individual is not eligible to receive the special survivor payment.')
					Return -1
		   	END IF
			ELSE			
			/*		if opening of type pension individual must be eligible for benefits
			*/
				nf_set_recipient_filter(FALSE)
				ll_result = idw_dw[1].GetItemNumber(1,'opening_no')
				idw_dw[1].GetChild('opening_no',ldwc_child)
				ll_result = ldwc_child.Find('opening_no = ' + String(ll_result),0,ldwc_child.RowCount())
				ls_opening_type_code = ' '
				IF ll_result > 0 THEN
					ls_opening_type_code = ldwc_child.GetItemString(ll_result,'opening_type_code')
				END IF
	
				SELECT pen_survivor_eligibility_flag, claim_role_code
				  INTO :ls_flag, :ls_role_code
				  FROM CLAIM_PARTICIPANT	
				 WHERE individual_no = :al_recipient_no
				   AND claim_no = :il_claim_no
				 USING SQLCA;

			   ll_result = SQLCA.nf_handle_error("Embedded SQL: Retrieve on CLAIM_PARTICIPANT","n_payment","nf_validate_recipient")
			   IF ll_result < 0 THEN
	   			return -1
		   	ELSEIF ll_result = 100 THEN
					MessageBox('Error','Invalid individual number.')
				   Return -1
				ELSEIF ls_flag <> 'Y' AND (ls_opening_type_code = 'PEN' OR ls_opening_type_code = 'SV' OR ls_opening_type_code = 'S1' OR ls_opening_type_code = 'S2') THEN
					MessageBox('Invalid Individual','This individual is not eligible to receive benefits.')
					Return -1
		   	END IF
	
				SELECT sin_no
				  INTO :ll_result
				  FROM INDIVIDUAL
				 WHERE individual_no = :al_recipient_no
				 USING SQLCA;
			   
				IF SQLCA.nf_handle_error("Embedded SQL: Retrieve on INDIVIDUAL","n_payment","nf_validate_recipient") < 0 THEN
					Return -1
				END IF

				IF ll_result = 0 THEN
					IF (ls_opening_type_code = 'PEN' OR ls_opening_type_code = 'SV' OR ls_opening_type_code = 'S1' OR ls_opening_type_code = 'S2')  AND &
					(ls_role_code = 'C' OR ls_role_code = 'SS') THEN
						MessageBox('Warning',"The individual's SIN is not recorded.  This should be entered for tax purposes.")
					ELSEIF NOT (ls_opening_type_code = 'PEN' OR ls_opening_type_code = 'SV' OR ls_opening_type_code = 'S1' OR ls_opening_type_code = 'S2') THEN
						MessageBox('Warning',"The individual's SIN is not recorded.  No payments are allowed.")
						Return -1
					END IF
				END IF
			END IF

			Return 1
	   CASE ELSE
			/*		March 27, 1998  - If recipient is a payee, read the provider sub-type from Service Providers table          
			*/
			IF IsNull(as_recipient_type_code) OR IsNull(al_recipient_no) THEN
				MessageBox('Warning','Recipient number or type is missing')
				Return -1
			END IF
			
		   SELECT PROVIDER.provider_sub_type_code
	   	  INTO :ls_recipient_sub_type
		     FROM PROVIDER  
	   	 WHERE ( PROVIDER.provider_no        = :al_recipient_no ) AND  
			   	 ( PROVIDER.provider_type_code = :as_recipient_type_code) AND
  	   	       ( PROVIDER.active_flag        = 'Y' )  
   	    USING SQLCA ;
	   	ll_result = SQLCA.nf_handle_error("Embedded SQL: Retrieve on SERVICE_PROVIDER","n_payment","nf_validate_recipient")
		   IF ll_result < 0 THEN
	   		return -1
		   ELSE
				IF ll_result = 100 THEN
					MessageBox('Error','Invalid provider number.')
		   		Return -1
				ELSE
					IF IsNull(ls_recipient_sub_type) THEN
						idw_dw[2].SetItem(al_tranrow,"recipient_sub_type_code",' ')  
					ELSE
						idw_dw[2].SetItem(al_tranrow,"recipient_sub_type_code",ls_recipient_sub_type)  /* March 27, 1998: set recipient sub-type code */
					END IF
					Return 1
	   		END IF
			END IF  
	END CHOOSE	
Return 0
end function

public function integer nf_delete_recipient ();INTEGER		li_rtn

IF idw_dw[2].RowCount() = 1 Then
	MessageBox('Delete','You cannot delete the only recipient')
	return -1
End if

IF Not IsNull(idw_dw[1].GetItemDateTime(1,'processed_date')) Then
	MessageBox('Delete','You cannot delete a recipient once the payment has been processed')
	return -1
End if

IF idw_dw[2].GetItemNumber(1,'batch_no') <> 0 Then
	MessageBox('Delete','You cannot delete a recipient once the payment has been accepted into a batch.')
	return -1
End if

li_rtn = MessageBox('Delete','Do you really want to delete this recipient?',Question!,YesNo!,2)
IF li_rtn = 2 Then RETURN -1

idw_dw[2].Deleterow(0)

return 1
end function

public function integer nf_delete_payment ();LONG 	ll_result, ll_recipient_cntr, ll_nmbr_recipients, ll_rownum, ll_payment_no
STRING  	ls_claim_admin_region
STRING	ls_authorization_type
STRING	ls_payment_type

ls_payment_type = idw_dw[1].GetItemString(idw_dw[1].GetRow(),'payment_type_code')
ll_payment_no = idw_dw[1].GetItemNumber(idw_dw[1].GetRow(),'payment_no')
ls_claim_admin_region = idw_basic_claim.GetItemString(1,'admin_region_code')
ls_authorization_type = gnv_user_authorizations.nf_get_authorization_type(ls_payment_type)

IF gnv_user_authorizations.nf_authorizations_exist(ls_claim_admin_region,ls_authorization_type) = False Then
	MessageBox('Deleting','You do not have the proper Authorizations to delete this payment.')
	RETURN -1
END IF

	ll_recipient_cntr = 1
	ll_nmbr_recipients = idw_dw[2].RowCount()

/*	the data window used below:
	
	idw_dw[2] = dw_transaction_details
	idw_dw[1] = dw_payment_details
*/
	IF idw_dw[1].RowCount() < 1 THEN
		MessageBox('Warning', 'No payements left to delete.')
		Return -1
	END IF

	ll_recipient_cntr = 1
	DO WHILE ll_recipient_cntr <= ll_nmbr_recipients
		idw_dw[2].DeleteRow(1)
		ll_recipient_cntr++
	LOOP




SQLCA.nf_begin_transaction()


	DELETE RTW_INCENTIVE_PAYMENT_XREF
	WHERE payment_no = :ll_payment_no
	USING SQLCA;
	
	SQLCA.nf_handle_error("n_payment","Embedded SQL: Delete RTW_INCENTIVE_PAYMENT_XREF records","nf_delete")

	idw_dw[1].DeleteRow(1)

	idw_dw[1].Update()
	IF SQLCA.nf_handle_error("Payment details update","n_payments","nf_delete") < 0 THEN
		Return -1
	END IF

	idw_dw[2].Update()
	IF SQLCA.nf_handle_error("Transactions update","n_payments","nf_delete") < 0 THEN
		Return -1
	END IF

SQLCA.nf_commit_transaction()


Return 0
end function

public function integer nf_validate_payment_type (string as_payment_type_code, ref integer ai_error_level, ref string as_error_message, ref string as_award_freq_code, ref string as_days_required, ref string as_dates_required);DECIMAL          lc_benefit_level_percentage
INTEGER	        li_rtn
STRING           ls_opening_type_code
LONG             ll_rownum
DATAWINDOWCHILD  ldwc_child

/* Function Name: nf_validate_payment_type                                                              
	Purpose:			The main purpose is to determine wether or not the claimant is entitled to benefits   
						for a certain payment type                                                           
						NOTE: The messages are not displayed by this function as it is called for two       
						different reasons:                                                              
						1. From wf_setup_default, it is used to determine if a particular default      
							payment type is allowed. Error messages are not displayed by this function.  
						2. From the ItemChanged Event of the data window (dw_payment_details), it is  
							used to validate that the payment type selected is valid for the current     
							claim.  The Error Messages will be displayed when the control is returned to 
							this script.                                                                
                                                                                                     
						NOTE: When a level 1 error is encountered, the message is added to l_error_message and
								the function continues with the validations. This will allow the user to see all
								warnings associated with the payment type.                                      
	Arguments:	as_payment_type_code- The payment category (ie "RLOE", "VR", or "LTD")               
					as_error_level      - 0 - Payment Allowed                                           
					1 - Payment Allowed but warning should be displayed        
					2 - Payment Not Allowed                                  
					as_error_message    - The message to be displayed                               
	Return Values:                                                                                  
*/

	ai_error_level      = 0
	as_error_message    = " "

	IF idw_dw[1].dataobject = 'd_payment_details' THEN
		li_rtn = idw_dw[1].GetChild('payment_type_and_sub_type', ldwc_child)
	ELSE
		li_rtn = idw_dw[1].GetChild('payment_type_code', ldwc_child)
	END IF
	ll_rownum = ldwc_child.Find("payment_type_code = '" + as_payment_type_code + "'",1, ldwc_child.RowCount())
	IF ll_rownum <= 0 or IsNull(ll_rownum) THEN
		ai_error_level = 2
		as_error_message = "Payment Type selected is not a valid Payment Type"
		Return -1
	ELSE
		ls_opening_type_code          = ldwc_child.GetItemString(ll_rownum,"benefit_category_code")
		lc_benefit_level_percentage   = ldwc_child.GetItemDecimal(ll_rownum,"benefit_level_percentage")
		as_dates_required             = ldwc_child.GetItemString(ll_rownum,'fromto_dates_flag')
		as_days_required              = ldwc_child.GetItemString(ll_rownum,'days_hours_flag')
	END IF
	
/*	VALIDATE according to payment category                                                            
*/
	CHOOSE CASE ls_opening_type_code

	CASE "RLOE"

// change
		IF as_payment_type_code = "16" Then
			IF Year(Date(idw_basic_claim.GetItemDateTime(1,'accident_date'))) > 1981 THEN
				ai_error_level = 2
				as_error_message = "Pre 82 payment code for post 81 claim"
				Return -1
			END IF
		END IF

	CASE "LTD"
		IF (as_payment_type_code = ".6" or as_payment_type_code = " 6" or as_payment_type_code = "-6") and &
			(idw_basic_claim.GetItemNumber(1,'cost_alloc_no') = 10002 or &
	       idw_basic_claim.GetItemNumber(1,'cost_alloc_no') = 65009  or &
   	    idw_basic_claim.GetItemNumber(1,'cost_alloc_no') = 3009 or &
      	 idw_basic_claim.GetItemNumber(1,'cost_alloc_no') = 5006) THEN
			ai_error_level = 2
			as_error_message = "Claim has an invalid employer number - " + String(idw_basic_claim.GetItemNumber(1,'cost_alloc_no')) + " for Short-Term Disability payments."
			Return -1
		END IF

	END CHOOSE

Return 0


end function

public function integer nf_validate_address ();DATAWINDOWCHILD	ldwc_child
LONG					ll_row, ll_rows
STRING				ls_string,ls_address_one,ls_address_two,ls_postal_code

ll_rows = idw_dw[2].RowCount()
FOR ll_row = 1 TO ll_rows
	ls_string = idw_dw[2].GetItemString(ll_row,'prov_state_code')
	IF Trim(ls_string) > '' THEN
		idw_dw[2].GetChild('prov_state_code', ldwc_child)
		ll_row = ldwc_child.Find('location_code = "'+ ls_string + '"',0,ldwc_child.RowCount())
		IF ll_row <= 0 THEN
			MessageBox('Invalid Prov/State','The province/state code entered is invalid.')
			idw_dw[2].SetColumn('prov_state_code')
			idw_dw[2].SetFocus()
			Return -1
		END IF
	END IF
NEXT
	
/* cant put this code aboce cus it's whacky so put it here! */
FOR ll_row = 1 TO ll_rows
	 ls_address_one = idw_dw[2].GetItemString(ll_row,'address_line1')
	 ls_address_two = idw_dw[2].GetItemString(ll_row,'address_line2')
	 ls_postal_code = idw_dw[2].GetItemString(ll_row,'postal_code')
	
	 /* ensure that at least on line in the address has been saved */
	 IF trim(ls_address_one) = "" and trim(ls_address_two) = "" THEN
		 MessageBox('Invalid Address','The Address portion must be included.')	
		 idw_dw[2].scrolltorow(ll_row)
		 idw_dw[2].SetColumn('address_line1')
		 idw_dw[2].SetFocus()
		 Return -1
	 END IF 

	 /* postal code must be there even if the use_default_address_flag = "Y" */
	 IF trim(ls_postal_code) = "" OR ISNULL(ls_postal_code) THEN
		 MessageBox('Invalid Postal Code','Postal Code is Mandatory and may be added through the applicable maintain screen')	
		 idw_dw[2].scrolltorow(ll_row)
		 idw_dw[2].SetColumn('postal_code')
		 idw_dw[2].SetFocus()
		 Return -1
	 END IF 
	 
	 /* address line number two now equals address line number 1 is this correct???? */
	IF TRIM(ls_address_one) = "" and trim(ls_address_two) <> "" THEN
		idw_dw[2].SetItem(ll_row,"address_line1",ls_address_two)
		idw_dw[2].SetItem(ll_row,"address_line2","")
	END IF
NEXT
	

Return 0
end function

public function integer nf_setup_address (string as_recipient_type_code, long al_recipient_no, long al_txn_row, string as_cheque_print_group_code);LONG             ll_tranrow
STRING				ls_address_line1, ls_address_line2
STRING           ls_care_of, ls_recipient_street, ls_city, ls_province
STRING           ls_country, ls_postal_code, ls_recipient_name
STRING				ls_country_desc

/* Function Name: nf_setup_address                                                                      
	
	This function should be called in the following cases
		1) when a new transaction record is created
		2) from the item changed event for
				a) use_default_address_flag is set from 'N' to 'Y'
				b) payment_sub_type_code
				c) recipient_no
*/

	IF al_txn_row = 0 THEN
		ll_tranrow = idw_dw[2].GetRow()
	ELSE
		ll_tranrow = al_txn_row
	END IF

/* Only need to do if payment not yet made
*/
	IF idw_dw[2].GetItemString(ll_tranrow, 'source_table_code') = 'A' THEN
		Return 0
	END IF
	
	/* for simplicity purposes if the recipient_type_code  = "I" 
   and their is no recipient no then set the address information to nothing
*/
IF al_recipient_no = 0 THEN
	ls_recipient_name = ""
   ls_city           = ""
   ls_province	      = ""
   ls_postal_code    = ""
	ls_country_desc   = ""
	idw_dw[2].SetItem(ll_tranrow,"address_line2","")
	idw_dw[2].SetItem(ll_tranrow,"address_line1","")
	//set the recipient type_code = "" so it doesn't get picked
	//up by the case statement - it's not brought in by reference
   as_recipient_type_code = ""
END IF 
	
CHOOSE CASE as_recipient_type_code
	CASE "I"
			
		SELECT given_names + ' ' + last_name, address_line1, address_line2,   
				 city, prov_state_code, country_code, postal_code  
		  INTO :ls_recipient_name, :ls_address_line1, :ls_address_line2, :ls_city, 
				 :ls_province, :ls_country, :ls_postal_code  
		  FROM INDIVIDUAL
		 WHERE ( individual_no = :al_recipient_no ) 
		 USING SQLCA ;
		
		SQLCA.nf_handle_error("Embedded SQL: Retrieve on INDIVIDUAL","n_payment","nf_setup_address")
			
		IF SQLCA.SQLNRows <> 1 Then SignalError(-666,'Error selecting individual address information.')
		
		//If the payment is "Transportation/Accommodation/All Prgms", this payment will be
		//sent to the rehab center for the claimant
		If as_cheque_print_group_code = 'W' Then
			idw_dw[2].SetItem(ll_tranrow,"address_line1","C/O WORKER'S REHAB CENTRE")
			idw_dw[2].SetItem(ll_tranrow,"address_line2",ls_address_line1)
		Else
			idw_dw[2].SetItem(ll_tranrow,"address_line1",ls_address_line1)
			idw_dw[2].SetItem(ll_tranrow,"address_line2",ls_address_line2)
		End if
		
	CASE ELSE
/*		If recipient is a payee, read the address from Service Providers table          
*/
		IF IsNull(as_recipient_type_code) OR IsNull(al_recipient_no) THEN
			MessageBox('Warning','Recipient number or type is missing')
			Return -1
		END IF
		
		IF al_recipient_no > 0 THEN
		
			SELECT PROVIDER.name, PROVIDER.address_line1, PROVIDER.address_line2,   
					 PROVIDER.city, PROVIDER.prov_state_code, PROVIDER.country_code, PROVIDER.postal_code  
			  INTO :ls_recipient_name, :ls_address_line1, :ls_address_line2, :ls_city, 
					 :ls_province, :ls_country, :ls_postal_code  
			  FROM PROVIDER  
			 WHERE ( PROVIDER.provider_no = :al_recipient_no ) AND  
					 ( PROVIDER.provider_type_code = :as_recipient_type_code) AND
					 ( PROVIDER.active_flag = 'Y' )  
			 USING SQLCA;
				 
			SQLCA.nf_handle_error("Embedded SQL: Retrieve on SERVICE_PROVIDER","n_payment","nf_setup_address")
				
			IF SQLCA.SQLNRows <> 1 Then 
				//SignalError(-666,'Error selecting PROVIDER address information.')
				MessageBox('Invalid recipient','No active Providers were found.  Select another.')
				return -1
			End if
			
			/*  If the provider type is Other or Voc Rehab and the provide has 
			    only one address line filled in, then the first address line is
				 bumped down to the second line and the claimant’s name is inserted 
				 in the first with the phrase ‘RE:’ in front of it.  If the Other or 
				 Voc Rehab provider has two address lines stored or the provider is a 
				 Medical Aid provider,  then both address lines are stored on the 
				 payment and the claimant’s name is not stored with the address.
			*/
			idw_dw[2].SetItem(ll_tranrow,"address_line2",ls_address_line2)
			idw_dw[2].SetItem(ll_tranrow,"address_line1",ls_address_line1)
			
		END IF 	
	END CHOOSE	

IF TRIM(ls_address_line1) = "" and len(trim(	ls_address_line2)) > 0 THEN
	idw_dw[2].SetItem(ll_tranrow,"address_line1",ls_address_line2)
	idw_dw[2].SetItem(ll_tranrow,"address_line2","")
END IF

	
If idw_dw[2].SetItem(ll_tranrow,"recipient_name",ls_recipient_name) <> 1 Then SignalError(-666,'Error setting recipient name')
if idw_dw[2].SetItem(ll_tranrow,"city",ls_city)  <> 1 Then SignalError(-666,'Error setting recipient name')
if idw_dw[2].SetItem(ll_tranrow,"prov_state_code",ls_province)	 <> 1 Then SignalError(-666,'Error setting recipient name')		
if idw_dw[2].SetItem(ll_tranrow,"postal_code",ls_postal_code)  <> 1 Then SignalError(-666,'Error setting recipient name')

SELECT location_desc1
  INTO :ls_country_desc
  FROM Location
 WHERE location_code = :ls_country
	AND location_type_code = 'C';

SQLCA.nf_handle_error('Embedded SQL: Select from Location','n_payment','nf_setup_address')
IF SQLCA.SQLNRows <> 1 Then SignalError(-666,'Error selecting country description.')
			
if idw_dw[2].SetItem(ll_tranrow,"country",ls_country_desc)  <> 1 Then SignalError(-666,'Error setting recipient name')

Return 0
end function

public function string nf_get_authorization_filter ();LONG 		ll_counter
STRING	ls_filter
BOOLEAN	lb_add_and
STRING	ls_admin_region

ls_admin_region = idw_basic_claim.GetItemString(1,'admin_region_code')

ls_filter = '('

if gnv_user_authorizations.nf_authorizations_exist(ls_admin_region,"loe") Then	
	ls_filter += 'authorization_type_code = "loe" '
	lb_add_and = TRUE
END IF

	
//SR70 - Added new authorization Code for Special Survivor Payments - SMANZER - JAN 2001
if gnv_user_authorizations.nf_authorizations_exist(ls_admin_region,"ssp") Then	
	IF lb_add_and THEN
		ls_filter += ' OR '
	End if		
	ls_filter += ' authorization_type_code = "ssp" '
	lb_add_and = TRUE		
END IF

//Check if the user has a pension authorization
if gnv_user_authorizations.nf_authorizations_exist(ls_admin_region,"pen") Then	
	IF lb_add_and THEN
		ls_filter += ' OR '
	End if
	ls_filter += ' authorization_type_code = "pen"'		
END IF

IF ls_filter > '' THEN
	ls_filter +=  ')'
END IF

Return ls_filter
end function

public function integer nf_check_mandatory ();DATAWINDOWCHILD  ldwc_child,ldwc_payment
LONG             ll_current_row, ll_result, ll_transaction_rowcount, ll_row, ll_individual, ll_claimno,ll_recipientno, ll_days_after_1800
STRING           ls_payment_method_code, ls_string, ls_pay_type, ls_ben_cat,ls_message, ls_payment_type_code
STRING 			  ls_ClaimantType, ls_courtOrder, ls_day,ls_month,ls_year, ls_opening, ls_legistration, ls_dependant
BOOLEAN			  lb_check
DATETIME 		  ld_birth, ld_paidto
DATE 				  ld_monthend
Integer 			  li_rtn,li_age
STRING				ls_payment_sub_type_code


ll_claimno = il_claim_no
	IF idw_dw[1].AcceptText() < 0 THEN Return -1		// payment details
	IF idw_dw[2].AcceptText() < 0 THEN Return -1		// transaction details
	
	
	ll_current_row = idw_dw[1].GetRow()
	
   ls_payment_type_code = idw_dw[1].GetItemString(ll_current_row ,"payment_type_code")
	ls_payment_sub_type_code = idw_dw[1].GetItemString(ll_current_row ,"payment_sub_type_code")

	IF idw_basic_claim.GetItemString(1,'claim_history_flag') = 'Y' THEN
		MessageBox('History Claim','This claim is a history claim.  No payments can be made until the claim information is verified.  Go to claim maintenance to verify.')
		Return -1
	END IF
		
	IF ll_current_row > 0 THEN
		
	
		IF IsNull(idw_dw[1].GetItemNumber(ll_current_row,'nmbr_cycles')) OR  idw_dw[1].GetItemNumber(ll_current_row,'nmbr_cycles') < 1 THEN
			MessageBox('Invalid Number of Cylces', 'The number of cycles must be at least one.')
			idw_dw[1].SetFocus()
			idw_dw[1].SetColumn('nmbr_cycles')
			Return -1
		END IF

		IF IsNull(idw_dw[1].GetItemDecimal(ll_current_row,'total_award_amount')) THEN
			MessageBox('Missing Award Amount', 'A value must be specified for the award amount.')
			idw_dw[1].SetFocus()
			idw_dw[1].SetColumn('total_award_amount')
			Return -1
		END IF

		IF IsNull(idw_dw[1].GetItemString(ll_current_row,"payment_type_code")) OR Trim(idw_dw[1].GetItemString(ll_current_row,"payment_type_code")) = ""  THEN
			MessageBox("Payment Module - Validation Error","Payment Type is required.",Exclamation!)
			idw_dw[1].SetFocus()
			idw_dw[1].SetColumn("payment_type_code")
			Return -1
		END IF

		ls_pay_type = idw_dw[1].GetItemString(ll_current_row,"payment_type_code")

		IF idw_dw[1].dataobject = 'd_payment_details' THEN
			//IF in the PAYMENT MAINTENANCE MODULE
			IF (idw_basic_claim.GetItemString(1,'claim_status_code') = 'F' AND (idw_basic_claim.GetItemString(1,'claim_status_type_code')= '03' OR idw_basic_claim.GetItemString(1,'claim_status_type_code') = '04')) THEN
				IF ls_payment_type_code <> '.4' THEN
					MessageBox('Invalid Payment Type Code','The payment type must be a Loss of Opportunity Lump Sum (.4) payment for a claim that has a status of Finalled/No Lost Time or Finalled/Lost Time Medical Aid Only')
					RETURN -1
				END IF
			END IF
			
			li_rtn = idw_dw[1].GetChild('payment_type_and_sub_type', ldwc_child)
			
		ELSE
			li_rtn = idw_dw[1].GetChild('payment_type_code', ldwc_child)
		END IF
		ll_result = ldwc_child.Find("payment_type_code = '" + ls_pay_type + "'",1, ldwc_child.RowCount())

		IF ll_result > 0 THEN
			ls_ben_cat = ldwc_child.GetItemString(ll_result,"benefit_category_code") 
			IF ldwc_child.GetItemString(ll_result,"fromto_dates_flag") = "Y" THEN
				//	If the payment type requires from/to dates, check that they are entered
				IF IsNull(idw_dw[1].GetItemDateTime(ll_current_row,"paid_from_date")) OR string(idw_dw[1].GetItemDateTime(ll_current_row,"paid_from_date")) = "0000 01 01 00:00:00" THEN
					MessageBox("Payment Module - Validation Error","Paid From Date is required for this type of payment",Exclamation!)
					idw_dw[1].SetFocus()
					idw_dw[1].SetColumn("paid_from_date")
					Return -1
				END IF
				IF IsNull(idw_dw[1].GetItemDateTime(ll_current_row,"paid_to_date")) OR string(idw_dw[1].GetItemDateTime(ll_current_row,"paid_to_date")) = "0000 01 01 00:00:00" THEN
					MessageBox("Payment Module - Validation Error","Paid To Date is a required field",Exclamation!)
					idw_dw[1].SetFocus()
					idw_dw[1].SetColumn("paid_to_date")
					Return -1
				END IF
			END IF
		ELSE
			MessageBox('Invalid Payment Type Code', 'You do not have the proper Authorizations to maintain this payment.')
			Return -1
		END IF

		IF IsNull(idw_dw[1].GetItemDateTime(ll_current_row,"scheduled_processing_date")) OR string(idw_dw[1].GetItemDateTime(ll_current_row,"scheduled_processing_date")) = "0000 01 01 00:00:00" THEN
			MessageBox("Payment Module - Validation Error","Order Date is a required field",Exclamation!)
			idw_dw[1].SetFocus()
			idw_dw[1].SetColumn("scheduled_processing_date")
			Return -1
		END IF
	END IF
/*	validate the transactions
*/

	ll_current_row = 1
	ll_transaction_rowcount = idw_dw[2].RowCount()
	DO WHILE ll_current_row <= ll_transaction_rowcount


		/*SR 4 - Court Order Split Payment Warning Begin*/
		Choose Case ls_ben_cat 
			case "RLOE","LTD","VR","PEN"
				
				SELECT court_order_flag 
				INTO :ls_courtOrder
				FROM INDIVIDUAL 
				where individual_no = :ll_individual
				USING SQLCA;
				IF ls_courtOrder = 'Y' and ll_transaction_rowcount < 2 then
					IF messagebox("Payment Module","This individual has a court order flag set to true. Do you wish to continue without spliting the payment?",question!,yesno!,2) = 2 THEN
						idw_dw[2].ScrollToRow(ll_current_row)
						idw_dw[2].SetFocus()
						idw_dw[2].SetColumn("recipient_type_code")
						Return -1
					END IF
				END IF
		END Choose
/*SR 4 - Court Order Split Payment Warning End*/
/* Ed Lenarczyk  - begin changes  July 29, 1999
		Make sure that only "I" or "M" recipient type is selected for all
		Medical Aid  (benefit_category_code = MA) payments
*/
		IF (ls_ben_cat = "MA" AND &   
			NOT (idw_dw[2].GetItemString(ll_current_row,"recipient_type_code") = "I" OR &
			idw_dw[2].GetItemString(ll_current_row,"recipient_type_code") = "O" OR idw_dw[2].GetItemString(ll_current_row,"recipient_type_code") = "M"))  THEN
			messagebox("Payment Module - Validation Error","Recipient type for this payment type has to be an Individual, Medical Aid Provider, or Other Payee.")
			idw_dw[2].ScrollToRow(ll_current_row)
			idw_dw[2].SetFocus()
			idw_dw[2].SetColumn("recipient_type_code")
			Return -1			
		END IF
/* Ed Lenarczyk  - end changes  July 29, 1999
*/		
		
		ls_payment_method_code  = idw_dw[2].GetItemString(ll_current_row,"payment_method_code")

/*		Recipient number is mandatory
*/
		IF IsNull(idw_dw[2].GetItemNumber(ll_current_row,"recipient_no")) THEN
			MessageBox("Payment Module - Validation Error","Recipient number is a required field",Exclamation!)
			idw_dw[2].ScrollToRow(ll_current_row)
			idw_dw[2].SetFocus()
			idw_dw[2].SetColumn("recipient_no")
			Return -1
		END IF

//Only do checks if there is a birth date.
		ll_recipientno = idw_dw[2].GetItemNumber(ll_current_row,"recipient_no")
		
		SELECT ISNULL(birth_date,'01/01/1800')
		INTO :ld_birth 
		FROM INDIVIDUAL
		WHERE individual_no = :ll_recipientno
		USING SQLCA;
		
		li_rtn = SQLCA.nf_handle_error('Embedded SQL: select from INDIVIDUAL','n_payment','nf_check_mandatory')
		
		SELECT top 1 DATEDIFF(dd,'1800-01-01',:ld_birth)
		INTO :ll_days_after_1800
		FROM sysobjects
		USING SQLCA;
		
		li_rtn = SQLCA.nf_handle_error('Embedded SQL: select datediff from sysobjects','n_payment','nf_check_mandatory')
		
		IF ll_days_after_1800 > 0 and idw_dw[2].GetItemString(ll_current_row,"recipient_type_code") = 'I' THEN
			SELECT claim_role_code ,dependent_reason_code
			INTO :ls_claimantType,:ls_dependant
			FROM CLAIM_PARTICIPANT
			WHERE individual_no = :ll_recipientno
			AND claim_no = :il_claim_no;	
			ld_paidto = idw_dw[1].GetItemDateTime(1,"paid_to_date")
			ls_day = '01'
			ls_year = string(YEAR(date(ld_paidto)))
			IF MONTH(date(ld_birth)) + 1 = 13 then
				ls_month = '01'
				ls_year = string(integer(ls_year) + 1)
			else
				ls_month = string(MONTH(date(ld_birth))+ 1)
			end if
			
			ld_monthend = date(ls_year + '/' + ls_month  + '/' + ls_day )
			ld_monthend = relativedate(ld_monthend,-1)
				
			li_age = integer((daysafter(date(ld_birth),ld_monthend)/365.25))
			li_rtn = idw_dw[1].GetChild("opening_no",ldwc_payment)
			ls_opening = ldwc_payment.GetItemString(ldwc_payment.getrow(),"opening_type_code")
			CHOOSE CASE ls_claimantType
				case 'C'
					if ls_opening = 'RLOE' or ls_opening = 'LTD' then
						if li_age >= 65 then
							if messagebox("Payment Warning","The age of this individual is greater than 65. Do you wish to continue?",Question!,yesno!,2) = 2 then 
								idw_dw[2].ScrollToRow(ll_current_row)
								idw_dw[2].SetFocus()
								idw_dw[2].SetColumn("cheque_no")
								Return -1
							end if
						end if
					end if
				case 'SS'
					SELECT legislation_code
					INTO :ls_legistration
					FROM CLAIM
					WHERE claim_no = :ll_claimno; 
					if ls_opening = 'S1' or ls_opening = 'S2' or ls_opening = 'SV' then
						IF ls_legistration = 'P81' or ls_legistration = 'P97' then	
							if li_age >= 65 then
								if messagebox("Payment Warning","The age of this individual is greater then 65. Do you wish to continue?",Question!,yesno!,2) = 2 then 
									idw_dw[2].ScrollToRow(ll_current_row)
									idw_dw[2].SetFocus()
									idw_dw[2].SetColumn("cheque_no")
									Return -1
								end if
							end if
						end if
					end if
				case 'DC'
					SELECT legislation_code
					INTO :ls_legistration
					FROM CLAIM
					WHERE claim_no = :ll_claimno; 
					
					if ls_opening = 'S1' or ls_opening = 'S2' or ls_opening = 'SV' then
						IF (ls_legistration = 'P81' or ls_legistration = 'P97') and ls_dependant <> 'DIS' then	
							if li_age >= 22 then
								if messagebox("Payment Warning","The age of this dependant is greater then 22. Do you wish to continue?",Question!,yesno!,2) = 2 then 
									idw_dw[2].ScrollToRow(ll_current_row)
									idw_dw[2].SetFocus()
									idw_dw[2].SetColumn("cheque_no")
									Return -1
								end if
							end if
						end if
					end if
			END CHOOSE
		
		// ls_pay_type //payment type code
		// ls_ben_cat  //Beneifit category
		END IF

/*		IF h/w cheque, cheque no, requisition no and issue date are mandatory
*/
		IF ls_payment_method_code = "H" THEN
			IF IsNull(idw_dw[2].GetItemDateTime(ll_current_row,"cheque_deposit_date")) THEN
				MessageBox("Payment Module - Validation Error","Issue Date is a required field for Handwritten Cheques",Exclamation!)
				idw_dw[2].ScrollToRow(ll_current_row)
				idw_dw[2].SetFocus()
				idw_dw[2].SetColumn("cheque_deposit_date")
				Return -1
			END IF
			IF IsNull(idw_dw[2].GetItemNumber(ll_current_row,"cheque_no")) or idw_dw[2].GetItemNumber(ll_current_row,"cheque_no") = 0 THEN
				MessageBox("Payment Module - Validation Error","Cheque Number is a required field for Handwritten Cheques",Exclamation!)
				idw_dw[2].ScrollToRow(ll_current_row)
				idw_dw[2].SetFocus()
				idw_dw[2].SetColumn("cheque_no")
				Return -1
			END IF

/*		IF h/w cheque, cheque no, requisition no and issue date are mandatory
		...'When creating a handwritten cheque transaction, the cheque number
		must be 
		(1) - greater than the minimum handwritten cheque number range and less
			   than the maximum handwritten cheque number range
		(2) - the handwritten cheque number must not already exist on the CHEQUE_HEADER
				table or on any UNAPPLIED_CLAIM_TXN records - The validation should be done
				when the user enters the handwritten cheque.'
				function returns -1(fail),-2 fails validation on min/max ranges,-3 found in 
				cheque header, - 4 found in unapplied_claim_txn 
				
				added aug/1999 - jw
				
SR70  (3) - The handwritten Cheque can exist when the payment is a Special Survivor Payment as some cheques were issued
            manually before the system was automated. JAN 2001 - SMANZER
*/
			/* only do this check on new or changed cheque numbers */
			IF idw_dw[2].GetItemStatus(ll_current_row, 0, Primary!) = NewModified! OR &
				idw_dw[2].GetItemNumber(ll_current_row, 'cheque_no', Primary!, TRUE) <> idw_dw[2].GetItemNumber(ll_current_row, 'cheque_no') THEN
				ll_result = nf_check_hand_cheque_range(idw_dw[2].GetItemnumber(ll_current_row,"cheque_no"),lb_check)
				CHOOSE CASE ll_result
					CASE -1
						ls_message = "The application failed to retrieve the correct information for this claim number."
					CASE -2
						ls_message = "The cheque number entered does not meet the min/max criteria, please enter another number."
					CASE -3,-4
						IF ls_payment_type_code <> 'S1' THEN
							ls_message = "The cheque number entered is a duplicate, please enter another number."
						ELSE
							ll_result = 0 
							lb_check = TRUE
							MessageBox("Payment","WARNING: The cheque number is a duplicate.",Information!)
						END IF	
				END CHOOSE		
								
				IF ll_result < 0 OR lb_check = FALSE THEN
					MessageBox("Payment Module - Validation Error",ls_message,Exclamation!)
					idw_dw[2].ScrollToRow(ll_current_row)
					idw_dw[2].SetFocus()
					idw_dw[2].SetColumn("cheque_no")
					Return -1
				END IF
			END IF

			IF IsNull(idw_dw[2].GetItemNumber(ll_current_row,"manual_cheque_req_no")) or idw_dw[2].GetItemNumber(ll_current_row,"manual_cheque_req_no") = 0 THEN
				MessageBox("Payment Module - Validation Error","Requisition Number is a required field for Handwritten Cheques",Exclamation!)
				idw_dw[2].ScrollToRow(ll_current_row)
				idw_dw[2].SetFocus()
				idw_dw[2].SetColumn("manual_cheque_req_no")
				Return -1
			END IF
		END IF

/*		From old comp: IF payment method is h/w cheque or automated cheque, City, Province, Country and Postal Code is mandatory
							Note: this edit will be replaced by appropriate address handling
*/
		IF (ls_payment_method_code = "H" or ls_payment_method_code = "A")  AND &
			idw_dw[2].GetItemString(ll_current_row,"use_default_address_flag") = 'N' THEN
			IF IsNull(idw_dw[2].GetItemString(ll_current_row,"city")) or Trim(idw_dw[2].GetItemString(ll_current_row,"city")) = "" THEN
				MessageBox("Payment Module - Validation Error","City is mandatory when payment method is handwritten or automated cheque",Exclamation!)
				idw_dw[2].ScrollToRow(ll_current_row)
				idw_dw[2].SetFocus()
				idw_dw[2].SetColumn("city")
				Return -1
			END IF
			idw_dw[2].GetChild('country', ldwc_child)
			ls_string = idw_dw[2].GetItemString(ll_current_row, 'country')
			ll_row = ldwc_child.Find('location_desc1 = "' + ls_string + '"',0,ldwc_child.RowCount())
/*			get the country code
*/
			IF ll_row > 0 THEN
				ls_string = ldwc_child.GetItemString(ll_row,'location_code')
			ELSE
				ls_string = ''
			END IF
/*			look up the code in the province look up
*/
			idw_dw[2].GetChild('prov_state_code', ldwc_child)
			ll_row = ldwc_child.Find('country_code = "' + ls_string + '"',0,ldwc_child.RowCount())
			IF ll_row > 0 THEN
				IF IsNull(idw_dw[2].GetItemString(ll_current_row,"prov_state_code")) or Trim(idw_dw[2].GetItemString(ll_current_row,"prov_state_code")) = "" THEN
					MessageBox("Payment Module - Validation Error","Province is mandatory when payment method is handwritten or automated cheque",Exclamation!)
					idw_dw[2].ScrollToRow(ll_current_row)
					idw_dw[2].SetFocus()
					idw_dw[2].SetColumn("prov_state_code")
					Return -1
				END IF
			ELSE
				IF idw_dw[2].GetItemString(ll_current_row,"prov_state_code") > '' THEN
					MessageBox("Payment Module - Validation Error","Province must not have a value for current country.",Exclamation!)
					idw_dw[2].ScrollToRow(ll_current_row)
					idw_dw[2].SetFocus()
					idw_dw[2].SetColumn("prov_state_code")
					Return -1
				END IF
			END IF
			IF IsNull(idw_dw[2].GetItemString(ll_current_row,"country")) or Trim(idw_dw[2].GetItemString(ll_current_row,"country")) = "" THEN
				MessageBox("Payment Module - Validation Error","Country is mandatory when payment method is handwritten or automated cheque",Exclamation!)
				idw_dw[2].ScrollToRow(ll_current_row)
				idw_dw[2].SetFocus()
				idw_dw[2].SetColumn("country")
				Return -1
			END IF
			IF IsNull(idw_dw[2].GetItemString(ll_current_row,"postal_code")) or Trim(idw_dw[2].GetItemString(ll_current_row,"postal_code")) = "" THEN
				MessageBox("Payment Module - Validation Error","Postal Code is mandatory when payment method is handwritten or automated cheque",Exclamation!)
				idw_dw[2].ScrollToRow(ll_current_row)
				idw_dw[2].SetFocus()
				idw_dw[2].SetColumn("postal_code")
				Return -1
			END IF
		END IF
		ll_current_row++
	LOOP

Return 0
end function

public function integer nf_change_item_2 ();DATAWINDOWCHILD	ldwc_child
DATAWINDOWCHILD	ldwc_child_opening
LONG					ll_no, ll_current_row, ll_result, ll_null,  ll_return, ll_payment_no, ll_txn_no
STRING				ls_recipient_type_code, ls_payment_code,ls_old_payment_code, ls_payment_type_code, ls_payment_sub_type_code
STRING 				ls_bank_no, ls_bank_transit_no, ls_bank_account_no,ls_message
STRING				ls_prov_tax_flag, ls_hst_flag
DATE 					ld_date,	ld_server_dt				
BOOLEAN	 			lb_check, lb_rcpt_hst
INTEGER				li_day_no, li_result
DECIMAL				ld_txn_amount
INTEGER				li_rtn
STRING				ls_cheque_print_group_code 
LONG					ll_opening_no
LONG					ll_opening_row
STRING				ls_opening_type_code


ld_server_dt             = Date(f_server_datetime())
ls_payment_type_code     = idw_dw[1].GetItemString(1,"payment_type_code")
ls_payment_sub_type_code = idw_dw[1].GetItemString(1,"payment_sub_type_code")
ll_payment_no            = idw_dw[1].GetItemNumber(1,"payment_no")

SetNull(ll_null)

ll_current_row   = idw_dw[2].GetRow()
CHOOSE CASE idw_dw[2].GetColumnName()
	CASE "payment_method_code"
		ls_payment_code     = idw_dw[2].GetText()
		ls_old_payment_code = idw_dw[2].GetItemString(ll_current_row,"payment_method_code")
		
		// reset the flags
		idw_dw[2].SetItem(ll_current_row,'use_default_address_flag','Y')

		//SR70 - Special Survivor Payment - JAN 2001 - SMANZER
		//Payment Method must be Automated or Manual Cheque for Special Survivor Payment
		IF ls_payment_type_code = 'S1' AND (ls_payment_code <> 'A' AND ls_payment_code <> 'H') THEN
			MessageBox('Invalid Payment Method','The payment method must be Automated or Hand-Written for a Special Survivor Payment.')
			Return -1
		END IF
		ls_recipient_type_code     = idw_dw[2].GetItemString(ll_current_row,"recipient_type_code")

		ll_opening_no = idw_dw[1].GetItemNumber(1,'opening_no')
		idw_dw[1].GetChild('opening_no',ldwc_child_opening)
		ll_opening_row = ldwc_child_opening.Find('opening_no = ' + String(ll_opening_no),1,ldwc_child_opening.RowCount())
		ls_opening_type_code = ldwc_child_opening.GetItemString(ll_opening_row,'opening_type_code')
		If ls_opening_type_code = '' Then SignalError(-666,'Error finding opening type code')
		//JAMES
		IF ls_payment_code = "R" AND is_claim_receiving_salary_flag <> "Y" THEN
			MessageBox("Payment Module - Validation Error","This payment method may only be used for claims flagged as receiving salary.")
			Return -1
		ELSEIF ls_old_payment_code = "R" AND is_claim_receiving_salary_flag = "Y" and ls_opening_type_code = 'RLOE' THEN
			MessageBox('Invalid Payment Method','The payment method cannot be changed because claim is receiving salary.')
			Return -1
		END IF
		
		 //IF direct deposit selected, ensure that the recipient is the individual and that there is direct 
		// deposit info available
		IF ls_payment_code = "D" THEN
			ll_txn_no = idw_dw[2].GetItemNumber(ll_current_row,'txn_no')
			IF nf_get_bank(idw_dw[2].GetItemNumber(ll_current_row,'recipient_no'),ls_recipient_type_code,ls_bank_no,ls_bank_transit_no, ls_bank_account_no, ll_payment_no,ll_txn_no) < 0 THEN
				MessageBox("Payment Module - Validation Error","Unable to determine Direct Deposit information.",Exclamation!)
				Return -1
			END IF

			IF ls_payment_code = "D" and Trim(ls_bank_no) = "" THEN
				MessageBox("Payment Module - Validation Error","There is no direct deposit information for this recipient~r~nYou must select another Payment Method",Exclamation!)
				idw_dw[2].SetItem(ll_current_row,"payment_method_code","A")	
				Return -1
			END IF
		END IF
		
		// IF we have switched to or from Direct Deposit, reset the address and banking information fields 
		IF ls_payment_code = "D" THEN
			//nf_setup_address("",0,0)
			idw_dw[2].SetItem(ll_current_row,'use_default_address_flag', 'Y')
			idw_dw[2].SetItem(ll_current_row,'cheque_print_group_code', ' ')
			idw_dw[2].SetItem(ll_current_row,'bank_no', ls_bank_no)
			idw_dw[2].SetItem(ll_current_row,'bank_transit_no', ls_bank_transit_no)
			idw_dw[2].SetItem(ll_current_row,'bank_account_no',ls_bank_account_no)
		ELSEIF ls_old_payment_code = "D" THEN
			idw_dw[2].SetItem(ll_current_row,'use_default_address_flag', 'Y')
			idw_dw[2].SetItem(ll_current_row,'cheque_print_group_code', ' ')
//			ls_recipient_type_code = idw_dw[2].GetItemString(ll_current_row,"recipient_type_code")
//			ll_no        = idw_dw[2].GetItemNumber(ll_current_row,"recipient_no")
//			IF NOT IsNull(ll_no) AND NOT IsNull(ls_recipient_type_code) THEN
//					nf_setup_address(ls_recipient_type_code,ll_no,0)
//			END IF
		END IF
		
		If ls_payment_code = 'I' and idw_dw[2].GetItemDecimal(ll_current_row,"txn_amount") <> 0 THen
			MessageBox('Invalid payment method','The payment method of "Inapplicable" is only valid for $0.00 payment amounts.')
			return -1
		Elseif idw_dw[2].GetItemDecimal(ll_current_row,"txn_amount") = 0 and ls_payment_code <> 'I' Then
			MessageBox('Invalid payment method','The payment method must be "Inapplicable" when the payment amount is $0.00')
			return -1
		End if

		// IF we have switched to or from handwritten cheque, reset the handwritten cheque fields
		IF ls_payment_code = "H" THEN
			ld_date = ld_server_dt
			idw_dw[2].SetItem(ll_current_row,"cheque_deposit_date",ld_date)
			idw_dw[2].SetItem(ll_current_row,"cheque_no",0)
			idw_dw[2].SetItem(ll_current_row,"manual_cheque_req_no",0)
		ELSEIF ls_old_payment_code = "H" THEN
			SetNull(ld_date)
			idw_dw[2].SetItem(ll_current_row,"cheque_deposit_date",ld_date)
			idw_dw[2].SetItem(ll_current_row,"cheque_no",0)
			idw_dw[2].SetItem(ll_current_row,"manual_cheque_req_no",0)
		END IF

		
		ls_recipient_type_code = idw_dw[2].GetItemString(ll_current_row,'recipient_type_code')
		IF ls_payment_code = 'A' AND ls_recipient_type_code = 'I' AND &
		ls_payment_type_code = '23' AND ls_payment_sub_type_code = '05' THEN
			li_day_no = DayNumber(ld_server_dt)	
			IF li_day_no <= 6 THEN					// Friday
				idw_dw[1].setitem(1,'scheduled_processing_date', DateTime(RelativeDate(ld_server_dt, 6 - li_day_no)))	
			ELSE
				idw_dw[1].setitem(1,'scheduled_processing_date',DateTime(RelativeDate(ld_server_dt, 7 - (li_day_no - 6))))
			END IF
			
			idw_dw[2].SetItem(ll_current_row,'use_default_address_flag','N')
			idw_dw[2].SetItem(ll_current_row,'cheque_print_group_code','W')
			idw_dw[2].SetColumn('cheque_print_group_code')
			nf_change_item_2()
		END IF
		
		idw_dw[2].setfocus()
		

	CASE "recipient_type_code"
		ls_recipient_type_code = idw_dw[2].GetText()
		//SR70 - Special Survivor Payment - SMANZER - JAN 2001
		//If payment type is S1 then the recipient must be the surviving spouse individual
		IF ls_payment_type_code = 'S1' AND ls_recipient_type_code <> 'I' THEN
			MessageBox('Invalid Recipient','The recipient type code must be the surviving spouse individual.')
			Return -1
		END IF
		

		IF ls_recipient_type_code <> "I" and idw_dw[2].GetItemString(ll_current_row,'payment_method_code') = "R" THEN
			MessageBox('Invalid Recipient','The recipient type code cannot be changed because the payment method is receiving salary.')
			Return -1
		END IF
		// Initialize the handwritten cheque fields because when we change the recipient type we always
		// default the payment method to either direct deposit or automated cheque

		
		SetNull(ld_date)
		idw_dw[2].SetItem(ll_current_row,"cheque_deposit_date",ld_date)
		idw_dw[2].SetItem(ll_current_row,"cheque_no",0)
		idw_dw[2].SetItem(ll_current_row,"manual_cheque_req_no",0)

		// SR70 - Special Survivor Payment
		IF ls_payment_type_code = 'S1' THEN
			idw_dw[2].SetTabOrder("recipient_no",20)
			ll_return = nf_set_surv_recipient_filter()
		ELSE
			nf_tab_order("recipient_type_code",ls_recipient_type_code)
		END IF
		
		// IF the recipient type has been changed to individual, default the banking info and address fields
		// PR 2660 - AND the scheduled date!
		IF ls_recipient_type_code  = "I" THEN
			// always defaults to the claimant unless the payment type code is S1 - For Special Survivor
			nf_set_recipient_filter(FALSE)
			
			idw_dw[2].SetItem(ll_current_row,"recipient_no",idw_basic_claim.GetItemNumber(1,'individual_no'))
			idw_dw[2].SetItem(ll_current_row,"recipient_name",idw_basic_claim.GetItemString(1,'given_names') + ' ' + idw_basic_claim.GetItemString(1,'last_name'))
			ll_txn_no = idw_dw[2].GetItemNumber(ll_current_row,'txn_no')
			
			IF nf_get_bank(idw_dw[2].GetItemNumber(ll_current_row,'recipient_no'),ls_recipient_type_code,ls_bank_no,ls_bank_transit_no, ls_bank_account_no, ll_payment_no,ll_txn_no) < 0 THEN
				MessageBox("Payment Module - Validation Error","Unable to determine Direct Deposit information.",Exclamation!)
				Return -1
			END IF
			
			IF Trim(ls_bank_no) <> '' THEN
				idw_dw[2].SetItem(ll_current_row,'bank_no', ls_bank_no)
				idw_dw[2].SetItem(ll_current_row,'bank_account_no', ls_bank_account_no)
				idw_dw[2].SetItem(ll_current_row,'bank_transit_no', ls_bank_transit_no)
			END IF
			
			//If the txn_amount is zero, the payment_method_code will be 'I'
			//and we don't want to change it
			IF Trim(ls_bank_no) = '' THEN
				idw_dw[2].SetItem(ll_current_row,"payment_method_code","A")				
			ELSE
				idw_dw[2].SetItem(ll_current_row,"payment_method_code","D")
			END IF
			
			idw_dw[1].setitem(1, 'scheduled_processing_date',ld_server_dt)
			
			IF ls_payment_type_code = '23' AND ls_payment_sub_type_code = '05' THEN
				ls_payment_code = idw_dw[2].GetItemString(ll_current_row,"payment_method_code")
				IF ls_payment_code = 'A' THEN
					li_day_no = DayNumber(ld_server_dt)	
					IF li_day_no <= 6 THEN					// Friday
						idw_dw[1].setitem(1,'scheduled_processing_date', DateTime(RelativeDate(ld_server_dt, 6 - li_day_no)))	
					ELSE
						idw_dw[1].setitem(1,'scheduled_processing_date',DateTime(RelativeDate(ld_server_dt, 7 - (li_day_no - 6))))
					END IF
					idw_dw[2].SetItem(ll_current_row,'use_default_address_flag','N')
					idw_dw[2].SetItem(ll_current_row,'cheque_print_group_code','W')
					idw_dw[2].SetColumn('cheque_print_group_code')
					nf_change_item_2()
				END IF
			END IF
			
			ll_no = idw_dw[2].GetItemNumber(ll_current_row,'recipient_no')
			ls_cheque_print_group_code = idw_dw[2].GetItemString(ll_current_row,"cheque_print_group_code")
			
			nf_setup_address(ls_recipient_type_code,ll_no,ll_current_row,ls_cheque_print_group_code )
			
		// IF the recipient type has been changed to a payee (Voc Rehab or Other), clean out the banking info and address fields
		// until a valid recipient number has been entered
		ELSE
			idw_dw[2].SetItem(ll_current_row,"recipient_no",ll_null)
			idw_dw[2].SetItem(ll_current_row,"recipient_name","")
			idw_dw[2].SetItem(ll_current_row,"payment_method_code","A")		
			idw_dw[2].SetItem(ll_current_row,'bank_no', '')
			idw_dw[2].SetItem(ll_current_row,'bank_account_no', '')
			idw_dw[2].SetItem(ll_current_row,'bank_transit_no', '')
			IF ls_recipient_type_code  <> "M" THEN
				idw_dw[1].setitem(1, 'scheduled_processing_date',ld_server_dt)
			ELSE // medical aid provider
				li_day_no = DayNumber(ld_server_dt)	
				IF li_day_no <= 5 THEN					// Thursday
					idw_dw[1].setitem(1,'scheduled_processing_date', DateTime(RelativeDate(ld_server_dt, 5 - li_day_no)))	
				ELSE
					idw_dw[1].setitem(1,'scheduled_processing_date',DateTime(RelativeDate(ld_server_dt, 7 - (li_day_no - 5))))
				END IF
			END IF
			//PR 3311
			idw_dw[2].SetItem(ll_current_row,"cheque_print_group_code"," ")
			
			nf_setup_address(ls_recipient_type_code,0,0,"" )//
			
			
		END IF
		
		
	CASE "recipient_no"
		ll_no = Long(idw_dw[2].GetText())
		ls_recipient_type_code = idw_dw[2].GetItemString(ll_current_row,"recipient_type_code")
		// Check that a valid recipient number has been entered, and if so, reset the tab order and 
		// default the address
		ll_result = idw_dw[1].GetItemNumber(1,'opening_no')
		idw_dw[1].GetChild('opening_no',ldwc_child)
		ll_result = ldwc_child.Find('opening_no = ' + String(ll_result),1,ldwc_child.RowCount())
		IF ll_result > 0 THEN
			IF (ldwc_child.GetItemString(ll_result,'opening_type_code') <> 'PEN' AND &
				 ldwc_child.GetItemString(ll_result,'opening_type_code') <> 'SV' AND &
				  ldwc_child.GetItemString(ll_result,'opening_type_code') <> 'S1' AND &
					ldwc_child.GetItemString(ll_result,'opening_type_code') <> 'S2') AND &
				ll_no <> idw_basic_claim.GetItemNumber(1,'individual_no') AND &
				idw_dw[2].GetItemString(ll_current_row,'recipient_type_code') = 'I' THEN
				MessageBox('Invalid Recipient No','The only valid "claimant/individual" recipient for this opening can be the claimant.')
				Return -1
			END IF
		END IF
		
		ll_result = nf_setup_address(ls_recipient_type_code,ll_no,0," ")
//		li_rtn = nf_setup_address(ls_recipient_type_code,ll_no,ll_current_row,idw_dw[2].GetItemString(ll_current_row,'cheque_print_group_code'))
		If ll_result = -1 Then Return li_rtn
		
		ll_txn_no = idw_dw[2].GetItemNumber(ll_current_row,'txn_no')
		IF nf_get_bank(ll_no,ls_recipient_type_code,ls_bank_no,ls_bank_transit_no, ls_bank_account_no, ll_payment_no,ll_txn_no) < 0 THEN
			MessageBox("Payment Module - Validation Error","Unable to determine Direct Deposit information.",Exclamation!)
			Return -1
		END IF
//		 default to direct deposit if banking info
		IF ls_bank_no > '' THEN
			idw_dw[2].SetItem(ll_current_row,'bank_no', ls_bank_no)
			idw_dw[2].SetItem(ll_current_row,'bank_account_no', ls_bank_account_no)
			idw_dw[2].SetItem(ll_current_row,'bank_transit_no', ls_bank_transit_no)
			idw_dw[2].SetItem(ll_current_row,'payment_method_code','D')
		ELSE
			idw_dw[2].SetItem(ll_current_row,'bank_no', '')
			idw_dw[2].SetItem(ll_current_row,'bank_account_no', '')
			idw_dw[2].SetItem(ll_current_row,'bank_transit_no', '')
			idw_dw[2].SetItem(ll_current_row,'payment_method_code','A')
		END IF
		
		IF ls_recipient_type_code <> 'I' THEN
			idw_dw[2].SetItem(ll_current_row,'use_default_address_flag','Y') // Only individuals can have 'N'
			idw_dw[2].SetItem(ll_current_row,'cheque_print_group_code',' ')
		END IF

//		li_rtn = nf_setup_address(ls_recipient_type_code,ll_no,ll_current_row,idw_dw[2].GetItemString(ll_current_row,'cheque_print_group_code'))
//		If li_rtn = -1 Then Return li_rtn
		
		IF ll_result < 0 THEN
			return -1
		ELSEIF ll_result = 100 THEN
			IF ls_recipient_type_code = "V" THEN
				MessageBox("Payment Module - Validation Error","Not a valid Voc Rehab Payee" + &
				 "~r~nYou must select another Voc Rehab Payee number",Exclamation!)
			ELSE
				MessageBox("Payment Module - Validation Error","Not a valid Payee" + &
				"~r~nYou must select another Payee number",Exclamation!)
			END IF
			Return -1
		END IF
		
//	//	P10230 (Retroactive HST Recovery) - Need to check if both the recipient type & the provider tax flags allow HST
//		IF ib_check_recipient_tax_flag = TRUE THEN	// Added for PR 3089					//PR3089
//			
//			SELECT	tax_flag
//			  INTO	:ls_prov_tax_flag
//			  FROM	PROVIDER
//			 WHERE	provider_no = :ll_no
//			   AND	provider_type_code = :ls_recipient_type_code
//			 USING   SQLCA;
//				
//			ll_result = SQLCA.nf_handle_error("Embedded SQL: SELECT tax_flag FROM PROVIDER","n_payment","nf_change_item_2")
//			IF ll_result < 0 THEN Return -1
//
//			IF ls_prov_tax_flag = '' THEN
//				MessageBox('Unknown Provider', 'The Provider number and type combination could not be found.~r~n' + &
//										+ 'Please contact the HelpDesk.', Exclamation!)
//				Return -1
//			END IF
//
//			IF ls_prov_tax_flag = 'N' THEN
//				MessageBox('Invalid Provider/HST Flag','HST Recovery is not allowed for this Provider number.', Exclamation!)
//				Return -1
//			END IF
//		END IF	

	CASE 'cheque_print_group_code'
		// only allowed for recipient types of I
		ll_no = idw_dw[2].GetItemNumber(ll_current_row,"recipient_no")
		ls_recipient_type_code = idw_dw[2].GetItemString(ll_current_row,'recipient_type_code')
		
		IF idw_dw[2].GetText() = 'W' THEN
			IF idw_dw[2].GetItemString(ll_current_row, 'recipient_type_code') <> 'I' THEN
				MessageBox('Warning', 'Only cheques issued to the individual can be sent to WRC.')
				Return -1
			END IF
			IF idw_dw[2].GetItemString(ll_current_row, 'payment_method_code') <> 'A' THEN
				MessageBox('Warning', 'Only automated cheques can be sent to WRC.')
				Return -1
			END IF
								
			idw_dw[2].SetItem(ll_current_row,'use_default_address_flag','N')
			nf_setup_address(ls_recipient_type_code,ll_no,ll_current_row,'W')
		
		ELSE
			// SET BACK TO INDIVIDUAL DEFAULT WHEN NOT CHECKED
			idw_dw[2].SetItem(ll_current_row, 'use_default_address_flag', 'Y')
			nf_setup_address(ls_recipient_type_code,ll_no,ll_current_row,'')
			
		END IF

	CASE "use_default_address_flag"
		// Note: The datawindow attributes ensure that dw_transactions_details field is protected when the payment 
		// method is anything other than 'A - Automated Cheque'
			ll_no = idw_dw[2].GetItemNumber(ll_current_row,"recipient_no")
			ls_recipient_type_code = idw_dw[2].GetItemString(ll_current_row,'recipient_type_code')
			
			IF IsNull(ll_no) or ll_no = 0 Then
				MessageBox("Cancel/Adjust Maintenance","You must first provide a valid recipient number",Exclamation!)
				Return -1
			END IF		
			
			IF IsNull(ls_recipient_type_code) OR ls_recipient_type_code = '' THEN
				MessageBox("Payment","You must first provide a valid recipient type",Exclamation!)
				Return -1
			END IF		
			
			IF idw_dw[2].GetText() = 'Y' THEN
				idw_dw[2].SetItem(ll_current_row,'cheque_print_group_code',' ')
				nf_setup_address(ls_recipient_type_code,ll_no,ll_current_row,idw_dw[2].GetItemString(ll_current_row,'cheque_print_group_code'))
			END IF			
			

	CASE "cheque_no"
		//	IF h/w cheque, cheque no, requisition no and issue date are mandatory
		//	...'When creating a handwritten cheque transaction, the cheque number must be 
		//	(1) - greater than the minimum handwritten cheque number range and less
		//			than the maximum handwritten cheque number range
		//	(2) - the handwritten cheque number must not already exist on the CHEQUE_HEADER
		//			table or on any UNAPPLIED_CLAIM_TXN records - The validation should be done
		//			when the user enters the handwritten cheque.'
      // SR70 - A handwritten cheque number CAN exist for manual cheques that were issued for Special Survivor Payments
		// SR70 - before the system was Automated to issue these payments. - JAN 2001      
		//						
		// only do this check on new or changed cheque numbers
		//		idw_dw[2].accepttext()
		//		ll_result = nf_check_hand_cheque_range(idw_dw[2].GetItemnumber(ll_current_row,"manual_cheque_no"),lb_check)
		IF idw_dw[2].GetItemStatus(ll_current_row, 0, Primary!) = NewModified! OR &
			idw_dw[2].GetItemNumber(ll_current_row,"cheque_no", Primary!, TRUE) <> Long(idw_dw[2].GetText()) THEN
			ll_result = nf_check_hand_cheque_range(Long(idw_dw[2].GetText()),lb_check)
			CHOOSE CASE ll_result 
				CASE -1
					ls_message = "The application failed to retrieve the correct information for this claim number."
				CASE -2
					ls_message = "The cheque number entered does not meet the min/max criteria, please enter another number."
				CASE -3,-4
					IF ls_payment_type_code <> 'S1' THEN
						ls_message = "The cheque number entered is a duplicate, please enter another number."
					ELSE
						ll_result = 0 
						lb_check = TRUE
						MessageBox("Payment","WARNING: The cheque number is a duplicate.",Information!)
					END IF	
			END CHOOSE		
			
			IF ll_result < 0 OR lb_check = FALSE THEN
				MessageBox("Payment",ls_message,Exclamation!)
				Return -1
			END IF
		END IF
		
	CASE "txn_amount"
	//SR70 - Special Survivor Payment - JAN 2001 - SMANZER
	//Amount Payable must be 80000.00 for Special Survivor Payment
	 	ld_txn_amount = Dec(idw_dw[2].GetText())
	 	IF ls_payment_type_code = 'S1' AND  ld_txn_amount <> 80000.00 THEN
			MessageBox('Invalid Payment Amount ','The payment amount must be $80,000.00 for a Special Survivor Payment.')
			Return -1
	 	END IF
	 
END CHOOSE

Return 0
end function

public function integer nf_check_dup_recipients ();LONG		ll_row, ll_max, ll_prev_no
STRING	ls_prev_type

/*	check to make sure no duplicate recipients
*/
	
	idw_dw[2].SetRedraw(FALSE)
	
	idw_dw[2].SetSort('recipient_type_code A, recipient_no A')
	idw_dw[2].Sort()
	
	//Initialize the vairables
	ll_max = idw_dw[2].RowCount()
	ll_row = 1
	ls_prev_type = ''
	ll_prev_no = 0
	
	DO WHILE ll_row <= ll_max
		IF idw_dw[2].GetItemString(ll_row,'recipient_type_code') = ls_prev_type AND &
			idw_dw[2].GetItemNumber(ll_row,'recipient_no') = ll_prev_no THEN
			MessageBox('Duplicate Recipients', 'The award cannot be divided amongst the same recipient.')
			idw_dw[2].SetRedraw(TRUE)
			Return -1
		END IF
		ls_prev_type = idw_dw[2].GetItemString(ll_row,'recipient_type_code')
		ll_prev_no = idw_dw[2].GetItemNumber(ll_row,'recipient_no')
		ll_row = ll_row + 1
	LOOP

	idw_dw[2].SetRedraw(TRUE)
Return 0
end function

public function integer nf_set_defaults ();LONG    				ll_row
DATE   				ld_null
DECIMAL  			lc_daily_rate,	lc_hourly_rate,	lc_benefit_level_percentage
INTEGER  			li_benefit_calculation_no,	li_opening_no
STRING   			ls_payment_method_code
DATAWINDOWCHILD	ldwc_child
long					ll_recipient_no
STRING				ls_payment_type
STRING				ls_payment_sub_type
STRING				ls_opening_type_code
INTEGER				li_x

/*	this routine only executes on an insert
*/
	ll_row = idw_dw[1].GetRow()
	IF ll_row < 0 THEN
	   	RETURN -1
	ELSE
		
		ll_recipient_no = idw_basic_claim.GetItemNumber(1,'individual_no')
		
		SetNull(ld_null)
		
		IF Trim(idw_basic_claim.GetItemString(1,'bank_no'))  = "" THEN
			ls_payment_method_code = 'A'
		ELSE
			ls_payment_method_code = 'D'			
			idw_dw[2].SetItem(1,'bank_no', idw_basic_claim.GetItemString(1,'bank_no'))
			idw_dw[2].SetItem(1,'bank_transit_no', idw_basic_claim.GetItemString(1,'bank_transit_no'))
			idw_dw[2].SetItem(1,'bank_account_no', idw_basic_claim.GetItemString(1,'bank_account_no'))
		END IF
		
		idw_dw[2].SetItem(1,"use_default_address_flag","Y")
		idw_dw[2].SetItem(1,"cheque_print_group_code"," ")
		idw_dw[2].SetItem(1,"payment_method_code",ls_payment_method_code)
		idw_dw[2].SetItem(1,"recipient_type_code","I")
		idw_dw[2].SetItem(1,"recipient_name",idw_basic_claim.GetItemString(1,'given_names') + ' ' + idw_basic_claim.GetItemString(1,'last_name') )
		idw_dw[2].SetItem(1,"recipient_no",idw_basic_claim.GetItemNumber(1,'individual_no') )
		idw_dw[2].SetItem(1,"cheque_deposit_date",ld_null)
		idw_dw[2].SetItem(1,"cheque_no",0)
		idw_dw[2].SetItem(1,"manual_cheque_req_no",0)
		idw_dw[2].SetItem(1,"batch_no",0)
		idw_dw[1].SetItem(1,'nmbr_cycles',1)
		idw_dw[1].SetItem(1,'total_award_amount',0)
		idw_dw[1].SetItem(1,'total_deductions',0)
		idw_dw[1].SetItem(1,'hour_amount',0)
		idw_dw[1].SetItem(1,'day_amount',0)
		idw_dw[1].SetItem(1,'explanation',"")
		
		
		/* new defaults for project 10229 new columns for PAYMENT  table */
		idw_dw[1].SetItem(1,'paid_quantity',0)
		idw_dw[1].SetItem(1,'tax_amount',0)
		idw_dw[1].SetItem(1,'tax_rate',0)
		idw_dw[1].SetItem(1,'authorization_no',0)
		//idw_dw[1].SetItem(1,'adjustment_quantity',0)//default 0
		idw_dw[1].SetItem(1,'adjustment_tax_amount',0)
		idw_dw[1].SetItem(1,'adjustment_payment_amount',0)
		idw_dw[1].SetItem(1,'payment_adjustment_flag',"N")
		idw_dw[1].SetItem(1,'total_deductions',0)
		idw_dw[1].SetItem(1,'final_payment_flag',"N")
		idw_dw[1].SetItem(1,'award_no',0)
		idw_dw[1].SetItem(1,'benefit_calculation_no',0)
		idw_dw[1].SetItem(1,'loe_explanation',"")
		idw_dw[1].SetItem(1,'submitted_amount',0)
		idw_dw[1].SetItem(1,'payment_sub_type_code',"")
		idw_dw[1].SetItem(1,'authorization_group_flag','N')
		idw_dw[1].SetItem(1,'authorization_group_no' , 0)
		idw_dw[1].SetItem(1,'authorization_group_date', ld_null)
		idw_dw[1].SetItem(1,'authorization_group_user_id', '')
		idw_dw[1].SetItem(1,'authorization_group_reason_code', '')
		idw_dw[1].SetItem(1,'authorization_group_reason_comment','')
		
		idw_dw[2].SetItem(1,'maintain_allowed_flag',"Y")
		idw_dw[2].SetItem(1,'txn_sub_type_code',"")
		idw_dw[2].SetItem(1,'tax_amount',0)
		idw_dw[2].SetItem(1,'txn_unit_of_work_no',0)
		idw_dw[2].SetItem(1,'direct_deposit_xmit_no',0)
		idw_dw[2].SetItem(1,'related_txn_no',0)
		idw_dw[2].SetItem(1,'cheque_no',0)
		idw_dw[2].SetItem(1,'coc_period',0)
		idw_dw[2].SetItem(1,'explanation',"")

		
		idw_dw[1].GetChild('opening_no', ldwc_child)	
		

		idw_dw[1].SetItem(1,"scheduled_processing_date",DateTime(Date(f_server_datetime())))	
	
	/* 	Setup the default occurrence and benefit calculation number 
	*/
		
		IF ldwc_child.RowCount() = 1 THEN
			li_opening_no = ldwc_child.GetItemNumber(1,"max_opening_no")
		ELSE
			li_opening_no = nf_check_openings(ldwc_child, 'RLOE')
			IF li_opening_no > 0 THEN
				ls_opening_type_code = ldwc_child.GetItemString(li_opening_no,'opening_type_code')
				li_opening_no = ldwc_child.GetItemNumber(li_opening_no,'opening_no')				
				/*		Reset some items if it is receiving salary
				*/
				IF idw_basic_claim.GetItemString(1,'receiving_salary_flag') = "Y" and ls_opening_type_code  = 'RLOE' THEN
					ls_payment_method_code = 'R'
					idw_dw[2].SetItem(1,"payment_method_code",ls_payment_method_code)
					idw_dw[2].SetItem(1,"recipient_type_code","I")
				END IF	
			ELSE
				li_opening_no = nf_check_openings(ldwc_child, 'LTD') 
				IF li_opening_no > 0 THEN
					li_opening_no = ldwc_child.GetItemNumber(li_opening_no,'opening_no')
				ELSE
					li_opening_no = ldwc_child.GetItemNumber(1,"max_opening_no")
				END IF
			END IF
		END IF
		
		idw_dw[1].SetItem(1,"opening_no",li_opening_no)
	
		nf_tab_order("recipient_type_code","I")
	
		idw_dw[1].GetChild('benefit_calculation_no', ldwc_child)
		IF li_opening_no = 0 OR ldwc_child.RowCount() = 1 THEN			//if rowcount = 1 then only benefit calc of 0
			ldwc_child.SetFilter(" benefit_calculation_no = 0")
		ELSE
			ldwc_child.SetFilter("opening_no = " + string(li_opening_no) + " or benefit_calculation_no = 0")
		END IF
		ldwc_child.Filter()
		li_benefit_calculation_no = ldwc_child.GetItemNumber(1,"max_benefit_calculation_no")
	
		nf_setup_benefit_info(li_benefit_calculation_no,lc_daily_rate,lc_hourly_rate,lc_benefit_level_percentage)
		idw_dw[1].SetItem(1,"benefit_calculation_no",li_benefit_calculation_no)
	
		nf_set_payment_filter(li_opening_no,li_benefit_calculation_no)
		// pr 2787 - new argument (below) indicates that the function call is a result of an insert, not a change
		nf_setup_default(lc_daily_rate,li_benefit_calculation_no,lc_benefit_level_percentage, li_opening_no,TRUE)

		ls_payment_type = idw_dw[1].GetItemString(1,'payment_type_code')
		ls_payment_sub_type = idw_dw[1].GetItemString(1,'payment_sub_type_code')
		
		IF li_opening_no > 0 AND ls_payment_type = 'R1' THEN		
			/*		Reset some items if it is receiving salary
			*/
			IF idw_basic_claim.GetItemString(1,'receiving_salary_flag') = "Y" and ls_opening_type_code  = 'RLOE' THEN
				ls_payment_method_code = 'A'
				idw_dw[2].SetItem(1,"payment_method_code",ls_payment_method_code)
				idw_dw[2].SetItem(1,"recipient_type_code","I")
			END IF
		END IF
		
		nf_rtw_defaulting(ls_payment_type)
		
		For li_x = 1 to idw_dw[2].RowCount()
			nf_setup_address(idw_dw[2].GetItemString(li_x,'recipient_type_code'),idw_dw[2].GetItemNumber(li_x,'recipient_no'),li_x,'')
		Next			
	
	END IF

Return 0   
end function

public function integer nf_insert_recipient (integer al_row);LONG  ll_rowcount

/*	Validate that another recipient is allowed                                          
*/

	ll_rowcount = idw_dw[2].InsertRow(al_row)
	IF ll_rowcount > 0 THEN
   	idw_dw[2].ScrollToRow(ll_rowcount)
	   // set defaults
	   idw_dw[2].SetItem(ll_rowcount, 'use_default_address_flag', 'Y')
		idw_dw[2].SetItem(ll_rowcount, 'cheque_print_group_code', ' ')
		idw_dw[2].SetItem(ll_rowcount,"txn_type_code","1")
		idw_dw[2].SetItem(ll_rowcount,"txn_sub_type_code","")
		idw_dw[2].SetItem(ll_rowcount,'batch_no',0)
		idw_dw[2].SetItem(ll_rowcount,'recipient_no','V')
		idw_dw[2].SetItem(ll_rowcount,'manual_cheque_req_no',0)
		idw_dw[2].SetItem(ll_rowcount,'cheque_no',0)
		idw_dw[2].SetItem(ll_rowcount,'payment_method_code', 'A')
		idw_dw[2].SetItem(ll_rowcount,'txn_amount',0)
		idw_dw[2].SetItem(ll_rowcount,'maintain_allowed_flag','Y')
		idw_dw[2].SetItem(ll_rowcount,'txn_unit_of_work_no',0)	

	   idw_dw[2].SetItem(ll_rowcount,'tax_amount',0)
	   idw_dw[2].SetItem(ll_rowcount,'direct_deposit_xmit_no',0)
	   idw_dw[2].SetItem(ll_rowcount,'related_txn_no',0)
	   idw_dw[2].SetItem(ll_rowcount,'coc_period',0)
	   idw_dw[2].SetItem(ll_rowcount,'explanation',"")
	
		
	END IF

Return ll_rowcount
end function

public function integer nf_setup_default (decimal adec_daily_rate, integer ai_benefit_calculation_no, decimal adec_benefit_level_percentage, integer ai_opening_no, boolean ab_insert);DATE					ldt_paid_from_date, ldt_paid_to_date, ldt_order_date, ldt_accident_recurrence_date, ldt_server_date
DECIMAL				lc_days_worked_per_week, lc_paid_days_lost, lc_day_amount
DECIMAL				lc_total_award_amount, lc_deduction 
INTEGER				li_error_level, li_year, li_month, li_day, li_counter, li_count_recipients
LONG					ll_benefit_rownum, ll_opening_rownum, ll_payment_no, ll_count, ll_result, ll_pymt_type_row, ll_txn_no
STRING				ls_payment_type_code, ls_error_message, ls_award_freq_code, ls_opening_type_code, ls_days_required, ls_dates_required
STRING				ls_b_no, ls_t_no, ls_a_no, ls_three_day_paid_flag, ls_payment_type_desc, ls_payment_sub_type_desc, ls_use_default_address_flag
DATAWINDOWCHILD	ldwc_child_opening, ldwc_child_benefit, ldwc_payment_and_sub
DATETIME				ldtm_paid_to_date, ldtm_benefit_start_date, ldtm_benefit_end_date
BOOLEAN				lb_first_payment_of_type
DATASTORE			ds_new_trans

/* Function Name: nf_setup_default                                                                      
	Purpose:       The purpose of this module is to setup a default payment.                             
	  	             This module is called when the user clicks the add button to add a new payment, and   
      	          when they change the benefit calculation.                                             
                                                                                                      
	 Arguments:     adec_daily_rate                                                                        
   	             ai_benefit_calculation_no                                                            
      	          adec_benefit_level_percentage                                                          
	                  ai_opening_no
					ab_insert
	 Return Values: n/a                                                 
	 
	 **	PR 2787 -	This function is also called when the user changes an opening.
	 						
	 						Also, added an argument (ab_insert) to determine,
							when the last payment was a split payment, if a row should be added or not. 
*/

	ldt_server_date = Date(f_server_datetime())
	SetNull(ldtm_benefit_start_date)
	SetNull(ldtm_benefit_end_date)

	IF ai_opening_no > 0 THEN
		idw_dw[1].GetChild('opening_no',ldwc_child_opening)
		ll_opening_rownum            = ldwc_child_opening.Find("opening_no = " + string(ai_opening_no),1,ldwc_child_opening.RowCount())
		ldt_accident_recurrence_date = Date(ldwc_child_opening.GetItemDateTime(ll_opening_rownum,"accident_recurrence_date"))
		ldtm_benefit_start_date      = ldwc_child_opening.GetItemDateTime(ll_opening_rownum,"benefit_start_date")
		ldtm_benefit_end_date        = ldwc_child_opening.GetItemDateTime(ll_opening_rownum,"benefit_end_date")
		ls_opening_type_code         = ldwc_child_opening.GetItemString(ll_opening_rownum, 'opening_type_code')
		ls_three_day_paid_flag       = ldwc_child_opening.GetItemString(ll_opening_rownum, 'three_day_paid_flag')
	ELSE
		SetNull(ldt_accident_recurrence_date)
	END IF

	lc_deduction = 0

	SetNull(ls_payment_type_code)
	SetNull(ldt_paid_from_date)
	SetNull(ldt_paid_to_date)
	SetNull(ldtm_paid_to_date)
	lc_deduction = 0

/*	if opening no > 0 then find the max payment of the type in the payment type list and default to that
*/

	IF ai_opening_no > 0 THEN
		SELECT Max(payment_no)
		  INTO :ll_payment_no
		  FROM PAYMENT
		 WHERE claim_no   = :il_claim_no
			AND opening_no = :ai_opening_no
			AND PAYMENT.payment_type_code 
		    IN (
			SELECT Payment_Type.payment_type_code
			  FROM Payment_Type, Payment_Combination  
	   	 WHERE ( Payment_Type.payment_type_code = Payment_Combination.payment_type_code )    
			 	AND authorization_type_code         = 'loe'
			   AND opening_type_code               = :ls_opening_type_code
				AND benefit_level_percentage        = :adec_benefit_level_percentage 
				AND Payment_Type.payment_type_code <> '3D');

		IF SQLCA.nf_handle_error('Embedded SQL: payment type', 'n_payments', 'nf_setup_defaults') < 0 THEN
			Return -1
		END IF
		IF NOT IsNull(ll_payment_no) THEN
			
			SELECT P.payment_type_code, P.paid_to_date, PT.payment_type_desc, PST.payment_sub_type_desc  
			  INTO :ls_payment_type_code, :ldtm_paid_to_date, :ls_payment_type_desc, :ls_payment_sub_type_desc 
			  FROM PAYMENT P,
			       Payment_Type PT, 
					 Payment_Sub_Type PST 
			 WHERE payment_no              = :ll_payment_no 
			   AND P.payment_type_code     = PT.payment_type_code 
				AND P.payment_type_code     = PST.payment_type_code 
				AND P.payment_sub_type_code = PST.payment_sub_type_code ;

			IF SQLCA.nf_handle_error('Embedded SQL: PAYMENT select','n_payment', 'nf_setup_default') < 0 THEN
				Return -1
			END IF
		END IF
	END IF

	IF IsNull(ls_payment_type_code) THEN
		IF ls_opening_type_code = 'RLOE' THEN
			IF adec_benefit_level_percentage = .90 THEN
				ls_payment_type_code = ".1"
			ELSEIF adec_benefit_level_percentage = .85 THEN
				ls_payment_type_code = "-1"
			ELSEIF adec_benefit_level_percentage = .80 THEN
				ls_payment_type_code = " 1"
			ELSEIF adec_benefit_level_percentage = .75 THEN
				ls_payment_type_code = "11"
			END IF
//Pr2055 - Aug 21,2001 - Set payment type code 7 as default instead of 6			
		ELSEIF ls_opening_type_code = 'LTD' THEN
			IF adec_benefit_level_percentage = .90 THEN
				ls_payment_type_code = ".7"
			ELSEIF adec_benefit_level_percentage = .85 THEN
				ls_payment_type_code = "-7"
			ELSEIF adec_benefit_level_percentage = .80 THEN
				ls_payment_type_code = " 7"
			END IF
		ELSEIF ls_opening_type_code = 'PEN' OR ls_opening_type_code = 'SV' OR ls_opening_type_code = 'S1' OR ls_opening_type_code = 'S2' THEN
			IF adec_benefit_level_percentage = 0 THEN
				ls_payment_type_code = 'IN'
			ELSE
				ls_payment_type_code = 'P'
			END IF
		ELSE
			
		END IF			
	END IF


   idw_dw[1].GetChild('benefit_calculation_no', ldwc_child_benefit)
	ll_benefit_rownum = ldwc_child_benefit.Find("benefit_calculation_no = " + string(ai_benefit_calculation_no),1, ldwc_child_benefit.RowCount())
	IF ll_benefit_rownum > 0 THEN
	  	ls_award_freq_code      = ldwc_child_benefit.GetItemString(ll_benefit_rownum,"award_freq_code")
		lc_days_worked_per_week = ldwc_child_benefit.GetItemDecimal(ll_benefit_rownum,'preacc_work_days_per_week')
	ELSE
		MessageBox('Error','Unable to find benefit information for defaulting payment.  Please try again.')
		Return -1
	END IF
	nf_validate_payment_type(ls_payment_type_code,li_error_level,ls_error_message,ls_award_freq_code, ls_days_required, ls_dates_required)
	

	IF li_error_level = 0 THEN
		idw_dw[1].SetItem(1,'payment_type_code', ls_payment_type_code)
		idw_dw[1].GetChild('payment_type_and_sub_type' ,ldwc_payment_and_sub)
		
		ll_pymt_type_row = ldwc_payment_and_sub.Find('payment_type_code = ' + string(ls_payment_type_code) , 1 , ldwc_payment_and_sub.RowCount())
		IF ll_pymt_type_row > 0 THEN
			ldwc_payment_and_sub.ScrollToRow(ll_pymt_type_row)
		END IF
		
		idw_dw[1].SetItem(1,'payment_type_and_sub_type', ls_payment_type_code)
/*		Set the paid from date */
		IF ls_dates_required = 'N' THEN	
			SetNull(ldt_paid_from_date)
			idw_dw[1].SetItem(1,'paid_from_date', DateTime(ldt_paid_from_date))
		ELSE
/*			the paid to date of the 'like' payment type is not null
*/
			IF NOT IsNull(ldtm_paid_to_date) THEN
				IF ldtm_paid_to_date >= ldtm_benefit_start_date THEN
					IF IsNull(ldtm_benefit_end_date) THEN
						idw_dw[1].SetItem(1,'paid_from_date', ldtm_paid_to_date)
					ELSEIF ldtm_paid_to_date <= ldtm_benefit_end_date THEN
						idw_dw[1].SetItem(1,'paid_from_date', ldtm_paid_to_date)			
					END IF
				END IF
			ELSEIF ai_opening_no > 0 THEN
				idw_dw[1].SetItem(1,'paid_from_date',ldtm_benefit_start_date)
			ELSE
				SetNull(ldt_paid_from_date)
				idw_dw[1].SetItem(1,'paid_from_date', DateTime(ldt_paid_from_date))
				idw_dw[1].SetColumn('paid_from_date')
				idw_dw[1].SetFocus()
				nf_tab_order("payment_type_code",ls_payment_type_code)
				Return 0
			END IF					
		END IF
/*		Set the paid to date
*/
		ldt_paid_from_date = Date(idw_dw[1].GetItemDateTime(1,'paid_from_date'))
		IF ls_dates_required = 'N' THEN	
			SetNull(ldt_paid_to_date)
			idw_dw[1].SetItem(1,'paid_to_date', DateTime(ldt_paid_from_date))
		ELSE
			IF ls_opening_type_code = 'RLOE' THEN
				IF IsNull(ldtm_benefit_end_date) OR RelativeDate(Date(ldtm_benefit_end_date),15) >  ldt_paid_from_date THEN
					idw_dw[1].SetItem(1,'paid_to_date', DateTime(RelativeDate(ldt_paid_from_date,14)))
				ELSE
					idw_dw[1].SetItem(1,'paid_to_date', ldtm_benefit_end_date)
				END IF

			ELSEIF ls_opening_type_code = 'LTD' THEN
/*			if no benefit end date or end date >= first month after the from date
*/
				IF IsNull(ldtm_benefit_end_date) OR ((Month(Date(ldtm_benefit_end_date)) > Month(ldt_paid_from_date) AND &
					Year(Date(ldtm_benefit_end_date)) = Year(ldt_paid_from_date) ) OR &
					(Month(Date(ldtm_benefit_end_date)) < Month(ldt_paid_from_date) AND &
					Year(Date(ldtm_benefit_end_date)) > Year(ldt_paid_from_date) )) THEN
					li_year	= Year(ldt_paid_from_date)
					li_month = Month(ldt_paid_from_date)
					li_day	= Day(ldt_paid_from_date)
					IF li_month <> 12 THEN
						li_month ++
					ELSE
						li_month = 1
						li_year++
					END IF
					idw_dw[1].SetItem(1,'paid_to_date', Date(li_year,li_month,1))
				ELSE
					SetNull(ldt_paid_from_date)
					idw_dw[1].SetItem(1,'paid_to_date', DateTime(ldt_paid_from_date))
					idw_dw[1].SetColumn('paid_to_date')
					idw_dw[1].SetFocus()
					nf_tab_order("payment_type_code",ls_payment_type_code)
					Return 0
				END IF
			END IF
		END IF
		ldt_paid_to_date = Date(idw_dw[1].GetItemDateTime(1,'paid_to_date'))

		IF ls_days_required = 'N' THEN	
			idw_dw[1].SetItem(1,'paid_days_lost', 0)
			idw_dw[1].SetItem(1,"day_amount",0.00)
			idw_dw[1].SetItem(1,"paid_hours_lost",0)
			idw_dw[1].SetItem(1,"hour_amount",0.00)
			idw_dw[1].SetItem(1,"total_award_amount",0.00)
			idw_dw[1].SetItem(1,"total_deductions",0.00)
			idw_dw[1].SetItem(1,"total_payment_amount",0.00)
		ELSE
			IF ls_opening_type_code = 'RLOE' AND DaysAfter(ldt_paid_from_date, ldt_paid_to_date) = 14 THEN
				idw_dw[1].SetItem(1,'paid_days_lost', 2 * lc_days_worked_per_week)
			ELSEIF ls_opening_type_code = 'LTD' AND Day(ldt_paid_to_date) = 1 AND &
				((Month(ldt_paid_to_date) > Month(ldt_paid_from_date)) OR (Month(ldt_paid_to_date) < Month(ldt_paid_from_date) AND &
				  Year(ldt_paid_to_date) > Year(ldt_paid_from_date))) THEN
				IF Day(ldt_paid_from_date) = 1 THEN
					idw_dw[1].SetItem(1,'paid_days_lost', 30)
				ELSE
					idw_dw[1].SetItem(1,'paid_days_lost',DaysAfter(ldt_paid_from_date, ldt_paid_to_date) -1)
				END IF
			ELSE
				idw_dw[1].SetItem(1,"paid_hours_lost",0)
				idw_dw[1].SetItem(1,"hour_amount",0.00)
				idw_dw[1].SetItem(1,'paid_days_lost', 0)
				idw_dw[1].SetItem(1,"day_amount",0.00)
				idw_dw[1].SetItem(1,"total_award_amount",0.00)
				idw_dw[1].SetItem(1,"total_deductions",0.00)
				idw_dw[1].SetItem(1,"total_payment_amount",0.00)
				idw_dw[1].SetColumn('paid_days_lost')
				idw_dw[1].SetFocus()
			END IF
		END IF
		lc_paid_days_lost = idw_dw[1].GetItemDecimal(1,'paid_days_lost')

		IF ls_opening_type_code = 'RLOE' THEN
/*			if rloe and the first payment for the recurrence
*/
			lb_first_payment_of_type = TRUE

			IF ai_opening_no > 0 THEN
				SELECT Count(*)
				  INTO :ll_count
				  FROM PAYMENT
				 WHERE claim_no = :il_claim_no
					AND opening_no 
			    IN (
				SELECT opening_no
				  FROM OPENING
		   	 WHERE ( accident_recurrence_date = :ldt_accident_recurrence_date
					AND	opening_type_code = 'RLOE'
					AND	claim_no = :il_claim_no));

				IF SQLCA.nf_handle_error('Embedded SQL: payment ', 'n_payments', 'nf_setup_defaults') < 0 THEN
					Return -1
				END IF
				IF ll_count > 0 THEN
					lb_first_payment_of_type = FALSE
				END IF
			END IF
			/* If first payment for the RLOE recurrence and the three day is not yet paid  
			*/
			IF lb_first_payment_of_type and ls_three_day_paid_flag = 'N' THEN
				IF MessageBox("Payment Module","Do you want to deduct the 3 day waiting period",Question!,YesNo!) = 1 THEN
					lc_deduction = ldwc_child_benefit.GetItemDecimal(ll_benefit_rownum,"award_amount")
					lc_deduction = -1 * Round((lc_deduction * .6),2)
				END IF
			END IF
		END IF

		adec_daily_rate               = idw_dw[1].GetItemDecimal(1,"daily_rate")

		//Set the order date default to the current date.
		ldt_order_date = ldt_server_date		
		
		idw_dw[1].SetItem(1,"scheduled_processing_date",DateTime(ldt_order_date))
		lc_day_amount               = lc_paid_days_lost * adec_daily_rate
		lc_total_award_amount       = Round(lc_day_amount,2)

		IF ls_payment_sub_type_desc = "" OR IsNull(ls_payment_sub_type_desc) THEN
			idw_dw[1].SetItem(1,"payment_type_description",ls_payment_type_desc)
		ELSE
			idw_dw[1].SetItem(1,"payment_type_description",ls_payment_type_desc + " - " + ls_payment_sub_type_desc)
		END IF
		idw_dw[1].SetItem(1,"nmbr_cycles",1)
		idw_dw[1].SetItem(1,"paid_hours_lost",0)
		idw_dw[1].SetItem(1,"day_amount",lc_day_amount)
		idw_dw[1].SetItem(1,"hour_amount",0)
		idw_dw[1].SetItem(1,"total_award_amount",lc_total_award_amount)
		idw_dw[1].SetItem(1,"total_deductions",lc_deduction)

		lc_total_award_amount = lc_total_award_amount + lc_deduction
		IF lc_total_award_amount < 0 THEN
			lc_total_award_amount = 0
		END IF
		idw_dw[1].SetItem(1,"total_payment_amount",lc_total_award_amount )
		idw_dw[1].SetItem(1,"final_payment_flag","N")
		idw_dw[1].SetItem(1,"payment_adjustment_flag","N")

/* find out if the last similar payment was sent to more than one recipient 
*/
		IF NOT IsNull(ll_payment_no) THEN
			SELECT count(recipient_no)
			INTO :li_count_recipients
			FROM APPLIED_CLAIM_TXN
			WHERE payment_no = :ll_payment_no and txn_type_code = '1';
	
			IF SQLCA.nf_handle_error('Embedded SQL: APPLIED select','n_payment', 'nf_setup_default') < 0 THEN
				Return -1
			END IF

/* if no similar payment was paid then find out if the last similar scheduled payment is to be sent to more than one recipient
*/
			IF li_count_recipients = 0 THEN
				SELECT count(recipient_no)
				INTO :li_count_recipients
				FROM UNAPPLIED_CLAIM_TXN
				WHERE payment_no = :ll_payment_no and txn_type_code = '1';
	
				IF SQLCA.nf_handle_error('Embedded SQL: UNAPPLIED select','n_payment', 'nf_setup_default') < 0 THEN
					Return -1
				END IF
			END IF

/* if there was more than one recipient in the last payment used for defaulting then ask the user
	if they wish to send this payment to the same recipients
*/

/* PR 2787 - only ask the question and insert a row if the call of this function is the result of an insert, not a change */
			IF ab_insert THEN
				IF li_count_recipients > 1 THEN
					IF MessageBox("Recipients","The last similar payment was sent to more than one recipient,~r~n" &
					+ "do you wish to duplicate recipients?",Question!,YesNo!) = 1 THEN
	
						nf_retrieve_recipients()
	/*					Feb 27/98  changed the retrieve to retrieve into a datastore first then move only the transactions
						of type '1' to the result dw.  If you retrieve into dw 2 directly all adjustments, cancellations, etc
						get copied in also resulting in bad data.  The user could then save the payment with negative txn
						amounts
	*/
			
						ds_new_trans = create datastore
						ds_new_trans.dataobject = 'd_transaction_details_datastore'
				
						ds_new_trans.SetTransObject(SQLCA)
						li_count_recipients = ds_new_trans.Retrieve(ll_payment_no)
	/*					move the data over
	*/
							ll_count = 1	
							idw_dw[2].Reset()
							DO WHILE ll_count <= li_count_recipients
								ll_result = idw_dw[2].InsertRow(1)
								IF ll_result > 0 THEN
									idw_dw[2].SetItem(1,"recipient_no",ds_new_trans.GetItemNumber(ll_count,"recipient_no"))
									idw_dw[2].SetItem(1,"recipient_type_code",ds_new_trans.GetItemString(ll_count,"recipient_type_code"))
									idw_dw[2].SetItem(1,"payment_method_code",ds_new_trans.GetItemString(ll_count,"payment_method_code"))
									idw_dw[2].SetItem(1,"admin_region_code",ds_new_trans.GetItemString(ll_count,'admin_region_code'))
									idw_dw[2].SetItem(1,"recipient_name",ds_new_trans.GetItemString(ll_count,"recipient_name"))
									idw_dw[2].SetItem(1,"address_line1",ds_new_trans.GetItemString(ll_count,"address_line1"))
									idw_dw[2].SetItem(1,"address_line2",ds_new_trans.GetItemString(ll_count,"address_line2"))
									idw_dw[2].SetItem(1,"city",ds_new_trans.GetItemString(ll_count,"city"))
									idw_dw[2].SetItem(1,"prov_state_code",ds_new_trans.GetItemString(ll_count,"prov_state_code"))
									idw_dw[2].SetItem(1,"country",ds_new_trans.GetItemString(ll_count,"country"))
									idw_dw[2].SetItem(1,"postal_code",ds_new_trans.GetItemString(ll_count,"postal_code"))
									ls_use_default_address_flag = ds_new_trans.GetItemString(ll_count,'use_default_address_flag')
									idw_dw[2].SetItem(1,"use_default_address_flag",ls_use_default_address_flag)
									idw_dw[2].SetItem(1,"cheque_print_group_code", ' ')
									
									idw_dw[2].SetItem(1,'txn_amount',ds_new_trans.GetItemNumber(ll_count,"txn_amount"))
									idw_dw[2].SetItem(1,'bank_no', ds_new_trans.GetItemString(ll_count,'bank_no'))
									idw_dw[2].SetItem(1,'bank_account_no', ds_new_trans.GetItemString(ll_count,'bank_account_no'))
									idw_dw[2].SetItem(1,'bank_transit_no',ds_new_trans.GetItemString(ll_count,'bank_transit_no'))
									idw_dw[2].SetItem(1,'txn_type_code',ds_new_trans.GetItemString(ll_count,'txn_type_code'))
											/* new defaults for project 10229 new columns for PAYMENT  table */
									idw_dw[2].SetItem(1,'maintain_allowed_flag',"Y")
									idw_dw[2].SetItem(1,'txn_sub_type_code',"")
									idw_dw[2].SetItem(1,'tax_amount',0)
									idw_dw[2].SetItem(1,'txn_unit_of_work_no',0)
									idw_dw[2].SetItem(1,'direct_deposit_xmit_no',0)
									idw_dw[2].SetItem(1,'related_txn_no',0)
									idw_dw[2].SetItem(1,'cheque_no',0)
									idw_dw[2].SetItem(1,'coc_period',0)
									idw_dw[2].SetItem(1,'explanation',"")
									
								ELSE
									MessageBox('Error','Unable to create transaction entries.')
									Return -1
								END IF
								ll_count = ll_count + 1
							LOOP
							
						Destroy ds_new_trans
						
						li_counter = 1
						
						DO WHILE li_counter <= idw_dw[2].RowCount()
							IF idw_dw[2].GetItemString(li_counter,'recipient_type_code') = 'I' THEN
								nf_set_recipient_filter(FALSE)
							ELSE
								nf_set_recipient_filter(TRUE)
							END IF
	
							IF idw_dw[2].GetItemString(li_counter,'payment_method_code') = 'D' THEN
								ll_txn_no = idw_dw[2].GetItemNumber(li_counter,'txn_no')
								nf_get_bank(idw_dw[2].GetItemNumber(li_counter,'recipient_no'),idw_dw[2].GetItemString(li_counter,'recipient_type_code'),ls_b_no, ls_t_no, ls_a_no, ll_payment_no,ll_txn_no)
								idw_dw[2].SetItem(li_counter,'bank_no', ls_b_no)
								idw_dw[2].SetItem(li_counter,'bank_transit_no', ls_t_no)
								idw_dw[2].SetItem(li_counter,'bank_account_no', ls_a_no)
							END IF
	
							idw_dw[2].SetItem(li_counter,'payment_no',0)
							idw_dw[2].SetItem(li_counter,'txn_no',0)
							idw_dw[2].SetItem(li_counter,'batch_no',0)
							idw_dw[2].SetItemStatus(li_counter,0,Primary!,NewModified!)
	
							li_counter++
						LOOP				
						idw_dw[2].SetRow(1)	
					END IF
				ELSE
					
				END IF
			END IF
		END IF
	ELSE
		idw_dw[1].SetItem(1,"paid_days_lost",0)
		idw_dw[1].SetItem(1,"day_amount",0.00)
		idw_dw[1].SetItem(1,"paid_hours_lost",0)
		idw_dw[1].SetItem(1,"hour_amount",0.00)
		idw_dw[1].SetItem(1,"total_award_amount",0.00)
		idw_dw[1].SetItem(1,"total_deductions",0.00)
		idw_dw[1].SetItem(1,"total_payment_amount",0.00)
	END IF
		
	IF idw_dw[2].RowCount() = 1 THEN
		idw_dw[2].SetItem(1,"txn_amount",lc_total_award_amount)
	END IF
	nf_tab_order("payment_type_code",ls_payment_type_code)

Return 0
end function

public function integer nf_setup_survivor ();LONG					ll_current_row, ll_result, ll_null, ll_count, ll_count2, ll_row
DATE 					ld_server_dt				

ld_server_dt = Date(f_server_datetime())

SetNull(ll_null)
ll_current_row  = idw_dw[1].GetRow()
ll_row 			 = idw_dw[2].GetRow()

//Check for another special survivor payment for this individual. Only allowed one special survivor payment/individual.

/*	SR 115: Need to add the ability to issue a special survivor payment if the recipient already has a payment but that
	payment has been cancelled.  Will still be prevented from adding a special survivor payment if the recipient already
	has a sucessful payment. (May 2, 2001 EMcD)
*/
	SELECT COUNT(*)
	  INTO :ll_count
	  FROM PAYMENT P, APPLIED_CLAIM_TXN A
	 WHERE A.claim_no          = P.claim_no
	   AND A.payment_no        = P.payment_no
	   AND P.claim_no          = :il_claim_no
	   AND P.payment_type_code = 'S1'
	   AND A.canceled_txn_flag = 'N'
	   AND A.txn_type_code NOT IN ('5')
	   AND NOT ( A.txn_type_code = 'J' AND A.txn_sub_type_code = '2')
	 USING SQLCA ;

ll_result = SQLCA.nf_handle_error('Embedded SQL: select from PAYMENT & APPLIED_CLAIM_TXN','n_payment','nf_setup_survivor')

	SELECT COUNT(*)
	  INTO :ll_count2
	  FROM PAYMENT P, UNAPPLIED_CLAIM_TXN A
	 WHERE A.claim_no          = P.claim_no
	   AND A.payment_no        = P.payment_no
	   AND P.claim_no          = :il_claim_no
	   AND P.payment_type_code = 'S1'
	   AND A.txn_type_code NOT IN ('5')
	   AND NOT ( A.txn_type_code = 'J' AND A.txn_sub_type_code = '2')
	 USING SQLCA ;

ll_result = SQLCA.nf_handle_error('Embedded SQL: select from PAYMENT & APPLIED_CLAIM_TXN','n_payment','nf_setup_survivor')

ll_count = ll_count + ll_count2

IF ll_count > 0 THEN				
	Messagebox("Payment Module - Warning", "A Special Survivor Payment has already been paid to this Individual." + &
	"Only one payment can be issued.", Information!)
	RETURN -1
END IF

//Set the Period TO/FROM Dates to todays Date
idw_dw[1].SetItem(1,"paid_from_date",ld_server_dt)
idw_dw[1].SetItem(1,"paid_to_date",ld_server_dt)

// Setup the remaining fields & payment amount to $80,000.00
idw_dw[1].SetItem(1,"paid_days_lost",0)
idw_dw[1].SetItem(1,"day_amount",0)
idw_dw[1].SetItem(1,"paid_hours_lost",0)
idw_dw[1].SetItem(1,"hour_amount",0)
idw_dw[1].SetItem(1,"total_award_amount",80000.00)
idw_dw[1].SetItem(1,"total_deductions",0)
idw_dw[1].SetItem(1,"total_payment_amount",80000.00)
idw_dw[2].SetItem(ll_row,"txn_amount",80000.00)
idw_dw[2].Modify("txn_amount.Protect=1")


RETURN 1
end function

public function integer nf_check_bus_rule ();DATAWINDOWCHILD  ldwc_child

LONG		ll_no,ll_count,ll_opening_row,ll_opening_no,ll_result
LONG		ll_transaction_rowcount,ll_txn_no,ll_benefit_calculation_no,ll_recipient_cntr,ll_recipient_no
LONG		ll_days_after,ll_tranrow,ll_benefit_rownum,ll_nmbr_cycles,ll_payment_no,ll_payment_cntr
LONG		ll_txn_rowcount, ll_3D_payment_count, ll_payment_row, ll_group_no, ll_group_rows, ll_rtn

INTEGER	li_return, li_rtn, li_trans, li_counter

STRING	ls_days_required_flag,ls_fromto_dates_required_flag,ls_award_freq_code
STRING   ls_authorization_type_code,ls_three_day_paid_flag,ls_string,ls_opening_type_code
STRING	ls_payment_type_code, ls_ben_class_code,ls_err_msg, ls_payment_sub_type_code, ls_use_default_address_flag, ls_payment_adjustment_flag
STRING	ls_payment_method_code,ls_recipient_type_code ,ls_cheque_print_group_code,ls_active,ls_active_sub
STRING	ls_maintain_allowed_flag, ls_days_hours_flag, ls_old_payment_type_code, ls_group_flag

DATETIME	ldt_paid_from_date,ldt_paid_to_date,ldt_scheduled_processing_date,ldt_server_date,ldtm_benefit_start,ldtm_benefit_end
DATETIME	ldtm_effective_date,ldt_LTD_6month_date

DATE		ld_paid_from_date,ld_paid_to_date,ld_temp_date,ld_null_date,ld_server_date, ld_man_cheque_date, ldt_impaired

DECIMAL	ldec_impaired
DECIMAL	ldec_paid_days_lost,ldec_paid_hours_lost,ldec_work_hours_per_day,ldec_temp_days,ldec_temp_txn_amount
DECIMAL	ldec_benefit_amount , ldec_deduction_amount, ldec_bencalc_award_amount,ldec_total_payment_amount, ldec_total_group_amount, ldec_original_payment_amount

/*	Check business rules for the payment. Perform consistency & mandatory validations */
SetNull(ld_null_date)
ls_payment_type_code 			= idw_dw[1].GetItemString(1,"payment_type_code")
ls_old_payment_type_code         = idw_dw[1].GetItemString(1, "payment_type_code",Primary!,TRUE)
ls_payment_sub_type_code	= idw_dw[1].GetItemString(1,"payment_sub_type_code")
ls_string 								= idw_dw[1].GetItemString(1,"effective_from_date")
ldt_paid_from_date 				= idw_dw[1].GetItemDateTime(1,"paid_from_date")
ldt_paid_to_date 					= idw_dw[1].GetItemDateTime(1,"paid_to_date")
ldec_paid_days_lost 				= idw_dw[1].GetItemDecimal(1,"paid_days_lost")
ldec_paid_hours_lost 				= idw_dw[1].GetItemDecimal(1,"paid_hours_lost")
ll_benefit_calculation_no		  	= idw_dw[1].GetItemDecimal(1,"benefit_calculation_no")
ll_nmbr_cycles 						= idw_dw[1].GetItemNumber(1,"nmbr_cycles")
ls_payment_adjustment_flag 	= idw_dw[1].GetItemString(1,"payment_adjustment_flag")
ldec_total_payment_amount	= idw_dw[1].GetItemDecimal(1,'total_payment_amount')
ldec_deduction_amount 			= idw_dw[1].GetItemDecimal(1,'total_deductions')

idw_dw[1].GetChild('benefit_calculation_no',ldwc_child)
ll_benefit_rownum 				= ldwc_child.Find("benefit_calculation_no = "+string(ll_benefit_calculation_no),1,ldwc_child.RowCount())
ls_award_freq_code 				= ldwc_child.GetItemString(ll_benefit_rownum,"award_freq_code")
ldec_bencalc_award_amount	= ldwc_child.GetItemDecimal(ll_benefit_rownum,"award_amount")

IF ll_benefit_calculation_no = 0 THEN
	SetNull(ldtm_effective_date)
ELSE
	ldtm_effective_date = ldwc_child.GetItemDateTime(ll_benefit_rownum,'effective_from_date')
END IF

IF (idw_basic_claim.GetItemString(1,'claim_status_code') = 'F' AND (idw_basic_claim.GetItemString(1,'claim_status_type_code') = '03' OR idw_basic_claim.GetItemString(1,'claim_status_type_code') = '04')) THEN
	IF ls_old_payment_type_code <> '.4' THEN
		MessageBox('Scheduled Payment','Scheduled payments other than Loss of Opportunity Lump Sum (.4) can not be maintained due to the status of the claim.', Exclamation!)
		RETURN -1
	END IF
END IF

IF ls_payment_type_code = '.4' THEN
	SELECT impaired_determination_date, percent_impaired
	INTO    :ldt_impaired, :ldec_impaired
	FROM   CLAIM
	WHERE claim_no = :il_claim_no
	USING  SQLCA;
	
	SQLCA.nf_handle_error('SELECT impaired_determination_date, percent_impaired FROM CLAIM ','n_payment','nf_check_bus_rule')
	
	IF IsNull(ldt_impaired) OR ldec_impaired = 0 THEN
		MessageBox("Information Not Entered","The PPI Determination Date and % PPI must be entered on the Claim before a '.4' payment can be added.", Information!)
		RETURN -1
	END IF
	
END IF

// P10261 RTW Incentive project
CHOOSE CASE ls_payment_type_code
	CASE 'R1','R2','R3'
		li_rtn = nf_check_rtw_bus_rules(ls_payment_type_code,ldec_total_payment_amount,ldec_deduction_amount,ldec_bencalc_award_amount,ldt_paid_from_date,ldt_paid_to_date,ldtm_effective_date,ll_nmbr_cycles)
		IF li_rtn < 0 THEN RETURN -1
END CHOOSE


ldt_server_date = f_server_datetime()
ld_server_date = Date(ldt_server_date)

IF idw_dw[1].dataobject = 'd_payment_details' THEN
	li_rtn = idw_dw[1].GetChild('payment_type_and_sub_type', ldwc_child)
ELSE
	li_rtn = idw_dw[1].GetChild('payment_type_code', ldwc_child)
END IF

ll_result = ldwc_child.RowCount()
ll_result = ldwc_child.Find("payment_type_code = '"+ls_payment_type_code+"'",1,ldwc_child.RowCount())

IF ll_result <= 0 or IsNull(ll_result) THEN
	MessageBox("Payment Module - Validation Error","Payment Type must be a valid.",Exclamation!)
	idw_dw[1].SetFocus()
	idw_dw[1].SetColumn("payment_type_code")
	Return -1
END IF


ll_txn_rowcount = idw_dw[2].RowCount()
IF ll_txn_rowcount > 1 THEN
	IF nf_check_dup_recipients() < 0 THEN
		Return -1
	END IF
END IF

SELECT active_flag, days_hours_flag
INTO	 :ls_active, :ls_days_hours_flag
FROM   Payment_Type 
WHERE	 payment_type_code = :ls_payment_type_code
USING	 SQLCA;

li_return = SQLCA.nf_handle_error('Embedded SQL: select from PAYMENT','n_payment','nf_check_bus_rule')


IF ls_days_hours_flag = 'N' THEN
	IF ldec_paid_days_lost <> 0 OR ldec_paid_hours_lost <> 0 THEN
		MessageBox('Payment Maintenance Validation Error','The chosen payment type does not allow paid days or hours to be entered.' &
			+ '~r~nPlease clear the days and hours or select another Payment Type.',StopSign!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn("payment_type_code")
		RETURN -1
	END IF
END IF

//PR6331		  
If ldec_deduction_amount  > 0 Then
	MessageBox('Payment Module - Validation Error','Deduction amount must be a negative number.',Exclamation!)
	idw_dw[1].SetColumn("total_deductions")
	idw_dw[1].SetFocus()
	RETURN -1
END IF

IF idw_dw[1].GetItemStatus(idw_dw[1].GetRow(), 0, Primary!) = New! OR &
	+ idw_dw[1].GetItemStatus(idw_dw[1].GetRow(), 0, Primary!) = NewModified! OR &
	+ idw_dw[1].GetItemStatus(idw_dw[1].GetRow(), "payment_type_code", Primary!) = DataModified! THEN

	IF ls_active = 'N' THEN
		MessageBox('Inactive Payment Type','The entered Payment Type is inactive and is not available for use.' &
		+ '~r~nPlease select another Payment Type.',Information!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn("payment_type_code")
		RETURN -1
	END IF
	
	IF IsNull(ls_payment_sub_type_code) THEN ls_payment_sub_type_code = ""
	
	SELECT active_flag
	INTO	 :ls_active_sub
	FROM	 Payment_Sub_Type
	WHERE	 payment_type_code = :ls_payment_type_code
	AND    payment_sub_type_code = :ls_payment_sub_type_code
	USING  SQLCA;

	li_return = SQLCA.nf_handle_error('Embedded SQL: select from PAYMENT','n_payment','nf_check_bus_rule')

	IF ls_active_sub = 'N' THEN	
		MessageBox('Inactive Payment Sub Type','The entered Payment Sub Type is inactive and is not available for use.' &
		+ '~r~nPlease select another Payment Type.',Information!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn("payment_type_code")
		RETURN -1
	END IF
		
END IF

/*
Sr 116 Wa & Ta Payments - New rules Start
1)	Wa & Ta type payments must have comments
2)	Wa must have recipent type of Individual
3)	Ta must have recipent type of Voc Rehab Payee

PR6297 20080514
BR 4.30 - The ‘Recipient Type’ must be an Individual (‘I’) if the payment is Weekly Allowance or Survivors Special Payment (‘WA’ or ‘S1’).
For a split payment, if the first recipient is individual, it will allow other recipient types for the other txns in the split payment.

BR 4.50 - The ‘Recipient Type’ must be a ‘Voc Rehab Payee’ if the payment type is Training Agreement (‘TA’).
This BR can fail for the same reason as BR 4.30 - it only validates the first txn in a split payment.

*/

CHOOSE CASE ls_payment_type_code
	CASE "WA", "S1"
			//Wa & Ta type payments must have comments
			IF ls_payment_type_code = "WA" THEN 
				IF ls_payment_sub_type_code = "" AND (trim(idw_dw[1].getitemstring(1,"explanation")) = "" or isnull(idw_dw[1].getitemstring(1,"explanation"))) THEN
					ls_err_msg = "You must supply a comment for a payment type code of Weekly Allowance ~n"
				END IF
			END IF 
			
			//BR 4.30 - The ‘Recipient Type’ must be an Individual (‘I’) if the payment is Weekly Allowance or Survivors Special Payment (‘WA’ or ‘S1’).
			FOR li_counter = 1 TO  idw_dw[2].rowcount()
				IF idw_dw[2].GetItemString(li_counter,"recipient_type_code") <> "I" THEN
					ls_err_msg = ls_err_msg + "The only valid recipient type for this payment type is Individual."
					EXIT
				END IF
			NEXT	
	CASE "TA"
			//	Wa & Ta type payments must have comments
			IF trim(idw_dw[1].getitemstring(1,"explanation")) = "" or isnull(idw_dw[1].getitemstring(1,"explanation")) THEN
				ls_err_msg = "You must supply a comment for this Payment Type Code ~n"
			END IF
				
			//BR 4.50 - The ‘Recipient Type’ must be a ‘Voc Rehab Payee’ if the payment type is Training Agreement (‘TA’).
			FOR li_counter = 1 TO  idw_dw[2].rowcount()
				IF idw_dw[2].GetItemString(li_counter,"recipient_type_code") <> "V" THEN
					ls_err_msg = ls_err_msg + "The only valid recipient type for this payment type is Voc Rehab Payee."
				     EXIT
				END IF
			NEXT
END CHOOSE
	
//end with a messagebox
IF trim(ls_err_msg) <> "" THEN
	messagebox ("Payment Type Error",ls_err_msg)
	idw_dw[1].SetFocus()
	idw_dw[1].SetColumn('payment_type_code')
	RETURN -1
END IF 
/*
Sr 116 New rules End
*/
ls_authorization_type_code 	= ldwc_child.GetItemString(ll_result,'authorization_type_code')
ls_fromto_dates_required_flag = ldwc_child.GetItemString(ll_result,"fromto_dates_flag")
ls_days_required_flag         = ldwc_child.GetItemString(ll_result,"days_hours_flag")
ll_opening_no = idw_dw[1].GetItemNumber(1,'opening_no')
IF ll_opening_no > 0 THEN
	idw_dw[1].GetChild('opening_no',ldwc_child)
	ll_opening_row         = ldwc_child.Find('opening_no = '+String(ll_opening_no),0,ldwc_child.RowCount())
	ls_opening_type_code   = ' '
	ls_three_day_paid_flag = ' '
	IF ll_opening_row > 0 THEN
		ldtm_benefit_start     = ldwc_child.GetItemDateTime(ll_opening_row,'benefit_start_date')
		ldtm_benefit_end       = ldwc_child.GetItemDateTime(ll_opening_row,'benefit_end_date')
		ls_opening_type_code   = ldwc_child.GetItemString(ll_opening_row,'opening_type_code')
		ls_three_day_paid_flag = ldwc_child.GetItemString(ll_opening_row,'three_day_paid_flag')
	END IF
END IF
IF ls_days_required_flag = "N" &
	AND  NOT (ls_payment_type_code = '3D' OR ls_payment_type_code = 'R2' OR ls_payment_type_code = 'R3') &
	AND  NOT (ls_opening_type_code = 'PEN'  OR ls_opening_type_code = 'SV' or ls_opening_type_code = 'S1' or ls_opening_type_code = 'S2') THEN
	idw_dw[1].SetItem(1,"benefit_calculation_no",0)
	ll_benefit_calculation_no = 0
	ls_award_freq_code        = "W"
END IF

IF ls_fromto_dates_required_flag = "Y" THEN
/*	IF payment type requires from/to dates, check they are entered*/
	ld_paid_from_date = Date(ldt_paid_from_date)
	ld_paid_to_date   = Date(ldt_paid_to_date)
	
	IF ll_opening_no > 0 THEN /*	validate against opening period*/
		
		IF ll_opening_row > 0 THEN
			
			IF ld_paid_from_date < Date(ldtm_benefit_start) THEN
				MessageBox('Invalid Paid From Date','The from date must be on or after the benefit start date for the opening.')
				idw_dw[1].SetFocus()
				idw_dw[1].SetColumn('paid_from_date')
				Return -1
			END IF
			
			IF NOT IsNull(ldtm_effective_date) THEN
				
				IF ld_paid_from_date < Date(ldtm_effective_date) THEN
					MessageBox('Invalid Paid From Date','The from date must be after the effective date of the benefit calculation.')
					idw_dw[1].SetFocus()
					idw_dw[1].SetColumn('paid_from_date')
					Return -1
				END IF
				
			END IF
			
			IF NOT IsNull(ldtm_benefit_end) AND ls_payment_type_code <> 'R1' THEN
				IF ld_paid_from_date > Date(ldtm_benefit_end) THEN
					MessageBox('Invalid Paid From Date','The from date must be before the benefit end date for the opening.')
					idw_dw[1].SetFocus()
					idw_dw[1].SetColumn('paid_from_date')
					Return -1
				END IF
				IF ld_paid_to_date > Date(ldtm_benefit_end) THEN
					MessageBox('Invalid Paid To  Date','The to date must be before the benefit end date for the opening.')
					idw_dw[1].SetFocus()
					idw_dw[1].SetColumn('paid_from_date')
					Return -1
				END IF
			END IF
			
		END IF
		
	END IF
	
/*		For cyclical payments, the period end date must be a complete cycle 	ie. must be 14 days after start date for weekly payments, 30 days after start date for monthly payments*/
	IF ll_nmbr_cycles > 1 THEN
		IF ls_award_freq_code = "W" and ld_paid_to_date <> RelativeDate(ld_paid_from_date,14) THEN
			MessageBox("Payment Module - Validation Error","For cyclical payments, the period end date must be 14 days after the start date"+"~r~nIt must be set to "+string(RelativeDate(ld_paid_from_date,14)),Exclamation!)
			idw_dw[1].SetFocus()
			idw_dw[1].SetColumn("paid_to_date")
			Return -1
		ELSEIF ls_award_freq_code = "M" THEN
			IF Day(ld_paid_from_date) <> Day(ld_paid_to_date) or Day(ld_paid_from_date) <> 1 or Day(ld_paid_to_date) <> 1 THEN
				MessageBox("Payment Module - Validation Error","For monthly cyclical payments, the period dates must be the first of 1 month to the first of the next month"+"~r~nThe day must be set to 1 or change the number of cyclical payments to 1",Exclamation!)
				idw_dw[1].SetFocus()
				idw_dw[1].SetColumn("paid_to_date")
				Return -1
			END IF
			IF	((Month(ld_paid_from_date)+1) <> Month(ld_paid_to_date)) and (Month(ld_paid_from_date) <> 12 and Month(ld_paid_to_date) <> 1) THEN
				MessageBox("Payment Module - Validation Error","For monthly cyclical payments, the period end date must be 1 month after the start date"+"~r~nThe month must be set to "+string(Month(ld_paid_from_date)+1),Exclamation!)
				idw_dw[1].SetFocus()
				idw_dw[1].SetColumn("paid_to_date")
				Return -1
			END IF
		END IF
	END IF
/*		Period to date cannot be before period from date*/
/* SR70 - Special Survivor Payment - the Period to date & from date must be the same date */
	ll_days_after = DaysAfter(ld_paid_from_date,ld_paid_to_date)

	IF ls_payment_type_code = 'S1' AND ll_days_after <> 0 THEN
		MessageBox("Validation Error","The period end date must be equal to the period start date for Special Surv. Payments",Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn("paid_to_date")
		Return -1
	ELSE	
		IF ll_days_after <= 0 AND ls_payment_type_code <> 'S1' THEN
			MessageBox("Validation Error","The period end date cannot be less than or equal to the period start date",Exclamation!)
			idw_dw[1].SetFocus()
			idw_dw[1].SetColumn("paid_to_date")
			Return -1
		END IF
	END IF	
/*		IF number of days required, it cannot exceed total number of days in period.IF it's a monthly payment THEN number of days must be 30 unless the monthly payment happens to be just to bring the cycle up to the first of the month */
	idw_dw[1].GetChild('benefit_calculation_no',ldwc_child)
	IF ls_days_required_flag = "Y" THEN
		
		IF (ldec_paid_days_lost + ldec_paid_hours_lost) <= 0 AND ls_payment_adjustment_flag = 'N' THEN
			MessageBox("Validation Error","Total days plus hours paid cannot equal zero for this payment type.",Exclamation!)
			idw_dw[1].SetFocus()
			idw_dw[1].SetColumn("paid_days_lost")
			Return -1
		END IF
		
		IF ls_award_freq_code = "W" THEN
			ldec_work_hours_per_day = ldwc_child.GetItemDecimal(ll_benefit_rownum,"preacc_work_hours_per_day")
			ldec_temp_days = (ldec_paid_hours_lost / ldec_work_hours_per_day)+ldec_paid_days_lost
			IF ldec_temp_days > ll_days_after THEN
				MessageBox("Validation Error","Total days plus hours paid cannot exceed total number of days in the payment period",Exclamation!)
				idw_dw[1].SetFocus()
				idw_dw[1].SetColumn("paid_days_lost")
				Return -1
			END IF
		ELSE
			IF Day(ld_paid_from_date) <> Day(ld_paid_to_date) THEN
				ldec_work_hours_per_day = ldwc_child.GetItemDecimal(ll_benefit_rownum,"preacc_work_hours_per_day")
				ldec_temp_days = (ldec_paid_hours_lost / ldec_work_hours_per_day)+ldec_paid_days_lost
				IF ldec_temp_days > ll_days_after THEN
					IF NOT (Month(ld_paid_from_date) = 2 AND ls_opening_type_code = 'LTD') THEN
						MessageBox("Validation Error","Total days plus hours paid cannot exceed total number of days in the payment period",Exclamation!)
						idw_dw[1].SetFocus()
						idw_dw[1].SetColumn("paid_days_lost")
						Return -1
					END IF
				END IF
			END IF
			IF	(Month(ld_paid_from_date) <> (Month(ld_paid_to_date)+1)) and (Month(ld_paid_from_date) <> 12 and Month(ld_paid_to_date) <> 1) THEN
				ldec_work_hours_per_day = ldwc_child.GetItemDecimal(ll_benefit_rownum,"preacc_work_hours_per_day")
				ldec_temp_days = (ldec_paid_hours_lost / ldec_work_hours_per_day)+ldec_paid_days_lost
				IF ldec_temp_days > ll_days_after THEN
					IF NOT (Month(ld_paid_from_date) = 2 AND ls_opening_type_code = 'LTD') THEN
						MessageBox("Validation Error","Total days plus hours paid cannot exceed total number of days in the payment period",Exclamation!)
						idw_dw[1].SetFocus()
						idw_dw[1].SetColumn("paid_days_lost")
						Return -1
					END IF
				END IF
			END IF
		END IF
	END IF
	
	IF ll_nmbr_cycles > 1 THEN
		IF ls_award_freq_code = "W" THEN
			ld_temp_date = RelativeDate(ld_paid_to_date,14 * (ll_nmbr_cycles - 1))
		ELSE
			ld_temp_date = RelativeDate(ld_paid_to_date,30 * (ll_nmbr_cycles - 1))
		END IF
	ELSE
		ld_temp_date = ld_paid_to_date
	END IF
	
	IF ls_payment_type_code = '-7' THEN
		SELECT Distinct DATEADD(mm, 6, GetDate())
	 	INTO :ldt_LTD_6month_date
		FROM sysobjects
		USING SQLCA;
		
		IF DaysAfter (Date(ldt_LTD_6month_date),ld_temp_date ) > 0 THEN
			MessageBox("Action Date",	"The Period 'To' date cannot be more than 6 months in the future for a '-7' payment." &
										+	"~n                               Please correct the date.",StopSign!)
			RETURN -1
		END IF
	END IF
	

	IF ld_temp_date > RelativeDate(ld_server_date,49) AND ls_payment_type_code <> '-7' THEN
		MessageBox("Validation Error","The paid to date cannot expand to more than 7 weeks in the future~r~n"+"Either change the paid to date or reduce the number of cycles",Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn("paid_to_date")
		Return -1
	END IF

	
/*		IF more than one cycle and payment is a monthly payment THEN validate last paid to date against end date*/
	IF ll_nmbr_cycles > 1 and ls_award_freq_code = "M" THEN
		ld_temp_date = ld_paid_to_date
		IF (Month(ld_temp_date)+(ll_nmbr_cycles - 1)) > 12 THEN
			ld_temp_date = Date(Year(ld_temp_date)+1,Month(ld_temp_date)+(ll_nmbr_cycles -1) -12,Day(ld_temp_date))
		ELSE
			ld_temp_date = Date(Year(ld_temp_date),Month(ld_temp_date)+(ll_nmbr_cycles -1),Day(ld_temp_date))
		END IF
	END IF
END IF

/*	Last scheduled processing date cannot be more than 7 weeks in future (ie. only 3 cycles are allowed + 1 weeks grace). 
	For monthly payments allow up to 16 cycles*/
ldt_scheduled_processing_date = idw_dw[1].GetItemDateTime(1,"scheduled_processing_date")
IF ll_nmbr_cycles > 1 Then
	IF ls_award_freq_code = "W" THEN
		ld_temp_date = RelativeDate(Date(ldt_scheduled_processing_date),14 * (ll_nmbr_cycles - 1))
	ELSE
		ld_temp_date = RelativeDate(Date(ldt_scheduled_processing_date),30 * (ll_nmbr_cycles - 1))
	END IF
ELSE
	ld_temp_date = Date(ldt_scheduled_processing_date)
END IF

IF ls_payment_type_code = '-7' THEN	
	IF DaysAfter (Date(ldt_LTD_6month_date),ld_temp_date ) > 0 THEN
		MessageBox("Action Date",	"The Order Date cannot be more than 6 months in the future for a '-7' payment." &
										+	"~n                               Please correct the date.",StopSign!)
		RETURN -1
	END IF
END IF
	
IF ld_temp_date > RelativeDate(ld_server_date,49) AND ls_payment_type_code <> '-7' THEN
	MessageBox("Validation Error","The order date cannot expand to more than 7 weeks in the future~r~n"+"Either change the order date or reduce the number of cycles",Exclamation!)
	idw_dw[1].SetFocus()
	idw_dw[1].SetColumn("scheduled_processing_date")
	Return -1
END IF

/*	Total payment amount can exceed users' authorization limit but will be held until authorized */
ldec_total_payment_amount = idw_dw[1].GetItemDecimal(1,"total_payment_amount")
ldec_original_payment_amount = idw_dw[1].GetItemDecimal(1,"total_payment_amount", PRIMARY!, TRUE)
ic_authorization_limit    = nf_get_authorization_limit(ls_authorization_type_code)
ls_group_flag = idw_dw[1].GetItemString(1,'authorization_group_flag')
IF ls_group_flag = '' OR IsNull(ls_group_flag) THEN ls_group_flag = 'N'
ll_group_no = idw_dw[1].GetItemNumber(1, 'authorization_group_no')

IF ll_group_no > 0 THEN
	ll_group_rows = idw_dw[4].Retrieve(ll_group_no)
	
	IF ll_group_rows > 0 THEN
		ldec_total_group_amount = idw_dw[4].GetItemDecimal(1, 'total_amount_for_group')
		ldec_total_group_amount = ldec_total_group_amount - ldec_original_payment_amount + ldec_total_payment_amount
	END IF
END IF

IF ic_authorization_limit = -1 Then
	RETURN -1
END IF

IF ls_group_flag = 'N' THEN
	IF ldec_total_payment_amount > ic_authorization_limit THEN 
		MessageBox("Payment Module","You are only authorized to make payments up to "+string(ic_authorization_limit,'$###,##0.00')+"~r~nThis payment will be held until it is authorized.",Exclamation!)
	END IF
ELSE 
	IF ldec_total_group_amount > ic_authorization_limit THEN
		MessageBox("Payment Module","You are only authorized to make payments up to "+string(ic_authorization_limit,'$###,##0.00')+"~r~nThis payment belongs to an authorization group which be held until it is authorized.",Exclamation!)
	END IF
END IF

/*	Validate transaction */
ldec_temp_txn_amount = 0
ll_count             = idw_dw[2].RowCount()
ll_no                = 1
DO WHILE ll_no <= ll_count
	ldec_temp_txn_amount += idw_dw[2].GetItemDecimal(ll_no,"txn_amount")
	ll_no ++
LOOP
/*	Total of amounts payable to recipients must be equal to payment amount */
IF ldec_temp_txn_amount <> ldec_total_payment_amount THEN
	MessageBox("Payment Module - Validation Error","Amount payable to recipients do not equal total payment amount",Exclamation!)
	idw_dw[2].SetFocus()
	idw_dw[2].SetColumn("txn_amount")
	Return -1
END IF
IF ls_fromto_dates_required_flag = "Y" THEN  /*	Warn if order date is more than 4 days before period end date */
	IF DaysAfter(Date(ldt_scheduled_processing_date),ld_paid_to_date) > 4 and ls_award_freq_code = 'D' THEN
		IF MessageBox("Payment Module - Warning","This is only a warning~r~n~r~nThe order date is more that 4 days "+"before the period end date~r~nDo you wish to save the payment anyway?",Question!,YesNo!) = 2 THEN
			Return -1
		END IF
	END IF
END IF


/* Warning for issuing 3 day payment when it is not applicable */
/*BR 2.200*/
IF (idw_dw[1].GetItemString(1,"payment_type_code") = '3D') AND ls_three_day_paid_flag = 'I' THEN
		IF MessageBox ("Payment Module - Warning","This is only a warning~r~n~r~nThe three day deduction "+"and reimbursement is INAPPLICABLE for this opening~r~nDo you wish to save the payment anyway?",Question!,YesNo!) = 2 THEN
			Return -1
		END IF
END IF

//Show warning message if there has already been a '3D' payment created
//for the claim.
/*BR 2.101*/
IF ls_payment_type_code = '3D' Then
	Select count(*)
	into :ll_3D_payment_count
	FROM PAYMENT
	WHERE payment_type_code = '3D'
	and   claim_no = :il_claim_no
	USING SQLCA;
	
	If ll_3D_payment_count > 0 Then
		li_rtn = MessageBox('Warning','A 3 day payment already exists for this claim.  Do you want to continue with the save?',Question!,YesNo!)
		IF li_rtn = 2 Then
			RETURN -1
		End if
	End if
End if

// BR 2.42
ldec_benefit_amount = idw_dw[1].GetItemDecimal(1,'total_award_amount')
IF ldec_benefit_amount <= 0 THEN
	SQLCA.nf_rollback_transaction()
	MessageBox('Payment Module - Validation Error','Benefit amount cannot be zero.',Exclamation!)
	Return -1
END IF

ldec_deduction_amount = idw_dw[1].GetItemDecimal(1,'total_deductions')
IF (ldec_benefit_amount + ldec_deduction_amount) < 0 THEN
	SQLCA.nf_rollback_transaction()
	MessageBox('Payment Module - Validation Error','Benefit amount plus deduction amount cannot sum to less than zero.',Exclamation!)
	Return -1
END IF


//Transaction are not maintainable if the total_payment_amount is zero
For li_trans = 1 to idw_dw[2].RowCount()
	IF idw_dw[2].GetItemDecimal(li_trans,'txn_amount') = 0 Then
		ls_maintain_allowed_flag = 'N'
	Else
		ls_maintain_allowed_flag = 'Y'
	End if
	idw_dw[2].SetItem(li_trans,'maintain_allowed_flag',ls_maintain_allowed_flag)
Next

	


/*********AllanM April 19 2000********/
/* Warning for first payment going direct deposit ie. 
only applicable to individual, with benefit_class_code = 'LOE' and claim does not already have an 'LOE' payment */
ls_payment_type_code = idw_dw[1].GetItemString(1,"payment_type_code")

SELECT benefit_class_code
INTO 	:ls_ben_class_code
FROM 	Payment_Type
WHERE payment_type_code = :ls_payment_type_code;

//IF upper(ls_ben_class_code) = 'LOE'  and idw_dw[2].getitemstring(1,'payment_method_code') = 'D' THEN
IF upper(ls_ben_class_code) = 'LOE'  and idw_dw[2].find("payment_method_code = 'D'",1,idw_dw[2].rowcount()) >0 THEN
			
	SELECT count(*)
	INTO 	:ll_count
	FROM 	PAYMENT p, 
			Payment_Type pt
	WHERE p.claim_no = :il_claim_no and
			pt.payment_type_code = p.payment_type_code and

			pt.benefit_class_code = 'LOE' and
			(exists (select * from APPLIED_CLAIM_TXN d
					  where d.payment_no = p.payment_no and
							  d.recipient_type_code = 'I') OR
			exists (select * from UNAPPLIED_CLAIM_TXN e
					  where e.payment_no = p.payment_no and
							  e.recipient_type_code = 'I'))
	USING SQLCA;
	
	ll_result = SQLCA.nf_handle_error('Embedded SQL: select from PAYMENT','n_payment','nf_check_bus_rule')
	
	IF ll_count <= 0 THEN				
		li_return = messagebox("Payment Module - Warning", "There is Banking information available for this Individual. (It may be out of date)~r" + &
		"Do you wish to use this information for Direct Deposit anyway?", Question!, YesNO!, 2)
		IF li_return = 2 THEN				
			RETURN -1
		END IF
	END IF
END IF


/************************************/

/*	Database Updates                                                                                     
Note: IF the payment is not a cyclical payment, we have to move in the key fields and THEN save.
However, if the payment is a cyclical payment we have to insert a record for each cycle past  
the first one (as the first one has already been inserted).  We also have to move all the fields from
the first record over to new records -- but, be careful with the dates as the second and on records 
represents future payments.  Also, for each recipient, a new record must be created for each cycle past one.
Ie. IF there are 3 recipients and the number of cycles is 3, we would end up with 3 PAYMENT records and
9 UNAPPLIED_CLAIM_TXN records!  */
ll_transaction_rowcount = idw_dw[2].RowCount()
IF idw_dw[1].GetItemStatus(1,0,Primary!) = NewModified! THEN // new payment
	ll_payment_cntr = 1
	ll_payment_no = nf_get_next_payment_identifier()
	IF ll_payment_no < 1 THEN
		Return -1
	END IF
	DO WHILE ll_payment_cntr <= ll_nmbr_cycles
		IF ll_payment_cntr > 1 THEN
			ll_payment_row = idw_dw[1].InsertRow(0)
			IF ll_payment_row = -1 THEN
				Error.Text        = "Error inserting row into idw_dw[1]"
				Error.Object      = "w_payments"
				Error.ObjectEvent = "clicked for cb_save"
				SignalError()
			ELSE
				ll_payment_no = nf_get_next_payment_identifier()
				IF ll_payment_no < 1 THEN Return -1
				
			END IF
			IF ls_fromto_dates_required_flag = "Y" THEN
				IF ls_award_freq_code = "W" THEN
					ldt_paid_from_date  = DateTime(RelativeDate(Date(ldt_paid_from_date),14))
					ldt_paid_to_date    = DateTime(RelativeDate(Date(ldt_paid_to_date),14))
				ELSE
					ldt_paid_from_date  = ldt_paid_to_date
					IF Month(Date(ldt_paid_to_date)) = 12 THEN
						ldt_paid_to_date = DateTime(Date(Year(Date(ldt_paid_to_date))+1,1,1))
					ELSE
						ldt_paid_to_date = DateTime(Date(Year(Date(ldt_paid_to_date)),Month(Date(ldt_paid_to_date))+1,1))
					END IF
				END IF
			END IF
			//validate any period dates for cycles after the first one.
			IF nf_check_dates(ldt_paid_to_date,ldt_paid_from_date,ldtm_benefit_end,ll_payment_cntr, ls_award_freq_code) < 0 THEN
				SQLCA.nf_rollback_transaction()
				idw_dw[1].SetFocus()
				idw_dw[1].SetColumn("paid_to_date")
				Return -1
			END IF
			
			If idw_dw[2].GetItemDecimal(ll_tranrow,'txn_amount') = 0 Then
				If idw_dw[2].GetItemString(ll_tranrow,'payment_method_code') <> 'I' Then
					idw_dw[2].SetItem(ll_tranrow,'payment_method_code','I')
					idw_dw[2].SetItem(ll_tranrow,'use_default_address_flag','Y')
					
					
					ls_recipient_type_code = idw_dw[2].GetItemString(ll_tranrow,'recipient_type_code')
					ll_recipient_no = idw_dw[2].GetItemNumber(ll_tranrow,'recipient_no')
					ls_cheque_print_group_code = idw_dw[2].GetItemString(ll_tranrow,'cheque_print_group_code')
					nf_setup_address(ls_recipient_type_code,ll_recipient_no,ll_tranrow,ls_cheque_print_group_code)
				End if
			ElseIf idw_dw[2].GetItemDecimal(ll_tranrow,'txn_amount') <> 0 and idw_dw[2].GetItemString(ll_tranrow,"payment_method_code") = 'I' Then
				SQLCA.nf_rollback_transaction()
				MessageBox('Invalid payment method','The payment method cannot be "Inapplicable" unless the transaction amount is $0.00')
				return -1		
			End if
			
			
			
			IF ls_award_freq_code = "W" THEN
				ldt_scheduled_processing_date = DateTime(RelativeDate(Date(ldt_scheduled_processing_date),14))
			ELSE
				IF Month(Date(ldt_scheduled_processing_date)) = 12 THEN
					ldt_scheduled_processing_date = DateTime(Date(Year(Date(ldt_scheduled_processing_date))+1,1,23))
				ELSEIF Month(Date(ldt_scheduled_processing_date)) = 11 THEN
					ldt_scheduled_processing_date = DateTime(Date(Year(Date(ldt_scheduled_processing_date)),Month(Date(ldt_scheduled_processing_date))+1,10))
				ELSE
					ldt_scheduled_processing_date = DateTime(Date(Year(Date(ldt_scheduled_processing_date)),Month(Date(ldt_scheduled_processing_date))+1,23))
				END IF
			END IF
			idw_dw[1].SetItem(ll_payment_row,"opening_no",idw_dw[1].GetItemNumber(1,"opening_no"))
			idw_dw[1].SetItem(ll_payment_row,"benefit_calculation_no",idw_dw[1].GetItemNumber(1,"benefit_calculation_no"))
			idw_dw[1].SetItem(ll_payment_row,"payment_type_code",idw_dw[1].GetItemString(1,"payment_type_code"))
			idw_dw[1].SetItem(ll_payment_row,"paid_days_lost",ldec_paid_days_lost)
			idw_dw[1].SetItem(ll_payment_row,"paid_hours_lost",ldec_paid_hours_lost)
			idw_dw[1].SetItem(ll_payment_row,"paid_from_date",ldt_paid_from_date)
			idw_dw[1].SetItem(ll_payment_row,"paid_to_date",ldt_paid_to_date)
			idw_dw[1].SetItem(ll_payment_row,"total_award_amount",idw_dw[1].GetItemDecimal(1,"total_award_amount"))
			idw_dw[1].SetItem(ll_payment_row,"total_deductions",idw_dw[1].GetItemDecimal(1,"total_deductions"))
			idw_dw[1].SetItem(ll_payment_row,"total_payment_amount",ldec_total_payment_amount)
			idw_dw[1].SetItem(ll_payment_row,"final_payment_flag",idw_dw[1].GetItemString(1,"final_payment_flag"))
			idw_dw[1].SetItem(ll_payment_row,"payment_adjustment_flag",idw_dw[1].GetItemString(1,"payment_adjustment_flag"))
			idw_dw[1].SetItem(ll_payment_row,'paid_quantity',idw_dw[1].GetItemNumber(1,'paid_quantity'))
			idw_dw[1].SetItem(ll_payment_row,'tax_amount',idw_dw[1].GetItemDecimal(1,'tax_amount'))
			idw_dw[1].SetItem(ll_payment_row,'tax_rate',idw_dw[1].GetItemDecimal(1,'tax_rate'))
			idw_dw[1].SetItem(ll_payment_row,'authorization_no',idw_dw[1].GetItemNumber(1,'authorization_no'))
			idw_dw[1].SetItem(ll_payment_row,'adjustment_tax_amount',idw_dw[1].GetItemDecimal(1,'adjustment_tax_amount'))
			idw_dw[1].SetItem(ll_payment_row,'adjustment_payment_amount',idw_dw[1].GetItemDecimal(1,'adjustment_payment_amount'))
			idw_dw[1].SetItem(ll_payment_row,'payment_adjustment_flag',idw_dw[1].GetItemString(1,'payment_adjustment_flag'))
			idw_dw[1].SetItem(ll_payment_row,'total_deductions',idw_dw[1].GetItemDecimal(1,'total_deductions'))
			idw_dw[1].SetItem(ll_payment_row,'final_payment_flag',idw_dw[1].GetItemString(1,'final_payment_flag'))
			idw_dw[1].SetItem(ll_payment_row,'award_no',0)
			idw_dw[1].SetItem(ll_payment_row,'benefit_calculation_no',idw_dw[1].GetItemNumber(1,'benefit_calculation_no'))
			idw_dw[1].SetItem(ll_payment_row,'loe_explanation',idw_dw[1].GetItemString(1,'loe_explanation'))
			idw_dw[1].SetItem(ll_payment_row,'submitted_amount',0)
			idw_dw[1].SetItem(ll_payment_row,'payment_sub_type_code',idw_dw[1].GetItemString(1,'payment_sub_type_code'))
		ELSE
			ll_payment_row = ll_payment_cntr
		END IF
		idw_dw[1].SetItem(ll_payment_row,"payment_no",ll_payment_no)
		idw_dw[1].SetItem(ll_payment_row,"claim_no",il_claim_no)
		
		IF ls_group_flag = 'N' THEN
			IF ldec_total_payment_amount <= ic_authorization_limit THEN
				idw_dw[1].SetItem(ll_payment_row,"authorized_by_code",vgst_user_profile.user_id)
				idw_dw[1].SetItem(ll_payment_row,"authorized_date",ldt_server_date)
			ELSE
				idw_dw[1].SetItem(ll_payment_row,"authorized_by_code",'')
				idw_dw[1].SetItem(ll_payment_row,"authorized_date",ld_null_date)
			END IF
		ELSE
			idw_dw[1].SetItem(ll_payment_row,"authorized_by_code",'')
			idw_dw[1].SetItem(ll_payment_row,"authorized_date",ld_null_date)
		END IF
					
		ll_recipient_cntr = 1
		DO WHILE ll_recipient_cntr <= ll_transaction_rowcount
			ll_txn_no = nf_get_next_txn_identifier()
			IF ll_txn_no < 1 	THEN Return -1

			IF ll_payment_cntr > 1 THEN
				ll_tranrow = idw_dw[2].InsertRow(0)
				IF ll_tranrow = -1 THEN
					Error.Text        = "Error inserting row into idw_dw[2]"
					Error.Object      = "w_payments"
					Error.ObjectEvent = "clicked for cb_save"
					SignalError()
				END IF
				idw_dw[2].SetItem(ll_tranrow,"recipient_no",idw_dw[2].GetItemNumber(ll_recipient_cntr,"recipient_no"))
				idw_dw[2].SetItem(ll_tranrow,"recipient_type_code",idw_dw[2].GetItemString(ll_recipient_cntr,"recipient_type_code"))
				idw_dw[2].SetItem(ll_tranrow,"cheque_no",idw_dw[2].GetItemNumber(ll_recipient_cntr,"cheque_no"))
				idw_dw[2].SetItem(ll_tranrow,"manual_cheque_req_no",idw_dw[2].GetItemNumber(ll_recipient_cntr,"manual_cheque_req_no"))
				idw_dw[2].SetItem(ll_tranrow,"cheque_deposit_date",idw_dw[2].GetItemDateTime(ll_recipient_cntr,"cheque_deposit_date"))
				idw_dw[2].SetItem(ll_tranrow,"payment_method_code",idw_dw[2].GetItemString(ll_recipient_cntr,"payment_method_code"))
				idw_dw[2].SetItem(ll_tranrow,"txn_amount",idw_dw[2].GetItemDecimal(ll_recipient_cntr,"txn_amount"))
				idw_dw[2].SetItem(ll_tranrow,"recipient_name",idw_dw[2].GetItemString(ll_recipient_cntr,"recipient_name"))
				idw_dw[2].SetItem(ll_tranrow,'maintain_allowed_flag',idw_dw[2].GetItemString(ll_recipient_cntr,'maintain_allowed_flag'))
				idw_dw[2].SetItem(ll_tranrow,'txn_sub_type_code',idw_dw[2].GetItemString(ll_recipient_cntr,'txn_sub_type_code'))
				idw_dw[2].SetItem(ll_tranrow,'tax_amount',idw_dw[2].GetItemDecimal(ll_recipient_cntr,'tax_amount'))
				idw_dw[2].SetItem(ll_tranrow,'txn_unit_of_work_no',0)
				idw_dw[2].SetItem(ll_tranrow,'direct_deposit_xmit_no',0)
				idw_dw[2].SetItem(ll_tranrow,'related_txn_no',0)
				idw_dw[2].SetItem(ll_tranrow,'cheque_no',0)
				idw_dw[2].SetItem(ll_tranrow,'coc_period',idw_dw[2].GetItemNumber(ll_recipient_cntr,'coc_period'))
				idw_dw[2].SetItem(ll_tranrow,'explanation',idw_dw[2].GetItemString(ll_recipient_cntr,'explanation'))
				
				//IF idw_dw[2].GetItemString(ll_recipient_cntr,"use_default_address_flag") = 'N' THEN
					idw_dw[2].SetItem(ll_tranrow,"address_line1",idw_dw[2].GetItemString(ll_recipient_cntr,"address_line1"))
					idw_dw[2].SetItem(ll_tranrow,"address_line2",idw_dw[2].GetItemString(ll_recipient_cntr,"address_line2"))
					idw_dw[2].SetItem(ll_tranrow,"city",idw_dw[2].GetItemString(ll_recipient_cntr,"city"))
					idw_dw[2].SetItem(ll_tranrow,"prov_state_code",idw_dw[2].GetItemString(ll_recipient_cntr,"prov_state_code"))
					idw_dw[2].SetItem(ll_tranrow,"country",idw_dw[2].GetItemString(ll_recipient_cntr,"country"))
					idw_dw[2].SetItem(ll_tranrow,"postal_code",idw_dw[2].GetItemString(ll_recipient_cntr,"postal_code"))
				//END IF
				
				ls_use_default_address_flag = idw_dw[2].GetItemString(ll_recipient_cntr,"use_default_address_flag")
				idw_dw[2].SetItem(ll_tranrow,"use_default_address_flag",ls_use_default_address_flag)
						
				idw_dw[2].SetItem(ll_tranrow,"cheque_print_group_code" , ' ')
				
			ELSE
				ll_tranrow = ll_recipient_cntr
			END IF
			
			ls_string = idw_dw[2].GetItemString(ll_tranrow,"recipient_type_code")
			IF (ls_string <> 'I' AND ls_string <> 'O' AND ls_string <> 'V' ) THEN
				SQLCA.nf_rollback_transaction()
				MessageBox("Payment Module - Validation Error","Recipient Type - "+ls_string+" is invalid.",Exclamation!)
				idw_dw[2].SetFocus()
				idw_dw[2].SetColumn("recipient_type_code")
				Return -1
			END IF
			
			/* PR 4649:  changed code that checked for a recipient of type 'I' for opening payment type code of 
			   Pensions, SV, S1 or S2 and prohibited payments for anything other than 'I'ndividual in these cases. 
				Business rules now allow that recipient type of Other Payee also should be allowed for Pension 
				payments, so that users can issue a recovery to WHSCC on Overpayment. This PR is also found later in this function and in n_award
			*/			
			IF (ls_opening_type_code = 'PEN' OR ls_opening_type_code = 'SV' OR ls_opening_type_code = 'S1' OR ls_opening_type_code = 'S2') &
								AND idw_dw[2].GetItemString(ll_tranrow,"recipient_type_code") <> 'I' &
								AND idw_dw[2].GetItemString(ll_tranrow,"recipient_type_code") <> 'O'THEN
				SQLCA.nf_rollback_transaction()
				MessageBox("Payment Module - Validation Error","'Claimant/Individual' or 'Other Payee' are the only valid recipient choices for pension payments.",Exclamation!)
				idw_dw[2].SetFocus()
				idw_dw[2].SetColumn("recipient_type_code")
				idw_dw[2].ScrollToRow(ll_tranrow)  // added this so the offending row will be visible after setFocus() occurrs on DataWindow
				Return -1
			END IF						
			
			ll_recipient_no = idw_dw[2].GetItemNumber(ll_tranrow,"recipient_no")
			// SR70 - Special Survivor Payment JAN 2001
			IF (ls_opening_type_code <> 'PEN' AND ls_opening_type_code <> 'SV' and ls_opening_type_code <> 'S1' and ls_opening_type_code <> 'S2') AND idw_dw[2].GetItemString(ll_tranrow,"recipient_type_code") = 'I' AND ll_recipient_no <> idw_basic_claim.GetItemNumber(1,'individual_no') AND ls_payment_type_code <> 'S1' THEN
				SQLCA.nf_rollback_transaction()
				MessageBox("Payment Module - Validation Error","Recipient No - must be that of the claimant.",Exclamation!)
				idw_dw[2].SetFocus()
				idw_dw[2].SetColumn("recipient_no")
				Return -1
			END IF
			
			IF nf_validate_recipient(ls_string,ll_recipient_no,ll_tranrow) < 0 THEN
				SQLCA.nf_rollback_transaction()
				idw_dw[2].SetFocus()
				idw_dw[2].SetColumn("recipient_no")
				Return -1
			END IF
			
			IF idw_dw[2].GetItemString(ll_tranrow,"use_default_address_flag") = 'N' THEN
				IF nf_validate_address() < 0 THEN
					SQLCA.nf_rollback_transaction()
					Return -1
				END IF
			END IF
			
			ls_payment_method_code = idw_dw[2].GetItemString(ll_tranrow,"payment_method_code")
			//RECEIVING SALARY BUSINESS RULES
			IF ls_opening_type_code = 'RLOE' and is_claim_receiving_salary_flag = 'Y' THEN
				CHOOSE CASE ls_payment_type_code
					CASE 'R1'
						CHOOSE CASE ls_payment_method_code
							CASE 'A','D','H'
								// OK
							CASE ELSE
								SQLCA.nf_rollback_transaction()
								MessageBox("Payment Module - Validation Error","Payment method must be 'Automated Cheque', 'Direct Deposit'" +&
																								"~nor 'Hand Written Cheque' when the claim is 'Receiving Salary'" +&
																								"~nand the paymen type is 'Job Search Return to Work Incentive'.")
								idw_dw[2].SetFocus()
								idw_dw[2].SetColumn("payment_method_code")
								Return -1
						END CHOOSE
						
					CASE ELSE
						// not an R1 (job search RTW Incentive) payment
						//BR 4.12
						IF ls_payment_method_code  <> 'R' Then
							SQLCA.nf_rollback_transaction()
							MessageBox("Payment Module - Validation Error","Payment method must be 'Recorded Not Issued - Receiving Salary' when the claim is 'Receiving Salary'.")
							idw_dw[2].SetFocus()
							idw_dw[2].SetColumn("recipient_type_code")
							Return -1
						END IF
				END CHOOSE
								
				//BR 2.61
				IF idw_dw[2].GetItemDecimal(1,'txn_amount') = 0 Then
					SQLCA.nf_rollback_transaction()
					MessageBox("Payment Module - Validation Error","Receiving salary payments cannot be $0.00")
					idw_dw[2].SetFocus()
					idw_dw[2].SetColumn("txn_amount")
					RETURN -1
				END IF
				
				//BR 4.1
				IF ls_payment_method_code  = 'R' and ls_string <> 'I' Then
					SQLCA.nf_rollback_transaction()
					MessageBox("Payment Module - Validation Error","Recipient Type must be 'Individual' for claims that are 'Receiving Salary'.",Exclamation!)
					idw_dw[2].SetFocus()
					idw_dw[2].SetColumn("recipient_type_code")
					Return -1
				END IF	
			Else
				//BR 4.13
				IF ls_payment_method_code  = 'R' Then
					SQLCA.nf_rollback_transaction()
					MessageBox("Payment Module - Validation Error","Payment method cannot be 'Recorded Not Issued - Receiving Salary' unless the claim is 'Receiving Salary' and the Opening is 'RLOE'.")
					idw_dw[2].SetFocus()
					idw_dw[2].SetColumn("recipient_type_code")
					Return -1
				END IF
			End if	
			
		
	
			ll_recipient_no = idw_dw[2].GetItemNumber(ll_tranrow,"recipient_no")
			
			If idw_dw[2].GetItemDecimal(ll_tranrow,'txn_amount') = 0 Then
				If idw_dw[2].GetItemString(ll_tranrow,'payment_method_code') <> 'I' Then
					idw_dw[2].SetItem(ll_tranrow,'payment_method_code','I')
					idw_dw[2].SetItem(ll_tranrow,'use_default_address_flag','Y')
					
					ls_recipient_type_code = idw_dw[2].GetItemString(ll_tranrow,'recipient_type_code')
					ls_cheque_print_group_code = idw_dw[2].GetItemString(ll_tranrow,'cheque_print_group_code')
					nf_setup_address(ls_recipient_type_code,ll_recipient_no,ll_tranrow,ls_cheque_print_group_code)
				End if
			ElseIf idw_dw[2].GetItemDecimal(ll_tranrow,'txn_amount') <> 0 and idw_dw[2].GetItemString(ll_tranrow,"payment_method_code") = 'I' Then
				SQLCA.nf_rollback_transaction()
				MessageBox('Invalid payment method','The payment method cannot be "Inapplicable" unless the transaction amount is $0.00')
				return -1		
			End if
			
			// SR70 - Special Survivor Payment JAN 2001
			IF ls_payment_type_code = 'S1' AND (idw_dw[2].GetItemString(ll_tranrow,"payment_method_code") <> 'A' AND &
				idw_dw[2].GetItemString(ll_tranrow,"payment_method_code") <> 'H') THEN
				SQLCA.nf_rollback_transaction()
				MessageBox("Payment Module - Validation Error","Method - Must be Automated or Handwritten Cheque.",Exclamation!)
				idw_dw[2].SetFocus()
				idw_dw[2].SetColumn("payment_method_code")
				Return -1
			END IF
			
			// BR 4.16
			ld_man_cheque_date = Date(idw_dw[2].GetItemDateTime(ll_tranrow,'cheque_deposit_date'))
			IF DaysAfter( ld_server_date , ld_man_cheque_date ) > 7 THEN
				SQLCA.nf_rollback_transaction()
				MessageBox('Payment Module - Validation Error','Issue date must be less than or equal to one week in future.',Exclamation!)
				Return -1
			ELSEIF DaysAfter( ld_server_date , ld_man_cheque_date ) < 0 THEN
				SQLCA.nf_rollback_transaction()
				MessageBox('Payment Module - Validation Error','Issue date cannot be in the past.',Exclamation!)
				Return -1
			ELSEIF DaysAfter( ld_server_date , ld_man_cheque_date ) > 0 THEN
				IF MessageBox('Date Validation', 'The Issue Date is set in the future. Are you sure you want to save the date?', Question!, YesNo!,2)  = 2 THEN
					SQLCA.nf_rollback_transaction()
					RETURN -1
				END IF
			END IF
			
			//BR 4.2
			IF idw_dw[2].GetItemString(ll_tranrow,'address_line1') = '' Then
				SQLCA.nf_rollback_transaction()
				MessageBox('Payment Module - Validation Error','Street address is a required field.')
				idw_dw[2].SetFocus()
				idw_dw[2].SetColumn("address_line1")
				Return -1
			END IF
			
			idw_dw[2].SetItem(ll_tranrow,"txn_no",ll_txn_no)
			idw_dw[2].SetItem(ll_tranrow,"txn_type_code","1")
			idw_dw[2].SetItem(ll_tranrow,"claim_no",il_claim_no)
			idw_dw[2].SetItem(ll_tranrow,"payment_no",ll_payment_no)
			IF ls_opening_type_code = 'PEN' OR ls_opening_type_code = 'SV' OR ls_opening_type_code = 'S1' OR ls_opening_type_code = 'S2' THEN
				idw_dw[2].SetItem(ll_tranrow,"admin_region_code",'PRV') /* admin region for pensions is always provincial */
			ELSE
				idw_dw[2].SetItem(ll_tranrow,"admin_region_code",idw_basic_claim.GetItemString(1,'admin_region_code'))
			END IF
			idw_dw[2].SetItem(ll_tranrow,"scheduled_processing_date",ldt_scheduled_processing_date)
			
			ls_recipient_type_code = idw_dw[2].GetItemString(ll_tranrow,'recipient_type_code')
			IF ls_recipient_type_code <> 'I' AND ls_recipient_type_code <> 'O' &
					AND (ls_payment_type_code = 'R1' OR ls_payment_type_code = 'R2' OR ls_payment_type_code = 'R3') THEN
				SQLCA.nf_rollback_transaction()
				MessageBox('Payment Module - Validation Error','Recipient Type must be either Individual or Other for an LTD RTW Incentive payment.')
				idw_dw[2].SetFocus()
				idw_dw[2].SetColumn("recipient_type_code")
				RETURN -1
			END IF
			
			ll_recipient_cntr++
		LOOP
		ll_payment_cntr++
	LOOP
ELSE
	// *** updating unprocessed payemnt
	
	ll_payment_no = idw_dw[1].GetItemNumber(1,"payment_no")
	ll_tranrow = 1 // check to see if a new tranrow was inserted
	DO WHILE ll_tranrow <= idw_dw[2].RowCount()
/*			We update these fields every time to get the most current information*/
		ls_string = idw_dw[2].GetItemString(ll_tranrow,"recipient_type_code")
		
		IF ls_string <> 'I' AND ls_string <> 'O' AND ls_string <> 'V' THEN
			SQLCA.nf_rollback_transaction()
			MessageBox("Payment Module - Validation Error","Recipient Type - "+ls_string+" is invalid.",Exclamation!)
			idw_dw[2].SetFocus()
			idw_dw[2].SetColumn("recipient_type_code")
			Return -1
		END IF
		
		
		/* PR 4649:  changed code that checked for a recipient of type 'I' for opening payment type code of 
			Pensions, SV, S1 or S2 and prohibited payments for anything other than 'I'ndividual in these cases. 
			Business rules now allow that recipient type of Other Payee also should be allowed for Pension 
			payments, so that users can issue a recovery to WHSCC on Overpayment. This PR is also found earlier in this function and in n_award
		*/			
		IF (ls_opening_type_code = 'PEN' OR ls_opening_type_code = 'SV' OR ls_opening_type_code = 'S1' or ls_opening_type_code = 'S2' ) &
						AND idw_dw[2].GetItemString(ll_tranrow,"recipient_type_code") <> 'I' &
						AND idw_dw[2].GetItemString(ll_tranrow,"recipient_type_code") <> 'O'THEN
			SQLCA.nf_rollback_transaction()
			MessageBox("Payment Module - Validation Error","'Claimant/Individual' or 'Other Payee' are the only valid recipient choices for pension payments.",Exclamation!)
			idw_dw[2].SetFocus()
			idw_dw[2].SetColumn("recipient_type_code")
			idw_dw[2].ScrollToRow(ll_tranrow)
			Return -1
		END IF
		
		
		ls_payment_method_code = idw_dw[2].GetItemString(ll_tranrow,"payment_method_code")
		//RECEIVING SALARY BUSINESS RULES
		IF ls_opening_type_code = 'RLOE' and is_claim_receiving_salary_flag = 'Y' THEN
			CHOOSE CASE ls_payment_type_code
				CASE 'R1'
					CHOOSE CASE ls_payment_method_code
						CASE 'A','D','H'
							// OK
						CASE ELSE
							SQLCA.nf_rollback_transaction()
							MessageBox("Payment Module - Validation Error","Payment method must be 'Automated Cheque', 'Direct Deposit'" +&
																							"~nor 'Hand Written Cheque' when the claim is 'Receiving Salary'" +&
																							"~nand the paymen type is 'Job Search Return to Work Incentive'.")
							idw_dw[2].SetFocus()
							idw_dw[2].SetColumn("payment_method_code")
							Return -1
					END CHOOSE
					
				CASE ELSE
					// not an R1 (job search RTW Incentive) payment
					//BR 4.12
					IF ls_payment_method_code  <> 'R' Then
						SQLCA.nf_rollback_transaction()
						MessageBox("Payment Module - Validation Error","Payment method must be 'Recorded Not Issued - Receiving Salary' when the claim is 'Receiving Salary'.")
						idw_dw[2].SetFocus()
						idw_dw[2].SetColumn("recipient_type_code")
						Return -1
					END IF
			END CHOOSE
			
			//BR 2.61
			IF idw_dw[2].GetItemDecimal(1,'txn_amount') = 0 Then
				MessageBox("Payment Module - Validation Error","Receiving salary payments cannot be $0.00")
				idw_dw[2].SetFocus()
				idw_dw[2].SetColumn("txn_amount")
				RETURN -1
			END IF
			
			//BR 4.1
			IF ls_payment_method_code  = 'R' and ls_string <> 'I' Then
				MessageBox("Payment Module - Validation Error","Recipient Type must be 'Individual' for claims that are 'Receiving Salary'.",Exclamation!)
				idw_dw[2].SetFocus()
				idw_dw[2].SetColumn("recipient_type_code")
				Return -1
			END IF	
		Else
			//BR 4.13
			IF ls_payment_method_code  = 'R' Then
				MessageBox("Payment Module - Validation Error","Payment method cannot be 'Recorded Not Issued - Receiving Salary' unless the claim is 'Receiving Salary' and the Opening is 'RLOE'.")
				idw_dw[2].SetFocus()
				idw_dw[2].SetColumn("recipient_type_code")
				Return -1
			END IF
		End if	
		
	

		ll_recipient_no = idw_dw[2].GetItemNumber(ll_tranrow,"recipient_no")
		
		If idw_dw[2].GetItemDecimal(ll_tranrow,'txn_amount') = 0 Then
			If idw_dw[2].GetItemString(ll_tranrow,'payment_method_code') <> 'I' Then
				idw_dw[2].SetItem(ll_tranrow,'payment_method_code','I')
				idw_dw[2].SetItem(ll_tranrow,'use_default_address_flag','Y')					
				
				ls_recipient_type_code = idw_dw[2].GetItemString(ll_tranrow,'recipient_type_code')
				ls_cheque_print_group_code = idw_dw[2].GetItemString(ll_tranrow,'cheque_print_group_code')
				nf_setup_address(ls_recipient_type_code,ll_recipient_no,ll_tranrow,ls_cheque_print_group_code)
			End if
		ElseIf idw_dw[2].GetItemDecimal(ll_tranrow,'txn_amount') <> 0 and idw_dw[2].GetItemString(ll_tranrow,"payment_method_code") = 'I' Then
			MessageBox('Invalid payment method','The payment method cannot be "Inapplicable" unless the transaction amount is $0.00')
			return -1		
		End if
		
	
		
		//SR70 - Special Survivor Payment JAN 2001
		IF (ls_opening_type_code <> 'PEN' AND ls_opening_type_code <> 'SV' and  ls_opening_type_code <> 'S1' and ls_opening_type_code <> 'S2'   ) AND idw_dw[2].GetItemString(ll_tranrow,"recipient_type_code") = 'I' 	AND ll_recipient_no <> idw_basic_claim.GetItemNumber(1,'individual_no') AND ls_payment_type_code <> 'S1'THEN
			SQLCA.nf_rollback_transaction()
			MessageBox("Payment Module - Validation Error","Recipient No - must be that of the claimant.",Exclamation!)
			idw_dw[2].SetFocus()
			idw_dw[2].SetColumn("recipient_no")
			Return -1
		END IF
		
		IF nf_validate_recipient(ls_string,ll_recipient_no,ll_tranrow) < 0 THEN
			SQLCA.nf_rollback_transaction()
			idw_dw[2].SetFocus()
			idw_dw[2].SetColumn("recipient_no")
			Return -1
		END IF
		
		// SR70 - Special Survivor Payment JAN 2001
		IF ls_payment_type_code = 'S1' AND (idw_dw[2].GetItemString(ll_tranrow,"payment_method_code") <> 'A' AND &
			idw_dw[2].GetItemString(ll_tranrow,"payment_method_code") <> 'H') THEN
			SQLCA.nf_rollback_transaction()
			MessageBox("Payment Module - Validation Error","Method - Must be Automated or Handwritten Cheque.",Exclamation!)
			idw_dw[2].SetFocus()
			idw_dw[2].SetColumn("payment_method_code")
			Return -1
		END IF
		
		ll_txn_no = idw_dw[2].GetItemNumber(ll_tranrow,'txn_no')
		IF IsNull(ll_txn_no) OR ll_txn_no = 0 THEN
			ll_txn_no = nf_get_next_txn_identifier()
			IF ll_txn_no < 1 THEN
				Return -1
			END IF
			idw_dw[2].SetItem(ll_tranrow,'txn_no',ll_txn_no)
			idw_dw[2].SetItem(ll_tranrow,'payment_no',ll_payment_no)
			idw_dw[2].SetItem(ll_tranrow,'claim_no',il_claim_no)
		END IF
		
		IF idw_dw[2].GetItemString(ll_tranrow,"use_default_address_flag") = 'N' THEN
			IF nf_validate_address() < 0 THEN
				SQLCA.nf_rollback_transaction()
				Return -1
			END IF
		END IF
		
		// BR 4.16
		ld_man_cheque_date = Date(idw_dw[2].GetItemDateTime(ll_tranrow,'cheque_deposit_date'))
		IF DaysAfter( ld_server_date , ld_man_cheque_date ) > 7 THEN
			SQLCA.nf_rollback_transaction()
			MessageBox('Payment Module - Validation Error','Issue date must be less than or equal to one week in future.',Exclamation!)
			Return -1
		ELSEIF DaysAfter( ld_server_date , ld_man_cheque_date ) < 0 THEN
			SQLCA.nf_rollback_transaction()
			MessageBox('Payment Module - Validation Error','Issue date cannot be in the past.',Exclamation!)
			Return -1
		ELSEIF DaysAfter( ld_server_date , ld_man_cheque_date ) > 0 THEN
			IF MessageBox('Date Validation', 'The Issue Date is set in the future. Are you sure you want to save the date?', Question!, YesNo!,2)  = 2 THEN
				SQLCA.nf_rollback_transaction()
				RETURN -1
			END IF
		END IF
	
		//BR 4.2
		IF idw_dw[2].GetItemString(ll_tranrow,'address_line1') = '' Then
			SQLCA.nf_rollback_transaction()
			MessageBox('Payment Module - Validation Error','Street address is a required field.')
			idw_dw[2].SetFocus()
			idw_dw[2].SetColumn("address_line1")
			Return -1
		END IF
		
		IF ls_opening_type_code = 'PEN' OR ls_opening_type_code = 'SV' or  ls_opening_type_code = 'S1' OR ls_opening_type_code = 'S2' THEN
			idw_dw[2].SetItem(ll_tranrow,"admin_region_code",'PRV') /*admin region for pensions always provincial */
		ELSE
			idw_dw[2].SetItem(ll_tranrow,"admin_region_code",idw_basic_claim.GetItemString(1,'admin_region_code'))
		END IF
		idw_dw[2].SetItem(ll_tranrow,"scheduled_processing_date",ldt_scheduled_processing_date)
		ll_tranrow++
	LOOP
		
		
	IF ls_group_flag = 'N' THEN	
		IF ldec_total_payment_amount <= ic_authorization_limit THEN
			idw_dw[1].SetItem(1,"authorized_by_code",vgst_user_profile.user_id)
			idw_dw[1].SetItem(1,"authorized_date",ldt_server_date)
		ELSE
			idw_dw[1].SetItem(1,"authorized_by_code",'')
			idw_dw[1].SetItem(1,"authorized_date",ld_null_date)
		END IF
	ELSEIF ls_group_flag = 'Y' AND ll_group_no = 0 THEN
		idw_dw[1].SetItem(1,"authorized_by_code",'')
		idw_dw[1].SetItem(1,"authorized_date",ld_null_date)
	ELSEIF ls_group_flag = 'Y' AND ll_group_no > 0 THEN
		IF ldec_total_group_amount <= ic_authorization_limit THEN
			ll_rtn = nf_authorize_group( idw_dw[4], 'A', FALSE)
		ELSE
			ll_rtn = nf_authorize_group( idw_dw[4], 'U', FALSE)
		END IF
	END IF
END IF


// T021106 - if the payment becomes unauthorized for any reason during this save cycle, raise a message to the user	
//                  The ELSE IF part is checking for when this is a new row, and the flag is on, in which case there was no initial retrieve, so we test for new record

IF idw_dw[1].getItemString(1,"authorized_by_code", primary!, false) = ''   THEN   // not authorized now
	IF idw_dw[1].getItemString(1,"authorized_by_code", primary!, true)  > "" THEN  // but it was an authorized payment on initial retrieve
		MESSAGEBOX("Payment Unauthorized", "This payment has now become unauthorized because it was flagged for 'Group for Authorization'.", Information!)
 
			  // check for a new row and if the flag is on
	ELSEIF	(idw_dw[1].getItemStatus(idw_dw[1].GetRow(),0,primary!) = New! OR  &
		        idw_dw[1].getItemStatus(idw_dw[1].GetRow(),0,primary!) = NewModified!)  AND ls_group_flag = 'Y' THEN 
			    MESSAGEBOX("Payment Unauthorized", "This payment must be authorized as part of a group.", Information!)
	END IF				
END IF

// Job Search RTW Incentive
li_rtn = nf_setup_rtw_incentive_payment_xref(ll_payment_no,ls_payment_type_code)
IF li_rtn <> 0 THEN RETURN -1


Return 1
end function

public function integer nf_change_item (long al_datawindow);DATAWINDOWCHILD	ldwc_child, ldwc_child_b,  ldwc_child_opening
LONG					ll_current_row,ll_newrow, ll_oldrow, ll_rownum, ll_opening_row
DECIMAL				ldec_total_award_amount, ldec_total_deductions, ldec_txn_amount, ldec_hour_amount, ldec_day_amount
DECIMAL				ldec_daily_rate, ldec_hourly_rate, ldec_paid_hours_lost, ldec_paid_days_lost, ldec_award_amount	
DECIMAL				ldec_benefit_level_percentage
INTEGER				li_error_level, li_benefit_calculation_no, li_year
INTEGER				li_month, li_day, li_no, li_opening_no, li_return, li_nmbr_cycles, li_rows
STRING				ls_string
STRING				ls_payment_type_code, ls_error_message,	ls_old_days_hours_flag, ls_old_payment_code
STRING				ls_days_hours_flag, ls_old_repeat_payment_allowed_flag, ls_repeat_payment_allowed_flag
STRING				ls_opening_type_code, ls_date, ls_award_freq_code, ls_dates_required_flag
String            ls_payment_sub_type_code, ls_payment_type_desc, ls_payment_sub_type_desc
DATETIME				lt_paid_to_date
DATE 					ld_date,	ld_order_date, ldt_accident_recurrence_date, ld_effective_from_date, ld_server_dt


ld_server_dt = Date(f_server_datetime())
CHOOSE CASE al_datawindow
	CASE 1
		ll_current_row = idw_dw[1].GetRow()
		CHOOSE CASE idw_dw[1].GetColumnName()
			CASE "opening_no"   
				IF MessageBox("Payment Module","Changing the opening will reset payment details" +     &
				  "~r~nDo you wish to continue",Question!,YesNo!) = 1 THEN
					idw_dw[1].SetItem(1,'payment_type_and_sub_type','')
					li_no = Integer(idw_dw[1].GetText())
					idw_dw[1].GetChild('benefit_calculation_no', ldwc_child)
					ldwc_child.SetFilter("opening_no = " + string(li_no) + " or benefit_calculation_no = 0")
					ldwc_child.Filter()

					li_benefit_calculation_no = ldwc_child.GetItemNumber(1,"max_benefit_calculation_no")
					idw_dw[1].SetItem(1,"benefit_calculation_no",li_benefit_calculation_no)
					// get the benefit category code and set a filter on the payment type
					nf_set_payment_filter(li_no, li_benefit_calculation_no)
					nf_setup_benefit_info(li_benefit_calculation_no,ldec_daily_rate,ldec_hourly_rate,ldec_benefit_level_percentage)
					
					// pr 2787 - new argument (below) indicates that the function call is a result of a change, not an insert
					nf_setup_default(ldec_daily_rate,li_benefit_calculation_no,ldec_benefit_level_percentage, li_no,FALSE)
					
					ls_payment_type_code = idw_dw[1].GetItemString(1,'payment_type_and_sub_type')
					
					nf_rtw_defaulting(ls_payment_type_code)
					idw_dw[1].SetItem(1,"tier_no",0)
					
					// Warn user if there are no tiers for claim opening
					IF ls_payment_type_code = 'R1' THEN
						li_rows = nf_retrieve_rtw_tier()
						IF li_rows = 0 THEN
							MessageBox('No Tiers Available','For this claim opening, the eligible tiers have' + &
																'~nbeen fully paid or there are no eligible tiers.',StopSign!)
							Return -1
						END IF						
					END IF
					
				ELSE
					li_no = idw_dw[1].GetItemNumber(1,"opening_no")
					idw_dw[1].settext(string(li_no))
					Return -1
				END IF

			CASE "benefit_calculation_no"
				IF MessageBox("Payment Module","Changing the benefit calculation will reset payment details" +     &
					  "~r~nDo you wish to continue",Question!,YesNo!) = 1 THEN
					li_no = Integer(idw_dw[1].GetText())
					idw_dw[1].SetItem(1,'payment_type_and_sub_type','')
					nf_set_payment_filter(idw_dw[1].GetItemNumber(1,'opening_no'),li_no)
					nf_setup_benefit_info(li_no,ldec_daily_rate,ldec_hourly_rate,ldec_benefit_level_percentage)
					// pr 2787 - new argument (below) indicates that the function call is a result of a change, not an insert
					nf_setup_default( ldec_daily_rate, li_no, ldec_benefit_level_percentage,idw_dw[1].GetItemNumber(1,'opening_no'),FALSE)
					
					ls_payment_type_code = idw_dw[1].GetItemString(1,'payment_type_and_sub_type')
					
					IF ls_payment_type_code = '' OR IsNull(ls_payment_type_code) THEN
					ELSE
						nf_rtw_defaulting(ls_payment_type_code)
						idw_dw[1].SetItem(1,"tier_no",0)
					END IF
					
					// Warn user if there are no tiers for claim opening
					IF ls_payment_type_code = 'R1' THEN
						li_rows = nf_retrieve_rtw_tier()
						IF li_rows = 0 THEN
							MessageBox('No Tiers Available','For this claim opening, the eligible tiers have' + &
																'~nbeen fully paid or there are no eligible tiers.',StopSign!)
							Return -1
						END IF						
					END IF
						
				ELSE
					li_no = idw_dw[1].GetItemNumber(1,"benefit_calculation_no")
					idw_dw[1].SetItem(1,'benefit_calculation_no',idw_dw[1].GetItemNumber(1,"benefit_calculation_no"))
					Return -1
				END IF

			CASE "payment_type_code", "payment_type_and_sub_type"
				idw_dw[1].GetChild('payment_type_and_sub_type', ldwc_child)
				ll_newrow = ldwc_child.GetRow()
				ls_payment_type_code = ldwc_child.GetItemString(ll_newrow, "payment_type_code")
				
				IF Trim(ls_payment_type_code) <> '' THEN
					ls_old_payment_code = idw_dw[1].GetItemString(1,"payment_type_code")
					// Check to see if the selected payment type code is allowed and if the benefit level of this code is 
					// consistent with the current benefit calculation
					li_no = idw_dw[1].GetItemNumber(1,"benefit_calculation_no")
					ll_rownum = idw_dw[1].GetChild('benefit_calculation_no', ldwc_child_b)
					ll_rownum  = ldwc_child_b.Find("benefit_calculation_no = " + string(li_no),1, ldwc_child_b.RowCount())
					ls_award_freq_code = ldwc_child_b.GetItemString(ll_rownum,"award_freq_code")

					nf_validate_payment_type(ls_payment_type_code, li_error_level, ls_error_message, ls_award_freq_code, ls_days_hours_flag, ls_dates_required_flag)
					IF li_error_level = 2 THEN
						MessageBox("Payment Module - Validation Error", ls_error_message,Exclamation!)
						Return -1
					END IF
					idw_dw[1].SetItem(1,"payment_type_code",ls_payment_type_code)
					idw_dw[1].SetItem(1,"explanation","") //Remove the comment when the payment type has changed.
					// Setup detail window for Special Survivor Payment 
					// SR70 - JAN 2001 - SMANZER
					IF ls_payment_type_code = 'S1' THEN
						li_return = nf_setup_survivor()
						nf_tab_order("recipient_type_code","I")
						IF li_return = -1 THEN
							RETURN -1 
						END IF	
					END IF	
					
					nf_rtw_defaulting(ls_payment_type_code)
					idw_dw[1].SetItem(1,"tier_no",0)
					
					idw_dw[1].GetChild('payment_type_and_sub_type', ldwc_child)
					ll_newrow = ldwc_child.GetRow()
					ldec_benefit_level_percentage = ldwc_child.GetItemDecimal(ll_newrow,"benefit_level_percentage")
					ls_payment_sub_type_code = ldwc_child.GetItemString(ll_newrow, "payment_sub_type_code")
					ls_payment_type_desc = ldwc_child.GetItemString(ll_newrow, "payment_type_desc")
					ls_payment_sub_type_desc = ldwc_child.GetItemString(ll_newrow, "payment_sub_type_desc")
					IF ls_payment_sub_type_desc = "" OR IsNull(ls_payment_sub_type_desc) THEN
						idw_dw[1].SetItem(1, "payment_type_description", ls_payment_type_desc)
					ELSE
						idw_dw[1].SetItem(1, "payment_type_description", ls_payment_type_desc + " - " + ls_payment_sub_type_desc)
					END IF
					idw_dw[1].SetItem(1, "payment_sub_type_code", ls_payment_sub_type_code)

					IF ldec_benefit_level_percentage > 0 THEN
						IF idw_dw[1].GetItemDecimal(1,"benefit_calculation_no") = 0 THEN
							MessageBox("Payment Module - Validation Error", "You must select a benefit calculation before you can make a " + &
							  "~r~nLoss of Earnings payment or a 3 Day Reimbursement", Exclamation!)
							Return -1
						END IF
						// Do not check the benefit level of the current benefit calculation against the 3 Day payment
						// type benefit level when the Accident/Recurrence of Injury is >= January 01, 1998. The reason 
						// being that the Payment_Type table only has one entry for 3 day payment and the benefit
						// level assocaited with it is 80%. The function nf_set_payment_filter forces the display of the
						// 3 day payment for 85% benefits for RLOE openings with accident/recurrences >= 1998/01/01
						li_opening_no	=	idw_dw[1].GetItemNumber(1,"opening_no")
						idw_dw[1].GetChild('opening_no', ldwc_child_opening)
						ll_opening_row = ldwc_child_opening.Find("opening_no = " + string(li_opening_no),1,ldwc_child_opening.RowCount())
						ldt_accident_recurrence_date = Date(ldwc_child_opening.GetItemDateTime(ll_opening_row,"accident_recurrence_date"))
						IF ldt_accident_recurrence_date >= Date ("1998/01/01") AND ls_payment_type_code = "3D" THEN
						ELSE
							IF ldec_benefit_level_percentage <> idw_dw[1].GetItemDecimal(1,"benefit_level_percentage") THEN
								MessageBox("Payment Module - Validation Error", "The benefit level of the current benefit calculation does not match the payment type" + &
							 "~r~nYou must select another benefit calculation or change the payment type", Exclamation!)
								Return -1
							END IF
						END IF 								
					END IF
					IF li_error_level = 1 THEN
						MessageBox("Payment Module",ls_error_message)
					END IF

					// Determine which fields need to be initialized :it allows the user to keep a 
					// lot of old values when the payment type is changed 
					IF not IsNull(ls_old_payment_code) THEN
						ll_oldrow = ldwc_child.Find("payment_type_code = '" + ls_old_payment_code + "'",1, ldwc_child.RowCount())
						// Check amounts to see if initialization is required
						IF ll_oldrow > 0 THEN
							ls_old_days_hours_flag = ldwc_child.GetItemString(ll_oldrow,"days_hours_flag")
							ls_days_hours_flag = ldwc_child.GetItemString(ll_newrow,"days_hours_flag")
						END IF
						IF ls_old_days_hours_flag = "Y" and ls_days_hours_flag = "N" THEN
							idw_dw[1].SetItem(1,"paid_days_lost",0)
							idw_dw[1].SetItem(1,"day_amount",0)
							idw_dw[1].SetItem(1,"paid_hours_lost",0)
							idw_dw[1].SetItem(1,"hour_amount",0)
							idw_dw[1].SetItem(1,"total_award_amount",0)
							idw_dw[1].SetItem(1,"total_deductions",0)
							idw_dw[1].SetItem(1,"total_payment_amount",0)
							IF idw_dw[2].RowCount() = 1 THEN
								idw_dw[2].SetItem(1,"txn_amount",0)
							END IF
						END IF
						IF ls_old_days_hours_flag = "N" and ls_days_hours_flag = "Y" THEN
							idw_dw[1].SetItem(1,"total_award_amount",0)
							idw_dw[1].SetItem(1,"total_deductions",0)
							idw_dw[1].SetItem(1,"total_payment_amount",0)
							IF idw_dw[2].RowCount() = 1 THEN
								idw_dw[2].SetItem(1,"txn_amount",0)
							END IF
						END IF		
						// Check number of cycles to see if initialization is requried
						IF ll_oldrow > 0 THEN
							ls_old_repeat_payment_allowed_flag = ldwc_child.GetItemString(ll_oldrow,"repeat_payment_allowed_flag")
						END IF
						ls_repeat_payment_allowed_flag = ldwc_child.GetItemString(ll_newrow,"repeat_payment_allowed_flag")
						IF ls_old_repeat_payment_allowed_flag = "Y" and ls_repeat_payment_allowed_flag = "N" THEN
							idw_dw[1].SetItem(1,"nmbr_cycles",1)
						END IF
					END IF
					// If the payment type is a RLOE code that represents a final payment, set the final payment flag on. 
					// If the benefit category of the new payment type is not VR, set the final payment flag to no.
					IF ls_payment_type_code = " 2" or ls_payment_type_code = "-2" or ls_payment_type_code = ".2" THEN
						idw_dw[1].SetItem(1,"final_payment_flag","Y")
					ELSE
						ls_opening_type_code = ldwc_child.GetItemString(ll_newrow,"benefit_category_code")
						idw_dw[1].SetItem(1,"final_payment_flag","N")
					END IF
					// If the payment type is a 3 day reimbursement, automatically calculate the reimbursement
					// (reimbursement is 3/5 or .6 of the weekly award amount) 
					IF ls_payment_type_code = "3D" THEN
						ldec_award_amount = ldwc_child_b.GetItemDecimal(ll_rownum,"award_amount")
						ldec_total_award_amount = Round((ldec_award_amount * .6),2)
						idw_dw[1].SetItem(1,"total_award_amount",ldec_total_award_amount)
						idw_dw[1].SetItem(1,"total_payment_amount",ldec_total_award_amount)
						IF idw_dw[2].RowCount() = 1 THEN
							idw_dw[2].SetItem(1,"txn_amount",ldec_total_award_amount)
						END IF
					END IF
					nf_tab_order("payment_type_code",ls_payment_type_code)
					idw_dw[1].SetItem(1,'payment_type_code',ls_payment_type_code)	// this was added because you could not see the value
																								// in the drop down until you tabbed away
					
					// Warn user if there are no tiers for claim opening
					IF ls_payment_type_code = 'R1' THEN
						idw_dw[1].SetItem(1,"paid_days_lost",0)
						idw_dw[1].SetItem(1,"paid_hours_lost",0)
						li_rows = nf_retrieve_rtw_tier()
						IF li_rows = 0 THEN
							MessageBox('No Tiers Available','For this claim opening, the eligible tiers have' + &
																'~nbeen fully paid or there are no eligible tiers.',StopSign!)
							Return -1
						END IF
					ELSEIF ls_payment_type_code = 'R2' OR ls_payment_type_code = 'R3' THEN
						idw_dw[1].SetItem(1,"paid_days_lost",0)
						idw_dw[1].SetItem(1,"paid_hours_lost",0)						
					END IF
				END IF

			CASE "nmbr_cycles"
				// entered value for number of cycles
				li_nmbr_cycles = Integer(idw_dw[1].GetText())
				
				idw_dw[1].GetChild('benefit_calcualtion_no', ldwc_child)
				li_benefit_calculation_no = idw_dw[1].GetItemNumber(1,"benefit_calculation_no")
				ll_rownum = ldwc_child.Find("benefit_calculation_no = " + string(li_benefit_calculation_no),1, ldwc_child.RowCount())
				ls_award_freq_code  = ldwc_child.GetItemString(ll_rownum,"award_freq_code")
				IF ls_award_freq_code = "W" and li_nmbr_cycles > 3 THEN
					MessageBox("Payment Module - Validation Error","Number of cycles must be 1 to 3 for weekly payments")
				ELSEIF ls_award_freq_code = "M" and li_nmbr_cycles > 16 THEN
					MessageBox("Payment Module - Validation Error","Number of cycles must be 1 to 16 for monthly payments")
				END IF
				
				ls_payment_type_code = idw_dw[1].GetItemString(1,'payment_type_code')
				CHOOSE CASE ls_payment_type_code
					CASE 'R1','R2','R3'
						IF li_nmbr_cycles <> 1 THEN
							MessageBox("Payment Module - Validation Error","Number of cycles must be 1 for RTW Incentive payments.")
							RETURN -1
						END IF
				END CHOOSE

			CASE "paid_from_date"
				ls_date = idw_dw[1].GetText()    
				ls_date = Left(ls_date,10)       
				ld_date = Date(ls_date)

				// The paid from date cannot be less than the date of occurrence for this payment
				li_no	=	idw_dw[1].GetItemNumber(1,"opening_no")
				idw_dw[1].GetChild('opening_no', ldwc_child)
				ll_rownum 	= ldwc_child.Find("opening_no = " + string(li_no),1,ldwc_child.RowCount())
				ldt_accident_recurrence_date = Date(ldwc_child.GetItemDateTime(ll_rownum,"accident_recurrence_date"))
				ls_string = ldwc_child.GetItemString(ll_rownum,'opening_type_code')
				IF ld_date < ldt_accident_recurrence_date THEN
					MessageBox("Payment Module - Validation Error","Paid From Date cannot be before the recurrence date",Exclamation!)
					Return -1
				END IF
				// The paid from date cannot be less than the effective date of the current benefit calculation 
				ls_payment_type_code =	idw_dw[1].GetItemString(1,"payment_type_code")
				idw_dw[1].GetChild('payment_type_and_sub_type', ldwc_child)
				ll_rownum =	ldwc_child.Find("payment_type_code = '" + ls_payment_type_code + "'",1,ldwc_child.RowCount())
				ldec_benefit_level_percentage = ldwc_child.GetItemDecimal(ll_rownum,"benefit_level_percentage")
				IF ldec_benefit_level_percentage > 0 THEN
					li_benefit_calculation_no = idw_dw[1].GetItemNumber(1,"benefit_calculation_no")
					idw_dw[1].GetChild('benefit_calculation_no', ldwc_child)
					ll_rownum = ldwc_child.Find("benefit_calculation_no = " + string(li_benefit_calculation_no),1, ldwc_child.RowCount())
					ld_effective_from_date = Date(ldwc_child.GetItemDateTime(ll_rownum,"effective_from_date"))

					IF ld_date < ld_effective_from_date THEN
						MessageBox("Payment Module - Validation Error","Paid From Date cannot be before the effective date of the benefit calculation",Exclamation!)
						Return -1
					END IF
					ls_award_freq_code  = ldwc_child.GetItemString(ll_rownum,"award_freq_code")
				ELSE
					ls_award_freq_code = "W"  // Since there are codes which require from/to dates, but not benefit calculations, it's hard to say what the paid to date should be
				END IF
				IF ls_award_freq_code = "W" THEN
					// Compute the paid to date (2 weeks from paid from date)
					lt_paid_to_date = DateTime(RelativeDate(ld_date,14))
					idw_dw[1].SetItem(1,"paid_to_date",lt_paid_to_date)
				ELSE
					// Compute the paid to date (the first of the next month)
					li_year = Year(ld_date)
					li_month = Month(ld_date)
					li_day = Day(ld_date)
					IF li_month <> 12 THEN
						li_month ++
					ELSE
						li_month = 1
						li_year++
					END IF
					lt_paid_to_date = DateTime(Date(li_year,li_month,1))
					idw_dw[1].SetItem(1,"paid_to_date",lt_paid_to_date)
					// Try to figure out the scheduled processing date based on 
					// when the period occurs in relation to the current date
// PR1639 - Nov 16, 2000						
					IF ld_date <= ld_server_dt THEN
							IF Date(lt_paid_to_date) > ld_server_dt and Day(ld_server_dt) < 23 THEN
								ld_order_date = Date(Year(ld_server_dt),Month(ld_server_dt),23)						
							ELSE			
								ld_order_date = ld_server_dt
							END IF
					ELSE				
							ld_order_date = Date(Year(ld_date),Month(ld_date),23)
					END IF

					IF Month(ld_order_date) = 12 THEN
						ld_order_date = Date(Year(ld_order_date),12,10)
					END IF
					idw_dw[1].SetItem(1,"scheduled_processing_date",DateTime(ld_order_date))
				END IF		

			CASE 'paid_to_date'
				// cannot be beyond the associated opening's benefit end dat
				ls_payment_type_code = idw_dw[1].GetItemString(1,'payment_type_code')
				li_no = idw_dw[1].GetItemNumber(1,'opening_no')
				idw_dw[1].GetChild('opening_no', ldwc_child)
				li_no = ldwc_child.Find("opening_no = " + string(li_no),1,ldwc_child.RowCount() )
				IF li_no > 0 AND ls_payment_type_code <> 'R1' THEN
					IF NOT IsNull(ldwc_child.GetItemDateTime(li_no,'benefit_end_date')) THEN
						IF DateTime(Date(Left(idw_dw[1].GetText(),10))) > ldwc_child.GetItemDateTime(li_no,'benefit_end_date') THEN
							MessageBox('Invalid To Date','The payment goes beyond benefit (opening) duration.')
							Return -1
						END IF
					END IF
				END IF
			CASE "scheduled_processing_date"
				ls_date = idw_dw[1].GetText()    // Note: we can't seem to use x=DateTime(idw_dw[1].GetText()) and x=Date(idw_dw[1].GetText()) does
				ls_date = Left(ls_date,10)       //       not return a proper value
				ld_date = Date(ls_date)
				// The scheduled processing date cannot be less than today 
				IF ld_date < ld_server_dt THEN
					MessageBox("Payment Module - Validation Error","Scheduled processing date cannot be less than Today",Exclamation!)
					Return -1
				END IF

			CASE "payment_adjustment_flag"
				ls_payment_type_code  = idw_dw[1].GetItemString(1,"payment_type_code")
				idw_dw[1].GetChild('payment_type_and_sub_type', ldwc_child)
				ll_rownum = ldwc_child.Find("payment_type_code = '" + ls_payment_type_code + "'",1, ldwc_child.RowCount())
				IF ll_rownum > 0 THEN
					ls_opening_type_code = ldwc_child.GetItemString(ll_rownum,"benefit_category_code")
					ls_days_hours_flag = ldwc_child.GetItemString(ll_rownum,"days_hours_flag")
				END IF

				// Adjustments do not required paid days and hours lost, the user can just enter the payment amount 
				
				IF idw_dw[1].GetText() = "Y" THEN
					idw_dw[1].SetItem(1,"paid_days_lost",0)
					idw_dw[1].SetItem(1,"day_amount",0)
					idw_dw[1].SetItem(1,"paid_hours_lost",0)
					idw_dw[1].SetItem(1,"hour_amount",0)
					idw_dw[1].SetItem(1,"total_award_amount",0)
					idw_dw[1].SetItem(1,"total_deductions",0)
					idw_dw[1].SetItem(1,"total_payment_amount",0)
					IF idw_dw[2].RowCount() = 1 THEN
						idw_dw[2].SetItem(1,"txn_amount",0)
					END IF
					idw_dw[1].SetTabOrder('paid_days_lost',0)
					idw_dw[1].SetTabOrder('paid_hours_lost',0)
					idw_dw[1].SetTabOrder('total_award_amount',120)
				ELSE
					IF ls_days_hours_flag = 'Y' THEN
						idw_dw[1].SetTabOrder('paid_days_lost',100)
						idw_dw[1].SetTabOrder('paid_hours_lost',110)
						idw_dw[1].SetTabOrder('total_award_amount',0)
					ELSEIF ls_days_hours_flag = 'N' THEN
						idw_dw[1].SetTabOrder('paid_days_lost',0)
						idw_dw[1].SetTabOrder('paid_hours_lost',0)
						idw_dw[1].SetTabOrder('total_award_amount',120)
					END IF
				END IF

			CASE "paid_days_lost"
				ldec_paid_days_lost  = Dec(idw_dw[1].GetText())

				// Recalculate the payment amount
				ldec_hour_amount = idw_dw[1].GetItemDecimal(1,"hour_amount")
				ldec_daily_rate = idw_dw[1].GetItemDecimal(1,"daily_rate")
				ldec_total_deductions = idw_dw[1].GetItemDecimal(1,"total_deductions")
				ldec_day_amount = ldec_paid_days_lost * ldec_daily_rate
				ldec_total_award_amount = Round(ldec_day_amount + ldec_hour_amount,2)
				ldec_txn_amount  = ldec_total_award_amount + ldec_total_deductions

				IF ldec_txn_amount < 0 THEN 
					ldec_txn_amount = 0
				END IF
				idw_dw[1].SetItem(1,"day_amount",ldec_day_amount)
				idw_dw[1].SetItem(1,"total_award_amount",ldec_total_award_amount)
				idw_dw[1].SetItem(1,"total_payment_amount",ldec_txn_amount)
				IF idw_dw[2].RowCount() = 1 THEN
					idw_dw[2].SetItem(1,"txn_amount",ldec_txn_amount)
				END IF

			CASE "paid_hours_lost"
				ldec_paid_hours_lost = Dec(idw_dw[1].GetText())

				// Recalculate the payment amount
				ldec_day_amount = idw_dw[1].GetItemDecimal(1,"day_amount")
				ldec_hourly_rate = idw_dw[1].GetItemDecimal(1,"hourly_rate")
				ldec_total_deductions = idw_dw[1].GetItemDecimal(1,"total_deductions")
				ldec_hour_amount = ldec_paid_hours_lost * ldec_hourly_rate
				ldec_total_award_amount = Round((ldec_day_amount + ldec_hour_amount),2)
				ldec_txn_amount = ldec_total_award_amount + ldec_total_deductions
	
				IF ldec_txn_amount < 0 THEN ldec_txn_amount = 0
	
				idw_dw[1].SetItem(1,"hour_amount",ldec_hour_amount)
				idw_dw[1].SetItem(1,"total_award_amount",ldec_total_award_amount)
				idw_dw[1].SetItem(1,"total_payment_amount",ldec_txn_amount)

				IF idw_dw[2].RowCount() = 1 THEN idw_dw[2].SetItem(1,"txn_amount",ldec_txn_amount)

			CASE "total_award_amount"
				ldec_total_award_amount   = Round(Dec(idw_dw[1].GetText()),2)
				ldec_total_deductions     = idw_dw[1].GetItemDecimal(1,"total_deductions")
				ldec_txn_amount           = ldec_total_award_amount + ldec_total_deductions

				IF ldec_txn_amount < 0 THEN ldec_txn_amount = 0
				idw_dw[1].SetItem(1,"total_payment_amount",ldec_txn_amount)
				//If there is only 1 recipient.  Set the transaction amoutn
				//to the full amount of the payment
				IF idw_dw[2].RowCount() = 1 THEN 
					idw_dw[2].SetItem(idw_dw[2].GetRow(),'txn_amount',ldec_txn_amount)
				End if

			CASE "total_deductions"
				ldec_total_deductions = Round(Dec(idw_dw[1].GetText()),2)
			  
//				If ldec_total_deductions > 0 Then
//					MessageBox('Payment Module - Validation Error','Deduction amount must be a negative number.',Exclamation!)
//					idw_dw[1].SetColumn("total_deductions")
//					RETURN -1
//				END IF
				ldec_total_award_amount    = idw_dw[1].GetItemDecimal(1,"total_award_amount")
				ldec_txn_amount            = ldec_total_award_amount + ldec_total_deductions
			  
				IF ldec_txn_amount < 0 THEN ldec_txn_amount = 0
				idw_dw[1].SetItem(1,"total_payment_amount",ldec_txn_amount)
				//If there is only 1 recipient.  Set the transaction amoutn
				//to the full amount of the payment
				IF idw_dw[2].RowCount() = 1 THEN 
					idw_dw[2].SetItem(idw_dw[2].GetRow(),'txn_amount',ldec_txn_amount)
				End if
			CASE 'tier_no'
				ii_tier_no = Integer(idw_dw[1].GetText())
				li_opening_no = idw_dw[1].GetItemNumber(1,'opening_no')
				nf_default_JS_RTW_date_range(li_opening_no)
				nf_default_JS_RTW_amount(li_opening_no)
			CASE 'explanation'
			CASE 'authorization_group_flag'
				IF idw_dw[1].GetItemNumber(idw_dw[1].GetRow(), 'authorization_group_no') > 0 THEN
					MessageBox('Authorization Group','This payment belongs to an Authorization Group. Please remove the payment from the Authorization Group prior to removing this flag.', Information!)
					RETURN -1
				END IF
		END CHOOSE

	CASE 2
		// this was broken out because the script was too long
		li_return = nf_change_item_2()
		IF li_return = -1 THEN RETURN -1

END CHOOSE
Return 0
end function

public function integer nf_retrieve_rtw_tier ();/*
retrieves the dddw for tier_no
*/

DATAWINDOWCHILD		ldwc_tier_child
DATASTORE				lds_txn_sum_for_tier
INTEGER						li_row , li_rows , li_opening_no, li_counter, li_tier_no, li_sum_rows
DECIMAL						ldec_rtw_incentive_amount, ldec_sum_tier_amount

li_row = idw_dw[1].GetRow()
IF li_row > 0 THEN
	// make sure that you are getting entered values
	idw_dw[1].AcceptText()
	li_opening_no = idw_dw[1].GetItemNumber(li_row,'opening_no')
	
	idw_dw[1].GetChild('tier_no', ldwc_tier_child)
	ldwc_tier_child.SetTransObject(SQLCA)
	ldwc_tier_child.SetFilter('')
	ldwc_tier_child.Filter()
	li_rows =  ldwc_tier_child.Retrieve(il_claim_no,li_opening_no)
	
	lds_txn_sum_for_tier = Create U_DS
	lds_txn_sum_for_tier.DataObject = 'ds_txn_sum_for_tier'
	lds_txn_sum_for_tier.SetTransObject(SQLCA)
	
	// remove tiers from dddw which have been fully paid
	// note that this datawindow is not updateable
	FOR li_counter = 1 TO li_rows
		li_tier_no = ldwc_tier_child.GetItemNumber(li_counter,'rtw_incentive_payment_xref_tier_no')
		
		li_sum_rows = lds_txn_sum_for_tier.Retrieve(il_claim_no, li_opening_no, li_tier_no)

		ldec_sum_tier_amount = lds_txn_sum_for_tier.GetItemDecimal(1,'sum_txns')
		
		SELECT	rtw_incentive_amount
		INTO		:ldec_rtw_incentive_amount
		FROM		Rtw_Incentive_Tier
		WHERE	tier_no = :li_tier_no
		USING SQLCA;
		
		SQLCA.nf_handle_error('n_payment','Embedded SQL: select from Rtw_Incentive_Tier','nf_retrieve_rtw_tier')		
		
		IF ldec_sum_tier_amount = ldec_rtw_incentive_amount THEN
			ldwc_tier_child.DeleteRow(li_counter)
			// decrement upperbound & counter
			li_rows = li_rows - 1
			li_counter = li_counter -1
			
		END IF
		
	NEXT
	
END IF

Return li_rows

end function

public subroutine nf_rtw_defaulting (string as_payment_type_code);/*
makes tier_no dddw visible as appropriate, sets number of cycles for RTW payments
sets payment method & recipient for R1 receiving salary payment txns
*/
STRING		ls_receiving_salary_flag

CHOOSE CASE as_payment_type_code	
	CASE 'R1'
		// Job Search RTW Incentive
		idw_dw[1].Modify("tier_no.Visible='1'")
		idw_dw[1].Modify("t_tier_no.Visible='1'")
		
		IF idw_dw[1].GetItemStatus(1,0,Primary!) = NotModified! THEN
			// Do nothing
		ELSE
			idw_dw[1].SetItem(1, "nmbr_cycles", 1)
			
			ls_receiving_salary_flag	= idw_basic_claim.GetItemString(1,'receiving_salary_flag')
			
			IF ls_receiving_salary_flag = "Y" THEN
				//	Reset some items if it is receiving salary
				idw_dw[2].SetItem(1,"payment_method_code",'A')
				idw_dw[2].SetItem(1,"recipient_type_code","I")
			END IF
		END IF
		
	CASE 'R2','R3'
		// LTD RTW Incentive
		idw_dw[1].Modify("tier_no.Visible='0'")
		idw_dw[1].Modify("t_tier_no.Visible='0'")
		IF idw_dw[1].GetItemStatus(1,0,Primary!) = NotModified! THEN
			// Do nothing
		ELSE
			idw_dw[1].SetItem(1, "nmbr_cycles", 1)
		END IF
	
	CASE ELSE
		idw_dw[1].Modify("tier_no.Visible='0'")
		idw_dw[1].Modify("t_tier_no.Visible='0'")
				
END CHOOSE
end subroutine

public function integer nf_check_rtw_bus_rules (string as_payment_type_code, decimal adec_payment_amount, decimal adec_deduction_amount, decimal adec_bencalc_award_amount, datetime adtm_paid_from_date, datetime adtm_paid_to_date, datetime adtm_effective_date, integer ai_nmbr_cycles);DATAWINDOWCHILD		ldwc_tier_child
DWItemStatus				ldwi_status

STRING			ls_claim_status_code, ls_claim_status_type_code, ls_message

DATETIME		ldtm_employment_start_date , ldtm_check_paid_to_date
DATE				ld_paid_from_date, ld_paid_to_date

INTEGER			li_months_from_employment_start, li_opening_no, li_count_qual, li_tier_row, li_rtw_incentive_no, li_rows
DECIMAL			ldec_rtw_incentive_amt, ldec_rtw_incentive_curr_app_pymt_sum, ldec_rtw_incentive_curr_unapp_pymt_sum
LONG				ll_payment_no


// P10261 RTW Incentive project
// most of the rules for payment types R1,R2,R3 are checked here

ld_paid_from_date	= Date(adtm_paid_from_date)
ld_paid_to_date 		= Date(adtm_paid_to_date)
ll_payment_no = idw_dw[1].GetItemNumber(1,'payment_no')


// rules common to all RTW payment types
IF ai_nmbr_cycles <> 1 THEN
	MessageBox('Payment Module - Validation Error','The number of cycles must be one for RTW Incentive payments.',StopSign!)
	Return -1
END IF

IF adec_deduction_amount <> 0 THEN
	MessageBox('Payment Module - Validation Error','Deduction amount must be zero for RTW Incentive payments.',StopSign!)
	idw_dw[1].SetFocus()
	idw_dw[1].SetColumn("total_deductions")
	Return -1
END IF


CHOOSE CASE as_payment_type_code	
			
	CASE 'R1'
		// Job Search RTW rules
		
		// claim status must be active or finalled/final
		SELECT	claim_status_code , claim_status_type_code
		INTO		:ls_claim_status_code , :ls_claim_status_type_code
		FROM		CLAIM
		WHERE	claim_no = :il_claim_no
		USING SQLCA;
		
		SQLCA.nf_handle_error('n_payment','Embedded SQL: select status from CLAIM','nf_check_rtw_bus_rules')
		
		IF ls_claim_status_code = 'A' OR (ls_claim_status_code = 'F' AND ls_claim_status_type_code = '01') THEN
			//OK
		ELSE
			MessageBox("Payment Module - Validation Error","A Job Search RTW payment can only be made on a claim with Active or Finalled/Final status.",StopSign!)
			Return -1
		END IF
		
		li_opening_no = idw_dw[1].GetItemNumber(1,'opening_no')
		
		Select count(*)
		Into    :li_count_qual
		From  RTW_INCENTIVE_QUALIFICATION
		Where claim_no = :il_claim_no
		And     opening_no = :li_opening_no
		And     rtw_incentive_type_code = 'JS'
		USING SQLCA;
		
		SQLCA.nf_handle_error('n_payment','Embedded SQL: select from RTW_INCENTIVE_QUALIFICATION','nf_check_rtw_bus_rules')
		
		IF li_count_qual < 1 THEN
			MessageBox("Payment Module - Validation Error","A Job Search RTW payment can only be made on an opening with an associated Job Search RTW qualification.",StopSign!)
			idw_dw[1].SetFocus()
			idw_dw[1].SetColumn("opening_no")
			Return -1
		END IF
				
		
		// get chosen tier number
		ii_tier_no = idw_dw[1].GetItemNumber(1,'tier_no')
		IF ii_tier_no = 0 OR IsNull(ii_tier_no) THEN
			MessageBox("Payment Module - Validation Error","A Job Search RTW tier must be chosen for an R1 payment type.",StopSign!)
			idw_dw[1].SetFocus()
			idw_dw[1].SetColumn("tier_no")
			Return -1
		ELSE
			li_tier_row = idw_dw[1].GetChild('tier_no',ldwc_tier_child)
		END IF
		
		ldwi_status = idw_dw[1].GetItemStatus(1,0,Primary!)
		
		SELECT	c.rtw_incentive_amount , a.rtw_incentive_no , c.months_from_employment_start , a.employment_start_date		
		INTO		:ldec_rtw_incentive_amt , :li_rtw_incentive_no , :li_months_from_employment_start , :ldtm_employment_start_date
		FROM		RTW_INCENTIVE_QUALIFICATION				a
		JOIN		RTW_INCENTIVE_PAYMENT_ELIGIBILITY		b	ON a.claim_no = b.claim_no AND a.rtw_incentive_no = b.rtw_incentive_no
		JOIN		Rtw_Incentive_Tier									c	ON b.tier_no = c.tier_no
		WHERE	a.claim_no = :il_claim_no
		AND		a.opening_no = :li_opening_no
		AND		a.qualification_flag = 'Y'
		AND		b.payment_eligibility_flag = 'Y'
		AND		c.tier_no = :ii_tier_no
		USING SQLCA;
		
		SQLCA.nf_handle_error('n_payment','Embedded SQL: select from RTW_INCENTIVE_QUALIFICATION, ...','nf_check_rtw_bus_rules')
		
		
		// add sum of Job Search RTW Incentive payments to existing payments
		// to ensure that allowable amount for tier is not exceeded
		IF ldwi_status = DataModified! THEN
			// updating an existing R1 payment

			// sum of any other existing payments
			// except for applied or unapplied third party recovery txns
			
			// applied
			SELECT	IsNull(Sum(c.txn_amount),0)
			INTO		:ldec_rtw_incentive_curr_app_pymt_sum
			FROM		PAYMENT									a
			JOIN		RTW_INCENTIVE_PAYMENT_XREF	b	ON	a.claim_no		= b.claim_no
																			AND	a.payment_no	= b.payment_no
			JOIN		APPLIED_CLAIM_TXN						c	ON	a.payment_no	= c.payment_no
			WHERE	b.claim_no				= :il_claim_no
			AND		b.tier_no					= :ii_tier_no
			AND		b.rtw_incentive_no	= :li_rtw_incentive_no
			AND		b.payment_no			<> :ll_payment_no
			AND NOT ( c.txn_type_code = 'J' AND c.txn_sub_type_code = '5' )
			USING SQLCA;
		
			SQLCA.nf_handle_error('n_payment','Embedded SQL: select sum from APPLIED_CLAIM_TXN (1)','nf_check_rtw_bus_rules')
			
			
			// unapplied
			SELECT	IsNull(Sum(c.txn_amount),0)
			INTO		:ldec_rtw_incentive_curr_unapp_pymt_sum
			FROM		PAYMENT									a
			JOIN		RTW_INCENTIVE_PAYMENT_XREF	b	ON	a.claim_no		= b.claim_no
																			AND	a.payment_no	= b.payment_no
			JOIN		UNAPPLIED_CLAIM_TXN					c	ON	a.payment_no	= c.payment_no
			WHERE	b.claim_no				= :il_claim_no
			AND		b.tier_no					= :ii_tier_no
			AND		b.rtw_incentive_no	= :li_rtw_incentive_no
			AND		b.payment_no			<> :ll_payment_no
			AND NOT ( c.txn_type_code = 'J' AND c.txn_sub_type_code = '5' )
			USING SQLCA;
		
			SQLCA.nf_handle_error('n_payment','Embedded SQL: select sum from UNAPPLIED_CLAIM_TXN (1)','nf_check_rtw_bus_rules')
					
			
			IF adec_payment_amount + ldec_rtw_incentive_curr_app_pymt_sum + ldec_rtw_incentive_curr_unapp_pymt_sum > ldec_rtw_incentive_amt THEN
				ls_message = 'The payment amount for the updated Job Search RTW Incentive would exceed limit ('+String(ldec_rtw_incentive_amt,'$0.00')+') for tier '+String(ii_tier_no)+'.' 
				IF ldec_rtw_incentive_curr_unapp_pymt_sum > 0 THEN
					IF ldec_rtw_incentive_curr_app_pymt_sum > 0 THEN
						ls_message = ls_message + '~nThere are processed and unprocessed payment transactions for this tier which total ' +String(ldec_rtw_incentive_curr_app_pymt_sum + ldec_rtw_incentive_curr_unapp_pymt_sum,'$0.00')+'.'
					ELSE
						ls_message = ls_message + '~nThere are unprocessed payment transactions for this tier which total ' +String(ldec_rtw_incentive_curr_app_pymt_sum + ldec_rtw_incentive_curr_unapp_pymt_sum,'$0.00')+'.'
					END IF
				END IF
				
				MessageBox('Payment Module - Validation Error',ls_message,StopSign!)
				Return -1			
			END IF
								
		ELSE
			// new R1 payment
			
			// sum of any other existing payments
			// except for applied or unapplied third party recovery txns
			SELECT	IsNull(Sum(c.txn_amount),0)
			INTO		:ldec_rtw_incentive_curr_app_pymt_sum
			FROM		PAYMENT									a
			JOIN		RTW_INCENTIVE_PAYMENT_XREF	b	ON	a.claim_no		= b.claim_no
																			AND	a.payment_no	= b.payment_no
			JOIN		APPLIED_CLAIM_TXN						c	ON	a.payment_no	= c.payment_no
			WHERE	b.claim_no				= :il_claim_no
			AND		b.tier_no					= :ii_tier_no
			AND		b.rtw_incentive_no	= :li_rtw_incentive_no
			AND NOT ( c.txn_type_code = 'J' AND c.txn_sub_type_code = '5' )
			USING SQLCA;
		
			SQLCA.nf_handle_error('n_payment','Embedded SQL: select sum from APPLIED_CLAIM_TXN (2)','nf_check_rtw_bus_rules')
			
			// sum of any existing payments
			SELECT	IsNull(Sum(c.txn_amount),0)
			INTO		:ldec_rtw_incentive_curr_unapp_pymt_sum
			FROM		PAYMENT									a
			JOIN		RTW_INCENTIVE_PAYMENT_XREF	b	ON	a.claim_no		= b.claim_no
																			AND	a.payment_no	= b.payment_no
			JOIN		UNAPPLIED_CLAIM_TXN					c	ON	a.payment_no	= c.payment_no
			WHERE	b.claim_no				= :il_claim_no
			AND		b.tier_no					= :ii_tier_no
			AND		b.rtw_incentive_no	= :li_rtw_incentive_no
			AND NOT ( c.txn_type_code = 'J' AND c.txn_sub_type_code = '5' )
			USING SQLCA;
		
			SQLCA.nf_handle_error('n_payment','Embedded SQL: select sum from UNAPPLIED_CLAIM_TXN (2)','nf_check_rtw_bus_rules')
			
			IF adec_payment_amount + ldec_rtw_incentive_curr_app_pymt_sum + ldec_rtw_incentive_curr_unapp_pymt_sum > ldec_rtw_incentive_amt THEN
				ls_message = 'The payment amount for the updated Job Search RTW Incentive would exceed limit ('+String(ldec_rtw_incentive_amt,'$0.00')+') for tier '+String(ii_tier_no)+'.' 
				IF ldec_rtw_incentive_curr_unapp_pymt_sum > 0 THEN
					IF ldec_rtw_incentive_curr_app_pymt_sum > 0 THEN
						ls_message = ls_message + '~nThere are processed and unprocessed payment transactions for this tier which total ' +String(ldec_rtw_incentive_curr_app_pymt_sum + ldec_rtw_incentive_curr_unapp_pymt_sum,'$0.00')+'.'
					ELSE
						ls_message = ls_message + '~nThere are unprocessed payment transactions for this tier which total ' +String(ldec_rtw_incentive_curr_app_pymt_sum + ldec_rtw_incentive_curr_unapp_pymt_sum,'$0.00')+'.'
					END IF
				END IF
				
				MessageBox('Payment Module - Validation Error',ls_message,StopSign!)
				Return -1
			END IF
		END IF
		
		
		// check that paid from date is same as employment start date for chosen tier, opening, claim
		IF ld_paid_from_date <> Date(ldtm_employment_start_date) THEN
			MessageBox('Invalid Paid From Date','The from date must equal the employment start date for the Job Search RTW Incentive payment.', StopSign!)
			idw_dw[1].SetFocus()
			idw_dw[1].SetColumn('paid_from_date')
			Return -1
		END IF
		
						
		// check that paid to date is correct number of months after paid from date
		SELECT Distinct DATEADD(mm, :li_months_from_employment_start, :adtm_paid_from_date )
		INTO :ldtm_check_paid_to_date
		FROM sysobjects
		USING SQLCA;
		
		SQLCA.nf_handle_error('n_payment','Embedded SQL: select from sysobjects','nf_check_rtw_bus_rules')
		
		IF ld_paid_to_date <> Date(ldtm_check_paid_to_date) THEN
			MessageBox('Invalid Paid To Date','The "To" date must be '+String(li_months_from_employment_start)+' months after the employment start date of the Job Search RTW Incentive for tier '+String(ii_tier_no)+'.', StopSign!)
			idw_dw[1].SetFocus()
			idw_dw[1].SetColumn('paid_to_date')
			Return -1
		END IF
		
		
	CASE 'R2', 'R3'
		// LTD RTW payment rules
		
		SELECT Distinct DATEADD(yy, 1, :adtm_effective_date )
		INTO :ldtm_check_paid_to_date
		FROM sysobjects
		USING SQLCA;
		
		SQLCA.nf_handle_error('n_payment','Embedded SQL: select from sysobjects','nf_check_rtw_bus_rules')
		
		IF ld_paid_to_date > Date(ldtm_check_paid_to_date) THEN
			MessageBox('Invalid Paid To Date','The "To" date must not be more than one year after the effective date of the RTW Incentive benefit calculation.')
			idw_dw[1].SetFocus()
			idw_dw[1].SetColumn('paid_to_date')
			Return -1
		END IF
		
		IF adec_payment_amount > adec_bencalc_award_amount*12 THEN
			MessageBox('Benefit Calculation Amount','The amount of the LTD RTW Incentive payment must not be more '+&
																	'~nthan 12 times the amount of the Benefit Calculation award.',StopSign!)
			RETURN -1
		END IF
		
END CHOOSE

end function

public function integer nf_setup_rtw_incentive_payment_xref (long al_payment_no, string as_payment_type_code);/*
this function sets the values in idw_dw[3] so that a record will be inserted into RTW_INCENTIVE_PAYMENT_XREF
if this is a new payment, or updates RTW_INCENTIVE_PAYMENT_XREF if it's a existing payment
*/

DataWindowChild	ldwc_tier_child
LONG					ll_row , ll_find_row, ll_rtw_incentive_no
INTEGER				li_rtn

SELECT	rtw_incentive_no
INTO		:ll_rtw_incentive_no
FROM		RTW_INCENTIVE_PAYMENT_XREF
WHERE	payment_no = :al_payment_no
USING SQLCA;
	
SQLCA.nf_handle_error('n_payment','EMBEDDED SQL: Select count(*) from RTW_INCENTIVE_PAYMENT_XREF','nf_setup_rtw_incentive_payment_xref')

IF ll_rtw_incentive_no <> 0 AND Not IsNull(ll_rtw_incentive_no) THEN
	
	li_rtn = idw_dw[3].Retrieve(al_payment_no)
	
	IF li_rtn > 1 THEN
		MessageBox('Data Integrity Problem', 'Contact the HELPDESK. There has been a data integrity problem for this RTW Incentive payement.')
		RETURN -1
	END IF
	
	IF as_payment_type_code = 'R1' THEN
		// tier_no is the only editable value, but the rtw_incentive_no can change if the opening/bencalc has been changed.
		idw_dw[3].SetItem(1, 'rtw_incentive_no', ll_rtw_incentive_no)
		idw_dw[3].SetItem(1, 'tier_no', ii_tier_no)
	ELSE
		// if the pay type is not R1, then it must have been changed from R1
		// this should be the only way that there was an xref record, so...
		// delete record
		idw_dw[3].DeleteRow(1)
	END IF
	
ELSEIF ll_rtw_incentive_no = 0 OR IsNull(ll_rtw_incentive_no) THEN
	IF as_payment_type_code = 'R1' THEN
		
		li_rtn = idw_dw[1].GetChild('tier_no',ldwc_tier_child)
		ll_find_row = ldwc_tier_child.Find('rtw_incentive_payment_xref_tier_no = '+ String(ii_tier_no),1,ldwc_tier_child.RowCount())
		ll_rtw_incentive_no = ldwc_tier_child.GetItemNumber(ll_find_row,'rtw_incentive_no')
		
		ll_row = idw_dw[3].InsertRow(0)
			
		li_rtn = idw_dw[3].SetItem(ll_row,'claim_no',il_claim_no)
		li_rtn = idw_dw[3].SetItem(ll_row,'rtw_incentive_type_code','JS')
		li_rtn = idw_dw[3].SetItem(ll_row,'rtw_incentive_no',ll_rtw_incentive_no)
		li_rtn = idw_dw[3].SetItem(ll_row,'tier_no',ii_tier_no)
		li_rtn = idw_dw[3].SetItem(ll_row,'payment_no',al_payment_no)
	END IF
END IF

RETURN 0
end function

public subroutine nf_default_js_rtw_date_range (ref integer ai_opening_no);// default the paid date range for Job Search RTW Incentive payment

INTEGER		li_row , li_months_from_employment_start
DATETIME	ldtm_employment_start_date, ldtm_paid_to_date


SELECT	months_from_employment_start , employment_start_date 
INTO		:li_months_from_employment_start , :ldtm_employment_start_date 
FROM		RTW_INCENTIVE_QUALIFICATION a
JOIN		RTW_INCENTIVE_PAYMENT_ELIGIBILITY b ON a.claim_no = b.claim_no AND a.rtw_incentive_no = b.rtw_incentive_no
JOIN		Rtw_Incentive_Tier c ON b.tier_no = c.tier_no
WHERE	a.claim_no = :il_claim_no
AND		a.opening_no = :ai_opening_no
AND		a.qualification_flag = 'Y'
AND		b.payment_eligibility_flag = 'Y'
AND		c.tier_no = :ii_tier_no
USING SQLCA;

SQLCA.nf_handle_error('n_payment','Embedded SQL: select from RTW_INCENTIVE_QUALIFICATION...','nf_default_js_rtw_date_range')

idw_dw[1].SetItem(1,'paid_from_date',ldtm_employment_start_date)


SELECT Distinct DATEADD(mm, :li_months_from_employment_start, :ldtm_employment_start_date )
INTO :ldtm_paid_to_date
FROM sysobjects
USING SQLCA;

SQLCA.nf_handle_error('n_payment','Embedded SQL: select from sysobjects','nf_default_js_rtw_date_range')

idw_dw[1].SetItem(1,'paid_to_date',ldtm_paid_to_date)
end subroutine

public subroutine nf_default_js_rtw_amount (ref integer ai_opening_no);LONG			ll_payment_no, ll_rtw_incentive_no

DECIMAL		ldec_rtw_incentive_current_applied_sum, ldec_rtw_incentive_current_unapplied_sum, ldec_rtw_incentive_amt, ldec_new_amount

SELECT	a.rtw_incentive_no , c.rtw_incentive_amount
INTO		:ll_rtw_incentive_no, :ldec_rtw_incentive_amt
FROM		RTW_INCENTIVE_QUALIFICATION a
JOIN		RTW_INCENTIVE_PAYMENT_ELIGIBILITY b ON a.claim_no = b.claim_no AND a.rtw_incentive_no = b.rtw_incentive_no
JOIN		Rtw_Incentive_Tier c ON b.tier_no = c.tier_no
WHERE	a.claim_no = :il_claim_no
AND		a.opening_no = :ai_opening_no
AND		a.qualification_flag = 'Y'
AND		b.payment_eligibility_flag = 'Y'
AND		c.tier_no = :ii_tier_no
USING SQLCA;

SQLCA.nf_handle_error('n_payment','Embedded SQL: select from RTW_INCENTIVE_QUALIFICATION...','nf_default_js_rtw_amount')



IF idw_dw[1].GetItemStatus(idw_dw[1].GetRow(), 0, Primary!) = DataModified! THEN
	// updating an existing R1 payment
	ll_payment_no = idw_dw[1].GetItemNumber(1,'payment_no')
	
	// sum of any other existing applied txns
	SELECT	IsNull(Sum(c.txn_amount),0)
	INTO		:ldec_rtw_incentive_current_applied_sum
	FROM		PAYMENT									a
	JOIN		RTW_INCENTIVE_PAYMENT_XREF	b	ON	a.claim_no		= b.claim_no
																	AND	a.payment_no	= b.payment_no
	JOIN		APPLIED_CLAIM_TXN						c	ON	a.payment_no	= c.payment_no
	WHERE	b.claim_no				= :il_claim_no
	AND		b.tier_no					= :ii_tier_no
	AND		b.rtw_incentive_no	= :ll_rtw_incentive_no
	AND		b.payment_no			<> :ll_payment_no
	AND		NOT ( c.txn_type_code = 'J' AND c.txn_sub_type_code = '5' )
	USING SQLCA;

	SQLCA.nf_handle_error('n_payment','Embedded SQL: select sum from APPLIED_CLAIM_TXN (1)','nf_default_js_rtw_amount')
	
	
	// sum of any other existing unapplied txns
	SELECT	IsNull(Sum(c.txn_amount),0)
	INTO		:ldec_rtw_incentive_current_unapplied_sum
	FROM		PAYMENT									a
	JOIN		RTW_INCENTIVE_PAYMENT_XREF	b	ON	a.claim_no		= b.claim_no
																	AND	a.payment_no	= b.payment_no
	JOIN		UNAPPLIED_CLAIM_TXN					c	ON	a.payment_no	= c.payment_no
	WHERE	b.claim_no				= :il_claim_no
	AND		b.tier_no					= :ii_tier_no
	AND		b.rtw_incentive_no	= :ll_rtw_incentive_no
	AND		b.payment_no			<> :ll_payment_no
	AND		NOT ( c.txn_type_code = 'J' AND c.txn_sub_type_code = '5' )
	USING SQLCA;

	SQLCA.nf_handle_error('n_payment','Embedded SQL: select sum from UNAPPLIED_CLAIM_TXN (1)','nf_default_js_rtw_amount')
	
ELSE
	// new R1 payment
	// sum of any existing applied txns
	SELECT	IsNull(Sum(c.txn_amount),0)
	INTO		:ldec_rtw_incentive_current_applied_sum
	FROM		PAYMENT									a
	JOIN		RTW_INCENTIVE_PAYMENT_XREF	b	ON	a.claim_no		= b.claim_no
																	AND	a.payment_no	= b.payment_no
	JOIN		APPLIED_CLAIM_TXN						c	ON	a.payment_no	= c.payment_no
	WHERE	b.claim_no				= :il_claim_no
	AND		b.tier_no					= :ii_tier_no
	AND		b.rtw_incentive_no	= :ll_rtw_incentive_no
	AND		NOT ( c.txn_type_code = 'J' AND c.txn_sub_type_code = '5' )
	USING SQLCA;

	SQLCA.nf_handle_error('n_payment','Embedded SQL: select sum from APPLIED_CLAIM_TXN (2)','nf_default_js_rtw_amount')
	
	
	// sum of any existing unapplied txns
	SELECT	IsNull(Sum(c.txn_amount),0)
	INTO		:ldec_rtw_incentive_current_unapplied_sum
	FROM		PAYMENT									a
	JOIN		RTW_INCENTIVE_PAYMENT_XREF	b	ON	a.claim_no		= b.claim_no
																	AND	a.payment_no	= b.payment_no
	JOIN		UNAPPLIED_CLAIM_TXN					c	ON	a.payment_no	= c.payment_no
	WHERE	b.claim_no				= :il_claim_no
	AND		b.tier_no					= :ii_tier_no
	AND		b.rtw_incentive_no	= :ll_rtw_incentive_no
	AND		NOT ( c.txn_type_code = 'J' AND c.txn_sub_type_code = '5' )
	USING SQLCA;

	SQLCA.nf_handle_error('n_payment','Embedded SQL: select sum from APPLIED_CLAIM_TXN (2)','nf_default_js_rtw_amount')
			
END IF

ldec_new_amount = ldec_rtw_incentive_amt - (ldec_rtw_incentive_current_applied_sum+ldec_rtw_incentive_current_unapplied_sum)
idw_dw[1].SetItem(1,'total_award_amount',ldec_new_amount)
idw_dw[1].SetItem(1,'total_payment_amount',ldec_new_amount)
idw_dw[2].SetItem(1,'txn_amount',ldec_new_amount)
end subroutine

public function integer nf_get_bank (long al_recipient_no, string as_recipient_type_code, ref string as_bank_no, ref string as_bank_transit_no, ref string as_bank_account_no, long al_payment_no, long al_txn_no);LONG		ll_result
STRING	ls_source_table


IF NOT IsNull(al_recipient_no) THEN
		
	ls_source_table = idw_dw[2].GetItemString( idw_dw[2].GetRow(), 'source_table_code' )
	
	IF IsNull(ls_source_table) THEN ls_source_table = 'U'
	
	IF ls_source_table = 'A' THEN	
	// Get bank info. from APPLIED_CLAIM_TXN table - Added for pr2452, Jun.'02
	
		SELECT bank_no, bank_transit_no, bank_account_no
		  INTO :as_bank_no, :as_bank_transit_no, :as_bank_account_no
		  FROM APPLIED_CLAIM_TXN
		 WHERE claim_no   = :il_claim_no
		   AND payment_no = :al_payment_no
			AND txn_no     = :al_txn_no
		 USING SQLCA;
		 
		ll_result = SQLCA.nf_handle_error('Embedded SQL: select from APPLIED_CLAIM_TXN','n_payment','nf_get_bank')
		IF ll_result  < 0 THEN Return -1

		IF ll_result = 100 THEN Return -1

	ELSEIF ls_source_table = 'U' THEN
	
		IF as_recipient_type_code = 'I' THEN
			SELECT bank_no, bank_transit_no, bank_account_no
		     INTO :as_bank_no, :as_bank_transit_no, :as_bank_account_no
			FROM INDIVIDUAL
			WHERE individual_no = :al_recipient_no
			USING SQLCA;
		ELSE
			SELECT bank_no, bank_transit_no, bank_account_no
		     INTO       :as_bank_no, :as_bank_transit_no, :as_bank_account_no
			FROM     BANK_INFO a 
			RIGHT OUTER JOIN PROVIDER b ON a.recipient_no = b.provider_no 
			                                                             AND a.recipient_type_code = b.provider_type_code
			WHERE b.provider_no = :al_recipient_no
			AND         b.provider_type_code = :as_recipient_type_code
			USING   SQLCA;
		END IF
	
		ll_result = SQLCA.nf_handle_error('SELECT bank_no, bank_transit_no, bank_account_no','n_payment','nf_get_bank')
		IF ll_result  < 0 THEN Return -1
	
		IF ll_result = 100 THEN Return -1
				
		IF IsNull(as_bank_no) THEN as_bank_no = ''
		IF IsNull(as_bank_transit_no) THEN as_bank_transit_no = ''
		IF IsNull(as_bank_account_no) THEN as_bank_account_no = ''

	ELSE
		RETURN -1
	END IF

END IF

RETURN 0
end function

public function integer nf_authorize_group (ref datawindow adw_dw, string as_action, boolean ab_remove);//Authorization function - call to authorize new group, groups with additions/removals
LONG ll_cntr, ll_rowcount
DATETIME ldtm_authorize_date
STRING ls_authorize_user

IF ab_remove THEN
	adw_dw.SetFilter( "checkbox_group = 0")
	adw_dw.Filter()
END IF

IF as_action = 'U' THEN
//	IF not within limits or deleting group, unauthorize all payments
	ls_authorize_user = ''
	SetNull(ldtm_authorize_date)
ELSEIF as_action = 'A' THEN
//	IF within limits, authorize all payments	
//	IF removing a payment from a group, re-authorize all payments	
//	IF adding a payment to a group, re-authorize all payments
	ls_authorize_user = vgst_user_profile.user_id
	ldtm_authorize_date = f_server_datetime()
END IF

ll_rowcount = adw_dw.RowCount()

IF ll_rowcount > 0 THEN
	//Update records to either authorize or unauthorize records depending on which action user has taken
	FOR ll_cntr = 1 to ll_rowcount
		adw_dw.SetItem(ll_cntr, 'authorized_by_code', ls_authorize_user)
		adw_dw.SetItem(ll_cntr, 'authorized_date', ldtm_authorize_date)
	NEXT 
END IF


RETURN 0
end function

public function integer nf_flagged_ungrouped (long al_claim_no);LONG ll_count


SELECT COUNT(*)
INTO     :ll_count
FROM   PAYMENT a
WHERE a.claim_no      = :al_claim_no
AND      a.authorization_group_flag = 'Y'
AND      a.authorization_group_no   = 0
USING   SQLCA;

SQLCA.nf_handle_error('n_payment','nf_flagged_ungrouped()','SELECT COUNT(*) FROM PAYMENT')

RETURN ll_count



end function

public function boolean nf_is_valid_status_act_pymt ();
/*  This function checks for a valid claim status for the purpose of making account payments. 
     It is a modified version of nf_is_valid_status_act()  found in n_payment, which was also being used for checking
	 claim status for the regular payment window (LOE, LTD), so this new function was created for T012790 in order to
	 separate the functionality, and provide for different criteria for claim status for account payment vs payment
*/

STRING ls_claim_status_code, ls_claim_status_type_code

BOOLEAN lb_status_ok

ls_claim_status_code = idw_basic_claim.GetItemString(1,'claim_status_code')
ls_claim_status_type_code = idw_basic_claim.GetItemString(1,'claim_status_type_code')

// check claim status and status type
	
IF ls_claim_status_code = 'A' OR          &	
(ls_claim_status_code = 'F'    AND         &
(ls_claim_status_type_code = '01'   OR   &
ls_claim_status_type_code  = '02'   OR    &
ls_claim_status_type_code  = '03'   OR    &
ls_claim_status_type_code  = '04')) OR   &	
ls_claim_status_code = 'P'              OR   & 	
ls_claim_status_code = 'J'               OR   &  	
(ls_claim_status_code = 'R'  AND              &                    
(ls_claim_status_type_code = '07' OR ls_claim_status_type_code =  '18') )   THEN             
	lb_status_ok = TRUE
ELSE
	lb_status_ok =  FALSE
END IF

RETURN lb_status_ok
end function

public function boolean nf_is_valid_doctype_act_pymt (string as_doctype);
STRING ls_claim_status_code, ls_claim_status_type_code

BOOLEAN lb_doctype_ok


ls_claim_status_code = idw_basic_claim.GetItemString(1,'claim_status_code')
ls_claim_status_type_code = idw_basic_claim.GetItemString(1,'claim_status_type_code')

// In addition to checking for the various status codes in nf_is_valid_status_act_pymt(), there are some further restrictions as to the 
// doc types that can be paid for claim statuses 'R' - '07','18', and 'P' and 'J' . 

// for Rejected (01, 18), Adjudicated, or Preadjudicated claims, below is a list of the only doc types allowed. 
// NOTE: this is a bit different  from the function wf_isaccount in w_account_payment, which checks for 'A' account type docs plus MPC, MPD SDC and SDD, 
// therefore that function will allow things like 'ANP', 'AO', 'ARX' etc., but these types need to be excluded for 'R', 'P', 'J' claim status, so below we code just the doc types allowed

IF ls_claim_status_code = 'P'  OR      ls_claim_status_code = 'J' OR   &  	
   (ls_claim_status_code = 'R'  AND  (ls_claim_status_type_code = '07' OR ls_claim_status_type_code =  '18') )   THEN  

		lb_doctype_ok = match(as_doctype,"AC")  OR match(as_doctype,"AD")    OR match(as_doctype,"AH") &
						   OR match(as_doctype,"AI")   OR match(as_doctype,"AP")    OR match(as_doctype,"AT")  &
						   OR match(as_doctype,"MPC") OR match(as_doctype,"MPD") OR match(as_doctype,"SDC")  OR match(as_doctype,"SDD")  
	
	IF lb_doctype_ok  THEN
		RETURN TRUE  
	ELSE
		RETURN FALSE
	END IF
ELSE 
	return true  // if we dont explicitly return something, a null is returned
END IF
end function

on n_payment.create
call super::create
end on

on n_payment.destroy
call super::destroy
end on

