$PBExportHeader$n_claims.sru
$PBExportComments$user object containing all claim business rules
forward
global type n_claims from n_pdc
end type
end forward

global type n_claims from n_pdc
end type
global n_claims n_claims

type variables
BOOLEAN ib_rehab_officer_needed
BOOLEAN ib_rloe_opening
STRING is_opening_type_code
DATETIME idtm_min_accident_recurrence
STRING is_change_status = ""
STRING is_claim_status_change
gstr_report_fee_documents istr_report_fee_documents
n_claim_employer	inv_claim_employer
n_reminders		inv_reminders
DATAWINDOWCHILD idwc_sob

end variables

forward prototypes
public function integer nf_retrieve (long al_claim_no)
public function integer nf_log_cost_alloc_change (long al_claim_no, long al_event_no)
public function integer nf_log_claim_status_change (long al_claim_no, long al_event_no)
public function integer nf_validate_current_filing (string as_curr_est_filed_payroll, string as_curr_act_filed_payroll)
public function integer nf_validate_seasonal_coverage (date adt_accident_date, date adt_start_seasonal_coverage, date adt_end_seasonal_coverage)
public function string nf_get_cost_alloc_name (long al_cost_alloc_no)
public function string nf_get_oper_name (long al_cost_alloc_no, long al_operation_no)
public function integer nf_validate_legislation (string as_legislation_code)
public function integer nf_determine_claim_status (string as_reason)
public function long nf_get_next_claim ()
public function integer nf_status_changed (long al_datawindow)
public function boolean nf_rehab_plan_exists (long al_claim_no)
public function long nf_get_next_identifier ()
public function integer nf_log_transfers_to_rdb ()
public function integer nf_report_fee_doc_presave (string as_status)
public function integer nf_validate_cost_alloc (long al_cost_alloc_no, long al_cost_alloc_operation_no, string as_cost_alloc_type)
public function integer nf_cost_allocation_no_cutoff (date adt_cutoff_date, datetime adt_accident_date, ref long al_no, long al_old_cost_alloc, long al_new_cost_alloc, string as_number_type)
public function integer nf_set_defaults ()
public function integer nf_sum_days_hours (long al_claim_no)
public function integer nf_status_change_to_rejected (long al_claim_no)
public function long nf_set_identifiers ()
public function string nf_get_status_descriptions (string as_code, string as_sub_code)
public function integer nf_set_unused_fields ()
public function integer nf_check_mandatory ()
public function integer nf_change_item (long al_datawindow)
public function integer nf_log_events ()
public function integer nf_check_bus_rule ()
public function integer nf_check_accident_county (string as_claim_status_code, string as_claim_status_type_code)
public function integer nf_check_cppd_reminders ()
public function integer nf_check_bus_rule_shared ()
public subroutine nf_firefighter_cost_alloc_change (long al_current_row, string as_cost_alloc_no)
public subroutine nf_firefighter_cost_alloc_oper_change (long al_current_row, string as_cost_alloc_operation_no)
public function integer nf_create_firefighter_claim_master (long al_new_fca_claim_no)
public function integer nf_create_firefighter_claim_participant (long al_new_fca_claim_no, long al_individual_no, long al_rejected_wca_claim_no)
public function integer nf_create_firefighter_claim_accident (long al_new_fca_claim_no, long al_rejected_wca_claim_no)
public function integer nf_create_firefighter_claim (long al_new_fca_claim_no, long al_individual_no, long al_rejected_wca_claim_no)
public function integer nf_create_firefighter_claim_events (long al_new_fca_claim_no, long al_rejected_wca_claim_no)
public function integer nf_create_firefighter_difficult_claim (long al_new_fca_claim_no, long al_rejected_wca_claim_no)
public function integer nf_create_firefighter (long al_rejected_wca_claim_no, long al_individual_no, ref long al_new_fca_claim_no)
end prototypes

public function integer nf_retrieve (long al_claim_no);LONG             ll_rows
DATAWINDOWCHILD  ldwc_child
STRING           ls_status, ls_filter, ls_string, ls_administering_act_code
LONG ll_row

/*	dw 1 = main claim
	dw 2 = claim participant

*/

	ll_rows = idw_dw[1].Retrieve(al_claim_no)
	IF SQLCA.nf_handle_error('retrieve child','n_claims', 'nf_retrieve') < 0 THEN
		Return -1
	END IF

/*	 now set a filter with the correct status code
*/
	ls_status = idw_dw[1].GetItemString(ll_rows,'claim_status_code')
	idw_dw[1].GetChild('claim_status_type_code', ldwc_child)
	ldwc_child.SetFilter("claim_status_code = '" + ls_status + "'")
	ldwc_child.Filter()
/*	NOTE: this statement was added because on a cancel - which fires off this function
	the description was replace by the code.  The code remained there until the column got focus
	then the description would appeared in the column
*/
	idw_dw[1].SetColumn('claim_status_type_code')

/*	the following logic is based on the diagram from May/96
	this determines what are the valid status changes based on the current status of 
	the claim
*/
	idw_dw[1].GetChild('claim_status_code', ldwc_child)
	CHOOSE CASE ls_status
   	CASE 'P'
      	ls_filter = "claim_status_code = 'P' OR claim_status_code = 'J' OR claim_status_code = 'R'"
	      ldwc_child.SetFilter(ls_filter)
   	   ldwc_child.Filter()
	   CASE 'J'
   	   ls_filter = "claim_status_code = 'J' OR claim_status_code = 'A' OR claim_status_code = 'F' OR claim_status_code = 'R'"
      	ldwc_child.SetFilter(ls_filter)
	      ldwc_child.Filter()
   	CASE 'A'
      	ls_filter = "claim_status_code = 'A' OR claim_status_code = 'F'"
	      ldwc_child.SetFilter(ls_filter)
   	   ldwc_child.Filter()
	   CASE 'F'
   	   ls_filter = "claim_status_code = 'F' OR claim_status_code = 'J'"
      	ldwc_child.SetFilter(ls_filter)
	      ldwc_child.Filter()
   	CASE 'R'
	      ls_filter = "claim_status_code = 'R' OR claim_status_code = 'J'"
   	   ldwc_child.SetFilter(ls_filter)
      	ldwc_child.Filter()
	END CHOOSE
	
	idw_dw[1].GetChild('disallow_reason_code', ldwc_child)
	IF ls_status = 'R' AND idw_dw[1].GetItemString(ll_rows,'claim_status_type_code') = '07' THEN
		ll_row = idw_dw[1].GetRow()
		
		IF ll_row > 0 THEN
			IF 	idw_dw[1].GetItemString(ll_row, 'claim_status_type_code') = '07' THEN
			/*	populate the drop down with disallowed reasons
			*/				
				ls_administering_act_code = 	idw_dw[1].GetItemString(ll_row, 'administering_act_code')
				
				IF ls_administering_act_code = 'FCA' THEN 
					ls_filter = "fca_act_flag = 'Y'"
				ELSE 	
					ls_filter = "wca_act_flag = 'Y'"
				END IF
				idw_dw[1].GetChild('disallow_reason_code',ldwc_child)
				ldwc_child.SetFilter(ls_filter)  
				ldwc_child.Filter()
			ELSE
				idw_dw[1].GetChild('disallow_reason_code',ldwc_child)
				ldwc_child.SetFilter("disallow_reason_code = ' '")
				ldwc_child.Filter()
				idw_dw[1].SetItem(ll_row, 'disallow_reason_code', ' ')
			END IF
		END IF
	ELSE
		ldwc_child.SetFilter("disallow_reason_code = ''")
	END IF
	ldwc_child.Filter()

/*	set the cost alloc titles
*/
	IF ll_rows = 1 THEN
		idw_dw[1].SetItem(1,'cost_alloc_emp_name', nf_get_cost_alloc_name(idw_dw[1].GetItemNumber(1, 'cost_alloc_no')))
		idw_dw[1].SetItem(1,'cost_alloc_oper_name', nf_get_oper_name(idw_dw[1].GetItemNumber(1, 'cost_alloc_no'), idw_dw[1].GetItemNumber(1, 'cost_alloc_operation_no')))

		idw_dw[1].SetItem(1,'accident_emp_name', nf_get_cost_alloc_name(idw_dw[1].GetItemNumber(1, 'accident_employer_no')))
		idw_dw[1].SetItem(1,'accident_oper_name', nf_get_oper_name(idw_dw[1].GetItemNumber(1, 'accident_employer_no'), idw_dw[1].GetItemNumber(1, 'accident_employer_operation_no')))

		idw_dw[1].SetItem(1,'pen_emp_name', nf_get_cost_alloc_name(idw_dw[1].GetItemNumber(1, 'pen_cost_alloc_no')))
		idw_dw[1].SetItem(1,'pen_oper_name', nf_get_oper_name(idw_dw[1].GetItemNumber(1, 'pen_cost_alloc_no'), idw_dw[1].GetItemNumber(1, 'pen_cost_alloc_operation_no')))

	END IF
	
	/* for the medical aid project  this is for dw_payment
	*/
	ll_rows = idw_dw[4].Retrieve(al_claim_no)
	IF SQLCA.nf_handle_error('retrieve child','n_claims', 'nf_retrieve') < 0 THEN
		Return -1
	END IF

	/* for the medical aid project  this is for dw_unapplied_claim_txn
	*/
	ll_rows = idw_dw[5].Retrieve(al_claim_no)
	IF SQLCA.nf_handle_error('retrieve child','n_claims', 'nf_retrieve') < 0 THEN
		Return -1
	END IF
	
	
/* NOTE this was added because of the above set column
*/
	idw_dw[1].SetColumn('claim_manager_user_id')
Return ll_rows

end function

public function integer nf_log_cost_alloc_change (long al_claim_no, long al_event_no);LONG  ll_result, ll_old_alloc, ll_new_alloc, ll_old_oper_no, ll_new_oper_no

/*	log changes in the cost allocation 
*/


	ll_result = idw_dw[1].GetRow()
	IF ll_result > 0 THEN
   	ll_old_alloc = idw_dw[1].GetItemNumber(ll_result, 'cost_alloc_no', Primary!, TRUE)
	   ll_new_alloc = idw_dw[1].GetItemNumber(ll_result, 'cost_alloc_no')
   	ll_old_oper_no = idw_dw[1].GetItemNumber(ll_result, 'cost_alloc_operation_no', Primary!, TRUE)
	   ll_new_oper_no = idw_dw[1].GetItemNumber(ll_result, 'cost_alloc_operation_no')

	   INSERT INTO COST_ALLOC_CHANGE  
   	       (claim_no, event_no, old_cost_alloc_no, old_cost_alloc_operation_no,   
      	     new_cost_alloc_no, new_cost_alloc_operation_no)  
	   VALUES (:al_claim_no, :al_event_no, :ll_old_alloc, :ll_old_oper_no, :ll_new_alloc, :ll_new_oper_no);

	   ll_result =  SQLCA.nf_handle_error("Embedded SQL: Insert COST_ALLOC_CHANGE","","nf_log_cost_alloc_change for n_claims")
   	IF ll_result < 0 THEN
	   	Return ll_result
	   END IF

	   IF SQLCA.SQLNRows <> 1 Then
   	//?????
	   	SQLCA.nf_rollback_transaction()
	   	
   		Return -1
	   END IF
	ELSE
   	MessageBox("Error", "No data supplied to cost allocation log function.")
	   Return -1
	END IF

Return 0
end function

public function integer nf_log_claim_status_change (long al_claim_no, long al_event_no);LONG    ll_result, ll_seq_no
STRING  ls_old_status_code, ls_new_status_code, ls_old_type_code, ls_new_type_code

/*	now log the claim status change 
*/

	ll_result = idw_dw[1].GetRow()
	IF ll_result > 0 THEN
/*	the old and the new values must be saved
*/
  		ls_old_status_code = idw_dw[1].GetItemString(ll_result, 'claim_status_code', Primary!, TRUE)
	   ls_new_status_code = idw_dw[1].GetItemString(ll_result, 'claim_status_code')
   	ls_old_type_code = idw_dw[1].GetItemString(ll_result, 'claim_status_type_code', Primary!, TRUE)
	   ls_new_type_code = idw_dw[1].GetItemString(ll_result, 'claim_status_type_code')

/*		this is necessary for an insert
*/
   	IF ls_old_status_code = "" OR IsNull(ls_old_status_code) THEN
      	ls_old_status_code = ' '
	   END IF
   	IF ls_old_type_code = "" OR IsNull(ls_old_type_code) THEN
      	ls_old_type_code = ' '
	   END IF
   	IF ls_new_status_code = "" OR IsNull(ls_new_status_code) THEN
      	ls_new_status_code = ' '
	   END IF
   	IF ls_new_type_code = "" OR IsNull(ls_new_type_code) THEN
      	ls_new_type_code = ' '
	   END IF
   	IF idw_dw[1].GetItemStatus(ll_result,0, Primary!) = NewModified! THEN
/*		old values should be blank
*/
  	   	ls_old_status_code = ' '
	      ls_old_type_code = ' '
   	END IF

/*		get the next seq number for the claim number
*/
	  	SELECT Max(seq_no)
	     INTO :ll_seq_no
   	  FROM CLAIM_STATUS_CHANGE
	    WHERE claim_no = :al_claim_no;

		IF SQLCA.nf_handle_error('Embedded SQL: select from CLAIM_STATUS_CHANGE', 'n_claims', 'nf_log_claim_status') < 0 THEN
			Return -1
		END IF
	   IF ll_seq_no = 0 OR IsNull(ll_seq_no) THEN
   	   ll_seq_no = 1
	   ELSE
   	   ll_seq_no = ll_seq_no + 1
	   END IF

	   INSERT INTO CLAIM_STATUS_CHANGE  
   	      (claim_no, seq_no, event_no, old_claim_status_code, old_claim_status_type_code,   
      	    new_claim_status_code, new_claim_status_type_code)  
	   VALUES 
   	      (:al_claim_no,:ll_seq_no, :al_event_no, :ls_old_status_code, :ls_old_type_code, :ls_new_status_code, :ls_new_type_code);
   	
	   ll_result =  SQLCA.nf_handle_error("Embedded SQL: Insert CLAIM_STATUS_CHANGE","","nf_log_claim_status_change for n_claims")
   	IF ll_result < 0 THEN
	   	Return ll_result
	   END IF

	   IF SQLCA.SQLNRows <> 1 Then
   //?????
		   SQLCA.nf_rollback_transaction()
   		
   		Return -1
	   END IF
	ELSE
   // this error should not happen
   	MessageBox("Error","No data supplied")
	   Return -1
	END IF
Return 0
end function

public function integer nf_validate_current_filing (string as_curr_est_filed_payroll, string as_curr_act_filed_payroll);String ls_static_message

//	This function validates the current filed flags to determine coverage.
//	Returns 1 if employer is selected, 0 if not covered, but selected manually (response to message) and
//	-1 if employer is not selected.

ls_static_message = "Contact an Assessment Officer if this is the required Employer/Rate Group."
ls_static_message = ls_static_message + " Do you wish to continue with this employer?"

IF as_curr_est_filed_payroll = 'A' AND as_curr_act_filed_payroll = 'I' THEN
	Return 1
END IF

IF as_curr_est_filed_payroll = 'I' AND as_curr_act_filed_payroll = 'I' THEN
	IF MessageBox("Employer/Rate Group does not have Coverage","Employer did not file for this year. " + ls_static_message,Exclamation!,YesNo!) = 1 THEN
		Return 0
	ELSE
		Return -1
	END IF
END IF

IF as_curr_act_filed_payroll <> 'I' THEN
	IF MessageBox("Employer/Rate Group does not have Coverage","Employer has gone out of business this year. " + ls_static_message,Exclamation!,YesNo!) = 1 THEN
		Return 0
	ELSE
		Return -1
	END IF
END IF

IF as_curr_est_filed_payroll = 'Z' THEN
	IF MessageBox("Employer/Rate Group does not have Coverage","Employer filed a zero payroll for this year. " + ls_static_message,Exclamation!,YesNo!) = 1 THEN
		Return 0
	ELSE
		Return -1
	END IF
END IF

//	Just in case there might be some way through, assume that the employer is not covered (Valid coverage only if
//	employer has filed an estimated payroll > 0 but not an actual payroll)
IF MessageBox("Employer/Rate Group does not have Coverage",ls_static_message,Exclamation!,YesNo!) = 1 THEN
	Return 0
ELSE
	Return -1
END IF

end function

public function integer nf_validate_seasonal_coverage (date adt_accident_date, date adt_start_seasonal_coverage, date adt_end_seasonal_coverage);/*	This function checks to see if the accident date falls within the current seasonal coverage dates.
	Returns: 1 when accident is within seasonal coverage dates
				-1 when accident is outside of seasonal coverage dates 
*/

	IF adt_accident_date < adt_start_seasonal_coverage OR adt_accident_date > adt_end_seasonal_coverage THEN
		MessageBox("Accident occurred outside of Seasonal Coverage Dates","Please notify an Assessment Officer that the employer has a claim outside of the filed Seasonal Coverage dates.",Exclamation!)
		Return -1
	ELSE
		Return 0
	END IF

end function

public function string nf_get_cost_alloc_name (long al_cost_alloc_no);STRING	ls_string

	IF IsNull(al_cost_alloc_no) OR al_cost_alloc_no < 1 THEN
		ls_string = ''
	ELSE
	   SELECT employer_name
  		  INTO :ls_string
	     FROM EMPLOYER_NAME
  		 WHERE employer_no = :al_cost_alloc_no 
			AND employer_name_type_code = 'L';

		IF SQLCA.nf_handle_error('Embedded SQL: EMPLOYER_NAME', 'n_claims', 'nf_get_cost_alloc_name') < 0 THEN
			Return ''
		END IF
	END IF
Return ls_string
	

 

end function

public function string nf_get_oper_name (long al_cost_alloc_no, long al_operation_no);String ls_string

IF IsNull(al_cost_alloc_no) OR IsNull(al_operation_no) OR al_cost_alloc_no < 1 OR al_operation_no < 1 THEN
	ls_string = ''
ELSE
	SELECT operation_name 
	  INTO :ls_string
	  FROM OPERATION 
	 WHERE employer_no = :al_cost_alloc_no
		AND operation_no = :al_operation_no ;

	IF SQLCA.nf_handle_error('Embedded SQL: OPERATION', 'n_claims', 'nf_get_oper_name') < 0 THEN
		Return ''
	END IF
END IF

Return ls_string


end function

public function integer nf_validate_legislation (string as_legislation_code);LONG 	ll_count, ll_count2, ll_claim_no

	ll_claim_no = idw_dw[1].GetItemNumber(1,'claim_no')
	IF is_opening_type_code = 'PEN' OR is_opening_type_code = 'SV' OR is_opening_type_code = 'S1' OR is_opening_type_code = 'S2' THEN
		SELECT Count(*) 
		  INTO :ll_count
		  FROM Legislation_Combination
		 WHERE legislation_code = :as_legislation_code
		   AND opening_type_code = :is_opening_type_code;

		IF SQLCA.nf_handle_error('Embedded SQL: Legislation_Combination', 'n_claims', 'nf_check_bus_rule') < 0 THEN
			Return -1
		END IF
		IF ll_count < 1 THEN
  		   MessageBox('Error - Invalid legislation code', 'The legislation is incorrect for the opening type of ' + is_opening_type_code + '.  Please correct.')
     		Return -1
	   END IF 
	END IF

/*	validate the participants
*/
	SELECT Count(*)
	  INTO :ll_count
	  FROM CLAIM_PARTICIPANT,   
			 Claim_Role_Combination  
	 WHERE CLAIM_PARTICIPANT.claim_role_code = Claim_Role_Combination.claim_role_code 
		AND claim_no = :ll_claim_no
		AND legislation_code = :as_legislation_code
	 USING SQLCA ;
	IF SQLCA.nf_handle_error('Embedded SQL: Select from Participant,Claim_Role_Combo','n_claims','nf_validate_legislation') < 0 THEN
		Return -1
	END IF
	SELECT Count(*)
	  INTO :ll_count2
	  FROM CLAIM_PARTICIPANT  
	 WHERE claim_no = :ll_claim_no
	 USING SQLCA ;
	IF SQLCA.nf_handle_error('Embedded SQL: Select from Participant,Claim_Role_Combo','n_claims','nf_validate_legislation') < 0 THEN
		Return -1
	END IF

	IF ll_count <> ll_count2 THEN
		MessageBox('Invalid Legislation Code', 'The legislation code is not valid for the listed participants.')
		Return -1
	END IF

Return 0

end function

public function integer nf_determine_claim_status (string as_reason);/* "P10127 when a claim's status is changed to rejected AND the disallowed reason is any status
	except "no insurance coverage/Employernot covered", display the new popup window 
   w_identify_reports_used - if no SDD, or SDC documents exist for the 
	claim, then display a warning message and do NOT display the pop-up window.
	OTHER:
	(1) The change to the status cannot be saved until at least one document 
		 has been identified as used in the decision process.
	(2) If the claim's status has been set to rejected before do-not display the pop-up window
	*/
INTEGER   li_eligible_doc_count
LONG      li_rowcount,li_accept,li_counter,li_accept_count,li_count,ll_claim_no,ll_current_row,ll_no2
LONG      ll_sdd_count, ll_sdc_count
STRING    ls_reason,ls_option,ls_claim_status_type


/* set the instance variable to whatever it was changed to ones that are important are
	F,A and R
*/	
					
IF is_claim_status_change = 'R' THEN//for rejected
	ll_claim_no          = idw_dw[1].GetItemNumber(idw_dw[1].getrow(),'claim_no')
	ls_claim_status_type = idw_dw[1].GetItemstring(idw_dw[1].getrow(),'claim_status_type_code')
	//let them go through with this one
	IF as_reason = "12" OR ls_claim_status_type <> "07" THEN
		is_change_status = "ACCEPT"
		RETURN 1
	END IF
	
	ls_reason = as_reason			
	IF ls_reason <> "12" AND TRIM(ls_reason) <> "" THEN//"no insurance coverage/Employernot covered"
		ll_no2 = 0
						
		SELECT count(*)
		INTO   :ll_no2
		FROM   DOCUMENT_INDEX a
		WHERE  a.claim_no  = :ll_claim_no
		AND    a.type_code in ('SDD','SDC')
		USING SQLCA;			
		SQLCA.nf_handle_error('Embedded SQL: OPENING','n_claims','nf_check_bus_rule')
						  
		IF isnull(ll_no2) OR ll_no2 < 1 THEN
			MessageBox("Associated Document Information ","The following document types are not associated with this claim; 'SDD','SDC'")
			is_change_status = "ACCEPT"
			RETURN 1
		END IF
		

		/* claim accident date must be before June 1 1999
		*/
		IF date(idw_dw[1].GetItemDateTime(1,'accident_date')) < date("1999-06-01") THEN
			MessageBox("Accident Date","The Accident Date cannot be before '1999-06-01'")
			is_change_status = "ACCEPT"
			RETURN 1
		END IF
		
			
		/* there are some cases where the claim may have valid doc's but
			 no provider number therfore
		*/
		
		SELECT IsNull(count(a.docid),0)
		INTO   :ll_sdd_count
		FROM   DOCUMENT_INDEX a
		JOIN   PROVIDER       b ON a.service_provider_no = b.provider_no
		WHERE  b.provider_type_code           = 'M'
		AND    b.nbms_early_filing_bonus_flag = 'Y'
		AND    a.type_code                    = 'SDD'
		AND    a.claim_no                     = :ll_claim_no
		AND EXISTS ( SELECT *
						 FROM   Nbms_Fee_Schedule c
						 WHERE  c.provider_type_code     = b.provider_type_code
						 AND    c.provider_sub_type_code = b.provider_sub_type_code )
		USING SQLCA;				
		SQLCA.nf_handle_error('Embedded SQL: SELECT count(a.docid)','n_claims','nf_check_bus_rule')
		
		
		SELECT IsNull(count(a.docid),0)
		INTO   :ll_sdc_count
		FROM   DOCUMENT_INDEX a
		JOIN   PROVIDER       c ON a.service_provider_no = c.provider_no
		WHERE  c.provider_type_code            = 'M'
		AND    c.provider_sub_type_code        = '04'
		AND    c.chiro_early_filing_bonus_flag = 'Y'
		AND    a.type_code                     = 'SDC'
		AND    a.claim_no                      = :ll_claim_no
		USING SQLCA;				
		SQLCA.nf_handle_error('Embedded SQL: SELECT count(a.docid)','n_claims','nf_check_bus_rule')
		
		IF (ll_sdd_count + ll_sdc_count) < 1 THEN
			//we want to messagebox them but allow for a save.
			MessageBox('Associated Document Information','This claim does not have a document associated with a medical aid provider that will result in an NBMS or NBCA Reporting Fee.')
			is_change_status = "ACCEPT"
			RETURN 1
		END IF
		
		/*
		allow the window to open if there are documents for which there is no ELIGIBLE_REPORT_FEE_DOCUMENTS record for this claim
		*/
		
		SELECT COUNT(*)
		INTO   :li_eligible_doc_count
		FROM   DOCUMENT_INDEX a
		WHERE  a.claim_no  = :ll_claim_no
		AND    a.type_code IN ('SDD','SDC')
		AND NOT EXISTS ( SELECT *
		                 FROM   ELIGIBLE_REPORT_FEE_DOCUMENTS b
							  WHERE  b.claim_no = a.claim_no
							  AND    b.docid    = a.docid )
		USING SQLCA;
		SQLCA.nf_handle_error('n_claims','embedded SQL: SELECT COUNT(*) FROM DOCUMENT_INDEX...','nf_determine_claim_status')
		
		IF li_eligible_doc_count = 0 THEN
			RETURN 1
		END IF
		
		//popup the response window
		OPENWITHPARM(w_identify_reports_used,ll_claim_no)
								
			
		istr_report_fee_documents = message.powerobjectparm
						
		/* everything pretty much depends on our array, if it contains values then we can proceed else 
			its a cancel. - but once we have a loaded array we can use it just like a datawindow
			for our insert purpose which can now be done anywhere we specify - rather then in the respose
			window.
		*/
		IF upperbound(istr_report_fee_documents.docid[]) < 1 OR NOT ISVALID(istr_report_fee_documents) THEN
			//MessageBox("Status Change","The status change can only be saved if at least "+&
							//"~rone document has been used in the decision making process")
							 
			is_change_status = "ACCEPT"
			RETURN 1
		ELSE
			is_change_status = "SAVE"
		END IF
						
		/* now we need to check if they have clicked any of the check boxes off
		*/
		FOR li_counter = 1 TO upperbound(istr_report_fee_documents.docid[])
			li_accept = istr_report_fee_documents.accepted[li_counter]
			IF li_accept = 1 THEN
				li_accept_count ++
			END IF
		NEXT
						
		IF isnull(li_accept_count) OR li_accept_count <= 0 THEN
			MessageBox("Status Change","The status change can only be saved if at least "+&
												"~rone document has been used in the decision making process")
							 
			is_change_status = "CANCEL"
			RETURN 1
		END IF
		
	END IF
END IF

RETURN 1


end function

public function long nf_get_next_claim ();LONG ll_claim_no, ll_error

SQLCA.nf_begin_transaction()

UPDATE Last_Claim_No
SET last_claim_no = last_claim_no +1
USING SQLCA;

ll_error = inv_transobj.nf_handle_error("n_claim","nf_get_next_claim","Embedded SQL")

SELECT last_claim_no
INTO :ll_claim_no
FROM Last_Claim_No
USING SQLCA;

ll_error = inv_transobj.nf_handle_error("n_claim","nf_get_next_claim","Embedded SQL")

IF ll_error < 0 THEN
	SQLCA.nf_rollback_transaction()
	RETURN - 1
ELSE
	SQLCA.nf_commit_transaction()
END IF



RETURN ll_claim_no
end function

public function integer nf_status_changed (long al_datawindow);long ll_accident_employer_no,ll_current_row
String ls_claim_status_code,ls_government_flag

ll_current_row = idw_dw[1].getrow()
ll_accident_employer_no = idw_dw[1].getitemnumber(ll_current_row, "accident_employer_no")
ls_claim_status_code = idw_dw[1].getitemstring(ll_current_row,"claim_status_code")
Choose case ls_claim_status_code 
	/*PR 2156 Removed cases 'A','R' as the users only want message if Final*/
	case 'F'
		select government_flag
		into 	:ls_government_flag
		from	EMPLOYER
		where	employer_no = :ll_accident_employer_no
		using sqlca;
			
		If ls_government_flag = 'Y' Then
			messagebox('Information','Form 32 must be issued to government employer.',Information!)
		End If
End choose

Return 1
end function

public function boolean nf_rehab_plan_exists (long al_claim_no);//A claim is only allowed to be case managed if there is a 
//REHAB_GOAL RECORD.
LONG		ll_goals_count


Select Count(*) into :ll_goals_count
FROM REHAB_GOAL
WHERE claim_no = :al_claim_no
USING inv_transobj;

inv_transobj.nf_handle_error('n_claims','nf_allow_case_managed','Select count(*) from REHAB_GOAL')

If ll_goals_count > 0 THen
	RETURN True
End if

RETURN False
end function

public function long nf_get_next_identifier ();LONG  ll_claim_no, ll_row 

/*	get the claim number
	check first to make sure that the user did not enter one with the 
	override feature - if so return that number
*/

	ll_row = idw_dw[1].GetRow()
	ll_claim_no = idw_dw[1].GetItemNumber(ll_row,'claim_no')
	IF ll_claim_no = 0  OR IsNull(ll_claim_no) THEN
   	ll_claim_no = idw_dw[2].Retrieve()
	   IF ll_claim_no = 1 THEN
			ll_claim_no = nf_get_next_claim()
	   ELSE
		   MessageBox('ERROR', 'An error occured while generating claim number.~r~nPlease see system administrator.')
   		Return -1
	   END IF
	END IF
Return ll_claim_no
end function

public function integer nf_log_transfers_to_rdb ();Return 0
end function

public function integer nf_report_fee_doc_presave (string as_status);  
INT	    li_counter,li_rowcount,li_check_box,li_accepted_count,li_message,li_row
LONG		 ll_docid,ll_claim_no,ll_no,ll_no2,ll_current_row,ll_payment,ll_txn_no,ll_provider_no,ll_found
DATETIME  ldt_current,ldt_create_date,ldt_processing_date
STRING    ls_sub_type,ls_payment_type_code,ls_user_id,ls_admin_region,ls_txn_type,ls_check_status
DATASTORE l_ds_eligible
DECIMAL   ldec_total_payment
STRING    ls_comment

					
ldt_current = f_server_datetime()
					
/* When the claims status changes to an accepted status (CLAIM.claim_status_code = 'A'.'F')
	and there are documents flagged to receive the reporting fee payment
	(ELIGIBLE_REPORT_FEE_DOCUMENTS.reporting_fee_eligibility_code = "ELG" or "APP").
						
	- Set ELIGIBLE_REPORT_FEE_DOCUMENTS.REPORTING_FEE_ELIGIBILITY_CODE = "ccs"
*/

/* we have used our response window w_identify_reports_used to populate our structure
	istr_report_fee_documents which now basically incudes all of the rows from our
	datawindow - so the first step is to insert records into ELIGIBLE_REPORT_FEE_DOCUMENTS  
	will let the NVO handle any rollbacks now that we are placing the code in the NVO 
	and not on the response window.
*/

ll_claim_no = idw_dw[1].GetItemNumber(idw_dw[1].getrow(),'claim_no')
		
/* the initial check will be to see if the status that tey picked is the same as 
   what they were.
*/
SELECT new_claim_status_code
  INTO :ls_check_status
  FROM CLAIM_STATUS_CHANGE a
 WHERE a.claim_no = :ll_claim_no
   AND a.seq_no = (SELECT max(b.seq_no) FROM CLAIM_STATUS_CHANGE b WHERE a.claim_no = b.claim_no);
	
SQLCA.nf_handle_error('SELECT new_claim_status_code','n_claims','nf_report_fee_doc_presave') 
  		
IF ls_check_status = as_status THEN RETURN 1
	
IF as_status = 'R' THEN
	//if the change status = accept then we can let then go through with the save without doing anything
	IF is_change_status = "ACCEPT" OR is_change_status = "" THEN
		return 1
	END IF
	
	IF is_change_status <> "SAVE" THEN
		MessageBox("Status Change","The status change can only be saved if at least "+&
		           "~rone document has been used in the decision making process")
		RETURN -1
	END IF
	
	FOR li_counter = 1 TO upperbound(istr_report_fee_documents.docid[])
		IF istr_report_fee_documents.accepted[li_counter] = 1 THEN
				
			ll_docid       = istr_report_fee_documents.docid[li_counter]
			ll_claim_no    = istr_report_fee_documents.claim_no[li_counter]
			ll_provider_no = istr_report_fee_documents.provider_no[li_counter]
			ls_sub_type    = istr_report_fee_documents.provider_sub_type[li_counter]
			ls_comment     = istr_report_fee_documents.comment[li_counter]
			
			/* we need to do a find here to make sure we dont add it twice
			*/
			SELECT count(*) 
		     INTO :ll_no2
		     FROM ELIGIBLE_REPORT_FEE_DOCUMENTS a
		     WHERE a.docid           = :ll_docid;
							
			SQLCA.nf_handle_error('Embedded SQL: SELECT count(*) ','n_claims','nf_report_fee_doc_presave')
			
			IF ll_no2 > 0 THEN//we have a problem
				MessageBox("Status Change","The ELIGIBLE REPORT FEE DOCUMENTS table already "+&
							"~rcontains a document id of " + string(ll_docid))
				idw_dw[3].reset()
				RETURN -1
			END IF
											  
				li_row = idw_dw[3].insertrow(0)
				idw_dw[3].setitem(li_row,"docid",ll_docid)
				idw_dw[3].setitem(li_row,"claim_no",ll_claim_no)
				idw_dw[3].setitem(li_row,"type_code","DC")
				idw_dw[3].setitem(li_row,"generated_method_code","M")
				idw_dw[3].setitem(li_row,"approved_date",ldt_current)
				idw_dw[3].setitem(li_row,"approved_user_id",vgst_user_profile.user_id)
				idw_dw[3].setitem(li_row,"comment",ls_comment)
				idw_dw[3].setitem(li_row,"eligibility_code","APP")
				idw_dw[3].setitem(li_row,"recipient_no",ll_provider_no)
				idw_dw[3].setitem(li_row,"recipient_type_code","M")
				idw_dw[3].setitem(li_row,"recipient_sub_type_code",ls_sub_type)
					
				li_accepted_count ++
		END IF
	NEXT	
	
		//just one more check just to make sure
		IF isnull(li_accepted_count) OR li_accepted_count <= 0 THEN
			MessageBox("Status Change","The status change can only be saved if at least "+&
							"~rone document has been used in the decision making process")
			RETURN -1
		END IF
		RETURN 1
END IF
	
/* When the claims status changes ta an accepted status (CLAIM.claim_status_code = "A","F")
   and there are documents flagged to receive the reporting fee payment 
	(ELIGIBLE_REPORT_FEE_DOCUMENTS.reporting_fee_eligibility_code = "ELG" or "APP")
	- Set ELIGIBLE_REPORT_FEE_DOCUMENTS.reporting_fee_eligibility_code = "CCS"
*/
	
IF as_status = 'A' OR as_status = 'F' THEN
					
	ll_no = idw_dw[1].GetItemNumber(idw_dw[1].getrow(),'claim_no')
								
	SELECT count(*) 
	  INTO :ll_no2
	  FROM ELIGIBLE_REPORT_FEE_DOCUMENTS a
	 WHERE a.reporting_fee_eligibility_code IN( "ELG","APP")
	   AND claim_no              = :ll_no;
							
	SQLCA.nf_handle_error('Embedded SQL: SELECT count(*) ','n_claims','nf_report_fee_doc_presave') 
							
		IF ll_no2 > 0 THEN
			idw_dw[3].settransobject(sqlca)
			li_rowcount = idw_dw[3].retrieve(ll_no)
				
			SQLCA.nf_handle_error('li_rowcount = idw_dw[3].retrieve(ll_no)','n_claims','nf_report_fee_doc_presave') 
				
			IF li_rowcount > 0 THEN
				FOR li_counter = 1 to idw_dw[3].rowcount()
					IF idw_dw[3].getitemstring(li_counter,"eligibility_code") = "ELG" OR +&
						idw_dw[3].getitemstring(li_counter,"eligibility_code") = "APP" THEN
						
						idw_dw[3].setitem(li_counter,"eligibility_code","CCS")	
					END IF	
				NEXT
			END IF
	  	END IF
		  
	  l_ds_eligible = CREATE DATASTORE
	  l_ds_eligible.dataobject = "d_eligible_report_fee_retrieve"
	  l_ds_eligible.settransobject(sqlca)
	  li_rowcount = l_ds_eligible.retrieve(ll_no)
		  
	  				 
	 /*When the claims status changes to an accepted status (CLAIM.claim_status_code = 'A','F')
		and there are scheduled reporting fee payments for the claim that are accepted into a batch
		(ELIGIBLE_REPORT_FEE_DOCUMENTS.reporting_fee_eligibility_code = 'SCH').
		Display an error message and do not allow the status to be changed.
	 */
		
	  IF li_rowcount > 0 THEN
			ll_payment = 0
					
			SELECT count(*)
			  INTO :ll_payment 
			  FROM PAYMENT_DOCUMENT a, UNAPPLIED_CLAIM_TXN b , ELIGIBLE_REPORT_FEE_DOCUMENTS c
			 WHERE a.payment_no 							 = b.payment_no 
				AND b.batch_no 							 > 0 
				AND b.claim_no 							 = c.claim_no
				AND c.docid      						    = a.doc_id
				AND c.claim_no    						 = :ll_no
				AND c.reporting_fee_eligibility_code = "SCH";
				
			SQLCA.nf_handle_error('Embedded SQL: SELECT count(*) ','n_claims','nf_report_fee_doc_presave') 
					
			IF ll_payment > 0 THEN
				MessageBox("Status Change","The status change can only be saved if there are no reporting fee payments " +&
								"~raccepted into a payment batch.")
				RETURN -1
			END IF
	   END IF
END IF
		
/* end of changes for p10127
*/
RETURN 1
end function

public function integer nf_validate_cost_alloc (long al_cost_alloc_no, long al_cost_alloc_operation_no, string as_cost_alloc_type);// nf_validate_cost_alloc
//
// Description: Validates the employer/rate group to ensure that they are valid for cost allocation purposes. 
// Returns: 0 for success   -1 for failure
//
// as_cost_alloc_type: C - Claim, A - Accident, P - Pension

Long     ll_sold_to_employer_no, ll_accident_year
Long     ll_sold_to_operation_no
Integer  li_operation_no, li_rtn
String   ls_static_message, ls_employer_type_code, ls_operation_status_code, ls_operation_status_desc
String   ls_billing_cycle_code, ls_emp_type, ls_column
Datetime ldt_coverage_start_date, ldt_coverage_end_date, ldt_accident_date
Datetime ldt_operation_start_date, ldt_operation_inactive_date

ls_static_message = "Contact an Assessment Officer if this is the required Employer/Rate Group. Do you wish to continue with this employer?"

//	Retrieve the data required to ensure that the employer/rate group is active.
ldt_accident_date = idw_dw[1].GetItemDateTime(1, "accident_date")

IF al_cost_alloc_no = 6000 OR al_cost_alloc_no = 7000 THEN
   MessageBox("Invalid Cost Allocation Number", "This Cost Allocation Number is a Special Reserve Account.  Please " +&
				  "re-enter and try again.", Information!)
	RETURN -1
END IF

// For all employers:
// Cost allocation number and operation number must exist as a valid employer/operation
SELECT O.sold_to_employer_no,  
		 O.sold_to_operation_no, 
		 O.operation_no, 
		 E.employer_type_code, 
		 O.operation_status_code, 
		 OS.operation_status_desc, 
		 E.billing_cycle_code, 
		 O.operation_start_date, 
		 O.operation_inactive_date 
  INTO :ll_sold_to_employer_no, 
		 :ll_sold_to_operation_no, 
		 :li_operation_no, 
		 :ls_employer_type_code, 
		 :ls_operation_status_code, 
		 :ls_operation_status_desc, 
		 :ls_billing_cycle_code, 
		 :ldt_operation_start_date, 
		 :ldt_operation_inactive_date
  FROM OPERATION O, 
       EMPLOYER E, 
		 Operation_Status OS 		 
 WHERE O.employer_no = :al_cost_alloc_no 
	AND O.operation_no = :al_cost_alloc_operation_no 
	AND E.employer_no = O.employer_no 
	AND O.operation_status_code = OS.operation_status_code; 

li_rtn = SQLCA.nf_handle_error('Embedded SQL: OPERATION O, EMPLOYER E', 'n_claims', 'nf_validate_cost_alloc')
IF li_rtn < 0 THEN
	RETURN -1
ELSEIF li_rtn = 100 THEN
	MessageBox("Invalid Employer Group Combination", "The Employer / Group combination does not exist.  Please " +&
				  "re-enter and try again.", Information!)
	RETURN -1
END IF

// Cost allocation number (employer) cannot be an UNREGISTERED employer 
IF ls_employer_type_code = "U" THEN
	MessageBox("Invalid Employer Type", "Cost Allocation Numbers cannot be an Unregistered Employer.  Employer " +&
				  String(al_cost_alloc_no) + " is an Unregistered Employer type.  Pick a different employer who is " +&
				  "not Unregistered and try again.", Information!)
	RETURN -1
END IF

IF as_cost_alloc_type = 'C' THEN
	ls_emp_type = 'CLAIM'
	ls_column   = 'cost_alloc_no'
ELSEIF as_cost_alloc_type = 'A' THEN
	ls_emp_type = 'ACCIDENT'
	ls_column   = 'accident_employer_no'
END IF

// For Monthly Assessed employers:
// There will be no further validation of the employer until the EIS database is merged into the EMPLOYER database
// The application will display the operation status and if the operation is Inactive or Final, 
// give the user the option to use the employer/operation.
IF ls_employer_type_code = "A" AND ls_billing_cycle_code = "M" THEN
	IF ls_operation_status_code = "I" OR ls_operation_status_code = "F" THEN 
		li_rtn = MessageBox("Warning: Operation " + ls_operation_status_desc, "Operation " +&
									String(al_cost_alloc_operation_no) + " of " + ls_emp_type + " Employer " + String(al_cost_alloc_no) +&
									" is currently " + ls_operation_status_desc + ".~r~r" +&
									"Are you sure you want to continue using this Employer / Operation?", Question!, YesNo!, 2)
	
		IF li_rtn = 1 THEN
			RETURN 0
		ELSEIF li_rtn = 2 THEN
			RETURN -1
		END IF
	END IF
END IF

// For Annual Assessed employers
IF ls_employer_type_code = "A" AND ls_billing_cycle_code = "A" THEN
	// Claim accident date must be greater than or equal to the OPERATION start date
	IF ldt_operation_start_date > ldt_accident_date THEN
		MessageBox("Invalid Operation", "Accident Date must be after the operation's start Date.  The start date for " +&
					  "Employer " + String(al_cost_alloc_no) + " Operation " + String(al_cost_alloc_operation_no) +&
					  " is " + String(ldt_operation_start_date, "mmmm dd, yyyy") + " and the accident date for the " +&
					  "claimant is " + String(ldt_accident_date, "mmmm dd, yyyy"), Information!)
		RETURN -1
	END IF

	// If the Operation status is Inactive or Final (i.e. the inactive date is not null)
	IF ls_operation_status_code = "I" OR ls_operation_status_code = "F" THEN
		// Check to see if the Employer/Operation was sold
		IF ll_sold_to_employer_no > 0 OR ll_sold_to_operation_no > 0 THEN
			IF ldt_accident_date > ldt_operation_inactive_date OR IsNull(ldt_operation_inactive_date) = TRUE THEN
				MessageBox("Employer/Group Sold", "Employer " + String(al_cost_alloc_no) + " Operation " +&
							  String(al_cost_alloc_operation_no) + " was sold to " + " Employer " +&
							  String(ll_sold_to_employer_no) + " Operation " + String(ll_sold_to_operation_no) + ".~r~r" +&
							  "You must Cost allocate to Employer " + String(ll_sold_to_employer_no) + " Operation " +&
							  String(ll_sold_to_operation_no) + ".", Information!)
				RETURN -1
			END IF
		ELSE
			IF ldt_accident_date > ldt_operation_inactive_date THEN
				MessageBox(ls_emp_type + " Employer/Group Inactive", "Unable to Cost allocate against " + ls_emp_type + " Employer " +&
							  String(al_cost_alloc_no) + " Operation " + String(al_cost_alloc_operation_no) +&
							  " because the accident date came after the operation's inactive date.  " +&
							  "The accident date was " + String(ldt_accident_date, "mmmm dd, yyyy") + " and the " +&
							  ls_emp_type + " Employer/Group was inactive as of " + String(ldt_operation_inactive_date, "mmmm dd, yyyy"), Information!)
				idw_dw[1].SetFocus()
				idw_dw[1].SetColumn(ls_column)
				
				RETURN -1
			END IF
		END IF
	END IF
END IF

// Check to see if a Seasonal Operation record exists in the assessment year of the year of the accident
ll_accident_year = Year(Date(ldt_accident_date))

SELECT coverage_start_date, 
		 coverage_end_date 
  INTO :ldt_coverage_start_date, 
		 :ldt_coverage_end_date  
  FROM SEASONAL_COVERAGE 
 WHERE employer_no = :al_cost_alloc_no 
	AND operation_no = :li_operation_no
	AND assessment_year = :ll_accident_year ;

li_rtn = SQLCA.nf_handle_error("Embedded SQL: SEASONAL_COVERAGE", "n_claims", "nf_validate_cost_alloc")
IF li_rtn < 0 THEN
	RETURN -1 
ELSEIF li_rtn = 0 THEN
	// If one is found then the Claim accident date must be greater than or equal to the 
	// Seasonal Coverage starting date and less than or equal to the Seasonal Coverage ending date 
	IF ldt_accident_date < ldt_coverage_start_date OR ldt_accident_date > ldt_coverage_end_date THEN
		MessageBox("Seasonal " + ls_emp_type + " Employer/Group", "Unable to Cost allocate against " + ls_emp_type + " Employer " +&
					  String(al_cost_alloc_no) + " Operation " + String(al_cost_alloc_operation_no) +&
					  " because the accident date does not fall between the Seasonal coverage dates." +&
					  "The accident date was " + String(ldt_accident_date, "mmmm dd, yyyy") + ".  The " +&
					  "Employer/Group was/is covered between " + String(ldt_coverage_start_date, "mmmm dd, yyyy") +&
					  " and " + String(ldt_coverage_end_date, "mmmm dd, yyyy") + ".", Information!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn(ls_column)
		RETURN -1
	END IF
END IF

RETURN 0
end function

public function integer nf_cost_allocation_no_cutoff (date adt_cutoff_date, datetime adt_accident_date, ref long al_no, long al_old_cost_alloc, long al_new_cost_alloc, string as_number_type);/*
This function was created for PR 2525 when NB Liquor became assessed instead of self-insured (as of 01-April-2002)

	adt_cutoff = date at which cost alloc number changed
	al_old_cost_alloc = old number
	al_new_cost_alloc = new number
	as_number_type = cost alloc, accident employer, pension cost alloc
*/

Date ldt_prior_to_cutoff
Date ld_accident_date

ld_accident_date = Date(adt_accident_date)

ldt_prior_to_cutoff = RelativeDate(adt_cutoff_date, -1)

CHOOSE CASE al_no
	CASE al_old_cost_alloc
		IF ld_accident_date >= adt_cutoff_date THEN
			MessageBox('Employer number has changed', &
				'As of '+String(adt_cutoff_date,'dd-Mmmm-yyyy')+' this employer has changed its number.~nYou cannot enter this '+as_number_type+' number for accidents after '+String(ldt_prior_to_cutoff,'dd-Mmmm-yyyy')+'.~nUse '+as_number_type+' number '+String(al_new_cost_alloc)+' instead.')
			RETURN 1
		END IF
	CASE al_new_cost_alloc
		IF ld_accident_date < adt_cutoff_date THEN
			MessageBox('Employer number has changed', &
				'As of '+String(adt_cutoff_date,'dd-Mmmm-yyyy')+' this employer has changed its number.~nYou cannot enter this '+as_number_type+' number for accidents prior to '+String(adt_cutoff_date,'dd-Mmmm-yyyy')+'.~nUse '+as_number_type+' number '+String(al_old_cost_alloc)+' instead.')
			RETURN 1
		END IF
	CASE ELSE
		
END CHOOSE

RETURN 0

end function

public function integer nf_set_defaults ();LONG  ll_row

/*	this routine only executes on an insert
*/

	ll_row = idw_dw[1].GetRow()
	IF ll_row < 0 THEN
   	RETURN -1
	ELSE
/*	default all the fields for the claim table
	all claims are created with a status of pre-adjudication
*/
  		idw_dw[1].SetItem(ll_row, 'claim_status_code', 'P')
  		idw_dw[1].SetItem(ll_row, 'imaged_flag', 'Y')
		idw_dw[1].SetItem(ll_row, 'cost_alloc_no',0)
		idw_dw[1].SetItem(ll_row, 'cost_alloc_operation_no',0)
		idw_dw[1].SetItem(ll_row, 'accident_employer_no',0)
		idw_dw[1].SetItem(ll_row, 'accident_employer_operation_no',0)
	END IF
Return 0   
end function

public function integer nf_sum_days_hours (long al_claim_no);/* PR4229 - J. Hawker 2004/10/14
   This function is called when trying to reject a claim. If the sum
   of the payment amount, days, hours, quantity, tax, or non tax amount
	do not equal zero, check if there have been hours converted to days.
	This will cause the summing to be off in some cases - where there are
	txns that do not have ben calc records.   
*/

DECIMAL {2} ldec_days_hours, ldec_days, ldec_total

SELECT sum(a.net_days_lost + a.net_hours_lost/b.preacc_work_hours_per_day)
INTO   :ldec_days_hours
FROM   PAYMENT a,
       BENEFIT_CALCULATION b
WHERE  a.claim_no                   = b.claim_no
AND    a.benefit_calculation_no     = b.benefit_calculation_no
AND    a.claim_no                   = :al_claim_no
AND    b.preacc_work_hours_per_day <> 0
AND    a.processed_date IS NOT NULL
AND NOT (a.payment_type_code = "21" 
AND    a.payment_sub_type_code IN ("03","04","05","06","07","08"))
USING  SQLCA;

SQLCA.nf_handle_error('n_claims','nf_sum_days_hours','SELECT sum(a.paid_days_lost + a.paid_hours_lost/b.preacc_work_hours_per_day)')

IF IsNull(ldec_days_hours) THEN ldec_days_hours = 0.00

SELECT sum(a.net_days_lost)
INTO   :ldec_days
FROM   PAYMENT a 
WHERE  a.claim_no = :al_claim_no
AND NOT EXISTS(SELECT *
		         FROM   BENEFIT_CALCULATION b
					WHERE  a.benefit_calculation_no = b.benefit_calculation_no
					AND    a.claim_no               = b.claim_no)
AND    a.processed_date IS NOT NULL
AND    NOT (a.payment_type_code = "21" 
AND    a.payment_sub_type_code in ("03","04","05","06","07","08"))
USING SQLCA;

SQLCA.nf_handle_error('n_claims','nf_sum_days_hours','SELECT sum(a.paid_days_lost)')

IF IsNull(ldec_days) THEN ldec_days = 0.00

ldec_total = ldec_days_hours + ldec_days

IF ldec_total <> 0.00 THEN
	RETURN -1
END IF 

RETURN 0
end function

public function integer nf_status_change_to_rejected (long al_claim_no);/*
The status of a claim can be changed to Rejected if all of the following conditions apply:
1. There are no scheduled payment transactions  for the claim
2. There are no Periodic Awards scheduled for the future  for the claim
3. All of the processed payments (excluding the NBMS/NBCA Reporting Fee payments, 
                                                and any payments that were cost allocated to the All-Assessed Non-Allocated Claim Cost Account (employer 8000)  ) 
   for the claim sum to zero for all columns below:
	net_payment_amount
   net_days_lost 
   net_hours_lost 
   net_quantity 
   net_tax_amount 
   net_non_tax_amount 

** We will no longer use the PAYMENT.zeroed_flag to identify the payments 
   that have been fully adjusted to zero.
*/
LONG    ll_count
DECIMAL ldc_net_payment_amount,ldc_net_days_lost, ldc_net_hours_lost, ldc_net_quantity
DECIMAL ldc_net_tax_amount, ldc_non_tax_amount
DECIMAL ldc_net_non_8000_cost_amout

/* 1. BR 1.201  A claim must not be rejected if there are scheduled payment transactions for the claim, ( excluding scheduled payments
                      that will be cost allocated to the All-Assessed Non-Allocated Claim cost account (#8000)
					 – i.e. the claim’s cost allocation is the All-Assessed Non-Allocated Claim cost account (#8000)  )
*/

SELECT COUNT(*) 
INTO :ll_count 
FROM UNAPPLIED_CLAIM_TXN a
JOIN CLAIM c on a.claim_no = c.claim_no
WHERE a.claim_no = :al_claim_no 
AND c.cost_alloc_no <> 8000
USING SQLCA;
SQLCA.nf_handle_error('n_claims','nf_status_change_to_rejected','SELECT COUNT(*) INTO :ll_count...#1')

IF ll_count > 0 THEN 
	messagebox("Claim Status Error","There are scheduled payment transactions for this claim ")
	RETURN -1
END IF 

/* 2. There are no Periodic Awards scheduled for the future for the claim */
SELECT count(*) INTO :ll_count FROM  PERIODIC_AWARD b WHERE b.claim_no = :al_claim_no 
      AND EXISTS (SELECT * FROM PERIODIC_AWARD_CONTROL a
           WHERE a.award_type_code  = b.award_type_code
	          AND a.processed_date   IS NULL
             AND b.award_start_date <= a.period_from_date
             AND b.award_end_date   >= a.period_to_date)
USING SQLCA;
 
SQLCA.nf_handle_error('n_claims','nf_status_change_to_rejected','SELECT COUNT(*) INTO :ll_count...#2')

IF ll_count > 0 THEN 
	messagebox("Claim Status Error","There are Periodic Awards scheduled for the future for this claim")
	RETURN -1
END IF

/* 3. BR 1.200 All of the processed payments (excluding the NBMS/NBCA Reporting Fee payments

//  T012790: modified version of 1.200, .. we now exlude payments cost allocated to non assessed cost allocation account (8000)

1.200 The claim must not have a claim status of Rejected unless all of the following conditions are true:
•	All of the processed payments have been fully adjusted to zero for the net amounts (i.e dollar amount, days lost and hours lost)  excluding the following payments:
	o	NBMS payments
	o	NBCA Reporting fee payments
	o	Processed payments that have been cost allocated to the All-Assessed Non-Allocated Claim cost account (#8000)

	ie - ensure only payments NOT cost allocated to the reserve account 8000 (and also not NBMS or NBMC) are tallied to determine if the status change should be rejected.  
	in others words payments cost allocated to 8000 should be excluded so we need to select from the COST_OF_CLAIMS_ALLOCATED, 
	since that is how we get the cost_alloc_no
*/

SELECT isnull(sum(c.cost_amount),0)
INTO   :ldc_net_non_8000_cost_amout
FROM   COST_OF_CLAIMS_ALLOCATED c
JOIN    PAYMENT p on c.payment_no = p.payment_no
WHERE p.claim_no = :al_claim_no     
AND     p.processed_date IS NOT NULL
AND NOT (payment_type_code = "21" AND  payment_sub_type_code in ("03","04","05","06","07","08","13","14","15","16","17","18"))
AND    c.cost_alloc_no <> 8000 
USING SQLCA;	 

SQLCA.nf_handle_error('n_claims','nf_status_change_to_rejected','SELECT sum(*) INTO :ll_count...#3')


SELECT isnull(sum(net_days_lost),0),
            isnull(sum(net_hours_lost),0)
INTO :ldc_net_days_lost, 
         :ldc_net_hours_lost
FROM PAYMENT WHERE claim_no = :al_claim_no
AND processed_date IS NOT NULL
AND NOT (payment_type_code = "21" AND  payment_sub_type_code in ("03","04","05","06","07","08","13","14","15","16","17","18"))
USING SQLCA;

SQLCA.nf_handle_error('n_claims','nf_status_change_to_rejected','SELECT sum(*) INTO :ll_count...#4')

/* PR4229 - J.Hawker, 2004-10-14.
   IF the days and hours do not add up to zero, call nf_sum_days_hours to calculate in a different way.
	May be off slightly due to conversion of hours to days. 
*/
IF ldc_net_non_8000_cost_amout > 0  THEN
	MessageBox("Claim Status Error","There are processed Payments that do not sum to zero for this claim")
	RETURN -1
ELSEIF ldc_net_days_lost > 0 OR ldc_net_hours_lost > 0 THEN
	
/* PR4229 - J.Hawker, 2004-10-14.
   IF the days and hours do not add up to zero, call nf_sum_days_hours to calculate in a different way.
	May be off slightly due to conversion of hours to days. 
*/
	IF nf_sum_days_hours(al_claim_no) < 0 THEN
		messagebox("Claim Status Error","There are processed Payments that do not sum to zero for days lost and hours lost for this claim")
		RETURN -1
	END IF
END IF 

/* P10273 Annuities - A claim must not be rejected if there is active benefit entitlement associated with the claim.*/

Select count(*)
Into    :ll_count
From  BENEFIT_ENTITLEMENT
Where claim_no = :al_claim_no
And     active_flag = 'Y'
Using SQLCA;

SQLCA.nf_handle_error('n_claims','nf_status_change_to_rejected','Select count from BENEFIT_ENTITLEMENT')

IF ll_count > 0 THEN
	Messagebox('Claim Status Error','There is active benefit entitlement associated with this claim. It can not be rejected.')
	Return -1
END IF

RETURN 1
end function

public function long nf_set_identifiers ();LONG  ll_claim_no, ll_row

/*	this is used to get and set the claim number on the datawindow
*/

	ll_row = idw_dw[1].GetRow()

	ll_claim_no = nf_get_next_identifier()
	IF ll_claim_no > 0 THEN
		IF idw_dw[1].GetItemNumber(ll_row, 'claim_no') = 0 OR IsNull(idw_dw[1].GetItemNumber(ll_row, 'claim_no')) THEN
	   	idw_dw[1].SetItem(ll_row,'claim_no', ll_claim_no)
		END IF
	ELSE
   	Return -1
	END IF
Return 0
end function

public function string nf_get_status_descriptions (string as_code, string as_sub_code);STRING ls_status_desc, ls_type_desc, ls_desc

SELECT claim_status_desc 
INTO   :ls_status_desc
FROM   Claim_Status 
WHERE  claim_status_code = :as_code;

IF SQLCA.nf_handle_error('Embedded SQL: Claim_Status', 'n_claims', 'nf_get_status_descriptions') < 0 THEN
	RETURN ''
END IF

IF TRIM(as_sub_code) = '' THEN
	ls_type_desc = TRIM(as_sub_code)
ELSE	
	SELECT claim_status_type_desc 
	INTO   :ls_type_desc
	FROM   Claim_Status_Type 
	WHERE  claim_status_code      = :as_code
	AND    claim_status_type_code = :as_sub_code;

	IF SQLCA.nf_handle_error('Embedded SQL: Claim_Status_Type', 'n_claims', 'nf_get_status_descriptions') < 0 THEN
		RETURN ''
	END IF
	
	ls_type_desc = ' - ' + ls_type_desc 
END IF

ls_desc = ls_status_desc + ls_type_desc

RETURN ls_desc
	

			

end function

public function integer nf_set_unused_fields ();LONG  ll_row
STRING ls_default_classification_system_code, ls_default_classification_system_type_code
INT li_year

	ll_row = idw_dw[1].GetRow()

/*	default all the unused fields for the claim table
	these defaults are for a new claim
*/

/*
	P10262 - NAICS, capture and set default values for classification_system_code and type, where 
	the Classification_System.classification_system_desc = 'Not Applicable', for current year
*/
	li_year = YEAR(DATE(f_server_datetime()))
    SELECT a.classification_system_type_code, a.classification_system_code
	INTO :ls_default_classification_system_type_code, :ls_default_classification_system_code
    FROM Classification_System a, Classification_System_Type b
    WHERE a.classification_system_type_code = b.classification_system_type_code
    AND a.classification_system_desc = 'Not Applicable'
    AND :li_year between b.effective_from_year and b.effective_to_year
	USING SQLCA;
	
	SQLCA.nf_handle_error("n_claims","nf_set_unused_fields","Select from Classification_System")
	
	IF IsNull(idw_dw[1].GetItemString(ll_row, 'claim_status_type_code')) THEN
		idw_dw[1].SetItem(ll_row, 'claim_status_type_code', ' ')
	END IF
	IF IsNull(idw_dw[1].GetItemString(ll_row,'claim_manager_user_id')) THEN
   	idw_dw[1].SetItem(ll_row, 'claim_manager_user_id', ' ')
	END IF
	IF IsNull(idw_dw[1].GetItemString(ll_row,'rehab_officer_user_id')) THEN
   	idw_dw[1].SetItem(ll_row, 'rehab_officer_user_id', ' ')
	END IF
	IF IsNull(idw_dw[1].GetItemString(ll_row,'occupational_therapist_user_id')) THEN
   	idw_dw[1].SetItem(ll_row, 'occupational_therapist_user_id', ' ')
	END IF
	IF IsNull(idw_dw[1].GetItemString(ll_row,'comp_week_code')) THEN
   	idw_dw[1].SetItem(ll_row, 'comp_week_code', ' ')
	END IF
	IF IsNull(idw_dw[1].GetItemString(ll_row,'comp_day_code')) THEN
   	idw_dw[1].SetItem(ll_row, 'comp_day_code', ' ')
	END IF
	IF IsNull(idw_dw[1].GetItemNumber(ll_row,'duplicate_of_claim_no')) THEN
   	idw_dw[1].SetItem(ll_row, 'duplicate_of_claim_no', 0)
	END IF
	IF IsNull(idw_dw[1].GetItemString(ll_row, 'case_managed_flag')) OR idw_dw[1].GetItemString(ll_row, 'case_managed_flag') <> 'Y' THEN
   	idw_dw[1].SetItem(ll_row, 'case_managed_flag', 'N')
	END IF
	IF IsNull(idw_dw[1].GetItemString(ll_row,'receiving_salary_flag'))  OR idw_dw[1].GetItemString(ll_row,'receiving_salary_flag') <> 'Y' THEN
   	idw_dw[1].SetItem(ll_row, 'receiving_salary_flag', 'N')
	END IF
	IF IsNull(idw_dw[1].GetItemString(ll_row,'difficult_claim_flag')) OR idw_dw[1].GetItemString(ll_row,'difficult_claim_flag') <> 'Y' THEN
   	idw_dw[1].SetItem(ll_row, 'difficult_claim_flag', 'N')
	END IF
	IF IsNull(idw_dw[1].GetItemString(ll_row,'coding_complete_flag')) OR idw_dw[1].GetItemString(ll_row,'coding_complete_flag') <> 'Y' THEN
   	idw_dw[1].SetItem(ll_row, 'coding_complete_flag', 'N')
	END IF
	IF IsNull(idw_dw[1].GetItemString(ll_row,'accident_county_code')) THEN
   	idw_dw[1].SetItem(ll_row, 'accident_county_code', ' ')
	END IF
	IF IsNull(idw_dw[1].GetItemString(ll_row,'accident_residence_county_code')) THEN
   	idw_dw[1].SetItem(ll_row, 'accident_residence_county_code', ' ')
	END IF
	IF IsNull(idw_dw[1].GetItemNumber(ll_row,'accident_employer_no')) THEN
   	idw_dw[1].SetItem(ll_row, 'accident_employer_no', 0)
	END IF
	IF IsNull(idw_dw[1].GetItemNumber(ll_row,'accident_employer_operation_no')) THEN
	   idw_dw[1].SetItem(ll_row, 'accident_employer_operation_no', 0)
	END IF
	IF IsNull(idw_dw[1].GetItemString(ll_row,'employer_type_code')) THEN
   	idw_dw[1].SetItem(ll_row, 'employer_type_code', ' ')
	END IF
	IF IsNull(idw_dw[1].GetItemString(ll_row,'classification_system_code')) THEN
		idw_dw[1].SetItem(ll_row, 'classification_system_code', ls_default_classification_system_code)
	END IF
	IF IsNull(idw_dw[1].GetItemString(ll_row,'classification_system_type_code')) THEN
   		idw_dw[1].SetItem(ll_row, 'classification_system_type_code', ls_default_classification_system_type_code)
	END IF
	IF IsNull(idw_dw[1].GetItemNumber(ll_row,'years_of_service')) THEN
   	idw_dw[1].SetItem(ll_row, 'years_of_service', '0')
	END IF
	IF IsNull(idw_dw[1].GetItemNumber(ll_row,'months_of_service')) THEN
   	idw_dw[1].SetItem(ll_row, 'months_of_service', '0')
	END IF
	IF IsNull(idw_dw[1].GetItemString(ll_row,'disallow_reason_code')) THEN
   	idw_dw[1].SetItem(ll_row, 'disallow_reason_code', ' ')
	END IF
	IF IsNull(idw_dw[1].GetItemNumber(ll_row,'cost_alloc_no')) THEN
   	idw_dw[1].SetItem(ll_row, 'cost_alloc_no', 0)
	END IF
	IF IsNull(idw_dw[1].GetItemNumber(ll_row,'cost_alloc_operation_no')) THEN
   	idw_dw[1].SetItem(ll_row, 'cost_alloc_operation_no', 0)
	END IF
	IF IsNull(idw_dw[1].GetItemString(ll_row,'cost_alloc_type_code')) THEN
		idw_dw[1].SetItem(ll_row, 'cost_alloc_type_code', ' ')
	END IF
	IF IsNull(idw_dw[1].GetItemNumber(ll_row,'pen_cost_alloc_no')) THEN
		idw_dw[1].SetItem(ll_row, 'pen_cost_alloc_no',0)
	END IF
	IF IsNull(idw_dw[1].GetItemNumber(ll_row,'pen_cost_alloc_operation_no')) THEN
		idw_dw[1].SetItem(ll_row, 'pen_cost_alloc_operation_no',0)
	END IF
	IF IsNull(idw_dw[1].GetItemString(ll_row,'pen_cost_alloc_type_code')) THEN
		idw_dw[1].SetItem(ll_row, 'pen_cost_alloc_type_code', ' ')
	END IF
	IF IsNull(idw_dw[1].GetItemDecimal(ll_row,'percent_disability')) THEN
		idw_dw[1].SetItem(ll_row, 'percent_disability',0)
	END IF
	IF IsNull(idw_dw[1].GetItemDecimal(ll_row,'percent_change_disability')) THEN
		idw_dw[1].SetItem(ll_row, 'percent_change_disability',0)
	END IF
	IF IsNull(idw_dw[1].GetItemString(ll_row,'legislation_code')) THEN
		idw_dw[1].SetItem(ll_row, 'legislation_code',' ')
	END IF
	IF IsNull(idw_dw[1].GetItemString(ll_row,'imaged_flag')) OR idw_dw[1].GetItemString(ll_row,'imaged_flag') <> 'N'  THEN
   	idw_dw[1].SetItem(ll_row, 'imaged_flag' ,'Y')
	END IF

  	idw_dw[1].SetItem(ll_row, 'history_flag', 'N')

	IF IsNull(idw_dw[1].GetItemDecimal(ll_row,'percent_impaired')) THEN
		idw_dw[1].SetItem(ll_row, 'percent_impaired',0)
	END IF

Return 0  
end function

public function integer nf_check_mandatory ();LONG  ll_current_row

IF idw_dw[1].AcceptText() < 0 THEN Return -1
IF idw_dw[2].AcceptText() < 0 THEN Return -1


IF idw_dw[9].GetRow() > 0 THEN
	IF idw_dw[9].GetItemNumber(idw_dw[9].GetRow(),'accident_employer_no') = 0 THEN
		idw_dw[9].SetItem(1,'employer_type_code','')
	END IF
END IF


ll_current_row = idw_dw[1].GetRow()

IF IsNull(idw_dw[1].GetItemDateTime(ll_current_row,'accident_date')) THEN
  	MessageBox("Error","Missing accident date.  Please enter.")
   idw_dw[1].SetColumn('accident_date')
	idw_dw[1].SetFocus()
  	Return -1
END IF

IF IsNull(idw_dw[1].GetItemString(ll_current_row,'part_of_body_code')) OR Trim(idw_dw[1].GetItemString(ll_current_row,'part_of_body_code')) = '' THEN
  	MessageBox("Error","Missing part of body code.")
   idw_dw[1].SetColumn('part_of_body_code')
	idw_dw[1].SetFocus()
  	Return -1
END IF

IF IsNull(idw_dw[1].GetItemString(ll_current_row,'side_of_body_code')) OR Trim(idw_dw[1].GetItemString(ll_current_row,'side_of_body_code')) = '' THEN
  	MessageBox("Error","Missing side of body code.")
   idw_dw[1].SetColumn('side_of_body_code')
	idw_dw[1].SetFocus()
  	Return -1
END IF

IF IsNull(idw_dw[1].GetItemString(ll_current_row,'admin_region_code')) OR Trim(idw_dw[1].GetItemString(ll_current_row,'admin_region_code')) = '' THEN
  	MessageBox("Error","Missing admin region code.")
   idw_dw[1].SetColumn('admin_region_code')
	idw_dw[1].SetFocus()
  	Return -1
END IF

IF IsNull(idw_dw[1].GetItemString(ll_current_row,'claim_status_code')) OR Trim(idw_dw[1].GetItemString(ll_current_row,'claim_status_code')) = '' THEN
  	MessageBox("Missing Status code","The claim status code must be supplied.")
   idw_dw[1].SetColumn('status_code')
	idw_dw[1].SetFocus()
  	Return -1
END IF

IF idw_dw[1].GetItemString(ll_current_row,'claim_status_code') = 'A' OR &
  	idw_dw[1].GetItemString(ll_current_row,'claim_status_code') = 'F' OR &
	idw_dw[1].GetItemString(ll_current_row,'claim_status_code') = 'R' THEN
   IF IsNull(idw_dw[1].GetItemString(ll_current_row, 'claim_manager_user_id')) OR Trim(idw_dw[1].GetItemString(ll_current_row, 'claim_manager_user_id')) = '' THEN
  	   MessageBox('Missing Claim Manager', 'The claim manager must be specified for Active, Finalled or Rejected claims.')
		idw_dw[1].SetColumn('claim_manager_user_id')
		idw_dw[1].SetFocus()
     	Return -1
   END IF
END IF



IF idw_dw[1].GetItemString(ll_current_row,'claim_status_code') = 'R' AND idw_dw[1].GetItemString(ll_current_row,'claim_status_type_code') = '07' THEN
/*	must have a disallowed reason
*/		
	IF IsNull(idw_dw[1].GetItemString(ll_current_row,'disallow_reason_code')) OR Trim(idw_dw[1].GetItemString(ll_current_row,'disallow_reason_code')) = '' THEN
		MessageBox('Missing Disallow Reason', 'Rejected Claims of type Disallowed must have a reason specified.')
		idw_dw[1].SetColumn('disallow_reason_code')
		idw_dw[1].SetFocus()
		Return -1
	END IF
END IF

IF ib_rloe_opening THEN
/*	must have a comp day and week if no end date
*/
 	IF IsNull(idw_dw[1].GetItemString(ll_current_row, 'comp_day_code')) OR Trim(idw_dw[1].GetItemString(ll_current_row, 'comp_day_code')) = '' THEN
  	   MessageBox('Missing Comp Day', 'A comp day must be filled in for claims with openings still active.')
		idw_dw[9].SetColumn('comp_day_code')
		idw_dw[9].SetFocus()
     	Return -1
   END IF
  	IF IsNull(idw_dw[1].GetItemString(ll_current_row, 'comp_week_code')) OR Trim(idw_dw[1].GetItemString(ll_current_row, 'comp_week_code')) = '' THEN
     	MessageBox('Missing Comp Week', 'A comp week must be filled in for claims with openings still active.')
		idw_dw[9].SetColumn('comp_week_code')
		idw_dw[9].SetFocus()
      Return -1
  	END IF
END IF

IF idw_dw[1].GetItemDecimal(ll_current_row,'percent_disability') > 0 THEN
	IF IsNull(idw_dw[1].GetItemDateTime(ll_current_row,'disability_determination_date')) THEN
		MessageBox('Missing Date','The determination date must be supplied when a percent disability is entered.')
		idw_dw[10].SetColumn('disability_determination_date')
		idw_dw[10].SetFocus()
		Return -1
	END IF
END IF
IF NOT IsNull(idw_dw[1].GetItemDateTime(ll_current_row,'disability_determination_date')) THEN
	IF IsNull(idw_dw[1].GetItemDecimal(ll_current_row,'percent_disability')) OR idw_dw[1].GetItemDecimal(ll_current_row,'percent_disability') = 0 THEN
		MessageBox('Missing Percent','The percent disability  must be supplied when a determination date is entered.')
		idw_dw[10].SetColumn('percent_disability')
		idw_dw[10].SetFocus()
		Return -1
	END IF
END IF

IF (Trim(idw_dw[1].GetItemString(ll_current_row,'legislation_code')) = ''  OR IsNull(idw_dw[1].GetItemString(ll_current_row,'legislation_code'))) &
	AND Trim(idw_dw[1].GetItemString(ll_current_row,'legislation_code',Primary!, TRUE)) > '' THEN
	MessageBox('Missing Legislation Code','Once the legislation code is entered it cannot be removed.')
	idw_dw[10].SetColumn('legislation_code')
	idw_dw[10].SetFocus()
	Return -1
END IF

IF Trim(idw_dw[1].GetItemString(ll_current_row,'legislation_code')) > ''   &
	AND (Trim(idw_dw[1].GetItemString(ll_current_row,'legislation_code',Primary!, TRUE)) = '' OR IsNull(idw_dw[1].GetItemString(ll_current_row,'legislation_code',Primary!, TRUE))) THEN
/*		if orignal value was blank and now not blank must be Active or Finaled Finaled claim
*/
	IF NOT(idw_dw[1].GetItemString(ll_current_row,'claim_status_code') = 'A' OR &
  	(idw_dw[1].GetItemString(ll_current_row,'claim_status_code') = 'F' AND &
	idw_dw[1].GetItemString(ll_current_row,'claim_status_type_code') = '01') ) THEN
		MessageBox('Error - Legislation Code','The legislation can only be entered if the claim status is Active or Finalled/Finalled.')
		idw_dw[10].SetColumn('legislation_code')
		idw_dw[10].SetFocus()
		Return -1
	END IF
END IF

IF Trim(idw_dw[1].GetItemString(ll_current_row,'legislation_code')) = '' OR IsNull(idw_dw[1].GetItemString(ll_current_row,'legislation_code')) THEN
/*		blank - no pension can be entered, no disablity data can be entered
*/
	IF idw_dw[1].GetItemNumber(ll_current_row, 'pen_cost_alloc_no') > 0 OR idw_dw[1].GetItemNumber(ll_current_row, 'pen_cost_alloc_operation_no') > 0 THEN
		MessageBox('Missing Legislation Code', 'The legislation code must be filled in if pension cost allocation is entered.')
		idw_dw[10].SetColumn('legislation_code')
		idw_dw[10].SetFocus()
		Return -1
	END IF

ELSE
	IF IsNull(idw_dw[1].GetItemNumber(ll_current_row, 'pen_cost_alloc_no')) OR &
	   idw_dw[1].GetItemNumber(ll_current_row, 'pen_cost_alloc_no') = 0 OR &
		IsNull(idw_dw[1].GetItemNumber(ll_current_row, 'pen_cost_alloc_operation_no')) OR &
        idw_dw[1].GetItemNumber(ll_current_row, 'pen_cost_alloc_operation_no') = 0  THEN
		MessageBox('Missing Pension Data', 'The pension cost allocation data must be filled in if legislation code is entered.')
		idw_dw[10].SetFocus()
		Return -1
	END IF
END IF
Return 0
end function

public function integer nf_change_item (long al_datawindow);String   ls_string, ls_string2, ls_employer_type, ls_county, ls_accident_county
String   ls_claim_status_code, ls_claim_status_type_code, ls_administering_act_code
String   ls_new_cost_alloc_no, ls_new_cost_alloc_operation_no, ls_filter
Long     ll_no, ll_current_row, ll_no2, ll_accident_employer_no, ll_no_old, ll_count
Integer  li_rtn
Decimal  ldec_no, ldec_no2
Datetime ldtm_date, ldtm_review
DATE ldt_accident_date, ldt_date

INT		li_msgbox

LONG			ll_cost_alloc_no, ll_num_rows, ll_rtn, ll_rowcount
LONG			ll_new_cost_alloc_no
LONG			ll_old_cost_alloc_no
LONG			ll_cost_alloc_operation_no
LONG			ll_new_cost_alloc_operation_no
LONG			ll_old_cost_alloc_operation_no

LONG			ll_accident_employer_operation_no
LONG			ll_charge_to_ind_operation_no

LONG			ll_new_accident_employer_no
LONG			ll_old_accident_employer_no
LONG			ll_new_accident_employer_operation_no
LONG			ll_old_accident_employer_operation_no
LONG        ll_open, ll_claim_no, ll_return
DWObject    l_dwo
DATAWINDOWCHILD ldwc_child
String ls_cppd_status_code
string ls_test, ls_pob, ls_new_side_of_body_code, ls_new_side_of_body_description



/*	the datawindow which triggered the item changed event can be found in
	the argument - al_datawindow
	In the itemchanged event of the window this function is called passing
	the datawindow corresponding to the array passed in to the nf_init() 
	function
*/
	
	CHOOSE CASE al_datawindow
   	CASE 1
/*	   changes to the main claim datawindow
*/
		ll_current_row = idw_dw[1].GetRow()
		
	    	CHOOSE CASE idw_dw[1].GetColumnName()

			
			CASE 'claim_status_code'
				
			// note* that claim create doesn't check this field because its set to Pending, but if someday that changes the reference to idw_dw[9] will require a change because that 
			// datawindow doesn't exist.
/*				diplay a form32 messagebox if the claim status changes and it is beyond adjudication
*/
				ll_accident_employer_no = idw_dw[1].getitemnumber(ll_current_row, "accident_employer_no")
				ls_claim_status_code = idw_dw[1].gettext() 
				
				IF idw_dw[1].GetItemString(idw_dw[1].GetRow(), 'claim_status_code', Primary!, TRUE) = 'F' AND idw_dw[1].GetItemString(idw_dw[1].GetRow(), 'claim_status_type_code', Primary!, TRUE) = '17' THEN
					IF vgst_user_profile.maintain_3rd_party_status_flag <> 'Y' THEN
						MessageBox('Restricted', 'You do not have the ability to change the Claim Status from Finalled - Terminated Third Party.', information!)
						idw_dw[1].SetItem(idw_dw[1].GetRow(),'claim_status_code', 'F')
						idw_dw[1].SetItem(idw_dw[1].GetRow(),'claim_status_type_code', '17')
						Return 1
					END IF
				END IF
			
/*				retrieve the data into the status type list box
*/
				idw_dw[1].GetChild('claim_status_type_code', ldwc_child)
				ls_string = idw_dw[1].GetText()
				ldwc_child.SetFilter('claim_status_code = "' + ls_string + '"')
				ldwc_child.Filter()
				idw_dw[1].SetItem(idw_dw[1].GetRow(), 'claim_status_type_code', ' ')
				idw_dw[1].SetItem(idw_dw[1].GetRow(), 'disallow_reason_code', ' ')


				IF ls_string = 'A' THEN    // if it goes to active compute the day and week
/* 	         	now set the comp_day and comp_week - (need the claim number for this)
*/
					IF nf_check_accident_county(ls_claim_status_code,"") = 1 THEN 
						idw_dw[9].SetColumn('accident_county_code')
						RETURN 1
					END IF
					
					
					IF Mod(idw_dw[1].GetItemNumber(ll_current_row, 'claim_no'), 2) = 0 THEN
						idw_dw[1].SetItem(ll_current_row, 'comp_week_code', 'E')
					ELSE
						idw_dw[1].SetItem(ll_current_row, 'comp_week_code', 'O')
					END IF   

					CHOOSE CASE DayName(Date(idw_dw[9].GetItemDatetime(ll_current_row,'accident_date')))
						CASE 'Sunday', 'Monday'
							idw_dw[1].SetItem(ll_current_row,'comp_day_code', 'MON')
						CASE 'Tuesday'
							idw_dw[1].SetItem(ll_current_row,'comp_day_code', 'TUE')
						CASE 'Wednesday'
							idw_dw[1].SetItem(ll_current_row,'comp_day_code', 'WED')
						CASE 'Thursday'
							idw_dw[1].SetItem(ll_current_row,'comp_day_code', 'THU')
						CASE 'Friday', 'Saturday'
							idw_dw[1].SetItem(ll_current_row,'comp_day_code', 'FRI')
					END CHOOSE
				ELSE
					idw_dw[9].SetItem(ll_current_row,'accident_county_stat',"")
				END IF
					
						/* "P10127 when a claim's status is changed to rejected AND the disallowed reason is any status
							 except "no insurance coverage/Employernot covered", display the new popup window 
							 w_identify_reports_used - if no SDD or SDC documents exist for the 
							 claim, then display a warning message and do NOT display the pop-up window.
							 OTHER:
							 (1) The change to the status cannot be saved until at least one document 
								  has been identified as used in the decision process.
							 (2) If the claim's status has been set to rejected before do-not display the pop-up window
						*/
				is_claim_status_change = idw_dw[1].GetText()	

						/* end of vhanges for p10127
						*/
				
				
			CASE 'claim_status_type_code'
					
				IF idw_dw[1].GetText() = '07' THEN
/*					populate the drop down with disallowed reasons
*/				
					ls_administering_act_code = idw_dw[1].GetItemString(ll_current_row, 'administering_act_code')
					
					IF ls_administering_act_code = 'FCA' THEN 
						ls_filter = "fca_act_flag = 'Y'"
					ELSE 	
						ls_filter = "wca_act_flag = 'Y'"
					END IF
					idw_dw[1].GetChild('disallow_reason_code',ldwc_child)
					ldwc_child.SetFilter(ls_filter)  
					ldwc_child.Filter()
				ELSE
					idw_dw[1].GetChild('disallow_reason_code',ldwc_child)
					ldwc_child.SetFilter("disallow_reason_code = ' '")
					ldwc_child.Filter()
					idw_dw[1].SetItem(idw_dw[1].GetRow(), 'disallow_reason_code', ' ')
				END IF
				
				IF idw_dw[1].GetItemString(idw_dw[1].GetRow(), 'claim_status_type_code', Primary!, TRUE) = '17' THEN
					IF vgst_user_profile.maintain_3rd_party_status_flag <> 'Y' THEN
						MessageBox('Restricted', 'You do not have the ability to change the Claim Status Type from Terminated Third Party.', information!)
						idw_dw[1].SetItem(idw_dw[1].GetRow(),'claim_status_type_code', '17')
						Return 1
					END IF
				END IF
					
					/* for p10127
					*/
				IF idw_dw[1].GetText() <> '07' THEN	
					is_change_status = ""
				END IF
				
				//P10151-64 County Accident Change
				ls_claim_status_code = idw_dw[1].GetItemString(idw_dw[1].GetRow(), 'claim_status_code')
				ls_claim_status_type_code =  idw_dw[1].GetText()
				
	
				IF nf_check_accident_county(ls_claim_status_code,ls_claim_status_type_code) < 0 THEN 
					idw_dw[9].SetColumn('accident_county_code')
					idw_dw[9].SetFocus()
					RETURN 0
				END IF

				
			CASE 'claim_manager_user_id'

			CASE 'cost_alloc_no'
				ll_new_cost_alloc_no = Long(idw_dw[1].GetText())

				idw_dw[1].SetItem(ll_current_row,'cost_alloc_emp_name', nf_get_cost_alloc_name(ll_new_cost_alloc_no))
				
				/* PR4899 - J. Hawker, 2005-08-04 - When cost allocating and accident emp/operation are not entered, 
				   default to the same value as cost alloc.
				*/
				SELECT employer_type_code
				INTO   :ls_employer_type
				FROM   EMPLOYER
				WHERE  employer_no = :ll_new_cost_alloc_no
				USING  SQLCA;
				
				IF SQLCA.nf_handle_error('Embedded SQL: select from EMPLOYER','n_claims','nf_change_item') < 0 THEN
					RETURN -1
				END IF
					
				IF ls_employer_type = 'A' OR ls_employer_type = 'S' THEN
					IF idw_dw[1].GetItemNumber(ll_current_row,'accident_employer_no') = 0 OR IsNull(idw_dw[9].GetItemNumber(ll_current_row,'accident_employer_no')) THEN
						idw_dw[1].SetItem(ll_current_row,'accident_employer_no',ll_new_cost_alloc_no)
						idw_dw[1].SetItem(ll_current_row,'accident_emp_name', nf_get_cost_alloc_name(ll_new_accident_employer_no))
					END IF
				END IF
				
				ls_administering_act_code = idw_dw[1].GetItemString(ll_current_row, 'administering_act_code')
				IF ls_employer_type <> 'F' THEN
					IF ls_administering_act_code = 'FCA' THEN
						MessageBox('Cost Allocation Error','Injured Workers associated with the Firefighters Compensation Act must have a firefighter cost allocation.')	
						RETURN 1
					END IF
					
				ELSEIF ls_employer_type = 'F' THEN
					IF ls_administering_act_code = 'WCA' THEN
						MessageBox('Cost Allocation Error','Injured Workers associated with the Workers Compensation Act must not have a firefighter cost allocation.')	
						RETURN 1
					END IF
				END IF
				
				IF ll_new_cost_alloc_no = 4003 THEN
					// default the 'Charge to Industry' operation to be the same as the related NAICS operation
					
					ll_accident_employer_no           = idw_dw[1].getitemnumber(ll_current_row, 'accident_employer_no')
					ll_accident_employer_operation_no = idw_dw[1].getitemnumber(ll_current_row, 'accident_employer_operation_no')
					
					SELECT a.operation_no
					INTO   :ll_charge_to_ind_operation_no
					FROM  OPERATION_CLASSIFICATION a
					WHERE  employer_no  = 4003
					AND EXISTS ( SELECT *
					             FROM  OPERATION_CLASSIFICATION b
									 WHERE  b.classification_system_code = a.classification_system_code
									 AND    b.employer_no  = :ll_accident_employer_no
									 AND    b.operation_no = :ll_accident_employer_operation_no )
					USING SQLCA;
					SQLCA.nf_handle_error('n_claims','embedded SQL: SELECT operation_no FROM OPERATION_CLASSIFICATION...', 'nf_change_item')
					
					idw_dw[1].SetItem(ll_current_row,'cost_alloc_operation_no',ll_charge_to_ind_operation_no)
					idw_dw[1].SetItem(ll_current_row,'cost_alloc_oper_name', nf_get_oper_name(ll_new_cost_alloc_no,ll_charge_to_ind_operation_no))
				ELSEIF ll_new_cost_alloc_no = 0 THEN
					//  set emp type to blank
					idw_dw[1].SetItem(ll_current_row,'cost_alloc_type_code','')
				END IF
				
				ls_claim_status_code = idw_dw[1].GetItemString(idw_dw[1].GetRow(), 'claim_status_code')
				ls_claim_status_type_code = idw_dw[1].GetItemString(idw_dw[1].GetRow(), 'claim_status_type_code')
				IF ll_new_cost_alloc_no = 8000 and ls_claim_status_code <> 'P' and ls_claim_status_code <> 'J'  AND ( ls_claim_status_code = 'R' and ls_claim_status_type_code <> '07') AND ( ls_claim_status_code = 'R' and ls_claim_status_type_code <> '18') THEN
					MESSAGEBOX("Cost allocation error", "Cost Allocation # 8000 is only permitted for claims with status of Pre-Adjudication, Adjudication, Rejected - Disallowed, or Rejected - Insufficient Information" & 
					                                                        + "~r~n You will need to change the status accordingly in order to save your changes")
				   Return 1
				END IF
				
				IF ll_new_cost_alloc_no = 8000 then 
					idw_dw[1].setItem(idw_dw[1].GetRow(), 'cost_alloc_operation_no', 1)
					idw_dw[1].setItem(idw_dw[1].GetRow(), 'cost_alloc_oper_name', 'RESERVE ACCOUNT')
				END IF
			
			CASE 'firefighter_cost_alloc_no'
				ls_new_cost_alloc_no = idw_dw[1].GetText()
				ll_new_cost_alloc_no = Long(ls_new_cost_alloc_no)
				idw_dw[1].SetItem(ll_current_row,'cost_alloc_emp_name', nf_get_cost_alloc_name(ll_new_cost_alloc_no))
				nf_firefighter_cost_alloc_change(ll_current_row,ls_new_cost_alloc_no)
				
				SELECT TOP 1 operation_no
				INTO   :ll_new_cost_alloc_operation_no
				FROM   OPERATION
				WHERE  employer_no = :ls_new_cost_alloc_no
				USING SQLCA;
				SQLCA.nf_handle_error('n_claims', 'nf_change_item', 'select operation_no from OPERATION...')
				
				ls_new_cost_alloc_operation_no = String(ll_new_cost_alloc_operation_no)
				
				IF ll_new_cost_alloc_operation_no > 0 THEN
					l_dwo = idw_dw[1].Object.firefighter_cost_alloc_operation_no
					idw_dw[1].SetColumn('firefighter_cost_alloc_operation_no')
					idw_dw[1].SetText(ls_new_cost_alloc_operation_no)
					idw_dw[1].Trigger Event ItemChanged(ll_current_row,l_dwo,ls_new_cost_alloc_operation_no)
//					nf_firefighter_cost_alloc_oper_change(ll_current_row,String(ll_new_cost_alloc_operation_no))
				END IF
					

				
			CASE 'cost_alloc_operation_no'
				ll_cost_alloc_no 					 = idw_dw[1].GetItemNumber(ll_current_row,'cost_alloc_no')
				ll_new_cost_alloc_operation_no = Long(idw_dw[1].GetText())
				ll_old_cost_alloc_operation_no = idw_dw[1].GetItemNumber(ll_current_row, 'cost_alloc_operation_no', PRIMARY!, TRUE)
				
				IF ll_new_cost_alloc_operation_no <> 0 and ll_cost_alloc_no <> 0 THEN
					li_rtn = inv_claim_employer.nf_set_employer_parameters(ll_cost_alloc_no,ll_new_cost_alloc_operation_no)
					IF li_rtn <> 1 Then
						idw_dw[1].SetItem(ll_current_row,'cost_alloc_operation_no',ll_old_cost_alloc_operation_no)
						idw_dw[1].SetColumn('cost_alloc_operation_no')
						RETURN 1
					END IF
					
					li_rtn = inv_claim_employer.nf_isvalid_cost_alloc_employer()
					IF li_rtn <> 1 Then
						idw_dw[1].SetItem(ll_current_row,'cost_alloc_operation_no',ll_old_cost_alloc_operation_no)
						idw_dw[1].SetColumn('cost_alloc_operation_no')
						RETURN 1
					END IF
				END IF
				
				idw_dw[1].SetItem(ll_current_row, 'cost_alloc_oper_name', nf_get_oper_name(ll_cost_alloc_no,ll_new_cost_alloc_operation_no))					
				
				/* PR4899 - J. Hawker, 2005-08-04 - When cost allocating and accident emp/operation are not entered, 
				   default to the same value as cost alloc.
				*/
				IF inv_claim_employer.is_employer_type_code = 'A' OR inv_claim_employer.is_employer_type_code = 'S' THEN
					IF idw_dw[1].GetItemNumber(ll_current_row,'accident_employer_operation_no') = 0 OR IsNull(idw_dw[9].GetItemNumber(ll_current_row,'accident_employer_operation_no')) THEN
						IF idw_dw[1].GetItemNumber(ll_current_row,'accident_employer_no') = ll_cost_alloc_no THEN
							idw_dw[1].SetItem(ll_current_row, 'accident_employer_operation_no',ll_new_cost_alloc_operation_no)
							idw_dw[1].SetItem(ll_current_row,'accident_oper_name',nf_get_oper_name(ll_cost_alloc_no,ll_new_cost_alloc_operation_no))
							idw_dw[1].SetItem(ll_current_row,'employer_type_code',inv_claim_employer.is_employer_type_code)
							idw_dw[1].SetItem(ll_current_row,'classification_system_code',inv_claim_employer.is_classification_system_code)
							idw_dw[1].SetItem(ll_current_row,'classification_system_type_code',inv_claim_employer.is_classification_system_type_code)
						END IF
					END IF
				END IF
			
			CASE 'firefighter_cost_alloc_operation_no'
				ls_new_cost_alloc_operation_no = idw_dw[1].GetText()
				ll_new_cost_alloc_operation_no = Long(ls_new_cost_alloc_operation_no)
				ll_old_cost_alloc_operation_no = idw_dw[1].GetItemNumber(ll_current_row, 'cost_alloc_operation_no', PRIMARY!, TRUE)
				
				nf_firefighter_cost_alloc_oper_change(ll_current_row,ls_new_cost_alloc_operation_no)
				IF idw_dw[1].GetItemNumber(ll_current_row,'cost_alloc_operation_no') <> ll_new_cost_alloc_operation_no THEN
					// rejected cost_alloc_operation_no
					idw_dw[1].SetItem(ll_current_row,'firefighter_cost_alloc_operation_no',ll_old_cost_alloc_operation_no)
					idw_dw[1].SetColumn('firefighter_cost_alloc_operation_no')
					RETURN 1
				END IF
				
			CASE 'months_of_service'
				IF Integer(idw_dw[1].GetText()) > 11 THEN
					MessageBox('Error in Months of Service', 'The months of service must be less than 12.')
					idw_dw[1].SetFocus()
					idw_dw[1].SetColumn('months_of_service')
					Return 1
				END IF

			CASE 'years_of_service'
				IF Integer(idw_dw[1].GetText()) > 65 THEN
					MessageBox('Error in Years of Service', 'The years of service must be less than or equal to 65.')
					idw_dw[1].SetFocus()
					idw_dw[1].SetColumn('years_of_service')
					RETURN 1
				END IF

			CASE 'duplicate_of_claim_no'
				ll_no = Long(idw_dw[1].GetText())
				IF ll_no > 0 THEN
/*						warn the user if the individuals differ
*/
					SELECT individual_no
					INTO :ll_no
					FROM CLAIM
					WHERE claim_no = :ll_no
					USING SQLCA;

					IF SQLCA.nf_handle_error('Embedded SQL: select from CLAIM','n_claims','nf_change_item') < 0 THEN
						Return -1
					END IF
					IF ll_no <> idw_dw[1].GetItemNumber(ll_current_row,'individual_no') THEN
						MessageBox('Error','The claim you have entered as a duplicate is not for the same individual.  Please correct the claim number.')
						idw_dw[1].SetFocus()
						idw_dw[1].SetColumn('duplicate_of_claim_no')
						Return 1
					END IF
				END IF
					
			CASE 'disallow_reason_code'
				/* P10151-171 - some disallow reason codes are not active anymore, so raise a message box if they are no longer valid
				*/
				idw_dw[1].GetChild('disallow_reason_code', ldwc_child)
				ls_string = ldwc_child.getitemstring(ldwc_child.getrow(), 'active_flag')
				IF ls_string = 'N' then
					MessageBox('Error','The disallow reason code is no longer a valid code.')
					idw_dw[1].SetFocus()
					idw_dw[1].SetColumn('disallow_reason_code')
					Return 1
				END IF
				
					/* "P10127 when a claim's status is changed to rejected AND the disallowed reason is any status
					    except "no insurance coverage/Employernot covered", display the new popup window 
						 w_identify_reports_used - if no SDD or SDC documents exist for the 
						 claim, then display a warning message and do NOT display the pop-up window.
						 OTHER:
						 (1) The change to the status cannot be saved until at least one document 
						     has been identified as used in the decision process.
						 (2) If the claim's status has been set to rejected before do-not display the pop-up window
					*/
						
				   li_rtn = nf_determine_claim_status(idw_dw[1].GetText())	

			CASE 'annual_ben_review_due_date'
				ll_claim_no = idw_dw[1].GetItemNumber(idw_dw[1].GetRow(), 'claim_no')
				ldtm_review = DateTime(Date(Left(idw_dw[1].GetText(),10)))
/*				PR4529 - J. Hawker - Annual Review Date must be entered. The Annual Review Date 
   			must only be automatically set when an opening is first created. 
*/				
				IF IsNull(ldtm_review) OR DATE(ldtm_review) = DATE('1900/01/01') THEN
					SELECT Count(*)
					INTO   :ll_open
					FROM   OPENING
					WHERE  claim_no = :ll_claim_no
					AND    benefit_end_date is NULL
					USING  SQLCA;
					
					IF SQLCA.nf_handle_error('Embedded SQL: SELECT FROM OPENING','n_claims','nf_change_item') < 0 THEN
						Return -1
					END IF
					
					IF ll_open = 1 THEN
						IF NOT IsNull(idw_dw[1].GetItemDateTime(ll_current_row,'annual_ben_review_due_date', PRIMARY!, TRUE)) THEN
							MessageBox('Missing Review Date', 'The annual benefit review date must be entered.')
							idw_dw[1].SetFocus()
							idw_dw[1].SetColumn('annual_ben_review_due_date')
							RETURN 1
						END IF
					END IF
				END IF

				CASE 'cppd_status_code'
					//ls_cppd_status_code = idw_dw[1].GetItemString(ll_current_row, 'cppd_status_code')
					ls_string = idw_dw[1].GetText()
					ldtm_date = idw_dw[1].GetItemDateTime(ll_current_row,'cppd_benefit_start_date')
					ll_claim_no =  idw_dw[1].GetItemNumber(ll_current_row,'claim_no')
					
					IF ls_string <> 'R' AND NOT IsNull(ldtm_date) THEN
						SetNull(ldtm_date)
						idw_dw[1].SetItem(ll_current_row,'cppd_benefit_start_date',ldtm_date)
					END IF
		
			
				CASE 'cppd_benefit_start_date'
					ldtm_date = DateTime(Date(Left(idw_dw[1].GetText(),10)))
					ls_cppd_status_code = idw_dw[1].GetItemString(ll_current_row, 'cppd_status_code')
					
					IF Date(ldtm_date) = 1900-01-01 THEN
						SetNull(ldtm_date)
						idw_dw[1].SetItem(ll_current_row,'cppd_benefit_start_date',ldtm_date)
					END IF	
				
					// there must not be a start date if the claimaint is not receiving benefits.
				
					IF NOT Isnull(ldtm_date) AND  ls_cppd_status_code <> 'R' THEN
						MessageBox('CPP Disablity Start Date Error','A CPP Disability Start Date must not be entered if the claimant is not receiving CPP Disability benefits.',Exclamation!,Ok!)
//						idw_dw[1].SetFocus()
//						idw_dw[1].SetColumn('cppd_benefit_start_date')
						Return 1
					END IF
					
				
			END CHOOSE

CASE 9
/*	   changes to the accident stats datawindow
*/
		ll_current_row = idw_dw[9].GetRow()
		  
	    	CHOOSE CASE idw_dw[9].GetColumnName()

         	CASE 'accident_date'
/*					verify the date is less than today
*/
	            IF DateTime(Date(Left(idw_dw[9].GetText(),10))) > f_server_datetime() THEN
   	            			MessageBox("Invalid Accident Date", "The accident date cannot be beyond today.")
						RETURN 1
         	   END IF

/*					Accident date validations (Expanded for SR 10, Aug. 2001, EMcD.)
*/
					IF Date(Left(idw_dw[9].GetText(),10)) < Date('1900-01-01') THEN
   	           			 MessageBox( "Invalid Accident Date", "The accident date cannot be before 1900.", Exclamation! )
					  	RETURN 1

					ELSEIF Date(Left(idw_dw[9].GetText(),10)) > Date('2079-06-01') THEN
   	            				MessageBox( "Invalid Accident Date", "The accident date cannot be before 1900.", Exclamation! )
							RETURN 1

					ELSEIF Date(Left(idw_dw[9].GetText(),10)) < RelativeDate( Date( f_server_datetime()), -180 ) THEN
							li_msgbox = MessageBox( "Date Warning", "The accident date is more than 6 months in the past.",&
												Information! )

					END IF
					
					ll_no = idw_dw[1].GetItemNumber(ll_current_row,'claim_no')
					IF NOT IsNull(ll_no) THEN
						IF NOT IsNull(idw_dw[9].GetItemDateTime(ll_current_row,'accident_date')) THEN
/*							if the accident date change then check out the openings
*/
							ldtm_date = idw_dw[9].GetItemDateTime(ll_current_row,'accident_date', Primary!, TRUE)
							SELECT Count(*)
							  INTO :ll_no2
							  FROM OPENING
							 WHERE claim_no = :ll_no
								AND accident_recurrence_date = :ldtm_date;
				
							IF SQLCA.nf_handle_error('Embedded SQL: OPENING','n_claims','nf_check_bus_rule') < 0 THEN
								Return -1
							END IF	
							IF ll_no2 > 0 THEN
								MessageBox('Warning','There were openings created with the accident/recurrence date equal to the old accident date.')
							END IF
						END IF
					END IF
					
			CASE 'accident_county_code'
				ls_accident_county = idw_dw[9].GetText()
				ls_county = idw_dw[9].GetItemString(ll_current_row,"accident_county_stat")
				
				IF ls_accident_county = '99' and ls_county = "Inside" THEN //The accident county code can not be set to Outside NB when the user said it should be in NB
					Messagebox('Error','The accident county code must be a county in New Brunswick.',Exclamation!)
					Return 2
				ELSEIF ls_accident_county <> '99' and ls_county = "Outside"	THEN
						Messagebox('Error','The accident county code must be outside New Brunswick.',Exclamation!)
						Return 2
				END IF	
			
				
			CASE 'accident_employer_no'
						
				ll_new_accident_employer_no = Long(idw_dw[9].GetText())
				
				IF ll_new_accident_employer_no = 0 THEN
					//  set emp type to blank
					idw_dw[9].SetItem(ll_current_row,'employer_type_code','')
				END IF
				
				idw_dw[9].SetItem(ll_current_row,'accident_emp_name', nf_get_cost_alloc_name(ll_new_accident_employer_no))
				

			CASE 'accident_employer_operation_no'
						
				ll_accident_employer_no 					 = idw_dw[9].GetItemNumber(ll_current_row,'accident_employer_no')
				ll_new_accident_employer_operation_no = Long(idw_dw[9].GetText())
				ll_old_accident_employer_operation_no = idw_dw[9].GetItemNumber(ll_current_row, 'accident_employer_operation_no', PRIMARY!, TRUE)

				IF ll_new_accident_employer_operation_no <> 0 and ll_accident_employer_no <> 0 THEN
					li_rtn = inv_claim_employer.nf_set_employer_parameters(ll_accident_employer_no,ll_new_accident_employer_operation_no)
					IF li_rtn <> 1 Then
						RETURN 1
					END IF
					
					li_rtn = inv_claim_employer.nf_isvalid_accident_employer()
					IF li_rtn <> 1 Then
						RETURN 1
					END IF
				END IF
				
				idw_dw[9].SetItem(ll_current_row, 'accident_oper_name', nf_get_oper_name(ll_accident_employer_no,ll_new_accident_employer_operation_no))					
			
			CASE 'part_of_body_code'
				If idw_dw[9].GetItemString(ll_current_row,'coding_complete_flag') = 'Y' Then
					MessageBox('Change not allowed','You cannot change the part of body code because the accident statistics coding has already been completed. Please notify Health Records to change the coding and if the claim is Active, notify the Case Manager.',Information!,Ok!)
					RETURN 1
				end if
				
				ls_pob = idw_dw[9].GetText()
							
				//filter the side of body drop down.
				ll_rtn = idw_dw[9].GetChild("side_of_body_code",idwc_sob)
				ll_num_rows = idwc_sob.RowCount()
				
				idwc_sob.SetTransObject(SQLCA)
				ll_rtn = idwc_sob.Retrieve()
				IF SQLCA.nf_handle_error("n_claims","nf_change_item","idwc_sob.Retrieve()") < 0 THEN
					RETURN -1
				END IF
				ll_rowcount = idwc_sob.RowCount()
			
			   idwc_sob.SetFilter('part_of_body_code = "' + ls_pob + '" and active_flag = "Y"')
				idwc_sob.Filter()
				idwc_sob.SetSort("#2 A")
				idwc_sob.Sort()
				
				ll_rowcount = idwc_sob.RowCount()
				IF ll_rowcount = 1 THEN
					ls_new_side_of_body_code        = idwc_sob.GetItemString(1,'side_of_body_code')
					ls_new_side_of_body_description = idwc_sob.GetItemString(1,'side_of_body_desc_e')
					
					idw_dw[9].SetItem(1,'side_of_body_code',ls_new_side_of_body_code)
					idw_dw[9].SetItem(1,'side_of_body_description',ls_new_side_of_body_description)
				ELSE
					idw_dw[9].SetItem(1,'side_of_body_code','')
					idw_dw[9].SetItem(1,'side_of_body_description','')
				END IF

				
			CASE 'side_of_body_code'
				
				// Add a check to see if the side of body is unknown if so they can change the side of body.
				If idw_dw[9].GetItemString(ll_current_row,'coding_complete_flag') = 'Y' Then
					MessageBox('Change not allowed','You cannot change the side of body code because the accident statistics coding has already been completed. Please notify Health Records to change the coding and if the claim is Active, notify the Case Manager.',Information!,Ok!)
					RETURN 1
				end if

			CASE 'comp_day_code'
				ls_string = idw_dw[9].GetText()
				IF Trim(ls_string) <> '' THEN
					SELECT Count(*)
					INTO :ll_no
					FROM Comp_Day
					WHERE comp_day_code = :ls_string;
						
					IF ll_no < 1 THEN
						MessageBox('Invalid Comp Day', 'Invalid comp day code selected.')
						RETURN 1
					END IF           
				END IF

			CASE 'comp_week_code'
				ls_string = idw_dw[9].GetText()
				
				IF Trim(ls_string) <> '' THEN
					IF ls_string <> 'E' AND ls_string <> 'O' THEN
						MessageBox('Invalid Comp Week', 'Invalid comp week code selected.')
						RETURN 1
					END IF           
				END IF		
					
			END CHOOSE
				
   	CASE 10
/*	   changes to the main claim datawindow
*/
		ll_current_row = idw_dw[10].GetRow()
  
	    	CHOOSE CASE idw_dw[10].GetColumnName()

		CASE 'pen_cost_alloc_no'
// more rules???
				ll_no = Long(idw_dw[10].GetText())
				ldt_accident_date = Date(idw_dw[1].GetItemDateTime(ll_current_row,'accident_date'))
				IF nf_cost_allocation_no_cutoff( 2002-04-01 , DateTime(ldt_accident_date) , ll_no , 50001 , 603509 , 'pension cost allocation' ) = 1 THEN
					RETURN 1
				ELSE
					idw_dw[10].SetText(String(ll_no))
				END IF
				
				idw_dw[10].SetItem(ll_current_row,'pen_emp_name', nf_get_cost_alloc_name(ll_no))
				
			CASE 'pen_cost_alloc_operation_no'
				ll_no = Long(idw_dw[10].GetText())
				ll_no2 = idw_dw[10].GetItemNumber(ll_current_row, 'pen_cost_alloc_no')
				idw_dw[1].SetItem(ll_current_row,'pen_oper_name', nf_get_oper_name(ll_no2,ll_no))
			
			CASE 'percent_disability'
				ldec_no = idw_dw[10].GetItemDecimal(ll_current_row,'percent_disability',Primary!,TRUE)
				ldec_no2 = Dec(idw_dw[10].GetText())
				IF  ldec_no > 0 THEN
					IF  ldec_no2 <> ldec_no THEN
/*						if changed then need to store the percent change
*/
						idw_dw[10].SetItem(ll_current_row,'percent_change_disability',ldec_no2 - ldec_no)
					END IF
				ELSE
					IF ldec_no = 0 and ldec_no2 <> 0 THEN
						idw_dw[10].SetItem(ll_current_row, 'percent_change_disability', 0)
					END IF
				END IF

			CASE 'disability_determination_date'
				ldtm_date = DateTime(Date(Left(idw_dw[10].GetText(),10)))
				IF ldtm_date < idw_dw[9].GetItemDateTime(ll_current_row,'accident_date') THEN
					MessageBox('Invalid Determination Date', 'The determination date cannot be before the accident date.')
					RETURN 1
				END IF
				IF ldtm_date > f_server_datetime() THEN
					MessageBox("Invalid Determination Date", "The disability determination date cannot be beyond today.")
					RETURN 1
				END IF
				

			CASE 'legislation_code'
				/* PR4256 - J. Hawker, 2005.08.03
				*/
				ls_string = idw_dw[10].GetText()
				IF (ls_string = 'P82' OR ls_string = 'P48') AND Date(idw_dw[1].GetItemDateTime(ll_current_row, 'accident_date')) > Date('1981-12-31') THEN
					MessageBox('Legislation Code Warning', 'The accident date is after 1981 for a pre-1982 claim or a pre-1948 Silicosis claim.')
				END IF
				IF ls_string = 'P81' AND Date(idw_dw[1].GetItemDateTime(ll_current_row, 'accident_date')) < Date('1982-01-01') THEN
					MessageBox('Legislation Code Warning', 'The accident date is prior to 1982 for a post-1981 claim ')
				END IF
				IF Trim(idw_dw[10].GetItemString(ll_current_row, 'legislation_code',Primary!, TRUE)) <> "" THEN
					MessageBox('Legislation Warning','Changing the legislation code affects the recipient types in WCB when the data is transferred.')
					idw_dw[10].SetItem(ll_current_row,'pen_cost_alloc_no', 0)
					idw_dw[10].SetItem(ll_current_row,'pen_cost_alloc_operation_no', 0)
				END IF

				

				
			END CHOOSE	
				
END CHOOSE


Return 0
end function

public function integer nf_log_events ();N_EVENT_LOG  lnv_event_log
STRING       ls_status_change, ls_status_new 
STRING       ls_string1, ls_string2
STRING       ls_dash = ' - '
STRING       ls_status_type, ls_status_type_new, ls_desc, ls_desc_new, ls_desc_old
LONG         ll_event_no, ll_claim_no, ll_row

	lnv_event_log = Create n_event_log
/*	only call the function to get the event number once per update since it gets the max from the table
*/
	ll_event_no = 0
	ll_row = idw_dw[1].GetRow()
	ll_claim_no = idw_dw[1].GetItemNumber(ll_row,'claim_no')

/*	determine what event to log
*/
	IF idw_dw[1].GetItemStatus(ll_row,0,Primary!) = NewModified! THEN
/*		log the creation of the claim event
*/
  		ll_event_no = lnv_event_log.nf_next_claim_event_no(ll_claim_no)
	   IF ll_event_no > 0 THEN
   	   IF lnv_event_log.nf_create_auto_event(ll_claim_no, ll_event_no,'010',' ','CC') < 0 THEN Return -1
      	IF nf_log_claim_status_change(ll_claim_no, ll_event_no) < 0 THEN Return -1
	   ELSE
   	   Return ll_event_no
	   END IF
	ELSE 
/*		modificatins to existing claims
*/
  		IF idw_dw[1].GetItemString(ll_row,'claim_status_code',Primary!, TRUE) <> idw_dw[1].GetItemString(ll_row,'claim_status_code') OR &
		   Trim(idw_dw[1].GetItemString(ll_row,'claim_status_type_code',Primary!, TRUE)) <> Trim(idw_dw[1].GetItemString(ll_row,'claim_status_type_code')) THEN
/*			log the change in status code
			determine what it was and what type to log
*/
  	   	ls_status_change    = idw_dw[1].GetItemString(ll_row,'claim_status_code', Primary!, TRUE)
			ls_status_type      = idw_dw[1].GetItemString(ll_row,'claim_status_type_code', Primary!, TRUE)
			
	      IF ls_status_change = '' THEN
/*				ls_status_change should have a value and not go in here
*/
  	      	ls_status_change = ' '
	      END IF
			
			IF IsNull(ls_status_type) THEN ls_status_type = ''
			
/*			Get the description for the Old claim status and Old claim status type.
*/
			ls_desc_old = nf_get_status_descriptions(ls_status_change, ls_status_type)
		
			IF ls_desc_old = '' THEN
				MessageBox('Error','A problem was encountered while trying to retrieve the old status descriptions.', information!)
				RETURN -1
			END IF
			
			ls_status_new      = idw_dw[1].GetItemString(ll_row, 'claim_status_code')
			ls_status_type_new = idw_dw[1].GetItemString(ll_row, 'claim_status_type_code')
			
			IF IsNull(ls_status_type_new) THEN ls_status_type_new = ''
			
/*			Get the description for the New claim status and New claim status type.
*/			
			ls_desc_new = nf_get_status_descriptions(ls_status_new, ls_status_type_new)
		
			IF ls_desc_new = '' THEN
				MessageBox('Error','A problem was encountered while trying to retrieve the new status descriptions.', information!)
				RETURN -1
			END IF
			
			ls_desc = 'Claim Status changed from ' + ls_desc_old + ' to ' + ls_desc_new + '.'
			
	      ls_status_change = ls_status_change + ls_status_new			
			
			IF ll_event_no < 1 THEN
   		   ll_event_no = lnv_event_log.nf_next_claim_event_no(ll_claim_no)
			ELSE
				ll_event_no = ll_event_no + 1
			END IF

/*			the logic for these changes is as of May/96
*/
      	IF ll_event_no > 0 THEN
         	CHOOSE CASE ls_status_change
            	CASE 'PJ' 
               	IF lnv_event_log.nf_create_auto_event(ll_claim_no, ll_event_no,'010', ls_desc,'AR') < 0 THEN Return -1
	            CASE 'PR'
   	            IF lnv_event_log.nf_create_auto_event(ll_claim_no, ll_event_no,'010', ls_desc,'AB') < 0 THEN Return -1
      	      CASE 'JA', 'JF', 'JR','JJ'
         	      IF lnv_event_log.nf_create_auto_event(ll_claim_no, ll_event_no,'010', ls_desc,'AJ') < 0 THEN Return -1
            	CASE 'RJ', 'FJ'
	               IF lnv_event_log.nf_create_auto_event(ll_claim_no, ll_event_no,'010', ls_desc,'RX') < 0 THEN Return -1
   	         CASE 'AF', 'FF'
      	         IF lnv_event_log.nf_create_auto_event(ll_claim_no, ll_event_no,'010', ls_desc,'FI') < 0 THEN Return -1
					CASE 'RR'
      	         IF lnv_event_log.nf_create_auto_event(ll_claim_no, ll_event_no,'010', ls_desc,'RJ') < 0 THEN Return -1
					CASE ELSE
						//continue - don't log an event
	         END CHOOSE
	         IF nf_log_claim_status_change(ll_claim_no, ll_event_no) < 0 THEN Return -1
	      ELSE
   	      Return ll_event_no
      	END IF
		ELSEIF Trim(idw_dw[1].GetItemString(ll_row,'claim_status_type_code',Primary!, TRUE)) <> Trim(idw_dw[1].GetItemString(ll_row,'claim_status_type_code'))  THEN
/*			log the change in status code
*/
         IF nf_log_claim_status_change(ll_claim_no, 0) < 0 THEN Return -1
		END IF

/*		get a new event number for each thing logged
*/
  		IF idw_dw[1].GetItemNumber(ll_row,'cost_alloc_no',Primary!, TRUE) <> idw_dw[1].GetItemNumber(ll_row,'cost_alloc_no')  OR &
			idw_dw[1].GetItemNumber(ll_row,'cost_alloc_operation_no',Primary!, TRUE) <> idw_dw[1].GetItemNumber(ll_row,'cost_alloc_operation_no')THEN
/*			cost allocation change
*/
			IF ll_event_no < 1 THEN
   		   ll_event_no = lnv_event_log.nf_next_claim_event_no(ll_claim_no)
			ELSE
				ll_event_no = ll_event_no + 1
			END IF
   	   IF ll_event_no > 0 THEN
      	   IF lnv_event_log.nf_create_auto_event(ll_claim_no, ll_event_no,'014','','') < 0 THEN Return -1      
         	IF nf_log_cost_alloc_change(ll_claim_no, ll_event_no) < 0 THEN Return -1
	      ELSE
   	      Return ll_event_no
      	END IF
	   END IF

	   IF idw_dw[1].GetItemString(ll_row,'case_managed_flag',Primary!, TRUE) <> idw_dw[1].GetItemString(ll_row,'case_managed_flag') THEN
/*			case manager flag
*/
			IF ll_event_no < 1 THEN
   		   ll_event_no = lnv_event_log.nf_next_claim_event_no(ll_claim_no)
			ELSE
				ll_event_no = ll_event_no + 1
			END IF
      	IF ll_event_no > 0 THEN
         	IF idw_dw[1].GetItemString(ll_row,'case_managed_flag') = 'Y' THEN
         	   ls_string1 = 'Y'
            	ls_string2 = 'Case Managed'
	         ELSE
   	         ls_string1 = 'N'
      	      ls_string2 = 'Not Case Managed'
         	END IF
	         IF lnv_event_log.nf_create_auto_event(ll_claim_no, ll_event_no,'011',ls_string2,ls_string1) < 0 THEN Return -1      
   	   ELSE
      	   Return ll_event_no
	      END IF
   	END IF

	   IF idw_dw[1].GetItemDateTime(ll_row,'accident_date',Primary!, TRUE)<> idw_dw[1].GetItemDateTime(ll_row,'accident_date')THEN
/*			accident date
*/
			IF ll_event_no < 1 THEN
   		   ll_event_no = lnv_event_log.nf_next_claim_event_no(ll_claim_no)
			ELSE
				ll_event_no = ll_event_no + 1
			END IF
	      IF ll_event_no > 0 THEN
/*				get the dates to add to the comments
*/
	         ls_string1 = String(idw_dw[1].GetItemDateTime(ll_row,'accident_date', Primary!, TRUE), 'yyyy-mm-dd')
   	      ls_string2 = String(idw_dw[1].GetItemDateTime(ll_row,'accident_date'), 'yyyy-mm-dd')
      	   IF lnv_event_log.nf_create_auto_event(ll_claim_no, ll_event_no,'013','The previous accident date was: ' + ls_string1 + ' and it has been changed to ' + ls_string2 ,'') < 0 THEN Return -1      
	      ELSE
   	      Return ll_event_no
	      END IF
   	END IF

	   IF idw_dw[1].GetItemString(ll_row,'admin_region_code',Primary!, TRUE) <> idw_dw[1].GetItemString(ll_row,'admin_region_code') THEN
			IF ll_event_no < 1 THEN
   		   ll_event_no = lnv_event_log.nf_next_claim_event_no(ll_claim_no)
			ELSE
				ll_event_no = ll_event_no + 1
			END IF
   	   IF ll_event_no > 0 THEN
/*				determine the comments and the event specific code
*/
	         CHOOSE CASE idw_dw[1].GetItemString(ll_row, 'admin_region_code')
            	CASE 'SW'
         	      ls_string1 = 'SW'
      	      CASE 'SE'
   	            ls_string1 = 'SE'
	            CASE 'NE'
               	ls_string1 = 'NE'
            	CASE 'NW'
         	      ls_string1 = 'NW'
      	      CASE 'PRV'
   	            ls_string1 = 'PRV'
	            CASE ELSE
            	   MessageBox('Error', 'Invalid admin region code.')
						Return -1
      	   END CHOOSE
   	      IF lnv_event_log.nf_create_auto_event(ll_claim_no, ll_event_no,'012', 'Changed from ' + idw_dw[1].GetItemString(ll_row,'admin_region_code',Primary!, TRUE) , ls_string1) < 0 THEN Return -1      
	      ELSE
         	Return ll_event_no
      	END IF
   	END IF
	END IF

	nf_log_transfers_to_rdb()
Return 0
end function

public function integer nf_check_bus_rule ();Long     ll_no, ll_count, ll_current_row, ll_no2, ll_no3, ll_claim_no, ll_no4, ll_row
Long     ll_sold_to_employer_no, ll_sold_to_operation_no, ll_temp, ll_no_old, ll_return, ll_pmt
Integer  li_rtn
String   ls_string, ls_string2, ls_employer_type_desc, ls_employer_type_code
string	ls_claim_manager_user_id, ls_stats_coded,ls_pob, ls_sob
STRING	ls_case_managed_flag, ls_administering_act_code, ls_active_flag
STRING	ls_job_position_code, ls_cppd_status_code
Datetime ldtm_date, ldtm_today, ldtm_create_date
DateTime	ldt_accident_date, ldtm_date_old
Decimal  ldec_percent, ldec_percent_old
Date 		ldt_cppd_start, ldt_date, ldt_today
DATAWINDOWCHILD ldwc_child
DWItemStatus    ldwis_cost_alloc_no, ldwis_cost_alloc_op_no

STRING			ls_claim_status_code, ls_claim_status_type_code, ls_cost_alloc_type_code
LONG				ll_individual_no, ll_count_sob
LONG				ll_cost_alloc_no, ll_cost_alloc_operation_no
LONG				ll_accident_employer_no, ll_accident_employer_operation_no

	ldtm_today = f_server_datetime()
	ldt_today = Date(ldtm_today)
	ll_current_row = idw_dw[1].GetRow()


// Check the shared datacolumns between claim create and maintain claim.
IF nf_check_bus_rule_shared() < 0 THEN
	Return -1
END IF	

ll_claim_no = idw_dw[1].GetItemNumber(ll_current_row,'claim_no')

/*	disallow reason code
*/
	ls_string = idw_dw[1].GetItemString(ll_current_row,'disallow_reason_code')
	IF Trim(ls_string) <> '' AND NOT IsNull(ls_string) THEN
		IF idw_dw[1].GetChild('disallow_reason_code',ldwc_child) < 1 THEN
			MessageBox('Error Retrieveing List', 'Unable to retrieve list of valid disallow reason codes.  Contact help desk.')
			Return -1
		END IF
		IF ldwc_child.Find('disallow_reason_code = "' + ls_string +  '"',0, ldwc_child.RowCount()) < 1 THEN
			MessageBox('Error - Disallow Reason','Invalid disallow reason code.  Please correct.')
			idw_dw[1].SetColumn('disallow_reason_code')
			idw_dw[1].SetFocus()
	      Return -1
		END IF
		
		/* P10151-171 - some disallow reason codes are not active anymore, so raise a message box and reject their choice
             if they are no longer valid, but only if the item has been changed. Although disallow reason code is checked in nf_change_item, 
		   we could potentially get this far with an invalid code if user changed it, plus made another claim change to enable the save button
		*/
		
		ls_active_flag = ldwc_child.getitemstring(ldwc_child.getrow(), 'active_flag')
		IF ls_active_flag = 'N' AND idw_dw[1].GetItemStatus(ll_current_row,'disallow_reason_code', Primary!) <>  NotModified! then
			MessageBox('Error','The disallow reason code is no longer a valid code.')
			idw_dw[1].SetFocus()
			idw_dw[1].SetColumn('disallow_reason_code')
			Return -1
		END IF
	END IF

/*	accident date
*/

	IF idw_dw[9].GetItemDateTime(ll_current_row,'accident_date') > ldtm_today THEN
   	MessageBox("Invalid Accident Date", "The accident date cannot be beyond today.")
	   idw_dw[9].SetColumn('accident_date')
		idw_dw[9].SetFocus()
		Return -1
	END IF
	IF Date(idw_dw[9].GetItemDateTime(ll_current_row,'accident_date')) < Date('19000101') THEN
   	MessageBox("Invalid Accident Date", "The accident date cannot be before 1900.")
	   idw_dw[9].SetColumn('accident_date')
		idw_dw[9].SetFocus()
   	Return -1
	END IF
	IF ll_claim_no <> 0 THEN
		IF NOT IsNull(idtm_min_accident_recurrence) THEN
			IF idw_dw[9].GetItemDateTime(ll_current_row,'accident_date') > idtm_min_accident_recurrence THEN
		   	MessageBox("Invalid Accident Date", "The accident date cannot be after the minimum accident recurrence date of any opening.")
	   		idw_dw[9].SetColumn('accident_date')
				idw_dw[9].SetFocus()
		   	Return -1
			END IF
		END IF
	END IF
	
	
	/* PR4282 - J. Hawker 2004/10/18 - Can't enter accident date later than create_date
	   PR4846 - J. Hawker 2005/06/09 - BR Revised - Accident date SHOULD not be later than the create date
	*/
	ldtm_create_date = idw_dw[1].getitemdatetime(ll_current_row,'create_date')
		
	IF idw_dw[9].GetItemDateTime(ll_current_row,'accident_date') > ldtm_create_date THEN
   	IF MessageBox("Invalid Accident Date", 'The accident date is greater than the create date of the claim.' &
		   + '~r~nWould you like to continue?',question!,yesno!) = 2 THEN
	   	idw_dw[9].SetColumn('accident_date')
			idw_dw[9].SetFocus()
			Return -1
		END IF
	END IF	
	
/* part of body
	Get the status of part_of_body_code and only validate if it has been modified and if stats have not been coded.
*/	
	ls_claim_status_code = idw_dw[1].GetItemString(ll_current_row,'claim_status_code')
	ls_stats_coded       = idw_dw[1].GetItemString(ll_current_row,'coding_complete_flag')
	
//	IF ls_claim_status_code = 'A' OR ls_claim_status_code = 'F' THEN
//		
//		ls_pob = idw_dw[9].GetItemString(ll_current_row,'part_of_body_code',Primary!,TRUE)
//		ls_string = idw_dw[9].GetItemString(ll_current_row,'part_of_body_code', Primary!,FALSE)
//		IF ls_string <> ls_pob THEN
//			IF idw_dw[9].GetChild('part_of_body_code',ldwc_child) < 1 THEN
//				MessageBox('Error Retrieving List', 'Unable to retrieve list of valid part of body codes.  Contact help desk.')
//				Return -1
//			END IF
//			IF ldwc_child.Find('part_of_body_code = "' + ls_string +  '"',0, ldwc_child.RowCount()) < 1 THEN
//				MessageBox('Error - Part of Body','Invalid part of body code.  Please correct.')
//				idw_dw[9].SetColumn('part_of_body_code')
//				idw_dw[9].SetFocus()
//				Return -1
//			END IF
//		ELSE
//			// 2.75 A valid part of body and side of body combination must be selected.			
//			ls_sob = idw_dw[9].GetItemString(ll_current_row,'side_of_body_code')
//			
//			SELECT Count(*)
//			INTO   :ll_count_sob
//			FROM   Part_Of_Body_xref_Side_Of_Body
//			WHERE  part_of_body_code = :ls_pob
//			AND    side_of_body_code = :ls_sob
//			AND    active_flag = 'Y'
//			USING SQLCA;
//			SQLCA.nf_handle_error('n_create_claim','nf_check_bus_rule','Select Count(*)')
//			
//			IF ll_count_sob < 1 THEN 
//				IF ls_stats_coded = 'N' THEN
//					MessageBox('Part of Body/Side of Body Error','The Part of Body and Side of Body combination are invalid. Please select a new combination.',Exclamation!)
//				ELSE
//					// fully coded
//					MessageBox('Part of Body/Side of Body Error','The Part of Body and Side of Body combination are invalid.' &
//					                                   + '~r~n~r~nThis claim is "fully coded", so please correct this combination in Accident Statistics Maintenance.',Exclamation!)						
//				END IF
//				Return -1
//			END IF
//		END IF		
//	END IF

/*	claim manager
*/
	ls_claim_manager_user_id = idw_dw[1].GetItemString(ll_current_row, 'claim_manager_user_id')
	ls_case_managed_flag = idw_dw[1].GetItemString(ll_current_row,'case_managed_flag')
	IF ls_claim_manager_user_id <> '' THEN
		//Get a reference to the child datawindow
		IF idw_dw[1].GetChild('claim_manager_user_id',ldwc_child) < 1 THEN
			MessageBox('Error Retrieveing List', 'Unable to retrieve list of valid claim managers.  Contact help desk.')
			Return -1
		END IF
		
		ll_row = ldwc_child.Find('user_id = "' + ls_claim_manager_user_id + '"', 1, ldwc_child.RowCount())
		//If we can't find the user_id in the list then warn the user.
		If ll_row =0 Then 
			MessageBox('Error - Claim manager','User is no longer a claim manager.  Please select a valid claim manager.')
			idw_dw[1].SetColumn('claim_manager_user_id')
			RETURN -1
		Elseif ll_row < 0 Then
			SignalError(-666,'Error finding user id in claim manager drop down ')
		End if
		
		ls_job_position_code = ldwc_child.GetItemString(ll_row,'job_position_code')
		
		//IF its case managed then make sure the user selected is a case manager
		If ls_case_managed_flag = 'Y' and ls_job_position_code <> 'CASMGR' Then
			MessageBox('Error - Case Managed','You have marked this claim as "Case Managed" but the claim manager is not a Case Manager.  This is not allowed. ~r~nChange one of the values and try again.  The save has been canceled')
			idw_dw[1].SetColumn('case_managed_flag')
			RETURN -1
			
		ElseIF ls_job_position_code = 'CASMGR' and ls_case_managed_flag = 'N' Then
			IF idw_dw[1].GetItemString(ll_current_row,'case_managed_flag', Primary!, TRUE) = idw_dw[1].GetItemString(ll_current_row,'case_managed_flag') AND &
				idw_dw[1].GetItemString(ll_current_row,'claim_manager_user_id') <> idw_dw[1].GetItemString(ll_current_row,'claim_manager_user_id', Primary!, TRUE) THEN
				IF MessageBox('Warning', 'The Case Managed Flag is set to No on this claim and a Case Manager is assigned.  Do you want to stop the save so you can make changes?', Question!, YesNo!) = 1 THEN
					idw_dw[1].SetColumn('case_managed_flag')
					RETURN -1
				END IF
			END IF
		End if
	Else
		//Claim manager is empty
		IF ls_case_managed_flag = 'Y' Then
			MessageBox('Error - Case Mananged','In order for a claim to become case managed it must have a Case Manager assined to it.')
			idw_dw[1].SetColumn('claim_manager_user_id')
			RETURN -1
		End if
	End if

/* case_managed_flag
*/

	IF	idw_dw[1].GetItemString(ll_current_row,'case_managed_flag') = 'Y'  Then
		If Not This.nf_rehab_plan_exists(ll_claim_no) Then
			MessageBox('Error - Case Managed','A Rehab Plan must exist before a claim can be "Case Managed".')
			idw_dw[1].SetColumn('case_managed_flag')
			RETURN -1
		End if
	End if


/*	rehab officer
*/
	ls_string = idw_dw[1].GetItemString(ll_current_row, 'rehab_officer_user_id')
	IF NOT(Trim(ls_string) = '' OR IsNull(ls_string)) THEN
		IF idw_dw[1].GetChild('rehab_officer_user_id',ldwc_child) < 1 THEN
			MessageBox('Error Retrieveing List', 'Unable to retrieve list of valid rehab officiers.  Contact help desk.')
			Return -1
		END IF
		IF ldwc_child.Find('user_id = "' + ls_string +  '"',0, ldwc_child.RowCount()) < 1 THEN
   	   MessageBox('Error','Invalid rehab officer user id.  Please correct.')
      	Return -1
	   END IF 
	END IF

	// Occupational Therapist
	ls_string = idw_dw[1].GetItemString(ll_current_row, 'occupational_therapist_user_id')
	IF NOT(Trim(ls_string) = '' OR IsNull(ls_string)) THEN
		IF idw_dw[1].GetChild('occupational_therapist_user_id',ldwc_child) < 1 THEN
			MessageBox('Error Retrieving List', 'Unable to retrieve list of valid Occupational Therapist.  Contact help desk.')
			Return -1
		END IF
		IF ldwc_child.Find('user_id = "' + ls_string +  '"',0, ldwc_child.RowCount()) < 1 THEN
   	   MessageBox('Error','Invalid occupational therapist user id.  Please correct.')
      	Return -1
	   END IF 
	END IF

/*	accident county code
*/
	ls_string = idw_dw[9].GetItemString(ll_current_row, 'accident_county_code')
	IF NOT(Trim(ls_string) = '' OR IsNull(ls_string)) THEN
		IF idw_dw[9].GetChild('accident_county_code',ldwc_child) < 1 THEN
			MessageBox('Error Retrieveing List', 'Unable to retrieve list of valid accident county codes.  Contact help desk.')
			Return -1
		END IF
		IF ldwc_child.Find('county_code = "' + ls_string +  '"',0, ldwc_child.RowCount()) < 1 THEN
   	   MessageBox('Error','Invalid accident county.  Please correct.')
      	Return -1
	   END IF
	   IF ls_string = "99" AND idw_dw[9].GetItemString(ll_current_row, "accident_county_stat") = "Inside"  THEN
		  Messagebox('Error','The accident county code must be a county in New Brunswick.',Exclamation!)
	       Return -1
	   ELSEIF ls_string <> "99" AND  idw_dw[9].GetItemString(ll_current_row, "accident_county_stat") = "Outside" THEN
		   Messagebox('Error','The accident county code must be outside New Brunswick.',Exclamation!)
	       Return -1		
       END IF	
	ELSE
		IF idw_dw[9].GetItemStatus(ll_current_row,0,Primary!) <> NewModified! THEN
			MessageBox('Invalid Accident County','Accident County must be entered.',information!)
			RETURN -1
		END IF
	END IF

/*	accident residence region
*/
	ls_string = idw_dw[1].GetItemString(ll_current_row, 'accident_residence_county_code')
	IF NOT(Trim(ls_string) = '' OR IsNull(ls_string)) THEN
		IF idw_dw[9].GetChild('accident_residence_county_code',ldwc_child) < 1 THEN
			MessageBox('Error Retrieveing List', 'Unable to retrieve list of valid admin codes.  Contact help desk.')
			Return -1
		END IF
		IF ldwc_child.Find('county_code = "' + ls_string +  '"',0, ldwc_child.RowCount()) < 1 THEN
   	   		MessageBox('Error','Invalid accident residence county.  Please correct.')
      	Return -1
	   END IF 
	END IF

/* COST ALLOCATION VALIDATION IS NOT PERFORMED DURING CLAIM CREATE BECAUSE THE USER CAN'T
   ASSIGN COST ALLOCATION ON THAT SCREEN.
*/
IF idw_dw[1].GetItemStatus(ll_current_row,0,Primary!) <> NewModified! Then

	/* COST ALLOCATION
	*/
	
	//Get parameters for claim employer validation
		ls_claim_status_code	 		= idw_dw[1].GetItemString(ll_current_row, 'claim_status_code')
		ls_claim_status_type_code 	= idw_dw[1].GetItemString(ll_current_row, 'claim_status_type_code')
		ldt_accident_date 			= idw_dw[9].GetItemDateTime(ll_current_row,'accident_date')
		ll_individual_no				= idw_dw[1].GetItemNumber(ll_current_row, 'individual_no')	
	
	//Get cost allocation parameters for cost allocation validation
		ll_cost_alloc_no 				= idw_dw[1].GetItemNumber(ll_current_row, 'cost_alloc_no')
		ll_cost_alloc_operation_no = idw_dw[1].GetItemNumber(ll_current_row, 'cost_alloc_operation_no')	
	
	//BR_12.10 

	IF ll_cost_alloc_no = 0  and ll_cost_alloc_operation_no = 0 Then
	
		//Active and Finaled claims must be cost allocated 
		IF ls_claim_status_code = 'A'  or (ls_claim_status_code = 'F' and ls_claim_status_type_code <> '05') Then
			MessageBox('Cost Allocation Required','Cost Allocation is required for "Active" and "Finalled" claims.')	
			RETURN -1
		END IF
	ELSE
			
		IF idw_dw[1].GetItemStatus(ll_current_row, 'cost_alloc_no',Primary!) 					= DataModified! or &
			idw_dw[1].GetItemStatus(ll_current_row, 'cost_alloc_operation_no',Primary!)		= DataModified! or &
			idw_dw[9].GetItemStatus(ll_current_row,'accident_date',Primary!)						= DataModified! or &
			idw_dw[1].GetItemStatus(ll_current_row, 'individual_no',Primary!)						= DataModified! Then 
		
					
			IF inv_claim_employer.nf_set_employer_parameters(ll_cost_alloc_no,ll_cost_alloc_operation_no) <> 1 THEN RETURN -1
			IF inv_claim_employer.nf_set_claim_parameters(ll_claim_no,ldt_accident_date) <> 1 THEN RETURN -1
			IF inv_claim_employer.nf_set_claimant_parameters(ll_individual_no)  <> 1 THEN RETURN -1
			
			
			li_rtn = inv_claim_employer.nf_IsValid_Cost_Alloc_Employer_for_Claim()
			IF li_rtn <> 1 THEN
				RETURN -1
			END IF		
			
			//If cost has changed then set the employer type code
			IF idw_dw[1].GetItemStatus(ll_current_row, 'cost_alloc_no',Primary!) 				= DataModified! or &
				idw_dw[1].GetItemStatus(ll_current_row, 'cost_alloc_operation_no',Primary!)		= DataModified! THEN
			
				idw_dw[1].SetItem(ll_current_row,'cost_alloc_type_code',inv_claim_employer.is_employer_type_code)
			END IF
			
		END IF
	END IF

	
	//P10152-083 - R.S. New Cost Allocation rules
	//2.180	All claims cost allocated to the All-Assessed Silicosis account (# 3009) must have an accident date prior to January 1, 1948.
	IF ll_cost_alloc_no = 3009 and ldt_accident_date >= DATETIME(1948-01-01) THEN
		MESSAGEBOX("Cost allocation error", "All claims cost allocated to the All-Assessed Silicosis account (# 3009), must have an accident date prior to January 1, 1948.")
		RETURN -1
	END IF
	
	//2.190	All claims cost allocated to the All-Assessed Pre-a982 Claims account (# 1004) must have an accident date prior to January 1, 1982.
	IF ll_cost_alloc_no = 1004 and ldt_accident_date >= DATETIME(1982-01-01) THEN
		MESSAGEBOX("Cost allocation error", "All claims cost allocated to the All-Assessed Pre-1982 Claims account (# 1004), must have an accident date prior to January 1, 1982.")
		RETURN -1
	END IF
	
	// 2.210 
	ls_cost_alloc_type_code = idw_dw[1].GetItemString(ll_current_row,'cost_alloc_type_code')
	IF ldt_accident_date < DATETIME(1982-01-01) AND ldt_accident_date >= DATETIME(1948-01-01) THEN
		IF    ( ls_cost_alloc_type_code = 'R' AND   ll_cost_alloc_no  =  1004                                 AND ll_cost_alloc_operation_no = 1 ) &
			OR ( ls_cost_alloc_type_code = 'R' AND   ll_cost_alloc_no  =  5006                                 AND ll_cost_alloc_operation_no = 1 ) &
			OR ( ls_cost_alloc_type_code = 'S' AND ( ll_cost_alloc_no >= 50000 AND ll_cost_alloc_no < 100000 ) AND ll_cost_alloc_operation_no > 0 ) &
			OR ( ls_cost_alloc_type_code = ''  AND   ll_cost_alloc_no  =     0 ) THEN
				// a-ok!
		ELSE
			MESSAGEBOX('Cost allocation error', 'This claim has an accident date prior to 1982.' &
			                                  + '~r~n~r~nThe cost allocation must be:' &
														 + '~r~n~r~n~t•  All Assessed Pre-1982 Claims Account (1004) or' &
														 +     '~r~n~t•  All Assessed Disaster Account (5006) or' &
														 +     '~r~n~t•  Self-Insured employer.',StopSign!)
			RETURN -1
		END IF
	ELSEIF ldt_accident_date < DATETIME(1948-01-01) THEN
		IF    ( ls_cost_alloc_type_code = 'R' AND   ll_cost_alloc_no  =  1004                                 AND ll_cost_alloc_operation_no = 1 ) &
			OR ( ls_cost_alloc_type_code = 'R' AND   ll_cost_alloc_no  =  3009                                 AND ll_cost_alloc_operation_no = 1 ) &
			OR ( ls_cost_alloc_type_code = 'R' AND   ll_cost_alloc_no  =  5006                                 AND ll_cost_alloc_operation_no = 1 ) &
			OR ( ls_cost_alloc_type_code = 'S' AND ( ll_cost_alloc_no >= 50000 AND ll_cost_alloc_no < 100000 ) AND ll_cost_alloc_operation_no > 0 ) &
			OR ( ls_cost_alloc_type_code = ''  AND   ll_cost_alloc_no  =     0 ) THEN
				// a-ok!
		ELSE
			MESSAGEBOX('Cost allocation error', 'This claim has an accident date prior to 1948.' &
			                                  + '~r~n~r~nThe cost allocation must be:' &
														 + '~r~n~r~n~t•  All Assessed Pre-1982 Claims Account (1004) or' &
														 +     '~r~n~t•  All Assessed Silicosis Account (3009) or' &
														 +     '~r~n~t•  All Assessed Disaster Account (5006) or' &
														 +     '~r~n~t•  Self-Insured employer.',StopSign!)
			RETURN -1
		END IF
	ELSE
		
	END IF
	
	
	ldwis_cost_alloc_no = idw_dw[1].GetItemStatus(ll_current_row,'cost_alloc_no',Primary!)
	ldwis_cost_alloc_op_no = idw_dw[1].GetItemStatus(ll_current_row,'cost_alloc_operation_no',Primary!)
	
	//2.200	No cost allocation to the Prior Years' Claim Reserve (# 1003) is permitted.
	IF ldwis_cost_alloc_no = DataModified! THEN
		// only enforce this BR if the cost has been modified
		IF ll_cost_alloc_no = 1003  THEN
			MESSAGEBOX("Cost allocation error", "No cost allocation to the Prior Years' Claim Reserve (# 1003) is permitted.")
			RETURN -1
		END IF
	END IF
	
	/*T012790 2.230  Cost Allocation to the All-Assessed Non-Allocated Claim Cost Account is only for claims at Pre-adjudication, 
	              at Adjudication or claims Rejected – Disallowed or claims Rejected – Insufficient Information 
*/
	IF ll_cost_alloc_no = 8000 and ls_claim_status_code <> 'P' and ls_claim_status_code <> 'J'  AND NOT(ls_claim_status_code = 'R' and ls_claim_status_type_code = '07') AND NOT ( ls_claim_status_code = 'R' and ls_claim_status_type_code = '18') THEN
		MESSAGEBOX("Cost allocation error", "Cost Allocation # 8000 is only permitted for claims with status of Pre-Adjudication, Adjudication, Rejected - Disallowed, or Rejected - Insufficient Information")
		RETURN -1
	END IF	

/*
    T012790   2.240   The cost allocation must be All-Assessed Non-Allocated Claim (8000 GL account code) if the claim status is at 
	                            Pre-adjudication, Adjudication, Rejected – Disallowed or Rejected – Insufficient Information and the claim has 
							  unprocessed (scheduled) payments.        See Rationale

	
	                  (user could make payments while still at P, J, or R-07 or R-18 and then prior to payment processing running,  try to
				       change the cost allocation to 0 (which causes a database constraint error), or change it to some other cost allocation number, 
						 which causes the wrong number to be applied for cost allocation purposes, because during payment processing it gets the current 
						 value from the claim for cost allocation purposes, not the value that was present when the payment was created)
*/
	
	IF (ll_cost_alloc_no <> 8000  or ll_cost_alloc_operation_no <> 1) AND &
	    (ls_claim_status_code = 'P' OR ls_claim_status_code = 'J'  OR ( ls_claim_status_code = 'R' and ls_claim_status_type_code = '07') OR ( ls_claim_status_code = 'R' and ls_claim_status_type_code = '18')) THEN 
		
		SELECT count(*)
		INTO :ll_count
		FROM UNAPPLIED_CLAIM_TXN
		WHERE claim_no = :ll_claim_no
		Using  SQLCA;
		SQLCA.nf_handle_error('n_claims','nf_check_bus_rule', 'Select on UNNAPLIED_CLAIM_TXN')
		
		IF 	ll_count > 0 THEN		
			MESSAGEBOX("Cost allocation error", "There are scheduled payments pending for this claim, therefore the cost allocation no and cost allocation operation no must be 8000/1.")
			RETURN -1
		END IF
	END IF

	
	/* ACCIDENT EMPLOYER
	*/
	
	//Get accident employer parameters for accident employer validation
	ll_accident_employer_no 	= idw_dw[9].GetItemNumber(ll_current_row, 'accident_employer_no')
	ll_accident_employer_operation_no = idw_dw[9].GetItemNumber(ll_current_row, 'accident_employer_operation_no')
	
	//a claim doesn't have to have an accident employer.
	IF not(ll_accident_employer_no = 0 and ll_accident_employer_operation_no = 0) Then
	
		IF idw_dw[9].GetItemStatus(ll_current_row, 'accident_employer_no',Primary!)			= DataModified! or &
			idw_dw[9].GetItemStatus(ll_current_row, 'accident_employer_operation_no',Primary!) = DataModified! or &
			idw_dw[9].GetItemStatus(ll_current_row, 'accident_date',Primary!)						= DataModified! Then 
		
					
			li_rtn = inv_claim_employer.nf_set_employer_parameters(ll_accident_employer_no,ll_accident_employer_operation_no)
			IF li_rtn = -1 Then
				idw_dw[9].SetColumn('accident_employer_no')
				RETURN -1
			ELSEIF li_rtn = 1 Then	
				IF inv_claim_employer.nf_set_claim_parameters(ll_claim_no,ldt_accident_date) <> 1 THEN RETURN -1
						
				li_rtn = inv_claim_employer.nf_IsValid_Accident_Employer_for_Claim()
				IF li_rtn <> 1 THEN
					RETURN -1
				END IF	
				
				//If cost has changed then set the employer type code
				IF idw_dw[9].GetItemStatus(ll_current_row, 'accident_employer_no',Primary!) 				= DataModified! or &
					idw_dw[9].GetItemStatus(ll_current_row, 'accident_employer_operation_no',Primary!)	= DataModified! THEN
				
					idw_dw[1].SetItem(ll_current_row,'employer_type_code',inv_claim_employer.is_employer_type_code)
					idw_dw[1].SetItem(ll_current_row,'classification_system_code',inv_claim_employer.is_classification_system_code)
					idw_dw[1].SetItem(ll_current_row,'classification_system_type_code',inv_claim_employer.is_classification_system_type_code)				    
				END IF
			End if
		END IF
	END IF
END IF

//CPPD Status

Select Count(*)
Into    :ll_count
From  CLAIM_REMINDER
Where claim_no = :ll_claim_no
And     reminder_status_code = 'P'
Using  SQLCA;

SQLCA.nf_handle_error('n_claims','nf_check_bus_rule','Select on CLAIM_REMINDER')


ls_string = idw_dw[1].GetItemString(ll_current_row, 'cppd_status_code')
IF ls_string = 'R' THEN //recieving CPPD benefits must have a start date
	ldt_cppd_start = Date(idw_dw[1].GetItemDateTime(ll_current_row,'cppd_benefit_start_date'))
	IF IsNull(ldt_cppd_start) OR ldt_cppd_start = 1900-01-01 THEN
		MessageBox('CPP Disablity Start Date Error','A CPP Disability Start Date must be entered if the claimant is receiving CPP Disability benefits.',Exclamation!,Ok!)
		Return -1
	END IF
END IF

// CPPD Benefit Start Date

ldt_date = Date(idw_dw[1].GetItemDateTime(ll_current_row, 'cppd_benefit_start_date'))
ls_cppd_status_code = idw_dw[1].GetItemString(ll_current_row, 'cppd_status_code')
IF (IsNull(ldt_date) and  ls_cppd_status_code = 'R')  OR (NOT Isnull(ldt_date) AND  ls_cppd_status_code <> 'R') THEN
	MessageBox('CPP Disablity Start Date Error','A CPP Disability Start Date must not be entered if the claimant is not receiving CPP Disability benefits.',Exclamation!,Ok!)
	Return -1
ELSE 
	IF ldt_date < 1982-01-01 THEN 	
			MessageBox('CPP Disablity Start Date Error','A CPP Disability Start Date must be on or after 1982-01-01.',Exclamation!,Ok!)
			Return -1
	ELSEIF ldt_date > RelativeDate(ldt_today, -1) THEN
			MessageBox('CPP Disablity Start Date Error','A CPP Disability Start Date must be in the past.',Exclamation!,Ok!)
			Return -1
	END IF	
END IF	
	
//Check CPPD Reminders Statuses create/update CLAIM_REMINDERS
IF nf_check_cppd_reminders() < 0 THEN 
	Return -1
END IF	


/*	comp week
*/
	ls_string = Trim(idw_dw[9].GetItemString(ll_current_row, 'comp_week_code'))
	IF Trim(ls_string) <> '' AND NOT IsNull(ls_string) THEN
		IF ls_string <> 'E' AND ls_string <> 'O' THEN
			MessageBox('Invalid Comp Week', 'The comp week is invalid.  Please correct.')
			Return -1
		END IF
	END IF
/*	comp day
*/
	ls_string = idw_dw[9].GetItemString(ll_current_row, 'comp_day_code')
	IF Trim(ls_string) <> '' AND NOT IsNull(ls_string) THEN
		SELECT comp_day_code
		  INTO :ls_string2
		  FROM Comp_Day
		 WHERE comp_day_code = :ls_string;
   	IF SQLCA.nf_handle_error( 'n_claims', 'nf_check_bus_rule','Embedded SQL: select from Comp_Day') < 0 THEN
			Return -1
		END IF
		IF ls_string <> ls_string2 THEN
			MessageBox('Invalid Comp Day', 'The comp day is invalid.  Please correct.')
			Return -1
		END IF
	END IF

/*	months of service
*/
	IF idw_dw[1].GetItemNumber(ll_current_row, 'months_of_service') > 11 THEN
		MessageBox('Invalid Months of Service', 'The months of sevice field must be less than 12.  Use the years column to indicate years.')
		Return -1
	END IF
	IF idw_dw[1].GetItemNumber(ll_current_row, 'months_of_service') < 0 THEN
		MessageBox('Invalid Months of Service', 'The months of sevice field cannot be less than zero.')
		Return -1
	END IF

/*	years of service
*/
	IF idw_dw[1].GetItemNumber(ll_current_row, 'years_of_service') > 65 THEN
		MessageBox('Invalid Months of Service', 'The years of sevice field must be less than 65.')
		Return -1
	END IF
	IF idw_dw[1].GetItemNumber(ll_current_row, 'years_of_service') < 0 THEN
		MessageBox('Invalid Years of Service', 'The years of sevice field cannot be less than zero.')
		Return -1
	END IF

/*	receiving salary - if changed check for scheduled payments with old value and warn
   Removed column; the payment method code of ‘R’ has been clarified to mean 
	‘Recorded/Not Issued Receiving Salary’ and therefore this column is 
	not required (i.e. use the payment method of R instead of the receiving_salary_flag)
*/
	ls_string = idw_dw[1].GetItemString(ll_current_row,'receiving_salary_flag',Primary!, TRUE)
	IF ls_string <> idw_dw[1].GetItemString(ll_current_row,'receiving_salary_flag') THEN
		
		SELECT Count(*)
		  INTO :ll_count
		  FROM UNAPPLIED_CLAIM_TXN, PAYMENT
		 WHERE UNAPPLIED_CLAIM_TXN.payment_method_code = 'R'
			AND PAYMENT.payment_no          = UNAPPLIED_CLAIM_TXN.payment_no
			AND PAYMENT.claim_no            = :ll_claim_no;
   	
        SQLCA.nf_handle_error( 'n_claims', 'nf_check_bus_rule','Embedded SQL: select from UNAPPLIED_CLAIM_TXN')
			
		IF ll_count > 0 THEN
			ll_count = MessageBox('Warning','This claim is recieving salary and there are scheduled payments.  Do you wish to continue?',Question!,YesNo!)
			IF ll_count = 2 THEN Return -1
		END IF
	END IF

/*	legislation code
*/
	ls_string = idw_dw[10].GetItemString(ll_current_row, 'legislation_code')
	IF NOT(Trim(ls_string) = '' OR IsNull(ls_string)) THEN
		IF nf_validate_legislation(ls_string) < 0 THEN
			Return -1
		END IF
	END IF

//	Pension cost allocation number
	ll_no = idw_dw[10].GetItemNumber(ll_current_row, 'pen_cost_alloc_no')
	IF ll_no > 0 THEN
   	SELECT E.employer_no, E.employer_type_code, ET.employer_type_desc
	     INTO :ll_count, :ls_employer_type_code, :ls_employer_type_desc 
   	  FROM EMPLOYER E,
		       Employer_Type ET 
	    WHERE E.employer_no = :ll_no 
		   AND E.employer_type_code = ET.employer_type_code ;
		 
		li_rtn = SQLCA.nf_handle_error( 'n_claims', 'nf_check_bus_rule','Embedded SQL: select from Employer')
   	IF li_rtn < 0 THEN
			Return -1
		ELSEIF li_rtn = 100 THEN
	      MessageBox("Invalid Pension Allocation Number", String(ll_no) + " is not a valid employer number.~r" +&
						  "Please correct.", Information!)
	      Return -1
   	END IF 
		
		
		CHOOSE CASE ls_employer_type_code
			CASE 'P','S','F'
				// if this is an FCA claim then if pension cost alloc is populated, it must be populated with a firefighter cost alloc
				ls_administering_act_code = idw_dw[1].GetItemString(ll_current_row, 'administering_act_code')
				IF ls_employer_type_code <> 'F' and ls_administering_act_code = 'FCA' THEN
					MessageBox('Invalid Pension Cost Allocation', 'The pension cost allocation number must be of type Firefighter ' +&
							  'for an FC Act claim.~rEmployer ' + String(ll_no) + ' is a ' + ls_employer_type_desc +&
							  ' type employer.  Please correct.', Information!)
					Return -1
				ELSEIF ls_employer_type_code = 'F' and ls_administering_act_code <> 'FCA' THEN
					MessageBox('Invalid Pension Cost Allocation', 'For a Workers Compensation Act claim, the pension cost allocation number' +&
							  '~rmust not be of type Firefighter.  Please correct.', Information!)
					Return -1
				END IF
				
				// OK
				idw_dw[10].SetItem(ll_current_row,'pen_cost_alloc_type_code', ls_employer_type_code)
			CASE ELSE
				MessageBox('Invalid Pension Cost Allocation', 'The pension cost allocation number must be of type Pension, ' +&
							  'Self-insured or Firefighter.~rEmployer ' + String(ll_no) + ' is a ' + ls_employer_type_desc +&
							  ' type employer.  Please correct.', Information!)
				Return -1
		END CHOOSE
		
		ldt_accident_date = idw_dw[10].GetItemDateTime(ll_current_row,'accident_date')
		IF nf_cost_allocation_no_cutoff( 2002-04-01 , ldt_accident_date , ll_no , 50001 , 603509 , 'pension cost allocation' ) = 1 THEN
			RETURN -1
		END IF
		
	END IF

//	pension cost allocation operation number
	ll_no = idw_dw[10].GetItemNumber(ll_current_row, 'pen_cost_alloc_operation_no')
	ll_no2 = idw_dw[10].GetItemNumber(ll_current_row, 'pen_cost_alloc_no')
	IF ll_no > 0 THEN
		
		SELECT employer_no
		  INTO :ll_temp 
		  FROM EMPLOYER 
		 WHERE employer_no = :ll_no2 ;
		li_rtn = SQLCA.nf_handle_error( 'n_claims', 'nf_check_bus_rule','Embedded SQL: select from EMPLOYER')

		IF li_rtn < 0 THEN
			RETURN -1
		ELSEIF li_rtn = 100 THEN
			MessageBox("Invalid Pension Allocation Number", String(ll_no2) + " is not a valid employer number.~r" +&
						  "Please re-enter and try again.", Information!)
			RETURN -1
		END IF
		
   	SELECT employer_no, sold_to_employer_no, sold_to_employer_no, sold_to_operation_no
	     INTO :ll_count, :ll_no3, :ll_sold_to_employer_no, :ll_sold_to_operation_no 
   	  FROM OPERATION 
	    WHERE operation_no = :ll_no 
   	   AND employer_no = :ll_no2;
			
		li_rtn = SQLCA.nf_handle_error( 'n_claims', 'nf_check_bus_rule','Embedded SQL: select from OPERATION')
   	IF li_rtn < 0 THEN
			Return -1
		ELSEIF li_rtn = 100 THEN
   	   MessageBox("Invalid Pension Operation Number", "Operation " + String(ll_no) + " is an invalid " +&
						  "operation for employer " + String(ll_no2) + ".~rPlease correct.", Information!)
	      Return -1
   	END IF 

		IF ll_no3 > 0 OR ll_sold_to_employer_no > 0 OR ll_sold_to_operation_no > 0 THEN 
			MessageBox("Invalid Pension Cost Allocation Numbers","The employer entered for pension has been sold." +&
						  "~rPlease re-enter.", Information!)
			Return -1
		END IF
		IF nf_validate_cost_alloc(ll_no2, ll_no, 'P') < 0 THEN
			RETURN -1
		END IF
	END IF

/*	percent disability
*/
	ldec_percent = idw_dw[10].GetItemDecimal(ll_current_row,'percent_disability')
	IF NOT IsNull(ldec_percent) THEN
		IF ldec_percent > 100 OR ldec_percent < 0 THEN
			MessageBox('Invalid % Disability','The % disability must be between 0 and 100.')
			idw_dw[10].SetColumn('percent_disability')
			idw_dw[10].SetFocus()
			Return -1
		END IF
		IF ldec_percent = 0 AND idw_dw[1].GetItemDecimal(ll_current_row,'percent_disability',Primary!, TRUE) > 0 THEN
/*		need to check for payment types of  16 - they must be fully cancelled or adjusted
*/
			SELECT Count(*)
			  INTO :ll_count
			  FROM PAYMENT
			 WHERE claim_no = :ll_claim_no
			   AND  payment_type_code = '16'
				AND zeroed_flag <> 'Y';

			IF SQLCA.nf_handle_error( 'n_claims', 'nf_check_bus_rule','Embedded SQL: select from PAYMENT') < 0 THEN
				Return -1
			END IF
			IF ll_count > 0 THEN
				MessageBox('Error - % disability','The % disability cannot be set to zero since there are payments that have not been cancelled or fully adjusted.')
				Return -1
			END IF
/*		also the determination date must be null
*/
			SetNull(ldtm_date)
			idw_dw[10].SetItem(ll_current_row,'disability_determination_date', ldtm_date)
		END IF
	END IF

/*	disability determination date
*/
	ldtm_date =	idw_dw[10].GetItemDateTime(ll_current_row,'disability_determination_date')
	IF NOT IsNull(ldtm_date) THEN
		IF ldtm_date > ldtm_today THEN
   			MessageBox("Invalid Determination Date", "The disability determination date cannot be beyond today.")
	   		idw_dw[10].SetColumn('disability_determination_date')
			idw_dw[10].SetFocus()
	   	Return -1
		END IF
		IF ldtm_date < idw_dw[1].GetItemDateTime(ll_current_row,'accident_date') THEN
			MessageBox('Invalid Determination Date', 'The disability determination date cannot be before the accident date.')
	   		idw_dw[10].SetColumn('disability_determination_date')
			idw_dw[10].SetFocus()
			Return -1
		END IF
		
	END IF
	
	//T023985 - PPI information - if % is entered, date must be entered
	
	ldec_percent_old =  idw_dw[1].GetItemDecimal(ll_current_row, 'percent_impaired', PRIMARY!, TRUE)
	ldtm_date_old = idw_dw[1].GetItemDateTime(ll_current_row, 'impaired_determination_date', PRIMARY!, TRUE)
	ldec_percent = idw_dw[1].GetItemDecimal(ll_current_row,'percent_impaired')
	ldtm_date =	idw_dw[1].GetItemDateTime(ll_current_row,'impaired_determination_date')
	
	SELECT Count(*)
	INTO    :ll_pmt
	FROM   PAYMENT
	WHERE claim_no = :ll_claim_no
	AND      payment_type_code = '.4'
	USING  SQLCA;
	
	SQLCA.nf_handle_error( 'n_claims', 'nf_check_bus_rule',"Embedded SQL: SELECT Count(*) PAYMENT WHERE payment_type_code = '.4'")
	
	IF ll_pmt > 0 THEN
		IF (ldec_percent_old > 0 AND ldec_percent = 0) THEN
			MessageBox('% PPI', 'A (.4) payment exists for this claim.  The % PPI cannot be removed.', Information! )
			RETURN -1
		END IF
		
		IF (NOT IsNull(ldtm_date_old) AND IsNull(ldtm_date)) THEN
			MessageBox('PPI Determination Date', 'A (.4) payment exists for this claim.  The PPI Determination date cannot be removed.', Information! )
			RETURN -1
		END IF
	END IF
	
	IF IsNull(ldtm_date) THEN		
		IF (ldec_percent > 0) THEN
			MessageBox('Invalid Impaired Date','The Impaired Date must be entered if the % PPI is entered.', Information!)
			RETURN -1
		END IF
	END IF

/*	% Permanent Partial Impairment
*/	
	IF NOT IsNull(ldec_percent) THEN
		IF ldec_percent > 100 OR ldec_percent < 0 THEN
			MessageBox('Invalid % PPI','The % Permanent Partial Impairment must be between 0 and 100.')
			idw_dw[1].SetColumn('percent_impaired')
			idw_dw[1].SetFocus()
			Return -1
		END IF
	END IF

/*	impairment determination date
*/
	IF NOT IsNull(ldtm_date) THEN
		IF ldtm_date > ldtm_today THEN
   		MessageBox("Invalid Impaired Date", "The impaired determination date cannot be beyond today.")
	   	idw_dw[1].SetColumn('impaired_determination_date')
			idw_dw[1].SetFocus()
	   	Return -1
		END IF
		IF ldtm_date < idw_dw[1].GetItemDateTime(ll_current_row,'accident_date') THEN
			MessageBox('Invalid Impaired Date', 'The impaired determination date cannot be before the accident date.')
	   		idw_dw[1].SetColumn('impaired_determination_date')
			idw_dw[1].SetFocus()
			Return -1
		END IF
		
	END IF
	
	
	/* for project 10127 (medical society agreement) we must confirm the following 
   The change to the status cannot be saved until at least one document has been identified
	as used in the decision process.
	Click on Save Button (on w_claim) saves changes to both the CLAIM and 
	ELIGIBLE_REPORT_FEE_DOCUMENTS table
	Click on the cancel button (on w_claim) cancels changes  to both the claim and 
	ELIGIBLE_REPORT_FEE_DOCUMENTS table
*/
	IF is_claim_status_change = 'R' OR is_claim_status_change = 'F' OR is_claim_status_change = 'A' THEN
		li_rtn = nf_report_fee_doc_presave(is_claim_status_change)
		IF li_rtn = -1 THEN
			RETURN -1
		END IF
	END IF

Return 1

end function

public function integer nf_check_accident_county (string as_claim_status_code, string as_claim_status_type_code);// this function checks the accident county code
String ls_status_code, ls_status_type_code, ls_accident_county_code, ls_org_status_code, ls_org_status_type
Long   ll_count, ll_claim_no, ll_current_row


ll_current_row =  idw_dw[9].GetRow()
ll_claim_no = idw_dw[9].GetItemNumber(ll_current_row,'claim_no')
ls_accident_county_code =  idw_dw[9].GetItemString(ll_current_row, 'accident_county_code')
ls_status_code = as_claim_status_code
ls_status_type_code = as_claim_status_type_code

Select claim_status_code, claim_status_type_code
Into    :ls_org_status_code, :ls_org_status_type
From  CLAIM
Where claim_no = :ll_claim_no
Using SQLCA;

SQLCA.nf_handle_error("w_claim","nf_check_accident_county_code","Select from CLAIM")

//If the existing claim status is Active or Finalled 01,02,03,04 then ignore the Accident County Question
IF ls_org_status_code = 'A' OR (ls_org_status_code = 'F' AND ( ls_org_status_type = '01' OR ls_org_status_type = '02' OR &
ls_org_status_type = '03' OR ls_org_status_type = '04')) THEN 
	idw_dw[9].SetItem(ll_current_row, 'accident_county_stat',"")
	Return 0 // No further checking required
END IF	

IF ls_status_code = 'A' OR (ls_status_code = 'F' AND ( ls_status_type_code = '01' OR ls_status_type_code = '02' OR &
ls_status_type_code = '03' OR ls_status_type_code = '04')) THEN 
	//Check below
ELSE
	idw_dw[9].SetItem(ll_current_row, 'accident_county_stat',"")
	Return 0 // No further checking required
END IF	


Select  count(*)
Into      :ll_count
From    CLAIM_STATUS_CHANGE
Where  claim_no = :ll_claim_no
And ((new_claim_status_code = 'A' 
	Or   (new_claim_status_code = 'F' AND new_claim_status_type_code = '01')
	Or   (new_claim_status_code = 'F' AND new_claim_status_type_code = '02')
	Or   (new_claim_status_code = 'F' AND new_claim_status_type_code = '03')
	Or   (new_claim_status_code = 'F' AND new_claim_status_type_code = '04'))
    OR  (old_claim_status_code = 'A'
    	Or   (old_claim_status_code = 'F' And old_claim_status_type_code = '01')
    	Or   (old_claim_status_code = 'F' And old_claim_status_type_code = '02')
    	Or   (old_claim_status_code = 'F' And old_claim_status_type_code = '03')
    	Or   (old_claim_status_code = 'F' And old_claim_status_type_code = '04')))	 
Using SQLCA;

SQLCA.nf_handle_error("w_claim","nf_check_accident_county_code","Select from CLAIM_STATUS_CHANGE")


IF ll_count  = 0 THEN //This is the first time the claim status has been set
	IF Messagebox ("Question?","Did this accident happen outside New Brunswick?",Question!,YesNo!) = 1 THEN
		//Set the County of Accidents to 99 Outside NB
		idw_dw[9].SetItem(ll_current_row, 'accident_county_stat','Outside')
		IF ls_accident_county_code <> '99' THEN
			idw_dw[9].SetItem(ll_current_row, 'accident_county_stat','Outside')
			idw_dw[9].SetItem(ll_current_row, 'accident_county_code','99')
			Messagebox("Information","The accident county code has been set to Outside New Brunswick.",Information!)
			Return 0
		END IF
	ELSE
		idw_dw[9].SetItem(ll_current_row, 'accident_county_stat','Inside')
		//Set the county of Accidents inside NB
		IF ls_accident_county_code = '99' THEN
			Messagebox("Error","The accident county code must be set to a county within New Brunswick.",Information!)
			Return -1
		END IF	
	END IF
END IF

Return 0
end function

public function integer nf_check_cppd_reminders ();String		ls_claim_status_code, ls_cppd_status_code_org, ls_cppd_status_new, ls_claim_status_new, ls_opening_type
Long		ll_claim_no, ll_count, ll_active_opening, ll_count_reminders
Date		ldt_today, ldt_annual_review_date
Datetime	ldtm_review_due_date_org
Boolean	lb_update = FALSE

ldt_today = Date(f_server_datetime())


ll_claim_no 					= idw_dw[1].GetItemNumber(idw_dw[1].GetRow(),"claim_no")
ls_claim_status_new 		= idw_dw[1].GetItemString(idw_dw[1].GetRow(),"claim_status_code")
ls_cppd_status_new 		=  idw_dw[1].GetItemString(idw_dw[1].GetRow(),"cppd_status_code")
ldt_annual_review_date 	= DATE(idw_dw[1].GetItemDatetime(idw_dw[1].GetRow(),"annual_ben_review_due_date") )

Select claim_status_code, cppd_status_code, annual_ben_review_due_date
Into    :ls_claim_status_code, :ls_cppd_status_code_org, :ldtm_review_due_date_org
From  CLAIM
Where claim_no = :ll_claim_no
Using   SQLCA;

SQLCA.nf_handle_error("n_claims","nf_check_cppd_reminders","Select from Claim")

Select count(*)
Into    :ll_active_opening
From  OPENING
Where claim_no = :ll_claim_no
And     opening_type_code = 'LTD'
And     (benefit_end_date > :ldt_today OR benefit_end_date Is Null)
Using   SQLCA;

SQLCA.nf_handle_error("n_claims","nf_check_cppd_reminders","Select from OPENING")


//Count the planned reminders
Select Count(*)
Into    :ll_count
From  CLAIM_REMINDER
Where claim_no = :ll_claim_no
And     reminder_status_code = 'P'
Using   SQLCA;
	
SQLCA.nf_handle_error("n_claims","nf_check_cppd_reminders","Select from CLAIM_REMINDER")

//Check Annual Review
IF DATE(ldtm_review_due_date_org) < ldt_annual_review_date THEN 
	IF ls_claim_status_new = 'A' AND ll_active_opening > 0 AND (ls_cppd_status_new <> 'R' AND ls_cppd_status_new <> 'D' AND ls_cppd_status_new <> 'N') THEN

// Note * There should not be more than 1 planned reminder of the same type when the annual review date changes
		Select 	Count(*)
		Into    	:ll_count_reminders
		From  	CLAIM_REMINDER
		Where  	claim_no 						= :ll_claim_no
		And      	reminder_type_code 			= 'CPPD'
		And     	reminder_sub_type_code 	= 'LTD'
		And     	reminder_status_code 		= 'P'
		Using 		SQLCA;
		
		SQLCA.nf_handle_error("n_claims","nf_check_cppd_reminders","Select from CLAIM_REMINDER")
		
		IF ll_count_reminders = 0 THEN
			//create auto reminder
			inv_reminders.nf_create_auto_reminder(ll_claim_no,'CPPD','LTD')
			MessageBox("CPP Disability Reminder","The change in the review date has created a planned CPP Disability reminder.",Information!,ok!)
			lb_update = TRUE
		END IF
	END IF	
END IF	

//Check if the claim status has changed to Finalled
IF ls_claim_status_code <> "F" AND ls_claim_status_new = "F" THEN
	IF ll_count > 0 THEN // The planned Reminders must be set to cancelled.
		inv_reminders.nf_update_reminder(ll_claim_no,'X','P')
		lb_update = TRUE
	END IF
END IF	
	
//Check CPP Disability Status Change

ls_cppd_status_new = idw_dw[1].GetItemString(idw_dw[1].GetRow(),"cppd_status_code")

/*
1.840	All planned CPP Disability reminders must be Completed if the CPP Disability status changes from Unknown to any of the following:
·	Denied, 
·	Not Applicable.

1.860	All planned CPP Disability reminders must be Completed if the CPP Disability status changes to Receiving CPP Disability.
*/
IF (ls_cppd_status_code_org = "U" AND ( ls_cppd_status_new = "D" OR ls_cppd_status_new = "N")) OR &
	(ls_cppd_status_code_org <> 'R' AND ls_cppd_status_new = "R" )THEN 
	IF ll_count > 0 THEN //The planned reminders must be set to Completed
		inv_reminders.nf_update_reminder(ll_claim_no,'C','P')
		lb_update = TRUE
	END IF
END IF

//Change from Unknown to Inapplicable
IF ls_cppd_status_code_org = "U" AND ls_cppd_status_new = "I" THEN 
	IF ll_count > 0 THEN //The planned reminders must be set to Cancelled
		inv_reminders.nf_update_reminder(ll_claim_no,'X','P')
		lb_update = TRUE		
	END IF
END IF

IF	lb_update = TRUE THEN  //Re-retrieve the reminder datawindow if a planned reminder is cancelled or completed.
		idw_dw[11].retrieve(ll_claim_no)
		SQLCA.nf_handle_error('Error','n_claim','Retrieve on idw_dw[14]')
END IF

RETURN 0
end function

public function integer nf_check_bus_rule_shared ();Long     ll_no, ll_count, ll_current_row, ll_no2, ll_no3, ll_claim_no, ll_no4, ll_row
Long     ll_sold_to_employer_no, ll_sold_to_operation_no, ll_temp, ll_no_old
Integer  li_rtn
String   ls_string, ls_string2, ls_employer_type_desc, ls_employer_type_code
string	ls_claim_manager_user_id, ls_stats_coded,ls_pob
STRING	ls_case_managed_flag
STRING	ls_job_position_code, ls_cppd_status_code
Datetime ldtm_date, ldtm_today, ldtm_create_date
DateTime	ldt_accident_date
Decimal  ldec_percent
Date 		ldt_cppd_start, ldt_date, ldt_today
DATAWINDOWCHILD ldwc_child

STRING			ls_claim_status_code, ls_claim_status_type_code
LONG				ll_individual_no
LONG				ll_cost_alloc_no, ll_cost_alloc_operation_no
LONG				ll_accident_employer_no, ll_accident_employer_operation_no

	ldtm_today = f_server_datetime()
	ldt_today = Date(ldtm_today)
	ll_current_row = idw_dw[1].GetRow()

/*	make sure that all the fields are filled in that are necessary to enter a claim
	validate uniqueness of claim number necessary if user enters their own claim number
*/
	ll_claim_no = idw_dw[1].GetItemNumber(ll_current_row,'claim_no')

	IF ll_claim_no <> 0 AND idw_dw[1].GetItemStatus(ll_current_row,0,Primary!) = NewModified! THEN
/*		the user must have used the override button on the claim creation and has entered a claim no
		it now must be validated for uniqueness
*/
   	SELECT Count(*) 
	     INTO :ll_count
   	  FROM CLAIM
	    WHERE claim_no = :ll_claim_no;

		li_rtn = SQLCA.nf_handle_error( 'n_claims', 'nf_check_bus_rule','Embedded SQL: select from CLAIM')
		IF li_rtn < 0 THEN
			RETURN -1
		ELSEIF ll_count > 0 THEN
   	   MessageBox('Error','Claim number already exists on file.  Please enter another number.')
			idw_dw[1].SetColumn('claim_no')
			idw_dw[1].SetFocus()
	      RETURN -1
   	END IF

		/* Override is for old claims only so must be <= 999999 */
		IF ll_claim_no > 999999 THEN
   	         MessageBox('Error','Override claim number must be less than or equal 999999 .  Please enter another number.')
			idw_dw[1].SetColumn('claim_no')
			idw_dw[1].SetFocus()
	      RETURN -1
		END IF
		

/*		check to see if the claim exists in the CLAIM_MASTER
*/
		SELECT imaged_claim_flag
		  INTO :ls_string
		  FROM CLAIM_MASTER
		 WHERE claim_no = :ll_claim_no
		 USING IMAGETRANS;

		IF IMAGETRANS.nf_handle_error('Embedded SQL: Select CLAIM_MASTER','n_claims', 'nf_check_bus_rule') < 0 THEN
			Return -1
		END IF
		IF IsNull(ls_string) OR ls_string = '' THEN
/*		ask the user how the imaged flag should be set since this is probably an old claim,
		the default is Y for imaged on all new claims but since the user entered a claim
   	number it may be a mircofilmed claim and may not be imaged.
*/
		   ll_no = MessageBox('Image Flag Setting', 'Is this claim imaged?', Question!, YesNo!)
   		IF ll_no = 2 THEN
      		idw_dw[1].SetItem(ll_current_row,'imaged_flag', 'N')
		   END IF
		ELSE
			IF ls_string = 'Y' THEN
				MessageBox('Warning','This is an imaged claim.')
      		idw_dw[1].SetItem(ll_current_row,'imaged_flag', 'Y')
			ELSE 
/* 		exists in claim master but imaged flag set to no
*/
      		idw_dw[1].SetItem(ll_current_row,'imaged_flag', 'N')
			END IF			
		END IF
	END IF


/*	check the admin region code
*/
	ls_string = idw_dw[1].GetItemString(ll_current_row,'admin_region_code')
	IF idw_dw[1].GetChild('admin_region_code',ldwc_child) < 1 THEN
		MessageBox('Error Retrieveing List', 'Unable to retrieve list of valid admin codes.  Contact help desk.')
		Return -1
	END IF
	IF ldwc_child.Find('admin_region_code = "' + ls_string + '"',0, ldwc_child.RowCount()) < 1 THEN
	   MessageBox('Error','Admin region is invalid.  Please correct.')
		idw_dw[1].SetColumn('admin_region_code')
		idw_dw[1].SetFocus()
		Return -1
	END IF


/*	claim status code
*/
	ls_string = idw_dw[1].GetItemString(ll_current_row,'claim_status_code')
	IF idw_dw[1].GetChild('claim_status_code',ldwc_child) < 1 THEN
		MessageBox('Error Retrieveing List', 'Unable to retrieve list of valid claim status codes.  Contact help desk.')
		Return -1
	END IF
	IF ldwc_child.Find('claim_status_code = "' + ls_string + '"',0, ldwc_child.RowCount()) < 1 THEN
   	MessageBox('Error','Invalid claim status code.  Please correct.')
		idw_dw[1].SetColumn('claim_status_code')
		idw_dw[1].SetFocus()
	   Return -1
	END IF 
	
	/* P10151-4 Claim in Error - The status of a claim can be changed to rejected if all
	   of the following conditions apply
		CHECK FUNCTION - MESSAGES PRODUCED IN FUNCTION
	*/
	IF ls_string = 'R' THEN
		IF nf_status_change_to_rejected(ll_claim_no) < 0 THEN RETURN -1
	END IF 
	
/*	claim status type
*/
	ls_string = Trim(idw_dw[1].GetItemString(ll_current_row,'claim_status_type_code'))
	ls_string2 = Trim(idw_dw[1].GetItemString(ll_current_row,'claim_status_code'))
	
/*	NOTE: this information is not check against a dddw because the claim
	create does not require this field and therefore does not have the dddw to check against.
*/
	IF ls_string <> '' AND NOT IsNull(ls_string) THEN
   	SELECT Count(*)
	     INTO :ll_count
   	  FROM Claim_Status_Type
	    WHERE claim_status_type_code = :ls_string
   	   AND claim_status_code = :ls_string2
	      AND active_flag = 'Y';

		li_rtn = SQLCA.nf_handle_error('n_claims', 'nf_check_bus_rule','Embedded SQL: select from Claim_Status_Type')
		IF li_rtn < 0 THEN
			RETURN -1
		ELSEIF li_rtn = 100 THEN
   	   MessageBox('Error','Invalid claim status type for the claim status.  Please correct.')
			idw_dw[1].SetColumn('claim_status_type_code')
			idw_dw[1].SetFocus()
	      Return -1
   	END IF
	ELSE
/*	if blank make sure that there is nothing to choose from
*/
   	SELECT Count(*)
	     INTO :ll_count
   	  FROM Claim_Status_Type
	    WHERE claim_status_code = :ls_string2
		AND claim_status_type_code  > ''
	    AND active_flag = 'Y';
		IF SQLCA.nf_handle_error( 'n_claims', 'nf_check_bus_rule','Embedded SQL: select from Claim_Status_Type') < 0 THEN
			Return -1
		END IF
   	IF ll_count > 0 THEN
	      MessageBox('Error','A claim status type must be supplied for the status code.')
			idw_dw[1].SetColumn('claim_status_type_code')
			idw_dw[1].SetFocus()
   	   Return -1
	   END IF
	END IF


/*	DUPLICATE CLAIM NO
	check duplicate of claim number
	must be 0 if claim status not Rejected and type not Claim error
	duplicate claim number must not be a duplicate claim number
	this claim cannot be registered as a duplicate
*/

	IF idw_dw[1].GetItemNumber(ll_current_row, 'duplicate_of_claim_no') <> 0 THEN
/*	first check to see if a dup claim number allowed
*/
   	IF idw_dw[1].GetItemString(ll_current_row, 'claim_status_code') = 'R' AND idw_dw[1].GetItemString(ll_current_row, 'claim_status_type_code') = '08' THEN
/*		'08' = Claim Error for status 'R' (Rejected)
		check and see if claim number is in claim and its duplicate is blank
*/
      	ll_no2 = idw_dw[1].GetItemNumber(ll_current_row, 'duplicate_of_claim_no')
	      IF ll_no2 = ll_claim_no THEN
   	      MessageBox('Duplicate Claim Error', 'The duplicate claim number cannot be the current claim.')
      	   Return -1
	      END IF
   	   SELECT duplicate_of_claim_no, claim_no, individual_no
      	  INTO :ll_no, :ll_no3, :ll_no4
	        FROM CLAIM
   	    WHERE claim_no = :ll_no2;
   		IF SQLCA.nf_handle_error( 'n_claims', 'nf_check_bus_rule','Embedded SQL: select from CLAIM') < 0 THEN
				Return -1
			END IF
      	IF NOT IsNull(ll_no3) AND ll_no3 > 0 THEN
	      // claim number found
	         IF ll_no <> 0 THEN
				/*	the claim is a duplicate itself
				*/
         	   MessageBox('Duplicate Claim Error','The claim number - ' + String(ll_no2) + '  is a duplicate for another claim.  This is not allowed.~r~nPlease correct.')
      	      Return -1
   	      END IF
	      ELSE
			/*	the claim does not exist in the claim table
			*/
      	   MessageBox('Invalid Claim Number', 'The claim number - ' + String(ll_no2) + ' does not exist in the claim table.~r~nPlease correct.')
   	      Return -1
	      END IF  
/*			the individual numbers cannot differ
*/
			IF ll_no4 <> idw_dw[1].GetItemNumber(ll_current_row,'individual_no') THEN
				MessageBox('Error','The claim you have entered as a duplicate is not for the same individual.  Please correct.')
				Return -1
			END IF

/*			also check that the current claim cannot be listed as a duplicate on another claim
*/
   	   SELECT  claim_no
	        INTO :ll_no
   		  FROM CLAIM
	       WHERE duplicate_of_claim_no = :ll_claim_no;
   		IF SQLCA.nf_handle_error( 'n_claims', 'nf_check_bus_rule','Embedded SQL: select from CLAIM') < 0 THEN
				Return -1
			END IF
   	   IF ll_no <> 0 THEN
/*			the claim is a duplicate itself on another claim
*/
         	MessageBox('Duplicate Claim Error','The current claim is listed as a duplicate for another claim - ' + String(ll_no) + '.  This is not allowed.~r~nPlease correct.')
      	   Return -1
   	   END IF
	  	ELSE
	      MessageBox('Invalid Status','A duplicate claim number can only be entered if the status is "Rejected" and the type is "Claim Error".~r~nPlease correct.')
      	Return -1
   	END IF
END IF		

Return 1
end function

public subroutine nf_firefighter_cost_alloc_change (long al_current_row, string as_cost_alloc_no);
DWObject l_dwo

l_dwo = idw_dw[1].object.cost_alloc_no

idw_dw[1].Modify("cost_alloc_no.Visible='1'")

idw_dw[1].SetColumn('cost_alloc_no')
idw_dw[1].SetText(as_cost_alloc_no)
idw_dw[1].AcceptText()

idw_dw[1].Trigger Event ItemChanged(al_current_row,l_dwo,as_cost_alloc_no)


idw_dw[1].Modify("cost_alloc_no.Visible='0'")

end subroutine

public subroutine nf_firefighter_cost_alloc_oper_change (long al_current_row, string as_cost_alloc_operation_no);DWObject l_dwo

l_dwo = idw_dw[1].object.cost_alloc_operation_no

idw_dw[1].Modify("cost_alloc_operation_no.Visible='1'")

idw_dw[1].SetColumn('cost_alloc_operation_no')
idw_dw[1].SetText(as_cost_alloc_operation_no)
idw_dw[1].AcceptText()

idw_dw[1].Trigger Event ItemChanged(al_current_row,l_dwo,as_cost_alloc_operation_no)

idw_dw[1].Modify("cost_alloc_operation_no.Visible='0'")

end subroutine

public function integer nf_create_firefighter_claim_master (long al_new_fca_claim_no);INTEGER      li_trancount
N_IMAGING    lnv_imaging

// create CLAIM_MASTER & FLD records in IMARA_DB database
lnv_imaging = Create n_imaging


ImageTrans.nf_begin_transaction()

/*	log the creation of the claim in imaging
*/
IF lnv_imaging.nf_create_claimsmaster('n_claims.nf_create_firefighter_claim', al_new_FCA_claim_no, 'Y') < 1 THEN
	SignalError(-1,'Could not create a new claim master record.')
	
	ImageTrans.nf_transaction_count(li_trancount,0,'','','',FALSE)
	IF li_trancount > 0 THEN
		ImageTrans.nf_rollback_transaction()
	END IF
ELSE
	ImageTrans.nf_commit_transaction()
END IF


RETURN 0

end function

public function integer nf_create_firefighter_claim_participant (long al_new_fca_claim_no, long al_individual_no, long al_rejected_wca_claim_no);INTEGER      li_claim_participant_row, li_SS_count
LONG         ll_SS_individual_no


li_claim_participant_row = idw_dw[13].InsertRow(0)
IF li_claim_participant_row > 0 THEN
	idw_dw[13].SetItem(li_claim_participant_row, 'claim_no',al_new_FCA_claim_no)
	idw_dw[13].SetItem(li_claim_participant_row, 'individual_no',al_individual_no)
	idw_dw[13].SetItem(li_claim_participant_row, 'claim_role_code','C')
	idw_dw[13].SetItem(li_claim_participant_row, 'sub_claim_no',0)
	idw_dw[13].SetItem(li_claim_participant_row, 'claimant_active_flag','Y')
ELSE
	SignalError(-1,'Could not create a new claim participant record for the claimant.')
END IF


SELECT Count(*)
INTO   :li_SS_count
FROM   CLAIM_PARTICIPANT
WHERE  claim_role_code = 'SS'
AND    claim_no = :al_rejected_WCA_claim_no
USING SQLCA;
SQLCA.nf_handle_error('n_claims', 'nf_create_firefighter_claim_participant', 'SELECT Count(*) FROM CLAIM_PARTICIPANT WHERE claim_role_code = SS ...')

IF li_SS_count > 0 THEN
	SELECT individual_no
	INTO   :ll_SS_individual_no
	FROM   CLAIM_PARTICIPANT
	WHERE  claim_role_code = 'SS'
	AND    claim_no = :al_rejected_WCA_claim_no
	USING SQLCA;
	SQLCA.nf_handle_error('n_claims', 'nf_create_firefighter_claim_participant', 'SELECT individual_no FROM CLAIM_PARTICIPANT WHERE claim_role_code = SS ...')
	
	li_claim_participant_row = idw_dw[13].InsertRow(0)
	IF li_claim_participant_row > 0 THEN
		idw_dw[13].SetItem(li_claim_participant_row, 'claim_no',al_new_FCA_claim_no)
		idw_dw[13].SetItem(li_claim_participant_row, 'individual_no',ll_SS_individual_no)
		idw_dw[13].SetItem(li_claim_participant_row, 'claim_role_code','SS')
		idw_dw[13].SetItem(li_claim_participant_row, 'sub_claim_no',1)
		idw_dw[13].SetItem(li_claim_participant_row, 'claimant_active_flag','N')
	ELSE
		SignalError(-1,'Could not create a new claim participant record for the surviving spouse.')
	END IF
END IF

RETURN 0

end function

public function integer nf_create_firefighter_claim_accident (long al_new_fca_claim_no, long al_rejected_wca_claim_no);INTEGER      li_claim_accident_row
STRING       ls_event_exposure_code, ls_source_of_injury_code, ls_second_source_of_injury_code
STRING       ls_original_nature_of_injury_code, ls_nature_of_injury_code, ls_original_part_of_body_code, ls_original_side_of_body_code, ls_occupation_code

li_claim_accident_row = idw_dw[14].InsertRow(0)
IF li_claim_accident_row > 0 THEN
	SELECT event_exposure_code,
			 source_of_injury_code,
			 second_source_of_injury_code,
			 original_nature_of_injury_code,
			 nature_of_injury_code,
			 original_part_of_body_code,
			 original_side_of_body_code,
			 occupation_code
	INTO   :ls_event_exposure_code,
			 :ls_source_of_injury_code,
			 :ls_second_source_of_injury_code,
			 :ls_original_nature_of_injury_code,
			 :ls_nature_of_injury_code,
			 :ls_original_part_of_body_code,
			 :ls_original_side_of_body_code,
			 :ls_occupation_code
	FROM   ACCIDENT
	WHERE  claim_no = :al_rejected_WCA_claim_no
	USING SQLCA;
	SQLCA.nf_handle_error('n_claims', 'nf_create_firefighter_claim', 'SELECT ... FROM ACCIDENT...')
	
	idw_dw[14].SetItem(li_claim_accident_row, 'claim_no',al_new_FCA_claim_no)
	idw_dw[14].SetItem(li_claim_accident_row, 'event_exposure_code',ls_event_exposure_code)
	idw_dw[14].SetItem(li_claim_accident_row, 'source_of_injury_code',ls_source_of_injury_code)
	idw_dw[14].SetItem(li_claim_accident_row, 'second_source_of_injury_code',ls_second_source_of_injury_code)
	idw_dw[14].SetItem(li_claim_accident_row, 'original_nature_of_injury_code',ls_original_nature_of_injury_code)
	idw_dw[14].SetItem(li_claim_accident_row, 'nature_of_injury_code',ls_nature_of_injury_code)
	idw_dw[14].SetItem(li_claim_accident_row, 'original_part_of_body_code',ls_original_part_of_body_code)
	idw_dw[14].SetItem(li_claim_accident_row, 'original_side_of_body_code',ls_original_side_of_body_code)
	idw_dw[14].SetItem(li_claim_accident_row, 'occupation_code',ls_occupation_code)
ELSE
	SignalError(-1,'Could not create a new claim accident record.')
END IF

RETURN 0

end function

public function integer nf_create_firefighter_claim (long al_new_fca_claim_no, long al_individual_no, long al_rejected_wca_claim_no);INTEGER      li_claim_row
STRING       ls_admin_region_code

li_claim_row = idw_dw[12].InsertRow(0)
IF li_claim_row > 0 THEN
	SELECT b.default_admin_region_code
	INTO   :ls_admin_region_code 
	FROM   INDIVIDUAL a
	JOIN   Location b on a.location_type_code = b.location_type_code
						  and a.location_code = b.location_code
	WHERE  individual_no = :al_individual_no
	USING SQLCA;
	SQLCA.nf_handle_error('n_claims', 'nf_create_firefighter_claim', 'SELECT default_admin_region_code	FROM INDIVIDUAL, Location...')
	
	idw_dw[12].SetItem(li_claim_row, 'claim_no',                        al_new_FCA_claim_no)
	idw_dw[12].SetItem(li_claim_row, 'claim_status_code',               'P')
	idw_dw[12].SetItem(li_claim_row, 'claim_status_type_code',          '')
	idw_dw[12].SetItem(li_claim_row, 'admin_region_code',               ls_admin_region_code)
	idw_dw[12].SetItem(li_claim_row, 'claim_manager_user_id',           idw_dw[1].GetItemString(1,'claim_manager_user_id'))

	idw_dw[12].SetItem(li_claim_row, 'individual_no',                   al_individual_no)
	idw_dw[12].SetItem(li_claim_row, 'administering_act_code',          'FCA')
	idw_dw[12].SetItem(li_claim_row, 'rejected_wca_claim_no',           al_rejected_WCA_claim_no)

	idw_dw[12].SetItem(li_claim_row, 'difficult_claim_flag',            idw_dw[1].GetItemString(1,'difficult_claim_flag'))
	idw_dw[12].SetItem(li_claim_row, 'imaged_flag',                     'Y')
	idw_dw[12].SetItem(li_claim_row, 'history_flag',                    'N')		

	idw_dw[12].SetItem(li_claim_row, 'accident_date',                   idw_dw[1].GetItemDateTime(1,'accident_date'))
	idw_dw[12].SetItem(li_claim_row, 'accident_county_code',            idw_dw[1].GetItemString(1,'accident_county_code'))
	idw_dw[12].SetItem(li_claim_row, 'accident_residence_county_code',  idw_dw[1].GetItemString(1,'accident_residence_county_code'))
	idw_dw[12].SetItem(li_claim_row, 'accident_employer_no',            idw_dw[1].GetItemNumber(1,'accident_employer_no'))
	idw_dw[12].SetItem(li_claim_row, 'accident_employer_operation_no',  idw_dw[1].GetItemNumber(1,'accident_employer_operation_no'))
	idw_dw[12].SetItem(li_claim_row, 'employer_type_code',              idw_dw[1].GetItemString(1,'employer_type_code'))
	idw_dw[12].SetItem(li_claim_row, 'part_of_body_code',               idw_dw[1].GetItemString(1,'part_of_body_code'))
	idw_dw[12].SetItem(li_claim_row, 'side_of_body_code',               idw_dw[1].GetItemString(1,'side_of_body_code'))
	idw_dw[12].SetItem(li_claim_row, 'classification_system_type_code', idw_dw[1].GetItemString(1,'classification_system_type_code'))
	idw_dw[12].SetItem(li_claim_row, 'classification_system_code',      idw_dw[1].GetItemString(1,'classification_system_code'))
ELSE
	// claim record not inserted
	SignalError(-1,'Could not create a new claim record.')
END IF

RETURN 0

end function

public function integer nf_create_firefighter_claim_events (long al_new_fca_claim_no, long al_rejected_wca_claim_no);INTEGER  li_counter, li_rowcount, li_new_row, li_rtn, li_max_event_no, li_next_event_no, ll_seq_no 
DATE     ldt_server_date
STRING  ls_event_comment 
U_DS     lds_rejected_wca_claim_events

ldt_server_date = Date(f_server_datetime())

// insert claim event for rejected WCA claim indicating that FCA claim has been auto-created
li_new_row = idw_dw[15].InsertRow(0)

SELECT IsNull(Max(event_no),0)
INTO   :li_max_event_no
FROM   CLAIM_EVENT
WHERE  claim_no = :al_rejected_wca_claim_no
USING SQLCA;
SQLCA.nf_handle_error('n_claims', 'nf_create_firefighter_claim_events', 'SELECT Max(event_no) FROM CLAIM_EVENT...')

li_next_event_no = li_max_event_no + 1

idw_dw[15].SetItem(li_new_row,'claim_no',al_rejected_wca_claim_no)
idw_dw[15].SetItem(li_new_row,'event_no',li_next_event_no)
idw_dw[15].SetItem(li_new_row,'event_date',ldt_server_date)
idw_dw[15].SetItem(li_new_row,'event_type_code','999')
idw_dw[15].SetItem(li_new_row,'event_specific_code','')
idw_dw[15].SetItem(li_new_row,'event_comment',"Firefighters' Compensation Act claim no "+String(al_new_fca_claim_no)+" auto-created")
idw_dw[15].SetItem(li_new_row,'event_category_code','C')

// insert create claim event for new FCA claim
li_new_row = idw_dw[15].InsertRow(0)
idw_dw[15].SetItem(li_new_row,'claim_no',al_new_fca_claim_no)
idw_dw[15].SetItem(li_new_row,'event_no',1)
idw_dw[15].SetItem(li_new_row,'event_date',ldt_server_date)
idw_dw[15].SetItem(li_new_row,'event_type_code','010')
idw_dw[15].SetItem(li_new_row,'event_specific_code','CC')
idw_dw[15].SetItem(li_new_row,'event_comment','Firefighter claim created from WCA rejected claim ' + String(al_rejected_wca_claim_no))
idw_dw[15].SetItem(li_new_row,'event_category_code','C')
idw_dw[15].SetItem(li_new_row,'copied_flag','N')

// INSERT a record into CLAIM_STATUS_CHANGE table 
SELECT MAX(seq_no)
  INTO :ll_seq_no
  FROM CLAIM_STATUS_CHANGE 
 WHERE claim_no = :al_new_fca_claim_no ; 

li_rtn = SQLCA.nf_handle_error('n_claims', '', 'nf_create_firefighter_claim_events - SELECT MAX(seq_no) FROM CLAIM_STATUS_CHANGE WHERE claim_no = :al_new_fca_claim_no') 

IF ll_seq_no = 0 OR IsNull(ll_seq_no) = TRUE THEN 
	ll_seq_no = 1
ELSE
	ll_seq_no = ll_seq_no + 1
END IF

INSERT CLAIM_STATUS_CHANGE (claim_no,            seq_no,     event_no, old_claim_status_code, old_claim_status_type_code, new_claim_status_code, new_claim_status_type_code) 
                    VALUES (:al_new_fca_claim_no,:ll_seq_no, 1,        ' ',                   ' ',                        'P',                   ' ') ; 

li_rtn = SQLCA.nf_handle_error('n_claims', '', 'nf_create_firefighter_claim_events - INSERT CLAIM_STATUS_CHANGE') 

// copy all claim events from the old WCA claim, excepts its create claim event
lds_rejected_wca_claim_events = Create U_DS
lds_rejected_wca_claim_events.DataObject = 'ds_rejected_wca_claim_events'
lds_rejected_wca_claim_events.SetTransObject(SQLCA)

li_rowcount = lds_rejected_wca_claim_events.Retrieve(al_rejected_wca_claim_no,li_next_event_no)
SQLCA.nf_handle_error('n_claims', 'nf_create_firefighter_claim_events', 'lds_rejected_wca_claim_events.Retrieve')

// the rejected WCA claim events will be renumbered to be starting at event_no = 2
FOR li_counter = 1 to li_rowcount
	li_new_row = idw_dw[15].InsertRow(0)
	IF li_new_row > 0 THEN
		idw_dw[15].SetItem(li_new_row,'claim_no',al_new_fca_claim_no)
		idw_dw[15].SetItem(li_new_row,'event_no',li_counter + 1)
		idw_dw[15].SetItem(li_new_row,'event_date',lds_rejected_wca_claim_events.GetItemDatetime(li_counter,'event_date'))
		idw_dw[15].SetItem(li_new_row,'event_type_code',lds_rejected_wca_claim_events.GetItemString(li_counter,'event_type_code'))
		idw_dw[15].SetItem(li_new_row,'event_specific_code',lds_rejected_wca_claim_events.GetItemString(li_counter,'event_specific_code'))

		ls_event_comment = f_clean_string_4(Trim(lds_rejected_wca_claim_events.GetItemString(li_counter,'event_comment')))

		// Add 'Copied from WCA claim - ' to comment
		IF Len(ls_event_comment) > 0 THEN
			ls_event_comment = 'Copied from WCA claim - ' + ls_event_comment
		ELSE
			ls_event_comment = 'Copied from WCA claim - '
		END IF

		idw_dw[15].SetItem(li_new_row,'event_comment',Trim(ls_event_comment))
		idw_dw[15].SetItem(li_new_row,'event_category_code','C')
		idw_dw[15].SetItem(li_new_row,'copied_flag','Y')
		idw_dw[15].SetItem(li_new_row,'create_date',lds_rejected_wca_claim_events.GetItemDatetime(li_counter,'create_date'))
		idw_dw[15].SetItem(li_new_row,'create_user_id',lds_rejected_wca_claim_events.GetItemString(li_counter,'create_user_id'))
	ELSE
		SignalError(-1,'Could not create new claim event records.')
	END IF
NEXT

RETURN 0
end function

public function integer nf_create_firefighter_difficult_claim (long al_new_fca_claim_no, long al_rejected_wca_claim_no);INTEGER  li_rowcount, li_new_row
U_DS     lds_rejected_wca_difficult_claim


lds_rejected_wca_difficult_claim = Create U_DS
lds_rejected_wca_difficult_claim.DataObject = 'ds_rejected_wca_difficult_claim'
lds_rejected_wca_difficult_claim.SetTransObject(SQLCA)
li_rowcount = lds_rejected_wca_difficult_claim.Retrieve(al_rejected_WCA_claim_no)
SQLCA.nf_handle_error('n_claims', 'nf_create_firefighter_difficult_claim', 'lds_rejected_wca_difficult_claim.Retrieve')

IF li_rowcount > 0 THEN
	li_new_row = idw_dw[16].InsertRow(0)
	IF li_new_row > 0 THEN
		idw_dw[16].SetItem(li_new_row, 'claim_no',                   al_new_fca_claim_no)
		idw_dw[16].SetItem(li_new_row, 'form_of_election_sent_flag', lds_rejected_wca_difficult_claim.GetItemString(1,'form_of_election_sent_flag'))
		idw_dw[16].SetItem(li_new_row, 'appeal_decision_flag',       lds_rejected_wca_difficult_claim.GetItemString(1,'appeal_decision_flag'))
		idw_dw[16].SetItem(li_new_row, 'fatality_flag',              lds_rejected_wca_difficult_claim.GetItemString(1,'fatality_flag'))
		idw_dw[16].SetItem(li_new_row, 'complex_adjudication_code',  lds_rejected_wca_difficult_claim.GetItemString(1,'complex_adjudication_code'))
	ELSE
		SignalError(-1,'Could not create a new difficult claim record.')
	END IF
END IF

RETURN 0
end function

public function integer nf_create_firefighter (long al_rejected_wca_claim_no, long al_individual_no, ref long al_new_fca_claim_no);INTEGER      li_rtn, li_counter
LONG         ll_error

al_new_FCA_claim_no = nf_get_next_claim()


IF al_new_FCA_claim_no > 0 THEN
	li_rtn = nf_create_firefighter_claim(al_new_FCA_claim_no,al_individual_no,al_rejected_WCA_claim_no)
	IF li_rtn < 0 THEN RETURN li_rtn
	
	li_rtn = nf_create_firefighter_claim_participant(al_new_FCA_claim_no,al_individual_no,al_rejected_WCA_claim_no)
	IF li_rtn < 0 THEN RETURN li_rtn
	
	li_rtn = nf_create_firefighter_claim_accident(al_new_FCA_claim_no,al_rejected_WCA_claim_no)
	IF li_rtn < 0 THEN RETURN li_rtn
	
	li_rtn = nf_create_firefighter_claim_events(al_new_FCA_claim_no,al_rejected_WCA_claim_no)
	IF li_rtn < 0 THEN RETURN li_rtn
	
	li_rtn = nf_create_firefighter_difficult_claim(al_new_FCA_claim_no,al_rejected_WCA_claim_no)
	IF li_rtn < 0 THEN RETURN li_rtn
ELSE
	// new claim_no not created
	SignalError(-1,'Could not obtain a new claim number.')
END IF


SQLCA.nf_begin_transaction()

// update dw's 12 to 16
// idw_dw[12] = ds_new_firefighter_claim
// idw_dw[13] = ds_new_firefighter_claim_participant
// idw_dw[14] = ds_new_firefighter_claim_accident
// idw_dw[15] = ds_rejected_wca_claim_events
// idw_dw[16] = ds_rejected_wca_difficult_claim
li_counter = 12
DO WHILE li_counter <= 16
	idw_dw[li_counter].Update()
   ll_error = SQLCA.nf_handle_error('n_claims', 'nf_create_firefighter_claim', 'update - idw_dw['+string(li_counter)+']')
   IF ll_error < 0 THEN
      Return ll_error
   END IF
	// Error Handling
	li_counter ++
LOOP

SQLCA.nf_commit_transaction()


li_rtn = nf_create_firefighter_claim_master(al_new_FCA_claim_no)
IF li_rtn < 0 THEN RETURN li_rtn

RETURN 0

end function

on n_claims.create
call super::create
end on

on n_claims.destroy
call super::destroy
end on

event constructor;call super::constructor;inv_claim_employer = create n_claim_employer
inv_reminders 	= create n_reminders
end event

