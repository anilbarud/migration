$PBExportHeader$n_awards.sru
forward
global type n_awards from n_pdc
end type
end forward

global type n_awards from n_pdc
end type
global n_awards n_awards

type variables
LONG il_claim_no
U_DW_ONLINE idw_basic_claim
DECIMAL ic_authorization_limit
BOOLEAN ib_unpaid_authorizable_award

STRING		is_mode
end variables

forward prototypes
public function integer nf_set_unused_fields ()
public subroutine nf_set_claim_no (long al_claim_no)
public function integer nf_retrieve_openings ()
public function integer nf_retrieve_benefit ()
public function integer nf_set_recipient_defaults ()
public function long nf_set_identifiers ()
public function boolean nf_is_valid_status ()
public function integer nf_calculate_age (date adt_birth_date)
public function integer nf_check_openings (datawindowchild adwc_openings, string as_type)
public function boolean nf_is_valid_status_act ()
public function integer nf_set_benefit_filter ()
public function integer nf_validate_cost_alloc ()
public function integer nf_set_basic_claim (datawindow adw_basic_claim)
public function integer nf_set_award_filter (long al_opening_no, long al_benefit_calculation_no)
public function integer nf_check_dup_recipients ()
public function integer nf_retrieve (long al_claim_no, long al_award_no)
public function integer nf_get_next_identifier (long al_claim_no)
public function integer nf_setup_benefit_info (integer ai_benefit_calculation_no, ref decimal ac_benefit_level_percentage)
public function integer nf_retrieve_recipients ()
public function integer nf_set_recipient_filter (boolean as_filter_on)
public function datetime nf_get_end_date (datetime adtm_award_start_date, long al_no_of_periods, string as_frequency_code)
public function decimal nf_get_authorization_limit (string as_type)
public function integer nf_setup_address (string as_recipient_type_code, long al_recipient_no)
public function integer nf_insert (long al_row)
public function integer uof_check_age (long al_recipientno, long al_claim, date ad_enddate, string as_opening)
public function integer nf_check_mandatory ()
public function integer nf_validate_claim ()
public function boolean nf_is_award_processed ()
public function integer nf_allow_modification ()
public function integer nf_delete ()
public function integer nf_delete_recipient ()
public function string nf_get_authorization_filter ()
public function integer nf_insert_recipient (integer al_row)
public subroutine nf_set_authorization (boolean ab_unpaid_authorizable_award)
public function integer nf_validate_payment_type (string as_payment_type_code, ref integer ai_error_level, ref string as_error_message, ref string as_award_freq_code, ref string as_days_required)
public subroutine nf_set_payment_filter (long al_opening_no, long al_benefit_calculation_no, string as_award_type)
public function integer nf_set_defaults ()
public function integer nf_setup_default (integer ai_benefit_calculation_no, decimal adec_benefit_level_percentage, integer ai_opening_no)
public function integer nf_change_item (long al_datawindow)
public function integer nf_check_bus_rule ()
public function integer nf_validate_recipient (string as_recipient_type_code, long al_recipient_no, long al_row, string as_award_type_code)
public function integer nf_get_bank (long al_recipient_no, string as_recipient_type_code, ref string as_bank_no, ref string as_bank_transit_no, ref string as_bank_account_no, ref string as_name)
end prototypes

public function integer nf_set_unused_fields ();LONG  ll_row, ll_loop, ll_max

	ll_row = idw_dw[1].GetRow()

	IF ll_row > 0 THEN
		IF IsNull(idw_dw[1].GetItemString(1,'explanation')) THEN
			idw_dw[1].SetItem(1,'explanation','')
		END IF
	END IF
	ll_max = idw_dw[2].RowCount()
	ll_loop = 1
	DO WHILE ll_loop <= ll_max
		IF IsNull(idw_dw[2].GetItemString(ll_loop,'recipient_sub_type_code')) THEN
			idw_dw[2].SetItem(ll_loop,'recipient_sub_type_code','')
		END IF
		ll_loop = ll_loop + 1
	LOOP
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
	//	ldwc_child.SetItem(1,'max_opening_no',0)
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
	ll_row = ldwc_child.InsertRow(1)
	IF ll_row > 0 THEN
/*	this is for payment types that do not need a benefit calc no
*/
		ldwc_child.SetItem(1,'benefit_calculation_no', 0)
	END IF
Return ll_row






end function

public function integer nf_set_recipient_defaults ();DATE		ldt_null
LONG     ll_transaction_rowcount, ll_tranrow, ll_counter
DECIMAL  ldec_sum_txn, ldec_total_payment_amount

	SetNull(ldt_null)
	ll_transaction_rowcount = idw_dw[2].RowCount()
	ll_tranrow = idw_dw[2].GetRow()

/* Setup Defaults                                                                      
*/
	idw_dw[2].SetItem(ll_tranrow,'txn_amount',0)
	idw_dw[2].SetItem(ll_tranrow,"recipient_type_code","V")
	idw_dw[2].SetItem(ll_tranrow,"payment_method_code","A")
	nf_set_recipient_filter(TRUE)
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

Return 0
end function

public function long nf_set_identifiers ();LONG	ll_award_no, ll_claim_no, ll_row, ll_max
STRING ls_award_type_code



	ll_claim_no = idw_basic_claim.GetItemNumber(1,'claim_no')

	idw_dw[1].SetItem(1,'claim_no', ll_claim_no)
/*	get next award number
*/

	IF IsNull(idw_dw[1].GetItemNumber(1,'award_no')) THEN
		ll_award_no = nf_get_next_identifier(ll_claim_no)
	ELSE
		ll_award_no = idw_dw[1].GetItemNumber(1,'award_no')
	END IF
	IF ll_award_no < 0 THEN
		MessageBox('Error','Unable to determine next award number.')
		Return -1
	END IF
	
		idw_dw[1].SetItem(1,'award_no', ll_award_no)
	
	ll_max = idw_dw[2].RowCount()
	ll_row = 1

	DO WHILE ll_row <= ll_max
		idw_dw[2].SetItem(ll_row,'claim_no', ll_claim_no)
		idw_dw[2].SetItem(ll_row,'award_no', ll_award_no)
		ll_row = ll_row + 1
	LOOP
	
	//P10151-286 for award type of 'CA' and 'CL' , force the opening number to be a zero (0)
	ls_award_type_code =  idw_dw[1].GetItemString(1,'award_type_code') 
	IF ls_award_type_code = 'CA' or ls_award_type_code = 'CL' THEN
		idw_dw[1].SetItem(1,'opening_no', 0)
	END IF
	
Return 0
end function

public function boolean nf_is_valid_status ();IF idw_basic_claim.GetItemString(1,'claim_status_code') = 'A' OR (idw_basic_claim.GetItemString(1,'claim_status_code') = 'F' AND (idw_basic_claim.GetItemString(1,'claim_status_type_code') = '01' OR idw_basic_claim.GetItemString(1,'claim_status_type_code') = '02')) THEN
	Return TRUE
ELSE
	Return FALSE
END IF

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

public function integer nf_check_openings (datawindowchild adwc_openings, string as_type);DATETIME	ldtm_server_date
LONG		ll_row
	ldtm_server_date = f_server_datetime()
	ll_row =  adwc_openings.Find('opening_type_code = "' + as_type + '" AND (IsNull(benefit_end_date) OR String(Date(benefit_end_date), "yyyy-mm-dd") > "' + String(Date(ldtm_server_date),'yyyy-mm-dd') + '")', 0, adwc_openings.RowCount())
Return ll_row
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
	
	ldwc_child.SetSort('benefit_calculation_no A')
	ldwc_child.Sort()
Return 0

end function

public function integer nf_validate_cost_alloc ();Long ll_result, ll_cost_alloc_no, ll_cost_alloc_operation_no

ll_cost_alloc_no = idw_basic_claim.GetItemNumber(1,'cost_alloc_no')
ll_cost_alloc_operation_no = idw_basic_claim.GetItemNumber(1,'cost_alloc_operation_no')

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
	MessageBox('Warning No Payments Allowed', "Claim has an invalid Employer/Operation ( " +&
				  String(ll_cost_alloc_no) + "/" + String(ll_cost_alloc_operation_no) + " )" )
	Return -1
END IF

Return 0
end function

public function integer nf_set_basic_claim (datawindow adw_basic_claim);	idw_basic_claim = adw_basic_claim

Return 0
end function

public function integer nf_set_award_filter (long al_opening_no, long al_benefit_calculation_no);DATAWINDOWCHILD  	ldwc_child
STRING          	ls_filter, ls_opening_type_code , ls_rtw_incentive_flag, ls_award_type_code
LONG					ll_row, ll_no
DECIMAL				ldec_benefit_level

/*	add a filter to get only payment types valid for this screen
*/

//If the window is in read only mode, there is not need to set filters since the
//user can't modify anything anyway. It also caused an application error if the user
//has no authorizations.
IF is_mode = 'READ' THen return 1

	IF IsNull(al_opening_no) THEN
		al_opening_no = 0
	END IF

	IF IsNull(al_benefit_calculation_no) THEN
		al_benefit_calculation_no = 0
	END IF

	ll_row = idw_dw[1].GetRow()
/*	I was having a problem with GetRow() - sometimes it would return 0 when there was a row???
	I haven't figured it out so I have hard coded it to one
*/
	IF ll_row = 0 and idw_dw[1].RowCount() > 0 THEN
		ll_row = 1
	END IF
	IF ll_row > 0 THEN
		ls_filter = nf_get_authorization_filter()
		IF ls_filter = '' THEN
//			MessageBox('No Authorization','You have no authorization for this region.')
			Return -1
		END IF

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
		ELSE
			ls_opening_type_code = 'I'
		END IF
		IF ls_opening_type_code > '' THEN
			ls_filter = ls_filter + ' AND opening_type_code = "' + ls_opening_type_code + '"'
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
				ls_rtw_incentive_flag = ldwc_child.GetItemString(ll_row,'rtw_incentive_flag' )
				
				SELECT	award_type_code
				INTO		:ls_award_type_code
				FROM		PERIODIC_AWARD
				WHERE	claim_no						= :il_claim_no
				AND		benefit_calculation_no	= :al_benefit_calculation_no
				USING SQLCA;
				
				SQLCA.nf_handle_error('n_awards','Embedded SQL: SELECT from PERIODIC_AWARD_CONTROL','nf_set_award_filter')
				
				IF ls_rtw_incentive_flag = 'Y' THEN
					IF ls_award_type_code <> 'RWI' THEN
						idw_dw[1].SetColumn('award_type_code')
						idw_dw[1].SetText('')
						idw_dw[1].SetColumn('payment_type_code')
						idw_dw[1].SetText('')
						idw_dw[1].SetColumn('award_type_code')
					END IF
					// there are records in BENEFIT_CALC_RTW_INCENTIVE_XREF
					// so limit award type to "Return to Work Incentive"
					ls_filter = ls_filter + ' AND award_type_code = "RWI"'
				ELSE
					IF ls_award_type_code = 'RWI' THEN
						idw_dw[1].SetColumn('award_type_code')
						idw_dw[1].SetText('')
						idw_dw[1].SetColumn('payment_type_code')
						idw_dw[1].SetText('')
						idw_dw[1].SetColumn('award_type_code')
					END IF
					ls_filter = ls_filter + ' AND award_type_code <> "RWI"'
				END IF
					
					
			END IF
		END IF

		ls_filter = ls_filter + ' AND benefit_level_percentage = ' + String(ldec_benefit_level) + ' AND award_type_code <> "I"'
		idw_dw[1].GetChild('award_type_code',ldwc_child)
		ldwc_child.SetFilter(ls_filter)
		ldwc_child.Filter()
	END IF
Return 0

end function

public function integer nf_check_dup_recipients ();LONG		ll_row, ll_max, ll_prev_no
STRING	ls_prev_type

/*	check to make sure no duplicate recipients
*/
	idw_dw[2].SetRedraw(FALSE)
	idw_dw[2].SetSort('recipient_type_code A, recipient_no A')
	idw_dw[2].Sort()
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

public function integer nf_retrieve (long al_claim_no, long al_award_no);LONG					ll_rows, ll_tran_row, ll_cnt, ll_recipient_no, ll_result
DATAWINDOWCHILD	ldwc_child
STRING				ls_b_no, ls_t_no, ls_a_no, ls_recipient_type_code
STRING				ls_recipient_city, ls_recipient_province, ls_recipient_country, ls_recipient_postal_code
STRING				ls_recipient_name, ls_address_line1, ls_address_line2


	nf_retrieve_openings()
	nf_retrieve_benefit()
	ll_rows = idw_dw[1].Retrieve(al_claim_no, al_award_no)

	IF ll_rows > 0 THEN
		nf_set_benefit_filter()
		nf_set_award_filter(idw_dw[1].GetItemNumber(1,'opening_no'), idw_dw[1].GetItemNumber(1,'benefit_calculation_no'))
		nf_set_payment_filter(idw_dw[1].GetItemNumber(1,'opening_no'), idw_dw[1].GetItemNumber(1,'benefit_calculation_no'), idw_dw[1].GetItemString(1,'award_type_code'))
		idw_dw[1].GetChild('benefit_calculation_no',ldwc_child)
		ll_rows = ldwc_child.Find('benefit_calculation_no = ' + String(idw_dw[1].GetItemNumber(1,'benefit_calculation_no')),1,ldwc_child.RowCount())
		IF ll_rows > 0 THEN
			idw_dw[1].SetItem(1,"benefit_level_percentage",ldwc_child.GetItemDecimal(ll_rows,"benefit_level_percentage"))
			IF ldwc_child.GetItemString(ll_rows,"award_freq_code") = "W" THEN
				idw_dw[1].SetItem(1,"week_or_month" , "/week")
			ELSE
				idw_dw[1].SetItem(1,"week_or_month" ,"/month")
			END IF
			IF IsNull(ldwc_child.GetItemDateTime(ll_rows,"effective_from_date")) THEN
				idw_dw[1].SetItem(1,"effective_from_date",'')
			ELSE
				idw_dw[1].SetItem(1,"effective_from_date","Effective Date: " + string(ldwc_child.GetItemDateTime(ll_rows,"effective_from_date"),'yyyy-mm-dd'))
			END IF
		END IF
/*		retrieve the transaction details
*/
		nf_retrieve_recipients()
		ll_tran_row = idw_dw[2].Retrieve(al_claim_no, al_award_no)
		IF ll_tran_row > 0 THEN
			IF idw_dw[2].GetItemString(1,'recipient_type_code') = 'I' THEN
				nf_set_recipient_filter(FALSE)
			ELSE
				nf_set_recipient_filter(TRUE)
			END IF
			ll_cnt = 1
			DO WHILE ll_cnt <= ll_tran_row

		   	IF idw_dw[2].GetItemString(ll_cnt,'payment_method_code') = 'D' THEN
					nf_get_bank(idw_dw[2].GetItemNumber(ll_cnt,'recipient_no'),idw_dw[2].GetItemString(ll_cnt,'recipient_type_code') ,ls_b_no, ls_t_no, ls_a_no, ls_recipient_name)
					idw_dw[2].SetItem(ll_cnt,'recipient_name', ls_recipient_name)
					idw_dw[2].SetItem(ll_cnt,'bank_no', ls_b_no)
					idw_dw[2].SetItem(ll_cnt,'bank_transit_no', ls_t_no)
					idw_dw[2].SetItem(ll_cnt,'bank_account_no', ls_a_no)
				ELSE
/*				this was added because I was having trouble determining the 'current_row' when I called
				nf_setup_address - for now this will do
*/
					ls_recipient_type_code = idw_dw[2].GetItemString(ll_cnt,'recipient_type_code')
					ll_recipient_no = idw_dw[2].GetItemNumber(ll_cnt,'recipient_no')
					IF ls_recipient_type_code = 'I' THEN
						SELECT given_names + ' ' + last_name,
						       address_line1,
								 address_line2,   
		   		          city,
								 prov_state_code,
								 country_code,
								 postal_code  
			   		INTO   :ls_recipient_name,
						       :ls_address_line1,
								 :ls_address_line2,
								 :ls_recipient_city, 
   			   	       :ls_recipient_province,
								 :ls_recipient_country,
								 :ls_recipient_postal_code  
			   		FROM   INDIVIDUAL
					   WHERE  individual_no = :ll_recipient_no
	   			   USING SQLCA ;
			
					   SQLCA.nf_handle_error("Embedded SQL: Retrieve on INDIVIDUAL","n_payment","nf_setup_address")
					ELSE
					   SELECT name,
						       address_line1,
								 address_line2,   
		   		          city,
								 prov_state_code,
								 country_code,
								 postal_code  
				   	INTO   :ls_recipient_name,
						       :ls_address_line1,
								 :ls_address_line2,
								 :ls_recipient_city, 
   				          :ls_recipient_province,
								 :ls_recipient_country,
								 :ls_recipient_postal_code  
				   	 FROM  PROVIDER  
					    WHERE provider_no        = :ll_recipient_no
						 AND   provider_type_code = :ls_recipient_type_code
						 AND   active_flag        = 'Y'
	   			    USING SQLCA ;
					   SQLCA.nf_handle_error("Embedded SQL: Retrieve on SERVICE_PROVIDER","n_awards","nf_setup_address")
					END IF
					
					idw_dw[2].SetItem(ll_cnt,"recipient_name",ls_recipient_name)
		   		idw_dw[2].SetItem(ll_cnt,"address_line1",ls_address_line1)
				   idw_dw[2].SetItem(ll_cnt,"address_line2",ls_address_line2)
			   	idw_dw[2].SetItem(ll_cnt,"city",ls_recipient_city)
	   			idw_dw[2].SetItem(ll_cnt,"prov_state_code",ls_recipient_province)
					
					SELECT location_desc1
					  INTO :ls_recipient_country
					  FROM Location
					 WHERE location_code = :ls_recipient_country
					   AND location_type_code = 'C';
	
					SQLCA.nf_handle_error('Embedded SQL: Select from Location','n_awards','nf_setup_address')
					
		   		idw_dw[2].SetItem(ll_cnt,"country",ls_recipient_country)
	   		   idw_dw[2].SetItem(ll_cnt,"postal_code",ls_recipient_postal_code)

				END IF
				ll_cnt = ll_cnt + 1
			LOOP
		END IF
	ELSE
		Return -1
	END IF

Return ll_rows
end function

public function integer nf_get_next_identifier (long al_claim_no);LONG  ll_award_no

	SELECT IsNull(Max(award_no),0)
	  INTO :ll_award_no
	  FROM PERIODIC_AWARD
	 WHERE claim_no = :al_claim_no;

	IF SQLCA.nf_handle_error('n_awards', 'nf_get_next_identifier', '') < 0 THEN
   	Return -1
	ELSE
   	Return (ll_award_no + 1)
	END IF

end function

public function integer nf_setup_benefit_info (integer ai_benefit_calculation_no, ref decimal ac_benefit_level_percentage);DATAWINDOWCHILD ldwc_child
LONG     ll_benefit_rownum
STRING   ls_effective_date, ls_award_freq_code
DECIMAL	ldec_award_amount

/* Function Name: nf_setup_benefit_info                                                              
                                                                                                  
	 Purpose:       The purpose of this module is to setup the benefit information for the benefit        
   	             calculation number passed                                                             
                                                                                                      
	 Arguments:     ai_benefit_calculation_no                                                            
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
		idw_dw[1].SetItem(1,"week_or_month" , "/week")
	   ldwc_child.SetItem(ll_benefit_rownum,"award_freq_code","W")
		idw_dw[1].SetItem(1,'total_award_amount',0)
		idw_dw[1].SetItem(1,'total_deductions',0)
		idw_dw[1].SetItem(1,'total_payment_amount',0)
		ac_benefit_level_percentage = 0
	ELSE
		ac_benefit_level_percentage = ldwc_child.GetItemDecimal(ll_benefit_rownum,"benefit_level_percentage")
		ldec_award_amount = ldwc_child.GetItemDecimal(ll_benefit_rownum,'award_amount')
		ls_award_freq_code = ldwc_child.GetItemString(ll_benefit_rownum,"award_freq_code")
		IF ls_award_freq_code = "W" THEN
			idw_dw[1].SetItem(1,"week_or_month" , "/week")
		ELSE
			idw_dw[1].SetItem(1,"week_or_month" ,"/month")
		END IF
		IF IsNull(ldwc_child.GetItemDateTime(ll_benefit_rownum,"effective_from_date")) THEN
			ls_effective_date = ""
		ELSE
			ls_effective_date = "Effective Date: " + string(ldwc_child.GetItemDateTime(ll_benefit_rownum,"effective_from_date"),'yyyy-mm-dd')
		END IF
		idw_dw[1].SetItem(1,"effective_from_date",ls_effective_date)
		idw_dw[1].SetItem(1,"benefit_level_percentage",ac_benefit_level_percentage)
		idw_dw[1].SetItem(1,'total_award_amount',ldec_award_amount)
		idw_dw[1].SetItem(1,'total_deductions',0)
		idw_dw[1].SetItem(1,'total_payment_amount',0)
	END IF


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

public function integer nf_set_recipient_filter (boolean as_filter_on);DATAWINDOWCHILD	ldwc_child, ldwc_child2

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

public function datetime nf_get_end_date (datetime adtm_award_start_date, long al_no_of_periods, string as_frequency_code);//	nf_get_end_date - returns the award_end_date for a PERIOD_AWARD record based on the values
//	                  of it's award_start_date and the number and frequency of payments.
//
//	Argument: adtm_award_start_date - The start date of the award.
//           al_no_of_periods  - The number of periods the award will be paid for.
//           as_frequency_code - The frequency that the award will be paid (Monthly / Annually).
//
Integer li_rtn
Long    ll_junk 
Date    ld_end_date

ld_end_date = Date(adtm_award_start_date)

IF as_frequency_code = 'A' THEN
	SELECT last_appointment_no, dateadd(yyyy, :al_no_of_periods, :adtm_award_start_date) 
	  INTO :ll_junk, :ld_end_date 
	  FROM Last_Appointment_No ; 
	  
	li_rtn = SQLCA.nf_handle_error("n_awards", "", "nf_get_end_date - SELECT last_appointment_no, dateadd(yyyy, :al_no_of_periods, :adtm_award_start_date) FROM Last_Appointment_No")
ELSE
	SELECT last_appointment_no, dateadd(mm, :al_no_of_periods, :adtm_award_start_date) 
	  INTO :ll_junk, :ld_end_date 
	  FROM Last_Appointment_No ; 

	li_rtn = SQLCA.nf_handle_error("n_awards", "", "nf_get_end_date - SELECT last_appointment_no, dateadd(yyyy, :al_no_of_periods, :adtm_award_start_date) FROM Last_Appointment_No")
END IF

RETURN Datetime(ld_end_date)

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

public function integer nf_setup_address (string as_recipient_type_code, long al_recipient_no);LONG             ll_result, ll_tranrow
STRING			  ls_address_line1, ls_address_line2
STRING           ls_recipient_city, ls_recipient_province
STRING           ls_recipient_country, ls_recipient_postal_code, ls_recipient_name
DATAWINDOWCHILD  ldwc_child

/* Function Name: nf_setup_address                                                                      

	Purpose:       The purpose of this function is to load the current transaction record with default   
		            address information                                                                   

	Arguments:     as_recipient_type_code - identifies the recipient type so we know where to go to get 
						the address                                                 
						al_recipient_no - if the recipient is not the claimant or employer, this contains    
						the payee number                                                   

	Return Values: n/a                                                                                   
*/
	ll_tranrow = idw_dw[2].GetRow()

	CHOOSE CASE as_recipient_type_code
/* If recipient type code empty, we just want to blank out the address and get out
*/
	   CASE ""
			idw_dw[2].SetItem(ll_tranrow,"recipient_name","")
		   idw_dw[2].SetItem(ll_tranrow,"address_line1","")
   		idw_dw[2].SetItem(ll_tranrow,"address_line2","")
		   idw_dw[2].SetItem(ll_tranrow,"city","")
   	 	idw_dw[2].SetItem(ll_tranrow,"prov_state_code","")
    		idw_dw[2].SetItem(ll_tranrow,"country","")
	      idw_dw[2].SetItem(ll_tranrow,"postal_code","")
/* 		If recipient is claimant, get the address from the basic claim info            
*/
	   CASE "I"
			IF al_recipient_no = 0 THEN
	   	   idw_dw[2].SetItem(ll_tranrow,"recipient_name", & 
            (idw_basic_claim.GetItemString(1,'given_names') + ' ' + idw_basic_claim.GetItemString(1,'last_name')))
	   	   idw_dw[2].SetItem(ll_tranrow,"address_line1",idw_basic_claim.GetItemString(1,'address_line1'))
   	   	idw_dw[2].SetItem(ll_tranrow,"address_line2",idw_basic_claim.GetItemString(1,'address_line2'))
	   	   idw_dw[2].SetItem(ll_tranrow,"city",idw_basic_claim.GetItemString(1,'city'))
   	   	idw_dw[2].SetItem(ll_tranrow,"prov_state_code",idw_basic_claim.GetItemString(1,'prov_state_code'))
	      	ls_recipient_country = idw_basic_claim.GetItemString(1,'country_code')
/*				need to get the description
*/
  			   idw_basic_claim.GetChild('country_code', ldwc_child)
   	   	ll_result = ldwc_child.Find('location_code = "' + ls_recipient_country + '"', 1, ldwc_child.RowCount())
	   	   IF ll_result > 0 THEN
   	   	   ls_recipient_country = ldwc_child.GetItemString(ll_result, 'location_desc1')
	      	END IF
		      idw_dw[2].SetItem(ll_tranrow,"country",ls_recipient_country)
   		   idw_dw[2].SetItem(ll_tranrow,"postal_code",idw_basic_claim.GetItemString(1,'postal_code'))
			ELSE
				SELECT given_names + ' ' + last_name,
				       address_line1,
						 address_line2,   
		             city,
						 prov_state_code,
						 country_code,
						 postal_code  
	   		INTO   :ls_recipient_name,
				       :ls_address_line1,
						 :ls_address_line2,
						 :ls_recipient_city, 
   	   	       :ls_recipient_province,
						 :ls_recipient_country,
						 :ls_recipient_postal_code  
			   FROM   INDIVIDUAL
			   WHERE  individual_no = :al_recipient_no
	   	   USING SQLCA ;
			
			   ll_result = SQLCA.nf_handle_error("Embedded SQL: Retrieve on INDIVIDUAL","n_payment","nf_setup_address")
			   IF ll_result = 100 THEN
			   	Return 100
		   	END IF
				idw_dw[2].SetItem(ll_tranrow,"recipient_name",ls_recipient_name)
   			idw_dw[2].SetItem(ll_tranrow,"address_line1",ls_address_line1)
			   idw_dw[2].SetItem(ll_tranrow,"address_line2",ls_address_line2)
	   		idw_dw[2].SetItem(ll_tranrow,"city",ls_recipient_city)
		   	idw_dw[2].SetItem(ll_tranrow,"prov_state_code",ls_recipient_province)
				SELECT location_desc1
				  INTO :ls_recipient_country
				  FROM Location
				 WHERE location_code = :ls_recipient_country
				   AND location_type_code = 'C';

				IF SQLCA.nf_handle_error('Embedded SQL: Select from Location','n_payment','nf_setup_address') < 0 THEN
					Return -1
				END IF
   			idw_dw[2].SetItem(ll_tranrow,"country",ls_recipient_country)
		      idw_dw[2].SetItem(ll_tranrow,"postal_code",ls_recipient_postal_code)

			END IF

	   CASE ELSE
/*		If recipient is a payee, read the address from Service Providers table          
*/
			IF IsNull(as_recipient_type_code) OR IsNull(al_recipient_no) THEN
				MessageBox('Warning','Recipient number or type is missing')
				Return -1
			END IF
		   SELECT name,
			       address_line1,
					 address_line2,   
	             city,
					 prov_state_code,
					 country_code,
					 postal_code  
	   	INTO   :ls_recipient_name,
			       :ls_address_line1,
					 :ls_address_line2,
					 :ls_recipient_city, 
   	          :ls_recipient_province,
					 :ls_recipient_country,
					 :ls_recipient_postal_code  
		   FROM   PROVIDER  
		   WHERE  provider_no        = :al_recipient_no
			AND    provider_type_code = :as_recipient_type_code
			AND    active_flag        = 'Y'
	      USING SQLCA ;
		   ll_result = SQLCA.nf_handle_error("Embedded SQL: Retrieve on SERVICE_PROVIDER","n_awards","nf_setup_address")
		   IF ll_result = 100 THEN
			   Return 100
	   	END IF

	   	idw_dw[2].SetItem(ll_tranrow,"recipient_name",ls_recipient_name)
			idw_dw[2].SetItem(ll_tranrow,"address_line1",ls_address_line1)
		   idw_dw[2].SetItem(ll_tranrow,"address_line2",ls_address_line2)
	   	idw_dw[2].SetItem(ll_tranrow,"city",ls_recipient_city)
	   	idw_dw[2].SetItem(ll_tranrow,"prov_state_code",ls_recipient_province)
			
			SELECT location_desc1
			INTO   :ls_recipient_country
			FROM   Location
			WHERE  location_code      = :ls_recipient_country
			AND    location_type_code = 'C';
			SQLCA.nf_handle_error('Embedded SQL: Select from Location','n_awards','nf_setup_address')
			
   		idw_dw[2].SetItem(ll_tranrow,"country",ls_recipient_country)
	      idw_dw[2].SetItem(ll_tranrow,"postal_code",ls_recipient_postal_code)
	END CHOOSE	
Return 0
end function

public function integer nf_insert (long al_row);	nf_retrieve_openings()
	idw_dw[1].Reset()
	nf_retrieve_benefit()
	idw_dw[1].InsertRow(al_row)
	
	idw_dw[2].Reset()
	nf_retrieve_recipients()
	idw_dw[2].InsertRow(al_row)
	
	nf_set_defaults()

Return 0
end function

public function integer uof_check_age (long al_recipientno, long al_claim, date ad_enddate, string as_opening);string ls_claimanttype,ls_dependant,ls_day,ls_year,ls_month,ls_legistration
date ld_birth,ld_monthend
datetime ldt_birth
integer li_age

//Only do checks if there is a birth date.

SELECT ISNULL(birth_date,'01/01/1800')
INTO :ldt_birth 
FROM INDIVIDUAL
WHERE individual_no = :al_recipientno;

ld_birth = date(ldt_birth)

IF ld_birth > date('01/01/1801') THEN
	SELECT claim_role_code ,dependent_reason_code
	INTO :ls_claimantType,:ls_dependant
	FROM CLAIM_PARTICIPANT
	WHERE individual_no = :al_recipientno
	AND claim_no = :al_claim;	
	ls_day = '01'
	ls_year = string(YEAR(ad_enddate))
	IF MONTH(date(ld_birth)) + 1 = 13 then
		ls_month = '01'
		ls_year = string(integer(ls_year) + 1)
	else
		ls_month = string(MONTH(date(ld_birth))+ 1)
	end if
	
	ld_monthend = date(ls_year + '/' + ls_month  + '/' + ls_day )
	ld_monthend = relativedate(ld_monthend,-1)
		
	li_age = integer((daysafter(ld_birth,ld_monthend)/365.25))
	
	CHOOSE CASE ls_claimantType
		case 'C'
			if as_opening = 'RLOE' or as_opening = 'LTD' then
				if li_age >= 65 then
					if messagebox("Payment Warning","The age of this individual is greater then 65. Do you wish to continue?",Question!,yesno!,2) = 2 then 
						Return -1
					else
						return 1
					end if
				end if
			end if
		case 'SS'
			SELECT legislation_code
			INTO :ls_legistration
			FROM CLAIM
			WHERE claim_no = :al_claim; 
			if as_opening = 'S1' or as_opening = 'S2' or as_opening = 'SV' then
				IF ls_legistration = 'P81' or ls_legistration = 'P97' then	
					if li_age >= 65 then
						if messagebox("Payment Warning","The age of this individual is greater then 65. Do you wish to continue?",Question!,yesno!,2) = 2 then 
							Return -1
						else
							return 1
						end if
					end if
				end if
			end if
		case 'DC'
			SELECT legislation_code
			INTO :ls_legistration
			FROM CLAIM
			WHERE claim_no = :al_claim; 
			
			if as_opening = 'S1' or as_opening = 'S2' or as_opening = 'SV' then
				IF ls_legistration = 'P81' or ls_legistration = 'P97' and ls_dependant <> 'DIS' then	
					if li_age >= 22 then
						if messagebox("Payment Warning","The age of this dependant is greater then 22. Do you wish to continue?",Question!,yesno!,2) = 2 then 
							Return -1
						else
							return 1
						end if
					end if
				end if
			end if
	END CHOOSE
END IF
return 1
end function

public function integer nf_check_mandatory ();DATAWINDOWCHILD  ldwc_child
LONG             ll_current_row, ll_result, ll_transaction_rowcount, ll_row
STRING           ls_payment_method_code, ls_string


	IF idw_dw[1].AcceptText() < 0 THEN Return -1		// payment details
	IF idw_dw[2].AcceptText() < 0 THEN Return -1		// transaction details

	ll_current_row = idw_dw[1].GetRow()
	IF ll_current_row > 0 THEN

		IF IsNull(idw_dw[1].GetItemDecimal(ll_current_row,'total_award_amount')) THEN
			MessageBox('Missing Award Amount', 'A value must be specified for the award amount.')
			idw_dw[1].SetFocus()
			idw_dw[1].SetColumn('total_award_amount')
			Return -1
		END IF

		IF IsNull(idw_dw[1].GetItemDecimal(ll_current_row,'total_payment_amount')) THEN
			MessageBox('Missing Award Amount', 'A value must be specified for the payment amount.')
			idw_dw[1].SetFocus()
			idw_dw[1].SetColumn('total_payment_amount')
			Return -1
		END IF

		IF IsNull(idw_dw[1].GetItemString(ll_current_row,"award_type_code")) OR Trim(idw_dw[1].GetItemString(ll_current_row,"award_type_code")) = ""  THEN
			MessageBox("Payment Module - Validation Error","Award Type is required.",Exclamation!)
			idw_dw[1].SetFocus()
			idw_dw[1].SetColumn("award_type_code")
			Return -1
		END IF

		IF IsNull(idw_dw[1].GetItemString(ll_current_row,"admin_region_code")) OR Trim(idw_dw[1].GetItemString(ll_current_row,"admin_region_code")) = ""  THEN
			MessageBox("Payment Module - Validation Error","Admin Region is required.",Exclamation!)
			idw_dw[1].SetFocus()
			idw_dw[1].SetColumn("admin_region_code")
			Return -1
		END IF

		IF IsNull(idw_dw[1].GetItemString(ll_current_row,"payment_type_code")) OR Trim(idw_dw[1].GetItemString(ll_current_row,"payment_type_code")) = ""  THEN
			MessageBox("Payment Module - Validation Error","Payment Type is required.",Exclamation!)
			idw_dw[1].SetFocus()
			idw_dw[1].SetColumn("payment_type_code")
			Return -1
		END IF

		IF IsNull(idw_dw[1].GetItemDateTime(ll_current_row,"award_start_date")) OR string(idw_dw[1].GetItemDateTime(ll_current_row,"award_start_date")) = "0000 01 01 00:00:00" THEN
			MessageBox("Payment Module - Validation Error","Award Start Date is required.",Exclamation!)
			idw_dw[1].SetFocus()
			idw_dw[1].SetColumn("award_start_date")
			Return -1
		END IF
		IF IsNull(idw_dw[1].GetItemDateTime(ll_current_row,"award_end_date")) OR string(idw_dw[1].GetItemDateTime(ll_current_row,"award_end_date")) = "0000 01 01 00:00:00" THEN
			MessageBox("Payment Module - Validation Error","Award End Date is a required field",Exclamation!)
			idw_dw[1].SetFocus()
			idw_dw[1].SetColumn("award_end_date")
			Return -1
		END IF

	END IF
/*	validate the transactions
*/
	ll_current_row = 1
	ll_transaction_rowcount = idw_dw[2].RowCount()
	DO WHILE ll_current_row <= ll_transaction_rowcount

/*		Recipient number is mandatory
*/
		IF IsNull(idw_dw[2].GetItemNumber(ll_current_row,"recipient_no")) THEN
			MessageBox("Payment Module - Validation Error","Recipient number is a required field",Exclamation!)
			idw_dw[2].ScrollToRow(ll_current_row)
			idw_dw[2].SetFocus()
			idw_dw[2].SetColumn("recipient_no")
			Return -1
		END IF

		IF IsNull(idw_dw[2].GetItemString(ll_current_row,"recipient_type_code")) THEN
			MessageBox("Payment Module - Validation Error","Recipient type code is a required field",Exclamation!)
			idw_dw[2].ScrollToRow(ll_current_row)
			idw_dw[2].SetFocus()
			idw_dw[2].SetColumn("recipient_type_code")
			Return -1
		END IF

		IF IsNull(idw_dw[2].GetItemString(ll_current_row,"payment_method_code")) THEN
			MessageBox("Payment Module - Validation Error","Payment Method code  is a required field",Exclamation!)
			idw_dw[2].ScrollToRow(ll_current_row)
			idw_dw[2].SetFocus()
			idw_dw[2].SetColumn("payment_method_code")
			Return -1
		END IF

		IF IsNull(idw_dw[2].GetItemDecimal(ll_current_row,"txn_amount")) THEN
			MessageBox("Payment Module - Validation Error","Transaction amount is a required field",Exclamation!)
			idw_dw[2].ScrollToRow(ll_current_row)
			idw_dw[2].SetFocus()
			idw_dw[2].SetColumn("txn_amount")
			Return -1
		END IF

		ll_current_row++
	LOOP

Return 0
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

public function boolean nf_is_award_processed ();DATETIME		ldtm_start_date
STRING		ls_award_type
LONG			ll_count
	
IF idw_dw[1].GetItemString(1,'authorized_user_id') > '' THEN
/*	if authorized then see if processed
*/
	ldtm_start_date = idw_dw[1].GetItemDateTime(1,'award_start_date')
	ls_award_type = idw_dw[1].GetItemString(1,'award_type_code')
	
	SELECT Count(*)
	  INTO :ll_count
	  FROM PERIODIC_AWARD_CONTROL
	 WHERE award_type_code = :ls_award_type
		AND period_from_date <= :ldtm_start_date
		AND period_to_date > :ldtm_start_date
		AND processed_date IS NOT NULL
	 USING SQLCA;

	SQLCA.nf_handle_error('Embedded SQL: select from PERIODIC_AWARD_CONTROL','n_awards','nf_delete')

	IF ll_count > 0 THEN	Return True
END IF

RETURN FALSE
end function

public function integer nf_allow_modification ();/* THe periodic award can only be modified if it has not
	been processed and it is not in a batch
	RETURN 	-1 (can't modify because the award is processed)
				-2 (can't modify because the award is in a batch)
				1	(modifications are allowed)
*/




If nf_is_award_processed() Then
	RETURN -1
END IF

IF idw_dw[1].GetItemNumber(1,'batch_no') > 0 THEN
	Return -2
END IF

RETURN 1
end function

public function integer nf_delete ();DATETIME	ldtm_start_date, ldtm_processed_date
STRING	ls_award_type
LONG		ll_count, ll_max
LONG		ll_allow_modification


	ll_allow_modification = nf_allow_modification()
	
	IF ll_allow_modification = -1 Then
		MessageBox('Delete Error','Unable to delete award.  The period has already been processed.')
		Return -1
	Elseif ll_allow_modification = -2 Then
		MessageBox('Delete Error','Unable to delete award.  The award has already been accepted into a batch.')
		Return -1
	END IF

	
	ll_max = idw_dw[2].RowCount()
	ll_count = 1
	DO WHILE ll_count <= ll_max
 		idw_dw[2].DeleteRow(1)
		ll_count++
	LOOP
	idw_dw[1].DeleteRow(1)
	
	SQLCA.nf_begin_transaction()

	idw_dw[1].Update()
	IF SQLCA.nf_handle_error("Award detail update","n_awardss","nf_delete") < 0 THEN
		Return -1
	END IF

	idw_dw[2].Update()
	IF SQLCA.nf_handle_error("Award Transactions update","n_awards","nf_delete") < 0 THEN
		Return -1
	END IF
	
	SQLCA.nf_commit_transaction()

		
Return 0

end function

public function integer nf_delete_recipient ();INTEGER		li_rtn


SetPointer(HourGlass!)

IF idw_dw[2].RowCount() = 1 Then
	MessageBox('Delete','You cannot delete the only recipient')
	return -1
end if

li_rtn = MessageBox('Delete','Do you really want to delete this recipient?',Question!,YesNo!,2)
IF li_rtn = 2 THen RETURN -1

li_rtn = nf_allow_modification() 
IF li_rtn = -1 THEN
	MessageBox('Delete','Recipient cannot be deleted.  Award has already been processed.')
	RETURN -1
Elseif li_rtn = -2 THEN
	MessageBox('Delete','Recipient cannot be deleted.  Award is in a batch.')
End if

idw_dw[2].DeleteRow(0)


RETURN 1
end function

public function string nf_get_authorization_filter ();STRING	ls_filter
BOOLEAN	lb_add_and
STRING	ls_admin_region

ls_admin_region = idw_basic_claim.GetItemString(1,'admin_region_code')


if gnv_user_authorizations.nf_authorizations_exist(ls_admin_region,"loe") Then	
	ls_filter += 'authorization_type_code = "loe" '
	lb_add_and = TRUE
END IF

	
if gnv_user_authorizations.nf_authorizations_exist(ls_admin_region,"act") Then	
	IF lb_add_and THEN
		ls_filter += ' OR '
	End if		
	ls_filter += ' authorization_type_code = "act" '
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
	ls_filter = '(' + ls_filter + ')'
END IF

Return ls_filter


end function

public function integer nf_insert_recipient (integer al_row);LONG  ll_rowcount

/*	Validate that another recipient is allowed                                          
*/

	ll_rowcount = idw_dw[2].InsertRow(al_row)
	IF ll_rowcount > 0 THEN
   	idw_dw[2].ScrollToRow(ll_rowcount)
		IF nf_set_recipient_defaults() < 0 THEN
			Return -1
		END IF
	END IF

Return ll_rowcount
end function

public subroutine nf_set_authorization (boolean ab_unpaid_authorizable_award);ib_unpaid_authorizable_award = ab_unpaid_authorizable_award
end subroutine

public function integer nf_validate_payment_type (string as_payment_type_code, ref integer ai_error_level, ref string as_error_message, ref string as_award_freq_code, ref string as_days_required);DECIMAL          lc_benefit_level_percentage
INTEGER	        li_age, li_opening_no
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

	idw_dw[1].GetChild('payment_type_code', ldwc_child)
	ll_rownum = ldwc_child.Find("payment_type_code = '" + as_payment_type_code + "'",1, ldwc_child.RowCount())
	IF ll_rownum <= 0 or IsNull(ll_rownum) THEN
		ai_error_level = 2
		as_error_message = "Payment Type selected is not a valid Payment Type"
		Return -1
	ELSE
		ls_opening_type_code      = ldwc_child.GetItemString(ll_rownum,"benefit_category_code")
		lc_benefit_level_percentage   = ldwc_child.GetItemDecimal(ll_rownum,"benefit_level_percentage")
		as_days_required = ldwc_child.GetItemString(ll_rownum,'fromto_dates_flag')
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
			(idw_basic_claim.GetItemNumber(1,'cost_alloc_no') = 999999 or &
      	 idw_basic_claim.GetItemNumber(1,'cost_alloc_no') = 10002 or &
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

public subroutine nf_set_payment_filter (long al_opening_no, long al_benefit_calculation_no, string as_award_type);DATAWINDOWCHILD  	ldwc_child
STRING          	ls_filter, ls_opening_type_code, ls_join
LONG					ll_row, ll_no
DECIMAL				ldec_benefit_level
INTEGER				li_rtn

/*	add a filter to get only payment types valid for this screen
*/

IF is_mode = 'READ' THen return

	IF IsNull(al_opening_no) THEN
		al_opening_no = 0
	END IF

	IF IsNull(al_benefit_calculation_no) THEN
		al_benefit_calculation_no = 0
	END IF

	IF IsNull(as_award_type) THEN
		as_award_type = ''
	END IF

	ll_row = idw_dw[1].GetRow()
	IF ll_row = 0 and idw_dw[1].RowCount() > 0 THEN
		ll_row = 1
	END IF
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
		ELSE
			ls_opening_type_code = 'I'
		END IF
		IF ls_opening_type_code > '' THEN
			IF ls_filter > '' THEN
				ls_join = 'AND'
			END IF
			ls_filter = ls_filter + ls_join + ' opening_type_code = "' + ls_opening_type_code + '"'
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
			END IF
		END IF

		IF ls_filter > '' THEN
			ls_join = 'AND'
		END IF
		ls_filter = ls_filter + ls_join + ' benefit_level_percentage = ' + String(ldec_benefit_level)
		
		ls_filter = ls_filter + ' AND award_type_code = "' + as_award_type + '"'

		idw_dw[1].GetChild('payment_type_code',ldwc_child)
		li_rtn = ldwc_child.SetFilter(ls_filter)
		If li_rtn = -1 Then SignalError(-666,'Error setting filter "' + ls_filter + '"')	
		ldwc_child.Filter()
	END IF
Return
end subroutine

public function integer nf_set_defaults ();LONG    				ll_row, ll_find_row, ll_num_rows
DATE   				ld_null
DECIMAL  			lc_daily_rate,	lc_hourly_rate,	lc_benefit_level_percentage
INTEGER  			li_benefit_calculation_no,	li_opening_no
STRING   			ls_payment_method_code, ls_opening_type_code
DATAWINDOWCHILD	ldwc_child


idw_basic_claim.Retrieve(il_claim_no)

/*	this routine only executes on an insert
*/
	ll_row = idw_dw[1].GetRow()
	IF ll_row < 0 THEN
   	RETURN -1
	ELSE
   	SetNull(ld_null)

		idw_dw[2].SetItem(1,'claim_no',il_claim_no)
		idw_dw[1].SetItem(1,'claim_no',il_claim_no)

   	idw_dw[2].SetItem(1,"recipient_type_code","I")
		nf_set_recipient_filter(FALSE)
   	idw_dw[2].SetItem(1,"recipient_no",idw_basic_claim.GetItemNumber(1,'individual_no'))
	   IF Trim(idw_basic_claim.GetItemString(1,'bank_no'))  = "" THEN
		   ls_payment_method_code = 'A'
			nf_setup_address('I',0)
	   ELSE
		   ls_payment_method_code = 'D'
			idw_dw[2].SetItem(1,'bank_no', idw_basic_claim.GetItemString(1,'bank_no'))
			idw_dw[2].SetItem(1,'bank_transit_no', idw_basic_claim.GetItemString(1,'bank_transit_no'))
			idw_dw[2].SetItem(1,'bank_account_no', idw_basic_claim.GetItemString(1,'bank_account_no'))
	   END IF
	   idw_dw[2].SetItem(1,"payment_method_code",ls_payment_method_code)
		idw_dw[1].SetItem(1,"batch_no",0)
		idw_dw[1].SetItem(1,'total_award_amount',0)
		idw_dw[1].SetItem(1,'total_payment_amount',0)
		idw_dw[1].SetItem(1,'total_deductions',0)
		idw_dw[1].SetItem(1,'admin_region_code',idw_basic_claim.GetItemString(1,'admin_region_code'))


/* 	Setup the default opening and benefit calculation number 
*/
	   idw_dw[1].GetChild('opening_no', ldwc_child)
		IF ldwc_child.RowCount() = 1 THEN
			li_opening_no = ldwc_child.GetItemNumber(1,"max_opening_no")
		ELSE
			li_opening_no = nf_check_openings(ldwc_child, 'RLOE')
			IF li_opening_no > 0 THEN
				li_opening_no = ldwc_child.GetItemNumber(li_opening_no,'opening_no')
			ELSE
				li_opening_no = nf_check_openings(ldwc_child, 'LTD') 
				IF li_opening_no > 0 THEN
					li_opening_no = ldwc_child.GetItemNumber(li_opening_no,'opening_no')
				ELSE
					li_opening_no = nf_check_openings(ldwc_child, 'PEN') 
					IF li_opening_no > 0 THEN
						li_opening_no = ldwc_child.GetItemNumber(li_opening_no,'opening_no')
					ELSE
						li_opening_no = ldwc_child.GetItemNumber(1,"max_opening_no")
						ll_num_rows = ldwc_child.RowCount()
						ll_find_row = ldwc_child.Find("opening_no = " + String(li_opening_no),1,ll_num_rows)
						ls_opening_type_code = ldwc_child.GetItemString(ll_find_row,"opening_type_code")
					END IF
				END IF
			END IF
		END IF
		idw_dw[1].SetItem(1,"opening_no",li_opening_no)

		
		idw_dw[1].GetChild('benefit_calculation_no', ldwc_child)
		IF li_opening_no = 0 OR ldwc_child.RowCount() = 1 THEN			//if rowcount = 1 then only benefit calc of 0
			ldwc_child.SetFilter(" benefit_calculation_no = 0")
		ELSEIF ls_opening_type_code = 'RLOE' THEN
			ldwc_child.SetFilter(" benefit_calculation_no = 0") // PR 3709 - KDM  - RLOE awards are never associated with a bencalc
		ELSE
			ldwc_child.SetFilter("opening_no = " + string(li_opening_no) + " or benefit_calculation_no = 0")
		END IF
		ldwc_child.Filter()
		li_benefit_calculation_no = ldwc_child.GetItemNumber(1,"max_benefit_calculation_no")


		nf_setup_benefit_info(li_benefit_calculation_no,lc_benefit_level_percentage)
		idw_dw[1].SetItem(1,"benefit_calculation_no",li_benefit_calculation_no)

		nf_set_award_filter(li_opening_no, li_benefit_calculation_no)
		nf_set_payment_filter(li_opening_no,li_benefit_calculation_no,'')
   	nf_setup_default(li_benefit_calculation_no,lc_benefit_level_percentage,li_opening_no)
		IF idw_dw[1].GetItemString(1,'award_type_code') = 'PEN'  THEN
			idw_dw[1].SetItem(1,'admin_region_code', 'PRV')
		END IF

	END IF

Return 0   
end function

public function integer nf_setup_default (integer ai_benefit_calculation_no, decimal adec_benefit_level_percentage, integer ai_opening_no);DECIMAL          lc_total_award_amount, lc_deduction 
INTEGER          li_error_level
LONG					ll_rownum, ll_award_no, ll_count,ll_no_periods
STRING				ls_award_type_code, ls_award_freq_code, ls_opening_type_code,ls_payment_freq_code
DATAWINDOWCHILD	ldwc_child_opening, ldwc_child_benefit, ldwc_child
DATETIME				ldtm_prev_award_end_date, ldtm_date, ldtm_benefit_start_date
STRING 				ls_allow
/* Function Name: nf_setup_default                                                                      
	 Arguments:                                                                             
   	             ai_benefit_calculation_no                                                            
      	          adec_benefit_level_percentage                                                          
	                                                                                                      
	 Return Values: n/a                                                                                   
*/

	IF ai_opening_no > 0 THEN
		idw_dw[1].GetChild('opening_no',ldwc_child_opening)
		ll_rownum = ldwc_child_opening.Find("opening_no = " + string(ai_opening_no),1,ldwc_child_opening.RowCount())
		ls_opening_type_code = ldwc_child_opening.GetItemString(ll_rownum, 'opening_type_code')
		ldtm_benefit_start_date = ldwc_child_opening.GetItemDateTime(ll_rownum,'benefit_start_date')
	END IF

	SetNull(ls_award_type_code)
	SetNull(ldtm_prev_award_end_date)

/*	if opening no > 0 then find the max award of the type in the and default to that
*/
	SetNull(ls_award_type_code)
	SetNull(ll_award_no)
	IF ai_opening_no > 0 THEN
		SELECT Max(award_no)
		  INTO :ll_award_no
		  FROM PERIODIC_AWARD
		 WHERE claim_no = :il_claim_no
		   AND opening_no = :ai_opening_no
			AND benefit_calculation_no = :ai_benefit_calculation_no
		 USING SQLCA;

		IF SQLCA.nf_handle_error('Embedded SQL: PERIODIC AWARD', 'n_awards', 'nf_setup_defaults') < 0 THEN
			Return -1
		END IF
		IF NOT IsNull(ll_award_no) THEN
			SELECT award_type_code, award_end_date, total_deductions
			  INTO :ls_award_type_code, :ldtm_prev_award_end_date, :lc_deduction 
			  FROM PERIODIC_AWARD
			 WHERE award_no = :ll_award_no
			   AND claim_no = :il_claim_no;

			IF SQLCA.nf_handle_error('Embedded SQL: PAYMENT select','n_payment', 'nf_setup_default') < 0 THEN
				Return -1
			END IF
			idw_dw[1].SetItem(1,'award_start_date', ldtm_prev_award_end_date)
		END IF
	END IF
   idw_dw[1].GetChild('benefit_calculation_no', ldwc_child_benefit)
	ll_rownum = ldwc_child_benefit.Find("benefit_calculation_no = " + string(ai_benefit_calculation_no),1, ldwc_child_benefit.RowCount())
	IF ll_rownum > 0 THEN
		lc_total_award_amount = ldwc_child_benefit.GetItemNumber(ll_rownum,'award_amount')
	ELSE
		MessageBox('Error','Unable to find benefit information for defaulting payment.  Please try again.')
		Return -1
	END IF
/*	get award info
*/
	idw_dw[1].GetChild('award_type_code',ldwc_child)

	IF IsNull(ls_award_type_code) THEN
		ll_rownum = ldwc_child.Find('opening_type_code = "' + ls_opening_type_code + '"',1,ldwc_child.RowCount())	
		IF ll_rownum > 0 THEN
			ls_payment_freq_code = ldwc_child.GetItemString(ll_rownum,'payment_freq_code')
			ll_no_periods = ldwc_child.GetItemNumber(ll_rownum,'no_authorization_periods')
			ls_award_type_code = ldwc_child.GetItemString(ll_rownum,'award_type_code')
		END IF
	ELSE
		ll_rownum = ldwc_child.Find('award_type_code = "' + ls_award_type_code + '"',1,ldwc_child.RowCount())	
		IF ll_rownum <= 0 THEN
			ls_award_type_code = ''
			ll_no_periods = 0
			ls_payment_freq_code = ''
		ELSE
			ls_payment_freq_code = ldwc_child.GetItemString(ll_rownum,'payment_freq_code')
			ll_no_periods = ldwc_child.GetItemNumber(ll_rownum,'no_authorization_periods')
		END IF
	END IF

	idw_dw[1].SetItem(1,'award_type_code',ls_award_type_code)
	nf_set_payment_filter(ai_opening_no, ai_benefit_calculation_no, ls_award_type_code)
/*	now try and set the payment type code
*/
	IF NOT IsNull(ls_award_type_code) AND Trim(ls_award_type_code) <> '' THEN
		idw_dw[1].GetChild('payment_type_code',ldwc_child)
		ll_rownum = ldwc_child.Find('award_type_code = "' + ls_award_type_code + '"',1,ldwc_child.RowCount())	
		IF ll_rownum > 0 THEN
			/* SR 135 - Remove -6 from Default payment type code - has to be hard-coded to do this.*/
			IF (ldwc_child.GetItemString(ll_rownum,'payment_type_code') = '6') OR  &
			   (ldwc_child.GetItemString(ll_rownum,'payment_type_code') = '-6') OR  &
				(ldwc_child.GetItemString(ll_rownum,'payment_type_code') = '.6') THEN 
				ll_rownum = ll_rownum + 1 
			END IF
			idw_dw[1].SetItem(1,'payment_type_code', ldwc_child.GetItemString(ll_rownum,'payment_type_code'))
		END IF

/*		get the start and end dates
*/
		IF NOT IsNull(ldtm_benefit_start_date) THEN
			SELECT Min(period_from_date)
			  INTO :ldtm_date
			  FROM PERIODIC_AWARD_CONTROL
			 WHERE processed_date IS NULL
				AND period_from_date >= :ldtm_benefit_start_date
				AND award_type_code = :ls_award_type_code
			 USING SQLCA;

			IF SQLCA.nf_handle_error('Embedded SQL: SELECT from PERIODIC_AWARD_CONTROL','n_awards','nf_change_item') < 0 THEN
				Return -1
			END IF
			IF	IsNull(idw_dw[1].GetItemDateTime(1,'award_start_date')) THEN
				idw_dw[1].SetItem(1,'award_start_date', ldtm_date)
			ELSE
				ldtm_date = ldtm_prev_award_end_date
			END IF
			idw_dw[1].GetChild('award_type_code',ldwc_child)
			ldtm_date = nf_get_end_date(ldtm_date,ll_no_periods,ls_payment_freq_code)	
		
			IF ldwc_child.GetItemString(ldwc_child.getrow(),'award_type_code') = 'CA' or ldwc_child.GetItemString(ldwc_child.getrow(),'award_type_code') = 'CL' THEN
				idw_dw[1].SetItem(1,'award_end_date', RelativeDate(Date(ldtm_date),-1))
			ELSE
				idw_dw[1].SetItem(1,'award_end_date', ldtm_date)
			END IF
		END IF
	END IF

	idw_dw[1].SetItem(1,'award_type_code', ls_award_type_code)
	idw_dw[1].SetItem(1,"total_award_amount",lc_total_award_amount)
	idw_dw[1].SetItem(1,"total_deductions",lc_deduction)

	lc_total_award_amount = lc_total_award_amount + lc_deduction
	IF lc_total_award_amount < 0 THEN
		lc_total_award_amount = 0
	END IF
	idw_dw[1].SetItem(1,"total_payment_amount",lc_total_award_amount )

	IF idw_dw[2].RowCount() = 1 THEN
		idw_dw[2].SetItem(1,"txn_amount",lc_total_award_amount)
	END IF

Return 0
end function

public function integer nf_change_item (long al_datawindow);DATAWINDOWCHILD	ldwc_child, ldwc_child_b
LONG					ll_no, ll_current_row,ll_newrow, ll_oldrow, ll_rownum, ll_result,ll_null, ll_recipient_no
DECIMAL				ldec_total_award_amount, ldec_total_deductions, ldec_txn_amount, ldec_benefit_level_percentage
INTEGER				li_error_level, li_year, li_month, li_day, li_no
STRING				ls_string, ls_string2,ls_recipient_type_code
STRING				ls_payment_code, ls_error_message,	ls_old_days_required_flag, ls_old_payment_code
STRING				ls_days_required_flag, ls_old_repeat_payment_allowed_flag, ls_repeat_payment_allowed_flag
STRING				ls_opening_type_code, ls_date, ls_award_freq_code
STRING 				ls_bank_no, ls_bank_transit_no, ls_bank_account_no, ls_recipient_name, ls_award_type_code
DATETIME				ldtm_date

ls_award_type_code = idw_dw[1].GetItemString(1,'award_type_code')

CHOOSE CASE al_datawindow
	CASE 1
		ll_current_row = idw_dw[1].GetRow()
		CHOOSE CASE idw_dw[1].GetColumnName()
			CASE "opening_no"   
				IF MessageBox("Payment Module","Changing the opening will reset payment details" +     &
				  "~r~nDo you wish to continue",Question!,YesNo!) = 1 THEN
					idw_dw[1].SetItem(1,'payment_type_code','')
					idw_dw[1].SetItem(1,'award_type_code','')
					
					li_no = Integer(idw_dw[1].GetText())
					
					idw_dw[1].GetChild('opening_no', ldwc_child_b)
					
					ll_rownum = ldwc_child_b.Find("opening_no = " + string(li_no),1, ldwc_child_b.RowCount() )
					IF ll_rownum > 0 THEN
						ls_opening_type_code = ldwc_child_b.GetItemString(ll_rownum,'opening_type_code')
					ELSE
						MessageBox('Error','Unable to find opening information.  Please try again.')
						Return -1
					END IF
					
					idw_dw[1].GetChild('benefit_calculation_no', ldwc_child)
					IF ls_opening_type_code = 'RLOE' THEN
						ldwc_child.SetFilter("benefit_calculation_no = 0")
						ldwc_child.Filter()
					ELSE
						ldwc_child.SetFilter("opening_no = " + string(li_no) + " or benefit_calculation_no = 0")
						ldwc_child.Filter()
					END IF
					
					IF ls_opening_type_code = 'RLOE' THEN
						idw_dw[1].SetItem(1,"benefit_calculation_no",0)
					ELSE
						ll_no = ldwc_child.GetItemNumber(1,"max_benefit_calculation_no")
						idw_dw[1].SetItem(1,"benefit_calculation_no",ll_no)
					END IF

					//	get the benefit category code and set a filter on the payment type
					nf_set_payment_filter(li_no, ll_no, '')
					nf_setup_benefit_info(ll_no,ldec_benefit_level_percentage)
					nf_setup_default(ll_no,ldec_benefit_level_percentage, li_no)
					nf_set_award_filter(li_no, ll_no)
				ELSE
					li_no = idw_dw[1].GetItemNumber(1,"opening_no")
					idw_dw[1].settext(string(li_no))
					Return -1
				END IF

			CASE "benefit_calculation_no"
				// Warn the user that changing the benefit calc resets payment details and see if they wish to continue
				IF MessageBox("Payment Module","Changing the benefit calculation will reset payment details" +     &
					  "~r~nDo you wish to continue",Question!,YesNo!) = 1 THEN
					li_no = Integer(idw_dw[1].GetText())
					idw_dw[1].SetItem(1,'payment_type_code','')
					idw_dw[1].SetItem(1,'award_type_code','')
					nf_set_payment_filter(idw_dw[1].GetItemNumber(1,'opening_no'),li_no, '')
					nf_setup_benefit_info(li_no,ldec_benefit_level_percentage)
					nf_setup_default(li_no,ldec_benefit_level_percentage,idw_dw[1].GetItemNumber(1,'opening_no'))
					nf_set_award_filter(idw_dw[1].GetItemNumber(1,'opening_no'), li_no)
					
				ELSE
					li_no = idw_dw[1].GetItemNumber(1,"benefit_calculation_no")
					idw_dw[1].SetItem(1,'benefit_calculation_no',idw_dw[1].GetItemNumber(1,"benefit_calculation_no"))
					Return -1
				END IF

			CASE "award_type_code"
				ls_string = idw_dw[1].GetText()
				idw_dw[1].SetItem(1,'payment_type_code','')
				nf_set_payment_filter(idw_dw[1].GetItemNumber(1,'opening_no'),idw_dw[1].GetItemNumber(1,'benefit_calculation_no'), ls_string )
				IF ls_string = 'PEN' OR ls_string = 'TSB' THEN
					idw_dw[1].SetItem(1,'admin_region_code', 'PRV')
				END IF
				ll_no = idw_dw[1].GetItemNumber(1,'benefit_calculation_no')
				IF ll_no > 0 THEN
					//	if there is a benefit calculation then get the effective date from the calcualtion
					idw_dw[1].GetChild('benefit_calculation_no',ldwc_child)
					ll_no = ldwc_child.Find('benefit_calculation_no = '+ String(ll_no),1,ldwc_child.RowCount())
					ldtm_date = ldwc_child.GetItemDateTime(ll_no,'effective_from_date')
				ELSE
					//	otherwise use the opening benefit start date
					ll_no = idw_dw[1].GetItemNumber(1,'opening_no')
					IF ll_no > 0 THEN
						idw_dw[1].GetChild('opening_no',ldwc_child)
						ll_no = ldwc_child.Find('opening_no = '+ String(ll_no),1,ldwc_child.RowCount())
						ldtm_date = ldwc_child.GetItemDateTime(ll_no,'benefit_start_date')
					END IF
				END IF
				IF NOT IsNull(ldtm_date) THEN
					// find the next period to date that has not been processed and use this for the start date
					SELECT Min(period_from_date)
					  INTO :ldtm_date
					  FROM PERIODIC_AWARD_CONTROL
					 WHERE processed_date IS NULL
						AND period_from_date >= :ldtm_date
						AND award_type_code = :ls_string
					 USING SQLCA;

					IF SQLCA.nf_handle_error('Embedded SQL: SELECT from PERIODIC_AWARD_CONTROL','n_awards','nf_change_item') < 0 THEN
						Return -1
					END IF
					idw_dw[1].SetItem(1,'award_start_date', ldtm_date)
					idw_dw[1].GetChild('award_type_code',ldwc_child)
					ll_no = ldwc_child.Find('award_type_code = "' + ls_string + '"',1,ldwc_child.RowCount())
					IF ll_no > 0 THEN
						ls_string2 = ldwc_child.GetItemString(ll_no,'payment_freq_code')
						ll_no = ldwc_child.GetItemNumber(ll_no,'no_authorization_periods')
						ldtm_date = nf_get_end_date(ldtm_date,ll_no,ls_string2)
					
					  /* pr4451: previously, the code checked ls_string (award type code) for a value of 
					  CA (care allowance) or CL (clothing allowance) and if either of those two, would do a
					  RelativeDate function on ldtm_date, subtractinand subtracted one day, to make award_end_date
					  equal to the last day of a month. This pr removes the IF statement which eliminates this
					  offset functionality for CA and CL and sets the award_end_date to ldtm_date. Other business rule code forces 
					  the user to use a start date that begins on the first day on the month, thus ensuring this code will force 
					  an end date that uses the first day of a month also. PR4451 is continued later in this function and in the function: nf_check_bus_rules */
					
					   idw_dw[1].SetItem(1,'award_end_date', ldtm_date)
					END IF
				END IF

			CASE "payment_type_code"
				ls_payment_code = idw_dw[1].GetText()
				IF Trim(ls_payment_code) <> '' THEN
					ls_old_payment_code = idw_dw[1].GetItemString(1,"payment_type_code")

					//	Check to see if the selected payment type code is allowed and if the benefit level of this code is 
					// consistent with the current benefit calculation
					li_no = idw_dw[1].GetItemNumber(1,"benefit_calculation_no")
					ll_rownum = idw_dw[1].GetChild('benefit_calculation_no', ldwc_child_b)
					ll_rownum  = ldwc_child_b.Find("benefit_calculation_no = " + string(li_no),1, ldwc_child_b.RowCount())
					ls_award_freq_code = ldwc_child_b.GetItemString(ll_rownum,"award_freq_code")

					nf_validate_payment_type(ls_payment_code, li_error_level, ls_error_message, ls_award_freq_code, ls_days_required_flag)
					IF li_error_level = 2 THEN
						MessageBox("Payment Module - Validation Error", ls_error_message,Exclamation!)
						Return -1
					END IF
					idw_dw[1].GetChild('payment_type_code', ldwc_child)
					ll_newrow = ldwc_child.GetRow()
					ldec_benefit_level_percentage = ldwc_child.GetItemDecimal(ll_newrow,"benefit_level_percentage")

					IF ldec_benefit_level_percentage > 0 THEN
						IF idw_dw[1].GetItemDecimal(1,"benefit_calculation_no") = 0 THEN
							MessageBox("Payment Module - Validation Error", "You must select a benefit calculation before you can make a " + &
							  "~r~nLoss of Earnings payment or a 3 Day Reimbursement", Exclamation!)
							Return -1
						END IF
					END IF
					IF li_error_level = 1 THEN
						MessageBox("Payment Module",ls_error_message)
					END IF

					IF not IsNull(ls_old_payment_code) THEN
						ll_oldrow = ldwc_child.Find("payment_type_code = '" + ls_old_payment_code + "'",1, ldwc_child.RowCount())
						//	Check amounts to see if initialization is required
						IF ll_oldrow > 0 THEN
							ls_old_days_required_flag = ldwc_child.GetItemString(ll_oldrow,"days_hours_flag")
							ls_days_required_flag = ldwc_child.GetItemString(ll_newrow,"days_hours_flag")
						END IF
						IF ls_old_days_required_flag = "Y" and ls_days_required_flag = "N" THEN
							idw_dw[1].SetItem(1,"total_award_amount",0)
							idw_dw[1].SetItem(1,"total_deductions",0)
							idw_dw[1].SetItem(1,"total_payment_amount",0)
							IF idw_dw[2].RowCount() = 1 THEN
								idw_dw[2].SetItem(1,"txn_amount",0)
							END IF
						END IF
						IF ls_old_days_required_flag = "N" and ls_days_required_flag = "Y" THEN
							idw_dw[1].SetItem(1,"total_award_amount",0)
							idw_dw[1].SetItem(1,"total_deductions",0)
							idw_dw[1].SetItem(1,"total_payment_amount",0)
							IF idw_dw[2].RowCount() = 1 THEN
								idw_dw[2].SetItem(1,"txn_amount",0)
							END IF
						END IF		
					END IF
					idw_dw[1].SetItem(1,'payment_type_code',ls_payment_code)	// this was added because you could not see the value
				END IF																		

			CASE "award_start_date"
				ldtm_date = DateTime(Date(Left(idw_dw[1].GetText(),10)))
				idw_dw[1].GetChild('award_type_code',ldwc_child)
				ls_string = idw_dw[1].GetItemString(1,'award_type_code')
				ll_no = ldwc_child.Find('award_type_code = "' + ls_string + '"',1,ldwc_child.RowCount())
				IF ll_no > 0 THEN
					ls_string2 = ldwc_child.GetItemString(ll_no,'payment_freq_code')
					ll_no = ldwc_child.GetItemNumber(ll_no,'no_authorization_periods')
					ldtm_date = nf_get_end_date(ldtm_date,ll_no,ls_string2)
					
					/* pr4451: previously, the code checked ls_string (award type code) for a value of 
					CA (care allowance) or CL (clothing allowance) and if either of those two, would do a
					RelativeDate function on ldtm_date, subtracting one day, to make award_end_date
					equal to the last day of the month. This pr removes the IF statement, thus eliminates this
					offset functionality for CA and CL and sets the award_end_date to ldtm_date. Other business rule code forces 
					the user to use a start date that begins on the first day on the month, thus ensuring this code will force 
					an end date that uses the first day of a month also* PR4451 is also contained in nf_check_bus_rule */
					
					idw_dw[1].SetItem(1,'award_end_date', ldtm_date)
				END IF

				IF idw_basic_claim.GetItemString(1,'legislation_code') = 'P48' THEN
					//	default end date to 5 years from start
					ldtm_date = DateTime(Date(Left(idw_dw[1].GetText(),10)))
					idw_dw[1].SetItem(1,'award_end_date', DateTime(Date(Year(Date(ldtm_date)) + 5, Month(Date(ldtm_date)), Day(Date(ldtm_date)) ) ) )
				END IF						

		  CASE "total_award_amount"
			  ldec_total_award_amount   = Round(Dec(idw_dw[1].GetText()),2)
			  ldec_total_deductions     = idw_dw[1].GetItemDecimal(1,"total_deductions")
			  ldec_txn_amount           = ldec_total_award_amount + ldec_total_deductions

				IF abs(ldec_total_deductions) > ldec_total_award_amount THEN
				   MessageBox ("Award Module -- Validation Error", "Award cannot be less than the Deduction.", Exclamation!)
					Return -1
				END IF
				idw_dw[1].SetItem(1,"total_payment_amount",ldec_txn_amount)
				IF idw_dw[2].RowCount() = 1 THEN idw_dw[2].SetItem(1,"txn_amount",ldec_txn_amount)


		  CASE "total_deductions"
			  ldec_total_deductions      = Round(Dec(idw_dw[1].GetText()),2)
			  ldec_total_award_amount    = idw_dw[1].GetItemDecimal(1,"total_award_amount")
			  IF abs(ldec_total_deductions) > ldec_total_award_amount THEN
				   MessageBox ("Award Module -- Validation Error", "Deductions cannot be more than the Award", Exclamation!)
					Return -1
				END IF
			  IF ldec_total_deductions > 0 THEN
				   MessageBox ("Award Module -- Validation Error", "Deduction amount must be a negative number", Exclamation!)
					Return -1
				END IF

			  ldec_txn_amount            = ldec_total_award_amount + ldec_total_deductions
			  IF ldec_txn_amount < 0 THEN ldec_txn_amount = 0
			  idw_dw[1].SetItem(1,"total_payment_amount",ldec_txn_amount)
			  IF idw_dw[2].RowCount() = 1 THEN idw_dw[2].SetItem(1,"txn_amount",ldec_txn_amount)
		END CHOOSE

	CASE 2
		SetNull(ll_null)
		idw_dw[2].AcceptText()
		ll_current_row   = idw_dw[2].GetRow()
		CHOOSE CASE idw_dw[2].GetColumnName()

			CASE "payment_method_code"
				ls_payment_code     = idw_dw[2].GetText()
				ls_old_payment_code = idw_dw[2].GetItemString(ll_current_row,"payment_method_code")
				ls_recipient_type_code     = idw_dw[2].GetItemString(ll_current_row,"recipient_type_code")
				ll_recipient_no                    = idw_dw[2].GetItemNumber(ll_current_row,"recipient_no")
				
				IF ls_payment_code = "D" THEN
					IF ll_recipient_no > 0 THEN
						IF nf_get_bank(ll_recipient_no,ls_recipient_type_code,ls_bank_no,ls_bank_transit_no, ls_bank_account_no, ls_recipient_name) < 0 THEN
							// this is for view only
							MessageBox("Payment Module - Validation Error","Unable to determine Direct Deposit information to display.",Exclamation!)
							// Return -1
						END IF
						IF Trim(ls_bank_no) = "" THEN
							MessageBox("Payment Module - Validation Error","There is no direct deposit information for this individual~r~nYou must select another Payment Method",Exclamation!)
							idw_dw[2].SetItem(ll_current_row, 'payment_method_code','A')
							Return -1
						END IF
						
						nf_setup_address(ls_recipient_type_code,0)
						idw_dw[2].SetItem(ll_current_row,'recipient_name', ls_recipient_name)
						idw_dw[2].SetItem(ll_current_row,'bank_no', ls_bank_no)
						idw_dw[2].SetItem(ll_current_row,'bank_transit_no', ls_bank_transit_no)
						idw_dw[2].SetItem(ll_current_row,'bank_account_no',ls_bank_account_no)		
					ELSE
						MessageBox("Recipient Number","Recipient Number has not been entered. Unable to determine Direct Deposit information to display.", Information!)
					END IF
				ELSE
					nf_setup_address(ls_recipient_type_code,ll_recipient_no)
				END IF
				idw_dw[2].setfocus()
				
			CASE "recipient_type_code"
				ls_recipient_type_code = idw_dw[2].GetText()

				//	IF the recipient type has been changed to individual, default the banking info and address fields
				IF ls_recipient_type_code  = "I" THEN
					nf_set_recipient_filter(FALSE)
					idw_dw[2].SetItem(ll_current_row,"recipient_no",idw_basic_claim.GetItemNumber(1,'individual_no'))
					IF nf_get_bank(idw_basic_claim.GetItemNumber(1,'individual_no'),ls_recipient_type_code,ls_bank_no,ls_bank_transit_no, ls_bank_account_no, ls_recipient_name) < 0 THEN
						MessageBox("Payment Module - Validation Error","Unable to determine Direct Deposit information.",Exclamation!)
						Return -1
					END IF
					idw_dw[2].SetItem(ll_current_row,'recipient_name', ls_recipient_name)
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
						nf_validate_recipient(ls_recipient_type_code,0,ll_current_row,ls_award_type_code)
					END IF	
				ELSE
					nf_set_recipient_filter(TRUE)
					idw_dw[2].SetItem(ll_current_row,"recipient_no",ll_null)
					idw_dw[2].SetItem(ll_current_row,"payment_method_code","A")
				END IF

			CASE "recipient_no"
				ll_no = Long(idw_dw[2].GetText())
				ls_recipient_type_code = idw_dw[2].GetItemString(ll_current_row,"recipient_type_code")
				//	Check that a valid recipient number has been entered, and if so, reset the tab order and 
				// default the address

				IF nf_get_bank(ll_no,ls_recipient_type_code,ls_bank_no,ls_bank_transit_no, ls_bank_account_no, ls_recipient_name) < 0 THEN
					 MessageBox("Payment Module - Validation Error","Unable to determine Direct Deposit information.",Exclamation!)
					 Return -1
				END IF
				//	default to direct deposit if banking info
				IF ls_bank_no > '' THEN
					idw_dw[2].SetItem(ll_current_row,'recipient_name', ls_recipient_name)
					idw_dw[2].SetItem(ll_current_row,'bank_no', ls_bank_no)
					idw_dw[2].SetItem(ll_current_row,'bank_account_no', ls_bank_account_no)
					idw_dw[2].SetItem(ll_current_row,'bank_transit_no', ls_bank_transit_no)
					idw_dw[2].SetItem(ll_current_row,'payment_method_code','D')
				ELSE
					idw_dw[2].SetItem(ll_current_row,'recipient_name', ls_recipient_name)
					idw_dw[2].SetItem(ll_current_row,'bank_no', '')
					idw_dw[2].SetItem(ll_current_row,'bank_account_no', '')
					idw_dw[2].SetItem(ll_current_row,'bank_transit_no', '')
					idw_dw[2].SetItem(ll_current_row,'payment_method_code','A')
				END IF

				ll_result = nf_validate_recipient(ls_recipient_type_code,ll_no,ll_current_row,ls_award_type_code)
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
				nf_setup_address(ls_recipient_type_code,ll_no)
		END CHOOSE
END CHOOSE
Return 0
end function

public function integer nf_check_bus_rule ();DATAWINDOWCHILD		ldwc_child, ldwc_child_b

DATETIME        	ldtm_award_start_date, ldtm_award_end_date, ldtm_server_date, ldtm_benefit_end_date, ldtm_date
DATETIME		ldtm_effective_from_date, ldtm_birth_date, ldtm_min_period_from_date
DATE             	ld_null_date, ld_year_after_effective_from
DECIMAL          	ldec_temp_txn_amount, ldec_total_payment_amount,ldec_new_limit,ldec_saved_limit, ldec_award_amount
DECIMAL			ld_txn_amount, ldec_bencalc_award_amount
LONG				ll_no, ll_count, ll_result, ll_claim_no, ll_opening_no, ll_award_no
LONG             	ll_transaction_rowcount, ll_recipient_cntr, ll_recipient_no, ll_no_periods, ll_rownum
STRING			ls_opening, ls_payment_method_code, ls_recipient_type_code, ls_recipient_type_desc
STRING           	ls_string, ls_payment_freq_code, ls_award_type_code, ls_authorization_type_code, ls_admin_region_code
STRING			ls_claim_status_code, ls_claim_status_type_code, ls_payment_type_code, ls_award_freq_code
STRING			ls_error_message, ls_days_required_flag
INTEGER			li_error_level,li_no, li_next_year , li_month , li_day , li_payment_count

/*	Check the business rules for the payment
*/
	SetNull(ld_null_date)
 
/*	Perform consistency and mandatory validations
*/
	ls_string = idw_dw[1].GetItemString(1, "effective_from_date")
	ldtm_award_start_date = idw_dw[1].GetItemDateTime(1,"award_start_date")
	ldtm_award_end_date = idw_dw[1].GetItemDateTime(1,"award_end_date")
	
	ll_claim_no = idw_dw[1].GetItemNumber(1,'claim_no')

	ll_opening_no = idw_dw[1].GetItemNumber(1,'opening_no')
	idw_dw[1].GetChild('opening_no',ldwc_child)
	ll_no = ldwc_child.Find('opening_no = ' + String(idw_dw[1].GetItemNumber(1,'opening_no')),1,ldwc_child.RowCount())
	IF ll_no < 1 THEN
		MessageBox('Invalid Opening Number','The opening selected cannot be found.')
		idw_dw[1].SetColumn('opening_no')
		idw_dw[1].SetFocus()
		Return -1
	END IF
	ls_opening = ldwc_child.getitemString(ll_no,"opening_type_code")
	ldtm_benefit_end_date = ldwc_child.GetItemDateTime(ll_no,'benefit_end_date')
	

	ls_award_type_code = idw_dw[1].GetItemString(1,'award_type_code')	
	idw_dw[1].GetChild('award_type_code',ldwc_child)
	ll_no = ldwc_child.Find('award_type_code = "' + ls_award_type_code + '"',1,ldwc_child.RowCount())
	IF ll_no < 1 THEN		
		MessageBox('Invalid Award Type','The award type entered is not valid.')
		idw_dw[1].SetColumn('award_type_code')
		idw_dw[1].SetFocus()
		Return -1
	END IF
	
	// PR 3981 - check claim status for award type
	ls_claim_status_code = idw_basic_claim.GetItemString(1,'claim_status_code')
	ls_claim_status_type_code = idw_basic_claim.GetItemString(1,'claim_status_type_code')
	
	CHOOSE CASE ls_claim_status_code 
		CASE 'F'
			CHOOSE CASE ls_claim_status_type_code
				CASE '01','02'
					//OK
				CASE '03','04'
					IF ls_award_type_code = 'LTD' or ls_award_type_code = 'PEN' THEN
						MessageBox('Invalid Award Type','The award type entered is not valid for the claim status.')
						idw_dw[1].SetColumn('award_type_code')
						idw_dw[1].SetFocus()
						Return -1
					END IF
				CASE ELSE
					// this should be prevented from happening in open event of w_periodic_award
					MessageBox('Invalid Award Type','The award type entered is not valid for the claim status.')
					idw_dw[1].SetColumn('award_type_code')
					idw_dw[1].SetFocus()
					Return -1
			END CHOOSE
		CASE 'A'
			//this is OK, its a valid claim status
		CASE ELSE
			MessageBox('Invalid Claim Status','The The claim status is not valid to add an award.')
			idw_dw[1].SetColumn('award_type_code')
			idw_dw[1].SetFocus()
			Return -1
	END CHOOSE
	
	ls_authorization_type_code = ldwc_child.GetItemString(ll_no, 'authorization_type_code')

	ls_payment_freq_code = ldwc_child.GetItemString(ll_no,'payment_freq_code')
	ll_no_periods = ldwc_child.GetItemNumber(ll_no,'no_authorization_periods')

	ldtm_server_date	= f_server_datetime()

	idw_dw[1].GetChild('benefit_calculation_no',ldwc_child)
	ll_no = ldwc_child.Find('benefit_calculation_no = ' + String(idw_dw[1].GetItemNumber(1,'benefit_calculation_no')),1,ldwc_child.RowCount())
	IF ll_no < 1 THEN
		MessageBox('Invalid Benefit Calculation Number','The benefit calculation selected cannot be found.')
		idw_dw[1].SetColumn('benefit_calculation_no')
		idw_dw[1].SetFocus()
		Return -1
	END IF
	ldtm_effective_from_date = ldwc_child.GetItemDateTime(ll_no,'effective_from_date')
	ldec_award_amount = ldwc_child.GetItemDecimal(ll_no,'award_amount')
	IF idw_dw[1].GetItemNumber(1,'benefit_calculation_no') > 0 THEN
		IF Round(idw_dw[1].GetItemDecimal(1,'total_award_amount'),4) <> ldec_award_amount THEN
			MessageBox('Invalid Award Amount', 'The total award amount cannot be changed.  It must match the benefit calculation.' )
			idw_dw[1].SetColumn('total_award_amount')
			idw_dw[1].SetFocus()
			Return -1
		END IF
	END IF
	
/* Validate award amount
	BR 1.15	
*/
	IF idw_dw[1].GetItemDecimal(1,'total_award_amount') < 0 Then
		MessageBox('Award Module - Validation Error','Award amount must be a positive amount.')
		idw_dw[1].SetColumn('total_award_amount')
		idw_dw[1].SetFocus()
		return -1
	End if

/*	validate deductions
	BR 1.6
*/
	IF idw_dw[1].GetItemDecimal(1,'total_deductions') > 0 Then
		MessageBox('Award Module - Validation Error','Deduction must be a negative number.')
		idw_dw[1].SetColumn('total_deductions')
		idw_dw[1].SetFocus()
		return -1
	End if
			
/*	validate the payment type code
*/
	idw_dw[1].GetChild('payment_type_code', ldwc_child)
	ls_payment_type_code = idw_dw[1].GetItemString(1,'payment_type_code')
	ll_result = ldwc_child.RowCount()
	ll_result = ldwc_child.Find("payment_type_code = '" + ls_payment_type_code + "'",1, ldwc_child.RowCount())
	IF ll_result <= 0 or IsNull(ll_result) THEN
		MessageBox("Award Module - Validation Error","Payment Type must be a valid.",Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn("payment_type_code")
		Return -1
	END IF	
	
	li_no = idw_dw[1].GetItemNumber(1,"benefit_calculation_no")
	ll_rownum = idw_dw[1].GetChild('benefit_calculation_no', ldwc_child_b)
	ll_rownum  = ldwc_child_b.Find("benefit_calculation_no = " + string(li_no),1, ldwc_child_b.RowCount())
	ls_award_freq_code = ldwc_child_b.GetItemString(ll_rownum,"award_freq_code")
	nf_validate_payment_type(ls_payment_type_code, li_error_level, ls_error_message, ls_award_freq_code, ls_days_required_flag)
	IF li_error_level = 2 THEN
		MessageBox("Payment Module - Validation Error", ls_error_message,Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn("payment_type_code")
		Return -1
	END IF

/*	The period end date cannot be before the period start date
*/
	IF DaysAfter(Date(ldtm_award_start_date),Date(ldtm_award_end_date)) <= 0 THEN
		MessageBox("Validation Error","The award end date cannot be less than or equal to the award start date",Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn("award_end_date")
	  	Return -1
	END IF
	
/*	the start date must be the first of the period
*/
	IF ls_payment_freq_code = 'A' THEN
		IF Day(Date(ldtm_award_start_date)) <> 1 OR Month(Date(ldtm_award_start_date)) <> 1 THEN
			MessageBox('Invalid Start Date','For annual awards the start date must be the first of the year.')
			idw_dw[1].SetColumn('award_start_date')
			idw_dw[1].SetFocus()
			Return -1
		END IF
	ELSEIF ls_payment_freq_code = 'M' THEN
		IF Day(Date(ldtm_award_start_date)) <> 1 THEN
			MessageBox('Invalid Start Date','For monthly awards the start date must be the first of the month.')
			idw_dw[1].SetColumn('award_start_date')
			idw_dw[1].SetFocus()
			Return -1
		END IF
	END IF
		
/*  PR4451: for award types 'CA' and 'CL', check that award end date uses the first day of a month
*/			
		ls_string = idw_dw[1].GetItemString(1,'award_type_code')		
		IF ls_string = 'CA' OR ls_string = 'CL' THEN
			IF DAY(date( ldtm_award_end_date )) <> 1 THEN
				Messagebox("Award Module", "The award end date must end on the first day of a month.", Information!)
				idw_dw[1].SetColumn('award_end_date')
				idw_dw[1].SetFocus()						
				RETURN -1
			END IF							
		END IF
/* end of PR4451  */
	IF ldtm_award_start_date <> idw_dw[1].GetItemDateTime(1,'award_start_date', Primary!, TRUE) OR &
		IsNull(idw_dw[1].GetItemDateTime(1,'award_start_date', Primary!, TRUE)) OR &
		idw_dw[1].GetItemStatus(1,0,Primary!) = NewModified! THEN
/*		if the start date has been changed it cannot be before any period for that type that has already been processed
		or any period that has been accepted into a batch
*/									
		SELECT Count(*)
		  INTO :ll_count
		  FROM PERIODIC_AWARD_CONTROL
		 WHERE processed_date IS NOT NULL
			AND period_from_date >= :ldtm_award_start_date
			AND award_type_code = :ls_award_type_code
		 USING SQLCA;

		IF SQLCA.nf_handle_error('Embedded SQL: SELECT from PERIODIC_AWARD_CONTROL','n_awards','nf_check_bus_rule') < 0 THEN
			Return -1
		END IF
		IF ll_count > 0 THEN
			MessageBox('Invalid Start Date','The processing date for this award has already passed.  Please change the start date.')
			idw_dw[1].SetColumn('award_start_date')
			idw_dw[1].SetFocus()
			Return -1
		END IF		
		ls_admin_region_code = idw_dw[1].GetItemString(1,'admin_region_code')
		SELECT Count(*)
		  INTO :ll_count
		  FROM PERIODIC_AWARD
		 WHERE batch_no > 0
			AND award_start_date >= :ldtm_award_start_date
			AND award_type_code = :ls_award_type_code
			AND admin_region_code = :ls_admin_region_code
		 USING SQLCA;

		IF SQLCA.nf_handle_error('Embedded SQL: SELECT from PERIODIC_AWARD','n_awards','nf_check_bus_rule') < 0 THEN
			Return -1
		END IF
		IF ll_count > 0 THEN
/*		if any have a batch number then check out the award date on the current award and make sure it should not be
		part of this batch
*/
			SELECT Count(*)
			  INTO :ll_count
			  FROM PERIODIC_AWARD_CONTROL, PERIODIC_AWARD
			 WHERE ( PERIODIC_AWARD_CONTROL.award_type_code = PERIODIC_AWARD.award_type_code ) AND
					 ( :ls_award_type_code = PERIODIC_AWARD_CONTROL.award_type_code) AND
					 ( :ldtm_award_start_date <= PERIODIC_AWARD_CONTROL.period_from_date ) AND
					 ( :ldtm_award_end_date >= PERIODIC_AWARD_CONTROL.period_to_date ) AND 
					 ( PERIODIC_AWARD_CONTROL.processed_date is NULL ) AND  
					 ( PERIODIC_AWARD_CONTROL.scheduled_processing_date <= getdate() ) AND  
					 ( PERIODIC_AWARD.admin_region_code = :ls_admin_region_code )
			 USING SQLCA;

			IF SQLCA.nf_handle_error('Embedded SQL: select from PERIODIC_AWARD_CONTROL','n_awards','nf_check_bus_rule') < 0 THEN
				Return -1
			END IF
			IF ll_count > 0 THEN
				MessageBox('Invalid Start Date','Awards for this period have already been accepted into a batch.  Remove the batch and try again or change the start date.')
				idw_dw[1].SetColumn('award_start_date')
				idw_dw[1].SetFocus()
				Return -1
			END IF		
		END IF
	END IF
/*	start date must not overlap with another award of the same type
*/
	IF idw_dw[1].GetItemStatus(1,0,Primary!) = NewModified! THEN
		SELECT Max(award_end_date)
		  INTO :ldtm_date
		  FROM PERIODIC_AWARD
		 WHERE award_start_date <= :ldtm_award_start_date
			AND award_type_code = :ls_award_type_code
			AND claim_no = :ll_claim_no
		 USING SQLCA;
	ELSE
		ll_award_no = idw_dw[1].GetItemNumber(1,'award_no')
		SELECT Max(award_end_date)
		  INTO :ldtm_date
		  FROM PERIODIC_AWARD
		 WHERE award_start_date <= :ldtm_award_start_date
			AND award_type_code = :ls_award_type_code
			AND claim_no = :ll_claim_no
			AND award_no <> :ll_award_no
		 USING SQLCA;
	END IF

	IF SQLCA.nf_handle_error('Embedded SQL: SELECT from PERIODIC_AWARD_CONTROL','n_awards','nf_check_bus_rule') < 0 THEN
		Return -1
	END IF
	IF NOT IsNull(ldtm_date) THEN
		IF ldtm_award_start_date < ldtm_date THEN
			MessageBox('Invalid Start Date','This award overlaps with another award of the same type for this claim.  Please change the dates.')
			idw_dw[1].SetColumn('award_start_date')
			idw_dw[1].SetFocus()
			Return -1
		END IF
	END IF		
/*	end date must not overlap with another award of the same type
*/
	IF idw_dw[1].GetItemStatus(1,0,Primary!) <> NewModified! THEN
		ll_award_no = idw_dw[1].GetItemNumber(1,'award_no')
		SetNull(ldtm_date)
		SELECT Min(award_start_date)
		  INTO :ldtm_date
		  FROM PERIODIC_AWARD
		 WHERE award_start_date > :ldtm_award_start_date
			AND award_type_code = :ls_award_type_code
			AND claim_no = :ll_claim_no
			AND award_no <> :ll_award_no
		 USING SQLCA;

		IF SQLCA.nf_handle_error('Embedded SQL: SELECT from PERIODIC_AWARD_CONTROL','n_awards','nf_check_bus_rule') < 0 THEN
			Return -1
		END IF
		IF NOT IsNull(ldtm_date) THEN
			IF ldtm_award_end_date > ldtm_date THEN
				MessageBox('Invalid End Date','This award overlaps with another award of the same type for this claim.  Please change the dates.')
				idw_dw[1].SetColumn('award_end_date')
				idw_dw[1].SetFocus()
				Return -1
			END IF
		END IF		
	END IF

	IF ldtm_award_end_date > idw_dw[1].GetItemDateTime(1,'award_end_date', Primary!, TRUE) OR &
		IsNull(idw_dw[1].GetItemDateTime(1,'award_end_date', Primary!, TRUE)) OR &
		idw_dw[1].GetItemStatus(1,0,Primary!) = NewModified! THEN
/*		if the end date has been changed (extended) it cannot be before any period for that type that has already been processed
*/									
		SELECT Count(*)
		  INTO :ll_count
		  FROM PERIODIC_AWARD_CONTROL
		 WHERE processed_date IS NOT NULL
			AND period_to_date >= :ldtm_award_end_date
			AND award_type_code = :ls_award_type_code
		 USING SQLCA;

		IF SQLCA.nf_handle_error('Embedded SQL: SELECT from PERIODIC_AWARD_CONTROL','n_awards','nf_check_bus_rule') < 0 THEN
			Return -1
		END IF
		IF ll_count > 0 THEN
			MessageBox('Invalid End Date','The processing date for this award has already passed.  Please change the dates.')
			idw_dw[1].SetColumn('award_end_date')
			idw_dw[1].SetFocus()
			Return -1
		END IF		
	ELSEIF ldtm_award_end_date < idw_dw[1].GetItemDateTime(1,'award_end_date', Primary!, TRUE) OR &
		IsNull(idw_dw[1].GetItemDateTime(1,'award_end_date', Primary!, TRUE)) OR &
		idw_dw[1].GetItemStatus(1,0,Primary!) = NewModified! THEN
/*		if the end date has been changed (end date moved back) it cannot be before any period for that type that has already been processed
*/									
		SELECT Count(*)
		  INTO :ll_count
		  FROM PERIODIC_AWARD_CONTROL
		 WHERE processed_date IS NOT NULL
			AND period_to_date > :ldtm_award_end_date
			AND award_type_code = :ls_award_type_code
		 USING SQLCA;

		IF SQLCA.nf_handle_error('Embedded SQL: SELECT from PERIODIC_AWARD_CONTROL','n_awards','nf_check_bus_rule') < 0 THEN
			Return -1
		END IF
		IF ll_count > 0 THEN
			MessageBox('Invalid End Date','The processing date for this award has already passed.  Please change the dates.')
			idw_dw[1].SetColumn('award_end_date')
			idw_dw[1].SetFocus()
			Return -1
		END IF		
	END IF
	
	// The award start date must not be less than the minimum period from date
	// (from PERIODIC_AWARD_CONTROL) of all unprocessed periods for the award type if the award is unprocessed.
	
	SELECT	Min(period_from_date)
	INTO		:ldtm_min_period_from_date
	FROM		PERIODIC_AWARD_CONTROL
	WHERE	processed_date IS NULL
	AND		award_type_code = :ls_award_type_code
	USING SQLCA;
	
	IF SQLCA.nf_handle_error('Embedded SQL: SELECT Min(period_from_date) from PERIODIC_AWARD_CONTROL','n_awards','nf_check_bus_rule') < 0 THEN
		Return -1
	END IF
	
	SELECT	Count(*)
	INTO		:li_payment_count
	FROM		PAYMENT	a
	WHERE	claim_no		= :ll_claim_no
	AND		award_no	= :ll_award_no
	AND		award_no	<> 0
	USING SQLCA;
	
	IF SQLCA.nf_handle_error('Embedded SQL: SELECT Count from PAYMENT','n_awards','nf_check_bus_rule') < 0 THEN
		Return -1
	END IF
	
	IF ldtm_award_start_date < ldtm_min_period_from_date AND li_payment_count = 0 THEN
		MessageBox('Invalid Start Date','The start date must be on or after ' + String(ldtm_min_period_from_date,'yyyy-mm-dd') + ', which is the minimum '&
											+ '~nautomated award scheduled processing date for a '+ls_award_type_code+' award.'&
											+ '~nPlease change the dates.')
		idw_dw[1].SetColumn('award_start_date')
		idw_dw[1].SetFocus()
		Return -1
	END IF
	
/*	for pen, ltd, RWI: cannot start before effective_from_date
*/
	CHOOSE CASE ls_award_type_code
		CASE 'PEN', 'LTD', 'RWI'
			IF ldtm_award_start_date < ldtm_effective_from_date THEN
				MessageBox('Invalid Start Date','The start date must not be before the effective from date.')
				idw_dw[1].SetColumn('award_start_date')
				idw_dw[1].SetFocus()
				Return -1
			END IF
	END CHOOSE
/*	if the effective date is in the future then ...
*/
	IF ldtm_effective_from_date > ldtm_server_date THEN
		SELECT Min(period_from_date)
		  INTO :ldtm_date
		  FROM PERIODIC_AWARD_CONTROL
		 WHERE period_from_date >= :ldtm_effective_from_date
			AND award_type_code = :ls_award_type_code
			AND scheduled_processing_date IS NULL
	    USING SQLCA;

		IF SQLCA.nf_handle_error('Embedded SQL: SELECT from PERIODIC_AWARD_CONTROL','n_awards','nf_check_bus_rule') < 0 THEN
			Return -1
		END IF
		IF NOT IsNull(ldtm_date) THEN
			MessageBox('Invalid Start Date','The start date must be after ' + String(ldtm_date,'yyyy-mm-dd') + '.  Please change the dates.')
			idw_dw[1].SetColumn('award_start_date')
			idw_dw[1].SetFocus()
			Return -1
		END IF		
	END IF

/*	determine if the end date is valid
	get the max end date based on the start date
*/
	ldtm_date = nf_get_end_date(ldtm_award_start_date,ll_no_periods,ls_payment_freq_code)

/*	award end date cannot be beyond the calculated date 
	unless...
	Type CA can't be beyond the end of the year
*/
	ls_string = idw_basic_claim.GetItemString(1,'legislation_code')

	IF ls_award_type_code = 'CA' THEN
		IF Year(Date(ldtm_award_end_date)) > Year(Date(ldtm_award_start_date)) THEN
			
			IF DaysAfter(Date(ldtm_award_start_date),Date(ldtm_award_end_date)) > 365 THEN
				IF messagebox('Warning!','The period for this Care Allowance exceeds 1 year.~r~nWould you like to Continue?',question!,yesno!) = 2 THEN
					idw_dw[1].SetColumn('award_end_date')
					idw_dw[1].SetFocus()
					Return -1
				END IF
				
	// PR 4451 (cont): remove warning message that pops up when Care Allowance goes into 
	// folowing year, as per PR4451, since now, the award end date must end on the 1st of a month

			END IF
		END IF
/*	type CL can't be beyond end of next year		
*/
	ELSEIF ls_award_type_code = 'CL' THEN
		IF Year(Date(ldtm_award_end_date)) > Year(Date(ldtm_award_start_date)) + 1 THEN
			
			IF DaysAfter(Date(ldtm_award_start_date),Date(ldtm_award_end_date)) > 730 THEN  // changed from >= to just > (to satisfy requirement, (PR4451), for an end date on 1st of month as per PR4451, and not raise message box)
				IF MessageBox('Warning','The period for this Clothing Allowance exceeds 2 years.~r~nWould you like to Continue?',question!,yesno!) = 2 THEN
					idw_dw[1].SetColumn('award_end_date')
					idw_dw[1].SetFocus()
					Return -1
				END IF
			END IF
		END IF
		
/*	for type PEN and legislation code = P48 end date cannot be more than 5 years after the award start
*/
	ELSEIF ls_award_type_code = 'PEN' AND ls_string = 'P48' THEN
		IF ldtm_award_end_date > DateTime(Date(Year(Date(ldtm_award_start_date)) + 5, Month(Date(ldtm_award_start_date)), Day(Date(ldtm_award_start_date)))) THEN
			MessageBox('Invalid Award End Date','The award end date is beyond the allowable duration ( 5 years from the award start date).')
			idw_dw[1].SetColumn('award_end_date')
			idw_dw[1].SetFocus()
			Return -1
		END IF

/*	for type PEN and legislation code =  P82 and claimant less than 65 and end date beyond calculated date 
	then error
	if more than one recipient then use the no_authorization_periods as the max
*/
	ELSEIF ls_award_type_code = 'PEN' AND ls_string = 'P82' THEN
		IF idw_dw[2].RowCount() > 1  OR ( idw_dw[2].RowCount() =  1 AND idw_dw[2].GetItemString(1,'recipient_type_code') <> 'I') THEN
			IF ldtm_award_end_date > ldtm_date THEN
				MessageBox('Invalid Award End Date','The award end date is beyond the allowable duration of : ' + String(ldtm_date,'yyyy-mm-dd') + '.')
				idw_dw[1].SetColumn('award_end_date')
				idw_dw[1].SetFocus()
				Return -1
			END IF
		ELSE						// must be IF idw_[2].GetItemString(1,'recipient_type_code') = 'I' THEN
/*			determine if SS or C
*/
			ll_no = idw_dw[2].GetItemNumber(1,'recipient_no')
			idw_dw[2].GetChild('recipient_no', ldwc_child)
			ll_no = ldwc_child.Find('individual_no = ' + String(ll_no),0,ldwc_child.RowCount())
			IF ll_no > 0 THEN
				IF ldwc_child.GetItemString(ll_no,'claim_role_code') = 'C' THEN
				   ldtm_birth_date = idw_basic_claim.GetItemDateTime(1,'birth_date')
				ELSEIF ldwc_child.GetItemString(ll_no,'claim_role_code') = 'SS' THEN
					ll_no = idw_dw[2].GetItemNumber(1,'recipient_no')
					SetNull(ldtm_birth_date)
					SELECT birth_date
					  INTO :ldtm_birth_date
					  FROM INDIVIDUAL
					 WHERE individual_no = :ll_no
					 USING SQLCA;

					IF SQLCA.nf_handle_error('Embedded SQL: select from INDIVIDUAL','n_awards','nf_check_bus_rule') < 0 THEN
						Return -1
					END IF		
				ELSE
					SetNull(ldtm_birth_date)
				END IF
			ELSE
				SetNull(ldtm_birth_date)
			END IF

			IF IsNull(ldtm_birth_date) THEN
				IF ldtm_award_end_date > ldtm_date THEN
					MessageBox('Invalid Award End Date','The award end date is beyond the allowable duration of : ' + String(ldtm_date,'yyyy-mm-dd') + '.')
					idw_dw[1].SetColumn('award_end_date')
					idw_dw[1].SetFocus()
					Return -1
				END IF	
			ELSE
				ll_no = nf_calculate_age(Date(ldtm_birth_date))
				IF ll_no >= 65 THEN
/*					the end date can be 5 years after the start date
*/
					IF ldtm_award_end_date > DateTime(Date(Year(Date(ldtm_award_start_date)) + 5, Month(Date(ldtm_award_start_date)), Day(Date(ldtm_award_start_date)))) THEN
						MessageBox('Invalid Award End Date','The award end date is more than 5 years after the start date.')
						idw_dw[1].SetColumn('award_end_date')
						idw_dw[1].SetFocus()
						Return -1
					END IF	
				ELSE
					IF ldtm_award_end_date > ldtm_date THEN
						MessageBox('Invalid Award End Date','The award end date is beyond the allowable duration of : ' + String(ldtm_date,'yyyy-mm-dd') + ' .')
						idw_dw[1].SetColumn('award_end_date')
						idw_dw[1].SetFocus()
						Return -1
					END IF	
				END IF
			END IF
		END IF

/*		last case
*/
	ELSEIF ldtm_award_end_date > ldtm_date THEN
		MessageBox('Invalid Award End Date','The award end date is beyond the allowable duration of : ' + String(ldtm_date,'yyyy-mm-dd') + '.')
		idw_dw[1].SetColumn('award_end_date')
		idw_dw[1].SetFocus()
		Return -1
	END IF


	IF ldtm_award_end_date <> idw_dw[1].GetItemDateTime(1,'award_end_date', Primary!, TRUE) &
		AND NOT IsNull(idw_dw[1].GetItemDateTime(1,'award_end_date', Primary!, TRUE)) THEN
/*		if the date has changed it can only be moved back if authorized
*/
		IF NOT IsNull(idw_dw[1].GetItemDateTime(1,'authorized_date')) THEN
			IF idw_dw[1].GetItemDateTime(1,'award_end_date') > idw_dw[1].GetItemDateTime(1,'award_end_date', Primary!, TRUE) THEN
/*			the person must have authorization to extend it
*/
				ls_admin_region_code = idw_dw[1].GetItemString(1,'admin_region_code')
				ldec_saved_limit = idw_dw[1].GetItemNumber(1,'total_award_amount')

				ls_string = vgst_user_profile.user_id
				ldec_new_limit = 0
				SELECT authorization_limit
				  INTO :ldec_new_limit 
				  FROM Authorizations
				 WHERE authorized_by_login_id = :ls_string
				   AND authorization_type_code = :ls_authorization_type_code
					AND admin_region_code = :ls_admin_region_code
					AND active_flag = 'Y'
					AND effective_from_date <= :ldtm_server_date
					AND IsNull(effective_to_date,getdate()) >= :ldtm_server_date
			  	USING SQLCA;

				IF SQLCA.nf_handle_error('Embedded SQL: SELECT from Authorizations','n_awards','nf_check_bus_rule') < 0 THEN
					Return -1
				END IF

				IF ldec_new_limit < ldec_saved_limit THEN
					MessageBox('Invalid End Date','This award has been authorized.  The end date can only be extended by some one with an authorization limit equal to  or greater than ' + String(ldec_saved_limit,'##,##0.00') + '.')
					idw_dw[1].SetColumn('award_end_date')
					idw_dw[1].SetFocus()
					Return -1
				END IF
			END IF
		END IF
	END IF

	IF (idw_dw[1].GetItemString(1,'award_type_code') = 'PEN' OR idw_dw[1].GetItemString(1,'award_type_code') = 'LTD' OR idw_dw[1].GetItemString(1,'award_type_code') = 'RWI') THEN
		IF ldtm_award_end_date > ldtm_benefit_end_date THEN
			MessageBox('Invalid Award End Date','The award end date is beyond the benefit end date of : ' + String(Date(ldtm_benefit_end_date),'yyyy-mm-dd') + '.')
			idw_dw[1].SetColumn('award_end_date')
			idw_dw[1].SetFocus()
			Return -1
		END IF
	END IF
/*	admin region must be PRV for award types of PEN or TSB
*/
	IF (idw_dw[1].GetItemString(1,'award_type_code') = 'PEN' OR idw_dw[1].GetItemString(1,'award_type_code') = 'TSB') &
		AND idw_dw[1].GetItemString(1,'admin_region_code') <> 'PRV' THEN
		MessageBox('Invalid Admin Region', 'For award types of "PEN" the admin region must be Provincial.')
		Return -1
	END IF

/*	if award type is other than CL or CA then the opening must be active
*/
	IF NOT (idw_dw[1].GetItemString(1,'award_type_code') = 'CL' OR idw_dw[1].GetItemString(1,'award_type_code') = 'CA') THEN
		SetNull(ldtm_benefit_end_date)
		SELECT benefit_end_date
		  INTO :ldtm_benefit_end_date
		  FROM OPENING
		 WHERE claim_no = :ll_claim_no
		   AND opening_no = :ll_opening_no
		 USING SQLCA;
		IF SQLCA.nf_handle_error('Embedded SQL: Select from OPENING', 'n_awards','nf_check_bus_rule') < 0 THEN
			Return -1
		END IF

		IF NOT IsNull(ldtm_benefit_end_date) THEN
			IF ldtm_benefit_end_date < ldtm_server_date THEN
				MessageBox('Invalid Opening','The opening must be active for the award chosen.')
				Return -1
			END IF
		END IF
	END IF

/*	The total payment amount can exceed the users' authorization limit
	but will be held until authorized
*/
	ldec_total_payment_amount = idw_dw[1].GetItemDecimal(1,"total_payment_amount")
	ic_authorization_limit = nf_get_authorization_limit(ls_authorization_type_code)
	IF ldec_total_payment_amount > ic_authorization_limit THEN
		MessageBox("Award Module","You are only authorized to make awards up to " + string(ic_authorization_limit,'$###,##0.00') + &
		"~r~nThis award will be held until it is authorized.",Exclamation!)
	END IF


/*	Validate  transaction
*/
	ldec_temp_txn_amount = 0
	ll_count = idw_dw[2].RowCount()
	ll_no = 1

	DO WHILE ll_no <= ll_count
		ldec_temp_txn_amount += idw_dw[2].GetItemDecimal(ll_no,"txn_amount")
	   ll_no = ll_no + 1
	LOOP

/*	The total of the amounts payable to recipients must be equal to the payment amount
*/
	IF ldec_temp_txn_amount <> ldec_total_payment_amount THEN
		MessageBox("Award Module - Validation Error","The amount payable to each recipient does not sum to the total payment amount",Exclamation!)
		idw_dw[2].SetFocus()
		idw_dw[2].SetColumn("txn_amount")
		Return -1
	END IF


	ll_transaction_rowcount = idw_dw[2].RowCount()

	IF ll_transaction_rowcount > 1 THEN
		IF nf_check_dup_recipients() < 0 THEN
			Return -1
		END IF
	END IF
	IF ldec_total_payment_amount <= ic_authorization_limit THEN
/*		if the award could be changed because no payments existed and the user was authorized then we need to 
		re-authorize ( or authorize in the case of a new entry) the award	
		otherwise the original authorizer's id will stay on the entry
		- in the routine that protect/unprotects the columns the id is blanked out at the appropriate authorization
*/
		IF ib_unpaid_authorizable_award = TRUE THEN
			idw_dw[1].SetItem(1,"authorized_user_id",vgst_user_profile.user_id)
			idw_dw[1].SetItem(1,"authorized_date",ldtm_server_date)
		END IF
	ELSEIF ib_unpaid_authorizable_award = TRUE THEN
		idw_dw[1].SetItem(1,"authorized_user_id",'')
		idw_dw[1].SetItem(1,"authorized_date",ld_null_date)
	END IF
	
	/*
	RTW Incentive checks
	*/
		
	IF ls_award_type_code = 'RWI' THEN
		li_next_year = Year(Date(ldtm_effective_from_date)) + 1
		li_month = Month(Date(ldtm_effective_from_date))
		li_day = Day(Date(ldtm_effective_from_date))
		ld_year_after_effective_from = Date(String(li_next_year) + '/' + String(li_month) + '/' + String(li_day))
		
		If ld_year_after_effective_from < Date(ldtm_award_end_date) THEN
			MessageBox('RTW Incentive Error','The award end date for a RTW Incentive award must not be more' &
												+		'~nthan one year after the benefit calculation effective date.')
			RETURN -1
		END IF
		
		IF idw_dw[1].GetItemDecimal(1,'total_deductions') <> 0 THEN
			MessageBox('RTW Incentive Error','The total deductions for a RTW Incentive award must be zero.')
			RETURN -1
		END IF
		
		ldec_award_amount = idw_dw[1].GetItemDecimal(1,'total_award_amount')
		idw_dw[1].GetChild('benefit_calculation_no',ldwc_child)
		ldec_bencalc_award_amount = ldwc_child.GetItemDecimal(ldwc_child.GetRow(),'award_amount')
		IF ldec_award_amount <> ldec_bencalc_award_amount THEN
			MessageBox('RTW Incentive Error','The total award amount for a RTW Incentive award must' &
												+		'~nequal the award amount of the benefit calculation.')			
			RETURN -1
		END IF
		
	END IF
	
	
	ll_recipient_cntr = 1
	DO WHILE ll_recipient_cntr <= ll_transaction_rowcount
		ls_recipient_type_code = idw_dw[2].GetItemString(ll_recipient_cntr,"recipient_type_code")
	
	/* PR 4649:  change code that checked for a recipient of type 'I' for award type of 
		Pensions and prohibited awards for anything other than 'I'ndividual. 
		Business rules now allow that recipient type of 'Other Payee' should be allowed as a payee for Pension 
		awards, so that users can issue a recovery on Overpayment to WHSCC. This PR is also found in n_payment
	*/
		IF ls_award_type_code = 'PEN' AND ls_recipient_type_code <> 'I' AND ls_recipient_type_code <> 'O' THEN
			MessageBox("Award Module - Validation Error","Recipient Type - " + ls_recipient_type_code + " is invalid.  Must be Individual or 'Other Payee' for Pension Awards.",Exclamation!)
			idw_dw[2].SetFocus()
			idw_dw[2].SetColumn("recipient_type_code")
			idw_dw[2].ScrollToRow(ll_recipient_cntr)
			Return -1
		END IF
	/* End PR 4649 */	
		IF ls_recipient_type_code <> 'I' AND ls_recipient_type_code <> 'O' &
				AND ls_recipient_type_code <> 'V' AND ls_recipient_type_code <> 'M' THEN
			MessageBox("Award Module - Validation Error","Recipient Type - " + ls_recipient_type_code + " is invalid.",Exclamation!)
			idw_dw[2].SetFocus()
			idw_dw[2].SetColumn("recipient_type_code")
			Return -1
		END IF
		ll_recipient_no = idw_dw[2].GetItemNumber(ll_recipient_cntr,"recipient_no")
		IF nf_validate_recipient(ls_recipient_type_code,ll_recipient_no,ll_recipient_cntr,ls_award_type_code) < 0 THEN
			idw_dw[2].SetFocus()
			idw_dw[2].SetColumn("recipient_no")
			Return -1
		END IF
		
		/*
		Do Age Check
		*/
		IF ls_award_type_code <> 'CA' AND ls_award_type_code <> 'CL' THEN
			IF uof_check_age ( ll_recipient_no, ll_claim_no, date(ldtm_award_end_date), ls_opening ) < 0 THEN
				idw_dw[2].SetFocus()
				idw_dw[2].SetColumn("recipient_no")
				Return -1
			END IF
		END IF
		
		ld_txn_amount = idw_dw[2].GetItemDecimal(ll_recipient_cntr,'txn_amount')
		ls_payment_method_code = idw_dw[2].GetItemString(ll_recipient_cntr,'payment_method_code')
		
		If ld_txn_amount > 0 and ls_payment_method_code = 'I' Then 
			MessageBox('Invalid payment method','Payment method "Inapplicable" is only valid if the transaction amount is zero.')
			idw_dw[2].SetFocus()
			idw_dw[2].SetColumn("payment_method_code")
			RETURN -1
		END IF
		
		If ld_txn_amount = 0 AND ls_payment_method_code <> 'I' Then
			MessageBox('Invalid payment method','Payment method must be set to "Inapplicable" when the transaction amount is zero.')
			idw_dw[2].SetFocus()
			idw_dw[2].SetColumn("payment_method_code")
			RETURN -1
		END IF
		
		IF ls_award_type_code = 'RWI' THEN
			IF ls_recipient_type_code <> 'I' AND ls_recipient_type_code <> 'O' THEN
				SELECT	recipient_type_desc
				INTO		:ls_recipient_type_desc
				FROM		Recipient_Type
				WHERE	recipient_type_code = :ls_recipient_type_code
				USING SQLCA;
				
				SQLCA.nf_handle_error('Embedded SQL: SELECT from Recipient_Type','n_awards','nf_check_bus_rule')
				
				MessageBox("Award Module - Validation Error","Recipient Type - " + ls_recipient_type_desc + " is invalid for a RTW Incentive Award.",Exclamation!)
				idw_dw[2].SetFocus()
				idw_dw[2].SetColumn("recipient_type_code")
			Return -1
			END IF
		END IF
		
		ll_recipient_cntr++
	LOOP
Return 1
end function

public function integer nf_validate_recipient (string as_recipient_type_code, long al_recipient_no, long al_row, string as_award_type_code);LONG             ll_result, ll_child_row
STRING           ls_type_code
DATAWINDOWCHILD  ldwc_child


	CHOOSE CASE as_recipient_type_code
		CASE ''
			Return -1
	   CASE "I"
			nf_set_recipient_filter(FALSE)
			idw_dw[2].GetChild('recipient_no',ldwc_child)
			ll_child_row = ldwc_child.Find('individual_no = ' + String(al_recipient_no),1,ldwc_child.RowCount())
			IF ll_child_row < 1 THEN
				MessageBox('Invalid Individual','The individual selected is not valid.')
				Return -1
			END IF
			IF as_award_type_code = 'PEN' OR as_award_type_code = 'TSB' THEN
				IF ldwc_child.GetItemString(ll_child_row,'pen_survivor_eligibility_flag') = 'N' THEN
					MessageBox('Invalid Individual','This individual is not eligible to recieve benefits.')
					Return -1
				END IF
			END IF
			SELECT sin_no
			  INTO :ll_result
			  FROM INDIVIDUAL
			 WHERE individual_no = :al_recipient_no
			 USING SQLCA;
		   
			IF SQLCA.nf_handle_error("Embedded SQL: Retrieve on INDIVIDUAL","n_award","nf_validate_recipient") < 0 THEN
				Return -1
			END IF

			IF ll_result = 0 THEN
				CHOOSE CASE as_award_type_code
					CASE 'PEN'
						IF ldwc_child.GetItemString(ll_child_row,'claim_role_code') = 'C' &
							OR ldwc_child.GetItemString(ll_child_row,'claim_role_code') = 'SS' THEN
							MessageBox('Warning',"The individual's SIN is not recorded.  This should be entered for tax purposes.")
						END IF
					CASE 'LTD', 'RWI'
						MessageBox('Error',"The individual's SIN is not recorded.  Award not allowed.", StopSign!)
						Return -1
				END CHOOSE
			END IF
			idw_dw[2].SetItem(al_row,'recipient_sub_type_code', ' ')
			Return 1
	   CASE ELSE
/*		If recipient is a payee, read the address from Service Providers table          
*/
			IF IsNull(as_recipient_type_code) OR IsNull(al_recipient_no) THEN
				MessageBox('Warning','Recipient number or type is missing')
				Return -1
			END IF
		   SELECT PROVIDER.provider_sub_type_code  
	   	  INTO :ls_type_code
		     FROM PROVIDER  
		    WHERE ( PROVIDER.provider_no = :al_recipient_no ) AND  
			   	 ( PROVIDER.provider_type_code = :as_recipient_type_code) AND
      	       ( PROVIDER.active_flag = 'Y' )  
	       USING SQLCA ;
		   ll_result = SQLCA.nf_handle_error("Embedded SQL: Retrieve on SERVICE_PROVIDER","n_award","nf_validate_address")
		   IF ll_result < 0 THEN
	   		return -1
		   ELSEIF ll_result = 100 THEN
				MessageBox('Error','Invalid provider number.')
			   Return -1
	   	END IF
			idw_dw[2].SetItem(al_row,'recipient_sub_type_code', ls_type_code)
	END CHOOSE	
Return 0
end function

public function integer nf_get_bank (long al_recipient_no, string as_recipient_type_code, ref string as_bank_no, ref string as_bank_transit_no, ref string as_bank_account_no, ref string as_name);LONG	ll_result
STRING	ls_given_names, ls_last_name

IF NOT IsNull(al_recipient_no) THEN
	IF as_recipient_type_code = 'I' THEN
		SELECT bank_no, bank_transit_no, bank_account_no, given_names + ' ' + last_name
		INTO :as_bank_no, :as_bank_transit_no, :as_bank_account_no, :as_name
		FROM INDIVIDUAL
		WHERE individual_no = :al_recipient_no
		USING SQLCA;
	ELSE
		SELECT bank_no, bank_transit_no, bank_account_no, name
		INTO       :as_bank_no, :as_bank_transit_no, :as_bank_account_no, :as_name
		FROM     BANK_INFO a 
		RIGHT OUTER JOIN PROVIDER b ON a.recipient_no = b.provider_no 
														 AND a.recipient_type_code = b.provider_type_code
		WHERE b.provider_no = :al_recipient_no
		AND         b.provider_type_code = :as_recipient_type_code
		USING   SQLCA;
	END IF
	
	ll_result = SQLCA.nf_handle_error('n_awards','nf_get_bank','SELECT bank_no, bank_transit_no, bank_account_no')
	
	IF ll_result  < 0 THEN Return -1

	IF ll_result = 100 THEN Return -1
			
	IF IsNull(as_bank_no) THEN as_bank_no = ''
	IF IsNull(as_bank_transit_no) THEN as_bank_transit_no = ''
	IF IsNull(as_bank_account_no) THEN as_bank_account_no = ''
ELSE
	RETURN -1
END IF
		
Return 0
end function

on n_awards.create
call super::create
end on

on n_awards.destroy
call super::destroy
end on

