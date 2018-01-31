$PBExportHeader$n_ephysio_invoice.sru
forward
global type n_ephysio_invoice from nonvisualobject
end type
end forward

global type n_ephysio_invoice from nonvisualobject
end type
global n_ephysio_invoice n_ephysio_invoice

forward prototypes
public function long nf_get_next_rehab_invoice_no ()
public function long nf_get_next_payment_no ()
public function long nf_get_next_unapplied_txn_no ()
public function long nf_get_individual_no (long al_claim_no)
public function string nf_get_claim_admin_region_code (long al_claim_no)
public function integer nf_get_banking_info (long al_recipient_no, string as_recipient_type_code, ref string as_bank_no, ref string as_bank_transit_no, ref string as_bank_account_no, ref string as_name)
public function integer nf_get_tax_rate (integer ai_option, string as_province_code, date adt_datecheck)
public function decimal nf_get_tax_rate_percent (integer ai_tax_rate_no)
public function integer nf_check_address (string as_address_line1, string as_address_line2, string as_city, string as_country_code)
public function string nf_get_physio_contract_flag (long al_provider_no, string as_provider_type)
public function date nf_get_thursday_processing_date (string as_recipient_type_code)
public function boolean nf_check_valid_info (long al_no, string as_type, integer ai_condition)
public function integer nf_check_items_vs_applied (long al_claim_no, integer ai_task_no, date adt_date, long al_xref_no, integer ai_item_no, integer ai_check)
public function decimal nf_get_item_unit_price (long al_billable_xref_no, date adt_service_date)
public function string nf_get_ephysio_flag (long al_no, string as_type)
public function string nf_get_item_description (long al_xref_no)
public function string nf_get_provider_name (long al_no, string as_type)
public function string nf_get_recipient_subtype (long al_no, string as_type)
public function long nf_get_rehab_invoice_no_from_docid (long al_docid)
public function boolean nf_provider_setup_on_website (long al_no, string al_type)
public function string nf_get_fixed_fee_flag (long al_xref_no)
public function string nf_get_prov_state_code (long al_no, string as_type_code)
public function date nf_get_min_service_date_from_line_items (long al_claim_no, integer ai_task_no)
public function boolean nf_set_allow_tax (long al_recipient_no, string as_recipient_type, long al_provider_no, string as_provider_type, long al_xref_no)
public function boolean nf_recipient_eligible_for_automated_pay (long al_no, string as_type)
public function boolean nf_total_payment_amount_enterable (long al_invoice_submitted_by, string as_invoice_submitted_by_type, long al_payable_to, string as_payable_to_type)
end prototypes

public function long nf_get_next_rehab_invoice_no ();LONG	ll_rehab_invoice_no

/*	To ensure that we get the next number without, Update the Last_Rehab_Invoice_No table incrementing the 
	last_rehab_invoice_no by 1  (This will lock it so no one else can get in). Then, read it back          
*/
UPDATE Last_Rehab_Invoice_No SET last_rehab_invoice_no = last_rehab_invoice_no + 1 using SQLCA;
SQLCA.nf_handle_error("n_ephysio_invoices", "nf_get_next_rehab_invoice_no()", "UPDATE Last_Rehab_Invoice_No")

CHOOSE CASE SQLCA.SQLNRows
/*	If update was successful (ie. SQLNRows would equal 1), read back the identifier */	
	CASE 1
		
		SELECT Last_Rehab_Invoice_No.last_rehab_invoice_no INTO :ll_rehab_invoice_no FROM Last_Rehab_Invoice_No USING SQLCA;
		SQLCA.nf_handle_error("n_ephysio_invoices", "nf_get_next_rehab_invoice_no()", "SELECT Last_Rehab_Invoice_No.last_rehab_invoice_no")
			
	CASE ELSE
/*		if anything other than 1 record found, display error*/
		SQLCA.nf_rollback_transaction()
		IF SQLCA.SQLCode <> 0 THEN
			Error.Text 				= "Error during rollback of Last_Payment_No in function nf_get_next_rehab_invoice_no"
			Error.WindowMenu	=	"n_ephysio_invoices"
			Error.Object				=	""
			Error.ObjectEvent		=	"nf_get_next_rehab_invoice_no"
			SignalError()
		END IF		
		
		MessageBox("E-Physio - Data Integrity Error", string(SQLCA.SQLNRows) + " record(s) found in Last_Rehab_Invoice_No~r~nPlease call the help desk",Exclamation!)
		RETURN -1

	END CHOOSE
		
	RETURN ll_rehab_invoice_no
	
end function

public function long nf_get_next_payment_no ();LONG	ll_payment_no

/*	To ensure that we get the next number without, Update the Last_Payment_No table incrementing the 
	last_payment_no by 1  (This will lock it so no one else can get in). Then, read it back          
*/
UPDATE Last_Payment_No SET last_payment_no = last_payment_no + 1 using SQLCA;
SQLCA.nf_handle_error("n_ephysio_invoices", "nf_get_next_payment_no()", "UPDATE Last_Payment_No.last_payment_no")

CHOOSE CASE SQLCA.SQLNRows
/*	If update was successful (ie. SQLNRows would equal 1), read back the identifier */	
	CASE 1
		
		SELECT Last_Payment_No.last_payment_no INTO :ll_payment_no FROM Last_Payment_No using SQLCA;
		SQLCA.nf_handle_error("n_ephysio_invoices", "nf_get_next_payment_no()", "SELECT Last_Payment_No.last_payment_no")
			
	CASE ELSE
/*		if anything other than 1 record found, display error*/
		SQLCA.nf_rollback_transaction()
		IF SQLCA.SQLCode <> 0 THEN
			Error.Text 				= "Error during rollback of Last_Payment_No in function nf_get_next_payment_no"
			Error.WindowMenu	= "n_ephysio_invoice"
			Error.Object				= ""
			Error.ObjectEvent		= "nf_get_next_payment_no"
			SignalError()
		END IF		
		
		MessageBox("E-Physio - Data Integrity Error", string(SQLCA.SQLNRows) + " record(s) found in Last_Payment_No~r~nPlease call the help desk",Exclamation!)
		RETURN -1

	END CHOOSE
		
RETURN ll_payment_no

end function

public function long nf_get_next_unapplied_txn_no ();LONG	ll_unapplied_txn_no

/*	To ensure that we get the next number without, Update the Last_Claim_Txn_no table incrementing the  
	last_txn_no by 1  (This will lock it so no one else can get in). Then, read it back                 
*/
UPDATE Last_Claim_Txn_No SET last_txn_no = last_txn_no + 1 USING SQLCA;
SQLCA.nf_handle_error("n_ephysio_invoice", "nf_get_next_unapplied_txn_no()", "UPDATE Last_Claim_Txn_No")

CHOOSE CASE SQLCA.SQLNRows
/*		If update was successful (ie. SQLNRows would equal 1), read back the identifier
*/
	CASE 1
		
		SELECT Last_Claim_Txn_No.last_txn_no INTO :ll_unapplied_txn_no FROM Last_Claim_Txn_No USING SQLCA;
		SQLCA.nf_handle_error("n_ephysio_invoice", "nf_get_next_unapplied_txn_no()", "SELECT Last_Claim_Txn_No.last_txn_no ")

	CASE ELSE
/*		If anything other than 1 record found, display error
*/
		MessageBox("Data Integrity Error", string(SQLCA.SQLNRows) + " record(s) found in Last_Claim_Txn_No~r~nPlease call the help desk",Exclamation!)
		
		SQLCA.nf_rollback_transaction()
		IF SQLCA.SQLCode <> 0 THEN
			Error.Text 				= "Error during rollback of Last_Claim_Txn_No in function nf_get_next_unapplied_txn_no"
			Error.WindowMenu	=	""
			Error.Object				=	""
			Error.ObjectEvent		=	"nf_get_next_unapplied_txn_no"
			SignalError()
		END IF	
		
		RETURN -1
		
	END CHOOSE

RETURN ll_unapplied_txn_no

end function

public function long nf_get_individual_no (long al_claim_no);LONG			ll_individual_no

SELECT  	individual_no
INTO			:ll_individual_no
FROM 		CLAIM
WHERE 	claim_no = :al_claim_no
USING 		SQLCA;
SQLCA.nf_handle_error("n_ephysio_invoices", "nf_get_individual_no()", "SELECT 	individual_no")

IF ISNULL(ll_individual_no) OR ll_individual_no < 0 THEN  ll_individual_no = 0

RETURN ll_individual_no

end function

public function string nf_get_claim_admin_region_code (long al_claim_no);STRING		ls_admin_region_code
	
SELECT 	admin_region_code
INTO			:ls_admin_region_code
FROM 		CLAIM 
WHERE 	claim_no = :al_claim_no
USING 		SQLCA;
SQLCA.nf_handle_error("n_ephysio_invoices", "nf_get_claim_admin_region_code()", "SELECT admin_region_code")

IF isnull(ls_admin_region_code) then ls_admin_region_code = ''
	
RETURN  ls_admin_region_code

end function

public function integer nf_get_banking_info (long al_recipient_no, string as_recipient_type_code, ref string as_bank_no, ref string as_bank_transit_no, ref string as_bank_account_no, ref string as_name);/*
SELECT 	bank_info_no, 			bank_info_purpose_code, recipient_no, 
				recipient_type_code, recipient_sub_type_code, bank_no,
				bank_transit_no, 		bank_account_no
 FROM 		BANK_INFO 
 
 If the provider is set up with banking information (BANKING_INFO table), then the provider will be reimbursed through a Direct Deposit payment method;
 otherwise, the provider will be reimbursed through an Automated Cheque. The payment will be issued on the next scheduled Thursday.
 
 */ 
LONG		ll_result
STRING		ls_given_names, ls_last_name

IF IsNull(al_recipient_no) THEN RETURN -1

IF as_recipient_type_code = 'I' THEN
		
	SELECT 	bank_no, bank_transit_no, bank_account_no, given_names + ' ' + last_name
	INTO 		:as_bank_no, :as_bank_transit_no, :as_bank_account_no, :as_name
	FROM 		INDIVIDUAL
	WHERE 	individual_no = :al_recipient_no
	USING 		SQLCA;
	IF SQLCA.nf_handle_error('n_ephysio_invoices','nf_get_banking_info','SELECT bank_no, bank_transit_no, bank_account_no') = 100 THEN RETURN -1
	
ELSE
		
	SELECT bank_no, bank_transit_no, bank_account_no, name
	INTO    :as_bank_no, :as_bank_transit_no, :as_bank_account_no, :as_name
	FROM    BANK_INFO a 
	RIGHT OUTER JOIN PROVIDER b ON a.recipient_no 				= b.provider_no 
													 AND a.recipient_type_code 	= b.provider_type_code
	WHERE b.provider_no 				= :al_recipient_no
	AND     	b.provider_type_code 	= :as_recipient_type_code
	USING   SQLCA;
	IF SQLCA.nf_handle_error('n_ephysio_invoices','nf_get_banking_info','SELECT bank_no, bank_transit_no, bank_account_no') = 100 THEN RETURN -1
	
END IF
	


IF IsNull(as_bank_no) 				THEN as_bank_no 					= ''
IF IsNull(as_bank_transit_no) 		THEN as_bank_transit_no 		= ''
IF IsNull(as_bank_account_no) 	THEN as_bank_account_no 	= ''

RETURN 1

end function

public function integer nf_get_tax_rate (integer ai_option, string as_province_code, date adt_datecheck);INTEGER				li_rate_no

/* Tax Amount
	If tax is applicable, the system would derive the tax amount from a look up table based on the date of service and the type of tax selected 
	(e.g. 13% of  the [unit price * quantity] if the type of tax is NB HST)
	
	RETURNS rate_no (Primary key for the table )	
*/

//set the effective tax rate to 0
li_rate_no = 0

IF adt_datecheck < Date('1997-04-01') THEN RETURN 0 // review this check
	
CHOOSE CASE  ai_option
	CASE 1 // GRAB max tax rate (id) for a specific province
		
			SELECT	tax_rate_no
			INTO		:li_rate_no
			FROM	Tax_Rate 
			WHERE	effective_date = (	SELECT		max(effective_date)
													FROM		Tax_Rate 
													WHERE  	effective_date 		<= :adt_datecheck
													AND			active_flag 			= 'Y'  
                            						AND      	prov_state_code 	= :as_province_code )
			AND  prov_state_code = :as_province_code
			USING	SQLCA;
	
			SQLCA.nf_handle_error("n_ephysio_invoices", "nf_get_tax_rate()", "SELECT tax_rate (1)") 

	CASE 2
		
	CASE ELSE
END CHOOSE

IF ISNULL(li_rate_no) THEN li_rate_no = 0

RETURN li_rate_no

end function

public function decimal nf_get_tax_rate_percent (integer ai_tax_rate_no);DECIMAL 			ldec_tax_rate_return

/* Grabs the tax rate percent based on the tax_rate_no
	RETURNS Tax Rate Percent
*/

SELECT		tax_rate
INTO			:ldec_tax_rate_return
FROM		Tax_Rate 
WHERE		tax_rate_no = :ai_tax_rate_no
USING		SQLCA;
SQLCA.nf_handle_error("n_ephysio_invoices", "nf_get_tax_rate_percent()", "SELECT tax_rate") 

RETURN ldec_tax_rate_return

end function

public function integer nf_check_address (string as_address_line1, string as_address_line2, string as_city, string as_country_code);/* Basic address check 
	ck_UNAPPLIED_CLAIM_TXN_18
	([address_line1]<>' ' AND [city]<>' ' AND [country]<>' ')
*/

/*		Check that at least one address line has been entered */
IF (IsNull(as_address_line1) OR Trim(as_address_line1) = '') AND &
    (IsNull(as_address_line2) OR Trim(as_address_line2) = '')THEN
    MessageBox('Missing Address', 'At least one address line must be filled in.')
   RETURN -1
END IF

/*		Check that the city has been entered */
IF IsNull(as_city) OR Trim(as_city) = '' THEN
	MessageBox('Missing City', 'The city must be specified.')
	RETURN -1
END IF

/*		Check that the country has been entered */
IF IsNull(as_country_code) OR Trim(as_country_code) = '' THEN
	MessageBox('Missing Country', 'The country must be specified.')
	RETURN -1
END IF

RETURN 0

end function

public function string nf_get_physio_contract_flag (long al_provider_no, string as_provider_type);STRING			ls_physio_contract_flag

IF as_provider_type = 'I' THEN 
	
	ls_physio_contract_flag = 'N'

ELSE
	// grab the contract flag of the provider
	SELECT 	physio_contract_flag
	INTO 		:ls_physio_contract_flag
	FROM 		PROVIDER
	WHERE 	provider_no 				= :al_provider_no
	AND 			provider_type_code 	= :as_provider_type
	USING 		SQLCA;
	SQLCA.nf_handle_error("n_ephysio_invoice","nf_get_physio_contract_flag()","SELECT physio_contract_flag") 
	
	IF ISNULL(ls_physio_contract_flag) OR TRIM(ls_physio_contract_flag) = '' THEN ls_physio_contract_flag = ''
END IF 

RETURN ls_physio_contract_flag

end function

public function date nf_get_thursday_processing_date (string as_recipient_type_code);/*
	The  scheduled date of payments issued to a Medical Aid provider must be set to:
	•         the current date, if the current day of the week is a Thursday and Payment Processing has not already been run for the current date 
	•         a week from the current date (i.e. the following Thursday), if the current day of the week is a Thursday and Payment Processing has been already run for the current date 
	•         the upcoming Thursday, if the current day of the week is not a Thursday 
	The  scheduled date of payments issued to an Individual must be set to:
	•         the current date, if Payment Processing has not already been run for the current date 
	•         the next date, if Payment Processing has been already run for the current date 
*/

DATE			ldt_scheduled_processing_date, ldt_date
DATETIME 	ldtm_check_processed
LONG			ll_no, ll_num_times_payment_processing_run_today

ldtm_check_processed 	= f_server_datetime()
ldt_date 						= DATE(ldtm_check_processed)

ll_no = DayNumber(ldt_date)

SELECT COUNT(*) 
INTO :ll_num_times_payment_processing_run_today 
FROM PAYMENT_RUN 
WHERE create_date >= :ldtm_check_processed ; 
	 
SQLCA.nf_handle_error('n_ephysio_invoice', 'nf_get_thursday_processing_date()', 'SELECT COUNT(*) FROM PAYMENT_RUN WHERE create_date > :ldt_date')

CHOOSE CASE as_recipient_type_code
	CASE 'M', 'V','O'
		
				IF ll_no <= 4 THEN  // Sun, Mon, Tue, Wed 
						ldt_scheduled_processing_date =  RelativeDate(ldt_date, 5 - ll_no)	
						
				ELSEIF ll_no = 5 THEN  // Thursday
						
						IF ll_num_times_payment_processing_run_today > 0 THEN
								ldt_scheduled_processing_date  = RelativeDate(ldt_date, 7 - (ll_no - 5))  // Set it to following Thursday
						ELSE
								ldt_scheduled_processing_date = RelativeDate(ldt_date, 5 - ll_no)  // Set it to Today
						END IF
		 
				ELSE  // Fri, Sat 
							ldt_scheduled_processing_date =	RelativeDate(ldt_date, 7 - (ll_no - 5))
				END IF
			
	CASE 	ELSE//INDIVIDUAL
		
			IF ll_num_times_payment_processing_run_today > 0 THEN
					ldt_scheduled_processing_date  = RelativeDate(ldt_date, 1)  // Set it to tomorrow
			ELSE
					ldt_scheduled_processing_date = ldt_date  // Set it to Today
			END IF
					
END CHOOSE

RETURN ldt_scheduled_processing_date

end function

public function boolean nf_check_valid_info (long al_no, string as_type, integer ai_condition);
LONG			ll_count

CHOOSE CASE ai_condition 
	CASE 1 //PROVIDER
		
		SELECT count(*)
		INTO		:ll_count                                     			
		FROM 	PROVIDER
		WHERE provider_no 				= :al_no
		AND		provider_type_code 	= :as_type
		USING 	SQLCA;
		SQLCA.nf_handle_error("n_ephysio_invoices", "nf_check_valid_info()","SELECT count(*) -  A")	
		
		IF ISNULL(ll_count) OR ll_count < 1 THEN RETURN FALSE
			
	CASE 2 //RECIPIENT
		
		IF  as_type = 'I' THEN 
			
			SELECT 	COUNT(*)
			INTO 		:ll_count
			FROM 		INDIVIDUAL
			WHERE 	individual_no = :al_no
			USING 		SQLCA;
		   
			SQLCA.nf_handle_error("n_ephysio_invoices", "nf_check_valid_info()","SELECT count(*) -  B")	
			
		ELSE
						
			SELECT 	COUNT(*)
			INTO 		:ll_count
			FROM		PROVIDER  
			WHERE		provider_no 				= :al_no  
			AND      	provider_type_code 	= :as_type 
			USING 		SQLCA ;
	
			SQLCA.nf_handle_error("n_ephysio_invoices", "nf_check_valid_info()","SELECT count(*) -  C")
				
		END IF 
				
		IF ISNULL(ll_count) OR ll_count < 1 THEN RETURN FALSE
				
	CASE ELSE
		
			RETURN FALSE
			
	END CHOOSE
						
RETURN TRUE

end function

public function integer nf_check_items_vs_applied (long al_claim_no, integer ai_task_no, date adt_date, long al_xref_no, integer ai_item_no, integer ai_check);/* 
1.250	There must only be one billable item for the same date of service per course of treatment for a claim 
				(a voided invoice line item for a billable item is excluded) . QUESTION : can they submit an invoice for a supply for the same DATE. 
				E.g.   For SUPPLIES, can we ignore duplicates but check they are not exceeding the max limit)  
1.260	There must only be one Physiotherapy Assessment billed per course of treatment for a claim (a voided invoice line item for a billable item is excluded). 	
1.270	There must not be a Physiotherapy Assessment and a Physiotherapy Treatment billed for the same date of service per course of treatment for a claim 
								(a voided invoice line item for a billable item is excluded). 													
1.280	There must not be a Physiotherapy Assessment (in the home) and a Physiotherapy Treatment (in the home) billed for the same date of service per course of treatment for a claim
				               (a voided invoice line item for a billable item is excluded). 							
1.290	There must not be Missed Appointment (252) on the same date as any of the following services (for a course of treatment ):
				•	Physiotherapy Assessment  (173)
				•	Physiotherapy Treatment (172)
				•	TENS Assessment (201)
				•	Physiotherapy Assessment (in the home) (278)
				•	Physiotherapy Treatment (in the home) (279)
				
				NOTE: from JP
Check REHAB_INVOICE_LINE_ITEM where
Claim_no =  claim number of the document being paid
Task_no  =   task number of the REHAB_TASK for the line item being entered (i.,e the task tab they are on)
Billable_xref_no = billable xref number for the line items being entered
Service_date = service date of the billable item for the line item being entered
Line_item_amended_code = blank
								
*/
INTEGER	li_count, li_temp_billable_item_no
	
CHOOSE CASE ai_check
		
	CASE 1 // 1.250
		
			SELECT 	count(*) 
			INTO    	:li_count
			FROM   		REHAB_INVOICE_LINE_ITEM 		a
				JOIN  	Billable_Item_Rehab_Task_Xref   d ON a.billable_xref_no 	= d.billable_xref_no
				JOIN     Billable_Item 								e ON e.billable_item_no 	= d.billable_item_no
			WHERE     a.claim_no 											= :al_claim_no
			AND			a.task_no 												= :ai_task_no 
			AND       	a.billable_xref_no 									= :al_xref_no
			AND			convert(char(10),a.service_date,120) 	= :adt_date
			AND			line_item_amended_code 						= ''
			USING 		SQLCA;
			SQLCA.nf_handle_error("n_ephysio_invoice","nf_check_items_vs_applied()","count(*)  - A") 
			
			IF li_count > 0 THEN RETURN -1
			
	CASE 2 // 1.260
		
			SELECT 	count(*) 
			INTO    	:li_count
			FROM   		REHAB_INVOICE_LINE_ITEM 		a
				JOIN  	Billable_Item_Rehab_Task_Xref 	d ON a.billable_xref_no 	= d.billable_xref_no
				JOIN  	Billable_Item 								e ON e.billable_item_no 	= d.billable_item_no
			WHERE    	a.claim_no 									= :al_claim_no
			AND			a.task_no 										= :ai_task_no 
			AND      	d.billable_item_no 							= 172
			AND			line_item_amended_code 				= ''
			USING 		SQLCA;
			SQLCA.nf_handle_error("n_ephysio_invoice","nf_check_items_vs_applied()","count(*)  - B") 
			
			IF li_count > 0 THEN RETURN -1
		
	CASE 3 // 1.270
		
		IF ai_item_no = 173 THEN li_temp_billable_item_no = 172 ELSE  li_temp_billable_item_no = 173
		
		SELECT 	count(*) 
		INTO    	:li_count
		FROM   		REHAB_INVOICE_LINE_ITEM 		a
			JOIN  	Billable_Item_Rehab_Task_Xref 	d ON a.billable_xref_no 	= d.billable_xref_no
			JOIN  	Billable_Item 								e ON e.billable_item_no 	= d.billable_item_no
		WHERE    	a.claim_no 											= :al_claim_no
		AND			a.task_no 												= :ai_task_no 
		AND      	d.billable_item_no 									= :li_temp_billable_item_no
		AND			convert(char(10),a.service_date, 120) 	= :adt_date
		AND			line_item_amended_code 						= ''
		USING 		SQLCA;
		SQLCA.nf_handle_error("n_ephysio_invoice","nf_check_items_vs_applied()","count(*)  - C") 
			
		IF li_count > 0 THEN RETURN -1
		
	CASE 4 // 1.280
		
		IF ai_item_no = 278 THEN li_temp_billable_item_no = 279 ELSE  li_temp_billable_item_no = 278
		
		SELECT 	count(*) 
		INTO    	:li_count
		FROM   		REHAB_INVOICE_LINE_ITEM 		a
			JOIN  	Billable_Item_Rehab_Task_Xref 	d ON a.billable_xref_no 	= d.billable_xref_no
			JOIN  	Billable_Item 								e ON e.billable_item_no 	= d.billable_item_no
		WHERE    	a.claim_no 											= :al_claim_no
		AND			a.task_no 												= :ai_task_no 
		AND      	d.billable_item_no 									= :li_temp_billable_item_no
		AND			convert(char(10),a.service_date,120) 	= :adt_date
		AND			line_item_amended_code 						= ''
		USING 		SQLCA;
		SQLCA.nf_handle_error("n_ephysio_invoice","nf_check_items_vs_applied()","count(*)  - D") 
			
		IF li_count > 0 THEN RETURN -1
			
	CASE 5 // 1.290
		
		IF ai_item_no = 252  THEN
		
			SELECT 	count(*) 
			INTO    	:li_count
			FROM   		REHAB_INVOICE_LINE_ITEM 		a
				JOIN  	Billable_Item_Rehab_Task_Xref 	d ON a.billable_xref_no 	= d.billable_xref_no
				JOIN  	Billable_Item 								e ON e.billable_item_no 	= d.billable_item_no
			WHERE    	a.claim_no 									= :al_claim_no
			AND			a.task_no 										= :ai_task_no 
			AND      	d.billable_item_no 							IN (173, 172, 201, 278, 279)
			AND			convert(char(10),a.service_date,120) 	= :adt_date
			USING 		SQLCA;
			SQLCA.nf_handle_error("n_ephysio_invoice","nf_check_items_vs_applied()","count(*)  - E") 
				
			IF li_count > 0 THEN RETURN -1
			
		ELSE
			
			SELECT 	count(*) 
			INTO    	:li_count
			FROM   		REHAB_INVOICE_LINE_ITEM 		a
				JOIN  	Billable_Item_Rehab_Task_Xref 	d ON a.billable_xref_no 	= d.billable_xref_no
				JOIN  	Billable_Item 								e ON e.billable_item_no 	= d.billable_item_no
			WHERE    	a.claim_no 									= :al_claim_no
			AND			a.task_no 										= :ai_task_no 
			AND      	d.billable_item_no 							= 252
			AND			convert(char(10),a.service_date,120) 	= :adt_date
			USING 		SQLCA;
			SQLCA.nf_handle_error("n_ephysio_invoice","nf_check_items_vs_applied()","count(*)  - F") 
				
			IF li_count > 0 THEN RETURN -1
		
	END IF 
END CHOOSE
		
RETURN 1

end function

public function decimal nf_get_item_unit_price (long al_billable_xref_no, date adt_service_date);DECIMAL {2} ldec_unit_price

SELECT b.unit_fee  
INTO		:ldec_unit_price      
FROM  billable_item_rehab_task_xref 	c  
			join billable_item 					a ON c.billable_item_no = a.billable_item_no   
			left join billable_item_fee 		b ON c.billable_xref_no = b.billable_xref_no        
WHERE  c.billable_xref_no =  :al_billable_xref_no
AND 		( 	:adt_service_date >= b.effective_from_date  AND :adt_service_date <= b.effective_to_date
OR        	:adt_service_date >= b.effective_from_date  AND b.effective_to_date IS NULL )
AND 		c.fixed_fee_flag = 'Y'
USING SQLCA;
SQLCA.nf_handle_error("n_ephysio_invoice", "nf_get_item_unit_price()", "SELECT b.unit_fee")

IF isnull(ldec_unit_price) THEN ldec_unit_price = 0

RETURN ldec_unit_price
end function

public function string nf_get_ephysio_flag (long al_no, string as_type);STRING			ls_ephysio_flag

SELECT 	ephysio_flag
INTO 		:ls_ephysio_flag
FROM 		PROVIDER
WHERE 	provider_no 				= :al_no
AND 			provider_type_code 	= :as_type
USING 		SQLCA;
SQLCA.nf_handle_error('n_ephysio_invoice', 'nf_get_ephysio_flag()', 'SELECT ephysio_flag...') 

IF ISNULL(ls_ephysio_flag) THEN ls_ephysio_flag = ''

RETURN ls_ephysio_flag
end function

public function string nf_get_item_description (long al_xref_no);STRING			ls_billable_item_desc_e

SELECT billable_item_desc_e
INTO		:ls_billable_item_desc_e
FROM  	Billable_Item_Rehab_Task_Xref a
	join 	Billable_Item b ON a.billable_item_no = b.billable_item_no
WHERE a.billable_xref_no = :al_xref_no
USING 	SQLCA;
SQLCA.nf_handle_error('n_ephysio_invoice', 'nf_get_item_description()', 'SELECT billable_item_desc_e') 

IF isnull(ls_billable_item_desc_e) then ls_billable_item_desc_e = ''

RETURN ls_billable_item_desc_e
end function

public function string nf_get_provider_name (long al_no, string as_type);STRING			ls_provider_name

CHOOSE CASE as_type
	CASE 'I'
		
		SELECT  given_names + ' ' + last_name
		INTO		:ls_provider_name
		FROM 	INDIVIDUAL
		WHERE individual_no = :al_no
		USING 	SQLCA;
		SQLCA.nf_handle_error("n_ephysio_invoices", "nf_get_provider_name", "SELECT given_names + ' ' + last_name")
	
	CASE ELSE

		SELECT name
		INTO		:ls_provider_name                                     			
		FROM 	PROVIDER
		WHERE provider_no 				= :al_no
		AND		provider_type_code 	= :as_type
		USING 	SQLCA;
		SQLCA.nf_handle_error("n_ephysio_invoices", "nf_get_provider_name()","SELECT name")	
		
END CHOOSE

IF ISNULL(ls_provider_name) THEN ls_provider_name = ''

RETURN ls_provider_name

end function

public function string nf_get_recipient_subtype (long al_no, string as_type);STRING			ls_recipient_subtype

SELECT provider_sub_type_code
INTO		:ls_recipient_subtype                                     			
FROM 	PROVIDER
WHERE provider_no 				= :al_no
AND		provider_type_code 	= :as_type
USING 	SQLCA;
SQLCA.nf_handle_error("n_ephysio_invoices", "nf_get_recipient_subtype()","SELECT provider_sub_type_code")	

IF ISNULL(ls_recipient_subtype) THEN ls_recipient_subtype = ''

RETURN ls_recipient_subtype

end function

public function long nf_get_rehab_invoice_no_from_docid (long al_docid);LONG			ll_rehab_invoice_no

/*  	The payment_no is unique for PAYMENT_DOCUMENT  not the doc_id therefore we have 
		to just grab the first one and hope that the inforamtion taken from this one is
		reflected in all of them...ala all the same payment_method
*/
SELECT 	rehab_invoice_no
INTO 		:ll_rehab_invoice_no 
FROM 		REHAB_INVOICE 
WHERE 	doc_id		= :al_docid
USING 		sqlca;
SQLCA.nf_handle_error("n_ephysio_invoices", "nf_get_rehab_invoice_no_from_docid()", "SELECT rehab_invoice_no")

IF ISNULL(ll_rehab_invoice_no)  OR ll_rehab_invoice_no < 0 THEN ll_rehab_invoice_no = 0

RETURN ll_rehab_invoice_no

end function

public function boolean nf_provider_setup_on_website (long al_no, string al_type);/*
PAYMENT RECIPIENT determines if invoice can be paid through this module
If the PAYMENT RECIPIENT is a provider
	If the Provider is set up the ePhysio Billing
		Do NOT allow (the provider must submit thru the website)
	Otherwise
		Can use this module to submit an invoice line item
Otherwise
	Can use this module to submit an invoice line item
*/
INTEGER li_count

SELECT 	COUNT(*) 
INTO 		:li_count
FROM 		PROVIDER
WHERE 	provider_no 				= :al_no
AND 			provider_type_code 	= :al_type
AND     		ephysio_flag 				= 'Y'
USING 		SQLCA;
SQLCA.nf_handle_error("n_ephysio_invoice", "nf_provider_setup_on_website()", "SELECT COUNT(*)") 

IF li_count > 0 THEN RETURN TRUE

RETURN FALSE

end function

public function string nf_get_fixed_fee_flag (long al_xref_no);STRING			ls_fixed_fee_flag

SELECT 	fixed_fee_flag
INTO			:ls_fixed_fee_flag
FROM 		billable_item_rehab_task_xref
WHERE 	billable_xref_no = :al_xref_no
USING 		SQLCA;
SQLCA.nf_handle_error('n_ephysio_invoice', 'nf_get_fixed_fee_flag()', 'SELECT fixed_fee_flag') 

IF isnull(ls_fixed_fee_flag) then ls_fixed_fee_flag = ''

RETURN ls_fixed_fee_flag
end function

public function string nf_get_prov_state_code (long al_no, string as_type_code);STRING			ls_prov_state_code

SELECT 	prov_state_code
INTO			:ls_prov_state_code
FROM 		PROVIDER
WHERE 	provider_no 					= :al_no
AND 			provider_type_code 		= :as_type_code
USING 		SQLCA;
SQLCA.nf_handle_error("n_ephysio_invoice", "nf_get_prov_state_code()", "	SELECT prov_state_code")

IF ISNULL(ls_prov_state_code) THEN ls_prov_state_code = ''

RETURN ls_prov_state_code
end function

public function date nf_get_min_service_date_from_line_items (long al_claim_no, integer ai_task_no);DATE		ldt_min_service_date		

SELECT  min(service_date)
INTO		:ldt_min_service_date
FROM 	REHAB_INVOICE_LINE_ITEM
WHERE claim_no 	= :al_claim_no
AND      task_no  	= :ai_task_no
USING 	SQLCA;
SQLCA.nf_handle_error("n_ephysio_invoices", "nf_get_min_service_date_from_line_items", "SELECT  min(service_date)")

RETURN ldt_min_service_date
end function

public function boolean nf_set_allow_tax (long al_recipient_no, string as_recipient_type, long al_provider_no, string as_provider_type, long al_xref_no);STRING			ls_provider_tax_flag, ls_physio_contract_flag, ls_taxable_flag, ls_tax_flag

/* 
Case  PAYMENT RECIPIENT is a PROVIDER

	CASE  PAYMENT RECIPIENT PROVIDER.tax_flag is YES

		Case  SERVICE PROVIDER PROVIDER.physio_contract_flag is YES
			If Billable_Item.tax_flag is YES  for the billable item being billed
				Can Charge Tax
			Else (Billable_Item.tax_flag is NO )
				Cannot Charge Tax

		Case  SERVICE PROVIDER PROVIDER.physio_contract_flag is NO
			If Payment_Sub_Type.tax_flag is YES for the payment type associated with the billable item being billed
				Can Charge Tax
			Else
				Cannot Charge Tax

	CASE  PAYMENT RECIPIENT PROVIDER.tax_flag is NO
		Cannot Charge Tax

Case  PAYMENT RECIPIENT is an INDIVIDUAL
	Cannot Charge Tax

*/

//Case  PAYMENT RECIPIENT is an INDIVIDUAL 	Cannot Charge Tax
IF as_recipient_type = 'I' THEN RETURN FALSE

//Case  PAYMENT RECIPIENT is a PROVIDER
SELECT 	tax_flag 
INTO			:ls_provider_tax_flag
FROM 		PROVIDER
WHERE 	provider_no 					= :al_recipient_no
AND 			provider_type_code 		= :as_recipient_type
USING 		SQLCA;
SQLCA.nf_handle_error("n_ephysio_invoice", "nf_set_allow_tax()", "SELECT tax_flag")

IF ls_provider_tax_flag = 'N' THEN RETURN FALSE 

//grab some basic info
SELECT 	a.taxable_flag, c.tax_flag
INTO			:ls_taxable_flag, :ls_tax_flag
FROM 		Billable_Item 							a 
	join Billable_Item_Rehab_Task_Xref 	b ON a.billable_item_no = b.billable_item_no
	join Payment_Sub_Type 					c ON b.payment_type_code = c.payment_type_code AND b.payment_sub_type_code = c.payment_sub_type_code
WHERE 	billable_xref_no = :al_xref_no
USING  		SQLCA;
SQLCA.nf_handle_error("n_ephysio_invoice", "nf_set_allow_tax()", "SELECT a.taxable_flag, c.tax_flag, b.billable_xref_no")

ls_physio_contract_flag = nf_get_physio_contract_flag(al_provider_no, as_provider_type )
CHOOSE CASE ls_physio_contract_flag
	CASE 'Y'
		/* 
		Case  SERVICE PROVIDER PROVIDER.physio_contract_flag is YES
		If Billable_Item.tax_flag is YES  for the billable item being billed
			Can Charge Tax
		Else (Billable_Item.tax_flag is NO )
			Cannot Charge Tax
		*/
		IF ls_taxable_flag = 'Y' THEN RETURN TRUE

	CASE 'N'
		
		/*
		Case  SERVICE PROVIDER PROVIDER.physio_contract_flag is NO
		If Payment_Sub_Type.tax_flag is YES for the payment type associated with the billable item being billed
			Can Charge Tax
		Else
			Cannot Charge Tax
		*/
		
		IF ls_tax_flag = 'Y' THEN RETURN TRUE
				
END CHOOSE

RETURN FALSE
				
end function

public function boolean nf_recipient_eligible_for_automated_pay (long al_no, string as_type);BOOLEAN		lb_check
INTEGER		li_count

/*
19.180 Payments must not be made to recipients who are set up to receive automated payments, such as WRC and ABCC. 
(A list of recipients who receive automated payments can be found in the App_Document_Index_Parameter table)  
*/

SELECT count(*)
INTO		:li_count                                     			
FROM 	App_Document_Index_Parameter
WHERE service_provider_no 				= :al_no
AND		service_provider_type_code 	= :as_type
USING 	SQLCA;
SQLCA.nf_handle_error("n_ephysio_invoices", "nf_recipient_eligible_for_automated_pay()","SELECT provider_sub_type_code")	

IF ISNULL(li_count) THEN li_count = 0

IF li_count > 0 THEN 
	lb_check = FALSE
ELSE
	lb_check = TRUE
END IF 


RETURN lb_check

end function

public function boolean nf_total_payment_amount_enterable (long al_invoice_submitted_by, string as_invoice_submitted_by_type, long al_payable_to, string as_payable_to_type);/*
NOTE: T035414 - Account Payment - Physio Reimbursement Screen
"It does not seem possible to reimburse an injured worker through the physio reimbursement screen.
If the injured worker paid more than the set fee and the provider is under the WorkSafeNB contract 
- the fee defaults to the set fee.  The total amount is set, the user can change the submitted amount but not the paid amount.  
Since the paid amount cannot be adjusted the injured worker cannot be properly reimbursed"

As a result of the above CASE becomes modified to If the recipient is an Individual  regardless of the physio contract you should be 
able to fill in the total payable amount

Modified: If the ‘Invoice Submitted By’ = anything other than a Medical aid Provider under physio contract 
And the ‘Payable to’ is anything other than a Medical aid Provider under physio contract
The ‘Payable Amount’ should be enterable.

The ‘Service Provided By’ does not factor into the user being able to enter an amount in the ‘Payable Amount’ column

*/

STRING		ls_physio_contract_flag_submitted, ls_physio_contract_flag_payable

IF as_invoice_submitted_by_type = 'M' OR as_payable_to_type = 'M' THEN
	
	ls_physio_contract_flag_submitted = nf_get_physio_contract_flag(al_invoice_submitted_by, as_invoice_submitted_by_type)
	ls_physio_contract_flag_payable    = nf_get_physio_contract_flag(al_payable_to, as_payable_to_type)
	
	IF ls_physio_contract_flag_submitted = 'Y' OR ls_physio_contract_flag_payable = 'Y' THEN
		RETURN FALSE
	ELSE 
		RETURN TRUE
	END IF 
	
ELSE
	
	RETURN TRUE
	
END IF 

RETURN FALSE



end function

on n_ephysio_invoice.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_ephysio_invoice.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

