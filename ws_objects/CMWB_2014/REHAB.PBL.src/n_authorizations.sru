$PBExportHeader$n_authorizations.sru
$PBExportComments$business rules for the authorization tab
forward
global type n_authorizations from n_pdc
end type
end forward

global type n_authorizations from n_pdc
end type
global n_authorizations n_authorizations

type variables
LONG  			il_claim_no, il_task_no
BOOLEAN  	ib_new_authorization
DATE 			idt_ephysio_implentation_date

end variables

forward prototypes
public function integer nf_set_defaults ()
public function integer nf_check_bus_rule ()
public function integer nf_check_mandatory ()
public function integer nf_set_unused_fields ()
public function long nf_get_next_identifier ()
public function long nf_set_identifiers ()
public function integer nf_change_item (long al_datawindow, long al_row, string as_dw_column, string as_data)
public function integer nf_provider_address (string as_recipient_type_code, long al_recipient_no)
public function boolean nf_check_accident_date (datetime adt_date)
public function long nf_get_next_authorization_no ()
public function integer nf_retrieve (long al_claim_no, long al_task_no, long al_authorization_no)
public function integer nf_set_claim_no (long al_claim_no, long al_task_no)
public function integer nf_insert (long al_row, string as_task_type_code)
public function long nf_check_for_authorization (integer ai_level, string as_task_type, string as_task_sub_type, string as_task_specific)
public subroutine nf_set_new_authorization (boolean ab_new_authorization)
public function integer nf_update ()
public function integer nf_set_billable_item_xref_filter (string as_service_code, string as_program_code, string as_task_type_code, string as_sub_type_code, string as_task_specific_code, boolean ab_new_authorization)
public subroutine nf_set_column_action (boolean ab_visible)
public function decimal nf_get_unit_price (long al_billable_xref_no, date adt_datecheck)
public function integer nf_authorized_amount_entry_scenario (long al_xref_no, long al_no, string as_type)
public function string nf_get_fixed_fee_flag (long al_xref_no)
public function string nf_get_physio_contract_flag (long al_no, string as_type)
public function string nf_get_authorized_amount_required_flag (long al_billable_xref_no)
public function decimal nf_get_max_authorized_amount (long al_billable_xref_no)
public subroutine nf_set_authorized_amount_scenario (integer ai_scenario_no, long al_claim_no, decimal adec_max_authorized_amount, integer ai_status)
public function string nf_get_other_billable_item_flag (long al_billable_item_no)
public function long nf_get_billable_item_no (long al_billable_xref_no)
end prototypes

public function integer nf_set_defaults ();LONG  ll_row
/*
Authorization number	system generated
Task number			REHAB_TASK.task_no for the task
Claim number		REHAB_TASK.claim_no for the task
Billable xref number		Billable_Item_Rehab_Task.billable_xref_no
Authorized quantity		Billable_Item_Rehab_Task.default_authorized_qty
Authorized amount		If the Billable_Item_Rehab_Task.fixed_fee_flag is Yes
				   	Billable_Item.fee for the billable item and the current date
				Otherwise,
					Billable_Item_Rehab_Task.max_authorized_amount
Paid Quantity		0 (Zero)
Paid Amount			0 (Zero)
Authorized Provider Type	REHAB_TASK.provider_type_code
Authorized Provider No	REHAB_TASK.provider_no
Authorized Date		Current date
Authorized By User Id		user_profile.userid
Authorization Comment	Blank
Expedited Billing Flag		No
Fixed Fee Flag		Billable_Item_Rehab_Task.fixed_fee_flag  
Auto Created Flag		Yes
Web Create User Id		Blank
Web Modify User Id		Blank

*/

 //set the default values for the individual insert
ll_row = idw_dw[1].GetRow()
IF ll_row > 0 THEN
	idw_dw[1].SetItem(1,'authorized_quantity',1)
	idw_dw[1].SetItem(1,'paid_quantity',0)
	idw_dw[1].SetItem(1,'authorization_comment','')
	idw_dw[1].SetItem(1,'authorized_date',date(f_server_datetime()))
	idw_dw[1].SetItem(1,'auto_created_flag','N')	
END IF

RETURN 0
end function

public function integer nf_check_bus_rule ();STRING		ls_string, ls_string2, ls_string3, ls_rehab_program_code, ls_rehab_service_code, ls_provider_sub_type_code
STRING		ls_task_type_code, ls_task_sub_type_code, ls_task_specific_code, ls_authorized_provider_type_code, ls_task_provider_type
STRING		ls_expedited_billing_flag, ls_fixed_fee_flag, ls_authorized_amount_required_flag, ls_changed_amount_comment, ls_revision_comment
STRING		ls_physio_contract_flag, ls_provider_type_code_original, ls_billable_item_is_a_service, ls_rta_other_billable_item_flag, ls_other_billable_item_desc
LONG		ll_count, ll_no, ll_authorization_no, ll_rehab_task_row, ll_task_no, ll_claim_no, ll_provider_no_original
LONG		ll_return, ll_billable_xref_no, ll_authorized_provider_no, ll_billable_item_no, ll_task_provider_no
DECIMAL	ldec_amount, ll_result, ldec_authorized_amount, ldec_authorized_quantity, ldec_paid_amount, ldec_test_amount
DECIMAL	ldec_default_authorized_qty, ldec_max_authorized_qty, ldec_max_authorized_amount, ldec_paid_quantity, ldec_test_quantity
BOOLEAN	lb_check = TRUE
INTEGER	li_message, li_authorization_count
DATE		ldt_current_date

dwItemStatus	l_provider_no_status, l_provider_type_code_status, l_billable_item_type_status, l_status_row_status, l_authorized_amount_status, l_authorized_quantity_status
dwItemStatus	l_other_billable_item_desc


ll_rehab_task_row =  idw_dw[2].getrow()
IF ISNULL(ll_rehab_task_row) OR ll_rehab_task_row < 1 THEN RETURN -1

// REHAB_TASK INFORMATION
ls_rehab_program_code 				= idw_dw[2].GetItemString(ll_rehab_task_row,'rehab_program_code')
ls_rehab_service_code 					= idw_dw[2].GetItemString(ll_rehab_task_row,'rehab_service_code')
ls_task_type_code  						= idw_dw[2].GetItemString(ll_rehab_task_row, 'task_type_code')
ls_task_sub_type_code     				= idw_dw[2].GetItemString(ll_rehab_task_row, 'task_sub_type_code')
ls_task_specific_code 					= idw_dw[2].GetItemString(ll_rehab_task_row,'task_specific_code')
ll_task_no										= idw_dw[2].GetItemnumber(ll_rehab_task_row,'task_no')
ll_claim_no										= idw_dw[2].GetItemnumber(ll_rehab_task_row,'claim_no')

//REHAB_TASK_AUTHORIZATION INFORAMTION
ll_billable_xref_no 							= idw_dw[1].GetItemnumber(1,'billable_xref_no')
ll_authorized_provider_no	  			= idw_dw[1].GetItemnumber(1,'authorized_provider_no')
ls_authorized_provider_type_code 	= idw_dw[1].GetItemstring(1,'authorized_provider_type_code')
ls_expedited_billing_flag					= idw_dw[1].GetItemstring(1,'expedited_billing_flag')
ldec_authorized_amount 				= idw_dw[1].GetItemdecimal(1,'authorized_amount')
ls_fixed_fee_flag							= idw_dw[1].GetItemstring(1,'fixed_fee_flag')
ldec_authorized_quantity				= idw_dw[1].GetItemdecimal(1,'authorized_quantity')
ldec_paid_amount							= idw_dw[1].GetItemdecimal(1,'paid_amount')
ldec_paid_quantity                       = idw_dw[1].GetItemNumber(1,'paid_quantity')
ls_revision_comment						= idw_dw[1].GetItemstring(1,'revision_comment')
ll_authorization_no							= idw_dw[1].GetItemnumber(1,'authorization_no')
ls_provider_type_code_original       = idw_dw[1].GetItemstring(1,'authorized_provider_type_code',Primary!,TRUE)
ll_provider_no_original                   = idw_dw[1].GetItemnumber(1,'authorized_provider_no',Primary!,TRUE)
ls_other_billable_item_desc				= idw_dw[1].GetItemstring(1,'other_billable_item_desc')            


ldec_test_quantity 						= idw_dw[1].GetItemDecimal(1,'authorized_quantity',Primary!,TRUE)
ldec_test_amount							= idw_dw[1].GetItemDecimal(1,'authorized_amount',Primary!,TRUE)

// grab the itemstatus for certain columns these are columns that may not have been modified  and are inactive
l_provider_no_status						= idw_dw[1].GetItemStatus(1,'authorized_provider_no', Primary!)
l_provider_type_code_status       	= idw_dw[1].GetItemStatus(1,'authorized_provider_type_code', Primary!)
l_billable_item_type_status            = idw_dw[1].GetItemStatus(1,'billable_xref_no', Primary!)
l_status_row_status   					= idw_dw[1].GetItemStatus(1,0, Primary!)
l_authorized_amount_status			= idw_dw[1].GetItemStatus(1,'authorized_amount', Primary!)
l_authorized_quantity_status     		= idw_dw[1].GetItemStatus(1,'authorized_quantity', Primary!)
l_other_billable_item_desc				= idw_dw[1].GetItemStatus(1,'other_billable_item_desc', Primary!)

/* NOTE: */
/* 4.200	The paid amount must not be entered.			-- PROTECTED IN DATAWINDOW */
/* 4.210	The paid quantity must not be entered. 		-- PROTECTED IN DATAWINDOW */
/* 4.220	The authorized by user must not be entered.	-- PROTECTED IN DATAWINDOW */
/* 4.230	The authorized date must not be entered.		-- PROTECTED IN DATAWINDOW */
/* 4.320	The authorized user id must not be entered.	-- PROTECTED IN DATAWINDOW  -- taken out of recent copy of BR*/
/* 4.330	The authorized date must not be entered. 	-- PROTECTED IN DATAWINDOW  -- taken out of recent copy of BR*/
/* 4.420	Billable item for an Initial Assessment Report, a Progress Report or a Discharge Report must not be displayed in the list of billable item to select from 
               (i.e. the Billable_Item_Rehab_Task_Xref.billable_item_no = 253, 254 or 255) coded as where in dddw_billable_item_rehab_task_xref 
				NOTE: 4.420 has been removed as of 2012.06.01 
*/

/* SET SOME DEFAULTS */
IF ISNULL(ldec_test_quantity) THEN ldec_test_quantity = 0
ldt_current_date							= date(f_server_datetime())

/* 4.120	The billable item must be entered.	*/
IF ISNULL(ll_billable_xref_no) OR ll_billable_xref_no < 1  THEN 
	MessageBox('Error', 'The billable item must be entered.')
	idw_dw[1].SetFocus()
	idw_dw[1].SetColumn('billable_xref_no')
	RETURN -1
END IF 
	
SELECT 	billable_item_no 
INTO 		:ll_billable_item_no 
FROM 		Billable_Item_Rehab_Task_Xref
WHERE 	billable_xref_no = :ll_billable_xref_no
USING 		SQLCA;
SQLCA.nf_handle_error("n_authorizations","nf_check_bus_rule()","SELECT billable_item_no") 
	
IF ISNULL(ll_billable_item_no) OR ll_billable_item_no < 1 THEN 
	MessageBox('Error', 'Could not determine the Billable Item Number.')
	idw_dw[1].SetFocus()
	idw_dw[1].SetColumn('billable_xref_no')
	RETURN -1
END IF 

/* start the checks */
IF l_billable_item_type_status <> NOTMODIFIED! THEN
	
	SELECT count(*)
	INTO 	:ll_count
	FROM 	Billable_Item_Rehab_Task_Xref
	WHERE billable_xref_no 				= :ll_billable_xref_no
	AND     task_type_code        		= :ls_task_type_code
	AND     task_sub_type_code 		= :ls_task_sub_type_code
	AND     task_specific_code     	= :ls_task_specific_code
	AND 		active_flag 					= 'Y'
	USING 	SQLCA;
	SQLCA.nf_handle_error("n_authorizations","nf_check_bus_rule()","SELECT count(*) FROM 	Rehab_Service_Program_Xref.") 
	
	/* 4.130	The billable item must be an active, billable item for the type of task under which the item is being authorized
						  (i.e. for the rehab service/program/task type/task subtype/task specific type combination). */
	IF ll_count < 1 OR ISNULL(ll_count)  THEN
		MessageBox('Invalid Billable item', "The billable item must be an active, billable item for the type of task under which the item is being authorized" , Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('billable_xref_no')
		RETURN -1
	END IF 

	/* 4.435	There must only be one authorization for a billable item per task for a task that is for a Physiotherapy service
	(if the REHAB_TASK.rehab-service_code = ‘S022’, then there must only be one REHAB_TASK_AUTHORIZATION for a specific billable item under that particular task). 
	
	REVISION: 20120419
	There must only be one authorization for a billable item  per task for billable item that is a SERVICE for a task that is for a Physiotherapy service
                (if the REHAB_TASK.rehab-service_code = ‘S022’, then there must only be one REHAB_TASK_AUTHORIZATION for a specific billable item 
					 under that particular task, provided the Billable_Item.is_a_service_flag = ‘Y’). 
	*/
	SELECT 	is_a_service_flag
	INTO 		:ls_billable_item_is_a_service
	FROM 		Billable_Item
	WHERE 	billable_item_no = :ll_billable_item_no
	USING 		SQLCA;
	SQLCA.nf_handle_error("n_authorizations","nf_check_bus_rule()","SELECT is_a_service_flag") 
	
	IF ls_rehab_service_code = 'S022'  AND ls_billable_item_is_a_service = 'Y' THEN 
		
		IF ll_authorization_no > 0  THEN 
		
			// can't have 2 of the same authorizations 
			SELECT count(*) 
			INTO 	:li_authorization_count
			FROM 	REHAB_TASK_AUTHORIZATION  a
			  JOIN 	REHAB_TASK  b ON a.claim_no = b.claim_no AND a.task_no = b.task_no
			WHERE a.task_no 					= :ll_task_no
			AND 		a.claim_no 				= :ll_claim_no
			AND     a.billable_xref_no 		= :ll_billable_xref_no
			AND		a.authorization_no 	<> :ll_authorization_no
			AND     b.rehab_service_code = 'S022'
			USING 	sqlca;
			SQLCA.nf_handle_error("n_authorizations","nf_check_bus_rule()","SELECT count(*) (1)") 
		
		ELSE
			
			// can't have 2 of the same authorizations 
			SELECT count(*) 
			INTO 	:li_authorization_count
			FROM 	REHAB_TASK_AUTHORIZATION  a
			  JOIN 	REHAB_TASK  b ON a.claim_no = b.claim_no AND a.task_no = b.task_no 
			WHERE a.task_no 						= :ll_task_no
			AND 		a.claim_no 					= :ll_claim_no
			AND     a.billable_xref_no 			= :ll_billable_xref_no
			AND     b.rehab_service_code 	= 'S022'
			USING 	sqlca;
			SQLCA.nf_handle_error("n_authorizations","nf_check_bus_rule()","SELECT count(*) (2)") 
			
		END IF 
		
		IF li_authorization_count > 0 THEN
			MessageBox('Invalid Billable item', "The selected billable item is already authorized" , Exclamation!)
			idw_dw[1].SetFocus()
			idw_dw[1].SetColumn('billable_xref_no')
			RETURN -1
		END IF 
	END IF 
END IF 

/* NEW STUFF 
1.	New rule - The other billable item description must be entered if the billable item requires the other billable item description.
2.	New rule - The other billable item description must not be entered if the billable item does not require the other billable item description.

[DB - Constraint ]
(datalength([other_billable_item_desc])=(0) OR [other_billable_item_desc] like '%[A-Z]%[A-Z]%[A-Z]%' AND [dbo].[udf_Dirty_String_1]([other_billable_item_desc])=(0))
*/
ls_rta_other_billable_item_flag = nf_get_other_billable_item_flag(ll_billable_item_no)
IF ls_rta_other_billable_item_flag = 'Y' AND (isnull(ls_other_billable_item_desc) OR trim(ls_other_billable_item_desc) = '')  THEN
	MessageBox('Invalid other billable item description', "The other billable item description must be entered if the billable item requires the other billable item description." , Exclamation!)
	RETURN -1	
END IF 

IF ls_rta_other_billable_item_flag = 'N' AND trim(ls_other_billable_item_desc) <> ''  THEN
	MessageBox('Invalid other billable item description', "The other billable item description must not be entered if the billable item does not require the other billable item description." , Exclamation!)
	RETURN -1	
END IF 

//[constraint]
IF len(trim(ls_other_billable_item_desc)) < 3 AND  ls_rta_other_billable_item_flag = 'Y' THEN 
	MessageBox('Invalid other billable item description', "The other billable item description must be greater then 2 characters." , Exclamation!)
	RETURN -1	
END IF 

/* 4.135	The authorized provider type, if entered, must be an active provider type (from Provider_Type table)	 */
IF trim(ls_authorized_provider_type_code) > '' AND NOT ISNULL (ls_authorized_provider_type_code) AND l_provider_type_code_status <> NOTMODIFIED! THEN
	
	SELECT count(*)
	INTO 	:ll_count
	FROM 	Provider_Type
	WHERE provider_type_code 	= :ls_authorized_provider_type_code
	AND 		active_flag 				= 'Y'
	USING 	SQLCA;			
	SQLCA.nf_handle_error("n_authorizations","nf_check_bus_rule()","SELECT count(*)") 
	
	IF ll_count < 1 OR ISNULL(ll_count)  THEN
		MessageBox('Invalid Authorized Provider Type', "The authorized provider type, if entered, must be an active provider type (from Provider_Type table)" , Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('authorized_provider_type_code')
		RETURN -1
	END IF 
	
	/* 4.160	The authorized provider number must be entered if the authorized provider type is entered. */
	IF ll_authorized_provider_no < 1 OR ISNULL(ll_authorized_provider_no) THEN
		MessageBox('Invalid Authorized Provider', "The authorized provider number must be entered if the authorized provider type is entered." , Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('authorized_provider_no')
		RETURN -1
	END IF 
END IF

/* 4.140	The authorized provider number, if entered, must be an active service provider (from PROVIDER table)	 */
/* 4.150	The authorized provider type must be entered if the authorized provider number is entered. */
IF ll_authorized_provider_no > 0 AND NOT ISNULL(ll_authorized_provider_no) AND l_provider_no_status <> NOTMODIFIED! THEN
	
	SELECT 	count(*)
	INTO	 		:ll_count
	FROM 		PROVIDER
	WHERE 	provider_no 				= :ll_authorized_provider_no
	AND 			provider_type_code 	= :ls_authorized_provider_type_code
	AND 			active_flag 				= 'Y'
	USING 		SQLCA;
	SQLCA.nf_handle_error("n_authorizations","nf_check_bus_rule()","SELECT count(*) FROM PROVIDER.") 
	
	IF ll_count < 1 OR ISNULL(ll_count) OR ISNULL(ls_authorized_provider_type_code) OR  trim(ls_authorized_provider_type_code) = '' THEN
		MessageBox('Invalid Authorized Provider',"The authorized provider number, if entered, must be an active service provider (from PROVIDER table)" , Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('authorized_provider_no')
		RETURN -1
	END IF 
	
		IF ldt_current_date >=  idt_ephysio_implentation_date THEN

			SELECT 	physio_contract_flag
			INTO 		:ls_physio_contract_flag
			FROM 		PROVIDER
			WHERE 	provider_no 				= :ll_authorized_provider_no
			AND 			provider_type_code 	= :ls_authorized_provider_type_code
			USING 		SQLCA;
			SQLCA.nf_handle_error("n_authorizations","nf_check_bus_rule()","SELECT count(*) FROM PROVIDER.") 
			
			 CHOOSE CASE ls_physio_contract_flag
				CASE 'Y', 'I', 'N'
				CASE ELSE
					MessageBox('Invalid Service Approval  Flag', "Please Contact the Helpdesk for information on this provider" , Exclamation!)
					idw_dw[1].SetFocus()
					RETURN -1
			  END CHOOSE
		ELSE
			
			ls_physio_contract_flag 		= "" //NO VALUE
			
		END IF 
ELSE
	
	ls_physio_contract_flag 		= "" //NO VALUE

END IF 

/* grab the default maxes for this item */
SELECT default_authorized_qty,  max_authorized_qty, max_authorized_amount , authorized_amount_required_flag
INTO 	:ldec_default_authorized_qty,   :ldec_max_authorized_qty,  :ldec_max_authorized_amount , :ls_authorized_amount_required_flag
FROM 	Billable_Item_Rehab_Task_Xref a
  INNER JOIN Billable_Item b   ON a.billable_item_no = b.billable_item_no
WHERE a.billable_xref_no 			= :ll_billable_xref_no
AND     a.task_type_code      	= :ls_task_type_code
AND     a.task_sub_type_code 	= :ls_task_sub_type_code
AND     a.task_specific_code     	= :ls_task_specific_code
//AND 		a.active_flag 					= 'Y' removed per request
USING 	SQLCA;
SQLCA.nf_handle_error("n_authorizations","nf_check_bus_rule()","SELECT default_authorized_qty,...") 

/* 4.250	The authorized amount must not be entered if the billable item has a fixed fee rate (i.e. Billable_Item_Rehab_Task_Xref.fixed_fee_flag = ‘Y’). 
	REVISION:
    4.250	The authorized amount must not be entered if the billable item has a fixed fee rate and the authorized provider is entered and the authorized provider 
                is under the WorkSafeNB physio contract (i.e. Billable_Item_Rehab_Task_Xref.fixed_fee_flag = ‘Y’ and PROVIDER.ephysio_flag = ‘Y’). */
IF  l_authorized_amount_status <> notmodified! AND ldec_authorized_amount <> ldec_max_authorized_amount THEN
	IF ldec_authorized_amount > 0 AND ls_fixed_fee_flag = 'Y' AND ls_physio_contract_flag = 'Y' THEN
		MessageBox('Invalid Authorized Amount', "The authorized amount must not be entered if the billable item has a fixed fee rate." +&
		                                  						 "~rand the authorized provider is entered and the authorized provider is under the WorkSafeNB physio contract", Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('authorized_amount')
		RETURN -1
	END IF 
	
	/* 4.255	The authorized amount must not be entered if the billable item has a fixed fee rate and the authorized provider is not entered.  */
	IF ldec_authorized_amount > 0 AND ll_authorized_provider_no = 0 AND ls_fixed_fee_flag = 'Y' THEN
		MessageBox('Invalid Authorized Amount',"The authorized amount must not be entered if the billable item has a fixed fee rate and the authorized provider is not entered.", Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('authorized_amount')
		RETURN -1	
	END IF 
END IF 

/* 4.260	The authorized amount must be entered if the billable item has a variable fee rate and the billable item requires an authorized amount 
              	(i.e. Billable_Item_Rehab_Task_Xref.fixed_fee_flag = ‘N’ and Billable_Item_Rehab_Task_Xref.authorized_amount_required_flag = ‘Y’)
 */			  
IF ls_authorized_amount_required_flag = 'Y' AND ls_fixed_fee_flag = 'N' THEN
	IF ldec_authorized_amount = 0 OR ISNULL(ldec_authorized_amount) THEN 
		MessageBox('Invalid Authorized Amount',"The authorized amount must be entered if the billable item has a variable fee rate and the billable item requires an authorized amount" , Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('authorized_amount')
		RETURN -1	
	END IF
END IF 

/* 4.265	The authorized amount must be entered if the authorized provider is entered and the authorized provider is not under the WorkSafeNB physio contract  
            and the billable item requires an authorized amount (i.e. PROVIDER.ephysio_flag = ‘N’ and Billable_Item_Rehab_Task_Xref.authorized_amount_required_flag = ‘Y’).
*/
IF ls_physio_contract_flag <> 'Y'  AND  ls_authorized_amount_required_flag = 'Y' THEN 
	IF  ldec_authorized_amount = 0 OR ISNULL(ldec_authorized_amount) THEN  
		MessageBox('Invalid Authorized Amount', "The authorized amount must be entered if the authorized provider is entered and the authorized provider is not under the WorkSafeNB physio contract" +&
		                                        					 "~rand the billable item requires an authorized amount", Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('authorized_amount')
		RETURN -1
	END IF 
END IF 

/* 4.275	The authorized amount may be entered if the billable item does not require an authorized amount and the authorized provider is entered and the 
            authorized provider is not under the WorkSafeNB physio contract (i.e. PROVIDER.ephysio_flag = ‘N’ and Billable_Item_Rehab_Task_Xref.authorized_amount_required_flag = ‘N’).			
			-- check dw
*/
		  
/* 4.290	The authorized amount must be set to the maximum authorized amount allowed for a billable item if the user does not enter an amount 
               (i.e. Billable_Item_Rehab_Task_Xref.max_authorized_amount).
REVISION
   4.290	   The authorized amount must be set to the maximum authorized amount allowed for a billable item (i.e. Billable_Item_Rehab_Task_Xref.max_authorized_amount) 
               if the user chooses not to enter an amount.	
				Set the  changed_amounts_comment when the user doesn’t enter it to ‘Set to the maximum authorized amount as no amount was entered’
*/
IF l_status_row_status <> notmodified! THEN
	IF ISNULL(ldec_authorized_amount) OR ldec_authorized_amount <=0 THEN 
		ldec_authorized_amount = ldec_max_authorized_amount
		idw_dw[1].setitem(1,'authorized_amount', ldec_authorized_amount)
		
		IF trim(ls_revision_comment) = '' OR ISNULL(ls_revision_comment) and  l_status_row_status <> newmodified!  THEN	//they haven't entered anything
				idw_dw[1].setitem(1,'changed_amounts_comment', 'Set to the maximum authorized amount as no amount was entered')
		END IF 
		
	END IF 
END IF 

/* 4.295	The authorized amount must be set to the maximum authorized amount allowed for a billable item (i.e. Billable_Item_Rehab_Task_Xref.max_authorized_amount) 
            if the billable item has a fixed fee rate and the authorized provider is entered and the authorized provider is under the WorkSafeNB physio contract  
		    (i.e. Billable_Item_Rehab_Task_Xref.fixed_fee_flag = ‘Y’ and PROVIDER.ephysio_flag = ‘Y’).
*/
IF  ls_fixed_fee_flag = 'Y' AND ls_physio_contract_flag = 'Y' THEN 
	IF ldec_authorized_amount <> ldec_max_authorized_amount THEN 
		MessageBox('Invalid Authorized Amount',"The authorized amount must be set to the maximum authorized amount allowed for a billable item" +&
		                                        					 "~rif the billable item has a fixed fee rate and the authorized provider is entered and the authorized provider is under the WorkSafeNB physio contract", Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('authorized_amount')
		RETURN -1
	END IF
END IF 

/* 4.297	The authorized amount must be set to the maximum authorized amount allowed for a billable item (i.e. Billable_Item_Rehab_Task_Xref.max_authorized_amount)
            if the billable item has a fixed fee rate and the authorized provider is not entered. 
*/
IF  ls_fixed_fee_flag = 'Y' AND ll_authorized_provider_no = 0 THEN 
	IF ldec_authorized_amount <> ldec_max_authorized_amount THEN 
		MessageBox('Invalid Authorized Amount',"The authorized amount must be set to the maximum authorized amount allowed for a billable item" +&
		                                         					"~rif the billable item has a fixed fee rate and the authorized provider is not entered. ", Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('authorized_amount')
		RETURN -1
	END IF
END IF 

/* 4.240	The expedited billing flag must be either Yes or No.	 */
IF ls_expedited_billing_flag <> 'Y' AND ls_expedited_billing_flag <> 'N'  THEN
	MessageBox('Invalid Expedited Billing Flag', "The expedited Billing flag must be Yes or No." , Exclamation!)
	idw_dw[1].SetFocus()
	idw_dw[1].SetColumn('expedited_service_flag')
	RETURN -1
END IF 	

/* 4.340	The following fields must not be modified if the authorized billable item has been partially or fully paid (the paid amount is greater than zero):
•	Billable item type
•	Authorized provider type
•	Authorized provider number
REVISED:
4.340	The following fields must not be modified if the authorized billable item has been partially or fully paid (the paid amount is greater than zero):
•	Billable item type
•	Other billable item description (if entered)
•	Authorized provider type
•	Authorized provider number
*/
IF ldec_paid_amount > 0 AND ((l_provider_no_status <> NOTMODIFIED! and ll_provider_no_original <> 0) OR (l_provider_type_code_status <> NOTMODIFIED! and trim(ls_provider_type_code_original) <> '') OR l_billable_item_type_status <> NOTMODIFIED! OR l_other_billable_item_desc <> NOTMODIFIED! ) THEN 
	MessageBox('Error', 'The following fields must not be modified if the authorized billable item has been partially or fully paid (the paid amount is greater than zero):'+&
	                             '~r•	Billable item type' +&
								   '~r•	Authorized provider type' +&	  
									'~r•	Other billable item description' +&	
								   '~r•	Authorized provider number')
	RETURN -1							
END IF 
	
/* 4.350 [DELETE] A an authorization must not be deleted if the billable item has been partially or fully paid (the paid amount is greater than zero or the paid quantity is greater than zero). */

/* 4.60 	The authorized amount must not be reduced to less than the paid amount. */
IF l_authorized_amount_status <> NOTMODIFIED! THEN
	IF (ldec_authorized_amount < ldec_paid_amount) AND ldec_paid_amount > 0 THEN
		MessageBox('Error', 'The authorized amount cannot be reduced to less than the paid amount of ' + string(ldec_paid_amount,"$###,###.00"))
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('authorized_amount')
		RETURN -1
	END IF

	/* 4.40	The authorized amount must not be increased If payments exist for the authorization item. REMOVED*/
				
	/*  4.280	The authorized amount, if entered, must be greater than zero and less than or 
				equal to the maximum authorized amount allowed for the billable item (i.e. Billable_Item_Rehab_Task_Xref.max_authorized_amount)	 */
	IF ldec_authorized_amount <= 0  THEN
		MessageBox('Error', 'The authorized amount, if entered, must be greater than zero.')
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('authorized_amount')
		RETURN -1	
	END IF 
				
	/*  4.280	The authorized amount, if entered, must be greater than zero and less than or 
			equal to the maximum authorized amount allowed for the billable item (i.e. Billable_Item_Rehab_Task_Xref.max_authorized_amount)	 */
	IF ldec_authorized_amount > ldec_max_authorized_amount THEN
		MessageBox('Error', 'The authorized amount, if entered, must be less than or equal to the maximum authorized amount ( ' + string(ldec_max_authorized_amount,"$###,###.00")  + ' ) allowed for the billable item ')
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('authorized_amount')
		RETURN -1	
	END IF 	
END IF 
		
	/*	The quantity has changed. */
IF l_authorized_quantity_status <> NOTMODIFIED! THEN 
	IF ldec_paid_quantity > 0 THEN
			
		/* 4.70	The authorized quantity must not be reduced to less than the paid quantity.	 */
		IF ldec_authorized_quantity < ldec_paid_quantity THEN
			MessageBox('Error', 'The authorized quantiy cannot be reduced to less than the paid quantity of ' + string(ldec_paid_quantity,"###,###.00"))
			idw_dw[1].SetFocus()
			idw_dw[1].SetColumn('authorized_quantity')
			RETURN -1
		END IF
				
		/* 4.50	The authorized quantity must not be increased if payments exist for the authorization item.	REMOVED*/
		
	END IF
END IF

IF NOT IsNull(ldec_authorized_quantity) AND NOT ISNULL(ldec_max_authorized_qty) AND l_authorized_quantity_status <> NOTMODIFIED! THEN
	
	/* 4.310	The authorized quantity must be greater than zero */
	IF ldec_authorized_quantity <= 0 THEN
		MessageBox('Error', 'The authorized quantity must be greater than zero.')
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('authorized_quantity')
		RETURN -1
	END IF
	
	/* 4.320	The authorized quantity of a billable item for a task must not be greater than the maximum authorized quantity for the billable item (i.e. Billable_Item_Rehab_Task_Xref.max_authorized_qty).    */
	IF ldec_authorized_quantity >  ldec_max_authorized_qty THEN
		MessageBox('Error', 'The authorized quantity of a billable item for a task must not be greater than the maximum authorized quantity  ( ' + string(ldec_max_authorized_qty,"###,###.00")  + ' ) for the billable item')
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn('authorized_quantity')
		RETURN -1
	END IF
	
	/* 4.330	The authorized quantity must not be increased by increments of more than the default authorized quantity 
					(i.e. Billable_Item_Rehab_Task_Xref.default_authorized_qty) if the billable item is for Physiotherapy Treatment
	     			under the Primary Physio program (Rehab Program P001 and billable item number 173) for the task. */
	// REVISED
	/*	4.330	The authorized quantity must not be increased by increments of more than the default authorized quantity
		(i.e. Billable_Item_Rehab_Task_Xref.default_authorized_qty) if the billable item is for Physiotherapy Treatment or Physiotherapy Treatment (at home)
		under the Primary Physio program (Rehab Program P001 and billable item number ‘173’ or ‘279’) for the task.
	*/
	IF ls_rehab_program_code = 'P001'  AND ( ll_billable_item_no = 173 OR ll_billable_item_no = 279 ) THEN 
		IF ( ldec_authorized_quantity - ldec_test_quantity) > ldec_default_authorized_qty THEN
			MessageBox('Error', 'The authorized quantity must not be increased by increments of more than '  +  string(ldec_default_authorized_qty,"###,###.00") +&
			                            '~rif the billable item is for Physiotherapy Treatment OR Physiotherapy Treatment at Home under the Primary Physio program for the task.')
			idw_dw[1].SetFocus()
			idw_dw[1].SetColumn('authorized_quantity')
			RETURN -1
		END IF 	
	END IF 	
END IF

/* 4.300	The authorized quantity must be entered.	 */
IF  (IsNull(ldec_authorized_quantity) OR ldec_authorized_quantity = 0) AND l_authorized_quantity_status <> NOTMODIFIED! THEN
	MessageBox('Error', 'The authorized quantity must be entered.')
	idw_dw[1].SetFocus()
	idw_dw[1].SetColumn('authorized_quantity')
	RETURN -1
END IF

/* 4.170	The authorized provider (type & provider number) should be entered if the related Rehab Task is for a Physiotherapy Service and the billable item is for one of the following:
			•	Primary Physio		Physiotherapy Treatment		[173] - P001
			•	Primary Physio 		Physiotherapy Assessment		[172] - P001
			•	Primary Physio 		Progress Report						[254] - P001
			•	Primary Physio 		Initial Assessment Report		[253] - P001
			•	Primary Physio 		Discharge Report					[255] - P001
			•	n/a			        	P & A Questionnaire				[256]
			•	Primary Physio 		Missed Appointment				[252] - P001
			•	Work Conditioning 	Physiotherapy Assessment		[278] - P003
			•	Work Conditioning 	Physiotherapy Treatment		[279] - P003
			•	Work Conditioning	Missed Appointment				[252] - P003
			•	Work Conditioning 	Work Conditioning Program		[277] - P003
			•	Shoulder Program 	Physiotherapy Assessment		[278] - P002
			•	Shoulder Program	Physiotherapy Treatment		[279] - P002
			•	Shoulder Program	Missed Appointment				[252] - P002
			•	Shoulder Program	Shoulder Program					[276] - P002
	REVISED
	4.170	The authorized provider (type & provider number) should be entered if the related Rehab Task is for a Physiotherapy Service and the 
	            billable item is for a service and the billable item is not one of the following (i.e. REHAB_TASK.rehab_service_code = ‘S022’ the Billable_Item.Is_A_Service_Flag = ‘Y’):
			24		Baby Sitting
			34		Care Allowance (Level 1)
			35		Care Allowance (Level 2)
			36		Care Allowance (Level 3)
			37		Care Allowance (Level 4)
			38		Care Allowance (Level 5)
			39		Care Allowance Assessment
			48		Clothing Allowance
			49		Clothing Allowance (Annual)
			50		Clothing Allowance (Rehabilitation)
			51		Clothing Allowance (Replacement)
			274	Travel Days for Treatment (By Injured Worker)
	
	ALSO: 
	4.180	The authorized provider should be a Physiotherapy Clinic if the related Rehab Task is for a Physiotherapy Service and the billable item is one of the following: 
	REVISED 
	4.180	The authorized provider, if entered,  should be a Physiotherapy Clinic  or a Hospital if the related Rehab Task is for a Physiotherapy Service and the billable item is one of the following
		         (ie provider_type_code ‘M’ and provider_sub_type_code = ‘06’ or ‘35’ and REHAB_TASK.rehab_service_code = ‘S022’).:
	REVISED
	4.180	The authorized provider, if entered by the user, should be a Physiotherapy Clinic  or a Hospital if the related rehab service is for Physiotherapy 
	            and the billable item is for a service and the billable item is not one of the following (ie provider_type_code ‘M’ and provider_sub_type_code = ‘06’ or ‘35’ 
				 and REHAB_TASK.rehab_service_code = ‘S022’ the Billable_Item.Is_A_Service_Flag = ‘Y’):		 
				•	24		Baby Sitting
				•	34		Care Allowance (Level 1)
				•	35		Care Allowance (Level 2)
				•	36		Care Allowance (Level 3)
				•	37		Care Allowance (Level 4)
				•	38		Care Allowance (Level 5)
				•	39		Care Allowance Assessment
				•	48		Clothing Allowance
				•	49		Clothing Allowance (Annual)
				•	50		Clothing Allowance (Rehabilitation)
				•	51		Clothing Allowance (Replacement)
				•	274	Travel Days for Treatment (By Injured Worker)
				
	REVISED
	4.190	The authorized provider, if entered by the user, should be the same  provider as the provider as the provider on the rehab task if the authorized provider is a 
	            Physiotherapy Clinic or a Hospital and the rehab service is for Physiotherapy and the billable item is for a service and the billable item is not one of the following 
				(i.e. REHAB_TASK.rehab_service_code = ‘S022’ and the Billable_Item.Is_A_Service_Flag = ‘Y’) :
				24		Baby Sitting
				34		Care Allowance (Level 1)
				35		Care Allowance (Level 2)
				36		Care Allowance (Level 3)
				37		Care Allowance (Level 4)
				38		Care Allowance (Level 5)
				39		Care Allowance Assessment
				48		Clothing Allowance
				49		Clothing Allowance (Annual)
				50		Clothing Allowance (Rehabilitation)
				51		Clothing Allowance (Replacement)
				274	Travel Days for Treatment (By Injured Worker)
 	(Rationale: want to ensure that the user does not mistakenly authorize a billable item to a different physio clinic from the clinic that is responsible for the treatment 
	 but we still need to allow for a different provider to be authorized to provide services and supplies that the clinic itself will not be providing. E.g. babysitting services, 
	 supplies such as TENS machines).
	 
	 REVISED
	 Can we change the wording on this BR 
	 4.190	The authorized provider, if entered by the user, should be the same  provider as the provider as the provider on the rehab task if the authorized provider is a 
	 Physiotherapy Clinic or a Hospital and the rehab service is for Physiotherapy and the billable item is for a service and the billable item is not one of the following
	 (i.e. REHAB_TASK.rehab_service_code = ‘S022’ and the Billable_Item.Is_A_Service_Flag = ‘Y’) :
	 		 
*/
IF ls_rehab_service_code = 'S022' AND ll_billable_xref_no > 0 THEN
	
	SELECT 	provider_sub_type_code
	INTO	 		:ls_provider_sub_type_code
	FROM 		PROVIDER
	WHERE 	provider_no 				= :ll_authorized_provider_no
	AND 			provider_type_code 	= :ls_authorized_provider_type_code
	USING 		SQLCA;
	SQLCA.nf_handle_error("n_authorizations","nf_check_bus_rule()","SELECT provider_sub_type_code") 
	
	SELECT 	provider_no, provider_type_code 
	INTO    	:ll_task_provider_no, :ls_task_provider_type
	FROM 		REHAB_TASK
	WHERE 	task_no 	= :ll_task_no
	AND			claim_no	= :ll_claim_no
	USING 		SQLCA;
	SQLCA.nf_handle_error("n_authorizations","nf_check_bus_rule()","SELECT provider_no, provider_type_code") 
		
	CHOOSE CASE ll_billable_item_no
		CASE 	24	, 34,	35	, 36, 37	,38,	39	, 48, 49, 50,	51, 274	
			// GOOD RESULTS
			
		CASE ELSE
			
				/* 4.170 */
				IF ISNULL(ls_authorized_provider_type_code) OR  trim(ls_authorized_provider_type_code) = '' OR  ll_authorized_provider_no < 1 OR ISNULL(ll_authorized_provider_no)  AND ls_billable_item_is_a_service = 'Y' THEN
					li_message = MessageBox('Invalid Authorized Provider Type',"The authorized provider should be entered if the related Rehab Task is for a Physiotherapy Service." +&
					                                        												 	"~rContinue with Save?",Question!,OKCancel!, 2)
					
					IF li_message = 2 THEN RETURN -1 
				END IF 	
				
				IF ll_authorized_provider_no > 0 AND NOT ISNULL(ll_authorized_provider_no) AND l_provider_no_status <> NOTMODIFIED! THEN
						//4.180
						IF (ls_provider_sub_type_code <> '35' AND ls_provider_sub_type_code <> '06' AND ls_authorized_provider_type_code <> 'M' AND ls_rehab_service_code = 'S022') AND ls_billable_item_is_a_service = 'Y' THEN
							li_message = MessageBox('Information',	"The authorized provider, if entered,  should be a Physiotherapy Clinic  or a Hospital if the related Rehab Task is for a Physiotherapy Service"+&
																		 				"~rContinue with Save?",Question!,OKCancel!, 2)
							
							IF li_message 	= 2 THEN RETURN -1 
							lb_check 			= FALSE
						END IF 
						
						//4.190
						IF lb_check = TRUE THEN // ONLY RUN THIS ONE IF THE OTHER ONE WAS NOT RUN.
							 IF 	( ll_authorized_provider_no <> ll_task_provider_no OR ls_authorized_provider_type_code <> ls_task_provider_type ) AND ls_billable_item_is_a_service = 'Y' THEN
									li_message = MessageBox('Information',	"The authorized provider for this billable item is different than the provider for this physiotherapy Task." +&
																								"~rDo you want to continue with the Save ?",Question!,OKCancel!, 2)
																														
									IF li_message = 2 THEN RETURN -1 
							 END IF 
						 END IF 
				 	END IF
	
	END CHOOSE
END IF 

RETURN 0


end function

public function integer nf_check_mandatory ();STRING				ls_type_code, ls_sub_type_code, ls_specific_code, ls_comment_required_flag, ls_revision_comment
LONG				ll_rehab_task_row
DECIMAL			ldec_authorized_quantity, ldec_authorized_amount
INTEGER			li_comment_len
dwItemStatus	l_authorized_amount, l_authorized_quantity, l_revision_comment_status, l_authorization_comment_status

/* accept the data windows */
IF  idw_dw[1].AcceptText()  < 0 THEN RETURN -1
	
ll_rehab_task_row = idw_dw[2].getrow()
	
IF isnull(ll_rehab_task_row) OR ll_rehab_task_row < 1 THEN RETURN -1

// GRAB THE VALUES WE NEED FROM THE REHAB_TASK
ls_specific_code 	= idw_dw[2].getitemstring( ll_rehab_task_row, 'task_specific_code')
ls_sub_type_code	= idw_dw[2].getitemstring( ll_rehab_task_row, 'task_sub_type_code')
ls_type_code			= idw_dw[2].getitemstring( ll_rehab_task_row, 'task_type_code')

	
//FIND OUT IF WE NEED THE COMMENT FOR THIS TASK
IF Trim(idw_dw[1].GetItemString(1,'authorization_comment')) = '' THEN

	SELECT comment_required_flag
	INTO		:ls_comment_required_flag
	FROM 	Task_Specific
	WHERE task_type_code 			= :ls_type_code
	AND 		task_sub_type_code 		= :ls_sub_type_code
	AND 		task_specific_code 		= :ls_specific_code
	USING 	SQLCA;
	SQLCA.nf_Handle_Error("n_authorizations","nf_check_mandatory()","SELECT comment_required_flag")
	
	IF ls_comment_required_flag = 'Y' THEN
		MessageBox('Invalid Comment','The comment is required for the task entered.')
		idw_dw[1].SetColumn('authorization_comment')
		idw_dw[1].SetFocus()
		RETURN -1
	END IF 
END IF 

l_authorized_amount						= idw_dw[1].GetItemStatus(1,'authorized_amount', Primary!)
l_authorized_quantity      				= idw_dw[1].GetItemStatus(1,'authorized_quantity', Primary!)
ldec_authorized_quantity				= idw_dw[1].GetItemdecimal(1,'authorized_quantity')
ldec_authorized_amount 				= idw_dw[1].GetItemdecimal(1,'authorized_amount')
ls_revision_comment						= idw_dw[1].GetItemstring(1,'revision_comment')
l_revision_comment_status 			= idw_dw[1].GetItemStatus(1,'revision_comment', Primary!)
l_authorization_comment_status 	= idw_dw[1].GetItemStatus(1,'authorization_comment', Primary!)

/* 4.360	An explanation must be entered when the authorized quantity or authorized amount is revised. */
IF l_authorized_amount	 = DATAMODIFIED! OR l_authorized_quantity  = DATAMODIFIED! THEN 
	// MAKE SURE THEY ARE DIFFERENT VALUES AND NOT JUST SET BACK TO THE OLD VALUE
	IF ldec_authorized_amount <> idw_dw[1].GetItemdecimal(1,'authorized_amount',primary!, TRUE) OR ldec_authorized_quantity <> idw_dw[1].GetItemdecimal(1,'authorized_quantity',primary!, TRUE) THEN
		IF trim(ls_revision_comment) = '' OR isnull(ls_revision_comment) THEN 
			IF ib_new_authorization <> TRUE THEN 
				nf_set_column_action(TRUE)
				MessageBox('Error', 'An explanation must be entered when the authorized quantity or authorized amount is revised.')
				RETURN -1
			END IF  
		
		ELSE
			//must be less than or equal to 200 and greater than 4
			IF LEN( trim(ls_revision_comment)) < 5  OR LEN( trim(ls_revision_comment)) > 200 THEN 
				MessageBox('Revision Comment', 'The revision comment must be longer then 4 and less then 200 characters')
				RETURN -1
			END IF 
				
			// SET THE VALUE FROM THE FAKE COLUMN - ONLY TIME THIS NEEDS TO BE DONE!	
			 idw_dw[1].setitem(1,'changed_amounts_comment', trim(ls_revision_comment))
		END IF  
	END IF 
END IF 

//remove any blanks
IF l_revision_comment_status <> notmodified! THEN
	idw_dw[1].SETITEM(1,'revision_comment',  TRIM(idw_dw[1].getitemstring( 1, 'revision_comment')))
END IF 

//remove any blanks
IF l_authorization_comment_status <> notmodified! THEN
	idw_dw[1].SETITEM(1,'authorization_comment',  TRIM(idw_dw[1].getitemstring( 1, 'authorization_comment')))
END IF 
			
RETURN 0
end function

public function integer nf_set_unused_fields ();LONG  		ll_row
STRING		ls_task_create_user_id

// determine which fields are null and fill them in with a default value
// at some point in time any of the following fields may be unused
ll_row = idw_dw[1].GetRow()
IF ll_row > 0 THEN
	IF IsNull(idw_dw[1].GetItemString(ll_row, 'authorization_comment')) OR TRIM(idw_dw[1].GetItemString(ll_row, 'authorization_comment')) = '' THEN
		idw_dw[1].SetItem(ll_row, 'authorization_comment','')
	END IF

	IF IsNull(idw_dw[1].GetItemString(ll_row, 'authorized_provider_type_code')) OR TRIM(idw_dw[1].GetItemString(ll_row, 'authorized_provider_type_code')) = '' THEN
		idw_dw[1].SetItem(ll_row, 'authorized_provider_type_code','')
	END IF
	
	IF IsNull(idw_dw[1].GetItemnumber(ll_row, 'authorized_provider_no')) THEN
		idw_dw[1].SetItem(ll_row, 'authorized_provider_no',0)
	END IF
	
	//The authorized by user must not be entered. authorized userid = the create user id of the authorization record
	f_user_id(ls_task_create_user_id)
	IF IsNull(idw_dw[1].GetItemString(ll_row, 'authorized_by_user_id')) OR TRIM(idw_dw[1].GetItemString(ll_row, 'authorized_by_user_id')) = '' THEN
		idw_dw[1].SetItem(ll_row, 'authorized_by_user_id', ls_task_create_user_id)
	END IF
END IF

RETURN 0





end function

public function long nf_get_next_identifier ();LONG  ll_task_no, ll_row

// get the next task number
ll_row = idw_dw[1].GetRow()
IF ll_row > 0 THEN
   IF idw_dw[1].GetItemNumber(ll_row,'task_no') = 0 OR  IsNull(idw_dw[1].GetItemNumber(ll_row,'task_no')) THEN 
	
		/*	get the next task number*/
		SELECT	 	Max(task_no)
		INTO 		:ll_task_no
		FROM 		REHAB_TASK
		WHERE 	claim_no = :il_claim_no
		USING 		SQLCA;
		SQLCA.nf_handle_error('Embedded SQL: select from Rehab_Task','n_authorizations','nf_get_next_identifier') 
		
		IF IsNull(ll_task_no) THEN
			ll_task_no = 0
		END IF
		
		ll_task_no = ll_task_no + 1
	END IF

ELSE
   RETURN 0
END IF

RETURN ll_task_no

end function

public function long nf_set_identifiers ();LONG  ll_no

IF idw_dw[1].RowCount() > 0 THEN
	idw_dw[1].SetItem(1,'claim_no', il_claim_no)
	idw_dw[1].SetItem(1,'task_no', il_task_no)
	IF idw_dw[1].GetItemNumber(1,'authorization_no') = 0 OR IsNull(idw_dw[1].GetItemNumber(1,'authorization_no')) THEN
		ll_no = nf_get_next_authorization_no()
		IF ll_no > 0 THEN
			idw_dw[1].SetItem(1,'authorization_no',ll_no)
		ELSE
			RETURN -1
		END IF
	END IF
END IF

RETURN ll_no
end function

public function integer nf_change_item (long al_datawindow, long al_row, string as_dw_column, string as_data);STRING					ls_fixed_fee_flag, ls_string, ls_is_a_service_flag, ls_provider_type_code, ls_other_billable_item_flag
LONG					ll_billable_xref_no, ll_claim_no, ll_billable_item_no, ll_provider_no, ll_task_no
INTEGER				li_entry_scenario
DECIMAL				ldec_authorized_quantity, ldec_authorized_amount, ldec_max_authorized_amount
DECIMAL				ldec_authorized_quantity_orig, ldec_authorized_amount_orig, ldec_quantity_computed, ldec_default_authorized_qty
BOOLEAN				lb_check_authorized_entry = FALSE
/*
Authorization number	system generated
Billable xref number		Billable_Item_Rehab_Task.billable_xref_no
Authorized quantity		Billable_Item_Rehab_Task.default_authorized_qty
Authorized amount		If the Billable_Item_Rehab_Task.fixed_fee_flag is Yes
				   	Billable_Item.fee for the billable item and the current date
				Otherwise,
					Billable_Item_Rehab_Task.max_authorized_amount
Fixed Fee Flag		Billable_Item_Rehab_Task.fixed_fee_flag  

When entering the Authorized Amount:

SERVICE provider and billable item determines whether the authorized amount is entered by user or not  
If the SERVICE provider is under the Contract for Fixed Fees
                If the Billable Item is a Fixed Fee
                                 Set the authorized amount to the max_authorized_amount    
                                The user cannot enter the authorized amount
                Otherwise
                                Do Not display the ‘Fixed Fee’ message over the authorized amount
                                If the authorized_amount_required_flag is Yes for the billable item
										The user must enter the authorized amount
                                Otherwise
                                       Set the authorized amount to the max_authorized_amount   
Otherwise
		Do Not display the ‘Fixed Fee’ message over the authorized amount
        If the authorized_amount_required_flag is Yes for the billable item
					The user must enter the authorized amount
        Otherwise
                    Set the authorized amount to the max_authorized_amount   
						  							  
 // Do the logic as illustrated above.	
IF ls_contract_flag = 'Y' THEN 
	IF  ls_fixed_fee_flag = 'Y' THEN 
		 // Set the authorized amount to the max_authorized_amount    
         //The user cannot enter the authorized amount
		RETURN 1	
	ELSE
		//Do Not display the ‘Fixed Fee’ message over the authorized amount
		IF ls_authorized_amount_required_flag = 'Y' THEN
			//The user must enter the authorized amount
			RETURN 2
		ELSE	
			// Set the authorized amount to the max_authorized_amount 
			RETURN 3
		END IF 	
	END IF 
ELSE
	//	Do Not display the ‘Fixed Fee’ message over the authorized amount
	IF ls_authorized_amount_required_flag = 'Y' THEN
		//The user must enter the authorized amount
		RETURN 4
	ELSE
		// Set the authorized amount to the max_authorized_amount 
		RETURN 5
	END IF 		
END IF 

*/

CHOOSE CASE al_datawindow
  	CASE 1
     	CHOOSE CASE as_dw_column
			CASE 'billable_xref_no'	
								
					ll_billable_xref_no 	= LONG(as_data)
					ll_claim_no 			=  idw_dw[1].getitemnumber (1,'claim_no')
					
					IF ISNULL(ll_billable_xref_no) OR ll_billable_xref_no < 1 THEN 
						MessageBox('Error', 'Could not determine the Billable Item Number.')
						idw_dw[1].SetFocus()
						idw_dw[1].SetColumn('billable_xref_no')
						RETURN -1
					END IF 
					
					SELECT a.fixed_fee_flag ,   a.default_authorized_qty,      a.max_authorized_amount,       b.is_a_service_flag, b.billable_item_no
					INTO 	:ls_fixed_fee_flag , :ldec_default_authorized_qty, :ldec_max_authorized_amount, :ls_is_a_service_flag,     :ll_billable_item_no
					FROM 	Billable_Item_Rehab_Task_Xref a join 
										Billable_Item b on a.billable_item_no = b.billable_item_no
					WHERE a.billable_xref_no  = :ll_billable_xref_no
					USING 	SQLCA;
					SQLCA.nf_handle_error("n_authorizations","nf_change_item()","SELECT fixed_fee_flag") 
					
					IF ISNULL(ls_fixed_fee_flag) OR trim(ls_fixed_fee_flag) < '' THEN 
						MessageBox('Error', 'Could not determine the Fixed Fee Flag.')
						idw_dw[1].SetFocus()
						idw_dw[1].SetColumn('billable_xref_no')
						RETURN -1
					END IF 
					
					idw_dw[1].setitem(al_row,'fixed_fee_flag', ls_fixed_fee_flag)
					
					// need  to default the default quantity based on the xref thingy 
					idw_dw[1].setitem(al_row,'authorized_quantity', ldec_default_authorized_qty)
																					
					/* 4.295	The authorized amount must be set to the maximum authorized amount allowed for a billable item (i.e. Billable_Item_Rehab_Task_Xref.max_authorized_amount) 
            						if the billable item has a fixed fee rate and the authorized provider is entered and the authorized provider is under the WorkSafeNB physio contract  
		    						(i.e. Billable_Item_Rehab_Task_Xref.fixed_fee_flag = ‘Y’ and PROVIDER.ephysio_flag = ‘Y’). */

					/* 4.297	The authorized amount must be set to the maximum authorized amount allowed for a billable item (i.e. Billable_Item_Rehab_Task_Xref.max_authorized_amount)
            						if the billable item has a fixed fee rate and the authorized provider is not entered.  */
										
					/* if the authorization amount is equal to the max authorized amount for the billable item
					    (from the lookup table), add an indicator that will indicate it is set to the max amount
						 cannot display text instead of max amount as the user still has to be able to change the amount if they want.
					*/
					idw_dw[1].setitem(al_row,'max_authorized_amount', ldec_max_authorized_amount) 
					
					/* 4.430	An authorization should default to the provider of the rehab task if the billable item is for a service and the
					               billable item is not one of the following (i.e. the Billable_Item.Is_A_Service_Flag = ‘Y’).
										24		Baby Sitting
										34		Care Allowance (Level 1)
										35		Care Allowance (Level 2)
										36		Care Allowance (Level 3)
										37		Care Allowance (Level 4)
										38		Care Allowance (Level 5)
										39		Care Allowance Assessment
										48		Clothing Allowance
										49		Clothing Allowance (Annual)
										50		Clothing Allowance (Rehabilitation)
										51		Clothing Allowance (Replacement)

									The user should be able to override this provider or remove it.
					*/
					IF ls_is_a_service_flag = 'Y' THEN
						
						CHOOSE CASE ll_billable_item_no
							CASE 24, 34, 35, 36, 37, 38, 39, 48, 49, 50, 51, 274
							CASE ELSE
								
									ll_task_no 		= idw_dw[2].getitemnumber (idw_dw[2].getrow(), 'task_no')
									ll_claim_no  	= idw_dw[2].getitemnumber (idw_dw[2].getrow(), 'claim_no') 
								
									SELECT provider_no, provider_type_code 
									INTO 	:ll_provider_no, :ls_provider_type_code
									FROM 	REHAB_TASK 
									WHERE claim_no 	= :ll_claim_no 
									AND 		task_no 	= :ll_task_no
									USING 	SQLCA;
									SQLCA.nf_handle_error("n_authorizations", "nf_change_item()", "SELECT provider_no, provider_type_code") 
								
									IF ll_provider_no > 0 AND ls_provider_type_code <> '' THEN 
										idw_dw[1].setitem(al_row,'authorized_provider_type_code', ls_provider_type_code) 
										idw_dw[1].setitem(al_row,'authorized_provider_no', ll_provider_no) 
									END IF 
									
						END CHOOSE
					END IF 
					
						lb_check_authorized_entry 	= TRUE
						ls_provider_type_code 	=  idw_dw[1].GetItemString(al_row,'authorized_provider_type_code')
						ll_provider_no				=  idw_dw[1].GetItemnumber(al_row,'authorized_provider_no')
						ll_billable_xref_no 			= 	 idw_dw[1].GetItemnumber(al_row,'billable_xref_no')
						
						/*
							2.2.	Rehab Plan changes

							#1. Authorization tab
							The Authorization tab will have additional functionality for the user to enter a description of a billable item, 
							if the billable item requires this description. The other description field should only be displayed if it is applicable for the billable item.
						*/
						ls_other_billable_item_flag = nf_get_other_billable_item_flag(ll_billable_item_no)
						IF ls_other_billable_item_flag = 'Y'  THEN 
							 idw_dw[1].Modify("t_other_billable_item_desc.Visible=1")
							 idw_dw[1].Modify("other_billable_item_desc.Visible=1")
						
						ELSE
							
							 idw_dw[1].setitem(al_row,'other_billable_item_desc', '') 
							 idw_dw[1].Modify("t_other_billable_item_desc.Visible=0")
							 idw_dw[1].Modify("other_billable_item_desc.Visible=0")
						END IF 
					
				CASE 'authorized_quantity'
				
					IF ib_new_authorization <> TRUE THEN 
						nf_set_column_action(TRUE)
					END IF 
					
				CASE 'authorized_amount'
					
					IF ib_new_authorization <> TRUE THEN 
						nf_set_column_action(TRUE)
					END IF  
					
				CASE 'revision_comment'	
					
					idw_dw[1].setitem(1,'revision_comment',trim(as_data))

				CASE 'authorization_comment'
					
					idw_dw[1].setitem(1,'authorization_comment',trim(as_data))
														
				CASE 'authorized_provider_no'
					ls_string 			= idw_dw[1].GetItemString(1,'authorized_provider_type_code')
					ll_provider_no	=  Long(as_data)
					nf_provider_address(ls_string,Long(as_data))
					
					lb_check_authorized_entry 	= TRUE
					ls_provider_type_code 			=  ls_string
					ll_billable_xref_no 					= 	 idw_dw[1].GetItemnumber(al_row,'billable_xref_no')
					
				CASE 'authorized_provider_type_code'	
					
					lb_check_authorized_entry = TRUE
					ls_provider_type_code 	=  as_data
					ll_provider_no				=  idw_dw[1].GetItemnumber(al_row,'authorized_provider_no')
					ll_billable_xref_no 			= 	 idw_dw[1].GetItemnumber(al_row,'billable_xref_no')
															
  	   END CHOOSE			
END CHOOSE

/*
max flag			        (t_1)		               	- if (  authorized_amount =  max_authorized_amount and fixed_fee_flag = 'N',1,0)
authorized_amount	(authorized_amount)  	- if (  fixed_fee_flag = 'Y' or ( authorized_amount =  max_authorized_amount and fixed_fee_flag = 'N') ,0,1)
fixed_rate_item	        (t_fixed_rate_item)		- if (  fixed_fee_flag = 'Y',1,0)
*/

//need to check if the authorized amount is enterable
IF lb_check_authorized_entry = TRUE THEN
	
	ll_claim_no  								= 	idw_dw[2].getitemnumber (idw_dw[2].getrow(), 'claim_no') 
	li_entry_scenario 						= nf_authorized_amount_entry_scenario(ll_billable_xref_no, ll_provider_no, ls_provider_type_code )
	ldec_max_authorized_amount 	= nf_get_max_authorized_amount(ll_billable_xref_no)
	
	nf_set_authorized_amount_scenario(li_entry_scenario, ll_claim_no,  ldec_max_authorized_amount, 1)
END IF

RETURN 0

end function

public function integer nf_provider_address (string as_recipient_type_code, long al_recipient_no);STRING	ls_address_line1, ls_address_line2, ls_city, ls_country, ls_province, ls_postal_code, ls_name
				
IF IsNull(as_recipient_type_code) OR IsNull(al_recipient_no) THEN
	MessageBox('Warning','Recipient number or type is missing')
	RETURN -1
END IF
	
SELECT 	name, address_line1, address_line2, city, prov_state_code, country_code, postal_code  
INTO 		:ls_name, :ls_address_line1, :ls_address_line2, :ls_city, :ls_province, :ls_country, :ls_postal_code  
FROM 		PROVIDER  
WHERE  	provider_no 				= :al_recipient_no  
AND   		provider_type_code 	= :as_recipient_type_code 
//AND 			active_flag 				= 'Y'   
USING 		SQLCA ;
IF  SQLCA.nf_handle_error("Embedded SQL: Retrieve on PROVIDER","n_authorizations","nf_provider_address") = 100 THEN  
	idw_dw[1].SetItem(1,"name",'')
	RETURN 100
END IF 

idw_dw[1].SetItem(1,"name",ls_name)
//idw_dw[1].SetItem(1,"address_line1",ls_address_line1)
//idw_dw[1].SetItem(1,"address_line2",ls_address_line2)
//idw_dw[1].SetItem(1,"city",ls_city)
//idw_dw[1].SetItem(1,"prov_state_code",ls_province)
//idw_dw[1].SetItem(1,"country",ls_country)
//idw_dw[1].SetItem(1,"postal_code",ls_postal_code)

RETURN 0
end function

public function boolean nf_check_accident_date (datetime adt_date);DATETIME	ldtm_date

SELECT accident_date
INTO 	:ldtm_date
FROM 	CLAIM
WHERE claim_no = :il_claim_no
USING 	SQLCA;
SQLCA.nf_handle_error('Embedded SQL: select from CLAIM','n_authorizations','nf_check_accident_date') 
	
/* the date passed in comes before the accident date - the check fails */	
IF Date(ldtm_date) > Date(adt_date) THEN
	RETURN FALSE
END IF
	
RETURN TRUE
end function

public function long nf_get_next_authorization_no ();LONG	ll_authorization_no

UPDATE Last_Authorization_No SET last_authorization_no = last_authorization_no + 1 USING SQLCA;
 SQLCA.nf_handle_error("Embedded SQL: Update Last_Authorization_No","n_authorizations","nf_get_next_authorization_no")

CHOOSE CASE SQLCA.SQLNRows
	/*	If update was successful (ie. SQLNRows would equal 1), read back the identifier*/	
	CASE 1
		SELECT Last_Authorization_No.last_authorization_no INTO :ll_authorization_no FROM Last_Authorization_No USING SQLCA;
		SQLCA.nf_handle_error("Embedded SQL: Update Last_Authorization_No","n_authorizations","nf_get_next_authorization_no")
		
	CASE ELSE
		/*		if anything other than 1 record found, display error*/
		SQLCA.nf_rollback_transaction()
		IF SQLCA.SQLCode <> 0 THEN
			Error.Text 					= "Error during rollback of Last_authorization_No in function nf_get_next_authorization_no"
			Error.WindowMenu		= "N_authorization"
			Error.Object					= ""
			Error.ObjectEvent			= "nf_get_next_authorization_no"
			SignalError()
		END IF		
		MessageBox("authorization Module - Data Integrity Error", string(SQLCA.SQLNRows) + " record(s) found in Last_authorization_No~r~nPlease call the help desk",Exclamation!)
		RETURN -1

END CHOOSE
		
RETURN ll_authorization_no


end function

public function integer nf_retrieve (long al_claim_no, long al_task_no, long al_authorization_no);LONG  			ll_rows, ll_billable_xref_no, ll_provider_no
STRING			ls_provider_type_code
DECIMAL		ldec_max_authorized_amount
INTEGER		li_entry_scenario

ll_rows = idw_dw[1].Retrieve(al_claim_no , al_task_no, al_authorization_no)
SQLCA.nf_handle_error('Retrieve of dw','n_authorizations','nf_retrieve') 

IF ll_rows < 1 THEN
	MessageBox('Retrieval Error','No authorizations.')
	RETURN -1
END IF
							
ll_billable_xref_no 			=  idw_dw[1].getitemnumber(1,'billable_xref_no')
ll_provider_no 				=  idw_dw[1].getitemnumber(1,'authorized_provider_no')
ls_provider_type_code 	=  idw_dw[1].getitemstring(1,'authorized_provider_type_code')

SELECT  	max_authorized_amount
INTO 	 	:ldec_max_authorized_amount 
FROM 		Billable_Item_Rehab_Task_Xref 
WHERE 	billable_xref_no  = :ll_billable_xref_no
USING 		SQLCA;
SQLCA.nf_handle_error("n_authorizations","nf_retrieve()","SELECT fixed_fee_flag") 
																		
/*
max flag			        (t_1)		               	- if (  authorized_amount =  max_authorized_amount and fixed_fee_flag = 'N',1,0)
authorized_amount	(authorized_amount)  	- if (  fixed_fee_flag = 'Y' or ( authorized_amount =  max_authorized_amount and fixed_fee_flag = 'N') ,0,1)
fixed_rate_item	        (t_fixed_rate_item)		- if (  fixed_fee_flag = 'Y',1,0)
*/
li_entry_scenario = nf_authorized_amount_entry_scenario(ll_billable_xref_no, ll_provider_no, ls_provider_type_code)
nf_set_authorized_amount_scenario(li_entry_scenario, al_claim_no, ldec_max_authorized_amount, 0)

RETURN ll_rows

end function

public function integer nf_set_claim_no (long al_claim_no, long al_task_no);
il_claim_no 	= al_claim_no
il_task_no 		= al_task_no
 
RETURN 0
end function

public function integer nf_insert (long al_row, string as_task_type_code);
idw_dw[1].Reset()
	
IF idw_dw[1].InsertRow(al_row) < 0 THEN RETURN -1	
IF nf_set_defaults()                   < 0 THEN RETURN -1

RETURN 0



end function

public function long nf_check_for_authorization (integer ai_level, string as_task_type, string as_task_sub_type, string as_task_specific);/*	This function is used to determine if an authorization record is allowed
	to be created for a task record. It	can be run for two levels, level 1 is
	at the task_type level while level 2 is at the task_type, task_sub_type
	and, task_specific level.
*/
	STRING	ls_value
	INTEGER	li_count1, li_count2
	
	IF ai_level = 1 THEN

/*	Count the number of records at level 1 with an authorization_level_code of 'N',
   and if it equals the total number of records at level 1 then restrict addition.
*/
		SELECT count(*)
		  INTO :li_count1
		  FROM Task_Specific
		 WHERE task_type_code = :as_task_type
		 USING SQLCA;

		SQLCA.nf_handle_error('Embedded SQL: SELECT count(*) INTO :li_count1','n_authorizations','nf_check_for_authorization()') 
		
		
		/* REMOUT JAMES WALKER - authorization_level_code no longer exists */
//		SELECT count(*)
//		  INTO :li_count2
//		  FROM Task_Specific
//		 WHERE task_type_code = :as_task_type
//		   AND authorization_level_code = 'N'
//		 USING SQLCA;

		SQLCA.nf_handle_error('Embedded SQL: SELECT count(*) INTO :li_count2','n_authorizations','nf_check_for_authorization()')
		
		IF li_count1 = li_count2 THEN
			MessageBox('Authorization Error','The addition of an authorization for the selected record is not permitted.')
			RETURN -1
		END IF
	ELSE
		
		
	/* REMOUT JAMES WALKER - authorization_level_code no longer exists */		
/*	Query the Task_Specific table to see if the combination is allowed.
*/
//		SELECT authorization_level_code
//		  INTO :ls_value
//		  FROM Task_Specific
//		 WHERE task_type_code = :as_task_type
//	   	AND task_sub_type_code = :as_task_sub_type
//			AND task_specific_code = :as_task_specific
//		 USING SQLCA;

		SQLCA.nf_handle_error('Embedded SQL: SELECT authorization_level_code INTO :ls_value','n_authorizations','nf_check_for_authorization()') 

		IF ls_value = 'N' THEN
			MessageBox('Authorization Error','The addition of an authorization for the selected record is not permitted.')
			RETURN -1
		END IF
	END IF

	RETURN 0
	
end function

public subroutine nf_set_new_authorization (boolean ab_new_authorization);ib_new_authorization = ab_new_authorization

end subroutine

public function integer nf_update ();/*************************************************************************/
INT	li_cntr = 1, li_bound

li_bound = UpperBound(idw_dw)
DO WHILE li_cntr <= li_bound
	IF li_cntr <> 2 THEN //DON'T want to update the control datawindow - there should be a better way to do this -- REHAB_TASK
		idw_dw[li_cntr].Update()
		inv_transobj.nf_handle_error("n_authorization","nf_update()","idw_dw[li_cntr].Update()")
	END IF 
	// Error Handling
	li_cntr ++
LOOP

RETURN 0
end function

public function integer nf_set_billable_item_xref_filter (string as_service_code, string as_program_code, string as_task_type_code, string as_sub_type_code, string as_task_specific_code, boolean ab_new_authorization);DATAWINDOWCHILD		ldwc_child
STRING							ls_filter
INTEGER						li_result, li_counter

idw_dw[1].GetChild('billable_xref_no', ldwc_child)
ldwc_child.SetTransObject(SQLCA)
ldwc_child.SetFilter('')
ldwc_child.Filter()
ldwc_child.reset()
ldwc_child.Retrieve(as_service_code,as_program_code,as_task_type_code, as_sub_type_code, as_task_specific_code )
SQLCA.nf_handle_error("n_authorizations","nf_set_billable_item_xref_filter()","ldwc_child.Retrieve(as_service_code,as_program_code,as_task_type_code, as_sub_type_code, as_task_specific_code )")

IF ab_new_authorization = TRUE THEN
	ls_filter 	= "active_flag = 'Y' and billable_item_no <> 257"
	li_result = ldwc_child.SetFilter(ls_filter)
	li_result = ldwc_child.Filter()
END IF 

RETURN 0


end function

public subroutine nf_set_column_action (boolean ab_visible);
IF ab_visible = TRUE THEN
	idw_dw[1].Modify("t_revision_comment.Visible='1'")
	idw_dw[1].Modify("revision_comment.Visible='1'")
ELSE
	idw_dw[1].Modify("t_revision_comment.Visible='0'")
	idw_dw[1].Modify("revision_comment.Visible='0'")	
END IF 
end subroutine

public function decimal nf_get_unit_price (long al_billable_xref_no, date adt_datecheck);DECIMAL 			ldec_return_value

SELECT		 unit_fee 
INTO 		:ldec_return_value
FROM 		Billable_Item_Fee 
WHERE		billable_xref_no = :al_billable_xref_no
AND			( 	:adt_datecheck >= effective_from_date  AND :adt_datecheck <= effective_to_date
OR        		:adt_datecheck >= effective_from_date  AND effective_to_date IS NULL )
USING 	SQLCA;
SQLCA.nf_handle_error("n_authorizations", "nf_get_unit_price()", "SELECT unit_fee ")

IF ISNULL(ldec_return_value)  THEN ldec_return_value = 0

RETURN ldec_return_value
end function

public function integer nf_authorized_amount_entry_scenario (long al_xref_no, long al_no, string as_type);STRING			ls_fixed_fee_flag, ls_contract_flag, ls_authorized_amount_required_flag

/*
When entering the Authorized Amount:

SERVICE provider and billable item determines whether the authorized amount is entered by user or not  
If the SERVICE provider is under the Contract for Fixed Fees
                If the Billable Item is a Fixed Fee
                                 Set the authorized amount to the max_authorized_amount    
                                The user cannot enter the authorized amount
                Otherwise
                                Do Not display the ‘Fixed Fee’ message over the authorized amount
                                If the authorized_amount_required_flag is Yes for the billable item
										The user must enter the authorized amount
                                Otherwise
                                       Set the authorized amount to the max_authorized_amount   
Otherwise
		Do Not display the ‘Fixed Fee’ message over the authorized amount
        If the authorized_amount_required_flag is Yes for the billable item
					The user must enter the authorized amount
        Otherwise
                    Set the authorized amount to the max_authorized_amount   
						  
1. Provider physio_contract = 'Y' and fixed fee is 'Y' then set to max user cannot modify
2. Provider physio_contract = 'Y' and fixed fee is 'N' authorized amount required = 'Y' user enters the amount
3. Provider physio_contract = 'Y' and fixed fee is 'N' authorized amount required = 'N' default the max user can modify
4. Provider physio_contract = 'N' (or no provider) and authorized amount required = 'Y' user enters the amount
5. Provider physio_contract = 'N' (or no provider) and authorized amount required = 'N' default the max user can modify						  
						  					  
*/			
									
ls_fixed_fee_flag 								= nf_get_fixed_fee_flag(al_xref_no)
ls_contract_flag 								= nf_get_physio_contract_flag(al_no, as_type)
ls_authorized_amount_required_flag 	= nf_get_authorized_amount_required_flag(al_xref_no)

// If noghting is fill in for these it should drop to the bottom.
IF ISNULL(ls_fixed_fee_flag) 								THEN ls_fixed_fee_flag 								= ''
IF ISNULL(ls_contract_flag) 								THEN ls_contract_flag 								= ''
IF ISNULL(ls_authorized_amount_required_flag) 	THEN ls_authorized_amount_required_flag 	= ''

// Do the logic as illustrated above.	
IF ls_contract_flag = 'Y' THEN 
	IF  ls_fixed_fee_flag = 'Y' THEN 
		 // Set the authorized amount to the max_authorized_amount    
         //The user cannot enter the authorized amount
		RETURN 1	
	ELSE
		//Do Not display the ‘Fixed Fee’ message over the authorized amount
		IF ls_authorized_amount_required_flag = 'Y' THEN
			//The user must enter the authorized amount
			RETURN 2
		ELSE	
			// Set the authorized amount to the max_authorized_amount 
			RETURN 3
		END IF 	
	END IF 
ELSE
	//	Do Not display the ‘Fixed Fee’ message over the authorized amount
	IF ls_authorized_amount_required_flag = 'Y' THEN
		//The user must enter the authorized amount
		RETURN 4
	ELSE
		// Set the authorized amount to the max_authorized_amount 
		RETURN 5
	END IF 		
END IF 

RETURN 0
end function

public function string nf_get_fixed_fee_flag (long al_xref_no);STRING			ls_fixed_fee_flag

SELECT 	fixed_fee_flag
INTO			:ls_fixed_fee_flag
FROM 		billable_item_rehab_task_xref
WHERE 	billable_xref_no = :al_xref_no
USING 		SQLCA;
SQLCA.nf_handle_error('n_authorizations', 'nf_get_fixed_fee_flag()', 'SELECT fixed_fee_flag') 

IF isnull(ls_fixed_fee_flag) then ls_fixed_fee_flag = ''

RETURN ls_fixed_fee_flag
end function

public function string nf_get_physio_contract_flag (long al_no, string as_type);STRING			ls_physio_contract_flag

IF as_type = 'I' THEN 
	
	ls_physio_contract_flag = 'N'

ELSE
	// grab the contract flag of the provider
	SELECT 	physio_contract_flag
	INTO 		:ls_physio_contract_flag
	FROM 		PROVIDER
	WHERE 	provider_no 				= :al_no
	AND 			provider_type_code 	= :as_type
	USING 		SQLCA;
	SQLCA.nf_handle_error("n_authorizations","nf_get_physio_contract_flag()","SELECT physio_contract_flag") 
	
	IF ISNULL(ls_physio_contract_flag) OR TRIM(ls_physio_contract_flag) = '' THEN ls_physio_contract_flag = ''
	
END IF 

RETURN ls_physio_contract_flag
end function

public function string nf_get_authorized_amount_required_flag (long al_billable_xref_no);STRING		ls_authorized_amount_flag

SELECT  	authorized_amount_required_flag 
INTO 		:ls_authorized_amount_flag
FROM 		billable_item_rehab_task_xref
WHERE 	billable_xref_no = :al_billable_xref_no
USING		SQLCA;
SQLCA.nf_handle_error("n_authorizations","nf_get_authorized_amount_required_flag()","SELECT authorized_amount_required_flag") 

IF ISNULL(ls_authorized_amount_flag) THEN ls_authorized_amount_flag = ''

RETURN ls_authorized_amount_flag

end function

public function decimal nf_get_max_authorized_amount (long al_billable_xref_no);DECIMAL		ldec_max_authorized_amount

SELECT   		max_authorized_amount
INTO 	 		:ldec_max_authorized_amount
FROM 			Billable_Item_Rehab_Task_Xref
WHERE 		billable_xref_no  = :al_billable_xref_no
USING 			SQLCA;
SQLCA.nf_handle_error("n_authorizations","nf_get_max_authorized_amount()","SELECT max_authorized_amount") 

IF ISNULL(ldec_max_authorized_amount) THEN ldec_max_authorized_amount = 0

RETURN ldec_max_authorized_amount


			

end function

public subroutine nf_set_authorized_amount_scenario (integer ai_scenario_no, long al_claim_no, decimal adec_max_authorized_amount, integer ai_status);/*
When entering the Authorized Amount:

SERVICE provider and billable item determines whether the authorized amount is entered by user or not  
If the SERVICE provider is under the Contract for Fixed Fees
                If the Billable Item is a Fixed Fee
                                 set the authorized amount to the max_authorized_amount    
                                The user cannot enter the authorized amount
                Otherwise
                                Do Not display the ‘Fixed Fee’ message over the authorized amount
                                If the authorized_amount_required_flag is Yes for the billable item
										The user must enter the authorized amount
                                Otherwise
                                       DEFAULT the authorized amount to the max_authorized_amount   
Otherwise
		Do Not display the ‘Fixed Fee’ message over the authorized amount
        If the authorized_amount_required_flag is Yes for the billable item
					The user must enter the authorized amount
        Otherwise
                    DEFAULT the authorized amount to the max_authorized_amount   
						  						  
1. Provider physio_contract = 'Y' and fixed fee is 'Y' then set to max user cannot modify
2. Provider physio_contract = 'Y' and fixed fee is 'N' authorized amount required = 'Y' user enters the amount
3. Provider physio_contract = 'Y' and fixed fee is 'N' authorized amount required = 'N' default the max user can modify
4. Provider physio_contract = 'N' (or no provider) and authorized amount required = 'Y' user enters the amount
5. Provider physio_contract = 'N' (or no provider) and authorized amount required = 'N' default the max user can modify

select * from PROVIDER WHERE physio_contract_flag = 'Y'
select * from PROVIDER WHERE physio_contract_flag = 'N'

select billable_item_desc_e, * from Billable_Item_Rehab_Task_Xref a
	join Billable_Item b on a.billable_item_no = b.billable_item_no where fixed_fee_flag = 'N'

select billable_item_desc_e, * from Billable_Item_Rehab_Task_Xref a
	join Billable_Item b on a.billable_item_no = b.billable_item_no where fixed_fee_flag = 'Y'

select billable_item_desc_e, * from Billable_Item_Rehab_Task_Xref a
	join Billable_Item b on a.billable_item_no = b.billable_item_no where authorized_amount_required_flag = 'N'

select billable_item_desc_e, * from Billable_Item_Rehab_Task_Xref a
	join Billable_Item b on a.billable_item_no = b.billable_item_no where authorized_amount_required_flag = 'Y'
	
	select billable_item_desc_e, * from Billable_Item_Rehab_Task_Xref a
	join Billable_Item b on a.billable_item_no = b.billable_item_no
and billable_xref_no = 11972

ai_status = 1 If record is being modified 	- do setitems
			  = 0 If record is being retrieved - NO setitems

*/		

CHOOSE CASE ai_scenario_no
		CASE 1
			// Set the authorized amount to the max_authorized_amount    
			//The user cannot enter the authorized amount
			idw_dw[1].modify("authorized_amount.visible = 0")
			idw_dw[1].modify("authorized_amount.protect = 1")
			idw_dw[1].modify("t_fixed_rate_item.visible = 1") //visible
			
			IF ai_status = 1 THEN 
				idw_dw[1].setitem(1,'authorized_amount', adec_max_authorized_amount)
								
				IF al_claim_no > 0 THEN//modified record	
					idw_dw[1].setitem(1,'revision_comment', 'Billable Item Change')	
				END IF
			END IF 

			//messagebox('Scenario - 1 ', 'Scenario - 1')

		CASE 2
			//Do Not display the ‘Fixed Fee’ message over the authorized amount
			//The user must enter the authorized amount
		
			idw_dw[1].modify("authorized_amount.protect = 0")
			idw_dw[1].modify("authorized_amount.visible = 1")
			idw_dw[1].modify("t_fixed_rate_item.visible = 0") // not visible
			
			IF ai_status = 1 THEN 
				idw_dw[1].setitem(1,'authorized_amount', 0)
			END IF 
				
			//messagebox('Scenario - 2 ', 'Scenario - 2')
							
		CASE 3
									
			idw_dw[1].modify("authorized_amount.protect = 0")
			idw_dw[1].modify("authorized_amount.visible = 1")
			idw_dw[1].modify("t_fixed_rate_item.visible = 0") // not visible
			
			IF ai_status = 1 THEN 
				// default the authorized amount to the max_authorized_amount - user can modify 
				idw_dw[1].setitem(1,'authorized_amount', adec_max_authorized_amount)
								
				IF al_claim_no > 0 THEN//modified record	
					idw_dw[1].setitem(1,'revision_comment', 'Billable Item Change')	
				END IF 
			END IF 	
				
			//messagebox('Scenario - 3 ', 'Scenario - 3')
							
		CASE 4
			//	Do Not display the ‘Fixed Fee’ message over the authorized amount
			//The user must enter the authorized amount
			idw_dw[1].modify("authorized_amount.protect = 0")
			idw_dw[1].modify("authorized_amount.visible = 1")
			idw_dw[1].modify("t_fixed_rate_item.visible = 0") // not visible
			
			IF ai_status = 1 THEN 
				idw_dw[1].setitem(1,'authorized_amount', 0)
			END IF 
			
			//messagebox('Scenario - 4 ', 'Scenario - 4')
							
		CASE 5
										
			idw_dw[1].modify("authorized_amount.protect = 0")
			idw_dw[1].modify("authorized_amount.visible = 1")
			idw_dw[1].modify("t_fixed_rate_item.visible = 0") //not visible
			
			IF ai_status = 1 THEN 
				// default the authorized amount to the max_authorized_amount - user can modify
				idw_dw[1].setitem(1,'authorized_amount', adec_max_authorized_amount)
								
				IF al_claim_no > 0 THEN//modified record	
					idw_dw[1].setitem(1,'revision_comment', 'Billable Item Change')	
				END IF
			END IF 
			
			//messagebox('Scenario - 5 ', 'Scenario - 5')
								
		CASE ELSE//0
			// nothing in here
			messagebox('Error ', 'The authorized amount scenario could not be determined')
END CHOOSE
									

end subroutine

public function string nf_get_other_billable_item_flag (long al_billable_item_no);STRING				ls_rta_other_billable_item_flag

SELECT 	rta_other_billable_item_flag
INTO			:ls_rta_other_billable_item_flag
FROM 		billable_item
WHERE 	billable_item_no = :al_billable_item_no
USING 		SQLCA;
SQLCA.nf_handle_error('n_authorizations', 'nf_get_other_billable_item_flag()', 'SELECT fixed_fee_flag') 

IF isnull(ls_rta_other_billable_item_flag) THEN ls_rta_other_billable_item_flag = ''

RETURN ls_rta_other_billable_item_flag
end function

public function long nf_get_billable_item_no (long al_billable_xref_no);LONG			ll_billable_item_no

SELECT   		billable_item_no
INTO 	 		:ll_billable_item_no
FROM 			Billable_Item_Rehab_Task_Xref
WHERE 		billable_xref_no  = :al_billable_xref_no
USING 			SQLCA;
SQLCA.nf_handle_error("n_authorizations","nf_get_billable_item_no()","SELECT billable_item_no") 

IF ISNULL(ll_billable_item_no) THEN ll_billable_item_no = 0

RETURN ll_billable_item_no


			

end function

on n_authorizations.create
call super::create
end on

on n_authorizations.destroy
call super::destroy
end on

event constructor;call super::constructor;idt_ephysio_implentation_date = Date(gdtm_ephysio_implementation_date)


if idt_ephysio_implentation_date = date('1900-01-01') THEN
	messagebox('Implementation Error', 'The ephysio implementation date could not be resolved. Please contact the helpdesk before proceeding.')
	return -1
end if 
end event

