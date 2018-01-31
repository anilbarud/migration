$PBExportHeader$w_physio_reimbursements.srw
$PBExportComments$A response window showing the formulary history
forward
global type w_physio_reimbursements from w_a_tool
end type
type cb_ok from commandbutton within w_physio_reimbursements
end type
type cb_cancel from commandbutton within w_physio_reimbursements
end type
type dw_rehab_invoice from u_datawindow within w_physio_reimbursements
end type
type dw_rehab_invoice_line_item from u_datawindow within w_physio_reimbursements
end type
type tab_line_item from tab within w_physio_reimbursements
end type
type tab_line_item from tab within w_physio_reimbursements
end type
type cb_add from commandbutton within w_physio_reimbursements
end type
type cb_minus from commandbutton within w_physio_reimbursements
end type
type dw_reimburse from u_datawindow within w_physio_reimbursements
end type
type cb_refresh from commandbutton within w_physio_reimbursements
end type
type cb_resize from commandbutton within w_physio_reimbursements
end type
end forward

global type w_physio_reimbursements from w_a_tool
integer x = 23
integer y = 376
integer width = 3401
integer height = 1840
string title = ""
boolean resizable = false
boolean righttoleft = true
boolean clientedge = true
event ue_postopen ( )
cb_ok cb_ok
cb_cancel cb_cancel
dw_rehab_invoice dw_rehab_invoice
dw_rehab_invoice_line_item dw_rehab_invoice_line_item
tab_line_item tab_line_item
cb_add cb_add
cb_minus cb_minus
dw_reimburse dw_reimburse
cb_refresh cb_refresh
cb_resize cb_resize
end type
global w_physio_reimbursements w_physio_reimbursements

type variables
U_DS 						idw_dw[]
U_DS						ids_rehab_task
U_DS						ids_payment_insert
U_DS						ids_unapplied_txn_insert
U_DS						ids_rehab_invoice
U_DS						ids_payment_document
U_DS						ids_rehab_task_authorization
U_DS						ids_authorizations_for_tabs
N_ACCOUNT_PAYMENT_CONTROLLER 	inv_controller
S_WINDOW_MESSAGE 							istr_message
W_ACCOUNT_PAYMENT						iwi_parent
w_inbasket                          				iwi_parent_window

n_resize 					inv_tab_resize
n_ephysio_invoice     inv_ephysio_invoice

str_info_to_tab istr_info_returned_from_tab

LONG			il_individual_no, il_claim_no

INTEGER ii_dw_resize =  980, ii_tab_resize = 900, il_open_status

end variables

forward prototypes
public subroutine wf_set_payment_object (n_payment anv_payment)
public subroutine wf_set_parent_window (window awi_parent)
public function integer wf_delete_record ()
public function integer wf_retrieve_billable_item_dddw (long al_task_no, long al_claim_no, long al_provider_no, string as_provider_type)
public function integer wf_save_invoice ()
public function integer wf_choose_reimburse_scenario ()
public function integer wf_set_reimburse_scenario_to_dw (integer ai_scenario)
protected function integer wf_add_record (long al_task_no)
public function integer wf_set_defaults_based_dw_values (long al_billable_xref_no, date adt_service_date)
public subroutine wf_set_submitted_amount ()
public subroutine wf_set_payment_method (long al_provider_no, string as_provider_type)
public function integer wf_get_address_info (long al_no, string as_provider_type)
public function integer wf_refresh_screen (string as_status)
public function integer wf_setup_tab ()
public subroutine wf_setup_window_initial ()
public function integer wf_check_buisness_rules ()
public subroutine wf_select_row (integer ai_row)
public subroutine wf_change_unit_price_on_provider_change (string as_contract_flag)
public subroutine wf_change_tax_flag_on_provider_change (long al_recipient_no, string as_recipient_type, long al_provider_no, string as_provider_type)
public subroutine wf_change_allow_payable_entry ()
end prototypes

event ue_postopen();INTEGER	li_selected_tab

//grab the currently selected tab to be used later
li_selected_tab = tab_line_item.selectedtab
IF li_selected_tab < 0  THEN li_selected_tab = 1

tab_line_item.selecttab(li_selected_tab)

IF li_selected_tab > 0 THEN
	 tab_line_item.Control[li_selected_tab].TabBackColor = RGB(0, 255, 255)
END IF 

end event

public subroutine wf_set_payment_object (n_payment anv_payment);
end subroutine

public subroutine wf_set_parent_window (window awi_parent);IF istr_message.as_mode = 'inbasketadd' or istr_message.as_mode = 'inbasketreject' THEN
	iwi_parent_window = awi_parent
ELSE
	iwi_parent = awi_parent
END IF
end subroutine

public function integer wf_delete_record ();/* The ability to delete an invoice that was created by a Physio Clinic via the physio website will be restricted to specific WorkSafeNB users.
*/
INTEGER				li_selected_row, li_rowcount, li_invoice_line_no, li_rehab_invoice_count, li_rehab_invoice_count_B, li_rehab_invoice_count_A
INTEGER				li_line_no, li_counter
LONG					ll_authorization_no, ll_payment_no, ll_claim_no, ll_rehab_invoice_no,  ll_docid, ll_task_no
LONG					ll_submitted_no, ll_billable_xref_no, ll_web_user_id
DECIMAL		 {2}	ldec_paid_quantity, ldec_total_payment_amount, ldec_rta_authorized_quantity, ldec_rta_paid_quantity, ldec_rta_authorized_amount, ldec_rta_paid_amount
DATETIME			 ldtm_processed_date, ldtm_compute_min_service_date, ldtm_min_start_date
STRING				ls_submitted_type_code, ls_ephysio_flag,  ls_billable_item_desc_e
			

// grab the line item that is deleted 
li_rowcount = dw_rehab_invoice_line_item.rowcount()
IF isnull(li_rowcount) OR li_rowcount < 1 THEN RETURN -1

li_selected_row = dw_rehab_invoice_line_item.getrow()
IF isnull(li_selected_row) OR li_selected_row < 1 THEN RETURN -1

//remove any records that didn't get removed in the button - i.e they hit the add and then tried to delete a different row.
FOR li_counter = 1 TO dw_rehab_invoice_line_item.rowcount()
	li_line_no = dw_rehab_invoice_line_item.getitemnumber(li_counter, 'line_no')
	IF isnull(li_line_no) OR li_line_no < 1 THEN 
		dw_rehab_invoice_line_item.deleterow(li_counter)
	END IF 	
NEXT
 
/*     FULL ON DELETE  FULL ON DELETE FULL ON DELETE ************************************/
/*
3	Delete Invoice Line Item										

For each invoice line item that is deleted:
REHAB_TASK_AUTHORIZATION
Column Name					Update Value
Claim_no							Not Updated
task_no							Not Updated
task_type_code					Not Updated
task_sub_type_code			Not Updated
task_specific_code				Not Updated
authorization_no				Not Updated
authorized_quantity			Not Updated
authorized_amount				Not Updated
paid_quantity					REHAB_TASK_AUTHORIZATION.paid_quantity - PAYMENT.paid_quantity
paid_amount					REHAB_TASK_AUTHORIZATION.paid_amount -PAYMENT.total_payment_amount
authorization_comment		Not Updated

Modification:
Yes, I removed the only line item for the task which means it probably needed to change the task status and the actual start & completion dates

--    task_status_code
--    01  planned    
--    02  in progress
--    03  cancelled  
--    04  closed     
--    05  reset   

-- 1) MUST not  have actual_start_date & actual_completion_date as planned or cancelled.
--              User defn of cancelled is it was never started and will not be.
-- 2) MUST      have actual_start_date but no actual_completion_date  as the task is "in-progress".
-- 3) MUST      have actual_start_date & actual_completion_date.
--              User defn of closed is the task was started and completed
--          OR  the task was started but no further work will be done on it even though it did not go thur to completion.  Hence the term "closed" instead of "completed". 
*/

ll_authorization_no 	= dw_rehab_invoice_line_item.getitemnumber(li_selected_row, 'authorization_no')
ll_payment_no			= dw_rehab_invoice_line_item.getitemnumber(li_selected_row, 'payment_no')
ll_claim_no				= dw_rehab_invoice_line_item.getitemnumber(li_selected_row, 'claim_no')
ll_rehab_invoice_no 	= dw_rehab_invoice_line_item.getitemnumber(li_selected_row, 'rehab_invoice_no')
li_invoice_line_no		= dw_rehab_invoice_line_item.getitemnumber(li_selected_row, 'line_no')
ll_task_no				= dw_rehab_invoice_line_item.getitemnumber(li_selected_row, 'task_no')
ll_web_user_id			= dw_rehab_invoice_line_item.getitemnumber(li_selected_row, 'web_create_id')
ll_billable_xref_no		= dw_rehab_invoice_line_item.getitemnumber(li_selected_row, 'billable_xref_no')       
ll_docid					= istr_message.al_doubleparm[2]

// make sure they have valid values
IF isnull(ll_authorization_no) 	OR ll_authorization_no 	< 0 THEN RETURN -1
IF isnull(ll_claim_no)       		     OR ll_claim_no 				< 0 THEN RETURN -1
IF isnull(ll_rehab_invoice_no)  	OR ll_rehab_invoice_no 	< 0 THEN RETURN -1
IF isnull(li_invoice_line_no)  	    	OR li_invoice_line_no 		< 0 THEN RETURN -1

/* grab some basic checks stuff */
SELECT 	authorized_quantity,  paid_quantity , authorized_amount, paid_amount
INTO 		:ldec_rta_authorized_quantity, :ldec_rta_paid_quantity, :ldec_rta_authorized_amount, :ldec_rta_paid_amount
FROM  	REHAB_TASK_AUTHORIZATION 
WHERE	authorization_no  	= :ll_authorization_no
USING 	SQLCA;
SQLCA.nf_handle_error('w_physio_reimbursements','wf_delete()','	SELECT authorized_quantity,  paid_quantity ')

/* continue with delete */
SELECT 	paid_quantity, 			total_payment_amount, 		 	processed_date
INTO		:ldec_paid_quantity, 	:ldec_total_payment_amount, 	:ldtm_processed_date
FROM 	PAYMENT
WHERE 	payment_no = :ll_payment_no
AND 		claim_no 		= :ll_claim_no
USING 	SQLCA;
SQLCA.nf_handle_error('w_physio_reimbursement', 'wf_delete_record()', 'SELECT ldec_paid_quantity, ldec_total_payment_amount') 

IF isnull(ldec_paid_quantity) 			THEN ldec_paid_quantity 				= 0
IF isnull(ldec_total_payment_amount) THEN ldec_total_payment_amount 	= 0

/*	An invoice line item must only be deleted if the associated payment has not been processed
	(i.e. the PAYMENT.processed_date is null for the REHAB_INVOICE_LINE_ITEM.payment_no)
*/
IF NOT ISNULL(ldtm_processed_date) THEN 
	 messagebox('Delete Error', 'The Payment associated with this line item has been Processed and cannot be deleted.') 
	 RETURN -1
END IF 

/* An invoice line item must not be deleted if the the invoice was submitted via the Physio website 
	(the submitted by is a provider that is set up to use the Physio website – REHAB_INVOICE.submitted_type_code and the REHAB_INVOICE.submitted_no 
	is a PROVIDER with a PROVIDER.ephysio_flag = ‘Y’) and the billable item is an automatically invoiced item
	(i.e. the Billable_item_Rehab_Task_Xref.auto_invoice_flag = Yes  for the REHAB_INVOICE_LINE_ITEM.billable_xref_no).
	
	NOTE: In delete section "The user will be to delete a line item for an invoice created through this module and for an invoice created by a clinic through the Physio website. "
	which contradicts this rule.
*/
SELECT 	submitted_by_type_code, submitted_by_no 
INTO 		:ls_submitted_type_code, :ll_submitted_no
FROM 	REHAB_INVOICE
WHERE 	rehab_invoice_no = :ll_rehab_invoice_no
USING 	SQLCA;
SQLCA.nf_handle_error('w_physio_reimbursement', 'wf_delete_record()', 'SELECT submitted_type_code, submitted_no') 

//grab the ephysio flag for the provider -- question on this one
ls_ephysio_flag = inv_ephysio_invoice.nf_get_ephysio_flag(ll_submitted_no, ls_submitted_type_code)

IF il_open_status = 1 THEN 
	IF ll_web_user_id > 0 THEN 
		 messagebox('Delete Error', 'An invoice line item must not be deleted if the the invoice was submitted via the Physio website.') 
		 RETURN -1
	END IF 
END IF 

//Grab the item description
ls_billable_item_desc_e = inv_ephysio_invoice.nf_get_item_description(ll_billable_xref_no)

/* Make sure the user wants to delete the record */
IF messagebox('Delete Line Item', 'Are you sure you want to delete the highlighted line ' +  ' ** ' + ls_billable_item_desc_e , question!, yesno!, 2) = 2 THEN
	RETURN 0
END IF

SQLCA.nf_begin_transaction()

/*********************************** DO THE DELETE DO THE DELETE DO THE DELETE ***************************/

UPDATE 	REHAB_TASK_AUTHORIZATION
SET 		paid_quantity 	= paid_quantity - :ldec_paid_quantity,
      		paid_amount 	= paid_amount 	- :ldec_total_payment_amount
FROM 	REHAB_TASK_AUTHORIZATION
WHERE 	authorization_no = :ll_authorization_no
USING 	SQLCA;
SQLCA.nf_handle_error('w_physio_reimbursement', 'wf_delete_record()', 'UPDATE REHAB_TASK_AUTHORIZATION') 

/* PAYMENT */
DELETE 	PAYMENT 
WHERE  	payment_no 	= :ll_payment_no
AND 		claim_no 		= :ll_claim_no
USING 	SQLCA;
SQLCA.nf_handle_error('w_physio_reimbursement', 'wf_delete_record()', 'DELETE PAYMENT ') 

/* UNAPPLIED_CLAIM_TXN  */
DELETE 	UNAPPLIED_CLAIM_TXN 
WHERE  	payment_no = :ll_payment_no
AND 		claim_no 		= :ll_claim_no
USING 	SQLCA;
SQLCA.nf_handle_error('w_physio_reimbursement', 'wf_delete_record()', 'DELETE UNAPPLIED_CLAIM_TXN') 

/* PAYMENT_DOCUMENT  */
DELETE 	PAYMENT_DOCUMENT 
WHERE  	payment_no = :ll_payment_no
AND 		doc_id 		= :ll_docid
USING 	SQLCA;
SQLCA.nf_handle_error('w_physio_reimbursement', 'wf_delete_record()', 'DELETE UNAPPLIED_CLAIM_TXN') 

// need this to determine if the rehab invoice can be deleted
/*
19.180 A rehab task must be set to ‘planned’ if all associated payments are deleted.
19.190 A rehab task must not have an actual completion date or actual start date if the rehab status is ‘Planned’.
*/
SELECT COUNT(* )
INTO 	  :li_rehab_invoice_count_A
FROM   REHAB_INVOICE_LINE_ITEM a 
    join   PAYMENT b ON a.claim_no = b.claim_no AND a.payment_no = b.payment_no
WHERE  a.claim_no 	 =    :ll_claim_no 
AND      a.task_no 		 =    :ll_task_no
AND      b.payment_no <> :ll_payment_no
USING  SQLCA;
SQLCA.nf_handle_error('w_physio_reimbursement', 'wf_delete_record()', 'SELECT COUNT(*) INTO :li_rehab_invoice_count A') 

SELECT COUNT(*) 
INTO 	     :li_rehab_invoice_count_B
FROM 	 REHAB_INVOICE_LINE_ITEM a
WHERE	a.claim_no 				= :ll_claim_no
AND 		a.rehab_invoice_no 	= :ll_rehab_invoice_no
AND 		a.line_no 				<> :li_invoice_line_no
USING 	SQLCA;
SQLCA.nf_handle_error('w_physio_reimbursement', 'wf_delete_record()', 'SELECT COUNT(*) INTO :li_rehab_invoice_count B') 

IF ISNULL(li_rehab_invoice_count_A) OR li_rehab_invoice_count_A < 0 THEN li_rehab_invoice_count_A = 0
IF ISNULL(li_rehab_invoice_count_B) OR li_rehab_invoice_count_B < 0 THEN li_rehab_invoice_count_B = 0
li_rehab_invoice_count = li_rehab_invoice_count_A + li_rehab_invoice_count_B 

/* Records will be deleted from the following tables when a REHAB_INVOICE_LINE_ITEM is deleted: */
dw_rehab_invoice_line_item.deleterow(li_selected_row)

IF  li_rehab_invoice_count = 0 THEN 
	
	/* If the Rehab Task is impacted by the deletion of a line item(s):
		REHAB_TASK
			Column Name	Update Value
			Task_status_code	if there are no invoices (REHAB_INVOICE) under this task Set to ‘01’ (Planned)
			Actual_start_date	If there are no invoices (REHAB_INVOICE) under this task Set to null
	*/
			UPDATE 	REHAB_TASK
			SET 		Task_status_code = '01', Actual_start_date = NULL, actual_completion_date = NULL
			WHERE 	claim_no 	= :ll_claim_no
			AND 		task_no 	= :ll_task_no
			USING 	SQLCA;
			SQLCA.nf_handle_error('w_physio_reimbursement', 'wf_delete_record()', 'UPDATE 	REHAB_TASK') 
			
		/* If there are no other REHAB_INVOICE_LINE_ITEM records associated with the REHAB_INVOICE, 
			then delete the REHAB_INVOICE record:
		*/
			dw_rehab_invoice.deleterow(1)

ELSE
		
	/* Otherwise (invoices still exist)
		If the min(REHAB_INVOICE_LINE_ITEM.service_date) of all invoices under this task is greater than the REHAB_TASK.actual_start_date then 
		Set to the min(REHAB_INVOICE_LINE_ITEM.service_date) of all invoices under this task 
		modified: 1.310	A task’s actual start date must be the earliest date of service of items billed against the task.		
	*/
		SELECT 	COUNT(*) 
		INTO 		:li_rehab_invoice_count_B
		FROM 	 REHAB_INVOICE_LINE_ITEM a
		WHERE	 a.claim_no 				= :ll_claim_no
		AND 		 a.task_no 				= :ll_task_no
		AND 		( a.line_no 				<> :li_invoice_line_no AND rehab_invoice_no <> :ll_rehab_invoice_no )
		USING 	 SQLCA;
		SQLCA.nf_handle_error('w_physio_reimbursement', 'wf_delete_record()', 'SELECT COUNT(*) INTO :li_rehab_invoice_count C') 
		
		IF ISNULL(li_rehab_invoice_count_B) THEN li_rehab_invoice_count_B = 0
	
		IF li_rehab_invoice_count_B > 0 THEN 
	
				SELECT 	min(service_date)
				INTO 		:ldtm_compute_min_service_date
				FROM 	 REHAB_INVOICE_LINE_ITEM 
				WHERE 	 claim_no 	= :ll_claim_no 
				AND 		 task_no 		= :ll_task_no
				AND 		 (line_no 		<> :li_invoice_line_no and rehab_invoice_no <> :ll_rehab_invoice_no )
				USING 	 SQLCA;
				SQLCA.nf_handle_error('w_physio_reimbursement', 'wf_delete_record()', 'SELECT 	min(service_date)') 
				
				// if the computed min service date is less then the existing date change it 
				SELECT 	actual_start_date
				INTO 		:ldtm_min_start_date
				FROM 	 REHAB_TASK 
				WHERE 	 claim_no 	= :ll_claim_no 
				AND 		 task_no 		= :ll_task_no
				USING 	 SQLCA;
				SQLCA.nf_handle_error('w_physio_reimbursement', 'wf_delete_record()', 'SELECT 	min(service_date)') 
				
				IF ldtm_min_start_date > ldtm_compute_min_service_date THEN 
										
					UPDATE 	REHAB_TASK
					SET 		actual_start_date 	= :ldtm_compute_min_service_date
					WHERE 	claim_no 				= :ll_claim_no
					AND 		task_no 				= :ll_task_no
					USING 	SQLCA;
					SQLCA.nf_handle_error('w_physio_reimbursement', 'wf_delete_record()', 'UPDATE 	REHAB_TASK(B)') 
				END IF 
			
		END IF 							
END IF

/*************** DO THE UPDATE AND COMMIT DO THE UPDATE AND COMMITT *************************************/
/********************************************************************************************************/

dw_rehab_invoice_line_item.update()
SQLCA.nf_handle_error('w_physio_reimbursement', 'wf_delete_record()', 'dw_rehab_invoice_line_item.update()') 

IF li_rehab_invoice_count = 0 THEN 
	dw_rehab_invoice.update()
	SQLCA.nf_handle_error('w_physio_reimbursement', 'wf_delete_record()', 'dw_rehab_invoice.update()') 
END IF 

SQLCA.nf_commit_transaction()

end function

public function integer wf_retrieve_billable_item_dddw (long al_task_no, long al_claim_no, long al_provider_no, string as_provider_type);/*
List of billable items
The list of billable items that are displayed will depend on the provider that provided the physio service or supply. 

If the provider is under the WorkSafeNB contract (i.e. PROVIDER.physio_contract_flag = ‘Y’)¸
then the list of billable items will be a combination of a list of billable items that are explicitly authorized and a list 
of billable items that can be implicitly authorized. For explicitly authorized items, the list will be all REHAB_TASK_AUTHORIZATION 
records for the related REHAB_TASK where the REHAB_TASK_AUTHORIZATION is authorized for the provider on the invoice that
was submitted for reimbursement. For implicitly authorized items, the list will be all Billable_Items_rehab_Task_Xref.explicit_authorization_flag=’N’ for the related REHAB_TASK).

If the provider is not under the WorkSafeNB contract (i.e. PROVIDER.physio_contract_flag = ‘N’)¸
then the list of billable items will  be a list of billable items that are explicitly authorized (i.e. all REHAB_TASK_AUTHORIZATION records for the related REHAB_TASK where the
REHAB_TASK_AUTHORIZATION is authorized for the provider on the invoice that was submitted for reimbursement).
*/
INTEGER 						li_rowcount
STRING							ls_physio_contract_flag
DATAWINDOWCHILD		ldwc_child

//select the physio_contract_flag from the PROVIDER
ls_physio_contract_flag = inv_ephysio_invoice.nf_get_physio_contract_flag(al_provider_no, as_provider_type)
IF ls_physio_contract_flag = '' THEN RETURN -1

//grab the child
dw_rehab_invoice_line_item.getchild('billable_xref_no', ldwc_child)

// always do the retrieve
ldwc_child.SetTransObject(SQLCA)
	
li_rowcount = ldwc_child.Retrieve(al_task_no, al_claim_no)
SQLCA.nf_handle_error("w_physio_reimbursement", "wf_retrieve_billable_item_dddw()", "ldwc_child.Retrieve()")
	
ldwc_child.SetFilter('')
ldwc_child.Filter()

//do the DD logic
IF ls_physio_contract_flag = 'Y' THEN 
		// do nothing all records valid
ELSE
	/*
	If the provider is not under the WorkSafeNB contract (i.e. PROVIDER.physio_contract_flag = ‘N’)¸
	then the list of billable items will  be a list of billable items that are explicitly authorized (i.e. all REHAB_TASK_AUTHORIZATION records for the related REHAB_TASK where the
	REHAB_TASK_AUTHORIZATION is authorized for the provider on the invoice that was submitted for reimbursement).
	*/
	ldwc_child.SetFilter('authorization_no > 0')
	ldwc_child.Filter()
	li_rowcount = ldwc_child.rowcount()	
END IF 

RETURN 1

end function

public function integer wf_save_invoice ();LONG     			ll_claim_no, ll_task_no, ll_next_rehab_invoice_no, ll_rehab_invoice_no, ll_billable_xref_no, ll_authorization_no_source
LONG				ll_rehab_invoice_line_item_count, ll_authorization_no, ll_next_payment_no,  ll_recipient_no_dw_reimburse
LONG				ll_next_unapplied_txn_no, ll_task_no_array[], ll_task_search_number, ll_rehab_authorization_source, ll_billable_xref_no_source 
INTEGER  			li_rtn, li_row, li_counter, li_payment_insert_row, li_unapplied_insert_row, li_record_found, li_line_no, li_max_line_no, li_scenario, li_counter_2
INTEGER			li_dw_reimburse_row, li_max_line_no_current, li_max_line_no_deleted, li_rehab_invoice_line_no, li_payment_document_insert_row
STRING   			ls_explanation,  ls_billable_unit_code, ls_is_a_service_flag
STRING				ls_payment_type_code, ls_payment_sub_type_code, ls_authorized_by_code, ls_task_status_code, ls_search_variable
STRING				ls_recipient_sub_type_code, ls_payment_method_code, ls_admin_region_code, ls_recipient_name, ls_address_line1
STRING				ls_use_default_address, ls_address_line2, ls_city, ls_prov_state_code, ls_country, ls_postal_code
STRING				ls_bank_no, ls_bank_transit_no, ls_bank_account_no, ls_bank_name, ls_recipient_type_code_dw_reimburse
DATETIME 		ldtm_cheque_deposit_date, ldtm_authorized_datetime
DATE				ldt_current_date, ldt_paid_to_date, ldt_paid_from_date, ldt_actual_start_date, ldt_scheduled_processing_date, ldt_compute_min_service_date
DATE               ldt_date_to_check, ldt_min_service_date_from_line_items
DECIMAL {2}		ldec_paid_quantity,  ldec_submitted_amount, ldec_tax_rate, ldec_paid_amount
DECIMAL {2}		ldec_total_tax_amount,  ldec_reimbursement_amount, ldec_authorization_limit
DECIMAL {2}		ldec_authorization_quantity_source, ldec_authorization_amount_source, ldec_unprocessed_quantity, ldec_unprocessed_amount
BOOLEAN			lb_update_rta
dwItemStatus	l_status_row

/*
When an Invoice is Created:
For each Invoice that is submitted
	Create a Rehab Invoice record
	Update the Rehab Task record, if the status is impacted
	Update the Last Rehab Invoice No table
	
	For each invoice line item on the invoice
	Create a Rehab Invoice Line Item record
	Create a Payment record
	For each recipient of the Payment:
		Create an Unapplied Claim Txn record
		Update the Last Claim Txn No table
	Create a Payment Document record to link scanned document to payment 
	Update the associated Rehab Task Authorization record (to adjust the quantity & amount paid)
	Update the Last Payment No table
*/

N_PROCESS_RUN_STATUS ln_process_run_status

/* CLEAR OUT ANY ROWS THAT DO NOT NEED TO BE LOOKED AT */
FOR li_counter = 1 TO ll_rehab_invoice_line_item_count
	
	//Grab the row status
	l_status_row 	=	dw_rehab_invoice_line_item.GetItemStatus(li_counter,0, Primary!)
	
	// this is going to have to change based on change or add
	IF l_status_row <> newmodified! AND l_status_row <> datamodified! THEN
		//delete the row
		dw_rehab_invoice_line_item.deleterow(li_counter)
	END IF 
NEXT

/******************************************************************************************/
/************************ CREATE REHAB INVOICE **********************************************/
/*
	A.	REIMBURSE THE PROVIDER 
	(i)		Provider is the provider that provided the service/supply [1]
	(ii)		Provider did not provide the service and clinic under contract [2]
	(iii)	Provider did not provide the service – clinic not under contract [3]
	
	B.	REIMBURSE THE INJURED WORKER 
	(i) 	Clinic is under contract [4]
	(ii)  	Clinic is not under contract [5]
	
	C.	SPLIT REIMBURSE [6]
*/

li_scenario = wf_choose_reimburse_scenario()
IF li_scenario < 1 THEN RETURN -1

//grab some default stuff
ldt_current_date					= date(f_server_datetime())
ll_claim_no								= il_claim_no

//PAYMENT
ids_payment_insert 								= CREATE u_ds
ids_payment_insert.dataobject 				= 'ds_ephysio_payment_insert'
ids_payment_insert.settransobject(sqlca)

//UNAPPLIED_CLAIM_TXN
ids_unapplied_txn_insert 						= CREATE u_ds
ids_unapplied_txn_insert.dataobject 		= 'ds_ephysio_unapplied_claim_txn_insert'
ids_unapplied_txn_insert.settransobject(sqlca)

//REHAB_INVOICE
ids_rehab_invoice 									= CREATE u_ds
ids_rehab_invoice.dataobject 					= 'ds_ephysio_rehab_invoice_select'
ids_rehab_invoice.settransobject(sqlca)

//PAYMENT_DOCUMENT
ids_payment_document 						= CREATE u_ds
ids_payment_document.dataobject 		= 'ds_ephysio_payment_document_insert'
ids_payment_document.settransobject(sqlca)

/*
REHAB_TASK
Column Name			Update Value
Task_status_code		if the task_status_code is ‘01’ (Planned) 
								update to ‘02’ (in Progress)
								
Actual_start_date		If the min(REHAB_INVOICE_LINE_ITEM.service_date) is less than the REHAB_TASK.actual_start_date then 
								Update to the REHAB_INVOICE_LINE_ITEM.service_date) for the invoice
*/

/* populate the task array */
// only one save - task ata time
ll_task_no_array[1] = istr_info_returned_from_tab.task_no

/* this takes an array of task numbers based on the tasks in the tabs */
ids_rehab_task 						= CREATE u_ds
ids_rehab_task.dataobject 		= 'ds_rehab_task_update'
ids_rehab_task.settransobject(sqlca)
li_row = ids_rehab_task.Retrieve(il_claim_no, ll_task_no_array)
SQLCA.nf_handle_error("w_physio_reimbursements", "wf_save_invoice()", "ids_rehab_task.Retrieve(ll_claim_no, ll_task_no)")

IF ISNULL(li_row) OR li_row < 0 THEN RETURN -1

/* grab some basic claim info to be used to determine payment info ADMIN REGION CODE*/
ls_admin_region_code =  inv_ephysio_invoice.nf_get_claim_admin_region_code(il_claim_no)

/*
REHAB_TASK_AUTHORIZATION
for the authorization number on the PAYMENT record:
Column Name	Update Value
paid_quantity	REHAB_TASK_AUTHORIZATION.paid_quantity +  PAYMENT.paid_quantity
paid_amount	REHAB_TASK_AUTHORIZATION.paid_amount + PAYMENT.total_payment_amount
*/
ids_rehab_task_authorization 							= CREATE u_ds
ids_rehab_task_authorization.dataobject 		= 'ds_rehab_task_authorization_update'
ids_rehab_task_authorization.settransobject(sqlca)
li_row = ids_rehab_task_authorization.Retrieve(ll_claim_no, ll_task_no_array )
SQLCA.nf_handle_error("w_physio_reimbursements", "wf_save_invoice()", "ids_rehab_task_authorization.Retrieve(ll_claim_no)")

IF ISNULL(li_row) OR li_row < 0 THEN RETURN -1

// grab information from the reimburse datawindow
li_dw_reimburse_row = dw_reimburse.getrow()
IF ISNULL(li_dw_reimburse_row) OR li_dw_reimburse_row < 1 THEN RETURN -1

ls_use_default_address 							= dw_reimburse.getitemstring( li_dw_reimburse_row, 'use_default_address_flag')
ll_recipient_no_dw_reimburse 				= dw_reimburse.getitemnumber( li_dw_reimburse_row, 'recipient_no')
ls_recipient_type_code_dw_reimburse 	= dw_reimburse.getitemstring( li_dw_reimburse_row, 'recipient_type_code')
ls_recipient_sub_type_code 					= dw_reimburse.getitemstring( li_dw_reimburse_row, 'recipient_sub_type_code')
ldt_scheduled_processing_date				= date(dw_reimburse.getitemdatetime( li_dw_reimburse_row, 'scheduled_processing_date'))
//ldt_scheduled_processing_date 				=  inv_ephysio_invoice.nf_get_thursday_processing_date(ls_recipient_type_code_dw_reimburse) // based on a bunch of rules
ls_payment_method_code						= dw_reimburse.getitemstring( li_dw_reimburse_row, 'payment_method_code')


ll_rehab_invoice_line_item_count = dw_rehab_invoice_line_item.rowcount()
IF ISNULL(ll_rehab_invoice_line_item_count) OR ll_rehab_invoice_line_item_count < 0 THEN RETURN -1

//select the max line number from the rehab_invoice_line_no table just in case this is a modification
ll_rehab_invoice_no = dw_rehab_invoice.getitemnumber(1,'rehab_invoice_no')
IF ll_rehab_invoice_no > 0 THEN 
	
	SELECT 	max(line_no)
	INTO    	:li_max_line_no_current
	FROM 		REHAB_INVOICE_LINE_ITEM
	WHERE 	rehab_invoice_no 		= :ll_rehab_invoice_no
	USING 		SQLCA;
	SQLCA.nf_handle_error("w_physio_reimbursements", "wf_save_invoice()", "SELECT max(line_no)")
	
	IF isnull(li_max_line_no) THEN li_max_line_no = 0
	
	SELECT 	max(line_no)
	INTO    	:li_max_line_no_deleted
	FROM 		REHAB_INVOICE_LINE_ITEM_DELETED
	WHERE 	rehab_invoice_no 		= :ll_rehab_invoice_no
	USING 		SQLCA;
	SQLCA.nf_handle_error("w_physio_reimbursements", "wf_save_invoice()", "SELECT max(line_no) FROM REHAB_INVOICE_LINE_ITEM_DELETED")
	
	IF isnull(li_max_line_no_current) THEN li_max_line_no_current 		= 0
	IF isnull(li_max_line_no_deleted) THEN li_max_line_no_deleted 	= 0
	IF isnull(li_max_line_no) 				THEN li_max_line_no 					= 0
	IF isnull(ll_rehab_invoice_no) 		THEN ll_rehab_invoice_no 			= 0
	
	IF li_max_line_no_current >  li_max_line_no_deleted  THEN 
		li_max_line_no = li_max_line_no_current
	ELSE 
		li_max_line_no = li_max_line_no_deleted
	END IF 

ELSE
	
	li_max_line_no = 0
	
END IF 

/******************************************************************************************
Daytime Payment Processing
- '002' refers to the Account Payment Maintenance module, '044' refers to the Payment Processing module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('002','044','save',SQLCA)

// module is blocked
IF li_rtn = 1 THEN 	
	messagebox('Payment Processing', 'Payments are currently being processed please try again in a minute')
	RETURN -1
END IF 
/*************************** END OF PROCESSING CHECK *********************/

SQLCA.nf_begin_transaction()

//grab the last rehab invoice number this will be used on the datawindow - setitem
IF ll_rehab_invoice_no > 0 THEN 
	ll_next_rehab_invoice_no = ll_rehab_invoice_no
ELSE
	
	ll_next_rehab_invoice_no =  inv_ephysio_invoice.nf_get_next_rehab_invoice_no()
	IF ISNULL(ll_next_rehab_invoice_no) OR ll_next_rehab_invoice_no < 0 THEN RETURN -1
	
	dw_rehab_invoice.setitem(1, 'rehab_invoice_no', ll_next_rehab_invoice_no )	
	dw_rehab_invoice.setitem(1, 'invoice_date', istr_message.adt_dateparm[1] )		//document_index.date_on_doc
	dw_rehab_invoice.setitem(1, 'submitted_date',  istr_message.adt_dateparm[2] )	//document_index.date_recieved

END IF 

/* If recipient type code is ‘I’ 	‘ ‘ (blank) Otherwise  PROVIDER.provider_sub_type_code for the Provider */
IF ls_recipient_type_code_dw_reimburse <> 'I' THEN 
		
	SELECT 	provider_sub_type_code, 		name, 				address_line1,
	               address_line2,                  		city,                 prov_state_code, 
					country_code, 						postal_code
	INTO 		:ls_recipient_sub_type_code, :ls_recipient_name, :ls_address_line1,
	              	:ls_address_line2, 					:ls_city, 				 :ls_prov_state_code, 		
				    :ls_country, 							:ls_postal_code
	FROM 		PROVIDER  
	WHERE  	provider_no        		= :ll_recipient_no_dw_reimburse  
	AND  		provider_type_code 	= :ls_recipient_type_code_dw_reimburse
	USING 		SQLCA;
	SQLCA.nf_handle_error("w_physio_reimbursements", "wf_save_invoice()", "SELECT 	provider_sub_type_code")
	
	//default address = 'Y' for non individuals
	 ls_use_default_address = 'Y'
		
ELSE
		
	IF ls_use_default_address = 'Y' THEN 
			
		SELECT 	address_line1,     			address_line2,          city,                      
						prov_state_code, 			country_code, 			postal_code
		INTO 		:ls_address_line1, 			:ls_address_line2, 		:ls_city, 
						:ls_prov_state_code, 		:ls_country, 				:ls_postal_code
		FROM 		INDIVIDUAL  
		WHERE 	individual_no = :il_individual_no
		USING 		SQLCA;
		SQLCA.nf_handle_error("w_physio_reimbursements", "wf_save_invoice()", "SELECT 	address_line1, address_line2, city,")
			
	ELSE	
				
		ls_address_line1 		= TRIM(dw_reimburse.getitemstring( li_dw_reimburse_row, 'address_line1')	)	
		ls_address_line2 	  	= TRIM(dw_reimburse.getitemstring( li_dw_reimburse_row, 'address_line2')	)		
		ls_city 					= TRIM(dw_reimburse.getitemstring( li_dw_reimburse_row, 'city'))	
		ls_prov_state_code 	= TRIM(dw_reimburse.getitemstring( li_dw_reimburse_row, 'prov_state_code'))			
		ls_country 				= TRIM(dw_reimburse.getitemstring( li_dw_reimburse_row, 'country')	)				
		ls_postal_code			= TRIM(dw_reimburse.getitemstring( li_dw_reimburse_row, 'postal_code'))	
	
	END IF
	
		ls_recipient_name     = TRIM(dw_reimburse.getitemstring( li_dw_reimburse_row, 'recipient_name'))
END IF

FOR li_counter = 1 TO ll_rehab_invoice_line_item_count
	
	//Grab the row status
	l_status_row 	=	dw_rehab_invoice_line_item.GetItemStatus(li_counter,0, Primary!)
	
	// this is going to have to change based on change or add
	IF l_status_row <> newmodified! THEN CONTINUE
	
	//grab the next payment_no
	ll_next_payment_no =  inv_ephysio_invoice.nf_get_next_payment_no()
	IF ISNULL(ll_next_payment_no) OR ll_next_payment_no < 0 THEN RETURN -1
	
	ll_authorization_no 		= dw_rehab_invoice_line_item.getitemnumber(li_counter, 'authorization_no' )
	ll_claim_no 					= dw_rehab_invoice_line_item.getitemnumber(li_counter, 'claim_no' )
	ll_task_no						= dw_rehab_invoice_line_item.getitemnumber(li_counter, 'task_no' )
	li_rehab_invoice_line_no	= dw_rehab_invoice_line_item.getitemnumber(li_counter, 'line_no' )
	
	/* DON'T CONTINUE IF THE line_no > 0 */
	IF li_rehab_invoice_line_no > 0 THEN CONTINUE
	
	/***************************** REHAB_INVOICE_LINE_ITEM ********************************************/
	dw_rehab_invoice_line_item.setitem(li_counter, 'rehab_invoice_no', ll_next_rehab_invoice_no ) //REHAB_INVOICE.invoice_no
	
	IF li_rehab_invoice_line_no > 0 THEN 
		//DO NOTHING
	ELSE
		li_max_line_no = li_max_line_no + 1
		dw_rehab_invoice_line_item.setitem(li_counter, 'line_no', li_max_line_no ) //	Starting at 1 and incrementing by 1 for each additional line item on the invoice (unless it's a modification of the record)
	END IF 
	
	// set the payment number onto the record
	dw_rehab_invoice_line_item.setitem(li_counter, 'payment_no', ll_next_payment_no )
	
	//UP THE LINE NUMBER
	 li_line_no ++ 
	
	// Grab information from the line item that we need for the PAYMENT
	ldec_paid_quantity 							= dw_rehab_invoice_line_item.getitemdecimal( li_counter, 'quantity')		//see DB
	ldec_paid_amount								= dw_rehab_invoice_line_item.getitemdecimal( li_counter, 'total_non_tax_amount')		
	ldt_paid_from_date							= date(dw_rehab_invoice_line_item.getitemdatetime( li_counter,  'service_date' ))						
	ldt_paid_to_date								= date(dw_rehab_invoice_line_item.getitemdatetime( li_counter,  'service_date' ))						
	ldec_total_tax_amount 						= dw_rehab_invoice_line_item.getitemdecimal( li_counter, 'compute_total_tax')	//see DB
	ldec_submitted_amount						= dw_rehab_invoice_line_item.getitemnumber( li_counter, 'submitted_amount')
	ldec_tax_rate									= dw_rehab_invoice_line_item.getitemdecimal( li_counter, 'tax_rate')	
	ll_billable_xref_no    							= dw_rehab_invoice_line_item.getitemnumber( li_counter, 'billable_xref_no')	
	ldec_reimbursement_amount 				= dw_rehab_invoice_line_item.getitemnumber( li_counter, 'total_award_amount')	
	ls_is_a_service_flag 							= dw_rehab_invoice_line_item.getitemstring(li_counter, 'is_a_service_flag' )
	ls_billable_unit_code							= dw_rehab_invoice_line_item.getitemstring(li_counter, 'billable_unit_code' )
	
	li_payment_insert_row = ids_payment_insert.insertrow(0)
	IF ISNULL(li_payment_insert_row) OR li_payment_insert_row < 0 THEN RETURN -1
	
	SELECT  	payment_type_code, payment_sub_type_code 
	INTO			:ls_payment_type_code, :ls_payment_sub_type_code
	FROM	 	Billable_Item_Rehab_Task_Xref
	WHERE 	billable_xref_no = :ll_billable_xref_no
	USING 		SQLCA;
	SQLCA.nf_handle_error("w_physio_reimbursements","wf_save_invoice()","SELECT payment_type_code, payment_sub_type_code ")
	
	/* setting of the payment authorization fields 
		Current user's id if the user has the proper authorization limit
		" " if the user does not have the proper authorization limit.
		Current date + Current time if the user has the proper authorization limit 
		Database nullable - Null if the user does not have the proper authorization limit.				
	*/
	/*	Total payment amount can exceed users' authorization limit but will be held until authorized */
	ldec_authorization_limit    = gnv_user_authorizations.nf_get_authorization_limit( ls_admin_region_code, 'act')
	IF ldec_reimbursement_amount > ldec_authorization_limit THEN 
		ls_authorized_by_code = ''
		setnull(ldtm_authorized_datetime)
	ELSE
		ls_authorized_by_code 			= vgst_user_profile.user_id
		ldtm_authorized_datetime		= f_server_datetime()	
	END IF 
	
	/*************************    PAYMENT **************************************************/
	ids_payment_insert.SetItem(li_payment_insert_row, "payment_no", ll_next_payment_no) //Last_Payment_No.last_payment_no + 1
	ids_payment_insert.SetItem(li_payment_insert_row, "claim_no", ll_claim_no) //REHAB_INVOICE_LINE_ITEM.claim_no
	ids_payment_insert.SetItem(li_payment_insert_row, "opening_no", 0) //DB
	ids_payment_insert.SetItem(li_payment_insert_row, 'benefit_calculation_no', 0) //DB
	ids_payment_insert.SetItem(li_payment_insert_row, 'award_no', 0) //DB
	ids_payment_insert.SetItem(li_payment_insert_row, "payment_type_code", ls_payment_type_code) //Payment_Billable_Item_Xref.payment_type_code for the billable item
	ids_payment_insert.SetItem(li_payment_insert_row, "payment_sub_type_code", ls_payment_sub_type_code) //Payment_Billable_Item_Xref.payment_sub_type_code for the billable item
	ids_payment_insert.SetItem(li_payment_insert_row, 'final_payment_flag', "N") //DB
	ids_payment_insert.SetItem(li_payment_insert_row, 'paid_days_lost', 0) //DB
	ids_payment_insert.SetItem(li_payment_insert_row, "paid_hours_lost", 0) //DB
	ids_payment_insert.SetItem(li_payment_insert_row, 'paid_quantity', ldec_paid_quantity)  //REHAB_INVOICE_LINE_ITEM.quantity
	ids_payment_insert.SetItem(li_payment_insert_row, "paid_from_date", ldt_paid_from_date) //REHAB_INVOICE_LINE_ITEM.service_date
	ids_payment_insert.SetItem(li_payment_insert_row, "paid_to_date", ldt_paid_to_date) //REHAB_INVOICE_LINE_ITEM.service_date
	ids_payment_insert.SetItem(li_payment_insert_row, "total_award_amount", ldec_reimbursement_amount) //REHAB_INVOICE_LINE_ITEM.??
	ids_payment_insert.SetItem(li_payment_insert_row, "submitted_amount", ldec_submitted_amount) //REHAB_INVOICE_LINE_ITEM.
	ids_payment_insert.SetItem(li_payment_insert_row, 'total_deductions', 0) //DB
	ids_payment_insert.SetItem(li_payment_insert_row, 'tax_amount', ldec_total_tax_amount)  //REHAB_INVOICE_LINE_ITEM.total_tax_amount -- compute_total_tax
	ids_payment_insert.SetItem(li_payment_insert_row, 'loe_explanation', '') //‘  ’  (blank)
	ids_payment_insert.SetItem(li_payment_insert_row, 'payment_adjustment_flag',"N")
	ids_payment_insert.SetItem(li_payment_insert_row, "authorized_by_code", ls_authorized_by_code) //Based on authorization information
	ids_payment_insert.SetItem(li_payment_insert_row, "authorized_date", ldtm_authorized_datetime) //Based on authorization information
	ids_payment_insert.SetItem(li_payment_insert_row, 'authorization_no', ll_authorization_no) //REHAB_INVOICE_LINE_ITEM.authorization_no
	ids_payment_insert.SetItem(li_payment_insert_row, 'tax_rate', ldec_tax_rate) //REHAB_INVOICE_LINE_ITEM.tax_rate
	// submitted_amount								Submitted Amount from the screen
	
	ll_next_unapplied_txn_no =  inv_ephysio_invoice.nf_get_next_unapplied_txn_no()
	IF ISNULL(ll_next_unapplied_txn_no) OR ll_next_unapplied_txn_no < 0 THEN RETURN -1
	
	li_unapplied_insert_row = ids_unapplied_txn_insert.insertrow(0)
	IF ISNULL(li_unapplied_insert_row) OR li_unapplied_insert_row < 0 THEN RETURN -1
	
	ls_explanation 						= STRING(ids_rehab_invoice.getitemnumber(1,'external_invoice_no'))
	IF ISNULL(ls_explanation) OR TRIM(ls_explanation) = '' THEN
		ls_explanation = ''
	END IF 
 
	/***************************************  UNAPPLIED_CLAIM_TXN *********************************************/
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row, 'txn_no', ll_next_unapplied_txn_no) //	Last_Claim_Txn_No. last_txn_no + 1
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row, 'claim_no', ll_claim_no)  //PAYMENT.claim_no
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row, 'payment_no', ll_next_payment_no) //PAYMENT.payment_no
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row, 'txn_type_code', '1') // DB
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row, 'txn_sub_type_code', "") //DB
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row, "batch_no", 0)	//DB
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row, "recipient_no", ll_recipient_no_dw_reimburse ) 
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row, "recipient_type_code", ls_recipient_type_code_dw_reimburse ) 
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row, "recipient_sub_type_code", ls_recipient_sub_type_code)
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row, 'coc_period', 0) //DB
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row, 'manual_cheque_req_no', 0) //DB
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row, 'cheque_no', 0) //DB
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row, 'cheque_deposit_date', SetNull(ldtm_cheque_deposit_date)) //DB
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row, 'direct_deposit_xmit_no', 0) //DB
	
	//if txn_amount = zero, Payment method is 'I' (Inapplicable)  Otherwise Payment method from the screen  (A or D)
	IF ldec_reimbursement_amount = 0 THEN
			ls_payment_method_code = 'I'
	ELSE
			ls_payment_method_code	= dw_reimburse.getitemstring( li_dw_reimburse_row, 'payment_method_code')
	END IF 
	
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row, "payment_method_code", ls_payment_method_code)
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row, 'tax_amount', ldec_total_tax_amount) // PAYMENT.tax_amount
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row, 'txn_amount', ldec_reimbursement_amount) //Reimbursement Amount from the screen
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row, "admin_region_code", ls_admin_region_code) 	// CLAIM.admin_region_code for the claim
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row, "scheduled_processing_date", ldt_scheduled_processing_date)  
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row, 'explanation', ls_explanation) //REHAB_INVOICE.external_invoice_no
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row, 'related_txn_no',0) //DB
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row, "recipient_name", ls_recipient_name) //If recipient_type_code is ‘I’ 	INDIVIDUAL.name Otherwise	PROVIDER.name  
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row,"address_line1", ls_address_line1 ) //see rule
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row,"address_line2", ls_address_line2 ) //see rule
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row,"city", ls_city) //see rule 
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row,"prov_state_code", ls_prov_state_code ) //see rule
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row,"country", ls_country ) //see rule
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row,"postal_code", ls_postal_code) //see rule
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row,"use_default_address_flag", ls_use_default_address) //If recipient_type_code is ‘I’	From the Screen (entered by the user)Otherwise 	‘Y’ 
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row,"cheque_print_group_code", " ")	//DB
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row,'txn_unit_of_work_no', 0) //DB
	ids_unapplied_txn_insert.SetItem(li_unapplied_insert_row,'maintain_allowed_flag', "Y") //DB

	/*****************************************    PAYMENT_DOCUMENT *************************************/
	li_payment_document_insert_row = ids_payment_document.insertrow(0)
	ids_payment_document.SetItem(li_payment_document_insert_row, "payment_no", ll_next_payment_no) // PAYMENT.payment_no
	ids_payment_document.SetItem(li_payment_document_insert_row, "doc_id", istr_message.al_doubleparm[2]) //	doc_id	REF.docid of the user selected document.
	ids_payment_document.SetItem(li_payment_document_insert_row, "paid_status_code", "S") // ‘S’  (scheduled)
	ids_payment_document.SetItem(li_payment_document_insert_row, "paid_status_explanation_code", '') // blank
	
NEXT
	
/********* ??????????????????????????????????   REHAB_TASK_AUTHORIZATION ????????????????????????????????? *********/
//Column Name	Update Value
//paid_quantity	REHAB_TASK_AUTHORIZATION.paid_quantity +  PAYMENT.paid_quantity               >> ldec_paid_quantity
//paid_amount	REHAB_TASK_AUTHORIZATION.paid_amount  + PAYMENT. total_payment_amount >> ldec_total_payment_amount
FOR li_counter = 1 TO ids_rehab_task_authorization.rowcount()
	
	ll_billable_xref_no_source  =  ids_rehab_task_authorization.getitemnumber(li_counter,  'billable_xref_no')	
	ll_authorization_no_source = ids_rehab_task_authorization.getitemnumber(li_counter,  'authorization_no')	
	
	ldec_authorization_quantity_source = ids_rehab_task_authorization.getitemdecimal(li_counter,'paid_quantity')
	ldec_authorization_amount_source  = ids_rehab_task_authorization.getitemdecimal(li_counter,'paid_amount')
	
	IF isnull(ldec_authorization_quantity_source) 	THEN ldec_authorization_quantity_source 	= 0
	IF isnull(ldec_authorization_amount_source) 	THEN ldec_authorization_amount_source 	= 0
	
	ldec_unprocessed_quantity 	= 0
	ldec_unprocessed_amount 	= 0
	lb_update_rta						= FALSE
	
	FOR li_counter_2 = 1 TO dw_rehab_invoice_line_item.rowcount()
		IF dw_rehab_invoice_line_item.getitemstatus(li_counter_2, 0, PRIMARY!) = newmodified! THEN 
			IF 	ll_billable_xref_no_source = dw_rehab_invoice_line_item.getitemnumber( li_counter_2, 'billable_xref_no')	 AND  ll_authorization_no_source = dw_rehab_invoice_line_item.getitemnumber( li_counter_2, 'authorization_no')	  THEN 
			
					ls_is_a_service_flag 	= dw_rehab_invoice_line_item.getitemstring(li_counter_2, 'is_a_service_flag' )
					ls_billable_unit_code	= dw_rehab_invoice_line_item.getitemstring(li_counter_2, 'billable_unit_code' )
			
					 IF ls_is_a_service_flag = 'Y' AND ls_billable_unit_code <> 'EA' THEN 
						ldec_unprocessed_quantity = ldec_unprocessed_quantity + dw_rehab_invoice_line_item.getitemdecimal( li_counter_2, 'quantity')	
					ELSE 
						ldec_unprocessed_quantity = ldec_unprocessed_quantity + dw_rehab_invoice_line_item.getitemdecimal( li_counter_2, 'units_billed')
					END IF 
					
						ldec_unprocessed_amount = ldec_unprocessed_amount + dw_rehab_invoice_line_item.getitemdecimal( li_counter_2, 'total_award_amount')		
						 lb_update_rta = TRUE
				END IF 
			END IF 	
	NEXT 
	
		IF lb_update_rta = TRUE THEN 
			ids_rehab_task_authorization.setitem( li_counter, 'paid_quantity',  ldec_authorization_quantity_source + ldec_unprocessed_quantity)
			ids_rehab_task_authorization.setitem( li_counter, 'paid_amount',  ldec_authorization_amount_source + ldec_unprocessed_amount)	
		END IF 
	
NEXT

/*		REHAB_TASK
Column Name			Update Value
Task_status_code		if the task_status_code is ‘01’ (Planned) 
								update to ‘02’ (in Progress)							
Actual_start_date		If the min(REHAB_INVOICE_LINE_ITEM.service_date) is less than the REHAB_TASK.actual_start_date then 
								Update to the REHAB_INVOICE_LINE_ITEM.service_date) for the invoice										
*/
ls_task_status_code 		= ids_rehab_task.getitemstring(1, 'task_status_code')
ldt_Actual_start_date 	= date( ids_rehab_task.getitemdatetime(1, 'actual_start_date')  ) 

IF ls_task_status_code = '01'  THEN 
	ids_rehab_task.setitem(1, 'task_status_code', '02')
END IF 
	
ldt_min_service_date_from_line_items 	= inv_ephysio_invoice.nf_get_min_service_date_from_line_items(il_claim_no,  istr_info_returned_from_tab.task_no)		
ldt_compute_min_service_date 				= DATE(dw_rehab_invoice_line_item.getitemdatetime(1, 'compute_min_service_date'))
	
IF 	isnull(ldt_min_service_date_from_line_items) THEN
	ldt_date_to_check = ldt_compute_min_service_date
ELSE
	
	IF ldt_min_service_date_from_line_items > ldt_compute_min_service_date THEN
		ldt_date_to_check = ldt_compute_min_service_date
	ELSE
		ldt_date_to_check = ldt_min_service_date_from_line_items	
	END IF 	
END IF 


IF ldt_Actual_start_date > ldt_date_to_check  OR isnull(ldt_Actual_start_date) THEN 
	ids_rehab_task.setitem(1,'actual_start_date' , ldt_date_to_check)
END IF 

/************************************************/
/* the order for the following is very import - due to triggers */
/************************************************/

// REHAB_INVOICE
dw_rehab_invoice.update()
SQLCA.nf_handle_error("w_physio_reimbursements", "wf_save_invoice()", "dw_rehab_invoice.update()")

//PAYMENT UPDATE
ids_payment_insert.update()
SQLCA.nf_handle_error("w_physio_reimbursements", "wf_save_invoice()", "ids_payment_insert.update()")

//REHAB_INVOICE_LINE_ITEM
dw_rehab_invoice_line_item.update()
SQLCA.nf_handle_error("w_physio_reimbursements", "wf_save_invoice()", "dw_rehab_invoice_line_item.update()")

//UNAPPLIED_CLAIM_TXN UPDATE
ids_unapplied_txn_insert.update()
SQLCA.nf_handle_error("w_physio_reimbursements", "wf_save_invoice()", "ids_unapplied_txn_insert.update()")

//PAYMENT_DOCUMENT UPDATE
ids_payment_document.update()
SQLCA.nf_handle_error("w_physio_reimbursements", "wf_save_invoice()", "ids_payment_document.update()")

//REHAB_TASK UPDATE
ids_rehab_task.update()
SQLCA.nf_handle_error("w_physio_reimbursements", "wf_save_invoice()", "ids_rehab_task.update()")

//REHAB_TASK_AUTHORIZATION
ids_rehab_task_authorization.update()
SQLCA.nf_handle_error("w_physio_reimbursements", "wf_save_invoice()", "ids_rehab_task_authorization.update()")

//LAST NUMBER TABLES ARE UPDATED IN THEIR OWN FUNCTIONS

SQLCA.nf_commit_transaction()

RETURN 1

end function

public function integer wf_choose_reimburse_scenario ();STRING 	ls_rtc_dw_reimburse, ls_ptc_dw_rehab_invoice, ls_physio_contract_flag
INTEGER	li_dw_reimburse_row, li_dw_rehab_invoice_row
LONG		ll_recipient_no_dw_reimburse, ll_provider_no_dw_rehab_invoice
	
/*
	A.	REIMBURSE THE PROVIDER 
	(i)		Provider is the provider that provided the service/supply
	** 1 **
		
	(ii)		Provider did not provide the service and clinic under contract
	** 2 **
	
	(iii)	Provider did not provide the service – clinic not under contract
	** 3 **
	
	B.	REIMBURSE THE INJURED WORKER 
	(i) 	Clinic is under contract
	** 4 **
		
	(ii)  	Clinic is not under contract
	** 5 **
	
	NOTE the following for unit price....
	
	scenario 1 unit price open
	scenario 2 unit price  fixed fee
	scenario 3 unit price open
	scenario 4 unit price fixed fee
	scenario 5 unit price open
	
	MODIFIED TO:
	
When entering the invoice line item:
SERVICE provider and billable item determines whether the unit price is entered by user or not  
If the SERVICE provider is under the Contract for Fixed Fees
	If the Billable Item is a Fixed Fee
		Set the unit price from the Billable_Item_Fee table    
		The user cannot override this unit price or the amount payable
	Otherwise
		Do Not set the unit price
		The user must enter the unit price
Otherwise
	Do Not set the unit price 
	The user must enter the unit price	
*/
		
/* grab the working rows */
li_dw_reimburse_row  		= dw_reimburse.getrow()
li_dw_rehab_invoice_row 	= dw_rehab_invoice.getrow()

IF isnull(li_dw_reimburse_row) 		OR li_dw_reimburse_row 		= -1 THEN RETURN -1
IF isnull(li_dw_rehab_invoice_row) 	OR li_dw_rehab_invoice_row 	= -1 THEN RETURN -1

// grab the values we need
ls_rtc_dw_reimburse 						= dw_reimburse.getitemstring( li_dw_reimburse_row, 'recipient_type_code')
ll_recipient_no_dw_reimburse 		= dw_reimburse.getitemnumber( li_dw_reimburse_row, 'recipient_no')

ls_ptc_dw_rehab_invoice 				= dw_rehab_invoice.getitemstring( li_dw_rehab_invoice_row, 'provider_type_code')
ll_provider_no_dw_rehab_invoice 	= dw_rehab_invoice.getitemnumber( li_dw_rehab_invoice_row, 'provider_no')

//grab the physio contract flag
ls_physio_contract_flag = inv_ephysio_invoice.nf_get_physio_contract_flag(ll_provider_no_dw_rehab_invoice, ls_ptc_dw_rehab_invoice)
IF ls_physio_contract_flag = '' THEN RETURN -1


/*************     5     *************/
// (ii)  		REIMBURSE THE INJURED WORKER Clinic is not under contract	
IF ls_rtc_dw_reimburse = 'I' AND ls_physio_contract_flag = 'N' THEN RETURN 5

/*************     4     *************/
// (i) 	 	REIMBURSE THE INJURED WORKER Clinic is under contract
IF ls_rtc_dw_reimburse = 'I' AND ls_physio_contract_flag = 'Y' THEN RETURN 4

/*************     1     *************/
// 	(i)			REIMBURSE THE PROVIDER  Provider is the provider that provided the service/supply unit price open
IF ls_rtc_dw_reimburse = ls_ptc_dw_rehab_invoice AND ll_recipient_no_dw_reimburse = ll_provider_no_dw_rehab_invoice AND  ls_rtc_dw_reimburse <> 'I'THEN
	RETURN 1
END IF 

/*************     2     *************/
// (ii)			REIMBURSE THE PROVIDER  Provider did not provide the service and clinic under contract
IF ls_rtc_dw_reimburse <> 'I' AND ls_physio_contract_flag = 'Y' THEN RETURN 2

/*************     3     *************/
//	(iii)		REIMBURSE THE PROVIDER  Provider did not provide the service – clinic not under contract
IF  ls_rtc_dw_reimburse <> 'I' AND  ls_physio_contract_flag <> 'Y' THEN RETURN 3

// DON'T KNOW - RETURN 0
RETURN 0
	
end function

public function integer wf_set_reimburse_scenario_to_dw (integer ai_scenario);/*
	NOTE the following for unit price....
	
	scenario 1 unit price open
	scenario 2 unit price  fixed fee
	scenario 3 unit price open
	scenario 4 unit price fixed fee
	scenario 5 unit price open
	
	MODIFIED:
	
	SERVICE provider and billable item determines whether the unit price is entered by user or not  
If the SERVICE provider is under the Contract for Fixed Fees
	If the Billable Item is a Fixed Fee
		Set the unit price from the Billable_Item_Fee table    
		The user cannot override this unit price or the amount payable
	Otherwise
		Do Not set the unit price
		The user must enter the unit price
Otherwise
	Do Not set the unit price 
	The user must enter the unit price

		ls_fixed_fee_flag = inv_ephysio_invoice.nf_get_fixed_fee_flag(ll_billable_xref_no)
	
	// SEE ABOVE
	IF ls_fixed_fee_flag = 'Y'  AND as_contract_flag = 'Y' THEN 
		dw_rehab_invoice_line_item.setitem(li_counter,'unit_price', ldec_unit_fee)
		dw_rehab_invoice_line_item.setitem(li_counter,'total_award_amount',ldec_unit_fee)	
		dw_rehab_invoice_line_item.setitem(li_counter,'submitted_amount', ldec_unit_fee)	
	ELSE
		dw_rehab_invoice_line_item.setitem(li_counter,'unit_price',0)
		dw_rehab_invoice_line_item.setitem(li_counter,'total_award_amount',0)	
		dw_rehab_invoice_line_item.setitem(li_counter,'submitted_amount',0)	
	END IF 

*/

INTEGER 	li_counter, li_rowcount, li_current_scenario_number
STRING		ls_fixed_fee_flag
DATE		ldt_service_date
DECIMAL	ldec_unit_price
LONG		ll_billable_xref_no

li_rowcount = dw_rehab_invoice_line_item.rowcount()
IF isnull(li_rowcount) OR li_rowcount < 0 THEN RETURN -1

FOR li_counter = 1 TO li_rowcount
	
	ll_billable_xref_no 		= dw_rehab_invoice_line_item.getitemnumber(li_counter, 'billable_xref_no') 
	
	
	//grab the current scenario number
	li_current_scenario_number = dw_rehab_invoice_line_item.getitemnumber(li_counter, 'scenario_number' )
	
	//IF we are going from a non fixed rate to a fixed rate then we need to remove what the user entered an put in the fixed amount
	CHOOSE CASE li_current_scenario_number
		CASE 1,3,5
			
				IF ai_scenario = 2 OR ai_scenario = 4 THEN 
					//fix the amount on each row
					ls_fixed_fee_flag 		= dw_rehab_invoice_line_item.getitemstring(li_counter, 'fixed_fee')
					
					IF ls_fixed_fee_flag = 'Y' THEN
							
						ll_billable_xref_no 		= dw_rehab_invoice_line_item.getitemnumber(li_counter, 'billable_xref_no') 
						ldt_service_date		= DATE(dw_rehab_invoice_line_item.getitemdatetime(li_counter, 'service_date') )
					
						ldec_unit_price = inv_ephysio_invoice.nf_get_item_unit_price(ll_billable_xref_no, ldt_service_date)
							
						dw_rehab_invoice_line_item.setitem(li_counter,'unit_price',ldec_unit_price)
						dw_rehab_invoice_line_item.setitem(li_counter,'submitted_amount',ldec_unit_price)
						dw_rehab_invoice_line_item.setitem(li_counter,'total_award_amount',ldec_unit_price)
														
					END IF 
				END IF 
		CASE ELSE
	END CHOOSE
	
	dw_rehab_invoice_line_item.setitem(li_counter, 'scenario_number', ai_scenario)	
NEXT

RETURN 1
end function

protected function integer wf_add_record (long al_task_no);/*
Rehab_invoice_no			
Line_no							
Authorization_no			REHAB_TASK_AUTHORIZATION.authorizatio_no for the rehab authorization used to authorize the billable item
Claim_no						REHAB_TASK_AUTHORIZATION.claim_no
Task_no						REHAB_TASK_AUTHORIZATION.task_no
Service_date					Service Date from the screen (i.e. entered by the user)
Fee_no							If the billable item is a fixed fee item,
									Billable_Item_Fee.fee_no
									Otherwise,	0 (zero)
Billable_xref_no				Billable_Item_Rehab_Task_Xref.billable_xref_no 
Billable_unit_code			If the billable item is a fixed fee item,
									Billable_Item_Fee.billable_unit_code
									Otherwise,
									‘EA’ (each)
Quantity						Set to REHAB_INVOICE_LINE_ITEM.units
									Units	If Billable_Item.is_a_service_flag = ‘Y’ and
Billable_Item_Fee.billable_unit_code = ‘EA’ 1 (one) Otherwise Units from the screen (i.e. entered by the user)
Unit_price						Unit Price from the screen  (see BRs for how this screen field is set)
Total_non_tax_amount	Computed:  Units * Unit Price
Total_tax_amount			If tax_applied_flag = ‘Y’	Total_non_tax_amount * tax_rate
Total_amount				Computed:  Total_non_tax_amount + total_tax_amount
Tax_applied_flag			‘Y’ if a tax_rate was entered by the user
‘N’									if no tax_rate was entered by the user
*/

INTEGER 		li_row,  li_scenario
STRING			ls_recipient_type_code, ls_provider_type_code
LONG			ll_recipient_no, ll_provider_no

IF il_open_status = 2 THEN
	messagebox('Delete Only','Sorry but this window is for deleting web created Line Items only.', INFORMATION!)
	RETURN -1
END IF 

//recipient Information
ls_recipient_type_code 	= dw_reimburse.getitemstring(1, 'recipient_type_code')
ll_recipient_no 				= dw_reimburse.getitemnumber(1, 'recipient_no')

//provider information
ls_provider_type_code 	= dw_rehab_invoice.getitemstring(1, 'provider_type_code')
ll_provider_no 				= dw_rehab_invoice.getitemnumber(1, 'provider_no')

IF isnull(ll_recipient_no) OR ll_recipient_no < 1 THEN 
	messagebox('Recipient Number', 'Please select a valid Recipient Number before proceeding with record Add')
	RETURN -1
END IF 

// grab the row
li_row = dw_rehab_invoice_line_item.insertrow(0)
IF ISNULL(li_row) OR li_row < 0 THEN RETURN -1

/* do some defaults at this point */
dw_rehab_invoice_line_item.setitem(li_row, 'claim_no', il_claim_no)
dw_rehab_invoice_line_item.setitem(li_row, 'task_no', al_task_no )

/*  
CASE 1  REIMBURSE THE PROVIDER 	(i)		Provider is the provider that provided the service/supply 		
CASE 2 //REIMBURSE PROVIDER ii)	Provider did not provide the service and clinic under contract
CASE 3 // REIMBURSE PROVIDER   iii)	Provider did not provide the service – clinic not under contract			
CASE 4 // REIMBURSE THE INJURED WORKER  i) Clinic is under contract
CASE 5 //	REIMBURSE THE INJURED WORKER  ii)  Clinic is not under contract		
*/		

li_scenario = wf_choose_reimburse_scenario()
IF li_scenario < 1 THEN RETURN -1	// SHOULD BE 1 TO 5
		
dw_rehab_invoice_line_item.setitem(li_row, 'scenario_number', li_scenario)

wf_retrieve_billable_item_dddw(al_task_no, il_claim_no ,ll_provider_no, ls_provider_type_code)

end function

public function integer wf_set_defaults_based_dw_values (long al_billable_xref_no, date adt_service_date);/* Based on the Service Date and the Billable Item Number we need to set some defaults
this is so the user can modify the BI or the Service date and everything defaults as expected

When entering the invoice line item:
SERVICE provider and billable item determines whether the unit price is entered by user or not  
If the SERVICE provider is under the Contract for Fixed Fees
	If the Billable Item is a Fixed Fee
		Set the unit price from the Billable_Item_Fee table    
		The user cannot override this unit price or the amount payable
	Otherwise
		Do Not set the unit price
		The user must enter the unit price
Otherwise
	Do Not set the unit price 
	The user must enter the unit price
*/

INTEGER			li_tax_rate_no, li_lineitem_row, li_billable_xref_info_rowcount
STRING			ls_is_a_service_flag, ls_billable_unit_code, ls_explicit_authorization_flag, ls_fixed_fee_flag
STRING			ls_prov_state_code, ls_physio_contract_flag, ls_recipient_type_code, ls_provider_type_code, ls_submitted_by_type_code
LONG				ll_recipient_no,	ll_billable_item_no, ll_fee_no, ll_provider_no_invoice, ll_submitted_by_no
DECIMAL		 	ldec_unit_fee, ldec_units_billed
U_DS				lds_billable_xref_info
	
// grab all of the information for the requested item
lds_billable_xref_info 					= CREATE u_ds
lds_billable_xref_info.DataObject = 'ds_billable_xref_info'
lds_billable_xref_info.SetTransObject(SQLCA)
	
IF isnull(adt_service_date) OR STRING(adt_service_date) = '1900-01-01' THEN RETURN 0

IF ISNULL(al_billable_xref_no) OR al_billable_xref_no < 1 THEN RETURN 0

li_lineitem_row = dw_rehab_invoice_line_item.getrow()
IF ISNULL(li_lineitem_row) OR li_lineitem_row < 0 THEN RETURN -1

li_billable_xref_info_rowcount = lds_billable_xref_info.retrieve(al_billable_xref_no, adt_service_date)
SQLCA.nf_handle_error("w_physio_reimbursement", "wf_set_defaults_based_dw_values()", "SELECT physio_contract_flag")

/* do the basic checks to ensure we have everything we need */
IF li_billable_xref_info_rowcount <> 1  THEN 	RETURN -1

/* grab basic information from the line item dw that the user enters into */
ldec_units_billed = dw_rehab_invoice_line_item.getitemdecimal(1, 'units_billed')

// set the working variables 
ls_is_a_service_flag  					= lds_billable_xref_info.getitemstring(1, 'is_a_service_flag')
ls_billable_unit_code  					= lds_billable_xref_info.getitemstring(1, 'billable_unit_code')
ls_explicit_authorization_flag 		= lds_billable_xref_info.getitemstring(1, 'explicit_authorization_flag')
ls_fixed_fee_flag 						= lds_billable_xref_info.getitemstring(1, 'fixed_fee_flag')
ldec_unit_fee 							= lds_billable_xref_info.getitemdecimal(1, 'unit_fee')
ll_billable_item_no 						= lds_billable_xref_info.getitemnumber(1, 'billable_item_no')
ll_fee_no									= lds_billable_xref_info.getitemnumber(1, 'fee_no')

//from the recipient
ls_prov_state_code 		= dw_reimburse.getitemstring(1,'prov_state_code')
ll_recipient_no 				= dw_reimburse.getitemnumber(1, 'recipient_no')
ls_recipient_type_code 	= dw_reimburse.getitemstring(1, 'recipient_type_code') 

ll_provider_no_invoice    = dw_rehab_invoice.getitemnumber(1, 'provider_no')
ls_provider_type_code 	= dw_rehab_invoice.getitemstring(1, 'provider_type_code')
ls_physio_contract_flag = inv_ephysio_invoice.nf_get_physio_contract_flag(ll_provider_no_invoice, ls_provider_type_code)

ls_submitted_by_type_code = dw_rehab_invoice.getitemstring(1, 'submitted_by_type_code')
ll_submitted_by_no             = dw_rehab_invoice.getitemnumber(1, 'submitted_by_no')

//make sure we have a valid value
IF isnull(ls_physio_contract_flag) OR ls_physio_contract_flag = '' THEN RETURN -1

/* Currently, Paul only has the NB HST in the table but we will be adding NS and eventually, NB GST, etc.
	It should default to NB HST and the tax rate will depend on the date of service entered (service_date within the Tax_Rate.effective_date) */
li_tax_rate_no =  inv_ephysio_invoice.nf_get_tax_rate(1, ls_prov_state_code, adt_service_date)

//1.80	The quantity to be paid must be equal to one (1) for a billable item that is a service.
IF ls_is_a_service_flag = 'Y' THEN 
	dw_rehab_invoice_line_item.setitem(li_lineitem_row,'quantity',1)
END IF 

// 1.85	The quantity to be paid must be equal to the number of units for a billable item that is a supply and the billable unit code is type ‘each’ .
IF ls_is_a_service_flag = 'N' AND ls_billable_unit_code = 'EA' THEN 
	dw_rehab_invoice_line_item.setitem(li_lineitem_row,'quantity',ldec_units_billed)
END IF 

//1.87	The quantity to be paid must be equal to the one (1) for a billable item that is a supply and the billable unit code is not type ‘each’ .
IF ls_is_a_service_flag = 'N' AND ls_billable_unit_code <> 'EA' THEN 
	dw_rehab_invoice_line_item.setitem(li_lineitem_row,'quantity',1)
END IF 

// 1.92	The number of units must be equal to one (1) if the billable item is explicitly authorized and the billable unit code is type ‘each’ and the billable item is a service.
IF ls_is_a_service_flag = 'Y' AND ls_billable_unit_code = 'EA'  AND ls_explicit_authorization_flag = 'Y' THEN 
	dw_rehab_invoice_line_item.setitem(li_lineitem_row,'units_billed',1)
END IF 

// 1.160	The type of tax must be non-taxable if tax cannot be applied to the billable item (Billable_Goods_And_Services.taxable_flag = ‘N’).
// 1.170	The type of tax must be non-taxable if the provider is not set up to submit tax (PROVIDER.tax_flag= ‘Y’ ).
IF  inv_ephysio_invoice.nf_set_allow_tax( ll_recipient_no, ls_recipient_type_code, ll_provider_no_invoice, ls_provider_type_code, al_billable_xref_no) = TRUE THEN 
	dw_rehab_invoice_line_item.setitem(li_lineitem_row,'item_allows_tax','Y')
ELSE	
	dw_rehab_invoice_line_item.setitem(li_lineitem_row,'item_allows_tax','N')
	dw_rehab_invoice_line_item.setitem(li_lineitem_row,'tax_rate_no',0)
	dw_rehab_invoice_line_item.setitem(li_lineitem_row,'tax_rate', 0.00)
	dw_rehab_invoice_line_item.setitem(li_lineitem_row,'tax_applied_flag','N')
END IF 

// If the SERVICE provider is under the Contract for Fixed Fees, If the Billable Item is a Fixed Fee, Set the unit price from the Billable_Item_Fee table    
IF  ls_physio_contract_flag = 'Y'  AND  ls_fixed_fee_flag = 'Y'  THEN 
	dw_rehab_invoice_line_item.setitem(li_lineitem_row,'unit_price',ldec_unit_fee)
	dw_rehab_invoice_line_item.setitem(li_lineitem_row,'total_award_amount',ldec_unit_fee)	
	dw_rehab_invoice_line_item.setitem(li_lineitem_row,'scenario_number', 1)
ELSE
	dw_rehab_invoice_line_item.setitem(li_lineitem_row,'unit_price',0)
	dw_rehab_invoice_line_item.setitem(li_lineitem_row,'total_award_amount',0)	
	dw_rehab_invoice_line_item.setitem(li_lineitem_row,'scenario_number', 0)
END IF 

//dw_rehab_invoice_line_item.setitem(li_lineitem_row,'authorization_no',ll_authorization_no)		
dw_rehab_invoice_line_item.setitem(li_lineitem_row,'fixed_fee',ls_fixed_fee_flag)		
dw_rehab_invoice_line_item.setitem(li_lineitem_row,'billable_item_no', ll_billable_item_no)
dw_rehab_invoice_line_item.setitem(li_lineitem_row,'billable_unit_code', ls_billable_unit_code)	
dw_rehab_invoice_line_item.setitem(li_lineitem_row,'is_a_service_flag', ls_is_a_service_flag)
dw_rehab_invoice_line_item.setitem(li_lineitem_row,'fee_no',  ll_fee_no)	

IF inv_ephysio_invoice.nf_total_payment_amount_enterable( ll_submitted_by_no, ls_submitted_by_type_code, ll_recipient_no, ls_recipient_type_code) = TRUE THEN
	dw_rehab_invoice_line_item.setitem(li_lineitem_row,'allow_payable_amount_entry', 'Y')	
ELSE
	dw_rehab_invoice_line_item.setitem(li_lineitem_row,'allow_payable_amount_entry', 'N')	
END IF 

RETURN 1
end function

public subroutine wf_set_submitted_amount ();DECIMAL 	ldec_compute_total_amount
INTEGER	li_line_item_row

/* NOTE: This function also sets the total_award_amount */

/* Total submitted Amount for the billable item – default to [units * (tax amount + unit price)] but can be overridden by the payment clerk */
li_line_item_row 					= dw_rehab_invoice_line_item.getrow()
ldec_compute_total_amount 	= dw_rehab_invoice_line_item.getitemdecimal(li_line_item_row, 'compute_total_amount')

IF NOT ISNULL(ldec_compute_total_amount) THEN 
	dw_rehab_invoice_line_item.setitem(li_line_item_row,'submitted_amount', ldec_compute_total_amount)	
	
	/* Reimbursement Amount - defaults to Submitted amount but can be overridden if the amount to be reimbursed differs from the amount submitted */
	dw_rehab_invoice_line_item.setitem(li_line_item_row,'total_award_amount', ldec_compute_total_amount)	
END IF 

end subroutine

public subroutine wf_set_payment_method (long al_provider_no, string as_provider_type);INTEGER 	li_dw_reimburse_row
STRING		ls_bank_no, ls_bank_transit_no, ls_bank_account_no, ls_bank_name, ls_payment_method_code

// should only be 1
li_dw_reimburse_row = dw_reimburse.getrow()

inv_ephysio_invoice.nf_get_banking_info(al_provider_no, as_provider_type, ls_bank_no, ls_bank_transit_no, ls_bank_account_no, ls_bank_name) 
					
IF ISNULL(ls_bank_no) OR trim(ls_bank_no) = '' THEN
	ls_payment_method_code = 'A'					
ELSE
	ls_payment_method_code = 'D'				
END IF 

IF li_dw_reimburse_row > 0  THEN 
	dw_reimburse.setitem(li_dw_reimburse_row, 'payment_method_code', ls_payment_method_code)
END IF 
end subroutine

public function integer wf_get_address_info (long al_no, string as_provider_type);/*
SELECT 	txn_no, 								claim_no, 							payment_no,  
 				recipient_no, 						recipient_type_code,			'' bank_no,
				 '' bank_account_no,				'' bank_transit_no,				payment_method_code, 
				scheduled_processing_date, 	recipient_name, 				address_line1, 
				address_line2, 						city, 									prov_state_code,
 				country, 								postal_code, 						use_default_address_flag,
               '' AS reimburse_selector
FROM 	UNAPPLIED_CLAIM_TXN a
WHERE payment_no 	= :val_payment_no 
AND 		claim_no 		= :al_claim_no
*/
STRING		ls_last_name, ls_given_names, ls_address_line1, ls_address_line2, ls_city, ls_prov_state_code
STRING		ls_location_code, ls_location_type_code, ls_recipient_name, ls_country_code, ls_postal_code

CHOOSE CASE as_provider_type

	CASE 'I'
		
		SELECT  	last_name, 
						given_names, 
						address_line1, 
						address_line2, 
						city, 
						prov_state_code, 
						country_code, 
						postal_code, 
						location_code, 
						location_type_code
			INTO		:ls_last_name, 
						:ls_given_names, 
						:ls_address_line1, 
						:ls_address_line2, 
						:ls_city, 
						:ls_prov_state_code, 
						:ls_country_code, 
						:ls_postal_code, 
						:ls_location_code, 
						:ls_location_type_code 
			FROM 	INDIVIDUAL
			WHERE individual_no = :al_no
			USING 	SQLCA;
			SQLCA.nf_handle_error("w_physio_reimbursements", "wf_get_address_info()", "select * FROM INDIVIDUAL")
			
			ls_recipient_name = ls_given_names + ' ' + ls_last_name
			
	CASE ELSE
		
			SELECT name,                                     
						address_line1,                  
						address_line2,                 
						city,                      
						prov_state_code, 
						country_code, 
						postal_code, 
						location_code, 
						location_type_code
			INTO		:ls_last_name,                                     
						:ls_address_line1,                  
						:ls_address_line2,                 
						:ls_city,                      
						:ls_prov_state_code, 
						:ls_country_code, 
						:ls_postal_code, 
						:ls_location_code, 
						:ls_location_type_code
			FROM PROVIDER
			WHERE provider_no 				= :al_no
			AND		provider_type_code 	= :as_provider_type
			USING 	SQLCA;
			
			SQLCA.nf_handle_error("w_physio_reimbursement", "wf_get_address_info()", "SELECT name,")
			
			ls_recipient_name = ls_last_name
				
END CHOOSE

IF as_provider_type = '' OR al_no = 0 THEN 
	ls_address_line1 		= ''
	ls_address_line2 		= ''
	ls_city 						= ''
	ls_prov_state_code 	= ''
	ls_country_code 		= ''
	ls_postal_code 			= ''
END IF 

dw_reimburse.setitem(1, 'address_line1', ls_address_line1)
dw_reimburse.setitem(1, 'address_line2', ls_address_line2)
dw_reimburse.setitem(1, 'city', ls_city)
dw_reimburse.setitem(1, 'prov_state_code', ls_prov_state_code)
dw_reimburse.setitem(1, 'country', ls_country_code)
dw_reimburse.setitem(1, 'postal_code', ls_postal_code)

RETURN 1

end function

public function integer wf_refresh_screen (string as_status);STRING 		ls_service_provider_type_code,  ls_provider_type_temp,	ls_submitted_by_type_temp, ls_name
LONG			ll_docid, ll_service_provider_no, ll_task_no, ll_rehab_invoice_row_no, ll_rehab_invoice_no,	ll_provider_no_temp, ll_submitted_by_no_temp
INTEGER		li_rehab_invoice_row, li_rehab_invoice_line_item_rowcount, li_reimburse_row, li_reimburse_rowcount, li_selected_tab
DATE		 	ldt_current_date

/*
In the Rehab Invoice window:

For each Invoice that is being reimbursed, the user must enter:
•	Provider (type and number) that provided the physio service
•	Recipient (type and number) that submitted the invoice or receipt for reimbursement
•	Provider’s own invoice number (optional)

The claim that received the service or supply will be derived from the document indexing data for the image that is being viewed
(i.e. DOCUMENT_INDEX.claim_no).

The provider that provided the physio service may be derived from the document indexing data for the image that is being viewed, 
if this information was entered at the time that the document was indexed 
(i.e. DOCUMENT_INDEX.service_provider_type_code & DOCUMENT_INDEX.service_provider_no).

The recipient that submitted the invoice for reimbursement must be one of the following:
•	Injured Worker for the associated claim (i.e. CLAIM.individual_no)
•	Physio Clinic that provided the service  (i.e. DOCUMENT_INDEX. service_provider_type_code & DOCUMENT_INDEX.service_provider_no)

The user must also select a default for the payment recipient of the reimbursement:
1.	Reimburse the Provider  - all invoice line items are paid to the provider
2.	Reimburse the Injured Worker – all invoice line items are paid to the injured worker 
*/

ls_service_provider_type_code 	= istr_message.as_stringparm[1]
il_claim_no 								= istr_message.al_doubleparm[1]
ll_docid 										= istr_message.al_doubleparm[2]
ll_service_provider_no				= istr_message.al_doubleparm[3]
il_open_status							= istr_message.al_doubleparm[4] // (1) = Normal (2) = Delete Only -- for web based records

IF il_open_status = 2 THEN 
	ll_rehab_invoice_no 				=  istr_message.al_doubleparm[5]
	dw_rehab_invoice.enabled 	= FALSE
	dw_reimburse.enabled 			= FALSE
END IF 
	
/**************************************************************/
cb_ok.enabled = FALSE

// setup some default variables
ldt_current_date = date(f_server_datetime())

//set up the window to its initial state
wf_setup_window_initial()

//grab the individual_no for later use
il_individual_no =  inv_ephysio_invoice.nf_get_individual_no(il_claim_no)
IF isnull(il_individual_no) OR il_individual_no < 0 THEN il_individual_no = 0

/* grab the specific rehab invoice  */
IF il_open_status = 1 THEN 
	ll_rehab_invoice_no = inv_ephysio_invoice.nf_get_rehab_invoice_no_from_docid(ll_docid)
END IF 

/* REHAB_INVOICE*/
li_rehab_invoice_row = dw_rehab_invoice.retrieve(ll_rehab_invoice_no)
SQLCA.nf_handle_error("w_physio_reimbursement","wf_refresh_screen()","dw_rehab_invoice.retrieve(ll_rehab_invoice_no)") 
			
IF li_rehab_invoice_row = 1 THEN //SAVED INVOICE
	
	ll_submitted_by_no_temp		=  dw_rehab_invoice.getitemnumber( li_rehab_invoice_row, 'submitted_by_no')
	ls_submitted_by_type_temp 	= 	dw_rehab_invoice.getitemstring( li_rehab_invoice_row, 'submitted_by_type_code')
	ll_provider_no_temp				=  dw_rehab_invoice.getitemnumber( li_rehab_invoice_row, 'provider_no')
	ls_provider_type_temp 			= 	dw_rehab_invoice.getitemstring( li_rehab_invoice_row, 'provider_type_code')
			
	ls_name 	=  inv_ephysio_invoice.nf_get_provider_name(ll_submitted_by_no_temp, ls_submitted_by_type_temp)	
	dw_rehab_invoice.setitem(li_rehab_invoice_row,'submitted_by_name', ls_name) 
			
	ls_name 	=  inv_ephysio_invoice.nf_get_provider_name(ll_provider_no_temp, ls_provider_type_temp)	
	dw_rehab_invoice.setitem(li_rehab_invoice_row,'name', ls_name) 
	
ELSE 
	
	ll_rehab_invoice_row_no =  dw_rehab_invoice.insertrow(0)
	IF ISNULL(ll_rehab_invoice_row_no) OR ll_rehab_invoice_row_no < 0 THEN RETURN -1
	
	ll_provider_no_temp			= ll_service_provider_no
	ls_provider_type_temp		= ls_service_provider_type_code
	
	dw_rehab_invoice.setitem(ll_rehab_invoice_row_no, 'provider_type_code', ls_provider_type_temp)
	dw_rehab_invoice.setitem(ll_rehab_invoice_row_no, 'provider_no', ll_provider_no_temp)
	dw_rehab_invoice.setitem(ll_rehab_invoice_row_no, 'doc_id',  ll_docid)
		
	ls_name 	=  inv_ephysio_invoice.nf_get_provider_name(ll_service_provider_no, ls_service_provider_type_code)	
	dw_rehab_invoice.setitem(ll_rehab_invoice_row_no, 'name',  ls_name) 
		
END IF 

/* UNAPPLIED */
li_reimburse_rowcount = dw_reimburse.retrieve(ll_docid, il_claim_no)
SQLCA.nf_handle_error("w_physio_reimbursement","wf_refresh_screen()","li_reimburse_rowcount = dw_reimburse.retrieve(ll_docid)") 

IF 	li_reimburse_rowcount < 1 THEN 
	
	li_reimburse_row = dw_reimburse.insertrow(0)	
	
	// set some defaults 
	dw_reimburse.setitem(li_reimburse_row, 'scheduled_processing_date',  inv_ephysio_invoice.nf_get_thursday_processing_date(''))
			 
END IF 

/* REHAB_INVOICE_LINE_ITEM */
li_rehab_invoice_line_item_rowcount = dw_rehab_invoice_line_item.retrieve( il_claim_no, ll_rehab_invoice_no)
SQLCA.nf_handle_error("w_physio_reimbursement","wf_refresh_screen()"," dw_rehab_invoice_line_item.retrieve( il_claim_no, ll_rehab_invoice_no)") 

//set the redraw off
dw_rehab_invoice_line_item.setredraw(false)	

wf_setup_tab()

li_selected_tab = tab_line_item.selectedtab

// this function brings back all of the information we need for the tabpage
istr_info_returned_from_tab = tab_line_item.Control[li_selected_tab].DYNAMIC FUNCTION of_return_tab_info()

ll_task_no 									= istr_info_returned_from_tab.task_no

wf_retrieve_billable_item_dddw(ll_task_no, il_claim_no , ll_provider_no_temp, ls_provider_type_temp)

//set the redraw back on
dw_rehab_invoice_line_item.setredraw(true)

RETURN 1
end function

public function integer wf_setup_tab ();STRING 		 ls_program, ls_tabpage_header, ls_rehab_program_desc
LONG			 ll_task_no
INTEGER		 li_counter, li_upperbound_check, li_selected_tab
DATE			 ldt_planned_start_date

str_info_to_tab		str_info_to_tab

//grab the currently selected tab to be used later
li_selected_tab = tab_line_item.selectedtab

/* close the tab for a refresh, this code removes the tab objects - if any exist */
li_upperbound_check = upperbound( tab_line_item.Control[])

//open a tabpage for each of the authorizations  - this will have to be modified once I fiqure out what to do with saved ones
IF ISNULL(li_upperbound_check) or li_upperbound_check = 0 THEN 
	FOR li_counter = 1 TO ids_authorizations_for_tabs.rowcount()
		
		ls_rehab_program_desc  	= ids_authorizations_for_tabs.getitemstring(li_counter, 'rehab_program_desc_e')
		ls_program 						= ids_authorizations_for_tabs.getitemstring(li_counter, 'rehab_program_code')
		ldt_planned_start_date 		= date(ids_authorizations_for_tabs.getitemdatetime(li_counter,'planned_start_date' ))
		ll_task_no 							= ids_authorizations_for_tabs.getitemnumber(li_counter, 'task_no')
		ls_tabpage_header 			=  string(ll_task_no) + ' - ' + ls_rehab_program_desc + ' - ' + string(ldt_planned_start_date,'YYYY-MM-DD') 
	
		str_info_to_tab.tab_text 					= ls_tabpage_header
		str_info_to_tab.task_no 					= ll_task_no
		str_info_to_tab.planned_start_date	= ldt_planned_start_date
		str_info_to_tab.claim_no					= il_claim_no
		
		tab_line_item.opentabwithparm(uo_tab_insert_physio, str_info_to_tab,0)
		
	NEXT
END IF 

//SET TO THE PREVIOUSLY SELECTED TAB 
IF li_selected_tab < 0  THEN li_selected_tab = 1

tab_line_item.selecttab(li_selected_tab)

IF li_selected_tab > 0 THEN
	 tab_line_item.Control[li_selected_tab].TabBackColor = RGB(0, 255, 255)
END IF 

RETURN 1
end function

public subroutine wf_setup_window_initial ();INTEGER				li_authorization_count

// move this into its own function or something
ids_authorizations_for_tabs 					= CREATE U_DS
ids_authorizations_for_tabs.DataObject 	= 'ds_authorizations_for_tabs'
ids_authorizations_for_tabs.SetTransObject(SQLCA)

/* if no authorizations they cant really do anything unless there is something already saved in the lineitems - will have to worry about that later */
li_authorization_count = ids_authorizations_for_tabs.retrieve(il_claim_no)

IF isnull(li_authorization_count) OR li_authorization_count < 1 THEN 
	messagebox('No Rehab Authorizations', 'No Rehab Authorizations are currently available.' )
END IF 

end subroutine

public function integer wf_check_buisness_rules ();INTEGER 				li_counter, li_line_item_rowcount, li_scenario_number
DATE					ldt_service_date, ldt_current_date
DATETIME				ldtm_rt_planned_start_date, ldtm_rt_actual_completed_date, ldtm_scheduled_processing_date
LONG					ll_rehab_invoice_no, ll_line_no, ll_claim_no, ll_task_no, ll_billable_xref_no, ll_recipient_no, ll_submitted_by_no, ll_provider_no_invoice
LONG					ll_tax_rate_no, ll_provider_no, ll_tax_rate_no_check, ll_billable_item_no, ll_authorization_no
STRING					ls_billable_unit_code, ls_tax_applied_flag, ls_taxable_flag, ls_address_line1, ls_address_line2, ls_city, ls_country_code
STRING					ls_fixed_fee, ls_rt_task_status_code, ls_is_a_goods_flag, ls_is_a_service_flag, ls_admin_region_code
STRING					ls_provider_taxable_flag, ls_provider_type, ls_prov_state_code, ls_external_invoice_id, ls_billable_item_desc_e, ls_ephysio_flag
STRING					ls_payment_method_code, ls_recipient_type_code, ls_submitted_by_type_code, ls_provider_type_code, ls_allow_payable_amount_entry 
STRING					ls_bank_no, ls_bank_transit_no, ls_bank_account_no, ls_bank_name
DECIMAL	 {2}		ldec_quantity, ldec_units_billed, ldec_unit_price, ldec_total_non_tax_amount,  ldec_total_amount, ldec_tax_rate, ldec_submitted_amount
DECIMAL	 {2}		ldec_reimburse_amount, ldec_total_tax_amount, ldec_max_units_on_entry,   ldec_rta_authorized_amount, ldec_rta_paid_amount
DECIMAL 	 {2} 		ldec_check,  ldec_rta_authorized_quantity, ldec_rta_paid_quantity,  ldec_saved_line_item_amount, ldec_saved_line_item_quantity , ldec_total_award_amount
BOOLEAN				lb_check

// variables for loop section
INTEGER				 li_temp_counter, li_temp_line_no
LONG					 ll_temp_billable_xref_no, ll_temp_billable_item_no, ll_temp_task_no, ll_temp_authorization_no
DECIMAL {2}  		 ldec_sum_amount_loop, ldec_sum_units_billed_loop
DATE					 ldt_temp_service_date

dwItemStatus		 l_dw_rehab_invoice_status_row, l_status_row, ldwis_rowstatus
	
/*
Terms and Facts											
Authorized Amount					>> REHAB_TASK_AUTHORIZATION.authorized_amount –  the authorized amount is either set to the maximum authorized amount allowed for the billable item 
														(if not entered by the Claim Manager) or is entered by the Claim Manager. The authorized amount cannot be entered by the Claim Manager 
														for a billable item that is based on a fixed rate fee. The authorized amount includes the tax amount if the billable item is to be taxed (i.e. it is the quantity * (amount + tax) )
Remaining Authorized Quantity	>> REHAB_TASK_AUTHORIZATION.authorized_quantity – REHAB_TASK_AUTHORIZATION paid_quantity  
Remaining Authorized Amount	>> REHAB_TASK_AUTHORIZATION.authorized_amount – REHAB_TASK_AUTHORIZATION.paid_amount
Explicit Authorization					>> An authorization for a billable item must exist under the Rehab Task in order for the clinic to invoice the billable item 
														(i.e. Rehab Task Authorization record must exist for the billable item)
Implicit Authorization					>> The billable item is implied to be authorized without having to pre-authorize the item (i.e. the Rehab Task Authorization record doesn’t have to exist
														when the clinic submits the billable item). All billable items that can be implicitly authorized must be based on a fixed rate fee.
Unit Price									>> The price of the billable item; this is either entered by the Clinic or is derived from the fee schedule for a billable item that is not based on a fixed rate fee. 
														The unit price does not include the taxes.
Course of Treatment					>>	The course of treatment for a claim is the REHAB_TASK for the claim
Voided Invoice Line Item 			>> This is a REHAB_INVOICE_LINE_ITEM for a billable item that was billed by the clinic and the payment to the clinic was processed. 
                                            			Following this, the payment for that invoice line item was adjusted by WorkSafeNB through a cancellation of the cheque or direct deposit or it was adjusted 
											   			for an overpayment recovery. The REHAB_INVOICE_LINE_ITEM.line_item_amended_code indicates if the line item was voided.
*/

/* DO AN ACCEPTTEXT ON EVERYTHING */
dw_rehab_invoice_line_item.triggerevent('itemchanged')

dw_rehab_invoice.ACCEPTTEXT()
dw_rehab_invoice_line_item.ACCEPTTEXT()
dw_reimburse.ACCEPTTEXT()

li_line_item_rowcount = dw_rehab_invoice_line_item.rowcount()
IF isnull(li_line_item_rowcount) OR li_line_item_rowcount < 0 THEN RETURN -1

// make sure the user has some authorizations
ls_admin_region_code =  inv_ephysio_invoice.nf_get_claim_admin_region_code(il_claim_no)
IF gnv_user_authorizations.nf_authorizations_exist(ls_admin_region_code, 'act') = FALSE THEN
	MessageBox('Warning',"You don't have the proper Authorization authority for this claims region." )
	RETURN -1
END IF

/* set some defaults */
ldt_current_date = DATE(f_server_datetime())

/* dw_reimburse */
ldtm_scheduled_processing_date 	= dw_reimburse.getitemdatetime(1, 'scheduled_processing_date')
ls_payment_method_code 			= dw_reimburse.getitemstring(1, 'payment_method_code')
ll_recipient_no 								= dw_reimburse.getitemnumber(1, 'recipient_no')
ls_recipient_type_code 					= dw_reimburse.getitemstring(1, 'recipient_type_code')
ls_address_line1 							= dw_reimburse.GetItemString(1, 'address_line1')
ls_address_line2 							= dw_reimburse.GetItemString(1, 'address_line2')
ls_city 											= dw_reimburse.GetItemString(1,'city')
ls_country_code 							= dw_reimburse.GetItemString(1,'country')
ls_prov_state_code 						= dw_reimburse.getitemstring(1,'prov_state_code')
l_status_row 									=	dw_reimburse.GetItemStatus(1,0, Primary!)


/* do a basic swich if applicable  for check ck_UNAPPLIED_CLAIM_TXN_18 */
IF TRIM(ls_address_line1) = "" and len(trim(ls_address_line2)) > 0 THEN
	dw_reimburse.SetItem(1,"address_line1",ls_address_line2)
	dw_reimburse.SetItem(1,"address_line2","")
END IF

/* Check the address this is only applicable under certain conditions -- INDIVIDUAL */
IF ls_recipient_type_code = 'I' THEN 
	IF inv_ephysio_invoice.nf_check_address(ls_address_line1, ls_address_line2, ls_city, ls_country_code )  < 0  THEN RETURN  -1
END IF 

/* In order to reimburse a provider through this module, the provider must not be a provider that is set up to use the Physio website. 
   Otherwise, the invoice must be returned to the provider and the provider directed to submit the invoice line items through the website.
	TURN THIS CHECK BACK ON 
*/
lb_check = inv_ephysio_invoice.nf_provider_setup_on_website(ll_recipient_no, ls_recipient_type_code)
IF lb_check = TRUE THEN 
	messagebox('E-Physio Provider', 'In order to reimburse a provider through this module, the provider must not be a provider that is set up to use the Physio website' )
	RETURN -1	
END IF 

/*
19.180 Payments must not be made to recipients who are set up to receive automated payments, such as WRC and ABCC. 
(A list of recipients who receive automated payments can be found in the App_Document_Index_Parameter table)  
*/
IF  inv_ephysio_invoice.	nf_recipient_eligible_for_automated_pay(ll_recipient_no, ls_recipient_type_code) = FALSE THEN
	MessageBox("Validation Error","This recipient is not eligible for payments",Exclamation!)
	RETURN -1
END IF 

/* payment_method_code payment_method_desc               
------------------- ----------------------------------
A                   Automated Cheque                  
C                   Cash                              
D                   Direct Deposit                    
H                   Hand Written Cheque               
I                   Inapplicable                      
R                   Receiving Salary - Not Issued     
*/

/* Integrity check on banking info */
CHOOSE CASE ls_payment_method_code
	CASE 'A'	
					
	CASE 'D'
		IF  inv_ephysio_invoice.nf_get_banking_info(ll_recipient_no, ls_recipient_type_code, ls_bank_no, ls_bank_transit_no, ls_bank_account_no, ls_bank_name) = -1 THEN 
			MessageBox("Validation Error","You have selected Direct deposit but there is no Banking information",Exclamation!)
			RETURN -1
		END IF 
					
		IF ISNULL(ls_bank_no) OR trim(ls_bank_no) = '' THEN
			MessageBox("Validation Error","You have selected Direct deposit but there is no Banking information",Exclamation!)
			RETURN -1
		END IF
END CHOOSE

IF l_status_row <> NOTMODIFIED! THEN
	IF (DATE(ldtm_scheduled_processing_date) < ldt_current_date) THEN
		MessageBox("Validation Error","Scheduled processing date cannot be less than Today",Exclamation!)
		RETURN -1
	END IF
	
	IF inv_ephysio_invoice.nf_check_valid_info(ll_recipient_no, ls_recipient_type_code, 2) = FALSE THEN 
		MessageBox("Validation Error","Invalid Recipient",Exclamation!)
		RETURN -1
	END IF
END IF 

/* dw_rehab_invoice */
ll_provider_no								= dw_rehab_invoice.getitemnumber(1,'provider_no')
ls_provider_type 							= dw_rehab_invoice.getitemstring(1,'provider_type_code')
ls_external_invoice_id 					= dw_rehab_invoice.getitemstring(1, 'external_invoice_id')
ll_submitted_by_no   						= dw_rehab_invoice.getitemnumber(1, 'submitted_by_no')
ls_submitted_by_type_code 			= dw_rehab_invoice.getitemstring(1, 'submitted_by_type_code')
ll_provider_no_invoice    				= dw_rehab_invoice.getitemnumber(1, 'provider_no')
ls_provider_type_code 					= dw_rehab_invoice.getitemstring(1, 'provider_type_code')
l_dw_rehab_invoice_status_row 	=	dw_rehab_invoice.GetItemStatus(1,0, Primary!)

IF len(ls_external_invoice_id) > 20 THEN
	 messagebox('Delete Error', 'The External Invoice Id can only be 20 chars in Len.') 
	 RETURN -1
END IF 
		
IF l_dw_rehab_invoice_status_row <> NOTMODIFIED! THEN
	
	//trim it
	dw_rehab_invoice.setitem(1, 'external_invoice_id'  , TRIM(ls_external_invoice_id)) //fix for check constraint.
	
	IF ISNULL(ls_external_invoice_id) THEN 
		dw_rehab_invoice.setitem(1, 'external_invoice_id','')
	END IF 
	
	IF inv_ephysio_invoice.nf_check_valid_info(ll_submitted_by_no, ls_submitted_by_type_code, 2) = FALSE THEN 
		MessageBox("Validation Error","Invalid Submitted By",Exclamation!)
		RETURN -1
	END IF
	
	IF inv_ephysio_invoice.nf_check_valid_info(ll_provider_no_invoice, ls_provider_type_code, 1) = FALSE THEN 
		MessageBox("Validation Error","Invalid Provider",Exclamation!)
		RETURN -1
	END IF
END IF 

//grab the ephysio flag for the provider
ls_ephysio_flag = inv_ephysio_invoice.nf_get_ephysio_flag(ll_recipient_no, ls_recipient_type_code)

IF ls_ephysio_flag = 'Y' THEN 
	 messagebox('Delete Error', 'The Recipient is set up to use the e-physio. The Invoice must be submitted through that.') 
	 RETURN -1
END IF 

FOR li_counter = 1 TO li_line_item_rowcount
	
		/* DON'T CONTINUE IF THE line_no > 0 */
		ll_line_no 									= dw_rehab_invoice_line_item.getitemnumber(li_counter,'line_no')
		IF ll_line_no > 0 THEN CONTINUE// ANYTHING GREATER THAN 0 HAS ALREADY BEEN SAVED
		
		ldt_service_date 							= date(dw_rehab_invoice_line_item.getitemdatetime(li_counter,'service_date'))
		ll_rehab_invoice_no 						= dw_rehab_invoice_line_item.getitemnumber(li_counter,'rehab_invoice_no')
		ll_claim_no 									= dw_rehab_invoice_line_item.getitemnumber(li_counter,'claim_no')
		ll_task_no 										= dw_rehab_invoice_line_item.getitemnumber(li_counter,'task_no')
		ll_billable_xref_no 							= dw_rehab_invoice_line_item.getitemnumber(li_counter,'billable_xref_no')
		ll_tax_rate_no 								= dw_rehab_invoice_line_item.getitemnumber(li_counter,'tax_rate_no')
		ls_billable_unit_code 						= dw_rehab_invoice_line_item.getitemstring(li_counter,'billable_unit_code')
		ls_tax_applied_flag 						= dw_rehab_invoice_line_item.getitemstring(li_counter,'tax_applied_flag')
		ls_fixed_fee 									= dw_rehab_invoice_line_item.getitemstring(li_counter,'fixed_fee')
		ldec_quantity 								= dw_rehab_invoice_line_item.getitemdecimal(li_counter,'quantity')
		ldec_units_billed 							= dw_rehab_invoice_line_item.getitemdecimal(li_counter,'units_billed')
		ldec_unit_price 								= dw_rehab_invoice_line_item.getitemdecimal(li_counter,'unit_price')
		ldec_total_non_tax_amount 			= dw_rehab_invoice_line_item.getitemdecimal(li_counter,'total_non_tax_amount')
		ldec_total_amount 						= dw_rehab_invoice_line_item.getitemdecimal(li_counter,'compute_total_amount')
		ldec_tax_rate 								= dw_rehab_invoice_line_item.getitemdecimal(li_counter,'tax_rate')
		ldec_submitted_amount 				= dw_rehab_invoice_line_item.getitemdecimal(li_counter,'submitted_amount')
		ldec_reimburse_amount 				= dw_rehab_invoice_line_item.getitemdecimal(li_counter,'total_award_amount')
		ldec_total_tax_amount 					= dw_rehab_invoice_line_item.getitemdecimal(li_counter,'compute_total_tax')
		ll_billable_item_no   						= dw_rehab_invoice_line_item.getitemnumber(li_counter,'billable_item_no')
		ll_authorization_no							= dw_rehab_invoice_line_item.getitemnumber(li_counter,'authorization_no')
		ls_allow_payable_amount_entry  	= dw_rehab_invoice_line_item.getitemstring(li_counter,'allow_payable_amount_entry')
		li_scenario_number                       = dw_rehab_invoice_line_item.getitemnumber(li_counter,'scenario_number')
		ldec_total_award_amount 				= dw_rehab_invoice_line_item.getitemnumber( li_counter, 'total_award_amount')	
	
		/* CHECK FOR SOME NULL VALUES */
		IF  ISNULL(ldec_total_amount) 			THEN ldec_total_amount 			= 0 
		IF  ISNULL(ldec_quantity)  					THEN ldec_quantity 					= 0 
		IF  ISNULL(ldec_unit_price) 				THEN ldec_unit_price 				= 0 
		IF  ISNULL(ldec_total_tax_amount) 	THEN ldec_total_tax_amount 	= 0 
		IF  ISNULL(ldec_submitted_amount)	THEN ldec_submitted_amount 	= 0 
		IF  ISNULL(ldec_reimburse_amount)	THEN ldec_reimburse_amount 	= 0 
		IF  ISNULL(ll_tax_rate_no)                THEN ll_tax_rate_no 					= 0 //DEFAULTED IN THE DATAWINDOW
		IF	 ISNULL(ldec_tax_rate) 					THEN ldec_tax_rate 					= 0.00 //DEFAULTED IN THE DATAWINDOW
		IF 	 ISNULL(ls_tax_applied_flag) 			THEN ls_tax_applied_flag			= 'N' //DEFAULTED IN THE DATAWINDOW
		
		// grab some basic information 
		SELECT 	a.is_a_service_flag , a.is_a_goods_flag, a.taxable_flag, a.billable_item_desc_e 
		INTO 		:ls_is_a_service_flag, :ls_is_a_goods_flag, :ls_taxable_flag, :ls_billable_item_desc_e 
		FROM 		Billable_Item a join Billable_Item_Rehab_Task_Xref b ON a.billable_item_no = b.billable_item_no
		WHERE 	billable_xref_no = :ll_billable_xref_no
		USING  		SQLCA;
		SQLCA.nf_handle_error('w_physio_reimbursements','wf_check_business_rules()','	SELECT a.is_a_goods_flag')
		
		SELECT 	authorized_quantity,  paid_quantity , authorized_amount, paid_amount
		INTO 		:ldec_rta_authorized_quantity, :ldec_rta_paid_quantity, :ldec_rta_authorized_amount, :ldec_rta_paid_amount
		FROM  		REHAB_TASK_AUTHORIZATION 
		WHERE		authorization_no  	= :ll_authorization_no
		USING 		SQLCA;
		SQLCA.nf_handle_error('w_physio_reimbursements','wf_check_business_rules()','	SELECT authorized_quantity,  paid_quantity ')
		
		IF ISNULL(ldec_rta_authorized_quantity) 	THEN 	ldec_rta_authorized_quantity 		= 0
		IF ISNULL(ldec_rta_paid_quantity) 				THEN 	ldec_rta_paid_quantity 				= 0
		IF ISNULL(ldec_rta_authorized_amount) 		THEN 	ldec_rta_authorized_amount 		= 0
		IF ISNULL(ldec_rta_paid_amount) 				THEN 	ldec_rta_paid_amount 				= 0
	
		/*******************************     start of checks  **************************************************/
		IF ls_allow_payable_amount_entry = 'N' AND (ldec_reimburse_amount <> ldec_total_amount) THEN
			messagebox("Validation Error", 'The total award amount (payable amount) must be equal to the total amount. There is a problem with the defaulted payable amount.' +&
			                  '~r If tax is applied try reapplying the tax to recalculate the amount.')
			dw_rehab_invoice_line_item.scrolltorow(li_counter)
			RETURN -1
		END IF 
		
		//this is for the czse of 0 billings ala missed appointments - originally we were not going to allow this now...apparently we are.
		IF ISNULL(ldec_units_billed) AND ls_billable_unit_code = 'EA' THEN
			dw_rehab_invoice_line_item.setitem(li_counter,'quantity', 1) 
			dw_rehab_invoice_line_item.setitem(li_counter,'units_billed', 1)
			ldec_quantity 		= 1
			ldec_units_billed 	= 1
		END IF 
		
		/* total award amount can't be less than 0 
			NOTE: (WG)  Yes on some items.  I think these items have a zero amount in the billable items table.
					They need to be able to record the treatment - they get paid under a lump sum assessment.
					
			5.17	The ‘’Amt Paid:’ must be > 0 if the payment is not for an electronic physiotherapy invoice.  (Total payable amount)		
			from account payment - a general rule on payment details.

			You should be able to put in physio treatment of $0.   The shoulder program has physio where the amount is $0.		
		*/
		IF ldec_reimburse_amount < 0 THEN //total_award_amount
			messagebox("Validation Error", 'The total award amount (payable amount) must be greater than 0')
			dw_rehab_invoice_line_item.scrolltorow(li_counter)
			RETURN -1
		END IF 
		
		/* 1.225	IF the submitted amount is not $0 then the payment amount must be greater then $0
 		(if the submitted amount = 0 the payment amount can be 0)

			PRINT 'Adding       ck_PAYMENT_18'
			CHECK           (    ( submitted_amount >  0 and  total_award_amount = 0 and processed_date is  null ) -- WRC Invoices
			 OR    ( submitted_amount >= 0 and  total_award_amount > 0 )  -- submitted on processed must have positive award amt
			 OR    ( submitted_amount  = 0 and  total_award_amount < 0 )   -- negative award amounts don't have submitted
			 OR    ( submitted_amount  = 0 and  total_award_amount = 0 )
			 OR    ( submitted_amount  > 0 and  total_award_amount = 0 and payment_no in (125489,173802,120327,108975,79207,108999,81706,116669) ) )   
			 
			 We should add a new BR that prevents the payment amount from being $0 if the submitted amount is > $0
			 
			 	ids_payment_insert.SetItem(li_payment_insert_row, "total_award_amount", ldec_reimbursement_amount) //REHAB_INVOICE_LINE_ITEM.??  -- 'total_award_amount
				ids_payment_insert.SetItem(li_payment_insert_row, "submitted_amount", ldec_submitted_amount) //REHAB_INVOICE_LINE_ITEM. --'submitted_amount
		*/
		IF ldec_submitted_amount > 0 AND ldec_total_award_amount = 0 THEN 
			messagebox("Validation Error", 'The Total Payment Amount must be greater than $0.00 if the Submitted Amount is greater than $0.00')
			dw_rehab_invoice_line_item.scrolltorow(li_counter)
			RETURN -1
		END IF 
		
		/* submitted_amount can't be less than 0 
		NOTE: (WG)  Yes on some items.  I think these items have a zero amount in the billable items table.
					They need to be able to record the treatment - they get paid under a lump sum assessment.
		*/
		IF ldec_submitted_amount < 0 THEN //submitted_amount
			messagebox("Validation Error", 'The submitted amount must be greater than 0')
			dw_rehab_invoice_line_item.scrolltorow(li_counter)
			RETURN -1
		END IF 

		IF ls_fixed_fee = 'N' THEN //we need to set some defaults - no record in fee table - this should have been taken care of elsewhere
			ls_billable_unit_code = 'EA'	
			dw_rehab_invoice_line_item.setitem(li_counter,'billable_unit_code', ls_billable_unit_code)
		END IF
		
		/* set some defaults --The quantity to be paid must be equal to the number of units for a billable item that is a supply and the billable unit code is type ‘each’ . */
		IF ldec_quantity <> ldec_units_billed AND ls_is_a_goods_flag = 'Y' AND ls_billable_unit_code = 'EA' THEN 
			 ldec_quantity = ldec_units_billed
			 dw_rehab_invoice_line_item.setitem(li_counter,'quantity', ldec_quantity)
		END IF 
		
		/* The quantity to be paid must be equal to the one (1) for a billable item that is a supply and the billable unit code is not type ‘each’  (1.87). */
		IF ldec_quantity <> 1 AND ls_is_a_goods_flag = 'Y' AND ls_billable_unit_code <> 'EA' THEN 
			 ldec_quantity = 1
			 dw_rehab_invoice_line_item.setitem(li_counter,'quantity', ldec_quantity)
		END IF 
		
		/* 1.10 	The date of service must be entered. */
		IF IsNull(ldt_service_date) OR ldt_service_date > Date('2079-06-06') OR ldt_service_date <= Date('1900-01-01') THEN 
		 	messagebox("Validation Error", 'The service date is required.')
			dw_rehab_invoice_line_item.scrolltorow(li_counter)
			RETURN -1
		END IF 
		
		/* 1.20	The date of service must be greater than or equal to the Rehab Task planned start date. */
		SELECT planned_start_date, actual_completion_date, task_status_code
		INTO		:ldtm_rt_planned_start_date, :ldtm_rt_actual_completed_date, :ls_rt_task_status_code
		FROM 	REHAB_TASK 
 		WHERE task_no 			= :ll_task_no
		AND 		claim_no 			= :ll_claim_no
		USING 	SQLCA;
		SQLCA.nf_handle_error('w_physio_reimbursements','wf_check_business_rules()','SELECT planned_start_date')
		
		IF IsNull(ldtm_rt_planned_start_date) OR STRING(ldtm_rt_planned_start_date,'YYYY-MM-DD') = '1900-01-01'  OR DATE(ldtm_rt_planned_start_date) > Date('2079-06-06') OR DATE(ldtm_rt_planned_start_date) < Date('1900-01-01') THEN 
		 	messagebox("Validation Error", 'The Rehab Task planned start date must be a valid date. Please check the Rehab Task record')
			dw_rehab_invoice_line_item.scrolltorow(li_counter)
			RETURN -1
		END IF 
		
		IF DATE(ldtm_rt_planned_start_date) > ldt_service_date THEN 
			messagebox("Validation Error", 'The service date must be on or after the start date of the treatment.')
			dw_rehab_invoice_line_item.scrolltorow(li_counter)
			RETURN -1	
		END IF 

		/* 1.30	The date of service must not be in the future. */
		IF DATE(ldt_service_date) > ldt_current_date THEN 
			messagebox("Validation Error", 'The service date must not be in the future.')
			dw_rehab_invoice_line_item.scrolltorow(li_counter)
			RETURN -1	
		END IF 
		
		/* 1.35		The date of service must be less than or equal to the Rehab Task actual completed date, if the task is closed.  -- 04  closed  */
		IF ls_rt_task_status_code = '04' THEN
			IF ldt_service_date > DATE(ldtm_rt_actual_completed_date) THEN
				messagebox("Validation Error", 'The date of service must be less than or equal to the Rehab Task actual completed date, if the task is closed.')
				dw_rehab_invoice_line_item.scrolltorow(li_counter)
				RETURN -1	
			END IF 
		END IF 

		/* 1.40		The billable item must be entered. */
		IF ISNULL(ll_billable_xref_no) OR ll_billable_xref_no <= 0 THEN 
			messagebox("Validation Error", 'The billable item is required and could not be determined. Check authorized dates for the item')
			dw_rehab_invoice_line_item.scrolltorow(li_counter)
			RETURN -1
		END IF 
		
		IF ISNULL(ll_billable_item_no) OR ll_billable_item_no <= 0 THEN 
			messagebox("Validation Error", 'The billable item is required and could not be determined. Check authorized dates for the item')
			dw_rehab_invoice_line_item.scrolltorow(li_counter)
			RETURN -1
		END IF

		/* 1.50		There must be an explicit authorization for the billable item under the rehab task or the billable item must be implicitly authorized for the rehab task 
               (i.e. there must be a Rehab Task Authorization under the Rehab Task for the billable item or the implicit flag must be Yes for the billable item in the 
				cross-reference table for the billable items allowed under the task types) 	
				
				NOT A RULE FOR CMWB JP
		*/
		
		/* 1.60 	The number of units must be entered. */
		/* 1.70 	The number of units must be greater than zero. */
		IF ISNULL(ldec_units_billed) OR ldec_units_billed < 1 THEN
			messagebox("Validation Error", 'The number of units is required and must be greater than 0')
			dw_rehab_invoice_line_item.scrolltorow(li_counter)
			RETURN -1
		END IF 
		
		/* 1.75 	The number of units must be an integer value. -- handled by the datawindow anyway*/
		
		/* 1.80	The quantity to be paid must be equal to one (1) for a billable item that is a service. */
		IF  ls_is_a_service_flag = 'Y' AND ldec_quantity <> 1 THEN 
			messagebox("Validation Error", 'The quantity to be paid must be equal to one (1) for a billable item that is a service.')
			dw_rehab_invoice_line_item.scrolltorow(li_counter)
			RETURN -1	
		END IF 
		
		/* 1.85	The quantity to be paid must be equal to the number of units for a billable item that is a supply and the billable unit code is type ‘each’ . */
		IF ldec_quantity <> ldec_units_billed AND ls_is_a_goods_flag = 'Y' AND ls_billable_unit_code = 'EA' THEN 
			messagebox("Validation Error", 'The quantity to be paid must be equal to the number of units for a billable item that is a supply and the billable unit code is type ‘each’.')
			dw_rehab_invoice_line_item.scrolltorow(li_counter)
			RETURN -1	
		END IF 
		
		/* 1.87	The quantity to be paid must be equal to (1) for a billable item that is a supply and the billable unit code is not type ‘each’ . */
		IF ldec_quantity <> 1 AND ls_is_a_goods_flag = 'Y' AND ls_billable_unit_code <> 'EA' THEN 
			messagebox("Validation Error", 'The quantity to be paid must be equal to one (1) for a billable item that is a supply and the billable unit code is not type ‘each’.')
			dw_rehab_invoice_line_item.scrolltorow(li_counter)
			RETURN -1	
		END IF 
		
		/* 1.90  The number of units must be less than or equal to default authorized quantity for the billable item if the billable item is a supply
		             and the billable item is implicitly authorized (less than or equal to Billable_Item_Rehab_Task_Xref.default_authorized_qty if Billable_Item.Is_A_Goods_Flag = ‘Y’ 
					  and Billable_Item_Rehab_Task_Xref.explicit_authorization_flag = ‘N’ ). 
					   // REMOVED NOT APPLICABLE - IMPLICIT
		*/
			
		/* 1.92	The number of units must be equal to one (1) if the billable item is explicitly authorized and the billable unit code is type ‘each’ and the billable item is a service. */
		 IF ls_billable_unit_code = 'EA' AND ls_is_a_service_flag = 'Y' AND ldec_units_billed <> 1 THEN
			messagebox("Validation Error", 'The number of units must be equal to one (1) if the billable item is explicitly authorized and the billable unit code is type ‘each’ and the billable item is a service.')
			dw_rehab_invoice_line_item.scrolltorow(li_counter)
			RETURN -1	
		 END IF 
				
		/* 1.97 	The number of units must be less than or equal to the maximum units per line item for the billable unit code if the billable item is explicitly authorized 
		                and the billable unit code is not of type ‘each’. */
		SELECT max_units_on_entry
		INTO 	:ldec_max_units_on_entry
		FROM  	Billable_Unit 
		WHERE billable_unit_code 			= :ls_billable_unit_code 
		USING 	SQLCA;
		SQLCA.nf_handle_error('w_physio_reimbursements','wf_check_business_rules()','	SELECT max_units_on_entry')
			
		IF ls_billable_unit_code <> 'EA'  AND (ldec_units_billed > ldec_max_units_on_entry) THEN
				messagebox("Validation Error", 'You have exceeded the number of units that you can place on a line item. In this case the max you can use is ' + string(ldec_max_units_on_entry) )
				dw_rehab_invoice_line_item.scrolltorow(li_counter)
				RETURN -1	
		END IF 	
		
		/* DONE IN SAVE    *************************************************************************
		 1.100	The Remaining Authorized Quantity must be calculated as the 
		               REHAB_TASK_AUTHORIZATION.authorized_quantity – REHAB_TASK_AUTHORIZATION.paid_quantity] 
		 */
		 	 
		/* 1.140	The number of units must be less than the maximum billable unit code limit if the billable unit code is not of type ‘each’ (i.e Billable_Units.max_units_on_entry). */
		IF  ls_billable_unit_code <> 'EA' AND ldec_units_billed >  ldec_max_units_on_entry THEN
			messagebox("Validation Error", 'The number of units for  ' + ls_billable_item_desc_e  + ' exceeds the maximum allowed for that item.')
			dw_rehab_invoice_line_item.scrolltorow(li_counter)
			RETURN -1				
		END IF 
			
		/* 1.150	The unit price must be greater than zero.
		   revised: 1.150	The unit price must be greater than zero if the user is permitted to enter the amount.
			IF(line_no > 0 or  scenario_number = 1 , 1,0) locked
			This is the rule from the common BR for physio invoices.

			basically - if the user is allowed to enter the value of the unit price 
			(without being set from the billable items table) then the value has to be greater than zero.
			
		*/
		IF ( (ldec_unit_price <= 0   OR ISNULL(ldec_unit_price)) AND li_scenario_number = 0) THEN
			messagebox("Validation Error", 'The unit price must be greater than 0.')
			dw_rehab_invoice_line_item.scrolltorow(li_counter)
			RETURN -1			
		END IF 
		
		/* 1.160	The type of tax must be non-taxable if tax cannot be applied to the billable item (Billable_Item.taxable_flag = ‘N’). */
		IF ll_tax_rate_no > 0 AND ls_taxable_flag = 'N' THEN
			messagebox("Validation Error", 'The type of tax must be non-taxable if tax cannot be applied to the billable item.')
			dw_rehab_invoice_line_item.scrolltorow(li_counter)
			RETURN -1	
		END IF 
		
		/* 1.170	The type of tax must be non-taxable if the provider is not set up to submit tax (PROVIDER.tax_flag= ‘Y’ ). */
		SELECT 	tax_flag
		INTO 		:ls_provider_taxable_flag
		FROM 		PROVIDER
		WHERE 	provider_no 				= :ll_provider_no
		AND 			provider_type_code 	= :ls_provider_type
		USING 		SQLCA;
		SQLCA.nf_handle_error('w_physio_reimbursements','wf_check_business_rules()','	SELECT tax_flag INTO :ls_provider_taxable_flag')
				
		IF ll_tax_rate_no > 0 AND ls_provider_taxable_flag <> 'Y' THEN
			messagebox("Validation Error", 'The type of tax must be non-taxable if the provider is not set up to submit tax ')
			dw_rehab_invoice_line_item.scrolltorow(li_counter)
			RETURN -1	
		END IF

		/* 1.180	The type of tax must be one of the following if the billable item can have tax applied to it: GST, PST, HST. */
		IF ll_tax_rate_no = 0 AND ls_taxable_flag = 'Y' AND ls_tax_applied_flag = 'Y' THEN
			messagebox("Validation Error", 'A tax rate could not be determined for the Providers Location: ' + ls_prov_state_code)
			dw_rehab_invoice_line_item.scrolltorow(li_counter)
			RETURN -1	
		END IF 
		
		/* 1.190	The tax rate must be the determined based on the tax rate type for the province of the provider and for the effective date…*/
		IF ll_tax_rate_no > 0 THEN 
			ll_tax_rate_no_check =  inv_ephysio_invoice.nf_get_tax_rate(1, ls_prov_state_code, ldt_service_date ) 
			IF  ll_tax_rate_no  <> ll_tax_rate_no_check THEN 
				messagebox("Validation Error", 'The tax rate must be the determined based on the tax rate type for the province of the provider and for the effective date')
				dw_rehab_invoice_line_item.scrolltorow(li_counter)
				RETURN -1		
			END IF 
		END IF 

		/* 1.200	The tax amount must be calculated as the unit price * tax rate . [computed]*/
		IF ls_tax_applied_flag = 'Y'  THEN 
			ldec_check = round(((ldec_unit_price * ldec_units_billed) * ldec_tax_rate),2)
			IF ldec_total_tax_amount <> round(((ldec_unit_price * ldec_units_billed) * ldec_tax_rate),2) THEN
				messagebox("Validation Error", 'The tax amount must be calculated as the unit price * tax rate.')
				dw_rehab_invoice_line_item.scrolltorow(li_counter)
				RETURN -1	
			END IF 
		END IF 
		
		/* 1.210	The tax amount must be greater than zero if the billable item is to be taxed (i.e. the type of tax is GST, PST or HST). */
		IF ldec_total_tax_amount <= 0 AND  ll_tax_rate_no > 0  THEN
			messagebox("Validation Error", 'The tax amount must be greater than zero if the billable item is to be taxed (i.e. the type of tax is GST, PST or HST).')
			dw_rehab_invoice_line_item.scrolltorow(li_counter)
			RETURN -1	
		END IF 
		
		/* 1.220 	The Total Amount of the billable item must be calculated as the [ number of units * (unit price + the tax amount)] */
		IF ldec_total_amount <> round((ldec_units_billed * ldec_unit_price) + ldec_total_tax_amount,2) THEN
			messagebox("Validation Error", 'The Total Amount of the billable item must be calculated correctly.')
			dw_rehab_invoice_line_item.scrolltorow(li_counter)
			RETURN -1	
		END IF 
		
		/* DONE IN SAVE    *************************************************************************
			 1.230	The Remaining Authorized Amount must be calculated as [REHAB_TASK_AUTHORIZATION.authorized_amount – REHAB_TASK_AUTHORIZATION paid_amount]. 
		*/
		
		/************************************************************************************
		the following are the same checks as below but these are looking in the DB for APPLIED_CLAIM_TXN'S 
		    don't want to do these in the loop
		*************************************************************************************/
		/* 1.250	There must only be one billable item for the same date of service per course of treatment for a claim 
						(a voided invoice line item for a billable item is excluded)  */
		IF	inv_ephysio_invoice.nf_check_items_vs_applied(il_claim_no, ll_task_no, ldt_service_date, ll_billable_xref_no, ll_billable_item_no, 1) = -1 THEN
			messagebox("Validation Error", 'Two or more services of the same type must not be billed for the same date of service. A Payment has already been applied.')
			dw_rehab_invoice_line_item.scrolltorow(li_counter)
			RETURN -1	
		END IF
				
		/* 1.260	There must only be one Physiotherapy Assessment billed per course of treatment for a claim (a voided invoice line item for a billable item is excluded). */
		IF ll_billable_item_no = 172 THEN 
			IF	inv_ephysio_invoice.nf_check_items_vs_applied(il_claim_no, ll_task_no, ldt_service_date, ll_billable_xref_no, ll_billable_item_no, 2)  = -1 THEN
				messagebox("Validation Error", 'There must only be one Physiotherapy Assessment billed per course of treatment for a claim. A Payment has already been applied.')
				dw_rehab_invoice_line_item.scrolltorow(li_counter)
				RETURN -1		
			END IF 
		END IF
				
		/* 1.270	There must not be a Physiotherapy Assessment and a Physiotherapy Treatment billed for the same date of service per course of treatment for a claim 
						(a voided invoice line item for a billable item is excluded). */
		IF ll_billable_item_no = 172 OR ll_billable_item_no = 173 THEN 
			IF	inv_ephysio_invoice.nf_check_items_vs_applied(il_claim_no, ll_task_no, ldt_service_date, ll_billable_xref_no, ll_billable_item_no, 3)  = -1 THEN
				messagebox("Validation Error", 'Physio Treatments and Physio Assessments cannot be billed for the same day. A Payment has already been applied.')
				dw_rehab_invoice_line_item.scrolltorow(li_counter)
				RETURN -1		
			END IF 
		END IF 
				
		/* 1.280	There must not be a Physiotherapy Assessment (in the home) and a Physiotherapy Treatment (in the home) billed for the same date of service per course of treatment for a claim
		              (a voided invoice line item for a billable item is excluded). */
		IF ll_billable_item_no = 278 OR ll_billable_item_no = 279 THEN 
			IF	inv_ephysio_invoice.nf_check_items_vs_applied(il_claim_no, ll_task_no, ldt_service_date, ll_billable_xref_no, ll_billable_item_no, 4)  = -1 THEN
				messagebox("Validation Error", 'Physio Treatments (in the home) and Physio Assessments (in the home) cannot be billed for the same day. A Payment has already been applied.')
				dw_rehab_invoice_line_item.scrolltorow(li_counter)
				RETURN -1		
			END IF 
		END IF 
				
		/* 1.290	There must not be Missed Appointment (252) on the same date as any of the following services (for a course of treatment ):
			•	Physiotherapy Assessment  (173)
			•	Physiotherapy Treatment (172)
			•	TENS Assessment (201)
			•	Physiotherapy Assessment (in the home) (278)
			•	Physiotherapy Treatment (in the home) (279)
		*/
		IF ll_billable_item_no = 252 OR ll_billable_item_no = 173 OR   ll_billable_item_no = 172   OR   ll_billable_item_no = 201  OR   ll_billable_item_no = 278  OR   ll_billable_item_no = 279 THEN 
			IF	inv_ephysio_invoice.nf_check_items_vs_applied(il_claim_no, ll_task_no, ldt_service_date, ll_billable_xref_no, ll_billable_item_no, 5)  = -1 THEN
				messagebox("Validation Error", 'Missed appointments cannot be billed on the same day as another service. A Payment has already been applied.')
				dw_rehab_invoice_line_item.scrolltorow(li_counter)
				RETURN -1		
			END IF 	
		END IF 
		
		ldec_sum_amount_loop 			= ldec_reimburse_amount
		
		IF ls_is_a_service_flag = 'Y' AND ls_billable_unit_code <> 'EA' THEN 
			ldec_sum_units_billed_loop 	= ldec_quantity
		ELSE
			ldec_sum_units_billed_loop 	= ldec_units_billed
		END IF 
				
		/* The following are BR's that have to be checked against all the line items -- the BR's are contained in the loop */
		FOR li_temp_counter = 1 TO dw_rehab_invoice_line_item.ROWCOUNT()
			
			ll_temp_billable_xref_no 	= dw_rehab_invoice_line_item.getitemnumber(li_temp_counter,'billable_xref_no')
			ldt_temp_service_date		= date(dw_rehab_invoice_line_item.getitemdatetime(li_temp_counter,'service_date'))
			ll_temp_billable_item_no  	= dw_rehab_invoice_line_item.getitemnumber(li_temp_counter,'billable_item_no')
			ll_temp_task_no  				= dw_rehab_invoice_line_item.getitemnumber(li_temp_counter,'task_no')
			ll_temp_authorization_no	= dw_rehab_invoice_line_item.getitemnumber(li_temp_counter,'authorization_no')
			li_temp_line_no 				= dw_rehab_invoice_line_item.getitemnumber(li_temp_counter,'line_no')
 
			IF li_temp_counter <> li_counter THEN 
				
				IF ll_temp_authorization_no = ll_authorization_no AND ll_temp_billable_xref_no = ll_billable_xref_no AND li_temp_line_no = 0 THEN 
					
					ldec_sum_amount_loop 			= ldec_sum_amount_loop 	+ dw_rehab_invoice_line_item.getitemdecimal(li_temp_counter,'total_award_amount')
					
					IF ls_is_a_service_flag = 'Y' AND ls_billable_unit_code <> 'EA' THEN 
						ldec_sum_units_billed_loop 	= ldec_sum_units_billed_loop + dw_rehab_invoice_line_item.getitemdecimal(li_temp_counter,'quantity')
					ELSE	
						ldec_sum_units_billed_loop 	= ldec_sum_units_billed_loop + dw_rehab_invoice_line_item.getitemdecimal(li_temp_counter,'units_billed')
					END IF								
			  	END IF 
									
				/* 1.250	There must only be one billable item for the same date of service per course of treatment for a claim 
						(a voided invoice line item for a billable item is excluded) . QUESTION : can they submit an invoice for a supply for the same DATE. 
						E.g.   For SUPPLIES, can we ignore duplicates but check they are not exceeding the max limit)  */
				IF ldt_service_date = ldt_temp_service_date AND  ll_temp_billable_xref_no = ll_billable_xref_no THEN 
						messagebox("Validation Error", 'Two or more services of the same type must not be billed for the same date of service.')
						dw_rehab_invoice_line_item.scrolltorow(li_counter)
						RETURN -1	
				END IF
				
				/* 1.260	There must only be one Physiotherapy Assessment billed per course of treatment for a claim (a voided invoice line item for a billable item is excluded). */
				IF ll_billable_item_no = 172 AND ll_temp_billable_item_no = 172 THEN 
						messagebox("Validation Error", 'There must only be one Physiotherapy Assessment billed per course of treatment for a claim.')
						dw_rehab_invoice_line_item.scrolltorow(li_counter)
						RETURN -1		
				END IF 
				
				/* 1.270	There must not be a Physiotherapy Assessment and a Physiotherapy Treatment billed for the same date of service per course of treatment for a claim 
								(a voided invoice line item for a billable item is excluded). */
				IF ll_billable_item_no = 172 AND ll_temp_billable_item_no = 173 AND ldt_service_date = ldt_temp_service_date THEN 
						messagebox("Validation Error", 'Physio Treatments and Physio Assessments cannot be billed for the same day.')
						dw_rehab_invoice_line_item.scrolltorow(li_counter)
						RETURN -1		
				END IF 
				
				/* 1.280	There must not be a Physiotherapy Assessment (in the home) and a Physiotherapy Treatment (in the home) billed for the same date of service per course of treatment for a claim
				               (a voided invoice line item for a billable item is excluded). */
				IF ll_billable_item_no = 278 AND ll_temp_billable_item_no = 279 AND ldt_service_date = ldt_temp_service_date THEN 
						messagebox("Validation Error", 'Physio Treatments (in the home) and Physio Assessments (in the home) cannot be billed for the same day.')
						dw_rehab_invoice_line_item.scrolltorow(li_counter)
						RETURN -1		
				END IF 
				
				/* 1.290	There must not be Missed Appointment (252) on the same date as any of the following services (for a course of treatment ):
				•	Physiotherapy Assessment  (173)
				•	Physiotherapy Treatment (172)
				•	TENS Assessment (201)
				•	Physiotherapy Assessment (in the home) (278)
				•	Physiotherapy Treatment (in the home) (279)
				*/
				IF ll_billable_item_no = 252 AND ( ll_temp_billable_item_no = 173 OR   ll_temp_billable_item_no = 172   OR   ll_temp_billable_item_no = 201  OR   ll_temp_billable_item_no = 278  OR   ll_temp_billable_item_no = 279)  AND ldt_service_date = ldt_temp_service_date THEN 
						messagebox("Validation Error", 'Missed appointments cannot be billed on the same day as another service')
						dw_rehab_invoice_line_item.scrolltorow(li_counter)
						RETURN -1		
				END IF 	
			END IF 
		NEXT
		
		/* 1.95 	The number of units must be less than or equal to the remaining authorized quantity for the billable item if the billable item is explicitly authorized 
	                and the billable unit code is type ‘each’. 
					REHAB_TASK_AUTHORIZATION.authorized_quantity – REHAB_TASK_AUTHORIZATION paid_quantity  */
// 		IF ls_billable_unit_code = 'EA' THEN
//			IF  ( ldec_rta_authorized_quantity - ldec_rta_paid_quantity )  < ( ll_compute_units - ldec_saved_line_item_quantity ) THEN
//					messagebox("Validation Error", 'You have gone over the maximum allowable units for this authorized item.'  + ls_billable_item_desc_e )
//					dw_rehab_invoice_line_item.scrolltorow(li_counter)
//					RETURN -1	
//			END IF 
//		END IF 

		/* calculations inside loop validation done outside loop */
		/* 1.110  	The sum of the quantity of all unsubmitted invoice line items for a billable item must be less than or equal to the 
						Remaining Authorized Quantity for the billable item, if the billable item is explicitly authorized. 	*/
		IF  ldec_rta_authorized_quantity  < (  ldec_rta_paid_quantity + ldec_sum_units_billed_loop) THEN
			messagebox("Validation Error", 'The number of units for ' + ls_billable_item_desc_e  + ' exceeds the maximum allowed for that item.')
			dw_rehab_invoice_line_item.scrolltorow(li_counter)
			RETURN -1		
		END IF 
				
		/* 1.240 	The sum of the Total Amount of all unsubmitted invoice line items for a billable item must be less than or equal to the
						 Remaining Authorized Amount for the billable item. difference between 1.95*/
		IF 	ldec_rta_authorized_amount  <   ( ldec_rta_paid_amount + ldec_sum_amount_loop ) THEN
			messagebox("Validation Error", 'The Total Amount exceeds the amount authorized for ' + ls_billable_item_desc_e )
			dw_rehab_invoice_line_item.scrolltorow(li_counter)
			RETURN -1	
		END IF 	
		
NEXT

RETURN 1
end function

public subroutine wf_select_row (integer ai_row);/* all the records have been removed, no reason to keep the window open. */
IF dw_rehab_invoice_line_item.rowcount() > 0 THEN
	dw_rehab_invoice_line_item.SelectRow(0, FALSE)
	dw_rehab_invoice_line_item.SelectRow(ai_row, TRUE)
	dw_rehab_invoice_line_item.scrolltorow(ai_row)
END IF 
end subroutine

public subroutine wf_change_unit_price_on_provider_change (string as_contract_flag);/* Based on the Service Date and the Billable Item Number we need to set some defaults
this is so the user can modify the BI or the Service date and everything defaults as expected

When entering the invoice line item:
SERVICE provider and billable item determines whether the unit price is entered by user or not  
If the SERVICE provider is under the Contract for Fixed Fees
	If the Billable Item is a Fixed Fee
		Set the unit price from the Billable_Item_Fee table    
		The user cannot override this unit price or the amount payable
	Otherwise
		Do Not set the unit price
		The user must enter the unit price
Otherwise
	Do Not set the unit price 
	The user must enter the unit price
	
	NOTE: ONLY DO THIS FUNCTION IF THE PROVIDERS ephysio_contract_flag = 'Y'
	
	Payable Amount:
I think the only time the user needs to override the payable amount is when the Service Provider is NOT under contract and the recipient is 
the Individual as they may have paid tax. Need to confirm this with Diana.	
*/
INTEGER			li_line_no, li_counter
LONG				ll_billable_xref_no
DATE				ldt_service_date
DECIMAL			ldec_unit_fee
STRING				ls_fixed_fee_flag
	
FOR li_counter = 1 TO dw_rehab_invoice_line_item.rowcount()
	
	li_line_no 				= dw_rehab_invoice_line_item.getitemnumber(li_counter, 'line_no')
	
	IF li_line_no > 0 THEN CONTINUE

	ll_billable_xref_no	= dw_rehab_invoice_line_item.getitemnumber(li_counter, 'billable_xref_no')
	ldt_service_date	= DATE(dw_rehab_invoice_line_item.getitemdatetime(li_counter, 'service_date'))
	ldec_unit_fee 		= inv_ephysio_invoice.nf_get_item_unit_price(ll_billable_xref_no, ldt_service_date)
	
	ls_fixed_fee_flag = inv_ephysio_invoice.nf_get_fixed_fee_flag(ll_billable_xref_no)
	
	// SEE ABOVE
	IF ls_fixed_fee_flag = 'Y'  AND as_contract_flag = 'Y' THEN 
		dw_rehab_invoice_line_item.setitem(li_counter,'unit_price', ldec_unit_fee)
		dw_rehab_invoice_line_item.setitem(li_counter,'total_award_amount',ldec_unit_fee)	
		dw_rehab_invoice_line_item.setitem(li_counter,'submitted_amount', ldec_unit_fee)	
		dw_rehab_invoice_line_item.setitem(li_counter,'scenario_number', 1)		
	ELSE
		dw_rehab_invoice_line_item.setitem(li_counter,'unit_price',0)
		dw_rehab_invoice_line_item.setitem(li_counter,'total_award_amount',0)	
		dw_rehab_invoice_line_item.setitem(li_counter,'submitted_amount',0)	
		dw_rehab_invoice_line_item.setitem(li_counter,'scenario_number', 0)	
	END IF 
	
NEXT

end subroutine

public subroutine wf_change_tax_flag_on_provider_change (long al_recipient_no, string as_recipient_type, long al_provider_no, string as_provider_type);/* Based on the Service Date and the Billable Item Number we need to set some defaults
this is so the user can modify the BI or the Service date and everything defaults as expected

When entering the invoice line item:
SERVICE provider and billable item determines whether the unit price is entered by user or not  
If the SERVICE provider is under the Contract for Fixed Fees
	If the Billable Item is a Fixed Fee
		Set the unit price from the Billable_Item_Fee table    
		The user cannot override this unit price or the amount payable
	Otherwise
		Do Not set the unit price
		The user must enter the unit price
Otherwise
	Do Not set the unit price 
	The user must enter the unit price
	
	NOTE: ONLY DO THIS FUNCTION IF THE PROVIDERS ephysio_contract_flag = 'Y'
	
	Payable Amount:
I think the only time the user needs to override the payable amount is when the Service Provider is NOT under contract and the recipient is 
the Individual as they may have paid tax. Need to confirm this with Diana.

*/
INTEGER			li_line_no, li_counter
LONG				ll_billable_xref_no
STRING				ls_item_allows_tax_source
	
FOR li_counter = 1 TO dw_rehab_invoice_line_item.rowcount()
		
	li_line_no 							= dw_rehab_invoice_line_item.getitemnumber(li_counter, 'line_no')
	ll_billable_xref_no  				= dw_rehab_invoice_line_item.getitemnumber(li_counter, 'billable_xref_no')
	ls_item_allows_tax_source 	= dw_rehab_invoice_line_item.getitemstring(li_counter, 'item_allows_tax')
	
	IF li_line_no > 0 THEN CONTINUE
	
	IF  inv_ephysio_invoice.nf_set_allow_tax( al_recipient_no, as_recipient_type, al_provider_no, as_provider_type,  ll_billable_xref_no) = TRUE THEN 
		dw_rehab_invoice_line_item.setitem(li_counter,'item_allows_tax','Y')	
	ELSE	
		dw_rehab_invoice_line_item.setitem(li_counter,'item_allows_tax','N')
		dw_rehab_invoice_line_item.setitem(li_counter,'tax_rate_no',0)
		dw_rehab_invoice_line_item.setitem(li_counter,'tax_rate', 0.00)
		dw_rehab_invoice_line_item.setitem(li_counter,'tax_applied_flag','N')
		wf_set_submitted_amount()
	END IF 
	
NEXT

end subroutine

public subroutine wf_change_allow_payable_entry ();/* Based on the Service Date and the Billable Item Number we need to set some defaults
this is so the user can modify the BI or the Service date and everything defaults as expected

When entering the invoice line item:
SERVICE provider and billable item determines whether the unit price is entered by user or not  
If the SERVICE provider is under the Contract for Fixed Fees
	If the Billable Item is a Fixed Fee
		Set the unit price from the Billable_Item_Fee table    
		The user cannot override this unit price or the amount payable
	Otherwise
		Do Not set the unit price
		The user must enter the unit price
Otherwise
	Do Not set the unit price 
	The user must enter the unit price
	
	NOTE: ONLY DO THIS FUNCTION IF THE PROVIDERS ephysio_contract_flag = 'Y'
	
	Payable Amount:
I think the only time the user needs to override the payable amount is when the Service Provider is NOT under contract and the recipient is 
the Individual as they may have paid tax. Need to confirm this with Diana.

*/
INTEGER			li_line_no, li_counter
LONG				ll_submitted_by_no, ll_recipient_no
STRING			ls_submitted_by_type_code, ls_recipient_type_code
BOOLEAN         lb_check    

ll_recipient_no 				    = dw_reimburse.getitemnumber(1, 'recipient_no')
ls_recipient_type_code 	    = dw_reimburse.getitemstring(1, 'recipient_type_code') 
ls_submitted_by_type_code = dw_rehab_invoice.getitemstring(1, 'submitted_by_type_code')
ll_submitted_by_no             = dw_rehab_invoice.getitemnumber(1, 'submitted_by_no')
	
lb_check = 	inv_ephysio_invoice.nf_total_payment_amount_enterable( ll_submitted_by_no, ls_submitted_by_type_code, ll_recipient_no, ls_recipient_type_code) 
		
FOR li_counter = 1 TO dw_rehab_invoice_line_item.rowcount()
		
	li_line_no 	= dw_rehab_invoice_line_item.getitemnumber(li_counter, 'line_no')
	
	IF li_line_no > 0 THEN CONTINUE
			
	IF lb_check = TRUE THEN 
		dw_rehab_invoice_line_item.setitem(li_counter,'allow_payable_amount_entry','Y')		
	ELSE	
		dw_rehab_invoice_line_item.setitem(li_counter,'allow_payable_amount_entry','N')
	END IF 
	
NEXT


end subroutine

event open;call super::open;/*
In the Rehab Invoice window:

For each Invoice that is being reimbursed, the user must enter:
•	Provider (type and number) that provided the physio service
•	Recipient (type and number) that submitted the invoice or receipt for reimbursement
•	Provider’s own invoice number (optional)

The claim that received the service or supply will be derived from the document indexing data for the image that is being viewed
(i.e. DOCUMENT_INDEX.claim_no).

The provider that provided the physio service may be derived from the document indexing data for the image that is being viewed, 
if this information was entered at the time that the document was indexed 
(i.e. DOCUMENT_INDEX.service_provider_type_code & DOCUMENT_INDEX.service_provider_no).

The recipient that submitted the invoice for reimbursement must be one of the following:
•	Injured Worker for the associated claim (i.e. CLAIM.individual_no)
•	Physio Clinic that provided the service  (i.e. DOCUMENT_INDEX. service_provider_type_code & DOCUMENT_INDEX.service_provider_no)

The user must also select a default for the payment recipient of the reimbursement:
1.	Reimburse the Provider  - all invoice line items are paid to the provider
2.	Reimburse the Injured Worker – all invoice line items are paid to the injured worker 
3.	Split Reimbursement - this option would require the user to enter the recipient for each line item

ls_service_provider_type_code 	= istr_message.as_stringparm[1]
il_claim_no 								= istr_message.al_doubleparm[1]
ll_docid 										= istr_message.al_doubleparm[2]
ll_service_provider_no				= istr_message.al_doubleparm[3]
ldt_date_on_doc						= istr_message.adt_dateparm[1]

*/

istr_message= Message.PowerObjectParm

/* create the Business Rule Object */
inv_ephysio_invoice = create n_ephysio_invoice

//run the common refresh screen button values grabbed on open
wf_refresh_screen('INITIAL')

THIS.postevent('ue_postopen')

end event

on w_physio_reimbursements.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.dw_rehab_invoice=create dw_rehab_invoice
this.dw_rehab_invoice_line_item=create dw_rehab_invoice_line_item
this.tab_line_item=create tab_line_item
this.cb_add=create cb_add
this.cb_minus=create cb_minus
this.dw_reimburse=create dw_reimburse
this.cb_refresh=create cb_refresh
this.cb_resize=create cb_resize
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.cb_cancel
this.Control[iCurrent+3]=this.dw_rehab_invoice
this.Control[iCurrent+4]=this.dw_rehab_invoice_line_item
this.Control[iCurrent+5]=this.tab_line_item
this.Control[iCurrent+6]=this.cb_add
this.Control[iCurrent+7]=this.cb_minus
this.Control[iCurrent+8]=this.dw_reimburse
this.Control[iCurrent+9]=this.cb_refresh
this.Control[iCurrent+10]=this.cb_resize
end on

on w_physio_reimbursements.destroy
call super::destroy
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.dw_rehab_invoice)
destroy(this.dw_rehab_invoice_line_item)
destroy(this.tab_line_item)
destroy(this.cb_add)
destroy(this.cb_minus)
destroy(this.dw_reimburse)
destroy(this.cb_refresh)
destroy(this.cb_resize)
end on

event close;call super::close;IF IsValid(iw_active_sheet) THEN
		iw_active_sheet.iw_account_payment.cb_refresh.PostEvent("ue_update_complete")
END IF 
end event

type st_title from w_a_tool`st_title within w_physio_reimbursements
boolean visible = false
integer x = 15
integer y = 13
integer width = 3163
string text = "Physio Invoice Reimbursements"
end type

type cb_close from w_a_tool`cb_close within w_physio_reimbursements
boolean visible = false
integer taborder = 100
boolean enabled = false
end type

type cb_ok from commandbutton within w_physio_reimbursements
integer x = 2450
integer y = 1741
integer width = 336
integer height = 67
integer taborder = 110
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
end type

event clicked;INTEGER li_trancount

IF il_open_status = 2 THEN
	messagebox('Delete Only','Sorry but this window is for deleting web created Line Items only.', INFORMATION!)
	RETURN -1
END IF 

/* DO AN ACCEPTTEXT ON EVERYTHING */
dw_rehab_invoice_line_item.triggerevent('itemchanged')


dw_rehab_invoice.ACCEPTTEXT()
dw_rehab_invoice_line_item.ACCEPTTEXT()
dw_reimburse.ACCEPTTEXT()

IF dw_rehab_invoice_line_item.rowcount() <= 0 THEN 
	messagebox('Line items', 'There are currently no line items to save. Please add some line items.' )
	RETURN -1	
END IF 

// run the complete BR checks
IF  wf_check_buisness_rules() < 1  THEN  RETURN 

// run the save functionality
IF wf_save_invoice() = -1 THEN
	SQLCA.nf_transaction_count(li_trancount,1,'','','',FALSE)
	IF li_trancount > 0 THEN
		SQLCA.nf_rollback_transaction()
	END IF		
	messagebox('Save Error', 'There was an Error while Saving this record. Please try again or contact the HelpDesk.')
END IF 

//refresh the screen (set to intial state)
wf_refresh_screen('INITIAL')

THIS.enabled = FALSE

end event

type cb_cancel from commandbutton within w_physio_reimbursements
integer x = 2798
integer y = 1741
integer width = 336
integer height = 67
integer taborder = 120
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Close"
end type

event clicked;

IF cb_ok.enabled = TRUE THEN  
	MESSAGEBOX('Save or cancel', 'Please save or cancel your changes before leaving the screen.')
	RETURN 1
END IF 	

CLOSE(PARENT)
end event

type dw_rehab_invoice from u_datawindow within w_physio_reimbursements
integer x = 7
integer y = 3
integer width = 3163
integer height = 371
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_rehab_invoice_insert"
boolean border = false
end type

event constructor;call super::constructor;THIS.settransobject(sqlca)
end event

event clicked;call super::clicked;STRING			ls_provider_type, ls_submitted_by_type_code, ls_name, ls_recipient_type_code
INTEGER			li_scenario
BOOLEAN		lb_retrieve_billable_items = FALSE, lb_check_unit_price_change = FALSE
BOOLEAN		lb_check_allow_payable_amount_entry = FALSE
LONG				ll_task_no, ll_recipient_no

S_WINDOW_MESSAGE		lstr_message

IF row <= 0 THEN RETURN 

CHOOSE CASE dwo.name
				
	CASE 'b_provider'
		
		/*	get the type to search for */
		ls_provider_type 	= 	THIS.getitemstring(row, 'provider_type_code')
		
		IF ls_provider_type = 'V' OR ls_provider_type =  'M' OR ls_provider_type =  'O' THEN
			OpenWithParm(w_service_provider_search, ls_provider_type)
			lstr_message = Message.PowerObjectParm
			IF lstr_message.al_doubleparm[1] > 0 THEN
				
				THIS.SetColumn('provider_no')
				THIS.SetItem(row, 'provider_no', lstr_message.al_doubleparm[1])
					
				ls_name =  inv_ephysio_invoice.nf_get_provider_name(lstr_message.al_doubleparm[1], ls_provider_type)	

				THIS.setitem(row,'name', ls_name) 
				
				cb_ok.enabled = TRUE

			END IF
		ELSE
			MessageBox('Warning', 'No search available for this recipient type.')
		END IF
		
		// run the code at the bottom
			lb_retrieve_billable_items						= TRUE
			lb_check_unit_price_change					= TRUE   
			lb_check_allow_payable_amount_entry 	= TRUE
				
	CASE 'b_submitted_by'
		
		//submitted_by_type_code
		//submitted_by_no
		//submitted_by_name
		
		ls_submitted_by_type_code 	= 	THIS.getitemstring(row, 'submitted_by_type_code')
		
		IF ls_submitted_by_type_code = 'V' OR ls_submitted_by_type_code =  'M' OR ls_submitted_by_type_code =  'O' THEN
			OpenWithParm(w_service_provider_search, ls_submitted_by_type_code)
			lstr_message = Message.PowerObjectParm
			IF lstr_message.al_doubleparm[1] > 0 THEN
				
				THIS.SetColumn('submitted_by_no')
				THIS.SetItem(row, 'submitted_by_no', lstr_message.al_doubleparm[1])
					
				ls_name =  inv_ephysio_invoice.nf_get_provider_name(lstr_message.al_doubleparm[1], ls_provider_type)	

				THIS.setitem(row,'submitted_by_name', ls_name) 		
				
				cb_ok.enabled = TRUE
			END IF
		ELSE
			MessageBox('Warning', 'No search available for this type.')
		END IF

		
	CASE ELSE		
END CHOOSE

/*  
	CASE 1  REIMBURSE THE PROVIDER 	(i)	Provider is the provider that provided the service/supply 		
	CASE 2  REIMBURSE PROVIDER          ii) 	Provider did not provide the service and clinic under contract
	CASE 3  REIMBURSE PROVIDER         iii) Provider did not provide the service – clinic not under contract			
	CASE 4  REIMBURSE THE INJURED WORKER  i) Clinic is under contract
	CASE 5  REIMBURSE THE INJURED WORKER  ii)  Clinic is not under contract		
*/	

/* NEED TO re-retrieve the billable items dddw */
IF lb_retrieve_billable_items	= TRUE THEN 
	ll_task_no 				= istr_info_returned_from_tab.task_no
	wf_retrieve_billable_item_dddw(ll_task_no, il_claim_no ,lstr_message.al_doubleparm[1], ls_provider_type)
END IF 

IF  lb_check_unit_price_change = TRUE THEN 
	wf_change_unit_price_on_provider_change( inv_ephysio_invoice.nf_get_physio_contract_flag(lstr_message.al_doubleparm[1], ls_provider_type))
END IF 

IF  lb_check_allow_payable_amount_entry 	= TRUE THEN	
	wf_change_allow_payable_entry()	
END IF 


end event

event itemchanged;call super::itemchanged;/*
  SELECT 	rehab_invoice_no,            		external_invoice_id,          		invoice_date,   
         		provider_no,            				provider_type_code,        		submitted_date,   
         		submitted_by_no,            		submitted_by_type_code,         '' as Name,               			
			   '' as submitted_by_name,         '' as submitted_by_selector										
*/

LONG			ll_provider_no
STRING			ls_provider_type_code, ls_individual_name, ls_submitted_by_type, ls_submitted_by_name, ls_check
INTEGER		li_counter
BOOLEAN		lb_provider_allows_tax

CHOOSE CASE dwo.name
	CASE  	'rehab_invoice_no'   
    CASE  	'external_invoice_id'   
			/* External Invoice Number – the user may enter the invoice number from the provider’s invoice. This is optional */
			ls_check = trim(data)
			this.setitem(row, 'external_invoice_id'  , ls_check) //fix for check constraint.
				
	CASE  	'provider_no'   
			/* Provider of the service or supply - The user must identify the provider – 
			    either the Clinic that provided the physiotherapy treatment OR a provider that provided the supply.  */
				 
			ls_submitted_by_type = THIS.getitemstring(row, 'provider_type_code')
			IF NOT ISNULL(ls_submitted_by_type) AND trim(ls_submitted_by_type) > ''  THEN 
							
				ll_provider_no 				= LONG(data)
				
				IF inv_ephysio_invoice.nf_check_valid_info(ll_provider_no, ls_submitted_by_type, 1) = FALSE THEN 
					MessageBox("Validation Error","Invalid provider",Exclamation!)
					RETURN 1
				END IF
					
				ls_submitted_by_name 	=  inv_ephysio_invoice.nf_get_provider_name(ll_provider_no, ls_submitted_by_type)                              			
		
				THIS.setitem(row,'name', ls_submitted_by_name)	
				
			END IF 
						
    CASE  	'provider_type_code'   
		
				THIS.setitem(row, 'provider_no', 0)
				THIS.setitem(row,'name', '')	
						
	CASE  	'submitted_by_no'   
			/* •	Submitted By Recipient – the user must identify the recipient (recipient type and number) that submitted the invoice or receipt for reimbursement 
			
				The recipient that submitted the invoice for reimbursement must be one of the following:
				•	Injured Worker for the associated claim (i.e. CLAIM.individual_no)
				•	Physio Clinic that provided the service  (REHAB_TASK.provider_no, REHAB_TASK.provider_type_code)
				•	Provider other than the Physio Clinic that provided the physio treatment 
			*/
			ls_submitted_by_type = THIS.getitemstring(row, 'submitted_by_type_code')
			IF NOT ISNULL(ls_submitted_by_type) AND trim(ls_submitted_by_type) > ''  THEN 
				
				ll_provider_no 				= LONG(data)
				
				IF inv_ephysio_invoice.nf_check_valid_info(ll_provider_no, ls_submitted_by_type, 1) = FALSE THEN 
					MessageBox("Validation Error","Invalid Submitted by",Exclamation!)
					RETURN 1
				END IF
				
				ls_submitted_by_name 	=  inv_ephysio_invoice.nf_get_provider_name(ll_provider_no, ls_submitted_by_type)                              			
		
				THIS.setitem(row,'submitted_by_name', ls_submitted_by_name)	
		
			END IF 
							
    CASE  	'submitted_by_type_code'   
		
			/* if it is the Individual set the number to the individual_no and the name to the Individuals name */
			IF data = 'I' THEN 
				
				THIS.setitem(row, 'submitted_by_no', il_individual_no)

				ls_submitted_by_name 	=  inv_ephysio_invoice.nf_get_provider_name(il_individual_no, data)     
			
				THIS.setitem(row,'submitted_by_name', ls_submitted_by_name)	
				
			ELSE
				THIS.setitem(row, 'submitted_by_no', 0)
				THIS.setitem(row,'submitted_by_name', '')	
			END IF 

END CHOOSE
	
end event

event ue_itemchangeaccepted;call super::ue_itemchangeaccepted;/*
  SELECT 	rehab_invoice_no,            		external_invoice_id,           	invoice_date,   
         			provider_no,            				provider_type_code,        		submitted_date,   
         			submitted_by_no,            		submitted_by_type_code,             '' as Name,              				
			   '' as submitted_by_name									
*/
INTEGER		li_scenario, li_error
BOOLEAN	lb_check_scenario = FALSE,  lb_check_tax_flag = FALSE, lb_retrieve_billable_items = FALSE, lb_check_unit_price_change = FALSE
BOOLEAN	lb_check_allow_payable_amount_entry = FALSE
LONG			ll_provider_no, ll_task_no, ll_recipient_no
STRING		ls_provider_type, ls_recipient_type_code, ls_check

IF al_row < 1 THEN RETURN 

cb_ok.enabled = TRUE

CHOOSE CASE as_column_name
	CASE  'rehab_invoice_no'   
    CASE  	'external_invoice_id'   
			/* External Invoice Number – the user may enter the invoice number from the provider’s invoice. This is optional */
					
	CASE  	'provider_no'   
			/* Provider of the service or supply - The user must identify the provider – 
			    either the Clinic that provided the physiotherapy treatment OR a provider that provided the supply.  */
			// run the code at the bottom
			lb_check_scenario 								= TRUE
			lb_retrieve_billable_items						= TRUE
			lb_check_unit_price_change					= TRUE  
			
    CASE  	'provider_type_code'   
				
			// run the code at the bottom
			lb_check_scenario 					= TRUE
			lb_retrieve_billable_items			= TRUE
		
	CASE  	'submitted_by_no'   
			/* •	Submitted By Recipient – the user must identify the recipient (recipient type and number) that submitted the invoice or receipt for reimbursement 
			
				The recipient that submitted the invoice for reimbursement must be one of the following:
				•	Injured Worker for the associated claim (i.e. CLAIM.individual_no)
				•	Physio Clinic that provided the service  (REHAB_TASK.provider_no, REHAB_TASK.provider_type_code)
				•	Provider other than the Physio Clinic that provided the physio treatment 
				
				NOTE: from jp - SUBMITTED BY recipient is for information only; it has no impact on the functionality	
			*/
			
			// run the code at the bottom
			lb_check_allow_payable_amount_entry 	= TRUE
				
    CASE  	'submitted_by_type_code'   
			/* NOTE: from jp - SUBMITTED BY recipient is for information only; it has no impact on the functionality */
			
			// run the code at the bottom
			lb_check_allow_payable_amount_entry 	= TRUE
		
END CHOOSE

/*  
	CASE 1  REIMBURSE THE PROVIDER 	           (i) Provider is the provider that provided the service/supply 		
	CASE 2  REIMBURSE PROVIDER                     ii) Provider did not provide the service and clinic under contract
	CASE 3  REIMBURSE PROVIDER                     iii) Provider did not provide the service – clinic not under contract			
	CASE 4  REIMBURSE THE INJURED WORKER   i) Clinic is under contract
	CASE 5  REIMBURSE THE INJURED WORKER   ii)  Clinic is not under contract		
*/	

ll_provider_no 		= THIS.getitemnumber(al_row, 'provider_no' )
ls_provider_type 	= THIS.getitemstring(al_row, 'provider_type_code')

/* NEED TO re-retrieve the billable items dddw */
IF lb_retrieve_billable_items	= TRUE THEN 
	ll_task_no 				= istr_info_returned_from_tab.task_no
	
	wf_retrieve_billable_item_dddw(ll_task_no, il_claim_no ,ll_provider_no, ls_provider_type)	
END IF 

IF  lb_check_unit_price_change = TRUE THEN 
	wf_change_unit_price_on_provider_change( inv_ephysio_invoice.nf_get_physio_contract_flag(ll_provider_no, ls_provider_type))	
END IF 

IF  lb_check_allow_payable_amount_entry 	= TRUE THEN
	wf_change_allow_payable_entry()	
END IF 

end event

type dw_rehab_invoice_line_item from u_datawindow within w_physio_reimbursements
integer x = 7
integer y = 979
integer width = 3354
integer height = 736
integer taborder = 30
boolean bringtotop = true
string dataobject = "d_rehab_invoice_line_item_insert"
boolean minbox = true
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = false
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;THIS.settransobject(sqlca)
THIS.uf_setselect(1)
end event

event clicked;call super::clicked;LONG						ll_claim_no, ll_task_no
INTEGER					li_selected_tab
s_window_message 	lstr_message 

CHOOSE CASE dwo.name
	CASE 'b_plus' //3	Delete Invoice Line Item	
		
		IF row <= 0  THEN RETURN 
				
		cb_add.triggerevent(clicked!)
		
	CASE 'b_minus'
		
			IF row <= 0  THEN RETURN 
		
			//CAN'T delete to nothing
			IF THIS.rowcount() > 0 THEN
				cb_minus.triggerevent(clicked!)
			END IF 
			
			THIS.SelectRow(0, FALSE)
			THIS.SelectRow(row, TRUE)
			
	CASE 'b_info'
			
			li_selected_tab = tab_line_item.selectedtab

			// this function brings back all of the information we need for the tabpage
			istr_info_returned_from_tab = tab_line_item.Control[li_selected_tab].DYNAMIC FUNCTION of_return_tab_info()

			ll_task_no 	= istr_info_returned_from_tab.task_no
			
			IF ll_task_no > 0  THEN 
				
				lstr_message.al_doubleparm[1] 		= il_claim_no
				lstr_message.al_doubleparm[2]  		= ll_task_no
		
				openwithparm(w_auth_info_popup, lstr_message)	
			END IF 
		
END CHOOSE

end event

event keydown;call super::keydown;IF THIS.GetColumnName()  = 'total_award_amount' THEN
	IF KeyDown(KeyEnter!) THEN
		cb_add.triggerevent(clicked!)
	END IF 
END IF

end event

event itemchanged;call super::itemchanged;STRING							ls_physio_contract_flag, ls_service_provider_type_code
INTEGER						li_provider_row
LONG							ll_service_provider_no, ll_billable_xref_no, ll_authorization_no
DATE							ldt_service_date
DATAWINDOWCHILD		ldwc_child


IF ISNULL(row) OR row <= 0 THEN  RETURN 1 

li_provider_row = dw_rehab_invoice.getrow()
IF ISNULL(li_provider_row) OR li_provider_row < 0 THEN RETURN 1

cb_ok.enabled = TRUE

ll_service_provider_no 				= dw_rehab_invoice.getitemnumber(li_provider_row, 'provider_no')
ls_service_provider_type_code 	= dw_rehab_invoice.getitemstring(li_provider_row, 'provider_type_code') 

//select the physio_contract_flag from the PROVIDER
ls_physio_contract_flag = inv_ephysio_invoice.nf_get_physio_contract_flag(ll_service_provider_no, ls_service_provider_type_code)
IF ls_physio_contract_flag = '' THEN RETURN -1

//grab the child
dw_rehab_invoice_line_item.getchild('billable_xref_no', ldwc_child)

CHOOSE CASE dwo.name
    CASE	'service_date'   	/* Service Date -- Can be entered by the user or the user can select from a calendar pick list */
		
			/* if the user has already set the billable item we need to set some defaults */			
			ldt_service_date 	= date(data)
			IF IsNull(ldt_service_date) OR STRING(ldt_service_date,'YYYY-MM-DD') = '1900-01-01'  OR ldt_service_date > Date('2079-06-06') OR ldt_service_date <= Date('1900-01-01') THEN 
		 		messagebox("Validation Error", 'The service date is required and must be a valid date between 1900-01-01 and 2079-06-06')
				RETURN 1
			END IF 
		
			ll_billable_xref_no 	= THIS.getitemnumber(row, 'billable_xref_no')
			
			IF ll_billable_xref_no > 0  THEN 
				wf_set_defaults_based_dw_values( ll_billable_xref_no, ldt_service_date)
			END IF 
					
    CASE	'fee_no'   
    CASE 	'billable_xref_no'   
		
			/* List of billable items
			The list of billable items that are displayed will depend on the provider that provided the physio service or supply. 

			If the provider is under the WorkSafeNB contract (i.e. PROVIDER.physio_contract_flag = ‘Y’)¸ 
			then the list of billable items will be a combination of a list of billable items that are explicitly authorized and a
			list of billable items that can be implicitly authorized. For explicitly authorized items, the list will be all REHAB_TASK_AUTHORIZATION 
			records for the related REHAB_TASK where the REHAB_TASK_AUTHORIZATION is authorized for the provider on the invoice that was 
			submitted for reimbursement. For implicitly authorized items, the list will be all Billable_Items_rehab_Task_Xref.explicit_authorization_flag=’N’ for the related REHAB_TASK).

			If the provider is not under the WorkSafeNB contract (i.e. PROVIDER.physio_contract_flag = ‘N’)¸
			then the list of billable items will a list of billable items that are explicitly authorized (i.e. all REHAB_TASK_AUTHORIZATION records for the related
			REHAB_TASK where the REHAB_TASK_AUTHORIZATION is authorized for the provider on the invoice that was submitted for reimbursement).
			*/
		
			ll_billable_xref_no = long(data)
			
			ldt_service_date = DATE(THIS.getitemdatetime(row, 'service_date'))
			
			IF NOT ISNULL(ldt_service_date) THEN 
				wf_set_defaults_based_dw_values( ll_billable_xref_no, ldt_service_date)
			END IF 
			
			ll_authorization_no = ldwc_child.getitemnumber(ldwc_child.getrow(), 'authorizationno')
			this.setitem(row,'authorization_no',ll_authorization_no)
									
    CASE   	'billable_unit_code'   	//BASED ON BILLABLE UNIT
    CASE    'quantity'   				//controlled by the billable item			
    CASE    'unit_price'   				//controlled by the billable item
		
			/* Unit Price
			Note: the invoices must be submitted by either the injured worker or by a physio clinic that is not set up to use the Physio website. 
			The authorized provider is the clinic that performed the service – for reimbursements submitted by the injured worker, 
			the authorized provider may be a physio clinic that is set up to use the Physio website.
			If the authorized provider of the billable item is under the WorkSafeNB physio contract and the billable item is a fixed fee rate, 
			the unit price will be set to the fixed fee and the user may not override it.   Is this true even if we are reimbursing the IW’s private insurance company like BlueCross ????
			If the authorized provider of the billable item is not under the WorkSafeNB physio contract, the unit price must be entered by the user.
			If the billable item is a variable fee rate, the unit price must be entered by the user.
			*/
				wf_set_submitted_amount()
			
    CASE    'total_non_tax_amount'   	/* Total Non-Tax Amount  Computed as the [unit price * quantity]  */
    CASE    'total_tax_amount'    		/* controlled by tax rate no */ 
    CASE    'total_amount'   				/* Total Amount Computed as the [unit price * quantity] + tax amount */		
	CASE 	'Submitted Amount' 			/* This would be entered by the user.  */	
    CASE    'total_award_amount' 			/* Reimbursed Amount Will default to the submitted amount but may be modified by the user */
		
				//	Payable Amount:
				// I think the only time the user needs to override the payable amount is when the Service Provider is NOT under contract 
				// and the recipient is the Individual as they may have paid tax. Need to confirm this with Diana.
				
END CHOOSE
end event

event ue_itemchangeaccepted;call super::ue_itemchangeaccepted;STRING		 	ls_prov_state_code, ls_recipient_type_code
INTEGER		li_tax_rate_no
DECIMAL		ldec_tax_rate
DATE			ldt_service_date
LONG			ll_authorization_no, ll_recipient_no

CHOOSE CASE as_column_name
    CASE	'service_date'    	/* Service Date -- Can be entered by the user or the user can select from a calendar pick list */
				
		THIS.setitem(al_row,'tax_applied_flag','N')		
		THIS.setitem(al_row,'tax_rate',0.00)	
		THIS.setitem(al_row,'tax_rate_no',0)	
	
		//set the submitted amount
		wf_set_submitted_amount()
				
    CASE	'fee_no'   
    CASE 	'billable_xref_no'  
		
		//set the submitted amount
		wf_set_submitted_amount()
				
    CASE   	'billable_unit_code'   //BASED ON BILLABLE UNIT
    CASE    'quantity'   
    CASE    'units_billed'   
		
		//set the submitted amount
		wf_set_submitted_amount()
			 
    CASE    'unit_price'    //controlled by the billable item
		
			/* Unit Price
			Note: the invoices must be submitted by either the injured worker or by a physio clinic that is not set up to use the Physio website. 
			The authorized provider is the clinic that performed the service – for reimbursements submitted by the injured worker, 
			the authorized provider may be a physio clinic that is set up to use the Physio website.
			If the authorized provider of the billable item is under the WorkSafeNB physio contract and the billable item is a fixed fee rate, 
			the unit price will be set to the fixed fee and the user may not override it.   Is this true even if we are reimbursing the IW’s private insurance company like BlueCross ????
			If the authorized provider of the billable item is not under the WorkSafeNB physio contract, the unit price must be entered by the user.
			If the billable item is a variable fee rate, the unit price must be entered by the user.
			*/
			
			//set the submitted amount
			wf_set_submitted_amount()

    CASE    'total_non_tax_amount'    	/* Total Non-Tax Amount  Computed as the [unit price * quantity]  */
    CASE    'total_tax_amount'    		/* controlled by tax rate no */ 
    CASE    'total_amount'    				/* Total Amount Computed as the [unit price * quantity] + tax amount */			
	CASE 	'Submitted Amount' 			/* This would be entered by the user.  */
    CASE    'total_award_amount' 		/* Reimbursed Amount Will default to the submitted amount but may be modified by the user */
	CASE   	'tax_applied_flag'
		
			ldt_service_date 						= DATE(THIS.getitemdatetime(al_row, 'service_date'))
			
			ls_recipient_type_code             	= dw_reimburse.getitemstring(1, 'recipient_type_code') 
			ll_recipient_no                         = dw_reimburse.getitemnumber(1, 'recipient_no')
		
			IF data = 'N' THEN
				//set the tax rate
				THIS.setitem(al_row,'tax_rate',0.00)	
				THIS.setitem(al_row,'tax_rate_no',0)	
			ELSE	
				
				//get the tax rate of the provider
				ls_prov_state_code 	= inv_ephysio_invoice.nf_get_prov_state_code(ll_recipient_no, ls_recipient_type_code)	
				li_tax_rate_no 			= inv_ephysio_invoice.nf_get_tax_rate(1, ls_prov_state_code, ldt_service_date)
					
				IF li_tax_rate_no > 0 THEN
					ldec_tax_rate =  inv_ephysio_invoice.nf_get_tax_rate_percent(li_tax_rate_no)
				
						THIS.setitem(al_row,'tax_rate', ldec_tax_rate)	
						THIS.setitem(al_row,'tax_rate_no', li_tax_rate_no)	
				ELSE
						THIS.setitem(al_row,'tax_rate',0.00)	
						THIS.setitem(al_row,'tax_rate_no',0)	
				END IF 
					
			END IF 
			
				wf_set_submitted_amount()
		
	CASE 	'apply_tax_confiqured'
		       
				wf_set_submitted_amount()
		
END CHOOSE

// enable the save button
cb_ok.enabled = TRUE
end event

type tab_line_item from tab within w_physio_reimbursements
integer x = 4
integer y = 899
integer width = 3141
integer height = 83
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
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
end type

event selectionchanged;DATE 					ldt_planned_date
LONG					ll_provider_no, ll_task_no
STRING					ls_provider_type, ls_filter
INTEGER				li_inserted_row

// this function brings back all of the information we need for the tabpage
istr_info_returned_from_tab = tab_line_item.Control[newindex].DYNAMIC FUNCTION of_return_tab_info()

ldt_planned_date 	= istr_info_returned_from_tab.planned_start_date 
ll_task_no 				= istr_info_returned_from_tab.task_no

ll_provider_no 		= dw_rehab_invoice.getitemnumber(1, 'provider_no') 
ls_provider_type 	= dw_rehab_invoice.getitemstring(1,'provider_type_code') 

//undo the current filter
dw_rehab_invoice_line_item.setfilter('')
dw_rehab_invoice_line_item.Filter()

//set the new filter based on the task_no
ls_filter = 'task_no = ' + string(ll_task_no)

/* filter the appropriate records in the billable items datawindow */
dw_rehab_invoice_line_item.SetRedraw(false)
dw_rehab_invoice_line_item.setfilter(ls_filter)
dw_rehab_invoice_line_item.Filter()
dw_rehab_invoice_line_item.SetRedraw(true)

/* retrieve the dropdown billable items */
wf_retrieve_billable_item_dddw(ll_task_no, il_claim_no ,ll_provider_no, ls_provider_type)

//select row #1 in the dw if there is one.
parent.wf_select_row(1)

IF newindex > 0 THEN
 tab_line_item.Control[newindex].TabBackColor = RGB(0, 255, 255)
END IF 

IF oldindex > 0 THEN
 tab_line_item.Control[oldindex].TabBackColor = 79741120
END IF 
end event

event selectionchanging;IF dw_rehab_invoice_line_item.rowcount() > 0 THEN 
	IF cb_ok.enabled = TRUE THEN 
		messagebox('Confirm Save', 'Please Save or Cancel your current work before moving to a different Task.')
		RETURN 1
	END IF 	
END IF

end event

type cb_add from commandbutton within w_physio_reimbursements
integer x = 1317
integer y = 1741
integer width = 176
integer height = 67
integer taborder = 120
boolean bringtotop = true
integer textsize = -14
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "+"
end type

event clicked;LONG			ll_task_no
INTEGER		li_rowcount

ll_task_no 	= istr_info_returned_from_tab.task_no

wf_add_record(ll_task_no)

li_rowcount = dw_rehab_invoice_line_item.rowcount()

// highlight the row if applicable
wf_select_row(li_rowcount)

dw_rehab_invoice_line_item.setfocus()
end event

type cb_minus from commandbutton within w_physio_reimbursements
integer x = 1488
integer y = 1741
integer width = 176
integer height = 67
integer taborder = 120
boolean bringtotop = true
integer textsize = -14
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "-"
end type

event clicked;INTEGER			li_rtn, li_selected_row
DWITEMSTATUS     	ldwis_rowstatus

li_selected_row = dw_rehab_invoice_line_item.getrow()
IF isnull(li_selected_row) OR li_selected_row < 1 THEN RETURN -1

ldwis_rowstatus = dw_rehab_invoice_line_item.getitemstatus(li_selected_row, 0, PRIMARY!)

//get the row status - if it is new modified just delete it
IF ldwis_rowstatus = new! OR ldwis_rowstatus = newmodified! THEN
	dw_rehab_invoice_line_item.deleterow(li_selected_row)
	
	// highlight the first row if applicable
	dw_rehab_invoice_line_item.selectrow(dw_rehab_invoice_line_item.rowcount(),true)
	
	RETURN 1	
END IF 

// need to check and see if there are pending changes - I don't think we should allow them to delete if a save is pending 
/* if the save is enabled do not allow delete */
IF cb_ok.enabled = TRUE THEN 
	messagebox('Save Pending', 'Please save or Cancel pending changes before proceeding with Delete.')
	RETURN -1
END IF 

/*     FULL ON DELETE  FULL ON DELETE FULL ON DELETE ************************************/
li_rtn = wf_delete_record()

IF li_rtn = -1 THEN
	SQLCA.nf_rollback_transaction()
	messagebox('Delete Error', 'There was an Error while deleting this record. Please try again or contact the HelpDesk.')
ELSEIF li_rtn = 0 THEN 
	RETURN 1//do not refreseh the screen
END IF 

//refresh the screen (set to intial state)
wf_refresh_screen('INITIAL')

/* all the records have been removed, no reason to keep the window open. */
wf_select_row(1)

end event

type dw_reimburse from u_datawindow within w_physio_reimbursements
integer y = 381
integer width = 3152
integer height = 522
integer taborder = 11
boolean bringtotop = true
string dataobject = "d_transaction_details_ephysio"
boolean controlmenu = true
boolean border = false
end type

event constructor;call super::constructor;THIS.settransobject(sqlca)
end event

event clicked;call super::clicked;STRING		ls_recipient_type, ls_name, ls_recipient_subtype, ls_provider_type_code, ls_data
INTEGER		li_scenario
BOOLEAN	lb_check_scenario = FALSE, lb_check_allow_tax_flag  = FALSE, lb_check_payment_method_code  = FALSE, lb_get_recipient_sub_type  = FALSE
BOOLEAN	lb_check_allow_payable_amount_entry = FALSE, lb_check
LONG			ll_provider_no_invoice, ll_color
S_WINDOW_MESSAGE		lstr_message

IF row <= 0 THEN RETURN 

CHOOSE CASE dwo.name
				
	CASE 'b_recipient'
	

		/*	get the type to search for */
		ls_recipient_type 	= 	THIS.getitemstring(row, 'recipient_type_code')
		
		IF ls_recipient_type = 'V' OR ls_recipient_type =  'M' OR ls_recipient_type =  'O' THEN
			OpenWithParm(w_service_provider_search, ls_recipient_type)
			lstr_message = Message.PowerObjectParm
			IF lstr_message.al_doubleparm[1] > 0 THEN
				
				IF  inv_ephysio_invoice.nf_provider_setup_on_website( lstr_message.al_doubleparm[1], ls_recipient_type) = TRUE THEN 
					messagebox('E-Physio Provider', 'In order to reimburse a provider through this module, the provider must not be a provider that is set up to use the Physio website' )
					RETURN
				END IF 	
				
				THIS.SetColumn('recipient_no')
				THIS.SetItem(row, 'recipient_no', lstr_message.al_doubleparm[1])
					
				ls_name =  inv_ephysio_invoice.nf_get_provider_name(lstr_message.al_doubleparm[1], ls_recipient_type)	
				THIS.setitem(row,'recipient_name', ls_name) 		
				
				cb_ok.enabled = TRUE
			END IF
		ELSE
			MessageBox('Warning', 'No search available for this recipient type.')
		END IF
		
		// run the code at the bottom
		 lb_check_scenario 								= TRUE
		 lb_check_allow_tax_flag  						= TRUE
		 lb_check_payment_method_code  		= TRUE
		 lb_get_recipient_sub_type  					= TRUE
		 lb_check_allow_payable_amount_entry 	= TRUE
		
		wf_get_address_info(lstr_message.al_doubleparm[1], ls_recipient_type)
		
	CASE ELSE
		
END CHOOSE

/*  
CASE 1  REIMBURSE THE PROVIDER 	(i)		Provider is the provider that provided the service/supply 		
CASE 2  REIMBURSE PROVIDER ii)	Provider did not provide the service and clinic under contract
CASE 3  REIMBURSE PROVIDER   iii)	Provider did not provide the service – clinic not under contract			
CASE 4  REIMBURSE THE INJURED WORKER  i) Clinic is under contract
CASE 5  REIMBURSE THE INJURED WORKER  ii)  Clinic is not under contract		 
*/			

ll_provider_no_invoice    = dw_rehab_invoice.getitemnumber(1, 'provider_no')
ls_provider_type_code 	= dw_rehab_invoice.getitemstring(1, 'provider_type_code')

IF lb_check_allow_tax_flag = TRUE THEN
	wf_change_tax_flag_on_provider_change( lstr_message.al_doubleparm[1], ls_recipient_type, ll_provider_no_invoice , ls_provider_type_code)
END IF 

IF lb_check_payment_method_code = TRUE THEN
	wf_set_payment_method(lstr_message.al_doubleparm[1], ls_recipient_type)
END IF 

IF lb_get_recipient_sub_type = TRUE THEN
	IF ls_recipient_type <> 'I' THEN 
		ls_recipient_subtype =  inv_ephysio_invoice.nf_get_recipient_subtype(lstr_message.al_doubleparm[1], ls_recipient_type)
	ELSE
		ls_recipient_subtype = ''
	END IF
	
	THIS.setitem(row, 'recipient_sub_type_code', ls_recipient_subtype)
END IF

IF  lb_check_allow_payable_amount_entry 	= TRUE THEN
	wf_change_allow_payable_entry()	
END IF 

end event

event itemchanged;call super::itemchanged;LONG			ll_recipient_no
STRING			ls_recipient_type_code, ls_individual_name, ls_recipient_name
INTEGER		li_scenario, li_counter

CHOOSE CASE dwo.name
	CASE 'recipient_no' 
		
		// SEE ue_itemchanged_accepted
		
			/* •	Reimbursement Recipient – the recipient or recipients that are being reimbursed */
			ls_recipient_type_code = THIS.getitemstring(row, 'recipient_type_code')
			IF NOT ISNULL(ls_recipient_type_code) AND trim(ls_recipient_type_code) > ''  THEN 
				
				ll_recipient_no 		= LONG(data)
				ls_recipient_name 	=  inv_ephysio_invoice.nf_get_provider_name(ll_recipient_no, ls_recipient_type_code)    
				
//				IF inv_ephysio_invoice.nf_check_valid_info(ll_recipient_no, ls_recipient_type_code, 2) = FALSE THEN 
//					MessageBox("Validation Error","Invalid Recipient",Exclamation!)
//					RETURN 1
//				END IF
				
//				IF  inv_ephysio_invoice.nf_provider_setup_on_website(ll_recipient_no, ls_recipient_type_code) = TRUE THEN 
//					messagebox('E-Physio Provider', 'In order to reimburse a provider through this module, the provider must not be a provider that is set up to use the Physio website' )
//					RETURN 1
//				END IF 			
				
				THIS.setitem(row,'recipient_name', ls_recipient_name)	
				
				wf_get_address_info(ll_recipient_no, ls_recipient_type_code)
							
			END IF 
								
	CASE 'recipient_type_code'
		
		// SEE ue_itemchanged_accepted
		
		/* if it is the Individual set the number to the individual_no and the name to the Individuals name */
			IF data = 'I' THEN 
				
				THIS.setitem(row, 'recipient_no',il_individual_no)
				ls_individual_name = inv_ephysio_invoice.nf_get_provider_name(il_individual_no, 'I')
			
				THIS.setitem(row,'recipient_name', ls_individual_name)	
				THIS.setitem(row,'recipient_sub_type_code', '')	
				
				ll_recipient_no = il_individual_no
				
			ELSE
				THIS.setitem(row, 'recipient_no', 0)
				THIS.setitem(row,'recipient_name', '')	
				ll_recipient_no = 0
			END IF 
			
			wf_get_address_info(ll_recipient_no, data)
			
			// set some defaults - scheduled processing date
			dw_reimburse.setitem(row, 'scheduled_processing_date',  inv_ephysio_invoice.nf_get_thursday_processing_date(data))
							
	CASE 'payment_method_code'
			/* •	Payment method for reimbursing – for each reimbursement recipient, the method of payment for that recipient */

	CASE 'use_default_address_flag'		
	
		// SEE ue_itemchanged_accepted
			
END CHOOSE

end event

event ue_itemchangeaccepted;call super::ue_itemchangeaccepted;BOOLEAN	lb_check_scenario = FALSE, lb_check_allow_tax_flag = FALSE, lb_check_payment_method_code = FALSE, lb_get_recipient_sub_type = FALSE
BOOLEAN	lb_check_allow_payable_amount_entry
LONG			ll_recipient_no, ll_provider_no_invoice
STRING		ls_recipient_type, ls_recipient_subtype, ls_provider_type_code

cb_ok.enabled = TRUE

IF al_row < 1 THEN RETURN 

CHOOSE CASE as_column_name
	CASE 'recipient_no' 
		
			lb_check_scenario					 			= TRUE
			lb_check_payment_method_code 			= TRUE
			lb_get_recipient_sub_type 					= TRUE
			lb_check_allow_tax_flag						= TRUE
			cb_ok.enabled 									= TRUE
			lb_check_allow_payable_amount_entry 	= TRUE
			
	CASE 'recipient_type_code'
		
			// run the code at the bottom
			lb_check_scenario 								= TRUE
			lb_check_payment_method_code 			= TRUE
			lb_get_recipient_sub_type 					= TRUE
			lb_check_allow_tax_flag						= TRUE
			cb_ok.enabled 									= TRUE
			lb_check_allow_payable_amount_entry 	= TRUE

	CASE 'payment_method_code'
			/* •	Payment method for reimbursing – for each reimbursement recipient, the method of payment for that recipient */

END CHOOSE

/*  
	CASE 1  REIMBURSE THE PROVIDER 	(i)		Provider is the provider that provided the service/supply 		
	CASE 2  REIMBURSE PROVIDER ii)	Provider did not provide the service and clinic under contract
	CASE 3  REIMBURSE PROVIDER   iii)	Provider did not provide the service – clinic not under contract			
	CASE 4  REIMBURSE THE INJURED WORKER  i) Clinic is under contract
	CASE 5  REIMBURSE THE INJURED WORKER  ii)  Clinic is not under contract		
*/

ls_recipient_type 			= THIS.getitemstring(al_row, 'recipient_type_code')
ll_recipient_no 				= THIS.getitemnumber(al_row, 'recipient_no')
ll_provider_no_invoice    	= dw_rehab_invoice.getitemnumber(1, 'provider_no')
ls_provider_type_code 	= dw_rehab_invoice.getitemstring(1, 'provider_type_code')

IF lb_check_allow_tax_flag = TRUE THEN
	wf_change_tax_flag_on_provider_change(ll_recipient_no, ls_recipient_type,ll_provider_no_invoice, ls_provider_type_code )
END IF 

IF lb_check_payment_method_code = TRUE THEN
	wf_set_payment_method(ll_recipient_no, ls_recipient_type)
END IF 

IF lb_get_recipient_sub_type = TRUE THEN
	IF ls_recipient_type <> 'I' THEN 
		ls_recipient_subtype =  inv_ephysio_invoice.nf_get_recipient_subtype(ll_recipient_no, ls_recipient_type)
	ELSE
		ls_recipient_subtype = ''
	END IF
	
	THIS.setitem(al_row, 'recipient_sub_type_code', ls_recipient_subtype)
END IF

IF  lb_check_allow_payable_amount_entry 	= TRUE THEN
	wf_change_allow_payable_entry()	
END IF 

end event

type cb_refresh from commandbutton within w_physio_reimbursements
integer x = 1905
integer y = 1741
integer width = 336
integer height = 67
integer taborder = 120
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Cancel"
end type

event clicked;dw_rehab_invoice.reset()
dw_reimburse.reset()
dw_rehab_invoice_line_item.reset()

cb_ok.enabled = FALSE

//refresh the screen
wf_refresh_screen('INITIAL')


end event

type cb_resize from commandbutton within w_physio_reimbursements
integer x = 18
integer y = 1731
integer width = 80
integer height = 67
integer taborder = 21
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "^"
end type

event clicked;IF dw_reimburse.visible = TRUE THEN	
	
	tab_line_item.move(5, dw_reimburse.y)				
	dw_rehab_invoice_line_item.height 	= dw_rehab_invoice_line_item.height + dw_reimburse.height 
	dw_rehab_invoice_line_item.move(5, tab_line_item.y + tab_line_item.height + 20)
	dw_reimburse.visible 							= FALSE
ELSE
	dw_reimburse.visible 							= TRUE	
	tab_line_item.move(5,  ii_tab_resize)	
	dw_rehab_invoice_line_item.y 			= ii_dw_resize
	dw_rehab_invoice_line_item.height 	= dw_rehab_invoice_line_item.height - dw_reimburse.height 	
END IF 

THIS.bringtotop 								= TRUE

end event

