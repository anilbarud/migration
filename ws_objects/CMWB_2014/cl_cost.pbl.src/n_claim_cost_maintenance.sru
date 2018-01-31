$PBExportHeader$n_claim_cost_maintenance.sru
forward
global type n_claim_cost_maintenance from nonvisualobject
end type
end forward

global type n_claim_cost_maintenance from nonvisualobject
end type
global n_claim_cost_maintenance n_claim_cost_maintenance

type variables
u_ds						ids_maintenance
u_ds						ids_target_txn
u_ds						ids_batch_control
u_ds						ids_rehab_task_authorization

uo_br_exception		iuo_br_exception

n_tax_functions		iuo_tax_functions

STRING					is_module_code

string					is_benefit_type

boolean ib_suppress_message2
boolean ib_suppress_message3
boolean ib_suppress_message4
boolean ib_suppress_message5
boolean ib_suppress_message6
boolean ib_suppress_message7
boolean ib_suppress_message8

boolean ib_suppress_warnings

BOOLEAN ib_cancel_sp

n_claim_employer inv_claim_employer
end variables

forward prototypes
public function integer of_set_module (string as_module_code)
public function integer of_check_business_rules () throws uo_br_exception
public function integer of_new ()
public function integer of_check_general_batch_rules () throws uo_br_exception
public subroutine of_reset ()
public subroutine of_check_annuity_payout (long al_row) throws uo_br_exception
public function integer of_check_cancel_txn_batch_rules () throws uo_br_exception
public function integer of_check_claim_correction_rules (long al_row) throws uo_br_exception
public function integer of_check_cost_transfer_rules (long al_row) throws uo_br_exception
public function integer of_check_for_over_adjustments (long al_txn_no, ref decimal ad_sum_payment_type, ref decimal ad_sum_cost_alloc, ref string as_message)
public function integer of_check_general_txn_rules (long al_row) throws uo_br_exception
public function integer of_check_recovery_txn_rules (long al_row) throws uo_br_exception
private function integer of_check_target_txn_maintainability_rule (long al_row) throws uo_br_exception
public function integer of_check_tax_rules (long al_row) throws uo_br_exception
public function integer of_check_transfer_rules (long al_row) throws uo_br_exception
public function integer of_check_valid_coc_period (long al_coc_period) throws uo_br_exception
public function integer of_isvalid_cheque (long al_cheque_no, datetime adt_cheque_deposit_date) throws uo_br_exception
public function integer of_isvalid_claim_no (long al_claim_no) throws uo_br_exception
public function integer of_isvalid_cost_alloc (long al_cost_alloc_no, long al_cost_alloc_operation_no) throws uo_br_exception
public function integer of_isvalid_explanation (string as_explanation) throws uo_br_exception
public function integer of_isvalid_payment_no (long al_payment_no) throws uo_br_exception
public function integer of_isvalid_payment_type (string as_payment_type_code, string as_payment_sub_type_code) throws uo_br_exception
public function integer of_isvalid_txn_no (long al_txn_no) throws uo_br_exception
public function integer of_isvalid_txn_type (string as_txn_type_code, string as_txn_sub_type_code) throws uo_br_exception
public function integer of_retrieve_target_txn_info (long al_target_txn_no[])
public function integer of_check_payment_type_correction_rules (long al_row) throws uo_br_exception
public function integer of_check_manuals_for_over_adjustments (long al_row) throws uo_br_exception
public subroutine nf_check_for_ma_payment (long al_target_payment_no, long al_target_txn_no, long al_target_claim_no, long al_related_txn_no, string as_msg_type, string as_efb_type, string as_correction_type) throws uo_br_exception
public subroutine nf_check_for_efb_payment (long al_target_payment_no, long al_target_txn_no, long al_target_claim_no, string as_msg_type, string as_correction_type) throws uo_br_exception
public function integer of_isvalid_authorization_no (decimal adcm_sum_rehab_qty, long al_authorization_no_to, long al_to_claim_no, decimal adcm_sum_amount, long al_from_claim_no) throws uo_br_exception
end prototypes

public function integer of_set_module (string as_module_code);LONG			ll_count


IF as_module_code = '' or IsNull(as_module_code) Then
	SignalError(-666,'Invalid module code')
End if
	
SELECT count(*) into :ll_count
FROM Module
WHERE module_code = :as_module_code
USING SQLCA;

SQLCA.nf_handle_error('n_claim_cost_maintenance','of_set_module','select count(*)')

IF ll_count = 0 or IsNull(ll_count) Then
	SignalError(-666,'Invalid module code "' + String(as_module_code) + '"')
End if


is_module_code = as_module_code

RETURN 1
end function

public function integer of_check_business_rules () throws uo_br_exception;INTEGER		li_x

LONG			ll_target_txn_count
long			ll_coc_period

STRING		ls_new_txn_type_code
STRING		ls_new_txn_sub_type_code


/*{ Busness Rules are checked here }*/


//Get some info about the target transactions
ll_target_txn_count = ids_maintenance.RowCount()


IF  ll_target_txn_count = 0 Then
	RETURN -1
END IF


IF ids_batch_control.RowCount() <> 1 THEN
	SignalError(-666,'Expecting one batch control record.')
END IF


IF is_module_code = '' THEN
	SignalError(-666,'You must set the is_module_code before calling n_claim_cost_maintenance.of_check_business_rules()')
END IF



ls_new_txn_type_code = ids_batch_control.GetItemString(1,'new_txn_type_code')
ls_new_txn_sub_type_code = ids_batch_control.GetItemString(1,'new_txn_sub_type_code')

iuo_br_exception.reset_indicator()

		
/* wrap in try/catch */


TRY


	/*2.10	The type of maintenance to be perform on the payments must be identified by one of the following transaction types:
	    Adjustment
        Cost Transfer
 */
	of_IsValid_Txn_Type(ls_new_txn_type_code,ls_new_txn_sub_type_code)
	
	
	/* 1.10	The cost of claims period must be open.  The only open periods are the current period or period immediately prior to the current period. */
	
	// Make sure valid coc period 
	of_check_valid_coc_period(ids_batch_control.GetItemNumber(1,'coc_period'))
	
	
	//Make sure the required fields are filled in before validation business rules
	of_isvalid_explanation(ids_batch_control.GetItemString(1,'explanation'))

	
	//For transfers, all the transfer information must be filled in. 
	//Claim, cost, and payment type even if they are the same as the from side.
	IF ls_new_txn_type_code = 'T' Then
		
		This.of_isValid_Claim_no(ids_batch_control.GetItemNumber(1,'to_claim_no'))
			
		This.of_isValid_Payment_Type(ids_batch_control.GetItemString(1,'to_payment_type_code'),ids_batch_control.GetItemString(1,'to_payment_sub_type_code'))
			
		/* checking the from side of cost allocation/relief transfers */
		IF ls_new_txn_sub_type_code = '7' or ls_new_txn_sub_type_code = '8' then
			This.of_isValid_Cost_Alloc(ids_batch_control.GetItemNumber(1,'from_cost_alloc_no'),ids_batch_control.GetItemNumber(1,'from_cost_alloc_operation_no'))		
			This.of_isValid_Cost_Alloc(ids_batch_control.GetItemNumber(1,'to_cost_alloc_no'),ids_batch_control.GetItemNumber(1,'to_cost_alloc_operation_no'))
		end if
		
		/* checking the from side of payment type transfers */
		IF ls_new_txn_sub_type_code = '9'  then
			This.of_isValid_Payment_Type(ids_batch_control.GetItemString(1,'from_payment_type_code'),ids_batch_control.GetItemString(1,'from_payment_sub_type_code'))	
		end if
		

		/* checking the from side of a claim transfer */
		IF ls_new_txn_sub_type_code = '6'  then
			This.of_isValid_Claim_no(ids_batch_control.GetItemNumber(1,'from_claim_no'))
			
			/*  */
		    This.of_isvalid_authorization_no(ids_batch_control.GetItemDecimal(1,'sum_rehab_qty')	,ids_batch_control.GetItemNumber(1,'authorization_no_to'),ids_batch_control.GetItemNumber(1,'to_claim_no'),ids_batch_control.GetItemDecimal(1,'total_adjustment_txn_amount'),ids_batch_control.GetItemNumber(1,'from_claim_no'))
		   			  
		end if	

	End if
	
	
	//Retrieve all the information needed to validate business rules.
	// This datawindow takes an array of txn_no
	THIS.of_retrieve_target_txn_info(ids_maintenance.object.target_txn_no[1,ll_target_txn_count])
	
	//Order the datawindows the same so related records are in the same rows.
	ids_maintenance.SetSort('target_txn_no asc')
	ids_maintenance.Sort()
	ids_target_txn.SetSort('target_txn_no asc')
	ids_target_txn.Sort()
	


	
	//Check general rules
	For li_x = 1 to ll_target_txn_count
		
		This.of_check_target_txn_maintainability_rule(li_x)
		
		This.of_check_general_txn_rules(li_x)
		

		//Check adjustment rules
		IF ls_new_txn_type_code = 'J' then

			
			CHOOSE CASE ls_new_txn_sub_type_code
				CASE '3','5'
					This.of_check_recovery_txn_rules(li_x)	
			END CHOOSE
	
		
		//Check transfer rules
		Elseif ls_new_txn_type_code = 'T' THEN
			
				
			This.of_check_transfer_rules(li_x)
			
			CHOOSE CASE ls_new_txn_sub_type_code
				CASE '6'
					This.of_check_claim_correction_rules(li_x)
				CASE '7','8'
					This.of_check_cost_transfer_rules(li_x)
				CASE '9'
					This.of_check_payment_type_correction_rules(li_x)
			END CHOOSE
		
		End if	
	
	
	
	NEXT
	
	// after personal coverage & operation warnings have 
	// potentially been displayed for this batch, reset the n_claim_employer instance variables
	inv_claim_employer.nf_reset_variables()
	
	This.of_check_general_batch_rules()
	
	//Check cancelation rules outside of the loop because most of them are validated
	// as a batch, not one at a time.
	IF ls_new_txn_type_code = 'J' and ls_new_txn_sub_type_code = '2' Then
		This.of_check_cancel_txn_batch_rules()
	END IF
	
CATCH (uo_br_exception luo_br_exception)
	THROW luo_br_exception
END TRY

return 1
end function

public function integer of_new ();LONG		ll_row

ll_row = ids_maintenance.InsertRow(0)
IF ll_row = -1 Then
	SignalError(-666,'Error creating a new claim cost maintenance record.')
End If

RETURN ll_row
end function

public function integer of_check_general_batch_rules () throws uo_br_exception;DECIMAL{2}				ldec_sum_adjustment_txn_amount
DECIMAL{2}				ldec_batch_adjustment_txn_amount
LONG						li_x
LONG						ll_found
LONG						ll_auth_new_row
LONG						ll_current_total
LONG						ll_current_quantity_adj
LONG						ll_authorized_quantity
LONG						ll_adjustment_quantity
LONG						ll_paid_quantity
LONG						ll_unapplied_quantity_adj
LONG						ll_new_paid_quantity
long						ll_claim_no
long						ll_authorization_no
long						ll_adjustment_rehab_qty
long						ll_remaining_authorized_quantity
long						ll_sum_of_unapplied_authorized_qty
u_ds						lds_unapplied_days_hours_quantity
LONG						ll_payment_no_array[]
long						ll_unapplied_rows
LONG						ll_target_payment_no
LONG						ll_target_txn_no
LONG						ll_target_claim_no

//DAYS
DECIMAL{2}				ldec_ORIG_days
DECIMAL{2}				ldec_net_days
DECIMAL{2}				ldec_NEW_net_days
DECIMAL{2}				ldec_SUM_unapplied_days_adj
DECIMAL{2}				ldec_SUM_current_days_adj
DECIMAL{2}				ldec_transfered_days
DECIMAL{2}				ldec_remaining_payment_days

//HOURS
DECIMAL{2}				ldec_ORIG_hours
DECIMAL{2}				ldec_net_hours
DECIMAL{2}				ldec_NEW_net_hours
DECIMAL{2}				ldec_SUM_unapplied_hours_adj
DECIMAL{2}				ldec_SUM_current_hours_adj
DECIMAL{2}				ldec_transfered_hours
DECIMAL{2}				ldec_remaining_payment_hours

//QUANTITY
INTEGER					li_paid_quantity
INTEGER					li_net_quantity
INTEGER					li_NEW_net_quantity
INTEGER					li_SUM_unapplied_quantity_adj
INTEGER					li_SUM_current_quantity_adj
INTEGER					li_remaining_authorized_quantity
INTEGER					li_transfered_and_corrected_quantity
INTEGER					li_remaining_payment_quantity

//PAYMENT
DECIMAL{2}				ldec_net_payment
DECIMAL{2}				ldec_SUM_unapplied_payment_adj
DECIMAL{2}				ldec_SUM_current_payment_adj
DECIMAL{2}				ldec_NEW_net_payment
DECIMAL{2}				ldec_total_payment_amount

STRING					ls_find
STRING					ls_new_txn_type_code
STRING					ls_new_txn_sub_type_code
LONG						ll_found_row
INTEGER					li_rtn


iuo_br_exception.reset_indicator()


ldec_batch_adjustment_txn_amount = ids_batch_control.GetItemDecimal(1,'total_adjustment_txn_amount')

FOR li_x = 1 to ids_maintenance.RowCount()
	ldec_sum_adjustment_txn_amount += ids_maintenance.GetItemDecimal(li_x,'adjustment_txn_amount')
NEXT

IF ldec_sum_adjustment_txn_amount <> ldec_batch_adjustment_txn_amount THEN
	iuo_br_exception.SetMessage('The sum of the transaction adjustments must equal ' + String(ldec_batch_adjustment_txn_amount,'$#,##0.00'))
	THROW iuo_br_exception
END IF


ls_new_txn_type_code			= ids_batch_control.GetItemString(1,'new_txn_type_code')
ls_new_txn_sub_type_code	= ids_batch_control.GetItemString(1,'new_txn_sub_type_code')

		
	
/* 7.30	The quantity must not exceed the remaining authorized quantity on the rehab authorization. */
For li_x = 1 to ids_maintenance.RowCount()
	
	//If the current row has an authorization, add a new row if that authorization
	//doesn't already exist in the authorization datastore or add the authorization
	//adjustment to the total if the authorization number already has a row.
	ll_authorization_no = ids_target_txn.GetItemNumber(li_x,'authorization_no')
	
	
	IF ll_authorization_no > 0 THEN
		
		//Get the amount being adjusted.
		ll_adjustment_quantity = ids_maintenance.GetItemNumber(li_x,'adjustment_quantity')
		
		/* holds rehab info for br 7.30 - check remain auth rehab qty */
		ls_find = "authorization_no = " + string(ll_authorization_no)
		ll_found = ids_rehab_task_authorization.Find(ls_find,1, ids_rehab_task_authorization.RowCount() + 1)
		
		//If the authorization doesn't already have a row, add one.
		if ll_found = 	0 then
			ll_auth_new_row = ids_rehab_task_authorization.insertrow(0)
			
			ids_rehab_task_authorization.setitem(ll_auth_new_row,'authorization_no',ll_authorization_no)
			ids_rehab_task_authorization.setitem(ll_auth_new_row,'adjustment_rehab_qty',ll_adjustment_quantity)
		else
			//If the authorization already has a row, add the adjustment amount to the total
			//adjustment amount
			ll_current_total = ids_rehab_task_authorization.getitemnumber(ll_found,'adjustment_rehab_qty')
			ids_rehab_task_authorization.setitem(ll_found,'adjustment_rehab_qty',  ll_current_total + ll_adjustment_quantity)
		end if
	End if
Next



FOR li_x = 1 to ids_rehab_task_authorization.rowcount()
	
	ll_authorization_no 			=  ids_rehab_task_authorization.getitemnumber(li_x,'authorization_no')
	ll_current_quantity_adj		=  ids_rehab_task_authorization.getitemnumber(li_x,'adjustment_rehab_qty')
	

	
	
	SELECT authorized_quantity,
	       paid_quantity 
	INTO   :ll_authorized_quantity,
	       :ll_paid_quantity
	FROM  REHAB_TASK_AUTHORIZATION
	WHERE authorization_no = :ll_authorization_no 
	USING SQLCA;
	
	SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_general_batch_rules','SELECT authorized_quantity - paid_quantity into :ll_remaining_authorized_quantity')
	
	IF SQLCA.SQLNRows = 0 THEN
		SignalError(-666,'Problem finding  authorization_no.' + string(ll_authorization_no) )
	END IF
	

	 /* what's in the UNAPPLIED_CLAIM_TXN ?*/
	 SELECT isnull(sum(dbo.ADJUSTMENT_TXN_WORK_TABLE.adjustment_quantity),0)  
    INTO   :ll_unapplied_quantity_adj
    FROM dbo.ADJUSTMENT_TXN_WORK_TABLE,   
         dbo.PAYMENT,   
         dbo.UNAPPLIED_CLAIM_TXN  
   WHERE ( dbo.ADJUSTMENT_TXN_WORK_TABLE.payment_no = dbo.PAYMENT.payment_no ) and  
         ( dbo.PAYMENT.payment_no = dbo.UNAPPLIED_CLAIM_TXN.payment_no ) and  
         ( dbo.PAYMENT.claim_no = :ll_claim_no ) AND  
         ( dbo.PAYMENT.authorization_no = :ll_authorization_no) ;
			
	SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_general_batch_rules','SELECT dbo.ADJUSTMENT_TXN_WORK_TABLE.adjustment_quantity from ADJUSTMENT_TXN_WORK_TABLE')

	ll_new_paid_quantity = ll_paid_quantity + ll_unapplied_quantity_adj + ll_current_quantity_adj
	
	IF ll_new_paid_quantity < 0 Then
		iuo_br_exception.SetMessage('The rehad authorizations paid quantity must not be adjusted below zero. Authorization #' + String(ll_authorization_no) + ' has a paid quantity of ' + String(ll_paid_quantity) + '.')
		THROW iuo_br_exception
	end if
	
	IF ll_new_paid_quantity > ll_authorized_quantity THEN
		iuo_br_exception.SetMessage('The rehab authorizations paid quantity must not exceed the authorized quantity . ~r~nAuthorization #' + String(ll_authorization_no) + ' has an authorized amount of ' + String(ll_authorized_quantity) + ' and your adjustments plus oustanding adjustment would result in the paid quantity being ' + String(ll_new_paid_quantity) + '.')
		THROW iuo_br_exception
	end if	

next

//Must reset the datastore or the next call to this function will add the adjustment amounts
//again and again and again....
ids_rehab_task_authorization.Reset()


//Get the days hours and quantity for transactions related to the selected payments that
//are still sitting in the UNAPPLIED
lds_unapplied_days_hours_quantity = create u_ds
lds_unapplied_days_hours_quantity.dataobject = 'd_unapplied_days_hours_quantity'
lds_unapplied_days_hours_quantity.SetTransObject(SQLCA)


ll_payment_no_array = ids_maintenance.object.payment_no.Primary
ll_unapplied_rows = lds_unapplied_days_hours_quantity.Retrieve(ll_payment_no_array)

SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_general_batch_rules','Retrieve ADJUSTMENT_TXN_WORK_TABLE records')

IF ll_unapplied_rows < 0 Then
	SignalError(-666,'Error retriving ADJUSTMENT_TXN_WORK_TABLE records.')
End if


//*********************************//
//    DAYS, HOUR AND QUANTITY      //
//*********************************//

//In order to validate the days, hours and quantity rules we must consider
//the NET amounts on the PAYMENT , any adjustments in the UNAPPLIED and 
//all adjustments currently being processed (could be multiple per payment)

//Loop through all the target transactions
//Calculate the NEW net amounts Using the equation
//NEW_Net = Current_Net + Unapplied Adjustment + New Adjustments
For li_x = 1 to ids_target_txn.RowCount()
	ll_target_payment_no 		= ids_target_txn.GetItemNumber(li_x,'target_payment_no')
	ldec_net_days 		= ids_target_txn.GetItemDecimal(li_x,'net_days_lost')
	ldec_ORIG_days		= ids_target_txn.GetItemDecimal(li_x,'paid_days_lost')
	ldec_net_hours 	= ids_target_txn.GetItemDecimal(li_x,'net_hours_lost')
	ldec_ORIG_hours 	= ids_target_txn.GetItemDecimal(li_x,'paid_hours_lost')
	li_net_quantity 	= ids_target_txn.GetItemNumber(li_x,'net_quantity')
	ldec_net_payment 	= ids_target_txn.GetItemDecimal(li_x,'net_payment_amount')
	ll_target_txn_no		=  ids_target_txn.GetItemNumber(li_x,'target_txn_no')
	ll_target_claim_no	=  ids_target_txn.GetItemNumber(li_x,'claim_no')
	ldec_total_payment_amount =  ids_target_txn.GetItemDecimal(li_x,'total_payment_amount')


	//Initialize the vairables each time through the loop
	ldec_SUM_unapplied_days_adj 		=0
	ldec_SUM_unapplied_hours_adj 		=0
	li_SUM_unapplied_quantity_adj		    =0
	ldec_SUM_unapplied_payment_adj	=0
	ldec_SUM_current_days_adj 			=0
	ldec_SUM_current_hours_adj			=0
	li_SUM_current_quantity_adj	      	=0
	ldec_SUM_current_payment_adj		=0
	
	
	IF ll_unapplied_rows > 0 THEN
		//FIND UNAPPLIED txns and adjust the net by that amount
		ls_find = 'payment_no = ' + String(ll_target_payment_no)
		ll_found_row = lds_unapplied_days_hours_quantity.Find(ls_find,1,ll_unapplied_rows + 1)
		IF ll_found_row  < 0 Then
			SignalError(-666,'Error finding unapplied row')
		Elseif ll_found_row > 0 THEN
			
			ldec_SUM_unapplied_days_adj 				= lds_unapplied_days_hours_quantity.GetItemDecimal(ll_found_row,'sum_adjustment_days_lost')
			ldec_SUM_unapplied_hours_adj 				= lds_unapplied_days_hours_quantity.GetItemDecimal(ll_found_row,'sum_adjustment_hours_lost')
			li_SUM_unapplied_quantity_adj				= lds_unapplied_days_hours_quantity.GetItemNumber(ll_found_row,'sum_adjustment_quantity')
			ldec_SUM_unapplied_payment_adj			= lds_unapplied_days_hours_quantity.GetItemDecimal(ll_found_row,'sum_unapplied_txn_amount')

		End if
	END IF
		
	//Adjust all the net amounts with the adjustment amounts that are being processed.
	Do
		ls_find = 'target_payment_no = ' + String(ll_target_payment_no)
		ll_found_row = ids_maintenance.Find(ls_find,ll_found_row + 1,ids_maintenance.RowCount() + 1)
		IF ll_found_row  < 0 Then
			SignalError(-666,'Error finding unapplied row')
		Elseif ll_found_row > 0 THEN
				
			ldec_SUM_current_days_adj 					+= ids_maintenance.GetItemDecimal(ll_found_row,'adjustment_days')		
			ldec_SUM_current_hours_adj 				+= ids_maintenance.GetItemDecimal(ll_found_row,'adjustment_hours')
			li_SUM_current_quantity_adj 				+= ids_maintenance.GetItemNumber(ll_found_row,'adjustment_quantity')
			ldec_SUM_current_payment_adj				+= ids_maintenance.GetItemDecimal(ll_found_row,'adjustment_txn_amount')
		End if
	Loop until ll_found_row = 0
	
	//No days and payment so you can't adjust days
	IF ldec_ORIG_days = 0 and ldec_SUM_current_days_adj <> 0 THEN
		iuo_br_exception.SetMessage('There are no days on the payment to adjust.  See payment #' + String(ll_target_payment_no))
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception
	END IF

	//No hours on payment so you can't adjust hours
	IF ldec_ORIG_hours = 0 and ldec_SUM_current_hours_adj <> 0 THEN
		iuo_br_exception.SetMessage('There are no hours on the payment to adjust.  See payment #' + String(ll_target_payment_no))
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception
	END IF
	
	ldec_NEW_net_days 	= ldec_net_days + ldec_SUM_unapplied_days_adj + ldec_SUM_current_days_adj
	ldec_NEW_net_hours 	= ldec_net_hours + ldec_SUM_unapplied_hours_adj + ldec_SUM_current_hours_adj
	li_NEW_net_quantity	= li_net_quantity + li_SUM_unapplied_quantity_adj + li_SUM_current_quantity_adj
	ldec_NEW_net_payment = ldec_net_payment + ldec_SUM_unapplied_payment_adj + ldec_SUM_current_payment_adj

	//Use the new NET amounts to validate the days, hours and quantity rules
	
	//BR 4.60 is handled in of_check_general_txn_rules
					
		
		//BR 4.140
	/*		The quantity on a payment must not be less than zero.		
	*/
	IF li_NEW_net_quantity < 0 THEN
		iuo_br_exception.SetMessage('The payment quantity cannot be less than zero. See payment #' + String(ll_target_payment_no) )
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)	
		THROW iuo_br_exception
	END IF
	
	//BR 4.200
	/*		The days on a payment must not be less than zero.		
	*/
	IF  ldec_NEW_net_days < 0 THEN
		iuo_br_exception.SetMessage('The payment days cannot be less than zero. See payment #' + String(ll_target_payment_no) )
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)	
		THROW iuo_br_exception
	END IF
	
	//BR 4.220
	/*		The hours on a payment must not be less than zero.		
	*/
	IF ldec_NEW_net_hours < 0 THEN
		iuo_br_exception.SetMessage('The payment hours cannot be less than zero. See payment #' + String(ll_target_payment_no) )
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)	
		THROW iuo_br_exception
	END IF
	
	
	/*PAYMENT AMOUNT = 0
	*/
	
	//BR 4.100
	//The payment quantity must be zero if the payment amount is zero.
	IF  ldec_NEW_net_payment = 0 and li_NEW_net_quantity <> 0 THEN
		iuo_br_exception.SetMessage('When the payment amount is reduced to zero, the quantity must also be reduced to zero.  See payment #' + String(ll_target_payment_no))
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)	
		THROW iuo_br_exception
	END IF
	
	//BR 4.230
	// The days lost must be zero if the payment amount is zero.
	IF  ldec_NEW_net_payment = 0 and ldec_NEW_net_days <> 0 THEN
		iuo_br_exception.SetMessage('When the payment amount is reduced to zero, the days lost must also be reduced to zero.  See payment #' + String(ll_target_payment_no))
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception
	END IF
	
	//BR 4.230
	// The hours lost must be zero if the payment amount is zero.
	IF  ldec_NEW_net_payment = 0 and ldec_NEW_net_hours <> 0.00 THEN
		iuo_br_exception.SetMessage('When the payment amount is reduced to zero, the hours lost must also be reduced to zero.  See payment #' + String(ll_target_payment_no))
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)	
		THROW iuo_br_exception
	END IF
	
	
	li_paid_quantity = ids_target_txn.GetItemNumber(li_x,'paid_quantity')
		
	
	/* check to see if warning should be checked */
	if ib_suppress_warnings = false then
		
		/* check to see if user has already seen the message once and click on yes to all */
		IF ib_suppress_message2 = FALSE THEN
			
			/*	QUANTITY = 0
			*/
			//BR 4.120
			/*		The quantity of a payment SHOULD not be zero unless the payment amount 
				is zero if the original quantity is greater than zero.
			*/
				
			IF li_paid_quantity > 0 Then
				
				IF  li_NEW_net_quantity = 0 and ldec_NEW_net_payment <> 0 THEN

					li_rtn = f_produce_messagebox('You have reduced the quantity to zero, for payment #' + String(ll_target_payment_no) + ', without reducing the payment amount to zero.')
		
					IF li_rtn = 2 Then 
						iuo_br_exception.SetMessage('')
						iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
						THROW iuo_br_exception
					ELSEIF li_rtn = 3 then
						ib_suppress_message2 = TRUE
					end if
					
		
				END IF
				
			END IF
		END IF
	end if
	
		
	/* DAYS AND HOURS = 0
	*/
	//BR 4.240 A payment amount must be adjusted to zero if both the days and hours are adjusted to zero.	
	IF (ldec_ORIG_days > 0 OR ldec_ORIG_hours > 0 ) and (ldec_NEW_net_days = 0 and ldec_NEW_net_hours = 0) and ldec_NEW_net_payment <> 0 THEN
		iuo_br_exception.SetMessage('When the days and hours are reduced to zero, the payment must also be reduced to zero.  See payment #' + String(ll_target_payment_no))
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)	
		THROW iuo_br_exception
	END IF
	

//BR 4.350 The paid days lost and paid hours lost on a payment must be adjusted if the payment amount is adjusted except for overpayment recovery involving an overpaid benefit level.
//BR 4.360 The paid days lost and paid hours lost on a payment must not be adjusted for overpayment recovery involving an overpaid benefit level.
	IF ls_new_txn_type_code	= 'J' AND ls_new_txn_sub_type_code = '3' and is_benefit_type = 'L' then
		
		/* BR 4.360 */
		IF ldec_SUM_current_payment_adj <> 0 and (ldec_SUM_current_days_adj <> 0 or ldec_SUM_current_hours_adj <> 0) then
			iuo_br_exception.SetMessage('The paid days lost and paid hours lost on a payment must not be adjusted for overpayment recovery involving an overpaid benefit level.. See Payment #' + String(ll_target_payment_no) + " .")
			iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)	
			THROW iuo_br_exception
		end if

	else
		/* BR 4.350 */
		IF ldec_SUM_current_payment_adj <> 0 and (ldec_SUM_current_days_adj =0 and ldec_SUM_current_hours_adj = 0) and (ldec_ORIG_days > 0 OR ldec_ORIG_hours > 0 )THEN
			iuo_br_exception.SetMessage('The paid days lost and paid hours lost on a payment must be adjusted if the payment amount is adjusted except for overpayment recovery involving an overpaid benefit level. See Payment #' + String(ll_target_payment_no) + " .")
			iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)	
			THROW iuo_br_exception
		end if	
	end if
	


ll_authorization_no = ids_target_txn.GetItemNumber(li_x,'authorization_no')


	IF ll_authorization_no > 0 THEN
		//Get the remaining authorized quantity
		SELECT Convert(Int,authorized_quantity) - Convert(Int,paid_quantity )
		into :li_remaining_authorized_quantity
		FROM REHAB_TASK_AUTHORIZATION
		WHERE authorization_no = :ll_authorization_no
		USING SQLCA;
		
		SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_general_txn_rules','SELECT authorized_quantity - paid_quantity into :ll_remaining_authorized_quantity')
		
		
		IF SQLCA.SQLNRows = 0 THEN
			SignalError(-666,'Invalid authorization_no.')
		END IF
		
		//Get the quantity that has been transfered to another payment or corrected
		SELECT IsNull(SUM(adjustment_quantity),0)
		INTO :li_transfered_and_corrected_quantity
		FROM   ADJUSTMENT_TXN_WORK_TABLE a,
				 APPLIED_CLAIM_TXN b
		WHERE a.txn_no = b.txn_no 
		  AND b.payment_no = :ll_target_payment_no
		  AND (    (b.txn_type_code = 'T' and txn_sub_type_code in('6','9'))
				  OR (b.txn_type_code = 'J' and txn_sub_type_code = 'Q'  )
				)
		USING SQLCA;		
		
		SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_general_txn_rules','SELECT IsNull(SUM(adjustment_quantity),0)')
	End if
	
		
	if ldec_SUM_current_days_adj	<> 0 then
		SELECT IsNull(SUM(adjustment_days_lost),0) INTO :ldec_transfered_days
		FROM   ADJUSTMENT_TXN_WORK_TABLE a,
				 APPLIED_CLAIM_TXN b
		WHERE a.txn_no = b.txn_no 
		  AND b.payment_no = :ll_target_payment_no
		  AND (    (b.txn_type_code = 'T' and txn_sub_type_code in(6,9)))
		USING SQLCA;
		
		SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_general_txn_rules','SELECT  SUM(adjustment_days_lost) into :ldec_transfer_days')	
	end if
			
			
	if ldec_SUM_current_days_adj <> 0 then
		SELECT IsNull(SUM(adjustment_hours_lost),0) INTO :ldec_transfered_hours
		FROM   ADJUSTMENT_TXN_WORK_TABLE a,
				 APPLIED_CLAIM_TXN b
		WHERE a.txn_no = b.txn_no 
		  AND b.payment_no = :ll_target_payment_no
		  AND (    (b.txn_type_code = 'T' and txn_sub_type_code in(6,9)))
		USING SQLCA;
		
		SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_general_txn_rules','SELECT  SUM(adjustment_days_hours) into :ldec_transfer_hours')	
	end if	


	//BR 4.150
	/*		Excluding adjustments for Rehab Quantity Correction, the quantity must not be greater than 
		the remaining payment quantity or the remaining authorized quantity (authorized qty - paid qty).  
	
	The remaining payment quantity is:
			·	The payment's original quantity
			·	Minus any quantity that has been transferred to another payment (via transfer - payment type correction or claim correction) 
			·	Plus any quantity that has been corrected (via adjustment - rehab quantity correction - positively or negatively adjusted).
	*/
	IF NOT(ls_new_txn_type_code = 'J' and ls_new_txn_sub_type_code = 'Q') THEN
		
		
		li_remaining_payment_quantity = li_paid_quantity + li_transfered_and_corrected_quantity
		
		
		IF li_NEW_net_quantity > li_remaining_payment_quantity THEN
			iuo_br_exception.SetMessage('The quantity must not be greater than the remaining payment quantity. Payment #' + String(ll_target_payment_no) + ' has a remaining quantity of ' + String(li_remaining_payment_quantity) + '.')
			iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)	
			THROW iuo_br_exception
		END IF
	END IF
	
		
	

	
	//BR 4.270
	ldec_remaining_payment_days = ldec_ORIG_days	 + ldec_transfered_days
	
	if ldec_NEW_net_days > ldec_remaining_payment_days then
		iuo_br_exception.SetMessage('The days lost must not be greater than the remaining payment days lost. Payment #' + String(ll_target_payment_no) + ' has a remaining days of ' + String(ldec_remaining_payment_days) + '.')
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)	
		THROW iuo_br_exception
	END IF
	
	
	//BR 4.280
	ldec_remaining_payment_hours = ldec_ORIG_hours	 + ldec_transfered_hours 
	
	if ldec_NEW_net_hours > ldec_remaining_payment_hours then
		iuo_br_exception.SetMessage('The hour lost must not be greater than the remaining payment hours lost. Payment #' + String(ll_target_payment_no) + ' has a remaining hours of ' + String(ldec_remaining_payment_hours) + '.')
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)	
		THROW iuo_br_exception
	END IF


	//BR4.285
	/* The paid days lost and paid hours lost must be adjusted up to the remaining amount when the payment amount is adjusted up to the remaining amount. */
	if (ldec_NEW_net_payment = ldec_total_payment_amount) and (ldec_batch_adjustment_txn_amount > 0) then
			
		if ldec_remaining_payment_days <> ldec_NEW_net_days then
			iuo_br_exception.SetMessage('The paid days lost must be adjusted UP TO the remaining paid days lost when the payment amount is adjusted up to the remaining amount. Payment #' + String(ll_target_payment_no) + '.~n~n' &
												+ 'The remaining paid days lost for the payment is ' + String(ldec_remaining_payment_days) + '.' )
			iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)	
			THROW iuo_br_exception
		end if
		
		if ldec_remaining_payment_hours <> ldec_NEW_net_hours then
			iuo_br_exception.SetMessage('The paid hours lost must be adjusted UP TO the remaining paid hours lost when the payment amount is adjusted up to the remaining amount. Payment #' + String(ll_target_payment_no) +  '.~n~n' &
												+ 'The remaining paid hours lost for the payment is ' + String(ldec_remaining_payment_hours) + '.' )
			iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
			THROW iuo_br_exception
		end if
		
	end if

Next

return 1

end function

public subroutine of_reset ();ids_maintenance.reset()
ids_target_txn.reset()
ids_batch_control.reset()
ids_rehab_task_authorization.reset()


ib_suppress_message2 = false
ib_suppress_message3 = false
ib_suppress_message4 = false
ib_suppress_message5 = false
ib_suppress_message6 = false
ib_suppress_message7 = false
ib_suppress_message8 = false


end subroutine

public subroutine of_check_annuity_payout (long al_row) throws uo_br_exception;long 	ll_count
long 	ll_claim_no
long 	ll_individual_no
long	ll_no_credit_memo
long	ll_no_debit_memo
long 	ll_target_payment_no
long 	ll_target_txn_no

decimal{2} ldc_total_payment_amount_credit
decimal{2} ldc_total_payment_amount_debit

string ls_target_payment_type_code

INTEGER li_rtn


//TARGET TXN DATA
ll_target_payment_no					= ids_target_txn.GetItemNumber(al_row,'target_payment_no')
ll_target_txn_no						= ids_target_txn.GetItemNumber(al_row,'target_txn_no')
ls_target_payment_type_code		= ids_target_txn.GetItemString(al_row,'payment_type_code')
ll_claim_no								= ids_target_txn.GetItemnumber(al_row,'claim_no')
ll_individual_no						= ids_target_txn.GetItemnumber(al_row,'recipient_no')


iuo_br_exception.reset_indicator()

/* BR 4.310 */
/* if payment is eligible for annuity - cannot be adjusted if the annuity has been paid out */
/* check make sure payment type is eligible */
	
/* check to see if warning should be check */	
IF ib_suppress_warnings = FALSE THEN

	/* check to see if warning needs to be checked if user has already seen the message and hit yes to all , they 
	won't see the message for the next payment if one is found */
	IF ib_suppress_message5 = FALSE THEN
			
		SELECT COUNT(*)
		INTO   :ll_count
		FROM   Payment_Type
		WHERE  annuity_flag = 'Y'
		AND    payment_type_code = :ls_target_payment_type_code
		USING SQLCA;
		SQLCA.nf_handle_error('n_claim_cost_maintenance', '', 'of_check_general_txn_rules - Embedded SQL:Select from Payment Type') 
		
		IF ll_count > 0 THEN
				
			SELECT isnull(sum(A.total_payment_amount),0.00),
			       isnull(count(*),0)
			INTO   :ldc_total_payment_amount_credit,
			       :ll_no_credit_memo
			FROM   PAYMENT           a
			JOIN   APPLIED_CLAIM_TXN b ON a.payment_no = b.payment_no
			WHERE  a.payment_type_code     = '97'
			AND    a.payment_sub_type_code IN ('CM','CO','CW')
			AND    a.claim_no              = :ll_claim_no
			AND    b.recipient_no          = :ll_individual_no
  			AND NOT EXISTS ( SELECT *
    		                 FROM   ANNUITY_SETASIDE_PRE_1993 c
		   					  WHERE  c.setaside_payment_no = a.payment_no )
			USING SQLCA;
			SQLCA.nf_handle_error('n_claim_cost_maintenance', 'Embedded SQL:Select from PAYMENT, APPLIED_CLAIM_TXN (CM)', 'of_check_general_txn_rules') 
			
			
			/* payout exist - rows > 0 */
			if ll_no_credit_memo > 0 then	
				/* check to see if pay out reversal exist*/
				SELECT IsNull(sum(A.total_payment_amount),0.00),
				       IsNull(count(*),0)
				INTO   :ldc_total_payment_amount_debit,
				       :ll_no_debit_memo
				FROM   PAYMENT           a
				JOIN   APPLIED_CLAIM_TXN b ON a.payment_no = b.payment_no
				WHERE  a.payment_type_code     = '97'
				AND    a.payment_sub_type_code IN ('DM','DO','DW')
				AND    a.claim_no              = :ll_claim_no
				AND    b.recipient_no          = :ll_individual_no
   			AND NOT EXISTS ( SELECT *
	    		                 FROM   ANNUITY_SETASIDE_PRE_1993 c
			   					  WHERE  c.setaside_payment_no = a.payment_no )
				USING SQLCA;
				SQLCA.nf_handle_error('n_claim_cost_maintenance', 'Embedded SQL:Select from PAYMENT, APPLIED_CLAIM_TXN (CM)', 'of_check_general_txn_rules') 
				
				/* check the amount payout reversal */
				IF ll_no_debit_memo > 0 THEN
					
					/* if the payout and reversal do not match then adjustment can be made */
					IF ldc_total_payment_amount_debit + ldc_total_payment_amount_credit = 0.00 THEN
						// reversed, so adjustment is allowed, so no message
					ELSE
						// not completely reversed, i.e. not equal to zero, so display warning.
						// paid out, partially paid out, or over-reversed.
						li_rtn = f_produce_messagebox('A payment transaction that is eligible for annuity should not be maintained once the annuity has been paid out or written off.  See payment # '  + String(ll_target_payment_no) +  ' .')
		
						IF li_rtn = 2 Then 
							iuo_br_exception.SetMessage('')
							iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_claim_no)
							THROW iuo_br_exception
						ELSEIF li_rtn = 3 then
							ib_suppress_message5 = TRUE
						END IF
		
					END IF
				ELSE
					li_rtn = f_produce_messagebox('A payment transaction that is eligible for annuity should not be maintained once the annuity has been paid out or written off.  See payment # '  + String(ll_target_payment_no) +  ' .')
		
					IF li_rtn = 2 Then 
						iuo_br_exception.SetMessage('')
						iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_claim_no)
						THROW iuo_br_exception
					ELSEIF li_rtn = 3 then
						ib_suppress_message5 = TRUE
					END IF
					
				END IF
			END IF
		END IF
	END IF
END IF


end subroutine

public function integer of_check_cancel_txn_batch_rules () throws uo_br_exception;
INTEGER			li_x
LONG				ll_row
DATETIME			ldt_cheque_deposit_date
DATETIME			ldt_previous_cheque_deposit_date

DECIMAL{2}		ldec_cheque_deposit_amount
DECIMAL{2}		ldec_cheque_remaining_amount
DECIMAL{2}		ldec_claim_cheque_amount
DECIMAL{2}		ldec_other_claim_cheque_amount
DECIMAL{2}		ldec_adjusted_amount
DECIMAL{2}		ldec_canceled_amount
DECIMAL{2}		ldec_sum_adjustment
DECIMAL{2}		ldec_txn_amount
STRING			ls_payment_method_code
STRING			ls_previous_payment_method_code
LONG				ll_cheque_deposit_no
LONG				ll_previous_cheque_deposit_no
STRING			ls_txn_type_code
LONG				ll_row_count
LONG				ll_claim_no
LONG				ll_cheque_txn_claim_no
u_ds				lds_cheque_deposit_list
s_window_message	ls_window_message
BOOLEAN			lb_cancel_OK
BOOLEAN			lb_more_claims_to_cancel
STRING			ls_cheque_deposit_desc
LONG				ll_recipient_no
long				ll_target_txn_no
long				ll_target_payment_no
long				ll_target_claim_no
STRING          ls_recipient_type_code
decimal ldec_sum_qty_adjustments

iuo_br_exception.reset_indicator()

ll_claim_no 							= ids_batch_control.GetItemNumber(1,'from_claim_no')

ll_row_count = ids_target_txn.RowCount()

//Loop through all the target transactions and cheque some business rules
//Make sure all the payment method codes are the same
FOR li_x = 1 to ll_row_count
	
	
	//BR 6.30
	ls_payment_method_code 	= ids_target_txn.GetItemString(li_x,'payment_method_code')
	ll_target_payment_no 	= ids_target_txn.GetItemNumber(li_x,'target_payment_no')
	ll_target_txn_no			= ids_target_txn.GetItemNumber(li_x,'target_txn_no')
	ll_target_claim_no			= ids_target_txn.GetItemNumber(li_x,'claim_no')
	
	CHOOSE CASE ls_payment_method_code 
		CASE 'A','H'
			ll_cheque_deposit_no = ids_target_txn.GetItemNumber(li_x,'cheque_no')
			ls_cheque_deposit_desc = 'cheque'
		CASE 'D'
			ll_cheque_deposit_no = ids_target_txn.GetItemNumber(li_x,'direct_deposit_xmit_no')
			ls_cheque_deposit_desc = 'direct deposit'
		CASE ELSE
			iuo_br_exception.SetMessage('Only "Automatic", "Handwritten" and "Direct Deposit" are valid payment methods for cancelations.')
			iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
			THROW iuo_br_exception
	END CHOOSE
	
	
	ll_recipient_no = ids_target_txn.GetItemNumber(li_x,'recipient_no')
	//BR 6.70 Atlantic Blue Cross Care cheques can't be canceled.
	IF ll_recipient_no = 5500 Then
		iuo_br_exception.SetMessage('A cheque payable to Atlantic Blue Cross Care must not be cancelled.')
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception
	END IF
	
	
	//BR 6.50  Cheque number and direct deposit number must not be zero
	IF ll_cheque_deposit_no = 0 THEN
		iuo_br_exception.SetMessage('The ' + ls_cheque_deposit_desc + ' number cannot be zero.')
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception
	END IF 
	
	
	ldt_cheque_deposit_date		= ids_target_txn.GetITemDateTime(li_x,'cheque_deposit_date')
	//BR 6.50  Cheque deposit date must be fill in
	IF IsNull(ldt_cheque_deposit_date ) THEN
		iuo_br_exception.SetMessage('Cheque/Deposit issue date is required when canceling a Cheque or Direct Deposit.')
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception
	End if
		
	
	
	//Payment method, cheque_no, direct_deposit_xmit_no and cheque_deposit_date must
	//be the same for all transactions.
	If li_x >=2 THEN	
		IF ls_payment_method_code <> ls_previous_payment_method_code THEN
			iuo_br_exception.SetMessage('All payment method codes must be the same when canceling a ' + ls_cheque_deposit_desc + '.')
			iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
			THROW iuo_br_exception
		END IF
		
		IF ll_cheque_deposit_no <> ll_previous_cheque_deposit_no Then
			iuo_br_exception.SetMessage('All ' + ls_cheque_deposit_desc + ' numbers must be the same when canceling a ' + ls_cheque_deposit_desc + '.')
			iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
			THROW iuo_br_exception
		END IF	
		
		IF ldt_cheque_deposit_date <> ldt_previous_cheque_deposit_date THEN
			iuo_br_exception.SetMessage('All cheque deposit dates must be the same when canceling a ' + ls_cheque_deposit_desc + '.')
			iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)	
			THROW iuo_br_exception
		END IF
	End if
		
	ls_previous_payment_method_code 	= ls_payment_method_code
	ll_previous_cheque_deposit_no 	= ll_cheque_deposit_no
	ldt_previous_cheque_deposit_date = ldt_cheque_deposit_date
	
	//Get the sum of all the adjustment_txn_amount(s)	
	ldec_sum_adjustment += ids_maintenance.GetItemDecimal(li_x,'adjustment_txn_amount')	
	
	ldec_sum_qty_adjustments += ids_maintenance.GetItemDecimal(li_x,'adjustment_txn_amount')	
	
	
NEXT



lds_cheque_deposit_list = CREATE u_ds
	
IF ls_payment_method_code = 'A' or ls_payment_method_code = 'H' THEN
	lds_cheque_deposit_list.dataobject = 'd_cheque_txn_list'
	lds_cheque_deposit_list.SetTransObject(SQLCA)
	ll_row = lds_cheque_deposit_list.Retrieve(ll_cheque_deposit_no,ldt_cheque_deposit_date)
	ls_cheque_deposit_desc = 'cheque'
Else
	ll_recipient_no = ids_target_txn.GetItemNumber(1,'recipient_no') // BR 6.25 indicates that there should only be one recipient in a DirDep cancellation, so the first one is chosen
	ls_recipient_type_code = ids_target_txn.GetItemString(1,'recipient_type_code')
	IF ib_cancel_sp = FALSE THEN
		lds_cheque_deposit_list.dataobject = 'd_deposit_txn_list'
		lds_cheque_deposit_list.SetTransObject(SQLCA)
		ll_row = lds_cheque_deposit_list.Retrieve(ll_claim_no,ll_cheque_deposit_no,ldt_cheque_deposit_date,ll_recipient_no)
	ELSE
		lds_cheque_deposit_list.dataobject = 'd_deposit_txn_list_recipient'
		lds_cheque_deposit_list.SetTransObject(SQLCA)
		ll_row = lds_cheque_deposit_list.Retrieve(ll_cheque_deposit_no,ldt_cheque_deposit_date,ll_recipient_no, ls_recipient_type_code)
	END IF
	ls_cheque_deposit_desc = 'direct deposit'
End if

IF ll_row < 0 THEN
	SignalError(-666,'Error occured retrieving remaing transactions for ' + ls_cheque_deposit_desc + ' #' + String(ll_cheque_deposit_no))
End if

SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_cancel_txn_batch_rules','retrieve transaction list for canceling')




//	CONVERT MANUAL TXN MODULE SPECIFIC RULES
if is_module_code = '006'  and (ls_payment_method_code = 'A' or ls_payment_method_code = 'H') THEN
		
	//Loop through the cheque an sum up all the regular transactions for this claim
	//the cancelation adjustments must reduce that amount to zero.
	For li_x = 1 To lds_cheque_deposit_list.RowCount()
		ll_cheque_txn_claim_no 	= lds_cheque_deposit_list.GetItemNumber(li_x,'claim_no')
		ls_txn_type_code 			= lds_cheque_deposit_list.GetItemString(li_x,'txn_type_code')
		ldec_txn_amount 			= lds_cheque_deposit_list.GetItemDecimal(li_x,'txn_amount')
		
		IF ll_cheque_txn_claim_no <> ll_claim_no and ls_txn_type_code = '1' Then
			ldec_other_claim_cheque_amount += ldec_txn_amount
		ELSEIF ll_cheque_txn_claim_no = ll_claim_no and ls_txn_type_code = '1' Then
			ldec_claim_cheque_amount += ldec_txn_amount
		End if
		
	Next
	

	//The only time we don't show a list of txns for the cheque is when the cheque will be 
	//fully canceled after the current txns are canceled.	
			
	//If the cheque has been adjusted, show the list and send BR violation.
	IF ldec_adjusted_amount < 0 Then
		ls_window_message.as_stringparm[1] = 'This cheque has been adjusted. A cheque cannot be canceled once it has been adjusted.'
	
	Elseif ldec_claim_cheque_amount + ldec_sum_adjustment <> 0  THEN
		ls_window_message.as_stringparm[1] = 'The portion of the cheque that applies to claim #' + String(ll_claim_no) + ' amounts to ' + String(ldec_claim_cheque_amount,'$#,##0.00') + ', the full amount must be canceled.'
	
	Elseif ldec_other_claim_cheque_amount + ldec_canceled_amount + ldec_sum_adjustment + ldec_claim_cheque_amount <> 0 Then
		ls_window_message.as_stringparm[1] = 'This will only cancel the portion of the cheque that applies to claim #' + String(ll_claim_no) + '. Please cancel the rest of the transactions when you get a chance. Thanks.'
		// don't set lb_cancel_OK = TRUE because we want to display cheque list
		lb_more_claims_to_cancel = TRUE
		
	Else
		lb_cancel_OK = TRUE
	END IF
	
	
	
	if NOT lb_cancel_OK THEN			
		ls_window_message.apo_powerobjectparm[1] = lds_cheque_deposit_list
		
		OpenWithParm(w_cheque_txn_list,ls_window_message)			
		
		IF NOT lb_more_claims_to_cancel THEN
			iuo_br_exception.SetMessage('')
			THROW iuo_br_exception
		END IF
	END IF		
	
Else	
	//This logic is applied for direct deposit in either '005' or '006' modules as well
	//as canceling a cheque in the Claim Cost Maintenance module.
	
	ldec_adjusted_amount = lds_cheque_deposit_list.GetItemDecimal(1,'c_adjusted_amount')
	ldec_canceled_amount = lds_cheque_deposit_list.GetItemDecimal(1,'c_canceled_amount')
		
			
	//Get the cheque deposit total amount
	//A cancelation can only be done if the cheque will be fully canceled when we are done.
	ldec_cheque_deposit_amount = lds_cheque_deposit_list.GetItemDecimal(1,'c_cheque_deposit_amount')
			
	ldec_cheque_remaining_amount = ldec_cheque_deposit_amount + ldec_canceled_amount + ldec_sum_adjustment
	
	//In the CLAIM COST MAINTENANCE MODULE we force the user to cancel the entire cheque at once.
	IF ldec_cheque_remaining_amount <> 0 THEN
		iuo_br_exception.SetMessage('The ' + ls_cheque_deposit_desc + ' must be canceled in full. The current maintenance will not fully cancel the ' + ls_cheque_deposit_desc + '.~r~n~r~n' &
											+ '   Original amount     :    ' + String(ldec_cheque_deposit_amount,'$#,##0.00') + '~r~n'&
											+ '   Previously canceled :  + ' + String(ldec_canceled_amount,'$#,##0.00') +'~r~n' &
											+ '   Current maintenance :  + ' + String(ldec_sum_adjustment,'$#,##0.00') + '~r~n' &
											+ '   Remaining uncanceled:  = ' + String(ldec_cheque_remaining_amount,'$#,##0.00'))
		THROW iuo_br_exception
	End if
		
End if	
		
	
return 1

end function

public function integer of_check_claim_correction_rules (long al_row) throws uo_br_exception;INTEGER					li_rtn

LONG						ll_to_claim_no
LONG						ll_from_claim_no
LONG						ll_target_claim_no
long						ll_claim_from_individual
long						ll_target_txn_no
LONG						ll_adjustment_quantity
LONG						ll_goal_count
LONG						ll_recipient_no
LONG						ll_target_payment_no
LONG				     	ll_claim_to_individual
long						ll_related_txn_no
long 						ll_authorization_no_to 
long  						ll_authorization_no
long 						ll_claim_no_belonging_to_authorization
long                         ll_paid_quantity    
long 						ll_task_no_belonging_to_authorization
long 						ll_billable_xref_no_from	
long 						ll_billable_xref_no_to	
long						ll_count_rehab_line_items

STRING					ls_claim_status_code
STRING					ls_claim_status_type_code
STRING					ls_recipient_type_code		
STRING					ls_from_claim_role_code
STRING					ls_to_claim_role_code
STRING					ls_selected
STRING					ls_EFB_type
STRING					ls_claim_from_administering_act_code
string						ls_claim_to_administering_act_code
string						ls_target_payment_type_code	
string						ls_target_payment_sub_type_code	
string						ls_authorization_type_code
string 					ls_authorized_provider_type_code_from
string 					ls_authorized_provider_no_from 
string 					ls_authorized_provider_type_code_to
string 					ls_authorized_provider_no_to		  
string 					li_task_status_code
string 					ls_authorized_provider_type_code_belonging_to_authorization
string 					ls_authorized_provider_no_belonging_to_authorization

DATETIME				ldt_paid_from_date
DATETIME				ldt_to_claim_accident_date
datetime 				ldtm_authorized_date_belonging_to_authorization
datetime 				ldtm_planned_start_date
datetime 				ldtm_planned_completion_date
datetime 				ldt_paid_to_date								

decimal {2}				ldec_target_txn_amount,ldcm_sum_rehab_qty
decimal {2} 				ldec_adjustment_txn_amount
decimal {2} 				ldcm_authorized_quantity_belonging_to_authorization
decimal {2} 				ldcm_paid_quantity_belonging_to_authorization


/* GET THE VALUES FROM THE USER INPUT SCREEN*/
								  
ll_to_claim_no 				    	=   ids_batch_control.GetItemNumber(1,'to_claim_no') 
ll_from_claim_no 				=   ids_batch_control.GetItemNumber(1,'from_claim_no') 
ldcm_sum_rehab_qty          =   ids_batch_control.GetItemDecimal(1,'sum_rehab_qty')
ll_authorization_no_to 		=   ids_batch_control.GetItemDecimal(1,'authorization_no_to')

/* INITIALIZE TO ZERO, IF NOT ENTERED */
if isnull(ldcm_sum_rehab_qty) then ldcm_sum_rehab_qty = 0							
if isnull(ll_authorization_no_to) then ll_authorization_no_to = 0


/*GET THE VALUES FROM THE  TXN ON THE SCREEN */
ll_adjustment_quantity					= ids_maintenance.GetItemNumber(al_row,'adjustment_quantity')
ldec_adjustment_txn_amount			= ids_maintenance.GetItemDecimal(al_row,'adjustment_txn_amount')
ll_recipient_no				      			= ids_target_txn.GetItemNumber(al_row,'recipient_no')
ls_recipient_type_code		             	= ids_target_txn.GetItemString(al_row,'recipient_type_code')
ll_target_claim_no			                  = ids_target_txn.GetItemNumber(al_row,'claim_no') 
ldt_paid_from_date			             	= ids_target_txn.GetItemDateTime(al_row,'paid_from_date')
ldt_paid_to_date			                  = ids_target_txn.GetItemDateTime(al_row,'paid_to_date')
ll_target_payment_no			             	= ids_target_txn.GetItemNumber(al_row,'target_payment_no')
ll_target_txn_no				             	= ids_target_txn.GetItemNumber(al_row,'target_txn_no')
ls_target_payment_type_code		    	= ids_target_txn.GetItemString(al_row,'payment_type_code')
ls_target_payment_sub_type_code 	= ids_target_txn.GetItemString(al_row,'payment_sub_type_code')
ldec_target_txn_amount				    	= ids_target_txn.GetItemDecimal(al_row,'txn_amount')
ll_authorization_no                     		= ids_target_txn.GetItemNumber(al_row,'authorization_no')
ll_paid_quantity                               	= ids_target_txn.GetItemnumber(al_row,'paid_quantity')


iuo_br_exception.reset_indicator()


/* 8.10	To transfer a payment from one claim to another, the From  Claim and To Claim must not be the same claim number. */
IF ll_from_claim_no = ll_to_claim_no THEN
	iuo_br_exception.SetMessage('BR 8.10 - Claim corrections to the same claim are not allowed. See payment #' +String(ll_target_payment_no))
	iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
	THROW iuo_br_exception
END IF

/*8.110	Imported Atlantic Blue Cross prescription payments must not be partially transferred to another claim.*/
IF ls_target_payment_type_code = '22' and (ls_target_payment_sub_type_code = 'BC' OR ls_target_payment_sub_type_code = 'RC') THEN
	IF ldec_adjustment_txn_amount	* -1 <> ldec_target_txn_amount THEN
		iuo_br_exception.SetMessage(' BR 8.110 - Cannot partially adjust or transfer an Atlantic Blue Cross prescription payment. Payment # ' + String(ll_target_payment_no) + ' .')
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception
	END IF	
end if

IF ll_target_claim_no <> ll_from_claim_no THEN
	iuo_br_exception.SetMessage('The selected transactions must be for claim #' + String(ll_from_claim_no) + '. See payment #' +String(ll_target_payment_no))
	iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
	THROW iuo_br_exception
END IF


/* BR 8.20	A claim must be one of the following statuses to receive a payment transfer from another claim:
•	Active 
•	Finalled - Final
•	Finalled - First & Finalled
•	Finalled - Lost Time Med Aid Only
•	Finalled - No Lost Time
•	Pre-Adjudication
•	Adjudication
•	Rejected - Claim Disallowed  , Rejected - Insufficient Information ('R' - '07', '18')
*/

SELECT claim_status_code, claim_status_type_code, administering_act_code
INTO  :ls_claim_status_code,:ls_claim_status_type_code, :ls_claim_to_administering_act_code
FROM  CLAIM
WHERE claim_no = :ll_to_claim_no
USING SQLCA;
SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_claim_correction_rules','SELECT claim_status_code, claim_status_type_code')

IF SQLCA.SQLNRows = 0 THEN
	SignalError(-666,'Error finding claim number ' + String(ll_to_claim_no))
End if

IF  NOT (ls_claim_status_code = 'A')    &
AND NOT (ls_claim_status_code = 'F' AND &
				(	ls_claim_status_type_code = '01' OR &
					ls_claim_status_type_code = '02' OR &
					ls_claim_status_type_code = '03' OR &
					ls_claim_status_type_code = '04'		)) &
AND NOT 	(ls_claim_status_code = 'P')    &	
AND NOT 	(ls_claim_status_code = 'A')    &	
AND NOT (ls_claim_status_code = 'R' AND  &
				(ls_claim_status_type_code = '07' OR ls_claim_status_type_code = '18') ) THEN 
	iuo_br_exception.SetMessage('BR 8.20 - The "To Claim" status does not allow for payment transfers. See payment #' +String(ll_target_payment_no))
	iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)	
	THROW iuo_br_exception
END IF


/*BR 8.40	A payment with a recipient type of ‘individual’ must only be transferred to another claim that has the same individual set up with the same claim role.*/
IF ls_recipient_type_code = 'I' THEN
	
	//Get the individuals claim_role_code
	SELECT claim_role_code INTO :ls_from_claim_role_code
	FROM CLAIM_PARTICIPANT
	WHERE claim_no = :ll_from_claim_no
	  AND individual_no = :ll_recipient_no
	USING SQLCA;
	SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_claim_correction_rules','SELECT COUNT(*) FROM REHAB_GOAL')

	IF ls_from_claim_role_code = '' OR IsNull(ls_from_claim_role_code) THEN
		SignalError(-666,'Invalid claim participant')
	END IF
	
	//Make sure the individual is set up
	//Get the individuals claim_role_code
	SELECT claim_role_code INTO :ls_to_claim_role_code
	FROM CLAIM_PARTICIPANT
	WHERE claim_no = :ll_to_claim_no
	  AND individual_no = :ll_recipient_no
	USING SQLCA;
	SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_claim_correction_rules','SELECT COUNT(*) FROM REHAB_GOAL')

	IF SQLCA.SQLNRows = 0 Then
		iuo_br_exception.SetMessage('The individual must be a participant of the "To Claim". See payment #' +String(ll_target_payment_no))
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)	
		THROW iuo_br_exception
	END IF
	
	IF ls_from_claim_role_code <> ls_to_claim_role_code THEN
		iuo_br_exception.SetMessage('BR 8.40 -  The individual must play the same role on the "To Claim". See payment #' +String(ll_target_payment_no))
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception
	END IF

End if



/* BR 8.90  The individual on the To Claim should be the same as the individual on the From Claim, if the payment is payable to a service provider (i.e. not payable to an individual). (See Rationale)*/
/*The individual on the To Claim should be the same as the individual on the From Claim, if the payment is payable to a service provider (i.e. not payable to an individual). */

/* This warning is issued to try and deal with such things as payments that are garnished and paid to the Court of Queens Bench or CCRA.  

    Since the system does not identify it is a garnishment of benefits, the reason for the garnishment or whose benefits are being garnished, the application cannot determine if the transfer makes sense. 
    It is further complicated when the individual whose payments are being garnished is not the injured worker or when there are other participants involved in the claim besides the injured worker.
	
	There is a rule that stops payments from being transferred to a different claim when the recipient of the payment is not a participant on the ‘to’ claim in the same claim role. 
	However, there is nothing to prevent service provider payments from being transferred to a different claim involving different individual(s).
*/


if ib_suppress_warnings = false then
	
	if ib_suppress_message6 = false then

		IF ls_recipient_type_code <> 'I' THEN 
			SELECT ISNULL(individual_no,0)
			INTO :ll_claim_to_individual
			FROM CLAIM
			WHERE claim_no = :ll_to_claim_no;
			SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_claim_correction_rules','SELECT individual from CLAIM_PARTICIPIANT')
			
			
			SELECT ISNULL(individual_no,0)
			INTO :ll_claim_from_individual
			FROM CLAIM
			WHERE claim_no = :ll_from_claim_no;
			SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_claim_correction_rules','SELECT individual from CLAIM_PARTICIPIANT')
			
			
			IF ll_claim_to_individual <>  ll_claim_from_individual THEN
			
				li_rtn = f_produce_messagebox('BR 8.90 - The claimant on the "to" claim should be the same as the claimant on the "from" claim. See payment # ' + String(ll_target_payment_no))
		
				IF li_rtn = 2 Then 
					iuo_br_exception.SetMessage('')
					iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)	
					THROW iuo_br_exception
				ELSEIF li_rtn = 3 then
					ib_suppress_message6 = TRUE
				end if
				
			END IF
		end if
	end if
end if



/*BR 8.70	An account payment should not be transferred to another claim, if any portion of the payment period is prior to the claim’s accident date. */
/*BR 8.80	A non-account payment must not be transferred to another claim, if any portion of the payment period is prior to the claim’s accident date */
		
	/* get the authorization type needed for rule 8.70 & 8.80 */
	select   DISTINCT authorization_type_code
	into 		:ls_authorization_type_code
	from     Payment_Combination  
	WHERE 	payment_type_code = :ls_target_payment_type_code
	using 	sqlca;
	SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_claim_correction_rules','SELECT  DISTINCT authorization_type_code Payment_Combination ')
	
	if SQLCA.sqlnrows > 1 then
		SignalError(-666,'Found Multiple authorizations for payment type code = ' + ls_target_payment_type_code)
	elseif SQLCA.sqlnrows = 0 then
		SignalError(-666,'Found no authorizations for payment type code = ' + ls_target_payment_type_code)
	end if
	
	
	/* get the claim accident date for rule 8.80 */
	SELECT accident_date INTO :ldt_to_claim_accident_date 
	FROM CLAIM
	WHERE claim_no = :ll_to_claim_no
	USING SQLCA;
	if SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_claim_correction_rules','SELECT accident_date INTO :ldt_to_claim_accident_date') = 100 then
		SignalError(-666,'Invalid claim no . Claim no is ' + string(ll_to_claim_no))	
	end if
	
	/* BR 8.70	An account payment should not be transferred to another claim, if any portion of the payment period is prior to the claim’s accident date.*/
	if ls_authorization_type_code = 'act' then
		
		if ib_suppress_warnings = false then
			if ib_suppress_message7 = false then
				
				IF ldt_paid_from_date < ldt_to_claim_accident_date THEN
				
					li_rtn = f_produce_messagebox('BR 8.70 - Payment Period - An account payment should not be transferred to a claim if any portion of the payment period is prior to the claims accident date. Payment #' + String(ll_target_payment_no) &
											  + ' has a "Paid From Date" of ' + String(ldt_paid_from_date,"yyyy-mm-dd") + ' and the claims accident did not occur until ' + String(ldt_to_claim_accident_date,"yyyy-mm-dd"))
		
					IF li_rtn = 2 Then 
						iuo_br_exception.SetMessage('')
						iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)	
						THROW iuo_br_exception
					ELSEIF li_rtn = 3 then
						ib_suppress_message7 = TRUE
					end if
				
				END IF
			end if
		end if
	else
		
		/* BR 8.80	A non-account payment must not be transferred to another claim, if any portion of the payment period is prior to the claim’s accident date */
		IF ldt_paid_from_date < ldt_to_claim_accident_date THEN
			iuo_br_exception.SetMessage("BR 8.80 Payment Period - A non_account payment must not be transferred to a claim if any portion " &
									  + "of the payment period is prior to the claim's accident date. Payment #" + String(ll_target_payment_no) &
									  + ' has a "Paid From Date" of ' + String(ldt_paid_from_date,'yyyy-mm-dd') + " and the claim's " &
									  + 'accident did not occur until ' + String(ldt_to_claim_accident_date,'yyyy-mm-dd'))						  
			iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
			THROW iuo_br_exception
		end if
	
		
	end if
	
	/*8.120	A Return to Work Incentive payment must not be transferred to another claim.*/
	
	IF ls_target_payment_type_code = '21' THEN
		IF ls_target_payment_sub_type_code = '' OR ls_target_payment_sub_type_code = '12' THEN
			// check for associated Early Filing Bonuses
			// display warning to user if such exist
			nf_check_for_efb_payment(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no,'warning','transferred')
			
		ELSEIF ls_target_payment_sub_type_code = '01' OR ls_target_payment_sub_type_code = '09' THEN
			SELECT		related_txn_no
			INTO			:ll_related_txn_no
			FROM			APPLIED_CLAIM_TXN
			WHERE		txn_no = :ll_target_txn_no
			USING SQLCA;
			SQLCA.nf_handle_error('n_claim_cost_maintenance', '', 'of_check_claim_correction_rules - Embedded SQL:Select from APPLIED_CLAIM_TXN') 
		
			IF ls_target_payment_sub_type_code = '01' THEN ls_EFB_type = 'NBMS'
			IF ls_target_payment_sub_type_code = '09' THEN ls_EFB_type = 'NBCA'
			
			// display warning to user if not selected
			nf_check_for_MA_payment(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no,ll_related_txn_no,'warning',ls_EFB_type,'transferred')
			
		END IF
	ELSEIF	ls_target_payment_type_code = 'R1' &
			OR ls_target_payment_type_code = 'R2' &
			OR ls_target_payment_type_code = 'R3' THEN
		iuo_br_exception.SetMessage('BR 8.120 - RTW Incentive payments must not be transferred out of a claim.  See payment #' + String(ll_target_payment_no))
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception
	END IF



/* BR 8.130 To transfer a payment from one claim to another, the administering act codes must be the same.*/
SELECT administering_act_code
INTO  :ls_claim_from_administering_act_code
FROM  CLAIM
WHERE claim_no = :ll_from_claim_no
USING SQLCA;
SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_claim_correction_rules','SELECT claim_status_code, claim_status_type_code')

IF SQLCA.SQLNRows = 0 THEN
	SignalError(-666,'Error finding claim number ' + String(ll_to_claim_no))
End if


IF ls_claim_from_administering_act_code <> ls_claim_to_administering_act_code THEN
	iuo_br_exception.SetMessage('BR 8.130 - Payments must not be transferred from one administering act (WCA,FCA) to another.  See payment #' + String(ll_target_payment_no))
	iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
	THROW iuo_br_exception
END IF


/*br 14.130  The billable item that is authorized for the To Claim must be the same billable item that was authorized for the From Claim payment(s)*/
/*BR 14.80	- If the rehab authorization for the To Claim has an authorized provider and the recipient of the payment(s) being transferred is a provider, then authorized provider and the payment recipient for each payment must be the same provider */
/*br 14.40 -	The Paid From and Paid To date of each payment being transferred, must fall within the Planned Start and Planned End date of the rehab task associated with the rehab authorization for the To Claim.*/
/*BR 14.120  A payment that is linked to a rehab invoice line item must not be partially transferred to another claim. */


if  ll_authorization_no > 0 then		
	
	
			/*14.120  A payment that is linked to a rehab invoice line item must not be partially transferred to another claim. */
 
			 /* SEE IF THE 'FROM'  is linked to a rehab invoice line item */
			select isnull(count(*),0) 
			into :ll_count_rehab_line_items
		    from   REHAB_INVOICE_LINE_ITEM a
		    where a.claim_no = :ll_from_claim_no  and  
			         a.authorization_no = : ll_authorization_no
		   USING SQLCA;
			
		  /* if  linked - then check to make sur it's fully be adjusted */	
           IF (ll_count_rehab_line_items > 0 ) then
                 	if (ldec_adjustment_txn_amount * -1  <> ldec_target_txn_amount) or (ll_adjustment_quantity * -1 <> ll_paid_quantity ) then
			            iuo_br_exception.SetMessage('BR 14.120 - You cannot transfer part of the payment amount for payment # ' + string(ll_target_payment_no) + ' as this payment was created from a rehab invoice line item.')
		    			   iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
	         		   THROW iuo_br_exception
				 end if
		end if
	
	
		SELECT b.task_status_code,
				   b.planned_start_date,
				   b.planned_completion_date
		into       :li_task_status_code,
				   :ldtm_planned_start_date,
				   :ldtm_planned_completion_date
		  FROM REHAB_TASK_AUTHORIZATION a
		join REHAB_TASK b
		on a.task_no = b.task_no and a.claim_no = b.claim_no
		where a.claim_no = :ll_to_claim_no  and a.authorization_no = :ll_authorization_no_to
         USING SQLCA;
	     if SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_claim_correction_rules','SELECT REHAB_TASK_AUTHORIZATION to side}') = 100 then
		   	SignalError(-666,'Error finding authorization from authorization/task' + String( ll_authorization_no_to) + ' claim no ' + string(ll_to_claim_no))
          end if
			 
		/*BR 14.90   The rehab task associated with the rehab authorization for the To Claim must not be Cancelled (i.e task status code must not be = ‘03’).  */
		if (li_task_status_code = '03' ) THEN
		     iuo_br_exception.SetMessage('BR 14.90 - The rehab task for rehab authorization # ' + string(ll_authorization_no_to) + ' must not be cancelled if payments are being transferred to it.')
		     iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
	         THROW iuo_br_exception
	    end if
			 
		/* BR 14.40	The Paid From and Paid To date of each payment being transferred, must fall within the Planned Start and Planned End date of the rehab task associated with the rehab authorization for the To Claim. */
		  IF (	ldt_paid_from_date < ldtm_planned_start_date  OR ldt_paid_from_date  > ldtm_planned_completion_date) THEN
		    iuo_br_exception.SetMessage('BR 14.40 - The payment period for the payment(s) being transferred are not within the planned period for the rehab authorization for Claim # ' + string(ll_to_claim_no))
			 iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
	         THROW iuo_br_exception
	    END IF
		 
		 
		/* BR 14.40	The Paid From and Paid To date of each payment being transferred, must fall within the Planned Start and Planned End date of the rehab task associated with the rehab authorization for the To Claim. */
	     IF (	ldt_paid_to_date < ldtm_planned_start_date  OR  ldt_paid_to_date  > ldtm_planned_completion_date) THEN
		    iuo_br_exception.SetMessage('BR 14.40- The payment period for the payment(s) being transferred are not within the planned period for the rehab authorization for Claim # ' + string(ll_to_claim_no) )
		     iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
	         THROW iuo_br_exception
	    END IF
		 
		 
	select a.authorized_provider_type_code,
			 a.authorized_provider_no ,
			 b.billable_xref_no 
			 into	:ls_authorized_provider_type_code_from,
			   		:ls_authorized_provider_no_from,
					:ll_billable_xref_no_from	
	from REHAB_TASK_AUTHORIZATION a
	    JOIN Billable_Item_Rehab_Task_Xref b
					ON a.billable_xref_no = b.billable_xref_no 
	where  a.claim_no = :ll_from_claim_no  and  
	           a.authorization_no = : ll_authorization_no
	USING SQLCA;
	if SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_claim_correction_rules','SELECT REHAB_TASK_AUTHORIZATIONfrom side}') = 100 then
			SignalError(-666,'Error finding auhtorization from authorization/billable item ' + String( ll_authorization_no) + ' claim no ' + string(ll_from_claim_no))
      end if
	

	select       a.authorized_provider_type_code,
				  a.authorized_provider_no ,
				  a.billable_xref_no 
	 into         :ls_authorized_provider_type_code_to,
				  :ls_authorized_provider_no_to ,
				  :ll_billable_xref_no_to	
	from REHAB_TASK_AUTHORIZATION a
	    JOIN Billable_Item_Rehab_Task_Xref b
					ON a.billable_xref_no = b.billable_xref_no 
	where a.claim_no = :ll_to_claim_no and  
	a.authorization_no = :ll_authorization_no_to
	USING SQLCA;
	if SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_claim_correction_rules','SELECT REHAB_TASK_AUTHORIZATION to side}') = 100 then
		 	SignalError(-666,'Error finding auhtorization from authorization/billable item ' + String( ll_authorization_no_to) + ' claim no ' + string(ll_to_claim_no))
     end if
	





	/*14.80	If the rehab authorization for the To Claim has an authorized provider and the recipient of the payment(s) being transferred is a provider, then authorized provider and the payment recipient for each payment must be the same provider  */	
	 if  (ls_recipient_type_code = 'M' OR  ls_recipient_type_code = 'O'  OR  ls_recipient_type_code = 'V') then
       	if (ls_authorized_provider_type_code_from = ls_authorized_provider_type_code_to and ls_authorized_provider_no_from = ls_authorized_provider_no_to) then
		else
			

			iuo_br_exception.SetMessage('BR 14.80	-The provider on the authorization for claim # ' + string(ll_to_claim_no) + ' is not the same as the provider that was paid for the payment that is being transferred.')
			iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
			 THROW iuo_br_exception
		end if
	END IF


	/*BR 14.130  The billable item that is authorized for the To Claim must be the same billable item that was authorized for the From Claim payment(s)*/
	if  (ll_billable_xref_no_to <> ll_billable_xref_no_from) then
			iuo_br_exception.SetMessage('BR 14.130  - The billable item on the authorization for claim # ' + string(ll_to_claim_no) + ' is not the same as the item that was billed for the payments being transferred for claim # ' + string(ll_from_claim_no ))
			iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
			 THROW iuo_br_exception
	end if
	
end if


RETURN 1
end function

public function integer of_check_cost_transfer_rules (long al_row) throws uo_br_exception;INTEGER					li_rtn
LONG						ll_to_cost_alloc_no
LONG						ll_to_cost_alloc_operation_no
LONG						ll_target_cost_alloc_no
LONG						ll_target_cost_alloc_operation_no
LONG						ll_from_cost_alloc_no
LONG						ll_from_cost_alloc_operation_no
LONG						ll_claim_no
LONG						ll_individual_no
LONG						ll_target_payment_no
long						ll_target_txn_no 
DATETIME				ldt_accident_date
string						ls_employer_type_code
string						ls_target_payment_type_code
string						ls_target_payment_sub_type_code
string						ls_new_txn_type_code
string						ls_new_txn_sub_type_code
STRING					ls_EFB_type
long						ll_no_coc_found_to
long						ll_no_coc_found_from
long						ll_target_claim_no
long						ll_related_txn_no
STRING					ls_selected
STRING					ls_administering_act_code


ll_to_cost_alloc_no						= ids_batch_control.GetItemNumber(1,'to_cost_alloc_no')
ll_to_cost_alloc_operation_no			= ids_batch_control.GetItemNumber(1,'to_cost_alloc_operation_no')
ll_from_cost_alloc_no						= ids_batch_control.GetItemNumber(1,'from_cost_alloc_no')
ll_from_cost_alloc_operation_no		= ids_batch_control.GetItemNumber(1,'from_cost_alloc_operation_no')
ll_claim_no 									= ids_target_txn.GetItemNumber(al_row,'claim_no')
ll_target_payment_no 					= ids_target_txn.GetItemNumber(al_row,'target_payment_no')
ll_target_txn_no 							= ids_target_txn.GetItemNumber(al_row,'target_txn_no')
ll_target_claim_no 						= ids_target_txn.GetItemNumber(al_row,'claim_no')
ll_individual_no 							= ids_target_txn.GetItemNumber(al_row,'individual_no')
ldt_accident_date							= ids_target_txn.GetItemDateTime(al_Row,'accident_date')
ll_target_cost_alloc_no 					= ids_target_txn.GetItemNumber(al_row,'cost_alloc_no')
ll_target_cost_alloc_operation_no 		= ids_target_txn.GetItemNumber(al_row,'cost_alloc_operation_no')
ls_target_payment_type_code			= ids_target_txn.GetItemString(1,'payment_type_code')
ls_target_payment_sub_type_code	= ids_target_txn.GetItemString(1,'payment_sub_type_code')
ls_new_txn_type_code					= ids_batch_control.GetItemString(1,'new_txn_type_code')
ls_new_txn_sub_type_code				= ids_batch_control.GetItemString(1,'new_txn_sub_type_code')

iuo_br_exception.reset_indicator()


IF is_module_code = '005' THEN
	
	/*BR 9.10	Transactions selected for cost transfer must all have the same cost allocation number and cost allocation operation number.*/
	IF ll_target_cost_alloc_no <> ll_from_cost_alloc_no &
	 OR ll_target_cost_alloc_operation_no <> ll_from_cost_alloc_operation_no THEN
		iuo_br_exception.SetMessage("The selected transaction's must be cost allocated to " + String(ll_from_cost_alloc_no) + "/" + String(ll_from_cost_alloc_operation_no) + ".")
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception
	END IF

	/* only validate n_claim_employer for cost allocation/cost relief transfers */
	if ls_new_txn_type_code = 'T' and (ls_new_txn_sub_type_code = '7'  or ls_new_txn_sub_type_code = '8') THEN

		li_rtn = inv_claim_employer.nf_set_employer_parameters(ll_to_cost_alloc_no,ll_to_cost_alloc_operation_no)
		IF li_rtn <> 1 THEN
			iuo_br_exception.SetMessage('')
			iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
			THROW iuo_br_exception
		END IF
		
		li_rtn = inv_claim_employer.nf_set_claim_parameters(ll_claim_no,ldt_accident_date)
		IF li_rtn <> 1 THEN
			iuo_br_exception.SetMessage('')
			iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
			THROW iuo_br_exception
		END IF
		
		li_rtn = inv_claim_employer.nf_set_claimant_parameters(ll_individual_no)
		IF li_rtn <> 1 THEN
			iuo_br_exception.SetMessage('')
			iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
			THROW iuo_br_exception
		END IF
		
		li_rtn = inv_claim_employer.nf_IsValid_Cost_Alloc_Emp_for_payment(TRUE,TRUE) // suppress multiple warnings for operation & personal coverage checks
		IF li_rtn <> 1 THEN
			//Set the message to '' because inv_claim_employer displays it's own messages.
			iuo_br_exception.SetMessage('')
			iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)	
			THROW iuo_br_exception
		END IF	
	
	end if
	
End if


IF is_module_code = '006' THEN
	// cost allocations must be different for a cost transfer
	IF (ll_target_cost_alloc_no = ll_to_cost_alloc_no) AND (ll_target_cost_alloc_operation_no = ll_to_cost_alloc_operation_no) THEN
		ll_target_payment_no = ids_target_txn.GetItemNumber(al_row,'target_payment_no')
		iuo_br_exception.SetMessage('Cannot convert manual cost transfer for target payment ('+String(ll_target_payment_no)+') with same cost allocation as the "to" cost allocation.')
		THROW iuo_br_exception
	END IF	
END IF

// needed to prevent incorrect firefighter cost transfers
SELECT administering_act_code
INTO   :ls_administering_act_code
FROM   CLAIM
WHERE  claim_no = :ll_claim_no
USING SQLCA;
SQLCA.nf_handle_error('n_claim_cost_maintenance', '', 'of_check_cost_transfer_rules - Embedded SQL:Select administering_act_code	FROM CLAIM...')


/*9.90	Firefighter employers must not be granted cost relief.  */
/*9.100	Cost Allocation Correction from a firefighter employer must be transferred to a firefighter employer.*/


//BR 9.45 Cost must not be transferred to or from a ‘cost relief’ employer (see 9.40) except for cost relief.
if ls_new_txn_type_code = 'T' and ls_new_txn_sub_type_code = '7' THEN

	Select isnull(count(*),0)
	into :ll_no_coc_found_to
	 from EMPLOYER A, EMPLOYER B
	where ((B.employer_type_code = 'R' AND B.employer_no not in (1003,6000,7000, 8000)) or 
	       B.employer_no = 4003 ) and 
			 A.employer_no = B.employer_no and 
			 A.employer_no = :ll_to_cost_alloc_no	 
	using SQLCA;
	SQLCA.nf_handle_error('n_claim_cost_maintenance', '', 'of_check_cost_transfer_rules - Embedded SQL:Select from EMPLOYER 2') 
		
	if ll_no_coc_found_to = 1 then
		iuo_br_exception.SetMessage("Cost must not be transferred to or from a ‘cost relief’ employer")
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)			
		THROW iuo_br_exception	
	end if
	
	
	Select isnull(count(*),0)
	into :ll_no_coc_found_from
	 from EMPLOYER A, EMPLOYER B
	where ((B.employer_type_code = 'R' AND B.employer_no not in (1003,6000,7000, 8000)) or 
			 B.employer_no = 4003 ) and 
			 A.employer_no = B.employer_no and 
			 A.employer_no = :ll_from_cost_alloc_no	 
	using SQLCA;
	SQLCA.nf_handle_error('n_claim_cost_maintenance', '', 'of_check_cost_transfer_rules - Embedded SQL:Select from EMPLOYER 3') 
	
	if ll_no_coc_found_from = 1  then
		iuo_br_exception.SetMessage("Cost must not be transferred to or from a ‘cost relief’ employer")
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception
	end if

	SELECT employer_type_code
	INTO   :ls_employer_type_code
	FROM   EMPLOYER
	WHERE  employer_no = :ll_to_cost_alloc_no
	USING SQLCA;
	SQLCA.nf_handle_error('n_claim_cost_maintenance', '', 'of_check_cost_transfer_rules - Embedded SQL:Select employer_type_code FROM EMPLOYER...')


	IF ls_administering_act_code = 'WCA' AND ls_employer_type_code = 'F' THEN
		iuo_br_exception.SetMessage("Cost for a WCA claim must not be transferred to a Firefighter Administration employer.")
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception
		
	ELSEIF ls_administering_act_code = 'FCA' AND ls_employer_type_code <> 'F' THEN
		iuo_br_exception.SetMessage("Cost for an FCA claim must not be transferred to a non-Firefighter Administration employer.")
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception
		
	END IF

end if

//9.40	Cost relief must relieve the employer by transferring cost to or from one of the following:  
//·	Any Reserve account used for cost allocation purposes excluding Prior Years' Claims Reserve (1003)
//·	Charge to Group/Industry (4003)
//(Note: Reserve accounts, 6000 and 7000 cannot be used for claim cost allocation purposes, as they are special purpose accounts for special survivors' payments only)
//
//BR 9.50 Self-insured employers must not be granted cost relief.  

if ls_new_txn_type_code = 'T' and ls_new_txn_sub_type_code = '8' THEN

	SELECT isnull(employer_type_code,'')
	into :ls_employer_type_code
	FROM EMPLOYER
	where employer_no = :ll_target_cost_alloc_no 
	using sqlca; 
	SQLCA.nf_handle_error('n_claim_cost_maintenance', '', 'of_check_cost_transfer_rules - Embedded SQL:Select from EMPLOYER 1') 
	
	IF ls_employer_type_code = '' then
		iuo_br_exception.SetMessage('Cost Allocation does not exist in EMPLOYER table' )
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception
	end if	
		
	//BR 9.50 Cost relief must relieve self-insured employers by 'transferring cost to' or correct cost relief by 'transferring cost from' the All Assessed Second Injuries Account (2006)
		
	if ls_employer_type_code = 'S' then
		
		SELECT isnull(count(*),0)
		INTO   :ll_no_coc_found_to
		FROM   EMPLOYER  a
		JOIN   EMPLOYER  b  ON a.employer_no = b.employer_no
		JOIN   OPERATION aa ON a.employer_no = aa.employer_no
		JOIN   OPERATION bb ON aa.employer_no  = bb.employer_no
		                   AND aa.operation_no = bb.operation_no
		WHERE  b.employer_type_code IN ('G','R')
		AND  ( b.employer_no        = 2006
             OR ( b.employer_no    = 4003
				  AND bb.operation_no >= 100 ) )
		AND    a.employer_no        = :ll_to_cost_alloc_no
		AND    aa.operation_no      = :ll_to_cost_alloc_operation_no
		USING SQLCA;
		SQLCA.nf_handle_error('n_claim_cost_maintenance', 'Embedded SQL:Select from EMPLOYER 1a', 'of_check_cost_transfer_rules') 
		
		SELECT isnull(count(*),0)
		INTO   :ll_no_coc_found_from
		FROM   EMPLOYER  a
		JOIN   EMPLOYER  b  ON a.employer_no = b.employer_no
		JOIN   OPERATION aa ON a.employer_no = aa.employer_no
		JOIN   OPERATION bb ON aa.employer_no  = bb.employer_no
		                   AND aa.operation_no = bb.operation_no
		WHERE  b.employer_type_code IN ('G','R')
		AND  ( b.employer_no        = 2006
             OR ( b.employer_no    = 4003
				  AND bb.operation_no >= 100 ) )
		AND    a.employer_no        = :ll_from_cost_alloc_no
		AND    aa.operation_no      = :ll_from_cost_alloc_operation_no
		using SQLCA;
		SQLCA.nf_handle_error('n_claim_cost_maintenance', 'Embedded SQL:Select from EMPLOYER 1b', 'of_check_cost_transfer_rules') 
		
		IF ll_no_coc_found_to + ll_no_coc_found_from = 0 THEN
			iuo_br_exception.SetMessage('Cost Relief or Cost Relief Correction for Self-Insured Employers must include either:'&
			                          + '~r~n•   the All Assessed Second Injuries Account (2006) OR' &
											  + '~r~n•   the Charge to Industry Account (4003) where the operation number is 100 or greater.')
			iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no,ll_target_claim_no)
			THROW iuo_br_exception
		END IF
	end if

		
	/*9.40	     Cost relief must relieve the employer by 'transferring cost to' one of the following accounts or correct cost relief by 'transferring cost from' one of the following accounts:  
								 •   Any Reserve account used for cost allocation purposes excluding Prior Years’ Claims Reserve (1003)
								 •	Charge to Group/Industry (4003)
								 (Note: Reserve accounts, 6000 and 7000 cannot be used for claim cost allocation purposes, as they are special purpose accounts for special survivors’ payments only)
	*/


	/* Cost relief must relieve the employer by transferring cost to or from 
	one of the assigned reserved accounts */
		
	Select isnull(count(*),0)
	into :ll_no_coc_found_to
	 from EMPLOYER A, EMPLOYER B
	where ((B.employer_type_code = 'R' AND B.employer_no not in (1003,6000,7000)) or 
	       B.employer_no = 4003 ) and 
			 A.employer_no = B.employer_no and 
			 A.employer_no = :ll_to_cost_alloc_no	 
	using SQLCA;
	SQLCA.nf_handle_error('n_claim_cost_maintenance', '', 'of_check_cost_transfer_rules - Embedded SQL:Select from EMPLOYER 2') 
	
	Select isnull(count(*),0)
	into :ll_no_coc_found_from
	 from EMPLOYER A, EMPLOYER B
	where ((B.employer_type_code = 'R' AND B.employer_no not in (1003,6000,7000)) or 
			 B.employer_no = 4003 ) and 
			 A.employer_no = B.employer_no and 
			 A.employer_no = :ll_from_cost_alloc_no	 
	using SQLCA;
	SQLCA.nf_handle_error('n_claim_cost_maintenance', '', 'of_check_cost_transfer_rules - Embedded SQL:Select from EMPLOYER 3') 

	if ll_no_coc_found_to + ll_no_coc_found_from =  0 then
		iuo_br_exception.SetMessage("Cost relief must relieve the employer by 'transferring cost to' one of the following accounts or correct cost relief by "&
                             + "~n'transferring cost from' one of the following accounts:  " &
									  + "~n·	Any Reserve account used for cost allocation purposes excluding Prior Years' Claims Reserve (1003)" &
									  + "~n·	Charge to Group/Industry (4003)")
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception
	end if
	
	IF ls_administering_act_code = 'FCA' THEN
		IF ll_no_coc_found_to > 0 THEN
			// the 'to' cost allocation must not be 'cost relief', i.e., employer_type_code = 'R' AND employer_no not in (1003,6000,7000)) or employer_no = 4003 
			iuo_br_exception.SetMessage("A Firefighters' Compensation Administration claim is not allowed to receive cost relief.")
			iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
			THROW iuo_br_exception
		ELSE
			// the 'from' cost allocation can not be 'cost relief', 
			// as long as the 'to' cost allocation is a firefighter cost allocation (employer_type_code = 'F')		
			SELECT employer_type_code
			INTO   :ls_employer_type_code
			FROM   EMPLOYER
			WHERE  employer_no = :ll_to_cost_alloc_no
			USING SQLCA;
			SQLCA.nf_handle_error('n_claim_cost_maintenance', '', 'of_check_cost_transfer_rules - Embedded SQL:Select employer_type_code FROM EMPLOYER...')
			
			IF ls_employer_type_code <> 'F' THEN
				iuo_br_exception.SetMessage("Cost Relief correction for a Firefighters' Compensation Administration claim must result in a transfer to a firefighter cost allocation.")
				iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
				THROW iuo_br_exception
			END IF
			
		END IF			
	END IF

END IF


IF ls_target_payment_type_code = '21' THEN
	IF ls_target_payment_sub_type_code = '' OR ls_target_payment_sub_type_code = '12' THEN
		// check for associated Early Filing Bonuses
		// display warning to user if such exist
		nf_check_for_efb_payment(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no,'warning','transferred')
	ELSEIF ls_target_payment_sub_type_code = '01' OR ls_target_payment_sub_type_code = '09' THEN
		SELECT		related_txn_no
		INTO			:ll_related_txn_no
		FROM			APPLIED_CLAIM_TXN
		WHERE		txn_no = :ll_target_txn_no
		USING SQLCA;
		SQLCA.nf_handle_error('n_claim_cost_maintenance', '', 'of_check_cost_transfer_rules - Embedded SQL:Select from APPLIED_CLAIM_TXN') 
		
		IF ls_target_payment_sub_type_code = '01' THEN ls_EFB_type = 'NBMS'
		IF ls_target_payment_sub_type_code = '09' THEN ls_EFB_type = 'NBCA'
		
		// check for associated Medical Account payment
		// display warning to user if not selected
		nf_check_for_MA_payment(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no,ll_related_txn_no,'warning',ls_EFB_type,'transferred')
	END IF
END IF


//P10152-083 - R.S. New Cost Allocation rules
//9.110	All claims cost allocated to the All-Assessed Silicosis account (# 3009) must have an accident date prior to January 1, 1948.
IF ll_to_cost_alloc_no = 3009 and ldt_accident_date >= DATETIME(1948-01-01) THEN
	iuo_br_exception.SetMessage("All claims cost allocated to the All-Assessed Silicosis account (# 3009), must have an accident date prior to January 1, 1948.")
	iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
	THROW iuo_br_exception
END IF

//9.120	All claims cost allocated to the All-Assessed Pre-1982 Claims account (# 1004) must have an accident date prior to January 1, 1982.
IF ll_to_cost_alloc_no = 1004 and ldt_accident_date >= DATETIME(1982-01-01) THEN
	iuo_br_exception.SetMessage("All claims cost allocated to the All-Assessed Pre-1982 Claims account (# 1004), must have an accident date prior to January 1, 1982.")
	iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
	THROW iuo_br_exception
END IF

//9.130	No cost allocation to the Prior Years' Claim Reserve (# 1003) is permitted.	
IF ll_to_cost_alloc_no = 1003  THEN
	iuo_br_exception.SetMessage("No cost allocation to the Prior Years' Claim Reserve (# 1003) is permitted.")
	iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
	THROW iuo_br_exception
END IF	

RETURN 1

end function

public function integer of_check_for_over_adjustments (long al_txn_no, ref decimal ad_sum_payment_type, ref decimal ad_sum_cost_alloc, ref string as_message);//*********************************************************************************************
//This function will report to the user whether the selected transaction
//is blocked by a manual transaction


STRING	ls_payment_type_code
STRING	ls_ntr_payment_group_code
LONG		ll_cost_alloc_no
LONG		ll_cost_alloc_operation_no
DECIMAL{2}		ldec_sum_txn_amount_cost
DECIMAL{2}		ldec_sum_txn_amount_cost_unapp
DECIMAL{2}		ldec_sum_txn_amount_cost_app
DECIMAL{2}		ldec_sum_txn_amount_type
DECIMAL{2}		ldec_sum_txn_amount_type_unapp
DECIMAL{2}		ldec_sum_txn_amount_type_app
DateTime	ldt_today
DateTime	ldt_max_manual_processed_date
DateTime	ldt_payment_processed_date
DateTime	ldt_earliest_processed_date
LONG		ll_payment_no
LONG		ll_count
LONG		ll_claim_no
INTEGER		li_return

SetPointer(Hourglass!)

iuo_br_exception.reset_indicator()

ldt_today = f_server_datetime()

//Get some information about the payment
SELECT b.payment_type_code,
		b.processed_date,
		a.claim_no,
		ntr_payment_group_code
INTO 	:ls_payment_type_code,
		:ldt_payment_processed_date,
		:ll_claim_no,
		:ls_ntr_payment_group_code
FROM	APPLIED_CLAIM_TXN a,
		PAYMENT b,
		Ntr_Payment_Group_Xref c
WHERE a.payment_no = b.payment_no
  and a.txn_no = :al_txn_no
  and c.payment_type_code = b.payment_type_code
USING SQLCA;

SQLCA.nf_handle_error('w_payment_inquiry','ue_manualtxn','SELECT payment_type_code')

//Set the cost allocation information
SELECT COALESCE(b.cost_alloc_no,RELATED_COC.cost_alloc_no,0) ,
       COALESCE(b.cost_alloc_operation_no ,RELATED_COC.cost_alloc_operation_no,0)
INTO  :ll_cost_alloc_no,
		:ll_cost_alloc_operation_no
FROM APPLIED_CLAIM_TXN a 
    LEFT OUTER JOIN COST_OF_CLAIMS_ALLOCATED b  ON a.txn_no = b.txn_no
    LEFT OUTER JOIN COST_OF_CLAIMS_ALLOCATED RELATED_COC  ON RELATED_COC.txn_no = a.related_txn_no
 WHERE a.txn_no = :al_txn_no
USING SQLCA;

SQLCA.nf_handle_error('w_payment_inquiry','ue_manualtxn','SELECT COALESCE(b.cost_alloc_no,RELATED_COC.cost_alloc_no,0)')
	


SELECT	count(*)
INTO		:ll_count
FROM		NTR_MANUAL_TXN_AFTER
WHERE	txn_no  = :al_txn_no
AND		deleted_flag = "N"
USING SQLCA;

SQLCA.nf_handle_error('Embedded SQL: select from NTR_MANUAL_TXN_AFTER','n_payment_inquiry','ue_manualtxn')


CHOOSE CASE ls_ntr_payment_group_code
	CASE 'I'
				
		IF ll_count > 0 THEN
			as_message = "A manual transaction is BLOCKING this payment transaction."
			RETURN 1
		ELSE
			as_message = "This transaction is NOT blocked by a manual transaction."
			RETURN 0
		END IF
		
	CASE 'L','P'
		//Is the payment processed in the last 6 years
		ldt_earliest_processed_date = DateTime(Date(Year(Date(ldt_today)) - 6,1,1))
		
		IF ldt_payment_processed_date < ldt_earliest_processed_date Then
			as_message = "This transaction is NOT blocked by a manual transaction."
			RETURN 0
		End if
		
		IF ll_count = 0 Then
			as_message = "This transaction is NOT blocked by a manual transaction."
			RETURN 0
		End if
			
		
		
		
		SELECT max(a.processed_date)
		INTO :ldt_max_manual_processed_date
		FROM PAYMENT a,
			  APPLIED_CLAIM_TXN b,
			  Ntr_Payment_Group_Xref c
		WHERE a.payment_no = b.payment_no
		  AND b.claim_no = :ll_claim_no
		  AND txn_type_code in ('2','4','5')
		  and c.payment_type_code = a.payment_type_code
		  and c.ntr_payment_group_code = :ls_ntr_payment_group_code
		USING SQLCA;
		  
		SQLCA.nf_handle_error('w_payment_inquiry','ue_manualtxn','select max(a.processed_date)')
		
		//Because ll_count > 0, we know that there should be a max processed_date
		IF ISNull(ldt_max_manual_processed_date) THen
			SIgnalError(-666,'Error finding max processed_date for manual transaction.')
		End if
			
		IF ll_cost_alloc_no <> 0 Then

			//sum by CLAIM and COST ALLOCATION
			select IsNull(sum(txn_amount),0)
			INTO :ldec_sum_txn_amount_cost_app
			FROM PAYMENT a (NOLOCK),
				  APPLIED_CLAIM_TXN b (NOLOCK),
				  COST_OF_CLAIMS_ALLOCATED c (NOLOCK),
				  Ntr_Payment_Group_Xref d
			WHERE a.payment_no = b.payment_no
			  and b.txn_no = c.txn_no
			  and a.claim_no = :ll_claim_no
			  and b.claim_no = :ll_claim_no
			  and c.claim_no = :ll_claim_no
			  and d.payment_type_code = a.payment_type_code
			  and d.ntr_payment_group_code = :ls_ntr_payment_group_code
			  and c.cost_alloc_no = :ll_cost_alloc_no
			  and c.cost_alloc_operation_no = :ll_cost_alloc_operation_no
			  and ((b.txn_type_code = '1' and a.processed_date < :ldt_max_manual_processed_date)
					  OR (b.txn_type_code <> '1' and a.processed_date <= :ldt_max_manual_processed_date))
			USING SQLCA;
			
			SQLCA.nf_handle_error('w_payment_inquiry','ue_manualtxn','select sum(txn_amount)')
			
			select IsNull(sum(txn_amount),0)
			INTO :ldec_sum_txn_amount_cost_unapp
			FROM PAYMENT a (NOLOCK),
				  UNAPPLIED_CLAIM_TXN b (NOLOCK),
				  COST_OF_CLAIMS_ALLOCATED c (NOLOCK),
				  Ntr_Payment_Group_Xref d
			WHERE a.payment_no = b.payment_no
			  and b.txn_no = c.txn_no
				and a.claim_no = :ll_claim_no
			  and b.claim_no = :ll_claim_no
			  and c.claim_no = :ll_claim_no
			  and d.payment_type_code = a.payment_type_code
			  and d.ntr_payment_group_code = :ls_ntr_payment_group_code
			  and c.cost_alloc_no = :ll_cost_alloc_no
			  and c.cost_alloc_operation_no = :ll_cost_alloc_operation_no
			  and b.txn_type_code <> '1' and a.processed_date <= :ldt_max_manual_processed_date
			USING SQLCA;
		  
			SQLCA.nf_handle_error('w_payment_inquiry','ue_manualtxn','select sum(txn_amount)')
		
		
			ldec_sum_txn_amount_cost = ldec_sum_txn_amount_cost_unapp + ldec_sum_txn_amount_cost_app
		End if
		  
	  	//sum by CLAIM and PAYMENT TYPE
	  	select IsNull(sum(txn_amount),0)
		INTO :ldec_sum_txn_amount_type_app
		FROM PAYMENT a (NOLOCK),
			  APPLIED_CLAIM_TXN b (NOLOCK),
			  COST_OF_CLAIMS_ALLOCATED c (NOLOCK),
			  Ntr_Payment_Group_Xref d
		WHERE a.payment_no = b.payment_no
		  and b.txn_no = c.txn_no
		  and a.claim_no = :ll_claim_no
		  and b.claim_no = :ll_claim_no
		  and c.claim_no = :ll_claim_no
		  and d.payment_type_code = a.payment_type_code
		  and d.ntr_payment_group_code = :ls_ntr_payment_group_code
		  and a.payment_type_code = :ls_payment_type_code
		  and ((b.txn_type_code = '1' and a.processed_date < :ldt_max_manual_processed_date)
				  OR (b.txn_type_code <> '1' and a.processed_date <= :ldt_max_manual_processed_date))
		USING SQLCA;
		
		SQLCA.nf_handle_error('w_payment_inquiry','ue_manualtxn','select sum(txn_amount)')
		
		select IsNull(Sum(txn_amount),0)
		INTO :ldec_sum_txn_amount_type_unapp
		FROM PAYMENT a (NOLOCK),
			  UNAPPLIED_CLAIM_TXN b (NOLOCK),
			  COST_OF_CLAIMS_ALLOCATED c (NOLOCK),
			  Ntr_Payment_Group_Xref d
		WHERE a.payment_no = b.payment_no
		  and b.txn_no = c.txn_no
			and a.claim_no = :ll_claim_no
		  and b.claim_no = :ll_claim_no
		  and c.claim_no = :ll_claim_no
		  and d.payment_type_code = a.payment_type_code
		  and d.ntr_payment_group_code = :ls_ntr_payment_group_code
		  and a.payment_type_code = :ls_payment_type_code
		  and b.txn_type_code <> '1' and a.processed_date <= :ldt_max_manual_processed_date
	  	USING SQLCA;
	  
		SQLCA.nf_handle_error('w_payment_inquiry','ue_manualtxn','select sum(txn_amount)')
		  
		ldec_sum_txn_amount_type = ldec_sum_txn_amount_type_unapp + ldec_sum_txn_amount_type_app
		
		
		ad_sum_payment_type = ldec_sum_txn_amount_type
		ad_sum_cost_alloc = ldec_sum_txn_amount_cost
		
		

		
		
		//RETURN 0 - NOT BLOCKED
		//RETURN 1 - BLOCKED
		//RETURN 2 - might be blocked by payment type or cost allocation
		//RETURN 3 - might be blocked by payment type (cost allocation not required for payment type)
		
		IF ll_cost_alloc_no <> 0 Then
			as_message = 'This transaction may be blocked by a manual transaction. ' &
						+ ' ~r~n~r~nMaintenance will be prevented if an adjustment takes either of the following sums below zero: (net payment sum for all transactions up to and including the latest manual transaction.)' &
						+ ' ~r~n~r~nCost allocation ' + String(ll_cost_alloc_no) + '/' + String(ll_cost_alloc_operation_no) + ' sums to: ' + String(ldec_sum_txn_amount_cost,'$#,##0.00') &
						+ ' ~r~nPayment Type "' + ls_payment_type_code + '" sums to: ' + String(ldec_sum_txn_amount_type,'$#,##0.00')
			li_return = 2
		Else 
			as_message = 'This transaction may be blocked by a manual transaction. ' &
						+ ' ~r~n~r~nMaintenance will be prevented if an adjustment takes the following sum below zero: (net payment sum for all transactions up to and including the latest manual transaction.)' &
						+ ' ~r~nPayment Type "' + ls_payment_type_code + '" sums to: ' + String(ldec_sum_txn_amount_type,'$#,##0.00')
			li_return = 3
		End if

			
			
				
	CASE ELSE
		as_message = "This transaction is NOT blocked by a manual transaction."
	   li_return = 0
		
		
END CHOOSE

return li_return


		
end function

public function integer of_check_general_txn_rules (long al_row) throws uo_br_exception;// of_check_general_txn_rules
//
// Argument - al_row
// removed 27 unused local variables...

Long       ll_target_payment_no, ll_cost_alloc_no, ll_cost_alloc_operation_no, ll_recipient_no, ll_ins_rehab_task_auth_row 
Long       ll_authorization_no, ll_coc_period, ll_target_coc_period, ll_claim_no, ll_individual_no, ll_target_txn_no, ll_target_claim_no
Integer    li_adjustment_quantity, li_net_quantity, li_paid_quantity, li_rtn
Decimal{2} ldec_target_txn_amount, ldec_target_tax_amount, ldec_max_positive_adjustment, ldec_max_positive_adjustment_tax, ldec_adjustment_txn_amount
Decimal{2} ldec_adjustment_tax_amount, ldec_net_paid_days_lost, ldec_net_paid_hours_lost, ldec_paid_days_lost, ldec_paid_hours_lost, ldec_net_tax_amount 
Decimal{2} ldec_net_payment_amount, ldec_adjustment_days, ldec_adjustment_hours, ldec_total_award_amount , ldec_total_deductions , ldec_adjustment_payment_amount
String     ls_new_txn_type_code, ls_new_txn_sub_type_code, ls_recipient_type_code, ls_payment_method_code, ls_target_payment_type_code 
String     ls_target_payment_sub_type_code, ls_admin_region_code, ls_from_payment_method_code, ls_to_payment_method_code 
Datetime   ldtm_paid_from_date, ldtm_paid_to_date, ldtm_target_processed_date 

//MAINTENANCE DATA
ldec_adjustment_txn_amount = ids_maintenance.GetItemDecimal(al_row,'adjustment_txn_amount')
ldec_adjustment_tax_amount = ids_maintenance.GetItemDecimal(al_row,'adjustment_tax_amount')
ldec_adjustment_days			= ids_maintenance.GetItemDecimal(al_row,'adjustment_days')
ldec_adjustment_hours		= ids_maintenance.GetItemDecimal(al_row,'adjustment_hours')
li_adjustment_quantity		= ids_maintenance.GetItemNumber(al_row,'adjustment_quantity')


ls_new_txn_type_code				= ids_batch_control.GetItemString(1,'new_txn_type_code')
ls_new_txn_sub_type_code		= ids_batch_control.GetItemString(1,'new_txn_sub_type_code')
ll_coc_period						= ids_batch_control.GetItemNumber(1,'coc_period')
ls_from_payment_method_code	= ids_batch_control.GetItemString(1,'from_payment_method_code')
ls_to_payment_method_code		= ids_batch_control.GetItemString(1,'to_payment_method_code')


//TARGET TXN DATA
ldec_target_txn_amount 				= ids_target_txn.GetItemDecimal(al_row,'txn_amount')
ldec_target_tax_amount 				= ids_target_txn.GetItemDecimal(al_row,'txn_tax_amount')
ll_target_payment_no					= ids_target_txn.GetItemNumber(al_row,'target_payment_no')
ll_target_txn_no						= ids_target_txn.GetItemNumber(al_row,'target_txn_no')
ll_target_claim_no					= ids_target_txn.GetItemNumber(al_row,'claim_no')
ll_cost_alloc_no						= ids_target_txn.GetItemNumber(al_row,'cost_alloc_no')
ll_cost_alloc_operation_no			= ids_target_txn.GetItemNumber(al_row,'cost_alloc_operation_no')
ll_recipient_no						= ids_target_txn.GetItemNumber(al_row,'recipient_no')
ls_recipient_type_code				= ids_target_txn.GetItemString(al_row,'recipient_type_code')
li_net_quantity						= ids_target_txn.GetItemNumber(al_row,'net_quantity')
li_paid_quantity						= ids_target_txn.GetItemNumber(al_row,'paid_quantity')
ldec_net_payment_amount				= ids_target_txn.GetItemDecimal(al_row,'net_payment_amount')
ll_authorization_no 					= ids_target_txn.GetItemNumber(al_row,'authorization_no')
ldec_net_paid_days_lost				= ids_target_txn.GetItemDecimal(al_row,'net_days_lost')
ldec_net_paid_hours_lost			= ids_target_txn.GetItemDecimal(al_row,'net_hours_lost')
ldec_paid_days_lost					= ids_target_txn.GetItemDecimal(al_row,'paid_days_lost')
ldec_paid_hours_lost					= ids_target_txn.GetItemDecimal(al_row,'paid_hours_lost')
ldec_net_tax_amount					= ids_target_txn.GetItemDecimal(al_row,'net_tax_amount')
ls_target_payment_type_code		= ids_target_txn.GetItemString(al_row,'payment_type_code')
ls_target_payment_sub_type_code	= ids_target_txn.GetItemString(al_row,'payment_sub_type_code')
ldtm_paid_from_date					= ids_target_txn.GetItemdatetime(al_row,'paid_from_date')
ldtm_paid_to_date						= ids_target_txn.GetItemdatetime(al_row,'paid_to_date')
ls_payment_method_code				= ids_target_txn.GetItemString(al_row,'payment_method_code')
ll_claim_no								= ids_target_txn.GetItemnumber(al_row,'claim_no')
ll_individual_no						= ids_target_txn.GetItemnumber(al_row,'recipient_no')
ldtm_paid_from_date					= ids_target_txn.GetItemdatetime(al_row,'paid_from_date')
ldtm_paid_to_date						= ids_target_txn.GetItemdatetime(al_row,'paid_to_date')
ldtm_target_processed_date			= ids_target_txn.GetItemdatetime(al_row,'processed_date')
ll_target_coc_period					= ids_target_txn.GetItemnumber(al_row,'coc_period')
		
iuo_br_exception.reset_indicator()


//BR 4.10 & 4.40
IF ldec_adjustment_txn_amount < 0 Then
	IF (ldec_target_txn_amount + ldec_adjustment_txn_amount) < 0 Then
		iuo_br_exception.SetMessage('Payment #' + String(ll_target_payment_no) + ', for ' &
		+ String(ldec_target_txn_amount,'$#,##0.00') + ' cannot be adjusted by ' &
		+ String(ldec_adjustment_txn_amount,'$#,##0.00') + '. This would result in an over adjustment.')
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception
	End if

//BR 4.20 Excluding adjustments for cancelled cheques/deposits, a transaction amount must not be positively adjusted by more than the absolute value of the sum of the transactions for a payment matching on the following:
//Transaction type and subtype (Adjustments) 
//Cost allocation number and operation number
//The same recipient.
Elseif ldec_adjustment_txn_amount > 0 AND ls_new_txn_type_code	= 'J' Then
	
	IF ll_cost_alloc_no > 0 Then
	
		SELECT IsNull(Abs(SUM(txn_amount)),0) INTO :ldec_max_positive_adjustment
		FROM APPLIED_CLAIM_TXN a,
			  COST_OF_CLAIMS_ALLOCATED b
		WHERE a.txn_no = b.txn_no
		  AND a.payment_no 				= :ll_target_payment_no
		  AND txn_type_code 				= :ls_new_txn_type_code
		  AND txn_sub_type_code 		= :ls_new_txn_sub_type_code
		  AND cost_alloc_no 				= :ll_cost_alloc_no
		  AND cost_alloc_operation_no = :ll_cost_alloc_operation_no
		  AND recipient_no 				= :ll_recipient_no
		  AND recipient_type_code     = :ls_recipient_type_code
		USING SQLCA;
		
		SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_txn_amount_rules','SELECT Abs(SUM(txn_amount))')
		
	
	ElseIf ll_cost_alloc_no = 0 THEN
		
		SELECT IsNull(Abs(SUM(txn_amount)),0) INTO :ldec_max_positive_adjustment
		FROM APPLIED_CLAIM_TXN a,
			  COST_OF_CLAIMS_ALLOCATED b,
			  COST_OF_CLAIMS_ALLOCATED c
		WHERE a.txn_no = b.txn_no
		  AND a.related_txn_no = c.txn_no
		  AND a.payment_no 					= :ll_target_payment_no
		  AND txn_type_code 					= :ls_new_txn_type_code
		  AND txn_sub_type_code 			= :ls_new_txn_sub_type_code
		  AND b.cost_alloc_no 				= c.cost_alloc_no
		  AND b.cost_alloc_operation_no 	= c.cost_alloc_operation_no
		  AND recipient_no 					= :ll_recipient_no
		  AND recipient_type_code     	= :ls_recipient_type_code
		USING SQLCA;
		
		SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_txn_amount_rules','SELECT Abs(SUM(txn_amount))')
			
			
	END IF
	
	if ls_new_txn_type_code	= 'J' and ls_new_txn_sub_type_code = '2' then
	else
		IF ldec_adjustment_txn_amount > ldec_max_positive_adjustment THEN
			iuo_br_exception.SetMessage('Payment #' + String(ll_target_payment_no) + ' cannot be adjusted above ' + String(ldec_max_positive_adjustment,'$#,##0.00'))
			THROW iuo_br_exception
		END IF
	end if
		

END IF

//BR 4.60
/* the amount and qty adjusted must be the same sign 
*/
IF li_adjustment_quantity <> 0 and ldec_adjustment_txn_amount <> 0 THEN
	IF Sign(li_adjustment_quantity) <> Sign(ldec_adjustment_txn_amount) THEN
		iuo_br_exception.SetMessage('Quantity and amount must both be positive or negative. See payment #' + String(ll_target_payment_no))
		THROW iuo_br_exception
	END IF
end if


if ldec_adjustment_tax_amount <> 0 and ldec_adjustment_txn_amount <> 0 THEN
	IF Sign(ldec_adjustment_tax_amount) <> Sign(ldec_adjustment_txn_amount) THEN
		iuo_br_exception.SetMessage('tax and amount must both be positive or negative. See payment #' + String(ll_target_payment_no))
		THROW iuo_br_exception
	END IF
end if


if  ldec_adjustment_days <> 0 and ldec_adjustment_txn_amount <> 0 THEN
	IF Sign(ldec_adjustment_days) <> Sign(ldec_adjustment_txn_amount) THEN
		iuo_br_exception.SetMessage('days lost and amount must both be positive or negative. See payment #' + String(ll_target_payment_no))
		THROW iuo_br_exception
	END IF
end if


if  ldec_adjustment_hours <> 0 and ldec_adjustment_txn_amount <> 0 THEN
	IF Sign(ldec_adjustment_hours) <> Sign(ldec_adjustment_txn_amount) THEN
		iuo_br_exception.SetMessage('hours lost and amount must both be positive or negative. See payment #' + String(ll_target_payment_no))
		THROW iuo_br_exception
	END IF
end if


//BR 7.10
IF ll_authorization_no = 0 and li_adjustment_quantity <> 0 THEN
	iuo_br_exception.SetMessage('There is no quantity to maintain.  See payment #' + String(ll_target_payment_no))
	THROW iuo_br_exception
END IF


// The tax must be zero if the payment amount is zero.
IF  (ldec_net_payment_amount + ldec_adjustment_txn_amount)  = 0 &
and ( ldec_net_tax_amount + ldec_adjustment_tax_amount  ) <> 0.00 THEN
	iuo_br_exception.SetMessage('When the payment amount is reduced to zero, the tax must also be reduced to zero.  See payment #' + String(ll_target_payment_no))
	THROW iuo_br_exception
END IF



//BR 
/* 	The quantity must not be adjusted without adjusting the payment amount 
   	except for a rehab quantity correction transaction.
*/
IF li_adjustment_quantity <> 0 and ldec_adjustment_txn_amount = 0 &
and not (ls_new_txn_type_code = 'J' and ls_new_txn_sub_type_code = 'Q') Then
	iuo_br_exception.SetMessage('The amount must always be adjusted when the quantity is adjusted, with the exception of "Rehab Quantity Corrections".  See payment #' + String(ll_target_payment_no))
	THROW iuo_br_exception
END IF


/* BR 4.13 */

if ib_suppress_warnings = false then
	if ib_suppress_message8 = false then
		/*	A payment that has a quantity greater than zero SHOULD have the quantity
			adjusted when the transaction amount is adjusted.
		*/
		IF li_net_quantity > 0 THEN
			
			IF li_adjustment_quantity = 0 and ldec_adjustment_txn_amount <> 0 THEN

				
				li_rtn = f_produce_messagebox('You have adjusted the payment amount, for payment #' + String(ll_target_payment_no) + ', without adjusting the quantity.')
				IF li_rtn = 2 Then 
					iuo_br_exception.SetMessage('')
					THROW iuo_br_exception
				ELSEIF li_rtn = 3 then
					ib_suppress_message8 = TRUE
				end if
				
			END IF
			
		END IF
	end if
end if



//BR 7.20
/*		Only quantity can be adjusted when performing a Rehab Quantity Correction
*/
IF ls_new_txn_type_code = 'J' and ls_new_txn_sub_type_code = 'Q' THEN
	IF ldec_adjustment_txn_amount <> 0 THEN
		iuo_br_exception.SetMessage('The amound must not be adjusted when performing a Rehab Quantity Correction. See payment #' + String(ll_target_payment_no))
		THROW iuo_br_exception
	END IF
	
	// BR 7.40
	/* ADJUSTMENT REHAB QTY MUST NOT BE ZERO 
	*/
	if li_adjustment_quantity = 0 then
		iuo_br_exception.SetMessage('The adjustment quantity for Rehab Quantity Correction must not be zero. See payment #' + String(ll_target_payment_no))
		THROW iuo_br_exception
	end if
	
END IF
			

/* only check max adj amount for transaction other then tax correction */
if ls_new_txn_type_code = "J" and ls_new_txn_sub_type_code = "4" then
	IF ldec_adjustment_tax_amount = 0 THEN
		iuo_br_exception.SetMessage('Must enter a adjustment tax amount when performing a tax correction. Payment # ' + String(ll_target_payment_no) + ' .')
		THROW iuo_br_exception
	END IF
end if

//BR 4.330	
if ib_suppress_warnings = false then

	IF ldec_adjustment_txn_amount <> 0 THEN
		IF ldec_target_txn_amount	- ( ldec_adjustment_txn_amount * -1) <= 1 and (ldec_target_txn_amount	- ( ldec_adjustment_txn_amount * -1) > 0 ) then
			li_rtn = MessageBox('Warning','You have adjusted the transaction amount within a $1.00 , for payment #' + String(ll_target_payment_no) + ', . Do you want to continue?',Question!,YesNo!)
			IF li_rtn = 2 THEN
				iuo_br_exception.SetMessage('')
				THROW iuo_br_exception
			END IF
		END IF
	END IF
end if


IF ls_payment_method_code	= "R" THEN
	IF (ls_new_txn_type_code = 'J' and (ls_new_txn_sub_type_code = '5' OR ls_new_txn_sub_type_code = "R") ) or +&
		(ls_new_txn_type_code = 'T' and ls_new_txn_sub_type_code = '7' ) THEN
		IF ls_new_txn_type_code = 'J' and ls_new_txn_sub_type_code = 'R' THEN
			// if you are cancelling a target, then
			IF ldec_adjustment_txn_amount * -1 <> ldec_target_txn_amount  then
				// BR 12.20 - A receiving salary payment must not be partially cancelled. 
				//A receiving salary payment must not be partially cancelled. 
				iuo_br_exception.SetMessage('Receiving Salary transactions cannot be partially canceled. Payment # ' + String(ll_target_payment_no) + ' .')
				THROW iuo_br_exception
			END IF
		END IF
	ELSE
		// BR 4.300 - Receiving salary payments must not be adjusted or transferred except 
		// ·	To correct cost allocation.
		// ·	To recover from a third party 
		// ·	To cancel the transaction
		iuo_br_exception.SetMessage('Not a valid txn type for Receiving Salary payments. Payment # ' + String(ll_target_payment_no) + ' .')
		THROW iuo_br_exception
	END IF
END IF

	
//BR 3.70
if ls_new_txn_type_code = "J" and ls_new_txn_sub_type_code = "4" then
ELSE
	IF ldec_adjustment_tax_amount < 0 Then
		IF (ldec_target_tax_amount + ldec_adjustment_tax_amount) < 0 Then
			iuo_br_exception.SetMessage('Payment #' + String(ll_target_payment_no) + " . The maximum tax is " + String(ldec_target_tax_amount,'$#,##0.00') + " . The tax cannot be adjusted by " +  + String(ldec_adjustment_tax_amount,'$#,##0.00') + " . This would result in an over adjustment." )			
			THROW iuo_br_exception
		End if
	
	//BR 3.70
	Elseif ldec_adjustment_tax_amount > 0 Then
		
		IF ll_cost_alloc_no > 0 Then
		
			SELECT IsNull(Abs(SUM(tax_amount)),0) INTO :ldec_max_positive_adjustment_tax
			FROM APPLIED_CLAIM_TXN a,
				  COST_OF_CLAIMS_ALLOCATED b
			WHERE a.txn_no = b.txn_no
			  AND a.payment_no 				= :ll_target_payment_no
			  AND txn_type_code 				= :ls_new_txn_type_code
			  AND txn_sub_type_code 		= :ls_new_txn_sub_type_code
			  AND cost_alloc_no 				= :ll_cost_alloc_no
			  AND cost_alloc_operation_no = :ll_cost_alloc_operation_no
			  AND recipient_no 				= :ll_recipient_no
			  AND recipient_type_code     = :ls_recipient_type_code
			USING SQLCA;
			
			SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_txn_amount_rules','SELECT Abs(SUM(tax_amount))')
			
		ElseIf ll_cost_alloc_no = 0 THEN
			
			SELECT IsNull(Abs(SUM(tax_amount)),0) INTO :ldec_max_positive_adjustment_tax
			FROM APPLIED_CLAIM_TXN a,
				  COST_OF_CLAIMS_ALLOCATED b,
				  COST_OF_CLAIMS_ALLOCATED c
			WHERE a.txn_no = b.txn_no
			  AND a.related_txn_no = c.txn_no
			  AND a.payment_no 					= :ll_target_payment_no
			  AND txn_type_code 					= :ls_new_txn_type_code
			  AND txn_sub_type_code 			= :ls_new_txn_sub_type_code
			  AND b.cost_alloc_no 				= c.cost_alloc_no
			  AND b.cost_alloc_operation_no 	= c.cost_alloc_operation_no
			  AND recipient_no 					= :ll_recipient_no
			  AND recipient_type_code     	= :ls_recipient_type_code
			USING SQLCA;
			
			SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_txn_amount_rules','SELECT Abs(SUM(tax_amount))')
				
		END IF
		
		IF ldec_adjustment_tax_amount > ldec_max_positive_adjustment_tax THEN
			iuo_br_exception.SetMessage('Payment #' + String(ll_target_payment_no) + ' . The tax cannot be adjusted above ' + String(ldec_max_positive_adjustment_tax,'$#,##0.00') + " . ")
			THROW iuo_br_exception
		END IF	
	END IF
end if



//* BR 1.130
SELECT isnull(dbo.CLAIM.admin_region_code,'')  
INTO :ls_admin_region_code  
FROM dbo.CLAIM  
WHERE dbo.CLAIM.claim_no = :ll_claim_no
using SQLCA;
SQLCA.nf_handle_error('n_claim_cost_maintenance', '', 'of_check_cost_transfer_rules - Embedded SQL:Select from CLAIM') 

IF ls_admin_region_code = '' THEN
	iuo_br_exception.SetMessage('Error - No Admin region found for claim no ' + string(ll_claim_no) + ' . ')
	THROW iuo_br_exception
END IF
	

//1.15 The cost of claims period must not be before the cost of claims period of the target transaction being maintained.
if ll_target_coc_period > ll_coc_period   then
	iuo_br_exception.SetMessage('Payment #' + String(ll_target_payment_no) + " .   The cost of claims period must not be before the cost of claims period of the target transaction being maintained.")
	THROW iuo_br_exception
end if


/* check annuity payout and if any manaul txn should prevent maintenece */
IF is_module_code = '005' THEN
	of_check_annuity_payout(al_row)	
	of_check_manuals_for_over_adjustments(al_row)
	of_check_tax_rules(al_row)
elseif is_module_code = '006' THEN
	/* BR 1.200 */
	if ls_from_payment_method_code  = 'R'  and ls_payment_method_code	<> "R"  then
		iuo_br_exception.SetMessage('Payment #' + String(ll_target_payment_no) + " .   A receiving salary manual payment must be applied to receiving salary payments")
		THROW iuo_br_exception
	end if
	/* BR 1.210 */
	if ls_from_payment_method_code  <> 'R'  and ls_payment_method_code = "R"  then
		li_rtn = MessageBox('Warning','You have selected a non-receiving salary manual payment to be applied to a receiving salary target payment. Do you want to continue?',Question!,YesNo!)
		IF li_rtn = 2 THEN
			iuo_br_exception.SetMessage('')
			THROW iuo_br_exception
		END IF
	end if
end if

// PR 6299 - Must add check for the following data situation the original data for a split payment has had one regular txn overadjusted.  When users add adjustment txns to 
//           the 2nd regular txn that does not overadjust it (the 2nd reg txn), but DOES overadjust the payment as a whole, then the app should stop the confirm.
SELECT total_award_amount,       total_deductions,       adjustment_payment_amount 
  INTO :ldec_total_award_amount, :ldec_total_deductions, :ldec_adjustment_payment_amount 
  FROM PAYMENT 
 WHERE payment_no = :ll_target_payment_no 
 USING SQLCA ; 

SQLCA.nf_handle_error('n_claim_cost_maintenance', '', 'of_check_general_txn_rules - Select from PAYMENT') 

// Determine if payment type = Medical Account and Payment Sub Type = NBMS Reporting Fee Reversal, OR NBCA Reporting Fee Reversal
IF ls_target_payment_type_code = '21' AND ( ls_target_payment_sub_type_code = '07' OR ls_target_payment_sub_type_code = '17') THEN 
	// BR 1.165 The sum of the total payment amount and the total payment adjustment amount of a payment must be less than or equal to zero when the payment is an NBMS Reporting Fee Reversal (see Rationale).
	IF (ldec_total_award_amount + ldec_total_deductions + ldec_adjustment_payment_amount + ldec_adjustment_txn_amount) > 0 THEN
		IF ls_target_payment_sub_type_code = '07' THEN
			iuo_br_exception.SetMessage('Payment #' + String(ll_target_payment_no) + ". The sum of the total payment amount and the total payment adjustment amount of a payment must be less than or equal to zero when the payment is an NBMS Reporting Fee Reversal.")
		ELSE
			iuo_br_exception.SetMessage('Payment #' + String(ll_target_payment_no) + ". The sum of the total payment amount and the total payment adjustment amount of a payment must be less than or equal to zero when the payment is an NBCA Reporting Fee Reversal.")
		END IF
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception
	END IF

	// BR 1.175 A payment must have a total payment adjustment amount greater than or equal to zero when the payment is an NBMS Reporting Fee Reversal (see Rationale).
	// IF (0                           + 18)                         < 0 THEN
	IF (ldec_adjustment_payment_amount + ldec_adjustment_txn_amount) < 0 THEN
		iuo_br_exception.SetMessage('Payment #' + String(ll_target_payment_no) + ". A payment must have a total payment adjustment amount greater than or equal to zero when the payment is an NBMS Reporting Fee Reversal.")		
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception
	END IF
ELSE
	// BR 1.160 The sum of the total payment amount and the total payment adjustment amount of a payment must be greater than or equal to zero when the payment is not an NBMS Reporting Fee Reversal (see Rationale).
	// BR 1.170 A payment must have a total payment adjustment amount less than or equal to zero when the payment is not an NBMS Reporting Fee Reversal (see Rationale).
	// 
	// IF (85.41                       + -85.41)                     > 0 THEN
	IF (ldec_adjustment_payment_amount + ldec_adjustment_txn_amount) > 0 THEN
		iuo_br_exception.SetMessage('Payment #' + String(ll_target_payment_no) + " . The sum of processed payment adjustments and new payment adjustments must not be more than $0.")
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception
	END IF
	
	// payment award + payment deductions + payment adjustments + new txns >= 0, otherwise error out
	IF (ldec_total_award_amount + ldec_total_deductions + ldec_adjustment_payment_amount + ldec_adjustment_txn_amount) < 0 THEN
		iuo_br_exception.SetMessage('Payment #' + String(ll_target_payment_no) + " . The sum of all transactions of a payment must not be less than $0.")
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception
	END IF
END IF

RETURN 1
end function

public function integer of_check_recovery_txn_rules (long al_row) throws uo_br_exception;STRING									ls_new_txn_type_code				
STRING									ls_new_txn_sub_type_code			
DECIMAL{2}								ldec_adjustment_txn_amount 
DECIMAL{2}								ldec_target_txn_amount		
DECIMAL{2}								ldec_canceled_amount
STRING									ls_claim_status_code			
STRING									ls_claim_status_type_code	
STRING									ls_EFB_type
LONG										ll_base_cheque_no
LONG										ll_row
DATETIME									ldt_base_cheque_deposit_date
u_ds										lds_cheque_txn_list, lds_txn_sum_for_tier

string									ls_payment_method_code
string									ls_target_payment_type_code
string									ls_target_payment_sub_type_code
datetime									ldtm_paid_from_date	
datetime									ldtm_paid_to_date	
long										ll_target_payment_no	
long										ll_target_txn_no
long										ll_target_claim_no
long										ll_related_txn_no
STRING								ls_selected, ls_message
integer								li_rtn, li_opening_no, li_tier_no, li_rows
DECIMAL								ldec_rtw_incentive_amount, ldec_existing_r1_txn_sum

iuo_br_exception.reset_indicator()

//Load some variables used in BR verification.
ls_new_txn_type_code			= ids_batch_control.GetItemString(1,'new_txn_type_code')
ls_new_txn_sub_type_code	= ids_batch_control.GetItemString(1,'new_txn_sub_type_code')

ldec_adjustment_txn_amount = ids_maintenance.GetItemDecimal(al_row,'adjustment_txn_amount')

ldec_target_txn_amount		= ids_target_txn.GetItemDecimal(al_row,'txn_amount')
ls_claim_status_code			= ids_target_txn.GetItemString(al_row,'claim_status_code')
ls_claim_status_type_code	= ids_target_txn.GetItemString(al_row,'claim_status_type_code')
ls_payment_method_code		= ids_target_txn.GetItemString(al_row,'payment_method_code')
ll_target_payment_no			= ids_target_txn.GetItemDecimal(al_row,'target_payment_no')
ll_target_txn_no				= ids_target_txn.GetItemDecimal(al_row,'target_txn_no')
ll_target_claim_no			= ids_target_txn.GetItemDecimal(al_row,'claim_no')
ls_target_payment_type_code			= ids_target_txn.GetItemString(1,'payment_type_code')
ls_target_payment_sub_type_code	= ids_target_txn.GetItemString(1,'payment_sub_type_code')


//Verify that the txn is a recovery before validating the rest of the rules
IF not(ls_new_txn_type_code = 'J' and (ls_new_txn_sub_type_code = '3' or ls_new_txn_sub_type_code = '5')) Then
	//NOT A RECOVERY TRANSACTION

	RETURN 0
END IF



//BR 5.10
/*		The adjusted Amount must not be zero for recovery transactions.
*/
IF ldec_adjustment_txn_amount = 0 THEN
	iuo_br_exception.SetMessage('Adjustment transaction amount is required for "Recovery" transactions. Payment #' + String(ll_target_payment_no))
	iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
	THROW iuo_br_exception
END IF
	


//BR 5.20
/*		Only transactions that have a transaction amount greater than 
	or equal to zero can be adjusted through a recovery transaction.
*/
IF ldec_target_txn_amount < 0 THEN
	iuo_br_exception.SetMessage('Only transactions that have a transaction amount greater than or equal to zero can be adjusted through a recovery transaction. Payment #' + String(ll_target_payment_no))
	iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
	THROW iuo_br_exception
END IF



//BR 5.30
/*		A claim must have one of the following statuses in order to be adjusted positively.
			·	Active
			·	Finalled - Final
			·	Finalled - First & Finalled
			·	Finalled - Lost Time Med Aid Only
			·	Finalled - No Lost Time
*/
IF ldec_adjustment_txn_amount > 0 THEN
	IF  NOT (ls_claim_status_code = 'A')    &
	AND NOT (ls_claim_status_code = 'F' AND &
					(	ls_claim_status_type_code = '01' OR &
						ls_claim_status_type_code = '02' OR &
						ls_claim_status_type_code = '03' OR &
						ls_claim_status_type_code = '04'		)) THEN
		iuo_br_exception.SetMessage('The claim status does not allow for positive adjustments. Payment #' + String(ll_target_payment_no))
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception
	END IF
END IF



IF ldec_adjustment_txn_amount < 0 THEN

	/*		You can't negatively adjust a cheque that has been canceled.
	*/
	
	ll_base_cheque_no						= ids_target_txn.GetItemNumber(1,'cheque_no')
	ldt_base_cheque_deposit_date		= ids_target_txn.GetITemDateTime(1,'cheque_deposit_date')
	
	
	IF ll_base_cheque_no <> 0 THEN
	
		//Put code here to display a list of cheque transactions that still need to be canceled.
		lds_cheque_txn_list = CREATE u_ds
		
		lds_cheque_txn_list.dataobject = 'd_cheque_txn_list'
		lds_cheque_txn_list.SetTransObject(SQLCA)
		
		//ll_txns_being_canceled = ids_target_txn.object.target_txn_no[1,ll_row_count]
		
		ll_row = lds_cheque_txn_list.Retrieve(ll_base_cheque_no,ldt_base_cheque_deposit_date)
		
		IF ll_row < 0 THEN
			SignalError(-666,'Error occured retrieving transactions for cheque #' + String(ll_base_cheque_no))
		End if
			
		ldec_canceled_amount = lds_cheque_txn_list.GetItemDecimal(1,'c_canceled_amount')
		
		IF ldec_canceled_amount <> 0 Then
			iuo_br_exception.SetMessage('A recovery transaction cannot be applied against cheque #' + String(ll_base_cheque_no) + '. That cheque has already been canceled.')
			iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
			THROW iuo_br_exception
		END IF
	END IF
end if


IF ls_target_payment_type_code = '21' THEN
	IF ls_target_payment_sub_type_code = '' OR ls_target_payment_sub_type_code = '12' THEN
		// check for associated Early Filing Bonuses
		// display warning to user if such exist
		nf_check_for_efb_payment(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no,'warning','adjusted')
	ELSEIF ls_target_payment_sub_type_code = '01' OR ls_target_payment_sub_type_code = '09' THEN
		SELECT		related_txn_no
		INTO			:ll_related_txn_no
		FROM			APPLIED_CLAIM_TXN
		WHERE		txn_no = :ll_target_txn_no
		USING SQLCA;
		SQLCA.nf_handle_error('n_claim_cost_maintenance', '', 'of_check_cost_transfer_rules - Embedded SQL:Select from APPLIED_CLAIM_TXN') 
	
		IF ls_target_payment_sub_type_code = '01' THEN ls_EFB_type = 'NBMS'
		IF ls_target_payment_sub_type_code = '09' THEN ls_EFB_type = 'NBCA'
		
		// check for associated Medical Account payment
		// display warning to user if not selected
		nf_check_for_MA_payment(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no,ll_related_txn_no,'warning',ls_EFB_type,'adjusted')
		
	END IF
ELSEIF ls_target_payment_type_code = 'R1' THEN
	// P10261 RTW Incentive
	// check that the reversal of an overpayment recovery will not
	// cause the RTW tier for the claim opening to be overpaid.
	SELECT	a.opening_no , b.tier_no, c.rtw_incentive_amount
	INTO		:li_opening_no , :li_tier_no, :ldec_rtw_incentive_amount
	FROM		PAYMENT a
	JOIN		RTW_INCENTIVE_PAYMENT_XREF b ON a.payment_no = b.payment_no
	JOIN		Rtw_Incentive_Tier c ON b.tier_no = c.tier_no
	WHERE	a.payment_no = :ll_target_payment_no
	USING	SQLCA;
	
	SQLCA.nf_handle_error('n_claim_cost_maintenance','select opening_no FROM PAYMENT','nf_check_recovery_txn_rules')
	
	lds_txn_sum_for_tier = CREATE u_ds
	
	lds_txn_sum_for_tier.dataobject = 'ds_txn_sum_for_tier'
	lds_txn_sum_for_tier.SetTransObject(SQLCA)
	
	li_rows = lds_txn_sum_for_tier.Retrieve(ll_target_claim_no,li_opening_no,li_tier_no)
	IF li_rows = 1 THEN
		ldec_existing_r1_txn_sum = lds_txn_sum_for_tier.GetItemDecimal(1,'sum_txns')
		IF ldec_existing_r1_txn_sum + ldec_adjustment_txn_amount > ldec_rtw_incentive_amount THEN
			ls_message = 'The overpayment recovery adjustment cannot be reversed.' &
						+ '~n~nThe reversal of the chosen transactions for RTW Incentive payment #'+String(ll_target_payment_no) &
						+ '~nwould result in a new payment total for RTW Incentive tier #' +String(li_tier_no)+' for claim #'+String(ll_target_claim_no)+',' &
						+ '~nopening #'+String(li_opening_no)+' that would exceed the amount authorized for the tier ('+String(ldec_rtw_incentive_amount,'$#,##0')+').'
			iuo_br_exception.SetMessage(ls_message)
			iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
			THROW iuo_br_exception
		END IF
	END IF
	
END IF


RETURN 1

																
																
end function

private function integer of_check_target_txn_maintainability_rule (long al_row) throws uo_br_exception;LONG			ll_target_txn_no
LONG			ll_target_payment_no
long 			ll_txn_unit_of_work_no
LONG			ll_prescription_set_no
STRING		ls_maintain_allowed_flag
LONG			ll_count
long			ll_target_claim_no

iuo_br_exception.reset_indicator()

ll_target_txn_no 				= ids_target_txn.GetItemNumber(al_row,'target_txn_no')
ll_prescription_set_no 		= ids_target_txn.GetItemNumber(al_row,'prescription_set_no')
ls_maintain_allowed_flag 	= ids_target_txn.GetItemString(al_row,'maintain_allowed_flag')
ll_target_payment_no	      = ids_target_txn.GetItemNumber(al_row,'target_payment_no')
ll_target_claim_no	      = ids_target_txn.GetItemNumber(al_row,'claim_no')



//Check maintain allowed flag
IF is_module_code = '005' Then
	
	//Maintain allowed flag must be "Y"
	IF ls_maintain_allowed_flag = 'N' Then
		iuo_br_exception.SetMessage('Transaction ' + String(ll_target_txn_no) + ' is not maintainable.')
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		Throw iuo_br_exception
	END IF
	

ELSEIF is_module_code = '006' Then
	
	//When the maintain allowed flag = "N", the txn must exist in 
	//the NTR_MANUAL_TXN_AFTER table to be maintainable.
	SELECT COUNT(*) INTO :ll_count
	FROM NTR_MANUAL_TXN_AFTER
	WHERE txn_no = :ll_target_txn_no
	USING SQLCA;
	
	SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_target_txn_maintainability_rule','select NTR_MANUAL_TXN_AFTER')

	IF ll_count <> 1 Then
		iuo_br_exception.SetMessage('Transaction ' + String(ll_target_txn_no) + ' is not maintainable.')
		Throw iuo_br_exception
	End if
		
End if
	
// BR 1.50 
/* must make sure txn is fully processed  
*/
SELECT COUNT(*) 
INTO :ll_count
FROM DAILY_CHEQUE
where txn_no = :ll_target_txn_no
using SQLCA;
SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_target_txn_maintainability_rule','select ~ from  DAILY_CHEQUE')
	
if ll_count > 0 then
	iuo_br_exception.SetMessage('Transaction ' + String(ll_target_txn_no) + ' EXISTS IN DAILY CHEQUE - THAT INDICATES THAT IT HAS NOT FULLY PROCESSSED.')
	iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
	Throw iuo_br_exception
end if


// BR 1.50 
/* must make sure txn is fully processed 
*/
SELECT COUNT(*) 
INTO :ll_count
FROM DAILY_DIRECT_DEPOSIT
where txn_no = :ll_target_txn_no
using SQLCA;
SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_target_txn_maintainability_rule','select ~ from  DAILY_DIRECT_DEPOSIT')

if ll_count > 0 then	
	iuo_br_exception.SetMessage('Transaction ' + String(ll_target_txn_no) + ' EXISTS IN DAILY DIRECT DEPOSIT - THAT INDICATES THAT IT HAS NOT FULLY PROCESSSED.')
	iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
	Throw iuo_br_exception
end if



//BR 1.60 
/* RELATED LINK TXN EXIST
*/
SELECT count(*) into :ll_count
FROM UNAPPLIED_CLAIM_TXN 
WHERE related_txn_no = :ll_target_txn_no
USING SQLCA;

SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_target_txn_maintainability_rule','select unapplied')

IF ll_count > 0 Then
	iuo_br_exception.SetMessage('Transaction ' + String(ll_target_txn_no) + ' has unprocessed maintenance already.')
	iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
	Throw iuo_br_exception
END IF

//BR 1.60 
/* A Related BLUE CROSS CARE prescription payment exist
*/
SELECT count(*) into :ll_count
FROM UNAPPLIED_CLAIM_TXN a,
     PAYMENT_PRESCRIPTION b
WHERE a.payment_no = b.payment_no
  AND b.prescription_set_no = :ll_prescription_set_no
USING SQLCA;

SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_target_txn_maintainability_rule','select unapplied')

IF ll_count > 0 Then
	iuo_br_exception.SetMessage('Transaction ' + String(ll_target_txn_no) + ' has unprocessed maintenance already.')
	iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
	Throw iuo_br_exception
END IF



SELECT count(*) into :ll_count
FROM UNAPPLIED_CLAIM_TXN 
WHERE payment_no = :ll_target_payment_no		
USING SQLCA;
SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_target_txn_maintainability_rule','select unapplied claim txn')

IF ll_count > 0 Then
	
	select  txn_unit_of_work_no, 
				isnull(count(*),0)
	into :ll_txn_unit_of_work_no,
		  :ll_count
   from UNAPPLIED_CLAIM_TXN
   where payment_no = :ll_target_payment_no
   group by txn_unit_of_work_no
	using SQLCA;
	SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_target_txn_maintainability_rule','select unapplied claim txn')

	IF ll_count > 0 then
		iuo_br_exception.SetMessage('Payment ' + String(ll_target_payment_no) + ' is a split payment. Part of this payment has been adjusted but not processed.  Either wait for the transaction to be processed or delete the unprocessed transaction.   The unprocessed transaction can be found in Unit of Work #' + string(ll_txn_unit_of_work_no) + ".")
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		Throw iuo_br_exception
	end if
END IF




return 1
end function

public function integer of_check_tax_rules (long al_row) throws uo_br_exception;date		ldt_payment_processed_date	
BOOLEAN  lb_positive_adjustment

DECIMAL {2} ldec_adjustment_txn_amount
DECIMAL {2}	ldec_adjustment_tax_amount
DECIMAL {2}	ldec_tax_rate	
DECIMAL {2}	ldec_target_txn_amount 
DECIMAL {2} ldec_target_tax_amount
decimal {2} ldec_max_tax_amount
decimal {2} ldec_max_adj_tax_amount
decimal {2} ldec_min_adjustment_amount_tax
decimal {2} ldec_max_txn_amount
decimal {2} ldec_tax_on_remaining

long		ll_target_payment_no
long		ll_target_txn_no
long		ll_recipient_no
long		ll_award_no	
long		ll_target_claim_no

string	ls_recipient_type_code
STRING 	ls_target_payment_type_code
string	ls_target_payment_sub_type_code
string 	ls_new_txn_type_code
string 	ls_new_txn_sub_type_code

iuo_br_exception.reset_indicator()

ls_new_txn_type_code		 = ids_batch_control.GetItemString(1,'new_txn_type_code')
ls_new_txn_sub_type_code = ids_batch_control.GetItemString(1,'new_txn_sub_type_code')

//MAINTENANCE DATA
ldec_adjustment_txn_amount 		= ids_maintenance.GetItemDecimal(al_row,'adjustment_txn_amount')
ldec_adjustment_tax_amount 		= ids_maintenance.GetItemDecimal(al_row,'adjustment_tax_amount')

//TARGET TXN DATA
ldec_target_txn_amount 				= ids_target_txn.GetItemDecimal(al_row,'txn_amount')
ldec_target_tax_amount 				= ids_target_txn.GetItemDecimal(al_row,'txn_tax_amount')
ll_target_payment_no					= ids_target_txn.GetItemNumber(al_row,'target_payment_no')
ll_target_txn_no						= ids_target_txn.GetItemNumber(al_row,'target_txn_no')
ll_target_claim_no					= ids_target_txn.GetItemNumber(al_row,'claim_no')
ll_recipient_no						= ids_target_txn.GetItemNumber(al_row,'recipient_no')
ls_recipient_type_code				= ids_target_txn.GetItemString(al_row,'recipient_type_code')
ls_target_payment_type_code		= ids_target_txn.GetItemString(al_row,'payment_type_code')
ls_target_payment_sub_type_code	= ids_target_txn.GetItemString(al_row,'payment_sub_type_code')
ldec_tax_rate							= ids_target_txn.GetItemDecimal(al_row,'tax_rate')
ldt_payment_processed_date			= date(ids_target_txn.GetItemDatetime(al_row,'processed_date'))
ll_award_no								= ids_target_txn.GetItemNumber(al_row,'award_no')


/* if it's a transfer and the adjusted txn amount is positive
 - no need to check tax min and max */
IF ls_new_txn_type_code = 'T' and ldec_adjustment_txn_amount > 0 then
	RETURN 1
END IF


/* HAVE NOT ENTERED A ADJUSTMENT TAX AMOUNT - 
 - NO NEED TO SEE IF THE PAYMENT ALLOWS TAX - ETC .. 
   UNTIL AN AMOUNT HAS BEEN ENTERED */

if ldec_adjustment_tax_amount <> 0  then
	
	//BR 3.30
	IF (ldec_adjustment_tax_amount + ldec_target_tax_amount) < 0 THEN 
		iuo_br_exception.SetMessage('Payment #' + String(ll_target_payment_no) + "  tax amount must not be less than zero. Please check payment.")
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception	
	END IF
	
	
	//BR 3.60
	if ll_award_no > 0 then
		iuo_br_exception.SetMessage('Tax must not be entered if payment is based on an award. Payment # ' + String(ll_target_payment_no) + ' .')
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception	
	end if
	
	/* The transation is processed prior May 30,2004 */
	if ldt_payment_processed_date < 2004-05-30 then
		iuo_br_exception.SetMessage('Tax must not be entered on a payment processed prior to 2004-05-30. Payment # ' + String(ll_target_payment_no) + ' .')
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception		
	end if
	
	/* check payment type/sub type combination to see if allow tax*/
	if iuo_tax_functions.nf_check_payment_type_tax(ls_target_payment_type_code,ls_target_payment_sub_type_code) = "N" THEN
		iuo_br_exception.SetMessage('Payment #' + String(ll_target_payment_no) + " has  payment type/sub type that does not allow tax. Please check payment.")
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception	
	END IF
	
	/* check the recipient type type for allowable tax */
	IF iuo_tax_functions.nf_check_recipient_type_tax(ls_recipient_type_code) = "N" THEN
		iuo_br_exception.SetMessage('Payment #' + String(ll_target_payment_no) + " has  recipient type that does not allow tax. Please check payment.")
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception
	END IF 
	
	/* the Service provider allows for tax recording */
	if iuo_tax_functions.nf_check_provider_tax_flag(ll_recipient_no,ls_recipient_type_code) = "N" THEN
		iuo_br_exception.SetMessage('Payment #' + String(ll_target_payment_no) + " has  provider no/type that does not allow tax. Please check payment.")
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception
	END IF 
	
	
	/* the payment should alreay have tax rate applied - error out if it does not */
	if ldec_tax_rate = 0 then
		iuo_br_exception.SetMessage('Payment #' + String(ll_target_payment_no) + " must have a valid tax rate. Please check payment.")
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception	
	end if	
end if



/* are you performing a pos of neg adustment 
 MIN AND MAX ARE DONE DIFFERENTLY */
IF ldec_adjustment_txn_amount > 0 THEN
	lb_positive_adjustment = true
else
	lb_positive_adjustment = false
end if



IF ls_new_txn_type_code = 'J' and ls_new_txn_sub_type_code = '4' THEN
	
	iuo_tax_functions.nf_get_max_tax_amount(ldec_target_txn_amount,ldec_adjustment_txn_amount ,ldec_tax_rate	,ldec_max_tax_amount)
	
	if (ldec_target_tax_amount + ldec_adjustment_tax_amount ) > ldec_max_tax_amount then
		iuo_br_exception.SetMessage('Payment #' + String(ll_target_payment_no) + " . This adjustment has exceeded the maximum allowable tax of " +  String(ldec_max_tax_amount,'$#,##0.00')  +  " . Please check payment.")
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception	
	end if	
	
ELSE
	
	IF lb_positive_adjustment  THEN
	
		
		SELECT sum(tax_amount),sum(txn_amount)
		into:ldec_max_tax_amount,:ldec_max_txn_amount 
		from APPLIED_CLAIM_TXN
		where txn_type_code 		= :ls_new_txn_type_code and
				txn_sub_type_code = :ls_new_txn_sub_type_code and
				payment_no			= :ll_target_payment_no and
				recipient_no 		= :ll_recipient_no
		using SQLCA;
		SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_tax_rules','SELECT sum(tax_amount) from APPLIED_CLAIM_TXN')


		/* max allowable tax amount*/
		ldec_max_tax_amount = abs(ldec_max_tax_amount)
		
		/* max allowable txn amount */
		ldec_max_txn_amount = abs(ldec_max_txn_amount)
	
		/* tax on remain txn */
		ldec_tax_on_remaining = ( ldec_max_txn_amount  - ldec_adjustment_txn_amount) * ldec_tax_rate / ( 1 + ldec_tax_rate)
		
		
		/* min amount tax can be adjusted */
		ldec_min_adjustment_amount_tax = ldec_max_tax_amount - ldec_tax_on_remaining
	
		/* calculated tax on the adjustment amount */
		ldec_max_adj_tax_amount = (ldec_adjustment_txn_amount * ldec_tax_rate / (1 + ldec_tax_rate))
			
		
		/* if the calculated tax on adjustment amount  > max allowable tax */  
		if ldec_max_adj_tax_amount > ldec_max_tax_amount then
			ldec_max_adj_tax_amount = ldec_max_tax_amount
		end if
	
		/* the max cannot be less then the min */
		if ldec_max_adj_tax_amount < ldec_min_adjustment_amount_tax then
			ldec_min_adjustment_amount_tax  = ldec_max_adj_tax_amount
		end if
	
	ELSE
		
		iuo_tax_functions.nf_get_min_adj_tax_amount(ldec_target_tax_amount,ldec_target_txn_amount,ldec_adjustment_txn_amount ,ldec_tax_rate,ldec_min_adjustment_amount_tax)
		iuo_tax_functions.nf_get_max_adj_tax_amount(ldec_adjustment_txn_amount ,ldec_tax_rate,ldec_max_adj_tax_amount)		
	
		IF ldec_max_adj_tax_amount > ldec_target_tax_amount THEN
			ldec_max_adj_tax_amount  = ldec_target_tax_amount 
		END IF

		END IF


	IF ldec_min_adjustment_amount_tax > ldec_max_adj_tax_amount THEN
		IF ldec_min_adjustment_amount_tax - ldec_max_adj_tax_amount <= 0.01 THEN 
			 ldec_max_adj_tax_amount = ldec_min_adjustment_amount_tax
		END IF
	END IF
	
	if abs(ldec_adjustment_tax_amount)  < ldec_min_adjustment_amount_tax then
		iuo_br_exception.SetMessage('Payment #' + String(ll_target_payment_no) + ". The min allowable tax adjustment has not been entered " +  String(ldec_min_adjustment_amount_tax,'$#,##0.00')  +  " . Please check payment.")
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception	
	end if
	
	if abs(ldec_adjustment_tax_amount)  > ldec_max_adj_tax_amount then
		iuo_br_exception.SetMessage('Payment #' + String(ll_target_payment_no) + ". The max allowable taxadjustment has been exceeded " +  String(ldec_max_adj_tax_amount,'$#,##0.00')  +  " . Please check payment.")
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception	
	end if

end if


RETURN 1


end function

public function integer of_check_transfer_rules (long al_row) throws uo_br_exception;DECIMAL{2}							ldec_adjustment_txn_amount 
DECIMAL{2}							ldec_target_txn_amount	
DECIMAL{2}							ldec_total_payment_amount
LONG									ll_target_payment_no	
LONG									ll_target_txn_no	
STRING								ls_target_payment_type_code
STRING								ls_target_payment_sub_type_code
STRING								ls_to_payment_type_code
STRING								ls_to_payment_sub_type_code
STRING								ls_fatality_flag
STRING								ls_new_txn_sub_type_code
LONG									ll_to_claim_no
long									ll_authorization_no
integer								li_adjustment_quantity	
integer								li_paid_quantity	
string								ls_admin_region_code
long									ll_target_claim_no					

iuo_br_exception.reset_indicator()

//MAINTENANCE DATA
ldec_adjustment_txn_amount 	= ids_maintenance.GetItemDecimal(al_row,'adjustment_txn_amount')
li_adjustment_quantity			= ids_maintenance.GetItemNumber(al_row,'adjustment_quantity')

ls_to_payment_type_code			= ids_batch_control.GetItemString(1,'to_payment_type_code')
ls_to_payment_sub_type_code	= ids_batch_control.GetItemString(1,'to_payment_sub_type_code')
ls_new_txn_sub_type_code		= ids_batch_control.GetItemString(1,'new_txn_sub_type_code')

ll_to_claim_no						= ids_batch_control.GetItemNumber(1,'to_claim_no')


//TARGET TXN DATA
ldec_target_txn_amount				= ids_target_txn.GetItemDecimal(al_row,'txn_amount')
ll_target_payment_no					= ids_target_txn.GetItemNumber(al_row,'target_payment_no')
ll_target_claim_no					= ids_target_txn.GetItemNumber(al_row,'claim_no')
ll_target_txn_no					= ids_target_txn.GetItemNumber(al_row,'target_txn_no')
ls_target_payment_type_code		= ids_target_txn.GetItemString(al_row,'payment_type_code')
ls_target_payment_sub_type_code	= ids_target_txn.GetItemString(al_row,'payment_sub_type_code')
ll_authorization_no 					= ids_target_txn.GetItemNumber(al_row,'authorization_no')
li_paid_quantity						= ids_target_txn.GetItemNumber(al_row,'paid_quantity')
ldec_total_payment_amount			= ids_target_txn.GetItemDecimal(al_row,'total_payment_amount')

//BR 4.30
/* A ZERO AMOUNT MUST NOT BE TRANSFERED 
*/
IF ldec_adjustment_txn_amount = 0 THEN
	iuo_br_exception.SetMessage('Transfering $0.00 is not allowed. Payment #' + String(ll_target_payment_no))
	iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
	THROW iuo_br_exception
END IF


//BR 4.80
/* The transfer amount must be the reverse sign of the target amount.
*/
IF Sign(ldec_total_payment_amount) = Sign(ldec_adjustment_txn_amount) THEN
	iuo_br_exception.SetMessage('The transfer amount must be the reverse sign of the target transaction amount. Payment #' + String(ll_target_payment_no) )
	iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
	THROW iuo_br_exception
End if


//BR 4.50
/* AMOUNT OF REVERSAL PAYMENT TYPE TXN MUST NOT EXCEED ABS VALUE OF TARGET TXN 
*/
IF ldec_target_txn_amount < 0 THEN
	
	IF Abs(ldec_target_txn_amount) - Abs(ldec_adjustment_txn_amount) < 0 THEN		
		iuo_br_exception.SetMessage('The target transaction for ' + String(ldec_target_txn_amount,'$#,##0.00') + ' cannot be adjusted by ' + String(ldec_adjustment_txn_amount,'$#,##0.00') + ' this would result in an overadjustment. Payment #' + String(ll_target_payment_no) )
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception
	End if	
	
END IF


//BR 10.70
IF ls_new_txn_sub_type_code = '6' or ls_new_txn_sub_type_code = '9' THEN
	
	
	IF ls_to_payment_type_code = '18' THEN
		SELECT IsNull(fatality_flag,'N') into :ls_fatality_flag
		FROM   CLAIM a
				  LEFT OUTER JOIN DIFFICULT_CLAIM b ON  a.claim_no = b.claim_no
		WHERE a.claim_no = :ll_to_claim_no
		USING SQLCA;
		
		SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_payment_type_correction_rules','SELECT IsNull(fatality_flag,"N") into :ls_fatality_flag')
	
		IF ls_fatality_flag = 'N' THEN
			iuo_br_exception.SetMessage('Burial Expenses must not be paid on non-fatal claims. Payment #' + String(ll_target_payment_no))
			iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
			THROW iuo_br_exception
		END IF
	END IF
	
	
	//8.100	On a partial claim transfer of a payment, there must be a minimum quantity of on the resulting transaction for the From Claim and a minimum quantity of one on the resulting transaction for the To Claim.*/
	
		/* is this a claim transfer and does payment have an authorization */
		IF ll_authorization_no > 0 and  ls_new_txn_sub_type_code = '6' then
	
			/* check for partial transfer */
			IF ldec_target_txn_amount <> (ldec_adjustment_txn_amount * -1) then
					//A minimum quantity of one must be transferred on a 
					//partial claim transfer on a payment with an authorized amount
				if (li_adjustment_quantity * -1) < 1 then
					iuo_br_exception.SetMessage('R 8.100 - A minimum quantity of one must be transferred on a partial claim transfer on a payment with an authorized amount. Payment #' + String(ll_target_payment_no))
					iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
					THROW iuo_br_exception
				end if
				
			end if

		END IF

end if



/* br 1.30 */
if ls_new_txn_sub_type_code = '6' then
	
	SELECT isnull(dbo.CLAIM.admin_region_code,'')  
	INTO :ls_admin_region_code  
	FROM dbo.CLAIM  
	WHERE dbo.CLAIM.claim_no = :ll_to_claim_no
	using SQLCA;
	SQLCA.nf_handle_error('n_claim_cost_maintenance', '', 'of_check_cost_transfer_rules - Embedded SQL:Select from CLAIM') 

	IF ls_admin_region_code = '' THEN
		iuo_br_exception.SetMessage('Error - No Admin region found for claim no ' + string(ll_to_claim_no) + ' . ')
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)	
		THROW iuo_br_exception
	END IF
		
end if


RETURN 1


end function

public function integer of_check_valid_coc_period (long al_coc_period) throws uo_br_exception;long ll_count
long ll_current_coc_period
long ll_previous_coc_period


iuo_br_exception.reset_indicator()

/* 1.10	The cost of claims period must be open.  The only open periods are the current period or period immediately prior to the current period*/

if is_module_code = '005' then
	
	SELECT current_coc_period
	into :ll_current_coc_period
	FROM Coc_Control
	WHERE (LEFT(CONVERT(CHAR(6),current_coc_period),4) = DATEPART(YEAR,GETDATE())) AND
	CONVERT(INT,RIGHT(current_coc_period,2)) = CONVERT(INT,DATEPART(MM,GETDATE()))
	using SQLCA;
	
	IF SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_valid_coc_period','select count(*)') = 100 THEN
		iuo_br_exception.SetMessage('Problem locating current coc period for' + string(al_coc_period))
		THROW iuo_br_exception	
	END IF
	
	if al_coc_period = ll_current_coc_period  then /* then coc period found */
	else
		
		SELECT previous_coc_period 
		into :ll_previous_coc_period 
		FROM Coc_Control
		WHERE (LEFT(CONVERT(CHAR(6),current_coc_period),4) = DATEPART(YEAR,GETDATE())) AND
		CONVERT(INT,RIGHT(current_coc_period,2)) = CONVERT(INT,DATEPART(MM,GETDATE())) AND
		previous_coc_closed_flag = 'N';
			
	
		IF SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_valid_coc_period','select count(*)') = 100 THEN
			iuo_br_exception.SetMessage('Problem locating previous coc period for' + string(al_coc_period))
			THROW iuo_br_exception	
		END IF
	
		if ll_previous_coc_period  = al_coc_period then
			
		else
			iuo_br_exception.SetMessage('Problem locating previous closed coc period for' + string(al_coc_period))
			THROW iuo_br_exception	
		END IF
	
	end if

end if


return 1
end function

public function integer of_isvalid_cheque (long al_cheque_no, datetime adt_cheque_deposit_date) throws uo_br_exception;LONG				ll_batch_cheque_no
DATETIME			ldt_batch_cheque_deposit_date

iuo_br_exception.reset_indicator()

//Make sure the cheque_no is filled in
IF al_cheque_no = 0  OR IsNull(al_cheque_no)Then
	iuo_br_exception.SetMessage('Cheque number cannot be zero when canceling a cheque.')
	THROW iuo_br_exception
END IF

//Make sure the cheque_deposit_date is filled in
IF IsNull(adt_cheque_deposit_date) Then
	iuo_br_exception.SetMessage('Cheque issue date must be supplied when canceling a cheque.')
	THROW iuo_br_exception
END IF


return 1
end function

public function integer of_isvalid_claim_no (long al_claim_no) throws uo_br_exception;LONG			ll_count

iuo_br_exception.reset_indicator()

SELECT count(*) into :ll_count
FROM CLAIM
WHERE claim_no = :al_claim_no
USING SQLCA;

SQLCA.nf_handle_error('n_claim_cost_maintenance','of_isvalid_claim_no','select count(*)')

IF ll_count = 0 or IsNull(ll_count) Then
	iuo_br_exception.SetMessage('Claim number ' + String(al_claim_no) + ' is invalid.')
	iuo_br_exception.setindicators(0,0,al_claim_no)
	THROW iuo_br_exception
END IF


return 1
end function

public function integer of_isvalid_cost_alloc (long al_cost_alloc_no, long al_cost_alloc_operation_no) throws uo_br_exception;LONG			ll_count

/*

9.140	The 'transferring cost from' employer and operation must be valid.

9.150	The 'transferring cost to' employer and operation must be valid.

*/


iuo_br_exception.reset_indicator()

SELECT count(*) into :ll_count
FROM OPERATION
WHERE employer_no = :al_cost_alloc_no
  AND operation_no = :al_cost_alloc_operation_no
USING SQLCA;

SQLCA.nf_handle_error('n_claim_cost_maintenance','of_isvalid_cost_alloc','select count(*)')

IF ll_count = 0 or IsNull(ll_count) Then
	iuo_br_exception.SetMessage('Cost allocation ' + String(al_cost_alloc_no) &
					+ '/' + String(al_cost_alloc_operation_no) + ' does not exist.')
	THROW iuo_br_exception
END IF

return 1
end function

public function integer of_isvalid_explanation (string as_explanation) throws uo_br_exception;STRING		ls_char
INTEGER		li_explanation_len, li_counter, li_char_counter

iuo_br_exception.reset_indicator()


li_explanation_len = Len(as_explanation)


// constraint requires at least 5 characters that are letters or numbers in the explanation string
FOR li_counter = 1 to li_explanation_len
	ls_char = Mid(as_explanation,li_counter,1)
	IF ls_char = ' ' AND li_counter = 1 THEN
		// explanation starts with a blank - constraint problem
		EXIT
	END IF
	
	CHOOSE CASE Lower(ls_char)
		CASE '1','2','3','4','5','6','7','8','9','0','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'
			li_char_counter = li_char_counter + 1
	END CHOOSE
	
	IF li_char_counter = 5 THEN EXIT // there are at least 5 alphanumeric characters - OK for constraint
NEXT

IF as_explanation = '' or IsNull(as_explanation) Then
	iuo_br_exception.SetMessage('Explanation is blank. Please enter an explanation.')
	THROW iuo_br_exception

ElseIF li_char_counter = 0 THEN
	// the only reason that li_char_counter = 0 is that explanation started with a blank
	iuo_br_exception.SetMessage('Explanation cannot start with a blank.')
	THROW iuo_br_exception
	
ElseIF li_char_counter < 5 Then
	iuo_br_exception.SetMessage('Explanation must be 5 or more characters.')
	THROW iuo_br_exception
END IF

return 1

end function

public function integer of_isvalid_payment_no (long al_payment_no) throws uo_br_exception;LONG			ll_count

iuo_br_exception.reset_indicator()


SELECT count(*) into :ll_count
FROM PAYMENT
WHERE payment_no = :al_payment_no
USING SQLCA;

SQLCA.nf_handle_error('n_claim_cost_maintenance','of_isvalid_payment_no','select count(*)')

IF ll_count = 0 or IsNull(ll_count) Then
	iuo_br_exception.SetMessage('Invalid payment number' + String(al_payment_no))
	iuo_br_exception.setindicators(al_payment_no,0,0)

	THROW iuo_br_exception
END IF


return 1
end function

public function integer of_isvalid_payment_type (string as_payment_type_code, string as_payment_sub_type_code) throws uo_br_exception;LONG			ll_count


iuo_br_exception.reset_indicator()

SELECT count(*) into :ll_count
FROM Payment_Sub_Type
WHERE payment_type_code = :as_payment_type_code
  AND payment_sub_type_code = :as_payment_sub_type_code
USING SQLCA;
SQLCA.nf_handle_error('n_claim_cost_maintenance','of_isvalid_payment_type','select count(*)')

IF ll_count = 0 or IsNull(ll_count) Then
	iuo_br_exception.SetMessage('Please enter a valid payment type combination.')
	THROW iuo_br_exception
END IF



return 1
end function

public function integer of_isvalid_txn_no (long al_txn_no) throws uo_br_exception;LONG			ll_count

iuo_br_exception.reset_indicator()

SELECT count(*) into :ll_count
FROM APPLIED_CLAIM_TXN
WHERE txn_no = :al_txn_no
USING SQLCA;

SQLCA.nf_handle_error('n_claim_cost_maintenance','of_isvalid_txn_no','select count(*)')

IF ll_count = 0 or IsNull(ll_count) Then
	iuo_br_exception.SetMessage('Invalid txn number' + String(al_txn_no))
	iuo_br_exception.setindicators(0,al_txn_no,0)

	THROW iuo_br_exception
END IF


return 1
end function

public function integer of_isvalid_txn_type (string as_txn_type_code, string as_txn_sub_type_code) throws uo_br_exception;LONG			ll_count

iuo_br_exception.reset_indicator()

//BR 1.20	The creation of transactions must be restricted to active transaction type/sub-type combinations that are allowed in the claim cost maintenance module. (I.E. Txn_Type_Combination claim_cost_maintenance_flag = Yes and active_flag = Yes)

SELECT count(*) into :ll_count
FROM Txn_Type_Combination
WHERE txn_type_code = :as_txn_type_code
  AND txn_sub_type_code = :as_txn_sub_type_code
  AND claim_cost_maintenance_flag = 'Y'
  AND active_flag = 'Y'
USING SQLCA;

SQLCA.nf_handle_error('n_claim_cost_maintenance','of_isvalid_txn_type','select count(*)')

IF ll_count = 0 or IsNull(ll_count) Then
	iuo_br_exception.SetMessage('Please enter a valid txn type combination.')
	THROW iuo_br_exception
END IF

return 1
end function

public function integer of_retrieve_target_txn_info (long al_target_txn_no[]);LONG			ll_rows

iuo_br_exception.reset_indicator()

//Returns the number of rows retrieved

ll_rows = ids_target_txn.Retrieve(al_target_txn_no)

IF ll_rows = -1 Then
	SignalError(-666,'Error retrieving target transaction information.')
Elseif ll_rows <> UpperBound(al_target_txn_no) Then
	SignalError(-666,'Rows retrieved does not match the number of target transactions supplied.')
End if

SQLCA.nf_handle_error('n_claim_cost_maintenance','of_retrieve_target_txn_info()','Retrieve')

return ll_rows
end function

public function integer of_check_payment_type_correction_rules (long al_row) throws uo_br_exception;STRING									ls_from_payment_type_code			
STRING									ls_from_payment_sub_type_code
STRING									ls_to_payment_type_code
STRING									ls_to_payment_sub_type_code
STRING									ls_target_payment_type_code
STRING									ls_target_payment_sub_type_code
STRING									ls_active_flag
STRING									ls_target_benefit_category_code
STRING									ls_to_benefit_category_code
STRING									ls_recipient_type_code
string										ls_authorization_type_code_from
string										ls_authorization_type_code_to
string										ls_payment_message_group_code_fr
string										ls_payment_message_group_code_to
string										ls_EFB_type
STRING									ls_find
BOOLEAN								lb_RLOE_VR_exception
LONG										ll_found_row
LONG										ll_recipient_no
LONG										ll_target_payment_no
LONG										ll_target_txn_no
LONG										ll_target_claim_no
INTEGER									li_rtn
decimal {2}								ldec_adjustment_tax_amount
decimal {2} 								ldcm_target_benefit_level_percentage
decimal {2}								ldcm_to_benefit_level_percentage
STRING									ls_selected


ls_target_payment_type_code			= ids_target_txn.GetItemString(al_row,'payment_type_code')
ls_target_payment_sub_type_code	= ids_target_txn.GetItemString(al_row,'payment_sub_type_code')
ls_recipient_type_code					= ids_target_txn.GetItemString(al_row,'recipient_type_code')
ll_recipient_no								= ids_target_txn.GetItemNumber(al_row,'recipient_no')
ll_target_payment_no						= ids_target_txn.GetItemNumber(al_row,'target_payment_no')
ll_target_txn_no							= ids_target_txn.GetItemNumber(al_row,'target_txn_no')
ll_target_claim_no							= ids_target_txn.GetItemNumber(al_row,'claim_no')
ls_to_payment_type_code				= ids_batch_control.GetItemString(1,'to_payment_type_code')
ls_to_payment_sub_type_code			= ids_batch_control.GetItemString(1,'to_payment_sub_type_code')
ls_from_payment_type_code			= ids_batch_control.GetItemString(1,'from_payment_type_code')
ls_from_payment_sub_type_code		= ids_batch_control.GetItemString(1,'from_payment_sub_type_code')
ldec_adjustment_tax_amount 			= ids_maintenance.GetItemDecimal(al_row,'adjustment_tax_amount')

iuo_br_exception.reset_indicator()

//BR 10.10 -The payment type/subtype transfer ‘To’ information must not be the same as the transfer ‘from’ payment type/subtype.
/* pay type/sub type from and to must be different
*/
IF ls_target_payment_type_code = ls_to_payment_type_code and &
   ls_target_payment_sub_type_code = ls_to_payment_sub_type_code THEN
	iuo_br_exception.SetMessage('The new payment type/ sub type must be different then the old type. See payment #' + String(ll_target_payment_no))
	iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
	THROW iuo_br_exception
End if


//BR 10.20 - The payment type/subtype transfer ‘To’ information must not be the same as the transfer ‘from’ payment type/subtype.
/* All transactions selected for a payment type correction must have the same payment type and sub type.
*/
IF is_module_code = '005' THEN // rule only applies to Claim Cost Maintenance
	//Find payments with a different payment_type_code than the control value
	ls_find = 'payment_type_code <> "' + ls_from_payment_type_code + '" or payment_sub_type_code <> "' + ls_from_payment_sub_type_code + '"'
	ll_found_row = ids_target_txn.Find(ls_find,1,ids_target_txn.RowCount())
	IF ll_found_row < 0 Then 
		SignalError(-666,'Error finding payment type code')
	Elseif ll_found_row > 0 THEN
		iuo_br_exception.SetMessage('The selected transactions must all have the same payment type as the "Pay Type Fr:" . ')
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception
	End if
END IF

//BR 10.30 - The payment type/subtype transfer ‘To’ information must not be the same as the transfer ‘from’ payment type/subtype.

/* check to see if warning should be checked */
if ib_suppress_warnings = false then 
	
	/* check to see if warning should be displayed based on
	 if user seen the message and hit yes to all */
	IF ib_suppress_message3 = FALSE THEN
			
		//PAYMENT TYPE
		SELECT active_flag into :ls_active_flag
		FROM Payment_Type
		WHERE payment_type_code = :ls_to_payment_type_code
		USING SQLCA;
		SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_payment_type_correction_rules','SELECT active_flag into :ls_active_flag')
		
		IF ls_active_flag = 'N' THEN
			li_rtn = f_produce_messagebox('Payment type "' + ls_to_payment_type_code + '" is inactive.  See payment #' + String(ll_target_payment_no) + '.')
			IF li_rtn = 2 Then 
				iuo_br_exception.SetMessage('')
				THROW iuo_br_exception
			ELSEIF li_rtn = 3 then
				ib_suppress_message3 = TRUE
			end if	
		END IF				
		
	END IF
	
	
	IF ib_suppress_message4 = FALSE THEN
		
		//PAYMENT SUB TYPE
		SELECT	active_flag into :ls_active_flag
		FROM		Payment_Sub_Type
		WHERE	payment_sub_type_code = :ls_to_payment_sub_type_code
		AND		payment_type_code = :ls_to_payment_type_code
		USING SQLCA;
		
		SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_payment_type_correction_rules','SELECT active_flag into :ls_active_flag')
		
		IF ls_active_flag = 'N' and ls_to_payment_sub_type_code <> '' THEN
			
			li_rtn = f_produce_messagebox('Payment type/sub type "' + ls_to_payment_type_code + '"/"' + ls_to_payment_sub_type_code + '" is inactive.  See payment #' + String(ll_target_payment_no) +  '.')
	
			IF li_rtn = 2 Then 
				iuo_br_exception.SetMessage('')
				THROW iuo_br_exception
			ELSEIF li_rtn = 3 then
				ib_suppress_message4 = TRUE
			end if
			
			
		END IF
	END IF
end if


/*10.45 	A transfer of an account payment type may cross benefit categories within the account payment benefit categories for payment types within the account/invoice payment grouping with the following exceptions:
•	An NBMS payment types must not be used for either side of a payment type correction 
•	An automated WRC invoice payment payable to the WRC service provider (recipient 3889) must not be transferred to another payment type
•	Payment types for Atlantic Blue Cross Care prescription payments must not be used for either side of a payment type transfer
*/


select   DISTINCT isnull(authorization_type_code,'')
into 		:ls_authorization_type_code_from
from     Payment_Combination  
WHERE 	payment_type_code = :ls_target_payment_type_code
using 	sqlca;
if SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_claim_correction_rules','SELECT  DISTINCT authorization_type_code Payment_Combination ') = 100 then
	SignalError(-666,'Found no authorizations for payment type code = ' + ls_target_payment_type_code)
elseif ls_authorization_type_code_from = '' then
	SignalError(-666,'Found no authorizations for payment type code = ' + ls_target_payment_type_code)
end if

select   DISTINCT isnull(authorization_type_code,'')
into 		:ls_authorization_type_code_to
from     Payment_Combination  
WHERE 	payment_type_code = :ls_to_payment_type_code
using 	sqlca;
if SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_claim_correction_rules','SELECT  DISTINCT authorization_type_code Payment_Combination ') = 100 then
	SignalError(-666,'Found no authorizations for payment type code = ' + ls_target_payment_type_code)
elseif ls_authorization_type_code_to = '' then
	SignalError(-666,'Found no authorizations for payment type code = ' + ls_target_payment_type_code)
end if

IF (ls_authorization_type_code_from = 'act' AND ls_authorization_type_code_to = 'act') THEN
	// OK
ELSE
	// BR 10.42 - A three day withhold payment (‘3D’ payment type) must not be transferred to another payment type.
	IF (ls_target_payment_type_code = '3D' OR ls_to_payment_type_code = '3D' ) THEN
		iuo_br_exception.SetMessage('A three day withhold of payment (‘3D’ payment type) payment must not be transferred to another payment type.  See payment #' + String(ll_target_payment_no))
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception
	END IF
	
	/* non account payment */	
	SELECT benefit_category_code,
			 benefit_level_percentage
	INTO  :ls_target_benefit_category_code,
		   :ldcm_target_benefit_level_percentage
	FROM Payment_Type
	WHERE payment_type_code = :ls_target_payment_type_code
	USING SQLCA;
	if SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_claim_correction_rules','SELECT  DISTINCT authorization_type_code Payment_type ') = 100 then
		SignalError(-666,'Found no benefit_category_code for payment type code = ' + ls_from_payment_type_code)
	end if

	SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_payment_type_correction_rules','SELECT benefit_category_code INTO :ls_benefit_category_code')
	SELECT benefit_category_code, 
	       benefit_level_percentage
	INTO :ls_to_benefit_category_code,
		  :ldcm_to_benefit_level_percentage
	FROM Payment_Type
	WHERE payment_type_code = :ls_to_payment_type_code
	USING SQLCA;	
	if SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_payment_type_correction_rules','SELECT benefit_category_code INTO :ls_benefit_category_code') = 100 then
			SignalError(-666,'Found no benefit_category_code for payment type code = ' + ls_to_payment_type_code)
	end if
	
	// BR 10.40
	// 10.40	A payment type transfer must not cross benefit categories for non-Account payments except between
	// ·	An RLOE payment with a benefit percentage level of 80% (' 1') and a voc-rehab (VR) payment type at the same benefit percentage level (payment types B1-B6) 
	// ·	An RLOE payment with a benefit percentage level of 85% ('-1') and a voc-rehab (VR) payment type at the same benefit percentage level (payment types A1-A6) 
	// ·	An RLOE payment with a benefit percentage level of 90% ('.1' only; excluding '.5') and a voc-rehab (VR) payment type at the same benefit percentage level (payment types 01-06)  
	// ·	A history regular loss of earnings payment and a history voc-rehab payment (HR to HV)
	CHOOSE CASE TRUE
		CASE (ls_target_benefit_category_code = 'RLOE' and ls_to_benefit_category_code = 'VR') OR (ls_target_benefit_category_code = 'VR' AND ls_to_benefit_category_code = 'RLOE')
			IF (ls_target_payment_type_code = '.5' AND ls_to_benefit_category_code = 'VR' AND ldcm_to_benefit_level_percentage = .9000)  OR &
					(ls_target_benefit_category_code = 'VR' AND ldcm_target_benefit_level_percentage = .9000 AND ls_to_payment_type_code = '.5') THEN
					// even though .5 has ben level = .90, it cannot be transferred with VR with .90
				iuo_br_exception.SetMessage('Payment transfers must not cross benefit categories for non account payments.  See payment #' + String(ll_target_payment_no) + '.')
				iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
				THROW iuo_br_exception
			ELSEIF (ls_target_payment_type_code = 'HR' AND ls_to_payment_type_code = 'HV' ) OR (ls_target_payment_type_code = 'HV' AND ls_to_payment_type_code = 'HR' ) THEN
				lb_RLOE_VR_exception = TRUE
			ELSEIF ldcm_target_benefit_level_percentage = ldcm_to_benefit_level_percentage THEN
				lb_RLOE_VR_exception = TRUE
			ELSE
				iuo_br_exception.SetMessage('Payment transfers must not cross benefit categories for non account payments.  See payment #' + String(ll_target_payment_no) + '.')
				iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
				THROW iuo_br_exception
			END IF
		CASE (ls_target_benefit_category_code <> ls_to_benefit_category_code) AND ls_target_benefit_category_code <> 'MA' AND ls_to_benefit_category_code <> 'MA'
			// both categories are not MA & they do not match & not part of exceptions in case above
			iuo_br_exception.SetMessage('Payment transfers must not cross benefit categories for non account payments.  See payment #' + String(ll_target_payment_no) + '.')
			iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
			THROW iuo_br_exception
	END CHOOSE
	
	
/*10.41	Non-account payments must not be transferred within the benefit category except
	A payment in the LTD benefit category may be transferred to another payment type within the LTD benefit category having the same benefit percentage level unless the payment type is a return to work incentive payment which must not be used for either side of a payment type correction.
	A payment in the VR benefit category may be transferred to another payment type within the VR benefit category having the same benefit percentage level but only for those payment types where the benefit percentage level is greater than zero.
*/
	CHOOSE CASE TRUE
		CASE ls_target_benefit_category_code = ls_to_benefit_category_code
			CHOOSE CASE ls_to_benefit_category_code
				CASE 'LTD'
					IF ldcm_target_benefit_level_percentage = ldcm_to_benefit_level_percentage THEN
						IF ls_target_payment_type_code = 'R2' OR ls_target_payment_type_code = 'R3' THEN
							iuo_br_exception.SetMessage('RTW Incentive payments must not be used as target payments.  See payment #' + String(ll_target_payment_no) + '.')
							iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
							THROW iuo_br_exception
						ELSEIF ls_to_payment_type_code = 'R2' OR ls_to_payment_type_code = 'R3' THEN
							iuo_br_exception.SetMessage('Target payments must not be transferred to RTW Incentive payment types.  See payment #' + String(ll_target_payment_no) + '.')
							iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
							THROW iuo_br_exception
						ELSE
							//OK
						END IF
					ELSE
						iuo_br_exception.SetMessage('Payment transfers must be within the same benefit level for LTD payments.  See payment #' + String(ll_target_payment_no) + '.')
						iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
						THROW iuo_br_exception
					END IF
				CASE 'VR'
					IF ldcm_target_benefit_level_percentage = ldcm_to_benefit_level_percentage THEN
						IF ldcm_target_benefit_level_percentage > 0 THEN
							// OK
						ELSE
							iuo_br_exception.SetMessage('Payment type transfers cannot involve VR payment types where the benefit level = 0.  See payment #' + String(ll_target_payment_no) + '.')
							iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)		
							THROW iuo_br_exception
						END IF
					ELSE
						iuo_br_exception.SetMessage('Payment type transfers must be within the same benefit level for Voc Rehab payments.  See payment #' + String(ll_target_payment_no) + '.')
						iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)	
						THROW iuo_br_exception
					END IF
				CASE ELSE
					IF ls_target_benefit_category_code <> 'MA' AND ls_to_benefit_category_code <> 'MA' THEN
						iuo_br_exception.SetMessage('Payment type transfers must not be transferred within the '+ls_to_benefit_category_code+' benefit category.  See payment #' + String(ll_target_payment_no) + '.')
						iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)	
						THROW iuo_br_exception
					END IF
			END CHOOSE
	END CHOOSE
	
END IF


//BR 10.46 	A payment type transfer must not cross payment groupings.

/* get the payment_message_group_code for the payment type from and to */
select ISNULL(payment_message_group_code,'')
INTO  :ls_payment_message_group_code_fr
from   Module_Payment_Sub_Type 
where  module_code = :is_module_code and    
       payment_type_code =  :ls_target_payment_type_code         And    
		 payment_sub_type_code = :ls_target_payment_sub_type_code
using SQLCA;
if SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_payment_type_correction_rules','SELECT Module_Payment_Sub_Type ' ) = 100 then
	signalerror(-666,"Found no message group code for the FROM payment type/sub type")
end if

select ISNULL(payment_message_group_code,'')
INTO  :ls_payment_message_group_code_to
from   Module_Payment_Sub_Type 
where  module_code = :is_module_code and    
       payment_type_code =  :ls_to_payment_type_code         And    
		 payment_sub_type_code = :ls_to_payment_sub_type_code
using SQLCA;
if SQLCA.nf_handle_error('n_claim_cost_maintenance','of_check_payment_type_correction_rules','SELECT Module_Payment_Sub_Type ') = 100 then
	signalerror(-666,"Found no message group code for the 'TO' payment type/sub type")
end if

if ls_payment_message_group_code_fr <> ls_payment_message_group_code_to then
	iuo_br_exception.SetMessage('A payment type transfer must not cross payment groupings.  See payment #' + String(ll_target_payment_no))
	iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
	THROW iuo_br_exception
end if
	
	
/*10.45 	A transfer of an account payment type may cross benefit categories within the account payment benefit categories for payment types within the account/invoice payment grouping with the following exceptions:
•	An NBMS payment types must not be used for either side of a payment type correction 
•	An automated WRC invoice payment payable to the WRC service provider (recipient 3889) must not be transferred to another payment type
•	Payment types for Atlantic Blue Cross Care prescription payments must not be used for either side of a payment type transfer
*/

if ldec_adjustment_tax_amount <> 0 then	
	/* check payment type/sub type combination to see if allow tax*/
	if iuo_tax_functions.nf_check_payment_type_tax(ls_to_payment_type_code,ls_to_payment_sub_type_code) = "N" THEN
		iuo_br_exception.SetMessage('Payment #' + String(ll_target_payment_no) + " CANNOT TRANSFER TO  payment type/sub type that does not allow tax. Please check payment.")
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception	
	END IF
end if

// BR 10.45
if ldec_adjustment_tax_amount = 0 then	
	if iuo_tax_functions.nf_check_payment_type_tax(ls_target_payment_type_code,ls_target_payment_sub_type_code) = "N" and &
			iuo_tax_functions.nf_check_payment_type_tax(ls_to_payment_type_code,ls_to_payment_sub_type_code) = "Y" THEN
		li_rtn = MessageBox('Warning','The "to" payment type can have tax. A tax correction may be required for payment #' + String(ll_target_payment_no) + ', . Do you want to continue?',Question!,YesNo!)
		IF li_rtn = 2 THEN
			iuo_br_exception.SetMessage('')
			THROW iuo_br_exception
		END IF
	END IF
END IF

//BR 10.45
IF ls_target_payment_type_code = '21' THEN
	CHOOSE CASE ls_target_payment_sub_type_code
		CASE '01','02','03','04','05','06','07','08','11'
			iuo_br_exception.SetMessage('NBMS payment types must not be used for either side of a payment type correction.  See payment #' + String(ll_target_payment_no))
			iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
			THROW iuo_br_exception
		CASE '09','10','10','13','14','15','16','17','18'
			iuo_br_exception.SetMessage('NBCA payment types must not be used for either side of a payment type correction.  See payment #' + String(ll_target_payment_no))
			iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
			THROW iuo_br_exception
		CASE '','12'
			// check for associated Early Filing Bonuses
			// warn user if such exist
			nf_check_for_efb_payment(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no,'warning','transferred')

	END CHOOSE
END IF

IF ls_to_payment_type_code = '21' THEN
	CHOOSE CASE ls_to_payment_sub_type_code
		CASE '01','02','03','04','05','06','07','08','11'
			iuo_br_exception.SetMessage('NBMS payment types must not be used for either side of a payment type correction.  See payment #' + String(ll_target_payment_no))
			iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)	
			THROW iuo_br_exception
		CASE '09','10','10','13','14','15','16','17','18'
			iuo_br_exception.SetMessage('NBCA payment types must not be used for either side of a payment type correction.  See payment #' + String(ll_target_payment_no))
			iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
			THROW iuo_br_exception
	END CHOOSE
END IF


//BR 10.45
IF ls_recipient_type_code = 'M' and ll_recipient_no = 3889 THEN
	iuo_br_exception.SetMessage('An automated WRC invoice payment payable to the WRC service provider (recipient 3889) must not be transferred to another payment type.  See payment #' + String(ll_target_payment_no))
	iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)	
	THROW iuo_br_exception
END IF


//BR 10.45
IF is_module_code = '005' THEN // rule only applies to Claim Cost Maintenance
	IF ls_target_payment_type_code = '22' THEN
		CHOOSE CASE ls_target_payment_sub_type_code
			CASE 'BC','RC'
				iuo_br_exception.SetMessage('Atlantic Blue Cross Care payment types must not be used for either side of a payment type correction. See payment #' + String(ll_target_payment_no))
				iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
				THROW iuo_br_exception
		END CHOOSE
	END IF
	
	IF ls_to_payment_type_code = '22' THEN
		CHOOSE CASE ls_to_payment_sub_type_code
			CASE 'BC','RC'
				iuo_br_exception.SetMessage('Atlantic Blue Cross Care payment types must not be used for either side of a payment type correction.  See payment #' + String(ll_target_payment_no))
				iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
				THROW iuo_br_exception
		END CHOOSE
	END IF
END IF


RETURN 1

end function

public function integer of_check_manuals_for_over_adjustments (long al_row) throws uo_br_exception;long	ll_target_txn_no
long	ll_target_claim_no 

decimal {2} ldec_sum_payment_type
decimal {2} ldec_sum_cost_alloc
decimal {2} ldec_adjustment_txn_amount
string  ls_message
integer	li_rc
long ll_target_payment_no


//MAINTENANCE DATA
ldec_adjustment_txn_amount = ids_maintenance.GetItemDecimal(al_row,'adjustment_txn_amount')


ll_target_txn_no 				= ids_target_txn.GetItemNumber(al_row,'target_txn_no')
ll_target_payment_no 		= ids_target_txn.GetItemNumber(al_row,'target_payment_no')
ll_target_claim_no 			= ids_target_txn.GetItemNumber(al_row,'claim_no')

iuo_br_exception.reset_indicator()

//RETURN 0 - NOT BLOCKED
//RETURN 1 - BLOCKED
//RETURN 2 - might be blocked by payment type or cost allocation
//RETURN 3 - might be blocked by payment type (cost allocation not required for payment type)

li_rc = of_check_for_over_adjustments(ll_target_txn_no,ldec_sum_payment_type,ldec_sum_cost_alloc,ls_message)

choose case li_rc 
	case 1
		iuo_br_exception.SetMessage(ls_message)
		iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		THROW iuo_br_exception
	case 2
		// if overadjusting on cost alloc
		if ldec_adjustment_txn_amount + ldec_sum_cost_alloc < 0 then
			iuo_br_exception.SetMessage(ls_message)
			iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
			THROW iuo_br_exception
		end if
		
		// if overadjusting on payment type
		if ldec_adjustment_txn_amount + ldec_sum_payment_type < 0  then
		 	iuo_br_exception.SetMessage(ls_message)
			iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		 	THROW iuo_br_exception
		end if
		
	case 3
		// if overadjusting on payment type
		if ldec_adjustment_txn_amount + ldec_sum_payment_type < 0  then
		 	iuo_br_exception.SetMessage(ls_message)
			iuo_br_exception.setindicators(ll_target_payment_no,ll_target_txn_no ,ll_target_claim_no)
		 	THROW iuo_br_exception
		end if
end choose


return 1
end function

public subroutine nf_check_for_ma_payment (long al_target_payment_no, long al_target_txn_no, long al_target_claim_no, long al_related_txn_no, string as_msg_type, string as_efb_type, string as_correction_type) throws uo_br_exception;LONG		ll_found , ll_rowcount, ll_MA_payment_no
STRING	ls_find, ls_efb_type
INTEGER	li_rtn

ll_rowcount = ids_target_txn.RowCount()

SELECT	top 1 b.payment_no
INTO		:ll_MA_payment_no
FROM		APPLIED_CLAIM_TXN	a,
			PAYMENT				b
WHERE	a.payment_no					= b.payment_no
AND		a.txn_no							= :al_related_txn_no
AND		b.payment_type_code		= '21'
USING SQLCA;
SQLCA.nf_handle_error('n_claim_cost_maintenance','nf_check_for_EFB','SELECT count(*)...NBMS')

IF ll_MA_payment_no > 0 THEN
	ls_find = 'target_payment_no = ' + String(ll_MA_payment_no)
	ll_found = ids_target_txn.Find(ls_find,1,ll_rowcount)
	IF ll_found = 0 THEN
		IF as_msg_type = 'error' THEN
			MessageBox('Error','The target '+as_EFB_type+' Early Filing Bonus payment ('+String(al_target_payment_no)+') has an associated ' &
									  + '~nMedical Account payment ('+ String(ll_MA_payment_no)+'). It must be '+as_correction_type+'.' , StopSign!)
			iuo_br_exception.SetMessage('')
			IF is_module_code = '005' THEN iuo_br_exception.setindicators(al_target_payment_no,al_target_txn_no ,al_target_claim_no)
			THROW iuo_br_exception
			
		ELSEIF as_msg_type = 'warning' THEN
			li_rtn = MessageBox('Warning','The target '+as_EFB_type+' Early Filing Bonus payment ('+String(al_target_payment_no)+') has an associated ' &
									  + '~nMedical Account payment ('+ String(ll_MA_payment_no)+'). It may also need to be '+as_correction_type+'. Do you want to continue?',Question!,YesNo!,2)
			IF li_rtn = 2 THEN
				iuo_br_exception.SetMessage('')
				IF is_module_code = '005' THEN iuo_br_exception.setindicators(al_target_payment_no,al_target_txn_no ,al_target_claim_no)
				THROW iuo_br_exception
			END IF
		END IF
	END IF
END IF

end subroutine

public subroutine nf_check_for_efb_payment (long al_target_payment_no, long al_target_txn_no, long al_target_claim_no, string as_msg_type, string as_correction_type) throws uo_br_exception;LONG		ll_found , ll_rowcount, ll_efb_payment_no_1, ll_efb_payment_no_2, ll_efb_payment_no
STRING	ls_find, ls_efb_type
INTEGER	li_rtn

ll_rowcount = ids_target_txn.RowCount()



SELECT	top 1 b.payment_no
INTO		:ll_efb_payment_no_1
FROM		APPLIED_CLAIM_TXN	a,
			PAYMENT				b
WHERE	a.payment_no					= b.payment_no
AND		a.related_txn_no				= :al_target_txn_no
AND		b.payment_type_code		= '21'
AND		b.payment_sub_type_code = '01'
USING SQLCA;
SQLCA.nf_handle_error('n_claim_cost_maintenance','nf_check_for_EFB','SELECT count(*)...NBMS')

IF ll_efb_payment_no_1 > 0 THEN ls_efb_type = 'NBMS'

SELECT	top 1 b.payment_no
INTO		:ll_efb_payment_no_2
FROM		APPLIED_CLAIM_TXN	a,
			PAYMENT				b
WHERE	a.payment_no					= b.payment_no
AND		a.related_txn_no				= :al_target_txn_no
AND		b.payment_type_code		= '21'
AND		b.payment_sub_type_code = '09'
USING SQLCA;
SQLCA.nf_handle_error('n_claim_cost_maintenance','nf_check_for_EFB','SELECT count(*)...NBCA')

IF ll_efb_payment_no_2 > 0 THEN ls_efb_type = 'NBCA'

IF ll_efb_payment_no_1 > 0 THEN
	ll_efb_payment_no = ll_efb_payment_no_1
ELSEIF ll_efb_payment_no_2 > 0 THEN
	ll_efb_payment_no = ll_efb_payment_no_2
END IF

IF ll_efb_payment_no > 0 THEN
	ls_find = 'target_payment_no = ' + String(ll_EFB_payment_no)
	ll_found = ids_target_txn.Find(ls_find,1,ll_rowcount)
	IF ll_found = 0 THEN
		IF as_msg_type = 'error' THEN
			MessageBox('Error','There is an associated '+ls_EFB_type+' Early Filing Bonus payment ('+String(ll_EFB_payment_no)+')' &
								+ '~nfor this target invoice payment ('+String(al_target_payment_no)+').' &
								+ '~nIt must be '+as_correction_type+'.', StopSign!)
			iuo_br_exception.SetMessage('')
			IF is_module_code = '005' THEN iuo_br_exception.setindicators(al_target_payment_no,al_target_txn_no ,al_target_claim_no)
			THROW iuo_br_exception
			
		ELSEIF as_msg_type = 'warning' THEN
			li_rtn = MessageBox('Warning','There is an associated '+ls_EFB_type+' Early Filing Bonus payment ('+String(ll_EFB_payment_no)+')' &
										+ '~nfor this target invoice payment ('+String(al_target_payment_no)+').' &
										+ '~nIt may also need to be '+as_correction_type+'. Do you want to continue?',Question!,YesNo!,2)
			IF li_rtn = 2 THEN
				iuo_br_exception.SetMessage('')
				IF is_module_code = '005' THEN iuo_br_exception.setindicators(al_target_payment_no,al_target_txn_no ,al_target_claim_no)
				THROW iuo_br_exception
			END IF
		END IF
		
	END IF
END IF

end subroutine

public function integer of_isvalid_authorization_no (decimal adcm_sum_rehab_qty, long al_authorization_no_to, long al_to_claim_no, decimal adcm_sum_amount, long al_from_claim_no) throws uo_br_exception;long 				ll_no_authorizations
long 				ll_claim_no_belonging_to_authorization
decimal {2}		ldcm_authorized_quantity_belonging_to_authorization
decimal {2}		ldcm_paid_quantity_belonging_to_authorization
decimal {2}		ldcm_remaining_qty
DECIMAL{2}		ldcm_authorized_amount_belonging_to_authorization
DECIMAL{2}		ldcm_paid_amount_belonging_to_authorization
DECIMAL{2}		ldcm_remaining_amount 

iuo_br_exception.reset_indicator()

if isnull(adcm_sum_rehab_qty) then adcm_sum_rehab_qty = 0			
if isnull(al_authorization_no_to) then al_authorization_no_to = 0
if isnull(adcm_sum_amount) then adcm_sum_amount = 0

/* check to see if we are dealing with claims linked to an authorizations - the sum of the adjustments  <> 0*/
if (adcm_sum_rehab_qty = 0) then
		
     /*	8.145  The rehab authorization number for the To Claim must not be entered if the From Claim payment(s) are not linked to a rehab authorization.*/
	   IF (al_authorization_no_to <> 0 ) THEN
			  	iuo_br_exception.SetMessage("BR 8.145 - You must not enter a rehab authorization number for Claim # " +   string(al_to_claim_no) + " as none of the payment(s) being transferred for Claim # " + string(al_from_claim_no)  + " have a rehab authorization")

				THROW iuo_br_exception
		END IF
	
else	
	     /*  BR  2.280  The ‘To’ authorization number must be supplied for a claim transfer if the ‘from’ payment has an  authorization number.  */
		 
		if (al_authorization_no_to = 0) then
		    	iuo_br_exception.SetMessage("BR  2.280  - You must enter a rehab authorization number for Claim # " + string(al_to_claim_no) +  "  as the payment(s) being transferred for Claim # " + string(al_from_claim_no) +  " have a rehab authorization.")
				THROW iuo_br_exception
		end if


        SELECT   claim_no,
					authorized_quantity,
					paid_quantity,
					authorized_amount,
					paid_quantity
				  into
				  :ll_claim_no_belonging_to_authorization,
				  :ldcm_authorized_quantity_belonging_to_authorization,
				  :ldcm_paid_quantity_belonging_to_authorization,
				  :ldcm_authorized_amount_belonging_to_authorization,
				  :ldcm_paid_amount_belonging_to_authorization
	from REHAB_TASK_AUTHORIZATION
	 where authorization_no = :al_authorization_no_to
     USING SQLCA;
	SQLCA.nf_handle_error('n_claim_cost_maintenance','of_isvalid_authorization_no','SELECT COUNT(*) FROM REHAB_TASK_AUTHORIZATION')


	IF SQLCA.SQLNRows = 0 Then
		iuo_br_exception.SetMessage('The Authorization to must exist to perform claim correction that have a rehab quantity')
		THROW iuo_br_exception
	end if
	
	 IF SQLCA.SQLNRows > 1 Then
		SignalError(-666,'Detected a data integrity issue with REHAB_TASK_AUTHORIZATION . Authorization No is ' + 	STRING(al_authorization_no_to))
	end if
	
   /*14.10	The rehab authorization number that is entered must exists as an authorization for the To Claim */
	if (ll_claim_no_belonging_to_authorization <> al_to_claim_no) then
	     iuo_br_exception.SetMessage('BR 14.10 - Rehab authorization #' +  string(al_authorization_no_to)  + ' does not exist for Claim # "' +  string(al_to_claim_no)) 
		THROW iuo_br_exception
	end if
		
		
		
/*BR 14.50 The sum of  the quantity transferred from all From Claim payments must be less than or equal to the remaining authorized quantity of the To Claim rehab authorization.  */
	ldcm_remaining_qty = ldcm_authorized_quantity_belonging_to_authorization - ldcm_paid_quantity_belonging_to_authorization
	
	if (adcm_sum_rehab_qty * -1) > ldcm_remaining_qty then
	     iuo_br_exception.SetMessage('BR 14.50 - The paid quantity being transferred to rehab authorization # ' + string(al_authorization_no_to) + ' is greater than the remaining authorized quantity for this authorization. ')
		THROW iuo_br_exception
    end if

	
	
  /*BR 14.60  The sum of the payment amount transferred from all From Claim payments must be less than or equal to the remaining authorized amount of the To Claim rehab authorization, provided the authorized amount is greater than zero. . */		
 /* note - some authorization are not set - they are zero therfore we cannot always check this */
	IF 	ldcm_authorized_amount_belonging_to_authorization  <> 0  then
	
		ldcm_remaining_amount = 	ldcm_authorized_amount_belonging_to_authorization - ldcm_paid_amount_belonging_to_authorization
	
		 if (ldcm_paid_quantity_belonging_to_authorization * -1 ) > ldcm_remaining_amount  then
				  iuo_br_exception.SetMessage('BR 14.60 - The amount paid that is being transferred to rehab authorization # ' + string(al_authorization_no_to) + ' is greater than the remaining authorized amount for this authorization.')  
				  THROW iuo_br_exception
		 end if
			
	end if
	
		
				
end if
	  
Return 1





end function

on n_claim_cost_maintenance.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_claim_cost_maintenance.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

event constructor;ids_maintenance     					= CREATE u_ds
ids_target_txn 						= CREATE u_ds
ids_batch_control						= CREATE u_ds
ids_rehab_task_authorization		= CREATE u_ds

iuo_br_exception 		= CREATE uo_br_exception
iuo_tax_functions		= CREATE n_tax_functions

ids_maintenance.DataObject 	= 'd_new_claim_cost_maintenance'
ids_target_txn.DataObject 		= 'd_target_txn'
ids_batch_control.DataObject 	= 'd_batch_control'
ids_rehab_task_authorization.DataObject 	= 'd_rehab_task_authorization_update'


ib_suppress_message2 = false
ib_suppress_message3 = false
ib_suppress_message4 = false
ib_suppress_message5 = false
ib_suppress_message6 = false
ib_suppress_message7 = false
ib_suppress_message8 = false

ib_suppress_warnings = false

is_benefit_type = ''

ids_target_txn.SetTransObject(SQLCA)

inv_claim_employer = Create n_claim_employer
end event

