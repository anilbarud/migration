$PBExportHeader$w_create_reporting_fee_payments.srw
forward
global type w_create_reporting_fee_payments from w_ancestor
end type
type cb_create from commandbutton within w_create_reporting_fee_payments
end type
type cb_cancel from commandbutton within w_create_reporting_fee_payments
end type
type dw_reporting_fee from u_dw_online within w_create_reporting_fee_payments
end type
type dw_totals from u_dw_online within w_create_reporting_fee_payments
end type
type dw_payment from u_dw_online within w_create_reporting_fee_payments
end type
type dw_unapplied_claim from u_dw_online within w_create_reporting_fee_payments
end type
type dw_payment_document from u_dw_online within w_create_reporting_fee_payments
end type
end forward

global type w_create_reporting_fee_payments from w_ancestor
integer x = 1893
integer y = 48
integer width = 3278
integer height = 3448
string title = "Create Reporting Fee Payments"
string menuname = "m_cmwb_notools"
windowtype windowtype = main!
long backcolor = 67108864
event ue_print ( )
cb_create cb_create
cb_cancel cb_cancel
dw_reporting_fee dw_reporting_fee
dw_totals dw_totals
dw_payment dw_payment
dw_unapplied_claim dw_unapplied_claim
dw_payment_document dw_payment_document
end type
global w_create_reporting_fee_payments w_create_reporting_fee_payments

type variables
W_SHEET		iw_sheet
N_ACCOUNT_PAYMENT_CONTROLLER inv_controller
N_PAYMENT inv_payment
long il_approved_count

end variables

forward prototypes
public function long wf_get_next_payment_identifier (long al_number)
public function long wf_get_next_txn_identifier (long al_number)
public function integer wf_validate_service_provider (long al_number, string as_type)
public subroutine wf_increment_totals (string as_doc_type, decimal adec_fee)
public function long wf_get_next_batch_no ()
public function integer wf_create_payment ()
public function string wf_get_payment_method (long al_provider_no, string as_provider_type_code, string as_provider_sub_type_code)
end prototypes

event ue_print();IF dw_reporting_fee.rowcount() > 0 THEN
	dw_reporting_fee.Object.Datawindow.Print.Orientation = 1
	dw_reporting_fee.print()
END IF
end event

public function long wf_get_next_payment_identifier (long al_number);LONG	ll_result, ll_payment_no

/*	To ensure that we get the next number without, Update the Last_Payment_No table incrementing the 
	last_payment_no by 1  (This will lock it so no one else can get in). Then, read it back          
*/
	UPDATE Last_Payment_No SET last_payment_no = last_payment_no + :al_number using SQLCA;

	ll_result = SQLCA.nf_handle_error("Embedded SQL: Update Last_Payment_No","w_create_reporting_fee_payments","wf_next_payment_identifier")
	IF ll_result < 0 THEN
		Return -1
	END IF

	CHOOSE CASE SQLCA.SQLNRows
/*	If update was successful (ie. SQLNRows would equal 1), read back the identifier
*/	
		CASE 1
			SELECT Last_Payment_No.last_payment_no INTO :ll_payment_no FROM Last_Payment_No using SQLCA;
			ll_result = SQLCA.nf_handle_error("Embedded SQL: Update Last_Payment_No","w_create_reporting_fee_payments","wf_next_payment_identifier")
			IF ll_result < 0 THEN
				Return -1
			END IF
		CASE ELSE
/*		if anything other than 1 record found, display error
*/
			SQLCA.nf_rollback_transaction()
			IF SQLCA.SQLCode <> 0 THEN
				Error.Text = "Error during rollback of Last_Payment_No in function wf_next_payment_no"
				Error.WindowMenu="w_create_reporting_fee_payments"
				Error.Object=""
				Error.ObjectEvent="wf_next_payment_no"
				SignalError()
			END IF		
			MessageBox("Payment Module - Data Integrity Error", string(SQLCA.SQLNRows) + " record(s) found in Last_Payment_No~r~nPlease call the help desk",Exclamation!)
			Return -1

	END CHOOSE
		
Return ll_payment_no

end function

public function long wf_get_next_txn_identifier (long al_number);LONG	ll_result, ll_txn_no


/*	To ensure that we get the next number without, Update the Last_Claim_Txn_no table incrementing the  
	last_txn_no by 1  (This will lock it so no one else can get in). Then, read it back                 
*/
	UPDATE Last_Claim_Txn_No SET last_txn_no = last_txn_no + :al_number using SQLCA;
	ll_result = SQLCA.nf_handle_error("Embedded SQL: Update Last_Claim_Txn_No","w_create_reporting_fee_payments","wf_next_txn_identifier")
	IF ll_result < 0 THEN
		Return -1
	END IF

	CHOOSE CASE SQLCA.SQLNRows
/*		If update was successful (ie. SQLNRows would equal 1), read back the identifier
*/
		CASE 1
			SELECT Last_Claim_Txn_No.last_txn_no INTO :ll_txn_no FROM Last_Claim_Txn_No using SQLCA;
			ll_result = SQLCA.nf_handle_error("Embedded SQL: Update Last_Claim_Txn_No","w_create_reporting_fee_payments","wf_next_txn_identifier")
			IF ll_result < 0 THEN
				Return -1
			END IF
		CASE ELSE
/*		If anything other than 1 record found, display error
*/
			MessageBox("Data Integrity Error", string(SQLCA.SQLNRows) + " record(s) found in Last_Claim_Txn_No~r~nPlease call the help desk",Exclamation!)
			SQLCA.nf_rollback_transaction()
			IF SQLCA.SQLCode <> 0 THEN
				Error.Text = "Error during rollback of Last_Claim_Txn_No in function wf_next_txn_identifier"
				Error.WindowMenu=""
				Error.Object=""
				Error.ObjectEvent="wf_next_txn_identifier"
				SignalError()
			END IF		
			Return -1

	END CHOOSE

Return ll_txn_no


end function

public function integer wf_validate_service_provider (long al_number, string as_type);	LONG	ll_count
	
	IF IsNull(as_type) OR IsNull(al_number) THEN
		MessageBox('Warning','Recipient number or type is missing')
		Return -1
	END IF
		   
			
	SELECT count(*)
	  INTO :ll_count
	  FROM PROVIDER  
	 WHERE ( PROVIDER.provider_no        = :al_number ) AND  
	   	 ( PROVIDER.provider_type_code = :as_type) AND
          ( PROVIDER.active_flag        = 'Y' )  
	 USING SQLCA ;
	 
	SQLCA.nf_handle_error("Embedded SQL: Retrieve on SERVICE_PROVIDER","w_create_report_fee_payments","wf_validate_service_provider")
   
	IF ll_count <> 1 THEN
		RETURN -1
	END IF
	
	RETURN 1
	
	
			
end function

public subroutine wf_increment_totals (string as_doc_type, decimal adec_fee);INTEGER li_row

li_row = dw_totals.GetRow()

IF as_doc_type = 'DC' THEN
	// amount 2 is disallowed
	dw_totals.setitem(li_row,"amount_two",dw_totals.GetItemNumber(li_row,'amount_two') + adec_fee )
	dw_totals.setitem(li_row,"total_two",dw_totals.GetItemNumber(li_row,'total_two') + adec_fee )
ELSEIF as_doc_type = 'NC' THEN
	// amount 1 is no claim
	dw_totals.setitem(li_row,"amount_one",dw_totals.GetItemNumber(li_row,'amount_one') + adec_fee )
	dw_totals.setitem(li_row,"total_two",dw_totals.GetItemNumber(li_row,'total_two') + adec_fee )
END IF
end subroutine

public function long wf_get_next_batch_no ();LONG		ll_next_batch_no

UPDATE Last_Batch_No
Set last_batch_no = last_batch_no + 1
USING SQLCA;

SQLCA.nf_handle_error('w_create_reporting_fee_payments','wf_get_next_batch_no','UPDATE Last_Batch_No')

SELECT last_batch_no into :ll_next_batch_no
FROM Last_Batch_No
USING SQLCA;

SQLCA.nf_handle_error('w_create_reporting_fee_payments','wf_get_next_batch_no','select Last_Batch_No')

IF IsNull(ll_next_batch_no ) or ll_next_batch_no = 0 Then 
	SignalError(-666,'Crashing... crashing... crash.')
End if


RETURN ll_next_batch_no
end function

public function integer wf_create_payment ();/* this is where we will create the actual payment.
*/
INT 			li_counter,li_rowcount,li_count_back,li_row
LONG        ll_payment_no = 0,ll_txn_no = 0,ll_foundrow
STRING      ls_payment_type_code ,ls_sub_type
DATETIME    ldt_paid_to,ldt_current_date
DATASTORE   lds_reporting_fee_combination
STRING      ls_payment_method_code


/* from the datawindow
*/
STRING  		ls_name,ls_recipient_type_code,ls_sub_type_code,ls_type_code , ls_doc_type
STRING      ls_method_code,ls_eligibility_code,ls_admin_region,ls_expression
LONG        ll_claim_no,ll_docid,ll_recipient_no,ll_no,ll_found,ll_nbr
DATETIME    ldt_date_on_doc,ldt_date_received,ldt_processing_date

DECIMAL		ld_total_txn_amount_created
DATE			ldt_date, ldt_document_date_check

STRING		ls_address_line1, ls_address_line2, ls_city, ls_postal_code, ls_country, ls_prov_state_code

// pr 2795
DECIMAL ldec_fee, ldec_nbms_reporting_fee, ldec_nbca_reporting_fee
DATETIME ldt_maxdate

LONG			ll_next_batch_no
LONG			ll_txn_count

STRING		ls_given_names
STRING		ls_last_name

STRING		ls_claim_status_type_code

/* set the pointer
*/
setpointer(hourglass!)

/* grab the rowcount number
*/
li_rowcount = dw_reporting_fee.rowcount()
IF li_rowcount < 0 THEN
	RETURN -1
END IF

/* grab the current datetime
*/
ldt_current_date = f_server_datetime()

/*set scheduled processing date - necessary for check mandatory in n_payment
*/
ldt_date = Date(ldt_current_date)
ll_no    = DayNumber(ldt_date)
IF ll_no <= 5 THEN					// Thursday
	ldt_processing_date = DateTime(RelativeDate(ldt_date, 5 - ll_no))	
ELSE
	ldt_processing_date = DateTime(RelativeDate(ldt_date, 7 - (ll_no - 5)))
END IF

/* Do the retrieve for the PAYMENT_DOCUMENT datawindow
*/
dw_payment_document.retrieve()
IF SQLCA.nf_handle_error('w_create_reporting_fee_payments','wf_create_payment','dw_payment_document') < 0 THEN
	RETURN -1
END IF

/* now retrieve the reporting fee combination values which we will use to populate
   the payment_sub_type_code
*/
lds_reporting_fee_combination = CREATE datastore
lds_reporting_fee_combination.DataObject = 'd_reporting_fee_combo'
lds_reporting_fee_combination.SetTransObject(SQLCA)
lds_reporting_fee_combination.Retrieve()

SQLCA.nf_handle_error('w_create_reporting_fee_payments','wf_create_payment','lds_reporting_fee_combination.Retrieve()')

FOR li_counter = 1 TO li_rowcount
	
	/* grab the information that we will need
	*/
	ll_claim_no            = dw_reporting_fee.getitemnumber(li_counter,"claim_no")
	ll_recipient_no        = dw_reporting_fee.getitemnumber(li_counter,"recipient_no")
	ls_name                = dw_reporting_fee.getitemstring(li_counter,"provider_name")
	ls_recipient_type_code = dw_reporting_fee.getitemstring(li_counter,"recipient_type_code")
	ls_sub_type_code       = dw_reporting_fee.getitemstring(li_counter,"recipient_sub_type_code")
	ls_type_code           = dw_reporting_fee.getitemstring(li_counter,"type_code")
   ls_method_code         = dw_reporting_fee.getitemstring(li_counter,"method_code")
	ls_eligibility_code    = dw_reporting_fee.getitemstring(li_counter,"eligibility_code")
   ll_docid               = dw_reporting_fee.getitemnumber(li_counter,"docid")
   ldt_date_on_doc        = dw_reporting_fee.getitemdatetime(li_counter,"date_on_document")
	ls_claim_status_type_code = dw_reporting_fee.getitemstring(li_counter,"claim_status_type_code")
	ldt_date_received      = dw_reporting_fee.getitemdatetime(li_counter,"date_received")
	ls_admin_region        = dw_reporting_fee.getitemstring(li_counter,"admin_region_code")
	ls_doc_type 			  = dw_reporting_fee.GetItemString(li_counter,"type_code_1")
	ldec_nbms_reporting_fee	= dw_reporting_fee.GetItemDecimal(li_counter,"reporting_fee")
	ldec_nbca_reporting_fee = dw_reporting_fee.GetItemDecimal(li_counter,"chiro_reporting_fee")

	
	ll_nbr = lds_reporting_fee_combination.RowCount()
	IF ll_nbr < 1 THEN SignalError(-666,"NBMS/NBCA Create Payments - No reporting fees found.")
	
	 // for chiropractors, use the expression where the association_type_code = 'NBCA'
	IF ls_recipient_type_code = 'M' AND ls_sub_type_code = '04' THEN  
		ls_expression = 'reporting_fee_type_code = "'+ trim(ls_type_code) + '" and generated_method_code = "' + trim(ls_method_code) + '" and reporting_fee_eligibility_code = "' + ls_eligibility_code + '" and association_type_code = "NBCA"'
		ldec_fee = ldec_nbca_reporting_fee
	
	// for all others search for reporting fee where association_type_code = 'NBMS'
	ELSE
		ls_expression = 'reporting_fee_type_code = "'+ trim(ls_type_code) + '" and generated_method_code = "' + trim(ls_method_code) + '" and reporting_fee_eligibility_code = "' + ls_eligibility_code + '" and association_type_code = "NBMS"'
		ldec_fee = ldec_nbms_reporting_fee
	END IF		
	
	ll_foundrow = lds_reporting_fee_combination.Find(ls_expression, 1, ll_nbr)
	
	IF ll_foundrow <= 0 THEN SignalError(-667,"NBMS/NBCA Create Payments - Can not find the reporting fee type for this payment - Ref Claim No: " + string(ll_claim_no))
	
	ls_sub_type = lds_reporting_fee_combination.getitemstring(ll_foundrow,"payment_sub_type_code")
	ls_payment_type_code = lds_reporting_fee_combination.getitemstring(ll_foundrow,"payment_type_code")
	
	/* Make sure we get some valid values */
	IF isnull(ls_sub_type) OR TRIM(ls_sub_type) = "" THEN SignalError(-668,"Invalid value found for payment sub type in  Reporting Fee Combination table.")
	IF isnull(ls_payment_type_code) OR TRIM(ls_payment_type_code) = "" THEN SignalError(-668,"Invalid value found for payment type in  Reporting Fee Combination table.")

	
	// If the claim is "Claim Disallowed" and the date on the document is before June 01, 1999 for SDD, MPD, or AD documents 
	//  or before March1, 2006 for SDC documents, we have to ask the user if they want to continue processing.
	IF ls_doc_type = 'SDD' or ls_doc_type = 'MPD' or ls_doc_type = 'AD' THEN
		ldt_document_date_check = 1999-06-01
	ELSEIF  ls_doc_type = 'SDC' THEN
		ldt_document_date_check = 2006-03-01
	ELSE
		// this should never happen, but just in case, we wont process a document that doesn't match the appropriate document types
		Messagebox("Processing Error", "There is a document for claim "+String(ll_claim_no)+" in the process that has a type other than SDD, MPD, AD or SDC." &
		                     + "~r~n~r~nThe process will be stopped. Please contact the HELPDESK.")
		RETURN -1
	END IF
	If ls_claim_status_type_code = '07' and ldt_date_on_doc < DateTime(ldt_document_date_check) Then
		IF MessageBox('Old Document', 'A reporting fee is about to be paid to provider '+ls_name+'.~rIt is for:~r~t- claim '+String(ll_claim_no)+&
			', which is Rejected/Disallowed~r~t- '+ls_doc_type+' document '+String(ll_docid)+' which is older than ' + STRING(ldt_document_date_check, 'Mmm d, yyyy')  &
			+' (Document date is: ' + String(ldt_date_on_doc,'Mmm d, yyyy')+').~n~nDo you want to continue processing?', Question! , YesNo!, 2) = 2 THEN
			RETURN -1
		End if
	End if
	
	/* we need this to link our payment and unapplied_claim_txn and payment_document if applicable
	*/	
	ll_payment_no = ll_payment_no + 1
	ll_txn_no     = ll_txn_no + 1	

	//Create the PAYMENT record
	dw_payment.insertrow(0)
	dw_payment.SetItem(li_counter,"payment_no",ll_payment_no)
	dw_payment.SetItem(li_counter,"claim_no",ll_claim_no)
	dw_payment.SetItem(li_counter,"opening_no",0)
	dw_payment.SetItem(li_counter,"benefit_calculation_no",0)
	dw_payment.SetItem(li_counter,"award_no",0)	
	dw_payment.SetItem(li_counter,"payment_type_code",ls_payment_type_code)
	dw_payment.SetItem(li_counter,"payment_sub_type_code",ls_sub_type)
	dw_payment.SetItem(li_counter,"final_payment_flag","N")
	dw_payment.SetItem(li_counter,"paid_days_lost",0)
	dw_payment.SetItem(li_counter,"paid_hours_lost",0)
	dw_payment.SetItem(li_counter,"paid_quantity",0)
	dw_payment.SetItem(li_counter,"paid_from_date",ldt_date_on_doc)
	dw_payment.SetItem(li_counter,"paid_to_date",ldt_date_on_doc)	
	dw_payment.SetItem(li_counter,"total_award_amount",ldec_fee)
	dw_payment.SetItem(li_counter,"total_deductions",0)
	dw_payment.SetItem(li_counter,"tax_amount",0)
	dw_payment.SetItem(li_counter,"adjustment_tax_amount",0)
	dw_payment.SetItem(li_counter,"adjustment_payment_amount",0)
	dw_payment.SetItem(li_counter,"tax_rate",0)	
	dw_payment.SetItem(li_counter,"loe_explanation","")
	dw_payment.SetItem(li_counter,"payment_adjustment_flag","N")	
   dw_payment.SetItem(li_counter,'authorization_no',0)
	dw_payment.SetItem(li_counter,'authorized_by_code',vgst_user_profile.user_id)
	dw_payment.SetItem(li_counter,'authorized_date',ldt_current_date)
	dw_payment.SetItem(li_counter,"submitted_amount",0)
	
	
	SELECT 	address_line1,
			 	address_line2,
				city,
				a.prov_state_code,
				postal_code,
				location_desc1
	INTO 	:ls_address_line1,
			:ls_address_line2 ,
			:ls_city,
			:ls_prov_state_code,
			:ls_postal_code,
			:ls_country				
	FROM PROVIDER a,
			Location b
	WHERE a.country_code = b.country_code
			and b.location_type_code = 'C'
			and a.provider_no = :ll_recipient_no
			and a.provider_type_code = :ls_recipient_type_code
			and b.active_flag = 'Y'
	USING SQLCA;
	
	SQLCA.nf_handle_error('w_create_reporing_fee_payments','wf_create_payments','SELECT :ls_address_line1 = address_line1,')
	
	IF isNull(ls_address_line1 ) or ls_address_line1 = '' Then
		SignalError(-666,'Error finding a valid address for the recipient.')
	END IF
	
	
	// PR 24852 - replace hardcoding of 'payment_method_code' with value from new function
	ls_payment_method_code = wf_get_payment_method(ll_recipient_no,ls_recipient_type_code,ls_sub_type_code)
	
	
  	//Create the UNAPPLIED_CLAIM_TXN record
	dw_unapplied_claim.insertrow(0)
	dw_unapplied_claim.SetItem(li_counter,"txn_no",ll_txn_no)
	dw_unapplied_claim.SetItem(li_counter,"claim_no",ll_claim_no)
   dw_unapplied_claim.SetItem(li_counter,"payment_no",ll_payment_no)
	dw_unapplied_claim.SetItem(li_counter,"txn_type_code","1")
	dw_unapplied_claim.SetItem(li_counter,"txn_sub_type_code","")
	dw_unapplied_claim.SetItem(li_counter,"recipient_no",ll_recipient_no)
   dw_unapplied_claim.SetItem(li_counter,"recipient_type_code",ls_recipient_type_code)
   dw_unapplied_claim.SetItem(li_counter,"recipient_sub_type_code",ls_sub_type_code)
	dw_unapplied_claim.SetItem(li_counter,"coc_period",0)
	dw_unapplied_claim.SetItem(li_counter,"manual_cheque_req_no",0)
	dw_unapplied_claim.SetItem(li_counter,"cheque_no",0)
	dw_unapplied_claim.SetItem(li_counter,"direct_deposit_xmit_no",0)
   dw_unapplied_claim.SetItem(li_counter,"payment_method_code",ls_payment_method_code)
	dw_unapplied_claim.SetItem(li_counter,"tax_amount",dw_payment.GetItemNumber(li_counter,"tax_amount"))
	dw_unapplied_claim.SetItem(li_counter,"txn_amount",dw_payment.GetItemNumber(li_counter,"total_award_amount")	)
	//Sum all the payment amounts for the TXN_BATCH_CONTROL record
	ld_total_txn_amount_created = ld_total_txn_amount_created + dw_payment.GetItemNumber(li_counter,"total_award_amount")	
   dw_unapplied_claim.SetItem(li_counter,"admin_region_code",ls_admin_region)
	dw_unapplied_claim.SetItem(li_counter,"scheduled_processing_date",ldt_processing_date)
   dw_unapplied_claim.SetItem(li_counter,"explanation",'Report Fee/Frais trans.')
   dw_unapplied_claim.SetItem(li_counter,"related_txn_no",0)
   dw_unapplied_claim.SetItem(li_counter,"recipient_name",ls_name)
	
	dw_unapplied_claim.SetItem(li_counter,"address_line1",ls_address_line1)
	dw_unapplied_claim.SetItem(li_counter,"address_line2",ls_address_line2)
	
	
	
	dw_unapplied_claim.SetItem(li_counter,"city",ls_city)
	dw_unapplied_claim.SetItem(li_counter,"prov_state_code",ls_prov_state_code)
	dw_unapplied_claim.SetItem(li_counter,"country",ls_country)
	dw_unapplied_claim.SetItem(li_counter,"postal_code",ls_postal_code)
	dw_unapplied_claim.SetItem(li_counter,"use_default_address_flag","Y")
	dw_unapplied_claim.SetItem(li_counter,"cheque_print_group_code","")
	dw_unapplied_claim.SetItem(li_counter,"txn_unit_of_work_no",0)
	dw_unapplied_claim.SetItem(li_counter,"maintain_allowed_flag","Y")
  
	//Create a PAYMENT_DOCUMENT record for the new payment.
	li_row = dw_payment_document.insertrow(0)
	dw_payment_document.setitem(li_row,"doc_id",ll_docid)
	dw_payment_document.setitem(li_row,"payment_no",ll_payment_no)
	dw_payment_document.setitem(li_row,"paid_status_code","S")
	dw_payment_document.setitem(li_row,"paid_status_explanation_code","")	

	/* Now we need to set the eligibility code to "SCH" so that we do not
	   pick up the same records again
	*/
	dw_reporting_fee.setitem(li_counter,"eligibility_code","SCH")	
NEXT


SQLCA.nf_begin_transaction()

	/* now lets put in the identifiers*/
	ll_payment_no = wf_get_next_payment_identifier((li_counter - 1))				
	ll_txn_no = wf_get_next_txn_identifier((li_counter - 1))
	ll_next_batch_no = wf_get_next_batch_no()
		
	/* set the countback to the rowcount*/
   li_counter = dw_reporting_fee.rowcount() 

	DO 		
		dw_payment.SetItem(li_counter,"payment_no",ll_payment_no)
		
		ll_found = dw_unapplied_claim.find('payment_no = ' + string(li_counter),0,dw_unapplied_claim.rowcount())
		IF ll_found > 0 THEN
			dw_unapplied_claim.SetItem(ll_found,"payment_no",ll_payment_no )
		ELSE//big problem
			SQLCA.nf_rollback_transaction()

			DESTROY lds_reporting_fee_combination
			messagebox("NBMS/NBCA Create Payments","Invalid Service Provider - Ref claim no: " + string(ll_claim_no))
			RETURN -1
		END IF
		
		ll_found = dw_payment_document.find('payment_no = ' + string(li_counter),0,dw_payment_document.rowcount())
		IF ll_found > 0 THEN
			dw_payment_document.SetItem(ll_found,"payment_no",ll_payment_no )
		END IF
		
		dw_unapplied_claim.SetItem(li_counter,"txn_no",ll_txn_no )
		dw_unapplied_claim.SetItem(li_counter,"batch_no",ll_next_batch_no)
		
		ll_payment_no = ll_payment_no - 1
		ll_txn_no     = ll_txn_no     - 1
		li_counter --
	LOOP UNTIL li_counter = 0
	
	//Create the TXN_BATCH_CONTROL record
	ll_txn_count = dw_unapplied_claim.RowCount()	
	
	INSERT TXN_BATCH_CONTROL (
		batch_no,
		batch_type_code,
		admin_region_code,
		user_batch_flag,
		number_txns_batched,
		number_txns_processed,
		number_txns_rejected,
		txn_amount_batched,
		txn_amount_processed,
		txn_amount_rejected,
		scheduled_processing_date)
	Values(
		:ll_next_batch_no,
		'B', //batch_type_code
		'',	//admin_region_code
		'N',	//user_batch_flag
		:ll_txn_count,
		0,		//number_txns_processed
		0,		//number_txns_rejected
		:ld_total_txn_amount_created,
		0,		//txn_amount_processed
		0,
		:ldt_processing_date)
	USING SQLCA;
	
	SQLCA.nf_handle_error('w_create_reporing_fee_payments','wf_create_payments','	INSERT INTO TXN_BATCH_CONTROL')
	
   dw_payment.accepttext()
   dw_unapplied_claim.accepttext()

	/* now update the payments/ and transaction datawindows
	*/
	IF dw_payment.update() < 0 Then SignalError(-666,'Datawindow error')
	SQLCA.nf_handle_error('w_create_reporting_fee_payments','wf_create_payment','dw_payment.update()')		
	
	IF dw_unapplied_claim.update() < 0 Then SignalError(-666,'Datawindow error')
	SQLCA.nf_handle_error('w_create_reporting_fee_payments','wf_create_payment','dw_unapplied_claim.update()')
	
	IF dw_payment_document.update() < 0 Then SignalError(-666,'Datawindow error')
	SQLCA.nf_handle_error('w_create_reporting_fee_payments','wf_create_payment','dw_payment_document.update()')
	
	IF dw_reporting_fee.update() < 0 Then SignalError(-666,'Datawindow error')
	SQLCA.nf_handle_error('w_create_reporting_fee_payments','wf_create_payment','dw_reporting_fee.update()')

	/* If all goes well commit the transaction
	*/

SQLCA.nf_commit_transaction()


RETURN 1

end function

public function string wf_get_payment_method (long al_provider_no, string as_provider_type_code, string as_provider_sub_type_code);INTEGER   li_bank_info_count
STRING    ls_payment_method_type_code

SELECT COUNT(*)
INTO   :li_bank_info_count
FROM   PROVIDER a
WHERE  a.provider_no            = :al_provider_no
AND    a.provider_type_code     = :as_provider_type_code
AND    a.provider_sub_type_code = :as_provider_sub_type_code
AND EXISTS ( SELECT *
             FROM   BANK_INFO b
             WHERE  b.recipient_no            = a.provider_no
             AND    b.recipient_type_code     = a.provider_type_code
             AND    b.recipient_sub_type_code = a.provider_sub_type_code )
USING SQLCA;
SQLCA.nf_handle_error('w_create_reporting_fee_payments','embedded SQL: SELECT COUNT(*) FROM PROVIDER...','wf_get_payment_method')

IF li_bank_info_count > 0 THEN
	ls_payment_method_type_code = 'D'
ELSE
	ls_payment_method_type_code = 'A'
END IF

RETURN ls_payment_method_type_code
end function

on w_create_reporting_fee_payments.create
int iCurrent
call super::create
if IsValid(this.MenuID) then destroy(this.MenuID)
if this.MenuName = "m_cmwb_notools" then this.MenuID = create m_cmwb_notools
this.cb_create=create cb_create
this.cb_cancel=create cb_cancel
this.dw_reporting_fee=create dw_reporting_fee
this.dw_totals=create dw_totals
this.dw_payment=create dw_payment
this.dw_unapplied_claim=create dw_unapplied_claim
this.dw_payment_document=create dw_payment_document
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_create
this.Control[iCurrent+2]=this.cb_cancel
this.Control[iCurrent+3]=this.dw_reporting_fee
this.Control[iCurrent+4]=this.dw_totals
this.Control[iCurrent+5]=this.dw_payment
this.Control[iCurrent+6]=this.dw_unapplied_claim
this.Control[iCurrent+7]=this.dw_payment_document
end on

on w_create_reporting_fee_payments.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_create)
destroy(this.cb_cancel)
destroy(this.dw_reporting_fee)
destroy(this.dw_totals)
destroy(this.dw_payment)
destroy(this.dw_unapplied_claim)
destroy(this.dw_payment_document)
end on

event open;call super::open;U_DWA 				ldw_dw[]
S_WINDOW_MESSAGE	lstr_message

//lstr_message = Message.PowerObjectParm

iw_sheet = w_frame.GetActiveSheet()

inv_controller = CREATE n_account_payment_controller
inv_payment    = CREATE N_PAYMENT

/*	pass all the info to the object - return the approved count sets an instance variable
*/

inv_controller.nf_set_window_parent(THIS)
//inv_controller.nf_set_datawindow(ldw_dw[],SQLCA)
//inv_controller.nf_init()

dw_reporting_fee.retrieve()
IF SQLCA.nf_handle_error('w_create_reporting_fee_payments','Open Event','dw_reporting_fee.retrieve()') < 0 THEN
	RETURN -1
END IF





//Setup the windows resizing
if IsNull(inv_resize) Or not IsValid (inv_resize) then
	inv_resize = create n_resize
	inv_resize.of_SetOrigSize (3220, 3220)
end if

// register controls for resizing
inv_resize.of_register(dw_reporting_fee,'ScaleToRight&Bottom')
inv_resize.of_register(cb_create,'FixedToRight')
inv_resize.of_register(cb_cancel,'FixedToRight')


end event

event closequery;call super::closequery;IF isvalid(inv_controller) THEN DESTROY inv_controller
IF isvalid(inv_payment) THEN DESTROY inv_payment
end event

event resize;call super::resize;long ll_workspacewidth,ll_workspaceheight


// Notify the resize service that the window size has changed.
ll_workspacewidth = This.WorkSpaceWidth()
ll_workspaceheight = This.WorkSpaceHeight()

If IsValid (inv_resize) Then
	inv_resize.Event pfc_Resize (sizetype,ll_workspacewidth  , ll_workspaceheight )
End If
end event

type cb_create from commandbutton within w_create_reporting_fee_payments
integer x = 2551
integer y = 48
integer width = 558
integer height = 108
integer taborder = 10
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = " Create &Payments"
end type

event clicked;// On the bottom of the report, identify  the number of the documents that are marked 
// as approved, but the claim's is not rejected - disallowed or rejected - no claim
//	
// On the click of the 'Create Payments' button, create the PAYMENT,
//	UNAPPIED_CLAIM_TXN and PAYMENT_DOCUMENT records as identified in charts A, B & C
//
Long     ll_statutory_holiday_days
Integer  li_error, li_rtn
String   ls_day_name
Datetime ldt_server_datetime, ldt_next_thursday, ldt_first_available_processing_date 

N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '017' refers to the Create NBMS Reporting Fee Payments module, '044' refers to the Payment Processing module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('017','044','save',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF
/******************************************************************************************/


// PR4050 - Must be run on a Thursday, if Thursday is a stat holiday then on first non-holiday before Thursday.
ldt_server_datetime = f_server_datetime()

ls_day_name = Upper(DayName(Date(ldt_server_datetime)))
IF ls_day_name <> "THURSDAY" THEN
	// Get the Date of the next Thursday and see if it is a holiday
	SELECT MIN(calendar_date) 
	  INTO :ldt_next_thursday 
	  FROM Company_Calendar 
	 WHERE calendar_date > :ldt_server_datetime 
		AND UPPER(day_of_week) = "THURSDAY" 
		AND parent_company_code = "WHSCC" ; 

	li_rtn = SQLCA.nf_handle_error("w_create_reporing_fee_payments", "", "SELECT MIN(calendar_date) FROM Company_Calendar")
		
	SELECT statutory_holiday_days 
	  INTO :ll_statutory_holiday_days 
	  FROM Company_Calendar 
	 WHERE calendar_date = :ldt_next_thursday 
	   AND parent_company_code = "WHSCC" ;

	li_rtn = SQLCA.nf_handle_error("w_create_reporing_fee_payments", "", "SELECT statutory_holiday_days FROM Company_Calendar")

	// If the Next Thursday is a holiday then...
	IF ll_statutory_holiday_days = 1 THEN
		SELECT MAX(calendar_date) 
		  INTO :ldt_first_available_processing_date 
		  FROM Company_Calendar 
		 WHERE calendar_date < :ldt_next_thursday 
			AND statutory_holiday_days = 0.0 
			AND parent_company_code = "WHSCC" ;

		li_rtn = SQLCA.nf_handle_error("w_create_reporing_fee_payments", "", "SELECT MAX(calendar_date) FROM Company_Calendar")
			
		IF Date(ldt_first_available_processing_date) <> Date(ldt_server_datetime) THEN
			Messagebox("Not Today", "Normally this module can only be run on Thursdays, but since this Thursday " +&
						  String(ldt_next_thursday, "mmm dd, yyyy") + " is a Statutory Holiday it can be run on " +&
						  "the first non-holiday prior to Thursday which is: " +&
						  String(ldt_first_available_processing_date, "dddd mmm dd, yyyy") + "~r~rTry again on " +&
						  String(ldt_first_available_processing_date, "dddd mmm dd, yyyy") + " to run this module.~r~r" +&
						  "Today is: " + String(ldt_server_datetime, "dddd mmm dd, yyyy") + ".", Exclamation!)
			RETURN
		END IF
	ELSE
		Messagebox("Not Today", "This module can only be run on Thursdays.  Today is: " +&
					  String(ldt_server_datetime, "dddd mmm dd, yyyy") + ".~r~rIf the Thursday falls on a statutory " +&
					  "holiday then it can be run on first non-holiday prior to Thursday.", Exclamation!)
		RETURN
	END IF
END IF

li_error = wf_create_payment()

IF li_error < 0 THEN
	this.enabled = false
	RETURN -1
ELSE
	w_create_reporting_fee_payments.triggerevent("ue_print")
	dw_reporting_fee.reset()
	THIS.enabled = FALSE
END IF


end event

type cb_cancel from commandbutton within w_create_reporting_fee_payments
integer x = 2551
integer y = 176
integer width = 558
integer height = 108
integer taborder = 70
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Cancel"
end type

event clicked;/* on click of the cancel button, close the window
   and do not make any data base updates
*/
CLOSE(parent)
end event

type dw_reporting_fee from u_dw_online within w_create_reporting_fee_payments
integer x = 14
integer y = 916
integer width = 3200
integer height = 2304
integer taborder = 60
boolean bringtotop = true
string dataobject = "d_create_reporting_fee_payments"
boolean hscrollbar = true
boolean vscrollbar = true
end type

event constructor;call super::constructor;this.settransobject(sqlca)
end event

event retrieveend;call super::retrieveend;/* Select all records in ELIGIBLE_REPORT_FEE_DOCUMENTS 
   with a reporting_fee_eligibility_code = "APP" and the (CLAIM.claim_status_code is "R"
	and the CLAIM.claim_status_type_code in ('07','09')) and display in the report datawindow.
	
	Display the count of the number of NO Claim payments (reporting_fee_type = "NC"
	and the number of Disallowed Claim payments (reporting_fee_type = "DC")
	that matched the criteria in the above comment.
	
	Calculate the amount of the payments to be issued by multiplying the
	number to created by the amount to be used. Determine the amount of each payment
	using the Nbms_Fee_Schedule (and Chiro_fee_schedule) table. The amount to be used is to be based 
	on the documents treatment_date (DOCUMENT_INDEX.date_on_document). 
	This is determined by: (select reporting_fee_amount from Nbms_Fee_Schedule
	where effective_date = (select max(effective_date from Nbms_Fee_Schedule
	where effective_date <:{DOCUMENT_INDEX.date_on_document}))
	
	On the bottom of the report, identify the number of documents that are marked as approved
	but the claim's status is not rejected - disallowed or rejected - no claim

*/

DECIMAL ldec_amount_one,ldec_amount_two, ldec_total_amount, ldec_nbms_fee
DECIMAL ldec_nbca_amount_one, ldec_nbca_amount_two, ldec_nbca_total_amount,  ldec_nbca_fee
LONG    ll_count_one,ll_count_two,ll_total_count, ll_nbca_count_one, ll_nbca_count_two, ll_nbca_total_number
INTEGER li_counter,li_rowcount,li_row
STRING  ls_reporting_type


/* Check the rowcount number
*/
li_rowcount = this.rowcount()
IF li_rowcount < 1 THEN
	messagebox("Reporting Fee Payments","There are at present no payments to create.",information!)
	RETURN	
ELSE
	cb_create.enabled = TRUE
END IF

FOR li_counter = 1 TO li_rowcount
	ldec_nbms_fee = this.getitemnumber(li_counter,"reporting_fee")   // get the nbms fee
	IF isnull(ldec_nbms_fee) OR ldec_nbms_fee = 0.00 THEN
		ldec_nbca_fee = this.getitemnumber(li_counter,"chiro_reporting_fee")   // if nbms fee is zero, then it must be a chiro fee, so get that value
		IF isnull(ldec_nbca_fee) OR ldec_nbca_fee = 0.00 THEN
			Messagebox("Error", " The create payment module encountered a problem with reading the reporting fee values.~rPlease report this to the HelpDesk.", Exclamation!)
			RETURN -1
		END IF
	END IF
	
	ls_reporting_type = this.getitemstring(li_counter,"type_code")
	
	CHOOSE CASE ls_reporting_type
		CASE "NC"
			IF ldec_nbms_fee > 0.00 THEN
				ldec_amount_one = ldec_amount_one + ldec_nbms_fee
				ll_count_one++
			ELSE
				ldec_nbca_amount_one = ldec_nbca_amount_one + ldec_nbca_fee
				ll_nbca_count_one++
			END IF
		CASE "DC"
			IF ldec_nbms_fee > 0.00 THEN
				ldec_amount_two = ldec_amount_two + ldec_nbms_fee
				ll_count_two++
			ELSE
				ldec_nbca_amount_two = ldec_nbca_amount_two + ldec_nbca_fee
				ll_nbca_count_two++
			END IF
	END CHOOSE
	
	ldec_nbms_fee= 0.00   // reset the values
	ldec_nbca_fee = 0.00
NEXT
/* set our total amounts
*/
ll_total_count    = ll_count_one + ll_count_two
ldec_total_amount = ldec_amount_one + ldec_amount_two

ll_nbca_total_number    = ll_nbca_count_one + ll_nbca_count_two
ldec_nbca_total_amount = ldec_nbca_amount_one + ldec_nbca_amount_two


/* once the above has calculated what we need then we can insert them
   into our external datawindow
*/
li_row = dw_totals.insertrow(0)
dw_totals.setitem(li_row,"count_one",ll_count_one)
dw_totals.setitem(li_row,"count_two",ll_count_two)
dw_totals.setitem(li_row,"amount_one",ldec_amount_one)
dw_totals.setitem(li_row,"amount_two",ldec_amount_two)
dw_totals.setitem(li_row,"total_one",ll_total_count)
dw_totals.setitem(li_row,"total_two",ldec_total_amount)

dw_totals.setitem(li_row,"nbca_count_one",ll_nbca_count_one)
dw_totals.setitem(li_row,"nbca_count_two",ll_nbca_count_two)
dw_totals.setitem(li_row,"nbca_amount_one",ldec_nbca_amount_one)
dw_totals.setitem(li_row,"nbca_amount_two",ldec_nbca_amount_two)
dw_totals.setitem(li_row,"nbca_total_number",ll_nbca_total_number)
dw_totals.setitem(li_row,"nbca_total_amount",ldec_nbca_total_amount)


end event

type dw_totals from u_dw_online within w_create_reporting_fee_payments
integer x = 9
integer y = 20
integer width = 2277
integer height = 880
integer taborder = 50
boolean bringtotop = true
string dataobject = "d_reporting_fee_totals"
borderstyle borderstyle = styleraised!
end type

type dw_payment from u_dw_online within w_create_reporting_fee_payments
boolean visible = false
integer x = 37
integer y = 3004
integer width = 183
integer height = 196
integer taborder = 40
boolean bringtotop = true
string dataobject = "d_med_payment"
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;this.settransobject(sqlca)
end event

type dw_unapplied_claim from u_dw_online within w_create_reporting_fee_payments
boolean visible = false
integer x = 242
integer y = 3008
integer width = 183
integer height = 196
integer taborder = 30
boolean bringtotop = true
string dataobject = "d_med_unapplied_claim"
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;this.settransobject(sqlca)
end event

type dw_payment_document from u_dw_online within w_create_reporting_fee_payments
boolean visible = false
integer x = 448
integer y = 3008
integer width = 183
integer height = 196
integer taborder = 20
boolean bringtotop = true
string dataobject = "d_med_payment_document"
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;this.settransobject(sqlca)
end event

