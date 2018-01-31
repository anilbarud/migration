$PBExportHeader$n_account_payment_controller.sru
$PBExportComments$the main object containing most of the business rules for the account payment controller ( other rules are in the n_payment object)
forward
global type n_account_payment_controller from n_pdc
end type
end forward

global type n_account_payment_controller from n_pdc
end type
global n_account_payment_controller n_account_payment_controller

type variables
N_PAYMENT inv_payment
LONG il_edit_row, il_docid
S_WINDOW_MESSAGE  istr_message
STRING is_authorization_type
w_sheet iw_active_sheet


BOOLEAN  ib_msgbox_already	// pr 1480
BOOLEAN	ib_hst_update_flag = TRUE
BOOLEAN	ib_display_hst_unchecked = FALSE
n_tax_functions inv_tax_functions//tax function object used to hold all tax functions

DECIMAL  idec_total_amount
DATETIME idtm_date_on_doc

constant Date IDT_NBMS_1_START = Date('1999-06-01')
constant Date IDT_NBMS_1_END   = Date('2005-05-31')
constant Date IDT_NBMS_2_START = Date('2005-06-01')
constant Date IDT_CHIRO_START  = Date('2006-03-01')
end variables

forward prototypes
public function integer nf_insert (long al_claim_no)
public function integer nf_reject ()
public function integer nf_validate_service_provider (string as_recipient_type_code, long al_recipient_no)
public function integer nf_save_reject ()
public function integer nf_check_document_paid (long al_doc_id, ref boolean ab_check)
public function boolean nf_check_document_type (string as_document_type)
public function integer nf_check_recieved_early_bonus (long al_provider_no, string as_provider_type_code, string as_provider_sub_type_code, long al_claim_no)
public function integer nf_check_claim_acceptance (long al_claim_no, ref datetime adt_first_acceptance_date, ref boolean ab_check)
public function integer nf_init_hst_update_flag (boolean ab_hst_update_flag)
public function integer nf_init_display_hst_unchecked (boolean ab_display_hst_unchecked)
public function integer nf_validate_recipient (string as_recipient_type_code, long al_recipient_no)
public subroutine nf_init ()
public function integer nf_validate_account_payments ()
public function long nf_set_identifiers ()
public function integer nf_retrieve_for_delete (long al_payment_no)
public function integer nf_set_unused_fields ()
public function integer nf_validate_authorization (long al_authorization_no, string as_mode)
public function integer nf_check_mandatory ()
public function integer nf_validate_account_transactions ()
public function integer nf_trans_itemfocuschanged ()
public function integer nf_validate_address ()
public function decimal nf_calculate_chiro_bonus_payment (datetime adt_treatment_date)
public function integer nf_check_valid_physician (long al_provider_no, ref boolean ab_check, boolean ab_old)
public function integer nf_check_chiro_document_paid (long al_doc_id, ref boolean ab_check)
public function long nf_set_identifiers_bonus ()
public function decimal nf_calculate_working_days (datetime adt_start_date, datetime adt_end_date, string as_company_code)
public function integer nf_retrieve (long al_payment_no)
public function integer nf_protect_columns (string as_module_source_code)
public function integer nf_create_early_filing_bonus (ref string as_message)
public function integer nf_create_new_early_filing_bonus (ref string as_message, string as_mode)
public function integer nf_delete ()
public function integer nf_change_payment_type (long al_datawindow)
public function integer nf_check_valid_chiropractor (long al_provider_no, ref boolean ab_check)
public function integer nf_create_chiro_early_filing_bonus (ref string as_message)
public function integer nf_handle_EFB_return (integer ai_return, string as_message, string as_msgbox_title)
public subroutine nf_filter_payment_sub_type (string as_payment_type, decimal adec_amount, decimal adec_submitted)
public function integer nf_change_item (long al_datawindow)
public function integer nf_set_defaults ()
public function integer nf_check_bus_rule ()
public function integer nf_check_fca_payment (long al_claim_no, long al_doc_id, string as_payment_type_code, string as_payment_sub_type_code, string as_recipient_type_code)
public function integer nf_create_manual_efb (string as_payment_sub_type, string as_administering_act_code)
public function integer nf_add_travel_expense (long al_claim_no, long al_payment_no, long al_rehab_invoice_no, integer al_line_no)
public function long nf_get_last_travel_expense_no ()
public function integer nf_delete_travel_expense (long al_travel_expense_no)
public function decimal nf_calculate_bonus_payment (datetime adt_treatment_date, string as_provider_type_code, string as_provider_sub_type_code)
public function integer nf_calculate_efb_by_percent (datetime adtm_treatment_date, string as_provider_type_code, string as_provider_sub_type_code, ref decimal adec_amount)
public function integer nf_check_early_filing_bonus (long al_payment_no, string as_provider_type_code, string as_provider_sub_type_code)
public function long nf_modify_early_filing_bonus (string as_provider_type_code, string as_provider_sub_type_code)
public function decimal nf_calculate_fca_report_fee_payment (datetime adt_treatment_date, string as_provider_type_code, string as_provider_sub_type_code)
public function string nf_get_module_source_code ()
end prototypes

public function integer nf_insert (long al_claim_no);//inv_payment.nf_insert(0)

idw_dw[1].Reset()
idw_dw[1].InsertRow(0)			// account details

idw_dw[2].Reset()
idw_dw[2].InsertRow(0)			// transaction

idw_dw[5].Reset()
idw_dw[5].InsertRow(0)  		// invoice

IF istr_message.as_mode = 'add' OR  istr_message.as_mode = 'inbasketadd' THEN
   idw_dw[6].Reset()
   idw_dw[6].InsertRow(0)  		// payment doc
END IF

/* for project 10127 we are using datawindow array numbers 9 and 10
*/

idw_dw[9].Reset()
idw_dw[9].InsertRow(0)  		// payment


idw_dw[10].Reset()
idw_dw[10].InsertRow(0)  		// account transactions


idw_dw[11].Reset()
idw_dw[11].InsertRow(0)  		// PAYMENT_DOCUMENT

/* end of insert fo this project
*/

nf_set_defaults()
Return 0

end function

public function integer nf_reject ();DATAWINDOWCHILD ldwc_child
//	Setup window

	idw_dw[1].Visible = False
	idw_dw[2].Visible = False
	idw_dw[5].Visible = False

	idw_dw[6].Visible = True
	idw_dw[6].InsertRow(0)

/*	Set some defaults
*/
	idw_dw[6].SetItem(1,"doc_id",istr_message.al_doubleparm[3])
	idw_dw[6].SetItem(1,"payment_no",0)
	idw_dw[6].GetChild('paid_status_code',ldwc_child)
	ldwc_child.SetFilter("paid_status_code = 'O' OR paid_status_code = 'H' OR paid_status_code = 'R'")
	ldwc_child.Filter()
	idw_dw[6].SetItem(1,"paid_status_code","R")

/*	Filter the paid status explanations to show only those which are valid
	when the paid status code = "R"
*/
	idw_dw[6].GetChild('paid_status_explanation_code', ldwc_child)
	ldwc_child.SetFilter("paid_status_code = 'R'")
	ldwc_child.Filter()
Return 0
end function

public function integer nf_validate_service_provider (string as_recipient_type_code, long al_recipient_no);LONG		ll_result
STRING	ls_recipient_name, ls_recipient_sub_type_code

/*	This function validates that the service provider entered exists on the 
	PROVIDER table.  If the service provider is found, the name
	and sub-type-code are automatically set on the datawindow
*/

	IF IsNull(al_recipient_no) or al_recipient_no = 0 THEN
		Return 100
	END IF

	SetNull(ls_recipient_name)

	SELECT PROVIDER.name, provider_sub_type_code
	  INTO :ls_recipient_name, :ls_recipient_sub_type_code
	  FROM PROVIDER  
	 WHERE ( PROVIDER.provider_no = :al_recipient_no ) AND  
		 	 ( PROVIDER.provider_type_code = :as_recipient_type_code) AND
       	 ( PROVIDER.active_flag = 'Y' )  
	 USING SQLCA ;
		
	ll_result = SQLCA.nf_handle_error("Embedded SQL: Retrieve on PROVIDER","n_account_payment_controller","nf_validate_service_provider")
	IF ll_result < 0  THEN
		Return ll_result
	ELSEIF IsNull(ls_recipient_name) THEN
		Return 100
	ELSE
		idw_dw[2].SetItem(idw_dw[2].GetRow(),"recipient_name",ls_recipient_name)
		idw_dw[2].SetItem(idw_dw[2].GetRow(),"recipient_sub_type_code",ls_recipient_sub_type_code)
	
		Return 0
	END IF

Return 0
end function

public function integer nf_save_reject ();STRING	ls_paid_status_code, ls_paid_status_explanation
/*	Check to see IF we are saving a rejection of a document. 
	IF so, validate and save the changes to idw_dw[6].
*/
	IF idw_dw[6].AcceptText() < 1 THEN Return -1

	IF idw_dw[6].RowCount() > 0 THEN							// payment document
		ls_paid_status_code = idw_dw[6].GetItemString(1,"paid_status_code")
		IF ls_paid_status_code = "R" OR ls_paid_status_code = "O" OR ls_paid_status_code = "H" THEN
			ls_paid_status_explanation = idw_dw[6].GetItemString(1,"paid_status_explanation_code")
			IF IsNull(ls_paid_status_explanation) OR ls_paid_status_explanation = "  " THEN
				MessageBox("Account Payment Maintenance","You must provide an explanation when rejecting a document",Exclamation!)
				idw_dw[6].SetFocus()
				Return -1
			END IF
		END IF
	END IF

	idw_dw[6].Update()
	IF SQLCA.nf_handle_error('Update to payment document', 'n_account_payment_controller','nf_save_reject') < 0 THEN
		SQLCA.nf_rollback_transaction()

		Return -1
	END IF	


Return 0
end function

public function integer nf_check_document_paid (long al_doc_id, ref boolean ab_check);
/* Determining if a document has been paid

"Create a function to determine if the documnet has been paid

1.Pass the argument al_doc_id(DOCUMENT_INDEX.doc_id) type long
2.Returns boolean - true if the document has been paid , 
  false if the document has not been paid
3.Determine if the document has been paid with the following query
   select count(*)
	from PAYMENT_DOCUMENT
	where doc_id = :al_doc_id
	and paid_status_code in ("S","O","P");
	
	if the query returns 1 or more then return true, otherwise return false

*/

INTEGER	li_count

/* set the reference variable
*/
ab_check = FALSE

SELECT count(distinct b.payment_no) 
  INTO :li_count
  FROM PAYMENT_DOCUMENT a,
  		 PAYMENT b
 WHERE a.doc_id = :al_doc_id
	AND a.paid_status_code in ("S","O","P")
	AND b.payment_sub_type_code in ('01','02')
	AND b.payment_type_code = '21' 
	AND b.payment_no        = a.payment_no ;
		
//do sql check
IF SQLCA.nf_handle_error("SELECT count(*)INTO :li_count","n_account_payment_controller",+&
					"nf_check_document_paid") < 0 THEN
	RETURN -1
END IF

IF li_count > 0 THEN
	ab_check = TRUE
END IF

RETURN 1
end function

public function boolean nf_check_document_type (string as_document_type);
/* There are only three document types that can be paid on
   SDD - FORM8 (initial Report) document from doctor
	MPD - Medical Progress report from Doctor
	AD  - Account from Doctor
	
	ARGS: as_document_type (checked argument)
	
	RETURNS: BOOLEAN TRUE if match ELSE FALSE
*/

CHOOSE CASE as_document_type
	CASE "SDD","MPD","AD"
	    RETURN TRUE
	CASE ELSE
		RETURN FALSE
END CHOOSE

RETURN FALSE
end function

public function integer nf_check_recieved_early_bonus (long al_provider_no, string as_provider_type_code, string as_provider_sub_type_code, long al_claim_no);
/* Determining if doc has already received an early filing bonus payment
   "Create a function to determine if a document has already received an early filing bonus payment
	
	- Pass the argument variable al_doc_id (DOCUMENT_INDEX.doc_id)
	- Return the number of early filing bonus payments found(integer). 
	  Return zero if no early filing bonus payments are found
   - Calculate the number of early filing bonus payments for the document using the following query:
	
	SELECT count(*)
	 INTO :li_count
	FROM PAYMENT p, APPLIED_CLAIM_TXN a
	WHERE p.payment_no              = a.payment_no
	  AND p.claim_no                = :al_claim_no
	  AND p.payment_type_code       = "21"
	  AND p.payment_sub_type_code in ("01","02")
	  AND a.recipient_no            = :al_provider_no
	  AND a.recipient_type_code     = :as_provider_type_code
	  AND a.recipient_sub_type_code = :as_provider_sub_type_code;


ARGS: al_provider_no
		as_provider_type_code
		as_provider_sub_type_code
		al_claim_no

RETURNS: Return the number of early filing bonus payments found(integer).

*/

INTEGER		li_count,li_count_two

  	SELECT count(*)
	 INTO :li_count
	FROM PAYMENT p, APPLIED_CLAIM_TXN a
	WHERE p.payment_no              = a.payment_no
	  AND p.claim_no                = :al_claim_no
	  AND p.payment_type_code       = "21"
	  AND p.payment_sub_type_code in ("01","02")
	  AND a.recipient_no            = :al_provider_no
	  AND a.recipient_type_code     = :as_provider_type_code
	  AND a.recipient_sub_type_code = :as_provider_sub_type_code;
	  
	  //check the sql return
	IF SQLCA.nf_handle_error("SELECT count(*)INTO :li_count","n_account_payment_controller",+&
				"nf_check_received_early_bonus") < 0 THEN
		RETURN -1
	END IF
	  
	IF isnull(li_count) THEN
		li_count = 0
	END IF
		
 SELECT count(*)
   INTO :li_count_two
	FROM PAYMENT p, UNAPPLIED_CLAIM_TXN u
	WHERE p.payment_no              = u.payment_no
	  AND p.claim_no                = :al_claim_no
	  AND p.payment_type_code       = "21"
	  AND p.payment_sub_type_code in ("01","02")
	  AND u.recipient_no            = :al_provider_no
	  AND u.recipient_type_code     = :as_provider_type_code
	  AND u.recipient_sub_type_code = :as_provider_sub_type_code;
	  
	  //do sql check
	IF SQLCA.nf_handle_error(" SELECT count(*)INTO :li_count_two","n_account_payment_controller",+&
	"nf_check_received_early_bonus") < 0 THEN
		RETURN -1
	END IF
	
	IF isnull(li_count_two) THEN
		li_count_two = 0
	END IF
	
	li_count = li_count + li_count_two
	
	RETURN li_count
end function

public function integer nf_check_claim_acceptance (long al_claim_no, ref datetime adt_first_acceptance_date, ref boolean ab_check);
/* 
"A claim is only eligible to receive the early filing bonus on 
 the first acceptance of a claim. A claim is considered to be a 
 first acceptance claim if the current status is active, Final-Final,
 Final-First & Final, Final-No Lost Time, Medical Aid Only,
 The accident date is June 1, 1999 or later and the document that is 
 being paid has not been paid before and the treatment_date is prior to the 
 first acceptance date
 
 To determine if the claim is a first acceptance of the claim, create a function as follows:
 
 1. Pass the arguments 
   al_claim_no(CLAIM.claim_no) type long
	adt_tratment_date (DOCUMENT_INDEX.date_on_document)(BY REF)
	
	RETURNS: BOOLEAN - TRUE  Claim is an accepted claim
							 FALSE claim is not an accepted claim
							 
	Determine if the claim has been accepted by:
	SELECT COUNT(*)
	FROM CLAIM_STATUS_CHANGE
	WHERE claim_no              = :al_claim_no
	AND ((new_claim_status_code = "A")
	OR (new_claim_status_code   = "F" 
	AND new_claim_status_type_code in ("01","02","03","04")))
	
	If the claim has been accepted, get the earliest date the claim was accepted by:
	SELECT min(create_date)
	FROM CLAIM_STATUS_CHANGE
	WHERE claim_no              = :al_claim_no
	AND ((new_claim_status_code = "A")
	OR (new_claim_status_code   = "F" 
	AND new_claim_status_type_code in ("01","02","03","04")))
	
	if the claim has been accepted, set the 
	value of the argument adt_first_acceptance_date"
*/

/* Determine if the claim has been accepted by:
*/

INT li_count
setnull(adt_first_acceptance_date)

/* set the check variable
*/

SELECT COUNT(*)
  INTO  :li_count
  FROM CLAIM_STATUS_CHANGE
 WHERE claim_no                    = :al_claim_no
   AND ((new_claim_status_code     = "A")
    OR (new_claim_status_code      = "F" 
   AND new_claim_status_type_code in ("01","02","03","04")));

//do sql check
IF SQLCA.nf_handle_error("SELECT COUNT(*) INTO  :li_count","n_account_payment_controller",+&
"nf_check_claim_acceptance") < 0 THEN
	RETURN -1
END IF

IF isnull(li_count) THEN
	li_count = 0
END IF

/* If the claim has been accepted, get the earliest date the claim was accepted by:
*/

IF li_count > 0 THEN

	SELECT min(create_date)
	  INTO :adt_first_acceptance_date
	  FROM CLAIM_STATUS_CHANGE
	 WHERE claim_no                 = :al_claim_no
		AND ((new_claim_status_code  = "A")
		 OR (new_claim_status_code   = "F" 
		AND new_claim_status_type_code in ("01","02","03","04")));
	
	//do sql check
	IF SQLCA.nf_handle_error("SELECT min(create_date)INTO :adt_treatment_date","n_account_payment_controller",+&
	"nf_check_claim_acceptance") < 0 THEN
		RETURN -1
	END IF
	
/* if all goes well then our date should be populated so lets
	set the checked flag to true
*/
	ab_check = TRUE

END IF

RETURN 1

end function

public function integer nf_init_hst_update_flag (boolean ab_hst_update_flag);

ib_hst_update_flag = ab_hst_update_flag

Return 1
end function

public function integer nf_init_display_hst_unchecked (boolean ab_display_hst_unchecked);// This is only set to true if the recover hst flag is being changed from checked to unchecked for a
//	pre-existing payment in Display mode in Account Payment

ib_display_hst_unchecked = ab_display_hst_unchecked

Return 1
end function

public function integer nf_validate_recipient (string as_recipient_type_code, long al_recipient_no);LONG             ll_result, ll_tranrow
//STRING           ls_recipient_care_of, ls_recipient_street, ls_recipient_city, ls_recipient_province
//STRING           ls_recipient_country, ls_recipient_postal_code, ls_recipient_name
STRING 			  ls_recipient_sub_type
DATAWINDOWCHILD  ldwc_child

	ll_tranrow = idw_dw[2].GetRow()

	CHOOSE CASE as_recipient_type_code
		CASE ''
			Return -1
			
		/* March 1998 Initialize the recipient sub-type to blank for Individuals and No Recipients*/	
	   CASE "I"
				idw_dw[2].SetItem(ll_tranrow,"recipient_sub_type_code",' ')  /* March 25, 1998  Initialize the recipient sub-type */
				Return 1
	   CASE ELSE
		/*		If recipient is a payee, read the address from Service Providers table          
		*/
			IF IsNull(as_recipient_type_code) OR IsNull(al_recipient_no) THEN
				MessageBox('Warning','Recipient number or type is missing')
				Return -1
			END IF

			/* March 25, 1998 - Get the current provider sub-type for the recipient entered  */
			SELECT PROVIDER.provider_sub_type_code
	   	  INTO :ls_recipient_sub_type
		     FROM PROVIDER  
		     WHERE ( PROVIDER.provider_no = :al_recipient_no ) AND  
			   	 ( PROVIDER.provider_type_code = :as_recipient_type_code) AND
      	       ( PROVIDER.active_flag = 'Y' )  
	        USING SQLCA ; 
			 
		   ll_result = SQLCA.nf_handle_error("Embedded SQL: Retrieve on SERVICE_PROVIDER","n_account_payment_controller","nf_validate_recipient")
		   IF ll_result < 0 THEN
	   		return -1
		   ELSEIF ll_result = 100 THEN
			   Return 100
			ELSE
				IF IsNull(ls_recipient_sub_type) THEN
 					idw_dw[2].SetItem(ll_tranrow,"recipient_sub_type_code",' ')  
				ELSE
					idw_dw[2].SetItem(ll_tranrow,"recipient_sub_type_code",ls_recipient_sub_type)   /* March 25, 1998  Set the recipient sub-type*/
				END IF
				Return 1
	   	END IF
	END CHOOSE	
Return 0
end function

public subroutine nf_init ();U_DWA ldw_dw[]

// create all the objects that need controlling from here

inv_tax_functions = create n_tax_functions
inv_payment = Create n_payment
ldw_dw[1] = idw_dw[1]															// 
ldw_dw[2] = idw_dw[2]															//
ldw_dw[3] = idw_dw[3]															// 
ldw_dw[4] = idw_dw[4]	
       														
																						
																						// 
inv_payment.nf_init(ldw_dw[], SQLCA, iwi_window_parent)

// set up the commit flag to only allow this controller to commit
nf_set_commit(TRUE)
inv_payment.nf_set_commit(FALSE)

Return
end subroutine

public function integer nf_validate_account_payments ();DATAWINDOWCHILD ldwc_child

	idw_dw[1].GetChild('payment_type_code', ldwc_child)
	IF ldwc_child.Find("payment_type_code = '" + idw_dw[1].GetItemString(1,"payment_type_code") + "'",1,ldwc_child.RowCount()) <= 0 THEN
		MessageBox("Validation Error","The payment type code you selected is no longer active",Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn("payment_type_code")
		Return -1
	END IF

/*	The period to date cannot be before the period from date
*/
	IF Date(idw_dw[1].GetItemDateTime(1,"paid_from_date")) < Date('1900/01/01') THEN
		MessageBox('Invalid Paid From Date', 'The paid from date is less than 1900.')
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn("paid_from_date")
	   Return -1
	END IF
		

	IF DaysAfter(Date(idw_dw[1].GetItemDateTime(1,"paid_from_date")),Date(idw_dw[1].GetItemDateTime(1,"paid_to_date"))) < 0 THEN
		MessageBox("Validation Error","The period end date cannot be less than the period start date",Exclamation!)
		idw_dw[1].SetFocus()
		idw_dw[1].SetColumn("paid_to_date")
	   Return -1
	END IF

	IF idw_dw[1].GetItemDateTime(1,'scheduled_processing_date') > DateTime(RelativeDate(Date(f_server_datetime()), 366)) THEN
		MessageBox('Invalid Processing Date','The sheduled processing date cannot be more than a year in the future.')
		Return -1
	END IF

	IF idw_dw[1].GetItemDateTime(1,'scheduled_processing_date') < DateTime(Date('1900/01/01')) THEN
		MessageBox('Invalid Processing Date','The sheduled processing date cannot be before 1900.')
		Return -1
	END IF

Return 0
end function

public function long nf_set_identifiers ();LONG 		ll_sub_claim_no, ll_payment_no, ll_row, ll_loop,ll_txn_no,ll_recipient_no, ll_return, ll_rowcount, ll_doc_id
STRING	ls_recipient_type_code, ls_txn_type
//********************************************************************
// get all the keys and set on all the datawindows
//********************************************************************


	IF idw_dw[1].RowCount() > 0 THEN
// get identifiers if data present
		IF IsNull(idw_dw[1].GetItemNumber(1,'payment_no')) THEN
			ll_payment_no = inv_payment.nf_get_next_payment_identifier()
			IF ll_payment_no < 0 THEN Return -1
			idw_dw[1].SetItem(1,"payment_no",ll_payment_no)
			idw_dw[1].SetItem(1,"claim_no",istr_message.al_doubleparm[2])	// claim no
		ELSE
			ll_payment_no = idw_dw[1].GetItemNumber(1,'payment_no')
		END IF
		IF IsNull(idw_dw[2].GetItemNumber(1, 'txn_no')) THEN
			ll_txn_no = inv_payment.nf_get_next_txn_identifier()
			IF ll_txn_no < 0 THEN Return -1

			idw_dw[2].SetItem(1,"txn_no",ll_txn_no)
			idw_dw[2].SetItem(1,"claim_no",istr_message.al_doubleparm[2])
			idw_dw[2].SetItem(1,"payment_no",ll_payment_no)
		ELSE
			ll_txn_no = idw_dw[2].GetItemNumber(1, 'txn_no')
		END IF

//	IF there are invoice records left, put in the identifiers and save them
		ll_row = idw_dw[5].RowCount()
		IF ll_row > 0 THEN
			ls_recipient_type_code	= idw_dw[2].GetItemString(1,"recipient_type_code")
			ll_recipient_no		   = idw_dw[2].GetItemNumber(1,"recipient_no")
			ll_loop = 1
			DO WHILE ll_loop <= ll_row
				idw_dw[5].SetItem(ll_loop,"recipient_no",ll_recipient_no)
				idw_dw[5].SetItem(ll_loop,"recipient_type_code",ls_recipient_type_code)
				idw_dw[5].SetItem(ll_loop,"payment_no",ll_payment_no)
				ll_loop ++
			LOOP
		END IF

	   IF idw_dw[6].RowCount() > 0 THEN 										// payment doc
 			idw_dw[6].SetItem(1,"payment_no",ll_payment_no)
		END IF
		
		//	Put the payment no on the travel expense records
		ll_row = idw_dw[13].RowCount()
		IF ll_row > 0 THEN
			ll_loop = 1
			DO WHILE ll_loop <= ll_row
				IF IsNull(idw_dw[13].getitemnumber(ll_loop,"payment_no")) or idw_dw[13].getitemnumber(ll_loop,"payment_no") = 0 THEN
					idw_dw[13].SetItem(ll_loop,"payment_no",ll_payment_no)
				END IF	
				ll_loop ++
			LOOP
		END IF

	END IF
	
	/* for medical society phase number 1
	*/
	ll_return = nf_set_identifiers_bonus()
	IF ll_return = -1 THEN
		return -1
	END IF
	

Return 0
end function

public function integer nf_retrieve_for_delete (long al_payment_no);// nf_retrieve_for_delete
//
Integer li_rtn
Long    ll_rowcount, ll_pymt_no, ll_txn_no, ll_return, ll_payment_no, ll_doc_id
String  ls_paid_status_code, ls_payment_type_code
DataWindowChild ldwc_child

// Lets use some meaningful variables so you can actually read the code
ll_payment_no = istr_message.al_doubleparm[1]
ll_doc_id = istr_message.al_doubleparm[3]

//	Check to see if we have a payment document record and if so, retrieve it.
IF ll_doc_id > 0 THEN					//doc id
	ll_rowcount = idw_dw[6].Retrieve(ll_doc_id, ll_payment_no)			// payment doc
	li_rtn = SQLCA.nf_handle_error("n_account_payment_controller", "", "nf_retrieve_for_delete - idw_dw[6].Retrieve(ll_doc_id, ll_payment_no)") 

	IF ll_rowcount <= 0 THEN
		MessageBox("Account Payment Maintenance - Data Integrity Error","Record not found retrieving from payment_document table for payment # " + &
						String(ll_payment_no) + " / document # " + string(ll_doc_id) + "~r~nAnother user may have deleted the payment.",Exclamation!)
		RETURN -1
	END IF

	// Make sure it is still ok to delete this payment (ie. another user may have changed something since it was first displayed back on the list
	ls_paid_status_code = idw_dw[6].GetItemString(1,"paid_status_code")
	IF ls_paid_status_code = "P" THEN
		MessageBox("Account Payment Maintenance", "You cannot delete a payment that has been processed", Exclamation!)
		RETURN -1
	END IF

	//	Check to see IF we need to change the screen display
	IF ls_paid_status_code = "O" OR ls_paid_status_code = "R"  OR ls_paid_status_code = "H" THEN
		idw_dw[1].Visible = FALSE  // payment 
		idw_dw[2].Visible = FALSE  // transactions
		idw_dw[6].Visible = TRUE   // payment doc
	END IF
END IF

//	IF there is a payment record retrieve it
IF ll_payment_no > 0 THEN
	ll_rowcount = idw_dw[1].Retrieve(ll_payment_no)
	li_rtn = SQLCA.nf_handle_error("n_account_payment_controller", "", "nf_retrieve_for_delete - idw_dw[1].Retrieve(ll_payment_no)") 
	
	IF ll_rowcount <= 0 THEN
		MessageBox("Account Payment Maintenance - Data Integrity Error","Record not found retrieving from PAYMENT table for payment #" + &
						string(ll_payment_no) + "~r~nAnother user may have deleted the payment.",Exclamation!)
		RETURN -1
	END IF

	ll_rowcount = idw_dw[2].Retrieve(ll_payment_no)
	li_rtn = SQLCA.nf_handle_error("n_account_payment_controller", "", "nf_retrieve_for_delete - idw_dw[2].Retrieve(ll_payment_no)") 
	IF ll_rowcount <= 0 THEN
		MessageBox("Account Payment Maintenance - Data Integrity Error","Record not found retrieving from TRANSACTION table for payment #" + &
						string(ll_payment_no) + "~r~nAnother user may have deleted the payment.",Exclamation!)
		RETURN -1
	END IF

	ll_rowcount = idw_dw[5].Retrieve(ll_payment_no)
	li_rtn = SQLCA.nf_handle_error("n_account_payment_controller", "", "nf_retrieve_for_delete - idw_dw[5].Retrieve(ll_payment_no)") 

	ll_pymt_no = idw_dw[1].GetItemNumber(idw_dw[1].GetRow(), 'payment_no')
	ll_txn_no = idw_dw[2].GetItemNumber(idw_dw[2].GetRow(), 'txn_no')

	// Make sure it is still ok to delete this payment (ie. another user may have changed something since it was first displayed back on the list
	IF IsNull(idw_dw[1].GetItemDateTime(1,"processed_date")) THEN
		IF idw_dw[2].GetItemNumber(1,"batch_no") > 0 THEN
			MessageBox("Account Payment Maintenance","You cannot delete a payment that is scheduled for processing",Exclamation!)
			RETURN -1
		END IF
	END IF

	// Filter payment_sub_type_code so that the proper sub type descriptions gets displayed.  Ie. payment subtype '05'  is in 21/05, 23/05, 97/05
	ls_payment_type_code = idw_dw[1].GetItemString(1, 'payment_type_code') 
	li_rtn = idw_dw[1].GetChild('payment_sub_type_code', ldwc_child) 
	li_rtn = ldwc_child.SetFilter('payment_type_code = "'+ ls_payment_type_code + '"')
	li_rtn = ldwc_child.Filter()
END IF

RETURN 1

end function

public function integer nf_set_unused_fields ();//*************************************************************************
LONG ll_row

	IF idw_dw[1].GetRow() = 1 THEN
		IF IsNull(idw_dw[1].GetItemNumber(1,'opening_no')) THEN
			idw_dw[1].SetItem(1,'opening_no',0)
		END IF
	END IF

/*	call the functions to set the unused fields
*/
	IF inv_payment.nf_set_unused_fields() < 0 THEN Return -1

/*	temp - move to payments  - July 28/97
*/
	IF IsNull(idw_dw[1].GetItemNumber(1,'authorization_no')) OR idw_dw[1].GetItemNumber(1,'authorization_no') = 0 THEN
		idw_dw[1].SetItem(1,'authorization_no',0)
		idw_dw[1].SetItem(1,'paid_quantity',0)
	END IF

	FOR ll_row = 1 TO idw_dw[2].RowCount()
		/* August 31, 1999 - New validation for default address flag. Cannot be No for payment methods D and R or non-individual */
		IF idw_dw[2].GetItemString(ll_row, "payment_method_code") = "D" OR &
		  idw_dw[2].GetItemString(ll_row, "payment_method_code") = "R" OR &
		  idw_dw[2].GetItemString(ll_row,"recipient_type_code") <> 'I' THEN
		  idw_dw[2].SetItem(ll_row,"use_default_address_flag", 'Y')
		  idw_dw[2].SetItem(ll_row,"cheque_print_group_code", ' ')
		END IF

		/* August 31, 1999 - If the default is to be used then don't save address information */

	NEXT

Return 0

end function

public function integer nf_validate_authorization (long al_authorization_no, string as_mode);LONG		ll_rows, ll_qty, ll_submitted_qty, ll_paid_qty, ll_original_qty, ll_billable_xref_no
DECIMAL	ldec_amount, ldec_submitted_amount, ldec_paid_amount, ldec_original_amount
STRING	 ls_flag, ls_type

/*	validate the data against the authorization
*/

/*	determine if this is a new add or a modification
*/
	ls_type = ''
	
	IF idw_dw[1].GetItemStatus(1,0,Primary!)  = NewModified! THEN
		ls_type = 'N'
	ELSEIF idw_dw[1].GetItemNumber(1,'authorization_no',Primary!,TRUE) <> idw_dw[1].GetItemNumber(1,'authorization_no') THEN
		ls_type = 'M'
	END IF
/*	get the authorization information
*/
	ll_rows = idw_dw[7].Retrieve(al_authorization_no,istr_message.al_doubleparm[2])
	IF SQLCA.nf_handle_error('Retrieve authorization','n_account_payment_controller','nf_validate_authorization') < 0 THEN
		Return -1
	END IF
	IF ll_rows < 1 THEN
		MessageBox('Error Retreiving Authorization','The authorization item cannot be found for this authorization number.  Please try again.')
		Return -1
	END IF
	IF ls_type = 'M' AND idw_dw[1].GetItemNumber(1,'authorization_no',Primary!,TRUE) <> 0 THEN
		ll_rows = idw_dw[8].Retrieve(idw_dw[1].GetItemNumber(1,'authorization_no',Primary!,TRUE),istr_message.al_doubleparm[2])
		IF SQLCA.nf_handle_error('Retrieve authorization','n_account_payment_controller','nf_validate_authorization') < 0 THEN
			Return -1
		END IF
		IF ll_rows < 1 THEN
			MessageBox('Error Retreiving Authorization','The authorization item cannot be found for this authorization number.  Please try again.')
			Return -1
		END IF
	END IF
	ldec_amount           = idw_dw[7].GetItemDecimal(1,'authorized_amount')
	ll_qty                = idw_dw[7].GetItemNumber(1,'authorized_quantity')
    ll_billable_xref_no = idw_dw[7].Getitemnumber(1,'billable_xref_no')
	ll_paid_qty           = idw_dw[7].GetItemNumber(1,'paid_quantity')
	ldec_paid_amount      = idw_dw[7].GetItemDecimal(1,'paid_amount')
	
	ldec_submitted_amount = idw_dw[1].GetItemDecimal(1,'total_award_amount')
	ll_submitted_qty      = idw_dw[1].GetItemNumber(1,'paid_quantity')
	IF ls_type <> 'N' THEN
		ldec_original_amount = idw_dw[1].GetItemDecimal(1,'total_award_amount', Primary!, TRUE)
		ll_original_qty      = idw_dw[1].GetItemNumber(1,'paid_quantity', Primary!, TRUE)
	ELSE
		ldec_original_amount = 0
		ll_original_qty = 0
	END IF

	IF IsNull(ldec_original_amount) THEN ldec_original_amount = 0
	IF IsNull(ll_original_qty) THEN ll_original_qty = 0

	IF as_mode = 'UPDATE' THEN
/* find out if the amount is mandatory*/
		
		IF ll_submitted_qty < 1 THEN
			MessageBox('Error','The paid quantity is required when an authorization is selected.')
			Return -1
		END IF
		
		IF ls_type = 'M' THEN
/*			update the old authorization item to remove the amount and quantity
*/
			IF ldec_amount > 0 THEN  // authorized amount is greater than zero
				IF ldec_submitted_amount > (ldec_amount - ldec_paid_amount) THEN
					MessageBox('Error - Invalid Mandatory Amount','The payment amount is for more than the remaining authorized amount.')
					Return -1
				END IF
			END IF
			IF ll_qty >= 1 THEN		// authorized quantity
				IF ll_submitted_qty > (ll_qty - ll_paid_qty) THEN   //the authorization number changed
					MessageBox('Error','The quantity submitted is greater than the authorized quantity.')
					Return -1
				END IF
			END IF
			IF idw_dw[8].RowCount() > 0 THEN // there may have not been another authorization number
				idw_dw[8].SetItem(1,'paid_quantity', idw_dw[8].GetItemNumber(1,'paid_quantity') - ll_original_qty)
				idw_dw[8].SetItem(1,'paid_amount', idw_dw[8].GetItemDecimal(1,'paid_amount') - ldec_original_amount)
			END IF
			idw_dw[7].SetItem(1,'paid_quantity', ll_paid_qty + ll_submitted_qty)
			idw_dw[7].SetItem(1,'paid_amount', ldec_paid_amount + ldec_submitted_amount)
		ELSE
			IF ldec_amount > 0 THEN  // authorized amount is greater than zero
				IF ldec_submitted_amount > (ldec_amount - ldec_paid_amount) + ldec_original_amount THEN
					MessageBox('Error - Invalid Mandatory Amount','The payment amount is for more than the remaining authorized amount.')
					Return -1
				END IF
			END IF
			IF ll_qty >= 1 THEN		// authorized quantity
				IF ll_submitted_qty > (ll_qty - ll_paid_qty) + ll_original_qty THEN
					MessageBox('Error','The quantity submitted is greater than the authorized quantity.')
					Return -1
				END IF
			END IF
			idw_dw[7].SetItem(1,'paid_quantity', ll_paid_qty + ll_submitted_qty - ll_original_qty)
			idw_dw[7].SetItem(1,'paid_amount', ldec_paid_amount + ldec_submitted_amount - ldec_original_amount)
		END IF
	
/* 	if deleting then remove paid amounts
*/	

	ELSEIF as_mode = 'DELETE' THEN
		idw_dw[7].SetItem(1,'paid_quantity', ll_paid_qty - ll_submitted_qty)
		idw_dw[7].SetItem(1,'paid_amount', ldec_paid_amount - ldec_submitted_amount)
	
	END IF

	
	Return 0
end function

public function integer nf_check_mandatory ();
//************************************************************************
// call the check mandatory functions in all the children
//************************************************************************
   IF idw_dw[1].AcceptText() < 0 THEN Return -1
	IF idw_dw[2].AcceptText() < 0 THEN Return -1
	IF idw_dw[5].AcceptText() < 0 THEN Return -1
	IF idw_dw[6].AcceptText() < 0 THEN Return -1
	IF idw_dw[7].AcceptText() < 0 THEN Return -1
	IF idw_dw[8].AcceptText() < 0 THEN Return -1

/*	check for a few defaults - this needs to be at least one for payment validation
*/
	IF idw_dw[1].GetItemNumber(1,'nmbr_cycles') < 1 OR IsNull(idw_dw[1].GetItemNumber(1,'nmbr_cycles')) THEN
		idw_dw[1].SetItem(1,'nmbr_cycles',1)
	END IF

/*		account payments must have an amount
      production version does not hit this message - decimal problem?
*/
	IF IsNull(idw_dw[1].GetItemNumber(1,'total_award_amount')) OR idw_dw[1].GetItemNumber(1,'total_award_amount') = 0 THEN
		MessageBox('Invalid Amount', 'The payment amount must be entered.')
		Return -1
	END IF

/*    Check Paid dates */

   IF IsNull(idw_dw[1].GetItemDateTime(1,'paid_from_date')) = TRUE AND IsNull(idw_dw[1].GetItemDateTime(1,'paid_to_date')) = FALSE THEN
		idw_dw[1].SetItem(1,'paid_from_date',idw_dw[1].GetItemDateTime(1,'paid_to_date'))
	END IF

   IF IsNull(idw_dw[1].GetItemDateTime(1,'paid_to_date')) = TRUE AND IsNull(idw_dw[1].GetItemDateTime(1,'paid_from_date')) = FALSE THEN
		idw_dw[1].SetItem(1,'paid_to_date',idw_dw[1].GetItemDateTime(1,'paid_from_date'))
	END IF
	
	IF inv_payment.nf_check_mandatory() < 0 THEN Return -1

Return 0
end function

public function integer nf_validate_account_transactions ();STRING				ls_payment_type_code, ls_payment_method_code, ls_string
LONG 					ll_result, ll_no
DATAWINDOWCHILD	ldwc_child
LONG					ll_row			// for PR 1679

/*	Check to ensure that if a voc rehab payment type code is selected, then
	either a voc rehab payee, or the claimant has been chosen as the recipient
*/
	ll_row = idw_dw[1].GetRow()
	IF ll_row > 0 THEN
		ls_payment_type_code = idw_dw[1].GetItemString(ll_row,"payment_type_code")
	END IF
	IF ls_payment_type_code = "07" OR ls_payment_type_code = "08" OR ls_payment_type_code = "09" &
	OR ls_payment_type_code = "10" THEN
		IF idw_dw[2].GetItemString(1,'recipient_type_code') = "M" THEN
			MessageBox("Account Payment Maintenance","You must choose a voc rehab payee as the recipient for the selected payment type",Exclamation!)
			idw_dw[2].SetFocus()
			idw_dw[2].SetColumn("recipient_type_code")
			Return -1
		END IF
	ELSE
		IF idw_dw[2].GetItemString(1,'recipient_type_code') = "V" THEN
			MessageBox("Account Payment Maintenance","You must choose a medical aid payee as the recipient for the selected payment type",Exclamation!)
			idw_dw[2].SetFocus()
			idw_dw[2].SetColumn("recipient_type_code")
			Return -1
		END IF
	END IF

/*	IF the payment method is handwritten cheque, THEN ensure that the handwritten cheque
	fields have been entered
*/


	ls_payment_method_code = idw_dw[2].GetItemString(1,"payment_method_code")
	IF ls_payment_method_code = "H" THEN
		IF IsNull(idw_dw[2].GetItemDateTime(1,"cheque_deposit_date")) THEN
			MessageBox("Account Payment Maintenance","You must enter a manual cheque date for handwritten cheques",Exclamation!)
			idw_dw[2].SetFocus()
			idw_dw[2].SetColumn("cheque_deposit_date")
			Return -1
		END IF
		IF idw_dw[2].GetItemNumber(1,"cheque_no") <= 0 THEN
			MessageBox("Account Payment Maintenance","You must enter a manual cheque number for handwritten cheques",Exclamation!)
			idw_dw[2].SetFocus()
			idw_dw[2].SetColumn("cheque_no")
			Return -1
		END IF
	END IF
	IF ls_payment_method_code = "A" OR  ls_payment_method_code = "H" THEN
		IF idw_dw[2].GetItemString(1,'use_default_address_flag') = 'N' THEN
			IF nf_validate_address() < 0 THEN Return -1
		END IF
	END IF
	
	CHOOSE CASE idw_dw[2].GetItemString(1,'recipient_type_code')
		CASE "I","M","V"
			IF idw_dw[2].GetItemString(1,'use_default_address_flag') = 'Y' THEN
				IF nf_validate_address() < 0 THEN Return -1
			END IF 
		CASE ELSE
	END CHOOSE
	
	idw_dw[2].GetChild('recipient_type_code', ldwc_child)

	ls_string = idw_dw[2].GetItemString(1,"recipient_type_code")
	ll_result = ldwc_child.Find("recipient_type_code = '" + ls_string + "'",1, ldwc_child.RowCount())
	IF ll_result <= 0 or IsNull(ll_result) THEN
		MessageBox("Account Payment Module - Validation Error","Recipient Type must be a valid.",Exclamation!)
		idw_dw[2].SetFocus()
		idw_dw[2].SetColumn("recipient_type_code")
		Return -1
	END IF


/*	validate the recipient no
*/
	ls_string = idw_dw[2].GetItemString(1,"recipient_type_code")
	ll_no = idw_dw[2].GetItemNumber(1,'recipient_no')
	IF IsNull(ll_no) OR ll_no = 0 THEN
		MessageBox('Missing Recipient Number','The recipient number has not been entered.')
		idw_dw[2].SetColumn("recipient_no")
		idw_dw[2].SetFocus()
		Return -1
	ELSE
		ll_result = nf_validate_recipient(ls_string,ll_no)
		IF ll_result < 0 THEN
			Return -1
		ELSEIF ll_result = 100 THEN
			MessageBox("Payment Module - Validation Error","Not a valid Payee" + &
			"~r~nYou must select another Payee number",Exclamation!)
			Return -1
		END IF
	END IF
/*	validate that the print cheque group code is set correctly - there seems to be a bug - if you
	double click the button it gets set even though the action code on the dw is set to 1
*/
	IF idw_dw[2].GetItemString(1,'cheque_print_group_code') = 'W' THEN
		IF idw_dw[2].GetItemString(1, 'recipient_type_code') <> 'I' THEN
			MessageBox('Warning', 'Only cheques issued to the individual can be sent to WRC.')
         Return -1
		END IF
		IF idw_dw[2].GetItemString(1, 'payment_method_code') <> 'A' THEN
			MessageBox('Warning', 'Only automated cheques can be sent to WRC.')
         Return -1
		END IF
	END IF
Return 0
end function

public function integer nf_trans_itemfocuschanged ();STRING	ls_explanation, ls_invoice, ls_from, ls_to
LONG		ll_cntr,	ll_rowcount, ll_row	
DATE		ld_from,	ld_to

ls_from = ""
ls_to = ""

/*
	ldw_dw[1] = uo_payment.dw_account_payment
	ldw_dw[2] = uo_payment.dw_account_transactions
	ldw_dw[3] = uo_payment.dw_deleted_payment- code removed
	ldw_dw[4] = uo_payment.dw_deleted_claim_txn - code removed
	ldw_dw[5] = dw_invoice
	ldw_dw[6] = dw_payment_document
*/

/*	Check to see if we need to default the explanation field
*/

	ll_row = idw_dw[2].GetRow()
	IF ll_row > 0 THEN

		IF idw_dw[2].GetColumnName() = "explanation" THEN
			ls_explanation = idw_dw[2].GetText()
			IF IsNull(ls_explanation) or Trim(ls_explanation) = '' THEN
/*				First, check for invoice #'s
*/
				ll_cntr = 1
				ll_rowcount = idw_dw[5].RowCount()
				DO WHILE ll_cntr <= ll_rowcount
					ls_invoice = idw_dw[5].GetItemString(ll_cntr,"invoice_no")
					IF Trim(ls_invoice) > "" THEN
						IF ls_from = "" THEN
							ls_from = trim(ls_invoice)
						ELSE
							ls_to = trim(ls_invoice)
						END IF
					END IF
					ll_cntr ++
				LOOP
/*				If nothing found, check the paid from and to dates
*/
				IF ls_from = "" THEN
					ld_from = Date(idw_dw[1].GetItemDateTime(1,"paid_from_date"))
					ld_to = Date(idw_dw[1].GetItemDateTime(1,"paid_to_date"))
					IF NOT IsNull(ld_from) THEN
						ls_from = string(ld_from,"yyyy-mm-dd")
					END IF
					IF NOT IsNull(ld_to) THEN
						ls_to = string(ld_to,"yyyy-mm-dd")
					END IF
				END IF

/*				Now, if we have anything format it and store it in explanation
*/
				IF ls_from = "" THEN Return 0
					IF ls_to = "" OR ls_to = ls_from THEN
					idw_dw[2].SetItem(ll_row,"explanation",ls_from)
				ELSE
					idw_dw[2].SetItem(ll_row,"explanation",ls_from + " - " + ls_to)
				END IF
			END IF
		END IF
	END IF
Return 0
end function

public function integer nf_validate_address ();DATAWINDOWCHILD	ldwc_child
LONG					ll_row
STRING				ls_string,ls_address_one,ls_address_two
STRING            ls_postal_code

FOR ll_row = 1 TO idw_dw[2].RowCount()
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

ls_address_one = idw_dw[2].GetItemString(1,'address_line1')
ls_address_two = idw_dw[2].GetItemString(1,'address_line2')
ls_postal_code = idw_dw[2].GetItemString(1,'postal_code')
	
/* ensure that at least on line in the address has been saved */
IF trim(ls_address_one) = "" and trim(ls_address_two) = "" THEN
	MessageBox('Invalid Address','The Address portion must be included.')	
	idw_dw[2].SetColumn('address_line1')
	idw_dw[2].SetFocus()
	Return -1
END IF 

/* postal code must be there even if the use_default_address_flag = "Y" */
IF trim(ls_postal_code) = "" OR ISNULL(ls_postal_code) THEN
	MessageBox('Invalid Postal Code','Postal Code is Mandatory and may be added through the applicable maintain screen')	
	idw_dw[2].SetFocus()
	Return -1
END IF 

/* address line number two now equals address line number 1 is this correct???? */
IF TRIM(ls_address_one) = "" and trim(ls_address_two) <> "" THEN
	idw_dw[2].SetItem(1,"address_line1",ls_address_two)
	idw_dw[2].SetItem(1,"address_line2","")
END IF

Return 0
end function

public function decimal nf_calculate_chiro_bonus_payment (datetime adt_treatment_date);
/* "Create a function to determine the amount of the Chiro early filing bonus
    1. Pass the argument variable adt_treatment_date
	    (DOCUMENT_INDEX.date_on_document)
	 2. Return the amount of early filing bonus payment found (money)
	    Determine the amount of the early filing bonus by the following
		 query:
		 
	  	SELECT early_filing_bonus_amount
	        FROM Chiro_Fee_Schedule
	       WHERE effective_date = (select max(effective_date) 
	       from Chiro_Fee_Schedule 
	       where effective_date <= :adt_treatment_date)"
										  
	  
ARGS: adt_treatment_date

RETURNS: amount of the chiro early filing bonus

*/
DEC  ldec_amount


SELECT early_filing_bonus_amount
  INTO :ldec_amount
  FROM Chiro_Fee_Schedule
 WHERE effective_date = (select max(effective_date) 
	                        from Chiro_Fee_Schedule 
			                 where effective_date <= :adt_treatment_date);
										  
//do sql check
IF SQLCA.nf_handle_error("SELECT early_filing_bonus_amount", +&
					"n_account_payment_controller","nf_calculate_chiro_bonus_payment") < 0 THEN
	RETURN -1
END IF
										  									  
IF isnull(ldec_amount) OR ldec_amount < 0 THEN
	ldec_amount = 0
END IF
		
RETURN ldec_amount
end function

public function integer nf_check_valid_physician (long al_provider_no, ref boolean ab_check, boolean ab_old);// nf_check_valid_physician 
//
// Determination of a valid Physician: 
//     - Provider type is "M" Medical Aid (PROVIDER.provider_type_code = "M")
//     - The provider sub type has Provider_Sub_Type.nbms_eligible_flag = 'Y' 
//     - The provider is currently active (PROVIDER.active_flag = "Y")
//     - The Physician must be a New Brunswick Physician (PROVIDER.prov_stae_code = "NB")
//
// ARGS: al_provider_no 
// 
// RETURNS: TRUE  is valid 
//          FALSE is NOT valid
//
Long    ll_count
Integer li_rtn
String  ls_eligible

SELECT COUNT(*),  P.nbms_early_filing_bonus_flag 
  INTO :ll_count, :ls_eligible
  FROM PROVIDER P, 
       Provider_Sub_Type PST 
 WHERE P.provider_type_code = 'M' 
   AND P.active_flag = 'Y' 
   AND P.prov_state_code = 'NB' 
	AND P.provider_no = :al_provider_no 
	AND P.provider_type_code = PST.provider_type_code 
	AND P.provider_sub_type_code = PST.provider_sub_type_code 
	AND PST.nbms_eligible_flag = 'Y' 
 GROUP BY P.nbms_early_filing_bonus_flag ; 

li_rtn = SQLCA.nf_handle_error("n_account_payment_controller", "", "nf_check_valid_physician - SELECT COUNT(*), P.nbms_early_filing_bonus_flag FROM PROVIDER P, Provider_Sub_Type PST") 
IF li_rtn < 0 THEN
	RETURN -1
END IF

IF ll_count <> 1 OR isnull(ll_count) THEN
	ab_check = FALSE
ELSE
	IF ab_old THEN
		ab_check = TRUE
	ELSE 
		IF ls_eligible = 'Y' THEN
			ab_check = TRUE
		END IF
	END IF
END IF

RETURN 1

end function

public function integer nf_check_chiro_document_paid (long al_doc_id, ref boolean ab_check);// nf_check_chiro_document_paid - Determining if a Chiro document has been paid
// 
// Arguments: al_doc_id (by value) - id of document to be checked
//            ab_check (by ref) -    true if the document has been paid, false if the document has not been paid
//
Integer li_rtn 
Long    ll_count 

ab_check = FALSE

SELECT COUNT(DISTINCT P.payment_no) 
  INTO :ll_count 
  FROM PAYMENT_DOCUMENT PD, 
  		 PAYMENT P 
 WHERE PD.doc_id = :al_doc_id
	AND PD.paid_status_code IN ('S', 'O', 'P') 
	AND P.payment_sub_type_code IN ('09', '10')
	AND P.payment_type_code = '21' 
	AND P.payment_no = PD.payment_no 
	AND P.zeroed_flag = 'N' ; 

li_rtn = SQLCA.nf_handle_error("", "n_account_payment_controller", "nf_check_chiro_document_paid - SELECT COUNT(DISTINCT P.payment_no) FROM PAYMENT_DOCUMENT PD, PAYMENT P ")
IF li_rtn < 0 THEN 
	RETURN -1
END IF

IF ll_count > 0 THEN 
	ab_check = TRUE
END IF

RETURN 1

end function

public function long nf_set_identifiers_bonus ();LONG 		ll_sub_claim_no, ll_payment_no, ll_row, ll_loop,ll_txn_no,ll_recipient_no
STRING	ls_recipient_type_code,ls_recipient_sub_type_code,ls_sub_type, ls_b_no, ls_t_no, ls_a_no
decimal  ldec_tax_rate
INTEGER  li_return
//********************************************************************
// get all the keys and set on all the datawindows
//********************************************************************


/* for project #10127 Medical Society Agreement we use this function to set the identifiers
   on our two new datawindows.
*/

IF idw_dw[9].RowCount() > 0 THEN
// get identifiers if data present
	
	//If the payment_no has a value, return.
	ll_payment_no = idw_dw[9].GetItemNumber(idw_dw[9].GetRow(), 'payment_no')
	ls_sub_type                = idw_dw[1].getitemstring(1,"payment_sub_type_code")
	ls_recipient_type_code     = idw_dw[2].getitemstring(1,"recipient_type_code")
	ls_recipient_sub_type_code = idw_dw[2].getitemstring(1,"recipient_sub_type_code")
	IF ll_payment_no > 0 AND ls_sub_type = '' THEN
		RETURN 0
	END IF

	ll_payment_no = inv_payment.nf_get_next_payment_identifier()
	IF ll_payment_no < 0 THEN Return -1
	idw_dw[9].SetItem(1,"payment_no",ll_payment_no)
	idw_dw[9].SetItem(1,"claim_no",istr_message.al_doubleparm[2])	// claim no

	IF IsNull(idw_dw[10].GetItemNumber(1, 'txn_no')) THEN
		ll_txn_no = inv_payment.nf_get_next_txn_identifier()
		IF ll_txn_no < 0 THEN Return -1

		idw_dw[10].SetItem(1,"txn_no",ll_txn_no)
		idw_dw[10].SetItem(1,"claim_no",istr_message.al_doubleparm[2])
		idw_dw[10].SetItem(1,"payment_no",ll_payment_no)	
	ELSE
		ll_txn_no = idw_dw[10].GetItemNumber(1, 'txn_no')
	END IF
	
	/* if all goes well then we can default all of our other values
	   from the values that are populated in dw1[1] and dw2[2]
	*/
	
	/* now we must seperate the manual entries from the automated ones
	   values taken from document
	*/
	
	IF isnull(ls_sub_type) THEN
		// either NBMS AEFB or NB Chiro AEFB
		IF ls_recipient_type_code = 'M' THEN
			CHOOSE CASE ls_recipient_sub_type_code
				CASE '10','11','12','13','14','15','16','17','18','19',&
					  '20','21','22','23','24','25','26','27','28','32','33','34','42','43'
					ls_sub_type = '01'
				CASE '04'
					ls_sub_type = '09'
			END CHOOSE
		END IF
	END IF
	
	IF ls_sub_type <> "02" THEN //we have an automated one
		// either NBMS AEFB or NB Chiro AEFB
		IF ls_recipient_type_code = 'M' THEN
			CHOOSE CASE ls_recipient_sub_type_code
				CASE '10','11','12','13','14','15','16','17','18','19',&
					  '20','21','22','23','24','25','26','27','28','32','33','34','42','43'
					ls_sub_type = '01'
				CASE '04'
					ls_sub_type = '09'
			END CHOOSE
		END IF
	END IF


	// bonus payment
	idw_dw[9].SetItem(1,"authorized_by_code",idw_dw[1].getitemstring(1,"authorized_by_code"))
	idw_dw[9].SetItem(1,"authorized_date",idw_dw[1].getitemdatetime(1,"authorized_date"))
	idw_dw[9].SetItem(1,'authorization_no',0)
	idw_dw[9].SetItem(1,"opening_no",0)
	//idw_dw[9].SetItem(1,"total_award_amount",0) 
	idw_dw[9].SetItem(1,"payment_type_code","21")
	idw_dw[9].SetItem(1,"payment_sub_type_code",ls_sub_type)
	idw_dw[9].SetItem(1,"paid_from_date",idw_dw[1].GetItemDateTime(1,'paid_from_date'))
	idw_dw[9].SetItem(1,"paid_to_date",idw_dw[1].GetItemDateTime(1,'paid_to_date'))
	
	
	/* new columns for p10229 */
	idw_dw[9].SetItem(1,'paid_quantity',0)
	idw_dw[9].SetItem(1,'tax_amount',0.00)
	idw_dw[9].SetItem(1,'adjustment_tax_amount',0.00)
	idw_dw[9].SetItem(1,'adjustment_payment_amount',0)
	idw_dw[9].SetItem(1,'payment_adjustment_flag',"N")
	idw_dw[9].SetItem(1,'total_deductions',0)
	idw_dw[9].SetItem(1,'final_payment_flag',"N")
	idw_dw[9].SetItem(1,'award_no',0)
	idw_dw[9].SetItem(1,'benefit_calculation_no',0)
	idw_dw[9].SetItem(1,'loe_explanation',"")
	idw_dw[9].SetItem(1,'paid_days_lost', 0)
	idw_dw[9].SetItem(1,"day_amount",0.00)
	idw_dw[9].SetItem(1,"paid_hours_lost",0)
	idw_dw[9].SetItem(1,"hour_amount",0.00)
	
	// need to set the tax rate on the bonus payment
//	li_return = inv_tax_functions.nf_get_valid_tax_rate(date(istr_message.adtm_datetimeparm[1]), date(idw_dw[1].GetItemDateTime(1,'paid_to_date')),ldec_tax_rate)
	//IF li_return < 0 OR ISNULL(ldec_tax_rate)THEN 
//		MessageBox ('Invalid Tax Rate','The Tax Rate could not be determined from the paid from and paid to dates', Information!)
//		Return -1
//	END IF 
	
	//set the tax rate on the payment record
	idw_dw[9].setitem(1,"tax_rate",0.00)
	
	idw_dw[10].SetItem(1,'maintain_allowed_flag',"Y")
	idw_dw[10].SetItem(1,'txn_sub_type_code',"")
	idw_dw[10].SetItem(1,'tax_amount',0.00)
	idw_dw[10].SetItem(1,'txn_unit_of_work_no',0)
	idw_dw[10].SetItem(1,'direct_deposit_xmit_no',0)
	idw_dw[10].SetItem(1,'related_txn_no',0)
	idw_dw[10].SetItem(1,'cheque_no',0)
	idw_dw[10].SetItem(1,'coc_period',0)
	idw_dw[10].SetItem(1,'explanation',"")
	idw_dw[10].SetItem(1,"address_line1",idw_dw[2].getitemstring(1,"address_line1"))
	idw_dw[10].SetItem(1,"address_line2",idw_dw[2].getitemstring(1,"address_line2"))
	idw_dw[10].SetItem(1,"city",idw_dw[2].getitemstring(1,"city"))
	idw_dw[10].SetItem(1,"prov_state_code",idw_dw[2].getitemstring(1,"prov_state_code"))
	idw_dw[10].SetItem(1,"country",idw_dw[2].getitemstring(1,"country"))
	idw_dw[10].SetItem(1,"postal_code",idw_dw[2].getitemstring(1,"postal_code"))
	
	/* end of new column adds */

	// bonus txn
	
	inv_payment.nf_get_bank(idw_dw[2].GetItemNumber(1,'recipient_no'),'M',ls_b_no, ls_t_no, ls_a_no, ll_payment_no,ll_txn_no)
	IF Len(ls_b_no) > 0 AND Len(ls_t_no) > 0 AND Len(ls_a_no) > 0 THEN
		idw_dw[10].SetItem(1, "payment_method_code", "D")
		idw_dw[10].SetItem(1, "bank_no", ls_b_no)
		idw_dw[10].SetItem(1, "bank_transit_no", ls_t_no)
		idw_dw[10].SetItem(1, "bank_account_no", ls_a_no)
	ELSE
		idw_dw[10].SetItem(1,"payment_method_code","A")
		idw_dw[10].SetItem(1,"bank_no"," ")
		idw_dw[10].SetItem(1,"bank_transit_no"," ")
		idw_dw[10].SetItem(1,"bank_account_no"," ")
	END IF
	
	idw_dw[10].SetItem(1,"admin_region_code",inv_payment.idw_basic_claim.GetItemString(1,'admin_region_code'))
	idw_dw[10].SetItem(1,"scheduled_processing_date",idw_dw[1].GetItemDateTime(1,'scheduled_processing_date'))
	idw_dw[10].SetItem(1,"use_default_address_flag","Y")
	idw_dw[10].SetItem(1,"cheque_print_group_code"," ")	
	idw_dw[10].SetItem(1,"screen_type","act")
	idw_dw[10].SetItem(1,"recipient_type_code","M")
	idw_dw[10].SetItem(1,"recipient_no",idw_dw[2].getitemnumber(1,"recipient_no"))
	idw_dw[10].SetItem(1,"txn_type_code","1")
	idw_dw[10].SetItem(1,"batch_no",0)	
	idw_dw[10].SetItem(1,"explanation","Bonus/Prime")
	idw_dw[10].SetItem(1,"recipient_name",idw_dw[2].getitemstring(1,"recipient_name"))
	idw_dw[10].SetItem(1,"recipient_sub_type_code",idw_dw[2].getitemstring(1,"recipient_sub_type_code"))
	
	// PR 3403 - set related_txn_no
	idw_dw[10].setitem(1,'related_txn_no', idw_dw[2].GetItemNumber(1, 'txn_no' ) )
	
	// payment document
	idw_dw[11].SetItem(1,"payment_no",ll_payment_no)
	idw_dw[11].SetItem(1,"doc_id",istr_message.al_doubleparm[3])
	idw_dw[11].SetItem(1,"paid_status_code","S")
	
END IF
Return 0
end function

public function decimal nf_calculate_working_days (datetime adt_start_date, datetime adt_end_date, string as_company_code);/* Calculation of working days
  " - Pass the argument variable adt_start_date and adt_end_date with type datetime
	  (NOTE:DOCUMENT_INDEX.date_on_document is the start date and
	        DOCUMENT_INDEX.date_received is the end date)
	- Pass the argument variable as_company_code with the value "WHSCC" with type string
	- Return the number of working days with a type of decimal
	
	- Calculate the number of working days using the following query:
					SELECT COUNT(*)
					FROM company_Calendar
					WHERE parent_company_code = :as_company_code
					AND   calendar_date >= :adt_start_date 
					AND   calendar_date < dateadd(day,1,adt_end_date)
					AND   working_days > 0 
					
		NOTE: The calculation for working days is to count the number 
		      of WHSCC working days from the treatment time to the date the 
				document was received where the day of treatment is considered day zero. "
				
				
		ARGS: adt_start_date
		      adt_end_date
				as_company_code
				
				
		RETURNS: ldec_working_days
		
		NOTE **** this function is being used in u_document_index also ****

*/

DECIMAL			ldec_working_days		


SELECT COUNT(*)
INTO :ldec_working_days
FROM Company_Calendar
WHERE parent_company_code = :as_company_code
AND   calendar_date      > :adt_start_date 
AND   calendar_date      < dateadd(day,1,:adt_end_date)
AND   working_days       > 0 ;

//do sql check
IF SQLCA.nf_handle_error("SELECT COUNT(*)INTO :ldec_working_days","n_account_payment_controller",+&
"nf_calculate_working_days") < 0 THEN
	RETURN -1
END IF

/* if we pass a -1 then we fail the function from the calling script
*/
IF isnull(ldec_working_days) THEN
	RETURN -1
END IF

RETURN ldec_working_days
end function

public function integer nf_retrieve (long al_payment_no);LONG					ll_rowcount, ll_row, ll_rows, ll_return, ll_pymt_no, ll_txn_no, ll_tr_pymt_no // tr = tax recovery
LONG					ll_related_txn_no
STRING				ls_paid_status_code, ls_payment_type_code, ls_hst, ls_return
DATAWINDOWCHILD	ldwc_child
STRING				ls_module_source_code, ls_payment_sub_type_filter
STRING				ls_filter, ls_b_no, ls_t_no, ls_a_no

/*	Check to see if we have a payment document record and if so, retrieve it.
*/
	IF istr_message.al_doubleparm[3] > 0 THEN
		ll_rowcount = idw_dw[6].Retrieve(istr_message.al_doubleparm[3] ,al_payment_no) 				// dw_payment_document
		IF SQLCA.nf_handle_error("dw_payment_document","n_account_payment_controller","nf_retrieve") < 0 THEN
			Return -1
		END IF
		IF ll_rowcount <= 0 THEN
			MessageBox("Account Payment Maintenance - Data Integrity Error","Record not found retrieving from payment_document table for payment # " + &
							String(al_payment_no) + " / document # " + string(istr_message.al_doubleparm[3]) + "~r~nAnother user may have deleted the payment.",Exclamation!)
			Return -1
		END IF

		ls_paid_status_code =  idw_dw[6].GetItemString(1,"paid_status_code")
		idw_dw[6].GetChild('paid_status_code',ldwc_child)
		ldwc_child.SetFilter("paid_status_code = 'O' OR paid_status_code = 'H' OR paid_status_code = 'R'")
		ldwc_child.Filter()
		IF ls_paid_status_code = "O" OR ls_paid_status_code = "R" OR ls_paid_status_code = "H" THEN
			idw_dw[6].GetChild('paid_status_explanation_code',ldwc_child)
			ldwc_child.SetFilter("paid_status_code = '" + ls_paid_status_code + "'")
			ldwc_child.Filter()
			idw_dw[1].Visible = False
			idw_dw[2].Visible = False
			idw_dw[6].Visible = True
			Return 0
		END IF
	END IF

/*	There must be a payment record, so retrieve it
*/
	ll_rowcount = idw_dw[1].Retrieve(al_payment_no)
	IF SQLCA.nf_handle_error("idw_dw[1]","n_account_payment_controller","nf_retrieve") < 0 THEN
		Return -1
	END IF

	IF ll_rowcount <= 0 THEN
		MessageBox("Account Payment Maintenance - Data Integrity Error","Record not found retrieving from PAYMENT table for payment #" + &
						String(al_payment_no) + "~r~nPlease call the help desk",Exclamation!)
		Return -1
	END IF

	ll_rowcount = idw_dw[2].Retrieve(al_payment_no)
	IF SQLCA.nf_handle_error("idw_dw[2]","n_account_payment_controller","nf_retrieve") < 0 THEN
		Return -1
	END IF
	IF ll_rowcount <= 0 THEN
		MessageBox("Account Payment Maintenance - Data Integrity Error","Record not found retrieving from TRANSACTION table for payment #" + &
						String(al_payment_no) + "~r~nPlease call the help desk",Exclamation!)
		Return -1
	END IF

//PR11551- Display bank info on scheduled payments
	IF idw_dw[2].GetItemString(idw_dw[2].GetRow(), 'source_table_code') = 'U' THEN
		IF idw_dw[2].GetItemString(idw_dw[2].GetRow(),'payment_method_code') = 'D' THEN
			ll_txn_no = idw_dw[2].GetItemNumber(idw_dw[2].GetRow(),'txn_no')
			inv_payment.nf_get_bank(idw_dw[2].GetItemNumber(idw_dw[2].GetRow(),'recipient_no'),idw_dw[2].GetItemString(idw_dw[2].GetRow(),'recipient_type_code'),ls_b_no, ls_t_no, ls_a_no, al_payment_no,ll_txn_no)
			idw_dw[2].SetItem(idw_dw[2].GetRow(),'bank_no', ls_b_no)
			idw_dw[2].SetItem(idw_dw[2].GetRow(),'bank_transit_no', ls_t_no)
			idw_dw[2].SetItem(idw_dw[2].GetRow(),'bank_account_no', ls_a_no)
		END IF
	END IF

	ll_rowcount = idw_dw[5].Retrieve(al_payment_no)			// dw_invoice
	IF SQLCA.nf_handle_error("idw_dw[5]","n_account_payment_controller","nf_retrieve") < 0 THEN
		Return -1
	END IF

	IF ll_rowcount = 0 THEN
		idw_dw[5].InsertRow(0)
	END IF

/* Check to see if this payment is processed, or in the process of being processed
*/
	IF IsNull(idw_dw[1].GetItemDateTime(1,"processed_date")) THEN
		IF idw_dw[2].GetItemNumber(1,"batch_no") > 0 THEN
			idw_dw[1].Enabled = False
			idw_dw[2].Enabled = False
			idw_dw[5].Enabled = False
		END IF
	ELSE
		idw_dw[1].Enabled = False
		idw_dw[2].Enabled = False
	END IF
	
	
	//Get the module source code
	SELECT module_source_code
	INTO :ls_module_source_code
	FROM DOC a,
	     CLAIM..PAYMENT_DOCUMENT b
	WHERE a.docid = b.doc_id
	and   b.payment_no = :al_payment_no
	USING ImageTrans;
	
	ImageTrans.nf_handle_error("w_account_payment", "", "cb_remove - SELECT module_source_code") 
	
	
	//If the document source is Medical e-invoice, limit the payment types to the following
	IF ls_module_source_code = '12' Then
		//Add to the filter
		ls_filter += "payment_type_code in('41','24','25','21')"
	Else
		ls_filter = "authorization_type_code = '" + is_authorization_type + "' AND opening_type_code = 'I' and award_type_code = 'I'"
	End if

	
	idw_dw[1].GetChild('payment_type_code', ldwc_child)
	ldwc_child.SetFilter(ls_filter)
	ldwc_child.Filter()

	ls_payment_type_code = idw_dw[1].GetItemString(1,'payment_type_code')
	
	// there is no sub type for payment types 41, 24, 25 for medical e-invoice
	IF ls_payment_type_code <> '21' AND ls_payment_type_code <> '25' THEN
		ls_payment_sub_type_filter = 'payment_type_code = "'+ ls_payment_type_code + '"'
	ELSE
		IF ls_module_source_code = '12' THEN
			IF ls_payment_type_code = '21' THEN
				// there is only one sub type for payment type 21 for medical e-invoice
				ls_payment_sub_type_filter = 'payment_sub_type_code = "12"'
			ELSEIF ls_payment_type_code = '25' THEN
				// there is only one sub type for payment type 25 for medical e-invoice - "blank"
				ls_payment_sub_type_filter = 'payment_sub_type_code = ""'
			END IF
		ELSE
			ls_payment_sub_type_filter = 'payment_type_code = "'+ ls_payment_type_code + '"'
		END IF
	END IF
	idw_dw[1].GetChild('payment_sub_type_code',ldwc_child)
	ldwc_child.SetFilter(ls_payment_sub_type_filter)
	ldwc_child.Filter()

	ll_rows = idw_dw[2].RowCount()
	FOR ll_row = 1 to ll_rows
		inv_payment.nf_tab_order('use_default_address_flag', idw_dw[2].GetItemString(ll_row,'use_default_address_flag'))
		IF idw_dw[2].GetItemString(ll_row,'use_default_address_flag') = 'Y' THEN
			inv_payment.nf_setup_address(idw_dw[2].GetItemString(ll_row, 'recipient_type_code'), idw_dw[2].GetItemNumber(ll_row, 'recipient_no'), ll_row,"")
		END IF
	NEXT

/*	set the scheduled processing date to the correct value
*/
	idw_dw[1].SetItem(1,'scheduled_processing_date',idw_dw[2].GetItemDateTime(1,'scheduled_processing_date'))

	ll_related_txn_no = idw_dw[2].GetItemNumber(1,'txn_no')
	ll_rowcount = idw_dw[3].Retrieve(ll_related_txn_no)  // payment record 
	IF SQLCA.nf_handle_error("idw_dw[3]","n_account_payment_controller","nf_retrieve") < 0 THEN
		Return -1
	END IF
	
	IF ll_rowcount > 0 THEN
		ll_rowcount = idw_dw[4].Retrieve(ll_related_txn_no)  // unapplied txn record 
		IF SQLCA.nf_handle_error("idw_dw[4]","n_account_payment_controller","nf_retrieve") < 0 THEN
			Return -1
		END IF
		IF ll_rowcount = 0 THEN
			MessageBox('Data Integrity Error','There is a record in PAYMENT for an Early Filing Bonus, but there is no corresponding record in APPLIED_CLAIM_TXN. Please contact the HELPDESK.',Exclamation!)
			RETURN -1
		END IF
		
		ll_rowcount = idw_dw[12].Retrieve(ll_related_txn_no)  // payment document record 
		IF SQLCA.nf_handle_error("idw_dw[12]","n_account_payment_controller","nf_retrieve") < 0 THEN
			Return -1
		END IF
		IF ll_rowcount = 0 THEN
			MessageBox('Data Integrity Error','There are records in PAYMENT and APPLIED_CLAIM_TXN for an Early Filing Bonus, but there is no corresponding record in PAYMENT_DOCUMENT. Please contact the HELPDESK.',Exclamation!)
			RETURN -1
		END IF
	END IF

Return 0

end function

public function integer nf_protect_columns (string as_module_source_code);
CHOOSE CASE as_module_source_code
	CASE "12"
		//Transaction
		idw_dw[2].object.recipient_type_code.protect = 1
		idw_dw[2].object.recipient_no.protect = 1
		idw_dw[2].object.payment_method_code.protect = 1
		idw_dw[2].object.cheque_no.protect = 1
		idw_dw[2].object.use_default_address_flag.protect = 1
		idw_dw[2].object.cheque_print_group_code.protect = 1
		idw_dw[2].object.address_line1.protect = 1
		idw_dw[2].object.address_line2.protect = 1
		idw_dw[2].object.manual_cheque_req_no.protect = 1
		idw_dw[2].object.city.protect = 1
		idw_dw[2].object.prov_state_code.protect = 1
		idw_dw[2].object.postal_code.protect = 1
		idw_dw[2].object.country.protect = 1
		
		//Invoice
		idw_dw[5].object.invoice_no.protect = 1
		
		//Payment
		idw_dw[1].object.scheduled_processing_date.protect = 1
		idw_dw[1].object.submitted_amount.protect = 1
		idw_dw[1].object.authorization_no.protect = 1
		
		
	CASE ELSE

		//Transaction
		idw_dw[2].object.recipient_type_code.protect = 1
		idw_dw[2].object.recipient_no.protect = 1
		idw_dw[2].object.payment_method_code.protect = 1
		idw_dw[2].object.explanation.protect = 1
		idw_dw[2].object.cheque_no.protect = 1
		idw_dw[2].object.use_default_address_flag.protect = 1
		idw_dw[2].object.cheque_print_group_code.protect = 1
		idw_dw[2].object.address_line1.protect = 1
		idw_dw[2].object.address_line2.protect = 1
		idw_dw[2].object.manual_cheque_req_no.protect = 1
		idw_dw[2].object.city.protect = 1
		idw_dw[2].object.prov_state_code.protect = 1
		idw_dw[2].object.postal_code.protect = 1
		idw_dw[2].object.country.protect = 1
		
		//Invoice
		idw_dw[5].object.invoice_no.protect = 1
		
		//Payment
		idw_dw[1].object.paid_quantity.protect = 1
		idw_dw[1].object.paid_from_date.protect = 1
		idw_dw[1].object.paid_to_date.protect = 1
		idw_dw[1].object.scheduled_processing_date.protect = 1
		idw_dw[1].object.payment_type_code.protect = 1
		idw_dw[1].object.payment_sub_type_code.protect = 1
		idw_dw[1].object.submitted_amount.protect = 1
		idw_dw[1].object.authorization_no.protect = 1

END CHOOSE

return 1
end function

public function integer nf_create_early_filing_bonus (ref string as_message);/* This function will be called if all of the creiteria is met to create a
   Early filing Bonus Payment - This is for Project number 10127 Medical Society
	Agreement.
   In order to create a Automated early filing bonus the following must be met
	
	1. The Payment_type = 21 (account payment) - assumed
	2. The payment_sub_type = blank (set this ?)
	3. The document being paid is an SDD document
	   (DOCUMENT_INDEX.type_code = SDD)
	4. The Physician is a valid New Brunswick Physician
	5. The Physician has not been paid an early filing bonus for this claim
	6. The document has not been paid
	7. The treatment date is less than the first acceptance date of the claim
	8. The document is filed within three working days of the treatment
	   (DOCUMENT_INDEX.date_on_document is the date of treatment and DOCUMENT_INDEX
		.date_received is the date that WHSCC received the document)
	9. If all of the above are met then create a PAYMENT and UNAPPLIED_CLAIM_TXN record
	   
		
	A manual early filing bonus payment is created if the following is met
	1. the payment type is 21 (account payment) - assumed
	2. the payment_sub_type = 02 (NBMS Early filing bonus manual)
	3. The payment clerk has authorization to create a manual early filing bonus
	   payments (User_Profile.maintain_nbms_payments_flag = "Y")
	4. the document being paid is an SDD document
	   (DOCUMENT_INDEX.TYPE_CODE = "SDD") OR AN MPD DOCUMENT
		(DOCUMENT_INDEX.TYPE_CODE = "MPD")
	5. The physician is a valid New Brunswick physician
	6. The physician has not been paid an early filing bonus for this claim
	7. The document has not been paid
	8. The treatment date is less than the first acceptance date of the claim
	9. The document is filed within five working days of treatment
	   DOCUMENT_INDEX.date_on_document is the date of the treatment and
		DOCUMENT_INDEX.date_received is the date that WHSCC received the document)
	10. If all of the above are met then create a PAYMENT and UNAPPLIED_CLAIM_TXN record
	
*/

/* These are the datawindows we are using to do our inserts
idw_dw[9]  = d_account_payment - dw_account_payment_2
idw_dw[10] = d_claim_txn       - dw_account_transaction_2
*/

/* This function will return the following
-1 Fail everything on the other side with a messagebox
-2 Fail everything on the otherside without a messagebox
-3 Do not fail the current transaction cancel this transaction and DO NOT display 
   a messagebox
 1 Everything passes validation
*/

LONG			ll_provider_no,ll_claim_no,ll_doc_id
INTEGER		li_return,li_message_return
STRING		ls_recipient_type_code,ls_sub_type,ls_check,ls_payment_type,ls_doc_type_code
BOOLEAN		lb_check
DECIMAL		ldc_working_days,ldc_amount
DATETIME		ldt_first_acceptance_date,ldt_start_date,ldt_end_date,ldt_accident_date
DATAWINDOWCHILD	ldwc_payment_sub_type
	
/* check to see if the physician is a valid New Brunswick physician
*/
ll_provider_no         = idw_dw[2].GetItemNumber(1,'recipient_no')
ls_recipient_type_code = idw_dw[2].GetItemString(1,'recipient_type_code')
ll_claim_no            = istr_message.al_doubleparm[2]
ls_sub_type            = idw_dw[2].GetItemString(1,'recipient_sub_type_code')
ll_doc_id              = istr_message.al_doubleparm[3]
ls_payment_type        = idw_dw[1].getitemstring(1,'payment_sub_type_code')
as_message             = ""

/* If payment type is 01 then automatic which means that we are creating a bonus
   behind the scences in this case do not create validation messages simply fail out
	if on the other hand the type is manual then show the message box validation error
	to the user - if on the other hand it is a database error the error handling should take care of it.
*/

IF ls_payment_type = '10' THEN
	as_message = "NB Chiro manual payments cannot be created for this provider."
	RETURN -1
END IF

IF isnull(ls_payment_type) OR ls_payment_type <> "02" THEN ls_payment_type = "01"

/* do initial value checks
*/
IF isnull(ll_provider_no) OR ISNULL(ll_claim_no) OR ls_sub_type = "" OR ls_recipient_type_code = "" THEN
	as_message = "Null or empty value has been reported"
	RETURN -1
END IF

/* The payment clerk must have authorization to create manual 
   early filing bonus payments
*/
SELECT maintain_nbms_payments_flag 
  INTO :ls_check
  FROM User_Profile
 WHERE user_id = :vgst_user_profile.user_id 
 USING SQLCA;
 
 SQLCA.nf_handle_error("SELECT maintain_nbms_payments_flag","n_account_payment_controller",+&
																					"nf_create_early_filing_bonus") 
	
IF ls_check <> "Y" AND ls_payment_type <> "01" THEN
	as_message = "The sign on account "+ vgst_user_profile.user_id +" does not have Authorization to create early filing Bonus Payments."
	RETURN -1
END IF

/* Now we need to check the document type in relationship to 
*/
 SELECT type_code 
   INTO :ls_doc_type_code 
   FROM DOCUMENT_INDEX
  WHERE claim_no = :ll_claim_no
    AND docid    = :ll_doc_id
  USING SQLCA;
	 
SQLCA.nf_handle_error("SELECT type_code FROM DOCUMENT_INDEX","n_account_payment_controller",+&
																					 "nf_create_early_filing_bonus") 
	
CHOOSE CASE ls_payment_type
	CASE "01"//automatic
		IF ls_doc_type_code <> "SDD" THEN
			as_message = "If Payment Type is Automatic the document type must be 'SDD'."
			RETURN -3
		END IF
		
	CASE "02"//manual
		IF ls_doc_type_code <> "SDD" AND ls_doc_type_code <> "MPD" THEN
			as_message = "This document type is not eligible for Early Filing Bonus payments."
			RETURN -1
		END IF
		
END CHOOSE


/* Check that the physician is a valid New Brunswick Physician
*/
li_return = nf_check_valid_physician(ll_provider_no,lb_check,TRUE)
IF li_return = -1 OR lb_check = FALSE THEN
	CHOOSE CASE ls_payment_type
		CASE "01"//automatic
				RETURN -3
		CASE "02"//manual
				as_message = "Physician is not a valid New Brunswick physician and is not eligible to receive the Early Filing Bonus."
				RETURN -1
	END CHOOSE
END IF

/* determine if this provider has received a bonus already for this claim
*/
li_return = nf_check_recieved_early_bonus(ll_provider_no,ls_recipient_type_code,ls_sub_type,ll_claim_no)
IF li_return <> 0 THEN
	CHOOSE CASE ls_payment_type
		CASE "01"//automatic
				RETURN -3
		CASE "02"//manual
				as_message = "Physician has already received an Early Filing Bonus payment for this claim."
				RETURN -1
	END CHOOSE
END IF

/* determine if the document has already been paid
*/
li_return = nf_check_document_paid(ll_doc_id,lb_check)
IF li_return = -1 OR lb_check = TRUE THEN
	CHOOSE CASE ls_payment_type
		CASE "01"//automatic
				RETURN -3
		CASE "02"//manual
				as_message = "Document " + string(ll_doc_id) + " has already been paid."
				RETURN -1
	END CHOOSE
END IF

/* Determination of first acceptance of a claim - a claim is only eligible to receive the early filing
   bonus on the first acceptance of a claim.
*/
li_return = nf_check_claim_acceptance(ll_claim_no,ldt_first_acceptance_date,lb_check)

IF li_return = -1 OR lb_check = FALSE OR isnull(ldt_first_acceptance_date) THEN														
   CHOOSE CASE ls_payment_type
		CASE "01"//automatic
				RETURN -3
		CASE "02"//manual
				as_message = "Claim " + string(ll_claim_no) + " is only eligible to receive the early "+&
															"filing bonus on the first acceptance of the claim."
				RETURN -1
	END CHOOSE
END IF

/* The accident date must be June1,1999 or later
*/
SELECT accident_date 
  INTO :ldt_accident_date
  FROM CLAIM 
 WHERE claim_no = :ll_claim_no 
 USING SQLCA;

SQLCA.nf_handle_error("SELECT COUNT(*) INTO :ll_count FROM CLAIM ","n_account_payment_controller",+&
																"nf_create_early_filing_bonus") 
	

IF date(ldt_accident_date) < date("1999/06/01") THEN
	CHOOSE CASE ls_payment_type
		CASE "01"//automatic
				RETURN -3
		CASE "02"//manual
				as_message = "The accident date for this claim is prior to June 1, 1999."+&
				             "~rOnly claims whose accident date is June 1, 1999 or later is eligible for the Early Filing Bonus payment."
				RETURN -1
	END CHOOSE
END IF

/* Calculation of Working days - The calculation of working days is to count the number of WHSCC 
   working days from the treatment time to the date the document was received where the 
	day of treatment is considered day zero
*/
 SELECT a.date_on_document,a.date_received
   INTO :ldt_start_date,:ldt_end_date
   FROM DOCUMENT_INDEX a
  WHERE a.claim_no = :ll_claim_no
    AND a.docid    = :ll_doc_id
  USING SQLCA;
     
SQLCA.nf_handle_error("SELECT date_on_document,date_received","n_account_payment_controller",+&
"nf_create_early_filing_bonus") 
		

/* The treatment_date (a.date_on_document = ldt_start_date) must be prior to first 
   acceptance_date = ldt_first_acceptance_date which we recieved back via reference in our function
	nf_check_claim_acceptance
*/
IF ldt_start_date >= ldt_first_acceptance_date THEN
	CHOOSE CASE ls_payment_type
		CASE "01"//automatic
				RETURN -3
		CASE "02"//manual
				as_message = "Treatment Date must be prior to Acceptance date"
				RETURN -1
	END CHOOSE	
END IF

ldc_working_days = nf_calculate_working_days(ldt_start_date,ldt_end_date,"WHSCC")
IF li_return = -1 THEN
	CHOOSE CASE ls_payment_type
		CASE "01"//automatic
				RETURN -3
		CASE "02"//manual
				as_message = "Validation failed on calculation of working days."
				RETURN -1
	END CHOOSE	
END IF

/* If the document is an "MPD" document or the document was received 4 or 5 working
   day's after treatment, warning/confirmation messages are required
*/
IF ls_doc_type_code = "MPD" THEN
	li_message_return = messagebox("MPD Document Validation Required","This is an 'MPD' document." +&
											 " Do you want to proceed with issuing the Early Filing Bonus?",question!,yesno!,1 )
	IF li_message_return <> 1 THEN
		RETURN -2
	END IF
END IF

IF (ldc_working_days = 4 OR ldc_working_days = 5) AND ls_payment_type = "02" THEN

		li_message_return = messagebox("MPD Document Validation Required","This document was received " + string(ldc_working_days)  +&
												 " working days after treatment. Do you wish to proceed with the bonus payment?",question!,yesno!,1)
		IF li_message_return <> 1 THEN
			RETURN -2
		END IF
ELSEIF ldc_working_days > 3 AND ls_payment_type = "01" THEN
		RETURN -3
ELSEIF ldc_working_days > 5 THEN
	as_message = "There are " + string(ldc_working_days) +" working days from the treatment date until WorkSafeNB received the document." +&
	             "~rThe maximum number of working days allowed for the Early Filing Bonus payment is 5."
	RETURN -1
END IF


/* Determine the amount of the early filing bonus payment
*/



ldc_amount = nf_calculate_bonus_payment(ldt_start_date,ls_recipient_type_code,ls_sub_type)
IF ldc_amount < 0 THEN
	CHOOSE CASE ls_payment_type
		CASE "01"//automatic
				RETURN -3
		CASE "02"//manual
				as_message = "Validation failed on calculation of bonus payment."
				RETURN -1
	END CHOOSE	
END IF

/* If every thing goes alright above then we can start to populate our new records
   depending on manual or automatic
*/
IF ls_payment_type = "02" THEN//we are creating one transaction
	idw_dw[1].setitem(1,'total_award_amount',ldc_amount)
	idw_dw[1].setitem(1,'submitted_amount',0)
	idw_dw[2].setitem(1,'txn_amount',ldc_amount)
	idw_dw[9].reset()
	idw_dw[10].reset()
	idw_dw[11].reset()
ELSE//it's for automatic so lets set the amounts
	idw_dw[9].setitem(1,'total_award_amount',ldc_amount)
	idw_dw[9].setitem(1,'submitted_amount',0)
	idw_dw[10].setitem(1,'txn_amount',ldc_amount)
END IF

RETURN 1
end function

public function integer nf_create_new_early_filing_bonus (ref string as_message, string as_mode);/* P10151-32 - J.Hawker, 2005-04-13
   This function will be called if all of the criteria is met to create an
   Early filing Bonus Payment when date_on_document is greater than May 31, 2005.
	
	- '01' Automatic EFB
	- '02' Manual EFB

	This function will return the following
	-1 Fail everything on the other side with a messagebox
	-2 Fail everything on the otherside without a messagebox
	-3 Do not fail the current transaction, only cancel this transaction(EFB) and DO NOT display 
   	a messagebox
 	1 Everything passes validation
*/

LONG			ll_provider_no,ll_claim_no,ll_doc_id
LONG        ll_payment_no = 0
INTEGER		li_return, li_message_return, li_count
STRING		ls_recipient_type_code,ls_sub_type,ls_payment_type,ls_doc_type_code, ls_eligible, ls_check
BOOLEAN		lb_check, lb_modify
DECIMAL     ldec_working_days,ldec_amount
DATETIME		ldtm_end_date
DATAWINDOWCHILD	ldwc_payment_sub_type
	
ll_provider_no         = idw_dw[2].GetItemNumber(1,'recipient_no')
ls_recipient_type_code = idw_dw[2].GetItemString(1,'recipient_type_code')
ll_claim_no            = istr_message.al_doubleparm[2]
ls_sub_type            = idw_dw[2].GetItemString(1,'recipient_sub_type_code')
ll_doc_id              = istr_message.al_doubleparm[3]
ls_payment_type        = idw_dw[1].getitemstring(1,'payment_sub_type_code')
as_message             = ""

idec_total_amount      = idw_dw[1].GetItemDecimal(1,"total_award_amount")

/* If payment type is 01 then automatic which means that we are creating a bonus
   behind the scences in this case do not create validation messages simply fail out
	if on the other hand the type is manual then show the message box validation error
	to the user - if on the other hand it is a database error the error handling should take care of it.
*/

IF ls_payment_type = '10' THEN
	as_message = "NB Chiro manual payments cannot be created for this provider."
	RETURN -1
END IF

IF isnull(ll_provider_no) OR ISNULL(ll_claim_no) OR ls_sub_type = "" OR ls_recipient_type_code = "" THEN
	as_message = "Null or empty value has been reported"
	RETURN -1
END IF

/* The payment clerk must have authorization to create manual 
   early filing bonus payments
*/
SELECT maintain_nbms_payments_flag 
INTO   :ls_check
FROM   User_Profile
WHERE  user_id = :vgst_user_profile.user_id 
USING  SQLCA;
 
SQLCA.nf_handle_error("SELECT maintain_nbms_payments_flag","n_account_payment_controller","nf_create_new_efb") 
	
IF ls_check <> "Y" AND ls_payment_type = "02" THEN
	as_message = "The sign on account "+ vgst_user_profile.user_id +" does not have Authorization to create a Manual Early filing Bonus Payments."
	RETURN -1
END IF

IF as_mode = 'modify' THEN 
	ll_payment_no = idw_dw[1].GetItemNumber(idw_dw[1].GetRow(),'payment_no')
	lb_modify = TRUE
	GOTO lbl_modify
END IF

IF isnull(ls_payment_type) OR ls_payment_type <> "02" THEN ls_payment_type = "01"


/* Check the document type 
*/
SELECT type_code 
INTO   :ls_doc_type_code 
FROM  DOCUMENT_INDEX
WHERE  claim_no = :ll_claim_no
AND    docid    = :ll_doc_id
USING  SQLCA;
	 
SQLCA.nf_handle_error("SELECT type_code FROM DOCUMENT_INDEX","n_account_payment_controller","nf_create_early_filing_bonus") 
	

IF ls_doc_type_code <> "SDD" AND ls_doc_type_code <> "MPD" AND ls_doc_type_code <> "AD" and ls_doc_type_code <> "AO" and ls_doc_type_code <> "ANP" THEN
	IF ls_payment_type = '01' THEN
		as_message = "The document type must be 'SDD','MPD', 'AD', 'AO', or 'ANP' if Payment Type is Automatic."
		RETURN -3
	ELSEIF ls_payment_type = '02' THEN
		as_message = "This document type is not eligible for Early Filing Bonus payments."
		RETURN -1
	END IF
END IF

/* Check that the physician is a valid New Brunswick Physician
*/
li_return = nf_check_valid_physician(ll_provider_no,lb_check,FALSE)
IF li_return = -1 OR lb_check = FALSE THEN
	CHOOSE CASE ls_payment_type
		CASE "01"
			RETURN -3
		CASE "02"
			as_message = "Physician is not a valid New Brunswick physician and is not eligible to receive the Early Filing Bonus."
			RETURN -1
	END CHOOSE
END IF

/* Calculation of Working days - The calculation of working days is to count the number of WHSCC 
   working days from the treatment time to the date the document was received where the 
	day of treatment is considered day zero
*/
SELECT date_received
INTO   :ldtm_end_date
FROM  DOCUMENT_INDEX 
WHERE  claim_no = :ll_claim_no
AND    docid    = :ll_doc_id
USING  SQLCA;
     
SQLCA.nf_handle_error("SELECT date_on_document,date_received","n_account_payment_controller","nf_create_early_filing_bonus") 
		

ldec_working_days = nf_calculate_working_days(idtm_date_on_doc,ldtm_end_date,"WHSCC")
IF li_return = -1 THEN
	CHOOSE CASE ls_payment_type
		CASE "01"
			RETURN -3
		CASE "02"
			as_message = "Validation failed on calculation of working days."
			RETURN -1
	END CHOOSE	
END IF

/* Determine if document was received in time.
*/

IF (ldec_working_days = 6 OR ldec_working_days = 7) AND ls_payment_type = "02" THEN
	li_message_return = MessageBox("Create Bonus Payment","This document was received " + string(ldec_working_days)  +&
											 " working days after treatment. Would you like to continue with the bonus payment?",question!,yesno!,2)
	IF li_message_return = 2 THEN
		RETURN -2
	END IF
ELSEIF ldec_working_days > 5 AND ls_payment_type = "01" THEN
	RETURN -3
ELSEIF ldec_working_days > 7 THEN
	IF ls_doc_type_code = 'AD' THEN
		li_message_return = MessageBox("Create Bonus Payment","This document was received " + string(ldec_working_days)  +&
											 " working days after treatment. Would you like to continue with the bonus payment?",question!,yesno!,2)
		IF li_message_return = 2 THEN
			RETURN -2
		END IF
	ELSE
		as_message = "There are " + string(ldec_working_days) +" working days from the treatment date until WorkSafeNB received the document." +&
		    			 "~rThe maximum number of working days allowed for the Early Filing Bonus payment is 7."
		RETURN -1
	END IF
END IF

lbl_modify:

/* Determine the amount of the early filing bonus payment and set the values
*/

CHOOSE CASE ls_payment_type 
	CASE ''
		IF nf_modify_early_filing_bonus(ls_recipient_type_code,ls_sub_type) < 0 THEN
			MessageBox('Error','A problem was encountered while re-calculating the Early Filing Bonus.' &
						+ '~r~nPlease call Help Desk.',information!)
			RETURN -2
		END IF
		
	CASE '01' 
		li_return = nf_calculate_efb_by_percent(idtm_date_on_doc,ls_recipient_type_code,ls_sub_type,idec_total_amount)
		
		IF li_return < 0 THEN
			RETURN -3
		END IF
		
		idw_dw[9].setitem(1,'total_award_amount',idec_total_amount)
		idw_dw[9].setitem(1,'submitted_amount',0)
		idw_dw[10].setitem(1,'txn_amount',idec_total_amount)	
		
	CASE '02'
		IF IsNull(ll_payment_no) THEN ll_payment_no = 0
		li_return = nf_check_early_filing_bonus(ll_payment_no,ls_recipient_type_code,ls_sub_type)
		IF li_return < 0 THEN
			RETURN -2
		END IF

		IF idec_total_amount > 40.00 THEN
			IF MessageBox('Warning!','The amount entered for the Early Filing Bonus is greater than $40.00.' &
			            + '~r~nWould you like to continue with the bonus payment?',question!,okcancel!,2) = 2 THEN
				RETURN -2
			END IF
		END IF
		
		IF lb_modify = FALSE THEN 
			idw_dw[1].setitem(1,'total_award_amount',idec_total_amount)
			idw_dw[1].setitem(1,'submitted_amount',0)
			idw_dw[2].setitem(1,'txn_amount',idec_total_amount)
			idw_dw[9].reset()
			idw_dw[10].reset()
			idw_dw[11].reset()
		END IF
END CHOOSE


RETURN 1
end function

public function integer nf_delete ();string ls_payment_type,ls_payment_sub_type, ls_doc_type
long	 ll_doc_id, ll_payment_no, ll_related_txn_no,ll_rownum, ll_rowcount, ll_cntr
integer li_msg, li_rtn
boolean lb_delete, lb_del_travel
LONG			ll_deleted_payment_no
LONG			ll_payment_docUment_doc_id
datetime ldtm_today

/* for project #10127 it has been decided that if an automatic bonus has been created  
   and the document that it was created from is the one being deleted then 
	we need to msgbox the the user asking if they want to delete the adjoing document
	if they do then both will be deleted else this whole transaction is cancelled.
*/

ldtm_today = f_server_datetime()

ll_doc_id           = istr_message.al_doubleparm[3]

IF idw_dw[1].RowCount() > 0 THEN
	ls_payment_type     = idw_dw[1].GetItemString(1,"payment_type_code")
	ls_payment_sub_type = idw_dw[1].GetItemString(1,"payment_sub_type_code")
	
	IF ls_payment_type = "21" THEN
		CHOOSE CASE ls_payment_sub_type
			CASE '','12'
				//we may have an adjoining early filing bonus payment
				// PR 3403 - We only want to delete payments related to THIS SCHEDULED payment
				// not payments with a PAYMENT_DOCUMENT.paid_status = 'P'
				
				ll_related_txn_no = idw_dw[2].GetItemNumber(1,"txn_no")
				
				SELECT	a.payment_no
				INTO		:ll_payment_no
				FROM		PAYMENT a , PAYMENT_DOCUMENT b , UNAPPLIED_CLAIM_TXN c
				WHERE	a.payment_no					= b.payment_no
				AND		a.payment_no				= c.payment_no
				AND		a.payment_type_code		= "21"
				AND		a.payment_sub_type_code	in ("01",'09')
				AND		b.paid_status_code		= 'S'
				AND		c.related_txn_no			= :ll_related_txn_no;
				
				SQLCA.nf_handle_error("delete payment document","n_account_payment_controller","nf_delete") 
				 
				IF ll_payment_no <= 0 OR isnull(ll_payment_no) THEN
					//we have had a problem - or no NBMS EFB records returned do not delete
					 lb_delete = FALSE
				ELSE
					//we need to msgbox the user to confirm the delete else cancel the entire operation
					li_msg = messagebox("Confirm Delete","Document ID# " + string(ll_doc_id) + " has an attached Early Filing Bonus."+&
											  "~rThis scheduled bonus will be deleted along with the scheduled Medical Account Payment."+&
											  "~rDo you wish to continue with delete?" ,question!,yesno!,2) 
					
					IF li_msg = 1 THEN
						lb_delete = TRUE
					ELSE
						RETURN -1
					END IF
				END IF
				
				
			CASE '01','09'
				// P10151-47 - kdm, same for NB Chiro AEFB deletions
				/* P10151-40, J. Hawker, 2006.01.23 - This needed to be changed to allow deletion
					of a scheduled NBMS Early Filing Bonus on an AD document without deleting the 
					associated account payment. 
					Getting the document type here and further down checking if it is an AD document
					or not. If so, skip the check for the associated account payment.
				*/
				
				SELECT type_code
				INTO   :ls_doc_type
				FROM  DOCUMENT_INDEX
				WHERE  docid = :ll_doc_id
				USING  SQLCA;
				
				SQLCA.nf_handle_error("SELECT type_code","n_account_payment_controller","nf_delete") 
							
				IF ls_doc_type <> 'AD' THEN
					//we may have an adjoining report fee payment
					// PR 3403 - We only want to delete payments related to THIS SCHEDULED payment
					// not payments with a PAYMENT_DOCUMENT.paid_status = 'P'
					
					ll_related_txn_no = idw_dw[2].GetItemNumber(1,"related_txn_no")
					
					SELECT	a.payment_no
					INTO		:ll_payment_no
					FROM		PAYMENT a , PAYMENT_DOCUMENT b , UNAPPLIED_CLAIM_TXN c
					WHERE	a.payment_no					= b.payment_no
					AND		a.payment_no					= c.payment_no
					AND		a.payment_type_code		= "21"
					AND		a.payment_sub_type_code	= " "
					AND		b.paid_status_code			= 'S'
					AND		c.txn_no				= :ll_related_txn_no;
									 
					SQLCA.nf_handle_error("delete payment document","n_account_payment_controller","nf_delete") 
					 
					IF ll_payment_no <= 0 OR isnull(ll_payment_no) THEN
						//we have had a problem - or no records returned do not delete
						 lb_delete = FALSE
					ELSE
						//we need to msgbox the user to confirm the delete else cancel the entire operation
						li_msg = messagebox("Confirm Delete","Document ID# " + string(ll_doc_id) + " is associated with a Medical Account payment."+&
												  "~rThis scheduled fee will be deleted along with the scheduled Early Filing Bonus."+&
												  "~rDo you wish to continue with delete?" ,question!,yesno!,2) 
						
						IF li_msg = 1 THEN
							lb_delete = TRUE
						ELSE
							RETURN -1
						END IF
					END IF
				END IF
			CASE ELSE
				// ******************************
				// types 02,10 (NBMS & Chiro manual early filing), 03,04,05,06 (NBMS payments) , 
				// 07 (NBMS reversal) , 08 (NBMS reissue)
				// None of these types require that a related txn be deleted.
				//
		END CHOOSE
	ELSEIF ls_payment_type = "23" and  Date(ldtm_today) >= Date(gdtm_ephysio_implementation_date) THEN		
		//Check for a travel expense and remove.
		
		IF IsValid(idw_dw[6])  AND idw_dw[6].RowCount() > 0 THEN
		
				ll_deleted_payment_no = idw_dw[6].GetItemNumber(1,'payment_no')
			
				SELECT	a.payment_no
				INTO		:ll_payment_no
				FROM		IW_TRAVEL_EXPENSE a, UNAPPLIED_CLAIM_TXN c
				WHERE	a.payment_no		= c.payment_no
				AND		a.payment_no		= :ll_deleted_payment_no
				USING SQLCA;
				
				SQLCA.nf_handle_error("n_account_payment_controller","nf_delete","Select From IW_TRAVEL_EXPENSE a, UNAPPLIED_CLAIM_TXN c" ) 
				 
				IF ll_payment_no <= 0 OR isnull(ll_payment_no) THEN
					 lb_del_travel = FALSE
				ELSE
					//we need to msgbox the user to confirm the delete else cancel the entire operation
					li_msg = messagebox("Confirm Delete","Document ID# " + string(ll_doc_id) + " has an attached Travel Expense Log."+&
											  "~rThis travel expense log will be deleted along with the scheduled Travel Expense Payment."+&
											  "~rDo you wish to continue with delete?" ,question!,yesno!,2) 
					
					IF li_msg = 1 THEN
						lb_del_travel = TRUE
					ELSE
						RETURN -1
					END IF
				END IF
			END IF
		
	END IF
END IF


/* PAYMENT_DOCUMENT*/
	IF idw_dw[6].RowCount() > 0 THEN
		ll_payment_docUment_doc_id = idw_dw[6].GetItemNumber(1,'doc_id')
		ll_deleted_payment_no = idw_dw[6].GetItemNumber(1,'payment_no')
		IF ll_deleted_payment_no < 0 Then
			SignalError(-666,'Error getting payment number to delete from PAYMENT_DOCUMENT')
		End if
		
		IF ll_deleted_payment_no = 0 Then
			DELETE 
			FROM PAYMENT_DOCUMENT 
			WHERE payment_no = :ll_deleted_payment_no
			and   doc_id = :ll_payment_docUment_doc_id
			USING SQLCA;
		Else		
			DELETE 
			FROM PAYMENT_DOCUMENT 
			WHERE payment_no = :ll_deleted_payment_no
			USING SQLCA;
		End if 
		
		SQLCA.nf_handle_error("delete payment document","n_account_payment_controller","nf_delete")

		idw_dw[6].Reset()
	END IF

/*	delete authorization if an authorization number
*/
	IF idw_dw[1].RowCount() > 0 THEN
		IF idw_dw[1].GetItemNumber(1,'authorization_no') > 0 THEN
			IF nf_validate_authorization(idw_dw[1].GetItemNumber(1,'authorization_no'),'DELETE') < 0 THEN
				RETURN -1
			END IF
		END IF
	END IF

/*	see if there are empty invoice records to delete
*/
	ll_rowcount = idw_dw[5].RowCount()
	IF ll_rowcount > 0 THEN
		ll_cntr = ll_rowcount
		DO WHILE ll_cntr >= 1
			idw_dw[5].DeleteRow(ll_cntr)
			ll_cntr --
		LOOP
		idw_dw[5].Update()
		IF SQLCA.nf_handle_error("delete from invoice","n_account_payment_controller","nf_delete") < 0 THEN
			 RETURN -1
		END IF
	END IF
		
/*	see if there is a payment/transaction record to delete
	-- PR 3403 - For every txn record associated with the payment about to be deleted,
	insert a row INTO DELETED_CLAIM_TXN
*/
	IF idw_dw[1].RowCount() > 0 THEN
		ll_rowcount = idw_dw[2].RowCount()
		
		ll_cntr = 1
		// remove the txns
		DO WHILE ll_cntr <= ll_rowcount
			idw_dw[2].DeleteRow(0)
			ll_cntr++
		LOOP
		
		IF lb_del_travel = True then
			DELETE IW_TRAVEL_EXPENSE
			WHERE payment_no = :ll_payment_no;
			li_rtn = SQLCA.nf_handle_error("", "n_account_payment_controller", "nf_delete - DELETE IW_TRAVEL_EXPENSE WHERE payment_no = :ll_payment_no ") 
			lb_del_travel = False
		END IF
			
		// Remove the payment
		idw_dw[1].DeleteRow(1)
		idw_dw[1].Update()
		SQLCA.nf_handle_error("delete of payment","n_account_payment_controller","nf_delete") 
		
		idw_dw[2].Update()
		SQLCA.nf_handle_error("delete of account transactions","n_account_payment_controller","nf_delete") 
			
		idw_dw[7].Update()
		SQLCA.nf_handle_error("update of authorization","n_account_payment_controller","nf_delete")
		
		
		// See if we have a related record to delete. This would be either an Early Filing Bonus or a Report Fee or a Travel Expense Log.
	
		IF lb_delete = TRUE THEN
			
			IF lb_del_travel = True then
				DELETE IW_TRAVEL_EXPENSE
				WHERE payment_no = :ll_payment_no;
				li_rtn = SQLCA.nf_handle_error("", "n_account_payment_controller", "nf_delete - DELETE IW_TRAVEL_EXPENSE WHERE payment_no = :ll_payment_no ") 
				lb_del_travel = False
			END IF
			
			
			DELETE PAYMENT_DOCUMENT 
			 WHERE doc_id = :ll_doc_id 
			   AND payment_no = :ll_payment_no ; 

			li_rtn = SQLCA.nf_handle_error("", "n_account_payment_controller", "nf_delete - DELETE PAYMENT_DOCUMENT WHERE doc_id = :ll_doc_id AND payment_no = :ll_payment_no ") 

			DELETE INVOICE 
			 WHERE payment_no = :ll_payment_no ; 

			li_rtn = SQLCA.nf_handle_error("", "n_account_payment_controller", "nf_delete - DELETE INVOICE WHERE payment = :ll_payment_no") 
				
			DELETE PAYMENT
			 WHERE payment_no = :ll_payment_no ; 

			li_rtn = SQLCA.nf_handle_error("", "n_account_payment_controller", "nf_delete - DELETE PAYMENT WHERE payment_no = :ll_payment_no ") 
						
			DELETE UNAPPLIED_CLAIM_TXN
			 WHERE payment_no = :ll_payment_no ; 

			li_rtn = SQLCA.nf_handle_error("", "n_account_payment_controller", "nf_delete - DELETE UNAPPLIED_CLAIM_TXN WHERE payment_no = :ll_payment_no ") 
			
		END IF
	END IF

Return 0

end function

public function integer nf_change_payment_type (long al_datawindow);Long    ll_rownum, ll_provider_no, ll_temp, ll_docid
Integer li_rtn
String  ls_payment_type_code,ls_recipient_type_code, ls_administering_act_code, ls_column_name, ls_module_source_code
Date    ldt_null
Datawindowchild ldwc_child, ldwc_child2 

IF al_datawindow = 1 THEN 
	ls_column_name = idw_dw[1].GetColumnName() 
	IF ls_column_name <> "payment_type_code" THEN 
		// let's defer the change of the transaction info until after some other dwo has focus.
		ls_payment_type_code = idw_dw[1].GetItemString(1, "payment_type_code")
		ls_recipient_type_code = idw_dw[2].GetItemString(1, "recipient_type_code")
		ll_rownum = idw_dw[2].GetRow()

		IF ls_payment_type_code = "07" OR ls_payment_type_code = "08" OR ls_payment_type_code = "09" OR ls_payment_type_code = "10" THEN
			//	Check to ensure that a voc rehab payee is selected
			IF ls_recipient_type_code = "M" THEN
				idw_dw[2].SetItem(ll_rownum,"recipient_type_code", "V")
				SetNull(ldt_null)
				idw_dw[2].SetItem(ll_rownum, "manual_cheque_req_no", 0)
				idw_dw[2].SetItem(ll_rownum, "cheque_no", 0)
				idw_dw[2].SetItem(ll_rownum, "cheque_deposit_date", ldt_null)
				idw_dw[2].SetItem(ll_rownum, "bank_no", "")
				idw_dw[2].SetItem(ll_rownum, "bank_transit_no", "")
				idw_dw[2].SetItem(ll_rownum, "bank_account_no", "")
				idw_dw[2].SetItem(ll_rownum, "address_line1", "")
				idw_dw[2].SetItem(ll_rownum, "address_line2", "")
				idw_dw[2].SetItem(ll_rownum, "city", "")
				idw_dw[2].SetItem(ll_rownum, "prov_state_code", "")
				idw_dw[2].SetItem(ll_rownum, "country", "")
				idw_dw[2].SetItem(ll_rownum, "postal_code", "")

				// See if recipient_no / recipient_type_code is valid
				ll_provider_no = istr_message.al_doubleparm[4]
				IF IsNull(ll_provider_no) = TRUE THEN ll_provider_no = 0

				IF nf_validate_service_provider("V", ll_provider_no) = 0 THEN
					idw_dw[2].SetItem(ll_rownum, "recipient_no", ll_provider_no)
					idw_dw[2].SetItem(ll_rownum, "recipient_name", "")
				ELSE
					idw_dw[2].SetItem(ll_rownum, "recipient_no", 0)
					idw_dw[2].SetItem(ll_rownum, "recipient_name", "")
				END IF
			END IF
			// do not allow cash payments for these types 
			idw_dw[2].GetChild('payment_method_code', ldwc_child)
			ldwc_child.SetFilter('payment_method_code <> "C"')
			ldwc_child.Filter()
		ELSE
			// Check to ensure that a medical aid payee is selected	
			idw_dw[2].GetChild('payment_method_code', ldwc_child)
			ldwc_child.SetFilter('')
			ldwc_child.Filter()

			IF ls_recipient_type_code = "V" THEN
				idw_dw[2].SetItem(ll_rownum, "recipient_type_code", "M")

				// See if recipient_no / recipient_type_code is valid
				ll_provider_no = istr_message.al_doubleparm[4]
				IF IsNull(ll_provider_no) = TRUE THEN ll_provider_no = 0

				IF nf_validate_service_provider("M", ll_provider_no) = 0 THEN
					idw_dw[2].SetItem(ll_rownum, "recipient_no", ll_provider_no)
					idw_dw[2].SetItem(ll_rownum, "recipient_name", "")
				ELSE
					idw_dw[2].SetItem(ll_rownum, "recipient_no", 0)
					idw_dw[2].SetItem(ll_rownum, "recipient_name", "")
				END IF
			END IF
		END IF

		idw_dw[1].GetChild('payment_sub_type_code', ldwc_child)
		ldwc_child.SetFilter('payment_type_code = "' + ls_payment_type_code + '"')
		ldwc_child.Filter()
		idw_dw[1].SetItem(ll_rownum,'payment_sub_type_code', ' ')
		
		
		ll_docid = istr_message.al_doubleparm[3]
		
		
		// CR000212 - radiologist reading fees pay type/sub type 21/12
		ls_module_source_code = nf_get_module_source_code()
		
	
		// for project 10127 we must filter out the subtype of automatic
		IF ls_payment_type_code = "21" THEN
			ldwc_child2.SetFilter('')
			idw_dw[1].GetChild('payment_sub_type_code', ldwc_child2)
			
			// CR000212 - new payment sub type 'Radiologist Reading Fees' (21/12)
			IF ls_module_source_code = '12' THEN
				ldwc_child2.SetFilter( "payment_type_code = '21' and payment_sub_type_code = '12' ")
			ELSE
				
				ls_administering_act_code = istr_message.as_stringparm[3]
	
				IF ls_administering_act_code = 'WCA' THEN
					ldwc_child2.SetFilter( "payment_type_code = '21' and payment_sub_type_code in ('02','10','12') ")
				ELSEIF ls_administering_act_code = 'FCA' THEN
					ldwc_child2.SetFilter( "payment_type_code = '21' and payment_sub_type_code in ('10','11') ")							
				END IF
				
			END IF
			ldwc_child2.Filter()
			
			IF ls_module_source_code = '12' THEN
				// '12' is the only acceptable selection for pay type 21 for medical e-invoices
				idw_dw[1].SetItem(1,'payment_sub_type_code','12')
			END IF
		ELSEIF ls_payment_type_code = "25" THEN
			IF ls_module_source_code = '12' THEN
				ldwc_child2.SetFilter('')
				idw_dw[1].GetChild('payment_sub_type_code', ldwc_child2)
				li_rtn = ldwc_child2.SetFilter( "payment_type_code = '25' and payment_sub_type_code = '' ")
				li_rtn = ldwc_child2.Filter()
				
				// 'blank' is the only acceptable selection for pay type 25 for medical e-invoices
				idw_dw[1].SetItem(1,'payment_sub_type_code','')
			END IF			
		END IF

		RETURN 0 
	END IF 
END IF
end function

public function integer nf_check_valid_chiropractor (long al_provider_no, ref boolean ab_check);
/* Determination of a valid Physician
  - Provider type is "M" Medical Aid (PROVIDER.provider_type_code = "M")
  - The provider sub type = '04'
  - The provider is currently active (PROVIDER.active_flag = "Y")
  - The Physician must be a New Brunswick Chiropractor (PROVIDER.prov_state_code = "NB")

 ARGS: al_provider_no - 
 
 RETURNS: TRUE is valid OR FALSE is NOT valid

*/

STRING      ls_eligible
LONG			ll_count

SELECT COUNT(*), chiro_early_filing_bonus_flag
  INTO  :ll_count, :ls_eligible
  FROM  PROVIDER
 WHERE  provider_type_code = "M"
   AND  active_flag        = "Y"
   AND  prov_state_code    = "NB"
	AND  provider_no        = :al_provider_no
   AND  provider_sub_type_code = '04'
GROUP BY chiro_early_filing_bonus_flag
USING SQLCA;

	//do sql check
	IF SQLCA.nf_handle_error("SELECT COUNT(*)INTO  :ll_count","n_account_payment_controller",+&
		"nf_check_valid_chiropractor") < 0 THEN
		RETURN -1
	END IF
	
	IF ll_count <> 1 OR isnull(ll_count) THEN
		ab_check = FALSE
	ELSE
		IF ls_eligible = 'Y' THEN
			ab_check = TRUE
		ELSE
			ab_check = FALSE
		END IF
	END IF
	
	RETURN 1
  
end function

public function integer nf_create_chiro_early_filing_bonus (ref string as_message);/* This function will be called if all of the creiteria is met to create a Chiro
   Early filing Bonus Payment - This is for Project number 10151-47 NB Chiro Association
	Agreement.
   In order to create a Automated early filing bonus the following must be met
	
	1. The Payment_type = 21 (account payment) - assumed
	2. The payment_sub_type = 09 (Chiro Early filing bonus auto)
	3. The document being paid is an SDC document
	   (DOCUMENT_INDEX.type_code = SDC)
	4. The physician is a valid New Brunswick chiropractor
	5. The chiropractor has not been paid an early filing bonus for this document
	6. The document is filed within five working days of treatment
	   DOCUMENT_INDEX.date_on_document is the date of the treatment and
		DOCUMENT_INDEX.date_received is the date that WHSCC received the document)
	7. If all of the above are met then create a PAYMENT and UNAPPLIED_CLAIM_TXN record
	   
		
	A manual early filing bonus payment is created if the following is met
	1. the payment type is 21 (account payment) - assumed
	2. the payment_sub_type = 10 (Chiro Early filing bonus manual)
	3. The payment clerk has authorization to create a manual early filing bonus
	   payments (User_Profile.maintain_chiro_payments_flag = "Y")
	4. the document being paid is an SDC document
	   (DOCUMENT_INDEX.type_code = SDC)
	5. The physician is a valid New Brunswick chiropractor
	6. The chiropractor has not been paid an early filing bonus for this document
	7. The document is filed within seven working days of treatment
	   DOCUMENT_INDEX.date_on_document is the date of the treatment and
		DOCUMENT_INDEX.date_received is the date that WHSCC received the document)
	8. If all of the above are met then create a PAYMENT and UNAPPLIED_CLAIM_TXN record
	
*/

/* These are the datawindows we are using to do our inserts
idw_dw[9]  = d_account_payment - dw_account_payment_2
idw_dw[10] = d_claim_txn       - dw_account_transaction_2
*/

/* This function will return the following
-1 Fail everything on the other side with a messagebox
-2 Fail everything on the otherside without a messagebox
-3 Do not fail the current transaction cancel this transaction and DO NOT display 
   a messagebox
 1 Everything passes validation
*/

LONG			ll_provider_no,ll_claim_no,ll_doc_id
INTEGER		li_return,li_message_return
STRING		ls_recipient_type_code,ls_sub_type,ls_check,ls_payment_type,ls_doc_type_code
BOOLEAN		lb_check
DECIMAL		ldc_working_days,ldc_amount, ldc_entered_amount
DATETIME		ldt_start_date,ldt_end_date
	
/* check to see if the physician is a valid New Brunswick physician
*/
ll_provider_no         = idw_dw[2].GetItemNumber(1,'recipient_no')
ls_recipient_type_code = idw_dw[2].GetItemString(1,'recipient_type_code')
ll_claim_no            = istr_message.al_doubleparm[2]
ls_sub_type            = idw_dw[2].GetItemString(1,'recipient_sub_type_code')
ll_doc_id              = istr_message.al_doubleparm[3]
ls_payment_type        = idw_dw[1].getitemstring(1,'payment_sub_type_code')
as_message             = ""

/* If payment type is 09 then automatic which means that we are creating a bonus
   behind the scences in this case do not create validation messages simply fail out
	if on the other hand the type is manual then show the message box validation error
	to the user - if on the other hand it is a database error the error handling should take care of it.
*/

IF ls_payment_type = '02' THEN
	as_message = "NBMS manual payments cannot be created for this provider."
	RETURN -1
END IF

IF isnull(ls_payment_type) OR ls_payment_type <> "10" THEN ls_payment_type = "09"

/* do initial value checks
*/
IF isnull(ll_provider_no) OR ISNULL(ll_claim_no) OR ls_sub_type = "" OR ls_recipient_type_code = "" THEN
	as_message = "Null or empty value has been reported"
	RETURN -1
END IF

/* The payment clerk must have authorization to create manual 
   early filing bonus payments
*/
SELECT maintain_chiro_payments_flag 
  INTO :ls_check
  FROM User_Profile
 WHERE user_id = :vgst_user_profile.user_id 
 USING SQLCA;
 
 SQLCA.nf_handle_error("SELECT maintain_chiro_payments_flag","n_account_payment_controller",+&
																					"nf_create_early_filing_bonus") 
	
IF ls_check <> "Y" AND ls_payment_type <> "09" THEN
	as_message = "The sign on account "+ vgst_user_profile.user_id +" does not have Authorization to create early filing Bonus Payments."
	RETURN -1
END IF

/* Now we need to check the document type in relationship to 
*/
 SELECT type_code 
   INTO :ls_doc_type_code 
   FROM DOCUMENT_INDEX
  WHERE claim_no = :ll_claim_no
    AND docid    = :ll_doc_id
  USING SQLCA;
	 
SQLCA.nf_handle_error("SELECT type_code FROM DOCUMENT_INDEX","n_account_payment_controller",+&
																					 "nf_create_early_filing_bonus") 
	
CHOOSE CASE ls_payment_type
	CASE "09"//automatic
		IF ls_doc_type_code <> "SDC" THEN
			as_message = "If Payment Type is Automatic the document type must be 'SDC'."
			RETURN -3
		END IF
		
	CASE "10"//manual
		IF ls_doc_type_code <> "SDC" THEN
			as_message = "This document type is not eligible for Early Filing Bonus payments."
			RETURN -1
		END IF
		
END CHOOSE


/* Check that the physician is a valid New Brunswick Physician
*/
li_return = nf_check_valid_chiropractor(ll_provider_no,lb_check)
IF li_return = -1 OR lb_check = FALSE THEN
	CHOOSE CASE ls_payment_type
		CASE "09"//automatic
				RETURN -3
		CASE "10"//manual
				as_message = "Physician is not a valid New Brunswick physician and is not eligible to receive the Early Filing Bonus."
				RETURN -1
	END CHOOSE
END IF

/* determine if the document has already been paid an EFB
*/
li_return = nf_check_chiro_document_paid(ll_doc_id,lb_check)
IF li_return = -1 OR lb_check = TRUE THEN
	CHOOSE CASE ls_payment_type
		CASE "09"//automatic
				RETURN -3
		CASE "10"//manual
				as_message = "Document " + string(ll_doc_id) + " has already been paid."
				RETURN -1
	END CHOOSE
END IF



/* Calculation of Working days - The calculation of working days is to count the number of WHSCC 
   working days from the treatment time to the date the document was received where the 
	day of treatment is considered day zero
*/
 SELECT a.date_on_document,a.date_received
   INTO :ldt_start_date,:ldt_end_date
   FROM DOCUMENT_INDEX a
  WHERE a.claim_no = :ll_claim_no
    AND a.docid    = :ll_doc_id
  USING SQLCA;
  
  IF date(ldt_start_date) < IDT_CHIRO_START THEN
	CHOOSE CASE ls_payment_type
		CASE "09"//automatic
				RETURN -3
		CASE "10"//manual
				as_message = "The date on the document for this claim is prior to March 1, 2006. Only claims whose accident "+&
                         "~rdate is March 1, 2006 or later are eligible for the NB Chiro Early Filing Bonus payment."
				RETURN -1
	END CHOOSE
END IF
     
SQLCA.nf_handle_error("SELECT date_on_document,date_received","n_account_payment_controller",+&
"nf_create_early_filing_bonus") 

ldc_working_days = nf_calculate_working_days(ldt_start_date,ldt_end_date,"WHSCC")
IF li_return = -1 THEN
	CHOOSE CASE ls_payment_type
		CASE "09"//automatic
				RETURN -3
		CASE "10"//manual
				as_message = "Validation failed on calculation of working days."
				RETURN -1
	END CHOOSE	
END IF

/* If the document was received 6 or 7 working day's after treatment,
   warning/confirmation messages are required
*/

IF (ldc_working_days = 6 OR ldc_working_days = 7) AND ls_payment_type = "10" THEN

		li_message_return = messagebox("SDC Document Validation Required","This document was received " + string(ldc_working_days)  +&
												 " working days after treatment. Do you wish to proceed with the bonus payment?",question!,yesno!,1)
		IF li_message_return <> 1 THEN
			RETURN -2
		END IF
ELSEIF ldc_working_days > 5 AND ls_payment_type = "09" THEN
		RETURN -3
ELSEIF ldc_working_days > 7 THEN
	as_message = "There are " + string(ldc_working_days) +" working days from the treatment date until WorkSafeNB received the document." + &
	             "~rThe maximum number of working days allowed for the Early Filing Bonus payment is 7."
	RETURN -1
END IF


/* Determine the amount of the early filing bonus payment
*/

ldc_amount = nf_calculate_chiro_bonus_payment(ldt_start_date)
IF ldc_amount < 0 THEN
	CHOOSE CASE ls_payment_type
		CASE "09"//automatic
				RETURN -3
		CASE "10"//manual
				as_message = "Validation failed on calculation of bonus payment."
				RETURN -1
	END CHOOSE
ELSE
	IF ls_payment_type = '10' THEN
		ldc_entered_amount = idw_dw[2].GetItemDecimal(1,'txn_amount')
		IF ldc_entered_amount <> ldc_amount THEN
			MessageBox('Incorrect Amount','The amount for the NB Chiro Early Filing Bonus must'&
			        +'~nbe '+String(ldc_amount,"$#,##0.00")+' if the treatment date is '+String(ldt_start_date,'yyyy-mm-dd')+'.' &
					  +'~nThe amount will be reset.',Information!)
		END IF
	END IF
END IF

/* If every thing goes alright above then we can start to populate our new records
   depending on manual or automatic
*/
IF ls_payment_type = "10" THEN//we are creating one transaction
	idw_dw[1].setitem(1,'total_award_amount',ldc_amount)
	idw_dw[1].setitem(1,'submitted_amount',0)
	idw_dw[2].setitem(1,'txn_amount',ldc_amount)
	idw_dw[9].reset()
	idw_dw[10].reset()
	idw_dw[11].reset()
	
ELSE//it's for automatic so lets set the amounts
	idw_dw[9].setitem(1,'total_award_amount',ldc_amount)
	idw_dw[9].setitem(1,'submitted_amount',0)
	idw_dw[10].setitem(1,'txn_amount',ldc_amount)
END IF


RETURN 1
end function

public function integer nf_handle_EFB_return (integer ai_return, string as_message, string as_msgbox_title);/* We can have the following 4 types of errors depending on the outcome
	of the function the outcomes are as follow
	(1) -1 Fail everything and display a messagebox
	(2) -2 Fail everything but do not display a messagebox
	(3) -3 Fail the current bonus payment but not the current payment - no msgbox
	(4) 1 Passes all validation
*/

CHOOSE CASE ai_return
	CASE -1
		idw_dw[9].reset()
		idw_dw[10].reset()
		idw_dw[11].reset()
		messagebox(as_msgbox_title,as_message,StopSign!)
		RETURN -1
	CASE -2
		idw_dw[9].reset()
		idw_dw[10].reset()
		idw_dw[11].reset()
		RETURN -1
	CASE -3
		idw_dw[9].reset()
		idw_dw[10].reset()
		idw_dw[11].reset()
	CASE 1
		//passes all validation
	
	CASE ELSE
		idw_dw[9].reset()
		idw_dw[10].reset()
		idw_dw[11].reset()
END CHOOSE

RETURN 0
end function

public subroutine nf_filter_payment_sub_type (string as_payment_type, decimal adec_amount, decimal adec_submitted);DATAWINDOWCHILD ldwc_child

idw_dw[1].GetChild('payment_sub_type_code',ldwc_child)
IF NOT ISNULL(as_payment_type) THEN
	
	ldwc_child.setredraw(false)
	//Sort the rows based on the prod_id column
	ldwc_child.setsort("payment_type_code a, payment_sub_type_code a")
	ldwc_child.sort()

	ldwc_child.SetFilter('payment_type_code = "' + as_payment_type + '"')
	ldwc_child.Filter()
	
	ldwc_child.setredraw(true)
		
	idw_dw[1].SetItem(idw_dw[1].GetRow(),'payment_sub_type_code', ' ')
	idw_dw[1].SetItem(idw_dw[1].GetRow(),'tax_amount', 0.00)
	idw_dw[2].SetItem(1,'tax_amount',0.00)
	
END IF


end subroutine

public function integer nf_change_item (long al_datawindow);STRING				ls_payment_type_code, ls_recipient_type_code, ls_sub_type_code, ls_payment_method_code
DATE					ldt_null, ld_paid_to, ld_paid_from, ld_server_dt
DATAWINDOWCHILD	ldwc_child
DECIMAL				ldec_bonus_amount, ldec_amount, ldec_submitted,ldc_txn_amount,ldec_tax_rate
DATETIME				ldt_start_date,ldt_paid_from_date,ldt_paid_to_date
INTEGER				li_day_no,li_error, li_count
LONG					ll_txn_amount,ll_recipient_no, ll_payment_no
STRING            ls_tax_flag,ls_active, ls_old_payment_type_code,ls_provider_type_code,ls_provider_sub_type_code
STRING            ls_module_source_code
BOOLEAN           lb_protect = TRUE


ib_msgbox_already = False	// pr 1480 - ensure set to off

	CHOOSE CASE al_datawindow
   	CASE 1
			CHOOSE CASE idw_dw[1].GetColumnName()
				CASE "payment_type_code"
					//ls_payment_type_code = idw_dw[1].GetItemString(1,'payment_type_code')
					ls_payment_type_code = idw_dw[1].GetText()
					ldec_amount          = idw_dw[1].GetItemDecimal(1,'total_award_amount')
					ldec_submitted       = idw_dw[1].GetItemDecimal(1,'submitted_amount')
					nf_filter_payment_sub_type(ls_payment_type_code,ldec_amount,ldec_submitted)
					
					ls_recipient_type_code = idw_dw[2].GetItemString(idw_dw[2].GetRow(),'recipient_type_code')
					ll_txn_amount          = idw_dw[2].GetItemNumber(1,"txn_amount")
					ls_sub_type_code       = idw_dw[1].GetItemString(1,'payment_sub_type_code')
					ll_recipient_no        = idw_dw[2].GetItemnumber(idw_dw[2].GetRow(),'recipient_no')
					
					/* make sure the payment type selected is valid
					   we will leave it in the list for those cases where it was 
						valid and then turned off - but you will not be allowed to select 
						it if it is not active now
					*/
					SELECT active_flag INTO :ls_active FROM Payment_Type 
					 WHERE payment_type_code = :ls_payment_type_code
					 USING SQLCA;
					 
					 SQLCA.nf_handle_error("select active_flag into","n_account_payment_controller","nf_change_item")
					 
					 IF ls_active = "N" THEN
						MessageBox('Invalid Payment Type',"The Payment Type is inactive and cannot be selected", Information!)
						idw_dw[1].object.tax_amount.protect = 0
						idw_dw[1].object.tax_rate.protect = 0
						RETURN -1 
					 END IF 
					
					/*
					Only service provider payments will have a tax amount > o
					Recipient_Type.tax_flag = "Y"
					if this isnt the case reset the tax amount to 0 and protect the column
					else unprotect the column
					*/		
					li_error = inv_tax_functions.nf_check_tax_flag(ls_payment_type_code,ls_sub_type_code,ls_recipient_type_code,ll_recipient_no,lb_protect)
					CHOOSE CASE lb_protect
						CASE FALSE
							idw_dw[1].object.tax_amount.protect = 0
							idw_dw[1].object.tax_rate.protect = 0
						CASE ELSE
							idw_dw[2].SetItem(1,"tax_amount",0.00)
							idw_dw[1].SetItem(1,"tax_amount",0.00)
							idw_dw[1].object.tax_amount.protect = 1
					END CHOOSE
						 
					/* check the tax rate seperately */
					/* check to see if the tax_rate should be there */
					ldt_paid_from_date        = idw_dw[1].GetItemDateTime(1,"paid_from_date")
					ldt_paid_to_date          = idw_dw[1].GetItemDateTime(1,"paid_to_date")
					IF NOT ISNULL(ldt_paid_from_date) AND NOT ISNULL(ldt_paid_to_date) THEN 
		
						/* check that the payment type is okay */
						IF inv_tax_functions.nf_check_payment_type_tax(ls_payment_type_code,ls_sub_type_code) = "Y" THEN
							/* check that there is a valid tax rate */
							ldec_tax_rate = 0
							li_error = inv_tax_functions.nf_get_valid_tax_rate(date(ldt_paid_from_date), date(ldt_paid_to_date),ldec_tax_rate)
								IF li_error < 0 OR ISNULL(ldec_tax_rate)THEN 
									idw_dw[1].setitem(idw_dw[1].GetRow(),"tax_rate",0.00)
								END IF 
								
							/* get the valid tax rates */
							idw_dw[1].SetItem(idw_dw[1].GetRow(),'tax_rate',ldec_tax_rate)
						ELSE
							idw_dw[1].SetItem(idw_dw[1].GetRow(),'tax_rate',0.00)
						END IF 
					END IF
					 
					IF ls_payment_type_code = '21' OR ls_payment_type_code = '25' THEN
						ls_module_source_code = nf_get_module_source_code()
													
						IF ls_module_source_code = '12' THEN
							IF ls_payment_type_code = '21' THEN
								// '12' is the only acceptable selection for pay type 21 for medical e-invoices
								idw_dw[1].SetItem(1,'payment_sub_type_code','12')
							ELSEIF ls_payment_type_code = '25' THEN
								// 'blank' is the only acceptable selection for pay type 25 for medical e-invoices
								idw_dw[1].SetItem(1,'payment_sub_type_code','')
							END IF
						END IF
					END IF
			
										
					/*
					let's defer the change of the transaction info until after some other dwo has focus.
					*/
					
					Return 2

				CASE "total_award_amount"
					idw_dw[2].SetItem(1,"txn_amount",Dec(idw_dw[1].GetText()))
					ldc_txn_amount = idw_dw[1].GetItemdecimal(1,"tax_amount")
					IF isnull(ldc_txn_amount) THEN ldc_txn_amount = 0
				   idw_dw[1].SetItem(1,"non_tax_amount",dec(idw_dw[1].GetText()) - ldc_txn_amount)
					
				CASE "payment_sub_type_code"
					
					/* for project 10127 
					*/
					idw_dw[1].GetChild('payment_sub_type_code',ldwc_child)
					ls_sub_type_code     = idw_dw[1].GetText()
					ls_payment_type_code = idw_dw[1].GetItemString(1,'payment_type_code')
					
					// P10151-260 NBMS - provider type/sub type changes				
					ls_provider_type_code = idw_dw[2].GetItemString(1,'recipient_type_code')
					ls_provider_sub_type_code = idw_dw[2].GetItemString(1,'recipient_sub_type_code')
					
					IF ls_sub_type_code = "02" AND ls_payment_type_code = "21" THEN
						
						idw_dw[2].SetItem(1,"explanation","Bonus/Prime")
						
						IF NOT isnull(istr_message.al_doubleparm[2]) AND istr_message.al_doubleparm[2] > 0 THEN
							SELECT a.date_on_document
   						  INTO :ldt_start_date
                       FROM DOCUMENT_INDEX a
                      WHERE a.claim_no = :istr_message.al_doubleparm[2]
                        AND a.docid    = :istr_message.al_doubleparm[3]
                      USING SQLCA;
							 
							IF String(ldt_start_date,'yyyy-mm-dd') = '1900-01-01' THEN
								messagebox("NBMS Early Filing Bonus Payment","There is no bonus amount where no document date exists. The "&
								                                         + "~npayment will not be saved as an Early Filing Bonus payment.")
								ldwc_child.setitem(1,'payment_sub_type_code'," ")
								idw_dw[2].SetItem(1,"explanation","")
								RETURN -1
							END IF
							
							IF SQLCA.nf_handle_error("SELECT date_on_document,date_received","n_account_payment_controller",+&
							"nf_change_item") < 0 THEN
								RETURN -1
							END IF
							//do what ever checks we can do at this point
							IF DATE(ldt_start_date) < DATE('2005-06-01') THEN
								
								ldec_bonus_amount = nf_calculate_bonus_payment(ldt_start_date,ls_provider_type_code,ls_provider_sub_type_code)
																
								IF NOT isnull(ldec_bonus_amount) AND ldec_bonus_amount > 0 THEN
									idw_dw[1].SetItem(1,"total_award_amount",ldec_bonus_amount)
									idw_dw[1].SetItem(1,"submitted_amount",0)
									idw_dw[2].SetItem(1,"txn_amount",ldec_bonus_amount)
									idw_dw[1].SetItem(1,"non_tax_amount",ldec_bonus_amount)
	
								ELSE
									messagebox("Early Filing Bonus Payment","There is no bonus amount that corresponds to the date " + string(ldt_start_date,"YYYY-MM-DD"))
									ldwc_child.setitem(1,'payment_sub_type_code'," ")
									RETURN -1
								END IF
							ELSE
								idw_dw[1].SetItem(1,"total_award_amount",0)
								idw_dw[1].SetItem(1,"submitted_amount",0)
								idw_dw[2].SetItem(1,"txn_amount",0)
								idw_dw[1].SetItem(1,"non_tax_amount",0)
							END IF
						END IF
					ELSEIF ls_sub_type_code = "10" AND ls_payment_type_code = "21" THEN
						/* changes for Project 10151-47
						*/
						idw_dw[2].SetItem(1,"explanation","Chiro Bonus/Prime")
						
						IF NOT isnull(istr_message.al_doubleparm[2]) AND istr_message.al_doubleparm[2] > 0 THEN
							SELECT a.date_on_document
   						  INTO :ldt_start_date
                       FROM DOCUMENT_INDEX a
                      WHERE a.claim_no = :istr_message.al_doubleparm[2]
                        AND a.docid    = :istr_message.al_doubleparm[3]
                      USING SQLCA;
							 
							IF String(ldt_start_date,'yyyy-mm-dd') = '1900-01-01' THEN
								messagebox("NB Chiro Early Filing Bonus Payment","There is no bonus amount where no document date exists. The "&
                                                                     + "~npayment will not be saved as an Early Filing Bonus payment.")
								ldwc_child.setitem(ldwc_child.GetRow(),'payment_sub_type_code'," ")
								idw_dw[2].SetItem(1,"explanation","")
								RETURN -1
							END IF
		  
							IF SQLCA.nf_handle_error("SELECT date_on_document,date_received","n_account_payment_controller",+&
							"nf_change_item") < 0 THEN
								RETURN -1
							END IF
							//do what ever checks we can do at this point
							IF DATE(ldt_start_date) >= IDT_CHIRO_START THEN
								ldec_bonus_amount = nf_calculate_chiro_bonus_payment(ldt_start_date)
																
								IF NOT isnull(ldec_bonus_amount) AND ldec_bonus_amount > 0 THEN
									idw_dw[1].SetItem(1,"total_award_amount",ldec_bonus_amount)
									idw_dw[1].SetItem(1,"submitted_amount",0)
									idw_dw[2].SetItem(1,"txn_amount",ldec_bonus_amount)
									idw_dw[1].SetItem(1,"non_tax_amount",ldec_bonus_amount)
	
								ELSE
									messagebox("Chiro Early Filing Bonus Payment","There is no bonus amount that corresponds to the date " + string(ldt_start_date,"YYYY-MM-DD"))
									ldwc_child.setitem(1,'payment_sub_type_code'," ")
									RETURN -1
								END IF
							ELSE
								idw_dw[1].SetItem(1,"total_award_amount",0)
								idw_dw[1].SetItem(1,"submitted_amount",0)
								idw_dw[2].SetItem(1,"txn_amount",0)
								idw_dw[1].SetItem(1,"non_tax_amount",0)
								messagebox("Chiro Early Filing Bonus Payment","There is no bonus amount that corresponds to the date " + string(ldt_start_date,"YYYY-MM-DD"))
								ldwc_child.setitem(1,'payment_sub_type_code'," ")
							END IF
						END IF
						
					ELSEIF ls_payment_type_code = "21" AND ls_sub_type_code = '11' THEN
						// P10281 - Firefighters Compensation Act
						// report fee
						idw_dw[2].SetItem(1,"explanation","FCA Report Fee/frais trans")
						
						IF NOT isnull(istr_message.al_doubleparm[2]) AND istr_message.al_doubleparm[2] > 0 THEN
							SELECT a.date_on_document
   						  INTO :ldt_start_date
                       FROM DOCUMENT_INDEX a
                      WHERE a.claim_no = :istr_message.al_doubleparm[2]
                        AND a.docid    = :istr_message.al_doubleparm[3]
                      USING ImageTrans;
							ImageTrans.nf_handle_error("n_account_payment_controller","nf_change_item","SELECT date_on_document,date_received")
							
							IF String(ldt_start_date,'yyyy-mm-dd') = '1900-01-01' THEN
								messagebox("FCA Report Fee Payment","No document date exists. The "&
                                                                     + "~npayment will not be saved as an FCA Report Fee payment.")
								ldwc_child.setitem(ldwc_child.GetRow(),'payment_sub_type_code'," ")
								idw_dw[2].SetItem(1,"explanation","")
								RETURN -1
							END IF
							
							ldec_bonus_amount = nf_calculate_fca_report_fee_payment(ldt_start_date,ls_provider_type_code,ls_provider_sub_type_code)
															
							IF NOT isnull(ldec_bonus_amount) AND ldec_bonus_amount > 0 THEN
								idw_dw[1].SetItem(1,"total_award_amount",ldec_bonus_amount)
								idw_dw[1].SetItem(1,"submitted_amount",0)
								idw_dw[2].SetItem(1,"txn_amount",ldec_bonus_amount)
								idw_dw[1].SetItem(1,"non_tax_amount",ldec_bonus_amount)

							ELSE
								messagebox("FCA Report Fee Payment","A manual FCA report fee cannot be paid on this document.")
								ldwc_child.setitem(1,'payment_sub_type_code'," ")
								RETURN -1
							END IF
						END IF
						
					ELSEIF ls_payment_type_code = "21" AND ls_sub_type_code = '12' THEN
						idw_dw[2].SetItem(1,"explanation","")
						
						
					ELSEIF ls_payment_type_code = "23" AND ls_sub_type_code = '05' THEN
						ls_recipient_type_code = idw_dw[2].GetItemString(idw_dw[2].GetRow(),'recipient_type_code')
						ls_payment_method_code = idw_dw[2].GetItemString(idw_dw[2].GetRow(),'payment_method_code')
						IF ls_recipient_type_code = 'I' AND ls_payment_method_code = 'A' THEN
							ld_server_dt = Date(f_server_datetime())
							li_day_no = DayNumber(ld_server_dt)	
							IF li_day_no <= 6 THEN					// Friday
								idw_dw[1].SetItem(idw_dw[1].GetRow(),'scheduled_processing_date', DateTime(RelativeDate(ld_server_dt, 6 - li_day_no)))	
							ELSE
								idw_dw[1].SetItem(idw_dw[1].GetRow(),'scheduled_processing_date',DateTime(RelativeDate(ld_server_dt, 7 - (li_day_no - 6))))
							END IF
							idw_dw[2].SetItem(idw_dw[2].GetRow(),'cheque_print_group_code','W')
							idw_dw[2].SetItem(idw_dw[2].GetRow(),'use_default_address_flag','N')
							idw_dw[2].SetColumn('cheque_print_group_code')
							inv_payment.nf_change_item_2()
						END IF
					END IF
					/* end of changes for project 10127 
					*/
					
					
					ll_txn_amount = idw_dw[2].GetItemNumber(1,"txn_amount")
					/*
					Only service provider payments will have a tax amount > o
					Recipient_Type.tax_flag = "Y"
					if this isnt the case reset the tax amount to 0 and protect the column
					else unprotect the column
					*/
					  ls_recipient_type_code = idw_dw[2].GetItemString(idw_dw[2].GetRow(),'recipient_type_code')
					  ls_payment_type_code   = idw_dw[1].GetItemString(1,'payment_type_code')
					  ll_recipient_no        = idw_dw[2].GetItemnumber(idw_dw[2].GetRow(),'recipient_no')
					  li_error = inv_tax_functions.nf_check_tax_flag(ls_payment_type_code,ls_sub_type_code,ls_recipient_type_code,ll_recipient_no,lb_protect)
						CHOOSE CASE lb_protect
							CASE FALSE
								idw_dw[1].object.tax_amount.protect = 0	
								idw_dw[1].object.tax_rate.protect = 0
							CASE ELSE
								idw_dw[1].SetItem(1,"tax_amount",0.00)
								idw_dw[2].SetItem(1,'tax_amount',0.00)
								idw_dw[1].object.tax_amount.protect = 1
						
							 END CHOOSE
						 
						 /* check the tax rate seperately */
						 /* check to see if the tax_rate should be there */
						ldt_paid_from_date        = idw_dw[1].GetItemDateTime(1,"paid_from_date")
						ldt_paid_to_date          = idw_dw[1].GetItemDateTime(1,"paid_to_date")
						IF NOT ISNULL(ldt_paid_from_date) AND NOT ISNULL(ldt_paid_to_date) THEN 
		
							/* check that the payment type is okay */
							IF inv_tax_functions.nf_check_payment_type_tax(ls_payment_type_code,ls_sub_type_code) = "Y" THEN
								/* check that there is a valid tax rate */
								ldec_tax_rate = 0
								li_error = inv_tax_functions.nf_get_valid_tax_rate(date(ldt_paid_from_date), date(ldt_paid_to_date),ldec_tax_rate)
									IF li_error < 0 OR ISNULL(ldec_tax_rate)THEN 
										idw_dw[1].setitem(idw_dw[1].GetRow(),"tax_rate",0.00)
									END IF 
								
								/* get the valid tax rates */
								idw_dw[1].SetItem(idw_dw[1].GetRow(),'tax_rate',ldec_tax_rate)
							ELSE
								idw_dw[1].SetItem(idw_dw[1].GetRow(),'tax_rate',0.00)
							END IF 
						END IF 		 
					
				CASE "paid_from_date"	// pr 1480 - added date validation to prevent application error messages
						
					ld_paid_from = Date(Left(idw_dw[1].GetText(),10))
					IF ld_paid_from < Date('1900-01-01') OR ld_paid_from > Date('2079-06-06') THEN
					/*	Under certain circumstances the messagebox occurs twice (once from itemchanged & again from
						check_bus_rules).  Boolean added - is set to true below if messagebox gets triggered.  Will be checked
						in check_bus_rule and messagebox only triggered if messagebox hasn't already occurred.
					*/
						ib_msgbox_already = True
						MessageBox('Invalid Date', "The Period 'from' date is invalid.  Please enter a date " + &
												+ "between 01/01/1900 and 06/06/2079.", Information!)
						idw_dw[1].SetFocus()
						idw_dw[1].SetColumn('paid_from_date')
					   Return -1
					END IF


				CASE "paid_to_date"		// pr 1480 - added date validation to prevent application error messages

					ld_paid_to = Date(Left(idw_dw[1].GetText(),10))
					IF ld_paid_to < Date('1900-01-01') OR ld_paid_to > Date('2079-06-06') THEN			
						ib_msgbox_already = True
						MessageBox('Invalid Date', "The Period 'to' date is invalid.  Please enter a date " + &
											+ "between 01/01/1900 and 06/06/2079.", Information!)
						idw_dw[1].SetFocus()
						idw_dw[1].SetColumn('paid_to_date')
					   Return -1
					END IF
					
				CASE "tax_amount"
						IF dec(idw_dw[1].GetText()) < 0 THEN
							MessageBox('Invalid Tax Amount',"The Tax amount cannot be less than 0", Information!)
						END IF 
						ldc_txn_amount = idw_dw[2].GetItemdecimal(1,"txn_amount")
				   	idw_dw[1].SetItem(1,"non_tax_amount",ldc_txn_amount - dec(idw_dw[1].GetText()))
					   idw_dw[2].SetItem(1,"tax_amount", dec(idw_dw[1].GetText()))
			END CHOOSE

	   CASE 2
			inv_payment.nf_check_recipient_tax_flag(TRUE)
			
			CHOOSE CASE idw_dw[2].GetColumnName()
					
				CASE "recipient_type_code"
			
					ls_recipient_type_code = idw_dw[2].GetText()
					ll_txn_amount          = idw_dw[2].GetItemNumber(1,"txn_amount")
					ls_payment_type_code   = idw_dw[1].GetItemString(1,'payment_type_code')
					ls_sub_type_code       = idw_dw[1].GetItemString(1,'payment_sub_type_code')
					ll_recipient_no        = idw_dw[2].GetItemnumber(idw_dw[2].GetRow(),'recipient_no')
					/*
					Only service provider payments will have a tax amount > o
					Recipient_Type.tax_flag = "Y"
					if this isnt the case reset the tax amount to 0 and protect the column
					else unprotect the column
					*/		
					li_error = inv_tax_functions.nf_check_tax_flag(ls_payment_type_code,ls_sub_type_code,ls_recipient_type_code,ll_recipient_no,lb_protect)
					CHOOSE CASE lb_protect
						CASE FALSE
							idw_dw[1].object.tax_amount.protect = 0
							idw_dw[1].object.tax_rate.protect = 0
						CASE ELSE
							idw_dw[1].SetItem(1,"tax_amount",0.00)
							idw_dw[2].SetItem(1,'tax_amount',0.00)
							idw_dw[1].object.tax_amount.protect = 1
						END CHOOSE
					  	 
			END CHOOSE
			
			Return inv_payment.nf_change_item(2)

		CASE 8
			CHOOSE CASE idw_dw[6].GetColumnName()
				CASE "paid_status_code"
/*					Initialize the paid_status_explanation_code field
*/
					idw_dw[6].SetItem(idw_dw[6].GetRow(),"paid_status_explanation_code","  ")

/*					Filter the drop down on paid_status_explanation_codes so that only explanation codes
					for the current status are displayed
*/
				   idw_dw[6].GetChild('paid_status_explanation_code', ldwc_child)
					ldwc_child.SetFilter("paid_status_code = '" + idw_dw[6].GetText() + "'")
					ldwc_child.Filter()
			END CHOOSE
	END CHOOSE

Return 0
end function

public function integer nf_set_defaults ();Long     ll_no, ll_docid, ll_invoice_no, ll_invoice_row, ll_individual_no, ll_num_times_payment_processing_run_today
Integer  li_rtn				
String   ls_payment_type_code, ls_recipient_type_code, ls_bank_no, ls_bank_transit_no, ls_bank_account_no, ls_service_provider_type_code
Date     ldt_date
Datetime ldt_paid_from_date,ldt_paid_to_date
STRING		ls_module_source_code, ls_administering_act_code
STRING		ls_filter, ls_payment_sub_type_filter
u_ds		lds_med_aid_e_invoice
LONG		ll_rows
STRING	ls_patient_account_no
LONG		ll_patient_bill_no
DECIMAL{2} 	ldec_total_submitted_amount
DECIMAL{2} 	ldec_total_award_amount
STRING		ls_invoice_no
STRING		ls_explanation
n_wrc_invoice lnv_wrc_invoice
DATAWINDOWCHILD ldwc_child, ldwc_child2

lnv_wrc_invoice = CREATE n_wrc_invoice

ll_docid = istr_message.al_doubleparm[3]

SELECT service_provider_type_code 
  INTO :ls_service_provider_type_code 
  FROM DOCUMENT_INDEX 
 WHERE docid = :ll_docid
 USING ImageTrans;

li_rtn = ImageTrans.nf_handle_error("n_account_payment_controller", "", "nf_set_defaults - SELECT service_provider_type_code FROM DOCUMENT_INDEX ") 

IF IsNull(ls_service_provider_type_code) = FALSE AND ls_service_provider_type_code <> '' THEN
	ls_recipient_type_code = ls_service_provider_type_code
ELSE
	ls_recipient_type_code = ''
END IF 


ls_module_source_code = nf_get_module_source_code()

//If the document source is Medical e-invoice, limit the payment types to the following
IF ls_module_source_code = '12' Then
	//Add to the filter
	ls_filter += "payment_type_code in('41','24','25','21')"
	
	/*T020703 - commenting next code line out because subtype code '12' (Radiologist reading fee) can only be only available when the selected payment type code
	 	 is '21' (medical account), and this gets tested for later in the function, (and when switching from one payment type to another in itemchanged)	
	*/
	//// CR000212 - new payment sub type 'Radiologist Reading Fees' (21/12)
	// ls_payment_sub_type_filter = "payment_sub_type_code in ('','12')"
	

Elseif istr_message.as_stringparm[1] = 'AO' OR istr_message.as_stringparm[1] = 'ANP' Then
	ls_filter += "payment_type_code in('21')"
	
ELSEIF istr_message.as_mode = 'advance' THEN
	// T023029 - 2.80	Surgical Materials (payment type ‘46’) must not be paid as an advance. 
	ls_filter += "payment_type_code not in('46')"

Else
	ls_filter = "authorization_type_code = '" + is_authorization_type + "' AND opening_type_code = 'I' and award_type_code = 'I'"

End if

//	NOTE:	if in the future the payment type code is set to 07,08, or 09 by default then the filter to
//       the payment_method must be set to filter out cash as a method of payment Jan 24/97

idw_dw[1].GetChild('payment_type_code', ldwc_child)
ldwc_child.SetFilter(ls_filter)
ldwc_child.Filter()


IF istr_message.as_mode = "add" OR  istr_message.as_mode = 'inbasketadd' THEN
	idw_dw[6].SetItem(1,"doc_id",istr_message.al_doubleparm[3])		// doc id
	idw_dw[6].SetItem(1,"paid_status_code","S")
END IF

idw_dw[1].SetItem(1,"opening_no",0)
idw_dw[1].SetItem(1,"total_award_amount",idw_dw[1].GetItemDecimal(1,"total_award_amount"))
idw_dw[1].SetItem(1,'nmbr_cycles',1)

//	set scheduled processing date - necessary for check mandatory in n_payment
ldt_date = Date(f_server_datetime())
idw_dw[1].SetItem(1,'scheduled_processing_date',ldt_date)
idw_dw[2].SetItem(1,"use_default_address_flag","Y")
idw_dw[2].SetItem(1,"cheque_print_group_code"," ")
inv_payment.nf_tab_order('use_default_address_flag', 'Y')
idw_dw[2].SetItem(1,"payment_method_code","A")
idw_dw[2].SetItem(1,"screen_type","act")

// new defaults for project 10229 new columns for PAYMENT  table
idw_dw[1].SetItem(1,'paid_quantity',0)
idw_dw[1].SetItem(1,'tax_amount',0.00)
//idw_dw[1].SetItem(1,'adjustment_quantity',0)//default 0
idw_dw[1].SetItem(1,'adjustment_tax_amount',0.00)
idw_dw[1].SetItem(1,'adjustment_payment_amount',0)
idw_dw[1].SetItem(1,'payment_adjustment_flag',"N")
idw_dw[1].SetItem(1,'total_deductions',0)
idw_dw[1].SetItem(1,'final_payment_flag',"N")
idw_dw[1].SetItem(1,'award_no',0)
idw_dw[1].SetItem(1,'benefit_calculation_no',0)
idw_dw[1].SetItem(1,'payment_sub_type_code',"")
idw_dw[1].SetItem(1,'loe_explanation',"")
idw_dw[2].SetItem(1,'maintain_allowed_flag',"Y")
idw_dw[2].SetItem(1,'txn_sub_type_code',"")
idw_dw[2].SetItem(1,'tax_amount',0.00)
idw_dw[2].SetItem(1,'txn_unit_of_work_no',0)
idw_dw[2].SetItem(1,'direct_deposit_xmit_no',0)
idw_dw[2].SetItem(1,'related_txn_no',0)
idw_dw[2].SetItem(1,'cheque_no',0)
idw_dw[2].SetItem(1,'coc_period',0)
idw_dw[2].SetItem(1,'explanation',"")

IF istr_message.as_mode = "advance" THEN
	IF ls_recipient_type_code = '' THEN ls_recipient_type_code = "M"
	idw_dw[2].SetItem(1, "recipient_type_code", ls_recipient_type_code)
	
	// IF inv_payment.nf_setup_address('M', istr_message.al_doubleparm[4],0,"") = 0 THEN
	//		idw_dw[2].SetItem(1,"recipient_no",istr_message.al_doubleparm[4])
	//	END IF
ELSE
	CHOOSE CASE istr_message.as_stringparm[1]
		CASE "AH"
			idw_dw[1].SetItem(1,"payment_type_code","41")
		CASE "AD", "SDD", "MPD", "SDC", "AO", "ANP"
			idw_dw[1].SetItem(1,"payment_type_code","21")
		CASE "AT", "AV"
			idw_dw[1].SetItem(1,"payment_type_code","23")
		CASE "AR", "ARX"
			idw_dw[1].SetItem(1,"payment_type_code","22")
	END CHOOSE

	idw_dw[1].SetItem(1,"paid_from_date",istr_message.adtm_datetimeparm[1])
	
	IF NOT (istr_message.as_stringparm[1] = "AT" OR istr_message.as_stringparm[1] = "AV" OR istr_message.as_stringparm[1] = "ARX") THEN //document_type
		IF ls_recipient_type_code = '' THEN ls_recipient_type_code = "M"
		idw_dw[2].SetItem(1,"recipient_type_code", ls_recipient_type_code)
		IF IsNull(istr_message.al_doubleparm[4]) THEN istr_message.al_doubleparm[4] = 0 // sender
		
		IF nf_validate_service_provider("M", istr_message.al_doubleparm[4]) = 0 THEN
			idw_dw[2].SetItem(1,"recipient_no",istr_message.al_doubleparm[4])
		END IF
	END IF
		// set up the address info
	IF istr_message.as_stringparm[1] = "ARX" THEN
		IF ls_recipient_type_code = '' THEN ls_recipient_type_code = "I"

		SELECT individual_no 
		  INTO :ll_individual_no 
		  FROM CLAIM 
		 WHERE claim_no = :istr_message.al_doubleparm[2] ;

		li_rtn = SQLCA.nf_handle_error('n_account_payment_controller', '', 'nf_set_defaults - SELECT individual_no FROM CLAIM')
		
		IF inv_payment.nf_setup_address(ls_recipient_type_code, ll_individual_no, 0, "") = 0 THEN
			idw_dw[2].SetItem(1, "recipient_no", ll_individual_no)
		END IF
	ELSE
		IF ls_recipient_type_code = '' THEN ls_recipient_type_code = "M"
		IF inv_payment.nf_setup_address(ls_recipient_type_code, istr_message.al_doubleparm[4], 0, "") = 0 THEN
			idw_dw[2].SetItem(1, "recipient_no", istr_message.al_doubleparm[4])
		END IF
	END IF
END IF

idw_dw[2].SetItem(1,"txn_type_code","1")
idw_dw[2].SetItem(1,"batch_no",0)
idw_dw[2].SetItem(1,"bank_no"," ")
idw_dw[2].SetItem(1,"bank_transit_no"," ")
idw_dw[2].SetItem(1,"bank_account_no"," ")

// PR 2816 - set scheduled processing date based upon recipient type, not document type
ll_no = DayNumber(ldt_date)
IF ls_recipient_type_code = 'M' or ls_recipient_type_code = 'V' or ls_recipient_type_code = 'O' THEN
	IF ll_no <= 4 THEN  // Sun, Mon, Tue, Wed 
		idw_dw[1].SetItem(1,'scheduled_processing_date', DateTime(RelativeDate(ldt_date, 5 - ll_no)))	
	ELSEIF ll_no = 5 THEN  // Thursday
		SELECT COUNT(*) 
		  INTO :ll_num_times_payment_processing_run_today 
		  FROM PAYMENT_RUN 
		 WHERE create_date >= :ldt_date ; 
		 
		li_rtn = SQLCA.nf_handle_error('n_account_payment_controller', '', 'nf_set_defaults - SELECT COUNT(*) FROM PAYMENT_RUN WHERE create_date > :ldt_date')

		IF ll_num_times_payment_processing_run_today > 0 THEN
			idw_dw[1].SetItem(1,'scheduled_processing_date', DateTime(RelativeDate(ldt_date, 7 - (ll_no - 5))))  // Set it to following Thursday
		ELSE
			idw_dw[1].SetItem(1,'scheduled_processing_date', DateTime(RelativeDate(ldt_date, 5 - ll_no)))  // Set it to Today
		END IF
		 
	ELSE  // Fri, Sat 
		idw_dw[1].SetItem(1,'scheduled_processing_date',DateTime(RelativeDate(ldt_date, 7 - (ll_no - 5))))
	END IF
	
	SELECT bank_no, bank_transit_no, bank_account_no
	INTO       :ls_bank_no,:ls_bank_transit_no, :ls_bank_account_no
	FROM    BANK_INFO
	WHERE recipient_no = :istr_message.al_doubleparm[4]
	AND        recipient_type_code = :ls_recipient_type_code
	USING   SQLCA;
	
	IF SQLCA.nf_handle_error('n_account_payment_controller', '', 'nf_set_defaults - SELECT bank_no, bank_transit_no, bank_account_no FROM BANK_INFO') < 0 THEN
		RETURN -1
	END IF
	
ELSE
	idw_dw[1].SetItem(1,'scheduled_processing_date', DateTime(ldt_date) )
	  
	SELECT I.bank_no, I.bank_transit_no, I.bank_account_no 
	  INTO :ls_bank_no, :ls_bank_transit_no, :ls_bank_account_no 
	  FROM CLAIM C, 
	       INDIVIDUAL I 
	 WHERE C.claim_no = :istr_message.al_doubleparm[2] 
	   AND C.individual_no = I.individual_no ;

	li_rtn = SQLCA.nf_handle_error('n_account_payment_controller', '', 'nf_set_defaults - SELECT I.bank_no, I.bank_transit_no, I.bank_account_no FROM CLAIM C, INDIVIDUAL I')

END IF

IF Len(ls_bank_no) > 0 AND Len(ls_bank_transit_no) > 0 AND Len(ls_bank_account_no) > 0 THEN
	idw_dw[2].SetItem(1, "payment_method_code", "D")
	idw_dw[2].SetItem(1, "bank_no", ls_bank_no)
	idw_dw[2].SetItem(1, "bank_transit_no", ls_bank_transit_no)
	idw_dw[2].SetItem(1, "bank_account_no", ls_bank_account_no)
	idw_dw[2].SetItem(1, "use_default_address_flag", "Y")
	idw_dw[2].SetItem(1, "cheque_print_group_code", " ")
END IF

// the IF TO ELSE added for project P10127 to filter out the payment_sub_type_code of 02  
ls_payment_type_code = idw_dw[1].GetItemString(1,'payment_type_code')
ls_administering_act_code = istr_message.as_stringparm[3]
IF ls_payment_type_code = "21" THEN
	idw_dw[1].GetChild('payment_sub_type_code',ldwc_child2)
	
	// if module source code is 12 (medical e-invoice) then filter for only choice (Radiologist Reading Fees, 21/12)
	IF ls_module_source_code = '12' THEN
		ldwc_child2.SetFilter( "payment_type_code = '21' and payment_sub_type_code = '12'")
	ELSE		
		IF ls_administering_act_code = 'WCA' THEN
			ldwc_child2.SetFilter( "payment_type_code = '21' and ( payment_sub_type_code = '02' or payment_sub_type_code = '10' or payment_sub_type_code = '12'")
		ELSEIF ls_administering_act_code = 'FCA' THEN
			ldwc_child2.SetFilter( "payment_type_code = '21' and ( payment_sub_type_code = '10' or payment_sub_type_code = '11'")
		END IF
	END IF
	ldwc_child2.Filter()
		
ELSE
	IF IsNull(ls_payment_type_code) THEN
		ls_payment_type_code = ' ' 
	END IF
	
	idw_dw[1].GetChild('payment_sub_type_code',ldwc_child)
	
	// CR000212 -  if module source code is 12 (medical e-invoice) then filter for only choice (Radiologist Reading Fees, 21/12),
	// or blank, if payment type code is one of the other possible types 41, 24, 25
	
	// T020703 - R.S. - changing CR000212 - commenting this first part out because subtype code '12' (Radiologist reading fee) can only be available when payment type code is 21 (medical account), 
	// and that is tested for in first part of IF statement just above. Leave the filter setting code line in, to ensure the subtype code dropdown gets filtered out. (P.S. -  'module source code' of 12 means something different - imported medical invoice)
	//IF ls_module_source_code = '12' THEN
		//ls_payment_sub_type_filter = "payment_sub_type_code in ('','12')"
	//ELSE
		ls_payment_sub_type_filter = 'payment_type_code = "'+ ls_payment_type_code + '"'
	//END IF
	li_rtn = ldwc_child.SetFilter(ls_payment_sub_type_filter)
	li_rtn = ldwc_child.Filter()
END IF


//******************************************************************************************
//WRC_INVOICING changes
//******************************************************************************************
ll_docid = istr_message.al_doubleparm[3]
IF ll_docid > 0 THEN 
	ll_invoice_no	= lnv_wrc_invoice.nf_get_invoice_number(ll_docid)
	ll_invoice_row = idw_dw[5].GetRow()
	IF ll_invoice_no > 0 THEN //The document we are paying against is an automatically created wrc_invoice
		li_rtn = idw_dw[5].SetItem(ll_invoice_row, "invoice_no", String(ll_invoice_no))

		// Get the from and to dates from the payment that was automatically created
      SELECT DISTINCT P.paid_from_date, P.paid_to_date 
        INTO :ldt_paid_from_date, :ldt_paid_to_date 
        FROM PAYMENT P, 
             PAYMENT_DOCUMENT PD, 
             WRC_INVOICE_HEADER WIH 
       WHERE WIH.wrc_invoice_no = :ll_invoice_no 
		   AND WIH.docid = PD.doc_id 
         AND PD.payment_no = P.payment_no 
       USING SQLCA ; 

		li_rtn = SQLCA.nf_handle_error('n_account_payment_controller', '', 'nf_set_defaults - SELECT DISTINCT paid_from_date, paid_to_date FROM PAYMENT P, PAYMENT_DOCUMENT PD, WRC_INVOICE_HEADER WIH ')
		
		IF li_rtn = 100 THEN 
			SELECT MIN(period_from_date) 
			  INTO :ldt_paid_from_date 
			  FROM WRC_INVOICE_DETAILS 
			 WHERE wrc_invoice_no = :ll_invoice_no ; 
			
			li_rtn = SQLCA.nf_handle_error('n_account_payment_controller', '', 'nf_set_defaults - SELECT MIN(period_from_date) FROM WRC_INVOICE_DETAILS WHERE invoice_no = :ll_invoice_no')
			
			SELECT MAX(period_to_date) 
			  INTO :ldt_paid_to_date 
			  FROM WRC_INVOICE_DETAILS 
			 WHERE wrc_invoice_no = :ll_invoice_no ; 
			
			li_rtn = SQLCA.nf_handle_error('n_account_payment_controller', '', 'nf_set_defaults - SELECT MAX(period_to_date) FROM WRC_INVOICE_DETAILS WHERE invoice_no = :ll_invoice_no') 

			idw_dw[1].SetItem(1, "paid_from_date", ldt_paid_from_date)
			idw_dw[1].SetItem(1, "paid_to_date", ldt_paid_to_date)
			idw_dw[6].SetItem(1, "paid_status_explanation_code", "16")  // 16 = 'From WRC Invoicing System' 
		ELSE
			idw_dw[1].SetItem(1, "paid_from_date", ldt_paid_from_date)
			idw_dw[1].SetItem(1, "paid_to_date", ldt_paid_to_date)
			idw_dw[6].SetItem(1, "paid_status_explanation_code", "16")  // 16 = 'From WRC Invoicing System' 
		END IF 
	END IF
END IF

//**********************************************************************
//E-invoicing changes
//**********************************************************************
IF ls_module_source_code = '12' Then
	

	lds_med_aid_e_invoice = create u_ds
	lds_med_aid_e_invoice.dataobject = 'd_med_aid_e_invoice'
	lds_med_aid_e_invoice.SetTransObject(SQLCA)
	ll_rows = lds_med_aid_e_invoice.Retrieve(ll_docid)
	SQLCA.nf_handle_error("n_account_payment_controller","nf_set_defaults",'lds_med_aid_e_invoice.Retrieve(ll_docid)')
	
	IF ll_rows <= 0 Then
		SignalError(-666,'Missing Medical Aid E-invoice record for docid ' + String(ll_docid))
	End if
	
		
	//Get some values from the MEDICAL_AID_E_INVOICE table
	ls_patient_account_no 		= lds_med_aid_e_invoice.GetItemString(1,'patient_account_no')
	ll_patient_bill_no			= lds_med_aid_e_invoice.GetItemNumber(1,'patient_bill_no')
	
	ldec_total_submitted_amount = lds_med_aid_e_invoice.GetItemDecimal(1,'total_submitted_amount')
	ldec_total_award_amount 	 = ldec_total_submitted_amount
	
	ldt_paid_from_date 		= lds_med_aid_e_invoice.GetItemDateTime(1,'period_from_date')
	ldt_paid_to_date 			= lds_med_aid_e_invoice.GetItemDateTime(1,'period_to_date')
	//whscc invoice_no 
	ls_invoice_no = ls_patient_account_no + '-' + String(ll_patient_bill_no)
	ls_explanation = ls_invoice_no
	
	
	//Invoice number and submitted must not be changed
	idw_dw[5].SetItem(1,'invoice_no',ls_invoice_no)
	idw_dw[1].SetITem(1,'submitted_amount',ldec_total_submitted_amount)
	
	//Set some defaults
	idw_dw[1].SetItem(1,'total_award_amount',ldec_total_award_amount)
	idw_dw[2].SetItem(1,'explanation',ls_explanation)
	idw_dw[2].SetItem(1,'txn_amount',ldec_total_award_amount)
	
	idw_dw[1].SetItem(1,"paid_from_date",ldt_paid_from_date)
	idw_dw[1].SetItem(1,"paid_to_date",ldt_paid_to_date)
	
	idw_dw[1].SetItem(1,"payment_type_code","41")
	
End if

Return 0
end function

public function integer nf_check_bus_rule ();LONG		ll_count, ll_claim_no,ll_rowcount, ll_recipient_no, ll_pmt, ll_txns, ll_rehab_invoice_no
LONG		ll_payment_no,ll_cntr,ll_recipient_cntr,ll_nmbr_recipients,ll_rownum, ll_max
LONG		ll_result, ll_x,ll_wrc_service_provider_no,ll_docid,ll_wrc_invoice_no, ll_payment, ll_acct, ll_txn, ll_travel_expense_no					
DATE		ld_null,ld_paid_to, ld_paid_from
BOOLEAN	lb_duplicate, lb_check, lb_payment_authorized
INTEGER	li_return,li_counter, li_rows,li_message, li_rtn, li_count, row, li_travel, li_line_no
STRING	ls_ben_class_code, ls_payment_type_code, ls_pay_adj_flag, ls_invoice_no, ls_admin_region_code
STRING	ls_dup_pay_nums, ls_pymt_recpt_type
STRING 	ls_message, ls_payment_type, ls_payment_sub_type, ls_fatality
STRING	ls_old_payment_type_code , ls_old_payment_sub_type_code, ls_recipient_type_code
DATETIME	ldt_paid_from_date, ldt_paid_to_date, ldtm_date_on_doc
DATE              ldt_service_date
DATASTORE	   lds_duplicate_payment_numbers			
n_wrc_invoice	lnv_wrc_invoice
DECIMAL {2}		ld_balance_owing,ldec_total_award_amount
DECIMAL			ld_total_payment_change,ld_total_payment_old	,lc_authorization_limit,ldec_tax_rate		
DECIMAL        ld_quantity_paid,ldc_tax_amount,ldc_max_tax_amount,ldc_submitted_amount
U_DS				lds_difficult_claim	
STRING         ls_required_flag,ls_recipient_type,ls_recipient_sub_type,ls_tax_flag, ls_app_function_desc
STRING         ls_nbms_efb_flag , ls_nb_chiro_efb_flag, ls_administering_act_code
DECIMAL {2}    ldec_old_amount, ldec_amount
SetNull(ld_null)

ldec_total_award_amount   = idw_dw[1].GetItemDecimal(1,"total_award_amount")
ls_pay_adj_flag           = idw_dw[1].getitemstring(1,"payment_adjustment_flag")
ls_payment_type_code      = idw_dw[1].GetItemString(1,"payment_type_code")
ldt_paid_from_date        = idw_dw[1].GetItemDateTime(1,"paid_from_date")
ldt_paid_to_date          = idw_dw[1].GetItemDateTime(1,"paid_to_date")
ll_claim_no               = istr_message.al_doubleparm[2]
ls_payment_type			  = idw_dw[1].GetItemString(1,"payment_type_code")
ls_payment_sub_type       = idw_dw[1].GetItemString(1,"payment_sub_type_code")
ld_quantity_paid          = idw_dw[1].GetItemdecimal(1,"paid_quantity")
ldc_tax_amount            = idw_dw[1].GetItemdecimal(1,"tax_amount")
ls_recipient_type         = idw_dw[2].GetItemstring(1,"recipient_type_code")
ls_recipient_sub_type     = idw_dw[2].GetItemstring(1,"recipient_sub_type_code")
ldc_submitted_amount      = idw_dw[1].GetItemDecimal(1,"submitted_amount")
ldec_tax_rate             = idw_dw[1].GetItemDecimal(1,"tax_rate")

lnv_wrc_invoice = CREATE n_wrc_invoice

//grab the value of the required flag (paid_from and paid_to dates (PAYMENT)
SELECT fromto_dates_flag 
  INTO :ls_required_flag
  FROM Payment_Type
 WHERE payment_type_code = :ls_payment_type
 USING SQLCA;
		 
SQLCA.nf_handle_error("SELECT fromto_dates_required_flag","n_account_payment_controller","nf_check_bus_rule") 
			
ll_docid = istr_message.al_doubleparm[3]
il_docid = ll_docid
If ll_docid > 0 THEN
	ll_wrc_invoice_no = lnv_wrc_invoice.nf_get_invoice_number(ll_docid)
End if
ll_wrc_service_provider_no = lnv_wrc_invoice.nf_get_service_provider_no()
ll_recipient_no = idw_dw[2].GetItemNumber(1,"recipient_no")
If ll_wrc_invoice_no > 0 Then	
	If ll_recipient_no <> ll_wrc_service_provider_no Then
		MessageBox("Invalid Recipient Number","The recipient number will be changed to " + string(ll_wrc_service_provider_no) + " because the recipient number for automated Wrc invoices has changed.")
		idw_dw[2].SetItem(1,'recipient_no',ll_wrc_service_provider_no)		
	end if
	
	ld_balance_owing = lnv_wrc_invoice.nf_get_balance_owing(ll_wrc_invoice_no)
	IF idw_dw[1].GetItemStatus(1,0,Primary!) = NewModified! THEN
		If ldec_total_award_amount > ld_balance_owing THEN
			MessageBox("Invalid Payment Amount", "The outstanding balance on WRC Invoice " + String(ll_wrc_invoice_no) + " is " + String(ld_balance_owing, "$#,##0.00") +&
						  ".  The payment amount cannot be greater than " + String(ld_balance_owing, "$#,##0.00"))
			idw_dw[1].SetColumn('total_award_amount')
			idw_dw[1].SetFocus()
			return -1
		end if
	else
		//If the record is Modified then we need to make sure that, the new value for total payment amount minus the old
		//value for total_award_amount is less than the balance_owing
		ld_total_payment_old = idw_dw[1].GetItemNumber(1,"total_award_amount",Primary!,True)
		ld_total_payment_change = ldec_total_award_amount - ld_total_payment_old
		If ld_total_payment_change > ld_balance_owing THEN
			MessageBox("Invalid Payment Amount","The payment amount cannot be greater than $" + string(ld_balance_owing + ld_total_payment_old,"###.00"))
			idw_dw[1].SetColumn('total_award_amount')
			idw_dw[1].SetFocus()
			return -1
		end if
	END IF
Else
	// Check to see if provider is setup for automatic payments
	SELECT distinct app_function_desc 
	  INTO :ls_app_function_desc 
	  FROM App_Document_Index_Parameter 
	 WHERE service_provider_no = :ll_recipient_no ;

	li_rtn = SQLCA.nf_handle_error('Embedded SQL: select from App_Document_Index_Parameter','n_account_payment_controller','nf_check_bus_rule')
	IF li_rtn = 0 THEN
		MessageBox("Invalid Recipient Number", "The recipient number must NOT be " + string(ll_recipient_no) + ", this recipient is reserved for automated payments for " + ls_app_function_desc + ".")
		idw_dw[2].SetColumn('recipient_no')
		idw_dw[2].SetFocus()
		return -1
	end if
END IF


/* the paid amount (total_award_amount) cannot be negative this was in the expression 
   painter in the datawindow but I have also placed it here because when you 
	remove or place in an alternate column the expression will no longer be valid
*/
IF ldec_total_award_amount < 0 THEN 
	MessageBox('Invalid Paid Amount','The Paid amount must be greater than 0 ')
	RETURN -1
End IF

/* new revised Business Rules for P10229 - modified 
6. Change in BRs for submittted_amount
-	the submitted_amount must > 0 under the following conditions
o	 the user is paying against an invoice  (i.e. an indexed document) and 
o	the payment type is not an NBMS Early Filing Bonus (Auto or Manual)
-	the submitted_amount must be 0 when the payment type is an NBMS Early Filing Bonus (Auto and Manual)
-	the submitted_amount must be >= 0 when the user is paying an advance 
   (i.e. there is no indexed document associated with the payment when the payment 
	is created or the payment is updated)

Note: the submitted_amount is > 0 for  the automatically generated WRC invoices
and cannot be updated by the user in the Account Payment module 

*/
// the submitted_amount must be 0 when the payment type is an NBMS Early Filing Bonus (Auto and Manual)
IF ls_payment_type_code = '21' THEN
	IF ldc_submitted_amount <> 0 THEN 
		CHOOSE CASE ls_payment_sub_type
			CASE '01', '02', '09', '10'
				MessageBox('Invalid Submitted Amount','The submitted amount is not applicable to early filing bonuses')
				idw_dw[1].SetColumn("submitted_amount")
				idw_dw[1].SetFocus()
				RETURN -1
			CASE '11'
				MessageBox('Invalid Submitted Amount','The submitted amount is not applicable to FCA Report Fees')
				idw_dw[1].SetColumn("submitted_amount")
				idw_dw[1].SetFocus()
				RETURN -1
		END CHOOSE
	ELSE
		CHOOSE CASE ls_payment_sub_type
			CASE '', ' ','12'
				MessageBox('Invalid Submitted Amount','The submitted amount must be greater than 0 ' +&
				                                      'when the user is paying against an Invoice')
				idw_dw[1].SetColumn("submitted_amount")
				idw_dw[1].SetFocus()
				RETURN -1
			CASE ELSE
				// OK
		END CHOOSE
	END IF
ELSE
	CHOOSE CASE ll_docid
		CASE IS > 0//NOT AN ADVANCE
			IF ldc_submitted_amount <= 0 THEN 
				MessageBox('Invalid Submitted Amount','The submitted amount must be greater than 0 ' +&
				                                      'when the user is paying against an Invoice')
				idw_dw[1].SetColumn("submitted_amount")
				idw_dw[1].SetFocus()
				RETURN -1
			End IF
			
		CASE ELSE//ADVANCE
			IF ldc_submitted_amount < 0 THEN 
				MessageBox('Invalid Submitted Amount','The submitted amount cannot be less than 0 ' +&
				                                      'when the user is paying an advance')
				idw_dw[1].SetColumn("submitted_amount")
				idw_dw[1].SetFocus()
				RETURN -1
			End IF
	END CHOOSE
END IF 

//'The payment method must be "Inapplicable" for zero payments'
If idw_dw[2].GetItemDecimal(1,'txn_amount') = 0 Then
	If idw_dw[2].GetItemString(1,'payment_method_code') <> 'I' Then
			MessageBox('Invalid payment method','The payment method must be "Inapplicable" for zero payments')
			idw_dw[2].SetColumn('payment_method_code')
			idw_dw[2].SetFocus()
			return -1
	End if
end if 
if idw_dw[2].GetItemString(1,'payment_method_code') = 'I' then
	If idw_dw[2].GetItemDecimal(1,'txn_amount') <> 0 Then
			MessageBox('Invalid payment method','The Payment Amount must be zero for a payment method of "Inapplicable"')
			idw_dw[2].SetColumn('payment_method_code')
			idw_dw[2].SetFocus()
			return -1
	End if
End if

/*	PR 1480 - added date validation to prevent application error messages
*/
/*	Under certain circumstances the messagebox occurred twice (once from itemchanged & again from
	check_bus_rules).  Boolean added (ib_msgbox_already) - is set to true in itemchanged if messagebox was triggered.
	Only trigger messagebox below if it hasn't already occurred.
*/
ld_paid_from = Date(ldt_paid_from_date)
IF ld_paid_from < Date('1900-01-01') OR ld_paid_from > Date('2079-06-06') THEN
	IF ib_msgbox_already = False THEN
		MessageBox('Invalid Date', "The Period 'from' date is invalid.  Please enter a date " + &
					+ "between 01/01/1900 and 06/06/2079.", Information!)
	END IF
	idw_dw[1].SetColumn('paid_from_date')
	idw_dw[1].SetFocus()
	ib_msgbox_already = False	// reset variable
	Return -1
END IF

ld_paid_to = Date(ldt_paid_to_date)
IF ld_paid_to < Date('1900-01-01') OR ld_paid_to > Date('2079-06-06') THEN			
	IF ib_msgbox_already = False THEN
		MessageBox('Invalid Date', "The Period 'to' date is invalid.  Please enter a date " + &
					+ "between 01/01/1900 and 06/06/2079.", Information!)
	END IF
	idw_dw[1].SetColumn('paid_to_date')
	idw_dw[1].SetFocus()
	ib_msgbox_already = False	// reset variable
  	Return -1
END IF	// End of PR 1480 changes

/* new edit checks added to paid to and from dates for P10229
*/
//grab the flag to see if the paid related dates are 
IF ls_required_flag <> "Y" THEN
	IF NOT isnull(ld_paid_from) AND NOT ISNULL(ld_paid_to) THEN
		MessageBox('Invalid Date', "The Period From and To dates are not required for this payment type " + &
					+ "and cannot be added.", Information!)			
		idw_dw[1].SetColumn('paid_to_date')
		idw_dw[1].SetFocus()
  		Return -1
	END IF 
END IF

// check the status on the columns to see if the have been modified if so check
/*
Hi. I spoke to Diana and here's what we are going to do:

1. Enforce the rules on the Payment_Type.fromto_date_flag  (Yes = must enter, No = cannot enter)
2. Period From Date - if > 6 months in past, Warning Message
3. Period To Date - if > 6 months in future, Warning Message
4. If Period From and Period To are more than 6 months apart, Warning Message

*/
IF idw_dw[1].getitemstatus(1,'paid_to_date',primary!) = datamodified! OR +&
   idw_dw[1].getitemstatus(1,'paid_to_date',primary!) = newmodified! OR +&
	idw_dw[1].getitemstatus(1,'paid_from_date',primary!) = datamodified! OR +&
   idw_dw[1].getitemstatus(1,'paid_from_date',primary!) = newmodified! THEN
	
	IF daysafter(date(f_server_datetime()),ld_paid_to) > 180 THEN
		li_message = MessageBox('Invalid date',"You have entered a paid to date that is more than 6 months in the future. Continue?", question!,yesno!)	
		IF li_message = 2 THEN
			idw_dw[1].SetColumn('paid_to_date')
			idw_dw[1].SetFocus()
			Return -1
		END IF 
	END IF 
	
	IF daysafter(date(f_server_datetime()),ld_paid_from) < -180 THEN
		li_message = MessageBox('Invalid Date',"You have entered a Period From date that is more than 6 months in the past. Continue?", question!,yesno!)	
		IF li_message = 2 THEN
			idw_dw[1].SetColumn('paid_from_date')
			idw_dw[1].SetFocus()
			Return -1
		END IF 
	END IF 
	
	IF daysafter(ld_paid_from,ld_paid_to) > 180 THEN
		li_message = MessageBox('Invalid Date',"The period From and Period To dates are more than 6 months apart. Continue?", question!,yesno!)			
		IF li_message = 2 THEN
			idw_dw[1].SetColumn('paid_to_date')
			idw_dw[1].SetFocus()
			Return -1
		END IF 
	END IF 
	
	IF daysafter(date(f_server_datetime()),ld_paid_to) > 549 THEN
		MessageBox('Invalid Date',"You have entered a Period to date that is more than a year and a half in the future",information!)	
		idw_dw[1].SetColumn('paid_to_date')
		idw_dw[1].SetFocus()
		Return -1
	END IF 
	
END IF 

/* edit checks for p10229 - checks concerned with tax amounts
   1. Only service providers payments will have a tax amount greater than 0
	(taken care of below)
	2. Only specific payment types and payment sub-types will have a tax amount 
	   greater than 0
	(taken care of below)
	3. The tax amount must be greater than or equal to 0
	(taken care of here)
   4. If the tax amount is greater than 0, it must be less than or equal to the 
	   maximum tax amount permitted
		If PAYMENT.tax_amount >= 0: THEN PAYMENT.tax_amount must be <=
      (PAYMENT.total_payment_amount * TAX_RATE.tax_rate) / 1 + TAX_RATE.tax_rate)
   (taken care of here)
*/
/* Tax amount cannot be less than 0 */
IF ldc_tax_amount < 0 THEN
	MessageBox ('Invalid Tax Amount','If entered, the Tax amount must be greater than 0.', Information!)
	idw_dw[1].SetColumn("tax_amount")
	idw_dw[1].SetFocus()
	Return -1	
END IF 

IF ldc_tax_amount > 0 THEN //otherwise just check if we need a rate
	ls_pymt_recpt_type = idw_dw[2].GetItemString(1,"recipient_type_code")
		
	/* first check to see if the period from date >= the first active tax rate */
	IF inv_tax_functions.nf_check_valid_from_date(date(ldt_paid_from_date), date(ldt_paid_to_date),ldec_tax_rate) = 0 THEN
		MessageBox ('Invalid Tax Rate','HST(Tax) is not applicable before "1997-04-01"', Information!)
		idw_dw[1].SetColumn("tax_amount")
		idw_dw[1].SetFocus()
		Return -1
	END IF 
		
	/* check that there is a valid tax rate */
	li_return = inv_tax_functions.nf_get_valid_tax_rate(date(ldt_paid_from_date), date(ldt_paid_to_date),ldec_tax_rate)
	IF li_return < 0 OR ISNULL(ldec_tax_rate)THEN 
		MessageBox ('Invalid Tax Rate','The Tax Rate could not be determined from the paid from and paid to dates', Information!)
		idw_dw[1].setitem(1,"tax_rate",0)
		idw_dw[1].SetFocus()
		Return -1
	END IF 
	
	//set the tax rate on the payment record - it's valid to here.
	idw_dw[1].setitem(1,"tax_rate",ldec_tax_rate)
	
   /* now check the recipient type type for allowable tax */
	IF inv_tax_functions.nf_check_recipient_type_tax(ls_pymt_recpt_type) = "N" THEN
		MessageBox ('Invalid Tax Amount','This type of Recipient does not allow for a Tax Amount', Information!)
		idw_dw[1].SetColumn("tax_amount")
		idw_dw[1].SetFocus()
		Return -1
	END IF 
	
	/* Get payment type & sub type combo HST allowed tax flag check it */
	IF inv_tax_functions.nf_check_payment_type_tax(ls_payment_type,ls_payment_sub_type) = "N" THEN
		MessageBox ('Invalid Tax Amount','This Payment Type/ Sub Type does not allow for a Tax Amount', Information!)
		idw_dw[1].SetColumn("tax_amount")
		idw_dw[1].setitem(1,"tax_rate",0)
		idw_dw[1].SetFocus()
		Return -1
	END IF 
	
	/* since the INDIVIDUAL will fail on the recipient type we simply need 
	   to check for individual recipients to see if their tax flags are okay
	*/
	IF ls_recipient_type <> 'I' THEN
		ll_recipient_no = idw_dw[2].GetItemNumber(1,"recipient_no")
		/* tax amount can only be given to certain PROVIDERS	*/
		IF inv_tax_functions.nf_check_provider_tax_flag(ll_recipient_no,ls_recipient_type) = "N" THEN
			MessageBox ('Invalid Provider','Tax amount is not valid for this Provider', Information!)
			idw_dw[1].SetColumn("tax_amount")
			idw_dw[1].SetFocus()
			Return -1
		END IF 
	END IF 
	
	/* check the maximum tax amount to ensure we have not added too much */
	li_return = inv_tax_functions.nf_get_max_tax_amount(ldt_paid_from_date, ldt_paid_to_date,ldec_total_award_amount,ldc_max_tax_amount)
	IF li_return < 0 THEN 
		MessageBox ('Invalid Tax Amount','Failed to retrieve the Maximum Tax amount', Information!)
		idw_dw[1].SetColumn("tax_amount")
		idw_dw[1].SetFocus()
		Return -1
	END IF 
		
	IF ldc_tax_amount > ldc_max_tax_amount THEN
		MessageBox ('Invalid Tax Amount','The Tax amount entered is greater than the maximum allowed under the effective tax rate', Information!)
		idw_dw[1].SetColumn("tax_amount")
		idw_dw[1].SetFocus()
		Return -1
	END IF 
END IF 
	
/* ensure that there is a tax rate in there if applicable - the only case that it
	is not applicable is for the payment type
	
	UPDATE: March 23 - constraint removed the following check is to replace checks
	
	1. Account Payment Maintenance
    Set the Tax Rate to 0 when the payment period is >= 1997.04.01 but 
	 either of the following is true
	- the Recipient Type is not 'taxable'   (i.e. Recipient_Type.tax_flag = 'N')
	- the Recipient Type is 'taxable' and the Recipient is a Provider and 
	  the Provider is not 'taxable' (i.e. PROVIDER.tax_flag = 'N')
     (we had been setting it to the HST Rate because of Paul's db constraint
	  but he's since changed his constraint)
*/

IF NOT ISNULL(ldt_paid_from_date) AND NOT ISNULL(ldt_paid_to_date) THEN 
		
	/* check that the payment type is okay */
	//IF inv_tax_functions.nf_check_payment_type_tax(ls_payment_type,ls_payment_sub_type) = "Y" THEN
	/* Get payment type & sub type combo HST allowed tax flag check it */
	ls_tax_flag = inv_tax_functions.nf_check_payment_type_tax(ls_payment_type,ls_payment_sub_type) 
	IF ls_tax_flag = "Y" THEN
		
		/* first check to see if the period from date >= the first active tax rate */
		IF inv_tax_functions.nf_check_valid_from_date(date(ldt_paid_from_date), date(ldt_paid_to_date),ldec_tax_rate) = 0 THEN
			idw_dw[1].setitem(1,"tax_rate",0)
		END IF 		
				
		li_return = inv_tax_functions.nf_get_valid_tax_rate(date(ldt_paid_from_date), date(ldt_paid_to_date),ldec_tax_rate)
		IF li_return < 0 OR ISNULL(ldec_tax_rate)THEN 
			idw_dw[1].setitem(1,"tax_rate",0)
		END IF 
		
		ls_pymt_recpt_type = idw_dw[2].GetItemString(1,"recipient_type_code")
			
		 /* now check the recipient type type for allowable tax */
		IF inv_tax_functions.nf_check_recipient_type_tax(ls_pymt_recpt_type) = "N" THEN
			idw_dw[1].setitem(1,"tax_rate",0)
		ELSE
			ll_recipient_no = idw_dw[2].GetItemNumber(1,"recipient_no")
			/* tax amount can only be given to certain PROVIDERS	*/
			IF inv_tax_functions.nf_check_provider_tax_flag(ll_recipient_no,ls_recipient_type) = "N" THEN
				idw_dw[1].setitem(1,"tax_rate",0)
			ELSE
				idw_dw[1].setitem(1,"tax_rate",ldec_tax_rate)
			END IF 	
		END IF 
	ELSE
		idw_dw[1].setitem(1,"tax_rate",0)
	END IF 
END IF 

/****************************************
****************************************/

IF istr_message.as_mode = "delete" THEN

/*	delete authorization if an authorization number
*/
	IF idw_dw[1].GetItemNumber(1,'authorization_no') > 0 THEN
		IF nf_validate_authorization(idw_dw[1].GetItemNumber(1,'authorization_no'),'DELETE') < 0 THEN
			Return -1
		END IF
	END IF

/* see IF there is a payment_document record to delete
	this is the delete section
*/
	IF idw_dw[6].RowCount() > 0 THEN  idw_dw[6].DeleteRow(0)//PAYMENT_DOCUMENT

	//	see IF there are empty invoice records to delete
	ll_rowcount = idw_dw[5].RowCount()   // invoice
	IF ll_rowcount > 0 THEN
		ll_cntr = ll_rowcount
		Do While ll_cntr >= 1
			idw_dw[5].DeleteRow(ll_cntr)
			ll_cntr --
		Loop
	END IF

	// see IF there is a payment/transaction record to delete
	IF idw_dw[1].RowCount() > 0 THEN
		ll_recipient_cntr = 1
		ll_nmbr_recipients = idw_dw[2].RowCount()
		idw_dw[4].Reset() 										// deleted transactions
		DO WHILE ll_recipient_cntr <= ll_nmbr_recipients
			ll_rownum = idw_dw[4].InsertRow(0)
			idw_dw[4].SetItem(ll_rownum,"txn_no",idw_dw[2].GetItemNumber(ll_recipient_cntr,"txn_no"))
			idw_dw[4].SetItem(ll_rownum,"claim_no",idw_dw[2].GetItemNumber(ll_recipient_cntr,"claim_no"))
			idw_dw[4].SetItem(ll_rownum,"payment_no",idw_dw[2].GetItemNumber(ll_recipient_cntr,"payment_no"))
			idw_dw[4].SetItem(ll_rownum,"txn_type_code",idw_dw[2].GetItemString(ll_recipient_cntr,"txn_type_code"))
			idw_dw[4].SetItem(ll_rownum,"txn_amount",idw_dw[2].GetItemDecimal(ll_recipient_cntr,"txn_amount"))
			idw_dw[4].SetItem(ll_rownum,"admin_region_code",idw_dw[2].GetItemString(ll_recipient_cntr,"admin_region_code"))
			idw_dw[4].SetItem(ll_rownum,"scheduled_processing_date",idw_dw[2].GetItemDateTime(ll_recipient_cntr,"scheduled_processing_date"))
			idw_dw[4].SetItem(ll_rownum,"txn_entry_date",idw_dw[2].GetItemDateTime(ll_recipient_cntr,"create_date"))
			idw_dw[4].SetItem(ll_rownum,"txn_entry_user_id",idw_dw[2].GetItemString(ll_recipient_cntr,"create_user_id"))
			ll_recipient_cntr++
		LOOP
		ll_recipient_cntr = 1
		DO WHILE ll_recipient_cntr <= ll_nmbr_recipients
			idw_dw[2].DeleteRow(0)
			ll_recipient_cntr++
		LOOP


		ll_payment_no = idw_dw[1].GetItemNumber(1,"payment_no")
		idw_dw[3].Reset()											// deleted payments
		ll_rownum = idw_dw[3].InsertRow(0)
		idw_dw[3].SetItem(ll_rownum,"payment_no",idw_dw[1].GetItemNumber(1,"payment_no"))
		idw_dw[3].SetItem(ll_rownum,"payment_type_code",idw_dw[1].GetItemString(1,"payment_type_code"))
		idw_dw[3].SetItem(ll_rownum,"claim_no",idw_dw[1].GetItemNumber(1,"claim_no"))
		idw_dw[3].SetItem(ll_rownum,"total_payment_amount",idw_dw[1].GetItemDecimal(1,"total_award_amount"))
		idw_dw[3].SetItem(ll_rownum,"payment_entry_date",idw_dw[1].GetItemDateTime(1,"create_date"))
		idw_dw[3].SetItem(ll_rownum,"payment_entry_user_id",idw_dw[1].GetItemString(1,"create_user_id"))
		idw_dw[1].DeleteRow(1)
			
	END IF
END IF

/*	Check to see IF we are saving payment details
IF so, validate and save the changes to idw_dw[1] and dw_account_transaction
*/
IF idw_dw[1].RowCount() > 0 THEN
	IF nf_validate_account_payments() < 0 THEN Return -1
	IF nf_validate_account_transactions() < 0 THEN Return -1
	lb_duplicate = false
	IF istr_message.as_mode = "display" THEN
		IF idw_dw[6].ModifiedCount() <= 0 and idw_dw[1].ModifiedCount() <= 0 and &
			idw_dw[2].ModifiedCount() <= 0 THEN Return -1
	END IF

	idw_dw[2].SetItem(1,"txn_amount",idw_dw[1].GetItemDecimal(1,"total_award_amount"))
	ls_admin_region_code = inv_payment.idw_basic_claim.GetItemString(1,'admin_region_code')
	idw_dw[2].SetItem(1,"admin_region_code",ls_admin_region_code)
	idw_dw[2].SetItem(1,"scheduled_processing_date",idw_dw[1].GetItemDateTime(1,'scheduled_processing_date'))
	
	lc_authorization_limit = gnv_user_authorizations.nf_get_authorization_limit(ls_admin_region_code,"act")
	IF idw_dw[2].GetItemDecimal(1,"txn_amount") > lc_authorization_limit THEN
		MessageBox("Account Payment Maintenance","You are only authorized to make payments up to " + string(lc_authorization_limit,"$#,##0.00") + &
						"~r~nThis payment will be held for authorization",Information!)
		idw_dw[1].SetItem(1,"authorized_by_code","")
		idw_dw[1].SetItem(1,"authorized_date",ld_null)
		lb_payment_authorized = FALSE
	ELSE
		idw_dw[1].SetItem(1,"authorized_by_code",vgst_user_profile.user_id)
		idw_dw[1].SetItem(1,"authorized_date",f_server_datetime())
		lb_payment_authorized = TRUE
	END IF
	
/*		check that the scheduled processing date is a Thursday for account payments
*/
	IF DayNumber(Date(idw_dw[2].GetItemDateTime(1,'scheduled_processing_date')) ) <> 5	THEN	// Thursday
		IF idw_dw[2].GetItemString(1,'recipient_type_code') <> 'I' AND idw_dw[2].GetItemString(1,'recipient_type_code') <> "V" THEN
			MessageBox('Scheduled Processing Date Error', 'The scheduled date of Medical Aid Account payments must be a Thursday. ')
			Return -1
		END IF
	END IF
END IF

//	IF there is a DOCUMENT_PAYMENT record, save it 

IF idw_dw[6].RowCount() > 0 THEN
	IF idw_dw[1].RowCount() > 0 THEN
		ll_payment_no = idw_dw[6].GetItemNumber(1,"payment_no")
		IF IsNull(ll_payment_no) or ll_payment_no = 0 THEN
			idw_dw[6].SetItem(1,"payment_no",idw_dw[1].GetItemNumber(1,"payment_no"))
		END IF
	END IF
END IF


//	Remove empty invoice records
ll_rowcount = idw_dw[5].RowCount()
IF ll_rowcount > 0 THEN
	ll_cntr = ll_rowcount
	Do While ll_cntr >=1
		ls_invoice_no = idw_dw[5].GetItemString(ll_cntr,"invoice_no") 
		IF IsNull(ls_invoice_no) or ls_invoice_no = "" THEN
			idw_dw[5].DeleteRow(ll_cntr)
		END IF
		ll_cntr --
	Loop
END IF

IF idw_dw[1].GetItemNumber(1,'authorization_no') > 0 THEN
	IF nf_validate_authorization(idw_dw[1].GetItemNumber(1,'authorization_no'),'UPDATE') < 0 THEN
		Return -1
	END IF
ELSE
	IF idw_dw[1].GetItemNumber(1,'authorization_no',PRIMARY!,TRUE) > 0 THEN
		IF nf_validate_authorization(idw_dw[1].GetItemNumber(1,'authorization_no',PRIMARY!,TRUE),'DELETE') < 0 THEN
			Return -1
		END IF
	END IF
END IF


/* Warning for first payment going direct deposit ie. 
only applicable to individual, with benefit_class_code = 'LOE' and claim does not already have an 'LOE' payment to the individual. 
*/


SELECT benefit_class_code
INTO 	:ls_ben_class_code
FROM 	Payment_Type
WHERE payment_type_code = :ls_payment_type_code;	

IF upper(ls_ben_class_code) = 'LOE'  and idw_dw[2].getitemstring(1,'payment_method_code') = 'D' THEN
	
		
	SELECT count(*)
	INTO 	:ll_count
	FROM 	PAYMENT p, 
			Payment_Type pt
	WHERE p.claim_no = :ll_claim_no and
			pt.payment_type_code = p.payment_type_code and
			pt.benefit_class_code = 'LOE' and
			(exists (select * from APPLIED_CLAIM_TXN d
					  where d.payment_no = p.payment_no and
							  d.recipient_type_code = 'I') OR
			exists (select * from UNAPPLIED_CLAIM_TXN e
					  where e.payment_no = p.payment_no and
							  e.recipient_type_code = 'I'))
								  
	USING SQLCA;	
	
	ll_result = SQLCA.nf_handle_error('Embedded SQL: select from PAYMENT','n_account_payment_controller','nf_check_bus_rule')
	
	IF ll_count = 0 THEN				
		li_return = messagebox("Account Payment Module - Warning", "There is Banking information available for this Individual. (It may be out of date)~r" + &
		"Do you wish to use this information for Direct Deposit anyway?", Question!, YesNO!, 2)
		IF li_return = 2 THEN				
			RETURN -1
		END IF
	END IF
END IF


SELECT date_on_document
INTO   :idtm_date_on_doc
FROM  DOCUMENT_INDEX 
WHERE  claim_no = :ll_claim_no
AND    docid    = :ll_docid
USING  SQLCA;
		  
SQLCA.nf_handle_error("SELECT date_on_document","n_account_payment_controller","nf_check_bus_rule") 

/*	Where payment is not identified as an adjustment, the amount is greater than $110 and
	the payment_type_code is one of the codes in the case statement then
	display a warning message to indicate if a payment of the same type and with
	the same dates or overlapping dates already exists for this claim and individual
*/

/* MA000705 removed criteria and ldec_total_payment_amount > 110
*/

// Check for possible duplicate payment 
IF (ls_pay_adj_flag = 'N' OR IsNull(ls_pay_adj_flag)) THEN 
	CHOOSE CASE ls_payment_type_code 
		CASE '18','21','22','23','24','25','26','27','29','41','42','43','44','45','46' 
			lds_duplicate_payment_numbers = create datastore
			lds_duplicate_payment_numbers.dataobject = 'ds_duplicate_payment_numbers'
			lds_duplicate_payment_numbers.settransobject(SQLCA)
			li_return = lds_duplicate_payment_numbers.retrieve(ll_claim_no, ls_payment_type_code, ldec_total_award_amount, Date(ldt_paid_from_date), Date(ldt_paid_to_date))

			IF li_return > 0 THEN 
				FOR ll_x = 1 TO li_return 
					ls_dup_pay_nums = ls_dup_pay_nums + String(lds_duplicate_payment_numbers.GetItemNumber(ll_x, "payment_no")) + "  " 
				NEXT 
				li_return = Messagebox("Possible Duplicate Payment - Warning", "The payment you are entering is a possible duplicate of payments " + & 
											  "previously authorized~rfor this claim.  Please verify that this payment is not a duplicate.~r~r" + & 
											  "Possible duplicate payment/s numbers are (" + ls_dup_pay_nums + ") ~rDo you wish to continue saving?", Information!, YesNo!, 2) 
				IF li_return = 2 THEN
					RETURN -1
				END IF
			END IF
		END CHOOSE 
END IF

ls_administering_act_code = inv_payment.idw_basic_claim.GetItemString(1,'administering_act_code')

IF istr_message.as_mode <> "display" THEN
	
	// P10281 - Firefighters' Compensation Act
	// check the BRs for the payment of an FCA document
	IF ls_administering_act_code = 'FCA' AND (Trim(ls_payment_sub_type) = '' OR Trim(ls_payment_sub_type) = '11') AND ll_docid > 0 THEN
		ls_recipient_type_code = idw_dw[2].GetItemString(idw_dw[2].GetRow(),'recipient_type_code')
		li_return = nf_check_fca_payment(ll_claim_no,ll_docid,ls_payment_type,Trim(ls_payment_sub_type),ls_recipient_type_code)
		IF li_return = -1 THEN RETURN li_return
	END IF
	
	/* business rule checks that apply for Project 10127 Medical Society Agreement
		must be a payment type of 21 and a subtype of 01 or 02
		If the type is automatic and fails on validation then fail but do not create the 
		transactions on the otherhand if it's manual then we need to fix any errors before proceeding
	*/
	// AND (isnull(ls_payment_sub_type) OR TRIM(ls_payment_sub_type) = "02") 
	IF ls_payment_type_code = '21' THEN
		IF (ls_payment_sub_type = '02' or ls_payment_sub_type = '10') AND ll_docid = 0 THEN
			MessageBox('Advance Payment Error','You cannot advance an Early Filing Bonus.',StopSign!)
			RETURN -1
		END IF
		
		/* P10151-32 - J.Hawker, 2005.04.13 - Get the date on document. If it is less than June 1, 2005,   
			call old function, nf_create_early_filing_bonus, otherwise, call new function, nf_create_new_efb
			which calculated the EFB using a percentage amount.
		*/
		IF ls_recipient_type = 'M' THEN
						
			SELECT nbms_early_filing_bonus_flag , chiro_early_filing_bonus_flag
			INTO   :ls_nbms_efb_flag ,            :ls_nb_chiro_efb_flag
			FROM   PROVIDER
			WHERE  provider_no = :ll_recipient_no
			USING SQLCA;			
			SQLCA.nf_handle_error("SELECT efb flags","n_account_payment_controller","nf_check_bus_rule") 
			
			IF ls_nbms_efb_flag = 'Y' AND ls_administering_act_code = 'WCA' THEN
				// potentially valid for NBMS EFB
				IF DATE(idtm_date_on_doc) <= IDT_NBMS_1_END	THEN
					li_return = nf_create_early_filing_bonus(ls_message)
				ELSE
					li_return = nf_create_new_early_filing_bonus(ls_message,'new')
				END IF

				// PR6505 - Give warning if early filing bonus is created but payment is not authorized
				IF li_return = 1 AND lb_payment_authorized = FALSE THEN
					Messagebox("Warning: Bonus on hold", "An early filing bonus was created but it will be held until it's associated payment is authorized.")
				END IF

				li_return = nf_handle_EFB_return(li_return,ls_message,"Medical Society Agreement")
				IF li_return = -1 THEN RETURN li_return

			ELSEIF ls_nb_chiro_efb_flag = 'Y' THEN
				// no EFB created for FCA report fee
				IF ls_payment_sub_type <> '11' THEN
					// potentially valid for NB Chiro EFB
							
					//**********************************************************
					// P10151-47 - NB Chiro Assoc Agreement
					
					li_return = nf_create_chiro_early_filing_bonus(ls_message)
	
					// PR6505 - Give warning if early filing bonus is created but payment is not authorized
					IF li_return = 1 AND lb_payment_authorized = FALSE THEN
						Messagebox("Warning: Bonus on hold", "An early filing bonus was created but it will be held until it's associated payment is authorized.")
					END IF
					
					li_return = nf_handle_EFB_return(li_return,ls_message,"NB Chiro Association Agreement")
					IF li_return = -1 THEN RETURN li_return
				END IF
				
			ELSE
				// not eligible for either EFB
				idw_dw[9].reset()
				idw_dw[10].reset()
				idw_dw[11].reset()
				IF Trim(ls_payment_sub_type) <> '' AND ls_payment_sub_type <> '11' AND ls_payment_sub_type <> '12' THEN
					// if attempting to create manual EFB, display message
					MessageBox('Incorrect Recipient','This Medical Aid provider is not eligible to receive an Early Filing Bonus payment.',StopSign!)
					RETURN -1
				END IF
			END IF

		ELSE
			// recipient type <> M
			idw_dw[9].reset()
			idw_dw[10].reset()
			idw_dw[11].reset()
			IF Trim(ls_payment_sub_type) <> '' THEN
				// if attempting to create manual, display message
				MessageBox('Incorrect Recipient','Early Filing Bonus payments cannot be paid to non-Medical Aid providers.',StopSign!)
				RETURN -1
			END IF
		END IF
					
	ELSE
		// Pay type <> 21
		idw_dw[9].reset()
		idw_dw[10].reset()
		idw_dw[11].reset()
	END IF	
ELSE
	//
   // modifying payment
	//
	//**********************************************************
	// P10151-47 NB Chiro Agreement changes
	
	// If the user has changed the payment to be a Medical Account payment, 
	// then check to see if an AEFB is needed:
	// NBMS EFB Agreement 1
	IF ls_payment_type_code = '21' THEN
		
		IF Trim(ls_payment_sub_type) = '' OR Trim(ls_payment_sub_type) = '12' THEN
			// payment sub type 12 applies to NBMS
			
			IF DATE(idtm_date_on_doc) >= IDT_NBMS_1_START AND DATE(idtm_date_on_doc) <= IDT_NBMS_1_END AND ls_administering_act_code = 'WCA' THEN 
				IF idw_dw[1].GetItemStatus(1,'payment_type_code',Primary!) = DataModified! OR idw_dw[1].GetItemStatus(1,'payment_sub_type_code',Primary!) = DataModified! THEN
					idw_dw[9].InsertRow(0)  		// payment
					idw_dw[10].InsertRow(0)  		// account transactions
					idw_dw[11].InsertRow(0)  		// PAYMENT_DOCUMENT
					
					li_return = nf_create_early_filing_bonus(ls_message)
					li_return = nf_handle_EFB_return(li_return,ls_message,"Medical Society Agreement")
					IF li_return = -1 THEN RETURN li_return
				END IF
			END IF
			
			// NBMS EFB Agreement 2
			IF DATE(idtm_date_on_doc) >= IDT_NBMS_2_START AND ls_administering_act_code = 'WCA' THEN 
				IF idw_dw[1].GetItemStatus(1,'payment_type_code',Primary!) = DataModified! OR idw_dw[1].GetItemStatus(1,'payment_sub_type_code',Primary!) = DataModified! THEN
					idw_dw[9].InsertRow(0)  		// payment
					idw_dw[10].InsertRow(0)  		// account transactions
					idw_dw[11].InsertRow(0)  		// PAYMENT_DOCUMENT
					li_return = nf_create_new_early_filing_bonus(ls_message,'new')				
				END IF
	
				/* P10151-32 - J.Hawker, 2005.04.14 - If payment is being modified and payment is a 
					medical account, check to see if it has a related AEFB(unprocessed) for doc with 
					date of June 1, 2005 or greater. If so, re-calculate the EFB and set new values 
					on the PAYMENT and UNAPPLIED_CLAIM_TXN records of the EFB payment.
				*/
				IF idw_dw[1].GetItemStatus(1,'total_award_amount',Primary!) = DataModified! THEN
					li_return = nf_create_new_early_filing_bonus(ls_message,'modify')
				END IF
				
				li_return = nf_handle_EFB_return(li_return,ls_message,"Medical Society Agreement")
				IF li_return = -1 THEN RETURN li_return
	
			END IF
		END IF
		
		IF Trim(ls_payment_sub_type) = '' THEN
			// payment sub type 12 does not apply to NBCA
			// NB Chiro EFB Agreement
			IF DATE(idtm_date_on_doc) >= IDT_CHIRO_START THEN 
				IF idw_dw[1].GetItemStatus(1,'payment_type_code',Primary!) = DataModified! OR idw_dw[1].GetItemStatus(1,'payment_sub_type_code',Primary!) = DataModified! THEN
					idw_dw[9].InsertRow(0)  		// payment
					idw_dw[10].InsertRow(0)  		// account transactions
					idw_dw[11].InsertRow(0)  		// PAYMENT_DOCUMENT
	
					li_return = nf_create_chiro_early_filing_bonus(ls_message)
					li_return = nf_handle_EFB_return(li_return,ls_message,"NB Chiro Association Agreement")
					IF li_return = -1 THEN RETURN li_return
				END IF
			END IF
		END IF
	END IF

	// If the user has changed the payment to NOT be a Medical Account payment, 
	// then check to see if user wants to delete AEFB :
	
	// NOTE that this will work for NB Chiro & NBMS Auto EFBs
	ls_old_payment_type_code     = idw_dw[1].GetItemString(1,'payment_type_code',Primary!,TRUE)
	ls_old_payment_sub_type_code = idw_dw[1].GetItemString(1,'payment_sub_type_code',Primary!,TRUE)
	ll_payment_no = idw_dw[1].GetItemNumber(1,'payment_no')
	
	IF (ls_old_payment_type_code = '21' and ls_payment_type_code <> '21') OR &
		(ls_old_payment_type_code = '21' and ls_old_payment_sub_type_code = ''   and ls_payment_type_code = '21' and ls_payment_sub_type <> '' ) OR &
		(ls_old_payment_type_code = '21' and ls_old_payment_sub_type_code = '12' and ls_payment_type_code = '21' and ls_payment_sub_type <> '12' ) THEN
		// check for scheduled Early Filing Bonus payments
		li_count = idw_dw[3].RowCount()
		
		IF li_count > 0 THEN
			IF MessageBox('Delete Early Filing Bonus?','This scheduled Medical Account payment has an associated scheduled'&
																 +'~nEarly Filing Bonus payment. This bonus payment must be deleted if'&
																 +'~nthe payment type or sub type of its related payment is changed. Do'&
																 +'~nyou wish to continue with the save?', Exclamation!, YesNo!,2) = 2 THEN
				// User chose not to update payment type/sub type
				RETURN -1
			ELSE
				li_rtn = idw_dw[3].DeleteRow(0)  // PAYMENT
				li_rtn = idw_dw[4].DeleteRow(0)  // UNAPPLIED_CLAIM_TXN
				li_rtn = idw_dw[12].DeleteRow(0) // PAYMENT_DOCUMENT
				IF ls_payment_type_code = '21' and ls_payment_sub_type <> '' and ls_payment_sub_type <> '12' THEN
					li_return = nf_create_manual_EFB(ls_payment_sub_type,ls_administering_act_code)
					IF li_return = -1 THEN RETURN li_return
				END IF
			END IF
		ELSE
			//there are no existing EFBs, however, should verify manual EFBs
			IF ls_payment_type_code = '21' and ls_payment_sub_type <> '' and ls_payment_sub_type <> '11' and ls_payment_sub_type <> '12' THEN // no EFB for NBMS FCA report fee!
				li_return = nf_create_manual_EFB(ls_payment_sub_type,ls_administering_act_code)
				IF li_return = -1 THEN RETURN li_return
			END IF
		END IF
	END IF
	
	// P10281 - Firefighters' Compensation Act
	// check the BRs for the payment of an FCA document
	IF ls_administering_act_code = 'FCA' AND ll_docid > 0 THEN
		ls_recipient_type_code = idw_dw[2].GetItemString(idw_dw[2].GetRow(),'recipient_type_code')
		li_return = nf_check_fca_payment(ll_claim_no,ll_docid,ls_payment_type,Trim(ls_payment_sub_type),ls_recipient_type_code)
		IF li_return = -1 THEN RETURN li_return
	END IF
END IF

IF ls_payment_type_code = "18" THEN // death benefit
	lds_difficult_claim = Create U_DS
	lds_difficult_claim.DataObject = 'd_difficult_claim'
	lds_difficult_claim.SetTransObject(SQLCA)
	
	li_rows = lds_difficult_claim.Retrieve(ll_claim_no)
	
	FOR li_counter = 1 TO li_rows
		ls_fatality = lds_difficult_claim.GetItemString(li_counter, 'fatality_flag')
		IF ls_fatality = 'Y' THEN
			EXIT
		END IF
	NEXT

	IF ls_fatality = 'N' OR IsNull(ls_fatality) OR ls_fatality = '' THEN
		MessageBox('Death Benefit Payment Error', 'The claim must have the fatality flag set in order to have death benefits paid.')
		RETURN -1
	END IF
END IF
	
//If the payment is a automatically created payment for WRC_INVOICE then change the paid status code to "S" scheduled
iF IDW_DW[6].rowcount() > 0 THEN
	iF idw_dw[6].GetItemString(1,"paid_status_code") = "E" and idw_dw[6].GetItemString(1,"paid_status_explanation_code") = "16"  THEN
		idw_dw[6].SetItem(1,"paid_status_code","S")
	END IF
end if

// ********* ePhysio - travel expense log *************** 

IF (istr_message.as_mode = 'add' OR istr_message.as_mode = 'advance' OR istr_message.as_mode = 'display') and idw_dw[1].RowCount() > 0 and ls_payment_type_code = "23" THEN
// Check for travel expenses and close the travel expense window - Close(w_travel_expense)
	IF IsValid(w_travel_expense) Then
		ll_max = w_travel_expense.dw_travel_expense.RowCount()
		ll_payment_no = idw_dw[1].getitemnumber(1,"payment_no")
		
		ldt_paid_from_date = idw_dw[1].getitemdatetime(1,"paid_from_date")
		ldt_paid_to_date    = idw_dw[1].getitemdatetime(1,"paid_to_date")	
		
		For row= 1 to ll_max 
			li_travel = w_travel_expense.dw_travel_expense.getitemnumber(row,"travel")
			ll_travel_expense_no = w_travel_expense.dw_travel_expense.getitemnumber(row,"iw_travel_expense_no")
			ll_rehab_invoice_no = w_travel_expense.dw_travel_expense.getitemnumber(row,"rehab_invoice_no")
			li_line_no = w_travel_expense.dw_travel_expense.getitemnumber(row,"line_no")
			ll_claim_no = w_travel_expense.dw_travel_expense.getitemnumber(row,"claim_no")
			ldt_service_date = Date(w_travel_expense.dw_travel_expense.getitemdatetime(row,"service_date"))
			ll_payment_no = w_travel_expense.dw_travel_expense.getitemnumber(row,"payment_no")

			IF li_travel = 1 and Date(ldt_paid_from_date) > ldt_service_date THEN 
				Messagebox('Error','The paid from date must be on or before the travel expense service date: ' + String(ldt_service_date,"mmm d, yyyy") + '.', Exclamation!)
				Return -1
			END IF
			
			IF li_travel = 1 and Date(ldt_paid_to_date) < ldt_service_date THEN 
				Messagebox('Error','The paid to date must be on or after the travel expense service date" ' + String(ldt_service_date,"mmm d, yyyy") + '.', Exclamation!)
				Return -1
			END IF

			IF ll_travel_expense_no = 0 THEN // there isn't a travel expense record 
				IF li_travel = 1 THEN  // there is a travel expense
					IF nf_add_travel_expense(ll_claim_no, ll_payment_no, ll_rehab_invoice_no, li_line_no) < 0 THEN
						RETURN -1
					END IF
				END IF
			ELSE // there was a pre-existing travel expense record
				IF li_travel = 0 THEN // the existing travel expense record is now being removed
					// remove the travel expense record which breaks the link with the payment
					nf_delete_travel_expense(ll_travel_expense_no)
				END IF
			END IF
		Next
	END IF
END IF


	
Return 0
end function

public function integer nf_check_fca_payment (long al_claim_no, long al_doc_id, string as_payment_type_code, string as_payment_sub_type_code, string as_recipient_type_code);INTEGER	li_msg
STRING   ls_document_type_code, ls_treatment_fee_flag, ls_report_fee_flag, ls_warning_flag, ls_error_flag


// check that the document type can be paid using the pay type/sub type combination

// get document type
SELECT type_code 
INTO   :ls_document_type_code 
FROM   DOCUMENT_INDEX
WHERE  claim_no = :al_claim_no
AND    docid    = :al_doc_id
USING SQLCA;
SQLCA.nf_handle_error('n_account_payment_controller','nf_check_fca_payment','SELECT type_code')

// get flags for BRs
SELECT treatment_fee_flag ,
		 report_fee_flag ,
		 warning_flag ,
		 error_flag
INTO   :ls_treatment_fee_flag,
		 :ls_report_fee_flag,
		 :ls_warning_flag,
		 :ls_error_flag
FROM   Admin_Act_Document_Restriction
WHERE  administering_act_code = 'FCA'
AND    document_type_code = :ls_document_type_code
USING SQLCA;
SQLCA.nf_handle_error('n_account_payment_controller','nf_check_fca_payment','SELECT report_fee_flag') 

// if an error could be displayed for the payment of this doc
IF ls_error_flag = 'Y' THEN
	CHOOSE CASE ls_treatment_fee_flag + ls_report_fee_flag
		CASE 'YY'
			// user should have been prevented from getting into the module for this situation
			MESSAGEBOX('UH-OH','user should have been prevented from getting into the module for this situation (1).',EXCLAMATION!)
			RETURN -1
			
		CASE 'YN'
			IF as_payment_sub_type_code = '' THEN
				// paying treatment fee - error
				MessageBox('Payment Sub Type Error','For a Firefighter Act claim, this document type ("'+ls_document_type_code+'") only allows for payment of the reporting fee and not payment of the account.', StopSign!)
				RETURN -1
			END IF
			
		CASE 'NY'
			IF as_payment_type_code = '21' AND as_payment_sub_type_code <> '' AND as_payment_sub_type_code <> '12' THEN
				// paying report fee - error
				MessageBox('Payment Sub Type Error','For a Firefighter Act claim, this document type ("'+ls_document_type_code+'") only allows for payment of the account and not payment of the reporting fee.', StopSign!)
				RETURN -1
			END IF
			
		CASE ELSE
			// constraint should prevent this
			MESSAGEBOX('UH-OH','constraint should prevent this (1).',EXCLAMATION!)
			RETURN -1
			
	END CHOOSE
	
// if a warning could be displayed for the payment of this doc
ELSEIF ls_warning_flag = 'Y' THEN
	CHOOSE CASE ls_treatment_fee_flag + ls_report_fee_flag
		CASE 'YY'
			// user should have been prevented from getting into the module for this situation
			MESSAGEBOX('UH-OH','user should have been prevented from getting into the module for this situation (2).',EXCLAMATION!)
			RETURN -1
			
		CASE 'YN'
			IF as_payment_sub_type_code = '' THEN
				// paying treatment fee - warning
				li_msg = MessageBox('Payment Sub Type Warning','For a Firefighter Act claim, this document type ("'+ls_document_type_code+'") typically allows for payment of the reporting fee and not payment of the account. Do you want to continue?', Question!, YesNo!, 2)
				IF li_msg = 2 THEN
					RETURN -1
				ELSE
					RETURN 1
				END IF
			END IF
			
		CASE 'NY'
			IF as_payment_type_code = '21' AND as_payment_sub_type_code <> '' AND as_payment_sub_type_code <> '12' THEN
				// paying report fee - warning
				li_msg = MessageBox('Payment Sub Type Warning','For a Firefighter Act claim, this document type ("'+ls_document_type_code+'") typically allows for payment of the account and not payment of the reporting fee. Do you want to continue?', Question!, YesNo!, 2)
				IF li_msg = 2 THEN
					RETURN -1
				ELSE
					RETURN 1
				END IF
				
			END IF
			
		CASE ELSE
			// constraint should prevent this
			MESSAGEBOX('UH-OH','constraint should prevent this (2).',EXCLAMATION!)
			RETURN -1
			
	END CHOOSE
END IF

IF as_payment_sub_type_code <> '' THEN
	// check that the recipient type is correct
	IF as_recipient_type_code = 'M' THEN
		//OK
	ELSE
		MessageBox('Recipient Error','For a Firefighter Act claim, the reporting fee can only be paid to a Medical Aid provider.', StopSign!)
		RETURN -1
	END IF
END IF

end function

public function integer nf_create_manual_efb (string as_payment_sub_type, string as_administering_act_code);INTEGER    li_return
STRING     ls_message

IF DATE(idtm_date_on_doc) >= IDT_NBMS_1_START AND DATE(idtm_date_on_doc) <= IDT_NBMS_1_END AND as_payment_sub_type = '02' AND as_administering_act_code = 'WCA' THEN
	li_return = nf_create_early_filing_bonus(ls_message)
	li_return = nf_handle_EFB_return(li_return,ls_message,"Medical Society Agreement")
	IF li_return = -1 THEN RETURN li_return				
ELSEIF DATE(idtm_date_on_doc) >= IDT_NBMS_2_START AND as_payment_sub_type = '02' AND as_administering_act_code = 'WCA' THEN
	li_return = nf_create_new_early_filing_bonus(ls_message,'new')
	li_return = nf_handle_EFB_return(li_return,ls_message,"Medical Society Agreement")
	IF li_return = -1 THEN RETURN li_return
ELSEIF DATE(idtm_date_on_doc) >= IDT_CHIRO_START AND as_payment_sub_type = '10' THEN
	li_return = nf_create_chiro_early_filing_bonus(ls_message)
	li_return = nf_handle_EFB_return(li_return,ls_message,"NB Chiro Association Agreement")
	IF li_return = -1 THEN RETURN li_return
ELSEIF (DATE(idtm_date_on_doc) < IDT_CHIRO_START AND as_payment_sub_type = '10') &
	OR  (DATE(idtm_date_on_doc) < IDT_NBMS_1_START AND as_payment_sub_type = '02') THEN
	MessageBox('Early Filing Bonus Error', 'This document is too old to have an early filing bonus of this type.', StopSign!)
	RETURN -1
END IF

RETURN li_return
end function

public function integer nf_add_travel_expense (long al_claim_no, long al_payment_no, long al_rehab_invoice_no, integer al_line_no);long ll_iw_travel_expense_no
long ll_row
long ll_status = 1 //PR25051 2014-10-21 David Worboys
datetime ldtm_datetime

//get the last travel expense no
ll_iw_travel_expense_no = nf_get_last_travel_expense_no()

IF (ll_iw_travel_expense_no > 0) THEN  //Start PR25051 2014-10-21 David Worboys

	ldtm_datetime = f_server_datetime()
	
	ll_row = idw_dw[13].insertrow(0)
	
	idw_dw[13].setitem(ll_row,"iw_travel_expense_no",ll_iw_travel_expense_no)
	idw_dw[13].setitem(ll_row,"claim_no",al_claim_no)
	idw_dw[13].setitem(ll_row,"payment_no",al_payment_no)  // the payment no is null until the nf_set_identifiers is triggered
	idw_dw[13].setitem(ll_row,"rehab_invoice_no",al_rehab_invoice_no)
	idw_dw[13].setitem(ll_row,"line_no",al_line_no)
	idw_dw[13].setitem(ll_row,"reimbursement_date",Date(ldtm_datetime))
ELSE
	ll_status = -1
END IF //End PR25051 2014-10-21 David Worboys

Return ll_status
end function

public function long nf_get_last_travel_expense_no ();// nf_get_last_travel_expense_no 
//
Integer li_rtn, li_trancount 
Long    ll_num_rows_updated, ll_iw_travel_expense_no 

// Check Transaction Count, if it is not 1 then an application error will be generated 
SQLCA.nf_transaction_count(li_trancount, 1, "n_account_payment_controller", "nf_get_last_travel_expense_no", "Transaction Count is not 1.", TRUE)

// Update Last_Iw_Travel_Expense_No to lock it 
UPDATE Last_Iw_Travel_Expense_No 
   SET last_iw_travel_expense_no = last_iw_travel_expense_no + 1 
 USING SQLCA ; 

// Get the number of rows updated 
ll_num_rows_updated = SQLCA.SQLNRows 

li_rtn = SQLCA.nf_handle_error("n_account_payment_controller", "", "nf_get_last_travel_expense_no - UPDATE Last_Iw_Travel_Expense_No SET last_iw_travel_expense_no = last_iw_travel_expense_no + 1")

// If 1 row was updated Select the identifier else generate application error 
IF ll_num_rows_updated = 1 THEN  
	SELECT last_iw_travel_expense_no
	  INTO :ll_iw_travel_expense_no
	  FROM Last_Iw_Travel_Expense_No
	 USING SQLCA ; 

	li_rtn = SQLCA.nf_handle_error("n_account_payment_controller", "", "nf_get_last_travel_expense_no - SELECT last_iw_travel_expense_no FROM Last_Iw_Travel_Expense_No")
ELSE  // Error, there should have been only one record updated
	Error.is_database = SQLCA.Database
	Error.Text = "An error occurred updating Last_Iw_Travel_Expense_No table. The SQLCA.SQLNRows variable says " + String(ll_num_rows_updated) + " row(s) were updated, only 1 row was expected." 
	Error.WindowMenu = "" 
	Error.Object = "n_account_payment_controller"
	Error.ObjectEvent = "nf_get_last_travel_expense_no"
	SignalError()  // calls application system error that will do rollbacks, open w_error and then halt close
END IF

RETURN ll_iw_travel_expense_no

end function

public function integer nf_delete_travel_expense (long al_travel_expense_no);// nf_delete_travel_expense
//
Long    ll_num_rows
Integer li_rtn 

ll_num_rows = idw_dw[14].Retrieve(al_travel_expense_no)
li_rtn = SQLCA.nf_handle_error("n_account_payment_controller", "", "nf_delete_travel_expense - idw_dw[14].Retrieve(al_travel_expense_no)")

IF ll_num_rows > 0 THEN
	li_rtn = idw_dw[14].DeleteRow(1)
	IF li_rtn = -1 THEN
		Error.is_database = SQLCA.Database
		Error.Text = "An error occurred deleting a row from the IW_TRAVEL_EXPENSE table. travel_expense_no = " + String(al_travel_expense_no) 
		Error.WindowMenu = "" 
		Error.Object = "n_account_payment_controller"
		Error.ObjectEvent = "nf_delete_travel_expense"
		SignalError()  
	END IF
END IF

RETURN 0

end function

public function decimal nf_calculate_bonus_payment (datetime adt_treatment_date, string as_provider_type_code, string as_provider_sub_type_code);
/* "Create a function to determine the amount of the early filing bonus
    1. Pass the argument variable adt_treatment_date
	    (DOCUMENT_INDEX.date_on_document)
	 2. Return the amount of early filing bonus payment found (money)
	    Determine the amount of the early filing bonus by the following
		 query:
		 
	  	SELECT early_filing_bonus_amount
	        FROM Nbms_Fee_Schedule
	       WHERE effective_date = (select max(effective_date) 
	       from Nbms_Fee_Schedule 
	       where effective_date <= :adt_treatment_date)"
										  
	  
ARGS: adt_treatment_date

RETURNS: amount of the early filing bonus

*/
DEC  ldec_amount


SELECT early_filing_bonus_amount
INTO   :ldec_amount
FROM   Nbms_Fee_Schedule
WHERE  provider_type_code     = :as_provider_type_code
AND    provider_sub_type_code = :as_provider_sub_type_code
AND    effective_date = (SELECT max(effective_date) 
                         FROM   Nbms_Fee_Schedule 
                         WHERE  effective_date        <= :adt_treatment_date
                         AND    provider_type_code     = :as_provider_type_code
                         AND    provider_sub_type_code = :as_provider_sub_type_code)
USING SQLCA;										  
SQLCA.nf_handle_error("SELECT early_filing_bonus_amount","n_account_payment_controller","nf_calculate_bonus_payment")

						  									  
IF isnull(ldec_amount) OR ldec_amount < 0 THEN
	ldec_amount = 0
END IF
		
RETURN ldec_amount
end function

public function integer nf_calculate_efb_by_percent (datetime adtm_treatment_date, string as_provider_type_code, string as_provider_sub_type_code, ref decimal adec_amount);/* P10151-32 - J. Hawker, 2005-04-21 
   New function to calculate the EFB amount using a percentage.
*/

DECIMAL ldec_percent, ldec_percent_two
DECIMAL ldec_amount

SELECT early_filing_bonus_factor1, early_filing_bonus_factor2
INTO   :ldec_percent, :ldec_percent_two
FROM   Nbms_Fee_Schedule
WHERE  provider_type_code     = :as_provider_type_code
AND    provider_sub_type_code = :as_provider_sub_type_code
AND    effective_date = (SELECT max(effective_date) 
                         FROM   Nbms_Fee_Schedule 
                         WHERE  effective_date        <= :adtm_treatment_date
                         AND    provider_type_code     = :as_provider_type_code
                         AND    provider_sub_type_code = :as_provider_sub_type_code)
USING SQLCA;
IF SQLCA.nf_handle_error("SELECT ", "n_account_payment_controller","nf_calculate_efb_by_percent") < 0 THEN
	RETURN -1
END IF
				
IF IsNull(ldec_percent) OR ldec_percent < 0 THEN
	ldec_percent = 0
END IF

IF IsNull(ldec_percent_two) OR ldec_percent_two < 0 THEN
	ldec_percent_two = 0
END IF

ldec_amount = ROUND(adec_amount * ldec_percent, 2)

IF ldec_percent_two > 0 THEN
	ldec_amount = ROUND(ldec_amount + ROUND(ROUND(adec_amount + ldec_amount, 2) * ldec_percent_two,2),2)
END IF

adec_amount = ldec_amount

IF IsNull(adec_amount) OR adec_amount < 0 THEN RETURN -1

RETURN 0
end function

public function integer nf_check_early_filing_bonus (long al_payment_no, string as_provider_type_code, string as_provider_sub_type_code);/* P10151-32 - J. Hawker, 2005.04.21 - New function to calculate what the Manual
   Early Filing Bonus amount SHOULD be based on sum of pmt amount - sum of bonuses.
*/

INTEGER li_return, li_count
DECIMAL ldec_total_amount, ldec_total_efb

SELECT Sum(net_payment_amount)
INTO   :ldec_total_amount
FROM   PAYMENT a, PAYMENT_DOCUMENT b
WHERE  a.payment_no = b.payment_no
AND    a.payment_type_code = '21'
AND    a.payment_sub_type_code in ('','12')
AND    b.doc_id = :il_docid
USING  SQLCA;

SQLCA.nf_handle_error("SELECT Sum(net_payment_amount)","n_account_payment_controller","nf_check_early_filing_bonus")

IF SQLCA.SQLNRows = 0 OR IsNull(ldec_total_amount) THEN 
	ldec_total_amount = 0
END IF

li_return = nf_calculate_efb_by_percent(idtm_date_on_doc,as_provider_type_code,as_provider_sub_type_code,ldec_total_amount)

IF li_return < 0 THEN
	MessageBox('Error','A problem was encountered while calculating the Early Filing Bonus.',information!)
	RETURN -1
END IF

SELECT Sum(net_payment_amount) 
INTO   :ldec_total_efb
FROM   PAYMENT a, PAYMENT_DOCUMENT b
WHERE  b.doc_id = :il_docid
AND    a.payment_sub_type_code in ('02','01')
AND    a.payment_type_code     = '21' 
AND    a.payment_no            = b.payment_no
AND    a.payment_no           <> :al_payment_no
USING  SQLCA;

SQLCA.nf_handle_error("SELECT Sum(net_payment_amount)","n_account_payment_controller","nf_check_early_filing_bonus")

IF SQLCA.SQLNRows = 0 OR IsNull(ldec_total_efb) THEN 
	ldec_total_efb = 0
END IF

ldec_total_amount = ldec_total_amount - ldec_total_efb

IF ldec_total_amount < 0 THEN
	ldec_total_amount = 0
END IF

IF	idec_total_amount <> ldec_total_amount THEN
	IF MessageBox('Confirm Amount','The Early Filing Bonus payment will be created with an amount of $' &
		 + STRING(ROUND(idec_total_amount,2)) + '.~r~nThis does not match the calculated Early Filing Bonus' &
		 + ' Amount of $' + STRING(ROUND(ldec_total_amount,2)) + '.~r~nWould you like to continue?', question!, okcancel!,2) = 2 THEN
		RETURN -1
	END IF
END IF

RETURN 0
end function

public function long nf_modify_early_filing_bonus (string as_provider_type_code, string as_provider_sub_type_code);/* P10151-32 - J. Hawker, 2005.04.21 - New function to update an Automatic EFB
   if the original payment amount was modified. 
*/

LONG    ll_payment_no, ll_related_txn_no, ll_recipient_no, ll_pmt, ll_txns, ll_old_recipient
INTEGER li_return
BOOLEAN lb_check, lb_recipient_chg, lb_amt_chg, lb_eligible
DECIMAL ldec_new_amount, ldec_old_amount, ldec_total_amount
STRING  ls_recipient_name,ls_address_line1,ls_address_line2,ls_city,ls_province
STRING  ls_country_desc,ls_country,ls_postal_code,ls_message

ldec_old_amount = idw_dw[1].Getitemdecimal(1,'total_award_amount',Primary!,TRUE)

IF idw_dw[2].RowCount() > 0 THEN			
 	ll_related_txn_no = idw_dw[2].GetItemNumber(1,"txn_no")
	ll_recipient_no   = idw_dw[2].GetItemNumber(1,'recipient_no')
	ll_old_recipient  = idw_dw[2].GetItemNumber(1,'recipient_no',Primary!,TRUE)
END IF

IF ll_related_txn_no > 0 THEN

	SELECT	a.payment_no
	INTO		:ll_payment_no
	FROM		PAYMENT a , UNAPPLIED_CLAIM_TXN b 
	WHERE	   a.payment_no				= b.payment_no
	AND		a.payment_type_code		= "21"
	AND		a.payment_sub_type_code	= "01"
	AND		b.related_txn_no			= :ll_related_txn_no
	AND      a.processed_date        IS NULL
	AND      b.batch_no              = 0
	USING    SQLCA;
	
	SQLCA.nf_handle_error("SELECT a.payment_no","n_account_payment_controller","nf_modify_early_filing_bonus") 
	
	IF ll_payment_no > 0 THEN		
						
		ll_pmt  = idw_dw[9].Retrieve(ll_payment_no)
		IF ll_pmt <= 0 THEN
			RETURN -1
		END IF
		
		ll_txns = idw_dw[10].Retrieve(ll_payment_no)
		IF ll_txns <= 0 THEN
			RETURN -1
		END IF
	
		IF nf_calculate_efb_by_percent(idtm_date_on_doc,as_provider_type_code,as_provider_sub_type_code,idec_total_amount) < 0 THEN
			RETURN -1
		END IF
				
		IF idec_total_amount > 0 THEN			
			idw_dw[9].SetItem(idw_dw[9].GetRow(),'total_award_amount',idec_total_amount)
			idw_dw[10].SetItem(idw_dw[10].GetRow(),'txn_amount',idec_total_amount)				
		ELSE			
			RETURN -1
		END IF			
	END IF	
END IF
	
	

RETURN 0
end function

public function decimal nf_calculate_fca_report_fee_payment (datetime adt_treatment_date, string as_provider_type_code, string as_provider_sub_type_code);/* "Create a function to determine the amount of the FCA report fee
    1. Pass the argument variable adt_treatment_date
	    (DOCUMENT_INDEX.date_on_document)
	 2. Return the amount of FCA report fee payment found (money)
	    Determine the amount of the early filing bonus by the following
		 query:
		 
	  	SELECT reporting_fee_amount
	        FROM Nbms_Fee_Schedule
	       WHERE effective_date = (select max(effective_date) 
	       from Nbms_Fee_Schedule 
	       where effective_date <= :adt_treatment_date)"
										  
	  
ARGS: adt_treatment_date

RETURNS: amount of the report fee

*/

DEC  ldec_amount


SELECT reporting_fee_amount
INTO   :ldec_amount
FROM   Nbms_Fee_Schedule
WHERE  provider_type_code     = :as_provider_type_code
AND    provider_sub_type_code = :as_provider_sub_type_code
AND    effective_date = (SELECT max(effective_date) 
                         FROM   Nbms_Fee_Schedule 
                         WHERE  effective_date        <= :adt_treatment_date
                         AND    provider_type_code     = :as_provider_type_code
                         AND    provider_sub_type_code = :as_provider_sub_type_code)
USING SQLCA;										  
SQLCA.nf_handle_error("SELECT early_filing_bonus_amount","n_account_payment_controller","nf_calculate_bonus_payment")

						  									  
IF isnull(ldec_amount) OR ldec_amount < 0 THEN
	ldec_amount = 0
END IF
		
RETURN ldec_amount

end function

public function string nf_get_module_source_code ();LONG     ll_docid
STRING   ls_module_source_code



ll_docid = istr_message.al_doubleparm[3]


SELECT module_source_code
INTO   :ls_module_source_code
FROM   DOC
WHERE  docid = :ll_docid
USING  ImageTrans;

ImageTrans.nf_handle_error("n_account_payment_controller", "embedded SQL: SELECT module_source_code", "nf_get_module_source_code") 


IF IsNull(ls_module_source_code) THEN
	ls_module_source_code = ''
END IF

RETURN ls_module_source_code

end function

on n_account_payment_controller.create
call super::create
end on

on n_account_payment_controller.destroy
call super::destroy
end on

