$PBExportHeader$w_issue_reporting_fee.srw
$PBExportComments$New window for forwarding mannually id'd doc's to ELIGIBLE_REPORT_FEE_DOCUMENTS
forward
global type w_issue_reporting_fee from w_ancestor
end type
type cb_save from commandbutton within w_issue_reporting_fee
end type
type cb_cancel from commandbutton within w_issue_reporting_fee
end type
type dw_reporting_fees from u_dw_online within w_issue_reporting_fee
end type
type reporting_fee_data from structure within w_issue_reporting_fee
end type
end forward

type Reporting_fee_data from structure
	long		sl_docid
	long		sl_claim_no
	long		sl_provider_no
	string		ss_provider_type
	string		ss_provider_sub_type
	string		ss_comment
end type

global type w_issue_reporting_fee from w_ancestor
integer x = 1289
integer y = 316
integer width = 2185
integer height = 1008
string menuname = ""
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
long backcolor = 67108864
cb_save cb_save
cb_cancel cb_cancel
dw_reporting_fees dw_reporting_fees
end type
global w_issue_reporting_fee w_issue_reporting_fee

type variables
s_reporting_fee_parameters is_reporting_fee_parameters
string 		is_mode, is_provider_name
DATASTORE	ids_payment_document

end variables

forward prototypes
public function integer wf_check_reporting_fee_received (long al_doc_id, long al_claim_no)
end prototypes

public function integer wf_check_reporting_fee_received (long al_doc_id, long al_claim_no);//
// This function will validate that the original reporting fee has been received either
// through a cancel or adjust payment. If cancel or adjustment is made, the zeroed flag 
// is set to yes.
//
// Returns:  0 - reporting fee received
//           1 - reporting fee not received
//          -1 - Error
//	             

STRING	ls_zeroed_flag
LONG		ll_doc_id, ll_payment_no

// Retrieve the last reporting fee payment made. Could be several (i.e. one reporting fee
// and several reporting fee re-issues). 
// 
// Note, We don't want to check for claim # or provider # because they could change 
//       through re-indexing and no record would be found. 
//

select b.doc_id, max(a.payment_no)
into   :ll_doc_id, :ll_payment_no
from   PAYMENT a, PAYMENT_DOCUMENT b
where  b.doc_id = :al_doc_id
and    a.payment_no = b.payment_no
and    a.payment_type_code = '21'
and    a.payment_sub_type_code in('03','04','05','06','08','13','14','15','16','18')
group by b.doc_id
using SQLCA;
	  
IF sqlca.nf_handle_error("SELECT max(payment_no)","w_issue_reporting_fee","wf_check_reporting_fee_received") < 0 THEN
	RETURN -1
END IF

IF SQLCA.SQLCODE = 100 THEN
	   Return -1  // If nothing found, discrepency exists - severe error
END IF

// Check if the last payment made for document was canceled or adjusted
//
select zeroed_flag
into   :ls_zeroed_flag
from   PAYMENT
where  payment_no = :ll_payment_no
using SQLCA;
	  
IF sqlca.nf_handle_error("SELECT zeroed_flag","w_issue_reporting_fee","wf_check_reporting_fee_received") < 0 THEN
	RETURN -1
END IF

CHOOSE CASE SQLCA.SQLCODE
	CASE 100    // If nothing found, discrepency exists - severe error
		Return -1
	CASE 0
		IF ls_zeroed_flag = "Y" THEN
			Return 0
		ELSE
			Return 1
		END IF
END CHOOSE

	
end function

on w_issue_reporting_fee.create
int iCurrent
call super::create
this.cb_save=create cb_save
this.cb_cancel=create cb_cancel
this.dw_reporting_fees=create dw_reporting_fees
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_save
this.Control[iCurrent+2]=this.cb_cancel
this.Control[iCurrent+3]=this.dw_reporting_fees
end on

on w_issue_reporting_fee.destroy
call super::destroy
destroy(this.cb_save)
destroy(this.cb_cancel)
destroy(this.dw_reporting_fees)
end on

event open;call super::open;/* Manually Identified Doc's Eligible for reporting fee
  ************This code should probably be moved to calling window

  before  a document can be forwarded (before opening w_issue_reporting_fee - can be open
       event of window)
  1. The claim must be selected
  2. A document in the document list must be highlighted
  3. The claim's status  must be 'Rejected - Disallowed' or 'Rejected - No Claim'
  4. If the claim's status is 'Rejected - No Claim' the document type must be one of 'SDD' or 'MPD' or 'SDC''
  5. If the claim's status is 'Rejected - Disallowed' the document must be one of 'SDD','MPD' or 'AD' or 'SDC'
  6. The document cannot have been  previously identified as eligible for the reporting
     fee(docid cannot exist in ELIGIBLE_REPORT_FEE_DOCUMENTS)
  7. Document cannot be paid
  8. If the document is not eligible, display an error message and do not open the window
  9. SR-193: If the document belongs to an inactive service provider, display error message and do not open window
*/

STRING		ls_status_code,ls_status_type_code,ls_document_type,ls_paid_status,ls_provider_type, ls_active_flag
STRING		ls_provider_name, ls_provider_sub_type, ls_admin_region, ls_comment
STRING	   ls_eligibility_code, ls_eligibility_desc, ls_nbms_eligible_flag, ls_nbca_eligible_flag
LONG			ll_count, ll_provider_no, ll_elg_provider_no
datetime    ldt_accident_date, ldt_date_on_document
DATE         ld_accident_date_check, ld_document_date_check
INTEGER     li_answer, li_rc
DECIMAL		li_balance

/* grab the value from the instance of our structure the first thing to check is the claim number
*/
is_reporting_fee_parameters = Message.PowerObjectParm

IF IsNull(is_reporting_fee_parameters.claim_no) OR is_reporting_fee_parameters.claim_no = 0 THEN
	MessageBox("Reporting Fees","Cannot determine claim number",Exclamation!)
	Close(THIS)
	RETURN
END IF

/* now we will want to check the doc_id to make sure that it is at least a valid number
*/
IF IsNull(is_reporting_fee_parameters.doc_id) OR is_reporting_fee_parameters.doc_id < 0 THEN
	MessageBox("Reporting Fees","Cannot determine document number",Exclamation!)
	Close(THIS)
	RETURN
END IF

/* now do the claim status validations
*/
SELECT claim_status_code, claim_status_type_code, accident_date, admin_region_code
  INTO :ls_status_code, :ls_status_type_code,:ldt_accident_date, :ls_admin_region
  FROM CLAIM
 WHERE claim_no = :is_reporting_fee_parameters.claim_no
 USING SQLCA;

IF sqlca.nf_handle_error("SELECT claim_status_code","w_issue_reporting_fee","Open") < 0 THEN
	Close(THIS)
	RETURN
END IF

/* The provider must have a valid provider number
*/
 SELECT  service_provider_no,service_provider_type_code
   INTO :ll_provider_no,:ls_provider_type
   FROM DOCUMENT_INDEX   
  WHERE claim_no                       = :is_reporting_fee_parameters.claim_no
    AND docid                          = :is_reporting_fee_parameters.doc_id
  USING imagetrans;

IF imagetrans.nf_handle_error("w_issue_reporting_fee","Open - Event"," SELECT  service_provider_no") < 0 THEN
	Close(THIS)
	RETURN
END IF

IF ll_provider_no = 0 THEN
	MessageBox("Reporting Fees","This document does not have a provider assigned to receive the payment. "+&
	"~rPlease assign a provider to the document to be able to issue the Provider Reporting Fee payment.",Exclamation!)
	Close(THIS)
	RETURN
END IF

/* SR193 Start - The provider must have an active provider
	PR5584 - J. Hawker - Added join on provider_type_code as multiple provider types 
	         can have the same provider number. 
*/
 SELECT  active_flag, name, provider_sub_type_code, nbms_early_filing_bonus_flag, chiro_early_filing_bonus_flag
   INTO :ls_active_flag, :ls_provider_name, :ls_provider_sub_type, :ls_nbms_eligible_flag, :ls_nbca_eligible_flag
   FROM PROVIDER   
  WHERE provider_no                       = :ll_provider_no
  AND   provider_type_code                = :ls_provider_type
  USING sqlca;

IF sqlca.nf_handle_error("w_issue_reporting_fee","Open - Event"," SELECT  active_flag") < 0 THEN
	Close(THIS)
	RETURN
END IF

IF ls_active_flag = "N" THEN
	MessageBox("Reporting Fees","This document belongs to an inactive service provider. "+&
	"~rPlease assign the document to an active provider to be able to issue the Provider Reporting Fee payment.",Exclamation!)
	Close(THIS)
	RETURN
END IF

/* SR193 Stop 
*/

IF ls_provider_type <> "M" THEN
	MessageBox("Reporting Fees","This document does not have a Provider assigned to receive the payment. "+&
	"~rPlease assign a Service Provider with a Service provider type of 'M'",Exclamation!)
	Close(THIS)
	RETURN
END IF


SELECT type_code, date_on_document
  INTO :ls_document_type, :ldt_date_on_document
  FROM DOCUMENT_INDEX
 WHERE claim_no = :is_reporting_fee_parameters.claim_no
   AND docid    = :is_reporting_fee_parameters.doc_id
 USING imagetrans;

IF imagetrans.nf_handle_error("SELECT type_code","w_issue_reporting_fee","Open") < 0 THEN
	Close(THIS)
	RETURN
END IF

IF (ls_provider_sub_type = '04' AND ls_document_type <> 'SDC' ) THEN 
	MESSAGEBOX("Invalid doc type for the provider", "The provider assigned to this document is a Chiropractor but the document is not an 'SDC'." &
	+  "~r~n Therefore, this document is not eligible for a reporting fee. It may be necessary to correct either the document type or the service provider assigned to the document before a reporting fee can be created.", INFORMATION!)
	Close(THIS)
	RETURN 
END IF

IF (ls_provider_sub_type <> '04' AND ls_document_type = 'SDC' ) THEN 
	MESSAGEBOX("Invalid doc type for the provider", "The document type is an 'SDC' but the provider is not a Chiropractor." &
	+  "~r~n Therefore, this document is not eligible for a reporting fee. It may be necessary to correct either the document type or the service provider assigned to the document before a reporting fee can be created.", INFORMATION!)
	Close(THIS)
	RETURN
END IF

IF (ls_provider_sub_type = '04' AND ls_nbca_eligible_flag = 'N') THEN 
	MESSAGEBOX("Invalid status for the provider", "The provider assigned to this document is NOT an NBCA-eligible provider." &
	+  "~r~n Therefore, this document is not eligible for a reporting fee. It may be necessary to correct the service provider's eligibilty status before a reporting fee can be created.", INFORMATION!)
	Close(THIS)
	RETURN 
END IF

IF (ls_provider_sub_type <> '04' AND ls_nbms_eligible_flag = 'N' ) THEN 
	MESSAGEBOX("Invalid status for the provider", "The provider assigned to this document is NOT an NBMS-eligible provider." &
	+  "~r~n Therefore, this document is not eligible for a reporting fee. It may be necessary to correct the service provider's eligibilty status before a reporting fee can be created.", INFORMATION!)
	Close(THIS)
	RETURN
END IF

/* see if the accident date > 1999-06-01 for SDD, MPD, AD document types or > 2006-03-01 for SDC document types
*/

IF ls_document_type = 'SDC' THEN
	ld_accident_date_check = 2006-03-01   //BR 1.65
ELSEIF ls_document_type = 'SDD' OR ls_document_type = 'MPD' OR ls_document_type = 'AD' THEN
	ld_accident_date_check = 1999-06-01   //BR 1.60
ELSE
	ld_accident_date_check = 1900-01-01
END IF

IF date(ldt_accident_date) <  ld_accident_date_check THEN
	MessageBox("Reporting Fees","Accident date cannot be earlier than " + STRING(ld_accident_date_check) + ".",Exclamation!)
	Close(THIS)
	RETURN
END IF

/* see if we have a valid document type
*/
IF isnull(ls_document_type) OR trim(ls_document_type) = "" THEN
	MessageBox("Reporting Fees","Cannot determine document type",Exclamation!)
	Close(THIS)
	RETURN
END IF

/* Now we need to check for the variations as illustrated in the header comment
*/
IF ls_status_type_code <> "07" AND ls_status_type_code <> "09" THEN
	MessageBox("Reporting Fees","The claim status is not valid for Provider Reporting Fee Payments. " +&
	           "~rThe status must be 'Rejected - Disallowed' or 'Rejected - No Claim'",Exclamation!)
	Close(THIS)
	RETURN
END IF

//BR 1.30
IF ls_status_type_code = "09" THEN //the document must be SDD or MPD or 'SDC'
	IF ls_document_type <> "SDD" AND ls_document_type <> "MPD" AND ls_document_type <> "SDC" THEN
		MessageBox("Reporting Fees","If the claim status is 'Rejected - No Claim' then "+&
				     "~rthe document type must be either 'SDD' or 'MPD', or 'SDC'",Exclamation!)
		Close(THIS)
		RETURN
	END IF
	
	// PR 2795 - if the date on document is less than 1999-06-01 for 'SDD' and 2006-03-01 for SDC types, display error message
	//BR 1.70 and 1.75  check that the document date is within the proper time frame
	IF ls_document_type = 'SDD' then 
		ld_document_date_check = 1999-06-01   //BR 1.70
	ELSEIF ls_document_type = 'SDC' then 
		ld_document_date_check = 2006-03-01   // BR1.75
	ELSE
		ld_document_date_check = 1900-01-01
	END IF
	
	IF ldt_date_on_document < DateTime(ld_document_date_check) THEN
		MessageBox("Reporting Fees","This document ("+String(is_reporting_fee_parameters.doc_id)+") is too old (less than " + STRING(ld_document_date_check) + ") for a claim with Rejected/No Claim status"+&
		                                          " and therefore is not valid for Provider Reporting Fee Payments. ",Exclamation!)
		Close(THIS)
		RETURN
	END IF
END IF

//BR 1.40
IF ls_status_type_code = "07" THEN //the document must be SDD or MPD or AD
	IF ls_document_type <> "SDD" AND ls_document_type <> "MPD" AND ls_document_type <> "AD" AND ls_document_type <> "SDC"THEN
		MessageBox("Reporting Fees","If the claim status is 'Rejected - Disallowed' then "+&
				     "~rthe document type must be;'SDD','MPD' or 'AD' or 'SDC'",Exclamation!)
		Close(THIS)
		RETURN
	END IF
END IF


/* The document cannot have been  previously identified as eligible for the reporting
  fee(docid cannot exist in ELIGIBLE_REPORT_FEE_DOCUMENTS)
*/

// SR193 Provider Redesign - This section will determine wether to issue or re-issue a reporting
//                       fee based on if it was ever paid.
//
SELECT a.reporting_fee_eligibility_code, a.recipient_no, b.reporting_fee_eligibility_desc
INTO :ls_eligibility_code, :ll_elg_provider_no, :ls_eligibility_desc
FROM ELIGIBLE_REPORT_FEE_DOCUMENTS a, Reporting_Fee_Eligibility b
WHERE a.docid = :is_reporting_fee_parameters.doc_id
and b.reporting_fee_eligibility_code = a.reporting_fee_eligibility_code
USING SQLCA;

IF SQLCA.nf_handle_error("SELECT reporting_fee_eligibility_code INTO :ls_eligibility_code","w_issue_reporting_fee","Open") < 0 THEN
	Close(THIS)
	RETURN
END IF

CHOOSE CASE SQLCA.SQLCode 
	CASE 100   // Document never identified has eligible (not found); ok to issue rep. fee
		is_mode = "ISSUE"		
	CASE 0     // Document found; check status
   	IF ls_eligibility_code <> "PD" THEN 
	  	 	MessageBox("Reporting Fees","This document has already been identified as eligible for the Provider reporting fee. It's current status is " + ls_eligibility_desc,Exclamation!)
 	  	 	Close(THIS)
	  	 	RETURN
		ELSE
			li_answer = MessageBox("Reporting Fees","This document has already been paid a reporting fee. Do you want to re-issue one?",Question!,YESNO!,1)
 	  
		   IF li_answer = 1 THEN
				
				// Check if reporting fee has been returned and received
				//
				li_rc = wf_check_reporting_fee_received(is_reporting_fee_parameters.doc_id, is_reporting_fee_parameters.claim_no)
				CHOOSE CASE li_rc
					CASE IS < 0       // severe error
						MessageBox("Reporting Fees","Problem determining if the reporting fee was returned. Cannot re-issue.",Exclamation!)
						Close(THIS)
	  	 				RETURN
					CASE IS > 0       // money not received
						MessageBox("Reporting Fees","A Reporting Fee cannot be re-issued until it is canceled or adjusted",Exclamation!)
						Close(THIS)
						Return
				   CASE 0         // money received
						is_mode = "REISSUE"  // Document has been paid
		
						// Set default comments for updating purposes
						//	
						IF ll_provider_no = ll_elg_provider_no THEN
							ls_comment = "Reporting Fee Re-issued"
						ELSE
							ls_comment = "Reporting Fee Re-issued to new provider no " + String(ll_provider_no) &
								+ " ; old provider no " + String(ll_elg_provider_no)
						END IF
				END CHOOSE
			ELSE
				Close(THIS)
				RETURN
			END IF
		END IF
END CHOOSE

	
/* If all goes well retrieve our datawindow with the appropriate values based on claim number and document id
*/
dw_reporting_fees.settransobject(sqlca)
ll_count = dw_reporting_fees.retrieve(is_reporting_fee_parameters.claim_no,is_reporting_fee_parameters.doc_id)

IF sqlca.nf_handle_error("dw_reporting_fees.retrieve","w_issue_reporting_fee","Open") < 0 THEN
	Close(THIS)
	RETURN
END IF

IF ll_count <= 0 THEN
	 MessageBox("Reporting Fees","Problem retrieving documents.",Exclamation!)
	 Close(THIS)
	 RETURN
END IF


// Display default comments when re-issuing a reporting fee 
//
IF is_mode = "REISSUE" THEN
	dw_reporting_fees.SetItem(1,"c_comment", ls_comment)
	cb_save.enabled = TRUE
END IF

 /* Now lets set focus to the datawindow 
 */
dw_reporting_fees.setfocus()

end event

type cb_save from commandbutton within w_issue_reporting_fee
integer x = 1847
integer y = 716
integer width = 274
integer height = 108
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
end type

event clicked;/* Information for this POPUP window for project 10127
   1.There is to be two buttons - Save and Cancel. Save button is disabled by default.
	2. A comment must be entered
	3. Clicking the cancel button closes the pop-up window without any database updates.
	4. The Save button returns control after a record is inserted into 
	   ELIGIBLE_REPORT_FEE_DOCUMENTS (see table e for details Phase 2 document)
		 	 
*/

INT			li_rowcount,li_check_box,li_row,li_error, li_rtn
DATASTORE	lds_eligibility_report
LONG			ll_docid,ll_claim_no,ll_provider_no
DATETIME		ldt_current
STRING		ls_comment,ls_type_code,ls_provider_type,ls_provider_sub_type
N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '018' refers to the Send for NBMS/NBCA Reporting Fee Payment module, '044' refers to the Payment Processing module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('018','044','save',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF
/******************************************************************************************/

/* set the pointer to an hourglass
*/
setpointer(hourglass!)

li_rowcount = dw_reporting_fees.rowcount() 
IF li_rowcount < 1 THEN
	RETURN
END IF
	
/* now check that there is a comment entered
*/
ls_comment = dw_reporting_fees.getitemstring(1,"c_comment")

IF LEN(TRIM(ls_comment)) > 0 AND NOT isnull(ls_comment) THEN
ELSE
	messagebox("Eligible Reporting Fees","A valid comment needs to be entered before proceeding!")
	RETURN
END IF	
	
//grab the current datetime/null string
ldt_current = f_server_datetime()

//grab the values from the datawindow that we will insert into our datastore
ll_docid             = dw_reporting_fees.getitemnumber(1,"docid")
ll_claim_no          = dw_reporting_fees.getitemnumber(1,"claim_no")
ll_provider_no       = dw_reporting_fees.getitemnumber(1,"provider_no")
ls_provider_type     = dw_reporting_fees.getitemstring(1,"provider_type_code")
ls_provider_sub_type = dw_reporting_fees.getitemstring(1,"provider_sub_type_code")

SELECT claim_status_type_code
INTO  :ls_type_code
FROM  CLAIM
WHERE claim_no = :ll_claim_no ;

SQLCA.nf_handle_error("SELECT claim_status_type_code...","w_issue_reporting_fee","cb_save_click")

//check that we have a valid value for the type code
IF ISNULL(ls_type_code) OR len(trim(ls_type_code)) < 1 THEN
	messagebox("Eligible Reporting Fees","An error occured while selecting the status code. Please cancel transaction!")
	RETURN
END IF	
	
/* If the claim's status is rejected - No Claim' then use 'NC'.
   If the claim's status is rejected - dissallowed', then use 'DC'.
	These fields coorespond to the following claim_status_type_code
	09 = Rejected no claim
	07 = Rejected Disallowed
*/
IF ls_type_code = "09" THEN
	ls_type_code = "NC"
ELSEIF ls_type_code = "07" THEN
	ls_type_code = "DC"
ELSE
	//what do we do if it is neither of these
	RETURN
END IF


SQLCA.nf_begin_transaction()

// SR193 NBMS Redesign  
// IN ISSUE MODE - Reporting fee never paid, create eligible record 
// IN REISSUE MODE - Reporting fee already been paid, reissue by changing the status to 'APR'

IF is_mode = "REISSUE" THEN
	// Update Eligibility record 
	
   UPDATE ELIGIBLE_REPORT_FEE_DOCUMENTS
	SET 	comment 			= :ls_comment, 
			recipient_no 	= :ll_provider_no,
			reporting_fee_eligibility_code = 'APR',
			approved_user_id = :vgst_user_profile.user_id,
			approved_date = GetDate()
	WHERE docid = :ll_docid
	USING SQLCA;
	
	SQLCA.nf_handle_error("UPDATE ELIGIBLE_REPORT_FEE_DOCUMENTS...","w_issue_reporting_fee","cb_save_click")
ELSE
	
	//set up the datastore for input
	lds_eligibility_report = CREATE DATASTORE
	lds_eligibility_report.DataObject = 'd_eligible_report_fee_payments'
	lds_eligibility_report.SetTransObject(SQLCA)
	
	/* at this point we need to insert a record into the following table
	   ELIGIBLE_REPORT_FEE_PAYMENTS this will be later updated.
	*/
	
	li_row = lds_eligibility_report.insertrow(0)
	lds_eligibility_report.setitem(li_row,"docid",ll_docid)
	lds_eligibility_report.setitem(li_row,"claim_no",ll_claim_no)
	lds_eligibility_report.setitem(li_row,"type_code",ls_type_code)
	lds_eligibility_report.setitem(li_row,"generated_method_code","M")
	lds_eligibility_report.setitem(li_row,"approved_date",ldt_current)
	lds_eligibility_report.setitem(li_row,"approved_user_id",vgst_user_profile.user_id)
	lds_eligibility_report.setitem(li_row,"comment",ls_comment)
	lds_eligibility_report.setitem(li_row,"eligibility_code","APP")
	lds_eligibility_report.setitem(li_row,"recipient_no",ll_provider_no)
	lds_eligibility_report.setitem(li_row,"recipient_type_code",ls_provider_type)
	lds_eligibility_report.setitem(li_row,"recipient_sub_type_code",ls_provider_sub_type)

	IF lds_eligibility_report.rowcount() > 0 THEN	
		li_error = lds_eligibility_report.update()
		IF li_error < 1 THEN
			SignalError(-666, "Error updating ELIGIBILITY_REPORT_FEE_DOCUMENTS in cb_save for w_issue_reporting_fee")			
		END IF
	END IF
END IF

SQLCA.nf_commit_transaction()


CLOSE(PARENT)
	
end event

type cb_cancel from commandbutton within w_issue_reporting_fee
integer x = 1541
integer y = 716
integer width = 274
integer height = 108
integer taborder = 10
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Cancel"
boolean default = true
end type

event clicked;CLOSE(PARENT)
end event

type dw_reporting_fees from u_dw_online within w_issue_reporting_fee
event ue_view ( )
integer x = 23
integer y = 28
integer width = 2103
integer height = 664
integer taborder = 20
boolean bringtotop = true
string dataobject = "d_issue_reporting_fee"
borderstyle borderstyle = stylelowered!
end type

event itemchanged;call super::itemchanged;/* There is to be two buttons - Save and Cancel. The save button is disabled by default
*/

INT			li_check,li_counter
STRING		ls_comment

/* Check that the text entered into the comment field is valid
   if it is then we can enable the command button elas we disable it.
*/
CHOOSE CASE  dwo.name
	CASE "c_comment"
		ls_comment = gettext()
		IF len(trim(ls_comment)) > 0 THEN
			cb_save.enabled = TRUE
		ELSE
			cb_save.enabled = FALSE
		END IF
	CASE ELSE
END CHOOSE
end event

event editchanged;call super::editchanged;/* There is to be two buttons - Save and Cancel. The save button is disabled by default
*/

INT			li_check,li_counter
STRING		ls_comment

/* Check that the text entered into the comment field is valid
   if it is then we can enable the command button elas we disable it.
*/
CHOOSE CASE  dwo.name
	CASE "c_comment"
		ls_comment = gettext()
		IF len(trim(ls_comment)) > 0 THEN
			cb_save.enabled = TRUE
		ELSE
			cb_save.enabled = FALSE
		END IF
	CASE ELSE
END CHOOSE
end event

event losefocus;call super::losefocus;/* make sure the comment is accepted
*/
this.accepttext()
end event

event retrieveend;call super::retrieveend;
datawindowchild ldw_child
this.getChild('provider_sub_type_code',ldw_child)
ldw_child.setFilter("provider_type_code = 'M'")
ldw_child.filter()
end event

