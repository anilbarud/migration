$PBExportHeader$w_issue_reporting_fee_lite.srw
$PBExportComments$Lite version of the origional window used to reset claims to be eligible for the reporting fee
forward
global type w_issue_reporting_fee_lite from w_ancestor
end type
type cb_save from commandbutton within w_issue_reporting_fee_lite
end type
type cb_cancel from commandbutton within w_issue_reporting_fee_lite
end type
type dw_reporting_fees from u_dw_online within w_issue_reporting_fee_lite
end type
end forward

global type w_issue_reporting_fee_lite from w_ancestor
integer x = 1289
integer y = 316
integer width = 2149
integer height = 872
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
global w_issue_reporting_fee_lite w_issue_reporting_fee_lite

type variables
s_reporting_fee_parameters is_reporting_fee_parameters

end variables

on w_issue_reporting_fee_lite.create
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

on w_issue_reporting_fee_lite.destroy
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
  4. If the claim's status is 'Rejected - Disallowed' the document type must be one of 'SDD' or 'MPD'
  5. If the claim's status is 'Rejected - Disallowed' the document must be one of 'SDD','MPD' or 'AD'
  6. The document cannot have been  previously identified as eligible for the reporting
     fee(docid cannot exist in ELIGIBLE_REPORT_FEE_DOCUMENTS)
  7. Document cannot be paid
  8. If the document is not eligible, display an error message and do not open the window

*/

STRING		ls_status_code,ls_status_type_code,ls_document_type,ls_paid_status,ls_provider_type
STRING       ls_active_flag, ls_provider_name, ls_provider_sub_type, ls_nbms_eligible_flag, ls_nbca_eligible_flag
LONG			ll_count,ll_provider_no
datetime     ldt_accident_date
DATE          ld_accident_date_check

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
SELECT claim_status_code, claim_status_type_code, accident_date
  INTO :ls_status_code, :ls_status_type_code,:ldt_accident_date
  FROM CLAIM
 WHERE claim_no = :is_reporting_fee_parameters.claim_no
 USING SQLCA;

IF sqlca.nf_handle_error("SELECT claim_status_code","w_issue_reporting_fee","Open") < 0 THEN
	Close(THIS)
	RETURN
END IF

/* The document must have a valid provider number
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
	MessageBox("Reporting Fees","This document does not have a Provider assigned to receive the payment. "+&
	"~rPlease assign a provider to the document to be able to issue the NBMS/NBCA Reporting Fee payment.",Exclamation!)
	Close(THIS)
	RETURN
END IF


IF ls_provider_type <> "M" THEN
	MessageBox("Reporting Fees","This document does not have a Provider assigned to receive the payment. "+&
	"~rPlease assign a Service Provider with a Service provider type of 'M'",Exclamation!)
	Close(THIS)
	RETURN
END IF

SELECT type_code
  INTO :ls_document_type
  FROM DOCUMENT_INDEX
 WHERE claim_no = :is_reporting_fee_parameters.claim_no
   AND docid    = :is_reporting_fee_parameters.doc_id
 USING imagetrans;

IF imagetrans.nf_handle_error("SELECT type_code","w_issue_reporting_fee","Open") < 0 THEN
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

// check for valid accident date

//T017023 - Sept3, 2015 - R.S  added a variable to hold the appropriate date since there are two dates now, one for SDC and one for SDD, MPD, AD types
IF ls_document_type = 'SDC' THEN
	ld_accident_date_check = 2006-03-01   //BR 1.65
ELSEIF ls_document_type = 'SDD' OR ls_document_type = 'MPD' OR ls_document_type = 'AD' THEN
	ld_accident_date_check = 1999-06-01   //BR 1.60
ELSE
	ld_accident_date_check = 1900-01-01  // fall back
END IF

IF date(ldt_accident_date) <  ld_accident_date_check THEN
	MessageBox("Reporting Fees","Accident date cannot be earlier than " + STRING(ld_accident_date_check) + ".",Exclamation!)
	Close(THIS)
	RETURN
END IF


 SELECT  active_flag, name, provider_sub_type_code, nbms_early_filing_bonus_flag, chiro_early_filing_bonus_flag
   INTO :ls_active_flag, :ls_provider_name, :ls_provider_sub_type, :ls_nbms_eligible_flag, :ls_nbca_eligible_flag
   FROM PROVIDER   
  WHERE provider_no                       = :ll_provider_no
  AND   provider_type_code                = :ls_provider_type
  USING sqlca;

IF sqlca.nf_handle_error("w_issue_reporting_fee_lite","Open - Event"," SELECT  active_flag") < 0 THEN
	Close(THIS)
	RETURN
END IF

// now check for document type is correct for provider type and that the provider is either NBMS or NBCA eligible
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


/* Now we need to check for the variations as illustrated in the header comment        /////////  T017023 - Sept3, 2015 - R.S. - not sure why, but this code was commented out before i got here
*/
//IF ls_status_type_code <> "07" AND ls_status_type_code <> "09" THEN
//	MessageBox("Reporting Fees","The claim status is not valid for NBMS Reporting Fee Payments. " +&
//	           "~rThe status must be 'Rejected - Disallowed' or 'Rejected - No Claim'",Exclamation!)
//	Close(THIS)
//	RETURN
//END IF

//IF ls_status_type_code = "09" THEN//the document must be SDD or MPD
//	IF ls_document_type <> "SDD" AND ls_document_type <> "MPD" THEN
//		MessageBox("Reporting Fees","If the claim status is 'Rejected - No Claim' then "+&
//				     "~rthe document type must be either 'SDD' or 'MPD'",Exclamation!)
//		Close(THIS)
//		RETURN
//	END IF
//END IF

//IF ls_status_type_code = "07" THEN//the document must be SDD or MPD or AD
//	IF ls_document_type <> "SDD" AND ls_document_type <> "MPD" AND ls_document_type <> "AD"THEN
//		MessageBox("Reporting Fees","If the claim status is 'Rejected - Disallowed' then "+&
//				     "~rthe document type must be;'SDD','MPD' or 'AD'",Exclamation!)
//		Close(THIS)
//		RETURN
//	END IF
//END IF
//
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
 /* Now lets set focus to the datawindow 
 */
dw_reporting_fees.setfocus()


end event

type cb_save from commandbutton within w_issue_reporting_fee_lite
integer x = 1847
integer y = 656
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

INT			li_rowcount
LONG			ll_docid,ll_claim_no
STRING		ls_comment,ls_type_code

/* set the pointer to an hourglass
*/
setpointer(hourglass!)

li_rowcount = dw_reporting_fees.rowcount() 
IF li_rowcount < 1 THEN
	RETURN
END IF
	
/* now check that there is a comment entered
*/
ll_claim_no = dw_reporting_fees.getitemnumber(1,"claim_no")
ls_comment  = dw_reporting_fees.getitemstring(1,"c_comment")

IF LEN(TRIM(ls_comment)) > 0 AND NOT isnull(ls_comment) THEN
ELSE
	messagebox("Eligible Reporting Fees","A valid comment needs to be entered before proceeding!")
	RETURN
END IF	

SELECT claim_status_type_code
INTO  :ls_type_code
FROM  CLAIM
WHERE claim_no = :ll_claim_no ;

IF SQLCA.nf_handle_error("SELECT claim_status_type_code...","w_issue_reporting_fee","cb_save_click") < 0 THEN
	Return -1
END IF

//check that we have a valid value for the type code
IF ISNULL(ls_type_code) OR len(trim(ls_type_code)) < 1 THEN
	messagebox("Eligible Reporting Fees","An error occured while selecting the status code. Please cancel transaction!")
	RETURN
END IF	

CLOSEWITHRETURN(PARENT,ls_comment)
	
end event

type cb_cancel from commandbutton within w_issue_reporting_fee_lite
integer x = 1541
integer y = 656
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

event clicked;
CLOSEWITHRETURN(PARENT,"")
end event

type dw_reporting_fees from u_dw_online within w_issue_reporting_fee_lite
event ue_view ( )
integer x = 23
integer y = 28
integer width = 2103
integer height = 612
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

