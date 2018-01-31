$PBExportHeader$u_document_index.sru
forward
global type u_document_index from userobject
end type
type cb_provider_search from commandbutton within u_document_index
end type
type dw_document_indexing from u_dw_online within u_document_index
end type
end forward

global type u_document_index from userobject
integer width = 2171
integer height = 776
long backcolor = 67108864
long tabtextcolor = 33554432
cb_provider_search cb_provider_search
dw_document_indexing dw_document_indexing
end type
global u_document_index u_document_index

type variables
DATAWINDOWCHILD	idwc_document_type
DATAWINDOWCHILD	idwc_recipient_types
DATAWINDOWCHILD	idwc_class_code
//DATAWINDOWCHILD	idwc_type_code
DATAWINDOWCHILD	idwc_subtype_code
N_IMAGING		inv_imaging
N_ACCOUNT_PAYMENT_CONTROLLER	inv_account_payment
N_REFERRAL    inv_referral
LONG			il_master_fldid
LONG			il_old_claim_no
BOOLEAN		ib_suppress_pbmessage
BOOLEAN		ib_knowsheet
BOOLEAN		ib_restrict_type_code_changes = True
BOOLEAN		ib_paid_document, ib_correspondence
W_SHEET		iw_sheet

constant Date ID_NBMS_1_START = Date('1999-06-01')
constant Date ID_NBMS_2_START = Date('2005-06-01')
constant Date ID_CHIRO_START  = Date('2006-03-01')
end variables

forward prototypes
public subroutine uf_date_received (boolean ab_protect)
public function integer uf_set_claim (long al_claim_no)
public function integer uf_count_chiro_early_filing_bonus (long al_doc_id, ref integer ai_count, ref string as_payment_sub_type_code, string as_check_type)
public function integer uf_retrieve (long al_docid, ref boolean ab_already_indexed)
public function integer uf_count_nbms_early_filing_bonus (long al_doc_id, ref integer ai_count, ref string as_payment_sub_type_code, string as_check_type)
public function integer uf_check_chiro_doc_type_change (long al_doc_id, string as_new_doc_type_code, string as_check_type)
public function integer uf_check_nbms_1_doc_type_change (long al_doc_id, string as_new_doc_type_code, string as_check_type)
public function integer uf_check_nbms_2_doc_type_change (long al_doc_id, string as_new_doc_type_code, string as_check_type)
public function integer uf_validate_chiro_efb (long al_docid)
public subroutine uf_get_original_dates (long al_docid, ref datetime adt_date_on_document, ref datetime adt_date_received)
public function integer uf_validate_nbms_efb (long al_docid)
public function integer uf_check_chiro_doc_date_change (long al_doc_id, datetime adt_new_date_on_document, datetime adt_new_date_received, string as_payment_sub_type_code, string as_check_type)
public function integer uf_check_nbms_2_doc_date_change (long al_doc_id, datetime adt_new_date_on_document, datetime adt_new_date_received, string as_payment_sub_type_code, string as_check_type)
public function integer uf_check_nbms_1_doc_date_change (long al_doc_id, datetime adt_new_date_on_document, datetime adt_new_date_received, string as_payment_sub_type_code, string as_check_type)
public function integer uf_check_required (ref string as_type, ref datetime adt_date)
end prototypes

public subroutine uf_date_received (boolean ab_protect);/* This ftn is used to protect or unprotect the column date_received.
*/
	IF ab_protect THEN
		dw_document_indexing.Modify("date_received.Visible=0")
		dw_document_indexing.Modify("date_received.Protect=1")
		dw_document_indexing.Modify("date_received_t.Visible=0")
	ELSE
		dw_document_indexing.Modify("date_received.Visible=1")
		dw_document_indexing.Modify("date_received.Protect=0")
		dw_document_indexing.Modify("date_received_t.Visible=1")
	END IF

end subroutine

public function integer uf_set_claim (long al_claim_no);LONG		ll_result, ll_position, ll_row, ll_docid, ll_doc_row, ll_orig_claim_no
STRING	ls_title, ls_module_source_code, ls_lost_time_flag, ls_referral_flag

/* Validate the claim number the user has entered and get the claim's master folder.
*/
	il_master_fldid = 0

/*	If the active sheet is not known, then abort.
*/
	IF not ib_knowsheet THEN
		RETURN -1
	END IF

	ll_result = iw_sheet.wf_set_claim(al_claim_no)

/* Anything but 1 means the document can't be indexed to this claim.
*/
	IF ll_result <> 1 THEN
		IF ll_result = -1 THEN
			MessageBox("History Claim","This is a History Claim and cannot be used for Indexing.",Exclamation!)
		END IF
		RETURN -1
	END IF
	
/* Make sure that this is a imaged claim.
*/
	IF iw_sheet.dw_basic_claim.GetItemString(1,"imaged_flag") <> "Y" THEN
		MessageBox('Non-Imaged Claim','This claim is not imaged. Indexing to this claim is not allowed.',Exclamation!)
		RETURN -1
	END IF

/*	If the claim's status type is '05' - out of province then just give a warning.
*/
	IF iw_sheet.dw_basic_claim.GetItemString(1,"claim_status_type_code") = '05' THEN
		MessageBox("Claim Status","Please Note: This claim has a status type of Out of Province.",Exclamation!)
	END IF
	

/* Get the master folderid for this claim.
*/
	SELECT folderid INTO :il_master_fldid
	  FROM CLAIM_MASTER
  	 WHERE claim_no = :al_claim_no
	 USING ImageTrans;

	IF ImageTrans.nf_handle_error("Embedded SQL: Select from CLAIM_MASTER","u_document_index","uf_set_claim") < 0 THEN
		RETURN -1
	END IF

	IF NOT il_master_fldid > 0 THEN
		RETURN -1
	END IF

/* If the claim's status is pre-adjudication and the claim is unassigned, then show the lost-time field.
*/
	IF iw_sheet.dw_basic_claim.GetItemString(1,"claim_status_code") = "P" AND &
		iw_sheet.dw_basic_claim.GetItemString(1,"claim_manager_user_id") = "" THEN
		
		ll_doc_row	= dw_document_indexing.getrow()
		ll_docid 		= dw_document_indexing.GetItemNumber(ll_doc_row, "docid")
		
		dw_document_indexing.Modify("lost_time.Visible=1")
		dw_document_indexing.Modify("lost_time.Protect=0")
		dw_document_indexing.Modify("lost_time_t.Visible=1")
		
		IF  ll_docid > 0  THEN 
			
			SELECT 	module_source_code 
			INTO    	:ls_module_source_code
			FROM   	DOC
			WHERE 	docid = :ll_docid
			USING	ImageTrans;
		 
		  ImageTrans.nf_handle_error("Embedded SQL: SELECT module_source_code ","u_document_index","uf_set_claim")
		
			IF  ls_module_source_code =  "13" OR ls_module_source_code =  "15" THEN
				
				IF ls_module_source_code =  "13" THEN
					
					SELECT 	lost_time_flag		
					INTO 		:ls_lost_time_flag 
					FROM    	CLAIM..I009_FORM67_IMPORT 
					WHERE 	docid = :ll_docid ;
		
					SQLCA.nf_handle_error("u_document_index", "dw_document_indexing", "uf_set_claim - select lost time flag")	
				
				ELSE
					
					SELECT 	lost_time_flag		
					INTO 		:ls_lost_time_flag 
					FROM    	CLAIM..WEB_FORM67 
					WHERE 	docid = :ll_docid ;
		
					SQLCA.nf_handle_error("u_document_index", "dw_document_indexing", "uf_set_claim - select lost time flag from WEB_FORM67 ")	
						
				END IF 
		
				// set the lost time flag based on the above
				IF ls_lost_time_flag = 'Y' THEN dw_document_indexing.setitem(ll_doc_row,'lost_time','Y')
				
				dw_document_indexing.Modify("lost_time.protect=1  lost_time.Background.Color='553648127'")
				
			END IF
		END IF 
		
	ELSE
		dw_document_indexing.Modify("lost_time.Visible=0")
		dw_document_indexing.Modify("lost_time.Protect=1")
		dw_document_indexing.Modify("lost_time_t.Visible=0")
	END IF

	RETURN 1

end function

public function integer uf_count_chiro_early_filing_bonus (long al_doc_id, ref integer ai_count, ref string as_payment_sub_type_code, string as_check_type);/*

Function will return count of NB Chiro Early Filing Bonus payments (scheduled or processed)

args: al_doc_id = document #
      ai_count = reference argument
		as_payment_sub_type_code = reference argument
      as_check_type indicates what type of check is being performed,
      a warning check or an error check
		
*/

INTEGER  li_count1, li_count2

IF as_check_type = 'error' THEN
	SELECT b.payment_sub_type_code, Count(*)
	INTO   :as_payment_sub_type_code, :li_count1
	FROM   UNAPPLIED_CLAIM_TXN a,
			 PAYMENT             b,
			 PAYMENT_DOCUMENT    c
	WHERE  a.payment_no            = b.payment_no
	AND    b.payment_no            = c.payment_no
	AND    c.doc_id                = :al_doc_id
	AND    b.payment_type_code     = '21'
	AND    b.payment_sub_type_code = '09'
	GROUP BY b.payment_sub_type_code
	USING SQLCA;
	
	SQLCA.nf_handle_error("u_document_index", "uf_check_Chiro_early_filing_bonus", "SELECT count(*) FROM UNAPPLIED_CLAIM_TXN...(09)") 
		
	SELECT b.payment_sub_type_code, Count(*)
	INTO   :as_payment_sub_type_code, :li_count2
	FROM   UNAPPLIED_CLAIM_TXN a,
			 PAYMENT             b,
			 PAYMENT_DOCUMENT    c
	WHERE  a.payment_no            = b.payment_no
	AND    b.payment_no            = c.payment_no
	AND    c.doc_id                = :al_doc_id
	AND    b.payment_type_code     = '21'
	AND    b.payment_sub_type_code = '10'
	GROUP BY b.payment_sub_type_code
	USING SQLCA;
	
	SQLCA.nf_handle_error("u_document_index", "uf_check_Chiro_early_filing_bonus", "SELECT count(*) FROM UNAPPLIED_CLAIM_TXN...(10)") 
	
ELSEIF as_check_type = 'warning' THEN
	SELECT b.payment_sub_type_code, Count(*)
	INTO   :as_payment_sub_type_code, :li_count1
	FROM   APPLIED_CLAIM_TXN   a,
			 PAYMENT             b,
			 PAYMENT_DOCUMENT    c
	WHERE  a.payment_no            = b.payment_no
	AND    b.payment_no            = c.payment_no
	AND    c.doc_id                = :al_doc_id
	AND    b.payment_type_code     = '21'
	AND    b.payment_sub_type_code = '09'
	GROUP BY b.payment_sub_type_code
	USING SQLCA;
	
	SQLCA.nf_handle_error("u_document_index", "uf_check_Chiro_early_filing_bonus", "SELECT count(*) FROM APPLIED_CLAIM_TXN...(09)") 
	
	SELECT b.payment_sub_type_code, Count(*)
	INTO   :as_payment_sub_type_code, :li_count2
	FROM   APPLIED_CLAIM_TXN   a,
			 PAYMENT             b,
			 PAYMENT_DOCUMENT    c
	WHERE  a.payment_no            = b.payment_no
	AND    b.payment_no            = c.payment_no
	AND    c.doc_id                = :al_doc_id
	AND    b.payment_type_code     = '21'
	AND    b.payment_sub_type_code = '10'
	GROUP BY b.payment_sub_type_code
	USING SQLCA;
	
	SQLCA.nf_handle_error("u_document_index", "uf_check_Chiro_early_filing_bonus", "SELECT count(*) FROM APPLIED_CLAIM_TXN...(10)") 
	

END IF

IF li_count1 > 0 THEN
	IF li_count2 > 0 THEN
		MessageBox('Data Integrity Error','There are automated and manual NB Chiro Early Filing Bonus'&
												+ '~npayments for this document. Please contact the HELPDESK.')
      RETURN -1
	ELSE
		ai_count = li_count1
	END IF
ELSE
	// if li_count1 = 0 then return whatever value li_count2 has
	ai_count = li_count2
END IF

RETURN 0
end function

public function integer uf_retrieve (long al_docid, ref boolean ab_already_indexed);LONG		ll_result, ll_claim_no, ll_sender
STRING	ls_sender, ls_sender_type, ls_class_code, ls_doc_type_code
LONG		ll_rows, ll_subtype

//	dw_document_indexing.GetChild("type_code",idwc_type_code)
//	idwc_type_code.SetTransObject(ImageTrans)
//	
//	ll_rows = idwc_type_code.Retrieve()
//	ImageTrans.nf_handle_error('u_document_index','constructor','	idwc_type_code.Retrieve()')
//	
//	If ll_rows <= 0 Then SIgnalError(-666,'Error retrieving document types.')
//

/*	This function checks to see if the document is already indexed.
*/
	il_old_claim_no = 0

	ll_result = dw_document_indexing.Retrieve(al_docid)
	IF ImageTrans.nf_handle_error("dw_document_indexing","u_document_index","uf_retrieve for u_document_index") < 0 THEN
		RETURN -1
	END IF

	IF ll_result > 0 THEN
		ab_already_indexed = TRUE

/* Get the claim number of the record.
*/
		ll_claim_no = dw_document_indexing.GetItemNumber(1,"claim_no")
		uf_set_claim(ll_claim_no)

/* Get the service provider name if the number is filled in.
*/
		ll_sender 		= dw_document_indexing.GetItemNumber(1,"service_provider_no")
		ls_sender_type = dw_document_indexing.GetItemString(1,"service_provider_type_code")
		IF ll_sender > 0 THEN
			ls_sender = inv_imaging.nf_service_provider_name(ll_sender,ls_sender_type,"u_document_index")
			dw_document_indexing.SetItem(1,"service_provider_name",ls_sender)
		END IF
		il_old_claim_no = dw_document_indexing.GetItemNumber(1,"claim_no")
		
	END IF

	RETURN ll_result

end function

public function integer uf_count_nbms_early_filing_bonus (long al_doc_id, ref integer ai_count, ref string as_payment_sub_type_code, string as_check_type);/*

Function will return count of NBMS Early Filing Bonus payments (scheduled or processed)

args: al_doc_id = document #
      ai_count = reference argument
		as_payment_sub_type_code = reference argument
      as_check_type indicates what type of check is being performed,
      a warning check or an error check
		
*/

INTEGER  li_count1, li_count2

IF as_check_type = 'error' THEN
	SELECT b.payment_sub_type_code, Count(*)
	INTO   :as_payment_sub_type_code, :li_count1
	FROM   UNAPPLIED_CLAIM_TXN a,
			 PAYMENT             b,
			 PAYMENT_DOCUMENT    c
	WHERE  a.payment_no            = b.payment_no
	AND    b.payment_no            = c.payment_no
	AND    c.doc_id                = :al_doc_id
	AND    b.payment_type_code     = '21'
	AND    b.payment_sub_type_code = '01'
	GROUP BY b.payment_sub_type_code
	USING SQLCA;
	
	SQLCA.nf_handle_error("u_document_index", "uf_check_NBMS_early_filing_bonus", "SELECT count(*) FROM UNAPPLIED_CLAIM_TXN...(01)") 

	SELECT b.payment_sub_type_code, Count(*)
	INTO   :as_payment_sub_type_code, :li_count2
	FROM   UNAPPLIED_CLAIM_TXN a,
			 PAYMENT             b,
			 PAYMENT_DOCUMENT    c
	WHERE  a.payment_no            = b.payment_no
	AND    b.payment_no            = c.payment_no
	AND    c.doc_id                = :al_doc_id
	AND    b.payment_type_code     = '21'
	AND    b.payment_sub_type_code = '02'
	GROUP BY b.payment_sub_type_code
	USING SQLCA;
	
	SQLCA.nf_handle_error("u_document_index", "uf_check_NBMS_early_filing_bonus", "SELECT count(*) FROM UNAPPLIED_CLAIM_TXN...(02)") 

ELSEIF as_check_type = 'warning' THEN
	SELECT b.payment_sub_type_code, Count(*)
	INTO   :as_payment_sub_type_code, :li_count1
	FROM   APPLIED_CLAIM_TXN   a,
			 PAYMENT             b,
			 PAYMENT_DOCUMENT    c
	WHERE  a.payment_no            = b.payment_no
	AND    b.payment_no            = c.payment_no
	AND    c.doc_id                = :al_doc_id
	AND    b.payment_type_code     = '21'
	AND    b.payment_sub_type_code = '01'
	GROUP BY b.payment_sub_type_code
	USING SQLCA;
	
	SQLCA.nf_handle_error("u_document_index", "uf_check_NBMS_early_filing_bonus", "SELECT count(*) FROM APPLIED_CLAIM_TXN...(01)") 
	
	SELECT b.payment_sub_type_code, Count(*)
	INTO   :as_payment_sub_type_code, :li_count2
	FROM   APPLIED_CLAIM_TXN   a,
			 PAYMENT             b,
			 PAYMENT_DOCUMENT    c
	WHERE  a.payment_no            = b.payment_no
	AND    b.payment_no            = c.payment_no
	AND    c.doc_id                = :al_doc_id
	AND    b.payment_type_code     = '21'
	AND    b.payment_sub_type_code = '02'
	GROUP BY b.payment_sub_type_code
	USING SQLCA;
	
	SQLCA.nf_handle_error("u_document_index", "uf_check_NBMS_early_filing_bonus", "SELECT count(*) FROM APPLIED_CLAIM_TXN...(02)") 
END IF

IF li_count1 > 0 THEN
	IF li_count2 > 0 THEN
		MessageBox('Data Integrity Error','There are automated and manual NBMS Early Filing Bonus'&
												+ '~npayments for this document. Please contact the HELPDESK.')
      RETURN -1
	ELSE
		ai_count = li_count1
	END IF
ELSE
	// if li_count1 = 0 then return whatever value li_count2 has
	ai_count = li_count2
END IF

RETURN 0
end function

public function integer uf_check_chiro_doc_type_change (long al_doc_id, string as_new_doc_type_code, string as_check_type);INTEGER	li_return
LONG		ll_docid
DATE		ld_date_on_document
STRING	ls_type_code

//	Get current data from u_dw_online
ll_docid = dw_document_indexing.GetItemNumber(1, "docid")
ld_date_on_document = Date(dw_document_indexing.GetItemDatetime(1, "date_on_document"))

SELECT type_code
INTO   :ls_type_code
FROM   DOCUMENT_INDEX
WHERE  docid = :ll_docid
USING ImageTrans;

ImageTrans.nf_handle_error("u_document_index", "uf_check_chiro_doc_type_change", "SELECT type_code FROM DOCUMENT_INDEX...")

IF ld_date_on_document > ID_CHIRO_START THEN
	// if the date-on-document is within the range of the NB Chiro agreement
	CHOOSE CASE ls_type_code
		CASE 'SDC'
			// and the doc type was originally 'SDC'
			CHOOSE CASE as_new_doc_type_code
				CASE 'SDC'
				// do nothing if the document type did not change
				CASE ELSE
					
					IF as_check_type = 'error' THEN
						// if there are scheduled EFBs, then prevent doc type change
						MessageBox('Document Type Error','You cannot change the document type because there are scheduled'&
                                               + '~nNB Chiro early filing bonuses associated with this document.')
						RETURN -1
					ELSEIF as_check_type = 'warning' THEN
						// if there are scheduled EFBs, then prevent doc type change
						IF MessageBox('Document Type Warning','There are processed NB Chiro early filing bonus payments associated with this'& 
                                                    + '~ndocument. Do you want to continue with this change of the document type?',Exclamation!,YesNo!) = 2 THEN
							RETURN -1
						END IF
					END IF
			END CHOOSE
	END CHOOSE
END IF

RETURN li_return
end function

public function integer uf_check_nbms_1_doc_type_change (long al_doc_id, string as_new_doc_type_code, string as_check_type);INTEGER	li_return
LONG		ll_docid
DATE		ld_date_on_document
STRING	ls_type_code

//	Get current data from u_dw_online
ll_docid = dw_document_indexing.GetItemNumber(1, "docid")
ld_date_on_document = Date(dw_document_indexing.GetItemDatetime(1, "date_on_document"))

SELECT type_code
INTO   :ls_type_code
FROM   DOCUMENT_INDEX
WHERE  docid = :ll_docid
USING ImageTrans;

ImageTrans.nf_handle_error("u_document_index", "uf_check_NBMS_1_doc_type_change", "SELECT type_code FROM DOCUMENT_INDEX...")

IF ld_date_on_document > ID_NBMS_1_START AND ld_date_on_document < ID_NBMS_2_START THEN
	// if the date-on-document is within the range of the NBMS agreement
	CHOOSE CASE ls_type_code
		CASE 'SDD','MPD'
			// and the doc type was originally either 'SDD' or 'MPD'
			CHOOSE CASE as_new_doc_type_code
				CASE 'SDD','MPD'
					// do nothing if it changed to the other type, or stayed the same
				CASE ELSE
					
					IF as_check_type = 'error' THEN
						// if there are scheduled EFBs, then prevent doc type change
						MessageBox('Document Type Error','You cannot change the document type because there are scheduled'&
                                               + '~nNBMS early filing bonuses associated with this document.')
						RETURN -1
					ELSEIF as_check_type = 'warning' THEN
						// if there are scheduled EFBs, then prevent doc type change
						IF MessageBox('Document Type Warning','There are processed NBMS early filing bonus payments associated with this'& 
                                                    + '~ndocument. Do you want to continue with this change of the document type?',Exclamation!,YesNo!) = 2 THEN
							RETURN -1
						END IF
					END IF
			END CHOOSE
	END CHOOSE
END IF

RETURN li_return
end function

public function integer uf_check_nbms_2_doc_type_change (long al_doc_id, string as_new_doc_type_code, string as_check_type);INTEGER	li_return
LONG		ll_docid
DATE		ld_date_on_document
STRING	ls_type_code

//	Get current data from u_dw_online
ll_docid = dw_document_indexing.GetItemNumber(1, "docid")
ld_date_on_document = Date(dw_document_indexing.GetItemDatetime(1, "date_on_document"))

SELECT type_code
INTO   :ls_type_code
FROM   DOCUMENT_INDEX
WHERE  docid = :ll_docid
USING ImageTrans;

ImageTrans.nf_handle_error("u_document_index", "uf_check_NBMS_1_doc_type_change", "SELECT type_code FROM DOCUMENT_INDEX...")

IF ld_date_on_document >= ID_NBMS_2_START THEN
	// if the date-on-document is within the range of the NBMS agreement
	CHOOSE CASE ls_type_code
		CASE 'SDD','MPD','AD'
			// and the doc type was originally either 'SDD','MPD', or 'AD'
			CHOOSE CASE as_new_doc_type_code
				CASE 'SDD','MPD','AD'
					// do nothing if it changed to one of the other types, or stayed the same
				CASE ELSE					
					IF as_check_type = 'error' THEN
						// if there are scheduled EFBs, then prevent doc type change
						MessageBox('Document Type Error','You cannot change the document type because there are scheduled'&
                                               + '~nNBMS early filing bonuses associated with this document.')
						RETURN -1
					ELSEIF as_check_type = 'warning' THEN
						// if there are scheduled EFBs, then prevent doc type change
						IF MessageBox('Document Type Warning','There are processed NBMS early filing bonus payments associated with this'& 
                                                    + '~ndocument. Do you want to continue with this change of the document type?',Exclamation!,YesNo!) = 2 THEN
							RETURN -1
						END IF
					END IF
			END CHOOSE
	END CHOOSE
END IF

RETURN li_return
end function

public function integer uf_validate_chiro_efb (long al_docid);INTEGER    li_rtn, li_count
STRING     ls_type, ls_payment_sub_type_code
DATETIME   ldt_date_on_document, ldt_date_received, ldt_new_date_on_document, ldt_new_date_received
DATE       ld_date_on_document, ld_date_received, ld_new_date_on_document, ld_new_date_received
LONG       ll_docid
DWItemStatus ldwis_status

ll_docid             = dw_document_indexing.GetItemNumber(1,'docid')
ls_type              = dw_document_indexing.GetItemString(1,'type_code')

uf_get_original_dates(ll_docid,ldt_date_on_document,ldt_date_received)
ld_date_on_document  = Date(ldt_date_on_document)
ld_date_received     = Date(ldt_date_received)

ldt_new_date_on_document = dw_document_indexing.GetItemDateTime(1,'date_on_document')
ld_new_date_on_document  = Date(ldt_new_date_on_document)
ldt_new_date_received    = dw_document_indexing.GetItemDateTime(1,'date_received')
ld_new_date_received     = Date(ldt_new_date_received)

// count of scheduled NB Chiro EFBs
li_rtn = uf_count_Chiro_early_filing_bonus(al_docid,li_count,ls_payment_sub_type_code,'error')
IF li_rtn < 0 THEN RETURN -1 // do not allow change
IF li_count > 0 THEN
	ldwis_status = dw_document_indexing.GetItemStatus(1, 'type_code', Primary!)
	IF ldwis_status = DataModified! THEN
		IF ld_date_on_document >= ID_CHIRO_START THEN
			li_rtn = uf_check_Chiro_doc_type_change(al_docid, ls_type, 'error')
			IF li_rtn < 0 THEN RETURN -1 // do not allow change
		END IF
	END IF
	
	IF ld_date_on_document <> ld_new_date_on_document OR ld_date_received <> ld_new_date_received THEN
		li_rtn = uf_check_Chiro_doc_date_change(al_docid, ldt_new_date_on_document, ldt_new_date_received,ls_payment_sub_type_code, 'error')
		IF li_rtn < 0 THEN RETURN -1 // do not allow change
	END IF
END IF

// count of processed NB Chiro EFBs
li_rtn = uf_count_Chiro_early_filing_bonus(al_docid,li_count,ls_payment_sub_type_code,'warning')
IF li_rtn < 0 THEN RETURN -1 // do not allow change
IF li_count > 0 THEN
	ldwis_status = dw_document_indexing.GetItemStatus(1, 'type_code', Primary!)
	IF ldwis_status = DataModified! THEN
		IF ld_date_on_document >= ID_CHIRO_START THEN
			li_rtn = uf_check_Chiro_doc_type_change(al_docid, ls_type, 'warning')
			IF li_rtn < 0 THEN RETURN -1 // user chose to cancel change
		END IF
	END IF
	
	IF ld_date_on_document <> ld_new_date_on_document OR ld_date_received <> ld_new_date_received THEN
		li_rtn = uf_check_Chiro_doc_date_change(al_docid, ldt_new_date_on_document, ldt_new_date_received,ls_payment_sub_type_code, 'warning')
		IF li_rtn < 0 THEN RETURN -1 // user chose to cancel change
	END IF
END IF

return 0
end function

public subroutine uf_get_original_dates (long al_docid, ref datetime adt_date_on_document, ref datetime adt_date_received);select date_on_document , date_received
into   :adt_date_on_document, :adt_date_received
from   DOCUMENT_INDEX
where  docid = :al_docid
using ImageTrans;

SQLCA.nf_handle_error("u_document_index", "uf_get_original_dates", "select date_on_document , date_received") 
end subroutine

public function integer uf_validate_nbms_efb (long al_docid);INTEGER    li_rtn, li_count
STRING     ls_type, ls_payment_sub_type_code
DATETIME   ldt_date_on_document, ldt_date_received, ldt_new_date_on_document, ldt_new_date_received
DATE       ld_date_on_document, ld_date_received, ld_new_date_on_document, ld_new_date_received
LONG       ll_docid
DWItemStatus ldwis_status

ll_docid             = dw_document_indexing.GetItemNumber(1,'docid')
ls_type              = dw_document_indexing.GetItemString(1,'type_code')

uf_get_original_dates(ll_docid,ldt_date_on_document,ldt_date_received)
ld_date_on_document  = Date(ldt_date_on_document)
ld_date_received     = Date(ldt_date_received)

ldt_new_date_on_document = dw_document_indexing.GetItemDateTime(1,'date_on_document')
ld_new_date_on_document  = Date(ldt_new_date_on_document)
ldt_new_date_received    = dw_document_indexing.GetItemDateTime(1,'date_received')
ld_new_date_received     = Date(ldt_new_date_received)

// count of scheduled NBMS EFBs
li_rtn = uf_count_NBMS_early_filing_bonus(al_docid,li_count,ls_payment_sub_type_code,'error')
IF li_rtn < 0 THEN RETURN -1 // do not allow change
IF li_count > 0 THEN
	ldwis_status = dw_document_indexing.GetItemStatus(1, 'type_code', Primary!)
	IF ldwis_status = DataModified! THEN
		IF ld_date_on_document >= ID_NBMS_2_START THEN
			li_rtn = uf_check_NBMS_2_doc_type_change(al_docid, ls_type, 'error')
			IF li_rtn < 0 THEN RETURN -1 // do not allow change
		END IF
	END IF
	
	IF ld_date_on_document <> ld_new_date_on_document OR ld_date_received <> ld_new_date_received THEN
		IF ld_date_on_document >= ID_NBMS_2_START THEN
			li_rtn = uf_check_NBMS_2_doc_date_change(al_docid, ldt_new_date_on_document, ldt_new_date_received,ls_payment_sub_type_code, 'error')
			IF li_rtn < 0 THEN RETURN -1 // do not allow change
		END IF
	END IF
	
	ldwis_status = dw_document_indexing.GetItemStatus(1, 'type_code', Primary!)
	IF ldwis_status = DataModified! THEN
		IF ld_date_on_document >= ID_NBMS_1_START AND ld_date_on_document < ID_NBMS_2_START THEN
			li_rtn = uf_check_NBMS_1_doc_type_change(al_docid, ls_type, 'error')
			IF li_rtn < 0 THEN RETURN -1 // do not allow change
		END IF
	END IF
	
	IF ld_date_on_document <> ld_new_date_on_document OR ld_date_received <> ld_new_date_received THEN
		IF ld_date_on_document >= ID_NBMS_1_START AND ld_date_on_document < ID_NBMS_2_START THEN
			li_rtn = uf_check_NBMS_1_doc_date_change(al_docid, ldt_new_date_on_document, ldt_new_date_received,ls_payment_sub_type_code, 'error')
			IF li_rtn < 0 THEN RETURN -1 // do not allow change
		END IF
	END IF
END IF

// count of processed NBMS EFBs
li_rtn = uf_count_NBMS_early_filing_bonus(al_docid,li_count,ls_payment_sub_type_code,'warning')
IF li_rtn < 0 THEN RETURN -1 // do not allow change
IF li_count > 0 THEN
	ldwis_status = dw_document_indexing.GetItemStatus(1, 'type_code', Primary!)
	IF ldwis_status = DataModified! THEN
		IF ld_date_on_document >= ID_NBMS_2_START THEN
			li_rtn = uf_check_NBMS_2_doc_type_change(al_docid, ls_type, 'warning')
			IF li_rtn < 0 THEN RETURN -1 // user chose to cancel change
		END IF
	END IF
	
	IF ld_date_on_document <> ld_new_date_on_document OR ld_date_received <> ld_new_date_received THEN
		IF ld_date_on_document >= ID_NBMS_2_START THEN
			li_rtn = uf_check_NBMS_2_doc_date_change(al_docid, ldt_new_date_on_document, ldt_new_date_received,ls_payment_sub_type_code, 'warning')
			IF li_rtn < 0 THEN RETURN -1 // user chose to cancel change
		END IF
	END IF
	
	ldwis_status = dw_document_indexing.GetItemStatus(1, 'type_code', Primary!)
	IF ldwis_status = DataModified! THEN
		IF ld_date_on_document >= ID_NBMS_1_START AND ld_date_on_document < ID_NBMS_2_START THEN
			li_rtn = uf_check_NBMS_1_doc_type_change(al_docid, ls_type, 'warning')
			IF li_rtn < 0 THEN RETURN -1 // user chose to cancel change
		END IF
	END IF
	
	IF ld_date_on_document <> ld_new_date_on_document OR ld_date_received <> ld_new_date_received THEN
		IF ld_date_on_document >= ID_NBMS_1_START AND ld_date_on_document < ID_NBMS_2_START THEN
			li_rtn = uf_check_NBMS_1_doc_date_change(al_docid, ldt_new_date_on_document, ldt_new_date_received,ls_payment_sub_type_code, 'warning')
			IF li_rtn < 0 THEN RETURN -1 // user chose to cancel change
		END IF
	END IF
END IF

return 0
end function

public function integer uf_check_chiro_doc_date_change (long al_doc_id, datetime adt_new_date_on_document, datetime adt_new_date_received, string as_payment_sub_type_code, string as_check_type);INTEGER	li_return
LONG		ll_docid
DECIMAL	ldec_new_working_days

// check that change made gap between date on document & date received more than allowable by agreement
ldec_new_working_days = inv_account_payment.nf_calculate_working_days(adt_new_date_on_document,adt_new_date_received,"WHSCC")

IF as_check_type = 'error' THEN
	IF   (as_payment_sub_type_code = '09' AND ldec_new_working_days > 5) &
	  OR (as_payment_sub_type_code = '10' AND ldec_new_working_days > 7) THEN
	   MessageBox('Document Date Error', 'You cannot change the date on the document because there are scheduled'& 
                                    + '~nNB Chiro early filing bonus payments associated with this document.')
		RETURN -1
	END IF
	
	IF Date(adt_new_date_on_document) < ID_CHIRO_START THEN
		MessageBox('Document Date Error', 'You cannot change the date on the document to be outside of the NB Chiro Agreement'& 
                                    + '~nbecause there are scheduled NB Chiro early filing bonus payments associated with this document.')
		RETURN -1
	END IF
ELSEIF as_check_type = 'warning' THEN
	IF   (as_payment_sub_type_code = '09' AND ldec_new_working_days > 5) &
	  OR (as_payment_sub_type_code = '10' AND ldec_new_working_days > 7) THEN
	   // if there are processed EFBs, then warn about doc date change
		IF MessageBox('Document Date Warning','There are processed NB Chiro early filing bonus payments associated with this'& 
													 + '~ndocument. Do you want to continue with this change of the document date?',Exclamation!,YesNo!,2) = 2 THEN
			RETURN -1
		END IF
   END IF
	
	IF Date(adt_new_date_on_document) < ID_CHIRO_START THEN
		IF MessageBox('Document Date Warning', 'There are processed NB Chiro early filing bonus payments associated with this document. Do'& 
													 + '~nyou want to continue to change the document date to be before the NB Chiro agreement?',Exclamation!,YesNo!,2) = 2 THEN
			RETURN -1
		END IF
	END IF
END IF
					 
RETURN li_return

end function

public function integer uf_check_nbms_2_doc_date_change (long al_doc_id, datetime adt_new_date_on_document, datetime adt_new_date_received, string as_payment_sub_type_code, string as_check_type);INTEGER	li_return
LONG		ll_docid
DECIMAL	ldec_new_working_days

// check that change made gap between date on document & date received more than allowable by agreement
ldec_new_working_days = inv_account_payment.nf_calculate_working_days(adt_new_date_on_document,adt_new_date_received,"WHSCC")

IF as_check_type = 'error' THEN
	IF   (as_payment_sub_type_code = '01' AND ldec_new_working_days > 5) &
	  OR (as_payment_sub_type_code = '02' AND ldec_new_working_days > 7) THEN
	   MessageBox('Document Date Error', 'You cannot change the date on the document because there are scheduled'& 
                                    + '~nNBMS early filing bonus payments associated with this document.')
      RETURN -1
	END IF
	
	IF Date(adt_new_date_on_document) < ID_NBMS_2_START THEN
		MessageBox('Document Date Error', 'You cannot change the date on the document to be outside of the second NBMS Agreement'& 
                                    + '~nbecause there are scheduled NBMS early filing bonus payments associated with this document.')
		RETURN -1
	END IF
ELSEIF as_check_type = 'warning' THEN
	IF   (as_payment_sub_type_code = '01' AND ldec_new_working_days > 5) &
	  OR (as_payment_sub_type_code = '02' AND ldec_new_working_days > 7) THEN
	   // if there are processed EFBs, then warn about doc date change
		IF MessageBox('Document Date Warning','There are processed NBMS early filing bonus payments associated with this'& 
													 + '~ndocument. Do you want to continue with this change of the document date?',Exclamation!,YesNo!,2) = 2 THEN
			RETURN -1
		END IF
   END IF
	
	IF Date(adt_new_date_on_document) < ID_NBMS_2_START THEN
		IF MessageBox('Document Date Warning', 'There are processed NBMS early filing bonus payments associated with this document. Do '& 
													 + '~nyou want to continue to change the document date to be before the second NBMS agreement?',Exclamation!,YesNo!,2) = 2 THEN
			RETURN -1
		END IF
	END IF
END IF

RETURN li_return


end function

public function integer uf_check_nbms_1_doc_date_change (long al_doc_id, datetime adt_new_date_on_document, datetime adt_new_date_received, string as_payment_sub_type_code, string as_check_type);INTEGER	li_return
LONG		ll_docid
DECIMAL	ldec_new_working_days

// check that change made gap between date on document & date received more than allowable by agreement
ldec_new_working_days = inv_account_payment.nf_calculate_working_days(adt_new_date_on_document,adt_new_date_received,"WHSCC")

IF as_check_type = 'error' THEN
	IF   (as_payment_sub_type_code = '01' AND ldec_new_working_days > 3) &
	  OR (as_payment_sub_type_code = '02' AND ldec_new_working_days > 5) THEN
	   MessageBox('Document Date Error', 'You cannot change the date on the document because there are scheduled'& 
                                    + '~nNBMS early filing bonus payments associated with this document.')
      RETURN -1
	END IF
	
	IF Date(adt_new_date_on_document) < ID_NBMS_1_START OR Date(adt_new_date_on_document) >= ID_NBMS_2_START THEN
		MessageBox('Document Date Error', 'You cannot change the date on the document to be outside of the first NBMS Agreement'& 
                                    + '~nbecause there are scheduled NBMS early filing bonus payments associated with this document.')
		RETURN -1
	END IF
ELSEIF as_check_type = 'warning' THEN
	IF   (as_payment_sub_type_code = '01' AND ldec_new_working_days > 3) &
	  OR (as_payment_sub_type_code = '02' AND ldec_new_working_days > 5) THEN
	   // if there are processed EFBs, then warn about doc date change
		IF MessageBox('Document Date Warning','There are processed NBMS early filing bonus payments associated with this'& 
													 + '~ndocument. Do you want to continue with this change of the document date?',Exclamation!,YesNo!,2) = 2 THEN
			RETURN -1
		END IF
   END IF
	
	IF Date(adt_new_date_on_document) < ID_NBMS_1_START OR Date(adt_new_date_on_document) >= ID_NBMS_2_START THEN
		IF MessageBox('Document Date Warning', 'There are processed NBMS early filing bonus payments associated with this document. Do '& 
													 + '~nyou want to continue to change the document date to be outside of the first NBMS agreement?',Exclamation!,YesNo!,2) = 2 THEN
			RETURN -1
		END IF
	END IF
END IF

RETURN li_return

end function

public function integer uf_check_required (ref string as_type, ref datetime adt_date);// uf_check_required
//
Long    ll_service_provider_no, ll_count, ll_claim_no, ll_rehab_no
Integer li_rtn
String  ls_type, ls_service_provider_name, ls_csc, ls_cstc, ls_service_provider_type_code, ls_comment, ls_class
String  ls_doc_class,  ls_doc_sub_type, ls_sub_type, ls_referral_flag
long    ll_orig_claim_no, ll_referral_no, ll_days, ll_count_subtype
Datetime ldtm_accident, ldtm_today, ldtm_create_date

inv_referral = create N_REFERRAL

ldtm_today = f_server_datetime()

// Check that you should be here.
IF dw_document_indexing.RowCount() <> 1 OR dw_document_indexing.AcceptText() = -1 THEN
	RETURN -1
END IF

/* The claim must not be changed to a different claim for a physio referral if the rehab task is in progress or completed.
*/
ll_orig_claim_no = This.dw_document_indexing.GetItemNumber(1, "claim_no", Primary!, TRUE)
ls_referral_flag =  this.dw_document_indexing.GetItemString(1, "referral_flag")

IF ll_orig_claim_no <> dw_document_indexing.GetItemNumber(1,"claim_no") AND ls_referral_flag = 'Y' THEN
	IF this.dw_document_indexing.GetItemNumber(1, "rehab_referral_no") > 0 THEN
		ll_referral_no =  this.dw_document_indexing.GetItemNumber(1, "rehab_referral_no")
		IF inv_referral.nf_validate_task_status(ll_referral_no) < 0 THEN
			MessageBox('Error','Cannot remove the claim no because the referral has an associated rehab task status that is Inprogress or Completed.',Exclamation!)
			Return -1
		END IF
	END IF
END IF

// Make sure there is a record in the sheet.
IF iw_sheet.dw_basic_claim.RowCount() < 1 THEN
	MessageBox("No Claim","Can not index a record when no claim exists on the sheet.",Exclamation!)
	RETURN -1
END IF

// Make sure the claim is imaged.
IF iw_sheet.dw_basic_claim.GetItemString(1, "imaged_flag") <> "Y" THEN
	MessageBox('Non-Imaged Claim','This claim is not imaged. Indexing to this claim is not allowed.',Exclamation!)
	RETURN -1
END IF

// Make sure a valid claim is entered.
IF IsNull(dw_document_indexing.GetItemNumber(1,"claim_no")) OR Not dw_document_indexing.GetItemNumber(1,"claim_no") > 0 THEN
	MessageBox("Claim Number", "Claim number must be filled in and validated.",Exclamation!)
	RETURN -1
END IF

// Check that class is filled in.
ls_class  = dw_document_indexing.GetItemString(1,"doc_class_code") 
IF IsNull(ls_class) OR Trim(ls_class) = "" THEN
	MessageBox("Document Indexing","Doc Class must be filled in.",Exclamation!)
	dw_document_indexing.SetFocus()
	dw_document_indexing.SetColumn("doc_class_code")
	RETURN -1
END IF

// Check that type is filled in.
as_type  = dw_document_indexing.GetItemString(1,"type_code") 
adt_date = dw_document_indexing.GetItemDateTime(1,"date_on_document") 
IF IsNull(as_type) OR Trim(as_type) = "" THEN
	MessageBox("Document Indexing","Type must be filled in.",Exclamation!)
	dw_document_indexing.SetFocus()
	dw_document_indexing.SetColumn("type_code")
	RETURN -1
END IF

//Check that the type & class match
Select doc_class_code
Into  :ls_doc_class 
From Doc_Type
Where doc_type_code = :as_type
Using SQLCA;
	
SQLCA.nf_handle_error('u_document_index','uf_check_required','Select From Doc_Type')

IF ls_doc_class <> ls_class THEN
	MessageBox('Error','The doc class and doc type code do not correspond. Please correct one.',Exclamation!)
	dw_document_indexing.SetFocus()
	dw_document_indexing.SetColumn("type_code")
	Return -1
END IF

//Check that the doc type code and sub-type codes correspond
ls_sub_type = dw_document_indexing.GetItemString(1,"doc_subtype_code")

Select doc_subtype_code
Into   :ls_doc_sub_type
From  Doc_Type_Subtype_Xref
Where doc_type_code = :as_type
And     doc_subtype_code = :ls_sub_type
Using SQLCA;
	
SQLCA.nf_handle_error('u_document_index','uf_check_required','Select From Doc_Type_Subtype_Xref')

IF ls_doc_sub_type <> ls_sub_type THEN
	MessageBox('Error','The doc type and doc sub type codes do not correspond. Please correct one.',Exclamation!)
	dw_document_indexing.SetFocus()
	dw_document_indexing.SetColumn("type_code")
	Return -1
END IF


Select count(doc_subtype_code)
Into   :ll_count_subtype
From  Doc_Type_Subtype_Xref
Where doc_type_code = :as_type
and doc_subtype_code <> ""
and active_flag = 'Y'
Using SQLCA;
	
SQLCA.nf_handle_error('u_document_index','uf_check_required','Select From Doc_Type_Subtype_Xref')

IF ll_count_subtype > 0 and ls_sub_type = "" THEN
	MessageBox('Error','A document sub type code must be entered for the class and doc type entered.',Exclamation!)
   	dw_document_indexing.Modify("doc_subtype_code.protect=0     doc_subtype_code.Background.Color='16777215'")
	dw_document_indexing.SetFocus()
	dw_document_indexing.SetColumn("doc_subtype_code")
	Return -1
END IF	


// Check that the date on document is filled in.
IF IsNull(adt_date) OR String(adt_date) = "0000 01 01 00:00:00" THEN
	MessageBox("Document Indexing","Date must filled in.",Exclamation!)
	dw_document_indexing.SetFocus()
	dw_document_indexing.SetColumn("date_on_document")
	RETURN -1
END IF

// Check to see if trying to index to a claim coded as Claim Error (claim status code & claim status type code of R & 08)
IF ib_knowsheet THEN
	ls_csc = iw_sheet.dw_basic_claim.GetItemString(1,'claim_status_code')
	ls_cstc = iw_sheet.dw_basic_claim.GetItemString(1,'claim_status_type_code')
	IF ls_csc = 'R' and ls_cstc = '08' THEN
		MessageBox("Document Indexing","Can not index to this claim as it is coded as a Claim Error.",Exclamation!)
		dw_document_indexing.SetFocus()
		dw_document_indexing.SetColumn("claim_no")
		RETURN -1
	END IF
ELSE
	MessageBox("Document Indexing","Could not determine if the claim is coded as a Claim Error.",Exclamation!)
END IF

//	Check the service provider number.
ll_service_provider_no = dw_document_indexing.GetItemNumber(1, "service_provider_no")
ls_service_provider_type_code = dw_document_indexing.GetItemString(1, "service_provider_type_code")

IF IsNull(ll_service_provider_no) THEN
	dw_document_indexing.SetItem(1,'service_provider_no',0)
ELSE
	IF ll_service_provider_no > 0 THEN
		ls_service_provider_name = inv_imaging.nf_service_provider_name(ll_service_provider_no, ls_service_provider_type_code, "w_document_indexing")
		If ls_service_provider_name = "" THEN
			MessageBox('Invalid Provider Number','The provider no (sender) selected is not valid/active.',Exclamation!)
			dw_document_indexing.SetColumn("service_provider_no")
			dw_document_indexing.SetFocus()
			RETURN -1
		END IF
	END IF
END IF

//	PR #970  - Code added by Ed Lenarczyk  May 10, 2000  Check for comments if sender type is 'M' and 
// no valid service provider is specified and check date_received vs date_on_document.  
// If the latter is greater than former, there will be a warning message displayed.  
// The user can still save such a document if he chooses so.
ls_comment = Trim(dw_document_indexing.GetItemString(1,"comment"))
IF ll_service_provider_no = 0 AND ls_service_provider_type_code = 'M' AND (ls_comment = '' OR ls_comment = '-') THEN
	MessageBox('Incomplete Data','Please enter valid provider no or Comment',Exclamation!)
	dw_document_indexing.SetColumn("comment")
	dw_document_indexing.SetFocus()
	RETURN -1
END IF

IF dw_document_indexing.GetItemString(1,'source_code') = 'R'  AND  &
	dw_document_indexing.GetItemDateTime(1,'date_on_document') > dw_document_indexing.GetItemDateTime(1,'date_received') Then
	If Messagebox("WARNING  !!!","Date on document is greater than Date received !  Do you want to correct ?",Question!,YesNo!) = 1 Then
		dw_document_indexing.SetColumn("date_received")
		dw_document_indexing.SetFocus()
		Return -1
	End If
END IF

//	If no reference number entered, initialize to 0.
IF IsNull(dw_document_indexing.GetItemNumber(1,'reference_no')) THEN
	dw_document_indexing.SetItem(1,'reference_no',0)
END IF

//Get the accident date
ll_claim_no = dw_document_indexing.GetItemNumber(1,'claim_no')

Select accident_date
into    :ldtm_accident
From CLAIM
Where claim_no = :ll_claim_no
Using SQLCA;

SQLCA.nf_handle_error('u_document_index','uf_check_required','Select From CLAIM')

// If the referral flag is checked then there has to be: 
// Service Provider Type 
// Service Provider Number (this is the provider that is making the referral)
// Date on Document (this is referral date )

IF dw_document_indexing.GetItemString(1,'referral_flag') = 'Y' THEN
	//Check the task_status if there is a referral
	IF IsNull(ll_service_provider_no) or ll_service_provider_no = 0 THEN
		MessageBox('Error','A Service Provider No must be entered if the referral flag is checked.',Exclamation!)
		Return -1
	END IF
	IF IsNull(ls_service_provider_type_code) or ls_service_provider_type_code = '' THEN
		MessageBox('Error','A Service Provider Type Code must be entered if the referral flag is checked.',Exclamation!)
		Return -1
	END IF		
    IF IsNull(adt_date) or String(adt_date) = "0000 01 01 00:00:00"  or String(adt_date) = '1900-01-01 00:00:00' THEN
		MessageBox('Error','A Date on Document must be entered if the referral flag is checked.',Exclamation!)
		Return -1
	END IF
	
	
	// Check the Date on document - must be on or after the accident date and not in the future.
	// The Date on Document must not be before the accident date for a referral document.
	// The Date on Document must not be in the future for a referral document.

	
	IF Date(adt_date) < Date(ldtm_accident) Then
		MessageBox('Error',"The Date on Document must be on or after the Claim's accident date of "+String(ldtm_accident,"yyyy-mm-dd")+" for a referral document.",Exclamation!)
		Return -1
	END IF
	
	IF Date(adt_date) > Date(ldtm_today) Then
		MessageBox('Error',"The Date on Document must not be in the future for a referral document.",Exclamation!)
		Return -1
	END IF
	
	// The date on document can not be corrected to a date that is greater than the day the document was indexed (create date on the doc).
	
	IF NOT IsNull(dw_document_indexing.GetItemDateTime(1,'create_date')) THEN
		ldtm_create_date = dw_document_indexing.GetItemDateTime(1,'create_date')
		IF dw_document_indexing.GetItemDateTime(1,'date_on_document') > ldtm_create_date THEN
			MessageBox('Error',"The Referral Date must be on or before the date the document was orginally indexed of "+String(Date(ldtm_create_date),'YYYY-MM-DD')+".",Exclamation!)
			Return -1
		END IF
	END IF
	
	// The Date on Document must be 6 weeks or less from the accident date for a physio referral if the task is In Progress or Completed
	ll_rehab_no = this.dw_document_indexing.GetItemNumber(1, "rehab_referral_no")
	IF NOT IsNull(this.dw_document_indexing.GetItemNumber(1, "rehab_referral_no")) THEN
		IF inv_referral.nf_validate_task_status(this.dw_document_indexing.GetItemNumber(1, "rehab_referral_no")) < 0 THEN
			//Check the date
			ll_days = DaysAfter ( date(ldtm_accident),Date(dw_document_indexing.GetItemDateTime(1,'date_on_document') ) )
			
			IF ll_days > 42 or ll_days < 0 THEN
				Messagebox('Date on Doc Error','The Date on Document must be 6 weeks or less from the accident date for a physio referral if the task is In Progress or Completed.',Exclamation!)
				Return -1
			END IF
		END IF	
	END IF
	
END IF

IF this.dw_document_indexing.GetItemNumber(1, "rehab_referral_no") > 0 THEN
		ll_referral_no =  this.dw_document_indexing.GetItemNumber(1, "rehab_referral_no")
		
		IF dw_document_indexing.GetItemString(1,'referral_flag') = 'N' THEN 
			IF inv_referral.nf_validate_task_status(ll_referral_no) < 0 THEN
				MessageBox('Error','Cannot remove the referral flag because the referral has an associated rehab task status that is Inprogress or Completed.',Exclamation!)
				Return -1
			END IF
		END IF
		
END IF
//

RETURN 1

end function

event constructor;LONG		ll_rows
string     ls_type_code

/*	Create a instance of the n_imaging user object.
*/
	inv_imaging = Create n_imaging
   inv_account_payment = Create n_account_payment_controller


/*	Set the transaction objects for the columns whoose edit styles are dddw's.
*/
	dw_document_indexing.GetChild("document_desc",idwc_document_type)
	dw_document_indexing.SetTransObject(ImageTrans)
	dw_document_indexing.GetChild("service_provider_type_code",idwc_recipient_types)
	idwc_recipient_types.SetTransObject(SQLCA)
	
	dw_document_indexing.GetChild("doc_class_code",idwc_class_code)
	idwc_class_code.SetTransObject(ImageTrans)
	
	ll_rows = idwc_class_code.Retrieve()
	ImageTrans.nf_handle_error('u_document_index','constructor','	idwc_class_code.Retrieve()')
	
	If ll_rows <= 0 Then SIgnalError(-666,'Error retrieving document types.')

	dw_document_indexing.GetChild("type_code",idwc_document_type)
	idwc_document_type.SetTransObject(ImageTrans)
	
	ll_rows = idwc_document_type.Retrieve()
	ImageTrans.nf_handle_error('u_document_index','constructor','	idwc_document_type.Retrieve()')
	
	If ll_rows <= 0 Then SIgnalError(-666,'Error retrieving document types.')


// //   ls_type_code = dw_document_indexing.GetItemstring(dw_document_indexing.getrow(),"type_code")

   // put code here to check the xref to see if there is a subtype, if there isn't one then disable and tab to next column

	dw_document_indexing.GetChild("doc_subtype_code",idwc_subtype_code)
	idwc_subtype_code.SetTransObject(ImageTrans)
	
	ll_rows = idwc_subtype_code.Retrieve()
	ImageTrans.nf_handle_error('u_document_index','constructor','	idwc_subtype_code.Retrieve()')
	
	If ll_rows <= 0 Then SIgnalError(-666,'Error retrieving document types.')

/*	Retrieve records for idwc_recipient_types and set filter for all V's and M's.
*/
	idwc_recipient_types.Retrieve()
	IF SQLCA.nf_handle_error("vidwc_recipient_types","u_document_index","constructor for u_document_index") < 0 THEN
		RETURN
	END IF
	idwc_recipient_types.SetFilter("recipient_type_code = 'V' or recipient_type_code = 'M'")
	idwc_recipient_types.Filter()

/*	Get the active work sheet. Also set variable if successful.
*/
	ib_knowsheet = TRUE
	iw_sheet = w_frame.GetActiveSheet()
	IF Not IsValid(iw_sheet) THEN
		ib_knowsheet = FALSE
	END IF

end event

on u_document_index.create
this.cb_provider_search=create cb_provider_search
this.dw_document_indexing=create dw_document_indexing
this.Control[]={this.cb_provider_search,&
this.dw_document_indexing}
end on

on u_document_index.destroy
destroy(this.cb_provider_search)
destroy(this.dw_document_indexing)
end on

on destructor;/* Destroy the instance of n_imaging.
*/
	IF IsValid(inv_imaging) THEN
		Destroy inv_imaging
	END IF

end on

type cb_provider_search from commandbutton within u_document_index
boolean visible = false
integer x = 800
integer y = 604
integer width = 87
integer height = 76
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "?"
end type

event clicked;S_WINDOW_MESSAGE lstr_message

STRING ls_type, ls_service_provider_name, ls_comment

// get the type to search for
ls_type = dw_document_indexing.GetItemString(1,'service_provider_type_code')
IF IsNull(ls_type) THEN
	MessageBox("Sender Search","Sender Type must be provided in order to do search.")
	RETURN
END IF

OpenWithParm(w_service_provider_index, ls_type)

lstr_message = Message.PowerObjectParm

// set the sevice provider (sender) no
// can also set the type since it is returned also
// If user has hit CANCEL on on SP search, do not overwrite entered vals

IF lstr_message.al_doubleparm[1] <> 0 THEN
	dw_document_indexing.SetItem(1,'service_provider_no', lstr_message.al_doubleparm[1])
	ls_service_provider_name = inv_imaging.nf_service_provider_name(lstr_message.al_doubleparm[1],ls_type,"w_document_indexing")
	dw_document_indexing.SetItem(1,"service_provider_name",ls_service_provider_name)
	IF Len(ls_service_provider_name) > 20 THEN	ls_comment= Trim(left(ls_service_provider_name,20)) ELSE ls_comment = Trim(ls_service_provider_name)
	dw_document_indexing.SetItem(1,"comment",ls_comment)
END IF

end event

type dw_document_indexing from u_dw_online within u_document_index
integer x = 14
integer y = 12
integer width = 2153
integer height = 760
integer taborder = 0
string dataobject = "d_document_indexing"
borderstyle borderstyle = styleraised!
end type

event itemchanged;call super::itemchanged;Long     ll_service_provider_no, ll_result, ll_rownum, ll_docid, ll_payment_no, ll_orig_claim_no
Long     ll_rows, ll_counter, ll_num_rows, ll_count, ll_orig_service_provider_no, ll_claim_no, ll_task_no
Long		ll_count_claims, ll_orginal_claim_no
Integer  li_rtn, li_correspond_no, li_count
String   ls_service_provider_name, ls_service_provider_type, ls_date, ls_payment_no_list, ls_msg, ls_type_code, ls_class_code, ls_task_status_code
String   ls_message, ls_payment_sub_type_code, ls_doc_type_desc_e, ls_doc_subtype_desc_e, ls_service_provider_type_code, ls_doc_type1, ls_comment
Date     ld_date, ld_doccreatedate, ld_date_on_document, ld_date_received
Datetime ldt_doccreatedate, ldt_eligibility_start_date, ldt_date_on_document, ldt_date, ldt_date_received, ldt_new_date_on_document, ldt_new_date_received, ldtm_date
u_ds     lds_payment_document_payment
long ll_rowcount, ll_subtype, ll_referral_no, ll_count_subtype

inv_referral = create N_REFERRAL

IF IsNull(dwo) = TRUE THEN
	RETURN
END IF

ll_rownum = GetRow()
IF ll_rownum <> 1 THEN
	MessageBox("Document Index","Error on document indexing form. Please refresh.",Exclamation!)
	RETURN
END IF

ll_docid = GetItemNumber(1, "docid")

uf_get_original_dates(ll_docid,ldt_date_on_document,ldt_date_received)

ld_date_on_document = Date(ldt_date_on_document)
ld_date_received = Date(ldt_date_received)

ls_type_code = This.GetItemString(row, "type_code")

SELECT claim_no 
INTO :ll_orginal_claim_no
FROM DOCUMENT_INDEX
WHERE docid = :ll_docid
USING ImageTrans;

li_rtn = ImageTrans.nf_handle_error("u_document_index", "dw_document_indexing", "itemchanged - SELECT create_date FROM DOCUMENT_INDEX") 

//The type & subtype must be filtered based on the class code entered. 
	
// Based on the column that is firing the event, run the appropriate validation code.
IF dwo.name = "claim_no" THEN
	ll_claim_no = Long(data)
	IF uf_set_claim(ll_claim_no) = -1 THEN
		AcceptText()
		RETURN 1
	END IF

	// PR 2966 - Does a payment exist against the existing claim?
	lds_payment_document_payment = Create U_DS
	lds_payment_document_payment.DataObject = 'd_account_payment_list'
	lds_payment_document_payment.SetTransObject(SQLCA)
	
	ll_rows = lds_payment_document_payment.Retrieve(ll_docid)

	IF ll_rows > 0 THEN
		FOR ll_counter = 1 TO ll_rows
			ll_payment_no = lds_payment_document_payment.GetItemNumber(ll_counter , 'payment_no')
			ls_payment_no_list = ls_payment_no_list + String(ll_payment_no) + ', '
		NEXT
		
		ls_payment_no_list = Left(ls_payment_no_list, (Len(ls_payment_no_list) - 2) )
		
		IF ll_rows = 1 THEN
			ls_msg = 'There is already a payment (' + ls_payment_no_list + ') associated with this claim for this document.~r' &
				   	+ 'Do you wish to continue re-indexing to another claim?'
		ELSE
			ls_msg = 'There are already payments (' + ls_payment_no_list + ') associated with this claim for this document.~r' &
						+ 'Do you wish to continue re-indexing to another claim?'
		END IF
		
		IF MessageBox('Payment Exists', ls_msg, Question!, YesNo!, 2) = 2 THEN
			ib_paid_document = TRUE
			RETURN 2
		END IF
	ELSE
		ib_paid_document = FALSE
	END IF
	
	//PR 3320 - Prevent out-going correspondence documents from being re-indexed to another claim.
	SELECT IsNull(correspond_no , 0)
	  INTO :li_correspond_no
	  FROM CORRESPONDENCE
	 WHERE doc_id = :ll_docid ;

	li_rtn = SQLCA.nf_handle_error("u_document_index", "dw_document_indexing", "itemchanged - SELECT IsNull(correspond_no , 0) FROM CORRESPONDENCE") 
	
	IF li_correspond_no > 0 THEN
		MessageBox('Outgoing Correspondence', 'This document (' + String(ll_docid) + '.DOC) is outgoing correspondence #' + &
					  String(li_correspond_no) + '~r~nand cannot be re-indexed to another claim.' &
					  + '~r~nPerhaps you should archive this document and recreate it under the new claim.',Exclamation!)
		ib_correspondence = TRUE
		RETURN 1
	ELSE
		ib_correspondence = FALSE
	END IF
	
	// An ARX document can only be re-indexed to a registered claim
	ls_type_code = This.GetItemString(row, "type_code")	
	IF ls_type_code = "ARX" THEN
		ll_orig_claim_no = This.GetItemNumber(row, "claim_no", Primary!, TRUE)
		IF ll_orig_claim_no > 0 AND ll_orig_claim_no <> ll_claim_no THEN
			SELECT COUNT(*)
			  INTO :ll_count 
			  FROM X001_REGISTRATION 
			 WHERE claim_no = :ll_claim_no ; 

			li_rtn = SQLCA.nf_handle_error("u_document_index", "dw_document_indexing", "itemchanged - SELECT COUNT(*) FROM X001_REGISTRATION")

			IF ll_count <= 0 THEN
				Messagebox("Invalid Claim #", "An ARX document can only be re-indexed to a registered claim.  " +&
							  String(ll_claim_no) + " is not registered for drug coverage.", Information!)
				This.SetItem(row, "claim_no", ll_orig_claim_no)
				This.SetColumn("claim_no")
				//PR20872 R.S. add this SetText function call, to reset/maintain what the user entered. The SetText function also assures that if the 
				//user hits 'Continue' button a second time, the AcceptText() in that button code will actually fire itemchanged again, thus performing these validations again
				This.SetText(STRING(ll_claim_no))
				This.SetFocus()
				RETURN 1
			END IF
			
			// Don't allow ARX document type to be re-indexed to another claim if the date on the document < eligibility start date
			ldt_date_on_document = This.GetItemDatetime(row, "date_on_document")
			
			SELECT COUNT(*)
			INTO   :ll_count_claims
			FROM   CLAIM_ELIGIBILITY
			WHERE  claim_no = :ll_claim_no
			AND     eligibility_start_date <= :ldt_date_on_document
			AND    (eligibility_end_date   >= :ldt_date_on_document or eligibility_end_date IS NULL)  //PR20872 > add conditional checking for a null end date
			USING  SQLCA;
			
			IF ll_count_claims = 0 THEN
				Messagebox("Invalid Claim #", "An ARX document can only be re-indexed to another claim as long as it " +&
							  "has coverage for the same date.", Information!)
				This.SetItem(row, "claim_no", ll_orig_claim_no)
				This.SetColumn("claim_no")
				//PR20872 - add SetText() - see comment above
				This.SetText(STRING(ll_claim_no))
				This.SetFocus()
				RETURN 1
			END IF
		END IF
	END IF
	
	//Check the task_status if there is a referral
	IF dw_document_indexing.GetItemString(row,"referral_flag") = 'Y' THEN
		IF this.getitemnumber(row, "rehab_referral_no") > 0 THEN
			ll_referral_no =  this.GetItemNumber(row, "rehab_referral_no")
			IF inv_referral.nf_validate_task_status(ll_referral_no) < 0 THEN
				MessageBox('Error','Cannot remove the claim no because the referral has an associated rehab task status that is Inprogress or Completed.',Exclamation!)
				Return 1
			END IF
		END IF
	END IF


ELSEIF dwo.name = "doc_class_code" THEN
	IF data = "" OR data = "   " OR IsNull(data) THEN
		RETURN
	END IF

	SetItem(1,"document_desc","")
	
	li_rtn = dw_document_indexing.GetChild("doc_class_code", idwc_class_code)
	ll_num_rows = idwc_class_code.RowCount()

	ll_result = idwc_class_code.Find("doc_class_code = '" + data + "'" ,1,ll_num_rows)
	IF ll_result <= 0 THEN
		MessageBox("Type","Invalid document type.",Exclamation!)
		RETURN 1
	END IF
	
	// the type & sub type depend on the class
//	ls_type_code = This.GetItemString(row, "type_code")
//	
	IF NOT IsNull(this.getitemstring(row,"type_code")) and this.getitemstring(row,"type_code") <> '' THEN
		This.SetItem(row, "type_code",'')
	END IF
	
	IF NOT IsNull(this.getitemstring(row,"doc_subtype_code")) and this.getitemstring(row,"doc_subtype_code") <> '' THEN
		This.SetItem(row, "doc_subtype_code",'')
	     This.object.doc_subtype_code.Protect = 0
	END IF
	
//	This.SetItem(row, "doc_subtype_code",'')
//	ldt_orig_accident_date = dw_claim.GetItemDatetime(row, "accident_date", Primary!, TRUE)
//	
	li_rtn = dw_document_indexing.GetChild("type_code",idwc_document_type)
	ll_num_rows = idwc_document_type.RowCount()
	
	idwc_document_type.Retrieve()
	IF SQLCA.nf_handle_error("u_document_index","itemchanged","ldwc_document_type.Retrieve()") < 0 THEN
		RETURN
	END IF
    ll_rowcount = idwc_document_type.RowCount()

	idwc_document_type.SetFilter("class_code = '" + data + "'" )
	idwc_document_type.Filter()
	idwc_document_type.SetSort("#1 A")
	idwc_document_type.Sort()
	
	idwc_subtype_code.SetFilter("")
	idwc_subtype_code.Filter()
	
	This.SetItem(1,"doc_subtype_code",'')
	This.object.doc_subtype_code.Protect = 1
	This.Modify("doc_subtype_code.Background.Color='79741120'")
	
	If ib_restrict_type_code_changes Then
		// RULE:  If the type_code is udpatable and we are not in the indexing module
		// then the document must be sent to corrections to change the type_code to something else.
		If GetItemString(1,"document_type_code_allow_update_flag") = "Y" Then
			MessageBox("Type code restriction","You must send this document to CORRECTIONS to have the document type changed.")
			RETURN 1
		End if
	End if

ELSEIF dwo.name = "type_code" THEN
	IF data = "" OR data = "   " OR IsNull(data) THEN
		RETURN
	END IF
	
	li_rtn = dw_document_indexing.GetChild("type_code", idwc_document_type)
	ll_num_rows = idwc_document_type.RowCount()
	
	ll_result = idwc_document_type.Find("type_code = '" + data + "'" ,1,ll_num_rows)
	IF ll_result <= 0 THEN
		MessageBox("Type","Invalid document type.",Exclamation!)
		RETURN 1	
	END IF
	

	IF NOT IsNull(this.getitemstring(row,"doc_subtype_code")) and this.getitemstring(row,"doc_subtype_code") <> '' THEN
		This.SetItem(row, "doc_subtype_code",'')
		  This.object.doc_subtype_code.Protect = 0
	END IF
	
	li_rtn = dw_document_indexing.GetChild("doc_subtype_code",idwc_subtype_code)
	ll_num_rows = idwc_subtype_code.RowCount()
	
	idwc_subtype_code.Retrieve()
	IF SQLCA.nf_handle_error("u_document_index","itemchanged","idwc_subtype_code.Retrieve()") < 0 THEN
		RETURN
	END IF
	 ll_rowcount = idwc_subtype_code.RowCount()
	 
	// If there are no subtypes for that doc type code then skip over that field, there is nothing to pick. 
	//dw_document_indexing.object.doc_subtype_code.protect = False
	
	idwc_subtype_code.SetFilter("doc_type_code = '" + data + "'" )
	idwc_subtype_code.Filter()
	idwc_subtype_code.SetSort("#1 A")
	idwc_subtype_code.Sort()

	If ib_restrict_type_code_changes Then
		// RULE:  If the type_code is udpatable and we are not in the indexing module
		// then the document must be sent to corrections to change the type_code to something else.
		If GetItemString(1,"document_type_code_allow_update_flag") = "Y" Then
			MessageBox("Type code restriction","You must send this document to CORRECTIONS to have the document type changed.")
			RETURN 1
		End if
	End if
	
	//display the document type desc
	Select  doc_type_desc_e
	Into     :ls_doc_type_desc_e
	From   Doc_Type	
	Where doc_type_code = :data
	Using SQLCA;
	
	SQLCA.nf_handle_error("u_document_index", "dw_document_indexing", "SELECT  FROM Doc_Type_Subtype_Xref") 
	
	SetItem(1,"document_desc",ls_doc_type_desc_e)

	//	If the document type is AW - WRC Account then fill in the date as today's date, and mark the document as paid.
	IF data = "AW " OR data = "AW" THEN
		ld_date = Date(f_server_datetime())
		SetItem(1,"date_on_document",ld_date)
		SetItem(1,"paid_flag","Y")
	END IF

	// If the old document type was an account and they had it marked as paid then if the new type
	// is not an account, unmark the paid field.
	IF Left(data,1) <> "A" AND data <> "SDC" AND data <> "SDD" AND data <> "MPC" AND data <> "MPD" AND &
		GetItemString(1,"paid_flag") = "Y" THEN
		SetItem(1,"paid_flag","N")
	End If

	// PR #970 change  made by Ed Lenarczyk  on May 10, 2000 Condition changed   from  <> 'SDC'  to  = 'SDC'  
	IF Left(data,1) = "A" OR  data = "SDC" OR data = "SDD" OR data = "MPC" OR data = "MPD" OR data = "MP" THEN	
		This.SetItem(1,'service_provider_type_code','M')
	ELSE
		This.SetItem(1,'service_provider_type_code','')
		This.SetItem(1,'service_provider_no',0)
		This.SetItem(1,'service_provider_name','')
		This.SetItem(1,'comment','')
	END IF

	IF data = "AR" THEN
		ll_service_provider_no = This.GetItemNumber(1,'service_provider_no')
		IF ll_service_provider_no = 0 OR IsNull(ll_service_provider_no) = TRUE THEN
			This.SetItem(1,'comment','Claim Reimbursement')
		END IF
	END IF
	
	//T015677 - 2015-06, R.S.  Direct Referral
	IF data = 'SP' THEN
		setItem(row, 'referral_flag', 'Y')	
		this.accepttext()
	ELSE
		setItem(row, 'referral_flag', 'N')	
		this.accepttext()
	END IF

	
	//******************************************************************
	// P10151-47 NB Chiro Agreement, fixes for NBMS Agreement

	li_rtn = uf_count_NBMS_early_filing_bonus(ll_docid,li_count,ls_payment_sub_type_code,'error')
	IF li_rtn < 0 THEN RETURN 1   // do not allow focus to change
	IF li_count > 0 THEN
		IF ld_date_on_document >= ID_NBMS_1_START AND ld_date_on_document < ID_NBMS_2_START THEN
			li_rtn = uf_check_NBMS_1_doc_type_change(ll_docid, data, 'error')
			IF li_rtn < 0 THEN RETURN 1   // do not allow focus to change
		ELSEIF ld_date_on_document >= ID_NBMS_2_START THEN
			li_rtn = uf_check_NBMS_2_doc_type_change(ll_docid, data, 'error')
			IF li_rtn < 0 THEN RETURN 1   // do not allow focus to change
		ELSE
			// should never happen, i.e., prior to NBMS agreement 1
		END IF
	END IF
	
	li_rtn = uf_count_Chiro_early_filing_bonus(ll_docid,li_count,ls_payment_sub_type_code,'error')
	IF li_rtn < 0 THEN RETURN 1   // do not allow focus to change
	IF li_count > 0 THEN
		IF ld_date_on_document >= ID_CHIRO_START THEN
			li_rtn = uf_check_Chiro_doc_type_change(ll_docid, data, 'error')
			IF li_rtn < 0 THEN RETURN 1   // do not allow focus to change
		ELSE
			// should never happen, i.e., prior to NB Chiro agreement
		END IF
	END IF
	
	// end Chiro changes
	//*********************************************************************
	

	 // Protect the sub type and skip it if the class and type doesn't have a sub type.
	 
	Select  count(doc_subtype_code)
	Into     :ll_subtype
	From   Doc_Type_Subtype_Xref
	Where doc_type_code = :data
	And     active_flag = 'Y'
	Using SQLCA;
	
	SQLCA.nf_handle_error("u_document_index", "dw_document_indexing", "SELECT  FROM Doc_Type_Subtype_Xref") 
	 
	 IF ll_subtype < 2  THEN
		This.SetItem(1,"doc_subtype_code",'')
		  This.object.doc_subtype_code.Protect = 1
		  This.Modify("doc_subtype_code.Background.Color='79741120'")
	ELSE
		  This.object.doc_subtype_code.Protect = 0	
		  This.Modify("doc_subtype_code.Background.Color='16777215'")
	END IF



// If the document type is AW - WRC Account then fill in the date as today's date, and mark the document as paid.
//	IF data = "AW " THEN
//		ld_date = Date(f_server_datetime())
//		SetItem(1,"date",ld_date)
//		SetItem(1,"paid_flag","Y")
//	END IF


ELSEIF dwo.name = "doc_subtype_code" THEN
	IF data = "" OR data = "   " OR IsNull(data) THEN
		RETURN
	END IF
	
	li_rtn = dw_document_indexing.GetChild("doc_subtype_code", idwc_subtype_code)
	ll_num_rows = idwc_subtype_code.RowCount()
	
	ll_result = idwc_subtype_code.Find("doc_subtype_code = '" + data + "'" ,1,ll_num_rows)
	IF ll_result <= 0 THEN
		MessageBox("SubType","Invalid document subtype.",Exclamation!)
		RETURN 1
	END IF
	
	If ib_restrict_type_code_changes Then
		// RULE:  If the type_code is updatable and we are not in the indexing module
		// then the document must be sent to corrections to change the type_code to something else.
		If GetItemString(1,"document_type_code_allow_update_flag") = "Y" Then
			MessageBox("SubType code restriction","You must send this document to CORRECTIONS to have the document sub type changed.")
			RETURN 1
		End if
	End if
	
	ls_doc_type1 = this.GetItemString(1,"type_code")
	
	//display the document type desc
	Select  doc_type_desc_e
	Into     :ls_doc_type_desc_e
	From   Doc_Type	
	Where doc_type_code = :ls_doc_type1
	Using SQLCA;
	
	SQLCA.nf_handle_error("u_document_index", "dw_document_indexing", "SELECT  FROM Doc_Type_Subtype_Xref") 
	
	//display the document sub type desc
	Select  doc_subtype_desc_e
	Into     :ls_doc_subtype_desc_e
	From   Doc_Subtype	
	Where doc_subtype_code = :data
	Using SQLCA;
	
	SQLCA.nf_handle_error("u_document_index", "dw_document_indexing", "SELECT  FROM Doc_Type_Subtype_Xref") 
	
	SetItem(1,"document_desc",ls_doc_type_desc_e +" "+ls_doc_subtype_desc_e)

ELSEIF dwo.name = "date_on_document" THEN
	IF data = "" OR data = "   " OR IsNull(data) THEN
		RETURN
	END IF

	ls_date = Left(data,10)
	ld_date = Date(ls_date)
	ldt_date = DateTime(ld_date)

	// 'Date on Document' validations (Expanded for SR 10, Aug. 2001, EMcD.)
	IF ld_date < Date('1920-01-01') THEN
		MessageBox( "Invalid Document Date", "The Document Date must not be before Jan. 1, 1920.", Exclamation!)
		RETURN 1
	ELSEIF ld_date > Date('2079-06-01') THEN
		MessageBox( "Invalid Document Date", "The Document Date must not be after June 1, 2079.", Exclamation!)
		RETURN 1
	ELSEIF ld_date > Date(f_server_datetime()) THEN
		MessageBox( "Invalid Document Date", "The Document Date must not be in the future.", Exclamation! )
		RETURN 1
	ELSEIF ld_date < RelativeDate( Date( f_server_datetime()), -180 ) THEN	// Added for SR 10, Aug. 2001, EMcD.
		MessageBox( "Date Warning", "Warning - The 'date on document' is more than 6 months in the past.", Information! )
	END IF
	
	//******************************************************************
	// P10151-47 NB Chiro Agreement, fixes for NBMS Agreement
	
	ldt_new_date_received = THIS.GetItemDateTime(1,'date_received')
	
	li_rtn = uf_count_NBMS_early_filing_bonus(ll_docid,li_count,ls_payment_sub_type_code,'error')
	IF li_rtn < 0 THEN RETURN 1   // do not allow focus to change
	IF li_count > 0 THEN
		// if original date was within the agreement range
		IF ld_date_on_document >= ID_NBMS_1_START AND ld_date_on_document < ID_NBMS_2_START THEN
			li_rtn = uf_check_NBMS_1_doc_date_change(ll_docid, ldt_date, ldt_new_date_received, ls_payment_sub_type_code, 'error')
			IF li_rtn < 0 THEN RETURN 1   // do not allow focus to change
		ELSEIF ld_date_on_document >= ID_NBMS_2_START THEN
			li_rtn = uf_check_NBMS_2_doc_date_change(ll_docid, ldt_date, ldt_new_date_received, ls_payment_sub_type_code, 'error')
			IF li_rtn < 0 THEN RETURN 1   // do not allow focus to change
		ELSE
			// should never happen, i.e., prior to NBMS agreement 1
		END IF
	END IF
	
	li_rtn = uf_count_Chiro_early_filing_bonus(ll_docid,li_count,ls_payment_sub_type_code,'error')
	IF li_rtn < 0 THEN RETURN 1   // do not allow focus to change
	IF li_count > 0 THEN
		IF ld_date_on_document >= ID_CHIRO_START THEN
			li_rtn = uf_check_Chiro_doc_date_change(ll_docid, ldt_date, ldt_new_date_received, ls_payment_sub_type_code, 'error')
			IF li_rtn < 0 THEN RETURN 1   // do not allow focus to change
		ELSE
			// should never happen, i.e., prior to NB Chiro agreement
		END IF
	END IF
	
	// end Chiro changes
	//*********************************************************************
	
ELSEIF dwo.name = "source_code" THEN
	//	If sent_flag is 'Y' then source code can't be 'R'eceived.
	IF GetItemString(1,"sent_flag") = "Y" AND data = 'R' THEN
		MessageBox("Source","Document can not be received and sent.",Exclamation!)
		RETURN 1
	END IF
ELSEIF dwo.name = "sent_flag" THEN
	// If source_code is 'R'eceived then sent flag can't be 'Y'es.
	IF GetItemString(1,"source_code") = "R" And data = 'Y' THEN
		MessageBox("Source","Document can not be received and sent.",Exclamation!)
		RETURN 1
	END IF
ELSEIF dwo.name = "service_provider_no" THEN
	IF data = "" Or data = "0" THEN
		RETURN
	END IF
	ll_service_provider_no = Long(data)
	ls_service_provider_type = This.GetItemString(1, "service_provider_type_code")

	// A document must not be indexed to a service provider that is set up to receive automated payments
	SELECT COUNT(*) 
	  INTO :ll_count 
	  FROM App_Document_Index_Parameter 
	 WHERE service_provider_no = :ll_service_provider_no ;

	li_rtn = SQLCA.nf_handle_error("u_document_index", "dw_document_indexing", "itemchanged - SELECT COUNT(*) FROM App_Document_Index_Parameter")
	 
	IF ll_count > 0 THEN
		ll_orig_service_provider_no = This.GetItemNumber(1, "service_provider_no", Primary!, TRUE)
		This.SetItem(1, "service_provider_no", ll_orig_service_provider_no)
		Messagebox("Service Provider Not Alllowed", "Service Provider " + String(ll_service_provider_no) + &
					  " is set up to receive automated payments.  Documents must not be indexed to service providers " +&
					  "that are set up to receive automated payments.~r~rChoose a different provider and try again.", Information!)
		RETURN 1
	END IF

	// Check that a valid sender number has been entered.
	ls_service_provider_name = inv_imaging.nf_service_provider_name(ll_service_provider_no, ls_service_provider_type, "w_document_indexing")
	IF ls_service_provider_name = "" THEN
		MessageBox("Validation Error","Invalid Service Provider Number",Exclamation!)
		RETURN 1
	ELSE
		SetItem(1,"service_provider_name",ls_service_provider_name)
		IF Len(ls_service_provider_name) > 20 THEN	ls_comment= Trim(left(ls_service_provider_name,20)) ELSE ls_comment = Trim(ls_service_provider_name)
     	SetItem(1,"comment",ls_comment)
	END IF
ELSEIF dwo.name = "date_received" THEN
	IF data = "" OR data = "   " OR IsNull(data) THEN
		RETURN
	END IF

	ls_date = Left(data,10)
	ld_date = Date(ls_date)
	ldt_date = DateTime(ld_date)

	// 'Date Received' validations (Expanded for SR 10, Aug. 2001, EMcD.)
	IF ld_date < Date('1920-01-01') THEN
		MessageBox( "Invalid Received Date", "The Date Received must not be before 1920-01-01.", Exclamation! )	// sr10
		RETURN 1
	ELSEIF ld_date > Date('2079-06-01') THEN
		MessageBox( "Invalid Received Date", "The Date Received must not be after June 1, 2079.", Exclamation! ) // sr10
		RETURN 1
	ELSEIF ld_date > Date(f_server_datetime()) THEN
		MessageBox( "Invalid Received Date", "The Date Received must not be in the future.", Exclamation! )	// sr10
		RETURN 1
	ELSEIF ld_date < RelativeDate( Date( f_server_datetime()), -90 ) THEN
		MessageBox( "Date Warning", "Warning - The Date Received is more than 3 months in the past.", Information! )
	END IF

	//	Date received must be before the document creation date.
	SELECT create_date 
	  INTO :ldt_doccreatedate
	  FROM DOC
	 WHERE docid = :ll_docid
	 USING ImageTrans;

	li_rtn = ImageTrans.nf_handle_error("u_document_index", "dw_document_indexing", "itemchanged - SELECT create_date FROM DOC") 
	IF li_rtn < 0 THEN
		RETURN
	END IF

	ld_doccreatedate = Date(ldt_doccreatedate)

	IF ld_date > ld_doccreatedate THEN
		MessageBox("Date Received","Date received must be before document was scanned in.",Exclamation!)
		RETURN 1 
	END IF
	
	//******************************************************************
	// P10151-47 NB Chiro Agreement, fixes for NBMS Agreement
	
	ldt_new_date_on_document = THIS.GetItemDateTime(1,'date_on_document')
	
	li_rtn = uf_count_NBMS_early_filing_bonus(ll_docid,li_count,ls_payment_sub_type_code,'error')
	IF li_rtn < 0 THEN RETURN 1   // do not allow focus to change
	IF li_count > 0 THEN
		IF ld_date_on_document >= ID_NBMS_1_START AND ld_date_on_document < ID_NBMS_2_START THEN
			li_rtn = uf_check_NBMS_1_doc_date_change(ll_docid, ldt_new_date_on_document, ldt_date, ls_payment_sub_type_code, 'error')
			IF li_rtn < 0 THEN RETURN 1   // do not allow focus to change
		ELSEIF ld_date_on_document >= ID_NBMS_2_START THEN
			li_rtn = uf_check_NBMS_2_doc_date_change(ll_docid, ldt_new_date_on_document, ldt_date, ls_payment_sub_type_code, 'error')
			IF li_rtn < 0 THEN RETURN 1   // do not allow focus to change
		ELSE
			// should never happen, i.e., prior to NBMS agreement
		END IF
	END IF
	
	li_rtn = uf_count_Chiro_early_filing_bonus(ll_docid,li_count,ls_payment_sub_type_code,'error')
	IF li_rtn < 0 THEN RETURN 1   // do not allow focus to change
	IF li_count > 0 THEN
		IF ld_date_on_document >= ID_CHIRO_START THEN
			li_rtn = uf_check_Chiro_doc_date_change(ll_docid, ldt_new_date_on_document, ldt_date, ls_payment_sub_type_code, 'error')
			IF li_rtn < 0 THEN RETURN 1   // do not allow focus to change
		ELSE
			// should never happen, i.e., prior to NB Chiro agreement
		END IF
	END IF
	
	// end Chiro changes
	//*********************************************************************
	
ELSEIF dwo.name = "reference_no" THEN
	IF data = "" THEN
		RETURN
	END IF
ELSEIF dwo.name = "paid_flag" THEN
	//	If the user marks the document as paid then the document type has to be an account (A%) and the document won't be routed.
	IF data = "Y" THEN
		IF Left(GetItemString(1,"type_code"),1) = "A" OR GetItemString(1,"type_code") = "SDC" OR GetItemString(1,"type_code") = "SDD" OR GetItemString(1,"type_code") = "MPC" OR GetItemString(1,"type_code") = "MPD" THEN
			RETURN
		ELSE
			MessageBox("Document Indexing - Paid","Only accounts can be marked as paid",Exclamation!)
			RETURN 2
		END IF
	END IF
ELSEIF dwo.name = "service_provider_type_code" THEN
	// Blank out service_provider_no, ..._name if service_provider_type_code has been changed.
	SetItem(1,"service_provider_no",0)
	SetItem(1,"service_provider_name","")
	SetItem(1,"comment","")
ELSEIF dwo.name = "referral_flag" THEN

// The referral flag can not be removed if there is an associated auto rehab task & authorization


IF data = 'N' THEN // the referral flag was removed
	IF this.getitemnumber(row, "rehab_referral_no") > 0 THEN
		ll_referral_no =  this.GetItemNumber(row, "rehab_referral_no")
		IF inv_referral.nf_validate_task_status(ll_referral_no) < 0 THEN
			MessageBox('Error','Cannot remove the referral flag because the associated rehab task status is Inprogress or Completed.',Exclamation!)
			Return 1
		END IF
	END IF
 END IF

END IF


end event

event buttonclicked;call super::buttonclicked;Long ll_docid

ll_docid = This.GetItemNumber(row, "docid")

OpenWithParm(w_form67_document_errors, ll_docid) 

end event

