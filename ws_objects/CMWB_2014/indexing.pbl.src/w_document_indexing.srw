$PBExportHeader$w_document_indexing.srw
$PBExportComments$Window for document indexing used by scanning/indexing group
forward
global type w_document_indexing from w_a_tool
end type
type cb_switch2 from commandbutton within w_document_indexing
end type
type cb_remlink from commandbutton within w_document_indexing
end type
type cb_cancel from commandbutton within w_document_indexing
end type
type st_index_only from statictext within w_document_indexing
end type
type st_full_routing from statictext within w_document_indexing
end type
type rb_index_only from radiobutton within w_document_indexing
end type
type rb_full_route from radiobutton within w_document_indexing
end type
type cb_refresh from commandbutton within w_document_indexing
end type
type dw_event from u_dw_online within w_document_indexing
end type
type dw_routing_assignments from u_dw_online within w_document_indexing
end type
type dw_routing_defaults from u_dw_online within w_document_indexing
end type
type dw_indexing_paid_documents from u_dw_online within w_document_indexing
end type
type cb_move from commandbutton within w_document_indexing
end type
type dw_options from u_dw_online within w_document_indexing
end type
type st_2 from statictext within w_document_indexing
end type
type dw_document_list from u_dw_online within w_document_indexing
end type
type cb_delete_document from commandbutton within w_document_indexing
end type
type dw_select_category from u_dw_online within w_document_indexing
end type
type cb_route from commandbutton within w_document_indexing
end type
type cb_alt1 from commandbutton within w_document_indexing
end type
type gb_routing_options from groupbox within w_document_indexing
end type
type uo_image_append from u_image_append within w_document_indexing
end type
type cb_search from commandbutton within w_document_indexing
end type
type cb_recall from commandbutton within w_document_indexing
end type
type uo_document_indexing from u_document_index within w_document_indexing
end type
type cb_refresh_cat from commandbutton within w_document_indexing
end type
type dw_duplicate_documents from u_dw_online within w_document_indexing
end type
end forward

global type w_document_indexing from w_a_tool
string title = ""
boolean resizable = false
cb_switch2 cb_switch2
cb_remlink cb_remlink
cb_cancel cb_cancel
st_index_only st_index_only
st_full_routing st_full_routing
rb_index_only rb_index_only
rb_full_route rb_full_route
cb_refresh cb_refresh
dw_event dw_event
dw_routing_assignments dw_routing_assignments
dw_routing_defaults dw_routing_defaults
dw_indexing_paid_documents dw_indexing_paid_documents
cb_move cb_move
dw_options dw_options
st_2 st_2
dw_document_list dw_document_list
cb_delete_document cb_delete_document
dw_select_category dw_select_category
cb_route cb_route
cb_alt1 cb_alt1
gb_routing_options gb_routing_options
uo_image_append uo_image_append
cb_search cb_search
cb_recall cb_recall
uo_document_indexing uo_document_indexing
cb_refresh_cat cb_refresh_cat
dw_duplicate_documents dw_duplicate_documents
end type
global w_document_indexing w_document_indexing

type variables
DataWindowChild		idwc_categories,idwc_recipient_types

Boolean	ib_already_indexed,ib_reset_rte_opts_reqd
String	is_curr_mode,is_old_paid_flag
Long		il_master_fldid,il_curr_col,il_setid

w_sheet			      iw_sheet
n_imaging		      inv_imaging
n_referral              inv_referral
n_event_log		      inv_event_log
w_document_indexing	iwi_window_parent
s_window_message     istr_window_message

date                 idt_todays_date
boolean ib_del_confirm_reqd  = TRUE

m_cmwb                             im_cmwb







end variables

forward prototypes
public subroutine wf_indexing_or_routing (boolean vas_index_only)
public subroutine wf_set_update_mode (boolean vab_update_on)
public function integer wf_set_screen_location (long al_screen_height, long al_screen_width, long al_screen_x, long al_screen_y)
public function integer wf_switch_documents ()
public subroutine wf_set_index_entry_mode (boolean vab_index_entry_on)
public function integer wf_set_claim (long al_claim_no)
public function integer wf_delete_document (long al_docid, long al_fldid, long al_rownum)
public function integer wf_update_referral ()
public function long wf_last_referral_no ()
protected function long wf_auto_routing (long al_docid, long al_fldid, string as_doc_type, string as_doc_class_type, string as_claim_status, string as_status_type, boolean ab_not_a_correction)
end prototypes

public subroutine wf_indexing_or_routing (boolean vas_index_only);// Sets up screen display for indexing or routing

	uo_document_indexing.dw_document_indexing.SetReDraw(False)

	If vas_index_only Then
//		dw_options.SetItem(1,"index_only","Y")
		uo_document_indexing.dw_document_indexing.SetItem(1,"message_info","To Be Indexed Only")
	Else
//		dw_options.SetItem(1,"index_only","N")
		uo_document_indexing.dw_document_indexing.SetItem(1,"message_info","To Be Fully Routed")
	End If

	uo_document_indexing.dw_document_indexing.SetReDraw(True)
end subroutine

public subroutine wf_set_update_mode (boolean vab_update_on);//	This function enables or disables objects that we know we can enable/disable
//	when the user is updating or finishes updating 
//
Long   ll_row
String ls_module_source_code

If vab_update_on Then

	dw_select_category.Enabled 	= False
	dw_document_list.Enabled 		= False
	uo_document_indexing.Enabled 	= False
	dw_options.Enabled 				= False
	dw_duplicate_documents.Hide()

	cb_close.Enabled 				= False
	cb_delete_document.Enabled = False
	cb_move.Enabled 				= False
	cb_remlink.Enabled 			= False
	cb_refresh.Enabled 			= False
	cb_recall.Enabled          = False

Else

	dw_select_category.Enabled 	= TRUE
	dw_document_list.Enabled 		= TRUE
	uo_document_indexing.Enabled 	= TRUE
	dw_options.Enabled 				= TRUE

	cb_close.Enabled 				= TRUE
	cb_delete_document.Enabled = TRUE
	cb_move.Enabled 				= TRUE
	cb_remlink.Enabled 			= TRUE
	cb_refresh.Enabled 			= TRUE
	
	// Form67 and Medical Aid Documents can't be recalled
	ll_row = dw_document_list.GetRow()
	IF ll_row = 0 THEN
		cb_recall.Enabled = TRUE
	ELSE
		ls_module_source_code = dw_document_list.GetItemString(ll_row, "module_source_code")
		IF ls_module_source_code = "12" OR ls_module_source_code = "13"  OR ls_module_source_code = "15"  THEN
			cb_recall.Enabled 		= FALSE
			dw_options.Enabled 	= FALSE
		ELSE
			cb_recall.Enabled 		= TRUE
		END IF
	END IF
End If
end subroutine

public function integer wf_set_screen_location (long al_screen_height, long al_screen_width, long al_screen_x, long al_screen_y);/* set the frame location based on the properties set in the coorespoding set function
*/
 
 w_frame.width  = al_screen_width
 w_frame.height = al_screen_height
 w_frame.x      = al_screen_x
 w_frame.y      = al_screen_y
 
 RETURN 1
end function

public function integer wf_switch_documents ();INTEGER	li_count, li_counter, li_documentcount
LONG		ll_docid, ll_pgfid, ll_fldid, ll_result, ll_master_fldid, ll_newdocfldid
LONG		ll_claim_no, ll_olddocid, ll_rownum, ll_masterfldid, ll_newdocid, ll_set_result
STRING	ls_type, ls_comment, ls_docname, ls_status, ls_casemgr, ls_action, ls_name, ls_doc_sub_type_code
DATETIME ldt_date

/*	This function switches a document that has been currently routed and indexed with a new
	document that is to be routed and index.
*/
	ll_claim_no		= uo_document_indexing.dw_document_indexing.GetItemNumber(1,"claim_no")
	/*
	PR 2046 Tyler Craft Sept 10
	Changed the below GetItemNumber statement to getrow() instead of a hard coded one 
	so that the document that the user picked is taken and not the first one in the list.
	*/
	ll_olddocid		= dw_duplicate_documents.GetItemNumber(dw_duplicate_documents.getrow(),"docid")
	/*PR 2046 ends*/
	ls_type			= uo_document_indexing.dw_document_indexing.GetItemString(1,"type_code")
	ls_doc_sub_type_code = uo_document_indexing.dw_document_indexing.GetItemString(1,"doc_subtype_code")
	ll_newdocid 	= dw_document_list.GetItemNumber(dw_document_list.GetRow(),'ref_docid')
	ll_masterfldid	= uo_document_indexing.il_master_fldid
	ls_casemgr		= iw_sheet.dw_basic_claim.GetItemString(1,"claim_manager_user_id")
	ls_status		= iw_sheet.dw_basic_claim.GetItemString(1,"claim_status_code")
	ll_newdocfldid	= uo_document_indexing.dw_document_indexing.GetItemNumber(1,"fldid")

/*	Set up some defaults for the creation of the CLAIM_WORKING record that will be needed.
*/
	ls_name = Trim(iw_sheet.dw_basic_claim.GetItemString(1,"given_names")) + " " + Trim(iw_sheet.dw_basic_claim.GetItemString(1,"last_name"))
	ls_action = "ZZ"

/*	Determine ls_action if the claim's status is pre-adjudication ("P").
*/
	IF ls_status = "P" THEN
		IF ls_casemgr = "" THEN
			IF uo_document_indexing.dw_document_indexing.GetItemString(1,"lost_time") = "Y" THEN
				ls_action = "LT"
			ELSE
				ls_action = "NL"
			END IF
		END IF
	END IF

/*	STEP 1. Check to see if an archived master record exists for the claim in the CLAIM_MASTER_ARCHIVE
	table.
*/
	SELECT folderid
	  INTO :ll_fldid
	  FROM CLAIM_MASTER_ARCHIVE
	 WHERE claim_no = :ll_claim_no
	 USING ImageTrans;

	ll_result = ImageTrans.nf_handle_error("Embedded SQL: Select from claimsmaster_archive","w_document_indexing","nf_archive_document")
	IF ll_result < 0 THEN
		Return -1
	ELSE
		IF ll_result = 100 THEN

/* No archive record	exists, so create one.
*/
			ll_fldid = inv_imaging.nf_create_workfolder("w_document_indexing",258)
			IF ll_fldid = -1 THEN
				RETURN -1
			END IF

			INSERT INTO CLAIM_MASTER_ARCHIVE
				(folderid, claim_no, imaged_claim_flag)
			VALUES (:ll_fldid, :ll_claim_no,"Y")
			USING ImageTrans;

			IF ImageTrans.nf_handle_error("Embedded SQL: Insert into CLAIM_MASTER_ARCHIVE","w_document_indexing","inv_imaging.nf_archive_document") < 0 THEN
				RETURN -1
			END IF

			IF ImageTrans.SQLNRows <> 1 THEN
				ImageTrans.nf_rollback_transaction()
				MessageBox("Document Archive","Unable to create claim archive record.",StopSign!)
				RETURN -1
			END IF
		END IF
	END IF

/*	STEP 2. Check to see if the document being replaced exists in another master folder.
*/
	SELECT count(*)
	  INTO :li_count
	  FROM REF
	 WHERE docid = :ll_olddocid 
		AND doccatid = 2
	 USING ImageTrans;

	IF ImageTrans.nf_handle_error("Embedded SQL: Select from REF","w_document_indexing","inv_imaging.nf_archive_document") < 0 THEN
		RETURN -1
	END IF

/* If the document exists in another master then duplicate the logical
	entries to create a second copy of the document.
*/
	IF li_count > 1 THEN

/*	Get the next available docid for the duplicated document.
*/
		ll_docid = inv_imaging.nf_get_next_docid("w_document_indexing")
		IF ll_docid = -1 THEN
			RETURN -1
		END IF

/*	Create logical document entry by entering the required records into the appropriate
	tables.
*/
		INSERT INTO DOC
			(docid, docname, docnpgs, docrefcount , file_extension )
		SELECT :ll_docid, docname, docnpgs, docrefcount, file_extension
		  FROM DOC
		 WHERE DOC.docid = :ll_olddocid
		 USING ImageTrans;

		IF ImageTrans.nf_handle_error("Embedded SQL: Insert into DOC","w_document_indexing","inv_imaging.nf_archive_document") < 0 THEN
			RETURN -1
		END IF

		IF ImageTrans.SQLNRows <> 1 THEN
			ImageTrans.nf_rollback_transaction()
			MessageBox("Document Archive","Unable to create document entry.",StopSign!)
			RETURN -1
		END IF

/* Create the page entries, but first need to find out how may pages make up this document.
*/
		SELECT count(*)
		  INTO :li_count
		  FROM PAG
		 WHERE pagdocid = :ll_olddocid
		 USING ImageTrans;

		IF ImageTrans.nf_handle_error("Embedded SQL: Select from PAG","w_document_indexing","inv_imaging.nf_archive_document") < 0 THEN
			RETURN -1
		END IF

/* Set up a loop and create a record for each page of the new document.
*/
		li_counter = 1
		DO WHILE li_counter <= li_count		

			INSERT INTO PAG
				(pagdocid,pagseq,pagfid )
			SELECT :ll_docid,pagseq,pagfid
			  FROM PAG 
			 WHERE PAG.pagdocid = :ll_olddocid
				AND PAG.pagseq = :li_counter
			 USING ImageTrans;

			IF ImageTrans.nf_handle_error("Embedded SQL: Insert into PAG","w_document_indexing","inv_imaging.nf_archive_document") < 0 THEN
				RETURN -1
			END IF

			IF ImageTrans.SQLNRows <> 1 THEN
				ImageTrans.nf_rollback_transaction()
				MessageBox("Document Indexing","Unable to create page entry.",StopSign!)
				RETURN -1
			END IF
			li_counter ++
		LOOP

/*	Replace the original document with the newly created one (this leaves the original document in the other master).
*/
		UPDATE REF
			SET docid = :ll_docid
		 WHERE REF.docid = :ll_olddocid
			AND REF.doccatid = 2
			AND REF.docfldid = :ll_masterfldid
		 USING ImageTrans;

		IF ImageTrans.nf_handle_error("Embedded SQL: Update REF","w_document_indexing","inv_imaging.nf_archive_document") < 0 THEN
			RETURN -1
		END IF

		IF ImageTrans.SQLNRows <> 1 THEN
			ImageTrans.nf_rollback_transaction()
			MessageBox("Document Indexing","Unable to create reference entry.",StopSign!)
			RETURN -1
		END IF

/* Create a DOCUMENT_INDEX record for this document. If the original document type is one 
	of the old 'split' codes then it has to be recoded to 'JK' to be archived.
*/
		SELECT type_code, doc_subtype_code
		  INTO :ls_type, :ls_doc_sub_type_code
		  FROM DOCUMENT_INDEX
		 WHERE docid = :ll_olddocid
		 USING ImageTrans;

		IF ImageTrans.nf_handle_error("Embedded SQL: Select from DOCUMENT_INDEX","w_document_indexing","inv_imaging.nf_archive_document") < 0 THEN
			RETURN -1
		END IF

      IF ls_type = 'AC!' Or ls_type = 'AD!' Or ls_type = 'SD!' Or ls_type = 'MP!' THEN
			ls_type = 'JK'
			ls_doc_sub_type_code = ''
		END IF

/* This may not create an entry if the document is not indexed.
*/
		INSERT INTO DOCUMENT_INDEX
			(docid,type_code,doc_subtype_code,date_on_document,comment,source_code,sent_flag,service_provider_no,
			 service_provider_type_code,claim_no,reference_no,date_received,english_flag,imaged_document_flag)
		SELECT :ll_docid,:ls_type,:ls_doc_sub_type_code,date_on_document,comment,source_code,sent_flag,service_provider_no,
				 service_provider_type_code,claim_no,reference_no,date_received,english_flag,imaged_document_flag
		  FROM DOCUMENT_INDEX
		 WHERE DOCUMENT_INDEX.docid = :ll_olddocid
		 USING ImageTrans;

		IF ImageTrans.nf_handle_error("Embedded SQL: Insert into DOCUMENT_INDEX","w_document_indexing","inv_imaging.nf_archive_document") < 0 THEN
			RETURN -1
		END IF

/* Set the docid to the new document number.
*/
		ll_olddocid = ll_docid
	END IF

/* STEP 3. Check for indexing information.
*/
	SELECT count(*)
	  INTO :li_count
	  FROM DOCUMENT_INDEX
	 WHERE docid = :ll_olddocid
	 USING ImageTrans;

	ll_result = ImageTrans.nf_handle_error("Embedded SQL: Select from DOCUMENT_INDEX","w_document_indexing","inv_imaging.nf_archive_document")
	IF ll_result < 0 THEN
		RETURN -1
	ELSE
		IF ll_result = 100 THEN

/*	No indexing information exists, so create it.
*/
			INSERT INTO DOCUMENT_INDEX
				(docid,type_code,doc_subtype_code, date_on_document,comment,source_code,sent_flag,service_provider_no,
				 service_provider_type_code,claim_no,reference_no, date_received, english_flag, imaged_document_flag)
			VALUES
				(:ll_olddocid,'JK','',getdate(),"",'I',"N",0,"",:ll_claim_no,0,getdate(),"Y","Y")
			USING ImageTrans;

			IF ImageTrans.nf_handle_error("Embedded SQL: Insert into DOCUMENT_INDEX","w_document_indexing","inv_imaging.nf_archive_document") < 0 THEN
				RETURN -1
			END IF

			IF ImageTrans.SQLNRows <> 1 THEN
				ImageTrans.nf_rollback_transaction()
				MessageBox("Document Archive","Unable to create DOCUMENT_INDEX record.",StopSign!)
				RETURN -1
			END IF
		END IF
	END IF

/*	STEP 4. Move the document to the archive category.
*/
	INSERT INTO DOCUMENT_INDEX_ARCHIVE
		(docid,type_code,doc_subtype_code,date_on_document,comment,source_code,sent_flag,service_provider_no,
		 service_provider_type_code,claim_no,reference_no,date_received,english_flag,imaged_document_flag,
		 reason_code, reason_text)
	SELECT docid,type_code,doc_subtype_code,date_on_document,comment,source_code,sent_flag,service_provider_no,
			 service_provider_type_code,claim_no,reference_no, date_received,english_flag,imaged_document_flag,
			 "DD",""
	  FROM DOCUMENT_INDEX
	 WHERE docid = :ll_olddocid
	 USING ImageTrans;

	IF ImageTrans.nf_handle_error("Embedded SQL: Insert into DOCUMENT_INDEX_ARCHIVE","w_document_indexing","inv_imaging.nf_archive_document") < 0 THEN
		RETURN -1
	END IF

	IF ImageTrans.SQLNRows <> 1 THEN
		ImageTrans.nf_rollback_transaction()
		MessageBox("Document Archive","Unable to archive DOCUMENT_INDEX record.",StopSign!)
		RETURN -1
	END IF

/*	STEP 5. Delete the original DOCUMENT_INDEX record.
*/
	DELETE
	  FROM DOCUMENT_INDEX
	 WHERE docid = :ll_olddocid
	 USING ImageTrans;

	IF ImageTrans.nf_handle_error("Embedded SQL: Delete from DOCUMENT_INDEX","w_document_indexing","inv_imaging.nf_archive_document") < 0 THEN
		RETURN -1
	END IF

	IF ImageTrans.SQLNRows <> 1 THEN
		ImageTrans.nf_rollback_transaction()
		MessageBox("Document Archive","Unable to remove DOCUMENT_INDEX record.",StopSign!)
		RETURN -1
	END IF

/*	STEP 7a. Delete the REF record of the new document which is going to replace the old document.
	This is the	record which shows the new document being in the Scanning set.
*/
	DELETE REF
	 WHERE docid = :ll_newdocid
		AND doccatid <> 2
	 USING ImageTrans;

	IF ImageTrans.nf_handle_error("Embedded SQL: Delete from REF","w_document_indexing","inv_imaging.nf_archive_document") < 0 THEN
		RETURN -1
	END IF

/* STEP 7b. Update the docrefcount 
*/
	UPDATE 	DOC
	SET		docrefcount = docrefcount - :ImageTrans.SQLNRows
	WHERE		docid = :ll_newdocid
	USING		ImageTrans;
	
	IF ImageTrans.nf_handle_error("Embedded SQL: UPDATE DOC","w_document_indexing","wf_switch_documents") < 0 THEN
		RETURN -1
	END IF

/*	STEP 8. If the folder containing the new document contained only the new document, then
	delete it as it will use the folder from the old document when switched.
*/
	SELECT count(*)
	  INTO :li_documentcount
	  FROM REF
	 WHERE docfldid = :ll_newdocfldid
	 USING ImageTrans;

	IF ImageTrans.nf_handle_error("Embedded SQL: Update DOC","w_document_indexing","inv_imaging.nf_archive_document") < 0 THEN
		RETURN -1
	END IF

	IF li_documentcount = 0 THEN
		DELETE FLD
		 WHERE fldid = :ll_newdocfldid
		 USING ImageTrans;

		IF ImageTrans.nf_handle_error("Embedded SQL: Update DOC","w_document_indexing","inv_imaging.nf_archive_document") < 0 THEN
			RETURN -1
		END IF
	END IF

/*	STEP 9a. Switch the docid to the new docid for all work folders.
*/
	UPDATE REF
		SET docid = :ll_newdocid
	 WHERE docid = :ll_olddocid
		AND doccatid <> 2
	 USING ImageTrans;

	IF ImageTrans.nf_handle_error("Embedded SQL: Delete from REF","w_document_indexing","inv_imaging.nf_archive_document") < 0 THEN
		RETURN -1
	END IF

/* STEP 9b.  Update the DOC for the ll_newdocid, set the docrefcount = docrefcount + the number of rows that
					where affected in STEP 9a.
*/

	UPDATE 	DOC
	SET		docrefcount = docrefcount + :ImageTrans.SQLNRows
	WHERE		docid = :ll_newdocid
	USING		ImageTrans;
	
	IF ImageTrans.nf_handle_error("Embedded SQL: UPDATE DOC","w_document_indexing","wf_switch_documents") < 0 THEN
		RETURN -1
	END IF

/*  STEP 9c.  Update the DOC for the ll_olddocid, set the docrefcount = 1.  The docrefcount should always be 1
					at this point.  In STEP 9a all references to ll_olddocid are removed except for the catid 2
					and there must be a catid 2 therefore the dorefcount should be 1.
*/

	UPDATE 	DOC
	SET		docrefcount = 1
	WHERE		docid = :ll_olddocid
	USING		ImageTrans;
	
	IF ImageTrans.nf_handle_error("Embedded SQL: UPDATE DOC","w_document_indexing","wf_switch_documents") < 0 THEN
		RETURN -1
	END IF
	

/*	STEP 10. Move the master REF entry for the document to archive category.
*/
	UPDATE REF
	SET doccatid = 258,
		 docfldid = :ll_fldid
	FROM REF
	WHERE doccatid = 2
	  AND docfldid = :ll_masterfldid
	  AND docid    = :ll_olddocid
	USING ImageTrans;

	IF ImageTrans.nf_handle_error("Embedded SQL: Update REF","w_document_indexing","inv_imaging.nf_archive_document") < 0 THEN
		RETURN -1
	END IF

	IF ImageTrans.SQLNRows <> 1 THEN
		ImageTrans.nf_rollback_transaction()
		MessageBox("Document Archive","Unable to update REF.",StopSign!)
		RETURN -1
	END IF

/*	STEP 11. Insert REF entry to place copy of document in the master category.
*/
	INSERT INTO REF (docid, docfldid, doccatid, docsetid )
	VALUES (:ll_newdocid, :ll_masterfldid, 2, 2 )
	USING ImageTrans;

	IF ImageTrans.nf_handle_error("Embedded SQL: Insert into REF","inv_imaging.nf_archive_document","cb_route") < 0 THEN
		RETURN -1
	END IF

	IF ImageTrans.SQLNRows <> 1 THEN
		ImageTrans.nf_rollback_transaction()
		MessageBox("Document Indexing","Could not place copy of document in master folder.~r~nDocument not indexed and routed.")
		RETURN -1
	END IF

/*	STEP 12. Increment the document counter to show a new copy of the document	has been created and update
	the doc name.
*/
	ls_comment	= uo_document_indexing.dw_document_indexing.GetItemString(1,"comment") 
	ls_type		= uo_document_indexing.dw_document_indexing.GetItemString(1,"type_code")
	ls_doc_sub_type_code		= uo_document_indexing.dw_document_indexing.GetItemString(1,"doc_subtype_code")
	ldt_date		= uo_document_indexing.dw_document_indexing.GetItemDateTime(1,"date_on_document") 
	ls_docname	= ls_type + STRING(ldt_date,'yyyymmdd') + ' ' + ls_comment
	UPDATE DOC
		SET docrefcount = docrefcount + 1,
			 docname     = :ls_docname
	 WHERE docid = :ll_newdocid
	 USING ImageTrans;

	IF ImageTrans.nf_handle_error("Embedded SQL: Update DOC","w_document_indexing","cb_route") < 0 THEN
		RETURN -1
	END IF

	IF ImageTrans.SQLNRows <> 1 THEN
		ImageTrans.nf_rollback_transaction()
		MessageBox("Document Indexing","Could not place copy of document in master folder.~r~nDocument not indexed and routed.")
		RETURN -1
	END IF

	RETURN 0

end function

public subroutine wf_set_index_entry_mode (boolean vab_index_entry_on);//	This function enables or disables objects depending on whether user is entering index data
LONG		ll_row
STRING 	ls_module_source_code

If vab_index_entry_on Then
	dw_select_category.Enabled = False
	dw_document_list.Enabled = False
	uo_document_indexing.Enabled = True
	dw_duplicate_documents.Hide()

	cb_close.Enabled = False
	cb_delete_document.Enabled = False
	cb_move.Enabled = False
	cb_remlink.Enabled = False
	cb_refresh.Enabled = False
	cb_switch2.Enabled = False
	cb_route.Enabled = True
	cb_cancel.Enabled = True

	gb_routing_options.Enabled = True
	rb_full_route.Enabled = True
	rb_index_only.Enabled = True
	
	// Form67 and Medical Aid Documents can't be recalled
	ll_row = dw_document_list.GetRow()
	IF ll_row > 0 THEN
		ls_module_source_code = dw_document_list.GetItemString(ll_row, "module_source_code")
		IF  ls_module_source_code = "13"  OR ls_module_source_code = "15" THEN //web form67 (15)
			dw_options.Enabled 	= FALSE
		ELSE
			dw_options.Enabled 	= TRUE
		END IF
	END IF
Else
	dw_select_category.Enabled = True
	dw_document_list.Enabled = True
	uo_document_indexing.Enabled = False
	dw_options.Enabled = False

	cb_close.Enabled = True
	cb_delete_document.Enabled = True
	cb_move.Enabled = True
	cb_remlink.Enabled = True
	cb_refresh.Enabled = True
	cb_switch2.Enabled = False
	cb_route.Enabled = False
	cb_cancel.Enabled = False

	gb_routing_options.Enabled = False
	rb_full_route.Enabled = False
	rb_index_only.Enabled = False
End If

end subroutine

public function integer wf_set_claim (long al_claim_no);// wf_set_claim_no
//
Long     ll_count, ll_orig_claim_no, ll_count_claims
Integer  li_rtn
String   ls_type_code
Datetime ldt_eligibility_start_date, ldt_date_on_document

IF uo_document_indexing.dw_document_indexing.RowCount() = 1 THEN
	// An ARX document can only be re-indexed to a registered claim
	ls_type_code = uo_document_indexing.dw_document_indexing.GetItemString(1, "type_code")	
	IF ls_type_code = "ARX" THEN
		ll_orig_claim_no = uo_document_indexing.dw_document_indexing.GetItemNumber(1, "claim_no", Primary!, TRUE)
		IF ll_orig_claim_no > 0 AND ll_orig_claim_no <> al_claim_no THEN
			SELECT COUNT(*)
			  INTO :ll_count 
			  FROM X001_REGISTRATION 
			 WHERE claim_no = :al_claim_no ; 

			li_rtn = SQLCA.nf_handle_error("w_document_indexing", "", "wf_set_claim - SELECT COUNT(*) FROM X001_REGISTRATION")

			IF ll_count <= 0 THEN
				Messagebox("Invalid Claim #", "An ARX document can only be re-indexed to a registered claim.  " +&
							  String(al_claim_no) + " is not registered for drug coverage.", Information!)
				uo_document_indexing.dw_document_indexing.SetItem(1, "claim_no", ll_orig_claim_no)
				uo_document_indexing.dw_document_indexing.SetColumn("claim_no")
				uo_document_indexing.dw_document_indexing.SetFocus()
				RETURN -1
			END IF
			
			// Don't allow ARX document type to be re-indexed to another claim if the date on the document < eligibility start date
			ldt_date_on_document = uo_document_indexing.dw_document_indexing.GetItemDatetime(1, "date_on_document")
			
			SELECT COUNT(*)
			INTO   :ll_count_claims
			FROM   CLAIM_ELIGIBILITY
			WHERE  claim_no = :al_claim_no
			AND    eligibility_start_date <= :ldt_date_on_document
			AND    (eligibility_end_date >= :ldt_date_on_document OR eligibility_end_date IS NULL)  // PR20872 > add conditional checking for a null end date
			USING  SQLCA;
			
			IF SQLCA.nf_handle_error("w_document_indexing", "", "wf_set_claim - SELECT eligibility_start_date FROM CLAIM_ELIGIBILITY") < 0 THEN RETURN -1
			
			IF ll_count_claims = 0 THEN
				Messagebox("Invalid Claim #", "An ARX document can only be re-indexed to another claim as long as it " +&
							  "has coverage for the same date.", Information!)
				uo_document_indexing.dw_document_indexing.SetItem(1, "claim_no", ll_orig_claim_no)
				uo_document_indexing.dw_document_indexing.SetColumn("claim_no")
				uo_document_indexing.dw_document_indexing.SetFocus()
				RETURN -1
			END IF
		END IF
	END IF	
	
	uo_document_indexing.dw_document_indexing.SetItem(1, "claim_no", al_claim_no)

	IF uo_document_indexing.uf_set_claim(al_claim_no) <> 1 THEN
		uo_document_indexing.dw_document_indexing.SetItem(1, "claim_no", 0)
	END IF
END IF

RETURN 0
end function

public function integer wf_delete_document (long al_docid, long al_fldid, long al_rownum);/* This function deletes the selected document from a folder and will delete the folder
	if deleting the document leaves the folder empty.
*/
	LONG		ll_count
	STRING	ls_commit, ls_create, ls_drop, ls_begin
	BOOLEAN	lb_needtodelete, lb_erroroccured

	lb_needtodelete = FALSE
	lb_erroroccured = FALSE
	

	ls_create = 'CREATE TABLE #Pgfids_To_Delete (pgfid int not null)'
	EXECUTE IMMEDIATE :ls_create USING ImageTrans;

	IF ImageTrans.nf_handle_error('CREATE TABLE #Pgfids_To_Delete (pgfid int not null)','w_document_indexing','wf_delete_document()') < 0 THEN
		RETURN -1
	END IF

	lb_needtodelete = TRUE
	
	

/*	First delete the row from the datawindow. This is the REF entry.
*/
	dw_document_list.DeleteRow(al_rownum)


	ImageTrans.nf_begin_transaction()

/*	Update the database.
*/
	dw_document_list.Update()
	IF ImageTrans.nf_handle_error("dw_document_list","w_document_indexing","wf_delete_document()") < 0 THEN
		lb_erroroccured = TRUE
		GOTO DeleteTableAfterError
	END IF

/*	Check to see if this document only exists here. This is possible if it has not been indexed yet.
*/
	SELECT count(*)
	  INTO :ll_count
	  FROM REF
	 WHERE docid = :al_docid
	 USING ImageTrans;

	IF ImageTrans.nf_handle_error("Embedded SQL: Select from REF","w_document_indexing","wf_delete_document()") < 0 THEN
		lb_erroroccured = TRUE
		GOTO DeleteTableAfterError
	END IF

/* If the count is > 0 then the document exists elsewhere, so just remove a reference to it.
*/
	IF ll_count > 0 THEN
		UPDATE DOC
			SET docrefcount = docrefcount - 1
		 WHERE docid = :al_docid
		 USING ImageTrans;

		IF ImageTrans.nf_handle_error("Embedded SQL: Update DOC","w_document_indexing","wf_delete_document") < 0 THEN
			lb_erroroccured = TRUE
			GOTO DeleteTableAfterError
		END IF

		IF ImageTrans.SQLNRows <> 1 THEN
			ImageTrans.nf_rollback_transaction()

			IF ImageTrans.SQLCode <> 0 THEN
				Error.Text = "Error during rollback of update DOC"
				Error.WindowMenu=""
				Error.Object="w_document_indexing"
				Error.ObjectEvent="wf_delete_document"
				SignalError()
			END IF
			MessageBox("Document Indexing","Could not delete document. Please refresh and try again.")
			lb_erroroccured = TRUE
			GOTO DeleteTableAfterError
		END IF
	ELSE

/*	Document only exists in this folder, so delete the document, its privileges, and its logical page entries.
*/
		DELETE
		  FROM DOC
		 WHERE docid = :al_docid
		 USING ImageTrans;

		IF ImageTrans.nf_handle_error("Embedded SQL: Delete DOC","w_document_indexing","wf_delete_document()") < 0 THEN
			lb_erroroccured = TRUE
			GOTO DeleteTableAfterError
		END IF

		IF ImageTrans.SQLNRows <> 1 THEN
			ImageTrans.nf_rollback_transaction()

			IF ImageTrans.SQLCode <> 0 THEN
				Error.Text = "Error during rollback of delete DOC"
				Error.WindowMenu=""
				Error.Object="w_document_indexing"
				Error.ObjectEvent="wf_delete_document()"
				SignalError()
			END IF
			MessageBox("Document Indexing","Could not delete document. Please refresh and try again.")
			lb_erroroccured = TRUE
			GOTO DeleteTableAfterError
		END IF
		
/* we have deleted the only DOC so we need to also delete the Docid_Reference_Xref table */
		DELETE
		  FROM Docid_Reference_Xref
		 WHERE docid = :al_docid
		 USING ImageTrans;

		ImageTrans.nf_handle_error("Embedded SQL: Delete Docid_Reference_Xref","w_document_indexing","wf_delete_document()") 

		INSERT INTO #Pgfids_To_Delete
			(pgfid)
		SELECT pagfid
		  FROM PAG
		 WHERE pagdocid = :al_docid
		 USING ImageTrans;

		IF ImageTrans.nf_handle_error('INSERT INTO #Pgfids_To_Delete','w_document_indexing','wf_delete_document()') < 0 THEN
			lb_erroroccured = TRUE
			GOTO DeleteTableAfterError
		END IF

/*	Delete the logical pointer to the physical page if there are any.
*/
		DELETE
		  FROM PAG
		 WHERE pagdocid = :al_docid
		 USING ImageTrans;

		IF ImageTrans.nf_handle_error("Embedded SQL: DELETE FROM PAG","w_document_indexing","wf_delete_document()") < 0 THEN
			lb_erroroccured = TRUE
			GOTO DeleteTableAfterError
		END IF

/*	Delete the physical page reference if there are any (document may have no pages).
*/
		DELETE PGF
		  FROM PGF, #Pgfids_To_Delete
		 WHERE PGF.pgfid = #Pgfids_To_Delete.pgfid
		 USING ImageTrans;

		IF ImageTrans.nf_handle_error("Embedded SQL: Delete PGF","w_document_indexing","wf_delete_document()") < 0 THEN
			lb_erroroccured = TRUE
			GOTO DeleteTableAfterError
		END IF
		
		/* Delete the document from the MED_AID_E_INVOICE table if it exists	
		PR7662 - Doc_id was not being set to 0 when the document was deleted. 
		PR8504 - moved code - deleted_flag and docid should only be set when it is not indexed to a claim
		Also, removed code that logically deletes document from F67 tables - these documents should 
		not be deleted, regardless of if they are indexed or not.
	    */		
		UPDATE CLAIM..MED_AID_E_INVOICE
		SET deleted_flag = 'Y',
				 doc_id         = 0
		where doc_id = :al_docid
		USING ImageTrans;
		
		ImageTrans.nf_handle_error('w_document_indexing','wf_delete_document','UPDATE MED_AID_E_INVOICE')
		
	END IF

/*	If this leaves an empty folder then delete it.
*/
	SELECT count(*)
	  INTO :ll_count
	  FROM REF
	 WHERE docfldid = :al_fldid
	 USING ImageTrans;

	IF ImageTrans.nf_handle_error("Embedded SQL: Select from REF","w_document_indexing","wf_delete_document()") < 0 THEN
		lb_erroroccured = TRUE
		GOTO DeleteTableAfterError
	END IF

/*	If the count is 0 then the document we are deleting is the only one in there.
*/
	IF ll_count = 0 THEN
		DELETE FLD
		 WHERE fldid = :al_fldid
		 USING ImageTrans;

		IF ImageTrans.nf_handle_error("Embedded SQL: Delete from FLD","w_document_indexing","wf_delete_document()") < 0 THEN
			lb_erroroccured = TRUE
			GOTO DeleteTableAfterError
		END IF

		IF ImageTrans.SQLNRows <> 1 THEN
			ImageTrans.nf_rollback_transaction()

			IF ImageTrans.SQLCode <> 0 THEN
				Error.Text = "Error during rollback of delete FLD"
				Error.WindowMenu=""
				Error.Object="w_document_indexing"
				Error.ObjectEvent="wf_delete_document()"
				SignalError()
			END IF
			MessageBox("Document Indexing","Could not delete folder. Please refresh and try again.")
			lb_erroroccured = TRUE
			GOTO DeleteTableAfterError
		END IF
	/*PR 7355 - 2008-05-12 - If deleting the FLD record, then delete the CLAIM_WORKING record if it exists
	*/
		DELETE CLAIM_WORKING
		WHERE  folderid = :al_fldid
		USING ImageTrans;
		
		ImageTrans.nf_handle_error('w_document_indexing', 'wf_delete_document', 'delete from CLAIM_WORKING')
	// end PR7355	
	END IF
	
DeleteTableAfterError:
/* The dropping of a temporary table must be outside of a transaction. Make the server's transaction closed,
	drop the table, then turn the transaction back on.
*/
	IF lb_needtodelete THEN
		
		ImageTrans.nf_commit_transaction()
				
		ls_drop = 'DROP TABLE #Pgfids_To_Delete'
		EXECUTE IMMEDIATE :ls_drop USING ImageTrans;

		IF ImageTrans.nf_handle_error('DROP TABLE #Pgfids_To_Delete','w_document_indexing','wf_delete_document()') < 0 THEN
			RETURN -1
		END IF
	   	
	END IF

	IF lb_erroroccured THEN
		RETURN -1
	ELSE
		RETURN 0
	END IF

end function

public function integer wf_update_referral ();long ll_claim_no, ll_rehab_referral_no, ll_referring_provider_no, ll_docid, ll_task_no
string ls_referring_provider_type_code, ls_rehab_service_code
datetime	ldtm_referred_on_date



// Update the REHAB_REFERRAL table if the referral flag is checked on.

/*	Initialize the variables.
*/
ll_claim_no                               = uo_document_indexing.dw_document_indexing.GetItemNumber(1,"claim_no") 
ll_referring_provider_no             = uo_document_indexing.dw_document_indexing.GetItemNumber(1,"service_provider_no") 
ls_referring_provider_type_code = uo_document_indexing.dw_document_indexing.GetItemString(1,"service_provider_type_code") 
ll_docid                                     = uo_document_indexing.dw_document_indexing.GetItemNumber(1,"docid")
ldtm_referred_on_date               = uo_document_indexing.dw_document_indexing.GetItemDatetime(1,"date_on_document")


//get the last rehab_referrral_no from the last number table
//ll_rehab_referral_no = 

//hardcoded for the physio service
ls_rehab_service_code = 'S022'

ll_task_no = 0

Return 1
end function

public function long wf_last_referral_no ();// passes back the referral no


Return 1
end function

protected function long wf_auto_routing (long al_docid, long al_fldid, string as_doc_type, string as_doc_class_type, string as_claim_status, string as_status_type, boolean ab_not_a_correction);	LONG 	  	ll_rownum, 				 	ll_claim,  ll_default_catid, ll_taxing_catid, &
			  	ll_destination_catid, 	ll_fldid,  ll_setid,			  ll_finalled_taxing_catid, &
			  	ll_lost_time_catid,	 	ll_no_lost_time_catid,		  ll_adjud_catid, &
			  	ll_finalled_daily_catid, ll_result, ll_seq_no

	INT	  	li_count,		li_indexed_count,	li_timebased_autorouting_period, li_rows

	STRING  	ls_destination_status,   ls_region, ls_casemgr, 	ls_case_managed_flag,  &
			  	ls_action, ls_note,   ls_name,	ls_claim_status,	ls_job_position, &
				ls_type, ls_doc_sub_type_code

	DATETIME	ldtm_status_date,	ldtm_accident_date
	
	DATE		ldt_finalled_date, ls_date

	BOOLEAN	lb_medical_report,	lb_letter,	lb_timebased_autoroute_cas
	
	U_DS     lds_routing_exceptions


/*
	Set the value of the time-based auto-routing period in number of days
	Three months at approximately 30 days per month = 90 days
	
	2013-10-23 - J. Hawker - The above has been changed from three months to one year, 365 days
*/
	li_timebased_autorouting_period = 365
	il_setid = 0

// Get the claim's number, region,  person assigned and whether the claim is "case managed"
	ll_claim   	 			= iw_sheet.dw_basic_claim.GetItemNumber(1,"claim_no")
	ls_region  	 			= iw_sheet.dw_basic_claim.GetItemString(1,"admin_region_code")
	ls_casemgr 	 			= iw_sheet.dw_basic_claim.GetItemString(1,"claim_manager_user_id")
//	ls_case_managed_flag = iw_sheet.dw_basic_claim.GetItemString(1,"case_managed_flag")
	ldtm_accident_date 	= iw_sheet.dw_basic_claim.GetItemDateTime(1,"accident_date")
	
	ls_type			= uo_document_indexing.dw_document_indexing.GetItemString(1,"type_code")
	ls_doc_sub_type_code = uo_document_indexing.dw_document_indexing.GetItemString(1,"doc_subtype_code")
		
		
	
/* 
	Handle Routing_Exceptions first
*/	

   lds_routing_exceptions = Create U_DS
	lds_routing_exceptions.DataObject = 'ds_routing_exceptions'
	lds_routing_exceptions.SetTransObject(SQLCA)
	
	li_rows = lds_routing_exceptions.Retrieve(as_doc_type,ls_region)
	
	IF li_rows = 1 THEN
		ll_destination_catid = lds_routing_exceptions.GetItemNumber(1,'routing_catid')
	ELSEIF li_rows > 1 THEN
		// error!
		MessageBox('Document Indexing Error','An attempt to determine the exception category associated with this document type and the admin region has resulted in an error. Contact the HELPDESK.',Exclamation!)
		Return -1
	ELSE
		// NO EXCEPTION found
		
	/* If the claim is assigned to a Claim Manager, determine if the Claim is case-managed by checking the 
		job-position of the user assigned to the claim. A job position code of "CASMGR" indicates the user is a Case Manager.
	*/
		ls_case_managed_flag = "N"
		ls_job_position = ""
	
		IF ls_casemgr <> "" THEN
			SELECT job_position_code INTO :ls_job_position
				FROM User_Profile
				WHERE user_id = :ls_casemgr and active_flag = "Y" using SQLCA;
			SQLCA.nf_handle_error("Embedded SQL: Select from User_Profile","w_document_indexing","wf_auto_routing")
			 
			IF ls_job_position = "CASMGR" THEN
				ls_case_managed_flag = "Y"
			END IF
		END IF
	
	
	/* Determine the finalled date of the Claim if the claim Status is Finalled. If the Finalled date cannot be determined , 
		default to the Claim's Accident date (prior to CIS Replacement on Oct 14/96 there were no event entries for
		tracking claim status changes).
	*/
		IF as_claim_status = "F" THEN
			SELECT  max(seq_no) , max(create_date) INTO :ll_seq_no, :ldtm_status_date 
				FROM CLAIM_STATUS_CHANGE
				WHERE claim_no = :ll_claim  AND 
						new_claim_status_code = "F" using SQLCA;
			SQLCA.nf_handle_error("Embedded SQL: Select from CLAIM_STATUS_CHANGE","w_document_indexing","wf_auto_routing")
	
			IF ISNULL(ldtm_status_date) THEN
				ldt_finalled_date = date(ldtm_accident_date)
			ELSE
				ldt_finalled_date = date(ldtm_status_date)
			END IF
	
		END IF
	 
		lb_timebased_autoroute_cas = false
	
	/* Determine if the claim meets the criteria for Time-Based Auto-routing of Finalled Claims
		If the claim is finalled by a case manager (i.e. is a case-managed claim) 
			If the finalled date is beyond the 1 year period after finalling the claim (3 month period has been changed to 1 year - P10151-287)
				the document is to be routed to the CAS unit 
			Otherwise (it is within the 1 year period of finalling the claim) (3 month period has been changed to 1 year - P10151-287)
				document routing follows the normal path
	*/
		IF as_claim_status = "F" AND ls_case_managed_flag = "Y" 		AND  &
			(relativedate(ldt_finalled_date , li_timebased_autorouting_period) < idt_todays_date ) THEN	
			lb_timebased_autoroute_cas = true
		END IF
	
	
	/* Set up some defaults
	*/
		ls_action = "ZZ"
		ls_name   = Trim(iw_sheet.dw_basic_claim.GetItemString(1,"given_names")) + " " + Trim(iw_sheet.dw_basic_claim.GetItemString(1,"last_name"))
		
		IF ls_doc_sub_type_code <> '' THEN
			ls_note   = ls_type +'-'+ ls_doc_sub_type_code
		ELSE
			ls_note   = ls_type
		END IF
	
	
	/* Get the default categories based on the claim's region
	*/
		ll_rownum = 1
	
		ll_rownum = dw_routing_defaults.Find("admin_region_code='" + ls_region + "'",1,dw_routing_defaults.RowCount())
		IF ll_rownum > 0 THEN
			ll_default_catid 		  	 = dw_routing_defaults.GetItemNumber(ll_rownum,"daily_routing_catid")
			ll_taxing_catid 			 = dw_routing_defaults.GetItemNumber(ll_rownum,"taxing_catid")
			ll_lost_time_catid		 = dw_routing_defaults.GetItemNumber(ll_rownum,"lost_time_catid")
			ll_no_lost_time_catid	 = dw_routing_defaults.GetItemNumber(ll_rownum,"no_lost_time_catid")
			ll_adjud_catid			  	 = dw_routing_defaults.GetItemNumber(ll_rownum,"adjud_catid")
			ll_finalled_taxing_catid = dw_routing_defaults.GetItemNumber(ll_rownum,"finalled_taxing_catid")
			ll_finalled_daily_catid  = dw_routing_defaults.GetItemNumber(ll_rownum,"finalled_daily_catid")
		ELSE
			MessageBox("Document Indexing","Could not find default routing categories.~r~nDocument not indexed or routed.")
			Return -1
		END IF

		
		/*
			A. ROUTING OF PRE-ADJUDICATED AND ADJUDICATION CLAIMS
			 P10151-304a  - 2014-10-26 - R.S. added new functionality for status of 'Adjudication'
		*/
		
		// If the claim's status is pre-adjudication ("P") OR  adjudication ("J") then the document will either
		// go to who is assigned to the claim, or, in the case of status = P, the lost time or no lost time category
		// depending on what the user has specified, and in the case of "J", then to the default adjudication category ID 
		// defined by the value of ll_adjud_catid (taken from the Routing_Defaults table)  
		
		IF as_claim_status = "P" OR as_claim_status = "J" THEN
	
			IF ls_casemgr <> "" THEN
				ll_rownum = dw_routing_assignments.Find("admin_region_code='" + ls_region + "' And claim_manager_user_id = '" + ls_casemgr + "'",1,dw_routing_assignments.RowCount())
				IF ll_rownum > 0 THEN
					ll_destination_catid = dw_routing_assignments.GetItemNumber(ll_rownum, "claim_manager_catid")
				END IF
			ELSE
				IF as_claim_status = "J" THEN  // send it to the cat id for adjudication
				
					ll_destination_catid = ll_adjud_catid
					
				ELSEIF as_claim_status = "P" THEN //send it to NLT or LT category
					
					IF uo_document_indexing.dw_document_indexing.GetItemString(1,"lost_time") = "Y" THEN
						ll_destination_catid = ll_lost_time_catid
						ls_action		      = "LT"
					ELSE
						ll_destination_catid = ll_no_lost_time_catid
						ls_action		      = "NL"
					END IF
				END IF
			END IF
		END IF
	
	/* B. ROUTING OF ACCOUNTS 
	
		If the Document is an Account (A%, SDC,SDD,MPC,MPD) and the claim's status is ACTIVE or FINALLED (but not an
		out-of-province claim), then route the document to a "taxing" category.  
	
		The regional taxing category id was acquired when the default categories was acquired above so only reset the 
		taxing category if it is NOT to be routed to the regional taxing category.
	*/
		IF (Left(as_doc_type,1) = "A" OR as_doc_type = "SDC" OR as_doc_type = "SDD" OR as_doc_type = "MPC" OR as_doc_type = "MPD") AND  &
			((as_claim_status = "F" AND as_status_type <> "05") OR (as_claim_status = "A") )        THEN
			/* 
				If the Claim is NOT Case-Managed:
					>>> route the document to the CAS unit taxing category
				If the claim is Case-Managed:
					1. If the claim's status is Finalled and:
						a) The finalled date of the claim is beyond the 1 year period of finalling the claim (3 month period has been changed to 1 year - P10151-287)
							>>> route the document to the CAS unit taxing category
						b) The finalled date of the claim is within the 1 year period of finalling the claim (3 month period has been changed to 1 year - P10151-287)
							>>> route the document to the regional taxing category 
					2. If the claim's status is Active
							>>> route the document to the regional taxing category 
			*/
			
			IF ls_case_managed_flag <> "Y"  THEN	
				ll_taxing_catid = ll_finalled_taxing_catid
			ELSE
				IF as_claim_status = "F" THEN
					IF lb_timebased_autoroute_cas THEN
						ll_taxing_catid = ll_finalled_taxing_catid
					END IF
				END IF
			END IF
	
	
			/* If the claim's status is Active and a taxing category for the claim manager is specified, then the document
				will be routed to the claim manager's taxing category instead of the regional or CAS taxing category.
				(As at 1996/12/06 the CAS claim managers are the only users that have a taxing category specified and
				the taxing category is the "PC-Taxing CAS" category for all of them).
			*/
			IF as_claim_status = "A" THEN
	
				ll_rownum = 1
				ll_destination_catid = 0
	
				ll_rownum = dw_routing_assignments.Find("admin_region_code='" + ls_region + "' And claim_manager_user_id = '" + ls_casemgr + "'",1,dw_routing_assignments.RowCount())
				IF ll_rownum > 0 THEN
					ll_finalled_taxing_catid = dw_routing_assignments.GetItemNumber(ll_rownum, "taxing_catid")
				ELSE
					ll_finalled_taxing_catid = 0
				END IF
	
				IF ll_finalled_taxing_catid > 0 THEN
					ll_taxing_catid = ll_finalled_taxing_catid
				END IF			
			END IF
	
			// Validate that the "taxing" catid exists (in the CAT table)
			ll_destination_catid = 0
	
			SELECT setid, catid INTO :ll_setid, :ll_destination_catid
				FROM CAT
				WHERE catid = :ll_taxing_catid
				USING ImageTrans;
	
			ImageTrans.nf_handle_error("Embedded SQL: Select from CAT","w_document_indexing","wf_auto_routing")
	
			IF ll_destination_catid = 0 THEN
				ll_destination_catid = ll_default_catid
				ll_taxing_catid = 0
			END IF
	
			IF ll_destination_catid = 0 THEN	
				ll_destination_catid = ll_taxing_catid
			END IF
			
		END IF	
	
	/*
		C. ROUTING TO THE CLAIM MANAGER'S CATEGORY ....
	
		If the document is to be routed based on time-based auto-routing:
			If the finalled date is beyond the 1 year period after finalling the claim (3 month period has been changed to 1 year - P10151-287)
				the document is routed to the CAS unit (i.e. the regional Daily Routing categories)
			Otherwise (it is within the1 year period of finalling the claim) (3 month period has been changed to 1 year - P10151-287)
				the document is routed to the Claim Manager assigned to the claim
		Otherwise:
			the document is routed to the Claim Manager assigned to the claim
	
		These documents can be routed to the Claim Manager when the following conditions are met:
		1. If the document is not an account (except for combined forms - SDC, SDD, MPC, MPD) and
			the claim's status is Active Or is Finalled as "Lost Time Medical Aid Only" (03) or "No Lost Time" (04) then
			the document should go to the user id assigned to the claim.
	 
		2.	If the claim's status is Rejected (06, 07, 08, 09, 10) - regardless of the document type - then
			the document should go to the user id assigned to the claim.
	 
		3.	If the document is a Medical Report (i.e. of type MP, MH, MD, SD, SE, SDC, SDD, MPC, MPD )
			and the claim's status is Finalled as "Final" (01) or "First & Final" (02) then
			the document should go to the user id assigned to the claim.
		
		4.	If the document is a Letter (i.e. of type LO, LC, LE, LR, BA)
			and the claim's status is Finalled as "Final" (01) then
			the document should go to the user id assigned to the claim.
	*/
		lb_medical_report = false
		lb_letter = false
	
		IF (as_doc_type = "MP" OR as_doc_type = "MH" 	OR as_doc_type = "MD"  OR as_doc_type = "SD" 	OR &
			as_doc_type = "SE"  OR as_doc_type = "SDC" 	OR as_doc_type = "SDD" 	OR as_doc_type = "MPC" 	OR &
			as_doc_type = "MPD")	THEN
			lb_medical_report = True
		END IF
		
		//for documents of type 'L'etter or doc type code BA ( Affidavits ), and status is finaled (final - 01) then route to claim manager if possible
		// P10151-304a - Removed the hard coded values for doc type codes and use the doc CLASS code (of 'L'etter) instead of 
		//                       doc TYPE code 'LO', 'LC' etc because some letter types were not being included
		IF (as_doc_class_type = "L" OR as_doc_type = "BA") THEN
			lb_letter = true
		END IF
	
		IF (Left(as_doc_type,1) <> "A" AND ((as_claim_status) = "A"  OR as_status_type = "03" 	OR as_status_type = "04")) 	OR &
			((as_claim_status) = "R") 																														OR & 
			((as_status_type = "01" OR as_status_type = "02") 	AND lb_medical_report ) 													OR &
			(as_status_type = "01" AND lb_letter) THEN
	
			/* If the claim's status is Active then find the default Adjudication category.
				This is where claims ready for adjudication and unassigned major claims go.
			*/
			IF as_claim_status = "A" THEN
				ll_destination_catid = ll_adjud_catid
			END IF
	
			/* If the claim is Case-Managed and finalled and the finalled date is beyond the 1 year period after finalling the claim  (3 month period has been changed to 1 year - P10151-287)
					>>> route the document to the CAS unit (i.e. the regional Daily Routing categories)
				Otherwise 
					>>> route the document to the Claim Manager assigned to the claim
			*/
			IF lb_timebased_autoroute_cas THEN
				ll_destination_catid = ll_default_catid
			ELSE
				/*	If the casemgr field is not blank then find the Claim Manager's category to send the document
				*/
				IF ls_casemgr <> "" THEN
					ll_rownum = 1
					ll_destination_catid = 0
	
					ll_rownum = dw_routing_assignments.Find("admin_region_code ='" + ls_region + "' And claim_manager_user_id = '" + ls_casemgr + "'",1,dw_routing_assignments.RowCount())
					IF ll_rownum > 0 THEN
						ll_destination_catid = dw_routing_assignments.GetItemNumber(ll_rownum, "claim_manager_catid")
					END IF
	
					IF ll_destination_catid = 0 THEN
						ll_destination_catid = ll_default_catid
					END IF		
				ELSE
					ll_destination_catid = ll_default_catid
				END IF
			END IF
			
	
			/* Combined-Forms: create the Working Folder and create the REF table entry for these documents  
				which are routed to two different categories. 
	
				A Working Folder is created for routing the document to the Taxing Category. The category identifier of the
				Claim Manager's category will be updated on the FDL entry to "move" the folder (created when the document was 
				scanned in) to the Claim Manager's Category.
	
				If the Claim is NOT a rejected Claim (i.e. Claim Status is either "A" or "F"), 
				a combined-form document (SDC, SDD, MPC, MPD) is routed to two locations:
					a) to the Claim Manager's category (determined above) if the claim is assigned to a Claim Manager
					b) to a Taxing Category (determined above) if the Claim is Active OR if the Claim is finalled and not 
						an out-of-province claim    
			*/
		
			IF ll_taxing_catid > 0 AND (as_doc_type = "SDC" OR as_doc_type = "SDD" OR as_doc_type = "MPC" OR as_doc_type = "MPD") AND &
				(as_claim_status = "A" OR (as_claim_status = "F" And as_status_type <> "05"))          THEN
	
				// Create a workfolder to put a copy of the document in it
				
				// this function begins & commits its own transaction, then inserts data outside of txn,
				// so it must be enclosed within its own txn. In this case the begin txn is placed before call
				// of this function (wf_auto_routing)
				
				ll_fldid = inv_imaging.nf_create_workfolder("w_document_indexing",ll_taxing_catid) 
				If ll_fldid = -1 Then
					MessageBox("Document Indexing","Document will not be routed or indexed. Please try again.")
					Return -1
				End If
	
				// Create claimsworking entry for the new folder
				INSERT INTO CLAIM_WORKING (folderid, claim_no, action_code)
					VALUES (:ll_fldid, :ll_claim, "ZZ")
					USING ImageTrans;
				ImageTrans.nf_handle_error("Embedded SQL: Insert into CLAIM_WORKING","w_document_indexing","wf_auto_routing")
	
				// Put a copy of the document in the folder
				INSERT INTO REF (docid, docfldid, doccatid, docsetid )
					VALUES (:al_docid, :ll_fldid, :ll_taxing_catid, :ll_setid )
					USING ImageTrans;
				ImageTrans.nf_handle_error("Embedded SQL: Insert into REF","w_document_indexing","wf_auto_routing")
				
				If ImageTrans.SQLNRows <> 1 Then
					ImageTrans.nf_rollback_transaction()
					MessageBox("Document Indexing","Could not place copy of document in workfolder.~r~nDocument not indexed and routed.")
					Return -1
				End If
	
				// Increment the document counter to show a new copy of the document has been created
				UPDATE DOC
					SET docrefcount = docrefcount + 1
					WHERE docid = :al_docid
					USING ImageTrans;
				ImageTrans.nf_handle_error("Embedded SQL: Update DOC","w_document_indexing","wf_auto_routing")
				
				IF ImageTrans.SQLNRows <> 1 THEN
					ImageTrans.nf_rollback_transaction()
					MessageBox("Document Indexing","Could not place copy of document in workfolder.~r~nDocument not indexed and routed.")
					Return -1
				END IF
			END IF		
		END IF
	
	/*
		D. DETERMINE NUMBER OF DOCUMENTS TO BE ROUTED.
	*/
	
	// Count the number of documents in the current folder
		li_count = 0
	
		SELECT count(*) INTO :li_count
		FROM REF
		WHERE docfldid = :al_fldid
		USING ImageTrans;
	
		ImageTrans.nf_handle_error("Embedded SQL: Select from REF","w_document_indexing","wf_auto_routing")
	
		If li_count > 1 Then
			// Count the number of indexed documents in the folder
			SELECT count(*) INTO :li_indexed_count
			FROM REF, DOCUMENT_INDEX
			WHERE REF.docid = DOCUMENT_INDEX.docid
			AND REF.docfldid = :al_fldid
			USING ImageTrans;
	
			ImageTrans.nf_handle_error("Embedded SQL: Select from REF and docindex","w_document_indexing","wf_auto_routing")
	
			If li_count <> li_indexed_count Then
				ImageTrans.nf_rollback_transaction()
				MessageBox("Document Indexing","Could not route current document as other documents exist in it's folder. Call the HELPDESK.")
				Return -1
			End If
		Else
			li_indexed_count = 1
		End If
	
	
	/* 
		E. DEFAULT ROUTING IF CATEGORY IS NOT YET DETERMINED...
	*/
	
	// If no other destination has been found then route the folder to Daily Mail
		If ll_destination_catid = 0 Then
			
			//P10151-304 R.S. - as a final fall thru scenario,  if a claim manager exists, and has a routing assignment, route any unassigned document 
			//                          to the claim manager's bucket, otherwise route it to the default bucket for the region
			IF ls_casemgr <> "" THEN
				ll_rownum = dw_routing_assignments.Find("admin_region_code='" + ls_region + "' And claim_manager_user_id = '" + ls_casemgr + "'",1,dw_routing_assignments.RowCount())
					IF ll_rownum > 0 THEN
						ll_destination_catid = dw_routing_assignments.GetItemNumber(ll_rownum, "claim_manager_catid")
					ELSE
						ll_destination_catid = ll_default_catid
					END IF
			ELSE
				ll_destination_catid = ll_default_catid
			END IF
		End If
	END IF
	
/* 
	F. MOVE THE FOLDER TO ITS' DESTINATION CATEGORY
		Update the current in-basket folder with the category id of the destination category determined
		in this function.
*/

	IF ab_not_a_correction THEN
		UPDATE FLD
		   SET fldcatid = :ll_destination_catid
		 WHERE fldid = :al_fldid
		 USING ImageTrans;
	ELSE
		UPDATE FLD
			SET fldcatid = :ll_destination_catid,
				 fldname = Upper(:ls_action) + CONVERT(varchar(10),:ll_claim) + :ls_name
		 WHERE fldid = :al_fldid
		 USING ImageTrans;
	END IF

	ImageTrans.nf_handle_error("Embedded SQL: Update FLD","w_document_indexing","wf_auto_routing")

	If ImageTrans.SQLNRows <> 1 Then
		ImageTrans.nf_rollback_transaction()
		
		MessageBox("Document Indexing","Could not route folder. Please refresh and try again.")
		Return -1
	End If

/*
	G. CODE THE WORKING FOLDER
*/
	IF ab_not_a_correction THEN
		ll_result = inv_imaging.nf_code_claimsworking("w_document_indexing",al_fldid,ll_claim,ls_action,Date(f_server_datetime()),ls_note,ls_name)
		IF ll_result <> 1 THEN
			RETURN -1
		END IF
	ELSE
		ls_date = Date(f_server_datetime())
		UPDATE CLAIM_WORKING
			SET action_code = :ls_action,
				 claim_no = :ll_claim,
				 action_note = :ls_note,
				 action_date = :ls_date
		 WHERE folderid = :al_fldid
 		 USING ImageTrans;

		ImageTrans.nf_handle_error("Embedded SQL: UPDATE CLAIM_WORKING","w_document_indexing","wf_auto_routing()")
	END IF

/*	Determine the setid for the destination catid.
*/
	SELECT setid
	  INTO :il_setid
	  FROM CAT
	 WHERE catid = :ll_destination_catid
	 USING ImageTrans;

	ImageTrans.nf_handle_error("Embedded SQL: SELECT setid INTO :il_setid","w_document_indexing","wf_auto_routing()")
			
	IF il_setid = 0 THEN
		ImageTrans.nf_rollback_transaction()
		
		MessageBox("Document Indexing","Could not find a valid set id for the determined category.")
		RETURN -1
	END IF

	Return ll_destination_catid

end function

event open;call super::open;DATETIME		 ldtm_now
s_window_message lstr_message


lstr_message = Message.PowerObjectParm


// Get the name of the active work sheet
iw_sheet = lstr_message.apo_powerobjectparm[1]
If IsValid(iw_sheet) = False Then
   MessageBox("Document Indexing","Error determining active sheet. You may have to close WorkBench and try again.")
	Close(This)
	Return
End If


im_cmwb = iw_active_sheet.MenuId       //  EdL

// Create an instance of the user object for the imaging functions
inv_imaging = CREATE n_imaging

// Create an instance of the user object for the referral functions
inv_referral = CREATE n_referral

// Create an instance of the user object for the event functions
inv_event_log = CREATE n_event_log



// The scanning category has a setid of 6
Long ll_setid = 6
dw_options.InsertRow(0)

dw_select_category.GetChild("catid",idwc_categories)
idwc_categories.SetTransObject(ImageTrans)
dw_document_list.SetTransObject(ImageTrans)
dw_duplicate_documents.SetTransObject(ImageTrans)
dw_event.SetTransObject(SQLCA)
dw_routing_defaults.SetTransObject(ImageTrans)
dw_routing_assignments.SetTransObject(ImageTrans)
dw_indexing_paid_documents.SetTransObject(SQLCA)

// Indexers currently don't want to waste their time looking the document type
// up in a list and they don't want to have to tab through the field either
// so just protect it so it just shows the description
uo_document_indexing.dw_document_indexing.Modify("document_desc.Protect=1")
uo_document_indexing.ib_restrict_type_code_changes = False

// Get all the buckets/categories in the scanning set
idwc_categories.Retrieve(ll_setid)
If ImageTrans.nf_handle_error("vidwc_categories","w_document_indexing","open for w_document_indexing") < 0 then
	Close(This)
	Return
End If

// Retrieve the routing data
dw_routing_defaults.Retrieve()
If ImageTrans.nf_handle_error("dw_routing_defaults","w_document_indexing","open for w_document_indexing") < 0 then
	Close(This)
	Return
End If

dw_routing_assignments.Retrieve()
If ImageTrans.nf_handle_error("dw_routing_assignments","w_document_indexing","open for w_document_indexing") < 0 then
	Close(This)
	Return
End If

ib_reset_rte_opts_reqd = TRUE

dw_select_category.InsertRow(0)

dw_document_list.uf_setselect(1)

/* Retrieve the System date - used by the auto-routing function
*/
ldtm_now  = f_server_datetime()
idt_todays_date = date(ldtm_now)

/* PR #970  Added by Ed Lenarczyk  May 10, 2000
	to prevent Alt-x
*/
IF IsValid(im_cmwb.m_workbench.m_exit) THEN im_cmwb.m_workbench.m_exit.Disable()

// removed the following code which is to be determined



end event

event closequery;call super::closequery;If IsValid(inv_imaging) Then
	Destroy inv_imaging
End If
end event

on w_document_indexing.create
int iCurrent
call super::create
this.cb_switch2=create cb_switch2
this.cb_remlink=create cb_remlink
this.cb_cancel=create cb_cancel
this.st_index_only=create st_index_only
this.st_full_routing=create st_full_routing
this.rb_index_only=create rb_index_only
this.rb_full_route=create rb_full_route
this.cb_refresh=create cb_refresh
this.dw_event=create dw_event
this.dw_routing_assignments=create dw_routing_assignments
this.dw_routing_defaults=create dw_routing_defaults
this.dw_indexing_paid_documents=create dw_indexing_paid_documents
this.cb_move=create cb_move
this.dw_options=create dw_options
this.st_2=create st_2
this.dw_document_list=create dw_document_list
this.cb_delete_document=create cb_delete_document
this.dw_select_category=create dw_select_category
this.cb_route=create cb_route
this.cb_alt1=create cb_alt1
this.gb_routing_options=create gb_routing_options
this.uo_image_append=create uo_image_append
this.cb_search=create cb_search
this.cb_recall=create cb_recall
this.uo_document_indexing=create uo_document_indexing
this.cb_refresh_cat=create cb_refresh_cat
this.dw_duplicate_documents=create dw_duplicate_documents
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_switch2
this.Control[iCurrent+2]=this.cb_remlink
this.Control[iCurrent+3]=this.cb_cancel
this.Control[iCurrent+4]=this.st_index_only
this.Control[iCurrent+5]=this.st_full_routing
this.Control[iCurrent+6]=this.rb_index_only
this.Control[iCurrent+7]=this.rb_full_route
this.Control[iCurrent+8]=this.cb_refresh
this.Control[iCurrent+9]=this.dw_event
this.Control[iCurrent+10]=this.dw_routing_assignments
this.Control[iCurrent+11]=this.dw_routing_defaults
this.Control[iCurrent+12]=this.dw_indexing_paid_documents
this.Control[iCurrent+13]=this.cb_move
this.Control[iCurrent+14]=this.dw_options
this.Control[iCurrent+15]=this.st_2
this.Control[iCurrent+16]=this.dw_document_list
this.Control[iCurrent+17]=this.cb_delete_document
this.Control[iCurrent+18]=this.dw_select_category
this.Control[iCurrent+19]=this.cb_route
this.Control[iCurrent+20]=this.cb_alt1
this.Control[iCurrent+21]=this.gb_routing_options
this.Control[iCurrent+22]=this.uo_image_append
this.Control[iCurrent+23]=this.cb_search
this.Control[iCurrent+24]=this.cb_recall
this.Control[iCurrent+25]=this.uo_document_indexing
this.Control[iCurrent+26]=this.cb_refresh_cat
this.Control[iCurrent+27]=this.dw_duplicate_documents
end on

on w_document_indexing.destroy
call super::destroy
destroy(this.cb_switch2)
destroy(this.cb_remlink)
destroy(this.cb_cancel)
destroy(this.st_index_only)
destroy(this.st_full_routing)
destroy(this.rb_index_only)
destroy(this.rb_full_route)
destroy(this.cb_refresh)
destroy(this.dw_event)
destroy(this.dw_routing_assignments)
destroy(this.dw_routing_defaults)
destroy(this.dw_indexing_paid_documents)
destroy(this.cb_move)
destroy(this.dw_options)
destroy(this.st_2)
destroy(this.dw_document_list)
destroy(this.cb_delete_document)
destroy(this.dw_select_category)
destroy(this.cb_route)
destroy(this.cb_alt1)
destroy(this.gb_routing_options)
destroy(this.uo_image_append)
destroy(this.cb_search)
destroy(this.cb_recall)
destroy(this.uo_document_indexing)
destroy(this.cb_refresh_cat)
destroy(this.dw_duplicate_documents)
end on

event close;call super::close;
/* PR #970  Added by Ed Lenarczyk  May 10, 2000
	to restore Alt-x
	Make sure there is a Work Sheet open when trying to close the Indexing module.
	This has once caused a problem...
*/
IF IsValid(im_cmwb.m_workbench.m_exit) THEN im_cmwb.m_workbench.m_exit.Enable()

post close(iw_sheet)

end event

type st_title from w_a_tool`st_title within w_document_indexing
integer x = 23
string text = "Document Indexing"
end type

type cb_close from w_a_tool`cb_close within w_document_indexing
integer x = 1509
integer y = 684
integer width = 311
integer height = 92
integer taborder = 220
string text = "Cl&ose"
end type

type cb_switch2 from commandbutton within w_document_indexing
integer x = 1751
integer y = 1712
integer width = 334
integer height = 92
integer taborder = 150
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "S&witch"
end type

event clicked;Long   ll_rownum, ll_docid, ll_result, ll_claimno, ll_newdocid, ll_olddocid
String ls_type, ls_region, ls_referral_flag
LONG		ll_count

// Get the current document
	ll_rownum = dw_duplicate_documents.GetRow()
	If ll_rownum <= 0 Then
		MessageBox("Document Indexing","Could not determine document to switch.")
		Return
	End If

	ll_docid		= dw_duplicate_documents.GetItemNumber(ll_rownum,"docid")
	ls_type		= uo_document_indexing.dw_document_indexing.GetItemString(1,"type_code")
	ll_claimno  = uo_document_indexing.dw_document_indexing.GetItemNumber(1,"claim_no")
	
// Document cannot be switched if one document is a physio referral and the other is not	
	ll_olddocid = ll_docid
	
	Select count(*)
	Into   :ll_count
	From REHAB_REFERRAL
	Where doc_id = :ll_olddocid
	Using SQLCA;
	
	SQLCA.nf_handle_error('w_document_indexing','cb_switch2.clicked','Select * from REHAB_REFERRAL')
	
	ls_referral_flag = uo_document_indexing.dw_document_indexing.getitemstring(1,"referral_flag")
	IF ls_referral_flag = "" or IsNull(ls_referral_flag) THEN ls_referral_flag = 'N' 
	IF (ls_referral_flag = 'Y' and ll_count = 0 ) THEN
		MessageBox("Document Indexing","Document could not be switched. The document that you are trying to switch with is a referral document.", Exclamation!)
		Return
	ELSEIF  (ls_referral_flag = 'N' and ll_count > 0 ) THEN 
		MessageBox("Document Indexing","Document could not be switched. This document is a referral and the document you are trying to switch with is not.", Exclamation!)
		Return
	End If
	
	
// Documents with payment information are not allowed to be archived, unless link broken first...
//	Setup a loop that can only be exited if control exited OR no payment links exist. This script structure chosen
// so as to allow for repeated testing of payment link existance on return from w_breakpay.
	Do While True
		// Determine if payment link exists
		ll_result = dw_indexing_paid_documents.Retrieve(ll_docid)
		If SQLCA.nf_handle_error("dw_indexing_paid_documents","w_document_indexing","clicked for cb_route") < 0 then
			Return
		End If

		// If a payment link exists...
		If ll_result > 0 Then
			If MessageBox("Document Indexing","Cannot switch with a document that has been marked as paid. Do you wish to break the payment / document link?",Question!,YesNo!) = 1 THEN
				// If payment link exists and the user has indicated a desire to break the link, then call window to allow removal of linkage
				istr_window_message.al_doubleparm[1] = ll_docid
				OpenWithParm(w_breakpay,istr_window_message)
			Else
			// If payment link exists and the user has indicated NO desire to break the link, then exit the control
				Return
			End If
    		Else
			// If no payment link exists, then exit the loop and continue to SWITCH
			Exit
		End If
	Loop


	ImageTrans.nf_begin_transaction()

// Switch the documents.
	If wf_switch_documents() = -1 Then
		MessageBox("Document Indexing","Document could not be switched. Please refresh and try again.")
		Return
	End If

// Move the new document in by indexing the document
	uo_document_indexing.dw_document_indexing.Update()
	If ImageTrans.nf_handle_error("dw_document_indexing","w_document_indexing","clicked for cb_route") < 0 then
		Return
	End If

// If the document is marked as paid then record it in the Claim database
	If uo_document_indexing.dw_document_indexing.GetItemString(1,"paid_flag") = 'Y' Then
		ll_docid = uo_document_indexing.dw_document_indexing.GetItemNumber(1,"docid")
		dw_indexing_paid_documents.Reset()
		dw_indexing_paid_documents.InsertRow(0)
		dw_indexing_paid_documents.SetItem(1,"doc_id",ll_docid)
		dw_indexing_paid_documents.SetItem(1,"paid_status_code","O")
		dw_indexing_paid_documents.SetItem(1,"paid_status_explanation_code","05")
		dw_indexing_paid_documents.SetItem(1,"payment_no",0)
		
		SQLCA.nf_begin_transaction()
		
		dw_indexing_paid_documents.Update()
		If SQLCA.nf_handle_error("dw_indexing_paid_documents","w_document_indexing","clicked for cb_route") < 0 then
			ImageTrans.nf_rollback_transaction()
			SQLCA.nf_rollback_transaction()
			Return
		End If

		SQLCA.nf_commit_transaction()
	End If
	
	/* do the work we need for the new document indexed */
	/* grab the new docid which was set in the switching function */
	ll_docid = uo_document_indexing.dw_document_indexing.GetItemNumber(1,"docid")
	
	/* if this the first time the user has indexed then we need to insert otherwise update */
	SELECT COUNT(*) INTO :ll_count FROM  USER_LAST_INDEXED_DOC WHERE user_id = :vgst_user_profile.user_id
   USING ImageTrans;
	ImageTrans.nf_handle_error("w_document_indexing","wf_switch_documents()","SELECT COUNT(*) INTO :ll_count FROM")

	IF ll_count = 1 THEN
	  UPDATE USER_LAST_INDEXED_DOC  set docid = :ll_docid
		WHERE user_id = :vgst_user_profile.user_id
		USING ImageTrans;

      ImageTrans.nf_handle_error("w_document_indexing","wf_switch_documents()","UPDATE USER_LAST_INDEXED_DOC")
	ELSE
		INSERT USER_LAST_INDEXED_DOC (user_id,docid)
		VALUES (:vgst_user_profile.user_id,:ll_docid)
		USING ImageTrans;
      ImageTrans.nf_handle_error("w_document_indexing","clicked","INSERT USER_LAST_INDEXED_DOC ")
	END IF 
	
	/* update the Docid_Reference_Xref table so that the docid has it's indexed flag
	   set to "Y" there may not be a record here in which case we don't really care.
	*/
	UPDATE Docid_Reference_Xref  SET indexed_flag = "Y"
	 WHERE docid = :ll_docid
	 USING ImageTrans;

   ImageTrans.nf_handle_error("w_document_indexing","clicked","UPDATE Docid_Reference_Xref")
	
	ImageTrans.nf_commit_transaction()


//Update the REHAB_REFERRAL with the new docid if the switch was done

	IF ls_referral_flag = 'Y'  THEN
		
		SQLCA.nf_begin_transaction()
		
		Update REHAB_REFERRAL
		Set doc_id = :ll_docid
		Where doc_id = :ll_olddocid
		Using SQLCA;
		
		SQLCA.nf_handle_error('w_document_indexing','cb_switch2.clicked','Select * from REHAB_REFERRAL')
		
		SQLCA.nf_commit_transaction()
	END IF 

// Close any open viewers
	ll_result = 1
	Do While ll_result > 0
		ll_result = f_close_viewer()
	Loop
// Open the current document in viewer
	uo_image_append.of_init(ll_docid)
		

// Refresh the document list
	cb_refresh.TriggerEvent(Clicked!)

end event

type cb_remlink from commandbutton within w_document_indexing
integer x = 878
integer y = 684
integer width = 311
integer height = 92
integer taborder = 160
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Del &Link"
end type

event clicked;long   ll_result,	ll_docid

/* under some instances the buttons are enabled when they probably shouldn't be
   check done here to ensure that there is a record in the datawindow
	before grabbing the value
*/
IF dw_document_list.rowcount() < 1 THEN RETURN

ll_docid  = dw_document_list.GetItemNumber(1,"ref_docid")

ll_result = dw_indexing_paid_documents.Retrieve(ll_docid)

If SQLCA.nf_handle_error("dw_indexing_paid_documents","w_document_indexing","clicked for cb_remlink") < 0 Then
		Return
	ElseIf ll_result = 0 then
	   MessageBox("Document Indexing","Document is NOT an account with payment records therefore link does not exist.", Exclamation!)
		Return
End If

istr_window_message.al_doubleparm[1] = ll_docid
OpenWithParm(w_breakpay,istr_window_message)
end event

type cb_cancel from commandbutton within w_document_indexing
integer x = 1408
integer y = 1712
integer width = 334
integer height = 92
integer taborder = 140
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Cance&l"
end type

event clicked;Long   ll_row, ll_docid, ll_handle
String ls_auto_created_flag
INTEGER	li_rtn



/* NOTE NOTE NOTE - PB10.5 ISSUE NOTE NOTE NOTE 
    IT APPEARS THAT THE FOLLOWING SETFILTER CODE RETURNS A (-1).
    WHICH USUALLY INDICATES THAT THE DATAOBJECT has not been assigned
    this is not the case. an isvalid returns fine as does the handle so not sure why it does not
    work. In the help file it states that the return value is not normally used. The code has been changed to adhere to this.
   could because it's an instance datawindow child on a user object? not really sure
*/
	//Remove the filter if module_source_code <> '12'
//    li_rtn = uo_document_indexing.idwc_class_code.SetFilter("")
//	IF li_rtn = -1 Then SignalError(-666,'Error Setting filter')	 
//	
//	li_rtn = uo_document_indexing.idwc_class_code.Filter()  
//	IF li_rtn = -1 Then SignalError(-666,'Error Setting filter')	
//		
//	li_rtn = uo_document_indexing.idwc_document_type.SetFilter("")
//	IF li_rtn = -1 Then SignalError(-666,'Error Setting filter')	
//	
//	li_rtn = uo_document_indexing.idwc_document_type.Filter()
//	IF li_rtn = -1 Then SignalError(-666,'Error filtering')
//
//    li_rtn = uo_document_indexing.idwc_subtype_code.SetFilter("")
//	IF li_rtn = -1 Then SignalError(-666,'Error Setting filter')	 
//	li_rtn = uo_document_indexing.idwc_subtype_code.Filter()  
//	IF li_rtn = -1 Then SignalError(-666,'Error Setting filter')	

//MessageBox('after insert', uo_document_indexing.idwc_document_type_code.Describe('datawindow.table.filter') + '  rows = ' + String(uo_document_indexing.ldwc_document_type_code.RowCOunt()) + ' filteredcount = ' + String(ldwc_document_type_code.FilteredCOunt()))


// If it is desired that the indexing dw is not 'blanked', omit the following 1 line
uo_document_indexing.dw_document_indexing.Reset()

dw_options.SetItem(1,"keep_claim","N")
dw_options.SetItem(1,"keep_type","N")
dw_options.SetItem(1,"keep_date","N")
dw_options.SetItem(1,"keep_sender","N")

//rb_full_route.Checked = FALSE
//rb_index_only.Checked = FALSE

cb_search.visible = False
cb_alt1.Enabled = FALSE
uo_document_indexing.cb_provider_search.Visible = False
uo_document_indexing.ib_paid_document = False
dw_duplicate_documents.Hide()

wf_set_index_entry_mode(FALSE)

// PR #970  added by Ed Lenarczyk  May 10, 2000 to make sure the routing info is cleared when canceling.
rb_full_route.Checked = FALSE
rb_index_only.Checked = FALSE

// Only allow claim number to be modified when Document_Type_Code.auto_created_flag = "Y"
ll_row = dw_document_list.GetRow()
IF ll_row > 0 THEN
	ll_docid = dw_document_list.GetItemNumber(ll_row, "ref_docid")

	SELECT DTC.auto_created_flag 
	  INTO :ls_auto_created_flag 
	  FROM DOCUMENT_INDEX DI, 
			 Document_Type_Code DTC 
	 WHERE DI.docid = :ll_docid 
		AND DI.type_code = DTC.type_code 
	 USING ImageTrans ; 
	
	ImageTrans.nf_handle_error("w_document_indexing", "", "cb_cancel - SELECT DTC.auto_created_flag FROM DOCUMENT_INDEX DI, Document_Type_Code DTC")
	
	IF ls_auto_created_flag = "Y" THEN
		cb_move.Enabled = FALSE
		cb_delete_document.Enabled = FALSE
		cb_remlink.Enabled = FALSE

		rb_full_route.Enabled = FALSE
		rb_full_route.Checked = FALSE
		rb_index_only.Checked = TRUE
		uo_document_indexing.cb_provider_search.Enabled = FALSE
	END IF
END IF

end event

type st_index_only from statictext within w_document_indexing
integer x = 727
integer y = 1720
integer width = 283
integer height = 76
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Index Only"
boolean focusrectangle = false
end type

type st_full_routing from statictext within w_document_indexing
integer x = 297
integer y = 1720
integer width = 325
integer height = 76
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Full Routing"
boolean focusrectangle = false
end type

type rb_index_only from radiobutton within w_document_indexing
integer x = 626
integer y = 1720
integer width = 64
integer height = 76
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
end type

on clicked;IF rb_index_only.checked = TRUE THEN
	wf_indexing_or_routing(True)
End If

uo_document_indexing.dw_document_indexing.SetFocus()
uo_document_indexing.dw_document_indexing.SetColumn(il_curr_col)
end on

on getfocus;il_curr_col = uo_document_indexing.dw_document_indexing.GetColumn()
end on

type rb_full_route from radiobutton within w_document_indexing
integer x = 224
integer y = 1716
integer width = 73
integer height = 76
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
boolean enabled = false
end type

on clicked;IF rb_full_route.checked = TRUE THEN
	If ib_already_indexed Then
		MessageBox("Document Indexing","Document is already indexed and can not be 'fully' routed.")
		rb_full_route.Checked = FALSE
		uo_document_indexing.dw_document_indexing.SetItem(1,"message_info","")
	Else
		wf_indexing_or_routing(False)
	End If
End If 

uo_document_indexing.dw_document_indexing.SetFocus()
uo_document_indexing.dw_document_indexing.SetColumn(il_curr_col)

end on

on getfocus;il_curr_col = uo_document_indexing.dw_document_indexing.GetColumn()
end on

type cb_refresh from commandbutton within w_document_indexing
integer x = 1746
integer y = 200
integer width = 87
integer height = 64
integer taborder = 120
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&R"
end type

event clicked;ib_reset_rte_opts_reqd = FALSE
dw_select_category.PostEvent(ItemChanged!)
end event

type dw_event from u_dw_online within w_document_indexing
boolean visible = false
integer x = 2638
integer y = 992
integer height = 364
integer taborder = 100
boolean enabled = false
string dataobject = "d_indexing_incoming_correspondence"
end type

type dw_routing_assignments from u_dw_online within w_document_indexing
boolean visible = false
integer x = 2629
integer y = 380
integer height = 364
integer taborder = 90
boolean enabled = false
string dataobject = "d_routing_assignments"
end type

type dw_routing_defaults from u_dw_online within w_document_indexing
boolean visible = false
integer x = 2633
integer y = 624
integer height = 364
integer taborder = 60
boolean enabled = false
string dataobject = "d_routing_defaults"
end type

type dw_indexing_paid_documents from u_dw_online within w_document_indexing
boolean visible = false
integer x = 2624
integer y = 244
integer height = 364
integer taborder = 50
boolean enabled = false
string dataobject = "d_indexing_paid_documents"
end type

type cb_move from commandbutton within w_document_indexing
integer x = 503
integer y = 684
integer width = 370
integer height = 92
integer taborder = 80
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Move Folder"
end type

on clicked;Long vll_fldid, vll_rownum

vll_rownum = dw_document_list.GetRow()
If vll_rownum <= 0 Then
	MessageBox("Document Indexing","Could not determine selected folder. Please try again.")
	Return
End If

vll_fldid = dw_document_list.GetItemNumber(vll_rownum,"ref_docfldid")

OpenWithParm(w_indexing_forward_folder,vll_fldid)

cb_refresh.PostEvent(Clicked!)
end on

type dw_options from u_dw_online within w_document_indexing
integer x = 192
integer y = 1536
integer width = 2153
integer height = 116
integer taborder = 210
boolean enabled = false
string dataobject = "d_options"
boolean border = false
end type

on getfocus;il_curr_col = uo_document_indexing.dw_document_indexing.GetColumn()
end on

on itemchanged;uo_document_indexing.dw_document_indexing.SetFocus()
uo_document_indexing.dw_document_indexing.SetColumn(il_curr_col)

end on

type st_2 from statictext within w_document_indexing
integer x = 14
integer y = 96
integer width = 274
integer height = 76
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Category :"
alignment alignment = right!
boolean focusrectangle = false
end type

type dw_document_list from u_dw_online within w_document_indexing
integer y = 192
integer width = 2633
integer height = 480
integer taborder = 30
string dataobject = "d_documents_by_category"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;call super::rowfocuschanged;// Open the highlighted document
Long     ll_rownum, ll_docid, ll_result, ll_claim_no, ll_fldid, ll_ref, ll_count, ll_catid, ll_subtype
Long     ll_sender,ll_doc_check, ll_previous_ref, ll_rows, ll_provider_no, ll_num_form67_errors, ll_worker_sin_no
Integer  li_rtn
String   ls_type, ls_sender, ls_sender_type, ls_paid, ls_oldtype, ls_comment, ls_doc_type, ls_auto_created_flag, ls_class_code, ls_comment2
DateTime	ldt_date, ldt_date_of_service, ldt_invoice_date
Datetime ldt_form67_submit_datetime, ldt_form67_resubmit_datetime, ldt_snb_export_datetime, ldt_create_date
Boolean  lb_reference_no_protect = FALSE
u_ds		lds_med_aid_e_invoice_indexing, lds_form67_indexing
String   ls_module_source_code, ls_provider_type_code, ls_default_type_code, ls_provider_name, ls_contact_language_code, ls_type_code_desc, ls_doc_class_code_desc
String   ls_create_user_id, ls_indexer_name, ls_doc_subtype_code, ls_doc_class_code, ls_default_class_code, ls_default_sub_type_code, ls_doc_type_desc_e, ls_doc_subtype_desc_e
String   ls_oldsubtype, ls_type_code,ls_doc_type_code

DataWindowChild	ldwc_service_provider_type_code, ldwc_document_type

u_ds		lds_med_aid_e_invoice_search_defaults, lds_form67_search_defaults, lds_web_form67_indexing, lds_web_form67_search_defaults
s_window_message lstr_message
s_claim_search_defaults   lstr_claim_search_defaults

If is_curr_mode = "Retrieve" Then Return

// Determine the current document
ll_rownum = GetRow()
If ll_rownum = 0 Then
	Return
End If

// Hide the duplicate documents window in case it is showing
dw_duplicate_documents.Hide()

// Highlight the document
dw_document_list.uf_processselect(ll_rownum,"Keyboard")

// Get the document id and folder id
ll_docid = GetItemNumber(ll_rownum,"ref_docid")
ll_fldid = GetItemNumber(ll_rownum,"ref_docfldid")
ls_module_source_code = GetItemString(ll_rownum,"module_source_code")

// setup the buttons here
wf_set_index_entry_mode(TRUE)

// set the delete default to Y this will make the messagebox appear
ib_del_confirm_reqd = TRUE

// If there is indexing information then save it for the next document if the user wants
If uo_document_indexing.dw_document_indexing.RowCount() = 1 Then
	ll_previous_ref = uo_document_indexing.dw_document_indexing.GetItemNumber(1,"reference_no")
	ll_previous_ref++

	If dw_options.GetItemString(1,"keep_claim") = "Y" Then
		ll_claim_no = uo_document_indexing.dw_document_indexing.GetItemNumber(1,"claim_no")
	End If
	
	If dw_options.GetItemString(1,"keep_type") = "Y" Then
		ls_doc_class_code = uo_document_indexing.dw_document_indexing.GetItemString(1,"doc_class_code")
		ls_type = uo_document_indexing.dw_document_indexing.GetItemString(1,"type_code")
		ls_doc_subtype_code = uo_document_indexing.dw_document_indexing.GetItemString(1,"doc_subtype_code")
		ls_paid = uo_document_indexing.dw_document_indexing.GetItemString(1,"paid_flag")
	End If

	If dw_options.GetItemString(1,"keep_date") = "Y" Then
		ldt_date = uo_document_indexing.dw_document_indexing.GetItemDateTime(1,"date_received")
	End If

	If dw_options.GetItemString(1,"keep_sender") = "Y" Then
		ll_sender 		= uo_document_indexing.dw_document_indexing.GetItemNumber(1,"service_provider_no")
		ls_sender 		= uo_document_indexing.dw_document_indexing.GetItemString(1,"service_provider_name")
		ls_sender_type = uo_document_indexing.dw_document_indexing.GetItemString(1,"service_provider_type_code")
		ls_comment		= uo_document_indexing.dw_document_indexing.GetItemString(1,"comment")
	End If
Else
	ll_previous_ref = 0
End If

// Check to see if this document is already indexed
ib_already_indexed = False
uo_document_indexing.cb_provider_search.Visible = False
cb_search.Visible = False
cb_alt1.Enabled = FALSE

// check to see if the document does not exist in the Docid_Reference_Xref table if it does exist DO NOT allow 
// them to enter a reference_no if IT DOESN't exist then enable the reference_no field
ll_count = 0

SELECT count(*), reference_no, docid
  INTO :ll_count , :ll_ref , :ll_doc_check
  FROM Docid_Reference_Xref 
 WHERE docid = :ll_docid
 group by reference_no,docid
  USING ImageTrans;

ImageTrans.nf_handle_error("w_document_indexing","dw_document_list - rowfocuschanged","SELECT count(*) INTO :ll_count")

ll_result = uo_document_indexing.uf_retrieve(ll_docid, ib_already_indexed)

/*Filter the document type drop down
*/
//MessageBox(ls_module_source_code, ldwc_document_type_code.Describe('datawindow.table.filter') + '  rows = ' + String(ldwc_document_type_code.RowCOunt()) + ' filteredcount = ' + String(ldwc_document_type_code.FilteredCOunt()))

// Check if Import Medical Aid Invoice 
IF ls_module_source_code = "12" THEN
	//Filter the document type dropdown
	
	//set the class code & type code & remove the sub type doesn't apply
	uo_document_indexing.idwc_class_code.SetFilter("doc_class_code = 'A'")
	li_rtn = uo_document_indexing.idwc_class_code.Filter()
	uo_document_indexing.idwc_class_code.SetSort("#1 A")
	li_rtn = uo_document_indexing.idwc_class_code.Sort()
	
			
	li_rtn = uo_document_indexing.idwc_document_type.SetFilter("type_code in('AC','AH')")
	li_rtn = uo_document_indexing.idwc_document_type.Filter()
	li_rtn = uo_document_indexing.idwc_document_type.SetSort("#1 A")
	li_rtn = uo_document_indexing.idwc_document_type.Sort()

ELSE
	//Remove the filter if module_source_code <> '12'
	li_rtn = uo_document_indexing.idwc_class_code.SetFilter("")	
	li_rtn = uo_document_indexing.idwc_class_code.Filter()
	li_rtn = uo_document_indexing.idwc_class_code.SetSort("#1 A")
	li_rtn = uo_document_indexing.idwc_class_code.Sort()
	
	li_rtn = uo_document_indexing.idwc_document_type.SetFilter("")
	li_rtn = uo_document_indexing.idwc_document_type.Filter()
	li_rtn = uo_document_indexing.idwc_document_type.SetSort("#1 A")
	li_rtn = uo_document_indexing.idwc_document_type.Sort()

END IF

//MessageBox(ls_module_source_code, ldwc_document_type_code.Describe('datawindow.table.filter') + '  rows = ' + String(ldwc_document_type_code.RowCOunt()) + ' filteredcount = ' + String(ldwc_document_type_code.FilteredCOunt()))

If ll_result <= 0 Then  // It isn't indexed
	uo_document_indexing.dw_document_indexing.InsertRow(0)
	
	IF ll_ref > 0 THEN
		uo_document_indexing.dw_document_indexing.SetItem(1,"reference_no",ll_ref)
	ELSE
		uo_document_indexing.dw_document_indexing.SetItem(1,"reference_no",ll_previous_ref)
	END IF

	// Check if Import Medical Aid Invoice	
	IF ls_module_source_code = "12" THEN
		lds_med_aid_e_invoice_indexing = CREATE u_ds
		lds_med_aid_e_invoice_indexing.DataObject = 'd_med_aid_e_invoice_indexing'
		li_rtn = lds_med_aid_e_invoice_indexing.SetTransObject(SQLCA)
		
		IF li_rtn = -1 Then SignalError(-666,'Error setting transaction object SQLCA.')
		
		ll_rows = lds_med_aid_e_invoice_indexing.Retrieve(ll_docid)
		SQLCA.nf_handle_error('w_document_indexing','dw_document_list.RowFocusChanged()','lds_med_aid_e_invoice_indexing.Retrieve(' + String(ll_docid) + ')')
	
		IF ll_rows < 0 THEN
			SignalError(-666,'An error occured retrieving docid ' + String(ll_docid))
		ElseIF ll_rows = 0 Then
			SignalError(-666,'Error finding MED_AID_E_INVOICE record for docid ' + String(ll_docid))
		End if
		
		//Set values HERE
		ldt_date_of_service  = lds_med_aid_e_invoice_indexing.GetItemDateTime(1,'date_of_service')
		ldt_invoice_date		= lds_med_aid_e_invoice_indexing.GetItemDateTime(1,'invoice_date')
		ls_provider_type_code = lds_med_aid_e_invoice_indexing.GetItemString(1,'provider_type_code')
		ll_provider_no			 = lds_med_aid_e_invoice_indexing.GetItemNumber(1,'provider_no')
		
		ls_default_class_code = "A"
		ls_default_type_code = "AH"
		ls_default_sub_type_code = ""
		
		
		ls_doc_type_desc_e = ""
		ls_doc_subtype_desc_e = ""
		
		//display the document type desc
		Select  doc_type_desc_e
		Into     :ls_doc_type_desc_e
		From   Doc_Type	
		Where doc_type_code = :ls_default_type_code
		Using SQLCA;
		
		SQLCA.nf_handle_error("u_document_index", "dw_document_indexing", "SELECT  FROM Doc_Type_Subtype_Xref") 
		
		//display the document sub type desc
		Select  doc_subtype_desc_e
		Into     :ls_doc_subtype_desc_e
		From   Doc_Subtype	
		Where doc_subtype_code = :ls_default_sub_type_code
		Using SQLCA;
		
		SQLCA.nf_handle_error("u_document_index", "dw_document_indexing", "SELECT  FROM Doc_Type_Subtype_Xref") 
		
		
		uo_document_indexing.dw_document_indexing.SetItem(1,'date_on_document',ldt_date_of_service)
		uo_document_indexing.dw_document_indexing.SetITem(1,'date_received',ldt_invoice_date)
		uo_document_indexing.dw_document_indexing.SetItem(1,'service_provider_type_code',ls_provider_type_code)
		uo_document_indexing.dw_document_indexing.SetItem(1,'service_provider_no',ll_provider_no)
		uo_document_indexing.dw_document_indexing.SetItem(1,'doc_class_code',ls_default_class_code)
      uo_document_indexing.dw_document_indexing.SetItem(1,'type_code',ls_default_type_code)
		uo_document_indexing.dw_document_indexing.SetItem(1,'doc_subtype_code',ls_default_sub_type_code)
		uo_document_indexing.dw_document_indexing.SetItem(1,'document_desc',Trim(ls_doc_type_desc_e) +" "+Trim(ls_doc_subtype_desc_e))
		
		ls_provider_name = uo_document_indexing.inv_imaging.nf_service_provider_name(ll_provider_no, ls_provider_type_code, "w_document_indexing")		
		//Set the Provider Name
		uo_document_indexing.dw_document_indexing.SetItem(1,'service_provider_name',ls_provider_name)
		//Set the comment to the provider name
		IF Len(ls_provider_name) > 20 THEN
			ls_comment2= Trim(left(ls_provider_name,20))
		ELSE
			ls_comment2 = ls_provider_name 
		END IF			
		uo_document_indexing.dw_document_indexing.SetItem(1,'comment',ls_comment2)
		
		//PR5739 2006-05-04 r.searle, moved these two lines inside this if statement. In previous version, they were below it 
		// which caused the full route checkbox to be enabled for any of the other document source types, which users did not want.
		wf_indexing_or_routing(False)  
		rb_full_route.checked = TRUE
		
	// Check if Form 67 document & WEB FORM 67 (15)
	ELSEIF ls_module_source_code = "13" OR ls_module_source_code = "15" THEN
				
		lds_form67_indexing 					= CREATE u_ds
			
		IF ls_module_source_code = "13" THEN 		
			lds_form67_indexing.DataObject 	= 'd_form67_indexing'		
		ELSE			
			lds_form67_indexing.DataObject 	= 'd_web_form67_indexing'		
		END IF 
		
		IF  lds_form67_indexing.SetTransObject(SQLCA)	 = -1 THEN 	
			SignalError(-666,'Error setting transaction object SQLCA.')
		END IF 
		
		// Get Information from Form67
		ll_rows = lds_form67_indexing.Retrieve(ll_docid)
		SQLCA.nf_handle_error('w_document_indexing','dw_document_list.RowFocusChanged()','lds_form67_indexing.Retrieve(' + String(ll_docid) + ')')
	
		IF ll_rows < 0 THEN
			SignalError(-666,'An error occured retrieving docid ' + String(ll_docid))
		ElseIF ll_rows = 0 Then
			SignalError(-666,'Error finding Form67 record for docid ' + String(ll_docid))
		End if

		IF ls_module_source_code = "13" THEN 	
			ldt_form67_resubmit_datetime  = lds_form67_indexing.GetItemDateTime(1, 'form67_resubmit_datetime')
			ldt_snb_export_datetime 		= lds_form67_indexing.GetItemDateTime(1, 'snb_export_datetime')
		END IF 

		ldt_form67_submit_datetime  	= lds_form67_indexing.GetItemDateTime(1, 'form67_submit_datetime')
		ls_contact_language_code 		= lds_form67_indexing.GetItemString(1, 'contact_language_code')
		
		// Pre-set values from Form67
		SELECT 	doc_class_desc_e 
		INTO    	:ls_doc_class_code_desc
		FROM 		Doc_Class
		WHERE 	doc_class_code = 'S'
		USING 	ImageTrans;
		
		ImageTrans.nf_handle_error("w_document_indexing","dw_document_list - rowfocuschanged","SELECT doc_class_desc_e")
		
		SELECT 	doc_type_desc_e 
		INTO    	:ls_type_code_desc
		FROM 		Doc_Type
		WHERE 	doc_type_code = 'SE'
		USING 	ImageTrans;
		
		ImageTrans.nf_handle_error("w_document_indexing","dw_document_list - rowfocuschanged","SELECT doc_type_desc_e")
		
		uo_document_indexing.dw_document_indexing.SetItem(1, 'type_code', 'SE') // Form 67 - Employer's Report of Accident
		uo_document_indexing.dw_document_indexing.SetItem(1,'doc_class_code',"S")
		uo_document_indexing.dw_document_indexing.SetItem(1,'doc_subtype_code',"")
		uo_document_indexing.dw_document_indexing.Modify("doc_subtype_code.protect=1")
		uo_document_indexing.dw_document_indexing.Modify("doc_subtype_code.Background.mode=1")
		uo_document_indexing.dw_document_indexing.Modify("doc_subtype_code.Background.Color='553648127'")	
		uo_document_indexing.dw_document_indexing.SetItem(1, 'document_desc', Trim(ls_doc_class_code_desc)+" "+Trim(ls_type_code_desc))

		IF ls_module_source_code = "13" THEN 
			
				uo_document_indexing.dw_document_indexing.SetItem(1, 'date_received', ldt_snb_export_datetime)
			
			IF IsNull(ldt_form67_resubmit_datetime) = TRUE THEN
				uo_document_indexing.dw_document_indexing.SetItem(1, 'date_on_document', ldt_form67_submit_datetime)
			ELSE
				uo_document_indexing.dw_document_indexing.SetItem(1, 'date_on_document', ldt_form67_resubmit_datetime)
				uo_document_indexing.dw_document_indexing.SetItem(1, 'comment', 'Resubmission of a no-lost time')
			END IF
		ELSE
				uo_document_indexing.dw_document_indexing.SetItem(1, 'date_on_document', ldt_form67_submit_datetime)
				uo_document_indexing.dw_document_indexing.SetItem(1, 'date_received', ldt_form67_submit_datetime)
		END IF 
		
		uo_document_indexing.dw_document_indexing.SetItem(1, 'source_code', 'R')
		uo_document_indexing.dw_document_indexing.SetItem(1, 'paid_flag', 'N')
		uo_document_indexing.dw_document_indexing.SetItem(1, 'sent_flag', 'N')
		uo_document_indexing.dw_document_indexing.SetItem(1, 'service_provider_type_code', '')
		uo_document_indexing.dw_document_indexing.SetItem(1, 'service_provider_no', 0)
		
		IF ls_module_source_code = "13" THEN 
			SELECT ISNULL(COUNT(IFIE.import_no), 0) 
			  INTO :ll_num_form67_errors 
			  FROM I009_FORM67_IMPORT_ERROR IFIE, 
					 I009_FORM67_IMPORT IFI 
			 WHERE IFI.docid = :ll_docid 
				AND IFI.import_no = IFIE.import_no ;
	
			SQLCA.nf_handle_error('w_document_indexing', '','dw_document_list.RowFocusChanged - SELECT COUNT(IFIE.import_no) FROM I009_FORM67_IMPORT_ERROR')
			
			uo_document_indexing.dw_document_indexing.SetItem(1, 'num_form67_errors', ll_num_form67_errors)
		END IF 
		
		wf_indexing_or_routing(False)  
		rb_full_route.checked = TRUE
		
	End if
			
	iw_sheet.wf_clear_worksheet()
	iw_sheet.wf_initialize_basic_claim()

	IF ll_count > 0 THEN
		uo_document_indexing.dw_document_indexing.Modify("reference_no.protect=1")
		uo_document_indexing.dw_document_indexing.Modify("reference_no.Background.mode=1")
		uo_document_indexing.dw_document_indexing.Modify("reference_no.Background.Color='553648127'")
		lb_reference_no_protect = TRUE
	ELSE
		uo_document_indexing.dw_document_indexing.Modify("reference_no.protect=0")
		uo_document_indexing.dw_document_indexing.Modify("reference_no.Background.mode=0")
		uo_document_indexing.dw_document_indexing.Modify("reference_no.Background.Color='16777215'")
	END IF
		
Else	// It is indexed
	// PR 6130 - Document already indexed?  Don't check Corrections folder catid = 864
	IF ll_rownum > 0 THEN
		ll_catid = dw_document_list.GetItemNumber(ll_rownum, "ref_doccatid")
		IF ll_catid <> 864 THEN
			SELECT create_user_id, create_date 
			  INTO :ls_create_user_id, :ldt_create_date 
			  FROM DOCUMENT_INDEX  
			 WHERE docid = :ll_docid 
			 USING imagetrans ; 

			li_rtn = imagetrans.nf_handle_error('w_document_indexing', 'dw_document_list', 'rowfocuschanged - SELECT create_user_id, create_date FROM DOCUMENT_INDEX...')
			
			IF li_rtn = 0 THEN
				IF vgst_user_profile.user_name = "" THEN
					ls_indexer_name = ls_create_user_id 
				ELSE
					ls_indexer_name = vgst_user_profile.user_name
				END IF
				
				MessageBox("Document already Indexed", "This document was already indexed by " + ls_indexer_name + " on " +&
							  + DayName(Date(ldt_create_date)) + ", " + String(ldt_create_date, "mmm dd, yyyy") + " at " + String(ldt_create_date, "hh:mm:ss am/pm"))
				RETURN
			END IF
		END IF
	END IF
	
	wf_indexing_or_routing(True)  // Index Only
	rb_index_only.checked = TRUE

	// Check to see if the document is marked as paid
	dw_indexing_paid_documents.Retrieve(ll_docid)
	If ImageTrans.nf_handle_error("dw_indexing_paid_documents","w_document_indexing","rowfocuschanged for dw_document_indexing") < 0 then
		Return
	End If

	If dw_indexing_paid_documents.RowCount() > 0 Then
		uo_document_indexing.dw_document_indexing.SetItem(1,"paid_flag","Y")
		is_old_paid_flag = 'Y'
	End If
	
	ls_oldtype = uo_document_indexing.dw_document_indexing.GetItemString(1,"type_code")
	ls_oldsubtype = uo_document_indexing.dw_document_indexing.GetItemString(1,"doc_subtype_code")
	
	ls_doc_type_desc_e = ""
	ls_doc_subtype_desc_e = ""
		
	//display the document type desc
	Select  doc_type_desc_e
	Into     :ls_doc_type_desc_e
	From   Doc_Type	
	Where doc_type_code = :ls_oldtype
	Using SQLCA;
	
	SQLCA.nf_handle_error("u_document_index", "dw_document_indexing", "SELECT  FROM Doc_Type_Subtype_Xref") 
		
	//display the document sub type desc
	Select  doc_subtype_desc_e
	Into     :ls_doc_subtype_desc_e
	From   Doc_Subtype	
	Where doc_subtype_code = :ls_oldsubtype
	Using SQLCA;
		
	SQLCA.nf_handle_error("u_document_index", "dw_document_indexing", "SELECT  FROM Doc_Type_Subtype_Xref") 

	uo_document_indexing.dw_document_indexing.SetItem(1,"document_desc",Trim(ls_doc_type_desc_e)+" "+Trim(ls_doc_subtype_desc_e))
End If

uo_document_indexing.uf_date_received(False)
uo_document_indexing.cb_provider_search.Visible = True
cb_search.Visible = True
cb_alt1.Enabled = TRUE

cb_route.Enabled = True

// If the document is not already indexed
If Not ib_already_indexed Then
	// Set up the index only/route message
	// If dw_options.GetItemString(1,"index_only") = "Y" Then
	IF rb_index_only.checked = TRUE THEN
		wf_indexing_or_routing(True)  // Index Only
	END IF
	
	IF rb_full_route.checked = TRUE THEN
		wf_indexing_or_routing(False)	// Route
	END IF

	// If they want to keep the claim number then reset and re-retrieve the information
	If dw_options.GetItemString(1,"keep_claim") = "Y" Then
		uo_document_indexing.dw_document_indexing.SetItem(1,"claim_no",ll_claim_no)
		wf_set_claim(ll_claim_no)
	End If

	// If they want to keep the type then reset the value
	If dw_options.GetItemString(1,"keep_type") = "Y" Then
		
		ls_doc_type_desc_e = ""
		ls_doc_subtype_desc_e = ""
		
		//display the document type desc
		Select  doc_type_desc_e
		Into     :ls_doc_type_desc_e
		From   Doc_Type	
		Where doc_type_code = :ls_default_type_code
		Using SQLCA;
		
		SQLCA.nf_handle_error("u_document_index", "dw_document_indexing", "SELECT  FROM Doc_Type_Subtype_Xref") 
		
		//display the document sub type desc
		Select  doc_subtype_desc_e
		Into     :ls_doc_subtype_desc_e
		From   Doc_Subtype	
		Where doc_subtype_code = :ls_default_sub_type_code
		Using SQLCA;
		
		SQLCA.nf_handle_error("u_document_index", "dw_document_indexing", "SELECT  FROM Doc_Type_Subtype_Xref") 
		
		uo_document_indexing.dw_document_indexing.SetItem(1,"doc_class_code",ls_doc_class_code)
		uo_document_indexing.dw_document_indexing.SetItem(1,"doc_subtype_code",ls_doc_subtype_code)
		uo_document_indexing.dw_document_indexing.SetItem(1,"type_code",ls_type)
		uo_document_indexing.dw_document_indexing.SetItem(1,"document_desc",Trim(ls_doc_type_desc_e)+" "+Trim(ls_doc_subtype_desc_e))
		uo_document_indexing.dw_document_indexing.SetItem(1,"paid_flag",ls_paid)
		If ls_paid = "Y" Then
			wf_indexing_or_routing(True)  // Index Only
			rb_index_only.checked = TRUE
		End If
	End If

	If dw_options.GetItemString(1,"keep_date") = "Y" Then
		uo_document_indexing.dw_document_indexing.SetItem(1,"date_received",ldt_date)
	End If

	// If they want to keep the sender then reset the value
	If dw_options.GetItemString(1,"keep_sender") = "Y" Then
		uo_document_indexing.dw_document_indexing.SetItem(1,"service_provider_no",ll_sender)
		uo_document_indexing.dw_document_indexing.SetItem(1,"service_provider_name",ls_sender)
		uo_document_indexing.dw_document_indexing.SetItem(1,"service_provider_type_code",ls_sender_type)

		If ll_sender = 0 Then
			IF Len(ls_comment) > 20 THEN	ls_comment= Trim(left(ls_comment,20))
			uo_document_indexing.dw_document_indexing.SetItem(1,"comment",ls_comment)
		Else
			IF Len(ls_sender) > 20 THEN	ls_comment= Trim(left(ls_sender,20))
			uo_document_indexing.dw_document_indexing.SetItem(1,"comment",ls_sender)
		End If
	End If

	// Put the cursor in the first empty field
	uo_document_indexing.dw_document_indexing.SetColumn("claim_no")
	If dw_options.GetItemString(1,"keep_claim") = "Y" Then
		uo_document_indexing.dw_document_indexing.SetColumn("doc_class_code")
		If dw_options.GetItemString(1,"keep_type") = "Y" Then
			uo_document_indexing.dw_document_indexing.SetColumn("date_on_document")
			If dw_options.GetItemString(1,"keep_date") = "Y" Then		
				uo_document_indexing.dw_document_indexing.SetColumn("comment")
			End If
		End If		
	End If
End If

// Set the docid, fldid, and imaged_document_flag on the indexing form
uo_document_indexing.dw_document_indexing.SetItem(1,"docid",ll_docid)
uo_document_indexing.dw_document_indexing.SetItem(1,"fldid",ll_fldid)
uo_document_indexing.dw_document_indexing.SetItem(1,"imaged_document_flag","Y")

// Only allow claim number to be modified when Document_Type_Code.auto_created_flag = "Y"	
SELECT DTC.auto_created_flag 
  INTO :ls_auto_created_flag 
  FROM DOCUMENT_INDEX DI, 
       Document_Type_Code DTC 
 WHERE DI.docid = :ll_docid 
   AND DI.type_code = DTC.type_code 
 USING ImageTrans ; 

ImageTrans.nf_handle_error("w_document_indexing","dw_document_indexing","rowfocuschanged - SELECT DTC.auto_created_flag FROM DOCUMENT_INDEX DI, Document_Type_Code DTC")

IF ls_auto_created_flag = "Y" THEN
	uo_document_indexing.dw_document_indexing.Modify("doc_class_code.protect=1                  doc_class_code.Background.Color='553648127'")
	uo_document_indexing.dw_document_indexing.Modify("type_code.protect=1                  type_code.Background.Color='553648127'")
	uo_document_indexing.dw_document_indexing.Modify("doc_subtype_code.protect=1            doc_subtype_code.Background.Color='553648127'")
	uo_document_indexing.dw_document_indexing.Modify("date_on_document.protect=1           date_on_document.Background.Color='553648127'")
	uo_document_indexing.dw_document_indexing.Modify("date_received.protect=1              date_received.Background.Color='553648127'")
	uo_document_indexing.dw_document_indexing.Modify("comment.protect=1                    comment.Background.Color='553648127'")
	uo_document_indexing.dw_document_indexing.Modify("service_provider_type_code.protect=1 service_provider_type_code.Background.Color='553648127'")
	uo_document_indexing.dw_document_indexing.Modify("service_provider_no.protect=1        service_provider_no.Background.Color='553648127'")
	uo_document_indexing.dw_document_indexing.Modify("reference_no.protect=1               reference_no.Background.Color='553648127'")
	uo_document_indexing.dw_document_indexing.Modify("source_code.protect=1                source_code.Background.Color='553648127'")
	uo_document_indexing.dw_document_indexing.Modify("paid_flag.protect=1                  paid_flag.Background.Color='553648127'")
	uo_document_indexing.dw_document_indexing.Modify("sent_flag.protect=1                  sent_flag.Background.Color='553648127'")

	cb_move.Enabled = FALSE
	rb_full_route.Enabled = FALSE
	rb_full_route.Checked = FALSE
	rb_index_only.Checked = TRUE
	uo_document_indexing.cb_provider_search.Enabled = FALSE
ELSE
	IF lb_reference_no_protect = FALSE THEN
		uo_document_indexing.dw_document_indexing.Modify("reference_no.protect=0            reference_no.Background.Color='16777215'")
	END IF

	// Medical Aid document
	IF ls_module_source_code = '12' Then
		uo_document_indexing.dw_document_indexing.Modify("date_on_document.protect=1           date_on_document.Background.Color='553648127'")
		uo_document_indexing.dw_document_indexing.Modify("date_received.protect=1              date_received.Background.Color='553648127'")
		uo_document_indexing.dw_document_indexing.Modify("comment.protect=1                    comment.Background.Color='553648127'")
		uo_document_indexing.dw_document_indexing.Modify("service_provider_type_code.protect=1 service_provider_type_code.Background.Color='553648127'")
		uo_document_indexing.dw_document_indexing.Modify("service_provider_no.protect=1        service_provider_no.Background.Color='553648127'")
		uo_document_indexing.dw_document_indexing.Modify("reference_no.protect=1               reference_no.Background.Color='553648127'")
		uo_document_indexing.dw_document_indexing.Modify("source_code.protect=1                source_code.Background.Color='553648127'")
		uo_document_indexing.dw_document_indexing.Modify("paid_flag.protect=1                  paid_flag.Background.Color='553648127'")
		uo_document_indexing.dw_document_indexing.Modify("sent_flag.protect=1                  sent_flag.Background.Color='553648127'")
	
		cb_move.Enabled = FALSE
		rb_full_route.Enabled = FALSE
		rb_index_only.Enabled = FALSE
		cb_remlink.Enabled = False
		uo_document_indexing.cb_provider_search.Enabled = FALSE
		
	ELSEIF ls_module_source_code = '13' OR ls_module_source_code = '15' THEN
		uo_document_indexing.dw_document_indexing.Modify("doc_class_code.protect=1           doc_class_code.Background.Color='553648127'")
		uo_document_indexing.dw_document_indexing.Modify("type_code.protect=1                  type_code.Background.Color='553648127'")
     	uo_document_indexing.dw_document_indexing.Modify("doc_subtype_code.protect=1     doc_subtype_code.Background.Color='553648127'")
		uo_document_indexing.dw_document_indexing.Modify("date_on_document.protect=1           date_on_document.Background.Color='553648127'")
		uo_document_indexing.dw_document_indexing.Modify("date_received.protect=1              date_received.Background.Color='553648127'")
		uo_document_indexing.dw_document_indexing.Modify("service_provider_type_code.protect=1 service_provider_type_code.Background.Color='553648127'")
		uo_document_indexing.dw_document_indexing.Modify("service_provider_no.protect=1        service_provider_no.Background.Color='553648127'")
		uo_document_indexing.dw_document_indexing.Modify("source_code.protect=1                source_code.Background.Color='553648127'")
		uo_document_indexing.dw_document_indexing.Modify("paid_flag.protect=1                  paid_flag.Background.Color='553648127'")
		uo_document_indexing.dw_document_indexing.Modify("sent_flag.protect=1                  sent_flag.Background.Color='553648127'")
		uo_document_indexing.dw_document_indexing.Modify("reference_no.protect=1               reference_no.Background.Color='553648127'")

		IF ls_module_source_code = '15' THEN
			uo_document_indexing.dw_document_indexing.Modify("referral_flag.protect=1               referral_flag.Background.Color='553648127'")
		END IF 



		rb_full_route.Enabled 									= FALSE
		rb_index_only.Enabled 									= FALSE
		uo_document_indexing.cb_provider_search.Enabled = FALSE
		cb_remlink.Enabled 										= FALSE
				
	Else
		uo_document_indexing.dw_document_indexing.Modify("doc_class_code.protect=0          doc_class_code.Background.Color='16777215'")
		uo_document_indexing.dw_document_indexing.Modify("type_code.protect=0                  type_code.Background.Color='16777215'")
     	uo_document_indexing.dw_document_indexing.Modify("doc_subtype_code.protect=0     doc_subtype_code.Background.Color='16777215'")
		uo_document_indexing.dw_document_indexing.Modify("date_on_document.protect=0           date_on_document.Background.Color='16777215'")
		uo_document_indexing.dw_document_indexing.Modify("date_received.protect=0              date_received.Background.Color='16777215'")
		uo_document_indexing.dw_document_indexing.Modify("comment.protect=0                    comment.Background.Color='16777215'")
		uo_document_indexing.dw_document_indexing.Modify("service_provider_type_code.protect=0 service_provider_type_code.Background.Color='16777215'")
		uo_document_indexing.dw_document_indexing.Modify("service_provider_no.protect=0        service_provider_no.Background.Color='16777215'")
		uo_document_indexing.dw_document_indexing.Modify("reference_no.protect=0               reference_no.Background.Color='16777215'")
		uo_document_indexing.dw_document_indexing.Modify("source_code.protect=0                source_code.Background.Color='16777215'")
		uo_document_indexing.dw_document_indexing.Modify("paid_flag.protect=0                  paid_flag.Background.Color='16777215'")
		uo_document_indexing.dw_document_indexing.Modify("sent_flag.protect=0                  sent_flag.Background.Color='16777215'")
	
		cb_move.Enabled = TRUE
		rb_full_route.Enabled = TRUE
		uo_document_indexing.cb_provider_search.Enabled = TRUE
	End if
END IF


ll_result = f_close_viewer()

// View the document
if uo_image_append.of_init(ll_docid)	<= 0 then
	If MessageBox("Document Indexing","There are no pages in this document.~r~nWould you like to delete the document?",Question!, YesNo!,1) = 1 Then
		ib_del_confirm_reqd = FALSE
		cb_delete_document.TriggerEvent(Clicked!)
	End If
end if
	
ls_doc_type =  uo_image_append.of_get_file_type()
CHOOSE CASE ls_doc_type
	CASE 'IMA', 'TIF'  // Imaged document
		uo_image_append.of_set_option()
		li_rtn = uo_image_append.of_append_image(ll_docid)
		if li_rtn < 0 then
			RETURN
		end if
	case else
		ll_result = iw_sheet.iu_dw_document_path.f_manage_document(ll_docid,"V","INDEX")
		If ll_result > 0 Then
		ElseIf ll_result = 0 Then
			If MessageBox("Document Indexing","There are no pages in this document.~r~nWould you like to delete the document?",Question!, YesNo!,1) = 1 Then
				ib_del_confirm_reqd = FALSE
				cb_delete_document.TriggerEvent(Clicked!)
			End If
		Else
			MessageBox("Document Indexing","Unable to open document. Please try again.")
			Return
		End If	
end choose
	
uo_document_indexing.dw_document_indexing.SetFocus()

uo_document_indexing.dw_document_indexing.Modify("lost_time.Visible=0")
uo_document_indexing.dw_document_indexing.Modify("lost_time.Protect=1")
uo_document_indexing.dw_document_indexing.Modify("lost_time_t.Visible=0")

// Default values if Module Source is Medical Aid or Form 67
If Not ib_already_indexed Then
	IF ls_module_source_code = '12' THEN
		lds_med_aid_e_invoice_search_defaults = CREATE u_ds
		lds_med_aid_e_invoice_search_defaults.dataobject = 'd_med_aid_e_invoice_search_defaults'
		lds_med_aid_e_invoice_search_defaults.SetTransObject(SQLCA)
		
		ll_rows = lds_med_aid_e_invoice_search_defaults.Retrieve(ll_docid)
		SQLCA.nf_handle_error('w_document_indexing','cb_search.clicked','ll_rows = lds_med_aid_e_invoice_search_defaults.Retrieve(' + String(ll_docid) + ')')
		IF ll_rows <> 1 Then SignalError(-666,'Error finding Med Aid Invoice.')
		
		//Date fields must be set  to NULL or they will get 1900-01-01
		SetNull(lstr_claim_search_defaults.birth_date )
		IF lds_med_aid_e_invoice_search_defaults.GetItemNumber(1,'medicare_no') <> 0 Then
			lstr_claim_search_defaults.medicare_no = lds_med_aid_e_invoice_search_defaults.GetItemNumber(1,'medicare_no')
		Else
			lstr_claim_search_defaults.last_name 	= lds_med_aid_e_invoice_search_defaults.GetItemString(1,'patient_last_name')
			lstr_claim_search_defaults.given_names	= lds_med_aid_e_invoice_search_defaults.GetItemString(1,'patient_given_names')
		End if
		
		lstr_message.awi_parent_window = PARENT
		lstr_message.as_stringparm[1] = 'd_basic_claim_search'
		lstr_message.apo_PowerObjectParm[1] = lstr_claim_search_defaults
		
		OpenWithParm(w_indexing_search, lstr_message)
		
		uo_document_indexing.dw_document_indexing.SetFocus()
		uo_document_indexing.dw_document_indexing.SetColumn("claim_no")

	// Form67 Document - Open Search screen with defaults
	ELSEIF ls_module_source_code = '13' OR ls_module_source_code = '15' THEN//form67 or webform67
		
		
		IF ls_module_source_code = '13' THEN 
			lds_form67_search_defaults 				= CREATE u_ds
			lds_form67_search_defaults.dataobject 	= 'd_form67_search_defaults'
			lds_form67_search_defaults.SetTransObject(SQLCA)		
		ELSE
			lds_form67_search_defaults 				= CREATE u_ds
			lds_form67_search_defaults.dataobject 	= 'd_web_form67_search_defaults'
			lds_form67_search_defaults.SetTransObject(SQLCA)	
		END IF 
		
		ll_rows = lds_form67_search_defaults.Retrieve(ll_docid)
		SQLCA.nf_handle_error('w_document_indexing','cb_search.clicked','ll_rows = lds_form67_search_defaults.Retrieve(' + String(ll_docid) + ')')
		IF ll_rows <> 1 Then SignalError(-666,'Error finding Form67 Document.')
	
		// Date fields must be set  to NULL or they will get 1900-01-01
		SetNull(lstr_claim_search_defaults.birth_date )

		ll_worker_sin_no = lds_form67_search_defaults.GetItemNumber(1,'worker_sin_no')
		IF ll_worker_sin_no > 0 THEN
			lstr_claim_search_defaults.sin_no = ll_worker_sin_no
		ELSE
			lstr_claim_search_defaults.given_names = Left(lds_form67_search_defaults.GetItemString(1,'worker_first_name'), 2)
			lstr_claim_search_defaults.last_name = lds_form67_search_defaults.GetItemString(1,'worker_last_name')
		END IF
	
		lstr_message.awi_parent_window = PARENT
		lstr_message.as_stringparm[1] = 'd_basic_claim_search'
		lstr_message.apo_PowerObjectParm[1] = lstr_claim_search_defaults
	
		OpenWithParm(w_indexing_search, lstr_message)
	
		uo_document_indexing.dw_document_indexing.SetFocus()
		uo_document_indexing.dw_document_indexing.SetColumn("claim_no")
	
	END IF

ELSE	

		ls_class_code = uo_document_indexing.dw_document_indexing.getitemstring(1,"doc_class_code")

		uo_document_indexing.dw_document_indexing.GetChild("type_code",uo_document_indexing.idwc_document_type)
	
	    ll_rows = uo_document_indexing.idwc_document_type.Retrieve()
	    ImageTrans.nf_handle_error('w_document_indexing','rowfocuschanged','idwc_document_type.Retrieve()')
		
		uo_document_indexing.idwc_document_type.SetFilter("class_code = '" + ls_class_code + "'" )
		uo_document_indexing.idwc_document_type.Filter()
		uo_document_indexing.idwc_document_type.SetSort("#1 A")
		uo_document_indexing.idwc_document_type.Sort()
		
		// Protect the sub type and skip it if the class and type doesn't have a sub type.
		 
       ls_doc_type_code =  uo_document_indexing.dw_document_indexing.GetItemString(1,"type_code")
		
		Select  count(doc_subtype_code)
		Into     :ll_subtype
		From   Doc_Type_Subtype_Xref
		Where doc_type_code = :ls_doc_type_code
		And     active_flag = 'Y'
		Using SQLCA;
		
		SQLCA.nf_handle_error("u_document_index", "dw_document_indexing", "SELECT  FROM Doc_Type_Subtype_Xref") 
		 

		IF (IsNull(uo_document_indexing.dw_document_indexing.getitemstring(1,"doc_subtype_code")) or &
			uo_document_indexing.dw_document_indexing.getitemstring(1,"doc_subtype_code") = '') and  ll_subtype < 2 THEN
			uo_document_indexing.dw_document_indexing.SetItem(1,"doc_subtype_code",'')
			uo_document_indexing.dw_document_indexing.Modify("doc_subtype_code.protect=1            doc_subtype_code.Background.Color='553648127'")
		ELSE
			uo_document_indexing.dw_document_indexing.Modify("doc_subtype_code.protect=0     doc_subtype_code.Background.Color='16777215'")
			ls_type_code = uo_document_indexing.dw_document_indexing.getitemstring(1,"type_code")
			uo_document_indexing.dw_document_indexing.GetChild("doc_subtype_code",uo_document_indexing.idwc_subtype_code)
		    ll_rows = uo_document_indexing.idwc_subtype_code.Retrieve()
	   		ImageTrans.nf_handle_error('w_document_indexing','rowfocuschanged','idwc_subtype_code.Retrieve()')
			
			uo_document_indexing.idwc_subtype_code.SetFilter("doc_type_code = '" + ls_type_code + "'" )
			uo_document_indexing.idwc_subtype_code.Filter()
			uo_document_indexing.idwc_subtype_code.SetSort("#1 A")
			uo_document_indexing.idwc_subtype_code.Sort()
			
	     END IF

	
END IF

end event

type cb_delete_document from commandbutton within w_document_indexing
integer x = 187
integer y = 684
integer width = 311
integer height = 92
integer taborder = 70
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Del &Doc"
end type

event clicked;
LONG		ll_count, ll_rownum, ll_docid, ll_fldid, ll_next_fldid, ll_result
STRING	ls_create, ls_drop, ls_begin, ls_commit,ls_document_path, ls_curr_mode

/* Disable button to prevent second click.
*/
	This.Enabled = False
	
/* Confirm that deletion is desired.
*/
	IF dw_document_list.RowCount() > 0 THEN
		IF ib_del_confirm_reqd THEN
			IF MessageBox("Document Indexing","Are you sure you want to delete this document?",Question!,YesNo!) = 2 THEN
				THIS.Enabled = TRUE
				RETURN
			END IF
		END IF
	ELSE
		MessageBox("Document Indexing","There are no documents to delete in this folder.")
		RETURN
	END IF

/* Make sure you can figure out what document they want to delete.
*/
	ll_rownum = dw_document_list.GetSelectedRow(0)
	IF ll_rownum = 0 THEN
		MessageBox("Delete Document","Could not determine selected document to delete. Please try again.")
		THIS.Enabled = TRUE
		RETURN
	END IF

/* Can only delete the document if it is still in the Scanning Set.
*/
	IF dw_document_list.GetItemNumber(ll_rownum,"ref_docsetid") <> 6 THEN
		MessageBox("Document Indexing","Only documents in the Scanning set can be deleted.")
		RETURN
	END IF
	
/* Get the document's id and the folder's id.
*/
	ll_docid = dw_document_list.GetItemNumber(ll_rownum,"ref_docid")
	ll_fldid = dw_document_list.GetItemNumber(ll_rownum,"ref_docfldid")
	
/* Ask the users if they want to print off a copy of the deleted document before completing 
   the deletion
*/
IF ib_del_confirm_reqd Then
	
	IF MessageBox("PRINT DOCUMENT","Would you like to print this document before you complete the delete?",Question!,YesNo!) = 1 THEN		
		
		SELECT strpath + "\" + pgffilename 
		  INTO :ls_document_path
		  FROM PGF,PAG,DOC,STORE 
		 WHERE DOC.docid    = :ll_docid
			AND PAG.pagdocid = DOC.docid
			AND PGF.pgfid    = pagfid
			AND STORE.strid  = PGF.pgfstore
		  USING ImageTrans;
		  
		IF ImageTrans.nf_handle_error('w_document_indexing','cb_delete_document - clicked','SELECT strpath + "\" + pgffilename...') <> 0 THEN
			RETURN -1
		END IF
		
		ll_result = iw_sheet.iu_dw_document_path.f_manage_document(ll_docid,"P","INDEX")
		
		If ll_result > 0 Then
		ElseIf ll_result = 0 Then
			If MessageBox("Document Indexing","There are no pages in this document.~r~nWould you like to delete the document?",Question!, YesNo!,1) = 1 Then
				ib_del_confirm_reqd = FALSE
			ELSE
				return
			End If
		Else
			MessageBox("Document Printing","Document: " + ls_document_path + " could not be printed.  Error No: " + String(ll_result) + ".~r~n Processing continues.")
			Return
		End If	
		
	END IF
End if

IF dw_document_list.RowCount() = 1 THEN	
	is_curr_mode = 'Del Last'
END IF

/*	Call function to do the actual deletion of the document.
*/
	IF wf_delete_document(ll_docid,ll_fldid,ll_rownum) = - 1 THEN
		RETURN
	END IF

/* Enable the button.
*/
	THIS.Enabled = TRUE

	ib_del_confirm_reqd = TRUE

/* Refresh the document list.
*/
	cb_refresh.TriggerEvent(Clicked!)

end event

type dw_select_category from u_dw_online within w_document_indexing
integer x = 306
integer y = 92
integer width = 837
integer height = 92
integer taborder = 20
string dataobject = "d_select_category_limited"
boolean border = false
end type

event itemchanged;call super::itemchanged;// Get the documents in the current category
	Long ll_result, ll_catid

	is_curr_mode = "Retrieve"
	
	

// Activate the clear button on the sheet
	iw_sheet.wf_clear_worksheet()
	iw_sheet.wf_initialize_basic_claim()

	ll_catid = Long(GetText())

	If Not ll_catid > 1 Then
		MessageBox("Document Indexing","Could not determine selected category. Please try again.")
		Return
	End If

	ll_result = dw_document_list.Retrieve(ll_catid)
	If ImageTrans.nf_handle_error("dw_document_list","w_document_indexing","itemchanged for dw_select_category") < 0 then
		Return
	ElseIf ll_result <= 0 Then
		MessageBox("Category","There are no more documents to be indexed in this folder. Please select another folder to continue indexing.",information!)
		cb_delete_document.Enabled = False
		cb_move.Enabled            = False
		cb_remlink.Enabled         = False
		cb_recall.Enabled          = False
		cb_alt1.Enabled            = FALSE
		
/* PR #970  - Next line added by Ed Lenarczyk  May 10, 2000
	Make sure everything is cleared when changing folders
*/
		cb_cancel.PostEvent(Clicked!)
		Return
	End If

	cb_alt1.Enabled = TRUE

	ImageTrans.nf_begin_transaction()
	
// Delete any empty folders
	DELETE FLD
	WHERE FLD.fldcatid = :ll_catid
	AND not exists (SELECT * FROM REF 
	WHERE FLD.fldid = REF.docfldid)
	USING ImageTrans;

	If ImageTrans.nf_handle_error("Embedded SQL: Delete fld","w_document_indexing","itemchanged for dw_select_category") < 0 then
		Return
	End If

	ImageTrans.nf_commit_transaction()
	
	
// Enable and Disable the appropriate buttons
	wf_set_update_mode(False)

	is_curr_mode = "Continue"

// Choose the first document in the list
	dw_document_list.SetRow(1)
	dw_document_list.PostEvent(RowFocusChanged!)

// Force user to reset routing options with change of category (reset to 'blank' here, error on 'route' if 'blank')
	IF ib_reset_rte_opts_reqd THEN
		rb_full_route.Checked = FALSE
   	rb_index_only.Checked = FALSE
		rb_full_route.Enabled = TRUE
   	rb_index_only.Enabled = TRUE
		uo_document_indexing.dw_document_indexing.SetItem(1,"message_info","")
	END IF
	ib_reset_rte_opts_reqd = TRUE
end event

event itemerror;return 2
end event

type cb_route from commandbutton within w_document_indexing
integer x = 1061
integer y = 1712
integer width = 334
integer height = 92
integer taborder = 130
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Continue"
end type

event clicked;/*	When clicked, one out of the three possible scenerios will be selected for processing the document.

	Scenario 1: A new document is being indexed only. The end result here, on the IMARA_DB side, will be 1 
					record in REF pointing to master info. The DOC record being updated with new info and a
					DOCUMENT_INDEX record being created.


	Scenario 2: A new document is being both routed and indexed. The end result here, on the IMARA_DB side
					is as follows:
					2 records in REF, 1 being the Master REF record and the other being the routed REF record.
					1 CLAIM_WORKING record is created. The FLD record is updated. The DOC record is updated.
					A DOCUMENT_INDEX record is created.
					If it is determined that the document may already exist, then the document could be switched,
					which would then cause the original document to be archived.

	Scenario 3:	A currently indexed & routed document is being corrected (via Corrections bucket). Will do 1
					of 2 things depending on whether the claim number has changed.
					No claim number change -> Only the DOCUMENT_INDEX record will be updated.
					A claim number change -> Master REF record will need to change to show the new master folder
					id for the new claim number. The REF record pointing to the Corrections bucket needs to be
					updated to point to the new routed destination based on the normal routing rules. Also the
					switching document code needs to be done if any of the three columns are being updated.
*/

	LONG     ll_claim_no, ll_sender, ll_counter, ll_docid, ll_rownum, ll_fldid, ll_destination_catid, ll_originalclaimno
	LONG		ll_setid, ll_result, ll_first_docid,ll_last_docid, ll_master_fldid, ll_event_no, ll_dupdoccount, ll_paiddoccount
	LONG		ll_rowsdeleted, ll_catid
	STRING	ls_type, ls_status, ls_status_type, ls_service_provider_name, ls_region, ls_event_specific, ls_doc_class_code
	STRING	ls_print_dt_received, ls_source_code, ls_comment, ls_docname, ls_originaltypecode,ls_doc_type, ls_create_user_id
	DATETIME	ldt_date, ldt_received, ldt_originaldocdate, ldt_create_date, ldtm_referred_on_date
	BOOLEAN	lb_update_claim, lb_update_referral
	integer li_rtn, li_trancount
	LONG		ll_reference_no,ll_count
	STRING		ls_module_source_code, ls_indexer_name , ls_message, ls_referring_provider_type_code, ls_referral_flag, ls_sub_type
	LONG			ll_claimant_medicare_no, ll_ref_count, ll_referral_no
	LONG			ll_invoice_medicare_no, ll_rtn
	
	N_PROCESS_RUN_STATUS ln_process_run_status
	
	/******************************************************************************************
	P10275 - Daytime Payment Processing
	- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
	- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
	- '019' refers to the Document Indexing module, '044' refers to the Payment Processing module
	******************************************************************************************/
	ln_process_run_status = Create N_PROCESS_RUN_STATUS
	
	li_rtn = ln_process_run_status.nf_in_progress('019','044','document routing',SQLCA)
	
	IF li_rtn = 1 THEN
		// module is blocked
		return
	END IF
	/******************************************************************************************/
	
	lb_update_claim = FALSE

	SetPointer(HourGlass!)
	li_rtn = uo_document_indexing.dw_document_indexing.accepttext()
	IF li_rtn = -1 THEN
		RETURN
	END IF
	
	/* PR 2966 - kdm */
	IF uo_document_indexing.ib_paid_document = TRUE THEN
		/* the user has seen the message from u_document_index's 
		    dw_document_indexing's itemchanged event and has not agreed to
			 the re-index. So, don't show another message */
		RETURN
	END IF
 
 	/* PR 3240 - kdm */
	IF uo_document_indexing.ib_correspondence = TRUE THEN
		/* the user has seen the message from u_document_index's 
		    dw_document_indexing's itemchanged event and has not agreed to
			 the re-index. So, don't show another message */
		RETURN
	END IF

/*	Make sure all the required fields are filled in.
*/
	IF uo_document_indexing.uf_check_required(ls_type,ldt_date) = -1 THEN
		RETURN
	END IF
	
		

/*	Check that received date is filled in where source = 'received'.
*/
	ldt_received = uo_document_indexing.dw_document_indexing.GetItemDateTime(1,"date_received") 
	ls_source_code = uo_document_indexing.dw_document_indexing.GetItemString(1,"source_code") 
	IF (IsNull(ldt_received) OR String(ldt_received) = "0000 01 01 00:00:00") AND ls_source_code = "R" THEN
		MessageBox("Document Indexing","Received Date must be filled in when Source is 'Received'.")
		uo_document_indexing.dw_document_indexing.SetFocus()
		uo_document_indexing.dw_document_indexing.SetColumn("date_received")
      RETURN
	END IF

	ll_docid = uo_document_indexing.dw_document_indexing.GetItemNumber(1,'docid')

	// PR 6130 - Document already indexed?  Don't check Corrections folder catid = 864
	ll_rownum = dw_document_list.GetRow()
	IF ll_rownum > 0 THEN
		ll_catid = dw_document_list.GetItemNumber(ll_rownum, "ref_doccatid")
		IF ll_catid <> 864 THEN
			SELECT create_user_id, create_date 
			  INTO :ls_create_user_id, :ldt_create_date 
			  FROM DOCUMENT_INDEX  
			 WHERE docid = :ll_docid 
			 USING imagetrans ; 

			li_rtn = imagetrans.nf_handle_error('w_document_indexing', '', 'cb_route - SELECT create_user_id, create_date FROM DOCUMENT_INDEX...')
			
			IF li_rtn = 0 THEN
				IF vgst_user_profile.user_name = "" THEN
					ls_indexer_name = ls_create_user_id 
				ELSE
					ls_indexer_name = vgst_user_profile.user_name
				END IF

				MessageBox("Document already Indexed", "This document was already indexed by " + ls_indexer_name + " on " +&
							  + DayName(Date(ldt_create_date)) + ", " + String(ldt_create_date, "mmm dd, yyyy") + " at " + String(ldt_create_date, "hh:mm:ss am/pm"))
				RETURN
			END IF
		END IF
	END IF

	//******************************************************************
	// P10151-47 NB Chiro Agreement, fixes for NBMS Agreement

	li_rtn = uo_document_indexing.uf_validate_NBMS_EFB(ll_docid)
	IF li_rtn < 0 THEN RETURN // do not allow change
	
	li_rtn = uo_document_indexing.uf_validate_Chiro_EFB(ll_docid)
	IF li_rtn < 0 THEN RETURN // do not allow change

	// end Chiro changes
	//*********************************************************************


/*	Make sure that the routing or indexing option has been specified.
*/
	IF rb_full_route.Checked = FALSE AND rb_index_only.Checked = FALSE THEN
		MessageBox("Document Indexing","Either the 'Full Routing' or 'Index Only' option must be specified.")
		RETURN
	END IF

/*	Get the current document.
*/
	ll_rownum = dw_document_list.GetRow()
	IF ll_rownum <= 0 THEN
		MessageBox("Document Indexing","Unable to determine the current document.")
		RETURN
	END IF

	wf_set_update_mode(TRUE)

/*	Make sure the tombstone contains a valid claim number.
*/
	IF iw_sheet.dw_basic_claim.RowCount() <> 1 THEN
		MessageBox("Document Indexing","Claim number must be filled in and validated.")
		RETURN
	END IF

/*	Initialize the variables.
*/
	ll_claim_no    = iw_sheet.dw_basic_claim.GetItemNumber(1,"claim_no") 
	ls_status      = iw_sheet.dw_basic_claim.GetItemString(1,"claim_status_code")
	ls_status_type = iw_sheet.dw_basic_claim.GetItemString(1,"claim_status_type_code")
	ls_region      = iw_sheet.dw_basic_claim.GetItemString(1,"admin_region_code")
	ll_sender  				  	 = uo_document_indexing.dw_document_indexing.GetItemNumber(1,"service_provider_no") 
	ls_service_provider_name = uo_document_indexing.dw_document_indexing.GetItemString(1,"service_provider_name") 
	il_master_fldid			 = uo_document_indexing.il_master_fldid
	ls_originaltypecode = uo_document_indexing.dw_document_indexing.GetItemString(1,"type_code",Primary!,TRUE)
	ldt_originaldocdate = uo_document_indexing.dw_document_indexing.GetItemDatetime(1,"date_on_document",Primary!,TRUE)
	ll_originalclaimno = uo_document_indexing.dw_document_indexing.GetItemNumber(1,"claim_no",Primary!,TRUE)
    ls_module_source_code = dw_document_list.GetITemString(dw_document_list.GetRow(),'module_source_code')
    ls_referring_provider_type_code = uo_document_indexing.dw_document_indexing.GetItemString(1,"service_provider_type_code") 
    ll_docid                         = uo_document_indexing.dw_document_indexing.GetItemNumber(1,"docid")
	ldtm_referred_on_date   = uo_document_indexing.dw_document_indexing.GetItemDatetime(1,"date_on_document")
	ls_referral_flag               = uo_document_indexing.dw_document_indexing.GetItemString(1,"referral_flag")
	ls_doc_class_code          = uo_document_indexing.dw_document_indexing.GetItemString(1,"doc_class_code") 
	ls_sub_type                  = uo_document_indexing.dw_document_indexing.GetItemString(1,"doc_subtype_code")
	

//What does module source code of 12 mean???
IF ls_module_source_code = "12" THEN
	
	SELECT medicare_no
	INTO   :ll_claimant_medicare_no
	FROM CLAIM a,
	     INDIVIDUAL b
	WHERE a.individual_no = b.individual_no
	AND   a.claim_no = :ll_claim_no;
	
	SQLCA.nf_handle_error('w_document_indexing','cb_route.clicked','SELECT medicare_no')


	ll_docid = uo_document_indexing.dw_document_indexing.GetItemNumber(1,"docid")
	
	SELECT medicare_no
	INTO   :ll_invoice_medicare_no
	FROM   MED_AID_E_INVOICE
	WHERE  doc_id = :ll_docid;

	SQLCA.nf_handle_error('w_document_indexing','cb_route.clicked','SELECT medicare_no')

	IF ll_claimant_medicare_no <> 0 and ll_invoice_medicare_no <> 0 Then
		IF ll_claimant_medicare_no <> ll_invoice_medicare_no Then
			li_rtn = MessageBox('Different Medicare',"The Medicare number on the Medical Aid Invoice is different than the Claimants.  Do you want to continue anyway?",Information!,YesNo!,2)
			IF li_rtn = 2 Then
				RETURN
			END IF
		END IF
	END IF
END IF
			

/*	Check to see if there is already a document out	there with the same claim_no, type_code, and date_on_document.
	If so then there may need to be a switch.
*/
IF NOT ib_already_indexed AND Trim(ls_type) <> "AW" THEN
		ll_dupdoccount = dw_duplicate_documents.Retrieve(ll_claim_no,ls_type,ldt_date)

		IF ImageTrans.nf_handle_error("dw_duplicate_documents","w_document_indexing","clicked for cb_route") < 0 THEN
			Close(Parent)
			RETURN
		END IF
		IF ll_dupdoccount > 0 THEN
			MessageBox("Document Indexing","Possible duplicate document(s) found.~r~nMaster document(s) will now be opened.")
			ll_counter = 1
			ll_last_docid = 0

/*	View the duplicate documents.
*/
			DO WHILE ll_counter <= ll_dupdoccount
				ll_docid = dw_duplicate_documents.GetItemNumber(ll_counter,"docid")
				IF ll_last_docid <> ll_docid THEN
					
				
					if uo_image_append.of_init(ll_docid)	<= 0 then
						RETURN
					end if
						
						
					ls_doc_type =  uo_image_append.of_get_file_type()
						
					
					CHOOSE CASE ls_doc_type
						/*  Imaged document */ 
						CASE 'IMA', 'TIF'
							uo_image_append.of_set_option()
							li_rtn = uo_image_append.of_append_image(ll_docid)
							if li_rtn < 0 then
								RETURN
							end if
						case else
							iw_sheet.iu_dw_document_path.f_manage_document(ll_docid,"V","NORMAL")
					end choose
			
					
				END IF
				ll_counter ++
				ll_last_docid = ll_docid
			LOOP

	      ll_result = MessageBox("Duplicate Documents","Do you want to switch the documents?",Question!,YesNoCancel!,2) 
			IF ll_result = 1 THEN
				IF ll_dupdoccount > 1 THEN
					dw_duplicate_documents.Show()
					dw_duplicate_documents.SetRow(1)
					MessageBox("Duplicate Documents","Highlight the document you wish to switch and click the Switch button.")
					cb_refresh.Enabled = TRUE
					cb_switch2.Enabled = TRUE
					RETURN
				ELSE
					dw_duplicate_documents.SetRow(1)
					cb_switch2.PostEvent(Clicked!)
				END IF
				GOTO CommonExit
			ELSE
				IF ll_result = 3 THEN
					ll_docid = uo_document_indexing.dw_document_indexing.GetItemNumber(1,"docid")
					GOTO CommonExit
				END IF
			END IF
		END IF
	END IF

	ll_docid = uo_document_indexing.dw_document_indexing.GetItemNumber(1,"docid")
	ll_fldid = uo_document_indexing.dw_document_indexing.GetItemNumber(1,"fldid")
	// Close any open viewers
	ll_result = 1
	Do While ll_result > 0
		ll_result = f_close_viewer()
	Loop
	// Open the current document in viewer
	uo_image_append.of_init(ll_docid)
	IF ll_fldid <= 0 THEN
		MessageBox("Document Indexing","Could not determine document's folder. Please select document and try again.")
		GOTO CommonExit
	END IF

/* select to see if the doc_id exists in the Docid_Reference_Xref table if it does 
	then we will populate the reference_no with the one contained there otherwise stick
	to the database default of 0
*/
	SELECT reference_no INTO :ll_reference_no FROM Docid_Reference_Xref 
	 WHERE docid = :ll_docid 
	 USING ImageTrans;
			
	IF ImageTrans.nf_handle_error("w_document_indexing","cb_route - clicked","SELECT reference_no") < 0 THEN
		Close(Parent)
		RETURN
	END IF 
		
	IF ll_reference_no > 0 THEN 
		uo_document_indexing.dw_document_indexing.SetItem(1,"reference_no",ll_reference_no)
	ELSE
		//we will get the reference_no from window - user entered.
	END IF
	
	
	
	
/*	Route the document. This means that the REF record will be changed to point to the correct
	bucket.
*/
   IF rb_full_route.checked = TRUE THEN
		
		ImageTrans.nf_begin_transaction()
		
		ll_destination_catid = wf_auto_routing(ll_docid,ll_fldid,ls_type,ls_doc_class_code,ls_status,ls_status_type,TRUE)
		IF ll_destination_catid = -1 THEN
			GOTO CommonExit
		ELSE
			dw_document_list.SetItem(ll_rownum,"ref_doccatid",ll_destination_catid)
			dw_document_list.SetItem(ll_rownum,"ref_docsetid",il_setid)
		END IF

		dw_document_list.Update()
		
		IF ImageTrans.nf_handle_error("dw_document_list.Update()","w_document_indexing","cb_route") < 0 THEN
			Close(Parent)
			RETURN
		END IF

	END IF
	
	// If the IMARA txn was not begun above, then begin one here
	ImageTrans.nf_transaction_count(li_trancount,0,'','','',FALSE)
	IF li_trancount = 0 THEN
		ImageTrans.nf_begin_transaction()
	END IF

/*	If the document is not currently indexed, then insert a REF record, placing a copy of the document into
	the master folder.
*/
	IF NOT ib_already_indexed THEN
		INSERT INTO REF (docid, docfldid, doccatid, docsetid )
		VALUES (:ll_docid,:il_master_fldid,2,2 )
		USING ImageTrans;
		IF ImageTrans.nf_handle_error("Embedded SQL: Insert into REF","w_document_indexing","cb_route") < 0 THEN
			Close(Parent)
			RETURN
		END IF
		IF ImageTrans.SQLNRows <> 1 THEN
			ImageTrans.nf_rollback_transaction()
			
			MessageBox("Document Indexing","Could not place a copy of the document in it's master folder.~r~nDocument not indexed and routed.")
			RETURN
		END IF

/* With the insertion of the new REF record, increment the document counter and update the doc name.
*/
		ls_comment = uo_document_indexing.dw_document_indexing.GetItemString(1,"comment") 
		IF IsNull(ls_comment) THEN ls_comment = ' '
		IF IsNull(ls_sub_type) THEN ls_sub_type = ' '
		ls_docname = ls_type + ' ' + ls_sub_type + STRING(ldt_date,'yyyymmdd') + ' ' + ls_comment
		UPDATE DOC
			SET docrefcount = docrefcount + 1,
			 	 docname     = :ls_docname
		 WHERE docid = :ll_docid
		 USING ImageTrans;
		IF ImageTrans.nf_handle_error("Embedded SQL: Update DOC","w_document_indexing","cb_route") < 0 THEN
			Close(Parent)
			RETURN
		END IF
		IF ImageTrans.SQLNRows <> 1 THEN
			ImageTrans.nf_rollback_transaction()
			
			MessageBox("Document Indexing","Could not place copy of document in master folder.~r~nDocument not indexed and routed.")
			RETURN
		END IF
	END IF

/*	If there has been a claim number change and the document was already indexed, then the document's master
	REF entry needs to have it's folder id point to the master folder for the new claim.
*/
	IF ib_already_indexed AND ll_claim_no <> uo_document_indexing.il_old_claim_no THEN
		UPDATE REF
			SET docfldid = :il_master_fldid
		 WHERE doccatid = 2
			AND docid = :ll_docid
		 USING ImageTrans;
		IF ImageTrans.nf_handle_error("Embedded SQL: Update REF","w_document_indexing","cb_route") < 0 THEN
			Close(Parent)
			RETURN
		END IF

/*	Because the claim number changed, remove all REF entries for the document except for the one's in the
	Corrections bucket (gets changed after re-routing) and the Master (gets changed below).
*/
		DELETE REF
		 WHERE docid = :ll_docid
			AND doccatid <> 2
			AND doccatid <> 864
		 USING ImageTrans;
		IF ImageTrans.nf_handle_error("Embedded SQL: Delete REF","w_document_indexing","cb_route") < 0 THEN
			Close(Parent)
			RETURN
		END IF

		ll_rowsdeleted = ImageTrans.SQLNRows
		ls_docname = ls_type + STRING(ldt_date,'yyyymmdd') + ' ' + ls_comment

		UPDATE DOC
			SET docrefcount = docrefcount - :ll_rowsdeleted,
			 	 docname     = :ls_docname
		 WHERE docid = :ll_docid
		 USING ImageTrans;
		IF ImageTrans.nf_handle_error("Embedded SQL: Update DOC","w_document_indexing","cb_route") < 0 THEN
			Close(Parent)
			RETURN
		END IF

		ll_destination_catid = wf_auto_routing(ll_docid,ll_fldid,ls_type, ls_doc_class_code, ls_status,ls_status_type,FALSE)
		IF ll_destination_catid = -1 THEN
			GOTO CommonExit
		ELSE
			dw_document_list.SetItem(ll_rownum,"ref_doccatid",ll_destination_catid)
			dw_document_list.SetItem(ll_rownum,"ref_docsetid",il_setid)
		END IF

		dw_document_list.Update()
		IF ImageTrans.nf_handle_error("dw_document_list.Update()","w_document_indexing","cb_route") < 0 THEN
			Close(Parent)
			RETURN
		END IF


/*	Reset the paid_flag for the document to 'N' as the claim number changed.
*/
		uo_document_indexing.dw_document_indexing.SetItem(1,"paid_flag","N")
	END IF

/*	Index the document. This is caused by updating uo_document_indexing.dw_document_indexing.
*/
	uo_document_indexing.dw_document_indexing.Update()
	IF ImageTrans.nf_handle_error("uo_document_indexing.dw_document_indexing.Update()","w_document_indexing","cb_route") < 0 THEN
		Close(Parent)
		RETURN
	END IF
	
	Select count(*)
     Into    :ll_ref_count
     From  REHAB_REFERRAL
     Where doc_id = :ll_docid  
     Using  SQLCA;

    SQLCA.nf_handle_error('w_document_indexing','cb_route','Select From REHAB_REFERRAL')
	
	// If the referral flag is selected then insert or update the REHAB_REFERRAL table.
	IF ls_referral_flag = 'Y' Then
		// first SQLCA begin txn
		SQLCA.nf_begin_transaction()
		
		// Check to see if it's an update or an insert
		IF ll_ref_count = 0 THEN
			IF inv_referral.nf_insert_referral(ll_claim_no, ll_sender,ls_referring_provider_type_code, ll_docid, ldtm_referred_on_date, 'S022', 0, ll_referral_no) < 0 THEN   // ll_referral_no passed by reference to hold a value for the newly created referral no
			   SQLCA.nf_rollback_transaction()
			   Return
			ELSE 
				lb_update_referral = TRUE
				
				//T015677 - create a direct referral task for 'SP' documents if business rules allow
				IF ls_type = 'SP' THEN
					IF inv_referral.nf_create_direct_referral(ll_claim_no, ll_sender,ls_referring_provider_type_code, ll_docid, ldtm_referred_on_date, 'S022',0, ll_referral_no) < 0 THEN
						SQLCA.nf_rollback_transaction()
			  		 	Return
					END IF
				END IF
				
			END IF
		ELSE
			IF inv_referral.nf_update_referral(ll_claim_no, ll_sender,ls_referring_provider_type_code, ll_docid, ldtm_referred_on_date, 'S022',0) < 0 THEN
				SQLCA.nf_rollback_transaction()
				Return
			ELSE 
				lb_update_referral = TRUE
			END IF
		END IF	
	ELSE
		IF ll_ref_count > 0 THEN
			// first SQLCA begin txn
			SQLCA.nf_begin_transaction()
		//this is a correction - there was a referral indicator and now it's been removed.
			IF inv_referral.nf_delete_referral(ll_claim_no, ll_docid) < 0 THEN
				SQLCA.nf_rollback_transaction()
				RETURN
			ELSE 
				lb_update_referral = TRUE				
			END IF
		END IF
	END IF

/* If the document is marked as paid then record it in the CLAIM database.
*/
	IF uo_document_indexing.dw_document_indexing.GetItemString(1,"paid_flag") = "Y" THEN

/* Check to see if the document is not already marked as paid.
*/
		ll_paiddoccount = dw_indexing_paid_documents.Retrieve(ll_docid)
		IF SQLCA.nf_handle_error("dw_indexing_paid_documents.Retrieve(ll_docid)","w_document_indexing","cb_route") < 0 THEN
			ImageTrans.nf_rollback_transaction()
			Close(Parent)
			RETURN
		END IF

		IF ll_paiddoccount = 0 THEN
			
			// if an SQLCA txn has not been begun by n_referral activity above, then begin a new txn
			SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
			IF li_trancount = 0 THEN
				SQLCA.nf_begin_transaction()
			END IF
			
			dw_indexing_paid_documents.Reset()
			dw_indexing_paid_documents.InsertRow(0)
			dw_indexing_paid_documents.SetItem(1,"doc_id",ll_docid)
			dw_indexing_paid_documents.SetItem(1,"paid_status_code","O")
			dw_indexing_paid_documents.SetItem(1,"paid_status_explanation_code","05")
			dw_indexing_paid_documents.SetItem(1,"payment_no",0)
	
			dw_indexing_paid_documents.Update()
			IF SQLCA.nf_handle_error("dw_indexing_paid_documents.Update()","w_document_indexing","cb_route") < 0 THEN
				ImageTrans.nf_rollback_transaction()
				Close(Parent)
				RETURN
			END IF
			lb_update_claim = TRUE
		END IF
	END IF

/*	If the document was marked as paid but now isn't then remove the payment/document link.
*/
	IF ib_already_indexed AND is_old_paid_flag = 'Y' AND uo_document_indexing.dw_document_indexing.GetItemString(1,"paid_flag") = 'N' THEN
		// if an SQLCA txn has not been begun by n_referral activity above, then begin a new txn
		SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
		IF li_trancount = 0 THEN
			SQLCA.nf_begin_transaction()
		END IF
		
		ll_counter = 1
		DO UNTIL ll_counter >= ll_paiddoccount
			IF dw_indexing_paid_documents.GetItemNumber(ll_counter,"doc_id") = ll_docid THEN
				dw_indexing_paid_documents.DeleteRow(ll_counter)
				ll_counter = ll_paiddoccount
			END IF
			ll_counter ++
		LOOP

		dw_indexing_paid_documents.Update()
		IF SQLCA.nf_handle_error("dw_indexing_paid_documents.Update()","w_document_indexing","cb_route") < 0 THEN
			ImageTrans.nf_rollback_transaction()
			Close(Parent)
			RETURN
		END IF
		lb_update_claim = TRUE
	END IF

/* If the document type is a letter then record the event.
*/
	IF Left(ls_type,1) = 'L' THEN

/* In the case of a change in claim numbers on a document, this document could already have an event
	associated with it.
*/
		ll_result = dw_event.Retrieve(ll_docid)
		IF SQLCA.nf_handle_error("dw_event.Retrieve(ll_docid)","w_document_indexing","cb_route") < 0 THEN
			ImageTrans.nf_rollback_transaction()
			Close(Parent)
			RETURN
		END IF

		IF ll_result = 0 THEN
			
			// if an SQLCA txn has not been begun by n_referral or 'paid documents' activities above, then begin a new txn
			SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
			IF li_trancount = 0 THEN
				SQLCA.nf_begin_transaction()
			END IF
			
/* Determine the event specific code.
*/
			ls_event_specific = inv_imaging.nf_choose_event_specific(ls_type)
			ldt_received      = uo_document_indexing.dw_document_indexing.GetItemDateTime(1,"date_received") 
			dw_event.Reset()
			dw_event.InsertRow(0)
			dw_event.SetItem(1,"doc_id",ll_docid)
			dw_event.SetItem(1,"claim_no",ll_claim_no)

			ll_event_no = inv_event_log.nf_next_claim_event_no(ll_claim_no)
			IF ll_event_no = -1 THEN
				Close(Parent)
				RETURN
			END IF

			dw_event.SetItem(1,"event_no",ll_event_no)
			IF IsNull(ldt_received) THEN
				ls_print_dt_received = '0000-00-00'
			ELSE
				ls_print_dt_received = String(ldt_received, 'yyyy-mm-dd')  
			END IF

			ll_result = inv_event_log.nf_create_auto_event(ll_claim_no,ll_event_no,"004",ls_type + "Dated: " + String(ldt_date,'yyyy-mm-dd') + "   Received: " + ls_print_dt_received,ls_event_specific)
			IF ll_result = -1 THEN
				Close(Parent)
				RETURN
			END IF
	
			dw_event.Update()
			IF SQLCA.nf_handle_error("dw_event.Update()","w_document_indexing","cb_route") < 0 THEN
				ImageTrans.nf_rollback_transaction()
				Close(Parent)
				RETURN
			END IF
			lb_update_claim = TRUE
		END IF
	END IF
	
	/* update the user_id/doc_id of the last indexed document to the 
	   new table - USER_LAST_INDEXED_DOC  we will use this table to 
		grab the last document that has been indexed for the "recall functionality
		supported by this application
	*/
	SELECT COUNT(*) INTO :ll_count FROM  USER_LAST_INDEXED_DOC WHERE user_id = :vgst_user_profile.user_id
   USING ImageTrans;
	ImageTrans.nf_handle_error("w_document_indexing","clicked","SELECT COUNT(*) INTO :ll_count FROM")

	IF ll_count = 1 THEN
		UPDATE USER_LAST_INDEXED_DOC  set docid = :ll_docid
		WHERE user_id = :vgst_user_profile.user_id
		USING ImageTrans;

      ImageTrans.nf_handle_error("w_document_indexing","clicked","UPDATE USER_LAST_INDEXED_DOC")
	ELSE
		INSERT USER_LAST_INDEXED_DOC (user_id,docid)
		VALUES (:vgst_user_profile.user_id,:ll_docid)
		USING ImageTrans;
      ImageTrans.nf_handle_error("w_document_indexing","clicked","INSERT USER_LAST_INDEXED_DOC ")
	END IF 
	
	/* update the Docid_Reference_Xref table so that the docid has it's indexed flag
	   set to "Y" - IF the doc is not there (it's old) and we dont care
	*/
	UPDATE Docid_Reference_Xref  SET indexed_flag = "Y"
	 WHERE docid = :ll_docid
	 USING ImageTrans;

   ImageTrans.nf_handle_error("w_document_indexing","clicked","UPDATE Docid_Reference_Xref")
	
	IF lb_update_claim or lb_update_referral THEN
		SQLCA.nf_commit_transaction()
	END IF

	ImageTrans.nf_commit_transaction()
	

/*	Remove the document from the list if it was just indexed and not routed.
*/
	IF NOT ib_already_indexed AND rb_index_only.checked = TRUE THEN
		ib_del_confirm_reqd = FALSE
		cb_delete_document.TriggerEvent(Clicked!)
	ELSE
		cb_refresh.TriggerEvent(Clicked!)
	END IF

CommonExit:
	wf_set_update_mode(False)
	wf_set_index_entry_mode(FALSE)
	// Close any open viewers
	ll_result = 1
	Do While ll_result > 0
		ll_result = f_close_viewer()
	Loop
	// Open the current document in viewer
	uo_image_append.of_init(ll_docid)
	uo_document_indexing.dw_document_indexing.SetFocus()
	
	
end event

type cb_alt1 from commandbutton within w_document_indexing
integer x = 1157
integer y = 1732
integer width = 128
integer height = 60
integer taborder = 230
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&?"
end type

on clicked;String	ls_colname

ls_colname = uo_document_indexing.dw_document_indexing.GetColumnName()

IF ls_colname = 'claim_no' THEN
	cb_search.TriggerEvent(Clicked!)
END IF

IF ls_colname = 'service_provider_no' THEN
	uo_document_indexing.cb_provider_search.TriggerEvent(Clicked!)
END IF

end on

type gb_routing_options from groupbox within w_document_indexing
integer x = 201
integer y = 1648
integer width = 832
integer height = 156
integer taborder = 110
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Options"
borderstyle borderstyle = styleraised!
end type

type uo_image_append from u_image_append within w_document_indexing
boolean visible = false
integer x = 2441
integer y = 716
integer taborder = 180
boolean bringtotop = true
end type

on uo_image_append.destroy
call u_image_append::destroy
end on

type cb_search from commandbutton within w_document_indexing
boolean visible = false
integer x = 942
integer y = 800
integer width = 82
integer height = 76
integer taborder = 200
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "?"
end type

event clicked;Long    ll_payment_no, ll_docid, ll_rows, ll_counter
Integer li_correspond_no
String  ls_payment_no_list, ls_msg
u_ds    lds_payment_document_payment
s_window_message lstr_message

// PR 2966 - Does a payment exist against the existing claim?
ll_docid = dw_document_list.GetItemNumber(dw_document_list.GetRow(), "ref_docid")

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
		RETURN
	END IF
END IF

// PR 3320 - Prevent out-going correspondence documents from being re-indexed to another claim.
SELECT IsNull( correspond_no , 0 )
  INTO :li_correspond_no
  FROM CORRESPONDENCE
 WHERE doc_id = :ll_docid
 USING SQLCA ;

IF li_correspond_no > 0 THEN
	MessageBox('Outgoing Correspondence', 'This document (' + String(ll_docid) + '.DOC) is outgoing correspondence #' +&
				  String(li_correspond_no) + '~r~nand cannot be re-indexed to another claim.' +&
				  '~r~nPerhaps you should archive this document and recreate it under the new claim.', Exclamation!)
	RETURN
END IF

lstr_message.awi_parent_window = PARENT
lstr_message.as_stringparm[1] = 'd_basic_claim_search'
OpenWithParm(w_indexing_search, lstr_message)

uo_document_indexing.dw_document_indexing.SetFocus()
uo_document_indexing.dw_document_indexing.SetColumn("claim_no")
end event

type cb_recall from commandbutton within w_document_indexing
integer x = 1193
integer y = 684
integer width = 311
integer height = 92
integer taborder = 170
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Recall"
end type

event clicked;LONG				  ll_docid, ll_claim_no, ll_provider,ll_set_row
S_WINDOW_MESSAGE lstr_message
DATETIME         ldtm_date_on_doc, ldtm_date_received
STRING           ls_type_code, ls_provider_type,ls_comment,ls_doc_type, ls_doc_class_code, ls_doc_subtype_code, ls_provider_type_code
         
/*
Information provided in the indexing section

SELECT  DOCUMENT_INDEX.docid, DOCUMENT_INDEX.claim_no ,
        DOCUMENT_INDEX.imaged_document_flag, DOCUMENT_INDEX.type_code ,
        DOCUMENT_INDEX.date_on_document, DOCUMENT_INDEX.comment ,
        DOCUMENT_INDEX.source_code, DOCUMENT_INDEX.sent_flag ,
        DOCUMENT_INDEX.service_provider_no, DOCUMENT_INDEX.english_flag ,
        DOCUMENT_INDEX.date_received, DOCUMENT_INDEX.reference_no ,
        message_info = '                               ',
        service_provider_name = '                                        ',
        document_desc = '                                             ',
        lost_time='N', DOCUMENT_INDEX.service_provider_type_code ,
        fldid = 0, paid_flag='N'    
   FROM DOCUMENT_INDEX      
  WHERE ( DOCUMENT_INDEX.docid = :al_docid )   
  
*/
/* open the window we will use to populate the information with */
Open(w_recall_document)

/* return the values...if any and populate the window */
lstr_message = Message.powerobjectparm

/* make sure we have a valid value */
IF NOT ISVALID(lstr_message) THEN RETURN

ll_claim_no        = lstr_message.al_doubleparm[1] 
ll_provider        = lstr_message.al_doubleparm[2] 
ldtm_date_on_doc   = lstr_message.adtm_datetimeparm[1] 
ls_type_code       = lstr_message.as_stringparm[1] 
ldtm_date_received = lstr_message.adtm_datetimeparm[2] 
ls_comment         =	lstr_message.as_stringparm[2]
ls_doc_class_code = lstr_message.as_stringparm[3]
ls_doc_subtype_code = lstr_message.as_stringparm[4]
ls_provider_type_code = lstr_message.as_stringparm[5]

/* all the docs have been removed so pop up the viewer again */
// View the document
ll_docid = dw_document_list.GetItemNumber(dw_document_list.getrow(),"ref_docid")
IF uo_image_append.of_init(ll_docid)	<= 0 THEN
	//do something here
END IF
		
ls_doc_type =  uo_image_append.of_get_file_type()
		
CHOOSE CASE ls_doc_type
/*  Imaged document */ 
	CASE 'IMA', 'TIF'
		uo_image_append.of_set_option()
		IF uo_image_append.of_append_image(ll_docid) < 0 THEN RETURN
	CASE ELSE	
END CHOOSE

ll_set_row = uo_document_indexing.dw_document_indexing.getrow()

IF ll_set_row > 0 THEN 
	
	/* check each value to see if there is something there... 
	   if so set it and trigger the user objects itemchanged event
	*/
	
	/* claim_no */
	IF NOT ISNULL(ll_claim_no) AND ll_claim_no > 0 	THEN
		uo_document_indexing.dw_document_indexing.setitem(ll_set_row,"claim_no",ll_claim_no)
	//	PR 3454 - Call to uf_set_claim added to fix problem where tombstone not refreshed when recalling a claim no. 
		uo_document_indexing.uf_set_claim(ll_claim_no)
		uo_document_indexing.dw_document_indexing.triggerevent("itemchanged")
	END IF
	
	/* service_provider_no */
	IF NOT ISNULL(ll_provider) AND ll_provider > 0 	THEN	
		uo_document_indexing.dw_document_indexing.setitem(ll_set_row,"service_provider_type_code",ls_provider_type_code)
		uo_document_indexing.dw_document_indexing.SetColumn("service_provider_type_code")
		uo_document_indexing.dw_document_indexing.triggerevent("itemchanged")
		uo_document_indexing.dw_document_indexing.setitem(ll_set_row,"service_provider_no",ll_provider)		
		uo_document_indexing.dw_document_indexing.SetColumn("service_provider_no")
		uo_document_indexing.dw_document_indexing.triggerevent("itemchanged")
	END IF 
	
	/* date_on_document */
	IF NOT ISNULL(ldtm_date_on_doc) THEN
		uo_document_indexing.dw_document_indexing.setitem(ll_set_row,"date_on_document",ldtm_date_on_doc)
		uo_document_indexing.dw_document_indexing.SetColumn("date_on_document")
		uo_document_indexing.dw_document_indexing.triggerevent("itemchanged")
	END IF
	
	/* date_received */
	IF NOT ISNULL(ldtm_date_received) THEN
		uo_document_indexing.dw_document_indexing.setitem(ll_set_row,"date_received",ldtm_date_received)
		uo_document_indexing.dw_document_indexing.SetColumn("date_received")
		uo_document_indexing.dw_document_indexing.triggerevent("itemchanged")
	END IF 
	
	/* type_code */
	IF NOT ISNULL(ls_type_code) AND TRIM(ls_type_code) > ""	THEN 
		uo_document_indexing.dw_document_indexing.setitem(ll_set_row,"doc_class_code",ls_doc_class_code)
		uo_document_indexing.dw_document_indexing.SetColumn("doc_class_code")
		uo_document_indexing.dw_document_indexing.triggerevent("itemchanged")
		uo_document_indexing.dw_document_indexing.setitem(ll_set_row,"type_code",ls_type_code)
		uo_document_indexing.dw_document_indexing.SetColumn("type_code")
		uo_document_indexing.dw_document_indexing.triggerevent("itemchanged")
		uo_document_indexing.dw_document_indexing.setitem(ll_set_row,"doc_subtype_code",ls_doc_subtype_code)
		uo_document_indexing.dw_document_indexing.SetColumn("doc_subtype_code")
		uo_document_indexing.dw_document_indexing.triggerevent("itemchanged")
	END IF
	
	/* comment */
	IF NOT ISNULL(ls_comment) AND TRIM(ls_comment) > ""	THEN
		IF Len(ls_comment) > 20 THEN	ls_comment= Trim(left(ls_comment,20)) ELSE ls_comment = Trim(ls_comment)
		uo_document_indexing.dw_document_indexing.setitem(ll_set_row,"comment",ls_comment)
		uo_document_indexing.dw_document_indexing.SetColumn("comment")
		uo_document_indexing.dw_document_indexing.triggerevent("itemchanged")
	END IF
END IF 

/* set focus to the details datawindow */
uo_document_indexing.dw_document_indexing.setfocus()


end event

type uo_document_indexing from u_document_index within w_document_indexing
event ue_keydown pbm_keydown
integer x = 174
integer y = 768
integer width = 2633
integer height = 788
integer taborder = 190
end type

on uo_document_indexing.destroy
call u_document_index::destroy
end on

type cb_refresh_cat from commandbutton within w_document_indexing
integer x = 1147
integer y = 92
integer width = 91
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "R"
end type

event clicked;// The scanning category has a setid of 6
Long ll_setid = 6
LONG ll_row

// Get all the buckets/categories in the scanning set
idwc_categories.Retrieve(ll_setid)
ImageTrans.nf_handle_error("vidwc_categories","w_document_indexing","cb_refresh_cat_click") 

IF dw_document_list.rowcount() <= 0 THEN
	ll_row = dw_select_category.InsertRow(0)
	dw_select_category.scrolltorow(ll_row)
END IF



end event

type dw_duplicate_documents from u_dw_online within w_document_indexing
boolean visible = false
integer x = 571
integer y = 908
integer width = 1641
integer height = 524
integer taborder = 10
string dataobject = "d_duplicate_documents"
boolean vscrollbar = true
end type

on clicked;call u_dwa::clicked;Long vll_listrow

vll_listrow = GetRow()
SelectRow(0,false)
SelectRow(vll_listrow,true)

end on

