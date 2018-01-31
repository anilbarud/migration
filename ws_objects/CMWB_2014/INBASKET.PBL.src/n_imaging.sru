$PBExportHeader$n_imaging.sru
forward
global type n_imaging from nonvisualobject
end type
end forward

global type n_imaging from nonvisualobject
end type
global n_imaging n_imaging

forward prototypes
public function integer nf_code_claimsworking (string as_windowname, long al_fldid, long al_claim_no, string as_action_code, date ad_action_date, string as_action_note, string as_claimant_name)
public function integer nf_create_claimsmaster (string as_windowname, long al_claim_no, string as_imaged_claim_flag)
public function string nf_choose_event_specific (string as_type)
public function integer nf_delete_workfolder (long al_fldid, boolean ab_unconditional_delete, string as_windowname)
public function long nf_create_workfolder (string as_windowname, long al_catid)
public function long nf_get_next_docid (string as_windowname)
public function long nf_get_next_pgfid (string as_windowname)
public function integer nf_recover_document (long al_docid, long al_fldid, string as_windowname)
public function string nf_service_provider_name (long al_recipient_no, string as_service_provider_type_code, string as_windowname)
public function long nf_create_template_document (string as_windowname, long al_fldid, string as_docextension, ref string as_pathname)
public function long nf_get_setid (long al_catid)
public function integer nf_archive_document (long al_claim_no, string as_imaged_claim_flag, long al_docid, long al_fldid, string as_reason, string as_other, string as_windowname)
end prototypes

public function integer nf_code_claimsworking (string as_windowname, long al_fldid, long al_claim_no, string as_action_code, date ad_action_date, string as_action_note, string as_claimant_name);/* This function creates the claimsworking entry and renames the work folder
	with the claimant's name.
*/

/* First create the claimsworking entry.
*/
	INSERT INTO CLAIM_WORKING (folderid, action_code, claim_no, action_note, action_date)
	VALUES (:al_fldid, :as_action_code, :al_claim_no, :as_action_note, :ad_action_date)
	USING ImageTrans;

	IF ImageTrans.nf_handle_error("Embedded SQL: Insert claimsworking",as_windowname,"nf_code_claimsworking") < 0 THEN
		RETURN -1
	END IF

/* Update the folder with the new name.
*/
	UPDATE FLD
		SET fldname = Upper(:as_action_code) + CONVERT(varchar(10),:al_claim_no) + :as_claimant_name
	 WHERE fldid = :al_fldid
	 USING ImageTrans;

	IF ImageTrans.nf_handle_error("Embedded SQL: Update FLD",as_windowname,"nf_code_claimsworking") < 0 THEN
		RETURN -1
	END IF

	RETURN 1
end function

public function integer nf_create_claimsmaster (string as_windowname, long al_claim_no, string as_imaged_claim_flag);STRING	ls_imaged_claim_flag
LONG		ll_fldid

/* This function creates a claimsmaster file and assigns a claim number to it
	NOTE: The function nf_create_workfolder commits when it gets the next folder number
	to use but does not commit the actual creation of the folder

	Return Values: -1 Failure
						 1 Success
						 0 Claim Already Exists
*/


/* First check that the claim number passed in does not already exist.
*/
	ls_imaged_claim_flag = ""

	SELECT imaged_claim_flag INTO :ls_imaged_claim_flag
	FROM CLAIM_MASTER
	WHERE claim_no = :al_claim_no
	USING ImageTrans;

	ImageTrans.nf_handle_error("Embedded SQL: Select CLAIM_MASTER",as_windowname,"nf_create_claimsmaster")

/* If the claim already exists then check to see if the user is just imaging a non-imaged claim.
*/
	IF (ls_imaged_claim_flag = 'Y' AND as_imaged_claim_flag = 'Y') OR (ls_imaged_claim_flag = 'N' AND as_imaged_claim_flag = 'N') THEN
		RETURN 0
	ELSEIF ls_imaged_claim_flag = 'Y' AND as_imaged_claim_flag = 'N' THEN
		MessageBox("Create Claimsmaster","An imaged claim may not be unimaged.")
		RETURN 0
	ELSEIF ls_imaged_claim_flag = 'N' AND as_imaged_claim_flag = 'Y' THEN
		UPDATE CLAIM_MASTER
			SET imaged_claim_flag = 'Y'
		 WHERE claim_no = :al_claim_no
		 USING ImageTrans;

		// expect 1 record to be updated
		ImageTrans.nf_handle_error("Embedded SQL: Update CLAIM_MASTER",as_windowname,"nf_create_claimsmaster",1)

		RETURN 1
	END IF

/* Create a new folder.
*/
	// this function begins & commits an IMARA txn, then begins another, which must be committed by calling object
	ll_fldid = nf_create_workfolder(as_windowname,2)
	IF ll_fldid = -1 THEN
		RETURN -1
	END IF

/* Code the new folder with the claim number.
*/
	INSERT INTO CLAIM_MASTER(folderid,claim_no,imaged_claim_flag)
	VALUES (:ll_fldid, :al_claim_no, :as_imaged_claim_flag)
	USING ImageTrans;
	
	// expect 1 record to be inserted
	ImageTrans.nf_handle_error("Embedded SQL: Insert CLAIM_MASTER",as_windowname,"nf_create_claimsmaster",1)

	RETURN 1

end function

public function string nf_choose_event_specific (string as_type);/* This function looks at the imaging document type and returns the event
	specific type code that corresponds.
*/
	CHOOSE CASE Trim(as_type)
		CASE 'LA'
/* Letter about account.
*/
			RETURN 'OTH'
		CASE 'LC' 
/* Claimant Letter.
*/
			RETURN 'CL'
		CASE 'LE'
/* Employer Letter.
*/
			RETURN 'EM'
		CASE 'LO' 
/* Outside Agency Letter.
*/
			RETURN 'OA'
		CASE 'LR' 
/* Representative Letter.
*/
			RETURN 'RE'
		CASE 'LS' 
/* Service Provider Letter.
*/
			RETURN 'SP'
		CASE 'LT'
/* Third Party Letter.
*/
			RETURN 'TP'
		CASE ELSE 
/* Anything Else.
*/
			RETURN 'OTH'
		END CHOOSE

end function

public function integer nf_delete_workfolder (long al_fldid, boolean ab_unconditional_delete, string as_windowname);STRING	ls_check_action
STRING	ls_create, ls_begin, ls_commit, ls_drop
LONG		ll_doc_cnt, ll_fldid_count
INTEGER	li_error_code
BOOLEAN	lb_needtodelete, lb_erroroccured

/*	This function deletes a folder entry from the FLD, PRV_FLD,
	REF, and CLAIM_WORKING tables in the IMARA_DB database.  A folder
	containing documents which exist nowhere else in the system cannot
	be deleted unless the "unconditional delete" flag is set to true

	Input Parameter:  al_fldid - Folder id of folder to be deleted
                     ab_unconditional_delete - A flag determining whether
                                               or not a folder can be 
                                               deleted if it contains 
                                               documents which exist no
                                               where else in the system.

	Return Values:    1 - Successful
                     0 - Folder contains documents nowhere else in the system
						  -1 - Unsuccessful
*/
	lb_needtodelete = FALSE
	lb_erroroccured = FALSE

/* Check that the folder number passed in is not for a master folder.
*/
	SELECT count(*)
	  INTO :ll_fldid_count
	  FROM REF
	 WHERE docfldid = :al_fldid
		AND doccatid = 2
	 USING ImageTrans;

	IF ImageTrans.nf_handle_error("Embedded SQL: Select from REF",as_windowname,"nf_delete_workfolder()") < 0 THEN
		RETURN -1
	END IF

	IF ll_fldid_count > 0 THEN
		MessageBox("Master Folder","You can not delete a master folder.")
		RETURN -1
	END IF

/*	Need to create a temporary table in order to do the delete of a workfolder. In order to do this
	the current transaction must be shut down. So commit, then create the table, then start the transaction
	again.
*/
		
	ls_create = 'CREATE TABLE #Docids_To_Update (docid int not null)'
	EXECUTE IMMEDIATE :ls_create USING ImageTrans;

	IF ImageTrans.nf_handle_error('CREATE TABLE #Docids_To_Update (docid int not null)','n_imaging','nf_delete_workfolder()') < 0 THEN
		RETURN -1
	END IF

	lb_needtodelete = TRUE

/* Count number of documents only in this folder.
*/
	IF ab_unconditional_delete = FALSE THEN
		SELECT count(*)
		INTO :ll_doc_cnt
		FROM REF,DOC
		WHERE REF.docid = DOC.docid
		AND DOC.docrefcount = 1
		AND REF.docfldid = :al_fldid
		USING ImageTrans;

		IF ImageTrans.nf_handle_error("Embedded SQL: Counting documents in Imaging",as_windowname,"nf_delete_workfolder()") < 0 THEN
			lb_erroroccured = TRUE
			GOTO DeleteTableAfterError
		END IF

	/* If rows found then warn user and skip deletion.
	*/
		IF ll_doc_cnt > 0 THEN
			MessageBox("Document May Not Exist in Master", "At least one document in folder exists nowhere else in system. Cannot delete folder.",STOPSIGN!,OK!)
			lb_erroroccured = FALSE
			GOTO DeleteTableAfterError
		END IF
	END IF

	INSERT INTO #Docids_To_Update
		(docid)
	SELECT docid
	  FROM REF
	 WHERE docfldid = :al_fldid
	USING ImageTrans;

	IF ImageTrans.nf_handle_error('INSERT INTO #Docids_To_Update','n_imaging','nf_delete_workfolder()') < 0 THEN
		lb_erroroccured = TRUE
		GOTO DeleteTableAfterError
	END IF
	

	ImageTrans.nf_begin_transaction()

/* Delete REF entries for folder.
*/
	DELETE REF
	 WHERE docfldid = :al_fldid
	 USING ImageTrans;

	li_error_code = ImageTrans.nf_handle_error("Embedded SQL: Deleting REF entries for folder",as_windowname,"nf_delete_workfolder()")
	IF li_error_code < 0 THEN
		lb_erroroccured = TRUE
		GOTO DeleteTableAfterError
	ELSE
		IF li_error_code = 100 THEN
			ImageTrans.nf_rollback_transaction()
			MessageBox("Folder","Folder not found to delete.",Exclamation!)
			lb_erroroccured = TRUE
			GOTO DeleteTableAfterError
		END IF
	END IF

	UPDATE DOC
	   SET docrefcount = docrefcount - 1
	  FROM DOC, #Docids_To_Update
	 WHERE DOC.docid = #Docids_To_Update.docid
	USING ImageTrans;

	IF ImageTrans.nf_handle_error('UPDATE DOC SET docrefcount = docrefcount - 1','n_imaging','nf_delete_workfolder()') < 0 THEN
		lb_erroroccured = TRUE
		GOTO DeleteTableAfterError
	END IF

/* Delete folder indexing information.
*/
	DELETE CLAIM_WORKING
	 WHERE folderid = :al_fldid
	 USING ImageTrans;

/* It is possible that there are no rows to delete - if the folder wasn't coded.
*/
	IF ImageTrans.nf_handle_error("Embedded SQL: Deleting CLAIM_WORKING",as_windowname,"nf_delete_workfolder()") < 0 THEN
		lb_erroroccured = TRUE
		GOTO DeleteTableAfterError
	END IF

/* Delete folder information.
*/
	DELETE FLD
	 WHERE fldid = :al_fldid
	 USING ImageTrans;

	li_error_code  = ImageTrans.nf_handle_error("Embedded SQL: Deleting FLD entries for folder",as_windowname,"nf_delete_workfolder()")
	IF li_error_code < 0 THEN
		lb_erroroccured = TRUE
		GOTO DeleteTableAfterError
	ELSE
		IF li_error_code = 100 THEN
			ImageTrans.nf_rollback_transaction()
			MessageBox("Folder","Folder not found to delete.",Exclamation!)
			lb_erroroccured = TRUE
			GOTO DeleteTableAfterError
		END IF
	END IF

DeleteTableAfterError:
/*	Since all has been updated, shut off transaction so that the temporary table can be dropped.
	After drop, turn transaction back on.
*/
	IF lb_needtodelete THEN
		
		ImageTrans.nf_commit_transaction()
		
		ls_drop = 'DROP TABLE #Docids_To_Update'
		EXECUTE IMMEDIATE :ls_drop USING ImageTrans;
		IF ImageTrans.nf_handle_error('DROP TABLE #Docids_To_Update','n_imaging','nf_delete_workfolder()') < 0 THEN
			RETURN -1
		END IF
		  
	END IF

	IF lb_erroroccured THEN
		RETURN -1
	ELSE
		RETURN 1
	END IF

end function

public function long nf_create_workfolder (string as_windowname, long al_catid);LONG	ll_fldid, ll_result

/* This function creates a workfolder in the category specified

	THIS FUNCTION COMMITS SO WATCH WHERE YOU USE IT!!!!

	This function commits so that it doesn't hold locks on the ID table
	since this table is used for document numbers, folder numbers, category numbers, etc...
*/

/* Increment the folder number then read it back.
*/

ImageTrans.nf_begin_transaction()

	UPDATE ID
		SET idvalue = idvalue + 1
	 WHERE idlevel = 3
	 USING ImageTrans;
	// expect one record to be updated
	ImageTrans.nf_handle_error("Embedded SQL: Update ID for next folder number",as_windowname,"nf_create_workfolder",1)


	SELECT idvalue
	  INTO :ll_fldid
	  FROM ID
	 WHERE idlevel = 3
	 USING ImageTrans;

	ll_result =  ImageTrans.nf_handle_error("Embedded SQL: Update ID for next folder number",as_windowname,"nf_create_workfolder") 
	IF ll_result = 100 THEN
		ImageTrans.nf_rollback_transaction()
		
		MessageBox("Create WorkFolder","Could not get next folder number to create work folder.")
		RETURN -1
	END IF
	
// commit matches begin above
ImageTrans.nf_commit_transaction()
	

/* Create the workfolder.
*/
	INSERT INTO FLD (fldid, fldcatid, fldname )
	VALUES (:ll_fldid,:al_catid,Convert(varchar(10),:ll_fldid) )
	USING ImageTrans;
	// expect one record to be inserted
	ImageTrans.nf_handle_error("Embedded SQL: Insert into FLD",as_windowname,"nf_create_workfolder",1)


	RETURN ll_fldid

end function

public function long nf_get_next_docid (string as_windowname);LONG ll_result, ll_docid


ImageTrans.nf_begin_transaction()

/* PLEASE NOTE: This function COMMITS when it grabs an id value so that other Imara
	users are not locked out.
*/
	UPDATE ID 
		SET idvalue=idvalue + 1 
	 WHERE idlevel = 4
	 USING ImageTrans;
	// expect one record to be updated
	ImageTrans.nf_handle_error("Embedded SQL: Update ID for new docid , idlevel = 4",as_windowname,"nf_get_next_docid",1)


	SELECT idvalue
	  INTO :ll_docid
	  FROM ID 
	 WHERE idlevel = 4
	 USING ImageTrans;

	ll_result = ImageTrans.nf_handle_error("Embedded SQL: Select from ID for docid, idlevel = 4",as_windowname,"nf_get_next_docid")
	IF ll_result = 100 THEN
		MessageBox("Next Document","Unable to get next docid from ID table.",StopSign!)
		RETURN -1
	END IF

ImageTrans.nf_commit_transaction()


RETURN ll_docid

end function

public function long nf_get_next_pgfid (string as_windowname);LONG	ll_result, ll_pgfid

/*	This function gets the next pgfid in order to create the required page entries for a document.

	PLEASE NOTE: This function COMMITS when it grabs an id value so that other Imara users
	are not locked out.
*/

ImageTrans.nf_begin_transaction()
	
/* Get the next pgfid for page entry.
*/
	UPDATE ID 
		SET idvalue=idvalue + 1 
	 WHERE idlevel = 5
	 USING ImageTrans;
	// expect one record to be updated
	ImageTrans.nf_handle_error("Embedded SQL: Update ID for new pgfid",as_windowname,"nf_get_next_pgfid",1)

	SELECT idvalue
	  INTO :ll_pgfid
	  FROM ID 
	 WHERE idlevel = 5
	 USING ImageTrans;

	ll_result = ImageTrans.nf_handle_error("Embedded SQL: Select from ID for pgfid",as_windowname,"nf_get_next_pgfid")
	IF ll_result = 100 THEN
		MessageBox("Next Page","Unable to get pgfid from ID table.",StopSign!)
		RETURN -1
	END IF

ImageTrans.nf_commit_transaction()
	
	
RETURN ll_pgfid

end function

public function integer nf_recover_document (long al_docid, long al_fldid, string as_windowname);INTEGER	li_count
LONG		ll_docid, ll_pgfid, ll_claim_no, ll_fldid, ll_result
STRING	ls_type

/* Get the claim number associated with the folder.
*/
	SELECT claim_no
	  INTO :ll_claim_no
	  FROM CLAIM_MASTER_ARCHIVE
	 WHERE folderid = :al_fldid
	 USING ImageTrans;

	ll_result = ImageTrans.nf_handle_error("Embedded SQL: Select from CLAIM_MASTER_ARCHIVE",as_windowname,"nf_recover_document") 
	IF ll_result < 0 THEN
		RETURN -1
	ELSE
		IF ll_result = 100 THEN
			MessageBox("Document Recover","Could not find claim associated with document to switch.")
			RETURN -1
		END IF
	END IF

/* Check to see if a master exists for the claim.
*/
	SELECT folderid
	  INTO :ll_fldid
	  FROM CLAIM_MASTER
	 WHERE claim_no = :ll_claim_no
	 USING ImageTrans;

	ll_result = ImageTrans.nf_handle_error("Embedded SQL: Select from claimsmaster",as_windowname,"nf_recover_document")
	IF ll_result < 0 THEN
		RETURN -1
	ELSE
		IF ll_result = 100 THEN
/* No master record.
*/
			MessageBox("Document Recover","Unable to find claim master record.",StopSign!)
			RETURN -1
		END IF
	END IF

/* Check for indexing information.
*/
	SELECT count(*)
	  INTO :li_count
	  FROM DOCUMENT_INDEX_ARCHIVE
	 WHERE docid = :al_docid
	 USING ImageTrans;

	ll_result = ImageTrans.nf_handle_error("Embedded SQL: Select from DOCUMENT_INDEX_ARCHIVE",as_windowname,"nf_recover_document")
	IF ll_result < 0 THEN
		RETURN -1
	ELSE
		IF ll_result = 100 THEN
/* No indexing information.
*/
			MessageBox("Document Recover","Unable to find archive record.",StopSign!)
			RETURN -1
		END IF
	END IF

	ImageTrans.nf_begin_transaction()

/* Now move the document back to the master category.
*/
	INSERT INTO DOCUMENT_INDEX (docid,type_code,doc_subtype_code,date_on_document,comment,source_code,sent_flag,service_provider_no,
										 service_provider_type_code,claim_no,reference_no,date_received,english_flag,
										 imaged_document_flag)
	SELECT docid,type_code,doc_subtype_code,date_on_document,comment,source_code,sent_flag,service_provider_no,
			 service_provider_type_code,claim_no,reference_no,date_received,english_flag,imaged_document_flag
	  FROM DOCUMENT_INDEX_ARCHIVE
	 WHERE docid = :al_docid
	 USING ImageTrans;

	IF ImageTrans.nf_handle_error("Embedded SQL: Insert into docindex",as_windowname,"nf_recover_document") < 0 THEN
		RETURN -1
	END IF

	IF ImageTrans.SQLNRows <> 1 THEN
		ImageTrans.nf_rollback_transaction()

		MessageBox("Document Recover","Unable to recover index record.",StopSign!)
		RETURN -1
	END IF

/* Delete the original docindex record.
*/
	DELETE
	  FROM DOCUMENT_INDEX_ARCHIVE
	 WHERE docid = :al_docid
	 USING ImageTrans;

	IF ImageTrans.nf_handle_error("Embedded SQL: Delete from docindex_archive",as_windowname,"nf_recover_document") < 0 THEN
		RETURN -1
	END IF

	IF ImageTrans.SQLNRows <> 1 THEN
		ImageTrans.nf_rollback_transaction()
		
		MessageBox("Document Recover","Unable to remove docindex record.",StopSign!)
		RETURN -1
	END IF

/* Move the document from archive to master category.
*/
	UPDATE REF
		SET doccatid = 2,
			 docfldid = :ll_fldid
	  FROM REF
	 WHERE doccatid = 258
	   AND docfldid = :al_fldid
      AND docid    = :al_docid
	 USING ImageTrans;

	IF ImageTrans.nf_handle_error("Embedded SQL: Update REF",as_windowname,"nf_recover_document") < 0 THEN
		RETURN -1
	END IF

	IF ImageTrans.SQLNRows <> 1 THEN
		ImageTrans.nf_rollback_transaction()
		
		MessageBox("Document Recover","Unable to update REF.",StopSign!)
		RETURN -1
	END IF

	ImageTrans.nf_commit_transaction()

	RETURN 0

end function

public function string nf_service_provider_name (long al_recipient_no, string as_service_provider_type_code, string as_windowname);LONG		ll_result
STRING	ls_recipient_name

/*	This function gets the payee name for the recipient number passed.
*/
	IF NOT ISNULL(as_service_provider_type_code) THEN
		SELECT PROVIDER.name
		  INTO :ls_recipient_name
		 FROM PROVIDER  
		 WHERE ( PROVIDER.provider_no = :al_recipient_no )
			AND ( PROVIDER.provider_type_code = :as_service_provider_type_code)
       	AND ( PROVIDER.active_flag = 'Y' )
		 USING SQLCA;
	
		ll_result = SQLCA.nf_handle_error("Embedded SQL: Retrieve on PROVIDER",as_windowname,"nf_service_provider_name")
		IF ll_result < 0 THEN
			RETURN ""
		ELSEIF ll_result = 100 THEN
			RETURN ""
		ELSE
			RETURN ls_recipient_name
		END IF
	ELSE
		RETURN ""
	END IF

end function

public function long nf_create_template_document (string as_windowname, long al_fldid, string as_docextension, ref string as_pathname);LONG		ll_result, ll_catid, ll_setid, ll_docid, ll_pgfid
STRING	ls_strpath,	ls_directory, ls_extension

/*	This function creates the logical entries for a new document
	(it was written for Word or Excel documents because it creates
	the logical entries pointing to the non-image share)

	PLEASE NOTE: This function COMMITS when it grabs an id value so
	that other Imara users are not locked out

	This function does not commit the creation of the logical entries
	for the document.  This should be done in the calling code.
*/

	as_docextension = Upper(as_docextension)

/* Get the store path. When strid = 24.
*/
	SELECT strpath INTO :ls_strpath
	  FROM STORE
	 WHERE strid = 24
	 USING ImageTrans;

	ll_result = ImageTrans.nf_handle_error("Embedded SQL: Select from STORE strid = 24",as_windowname,"nf_create_template_document")
	IF ll_result = 100 THEN
		MessageBox("Create Document Template","Unable to find storage path (24). Please check with the Helpdesk.",StopSign!)
		RETURN -1
	END IF

/* Get the year and month (YYMM) to build the directory path.
*/
	ls_directory = Right(String(Year(Today())),2) + String(Month(Today()),"00")

/* Validate the folder and get the category and set that it exists in.
*/
	SELECT fldcatid, setid
	  INTO :ll_catid, :ll_setid
	  FROM FLD, CAT
	 WHERE fldcatid = catid 
		AND fldid = :al_fldid
	 USING ImageTrans;

	ll_result = ImageTrans.nf_handle_error("Embedded SQL: Select from FLD and CAT",as_windowname,"nf_create_template_document")
	IF ll_result = 100 THEN
		MessageBox("Create Document Template","Unable to find folder" + String(al_fldid) + ".",StopSign!)
		RETURN -1
	END IF

/* Get the next docid for document entry.
*/
	ll_docid = nf_get_next_docid(as_windowname)
	IF ll_docid = -1 THEN
		RETURN -1
	END IF

/* Get the next pgfid for page entries.
*/
	ll_pgfid = nf_get_next_pgfid(as_windowname)
	IF ll_pgfid = -1 THEN
		RETURN -1
	END IF
	
	// *********************************************************************************
	// THIS IMARA TXN must be committed by calling object
	ImageTrans.nf_begin_transaction( )

   INSERT INTO REF (docid, docfldid, docsetid, doccatid )
   VALUES (:ll_docid, :al_fldid, :ll_setid, :ll_catid )
	USING ImageTrans;	
	// expect one record to be inserted
	ImageTrans.nf_handle_error("Embedded SQL: Insert into REF",as_windowname,"nf_create_template_document",1)


/* Create logical document entry.
*/
	ls_extension = Lower(as_docextension)
	INSERT INTO DOC (docid,docname,docnpgs,docrefcount, file_extension )
	VALUES (:ll_docid, Convert(varchar(10),:ll_docid), 1, 1, :ls_extension )
	USING ImageTrans;
	// expect one record to be inserted
	ImageTrans.nf_handle_error("Embedded SQL: Insert into DOC",as_windowname,"nf_create_template_document",1)
	

/* Create logical page entries.
*/
	INSERT INTO PAG (pagdocid,pagseq,pagfid )
	VALUES (:ll_docid,1,:ll_pgfid )
	USING ImageTrans;
	// expect one record to be inserted
	ImageTrans.nf_handle_error("Embedded SQL: Insert into PAG",as_windowname,"nf_create_template_document",1)


	INSERT INTO PGF (pgfid, pgffilename, pgftype, pgfstore)
	VALUES (:ll_pgfid, :ls_directory + "\" + CONVERT(varchar(10),:ll_docid) + "." + :as_docextension, "T", 24)
	USING ImageTrans;
	// expect one record to be inserted
	ImageTrans.nf_handle_error("Embedded SQL: Insert into PGF",as_windowname,"nf_create_template_document",1)

/* Sent up the pathname for the new document.
*/
	as_pathname = ls_strpath + "\" + ls_directory + "\" + String(ll_docid) + "." + as_docextension

	RETURN ll_docid

end function

public function long nf_get_setid (long al_catid);LONG		ll_setid

SELECT setid
  INTO :ll_setid
  FROM CAT
 WHERE catid = :al_catid
 USING ImageTrans;

IF ImageTrans.nf_handle_error("Embedded SQL: Select from CAT","n_imaging","nf_get_setid") < 0 THEN
	Close(PARENT)
	RETURN -1
END IF


RETURN ll_setid
end function

public function integer nf_archive_document (long al_claim_no, string as_imaged_claim_flag, long al_docid, long al_fldid, string as_reason, string as_other, string as_windowname);INTEGER	li_count, li_counter, li_trancount
LONG		ll_docid, ll_pgfid, ll_fldid, ll_result
STRING	ls_type, ls_subtype

/*	Check to see if an archival master exists for the claim.
*/
SELECT folderid
INTO   :ll_fldid
FROM   CLAIM_MASTER_ARCHIVE
WHERE  claim_no = :al_claim_no
USING ImageTrans;

ll_result = ImageTrans.nf_handle_error("Embedded SQL: Select from claimsmaster_archive",as_windowname,"nf_archive_document")
IF ll_result < 0 THEN
	RETURN -1
ELSE
	IF ll_result = 100 THEN

/* No archive record was found so create one, but first need to create a folder.
*/
		ImageTrans.nf_begin_transaction()
	
		// this function begins & commits its own transaction, then inserts data outside of txn,
		// so it must be enclosed within its own txn
		
		ll_fldid = nf_create_workfolder(as_windowname,258)
		IF ll_fldid = -1 THEN
			RETURN -1
		END IF

		INSERT INTO CLAIM_MASTER_ARCHIVE
		       (folderid, claim_no, imaged_claim_flag)
		VALUES (:ll_fldid, :al_claim_no, :as_imaged_claim_flag)
		USING ImageTrans;

		IF ImageTrans.nf_handle_error("Embedded SQL: Insert into CLAIM_MASTER_ARCHIVE",as_windowname,"nf_archive_document") < 0 THEN
			RETURN -1
		END IF

		IF ImageTrans.SQLNRows <> 1 THEN
			ImageTrans.nf_rollback_transaction()

			MessageBox("Document Archive","Unable to create claim archive record.",StopSign!)
			RETURN -1
		END IF
	END IF
END IF


/* See if the document exists in another master folder.
*/
SELECT count(*)
INTO   :li_count
FROM   REF
WHERE  docid = :al_docid 
AND    doccatid = 2
USING ImageTrans;

IF ImageTrans.nf_handle_error("Embedded SQL: Select from REF",as_windowname,"nf_archive_document") < 0 THEN
	RETURN -1
END IF


// if an IMARA txn has not been started by the call of nf_create_workfolder above, then start one here
ImageTrans.nf_transaction_count(li_trancount,0,'','','',FALSE)
IF li_trancount = 0 THEN
	ImageTrans.nf_begin_transaction( )
END IF

/* If the document exists in another master then duplicate the logical entries to create a
	second copy of the document.
*/
IF li_count > 1 THEN

/* Get the next available docid.
*/
	ll_docid = nf_get_next_docid(as_windowname)
	IF ll_docid = -1 THEN
		RETURN -1
	END IF

/* Create logical document entry.
*/
	INSERT INTO DOC (docid,docname,docnpgs,docrefcount, file_extension)
	SELECT :ll_docid,docname,docnpgs,docrefcount, file_extension
	FROM   DOC
	WHERE  DOC.docid = :al_docid
	USING  ImageTrans;

	IF ImageTrans.nf_handle_error("Embedded SQL: Insert into DOC",as_windowname,"nf_archive_document") < 0 THEN
		RETURN -1
	END IF

	IF ImageTrans.SQLNRows <> 1 THEN
		ImageTrans.nf_rollback_transaction()
		MessageBox("Document Archive","Unable to create document entry.",StopSign!)
		RETURN -1
	END IF

/* Create the page entries. First, find out how may pages make up this document.
*/
	SELECT count(*) INTO :li_count
	FROM   PAG
	WHERE  pagdocid = :al_docid
	USING ImageTrans;

	IF ImageTrans.nf_handle_error("Embedded SQL: Select from PAG",as_windowname,"nf_archive_document") < 0 THEN
		RETURN -1
	END IF

/* Create a page record for each page of the new document.
*/
	li_counter = 1
	DO WHILE li_counter <= li_count

		INSERT INTO PAG (pagdocid,pagseq,pagfid )
		SELECT :ll_docid,pagseq,pagfid
		FROM   PAG 
		WHERE  PAG.pagdocid = :al_docid
		AND    PAG.pagseq = :li_counter
		USING ImageTrans;

		IF ImageTrans.nf_handle_error("Embedded SQL: Insert into PAG",as_windowname,"nf_archive_document") < 0 THEN
			RETURN -1
		END IF

		IF ImageTrans.SQLNRows <> 1 THEN
			ImageTrans.nf_rollback_transaction()
			MessageBox("Document Indexing","Unable to create page entry.",StopSign!)
			RETURN -1
		END IF
		li_counter++
	LOOP

/* Replace the original document with the newly created one (this leaves the original document in the other master).
*/
	UPDATE REF
	SET    docid = :ll_docid
	WHERE  REF.docid = :al_docid
	AND    REF.doccatid = 2
	AND    REF.docfldid = :al_fldid
	USING ImageTrans;

	IF ImageTrans.nf_handle_error("Embedded SQL: Update REF",as_windowname,"nf_archive_document") < 0 THEN
		RETURN -1
	END IF

	IF ImageTrans.SQLNRows <> 1 THEN
		ImageTrans.nf_rollback_transaction()
		MessageBox("Document Indexing","Unable to create reference entry.",StopSign!)
		RETURN -1
	END IF

/* Create a DOCUMENT_INDEX record for this document. If original document type is one of the
	old 'split' codes then it has to be recoded to 'JK' to be archived.
*/
	SELECT type_code, doc_subtype_code INTO :ls_type, :ls_subtype
	FROM   DOCUMENT_INDEX
	WHERE  docid = :al_docid
	USING ImageTrans;

	IF ImageTrans.nf_handle_error("Embedded SQL: Select from DOCUMENT_INDEX",as_windowname,"nf_archive_document") < 0 THEN
		RETURN -1
	END IF

	IF ls_type = 'AC!' Or ls_type = 'AD!' Or ls_type = 'SD!' Or ls_type = 'MP!' THEN
		ls_type = 'JK'
	END IF

/* This may not create an entry if the document is not indexed.
*/
	INSERT INTO DOCUMENT_INDEX (docid,type_code,doc_subtype_code,date_on_document,comment,source_code,sent_flag,
										 service_provider_no, service_provider_type_code,claim_no,reference_no,
										 date_received,english_flag,imaged_document_flag)
	SELECT :ll_docid,:ls_type, :ls_subtype,date_on_document,comment,source_code,sent_flag,service_provider_no,
			 service_provider_type_code,claim_no,reference_no, date_received, english_flag, imaged_document_flag
	FROM   DOCUMENT_INDEX
	WHERE  DOCUMENT_INDEX.docid = :al_docid
	USING ImageTrans;

	IF ImageTrans.nf_handle_error("Embedded SQL: Insert into DOCUMENT_INDEX",as_windowname,"nf_archive_document") < 0 THEN
		RETURN -1
	END IF

/* Set the docid to the new document number.
*/
	al_docid = ll_docid
END IF

/* Check for indexing information.
*/
SELECT count(*) INTO :li_count
FROM   DOCUMENT_INDEX
WHERE  docid = :al_docid
USING ImageTrans;

ll_result = ImageTrans.nf_handle_error("Embedded SQL: Select from DOCUMENT_INDEX",as_windowname,"nf_archive_document")
IF ll_result < 0 THEN
	RETURN -1
ELSE
	IF ll_result = 100 THEN

/* No indexing information, create it.
*/
		INSERT INTO DOCUMENT_INDEX (docid,type_code,doc_subtype_code,date_on_document,comment,source_code,sent_flag,
											 service_provider_no, service_provider_type_code,claim_no,reference_no,
											 date_received,english_flag,imaged_document_flag)
		VALUES (:al_docid,'JK','',getdate(),"",'I',"N",0,"",:al_claim_no,0,getdate(),"Y",:as_imaged_claim_flag)
		USING ImageTrans;

		IF ImageTrans.nf_handle_error("Embedded SQL: Insert into DOCUMENT_INDEX",as_windowname,"nf_archive_document") < 0 THEN
			RETURN -1
		END IF

		IF ImageTrans.SQLNRows <> 1 THEN
			ImageTrans.nf_rollback_transaction()
			MessageBox("Document Archive","Unable to create DOCUMENT_INDEX record.",StopSign!)
			RETURN -1
		END IF
	END IF
END IF

/* Now move the document to the archive category.
*/
INSERT INTO DOCUMENT_INDEX_ARCHIVE(docid,type_code,doc_subtype_code,date_on_document,comment,source_code,sent_flag,
											  service_provider_no, service_provider_type_code,claim_no,reference_no,
											  date_received,english_flag,imaged_document_flag,reason_code,reason_text)
SELECT docid,type_code,doc_subtype_code,date_on_document,comment,source_code,sent_flag,service_provider_no,
		 service_provider_type_code,claim_no,reference_no,date_received,english_flag,imaged_document_flag,
		 :as_reason,:as_other
FROM   DOCUMENT_INDEX
WHERE  docid = :al_docid
USING ImageTrans;

IF ImageTrans.nf_handle_error("Embedded SQL: Insert into DOCUMENT_INDEX_ARCHIVE",as_windowname,"nf_archive_document") < 0 THEN
	RETURN -1
END IF

IF ImageTrans.SQLNRows <> 1 THEN
	ImageTrans.nf_rollback_transaction()
	MessageBox("Document Archive","Unable to archive DOCUMENT_INDEX record.",StopSign!)
	RETURN -1
END IF

/* Delete the original DOCUMENT_INDEX record.
*/
DELETE
FROM DOCUMENT_INDEX
WHERE docid = :al_docid
USING ImageTrans;

IF ImageTrans.nf_handle_error("Embedded SQL: Delete from DOCUMENT_INDEX",as_windowname,"nf_archive_document") < 0 THEN
	RETURN -1
END IF

IF ImageTrans.SQLNRows <> 1 THEN
	ImageTrans.nf_rollback_transaction()
	MessageBox("Document Archive","Unable to remove DOCUMENT_INDEX record.",StopSign!)
	RETURN -1
END IF

/* Delete the document from all work folders.
*/
DELETE
FROM  REF
WHERE docid = :al_docid
AND   doccatid <> 2
USING ImageTrans;

IF ImageTrans.nf_handle_error("Embedded SQL: Delete from REF",as_windowname,"nf_archive_document") < 0 THEN
	RETURN -1
END IF
	
	
//docrefcount fix  MA000826 pr_1421	

/* Update Doc with new docrefcount
*/
UPDATE DOC
SET docrefcount = docrefcount - :ImageTrans.SQLNRows
WHERE docid = :al_docid
USING ImageTrans;
 
IF ImageTrans.nf_handle_error("Embedded SQL: Update DOC",as_windowname,"nf_archive_document") < 0 THEN
	RETURN -1
END IF

IF ImageTrans.SQLNRows <> 1 THEN
	ImageTrans.nf_rollback_transaction()
	MessageBox("Document Archive","Unable to update DOC.",StopSign!)
	RETURN -1
END IF

/* Move the document from the master category to the archive category.
*/
UPDATE REF
SET    doccatid = 258,
		 docfldid = :ll_fldid
FROM   REF
WHERE  doccatid = 2
AND    docfldid = :al_fldid
AND    docid    = :al_docid
USING ImageTrans;

IF ImageTrans.nf_handle_error("Embedded SQL: Update REF",as_windowname,"nf_archive_document") < 0 THEN
	RETURN -1
END IF

IF ImageTrans.SQLNRows <> 1 THEN
	ImageTrans.nf_rollback_transaction()
	MessageBox("Document Archive","Unable to update REF.",StopSign!)
	RETURN -1
END IF

ImageTrans.nf_commit_transaction()

RETURN 0

end function

on n_imaging.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_imaging.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

