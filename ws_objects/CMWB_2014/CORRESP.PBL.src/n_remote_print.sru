$PBExportHeader$n_remote_print.sru
$PBExportComments$Remote Print System (RPS) Object - Writes Structures to tables.
forward
global type n_remote_print from uo_nvo_base
end type
type str_cc from structure within n_remote_print
end type
type str_attach from structure within n_remote_print
end type
type str_orig from structure within n_remote_print
end type
end forward

type str_cc from structure
	string		s_item_type_code
	string		s_distribution_method_code
	string		s_related_file_name
	string		s_recipient_type_code
	long		l_recipient_no
	datetime		dt_date_on_document
	string		s_file_name
end type

type str_attach from structure
	string		s_item_type_code
	string		s_distribution_method_code
	string		s_related_file_name
	string		s_recipient_type_code
	long		l_recipient_no
	datetime		dt_date_on_document
	string		s_file_name
end type

type str_orig from structure
	string		s_item_type_code
	string		s_distribution_method_code
	string		s_recipient_type_code
	long		l_recipient_no
	datetime		dt_date_on_document
	string		s_file_name
end type

global type n_remote_print from uo_nvo_base
end type
global n_remote_print n_remote_print

type variables
PRIVATE:
LONG		il_claim_no
STRING		is_mail_status_code
STRING		is_package_type_code
STRING		is_drive

STRING		is_document_type_code
DATETIME	idt_date_on_document
STRING		is_index_comment
STRING		is_language_code

STR_CC		istr_cc[]
STR_ATTACH	istr_attach[]
STR_ORIG	istr_orig[]



end variables

forward prototypes
public function integer nf_init ()
public function integer nf_eval_all ()
public function integer nf_set_print_package (long l_claim_no, string s_mail_status_code, string s_package_type_code, string s_document_type_code, string s_index_comment, string s_language_code)
public function integer nf_set_print_item (string s_item_type_code, string s_related_file_name, string s_distribution_method_code, string s_recipient_type_code, long l_recipient_no, datetime dt_date_on_document, string s_file_name)
end prototypes

public function integer nf_init ();/**********************************************************
		Description :
			Initializes the temp. memory storage locations.		Precondition :
		Paramaters :
			None.
		Postcondition :
		Returns :
			-  0 - Success
			- -1 - Error
**********************************************************/
INT					iReturn = 0
INT					iCounter



iReturn = SetNull(is_package_type_code)
iReturn = SetNull(is_mail_status_code)
iReturn = SetNull(is_document_type_code)
iReturn = SetNull(is_language_code)
iReturn = SetNull(is_index_comment)
iReturn = SetNull(il_claim_no)

FOR iCounter = 1 to UpperBound(istr_orig[])
	iReturn = SetNull(istr_orig[iCounter].s_item_type_code)
	iReturn = SetNull(istr_orig[iCounter].s_distribution_method_code)
	iReturn = SetNull(istr_orig[iCounter].s_recipient_type_code)
	iReturn = SetNull(istr_orig[iCounter].l_recipient_no)
	iReturn = SetNull(istr_orig[iCounter].s_file_name)
	iReturn = SetNull(istr_orig[iCounter].dt_date_on_document)
NEXT

FOR iCounter = 1 to UpperBound(istr_cc[])
	iReturn = SetNull(istr_cc[iCounter].s_item_type_code)
	iReturn = SetNull(istr_cc[iCounter].s_distribution_method_code)
	iReturn = SetNull(istr_cc[iCounter].s_recipient_type_code)
	iReturn = SetNull(istr_cc[iCounter].l_recipient_no)
	iReturn = SetNull(istr_cc[iCounter].s_file_name)
	iReturn = SetNull(istr_cc[iCounter].s_related_file_name)
	iReturn = SetNull(istr_cc[iCounter].dt_date_on_document)
NEXT

FOR iCounter = 1 to UpperBound(istr_attach[])
	iReturn = SetNull(istr_attach[iCounter].s_item_type_code)
	iReturn = SetNull(istr_attach[iCounter].s_distribution_method_code)
	iReturn = SetNull(istr_attach[iCounter].s_related_file_name)
	iReturn = SetNull(istr_attach[iCounter].s_recipient_type_code)
	iReturn = SetNull(istr_attach[iCounter].l_recipient_no)
	iReturn = SetNull(istr_attach[iCounter].s_file_name)
	iReturn = SetNull(istr_attach[iCounter].dt_date_on_document)
NEXT

IF iReturn = 1 THEN iReturn = 0

RETURN iReturn

/**********************************************************
Revision History:
Developer		Ian L. Ross
Date				1996.09.09
Comment			Version 1.0 complete
**********************************************************/


end function

public function integer nf_eval_all ();/**********************************************************
		Description :
			Evaluates the user object in this case writes the data 
			in the approate data structures.
		Precondition :
		Paramaters :
			None.
		Postcondition :
		Returns :
			-  0 - Success
			- -1 - Error
**********************************************************/

INT					iReturn = 0
INT					iStatus
INT					iCounter
INT					iTot_Print_Items

LONG					lPackage_no
LONG					lItem_no
LONG					lRelated_item_no
LONG					lRelateItem = 1
LONG					lCounter
LONG					lTotalPages
LONG					lPageCnt

STRING				srelation[500,2]
STRING				sCheckName
STRING				sCheckString
STRING				sFile

Boolean				bMail_Package_Entry_Only = False


SQLCA.nf_begin_transaction()


// Retrieve the last package number.
UPDATE Last_Package_No SET last_package_no = last_package_no + 1 using SQLCA;
SQLCA.nf_handle_error("Last_Package_NO","n_remote_print","")


SELECT last_package_no INTO :lPackage_No FROM Last_Package_No using SQLCA;
SQLCA.nf_handle_error("Last_Package_NO","n_remote_print","")

IF iReturn <> -1 THEN

// 	Add in the MAIL_PACKAGE record
		INSERT INTO MAIL_PACKAGE (
			package_no,
			claim_no,
			mail_status_code,
			package_type_code,
			document_type_code,
			index_comment,
			language_code)
		VALUES (
			:lPackage_no,	
			:il_claim_no,
			:is_mail_status_code,
			:is_package_type_code,
			:is_document_type_code,
			:is_index_comment,
			:is_language_code)
		USING SQLCA;
END IF

SQLCA.nf_handle_error("Embedded SQL INSERT on MAIL_PACKAGE ","n_remote_print","")


// Check to see if this is an entry into the MAIL_PACKAGE table only
IF isNull(istr_orig[1].s_item_type_code) Then
	bMail_Package_Entry_Only = TRUE
END IF

IF (iReturn <> -1) AND NOT bMail_Package_Entry_Only THEN 

	// Get the next available item number
	// First determine how many records will be inserted ino PRINT_ITEM Table
	// It will be Total CC's + Attachment's + 1 Orignal

	// Calc total Orig's
	FOR iCounter = 1 TO UpperBound(istr_orig[])
		IF NOT isNull(istr_orig[iCounter].s_file_name) THEN
			iTot_Print_Items++
		END IF
	NEXT
	// Calc total CC's
	FOR iCounter = 1 TO UpperBound(istr_cc[])
		IF NOT isNull(istr_cc[iCounter].s_file_name) THEN
			iTot_Print_Items++
		END IF
	NEXT
	// Calc total Attachment's
	FOR iCounter = 1 TO UpperBound(istr_attach[])
		IF NOT isNull(istr_Attach[iCounter].s_file_name) THEN
			iTot_Print_Items++
		END IF
	NEXT

	// Retrieve the last item number and set the next one.
	UPDATE Last_Item_No SET last_item_no = (last_item_no + :iTot_Print_Items) using SQLCA;
	SQLCA.nf_handle_error("Last_Item_NO","n_remote_print","")
	
	SELECT last_item_no INTO :lItem_No FROM Last_Item_No using SQLCA;
	SQLCA.nf_handle_error("Last_Item_NO","n_remote_print","")
	
	lItem_No = lItem_No - iTot_Print_Items
	
	
		// Insert the Ogiginal Record
		FOR iCounter = 1 to UpperBound(istr_Orig[])
			IF NOT isNull(istr_Orig[iCounter].s_distribution_method_code) THEN
				IF iReturn <> -1 THEN       // Not an error
					INSERT INTO PRINT_ITEM (
						package_no,
						item_no,
						item_type_code,
						related_item_no,
						distribution_method_code,
						recipient_type_code,
						recipient_no,
						date_on_document,
						file_name)
					VALUES (			
						:lpackage_no,
						:litem_no + 1,
						:istr_Orig[iCounter].s_item_type_code,
						:lItem_no + 1,
						:istr_Orig[iCounter].s_distribution_method_code,
						:istr_Orig[iCounter].s_recipient_type_code,
						:istr_Orig[iCounter].l_recipient_no,
						:istr_Orig[iCounter].dt_date_on_document,
						:istr_Orig[iCounter].s_file_name)
					USING SQLCA;
					// increment the lItem_no counter
					lItem_no++
				END IF
				// Add this record into the relationship array
				sRelation[lRelateItem,1] = istr_Orig[iCounter].s_file_name
				sRelation[lRelateItem,2] = STRING(lItem_no)
				lRelateItem++
			END IF
			SQLCA.nf_handle_error("Embedded SQL INSERT on PRINT_ITEM ","n_remote_print","")
			
		NEXT
	
END IF

IF (iReturn <> -1) AND NOT bMail_Package_Entry_Only THEN 
	// Insert the Carbon Copy, (CC), Record(s)
	FOR iCounter = 1 to UpperBound(istr_cc[])
		IF NOT isNull(istr_cc[iCounter].s_distribution_method_code) THEN
			IF iReturn <> -1 THEN       // Not an error
					INSERT INTO PRINT_ITEM (
						package_no,
						item_no,
						item_type_code,
						related_item_no,
						distribution_method_code,
						recipient_type_code,
						recipient_no,
						date_on_document,
						file_name)
					VALUES (			
						:lpackage_no,
						:litem_no + 1,
						:istr_cc[iCounter].s_item_type_code,
						:litem_no + 1,
						:istr_cc[iCounter].s_distribution_method_code,
						:istr_cc[iCounter].s_recipient_type_code,
						:istr_cc[iCounter].l_recipient_no,
						:istr_cc[iCounter].dt_date_on_document,
						:istr_cc[iCounter].s_file_name)
					USING SQLCA;
					// increment the lItem_no counter
					lItem_no++
			END IF
			// Add this record into the relationship array
			sRelation[lRelateItem,1] = istr_cc[iCounter].s_file_name
			sRelation[lRelateItem,2] = STRING(lItem_no)
			lRelateItem++
		END IF

		SQLCA.nf_handle_error("Embedded SQL INSERT on PRINT_ITEM ","n_remote_print","")

	NEXT
END IF

IF (iReturn <> -1) AND NOT bMail_Package_Entry_Only THEN 
	// Insert the Attachments Record(s)
	FOR iCounter = 1 to UpperBound(istr_Attach[])
		IF NOT isNull(istr_Attach[iCounter].s_distribution_method_code) THEN
			IF iReturn <> -1 THEN       // Not an error
				// Get the related item Key value from the sRelatin array
				SetNull(lrelated_item_no)
				FOR lCounter = 1 to UpperBound(sRelation,1)
					sCheckName = istr_Attach[iCounter].s_related_file_name
					IF sRelation[lCounter,1] = sCheckName THEN
						lrelated_item_no = Long(sRelation[lCounter,2])
						EXIT
					END IF
					IF isNull(sRelation[lCounter,1]) THEN EXIT
					IF (sRelation[lCounter,1] = "" ) THEN EXIT
				NEXT

				IF NOT isNull(lrelated_item_no) THEN
					INSERT INTO PRINT_ITEM (
						package_no,
						item_no,
						item_type_code,
						related_item_no,
						distribution_method_code,
						recipient_type_code,
						recipient_no,
						date_on_document,
						file_name)
					VALUES (			
						:lpackage_no,
						:litem_no + 1,
						:istr_attach[iCounter].s_item_type_code,
						:lrelated_item_no,
						:istr_attach[iCounter].s_distribution_method_code,
						:istr_attach[iCounter].s_recipient_type_code,
						:istr_attach[iCounter].l_recipient_no,
						:istr_attach[iCounter].dt_date_on_document,
						:istr_attach[iCounter].s_file_name)
					USING SQLCA;
					// increment the lItem_no counter
					lItem_no++
				ELSE
					MessageBox('Data Validation Problem','Records not written, contact production support')
					iReturn = -1
					SQLCA.nf_rollback_transaction()
					EXIT
				END IF

				SQLCA.nf_handle_error("Embedded SQL INSERT on PRINT_ITEM ","n_remote_print","")

			END IF
		END IF
	NEXT
END IF

IF iReturn = 0 THEN
	SQLCA.nf_commit_transaction()
ELSE
	SQLCA.nf_rollback_transaction()
END IF


RETURN iReturn


/**********************************************************
Revision History:
Developer		Ian L. Ross
Date				1997.03.19
Comment			Version 1.1 complete
**********************************************************/


end function

public function integer nf_set_print_package (long l_claim_no, string s_mail_status_code, string s_package_type_code, string s_document_type_code, string s_index_comment, string s_language_code);/**********************************************************
		Description :
			Loads the passed parameters for MAIL_PACKAGE elements 
			into instance varibles to be written down during the 
			nf_Eval() Function.
		Precondition :
		Paramaters :
			l_claim_no 				: 
			s_mail_status_code	: 
			s_package_type_code	: 
			s_document_type_code	: 
			s_index_comment		: 
			s_language_code		: 

		Postcondition :
		Returns :
			-  0 - Success
			- -1 - Error
**********************************************************/
INT					iReturn = 0
INT					iStatus

STRING				sDesc1
STRING				sDesc2

//Validate the passed in data

// Make sure that the claim_no is populated
IF isNull(l_claim_no) THEN iReturn = -1

IF iReturn <> -1 THEN

	SELECT	a.package_type_desc,
				b.mail_status_desc
	INTO		:sDesc1,
				:sDesc2
	FROM 		Package_Type a,
				Mail_Status b
	WHERE		(a.package_type_code=:s_package_type_code)
	AND		(b.mail_status_code=:s_mail_status_code);

	iStatus = SQLCA.SQLCODE
	IF iStatus < 0 OR iStatus = 100	THEN iReturn 			= -1
	IF iStatus = 100 						THEN SQLCA.SQLCODE 	= -1
	SQLCA.nf_set_display_mess_flag(True)
	iReturn = SQLCA.nf_handle_error("Embedded SQL SELECT on Codes Tables","in nf_set_print_package","in User Object n_remote_print")
End If

IF iReturn <> -1 THEN
	il_claim_no					= l_claim_no 
	is_mail_status_code 		= s_mail_status_code
	is_package_type_code 	= s_package_type_code
	is_document_type_code 	= s_document_type_code
	is_index_comment 			= s_index_comment
	is_language_code 			= s_language_code
END IF

RETURN iReturn
/**********************************************************
Revision History:
Developer		Ian L. Ross
Date				1996.09.09
Comment			Version 1.0 complete
**********************************************************/


end function

public function integer nf_set_print_item (string s_item_type_code, string s_related_file_name, string s_distribution_method_code, string s_recipient_type_code, long l_recipient_no, datetime dt_date_on_document, string s_file_name);/**********************************************************
		Description :
			Loads the main mail print package information ino instace 
			varibles to be written down during the nf_Eval() Function.
		Precondition :
		Paramaters :
			s_item_type_code 
			s_related_file_name
			s_distribution_method_code
			s_recipient_type_code
			l_recipient_no
			dt_date_on_document
			s_file_name
		Postcondition :
		Returns :
			-  0 - Success
			- -1 - Error
**********************************************************/
INT					iReturn = 0
INT					iNextValue
INT					iFile_Handle
INT					iStatus

LONG					iCounter

STRING				s_File
STRING				sDesc

Boolean				bDID_File
Boolean				bFound

// See if the file being passed exists
//s_File	= "\\" + TRIM(s_recipient_type_code) + "\" + TRIM(l_recipient_no) + "\" + TRIM(s_file_name)
//s_file = TRIM(l_recipient_no) + TRIM(s_file_name)
s_file = TRIM(s_file_name)

// See if the file name is document ID
IF Pos(Upper(s_file_name), ".DID") > 0 THEN bDID_File = TRUE

// Allow the entry ONLY if the file exists or it is a document Id file.
IF FileExists(s_File) OR (bDID_File) THEN  // The file exists, store it into the proper memory structures...
	// Make sure a valid distribution code is being passed
	SELECT distribution_method_desc INTO :sDesc FROM Distribution_Method WHERE (distribution_method_code=:s_distribution_method_code);

	SQLCA.nf_set_display_mess_flag(True)
	iReturn = SQLCA.nf_handle_error("Embedded SQL SELECT on Distribution_Method","in nf_set_print_item","in User Object n_remote_print")
	iStatus = SQLCA.SQLCODE

	IF (iStatus = 0) THEN
		Choose Case s_item_type_code
			Case "O"
			// Find out the next value for the record to be inserted into the structure
			bFound = False
			For iCounter = 1 to UpperBound(istr_orig[])
				If isNull(istr_orig[iCounter].s_item_type_code) Then
					iNextValue = iCounter
					bFound = True
					Exit
				END IF
			NEXT
			If not bFound THEN
				iNextValue = UpperBound(istr_orig[])+1 
			End If

			istr_orig[inextValue].s_item_type_code					= s_item_type_code
			istr_orig[inextValue].s_file_name						= s_file_name
			istr_orig[inextValue].s_recipient_type_code			= s_recipient_type_code
			istr_orig[inextValue].l_recipient_no					= l_recipient_no
			istr_orig[inextValue].s_distribution_method_code	= s_distribution_method_code
			istr_orig[inextValue].dt_date_on_document				= dt_date_on_document

			Case "C"
			// Find out the next value for the record to be inserted into the structure
			bFound = False
			For iCounter = 1 to UpperBound(istr_cc[])
				If isNull(istr_cc[iCounter].s_item_type_code) Then
					iNextValue = iCounter
					bFound = True
					Exit
				END IF
			NEXT
			If not bFound THEN
				iNextValue = UpperBound(istr_cc[])+1 
			End If
			istr_cc[inextValue].s_item_type_code				= s_item_type_code
			istr_cc[inextValue].s_file_name						= s_file_name
			istr_cc[inextValue].s_recipient_type_code			= s_recipient_type_code
			istr_cc[inextValue].l_recipient_no					= l_recipient_no
			istr_cc[inextValue].s_distribution_method_code	= s_distribution_method_code
			istr_cc[inextValue].s_related_file_name			= s_related_file_name
			istr_cc[inextValue].dt_date_on_document			= dt_date_on_document
			Case "A"
			bFound = False
			For iCounter = 1 to UpperBound(istr_attach[])
				If isNull(istr_attach[iCounter].s_item_type_code) Then
					iNextValue = iCounter
					bFound = True
					Exit
				END IF
			NEXT
			If not bFound THEN
				iNextValue = UpperBound(istr_attach[])+1 
			End If
			istr_attach[inextValue].s_item_type_code				= s_item_type_code
			istr_attach[inextValue].s_file_name						= s_file_name
			istr_attach[inextValue].s_recipient_type_code		= s_recipient_type_code
			istr_attach[inextValue].l_recipient_no					= l_recipient_no
			istr_attach[inextValue].s_distribution_method_code	= s_distribution_method_code
			istr_attach[inextValue].s_related_file_name			= s_related_file_name
			istr_attach[inextValue].dt_date_on_document			= dt_date_on_document
			Case Else
				iReturn = -1
		End Choose
	ELSE
		iReturn = -1
	END IF
ELSE
	iReturn = -1
END IF

RETURN iReturn
/**********************************************************
Revision History:
Developer		Ian L. Ross
Date				1996.09.09
Comment			Version 1.0 complete
**********************************************************/

end function

on constructor;call uo_nvo_base::constructor;//is_drive = profilestring(vgs_ini_filename,"CORRESPONDENCE","Signature"," ")
end on

on n_remote_print.create
call super::create
end on

on n_remote_print.destroy
call super::destroy
end on

