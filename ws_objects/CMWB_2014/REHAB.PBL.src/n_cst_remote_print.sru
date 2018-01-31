$PBExportHeader$n_cst_remote_print.sru
$PBExportComments$Remote Print System (RPS) Object - Writes Structures to tables.
forward
global type n_cst_remote_print from nonvisualobject
end type
type str_attach from structure within n_cst_remote_print
end type
type str_cc from structure within n_cst_remote_print
end type
type str_orig from structure within n_cst_remote_print
end type
end forward

type str_attach from structure
	string		s_item_type_code
	string		s_distribution_method_code
	string		s_related_file_name
	string		s_recipient_type_code
	long		l_recipient_no
	string		s_file_name
end type

type str_cc from structure
	string		s_item_type_code
	string		s_distribution_method_code
	string		s_related_file_name
	string		s_recipient_type_code
	long		l_recipient_no
	string		s_file_name
end type

type str_orig from structure
	string		s_item_type_code
	string		s_distribution_method_code
	string		s_recipient_type_code
	long		l_recipient_no
	string		s_file_name
end type

global type n_cst_remote_print from nonvisualobject
end type
global n_cst_remote_print n_cst_remote_print

type variables
PRIVATE:
LONG		il_claim_no
STRING		is_mail_status_code
STRING		is_package_type_code
STRING		is_drive

STRING		is_document_type_code
STRING		is_index_comment
STRING		is_language_code

STR_CC		istr_cc[]
STR_ATTACH	istr_attach[]
STR_ORIG	istr_orig[]



end variables

forward prototypes
public function integer of_init ()
public function integer of_insert_into_mail_package_table ()
public function integer of_set_mail_package_values (long al_claim_no, string as_mail_status_code, string as_package_type_code, string as_document_type_code, string as_index_comment, string as_language_code)
end prototypes

public function integer of_init ();/**********************************************************
		Description :
			Initializes the temp. memory storage locations.		Precondition :
		Paramaters :
			None.
		Postcondition :
		Returns :
			-  1 - Success
			- -1 - Error
**********************************************************/
Int					li_Return = 1

Long					ll_Counter



IF li_Return = 1 Then li_return = SetNull(is_package_type_code)
IF li_Return = 1 Then li_return = SetNull(is_mail_status_code)
IF li_Return = 1 Then li_return = SetNull(is_document_type_code)
IF li_Return = 1 Then li_return = SetNull(is_language_code)
IF li_Return = 1 Then li_return = SetNull(is_index_comment)
IF li_Return = 1 Then li_return = SetNull(il_claim_no)

FOR ll_counter = 1 to UpperBound(istr_orig[])
	IF li_Return = 1 Then li_return = SetNull(istr_orig[ll_counter].s_item_type_code)
	IF li_Return = 1 Then li_return = SetNull(istr_orig[ll_counter].s_distribution_method_code)
	IF li_Return = 1 Then li_return = SetNull(istr_orig[ll_counter].s_recipient_type_code)
	IF li_Return = 1 Then li_return = SetNull(istr_orig[ll_counter].l_recipient_no)
	IF li_Return = 1 Then li_return = SetNull(istr_orig[ll_counter].s_file_name)
NEXT

FOR ll_counter = 1 to UpperBound(istr_cc[])
	IF li_Return = 1 Then li_return = SetNull(istr_cc[ll_counter].s_item_type_code)
	IF li_Return = 1 Then li_return = SetNull(istr_cc[ll_counter].s_distribution_method_code)
	IF li_Return = 1 Then li_return = SetNull(istr_cc[ll_counter].s_recipient_type_code)
	IF li_Return = 1 Then li_return = SetNull(istr_cc[ll_counter].l_recipient_no)
	IF li_Return = 1 Then li_return = SetNull(istr_cc[ll_counter].s_file_name)
	IF li_Return = 1 Then li_return = SetNull(istr_cc[ll_counter].s_related_file_name)
NEXT

FOR ll_counter = 1 to UpperBound(istr_attach[])
	IF li_Return = 1 Then li_return = SetNull(istr_attach[ll_counter].s_item_type_code)
	IF li_Return = 1 Then li_return = SetNull(istr_attach[ll_counter].s_distribution_method_code)
	IF li_Return = 1 Then li_return = SetNull(istr_attach[ll_counter].s_related_file_name)
	IF li_Return = 1 Then li_return = SetNull(istr_attach[ll_counter].s_recipient_type_code)
	IF li_Return = 1 Then li_return = SetNull(istr_attach[ll_counter].l_recipient_no)
	IF li_Return = 1 Then li_return = SetNull(istr_attach[ll_counter].s_file_name)
NEXT


RETURN li_Return

/**********************************************************
Revision History:
Developer		Ian L. Ross
Date				1998.03.03
Comment			Version 1.0 complete
**********************************************************/


end function

public function integer of_insert_into_mail_package_table ();/**********************************************************
		Description :
			Evaluates the user object in this case writes the data 
			in the approate data structures.
		Precondition :
		Paramaters :
			None.
		Postcondition :
		Returns :
			-  1 - Success
			- -1 - Error
**********************************************************/

Int					li_Return = 1
Int					li_rc

LONG					ll_Package_no

// Retrieve the last package number.
UPDATE 	Last_Package_No 
SET 		last_package_no = last_package_no + 1 
using SQLCA;

SQLCA.nf_set_display_mess_flag(True)
li_rc = SQLCA.nf_handle_error("Update on Last_package_No","Code In :" + This.ClassName(), "In Object : n_cst_remote_print")

If li_rc <> 0 Then li_return = -1

IF li_Return = 1 THEN
	SELECT last_package_no INTO :ll_Package_No FROM Last_Package_No using SQLCA;
	SQLCA.nf_set_display_mess_flag(True)

	li_rc = SQLCA.nf_handle_error("Select on Last_package_No","Code In :" + This.ClassName(), "In Object : n_cst_remote_print")
	If (li_rc <> 0) OR (SQLCA.SQLCODE = 100) Then li_return = -1
END IF

IF li_return = 1 THEN

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
			:ll_Package_no,	
			:il_claim_no,
			:is_mail_status_code,
			:is_package_type_code,
			:is_document_type_code,
			:is_index_comment,
			:is_language_code)
		USING SQLCA;
		SQLCA.nf_set_display_mess_flag(True)
		li_rc = SQLCA.nf_handle_error("Embedded SQL INSERT on MAIL_PACKAGE Table","Code In :" + This.ClassName(), "In Object : n_cst_remote_print")
		If li_rc <> 0 Then
			li_return = -1
			SQLCA.nf_rollback_transaction()
		END IF
			
END IF


RETURN li_return

/**********************************************************
Revision History:
Developer		Ian L. Ross
Date				1998.03.03
Comment			Version 1.0 complete
**********************************************************/

end function

public function integer of_set_mail_package_values (long al_claim_no, string as_mail_status_code, string as_package_type_code, string as_document_type_code, string as_index_comment, string as_language_code);/**********************************************************
		Description :
			Loads the passed parameters for MAIL_PACKAGE elements 
			into instance varibles to be written down during the 
			nf_Eval() Function.
		Precondition :
		Paramaters :
			al_claim_no 		   	: 
			as_mail_status_code		: 
			as_package_type_code		: 
			as_document_type_code	: 
			as_index_comment			: 
			as_language_code			: 

		Postcondition :
		Returns :
			-  1 - Success
			- -1 - Error
**********************************************************/
Int					li_Return = 1
Int					li_rc

Long					ll_Status

STRING				sDesc1
STRING				sDesc2
STRING				sDesc3
STRING				sDesc4
STRING				sDesc5

//Validate the passed in data

// Make sure that the context_no is populated
IF isNull(al_claim_no) THEN li_Return = -1

IF li_Return = 1 THEN

	SELECT	a.package_type_desc,
				b.mail_status_desc
	INTO		:sDesc1,
				:sDesc2
	FROM 		Package_Type a,
				Mail_Status b
	WHERE		(a.package_type_code=:as_package_type_code)
	AND		(b.mail_status_code=:as_mail_status_code);

	ll_Status = SQLCA.SQLCODE
	IF (ll_Status < 0) OR (ll_Status = 100)	THEN li_Return 		= -1
	IF ll_Status = 100 								THEN SQLCA.SQLCODE 	= -1
	SQLCA.nf_set_display_mess_flag(True)
	li_rc = SQLCA.nf_handle_error("Embedded SQL SELECT on Codes Tables","Code In :" + This.ClassName(), "In Object : n_cst_remote_print")

End If

IF li_Return = 1 THEN
	il_claim_no					= al_claim_no 
	is_mail_status_code 		= as_mail_status_code
	is_package_type_code 	= as_package_type_code
	is_document_type_code 	= as_document_type_code
	is_index_comment 			= as_index_comment
	is_language_code 			= as_language_code
END IF

RETURN li_Return
/**********************************************************
Revision History:
Developer		Ian L. Ross
Date				1998.03.03
Comment			Version 1.0 complete
**********************************************************/


end function

on n_cst_remote_print.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_cst_remote_print.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

