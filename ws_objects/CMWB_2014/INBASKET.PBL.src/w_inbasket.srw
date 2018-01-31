$PBExportHeader$w_inbasket.srw
$PBExportComments$Inbasket Module
forward
global type w_inbasket from w_a_tool
end type
type dw_account_payment_list from u_dw_online within w_inbasket
end type
type cb_inbasket_doc_reject_acct from commandbutton within w_inbasket
end type
type dw_account_payment_paid_status from u_dw_online within w_inbasket
end type
type dw_claimsmaster_ref_count from u_dw_online within w_inbasket
end type
type cb_inbasket_doc_pay_acct from commandbutton within w_inbasket
end type
type cb_inbasket_refresh from commandbutton within w_inbasket
end type
type cb_inbasket_doc_delete from commandbutton within w_inbasket
end type
type cb_inbasket_doc_view from commandbutton within w_inbasket
end type
type cb_inbasket_doc_index from commandbutton within w_inbasket
end type
type dw_inbasket_category_select from u_dw_online within w_inbasket
end type
type cb_inbasket_folder_delete from commandbutton within w_inbasket
end type
type st_inbasket_forward_to from statictext within w_inbasket
end type
type dw_inbasket_forward_to from u_dw_online within w_inbasket
end type
type cb_inbasket_folder_index_ok from commandbutton within w_inbasket
end type
type cb_inbasket_folder_index_cancel from commandbutton within w_inbasket
end type
type dw_display_claim_info from u_dw_online within w_inbasket
end type
type gb_inbasket_category_selection from groupbox within w_inbasket
end type
type dw_inbasket_folder_index from u_dw_online within w_inbasket
end type
type cb_bfclear from commandbutton within w_inbasket
end type
type uo_reference_search from u_reference_search within w_inbasket
end type
type cb_inbasket_folder_unpaid_accts from commandbutton within w_inbasket
end type
type cb_inbasket_doc_reviewed from commandbutton within w_inbasket
end type
type uo_image_append from u_image_append within w_inbasket
end type
type cbx_claim_view from checkbox within w_inbasket
end type
type cb_event from commandbutton within w_inbasket
end type
type dw_inbasket from u_dw_online within w_inbasket
end type
type ln_height_sizer from line within w_inbasket
end type
type ln_width_sizer from line within w_inbasket
end type
type s_inbasket_event from structure within w_inbasket
end type
end forward

type s_inbasket_event from structure
	long		claim_no
	long		individual_no
	string		event_category_code
	long		event_no
end type

global type w_inbasket from w_a_tool
integer width = 6624
integer height = 1844
boolean resizable = false
event ue_enable_inbasket_menu pbm_custom02
dw_account_payment_list dw_account_payment_list
cb_inbasket_doc_reject_acct cb_inbasket_doc_reject_acct
dw_account_payment_paid_status dw_account_payment_paid_status
dw_claimsmaster_ref_count dw_claimsmaster_ref_count
cb_inbasket_doc_pay_acct cb_inbasket_doc_pay_acct
cb_inbasket_refresh cb_inbasket_refresh
cb_inbasket_doc_delete cb_inbasket_doc_delete
cb_inbasket_doc_view cb_inbasket_doc_view
cb_inbasket_doc_index cb_inbasket_doc_index
dw_inbasket_category_select dw_inbasket_category_select
cb_inbasket_folder_delete cb_inbasket_folder_delete
st_inbasket_forward_to st_inbasket_forward_to
dw_inbasket_forward_to dw_inbasket_forward_to
cb_inbasket_folder_index_ok cb_inbasket_folder_index_ok
cb_inbasket_folder_index_cancel cb_inbasket_folder_index_cancel
dw_display_claim_info dw_display_claim_info
gb_inbasket_category_selection gb_inbasket_category_selection
dw_inbasket_folder_index dw_inbasket_folder_index
cb_bfclear cb_bfclear
uo_reference_search uo_reference_search
cb_inbasket_folder_unpaid_accts cb_inbasket_folder_unpaid_accts
cb_inbasket_doc_reviewed cb_inbasket_doc_reviewed
uo_image_append uo_image_append
cbx_claim_view cbx_claim_view
cb_event cb_event
dw_inbasket dw_inbasket
ln_height_sizer ln_height_sizer
ln_width_sizer ln_width_sizer
end type
global w_inbasket w_inbasket

type variables
boolean		vib_inbasket_folder_multi_claim
boolean 		ib_imaged_view
datetime                idtm_date_on_document
long		vil_inbasket_filter_cnt
long                      il_docid
long                      il_service_provider_no
long                      il_claim_no
long                      il_selected_claim_no
LONG                      il_claim_view_filter_no
string                    is_document_type
string                    is_claim_status
string                    is_claim_status_type
int		il_category_id_nmbr
DataWindowChild   vidw_category_select_child
DataWindowChild   vidw_forward_to_child
DataWindowChild   vidw_action_child
Long 		vil_folder_list_anchor_row
n_imaging              inv_imaging
w_sheet		iw_sheet
s_window_message	istr_window_message
string                    is_row_filter
long                      il_days_after
string                    is_filter_choice
N_PAYMENT 	inv_payment
w_account_payment_maintenance             iw_account_payment_maintenance
w_inbasket            iwi_parent
w_account_payment  iw_account_payment
n_check_dw inv_check_dw


BOOLEAN   ib_opening
STRING    is_opening_method

integer			ii_oldsheetwidth
INTEGER        ii_oldsheetx
integer			ii_oldsheetheight
INTEGER        ii_oldsheety
integer			ii_frameheight
integer			ii_framewidth
integer			ii_framex
integer			ii_framey
BOOLEAN        ib_explodewasgood

LONG           il_catid[]

PRIVATE s_inbasket_event    istr_inbasket_event, istr_null
end variables

forward prototypes
public subroutine wf_inbasket_get_doc_info ()
public function integer wf_retrieve_claim (long al_claim_no)
public function integer wf_set_claim (long al_claim_no)
public function integer wf_validate_acct_doc ()
private subroutine wf_inbasket_clear_workbench ()
private function integer wf_inbasket_display_folder_index (long al_fldid)
private function integer wf_inbasket_forward_claim (long al_catid, long al_fldid)
private function long wf_inbasket_get_selected_fldid ()
private function integer wf_inbasket_process_folder_index (long al_fldid)
private function integer wf_inbasket_process_folder_name (long val_fldid)
public subroutine wf_inbasket_unpaid_accounts ()
public function integer wf_inbasket_validate_for_unpaid ()
public subroutine wf_inbasket_set_menu_status (integer ai_menu_status)
public function integer wf_check_doc_index ()
private function integer wf_inbasket_refresh_folder_list ()
public function integer wf_validate_imaged ()
public function string wf_validate_list_data ()
public function integer wf_validate_nonimaged ()
public subroutine wf_inbasket_account_pay ()
private function integer wf_inbasket_setup (string vas_inbasket_state)
private subroutine wf_inbasket_display_folder_details ()
private subroutine wf_inbasket_display_document_index ()
public function integer wf_explode_frame_and_set_sheets ()
public subroutine wf_reset_screen_sizes ()
public function integer wf_verify_event ()
public subroutine wf_determine_cat_array ()
public subroutine wf_post_select_folder (long al_fldid)
public subroutine wf_event_button_visibility (long al_current_row)
end prototypes

on ue_enable_inbasket_menu;call w_a_tool::ue_enable_inbasket_menu;/* Enable the InBasket Menu Item
*/
wf_inbasket_set_menu_status(0)
end on

public subroutine wf_inbasket_get_doc_info ();LONG     ll_rownum, ll_docid,	ll_claim_no, ll_rowcount, ll_cntr, ll_account_payment_list_rownum
STRING   ls_folder_list
INTEGER  li_return_value
BOOLEAN  lb_payment_allowed = True,  lb_link_allowed = True


	SetPointer(HourGlass!)

/* Get the row number of the document selected from the document list
*/
	ll_rownum = dw_inbasket.GetRow()

/*	Get the document Identifier, document type, service provier and document date of 
	the selected document
*/
	il_docid               = dw_inbasket.GetItemNumber(ll_rownum,'docid')
	IF il_docid = 0 THEN
		MessageBox('InBasket','Please select a folder that has a document.')
		RETURN
	END IF
	is_document_type       = dw_inbasket.GetItemString(ll_rownum, 'type_code')
	il_service_provider_no = dw_inbasket.GetItemNumber(ll_rownum,'service_provider_no')
	idtm_date_on_document  = dw_inbasket.GetItemDateTime(ll_rownum,'date_on_document')


/*	Retrieve claim information on the document.
*/
	dw_display_claim_info.Reset()
	dw_display_claim_info.InsertRow(0)

	il_claim_no = dw_inbasket.GetItemNumber(ll_rownum,'claim_no')
	IF IsNull(il_claim_no) THEN il_claim_no = 0

/* The user cannot select the Claim Master Category through the In-Basket: therefore, must
	always retrieve the claim information for this claim as the claim changes with the document selected
*/
	IF il_claim_no > 0 THEN
		li_return_value = wf_retrieve_claim(il_claim_no)
		IF li_return_value = 0 THEN
			MessageBox('InBasket','Error Validating Claim Number. Please verify indexing on this document',Exclamation!)
		ELSE
			is_claim_status 		= dw_display_claim_info.GetItemString(1,'claim_status_code') 
			is_claim_status_type = dw_display_claim_info.GetItemString(1,'claim_status_type_code') 
		END IF
	END IF



end subroutine

public function integer wf_retrieve_claim (long al_claim_no);STRING   ls_status
INTEGER	li_return_value

li_return_value = dw_display_claim_info.Retrieve(al_claim_no)
SQLCA.nf_handle_error('Retrieve of dw_display_claim_info','w_inbasket', 'wf_retrieve_claim')
IF li_return_value < 1 THEN Return li_return_value

iw_sheet.wf_set_claim(dw_display_claim_info.GetItemNumber(1,"claim_no"))

ls_status = dw_display_claim_info.GetItemString(1,'imaged_flag')
IF ls_status = "N" THEN
	ib_imaged_view = FALSE
ELSE
	ib_imaged_view = TRUE
END IF
	
Return 1
end function

public function integer wf_set_claim (long al_claim_no);	long 		ll_result

	ll_result = iw_sheet.wf_set_claim(al_claim_no)

	// Anything but 1 means the the claim can't be touched
	If ll_result <> 1 Then
		Return -1
	End If
	
	Return 1
end function

public function integer wf_validate_acct_doc ();LONG  	ll_rowcount, 	ll_cntr
STRING	ls_folder_list

/* Verify the document was indexed to a claim
*/
	IF il_claim_no = 0 THEN
		Messagebox("In-Basket","The document must be indexed to a claim", Exclamation!)
		Return -1
	END IF		

/* Verify the document was indexed
*/
	IF ISNULL (il_docid) THEN
		Messagebox("In-Basket","The document must be indexed before it can be paid/rejected", Exclamation!)
		Return -1
	END IF

/* Check the Document Type
*/
	IF is_document_type <> "SDC" AND is_document_type <> "SDD"  AND &
  		is_document_type <> "MPC" AND is_document_type <> "MPD" THEN
		Messagebox("In-Basket","Only document types SDC, SDD, MPC and MPD ~r~n" + &
		"can be paid/rejected at this time", Exclamation!)
		Return -1
	END IF

/* Verify the Claim Status is Finalled and not out-of-province (claim status type 05)
*/
	IF is_claim_status = 'F' AND &
		(is_claim_status_type <> '05') THEN
		// Finalled claims can be paid through In-Basket
	ELSE
		MessageBox("In-Basket","Claim status does not permit account payments/rejection from In-Basket",Exclamation!)
		Return -1
	END IF

//	Check to ensure data integrity of document (ie. It must exist in only one master folder (it's own))

	ll_rowcount = dw_claimsmaster_ref_count.Retrieve(il_docid)

	IF ll_rowcount <= 0 THEN
		MessageBox("In-Basket","This document does not exist in a master folder...Please call the helpdesk",Exclamation!)
		return -1
	ELSEIF ll_rowcount > 1 THEN
		ll_cntr = ll_rowcount
		DO WHILE ll_cntr <= ll_rowcount
			ls_folder_list = "Claim Number: " + string(dw_claimsmaster_ref_count.GetItemNumber(ll_cntr,"claimsmaster_claim")) + "~r~n"
			ll_cntr ++
		LOOP
		MessageBox("In-Basket","This document exists in the following master folders:~r~n" + &
						ls_folder_list + "Please fix before making payment",Exclamation!)
		return -1
	ELSE
		IF not dw_claimsmaster_ref_count.GetItemNumber(1,"claimsmaster_claim") = il_claim_no THEN
			MessageBox("In-Basket","This document is in the wrong master folder. ~r~n" + &
							"The claim number on the index form is " + string(il_claim_no) + "~r~n" + &
							"But it resides in the master folder for claim number " + string(dw_claimsmaster_ref_count.GetItemNumber(1,"claimsmaster_claim")) + "~r~n" +&
							"Please fix before making/rejecting payment",Exclamation!)
			return -1
		END IF
	END IF


/*	If there is Paid Status of type "O"(Paid via another system) or "R"(rejected) or "H" (hold) or
 	"S" (Scheduled), don't allow the user to make/reject the payment. Find all Payment_Documents for this
   document that have a Paid Status <> "P" (Paid).
*/
	ll_rowcount = dw_account_payment_paid_status.retrieve(il_docid)
	IF SQLCA.nf_handle_error("dw_account_payment_paid_status","In Basket","Retrieve Paid status for document") < 0 THEN
		MessageBox("In-Basket","Could not retrieve list of payments for this document. Please try again later",Exclamation!)
		return -1
	END IF
	IF ll_rowcount > 0 THEN
		Messagebox("In-Basket","Document cannot be paid/rejected as it has been either Paid via ~r~n" + &
 					"Another System, is Rejected or is Scheduled", Exclamation!)
		Return -1
	END IF


return 0
end function

private subroutine wf_inbasket_clear_workbench ();// Activate the clear button on the sheet
iw_sheet.wf_clear_worksheet()
end subroutine

private function integer wf_inbasket_display_folder_index (long al_fldid);//  This script sets up the display of the folder index.  If no index data
//  exists for the folder then a blank indexing form is prepared for
//  data entry.

// -------------------------
//  Declare local variables
// -------------------------
Long		ll_results,		ll_row_nmbr,	ll_index_fldid,	ll_rows_loaded, &
			ll_claim_no
Integer	li_error_code, li_inserted_row


ll_row_nmbr = dw_inbasket_folder_index.Retrieve(al_fldid)
ImageTrans.nf_handle_error("dw_inbasket_folder_index","w_inbasket","wf_inbasket_display_folder_index")

// Folder does not exist
If ll_row_nmbr = 0 AND al_fldid <> 0 Then

	MessageBox("In-Basket", "Some one has deleted the current work folder.  Please refresh the In-Basket.", INFORMATION!)
   
	// If all folders have been forwarded or deleted then no folders available
	IF vil_inbasket_filter_cnt = 0 THEN
		wf_inbasket_setup("FOLDERS_NOTFOUND")
	END IF

	Return -1
End If


// ---------------------------------------------------------------
//  If no entry is found then set up a blank folder indexing form
// ---------------------------------------------------------------

// Get folderid of index part of query
IF dw_inbasket_folder_index.GetRow() > 0 THEN
	ll_index_fldid = dw_inbasket_folder_index.GetItemNumber(1,"folderid")
	
	// Insert row into data window to create buffer to create workfolder entry
	IF IsNull(ll_index_fldid) THEN
	
		// Setup the defaults
		ll_claim_no = dw_inbasket.GetItemNumber(1,"claim_no")
		dw_inbasket_folder_index.SetItem(1,"claim_no",ll_claim_no)
	END IF
ELSE
	ll_claim_no = dw_inbasket.GetItemNumber(1,"claim_no")
	li_inserted_row = dw_inbasket_folder_index.InsertRow(0)
	dw_inbasket_folder_index.SetItem(li_inserted_row,"claim_no",ll_claim_no)
END IF

//  Set up category forward to list
// ---------------------------------
dw_inbasket_forward_to.Reset()
dw_inbasket_forward_to.InsertRow(0)

Return 1
end function

private function integer wf_inbasket_forward_claim (long al_catid, long al_fldid);//  This function moves the selected folder to another category
//
//  Input Parameter:  as_catid - Catgeory id of destination category
//                    as_fldid - Folder id of folder to be transferred
//
//  Return Values:    1 - Successful
//                   -1 - Failure
   
// Declare local varaibles
String  ls_folder_action,		ls_check_action
Integer li_error_code,			ll_results


ImageTrans.nf_begin_transaction()

// Change FLD entry to move the folder
UPDATE FLD
   SET fldcatid = :al_catid
 WHERE fldid = :al_fldid
 USING ImageTrans;

If ImageTrans.nf_handle_error("Embedded SQL: Update FLD","w_inbasket","wf_inbasket_forward_claim") < 0 Then
	Return -1
End If

// Change REF entries to move the folder and its documents
UPDATE REF
   SET doccatid = :al_catid
 WHERE docfldid = :al_fldid
 USING ImageTrans;

If ImageTrans.nf_handle_error("Embedded SQL: Update REF","w_inbasket","wf_inbasket_forward_claim") < 0 Then
	Return -1
END IF

// Wipe out the 'Bring Forward' information for the CLAIM_WORKING record for this folder as
// the user receiving this folder doesn't want that information.
UPDATE CLAIM_WORKING  
   SET bring_fwd_date = null,   
       bring_fwd_note = ""
 WHERE CLAIM_WORKING.folderid = :al_fldid
 USING ImageTrans;

If ImageTrans.nf_handle_error("Embedded SQL: Update CLAIM_WORKING","w_inbasket","wf_inbasket_forward_claim") < 0 Then
	Return -1
END IF

ImageTrans.nf_commit_transaction()


Return 1
end function

private function long wf_inbasket_get_selected_fldid ();//  This function returns the folder id of the selected folder in the folder
//  list window.
//  Return Values:    fldid of the selected folder

// Declare local variables
INTEGER  li_rowcount, li_row
LONG     ll_fldid


// Get id of selected row
li_row      = dw_inbasket.GetSelectedRow(0)
li_rowcount = dw_inbasket.RowCount()
IF li_row = 0 THEN
	IF li_rowcount > 0 THEN
		MessageBox('Get Selected Folder','Could not determine which folder is selected.  Please try selecting it again.')
		Return -1
	ELSE
		Return 0
	END IF
END IF

// Get value of folder id field on selected row
ll_fldid = dw_inbasket.GetItemNumber(li_row,'fldid')
IF ll_fldid <= 0 OR IsNull(ll_fldid) THEN
	MessageBox('Inbasket Folder Id','Error determining folder id of selected row in folder list.')
	Return -1
END IF


// End of function
RETURN ll_fldid
end function

private function integer wf_inbasket_process_folder_index (long al_fldid);//  This function processes changes to the folder indexing form

// Declare local variables
Long    		 ll_claim_no,				ll_check_claim
Integer		 li_error_code
String  		 ls_job_position_code,	ls_action,		ls_claimant_name
dwitemstatus ldwis_row_status

// Accept text in case user did not tab out of the field properly
If dw_inbasket_folder_index.AcceptText() = -1 Then
	dw_inbasket_folder_index.SetFocus()
	Return 0
End If

// Check the folder index form to see if it has been changed
ldwis_row_status = dw_inbasket_folder_index.GetItemStatus(1,0,PRIMARY!)
IF ldwis_row_status = NEW! OR ldwis_row_status = NOTMODIFIED! THEN
   Return 0
END IF

ls_action = dw_inbasket_folder_index.GetItemString(1,"action_code")

If IsNull(ls_action) Then
   MessageBox("Action Code", "You must select an action.")
	Return -1
End If

// Set CLAIM_WORKING folderid if this should be an insert
If IsNull(dw_inbasket_folder_index.GetItemNumber(1,"folderid")) Then
	dw_inbasket_folder_index.SetItem(1,"folderid",al_fldid)
	dw_inbasket_folder_index.SetItemStatus(1,0,PRIMARY!,NewModified!)
End If

// Test claim number for existence on CLAIM_MASTER
ll_claim_no = dw_inbasket_folder_index.GetItemNumber(1,"claim_no")
If IsNull(ll_claim_no) = False Then
	SELECT claim_no
	INTO :ll_check_claim
	FROM CLAIM_MASTER
	WHERE claim_no= :ll_claim_no
	USING ImageTrans;
	li_error_code = ImageTrans.nf_handle_error("cb_inbasket_folder_index_ok","w_inbasket","Validate claim number.")

	IF li_error_code = 100 THEN
		MessageBox("In-Basket", "The claim number specified does not exist in the CLAIM_MASTER file.", StopSign!, Ok!)
		Return -1
   END IF
Else
	MessageBox("In-Basket", "A claim number must be specified.", StopSign!, Ok!)
   Return 0
END IF

ls_claimant_name = iw_sheet.dw_basic_claim.GetItemString(1,"given_names") + " " + iw_sheet.dw_basic_claim.GetItemString(1,"last_name")


ImageTrans.nf_begin_transaction()

// Update folder index form
dw_inbasket_folder_index.Update()
If ImageTrans.nf_handle_error("dw_inbasket_folder_index","w_inbasket","wf_inbasket_process_folder_index") < 0 Then
	Return -1
End If

// Update the folder with the new name
UPDATE FLD
SET    fldname = Upper(:ls_action) + CONVERT(varchar(10),:ll_claim_no) + :ls_claimant_name
WHERE  fldid = :al_fldid
USING ImageTrans;
ImageTrans.nf_handle_error("Embedded SQL: Update FLD","w_inbasket","wf_inbasket_process_folder_index")

ImageTrans.nf_commit_transaction()


Return 1
end function

private function integer wf_inbasket_process_folder_name (long val_fldid);//  This function processes changes to the folder name sle field

// Declare local variables
Integer vli_error_code
Integer vli_function_return_code

LONG    ll_row
STRING  ls_fldname

vli_function_return_code = 1


ll_row = dw_inbasket.GetSelectedRow(0)
IF ll_row = 0 THEN
	IF dw_inbasket.RowCount() > 0 THEN
		MessageBox("Display Folder Details","Could not determine selected folder. Please try selecting it again.")
		Return -1
	END IF
END IF

ls_fldname = dw_inbasket.GetItemString(ll_row,"fldname")

ImageTrans.nf_begin_transaction()

// Update folder name
UPDATE FLD
SET    fldname = :ls_fldname
WHERE  fldid   = :val_fldid
USING ImageTrans;
imagetrans.nf_handle_error("wf_inbasket_process_folder_name","w_inbasket","Update folder name")

// Commit the transaction
ImageTrans.nf_commit_transaction()


RETURN vli_function_return_code

end function

public subroutine wf_inbasket_unpaid_accounts ();

	istr_window_message.al_doubleparm[2] = il_claim_no

/*	Try to open the window and initialize.  If something happens in the initialization
	ensure the window is closed.
*/
	OpenWithParm(iw_account_payment,istr_window_message,iw_sheet)



end subroutine

public function integer wf_inbasket_validate_for_unpaid ();LONG  	ll_rowcount, 	ll_cntr
STRING	ls_folder_list


/* Verify the Claim Status is Finalled and is No Lost Time or Lost Time Medical Aid Only (claim status type 03 and 04)
*/
	IF is_claim_status = 'F' AND &
		(is_claim_status_type = '03') or (is_claim_status_type = '04')  THEN
		// Then can view unpaid accounts for this claim
	ELSE
		MessageBox("In-Basket","Claim status does not permit viewing Unpaid Accounts from In-Basket",Exclamation!)
		Return -1
	END IF


return 0
end function

public subroutine wf_inbasket_set_menu_status (integer ai_menu_status);
/* Determine whether to disable or enable the InBasket Menu option.
	This is required when the Acocunt Payment Maintenance screen is called from within
	the InBasket module to prevent the user from returning to Inbasket without 
	closing Account Payment first

	Argument: Menu Status  	0	- Enable the Inbasket Menu Item
									1	- Disable the Inbasket Menu Item
*/
	IF ai_menu_status = 0 THEN
		iw_sheet.im_menu.m_tools.m_inbasket.Enabled = True
	ELSE
		iw_sheet.im_menu.m_tools.m_inbasket.Enabled = False
	END IF
end subroutine

public function integer wf_check_doc_index ();LONG	ll_rowcount


	SELECT count(*) 
	  INTO :ll_rowcount
	  FROM DOCUMENT_INDEX
	 WHERE claim_no = :il_claim_no
	   AND docid = :il_docid
	 USING IMAGETRANS;
	 IMAGETRANS.nf_handle_error('Embedded SQL: select from doc index','w_inbasket','cb_inbasket_doc_pay_acct')
	 
	IF ll_rowcount < 1 THEN
		MessageBox('Claim Error', 'The claim number for this document does not match the indexing information.  Please call the help desk.')
		Return -1
	END IF
	
Return 0


end function

private function integer wf_inbasket_refresh_folder_list ();//  This function repopulates the folder list from the selected category

//  Set redraw off to stop double display of data
// -----------------------------------------------


dw_inbasket.SetRedraw(False)

dw_inbasket.SetFilter("")
dw_inbasket.Filter()

IF il_claim_view_filter_no > 0 THEN
	dw_inbasket.SetFilter('claim_no = ' + string(il_claim_view_filter_no))
	dw_inbasket.Filter()
ELSEIF is_row_filter <> '' THEN
	dw_inbasket.SetFilter(is_row_filter)
	dw_inbasket.Filter()
END IF

// retrieve using array built above
dw_inbasket.Retrieve(il_catid)
SQLCA.nf_handle_error('w_inbasket','dw_inbasket.retrieve','wf_inbasket_refresh_folder_list')

// Put folder count into instance variable
vil_inbasket_filter_cnt = dw_inbasket.RowCount()

// ------------------------------------------
//  Apply filter to retrieved rows
// ------------------------------------------
IF vil_inbasket_filter_cnt > 0 THEN
	dw_inbasket.SetRow(1)
	dw_inbasket.SelectRow(1,TRUE)

	// -----------------------
	//  Set up screen objects
	// -----------------------
  	wf_inbasket_setup("FOLDERS_FOUND")

ELSEIF vil_inbasket_filter_cnt = 0 THEN
   // Number of rows after filtering is zero if no rows found on retrieve
	wf_inbasket_setup("FOLDERS_NOTFOUND")
	IF is_opening_method <> 'SEARCH' THEN
	   wf_inbasket_clear_workbench()
	END IF
ELSE
	MessageBox("Inbasket Filter Folders","Error determining row count for folder list data window.")
END IF

// --------------------
//  Set redraw back on
// --------------------
dw_inbasket.SetRedraw(True)

// -----------------
//  Sort datawindow
// -----------------
dw_inbasket.Sort()

RETURN 1
end function

public function integer wf_validate_imaged ();long ll_docid
string ls_temp,ls_sort,ls_table,ls_col,ls_cols,ls_filter
datastore lds_data
u_dw_online ldw_data //Holds values from dw_account_payment_list
/*Variable names that start with lower case d are database values 
Variable names that start with lower case w are window values from w_inasket
*/
long ll_dCount,ll_wCount,ll_ColCount,ll_RowCntr,ll_ColCntr
String ls_dwsyntax,ls_errors,ls_oldSyntax
ldw_data = dw_account_payment_list
ll_docid = dw_inbasket.getitemnumber(dw_inbasket.getrow(),'docid')

IF ll_docid = 0 THEN
	MessageBox('Inbasket','Please select a folder that has a document.')
	RETURN -1
END IF

lds_data = create datastore
lds_data.dataobject = ldw_data.dataobject
lds_data.settransobject(SQLCA)
ll_dCount = lds_data.retrieve(ll_docid)

ls_sort = ldw_data.Describe("DataWindow.Table.Sort")
lds_data.setSort(ls_sort)
lds_data.sort()

ldw_data.setSort(ls_sort)
ldw_data.sort()

//Compare rowcount
ll_wCount = ldw_data.RowCount()
if ll_wCount = ll_dCount then
	ll_ColCount = long(lds_data.Object.DataWindow.Column.Count)
	FOR ll_RowCntr = 1 to ll_wCount
		FOR ll_ColCntr = 1 to ll_ColCount
			IF lds_data.Object.Data.Primary.Current[ll_RowCntr,ll_ColCntr] = ldw_data.Object.Data.Primary.Original[ll_RowCntr,ll_ColCntr] THEN
				//Records match do nothing
			ELSE
				IF isNull(lds_data.Object.Data.Primary.Current[ll_RowCntr,ll_ColCntr]) and isNull(ldw_data.Object.Data.Primary.Original[ll_RowCntr,ll_ColCntr]) THEN
				ELSE
					//Records do not match return  error
					return  -4
				END IF
			END IF
		NEXT
	NEXT
else
	if ll_wCount < ll_dCount then
		return  -2
	else
		return  -3
	end if	
end if
return  0
end function

public function string wf_validate_list_data ();long ll_rtn
string ls_rtn
IF ib_imaged_view THEN
	ll_rtn = this.trigger function wf_validate_imaged()
	IF ll_rtn < 0 THEN 
		Choose Case ll_rtn 
			Case -1
				ls_rtn =  "Error retrieving claim information."
			Case -2
				ls_rtn =  "There has been an addition to this document. Please refresh data."
			Case -3 
				ls_rtn =  "There has been a deletion to this document. Please refresh data."
			Case -4
				ls_rtn =  "There has been a change to this documents data. Please refresh data."
		End Choose
	End IF
ELSE
	ll_rtn = this.trigger function wf_validate_nonimaged()
	IF ll_rtn < 0 THEN 
		Choose Case ll_rtn 
			Case -1
				ls_rtn =  "Error retrieving claim information on non imaged Claim."
			Case -2
				ls_rtn =  "There has been an addition to this claim. Please refresh data."
			Case -3 
				ls_rtn =  "There has been a deletion to this claim. Please refresh data."
			Case -4
				ls_rtn =  "There has been a change to this claim data. Please refresh data."
		End Choose		
	End IF
END IF

RETURN ls_rtn

end function

public function integer wf_validate_nonimaged ();long ll_docid
string ls_temp,ls_sort,ls_table,ls_col,ls_cols,ls_filter
datastore lds_data
u_dw_online ldw_data //Holds values from dw_account_payment_list
/*Variable names that start with lower case d are database values 
Variable names that start with lower case w are window values from w_inbasket
*/
long ll_dCount,ll_wCount,ll_ColCount,ll_RowCntr,ll_ColCntr
String ls_dwsyntax,ls_errors,ls_oldSyntax
ldw_data = dw_account_payment_list
ll_docid = dw_inbasket.getitemnumber(dw_inbasket.getrow(),'docid')
IF ll_docid = 0 THEN
	MessageBox('Inbasket','Please select a folder that has a document.')
	RETURN -1
END IF
lds_data = create datastore
lds_data.dataobject = ldw_data.dataobject
lds_data.settransobject(SQLCA)
lds_data.retrieve(ll_docid)

ls_sort = ldw_data.Describe("DataWindow.Table.Sort")
lds_data.setSort("")
lds_data.sort()
ldw_data.setSort("")
ldw_data.sort()

lds_data.setSort("payment_no DESC")
lds_data.sort()
ldw_data.setSort("payment_no DESC")
ldw_data.sort()

//Compare rowcount
ll_wCount = ldw_data.rowcount() 
ll_dCount = lds_data.rowcount()
if ll_wCount = ll_dCount then
	ll_ColCount = long(lds_data.Object.DataWindow.Column.Count)
	FOR ll_RowCntr = 1 to ll_wCount
		FOR ll_ColCntr = 1 to ll_ColCount
			IF lds_data.Object.Data.Primary.Current[ll_RowCntr,ll_ColCntr] = ldw_data.Object.Data.Primary.Original[ll_RowCntr,ll_ColCntr] THEN
				//Records match do nothing
			ELSE
				IF isNull(lds_data.Object.Data.Primary.Current[ll_RowCntr,ll_ColCntr]) and isNull(ldw_data.Object.Data.Primary.Original[ll_RowCntr,ll_ColCntr]) THEN
				ELSE
					//Records do not match return  error
					ldw_data.setSort(ls_sort)
					ldw_data.sort()
					return  -4
				END IF
			END IF
		NEXT
	NEXT
else
	if ll_wCount < ll_dCount then
		ldw_data.setSort(ls_sort)
		ldw_data.sort()
		return  -2
	else
		ldw_data.setSort(ls_sort)
		ldw_data.sort()
		return  -3
	end if	
end if
ldw_data.setSort(ls_sort)
ldw_data.sort()
return  0
end function

public subroutine wf_inbasket_account_pay ();//	Make sure the user is authorized for the claim's region
string ls_admin_region

ls_admin_region = iw_sheet.dw_basic_claim.GetItemString(1,'admin_region_code')
	
	
IF gnv_user_authorizations.nf_authorizations_exist(ls_admin_region,"act") = FALSE THEN
	MessageBox("In-Basket - Pay Account","You have no authorization for this claim's region",Exclamation!)
	Return
END IF

/* The nf_validate_cost_alloc function displays the error message within the function
*/
IF inv_payment.nf_validate_cost_alloc() < 0 THEN
	Return
END IF

istr_window_message.al_doubleparm[2] = il_claim_no
/*	Try to open the window and initialize.  If something happens in the initialization
ensure the window is closed.
*/
OpenWithParm(iw_account_payment_maintenance,istr_window_message,iw_sheet)
IF IsValid(iw_account_payment_maintenance) THEN
	iw_account_payment_maintenance.wf_set_payment_object(inv_payment)
	iw_account_payment_maintenance.wf_set_parent_window(this)
END IF



end subroutine

private function integer wf_inbasket_setup (string vas_inbasket_state);
// setting the inbasket controls for 'search' mode overrides all other setup cases
IF is_opening_method = 'SEARCH' AND ib_opening THEN
	dw_inbasket_category_select.Enabled                       = FALSE
	dw_inbasket_category_select.Object.catid.background.color = '553648127'
	
	st_title.Text                           = 'In-Basket for Claim'
	cbx_claim_view.Enabled                  = FALSE
	cb_inbasket_refresh.Enabled             = FALSE
	IF dw_inbasket.RowCount() > 0 THEN
		IF ib_opening THEN			
			dw_inbasket.SetRow(1)
			dw_inbasket.SelectRow(1,TRUE)
		END IF
	END IF
END IF


CHOOSE CASE vas_inbasket_state

   CASE "DOCUMENTS_FOUND"
      cb_inbasket_doc_view.enabled 					= True
		cb_inbasket_doc_reviewed.enabled				= True
      cb_inbasket_doc_index.enabled		 			= True
      cb_inbasket_doc_delete.enabled	 			= True
      cb_inbasket_doc_pay_acct.enabled	 			= True
      cb_inbasket_doc_reject_acct.enabled	 		= True
		cb_inbasket_folder_unpaid_accts.Enabled   = TRUE

   CASE "DOCUMENTS_NOT_FOUND"
      cb_inbasket_doc_view.enabled 					= False
		cb_inbasket_doc_reviewed.enabled				= False
      cb_inbasket_doc_index.enabled 				= False
      cb_inbasket_doc_delete.enabled 				= False
      cb_inbasket_doc_pay_acct.enabled 			= False
      cb_inbasket_doc_reject_acct.enabled 		= False
		cb_inbasket_folder_unpaid_accts.Enabled   = FALSE

	CASE "FOLDERS_FOUND"
		gb_inbasket_category_selection.enabled		= True
      cb_inbasket_refresh.enabled        			= True
      cb_close.enabled          						= True
      dw_inbasket_category_select.enabled  		= True
      dw_inbasket.enabled                       = True
      
	CASE "FOLDERS_NOTFOUND"
		gb_inbasket_category_selection.enabled    = True
      cb_inbasket_refresh.enabled       		   = True
      cb_close.enabled  			       		   = True
      dw_inbasket_category_select.enabled  	   = True
      dw_inbasket.enabled                       = False
     
	CASE "OPEN"
		gb_inbasket_category_selection.enabled    = True
      cb_inbasket_refresh.enabled               = False
      cb_close.enabled                          = True
      dw_inbasket_category_select.enabled       = True
      dw_inbasket.enabled                       = False
   
	CASE "REFRESH"
		gb_inbasket_category_selection.enabled  	= True
      cb_inbasket_refresh.enabled      		 	= False
      cb_close.enabled        			 		 	= True
      dw_inbasket_category_select.enabled		 	= True
      dw_inbasket.enabled                       = True

END CHOOSE


Return 1
end function

private subroutine wf_inbasket_display_folder_details ();//  This function sets up and displays the folder details

// -------------------------
//  Declare local variables
// -------------------------
Long    ll_row_nmbr,	ll_docid,	ll_fldid,	ll_claim_cnt,	ll_results
Integer li_error_code

// Initialize Instance variables
vib_inbasket_folder_multi_claim = False

// ------------------------------------------------------
//  Display folder name at the top of the screen display
// ------------------------------------------------------
ll_row_nmbr = dw_inbasket.GetSelectedRow(0)
IF ll_row_nmbr = 0 THEN
	IF dw_inbasket.RowCount() > 0 THEN
		MessageBox("Display Folder Details","Could not determine selected folder. Please try selecting it again.")
		Return
	END IF
END IF

// --------------------------------------------------
//  Display list of documents for the current folder
// --------------------------------------------------
ll_fldid = wf_inbasket_get_selected_fldid()
If ll_fldid = -1 OR ll_fldid = 0 Then
	Return
End If

// Display document list
ll_docid = dw_inbasket.GetItemNumber(ll_row_nmbr,'docid')


// No documents found for the selected folder
IF ll_docid = 0 THEN
   wf_inbasket_setup("DOCUMENTS_NOT_FOUND")
   // Declare folder index information
   ll_results = wf_inbasket_display_folder_index(ll_fldid)
	RETURN
ELSE
	wf_post_select_folder(ll_fldid)
END IF
end subroutine

private subroutine wf_inbasket_display_document_index ();// wf_inbasket_display_document_index - displays document indexing information.

// Declare local variables
INTEGER li_rtn
Long    ll_docid,		ll_selected_row,	ll_fldid
String  ls_type_code, ls_auto_created_flag
Boolean lb_indexed

// --------------------------------------
//  Get document id of selected document 
// --------------------------------------
ll_selected_row = dw_inbasket.GetSelectedRow(0)
IF ll_selected_row = 0 THEN
   MessageBox('Inbasket Document','Error determining selected document. Please try again.')
	Return
END IF

// get document id
ll_docid = dw_inbasket.GetItemNumber(ll_selected_row,'docid')
IF ll_docid = 0 THEN
   MessageBox('InBasket','Please select a folder that has a document.')
	RETURN
END IF

// See if document was automatically generated
ls_type_code = dw_inbasket.GetItemString(ll_selected_row, 'type_code')
SELECT auto_created_flag 
  INTO :ls_auto_created_flag 
  FROM Document_Type_Code 
 WHERE type_code = :ls_type_code ;
SQLCA.nf_handle_error('w_inbasket','SELECT auto_created_flag FROM Document_Type_Code...','wf_inbasket_display_document_index')


IF ls_auto_created_flag = 'Y' THEN
	Messagebox('Inbasket Document', 'Automatically generated docuents must be re-indexed from the Indexing Module.', Exclamation!)
	RETURN
END IF

istr_window_message.al_doubleparm[1] = ll_docid

OpenWithParm(w_inbasket_document_index,istr_window_message)

dw_inbasket.SetRedraw(FALSE)

// Refresh the folder/document list
ll_fldid = dw_inbasket.GetItemNumber(1,'fldid')

dw_inbasket.Retrieve(il_catid)
ImageTrans.nf_handle_error('w_inbasket','dw_inbasket.retrieve','wf_inbasket_display_document_index')

dw_inbasket.SetRow(ll_selected_row)
dw_inbasket.SelectRow(ll_selected_row,TRUE)
dw_inbasket.ScrollToRow(ll_selected_row)
dw_inbasket.uf_processselect(ll_selected_row,'Keyboard')

dw_inbasket.SetRedraw(TRUE)

// Mark row clicked as the selected row
wf_inbasket_setup('DOCUMENT_FOUND')

end subroutine

public function integer wf_explode_frame_and_set_sheets ();/*	This function is used to resize the frame and the 'Work Sheet'. It also move all
	the controls over to the right side.
*/
	INTEGER			li_frame_width, li_frame_height, li_screen_width, li_screen_height, li_height_line, li_width_line
	INTEGER			li_rtn
	ENVIRONMENT		lenv_enviroment
	

/*	Turn off the drawing of the application until all is done.
*/
	w_frame.SetRedraw(FALSE)

/*	Grab the position and width of the both the frame and the sheet.
*/
	ii_frameheight = w_frame.Height
	ii_framewidth = w_frame.Width
	ii_framex = w_frame.X
	ii_framey = w_frame.y
	ii_oldsheetwidth = iw_sheet.Width
	ii_oldsheetx = iw_sheet.X
	ii_oldsheetheight = iw_sheet.Height
	ii_oldsheetx = iw_sheet.Y

// set height/width to fit the module; the ln_height_sizer & ln_width_sizer line objects are as high & wide as the sheet
	li_height_line = ln_height_sizer.endY
	li_width_line  = ln_width_sizer.endX

	li_frame_height = li_height_line + .75 * li_height_line
	li_frame_width  = li_width_line  + .03 * li_width_line
	

/*	Make the size of the window equal to the size of the screen.
*/
	w_frame.Height = li_frame_height
	w_frame.Width  = li_frame_width
	
	
/* Grab the enviroment of the system. Will need to get the size of the desktop to assist in centering of frame
*/
	li_rtn = GetEnvironment(lenv_enviroment)
	IF li_rtn < 0 THEN
		RETURN li_rtn
	ELSE
		li_screen_height = lenv_enviroment.ScreenHeight
		li_screen_height = PixelsToUnits(li_screen_height,YPixelsToUnits!)
		li_screen_width = lenv_enviroment.ScreenWidth
		li_screen_width = PixelsToUnits(li_screen_width,XPixelsToUnits!)
	END IF
	
	// center the frame
	w_frame.X = (li_screen_width - li_frame_width)/2
	w_frame.Y = (li_screen_height - li_frame_height)/2


/*	Turn the drawing of the application back on.
*/
	w_frame.SetRedraw(TRUE)
	
	RETURN 0
end function

public subroutine wf_reset_screen_sizes ();INTEGER			li_openedsheets
W_SHEET			lw_activesheet
WINDOW			lw_wsheet
STRING			wName

/*	Set the size and position of the frame back to their originals.
*/
	w_frame.Width  = ii_framewidth
	w_frame.Height = ii_frameheight
	w_frame.X      = ii_framex
	w_frame.Y      = ii_framey

/*	Determine if there is a work sheet opened, and if so then move all the controls over.
*/
	li_openedsheets = 0
	lw_wsheet = w_frame.GetFirstSheet()
	IF IsValid(lw_wsheet) THEN
		wName = lw_wsheet.ClassName()
		IF wname = 'w_sheet' THEN
			li_openedsheets ++
		END IF
		DO
			lw_wsheet = w_frame.GetNextSheet(lw_wsheet)
			IF IsValid(lw_wsheet) THEN
				wName = lw_wsheet.ClassName()
				IF wname = 'w_sheet' THEN
					li_openedsheets ++
				END IF
			END IF	
		LOOP WHILE IsValid(lw_wsheet)
	END IF

	IF li_openedsheets > 0 THEN
		iw_sheet.X      = ii_oldsheetx
		iw_sheet.Width  = ii_oldsheetwidth
		iw_sheet.Y      = ii_oldsheety
		iw_sheet.Height = ii_oldsheetheight
	END IF
	
end subroutine

public function integer wf_verify_event ();INTEGER        li_event_count



IF istr_inbasket_event.event_category_code = 'C' THEN
	
	SELECT COUNT(*)
	INTO   :li_event_count
	FROM   CLAIM_EVENT
	WHERE  claim_no = :istr_inbasket_event.claim_no
	AND    event_no = :istr_inbasket_event.event_no
	USING SQLCA;
	SQLCA.nf_handle_error('w_inbasket','embedded SQL: SELECT COUNT(*)	FROM CLAIM_EVENT...','wf_verify_event')
	
ELSEIF istr_inbasket_event.event_category_code = 'I' THEN

	SELECT COUNT(*)
	INTO   :li_event_count
	FROM   INDIVIDUAL_EVENT
	WHERE  individual_no       = :istr_inbasket_event.individual_no
	AND    individual_event_no = :istr_inbasket_event.event_no
	USING SQLCA;
	SQLCA.nf_handle_error('w_inbasket','embedded SQL: SELECT COUNT(*)	FROM CLAIM_EVENT...','wf_verify_event')
	
ELSE
	li_event_count = 0
END IF

RETURN li_event_count
end function

public subroutine wf_determine_cat_array ();
DataWindowChild   ldw_category_select_child
INTEGER           li_counter, li_category_count
LONG              ll_null[]
STRING            ls_cat_catname


dw_inbasket_category_select.GetChild('catid',ldw_category_select_child)
ldw_category_select_child.SetTransObject(ImageTrans)

ls_cat_catname = ldw_category_select_child.GetItemString(ldw_category_select_child.GetRow(),'cat_catname')

IF ls_cat_catname = 'ALL' THEN	
	li_category_count = ldw_category_select_child.RowCount()
	FOR li_counter = 1 TO li_category_count
		IF IsNull(ldw_category_select_child.GetItemNumber(li_counter, 'cat_catid')) THEN
			// the 'ALL' category has catid = null
			CONTINUE
		ELSE
			il_catid[li_counter] = ldw_category_select_child.GetItemNumber(li_counter, 'cat_catid')
		END IF
	NEXT
ELSE
	// reset array, and set to selected category
	il_catid    = ll_null
	il_catid[1] = il_category_id_nmbr
END IF

end subroutine

public subroutine wf_post_select_folder (long al_fldid);// this function is to be called when a folder has been selected, and the folder has an associated document

INTEGER     li_claim_count, li_rtn



// Setup screen display for listing documents
wf_inbasket_setup('DOCUMENTS_FOUND')

// Count how many claim numbers are contained in the folder
SELECT count(DISTINCT claim_no)
INTO   :li_claim_count
FROM   REF            a
JOIN   DOCUMENT_INDEX b ON a.docid = b.docid
WHERE  a.docfldid = :al_fldid
USING ImageTrans;
ImageTrans.nf_handle_error('Embedded SQL: Select from REF and DOCUMENT_INDEX','w_inbasket','wf_inbasket_display_folder_details')

// If no claim number or a single claim number is assigned to the documents 
// in the work folder then allow the user to maintain the work folder form.
// Otherwise, the user can change the folder name directly

IF IsNull(li_claim_count) or li_claim_count < 2 THEN

	// Declare folder index information
	li_rtn = wf_inbasket_display_folder_index(al_fldid)
	IF li_rtn = -1 THEN RETURN

ELSE
	vib_inbasket_folder_multi_claim     = TRUE
	cb_inbasket_folder_index_ok.enabled = FALSE
	
	MessageBox('Folder','This folder contains documents from multiple claims.~r~nYou may not create/modify the folder index for this folder.')
END IF
end subroutine

public subroutine wf_event_button_visibility (long al_current_row);
BOOLEAN           lb_event_found
INTEGER           li_pos, li_event_count
LONG              ll_event_no
STRING            ls_action_note, ls_event_no_substring




ls_action_note = lower(dw_inbasket.GetItemString(al_current_row,'action_note'))

// see claim event 
li_pos = Pos(ls_action_note,'see claim event ')
IF li_pos > 0 THEN
	ls_event_no_substring = Right(ls_action_note, (Len(ls_action_note) - (li_pos+15))) 
	ll_event_no = LONG(ls_event_no_substring)
	IF ll_event_no > 0 THEN
		istr_inbasket_event.claim_no            = dw_inbasket.GetItemNumber(al_current_row,'claim_no')
		istr_inbasket_event.individual_no       = dw_inbasket.GetItemNumber(al_current_row,'individual_no')
		istr_inbasket_event.event_category_code = 'C'
		istr_inbasket_event.event_no            = ll_event_no
		
		SELECT COUNT(*)
		INTO   :li_event_count
		FROM   CLAIM_EVENT
		WHERE  event_category_code = 'C'
		AND    claim_no            = :istr_inbasket_event.claim_no
		AND    event_no            = :istr_inbasket_event.event_no
		USING SQLCA;
		SQLCA.nf_handle_error('w_inbasket','embedded SQL: SELECT COUNT(*) FROM CLAIM_EVENT (1)...','dw_inbasket.RowFocusChanged')
		
		IF li_event_count > 0 THEN
			lb_event_found = TRUE
		END IF
	END IF
END IF

// see event 
li_pos = Pos(ls_action_note,'see event ')
IF li_pos > 0 THEN
	ls_event_no_substring = Right(ls_action_note, (Len(ls_action_note) - (li_pos+9))) 
	ll_event_no = LONG(ls_event_no_substring)
	IF ll_event_no > 0 THEN
		istr_inbasket_event.claim_no            = dw_inbasket.GetItemNumber(al_current_row,'claim_no')
		istr_inbasket_event.individual_no       = dw_inbasket.GetItemNumber(al_current_row,'individual_no')
		istr_inbasket_event.event_category_code = 'C'
		istr_inbasket_event.event_no            = ll_event_no
		
		SELECT COUNT(*)
		INTO   :li_event_count
		FROM   CLAIM_EVENT
		WHERE  event_category_code = 'C'
		AND    claim_no            = :istr_inbasket_event.claim_no
		AND    event_no            = :istr_inbasket_event.event_no
		USING SQLCA;
		SQLCA.nf_handle_error('w_inbasket','embedded SQL: SELECT COUNT(*) FROM CLAIM_EVENT (2)...','dw_inbasket.RowFocusChanged')
		
		IF li_event_count > 0 THEN
			lb_event_found = TRUE
		END IF
	END IF
END IF

// see ind. event 
li_pos = Pos(ls_action_note,'see ind. event ')
IF li_pos > 0 THEN
	ls_event_no_substring = Right(ls_action_note, (Len(ls_action_note) - (li_pos+14))) 
	ll_event_no = LONG(ls_event_no_substring)
	IF ll_event_no > 0 THEN
		istr_inbasket_event.claim_no            = dw_inbasket.GetItemNumber(al_current_row,'claim_no')
		istr_inbasket_event.individual_no       = dw_inbasket.GetItemNumber(al_current_row,'individual_no')
		istr_inbasket_event.event_category_code = 'I'
		istr_inbasket_event.event_no            = ll_event_no
		
		SELECT COUNT(*)
		INTO   :li_event_count
		FROM   INDIVIDUAL_EVENT
		WHERE  event_category_code = 'I'
		AND    individual_no       = :istr_inbasket_event.individual_no
		AND    individual_event_no = :istr_inbasket_event.event_no
		USING SQLCA;
		SQLCA.nf_handle_error('w_inbasket','embedded SQL: SELECT COUNT(*) FROM INDIVIDUAL_EVENT...','dw_inbasket.RowFocusChanged')
		
		IF li_event_count > 0 THEN
			lb_event_found = TRUE
		END IF
	END IF
END IF

// see ind. event 
li_pos = Pos(ls_action_note,'prgs')
IF li_pos > 0 THEN
	ls_event_no_substring = Right(ls_action_note, (Len(ls_action_note) - (li_pos+3))) 
	ll_event_no = LONG(ls_event_no_substring)
	IF ll_event_no > 0 THEN
		istr_inbasket_event.claim_no            = dw_inbasket.GetItemNumber(al_current_row,'claim_no')
		istr_inbasket_event.individual_no       = dw_inbasket.GetItemNumber(al_current_row,'individual_no')
		istr_inbasket_event.event_category_code = 'C'
		istr_inbasket_event.event_no            = ll_event_no
		
		SELECT COUNT(*)
		INTO   :li_event_count
		FROM   CLAIM_EVENT
		WHERE  event_category_code = 'C'
		AND    claim_no = :istr_inbasket_event.claim_no
		AND    event_no = :istr_inbasket_event.event_no
		USING SQLCA;
		SQLCA.nf_handle_error('w_inbasket','embedded SQL: SELECT COUNT(*) FROM EVENT...','dw_inbasket.RowFocusChanged')
		
		IF li_event_count > 0 THEN
			lb_event_found = TRUE
		END IF
	END IF
END IF


IF lb_event_found THEN
	cb_event.Visible = TRUE
ELSE
	cb_event.Visible = FALSE
END IF

end subroutine

event open;call super::open;//
//-----APPLICATION SECURITY CODE-----
G_PFSecurity.UOF_Check_Access(This)
This.I_Authorized_Access = True

// ---------------------------
//  Declare working variables
// ---------------------------
INTEGER           li_result
long              ll_rows_loaded,		ll_counter
DataWindowChild   ldwc_child_name
s_window_message  lstr_message


lstr_message = Message.PowerObjectParm
is_opening_method    = lstr_message.as_stringparm[1]
il_selected_claim_no = lstr_message.al_doubleparm[1]
iw_sheet             = lstr_message.apo_powerobjectparm[1]
If IsValid(iw_sheet) = False Then
	MessageBox("Inbasket","Error determining active sheet. You may have to close WorkBench and try again.")
	Close(This)
	Return
End If

// Create an instance of the user object for the imaging functions
inv_imaging = CREATE n_imaging


ib_explodewasgood = TRUE
li_result = wf_explode_frame_and_set_sheets()
IF li_result < 0 THEN
	ib_explodewasgood = FALSE
	CLOSE(THIS)
	RETURN
END IF


// -------------------------------
//  Initialize instance variables
// -------------------------------
il_category_id_nmbr        = 0
vil_inbasket_filter_cnt    = 0
vil_folder_list_anchor_row = 0

// ------------------------------------------
//  Set transaction objects for data windows
// ------------------------------------------
dw_inbasket_category_select.SetTransObject(ImageTrans)
dw_inbasket_folder_index.SetTransObject(ImageTrans)
dw_inbasket_forward_to.SetTransObject(ImageTrans)
dw_inbasket.SetTransObject(SQLCA)

/*	Set transaction objects on the datawindows to allow the user to create
	payments from within In-Basket
*/
	dw_display_claim_info.SetTransObject(SQLCA)
	dw_account_payment_paid_status.SetTransObject(SQLCA)
	dw_account_payment_list.SetTransObject(SQLCA)
	dw_claimsmaster_ref_count.SetTransObject(ImageTrans)




// -------------------------------------------------
//  Set which sheet objects are visible and enabled
// -------------------------------------------------
ib_opening = TRUE
wf_inbasket_setup("OPEN")


// -------------------------------------------------
// Load the user's inbasket list
// -------------------------------------------------
dw_inbasket_category_select.GetChild("catid",vidw_category_select_child)
vidw_category_select_child.SetTransObject(ImageTrans)

ll_rows_loaded = vidw_category_select_child.Retrieve(vgst_user_profile.user_id)
If ImageTrans.nf_handle_error("viw_category_select_child","w_inbasket","Open") < 0 then
	Close(This)
	Return
End If

// If no rows found then user has no access to InBasket
If ll_rows_loaded <= 0 Then
   MessageBox("In-Basket", "No In-baskets found for you.  You cannot use the In-Basket until you have added some.", StopSign!, OK!)
	Close(This)
	Return
End If

// Show the category list
dw_inbasket_category_select.InsertRow(0)



// -------------------------------------------------
// Load user's forward to list
// -------------------------------------------------
dw_inbasket_forward_to.GetChild("catid",vidw_forward_to_child)
vidw_forward_to_child.SetTransObject(ImageTrans)

ll_rows_loaded = vidw_forward_to_child.Retrieve(vgst_user_profile.user_id)
If ImageTrans.nf_handle_error("viw_forward_to_child","w_inbasket","Open") < 0 then
	Close(This)
	Return
End If

// If no rows found then user has no access to InBasket
If ll_rows_loaded <= 0 Then
   MessageBox("In-Basket", "No Forwarding To baskets found.  You cannot use the In-Basket until you have added some.", StopSign!, OK!)
	Close(This)
	Return
End If

dw_inbasket.uf_setselect(1)
dw_inbasket.uf_setsort(True)


uo_reference_search.uf_set_parent(Handle(This))

il_selected_claim_no = iw_sheet.dw_basic_claim.GetItemNumber(1,'claim_no')

IF is_opening_method <> 'SEARCH' THEN ib_opening = FALSE



end event

event closequery;call super::closequery;If IsValid(inv_imaging) Then
	Destroy inv_imaging
End If


end event

on w_inbasket.create
int iCurrent
call super::create
this.dw_account_payment_list=create dw_account_payment_list
this.cb_inbasket_doc_reject_acct=create cb_inbasket_doc_reject_acct
this.dw_account_payment_paid_status=create dw_account_payment_paid_status
this.dw_claimsmaster_ref_count=create dw_claimsmaster_ref_count
this.cb_inbasket_doc_pay_acct=create cb_inbasket_doc_pay_acct
this.cb_inbasket_refresh=create cb_inbasket_refresh
this.cb_inbasket_doc_delete=create cb_inbasket_doc_delete
this.cb_inbasket_doc_view=create cb_inbasket_doc_view
this.cb_inbasket_doc_index=create cb_inbasket_doc_index
this.dw_inbasket_category_select=create dw_inbasket_category_select
this.cb_inbasket_folder_delete=create cb_inbasket_folder_delete
this.st_inbasket_forward_to=create st_inbasket_forward_to
this.dw_inbasket_forward_to=create dw_inbasket_forward_to
this.cb_inbasket_folder_index_ok=create cb_inbasket_folder_index_ok
this.cb_inbasket_folder_index_cancel=create cb_inbasket_folder_index_cancel
this.dw_display_claim_info=create dw_display_claim_info
this.gb_inbasket_category_selection=create gb_inbasket_category_selection
this.dw_inbasket_folder_index=create dw_inbasket_folder_index
this.cb_bfclear=create cb_bfclear
this.uo_reference_search=create uo_reference_search
this.cb_inbasket_folder_unpaid_accts=create cb_inbasket_folder_unpaid_accts
this.cb_inbasket_doc_reviewed=create cb_inbasket_doc_reviewed
this.uo_image_append=create uo_image_append
this.cbx_claim_view=create cbx_claim_view
this.cb_event=create cb_event
this.dw_inbasket=create dw_inbasket
this.ln_height_sizer=create ln_height_sizer
this.ln_width_sizer=create ln_width_sizer
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_account_payment_list
this.Control[iCurrent+2]=this.cb_inbasket_doc_reject_acct
this.Control[iCurrent+3]=this.dw_account_payment_paid_status
this.Control[iCurrent+4]=this.dw_claimsmaster_ref_count
this.Control[iCurrent+5]=this.cb_inbasket_doc_pay_acct
this.Control[iCurrent+6]=this.cb_inbasket_refresh
this.Control[iCurrent+7]=this.cb_inbasket_doc_delete
this.Control[iCurrent+8]=this.cb_inbasket_doc_view
this.Control[iCurrent+9]=this.cb_inbasket_doc_index
this.Control[iCurrent+10]=this.dw_inbasket_category_select
this.Control[iCurrent+11]=this.cb_inbasket_folder_delete
this.Control[iCurrent+12]=this.st_inbasket_forward_to
this.Control[iCurrent+13]=this.dw_inbasket_forward_to
this.Control[iCurrent+14]=this.cb_inbasket_folder_index_ok
this.Control[iCurrent+15]=this.cb_inbasket_folder_index_cancel
this.Control[iCurrent+16]=this.dw_display_claim_info
this.Control[iCurrent+17]=this.gb_inbasket_category_selection
this.Control[iCurrent+18]=this.dw_inbasket_folder_index
this.Control[iCurrent+19]=this.cb_bfclear
this.Control[iCurrent+20]=this.uo_reference_search
this.Control[iCurrent+21]=this.cb_inbasket_folder_unpaid_accts
this.Control[iCurrent+22]=this.cb_inbasket_doc_reviewed
this.Control[iCurrent+23]=this.uo_image_append
this.Control[iCurrent+24]=this.cbx_claim_view
this.Control[iCurrent+25]=this.cb_event
this.Control[iCurrent+26]=this.dw_inbasket
this.Control[iCurrent+27]=this.ln_height_sizer
this.Control[iCurrent+28]=this.ln_width_sizer
end on

on w_inbasket.destroy
call super::destroy
destroy(this.dw_account_payment_list)
destroy(this.cb_inbasket_doc_reject_acct)
destroy(this.dw_account_payment_paid_status)
destroy(this.dw_claimsmaster_ref_count)
destroy(this.cb_inbasket_doc_pay_acct)
destroy(this.cb_inbasket_refresh)
destroy(this.cb_inbasket_doc_delete)
destroy(this.cb_inbasket_doc_view)
destroy(this.cb_inbasket_doc_index)
destroy(this.dw_inbasket_category_select)
destroy(this.cb_inbasket_folder_delete)
destroy(this.st_inbasket_forward_to)
destroy(this.dw_inbasket_forward_to)
destroy(this.cb_inbasket_folder_index_ok)
destroy(this.cb_inbasket_folder_index_cancel)
destroy(this.dw_display_claim_info)
destroy(this.gb_inbasket_category_selection)
destroy(this.dw_inbasket_folder_index)
destroy(this.cb_bfclear)
destroy(this.uo_reference_search)
destroy(this.cb_inbasket_folder_unpaid_accts)
destroy(this.cb_inbasket_doc_reviewed)
destroy(this.uo_image_append)
destroy(this.cbx_claim_view)
destroy(this.cb_event)
destroy(this.dw_inbasket)
destroy(this.ln_height_sizer)
destroy(this.ln_width_sizer)
end on

event close;call super::close;
IF ib_explodewasgood 				THEN	wf_reset_screen_sizes()


IF is_opening_method = 'MENU' THEN
	post close(iw_sheet)
END IF

end event

type st_title from w_a_tool`st_title within w_inbasket
integer x = 23
integer width = 6578
string text = "In-Basket"
end type

type cb_close from w_a_tool`cb_close within w_inbasket
integer x = 2555
integer y = 1668
integer taborder = 240
end type

type dw_account_payment_list from u_dw_online within w_inbasket
boolean visible = false
integer x = 494
integer y = 1684
integer width = 165
integer height = 100
integer taborder = 290
boolean enabled = false
string dataobject = "d_account_payment_list"
end type

type cb_inbasket_doc_reject_acct from commandbutton within w_inbasket
integer x = 4320
integer y = 1500
integer width = 407
integer height = 96
integer taborder = 300
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Re&ject Acct..."
end type

event clicked;LONG  ll_rowcount, ll_cntr, ll_valid_acct, ll_rec_count,	ll_docid, ll_payment_no, ll_account_rownum




//	Create an instance of the payment user object

	inv_payment = Create n_payment
	inv_payment.nf_set_basic_claim(dw_display_claim_info)

/*
	Get the document information
*/
	wf_inbasket_get_doc_info()

	IF wf_check_doc_index() < 0 THEN
		Return
	END IF		
/* Validate the document meets the requirements for paying/rejecting 
*/
	ll_valid_acct = wf_validate_acct_doc()

/* Now that the claim and document has been validated, validate that all payments for this document
	have been cancelled (if paid) before allowing the user to reject the payment
*/
	IF ll_valid_acct = 0 THEN
		/*	Check to ensure that all payments for this document have been cancelled.
			We didn't do this in rowfocuschanged, since we want the user to know why
			they can't reject and we didn't want to give them the message unless they
			actually tried to reject
		*/
		ll_rowcount = dw_account_payment_list.Retrieve(il_docid)
		IF ll_rowcount > 0 THEN

			ll_cntr = 1
			DO WHILE ll_cntr <= ll_rowcount
				ll_docid		= dw_account_payment_list.GetItemNumber(ll_cntr,"doc_id")
				ll_payment_no = dw_account_payment_list.GetItemNumber(ll_cntr,"payment_no")
				IF IsNull(ll_payment_no) THEN
					MessageBox("In-Basket  - Reject Account" + " - Data Integrity Error","Payment Number not found for document # " + string(ll_docid) + &
								"~r~nReject not allowed.  Please call the help desk",Exclamation!)
					Return
				END IF

				SELECT 	count(*) 
			  		INTO	:ll_rec_count
			  		FROM	APPLIED_CLAIM_TXN 
			  		WHERE	payment_no = :ll_payment_no and 
							(canceled_txn_flag = "N" and
							 related_txn_no = 0) using SQLCA;

				IF ll_rec_count > 0 THEN
					MessageBox("In-Basket  - Reject Account","You must cancel all payments for this document before it can be rejected",Exclamation!)
					Return
				END IF
				ll_cntr ++
			LOOP
		END IF
		/* Now that the document has been validate for rejecting, 
		 	Load the structure and open the account payment maintenance window
		*/
		istr_window_message.al_doubleparm[1]		= 0
		istr_window_message.al_doubleparm[3] 		= il_docid
		istr_window_message.al_doubleparm[4] 		= il_service_provider_no
		istr_window_message.as_stringparm[1]		= is_document_type
		istr_window_message.as_stringparm[2]      = ''
		istr_window_message.adtm_datetimeparm[1]	= idtm_date_on_document
		istr_window_message.as_mode					= "inbasketreject"
		
		wf_inbasket_account_pay()
	
	END IF
end event

type dw_account_payment_paid_status from u_dw_online within w_inbasket
boolean visible = false
integer x = 878
integer y = 1684
integer width = 165
integer height = 100
integer taborder = 160
boolean enabled = false
string dataobject = "d_account_payment_paid_status"
end type

type dw_claimsmaster_ref_count from u_dw_online within w_inbasket
boolean visible = false
integer x = 686
integer y = 1684
integer width = 165
integer height = 100
integer taborder = 150
string dataobject = "d_claimsmaster_ref_count"
end type

type cb_inbasket_doc_pay_acct from commandbutton within w_inbasket
integer x = 3890
integer y = 1500
integer width = 407
integer height = 96
integer taborder = 330
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Pa&y Acct..."
end type

event clicked;LONG 		ll_valid_acct

	iw_account_payment_maintenance = w_account_payment_maintenance

//	Create an instance of the payment user object

	inv_payment = Create n_payment
	inv_payment.nf_set_basic_claim(dw_display_claim_info)

/*
	Get the document information
*/
	wf_inbasket_get_doc_info()

/*	June 19/97 - before continuing check that the docid matches the entry for the claim on the 
	Document_Index table
	there was a problem with some payments being paid to the wrong claim even though the document_index info
	was correct - this is a doulbe check to help track down the error
*/
	IF wf_check_doc_index() < 0 THEN
		Return
	END IF		
/* Validate the document meets the requirements for paying/rejecting 
*/
	ll_valid_acct =  wf_validate_acct_doc()

/* Now that the claim and document has been validated, open the Account Payment Window
	Load the structure and open the payment maintenance window
*/
	IF ll_valid_acct = 0 THEN
		/* Check claim to see if History. */
		IF dw_display_claim_info.GetItemString(1,'claim_history_flag') = 'Y' OR &
		   dw_display_claim_info.GetItemString(1, 'individual_history_flag') = 'Y' THEN
			MessageBox("Invalid Claim","History Claim does not allow for account payments.",Exclamation!)
		ELSE
			istr_window_message.al_doubleparm[1]		= 0
			istr_window_message.al_doubleparm[3]		= il_docid
			istr_window_message.al_doubleparm[4]		= il_service_provider_no
			istr_window_message.as_stringparm[1]		= is_document_type
			istr_window_message.adtm_datetimeparm[1]	= idtm_date_on_document
			istr_window_message.as_mode					= "inbasketadd"
			istr_window_message.al_doubleparm[5]      = 0
			istr_window_message.as_stringparm[2]		= ""
			
			SELECT administering_act_code
			INTO   :istr_window_message.as_stringparm[3]
			FROM   CLAIM
			WHERE  claim_no = :il_claim_no
			USING SQLCA;
			SQLCA.nf_handle_error('w_inbasket', 'cb_inbasket_doc_pay_acct.clicked', 'SELECT administering_act_code...')
			/* Open w_account_payment_maintenance
			*/
			wf_inbasket_account_pay()
		END IF
	END IF
end event

type cb_inbasket_refresh from commandbutton within w_inbasket
integer x = 5001
integer y = 300
integer width = 96
integer height = 80
integer taborder = 130
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&R"
end type

event clicked;dw_inbasket_category_select.PostEvent(ItemChanged!)


end event

type cb_inbasket_doc_delete from commandbutton within w_inbasket
integer x = 530
integer y = 1500
integer width = 375
integer height = 96
integer taborder = 190
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Delete Doc"
end type

event clicked;//  This script deletes the selected document

LONG       ll_results, ll_count, ll_rownum, ll_docid, ll_docfldid
STRING     ls_find



// Disable button to prevent second click
Enabled = False

// Make sure you can figure out what document they want to delete
ll_rownum = dw_inbasket.GetSelectedRow(0)
If ll_rownum = 0 Then
	MessageBox('Delete Document','Could not determine selected document to delete. Please try again.')
	Enabled = True
	Return
End If

// Get the document's id
ll_docid = dw_inbasket.GetItemNumber(ll_rownum,'docid')
If ll_docid = 0 Then
	MessageBox('Delete Document','Could not determine selected document to delete. Please try again.')
	Enabled = True
	Return
ELSE
	ll_docfldid = dw_inbasket.GetItemNumber(ll_rownum,'fldid')
End If

// Check that the document exists in more that one place
SELECT count(*)
INTO   :ll_count
FROM   REF
WHERE  docid = :ll_docid
USING ImageTrans;
ImageTrans.nf_handle_error('Embedded SQL: Select from REF','w_inbasket','clicked for cb_inbasket_doc_delete')

If ll_count = 1 Then
	MessageBox('Delete Document','Can not delete document because it does not exist anywhere else.')
	Enabled = True
	Return
End If


ImageTrans.nf_begin_transaction()

// remove reference for document in folder
DELETE REF
WHERE  docfldid = :ll_docfldid
AND    docid    = :ll_docid
USING ImageTrans;
ImageTrans.nf_handle_error('Embedded SQL: Update DOC','w_inbasket','clicked for cb_inbasket_doc_delete')

// Decrement the document reference counter
UPDATE DOC
SET docrefcount = :ll_count - 1
WHERE docid = :ll_docid
USING ImageTrans;
ImageTrans.nf_handle_error('Embedded SQL: Update DOC','w_inbasket','clicked for cb_inbasket_doc_delete')

ImageTrans.nf_commit_transaction()


dw_inbasket.SetRedraw(FALSE)

// refresh folders and documents
wf_inbasket_refresh_folder_list()

dw_inbasket.ScrollToRow(ll_rownum)
dw_inbasket.SelectRow(ll_rownum,TRUE)
dw_inbasket.SetRow(ll_rownum)

dw_inbasket.SetRedraw(TRUE)


/* Check added by Rob Head 98/09/17 */
If dw_inbasket.RowCount() = 0 Then
	wf_inbasket_setup('DOCUMENTS_NOT_FOUND')
End If

end event

type cb_inbasket_doc_view from commandbutton within w_inbasket
integer x = 1298
integer y = 1500
integer width = 306
integer height = 96
integer taborder = 210
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&View "
end type

event clicked;//  This script opens the document in the imaging viewer

Long ll_row_nmbr, ll_docid, ll_result
string ls_doc_type
integer li_rowsreturned
// Disable button to prevent double clicking
Enabled = False

// Get id of selected row
ll_row_nmbr = dw_inbasket.GetSelectedRow(0)
If ll_row_nmbr <= 0 Then
   MessageBox("InBasket","Error determining selected row. Please try again")
	Enabled = True
	Return
End If

// Get value of folder id field on selected row
ll_docid = dw_inbasket.GetItemNumber(ll_row_nmbr,"docid")
If ll_docid < 0 Then
   MessageBox('InBasket','Please select a folder that has a document.')
	Enabled = True
	Return
End If


ll_result = f_close_viewer()


if isvalid(uo_image_append) then
	
	/* check to see if new viewe should be called */
		if uo_image_append.of_init(ll_docid)	<= 0 then
			RETURN 
		end if
			
			
		ls_doc_type =  uo_image_append.of_get_file_type()
			
		
		CHOOSE CASE ls_doc_type
			/*  Imaged document */ 
			CASE 'IMA', 'TIF'
				uo_image_append.of_set_option()
				li_rowsreturned = uo_image_append.of_append_image(ll_docid) 
				if li_rowsreturned < 0 then
					return
				end if
			case else
				/* execel and word */
				li_rowsreturned = iw_active_sheet.iu_dw_document_path.f_manage_document(ll_docid,"V","NORMAL")
		end choose
	
		
		IF li_rowsreturned < 0 THEN
			return	
		END IF


end if

	

// Enable button
Enabled = True

end event

type cb_inbasket_doc_index from commandbutton within w_inbasket
integer x = 951
integer y = 1500
integer width = 306
integer height = 96
integer taborder = 200
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Index "
end type

on clicked;//  This script sets up document index information
//
//--------------------------------------------------------------------------

// Disable button to prevent second click
Enabled = False

// Display document indexing information
wf_inbasket_display_document_index()

// Enable the button
Enabled = True
end on

type dw_inbasket_category_select from u_dw_online within w_inbasket
integer x = 73
integer y = 192
integer width = 878
integer height = 92
integer taborder = 100
string dataobject = "d_cat_list_external"
boolean border = false
end type

event itemchanged;
IF uo_reference_search.ib_is_search_expanded THEN
	uo_reference_search.ib_is_search_expanded = FALSE
	uo_reference_search.gb_reference.Height = 153
	uo_reference_search.Height = 155
	uo_reference_search.BringToTop = FALSE
END IF


// clear filters
il_claim_view_filter_no = 0
is_row_filter = ''


//  Retrieve category id number of the selected category.
//  Place value in an instance variable for other functions
//  to use
il_category_id_nmbr = Long(GetText())


// If user selects category 2 then they can no longer do anything with it
If il_category_id_nmbr = 2 Then
	MessageBox ("In_Basket", "You may no longer access the CLAIMS MASTER FILE category from In-Basket.", STOPSIGN!, OK!)
	Return 1
Else
	// get array argument for retrieval
	wf_determine_cat_array()
	
   // Refresh Folder List
	is_row_filter = ""
   wf_inbasket_refresh_folder_list()
	cbx_claim_view.Checked = False
	
	//  This script sets up the display of the folder contents
	
	IF uo_reference_search.ib_is_search_expanded THEN
		uo_reference_search.ib_is_search_expanded = FALSE
		uo_reference_search.gb_reference.Height = 153
		uo_reference_search.Height = 155
		uo_reference_search.BringToTop = FALSE
	END IF
		
	// display folder details
	wf_inbasket_display_folder_details()
	
	IF dw_inbasket.RowCount() > 0 THEN
		dw_inbasket.Trigger Event RowFocusChanged(1)
	END IF
	
	// Enable button to prevent double clicking
	Enabled = True
	
End If

end event

event dberror;imagetrans.SQLDBCode = sqldbcode
imagetrans.SQLErrText = sqlerrtext
RETURN 1  // Don't display message
end event

type cb_inbasket_folder_delete from commandbutton within w_inbasket
integer x = 50
integer y = 1500
integer width = 434
integer height = 96
integer taborder = 270
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "D&elete Folder"
end type

event clicked;// Declare local variables
Long	ll_results,		ll_fldid
Int	li_rowcount, li_current_row

IF NOT IsValid(inv_imaging) THEN
	inv_imaging = Create n_imaging
END IF

// Disable button to prevent second click
Enabled = FALSE

// Ask user if they really want to delete the folder
If MessageBox("In-Basket", "Do you wish to delete the current folder?",Question!,YesNo!) = 2 Then
	GoTo Normal_Exit
End If

// Determine selected folder
ll_fldid = wf_inbasket_get_selected_fldid()
If ll_fldid = -1 OR ll_fldid = 0 Then
	Goto Normal_Exit	
End If

// Delete folder entry from imaging
ll_results = inv_imaging.nf_delete_workfolder(ll_fldid, False,"w_inbasket")
If ll_results < 1 Then
	GoTo Normal_Exit
End If

li_current_row = dw_inbasket.GetRow()

wf_inbasket_refresh_folder_list()


li_rowcount = dw_inbasket.RowCount()
IF li_rowcount > 0 THEN
	IF li_rowcount < li_current_row THEN
		li_current_row = li_rowcount
	END IF
	
	dw_inbasket.SelectRow(0,FALSE)
	dw_inbasket.SetRow(li_current_row)
	dw_inbasket.SelectRow(li_current_row,TRUE)
	dw_inbasket.ScrollToRow(li_current_row)
	
	IF is_opening_method = 'SEARCH' THEN
		iw_sheet.uo_claim_search.Trigger function uf_refresh_search()
	END IF
ELSE
	cb_event.Visible = FALSE
END IF


// End of script
Normal_Exit:
   Enabled = True
   Return
end event

type st_inbasket_forward_to from statictext within w_inbasket
integer x = 5353
integer y = 1148
integer width = 1147
integer height = 68
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Forward To:"
alignment alignment = center!
boolean focusrectangle = false
end type

type dw_inbasket_forward_to from u_dw_online within w_inbasket
integer x = 5353
integer y = 1236
integer width = 1147
integer height = 96
integer taborder = 180
string dataobject = "d_forward_cat_list_external"
boolean border = false
end type

event dberror;imagetrans.SQLDBCode = sqldbcode
imagetrans.SQLErrText = sqlerrtext
RETURN 1
end event

type cb_inbasket_folder_index_ok from commandbutton within w_inbasket
integer x = 6144
integer y = 1364
integer width = 375
integer height = 96
integer taborder = 230
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Save"
end type

event clicked;//  This script validates the changes made by the user and updates
//  the tables as required

// Declare local variables
Long     ll_results,	ll_fldid,	 ll_claim_no,	ll_process_folder_index_results, &
		   ll_process_folder_name_results,			ll_forward_to_category_id_nmbr, &
		   ll_selected_row
Integer  li_error_code
String   ls_forward_category_name, ls_keyword,	ls_local_status, &
		   ls_master_status,			 ls_folder_name, ls_temp
DateTime ldt_action_date

// Disable button to prevent second clicking
Enabled = False

// Initialize local variables
ll_process_folder_index_results = 0
ll_process_folder_name_results  = 0

// Get folderid of selected claim
ll_fldid = wf_inbasket_get_selected_fldid()
If ll_fldid = -1 OR ll_fldid = 0 Then
	Goto Normal_Exit
End If

// Process the folder index form or the folder name field depending on which one is active
IF Not vib_inbasket_folder_multi_claim THEN	
	
	inv_check_dw = Create n_check_dw
	ls_temp = inv_check_dw.nf_check_dw(dw_inbasket_folder_index, ImageTrans, ll_fldid, 'work folder', 'in-basket')
	IF ls_temp = '-1' THEN
		RETURN
	END IF
	
	IF len(trim(ls_temp) ) > 0 THEN
		MessageBox("Data Error",ls_temp)
		RETURN
	END IF
	
	ll_process_folder_index_results = wf_inbasket_process_folder_index(ll_fldid)
   If ll_process_folder_index_results = -1 THEN
      GoTo Normal_Exit
   END IF
ELSE
   ll_process_folder_name_results = wf_inbasket_process_folder_name(ll_fldid)
	IF ll_process_folder_name_results = -1 THEN
      GoTo Normal_Exit
   END IF
END IF

// Forward claim
IF Not vib_inbasket_folder_multi_claim THEN
   ll_forward_to_category_id_nmbr = dw_inbasket_forward_to.GetItemNumber(1,"catid")
Else
   SetNull(ll_forward_to_category_id_nmbr)
END IF

IF Not IsNull(ll_forward_to_category_id_nmbr) THEN

   // If destination and current category are the same then skip the forwarding of the claim
   IF ll_forward_to_category_id_nmbr = il_category_id_nmbr Or IsNull(ll_forward_to_category_id_nmbr) THEN 
		MessageBox("Folder Index Changes","Forwarding not performed as destination category same as current category. Please try again.")
      GOTO Normal_Exit
   End IF

   // Forward the claim
   wf_inbasket_forward_claim(ll_forward_to_category_id_nmbr, ll_fldid)
	
	// the 'envelope' icon should be invisible when the search list is refreshed
	IF is_opening_method = 'SEARCH' THEN
		iw_sheet.uo_claim_search.Trigger function uf_refresh_search()
	END IF
	
	parent.SetRedraw(FALSE)
	
	ll_selected_row = dw_inbasket.GetSelectedRow(0)
	
	wf_inbasket_refresh_folder_list()
	
	dw_inbasket.SetRow(ll_selected_row)
	dw_inbasket.SelectRow(ll_selected_row,TRUE)
	dw_inbasket.ScrollToRow(ll_selected_row)
	
	parent.SetRedraw(TRUE)
ELSE

   // If the folder index was processed then lookup the new folder name
   IF ll_process_folder_index_results = 1 THEN

      SELECT fldname
        INTO :ls_folder_name
        FROM FLD
       WHERE fldid = :ll_fldid
       USING ImageTrans;
		ImageTrans.nf_handle_error("Embedded SQL: Select FLD","w_inbasket","clicked for cb_inbasket_folder_index_ok")

   END IF


   // Get the currently selected row
   ll_selected_row =  dw_inbasket.GetSelectedRow(0)  
   IF ll_selected_row = 0 THEN
		IF dw_inbasket.RowCount() > 0 THEN
			MessageBox("Folder Index Changes","Could not determine selected folder. Please try again.")
			Goto Normal_Exit
		END IF
   END IF


   // Update the keyword and action date fields in the data window
   IF vib_inbasket_folder_multi_claim = False THEN
      ls_keyword = dw_inbasket_folder_index.GetItemString(1,"action_note")
      ldt_action_date = dw_inbasket_folder_index.GetItemDateTime(1,"action_date")

      dw_inbasket.SetItem(ll_selected_row,"action_note",ls_keyword)
      dw_inbasket.SetItem(ll_selected_row,"action_date",ldt_action_date)
   END IF
   
END IF

// Normal End of Function
Normal_Exit:
   Enabled = True

end event

type cb_inbasket_folder_index_cancel from commandbutton within w_inbasket
integer x = 5349
integer y = 1364
integer width = 375
integer height = 96
integer taborder = 220
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
end type

event clicked;//  This script cancels changes made to the folder index and
//  the forwarding of a claim folder

// Declare local variables
Long ll_results,	ll_fldid,	ll_selected_row

// Disable button to prevent second clicking
Enabled = False

// Get folder id of selected folder
ll_fldid = wf_inbasket_get_selected_fldid()
If ll_fldid = -1 OR ll_fldid = 0 Then
	Goto Exit_Script
End If	

// Redisplay screen setup and folder name field
cb_inbasket_folder_index_ok.enabled			= TRUE

// Get folder name from folder selected in folder list data winodw
ll_selected_row = dw_inbasket.GetSelectedRow(0)
IF ll_selected_row = 0 THEN
	IF dw_inbasket.RowCount() > 0 THEN
		MessageBox("Folder Index Cancel","Could not determine selected folder.  Please try selecting folder again.")
		Goto Exit_Script
	END IF
END IF

// Re - Retrieve folder index information for the folder
// Initialize forward to drop-down list
wf_inbasket_display_folder_index(ll_fldid)

Exit_Script:
	// Enable button
	Enabled = True
end event

type dw_display_claim_info from u_dw_online within w_inbasket
boolean visible = false
integer x = 110
integer y = 1684
integer width = 165
integer height = 100
integer taborder = 140
boolean enabled = false
string dataobject = "d_basic_claim_everything"
end type

type gb_inbasket_category_selection from groupbox within w_inbasket
boolean visible = false
integer x = 59
integer y = 140
integer width = 910
integer height = 156
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "In-Basket (Category)"
end type

type dw_inbasket_folder_index from u_dw_online within w_inbasket
integer x = 5275
integer y = 308
integer width = 1271
integer height = 812
integer taborder = 40
boolean bringtotop = true
string dataobject = "d_inbasket_work_folder_index"
end type

event itemchanged;call super::itemchanged;INTEGER	li_addondays
STRING	ls_adate
DATE		ld_bringfwddate, ld_actiondate
DATETIME ldt_6_months_in_future, ldt_18_months_in_future


SELECT Distinct DATEADD(mm, 18, GetDate())
INTO :ldt_18_months_in_future
FROM sysobjects
USING SQLCA;
SQLCA.nf_handle_error('w_inbasket','embedded SQL: SELECT Distinct DATEADD(mm, 18, GetDate()) FROM sysobjects...','dw_inbasket_folder_index.ItemChanged')
			
SELECT Distinct DATEADD(mm, 6, GetDate())
INTO :ldt_6_months_in_future
FROM sysobjects
USING SQLCA;
SQLCA.nf_handle_error('w_inbasket','embedded SQL: SELECT Distinct DATEADD(mm, 6, GetDate()) FROM sysobjects...','dw_inbasket_folder_index.ItemChanged')



//	Determine if the values entered are valid.
CHOOSE CASE GetColumnName()
	CASE "action_date"
		ls_adate = GetText()
		IF ls_adate = "" THEN
			SetNull(ld_actiondate)
		ELSE
			ld_actiondate = Date(Mid(GetText(),1,10))
		END IF
		
		IF NOT IsNull(ld_actiondate) THEN
			IF DaysAfter (Date(f_server_datetime()), ld_actiondate ) < 0 THEN
				MessageBox("Action Date",	"The action date is in the past." &
												+	"~n Please correct the date.",StopSign!)
				RETURN 1
			END IF
						
			IF DaysAfter (Date(ldt_18_months_in_future), ld_actiondate ) > 0 THEN
				MessageBox("Action Date",	"The action date is more than 18 months in the future." &
												+	"~n               Please correct the date.",StopSign!)
				RETURN 1
			END IF
						
			IF DaysAfter (Date(ldt_6_months_in_future), ld_actiondate ) > 0 THEN
				MessageBox("Action Date","The action date is more than 6 months in the future.",Exclamation!)
			END IF		
		END IF

	CASE "bring_fwd_day"
		li_addondays = Integer(GetText())
		IF li_addondays < 0 THEN
			MessageBox("Days","The value entered for 'Days' can not be less than 0.",Exclamation!)
			RETURN 1
		ELSE
			ld_bringfwddate = RelativeDate(Date(f_server_datetime()),li_addondays)
			IF DaysAfter (Date(ldt_18_months_in_future), ld_bringfwddate ) > 0 THEN
				MessageBox("Action Date",	"The bring forward date would more than 18 months in the future." &
												+	"~n               Please correct the date.",StopSign!)
				RETURN 1
			END IF
						
			IF DaysAfter (Date(ldt_6_months_in_future), ld_bringfwddate ) > 0 THEN
				MessageBox("Action Date","The bring forward date will be more than 6 months in the future.",Exclamation!)
			END IF
			
			THIS.SetItem(1,'bring_fwd_date',ld_bringfwddate)
		END IF

	CASE "bring_fwd_date"
		ls_adate = GetText()
		IF ls_adate = "" THEN
			SetNull(ld_bringfwddate)
		ELSE
			ld_bringfwddate = Date(Mid(GetText(),1,10))
		END IF
		
		IF NOT IsNull(ld_bringfwddate) THEN
			IF DaysAfter (Date(f_server_datetime()), ld_bringfwddate ) < 0 THEN
				MessageBox("Bring Forward Date",	"The bring forward date is in the past." &
															+	"~n          Please correct the date.",StopSign!)
				RETURN 1
			END IF
			
			IF DaysAfter (Date(ldt_18_months_in_future), ld_bringfwddate ) > 0 THEN
				MessageBox("Action Date",	"The bring forward is more than 18 months in the future." &
												+	"~n               Please correct the date.",StopSign!)
				RETURN 1
			END IF
						
			IF DaysAfter (Date(ldt_6_months_in_future), ld_bringfwddate ) > 0 THEN
				MessageBox("Action Date","The bring forward date is more than 6 months in the future.",Exclamation!)
			END IF
		END IF

END CHOOSE

end event

type cb_bfclear from commandbutton within w_inbasket
integer x = 5303
integer y = 956
integer width = 311
integer height = 108
integer taborder = 170
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Clear &BF"
end type

on clicked;DATETIME	ldt_nullvalue

SetNull(ldt_nullvalue)

dw_inbasket_folder_index.SetItem(1,'bring_fwd_date',ldt_nullvalue)
dw_inbasket_folder_index.SetItem(1,'bring_fwd_note',' ')

end on

type uo_reference_search from u_reference_search within w_inbasket
integer x = 1403
integer y = 140
integer taborder = 10
end type

on uo_reference_search.destroy
call u_reference_search::destroy
end on

type cb_inbasket_folder_unpaid_accts from commandbutton within w_inbasket
integer x = 4759
integer y = 1500
integer width = 407
integer height = 96
integer taborder = 320
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Unpaid Accts..."
end type

event clicked;LONG   				ll_valid_acct, ll_rownum
s_window_message	lstr_window_message



	iw_account_payment = w_account_payment

//	Create an instance of the payment user object

	inv_payment = Create n_payment
	inv_payment.nf_set_basic_claim(dw_display_claim_info)



/*	Retrieve claim information for the folder
*/
	il_claim_no 			= iw_sheet.dw_basic_claim.GetItemNumber(1,'claim_no')
	is_claim_status 		= iw_sheet.dw_basic_claim.GetItemString(1,'claim_status_code') 
	is_claim_status_type = iw_sheet.dw_basic_claim.GetItemString(1,'claim_status_type_code') 


/* Validate the claim meets the requirements for viewing Unpaid Accounts
*/
	ll_valid_acct =  wf_inbasket_validate_for_unpaid()

	ll_rownum = dw_inbasket.GetRow()
	IF ll_rownum > 0 THEN
		il_docid = dw_inbasket.GetItemNumber(ll_rownum,'docid')
		IF il_docid = 0 THEN
			MessageBox('InBasket','Please select a folder that has a document.')
			RETURN
		END IF

		IF wf_check_doc_index() < 0 THEN
			Return
		END IF		
	END IF

/* Now that the claim and document has been validated, open the Account Payment Window

*/
	IF ll_valid_acct = 0 THEN
		lstr_window_message.awi_parent_window = w_inbasket
		lstr_window_message.as_mode	= 'inbasketunpaid'
		lstr_window_message.al_doubleparm[2] = il_claim_no
		lstr_window_message.apo_powerobjectparm[1] = iw_sheet

	/*	Try to open the window and initialize.  If something happens in the initialization
		ensure the window is closed. Disable the InBasket Menu item if the Account Payment screen
		is opened.
	*/
		OpenWithParm(iw_account_payment,lstr_window_message,iw_sheet)
		IF IsValid(iw_account_payment) THEN
			wf_inbasket_set_menu_status(1)
			iw_account_payment.wf_set_parent_window(parent)
		END IF

	END IF
end event

type cb_inbasket_doc_reviewed from commandbutton within w_inbasket
integer x = 1646
integer y = 1500
integer width = 315
integer height = 96
integer taborder = 310
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Reviewed"
end type

event clicked;/*	This button stamps the DOCUMENT_INDEX record to show that the record has been reviewed
	by somebody. It does this by setting the reviewed_flag to 'Y'. It will also add a
	record to the Reviewed_Documents table to show who reviewed it.
*/
	INTEGER	li_messageanswer, li_existcount
	LONG		ll_currentrow, ll_docid, ll_result, ll_foundonrow
	STRING	ls_alreadyreviewed
	DATETIME	ldt_today

/*	Determine the current document.
*/
	ll_currentrow = dw_inbasket.GetSelectedRow(0)
	IF ll_currentrow = 0 THEN
		MessageBox('Reviewed Document Error','A document must first be selected before it can be stamped "Reviewed".')
	ELSE

/*	Check to see if the document has already been reviewed. If not, then continue to set it to 'reviewed'.
	Need to find in iw_sheet.dw_documents by document id.
*/
		ll_docid = dw_inbasket.GetItemNumber(ll_currentrow,'docid')
		IF ll_docid = 0 THEN
			MessageBox('InBasket','Please select a folder that has a document.')
			RETURN
		END IF
		
		
		long ll_rowcount
		string ls_find
		ll_rowcount = iw_sheet.dw_documents.RowCount()
		ls_find = 'ref_docid = ' + String(ll_docid)
		
		ll_foundonrow = iw_sheet.dw_documents.Find(ls_find,1,ll_rowcount)
		IF ll_foundonrow <= 0 THEN
			MessageBox('Reviewed Document Error','Unable to determine if the document was already reviewed.')
			RETURN
		END IF

		SELECT count(*)
		  INTO :li_existcount
		  FROM REVIEWED_DOCUMENTS
		 WHERE docid = :ll_docid
		 USING ImageTrans;
		ImageTrans.nf_handle_error('Embedded SQL: SELECT count(*) INTO :li_existcount','w_inbasket','cb_inbasket_doc_reviewed')

		IF li_existcount = 0 THEN

/*	Ask the user if they wish to continue with the stamping of the document.
*/
			li_messageanswer = MessageBox('Reviewed Document Stamp','Do you wish to stamp the selected document as having been reviewed.',Question!,OkCancel!)
			IF li_messageanswer = 2 THEN
				RETURN
			END IF
			
			ImageTrans.nf_begin_transaction()

/*	Insert a record into the Reviewed_Documents table.
*/
			ldt_today = f_server_datetime()
			INSERT INTO REVIEWED_DOCUMENTS
				(docid,reviewed_by_user_id,reviewed_date)
			VALUES
				(:ll_docid,:vgst_user_profile.user_id,:ldt_today)
			USING ImageTrans;

			ll_result = ImageTrans.nf_handle_error('Embedded SQL: INSERT INTO Reviewed_Documents','w_inbasket','cb_inbasket_doc_reviewed')
			IF ll_result < 0 THEN
				RETURN
			END IF

/*	Stamp the DOCUMENT_INDEX record.
*/
			UPDATE DOCUMENT_INDEX
				SET reviewed_flag = 'Y'
			 WHERE docid = :ll_docid
			 USING ImageTrans;

			ll_result = ImageTrans.nf_handle_error('Embedded SQL: UPDATE DOCUMENT_INDEX','w_inbasket','cb_inbasket_doc_reviewed')
			IF ll_result < 0 THEN
				RETURN
			END IF

			ImageTrans.nf_commit_transaction()
			

/*	Display the reviewed icon for the document.
*/
			iw_sheet.dw_documents.SetItem(ll_foundonrow,'reviewed_flag','Y')
		ELSE
			MessageBox('Reviewed Document Warning','The currently selected document has already been stamped as "Reviewed".')
		END IF
	END IF

end event

type uo_image_append from u_image_append within w_inbasket
boolean visible = false
integer x = 302
integer y = 1684
integer width = 165
integer height = 100
integer taborder = 260
boolean bringtotop = true
end type

on uo_image_append.destroy
call u_image_append::destroy
end on

type cbx_claim_view from checkbox within w_inbasket
integer x = 997
integer y = 192
integer width = 402
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Claim View"
end type

event clicked;INTEGER  li_rtn, li_row, li_find
LONG     ll_docid, ll_fldid, ll_claim_cnt
STRING   ls_find


PARENT.SetRedraw(FALSE)

// clear filter string
is_row_filter = ''

li_row = dw_inbasket.GetRow()

IF li_row > 0 THEN
	ll_docid = dw_inbasket.GetItemNumber(li_row,'docid')
	ll_fldid = dw_inbasket.GetItemNumber(li_row,'fldid')
ELSE
	ll_docid = 0
	ll_fldid = 0
END IF

IF THIS.Checked THEN
	IF is_opening_method = 'SEARCH' THEN
		il_claim_view_filter_no = il_selected_claim_no
	ELSE
		il_claim_view_filter_no = il_claim_no
	END IF
	IF il_claim_view_filter_no = 0 or IsNull(il_claim_view_filter_no) THEN
		MessageBox('Error','No claim number to filter on.')
		THIS.checked = FALSE
	ELSE
		If dw_inbasket.SetFilter('claim_no = ' + String(il_claim_view_filter_no)) = -1 THEN SignalError(-666,'Error setting claim filter')
		If dw_inbasket.Filter() = -1 THen SignalError(-666,'Error filtering for claim')
		vil_inbasket_filter_cnt = dw_inbasket.RowCount()
	END IF
Else
	IF dw_inbasket.SetFilter('') = -1 THEN SignalError(-666,'Error removing filter')
	IF dw_inbasket.Filter() = -1      THEN SignalError(-666,'Error removing filter')
	il_claim_view_filter_no = 0
	vil_inbasket_filter_cnt = dw_inbasket.RowCount()
END IF
	
IF vil_inbasket_filter_cnt = 0 THEN
	wf_inbasket_setup('FOLDERS_NOTFOUND')
ELSE
	wf_inbasket_setup('FOLDERS_FOUND')
	
	IF THIS.Checked THEN
		ll_fldid = dw_inbasket.GetItemNumber(1,'fldid')
		ll_docid = dw_inbasket.GetItemNumber(1,'docid')
		IF IsNull(ll_docid) THEN ll_docid = 0
	END IF
	
	// No documents found for the selected folder
	IF ll_docid = 0 THEN
		wf_inbasket_setup('DOCUMENTS_NOT_FOUND')
		// Declare folder index information
		li_rtn = wf_inbasket_display_folder_index(ll_fldid)
		
		ls_find = 'fldid = ' + String(ll_fldid)
		li_find = dw_inbasket.Find(ls_find,1,vil_inbasket_filter_cnt)				
	ELSE
		ls_find = 'fldid = ' + String(ll_fldid) + ' AND docid = ' + String(ll_docid)
		li_find = dw_inbasket.Find(ls_find,1,vil_inbasket_filter_cnt)
		
		wf_post_select_folder(ll_fldid)
	END IF
	
	IF li_find = 0 OR THIS.Checked THEN
		dw_inbasket.SelectRow(0,FALSE)
		li_find = 1
	END IF

	dw_inbasket.SetRow(li_find)
	dw_inbasket.SelectRow(li_find,TRUE)
	dw_inbasket.ScrollToRow(li_find)
	
END IF

IF is_opening_method = 'SEARCH' THEN
	ib_opening = FALSE
	wf_event_button_visibility(1)
END IF

PARENT.SetRedraw(TRUE)
end event

type cb_event from commandbutton within w_inbasket
boolean visible = false
integer x = 2542
integer y = 1500
integer width = 402
integer height = 96
integer taborder = 280
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Event"
end type

event clicked;DATE              ldt_null

S_WINDOW_MESSAGE    lstr_message

lstr_message.al_doubleparm[1]    = istr_inbasket_event.claim_no
lstr_message.al_doubleparm[2]    = istr_inbasket_event.individual_no
lstr_message.al_doubleparm[3]    = istr_inbasket_event.event_no

lstr_message.adt_DateParm[1]     = ldt_null  // accident_date
lstr_message.as_StringParm[1]    = 'C'       // claimant
lstr_message.as_StringParm[2] 	= ''        // last_name
lstr_message.as_StringParm[3] 	= ''        // given_names
lstr_message.as_StringParm[4]    = istr_inbasket_event.event_category_code // taken from Event_Category - Claim vs Individual
lstr_message.as_StringParm[5]    = ''        // taken from Event_Type - Merge Individual
lstr_message.as_StringParm[6]    = ''        // event_specific_code
lstr_message.as_StringParm[7]    = 'Y'       // allow parameter change
lstr_message.as_StringParm[8]    = 'N'       // add new event
lstr_message.as_StringParm[9]    = ''        // put message here?????
lstr_message.as_StringParm[10]   = 'Y'       // find event
lstr_message.as_StringParm[11]   = istr_inbasket_event.event_category_code // taken from Event_Category - Claim vs Individual - this one is for searching

OpenWithParm(w_event_log_response, lstr_message)
end event

type dw_inbasket from u_dw_online within w_inbasket
event ue_to_cra_requests ( )
event ue_to_formulary ( )
integer x = 37
integer y = 300
integer width = 5161
integer height = 1168
integer taborder = 110
string dataobject = "d_folders_and_documents"
boolean vscrollbar = true
end type

event ue_to_cra_requests();
w_sheet lw_active_sheet
LONG    ll_event_no, ll_row, ll_request_no, ll_claim_no
STRING  ls_event_note, ls_search_string, ls_check, ls_control
integer li_counter
Boolean lb_rtn

ls_search_string = "Request"
//"See Request"

/* grab the current window to put this child in */
lw_active_sheet = w_frame.GetActiveSheet()

IF NOT IsValid(lw_active_sheet) THEN RETURN

ll_row = THIS.GetRow()

IF ISNULL(ll_row) OR ll_row < 1 THEN RETURN

ll_claim_no     = THIS.GetItemNumber(ll_row,'claim_no')

/* grab the note and break it up so that we can get the request number */
ls_event_note = THIS.GetItemString(ll_row,"action_note")

IF IsNull(ls_event_note) OR trim(ls_event_note) = ""  THEN 
	IF MessageBox("No Match","The Request number could not be determined. Continue?",Question!,YesNo!) = 2 THEN RETURN
END IF 

/* make sure we have a match on the search string */
IF Match(Upper(ls_event_note),Upper(ls_search_string)) = FALSE THEN
	IF MessageBox("NO MATCH","The Request number could not be determined. Continue?",Question!,YesNo!) = 2 THEN RETURN	
END IF 

/* grab the request number from the datawindow */
FOR li_counter = 1 TO LEN(ls_event_note)
	ls_check = Mid(ls_event_note,li_counter,1)
	IF isnumber(ls_check) THEN ls_control = ls_control + ls_check
NEXT

ll_request_no = LONG(ls_control)

istr_window_message.as_stringparm[1] = 'INBASKET'
istr_window_message.al_doubleparm[1] = ll_claim_no
istr_window_message.al_doubleparm[2] = ll_request_no

/* open the formulary window */
OpenWithParm(w_income_requests,istr_window_message,lw_active_sheet)

end event

event ue_to_formulary();
w_sheet lw_active_sheet
LONG    ll_event_no, ll_row
STRING  ls_event_note, ls_search_string, ls_check, ls_control
integer li_counter
Boolean lb_rtn

ls_search_string = "Event"
//"See Claim Event"

/* grab the current window to put this child in */
lw_active_sheet = w_frame.GetActiveSheet()
IF not IsValid(lw_active_sheet) THEN RETURN

ll_row = this.getrow()
IF ISNULL(ll_row) OR ll_row < 1 THEN RETURN

/* grab the event note and break it up so that we can get the event number */
ls_event_note = THIS.getitemstring(ll_row,"action_note")
IF ISNULL(ls_event_note) OR trim(ls_event_note) = ""  THEN 
	IF MESSAGEBOX("NO MATCH","The Event number could not be determined. Continue?",question!,yesno!) = 2 THEN RETURN
END IF 

/* make sure we have a match on the search string */
IF Match(upper(ls_event_note),upper(ls_search_string)) = FALSE THEN
	IF MESSAGEBOX("NO MATCH","The Event number could not be determined. Continue?",question!,yesno!) = 2 THEN RETURN	
END IF 

/* grab the event number from the datawindow */
FOR li_counter = 1 TO LEN(ls_event_note)
	ls_check = Mid(ls_event_note,li_counter,1)
	IF isnumber(ls_check) THEN ls_control = ls_control + ls_check
NEXT

IF  m_cmwb.m_tools.m_abcc_tools.m_maintainformulary.enabled = True And m_cmwb.m_tools.m_abcc_tools.m_maintainformulary.visible = True Then
		istr_window_message.as_mode = "UPDATE"
ELSEIF  m_cmwb.m_tools.m_abcc_tools.m_maintainformularyreadonly.enabled = True And m_cmwb.m_tools.m_abcc_tools.m_maintainformularyreadonly.visible = True Then
		istr_window_message.as_mode = "READ"		
ELSE
	Messagebox("Access Denied", "You do not have proper security priveleges to open this window.~r~r" +&
				  "If you need to open this window, Please call the Helpdesk to get the proper security priveleges.", Exclamation!)
	Return
END IF

istr_window_message.al_doubleparm[1] = integer(ls_control)
/* open the formulary window */
openwithparm(w_maintain_formulary,istr_window_message,lw_active_sheet)

end event

event rowfocuschanged;call super::rowfocuschanged;// do not extend ancestor script


// reset local instance structure
istr_inbasket_event = istr_null

If CurrentRow <= 0 Then	Return


IF CurrentRow > 0 THEN
	wf_event_button_visibility(CurrentRow)	
END IF


IF is_opening_method = 'SEARCH' THEN
	// do nothing, il_selected_claim_no already set
	il_claim_no = il_selected_claim_no	
ELSE
	// Get the claim associated with the folder
	il_claim_no = GetItemNumber(CurrentRow, "claim_no")
	
	// Clear the workbench
	wf_inbasket_clear_workbench()	
END IF

If Not IsNull(il_claim_no) Then
	wf_set_claim(il_claim_no)
End If

cb_inbasket_folder_delete.Enabled = True

THIS.SelectRow(CurrentRow,TRUE)

wf_inbasket_display_folder_details()

end event

event rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup


/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	lm_popup.m_options.m_sort.visible = TRUE
	
	IF is_opening_method <> 'SEARCH' THEN
		// do not allow filter when opening from selected claim from search list- it is already being filtered
		lm_popup.m_options.m_filterlist.visible = TRUE
	END IF
	
	lm_popup.m_options.m_gotoformulary.visible = TRUE
	lm_popup.m_options.m_maintaincrarequests.visible = TRUE
	lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))

	Destroy lm_popup
end event

event ue_filter;call super::ue_filter;S_WINDOW_MESSAGE ls_message
long ll_results,	ll_counter,	ll_fldid

SetPointer(HourGlass!)

OpenWithParm(w_inbasket_filter, il_category_id_nmbr)

ls_message = Message.PowerObjectParm

vil_inbasket_filter_cnt = ls_message.al_doubleparm[1]
is_row_filter = ls_message.as_stringparm[1]
il_days_after = ls_message.al_doubleparm[2]
is_filter_choice = ls_message.as_stringparm[2]

// Check for CANCEL on response window
IF is_row_filter <> "" THEN

	// For specific filtering criteria options, determine the folder ids of folders that do not meet the criteria and
	// add to the filter statement so as to remove these rows. 
	IF is_filter_choice = "G" OR is_filter_choice = "H" THEN
			// Remove the folders that are under the "number of days old" that the user has specified.

			ll_counter = 1

			Do While ll_counter <= dw_inbasket.RowCount()
				If DaysAfter(Date(dw_inbasket.GetItemDateTime(ll_counter,"create_date")),Date(f_server_datetime())) < il_days_after Then
					ll_fldid = dw_inbasket.GetItemNumber(ll_counter,"folderid")
					is_row_filter = is_row_filter + " And folderid <> " + String(ll_fldid)
				End If
	
				ll_counter++
			Loop
	END IF

	// --------------------------------------
	//  Set filter on data window and filter
	// --------------------------------------
	cbx_claim_view.checked  = False
	il_claim_view_filter_no = 0
	
	dw_inbasket.SelectRow(0, False) /* get rid of selected rows so they won't come back when we unfilter */
	
	dw_inbasket.SetFilter(is_row_filter)
	dw_inbasket.Filter()

	// Set instance variable to number of active rows
	ll_results = dw_inbasket.RowCount()
	IF ll_results <> -1 THEN 
   	vil_inbasket_filter_cnt = ll_results
	ELSE
   	MessageBox("Inbasket Filter Folders","Error determining row count for folder list data window.")
	END IF

	IF dw_inbasket.GetSelectedRow(0) = 0 THEN
		IF dw_inbasket.RowCount() > 0 THEN
			dw_inbasket.trigger Event RowFocusChanged(1)
		END IF
		wf_inbasket_setup('REFRESH')
	END IF

END IF
end event

event doubleclicked;call super::doubleclicked;LONG   ll_row, ll_docid


ll_row = dw_inbasket.GetRow()

IF ll_row > 0 THEN
	ll_docid = dw_inbasket.GetItemNumber(ll_row,'docid')
	IF ll_docid > 0 THEN
		cb_inbasket_doc_view.PostEvent(Clicked!)
	END IF
END IF
end event

event retrieveend;call super::retrieveend;
If RowCount <= 0 Then	Return

// all filtered, do not continue
IF RowCount = THIS.FilteredCount() THEN RETURN



// Get the claim associated with the folder
IF is_opening_method = 'SEARCH' THEN
	// do nothing, il_selected_claim_no already set
	il_claim_no = il_selected_claim_no	
ELSE
	// Clear the workbench
	wf_inbasket_clear_workbench()
	
	// Get the claim associated with the folder
	IF THIS.GetRow() > 0 THEN
		il_claim_no = GetItemNumber(1, "claim_no")	
	ELSE
		il_claim_no = 0
	END IF
END IF

If Not IsNull(il_claim_no) Then
	wf_set_claim(il_claim_no)
End If

cb_inbasket_folder_delete.Enabled = True

dw_inbasket.SetRow(1)
dw_inbasket.SelectRow(1,TRUE)

wf_inbasket_display_folder_details()

end event

type ln_height_sizer from line within w_inbasket
boolean visible = false
long linecolor = 33554432
integer linethickness = 4
integer endy = 1852
end type

type ln_width_sizer from line within w_inbasket
boolean visible = false
long linecolor = 33554432
integer linethickness = 4
integer endx = 6610
end type

