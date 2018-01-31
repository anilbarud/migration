$PBExportHeader$w_inbasket_old.srw
$PBExportComments$Inbasket Module
forward
global type w_inbasket_old from w_a_tool
end type
type dw_account_payment_list from u_dw_online within w_inbasket_old
end type
type cb_inbasket_doc_reject_acct from commandbutton within w_inbasket_old
end type
type dw_account_payment_paid_status from u_dw_online within w_inbasket_old
end type
type dw_claimsmaster_ref_count from u_dw_online within w_inbasket_old
end type
type cb_inbasket_doc_pay_acct from commandbutton within w_inbasket_old
end type
type cb_inbasket_refresh from commandbutton within w_inbasket_old
end type
type cb_inbasket_doc_delete from commandbutton within w_inbasket_old
end type
type cb_inbasket_doc_view from commandbutton within w_inbasket_old
end type
type st_inbasket_folder_name from statictext within w_inbasket_old
end type
type cb_inbasket_doc_index from commandbutton within w_inbasket_old
end type
type dw_inbasket_category_select from u_dw_online within w_inbasket_old
end type
type cb_inbasket_folder_prev from commandbutton within w_inbasket_old
end type
type cb_inbasket_folder_next from commandbutton within w_inbasket_old
end type
type sle_inbasket_folder_name from singlelineedit within w_inbasket_old
end type
type cb_inbasket_folder_delete from commandbutton within w_inbasket_old
end type
type st_inbasket_forward_to from statictext within w_inbasket_old
end type
type dw_inbasket_forward_to from u_dw_online within w_inbasket_old
end type
type cb_inbasket_folder_index_ok from commandbutton within w_inbasket_old
end type
type cb_inbasket_folder_index_cancel from commandbutton within w_inbasket_old
end type
type dw_inbasket_document_list from u_dw_online within w_inbasket_old
end type
type dw_display_claim_info from u_dw_online within w_inbasket_old
end type
type dw_inbasket_folder_list from u_dw_online within w_inbasket_old
end type
type gb_inbasket_details_doc from groupbox within w_inbasket_old
end type
type gb_inbasket_details_wrkfld from groupbox within w_inbasket_old
end type
type gb_inbasket_sort_by from groupbox within w_inbasket_old
end type
type gb_inbasket_category_selection from groupbox within w_inbasket_old
end type
type dw_inbasket_folder_index from u_dw_online within w_inbasket_old
end type
type cb_bfclear from commandbutton within w_inbasket_old
end type
type uo_reference_search from u_reference_search within w_inbasket_old
end type
type cb_inbasket_folder_unpaid_accts from commandbutton within w_inbasket_old
end type
type cb_inbasket_doc_reviewed from commandbutton within w_inbasket_old
end type
type cb_inbasket_folder_details from commandbutton within w_inbasket_old
end type
type cb_inbasket_folder_list from commandbutton within w_inbasket_old
end type
type uo_image_append from u_image_append within w_inbasket_old
end type
type cbx_claim_view from checkbox within w_inbasket_old
end type
end forward

global type w_inbasket_old from w_a_tool
integer width = 3186
integer height = 1796
boolean resizable = false
event ue_post_refreshdoc pbm_custom01
event ue_enable_inbasket_menu pbm_custom02
dw_account_payment_list dw_account_payment_list
cb_inbasket_doc_reject_acct cb_inbasket_doc_reject_acct
dw_account_payment_paid_status dw_account_payment_paid_status
dw_claimsmaster_ref_count dw_claimsmaster_ref_count
cb_inbasket_doc_pay_acct cb_inbasket_doc_pay_acct
cb_inbasket_refresh cb_inbasket_refresh
cb_inbasket_doc_delete cb_inbasket_doc_delete
cb_inbasket_doc_view cb_inbasket_doc_view
st_inbasket_folder_name st_inbasket_folder_name
cb_inbasket_doc_index cb_inbasket_doc_index
dw_inbasket_category_select dw_inbasket_category_select
cb_inbasket_folder_prev cb_inbasket_folder_prev
cb_inbasket_folder_next cb_inbasket_folder_next
sle_inbasket_folder_name sle_inbasket_folder_name
cb_inbasket_folder_delete cb_inbasket_folder_delete
st_inbasket_forward_to st_inbasket_forward_to
dw_inbasket_forward_to dw_inbasket_forward_to
cb_inbasket_folder_index_ok cb_inbasket_folder_index_ok
cb_inbasket_folder_index_cancel cb_inbasket_folder_index_cancel
dw_inbasket_document_list dw_inbasket_document_list
dw_display_claim_info dw_display_claim_info
dw_inbasket_folder_list dw_inbasket_folder_list
gb_inbasket_details_doc gb_inbasket_details_doc
gb_inbasket_details_wrkfld gb_inbasket_details_wrkfld
gb_inbasket_sort_by gb_inbasket_sort_by
gb_inbasket_category_selection gb_inbasket_category_selection
dw_inbasket_folder_index dw_inbasket_folder_index
cb_bfclear cb_bfclear
uo_reference_search uo_reference_search
cb_inbasket_folder_unpaid_accts cb_inbasket_folder_unpaid_accts
cb_inbasket_doc_reviewed cb_inbasket_doc_reviewed
cb_inbasket_folder_details cb_inbasket_folder_details
cb_inbasket_folder_list cb_inbasket_folder_list
uo_image_append uo_image_append
cbx_claim_view cbx_claim_view
end type
global w_inbasket_old w_inbasket_old

type variables
boolean		vib_inbasket_folder_multi_claim
boolean 		ib_imaged_view
datetime                idtm_date_on_document
long		vil_inbasket_folder_cnt
long		vil_inbasket_filter_cnt
long                      il_docid
long                      il_service_provider_no
long                      il_claim_no
long                      il_selected_claim_no
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

STRING    is_opening_method
end variables

forward prototypes
public function integer wf_inbasket_folder_list_move (string as_direction)
public subroutine wf_inbasket_get_doc_info ()
public function integer wf_retrieve_claim (long al_claim_no)
public function integer wf_set_claim (long al_claim_no)
public function integer wf_validate_acct_doc ()
private subroutine wf_inbasket_clear_workbench ()
private function integer wf_inbasket_display_folder_index (long al_fldid)
private function integer wf_inbasket_forward_claim (long al_catid, long al_fldid)
private function long wf_inbasket_get_selected_fldid ()
private function long wf_inbasket_hide_all ()
private function integer wf_inbasket_process_folder_index (long al_fldid)
private function integer wf_inbasket_process_folder_name (long val_fldid)
private function long wf_inbasket_remove_folder_from_list ()
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
public subroutine wf_set_selected_claim (long al_selected_claim_no)
end prototypes

event ue_post_refreshdoc;call super::ue_post_refreshdoc;LONG 	ll_fldid
	

// Refresh the document list
IF dw_inbasket_document_list.RowCount() > 0 THEN
	ll_fldid = dw_inbasket_document_list.GetItemNumber(1,"ref_docfldid")

	dw_inbasket_document_list.Retrieve(ll_fldid)
	If ImageTrans.nf_handle_error("dw_inbasket_document_list","w_inbasket","clicked for cb_inbasket_doc_refresh") < 0 Then
		Return
	End If

	dw_inbasket_document_list.SetRow(1)
	dw_inbasket_document_list.uf_processselect(1,"Keyboard")
END IF
end event

on ue_enable_inbasket_menu;call w_a_tool::ue_enable_inbasket_menu;/* Enable the InBasket Menu Item
*/
wf_inbasket_set_menu_status(0)
end on

public function integer wf_inbasket_folder_list_move (string as_direction);//  This function moves one folder forward or backward in the folder list
//  datawindow

// Declare local variables
Long	ll_row_nmbr,	ll_fldid,	ll_results,		ll_claim_no

// Determine the row currently highlighted
ll_row_nmbr = dw_inbasket_folder_list.GetSelectedRow(0)
IF ll_row_nmbr = 0 THEN
	MessageBox("Move in Folder List","Could not determine selected folder. Please try selecting it again.")
	Goto Normal_Exit
END IF

// Verify that it is possible to move in the correct direction.  This should never happen due to button disabling
IF (ll_row_nmbr = 1 and as_direction = "PREV") OR (ll_row_nmbr = dw_inbasket_folder_list.RowCount() and as_direction = "NEXT") THEN
   GOTO Normal_Exit
END IF

// Determine the row number of the next row to go to 
IF as_direction = "NEXT" THEN
   ll_row_nmbr = ll_row_nmbr + 1
ELSE
   ll_row_nmbr = ll_row_nmbr - 1
END IF

// Select the new row
dw_inbasket_folder_list.SetRow(ll_row_nmbr)

// Scroll to the new row
dw_inbasket_folder_list.ScrollToRow(ll_row_nmbr)

IF as_direction = "NEXT" and cb_inbasket_folder_prev.Enabled = FALSE THEN
   cb_inbasket_folder_prev.Enabled = TRUE
END IF

IF as_direction = "PREV" and cb_inbasket_folder_next.Enabled = FALSE THEN
   cb_inbasket_folder_next.Enabled = TRUE
END IF

// Highlight the row and display the details
dw_inbasket_folder_list.uf_processselect(ll_row_nmbr,"Keyboard")

// Clear the workbench
wf_inbasket_clear_workbench()

// Get the claim associated with the folder
ll_claim_no = dw_inbasket_folder_list.GetItemNumber(ll_row_nmbr,"claim_no")
 
wf_set_claim(ll_claim_no)

cb_inbasket_folder_details.Enabled = True

dw_inbasket_folder_list.TriggerEvent(DoubleClicked!)

// Normal exit from script
Normal_exit:
   If dw_inbasket_folder_list.visible = False THEN
      IF ll_row_nmbr = 1 and cb_inbasket_folder_prev.enabled = TRUE THEN
         cb_inbasket_folder_prev.enabled = FALSE
      END IF
      IF ll_row_nmbr = dw_inbasket_folder_list.RowCount() and cb_inbasket_folder_next.enabled = TRUE THEN
         cb_inbasket_folder_next.enabled = FALSE
      END IF
   END IF
   RETURN 1

end function

public subroutine wf_inbasket_get_doc_info ();LONG     ll_rownum, ll_docid,	ll_claim_no, ll_rowcount, ll_cntr, ll_account_payment_list_rownum
STRING   ls_folder_list
INTEGER  li_return_value
BOOLEAN  lb_payment_allowed = True,  lb_link_allowed = True


	SetPointer(HourGlass!)

/* Get the row number of the document selected from the document list
*/
	ll_rownum = dw_inbasket_document_list.GetRow()

/*	Get the document Identifier, document type, service provier and document date of 
	the selected document
*/
	il_docid = dw_inbasket_document_list.GetItemNumber(ll_rownum,"ref_docid")
	is_document_type = dw_inbasket_document_list.GetItemString(ll_rownum, "type_code")
	il_service_provider_no = dw_inbasket_document_list.GetItemNumber(ll_rownum,"document_index_service_provider_no")
	idtm_date_on_document = dw_inbasket_document_list.GetItemDateTime(ll_rownum,"date_on_document")


/*	Retrieve claim information on the document.
*/
	dw_display_claim_info.Reset()
	dw_display_claim_info.InsertRow(0)

	il_claim_no = dw_inbasket_document_list.GetItemNumber(ll_rownum,"claim_no")
	IF IsNull(il_claim_no) THEN il_claim_no = 0

/* The user cannot select the Claim Master Category through the In-Basket: therefore, must
	always retrieve the claim information for this claim as the claim changes with the document selected
*/
	IF il_claim_no > 0 THEN
		li_return_value = wf_retrieve_claim(il_claim_no)
		IF li_return_value = 0 THEN
			MessageBox("InBasket","Error Validating Claim Number. Please verify indexing on this document",Exclamation!)
		ELSE
			is_claim_status 		= dw_display_claim_info.GetItemString(1,"claim_status_code") 
			is_claim_status_type = dw_display_claim_info.GetItemString(1,"claim_status_type_code") 
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
Integer	li_error_code

ll_row_nmbr = dw_inbasket_folder_index.Retrieve(al_fldid)

li_error_code  = ImageTrans.nf_handle_error("dw_inbasket_folder_index","w_inbasket","wf_inbasket_display_folder_index")
If li_error_code < 0 Then

	// Change screen display
	wf_inbasket_setup("DETAILS_HIDDEN")
	wf_inbasket_setup("FOLDER_LIST_VISIBLE")

	// If all folders have been forwarded or deleted then no folders available
	IF vil_inbasket_filter_cnt = 0 THEN
		wf_inbasket_setup("FOLDERS_NOTFOUND")
	END IF

	Return -1
End If

// Folder does not exist
If ll_row_nmbr = 0 Then

	MessageBox("In-Basket", "Some one has deleted the current work folder.  Please refresh the In-Basket.", INFORMATION!)
   
	// Change screen display
	wf_inbasket_setup("DETAILS_HIDDEN")
	wf_inbasket_setup("FOLDER_LIST_VISIBLE")

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
ll_index_fldid = dw_inbasket_folder_index.GetItemNumber(1,"folderid")

// Insert row into data window to create buffer to create workfolder entry
If IsNull(ll_index_fldid) Then

   // Setup the defaults
	ll_claim_no = dw_inbasket_document_list.GetItemNumber(1,"claim_no")
	dw_inbasket_folder_index.SetItem(1,"claim_no",ll_claim_no)
End If

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

// Delete folder entry from folder list datawindow
wf_inbasket_remove_folder_from_list()

Return 1
end function

private function long wf_inbasket_get_selected_fldid ();//  This function returns the folder id of the selected folder in the folder
//  list window.
//  Return Values:    fldid of the selected folder

// Declare local variables
long ll_fldid,		ll_row_nmbr

// Get id of selected row
ll_row_nmbr = dw_inbasket_folder_list.GetSelectedRow(0)
IF ll_row_nmbr = 0 THEN
	MessageBox("Get Selected Folder","Could not determine which folder is selected.  Please try selecting it again.")
	Return -1
END IF

// Get value of folder id field on selected row
ll_fldid = dw_inbasket_folder_list.GetItemNumber(ll_row_nmbr,"fldid")
IF ll_fldid <= 0 OR IsNUll(ll_fldid) THEN
   MessageBox("Inbasket Folder Id","Error determining folder id of selected row in folder list.")
	Return -1
END IF

// End of function
RETURN ll_fldid
end function

private function long wf_inbasket_hide_all ();//  This function disables and/ or hides all window objects
//  except the CLOSE button.  It is used if a user does not have
//  access or if a fatal application error occurs but the workbench
//  is to be left open.
//  Return Values:    1 - Successful
//                   -1 - Failure

// Declare local varaibles
Integer vli_return_code,	vli_error_code
String  vls_check_action,  vls_folder_action

// Initialize local variables
vli_return_code = 1

// Hide folder details
IF sle_inbasket_folder_name.visible = True THEN
   wf_inbasket_setup("DETAILS_HIDDEN")
END IF


// If folder list visible then screen display is folder list.
IF dw_inbasket_folder_list.visible = TRUE THEN
   wf_inbasket_setup("FOLDER_LIST_HIDDEN")
END IF


// Hide command buttons across the button of the screen
cb_inbasket_folder_delete.visible = False
cb_inbasket_folder_details.visible = False
cb_inbasket_folder_list.visible = False

RETURN 1
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

	li_error_code  = ImageTrans.nf_handle_error("cb_inbasket_folder_index_ok","w_inbasket","Validate claim number.")
	IF li_error_code < 0 THEN
		Return -1
	END IF

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
SET fldname = Upper(:ls_action) + CONVERT(varchar(10),:ll_claim_no) + :ls_claimant_name
WHERE fldid = :al_fldid
USING ImageTrans;

If ImageTrans.nf_handle_error("Embedded SQL: Update FLD","w_inbasket","wf_inbasket_process_folder_index") < 0 Then
	Return -1
End If

ImageTrans.nf_commit_transaction()


Return 1
end function

private function integer wf_inbasket_process_folder_name (long val_fldid);//  This function processes changes to the folder name sle field

// Declare local variables
Integer vli_error_code
Integer vli_function_return_code

vli_function_return_code = 1


ImageTrans.nf_begin_transaction()

// Update folder name
UPDATE FLD
   SET fldname = :sle_inbasket_folder_name.Text
 WHERE fldid   = :val_fldid
 USING ImageTrans;

 vli_error_code  = imagetrans.nf_handle_error("wf_inbasket_process_folder_name","w_inbasket","Update folder name")
 IF vli_error_code < 0 THEN
    vli_function_return_code = -1
    GoTo Normal_Exit
 END IF

// Commit the transaction
ImageTrans.nf_commit_transaction()


// End of function
Normal_Exit:
   RETURN vli_function_return_code

end function

private function long wf_inbasket_remove_folder_from_list ();//  This function removes a folder from the list and selects the next folder
//--------------------------------------------------------------------------

// Declare local variables
Long	ll_results,		ll_delete_row,		ll_selected_row

// Get id of selected row
ll_delete_row = dw_inbasket_folder_list.GetSelectedRow(0)
IF ll_delete_row = 0 THEN
	MessageBox("Delete Folder","Could not determine folder to delete.  Please try selecting it again.")
	Goto Exit_Function	
END IF


// IF number of rows is not 1 THEN move forward or backward in the list.  
// Otherwise exit
IF vil_inbasket_filter_cnt <> 1 THEN

   // If not last row then move to next row else move to previous row
   ll_results = 1
   If ll_delete_row = vil_inbasket_filter_cnt THEN
      ll_results = wf_inbasket_folder_list_move("PREV")
   ELSE
      ll_results = wf_inbasket_folder_list_move("NEXT")
   END IF

   If ll_results = -1 Then
      MessageBox("In-Basket", "Error moving to new folder in list.  Please exit and re-enter the In-Basket.", STOPSIGN!, OK!)
      wf_inbasket_hide_all()
      Goto Exit_Function
   End If     

   // Get selected row
   ll_selected_row = dw_inbasket_folder_list.GetSelectedRow(0)
   IF ll_selected_row = 0 THEN
      Goto Exit_Function
   END IF

END IF

// Delete row from u_dw_online
dw_inbasket_folder_list.DeleteRow(ll_delete_row)


// If the row to be deleted is earlier in the folder list than the new selected row
// Then subtract one from the new selected row
IF ll_delete_row < ll_selected_row THEN 
   ll_selected_row = ll_selected_row - 1
END IF


// Select the new row
IF vil_inbasket_filter_cnt <> 1 THEN
   ll_results = dw_inbasket_folder_list.SelectRow(ll_selected_row,TRUE)
   IF ll_results = -1 THEN
      MessageBox("In-Basket", "Error selecting row in folder list after delete.",STOPSIGN!,OK!)
		Goto Exit_Function
   END IF
END IF



// Update instance counters of available information
vil_inbasket_filter_cnt = vil_inbasket_filter_cnt - 1
vil_inbasket_folder_cnt = vil_inbasket_folder_cnt - 1


// -----------------------------------------------------------------------------
// If no more folders to process then move user back to the folder list display
// ------------------------------------------------------------------------------
// If all folders have been forwarded or deleted then no folders available
IF vil_inbasket_filter_cnt = 0 THEN

   // Change screen display
   wf_inbasket_setup("DETAILS_HIDDEN")
   wf_inbasket_setup("FOLDER_LIST_VISIBLE")
   wf_inbasket_setup("FOLDERS_NOTFOUND")

END IF


// End of function
Exit_Function:
	RETURN 1
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
	 
	 IF IMAGETRANS.nf_handle_error('Embedded SQL: select from doc index','w_inbasket','cb_inbasket_doc_pay_acct') < 0 THEN
		Return -1
	END IF
	IF ll_rowcount < 1 THEN
		MessageBox('Claim Error', 'The claim number for this document does not match the indexing information.  Please call the help desk.')
		Return -1
	END IF
	
Return 0


end function

private function integer wf_inbasket_refresh_folder_list ();//  This function repopulates the folder list from the selected category

//  Set redraw off to stop double display of data
// -----------------------------------------------
dw_inbasket_folder_list.SetRedraw(False)

dw_inbasket_folder_list.SetFilter("")
dw_inbasket_folder_list.Filter()

dw_inbasket_folder_list.Retrieve(il_category_id_nmbr)
If SQLCA.nf_handle_error("dw_inbasket_refresh_folder_list","w_inbasket","wf_inbasket_refresh_folder_list") < 0 Then
	dw_inbasket_folder_list.SetRedraw(False)
	Return -1
End If

If dw_inbasket_folder_list.RowCount() = 0 Then
	wf_inbasket_setup("FOLDERS_NOTFOUND")
End If

// Put folder count into instance variable
vil_inbasket_folder_cnt = dw_inbasket_folder_list.RowCount()

// ------------------------------------------
//  Apply filter to retrieved rows
// ------------------------------------------
IF vil_inbasket_folder_cnt > 0 THEN

	dw_inbasket_folder_list.SetFilter(is_row_filter)
	dw_inbasket_folder_list.Filter()

	IF dw_inbasket_folder_list.RowCount() <> -1 THEN 
   	vil_inbasket_filter_cnt = dw_inbasket_folder_list.RowCount()
	ELSE
   	MessageBox("Inbasket Filter Folders","Error determining row count for folder list data window.")
	END IF

	// -----------------------
	//  Set up screen objects
	// -----------------------
	If vil_inbasket_filter_cnt > 0 THEN
   	wf_inbasket_setup("FOLDERS_FOUND")
	ELSE
   	wf_inbasket_setup("FOLDERS_NOTFOUND")
	END IF

ELSE
   // Number of rows after filtering is zero if no rows found on retrieve
   vil_inbasket_filter_cnt = 0
END IF

// --------------------
//  Set redraw back on
// --------------------
dw_inbasket_folder_list.SetRedraw(True)

// -------------------------
//  Normal exit of function
// -------------------------

// -----------------
//  Sort datawindow
// -----------------
dw_inbasket_folder_list.Sort()

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
ll_docid = dw_inbasket_document_list.getitemnumber(dw_inbasket_document_list.getrow(),'ref_docid')
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
ll_docid = dw_inbasket_document_list.getitemnumber(dw_inbasket_document_list.getrow(),'ref_docid')
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

private function integer wf_inbasket_setup (string vas_inbasket_state);// Declare local variables
Long vll_row_selected

CHOOSE CASE vas_inbasket_state

   CASE "DETAILS_HIDDEN"
		gb_inbasket_details_doc.visible           = FALSE
		gb_inbasket_details_wrkfld.visible        = FALSE
      cb_inbasket_doc_view.visible					= False
		cb_inbasket_doc_reviewed.visible				= False
      cb_inbasket_doc_index.visible					= False
      cb_inbasket_doc_delete.visible				= False
      cb_inbasket_doc_pay_acct.visible				= False
		cb_inbasket_doc_reject_acct.visible			= False
 		cb_inbasket_folder_unpaid_accts.visible	= False
      cb_inbasket_folder_index_cancel.visible	= False
      cb_inbasket_folder_delete.visible		   = FALSE
      cb_inbasket_folder_list.visible				= False
      cb_inbasket_folder_next.visible				= False
      cb_inbasket_folder_index_ok.visible			= False
      cb_inbasket_folder_prev.visible				= False
      dw_inbasket_document_list.visible			= False
      dw_inbasket_folder_index.visible				= False
      dw_inbasket_forward_to.visible				= False
      sle_inbasket_folder_name.visible				= False
      st_inbasket_forward_to.visible				= False
      st_inbasket_folder_name.visible				= False
		cb_bfclear.visible								= False
		uo_reference_search.visible					= True

   CASE "DETAILS_OPEN"
      // Determine which folder row is selected
      vll_row_selected = dw_inbasket_folder_list.GetSelectedRow(0)

      cb_inbasket_doc_view.enabled 					= False
		cb_inbasket_doc_reviewed.enabled				= False
      cb_inbasket_doc_index.enabled 				= False
      cb_inbasket_doc_delete.enabled 				= False
      cb_inbasket_doc_pay_acct.enabled 			= False
      cb_inbasket_doc_reject_acct.enabled 		= False
  	 	cb_inbasket_folder_unpaid_accts.enabled 	= True
      cb_inbasket_folder_index_cancel.enabled 	= True
      cb_inbasket_folder_delete.enabled		   = TRUE
      cb_inbasket_folder_list.enabled 				= True

      IF vll_row_selected <> vil_inbasket_filter_cnt THEN
         cb_inbasket_folder_next.enabled 			= True
      ELSE
         cb_inbasket_folder_next.enabled 			= False
      END IF
      cb_inbasket_folder_index_ok.enabled 		= True
      IF vll_row_selected <> 1 THEN
         cb_inbasket_folder_prev.enabled 			= True
      ELSE
         cb_inbasket_folder_prev.enabled 			= False
      END IF
      dw_inbasket_document_list.enabled 			= False
      dw_inbasket_folder_index.enabled 			= True
      dw_inbasket_forward_to.enabled 				= True
      sle_inbasket_folder_name.borderstyle 		= StyleBox!
      sle_inbasket_folder_name.DisplayOnly 		= True
      sle_inbasket_folder_name.BackColor			= 67108864
      sle_inbasket_folder_name.enabled 			= True
      st_inbasket_forward_to.enabled 				= True
      st_inbasket_folder_name.enabled 				= True
		cb_bfclear.enabled								= True
		uo_reference_search.visible					= False
 
   CASE "DETAILS_VISIBLE"
		gb_inbasket_details_doc.visible           = TRUE
		gb_inbasket_details_wrkfld.visible        = TRUE
      cb_inbasket_doc_view.visible					= True
		cb_inbasket_doc_reviewed.visible				= True
      cb_inbasket_doc_index.visible					= True
      cb_inbasket_doc_delete.visible				= True
      cb_inbasket_doc_pay_acct.visible				= True
      cb_inbasket_doc_reject_acct.visible			= True
      cb_inbasket_folder_unpaid_accts.visible	= True
      cb_inbasket_folder_index_cancel.visible	= True
      cb_inbasket_folder_delete.visible		   = TRUE
      cb_inbasket_folder_list.visible				= True
      cb_inbasket_folder_next.visible				= True
      cb_inbasket_folder_index_ok.visible			= True
      cb_inbasket_folder_prev.visible				= True
      dw_inbasket_document_list.visible 			= True
      dw_inbasket_folder_index.visible 			= True
      dw_inbasket_forward_to.visible 				= True
      sle_inbasket_folder_name.borderstyle 		= StyleBox!
      sle_inbasket_folder_name.DisplayOnly 		= True
      sle_inbasket_folder_name.BackColor			= 67108864
      sle_inbasket_folder_name.visible 			= True
      st_inbasket_forward_to.visible 				= True
      st_inbasket_folder_name.visible 				= True
		cb_bfclear.visible								= True
		uo_reference_search.visible					= False

   CASE "DOCUMENT_NOT_SELECTED"
      cb_inbasket_doc_view.enabled 					= False
		cb_inbasket_doc_reviewed.enabled				= False
      cb_inbasket_doc_index.enabled 				= False
      cb_inbasket_doc_delete.enabled 				= False
      cb_inbasket_doc_pay_acct.enabled 			= False
      cb_inbasket_doc_reject_acct.enabled 		= False

   CASE "DOCUMENT_SELECTED"
      cb_inbasket_doc_view.enabled 					= True
		cb_inbasket_doc_reviewed.enabled				= True
      cb_inbasket_doc_index.enabled		 			= True
      cb_inbasket_doc_delete.enabled	 			= True
      cb_inbasket_doc_pay_acct.enabled	 			= True
      cb_inbasket_doc_reject_acct.enabled	 		= True

   CASE "DOCUMENTS_FOUND"
      cb_inbasket_doc_view.enabled 					= False
		cb_inbasket_doc_reviewed.enabled				= False
      cb_inbasket_doc_index.enabled 				= False
      cb_inbasket_doc_delete.enabled 				= False
      cb_inbasket_doc_pay_acct.enabled 			= False
      cb_inbasket_doc_reject_acct.enabled 		= False
      dw_inbasket_document_list.enabled 			= True

   CASE "DOCUMENTS_NOT_FOUND"
      cb_inbasket_doc_view.enabled 					= False
		cb_inbasket_doc_reviewed.enabled				= False
      cb_inbasket_doc_index.enabled 				= False
      cb_inbasket_doc_delete.enabled 				= False
      cb_inbasket_doc_pay_acct.enabled 			= False
      cb_inbasket_doc_reject_acct.enabled 		= False
      dw_inbasket_document_list.enabled 			= False

   CASE "FLDINDEX_HIDDEN"
      st_inbasket_forward_to.visible 				= False
      cb_inbasket_folder_index_cancel.visible	= False
      cb_inbasket_folder_index_cancel.enabled	= False
      cb_inbasket_folder_index_ok.visible			= False
      cb_inbasket_folder_index_ok.enabled			= False
      dw_inbasket_folder_index.visible 			= False
      dw_inbasket_folder_index.enabled 			= False
      dw_inbasket_forward_to.visible 				= False
      dw_inbasket_forward_to.enabled 				= False
      dw_inbasket_folder_index.visible 			= False
      dw_inbasket_forward_to.visible 				= False
      dw_inbasket_forward_to.enabled 				= False
		cb_bfclear.visible								= False
		cb_bfclear.enabled								= False
		uo_reference_search.visible					= True

   CASE "FLDINDEX_VISIBLE"
      st_inbasket_forward_to.visible	 			= True
      dw_inbasket_forward_to.visible 				= True
  	   dw_inbasket_forward_to.enabled 				= True
      cb_inbasket_folder_index_cancel.visible	= True
  	   cb_inbasket_folder_index_cancel.enabled	= True
     	cb_inbasket_folder_index_ok.visible 		= True
      IF Not vib_inbasket_folder_multi_claim THEN
         dw_inbasket_folder_index.visible 		= True
         dw_inbasket_folder_index.enabled 		= True
      ELSE
         dw_inbasket_folder_index.visible 		= False
         dw_inbasket_folder_index.enabled 		= False
         st_inbasket_forward_to.visible 			= False
         dw_inbasket_forward_to.visible 			= False
         dw_inbasket_forward_to.enabled 			= False
      END IF
      cb_bfclear.visible								= True
      cb_bfclear.enabled								= True
		uo_reference_search.visible					= False

   CASE "FOLDER_LIST_HIDDEN"
		gb_inbasket_category_selection.visible  	= False
      cb_close.visible     			     			= True
		cb_inbasket_folder_details.visible 			= False
      cb_inbasket_refresh.visible        			= False
      dw_inbasket_category_select.visible 		= False
      dw_inbasket_folder_list.visible      		= False
		cbx_claim_view.visible 							= False

	CASE "FOLDER_LIST_VISIBLE"
		gb_inbasket_category_selection.visible  	= True
      cb_close.visible			          			= True
		cb_inbasket_folder_details.visible 			= True
      dw_inbasket_category_select.visible  		= True
      dw_inbasket_folder_list.visible      		= True
      cb_inbasket_refresh.visible        			= True
		cbx_claim_view.visible							= True

	CASE "FOLDERS_FOUND"
		gb_inbasket_category_selection.enabled		= True
      cb_inbasket_refresh.enabled        			= True
      cb_close.enabled          						= True
      cb_inbasket_folder_details.enabled 			= False
      dw_inbasket_category_select.enabled  		= True
      dw_inbasket_folder_list.enabled      		= True
	
      
	CASE "FOLDERS_NOTFOUND"
		gb_inbasket_category_selection.enabled  = True
      cb_inbasket_refresh.enabled       		 = True
      cb_close.enabled  			       		 = True
      cb_inbasket_folder_details.enabled		 = False
      dw_inbasket_category_select.enabled  	 = True		
      dw_inbasket_folder_list.enabled     	 = False
     
	CASE "OPEN"
		gb_inbasket_category_selection.enabled  = True
      cb_inbasket_refresh.enabled       		 = False
      cb_close.enabled     			    		 = True
		cb_inbasket_folder_details.enabled		 = False
      dw_inbasket_category_select.enabled 	 = True
      dw_inbasket_folder_list.enabled     	 = False
   
	CASE "REFRESH"
		gb_inbasket_category_selection.enabled  	= True
      cb_inbasket_refresh.enabled      		 	= False
      cb_close.enabled        			 		 	= True
		cb_inbasket_folder_details.enabled		 	= False
      dw_inbasket_category_select.enabled		 	= True
      dw_inbasket_folder_list.enabled     		= True
     

END CHOOSE

// setting the inbasket controls for 'search' mode overrides all other setup cases
IF is_opening_method = 'SEARCH' THEN
	dw_inbasket_category_select.Enabled                       = FALSE
	dw_inbasket_category_select.Object.catid.background.color = '553648127'
	
	st_title.Text                           = 'In-Basket for Claim'
	cbx_claim_view.Enabled                  = FALSE
	cb_inbasket_refresh.Enabled             = FALSE
END IF		

Return 1
end function

private subroutine wf_inbasket_display_folder_details ();//  This function sets up and displays the folder details

// -------------------------
//  Declare local variables
// -------------------------
Long    ll_row_nmbr,	ll_doc_rows,	ll_fldid,	ll_claim_cnt,	ll_results
Integer li_error_code

// Initialize Instance variables
vib_inbasket_folder_multi_claim = False

// -------------------------------------------------------
//  Set up screen display for folders detail presentation
// -------------------------------------------------------
wf_inbasket_setup("FOLDER_LIST_HIDDEN")
wf_inbasket_setup("DETAILS_VISIBLE")
wf_inbasket_setup("DETAILS_OPEN")

// ------------------------------------------------------
//  Display folder name at the top of the screen display
// ------------------------------------------------------
ll_row_nmbr = dw_inbasket_folder_list.GetSelectedRow(0)
IF ll_row_nmbr = 0 THEN
	MessageBox("Display Folder Details","Could not determine selected folder. Please try selecting it again.")
	Return
END IF

// Put folder name of selected row onto folder detail string. 
sle_inbasket_folder_name.Text = dw_inbasket_folder_list.GetItemString(ll_row_nmbr,"fld_fldname")

// --------------------------------------------------
//  Display list of documents for the current folder
// --------------------------------------------------
ll_fldid = wf_inbasket_get_selected_fldid()
If ll_fldid = -1 Then
	Return
End If

// Display document list
ll_doc_rows = dw_inbasket_document_list.Retrieve(ll_fldid)
If ImageTrans.nf_handle_error("dw_inbasket_document_list","w_inbasket","wf_inbasket_display_folder_details") < 0 Then
	// Change screen display to folder list
	wf_inbasket_setup("DETAILS_HIDDEN")
	wf_inbasket_setup("FOLDER_LIST_VISIBLE")
	Return
End If

// No documents found for the selected folder
IF ll_doc_rows = 0 THEN
   wf_inbasket_setup("DOCUMENTS_NOT_FOUND")
   // Declare folder index information
   ll_results = wf_inbasket_display_folder_index(ll_fldid)
	Return
END IF
 
// At least one document found for the selected folder
IF ll_doc_rows > 0 THEN

	// Setup screen display for listing documents
	wf_inbasket_setup("DOCUMENTS_FOUND")

	// Count how many claim numbers are contained in the folder
	SELECT count(distinct claim_no)
	INTO :ll_claim_cnt
	FROM REF,DOCUMENT_INDEX
	WHERE REF.docid = DOCUMENT_INDEX.docid
	AND REF.docfldid = :ll_fldid
	USING ImageTrans;

	If ImageTrans.nf_handle_error("Embedded SQL: Select from REF and DOCUMENT_INDEX","w_inbasket","wf_inbasket_display_folder_details") < 0 Then
		// Change screen display to folder list
		wf_inbasket_setup("DETAILS_HIDDEN")
		wf_inbasket_setup("FOLDER_LIST_VISIBLE")
      Return
	End If

   // If no claim number or a single claim number is assigned to the documents 
   // in the work folder then allow the user to maintain the work folder form.
   // Otherwise, the user can change the folder name directly

   IF IsNull(ll_claim_cnt) or ll_claim_cnt < 2 THEN

      // Declare folder index information
      ll_results = wf_inbasket_display_folder_index(ll_fldid)
      If ll_results = -1 THEN
         Return
      END IF

   ELSE
      vib_inbasket_folder_multi_claim = True
	cb_inbasket_folder_index_ok.enabled			= False
      wf_inbasket_setup("FLDINDEX_VISIBLE")
		MessageBox("Folder","This folder contains documents from multiple claims.~r~nYou may not create/modify the folder index for this folder.")
   END IF
END IF
end subroutine

private subroutine wf_inbasket_display_document_index ();// wf_inbasket_display_document_index - displays document indexing information.

// Declare local variables
Long    ll_docid,		ll_selected_row,	ll_fldid
String  ls_type_code, ls_auto_created_flag
Boolean lb_indexed

// --------------------------------------
//  Get document id of selected document 
// --------------------------------------
ll_selected_row = dw_inbasket_document_list.GetSelectedRow(0)
IF ll_selected_row = 0 THEN
   MessageBox("Inbasket Document","Error determining selected document. Please try again.")
	Return
END IF

// get document id
ll_docid = dw_inbasket_document_list.GetItemNumber(ll_selected_row,"ref_docid")
IF ll_docid = 0 THEN
   MessageBox("Inbasket Document","Error determining selected document.  Please try again.")
	Return
END IF

// See if document was automatically generated
ls_type_code = dw_inbasket_document_list.GetItemString(ll_selected_row, "type_code")
SELECT auto_created_flag 
  INTO :ls_auto_created_flag 
  FROM Document_Type_Code 
 WHERE type_code = :ls_type_code ;

IF ls_auto_created_flag = "Y" THEN
	Messagebox("Inbasket Document", "Automatically generated docuents must be re-indexed from the Indexing Module.", Exclamation!)
	RETURN
END IF

istr_window_message.al_doubleparm[1] = ll_docid

OpenWithParm(w_inbasket_document_index,istr_window_message)

// Refresh the document list
ll_fldid = dw_inbasket_document_list.GetItemNumber(1,"ref_docfldid")

dw_inbasket_document_list.Retrieve(ll_fldid)
If ImageTrans.nf_handle_error("dw_inbasket_document_list","w_inbasket","clicked for cb_inbasket_doc_index_ok") < 0 Then
	Return
End If

dw_inbasket_document_list.SetRow(ll_selected_row)
dw_inbasket_document_list.uf_processselect(ll_selected_row,"Keyboard")
end subroutine

public subroutine wf_set_selected_claim (long al_selected_claim_no);il_selected_claim_no = al_selected_claim_no
end subroutine

event open;call super::open;//
//-----APPLICATION SECURITY CODE-----
G_PFSecurity.UOF_Check_Access(This)
This.I_Authorized_Access = True

// ---------------------------
//  Declare working variables
// ---------------------------               
long            ll_rows_loaded,		ll_counter
DataWindowChild ldwc_child_name       
s_window_message lstr_message

lstr_message = Message.PowerObjectParm

is_opening_method = lstr_message.as_stringparm[1]
iw_sheet          = lstr_message.apo_powerobjectparm[1]
If IsValid(iw_sheet) = False Then
	MessageBox("Inbasket OLD","Error determining active sheet. You may have to close WorkBench and try again.")
	Close(This)
	Return
End If

// Create an instance of the user object for the imaging functions
inv_imaging = CREATE n_imaging


// -------------------------------
//  Initialize instance variables
// -------------------------------
il_category_id_nmbr        = 0
vil_inbasket_folder_cnt    = 0
vil_inbasket_filter_cnt    = 0
vil_folder_list_anchor_row = 0

// ------------------------------------------
//  Set transaction objects for data windows
// ------------------------------------------
dw_inbasket_category_select.SetTransObject(ImageTrans)
dw_inbasket_folder_list.SetTransObject(SQLCA)
dw_inbasket_document_list.SetTransObject(ImageTrans)
dw_inbasket_folder_index.SetTransObject(ImageTrans)
dw_inbasket_forward_to.SetTransObject(ImageTrans)

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
wf_inbasket_setup("DETAILS_HIDDEN")
wf_inbasket_setup("FOLDER_LIST_VISIBLE")
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

dw_inbasket_folder_list.uf_setselect(1)
dw_inbasket_folder_list.uf_setsort(True)
dw_inbasket_document_list.uf_setselect(1)
uo_reference_search.uf_set_parent(Handle(This))

il_selected_claim_no = iw_sheet.dw_basic_claim.GetItemNumber(1,'claim_no')
end event

event closequery;call super::closequery;If IsValid(inv_imaging) Then
	Destroy inv_imaging
End If


end event

on w_inbasket_old.create
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
this.st_inbasket_folder_name=create st_inbasket_folder_name
this.cb_inbasket_doc_index=create cb_inbasket_doc_index
this.dw_inbasket_category_select=create dw_inbasket_category_select
this.cb_inbasket_folder_prev=create cb_inbasket_folder_prev
this.cb_inbasket_folder_next=create cb_inbasket_folder_next
this.sle_inbasket_folder_name=create sle_inbasket_folder_name
this.cb_inbasket_folder_delete=create cb_inbasket_folder_delete
this.st_inbasket_forward_to=create st_inbasket_forward_to
this.dw_inbasket_forward_to=create dw_inbasket_forward_to
this.cb_inbasket_folder_index_ok=create cb_inbasket_folder_index_ok
this.cb_inbasket_folder_index_cancel=create cb_inbasket_folder_index_cancel
this.dw_inbasket_document_list=create dw_inbasket_document_list
this.dw_display_claim_info=create dw_display_claim_info
this.dw_inbasket_folder_list=create dw_inbasket_folder_list
this.gb_inbasket_details_doc=create gb_inbasket_details_doc
this.gb_inbasket_details_wrkfld=create gb_inbasket_details_wrkfld
this.gb_inbasket_sort_by=create gb_inbasket_sort_by
this.gb_inbasket_category_selection=create gb_inbasket_category_selection
this.dw_inbasket_folder_index=create dw_inbasket_folder_index
this.cb_bfclear=create cb_bfclear
this.uo_reference_search=create uo_reference_search
this.cb_inbasket_folder_unpaid_accts=create cb_inbasket_folder_unpaid_accts
this.cb_inbasket_doc_reviewed=create cb_inbasket_doc_reviewed
this.cb_inbasket_folder_details=create cb_inbasket_folder_details
this.cb_inbasket_folder_list=create cb_inbasket_folder_list
this.uo_image_append=create uo_image_append
this.cbx_claim_view=create cbx_claim_view
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_account_payment_list
this.Control[iCurrent+2]=this.cb_inbasket_doc_reject_acct
this.Control[iCurrent+3]=this.dw_account_payment_paid_status
this.Control[iCurrent+4]=this.dw_claimsmaster_ref_count
this.Control[iCurrent+5]=this.cb_inbasket_doc_pay_acct
this.Control[iCurrent+6]=this.cb_inbasket_refresh
this.Control[iCurrent+7]=this.cb_inbasket_doc_delete
this.Control[iCurrent+8]=this.cb_inbasket_doc_view
this.Control[iCurrent+9]=this.st_inbasket_folder_name
this.Control[iCurrent+10]=this.cb_inbasket_doc_index
this.Control[iCurrent+11]=this.dw_inbasket_category_select
this.Control[iCurrent+12]=this.cb_inbasket_folder_prev
this.Control[iCurrent+13]=this.cb_inbasket_folder_next
this.Control[iCurrent+14]=this.sle_inbasket_folder_name
this.Control[iCurrent+15]=this.cb_inbasket_folder_delete
this.Control[iCurrent+16]=this.st_inbasket_forward_to
this.Control[iCurrent+17]=this.dw_inbasket_forward_to
this.Control[iCurrent+18]=this.cb_inbasket_folder_index_ok
this.Control[iCurrent+19]=this.cb_inbasket_folder_index_cancel
this.Control[iCurrent+20]=this.dw_inbasket_document_list
this.Control[iCurrent+21]=this.dw_display_claim_info
this.Control[iCurrent+22]=this.dw_inbasket_folder_list
this.Control[iCurrent+23]=this.gb_inbasket_details_doc
this.Control[iCurrent+24]=this.gb_inbasket_details_wrkfld
this.Control[iCurrent+25]=this.gb_inbasket_sort_by
this.Control[iCurrent+26]=this.gb_inbasket_category_selection
this.Control[iCurrent+27]=this.dw_inbasket_folder_index
this.Control[iCurrent+28]=this.cb_bfclear
this.Control[iCurrent+29]=this.uo_reference_search
this.Control[iCurrent+30]=this.cb_inbasket_folder_unpaid_accts
this.Control[iCurrent+31]=this.cb_inbasket_doc_reviewed
this.Control[iCurrent+32]=this.cb_inbasket_folder_details
this.Control[iCurrent+33]=this.cb_inbasket_folder_list
this.Control[iCurrent+34]=this.uo_image_append
this.Control[iCurrent+35]=this.cbx_claim_view
end on

on w_inbasket_old.destroy
call super::destroy
destroy(this.dw_account_payment_list)
destroy(this.cb_inbasket_doc_reject_acct)
destroy(this.dw_account_payment_paid_status)
destroy(this.dw_claimsmaster_ref_count)
destroy(this.cb_inbasket_doc_pay_acct)
destroy(this.cb_inbasket_refresh)
destroy(this.cb_inbasket_doc_delete)
destroy(this.cb_inbasket_doc_view)
destroy(this.st_inbasket_folder_name)
destroy(this.cb_inbasket_doc_index)
destroy(this.dw_inbasket_category_select)
destroy(this.cb_inbasket_folder_prev)
destroy(this.cb_inbasket_folder_next)
destroy(this.sle_inbasket_folder_name)
destroy(this.cb_inbasket_folder_delete)
destroy(this.st_inbasket_forward_to)
destroy(this.dw_inbasket_forward_to)
destroy(this.cb_inbasket_folder_index_ok)
destroy(this.cb_inbasket_folder_index_cancel)
destroy(this.dw_inbasket_document_list)
destroy(this.dw_display_claim_info)
destroy(this.dw_inbasket_folder_list)
destroy(this.gb_inbasket_details_doc)
destroy(this.gb_inbasket_details_wrkfld)
destroy(this.gb_inbasket_sort_by)
destroy(this.gb_inbasket_category_selection)
destroy(this.dw_inbasket_folder_index)
destroy(this.cb_bfclear)
destroy(this.uo_reference_search)
destroy(this.cb_inbasket_folder_unpaid_accts)
destroy(this.cb_inbasket_doc_reviewed)
destroy(this.cb_inbasket_folder_details)
destroy(this.cb_inbasket_folder_list)
destroy(this.uo_image_append)
destroy(this.cbx_claim_view)
end on

event close;call super::close;
IF is_opening_method = 'MENU' THEN
	post close(iw_sheet)
END IF

end event

type st_title from w_a_tool`st_title within w_inbasket_old
integer x = 18
string text = "In-Basket"
end type

type cb_close from w_a_tool`cb_close within w_inbasket_old
integer x = 2679
integer taborder = 240
end type

type dw_account_payment_list from u_dw_online within w_inbasket_old
boolean visible = false
integer x = 91
integer y = 600
integer width = 558
integer height = 460
integer taborder = 290
boolean enabled = false
string dataobject = "d_account_payment_list"
end type

type cb_inbasket_doc_reject_acct from commandbutton within w_inbasket_old
integer x = 553
integer y = 1564
integer width = 407
integer height = 96
integer taborder = 300
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
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
		istr_window_message.adtm_datetimeparm[1]	= idtm_date_on_document
		istr_window_message.as_mode					= "inbasketreject"
		
		wf_inbasket_account_pay()
	
	END IF
end event

type dw_account_payment_paid_status from u_dw_online within w_inbasket_old
boolean visible = false
integer x = 635
integer y = 912
integer height = 360
integer taborder = 160
boolean enabled = false
string dataobject = "d_account_payment_paid_status"
end type

type dw_claimsmaster_ref_count from u_dw_online within w_inbasket_old
boolean visible = false
integer x = 626
integer y = 604
integer height = 360
integer taborder = 150
string dataobject = "d_claimsmaster_ref_count"
end type

type cb_inbasket_doc_pay_acct from commandbutton within w_inbasket_old
integer x = 69
integer y = 1564
integer width = 407
integer height = 96
integer taborder = 330
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
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

type cb_inbasket_refresh from commandbutton within w_inbasket_old
integer x = 2446
integer y = 336
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

type cb_inbasket_doc_delete from commandbutton within w_inbasket_old
integer x = 69
integer y = 1468
integer width = 297
integer height = 96
integer taborder = 190
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Delete"
end type

event clicked;//  This script deletes the selected document

Long ll_results, ll_count, ll_rownum, ll_docid

// Disable button to prevent second click
Enabled = False

// Make sure you can figure out what document they want to delete
ll_rownum = dw_inbasket_document_list.GetSelectedRow(0)
If ll_rownum = 0 Then
	MessageBox("Delete Document","Could not determine selected document to delete. Please try again.")
	Enabled = True
	Return
End If

// Get the document's id
ll_docid = dw_inbasket_document_list.GetItemNumber(ll_rownum,"ref_docid")
If ll_docid = -1 Then
	MessageBox("Delete Document","Could not determine selected document to delete. Please try again.")
	Enabled = True
	Return
End If

// Check that the document exists in more that one place
SELECT count(*) INTO :ll_count
FROM REF
WHERE docid = :ll_docid
USING ImageTrans;

If ImageTrans.nf_handle_error("Embedded SQL: Select from REF","w_inbasket","clicked for cb_inbasket_doc_delete") < 0 Then
	Close(Parent)
	Return 
End If

If ll_count = 1 Then
	MessageBox("Delete Document","Can not delete document because it does not exist anywhere else.")
	Enabled = True
	Return
End If

// Delete document from u_dw_online
ll_results = dw_inbasket_document_list.DeleteRow(ll_rownum)
If ll_results = -1 Then
	MessageBox("Delete Document","Could not delete document.")
	Enabled = True
	Return
End If

ImageTrans.nf_begin_transaction()

// Decrement the document reference counter
UPDATE DOC
SET docrefcount = :ll_count - 1
WHERE docid = :ll_docid
USING ImageTrans;

If ImageTrans.nf_handle_error("Embedded SQL: Update DOC","w_inbasket","clicked for cb_inbasket_doc_delete") < 0 Then
	Close(Parent)
	Return 
End If

// Remove the document reference
dw_inbasket_document_list.Update()
If ImageTrans.nf_handle_error("dw_inbasket_document_list","w_inbasket","clicked for cb_inbasket_doc_delete") < 0 Then
	Return 
End If

ImageTrans.nf_commit_transaction()


// Enable the button
Enabled = True
/* Check added by Rob Head 98/09/17 */
If dw_inbasket_document_list.RowCount() = 0 Then
	wf_inbasket_setup("DOCUMENTS_NOT_FOUND")
End If

end event

type cb_inbasket_doc_view from commandbutton within w_inbasket_old
integer x = 759
integer y = 1468
integer width = 302
integer height = 96
integer taborder = 210
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&View "
end type

event clicked;//  This script opens the document in the imaging viewer

Long ll_row_nmbr, ll_docid, ll_result
string ls_doc_type
integer li_rowsreturned
// Disable button to prevent double clicking
Enabled = False

// Get id of selected row
ll_row_nmbr = dw_inbasket_document_list.GetSelectedRow(0)
If ll_row_nmbr <= 0 Then
   MessageBox("InBasket","Error determining selected row. Please try again")
	Enabled = True
	Return
End If

// Get value of folder id field on selected row
ll_docid = dw_inbasket_document_list.GetItemNumber(ll_row_nmbr,"ref_docid")
If ll_docid <= 0 Then
   MessageBox("InBasket","Could not determine document id of selected row.  Please try again.")
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

type st_inbasket_folder_name from statictext within w_inbasket_old
integer x = 279
integer y = 140
integer width = 352
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Folder Name:"
alignment alignment = center!
boolean focusrectangle = false
end type

type cb_inbasket_doc_index from commandbutton within w_inbasket_old
integer x = 411
integer y = 1468
integer width = 302
integer height = 96
integer taborder = 200
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
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

type dw_inbasket_category_select from u_dw_online within w_inbasket_old
integer x = 73
integer y = 192
integer width = 878
integer height = 92
integer taborder = 100
string dataobject = "d_cat_list_external_old"
boolean border = false
end type

event itemchanged;IF uo_reference_search.ib_is_search_expanded THEN
	uo_reference_search.ib_is_search_expanded = FALSE
	uo_reference_search.gb_reference.Height = 153
	uo_reference_search.Height = 155
	uo_reference_search.BringToTop = FALSE
END IF

//  Retrieve category id number of the selected category.
//  Place value in an instance variable for other functions
//  to use
il_category_id_nmbr = Long(GetText())

If il_category_id_nmbr <= 0 Then
	MessageBox("Selected Category","Could not determine selected category/bucket.  Please try again.")
	Return
End If

// If user selects category 2 then they can no longer do anything with it
If il_category_id_nmbr = 2 Then
	MessageBox ("In_Basket", "You may no longer access the CLAIMS MASTER FILE category from In-Basket.", STOPSIGN!, OK!)
	Return 1
Else
   // Refresh Folder List
	is_row_filter = ""
   wf_inbasket_refresh_folder_list()
	cbx_claim_view.Checked = False
End If
end event

event dberror;imagetrans.SQLDBCode = sqldbcode
imagetrans.SQLErrText = sqlerrtext
RETURN 1  // Don't display message
end event

type cb_inbasket_folder_prev from commandbutton within w_inbasket_old
integer x = 1778
integer y = 136
integer width = 375
integer height = 96
integer taborder = 70
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Prev"
end type

on clicked;//  This script forces the display of folder information back one row

// Move backwards one row in the folder list data window
wf_inbasket_folder_list_move("PREV")
end on

type cb_inbasket_folder_next from commandbutton within w_inbasket_old
integer x = 2217
integer y = 136
integer width = 375
integer height = 96
integer taborder = 90
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Next"
end type

on clicked;//  This script forces the display of folder information back one row

// Move forwards one row in the folder list data window
wf_inbasket_folder_list_move("NEXT")
end on

type sle_inbasket_folder_name from singlelineedit within w_inbasket_old
integer x = 677
integer y = 140
integer width = 1024
integer height = 88
integer taborder = 80
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean border = false
boolean autohscroll = false
end type

type cb_inbasket_folder_delete from commandbutton within w_inbasket_old
integer x = 69
integer y = 1680
integer width = 434
integer height = 96
integer taborder = 270
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "D&elete Folder"
end type

event clicked;// Declare local variables
Long	ll_results,		ll_fldid
Int	li_delete_folder

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
If ll_fldid = -1 Then
	Goto Normal_Exit
End If

// Delete folder entry from imaging
ll_results = inv_imaging.nf_delete_workfolder(ll_fldid, False,"w_inbasket")
If ll_results < 1 Then
	GoTo Normal_Exit
End If

// Remove folder entry from folder list
wf_inbasket_remove_folder_from_list()


IF is_opening_method = 'SEARCH' THEN
	iw_sheet.uo_claim_search.Trigger function uf_refresh_search()
END IF


// End of script
Normal_Exit:
   Enabled = True
   Return
end event

type st_inbasket_forward_to from statictext within w_inbasket_old
integer x = 1737
integer y = 1144
integer width = 955
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

type dw_inbasket_forward_to from u_dw_online within w_inbasket_old
integer x = 1728
integer y = 1208
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

type cb_inbasket_folder_index_ok from commandbutton within w_inbasket_old
integer x = 2455
integer y = 1468
integer width = 375
integer height = 96
integer taborder = 230
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
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
If ll_fldid = -1 Then
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


ELSE

   // If the folder index was processed then lookup the new folder name
   IF ll_process_folder_index_results = 1 THEN

      SELECT fldname
        INTO :ls_folder_name
        FROM FLD
       WHERE fldid = :ll_fldid
       USING ImageTrans;
		ImageTrans.nf_handle_error("Embedded SQL: Select FLD","w_inbasket","clicked for cb_inbasket_folder_index_ok")

      // Update folder name on the screen
      sle_inbasket_folder_name.text = ls_folder_name

   END IF

   // If the folder name was changed directly then copy the new value to the local variable
   IF ll_process_folder_name_results = 1 THEN

      // Set local folder name to the value in the entry field
      ls_folder_name = sle_inbasket_folder_name.text

   END IF

   // Get the currently selected row
   ll_selected_row =  dw_inbasket_folder_list.GetSelectedRow(0)  
   IF ll_selected_row = 0 THEN
		MessageBox("Folder Index Changes","Could not determine selected folder. Please try again.")
		Goto Normal_Exit
   END IF

   // If either the folder index or folder name was changed then change the name in the folder list data window
   IF ll_process_folder_name_results = 1 OR ll_process_folder_index_results = 1 THEN
      dw_inbasket_folder_list.SetItem(ll_selected_row,"fld_fldname",ls_folder_name)
   END IF

   // Update the keyword and action date fields in the data window
   IF vib_inbasket_folder_multi_claim = False THEN
      ls_keyword = dw_inbasket_folder_index.GetItemString(1,"action_note")
      ldt_action_date = dw_inbasket_folder_index.GetItemDateTime(1,"action_date")

      dw_inbasket_folder_list.SetItem(ll_selected_row,"action_note",ls_keyword)
      dw_inbasket_folder_list.SetItem(ll_selected_row,"action_date",ldt_action_date)
   END IF
   
END IF

// Normal End of Function
Normal_Exit:
   Enabled = True

end event

type cb_inbasket_folder_index_cancel from commandbutton within w_inbasket_old
integer x = 1920
integer y = 1468
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
If ll_fldid = -1 Then
	Goto Exit_Script
End If	

// Redisplay screen setup and folder name field
wf_inbasket_setup("FLDINDEX_VISIBLE")
cb_inbasket_folder_index_ok.enabled			= TRUE

// Get folder name from folder selected in folder list data winodw
ll_selected_row = dw_inbasket_folder_list.GetSelectedRow(0)
IF ll_selected_row = 0 THEN
	MessageBox("Folder Index Cancel","Could not determine selected folder.  Please try selecting folder again.")
	Goto Exit_Script
END IF

// Get folder name
sle_inbasket_folder_name.Text = dw_inbasket_folder_list.GetItemString(ll_selected_row,"fld_fldname")

// Re - Retrieve folder index information for the folder
// Initialize forward to drop-down list
wf_inbasket_display_folder_index(ll_fldid)

Exit_Script:
	// Enable button
	Enabled = True
end event

type dw_inbasket_document_list from u_dw_online within w_inbasket_old
integer x = 69
integer y = 356
integer width = 1358
integer height = 1104
integer taborder = 50
string dataobject = "d_inbasket_document_list"
boolean hscrollbar = true
boolean vscrollbar = true
end type

on doubleclicked;call u_dw_online::doubleclicked;// View document
cb_inbasket_doc_view.PostEvent(Clicked!)
end on

event clicked;call super::clicked;// This event script marks a clicked row as selected 
If row <= 0 Then
	Return
End If

SetRow(row)

// Mark row clicked as the selected row
wf_inbasket_setup("DOCUMENT_SELECTED")

end event

type dw_display_claim_info from u_dw_online within w_inbasket_old
boolean visible = false
integer x = 133
integer y = 896
integer height = 360
integer taborder = 140
boolean enabled = false
string dataobject = "d_basic_claim_everything"
end type

type dw_inbasket_folder_list from u_dw_online within w_inbasket_old
event ue_to_formulary ( )
event ue_to_cra_requests ( )
integer x = 27
integer y = 336
integer width = 3136
integer height = 1332
integer taborder = 30
string dataobject = "d_inbasket_folder_list"
boolean hscrollbar = true
boolean vscrollbar = true
end type

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

event clicked;call super::clicked;// This event script marks a clicked row as selected 


If row <= 0 Then
	Return
End If

SetRow(row)

// Clear the workbench
wf_inbasket_clear_workbench()

// Get the claim associated with the folder
il_selected_claim_no = GetItemNumber(row, "claim_no")

If Not IsNull(il_selected_claim_no) Then
	wf_set_claim(il_selected_claim_no)
End If

cb_inbasket_folder_details.Enabled = True
cb_inbasket_folder_delete.Enabled = True

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

			Do While ll_counter <= dw_inbasket_folder_list.RowCount()
				If DaysAfter(Date(dw_inbasket_folder_list.GetItemDateTime(ll_counter,"fld_fldcreated")),Date(f_server_datetime())) < il_days_after Then
					ll_fldid = dw_inbasket_folder_list.GetItemNumber(ll_counter,"folderid")
					is_row_filter = is_row_filter + " And folderid <> " + String(ll_fldid)
				End If
	
				ll_counter++
			Loop
	END IF

	// --------------------------------------
	//  Set filter on data window and filter
	// --------------------------------------
	cbx_claim_view.checked = False
	
	dw_inbasket_folder_list.SelectRow(0, False) /* get rid of selected rows so they won't come back when we unfilter */
	
	dw_inbasket_folder_list.SetFilter(is_row_filter)
	dw_inbasket_folder_list.Filter()

	// Set instance variable to number of active rows
	ll_results = dw_inbasket_folder_list.RowCount()
	IF ll_results <> -1 THEN 
   	vil_inbasket_filter_cnt = ll_results
	ELSE
   	MessageBox("Inbasket Filter Folders","Error determining row count for folder list data window.")
	END IF

	IF dw_inbasket_folder_list.GetSelectedRow(0) = 0 THEN
		wf_inbasket_setup('REFRESH')
	END IF

END IF
end event

event doubleclicked;call super::doubleclicked;IF cb_inbasket_folder_details.Enabled = TRUE THEN
	cb_inbasket_folder_details.PostEvent(Clicked!)
END IF
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
	lm_popup.m_options.m_filterlist.visible = TRUE
	lm_popup.m_options.m_gotoformulary.visible = TRUE
	lm_popup.m_options.m_maintaincrarequests.visible = TRUE
	lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))

	Destroy lm_popup
end event

event rowfocuschanged;// do not extend ancestor script
end event

type gb_inbasket_details_doc from groupbox within w_inbasket_old
integer x = 14
integer y = 260
integer width = 1454
integer height = 1412
integer taborder = 120
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Documents"
borderstyle borderstyle = styleraised!
end type

type gb_inbasket_details_wrkfld from groupbox within w_inbasket_old
integer x = 1600
integer y = 256
integer width = 1454
integer height = 1412
integer taborder = 110
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Work Folders"
borderstyle borderstyle = styleraised!
end type

type gb_inbasket_sort_by from groupbox within w_inbasket_old
boolean visible = false
integer x = 2002
integer y = 356
integer width = 581
integer height = 432
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Sort By:"
end type

type gb_inbasket_category_selection from groupbox within w_inbasket_old
boolean visible = false
integer x = 59
integer y = 136
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

type dw_inbasket_folder_index from u_dw_online within w_inbasket_old
integer x = 1650
integer y = 348
integer width = 1358
integer height = 764
integer taborder = 40
boolean bringtotop = true
string dataobject = "d_inbasket_work_folder_index_old"
end type

event itemchanged;call super::itemchanged;INTEGER	li_addondays
STRING	ls_adate
DATE		ld_bringfwddate, ld_actiondate
DATETIME ldt_date


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
			
			SELECT Distinct DATEADD(mm, 18, GetDate())
			INTO :ldt_date
			FROM sysobjects
			USING SQLCA;
						
			IF DaysAfter (Date(ldt_date), ld_actiondate ) > 0 THEN
				MessageBox("Action Date",	"The action date is more than 18 months in the future." &
												+	"~n               Please correct the date.",StopSign!)
				RETURN 1
			END IF
						
			SELECT Distinct DATEADD(mm, 6, GetDate())
			INTO :ldt_date
			FROM sysobjects
			USING SQLCA;
						
			IF DaysAfter (Date(ldt_date), ld_actiondate ) > 0 THEN
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
			
			SELECT Distinct DATEADD(mm, 18, GetDate())
			INTO :ldt_date
			FROM sysobjects
			USING SQLCA;
						
			IF DaysAfter (Date(ldt_date), ld_bringfwddate ) > 0 THEN
				MessageBox("Bring Forward Date",	"The bring forward date is more than 18 months in the future." &
															+	"~n                       Please correct the date.",StopSign!)
				RETURN 1
			END IF
						
			SELECT Distinct DATEADD(mm, 6, GetDate())
			INTO :ldt_date
			FROM sysobjects
			USING SQLCA;
						
			IF DaysAfter (Date(ldt_date), ld_bringfwddate ) > 0 THEN
				MessageBox("Bring Forward Date","The bring forward date is more than 6 months in the future.",Exclamation!)
			END IF		
		END IF

END CHOOSE

end event

type cb_bfclear from commandbutton within w_inbasket_old
integer x = 1669
integer y = 992
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

type uo_reference_search from u_reference_search within w_inbasket_old
integer x = 1403
integer y = 136
integer taborder = 10
end type

on uo_reference_search.destroy
call u_reference_search::destroy
end on

type cb_inbasket_folder_unpaid_accts from commandbutton within w_inbasket_old
integer x = 1038
integer y = 1564
integer width = 407
integer height = 96
integer taborder = 320
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
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
	il_claim_no 			= iw_sheet.dw_basic_claim.GetItemNumber(1,"claim_no")
	is_claim_status 		= iw_sheet.dw_basic_claim.GetItemString(1,"claim_status_code") 
	is_claim_status_type = iw_sheet.dw_basic_claim.GetItemString(1,"claim_status_type_code") 


/* Validate the claim meets the requirements for viewing Unpaid Accounts
*/
	ll_valid_acct =  wf_inbasket_validate_for_unpaid()

	ll_rownum = dw_inbasket_document_list.GetRow()
	IF ll_rownum > 0 THEN
		il_docid = dw_inbasket_document_list.GetItemNumber(ll_rownum,"ref_docid")

		IF wf_check_doc_index() < 0 THEN
			Return
		END IF		
	END IF

/* Now that the claim and document has been validated, open the Account Payment Window

*/
	IF ll_valid_acct = 0 THEN
		lstr_window_message.awi_parent_window = w_inbasket
		lstr_window_message.as_mode	= "inbasketunpaid"
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

type cb_inbasket_doc_reviewed from commandbutton within w_inbasket_old
integer x = 1106
integer y = 1468
integer width = 315
integer height = 96
integer taborder = 310
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
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
	ll_currentrow = dw_inbasket_document_list.GetSelectedRow(0)
	IF ll_currentrow = 0 THEN
		MessageBox("Reviewed Document Error","A document must first be selected before it can be stamped 'Reviewed'.")
	ELSE

/*	Check to see if the document has already been reviewed. If not, then continue to set it to 'reviewed'.
	Need to find in iw_sheet.dw_documents by document id.
*/
		ll_docid = dw_inbasket_document_list.GetItemNumber(ll_currentrow,'ref_docid')
		ll_foundonrow = iw_sheet.dw_documents.Find("ref_docid = " + String(ll_docid),1,iw_sheet.dw_documents.RowCount())
		IF ll_foundonrow <= 0 THEN
			MessageBox("Reviewed Document Error","Unable to determine if the document was already reviewed.")
			RETURN
		END IF

		SELECT count(*)
		  INTO :li_existcount
		  FROM REVIEWED_DOCUMENTS
		 WHERE docid = :ll_docid
		 USING ImageTrans;
		
		ll_result = ImageTrans.nf_handle_error("Embedded SQL: SELECT count(*) INTO :li_existcount","w_inbasket","cb_inbasket_doc_reviewed")
		IF ll_result < 0 THEN
			RETURN
		END IF

		IF li_existcount = 0 THEN

/*	Ask the user if they wish to continue with the stamping of the document.
*/
			li_messageanswer = MessageBox("Reviewed Document Stamp","Do you wish to stamp the selected document as having been reviewed.",Question!,OkCancel!)
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

			ll_result = ImageTrans.nf_handle_error("Embedded SQL: INSERT INTO Reviewed_Documents","w_inbasket","cb_inbasket_doc_reviewed")
			IF ll_result < 0 THEN
				RETURN
			END IF

/*	Stamp the DOCUMENT_INDEX record.
*/
			UPDATE DOCUMENT_INDEX
				SET reviewed_flag = 'Y'
			 WHERE docid = :ll_docid
			 USING ImageTrans;

			ll_result = ImageTrans.nf_handle_error("Embedded SQL: UPDATE DOCUMENT_INDEX","w_inbasket","cb_inbasket_doc_reviewed")
			IF ll_result < 0 THEN
				RETURN
			END IF

			ImageTrans.nf_commit_transaction()
			

/*	Display the reviewed icon for the document.
*/
			iw_sheet.dw_documents.SetItem(ll_foundonrow,'reviewed_flag','Y')
		ELSE
			MessageBox("Reviewed Document Warning","The currently selected document has already been stamped as 'Reviewed'.")
		END IF
	END IF

end event

type cb_inbasket_folder_details from commandbutton within w_inbasket_old
integer x = 1806
integer y = 1680
integer width = 393
integer height = 100
integer taborder = 250
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "De&tails ..."
end type

on clicked;//  This script sets up the display of the folder contents

IF uo_reference_search.ib_is_search_expanded THEN
	uo_reference_search.ib_is_search_expanded = FALSE
	uo_reference_search.gb_reference.Height = 153
	uo_reference_search.Height = 155
	uo_reference_search.BringToTop = FALSE
END IF

// Disable button to prevent double clicking
Enabled = False

// display folder details
wf_inbasket_display_folder_details()

// Enable button to prevent double clicking
Enabled = True
end on

type cb_inbasket_folder_list from commandbutton within w_inbasket_old
integer x = 1806
integer y = 1680
integer width = 393
integer height = 92
integer taborder = 280
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Folders ..."
end type

event clicked;//  This script moves from folder detail processing to folder list processing

// Disable button to prevent second clicking
Enabled = False

// Change screen display
wf_inbasket_setup("DETAILS_HIDDEN")
wf_inbasket_setup("FOLDER_LIST_VISIBLE")

// If all folders have been forwarded or deleted then no folders available
If vil_inbasket_filter_cnt = 0 Then
   wf_inbasket_setup("FOLDERS_NOTFOUND")
End If

// Disable button to prevent second clicking
Enabled = True
end event

type uo_image_append from u_image_append within w_inbasket_old
boolean visible = false
integer x = 1207
integer y = 1716
integer taborder = 260
boolean bringtotop = true
end type

on uo_image_append.destroy
call u_image_append::destroy
end on

type cbx_claim_view from checkbox within w_inbasket_old
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

event clicked;
If this.Checked Then
	If il_selected_claim_no = 0 or IsNull(il_selected_claim_no) THen
		MessageBox('Error','No claim number to filter on.')
		THis.checked = False
	Else
		If dw_inbasket_folder_list.SetFilter('claim_no = ' + String(il_selected_claim_no)) = -1 THen SignalError(-666,'Error setting claim fitler')
		If dw_inbasket_folder_list.Filter() = -1 THen SignalError(-666,'Error filtering for claim')
		vil_inbasket_filter_cnt = dw_inbasket_folder_list.RowCount()
	End if
Else
	If dw_inbasket_folder_list.SetFilter('') = -1 THen SignalError(-666,'Error removing filter')
	If dw_inbasket_folder_list.Filter() = -1 THen SignalError(-666,'Error removing filter')
	vil_inbasket_filter_cnt = dw_inbasket_folder_list.RowCount()
End if


wf_inbasket_setup("DETAILS_HIDDEN")
wf_inbasket_setup("FOLDER_LIST_VISIBLE")
	
IF vil_inbasket_filter_cnt = 0 THEN
	wf_inbasket_setup("FOLDERS_NOTFOUND")
ELSE
	wf_inbasket_setup("FOLDERS_FOUND")
	cb_inbasket_folder_details.Enabled = True
END IF
end event

