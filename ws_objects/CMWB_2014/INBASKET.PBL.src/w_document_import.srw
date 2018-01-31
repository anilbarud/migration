$PBExportHeader$w_document_import.srw
$PBExportComments$Window for importing Word and Excel documents
forward
global type w_document_import from w_a_tool
end type
type uo_document_indexing from u_document_index within w_document_import
end type
type st_2 from statictext within w_document_import
end type
type sle_pathname from singlelineedit within w_document_import
end type
type st_message from statictext within w_document_import
end type
type dw_indexing_paid_documents from u_dw_online within w_document_import
end type
type dw_duplicate_documents from u_dw_online within w_document_import
end type
type cb_cancel from commandbutton within w_document_import
end type
type cb_ok from commandbutton within w_document_import
end type
type cb_import from commandbutton within w_document_import
end type
type cb_alt1 from commandbutton within w_document_import
end type
end forward

global type w_document_import from w_a_tool
integer width = 3173
integer height = 1800
boolean resizable = false
uo_document_indexing uo_document_indexing
st_2 st_2
sle_pathname sle_pathname
st_message st_message
dw_indexing_paid_documents dw_indexing_paid_documents
dw_duplicate_documents dw_duplicate_documents
cb_cancel cb_cancel
cb_ok cb_ok
cb_import cb_import
cb_alt1 cb_alt1
end type
global w_document_import w_document_import

type variables
STRING		is_curr_mode
W_SHEET	iw_sheet
N_IMAGING	inv_imaging
LONG		il_master_fldid

end variables

forward prototypes
public function integer wf_set_claim (long al_claim_no)
public function integer wf_validate_file_extension (string as_filename, ref string as_file_extension)
end prototypes

public function integer wf_set_claim (long al_claim_no);IF uo_document_indexing.dw_document_indexing.RowCount() = 1 THEN
	uo_document_indexing.dw_document_indexing.SetItem(1,"claim_no",al_claim_no)
	IF uo_document_indexing.uf_set_claim(al_claim_no) <> 1 THEN
		uo_document_indexing.dw_document_indexing.SetItem(1,"claim_no",0)
	END IF
END IF

RETURN 0

end function

public function integer wf_validate_file_extension (string as_filename, ref string as_file_extension);// wf_validate_file_extension
//
Long    ll_pos, ll_count, ll_num_rows, n
Integer li_rtn
String  ls_file_type_code, ls_file_type_desc, ls_descriptions
u_ds lds_file_type_document_import_types 

// Make sure file is a valid type
ll_pos = LastPos(as_filename, ".")
IF ll_pos = 0 OR IsNull(ll_pos) = TRUE THEN
	MessageBox("File Extension unknown", "Unable to determine file extension of file: " + as_filename + ".~r~rSelect a different file and try again.")
	RETURN -1
END IF

as_file_extension = Lower(Right(as_filename, Len(as_filename) - ll_pos))

SELECT COUNT(file_type_code) 
  INTO :ll_count 
  FROM File_Type 
 WHERE Lower(file_type_code) = :as_file_extension 
   AND document_import_flag = 'Y' ; 

li_rtn = SQLCA.nf_handle_error("w_document_import", "", "wf_validate_file_extension - SELECT COUNT(file_type_code) FROM File_Type ")

IF ll_count = 0 THEN
	lds_file_type_document_import_types = CREATE u_ds
	lds_file_type_document_import_types.dataobject = 'd_file_type_document_import_types'
	li_rtn = lds_file_type_document_import_types.SetTransObject(SQLCA)
	ll_num_rows = lds_file_type_document_import_types.Retrieve()
	li_rtn = SQLCA.nf_handle_error("w_document_import", "", "wf_validate_file_extension - lds_file_type_document_import_types.Retrieve()")
	FOR n = 1 TO ll_num_rows
		ls_file_type_code = lds_file_type_document_import_types.GetItemString(n, "file_type_code")
		ls_file_type_desc = lds_file_type_document_import_types.GetItemString(n, "file_type_desc") 
		ls_descriptions = ls_descriptions + "~r~t" + ls_file_type_code + " - " + ls_file_type_desc
	NEXT
	DESTROY lds_file_type_document_import_types
	
	MessageBox("Invalid File Extension", "The document type: " + as_file_extension + " is not a valid type.~r~r" +&
				  "These are the Valid extensions: " + ls_descriptions + "~r~rSelect a file with a different extension and try again.")
	RETURN -1
END IF

RETURN 0

end function

event open;call super::open;/* Create an instance of the user object for the imaging functions.
*/
	inv_imaging = CREATE n_imaging

/* Get the name of the active work sheet.
*/
	iw_sheet = w_frame.GetActiveSheet()

	IF IsValid(iw_sheet) = FALSE THEN
	   MessageBox("Document Import","Error determining sheet.")
		Close(THIS)
		RETURN
	END IF

	dw_duplicate_documents.SetTransObject(ImageTrans)
	dw_indexing_paid_documents.SetTransObject(SQLCA)
	IF iw_sheet.dw_basic_claim.RowCount() < 1 THEN
		MessageBox('No Cliam','No claim has been retrieved. Search for a claim first.')
		Close(THIS)
		RETURN
	END IF
	uo_document_indexing.dw_document_indexing.Modify("document_type_desc.Protect=0")
	uo_document_indexing.dw_document_indexing.Modify("claim_no.Protect=1")
   uo_document_indexing.dw_document_indexing.Modify("reference_no.Protect=1")
	
	cb_import.PostEvent(Clicked!)

end event

on closequery;call w_a_tool::closequery;IF IsValid(inv_imaging) THEN
	Destroy inv_imaging
END IF

end on

on w_document_import.create
int iCurrent
call super::create
this.uo_document_indexing=create uo_document_indexing
this.st_2=create st_2
this.sle_pathname=create sle_pathname
this.st_message=create st_message
this.dw_indexing_paid_documents=create dw_indexing_paid_documents
this.dw_duplicate_documents=create dw_duplicate_documents
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.cb_import=create cb_import
this.cb_alt1=create cb_alt1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.uo_document_indexing
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.sle_pathname
this.Control[iCurrent+4]=this.st_message
this.Control[iCurrent+5]=this.dw_indexing_paid_documents
this.Control[iCurrent+6]=this.dw_duplicate_documents
this.Control[iCurrent+7]=this.cb_cancel
this.Control[iCurrent+8]=this.cb_ok
this.Control[iCurrent+9]=this.cb_import
this.Control[iCurrent+10]=this.cb_alt1
end on

on w_document_import.destroy
call super::destroy
destroy(this.uo_document_indexing)
destroy(this.st_2)
destroy(this.sle_pathname)
destroy(this.st_message)
destroy(this.dw_indexing_paid_documents)
destroy(this.dw_duplicate_documents)
destroy(this.cb_cancel)
destroy(this.cb_ok)
destroy(this.cb_import)
destroy(this.cb_alt1)
end on

type st_title from w_a_tool`st_title within w_document_import
integer x = 14
string text = "Document Importing"
end type

type cb_close from w_a_tool`cb_close within w_document_import
integer x = 2482
integer y = 1672
integer taborder = 90
end type

type uo_document_indexing from u_document_index within w_document_import
integer x = 489
integer y = 212
integer width = 2185
integer height = 784
integer taborder = 70
end type

on uo_document_indexing.destroy
call u_document_index::destroy
end on

type st_2 from statictext within w_document_import
integer x = 485
integer y = 1072
integer width = 539
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Document to Import:"
alignment alignment = center!
boolean focusrectangle = false
end type

type sle_pathname from singlelineedit within w_document_import
integer x = 1038
integer y = 1068
integer width = 1458
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
boolean displayonly = true
end type

type st_message from statictext within w_document_import
integer x = 320
integer y = 1252
integer width = 2537
integer height = 88
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16711680
long backcolor = 67108864
boolean enabled = false
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type dw_indexing_paid_documents from u_dw_online within w_document_import
boolean visible = false
integer x = 2313
integer y = 1368
integer height = 108
integer taborder = 60
boolean enabled = false
string dataobject = "d_indexing_paid_documents"
end type

type dw_duplicate_documents from u_dw_online within w_document_import
boolean visible = false
integer x = 581
integer y = 1496
integer width = 1861
integer height = 96
integer taborder = 10
boolean enabled = false
string dataobject = "d_duplicate_documents"
boolean border = false
end type

type cb_cancel from commandbutton within w_document_import
integer x = 1806
integer y = 1672
integer width = 320
integer height = 100
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
end type

on clicked;uo_document_indexing.dw_document_indexing.Reset()
uo_document_indexing.cb_provider_search.Visible = FALSE
cb_alt1.Enabled = FALSE
cb_ok.Enabled = FALSE
cb_import.Enabled = TRUE
cb_cancel.Enabled = FALSE
sle_pathname.text = ""

end on

type cb_ok from commandbutton within w_document_import
integer x = 2144
integer y = 1672
integer width = 320
integer height = 100
integer taborder = 50
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
end type

event clicked;Integer  li_rtn
Long     ll_docid, ll_result, ll_claim_no, ll_sender
String   ls_file_extension, ls_pathname, ls_type, ls_filename
Datetime ldt_date
n_process_run_status ln_process_run_status

//******************************************************************************************
// P10275 - Daytime Payment Processing
//        - added new function call to prevent updating of tables used by PRODSVCS Payment Processing
//        - new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
//        - '021' refers to the Document Import module, '044' refers to the Payment Processing module
//******************************************************************************************/
ln_process_run_status = CREATE N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('021', '044', 'import', SQLCA)
IF li_rtn = 1 THEN
	// module is blocked
	RETURN
END IF
/******************************************************************************************/

// Create the logical document and save the real document in the new directory.

// Check that the required fields are filled in.
IF uo_document_indexing.uf_check_required(ls_type,ldt_date) = -1 THEN
	RETURN
END IF

cb_ok.Enabled = FALSE

// Make sure the claim number is valid.
IF NOT iw_sheet.dw_basic_claim.GetItemNumber(1,"claim_no") > 0 THEN
	MessageBox("Document Indexing","Claim number must be filled in and validated.")
	cb_ok.Enabled = TRUE
	RETURN
END IF

il_master_fldid = uo_document_indexing.il_master_fldid
ll_claim_no		 = uo_document_indexing.dw_document_indexing.GetItemNumber(1,"claim_no") 

//	Check to see if a document of the same type, and date exists.
dw_duplicate_documents.Retrieve(ll_claim_no, ls_type, ldt_date)
IF ImageTrans.nf_handle_error("w_document_import", "", "cb_ok - dw_duplicate_documents.Retrieve(ll_claim_no, ls_type, ldt_date)") < 0 THEN
	Close(PARENT)
	RETURN
END IF

IF dw_duplicate_documents.RowCount() > 0 THEN
	IF MessageBox("Document Import","A document of the same claim, type, date already exists. Do you wish to continue?",Question!,YesNo!,2) = 2 THEN
		cb_ok.Enabled = TRUE
		RETURN
	END IF
END IF

// Validate the filename extension
ls_filename = sle_pathname.Text
li_rtn = wf_validate_file_extension(ls_filename, ls_file_extension) 
IF li_rtn = -1 THEN
	cb_ok.Enabled = TRUE
	RETURN
END IF

// Create logical entries for document.
// this function begins & commits two IMARA txns, then begins another. This txn must be committed by calling object
ll_docid = inv_imaging.nf_create_template_document("w_document_import", il_master_fldid, ls_file_extension, ls_pathname)
IF ll_docid = -1 THEN
	cb_ok.Enabled = TRUE
	RETURN
END IF

// Add the docid to the index form for the docindex.
uo_document_indexing.dw_document_indexing.SetItem(1, "docid", ll_docid)
uo_document_indexing.dw_document_indexing.Update()
IF ImageTrans.nf_handle_error("w_document_import", "", "cb_ok - uo_document_indexing.dw_document_indexing.Update()") < 0 THEN
	Close(PARENT)
	RETURN
END IF

// Now save the document in the new directory.
ll_result = f_copyfile(ls_filename, ls_pathname)
IF ll_result = -1 THEN
	IF MessageBox("Document Import","Click OK if you wish to try again.",Question!,OKCancel!,1) = 1 THEN
		ll_result = f_copyfile(ls_filename, ls_pathname)
	END IF
	IF ll_result = -1 THEN
		ImageTrans.nf_rollback_transaction()
		uo_document_indexing.dw_document_indexing.Reset()
		uo_document_indexing.dw_document_indexing.InsertRow(0)
		sle_pathname.text = ""
		RETURN
	END IF
END IF

// If the document is marked as paid then record it in the Claim database.
IF uo_document_indexing.dw_document_indexing.GetItemString(1,"paid_flag") = "Y" THEN

// Make sure the document is not already marked as paid.
	ll_result = dw_indexing_paid_documents.Retrieve(ll_docid)
	IF SQLCA.nf_handle_error("w_document_import", "", "cb_ok - dw_indexing_paid_documents.Retrieve(ll_docid)") < 0 THEN
		ImageTrans.nf_rollback_transaction()			
		Close(PARENT)
		RETURN
	END IF

	IF ll_result = 0 THEN
		// Not already marked.
		dw_indexing_paid_documents.Reset()
		dw_indexing_paid_documents.InsertRow(0)
		dw_indexing_paid_documents.SetItem(1,"doc_id",ll_docid)
		dw_indexing_paid_documents.SetItem(1,"paid_status_code","O")
		dw_indexing_paid_documents.SetItem(1,"paid_status_explanation_code","05")
		dw_indexing_paid_documents.SetItem(1,"payment_no",0)
		
		SQLCA.nf_begin_transaction()
		
		dw_indexing_paid_documents.Update()
		IF SQLCA.nf_handle_error("w_document_import", "", "cb_ok - dw_indexing_paid_documents.Update()") < 0 THEN
			ImageTrans.nf_rollback_transaction()
			Close(PARENT)
			RETURN
		END IF
		SQLCA.nf_commit_transaction()
	END IF
END IF
		
// Commit the logical entries.
ImageTrans.nf_commit_transaction()

uo_document_indexing.dw_document_indexing.Reset()
uo_document_indexing.cb_provider_search.Visible = FALSE
cb_alt1.Enabled = FALSE
cb_ok.Enabled = FALSE
cb_import.Enabled = TRUE
cb_cancel.Enabled = FALSE
sle_pathname.text = ""

// Refresh the worksheet document list.
iw_sheet.wf_list_documents()
st_message.text = "Document successfully imported."

end event

type cb_import from commandbutton within w_document_import
integer x = 1467
integer y = 1672
integer width = 320
integer height = 100
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Import"
end type

event clicked;Integer li_rtn 
String  ls_filename, ls_file_extension

cb_ok.Enabled = FALSE
st_message.text = ""

// Let the user choose the file/document to import.
li_rtn = GetFileOpenName("Document Import", sle_pathname.text, ls_filename)
IF li_rtn < -1 THEN
	Close(PARENT)
	RETURN
ELSE
	IF li_rtn = 0 THEN  // = 0 when user hits cancel button
		RETURN
	END IF
END IF

// Validate the filename extension
li_rtn = wf_validate_file_extension(ls_filename, ls_file_extension) 
IF li_rtn = -1 THEN
	cb_import.PostEvent(Clicked!)
	RETURN
END IF

// Insert a row and set-up some defaults.
uo_document_indexing.dw_document_indexing.Reset()
uo_document_indexing.dw_document_indexing.InsertRow(0)
uo_document_indexing.dw_document_indexing.SetItem(1,'claim_no',iw_sheet.dw_basic_claim.GetItemNumber(1,'claim_no'))
IF uo_document_indexing.uf_set_claim(iw_sheet.dw_basic_claim.GetItemNumber(1,'claim_no')) = -1 THEN
	MessageBox('Import Failure','Unable to determine claim number.')
	RETURN
END IF
uo_document_indexing.dw_document_indexing.SetItem(1,"date_on_document",Date(f_server_datetime()))
uo_document_indexing.dw_document_indexing.SetItem(1,"source_code","I")
uo_document_indexing.dw_document_indexing.SetItem(1,"sent_flag","N")
uo_document_indexing.uf_date_received(TRUE)
uo_document_indexing.cb_provider_search.Visible = TRUE
uo_document_indexing.dw_document_indexing.Modify("document_desc.Protect=1")
cb_alt1.Enabled = TRUE
cb_ok.Enabled = TRUE
cb_cancel.Enabled = TRUE
cb_import.Enabled = FALSE

st_message.text = "Make sure the document is not open before clicking the Save button."

uo_document_indexing.dw_document_indexing.SetFocus()

end event

type cb_alt1 from commandbutton within w_document_import
integer x = 1595
integer y = 1680
integer width = 128
integer height = 72
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&?"
end type

on clicked;STRING	ls_colname

ls_colname = uo_document_indexing.dw_document_indexing.GetColumnName()

IF ls_colname = 'service_provider_no' THEN
	uo_document_indexing.cb_provider_search.TriggerEvent(Clicked!)
END IF

end on

