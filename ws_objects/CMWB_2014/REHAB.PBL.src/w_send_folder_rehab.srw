$PBExportHeader$w_send_folder_rehab.srw
$PBExportComments$window to send messages through the inbasket from the rehab task tab
forward
global type w_send_folder_rehab from window
end type
type dw_send_documents from u_dw_online within w_send_folder_rehab
end type
type cb_send_ok from commandbutton within w_send_folder_rehab
end type
type cb_send_cancel from commandbutton within w_send_folder_rehab
end type
end forward

global type w_send_folder_rehab from window
integer x = 2199
integer y = 688
integer width = 1563
integer height = 780
boolean titlebar = true
string title = "Send Documents"
windowtype windowtype = response!
long backcolor = 67108864
dw_send_documents dw_send_documents
cb_send_ok cb_send_ok
cb_send_cancel cb_send_cancel
end type
global w_send_folder_rehab w_send_folder_rehab

type variables
BOOLEAN			I_Authorized_Access
W_rehab_SHEET			iw_sheet
S_SEND_DOC_PARAMETERS	is_send_doc_parameters
N_IMAGING			inv_imaging

end variables

event open;/* APPLICATION SECURITY CODE.
*/
	G_PFSecurity.UOF_Check_Access(THIS)
	THIS.I_Authorized_Access = TRUE				//declared as an instance variable.

LONG					ll_rowcount, ll_results, ll_task_no, ll_event_no
STRING				ls_imaged_claim_flag
DATAWINDOWCHILD	ldwc_dropdown

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


/* Create an instance of the user object for the imaging functions.
*/
	inv_imaging = CREATE n_imaging

/* Get the name of the active work sheet.
*/
	iw_sheet = w_frame.GetActiveSheet()

	IF IsValid(iw_sheet) = FALSE THEN
   	MessageBox("Document Indexing","Error determining active sheet. You may have to reboot and try again.")
		Close(THIS)
		Return
	END IF

	is_send_doc_parameters = Message.PowerObjectParm
	IF IsNull(is_send_doc_parameters.claim_no) OR is_send_doc_parameters.claim_no = 0 THEN
		MessageBox("Send Documents","Cannot Determine claim number",Exclamation!)
		Close(THIS)
		RETURN
	END IF

	IF is_send_doc_parameters.document_list.GetSelectedRow(0) = 0 AND is_send_doc_parameters.msg_mode = FALSE THEN
		MessageBox("Send Documents","You must select one or more documents first",Exclamation!)
		Close(THIS)
		RETURN
	END IF

	dw_send_documents.SetTransObject(ImageTrans)
	dw_send_documents.GetChild("catid",ldwc_dropdown)
	ldwc_dropdown.SetTransObject(ImageTrans)

/* Retrieve the list of categories the user can send to.
*/
	ll_rowcount = ldwc_dropdown.Retrieve(vgst_user_profile.user_id)
	IF ImageTrans.nf_handle_error("ldwc_dropdown","w_send_folder","Open") < 0 THEN
		Close(THIS)
		RETURN
	END IF

/* If no rows found then user has not set up any buckets to send to.
*/
	IF ll_rowcount <= 0 THEN
	   MessageBox("Send Documents", "No 'Send To:' categories found for sending documents.~r~n" + &
					  "You must select 'Send To' categories using the Options - Maintain In/Out Buckets menu option.",StopSign!,OK!)
	   Close(THIS)
		RETURN
	END IF

/* Insert blank row into the datawindow.
*/
	dw_send_documents.InsertRow(0)
	dw_send_documents.SetItem(1,"claim_no",is_send_doc_parameters.claim_no)

/* Set defaults for phone message notification usage.
*/
	IF is_send_doc_parameters.msg_mode THEN
		CHOOSE CASE	is_send_doc_parameters.document_list.DataObject
			CASE 'd_progress_notes_for_task_list'
				This.Title = 'Send Event Entry'
				ll_task_no = is_send_doc_parameters.document_list.GetItemNumber(is_send_doc_parameters.document_list.GetRow(),"task_no")
				ll_event_no = is_send_doc_parameters.document_list.GetItemNumber(is_send_doc_parameters.document_list.GetRow(),"event_no")
				dw_send_documents.SetItem(1,"action_note","See Task" + String(ll_task_no) + ", Prgs" + String(ll_event_no))
			CASE ELSE
				This.Title = 'Send Task Entry'
				dw_send_documents.SetItem(1,"action_note","See Task " + String(is_send_doc_parameters.document_list.GetItemNumber(1,"task_no")))
		END CHOOSE
		dw_send_documents.SetColumn("action_code")
		dw_send_documents.SetItem(1,"action_date",f_server_datetime())
	END IF

end event

on closequery;IF IsValid(inv_imaging) THEN
	Destroy inv_imaging
END IF

end on

on w_send_folder_rehab.create
this.dw_send_documents=create dw_send_documents
this.cb_send_ok=create cb_send_ok
this.cb_send_cancel=create cb_send_cancel
this.Control[]={this.dw_send_documents,&
this.cb_send_ok,&
this.cb_send_cancel}
end on

on w_send_folder_rehab.destroy
destroy(this.dw_send_documents)
destroy(this.cb_send_ok)
destroy(this.cb_send_cancel)
end on

type dw_send_documents from u_dw_online within w_send_folder_rehab
integer x = 27
integer y = 80
integer width = 1454
integer height = 396
integer taborder = 10
string dataobject = "d_send_specs"
boolean border = false
end type

type cb_send_ok from commandbutton within w_send_folder_rehab
integer x = 393
integer y = 540
integer width = 334
integer height = 108
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
boolean default = true
end type

event clicked;LONG		ll_fldid, ll_selected_row,	ll_catid, ll_setid, ll_docid, ll_results, ll_claim_no, ll_event_no
STRING   ls_action, ls_action_date, ls_keyword, ls_claimant_name
DATETIME	ldt_action_date

SetPointer(HourGlass!)
Enabled = FALSE
cb_send_cancel.Enabled = FALSE

/* If there is no row, then there was a problem so get out.
*/
	IF dw_send_documents.RowCount() <> 1 THEN
		Close(PARENT)
		RETURN
	END IF

	IF dw_send_documents.AcceptText() = -1 THEN
		RETURN
	END IF

	IF iw_sheet.dw_basic_claim.RowCount() <> 1 THEN

/*	error - trigger a application error
*/
		IF IsValid(iw_sheet) THEN
			Error.Text = iw_sheet.Title
		ELSE
			Error.Text = 'Unable to determine active sheet'
		END IF
		Error.Text = Error.Text + 'Row Count for basic claim <> 1 = ' + String(iw_sheet.dw_basic_claim.RowCount())
		Error.WindowMenu="cmwb"
		Error.Object="w_send_folder"
		Error.ObjectEvent="clicked for cb_ok"
		SignalError()
		Return
	END IF	

/* Get variables, and validate.
*/
	ls_claimant_name = iw_sheet.dw_basic_claim.GetItemString(1,"given_names") + " " + iw_sheet.dw_basic_claim.GetItemString(1,"last_name")
	ll_claim_no		  = dw_send_documents.GetItemNumber(1,"claim_no")
	ls_action        = dw_send_documents.GetItemString(1,"action_code")
	ll_catid         = dw_send_documents.GetItemNumber(1,"catid")
	ldt_action_date  = dw_send_documents.GetItemDateTime(1,"action_date")
	ls_keyword       = dw_send_documents.GetItemString(1,"action_note")

	IF IsNull(ls_action) THEN
	   MessageBox("Send Documents","You must select an action code before forwarding documents.",Exclamation!)
		GOTO Normal_Exit
	END IF

	IF IsNull(ll_catid) OR ll_catid <= 0 THEN
	   MessageBox("Send Documents","You must select a destination category before forwarding documents.",Exclamation!)
		GOTO Normal_Exit
	END IF

	IF Date(ldt_action_date) < Date("1900,01,01") THEN
		MessageBox("Send Documents","You must provide an action date greater than or equal to Jan 1, 1900",Exclamation!)
		GOTO Normal_Exit
	END IF
	
	
/* Create a work folder.
*/
	ImageTrans.nf_begin_transaction()
	
	// this function begins & commits its own transaction, then inserts data outside of txn,
	// so it must be enclosed within its own txn
	ll_fldid = inv_imaging.nf_create_workfolder("w_send_folder",ll_catid)
	IF ll_fldid = -1 THEN
		MessageBox("Send Folder","Unable to create work folder. Please try again.")
		GOTO Normal_Exit
	END IF

	ImageTrans.nf_commit_transaction()

	dw_send_documents.SetItem(1,"folderid",ll_fldid)

/* Get the setid for the Category.
*/
	SELECT setid
	  INTO :ll_setid
	  FROM CAT
	 WHERE catid = :ll_catid
	 USING ImageTrans;

	IF ImageTrans.nf_handle_error("Embedded SQL: Select from CAT","w_send_folder","clicked for cb_send_ok") < 0 THEN
	   Close(PARENT)
		RETURN
	END IF
	
	
	ImageTrans.nf_begin_transaction()

/* Add selected documents to the work folder.
*/
	ll_selected_row = 0
	DO WHILE TRUE AND is_send_doc_parameters.msg_mode = FALSE

/* Get the next selected row.
*/
	   ll_selected_row = is_send_doc_parameters.document_list.GetSelectedRow(ll_selected_row)
	   IF ll_selected_row = 0 THEN
			EXIT
		END IF

/* Get the document id of the selcted row.
*/
	   ll_docid = is_send_doc_parameters.document_list.GetItemNumber(ll_selected_row,"ref_docid")
	   IF ll_docid = 0 THEN
	      MessageBox("Send Documents", "Error determining docid for selected document.")
			GOTO Normal_Exit
	   END IF

/* Add the document to the folder.
*/


//******************** SR 196 *****************
//
// note that since this loop will never fire (is_send_doc_parameters.msg_mode always TRUE in this window)
// I have commented out the change that I made to the original code. - kdm 2002/feb/19
//
//********************************************

//		INSERT INTO REF (docid, docfldid, doccatid, docsetid )
//		VALUES (:ll_docid, :ll_fldid, :ll_catid, :ll_setid )
//		USING ImageTrans;
//
//		IF ImageTrans.nf_handle_error("Embedded SQL: Insert into REF","w_send_folder","clicked for cb_send_ok") < 0 THEN
//			Close(PARENT)
//			RETURN
//		END IF

/* Increment the document reference counter.
*/
		UPDATE DOC
			SET docrefcount = docrefcount + 1
		 WHERE docid = :ll_docid
		 USING ImageTrans;

		IF ImageTrans.nf_handle_error("Embedded SQL: Update DOC","w_send_folder","clicked for cb_send_ok") < 0 THEN
			Close(PARENT)
			RETURN
		END IF
	LOOP

/* Index the work folder.
*/
	dw_send_documents.Update()
	IF ImageTrans.nf_handle_error("dw_send_documents","w_send_folder","clicked for cb_send_ok") < 0 THEN
		Close(PARENT)
		RETURN
	END IF

/* Update the folder with the new name.
*/
	UPDATE FLD
		SET fldname = Upper(:ls_action) + CONVERT(varchar(10),:ll_claim_no) + :ls_claimant_name
	 WHERE fldid = :ll_fldid
	 USING ImageTrans;

	IF ImageTrans.nf_handle_error("Embedded SQL: Update FLD","w_send_folder","clicked fro cb_send_ok") < 0 THEN
		RETURN
	END IF

	ImageTrans.nf_commit_transaction()

	Close(PARENT)
	RETURN

Normal_Exit:
	SetPointer(Arrow!)
	Enabled = TRUE
	cb_send_cancel.Enabled = TRUE
   RETURN

end event

type cb_send_cancel from commandbutton within w_send_folder_rehab
integer x = 800
integer y = 540
integer width = 334
integer height = 108
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
end type

on clicked;Close(PARENT)

end on

