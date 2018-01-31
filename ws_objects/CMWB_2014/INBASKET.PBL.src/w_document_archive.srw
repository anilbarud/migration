$PBExportHeader$w_document_archive.srw
$PBExportComments$Window to allow user to archive documents
forward
global type w_document_archive from w_a_tool
end type
type dw_archive_reasons from u_dw_online within w_document_archive
end type
type mle_other from multilineedit within w_document_archive
end type
type dw_transfer_information from u_dw_online within w_document_archive
end type
type dw_paid_documents from u_dw_online within w_document_archive
end type
type cb_transfer from commandbutton within w_document_archive
end type
type st_reason from statictext within w_document_archive
end type
type gb_2 from groupbox within w_document_archive
end type
type gb_1 from groupbox within w_document_archive
end type
type gb_other from groupbox within w_document_archive
end type
type cb_arch_remlink from commandbutton within w_document_archive
end type
type dw_master_documents from u_dw_online within w_document_archive
end type
type dw_archived_documents from u_dw_online within w_document_archive
end type
type uo_image_append from u_image_append within w_document_archive
end type
end forward

global type w_document_archive from w_a_tool
integer width = 3150
integer height = 1800
boolean resizable = false
dw_archive_reasons dw_archive_reasons
mle_other mle_other
dw_transfer_information dw_transfer_information
dw_paid_documents dw_paid_documents
cb_transfer cb_transfer
st_reason st_reason
gb_2 gb_2
gb_1 gb_1
gb_other gb_other
cb_arch_remlink cb_arch_remlink
dw_master_documents dw_master_documents
dw_archived_documents dw_archived_documents
uo_image_append uo_image_append
end type
global w_document_archive w_document_archive

type variables
DATAWINDOWCHILD	idw_reason_codes
N_IMAGING		inv_imaging
LONG			il_master_row_old
LONG			il_archive_row_old
S_WINDOW_MESSAGE	istr_window_message
end variables

forward prototypes
public subroutine wf_breakpay ()
end prototypes

public subroutine wf_breakpay ();LONG		ll_docid, ll_selected_row, ll_fldid
BOOLEAN	lb_indexed

ll_selected_row = dw_master_documents.GetSelectedRow(0)
IF ll_selected_row = 0 THEN
   MessageBox("Document Archive","Error determining selected document. Please try again.")
	Return
END IF

/* Get document id.
*/
	ll_docid = dw_master_documents.GetItemNumber(ll_selected_row,"ref_docid")
	IF ll_docid = 0 THEN
	   MessageBox("Document Archive","Error determining selected document. Please try again.")
		Return
	END IF

/* Call window to allow removal of linkage.
*/
	istr_window_message.al_doubleparm[1] = ll_docid
	OpenWithParm(w_breakpay,istr_window_message)

end subroutine

event open;call super::open;Long    ll_row, ll_num_rows, ll_claim_no
Integer li_rtn
STRING  ls_imaged_claim_flag

// Create an instance of the user object for the imaging functions.
inv_imaging = CREATE n_imaging

ll_claim_no = iw_active_sheet.dw_basic_claim.GetItemNumber(1,'claim_no')

SELECT	imaged_claim_flag
INTO		:ls_imaged_claim_flag
FROM		CLAIM_MASTER
WHERE		claim_no = :ll_claim_no
USING		ImageTrans;
ImageTrans.nf_Handle_Error('w_document_archive','open event','SELECT	imaged_claim_flag FROM CLAIM_MASTER...')

//	Create an instance of the user object for the view function.
dw_master_documents.SetTransObject(SQLCA)
dw_master_documents.Retrieve(ll_claim_no,ls_imaged_claim_flag,'N')
SQLCA.nf_handle_error('w_document_archive', 'dw_master_documents', 'open')


li_rtn = dw_archived_documents.SetTransObject(ImageTrans)
ll_num_rows = dw_archived_documents.Retrieve(iw_active_sheet.dw_basic_claim.GetItemNumber(1,"claim_no"))
IF ImageTrans.nf_handle_error("dw_archived_documents","w_document_archive","open") < 0 THEN
	Close(THIS)
	RETURN
END IF

IF dw_master_documents.RowCount() = 0 AND dw_archived_documents.RowCount() = 0 THEN
	MessageBox("Document Archiving","There are no documents to archive or recover.")
	Close(THIS)
	RETURN
END IF

li_rtn = dw_paid_documents.SetTransObject(SQLCA)

dw_master_documents.uf_setselect(1)
dw_archived_documents.uf_setselect(1)

li_rtn = dw_archive_reasons.SetTransObject(ImageTrans)
li_rtn = dw_archive_reasons.GetChild("archive_reason_code",idw_reason_codes)
ll_row = dw_archive_reasons.InsertRow(0)

dw_master_documents.SetFocus()
dw_master_documents.SetRow(1)
dw_master_documents.TriggerEvent(RowFocusChanged!)

end event

on closequery;IF IsValid(inv_imaging) THEN
	Destroy inv_imaging
END IF

end on

on w_document_archive.create
int iCurrent
call super::create
this.dw_archive_reasons=create dw_archive_reasons
this.mle_other=create mle_other
this.dw_transfer_information=create dw_transfer_information
this.dw_paid_documents=create dw_paid_documents
this.cb_transfer=create cb_transfer
this.st_reason=create st_reason
this.gb_2=create gb_2
this.gb_1=create gb_1
this.gb_other=create gb_other
this.cb_arch_remlink=create cb_arch_remlink
this.dw_master_documents=create dw_master_documents
this.dw_archived_documents=create dw_archived_documents
this.uo_image_append=create uo_image_append
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_archive_reasons
this.Control[iCurrent+2]=this.mle_other
this.Control[iCurrent+3]=this.dw_transfer_information
this.Control[iCurrent+4]=this.dw_paid_documents
this.Control[iCurrent+5]=this.cb_transfer
this.Control[iCurrent+6]=this.st_reason
this.Control[iCurrent+7]=this.gb_2
this.Control[iCurrent+8]=this.gb_1
this.Control[iCurrent+9]=this.gb_other
this.Control[iCurrent+10]=this.cb_arch_remlink
this.Control[iCurrent+11]=this.dw_master_documents
this.Control[iCurrent+12]=this.dw_archived_documents
this.Control[iCurrent+13]=this.uo_image_append
end on

on w_document_archive.destroy
call super::destroy
destroy(this.dw_archive_reasons)
destroy(this.mle_other)
destroy(this.dw_transfer_information)
destroy(this.dw_paid_documents)
destroy(this.cb_transfer)
destroy(this.st_reason)
destroy(this.gb_2)
destroy(this.gb_1)
destroy(this.gb_other)
destroy(this.cb_arch_remlink)
destroy(this.dw_master_documents)
destroy(this.dw_archived_documents)
destroy(this.uo_image_append)
end on

type st_title from w_a_tool`st_title within w_document_archive
integer width = 3122
string text = "Document Archiving"
end type

type cb_close from w_a_tool`cb_close within w_document_archive
integer x = 2350
integer y = 1644
integer width = 274
integer taborder = 100
end type

type dw_archive_reasons from u_dw_online within w_document_archive
integer x = 1010
integer y = 1428
integer width = 1243
integer height = 100
integer taborder = 60
string dataobject = "d_archive_reasons"
boolean border = false
end type

on itemchanged;call u_dwa::itemchanged;/*	If the reason code is 'Other' then show the multi-line edit field so that the
	user can enter a reason.
*/
	IF GetText() = 'AO' OR GetText() = 'RO' THEN
		gb_other.Show( )
	   mle_other.Show( )
	   mle_other.SetFocus( )
	ELSE
		mle_other.Hide( )
		gb_other.Hide( )
	END IF

end on

type mle_other from multilineedit within w_document_archive
boolean visible = false
integer x = 32
integer y = 1576
integer width = 1678
integer height = 204
integer taborder = 110
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
boolean border = false
end type

type dw_transfer_information from u_dw_online within w_document_archive
boolean visible = false
integer x = 2194
integer y = 1360
integer width = 485
integer height = 92
integer taborder = 120
boolean bringtotop = true
boolean enabled = false
string dataobject = "d_transfer_information"
end type

type dw_paid_documents from u_dw_online within w_document_archive
boolean visible = false
integer x = 2194
integer y = 1464
integer height = 92
integer taborder = 50
boolean enabled = false
string dataobject = "d_indexing_paid_documents"
end type

type cb_transfer from commandbutton within w_document_archive
integer x = 1746
integer y = 1644
integer width = 274
integer height = 100
integer taborder = 90
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Transfer"
end type

event clicked;Long   ll_claim_no, ll_result, ll_fldid, ll_docid, ll_rownum
String ls_transfer_to, ls_transfer_from, ls_reason, ls_other, ls_type, ls_transfer, ls_imaged_claim_flag

cb_transfer.Enabled = FALSE
cb_close.Enabled = FALSE

//	Get values for variables.
ll_fldid    = dw_transfer_information.GetItemNumber(1,"fldid")
ll_docid    = dw_transfer_information.GetItemNumber(1,"docid")
ls_type     = dw_transfer_information.GetItemString(1,"type")
ls_transfer = dw_transfer_information.GetItemString(1,"transfer")
ls_other    = mle_other.text
ll_claim_no = dw_transfer_information.GetItemNumber(1,"claim_no")
ls_imaged_claim_flag = dw_transfer_information.GetItemString(1,"imaged_claim_flag")

IF ls_type = "ARX" THEN
	MessageBox("Error", "Documents of type ARX (Prescription Drugs Via ABCC) can not be archived.", Exclamation!)
	GOTO Common_Exit
END IF

IF ls_transfer = 'A' THEN
	ls_reason = dw_archive_reasons.GetItemString(1,"archive_reason_code")
END IF

// Check to make sure that all the variables required are there.
IF IsNull(ls_transfer) OR (ls_transfer <> 'A' AND ls_transfer <> 'R') THEN
	MessageBox("Transfer","An error has occurred in transfer direction.~r~nPlease make sure one has been selected.",StopSign!,OK!)
	dw_master_documents.SetFocus()
	GOTO Common_Exit
ELSE
	IF ls_transfer = 'A' THEN
		ls_transfer_to = 'ARCHIVE'
		ls_transfer_from = 'PRODUCTION'
	ELSE
		ls_transfer_to = 'PRODUCTION'
		ls_transfer_from = 'ARCHIVE'
	END IF
END IF

// Check that there is a folder id and document id.
IF ll_fldid <= 0 OR IsNull(ll_fldid) OR ll_docid <= 0 OR IsNull(ll_docid) THEN
	MessageBox("Error","Could not determine selected document to transfer",StopSign!)
	GOTO Common_Exit
END IF

IF ls_transfer = 'A' AND IsNull(ls_reason) THEN
	MessageBox("Reason","Reason for transfer must be chosen.",Exclamation!)
	dw_archive_reasons.SetFocus()
	GOTO Common_Exit
END IF

IF ls_reason = 'AO' THEN
	ls_other = mle_other.text
	IF IsNull(ls_other) OR ls_other = "" THEN
		MessageBox("Other Reason","Must be specified for Other reason code",Exclamation!,OK!)
		mle_other.SetFocus()
		GOTO Common_Exit
	END IF
END IF

IF ls_transfer = 'A' AND (Left(ls_type,1) = 'A' OR ls_type = 'SDC' OR ls_type = 'SDD' OR ls_type = 'MPC' OR ls_type = 'MPD') THEN
	// Accounts with payment information are not allowed to be archived.
	ll_result = dw_paid_documents.Retrieve(ll_docid)

	IF SQLCA.nf_handle_error("dw_paid_documents","w_document_archive","clicked for cb_transfer") < 0 THEN
		Close(PARENT)
		RETURN
	ELSEIF ll_result > 0 THEN
		IF MessageBox("Document Archiving","Document is an account with payment records therefore it can not be transferred. Do you wish to break payment / document links? (You must then retry the transfer!)",Question!,YesNo!) = 1 THEN
			wf_breakpay()
		END IF
		GOTO Common_Exit
	END IF
END IF

// If you are trying to recover a document of type: AC!, AD!, MP!, or SD! type then
// you can't because we no longer allow these types (removal of split documents).
IF (ls_type = "AC!" OR ls_type = "AD!" OR ls_type = "SD!" OR ls_type = "MP!") THEN
	IF ls_transfer = "A" THEN
		IF MessageBox("Confirm","This document can not be recovered once it is archived. Do you still wish to continue?",Question!,YesNo!) = 2 THEN
			GOTO Common_Exit
		END IF
	ELSEIF ls_transfer = "R" THEN
		MessageBox("Split Document","This document can not be recovered because its type is " + ls_type, StopSign!)
		GOTO Common_Exit
	END IF
END IF

IF MessageBox("Confirm","You have requested document from~r~n~r~n          Claim: " + String(iw_active_sheet.dw_basic_claim.GetItemNumber(1,"claim_no")) &
+ "~r~n~r~n be transferred~r~n~r~n From: " + ls_transfer_from + "~r~n To: " + ls_transfer_to + "~r~n~r~nDo you wish to complete this transfer?",Question!,YesNo!) = 1 THEN
	IF ls_transfer = "A" THEN
		IF inv_imaging.nf_archive_document(ll_claim_no,ls_imaged_claim_flag,ll_docid,ll_fldid,ls_reason,ls_other,"w_document_archive") = -1 THEN
			GOTO Common_Exit
		END IF
	ELSE
		IF inv_imaging.nf_recover_document(ll_docid,ll_fldid,"w_document_archive") = -1 THEN
			GOTO Common_Exit
		END IF
	END IF
ELSE
	GOTO Common_Exit
END IF

// Refresh the data.
dw_archived_documents.Retrieve(iw_active_sheet.dw_basic_claim.GetItemNumber(1,"claim_no"))
IF ImageTrans.nf_handle_error("dw_archived_documents","w_document_archive","clicked for cb_transfer") < 0 THEN
	Close(PARENT)
	RETURN
END IF

//	Refresh the document list on the sheet which will in turn refresh the master document list.
iw_active_sheet.wf_list_documents()
dw_master_documents.SetFocus()
dw_master_documents.SetRow(1)
dw_master_documents.TriggerEvent(RowFocusChanged!)

Common_Exit:
	cb_transfer.Enabled = TRUE
	cb_close.Enabled = TRUE
	RETURN
end event

type st_reason from statictext within w_document_archive
integer x = 393
integer y = 1436
integer width = 590
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Reason For Archiving :"
alignment alignment = center!
boolean focusrectangle = false
end type

type gb_2 from groupbox within w_document_archive
integer x = 1573
integer y = 120
integer width = 1527
integer height = 1272
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Archived Documents"
end type

type gb_1 from groupbox within w_document_archive
integer x = 9
integer y = 120
integer width = 1527
integer height = 1272
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Master Documents"
end type

type gb_other from groupbox within w_document_archive
integer x = 18
integer y = 1512
integer width = 1705
integer height = 280
integer taborder = 70
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Specify ~'Other~' Reason For Archiving :"
end type

type cb_arch_remlink from commandbutton within w_document_archive
integer x = 2048
integer y = 1644
integer width = 274
integer height = 100
integer taborder = 80
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Del Link"
end type

event clicked;Long   ll_result, ll_docid
String ls_transfer, ls_type

ll_docid    = dw_transfer_information.GetItemNumber(1,"docid")
ls_transfer = dw_transfer_information.GetItemString(1,"transfer")
ls_type = dw_transfer_information.GetItemString(1,"type")

IF ls_type = "ARX" THEN
	MessageBox("Error", "Documents of type ARX (Prescription Drugs Via ABCC) can not have their link removed.", Exclamation!)
	RETURN
END IF

IF ls_transfer <> 'A' THEN
   MessageBox("Document Archiving","Link cannot exist for archived documents.",Exclamation!)
	RETURN
END IF

ll_result = dw_paid_documents.Retrieve(ll_docid)

IF SQLCA.nf_handle_error("dw_paid_documents","w_document_archive","clicked for cb_arch_remlink") < 0 THEN
	Close(PARENT)
	RETURN
ELSE
	IF ll_result = 0 THEN
	   MessageBox("Document Archiving","Document is NOT an account with payment records therefore link does not exist.",Exclamation!)
		RETURN
	END IF
END IF

wf_breakpay()

end event

type dw_master_documents from u_dw_online within w_document_archive
integer x = 46
integer y = 200
integer width = 1454
integer height = 1152
integer taborder = 40
boolean bringtotop = true
string dataobject = "d_master_documents_sp"
boolean hscrollbar = true
boolean vscrollbar = true
end type

on rowfocuschanged;call u_dw_online::rowfocuschanged;LONG	ll_rownum, ll_docid

ll_rownum = GetRow()
IF ll_rownum = 0 THEN
	RETURN
END IF

/* Unhighlight the archived document list.
*/
	dw_archived_documents.SelectRow(0,FALSE)
	uf_processSelect(ll_rownum,"Keyboard")
	dw_archive_reasons.Reset()
	dw_archive_reasons.InsertRow(0)
	st_reason.Show( )
   mle_other.Hide( )
	gb_other.Hide( )

/* Get document id, folder id, document name, and transfer direction of the selected document.
*/
	dw_transfer_information.Reset()
	dw_transfer_information.InsertRow(0)
	dw_transfer_information.SetItem(1,"fldid", GetItemNumber(ll_rownum,"claimsmaster_folderid"))
	dw_transfer_information.SetItem(1,"docid",GetItemNumber(ll_rownum,"ref_docid"))
	dw_transfer_information.SetItem(1,"type",GetItemString(ll_rownum,"docindex_type"))
	dw_transfer_information.SetItem(1,"transfer","A")
	dw_transfer_information.SetItem(1,"claim_no",iw_active_sheet.dw_basic_claim.GetItemNumber(1,"claim_no"))
	dw_transfer_information.SetItem(1,"imaged_claim_flag",iw_active_sheet.dw_basic_claim.GetItemString(1,"imaged_flag"))

	il_master_row_old = dw_master_documents.GetSelectedRow(0)

end on

event doubleclicked;call super::doubleclicked;LONG	ll_rownum, ll_docid
string ls_doc_type

ll_rownum = GetRow()
IF ll_rownum = 0 THEN
	RETURN
END IF

ll_docid = GetItemNumber(ll_rownum,"ref_docid")

/* View the document.
*/



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
				if uo_image_append.of_append_image(ll_docid) < 1 then return
			case else
				/* execel and word */
				iw_active_sheet.iu_dw_document_path.f_manage_document(ll_docid,"V","NORMAL")
		end choose
end if
		
	
	
	
	


end event

on clicked;call u_dw_online::clicked;IF il_master_row_old = dw_master_documents.GetSelectedRow(0) THEN
	dw_master_documents.TriggerEvent(RowFocusChanged!)
END IF
end on

type dw_archived_documents from u_dw_online within w_document_archive
integer x = 1609
integer y = 200
integer width = 1454
integer height = 1152
integer taborder = 30
boolean bringtotop = true
string dataobject = "d_archived_documents"
boolean hscrollbar = true
boolean vscrollbar = true
end type

on rowfocuschanged;call u_dw_online::rowfocuschanged;LONG	ll_rownum, ll_docid

ll_rownum = GetRow()
IF ll_rownum = 0 THEN
	RETURN
END IF

/* Unhighlight the master document list.
*/
	dw_master_documents.SelectRow(0,FALSE)
	uf_processSelect(ll_rownum,"Keyboard")
	dw_archive_reasons.Reset()
	st_reason.Hide( )
   mle_other.Hide( )
	gb_other.Hide( )

/* Get document id, folder id, document name, and transfer direction of the selected document.
*/
	dw_transfer_information.Reset()
	dw_transfer_information.InsertRow(0)
	dw_transfer_information.SetItem(1,"fldid", GetItemNumber(ll_rownum,"claimsmaster_folderid"))
	dw_transfer_information.SetItem(1,"docid",GetItemNumber(ll_rownum,"ref_docid"))
	dw_transfer_information.SetItem(1,"type",GetItemString(ll_rownum,"docindex_type"))
	dw_transfer_information.SetItem(1,"transfer","R")
	dw_transfer_information.SetItem(1,"claim_no",iw_active_sheet.dw_basic_claim.GetItemNumber(1,"claim_no"))
	dw_transfer_information.SetItem(1,"imaged_claim_flag",iw_active_sheet.dw_basic_claim.GetItemString(1,"imaged_flag"))

	il_archive_row_old = dw_archived_documents.GetSelectedRow(0)

end on

event doubleclicked;call super::doubleclicked;LONG	ll_rownum, ll_docid
string ls_doc_type

ll_rownum = GetRow()
IF ll_rownum = 0 THEN
	RETURN
END IF

ll_docid = GetItemNumber(ll_rownum,"ref_docid")

/* View the document.
*/
	
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
				if uo_image_append.of_append_image(ll_docid) < 1 then return
			case else
				/* execel and word */
				iw_active_sheet.iu_dw_document_path.f_manage_document(ll_docid,"V","NORMAL")
		end choose
end if
		
	
end event

on clicked;call u_dw_online::clicked;IF il_archive_row_old = dw_archived_documents.GetSelectedRow(0) OR il_archive_row_old = 0 THEN
	dw_archived_documents.TriggerEvent(RowFocusChanged!)
END IF
end on

type uo_image_append from u_image_append within w_document_archive
boolean visible = false
integer x = 2322
integer y = 1452
integer taborder = 70
boolean bringtotop = true
end type

on uo_image_append.destroy
call u_image_append::destroy
end on

