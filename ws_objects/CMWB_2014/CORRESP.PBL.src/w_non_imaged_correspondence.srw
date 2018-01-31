$PBExportHeader$w_non_imaged_correspondence.srw
$PBExportComments$Used to maintain non imaged correspondence.
forward
global type w_non_imaged_correspondence from w_a_tool
end type
type cb_add from commandbutton within w_non_imaged_correspondence
end type
type cb_delete from commandbutton within w_non_imaged_correspondence
end type
type cb_save from commandbutton within w_non_imaged_correspondence
end type
type cb_cancel from commandbutton within w_non_imaged_correspondence
end type
type dw_received_document_list from u_dw_online within w_non_imaged_correspondence
end type
type dw_received_document_detail from u_dw_online within w_non_imaged_correspondence
end type
end forward

global type w_non_imaged_correspondence from w_a_tool
boolean resizable = false
event ue_get_documents pbm_custom50
cb_add cb_add
cb_delete cb_delete
cb_save cb_save
cb_cancel cb_cancel
dw_received_document_list dw_received_document_list
dw_received_document_detail dw_received_document_detail
end type
global w_non_imaged_correspondence w_non_imaged_correspondence

type variables
BOOLEAN	ib_promptforsave	// Determines if record has been changed and if need to prompt for saving.
W_SHEET	iw_wsheet	// Contains an instance of the worksheet.
LONG		il_claimno	// Contains the value of the current claim_no on the worksheet.
STRING		is_imagedflag	// Determines if the claim is an imaged claim.
DATETIME	idt_accidentdate	// The accident date for the claim.

end variables

forward prototypes
public function integer wf_next_correspond_no_is ()
public subroutine wf_set_new_record_values ()
public subroutine wf_add_new_record ()
public subroutine wf_check_if_save_needed ()
public subroutine wf_delete_current_record ()
public subroutine wf_save ()
public subroutine wf_update_list ()
public function integer wf_check_mod_rules ()
public function integer wf_retrieve_list ()
public function integer wf_populate_detail ()
end prototypes

on ue_get_documents;call w_a_tool::ue_get_documents;INTEGER ll_rowsinlist, li_ftnresult

/*	Using the value in il_claimno, retrieve the documents and insert a
	blank row into dw_received_document_detail.
*/
	ll_rowsinlist = wf_retrieve_list()
	IF ll_rowsinlist > 0 THEN
		dw_received_document_list.SetRow(1)
		li_ftnresult = wf_populate_detail()
		dw_received_document_detail.Enabled = TRUE
	ELSE
		cb_delete.Enabled = FALSE
		dw_received_document_detail.Enabled = FALSE
	END IF

/*	Warn the user if the current claim is an imaged claim.
*/
	IF is_imagedflag = 'Y' THEN
		MessageBox("Imaged Claim Warning","The claim that you have opened this screen for is an imaged claim.",Exclamation!)
	END IF
	THIS.SetRedraw(TRUE)

end on

public function integer wf_next_correspond_no_is ();LONG		ll_error
INTEGER	li_nextcorrespondno, li_currentcorrespondno

/*	This function uses embedded SQL to select the current maximum
	correspond_no from NONIMAGED_RECVD_CORRESP_LOG. It then adds
	1 to it to get the next correspond_no.
*/
	SELECT IsNull(Max(correspond_no),0)
	  INTO :li_currentcorrespondno
	  FROM NONIMAGED_RECVD_CORRESP_LOG
	 WHERE claim_no = :il_claimno;

	ll_error = SQLCA.nf_handle_error("wf_next_correspond_no_is","w_non_imaged_correspondence","SELECT Max(correspond_no)....")
	IF ll_error < 0 THEN
		RETURN -1
	END IF

	li_nextcorrespondno = li_currentcorrespondno + 1

	RETURN li_nextcorrespondno

end function

public subroutine wf_set_new_record_values ();INTEGER	li_nextcorrespondno
DateTime ldt_currdate

/*	This function sets the claim_no, correspond_no, and date_received
	columns for any new records.
*/
	li_nextcorrespondno = wf_next_correspond_no_is()
	dw_received_document_detail.SetItem(1,'claim_no',il_claimno)
	ldt_currdate = DateTime(Date(f_server_datetime())) // Rob Head changed to server date 98/09/23.
	dw_received_document_detail.SetItem(1,'date_received',ldt_currdate)
	dw_received_document_detail.SetItem(1,'correspond_no',li_nextcorrespondno)

end subroutine

public subroutine wf_add_new_record ();/*	This function adds a new record to dw_received_document_detail.
*/
	IF dw_received_document_detail.Enabled = FALSE THEN dw_received_document_detail.Enabled = TRUE
	dw_received_document_detail.Reset()
	dw_received_document_detail.InsertRow(0)

/*	Call ftn to initialize the column values.
*/
	wf_set_new_record_values()

/*	Set save prompt variable, and set focus to the detail datawindow.
*/
	ib_promptforsave = TRUE
	dw_received_document_detail.SetColumn('document_type_code')
	dw_received_document_detail.SetFocus()

/*	Set buttons.
*/
	cb_add.Enabled = FALSE
	cb_save.Enabled = TRUE
	cb_cancel.Enabled = TRUE
	cb_delete.Enabled = FALSE

end subroutine

public subroutine wf_check_if_save_needed ();INTEGER	li_answer

/*	Check to see if the record needs to be saved (ib_promptforsave = TRUE).
	If so, ask question and if answer is YES then save record.
*/
	IF ib_promptforsave THEN
		li_answer = MessageBox("New/Modified Record","The current record has changes which have yet to be saved, save now?",Exclamation!,YesNo!)
		IF li_answer = 1 THEN
			wf_save()
		END IF
	END IF

	RETURN

end subroutine

public subroutine wf_delete_current_record ();INTEGER	li_answer

/*	This function deletes the current record in dw_received_document_detail
	from the database. It also removes it from dw_received_document_list by
	re-retrieving.
*/
	dw_received_document_detail.DeleteRow(1)
	wf_save()

	RETURN

end subroutine

public subroutine wf_save ();INTEGER	li_saveresult, li_nextcorrespondno
BOOLEAN  lb_retrieve = FALSE
LONG		ll_rownumber

/*	This function applies the change made to the current record in the datawindow
	dw_received_document_detail to the database. But first check to see if all the
	modifications rules have been passed.
*/
	IF wf_check_mod_rules() = 0 THEN
		/* Double check that correspondence number is still good for new records. Rob Head 98/09/23 */
		If dw_received_document_detail.GetItemStatus(1,0,Primary!) = NewModified! Then
			li_nextcorrespondno = wf_next_correspond_no_is()
			If li_nextcorrespondno <> dw_received_document_detail.GetItemNumber(1, 'correspond_no') Then
				dw_received_document_detail.SetItem(1,'correspond_no',li_nextcorrespondno)
				MessageBox('Information', 'Correspondence Number has been changed to ' + String(li_nextcorrespondno) + '.')
				lb_retrieve = TRUE
			End If
		End If
		
		SQLCA.nf_begin_transaction()
		
		dw_received_document_detail.Update(TRUE)
		li_saveresult = SQLCA.nf_handle_error("w_non_imaged_correspondence","dw_received_document_detail","wf_save(): 'dw_received_document_detail.Update()'")
		IF li_saveresult = 0 THEN
			SQLCA.nf_commit_transaction()
			
			ib_promptforsave = FALSE
			If lb_retrieve Then // If/ELSE added by Rob Head 98/09/23
				wf_retrieve_list()
				ll_rownumber = dw_received_document_list.Find("correspond_no = " + String(li_nextcorrespondno),1,dw_received_document_list.RowCount())
				dw_received_document_list.ScrollToRow(ll_rownumber)
			ELSE
				wf_update_list()
			END IF
		END IF
	END IF

	RETURN

end subroutine

public subroutine wf_update_list ();INTEGER	li_ftnresult
LONG		ll_correspondno, ll_rownumber, ll_rowsindw, ll_rowis
BOOLEAN	lb_cansetrow

/*	This function updates dw_received_document_list with any of the changes that
	have been applied to the database.
*/
	ll_rowsindw = dw_received_document_detail.RowCount()
	IF ll_rowsindw = 0 THEN

/*	Record was deleted from the detail, now delete from the list.
*/
		dw_received_document_list.DeleteRow(dw_received_document_list.GetRow())
		li_ftnresult = wf_populate_detail()
		RETURN
	END IF

/*	Find the record in the list with the correspond_no which matches the record 
	in dw_received_document_detail.
*/
	ll_correspondno = dw_received_document_detail.GetItemNumber(1,"correspond_no")
	ll_rownumber = dw_received_document_list.Find("correspond_no = " + String(ll_correspondno),1,dw_received_document_list.RowCount())
	IF ll_rownumber = 0 THEN

/*	Not found, so it's an add.
*/
		ll_rownumber = dw_received_document_list.InsertRow(0)
		dw_received_document_list.SetRow(ll_rownumber)
		dw_received_document_list.SetItem(ll_rownumber,'correspond_no',dw_received_document_detail.GetItemNumber(1,'correspond_no'))
		dw_received_document_list.SetItem(ll_rownumber,'date_received',dw_received_document_detail.GetItemDatetime(1,'date_received'))
		dw_received_document_list.SetItem(ll_rownumber,'document_type_code',dw_received_document_detail.GetItemString(1,'document_type_code'))
//		dw_received_document_list.SetItem(ll_rownumber,'create_user_id',dw_received_document_detail.GetItemString(1,'create_user_id'))
		dw_received_document_list.SetItem(ll_rownumber,'create_user_id',vgst_user_profile.user_id)
		dw_received_document_list.ScrollToRow(ll_rownumber)
		li_ftnresult = wf_populate_detail()
	ELSE

/*	Found, so it's a modify.
*/
		dw_received_document_list.SetItem(ll_rownumber,'date_received',dw_received_document_detail.GetItemDatetime(1,'date_received'))
		dw_received_document_list.SetItem(ll_rownumber,'document_type_code',dw_received_document_detail.GetItemString(1,'document_type_code'))
		li_ftnresult = wf_populate_detail()
	END IF

	RETURN

end subroutine

public function integer wf_check_mod_rules ();INTEGER				li_rowsindw, li_entryfoundat
STRING				ls_doctypecode
DATETIME				ldt_createdate, ldt_receiveddate
DATAWINDOWCHILD	ldwc_child

/*	This function checks to see if all the mandatory, validation, and
	consistency rules have been satisfied prior to saving the record.
	If the save is for a delete, then do not need to check the rules.
*/
	dw_received_document_detail.AcceptText()
	IF dw_received_document_detail.RowCount() = 1 THEN
		ldt_receiveddate	= dw_received_document_detail.GetItemDatetime(1,'date_received')
		ls_doctypecode		= dw_received_document_detail.GetItemString(1,'document_type_code')
		ldt_createdate		= dw_received_document_detail.GetItemDatetime(1,'create_date')

/*	Check to see if all mandatory fields have been entered.
*/
		IF IsNull(ls_doctypecode) or ls_doctypecode = '' THEN
			MessageBox("Validation Error","Document Type is not valid.",Exclamation!)
			RETURN -1
		END IF

		IF IsNull(ldt_receiveddate) THEN
			MessageBox("Validation Error","Date Received is not valid.",Exclamation!)
			RETURN -1
		END IF

/*	Check to see if all validation rules have been satisfied.
*/
		IF Date(ldt_receiveddate) > Today() THEN
			MessageBox("Invalid Date Received","Date Received can not be in the future.",Exclamation!)
			RETURN -1
		END IF

		dw_received_document_detail.GetChild('document_type_code',ldwc_child)
		li_rowsindw = ldwc_child.RowCount()
		li_entryfoundat = ldwc_child.Find("document_type_code = '" + ls_doctypecode + "'",1,li_rowsindw)
		IF li_entryfoundat = 0 THEN
			MessageBox("Invalid Document Type","Document Type is not valid.",Exclamation!)
			RETURN -1
		END IF

/*	Check to see if all consistency rules have been satisfied.
*/
		IF not IsNull(ldt_createdate) THEN
			IF Date(ldt_receiveddate) > Date(ldt_createdate) THEN
				MessageBox("Invalid Date Received","Date Received can not greater than the Create Date.",Exclamation!)
				RETURN -1
			END IF
		END IF

		IF Date(ldt_receiveddate) < Date(idt_accidentdate) THEN
			MessageBox("Invalid Date Received","Date Received can not be before Accident Date.",Exclamation!)
			RETURN -1
		END IF
	END IF

	RETURN 0

end function

public function integer wf_retrieve_list ();INTEGER	ll_rowsinlist

/*	This function retrieve all the non-imaged documents for a
	given claim_no.
*/
	ll_rowsinlist = dw_received_document_list.Retrieve(il_claimno)
	IF SQLCA.nf_handle_error('w_non_imaged_correspondence','wf_retrieve_list', 'dw_received_document_list.Retrieve(il_claimno)') < 0 THEN
		Return -1
	END IF

	RETURN ll_rowsinlist

end function

public function integer wf_populate_detail ();LONG	ll_correspondno, ll_rownumber

/*	This function populates the datawindow dw_received_document_detail with the
	currently highlighted record in dw_received_document_list.
*/
	ll_rownumber = dw_received_document_list.GetRow()
	IF ll_rownumber > 0 THEN
		dw_received_document_list.uf_processselect(ll_rownumber,"Mouse")
		ll_correspondno = dw_received_document_list.GetItemNumber(ll_rownumber,"correspond_no")
		IF not IsNull(ll_correspondno) THEN
			dw_received_document_detail.Retrieve(il_claimno,ll_correspondno)
			IF SQLCA.nf_handle_error('w_non_imaged_correspondence','wf_populate_detail', 'dw_received_document_detail.Retrieve(il_claimno,ll_correspondno)') < 0 THEN
				RETURN -1
			END IF
			dw_received_document_detail.SetColumn('document_type_code')
			dw_received_document_detail.SetFocus()
		END IF

/*	Set Delete button.
*/
		cb_delete.Enabled = TRUE
	ELSE
		dw_received_document_detail.Reset()
		dw_received_document_detail.InsertRow(0)
		dw_received_document_detail.Enabled = FALSE

/*	Set Delete button.
*/
		cb_delete.Enabled = FALSE
	END IF

/*	Set rest of buttons.
*/
	cb_cancel.Enabled = FALSE
	cb_save.Enabled = FALSE
	cb_add.Enabled = TRUE

	RETURN 0

end function

event open;call super::open;/*	Grab the claim_no off the datawindow dw_basic_claim located on the worksheet
   and return all the non-imaged documents for it. But first obtain a reference
	to the current instance of w_sheet.
*/
	THIS.SetRedraw(FALSE)
	iw_wsheet = ParentWindow()
	il_claimno			= iw_wsheet.dw_basic_claim.GetitemNumber(1,'claim_no')
	is_imagedflag		= iw_wsheet.dw_basic_claim.GetitemString(1,'imaged_flag')
	idt_accidentdate	= iw_wsheet.dw_basic_claim.GetitemDatetime(1,'accident_date')

/*	Check to see if trying to maintain correspondence for a claim coded as Claim Error
	(claim status code & claim status type code of R & 08)
*/
	IF iw_wsheet.dw_basic_claim.GetItemString(1,'claim_status_code') = 'R' and iw_wsheet.dw_basic_claim.GetItemString(1,'claim_status_type_code') = '08' THEN
		MessageBox("Non Imaged Correspondence","Can not maintain correspondence for this claim as it is coded as a Claim Error.")
		Close(THIS)
		Return
	END IF

/*	Set transaction objects.
*/
	dw_received_document_detail.SetTransObject(SQLCA)
	dw_received_document_list.SetTransObject(SQLCA)
	dw_received_document_list.InsertRow(0)
	dw_received_document_detail.InsertRow(0)

/*	Set up list datawindow to allow only the selection of 1 row at a time.
*/
	dw_received_document_list.uf_setselect(1)
	
/*	Call user event 'ue_get_documents' to retrieve the documents for the
	claim after the screen has been opened.
*/
	THIS.PostEvent('ue_get_documents')


end event

on w_non_imaged_correspondence.create
int iCurrent
call super::create
this.cb_add=create cb_add
this.cb_delete=create cb_delete
this.cb_save=create cb_save
this.cb_cancel=create cb_cancel
this.dw_received_document_list=create dw_received_document_list
this.dw_received_document_detail=create dw_received_document_detail
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_add
this.Control[iCurrent+2]=this.cb_delete
this.Control[iCurrent+3]=this.cb_save
this.Control[iCurrent+4]=this.cb_cancel
this.Control[iCurrent+5]=this.dw_received_document_list
this.Control[iCurrent+6]=this.dw_received_document_detail
end on

on w_non_imaged_correspondence.destroy
call super::destroy
destroy(this.cb_add)
destroy(this.cb_delete)
destroy(this.cb_save)
destroy(this.cb_cancel)
destroy(this.dw_received_document_list)
destroy(this.dw_received_document_detail)
end on

type st_title from w_a_tool`st_title within w_non_imaged_correspondence
string text = "Maintain Received Correspondence for Non-Imaged Claims"
end type

type cb_close from w_a_tool`cb_close within w_non_imaged_correspondence
integer x = 2103
integer y = 1624
integer width = 411
integer height = 108
integer taborder = 70
end type

on cb_close::clicked;/*	Check to see if the record needs to be saved, then close window.
*/
	wf_check_if_save_needed()

	Close(PARENT)

end on

type cb_add from commandbutton within w_non_imaged_correspondence
integer x = 201
integer y = 1624
integer width = 411
integer height = 108
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Add"
boolean default = true
end type

on clicked;/*	Check to see if the record needs to be saved.
*/
	wf_check_if_save_needed()

/*	Call function to add new record.
*/	
	wf_add_new_record()


end on

type cb_delete from commandbutton within w_non_imaged_correspondence
integer x = 677
integer y = 1624
integer width = 411
integer height = 108
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Delete"
end type

on clicked;INTEGER	li_answer

/*	Check to see if the record needs to be saved.
*/
	wf_check_if_save_needed()

/*	Prompt user to see if they wish to continue with deletion.
*/
	dw_received_document_detail.SetRedraw(FALSE)
	li_answer = MessageBox("Delete Warning","Continue with deletion of current record?",Exclamation!,YesNo!)
	IF li_answer = 1 THEN
		wf_delete_current_record()
	END IF
	dw_received_document_detail.SetRedraw(TRUE) 
end on

type cb_save from commandbutton within w_non_imaged_correspondence
integer x = 1152
integer y = 1624
integer width = 411
integer height = 108
integer taborder = 50
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
end type

event clicked;/*	Save the changes to the current record in dw_received_document_detail.
*/
	wf_save()

end event

type cb_cancel from commandbutton within w_non_imaged_correspondence
integer x = 1627
integer y = 1624
integer width = 411
integer height = 108
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Cance&l"
end type

on clicked;INTEGER	li_ftnresult

/*	Set save prompt off.
*/
	ib_promptforsave = FALSE

/*	Retrieve the detailed record for the currently selected row in
	dw_received_document_list.
*/
	li_ftnresult = wf_populate_detail()

end on

type dw_received_document_list from u_dw_online within w_non_imaged_correspondence
integer x = 41
integer y = 160
integer width = 2592
integer height = 840
integer taborder = 10
string dataobject = "d_nonimaged_document_list"
boolean vscrollbar = true
end type

on rowfocuschanged;INTEGER	li_answer, li_rownumber, li_ftnresult

/*	Check to see if the record needs to be saved.
*/
	IF ib_promptforsave THEN
		li_answer = MessageBox("New/Modified Record","The current record has changes which have yet to be saved, save now?",Exclamation!,YesNo!)
		IF li_answer = 1 THEN
			wf_save()
		ELSE
			ib_promptforsave = FALSE
		END IF
	END IF

/*	Retrieve the detailed record for the selected row in list.
*/
	li_rownumber = THIS.GetRow()
	IF li_rownumber > 0 THEN
		li_ftnresult = wf_populate_detail()
	END IF

end on

on rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup

	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	lm_popup.m_options.m_sort.visible = TRUE
	lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))

	Destroy lm_popup
end on

type dw_received_document_detail from u_dw_online within w_non_imaged_correspondence
integer x = 183
integer y = 1004
integer width = 2345
integer height = 592
integer taborder = 20
string dataobject = "d_nonimaged_document_detail"
boolean border = false
end type

event itemchanged;call super::itemchanged;INTEGER	li_ftnresult

li_ftnresult = wf_check_mod_rules()

IF li_ftnresult = 0 THEN
	//	Turn on Save and Cancel buttons as a changed occured.
	IF cb_save.Enabled = FALSE THEN cb_save.Enabled = TRUE
	IF cb_cancel.Enabled = FALSE THEN cb_cancel.Enabled = TRUE

	//	Set instance variable for saving prompt.
	ib_promptforsave = TRUE
ELSE
	THIS.ib_supress_pbmessage = TRUE
	RETURN 1
END IF

end event

on retrieveend;call u_dw_online::retrieveend;/*	Display error if no record returned.
*/
	IF THIS.RowCount() = 0 THEN
		MessageBox("Retrieve Error","Unable to retrieve the selected record into the Maintain Correspondence Details section.",Exclamation!)
	END IF

end on

event editchanged;call super::editchanged;//	Turn on Save and Cancel buttons as a changed occured.
IF cb_save.Enabled = FALSE THEN cb_save.Enabled = TRUE
IF cb_cancel.Enabled = FALSE THEN cb_cancel.Enabled = TRUE

//	Set instance variable for saving prompt.
ib_promptforsave = TRUE

end event

