$PBExportHeader$w_non_imaged_files_logged_out.srw
$PBExportComments$Used to maintain the logging in and out of non imaged files.
forward
global type w_non_imaged_files_logged_out from w_a_tool
end type
type cb_delete from commandbutton within w_non_imaged_files_logged_out
end type
type cb_save from commandbutton within w_non_imaged_files_logged_out
end type
type cb_cancel from commandbutton within w_non_imaged_files_logged_out
end type
type dw_files_logged_out_list from u_dw_online within w_non_imaged_files_logged_out
end type
type dw_files_logged_out_detail from u_dw_online within w_non_imaged_files_logged_out
end type
type cb_login from commandbutton within w_non_imaged_files_logged_out
end type
type cb_logout from commandbutton within w_non_imaged_files_logged_out
end type
type st_statusheading from statictext within w_non_imaged_files_logged_out
end type
type st_status from statictext within w_non_imaged_files_logged_out
end type
end forward

global type w_non_imaged_files_logged_out from w_a_tool
integer width = 2674
boolean resizable = false
event ue_get_documents pbm_custom50
cb_delete cb_delete
cb_save cb_save
cb_cancel cb_cancel
dw_files_logged_out_list dw_files_logged_out_list
dw_files_logged_out_detail dw_files_logged_out_detail
cb_login cb_login
cb_logout cb_logout
st_statusheading st_statusheading
st_status st_status
end type
global w_non_imaged_files_logged_out w_non_imaged_files_logged_out

type variables
BOOLEAN	ib_promptforsave	// Determines if record has been changed and if need to prompt for saving.
W_SHEET	iw_wsheet	// Contains an instance of the worksheet.
LONG		il_claimno	// Contains the value of the current claim_no on the worksheet.
STRING		is_imagedflag	// Determines if the claim is an imaged claim.
DATETIME	idt_accidentdate	// The accident date for the claim.
BOOLEAN	ib_altered // logged out by another user before current user could complete log out
								// or logged in by another user before current user could complete log in

end variables

forward prototypes
public subroutine wf_check_if_save_needed ()
public subroutine wf_delete_current_record ()
public subroutine wf_update_list ()
public function integer wf_retrieve_list ()
public function integer wf_next_sequence_no_is ()
public subroutine wf_set_claim_as (string as_claim_status)
public function integer wf_populate_detail ()
public function integer wf_check_mod_rules (string as_called_by)
public subroutine wf_logout_claim ()
public subroutine wf_set_new_record_values (boolean ab_new)
public function integer wf_reset ()
public subroutine wf_save ()
public function string wf_check_claim_status ()
end prototypes

on ue_get_documents;call w_a_tool::ue_get_documents;STRING	ls_claimstatus
INTEGER	ll_rowsinlist

/*	Using the value in il_claimno, retrieve the documents and insert a
	blank row into dw_files_logged_out_detail.
*/
	ll_rowsinlist = wf_retrieve_list()
	IF ll_rowsinlist > 0 THEN
		dw_files_logged_out_list.SetRow(1)
		wf_populate_detail()
	ELSE
		IF ll_rowsinlist < 0 THEN

		ELSE
			wf_set_claim_as('IN')
		END IF
	END IF

/*	Warn the user if the current claim is an imaged claim.
*/
	IF is_imagedflag = 'Y' THEN
		MessageBox("Imaged Claim Warning","The claim that you have opened this screen for is an imaged claim.",Exclamation!)
	END IF
	THIS.SetRedraw(TRUE)

end on

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

/*	This function deletes the current record in dw_files_logged_out_detail
	from the database. It also removes it from dw_files_logged_out_list.
*/
	dw_files_logged_out_detail.DeleteRow(1)
	wf_save()

	RETURN

end subroutine

public subroutine wf_update_list ();LONG		ll_sequenceno, ll_rownumber, ll_rowsindw, ll_rowis, li_ftnresult
BOOLEAN	lb_cansetrow

/*	This function updates dw_files_logged_out_list with any of the changes that
	have been applied to the database.
*/
	ll_rowsindw = dw_files_logged_out_detail.RowCount()
	IF ll_rowsindw = 0 THEN

/*	Record was deleted from the detail, now delete from the list.
*/
		dw_files_logged_out_list.DeleteRow(dw_files_logged_out_list.GetRow())
		li_ftnresult = wf_populate_detail()
		RETURN
	END IF

/*	Find the record in the list with the sequence_no which matches the record 
	in dw_files_logged_out_detail.
*/
	ll_sequenceno = dw_files_logged_out_detail.GetItemNumber(1,"sequence_no")
	ll_rownumber = dw_files_logged_out_list.Find("sequence_no = " + String(ll_sequenceno),1,dw_files_logged_out_list.RowCount())
	IF ll_rownumber = 0 THEN

/*	Not found, so it's an add. Insert a row before the current first row.
*/
		dw_files_logged_out_list.InsertRow(1)
		dw_files_logged_out_list.ScrollToRow(1)
		dw_files_logged_out_list.SetItem(1,'sequence_no',dw_files_logged_out_detail.GetItemNumber(1,'sequence_no'))
		dw_files_logged_out_list.SetItem(1,'actual_return_date',dw_files_logged_out_detail.GetItemDatetime(1,'actual_return_date'))
		dw_files_logged_out_list.SetItem(1,'logged_out_date',dw_files_logged_out_detail.GetItemDatetime(1,'logged_out_date'))
		dw_files_logged_out_list.SetItem(1,'logged_out_user_id',dw_files_logged_out_detail.GetItemString(1,'logged_out_user_id'))
		dw_files_logged_out_list.SetItem(1,'logged_out_dept_code',dw_files_logged_out_detail.GetItemString(1,'logged_out_dept_code'))
		li_ftnresult = wf_populate_detail()
	ELSE

/*	Found, so it's a modify.
*/
		dw_files_logged_out_list.SetItem(ll_rownumber,'actual_return_date',dw_files_logged_out_detail.GetItemDatetime(1,'actual_return_date'))
		dw_files_logged_out_list.SetItem(ll_rownumber,'logged_out_date',dw_files_logged_out_detail.GetItemDatetime(1,'logged_out_date'))
		dw_files_logged_out_list.SetItem(ll_rownumber,'logged_out_user_id',dw_files_logged_out_detail.GetItemString(1,'logged_out_user_id'))
		dw_files_logged_out_list.SetItem(ll_rownumber,'logged_out_dept_code',dw_files_logged_out_detail.GetItemString(1,'logged_out_dept_code'))
		li_ftnresult = wf_populate_detail()
	END IF

	RETURN

end subroutine

public function integer wf_retrieve_list ();INTEGER	ll_rowsinlist

/*	This function retrieve all the non-imaged documents for a
	given claim_no.
*/
	ll_rowsinlist = dw_files_logged_out_list.Retrieve(il_claimno)
	IF SQLCA.nf_handle_error('w_non_imaged_files_logged_out','wf_retrieve_list', 'dw_files_logged_out_list.Retrieve(il_claimno)') < 0 THEN
		RETURN -1
	END IF


	RETURN ll_rowsinlist

end function

public function integer wf_next_sequence_no_is ();LONG		ll_error
INTEGER	li_nextsequenceno, li_currentsequenceno

/*	This function uses embedded SQL to select the current maximum
	sequence_no from NONIMAGED_RECVD_CORRESP_LOG. It then adds
	1 to it to get the next sequence_no.
*/
	SELECT IsNull(Max(sequence_no),0)
	  INTO :li_currentsequenceno
	  FROM NONIMAGED_FILES_LOGGED_OUT
	 WHERE claim_no = :il_claimno;

	ll_error = SQLCA.nf_handle_error("wf_next_sequence_no_is","w_non_imaged_files_logged_out","SELECT Max(sequence_no)....")
	IF ll_error < 0 THEN
		RETURN -1
	END IF

	li_nextsequenceno = li_currentsequenceno + 1

	RETURN li_nextsequenceno

end function

public subroutine wf_set_claim_as (string as_claim_status);/*	This function sets up the claim status message and enables/disables
	the command buttons based on the current status of the claim.

	Argument:	as_claim_status - represents the current status of the claim.
*/
	IF as_claim_status = 'IN' THEN
		st_status.Text = 'IN'
		cb_login.Enabled = FALSE
		cb_logout.Enabled = TRUE
		cb_delete.Enabled = FALSE
		cb_cancel.Enabled = FALSE
		cb_save.Enabled = FALSE
		dw_files_logged_out_detail.Enabled = FALSE
	ELSE
		IF as_claim_status = 'OUT' THEN
			st_status.Text = 'OUT'
			cb_login.Enabled = TRUE
			cb_logout.Enabled = FALSE
			cb_delete.Enabled = TRUE
			cb_cancel.Enabled = FALSE
			cb_save.Enabled = FALSE
			dw_files_logged_out_detail.Enabled = TRUE
		ELSE
			cb_delete.Enabled = FALSE
			cb_login.Enabled = FALSE
			cb_cancel.Enabled = FALSE
			cb_save.Enabled = FALSE
			dw_files_logged_out_detail.Enabled = FALSE
		END IF
	END IF

end subroutine

public function integer wf_populate_detail ();STRING	ls_claimstatus
LONG		ll_sequenceno, ll_rownumber

/*	This function populates the datawindow dw_files_logged_out_detail with the
	currently highlighted record in dw_files_logged_out_list.
*/
	ll_rownumber = dw_files_logged_out_list.GetRow()
	IF ll_rownumber > 0 THEN
		dw_files_logged_out_list.uf_processselect(ll_rownumber,"Mouse")
		ll_sequenceno = dw_files_logged_out_list.GetItemNumber(ll_rownumber,"sequence_no")
		IF not IsNull(ll_sequenceno) THEN
			dw_files_logged_out_detail.Retrieve(il_claimno,ll_sequenceno)
			IF SQLCA.nf_handle_error('w_non_imaged_files_logged_out','wf_populate_detail', 'dw_files_logged_out_detail.Retrieve(il_claimno,ll_sequenceno)') < 0 THEN
				RETURN -1
			END IF
		END IF

/*	Check the status of the claim.
*/
		ls_claimstatus = wf_check_claim_status()
		wf_set_claim_as(ls_claimstatus)
	ELSE
		dw_files_logged_out_detail.Reset()
		dw_files_logged_out_detail.InsertRow(0)
		wf_set_claim_as('IN')
	END IF

	RETURN 0

end function

public function integer wf_check_mod_rules (string as_called_by);INTEGER				li_rowsindw, li_entryfoundat
STRING				ls_userid, ls_comment, ls_loggedoutdeptcode, ls_externalflag
DATETIME				ldt_loggedoutdate, ldt_returneddate
DATAWINDOWCHILD	ldwc_child

/*	This function checks to see if all the mandatory, validation, and
	consistency rules have been satisfied prior to saving the record.
	If the save is for a delete, then do not need to check the rules.
*/
	dw_files_logged_out_detail.AcceptText()
	IF dw_files_logged_out_detail.RowCount() = 1 THEN
		ls_userid				= dw_files_logged_out_detail.GetItemString(1,'logged_out_user_id')
		ls_comment				= dw_files_logged_out_detail.GetItemString(1,'comment')
		ls_loggedoutdeptcode	= dw_files_logged_out_detail.GetItemString(1,'logged_out_dept_code')
		ldt_loggedoutdate		= dw_files_logged_out_detail.GetItemDatetime(1,'logged_out_date')
		ldt_returneddate		= dw_files_logged_out_detail.GetItemDatetime(1,'actual_return_date')

/*	Check to see if all mandatory fields have been entered.
*/
		IF IsNull(ls_loggedoutdeptcode) or ls_loggedoutdeptcode = '' THEN
			IF as_called_by = 'SAVE' THEN
				MessageBox("Validation Error","Department is invalid.",Exclamation!)
			END IF
			RETURN -1
		END IF

		IF IsNull(ldt_loggedoutdate) THEN
			IF as_called_by = 'SAVE' THEN
				MessageBox("Validation Error","Logged Out Date is invalid.",Exclamation!)
			END IF
			RETURN -1
		END IF

		IF IsNull(ls_userid) or ls_userid = '' THEN
			IF as_called_by = 'SAVE' THEN
				MessageBox("Validation Error","Logged Out By is invalid.",Exclamation!)
			END IF
			RETURN -1
		END IF

/*	Check to see if all validation rules have been satisfied.
*/
		IF Date(ldt_loggedoutdate) > Today() THEN
			IF as_called_by = 'SAVE' THEN
				MessageBox("Invalid Logged Out Date","Logged Out Date can not be in the future.",Exclamation!)
			END IF
			RETURN -1
		END IF

		IF not IsNull(ldt_returneddate) THEN
			IF Date(ldt_returneddate) > Today() THEN
				IF as_called_by = 'SAVE' THEN
					MessageBox("Invalid Returned Date","Returned Date can not be in the future.",Exclamation!)
				END IF
				RETURN -1
			END IF
		END IF
		
		dw_files_logged_out_detail.GetChild('logged_out_dept_code',ldwc_child)
		li_rowsindw = ldwc_child.RowCount()
		li_entryfoundat = ldwc_child.Find("department_code = '" + ls_loggedoutdeptcode + "'",1,li_rowsindw)
		IF li_entryfoundat = 0 THEN
			IF as_called_by = 'SAVE' THEN
				MessageBox("Invalid Department","Department is invalid.",Exclamation!)
			END IF
			RETURN -1
		ELSE
			ls_externalflag = ldwc_child.GetItemString(li_entryfoundat,'external_flag')
		END IF

		dw_files_logged_out_detail.GetChild('logged_out_user_id',ldwc_child)
		li_rowsindw = ldwc_child.RowCount()
		li_entryfoundat = ldwc_child.Find("user_id = '" + ls_userid + "'",1,li_rowsindw)
		IF li_entryfoundat = 0 THEN
			IF as_called_by = 'SAVE' THEN
				MessageBox("Invalid Logged Out By","Logged Out By is invalid.",Exclamation!)
			END IF
			RETURN -1
		END IF

/*	Check to see if all consistency rules have been satisfied.
*/
		IF not IsNull(ldt_returneddate) THEN
			IF Date(ldt_returneddate) < Date(ldt_loggedoutdate) THEN
				IF as_called_by = 'SAVE' THEN
					MessageBox("Invalid Returned Date","Returned Date can not be before Logged Out Date.",Exclamation!)
				END IF
				RETURN -1
			END IF
		END IF

		IF ls_externalflag = 'Y' and (IsNull(ls_comment) or ls_comment = '') THEN
			IF as_called_by = 'SAVE' THEN
				MessageBox("Invalid Comment","A Comment is required for a record with an external Department.",Exclamation!)
			END IF
			RETURN -1
		END IF

		IF Date(ldt_loggedoutdate) < Date(idt_accidentdate) THEN
			IF as_called_by = 'SAVE' THEN
				MessageBox("Invalid Logged Out Date","Logged Out Date can not be before Accident Date.",Exclamation!)
			END IF
			RETURN -1
		END IF
	END IF

	RETURN 0

end function

public subroutine wf_logout_claim ();/*	This function logs out the claim by inserting a new record
	into dw_files_logged_out_detail.
*/
	dw_files_logged_out_detail.Enabled = TRUE
	dw_files_logged_out_detail.Reset()
	dw_files_logged_out_detail.InsertRow(0)

/*	Set focus to the detail datawindow.
*/
	dw_files_logged_out_detail.SetColumn('logged_out_date')
	dw_files_logged_out_detail.SetFocus()

/*	Set buttons.
*/
	cb_delete.Enabled = FALSE
	dw_files_logged_out_detail.TriggerEvent(itemchanged!)


end subroutine

public subroutine wf_set_new_record_values (boolean ab_new);INTEGER	li_nextsequenceno

/*	This function sets the claim_no, sequence_no, and date_received
	columns for any new records.
*/
IF ab_new THEN
	li_nextsequenceno = wf_next_sequence_no_is()
	dw_files_logged_out_detail.SetItem(1,'claim_no',il_claimno)
	dw_files_logged_out_detail.SetItem(1,'logged_out_date',Today())
	dw_files_logged_out_detail.SetItem(1,'sequence_no',li_nextsequenceno)
END IF
end subroutine

public function integer wf_reset ();cb_cancel.TriggerEvent(Clicked!)
THIS.PostEvent('ue_get_documents')

return 0
end function

public subroutine wf_save ();INTEGER	li_saveresult
STRING	ls_file_status

/*	This function applies the change made to the current record in the datawindow
	dw_files_logged_out_detail to the database. But first check to see if all the
	modifications rules have been passed.
*/
	IF wf_check_mod_rules('SAVE') = 0 THEN
		ls_file_status = wf_check_claim_status()
		IF ib_altered = TRUE THEN
			ib_altered = FALSE
			IF ls_file_status = 'IN' THEN
				MessageBox('Check-In Error','This file has been checked in by another user. Your attempted check-in will be canceled and the window will be refreshed.')
			ELSEIF ls_file_status = 'OUT' THEN
				MessageBox('Check-Out Error','This file has been checked out by another user. Your attempted check-out will be canceled and the window will be refreshed.')
			END IF
			wf_reset()
			RETURN
		END IF
		
		SQLCA.nf_begin_transaction()
		
		dw_files_logged_out_detail.Update(TRUE)
		li_saveresult = SQLCA.nf_handle_error("w_non_imaged_files_logged_out","dw_files_logged_out_detail","wf_save(): 'dw_files_logged_out_detail.Update()'")
		IF li_saveresult = 0 THEN
			
			SQLCA.nf_commit_transaction()
			ib_promptforsave = FALSE
			wf_update_list()
		END IF
	END IF

	RETURN

end subroutine

public function string wf_check_claim_status ();LONG			ll_atrow
INTEGER		li_seq_no
DATETIME	ldt_actual_rtn
STRING		ls_status

/*	This function returns the status of the claim. The possible
	return values are IN and OUT. Because the claim can be sorted,
	the only way to determine if it's OUT is by actual_return_date.
	Look for one that is NULL, if none found then it's IN.
*/

// PR 3296 - need to ensure that other users have not logged out file during this user's attempt to log it out
// likewise for logging the file in.

SELECT	Max(sequence_no)
INTO		:li_seq_no
FROM		NONIMAGED_FILES_LOGGED_OUT
WHERE	claim_no = :il_claimno
USING SQLCA;

IF NOT IsNull(li_seq_no) THEN
	SELECT	actual_return_date
	INTO		:ldt_actual_rtn
	FROM		NONIMAGED_FILES_LOGGED_OUT
	WHERE	claim_no = :il_claimno
	AND		sequence_no = :li_seq_no
	USING SQLCA;
ELSE
	// never been checked out
	ls_status = 'IN'
	wf_set_claim_as(ls_status)
	IF ls_status <> st_status.Text THEN
		ib_altered = TRUE
	END IF
	RETURN ls_status
END IF

IF IsNull(ldt_actual_rtn) THEN
	ls_status = 'OUT'
ELSE
	ls_status = 'IN'
END IF

IF ls_status <> st_status.Text AND st_status.Text <> 'NONE' THEN
	ib_altered = TRUE
END IF
wf_set_claim_as(ls_status)
RETURN ls_status

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

/*	Check to see if trying to log out files to a claim coded as Claim Error
	(claim status code & claim status type code of R & 08)
*/
	IF iw_wsheet.dw_basic_claim.GetItemString(1,'claim_status_code') = 'R' and iw_wsheet.dw_basic_claim.GetItemString(1,'claim_status_type_code') = '08' THEN
		MessageBox("Non Imaged Files Logged Out","Can not log out files to this claim as it is coded as a Claim Error.")
		Close(THIS)
		Return
	END IF

/*	Set transaction objects.
*/
	dw_files_logged_out_detail.SetTransObject(SQLCA)
	dw_files_logged_out_list.SetTransObject(SQLCA)
	dw_files_logged_out_list.InsertRow(0)
	dw_files_logged_out_detail.InsertRow(0)

/*	Set up list datawindow to allow only the selection of 1 row at a time.
*/
	dw_files_logged_out_list.uf_setselect(1)
	
/*	Call user event 'ue_get_documents' to retrieve the documents for the
	claim after the screen has been opened.
*/
	THIS.PostEvent('ue_get_documents')


end event

on w_non_imaged_files_logged_out.create
int iCurrent
call super::create
this.cb_delete=create cb_delete
this.cb_save=create cb_save
this.cb_cancel=create cb_cancel
this.dw_files_logged_out_list=create dw_files_logged_out_list
this.dw_files_logged_out_detail=create dw_files_logged_out_detail
this.cb_login=create cb_login
this.cb_logout=create cb_logout
this.st_statusheading=create st_statusheading
this.st_status=create st_status
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_delete
this.Control[iCurrent+2]=this.cb_save
this.Control[iCurrent+3]=this.cb_cancel
this.Control[iCurrent+4]=this.dw_files_logged_out_list
this.Control[iCurrent+5]=this.dw_files_logged_out_detail
this.Control[iCurrent+6]=this.cb_login
this.Control[iCurrent+7]=this.cb_logout
this.Control[iCurrent+8]=this.st_statusheading
this.Control[iCurrent+9]=this.st_status
end on

on w_non_imaged_files_logged_out.destroy
call super::destroy
destroy(this.cb_delete)
destroy(this.cb_save)
destroy(this.cb_cancel)
destroy(this.dw_files_logged_out_list)
destroy(this.dw_files_logged_out_detail)
destroy(this.cb_login)
destroy(this.cb_logout)
destroy(this.st_statusheading)
destroy(this.st_status)
end on

type st_title from w_a_tool`st_title within w_non_imaged_files_logged_out
string text = "Maintain Files Logged Out for Non-Imaged Claims"
end type

type cb_close from w_a_tool`cb_close within w_non_imaged_files_logged_out
integer x = 2167
integer y = 1656
integer width = 347
integer height = 108
integer taborder = 80
end type

on cb_close::clicked;/*	Check to see if the record needs to be saved, then close window.
*/
	wf_check_if_save_needed()

	Close(PARENT)

end on

type cb_delete from commandbutton within w_non_imaged_files_logged_out
integer x = 960
integer y = 1656
integer width = 347
integer height = 108
integer taborder = 50
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Delete"
end type

on clicked;INTEGER	li_answer
STRING	ls_claimstatus

/*	Check to see if the claim is currently logged out, and if it is
	then it can be deleted.
*/
	ls_claimstatus = wf_check_claim_status()
	IF ls_claimstatus = 'OUT' THEN

/*	Check to see if the record needs to be saved.
*/
		wf_check_if_save_needed()

/*	Prompt user to see if they wish to continue with deletion.
*/
		dw_files_logged_out_detail.SetRedraw(FALSE)
		li_answer = MessageBox("Delete Warning","Continue with deletion of current record?",Exclamation!,YesNo!)
		IF li_answer = 1 THEN
			wf_delete_current_record()
		END IF
		dw_files_logged_out_detail.SetRedraw(TRUE) 
	ELSE
		MessageBox("Delete Warning","Can not delete record as it has a returned date.",Exclamation!)
	END IF

end on

type cb_save from commandbutton within w_non_imaged_files_logged_out
integer x = 1362
integer y = 1656
integer width = 347
integer height = 108
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
end type

event clicked;/*	Save the changes to the current record in dw_files_logged_out_detail.
*/
this.enabled = false
	// Call ftn to initialize the column values.
	IF st_status.Text = 'IN' THEN
		wf_set_new_record_values(TRUE)
	END IF
	
	wf_save()
	
	ib_altered = FALSE

end event

type cb_cancel from commandbutton within w_non_imaged_files_logged_out
integer x = 1765
integer y = 1656
integer width = 347
integer height = 108
integer taborder = 70
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
	dw_files_logged_out_list.
*/
	li_ftnresult = wf_populate_detail()

end on

type dw_files_logged_out_list from u_dw_online within w_non_imaged_files_logged_out
integer x = 73
integer y = 204
integer width = 2528
integer height = 692
integer taborder = 10
string dataobject = "d_nonimaged_files_logged_out_list"
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

event rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup

//	Create popup menu to display options to print and sort the datawindow.
lm_popup = Create m_dw_online_rmb_popup
lm_popup.mf_set_datawindow(This)
lm_popup.m_options.m_sort.visible = TRUE
lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))

Destroy lm_popup

end event

type dw_files_logged_out_detail from u_dw_online within w_non_imaged_files_logged_out
integer x = 73
integer y = 944
integer width = 2537
integer height = 640
integer taborder = 20
string dataobject = "d_nonimaged_files_logged_out_detail"
boolean border = false
end type

event itemchanged;call super::itemchanged;INTEGER	li_ftnresult

li_ftnresult = wf_check_mod_rules('DW')
IF li_ftnresult = 0 THEN
	//	Turn on Save and Cancel buttons as a changed occured.
	IF cb_save.Enabled = FALSE THEN cb_save.Enabled = TRUE
	IF cb_cancel.Enabled = FALSE THEN cb_cancel.Enabled = TRUE

	//	Set instance variable for saving prompt.
	ib_promptforsave = TRUE
ELSE
	THIS.ib_supress_pbmessage = TRUE
	RETURN 2
END IF
end event

on retrieveend;call u_dw_online::retrieveend;/*	Display error if no record returned.
*/
	IF THIS.RowCount() = 0 THEN
		MessageBox("Retrieve Error","Unable to retrieve the selected record into the Maintain Files Logged Out Details section.",Exclamation!)
	END IF
 
end on

event editchanged;call super::editchanged;//	Turn on Save and Cancel buttons as a changed occured.
IF cb_save.Enabled = FALSE THEN cb_save.Enabled = TRUE
IF cb_cancel.Enabled = FALSE THEN cb_cancel.Enabled = TRUE

//	Set instance variable for saving prompt.
ib_promptforsave = TRUE

end event

type cb_login from commandbutton within w_non_imaged_files_logged_out
integer x = 558
integer y = 1656
integer width = 347
integer height = 108
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Log &In"
end type

event clicked;/*	Logging in a claim is done when the claim is returned, therefore
	set the actual return date equal to today.
*/
	THIS.Enabled = FALSE
	dw_files_logged_out_detail.SetItem(1,'actual_return_date',Today())
	dw_files_logged_out_detail.TriggerEvent(ItemChanged!)
end event

type cb_logout from commandbutton within w_non_imaged_files_logged_out
integer x = 155
integer y = 1656
integer width = 347
integer height = 108
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Log &Out"
end type

event clicked;STRING	ls_claimstatus

/*	Check to see if the claim is currently logged in and if it is,
	then log it out.
*/
	ls_claimstatus = wf_check_claim_status()
	IF ls_claimstatus = 'IN' THEN
		THIS.Enabled = FALSE
		cb_save.Enabled = TRUE
		wf_logout_claim()
	ELSE
		MessageBox("Log Out Error","Can not log out a currently logged out claim.",Exclamation!)
	END IF

end event

type st_statusheading from statictext within w_non_imaged_files_logged_out
integer x = 1687
integer y = 104
integer width = 805
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16711680
long backcolor = 67108864
boolean enabled = false
string text = "Current status of file:   Logged "
boolean focusrectangle = false
end type

type st_status from statictext within w_non_imaged_files_logged_out
integer x = 2496
integer y = 104
integer width = 160
integer height = 56
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16711680
long backcolor = 67108864
boolean enabled = false
string text = "NONE"
boolean focusrectangle = false
end type

