$PBExportHeader$w_override_termination_date.srw
forward
global type w_override_termination_date from window
end type
type cb_close from commandbutton within w_override_termination_date
end type
type cb_save from commandbutton within w_override_termination_date
end type
type cb_delete from commandbutton within w_override_termination_date
end type
type cb_cancel from commandbutton within w_override_termination_date
end type
type cb_add from commandbutton within w_override_termination_date
end type
type cbx_open_ended_drug_coverage from checkbox within w_override_termination_date
end type
type dw_override_term_date from u_dw_online within w_override_termination_date
end type
type gb_2 from groupbox within w_override_termination_date
end type
end forward

global type w_override_termination_date from window
integer width = 3072
integer height = 876
boolean titlebar = true
string title = "OverrideTermination Date Entry"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
cb_close cb_close
cb_save cb_save
cb_delete cb_delete
cb_cancel cb_cancel
cb_add cb_add
cbx_open_ended_drug_coverage cbx_open_ended_drug_coverage
dw_override_term_date dw_override_term_date
gb_2 gb_2
end type
global w_override_termination_date w_override_termination_date

type variables
Long    il_claim_no 
Boolean I_Authorized_Access 
String  is_open_ended_drug_coverage, is_status
Date	  idt_termination_date

N_BR_BLUECROSS	 inv_bc
end variables

forward prototypes
public subroutine wf_read_only ()
public function integer wf_retrieve_man_term_date_dw ()
end prototypes

public subroutine wf_read_only ();// wf_read_only
//
cb_add.Enabled = FALSE
cb_cancel.Enabled = FALSE
cb_delete.Enabled = FALSE
cb_save.Enabled = FALSE

dw_override_term_date.Enabled = FALSE
dw_override_term_date.Modify("eligibility_end_date.Background.Color='553648127' eligibility_end_date.protect=1")
dw_override_term_date.Modify("comment.Background.Color='553648127' comment.protect=1")
cbx_open_ended_drug_coverage.Enabled = FALSE
end subroutine

public function integer wf_retrieve_man_term_date_dw ();// wf_retrieve_man_term_date_dw
//
Long     ll_num_rows
Integer  li_rtn
Datetime ldt_eligibility_end_date

ll_num_rows = dw_override_term_date.Retrieve(il_claim_no)
li_rtn = SQLCA.nf_handle_error("w_override_termination_date", "", "wf_retrieve_man_term_date_dw - dw_drug_coverage_manual_term_date.Retrieve(il_claim_no)")

IF ll_num_rows > 0 THEN
	dw_override_term_date.Visible = TRUE
	cbx_open_ended_drug_coverage.Visible = TRUE
	
	ldt_eligibility_end_date = dw_override_term_date.GetItemDatetime(1, "eligibility_end_date")

	IF IsNull(ldt_eligibility_end_date) = TRUE THEN
		is_open_ended_drug_coverage = "Y"
		cbx_open_ended_drug_coverage.Checked = TRUE
		dw_override_term_date.Modify("eligibility_end_date.Background.Color='553648127' protect=1")
		dw_override_term_date.object.eligibility_end_date.protect = TRUE
	ELSE		
		is_open_ended_drug_coverage = "N"
		cbx_open_ended_drug_coverage.Checked = FALSE
		dw_override_term_date.Modify("eligibility_end_date.Background.Color='16777215' protect=0")
		dw_override_term_date.SetItem(1, "eligibility_end_date", ldt_eligibility_end_date)
	END IF
ELSE
	dw_override_term_date.Visible = FALSE
	cbx_open_ended_drug_coverage.Visible = FALSE
END IF

RETURN 0

end function

on w_override_termination_date.create
this.cb_close=create cb_close
this.cb_save=create cb_save
this.cb_delete=create cb_delete
this.cb_cancel=create cb_cancel
this.cb_add=create cb_add
this.cbx_open_ended_drug_coverage=create cbx_open_ended_drug_coverage
this.dw_override_term_date=create dw_override_term_date
this.gb_2=create gb_2
this.Control[]={this.cb_close,&
this.cb_save,&
this.cb_delete,&
this.cb_cancel,&
this.cb_add,&
this.cbx_open_ended_drug_coverage,&
this.dw_override_term_date,&
this.gb_2}
end on

on w_override_termination_date.destroy
destroy(this.cb_close)
destroy(this.cb_save)
destroy(this.cb_delete)
destroy(this.cb_cancel)
destroy(this.cb_add)
destroy(this.cbx_open_ended_drug_coverage)
destroy(this.dw_override_term_date)
destroy(this.gb_2)
end on

event open;Long     ll_num_rows, ll_count, ll_row
Integer  li_rtn
String   ls_claim_status_code, ls_term_date
Datetime ldt_death_date
Boolean  lb_read_only = FALSE
s_window_message lstr_message

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


lstr_message = Message.PowerObjectParm

SetPointer(Hourglass!)

G_PFSecurity.UOF_Check_Access(This)
This.I_Authorized_Access = True   // declared as an instance variable

il_claim_no = lstr_message.al_doubleparm[1]
IF lstr_message.as_mode = "READ" THEN
	lb_read_only = TRUE
END IF

dw_override_term_date.Visible = TRUE
cbx_open_ended_drug_coverage.Visible = TRUE

// Set the transobjects and do the retrieves
inv_bc = Create n_br_bluecross

li_rtn = dw_override_term_date.SetTransObject(SQLCA)
li_rtn = wf_retrieve_man_term_date_dw()


// If the claim is not active or finalled then put screen in read only mode
//SELECT claim_status_code 
//  INTO :ls_claim_status_code
//  FROM CLAIM 
// WHERE claim_no = :il_claim_no ;
//
//li_rtn = SQLCA.nf_handle_error("w_manual_drug_coverage_termination_date", "", "open - SELECT claim_status_code FROM CLAIM")
//
//IF ls_claim_status_code <> "A" AND ls_claim_status_code <> "F" THEN
//	wf_read_only()
//	lb_read_only = TRUE
//END IF
//
//// If claimant has a death date put screen in read-only mode and display message
//SELECT I.death_date 
//  INTO :ldt_death_date
//  FROM CLAIM C, 
//		 INDIVIDUAL I 
// WHERE C.claim_no = :il_claim_no 
//	AND C.individual_no = I.individual_no ;
//
//li_rtn = SQLCA.nf_handle_error("w_manual_drug_coverage_termination_date", "", "open - SELECT I.death_date FROM CLAIM C, INDIVIDUAL I")
//
//IF IsNull(ldt_death_date) = FALSE THEN
//	wf_read_only()
//	lb_read_only = TRUE
//	Messagebox("Drug Coverage Terminated", "Drug coverage is terminated due to the death of the claimant.", Information!)
//END IF

// See if an unprocessed manual termination date exists
ll_num_rows = dw_override_term_date.RowCount()

IF lb_read_only = FALSE THEN
	IF ll_num_rows > 0 THEN
		dw_override_term_date.Enabled = TRUE
		cb_add.Enabled = FALSE
		cb_delete.Enabled = TRUE
	ELSE
		cb_add.Enabled = TRUE
	END IF
ELSE
	dw_override_term_date.Enabled = FALSE
	cbx_open_ended_drug_coverage.Enabled = FALSE
	cb_add.Enabled = FALSE
	cb_delete.Enabled = FALSE
	cb_save.Enabled = FALSE
	cb_cancel.Enabled = FALSE
END IF


cb_save.Enabled = FALSE
cb_cancel.Enabled = FALSE

// Disable Add button if claim exists in X001_EXCLUDED_CLAIMS
//SELECT COUNT(*) 
//  INTO :ll_count 
//  FROM X001_EXCLUDED_CLAIM 
// WHERE claim_no = :il_claim_no 
//   AND excluded_flag = "Y" ; 
//
//li_rtn = SQLCA.nf_handle_error("w_manual_drug_coverage_termination_date", "", "open - SELECT COUNT(*) FROM X001_EXCLUDED_CLAIMS")
//
//IF ll_count > 0 THEN
//	cb_add.Enabled = FALSE
//END IF

//em_computed_date.SetFocus()

end event

type cb_close from commandbutton within w_override_termination_date
integer x = 2542
integer y = 644
integer width = 329
integer height = 92
integer taborder = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;Boolean lb_status

Integer 	li_rtn

IF cb_save.Enabled = TRUE THEN
	li_rtn = Messagebox("Save Changes?", "A change has been made and not saved yet," +&
							  " Do you want to save it?",Question!, YesNoCancel!, 1)
	IF li_rtn = 1 THEN
		cb_save.Triggerevent(Clicked!)
		IF cb_save.Enabled = TRUE THEN
			dw_override_term_date.SetFocus()
			RETURN 1
		END IF
	ELSEIF li_rtn = 3 THEN
		RETURN 1
	ELSEIF li_rtn = 2 THEN
		wf_retrieve_man_term_date_dw()
	END IF
END IF


IF cbx_open_ended_drug_coverage.Checked = TRUE THEN
	is_status = "TRUE"	
Else
	is_status = "False"
End If	


CloseWithReturn(Parent,is_status)

end event

type cb_save from commandbutton within w_override_termination_date
integer x = 2085
integer y = 644
integer width = 329
integer height = 92
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
end type

event clicked;Integer  li_rtn, li_year
Long     ll_row, ll_days, ll_len, n
Datetime ldt_eligibility_end_date, ldt_accident_date, ldt_server_datetime, ldt_orig_eligibility_end_date
String   ls_comment, ls_char
Boolean  lb_found_letter
Date		ldt_future_date, ldt_server_date
N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '008' refers to the Maintain Formulary module, '046' refers to the ABCC Eligibility Export module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('008','046','save',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF

/******************************************************************************************/

SetPointer(HourGlass!)

li_rtn = dw_override_term_date.AcceptText()
IF li_rtn = -1 THEN
	RETURN
END IF

ll_row = dw_override_term_date.GetRow()
IF ll_row < 1 THEN
	RETURN
END IF

// Get the data entered on DW
ldt_eligibility_end_date = dw_override_term_date.GetItemDatetime(ll_row, "eligibility_end_date")
IF NOT ISNULL(dw_override_term_date.GetItemDatetime(ll_row, "eligibility_end_date", Primary!, TRUE)) THEN
	ldt_orig_eligibility_end_date = dw_override_term_date.GetItemDatetime(ll_row, "eligibility_end_date", Primary!, TRUE)
ELSE
	SetNull(ldt_orig_eligibility_end_date)
END IF

// IF both fields are now blank...
IF cbx_open_ended_drug_coverage.Checked = FALSE AND IsNull(ldt_eligibility_end_date) = TRUE THEN
	// If there was a date and now there isn't a date
	IF IsNull(ldt_orig_eligibility_end_date) = FALSE THEN
		MessageBox("Invalid Combination", "To make the drug coverage open ended (ie. no termination date) check the box next to the date.", Information!)
		RETURN
	END IF
	
	// If box was checked and now unchecked...
	IF is_open_ended_drug_coverage = "Y" THEN
		MessageBox("Enter a date", "A date is now required, please enter one or delete the entry.")
		RETURN
	END IF

	// If both fields were blank
	IF is_open_ended_drug_coverage = "N" AND IsNull(ldt_orig_eligibility_end_date) = TRUE THEN
		MessageBox("No Data", "No data has been entered.  Enter a date or delete the entry.", Information!)
		RETURN
	END IF
END IF

// Make sure Eligibility End date is after accident date
SELECT accident_date
  INTO :ldt_accident_date 
  FROM CLAIM 
 WHERE claim_no = :il_claim_no ; 

li_rtn = SQLCA.nf_handle_error("w_override_termination_date", "", "cb_save - SELECT accident_date FROM CLAIM )")

IF ldt_eligibility_end_date < ldt_accident_date OR DATE(ldt_eligibility_end_date) < DATE('1997-01-01') THEN
	Messagebox("Invalid Date", "The drug coverage termination date must be on or after the claimant's accident date and after 1997-01-01.  The accident date is " + String(ldt_accident_date, "mmm dd, yyyy") + ".", Exclamation!)
	dw_override_term_date.SetColumn("eligibility_end_date")
	dw_override_term_date.SetFocus()
	RETURN
END IF

// Make sure Eligibility End date is not more than 5 years in future (365 * 5 + 1 = 1826)
ldt_server_datetime = f_server_datetime()
ll_days = DaysAfter(Date(ldt_server_datetime), Date(ldt_eligibility_end_date))
IF ll_days > 1826 THEN
	Messagebox("Invalid Date", "The drug coverage termination date cannot be more than 5 years in the future.  Please correct.", Information!)
	dw_override_term_date.SetColumn("eligibility_end_date")
	dw_override_term_date.SetFocus()
	RETURN
END IF

// Make sure a comment is entered
ls_comment = Trim(dw_override_term_date.GetItemString(ll_row, "comment"))
IF ls_comment = "" OR IsNull(ls_comment) THEN
	Messagebox("Comment Required", "Please enter a comment and try again.", Exclamation!)
	dw_override_term_date.SetColumn("comment")
	dw_override_term_date.SetFocus()
	RETURN
END IF

// Make sure comment is at least 3 characters long
IF Len(ls_comment) < 3 THEN
	Messagebox("Comment Not Long Enough", "Please enter a comment that is at least 3 characters long and try again.", Exclamation!)
	dw_override_term_date.SetColumn("comment")
	dw_override_term_date.SetFocus()
	RETURN
END IF

// The first letter of the comment must be a character
ls_char = LEFT(ls_comment,1) 
IF (Asc(ls_char) >= Asc("a") AND Asc(ls_char) <= Asc("z")) OR (Asc(ls_char) >= Asc("A") AND Asc(ls_char) <= Asc("Z")) THEN
	//continue
ELSE
	Messagebox("Invalid Comment", "The comment must start with a letter, try again.", Exclamation!)
	dw_override_term_date.SetColumn("comment")
	dw_override_term_date.SetFocus()
	RETURN
END IF	


// The database has a constraint on it we have to check for a letter in the first 3 characters of the comment
ll_len = Len(ls_comment)
lb_found_letter = TRUE
FOR n = 1 TO 3
	ls_char = Mid(ls_comment, n, 1)
	IF (Asc(ls_char) >= Asc("a") AND Asc(ls_char) <= Asc("z")) OR (Asc(ls_char) >= Asc("A") AND Asc(ls_char) <= Asc("Z")) THEN
		//continue
	ELSE	
		lb_found_letter = FALSE
		EXIT
	END IF
NEXT
IF lb_found_letter = FALSE THEN
	Messagebox("Invalid Comment", "Comment must start with 3 letters from the alphabet.  Add a 3 letters and try again.", Exclamation!)
	dw_override_term_date.SetColumn("comment")
	dw_override_term_date.SetFocus()
	RETURN
END IF

// Constraint was modified to not allow a blank for the first character!!!
dw_override_term_date.SetItem(ll_row, "comment", ls_comment)
li_rtn = dw_override_term_date.AcceptText()

SQLCA.nf_begin_transaction()

// Update and Commit
li_rtn = dw_override_term_date.Update()
li_rtn = SQLCA.nf_handle_error("w_override_termination_date", "", "cb_save - dw_override_term_date.Update()")

SQLCA.nf_commit_transaction()

cb_save.Enabled = FALSE
cb_cancel.Enabled = FALSE
cb_add.Enabled = FALSE
cb_delete.Enabled = TRUE

// Refresh DW
//li_rtn = wf_retrieve_man_term_date_dw()

end event

type cb_delete from commandbutton within w_override_termination_date
integer x = 1627
integer y = 644
integer width = 329
integer height = 92
integer taborder = 40
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Delete"
end type

event clicked;Integer li_rtn
Long    ll_row
N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '008' refers to the Maintain Formulary module, '046' refers to the ABCC Eligibility Export module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('008','046','delete',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF

/******************************************************************************************/


li_rtn = Messagebox("Continue Delete?", "The override termination date will be removed.  Do you wish to continue?", Question!, YesNo!, 2)
IF li_rtn = 2 THEN
	RETURN
END IF

SetPointer(Hourglass!)

cb_add.Enabled = TRUE
cb_save.Enabled = FALSE
cb_delete.Enabled = FALSE

ll_row = dw_override_term_date.GetRow()
IF ll_row > 0 THEN
	li_rtn = dw_override_term_date.DeleteRow(ll_row)
	
	SQLCA.nf_begin_transaction()
	
	li_rtn = dw_override_term_date.Update()
	li_rtn = SQLCA.nf_handle_error("w_override_termination_date", "", "cb_delete - dw_override_term_date.Update()")
	
	SQLCA.nf_commit_transaction()
END IF

li_rtn = wf_retrieve_man_term_date_dw()

end event

type cb_cancel from commandbutton within w_override_termination_date
integer x = 1170
integer y = 644
integer width = 329
integer height = 92
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Cancel"
end type

event clicked;Long    ll_num_rows
Integer li_rtn

SetPointer(Hourglass!)

li_rtn = wf_retrieve_man_term_date_dw()

cb_add.Enabled = TRUE
cb_cancel.Enabled = FALSE
cb_save.Enabled = FALSE

end event

type cb_add from commandbutton within w_override_termination_date
integer x = 713
integer y = 644
integer width = 329
integer height = 92
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Add"
end type

event clicked;Long ll_row

cb_add.Enabled = FALSE
cb_cancel.Enabled = TRUE
cb_save.Enabled = TRUE

dw_override_term_date.Visible = TRUE
cbx_open_ended_drug_coverage.Visible = TRUE

ll_row = dw_override_term_date.InsertRow(0)
dw_override_term_date.Modify("eligibility_end_date.Background.Color='16777215' eligibility_end_date.protect=0")
dw_override_term_date.Enabled = TRUE
dw_override_term_date.SetItem(ll_row, "claim_no", il_claim_no)
dw_override_term_date.SetItem(ll_row, "record_no", 0)
dw_override_term_date.SetColumn("eligibility_end_date")
dw_override_term_date.SetFocus()

end event

type cbx_open_ended_drug_coverage from checkbox within w_override_termination_date
integer x = 142
integer y = 396
integer width = 809
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Open Ended Drug Coverage"
end type

event clicked;Long     ll_row
Datetime ldt_null

SetNull(ldt_null)
cb_save.Enabled = TRUE
cb_cancel.Enabled = TRUE

IF cbx_open_ended_drug_coverage.Checked = TRUE THEN
	ll_row = dw_override_term_date.GetRow()
	IF ll_row > 0 THEN
		dw_override_term_date.SetItem(ll_row, "eligibility_end_date", ldt_null)
	END IF
	
	dw_override_term_date.Modify("eligibility_end_date.Background.Color='553648127' eligibility_end_date.protect=1")
ELSE
	dw_override_term_date.Modify("eligibility_end_date.Background.Color='16777215' eligibility_end_date.protect=0")
END IF

end event

type dw_override_term_date from u_dw_online within w_override_termination_date
integer x = 101
integer y = 96
integer width = 2889
integer height = 276
integer taborder = 10
string title = "none"
string dataobject = "d_override_term_date"
boolean border = false
end type

event dberror;SQLCA.SQLDBCode = sqldbcode
SQLCA.SQLErrText = sqlerrtext
RETURN 1

end event

event itemchanged;Long     ll_days
Datetime ldt_null
Date     ldt_eligibility_end_date, ldt_server_date, ldt_future_date
STRING	ls_data

SetNull(ldt_null)
cb_save.Enabled = TRUE
cb_cancel.Enabled = TRUE

IF dwo.Name = "eligibility_end_date" THEN
	ls_data = LEFT(data,10)
	ldt_eligibility_end_date = Date(ls_data)
	ldt_server_date = Date(f_server_datetime())

	ll_days = DaysAfter(ldt_server_date, ldt_eligibility_end_date)
	IF ll_days > 365 THEN
		MessageBox("Warning", "The override date that you entered, " + String(ldt_eligibility_end_date, "mmm dd, yyyy") +&
		           " is more than a year from now.")
	END IF
END IF

end event

event editchanged;cb_save.Enabled = TRUE
cb_cancel.Enabled = TRUE

end event

type gb_2 from groupbox within w_override_termination_date
integer x = 55
integer y = 20
integer width = 2949
integer height = 548
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Enter Date or Select Open Ended:"
end type

