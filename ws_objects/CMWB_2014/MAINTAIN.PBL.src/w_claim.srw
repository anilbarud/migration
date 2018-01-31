$PBExportHeader$w_claim.srw
$PBExportComments$main claim maintenance - no adds or deletes allowed
forward
global type w_claim from w_a_tool
end type
type cb_cancel from commandbutton within w_claim
end type
type cb_participant from commandbutton within w_claim
end type
type cb_save from commandbutton within w_claim
end type
type cb_delete from commandbutton within w_claim
end type
type cb_add from commandbutton within w_claim
end type
type cb_emp_search from commandbutton within w_claim
end type
type sle_openings from singlelineedit within w_claim
end type
type cb_rx_term from commandbutton within w_claim
end type
type dw_payment from u_dw_online within w_claim
end type
type dw_unapplied_claim_txn from u_dw_online within w_claim
end type
type dw_deleted_payment from u_dw_online within w_claim
end type
type dw_deleted_claim_txn from u_dw_online within w_claim
end type
type dw_claim_no from u_dw_online within w_claim
end type
type cb_rtw from commandbutton within w_claim
end type
type tabpages_claim from tab within w_claim
end type
type tab_accident_stat from userobject within tabpages_claim
end type
type cb_emp_search2 from commandbutton within tab_accident_stat
end type
type cb_accident_statistics from commandbutton within tab_accident_stat
end type
type dw_difficult_claim from u_dw_online within tab_accident_stat
end type
type dw_accident_stat from u_dw_online within tab_accident_stat
end type
type tab_accident_stat from userobject within tabpages_claim
cb_emp_search2 cb_emp_search2
cb_accident_statistics cb_accident_statistics
dw_difficult_claim dw_difficult_claim
dw_accident_stat dw_accident_stat
end type
type tab_pension from userobject within tabpages_claim
end type
type cb_emp_search3 from commandbutton within tab_pension
end type
type dw_pension from u_dw_online within tab_pension
end type
type tab_pension from userobject within tabpages_claim
cb_emp_search3 cb_emp_search3
dw_pension dw_pension
end type
type tab_openings from userobject within tabpages_claim
end type
type dw_openings from u_dw_online within tab_openings
end type
type dw_opening_details from u_dw_online within tab_openings
end type
type tab_openings from userobject within tabpages_claim
dw_openings dw_openings
dw_opening_details dw_opening_details
end type
type tab_reminders from userobject within tabpages_claim
end type
type dw_claim_reminder_details from u_dw_online within tab_reminders
end type
type dw_claim_reminder from u_dw_online within tab_reminders
end type
type tab_reminders from userobject within tabpages_claim
dw_claim_reminder_details dw_claim_reminder_details
dw_claim_reminder dw_claim_reminder
end type
type tabpages_claim from tab within w_claim
tab_accident_stat tab_accident_stat
tab_pension tab_pension
tab_openings tab_openings
tab_reminders tab_reminders
end type
type dw_override_main from u_dw_online within w_claim
end type
type st_open_ended from statictext within w_claim
end type
type dw_claim from u_dw_online within w_claim
end type
type dw_eligible_report_fee from u_dw_online within w_claim
end type
type dw_firefighter_claim from u_dw_online within w_claim
end type
type dw_firefighter_claim_participant from u_dw_online within w_claim
end type
type dw_firefighter_claim_accident from u_dw_online within w_claim
end type
type dw_rejected_wca_claim_events from u_dw_online within w_claim
end type
type dw_rejected_wca_difficult_claim from u_dw_online within w_claim
end type
type cb_create_firefighter_claim from commandbutton within w_claim
end type
end forward

global type w_claim from w_a_tool
integer width = 2862
integer height = 1992
string title = ""
boolean resizable = false
cb_cancel cb_cancel
cb_participant cb_participant
cb_save cb_save
cb_delete cb_delete
cb_add cb_add
cb_emp_search cb_emp_search
sle_openings sle_openings
cb_rx_term cb_rx_term
dw_payment dw_payment
dw_unapplied_claim_txn dw_unapplied_claim_txn
dw_deleted_payment dw_deleted_payment
dw_deleted_claim_txn dw_deleted_claim_txn
dw_claim_no dw_claim_no
cb_rtw cb_rtw
tabpages_claim tabpages_claim
dw_override_main dw_override_main
st_open_ended st_open_ended
dw_claim dw_claim
dw_eligible_report_fee dw_eligible_report_fee
dw_firefighter_claim dw_firefighter_claim
dw_firefighter_claim_participant dw_firefighter_claim_participant
dw_firefighter_claim_accident dw_firefighter_claim_accident
dw_rejected_wca_claim_events dw_rejected_wca_claim_events
dw_rejected_wca_difficult_claim dw_rejected_wca_difficult_claim
cb_create_firefighter_claim cb_create_firefighter_claim
end type
global w_claim w_claim

type variables
S_WINDOW_MESSAGE istr_message
N_MAINTAIN_CLAIM_CONTROLLER inv_controller
N_BR_BLUECROSS	 inv_bc
N_REMINDERS inv_reminders
LONG il_claim_no, il_reminder_no
STRING is_mode
BOOLEAN ib_lose_focus, ib_StatusChanged, ib_remove_override, ib_accident_date_change, ib_registered = FALSE, ib_firefighter_visible
W_OVERRIDE_TERMINATION_DATE	iw_override_termination
DATE idt_old_eligibility_end_date
W_JOB_SEARCH_RTW  iw_job_search_rtw


end variables

forward prototypes
public function integer wf_read_only ()
public function integer wf_check_payments (long al_opening_no, long al_claim_no)
public function integer wf_is_override_valid ()
public function integer wf_set_cb_rx_term_state ()
public function integer wf_set_rtw (string as_status_code, string as_status_type)
public function integer wf_setup_reminder_flag ()
public subroutine wf_firefighter_button ()
public subroutine wf_set_side_of_body_desc ()
private function integer wf_route_unpaid_docs (string as_original_claim_status)
public function integer wf_reverse_payments_on_employer_8000 (long al_claim_no)
end prototypes

public function integer wf_read_only ();/*	protect all columns of visible dw's
*/
	dw_claim.uf_protect_allattributes(TRUE)
	tabpages_claim.tab_accident_stat.dw_accident_stat.uf_protect_allattributes(TRUE)
	tabpages_claim.tab_accident_stat.dw_difficult_claim.uf_protect_allattributes(TRUE)
	tabpages_claim.tab_pension.dw_pension.uf_protect_allattributes(TRUE)
	tabpages_claim.tab_openings.dw_openings.uf_protect_allattributes(TRUE)
	tabpages_claim.tab_openings.dw_opening_details.uf_protect_allattributes(TRUE)
	tabpages_claim.tab_reminders.dw_claim_reminder.uf_protect_allattributes(TRUE)
	tabpages_claim.tab_reminders.dw_claim_reminder_details.uf_protect_allattributes(TRUE)
	
	/* PR6638 - 2008-04-28 r.s. - One field was not being protected in dw_difficult_claim. The function uf_protect_allattributes() 
	 	in the ancestor object u_dw_online cycles thru based on column count, protecting each column. 
		dw_difficult_claim has 6 columns but 7 fields, complex_adjudication_code is overlaid on filtered_complex_adjudication_code 
		the later protected by u_dw_online so we'll protect complex_adjudication_code manually here
	*/
	tabpages_claim.tab_accident_stat.dw_difficult_claim.Modify("complex_adjudication_code.Protect= 1" )
	// end PR6638
	
	dw_claim.uf_set_backcolor()
	tabpages_claim.tab_accident_stat.dw_accident_stat.uf_set_backcolor()
	tabpages_claim.tab_accident_stat.dw_difficult_claim.uf_set_backcolor()
	tabpages_claim.tab_pension.dw_pension.uf_set_backcolor()
	tabpages_claim.tab_openings.dw_openings.uf_set_backcolor()
	tabpages_claim.tab_openings.dw_opening_details.uf_set_backcolor()
	tabpages_claim.tab_reminders.dw_claim_reminder.uf_set_backcolor()
	tabpages_claim.tab_reminders.dw_claim_reminder_details.uf_set_backcolor()
	
	//PR6638 - part of PR described above - its now protected, so set the background color so it looks disabled.
	tabpages_claim.tab_accident_stat.dw_difficult_claim.Modify("complex_adjudication_code.Background.Color= '553648127'" )
	
/*	il_read_only = 1 if read only and 0 if not
	this is used to pass along to the participant window
*/
	is_mode = 'READ'

/*	disable the buttons
*/
	cb_add.enabled = FALSE
	cb_cancel.enabled = FALSE
	cb_delete.enabled = FALSE
	cb_emp_search.enabled = FALSE
	cb_save.enabled = FALSE
	cb_create_firefighter_claim.enabled = FALSE
	/* PR6638 - 2008-04-28 r.s. as part of this PR, disable the other 2 employer search buttons
		so as to prevent a user from changing an employer and saving the change, when in READ ONLY state
	*/
	tabpages_claim.tab_accident_stat.cb_emp_search2.enabled = FALSE
	tabpages_claim.tab_pension.cb_emp_search3.enabled = FALSE
Return 0


end function

public function integer wf_check_payments (long al_opening_no, long al_claim_no);/*  PR3917 - IF Payments, Benefit Calculations, or Awards exist for
    this opening, do not allow the user to change the opening type.
*/

LONG 		ll_payments, ll_awards, ll_bencalc

SELECT Count(*)
INTO   :ll_payments
FROM   PAYMENT 
WHERE  claim_no   = :al_claim_no
AND    opening_no = :al_opening_no
USING  SQLCA;

IF SQLCA.nf_handle_error('SELECT from Claim_Disposition_Type','dw_opening_details','itemchanged')<0 THEN
	Return -1
END IF

IF ll_payments > 0 THEN
	RETURN 1
END IF

SELECT Count(*)
INTO   :ll_awards
FROM   PERIODIC_AWARD
WHERE  claim_no   = :al_claim_no
AND    opening_no = :al_opening_no
USING  SQLCA;

IF SQLCA.nf_handle_error('SELECT from Claim_Disposition_Type','dw_opening_details','itemchanged')<0 THEN
	Return -1
END IF

IF ll_awards > 0 THEN
	RETURN 1
END IF

SELECT Count(*)
INTO   :ll_bencalc
FROM   BENEFIT_CALCULATION
WHERE  claim_no   = :al_claim_no
AND    opening_no = :al_opening_no
USING  SQLCA;

IF SQLCA.nf_handle_error('SELECT from Claim_Disposition_Type','dw_opening_details','itemchanged')<0 THEN
	Return -1
END IF

IF ll_bencalc > 0 THEN
	RETURN 1
END IF

RETURN 0
end function

public function integer wf_is_override_valid ();INTEGER	li_rtn
LONG		ll_rows
String	ls_status, ls_status_type
Datetime	ldt_ord

N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '008' refers to the Claim Maintenance module, '046' refers to the ABCC Eligibility Export module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('027','046','close',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return -1
END IF

/******************************************************************************************/



// Check for an Override Term Date
IF dw_override_main.Retrieve(il_claim_no) < 1 THEN
	RETURN 0
END IF	

ldt_ord 			= dw_override_main.GetItemDateTime(dw_override_main.GetRow(), 'eligibility_end_date')
ls_status 		= dw_claim.GetItemString(dw_claim.getrow(),'claim_status_code')
ls_status_type = dw_claim.GetItemString(dw_claim.getrow(),'claim_status_type_code')


IF ls_status <> 'A' AND (ls_status <> 'F' AND (ls_status_type <> '01' OR ls_status_type <> '02' OR & 
 	ls_status_type <> '03' OR ls_status_type <> '04')) THEN
	   MessageBox('WARNING','Override Termination Date will be removed.')
		dw_override_main.DeleteRow(dw_override_main.GetRow())
		
		
		SQLCA.nf_begin_transaction()
		
		dw_override_main.Update()
		SQLCA.nf_handle_error("w_claim", "", "cb_cancel - dw_override_main.Update()")
	
		SQLCA.nf_commit_transaction()		
END IF


RETURN 0
end function

public function integer wf_set_cb_rx_term_state ();// wf_set_cb_rx_term_state - Disable Rx Term button if claim is not active or finalled or claim is excluded from drug coverage
//
Long    ll_count, ll_row, ll_count_status
Integer li_rtn
String  ls_claim_status_code, ls_claim_status_type_code
Datetime ldt_death_date

ll_row = dw_claim.GetRow()
IF ll_row > 0 THEN
	
	SELECT COUNT(*)
	INTO   :ll_count
	FROM   X001_REGISTRATION
	WHERE  claim_no = :il_claim_no
	USING  SQLCA;
	
	SQLCA.nf_handle_error('w_claim', '', 'wf_set_cb_rx_term_state - SELECT COUNT(*) FROM X001_REGISTRATION')
	
	IF ll_count > 0 THEN
		cb_rx_term.Enabled = FALSE
		
		SELECT COUNT(*)
		INTO   :ll_count
		FROM   X001_OVERRIDE_ELIGIBILITY
		WHERE  claim_no = :il_claim_no
		USING  SQLCA;
		
   		SQLCA.nf_handle_error('w_claim', '', 'wf_set_cb_rx_term_state - SELECT COUNT(*) FROM X001_OVERRIDE_ELIGIBILITY')		
		
		RETURN 0
	END IF
	
	IF inv_bc.uf_check_if_excluded_claim(il_claim_no) = TRUE THEN
		cb_rx_term.Enabled = FALSE
		RETURN 0
	END IF
	
	//Check if deceased
	SELECT	I.death_date
	INTO		:ldt_death_date
	FROM 		CLAIM C, INDIVIDUAL I
	WHERE		C.claim_no = :il_claim_no
	AND		I.individual_no = C.individual_no
	USING 	SQLCA;

	SQLCA.nf_handle_error("n_br_bluecross","wf_set_cb_rx_term_state","select from CLAIM")

	IF NOT ISNULL(ldt_death_date) THEN
		cb_rx_term.Enabled = FALSE
		RETURN 0
	END IF	
	
	ls_claim_status_code = dw_claim.GetItemString(ll_row, "claim_status_code")
	IF ls_claim_status_code = 'A' THEN
		cb_rx_term.Enabled = TRUE
		wf_set_rtw(ls_claim_status_code,'')
		RETURN 0
	END IF	
	IF ls_claim_status_code = 'F' THEN
	   ls_claim_status_type_code = dw_claim.GetItemString(ll_row, "claim_status_type_code")
	   
		IF ls_claim_status_type_code = '01' OR ls_claim_status_type_code = '02' OR ls_claim_status_type_code = '03' THEN
			cb_rx_term.Enabled = TRUE
			wf_set_rtw(ls_claim_status_code,ls_claim_status_type_code)
			RETURN 0
		END IF	
		
		IF ls_claim_status_type_code = '04' THEN
		
			SELECT COUNT(*)
			INTO	 :ll_count
			FROM	 ACCIDENT
			WHERE	 claim_no = :il_claim_no
			AND	 (nature_of_injury_code IS NULL OR nature_of_injury_code = '')
			USING	 SQLCA;
		
			SQLCA.nf_handle_error("w_claim","wf_set_cb_rx_term_state","select from ACCIDENT")

			IF ll_count > 0  THEN
				cb_rx_term.Enabled = FALSE
			ELSE
				cb_rx_term.Enabled = TRUE
			END IF	
		ELSE
			cb_rx_term.Enabled = FALSE
		END IF
	ELSE
		cb_rx_term.Enabled = FALSE
	END IF	
END IF

RETURN 0

end function

public function integer wf_set_rtw (string as_status_code, string as_status_type);String ls_status_code,ls_status_type
Long   ll_count_status, ll_count, ll_count_openings



ls_status_code = as_status_code
ls_status_type = as_status_type

Select  count(*)
Into     :ll_count_status
From    CLAIM_STATUS_CHANGE
Where  claim_no = :il_claim_no
And      (old_claim_status_code = 'A'  OR  (old_claim_status_code = 'F' AND old_claim_status_type_code = '01'))
Using SQLCA;

SQLCA.nf_handle_error("w_claim", "", "open - Select count(*) From CLAIM_STATUS_CHANGE")

Select count(*)
Into     :ll_count
From   RTW_INCENTIVE_QUALIFICATION
Where claim_no = :il_claim_no
Using SQLCA;

SQLCA.nf_handle_error("w_claim", "", "open - Select count(*) From RTW_INCENTIVE_QUALIFCATION")

Select count(*)
Into    :ll_count_openings
From   OPENING
Where claim_no = :il_claim_no 
And     opening_type_code = 'RLOE'
Using   SQLCA;

SQLCA.nf_handle_error("w_claim", "", "open - Select count(*) From RTW_INCENTIVE_QUALIFCATION")

//IF there is an RLOE opening and the status is or was Active/ Finalled-Final OR there is a RTW Qualification then enable the button.

IF ll_count_openings > 0 AND (ls_status_code = 'A' OR (ls_status_code = 'F' AND ls_status_type = '01')  OR ll_count_status > 0)  OR ll_count > 0 THEN
	cb_rtw.enabled = True
ELSE
	cb_rtw.enabled = False
END IF

Return 0
end function

public function integer wf_setup_reminder_flag ();String ls_string, ls_new_pic
Long	ll_position, ll_len_white, ll_len_red, ll_count, ll_count_overdue
int 	li_rtn
Date 	ldt_date

ls_string = tabpages_claim.tab_reminders.picturename

li_rtn = tabpages_claim.tab_reminders.SetRedraw(True)

ldt_date = Date(f_server_datetime())


	
	Select count(*)
	Into    :ll_count_overdue
	From  CLAIM_REMINDER
	Where claim_no = :il_claim_no
	And     reminder_status_code = 'P'
	And     due_date <= :ldt_date 
	Using SQLCA;
	
	SQLCA.nf_handle_error('w_claim','wf_setup_reminder_flag','Select from CLAIM_REMINDER')
	
	Select count(*)
	Into    :ll_count
	From  CLAIM_REMINDER
	Where claim_no = :il_claim_no
	And     reminder_status_code = 'P'
	And     due_date > :ldt_date 
	Using SQLCA;
	
	SQLCA.nf_handle_error('w_claim','wf_setup_reminder_flag','Select from CLAIM_REMINDER')
	
	// Planned reminders overdue show red flag.
	// Planned reminders future due date show white flag.
	IF ll_count_overdue > 0 THEN 
		tabpages_claim.tab_reminders.picturename = "followup_red_selected.gif"
	ELSEIF ll_count > 0 THEN
		tabpages_claim.tab_reminders.picturename = "followup_white_selected.gif"
	ELSE
		tabpages_claim.tab_reminders.picturename = ""
	END IF


//IF ls_string = '' THEN
		//tabpages_claim.tab_reminders.picturename = "Start!"
//		tabpages_claim.tab_reminders.picturename = "Warning!"
//END IF	

Return 0
end function

public subroutine wf_firefighter_button ();INTEGER       	li_count
STRING         ls_administering_act_code, ls_claim_status_code, ls_claim_status_type_code

SELECT Count(*)
INTO   :li_count
FROM   CLAIM
WHERE  rejected_wca_claim_no = :il_claim_no
USING SQLCA;
SQLCA.nf_handle_error('w_claim', 'wf_firefighter_button', 'SELECT Count(*) FROM CLAIM...')

IF li_count = 0 THEN
	ls_administering_act_code = dw_claim.GetItemString(1, 'administering_act_code')
	ls_claim_status_code = dw_claim.GetItemString(1, 'claim_status_code')
	ls_claim_status_type_code = dw_claim.GetItemString(1, 'claim_status_type_code')

	IF ls_claim_status_code = 'R' and ls_claim_status_type_code = '07' AND ls_administering_act_code = 'WCA' AND ib_firefighter_visible THEN
		cb_create_firefighter_claim.Visible = TRUE
	ELSE
		cb_create_firefighter_claim.Visible = FALSE
	END IF
ELSE
	cb_create_firefighter_claim.Visible = FALSE
END IF
end subroutine

public subroutine wf_set_side_of_body_desc ();STRING       ls_side_of_body_code
U_DW_ONLINE  ldw_accident_stat


ldw_accident_stat = THIS.tabpages_claim.tab_accident_stat.dw_accident_stat


ls_side_of_body_code = ldw_accident_stat.GetItemString(1,'side_of_body_code')

ldw_accident_stat.setItem(1, 'side_of_body_description', ls_side_of_body_code)

end subroutine

private function integer wf_route_unpaid_docs (string as_original_claim_status);
// On a claim status change from Pre-Adjudication, Adjudication, Rejected - Dissallowed or Rejected Insufficient Information , 
// to a new claim status of Active or Finaled (Except Out Of Province and Third Party),
//  all unpaid accounts MAY be automatically routed to the appropriate default taxing category for the claim's region, for payment.
//  BUT in real world, only a claim status of Adjudication can be changed to Active or Finaled, the other statuses have to first be changed 
//  to Adjudication because of other reasons, so in reality, we only need to test for a previous status code of 'J'

STRING ls_new_claim_status, ls_new_claim_status_type
STRING ls_admin_region, ls_action, ls_claimant_name, ls_keyword
STRING ls_text

long ll_taxing_catid = 774    //  this is the cat id for the ABS-Taxing bucket - direct SQL insert (below) cant use constants so thats why its not declared as such -

long       ll_row,  ll_setid, ll_claim_no, ll_fldid , ll_rows, ll_docid
int          li_inserted_row, li_cntr, li_trancount, li_rtn
DATE      ldt_action_date
datastore lds_send_documents, lds_unpaid_account_docs
n_imaging        inv_imaging 


ll_row = dw_claim.getRow()

ls_new_claim_status        =  dw_claim.getItemstring(ll_row, 'claim_status_code')
ls_new_claim_status_type = dw_claim.getItemstring(ll_row, 'claim_status_type_code')
ls_admin_region              = dw_claim.getItemString(ll_row, 'admin_region_code')


IF  as_original_claim_status = 'J'  THEN
	 
	IF  ls_new_claim_status = 'A' OR (ls_new_claim_status = 'F' AND &
	    ls_new_claim_status_type <> '05' and ls_new_claim_status_type <> '17' ) THEN
		
		ls_admin_region = dw_claim.getItemString(ll_row, 'admin_region_code') 
		
		//Retrieve any unpaid account documents
		lds_unpaid_account_docs = create datastore
		lds_unpaid_account_docs.dataobject = 'd_unpaid_account_docs'
		lds_unpaid_account_docs.settransobject(SQLCA)
				
		ll_rows = lds_unpaid_account_docs.retrieve(il_claim_no)
		
		//Check to see if user want to auto route these
				
		IF ll_rows > 0 THEN
			IF ll_rows =  1 then 
				ls_text = 'There is ' + STRING (ll_rows) +  " unpaid account document"
			ELSE 
				ls_text = 'There are ' + STRING (ll_rows) +  " unpaid account document "
			END IF
			li_rtn = Messagebox("Unpaid Account Documents to Route", ls_text + " that can be routed to the PC-ABS TAXING category." &
			                              + "~r~n~r~nDo you wish to proceed with the automatic routing of these documents to that taxing category?" , Question!, YesNo!, 2)
			IF 	li_rtn = 2 THEN
				return 1
			END IF
		END IF
		
		
		IF isnull(ls_admin_region) or ls_admin_region = '' THEN
			Messagebox("", "unable to determine admin region code, cannot procede until this is corrected")
			Return -1
		END IF
		
		
		SELECT a.given_names + ' ' + a.last_name
		INTO   :ls_claimant_name
		FROM   INDIVIDUAL a
		JOIN   CLAIM b ON a.individual_no = b.individual_no
		WHERE  b.claim_no = :il_claim_no
		USING SQLCA;
		SQLCA.nf_handle_error('w_send_folder', 'nf auto route the documents', 'embedded SQL: SELECT given_names + last_name FROM INDIVIDUAL JOIN CLAIM...')
		
		ll_claim_no		  = il_claim_no
		ls_action        =  "TX"
		ldt_action_date  = DATE(f_server_datetime())
		ls_keyword       = "STAT CHG.-unpd to TX"
			
		inv_imaging = CREATE n_imaging
		
		/* Get the setid for the Category. */
		ll_setid = inv_imaging.nf_get_setid(ll_taxing_catid)
		
		ImageTrans.nf_begin_transaction()
			
		For li_cntr = 1 to ll_rows
		
			/* Create a work folder.*/
			ll_fldid = inv_imaging.nf_create_workfolder("w_claim",ll_taxing_catid)
			IF ll_fldid = -1 THEN
				MessageBox("Auto-route unpaid account docs","Auto-route unpaid account docs - Unable to create work folder. Please contact help desk.")
				GOTO Normal_Exit
			END IF
			
			ll_docid = lds_unpaid_account_docs.getItemNumber(li_cntr, 'docid')
			
			li_rtn = inv_imaging.nf_code_claimsworking("w_claim", ll_fldid, ll_claim_no, ls_action, ldt_action_date, ls_keyword, ls_claimant_name)
			
			IF li_rtn = -1 THEN
				MessageBox("Auto-route unpaid account docs","Auto-route unpaid account docs - Unable to create claim working. Please contact help desk.")
				GOTO Normal_Exit
			END IF	
	
		/* Add the document to the folder.
		*/
			INSERT INTO REF (docid, docfldid, doccatid, docsetid )
			VALUES (:ll_docid, :ll_fldid, :ll_taxing_catid, :ll_setid )
			 USING ImageTrans;
		
			ImageTrans.nf_handle_error("w_claim","nf auto route the documents","INSERT INTO REF") 
				
	
		/* Increment the document reference counter.
		*/
			UPDATE DOC
			SET docrefcount = docrefcount + 1
			WHERE docid = :ll_docid
			USING ImageTrans;
		
			ImageTrans.nf_handle_error("w_send_folder","nf auto route the documents","UPDATE DOC") 
		
		NEXT	
	
		ImageTrans.nf_transaction_count(li_trancount,1,'','','',FALSE)
		IF li_trancount > 0 THEN
			ImageTrans.nf_commit_transaction()
		END IF

	return 1
	
	Normal_Exit:
		
		ImageTrans.nf_transaction_count(li_trancount,1,'','','',FALSE)
		IF li_trancount > 0 THEN
			ImageTrans.nf_rollback_transaction()
		END IF	
		SetPointer(Arrow!)
	
		RETURN	 -1	
	END IF
END IF


end function

public function integer wf_reverse_payments_on_employer_8000 (long al_claim_no);
LONG ll_row
LONG ll_cost_alloc_no
INT    li_cost_alloc_op_no
STRING ls_new_claim_status, ls_new_claim_status_type

ll_row = dw_claim.GetRow()
ls_new_claim_status        =  dw_claim.getItemstring(ll_row, 'claim_status_code')
ls_new_claim_status_type = dw_claim.getItemstring(ll_row, 'claim_status_type_code')

//only create reversals if new status is Active or Finaled (except third party  or  Out of province)
IF ls_new_claim_status = 'A' OR (ls_new_claim_status = 'F' AND &
	ls_new_claim_status_type <> '05' and ls_new_claim_status_type <> '17' ) THEN
	
	ll_cost_alloc_no      = dw_claim.GetItemNumber(ll_row, "cost_alloc_no")
	li_cost_alloc_op_no = dw_claim.GetItemNumber(ll_row, "cost_alloc_operation_no")

	DECLARE lp_reverse_payments PROCEDURE FOR p_Reverse_Payments_for_Employer_8000
		@claim_no           		       = :il_claim_no,
		@new_cost_alloc_no           = :ll_cost_alloc_no,
		@new_cost_alloc_op_no      = :li_cost_alloc_op_no
	USING SQLCA;
	
	SQLCA.nf_handle_error("w_claim","wf_reverse_payments_employer_8000","DECLARE lp_reverse_payments")
	
	EXECUTE lp_reverse_payments;
	SQLCA.nf_handle_error('w_claim','wf_reverse_payments_employer_8000()','EXECUTE lp_reverse_payments;')

END IF


return 1
end function

event open;call super::open;U_DWA   ldw_dw[]
STRING  ls_string, ls_oper_no, ls_status_code, ls_status_type, ls_sob
LONG    ll_emp_no, ll_oper_no, ll_rows, ll_count, ll_count_status


istr_message = Message.PowerObjectParm

iw_active_sheet = w_frame.GetActiveSheet()
il_claim_no = iw_active_sheet.dw_basic_claim.GetItemNumber(1,'claim_no')


inv_controller = Create n_maintain_claim_controller
inv_bc = Create n_br_bluecross
inv_reminders = Create n_reminders

inv_controller.nf_set_window_parent(THIS)

ldw_dw[1] = dw_claim
ldw_dw[2] = dw_claim_no
ldw_dw[3] = tabpages_claim.tab_openings.dw_openings
ldw_dw[4] = tabpages_claim.tab_accident_stat.dw_difficult_claim

/* for medical society agreement - these will be invisible to the user.
*/
ldw_dw[5] = dw_eligible_report_fee
ldw_dw[6] = dw_payment
ldw_dw[7] = dw_unapplied_claim_txn
ldw_dw[8] = dw_deleted_payment
ldw_dw[9] = dw_deleted_claim_txn
///* end of add for medical society agreement*/
ldw_dw[10] = dw_override_main
ldw_dw[11] = tabpages_claim.tab_accident_stat.dw_accident_stat
ldw_dw[12] = tabpages_claim.tab_pension.dw_pension
ldw_dw[13] = tabpages_claim.tab_openings.dw_opening_details
ldw_dw[14] = tabpages_claim.tab_reminders.dw_claim_reminder
ldw_dw[15] = tabpages_claim.tab_reminders.dw_claim_reminder_details

// for Firefighters' Compensation Act project P10281 - invisible to user
ldw_dw[16] = dw_firefighter_claim
ldw_dw[17] = dw_firefighter_claim_participant
ldw_dw[18] = dw_firefighter_claim_accident
ldw_dw[19] = dw_rejected_WCA_claim_events
ldw_dw[20] = dw_rejected_WCA_difficult_claim

inv_controller.nf_set_datawindow(ldw_dw[],SQLCA)
inv_controller.nf_init()
inv_controller.nf_retrieve(il_claim_no)


IF inv_bc.uf_is_registered(il_claim_no) > 0 THEN
	ib_registered = TRUE
	dw_override_main.object.eligibility_end_date_t.Visible = FALSE
	st_open_ended.Visible = FALSE
	cb_rx_term.Visible = FALSE
END IF		

ll_rows = dw_override_main.Retrieve(il_claim_no)
IF ll_rows > 0 THEN
	IF inv_bc.uf_is_registered(il_claim_no) > 0 THEN
		dw_override_main.object.eligibility_end_date.Visible = FALSE
	ELSE	
		IF ISNULL(dw_override_main.GetITemDateTime(dw_override_main.GetRow(),"eligibility_end_date")) THEN
			st_open_ended.Visible = TRUE
		ELSE
			st_open_ended.Visible = FALSE
		END IF
	END IF	
END IF	

wf_set_cb_rx_term_state()

ldw_dw[3].ShareData(ldw_dw[13])
ldw_dw[1].ShareData(ldw_dw[11])
ldw_dw[1].ShareData(ldw_dw[12])
ldw_dw[14].ShareData(ldw_dw[15])
tabpages_claim.tab_openings.dw_openings.uf_setselect(1)
tabpages_claim.tab_reminders.dw_claim_reminder.uf_setselect(1)

IF ldw_dw[3].RowCount() > 0 THEN
	sle_openings.Text = '<Openings Entered>'
	cb_delete.enabled = True
END IF
dw_claim.uf_set_pbmessage(TRUE)
tabpages_claim.tab_accident_stat.dw_difficult_claim.uf_set_pbmessage(TRUE)
tabpages_claim.tab_openings.dw_opening_details.uf_set_pbmessage(TRUE)
tabpages_claim.tab_reminders.dw_claim_reminder_details.uf_set_pbmessage(TRUE)

IF dw_claim.GetItemString(1,'history_flag') = 'Y' THEN
	cb_save.enabled = TRUE
	cb_rtw.enabled = FALSE
END IF

// keep the initial status of the button, after access has been determined 
// user has to belong to a security profile that allows the user to create FFs
ib_firefighter_visible = cb_create_firefighter_claim.Visible

dw_claim.SetFocus()
IF istr_message.as_mode = 'READ' THEN
	wf_read_only()
END IF

ls_status_code = ldw_dw[1].getitemstring(1,"claim_status_code")
ls_status_type = ldw_dw[1].getitemstring(1,"claim_status_type_code")

ls_sob = ldw_dw[1].GetitemString(1,"side_of_body_code")

//IF Side of Body = U then display Unknown because U is not an active selection and was preset for existing claims
//Set the side of body description text
wf_set_side_of_body_desc()


wf_set_rtw(ls_status_code,ls_status_type)

// keep the initial status of the button, after access has been determined 
// user has to belong to FIREFIGHTER_CLAIM_CREATOR security profile


// makes 'Create Firefighter Claim' button visible, if applicable
IF ib_firefighter_visible THEN
	post function wf_firefighter_button()
END IF
end event

event closequery;call super::closequery;int		li_rtn

IF cb_save.enabled THEN
   IF MessageBox('Warning', 'Data not saved.  Save now?', Question!, YesNo!) = 1 THEN
      cb_save.TriggerEvent(Clicked!)
      IF cb_save.enabled THEN
         Message.ReturnValue = 1
      END IF
   ELSE
		// Check for an override date that should be removed if they didn't save
		inv_controller.nf_retrieve(il_claim_no)
		li_rtn = wf_is_override_valid()
		IF li_rtn = -1 THEN
			// prevent close
			RETURN 1
		END IF
	END IF
END IF

end event

on close;call w_a_tool::close;Destroy inv_controller
end on

on w_claim.create
int iCurrent
call super::create
this.cb_cancel=create cb_cancel
this.cb_participant=create cb_participant
this.cb_save=create cb_save
this.cb_delete=create cb_delete
this.cb_add=create cb_add
this.cb_emp_search=create cb_emp_search
this.sle_openings=create sle_openings
this.cb_rx_term=create cb_rx_term
this.dw_payment=create dw_payment
this.dw_unapplied_claim_txn=create dw_unapplied_claim_txn
this.dw_deleted_payment=create dw_deleted_payment
this.dw_deleted_claim_txn=create dw_deleted_claim_txn
this.dw_claim_no=create dw_claim_no
this.cb_rtw=create cb_rtw
this.tabpages_claim=create tabpages_claim
this.dw_override_main=create dw_override_main
this.st_open_ended=create st_open_ended
this.dw_claim=create dw_claim
this.dw_eligible_report_fee=create dw_eligible_report_fee
this.dw_firefighter_claim=create dw_firefighter_claim
this.dw_firefighter_claim_participant=create dw_firefighter_claim_participant
this.dw_firefighter_claim_accident=create dw_firefighter_claim_accident
this.dw_rejected_wca_claim_events=create dw_rejected_wca_claim_events
this.dw_rejected_wca_difficult_claim=create dw_rejected_wca_difficult_claim
this.cb_create_firefighter_claim=create cb_create_firefighter_claim
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_cancel
this.Control[iCurrent+2]=this.cb_participant
this.Control[iCurrent+3]=this.cb_save
this.Control[iCurrent+4]=this.cb_delete
this.Control[iCurrent+5]=this.cb_add
this.Control[iCurrent+6]=this.cb_emp_search
this.Control[iCurrent+7]=this.sle_openings
this.Control[iCurrent+8]=this.cb_rx_term
this.Control[iCurrent+9]=this.dw_payment
this.Control[iCurrent+10]=this.dw_unapplied_claim_txn
this.Control[iCurrent+11]=this.dw_deleted_payment
this.Control[iCurrent+12]=this.dw_deleted_claim_txn
this.Control[iCurrent+13]=this.dw_claim_no
this.Control[iCurrent+14]=this.cb_rtw
this.Control[iCurrent+15]=this.tabpages_claim
this.Control[iCurrent+16]=this.dw_override_main
this.Control[iCurrent+17]=this.st_open_ended
this.Control[iCurrent+18]=this.dw_claim
this.Control[iCurrent+19]=this.dw_eligible_report_fee
this.Control[iCurrent+20]=this.dw_firefighter_claim
this.Control[iCurrent+21]=this.dw_firefighter_claim_participant
this.Control[iCurrent+22]=this.dw_firefighter_claim_accident
this.Control[iCurrent+23]=this.dw_rejected_wca_claim_events
this.Control[iCurrent+24]=this.dw_rejected_wca_difficult_claim
this.Control[iCurrent+25]=this.cb_create_firefighter_claim
end on

on w_claim.destroy
call super::destroy
destroy(this.cb_cancel)
destroy(this.cb_participant)
destroy(this.cb_save)
destroy(this.cb_delete)
destroy(this.cb_add)
destroy(this.cb_emp_search)
destroy(this.sle_openings)
destroy(this.cb_rx_term)
destroy(this.dw_payment)
destroy(this.dw_unapplied_claim_txn)
destroy(this.dw_deleted_payment)
destroy(this.dw_deleted_claim_txn)
destroy(this.dw_claim_no)
destroy(this.cb_rtw)
destroy(this.tabpages_claim)
destroy(this.dw_override_main)
destroy(this.st_open_ended)
destroy(this.dw_claim)
destroy(this.dw_eligible_report_fee)
destroy(this.dw_firefighter_claim)
destroy(this.dw_firefighter_claim_participant)
destroy(this.dw_firefighter_claim_accident)
destroy(this.dw_rejected_wca_claim_events)
destroy(this.dw_rejected_wca_difficult_claim)
destroy(this.cb_create_firefighter_claim)
end on

event show;call super::show;

wf_setup_reminder_flag()
end event

type st_title from w_a_tool`st_title within w_claim
integer y = 4
integer width = 2651
string text = "Maintain Claim"
end type

type cb_close from w_a_tool`cb_close within w_claim
integer x = 2267
integer y = 1728
integer width = 389
integer height = 104
integer taborder = 110
end type

type cb_cancel from commandbutton within w_claim
integer x = 1810
integer y = 1728
integer width = 389
integer height = 104
integer taborder = 100
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Cance&l"
end type

event clicked;INTEGER	li_rtn
LONG    ll_emp_no, ll_oper_no, ll_row_count, ll_rows, ll_count_noi
STRING  ls_string, ls_oper_no, ls_status_type, ls_status, ls_sob
DATETIME  ldtm_elig_end_date
N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '008' refers to the Claim Maintenance module, '046' refers to the ABCC Eligibility Export module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('027','046','cancel',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF

/******************************************************************************************/



SetPointer(HourGlass!)

/* PR#1060 Added so that values on the openings details window would not get lost after an add/cancel */
ll_row_count = tabpages_claim.tab_openings.dw_openings.GetRow()
IF ll_row_count > 0 THEN
	IF tabpages_claim.tab_openings.dw_openings.GetItemStatus(ll_row_count,0,Primary!) = newmodified! THEN
		cb_delete.TriggerEvent(Clicked!)
	END IF
END IF	


SELECT COUNT(*)
INTO	 :ll_count_noi
FROM	 ACCIDENT
WHERE	 claim_no = :il_claim_no
AND	 (nature_of_injury_code IS NOT NULL AND nature_of_injury_code <> '')
USING	 SQLCA;
		
SQLCA.nf_handle_error("w_claim","cb_cancel","select from ACCIDENT")

inv_controller.nf_retrieve(il_claim_no)
IF ib_registered = FALSE THEN
	ls_status 	   = dw_claim.getitemstring(dw_claim.getrow(),"claim_status_code")
	ls_status_type = dw_claim.getitemstring(dw_claim.getrow(),"claim_status_type_code")
	IF  ls_status = 'A' OR (ls_status = 'F' AND (ls_status_type = '01' OR ls_status_type = '02' OR & 
 	ls_status_type = '03' OR (ls_status_type = '04' AND ll_count_noi > 0))) THEN
	   //CONTINUE
	ELSE
		IF dw_override_main.Retrieve(il_claim_no) > 0 THEN
			MessageBox('Warning','Cancelling will cause the Override Termination Date to be removed.',Exclamation!,Ok!)
			dw_override_main.DeleteRow(dw_override_main.GetRow())
			
			
			SQLCA.nf_begin_transaction()
			
			dw_override_main.Update()
	  		SQLCA.nf_handle_error("w_claim", "", "cb_cancel - dw_override_main.Update()")
						
			SQLCA.nf_commit_transaction()
			
			
			st_open_ended.Visible = False
		END IF
	END IF	
		
	
	dw_override_main.object.eligibility_end_date.Visible = 1
	ll_rows = dw_override_main.Retrieve(il_claim_no)
	IF ll_rows > 0 THEN
		IF ISNULL(dw_override_main.GetITemDateTime(dw_override_main.GetRow(),"eligibility_end_date")) THEN
			st_open_ended.Visible = True
		ELSE
			st_open_ended.Visible = False
		END IF
	END IF	
	wf_set_cb_rx_term_state()
END IF	
dw_claim.SetFocus()
cb_save.enabled = FALSE
cb_cancel.enabled = FALSE
cb_participant.enabled = TRUE

IF  tabpages_claim.tab_openings.dw_openings.RowCount() > 0 THEN
	sle_openings.Text = '<Openings Entered>'
	cb_delete.Enabled = TRUE
END IF

ls_status 	   = dw_claim.getitemstring(dw_claim.getrow(),"claim_status_code")
ls_status_type = dw_claim.getitemstring(dw_claim.getrow(),"claim_status_type_code")

wf_set_rtw(ls_status,ls_status_type) //Set the Return to Work incentive Button

tabpages_claim.tab_accident_stat.dw_accident_stat.SetItem(tabpages_claim.tab_accident_stat.dw_accident_stat.Getrow(),"accident_county_stat","")
//dw_claim.SetItem(dw_claim.Getrow(),"accident_county_stat","")


ls_sob = tabpages_claim.tab_accident_stat.dw_accident_stat.GetitemString(1,"side_of_body_code")
//IF Side of Body = U then display Unknown because U is not an active selection and was preset for existing claims
//Set the side of body description text
wf_set_side_of_body_desc()


end event

type cb_participant from commandbutton within w_claim
integer x = 1015
integer y = 1728
integer width = 389
integer height = 104
integer taborder = 80
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Participant..."
end type

on clicked;S_WINDOW_MESSAGE  lstr_message

SetPointer(HourGlass!)
lstr_message.al_doubleparm[1] = il_claim_no
lstr_message.as_mode = is_mode

OpenWithParm(w_maintain_claim_participant, lstr_message, PARENT)

iw_active_sheet.wf_set_claim(il_claim_no)

end on

type cb_save from commandbutton within w_claim
integer x = 1413
integer y = 1728
integer width = 389
integer height = 104
integer taborder = 90
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
end type

event clicked;Long     ll_row, ll_count, ll_return, ll_override_row
Integer  li_rtn, li_array, li_cntr
String   ls_claim_status_code, ls_orig_claim_status_code, ls_delete_message
String   ls_claim_status_type, ls_orig_claim_status_type_code, ls_term_date
DateTime ldt_accident_date, ldt_orig_accident_date, ldt_eligibility_end_date, ldt_death_date
DAteTime ldt_override_term_date
Boolean  lb_eligible, lb_display_claim_status_change_message, lb_display_message
n_mail    lnv_mail

N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '008' refers to the Claim Maintenance module, '044' refers to the Payment Processing module
- likewise, '046' refers to the ABCC Eligibility Export module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('008','044','save',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF

//clear variable
li_rtn = 0
li_rtn = ln_process_run_status.nf_in_progress('008','046','save',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF

/******************************************************************************************/


lnv_mail = CREATE n_mail

SetPointer(HourGlass!)

tabpages_claim.tab_accident_stat.dw_difficult_claim.SetRedraw(FALSE)
li_rtn = tabpages_claim.tab_openings.dw_opening_details.AcceptText()
IF li_rtn < 0 THEN RETURN
li_rtn = tabpages_claim.tab_accident_stat.dw_difficult_claim.AcceptText()
IF li_rtn < 0 THEN RETURN
li_rtn = tabpages_claim.tab_accident_stat.dw_accident_stat.AcceptText()
IF li_rtn < 0 THEN RETURN
li_rtn = tabpages_claim.tab_pension.dw_pension.AcceptText()
IF li_rtn < 0 THEN RETURN


IF ib_statuschanged = TRUE THEN
	lb_display_claim_status_change_message = TRUE
ELSE
	lb_display_claim_status_change_message = FALSE
END IF

// Check if claim status changed
ll_row = dw_claim.GetRow()
ls_claim_status_code = dw_claim.GetItemString(ll_row, "claim_status_code")
ls_claim_status_type = dw_claim.GetItemString(ll_row, "claim_status_type_code")

// If there is a death date, make sure there isn't an unprocessed entry
IF ll_row > 0 THEN
	SELECT I.death_date 
	  INTO :ldt_death_date
	  FROM CLAIM C, 
	       INDIVIDUAL I 
	 WHERE C.claim_no = :il_claim_no 
	   AND C.individual_no = I.individual_no ;

	li_rtn = SQLCA.nf_handle_error('w_claim', '', 'cb_save - SELECT I.death_date FROM CLAIM C, INDIVIDUAL I ')


	IF NOT IsNull(ldt_death_date) THEN
		SELECT COUNT(*)
		INTO   :ll_count
		FROM   X001_OVERRIDE_ELIGIBILITY
		WHERE  claim_no = :il_claim_no
		AND    record_no = 0
		USING  SQLCA;
		
		SQLCA.nf_handle_error('w_claim', '', 'cb_save - SELECT FROM X001_OVERRRIDE_ELIGIBILITY')
		
		IF ll_count > 0 THEN
			ib_remove_override = TRUE
			MessageBox('Information',"The claimant is now deceased. The override termination date for drug coverage is being removed as it no longer applicable.",Information!,ok!)
		END IF
	END IF
END IF

SQLCA.nf_begin_transaction()

IF ib_remove_override = TRUE THEN
	dw_override_main.DeleteRow(dw_override_main.GetRow())
	dw_override_main.Update()
	SQLCA.nf_handle_error("w_claim", "", "cb_save - dw_override_main.Update()")
END IF

IF ll_row > 0 THEN
	ls_orig_claim_status_code = dw_claim.GetItemString(ll_row, "claim_status_code", Primary!, TRUE)
	ls_orig_claim_status_type_code = dw_claim.GetItemString(ll_row, "claim_status_type_code", Primary!, TRUE)
	IF ls_claim_status_code <> ls_orig_claim_status_code OR ls_claim_status_type <> ls_orig_claim_status_type_code THEN
		inv_controller.nf_status_changed(1)			
	END IF
END IF


IF inv_controller.nf_save() = 0 THEN	
	//T012790  - evertything good so far, now check if there are any unpaid account documents 
	//that  could possibly be routed to the default taxing category
	IF ib_statuschanged THEN 
		
		/*BR1.880	On a claim status change from Pre-Adjudication, Adjudication, Rejected - Disallowed or Rejected Insufficient Information , 
		                 to a new claim status of Active or Finaled (All claim status sub types except Out Of Province and Third Party),
					    all unpaid accounts of the following types SHOULD be automatically routed to the appropriate PC- ABS Taxing bucket, for payment. 
                         'AC', 'AD', 'AH', 'AI', 'AP', 'AT', 'ANP', 'AO', 'AOR', 'AR', 'ARX', 'AV',  'AW', 'MPC', 'MPD', 'SDC', 'SDD'
		*/

		li_rtn = wf_route_unpaid_docs(ls_orig_claim_status_code)
		IF li_rtn = -1 THEN
			SQLCA.nf_rollback_transaction()
			MessageBox(" Claim Error", "There was a problem routing the unpaid documents to the taxing category." &
			                + "~r~nTry saving your changes again without performing the auto routing option, and send a note to the help desk about the problem", INFORMATION!)
			cb_cancel.triggerEvent(Clicked!)
			Return -1
		END IF
		
		
		/*
		BR 1.890	   On a claim status change from Pre-Adjudication, Adjudication, Rejected - Disallowed or Rejected Insufficient Information ,
		to a new claim status of Active or Finaled (All claim status sub types except Out Of Province and Third Party), all existing payments 
		cost allocated to the Non-Assessed Non –Allocated Claim Cost Account must be corrected (reversed) to the newly entered Cost Allocation assignment. 
		NOTE: if claim status being changed to active or finaled, it has to start at (or be changed first to) 'J' Adjudication, a restriction made by other rules
		so we only really need to check for original status of 'J'
		*/
		IF ls_orig_claim_status_code = 'J' THEN			
			li_rtn = wf_reverse_payments_on_employer_8000(il_claim_no)
			IF li_rtn = -1 THEN
				SQLCA.nf_rollback_transaction()
				MessageBox(" Claim Error", "There was a problem reversing some existing payments that were cost-allocated to employer 8000.", INFORMATION!)
			                
				cb_cancel.triggerEvent(Clicked!)
				Return -1
			END IF
		END IF
	END IF
	
	SQLCA.nf_commit_transaction()
	
	ib_statuschanged = False
	
	//call fxn to setup/send email
	li_array = Upperbound(inv_controller.ii_reason[])

	IF li_array > 0 THEN
		inv_controller.istr_email_setup.al_claim_no = il_claim_no
		FOR li_cntr = 1 to li_array
			inv_controller.istr_email_setup.ai_email_reason_code = inv_controller.ii_reason[li_cntr]
			IF lnv_mail.nf_init_values(inv_controller.istr_email_setup) < 0 THEN
				RETURN -1
			END IF
			IF lnv_mail.nf_auto_email()	< 0 THEN
				RETURN -1
			END IF
			inv_controller.ii_reason[li_cntr] = 0
		NEXT
	END IF
	
	tabpages_claim.tab_accident_stat.dw_accident_stat.SetRedraw(FALSE)

	inv_controller.nf_retrieve(il_claim_no)
	SQLCA.nf_handle_error("w_claim", "", "cb_save - inv_controller.nf_retrieve()")
	
	wf_set_cb_rx_term_state()
	wf_set_rtw(ls_claim_status_code,ls_claim_status_type)
	cb_save.enabled = FALSE
	cb_cancel.enabled = FALSE
	cb_participant.enabled = TRUE
	iw_active_sheet.wf_set_claim(il_claim_no)
	tabpages_claim.tab_accident_stat.dw_accident_stat.SetItem(tabpages_claim.tab_accident_stat.dw_accident_stat.Getrow(),"accident_county_stat","")
	//dw_claim.SetItem(dw_claim.Getrow(),"accident_county_stat","")
	tabpages_claim.tab_accident_stat.dw_accident_stat.SetItemStatus(tabpages_claim.tab_accident_stat.dw_accident_stat.Getrow(),0,Primary!,NotModified!)
	
	// set up side of body description
	wf_set_side_of_body_desc()
	
	tabpages_claim.tab_accident_stat.dw_accident_stat.SetRedraw(TRUE)
ELSE
	SQLCA.nf_rollback_transaction()
END IF

tabpages_claim.tab_accident_stat.dw_difficult_claim.SetRedraw(TRUE)
tabpages_claim.tab_reminders.dw_claim_reminder.SetRedraw(TRUE)

IF tabpages_claim.tab_reminders.dw_claim_reminder.RowCount() > 0 THEN
	wf_setup_reminder_flag()
END IF	

// makes 'Create Firefighter Claim' button visible, if applicable
IF ib_firefighter_visible THEN
	wf_firefighter_button()
END IF

end event

type cb_delete from commandbutton within w_claim
boolean visible = false
integer x = 379
integer y = 1728
integer width = 347
integer height = 104
integer taborder = 70
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Delete"
end type

event clicked;long ll_count, ll_opening_no


// An opening must not be removed if there is an associated Job Search RTW Incentive qualification.

ll_opening_no =  tabpages_claim.tab_openings.dw_openings.getitemnumber(tabpages_claim.tab_openings.dw_openings.getrow(),"opening_no")

Select count(*)
Into    : ll_count
From  RTW_INCENTIVE_QUALIFICATION
Where claim_no = :il_claim_no
And     opening_no = :ll_opening_no
Using SQLCA;

SQLCA.nf_handle_error("w_claim", "cb_delete", "clicked - Select count(*) From RTW_INCENTIVE_QUALIFCATION")

IF ll_count > 0 THEN 
	MessageBox('Error','This opening can not be removed. There is an associated Job Search RTW Incentive qualification.',Exclamation!)
	Return -1
END IF	

tabpages_claim.tab_openings.dw_openings.DeleteRow(tabpages_claim.tab_openings.dw_openings.GetRow())
IF tabpages_claim.tab_openings.dw_openings.RowCount() <= 0 THEN
   cb_delete.enabled = FALSE
	sle_openings.Text = ''
END IF
cb_cancel.enabled = TRUE
cb_save.enabled = TRUE
cb_participant.enabled = FALSE
cb_rx_term.Enabled = FALSE
cb_rtw.enabled = FALSE

end event

type cb_add from commandbutton within w_claim
boolean visible = false
integer x = 18
integer y = 1728
integer width = 347
integer height = 104
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Add"
end type

event clicked;
	
IF inv_controller.nf_insert(0) < 0 THEN
	Return
END IF

cb_delete.enabled = TRUE
cb_cancel.enabled = TRUE
cb_save.enabled = TRUE
cb_rtw.enabled = FALSE
	
tabpages_claim.tab_openings.dw_openings.TriggerEvent('ue_more_details')  //dw_openings
	 

end event

type cb_emp_search from commandbutton within w_claim
integer x = 718
integer y = 516
integer width = 101
integer height = 72
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "?"
end type

event clicked;DATAWINDOWCHILD   ldwc_child
DWObject		      l_dwo
INTEGER           li_find, li_rtn
LONG              ll_row
STRING            ls_administering_act_code, ls_firefighter_cost_alloc_no
S_WINDOW_MESSAGE  lstr_return_message, lstr_open_message
	
	SetPointer(HourGlass!)

	ll_row = dw_claim.GetRow()

   SELECT administering_act_code
	INTO   :ls_administering_act_code
	FROM   CLAIM
	WHERE  claim_no = :il_claim_no
	USING SQLCA;
	SQLCA.nf_handle_error('w_claim', 'cb_emp_search', 'SELECT administering_act_code FROM CLAIM...')
	
	IF ls_administering_act_code = 'WCA' THEN
		lstr_open_message.as_stringparm[1] = 'C'
		lstr_open_message.al_doubleparm[1] = dw_claim.GetItemNumber(ll_row,'accident_employer_no')
		lstr_open_message.al_doubleparm[2] = dw_claim.GetItemNumber(ll_row,'accident_employer_operation_no')
		OpenWithParm(w_employer_search, lstr_open_message)
	ELSEIF ls_administering_act_code = 'FCA' THEN
		lstr_open_message.as_stringparm[1] = 'F'
		lstr_open_message.al_doubleparm[1] = dw_claim.GetItemNumber(ll_row,'accident_employer_no')
		lstr_open_message.al_doubleparm[2] = dw_claim.GetItemNumber(ll_row,'accident_employer_operation_no')
		OpenWithParm(w_employer_search, lstr_open_message)
	ELSE
		MessageBox('No Compensation Act','This claim has no compensation act. Contact the HELPDESK',Exclamation!)
		RETURN
	END IF

	lstr_return_message = Message.PowerObjectParm
	IF lstr_return_message.al_doubleparm[1] > 0 THEN
		
		// the cost allocation is set differently for Workers' claims & Firefighters' claims
		IF ls_administering_act_code = 'WCA' THEN
			dw_claim.SetItem(ll_row, 'cost_alloc_no', lstr_return_message.al_doubleparm[1])
			dw_claim.SetItem(ll_row, 'cost_alloc_type_code',lstr_return_message.as_stringparm[3])
			IF lstr_return_message.as_stringparm[3] = 'S' OR lstr_return_message.as_stringparm[3] = 'A' THEN 
				IF dw_claim.GetItemNumber(ll_row,'accident_employer_no') = 0 OR IsNull(dw_claim.GetItemNumber(ll_row,'accident_employer_no')) THEN
					dw_claim.SetItem(ll_row, 'accident_employer_no', lstr_return_message.al_doubleparm[1])
					dw_claim.SetItem(ll_row,'accident_emp_name', lstr_return_message.as_stringparm[1])
				END IF
			END IF
	
			dw_claim.SetItem(ll_row, 'cost_alloc_operation_no', lstr_return_message.al_doubleparm[2])
			IF lstr_return_message.as_stringparm[3] = 'S' OR lstr_return_message.as_stringparm[3] = 'A' THEN 
				IF dw_claim.GetItemNumber(ll_row,'accident_employer_operation_no') = 0 OR IsNull(dw_claim.GetItemNumber(ll_row,'accident_employer_operation_no')) THEN
					dw_claim.SetItem(ll_row, 'accident_employer_operation_no', lstr_return_message.al_doubleparm[2])
					dw_claim.SetItem(ll_row,'employer_type_code', lstr_return_message.as_stringparm[3])
					dw_claim.SetItem(ll_row,'accident_oper_name',lstr_return_message.as_stringparm[2])
					dw_claim.SetItem(ll_row, 'classification_system_code', lstr_return_message.as_stringparm[4])
					dw_claim.SetItem(ll_row, 'classification_system_type_code', lstr_return_message.as_stringparm[5])
				END IF
			END IF
			dw_claim.SetItem(ll_row,'cost_alloc_emp_name',lstr_return_message.as_stringparm[1])
			dw_claim.SetItem(ll_row,'cost_alloc_oper_name',lstr_return_message.as_stringparm[2])
			
			dw_claim.SetColumn('cost_alloc_no')
			dw_claim.SetFocus()
		ELSE
			ls_firefighter_cost_alloc_no = String(lstr_return_message.al_doubleparm[1])
			
			li_rtn = dw_claim.GetChild('firefighter_cost_alloc_no', ldwc_child)
			li_find = ldwc_child.Find('employer_no = ' + ls_firefighter_cost_alloc_no,0, ldwc_child.RowCount())
			IF li_find > 0 THEN
				dw_claim.SetColumn('firefighter_cost_alloc_no')
				dw_claim.SetFocus()
				ldwc_child.ScrollToRow(li_find)
				ldwc_child.SelectRow(li_find,TRUE)
				dw_claim.SetText(ls_firefighter_cost_alloc_no)
			ELSE
				MessageBox('No Firefighter Employer','The Firefighter Employer was not found. Call the HELPDESK.',Exclamation!)
			END IF
			
			dw_claim.SetItem(ll_row, 'firefighter_cost_alloc_no', lstr_return_message.al_doubleparm[1])
			dw_claim.SetItem(ll_row, 'firefighter_cost_alloc_operation_no', lstr_return_message.al_doubleparm[2])
			dw_claim.SetItem(ll_row, 'cost_alloc_type_code',lstr_return_message.as_stringparm[3])	

			dw_claim.SetItem(ll_row,'cost_alloc_emp_name',lstr_return_message.as_stringparm[1])
			dw_claim.SetItem(ll_row,'cost_alloc_oper_name',lstr_return_message.as_stringparm[2])
			
			l_dwo = dw_claim.object.firefighter_cost_alloc_no
			dw_claim.POST Event ItemChanged(ll_row,l_dwo,ls_firefighter_cost_alloc_no)
			l_dwo = dw_claim.object.firefighter_cost_alloc_operation_no
			dw_claim.POST Event ItemChanged(ll_row,l_dwo,String(lstr_return_message.al_doubleparm[2]))
			
			dw_claim.SetColumn('firefighter_cost_alloc_no')
			dw_claim.SetFocus()
		END IF
		
		cb_save.enabled = TRUE
		cb_cancel.enabled = TRUE
		cb_participant.enabled = FALSE
		cb_rx_term.Enabled = FALSE
		cb_rtw.enabled = FALSE
	ELSE
		
	END IF

Return


end event

type sle_openings from singlelineedit within w_claim
integer x = 1810
integer y = 8
integer width = 814
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16711680
long backcolor = 67108864
boolean border = false
boolean autohscroll = false
boolean displayonly = true
end type

type cb_rx_term from commandbutton within w_claim
integer x = 951
integer y = 736
integer width = 247
integer height = 80
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Override"
end type

event clicked;String	ls_message, ls_status
Date		ldt_override_term_date
Long		ll_row

s_window_message lstr_message

SetPointer(HourGlass!)

lstr_message.al_doubleparm[1] = il_claim_no
lstr_message.as_mode = is_mode

OpenWithParm(w_override_termination_date, lstr_message)
ls_message = Message.StringParm
ls_status = ls_message

If ls_status = "TRUE" then
	st_open_ended.Visible = True
Else
	st_open_ended.Visible = False
End If	

ll_row = dw_override_main.Retrieve(il_claim_no)
ll_row = dw_override_main.getrow()

IF ll_row > 0 THEN
	dw_override_main.object.eligibility_end_date.Visible = TRUE
	ldt_override_term_date = DATE(dw_override_main.getitemdatetime(ll_row,'eligibility_end_date'))
	ib_remove_override = FALSE
ELSE
	dw_override_main.object.eligibility_end_date.Visible = FALSE
	ldt_override_term_date = DATE('0000-00-00')
END IF	

wf_set_cb_rx_term_state()
idt_old_eligibility_end_date = ldt_override_term_date
end event

type dw_payment from u_dw_online within w_claim
boolean visible = false
integer x = 105
integer y = 1676
integer width = 59
integer height = 120
integer taborder = 0
string dataobject = "d_med_payment"
end type

type dw_unapplied_claim_txn from u_dw_online within w_claim
boolean visible = false
integer x = 178
integer y = 1676
integer width = 59
integer height = 120
integer taborder = 0
string dataobject = "d_med_unapplied_claim"
end type

type dw_deleted_payment from u_dw_online within w_claim
boolean visible = false
integer x = 251
integer y = 1676
integer width = 59
integer height = 120
integer taborder = 0
string dataobject = "d_med_deleted_payment"
end type

type dw_deleted_claim_txn from u_dw_online within w_claim
boolean visible = false
integer x = 325
integer y = 1676
integer width = 59
integer height = 120
integer taborder = 0
string dataobject = "d_med_deleted_claim"
end type

type dw_claim_no from u_dw_online within w_claim
boolean visible = false
integer x = 23
integer y = 1784
integer width = 347
integer height = 96
integer taborder = 0
string dataobject = "d_claim_no"
end type

type cb_rtw from commandbutton within w_claim
integer x = 1175
integer y = 432
integer width = 549
integer height = 76
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&JS RTW Incentive"
end type

event clicked;String	     ls_message, ls_status
Long		ll_row

s_window_message lstr_message

SetPointer(HourGlass!)

lstr_message.al_doubleparm[1] = il_claim_no
lstr_message.as_mode = is_mode

OpenWithParm(w_job_search_rtw, lstr_message)
ls_message = Message.StringParm
ls_status = ls_message


dw_claim.Retrieve(il_claim_no)
SQLCA.nf_handle_error("w_claim", "", "clicked for cb_rtw")

wf_set_cb_rx_term_state()

end event

type tabpages_claim from tab within w_claim
integer x = 9
integer y = 828
integer width = 2656
integer height = 896
integer taborder = 50
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
boolean raggedright = true
boolean focusonbuttondown = true
integer selectedtab = 1
tab_accident_stat tab_accident_stat
tab_pension tab_pension
tab_openings tab_openings
tab_reminders tab_reminders
end type

on tabpages_claim.create
this.tab_accident_stat=create tab_accident_stat
this.tab_pension=create tab_pension
this.tab_openings=create tab_openings
this.tab_reminders=create tab_reminders
this.Control[]={this.tab_accident_stat,&
this.tab_pension,&
this.tab_openings,&
this.tab_reminders}
end on

on tabpages_claim.destroy
destroy(this.tab_accident_stat)
destroy(this.tab_pension)
destroy(this.tab_openings)
destroy(this.tab_reminders)
end on

event selectionchanged;IF newindex = 3 THEN          //tabpages_claim.tab_openings.dw_openings
	cb_delete.visible = TRUE
	cb_add.visible = TRUE
	cb_create_firefighter_claim.Visible = FALSE

	tabpages_claim.tab_openings.dw_openings.SetFocus()
ELSEIF newindex = 2 OR newindex = 4 THEN
	cb_create_firefighter_claim.Visible = FALSE
ELSEIF newindex = 1 THEN
	IF ib_firefighter_visible THEN
		wf_firefighter_button()
	END IF
ELSE
	cb_add.visible = FALSE
	cb_delete.visible = FALSE
END IF

end event

type tab_accident_stat from userobject within tabpages_claim
integer x = 18
integer y = 108
integer width = 2619
integer height = 772
long backcolor = 67108864
string text = "Accident Stats"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
cb_emp_search2 cb_emp_search2
cb_accident_statistics cb_accident_statistics
dw_difficult_claim dw_difficult_claim
dw_accident_stat dw_accident_stat
end type

on tab_accident_stat.create
this.cb_emp_search2=create cb_emp_search2
this.cb_accident_statistics=create cb_accident_statistics
this.dw_difficult_claim=create dw_difficult_claim
this.dw_accident_stat=create dw_accident_stat
this.Control[]={this.cb_emp_search2,&
this.cb_accident_statistics,&
this.dw_difficult_claim,&
this.dw_accident_stat}
end on

on tab_accident_stat.destroy
destroy(this.cb_emp_search2)
destroy(this.cb_accident_statistics)
destroy(this.dw_difficult_claim)
destroy(this.dw_accident_stat)
end on

type cb_emp_search2 from commandbutton within tab_accident_stat
integer x = 713
integer y = 408
integer width = 96
integer height = 72
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "?"
end type

event clicked;LONG              ll_row
S_WINDOW_MESSAGE  lstr_return_message, lstr_open_message


	SetPointer(HourGlass!)
	
	lstr_open_message.as_stringparm[1] = 'A'
	lstr_open_message.al_doubleparm[1] = 0
	lstr_open_message.al_doubleparm[2] = 0
	OpenWithParm(w_employer_search, lstr_open_message)

	lstr_return_message = Message.PowerObjectParm

	IF lstr_return_message.al_doubleparm[1] > 0 THEN
		ll_row = dw_claim.GetRow()
		dw_claim.SetItem(ll_row, 'accident_employer_no', lstr_return_message.al_doubleparm[1])
		dw_claim.SetItem(ll_row, 'accident_employer_operation_no', lstr_return_message.al_doubleparm[2])
		dw_claim.SetItem(ll_row,'employer_type_code', lstr_return_message.as_stringparm[3])
		dw_claim.SetItem(ll_row,'accident_emp_name',lstr_return_message.as_stringparm[1])
		dw_claim.SetItem(ll_row,'accident_oper_name',lstr_return_message.as_stringparm[2])
		dw_claim.SetItem(ll_row, 'classification_system_code', lstr_return_message.as_stringparm[4])
		dw_claim.SetItem(ll_row, 'classification_system_type_code', lstr_return_message.as_stringparm[5])

		cb_save.enabled = TRUE
		cb_cancel.enabled = TRUE
		cb_participant.enabled = FALSE
		cb_rx_term.Enabled = FALSE
		cb_rtw.enabled = FALSE
	END IF
	dw_claim.SetColumn('accident_employer_no')
	dw_claim.SetFocus()
Return


end event

type cb_accident_statistics from commandbutton within tab_accident_stat
integer x = 800
integer y = 4
integer width = 585
integer height = 72
integer taborder = 120
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Accident Statistics"
end type

event clicked;OpenWithParm(w_accidents_display, il_claim_no)
end event

type dw_difficult_claim from u_dw_online within tab_accident_stat
integer x = 5
integer y = 556
integer width = 2610
integer height = 216
integer taborder = 11
string dataobject = "d_difficult_claim"
boolean border = false
end type

event clicked;call super::clicked;DWObject dwo_new
if dwo.name = "complex_adjudication_code" then
	this.Modify("complex_adjudication_code.Visible='0'")
	dwo_new = this.object.filtered_complex_adjudication_code
	this.post event clicked(xpos,ypos,row,dwo_new)
else
	this.Modify("complex_adjudication_code.Visible='1'")
end if
end event

event itemchanged;call super::itemchanged;dw_difficult_claim.uf_set_pbmessage(TRUE)
inv_controller.nf_change_item(4)
cb_save.enabled = TRUE
cb_cancel.enabled = TRUE
cb_participant.enabled = FALSE
cb_rtw.enabled = FALSE
IF cb_rx_term.Enabled = TRUE THEN
	//CONTINUE
ELSE
	cb_rx_term.Enabled = FALSE
END IF	
end event

event losefocus;call super::losefocus;/* this is because a click inside the same DW causing lose focus to frame and successful accepttext on bad data. */
String lsClass
GraphicObject lgoFocus

lgoFocus = GetFocus()
If IsValid(lgoFocus) Then
	lsClass = ClassName(lgoFocus)
End If
If IsNull(lsClass) Then
	lsClass = ''
End If

this.Modify("complex_adjudication_code.Visible='1'")
end event

type dw_accident_stat from u_dw_online within tab_accident_stat
event ue_accepttext ( )
integer width = 2615
integer height = 568
integer taborder = 21
string dataobject = "d_claim_maintain_accident_stat"
boolean border = false
end type

event itemchanged;call super::itemchanged;String ls_accident
Datetime ldt_accident_date, ldt_orig_accident_date
Date	ldt_override_term
Long 	ll_row, ll_return
Boolean 	lb_eligible


dw_accident_stat.uf_set_pbmessage(TRUE)
ll_return = inv_controller.nf_change_item(9)

IF ll_return = 1 THEN
	cb_save.Enabled = FALSE
	cb_rtw.enabled = TRUE
	lb_eligible = FALSE
ELSEIF dwo.name = "accident_date" THEN
	ls_accident = LEFT(data,10)
	ldt_accident_date = Datetime(Date(ls_accident))
	dw_claim.SetItem(dw_claim.GetRow(), 'accident_date', ldt_accident_date)	
	ldt_orig_accident_date = dw_claim.GetItemDatetime(row, "accident_date", Primary!, TRUE)
	IF ldt_accident_date <> ldt_orig_accident_date THEN

		//BR 14.140 & 14.150 The claim accident date must be less than or equal to the effective date for a claim eligibility,
		//and claim formulary that has not been setup on BlueCross.
		
		IF inv_bc.uf_determine_eligibility_setup(il_claim_no, ldt_accident_date) < 0 THEN
			MessageBox('ERROR',"The claim's eligibility effective date is before the accident date, you must correct this to proceed.")
			RETURN 1
		END IF
		
		IF inv_bc.uf_determine_formulary_setup(il_claim_no, ldt_accident_date) < 0 THEN
			MessageBox('ERROR',"The claim's formulary effective date is before the accident date, you must correct this to proceed.")
			RETURN 1
		END IF

		IF inv_bc.uf_is_registered(il_claim_no) = 1 THEN  //Claim is registered
			dw_override_main.object.eligibility_end_date_t.Visible = FALSE
			dw_override_main.object.eligibility_end_date.Visible = FALSE
			cb_rx_term.Visible = FALSE
				
		ELSE //Claim is not registered
			ll_row = dw_override_main.GetRow()
			IF ll_row > 0 THEN
				ldt_override_term = DATE(dw_override_main.GetItemDateTime(dw_override_main.GetRow(), 'eligibility_end_date'))
				IF ldt_override_term < DATE(ldt_accident_date)  THEN
					MessageBox('ERROR',"The Override Termination Date is now before the accident date.~n" &
					+"Please correct the override termination date.",Exclamation!,OK!)
					cb_rx_term.Enabled = TRUE
					cb_cancel.Enabled = TRUE
					Return 1
				END IF
			END IF	
		END IF
	ELSE	
		lb_eligible = cb_rx_term.Enabled
	END IF
	ll_return = 0
		
ELSEIF dwo.name = 'side_of_body_code' THEN
	
	THIS.SetRedraw(FALSE)
	
	/*	Select and highlight the line.
	*/
	SelectText(1,Len(textline()))
	/*
	side_of_body_code can be 'N' for 'not coded' but only for display on existing claims with no known value, 
	'not coded' cannot be one of the selectable items in the dropdown because its not active. Thats why we have a
	plain edit field linked to a dddw that includes 'unknown' to display the description of unknown when required.
	*/
	
	//side_of_body_code_description lies on top so set it so it looks th same as what was picked, for consistency
	THIS.setItem(1,'side_of_body_description', data)
	
	THIS.SetRedraw(TRUE)

END IF

IF ll_return = 0 THEN
	cb_save.Enabled = TRUE
	cb_rtw.enabled = FALSE
END IF

cb_cancel.Enabled = TRUE
cb_participant.Enabled = FALSE

IF lb_eligible = TRUE THEN
	cb_rx_term.Enabled = TRUE
ELSE	
	cb_rx_term.Enabled = FALSE
END IF	

Return ll_return


end event

event retrieveend;call super::retrieveend;STRING    ls_part_of_body_code


ls_part_of_body_code = THIS.GetItemString(getrow(),'part_of_body_code')
if isnull(ls_part_of_body_code) or ls_part_of_body_code = '' THEN ls_part_of_body_code = '99990'  //should never happen, but put 'unknown - 99990' part of body in for now, it will be checked on save
	
wf_set_side_of_body_desc()
end event

type tab_pension from userobject within tabpages_claim
integer x = 18
integer y = 108
integer width = 2619
integer height = 772
long backcolor = 67108864
string text = "Pension"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
cb_emp_search3 cb_emp_search3
dw_pension dw_pension
end type

on tab_pension.create
this.cb_emp_search3=create cb_emp_search3
this.dw_pension=create dw_pension
this.Control[]={this.cb_emp_search3,&
this.dw_pension}
end on

on tab_pension.destroy
destroy(this.cb_emp_search3)
destroy(this.dw_pension)
end on

type cb_emp_search3 from commandbutton within tab_pension
integer x = 722
integer y = 128
integer width = 96
integer height = 72
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "?"
end type

event clicked;LONG              ll_row
STRING            ls_administering_act_code
S_WINDOW_MESSAGE  lstr_return_message, lstr_open_message

	SetPointer(HourGlass!)
	
	ll_row = dw_claim.GetRow()
	
	ls_administering_act_code = dw_claim.GetItemString(ll_row, 'administering_act_code')
		
   IF ls_administering_act_code = 'FCA' THEN
		lstr_open_message.as_stringparm[1] = 'F'
		lstr_open_message.al_doubleparm[1] = 0
		lstr_open_message.al_doubleparm[2] = 0		
	ELSE
		lstr_open_message.as_stringparm[1] = 'P'
		lstr_open_message.al_doubleparm[1] = 0
		lstr_open_message.al_doubleparm[2] = 0
	END IF
	OpenWithParm(w_employer_search, lstr_open_message)
	
	lstr_return_message = Message.PowerObjectParm

	IF lstr_return_message.al_doubleparm[1] > 0 THEN
		dw_claim.SetItem(ll_row, 'pen_cost_alloc_no', lstr_return_message.al_doubleparm[1])
		dw_claim.SetItem(ll_row, 'pen_cost_alloc_operation_no', lstr_return_message.al_doubleparm[2])
		dw_claim.SetItem(ll_row,'pen_cost_alloc_type_code', lstr_return_message.as_stringparm[3])
		dw_claim.SetItem(ll_row,'pen_emp_name',lstr_return_message.as_stringparm[1])
		dw_claim.SetItem(ll_row,'pen_oper_name',lstr_return_message.as_stringparm[2])

		cb_save.enabled = TRUE
		cb_cancel.enabled = TRUE
		cb_participant.enabled = FALSE
		cb_rx_term.Enabled = FALSE
		cb_rtw.enabled = FALSE
	END IF
	dw_claim.SetColumn('pen_cost_alloc_no')
	dw_claim.SetFocus()
Return


end event

type dw_pension from u_dw_online within tab_pension
integer x = 23
integer y = 32
integer width = 1591
integer height = 700
integer taborder = 11
string dataobject = "d_claim_maintain_pension"
boolean border = false
end type

event itemchanged;call super::itemchanged;String ls_accident
Datetime ldt_accident_date, ldt_orig_accident_date
Date	ldt_override_term
Long 	ll_row, ll_return
Boolean 	lb_eligible


dw_pension.uf_set_pbmessage(TRUE)
ll_return = inv_controller.nf_change_item(10)

IF ll_return = 1 THEN
	cb_save.Enabled = FALSE
	cb_rtw.enabled = TRUE
	lb_eligible = FALSE
ELSE
	cb_save.enabled = TRUE
	cb_cancel.enabled = TRUE
	cb_participant.enabled = FALSE
	cb_rtw.enabled = FALSE
END IF

Return ll_return
end event

type tab_openings from userobject within tabpages_claim
integer x = 18
integer y = 108
integer width = 2619
integer height = 772
long backcolor = 67108864
string text = "Openings"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_openings dw_openings
dw_opening_details dw_opening_details
end type

on tab_openings.create
this.dw_openings=create dw_openings
this.dw_opening_details=create dw_opening_details
this.Control[]={this.dw_openings,&
this.dw_opening_details}
end on

on tab_openings.destroy
destroy(this.dw_openings)
destroy(this.dw_opening_details)
end on

event destructor;
	cb_add.visible = FALSE
	cb_delete.visible = FALSE


end event

type dw_openings from u_dw_online within tab_openings
integer y = 4
integer width = 2619
integer height = 216
integer taborder = 11
string dataobject = "d_openings"
boolean vscrollbar = true
boolean livescroll = false
borderstyle borderstyle = stylelowered!
end type

event clicked;call super::clicked;This.TriggerEvent('ue_more_details')
end event

event doubleclicked;call super::doubleclicked;This.TriggerEvent('ue_more_details')
end event

event itemchanged;call super::itemchanged;dw_openings.uf_set_pbmessage(TRUE)
inv_controller.nf_change_item(3)
cb_save.enabled = TRUE
cb_cancel.enabled = TRUE
cb_participant.enabled = FALSE
cb_rx_term.Enabled = FALSE
cb_rtw.enabled = FALSE

end event

event losefocus;call super::losefocus;/* this is because a click inside the same DW causing lose focus to frame and successful accepttext on bad data. */
String lsClass
GraphicObject lgoFocus

lgoFocus = GetFocus()
If IsValid(lgoFocus) Then
	lsClass = ClassName(lgoFocus)
End If
If IsNull(lsClass) Then
	lsClass = ''
End If

end event

event rbuttondown;call super::rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup
/*	
	create the menu
*/


	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	
	lm_popup.m_options.m_sort.visible = TRUE
	lm_popup.m_options.m_moredetails.visible = TRUE
	lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))
	Destroy lm_popup
end event

event retrieveend;call super::retrieveend;IF THIS.RowCount() < 1 THEN
   cb_delete.enabled = FALSE
END IF
THIS.TriggerEvent(RowFocusChanged!)
end event

event rowfocuschanged;call super::rowfocuschanged;LONG	ll_opening_row
BOOLEAN	lb_openings_found
DATE	ldt_accident_recurrence_date

	ll_opening_row = dw_openings.GetRow()
	IF ll_opening_row < 1 THEN
		lb_openings_found = false
	ELSE
		lb_openings_found = true
	END IF
	
	dw_openings.SelectRow(0,FALSE)
	IF ll_opening_row < 1 THEN
		ll_opening_row = 1
	END IF
	dw_openings.SelectRow(0,FALSE)
	dw_openings.SelectRow(ll_opening_row,TRUE)
	dw_opening_details.ScrollToRow(ll_opening_row)
	dw_opening_details.SetTabOrder('three_day_exempt_code',0) 
	
	
	IF lb_openings_found THEN
		ldt_accident_recurrence_Date = date(dw_openings.GetItemDateTime(ll_opening_row, 'accident_recurrence_date'))
		IF ldt_accident_recurrence_date >= date ("1998/01/01")  AND &
	   	dw_openings.GetItemString(ll_opening_row,'opening_type_code') = "RLOE" AND &
			dw_openings.GetItemString(ll_opening_row, 'recurrence_type_code') = "R" THEN
			dw_opening_details.SetTabOrder('three_day_exempt_code',130) 
		ELSE
			dw_opening_details.SetTabOrder('three_day_exempt_code',0) 
		END IF
	END IF

dw_opening_details.TriggerEvent(RowFocusChanged!)

end event

event ue_more_details;call super::ue_more_details;LONG 	ll_opening_row

	IF dw_openings.RowCount() < 1 THEN
		IF inv_controller.nf_insert(0)  < 0 THEN
			Return
		END IF
	END IF

	ll_opening_row = dw_openings.GetRow()
	IF ll_opening_row < 1 THEN
		ll_opening_row = 1
	END IF
	dw_opening_details.ScrollToRow(ll_opening_row)
	dw_opening_details.visible = TRUE
	dw_opening_details.enabled = TRUE
	dw_opening_details.SetFocus()
	sle_openings.Text = '<Openings Entered>'
	cb_delete.enabled = True

end event

type dw_opening_details from u_dw_online within tab_openings
integer y = 224
integer width = 2619
integer height = 552
integer taborder = 11
string dataobject = "d_openings_details"
boolean border = false
end type

event itemchanged;call super::itemchanged;DATAWINDOWCHILD	ldwc_child
STRING				ls_string, ls_string2, ls_string3, ls_string4, ls_string5, ls_opening_type, ls_comment_req_flag
STRING				ls_opening, ls_three_day_exempt_code, ls_old_three_day_exempt_code, ls_administering_act_code
LONG					ll_row, ll_no, ll_current_row, ll_max, ll_opening_no, ll_payments
LONG					ll_awards, ll_bencalc, ll_rtn
DATE					ldt_date, ldt_null, ld_death, ld_valid, 	ldt_accident_reccurence_date
DATETIME				ldtm_date, ldtm_max, ldtm_deathdate, ldtm_employment, 	ldtm_accident_reccurence_date

dw_opening_details.uf_set_pbmessage(TRUE)

//	the datawindow which triggered the item changed event can be found in
//	the arguement - al_datawindow
//	In the itemchanged event of the window this function is called passing
//	the datawindow corresponding to the array passed in to the nf_init() function

ll_current_row = This.GetRow()
CHOOSE CASE This.GetColumnName()

	CASE 'opening_type_code'
		This.GetChild('work_restriction_flag', ldwc_child)
		ls_string = "opening_type_code = '" + This.GetText() + "'" 
		IF IsNull(ls_string) THEN
			ls_string = ''
		END IF
		ldwc_child.SetFilter(ls_string)
		ldwc_child.Filter()
		This.SetItem(ll_current_row,'work_restriction_flag', ' ')

		This.GetChild('claimant_working_flag', ldwc_child)
		ldwc_child.SetFilter("opening_type_code = ''")
		ldwc_child.Filter()
		This.SetItem(ll_current_row, 'claimant_working_flag' , ' ')

		This.GetChild('receiving_ltd_code', ldwc_child)
		ldwc_child.SetFilter("opening_type_code = ''")
		ldwc_child.Filter()
		This.SetItem(ll_current_row, 'receiving_ltd_code' , ' ')

		This.GetChild('claim_disposition_code', ldwc_child)
		ldwc_child.SetFilter("opening_type_code = ''")
		ldwc_child.Filter()
		This.SetItem(ll_current_row, 'claim_disposition_code' , ' ')
		This.SetItem(ll_current_row, 'disposition_comment', ' ')
		
		/*	PR3917 - J.M. 2004/06/24 - If payments exist for this opening
  		   do not allow the opening_type_code to be changed.				
		*/
		ls_opening  = This.GetItemString(ll_current_row,'opening_type_code',Primary!,TRUE)
		ll_opening_no = dw_openings.GetItemNumber(dw_openings.GetRow(), 'opening_no')
		
		IF ll_opening_no > 0 THEN
			ll_rtn = wf_check_payments(ll_opening_no,il_claim_no) 
			
			IF ll_rtn = -1 THEN
				RETURN -1
			ELSEIF ll_rtn = 1 THEN
				MessageBox('Payments Exist','The opening type cannot be changed because there are payments that exist for this opening.',information!)			
				This.SetItem(ll_current_row,'opening_type_code', ls_opening)
				Return 1
			END IF			
		END IF
				
		// The user may modify the three day exemption code column only under the following conditions:
		//	1. Opening is a RLOE opening
		//	2. "R" Recurrence type 
		//	3. Accident/Recurrence date is on or after January 01, 1998
		ldt_date = date(this.GetItemDateTime(ll_current_row, 'accident_recurrence_date'))
		IF ldt_date >= date ("1998/01/01")  AND &
			This.GetText() = "RLOE" AND &
			This.GetItemString(ll_current_row, 'recurrence_type_code') = "R" THEN
			This.SetTabOrder('three_day_exempt_code',130) 
		ELSE
			This.SetItem(ll_current_row,'three_day_exempt_code','.')
			This.SetTabOrder('three_day_exempt_code',0) 
		END IF
		
	CASE 'accident_recurrence_date'
		ldt_date = Date(Left(This.GetText(),10))
		IF ldt_date < Date(dw_claim.GetItemDateTime(1, 'accident_date')) THEN
			MessageBox('Invalid Recurrence Date', 'The recurrence date cannot be before the original accident date.')
			Return 1
		END IF
		
		ll_opening_no = dw_openings.GetItemNumber(dw_openings.GetRow(), 'opening_no')
		//An accident recurrence date can not be after an associated Job Search RTW Incentive Qualification employment start date.
		Select employment_start_date
		Into    :ldtm_employment
		From  RTW_INCENTIVE_QUALIFICATION
		Where claim_no = :il_claim_no
		And     opening_no = :ll_opening_no
		Using SQLCA;
		
		SQLCA.nf_handle_error("w_claim", "itemchanged", "dw_opening_details - Select employment_start_date From RTW_INCENTIVE_QUALIFCATION")
		
		IF NOT IsNULL(ldtm_employment) AND DATE(ldtm_employment) <> 1900-01-01 THEN
			IF ldt_date > DATE(ldtm_employment) THEN 
				MessageBox('Invalid Recurrence Date', 'The recurrence date cannot be after the employment start date of the associated RTW Incentive qualification.')
				Return 1
			END IF			
		END IF		

		
		IF ldt_date <> Date(dw_claim.GetItemDateTime(1, 'accident_date')) THEN
			IF This.Find("Date(accident_recurrence_date) = Date('" + String(ldt_date,'yyyy/mm/dd') + "') ",0,This.RowCount()) < 1 THEN
				MessageBox('Warning',"You have entered an accident/recurrence date not already in the list.  This should only be done if you are re-opening~r~n" + &
							"the claim for 'recurrence' of injury as it affects the entitlement for benefits.")
				This.SetItem(ll_current_row,'recurrence_type_code', 'R')
			END IF
		END IF
		
		// The user may modify the three day exemption code column only under the following conditions:
		//	1. Opening is a RLOE opening
		//	2. "R" Recurrence type 
		//	3. Accident/Recurrence date is on or after January 01, 1998
		IF ldt_date >= date ("1998/01/01")  AND &
			This.GetItemString(ll_current_row,'opening_type_code') = "RLOE" AND &
			This.GetItemString(ll_current_row, 'recurrence_type_code') = "R" THEN
			This.SetTabOrder('three_day_exempt_code',130) 
		ELSE
			This.SetItem(ll_current_row,'three_day_exempt_code','.')
			This.SetTabOrder('three_day_exempt_code',0) 
		END IF
		
	CASE 'benefit_start_date'
		// greater than accident date
		ls_string = This.GetText()
		IF ls_string <> "" THEN
			ldt_date = Date(Left(ls_string,10))
			IF ldt_date < Date(dw_claim.GetItemDateTime(1,'accident_date')) THEN
				MessageBox('Invalid Start Date', 'The start date cannot be before the accident date.')
				Return 1
			END IF
		END IF

	CASE 'benefit_end_date'
		// after start and accident
		ls_string = This.GetText()
		IF ls_string <> "" THEN
			ldt_date = Date(Left(ls_string,10))
			IF ldt_date < Date(dw_claim.GetItemDateTime(1,'accident_date')) THEN
				MessageBox('Invalid End Date', 'The end date must not be less than the accident date.')
				RETURN 1
			ELSEIF ldt_date < Date(This.GetItemDateTime(ll_current_row,'benefit_start_date')) THEN
				MessageBox('Invalid End Date', 'The end date must not be less than the start date.')
				RETURN 1
			END IF
			
			ll_opening_no = dw_openings.GetItemNumber(dw_openings.GetRow(), 'opening_no')
		    //An Benefit End should be less than or equal to the associated Job Search RTW Incentive Qualification employment start date.
		    Select employment_start_date
		    Into    :ldtm_employment
		    From  RTW_INCENTIVE_QUALIFICATION
     	    Where claim_no = :il_claim_no
		    And     opening_no = :ll_opening_no
		    Using SQLCA;
		
		    SQLCA.nf_handle_error("w_claim", "itemchanged", "dw_opening_details - Select employment_start_date From RTW_INCENTIVE_QUALIFCATION")
		
		   IF NOT IsNULL(ldtm_employment) AND DATE(ldtm_employment) <> 1900-01-01 THEN
			  IF ldt_date > DATE(ldtm_employment) THEN 
				 MessageBox('Warning', 'The benefit end date should be less than or equal to the employment start date of the associated RTW Incentive qualification.')
			  END IF			
		   END IF
			
			
			//	set up the work restriction
			This.GetChild('work_restriction_flag', ldwc_child)
			ls_string = This.GetItemString(ll_current_row,'opening_type_code')
			ls_string = "opening_type_code = '" + ls_string + "'"
			IF IsNull(ls_string) THEN
				ls_string = ''
			END IF
			ldwc_child.SetFilter(ls_string)
			ldwc_child.Filter()
			This.SetItem(ll_current_row, 'work_restriction_flag' , ' ')

			IF dw_claim.GetItemString(1,'claim_status_code') = 'F' AND dw_claim.GetItemString(1,'claim_status_type_code') = '02' THEN
				//	set up the defaults
				This.SetItem(ll_current_row, 'work_restriction_flag' , 'N')
				This.GetChild('claimant_working_flag', ldwc_child)
				ls_string2 = This.GetItemString(ll_current_row,'opening_type_code')
				ls_string = "opening_type_code = '" + ls_string2 + "' and work_restriction_flag = 'N'" 
				IF IsNull(ls_string) THEN
					ls_string = ''
				END IF
				ldwc_child.SetFilter(ls_string)
				ldwc_child.Filter()
				This.SetItem(ll_current_row, 'claimant_working_flag' , 'Y')

				This.GetChild('receiving_ltd_code', ldwc_child)
				ls_string2 = This.GetItemString(ll_current_row,'opening_type_code')
				ls_string = "opening_type_code = '" + ls_string2 + "' and work_restriction_flag = 'N' and working_flag = 'Y'"
				IF IsNull(ls_string) THEN
					ls_string = ''
				END IF
				ldwc_child.SetFilter(ls_string)
				ldwc_child.Filter()
				This.SetItem(ll_current_row, 'receiving_ltd_code' , 'Z')

				This.GetChild('claim_disposition_code', ldwc_child)
				ls_string2 = This.GetItemString(ll_current_row,'opening_type_code')
				ls_string = "opening_type_code = '" + ls_string2 + "' and work_restriction_flag = 'N'" &
				+ " and working_flag = 'Y' and receiving_ltd_code = 'Z'"
				IF IsNull(ls_string) THEN
					ls_string = ''
				END IF
				ldwc_child.SetFilter(ls_string)
				ldwc_child.Filter()
				This.SetItem(ll_current_row, 'claim_disposition_code' , ' ')
				This.SetItem(ll_current_row, 'disposition_comment', ' ')
			ELSE
				This.GetChild('claimant_working_flag', ldwc_child)
				ldwc_child.SetFilter("opening_type_code = ''")
				ldwc_child.Filter()
				This.SetItem(ll_current_row, 'claimant_working_flag' , ' ')

				This.GetChild('receiving_ltd_code', ldwc_child)
				ldwc_child.SetFilter("opening_type_code = ''")
				ldwc_child.Filter()
				This.SetItem(ll_current_row, 'receiving_ltd_code' , ' ')

				This.GetChild('claim_disposition_code', ldwc_child)
				ldwc_child.SetFilter("opening_type_code = ''")
				ldwc_child.Filter()
				This.SetItem(ll_current_row, 'claim_disposition_code' , ' ')
				This.SetItem(ll_current_row,'disposition_comment',' ')			
			END IF
		ELSE
			This.GetChild('claimant_working_flag', ldwc_child)
			ldwc_child.SetFilter("opening_type_code = ''")
			ldwc_child.Filter()
			This.SetItem(ll_current_row, 'claimant_working_flag' , ' ')

			This.GetChild('receiving_ltd_code', ldwc_child)
			ldwc_child.SetFilter("opening_type_code = ''")
			ldwc_child.Filter()
			This.SetItem(ll_current_row, 'receiving_ltd_code' , ' ')
			This.GetChild('claim_disposition_code', ldwc_child)
			ldwc_child.SetFilter("opening_type_code = ''")
			ldwc_child.Filter()
			This.SetItem(ll_current_row, 'claim_disposition_code' , ' ')
			This.SetItem(ll_current_row,'disposition_comment',' ')
			
			SetNull(ldt_null)
			IF dw_openings.GetRow() > 0 THEN
				dw_openings.SetItem(dw_openings.GetRow(), 'opening_disposition_code_entry_date', ldt_null)
			END IF
		END IF

	CASE 'work_restriction_flag'
			ls_string = This.GetText()
			This.GetChild('claimant_working_flag', ldwc_child)
			ls_string2 = This.GetItemString(ll_current_row,'opening_type_code')
			ls_string = "opening_type_code = '" + ls_string2 + "' and work_restriction_flag = '" + ls_string + "'"
			IF IsNull(ls_string) THEN
				ls_string = ''
			END IF
			ldwc_child.SetFilter(ls_string)
			ldwc_child.Filter()
			This.SetItem(ll_current_row,'claimant_working_flag', ' ')
			This.GetChild('receiving_ltd_code', ldwc_child)
			ldwc_child.SetFilter("opening_type_code = ''")
			ldwc_child.Filter()
			This.SetItem(ll_current_row, 'receiving_ltd_code' , ' ')

			This.GetChild('claim_disposition_code', ldwc_child)
			ldwc_child.SetFilter("opening_type_code = ''")
			ldwc_child.Filter()
			This.SetItem(ll_current_row, 'claim_disposition_code' , ' ')
			This.SetItem(ll_current_row,'disposition_comment',' ')
			This.SetItem(ll_current_row, 'comment_required_flag' , ' ')

	CASE 'claimant_working_flag'
			// set up the ltd
			ls_string = This.GetText()
			This.GetChild('receiving_ltd_code', ldwc_child)
			ls_string2 = This.GetItemString(ll_current_row,'opening_type_code')
			ls_string3 = This.GetItemString(ll_current_row,'work_restriction_flag')
			ls_string = "opening_type_code = '" + ls_string2 + "' and work_restriction_flag = '" + ls_string3 + "'" &
			+ " and working_flag = '" + ls_string + "'"
			IF IsNull(ls_string) THEN
				ls_string = ''
			END IF
			ldwc_child.SetFilter(ls_string)
			ldwc_child.Filter()
			This.SetItem(ll_current_row, 'receiving_ltd_code' , ' ')

			This.GetChild('claim_disposition_code', ldwc_child)
			ldwc_child.SetFilter("opening_type_code = ''")
			ldwc_child.Filter()
			This.SetItem(ll_current_row, 'claim_disposition_code' , ' ')
			This.SetItem(ll_current_row,'disposition_comment',' ')
			This.SetItem(ll_current_row, 'comment_required_flag' , ' ')


	CASE 'receiving_ltd_code'
			//	get the values to determine what disposition codes are allowed
			ls_string = This.GetText()
			This.GetChild('claim_disposition_code', ldwc_child)
			ls_string2 = This.GetItemString(ll_current_row,'opening_type_code')
			ls_string3 = This.GetItemString(ll_current_row,'work_restriction_flag')
			ls_string4 = This.GetItemString(ll_current_row,'claimant_working_flag')
			ls_string = "opening_type_code = '" + ls_string2 + "' and work_restriction_flag = '" + ls_string3 + "'" &
			+ " and working_flag = '" + ls_string4 + "' and receiving_ltd_code = '" + ls_string + "'"
			IF IsNull(ls_string) THEN
				ls_string = ''
			END IF
			ldwc_child.SetFilter(ls_string)
			ldwc_child.Filter()
			This.SetItem(ll_current_row, 'claim_disposition_code' , ' ')
			This.SetItem(ll_current_row,'disposition_comment',' ')
			This.SetItem(ll_current_row, 'comment_required_flag' , ' ')
			
			

/* PR1060 */
	CASE 'claim_disposition_code'
			//	get the flag to determine if the disposition comment should be visible
			ls_string = This.GetText()
	
			SELECT comment_required_flag
			INTO :ls_comment_req_flag
			FROM Claim_Disposition_Type
			WHERE claim_disposition_code = :ls_string
			USING SQLCA;

			IF SQLCA.nf_handle_error('SELECT from Claim_Disposition_Type','dw_opening_details','itemchanged')<0 THEN
				Return -1
			END IF
			This.SetItem(ll_current_row,'comment_required_flag',ls_comment_req_flag)
			This.SetItem(ll_current_row,'disposition_comment','')
			/* PR 1458 */
			IF dw_openings.GetRow() > 0 THEN
				dw_openings.SetItem(dw_openings.GetRow(),'opening_disposition_code_entry_date',Today())
			END IF

/* PR1060 */
	CASE 'disposition_comment'
			//	put the disposition comment into the primary buffer
			ls_string = This.GetText()
			IF IsNull(ls_string) THEN
				ls_string = ''
			END IF
			This.SetItem(ll_current_row,'disposition_comment',ls_string)

	CASE 'review_12_week_date'
			ls_string = This.GetItemString(ll_current_row,'opening_type_code')
			IF ls_string <> 'RLOE' AND Trim(This.GetText()) <> '' THEN
				MessageBox('12 Week Review Warning','This is only applicable for Openings of type RLOE')
				SetNull(ldtm_date )
				This.SetItem(ll_current_row,'review_12_week_date', ldtm_date)
			END IF
	
	CASE 'recurrence_type_code'
			ls_string = This.GetText()
			IF ls_string = 'A' and This.GetItemString(ll_current_row,'recurrence_type_code') = 'R' THEN
				MessageBox('Warning', 'The recurrence type has changed.  This should only be done if there is a change in benefit level/entitlement required. Ensure benefits are recalculated and retroactive payments are issued.')
			END IF

			ll_row = 1
			ll_max = dw_openings.RowCount()
			ldtm_date = DateTime(Date('1900/01/01'))
			ldtm_max = This.GetItemDateTime(ll_current_row,'accident_recurrence_date')
			DO WHILE ll_row <= ll_max
				IF ll_row <> ll_current_row THEN
					IF dw_openings.GetItemDateTime(ll_row,'accident_recurrence_date') > ldtm_date AND &
						dw_openings.GetItemString(ll_row,'opening_type_code') = &
						This.GetItemString(ll_current_row,'opening_type_code') AND &
						dw_openings.GetItemDateTime(ll_row,'accident_recurrence_date') < ldtm_max AND &
						dw_openings.GetItemString(ll_row,'recurrence_type_code') = 'R' THEN
						//	get the maximum recurrence date that is less than the current accident_recurrence_date
						ldtm_date = dw_openings.GetItemDateTime(ll_row,'accident_recurrence_date')
					END IF
				END IF
				ll_row = ll_row + 1
			LOOP
			This.SetItem(ll_current_row,'accident_recurrence_date',ldtm_date)
	
		// The user may modify the three day exemption code column only under the following conditions:
		//	1. Opening is a RLOE opening
		//	2. "R" Recurrence type 
		//	3. Accident/Recurrence date is on or after January 01, 1998
		ldt_date = date(this.GetItemDateTime(ll_current_row, 'accident_recurrence_date'))
		IF ldt_date >= date ("1998/01/01")  AND &
			This.GetItemString(ll_current_row,'opening_type_code') = "RLOE" AND &
			This.GetText() =  "R" THEN
			This.SetTabOrder('three_day_exempt_code',130) 
		ELSE
			This.SetItem(ll_current_row,'three_day_exempt_code','.')
			This.SetTabOrder('three_day_exempt_code',0) 
		END IF
			
	CASE 'three_day_exempt_code'
		ls_three_day_exempt_code = This.GetText()
		ls_old_three_day_exempt_code = This.GetItemString(ll_current_row,'three_day_exempt_code')
		IF ls_three_day_exempt_code = 'D' THEN
			ls_administering_act_code = dw_claim.GetItemString(1, 'administering_act_code')
		
			IF ls_administering_act_code <> 'FCA' THEN
				MessageBox('Error ' , 'Only Firefighter Compensation Act injured workers can have "Disabled more than 20 days" as a 3 day exemption.')
				this.SetItem(ll_current_row, 'three_day_exempt_code', ls_old_three_day_exempt_code)
				RETURN 2
			END IF
		END IF
	
		MessageBox('Warning', 'Changing the three day waive reason may affect the requirements~r~n' + &
					'for the deduction and reimbursement of the three day payment', Information!)

END CHOOSE

cb_save.enabled = TRUE
cb_cancel.enabled = TRUE
cb_participant.enabled = FALSE
cb_rtw.enabled = FALSE

end event

event losefocus;call super::losefocus;/* this is because a click inside the same DW causing lose focus to frame and successful accepttext on bad data. */
String lsClass
GraphicObject lgoFocus

lgoFocus = GetFocus()
If IsValid(lgoFocus) Then
	lsClass = ClassName(lgoFocus)
End If
If IsNull(lsClass) Then
	lsClass = ''
End If

end event

event rowfocuschanged;call super::rowfocuschanged;DATAWINDOWCHILD	ldwc_child
STRING				ls_string, ls_string2, ls_string3, ls_string4, ls_string5, ls_value
LONG					ll_current_row
DWITEMSTATUS		ldwis_original_status
	ll_current_row = This.GetRow()
	IF ll_current_row > 0 THEN
		This.SetRedraw(FALSE)
	 	ldwis_original_status = dw_openings.GetItemStatus(ll_current_row,0,Primary!) 
		IF NOT IsNull(This.GetItemDateTime(ll_current_row,'benefit_end_date')) THEN
			ls_value = This.GetItemString(ll_current_row,'work_restriction_flag')
	      This.GetChild('work_restriction_flag', ldwc_child)
			ls_string2 = This.GetItemString(ll_current_row,'opening_type_code')
			ls_string = "opening_type_code = '" + ls_string2 + "' "
 			ldwc_child.SetFilter(ls_string)
			ldwc_child.Filter()
			This.SetItem(ll_current_row,'work_restriction_flag', ls_value)

			ls_value = This.GetItemString(ll_current_row,'claimant_working_flag')
  	      This.GetChild('claimant_working_flag', ldwc_child)
			ls_string3 = This.GetItemString(ll_current_row,'work_restriction_flag')
			ls_string = "opening_type_code = '" + ls_string2 + "' and work_restriction_flag = '" +ls_string3 + "'"
			IF IsNull(ls_string) THEN
				ls_string = ''
			END IF
			ldwc_child.SetFilter(ls_string)
			ldwc_child.Filter()
			This.SetItem(ll_current_row,'claimant_working_flag', ls_value)

	      ls_value = This.GetItemString(ll_current_row,'receiving_ltd_code')
         This.GetChild('receiving_ltd_code', ldwc_child)
			ls_string4 = This.GetItemString(ll_current_row,'claimant_working_flag')
			ls_string = "opening_type_code = '" + ls_string2 + "' and work_restriction_flag = '" +ls_string3 + &
			"' and working_flag = '" +ls_string4 + "'"
			IF IsNull(ls_string) THEN
				ls_string = ''
			END IF
			ldwc_child.SetFilter(ls_string)
			ldwc_child.Filter()
			This.SetItem(ll_current_row,'receiving_ltd_code', ls_value)

			ls_value = This.GetItemString(ll_current_row,'claim_disposition_code')
         This.GetChild('claim_disposition_code', ldwc_child)
			ls_string5 = This.GetItemString(ll_current_row,'receiving_ltd_code')
			ls_string = "opening_type_code = '" + ls_string2 + "' and work_restriction_flag = '" +ls_string3 + &
			"' and working_flag = '" +ls_string4 + "' and receiving_ltd_code = '" + ls_string5 + "'"
			IF IsNull(ls_string) THEN
				ls_string = ''
			END IF
			ldwc_child.SetFilter(ls_string)
			ldwc_child.Filter()
			This.SetItem(ll_current_row, 'claim_disposition_code', ls_value)
		ELSE
	         This.GetChild('work_restriction_flag', ldwc_child)
 				ldwc_child.SetFilter("opening_type_code = ''")
				ldwc_child.Filter()
				This.SetItem(ll_current_row, 'work_restriction_flag' , ' ')

   	      This.GetChild('claimant_working_flag', ldwc_child)
				ldwc_child.SetFilter("opening_type_code = ''")
				ldwc_child.Filter()
				This.SetItem(ll_current_row, 'claimant_working_flag' , ' ')

	         This.GetChild('receiving_ltd_code', ldwc_child)
				ldwc_child.SetFilter("opening_type_code = ''")
				ldwc_child.Filter()
				This.SetItem(ll_current_row, 'receiving_ltd_code' , ' ')

	         This.GetChild('claim_disposition_code', ldwc_child)
				ldwc_child.SetFilter("opening_type_code = ''")
				ldwc_child.Filter()
				This.SetItem(ll_current_row, 'claim_disposition_code' , ' ')
		END IF
/*		this setting of the status is necessary because the status of the opening dw was getting changed with
		the setitems even though they were set back to the original values.  This was causing an error message to 
		be broadcast to the user when the save was hit.  It said the opening could not be changed even though the 
		user did not make a change
*/
		IF ldwis_original_status <> dw_openings.GetItemStatus(ll_current_row,0,Primary!) THEN
			dw_openings.SetItemStatus(ll_current_row,0,Primary!,ldwis_original_status)
		END IF

		dw_openings.SelectRow(ll_current_row,TRUE)
		dw_openings.ScrollToRow(ll_current_row)

		This.SetRedraw(TRUE)
	END IF
	This.SetFocus()
	This.SetColumn('opening_type_code')
end event

type tab_reminders from userobject within tabpages_claim
integer x = 18
integer y = 108
integer width = 2619
integer height = 772
long backcolor = 67108864
string text = "Reminders"
long tabtextcolor = 33554432
long picturemaskcolor = 553648127
dw_claim_reminder_details dw_claim_reminder_details
dw_claim_reminder dw_claim_reminder
end type

on tab_reminders.create
this.dw_claim_reminder_details=create dw_claim_reminder_details
this.dw_claim_reminder=create dw_claim_reminder
this.Control[]={this.dw_claim_reminder_details,&
this.dw_claim_reminder}
end on

on tab_reminders.destroy
destroy(this.dw_claim_reminder_details)
destroy(this.dw_claim_reminder)
end on

type dw_claim_reminder_details from u_dw_online within tab_reminders
integer x = 5
integer y = 376
integer width = 2615
integer height = 492
integer taborder = 11
string dataobject = "d_claim_reminder_details"
boolean minbox = true
boolean border = false
end type

event itemchanged;call super::itemchanged;Long ll_row
String  ls_string, ls_reminder_status_old, ls_claim_status_code,ls_cppd_status
Date ldt_current_date, ldt_reminder_closed_date


dw_claim_reminder_details.uf_set_pbmessage(TRUE)
ldt_current_date = DATE(f_server_datetime())

cb_cancel.enabled = TRUE
cb_participant.enabled = FALSE
cb_rtw.enabled = FALSE

ll_row = This.GetRow()
CHOOSE CASE This.GetColumnName()

	CASE 'reminder_status_code'
			ls_string = Trim(This.GetText())
			ls_reminder_status_old = dw_claim_reminder.getitemstring(row,"reminder_status_code")
			ldt_reminder_closed_date = Date(dw_claim_reminder.getitemdatetime(row,"closed_date"))
			ls_claim_status_code = dw_claim.getitemstring(dw_claim.getrow(),"claim_status_code")
			ls_cppd_status = dw_claim.getitemstring(dw_claim.getrow(),"cppd_status_code")
			
			// A cancelled or completed reminder may be changed to a different status if the change occurs on the reminder closed date. And the CPPD disability status 
			// is not receiving CPPD
			IF ls_cppd_status = 'R' THEN //Receiving CPPD cannot change the status
				Messagebox('ERROR','The reminder status can not change when Receiving CPP Disablity.',Information!) 
				This.SetItem(ll_row,'reminder_status_code',ls_reminder_status_old)
				Return 1
			END IF	
			
			IF ls_reminder_status_old = 'C' AND ls_string <> 'C' THEN
				IF ldt_reminder_closed_date <> ldt_current_date THEN
					MessageBox('Error on Reminder Status Code','The Completed reminder status can only be changed on the Reminder Closed Date.',Exclamation!,ok!)
					This.SetItem(ll_row,'reminder_status_code',ls_reminder_status_old)
					Return 1
				ELSEIF ls_string = 'P' THEN
					IF ls_claim_status_code = 'A' Then
						SetNull(ldt_current_date)
						This.SetItem(ll_row,'closed_date',ldt_current_date)	
					ELSE
						MessageBox('Error on Reminder Status Code','The Completed reminder status can only be changed to a planned status on the Reminder Closed Date of an Active claim.',Exclamation!,ok!)
						This.SetItem(ll_row,'reminder_status_code',ls_string)
						Return 1	
					END IF	
				END IF
			END IF

			IF ls_reminder_status_old = 'X' AND ls_string <> 'X' THEN
				IF ldt_reminder_closed_date <> ldt_current_date THEN
					MessageBox('Error on Reminder Status Code','The Cancelled reminder status can only be changed on the Reminder Closed Date.',Exclamation!,ok!)
					This.SetItem(ll_row,'reminder_status_code',ls_reminder_status_old)
					Return 1
				ELSEIF ls_string = 'P' THEN
					IF ls_claim_status_code = 'A' Then
						SetNull(ldt_current_date)
						This.SetItem(ll_row,'closed_date',ldt_current_date)	
					ELSE
						MessageBox('Error on Reminder Status Code','The Cancelled reminder status can only be changed to a planned status on the Reminder Closed Date of an Active claim.',Exclamation!,ok!)
						This.SetItem(ll_row,'reminder_status_code',ls_reminder_status_old)
						Return 1	
					END IF	
				END IF
			END IF

			// A cancelled or completed reminder must be closed with the current date.
			IF ls_string = 'C' OR ls_string = 'X' THEN
				This.SetItem(ll_row,'closed_date',ldt_current_date)
			END IF	
			
			This.SetItem(ll_row,'reminder_status_code',ls_string)

END CHOOSE

cb_save.enabled = TRUE

end event

event editchanged;call super::editchanged;cb_cancel.enabled = True
end event

event rowfocuschanged;call super::rowfocuschanged;
STRING				ls_string, ls_value
LONG					ll_current_row
DWITEMSTATUS		ldwis_original_status

	
ll_current_row = This.GetRow()
IF ll_current_row > 0 THEN
		This.SetRedraw(FALSE)
	 	ldwis_original_status = dw_claim_reminder.GetItemStatus(ll_current_row,0,Primary!) 
		ls_value = This.GetItemString(ll_current_row,'reminder_status_code')
		This.SetItem(ll_current_row,'reminder_status_code', ls_value)

		IF ldwis_original_status <> dw_claim_reminder.GetItemStatus(ll_current_row,0,Primary!) THEN
			dw_claim_reminder.SetItemStatus(ll_current_row,0,Primary!,ldwis_original_status)
		END IF

		This.SetRedraw(TRUE)
END IF
This.SetFocus()

end event

event ue_post_losefocus;call super::ue_post_losefocus;	IF This.AcceptText() < 0 THEN
		This.SetFocus()
	END IF
end event

event losefocus;call super::losefocus;String lsClass
GraphicObject lgoFocus

lgoFocus = GetFocus()
If IsValid(lgoFocus) Then
	lsClass = ClassName(lgoFocus)
End If
If IsNull(lsClass) Then
	lsClass = ''
End If
end event

type dw_claim_reminder from u_dw_online within tab_reminders
integer x = 5
integer y = 4
integer width = 2606
integer height = 372
integer taborder = 11
string dataobject = "d_claim_reminder"
boolean vscrollbar = true
boolean livescroll = false
borderstyle borderstyle = stylelowered!
end type

event clicked;call super::clicked;This.TriggerEvent('ue_more_details')
end event

event buttonclicked;call super::buttonclicked;string		ls_filter
LONG			ll_begin_rowcount
LONG			ll_end_rowcount

ll_begin_rowcount = this.rowcount()

ll_end_rowcount = this.RowCount()

IF ll_end_rowcount <> ll_begin_rowcount THen
	this.SelectRow(0,False)
	this.SelectRow(1,True)
End If
end event

event rowfocuschanged;call super::rowfocuschanged;LONG	ll_row

ll_row = dw_claim_reminder.GetRow()
	
dw_claim_reminder.SelectRow(0,FALSE)
IF ll_row < 1 THEN
	ll_row = 1
END IF
dw_claim_reminder.SelectRow(0,FALSE)
dw_claim_reminder.SelectRow(ll_row,TRUE)
dw_claim_reminder_details.ScrollToRow(ll_row)
	

dw_claim_reminder_details.TriggerEvent(RowFocusChanged!)



end event

event rowfocuschanging;call super::rowfocuschanging;//IF currentrow > 0 THEN
//   THIS.SelectRow(currentrow,FALSE)
//END IF 
end event

event ue_more_details;call super::ue_more_details;LONG 	ll_row

ll_row = dw_claim_reminder.GetRow()
IF ll_row < 1 THEN 
	MessageBox("Information","There are no reminders to view.",Information!,ok!)
	Return
END IF	

dw_claim_reminder_details.ScrollToRow(ll_row)
dw_claim_reminder_details.visible = TRUE
dw_claim_reminder_details.enabled = TRUE
dw_claim_reminder_details.SetFocus()

end event

event itemchanged;call super::itemchanged;dw_claim_reminder.uf_set_pbmessage(TRUE)
cb_save.enabled = TRUE
cb_cancel.enabled = TRUE
cb_participant.enabled = FALSE
cb_rx_term.Enabled = FALSE
cb_rtw.enabled = FALSE

end event

event losefocus;call super::losefocus;String lsClass
GraphicObject lgoFocus

lgoFocus = GetFocus()
If IsValid(lgoFocus) Then
	lsClass = ClassName(lgoFocus)
End If
If IsNull(lsClass) Then
	lsClass = ''
End If
end event

event retrieveend;call super::retrieveend;IF THIS.RowCount() < 1 THEN
   cb_delete.enabled = FALSE
END IF
THIS.TriggerEvent(RowFocusChanged!)
end event

event ue_post_losefocus;call super::ue_post_losefocus;	IF This.AcceptText() < 0 THEN
		This.SetFocus()
	END IF
end event

type dw_override_main from u_dw_online within w_claim
integer x = 18
integer y = 740
integer width = 923
integer height = 80
integer taborder = 0
string dataobject = "d_override_termination"
boolean border = false
end type

type st_open_ended from statictext within w_claim
boolean visible = false
integer x = 617
integer y = 744
integer width = 325
integer height = 72
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Open Ended"
boolean focusrectangle = false
end type

type dw_claim from u_dw_online within w_claim
integer y = 68
integer width = 2661
integer height = 760
integer taborder = 10
string dataobject = "d_claim_maintain"
boolean border = false
end type

event itemchanged;call super::itemchanged;Long     ll_return, ll_count, ll_row, ll_rtn
Integer  li_rtn
Datetime ldt_accident_date, ldt_orig_accident_date, ldt_eligibility_end_date
STRING   ls_accident, ls_claim_status_code, ls_claim_status_type
BOOLEAN	lb_eligible, lb_death_date
DATE		ldt_override_term



dw_claim.uf_set_pbmessage(TRUE)
ll_return = inv_controller.nf_change_item(1)

IF ll_return = 1 THEN
	cb_save.Enabled = FALSE
	cb_rtw.enabled = TRUE
	lb_eligible = FALSE
ELSE	
 IF dwo.name = "claim_status_code" OR dwo.name = "claim_status_type_code" THEN
	ib_statuschanged = TRUE
	
	IF dwo.name = "claim_status_code" then 
		ls_claim_status_code = data
		ls_claim_status_type = dw_claim.GetItemString(dw_claim.GetRow(), 'claim_status_type_code')
	ELSE
		ls_claim_status_type = data
		ls_claim_status_code = dw_claim.GetItemString(dw_claim.GetRow(), 'claim_status_code') 
	END IF
	
	IF ls_claim_status_code = 'A' OR (ls_claim_status_code = 'F' AND ls_claim_status_type = '01') THEN
		wf_set_rtw(ls_claim_status_code,ls_claim_status_type)
	End If	
	
	
	//Check to see if the Claim is registered
	IF inv_bc.uf_is_registered(il_claim_no) = 1 THEN  //Claim is registered
		dw_override_main.object.eligibility_end_date_t.Visible = FALSE
		dw_override_main.object.eligibility_end_date.Visible = FALSE
		cb_rx_term.Visible = FALSE
		st_open_ended.Visible = FALSE
	ELSE //Claim is not registered
		// Check to see if claim already has an override
		
		dw_override_main.Retrieve(il_claim_no)
		ll_row = dw_override_main.GetRow()
		IF ll_row > 0 THEN
			ldt_override_term = DATE(dw_override_main.GetItemDateTime(dw_override_main.GetRow(), 'eligibility_end_date'))
		END IF
		
		// BR 10.20 An overrride term date can only be entered when the claim is eligible & is not deceased.
		lb_death_date = inv_bc.uf_check_for_death_date(il_claim_no)
		IF lb_death_date = TRUE THEN
			lb_eligible = FALSE
		ELSE	
			//Check claim status to see if the claim is now eligible to be registered.
			lb_eligible = inv_bc.uf_check_for_eligibility(il_claim_no,ls_claim_status_code,ls_claim_status_type)
		END IF
		
		IF lb_eligible = TRUE THEN
			MessageBox("Information",'The claim will be registered during the next eligiblity export process.',Information!,OK!)
		ELSE //The claim is not eligible for coverage
			lb_eligible = FALSE
			
			SELECT COUNT(*)
			INTO   :ll_count
			FROM   X001_OVERRIDE_ELIGIBILITY
			WHERE  claim_no = :il_claim_no
			USING  SQLCA;
			
			SQLCA.nf_handle_error('w_claim', 'dw_claim', 'itemchanged - SELECT COUNT(*) FROM X001_OVERRIDE_ELIGIBILITY')
			
			IF ll_count > 0 THEN
				MessageBox('WARNING','The Claim is no longer eligible to be registered, the Override Termination Date will be removed.', &
				Information!,OK!) 
				ib_remove_override = TRUE
				dw_override_main.object.eligibility_end_date.Visible = 0
				st_open_ended.Visible = False
			END IF
		END IF
	END IF
 ELSE
	lb_eligible = cb_rx_term.Enabled
 END IF
 

cb_save.Enabled = TRUE
cb_rtw.enabled = FALSE

END IF
cb_cancel.Enabled = TRUE
cb_participant.Enabled = FALSE
IF lb_eligible = TRUE THEN
	cb_rx_term.Enabled = TRUE
ELSE	
	cb_rx_term.Enabled = FALSE
END IF	

RETURN ll_return

end event

event losefocus;call super::losefocus;/* this is because a click inside the same DW causing lose focus to frame and successful accepttext on bad data. */
String lsClass

GraphicObject lgoFocus

lgoFocus = GetFocus()
If IsValid(lgoFocus) Then
	lsClass = ClassName(lgoFocus)
End If
If IsNull(lsClass) Then
	lsClass = ''
End If
If lsClass = 'w_frame' Or Parent.WindowState = Minimized! Then 
	Return 1
End If
end event

type dw_eligible_report_fee from u_dw_online within w_claim
boolean visible = false
integer x = 32
integer y = 1676
integer width = 59
integer height = 120
integer taborder = 0
string dataobject = "d_eligible_report_fee_payments"
end type

type dw_firefighter_claim from u_dw_online within w_claim
boolean visible = false
integer x = 613
integer y = 1852
integer width = 59
integer height = 120
integer taborder = 11
string dataobject = "ds_new_firefighter_claim"
end type

type dw_firefighter_claim_participant from u_dw_online within w_claim
boolean visible = false
integer x = 695
integer y = 1852
integer width = 59
integer height = 120
integer taborder = 11
boolean bringtotop = true
string dataobject = "ds_new_firefighter_claim_participant"
end type

type dw_firefighter_claim_accident from u_dw_online within w_claim
boolean visible = false
integer x = 777
integer y = 1852
integer width = 59
integer height = 120
integer taborder = 11
boolean bringtotop = true
string dataobject = "ds_new_firefighter_claim_accident"
end type

type dw_rejected_wca_claim_events from u_dw_online within w_claim
boolean visible = false
integer x = 859
integer y = 1852
integer width = 59
integer height = 120
integer taborder = 11
boolean bringtotop = true
string dataobject = "ds_rejected_wca_claim_events"
end type

type dw_rejected_wca_difficult_claim from u_dw_online within w_claim
boolean visible = false
integer x = 942
integer y = 1852
integer width = 59
integer height = 120
integer taborder = 11
boolean bringtotop = true
string dataobject = "ds_rejected_WCA_difficult_claim"
end type

type cb_create_firefighter_claim from commandbutton within w_claim
integer x = 1746
integer y = 1628
integer width = 878
integer height = 80
integer taborder = 21
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Create Firefighter Claim"
end type

event clicked;INTEGER   li_msg
LONG      ll_individual_no, ll_new_FCA_claim_no

li_msg = MessageBox('Firefighter Claim?',"Are you sure you want to create a Firefighters' Compensation Act claim?",Question!,YesNo!,2)
IF li_msg = 1 THEN
	ll_individual_no = dw_claim.GetItemNumber(1,'individual_no')
	inv_controller.nf_create_firefighter(il_claim_no,ll_individual_no,ll_new_FCA_claim_no)
	
	MessageBox('New FCA Claim','The new firefighter claim ('+String(ll_new_FCA_claim_no)+') was created successfully.')
	THIS.visible = FALSE
END IF


end event

