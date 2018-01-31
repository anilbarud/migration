$PBExportHeader$w_add_participant_individual.srw
$PBExportComments$response window to maintain claim partic. which allows for edit of claim role and/or individual
forward
global type w_add_participant_individual from w_ancestor
end type
type cb_history from commandbutton within w_add_participant_individual
end type
type dw_participant from u_dw_online within w_add_participant_individual
end type
type cb_search from commandbutton within w_add_participant_individual
end type
type cb_ok from commandbutton within w_add_participant_individual
end type
type cb_cancel from commandbutton within w_add_participant_individual
end type
type dw_main_name from u_dw_online within w_add_participant_individual
end type
type cb_delete from commandbutton within w_add_participant_individual
end type
type dw_individual from u_dw_online within w_add_participant_individual
end type
type dw_claim_list from u_dw_online within w_add_participant_individual
end type
type dw_deleted_individual from u_dw_online within w_add_participant_individual
end type
type dw_deleted_name from u_dw_online within w_add_participant_individual
end type
type dw_injury from u_dw_online within w_add_participant_individual
end type
type dw_annuity_eligibility from u_dw_online within w_add_participant_individual
end type
type uo_sin_med_check from u_check_sin_med within w_add_participant_individual
end type
type dw_claim_individual from u_dw_online within w_add_participant_individual
end type
type dw_next_individual_no from u_dw_online within w_add_participant_individual
end type
type dw_name from u_dw_online within w_add_participant_individual
end type
type dw_participant_list from u_dw_online within w_add_participant_individual
end type
end forward

global type w_add_participant_individual from w_ancestor
integer x = 928
integer y = 388
integer width = 2683
integer height = 2228
string title = "Claim Role/Individual Maintenance"
string menuname = ""
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
long backcolor = 67108864
cb_history cb_history
dw_participant dw_participant
cb_search cb_search
cb_ok cb_ok
cb_cancel cb_cancel
dw_main_name dw_main_name
cb_delete cb_delete
dw_individual dw_individual
dw_claim_list dw_claim_list
dw_deleted_individual dw_deleted_individual
dw_deleted_name dw_deleted_name
dw_injury dw_injury
dw_annuity_eligibility dw_annuity_eligibility
uo_sin_med_check uo_sin_med_check
dw_claim_individual dw_claim_individual
dw_next_individual_no dw_next_individual_no
dw_name dw_name
dw_participant_list dw_participant_list
end type
global w_add_participant_individual w_add_participant_individual

type variables
S_WINDOW_MESSAGE istr_message
N_MAINTAIN_PARTICIPANT_CONTROLLER inv_controller
N_INDIVIDUAL inv_individual 
LONG il_edit_row, il_individual_no, il_claim_no
BOOLEAN ib_from_delete
String is_eventDesc, is_status


end variables

forward prototypes
public function integer wf_insert_event (long al_individual_no)
public subroutine wf_adjust_account_no (ref string as_account, ref string as_account_after)
public function integer wf_read_only ()
public function string wf_set_status ()
end prototypes

public function integer wf_insert_event (long al_individual_no);N_EVENT_LOG  lnv_event_log
DATASTORE lds_claims
LONG ll_rows, ll_cntr, ll_claim, ll_event_no
STRING ls_specific

lnv_event_log = create N_EVENT_LOG

lds_claims = create datastore
lds_claims.dataobject = 'd_individual_claims_approved'
lds_claims.settransobject(sqlca)
ll_rows = lds_claims.Retrieve(al_individual_no)

CHOOSE CASE is_status
	Case 'REMOVED'
		ls_specific = 'RV'
	Case 'CHANGED'
		ls_specific = 'CG'
	Case 'ENTERED'
		ls_specific = 'ENT'
	Case ELSE
		Return 0
END CHOOSE

IF ll_rows > 0 THEN
	FOR ll_cntr = 1 to ll_rows
		ll_claim = lds_claims.GetItemNumber(ll_cntr, "claim_no")
		ll_event_no = lnv_event_log.nf_next_claim_event_no(ll_claim)
		
		SQLCA.nf_begin_transaction()
		
		IF lnv_event_log.nf_create_auto_event(ll_claim,ll_event_no,'024','Bank Information Change',ls_specific) = 0 THEN
			SQLCA.nf_commit_transaction()
		END IF
	NEXT
END IF

Return 0
end function

public subroutine wf_adjust_account_no (ref string as_account, ref string as_account_after);/*This function will take out any dashes that are in the account number so it can be converted
  to a long for comparison. PR3519 - J.M. July 16/03
*/

LONG ll_len_bef, ll_len_after, ll_cntr, ll_cntr2
STRING ls_mid

ll_len_bef = LEN(as_account)
ll_len_after = LEN(as_account_after)

FOR ll_cntr = 1 to ll_len_bef
	ls_mid = MID(as_account,ll_cntr,1)
	IF IsNumber(ls_mid) OR TRIM(ls_mid) <> '-' THEN
		CONTINUE
	ELSE 
		as_account = Replace(as_account,ll_cntr,1,'')
	END IF
NEXT
ls_mid = ''
FOR ll_cntr2 = 1 to ll_len_after
	ls_mid = MID(as_account_after,ll_cntr2,1)
	IF IsNumber(ls_mid) OR TRIM(ls_mid) <> '-' THEN
		CONTINUE
	ELSE 
		as_account_after = Replace(as_account_after,ll_cntr2,1,'')
	END IF
NEXT
end subroutine

public function integer wf_read_only ();/*	protect all columns of visible dw's
*/
	dw_individual.uf_protect_allattributes(TRUE)
	dw_main_name.uf_protect_allattributes(TRUE)
	dw_participant.uf_protect_allattributes(TRUE)
	dw_participant_list.uf_protect_allattributes(TRUE)
	dw_annuity_eligibility.uf_protect_allattributes(TRUE)

	dw_individual.uf_set_backcolor()
	dw_main_name.uf_set_backcolor()
	dw_participant.uf_set_backcolor()
	dw_participant_list.uf_set_backcolor()
	dw_annuity_eligibility.uf_set_backcolor()


/*	disable the buttons
*/
	cb_search.enabled = FALSE
	cb_delete.enabled = FALSE
	cb_ok.enabled = FALSE

Return 0
end function

public function string wf_set_status ();/* New function to compare old with new values (Bank Information) July 8/03 - J.M.
*/

STRING ls_bank_no, ls_transit, ls_account, ls_bank_no_after, ls_transit_after, ls_account_after
BOOLEAN lb_acct_after, lb_acct

IF dw_individual.RowCount() > 0 THEN

	ls_bank_no = dw_individual.GetItemString(dw_individual.GetRow(),'bank_no',Primary!,TRUE)
	IF IsNull(ls_bank_no) THEN ls_bank_no = '0'
	ls_bank_no_after = dw_individual.GetItemString(dw_individual.GetRow(),'bank_no')
	IF IsNull(ls_bank_no_after) THEN ls_bank_no_after = '0'
	ls_transit = dw_individual.GetItemString(dw_individual.GetRow(),'bank_transit_no',Primary!,TRUE)
	IF IsNull(ls_transit) THEN ls_transit = '0'
	ls_transit_after = dw_individual.GetItemString(dw_individual.GetRow(),'bank_transit_no')
	IF IsNull(ls_transit_after) THEN ls_transit_after = '0'
	ls_account = dw_individual.GetItemString(dw_individual.GetRow(),'bank_account_no',Primary!,TRUE)
	IF IsNull(ls_account) THEN ls_account = '0'
	ls_account_after = dw_individual.GetItemString(dw_individual.GetRow(),'bank_account_no')
	IF IsNull(ls_account_after) THEN ls_account_after = '0'

END IF

wf_adjust_account_no(ls_account,ls_account_after)

IF (NOT IsNumber(ls_account_after)) AND ls_account_after > '' THEN lb_acct_after = TRUE
IF (NOT IsNumber(ls_account)) AND ls_account > '' THEN lb_acct = TRUE

IF (ls_bank_no > '' AND (LONG(ls_bank_no) = 0 OR ls_bank_no_after = '') ) OR &
	(ls_transit > '' AND (LONG(ls_transit_after) = 0 OR ls_transit_after = '') ) OR &
	(ls_account > '' AND ((LONG(ls_account_after) = 0 AND lb_acct_after = FALSE) OR ls_account_after = '') ) THEN
	is_status = 'REMOVED'
ELSEIF ((TRIM(ls_bank_no) = '' OR LONG(TRIM(ls_bank_no)) = 0 ) AND TRIM(ls_bank_no_after) > '' ) OR &	
	((TRIM(ls_transit) = '' OR LONG(TRIM(ls_transit)) = 0 ) AND TRIM(ls_transit_after) > '' ) OR &	
	((TRIM(ls_account) = '' OR (LONG(TRIM(ls_account)) = 0 ) AND lb_acct = FALSE) AND TRIM(ls_account_after) > '' ) THEN
	is_status = 'ENTERED'
ELSEIF ((ls_bank_no > '' AND LONG(ls_bank_no) <> 0) AND &
	(ls_bank_no_after > '' AND LONG(ls_bank_no_after) <> 0) AND &
	ls_bank_no <> ls_bank_no_after) OR &	
	((ls_transit > '' AND LONG(ls_transit) <> 0) AND &	
	(ls_transit_after > '' AND LONG(ls_transit_after) <> 0) AND & 	
	ls_transit <> ls_transit_after ) OR &		
	((ls_account > '' AND (LONG(ls_account) <> 0) OR lb_acct = TRUE) AND &	
	(ls_account_after > '' AND (LONG(ls_account_after) <> 0) OR lb_acct_after = TRUE) AND & 	
	ls_account <> ls_account_after ) THEN
	is_status = 'CHANGED'
END IF

RETURN is_status
end function

event open;call super::open;DATAWINDOWCHILD	ldwc_child
U_DWA					ldw_dw[]
INTEGER				li_rtn
Long ll_count_hist

istr_message= Message.PowerObjectParm
inv_controller = Create n_maintain_participant_controller
inv_individual = Create n_individual

ldw_dw[1] = dw_participant_list
ldw_dw[2] = dw_individual
ldw_dw[3] = dw_main_name
ldw_dw[4] = dw_name
ldw_dw[5] = dw_next_individual_no
ldw_dw[6] = dw_claim_individual
ldw_dw[7] = dw_claim_list
ldw_dw[8] = dw_deleted_individual
ldw_dw[9] = dw_deleted_name
ldw_dw[10]= dw_annuity_eligibility

ldw_dw[11] = dw_injury
ldw_dw[12] = dw_participant

inv_controller.nf_set_window_parent(THIS)
inv_controller.nf_set_datawindow(ldw_dw[],SQLCA)
inv_controller.nf_init()

il_individual_no = istr_message.al_doubleparm[2]
il_claim_no = istr_message.al_doubleparm[1]

IF istr_message.al_doubleparm[2] = 0 THEN
	inv_controller.nf_insert(istr_message.al_doubleparm[1])
	il_edit_row = 1
ELSE
	il_edit_row = inv_controller.nf_retrieve( istr_message.al_doubleparm[1], istr_message.al_doubleparm[2])
	cb_delete.Enabled = TRUE
END IF

//check to see if there is history data. If so enable cb_history
Select Count(*)
into    :ll_count_hist
From  ANNUITY_ACCOUNT, ANNUITY_ELIGIBILITY
Where ANNUITY_ACCOUNT.annuity_account_no = ANNUITY_ELIGIBILITY.annuity_account_no
And     individual_no = :il_individual_no
Using SQLCA;
SQLCA.nf_handle_error("w_add_participant_indidivual","open","Select Count From ANNUITY_ACCOUNT")

IF ll_count_hist > 0 THEN cb_history.enabled = True

dw_participant.SetTransObject(SQLCA)
dw_participant_list.ShareData(dw_participant)
dw_participant.ScrollToRow(il_edit_row)
IF dw_participant.GetItemString(il_edit_row,'dependent_flag') <> 'Y' THEN
	dw_participant.GetChild('dependent_reason_code',ldwc_child)
	ldwc_child.SetFilter("dependent_reason_code = ' '")
	ldwc_child.Filter()
END IF

//Retrieve the claim list
dw_claim_list.SetTransObject(SQLCA)

dw_main_name.SetFocus()

IF istr_message.as_mode = 'READ' THEN
	wf_read_only()
END IF

end event

on closequery;call w_ancestor::closequery;SetPointer(HourGlass!)

IF cb_ok.enabled THEN
   IF MessageBox("Save", 'Data needs saved.  Save?', Question!,YesNo!) = 1 THEN
      cb_ok.TriggerEvent(Clicked!)
      IF cb_ok.enabled THEN
         Message.ReturnValue = 1
      END IF
   END IF
END IF

end on

on w_add_participant_individual.create
int iCurrent
call super::create
this.cb_history=create cb_history
this.dw_participant=create dw_participant
this.cb_search=create cb_search
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.dw_main_name=create dw_main_name
this.cb_delete=create cb_delete
this.dw_individual=create dw_individual
this.dw_claim_list=create dw_claim_list
this.dw_deleted_individual=create dw_deleted_individual
this.dw_deleted_name=create dw_deleted_name
this.dw_injury=create dw_injury
this.dw_annuity_eligibility=create dw_annuity_eligibility
this.uo_sin_med_check=create uo_sin_med_check
this.dw_claim_individual=create dw_claim_individual
this.dw_next_individual_no=create dw_next_individual_no
this.dw_name=create dw_name
this.dw_participant_list=create dw_participant_list
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_history
this.Control[iCurrent+2]=this.dw_participant
this.Control[iCurrent+3]=this.cb_search
this.Control[iCurrent+4]=this.cb_ok
this.Control[iCurrent+5]=this.cb_cancel
this.Control[iCurrent+6]=this.dw_main_name
this.Control[iCurrent+7]=this.cb_delete
this.Control[iCurrent+8]=this.dw_individual
this.Control[iCurrent+9]=this.dw_claim_list
this.Control[iCurrent+10]=this.dw_deleted_individual
this.Control[iCurrent+11]=this.dw_deleted_name
this.Control[iCurrent+12]=this.dw_injury
this.Control[iCurrent+13]=this.dw_annuity_eligibility
this.Control[iCurrent+14]=this.uo_sin_med_check
this.Control[iCurrent+15]=this.dw_claim_individual
this.Control[iCurrent+16]=this.dw_next_individual_no
this.Control[iCurrent+17]=this.dw_name
this.Control[iCurrent+18]=this.dw_participant_list
end on

on w_add_participant_individual.destroy
call super::destroy
destroy(this.cb_history)
destroy(this.dw_participant)
destroy(this.cb_search)
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.dw_main_name)
destroy(this.cb_delete)
destroy(this.dw_individual)
destroy(this.dw_claim_list)
destroy(this.dw_deleted_individual)
destroy(this.dw_deleted_name)
destroy(this.dw_injury)
destroy(this.dw_annuity_eligibility)
destroy(this.uo_sin_med_check)
destroy(this.dw_claim_individual)
destroy(this.dw_next_individual_no)
destroy(this.dw_name)
destroy(this.dw_participant_list)
end on

type cb_history from commandbutton within w_add_participant_individual
integer x = 2313
integer y = 2016
integer width = 325
integer height = 96
integer taborder = 80
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Ann History"
end type

event clicked;
istr_message.al_doubleparm[3] = il_individual_no
OpenWithParm(w_annuity_eligibility_history, istr_message,Parent)

end event

type dw_participant from u_dw_online within w_add_participant_individual
integer x = 14
integer y = 12
integer width = 2651
integer height = 284
integer taborder = 60
string dataobject = "d_claim_participant_claim_maint"
borderstyle borderstyle = styleraised!
end type

event rowfocuschanged;call super::rowfocuschanged;DATAWINDOWCHILD	ldwc_child
STRING				ls_string

	
	dw_participant.ScrollToRow(il_edit_row)
	IF il_edit_row <= This.RowCount() AND il_edit_row > 0 THEN
		This.GetChild('dependent_reason_code',ldwc_child)
		ls_string = This.GetItemString(il_edit_row,'claim_role_code')
		ldwc_child.SetFilter('claim_role_code = "' + ls_string + '"')
		ldwc_child.Filter()
		
	END IF


end event

event itemchanged;call super::itemchanged;DATAWINDOWCHILD	ldwc_child
LONG					ll_current_row, ll_count
STRING				ls_string, ls_string2, ls_flag, ls_column_name, ls_old_code

   ll_current_row = This.GetRow()
	
  	CHOOSE CASE This.GetColumnName()

		CASE 'claim_role_code'
			
				IF data = 'SS' THEN
					//A claim must only have one Surviving Spouse.
					Select Count(*)
					Into :ll_count
					From CLAIM_PARTICIPANT
					Where claim_no = :il_claim_no
					And  claim_role_code = 'SS'
					Using SQLCA;

					SQLCA.nf_handle_error('w_add_participant','dw_participant','itemchanged - Select from CLAIM_PARTICIPANT')
					
					IF ll_count > 0 THEN
						Messagebox('Error','A claim must only have one Surviving Spouse.')
						This.Setitem(row,'claim_role_code',this.getitemstring(row,'claim_role_code'))
     					Return 1
					END IF
				END IF
				
				//BR 1.160 A Surving Spouse with an annuity account must not change there role code.
				
				ls_old_code = this.getitemstring(row,"claim_role_code")
				ll_count = 0
				
				Select Count(*)
				Into   :ll_count
				From ANNUITY_ACCOUNT
				Where claim_no = :il_claim_no
				And     claim_role_code = 'SS'
				Using SQLCA;
				
				SQLCA.nf_handle_error('w_add_participant','dw_participant','itemchanged - Select from CLAIM_PARTICIPANT')
				
				IF ls_old_code = 'SS' AND ll_count > 0 THEN
					Messagebox('Error','The role code can not change from Surving Spouse when there is an annuity account setup.',Exclamation!)
					This.Setitem(row,'claim_role_code',this.getitemstring(row,'claim_role_code'))					
					Return 1
				END IF
				
			
				This.GetChild('dependent_reason_code',ldwc_child)
				ls_string = This.GetText()
				ls_string2 = This.GetItemString(ll_current_row,'dependent_flag')
				IF ls_string2 = 'Y' THEN
					ldwc_child.SetFilter('claim_role_code = "' + ls_string + '"')
				ELSE
					ldwc_child.SetFilter("dependent_reason_code = ' '")
				END IF
				ldwc_child.Filter()
				This.SetItem(ll_current_row,'dependent_reason_code', ' ')

     	CASE 'dependent_flag'
			IF This.GetText() = 'N' THEN
				This.GetChild('dependent_reason_code',ldwc_child)
				ldwc_child.SetFilter("dependent_reason_code = ' '")
				ldwc_child.Filter()
			ELSE
				This.GetChild('dependent_reason_code',ldwc_child)
				ls_string = This.GetItemString(ll_current_row,'claim_role_code')
				ldwc_child.SetFilter('claim_role_code = "' + ls_string + '"')
				ldwc_child.Filter()
			END IF
				This.SetItem(ll_current_row,'dependent_reason_code', ' ')
				
				
		CASE 'claim_participant_claimant_active_flag'
			// The claimant must not be made inactive if there is an incomplete annuity checklist.
			IF data = 'N' THEN
				ls_flag = This.getitemstring(row,'claim_participant_claimant_active_flag')

				IF ls_flag = 'Y' THEN
					Select count(*)
					Into    :ll_count
					From  ANNUITY_ACCOUNT, SUBSCRIBER_CHECKLIST_XREF, CHECKLIST
					Where ANNUITY_ACCOUNT.checklist_subscriber_no = SUBSCRIBER_CHECKLIST_XREF.checklist_subscriber_no
					And  SUBSCRIBER_CHECKLIST_XREF.checklist_no = CHECKLIST.checklist_no
					And  CHECKLIST.checklist_status_code = 'IA'
					And  ANNUITY_ACCOUNT.individual_no = :il_individual_no
					Using SQLCA;
					
					SQLCA.nf_handle_error('w_add_participant','dw_participant','itemchanged - Select from ANNUITY_ACCOUNT, SUBSCRIBER_CHECKLIST_XREF, CHECKLIST')
					
					IF ll_count > 0 THEN
						Messagebox('Error','The claimant must not be made inactive if there is an incomplete annuity checklist.')
						Return 1
					END IF
				END IF
			END IF
		
		END CHOOSE

	cb_ok.enabled = TRUE

end event

event rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup
/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	
	lm_popup.m_options.PopMenu(w_add_participant_individual.PointerX(), w_add_participant_individual.PointerY())
	
	Destroy lm_popup
end event

event ue_print;LONG ll_job

	ll_job = PrintOpen()
	Parent.Print(ll_job,0,0)
	PrintClose(ll_job)

end event

type cb_search from commandbutton within w_add_participant_individual
integer x = 55
integer y = 2016
integer width = 325
integer height = 96
integer taborder = 120
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Search..."
end type

event clicked;S_WINDOW_MESSAGE lstr_message
LONG ll_individual_no

SetPointer(HourGlass!)
lstr_message.awi_parent_window = PARENT
lstr_message.as_stringparm[1] = 'd_basic_claim_search'
lstr_message.as_stringparm[2] =  istr_message.as_stringparm[1]
lstr_message.al_doubleparm[1] = il_individual_no

OpenWithParm(w_individual_search, lstr_message)
lstr_message = Message.PowerObjectParm
IF lstr_message.al_doubleparm[1] > 0 THEN
   inv_controller.nf_retrieve_individual(lstr_message.al_doubleparm[1])
   cb_ok.enabled = TRUE
   cb_cancel.enabled = TRUE
END IF
end event

type cb_ok from commandbutton within w_add_participant_individual
integer x = 978
integer y = 2016
integer width = 325
integer height = 96
integer taborder = 130
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&OK"
end type

event clicked;LONG		ll_return_value, ll_cnt, ll_no
STRING	ls_old_alias_lname, ls_old_alias_fname,  ls_old_main_lname, ls_old_main_fname
STRING	ls_new_alias_lname, ls_new_alias_fname, ls_new_main_lname, ls_new_main_fname
STRING	ls_temp_alias_fname, ls_temp_alias_lname, ls_temp_main_lname, ls_temp_main_fname
LONG		ll_individual_no, ll_claim_no, ll_rowcnt
INT		li_msgbox, li_rtn, li_trancount
boolean  lb_death_date_modified

N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '008' refers to the Claim Maintenance module, '044' refers to the Payment Processing module
- likewise, '046' refers to the ABCC Eligibility Export module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('008','044','save of a claim participant',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF

//clear variable
li_rtn = 0
li_rtn = ln_process_run_status.nf_in_progress('008','046','save of a claim participant',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF

/******************************************************************************************/

SetPointer(HourGlass!)

is_Status = wf_set_status()
ll_individual_no = istr_message.al_doubleparm[2]

/* Check if main or alias names have been changed.  If name(s) have been changed, then check if the alias & main names
	are being switched.  Since SQLServer07 was implemented, users have had duplicate primary key problems when trying
	to switch the alias & main	names for an individual.  The problems are due to how primary keys are now handled.
	In order to avoid a violation of the 'pk_INDIVIDUAL_NAME' primary key constraint, the name switch must be done in
	two stages.
*/
IF ( dw_main_name.GetItemStatus ( dw_main_name.GetRow(), 0, Primary! ) = DataModified! OR &
	  ( dw_individual.GetItemStatus ( 1, "individual_name_last_name", Primary! ) = DataModified! OR &
	    dw_individual.GetItemStatus ( 1, "individual_name_given_names", Primary! ) = DataModified! ) ) THEN

/* Get original alias names from table
*/
	SELECT last_name, given_names
	  INTO :ls_old_alias_lname, :ls_old_alias_fname
	  FROM INDIVIDUAL_NAME
	 WHERE individual_no = :ll_individual_no
	   AND name_type_code = 'A'
	 USING SQLCA ;
 
	IF SQLCA.nf_handle_error('Embedded SQL: SELECT Alias Name FROM INDIVIDUAL_NAME', 'w_add_participant_individual', 'cb_ok clicked event') < 0 THEN
		Return -1
	END IF

/* Get original main names from table
*/
	SELECT last_name, given_names
	  INTO :ls_old_main_lname, :ls_old_main_fname
	  FROM INDIVIDUAL_NAME
	 WHERE individual_no = :ll_individual_no
	   AND name_type_code = 'M'
	 USING SQLCA ;
 
	IF SQLCA.nf_handle_error('Embedded SQL: SELECT Main Name FROM INDIVIDUAL_NAME', 'w_add_participant_individual', 'cb_ok clicked event') < 0 THEN
		Return -1
	END IF
 
	IF dw_individual.AcceptText() < 0 THEN
		dw_individual.SetFocus()
		Return
	END IF

	IF dw_main_name.AcceptText() < 0 THEN
		dw_main_name.SetFocus()
		Return
	END IF	

	ls_new_alias_lname = dw_individual.GetItemString( 1, 'individual_name_last_name' )
	ls_new_alias_fname = dw_individual.GetItemString( 1, 'individual_name_given_names' )
	ls_new_main_lname = dw_main_name.GetItemString( 1, 'last_name' )
	ls_new_main_fname = dw_main_name.GetItemString( 1, 'given_names' )

//check to see if main last name is null
	IF ls_new_main_lname = '' THEN
		MessageBox('Last Name', 'Last Name is required.',information!)
		Return -1
	END IF
	
//check to see if main first name is null
	IF	ls_new_main_fname = '' THEN
		MessageBox('First Name', 'First Name is required.',information!)
		Return -1
	END IF
	
//check if new name and alias name are the same, if so prevent save.
	IF ls_new_alias_fname + ls_new_alias_lname = ls_new_main_fname + ls_new_main_lname THEN
		MessageBox("Error", "The alias cannot be the same as the main name, please delete or change it.")
		Return -1
	END IF

/* Check if the alias and main names have been switched
*/
	IF ( ls_new_alias_fname + ls_new_alias_lname = ls_old_main_fname + ls_old_main_lname ) &
	OR ( ls_new_main_fname + ls_new_main_lname = ls_old_alias_fname + ls_old_alias_lname ) THEN

	/*	Names have been switched, so set dummy alias name values
	
	   PR3936/PR4072 - J.M. 2004/06/09 - Only need to change(adding a '.' to the end) the main name when 
		the alias and main are being switched(they are identical), changing both is unnecessary. 
		Only add the '.' to the end if there is a value there, otherwise set it to Null(PR4072).
		If the alias was null and was set to '.', when in the nf_insert_individual_name it sees 
		it as a value and sets that in the dw.  It is actually a null value and should be removed 
		because when it returns from that fxn, it updates the INDIVIDUAL_NAME table using SQL, with the 
		actual value, that being NULL - which is not allowed.
		Removed the code that gets the alias values since this won't be used anymore. 			
	*/
		ls_temp_main_lname = dw_main_name.GetItemString( dw_main_name.GetRow(), 'last_name') 
		ls_temp_main_fname = dw_main_name.GetItemString( dw_main_name.GetRow(), 'given_names')
		
	/* If the main names are "" then set to null and only add '.' if there is a value to 
	   prevent the app from saving the '.' when there shouldn't be anything there.
	*/
		IF ls_temp_main_lname = "" OR IsNull(ls_temp_main_lname) THEN
			SetNull(ls_temp_main_lname)
			dw_main_name.SetItem( dw_main_name.GetRow(), 'last_name', ls_temp_main_lname )
		ELSE
			dw_main_name.SetItem( dw_main_name.GetRow(), 'last_name', 'Name-Change' )
		END IF
	
		IF ls_temp_main_fname = "" OR IsNull(ls_temp_main_fname) THEN
			SetNull(ls_temp_main_fname)
			dw_main_name.SetItem(dw_main_name.GetRow(), 'given_names', ls_temp_main_fname)
		ELSE 
			dw_main_name.SetItem( dw_main_name.GetRow(), 'given_names', 'In-Progress' )
		END IF
		
		
		SQLCA.nf_begin_transaction()
		
		ll_return_value = inv_controller.nf_save()
		IF ll_return_value < 0 THEN
			
			SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
			IF li_trancount > 0 THEN
				SQLCA.nf_rollback_transaction()
			END IF
			
			dw_main_name.SetItem( dw_main_name.GetRow(), 'last_name', ls_new_main_lname)
			dw_main_name.SetItem( dw_main_name.GetRow(), 'given_names', ls_new_main_fname)
			GOTO save_failed
		ELSE
			SQLCA.nf_commit_transaction()
		END IF
		
		
		SQLCA.nf_begin_transaction()
		
		UPDATE INDIVIDUAL_NAME
		SET    last_name = :ls_new_main_lname, given_names = :ls_new_main_fname
		WHERE  individual_no = :ll_individual_no
		AND    name_type_code = 'M'
		USING SQLCA ;

		IF SQLCA.nf_handle_error('Embedded SQL: INSERT Main Name INTO INDIVIDUAL_NAME', 'w_create_claim', 'cb_save clicked event') < 0 THEN
			RETURN -1
		END IF		
		
		/* PR3936 - Only update the alias names in the INDIVIDUAL_NAME table if
		   they actually have a value. 
		*/		
		IF NOT IsNull(ls_new_alias_lname) AND NOT IsNull(ls_new_alias_fname) THEN
			UPDATE INDIVIDUAL_NAME
			SET    last_name = :ls_new_alias_lname, given_names = :ls_new_alias_fname
			WHERE  individual_no = :ll_individual_no
			AND    name_type_code = 'A'
			USING SQLCA ;
	
			IF SQLCA.nf_handle_error('Embedded SQL: INSERT Alias Name INTO INDIVIDUAL_NAME', 'w_add_participant_individual', 'cb_ok clicked') < 0 THEN
				RETURN -1
			END IF
		END IF
		
		UPDATE INDIVIDUAL
		SET    last_name = :ls_new_main_lname, given_names = :ls_new_main_fname
		WHERE  individual_no = :ll_individual_no
		USING  SQLCA ;
		
		IF SQLCA.nf_handle_error('Embedded SQL: INSERT Name INTO INDIVIDUAL', 'w_add_participant_individual', 'cb_ok clicked') < 0 THEN
			RETURN -1
		END IF


	/*	Need to re-retrieve here to avoid application errors during nf_save() below.
	*/
//		   ll_return_value = inv_controller.nf_retrieve_individual (ll_individual_no)
		ll_claim_no = dw_participant.GetItemNumber(dw_participant.GetRow(), 'claim_no')
		ll_return_value = inv_controller.nf_retrieve (ll_claim_no, ll_individual_no)
		IF ll_return_value < 0 THEN
			MessageBox ('Retrieval Error', 'An error occurred while retrieving information.~r~nPlease contact the HelpDesk', Exclamation!)				
			SQLCA.nf_rollback_transaction()
			Return -1
		END IF
		
		IF ll_return_value < 0 THEN
			li_msgbox = MessageBox('Retrieval Error','Could not re-retrieve information for this individual.' + &
											+ '~r~nPlease contact the HelpDesk.')
		/*	If the retrieve fails, then nf_save() will fail below.  The result would be half a transaction - the first
			Update succeeds but the second fails.  The result if the first Update isn't rolled back is that the
			temporary alias names (name + '.') will be stored in the database & will subsequently get displayed in the
			datawindow. The below rollback is issued so that both transactions get reversed ensuring that the names are returned to
			their original values in the database.
		*/
			SQLCA.nf_rollback_transaction()
			Return -1
		END IF
	END IF
END IF

inv_controller.nf_set_commit(TRUE)

// if a transaction was not begun above, then begin one here
SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
IF li_trancount = 0 THEN
	SQLCA.nf_begin_transaction()
END IF

ll_return_value = inv_controller.nf_save()

save_failed:
IF ll_return_value = 0 OR (ib_from_delete AND ll_return_value < 0) THEN
	
	SQLCA.nf_commit_transaction()
	
	ib_from_delete = FALSE
   
	IF is_status = 'REMOVED' OR is_status = 'CHANGED' OR is_status = 'ENTERED' THEN
		IF wf_insert_event(ll_individual_no) < 0 THEN
			Messagebox('Error','An Error occurred while trying to log an event',information!)
		END IF
	END IF
	is_status = ''
	cb_cancel.TriggerEvent(Clicked!,0,1)
ELSE
	SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
	IF li_trancount > 0 THEN
		SQLCA.nf_rollback_transaction()
	END IF
	
	ib_from_delete = FALSE
  // display user object for sin and med if necessary
   IF ll_return_value = -201 THEN
   // sin duplicate
      uo_sin_med_check.uf_set_dw('SIN')
      ll_no = dw_individual.GetItemNumber(1,'sin_no')
      ll_cnt = uo_sin_med_check.dw_list.Retrieve(ll_no)
      IF ll_cnt > 0 THEN
      // display warning that individuals exist with the same sin
      // display the list
         uo_sin_med_check.visible = TRUE 
         uo_sin_med_check.dw_list.visible = TRUE
         uo_sin_med_check.sle_message.text = 'The above individual(s) exist with the same SIN.  Please correct.'
         uo_sin_med_check.BringToTop = TRUE
         uo_sin_med_check.dw_list.SetFocus()
         Return -1
      END IF 
   END IF
   IF ll_return_value = -202 THEN
   // medicare duplicate
      uo_sin_med_check.uf_set_dw('MED')
      ll_no = dw_individual.GetItemNumber(1,'medicare_no')
      ll_cnt = uo_sin_med_check.dw_list.Retrieve(ll_no)
      IF ll_cnt > 0 THEN
      // display warning that individuals exist with the same medicare
      // display the list
         uo_sin_med_check.visible = TRUE 
         uo_sin_med_check.dw_list.visible = TRUE
         uo_sin_med_check.sle_message.text = 'The above individual(s) exist with the same medicare number.  Please correct.'
         uo_sin_med_check.BringToTop = TRUE
         uo_sin_med_check.dw_list.SetFocus()
         Return -1
      END IF 
   END IF
	Return -1
END IF

Return
end event

type cb_cancel from commandbutton within w_add_participant_individual
integer x = 1312
integer y = 2016
integer width = 325
integer height = 96
integer taborder = 150
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cancel"
boolean cancel = true
end type

event clicked;LONG ll_message

ll_message = message.LongParm

SetPointer(HourGlass!)

cb_ok.enabled = FALSE

IF ll_message <> 1 THEN
	is_status = ''
END IF

Close(PARENT)
end event

type dw_main_name from u_dw_online within w_add_participant_individual
integer x = 55
integer y = 896
integer width = 2587
integer height = 92
integer taborder = 100
string dataobject = "d_individual_name_main"
boolean border = false
end type

event itemchanged;call super::itemchanged;inv_controller.nf_change_item(3)
cb_ok.enabled = TRUE
end event

event rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup
/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	
	lm_popup.m_options.PopMenu(w_add_participant_individual.PointerX(), w_add_participant_individual.PointerY())
	
	Destroy lm_popup
end event

event ue_print;LONG ll_job

	ll_job = PrintOpen()
	Parent.Print(ll_job,0,0)
	PrintClose(ll_job)

end event

event editchanged;call super::editchanged;IF len(data) = 1  OR (  len(data) > 1 AND MATCH( MID(data, len(data) - 1, 1) , "[ '-]" ) ) THEN   //looks for a space or a hyphen or a apostrophy
	// this.settext(WordCap(data))  WordCap function works for the space but not the hyphen or apostrophy so manually upper case the last character
	this.settext (  left (data,len(data)-1) + upper(right(data,1)  ))	
	this.SelectText(this.selectedstart() + len(data) + 1, 0)	
END IF
end event

type cb_delete from commandbutton within w_add_participant_individual
integer x = 430
integer y = 2016
integer width = 325
integer height = 96
integer taborder = 140
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Delete"
end type

event clicked;/* PR3914 - Check to see if participant can be removed.  Cannot be the claimant, 
   cannot have payments. 
*/
LONG ll_rtn

IF inv_controller.nf_can_delete(istr_message.al_doubleparm[2],istr_message.al_doubleparm[1]) < 0 THEN
	RETURN -1
END IF

IF MessageBox('Delete?','Are you sure you would like to delete this Participant?',question!,yesno!) = 1 THEN
	
	SQLCA.nf_begin_transaction()
	
	DELETE CLAIM_PARTICIPANT
	WHERE  claim_no      = :istr_message.al_doubleparm[1] 
	AND    individual_no = :istr_message.al_doubleparm[2]
	USING  SQLCA;
	
	IF SQLCA.nf_handle_error('DELETE CLAIM_PARTICIPANT', "w_add_participant_individual","cb_delete") < 0 THEN
		MessageBox('Error','There was a problem removing the Participant. Please Call Help Desk',information!)
		SQLCA.nf_rollback_transaction()
		RETURN -1
	END IF
	
	SQLCA.nf_commit_transaction()
	
END IF




Close(Parent)



end event

type dw_individual from u_dw_online within w_add_participant_individual
integer x = 18
integer y = 824
integer width = 2647
integer height = 1164
integer taborder = 110
string dataobject = "d_individual"
borderstyle borderstyle = styleraised!
end type

on itemfocuschanged;/*	overridden because it messes up edit masked columns
*/
end on

event itemchanged;call super::itemchanged;is_eventDesc = this.getitemstring(1,"event_desc")
IF dwo.name = "individual_court_order_flag" THEN
	this.setitem(row,'event_desc','')
END IF
dw_individual.uf_set_pbmessage(TRUE)
IF inv_controller.nf_change_item(2) < 0 THEN
	Return 1
END IF
cb_ok.enabled = TRUE
end event

event rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup
/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	
	lm_popup.m_options.PopMenu(w_add_participant_individual.PointerX(), w_add_participant_individual.PointerY())
	
	Destroy lm_popup
end event

event ue_print;LONG ll_job

	ll_job = PrintOpen()
	Parent.Print(ll_job,0,0)
	PrintClose(ll_job)

end event

event retrieveend;call super::retrieveend;STRING ls_eventMessage,ls_eventSpecfic
long ll_individual, ll_medicare, ll_sin

if rowcount > 0 then
	ll_individual = this.getitemnumber(this.getrow(),'individual_no')
	SELECT TOP 1 CLAIM_EVENT.event_comment ,event_specific_code  
	INTO :ls_eventMessage,:ls_eventSpecfic 
	FROM CLAIM_EVENT  
	WHERE 	CLAIM_EVENT.event_type_code = '020' AND
		CLAIM_EVENT.claim_no in (  SELECT CLAIM.claim_no  
												FROM CLAIM  
												WHERE CLAIM.individual_no = :ll_individual )   
	Order By CLAIM_EVENT.event_date desc,
				CLAIM_EVENT.event_no desc;
	
	IF len(trim(ls_eventMessage)) > 0 then
		this.setitem(this.getrow(),'event_desc',ls_eventMessage)		
	end if
end if

IF vgst_user_profile.maintain_sin_med_flag = 'N' THEN	
	ll_sin = dw_individual.GetItemNumber(dw_individual.GetRow(), 'sin_no')
	IF ll_sin > 0 THEN
		dw_individual.Object.sin_no.Protect = 1
	END IF
	ll_medicare = dw_individual.GetItemNumber(dw_individual.GetRow(), 'medicare_no')
	IF ll_medicare > 0 THEN
		dw_individual.Object.medicare_no.Protect = 1
	END IF
END  IF
end event

event rowfocuschanged;call super::rowfocuschanged;STRING ls_eventMessage,ls_eventSpecfic
long ll_individual
if this.rowcount() > 0 then
	ll_individual = this.getitemnumber(currentrow,'individual_no')
	SELECT TOP 1 CLAIM_EVENT.event_comment ,event_specific_code  
	INTO :ls_eventMessage,:ls_eventSpecfic 
	FROM CLAIM_EVENT  
	WHERE 	CLAIM_EVENT.event_type_code = '020' AND
		CLAIM_EVENT.claim_no in (  SELECT CLAIM.claim_no  
												FROM CLAIM  
												WHERE CLAIM.individual_no = :ll_individual )   
	Order By CLAIM_EVENT.event_date desc,
				CLAIM_EVENT.event_no desc;
	
	IF len(trim(ls_eventMessage)) > 0 then
		this.setitem(currentrow,'event_desc',ls_eventMessage)
	end if
end if
end event

event updateend;call super::updateend;IF isNull(is_eventDesc) or trim(is_eventDesc) = "" then
ELSE
	this.Setitem(1,"event_desc",is_eventDesc)
END IF
end event

event updatestart;call super::updatestart;is_eventDesc = this.getitemstring(1,"event_desc")
end event

event editchanged;call super::editchanged;IF dwo.name = 'individual_name_last_name' OR dwo.name ='individual_name_given_names' THEN
	IF len(data) = 1  OR (  len(data) > 1 AND MATCH( MID(data, len(data) - 1, 1) , "[ '-]" ) ) THEN   //looks for a space or a hyphen or a apostrophy
		// this.settext(WordCap(data))  WordCap function works for the space but not the hyphen or apostrophy so manually upper case the last character
		this.settext (  left (data,len(data)-1) + upper(right(data,1)  ))	
		this.SelectText(this.selectedstart() + len(data) + 1, 0)	
	END IF
END IF
end event

type dw_claim_list from u_dw_online within w_add_participant_individual
boolean visible = false
integer x = 2112
integer y = 1596
integer width = 187
integer height = 144
integer taborder = 30
string dataobject = "d_claims_for_individual"
end type

type dw_deleted_individual from u_dw_online within w_add_participant_individual
boolean visible = false
integer x = 1691
integer y = 1576
integer width = 242
integer height = 184
integer taborder = 20
string dataobject = "d_individual"
end type

type dw_deleted_name from u_dw_online within w_add_participant_individual
boolean visible = false
integer x = 2002
integer y = 1572
integer width = 192
integer height = 188
integer taborder = 10
string dataobject = "d_individual_name_both"
end type

type dw_injury from u_dw_online within w_add_participant_individual
boolean visible = false
integer x = 2482
integer y = 1708
integer width = 219
integer height = 140
integer taborder = 160
string dataobject = "d_injury"
end type

type dw_annuity_eligibility from u_dw_online within w_add_participant_individual
integer x = 9
integer y = 308
integer width = 2651
integer height = 508
integer taborder = 2
string dataobject = "d_annuity_eligibility_display"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup
/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	
	lm_popup.m_options.PopMenu(w_add_participant_individual.PointerX(), w_add_participant_individual.PointerY())
	
	Destroy lm_popup
end event

event ue_print;LONG ll_job

	ll_job = PrintOpen()
	Parent.Print(ll_job,0,0)
	PrintClose(ll_job)

end event

type uo_sin_med_check from u_check_sin_med within w_add_participant_individual
boolean visible = false
integer x = 174
integer y = 412
integer height = 848
integer taborder = 70
end type

on uo_sin_med_check.destroy
call u_check_sin_med::destroy
end on

type dw_claim_individual from u_dw_online within w_add_participant_individual
boolean visible = false
integer x = 1536
integer y = 1572
integer width = 270
integer height = 172
integer taborder = 40
string dataobject = "d_claim_individual"
end type

type dw_next_individual_no from u_dw_online within w_add_participant_individual
boolean visible = false
integer x = 1925
integer y = 1584
integer width = 192
integer height = 132
integer taborder = 80
string dataobject = "d_next_individual_no"
end type

type dw_name from u_dw_online within w_add_participant_individual
boolean visible = false
integer x = 2318
integer y = 1568
integer width = 283
integer height = 172
integer taborder = 90
string dataobject = "d_individual_name"
end type

type dw_participant_list from u_dw_online within w_add_participant_individual
boolean visible = false
integer x = 2565
integer y = 1544
integer width = 233
integer height = 140
integer taborder = 50
string dataobject = "d_claim_participant_maint"
end type

