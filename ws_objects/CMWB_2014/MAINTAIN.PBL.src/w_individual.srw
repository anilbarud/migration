$PBExportHeader$w_individual.srw
$PBExportComments$maintains individual - no add or delete allowed
forward
global type w_individual from w_a_tool
end type
type cb_save from commandbutton within w_individual
end type
type cb_cancel from commandbutton within w_individual
end type
type dw_individual_name from u_dw_online within w_individual
end type
type dw_next_individual_no from u_dw_online within w_individual
end type
type uo_sin_med_check from u_check_sin_med within w_individual
end type
type dw_claim_list from u_dw_online within w_individual
end type
type tab_individual from tab within w_individual
end type
type tabpage_individual_maintenance from userobject within tab_individual
end type
type dw_individual from u_dw_online within tabpage_individual_maintenance
end type
type dw_main_name from u_dw_online within tabpage_individual_maintenance
end type
type tabpage_individual_maintenance from userobject within tab_individual
dw_individual dw_individual
dw_main_name dw_main_name
end type
type tabpage_injured_worker_registration from userobject within tab_individual
end type
type cb_produce_mail_package from commandbutton within tabpage_injured_worker_registration
end type
type dw_entity_administration from u_dw_online within tabpage_injured_worker_registration
end type
type dw_token from u_dw_online within tabpage_injured_worker_registration
end type
type tabpage_injured_worker_registration from userobject within tab_individual
cb_produce_mail_package cb_produce_mail_package
dw_entity_administration dw_entity_administration
dw_token dw_token
end type
type tab_individual from tab within w_individual
tabpage_individual_maintenance tabpage_individual_maintenance
tabpage_injured_worker_registration tabpage_injured_worker_registration
end type
end forward

global type w_individual from w_a_tool
integer width = 3232
boolean resizable = false
cb_save cb_save
cb_cancel cb_cancel
dw_individual_name dw_individual_name
dw_next_individual_no dw_next_individual_no
uo_sin_med_check uo_sin_med_check
dw_claim_list dw_claim_list
tab_individual tab_individual
end type
global w_individual w_individual

type variables
N_INDIVIDUAL inv_individual
LONG il_individual_no, il_claim_no
BOOLEAN ib_in_itemchanged
STRING is_eventdesc, is_status

u_dw_online idw_individual, idw_main_name, idw_token
end variables

forward prototypes
public function integer wf_read_only ()
public function integer wf_insert_event (long al_individual_no)
public subroutine wf_adjust_account_no (ref string as_account, ref string as_account_after)
public function string wf_set_status ()
public subroutine wf_set_iw_tab_picture ()
public function integer wf_process_injured_worker_package (long al_wif_entity_no, long al_no_minutes_to_expire)
end prototypes

public function integer wf_read_only ();

/*	protect all columns of visible dw's
*/
	idw_individual.uf_protect_allattributes(TRUE)
	idw_main_name.uf_protect_allattributes(TRUE)

	idw_individual.uf_set_backcolor()
	idw_main_name.uf_set_backcolor()


/*	disable the buttons
*/
	cb_save.enabled = FALSE
	cb_cancel.enabled = FALSE
	
	tab_individual.tabpage_injured_worker_registration.cb_produce_mail_package.Enabled = FALSE

Return 0
end function

public function integer wf_insert_event (long al_individual_no);N_EVENT_LOG  lnv_event_log
//DATASTORE    lds_claims 	//PR 19887 2014-11-21 David Worboys
INTEGER      li_trancount
//LONG         	ll_rows 		//PR 19887 2014-11-21 David Worboys
//LONG			ll_cntr			//PR 19887 2014-11-21 David Worboys
LONG			ll_claim
LONG			ll_event_no
STRING       ls_specific

lnv_event_log = create N_EVENT_LOG

/* PR 19887 2014-11-21  David Worboys Removed
lds_claims = create datastore
lds_claims.dataobject = 'd_individual_claims_approved'
lds_claims.settransobject(sqlca)
ll_rows = lds_claims.Retrieve(al_individual_no)
*/
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

ll_event_no = lnv_event_log.nf_next_individual_event_no(al_individual_no)

SQLCA.nf_begin_transaction()

IF (lnv_event_log.nf_create_auto_individual_event( al_individual_no, ll_event_no, '068','Bank Information Change', ls_specific) = 0) THEN
	SQLCA.nf_commit_transaction()
END IF

/* PR 19887 2014-10-27 David Worboys - only want one individual  event number registered, not generate one for each claim!

IF ll_rows > 0 THEN
	FOR ll_cntr = 1 to ll_rows
		ll_claim = lds_claims.GetItemNumber(ll_cntr, "claim_no")
//PR 19887 2014-10-27 David Worboys Change to Individual Events		
//		ll_event_no = lnv_event_log.nf_next_claim_event_no(ll_claim)
		ll_event_no = lnv_event_log.nf_next_individual_event_no(al_individual_no)
		
		SQLCA.nf_begin_transaction()
//PR 19887 2014-10-27 David Worboys Change to Individual Events				
//		IF lnv_event_log.nf_create_auto_event(ll_claim,ll_event_no,'024','Bank Information Change',ls_specific) = 0 THEN
		IF (lnv_event_log.nf_create_auto_individual_event( al_individual_no, ll_event_no, '068','Bank Information Change', ls_specific) = 0) THEN
			SQLCA.nf_commit_transaction()
		END IF
	NEXT
END IF
*/
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

public function string wf_set_status ();/* New function to compare old with new values (Bank Information) July 8/03 - J.M.
*/

INTEGER  li_row
STRING ls_bank_no, ls_transit, ls_account, ls_bank_no_after, ls_transit_after, ls_account_after
BOOLEAN lb_acct_after, lb_acct


li_row = idw_individual.GetRow()


ls_bank_no = idw_individual.GetItemString(li_row,'bank_no',Primary!,TRUE)
IF IsNull(ls_bank_no) THEN ls_bank_no = '0'
ls_bank_no_after = idw_individual.GetItemString(li_row,'bank_no')
IF IsNull(ls_bank_no_after) THEN ls_bank_no_after = '0'
ls_transit = idw_individual.GetItemString(li_row,'bank_transit_no',Primary!,TRUE)
IF IsNull(ls_transit) THEN ls_transit = '0'
ls_transit_after = idw_individual.GetItemString(li_row,'bank_transit_no')
IF IsNull(ls_transit_after) THEN ls_transit_after = '0'
ls_account = idw_individual.GetItemString(li_row,'bank_account_no',Primary!,TRUE)
IF IsNull(ls_account) THEN ls_account = '0'
ls_account_after = idw_individual.GetItemString(li_row,'bank_account_no')
IF IsNull(ls_account_after) THEN ls_account_after = '0'

wf_adjust_account_no(ls_account,ls_account_after)

IF (NOT IsNumber(ls_account_after)) AND ls_account_after > '' THEN lb_acct_after = TRUE
IF (NOT IsNumber(ls_account)) AND ls_account > '' THEN lb_acct = TRUE

IF (ls_bank_no > '' AND (LONG(ls_bank_no_after) = 0 OR ls_bank_no_after = '') ) OR &
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

public subroutine wf_set_iw_tab_picture ();INTEGER   li_token_rows, li_token_status_code


li_token_rows = idw_token.RowCount()

IF li_token_rows > 0 THEN
	li_token_status_code = idw_token.GetItemNumber(1,'token_status_code')
ELSE
	li_token_status_code = 99
END IF

CHOOSE CASE li_token_status_code
	CASE 0  // Available
		tab_individual.tabpage_injured_worker_registration.picturename = 'mail_16_h.gif'
		st_title.Text = 'Maintain Individual - Invited'
	CASE 1  // Used
		tab_individual.tabpage_injured_worker_registration.picturename = 'tick_16_h.gif'
		st_title.Text = 'Maintain Individual - Registered'
	CASE 2  // Expired
		tab_individual.tabpage_injured_worker_registration.picturename = 'delete_x_16_h.gif'
		st_title.Text = 'Maintain Individual - Expired'
	CASE 3  // Cancelled
		tab_individual.tabpage_injured_worker_registration.picturename = 'delete_x_16_h.gif'
		st_title.Text = 'Maintain Individual - Cancelled'
	CASE ELSE
		tab_individual.tabpage_injured_worker_registration.picturename = ''
		st_title.Text = 'Maintain Individual - No Registration'
END CHOOSE

end subroutine

public function integer wf_process_injured_worker_package (long al_wif_entity_no, long al_no_minutes_to_expire);DATETIME  ldtm_package
INTEGER   li_rtn, li_current_token_row, li_mail_package_rows, li_mail_package_row, li_msg_rtn, li_inserted_row
LONG      ll_record_no
STRING    ls_exp_datetime, ls_token_id
U_DS      lds_create_token, lds_cancel_mail_package, lds_workflow_token_related_mail_package


lds_cancel_mail_package = CREATE u_ds
lds_cancel_mail_package.DataObject = 'ds_cancel_mail_package'
lds_cancel_mail_package.SetTransObject(SQLCA)

li_current_token_row = idw_token.GetRow()
IF li_current_token_row > 0 THEN
	ll_record_no = idw_token.GetItemNumber(li_current_token_row, 'record_no')
	ldtm_package = idw_token.GetItemDateTime(li_current_token_row, 'create_date')
	li_mail_package_rows = lds_cancel_mail_package.Retrieve(ll_record_no)
	li_mail_package_row = lds_cancel_mail_package.GetRow()
	IF li_mail_package_rows > 0 THEN
		li_msg_rtn = MessageBox('Existing Registration Letter','A registration package was created for this individual on '+String(ldtm_package,'yyyy-mm-dd')+'.' &
														                 + '~r~n~r~n' &
																			  + 'Would you like to cancel it and create a new one?',Question!,YesNo!,2)
		IF li_msg_rtn = 2 THEN
			RETURN -1
		ELSE			
			lds_cancel_mail_package.SetItem(li_mail_package_row,'mail_status_code','X')
		END IF
	ELSE
		li_msg_rtn = MessageBox('Create Letter?','You are about to create a new registration package for this claimant.' &
													      + '~r~n~r~n' &
														   + 'Would you like to continue?',Question!,YesNo!,2)
		IF li_msg_rtn = 2 THEN
			RETURN -1
		END IF
	END IF
ELSE
	li_msg_rtn = MessageBox('Create Letter?','You are about to create the first registration package for this claimant.' &
												      + '~r~n~r~n' &
												      + 'Would you like to continue?',Question!,YesNo!,2)
	IF li_msg_rtn = 2 THEN
		RETURN -1
	END IF
END IF


lds_create_token = CREATE u_ds
lds_create_token.DataObject =  "d_create_token"
lds_create_token.SetTransObject(SQLCA)

lds_workflow_token_related_mail_package = Create U_DS
lds_workflow_token_related_mail_package.DataObject =  "ds_workflow_token_related_mail_package"
lds_workflow_token_related_mail_package.SetTransObject(SQLCA)

li_inserted_row = lds_workflow_token_related_mail_package.InsertRow(0)


SQLCA.nf_begin_transaction()

IF li_mail_package_rows > 0 THEN
	li_rtn = lds_cancel_mail_package.Update()
	SQLCA.nf_handle_error('w_individual','lds_cancel_mail_package.Update','wf_process_injured_worker_package')
END IF

// this stored procedure inserts into 'WIF' & 'WORKFLOW' tables
li_rtn = lds_create_token.retrieve(al_wif_entity_no, '', '', al_no_minutes_to_expire, 'I', 4)   // two blanks are 'wif_entity_subtype_code' (no subtype for individual) and 'registration_code' which is not required, 'I' is for Individual, as a entity type code, the '4' is harcoded for the secret question identifier (dynamic question)
SQLCA.nf_handle_error('w_individual','lds_create_token.retrieve','wf_process_injured_worker_package')

ls_token_id = lds_create_token.GetItemString(1,'GUID_RETURN')

lds_workflow_token_related_mail_package.SetItem(li_inserted_row,'token_id',         ls_token_id)
lds_workflow_token_related_mail_package.SetItem(li_inserted_row,'package_no',       0)
lds_workflow_token_related_mail_package.SetItem(li_inserted_row,'package_type_code','IR')

li_rtn = lds_workflow_token_related_mail_package.Update()
SQLCA.nf_Handle_error('w_individual','lds_workflow_token_related_mail_package','wf_process_injured_worker_package')


SQLCA.nf_commit_transaction()


ls_exp_datetime =  lds_create_token.getitemstring(li_rtn,'exp_datetime')
MessageBox('Registration Expiry','The Injured Worker Registration was successfully created.' &
                               + '~r~n~r~n' &
										 + 'This registration expires at ' + String(DATETIME(ls_exp_datetime),'YYYY-MM-DD HH:MM') + '.', Information!)

destroy lds_create_token


RETURN 0
end function

event open;call super::open;U_DWA             ldw_dw[]
S_WINDOW_MESSAGE	lstr_message
STRING            ls_eventMessage,ls_eventSpecfic, ls_profile
LONG			      ll_individual_list[]
INTEGER		      li_rtn, li_token_status_code, li_token_rows



	lstr_message = Message.PowerObjectParm
	inv_individual = Create n_individual
	
	idw_individual = tab_individual.tabpage_individual_maintenance.dw_individual
	idw_main_name  = tab_individual.tabpage_individual_maintenance.dw_main_name
	idw_token      = tab_individual.tabpage_injured_worker_registration.dw_token
	
/*	initialize the object
*/
	ldw_dw[1] = idw_individual
	ldw_dw[2] = idw_main_name
	ldw_dw[3] = dw_individual_name
	ldw_dw[4] = dw_next_individual_no
	ldw_dw[5] = dw_claim_list

	inv_individual.nf_init(ldw_dw[],SQLCA,THIS)

/* check if ok
*/
	il_individual_no = iw_active_sheet.dw_basic_claim.GetItemNumber(1,'individual_no')
	inv_individual.nf_retrieve(il_individual_no)
	
//Retrieve the claim list
	ll_individual_list[1] = il_individual_no
	
	dw_claim_list.SetTransObject(SQLCA)
	li_rtn = dw_claim_list.Retrieve(ll_individual_list,0)
	
	SQLCA.nf_handle_error('w_individual','OPEN','Retrieve claim list')
	
	IF li_rtn < 0 Then
		SignalError(-666,'An error occured retrieving the claim list for individual ' + String(il_individual_no))
	End if
	
/* Set claim number
*/
	//	il_claim_no      = iw_active_sheet.dw_basic_claim.GetItemNumber(1,'claim_no')
	il_claim_no = 0 //not applicable for Individual (p10273)
	inv_individual.nf_set_current_claim(il_claim_no)

/*	Put the window into read only mode ?
*/
	IF lstr_message.as_mode = 'READ' THEN
		wf_read_only()
	END IF


SELECT TOP 1
       event_comment,
       event_specific_code  
INTO   :ls_eventMessage,
       :ls_eventSpecfic 
FROM   CLAIM_EVENT
WHERE  event_type_code = '020'
AND    claim_no IN ( SELECT claim_no  
                     FROM   CLAIM
                     WHERE  individual_no = :il_individual_no )   
ORDER BY event_date DESC,
	      event_no   DESC
USING SQLCA;
SQLCA.nf_handle_error('w_individual','embedded SQL: SELECT TOP event_comment,event_specific_code FROM CLAIM_EVENT...','open event')

IF len(trim(ls_eventMessage)) > 0  and idw_individual.rowcount() > 0 then
	idw_individual.setitem(idw_individual.getrow(),'event_desc',ls_eventMessage)
end if


li_token_rows = idw_token.Retrieve(il_individual_no)
SQLCA.nf_handle_error('w_individual','dw_token','open event')

wf_set_iw_tab_picture()




end event

event closequery;call super::closequery;LONG ll_ans

SetPointer(HourGlass!)

IF cb_save.enabled THEN
   ll_ans = MessageBox("Save", 'Data needs to be saved.  Save?', Question!,YesNo!)
   IF ll_ans = 1 THEN
      cb_save.TriggerEvent(Clicked!)
      IF cb_save.enabled THEN
         Message.ReturnValue = 1
      END IF
   END IF
END IF

end event

on close;call w_a_tool::close;Destroy inv_individual
end on

on w_individual.create
int iCurrent
call super::create
this.cb_save=create cb_save
this.cb_cancel=create cb_cancel
this.dw_individual_name=create dw_individual_name
this.dw_next_individual_no=create dw_next_individual_no
this.uo_sin_med_check=create uo_sin_med_check
this.dw_claim_list=create dw_claim_list
this.tab_individual=create tab_individual
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_save
this.Control[iCurrent+2]=this.cb_cancel
this.Control[iCurrent+3]=this.dw_individual_name
this.Control[iCurrent+4]=this.dw_next_individual_no
this.Control[iCurrent+5]=this.uo_sin_med_check
this.Control[iCurrent+6]=this.dw_claim_list
this.Control[iCurrent+7]=this.tab_individual
end on

on w_individual.destroy
call super::destroy
destroy(this.cb_save)
destroy(this.cb_cancel)
destroy(this.dw_individual_name)
destroy(this.dw_next_individual_no)
destroy(this.uo_sin_med_check)
destroy(this.dw_claim_list)
destroy(this.tab_individual)
end on

type st_title from w_a_tool`st_title within w_individual
integer width = 3141
string text = "Maintain Individual"
end type

type cb_close from w_a_tool`cb_close within w_individual
integer x = 1605
integer y = 1696
integer height = 108
integer taborder = 90
end type

type cb_save from commandbutton within w_individual
integer x = 837
integer y = 1696
integer width = 379
integer height = 108
integer taborder = 70
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
end type

event clicked;INT		li_msgbox, li_rtn, li_trancount, li_main_name_row
LONG		ll_return_value, ll_no, ll_cnt
STRING	ls_new_alias_fname, ls_new_alias_lname, ls_new_main_fname, ls_new_main_lname, ls_temp_alias_fname, &
			ls_temp_alias_lname, ls_old_alias_fname, ls_old_alias_lname, ls_old_main_fname, ls_old_main_lname, &
			ls_temp_main_lname, ls_temp_main_fname
DATETIME	ldtm_bday, ldtm_dday
			
N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '010' refers to the Individual Maintenance module, '044' refers to the Payment Processing module
- likewise, '046' refers to the ABCC Eligibility Export module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('010','044','save',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF

//clear variable
li_rtn = 0
li_rtn = ln_process_run_status.nf_in_progress('010','046','save',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF
/******************************************************************************************/

idw_individual.AcceptText()

//T026667 - this datawindow needs an accept test to avoid a problem with alias name vs main name that would occur under specific conditions
idw_main_name.AcceptText()

is_eventdesc = idw_individual.GetItemString(1,"event_desc")
idw_individual.SetItem(1,"event_desc",is_eventdesc)
SetPointer(HourGlass!)

is_Status = wf_set_status()

/*	Check if main or alias names have been changed.  If name(s) have been changed, then check if the alias & main names
	are being switched.  Since SQLServer07 was implemented, users have had duplicate primary key problems when trying
	to switch the alias & main	names for an individual.  The problems are due to how primary keys are now handled.
	In order to avoid a violation of the 'pk_INDIVIDUAL_NAME' primary key constraint, the name switch must be done in
	two stages.
*/
IF ( idw_main_name.GetItemStatus ( idw_main_name.GetRow(), 0, Primary! ) = DataModified! OR &
	  ( idw_individual.GetItemStatus ( 1, "individual_name_last_name", Primary! ) = DataModified! OR &
	    idw_individual.GetItemStatus ( 1, "individual_name_given_names", Primary! ) = DataModified! ) ) THEN

/* Get original alias names from table
*/
	SELECT last_name, given_names
	  INTO :ls_old_alias_lname, :ls_old_alias_fname 
	  FROM INDIVIDUAL_NAME
	 WHERE individual_no = :il_individual_no
	   AND name_type_code = 'A'
	 USING SQLCA ;
 	SQLCA.nf_handle_error('Embedded SQL: SELECT Alias Name FROM INDIVIDUAL_NAME', 'w_individual', 'cb_save clicked')

/* Get original main names from table
*/
	SELECT last_name, given_names
	  INTO :ls_old_main_lname, :ls_old_main_fname
	  FROM INDIVIDUAL_NAME
	 WHERE individual_no = :il_individual_no
	   AND name_type_code = 'M'
	 USING SQLCA ; 
	SQLCA.nf_handle_error('Embedded SQL: SELECT Main Name FROM INDIVIDUAL_NAME', 'w_individual', 'cb_save clicked')
 
	ls_new_alias_lname = idw_individual.GetItemString( 1, 'individual_name_last_name' )
	ls_new_alias_fname = idw_individual.GetItemString( 1, 'individual_name_given_names' )
	ls_new_main_lname = idw_main_name.GetItemString( 1, 'last_name' )
	ls_new_main_fname = idw_main_name.GetItemString( 1, 'given_names' )

//check to see if main last name is null
	IF TRIM(ls_new_main_lname) = '' THEN
		MessageBox('Last Name', 'Last Name is required.',information!)
		Return -1
	END IF
	
//check to see if main first name is null
	IF	TRIM(ls_new_main_fname) = '' THEN
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
	
		PR3936/PR4072/PR4144 - J.M. 2004/06/09 - Only need to change(adding a '.' to the end) the main name when 
		the alias and main are being switched(they are identical), changing both is unnecessary. 
		Only add the '.' to the end if there is a value there, otherwise set it to Null(PR4072).
		If the alias was null and was set to '.', when in the nf_insert_individual_name it sees 
		it as a value and sets that in the dw.  It is actually a null value and should be removed 
		because when it returns from that fxn, it updates the INDIVIDUAL_NAME table using SQL, with the 
		actual value, that being NULL - which is not allowed.	
		Removed the code that gets the alias values since this won't be used anymore. 
	*/
	
		li_main_name_row = idw_main_name.GetRow()
		
		ls_temp_main_lname = idw_main_name.GetItemString(li_main_name_row, 'last_name' ) 
		ls_temp_main_fname = idw_main_name.GetItemString(li_main_name_row, 'given_names' ) 

		/* If the main names are "" then set to null and only add '.' if there is a value to 
	   prevent the app from saving the '.' when there shouldn't be anything there.
	*/
		IF ls_temp_main_lname = "" OR IsNull(ls_temp_main_lname) THEN
			SetNull(ls_temp_main_lname)
			idw_main_name.SetItem(li_main_name_row, 'last_name', ls_temp_main_lname )
		ELSE
			//W02002920 - constraint changed to not allow a  '.'  anymore so instead we will use something different as the temp name during a name switch
			idw_main_name.SetItem(li_main_name_row, 'last_name', 'Name-Change' )
		END IF
	
		IF ls_temp_main_fname = "" OR IsNull(ls_temp_main_fname) THEN
			SetNull(ls_temp_main_fname)
			idw_main_name.SetItem(li_main_name_row, 'given_names', ls_temp_main_fname)
		ELSE 
			idw_main_name.SetItem(li_main_name_row, 'given_names', 'In-Progress' )
		END IF
		
		
		SQLCA.nf_begin_transaction()
		
		ll_return_value = inv_individual.nf_save()
		IF ll_return_value < 0 THEN
			SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
			IF li_trancount > 0 THEN
				SQLCA.nf_rollback_transaction()
			END IF

			idw_main_name.SetItem(li_main_name_row, 'last_name', ls_new_main_lname)
			idw_main_name.SetItem(li_main_name_row, 'given_names', ls_new_main_fname)
			GOTO save_failed
		END IF
		
				
	/*	Do first of two updates - Need to save dummy values first, then save the real values
		during second update in nf_save() below.  This is necessary to avoid a primary key application error due to how
		SQLServer07 deals with the primary key constraint.
	*/
		UPDATE INDIVIDUAL_NAME
		SET    last_name = :ls_new_main_lname, given_names = :ls_new_main_fname
		WHERE  individual_no = :il_individual_no
		AND    name_type_code = 'M'
		USING SQLCA ;
		SQLCA.nf_handle_error('Embedded SQL: INSERT Main Name INTO INDIVIDUAL_NAME', 'w_individual', 'cb_save clicked')

	/* PR3936 - Only update the alias names in the INDIVIDUAL_NAME table if
		they actually have a value. 
	*/		
		IF NOT IsNull(ls_new_alias_lname) AND NOT IsNull(ls_new_alias_fname) THEN
			UPDATE INDIVIDUAL_NAME
			SET    last_name = :ls_new_alias_lname, given_names = :ls_new_alias_fname
			WHERE  individual_no = :il_individual_no
			AND    name_type_code = 'A'
			USING SQLCA ;	
			SQLCA.nf_handle_error('Embedded SQL: INSERT Alias Name INTO INDIVIDUAL_NAME', 'w_individual', 'cb_save clicked')
		END IF
		
		UPDATE INDIVIDUAL
		SET    last_name = :ls_new_main_lname, given_names = :ls_new_main_fname
		WHERE  individual_no = :il_individual_no
		USING  SQLCA ;		
		SQLCA.nf_handle_error('Embedded SQL: INSERT Name INTO INDIVIDUAL', 'w_individual', 'cb_save clicked')
	
		ll_return_value = inv_individual.nf_retrieve(il_individual_no)
		IF ll_return_value < 0 THEN
		/*	If the retrieve fails, then nf_save() will fail below.  The result would be half a transaction - the first
			Update succeeds but the second fails.  The result if the first Update isn't rolled back is that the
			temporary alias names (name + '.') will be stored in the database & will subsequently get displayed in the
			datawindow. The below rollback is issued so that both transactions get reversed ensuring that the names are
			returned to their original values in the database.
		*/
			SQLCA.nf_rollback_transaction()
			Return -1
		END IF
		
	END IF
END IF

// if a transaction was not begun above, then begin one here
SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
IF li_trancount = 0 THEN
	SQLCA.nf_begin_transaction()
END IF

ll_return_value = inv_individual.nf_save()

save_failed:
IF ll_return_value < 0 THEN
   IF ll_return_value = -201 THEN
		// Rollback if there is open txn & there is an error
		SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
		IF li_trancount > 0 THEN
			SQLCA.nf_rollback_transaction()
		END IF
		
   // sin duplicate
		SetPointer(HourGlass!)
      uo_sin_med_check.uf_set_dw('SIN')
      ll_no = idw_individual.GetItemNumber(1,'sin_no')
      ll_cnt = uo_sin_med_check.dw_list.Retrieve(ll_no)
      IF ll_cnt > 0 THEN
      // display warning that individuals exist with the same sin
      // display the list
         uo_sin_med_check.visible = TRUE 
         uo_sin_med_check.dw_list.visible = TRUE
         uo_sin_med_check.sle_message.text = 'The above individual(s) exist with the same SIN.  Please correct.'
         uo_sin_med_check.BringToTop = TRUE
         uo_sin_med_check.dw_list.SetFocus()
			idw_individual.SetItem(1,"event_desc",is_eventdesc)
         Return -1
      END IF 
   ELSEIF ll_return_value = -202 THEN
		// Rollback if there is open txn & there is an error
		SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
		IF li_trancount > 0 THEN
			SQLCA.nf_rollback_transaction()
		END IF
		
   // medicare duplicate
		SetPointer(HourGlass!)
      uo_sin_med_check.uf_set_dw('MED')
      ll_no = idw_individual.GetItemNumber(1,'medicare_no')
      ll_cnt = uo_sin_med_check.dw_list.Retrieve(ll_no)
      IF ll_cnt > 0 THEN
      // display warning that individuals exist with the same medicare
      // display the list
         uo_sin_med_check.visible = TRUE 
         uo_sin_med_check.dw_list.visible = TRUE
         uo_sin_med_check.sle_message.text = 'The above individual(s) exist with the same medicare number.  Please correct.'
         uo_sin_med_check.BringToTop = TRUE
         uo_sin_med_check.dw_list.SetFocus()
			idw_individual.SetItem(1,"event_desc",is_eventdesc)
         Return -1
      END IF 
   ELSEIF ll_return_value = -203 THEN
	/*	Main & alias names are the same (Added for PR 2011, July 2001)
	*/
		SQLCA.nf_rollback_transaction()

		idw_individual.SetColumn('individual_name_last_name')
		idw_individual.SetFocus()
		idw_individual.SetItem(1,"event_desc",is_eventdesc)
		Return -1
	ELSE
		// Rollback if there is open txn & there is an error
		SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
		IF li_trancount > 0 THEN
			SQLCA.nf_rollback_transaction()
		END IF
		RETURN -1
	END IF
ELSE
	SQLCA.nf_commit_transaction()
	
	iw_active_sheet.wf_set_claim(iw_active_sheet.dw_basic_claim.GetItemNumber(1,'claim_no'))
 	cb_cancel.TriggerEvent(Clicked!,0,1)
	
	IF is_status = 'REMOVED' OR is_status = 'CHANGED' OR is_status = 'ENTERED' THEN
		IF wf_insert_event(il_individual_no) < 0 THEN
			Messagebox('Error','An Error occurred while trying to log an event',information!)
		END IF
	END IF
	is_status = ''	
	
	IF trim(inv_individual.is_x001_override_elig_delete) > "" THEN 
		Messagebox("Override Termination Date Removed", inv_individual.is_x001_override_elig_delete, Information!)
	END IF 	
	
END IF
idw_individual.SetItem(1,"event_desc",is_eventdesc)
Return
end event

type cb_cancel from commandbutton within w_individual
integer x = 1221
integer y = 1696
integer width = 379
integer height = 108
integer taborder = 80
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Cance&l"
end type

event clicked;string ls_temp
LONG ll_message

ll_message = message.LongParm

SetPointer(HourGlass!)
ls_temp = trim(idw_individual.getitemString(1,"event_desc"))
inv_individual.nf_retrieve(il_individual_no)
cb_save.enabled = FALSE
cb_cancel.enabled = FALSE
idw_individual.setItem(1,"event_desc",is_eventdesc)

IF ll_message <> 1 THEN
	is_status = ''
END IF	




end event

type dw_individual_name from u_dw_online within w_individual
boolean visible = false
integer x = 37
integer y = 1440
integer width = 101
integer height = 76
integer taborder = 30
string dataobject = "d_individual_name"
end type

type dw_next_individual_no from u_dw_online within w_individual
boolean visible = false
integer x = 151
integer y = 1440
integer width = 105
integer height = 76
integer taborder = 20
string dataobject = "d_next_individual_no"
end type

type uo_sin_med_check from u_check_sin_med within w_individual
boolean visible = false
integer x = 311
integer y = 1180
integer height = 848
integer taborder = 50
end type

on uo_sin_med_check.destroy
call u_check_sin_med::destroy
end on

type dw_claim_list from u_dw_online within w_individual
boolean visible = false
integer x = 59
integer y = 1552
integer width = 128
integer height = 116
integer taborder = 10
string dataobject = "d_claims_for_individual"
end type

type tab_individual from tab within w_individual
integer x = 18
integer y = 108
integer width = 3141
integer height = 1292
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
alignment alignment = center!
integer selectedtab = 1
tabpage_individual_maintenance tabpage_individual_maintenance
tabpage_injured_worker_registration tabpage_injured_worker_registration
end type

on tab_individual.create
this.tabpage_individual_maintenance=create tabpage_individual_maintenance
this.tabpage_injured_worker_registration=create tabpage_injured_worker_registration
this.Control[]={this.tabpage_individual_maintenance,&
this.tabpage_injured_worker_registration}
end on

on tab_individual.destroy
destroy(this.tabpage_individual_maintenance)
destroy(this.tabpage_injured_worker_registration)
end on

event selectionchanged;IF newindex = 2 THEN	
	tab_individual.tabpage_injured_worker_registration.dw_entity_administration.Retrieve(il_individual_no,'I')
END IF
end event

event selectionchanging;
IF cb_save.enabled THEN
	MessageBox('Not Saved','Please save the changes to the claimant before attempting to create a registration.',Exclamation!)
	RETURN 1
ELSE
	RETURN 0
END IF

end event

type tabpage_individual_maintenance from userobject within tab_individual
integer x = 18
integer y = 108
integer width = 3104
integer height = 1168
long backcolor = 67108864
string text = "Individual Maintenance"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_individual dw_individual
dw_main_name dw_main_name
end type

on tabpage_individual_maintenance.create
this.dw_individual=create dw_individual
this.dw_main_name=create dw_main_name
this.Control[]={this.dw_individual,&
this.dw_main_name}
end on

on tabpage_individual_maintenance.destroy
destroy(this.dw_individual)
destroy(this.dw_main_name)
end on

type dw_individual from u_dw_online within tabpage_individual_maintenance
integer y = 28
integer width = 2638
integer height = 1152
integer taborder = 11
string dataobject = "d_individual"
boolean border = false
end type

event itemchanged;call super::itemchanged;Integer li_rtn

cb_save.enabled = TRUE
cb_cancel.enabled = TRUE
uf_set_pbmessage(TRUE)
ib_in_itemchanged = TRUE

IF dwo.name = "individual_court_order_flag" THEN
	this.setitem(row,'event_desc','')
END IF

IF dwo.name = "event_desc" then
	ib_in_itemchanged = FALSE
	Return
END IF

li_rtn = inv_individual.nf_change_item(1)
IF li_rtn = -1 THEN
	RETURN 1
END IF

ib_in_itemchanged = FALSE
end event

event dberror;call super::dberror;RETURN 1

end event

event itemfocuschanged;/*	this is overriden because it screws up validation of edit masked columns
*/
end event

event retrieveend;call super::retrieveend;LONG ll_sin, ll_medicare

IF (rowcount > 0) THEN  //PR19887 2014-11-26 David Worboys Added Extra Protection as this code blew in testing!

	IF isNull(is_eventDesc) or trim(is_eventDesc) = "" then
	ELSE
		this.Setitem(1,"event_desc",is_eventDesc)
	END IF
		
	IF vgst_user_profile.maintain_sin_med_flag = 'N' THEN	
//		ll_sin = dw_individual.GetItemNumber(dw_individual.GetRow(), 'sin_no')
		ll_sin = dw_individual.GetItemNumber(1, 'sin_no')		//PR19887 2014-11-26 David Worboys has to be row 1
		IF ll_sin > 0 THEN
			dw_individual.Object.sin_no.Protect = 1
		END IF
//		ll_medicare = dw_individual.GetItemNumber(dw_individual.GetRow(), 'medicare_no')
		ll_medicare = dw_individual.GetItemNumber(1, 'medicare_no') //PR19887 2014-11-26 David Worboys has to be row 1
		IF ll_medicare > 0 THEN
			dw_individual.Object.medicare_no.Protect = 1
		END IF
	END  IF
END IF  //PR19887 2014-11-26 David Worboys Added Extra Protection
end event

event ue_print;call super::ue_print;LONG ll_job

	ll_job = PrintOpen()
	Parent.Print(ll_job,0,0)
	PrintClose(ll_job)

end event

event updatestart;call super::updatestart;is_eventDesc = this.getitemString(1,"event_desc")
end event

event editchanged;call super::editchanged;
IF dwo.name = 'individual_name_last_name' OR dwo.name ='individual_name_given_names' THEN
	IF len(data) = 1  OR (  len(data) > 1 AND MATCH( MID(data, len(data) - 1, 1) , "[ '-]" ) ) THEN   //looks for a space or a hyphen or a apostrophy
		// this.settext(WordCap(data))  WordCap function works for the space but not the hyphen or apostrophy so manually upper case the last character
		this.settext (  left (data,len(data)-1) + upper(right(data,1)  ))	
		this.SelectText(this.selectedstart() + len(data) + 1, 0)	
	END IF
END IF
end event

type dw_main_name from u_dw_online within tabpage_individual_maintenance
integer x = 27
integer y = 84
integer width = 2501
integer height = 88
integer taborder = 11
boolean bringtotop = true
string dataobject = "d_individual_name_main"
boolean border = false
string icon = "AppIcon!"
end type

event itemchanged;call super::itemchanged;Integer li_rtn

cb_save.enabled = TRUE
cb_cancel.enabled = TRUE
uf_set_pbmessage(TRUE)

li_rtn = inv_individual.nf_change_item(2)
IF li_rtn = -1 THEN
	cb_save.enabled = FALSE		// Need to disable cb_save or will be prompted with msgbox to save data even
										// though no further changes made
	RETURN 1
END IF

end event

event ue_print;call super::ue_print;LONG ll_job

	ll_job = PrintOpen()
	Parent.Print(ll_job,0,0)
	PrintClose(ll_job)

end event

event editchanged;call super::editchanged; IF len(data) = 1  OR (  len(data) > 1 AND MATCH( MID(data, len(data) - 1, 1) , "[ '-]" ) ) THEN   //looks for a space or a hyphen or a apostrophy
	// this.settext(WordCap(data))  WordCap function works for the space but not the hyphen or apostrophy so manually upper case the last character
	this.settext (  left (data,len(data)-1) + upper(right(data,1)  ))	
	this.SelectText(this.selectedstart() + len(data) + 1, 0)	
END IF
end event

type tabpage_injured_worker_registration from userobject within tab_individual
integer x = 18
integer y = 108
integer width = 3104
integer height = 1168
long backcolor = 67108864
string text = "MyServices Registration"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
cb_produce_mail_package cb_produce_mail_package
dw_entity_administration dw_entity_administration
dw_token dw_token
end type

on tabpage_injured_worker_registration.create
this.cb_produce_mail_package=create cb_produce_mail_package
this.dw_entity_administration=create dw_entity_administration
this.dw_token=create dw_token
this.Control[]={this.cb_produce_mail_package,&
this.dw_entity_administration,&
this.dw_token}
end on

on tabpage_injured_worker_registration.destroy
destroy(this.cb_produce_mail_package)
destroy(this.dw_entity_administration)
destroy(this.dw_token)
end on

type cb_produce_mail_package from commandbutton within tabpage_injured_worker_registration
integer x = 18
integer y = 564
integer width = 846
integer height = 104
integer taborder = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Produce Registration Package"
end type

event clicked;DATETIME   ldtm_death_date
LONG       ll_individual_no, ll_no_minutes_to_expire, ll_sin_no, ll_medicare_no
INT        li_rtn, li_token_rows



ldtm_death_date = idw_individual.GetItemDateTime(1,'death_date')

IF IsNull(ldtm_death_date) THEN
	// OK
ELSE
	MessageBox('Deceased','The injured worker is deceased, and must not be registered.',StopSign!)
	RETURN
END IF


ll_sin_no = idw_individual.GetItemNumber(1,'sin_no')
ll_medicare_no = idw_individual.GetItemNumber(1,'medicare_no')

IF ll_sin_no = 0 AND ll_medicare_no = 0 THEN
	MessageBox('No SIN or Medicare','The injured worker has neither the SIN nor the Medicare number that are required to register on MyServices.',StopSign!)
	RETURN
END IF

// set the no minutes for workflow token to expire
ll_no_minutes_to_expire = Long(ProfileString(vgs_ini_filename, "workflow token", "no_minutes_to_expire_invite_iw", '0'))

ll_individual_no = idw_individual.GetItemNumber(1,'individual_no')


li_rtn = wf_process_injured_worker_package(ll_individual_no,ll_no_minutes_to_expire)
IF li_rtn < 0 THEN RETURN


li_token_rows = dw_token.retrieve(ll_individual_no)		
SQLCA.nf_handle_error('w_individual','dw_token','cb_produce_mail_package.clicked')


wf_set_iw_tab_picture()


IF li_token_rows >= 1 THEN
	dw_token.ScrollToRow(1)
	dw_token.SelectRow(1,True)
	dw_token.Trigger Event RowFocusChanged(1)
END IF

end event

type dw_entity_administration from u_dw_online within tabpage_injured_worker_registration
integer x = 18
integer y = 700
integer width = 3058
integer height = 444
integer taborder = 11
string dataobject = "d_group_member"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;THIS.SetTransObject(SQLCA)
THIS.uf_SetSelect(1)
end event

type dw_token from u_dw_online within tabpage_injured_worker_registration
integer x = 18
integer y = 28
integer width = 3058
integer height = 496
integer taborder = 11
string dataobject = "d_individual_token"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;THIS.SetTransObject(SQLCA)
THIS.uf_SetSelect(1)
end event

event clicked;call super::clicked;/*	If the current row is the same as the row the user just clicked,
	a rowfocuschanges will not happen.  Therefore, force one
	so the current employer will be re-retrieved
*/

If This.GetRow() = row THEN
	This.TriggerEvent(rowfocuschanged!)
End IF
end event

