$PBExportHeader$w_maintain_benefit_entitlement.srw
$PBExportComments$Used to maintain the benefit entitlement - inherited from w_checklist
forward
global type w_maintain_benefit_entitlement from w_ancestor
end type
type rb_all from radiobutton within w_maintain_benefit_entitlement
end type
type rb_non_zero from radiobutton within w_maintain_benefit_entitlement
end type
type dw_worker_details from u_dw_online within w_maintain_benefit_entitlement
end type
type uo_checklist from u_checklist within w_maintain_benefit_entitlement
end type
type tab_entitlement from tab within w_maintain_benefit_entitlement
end type
type tabpage_openings from userobject within tab_entitlement
end type
type st_horz_splitbar_opening from u_splitbar_horizontal within tabpage_openings
end type
type dw_opening_detail from u_dw_online within tabpage_openings
end type
type dw_opening_master from u_dw_online within tabpage_openings
end type
type tabpage_openings from userobject within tab_entitlement
st_horz_splitbar_opening st_horz_splitbar_opening
dw_opening_detail dw_opening_detail
dw_opening_master dw_opening_master
end type
type tabpage_payments from userobject within tab_entitlement
end type
type dw_payments from u_dw_online within tabpage_payments
end type
type tabpage_payments from userobject within tab_entitlement
dw_payments dw_payments
end type
type tabpage_awards from userobject within tab_entitlement
end type
type dw_awards from u_dw_online within tabpage_awards
end type
type tabpage_awards from userobject within tab_entitlement
dw_awards dw_awards
end type
type tabpage_overpayment from userobject within tab_entitlement
end type
type dw_overpayment from u_dw_online within tabpage_overpayment
end type
type tabpage_overpayment from userobject within tab_entitlement
dw_overpayment dw_overpayment
end type
type tabpage_claim from userobject within tab_entitlement
end type
type dw_claim_information from u_dw_online within tabpage_claim
end type
type tabpage_claim from userobject within tab_entitlement
dw_claim_information dw_claim_information
end type
type tab_entitlement from tab within w_maintain_benefit_entitlement
tabpage_openings tabpage_openings
tabpage_payments tabpage_payments
tabpage_awards tabpage_awards
tabpage_overpayment tabpage_overpayment
tabpage_claim tabpage_claim
end type
type dw_injured_worker from u_dw_online within w_maintain_benefit_entitlement
end type
type dw_annuity_dates from u_dw_online within w_maintain_benefit_entitlement
end type
type st_vert_splitbar_opening from u_splitbar_vertical within w_maintain_benefit_entitlement
end type
type tab_qualification from tab within w_maintain_benefit_entitlement
end type
type tabpage_entitlement from userobject within tab_qualification
end type
type dw_benefit_entitlement from u_dw_online within tabpage_entitlement
end type
type tabpage_entitlement from userobject within tab_qualification
dw_benefit_entitlement dw_benefit_entitlement
end type
type tabpage_qualification from userobject within tab_qualification
end type
type dw_benefit_qualification from u_dw_online within tabpage_qualification
end type
type tabpage_qualification from userobject within tab_qualification
dw_benefit_qualification dw_benefit_qualification
end type
type tab_qualification from tab within w_maintain_benefit_entitlement
tabpage_entitlement tabpage_entitlement
tabpage_qualification tabpage_qualification
end type
type cb_validate_entitlement from commandbutton within w_maintain_benefit_entitlement
end type
type cb_create from commandbutton within w_maintain_benefit_entitlement
end type
type cb_combine from commandbutton within w_maintain_benefit_entitlement
end type
type cb_split from commandbutton within w_maintain_benefit_entitlement
end type
type cb_refresh from commandbutton within w_maintain_benefit_entitlement
end type
type cb_temp_save from commandbutton within w_maintain_benefit_entitlement
end type
type st_multiple_accounts from statictext within w_maintain_benefit_entitlement
end type
type cb_extract from commandbutton within w_maintain_benefit_entitlement
end type
type cb_add from commandbutton within w_maintain_benefit_entitlement
end type
end forward

global type w_maintain_benefit_entitlement from w_ancestor
integer width = 4855
integer height = 2716
string title = ""
string menuname = "m_annuity"
windowtype windowtype = main!
long backcolor = 67108864
boolean clientedge = true
boolean center = true
long il_design_time_height = 2512
long il_design_time_width = 4718
event ue_post_open ( )
rb_all rb_all
rb_non_zero rb_non_zero
dw_worker_details dw_worker_details
uo_checklist uo_checklist
tab_entitlement tab_entitlement
dw_injured_worker dw_injured_worker
dw_annuity_dates dw_annuity_dates
st_vert_splitbar_opening st_vert_splitbar_opening
tab_qualification tab_qualification
cb_validate_entitlement cb_validate_entitlement
cb_create cb_create
cb_combine cb_combine
cb_split cb_split
cb_refresh cb_refresh
cb_temp_save cb_temp_save
st_multiple_accounts st_multiple_accounts
cb_extract cb_extract
cb_add cb_add
end type
global w_maintain_benefit_entitlement w_maintain_benefit_entitlement

type variables
windowstate		iws_frame_open_state
w_sheet				iw_active_sheet
LONG					il_claim_no, il_individual_no, il_claim_no_dddw[], il_checklist_no
LONG 					il_annuity_account_no, il_annuity_eligibility_no , il_mousemove = 0, il_annuity_payout_no
STRING				is_annuity_eligibility_run_option_code, is_claim_role_code, is_sort
BOOLEAN			ib_payment_list_drag , ib_selected_payment_drag, ib_deleted, ib_checklist_posted, ib_locked
INTEGER               il_action_code
datetime				idtm_annuity_start_date, idtm_annuity_end_date
			
DATASTORE         ids_error_messages
U_DS					ids_bencalcs, ids_openings
u_dw_online 		idw_benefit_entitlement

n_resize inv_tab_resize

ULONG		iul_handle

Protected:
n_transaction	inv_transobj
w_ancestor				iwi_window_parent
boolean					ib_DoCommit, ib_cannot_save_SIN, ib_cannot_save_history, ib_incomplete_annuity_checklist

u_dw_online  			idw_opening_master
u_dw_online 			idw_opening_detail

u_dw_online 			idw_payments
u_dw_online 			idw_payment_totals
u_dw_online 			idw_awards
u_dw_online 			idw_overpayment
u_dw_online 			idw_benefit_qualification

n_maintain_benefit 	inv_maintain_benefit
n_common_annuity 	inv_common_annuity
n_checklist				inv_checklist



end variables

forward prototypes
public subroutine wf_set_datawindows (u_dwa adw_dw[], n_transaction anv_transobj)
public function integer wf_setup_for_verify (long al_individual_no, long al_claim_no, string as_claim_role_code)
public function long wf_get_annuity_account_no (long al_individual_no, long al_claim_no, string as_claim_role_code)
public function integer wf_get_open_checklist (long al_annuity_account_no)
public function integer wf_populate_worker_info (long al_individual_no, string as_claim_no)
public function integer wf_premove_check ()
public function integer wf_select_associated_payments (long al_award_no, integer ai_action_code)
public function integer wf_save_benefit_entitlement ()
public function integer wf_check_benefit_entitlement ()
public function integer wf_combine_payment ()
public function integer wf_add_selected_payments ()
public function integer wf_split_payment (integer ai_split_count)
public function integer wf_refresh_entitlement ()
public function integer wf_populate_claim_array ()
public subroutine wf_populate_claim_dddw ()
public subroutine wf_retrieve_dddw_opening (long al_claim_no)
public function integer wf_delete_benefit_entitlement ()
public function long wf_get_annuity_claim_no (long al_annuity_account_no)
public subroutine wf_pop_info_from_account_info (long al_annuity_account_no)
public subroutine wf_retrieve_dddw_benefit (long al_claim_no)
public function integer wf_set_ann_eligibility_checklist_no (long al_annuity_account_no, long al_annuity_eligibility_no, long al_checklist_no)
public subroutine wf_retrieve_payment_type_dddw (string as_claim_role_code)
public subroutine wf_filter_dddw_opening (long al_claim_no)
public subroutine wf_filter_dddw_benefit_no (long al_claim_no)
public function string wf_get_authorization_filter (long al_claim_no)
public subroutine wf_filter_dddw_payment_type (long al_opening_no, long al_benefit_calculation_no, long al_claim_no)
public subroutine wf_populate_benefit_totals (long al_annuity_account_no)
public subroutine wf_reapply_ben_filter (integer ai_row)
public subroutine wf_insert_error (integer ai_rownum, string as_column_name, string as_error_message)
public function integer wf_other_process_date_check ()
public function integer wf_populate_annuity_dates (long al_annuity_account_no)
public function decimal wf_calculate_amount (integer ai_row, decimal adec_user_defined, integer ai_type)
public subroutine wf_populate_only_claim_dddw ()
public subroutine wf_populate_bencalc_text ()
public subroutine wf_populate_opening_text ()
public function long wf_get_claim_no ()
public function long wf_get_individual_no ()
public function string wf_get_claim_role_code ()
public function string wf_get_module_code ()
public subroutine wf_setrow (integer ai_row)
public subroutine wf_retrieve_dddw_claim_no (long al_individual_no)
public function string wf_retrieve_display_value (integer ai_type, long al_opening_no, long al_claim_no, long al_ben_calc_no)
public subroutine wf_post_resize_checklist_position ()
public subroutine wf_populate_award_name ()
public subroutine wf_print ()
public subroutine wf_print_payments ()
public function boolean wf_check_ss_date_range (date adt_paid_from_date, date adt_paid_to_date)
public function integer wf_populate_calculated_totals (long al_annuity_account_no)
public subroutine wf_populate_ae_status_code (integer ai_rowcount)
public function integer wf_check_unsaved_changes (string as_action)
public function integer wf_populate_worker_details (long al_individual_no)
public function long wf_check_be_for_valid_claim ()
public function w_maintain_benefit_entitlement wf_get_window_reference ()
public function integer wf_set_annuity_payout_checklist_no (long al_checklist_no)
end prototypes

event ue_post_open();LONG		ll_checklist_step_no, ll_claim_in_error
STRING	ls_checklist_step_type_code

//starts the resize service in the ancestor object (Move H,Move V,Grow H, Grow V)
inv_tab_resize.of_setOrigsize( tab_qualification.Width + 8, tab_qualification.Height + 8)
inv_tab_resize.of_register(idw_benefit_entitlement,0,0,100,100)
inv_tab_resize.of_register(idw_benefit_qualification ,0,0,100,100)

IF isvalid(idw_benefit_entitlement) AND isvalid(tab_qualification) THEN
	idw_benefit_entitlement.width 	= tab_qualification.width - 35
	idw_benefit_entitlement.height = tab_qualification.height - 160
	
	idw_benefit_qualification.width 	= tab_qualification.width - 35
	idw_benefit_qualification.height 	= tab_qualification.height - 160
END IF 

/*
it is possible for a user to enter into the module with for the purpose of verifying the benefit entitlement 
and only check off the items in the checklist without ever changing any data.  At this point, the data 
should be validated with regards to the dates to ensure that no other process affected the dates.
*/

IF il_annuity_account_no > 0 THEN 
	 THIS.POST wf_refresh_entitlement()
END IF 

//NOTE: THIS DOESNT LOOK CORRECT!!!!!!!!!!!
ll_checklist_step_no = inv_checklist.nf_max_completed_step(il_checklist_no)

SELECT	checklist_step_type_code
INTO		:ls_checklist_step_type_code
FROM		CHECKLIST_STEP
WHERE	checklist_no = :il_checklist_no
AND		checklist_step_no = :ll_checklist_step_no
USING SQLCA;
SQLCA.nf_handle_error('w_maintain_benefit_entitlement', 'embedded SQL', 'SELECT checklist_step_type_code FROM CHECKLIST_STEP...')

IF ls_checklist_step_type_code = '006' THEN
	// 1.100	The benefit entitlement data must not be changed once the Verify Benefit Entitlement step has been completed.  
	ib_locked = TRUE
END IF

idw_benefit_entitlement.uf_settooltip(TRUE)

/* check to make sure the claims in BE are still valid if Not warn the User and direct them to the claim information page
*/
ll_claim_in_error = wf_check_be_for_valid_claim()

IF ll_claim_in_error > 0 THEN 
	tab_entitlement.SelectTab(5)//claim_information tab
	messagebox('Claim Status Errors', 'Some claim numbers may be in error on the Benefit Entitlement Worksheet. Please review.') //probably want to change this message
END IF 

/*
Although this is a MUST for the Verify step - at this point it is a informational message for the users
we may want to take it out of the post open and only call it here - see BR1.110 & BR 1.120 for details
1.110	The Verify Benefit Entitlement checklist step must not be completed if benefit entitlement spans the annuity start date.
1.120 The Verify Benefit Entitlement checklist step must not be completed if benefit entitlement spans the annuity end date.
*/
wf_other_process_date_check()

end event

public subroutine wf_set_datawindows (u_dwa adw_dw[], n_transaction anv_transobj);/*************************************************************************
	Description:		This function registers datawindows and transaction objects plus, it calls the 
	                       settransobject for the  datawindow using the corresponding element in the
						transaction array.
*************************************************************************/
INT	li_cntr = 1

inv_transobj 	=	anv_transobj
end subroutine

public function integer wf_setup_for_verify (long al_individual_no, long al_claim_no, string as_claim_role_code);/*	Checklist	(From DD)	
	When the maintain benefit entitlement module is opened to verify the entitlement (i.e. opened from another module) 
	the check list will automatically be created if an incomplete check list does not exist.  Once a checklist is created the
	initiating process cannot continue until the Verify Benefit Entitlement checklist is complete.
	
	Replaced with the following...
	1.10	A Verify Benefit Entitlement checklist must only be created if benefit entitlement is created.
	
	Checklist must also fonform to the following rules.
	
	1.10	A Verify Benefit Entitlement checklist must only be created if benefit entitlement is created.
	1.20	At most one incomplete checklist of type Verify Benefit Entitlement must exist at a time for an annuity account.
	1.30	The Verify Benefit Entitlement checklist must not be cancelled.
	1.40	The checklist must be complete before a new Verify Benefit Entitlement checklist can be created for the annuity account.
	1.50	The annuity account must exist prior to creating the Verify Benefit Entitlement checklist for the annuity account.
	1.60	A Verify Benefit Entitlement checklist must contain the following steps:
		·	Identified
		·	Openings/Benefit Calculations
		·	Payments
		·	Awards
		·	Overpayments
		·	Events
		·	Summary Sheet/Document List
		·	Verify Benefit Entitlement
		·	Checklist Completed
	1.70		All Verify Benefit Entitlement steps must be completed except the following:
		·	Events
		·	Summary Sheet/Document List
*/
DATETIME		ldtm_current
INTEGER          li_rtn
LONG			ll_checklist_no, ll_checklist_subscriber_no, ll_related_checklist_no



/* determine if we need to create a new checklist */
ll_checklist_no = wf_get_open_checklist(il_annuity_account_no)
	
IF isnull(ll_checklist_no) OR 	ll_checklist_no = 0 THEN
	
	SQLCA.nf_begin_transaction()

	
	// there must be an existing checklist subscriber
	ll_checklist_subscriber_no = inv_common_annuity.nf_get_ann_acct_checklist_subscriber_no(il_annuity_account_no)
	IF isnull(ll_checklist_subscriber_no) OR ll_checklist_subscriber_no <= 0 THEN //WE have a problem do not continue
		MessageBox('Data Integrity Problem','The checklist subscriber was not created.')
		RETURN -1
	END IF
	
	// get related checklist number, if any exists
	ll_related_checklist_no = inv_common_annuity.nf_get_related_checklist_no(il_annuity_account_no, 'VBE')
 
	//create a new checklist
	ll_checklist_no = inv_checklist.nf_create_checklist('049','VBE','IA','A','INA','A',ll_related_checklist_no)
	
	IF isnull(ll_checklist_no) OR ll_checklist_no <= 0 THEN //WE have a problem do not continue
		MessageBox('Data Integrity Problem','The checklist was not created.')
		RETURN -1
	END IF
	
	//create a new subscriber checklist xref
	inv_checklist.nf_insert_subscriber_checklist_xref(ll_checklist_subscriber_no,ll_checklist_no)
	
	// update CHECKLIST_STEP for 'identified step
	ldtm_current = f_server_datetime()
	li_rtn = inv_checklist.nf_save_checklist_step(ll_checklist_no, '001', 'COA', ldtm_current, vgst_user_profile.user_id, '','049','VBE','A')
	IF li_rtn < 0 THEN
		RETURN -1
	END IF
		
	//set the ANNUITY_ELIGIBITY VBE checklist_no
	wf_set_ann_eligibility_checklist_no(il_annuity_account_no, il_annuity_eligibility_no, ll_checklist_no)
	
	//set the ANNUITY_PAYOUT VBE checklist_no
	wf_set_annuity_payout_checklist_no(ll_checklist_no)
	
	SQLCA.nf_commit_transaction()		
ELSE
	
	// Put cool stuff here
	
END IF 

il_checklist_no = ll_checklist_no

//now retrieve the checklist just created
inv_checklist.nf_retrieve_checklists('VBE',ll_checklist_no)

RETURN 1
end function

public function long wf_get_annuity_account_no (long al_individual_no, long al_claim_no, string as_claim_role_code);LONG ll_annuity_account_no

CHOOSE CASE as_claim_role_code
	CASE 'C'
		
		al_claim_no = 0
		
	CASE 'SS'	
END CHOOSE

//this assumes we don't have an account number TBD
SELECT 	annuity_account_no 
INTO 		:ll_annuity_account_no 
FROM 	ANNUITY_ACCOUNT
WHERE 	individual_no 		= :al_individual_no 
AND 		claim_no 			= :al_claim_no
AND 		claim_role_code 	= :as_claim_role_code
USING    SQLCA;

SQLCA.nf_handle_error('W_maintain_benefit_entitlement','wf_get_annuity_account_no()','SELECT annuity_account_no ')

IF ISNULL(ll_annuity_account_no) OR ll_annuity_account_no < 0 THEN
	ll_annuity_account_no = 0
END IF 

RETURN ll_annuity_account_no


end function

public function integer wf_get_open_checklist (long al_annuity_account_no);/* */
LONG	ll_checklist_no

SELECT	d.checklist_no INTO :ll_checklist_no
FROM		ANNUITY_ACCOUNT					a
JOIN		CHECKLIST_SUBSCRIBER			b ON a.checklist_subscriber_no 	= b.checklist_subscriber_no
JOIN		SUBSCRIBER_CHECKLIST_XREF	c ON b.checklist_subscriber_no 	= c.checklist_subscriber_no
JOIN		CHECKLIST								d ON c.checklist_no 					= d.checklist_no
WHERE	d.checklist_type_code 				= 'VBE'
AND		d.checklist_status_code 				= 'IA'
AND		b.checklist_subscriber_type_code 	= 'ANN'
AND		a.annuity_account_no 				= :al_annuity_account_no
USING SQLCA;

RETURN ll_checklist_no
end function

public function integer wf_populate_worker_info (long al_individual_no, string as_claim_no);/* this function simply fills in the static text boxes at the top of the screen  with the 
    individual_no and the injured workers name
*/
dw_injured_worker.retrieve(al_individual_no, as_claim_no)
SQLCA.nf_handle_error("w_maintain_benefit_entitlement","wf_populate_worker_info()","dw_injured_worker.retrieve(al_individual_no)")

RETURN 1
end function

public function integer wf_premove_check ();/*
If the user selects a payment that was created from an award, they should be able to right click on a payment associated
with an award and select an option that will automatically select all the payments associated with the award. 
****imp ****>>>> 
If the user does not choose this option they should be prompted to see if they want the other award payments 
moved when they try and move the award payment to the benefit entitlement area.

 make sure all records with associated payments are selected
*/

LONG 			ll_award_no, ll_checked_array[], ll_payment_no, ll_claim_no
STRING 		ls_selectable
INTEGER 	li_counter, li_rowcount, li_rtn, li_message, li_checked = 1, li_checked_counter
BOOLEAN 	result,lb_checked

//make sure we have some rows
li_rowcount = idw_payments.rowcount()

IF li_rowcount = 0 THEN RETURN 0

//all correct rows are selected so now see if the user wants to select all associated payments for awards
FOR li_counter = 1 TO li_rowcount
	
	//grab the selectable flag
	//ls_selectable = idw_payments.getitemstring(li_counter,'selectable_flag') (WG EMAIL)
	
	//check the selection value of the row - it shouldn't be selected but just in case...
	result = idw_payments.IsSelected(li_counter)
	
	//IF result = TRUE AND ls_selectable = 'N' THEN (WG EMAIL)
	IF result = TRUE THEN
		//unselect the row
//		idw_payments.selectrow(li_counter,FALSE)  (WG EMAIL)
//	ELSEIF  result = TRUE AND ls_selectable = 'Y' THEN  (WG EMAIL)
		
		//GRAB THE AWARD #/ Payment_no
		ll_award_no     	= idw_payments.getitemnumber(li_counter,'award_no')
		ll_payment_no  	= idw_payments.getitemnumber(li_counter, 'payment_no')
		ll_claim_no       	= idw_payments.getitemnumber(li_counter, 'claim_no')	
		
		lb_checked = FALSE
		FOR li_checked_counter = 1 TO upperbound(ll_checked_array)
			IF ll_award_no = ll_checked_array[li_checked_counter] THEN
				lb_checked = TRUE
				EXIT	
			END IF 
		NEXT
		
		IF ll_award_no > 0  AND lb_checked = FALSE THEN 
			li_rtn = wf_select_associated_payments(ll_award_no, 2)
			
			IF li_rtn = 2 THEN
				//message the user
				li_message = MessageBox('Unselected Payments?','Do you wish to select all Payments associated with the selected Award: ' + string(ll_award_no) + ' ?',Question!,YesNo!,2)
				IF li_message = 1 THEN
					wf_select_associated_payments(ll_award_no, 1)
				END IF 	
				
				ll_checked_array[li_checked] = ll_award_no
				li_checked ++
				
			END IF 
		END IF 
	END IF 		
NEXT	

RETURN 1
end function

public function integer wf_select_associated_payments (long al_award_no, integer ai_action_code);/*
If the user selects a payment that was created from an award, they should be able to right click on a payment associated 
with an award and select an option that will automatically select all the payments associated with the award. 
If the user does not choose this option they should be prompted to see if they want the other award payments 
moved when they try and move the award payment to the benefit entitlement area.

The argument award_no will be 0 when it enters here need to get the row and grab the award from there
unless an award number is provided

IF the action_code = 1 then make the change otherwise it is just a check  
*/

//award_no should be 0 going into this
LONG 			ll_award_no
STRING 		ls_selectable
INTEGER 	li_counter, li_rowcount

//make sure we have some rows
li_rowcount = idw_payments.rowcount()

IF li_rowcount = 0 THEN RETURN 0

//if this function is called from somewhere else it may have an award no - check for that
IF al_award_no > 0 THEN
	ll_award_no = al_award_no
ELSE
	//grab the award_no from the selected row
	ll_award_no = idw_payments.getitemnumber(idw_payments.getrow(),'award_no')
END IF 

//not an award return out
IF ll_award_no <= 0 THEN RETURN 0

//make sure the row is selectable
FOR li_counter = 1 TO li_rowcount
	
	//grab the selectable flag
	ls_selectable = idw_payments.getitemstring(li_counter,'selectable_flag') // (WG EMAIL)
	
	//move onto the next record
	IF ls_selectable = 'N' THEN  CONTINUE //(WG EMAIL)

	//do the selection
	IF idw_payments.getitemnumber(li_counter,'award_no') = ll_award_no THEN
		IF idw_payments.isselected(li_counter) = FALSE THEN
			IF ai_action_code = 1 THEN
				idw_payments.selectrow(li_counter,TRUE)
			ELSE
				RETURN 2 //MESSAGE TO THE CALLING CODE
			END IF
		END IF 	
	END IF 	
NEXT	
	
RETURN 1
end function

public function integer wf_save_benefit_entitlement ();/*
.  New entitlement data will simply be inserted in to the BENEFIT_ENTITLEMENT table and set as active.  
Changed data will be inserted as a new entitlement and the row that was modified will be updated as inactive.
*/
INTEGER 				li_rtn, li_rowcount, li_counter
LONG						ll_benefit_entitlement_no, ll_benefit_entitlement_no_org
STRING                    	ls_deleted_flag
BOOLEAN				lb_check
DWITEMSTATUS     	ldwis_rowstatus
	
li_rowcount = idw_benefit_entitlement.rowcount()

IF li_rowcount<= 0 THEN RETURN 0

//if there are rows we need to make sure there is a valid annuity_account, if there isn't one we need to create it.
il_annuity_account_no = wf_get_annuity_account_no(il_individual_no, il_claim_no, is_claim_role_code)

IF il_annuity_account_no <= 0 THEN 
	il_annuity_account_no = inv_common_annuity.nf_insert_annuity_account(il_individual_no, il_claim_no, is_claim_role_code,0,'N')
END IF 

FOR li_counter = 1 TO li_rowcount
	//CHECK THE ROW STATUS
	ldwis_rowstatus = idw_benefit_entitlement.GetItemStatus(li_counter,0,Primary!)
	IF ldwis_rowstatus = NEWModified! THEN	
		//we need to insert the last number at this point so we don't block anyone.
		ll_benefit_entitlement_no = inv_maintain_benefit.nf_get_next_entitlement_no(1)//number of records to update
		idw_benefit_entitlement.setitem(li_counter,'benefit_entitlement_no',ll_benefit_entitlement_no)	
		
		//set the active flag on the source datawindow 
		idw_benefit_entitlement.setitem(li_counter,'active_flag','Y')		
		
		//put in the account number there may not be one
		idw_benefit_entitlement.setitem(li_counter,'annuity_account_no',il_annuity_account_no)
			
	ELSEIF ldwis_rowstatus = DATAModified! 	THEN
		
		//if the record is deleted DO NOT make another record simply set this record as inactive and.
		ls_deleted_flag = idw_benefit_entitlement.getitemstring(li_counter,'deleted_flag')
		IF ls_deleted_flag = 'Y'  THEN 
			idw_benefit_entitlement.setitem(li_counter,'active_flag','N')		
			CONTINUE
		END IF 
		
		//set the redraw off
		idw_benefit_entitlement.setredraw(FALSE) 
		
		/* removed and replaced with 10.40	A benefit entitlement must not be modified 
		    if the entitlement is referenced in a cross-reference table.
		*/
		
		//grab the original key - to be updated
		ll_benefit_entitlement_no_org = idw_benefit_entitlement.getitemnumber(li_counter,'benefit_entitlement_no')
		IF ISNULL(ll_benefit_entitlement_no_org) OR ll_benefit_entitlement_no_org = 0 THEN RETURN -1
		
		//check for the record in the XREF table 
		lb_check = inv_maintain_benefit.nf_entitlement_xref_exists(il_annuity_account_no, ll_benefit_entitlement_no_org)
		
		IF lb_check = TRUE THEN
				
			/*1	Modify an existing benefit entitlementChange a value and save The benefit entitlement is NOT in an xref table	
					The current row is modified	X	?? is this true??
			   2	Modify an existing benefit entitlementChange a value and save The benefit entitlement is in an xref table	
					The existing data is archived (marked as inactive) and a new row is saved			
			*/		
			ll_benefit_entitlement_no = inv_maintain_benefit.nf_get_next_entitlement_no(1)//number of records to update
						
			//this row gets copied and set as new the sorce row gets set to inactive
			idw_benefit_entitlement.RowsCopy(li_counter,li_counter, Primary!, idw_benefit_entitlement, idw_benefit_entitlement.rowcount() + 1, Primary!)
			idw_benefit_entitlement.setitem( idw_benefit_entitlement.rowcount(),'benefit_entitlement_no',ll_benefit_entitlement_no)			
						
			//cant get the original data back so set the orginal record to NOTMODIFIED and then do an embedded update
			idw_benefit_entitlement.SetItemStatus(li_counter,0,  Primary!, NotModified!)
					
			//now update the original record 
			UPDATE BENEFIT_ENTITLEMENT
			SET active_flag = 'N' WHERE benefit_entitlement_no = :ll_benefit_entitlement_no_org
			USING SQLCA;
			SQLCA.nf_handle_error("w_maintain_benefit_entitlement","wf_save_benefit_entitlement","	UPDATE BENEFIT_ENTITLEMENT")
			
		ELSE
			
			//DON'T NEED TO DO anything just save
			
		END IF
		
	//	set the redraw on
		idw_benefit_entitlement.setredraw(TRUE)
		
	END IF 
NEXT

RETURN 1
end function

public function integer wf_check_benefit_entitlement ();/*
function does all of the validations on the entered Benefit Entitlements
calls the NVO that contains the majority of the BR's
called from cb_validate.

The user will need the ability to validate the data before it is saved.  
Since the user has the ability to move a lot of data over in one move 
it may be confusing to validate the data and present the error back to the user
as the data is being dropped on the screen.  So there will be no validation on the dropped data. 
The user will have the ability to validate the data when they are ready.  The ‘Validate’ button will run
through the business rules and highlight any data that is invalid.  
*/    

INTEGER 		li_rtn, li_counter, li_rowcount, li_error_count, li_error_row, li_count_s1, li_count_s2, li_lump_sum_count, li_error
INTEGER 		li_opening_rowcount, li_opening_counter, li_check_opening_no, li_opening_upperbound, li_openings[], li_bencalcs_for_review[], li_sum_of_days_warning[]
INTEGER 		li_from_day, li_to_day, li_month_diff
INTEGER			li_bencalc_counter, li_upperbound
LONG				ll_benefit_entitlement_no,  ll_claim_no, ll_opening_no, ll_benefit_calculation_no 	
LONG				ll_annuity_account_no, ll_payment_no, ll_row, ll_opening_claim_no
STRING			ls_payment_type_code, ls_payment_sub_type_code,ls_ben_freq_code_code, ls_deleted_flag, ls_active_flag, ls_opening_type
STRING			ls_benefit_category, ls_error_text, ls_ben_freq_code, ls_error_msg, ls_column_name, ls_check, ls_sixty_percent_flag
STRING			ls_pre_1993_flag, ls_bencalc_msg_text
DATETIME	 	ldtm_be_from_date, ldtm_be_to_date, ldtm_deleted_date
DECIMAL {2} 	ldc_be_number_of_months, ldc_be_number_of_weeks, ldec_be_total_award_amount
DECIMAL {2} 	ldc_be_number_of_hours, ldc_be_amount 	, ldc_be_number_of_days, ldec_computed_be_total
DECIMAL {2}    ldc_Daily_rate, ldc_hourly_rate, ldc_weekly_rate, ldc_monthly_rate, ldec_be_award_amount
DECIMAL {2}    ldc_calculated_total, ldec_ben_award_amount, ldec_days_per_week, ldec_hours_per_day
DECIMAL          ldec_three_day, ldec_three_day_check, ldec_test
BOOLEAN		lb_already_checked = FALSE, lb_number_of_months_error, lb_check

			
s_window_message	lstr_message
DWITEMSTATUS     	ldwis_rowstatus
DATAWINDOWCHILD	ldwc_child

SETPOINTER(HOURGLASS!)

//accept whatever is in the datawindow
idw_benefit_entitlement.accepttext()

li_rowcount = idw_benefit_entitlement.rowcount()

//NO ROWS
IF li_rowcount <= 0 THEN RETURN 0

/* create the datastore - if it doesn't already exist */
IF ISVALID(ids_error_messages) THEN DESTROY ids_error_messages

/* (rownum, column name, error text */
ids_error_messages 					= CREATE DATASTORE
ids_error_messages.dataobject 	= 'd_benefit_error_messages'
ids_error_messages.settransobject(sqlca)

//DEFAULT THE ERROR TEXT AND other variables
ls_error_text 			= ''
li_count_s1 				= 0
li_count_s2 				= 0
li_lump_sum_count 	= 0

idw_payments.setredraw(false)

// grab the pre93 flag if applicable
SELECT pre_1993_annuity_eligibility_flag
INTO   :ls_pre_1993_flag
FROM   ANNUITY_ELIGIBILITY
WHERE  annuity_eligibility_no = :il_annuity_eligibility_no
USING SQLCA;
SQLCA.nf_handle_error('w_maintain_benefit_entitlement','embedded SQL: SELECT pre_1993_annuity_eligibility_flag FROM ANNUITY_ELIGIBILITY...','wf_check_benefit_entitlement')



FOR li_counter = 1 TO li_rowcount
	
		// grab all the values from the datawindow
		ll_benefit_entitlement_no 		= idw_benefit_entitlement.getitemnumber(li_counter,'benefit_entitlement_no')
		ll_claim_no 							= idw_benefit_entitlement.getitemnumber(li_counter,'claim_no')
		ll_opening_no 						= idw_benefit_entitlement.getitemnumber(li_counter,'opening_no')
		ll_benefit_calculation_no 		= idw_benefit_entitlement.getitemnumber(li_counter,'benefit_calculation_no')
		ls_payment_type_code 			= idw_benefit_entitlement.getitemstring(li_counter,'payment_type_code')
		ls_payment_sub_type_code 		= idw_benefit_entitlement.getitemstring(li_counter,'payment_sub_type_code')
		ldtm_be_from_date 			    	= idw_benefit_entitlement.getitemdatetime(li_counter,'benefit_entitlement_from_date')
		ldtm_be_to_date 					= idw_benefit_entitlement.getitemdatetime(li_counter,'benefit_entitlement_to_date')
		ldc_be_number_of_months 		= idw_benefit_entitlement.getitemdecimal(li_counter,'number_of_months')
		ldc_be_number_of_weeks 		= idw_benefit_entitlement.getitemdecimal(li_counter,'number_of_weeks')
		ldc_be_number_of_days 			= idw_benefit_entitlement.getitemdecimal(li_counter,'number_of_days')
		ldc_be_number_of_hours 		= idw_benefit_entitlement.getitemdecimal(li_counter,'number_of_hours')
		ldc_be_amount 					= idw_benefit_entitlement.getitemdecimal(li_counter,'benefit_entitlement_amount')
		ls_deleted_flag 					= idw_benefit_entitlement.getitemstring(li_counter,'deleted_flag')
		ldtm_deleted_date 				= idw_benefit_entitlement.getitemdatetime(li_counter,'deleted_date')
		ls_active_flag 						= idw_benefit_entitlement.getitemstring(li_counter,'active_flag')
		ll_payment_no						= idw_benefit_entitlement.getitemnumber(li_counter,'payment_no')
		ls_ben_freq_code					= idw_benefit_entitlement.getitemstring(li_counter,'award_freq_code')
		ldec_be_award_amount          	= idw_benefit_entitlement.getitemdecimal(li_counter,'be_award_amount')
		ldec_three_day          			= idw_benefit_entitlement.getitemdecimal(li_counter,'three_day_withhold_deduction_amount')
		ldec_be_total_award_amount  	= idw_benefit_entitlement.getitemdecimal(li_counter,'be_total_award_amount')
		ldec_computed_be_total          	= idw_benefit_entitlement.getitemdecimal(li_counter,'compute_be_total') 
		ls_sixty_percent_flag               	= idw_benefit_entitlement.getitemstring(li_counter,'sixty_percent_flag') 
		
		ll_annuity_account_no = il_annuity_account_no
		
		/* cases where we need to check unmodified records as well */
		/*	6.160	Active benefit entitlement for a survivor must not include entitlement for both ‘Survivor 60% elected’ (S1 opening) 
	         and ‘Survivor 85% elected’ (S2 opening) benefits. 
			checked at bottom outside of loop
		*/
		
		//grab the opening type if one does not already exist
		IF ll_opening_no > 0 THEN
			ls_opening_type = inv_maintain_benefit.nf_get_opening_type(ll_claim_no, ll_opening_no)
		ELSE
			ls_opening_type = inv_maintain_benefit.nf_get_opening_type_from_combination(ls_payment_type_code)	
		END IF 
				
		CHOOSE CASE ls_opening_type
			CASE 'S1'
				li_count_s1 ++	
			CASE 'S2'
				li_count_s2 ++	
			CASE ELSE
				//HOPEFULLY GETS CAUGHT SOMEWHERE!!!!
		END CHOOSE
		
		//USE IN COUNT BELOW
		IF ls_sixty_percent_flag = 'Y' THEN
			li_lump_sum_count ++
		END IF 
	
		//CHECK THE ROW STATUS
		ldwis_rowstatus = idw_benefit_entitlement.GetItemStatus(li_counter,0,Primary!)
	IF ldwis_rowstatus <> NotModified! THEN
		
		// if it is a deleted record we will still have to do some checks as the user could have modified stuff and 
		// those changes will crash on constraints so perhaps we should move those types of things to the
		// itemchanged event and in here if it is a deleted record go to the next
		IF ls_deleted_flag = 'Y' THEN CONTINUE
		
		// CHECK THE NUMBERS DEFAULT NULLS TO 0
		IF ISNULL(ldec_be_award_amount) 			THEN ldec_be_award_amount 			= 0
		IF ISNULL(ldec_be_total_award_amount) 	THEN ldec_be_total_award_amount 	= 0
		IF ISNULL(ldec_three_day) 						THEN ldec_three_day 					= 0
		IF ISNULL(ldc_be_amount) 						THEN ldc_be_amount 					= 0
		
		//MANDATORY CHECKS - ALL OTHER CHECKS REQUIRE THESE SO FAIL OUT
		IF ISNULL(ll_claim_no) THEN
			ls_error_text = 'A benefit entitlement must  have a claim associated with it.' +&
								'~rPlease select a claim number from the drop down list.'
			wf_insert_error(li_counter, 'claim_no', ls_error_text)
			CONTINUE// GO ONTO NEXT RECORD IN LOOP
		END IF
		
		/* ck_BENEFIT_ENTITLEMENT_03 ' CHECK ( ( benefit_calculation_no > 0 and opening_no > 0 )
               OR ( benefit_calculation_no = 0 and opening_no = 0 ) )
		*/
		IF ll_benefit_calculation_no = 0 AND ll_opening_no > 0 THEN 
			ls_error_text = 'A benefit calculation must be selected if a Opening has been.'
			idw_payments.setredraw(true)
		 	messagebox("Benefit Entitlement Validation", ls_error_text)
			RETURN -1
		ELSEIF ll_opening_no = 0 AND ll_benefit_calculation_no > 0 THEN 
			ls_error_text = 'A Opening must be selected if a benefit calculation has been.'
			idw_payments.setredraw(true)
		 	messagebox("Benefit Entitlement Validation", ls_error_text)
			RETURN -1
		END IF 
		
		IF ISNULL(ls_payment_type_code) OR ls_payment_type_code = '' THEN
			ls_error_text = 'A benefit entitlement must be assigned a payment type.' +&
								'~rPlease select a payment type from the drop down list.'
			idw_payments.setredraw(true)
		 	messagebox("Benefit Entitlement Validation", ls_error_text)
			RETURN -1
		END IF
		
		// 7.30 A benefit entitlement must have a period from date.
		IF inv_maintain_benefit.nf_check_valid_date(ldtm_be_from_date) = -1 THEN 
			ls_error_text = 'A benefit entitlement must have a valid period from date.  Please correct.'
			idw_payments.setredraw(true)
		 	messagebox("Benefit Entitlement Validation", ls_error_text)
			RETURN -1
		END IF
		
		// 7.40 A benefit entitlement must have a period to date.
		IF inv_maintain_benefit.nf_check_valid_date(ldtm_be_to_date) = -1 THEN 
			ls_error_text = 'A benefit entitlement must have a valid period to date.  Please correct.'
			idw_payments.setredraw(true)
		 	messagebox("Benefit Entitlement Validation", ls_error_text)
			RETURN -1
		END IF
		
		// 6.20 Benefit entitlement must only be applicable to injured workers and surviving spouses.	
		IF inv_maintain_benefit.nf_check_benefit_applicable(is_claim_role_code) = -1 THEN 
			ls_error_text = 'Benefit entitlement is only applicable to injured workers and surviving spouses.'
			wf_insert_error(li_counter, 'payment_no', ls_error_text)
		END IF 
		
		//grab the benefit category 
	   	ls_benefit_category = inv_maintain_benefit.nf_get_benefit_category_code(ls_payment_type_code)
		IF	ls_benefit_category = '' THEN
			idw_payments.setredraw(true)
			messagebox("Benefit Entitlement Validation", "The Benefit Category code for benefit calculation number: " + string(ll_benefit_calculation_no) + " is missing. Please select a valid payment type.")
			RETURN -1
		END IF 
		
		/* 6.330	The 60% option lump sum indicator must be ‘No’ if the opening is not a ‘Survivor 60% elected’ opening (i.e. S1 opening).
		*/
		IF ls_opening_type <> 'S1' AND ls_sixty_percent_flag = 'Y' THEN
			ls_error_text = 'The 60% option lump sum indicator must be ‘No’'+&
			                       '~rif the opening is not a ‘Survivor 60% elected’ opening (i.e. S1 opening).'
			wf_insert_error(li_counter, 'opening_type_code', ls_error_text)
		END IF 
		
		/* 	6.340	There must be at most one active benefit entitlement with the 60% option lump sum indicator is set to ‘Yes’.
					checked at bottom outside of loop
					
			8.50 The number of months of a benefit entitlement must be 12 if the 60% option lump sum indicator is set to ‘Yes’.
			8.60  The number of days of a benefit entitlement must be zero if the 60% option lump sum indicator is set to ‘Yes’
		*/
		IF ls_sixty_percent_flag = 'Y' THEN
			IF ldc_be_number_of_months <> 12  THEN
				ls_error_text = 'The number of months of a benefit entitlement must be 12 if the 60% option lump sum indicator is set to ‘Yes’.'
				wf_insert_error(li_counter, 'number_of_months', ls_error_text)
			END IF 
			
			IF ldc_be_number_of_days <> 0 THEN
				ls_error_text = 'The number of days of a benefit entitlement must be zero if the 60% option lump sum indicator is set to ‘Yes’'
				wf_insert_error(li_counter, 'number_of_days', ls_error_text)
			END IF 
		END IF 
				
		// 6.30/ 6.40 An injured worker must have a Social Insurance Number.
		li_rtn = inv_maintain_benefit.nf_check_for_ss_number(il_individual_no, is_claim_role_code)
		IF li_rtn = 0 THEN
			IF lb_already_checked = FALSE THEN
				messagebox("Benefit Entitlement Validation", "A surviving spouse should have a Social Insurance Number.") 
				lb_already_checked = TRUE
			END IF
		END IF 
		
		// 6.50 A payment should exist for the same period as the benefit entitlement (REMOVED AS REQUESTED)
		// 6.60 A payment of the same type as the benefit entitlement should exist. (Removed WG)
		
		// 6.70 An entitlement should not be based on a payment that has been fully adjusted to zero.
		IF inv_maintain_benefit.nf_check_payment_fully_adjusted(ll_payment_no) > 0 THEN 
			messagebox("Benefit Entitlement Validation", "An entitlement should not be created from a payment that has been fully adjusted to zero.")
		END IF
		
		// 6.80 A benefit entitlement period must not span the annuity start date. (REMOVED WG)
		// 6.90 A benefit entitlement period must not span the annuity end date. (REMOVED WG)
		
		//???? The benefit calculation associated with the benefit entitlement should also be associated with a payment. WAS 6.210
		IF ll_benefit_calculation_no > 0 THEN 
			IF inv_maintain_benefit.nf_check_calculation_for_payment(ll_benefit_calculation_no, idw_payments) = 0 THEN 
				
				li_upperbound =  upperbound(li_bencalcs_for_review)
				
				IF isnull(li_upperbound) OR li_upperbound = 0 THEN
					li_bencalcs_for_review[1] = ll_benefit_calculation_no
				END IF 
				
				FOR li_bencalc_counter = 1 TO upperbound(li_bencalcs_for_review)
					
					IF li_bencalcs_for_review[ li_bencalc_counter] = ll_benefit_calculation_no THEN EXIT 
					
					IF li_bencalc_counter =  upperbound(li_bencalcs_for_review) THEN
						li_bencalcs_for_review[ UpperBound(li_bencalcs_for_review) +1] = ll_benefit_calculation_no
						EXIT
					END IF
				NEXT 
			END IF
		END IF 
		
		//8.150	The benefit entitlement benefit amount must be calculated as the total award amount plus the three-day deduction.
		//computed column -- benefit_entitlement
		
		//Benefit entitlement must not be created from a receiving salary payment. WAS 6.220
		IF inv_maintain_benefit.nf_check_if_receiving_salary_payment(ll_payment_no, ll_claim_no) = 0 THEN 
			ls_error_text =  "Benefit entitlement must not be created from a receiving salary payment."
			wf_insert_error(li_counter, 'payment_no', ls_error_text)
		END IF
		
		IF is_claim_role_code = 'C' THEN
			// 6.100 The claim number on the benefit entitlement must be a claim on which the individual is a claimant if the annuity account is for an injured worker.
			IF inv_maintain_benefit.nf_check_associated_benefit_entitlement(il_individual_no, ll_claim_no, 'C') = -1 THEN 
				ls_error_text = 'The claim number on the benefit entitlement must be a claim on which the individual is a claimant' +& 
									'~rif the annuity account is for an injured worker.'
				wf_insert_error(li_counter, 'claim_no', ls_error_text)
			END IF
		
			// 6.110 The individual must be the active claimant on the claim of the benefit entitlement if the individual is an injured worker.
			IF inv_maintain_benefit.nf_check_claim_participant(il_individual_no, ll_claim_no, 'C') = -1 THEN 
				ls_error_text = 'This individual is no longer the injured worker on the claim.' +& 
									'~rPlease review the claimant role and the active claimant indicator in the claim participant module.'			 
				wf_insert_error(li_counter, 'claim_no', ls_error_text)
			END IF
			
			//	6.300	The benefit entitlement payment type, for an injured worker, must be a payment type that requires days. 
			li_rtn = inv_maintain_benefit.nf_check_paytype_requires_days(ls_payment_type_code)
			CHOOSE CASE li_rtn
				CASE 1//payment type has days_hours_flags set to 'Y'
				
				CASE 0//payment type has days_hours_flags set to 'N'
					ls_error_text = 'The benefit entitlement payment type, for an injured worker, must be a payment type that requires days' 
					wf_insert_error(li_counter, 'claim_no', ls_error_text)
				CASE ELSE	//ERROR
					ls_error_text = 'There was an error determining the days_hours_flags on the Payment_Type table for payment type: ' +  ls_payment_type_code
					wf_insert_error(li_counter, 'claim_no', ls_error_text)
			END CHOOSE
		END IF 
		
		// 6.200	A benefit entitlement must be associated with an opening if an opening exists that covers the benefit entitlement period. (NEW)
		IF ll_opening_no = 0 THEN 			
			li_opening_rowcount = idw_opening_master.RowCount()
			
			FOR li_opening_counter = 1 to li_opening_rowcount
				ll_opening_claim_no = idw_opening_master.GetItemNumber(li_opening_counter,'claim_no')
				IF ll_opening_claim_no = ll_claim_no THEN
					li_check_opening_no = idw_opening_master.GetItemNumber(li_opening_counter,'opening_no')
					li_opening_upperbound = UpperBound(li_openings)
					li_openings[li_opening_upperbound+1] = li_check_opening_no
				ELSE
					CONTINUE
				END IF
			NEXT
			
			IF li_opening_upperbound > 0 THEN
				// there are openings
				IF inv_maintain_benefit.nf_check_opening_covers_period(ll_claim_no, li_openings, ldtm_be_from_date, ldtm_be_to_date) > 0 THEN 
					ls_error_text = 'There is an opening that covers the benefit entitlement period.'+&
												 '~rAn opening must be selected for this benefit entitlement.'
					wf_insert_error(li_counter, 'opening_no', ls_error_text)
				END IF 
			END IF
			
			/* 7.80	The benefit entitlement period from date must not be less than the accident date of the associated claim 
		              	if the benefit entitlement is not associated with an opening. (number is now something else in BR doc????)
			*/
			IF DATE(ldtm_be_from_date) < inv_maintain_benefit.nf_get_claim_accident_date(ll_claim_no) THEN
				ls_error_text = 'The period from date must not be before the accident date of the associated claim.' 
				wf_insert_error(li_counter, 'opening_no', ls_error_text)
			END IF 	
			
			IF is_claim_role_code = 'C' THEN
				/* 6.190	A benefit entitlement must be associated with an opening if the benefit entitlement is for an injured worker 
					and the benefit entitlement period from date is on or after April 1, 1999.  (See Rationale) (NEW)
				*/
				IF DATE(ldtm_be_from_date) >= DATE('1999-04-01') AND ll_opening_no = 0 THEN
					ls_error_text = 'All benefit entitlement for an injured worker must have an associated  ' +&
							             '~ropening if the period from date is on or after April 1, 1999.  Please select an opening.'
					wf_insert_error(li_counter, 'opening_no', ls_error_text)
				END IF 	
				
				/* 6.270	The payment type must be a payment type that attracts annuities for an injured worker if the benefit entitlement 
		                      is for an injured worker and the entitlement is not associated with an opening.
				*/
				IF inv_maintain_benefit.nf_check_payment_type_claim_role(ls_payment_type_code, is_claim_role_code ) = FALSE THEN
					ls_error_text = 'The payment type must be a payment type that attracts annuities for an injured worker if the' +&
							             '~rbenefit entitlement is for an injured worker and the entitlement is not associated with an opening.'
					wf_insert_error(li_counter, 'opening_no', ls_error_text)
				END IF 	
				
			ELSE //SS
				
				/* 6.200	A benefit entitlement must be associated with an opening if the benefit entitlement is for a surviving spouse 
					and the benefit entitlement period from date is on or after March 1, 1997.  (See Rationale) (NEW)
				*/
				IF DATE(ldtm_be_from_date) >= DATE('1997-03-01') AND ll_opening_no = 0 THEN
					ls_error_text = 'All benefit entitlement for a surviving spouse must have an associated opening if the'+&
					                      '~rperiod from date is on or after March 1, 1997.  Please select an opening.'
					wf_insert_error(li_counter, 'opening_no', ls_error_text)
				END IF 
				
				/* 6.280	The payment type must be a payment type that attracts annuities for a surviving spouse if the benefit entitlement 
		              is for a surviving spouse and the entitlement is not associated with an opening.
				*/
				IF inv_maintain_benefit.nf_check_payment_type_claim_role(ls_payment_type_code, is_claim_role_code) = FALSE THEN
					ls_error_text = 'The payment type must be a payment type that attracts annuities for a surviving spouse '+&
					                      '~rif the benefit entitlement is for a surviving spouse and the entitlement is not associated with an opening.'
					wf_insert_error(li_counter, 'opening_no', ls_error_text)
				END IF 	
			END IF 
		END IF 	
		
		/* 6.202	The opening must be of type Survivor (SV), if all of the following are true:
			·	benefit entitlement is for a surviving spouse
			·	the benefit entitlement period from date is prior to January 1, 1998.
			·	There is an opening selected
		*/
		IF is_claim_role_code = 'SS' AND  DATE(ldtm_be_from_date) < date('1998-01-01') AND ls_opening_type <> 'SV' AND ll_opening_no <> 0 THEN
			ls_error_text = 'The opening must be of type Survivor (SV), if the benefit entitlement is for a'+&
			                      '~rsurviving spouse and the benefit entitlement period from date is prior to January 1, 1998.'
			wf_insert_error(li_counter, 'opening_no', ls_error_text)
		END IF 

		/* 6.204	The opening must be of type Survivor (SV), if all of the following are true:
			·	benefit entitlement is for a surviving spouse
			·	the benefit entitlement period from date is prior to March 1, 1997 
			.  there is no opening selected for the benefit entitlement.
		*/
		IF is_claim_role_code = 'SS' AND  DATE(ldtm_be_from_date) < date('1997-03-01') AND ls_opening_type <> 'SV' AND ll_opening_no = 0 THEN
			/*
			wg - 2009-09-23
			ls_error_text = 'The opening must be of type Survivor (SV), if the benefit entitlement is for a'+&
			                      '~rsurviving spouse and the benefit entitlement period from date is prior to  March 1, 1997.' +&
							    '~rand there is no opening selected for the benefit entitlement.'
			wf_insert_error(li_counter, 'opening_no', ls_error_text)
			*/
			ls_opening_type = 'SV'
		END IF 
				
		IF is_claim_role_code = 'SS' THEN
			// 6.130 The claim on which the individual is a surviving spouse must be associated with a benefit entitlement if the annuity account is for a surviving spouse.
			IF inv_maintain_benefit.nf_check_associated_benefit_entitlement(il_individual_no,ll_claim_no,'SS') = -1 THEN 
				ls_error_text = 'The claim on which the individual is a surviving spouse must be associated with a benefit entitlement ' +& 
									'~rif the annuity account is for a surviving spouse.'
				wf_insert_error(li_counter, 'claim_no', ls_error_text)
			END IF
		END IF 
	
		// 6.140 Only openings of types that attract annuities for a claimant must be available if the annuity account is for an injured worker.
		IF is_claim_role_code = 'C' AND ll_opening_no > 0 THEN
			IF inv_maintain_benefit.nf_check_benefit_attracts_annuity(is_claim_role_code, ls_opening_type) = -1 THEN 
				ls_error_text = 'Only openings of types that attract annuities for a claimant must be available ' +&
				                      '~rif the annuity account is for an injured worker.'
				wf_insert_error(li_counter, 'opening_no', ls_error_text)
			END IF
		END IF 
		
		/* 6.340	The benefit entitlement award frequency must be the award frequency for the payment type if the 
			benefit entitlement is not associated with a benefit calculation.  See Rationale...
		 	The award frequency can be found in the Payment_Type table if there is no benefit calculation
			associated with the benefit entitlement.
		*/
		IF ll_benefit_calculation_no = 0 THEN
			ls_check = inv_maintain_benefit.nf_get_freq_from_payment_type(ls_payment_type_code)
			IF  ls_check <> ls_ben_freq_code THEN
				ls_error_text = 'A monthly payment type must have a monthly frequency and a weekly payment type' +&
				                      '~rmust have a weekly frequency.'
				wf_insert_error(li_counter, 'award_freq_code', ls_error_text)
			END IF 
		END IF 
		
		/* 6.350 The payment type must have the same benefit level as the benefit calculation,
		             if there is a benefit calculation associated with the benefit entitlement.
					payment type's benefit level %  is in sync with the benefit calculation's benefit level %.	 
						 				 
		*/
		IF ll_benefit_calculation_no <> 0 THEN
			lb_check = inv_maintain_benefit.nf_check_benefit_level_percent(ls_payment_type_code, ll_benefit_calculation_no, ll_claim_no)
			IF  lb_check = FALSE THEN
				ls_error_text = 'The payment type must have the same benefit level as the benefit calculation,' +&
                                         '~rif there is a benefit calculation associated with the benefit entitlement.'
				wf_insert_error(li_counter, 'payment_type_code', ls_error_text)
			END IF 
		END IF 
		
		
	
		//grab the freq of the Benefit_calculation
		//8.70		An award frequency must be provided if the benefit entitlement is not associated with a benefit calculation.
		IF ls_ben_freq_code = '' OR ISNULL(ls_ben_freq_code) THEN
			ls_error_text = 'Please indicate whether this is a monthly or weekly benefit entitlement.'
		 	wf_insert_error(li_counter, 'benefit_calculation_no', ls_error_text)
		END IF
		
		IF ls_ben_freq_code = 'W' THEN 
			// 7.10 A benefit entitlement based on a weekly benefit calculation must not have an entitlement period that spans one of the annuity interest periods (i.e. quarter).
			IF inv_maintain_benefit.nf_check_span_annuity_interest_period(ldtm_be_from_date,ldtm_be_to_date) = -1 THEN 
				ls_error_text = 'Please split the benefit entitlement to indicate which portion belongs to the' +& 
									'~rappropriate annuity interest quarter.  For example, a period of Mar 28 to Apr 11 should be split into' +&
									'~rMar 28 to Apr 1 and Apr 1 to Apr 11 since the annuity interest quarter ends on March 31.'
				wf_insert_error(li_counter, 'benefit_entitlement_from_date', ls_error_text)
			END IF
		ELSEIF ls_ben_freq_code = 'M' THEN 
		
			// 7.20 A benefit entitlement that is based on a monthly benefit calculation must not have a partial month that spans an annuity interest period.
			IF inv_maintain_benefit.nf_check_partial_span_annuity_interest(ldtm_be_from_date,ldtm_be_to_date) = -1 THEN 
				ls_error_text = 'Please split the benefit entitlement to indicate which portion belongs to the appropriate annuity interest quarter.' +& 
								     '~rFor example a period of Mar 28 to May 1 should be split into Mar 28 to Apr 1 and Apr 1 to May 1.'
				wf_insert_error(li_counter, 'benefit_entitlement_from_date', ls_error_text)
			END IF
			
			// 8.90 A benefit entitlement deduction must only be associated with an entitlement based on a weekly award.
			IF ldec_three_day <> 0.00  THEN 
				ls_error_text = 'The three day entitlement deduction is not allowed on a monthly benefit entitlement.'
				wf_insert_error(li_counter, 'three_day_withhold_deduction_amount', ls_error_text)
			END IF 
		END IF
		
		IF ll_opening_no > 0 THEN
			 /* 7.70 A benefit entitlement must not overlap with any other benefit entitlement of the same opening type and claim for the specified annuity account. 
			 TO
			 A benefit entitlement must not overlap with any other benefit entitlement of the same opening type and	 
			 claim for the specified annuity account unless the account is for a surviving spouse and the opening type is a Survivor 60% elected (S1) type. 
			 Revised back to 
			 7.70	A benefit entitlement must not overlap with any other benefit entitlement of the same opening type and	 claim for the specified
			 		annuity account if the annuity account is for an injured worker.
			 */
			IF is_claim_role_code = 'C'  THEN
				li_error =  inv_maintain_benefit.nf_check_entitlement_overlap(ldtm_be_from_date,ldtm_be_to_date,ll_claim_no, li_counter, idw_benefit_entitlement, ls_payment_type_code, ls_opening_type, ll_opening_no,is_claim_role_code) 
				
				IF li_error = -1 THEN
					ls_error_text = 'This benefit entitlement period overlaps with another entitlement period for the ' +& 
										'~rsame claim and opening type.  This is not allowed.  Please verify and adjust.'
					wf_insert_error(li_counter, 'benefit_entitlement_from_date', ls_error_text)
				ELSEIF  li_error = -2 THEN
					ls_error_text = 'Opening_Type could not be determined from the Payment_Combination table for payment type: ' + ls_payment_type_code + '.' +&
										'~rContact the helpdesk with this information.  Failure on BR7.70 for Payment_Combination'
										
					wf_insert_error(li_counter, 'benefit_entitlement_from_date', ls_error_text)
				END IF 
			
			ELSEIF is_claim_role_code = 'SS'  THEN
				IF ls_opening_type = 'S1' AND ls_sixty_percent_flag = 'Y' THEN
					 //DO NOTHING
				ELSE
					/*7.80	A benefit entitlement must not overlap with any other benefit entitlement for a surviving spouse if the annuity account is for a surviving spouse unless all the following are true:
						·	The opening type is a Survivor 60% elected (S1) type 
						·	The overlapping opening has the 60% option lump sum indicator set to ‘Yes’. 
					*/
					li_error =  inv_maintain_benefit.nf_check_entitlement_overlap(ldtm_be_from_date,ldtm_be_to_date,ll_claim_no, li_counter, idw_benefit_entitlement, ls_payment_type_code, ls_opening_type, ll_opening_no, is_claim_role_code)  
							
					IF li_error = -1 THEN
						ls_error_text = 'A benefit entitlement must not overlap with any other benefit entitlement for a surviving spouse.' +& 
												'~rThis is not allowed.  Please verify and adjust.'
						wf_insert_error(li_counter, 'benefit_entitlement_from_date', ls_error_text)
					END IF 		
				END IF 
			END IF 
			
			// 7.90 The benefit entitlement period from date must not be less than the benefit start date the associated opening on the benefit entitlement.'
			IF inv_maintain_benefit.nf_check_benefit_entitlement_dates(date(ldtm_be_from_date),ll_opening_no, ll_claim_no,ls_opening_type, ls_payment_type_code, 1) = -1 THEN 
				ls_error_text = 'The benefit entitlement from date must be on or after the benefit start date of the opening. ' 
				wf_insert_error(li_counter, 'benefit_entitlement_from_date', ls_error_text)
			END IF
			
			// 7.100 The benefit entitlement period from date must not be less than the effective date of the associated benefit calculation for the benefit entitlement.
			IF inv_maintain_benefit.nf_check_benefit_calculation_dates(date(ldtm_be_from_date),ll_benefit_calculation_no,ll_claim_no,ll_opening_no,1) = -1 THEN 
				ls_error_text = 'The benefit entitlement from date must be on or after the effective date of the benefit calculation.' 
				wf_insert_error(li_counter, 'benefit_entitlement_from_date', ls_error_text)
			END IF
			
			// 7.110 The benefit entitlement period from date must not be greater than the benefit end date for the associated opening on the benefit entitlement.
			IF inv_maintain_benefit.nf_check_benefit_entitlement_dates(date(ldtm_be_from_date),ll_opening_no, ll_claim_no,ls_opening_type, ls_payment_type_code, 2) = -1 THEN 
				ls_error_text = 'The benefit entitlement from date must be on or before the benefit end date of the opening.' 
				wf_insert_error(li_counter, 'benefit_entitlement_from_date', ls_error_text)
			END IF
			
			// 7.120 The benefit entitlement period from date must not be before the accident recurrence date of the associated opening on the benefit entitlement.
			IF inv_maintain_benefit.nf_check_benefit_entitlement_dates(date(ldtm_be_from_date),ll_opening_no, ll_claim_no,ls_opening_type, ls_payment_type_code, 3) = -1 THEN 
				ls_error_text = 'The benefit entitlement period from date must not be before the accident recurrence date' +& 
											 'of the associated opening on the benefit entitlement.'
				wf_insert_error(li_counter, 'benefit_entitlement_from_date', ls_error_text)
			END IF
			
			// 7.140 The benefit entitlement period to date must not be greater than the benefit end date of the associated opening on the benefit entitlement.
			IF inv_maintain_benefit.nf_check_benefit_entitlement_dates(date(ldtm_be_to_date),ll_opening_no, ll_claim_no,ls_opening_type, ls_payment_type_code, 4) = -1 THEN  
				ls_error_text = ' The benefit entitlement period to date must not be after the benefit end date of the associated opening.' 
				wf_insert_error(li_counter, 'benefit_entitlement_to_date', ls_error_text)
			END IF 
		END IF 
		
		// 7.130 The benefit entitlement period to date must be greater than the benefit entitlement period from date.
		IF inv_maintain_benefit.nf_check_dates_assorted(date(ldtm_be_from_date),date(ldtm_be_to_date),1) = -1 THEN 
			ls_error_text = 'The benefit entitlement period to date must be after the benefit entitlement period from date.'
		 	wf_insert_error(li_counter, 'benefit_entitlement_to_date', ls_error_text)
		END IF
			
		IF is_claim_role_code = 'C' THEN
			// 7.160 A benefit entitlement must start on or after January 1st, 1991 if the annuity account is for an injured worker. 
			// REVISED TO
			// 7.170	A benefit entitlement must start on or after January 1st, 1991 if the annuity account is for an injured worker 
			// and the injured worker was not eligible for annuity benefits prior to January 1st, 1993. 
			IF ls_pre_1993_flag = 'N' THEN
				IF inv_maintain_benefit.nf_check_dates_assorted(date(ldtm_be_from_date),date(ldtm_be_from_date),4) = -1 THEN 
					ls_error_text = 'Benefit entitlement cannot be before January 1, 1991.' +&
										'~rand the injured worker was not eligible for annuity benefits prior to January 1st, 1993.'
					wf_insert_error(li_counter, 'benefit_entitlement_from_date', ls_error_text)
				END IF
			END IF 
			
			//7.175	A benefit entitlement must start on or after January 1st, 1993 if the annuity account is for an injured worker and 
			//  the injured worker was eligible for annuity benefits prior to January 1st, 1993.  (See Rationale) -- CASE 6
			IF ls_pre_1993_flag = 'Y' THEN
				IF inv_maintain_benefit.nf_check_dates_assorted(date(ldtm_be_from_date),date(ldtm_be_from_date),6) = -1 THEN 
					ls_error_text = 'A benefit entitlement must start on or after January 1st, 1993 if the annuity account ' +&
										'~ris for an injured worker and the injured worker was eligible for annuity benefits' +&
										'~rprior to January 1st, 1993.' 
					wf_insert_error(li_counter, 'benefit_entitlement_from_date', ls_error_text)
				END IF
			END IF 	
		END IF
		
		IF is_claim_role_code = 'SS' THEN
			// 7.180 A benefit entitlement must start on or after January 1st, 1982 if the annuity account is for a surviving spouse.
			IF inv_maintain_benefit.nf_check_dates_assorted(date(ldtm_be_from_date),date(ldtm_be_from_date),5) = -1 THEN 
				ls_error_text = 'Benefit entitlement cannot be before January 1, 1982.' 
				wf_insert_error(li_counter, 'benefit_entitlement_from_date', ls_error_text)
			END IF
			
			// 6.170 Only openings of types that attract annuities for a surviving spouse must be available if the annuity account is for a surviving spouse.
			//NOTE: should also be covered in DW
			IF ll_opening_no > 0 THEN
				IF inv_maintain_benefit.nf_check_benefit_attracts_annuity(is_claim_role_code, ls_opening_type) = -1 THEN 
					ls_error_text = 'Only openings of types that attract annuities for a surviving spouse must be available' +&
												 '~rif the annuity account is for a surviving spouse.'
					wf_insert_error(li_counter, 'opening_no', ls_error_text)
				END IF
			END IF 
			
			// 8.120	A benefit entitlement associated with a surviving spouse must not have a benefit entitlement deduction.
			IF	ldec_three_day <> 0 THEN
				ls_error_text = 'Benefit entitlement deductions are not allowed for a surviving spouse.'
				wf_insert_error(li_counter, 'three_day_withhold_deduction_amount', ls_error_text)
			END IF
			
			// 8.250	The benefit frequency for benefit entitlement for a surviving spouse must be monthly if no benefit calculation is associated with the benefit entitlement.
			IF ll_benefit_calculation_no = 0 AND ls_ben_freq_code <> 'M' THEN 
				ls_error_text = 'Only monthly payment types are allowed for a surviving spouse.' 
				wf_insert_error(li_counter, 'award_freq_code', ls_error_text)
			END IF
		END IF 
		
		// 8.50 At least one of the units: ‘Days’, ‘Hours’, ‘Weeks’, or ‘Months’ entitled must be greater than zero.
		//NOTE: We still need the one displayed but that should only be displayed if all of the columns (days, hours, weeks and months) are zero.

		IF ISNULL(ldc_be_number_of_months) 	THEN  ldc_be_number_of_months 	= 0.00	
		IF ISNULL(ldc_be_number_of_weeks)   	THEN  ldc_be_number_of_weeks 		= 0.00	
		IF ISNULL(ldc_be_number_of_days) 	  	THEN  ldc_be_number_of_days 		= 0.00	
		IF ISNULL(ldc_be_number_of_hours) 		THEN  ldc_be_number_of_hours 		= 0.00	
		
		//ck_BENEFIT_ENTITLEMENT_04
		//(((([benefit_entitlement_number_of_months]+[benefit_entitlement_number_of_weeks])+[benefit_entitlement_number_of_days])+[benefit_entitlement_number_of_hours])>(0))		
		IF  ldc_be_number_of_days <= 0 AND  ldc_be_number_of_hours <= 0 AND  ldc_be_number_of_weeks <= 0  AND ldc_be_number_of_months <= 0 THEN 
			ls_error_text = 'You must enter a value for at least one of the following: days, hours, weeks or months.'
		 	wf_insert_error(li_counter, 'number_of_months', ls_error_text)
		END IF
		
		// 8.60		The ‘Days’ must be greater than zero if entered.
		IF  ldc_be_number_of_days < 0 THEN 
			ls_error_text = 'The ‘Days’ must be greater than zero if entered.'
		 	wf_insert_error(li_counter, 'number_of_days', ls_error_text)
		END IF
		
		// 8.70		The ‘Hours’ must be greater than zero if entered.
		IF  ldc_be_number_of_hours < 0  THEN 
			ls_error_text = 'The ‘Hours’ must be greater than zero if entered.'
		 	wf_insert_error(li_counter, 'number_of_hours', ls_error_text)
		END IF

		// 8.80		The ‘Weeks’ must be greater than zero if entered.
		IF  ldc_be_number_of_weeks < 0  THEN 
			ls_error_text = 'The ‘Weeks’ must be greater than zero if entered.'
		 	wf_insert_error(li_counter, 'number_of_weeks', ls_error_text)
		END IF

		// 8.90		The ‘Months’ must be greater than zero if entered.
		IF  ldc_be_number_of_months < 0 THEN 
			ls_error_text = 'The ‘Months’ must be greater than zero if entered.'
		 	wf_insert_error(li_counter, 'number_of_months', ls_error_text)
		END IF

		// removed WG
		//Hours and days can only be .00 - .25 - .50 - .75
		
		// removed WG
		//Days can only be .00 - .25 - .50 - .75
		
		// 8.80 An award amount must be provided if the benefit entitlement is not associated with a benefit calculation.
		IF ldec_be_award_amount <= 0 THEN 
			ls_error_text = 'Since the benefit entitlement does not have a benefit calculation you must enter an award amount.'
		 	wf_insert_error(li_counter, 'be_award_amount', ls_error_text)
		ELSE
			
			//	8.100	A benefit entitlement deduction must only be 3/5ths of the weekly amount.	
			IF ldec_three_day <> 0.00 THEN
				ldec_three_day_check =  inv_maintain_benefit.nf_calculate_3_fifths(ldec_be_award_amount)
				IF abs(ldec_three_day) <> round(ldec_three_day_check,2) THEN
					ls_error_text = 'A benefit entitlement deduction must only be 3/5ths of the weekly amount.' +&
					                      '~rThe deduction amount, if entered, should be ' + string(round(ldec_three_day_check,2))
					wf_insert_error(li_counter, 'three_day_withhold_deduction_amount', ls_error_text)
				END IF 
			END IF
		END IF
		
		//check based on constraint ck_BENEFIT_ENTITLEMENT_09 -- ([three_day_withhold_deduction_amount]<=(0))
		// 8.110	A benefit entitlement deduction must be stored as a negative amount.
		IF ldec_three_day > 0 THEN
			ls_error_text = 'The three day deduction must be entered as a negative amount.'
		 	wf_insert_error(li_counter, 'three_day_withhold_deduction_amount', ls_error_text)
		END IF 
	
		// 8.60 The benefit entitlement benefit amount must be greater than zero. (computed column nothing in it to start)
		IF ldec_computed_be_total <= 0 THEN 
			ls_error_text = 'The benefit entitlement benefit amount must be greater than zero.'
		 	wf_insert_error(li_counter, 'be_award_amount', ls_error_text)
		END IF
		
		// 6.190 Only benefit calculations associated with the selected opening must be available for selection for the benefit entitlement.
		IF ll_opening_no > 0 THEN	
			IF inv_maintain_benefit.nf_check_opening_with_bencalc(ll_benefit_calculation_no,ll_opening_no, ll_claim_no) = -1 THEN 
				ls_error_text = 'Only benefit calculations associated with the selected opening must be available' +& 
											  '~rfor selection for the benefit entitlement.'
				wf_insert_error(li_counter, 'benefit_calculation_no', ls_error_text)
			END IF
		END IF 
		
		// 6.250 A Payment Type must be associated with a benefit entitlement.
		IF inv_maintain_benefit.nf_check_valid_payment_type(ls_payment_type_code) = -1 THEN 
			ls_error_text = 'You must select a payment type from the drop down list.'
		 	wf_insert_error(li_counter, 'payment_type_code', ls_error_text)
		END IF
		
		/* 7.190	The period from date of a benefit entitlement must be set to the benefit start date of the selected opening 
					if the 60% option lump sum indicator is set to ‘Yes’.
		    7.200	The period to date of a benefit entitlement must be set to one day after the benefit start date of the selected opening 
					if the 60% option lump sum indicator is set to ‘Yes’.
		*/
		IF ls_sixty_percent_flag = 'Y' THEN
			IF inv_maintain_benefit.nf_check_benefit_entitlement_dates(date(ldtm_be_from_date),ll_opening_no, ll_claim_no,ls_opening_type, ls_payment_type_code, 6) = -1 THEN 
				ls_error_text = 'The period from date of a benefit entitlement must be set to the benefit start date of the selected opening' +&
									'~rif the 60% option lump sum indicator is set to ‘Yes’.'
				wf_insert_error(li_counter, 'benefit_entitlement_from_date', ls_error_text)
			END IF
			
			IF inv_maintain_benefit.nf_check_benefit_entitlement_dates(date(ldtm_be_to_date),ll_opening_no, ll_claim_no,ls_opening_type, ls_payment_type_code, 7) = -1 THEN 
				ls_error_text = 'The period to date of a benefit entitlement must be set to one day after the benefit start date of the ' +&
									'~rselected opening if the 60% option lump sum indicator is set to ‘Yes’.'
				wf_insert_error(li_counter, 'benefit_entitlement_to_date', ls_error_text)
			END IF
		END IF
		
			
		/* NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE */
		/* all of the calculations are based on the associated bencalc                */
		/* NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE */
				
		/* 8.130 The benefit entitlement benefit amount must be calculated as follows
		            if a benefit calculation is associated with the benefit entitlement:
			Daily rate 		* number of Days entitled		+
			Hourly rate 		* number of Hours entitled  	+	
			Weekly rate 	* number of Weeks entitled  	+
			Monthly rate 	* number of Months entitled	+
			Benefit Amount, if ‘Days’, ‘Hours’, ‘Weeks’ and ‘Months’ entitled not required
		*/
		
		/*
		I believe I sent you a similar error yesterday.
		This rule is just supposed to be an internal one - the user should not see this message.  
		The code should not calculate the rate it should just take what the user enters.
		(WG)
		 8.180	A daily rate must not be calculated for a benefit entitlement not associated with a benefit calculation.
		 8.200	An hourly rate must not be calculated for a benefit entitlement not associated with a benefit calculation.
		*/

		IF ls_ben_freq_code = 'W' THEN 
			
			/* 	8.160	The daily rate for a weekly benefit must be calculated as follows:
				Benefit Award amount (from benefit calculation) / number of days per week (from benefit calculation)
				NOTE:: INTERNAL CALCULATION - wf_calculate_amount(row,number,case)
     		*/
			
			// 8.280	The months must be zero if the benefit entitlement is based on a weekly frequency award.
			IF ldc_be_number_of_months <> 0 THEN
				ls_error_text = 'You cannot enter a number of months for a weekly award.  Please enter days, hours or weeks.'
		 		wf_insert_error(li_counter, 'number_of_months', ls_error_text)
			END IF
			
			// 8.220	The weekly rate must be the weekly award amount.
			// but this is only to be used in a calculation.  It shouldn't be an error. (WG)
			//NOTE:: INTERNAL CALCULATION - wf_calculate_amount(row,number,case)
			
			// 8.230	The monthly rate must only be applicable to the monthly awards.
			IF ldc_be_number_of_months <> 0 THEN
				ls_error_text = 'The monthly rate must only be applicable to the monthly awards.'
		 		wf_insert_error(li_counter, 'number_of_months', ls_error_text)
			END IF
			
			/* 8.160 The hourly rate for a weekly benefit must be calculated as follows:(Benefit Award amount / number of days per week from the benefit calculation)
						/ number of hours worked per day from the benefit calculation */
			//NOTE:: INTERNAL CALCULATION - wf_calculate_amount(row,number,case)
			
			
			// 8.10 The sum of the days, hours, weeks and months for a benefit entitlement must not exceed the number of days in the benefit entitlement period.
			// reworded to
			// 8.10 The sum of the days, hours and weeks for a benefit entitlement must not exceed the number of days in the benefit entitlement period if the benefit entitlement is associated with a weekly award.
			// msg reworded br remained the same
			
			li_error = inv_maintain_benefit.nf_check_benefit_calculations_days(ldc_be_number_of_days, ldc_be_number_of_hours, ldc_be_number_of_weeks, ldtm_be_from_date, ldtm_be_to_date,ll_claim_no, ll_opening_no, ll_benefit_calculation_no) 
			IF li_error = -2 THEN 
			    li_sum_of_days_warning[UPPERBOUND(li_sum_of_days_warning) + 1 ] = li_counter
					
			ELSEIF li_error = -1 THEN
									
				ls_error_text = 'The sum of the days, hours and weeks must not exceed the actual number of days in the period. ' +&
									 '~rFor example Jan 1 to Jan 15 is 14 days.'
				wf_insert_error(li_counter, 'number_of_days', ls_error_text)		
			END IF 
				
			/* 8.20	A ‘month’ must consist of 30 days for calculation purposes if the benefit entitlement is associated with a monthly award.   See Rationale
						A monthly award is always based on 30 days.  For the month of February where there is less than 30 days, 
						if the entire month is part of the period then assume the month has 30 days.  
						If only a partial month is entered then the number of days will be less than 30 but the days could be 29 – 
						this would still be acceptable for February even though the month may only have 28 days.
			*/
	
		ELSEIF ls_ben_freq_code = 'M' THEN
			
			// 8.350 The number of days must be zero, if the benefit entitlement 
			//       is based on a monthly frequency award and the number of months is greater than zero. (Refer to Rationale)
			// 8.360	The number of months must be zero, if the benefit entitlement 
			//       is based on a monthly frequency award and the number days are greater than zero. (Refer to Rationale)

			IF ldc_be_number_of_days > 0 AND ldc_be_number_of_months > 0 THEN
				ls_error_text = 'For a monthly award, only days OR months can be entered.'
				wf_insert_error(li_counter, 'number_of_days', ls_error_text)
			END IF
			
			SELECT TOP 1 DateDiff(mm,:ldtm_be_from_date,:ldtm_be_to_date)
			INTO   :li_month_diff
			FROM   sysobjects
			USING SQLCA;
			SQLCA.nf_handle_error('w_maintain_benefit_entitlement', 'wf_check_benefit_entitlement', 'SELECT DateDiff...')
			
			
			// 7.210	The benefit entitlement period must fall within only one calendar month, if the benefit entitlement 
			//       is based on a monthly frequency award and the number of days is greater than zero. (Refer to Rationale)
			IF ldc_be_number_of_days > 0 THEN				
				IF li_month_diff = 0 THEN
					// OK - within one calendar month
				ELSEIF li_month_diff = 1 THEN
					IF Day(Date(ldtm_be_to_date)) = 1 THEN
						// OK - B/E period to date = first of subsequent month
					ELSE
						// B/E period to date not first of subsequent month
						ls_error_text = 'The benefit entitlement period must fall within only one calendar month, if the benefit' +&
									    	'~r entitlement is based on a monthly frequency award and the number of days is greater' +&
										 	'~rthan zero. Please correct the entitlement period.'
				 		wf_insert_error(li_counter, 'benefit_entitlement_from_date', ls_error_text)
					END IF
				ELSEIF li_month_diff > 1 THEN
					ls_error_text = 'The benefit entitlement period must fall within only one calendar month, if the benefit' +&
									    	'~rentitlement is based on a monthly frequency award and the number of days is greater' +&
										 	'~rthan zero. Please correct the entitlement period.'
			 		wf_insert_error(li_counter, 'benefit_entitlement_from_date', ls_error_text)
				END IF
			END IF
			
			
			// 8.40	The number of days for a monthly award must be less than 30 days.  See Rationale
			IF ldc_be_number_of_days >= 30 THEN
				ls_error_text = 'For a monthly award, 30 days is a full month and must be indicated as 1 in the month column. '+&
				                       '~rAnything less than 30 days must be entered in the days column.'
				wf_insert_error(li_counter, 'number_of_days', ls_error_text)
			END IF
			
			// 8.260	The hours must be zero if the benefit entitlement is based on a monthly frequency award.
			IF ldc_be_number_of_hours <> 0 THEN 
				ls_error_text = 'Hours must not be entered for monthly entitlement.  Please enter days or months.'
		 		wf_insert_error(li_counter, 'number_of_hours', ls_error_text)
			END IF
			
			// 8.270	The weeks must be zero if the benefit entitlement is based on a monthly frequency award.
			IF ldc_be_number_of_weeks <> 0 THEN
				ls_error_text = 'Weeks must not be entered for monthly entitlement.  Please enter days or months.'
		 		wf_insert_error(li_counter, 'number_of_weeks', ls_error_text)
			END IF
			
			/*  8.30	The sum of the days and months must not exceed the number of months in the period and partial days if a fraction 
						of a month is covered for a monthly award.
						FEB Problem  - only has 28 days
				revised to	
				8.30	The sum of the days and months must not exceed the number of months in the period and partial days if a fraction
				of a month is covered for a monthly award except if the 60% lump sum indicator is set to ‘Yes’.		
			*/
			IF ls_sixty_percent_flag = 'N' THEN
				IF ldc_be_number_of_days + (ldc_be_number_of_months * 30)  > inv_maintain_benefit.nf_calculate_relative_days(ldtm_be_from_date, ldtm_be_to_date)  THEN
					ls_error_text = 'The number of months must not exceed the number of calendar months in the period or the' +&
										 '~rnumber of days must not exceed the number of days within the calendar month, if the period' +&
										 '~rcovers a partial month. A "month" for a monthly award is always based on 30 days. For' +&
										 '~rexample, 2009-02-01 to 2009-03-01 is 1 month, 2009-02-05 to 2009-02-20 is 15 days in February.'
					wf_insert_error(li_counter, 'number_of_months', ls_error_text)
				END IF
			END IF
			
			
			IF ldc_be_number_of_months > 0 AND ls_sixty_percent_flag = 'N' THEN
				// if months are entered, then 'from' and 'to' dates must start on first day of month
				li_from_day = Day(Date(ldtm_be_from_date))
				li_to_day = Day(Date(ldtm_be_to_date))
				
				// 7.220	The benefit entitlement period must start on the first of a month and end on the first of a subsequent month,
				// if the benefit entitlement is based on a monthly frequency award and the number of months is greater than zero. (Refer to Rationale)
				IF li_from_day <> 1 OR li_to_day <> 1 THEN
					ls_error_text = 'The benefit entitlement period must start on the first of a month and'+&
										'~rend on the first of a subsequent month, if the benefit entitlement is'+&
										'~rbased on a monthly frequency award and the number of months is greater than zero.'
					wf_insert_error(li_counter, 'benefit_entitlement_from_date', ls_error_text)
				END IF
				
				// 8.370	The number of months must be the number of calendar months in the benefit entitlement period, 
				//       if the benefit entitlement is based on a monthly frequency award and the number of months
				//       is greater than zero. (Refer to Rationale)
								
				// IF period end date = first day of month, 
				// then DateDiff in months = # calendar months, so do not display message for BR 8.370	
				IF li_to_day = 1 AND li_month_diff <> ldc_be_number_of_months THEN
					lb_number_of_months_error = TRUE
				END IF
								
				// IF period end date <> first day of month, 
				// then DateDiff in months will always return one month less than # calendar months,
				// so display message for BR 8.370
				IF li_to_day <> 1 AND (li_month_diff + 1) <> ldc_be_number_of_months THEN
					lb_number_of_months_error = TRUE				
				END IF
				
				IF lb_number_of_months_error = TRUE THEN
					ls_error_text = 'The number of months must be the number of calendar months in the benefit entitlement'+&
											'~rperiod, if the benefit entitlement is based on a monthly frequency award and'+&
											'~rthe number of months is greater than zero.'
					wf_insert_error(li_counter, 'number_of_months', ls_error_text)
				END IF
				lb_number_of_months_error = FALSE
			END IF
						
			// 8.240	The monthly rate must be the monthly award amount.
			//NOTE:: INTERNAL CALCULATION - wf_calculate_amount(row,number,case)
		
		END IF 
		
		// 6.260 The payment type must be allowable for the type of opening associated with the benefit entitlement.
		IF ll_opening_no > 0 THEN
			IF inv_maintain_benefit.nf_check_payment_type_with_opening(ls_payment_type_code,ls_opening_type) = -1 THEN 
				ls_error_text = 'The payment type must be allowable for the type of opening associated with the benefit entitlement.'
				wf_insert_error(li_counter, 'payment_type_code', ls_error_text)
			END IF
		END IF
		
		// 6.290 The benefit entitlement payment type must be a payment type that attracts annuity.
		IF inv_maintain_benefit.nf_payment_type_attracts_annuity(is_claim_role_code, ls_payment_type_code) = -1 THEN 
			ls_error_text = 'The benefit entitlement payment type must be a payment type that attracts annuity.'
		 	wf_insert_error(li_counter, 'payment_type_code', ls_error_text)
		END IF
		
		// 10.20 The date a benefit entitlement is ‘deleted’ must be recorded. 
		IF ls_deleted_flag = 'Y' AND ISNULL(ldtm_deleted_date) THEN
			ls_error_text = 'The date a benefit entitlement is ‘deleted’ must be recorded.'
		 	wf_insert_error(li_counter, 'deleted_date', ls_error_text)
		END IF

		// 10.30 A deleted entitlement must be inactive.
		IF ls_deleted_flag = 'Y' AND ls_active_flag = 'Y' THEN
			ls_error_text = 'A deleted entitlement must be inactive.'
		 	wf_insert_error(li_counter, 'active_flag', ls_error_text)
		END IF

	ELSE
		CONTINUE
	END IF 
NEXT

/* 6.160	Active benefit entitlement for a survivor must not include entitlement for both ‘Survivor 60% elected’ (S1 opening) 
              and ‘Survivor 85% elected’ (S2 opening) benefits. */
IF li_count_s1 > 0 AND li_count_s2 > 0 THEN 
	ls_error_text = 'Active benefit entitlement for a survivor must not include entitlement for both ‘Survivor 60% elected’' +& 
              			'~r(S1 opening) and ‘Survivor 85% elected’ (S2 opening) benefits'
	wf_insert_error(1, 'opening_type_code', ls_error_text)
END IF 

/* 	6.340	There must be at most one active benefit entitlement with the 60% option lump sum indicator is set to ‘Yes’.
			ONLY CALL ONCE
*/
IF li_lump_sum_count > 1 THEN
	ls_error_text = 'There must be at most one active benefit entitlement with the 60% option lump sum indicator is set to ‘Yes’. ' 
	wf_insert_error(1, 'sixty_percent_flag', ls_error_text)
END IF 


//CHECK THE ERROR COUNT pop the error window open with a summary of errors
IF ids_error_messages.rowcount() >= 1 THEN
	idw_payments.setredraw(TRUE)
	lstr_message.awi_parent_window 		= THIS
	lstr_message.apo_powerobjectparm[1] 	= ids_error_messages
	lstr_message.as_mode 						= "A" //used in header

	openwithparm(w_view_benefit_error_msgs,lstr_message)
	
	RETURN -1	
END IF 

//CHECK and see if there is anything in the becalc review array if there are add them to a message box
// The benefit calculation associated with the benefit entitlement should also be associated with a payment
ls_bencalc_msg_text = ''
IF upperbound(li_bencalcs_for_review) > 0 THEN
	FOR li_bencalc_counter = 1 TO upperbound(li_bencalcs_for_review)
		ls_bencalc_msg_text  = ls_bencalc_msg_text + string(li_bencalcs_for_review[li_bencalc_counter]) + '~r'
	NEXT
	
	 messagebox("Benefit Entitlement Information",'The following benefit calculation associated with the benefit entitlement should also be associated with a payment. ~r' + ls_bencalc_msg_text )

END IF 

/*
	8.15	The sum of the days, hours and weeks for a benefit entitlement should not exceed the number of days in the
             benefit entitlement period based on a twelve-hour day if the benefit entitlement is associated with a weekly award
			and no benefit calculation information is available.

	8.15	This is a warning to the user.  This is only necessary if a benefit calculation does not exist.  
	         Do this check using 12 hours per day to convert the hours to days.
*/
IF upperbound(li_sum_of_days_warning) > 0 THEN
	FOR li_bencalc_counter = 1 TO upperbound(li_sum_of_days_warning[])
		// set text to our information window - otherwise all I have is the row number
		ls_error_text = 'The sum of the days, hours and weeks Should not exceed the actual number of days in the period. ' +&
							'~rFor example Jan 1 to Jan 15 is 14 days. Please Click to review.'
		wf_insert_error(li_sum_of_days_warning[li_bencalc_counter], 'claim_no', ls_error_text)
	NEXT
	
	idw_payments.setredraw(TRUE)
	lstr_message.awi_parent_window 		= THIS
	lstr_message.apo_powerobjectparm[1] 	= ids_error_messages
	lstr_message.as_mode 						= "B" //used in header

	openwithparm(w_view_benefit_error_msgs,lstr_message)
	
END IF 

idw_payments.setredraw(TRUE)
 
RETURN 1
end function

public function integer wf_combine_payment ();/*
If multiple payments are selected the user will be given the option of creating an entitlement for 
each payment selected or for combining all selected payments into one entitlement.

If the user selects to create one entitlement from multiple payments, then the following rules must apply:
·	The payments must all be from the same claim
·	The payments must all be the same type
·	The payments must for the same opening
·	The payments must be for the same benefit calculation
·	There must be no gaps in the payment period – i.e. the from and to date from payment to payment should be continuous
·	Set the benefit entitlement from date to the earliest payment period from date 
·	Set the benefit entitlement to date to the latest payment period to date
*/

// If multiple Payments are selected run the above Payment rules
INTEGER li_rowcount, li_counter, li_selected, li_selected_row, li_first_selected_row, li_inserted_row, li_row, li_calculated_month
INTEGER	li_payment_counter
LONG		ll_claim_no, ll_opening_no, ll_ben_calc_no, ll_payment_no, ll_row
STRING	ls_payment_type, ls_message, ls_payment_sub_type_code, ls_dropvalue, ls_ben_freq_code, ls_txn_type
STRING	ls_for_split
DATE		ldt_payment_start, ldt_payment_end, ldt_effective_from
DECIMAL ldec_hours, ldec_days, ldec_total_award_amount, ldec_net_payment_amount, ldec_total_payment_amount
DECIMAL	ldec_award_amount, ldec_total_deductions
BOOLEAN	ib_deduction_exists

DATASTORE lds_additional_payments
DATASTORE lds_payment_dates_sort


li_rowcount = idw_payments.rowcount()

//make sure we have a row
IF li_rowcount <= 0 THEN RETURN -1

// only allow if the selected tab is the payment tab
IF tab_entitlement.selectedtab <> 2 THEN RETURN -1

// get the selected count
li_selected = inv_maintain_benefit.nf_count_selected_rows(idw_payments)

//make sure we have multiple selected rows
IF li_selected <= 1 THEN RETURN -1
		
//do a premove check to see if they want all associated records selected
IF wf_premove_check() = -1  THEN RETURN -1

//default the ib_deduction to false - if it is true at the end inform the user that deductions existed for 1 of the payments
ib_deduction_exists = FALSE

	
//grab the information
li_first_selected_row 				= idw_payments.GetSelectedRow(0)
ll_claim_no							= idw_payments.getitemnumber(li_first_selected_row, 'claim_no')
ll_opening_no						= idw_payments.getitemnumber(li_first_selected_row, 'opening_no')
ll_ben_calc_no						= idw_payments.getitemnumber(li_first_selected_row, 'benefit_calculation_no')
ls_payment_type					= idw_payments.getitemstring(li_first_selected_row, 'payment_type_code')
ll_payment_no           			= idw_payments.getitemnumber(li_first_selected_row, 'payment_no')
ldec_hours           					= idw_payments.getitemdecimal(li_first_selected_row, 'paid_hours_lost')
ldec_days            					= idw_payments.getitemdecimal(li_first_selected_row, 'paid_days_lost')
ldec_total_award_amount 		= idw_payments.getitemdecimal(li_first_selected_row, 'total_award_amount')
ls_payment_sub_type_code 		= idw_payments.getitemstring(li_first_selected_row, 'payment_sub_type_code')
ls_txn_type                        		= idw_payments.getitemstring(li_first_selected_row, 'txn_type_code')
ldec_net_payment_amount 		= idw_payments.getitemdecimal(li_first_selected_row, 'net_payment_amount')
ldec_total_payment_amount 	= idw_payments.getitemdecimal(li_first_selected_row, 'total_payment_amount')
ldec_total_deductions 			= idw_payments.getitemdecimal(li_first_selected_row, 'total_deductions')


IF ldec_total_deductions > 0 THEN ib_deduction_exists = TRUE

//need to grab the effective_date from the benefit calculation
ldt_effective_from = DATE(inv_maintain_benefit.nf_get_bencalc_eff_date(ll_ben_calc_no, ll_claim_no, ll_opening_no))

IF ISNULL(ls_payment_sub_type_code) THEN ls_payment_sub_type_code = ' ' 

/* ·	Set the benefit entitlement from date to the earliest payment period from date 
	·	Set the benefit entitlement to date to the latest payment period to date
*/
lds_payment_dates_sort = CREATE DATASTORE

lds_payment_dates_sort.dataobject = 'ds_payment_dates_for_sort'
lds_payment_dates_sort.settransobject(sqlca)

FOR li_counter = 1 TO li_rowcount
	IF idw_payments.IsSelected(li_counter) = TRUE THEN
		
		//GRAB THE PAYMENT START AND END DATE
		ldt_payment_start 	= DATE(idw_payments.getitemdatetime(li_counter, 'paid_from_date'))
		ldt_payment_end	= DATE(idw_payments.getitemdatetime(li_counter, 'paid_to_date'))
		
		li_row = lds_payment_dates_sort.insertrow(0)
		lds_payment_dates_sort.setitem(li_row,'payment_start_date', ldt_payment_start)
		lds_payment_dates_sort.setitem(li_row,'payment_end_date', ldt_payment_end)
		
	END IF 
NEXT

//do a sort on the datastore to get the dates
lds_payment_dates_sort.SetSort("paid_from_date asc")
lds_payment_dates_sort.Sort()

//There must be no gaps in the payment period – i.e. the from and to date from payment to payment should be continuous
FOR li_counter = 1 TO lds_payment_dates_sort.rowcount()
	
	//MAKE SURE WE HAVE valid rows
	IF li_counter + 1 > lds_payment_dates_sort.rowcount() THEN EXIT
	
	//grab the start payment period for the next payment - must be the next day
	ldt_payment_start 	= lds_payment_dates_sort.getitemdate(li_counter + 1, 'payment_start_date')
	ldt_payment_end 	= lds_payment_dates_sort.getitemdate(li_counter, 'payment_end_date')
	
	IF DaysAfter(ldt_payment_end, ldt_payment_start  ) > 2 THEN
		ls_message = 'There must be no gaps in the payment period – i.e. the from and to date from payment to payment should be continuous'
		EXIT
	END IF 	
NEXT

//GRAB THE START DATE
ldt_payment_start = lds_payment_dates_sort.getitemdate(1, 'payment_start_date')

//reset the sort
lds_payment_dates_sort.SetSort("")
lds_payment_dates_sort.Sort()

//do a sort on the datastore to get the dates
lds_payment_dates_sort.SetSort("paid_to_date D")
lds_payment_dates_sort.Sort()

//GRAB THE END DATE
ldt_payment_end = lds_payment_dates_sort.getitemdate(lds_payment_dates_sort.rowcount(), 'payment_end_date')
	
li_selected_row = 0
DO WHILE TRUE

	/* Get the next selected row.*/
   	li_selected_row = idw_payments.GetSelectedRow(li_selected_row)
	IF li_selected_row = 0 THEN
		EXIT
	END IF
	
	//use the above as our base data - now check each record
	//	The payments must all be from the same claim
	IF idw_payments.getitemnumber(li_selected_row, 'claim_no') <> ll_claim_no THEN
		ls_message = 'The payments must all be from the same claim'
		EXIT//ERROR
	END IF 
		
	//The payments must for the same opening
	IF idw_payments.getitemnumber(li_selected_row, 'opening_no') <> ll_opening_no THEN
		ls_message = 'The payments must for the same opening'
		EXIT//ERROR
	END IF 
		
	//The payments must be for the same benefit calculation
	IF idw_payments.getitemnumber(li_selected_row, 'benefit_calculation_no')	<> ll_ben_calc_no THEN
		ls_message = 'The payments must be for the same benefit calculation'
		EXIT//ERROR
	END IF 
		
	//The payments must all be the same type
	IF idw_payments.getitemstring(li_selected_row, 'payment_type_code') <> ls_payment_type THEN
		ls_message = 'The payments must all be the same type'
		EXIT//ERROR
	END IF 
	
	//GRAB THE deduction amount
	ldec_total_deductions = idw_payments.getitemdecimal(li_selected_row, 'total_deductions')
	IF ldec_total_deductions > 0 THEN ib_deduction_exists = TRUE
			
LOOP
	
IF trim(ls_message) <> '' THEN
	//THERE WAS AN error - msg and get out
	MESSAGEBOX('Benefit Entitlement Creation',ls_message)
	RETURN -1
END IF

//if applicable message the user about the deductions
IF  ib_deduction_exists = TRUE THEN
	MESSAGEBOX('Combine Benefit Entitlement','Deductions existed for some of the combined payments, Please Review.')
END IF 

//IF IT PASSES all the rules turn off the selectablility
FOR li_counter = 1 TO li_rowcount
	IF idw_payments.IsSelected(li_counter) = TRUE THEN
	
		//disable the selected payment so that it can no longer be selected
		idw_payments.setitem(li_counter,'selectable_flag','N')
		
	END IF 
NEXT
		
//grab the freq information
ls_ben_freq_code = inv_maintain_benefit.nf_get_ben_freq_code(ll_claim_no,ll_ben_calc_no, ll_opening_no)
IF isnull(ls_ben_freq_code) THEN ls_ben_freq_code = ''

// if there is no frequency code, then get code from Payment_Type table
IF ls_ben_freq_code = '' THEN
	// look at payment type's frequency
	SELECT	award_freq_code
	INTO		:ls_ben_freq_code
	FROM		Payment_Type
	WHERE	payment_type_code = :ls_payment_type
	USING SQLCA;
	SQLCA.nf_handle_error('w_maintain_benefit_entitlement', 'wf_combine_payment', 'EMBEDDED SQL: SELECT award_freq_code FROM Payment_Type...')
END IF

/*	7)Defaulting days, hours, weeks and months -- 
    		When you drag over a payment and the frequency is monthly - the payment will have days - 
		can we take the days and divide by 30 to get months and default the months to this value - 
		anything left over will go into the days column.  For example 65 day would be 2 months and 5 days
		(new functionality)
*/
IF ls_ben_freq_code = 'M' THEN
	li_calculated_month = int(ldec_days /30)
END IF 

//need to grab the award amount and put it in the BE award amount column on the BE datawindow
ldec_award_amount = inv_maintain_benefit.nf_get_bc_award_amount(ll_claim_no,ll_ben_calc_no, ll_opening_no)

//see if we need to split this record
lds_additional_payments = inv_maintain_benefit.nf_split_payment_for_interest_period(ldt_payment_start, ldt_payment_end, ls_ben_freq_code)

/************* create the new entitlement ***************************/
FOR li_payment_counter = 1 TO lds_additional_payments.ROWCOUNT()
	
	//grab the dates from the datastore we dont care about anything else
	ldt_payment_start 	= lds_additional_payments.getitemdate(li_payment_counter,'annuity_date_from')
	ldt_payment_end  	= lds_additional_payments.getitemdate(li_payment_counter,'annuity_date_to')
	ls_for_split  			= lds_additional_payments.getitemstring(li_payment_counter,'for_split')
			
	//insert a row into the benefit entitlement datawindow 
	li_inserted_row =  idw_benefit_entitlement.insertrow(0)
			
	//default the values			
	idw_benefit_entitlement.setitem(li_inserted_row,'opening_no', ll_opening_no)
	idw_benefit_entitlement.setitem(li_inserted_row,'claim_no', ll_claim_no)
	idw_benefit_entitlement.setitem(li_inserted_row,'benefit_calculation_no', ll_ben_calc_no)
	idw_benefit_entitlement.setitem(li_inserted_row,'payment_no', ll_payment_no)
	idw_benefit_entitlement.setitem(li_inserted_row,'payment_type_code', ls_payment_type)
	idw_benefit_entitlement.setitem(li_inserted_row,'benefit_entitlement_from_date', ldt_payment_start)
	idw_benefit_entitlement.setitem(li_inserted_row,'benefit_entitlement_to_date', ldt_payment_end)
	idw_benefit_entitlement.setitem(li_inserted_row,'payment_sub_type_code', ' ')
	idw_benefit_entitlement.setitem(li_inserted_row,'effective_from_date', ldt_effective_from)
	idw_benefit_entitlement.setitem(li_inserted_row,'number_of_hours', ldec_hours)
	idw_benefit_entitlement.setitem(li_inserted_row,'number_of_days', ldec_days)
	//idw_benefit_entitlement.setitem(li_inserted_row,'benefit_entitlement_amount', ldec_payment_amount) (computed)
	idw_benefit_entitlement.setitem(li_inserted_row,'payment_sub_type_code',' ')
	idw_benefit_entitlement.setitem(li_inserted_row,'award_freq_code',ls_ben_freq_code)
	idw_benefit_entitlement.setitem(li_inserted_row, 'be_award_amount', ldec_award_amount)
	
	/* 8) When splitting a payment automatically over the Annuity interest period (which works great) 
			do not default the days, hours, months or benefit amount.  Let the user enter them.  (new functionality)
	*/
	IF ls_for_split = 'Y' THEN 
		idw_benefit_entitlement.setitem(li_inserted_row,'number_of_hours', 0.00)
		idw_benefit_entitlement.setitem(li_inserted_row,'number_of_days', 0.00)
		idw_benefit_entitlement.setitem(li_inserted_row,'number_of_months', 0)
		idw_benefit_entitlement.setitem(li_inserted_row,'be_total_award_amount', 0.00)  
	ELSE//DEFAULT THE VALUES - NOT SPLIT
		idw_benefit_entitlement.setitem(li_inserted_row,'number_of_hours', ldec_hours)
		idw_benefit_entitlement.setitem(li_inserted_row,'number_of_days', ldec_days)
				
		//calculate the total award amount
		ldec_total_award_amount = wf_calculate_amount(li_inserted_row, 0, 5)
				
		/* 2) If the transaction type 1 has been adjusted - default the benefit entitlement amount to zero 
		(may be if the net amount <> total payment amount)
		*/
		IF ls_txn_type = '1' AND ldec_net_payment_amount <> ldec_total_payment_amount  THEN 
			ldec_total_award_amount = 0.00
		END IF 
			
		IF ISNULL(ldec_total_award_amount) OR ldec_total_award_amount < 0 THEN ldec_total_award_amount = 0.00
		idw_benefit_entitlement.setitem(li_inserted_row,'be_total_award_amount', ldec_total_award_amount)  
	END IF 
	
	/********* OPENING TEXT ********************/
	//wf_retrieve_display_value(1,opening_no,claim_no,bencalc)
	ls_dropvalue = wf_retrieve_display_value(1, ll_opening_no, ll_claim_no, 0)
		
	IF ls_dropvalue <> '' THEN
		idw_benefit_entitlement.setitem(li_inserted_row,'opening_text',ls_dropvalue)
	ELSE
		idw_benefit_entitlement.setitem(li_inserted_row,'opening_text','0')
	END IF 
						
	/********* BENCALC TEXT ********************/
	ls_dropvalue = wf_retrieve_display_value(2, ll_opening_no, ll_claim_no, ll_ben_calc_no)
			
	IF ls_dropvalue <> '' THEN
		idw_benefit_entitlement.setitem(li_inserted_row,'bencalc_text',ls_dropvalue)
	ELSE
		idw_benefit_entitlement.setitem(li_inserted_row,'bencalc_text','0')
	END IF 

NEXT
	
//scroll and set the row
idw_benefit_entitlement.SelectRow(0, FALSE)
idw_benefit_entitlement.SetRow(li_inserted_row)
idw_benefit_entitlement.SelectRow(li_inserted_row, TRUE)
idw_benefit_entitlement.scrolltorow(li_inserted_row)

RETURN 1
end function

public function integer wf_add_selected_payments ();/*
If multiple payments are selected the user will be given the option of creating an entitlement for 
each payment selected or for combining all selected payments into one entitlement.
*/

//grab the selected count to see if this is valid
//if there is only 1 row simply call the create button
cb_create.triggerevent('clicked')

RETURN 1
end function

public function integer wf_split_payment (integer ai_split_count);/*
To split a payment into multiple entitlements only one payment may be selected at a time.  
The user will need to be prompted for the number of entitlement rows to create.  
Based on the input from the user create the number of entitlement entries specified and
default all rows to the same claim number, benefit calculation, opening number and payment type 
as the selected payment.
*/
INTEGER li_rtn, li_selected_row, li_counter, li_inserted_row
LONG		ll_claim_no, ll_opening_no, ll_ben_calc_no, ll_benefit_entitlement_no, ll_row
STRING	ls_payment_type, ls_payment_sub_type_code, ls_dropvalue, ls_ben_freq_code, ls_txn_type
DECIMAL ldec_hours, ldec_days, ldec_payment_amount, ldec_total_award_amount, ldec_net_payment_amount
DECIMAL	ldec_total_payment_amount, ldec_award_amount 
DATE		ldt_effective_from


//ensure there is only 1 record selected
li_rtn = inv_maintain_benefit.nf_count_selected_rows(idw_payments)

IF li_rtn <> 1 THEN
	MESSAGEBOX('Split Payments', 'Only 1 payment can be selected, payment cannot be split.')
	RETURN -1
END IF

//ensure the value passed in is valid -- probably need other checks here i.e. (# of splits that can be created)
IF isnull(ai_split_count) OR ai_split_count < 1  THEN 
	MESSAGEBOX('Error in Split Count', 'The number or Benefit Calculations to be created is not Valid. Please Review.')
	RETURN -1
END IF

/* Get selected row to grab defaulted data from.*/
li_selected_row     = idw_payments.GetSelectedRow(0)
IF li_selected_row = 0 THEN RETURN -1 //already checked above

//grab the information from the payment datawindow
ll_claim_no							= idw_payments.getitemnumber(li_selected_row, 'claim_no')
ll_opening_no						= idw_payments.getitemnumber(li_selected_row, 'opening_no')
ll_ben_calc_no						= idw_payments.getitemnumber(li_selected_row, 'benefit_calculation_no')
ls_payment_type					= idw_payments.getitemstring(li_selected_row, 'payment_type_code')
ls_payment_sub_type_code 		= idw_payments.getitemstring(li_selected_row, 'payment_sub_type_code')
ldec_hours           					= idw_payments.getitemdecimal(li_selected_row, 'paid_hours_lost')
ldec_days            					= idw_payments.getitemdecimal(li_selected_row, 'paid_days_lost')
ldec_payment_amount 			= idw_payments.getitemdecimal(li_selected_row, 'total_award_amount')
ls_txn_type                        		= idw_payments.getitemstring(li_selected_row, 'txn_type_code')
ldec_net_payment_amount 		= idw_payments.getitemdecimal(li_selected_row, 'net_payment_amount')
ldec_total_payment_amount 	= idw_payments.getitemdecimal(li_selected_row, 'total_payment_amount')


IF isnull(ll_opening_no) 	THEN ll_opening_no 	= 0
IF isnull(ll_ben_calc_no) 	THEN ll_ben_calc_no 	= 0

IF isnull(ls_payment_sub_type_code) THEN ls_payment_sub_type_code = ' '	

//grab the freq code if applicable
ls_ben_freq_code = inv_maintain_benefit.nf_get_ben_freq_code(ll_claim_no,ll_ben_calc_no, ll_opening_no)
IF isnull(ls_ben_freq_code) THEN ls_ben_freq_code = ''

// if there is no frequency code, then get code from Payment_Type table
IF ls_ben_freq_code = '' THEN
    ls_ben_freq_code = inv_maintain_benefit.nf_get_freq_from_payment_type(ls_payment_type)
END IF

//need to grab the award amount and put it in the BE award amount column on the BE datawindow
ldec_award_amount = inv_maintain_benefit.nf_get_bc_award_amount(ll_claim_no,ll_ben_calc_no, ll_opening_no)

//need to grab the effective_date from the benefit calculation
ldt_effective_from = DATE(inv_maintain_benefit.nf_get_bencalc_eff_date(ll_ben_calc_no, ll_claim_no, ll_opening_no))
	
//disable the selected payment so that it can no longer be selected
idw_payments.setitem(li_selected_row,'selectable_flag','N')

// create a new entitlement for the count selected by the user in the popup
/* 38	B/E – on a split do not default # days, hours, months – set to 0 */
FOR li_counter = 1 TO ai_split_count
	
	//insert a row into the benefit entitlement datawindow 
	li_inserted_row =  idw_benefit_entitlement.insertrow(0)
	
	//for now create this as though it is just one record
	idw_benefit_entitlement.setitem(li_inserted_row,'benefit_entitlement_no',ll_benefit_entitlement_no)
	idw_benefit_entitlement.setitem(li_inserted_row,'opening_no', ll_opening_no)
	idw_benefit_entitlement.setitem(li_inserted_row,'claim_no', ll_claim_no)
	idw_benefit_entitlement.setitem(li_inserted_row,'benefit_calculation_no', ll_ben_calc_no)
	idw_benefit_entitlement.setitem(li_inserted_row,'payment_type_code', ls_payment_type)
	idw_benefit_entitlement.setitem(li_inserted_row,'payment_sub_type_code',' ')
	idw_benefit_entitlement.setitem(li_inserted_row,'number_of_hours', 0)//MODIFIED TO 0 pi
	idw_benefit_entitlement.setitem(li_inserted_row,'number_of_days', 0)//MODIFIED TO 0 pi
	idw_benefit_entitlement.setitem(li_inserted_row,'award_freq_code',ls_ben_freq_code)
	idw_benefit_entitlement.setitem(li_inserted_row, 'be_award_amount', ldec_award_amount)
	idw_benefit_entitlement.setitem(li_inserted_row,'effective_from_date', ldt_effective_from)
	//idw_benefit_entitlement.setitem(li_inserted_row,'benefit_entitlement_amount', ldec_payment_amount) (computed)
	
	//calculate the total award amount
	ldec_total_award_amount = wf_calculate_amount(li_inserted_row, 0, 5)
	
	/* 2) If the transaction type 1 has been adjusted - default the benefit entitlement amount to zero 
       (may be if the net amount <> total payment amount)
	*/
	IF ls_txn_type = '1' AND ldec_net_payment_amount <> ldec_total_payment_amount  THEN 
		ldec_total_award_amount = 0.00
	END IF 
			
	IF ISNULL(ldec_total_award_amount) OR ldec_total_award_amount < 0 THEN ldec_total_award_amount = 0.00
	idw_benefit_entitlement.setitem(li_inserted_row,'be_total_award_amount', ldec_total_award_amount)  
	
	/********* OPENING TEXT ********************/
	ls_dropvalue = wf_retrieve_display_value(1, ll_opening_no, ll_claim_no, 0)
			
	IF ls_dropvalue <> '' THEN
		idw_benefit_entitlement.setitem(li_inserted_row,'opening_text',ls_dropvalue)
	ELSE
		idw_benefit_entitlement.setitem(li_inserted_row,'opening_text','0')
	END IF 
						
	/********* BENCALC TEXT ********************/
	ls_dropvalue = wf_retrieve_display_value(2, ll_opening_no, ll_claim_no, ll_ben_calc_no)
			
	IF ls_dropvalue <> '' THEN
		idw_benefit_entitlement.setitem(li_inserted_row,'bencalc_text',ls_dropvalue)
	ELSE
		idw_benefit_entitlement.setitem(li_inserted_row,'bencalc_text','0')
	END IF 
	
NEXT	
		
RETURN 1
end function

public function integer wf_refresh_entitlement ();/* refresh the entitlement datawindow */
INTEGER 				li_rowcount, li_counter, li_payment_counter
STRING					ls_selectable_flag, ls_claim_no
LONG						ll_payment_no, ll_award_no, ll_found, ll_claim_in_error
DWITEMSTATUS   	ldwis_rowstatus

//LOTS OF FLICKER SET THE REDRAW FALSE
idw_benefit_entitlement.setredraw(FALSE)

//reset the deleted flag 
ib_deleted = FALSE

// nothing to do
IF il_annuity_account_no <= 0  THEN 
	idw_benefit_entitlement.reset()
	idw_benefit_entitlement.setredraw(TRUE)
	RETURN 1
END IF 

//for each record in the entitlement datawindow that is new modified 
//reset their selectable flag to 'Y' based on the payment_no
FOR li_counter = 1 TO idw_benefit_entitlement.ROWCOUNT()
	//grab the itemstatus
	ldwis_rowstatus = idw_benefit_entitlement.GetItemStatus(li_counter,0,Primary!)
	IF ldwis_rowstatus = newmodified!  THEN
		
		//GRAB THE PAYMENT_NO
		ll_payment_no 	= idw_benefit_entitlement.getitemnumber(li_counter,'payment_no')
		ll_found 			= idw_payments.Find("payment_no =" + string(ll_payment_no), 1, idw_payments.RowCount())
		
		IF ll_found > 0 THEN 
			ll_award_no = idw_payments.getitemnumber(ll_found,'award_no')
			IF ISNULL(ll_award_no) THEN ll_award_no = 0
		END IF 
		
		//SET THE SELECTABLE FLAG ON THE PAYMENT -- this won't work with the combine!!!!! needs to be fixed
		FOR li_payment_counter = 1 TO idw_payments.rowcount()
			IF idw_payments.getitemnumber(li_payment_counter,'payment_no') = ll_payment_no AND NOT ISNULL(idw_payments.getitemdatetime(li_payment_counter,'payment_processed_date')) THEN
				idw_payments.setitem(li_payment_counter,'selectable_flag','Y')
			END IF 
			
			IF idw_payments.getitemnumber(li_payment_counter,'award_no') = ll_award_no AND NOT ISNULL(idw_payments.getitemdatetime(li_payment_counter,'payment_processed_date')) THEN
				idw_payments.setitem(li_payment_counter,'selectable_flag','Y')
			END IF 
			
		NEXT
	END IF 
NEXT

inv_maintain_benefit.nf_get_annuity_dates(il_annuity_account_no, il_claim_no, il_individual_no, is_claim_role_code, idtm_annuity_start_date, idtm_annuity_end_date)

//LOTS OF FLICKER SET THE REDRAW TRUE 
idw_benefit_entitlement.setredraw(true)

/* tabpage_benefit_entitlement */
idw_benefit_entitlement.settransobject(sqlca)
li_rowcount = idw_benefit_entitlement.retrieve(il_annuity_account_no)
SQLCA.nf_handle_error("w_maintain_benefit_entitlement","wf_refresh_entitlement()","idw_benefit_entitlement.retrieve(il_annuity_account_no)")

IF li_rowcount > 0 THEN
	wf_populate_benefit_totals(il_annuity_account_no)
    wf_populate_only_claim_dddw()
	idw_benefit_entitlement.SetItemStatus(1, 0, Primary!, NotModified!)///shouldn't have to do this....
	
	idw_benefit_entitlement.selectrow(0,FALSE)
	idw_benefit_entitlement.SelectRow(1, TRUE)
	
	wf_populate_AE_status_code(li_rowcount)
	
END IF

idw_benefit_qualification.settransobject(sqlca)
li_rowcount = idw_benefit_qualification.retrieve(il_annuity_account_no)
SQLCA.nf_handle_error("w_maintain_benefit_entitlement","wf_refresh_entitlement()","idw_benefit_qualification.retrieve(il_annuity_account_no)")

/* Once the qualifying period has been established this part of the screen will display 
   a tab to separate the qualification information from the entitlement information.
	may need to make this a general function if used elsewhere.
*/
IF li_rowcount > 0 THEN 
	tab_qualification.tabpage_qualification.visible = TRUE
ELSE
	tab_qualification.tabpage_qualification.visible = FALSE
END IF

/* re-retrieve the dddw information the user may have added a new one through the bencalc module */
/* populate the OPENINGS dropdown values */
ids_openings.Retrieve(il_claim_no_dddw[],is_claim_role_code)
SQLCA.nf_handle_error("w_maintain_benefit_entitlement","wf_refresh_entitlement()","ids_openings.Retrieve(il_claim_no_dddw[])")

/* populate the Bencalc dropdown values */
ids_bencalcs.Retrieve(il_claim_no_dddw[])
SQLCA.nf_handle_error("w_maintain_benefit_entitlement","wf_refresh_entitlement()","ids_bencalcs.Retrieve(il_claim_no_dddw[])")

wf_populate_opening_text()
wf_populate_bencalc_text()

idw_benefit_entitlement.Object.t_calculate_dates.TEXT 	= '' 
idw_benefit_entitlement.Object.b_calculate.TEXT 			= 'Calculate Totals' 

IF il_claim_no = 0 THEN
	ls_claim_no = ''
ELSE
	ls_claim_no = STRING(il_claim_no)
END IF 

/* now retrieve the injured worker info - top datawindow  we have to do this in case the Individual and a DOB-DOD Date change 
   outside of this module.
*/
wf_populate_worker_info(il_individual_no, ls_claim_no )
IF is_claim_role_code = 'SS' THEN
	dw_injured_worker.Object.individual_no_t.Text 	= 'Surviving Spouse'	
ELSE
	dw_injured_worker.Object.individual_no_t.Text 	= 'Injured Worker'
END IF 

//re-populate the worker detail inforamtion just in case something has changed
wf_populate_worker_details(il_individual_no)

RETURN 1


end function

public function integer wf_populate_claim_array ();/* this array will be used to populate the dropdown for an injured worker - not SS
*/
INTEGER li_counter, li_rowcount
LONG		ll_claim_no
U_DS		lds_claims

// dont need to do anything if the claim_role = 'SS' only need the one claim number
IF is_claim_role_code = 'SS' THEN
	 il_claim_no_dddw[1] = il_claim_no
	 RETURN 1
END IF 

//start the loopy thing to populate the claim numbers 
lds_claims 					= Create U_DS
lds_claims.DataObject 	= 'ds_claims'
lds_claims.SetTransObject(SQLCA)

li_rowcount = lds_claims.Retrieve(il_individual_no, is_claim_role_code)
SQLCA.nf_handle_error("w_maintain_benefit_entitlement","wf_populate_claim_array()","li_rowcount = lds_claims.Retrieve(il_individual_no, is_claim_role_code)")

IF li_rowcount > 0 THEN
	
	FOR li_counter = 1 TO li_rowcount
		ll_claim_no 							= lds_claims.getitemnumber(li_counter,'claim_no')
		il_claim_no_dddw[li_counter] 	= ll_claim_no
	NEXT
	
ELSE
	// UH-OH - no claims set the array to nothing and on outside turn off all buttons
	il_claim_no_dddw[1] 	= 0
	RETURN -1
END IF

RETURN 1
end function

public subroutine wf_populate_claim_dddw ();/* using the claim_no array we will populate the claim_no 
    dddw the user will be able to select from this 
	the claim number the user selects will determine which openings and fronm that bencalcs
*/
LONG						ll_claim_no, ll_row, ll_data
INTEGER					li_counter, li_row, li_rowcount
dwobject   				ldwo 
DATAWINDOWCHILD	ldwc_child

//NO ROWCOUNT DON'T DO ANYTHING
IF  idw_benefit_entitlement.rowcount() <= 0 THEN RETURN

idw_benefit_entitlement.GetChild('claim_no', ldwc_child)
ldwc_child.SetTransObject(SQLCA)
ldwc_child.SetFilter('')
ldwc_child.Filter()
ldwc_child.Retrieve(il_individual_no, is_claim_role_code)
SQLCA.nf_handle_error("w_maintain_benefit_entitlement","wf_populate_claim_dddw()","idw_benefit_entitlement.GetChild('claim_no', ldwc_child)")

li_rowcount = ldwc_child.rowcount()

IF li_rowcount > 1  THEN
	
	wf_retrieve_dddw_opening(il_claim_no_dddw[1])
	wf_retrieve_dddw_benefit(il_claim_no_dddw[1])
	wf_retrieve_payment_type_dddw(is_claim_role_code)

	//user must select claim_no set the associated DDDW's to reflect that
	
	// Get a reference to the "dwo" (DWObject) - <dw_name>.OBJECT.<columnname> 
	ldwo = idw_benefit_entitlement.OBJECT.opening_no_filtered

	// Define the "data" to pass 
	ll_data = idw_benefit_entitlement.GetItemNUMBER(1,"opening_no_filtered")
		
ELSEIF li_rowcount = 1 THEN
	//select the lone record to populate the dddw
	idw_benefit_entitlement.setitem(idw_benefit_entitlement.getrow(),'claim_no',il_claim_no_dddw[1])
	
	wf_filter_dddw_opening(il_claim_no_dddw[1])
		
	/******* benefit_calculation_no ***************/
	wf_filter_dddw_benefit_no(il_claim_no_dddw[1])
	
	//trigger the itemchanged on the benefit_no column which will cause the payment type to be filtered
	// DevNote : dw_address.OBJECT.textual_column_name - Does NOT Work !!! 
	//    (it must be the actual column name [from design time])

	// Get a reference to the "dwo" (DWObject) - <dw_name>.OBJECT.<columnname> 
	ldwo = idw_benefit_entitlement.OBJECT.benefit_calculation_no_filtered

	// Define the "data" to pass 
	ll_data = idw_benefit_entitlement.GetItemNUMBER(1,"benefit_calculation_no_filtered")
	
ELSE //NOT SURE

END IF 


end subroutine

public subroutine wf_retrieve_dddw_opening (long al_claim_no);DATAWINDOWCHILD	ldwc_child, ldwc_child_filtered
LONG					ll_row

idw_benefit_entitlement.GetChild('opening_no', ldwc_child)
ldwc_child.SetTransObject(SQLCA)
ldwc_child.SetFilter('')
ldwc_child.Filter()
ldwc_child.Retrieve(il_claim_no_dddw[], is_claim_role_code)
SQLCA.nf_handle_error("w_maintain_benefit_entitlement","wf_retrieve_dddw_opening()","ldwc_child.Retrieve(il_claim_no_dddw[])")

/*		always set a zeri no openings 
*/
//ll_row = ldwc_child.InsertRow(1)
//IF ll_row > 0 THEN
//	ldwc_child.SetItem(ll_row,'opening_no',0)
//END IF
//	
idw_benefit_entitlement.GetChild('opening_no_filtered', ldwc_child_filtered)
ldwc_child_filtered.SetTransObject(SQLCA)
ldwc_child_filtered.SetFilter('')
ldwc_child_filtered.Filter()
ldwc_child_filtered.Retrieve(il_claim_no_dddw[],is_claim_role_code)
SQLCA.nf_handle_error("w_maintain_benefit_entitlement","wf_retrieve_dddw_opening()","ldwc_child_filtered.Retrieve(il_claim_no_dddw[])")

/*		always set a zeri no openings 
*/
//ll_row = ldwc_child_filtered.InsertRow(1)
//IF ll_row > 0 THEN
//	ldwc_child_filtered.SetItem(ll_row,'opening_no',0)
//END IF
//		
/* benefit_qualification ************************/
idw_benefit_qualification.GetChild('opening_no', ldwc_child)
ldwc_child.SetTransObject(SQLCA)
ldwc_child.SetFilter('')
ldwc_child.Filter()
ldwc_child.Retrieve(il_claim_no_dddw[],is_claim_role_code)
SQLCA.nf_handle_error("w_maintain_benefit_entitlement","wf_retrieve_dddw_opening()","ldwc_child.Retrieve(il_claim_no_dddw[])")

/*		always set a zeri no openings 
*/
ll_row = ldwc_child.InsertRow(1)
IF ll_row > 0 THEN
	ldwc_child.SetItem(1,'opening_no',0)
END IF
end subroutine

public function integer wf_delete_benefit_entitlement ();/*
I talked to Paul & Penny about retaining all BENEFIT_ENTITLEMENT after it has been changed or deleted. 
It is preferred to not retain those that are not linked via an XREF. We were not intending on providing 
the ability to reactivate/reinstate records that had been logically deleted. 

The software should be ensuring that linked records (via XREF) are not attempting to physically delete them. 
It should allow deletion of those not linked to an XREF. 

There are also foreign keys that will prevent the physical removal of a BENEFIT_ENTITLEMENT that is 
linked via an XREF.

FYI,
Jan

soooo..... if they click the delete flag check to see if it is in the xref table if it is 
do not physically delete the record 
	
If the record does not exist in the xref table - delete the row.


3	Delete an existing benefit entitlement The benefit entitlement is NOT in an xref table	
	The row is deleted		Note: If you click the del checkbox and then unclick it the record has still been modified and a copy is created.
4	Delete an existing benefit entitlementThe benefit entitlement is in an xref table	The existing data is archived
	(marked as inactive and deleted) and a new row is saved		


*/
INTEGER 	li_rowcount, li_counter, li_result
STRING		ls_deleted_flag
LONG			ll_account_no, ll_benefit_no, ll_loopcount, ll_rowsindw
BOOLEAN	lb_check


ll_loopcount = 1
ll_rowsindw = idw_benefit_entitlement.ROWCOUNT()
DO WHILE ll_loopcount <= ll_rowsindw
	
	//if the record is deleted DO NOT make another record simply set this record as inactive and.
	ls_deleted_flag = idw_benefit_entitlement.getitemstring(ll_loopcount,'deleted_flag')
	
	IF ls_deleted_flag = 'Y'  THEN 
		
		//grab the values we need
		ll_account_no 	= idw_benefit_entitlement.getitemnumber(ll_loopcount,'annuity_account_no')
		ll_benefit_no 	= idw_benefit_entitlement.getitemnumber(ll_loopcount,'benefit_entitlement_no')
				
		//check for the record in the XREF table 
		lb_check = inv_maintain_benefit.nf_entitlement_xref_exists(ll_account_no, ll_benefit_no)
				
		IF lb_check = TRUE THEN
			//DO NOTHING
			ll_loopcount ++
			CONTINUE
		ELSE
			//this needs to be checked to ensure that the rowcount doesn't get thrown out of whack
			li_result = idw_benefit_entitlement.deleterow(ll_loopcount)
			
			//set the deleted flag we need this in case only one record was modified it will never be saved
			ib_deleted = TRUE
			
			IF li_result = 1 THEN
				ll_rowsindw --
			ELSE
				RETURN -1
			END IF
		END IF
	ELSE	
		ll_loopcount ++
	END IF 
LOOP

RETURN 1
end function

public function long wf_get_annuity_claim_no (long al_annuity_account_no);LONG	ll_claim_no

SELECT claim_no INTO :ll_claim_no
FROM ANNUITY_ACCOUNT b
WHERE  annuity_account_no  = :al_annuity_account_no
USING SQLCA;

sqlca.nf_handle_error('w_maintain_benefit_entitlement','wf_get_annuity_claim_no()','SELECT claim_no INTO :ll_claim_no')

RETURN ll_claim_no
end function

public subroutine wf_pop_info_from_account_info (long al_annuity_account_no);/*Grabs the inforation from the annuity_account based on the annuity_account_no */

SELECT 	individual_no, claim_no, claim_role_code  
INTO 		:il_individual_no, :il_claim_no, :is_claim_role_code
FROM 	ANNUITY_ACCOUNT b
WHERE  	annuity_account_no  = :al_annuity_account_no
USING 	SQLCA;

SQLCA.nf_handle_error('w_maintain_benefit_entitlement','wf_pop_info_from_account_info()','SELECT claim_no INTO :ll_claim_no')


end subroutine

public subroutine wf_retrieve_dddw_benefit (long al_claim_no);datawindowchild ldwc_child, ldwc_child_filtered
LONG ll_row
	
//entitlement
idw_benefit_entitlement.GetChild('benefit_calculation_no_filtered', ldwc_child_filtered)
ldwc_child_filtered.SetTransObject(SQLCA)
ldwc_child_filtered.SetFilter('')
ldwc_child_filtered.Filter()
ldwc_child_filtered.Retrieve(il_claim_no_dddw[])
SQLCA.nf_handle_error("w_maintain_benefit_entitlement","wf_retrieve_dddw_benefit()","ldwc_child_filtered.Retrieve(il_claim_no_dddw[])")

/*		always set a zeri no openings 
*/
//ll_row = ldwc_child_filtered.InsertRow(1)
//IF ll_row > 0 THEN
//	ldwc_child_filtered.SetItem(1,'benefit_calculation_no',0)
//END IF
		
//qualification	tab
idw_benefit_qualification.GetChild('benefit_calculation_no', ldwc_child)
ldwc_child.SetTransObject(SQLCA)
ldwc_child.SetFilter('')
ldwc_child.Filter()
ldwc_child.Retrieve(il_claim_no_dddw[])
SQLCA.nf_handle_error("w_maintain_benefit_entitlement","wf_retrieve_dddw_benefit()","ldwc_child.Retrieve(il_claim_no_dddw[])")

/*		always set a zeri no openings 
*/
ll_row = ldwc_child.InsertRow(1)
IF ll_row > 0 THEN
	ldwc_child.SetItem(1,'benefit_calculation_no',0)
END IF
	
end subroutine

public function integer wf_set_ann_eligibility_checklist_no (long al_annuity_account_no, long al_annuity_eligibility_no, long al_checklist_no);/* This functionality is needed for the Verify Module - sets the step so that the 
    so that the checklist is updated in the calling module	 
*/
UPDATE 	ANNUITY_ELIGIBILITY
SET 		verify_benefit_entitlement_checklist_no 	= :al_checklist_no
WHERE 	annuity_account_no 							= :al_annuity_account_no
AND		annuity_eligibility_no 							= :al_annuity_eligibility_no
AND       annuity_eligibility_status_code                = 'P'
USING 	SQLCA;

SQLCA.nf_handle_error("w_maintain_benefit_entitlement","wf_set_ann_eligibility_checklist_no()","UPDATE ANNUITY_ELIGIBILITY")

RETURN 1
end function

public subroutine wf_retrieve_payment_type_dddw (string as_claim_role_code);DATAWINDOWCHILD	ldwc_child, ldwc_child_filtered
LONG						ll_row

idw_benefit_entitlement.GetChild('payment_type_code', ldwc_child)
ldwc_child.SetTransObject(SQLCA)
ldwc_child.SetFilter('')
ldwc_child.Filter()
ldwc_child.Retrieve(as_claim_role_code)
SQLCA.nf_handle_error("w_maintain_benefit_entitlement","wf_retrieve_payment_type_dddw()","idw_benefit_entitlement.GetChild('payment_type_code', ldwc_child)")

/*		
*/
ll_row = ldwc_child.InsertRow(1)
IF ll_row > 0 THEN
	ldwc_child.SetItem(1,'payment_type_code',' ')
END IF

/* filtered dddw */
idw_benefit_entitlement.GetChild('payment_type_filtered', ldwc_child_filtered)
ldwc_child_filtered.SetTransObject(SQLCA)
ldwc_child_filtered.SetFilter('')
ldwc_child_filtered.Filter()
ldwc_child_filtered.Retrieve(as_claim_role_code)
SQLCA.nf_handle_error("w_maintain_benefit_entitlement","wf_retrieve_payment_type_dddw()","ldwc_child_filtered.Retrieve(as_claim_role_code)")

/*	
*/
ll_row = ldwc_child_filtered.InsertRow(1)
IF ll_row > 0 THEN
	ldwc_child_filtered.SetItem(1,'payment_type_code',' ')
END IF

end subroutine

public subroutine wf_filter_dddw_opening (long al_claim_no);/*
2.10	All openings of types associated with benefits that attract annuities must be available for all claims of the 
         injured worker if the annuity account is for an injured worker.
2.20	All openings of survivor benefit types must be available for the claim on which the individual 
         is a surviving spouse if the annuity account is for a surviving spouse.
*/

datawindowchild ldwc_child

idw_benefit_entitlement.SetRedraw ( false )

//entitlement
idw_benefit_entitlement.GetChild('opening_no_filtered', ldwc_child)
ldwc_child.SetTransObject(SQLCA)
ldwc_child.SetFilter('')
ldwc_child.Filter()

//set the filter
ldwc_child.SetFilter("claim_no=" + String(al_claim_no) + " or opening_no = 0") 

//filter
ldwc_child.Filter()

//set the redraw back on this will put the correct value in the DD	
idw_benefit_entitlement.SetRedraw ( true )

end subroutine

public subroutine wf_filter_dddw_benefit_no (long al_claim_no);/*
2.30	All benefit calculations associated with the selected openings must be available.

In this case when the opening is selected the appropriate records are filtered.
We have a hidden DDDW that contains FILTERED items...this DDDW contains ALL items for the claim
*/
datawindowchild 	ldwc_child

idw_benefit_entitlement.SetRedraw ( false )

//entitlement
idw_benefit_entitlement.GetChild('benefit_calculation_no_filtered', ldwc_child)
ldwc_child.SetTransObject(SQLCA)
ldwc_child.SetFilter('')
ldwc_child.Filter()

//set the filter
ldwc_child.SetFilter("claim_no=" + String(al_claim_no)) 

//filter
ldwc_child.Filter()

//set the redraw back on this will put the correct value in the DD	
idw_benefit_entitlement.SetRedraw ( true )

end subroutine

public function string wf_get_authorization_filter (long al_claim_no);STRING		ls_filter, ls_admin_region
BOOLEAN	lb_add_and

SELECT 	admin_region_code
INTO 		:ls_admin_region
FROM 	CLAIM 
WHERE 	claim_no = :al_claim_no
USING 	SQLCA;

SQLCA.nf_handle_error("w_maintain_benefit_entitlement","wf_get_authorization_filter()","SELECT admin_region_code")

ls_filter = '('

IF gnv_user_authorizations.nf_authorizations_exist(ls_admin_region,"loe") THEN	
	ls_filter += 'authorization_type_code = "loe" '
	lb_add_and = TRUE
END IF

//SR70 - Added new authorization Code for Special Survivor Payments - SMANZER - JAN 2001
IF gnv_user_authorizations.nf_authorizations_exist(ls_admin_region,"ssp") THEN	
	IF lb_add_and THEN
		ls_filter += ' OR '
	END IF	
	ls_filter += ' authorization_type_code = "ssp" '
	lb_add_and = TRUE		
END IF

//Check if the user has a pension authorization
if gnv_user_authorizations.nf_authorizations_exist(ls_admin_region,"pen") THEN	
	IF lb_add_and THEN
		ls_filter += ' OR '
	END IF
	ls_filter += ' authorization_type_code = "pen"'		
END IF

IF ls_filter > '' THEN
	ls_filter +=  ')'
END IF

RETURN ls_filter
end function

public subroutine wf_filter_dddw_payment_type (long al_opening_no, long al_benefit_calculation_no, long al_claim_no);DATAWINDOWCHILD  	ldwc_child
STRING       ls_filter, ls_opening_type_code, ls_rtw_incentive_flag, ls_payment_type_code, ls_find
LONG			ll_row
DECIMAL		ldec_benefit_level
DATE			ldt_accident_recurrence_date
INTEGER     li_error, li_find, li_filter_count


idw_benefit_entitlement.SetRedraw ( false )

/*	add a filter to get only payment types valid for this screen*/
IF IsNull(al_opening_no) 					THEN al_opening_no 					= 0
IF IsNull(al_benefit_calculation_no) 	THEN al_benefit_calculation_no 	= 0
IF IsNull(al_claim_no) 					THEN al_claim_no 						= 0

IF idw_benefit_entitlement.GetRow() <= 0 THEN 
	idw_benefit_entitlement.SetRedraw ( true )
	RETURN 
END IF 

//if no bencalc or opening just leaved the basic retrieve
IF al_opening_no = 0 AND al_benefit_calculation_no = 0 THEN
	idw_benefit_entitlement.GetChild('payment_type_filtered', ldwc_child)
	ldwc_child.SetTransObject(SQLCA)
	ldwc_child.SetFilter('')
	ldwc_child.Filter()
	idw_benefit_entitlement.SetRedraw ( true )
	RETURN
END IF

ls_filter = ''
				
/*	find the row of the currently selected number*/
idw_benefit_entitlement.GetChild('opening_no_filtered',ldwc_child)

IF al_opening_no > 0 THEN
	ll_row = ldwc_child.Find('opening_no = ' + string(al_opening_no),0,ldwc_child.RowCount())
ELSE
	ll_row = 0
END IF

IF ll_row > 0 THEN
	ls_opening_type_code 			= ldwc_child.GetItemString(ll_row,'opening_type_code')
	ldt_accident_recurrence_date 	= date(ldwc_child.GetItemDateTime(ll_row,'accident_recurrence_date'))
ELSE
	ls_opening_type_code = 'I'
END IF

IF ls_opening_type_code > '' THEN
	ls_filter = ls_filter + ' (opening_type_code = "' + ls_opening_type_code + '"'
END IF
				
IF al_benefit_calculation_no = 0 THEN
/*	see pay types where openings match and benefit level = 0 */		
	ldec_benefit_level = 0
ELSE
	idw_benefit_entitlement.GetChild('benefit_calculation_no_filtered', ldwc_child)
	ll_row = ldwc_child.Find('benefit_calculation_no = ' + string(al_benefit_calculation_no),0,ldwc_child.RowCount())
	IF ll_row > 0 THEN
		ldec_benefit_level 			= ldwc_child.GetItemNumber(ll_row,'benefit_level_percentage')
		ls_rtw_incentive_flag 		= ldwc_child.GetItemString(ll_row,'rtw_incentive_flag')
	END IF
END IF
		
/*		
		Prior to January 01, 1998, the 3 Day payment is a valid payment type for RLOE openings having a
		benefit level of 80%. For accidents or recurrences of injury after December 31, 1997, all RLOE benefits 
		start at 85%. The valid payments are determined through the payment type table which is based on the 
		benefit category and the benefit level. Since the 3D payment in this table is at 80% and another payment
		of type "3D" cannot be added, the filter was forced to include 3D payments for RLOE openings after 
		December 31, 1997. NOTE: at some point, the use of the payment_combination and the payment_type tables
		should be reviewed to eliminate the reliance on the benefit level in determining the valid payment types.	
*/
		
IF ls_opening_type_code = "RLOE"   							AND  &
	ldt_accident_recurrence_date > Date ("1997/12/31") AND &
	al_benefit_calculation_no > 0 							THEN
	ls_filter = ls_filter + ' AND benefit_level_percentage = ' + String(ldec_benefit_level) + ') OR  payment_type_code = "3D"'
ELSE
	IF ls_opening_type_code > '' THEN
		ls_filter = ls_filter + ' AND benefit_level_percentage = ' + String(ldec_benefit_level) + ')'
	ELSE
		ls_filter = ls_filter + ' AND benefit_level_percentage = ' + String(ldec_benefit_level)
	END IF
END IF
		
/* P10261 - RTW Incentive */
ls_payment_type_code = idw_benefit_entitlement.GetItemString(1,'payment_type_code')
IF ls_rtw_incentive_flag ='Y' THEN
	IF ls_payment_type_code <> 'R2' AND ls_payment_type_code <> 'R3' THEN
		idw_benefit_entitlement.SetItem(1,'payment_type_code','')
	END IF
	
	ls_filter = ls_filter + ' AND (payment_type_code = "R2" OR payment_type_code = "R3" )'
	
ELSE
	IF ls_payment_type_code = 'R2' OR ls_payment_type_code = 'R3' THEN
		idw_benefit_entitlement.SetItem(1,'payment_type_code','')
	END IF
	ls_filter = ls_filter + ' AND (payment_type_code <> "R2" AND payment_type_code <> "R3" )'
END IF
		
li_error = idw_benefit_entitlement.GetChild('payment_type_filtered', ldwc_child)
IF li_error <> 1  THEN 
	SignalError(-666,'Error getting child DDDW')
End if

//make sure we have the empty space option
ls_filter = ls_filter + ' OR  payment_type_code = " "'

//remove any existing filter
ldwc_child.SetTransObject(SQLCA)
ldwc_child.SetFilter('')
ldwc_child.Filter()

IF  ldwc_child.SetFilter(ls_filter) = -1 Then
	SignalError(-666,'Error setting filter criteria for payment_type_code. Expression = "' + ls_filter + '"')
End if

//RUN THE FILTER
li_filter_count = ldwc_child.Filter()


IF li_filter_count < 1 THEN  
	ls_find 	= 'payment_type_code = " "'
	li_find 	= ldwc_child.Find(ls_find,1,ldwc_child.RowCount())
	
	IF li_find > 0 THEN
		ldwc_child.ScrollToRow(li_find)
		idw_benefit_entitlement.setitem(1,'payment_type_code'," ")
	END IF
END IF 

idw_benefit_entitlement.SetRedraw ( true )

end subroutine

public subroutine wf_populate_benefit_totals (long al_annuity_account_no);/* This functions populates the totals in the benefit datawindow 
At the bottom of the list of entitlement the following columns will be totaled. 
     Number of hours 
·	Number of days 
·	Number of weeks
·	Number of months
·	Total benefit entitlement amount

There is three different ways the data can be totaled.  All three sets of totals will be displayed.  
The first row will total only the data in the qualification period – all the data prior to the annuity start date.  
The next row will total the data that falls with in the annuity start and end date.  The third row will total all 
the data on the screen (the total of both previous totals).  Prior to the annuity start and end dates being 
established it may only be necessary to display the grand total since there is no way to break the data up.

REVISED: the totals are now done 4 different ways

$#,##0.00;($#,##0.00)
*/
DECIMAL{2} ldc_Qualification_sum, ldc_Qualification_Days, ldc_Qualification_Weeks, ldc_Qualification_Months, ldc_Qualification_hours
DECIMAL{2} ldc_Entitlement_sum, ldc_Entitlement_Days, ldc_Entitlement_Weeks, ldc_Entitlement_Months, ldc_Entitlement_hours
DECIMAL{2} ldc_total_sum, ldc_total_Days, ldc_total_Weeks, ldc_total_Months, ldc_total_hours
DECIMAL{2} ldc_total_sum_after, ldc_total_Days_after, ldc_total_Weeks_after, ldc_total_Months_after, ldc_total_hours_after
DATE 			ldt_annuity_start, ldt_annuity_end

//GRAB THE annuity dates
SELECT 	annuity_start_date, annuity_end_date  
INTO		:ldt_annuity_start, :ldt_annuity_end
FROM 	ANNUITY_ELIGIBILITY a JOIN ANNUITY_ACCOUNT b
ON 		a.annuity_account_no 				= b.annuity_account_no
AND 		b.annuity_account_no 				= :al_annuity_account_no
AND 		a.annuity_eligibility_status_code 	= 'A'
USING SQLCA;

SQLCA.nf_handle_error('w_maintain_benefit_entitlement','wf_populate_benefit_totals()','SELECT annuity_start_date, annuity_end_date')

/*
Total Pre-Qualification
9.10	The Total Pre-Qualification must be the sum of the benefit amount on all entitlement prior to the annuity start date.
9.20	The Total Pre- Qualification Days must be the sum of the Days entitled on all entitlement prior to the annuity start date.
9.30	The Total Pre- Qualification Hours must be the sum of Hours entitled on all entitlement prior to the annuity start date.
9.40	The Total Pre- Qualification Weeks must be the sum of Weeks entitled on all entitlement prior to the annuity start date.
9.50	The Total Pre- Qualification Months must be the sum of Months entitled on all entitlement prior to the annuity start date.
*/
IF ldt_annuity_start <> date('1900-01-01')  THEN
	
	SELECT 	SUM(benefit_entitlement_amount),
				SUM(benefit_entitlement_number_of_hours),
				SUM(benefit_entitlement_number_of_days),
				SUM(benefit_entitlement_number_of_weeks),
				SUM(benefit_entitlement_number_of_months)
	INTO     	:ldc_Qualification_sum, : ldc_Qualification_hours, :ldc_Qualification_Days, :ldc_Qualification_Weeks, :ldc_Qualification_Months		
	FROM 	BENEFIT_ENTITLEMENT a
	WHERE 	benefit_entitlement_to_date   <= :ldt_annuity_start
	AND       a.annuity_account_no            	= :al_annuity_account_no  
	AND    	a.active_flag 						= 'Y'
	USING 	SQLCA;
			
	SQLCA.nf_handle_error('w_maintain_benefit_entitlement','wf_populate_benefit_totals()','SELECT SUM(benefit_entitlement_amount)(1)')
	
ELSE
	
		ldc_Qualification_sum 		= 0 
		ldc_Qualification_hours 		= 0
		ldc_Qualification_Days 		= 0
		ldc_Qualification_Weeks 		= 0
		ldc_Qualification_Months		= 0
	
END IF 

/* The Total Benefit Entitlement
9.60	The Total Benefit Entitlement must be the sum of the benefit amount on all entitlement from the annuity start date up to and including the annuity end date.
9.70	The Total Benefit Entitlement Days must be the sum of the Days entitled on all entitlement from the annuity start date up to and including the annuity end date.
9.80	The Total Benefit Entitlement Hours must be the sum of Hours entitled on all entitlement from the annuity start date up to and including the annuity end date.
9.90	The Total Benefit Entitlement Weeks must be the sum of Weeks entitled on all entitlement from the annuity start date up to and including the annuity end date.
9.100	The Total Benefit Entitlement Months must be the sum of Months entitled on all entitlement from the annuity start date up to and including the annuity end date.
*/
IF ldt_annuity_start <> date('1900-01-01') and not isnull(ldt_annuity_start) THEN
	
	SELECT 	SUM(benefit_entitlement_amount),
				SUM(benefit_entitlement_number_of_hours),
				SUM(benefit_entitlement_number_of_days),
				SUM(benefit_entitlement_number_of_weeks),
				SUM(benefit_entitlement_number_of_months)
	INTO     	:ldc_Entitlement_sum, :ldc_Entitlement_hours, :ldc_Entitlement_Days, :ldc_Entitlement_Weeks, :ldc_Entitlement_Months			
	FROM 	BENEFIT_ENTITLEMENT a
	WHERE 	(benefit_entitlement_to_date > :ldt_annuity_start AND benefit_entitlement_to_date <= dateadd(day,1,:ldt_annuity_end))
	AND       a.annuity_account_no          = :al_annuity_account_no  
	AND    	a.active_flag 					= 'Y'
	USING 	SQLCA;
			
	SQLCA.nf_handle_error('w_maintain_benefit_entitlement','wf_populate_benefit_totals()','SELECT SUM(benefit_entitlement_amount)(2)')
	
ELSE
	 
	ldc_Entitlement_sum 		= 0 
	ldc_Entitlement_hours 	= 0
	ldc_Entitlement_Days 		= 0
	ldc_Entitlement_Weeks 	= 0
	ldc_Entitlement_Months	= 0

END IF 

/* Post-Eligibility
9.110	The Total Post-Eligibility must be the sum of the benefit amount on all entitlement from the beginning of the month following the annuity end date.
9.120	The Total Post-Eligibility Days must be the sum of the Days entitled on all entitlement from the beginning of the month following the annuity end date.
9.130	The Total Post-Eligibility Hours must be the sum of Hours entitled on all entitlement from the beginning of the month following the annuity end date.
9.140	The Total Post-Eligibility Weeks must be the sum of Weeks entitled on all entitlement from the beginning of the month following the annuity end date.
9.150	The Total Post-Eligibility Months must be the sum of Months entitled on all entitlement from the beginning of the month following the annuity end date.
*/
IF ldt_annuity_end <> date('1900-01-01') and not isnull(ldt_annuity_end) THEN
	
	SELECT 	SUM(benefit_entitlement_amount),
				SUM(benefit_entitlement_number_of_hours),
				SUM(benefit_entitlement_number_of_days),
				SUM(benefit_entitlement_number_of_weeks),
				SUM(benefit_entitlement_number_of_months)
	INTO     	:ldc_total_sum_after, :ldc_total_hours_after, :ldc_total_Days_after, :ldc_total_Weeks_after, :ldc_total_Months_after			
	FROM 	BENEFIT_ENTITLEMENT a
	WHERE 	 benefit_entitlement_to_date > dateadd(day,1,:ldt_annuity_end)
	AND       a.annuity_account_no          = :al_annuity_account_no  
	AND    	a.active_flag 					= 'Y'
	USING 	SQLCA;
				
	SQLCA.nf_handle_error('w_maintain_benefit_entitlement','wf_populate_benefit_totals()','SELECT SUM(benefit_entitlement_amount)(4)')
	
ELSE
	
	ldc_total_sum_after 		= 0 
	ldc_total_hours_after 		= 0
	ldc_total_Days_after 		= 0
	ldc_total_Weeks_after 	= 0
	ldc_total_Months_after	= 0
		
END IF 

/* TOTALS
9.160	The Grant Total of Entitlement must be the sum of the benefit amount on all entitlement.
9.170	The Grand Total Days must be the sum of the Days entitled on all entitlement. 
9.180	The Grand Total Hours must be the sum of Hours entitled on all entitlement.
9.190	The Grand Total Weeks must be the sum of Weeks entitled on all entitlement. 
9.200	The Grand Total Months must be the sum of Months entitled on all entitlement.
*/
 SELECT 	SUM(benefit_entitlement_amount),
			SUM(benefit_entitlement_number_of_hours),
			SUM(benefit_entitlement_number_of_days),
			SUM(benefit_entitlement_number_of_weeks),
			SUM(benefit_entitlement_number_of_months)
INTO     	:ldc_total_sum, :ldc_total_hours, :ldc_total_Days, :ldc_total_Weeks, :ldc_total_Months			
FROM 	BENEFIT_ENTITLEMENT a
WHERE 	a.annuity_account_no         = :al_annuity_account_no  
AND    	a.active_flag 					= 'Y'
USING 	SQLCA;
			
SQLCA.nf_handle_error('w_maintain_benefit_entitlement','wf_populate_benefit_totals()','SELECT SUM(benefit_entitlement_amount)(5)')
		
//IF NULL - 0
IF isnull(ldc_Entitlement_Days) 		THEN  ldc_Entitlement_Days 		= 0.00
IF isnull(ldc_Entitlement_Weeks) 	THEN  ldc_Entitlement_Weeks 		= 0.00
IF isnull(ldc_Entitlement_Months) 	THEN  ldc_Entitlement_Months 		= 0.00
IF isnull(ldc_Entitlement_sum) 		THEN  ldc_Entitlement_sum 			= 0.00
IF isnull(ldc_Entitlement_hours) 	THEN  ldc_Entitlement_hours 		= 0.00

IF isnull(ldc_Qualification_Days) 	THEN  ldc_Qualification_Days 		= 0.00
IF isnull(ldc_Qualification_Months) THEN  ldc_Qualification_Months 	= 0.00
IF isnull(ldc_Qualification_Weeks) 	THEN  ldc_Qualification_Weeks 	= 0.00
IF isnull(ldc_Qualification_sum) 	THEN  ldc_Qualification_sum 		= 0.00
IF isnull(ldc_Qualification_hours) 	THEN  ldc_Qualification_hours 		= 0.00

IF isnull(ldc_total_Days_after) 		THEN  ldc_total_Days_after 			= 0.00
IF isnull(ldc_total_Months_after) 	THEN  ldc_total_Months_after 		= 0.00
IF isnull(ldc_total_Weeks_after) 	THEN  ldc_total_Weeks_after 		= 0.00
IF isnull(ldc_total_sum_after) 		THEN  ldc_total_sum_after 			= 0.00
IF isnull(ldc_total_hours_after) 		THEN  ldc_total_hours_after 		= 0.00

IF isnull(ldc_total_Days) 				THEN  ldc_total_Days 					= 0.00
IF isnull(ldc_total_Months) 			THEN  ldc_total_Months 				= 0.00
IF isnull(ldc_total_Weeks) 			THEN  ldc_total_Weeks 				= 0.00
IF isnull(ldc_total_sum) 				THEN  ldc_total_sum 					= 0.00
IF isnull(ldc_total_hours) 			THEN  ldc_total_hours 				= 0.00

/* POPULATE THE TEXT COLUMNS IN THE DATAWINDOW */
//BEFORE
idw_benefit_entitlement.Object.t_ben_hours.Text 		= STRING(ldc_Entitlement_hours)
idw_benefit_entitlement.Object.t_ben_days.Text 		= STRING(ldc_Entitlement_Days)
idw_benefit_entitlement.Object.t_ben_weeks.Text 	= STRING(ldc_Entitlement_Weeks)
idw_benefit_entitlement.Object.t_ben_months.Text 	= STRING(ldc_Entitlement_Months)
idw_benefit_entitlement.Object.t_ben_amount.Text 	= STRING(ldc_Entitlement_sum,'$#,##0.00;($#,##0.00)')

//QUALIFICATION
idw_benefit_entitlement.Object.t_qual_hours.Text 	= STRING(ldc_Qualification_hours)
idw_benefit_entitlement.Object.t_qual_days.Text 		= STRING(ldc_Qualification_Days)
idw_benefit_entitlement.Object.t_qual_months.Text 	= STRING(ldc_Qualification_Months)
idw_benefit_entitlement.Object.t_qual_weeks.Text 	= STRING(ldc_Qualification_Weeks)
idw_benefit_entitlement.Object.t_qual_amount.Text 	= STRING(ldc_Qualification_sum,'$#,##0.00;($#,##0.00)')

//AFTER
idw_benefit_entitlement.Object.t_post_hours.Text 	= STRING(ldc_total_hours_after)
idw_benefit_entitlement.Object.t_post_days.Text 		= STRING(ldc_total_Days_after)
idw_benefit_entitlement.Object.t_post_months.Text 	= STRING(ldc_total_Months_after)
idw_benefit_entitlement.Object.t_post_weeks.Text 	= STRING(ldc_total_Weeks_after)
idw_benefit_entitlement.Object.t_post_amount.Text 	= STRING(ldc_total_sum_after,'$#,##0.00;($#,##0.00)')

//TOTALS
idw_benefit_entitlement.Object.t_sum_hours.Text 	= STRING(ldc_total_hours)
idw_benefit_entitlement.Object.t_sum_days.Text 		= STRING(ldc_total_Days)
idw_benefit_entitlement.Object.t_sum_months.Text 	= STRING(ldc_total_Months)
idw_benefit_entitlement.Object.t_sum_weeks.Text 	= STRING(ldc_total_Weeks)
idw_benefit_entitlement.Object.t_sum_amount.Text 	= STRING(ldc_total_sum,'$#,##0.00;($#,##0.00)')

end subroutine

public subroutine wf_reapply_ben_filter (integer ai_row);/* this function is called after the save causes all data to be re-filtered which is required
    due to the hidden filtered DDDW's we have.
*/
LONG			ll_opening_no, ll_bencalc_no, ll_claim_no
INTEGER		li_find
STRING		ls_find
DATAWINDOWCHILD ldwc_child_bencalc, ldwc_child_opening

IF idw_benefit_entitlement.ROWCOUNT() < 1  THEN RETURN

ll_opening_no 	= idw_benefit_entitlement.GetItemNumber(ai_row,'opening_no')
ll_opening_no 	= idw_benefit_entitlement.getitemnumber(ai_row,'opening_no_filtered')
ll_bencalc_no 	= idw_benefit_entitlement.getitemnumber(ai_row,'benefit_calculation_no_filtered')
ll_claim_no 		= idw_benefit_entitlement.getitemnumber(ai_row,'claim_no')
	
idw_benefit_entitlement.GetChild('benefit_calculation_no_filtered', ldwc_child_bencalc)
idw_benefit_entitlement.GetChild('opening_no_filtered', ldwc_child_opening)
				
//if rowcount = 1 then only benefit calc of 0
IF ll_opening_no = 0 OR ldwc_child_bencalc.RowCount() = 1 THEN		
	ldwc_child_bencalc.SetFilter(" benefit_calculation_no = 0")
ELSE
	ldwc_child_bencalc.SetFilter("(opening_no = " + string(ll_opening_no) + " AND claim_no = " + string(ll_claim_no) + ")" + " or benefit_calculation_no = 0")
END IF
		
ldwc_child_bencalc.Filter()
ls_find = 'benefit_calculation_no = ' + string(ll_bencalc_no)
	
li_find = ldwc_child_bencalc.Find(ls_find,1,ldwc_child_bencalc.RowCount())

IF li_find > 0 THEN
	ldwc_child_bencalc.ScrollToRow(li_find)
END IF
		
ldwc_child_bencalc.SetSort('benefit_calculation_no A')
ldwc_child_bencalc.Sort()
		
/**********payment_type*******************/
 
IF ISNULL(ll_opening_no) 	THEN ll_opening_no 	= 0
IF ISNULL(ll_bencalc_no) 	THEN ll_bencalc_no 	= 0
iF ISNULL(ll_claim_no) 		THEN ll_bencalc_no 	= 0

wf_filter_dddw_payment_type(ll_opening_no, ll_bencalc_no, ll_claim_no )
		
end subroutine

public subroutine wf_insert_error (integer ai_rownum, string as_column_name, string as_error_message);/* inserts a record into the error message datawindow */
INTEGER li_row

li_row = ids_error_messages.insertrow(0)
ids_error_messages.setitem(li_row,'row_num',ai_rownum)
ids_error_messages.setitem(li_row,'column_name',as_column_name)
ids_error_messages.setitem(li_row,'error_text',as_error_message)

end subroutine

public function integer wf_other_process_date_check ();/*
Determining the Annuity Start and End Dates

A common function will be created to determine the annuity start and end dates.  
The module will not set the dates but it will be necessary to determine the dates on the save to verify the entitlement data.  

The benefit entitlement period on a single entitlement entry cannot span the annuity start or end date because 
it will not be possible to properly calculate the portion of the entitlement period that is eligible for annuity.  
(The creation of the function will need to be coordinated with the development of the Confirm Annuity Eligibility module).

Since the dates are changing the saved data will need to be checked each time the dates are determined.  
The user must be forced to fix any data that violates the business rules around spanning the annuity start and end dates.

Also it is possible for a user to enter into the module with for the purpose of verifying the benefit entitlement 
and only check off the items in the checklist without ever changing any data.  At this point, the data should be 
validated with regards to the dates to ensure that no other process affected the dates.

NOTE: 2010.10.19
"Staff note that messages regarding benefit entitlement spanning the start or end date of the eligibility period 
are provided once you complete the verify checklist step of verify.  They say it would be more helpful to have those
messages on the save when you're still in the verify screen rather then when you're going thru the checklist."

*/

INTEGER			li_counter
DATETIME	 	ldtm_be_from_date, ldtm_be_to_date, ldtm_deleted_date
STRING			ls_error_text

setpointer(hourglass!)

FOR li_counter = 1 TO idw_benefit_entitlement.rowcount()
	
	ldtm_be_from_date 	= idw_benefit_entitlement.getitemdatetime(li_counter,'benefit_entitlement_from_date')
	ldtm_be_to_date 		= idw_benefit_entitlement.getitemdatetime(li_counter,'benefit_entitlement_to_date')

	//6.80 A benefit entitlement period must not span the annuity start date. (OLD)
	/* (NEW)
	1.110	The Verify Benefit Entitlement checklist step must not be completed if benefit entitlement spans the annuity start date.
	*/
	ls_error_text = inv_maintain_benefit.nf_check_span_start_date(DATE(ldtm_be_from_date), DATE(ldtm_be_to_date),idtm_annuity_start_date, idtm_annuity_end_date,1) 
	IF ls_error_text <> '' THEN
	 	MESSAGEBOX('Annuity Start Date Violation', ls_error_text)
		RETURN -1
	END IF

	//6.90 A benefit entitlement period must not span the annuity end date. (OLD)
	/* (NEW)
	1.120 The Verify Benefit Entitlement checklist step must not be completed if benefit entitlement spans the annuity end date.
	*/
	ls_error_text = inv_maintain_benefit.nf_check_span_start_date(DATE(ldtm_be_from_date), DATE(ldtm_be_to_date),idtm_annuity_start_date, idtm_annuity_end_date,2) 
	IF ls_error_text <> '' THEN
		MESSAGEBOX('Annuity End Date Violation', ls_error_text)
		RETURN -1
	END IF
	
NEXT

RETURN 1
end function

public function integer wf_populate_annuity_dates (long al_annuity_account_no);/*
If Maintain Benefit Entitlement was initiated to Verify Benefit Entitlement for the purpose of 
Prepare and Request Annuity Payout or if the module was opened from the menu and the 
annuity start and end date have already been established the following is displayed:
Ø	Annuity Start Date 
Ø	Annuity End Date.
*/

dw_annuity_dates.retrieve(al_annuity_account_no)
SQLCA.nf_handle_error("w_maintain_benefit_entitlement","wf_populate_annuity_dates()","dw_annuity_dates.retrieve()")

RETURN 1
end function

public function decimal wf_calculate_amount (integer ai_row, decimal adec_user_defined, integer ai_type);/*
8.50	The benefit entitlement benefit amount must be calculated as follows:
Daily rate 	* number of Days entitled	+
Hourly rate 	* number of Hours entitled  	+
Weekly rate 	* number of Weeks entitled  	+
Monthly rate 	* number of Months entitled	+
Benefit Amount, if ‘Days’, ‘Hours’, ‘Weeks’ and ‘Months’ entitled not required

Ø	The daily rate for a weekly benefit must be calculated as follows:
	Benefit Award amount / number of days per week on the benefit calculation
Ø	The daily rate for a monthly benefit must be calculated as follows:
	Benefit Award amount / 30
Ø	The hourly rate for a weekly benefit must be calculated as follows:
	(Benefit Award amount / number of days per week on the benefit calculation) / number of hours worked per day
*/

STRING 		ls_ben_freq
LONG			ll_opening_no, ll_claim_no, ll_bencalc_no, ll_test
DECIMAL		ldec_benefit_amount, ldec_days, ldec_hours, ldec_months, ldec_weeks
DECIMAL		ldec_ben_award_amount,  ldec_days_per_week, ldec_hours_per_day 
DECIMAL		ldec_daily_rate, ldec_hourly_rate, ldec_weekly_rate, ldec_monthly_rate, ldec_amount

//just in case
IF ISNULL(ai_row) OR ai_row <= 0 THEN RETURN 0

idw_benefit_entitlement.accepttext()

//grab the values
ll_claim_no 		= idw_benefit_entitlement.getitemnumber(ai_row,'claim_no')
ll_opening_no 	= idw_benefit_entitlement.getitemnumber(ai_row,'opening_no_filtered')
ll_bencalc_no 	= idw_benefit_entitlement.getitemnumber(ai_row,'benefit_calculation_no_filtered')
ll_test 			= idw_benefit_entitlement.getitemnumber(ai_row,'benefit_calculation_no')
ldec_days		= idw_benefit_entitlement.getitemdecimal(ai_row,'number_of_days')
ldec_hours		= idw_benefit_entitlement.getitemdecimal(ai_row,'number_of_hours')
ldec_months	= idw_benefit_entitlement.getitemnumber(ai_row,'number_of_months')
ldec_weeks		= idw_benefit_entitlement.getitemnumber(ai_row,'number_of_weeks')

//check em out
IF ISNULL(ll_claim_no) 		THEN ll_claim_no 		= 0
IF ISNULL(ll_opening_no) 	THEN ll_opening_no 	= 0
IF ISNULL(ll_bencalc_no) 	THEN ll_bencalc_no 	= 0
IF ISNULL(ldec_days) 			THEN ldec_days 		= 0.00
IF ISNULL(ldec_hours) 		THEN ldec_hours 		= 0.00
IF ISNULL(ldec_months) 		THEN ldec_months 	= 0
IF ISNULL(ldec_weeks) 		THEN ldec_weeks 		= 0

//grab the freq (weekly/monthly) from the bencalc
ls_ben_freq = inv_maintain_benefit.nf_get_ben_freq_code(ll_claim_no, ll_bencalc_no, ll_opening_no)
	
//now get the daily rate from the bencalc
SELECT 	award_amount, preacc_work_days_per_week, preacc_work_hours_per_day  
INTO 		:ldec_ben_award_amount,  :ldec_days_per_week, :ldec_hours_per_day  
FROM 	BENEFIT_CALCULATION
WHERE 	claim_no    					= :ll_claim_no
AND     	benefit_calculation_no 	= :ll_bencalc_no
AND 		opening_no 					= :ll_opening_no
USING 	SQLCA;
	
SQLCA.nf_handle_error('w_maintain_benefit_entitlement','wf_calculate_amount()','SELECT award_amount,')

IF ISNULL(ldec_ben_award_amount) 	THEN ldec_ben_award_amount 	= 0.00
IF ISNULL(ldec_days_per_week) 		THEN ldec_days_per_week 			= 0.00
IF ISNULL(ldec_hours_per_day) 		THEN ldec_hours_per_day 			= 0.00

//now calculate the rate from the benefit - this will be used to calculate the total amount placed in the benefit_entitlement_amount column
CHOOSE CASE ls_ben_freq
	CASE 'W' //WEEKLY
		/*
		The daily rate for a weekly benefit must be calculated as follows:
		Benefit Award amount / number of days per week on the benefit calculation
		*/
		ldec_daily_rate = ldec_ben_award_amount / ldec_days_per_week
		
		/*
		The hourly rate for a weekly benefit must be calculated as follows:
		(Benefit Award amount / number of days per week on the benefit calculation) / number of hours worked per day
		*/
		ldec_hourly_rate = (ldec_ben_award_amount / ldec_days_per_week) / ldec_hours_per_day
		
		//weekly rate = award amount
		ldec_weekly_rate = ldec_ben_award_amount
		
		//what would the monthly award amount be???? (NOT APPLICABLE)
		ldec_monthly_rate = 0.00
		
	CASE 'M' //MONTHLY
		/*
		The daily rate for a monthly benefit must be calculated as follows:
		Benefit Award amount / 30
		*/
		ldec_daily_rate = ldec_ben_award_amount / 30
		
		//monthly amount = award amount
		ldec_monthly_rate = ldec_ben_award_amount
		
		ldec_hourly_rate = 0.00	//not applicable
		 
		ldec_weekly_rate = 0.00//not applicable
		
END CHOOSE
		
ldec_amount = (ldec_daily_rate * ldec_days) + (ldec_hourly_rate * ldec_hours) + (ldec_monthly_rate * ldec_months) + (ldec_weekly_rate * ldec_weeks)

IF isnull(ldec_amount) OR ldec_amount < 0 THEN ldec_amount = 0//we have a problem

RETURN ROUND(ldec_amount,2)

end function

public subroutine wf_populate_only_claim_dddw ();/* using the claim_no array we will populate the claim_no 
*/
LONG						ll_claim_no
INTEGER					li_counter, li_row, li_rowcount
DATAWINDOWCHILD	ldwc_child

//NO ROWCOUNT DON'T DO ANYTHING
IF  idw_benefit_entitlement.rowcount() <= 0 THEN RETURN

idw_benefit_entitlement.GetChild('claim_no', ldwc_child)

//if the array already holds values - populated on open of window DO NOT repopulate it
IF ldwc_child.rowcount() > 0  THEN RETURN  

FOR li_counter = 1 TO upperbound(il_claim_no_dddw[])

	ll_claim_no = il_claim_no_dddw[li_counter]

	li_row = ldwc_child.insertrow(1)
	ldwc_child.setitem(li_row, 'claim_no', ll_claim_no)

NEXT

end subroutine

public subroutine wf_populate_bencalc_text ();/*
 SELECT benefit_calculation_no, effective_from_date, benefit_level_percentage,   
        		award_amount, opening_no, calculation_note, claim_no, rtw_incentive_flag
   FROM BENEFIT_CALCULATION, OPENING
  WHERE BENEFIT_CALCULATION.claim_no = OPENING.claim_no
  AND opening_no = OPENING.opening_no
  AND claim_no in ( :al_claimno);
*/
INTEGER 		li_counter , li_rowcount
LONG				ll_claim, ll_opening, ll_bencalc, ll_row
STRING			ls_dropvalue
dwitemstatus 	ldwis
 
 FOR li_counter = 1 TO idw_benefit_entitlement.rowcount()

	ldwis = idw_benefit_entitlement.GetItemStatus(li_counter,0,Primary!)
	
	ll_claim 		= idw_benefit_entitlement.getitemnumber(li_counter,'claim_no')
	ll_opening 	= idw_benefit_entitlement.getitemnumber(li_counter,'opening_no_filtered')
	ll_bencalc 	= idw_benefit_entitlement.getitemnumber(li_counter,'benefit_calculation_no_filtered')
	
	/********* BENCALC TEXT ********************/
	ls_dropvalue = wf_retrieve_display_value(2, ll_opening, ll_claim, ll_bencalc)
			
	IF ls_dropvalue <> '' THEN
		idw_benefit_entitlement.setitem(li_counter,'bencalc_text',ls_dropvalue)
	ELSE
		idw_benefit_entitlement.setitem(li_counter,'bencalc_text','0')
	END IF 
	
	idw_benefit_entitlement.SetItemStatus(li_counter, 0, Primary!, ldwis)
		
NEXT

end subroutine

public subroutine wf_populate_opening_text ();/*
Populates the dropdown text of the opening dropdown - crazzzzzyyyy!!!!!!
*/

INTEGER 		li_counter , li_rowcount
LONG				ll_claim, ll_opening, ll_row
STRING			ls_dropvalue
dwitemstatus 	ldwis
 
 FOR li_counter = 1 TO idw_benefit_entitlement.rowcount()

	ldwis = idw_benefit_entitlement.GetItemStatus(li_counter,0,Primary!)
	
	ll_claim 		= idw_benefit_entitlement.getitemnumber(li_counter,'claim_no')
	ll_opening 	= idw_benefit_entitlement.getitemnumber(li_counter,'opening_no_filtered')
	
	ls_dropvalue = wf_retrieve_display_value(1, ll_opening, ll_claim, 0)
			
	IF ls_dropvalue <> '' THEN
		idw_benefit_entitlement.setitem(li_counter,'opening_text',ls_dropvalue)
	ELSE
		idw_benefit_entitlement.setitem(li_counter,'opening_text','0')
	END IF 
	
	idw_benefit_entitlement.SetItemStatus(li_counter, 0, Primary!, ldwis)
NEXT

end subroutine

public function long wf_get_claim_no ();RETURN il_claim_no
end function

public function long wf_get_individual_no ();RETURN il_individual_no
end function

public function string wf_get_claim_role_code ();RETURN is_claim_role_code
end function

public function string wf_get_module_code ();RETURN '049'
end function

public subroutine wf_setrow (integer ai_row);IF idw_benefit_entitlement.rowcount() > 0 THEN
	idw_benefit_entitlement.SELECTROW(0,false)
	idw_benefit_entitlement.SELECTROW(ai_row,true)
	idw_benefit_entitlement.setrow(ai_row)
END IF
end subroutine

public subroutine wf_retrieve_dddw_claim_no (long al_individual_no);DATAWINDOWCHILD	ldwc_child

//NO ROWCOUNT DON'T DO ANYTHING
IF  idw_benefit_entitlement.rowcount() <= 0 THEN RETURN

idw_benefit_entitlement.GetChild('claim_no', ldwc_child)
ldwc_child.SetTransObject(SQLCA)
ldwc_child.SetFilter('')
ldwc_child.Filter()
ldwc_child.Retrieve(al_individual_no)
SQLCA.nf_handle_error("w_maintain_benefit_entitlement","wf_retrieve_dddw_claim_no()","idw_benefit_entitlement.GetChild('claim_no', ldwc_child)")
	
end subroutine

public function string wf_retrieve_display_value (integer ai_type, long al_opening_no, long al_claim_no, long al_ben_calc_no);STRING	ls_dropvalue
LONG		ll_row

/*
NOTE: Display value will be assigned by the calling code
*/

//type 1,2 (opening/bencalc)
CHOOSE CASE ai_type
	CASE 1
		
		IF al_opening_no > 0 THEN 
				ll_row = ids_openings.Find("opening_no = " + string(al_opening_no) + " and claim_no = " + string(al_claim_no),0,ids_openings.RowCount())
		ELSE
				ll_row = ids_openings.Find("opening_no = " + string(0) ,0,ids_openings.RowCount())
		END IF
		
		IF ll_row > 0 THEN
			
			//this will be returned
			ls_dropvalue 	= ids_openings.GetItemstring(ll_row,'display')
				
		ELSE
			ls_dropvalue = ''
		END IF 	
		
	CASE 2//bencalc text
		
		IF al_ben_calc_no > 0 THEN 
			ll_row = ids_bencalcs.Find("benefit_calculation_no = " + string(al_ben_calc_no) + " and claim_no = " + string(al_claim_no) + " and opening_no = " + string(al_opening_no),0,ids_bencalcs.RowCount())
		ELSE
			ll_row = ids_bencalcs.Find("benefit_calculation_no = " + string(0) ,0,ids_bencalcs.RowCount())
		END IF
			
		IF ll_row > 0 THEN
							
			ls_dropvalue 	= ids_bencalcs.GetItemstring(ll_row,'drop_value')

		ELSE
			ls_dropvalue = ''
		END IF 		
END CHOOSE
	
RETURN ls_dropvalue
end function

public subroutine wf_post_resize_checklist_position ();LONG		ll_x , ll_width

IF ib_checklist_posted THEN
	
	ll_x = THIS.x
	ll_width = THIS.width
	
	IF uo_checklist.ib_maximized THEN
		// minimize the checklist before resize
		uo_checklist.p_right_arrow.triggerevent(clicked!)
		uo_checklist.il_maximized_x = (ll_width - 160) - 2076
		uo_checklist.il_minimized_x = ll_width - 160
	END IF
		
	uo_checklist.x = ll_width - 160
	
	ib_checklist_posted = FALSE
END IF
end subroutine

public subroutine wf_populate_award_name ();/* populate the award_name text in the award datawindow
    probably a better way to do it but i don't know
	  
*/
INTEGER		li_counter , li_rowcount
STRING		ls_recipient_type, ls_recipient_name, ls_recipient_care_of
LONG			ll_recipient_no


li_rowcount = idw_awards.rowcount()

IF li_rowcount <= 0 THEN RETURN

FOR li_counter = 1 TO li_rowcount
	
	//grab the recipient type
	ls_recipient_type = idw_awards.getitemstring(li_counter, 'recipient_type_code')
	ll_recipient_no    = idw_awards.getitemnumber(li_counter, 'recipient_no')
	
	CHOOSE CASE ls_recipient_type
		CASE 'I'//INDIVIDUAL
			
			SELECT given_names + ' ' + last_name INTO :ls_recipient_name  FROM INDIVIDUAL  WHERE individual_no = :ll_recipient_no   USING SQLCA ;
			SQLCA.nf_handle_error('w_maintain_benefit_entitlement','wf_populate_award_name()','SELECT  FROM INDIVIDUAL')
	    
		CASE ""//NOTHING
			ls_recipient_name = ''
			
		CASE ELSE//RECIPIENT If recipient is a payee, read the address from Service Providers table          

		   SELECT	 name
	   	   INTO 		:ls_recipient_name
		   FROM 		PROVIDER  
		   WHERE  	provider_no 			= :ll_recipient_no 
		   AND   		provider_type_code 	= :ls_recipient_type
		  USING 		SQLCA ;
					 
 		SQLCA.nf_handle_error('w_maintain_benefit_entitlement','wf_populate_award_name()',' SELECT PROVIDER.name')		
				
	END CHOOSE
	
	IF ISNULL(ls_recipient_name) THEN ls_recipient_name = ''
	
	idw_awards.setitem(li_counter,'recipient_name',ls_recipient_name)
	
NEXT
end subroutine

public subroutine wf_print ();IF idw_benefit_entitlement.rowcount() > 0 THEN 
	PRINT(idw_benefit_entitlement)
END IF 
end subroutine

public subroutine wf_print_payments ();idw_payments.print()
end subroutine

public function boolean wf_check_ss_date_range (date adt_paid_from_date, date adt_paid_to_date);INTEGER			li_paid_from_year, li_paid_from_month, li_paid_from_day, li_paid_to_year, li_paid_to_month, li_paid_to_day

li_paid_from_year = Year(adt_paid_from_date)
li_paid_from_month = Month(adt_paid_from_date)
li_paid_from_day = Day(adt_paid_from_date)

li_paid_to_year = Year(adt_paid_to_date)
li_paid_to_month = Month(adt_paid_to_date)
li_paid_to_day = Day(adt_paid_to_date)

IF li_paid_from_month = li_paid_to_month - 1 THEN
	IF li_paid_from_day = li_paid_to_day THEN
		RETURN TRUE
	ELSE
		RETURN FALSE
	END IF
ELSEIF li_paid_from_month = 12 AND li_paid_to_month = 1 THEN
	IF li_paid_from_year = li_paid_to_year - 1 THEN
		IF li_paid_from_day = li_paid_to_day THEN
			RETURN TRUE
		ELSE
			RETURN FALSE
		END IF
	ELSE
		RETURN FALSE
	END IF
ELSE
	RETURN FALSE
END IF
end function

public function integer wf_populate_calculated_totals (long al_annuity_account_no);/* This functions populates the totals in the benefit datawindow 
At the bottom of the list of entitlement the following columns will be totaled. 
     Number of hours 
·	Number of days 
·	Number of weeks
·	Number of months
·	Total benefit entitlement amount

There is three different ways the data can be totaled.  All three sets of totals will be displayed.  
The first row will total only the data in the qualification period – all the data prior to the annuity start date.  
The next row will total the data that falls with in the annuity start and end date.  The third row will total all 
the data on the screen (the total of both previous totals).  Prior to the annuity start and end dates being 
established it may only be necessary to display the grand total since there is no way to break the data up.

$#,##0.00;($#,##0.00)

-- these are calculated totals - what may happen if the user saves the data and the annuity_dates remain as determined 
-- using the function below
*/
DECIMAL{2} 	ldc_Qualification_sum, ldc_Qualification_Days, ldc_Qualification_Weeks, ldc_Qualification_Months, ldc_Qualification_hours
DECIMAL{2} 	ldc_Entitlement_sum, ldc_Entitlement_Days, ldc_Entitlement_Weeks, ldc_Entitlement_Months, ldc_Entitlement_hours
DECIMAL{2}		ldc_be_number_of_months, ldc_be_number_of_weeks, ldc_be_number_of_days, 	ldc_be_number_of_hours, ldc_be_amount
DECIMAL{2}		ldc_total_sum, ldc_total_hours, ldc_total_Days, ldc_total_Weeks, ldc_total_Months		
DECIMAL{2} 	ldc_total_sum_after, ldc_total_Days_after, ldc_total_Weeks_after, ldc_total_Months_after, ldc_total_hours_after
DATETIME 		ldtm_annuity_start, ldtm_annuity_end, ldtm_be_from_date, ldtm_be_to_date
INTEGER			li_counter, li_return
STRING			ls_type

//GRAB THE annuity dates
li_return = inv_maintain_benefit.nf_get_annuity_dates(al_annuity_account_no, il_claim_no, il_individual_no, is_claim_role_code, ldtm_annuity_start, ldtm_annuity_end)

IF ldtm_annuity_start > ldtm_annuity_end THEN
	IF is_claim_role_code = 'C' THEN
		ls_type = ' claimant '
	ELSE
		ls_type = ' surviving spouse '
	END IF
	MESSAGEBOX('Annuity Dates', 'The'+ls_type+'turned 65 or died prior to the potential annuity start date, and therefore will not be eligible.')
	RETURN -1
END IF

IF li_return = -1 THEN 
	MESSAGEBOX('Annuity Dates', 'Annuity Start and End Dates could not be calculated')
	RETURN -1
END IF 

 //NOTHING CAN BE DONE
IF ISNULL(ldtm_annuity_start) OR ISNULL(ldtm_annuity_end) THEN
	RETURN -1
END IF 

//initialize the variables
ldc_Qualification_sum 		= 0
ldc_Qualification_hours 		= 0
ldc_Qualification_Days 		= 0
ldc_Qualification_Weeks  	= 0
ldc_Qualification_Months 	= 0
ldc_Entitlement_sum 			= 0
ldc_Entitlement_hours		= 0
ldc_Entitlement_Days			= 0
ldc_Entitlement_Weeks		= 0
ldc_Entitlement_Months		= 0	

/* do a loop grab  the values and add them together */
FOR li_counter = 1 TO idw_benefit_entitlement.rowcount()
	
		ldtm_be_from_date 			    	= idw_benefit_entitlement.getitemdatetime(li_counter,'benefit_entitlement_from_date')
		ldtm_be_to_date 					= idw_benefit_entitlement.getitemdatetime(li_counter,'benefit_entitlement_to_date')
		ldc_be_number_of_months 		= idw_benefit_entitlement.getitemdecimal(li_counter,'number_of_months')
		ldc_be_number_of_weeks 		= idw_benefit_entitlement.getitemdecimal(li_counter,'number_of_weeks')
		ldc_be_number_of_days 			= idw_benefit_entitlement.getitemdecimal(li_counter,'number_of_days')
		ldc_be_number_of_hours 		= idw_benefit_entitlement.getitemdecimal(li_counter,'number_of_hours')
		ldc_be_amount 					= idw_benefit_entitlement.getitemdecimal(li_counter,'benefit_entitlement_amount')
		
		IF ISNULL(ldc_be_number_of_months) 	THEN ldc_be_number_of_months 	= 0
		IF ISNULL(ldc_be_number_of_weeks) 	THEN ldc_be_number_of_weeks 	= 0
		IF ISNULL(ldc_be_number_of_days) 		THEN ldc_be_number_of_days 		= 0
		IF ISNULL(ldc_be_number_of_hours) 		THEN ldc_be_number_of_hours 	= 0
		IF ISNULL(ldc_be_amount) 					THEN ldc_be_amount 				= 0
		
		/* QUALIFICATION
			9.10	The Total Pre-Qualification must be the sum of the benefit amount on all entitlement prior to the annuity start date.
             9.20	The Total Pre- Qualification Days must be the sum of the Days entitled on all entitlement prior to the annuity start date.
             9.30	The Total Pre- Qualification Hours must be the sum of Hours entitled on all entitlement prior to the annuity start date.
             9.40	The Total Pre- Qualification Weeks must be the sum of Weeks entitled on all entitlement prior to the annuity start date.
             9.50	The Total Pre- Qualification Months must be the sum of Months entitled on all entitlement prior to the annuity start date.
		*/
		IF DATE(ldtm_be_to_date) <= DATE(ldtm_annuity_start) THEN
			
		 	ldc_Qualification_sum 		= 	ldc_Qualification_sum 	+ ldc_be_amount
			ldc_Qualification_hours 		=  ldc_Qualification_hours 	+ ldc_be_number_of_hours
			ldc_Qualification_Days 		=  ldc_Qualification_Days 	+ ldc_be_number_of_days
			ldc_Qualification_Weeks  	=  ldc_Qualification_Weeks 	+ ldc_be_number_of_weeks
			ldc_Qualification_Months 	=  ldc_Qualification_Months + ldc_be_number_of_months
			
		END IF 
		
		/* ENTITLEMENT
		9.60	The Total Benefit Entitlement must be the sum of the benefit amount on all entitlement from the annuity start date up to and including the annuity end date.
		9.70	The Total Benefit Entitlement Days must be the sum of the Days entitled on all entitlement from the annuity start date up to and including the annuity end date.
		9.80	The Total Benefit Entitlement Hours must be the sum of Hours entitled on all entitlement from the annuity start date up to and including the annuity end date.
		9.90	The Total Benefit Entitlement Weeks must be the sum of Weeks entitled on all entitlement from the annuity start date up to and including the annuity end date.
		9.100	The Total Benefit Entitlement Months must be the sum of Months entitled on all entitlement from the annuity start date up to and including the annuity end date.
		*/
		IF	(DATE(ldtm_be_to_date) > DATE(ldtm_annuity_start) AND DATE(ldtm_be_to_date) <= RELATIVEDATE(DATE(ldtm_annuity_end),1)) THEN
			
			ldc_Entitlement_sum 		= 	ldc_Entitlement_sum 		+ ldc_be_amount
			ldc_Entitlement_hours 	=  ldc_Entitlement_hours 	+ ldc_be_number_of_hours
			ldc_Entitlement_Days 		=  ldc_Entitlement_Days 		+ ldc_be_number_of_days
			ldc_Entitlement_Weeks  	=  ldc_Entitlement_Weeks 	+ ldc_be_number_of_weeks
			ldc_Entitlement_Months 	=  ldc_Entitlement_Months 	+ ldc_be_number_of_months
				
		END IF 	
		
		/* POST ENTITLEMENT
		9.110	The Total Post-Eligibility must be the sum of the benefit amount on all entitlement from the beginning of the month following the annuity end date.
		9.120	The Total Post-Eligibility Days must be the sum of the Days entitled on all entitlement from the beginning of the month following the annuity end date.
		9.130	The Total Post-Eligibility Hours must be the sum of Hours entitled on all entitlement from the beginning of the month following the annuity end date.
		9.140	The Total Post-Eligibility Weeks must be the sum of Weeks entitled on all entitlement from the beginning of the month following the annuity end date.
		9.150	The Total Post-Eligibility Months must be the sum of Months entitled on all entitlement from the beginning of the month following the annuity end date.
		*/
		IF	(DATE(ldtm_be_to_date) >  RELATIVEDATE(DATE(ldtm_annuity_end),1)) THEN
			
			ldc_total_sum_after 		= 	ldc_total_sum_after 		+ ldc_be_amount
			ldc_total_hours_after 		=  ldc_total_hours_after 		+ ldc_be_number_of_hours
			ldc_total_Days_after 		=  ldc_total_Days_after 		+ ldc_be_number_of_days
			ldc_total_Weeks_after  	=  ldc_total_Weeks_after 	+ ldc_be_number_of_weeks
			ldc_total_Months_after 	=  ldc_total_Months_after 	+ ldc_be_number_of_months
				
		END IF 	
			
NEXT

/*
Also, per the BRs, the Grand Total isn't supposed to be the sum of the 2 totals on the screen, it is supposed to be sum of All B/E.

PS: If you click on Calculate Totals, I think the first 2 totals come up right but the Grand total still is not the total of all B/E.
*/

/*
9.160	The Grand Total of Entitlement must be the sum of the benefit amount on all entitlement.
9.170	The Grand Total Days must be the sum of the Days entitled on all entitlement. 
9.180	The Grand Total Hours must be the sum of Hours entitled on all entitlement.
9.190	The Grand Total Weeks must be the sum of Weeks entitled on all entitlement. 
9.200	The Grand Total Months must be the sum of Months entitled on all entitlement.
*/
 SELECT 	SUM(benefit_entitlement_amount),
			SUM(benefit_entitlement_number_of_hours),
			SUM(benefit_entitlement_number_of_days),
			SUM(benefit_entitlement_number_of_weeks),
			SUM(benefit_entitlement_number_of_months)
INTO     	:ldc_total_sum, :ldc_total_hours, :ldc_total_Days, :ldc_total_Weeks, :ldc_total_Months			
FROM 	BENEFIT_ENTITLEMENT a
WHERE 	a.annuity_account_no         = :al_annuity_account_no  
AND    	a.active_flag 					= 'Y'
USING 	SQLCA;
			
SQLCA.nf_handle_error('w_maintain_benefit_entitlement','wf_populate_calculated_totals()','SELECT SUM(benefit_entitlement_amount)(4)')

IF isnull(ldc_total_Days) 				THEN  ldc_total_Days 					= 0.00
IF isnull(ldc_total_Months) 			THEN  ldc_total_Months 				= 0.00
IF isnull(ldc_total_Weeks) 			THEN  ldc_total_Weeks 				= 0.00
IF isnull(ldc_total_sum) 				THEN  ldc_total_sum 					= 0.00
IF isnull(ldc_total_hours) 			THEN  ldc_total_hours 				= 0.00

/* POPULATE THE TEXT COLUMNS IN THE DATAWINDOW */
//BEFORE
idw_benefit_entitlement.Object.t_ben_hours.Text 		= STRING(ldc_Entitlement_hours)
idw_benefit_entitlement.Object.t_ben_days.Text 		= STRING(ldc_Entitlement_Days)
idw_benefit_entitlement.Object.t_ben_weeks.Text 	= STRING(ldc_Entitlement_Weeks)
idw_benefit_entitlement.Object.t_ben_months.Text 	= STRING(ldc_Entitlement_Months)
idw_benefit_entitlement.Object.t_ben_amount.Text 	= STRING(ldc_Entitlement_sum,'$#,##0.00;($#,##0.00)')

//QUALIFICATION
idw_benefit_entitlement.Object.t_qual_hours.Text 	= STRING(ldc_Qualification_hours)
idw_benefit_entitlement.Object.t_qual_days.Text 		= STRING(ldc_Qualification_Days)
idw_benefit_entitlement.Object.t_qual_months.Text 	= STRING(ldc_Qualification_Months)
idw_benefit_entitlement.Object.t_qual_weeks.Text 	= STRING(ldc_Qualification_Weeks)
idw_benefit_entitlement.Object.t_qual_amount.Text 	= STRING(ldc_Qualification_sum,'$#,##0.00;($#,##0.00)')

//AFTER
idw_benefit_entitlement.Object.t_post_hours.Text 	= STRING(ldc_total_hours_after)
idw_benefit_entitlement.Object.t_post_days.Text 		= STRING(ldc_total_Days_after)
idw_benefit_entitlement.Object.t_post_months.Text 	= STRING(ldc_total_Months_after)
idw_benefit_entitlement.Object.t_post_weeks.Text 	= STRING(ldc_total_Weeks_after)
idw_benefit_entitlement.Object.t_post_amount.Text 	= STRING(ldc_total_sum_after,'$#,##0.00;($#,##0.00)')

//TOTALS
idw_benefit_entitlement.Object.t_sum_hours.Text 	= STRING(ldc_total_hours)
idw_benefit_entitlement.Object.t_sum_days.Text 		= STRING(ldc_total_Days)
idw_benefit_entitlement.Object.t_sum_months.Text 	= STRING(ldc_total_Months)
idw_benefit_entitlement.Object.t_sum_weeks.Text 	= STRING(ldc_total_Weeks)
idw_benefit_entitlement.Object.t_sum_amount.Text 	= STRING(ldc_total_sum,'$#,##0.00;($#,##0.00)')

//DATES
idw_benefit_entitlement.Object.t_calculate_dates.TEXT = 'Dates Used:' + '~r' + string(date(ldtm_annuity_start),'yyyy-mm-dd') + ' To ' + string(date(ldtm_annuity_end),'yyyy-mm-dd')


end function

public subroutine wf_populate_ae_status_code (integer ai_rowcount);INTEGER		li_counter
LONG			ll_annuity_eligibility_no
STRING		ls_annuity_eligibility_status_code


FOR li_counter = 1 TO ai_rowcount
	ll_annuity_eligibility_no = idw_benefit_entitlement.GetItemNumber(li_counter,'annuity_eligibility_no')
	IF ll_annuity_eligibility_no > 0 THEN
		SELECT	annuity_eligibility_status_code
		INTO		:ls_annuity_eligibility_status_code
		FROM		ANNUITY_ELIGIBILITY
		WHERE	annuity_eligibility_no = :ll_annuity_eligibility_no
		USING SQLCA;
		SQLCA.nf_handle_error('w_maintain_benefit_entitlement', 'wf_populate_AE_status_code', 'SELECT annuity_eligibility_status_code FROM ANNUITY_ELIGIBILITY...')
		
		idw_benefit_entitlement.SetItem(li_counter,'annuity_eligibility_status_code',ls_annuity_eligibility_status_code)
		idw_benefit_entitlement.SetItemStatus(li_counter, 0, Primary!, NotModified!)
	ELSE
		idw_benefit_entitlement.SetItem(li_counter,'annuity_eligibility_status_code','X')
		idw_benefit_entitlement.SetItemStatus(li_counter, 0, Primary!, NotModified!)
		CONTINUE
	END IF
	
NEXT


end subroutine

public function integer wf_check_unsaved_changes (string as_action);INTEGER 			li_counter, li_count, li_rowcount
DWITEMSTATUS   ldwis_rowstatus
STRING				ls_window_name

idw_benefit_entitlement.accepttext()

li_rowcount = idw_benefit_entitlement.ROWCOUNT()

li_count = 0
FOR li_counter = 1 TO li_rowcount
	//CHECK THE ROW STATUS
	ldwis_rowstatus = idw_benefit_entitlement.GetItemStatus(li_counter,0,Primary!)
	IF ldwis_rowstatus = DATAModified! OR ldwis_rowstatus = NEWModified! THEN
		li_count ++
	END IF 	
NEXT		
 
IF li_count > 0  THEN 
	MESSAGEBOX('Pending Changes', 'Please Refresh the Screen or Save Changes before you '+as_action+'.')
	RETURN 1
ELSE
	RETURN 0
END IF 	

end function

public function integer wf_populate_worker_details (long al_individual_no);/* this function simply fills in the worker information based on Individual_no

	DOB - DOD - AGE
*/

n_individual lnv_individual
DATE			ldt_birth, ldt_death
INTEGER		li_age, li_row

IF dw_injured_worker.rowcount() > 0 THEN
	lnv_individual = CREATE n_individual 
	//grab the birth and death dates
	ldt_birth		= DATE(dw_injured_worker.getitemdatetime(1, 'birth_date'))
	ldt_death     = DATE(dw_injured_worker.getitemdatetime(1, 'death_date'))
	
	li_age = lnv_individual.nf_calculate_age(ldt_birth, ldt_death)
	IF ISNULL(li_age) OR li_age < 0 THEN li_age = 0
		
	/*
	Leave Name where it is, Add Date of Birth, Date of Death & Age above Annuity Dates
	Highlight in Red if reached age 65 or deceased

	Need to check where Pre-93 eligibillity & Multiple accounts are displayed
	*/
	
	// make sure there are no rows in the datawindow
	IF dw_worker_details.rowcount() > 0 THEN
		dw_worker_details.reset()
	END IF 
	
	// now insert the new data into the worker information detail window
	li_row = dw_worker_details.insertrow(0)
	
	//now set the data
	dw_worker_details.setitem(li_row, 'birth_date',ldt_birth)
	dw_worker_details.setitem(li_row, 'death_date',ldt_death)
	dw_worker_details.setitem(li_row, 'age',li_age)
	
	DESTROY lnv_individual
	
END IF 

RETURN 1
end function

public function long wf_check_be_for_valid_claim ();/* This function will simply loop through all of the BE to check for claims that should appear in the dropdown

	If any claims are found they are directed to the claim information tab
	
	return 0  			- no problems
	return claim_no  	- claim exists that shouldn't be there
	return -1 			- general error
*/
INTEGER li_counter_be, li_counter, li_found
LONG		ll_claim_no, ll_claim_no_be

//loop every record in the BE datawindow
FOR li_counter_be = 1 TO Idw_benefit_entitlement.rowcount()
	
	// grab the claim number from the outter loop
	ll_claim_no_be = Idw_benefit_entitlement.getitemnumber(li_counter_be, 'claim_no')

	//reset the found variable to not found
	li_found = 0
	
	FOR li_counter = 1 TO UPPERBOUND(il_claim_no_dddw)
		ll_claim_no 	   = il_claim_no_dddw[li_counter] 
		IF ll_claim_no = ll_claim_no_be  THEN 
			li_found = 1
			EXIT
		END IF
		
		//last one in this check - must be found
		IF li_counter = UPPERBOUND(il_claim_no_dddw) THEN
			IF li_found = 0 THEN 
				RETURN ll_claim_no_be// STAFF NEEDS TO LOOK AT THIS.
			END IF 
		END IF 
	NEXT
NEXT

// all clear
RETURN 0
end function

public function w_maintain_benefit_entitlement wf_get_window_reference ();RETURN THIS
end function

public function integer wf_set_annuity_payout_checklist_no (long al_checklist_no);/* This functionality is needed for the Verify Module - sets the step so that the 
    so that the checklist is updated in the calling module	 
*/
UPDATE 	ANNUITY_PAYOUT
SET 		verify_benefit_entitlement_checklist_no = :al_checklist_no
WHERE 	annuity_payout_no 							 = :il_annuity_payout_no
USING 	SQLCA;

SQLCA.nf_handle_error("w_maintain_benefit_entitlement","UPDATE ANNUITY_PAYOUT...","wf_set_annuity_payout_checklist_no()")

RETURN 1
end function

on w_maintain_benefit_entitlement.create
int iCurrent
call super::create
if IsValid(this.MenuID) then destroy(this.MenuID)
if this.MenuName = "m_annuity" then this.MenuID = create m_annuity
this.rb_all=create rb_all
this.rb_non_zero=create rb_non_zero
this.dw_worker_details=create dw_worker_details
this.uo_checklist=create uo_checklist
this.tab_entitlement=create tab_entitlement
this.dw_injured_worker=create dw_injured_worker
this.dw_annuity_dates=create dw_annuity_dates
this.st_vert_splitbar_opening=create st_vert_splitbar_opening
this.tab_qualification=create tab_qualification
this.cb_validate_entitlement=create cb_validate_entitlement
this.cb_create=create cb_create
this.cb_combine=create cb_combine
this.cb_split=create cb_split
this.cb_refresh=create cb_refresh
this.cb_temp_save=create cb_temp_save
this.st_multiple_accounts=create st_multiple_accounts
this.cb_extract=create cb_extract
this.cb_add=create cb_add
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.rb_all
this.Control[iCurrent+2]=this.rb_non_zero
this.Control[iCurrent+3]=this.dw_worker_details
this.Control[iCurrent+4]=this.uo_checklist
this.Control[iCurrent+5]=this.tab_entitlement
this.Control[iCurrent+6]=this.dw_injured_worker
this.Control[iCurrent+7]=this.dw_annuity_dates
this.Control[iCurrent+8]=this.st_vert_splitbar_opening
this.Control[iCurrent+9]=this.tab_qualification
this.Control[iCurrent+10]=this.cb_validate_entitlement
this.Control[iCurrent+11]=this.cb_create
this.Control[iCurrent+12]=this.cb_combine
this.Control[iCurrent+13]=this.cb_split
this.Control[iCurrent+14]=this.cb_refresh
this.Control[iCurrent+15]=this.cb_temp_save
this.Control[iCurrent+16]=this.st_multiple_accounts
this.Control[iCurrent+17]=this.cb_extract
this.Control[iCurrent+18]=this.cb_add
end on

on w_maintain_benefit_entitlement.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.rb_all)
destroy(this.rb_non_zero)
destroy(this.dw_worker_details)
destroy(this.uo_checklist)
destroy(this.tab_entitlement)
destroy(this.dw_injured_worker)
destroy(this.dw_annuity_dates)
destroy(this.st_vert_splitbar_opening)
destroy(this.tab_qualification)
destroy(this.cb_validate_entitlement)
destroy(this.cb_create)
destroy(this.cb_combine)
destroy(this.cb_split)
destroy(this.cb_refresh)
destroy(this.cb_temp_save)
destroy(this.st_multiple_accounts)
destroy(this.cb_extract)
destroy(this.cb_add)
end on

event open;call super::open;INTEGER									li_rowcount , li_upperbound, li_row, li_age, li_rtn, li_incomplete_annuity_checklist
LONG										ll_sin_no
STRING									ls_claim_no, ls_history_flag, ls_name, ls_message
s_window_message 						lstr_message
w_maintain_benefit_entitlement   lw_win
n_individual                     lnv_individual
DATE										ldt_birth, ldt_death

/* populate the local structure 
    Accessing the module from the Verify checklist assumes that an individual number and claim role code
    are passed into the module.  If the module is opened for a surviving spouse a claim number 
    will also be passed in.
*/
lstr_message = Message.powerobjectparm

/* Populate variables from the message object */
il_annuity_account_no 	   = lstr_message.al_doubleparm[1]
il_annuity_eligibility_no 	= lstr_message.al_doubleparm[2]
il_action_code 				= lstr_message.al_doubleparm[3]
il_claim_no					   = lstr_message.al_doubleparm[4]
il_individual_no 			   = lstr_message.al_doubleparm[5]
il_annuity_payout_no 		= lstr_message.al_doubleparm[6]

is_claim_role_code 		   = lstr_message.as_stringparm[1]	


//create the NVO
inv_maintain_benefit = CREATE n_maintain_benefit
inv_common_annuity 	= CREATE n_common_annuity 


IF il_individual_no <> 0 THEN
	
	SELECT sin_no , history_flag , given_names + ' ' + last_name
	INTO   :ll_sin_no , :ls_history_flag , :ls_name
	FROM   INDIVIDUAL
	WHERE  individual_no = :il_individual_no
	USING SQLCA;
	SQLCA.nf_handle_error('w_maintain_benefit_entitlement', 'embedded SQL: SELECT sin_no...', 'open event')

	IF ll_sin_no = 0 THEN
		IF is_claim_role_code = 'C' THEN
			MessageBox("Cannot Save Changes","An injured worker must have a Social Insurance Number. You cannot save any changes to the Benefit Entitlement for this injured worker ("+ls_name+")",StopSign!)
			ib_cannot_save_SIN = TRUE
		ELSEIF is_claim_role_code = 'SS' THEN
			MessageBox("Benefit Entitlement Validation","A surviving spouse should have a Social Insurance Number.",Information!)
		END IF
	END IF
	
	IF ls_history_flag = 'Y' THEN
		IF is_claim_role_code = 'C' THEN
			MessageBox("Cannot Save Changes","This injured worker is a historical individual. You cannot save any changes to the Benefit Entitlement for this injured worker ("+ls_name+")",StopSign!)
		ELSEIF is_claim_role_code = 'SS' THEN
			MessageBox("Cannot Save Changes","This surviving spouse is a historical individual. You cannot save any changes to the Benefit Entitlement for this surviving spouse ("+ls_name+")",StopSign!)
		END IF
		ib_cannot_save_history = TRUE
	END IF
END IF

// this should be taken care of in the code that calls this but just in case add it here
IF is_claim_role_code = 'C' THEN il_claim_no = 0

//grab the annuity_account info if applicable
IF isnull(il_annuity_account_no) OR il_annuity_account_no = 0 THEN
	il_annuity_account_no = wf_get_annuity_account_no(il_individual_no,il_claim_no,is_claim_role_code)
	
	//populate the instance variables
	wf_pop_info_from_account_info(il_annuity_account_no)
	
END IF


IF il_action_code = 2 THEN
	// Opened Maintain Benefit Entitlement
	
	// BR 1.160
	// Benefit entitlement must not be changed through the Maintain Benefit Entitlement module, 
	// if an incomplete annuity-related checklist exists for the annuity account.
	SELECT Count(*)
	INTO   :li_incomplete_annuity_checklist
	FROM   ANNUITY_ACCOUNT                a
	JOIN   SUBSCRIBER_CHECKLIST_XREF      b on a.checklist_subscriber_no = b.checklist_subscriber_no
	JOIN   CHECKLIST_SUBSCRIBER           c on b.checklist_subscriber_no = c.checklist_subscriber_no
	JOIN   CHECKLIST                      d on b.checklist_no            = d.checklist_no
	WHERE  c.checklist_subscriber_type_code = 'ANN'
	AND    d.checklist_status_code          = 'IA'
	AND    a.annuity_account_no             = :il_annuity_account_no
	GROUP BY a.individual_no
	USING SQLCA;
	SQLCA.nf_handle_error('w_maintain_benefit_entitlement', 'open event', 'embedded SQL: SELECT Count(*) FROM ANNUITY_ACCOUNT,SUBSCRIBER_CHECKLIST_XREF,CHECKLIST_SUBSCRIBER,CHECKLIST...')
	
	IF li_incomplete_annuity_checklist > 0 THEN
		ib_incomplete_annuity_checklist = TRUE
		IF is_claim_role_code =  'C' THEN
			ls_message = 'This injured worker has an incomplete annuity checklist.~r~n'&
						  + 'You will not be allowed to save any new or modified benefit entitlement until all annuity checklists are completed.'
		ELSE
			ls_message = 'This surviving spouse has an incomplete annuity checklist.~r~n'&
						  + 'You will not be allowed to save any new or modified benefit entitlement until all annuity checklists are completed.'
		END IF
		MessageBox('Incomplete Annuity Checklist',ls_message,StopSign!)
	END IF
END IF


//check and see if the individual has multiple accounts if they do make the image visible
IF inv_common_annuity.nf_multiple_annuity_accounts(il_annuity_account_no) > 0 THEN
	st_multiple_accounts.visible = TRUE
ELSE
	st_multiple_accounts.visible = FALSE
END IF 

/*
The top portion of the window contains basic information about the individual (injured worker or surviving spouse), as follows:
Ø	Injured Worker or Surviving Spouse individual identifier
Ø	Claim Number (surviving spouse only)
Ø	Name
*/

/* set up the datawindows instances -- this will allow us to shorten the datawindow name*/
idw_opening_master 		= tab_entitlement.tabpage_openings.dw_opening_master
idw_opening_detail 		= tab_entitlement.tabpage_openings.dw_opening_detail
idw_payments 				= tab_entitlement.tabpage_payments.dw_payments
idw_awards 				= tab_entitlement.tabpage_awards.dw_awards
idw_overpayment			= tab_entitlement.tabpage_overpayment.dw_overpayment
idw_benefit_entitlement 	= tab_qualification.tabpage_entitlement.dw_benefit_entitlement
idw_benefit_qualification	= tab_qualification.tabpage_qualification.dw_benefit_qualification

lw_win = THIS

//set up the user object checklist stuff
uo_checklist.uf_set_parent_window(lw_win)

uo_checklist.itr_trans_object = SQLCA

uo_checklist.uf_get_checklist_nvo(inv_checklist)

uo_checklist.uf_set_module_code('049')
uo_checklist.uf_set_checklist_type_code('VBE')


inv_checklist.nf_set_datawindow(uo_checklist.idw_dw[],SQLCA)
inv_checklist.nf_set_checklist_object(uo_checklist)
inv_checklist.nf_set_checklist_subscriber_type('ANN')

/*
This module can only be opened if an individual is already identified.  
Accessing the module from the Verify checklist assumes that an individual number 
and claim role code are passed into the module.  If the module is opened for a 
surviving spouse a claim number will also be passed in.

The title of the tab displaying all the entitlement information will be: LOE Entitlement if the individual 
is an injured worker or SS Entitlement if the individual is a surviving spouse. The second tab that 
displays the qualification should be labeled: Qualification Entitlement

 birth date, death date and age of the individual on the Benefit Entitlement screen?  

*/

//PRE-SETUP STUFF
CHOOSE CASE il_action_code
	CASE 1//from Confirm Annuity Eligibility module's VBE checklist button
		
			// need the il_annuity_account_no populated if it isn't we have a problem
			IF ISNULL(il_annuity_account_no) OR il_annuity_account_no = 0 THEN
				messagebox('Invalid Annuity Account Number','No valid Annuity Account number has been supplied, the application cannot proceed. Contact the helpdesk')
				CLOSE(THIS)
			END IF 
				
			//	1.10	A Verify Benefit Entitlement checklist must only be created if benefit entitlement is created (has been _revised)
			IF wf_setup_for_verify(il_individual_no,il_claim_no,is_claim_role_code) = -1 THEN RETURN -1
	
			 //The checklist will not be displayed if opening the module from the menu and the checklist must not be updated.  
			uo_checklist.uf_set_checklist_visible()
			
			//SWITCH THE TITLE
			THIS.title= 'Verify Benefit Entitlement'
			
	CASE 2//MAINTAIN
		
		//depending on what calls this the title of the window changes
		THIS.title= 'Maintain Benefit Entitlement'
		
		 //The checklist will not be displayed if opening the module from the menu and the checklist must not be updated.  
		 uo_checklist.uf_set_checklist_invisible()
				
END CHOOSE

/* Displaying the Annuity Start and End Dates
	The dates should only be displayed if the module has been opened for the purpose of verifying 
	the annuity payout.   At this point the dates should be established and not changed by any 
	maintenance in this module.
*/
wf_populate_annuity_dates(il_annuity_account_no)

//set the global variable used in the visible property of the BE datawindow for  60% flag
gs_claim_role_code =  is_claim_role_code             

//do claim role specific stuff
IF  is_claim_role_code = 'SS' THEN
	
	ls_claim_no = string(il_claim_no)	
	
	idw_opening_master.dataobject = "d_entitlement_opening_ss"
	idw_opening_master.settransobject(sqlca)
	li_rowcount = idw_opening_master.retrieve(il_individual_no,il_claim_no)
	SQLCA.nf_handle_error("w_maintain_benefit_entitlement","open","idw_opening_master.retrieve(ll_individual_no,ll_claim_no)")
				
	// set award datawindow object  based on stored procedure
	idw_awards.dataobject = "d_entitlement_award_ss"
	idw_awards.settransobject(sqlca)
	
	li_rowcount = idw_awards.retrieve(il_individual_no, il_claim_no)
	SQLCA.nf_handle_error("w_maintain_benefit_entitlement","open","idw_awards.retrieve(il_individual_no) -- SS")
				
	tab_qualification.tabpage_entitlement.text = 'SS Entitlement'
			
ELSE
	
	idw_opening_master.dataobject = "d_entitlement_opening"//move this furthur down to use the instance array
	idw_opening_master.settransobject(sqlca)
	li_rowcount = idw_opening_master.retrieve(il_individual_no)
	SQLCA.nf_handle_error("w_maintain_benefit_entitlement","open","idw_opening_master.retrieve(ll_individual_no)")
							
	// set award datawindow object  based on stored procedure
	idw_awards.dataobject = "d_entitlement_award"
	idw_awards.settransobject(sqlca)
	
	li_rowcount = idw_awards.retrieve(il_individual_no)
	SQLCA.nf_handle_error("w_maintain_benefit_entitlement","open","idw_awards.retrieve(il_individual_no)-- C")	
			
	tab_qualification.tabpage_entitlement.text = 'LOE Entitlement'
	
	//shouldn't be a claim_no
	ls_claim_no = ''
	
END IF 

/* now retrieve the injured worker info - top datawindow */
wf_populate_worker_info(il_individual_no, ls_claim_no )
IF is_claim_role_code = 'SS' THEN
	dw_injured_worker.Object.individual_no_t.Text 	= 'Surviving Spouse'	
ELSE
	dw_injured_worker.Object.individual_no_t.Text 	= 'Injured Worker'
END IF 

/* determine initial state of frame */
iws_frame_open_state = w_frame.WindowState

/* tabpage_payments */
IF is_claim_role_code = 'SS' THEN
	li_rowcount = idw_payments.retrieve(il_individual_no,is_claim_role_code,il_claim_no)
	SQLCA.nf_handle_error("w_maintain_benefit_entitlement","open","idw_payments.retrieve(il_individual_no,is_claim_role_code,il_claim_no)")
		
ELSE
	li_rowcount = idw_payments.retrieve(il_individual_no,is_claim_role_code,0)
	SQLCA.nf_handle_error("w_maintain_benefit_entitlement","open"," idw_payments.retrieve(il_individual_no,is_claim_role_code,0)")
END IF 


//Populate the array of claim numbers - this will be used in the benefit entitlement datawindow
//when the user drags over payments - based on an external datawindow - should only have multiples for injured workers not for SS
//Can't do that because he is on the list to be confirmed.  They need to complete the checklist.
//We need a rule to stop this from happening. Don't do anything. (WG)
IF wf_populate_claim_array() = -1 THEN //NO CLAIMS
	//turn off all of the buttons
	cb_add.enabled 			= FALSE
	cb_create.enabled 		= FALSE
	cb_combine.enabled 		= FALSE
	cb_split.enabled 			= FALSE
	cb_temp_save.enabled 	= FALSE
END IF 

/* tabpage_overpayments */
/* tabpage_overpayments */
idw_overpayment.settransobject(sqlca)
li_rowcount = idw_overpayment.retrieve(il_individual_no)
SQLCA.nf_handle_error("w_maintain_benefit_entitlement","open","idw_overpayment.retrieve(il_individual_no)")

is_sort = 'recipient_no desc, claim_no desc, overpayment_detail_overpayment_date asc'
idw_overpayment.SetSort(is_sort)
idw_overpayment.Sort()

idw_overpayment.Groupcalc( )


/* populate the OPENINGS dropdown values */
/* populate the OPENINGS dropdown values */
ids_openings = CREATE u_ds
ids_openings.dataobject = 'dddw_openings_list_ann'
ids_openings.settransobject(sqlca)
li_row = ids_openings.Retrieve(il_claim_no_dddw[],is_claim_role_code)
SQLCA.nf_handle_error("w_maintain_benefit_entitlement","open","ids_openings.Retrieve(il_claim_no_dddw[])")

/* populate the Bencalc dropdown values */
/* populate the Bencalc dropdown values */
ids_bencalcs = CREATE u_ds
ids_bencalcs.dataobject = 'dddw_ben_calcs_for_annuities'
ids_bencalcs.settransobject(sqlca)
ids_bencalcs.Retrieve(il_claim_no_dddw[])
SQLCA.nf_handle_error("w_maintain_benefit_entitlement","open","ids_bencalcs.Retrieve(il_claim_no_dddw[])")

//added to include a 0 into the dddw
li_row = ids_bencalcs.InsertRow(1)
IF li_row > 0 THEN
	ids_bencalcs.SetItem(li_row,'benefit_calculation_no',0)
END IF


/***** NOTE MAY HAVE TO CHANGE WHEN THESE GET POPULATED ********/
//retrieve the dropdown datawindow children
wf_retrieve_dddw_claim_no(il_individual_no)
wf_retrieve_dddw_opening(il_claim_no)
wf_retrieve_dddw_benefit(il_claim_no)
wf_retrieve_payment_type_dddw(is_claim_role_code)

//populate the awardname
wf_populate_award_name()

/* tabpage_benefit_entitlement */
/* tabpage_benefit_entitlement & qualification*/
IF il_annuity_account_no > 0 THEN 
	THIS.POST wf_refresh_entitlement()
END IF 

li_upperbound = UpperBound(gstr_window_array) + 1

iul_handle = Handle(THIS)

gstr_window_array[li_upperbound].window_element = THIS
gstr_window_array[li_upperbound].handle_element 	= iul_handle

//* SPLITBAR */
tab_entitlement.tabpage_openings.st_horz_splitbar_opening.of_register(idw_opening_master)
tab_entitlement.tabpage_openings.st_horz_splitbar_opening.of_register(idw_opening_detail)
st_vert_splitbar_opening.of_register(idw_opening_master)
st_vert_splitbar_opening.of_register(idw_opening_detail)
st_vert_splitbar_opening.of_register(tab_entitlement.tabpage_openings.st_horz_splitbar_opening)
st_vert_splitbar_opening.of_register(tab_entitlement)
st_vert_splitbar_opening.of_register(idw_payments)
st_vert_splitbar_opening.of_register(idw_overpayment)
st_vert_splitbar_opening.of_register(idw_awards)
st_vert_splitbar_opening.of_register(tab_entitlement.tabpage_claim.dw_claim_information)

st_vert_splitbar_opening.of_register(tab_qualification)
// the datawindows on tab_qualification are resized as the tab control is resized

IF IsValid(inv_resize) THEN
ELSE
	inv_resize = CREATE n_resize
END IF

inv_resize.of_SetOrigSize (THIS.width, THIS.height)

//starts the resize service in the ancestor object (Move H,Move V,Grow H, Grow V)
//right
inv_resize.of_register(tab_qualification,50,0,50,100)
inv_resize.of_register(st_vert_splitbar_opening,50,0,0,100)

//left
inv_resize.of_register(tab_entitlement,0,0,50,100)
inv_resize.of_register(tab_entitlement.tabpage_openings.st_horz_splitbar_opening,0,0,50,0)
inv_resize.of_register(idw_opening_master,0,0,50,0)
inv_resize.of_register(idw_opening_detail,0,0,50,100)
inv_resize.of_register(idw_payments,0,0,50,100)
inv_resize.of_register(idw_awards,0,0,50,100)
inv_resize.of_register(idw_overpayment,0,0,50,100)
inv_resize.of_register(tab_entitlement.tabpage_claim.dw_claim_information,0,0,50,100)
inv_resize.of_register(cb_validate_entitlement,50,100,0,0)
inv_resize.of_register(cb_split,50,100,0,0)
inv_resize.of_register(cb_combine,50,100,0,0)
inv_resize.of_register(cb_create,50,100,0,0)
inv_resize.of_register(cb_refresh,50,100,0,0)
inv_resize.of_register(rb_non_zero,50,100,0,0)
inv_resize.of_register(rb_all,50,100,0,0)
inv_resize.of_register(cb_temp_save,50,100,0,0)
inv_resize.of_register(cb_extract,50,100,0,0)
inv_resize.of_register(cb_add,50,100,0,0)

/* maximize frame while module is envoked */
THIS.WindowState = Maximized!


// make report menu item not visible
inv_common_annuity.nf_make_menu_item_invisible(lw_win,'m_file','m_report')

THIS.POST EVENT ue_post_open()


end event

event closequery;call super::closequery;INTEGER 			li_count

li_count = wf_check_unsaved_changes('close')

IF li_count > 0 THEN
	RETURN 1
END IF
end event

event close;call super::close;INTEGER		li_counter, li_upper

inv_common_annuity.nf_close_handle_array(iul_handle)

li_upper = UpperBound(gstr_window_array)

FOR li_counter = 1 TO li_upper
	IF gstr_window_array[li_counter].window_element.ClassName() = 'w_confirm_annuity_eligibility' THEN
		gstr_window_array[li_counter].window_element.SetFocus()
	END IF
NEXT
end event

event resize;call super::resize;st_vert_splitbar_opening.of_SetRequestor(THIS)

function post wf_post_resize_checklist_position()
ib_checklist_posted = TRUE
end event

type rb_all from radiobutton within w_maintain_benefit_entitlement
boolean visible = false
integer x = 411
integer y = 2456
integer width = 402
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "All Balances"
boolean checked = true
end type

event clicked;/* The only filtering allowed on this tab is the ability to filter out all overpayments where the balance is zero. */
STRING 	ls_filter, ls_sort



ls_filter = ""

ls_sort = 'recipient_no desc, claim_no desc, overpayment_detail_overpayment_date asc'

//set and forget the filter
idw_overpayment.SetRedraw(false)

idw_overpayment.SetFilter(ls_filter)
idw_overpayment.Filter()

idw_overpayment.Retrieve(il_individual_no)
SQLCA.nf_handle_error('w_maintain_benefit_entitlement','idw_overpayment.Retrieve','cb_filter.clicked')

idw_overpayment.SetSort(ls_sort)
idw_overpayment.Sort()

idw_overpayment.GroupCalc()
idw_overpayment.SetRedraw(true)


//set the the filter text on the datawindow 
idw_overpayment.Object.t_filtered_count.Text = ''
end event

type rb_non_zero from radiobutton within w_maintain_benefit_entitlement
boolean visible = false
integer x = 416
integer y = 2392
integer width = 576
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Non-Zero Balances"
end type

event clicked;/* The only filtering allowed on this tab is the ability to filter out all overpayments where the balance is zero. */
STRING 	ls_filter, ls_filtered_text
LONG		ll_filtered_count


ls_filter = 'balance_amount <> 0'

//set and forget the filter
idw_overpayment.SetRedraw(false)

idw_overpayment.SetFilter(ls_filter)
idw_overpayment.Filter()

idw_overpayment.Retrieve(il_individual_no)
SQLCA.nf_handle_error('w_maintain_benefit_entitlement','idw_overpayment.Retrieve','cb_filter.clicked')

idw_overpayment.SetSort(is_sort)
idw_overpayment.Sort()

idw_overpayment.GroupCalc()
idw_overpayment.SetRedraw(true)

/*grab the filtered count and put it on the datawindow - otherwise set it to an empty
 string and the expression will make the column invisible
*/

ll_filtered_count = idw_overpayment.FilteredCount( )
ls_filtered_text = 'There are currently ' + string(ll_filtered_count) + ' filtered records'

//set the the filter text on the datawindow 
idw_overpayment.Object.t_filtered_count.Text = ls_filtered_text
end event

type dw_worker_details from u_dw_online within w_maintain_benefit_entitlement
integer x = 1682
integer y = 16
integer width = 800
integer height = 176
integer taborder = 0
string dataobject = "d_worker_details"
boolean border = false
end type

type uo_checklist from u_checklist within w_maintain_benefit_entitlement
integer x = 4658
integer y = 196
integer width = 119
integer taborder = 130
boolean ib_checklist_visible = true
boolean ib_cannot_change_status = true
long il_resize_steps = 25
end type

on uo_checklist.destroy
call u_checklist::destroy
end on

event ue_checklist_buttonclicked;call super::ue_checklist_buttonclicked;/*
module_code checklist_type_code checklist_step_type_code checklist_step_type_desc
----------- ------------------- ------------------------ ----------------------------------------
049         VBE                 001                      Identified
049         VBE                 006                      Verify Benefit Entitlement
049         VBE                 010                      Openings / Benefit Calculations
049         VBE                 011                      Payments
049         VBE                 012                      Awards
049         VBE                 013                      Overpayments
049         VBE                 014                      Events
049         VBE                 015                      Summary Sheets/Document List
049         VBE                 016                      Maintain Benefit Entitlement
049         VBE                 024                      Checklist Completed

Checklist must also conform to the following rules.
	
	1.10	A Verify Benefit Entitlement checklist must only be created if benefit entitlement is created.
	1.20	At most one incomplete checklist of type Verify Benefit Entitlement must exist at a time for an annuity account.
	1.30	The Verify Benefit Entitlement checklist must not be cancelled.
	1.40	The checklist must be complete before a new Verify Benefit Entitlement checklist can be created for the annuity account.
	1.50	The annuity account must exist prior to creating the Verify Benefit Entitlement checklist for the annuity account.
	1.60	A Verify Benefit Entitlement checklist must contain the following steps:
		·	Identified
		·	Openings/Benefit Calculations
		·	Payments
		·	Awards
		·	Overpayments
		·	Events
		·	Summary Sheet/Document List
		·	Verify Benefit Entitlement
		·	Checklist Completed
	1.70		All Verify Benefit Entitlement steps must be completed except the following:
		·	Events
		·	Summary Sheet/Document List
		
	1.90	The Verify Benefit Entitlement step must not be completed if there are outstanding changes to the benefit entitlement data.  See Rationale 
	1.100	The benefit entitlement data must not be changed once the Verify Benefit Entitlement step has been completed.  .  See Rationale
	1.110	The Verify Benefit Entitlement checklist step must not be completed if benefit entitlement spans the annuity start date.
	1.120 The Verify Benefit Entitlement checklist step must not be completed if benefit entitlement spans the annuity end date.

*/
STRING			ls_checklist_step_type_code, ls_window_to_open
DATE				ldt_null

IF adw_dw.ClassName() <> 'dw_checklist' THEN RETURN
	
ls_checklist_step_type_code 	= adw_dw.GetItemString(row,'checklist_step_type_code')
ls_window_to_open 				= adw_dw.GetItemString(row,'open_window_name')

CHOOSE CASE ls_checklist_step_type_code
	CASE '001' // Identified
	CASE '006' // Verify Benefit Entitlement
		//The Verify Benefit Entitlement check box is ticked on when benefit entitlement has been completed and fully verified.
		
	CASE '010' // Openings / Benefit Calculations
		//The Openings  Benefit Calculations button takes you to the Openings 
		//Benefit Calculations tab in the Maintain Benefit Entitlement module.
		
		tab_entitlement.selecttab(1)
		
	CASE '011' // Payments
		//The Payments button takes you to the Payments tab in the Maintain Benefit Entitlement module.
		
		tab_entitlement.selecttab(2)
		
	CASE '012' // Awards
		//The Awards button takes you to the Awards tab in the Maintain Benefit Entitlement module.
		tab_entitlement.selecttab(3)
		
	CASE '013' // Overpayments
		//The Overpayments button takes you to the Overpayments tab in the Maintain Benefit Entitlement module.
		tab_entitlement.selecttab(4)
		
	CASE '014' //The Events button opens the Event Log. 
		
		SetNull(ldt_null)
		inv_common_annuity.nf_open_event_log(ls_window_to_open, is_claim_role_code, 0, il_individual_no, ldt_null, '', '', '', '', '', 'Y', 'N','')
		
	CASE '015' //The Summary Sheets/Document List button opens the Individual/Claim Document List. 
		inv_common_annuity.nf_open_document_list(ls_window_to_open, is_claim_role_code, '049',il_claim_no,  il_individual_no)
		
	CASE '016' // Maintain Benefit Entitlement
	
	CASE '024' //  Checklist Completed
		/*	The final step in every checklist will be the Checklist Complete step.  This step must be completed manually, 
			and when the completed box is ticked for this final step, the user will be prompted with a ‘Are you sure?’
			type message as continuing will complete the full checklist. 
		*/	
										
	CASE ELSE
END CHOOSE

	idw_dw[1].SelectRow(0, false)
	idw_dw[1].SelectRow(row, true)


end event

event ue_checklist_itemchangeaccepted;call super::ue_checklist_itemchangeaccepted;INT			li_row
STRING		ls_checklist_step_type_code

SQLCA.nf_commit_transaction()

il_checklist_no = adw_dw.getitemnumber(al_row,"checklist_no")
inv_checklist.nf_retrieve_checklists('VBE',il_checklist_no)

li_row = inv_checklist.nf_get_next_checklist_step(il_checklist_no)

// zero will be returned when the checklist has been completed and there isn't any remaining steps.
IF li_row > 0 THEN
	adw_dw.SelectRow(0,false)
	adw_dw.SelectRow(li_row,true)
END IF

IF IsValid(idw_dw[1]) THEN
	idw_dw[1].setcolumn('checklist_step_status_code')
END IF


ls_checklist_step_type_code = adw_dw.GetItemString(al_row,'checklist_step_type_code')

CHOOSE CASE ls_checklist_step_type_code
	CASE '006'
		// Prevent users from saving, adding, etc.
		ib_locked = TRUE
	CASE ELSE
		
END CHOOSE
end event

event ue_checklist_buttonclicking;call super::ue_checklist_buttonclicking;INTEGER     li_count, li_rtn
STRING      ls_checklist_step_type_code
ULONG       lul_handle


// 1.170	Benefit Entitlement work must not continue for an individual who has been merged since the module was opened.
li_count = inv_common_annuity.nf_check_individual_exists(il_individual_no)
IF li_count = 0 THEN
	MessageBox('Individual Not Found','The individual number that you are working on no longer exists.'&
											+'~r~nThe individual may have been merged. The module will not be opened.'&
											+'~r~nPlease close this module and the Confirm Annuity Eligibility module (if it is open).', Exclamation!)
	al_return = 1
	GOTO Label_End
END IF

	
ls_checklist_step_type_code = adw_dw.GetItemString(row,'checklist_step_type_code')

CHOOSE CASE ls_checklist_step_type_code
	CASE '001'
		// Identified
	CASE '010'
		// Openings / Benefit Calculations
	CASE '011'
		// Payments
	CASE '012'
		// Awards
	CASE '013'
		// Overpayments
	CASE '014'
		// Events
		
		// check that window is not already open
		li_rtn = inv_common_annuity.nf_check_for_open_window('w_select_claims','Select Claim for Event Log',lul_handle)
		IF li_rtn = 1 THEN
			BringWindowToTop(lul_handle)
			al_return = 1
			GOTO Label_End
		END IF
		
	CASE '015'
		// Summary Sheets/Document List
		
		// check that window is not already open
		li_rtn = inv_common_annuity.nf_check_for_open_window('w_individual_doclist','Summary Sheets/Document List',lul_handle)
		IF li_rtn = 1 THEN
			BringWindowToTop(lul_handle)
			al_return = 1
			GOTO Label_End
		END IF
		
	CASE '006'
		// Verify Benefit Entitlement
	CASE '024'
		// Checklist Completed
		
END CHOOSE

al_return = 0

Label_End:

end event

event ue_checklist_itemchanged;call super::ue_checklist_itemchanged;STRING		ls_dwo_name, ls_checklist_step_type_code, ls_opening_type_code, ls_sixty_percent_flag, ls_message
STRING		ls_payment_type_code
INTEGER		li_count, li_rtn, li_counter,  li_rowcount, li_opening_count, li_percent_count
DWITEMSTATUS   ldwis_rowstatus
DATETIME	ldtm_null_date
LONG			ll_opening_no, ll_claim_no

/*
1.80	A Verify Benefit Entitlement checklist step must not be completed if active benefit entitlement data 
         exists for a claim where the claimant is inactive.  (See Rationale)
			
		The claimant can be made inactive on a claim if they are associated with the wrong claim.  
		If benefit entitlement is saved for a claim where the injured worker is no longer the active claimant 
		then this will need to be removed.  This will be check before the checklist can be completed.  
		The claimant could be made inactive for a variety of reasons.  For example the father and son may 
		have the same name and both are injured workers.  The incorrect individual could be selected for 
		the claim and then later corrected.  If benefit entitlement is saved before the correction this will 
		need to be corrected.
		
		This check should only be for an injured worker.
		For the surviving spouse the claimant_active_flag will always be 'N' (WG)
			
		1.90	The Verify Benefit Entitlement step must not be completed if there are outstanding changes to the benefit entitlement data.  See Rationale 
		1.100	The benefit entitlement data must not be changed once the Verify Benefit Entitlement step has been completed.  .  See Rationale
		1.110	The Verify Benefit Entitlement checklist step must not be completed if benefit entitlement spans the annuity start date.
		1.120 The Verify Benefit Entitlement checklist step must not be completed if benefit entitlement spans the annuity end date.
				
*/

IF is_claim_role_code <> 'SS' THEN
	li_count = inv_maintain_benefit.nf_get_claimant_active_flag(il_individual_no, is_claim_role_code)
	
	IF li_count <> 0 THEN 
		MESSAGEBOX ('Claimant Active Flag',' This individual ' + string(il_individual_no) +' is no longer the active claimant. '+&
		                                                      '~rPlease remove entitlement for this individual before completing the check list.' )
		al_return = 1
		GOTO Label_End
	END IF
END IF

ls_dwo_name = dwo.name

IF ls_dwo_name = 'checklist_step_status_code' THEN
	
	ls_checklist_step_type_code = adw_dw.GetItemString(row,'checklist_step_type_code')

		
	// 1.170	Benefit Entitlement work must not continue for an individual who has been merged since the module was opened.
	li_count = inv_common_annuity.nf_check_individual_exists(il_individual_no)
	IF li_count = 0 THEN
		MessageBox('Individual Not Found','The individual number that you are working on no longer exists.'&
												+'~r~nThe individual may have been merged. The checklist step will not be concluded.'&
												+'~r~nPlease close this module and the Confirm Annuity Eligibility module (if it is open).', Exclamation!)
		al_return = 2
		GOTO label_end
	END IF
	
	CHOOSE CASE ls_checklist_step_type_code
		CASE '001' // Identified
		CASE '006' // Verify Benefit Entitlement
				/*
					1.110	The Verify Benefit Entitlement checklist step must not be completed if benefit entitlement spans the annuity start date.
					1.120 The Verify Benefit Entitlement checklist step must not be completed if benefit entitlement spans the annuity end date.
				*/
				SetNull(ldtm_null_date)
				li_rtn = inv_common_annuity.nf_get_annuity_end_date(il_individual_no, is_claim_role_code, ls_message, idtm_annuity_end_date, ldtm_null_date, ldtm_null_date, ldtm_null_date)
				IF li_rtn < 0 THEN
					MessageBox('Annuity Problem',ls_message,Exclamation!)
					al_return = 2
					GOTO Label_End					
				END IF
				
				
				li_rtn = wf_other_process_date_check()
				IF li_rtn < 0 THEN
					al_return = 2
					GOTO Label_End
				END IF
				
				//	1.90	The Verify Benefit Entitlement step must not be completed if there are outstanding changes to the benefit entitlement data.  See Rationale 
				idw_benefit_entitlement.accepttext()
	
				li_rowcount = idw_benefit_entitlement.ROWCOUNT()
	
				li_count 				= 0
				li_opening_count 	= 0
				li_percent_count 	= 0
				
				FOR li_counter = 1 TO li_rowcount
					//CHECK THE ROW STATUS
					ldwis_rowstatus = idw_benefit_entitlement.GetItemStatus(li_counter,0,Primary!)
					IF ldwis_rowstatus = DATAModified! OR ldwis_rowstatus = NEWModified! THEN
						li_count ++
					END IF 
					
					/* 1.130 The Verify Benefit Entitlement checklist step must not be completed if all of the following are true:
						·	The benefit entitlement is for a surviving spouse
						·	At least one active benefit entitlement is associated with a Survivor 60% elected (S1) opening
						·	There is no active benefit entitlement with the 60% option lump sum indicator set to ‘Yes’
					*/
					IF is_claim_role_code = 'SS' THEN
						// grab the percent code  & the opening_type_code
						ls_payment_type_code 	= idw_benefit_entitlement.getitemstring(li_counter,'payment_type_code')
						ls_sixty_percent_flag  	= idw_benefit_entitlement.getitemstring(li_counter,'sixty_percent_flag')
						
						ll_opening_no =  idw_benefit_entitlement.getitemnumber(li_counter,'opening_no')
						ll_claim_no     =  idw_benefit_entitlement.getitemnumber(li_counter,'claim_no')
						
						IF ll_opening_no > 0 THEN
							ls_opening_type_code = inv_maintain_benefit.nf_get_opening_type(ll_claim_no, ll_opening_no)
						ELSE
	 						ls_opening_type_code = inv_maintain_benefit.nf_get_opening_type_from_combination(ls_payment_type_code)	
						END IF 
						
						IF ls_opening_type_code = '' THEN //ERROR OUT
						//ASSUME IT IS AN  SV
							ls_opening_type_code = 'SV'
						END IF 		

						 IF ls_opening_type_code = 'S1' THEN
						 	li_opening_count ++
						 END IF 
						 
						 IF	ls_sixty_percent_flag = 'Y' THEN
						 	li_percent_count ++
						 END IF 
						 
						 IF li_opening_count > 0 AND li_percent_count = 0 THEN
							MESSAGEBOX ('Verify Benefit Entitlement','The Verify Benefit Entitlement checklist step must not be completed due to the following reasons' +&
		                                             '~rThe Entitlement record has is for a Surviving Spouse with a S1 Opening type and lump sum indicator not equal to Yes.' )
							al_return = 1
							GOTO Label_End
						 END IF 
					END IF 	
				NEXT		
				 
				IF li_count > 0  THEN 
					MESSAGEBOX('Pending Changes', 'Please Refresh the Screen or Save Changes before you complete this step')
					al_return = 2
					
					GOTO Label_End
				END IF 	
			
		CASE '010' // Openings / Benefit Calculations
		CASE '011' // Payments		
		CASE '012' // Awards
		CASE '013' // Overpayments		
		CASE '014' // Events		
		CASE '015' // Summary Sheets/Document List
		CASE '016' // Maintain Benefit Entitlement	
		CASE '024' //  Checklist Completed
			
	END CHOOSE
	
ELSEIF ls_dwo_name = 'step_checked' THEN	
	al_return = 2
	GOTO Label_End
END IF

// if no errors then set as modified, rtn 0
adw_dw.SetItemStatus(row,ls_dwo_name,Primary!,DataModified!)
al_return = 0

SQLCA.nf_begin_transaction()

 Label_End:

end event

event ue_checklist_rowfocuschanged;call super::ue_checklist_rowfocuschanged;/*
module_code checklist_type_code checklist_step_type_code checklist_step_type_desc
----------- ------------------- ------------------------ ----------------------------------------
049         VBE                 001                      Identified
049         VBE                 006                      Verify Benefit Entitlement
049         VBE                 010                      Openings / Benefit Calculations
049         VBE                 011                      Payments
049         VBE                 012                      Awards
049         VBE                 013                      Overpayments
049         VBE                 014                      Events
049         VBE                 015                      Summary Sheets/Document List
049         VBE                 016                      Maintain Benefit Entitlement
049         VBE                 024                      Checklist Completed

Checklist must also conform to the following rules.
	
	1.10	A Verify Benefit Entitlement checklist must only be created if benefit entitlement is created.
	1.20	At most one incomplete checklist of type Verify Benefit Entitlement must exist at a time for an annuity account.
	1.30	The Verify Benefit Entitlement checklist must not be cancelled.
	1.40	The checklist must be complete before a new Verify Benefit Entitlement checklist can be created for the annuity account.
	1.50	The annuity account must exist prior to creating the Verify Benefit Entitlement checklist for the annuity account.
	1.60	A Verify Benefit Entitlement checklist must contain the following steps:
		·	Identified
		·	Openings/Benefit Calculations
		·	Payments
		·	Awards
		·	Overpayments
		·	Events
		·	Summary Sheet/Document List
		·	Verify Benefit Entitlement
		·	Checklist Completed
	1.70		All Verify Benefit Entitlement steps must be completed except the following:
		·	Events
		·	Summary Sheet/Document List
*/

STRING			ls_checklist_step_type_code, ls_window_to_open

if isnull(adw_dw) then return

IF adw_dw.ClassName() <> 'dw_checklist' THEN RETURN

IF currentrow <= 0 OR ISNULL(currentrow) THEN RETURN
	
ls_checklist_step_type_code 	= adw_dw.GetItemString(currentrow,'checklist_step_type_code')
ls_window_to_open 				= adw_dw.GetItemString(currentrow,'open_window_name')

CHOOSE CASE ls_checklist_step_type_code
	CASE '001' // Identified
	CASE '006' // Verify Benefit Entitlement
		
	CASE '010' // Openings / Benefit Calculations
		
		tab_entitlement.selecttab(1)
		
	CASE '011' // Payments
		//The Payments button takes you to the Payments tab in the Maintain Benefit Entitlement module.
		
		tab_entitlement.selecttab(2)
		
	CASE '012' // Awards
		//The Awards button takes you to the Awards tab in the Maintain Benefit Entitlement module.
		tab_entitlement.selecttab(3)
		
	CASE '013' // Overpayments
		//The Overpayments button takes you to the Overpayments tab in the Maintain Benefit Entitlement module.
		tab_entitlement.selecttab(4)
		
	CASE '014' // Events		
	CASE '015' // Summary Sheets/Document List
	CASE '016' // Maintain Benefit Entitlement	
	CASE '024' //  Checklist Completed
					
	CASE ELSE
END CHOOSE

end event

event ue_checklist_note_save_btn_clicking;call super::ue_checklist_note_save_btn_clicking;INTEGER      li_count


// 1.170	Benefit Entitlement work must not continue for an individual who has been merged since the module was opened.
li_count = inv_common_annuity.nf_check_individual_exists(il_individual_no)
IF li_count = 0 THEN
	// cancel note save
	MessageBox('Individual Not Found','The individual number that you are working on no longer exists.'&
											+'~r~nThe individual may have been merged. The checklist note will not be saved.'&
											+'~r~nPlease close this module and the Confirm Annuity Eligibility module (if it is open).', Exclamation!)
	al_return = -1
	GOTO Label_End
END IF

al_return = 0

Label_End:
end event

event ue_checklist_step_status_changed;call super::ue_checklist_step_status_changed;INTEGER         li_rtn
DATAWINDOWCHILD ldwc_child

IF as_status_assigned_method_code = '' THEN
	
	li_rtn = uo_checklist.tab_checklist.tabpage_checklist.dw_checklist.GetChild('checklist_step_status_code',ldwc_child)
	
	as_status_assigned_method_code = ldwc_child.GetItemString(ldwc_child.GetRow(),'status_assigned_method_code')

END IF
end event

type tab_entitlement from tab within w_maintain_benefit_entitlement
integer y = 224
integer width = 2208
integer height = 2172
integer taborder = 10
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
tabpage_openings tabpage_openings
tabpage_payments tabpage_payments
tabpage_awards tabpage_awards
tabpage_overpayment tabpage_overpayment
tabpage_claim tabpage_claim
end type

on tab_entitlement.create
this.tabpage_openings=create tabpage_openings
this.tabpage_payments=create tabpage_payments
this.tabpage_awards=create tabpage_awards
this.tabpage_overpayment=create tabpage_overpayment
this.tabpage_claim=create tabpage_claim
this.Control[]={this.tabpage_openings,&
this.tabpage_payments,&
this.tabpage_awards,&
this.tabpage_overpayment,&
this.tabpage_claim}
end on

on tab_entitlement.destroy
destroy(this.tabpage_openings)
destroy(this.tabpage_payments)
destroy(this.tabpage_awards)
destroy(this.tabpage_overpayment)
destroy(this.tabpage_claim)
end on

event selectionchanged;
rb_all.visible 				= FALSE
rb_non_zero.visible 			= FALSE

CHOOSE CASE newindex
	CASE 1
		
		
	CASE 2
		
		idw_payments.width = idw_opening_detail.width
		
	CASE 3
		
		idw_awards.width = idw_opening_detail.width
		
	CASE 4
		
		idw_overpayment.width 	= idw_opening_detail.width
		rb_all.visible 				= TRUE
		rb_non_zero.visible 			= TRUE
		
	CASE 5//Claim_information

		IF is_claim_role_code = 'SS' THEN
			tab_entitlement.tabpage_claim.dw_claim_information.dataobject = 'd_be_claim_information_ss'
		ELSE
			tab_entitlement.tabpage_claim.dw_claim_information.dataobject = 'd_be_claim_information_iw'
		END IF 

		tab_entitlement.tabpage_claim.dw_claim_information.settransobject(sqlca)
		tab_entitlement.tabpage_claim.dw_claim_information.retrieve(il_individual_no, is_claim_role_code)
		SQLCA.nf_handle_error("w_maintain_benefit_entitlement","tab_control - selection changed"," tab_entitlement.tabpage_claim.dw_claim_information.retrieve(il_individual_no)")

END CHOOSE
end event

type tabpage_openings from userobject within tab_entitlement
integer x = 18
integer y = 108
integer width = 2171
integer height = 2048
long backcolor = 67108864
string text = "Openings/Ben Calcs"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
st_horz_splitbar_opening st_horz_splitbar_opening
dw_opening_detail dw_opening_detail
dw_opening_master dw_opening_master
end type

on tabpage_openings.create
this.st_horz_splitbar_opening=create st_horz_splitbar_opening
this.dw_opening_detail=create dw_opening_detail
this.dw_opening_master=create dw_opening_master
this.Control[]={this.st_horz_splitbar_opening,&
this.dw_opening_detail,&
this.dw_opening_master}
end on

on tabpage_openings.destroy
destroy(this.st_horz_splitbar_opening)
destroy(this.dw_opening_detail)
destroy(this.dw_opening_master)
end on

type st_horz_splitbar_opening from u_splitbar_horizontal within tabpage_openings
integer y = 1128
integer width = 2176
integer height = 40
borderstyle borderstyle = styleraised!
long il_min_units_from_top = 600
long il_min_units_from_bottom = 1000
long il_offset = 500
end type

type dw_opening_detail from u_dw_online within tabpage_openings
integer y = 1176
integer width = 2171
integer height = 856
integer taborder = 11
string dataobject = "d_entitlement_bencalc"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;settransobject(sqlca)
THIS.uf_setselect(1)
end event

event rbuttondown;M_DW_RMB_POPUP lm_popup
window		lw_window

IF IsValid(inv_rowselection) Then
	inv_rowselection.event pfc_rbuttondown(xpos,ypos,row,dwo)
End if


/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
lm_popup = Create m_dw_rmb_popup
lm_popup.mf_set_datawindow(This)

// don't display lines
lm_popup.m_options.m_1.Visible = FALSE
lm_popup.m_options.m_2.Visible = FALSE

lw_window = wf_get_window_reference()	

lm_popup.m_options.PopMenu(lw_window.PointerX( ), lw_window.PointerY( ))

Destroy lm_popup
end event

type dw_opening_master from u_dw_online within tabpage_openings
integer y = 16
integer width = 2181
integer height = 1100
integer taborder = 11
string dataobject = "d_entitlement_opening"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;settransobject(sqlca)
THIS.uf_setselect(1)

end event

event rowfocuschanged;call super::rowfocuschanged;LONG		ll_bencalc_no, ll_claim_no

IF currentrow <> 0 AND  idw_opening_master.rowcount() > 0 THEN
	// need to grab the bencalc number to pass into the detail datawindow
	ll_bencalc_no 	= idw_opening_master.getitemnumber(idw_opening_master.getrow(),"opening_no")
	ll_claim_no		= idw_opening_master.getitemnumber(idw_opening_master.getrow(),"claim_no")
	idw_opening_detail.settransobject(sqlca)
	idw_opening_detail.retrieve(ll_bencalc_no, ll_claim_no)
	SQLCA.nf_handle_error("w_maintain_benefit_entitlement","tabpage_openings - dw_opening_master","rowfocuschanged-idw_opening_detail.retrieve(ll_bencalc_no)")
END IF
end event

event rbuttondown;M_DW_RMB_POPUP lm_popup
window		lw_window

IF IsValid(inv_rowselection) Then
	inv_rowselection.event pfc_rbuttondown(xpos,ypos,row,dwo)
End if


/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
lm_popup = Create m_dw_rmb_popup
lm_popup.mf_set_datawindow(This)

// don't display lines
lm_popup.m_options.m_1.Visible = FALSE
lm_popup.m_options.m_2.Visible = FALSE

lw_window = wf_get_window_reference()	

lm_popup.m_options.PopMenu(lw_window.PointerX( ), lw_window.PointerY( ))

Destroy lm_popup
end event

type tabpage_payments from userobject within tab_entitlement
integer x = 18
integer y = 108
integer width = 2171
integer height = 2048
long backcolor = 67108864
string text = "Payments"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_payments dw_payments
end type

on tabpage_payments.create
this.dw_payments=create dw_payments
this.Control[]={this.dw_payments}
end on

on tabpage_payments.destroy
destroy(this.dw_payments)
end on

type dw_payments from u_dw_online within tabpage_payments
integer y = 20
integer width = 2171
integer height = 2008
integer taborder = 11
string dragicon = "Query5!"
string dataobject = "d_entitlement_payment_list"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;
THIS.uf_SetSelect(THIS.RS_EXTENDED)
this.settransobject(sqlca)
end event

event clicked;call super::clicked;STRING		ls_reselectable
DATETIME	ldtm_processed_date

IF ib_locked THEN
	MessageBox('No Data Entry','The "Verify" step has been completed. You must complete this checklist and start another before you can add new data.',Exclamation!)
	RETURN
END IF

IF row <> 0 THEN
	ldtm_processed_date = THIS.getitemdatetime(row,'payment_processed_date')
	ls_reselectable = this.getitemstring(row,'reselectable_flag')
	IF  ls_reselectable = 'N' 			THEN RETURN
	IF isnull(ldtm_processed_date) THEN RETURN
	
	ib_payment_list_drag = TRUE
	il_mousemove 			= 0
END IF

end event

event dragdrop;call super::dragdrop;datawindow		ldw_source

IF ib_selected_payment_drag = True THen
	IF source.TypeOf() = DataWindow! THEN
			ldw_Source = source
			
			ib_selected_payment_drag = false
			
			//maybe need to stop the drag if the record is not selectable
			/*
			IF GetItemString(al_row,'selectable_flag') = 'Y' Then
				return 1
			Else
				return -1
			End if
			*/
	END IF
End if
end event

event dwmousemove;call super::dwmousemove;IF KeyDown(KeyLeftButton!) Then
	IF ib_payment_list_drag = TRUE Then		
		IF il_mousemove >= 1 THEN
			This.Drag(Begin!)
		End if		
		il_mousemove ++
	End if
End if
end event

event lbuttonup;call super::lbuttonup;IF ib_payment_list_drag = True Then
	ib_payment_list_drag = False
	This.Drag(End!)
End if
end event

event ue_rowselecting;call super::ue_rowselecting;//IF GetItemString(al_row,'payment_flag') = 'Y' AND NOT ISNULL(getitemdatetime(al_row,'payment_processed_date')) Then
IF GetItemString(al_row,'reselectable_flag') = 'Y' AND NOT ISNULL(getitemdatetime(al_row,'payment_processed_date')) Then
	RETURN 1
ELSE
	RETURN -1
END IF
//(WG EMAIL)

RETURN 1
end event

event rbuttondown;/* See if the selectedrow is an award & selectable*/

//award_no should be 0 going into this
LONG 			ll_award_no
STRING 		ls_reselectable
INTEGER 	   li_counter, li_rowcount
WINDOW		lw_window


//make sure we have some rows
li_rowcount = idw_payments.rowcount()

IF li_rowcount = 0 THEN RETURN 0
IF row <= 0 THEN RETURN 0

//grab the award_no from the selected row
ll_award_no 	= idw_payments.getitemnumber(row,'award_no')
ls_reselectable 	= idw_payments.getitemstring(row,'reselectable_flag')

IF ls_reselectable = 'Y' THEN 
	idw_payments.selectrow(0, false)
	idw_payments.selectrow(row, true)
	
	m_benefit_action		im_menu
	im_menu = CREATE m_benefit_action
	
	lw_window = wf_get_window_reference()
	
	im_menu.m_action.Popmenu(lw_window.Pointerx(),lw_window.Pointery())
END IF 

end event

event doubleclicked;call super::doubleclicked;LONG			ll_payment_no

IF isnull(row) OR row < 1  THEN RETURN 

//grab the payment_no
ll_payment_no = idw_payments.getitemnumber(row,'payment_no')

//make sure it is valid
IF isnull(ll_payment_no) OR ll_payment_no = 0 THEN RETURN 

openwithparm(w_payment_details_for_entitlement, ll_payment_no)
end event

event buttonclicked;call super::buttonclicked;String    ls_Object

ls_Object = String(dwo.name)

If ls_Object = "b_extract" Then
	IF THIS.RowCount() = 0 THEN
		RETURN
	END IF
	
	THIS.Saveas('',Excel!,TRUE)
End If
end event

type tabpage_awards from userobject within tab_entitlement
event create ( )
event destroy ( )
integer x = 18
integer y = 108
integer width = 2171
integer height = 2048
long backcolor = 67108864
string text = "Awards"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_awards dw_awards
end type

on tabpage_awards.create
this.dw_awards=create dw_awards
this.Control[]={this.dw_awards}
end on

on tabpage_awards.destroy
destroy(this.dw_awards)
end on

type dw_awards from u_dw_online within tabpage_awards
integer y = 20
integer width = 2167
integer height = 2016
integer taborder = 21
string dataobject = "d_entitlement_award"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;THIS.uf_setselect(1)
end event

event rowfocuschanged;call super::rowfocuschanged;
end event

event rbuttondown;M_DW_RMB_POPUP lm_popup
window		lw_window

IF IsValid(inv_rowselection) Then
	inv_rowselection.event pfc_rbuttondown(xpos,ypos,row,dwo)
End if


/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
lm_popup = Create m_dw_rmb_popup
lm_popup.mf_set_datawindow(This)

// don't display lines
lm_popup.m_options.m_1.Visible = FALSE
lm_popup.m_options.m_2.Visible = FALSE

lw_window = wf_get_window_reference()	

lm_popup.m_options.PopMenu(lw_window.PointerX( ), lw_window.PointerY( ))

Destroy lm_popup
end event

type tabpage_overpayment from userobject within tab_entitlement
event create ( )
event destroy ( )
integer x = 18
integer y = 108
integer width = 2171
integer height = 2048
long backcolor = 67108864
string text = "Overpayment"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_overpayment dw_overpayment
end type

on tabpage_overpayment.create
this.dw_overpayment=create dw_overpayment
this.Control[]={this.dw_overpayment}
end on

on tabpage_overpayment.destroy
destroy(this.dw_overpayment)
end on

type dw_overpayment from u_dw_online within tabpage_overpayment
integer y = 12
integer width = 2162
integer height = 2020
integer taborder = 21
string dataobject = "d_entitlement_overpayment"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;THIS.uf_setselect(1)

end event

event rowfocuschanged;call super::rowfocuschanged;//LONG			ll_annuity_account_no, ll_checklist_no, ll_individual_no
//INTEGER		li_rows, li_step_no, li_row
//STRING		ls_name
//
//IF currentrow <> 0 THEN
//	IF is_annuity_eligibility_run_option_code = 'IW' OR is_annuity_eligibility_run_option_code = 'IA' THEN
//		ll_annuity_account_no = THIS.GetItemNumber(currentrow,'annuity_account_no')
//		ll_individual_no = THIS.GetItemNumber(currentrow,'individual_no')
//		ll_checklist_no = THIS.GetItemNumber(currentrow,'checklist_no')
//		ls_name = THIS.GetItemString(currentrow,'name')
//		
//		// current & history eligibility datawindows
//		li_rows = tab_eligibility.tabpage_current_eligibility.dw_current_eligibility.Retrieve(ll_annuity_account_no)
//		li_rows = tab_eligibility.tabpage_history_eligibility.dw_history_eligibility.Retrieve(ll_annuity_account_no)
//		
//		// checklist datawindows		
//		li_rows = tab_checklist.tabpage_checklist.dwindow.Retrieve('VBE',ll_checklist_no)
//		tab_checklist.tabpage_checklist.uo_checklist.st_checklist_name.text = ls_name + ' - ' + String(ll_individual_no)
//		li_row = tab_checklist.tabpage_checklist.dwindow.GetRow()
//		li_step_no = tab_checklist.tabpage_checklist.dwindow.GetItemNumber(li_row,'checklist_step_no')
//		
//		li_rows = tab_checklist.tabpage_checklist_notes.dwindow_notes.Retrieve(ll_checklist_no)
//		li_rows = tab_checklist.tabpage_checklist_notes.dwindow_notes_entered.Retrieve(ll_checklist_no,li_step_no)
//		
//		tab_checklist.tabpage_checklist_history.dwindow_history_master.DataObject = 'd_checklist_history_master'
//		tab_checklist.tabpage_checklist_history.dwindow_history_master.SetTransObject(SQLCA)
//		tab_checklist.tabpage_checklist_history.dwindow_history_detail.DataObject = 'd_checklist_history_details'
//		tab_checklist.tabpage_checklist_history.dwindow_history_detail.SetTransObject(SQLCA)
//		li_rows = tab_checklist.tabpage_checklist_history.dwindow_history_master.Retrieve(ll_annuity_account_no)
//		li_rows = tab_checklist.tabpage_checklist_history.dwindow_history_detail.Retrieve(ll_checklist_no)
//		
//	END IF
//	
//END IF
end event

event ue_print;THIS.object.datawindow.print.orientation = 1  //Landscape
THIS.Print()
end event

event rbuttondown;M_DW_RMB_POPUP lm_popup
window		lw_window

IF IsValid(inv_rowselection) Then
	inv_rowselection.event pfc_rbuttondown(xpos,ypos,row,dwo)
End if


/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
lm_popup = Create m_dw_rmb_popup
lm_popup.mf_set_datawindow(This)

// don't display lines
lm_popup.m_options.m_1.Visible = FALSE
lm_popup.m_options.m_2.Visible = FALSE

lw_window = wf_get_window_reference()	

lm_popup.m_options.PopMenu(lw_window.PointerX( ), lw_window.PointerY( ))

Destroy lm_popup
end event

type tabpage_claim from userobject within tab_entitlement
integer x = 18
integer y = 108
integer width = 2171
integer height = 2048
long backcolor = 67108864
string text = "Claim Information"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_claim_information dw_claim_information
end type

on tabpage_claim.create
this.dw_claim_information=create dw_claim_information
this.Control[]={this.dw_claim_information}
end on

on tabpage_claim.destroy
destroy(this.dw_claim_information)
end on

type dw_claim_information from u_dw_online within tabpage_claim
integer y = 24
integer width = 2176
integer height = 2020
integer taborder = 11
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;THIS.uf_setselect(1)
end event

event rbuttondown;M_DW_RMB_POPUP lm_popup
window		lw_window

IF IsValid(inv_rowselection) Then
	inv_rowselection.event pfc_rbuttondown(xpos,ypos,row,dwo)
End if


/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
lm_popup = Create m_dw_rmb_popup
lm_popup.mf_set_datawindow(This)

// don't display lines
lm_popup.m_options.m_1.Visible = FALSE
lm_popup.m_options.m_2.Visible = FALSE

lw_window = wf_get_window_reference()	

lm_popup.m_options.PopMenu(lw_window.PointerX( ), lw_window.PointerY( ))

Destroy lm_popup
end event

type dw_injured_worker from u_dw_online within w_maintain_benefit_entitlement
integer x = 5
integer y = 8
integer width = 1650
integer height = 144
integer taborder = 0
boolean bringtotop = true
string dataobject = "d_entitlement_worker_info"
boolean border = false
end type

event constructor;call super::constructor;settransobject(sqlca)
end event

type dw_annuity_dates from u_dw_online within w_maintain_benefit_entitlement
integer x = 2501
integer y = 4
integer width = 2048
integer height = 180
integer taborder = 0
boolean bringtotop = true
string dataobject = "d_entitlement_annuity_dates"
boolean border = false
end type

event constructor;call super::constructor;settransobject(sqlca)
end event

type st_vert_splitbar_opening from u_splitbar_vertical within w_maintain_benefit_entitlement
integer x = 2231
integer y = 308
integer width = 23
integer height = 2076
boolean bringtotop = true
long il_min_units_from_left = 200
long il_min_units_from_right = 200
end type

event ue_moved;call super::ue_moved;

tab_entitlement.tabpage_openings.st_horz_splitbar_opening.x = idw_payments.x
tab_entitlement.tabpage_openings.st_horz_splitbar_opening.width = idw_payments.width
end event

type tab_qualification from tab within w_maintain_benefit_entitlement
event create ( )
event destroy ( )
event ue_resize pbm_size
integer x = 2267
integer y = 216
integer width = 2286
integer height = 2176
integer taborder = 20
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
tabpage_entitlement tabpage_entitlement
tabpage_qualification tabpage_qualification
end type

on tab_qualification.create
this.tabpage_entitlement=create tabpage_entitlement
this.tabpage_qualification=create tabpage_qualification
this.Control[]={this.tabpage_entitlement,&
this.tabpage_qualification}
end on

on tab_qualification.destroy
destroy(this.tabpage_entitlement)
destroy(this.tabpage_qualification)
end on

event ue_resize;inv_tab_resize.Event pfc_Resize(sizetype, This.width, This.Height)
end event

event constructor; inv_tab_resize = create n_resize
end event

type tabpage_entitlement from userobject within tab_qualification
event create ( )
event destroy ( )
integer x = 18
integer y = 108
integer width = 2249
integer height = 2052
long backcolor = 67108864
string text = "Benefit Entitlement"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_benefit_entitlement dw_benefit_entitlement
end type

on tabpage_entitlement.create
this.dw_benefit_entitlement=create dw_benefit_entitlement
this.Control[]={this.dw_benefit_entitlement}
end on

on tabpage_entitlement.destroy
destroy(this.dw_benefit_entitlement)
end on

type dw_benefit_entitlement from u_dw_online within tabpage_entitlement
integer y = 16
integer width = 2245
integer height = 2020
integer taborder = 31
string dataobject = "d_benefit_entitlement"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;settransobject(sqlca)
THIS.uf_setselect(1)

end event

event dragdrop;call super::dragdrop;/*
If multiple payments are selected the user will be given the option of creating an entitlement 
for each payment selected or for combining all selected payments into one entitlement.
*/

IF ib_locked THEN
	MessageBox('No Data Entry','The "Verify" step has been completed. You must complete this checklist and start another before you can add new data.',Exclamation!)
	RETURN 0
END IF

cb_create.triggerevent(clicked!)

ib_selected_payment_drag = FALSE

end event

event dwmousemove;call super::dwmousemove;IF KeyDown(KeyLeftButton!) Then
	IF ib_selected_payment_drag = TRUE Then
		IF il_mousemove >= 1 THEN
			This.Drag(Begin!)
		End if		
		il_mousemove ++
	End if
End if

RETURN

end event

event lbuttonup;call super::lbuttonup;IF ib_selected_payment_drag = True THEN
	ib_selected_payment_drag = FALSE
	THIS.Drag(End!)	
END IF 
end event

event itemchanged;call super::itemchanged;/*
The user will also need the ability to delete a benefit entitlement entry.  
There will need to be a checkbox to allow the user to mark an entitlement as deleted. 
The user can uncheck the box as long as the data has not yet been saved. 
Once the data is saved none of the ‘deleted’ data will display again.  
The data will be marked as deleted (deleted_flag = ‘Y’) and the deleted date will be set to the current date.    
*/
STRING 			ls_deleted_flag, ls_opening_type, ls_award_type, ls_find, ls_ben_freq
STRING			ls_dropvalue, ls_ben_freq_code, ls_payment_type
DATETIME 		ldtm_deleted_date, ldtm_effective_from_date, ldtm_opening_start, ldtm_opening_end
LONG				ll_opening_no, ll_claim_no, ll_bencalc_no, ll_row
INTEGER			li_find, li_counter , li_rowcount
DATAWINDOWCHILD	ldwc_child_bencalc, ldwc_child_opening
DECIMAL			ldec_benefit_amount, ldec_award_amount


//enable the save button
cb_temp_save.enabled = TRUE

//grab the value of the deleted_flag
CHOOSE CASE dwo.name
	CASE 'deleted_flag'
			IF data = 'Y' THEN	
				ldtm_deleted_date = f_server_datetime()		
				THIS.setitem(row, 'deleted_date', date(ldtm_deleted_date))
			ELSE
				SETNULL(ldtm_deleted_date)
				THIS.setitem(row, 'deleted_date', ldtm_deleted_date)	
			END IF 
	CASE 'claim_no'
		
			ll_claim_no 		= long(data)
			ll_opening_no 	= THIS.getitemnumber(row, 'opening_no_filtered')
			ll_bencalc_no 	= THIS.getitemnumber(row, 'benefit_calculation_no_filtered')
	
			IF ISNULL(ll_opening_no) 	THEN ll_opening_no 	= 0
			IF ISNULL(ll_bencalc_no) 	THEN ll_bencalc_no 	= 0
			IF ISNULL(ll_claim_no) 		THEN ll_bencalc_no 	= 0
				
			/************** opening_no ***************/
			wf_filter_dddw_opening(ll_claim_no)
			
			/******* benefit_calculation_no ***************/
			wf_filter_dddw_benefit_no(ll_claim_no)
			
			/**********payment_type*******************/
			wf_filter_dddw_payment_type(ll_opening_no, ll_bencalc_no,ll_claim_no )
			
			//now set each to 0 or '' for payment type
			idw_benefit_entitlement.GetChild('benefit_calculation_no_filtered', ldwc_child_bencalc)
			idw_benefit_entitlement.GetChild('opening_no_filtered', ldwc_child_opening)
			
			//opening_no to 0
			ls_find 	= 'opening_no = ' + string(0)
			li_find 	= ldwc_child_bencalc.Find(ls_find, 1, ldwc_child_opening.RowCount())
			ldwc_child_opening.ScrollToRow(li_find)
			idw_benefit_entitlement.setitem(row,'opening_no_filtered',0)
			
			//bencalc to 0
			ls_find 	= 'benefit_calculation_no = ' + string(0)
			li_find 	= ldwc_child_bencalc.Find(ls_find, 1, ldwc_child_bencalc.RowCount())
			ldwc_child_bencalc.ScrollToRow(li_find)
			idw_benefit_entitlement.setitem(row,'benefit_calculation_no_filtered',0)
			
			/********* OPENING TEXT ********************/
			//wf_retrieve_display_value(1,opening_no,claim_no,bencalc)
			ls_dropvalue = wf_retrieve_display_value(1, 0, ll_claim_no, 0)
			
			IF ls_dropvalue <> '' THEN
				idw_benefit_entitlement.setitem(row,'opening_text',ls_dropvalue)
			ELSE
				idw_benefit_entitlement.setitem(row,'opening_text','0')
			END IF 
						
			/********* BENCALC TEXT ********************/
			ls_dropvalue = wf_retrieve_display_value(2, 0, ll_claim_no, 0)
			
			IF ls_dropvalue <> '' THEN
				idw_benefit_entitlement.setitem(row,'bencalc_text',ls_dropvalue)
			ELSE
				idw_benefit_entitlement.setitem(row,'bencalc_text','0')
			END IF 
										
	CASE 'opening_no_filtered'
		
			ll_opening_no = long(data) 
			IF ll_opening_no = idw_benefit_entitlement.getitemnumber(row, 'opening_no_filtered') THEN
			END IF 
			
			
			idw_benefit_entitlement.GetChild('benefit_calculation_no_filtered', ldwc_child_bencalc)
			idw_benefit_entitlement.GetChild('opening_no_filtered', ldwc_child_opening)
			
			ll_claim_no 	= THIS.getitemnumber(row, 'claim_no')

	 
			IF ISNULL(ll_opening_no) 	THEN ll_opening_no 	= 0
			IF ISNULL(ll_claim_no) 		THEN ll_claim_no 	= 0

			//just set the bencalc_no to 0
			ll_bencalc_no = 0
					
			//if rowcount = 1 then only benefit calc of 0
			IF ll_opening_no = 0 THEN		
				ldwc_child_bencalc.SetFilter("benefit_calculation_no = 0")
			ELSE
				ldwc_child_bencalc.SetFilter("(opening_no = " + string(ll_opening_no) + " AND claim_no = " + string(ll_claim_no) + ")" + " or benefit_calculation_no = 0")
			END IF
			
			ldwc_child_bencalc.Filter()
			
			//just set the bencalc to 0
			idw_benefit_entitlement.setitem(idw_benefit_entitlement.getrow(),'benefit_calculation_no',0)
			
			ldwc_child_bencalc.SetSort('benefit_calculation_no A')
			ldwc_child_bencalc.Sort()
			
			/********* OPENING TEXT ********************/
			//wf_retrieve_display_value(1,opening_no,claim_no,bencalc)
			ls_dropvalue = wf_retrieve_display_value(1, ll_opening_no, ll_claim_no, 0)
			
			IF ls_dropvalue <> '' THEN
				idw_benefit_entitlement.setitem(row,'opening_text',ls_dropvalue)
			ELSE
				idw_benefit_entitlement.setitem(row,'opening_text','0')
			END IF 
						
			/********* BENCALC TEXT ********************/
			ls_dropvalue = wf_retrieve_display_value(2, 0, ll_claim_no, 0)
			
			IF ls_dropvalue <> '' THEN
				idw_benefit_entitlement.setitem(row,'bencalc_text',ls_dropvalue)
			ELSE
				idw_benefit_entitlement.setitem(row,'bencalc_text','0')
			END IF 	
			
		/**********effective_date*******************/
			//need to grab the award amount and put it in the BE award amount column on the BE datawindow
			ldtm_effective_from_date = inv_maintain_benefit.nf_get_effective_date(ll_claim_no, ll_bencalc_no, ll_opening_no)
			idw_benefit_entitlement.setitem(row, 'effective_from_date', ldtm_effective_from_date)
			
		/**********payment_type*******************/
		//	idw_benefit_entitlement.setitem(row,'payment_type_filtered',' ')
			wf_filter_dddw_payment_type(ll_opening_no, ll_bencalc_no, ll_claim_no )
			
		/********* Calculate the Amounts *************/
			//do the calculation
			ldec_benefit_amount = wf_calculate_amount(row, 0, 5)
			
			IF ISNULL(ldec_benefit_amount) OR ldec_benefit_amount < 0 THEN ldec_benefit_amount = 0
			
			//SETITEM TO benefit_entitlement_amount
			idw_benefit_entitlement.setitem(row, 'be_total_award_amount', round(ldec_benefit_amount,2))
				
	CASE 'benefit_calculation_no_filtered'	
		
	/**********payment_type*******************/
			ll_opening_no 	= THIS.getitemnumber(row,'opening_no_filtered')
			ll_bencalc_no   = long(data)
			ll_claim_no		= THIS.getitemnumber(row, 'claim_no')
	 
			IF ISNULL(ll_opening_no)	THEN ll_opening_no 	= 0
			IF ISNULL(ll_bencalc_no) 	THEN ll_bencalc_no 	= 0
	
			/* setup the payment type information */
			//idw_benefit_entitlement.setitem(row,'payment_type_filtered',' ')
			wf_filter_dddw_payment_type(ll_opening_no, ll_bencalc_no , ll_claim_no)
			
			//do the calculation
			ldec_benefit_amount = wf_calculate_amount(row, 0, 5)
			
			IF ISNULL(ldec_benefit_amount) OR ldec_benefit_amount < 0 THEN ldec_benefit_amount = 0
				 			
			//SETITEM TO benefit_entitlement_amount
			idw_benefit_entitlement.setitem(row, 'be_total_award_amount', ldec_benefit_amount)
			
			//grab the award_freq_code and switch on the screen one
			ls_ben_freq_code = inv_maintain_benefit.nf_get_ben_freq_code(ll_claim_no, ll_bencalc_no, ll_opening_no)
			
			IF isnull(ls_ben_freq_code) THEN ls_ben_freq_code = ' '
			idw_benefit_entitlement.setitem(row, 'award_freq_code', ls_ben_freq_code)
			
			//need to grab the award amount and put it in the BE award amount column on the BE datawindow
			ldec_award_amount = inv_maintain_benefit.nf_get_bc_award_amount(ll_claim_no, ll_bencalc_no, ll_opening_no)
			idw_benefit_entitlement.setitem(row, 'be_award_amount', ldec_award_amount)
			
			//need to grab the award amount and put it in the BE award amount column on the BE datawindow
			ldtm_effective_from_date = inv_maintain_benefit.nf_get_effective_date(ll_claim_no, ll_bencalc_no, ll_opening_no)
			idw_benefit_entitlement.setitem(row, 'effective_from_date', ldtm_effective_from_date)
			
				/********* BENCALC TEXT ********************/
			ls_dropvalue = wf_retrieve_display_value(2, ll_opening_no, ll_claim_no, ll_bencalc_no)
			
			IF ls_dropvalue <> '' THEN
				idw_benefit_entitlement.setitem(row,'bencalc_text',ls_dropvalue)
			ELSE
				idw_benefit_entitlement.setitem(row,'bencalc_text','0')
			END IF 	
												
	CASE 'number_of_days'
		
			//do the calculation
			ldec_benefit_amount = wf_calculate_amount(row, long(data), 2)
		
			IF ISNULL(ldec_benefit_amount) OR ldec_benefit_amount < 0 THEN ldec_benefit_amount = 0
		
			//SETITEM TO benefit_entitlement_amount
			idw_benefit_entitlement.setitem(row, 'be_total_award_amount', ldec_benefit_amount)
				
	CASE 'number_of_hours'
		
			//do the calculation
			ldec_benefit_amount = wf_calculate_amount(row, Dec(data), 1)
		
			IF ISNULL(ldec_benefit_amount) OR ldec_benefit_amount < 0 THEN ldec_benefit_amount = 0
		
			//SETITEM TO benefit_entitlement_amount
			idw_benefit_entitlement.setitem(row, 'be_total_award_amount', ldec_benefit_amount)
	
	CASE 'number_of_weeks'
	
			//do the calculation
			ldec_benefit_amount = wf_calculate_amount(row, long(data),3)
		
			IF ISNULL(ldec_benefit_amount) OR ldec_benefit_amount < 0 THEN ldec_benefit_amount = 0
		
			//SETITEM TO benefit_entitlement_amount
			idw_benefit_entitlement.setitem(row, 'be_total_award_amount', ldec_benefit_amount)
	
	CASE 'number_of_months'
		
			//do the calculation
			ldec_benefit_amount = wf_calculate_amount(row, long(data), 4)
		
			IF ISNULL(ldec_benefit_amount) OR ldec_benefit_amount < 0 THEN ldec_benefit_amount = 0
		
			//SETITEM TO benefit_entitlement_amount
			idw_benefit_entitlement.setitem(row, 'be_total_award_amount', ldec_benefit_amount)
			
	CASE 'sixty_percent_flag'
		/*
		8.50	The number of months of a benefit entitlement must be 12 if the 60% option lump sum indicator is set to ‘Yes’.
		8.60 	The number of days of a benefit entitlement must be zero if the 60% option lump sum indicator is set to ‘Yes’
		7.190	The period from date of a benefit entitlement must be set to the benefit start date of the selected opening 
				if the 60% option lump sum indicator is set to ‘Yes’.
		7.200	The period to date of a benefit entitlement must be set to one day after the benefit start date of the selected opening
				if the 60% option lump sum indicator is set to ‘Yes’.
		*/
			IF data = 'Y' THEN
				ll_claim_no 			= THIS.getitemnumber(row, 'claim_no')
				ll_opening_no 		= THIS.getitemnumber(row,'opening_no')
				ls_payment_type   = THIS.getitemstring(row,'payment_type_code')
								
				//grab the opening type if one does not already exist
				IF ll_opening_no > 0 THEN
					ls_opening_type = inv_maintain_benefit.nf_get_opening_type(ll_claim_no, ll_opening_no)
				ELSE
					ls_opening_type = inv_maintain_benefit.nf_get_opening_type_from_combination(ls_payment_type)	
				END IF 
				
				//ASSUME AN EMPTY opening type = 'SV'
				IF ls_opening_type = '' THEN
					ls_opening_type = 'SV'
				END IF 
						
				//returned by reference
				inv_maintain_benefit.nf_get_opening_dates(ll_claim_no, ll_opening_no, ls_opening_type , ldtm_opening_start, ldtm_opening_end)
				
				IF NOT ISNULL(ldtm_opening_start) THEN
					idw_benefit_entitlement.setitem(row, 'benefit_entitlement_from_date', ldtm_opening_start)
					idw_benefit_entitlement.setitem(row, 'benefit_entitlement_to_date',RelativeDate( date(ldtm_opening_start), 1))
				END IF 
				 
				idw_benefit_entitlement.setitem(row, 'number_of_months', 12)
				idw_benefit_entitlement.setitem(row, 'number_of_days', 0)
				
				//do the calculation
				ldec_benefit_amount = wf_calculate_amount(row, 0, 5)
				
				IF ISNULL(ldec_benefit_amount) OR ldec_benefit_amount < 0 THEN ldec_benefit_amount = 0
				
				//SETITEM TO benefit_entitlement_amount
				idw_benefit_entitlement.setitem(row, 'be_total_award_amount', round(ldec_benefit_amount,2))
			ELSE//NO
			END IF 	
			
	CASE ELSE
				
END CHOOSE
end event

event rowfocuschanged;call super::rowfocuschanged;/* this function will filter the dddw_opening for the correct claim_no */
LONG					ll_claim_no, ll_opening, ll_bencalc_no	, ll_opening_no
INTEGER				li_find
STRING				ls_find
datawindowchild	ldwc_child_bencalc


//MAKE SURE WE HAVE A ROW
IF currentrow <= 0  THEN RETURN

ll_claim_no 		= THIS.getitemnumber(currentrow,'claim_no')

// make sure we have a valid claim no
IF ISNULL(ll_claim_no) OR ll_claim_no <= 0 THEN RETURN 

/************** opening_no ***************/
/***************************************/
wf_filter_dddw_opening(ll_claim_no)

/******* benefit_calculation_no ***************/
/***************************************/
wf_filter_dddw_benefit_no(ll_claim_no)

//ll_opening_no 	= THIS.GetItemNumber(currentrow,'opening_no')

ll_opening_no 	= THIS.getitemnumber(currentrow,'opening_no_filtered')
ll_bencalc_no 	= THIS.getitemnumber(currentrow, 'benefit_calculation_no_filtered')

IF ISNULL(ll_opening_no) 	THEN ll_opening_no 	= 0
IF ISNULL(ll_bencalc_no) 	THEN ll_bencalc_no 	= 0
	
idw_benefit_entitlement.GetChild('benefit_calculation_no_filtered', ldwc_child_bencalc)

//if rowcount = 1 then only benefit calc of 0
IF ll_opening_no = 0 THEN		
	ldwc_child_bencalc.SetFilter("benefit_calculation_no = 0")
ELSE
	ldwc_child_bencalc.SetFilter("(opening_no = " + string(ll_opening_no) + " AND claim_no = " + string(ll_claim_no) + ")" + " or benefit_calculation_no = 0")
END IF

ldwc_child_bencalc.filter()

ls_find 	= 'benefit_calculation_no = ' + string(ll_bencalc_no)
li_find 	= ldwc_child_bencalc.Find(ls_find, 1, ldwc_child_bencalc.RowCount())

IF li_find > 0 THEN
	ldwc_child_bencalc.ScrollToRow(li_find)
	//	idw_benefit_entitlement.setitem(row,'benefit_calculation_no',0)
ELSE//need to do something here -- no bencalc
	ls_find 	= 'benefit_calculation_no = ' + string(0)
	li_find 	= ldwc_child_bencalc.Find(ls_find, 1, ldwc_child_bencalc.RowCount())
	ldwc_child_bencalc.ScrollToRow(li_find)
	idw_benefit_entitlement.setitem(currentrow,'benefit_calculation_no_filtered',0)
END IF

/********* payment_type_code ***************/
/***************************************/
wf_filter_dddw_payment_type(ll_opening_no, ll_bencalc_no, ll_claim_no )

end event

event buttonclicked;call super::buttonclicked;INTEGER 	li_count
STRING		ls_Object, ls_Win

ls_Object = STRING(dwo.name)

CHOOSE CASE ls_Object
	CASE 'b_calculate'
		IF dwo.text = 'Calculate Totals' THEN
			
			li_count = wf_check_unsaved_changes('calculate the totals')
			IF li_count > 0 THEN
				// message from function
				RETURN 0
			END IF
							 
			// call the function to grab the pr
			IF wf_populate_calculated_totals(il_annuity_account_no) = -1 THEN RETURN
			dwo.text = 'Refresh Totals'
				
		ELSEIF dwo.text = 'Refresh Totals' THEN
			
			//reset the origonal totals not including unsaved data
			wf_populate_benefit_totals(il_annuity_account_no)
	 		dwo.text = 'Calculate Totals'
			idw_benefit_entitlement.Object.t_calculate_dates.TEXT = '' 
		END IF 	
	CASE ELSE
END CHOOSE

end event

event rbuttondown;M_DW_RMB_POPUP lm_popup
window		lw_window

IF IsValid(inv_rowselection) Then
	inv_rowselection.event pfc_rbuttondown(xpos,ypos,row,dwo)
End if


/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
lm_popup = Create m_dw_rmb_popup
lm_popup.mf_set_datawindow(This)

// don't display lines
lm_popup.m_options.m_1.Visible = FALSE
lm_popup.m_options.m_2.Visible = FALSE

lw_window = wf_get_window_reference()	

lm_popup.m_options.PopMenu(lw_window.PointerX( ), lw_window.PointerY( ))

Destroy lm_popup
end event

type tabpage_qualification from userobject within tab_qualification
event create ( )
event destroy ( )
integer x = 18
integer y = 108
integer width = 2249
integer height = 2052
long backcolor = 67108864
string text = "Qualification Entitlement"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_benefit_qualification dw_benefit_qualification
end type

on tabpage_qualification.create
this.dw_benefit_qualification=create dw_benefit_qualification
this.Control[]={this.dw_benefit_qualification}
end on

on tabpage_qualification.destroy
destroy(this.dw_benefit_qualification)
end on

type dw_benefit_qualification from u_dw_online within tabpage_qualification
integer y = 16
integer width = 2245
integer height = 2008
integer taborder = 41
string dataobject = "d_benefit_qualification"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;settransobject(sqlca)
THIS.uf_setselect(1)
end event

event rbuttondown;M_DW_RMB_POPUP lm_popup
window		lw_window

IF IsValid(inv_rowselection) Then
	inv_rowselection.event pfc_rbuttondown(xpos,ypos,row,dwo)
End if


/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
lm_popup = Create m_dw_rmb_popup
lm_popup.mf_set_datawindow(This)

// don't display lines
lm_popup.m_options.m_1.Visible = FALSE
lm_popup.m_options.m_2.Visible = FALSE

lw_window = wf_get_window_reference()	

lm_popup.m_options.PopMenu(lw_window.PointerX( ), lw_window.PointerY( ))

Destroy lm_popup
end event

type cb_validate_entitlement from commandbutton within w_maintain_benefit_entitlement
integer x = 2537
integer y = 2400
integer width = 265
integer height = 84
integer taborder = 90
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Validate"
end type

event clicked;/*
The user will need the ability to validate the data before it is saved.  
Since the user has the ability to move a lot of data over in one move it may be 
confusing to validate the data and present the error back to the user as the data 
is being dropped on the screen.  So there will be no validation on the dropped data.  
The user will have the ability to validate the data when they are ready.  The ‘Validate’ button 
will run through the business rules and highlight any data that is invalid.  
*/

/* accept any new changes */
idw_benefit_entitlement.accepttext()

IF idw_benefit_entitlement.rowcount() = 0 THEN 
	messagebox('Validation Not Required', 'No benefit entitlement to validate.')
	RETURN 1
END IF 

/* Check all the business rules*/
IF  wf_check_benefit_entitlement() = -1 THEN RETURN -1

messagebox('Validation', 'No validation errors found in the benefit entitlement data.')
end event

type cb_create from commandbutton within w_maintain_benefit_entitlement
integer x = 1445
integer y = 2396
integer width = 251
integer height = 84
integer taborder = 50
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Create"
end type

event clicked;/*
To create an entitlement from a payment, the entitlement entry is created on the entitlement tab and all of the data is defaulted from the payment data.
4	Creating/Modifying Benefit Entitlement						

BENEFIT_ENTITLEMENT
Benefit entitlement is created everytime a benefit entitlement is created or modified on the Maintian Benefit Entitlement module.
*/

// If multiple Payments are selected run the above Payment rules
INTEGER li_rowcount, li_counter, li_selected, li_inserted_row, li_check, li_payment_counter, li_calculated_month
LONG		ll_claim_no, ll_opening_no, ll_ben_calc_no, ll_payment_no,ll_row
STRING	ls_payment_type, ls_payment_sub_type_code, ls_ben_freq_code, ls_dropvalue, ls_for_split, ls_txn_type
STRING	ls_error_text
DATE		ldt_payment_start, ldt_payment_end, ldt_effective_from
DECIMAL ldec_hours, ldec_days, ldec_payment_amount, ldec_total_award_amount, ldec_net_payment_amount
DECIMAL	ldec_total_payment_amount, ldec_award_amount, ldec_total_deductions
BOOLEAN	ib_set_deduction, lb_one_month_SS_payment

DATASTORE 			lds_additional_payments
s_window_message	lstr_message

IF ib_locked THEN
	MessageBox('No Data Entry','The "Verify" step has been completed. You must complete this checklist and start another before you can add new data.',Exclamation!)
	RETURN
END IF

IF ISVALID(ids_error_messages) THEN DESTROY ids_error_messages

/* (rownum, column name, error text */
ids_error_messages 					= CREATE DATASTORE
ids_error_messages.dataobject 	= 'd_benefit_error_messages'
ids_error_messages.settransobject(sqlca)

//DEFAULT THE ERROR TEXT
ls_error_text = ''

li_rowcount = idw_payments.rowcount()

IF li_rowcount <= 0 THEN RETURN 1

// only allow if the selected tab is the payment tab
IF tab_entitlement.selectedtab <> 2 THEN RETURN

setpointer(hourglass!) 

// get the selected count
li_selected = inv_maintain_benefit.nf_count_selected_rows(idw_payments)

IF li_selected >= 2 THEN
	
	/*  If multiple payments are selected the user will be given the option of creating an entitlement for each payment 
		 selected or for combining all selected payments into one entitlement.
	*/
	li_check = messagebox("Multiple Payments Selected", "Multiple Payments have been selected." +&
								  "~rOptions:" +&
	                                  "~rSelect YES if you would like to create an entiltlement for each payment." +&
								   "~rSelect NO to Combine all selected payments into 1 Payment." +&
								   "~rSelect Cancel to terminate this action.",Question!,YesNoCancel!,3)
									
	CHOOSE CASE li_check
		CASE 1//YES- CONTINUE
		CASE 2//NO - COMBINE
			cb_combine.triggerevent(clicked!)
			RETURN
		CASE 3//CANCEL - RETURN OUT
			RETURN
	END CHOOSE
END IF 	

//do a premove check to see if they want all associated records selected
IF wf_premove_check() = -1  THEN RETURN

//set the redraw off
idw_benefit_entitlement.setredraw(FALSE)

// create a new entitlement for each selected PAYMENT  
FOR li_counter = 1 TO idw_payments.rowcount()
	
	IF idw_payments.isselected(li_counter) THEN
		
		//DEFAULT THE DEDUCTION THING
		ib_set_deduction = FALSE

		//GRAB THE REST OF THE VALUES WE NEED
		ll_claim_no							= idw_payments.getitemnumber(li_counter, 'claim_no')
		ll_opening_no						= idw_payments.getitemnumber(li_counter, 'opening_no')
		ll_ben_calc_no						= idw_payments.getitemnumber(li_counter, 'benefit_calculation_no')
		ls_payment_type					= idw_payments.getitemstring(li_counter, 'payment_type_code')
		ll_payment_no						= idw_payments.getitemnumber(li_counter, 'payment_no')
		ldt_payment_start 					= DATE(idw_payments.getitemdatetime(li_counter, 'paid_from_date'))
		ldt_payment_end					= DATE(idw_payments.getitemdatetime(li_counter, 'paid_to_date'))
		ldec_hours           					= idw_payments.getitemdecimal(li_counter, 'paid_hours_lost')
		ldec_days            					= idw_payments.getitemdecimal(li_counter, 'paid_days_lost')
		ldec_payment_amount 			= idw_payments.getitemdecimal(li_counter, 'total_award_amount')
		ls_payment_sub_type_code 		= idw_payments.getitemstring(li_counter, 'payment_sub_type_code')
		ls_txn_type                        		= idw_payments.getitemstring(li_counter, 'txn_type_code')
		ldec_net_payment_amount 		= idw_payments.getitemdecimal(li_counter, 'net_payment_amount')
		ldec_total_payment_amount 	= idw_payments.getitemdecimal(li_counter, 'total_payment_amount')
		ldec_total_deductions 			= idw_payments.getitemdecimal(li_counter, 'total_deductions')
							
		//need to grab the effective_date from the benefit calculation
		ldt_effective_from = DATE(inv_maintain_benefit.nf_get_bencalc_eff_date(ll_ben_calc_no, ll_claim_no, ll_opening_no))
		
		//grab the freq of the Benefit_calculation
		ls_ben_freq_code = inv_maintain_benefit.nf_get_ben_freq_code(ll_claim_no,ll_ben_calc_no, ll_opening_no)
		IF isnull(ls_ben_freq_code) THEN ls_ben_freq_code = ''
		
		//need to grab the award amount and put it in the BE award amount column on the BE datawindow
		ldec_award_amount = inv_maintain_benefit.nf_get_bc_award_amount(ll_claim_no,ll_ben_calc_no, ll_opening_no)
				
		/*	
			7)Defaulting days, hours, weeks and months -- 
    			When you drag over a payment and the frequency is monthly - the payment will have days - 
			can we take the days and divide by 30 to get months and default the months to this value - 
			anything left over will go into the days column.  For example 65 day would be 2 months and 5 days
			(new functionality)
		*/
		IF ls_ben_freq_code = 'M' THEN
			li_calculated_month = int(ldec_days /30)
		ELSE
			li_calculated_month = 0
		END IF
		
		IF is_claim_role_code = 'SS' THEN
			lb_one_month_SS_payment = wf_check_ss_date_range(ldt_payment_start,ldt_payment_end)
			IF lb_one_month_SS_payment = TRUE THEN
				ldec_days = 30
				li_calculated_month = 1
			END IF
		END IF
		
		// An entitlement should not be based on a payment that has been fully adjusted to zero.
		IF inv_maintain_benefit.nf_check_payment_fully_adjusted(ll_payment_no) > 0 THEN 
			messagebox("Benefit Entitlement Validation", "An entitlement should not be created from a payment that has been fully adjusted to zero.")
		END IF
	
		//if no frequency code don't split, user must fill in and create information (WG -EMAIL)
		//SPLITpayment if required
		// if there is no frequency code, then get code from Payment_Type table
		IF ls_ben_freq_code = '' THEN
		    ls_ben_freq_code = inv_maintain_benefit.nf_get_freq_from_payment_type(ls_payment_type)
		END IF
		
		lds_additional_payments = inv_maintain_benefit.nf_split_payment_for_interest_period(ldt_payment_start, ldt_payment_end, ls_ben_freq_code)
		
		//DEFAULT THE 3day deduction amount if applicable -- single record
		IF li_selected = 1 THEN 
			IF ldec_total_deductions <> 0 AND ll_ben_calc_no > 0 THEN 
				li_check = messagebox("Payment Deduction", "A Payment Deduction exists for this payment. Include it on the Benefit Entitlement?" +&
				                                   "~rIf the Payment is a split payment the deduction will not be included",Question!,YesNo!,2)
				IF li_check = 1 THEN 
					ib_set_deduction = TRUE
				ELSE
					ib_set_deduction = FALSE
				END IF
			END IF 
		ELSEIF  li_selected > 1 THEN 	//DEFAULT THE 3day deduction amount if applicable -- multiple record
			IF ldec_total_deductions <> 0 AND ll_ben_calc_no > 0 AND abs(ldec_total_deductions) = ROUND(inv_maintain_benefit.nf_calculate_3_fifths(ldec_award_amount),2) THEN 
				ib_set_deduction = TRUE
			ELSE
				ib_set_deduction = FALSE
			END IF 	
		END IF 
	
		FOR li_payment_counter = 1 TO lds_additional_payments.ROWCOUNT()
			
			//grab the dates from the datastore we dont care about anything else
			ldt_payment_start 	= lds_additional_payments.getitemdate(li_payment_counter,'annuity_date_from')
			ldt_payment_end  	= lds_additional_payments.getitemdate(li_payment_counter,'annuity_date_to')
			ls_for_split  			= lds_additional_payments.getitemstring(li_payment_counter,'for_split')
			
			//insert a row into the benefit entitlement datawindow 
			li_inserted_row =  idw_benefit_entitlement.insertrow(0)
			
			IF li_calculated_month >= 1 AND ls_for_split <> 'Y' THEN
			 	idw_benefit_entitlement.setitem(li_inserted_row,'number_of_months', li_calculated_month)
				ldec_days = ldec_days - (li_calculated_month * 30)
				idw_benefit_entitlement.setitem(li_inserted_row,'number_of_days',ldec_days)
			END IF 
			
			idw_benefit_entitlement.setitem(li_inserted_row,'opening_no', ll_opening_no)
			idw_benefit_entitlement.setitem(li_inserted_row,'claim_no', ll_claim_no)
			idw_benefit_entitlement.setitem(li_inserted_row,'benefit_calculation_no', ll_ben_calc_no)
			idw_benefit_entitlement.setitem(li_inserted_row,'payment_no', ll_payment_no)
			idw_benefit_entitlement.setitem(li_inserted_row,'payment_type_code', ls_payment_type)
			idw_benefit_entitlement.setitem(li_inserted_row,'benefit_entitlement_from_date', ldt_payment_start)
			idw_benefit_entitlement.setitem(li_inserted_row,'benefit_entitlement_to_date', ldt_payment_end)
			idw_benefit_entitlement.setitem(li_inserted_row,'payment_sub_type_code', ' ')
			idw_benefit_entitlement.setitem(li_inserted_row,'effective_from_date', ldt_effective_from)
			idw_benefit_entitlement.setitem(li_inserted_row,'payment_sub_type_code',' ')
			idw_benefit_entitlement.setitem(li_inserted_row,'award_freq_code',ls_ben_freq_code)
			idw_benefit_entitlement.setitem(li_inserted_row, 'be_award_amount', ldec_award_amount)
			
			/*
			8) When splitting a payment automatically over the Annuity interest period (which works great) 
			do not default the days, hours, months or benefit amount.  Let the user enter them.  (new functionality)
			*/
			IF ls_for_split = 'Y' THEN 
				idw_benefit_entitlement.setitem(li_inserted_row,'number_of_hours', 0.00)
				idw_benefit_entitlement.setitem(li_inserted_row,'number_of_days', 0.00)
				idw_benefit_entitlement.setitem(li_inserted_row,'number_of_months', 0)
				idw_benefit_entitlement.setitem(li_inserted_row,'be_total_award_amount', 0.00)  
					
			ELSE//DEFAULT THE VALUES - NOT SPLIT
				idw_benefit_entitlement.setitem(li_inserted_row,'number_of_hours', ldec_hours)
				idw_benefit_entitlement.setitem(li_inserted_row,'number_of_days', ldec_days)
				
				//calculate the total award amount
				ldec_total_award_amount = wf_calculate_amount(li_inserted_row, 0, 5)
				
				/* 2) If the transaction type 1 has been adjusted - default the benefit entitlement amount to zero 
				(may be if the net amount <> total payment amount)
				*/
				IF ls_txn_type = '1' AND ldec_net_payment_amount <> ldec_total_payment_amount  THEN 
					ldec_total_award_amount = 0.00
				END IF 
			
				IF ISNULL(ldec_total_award_amount) OR ldec_total_award_amount < 0 THEN ldec_total_award_amount = 0.00
				
				idw_benefit_entitlement.setitem(li_inserted_row,'be_total_award_amount', ldec_total_award_amount)  
				
				//add in the deduction FOR A SINGLE RECORD/multiple if they have not been split
				IF ib_set_deduction = TRUE THEN
					IF li_selected = 1 THEN
						idw_benefit_entitlement.setitem(li_inserted_row,'three_day_withhold_deduction_amount', ldec_total_deductions)  
					ELSEIF li_selected > 1 THEN
						idw_benefit_entitlement.setitem(li_inserted_row,'three_day_withhold_deduction_amount', ldec_total_deductions)  
						ls_error_text =  "The following Deduction amount " + string(ldec_total_deductions,"$#,##0.00;($#,##0.00)") + " Has been added to the Benefit Entitlement. " +&
													 "~rClick this row to select."
						wf_insert_error(li_inserted_row, 'three_day_withhold_deduction_amount', ls_error_text)		
					END IF 	
				END IF 	
			END IF 
			
			/********* OPENING TEXT ********************/
			//wf_retrieve_display_value(1,opening_no,claim_no,bencalc)
			ls_dropvalue = wf_retrieve_display_value(1, ll_opening_no, ll_claim_no, 0)
			
			IF ls_dropvalue <> '' THEN
				idw_benefit_entitlement.setitem(li_inserted_row,'opening_text',ls_dropvalue)
			ELSE
				idw_benefit_entitlement.setitem(li_inserted_row,'opening_text','0')
			END IF 
						
			/********* BENCALC TEXT ********************/
			ls_dropvalue = wf_retrieve_display_value(2, ll_opening_no, ll_claim_no, ll_ben_calc_no)
			
			IF ls_dropvalue <> '' THEN
				idw_benefit_entitlement.setitem(li_inserted_row,'bencalc_text',ls_dropvalue)
			ELSE
				idw_benefit_entitlement.setitem(li_inserted_row,'bencalc_text','0')
			END IF 
		
		NEXT
		
		/*Once a payment has been selected and dragged to the entitlement area the user will need a 
			 visual indicator to let them know that they have already dealt with the payment.  Ideally the payment 
			 should be grayed out so that they will know not to select it again.  This will only be for the current session.  
			 Once the window is closed there will be no way to indicate which payments have already been used 
			 to create entitlement. 
		*/
		//SET THE SELECTABLE FLAG = 'N'
		idw_payments.setitem(li_counter,'selectable_flag','N')
		
	END IF 
NEXT	

//NOW POPULATE THE POSSIBLE CLAIM_NUMBERS
wf_populate_claim_dddw()

//set the redraw off
idw_benefit_entitlement.setredraw(TRUE)

//see if any deductions were added to Benefit Entitlement records only applicable to multi select
IF ids_error_messages.rowcount() > 0 THEN
	idw_payments.setredraw(TRUE)
	// open the error window
	lstr_message.awi_parent_window 		= THIS.GETPARENT()
	lstr_message.apo_powerobjectparm[1] 	= ids_error_messages
	lstr_message.as_mode 	                       = 'B'//information message

	openwithparm(w_view_benefit_error_msgs,lstr_message)
END IF 

//if a row has been inserted scroll to it - if multiple rows scroll to last inserted
IF li_inserted_row > 0 THEN
	//scroll and set the row
	idw_benefit_entitlement.SelectRow(0, FALSE)
	idw_benefit_entitlement.SetRow(li_inserted_row)
	idw_benefit_entitlement.SelectRow(li_inserted_row, TRUE)	
	idw_benefit_entitlement.scrolltorow(li_inserted_row)
END IF

end event

type cb_combine from commandbutton within w_maintain_benefit_entitlement
integer x = 1696
integer y = 2396
integer width = 283
integer height = 84
integer taborder = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Combine"
end type

event clicked;/* Combine payments (i.e. awards) into a single entitlement 
If the user selects to create one entitlement from multiple payments, then the following rules must apply:
·	The payments must all be from the same claim
·	The payments must all be the same type
·	The payments must for the same opening
·	The payments must be for the same benefit calculation

*/
INTEGER li_rtn

// only allow if the selected tab is the payment tab
IF tab_entitlement.selectedtab <> 2 THEN RETURN

IF ib_locked THEN
	MessageBox('No Data Entry','The "Verify" step has been completed. You must complete this checklist and start another before you can add new data.',Exclamation!)
	RETURN
END IF


//grab the selected rowcount, if it equals 1 no combination can be done
li_rtn = inv_maintain_benefit.nf_count_selected_rows(idw_payments)

IF li_rtn = 1 THEN
	MESSAGEBOX('Combine Payments', 'Only 1 payment has been selected. Payments cannot be combined.')
	RETURN
END IF 

//run the combine payment functionality
li_rtn = wf_combine_payment()
end event

type cb_split from commandbutton within w_maintain_benefit_entitlement
integer x = 1979
integer y = 2396
integer width = 233
integer height = 84
integer taborder = 70
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Split"
end type

event clicked;/* 
To split a payment into multiple entitlements only one payment may be selected at a time.  
The user will need to be prompted for the number of entitlement rows to create.  
Based on the input from the user create the number of entitlement entries specified 
and default all rows to the same claim number, benefit calculation, opening number 
and payment type as the selected payment.
*/
LONG		ll_return_count
INTEGER li_count

// only allow if the selected tab is the payment tab
IF tab_entitlement.selectedtab <> 2 THEN RETURN

IF ib_locked THEN
	MessageBox('No Data Entry','The "Verify" step has been completed. You must complete this checklist and start another before you can add new data.',Exclamation!)
	RETURN
END IF

//grab the selected rowcount, if it equals 1 no combination can be done
li_count = inv_maintain_benefit.nf_count_selected_rows(idw_payments)
IF li_count > 1 THEN
	MESSAGEBOX('Split Payments','To split a payment into multiple entitlements only one payment may be selected at a time.')
	RETURN
ELSEIF li_count = 0 THEN
	MESSAGEBOX('Split Payments','a payment must be selected for the Split Action')
	RETURN
END IF 

//how many rows does the user require?
open(w_prompt_user_for_be_count)
ll_return_count = Message.DoubleParm	 

IF ll_return_count = 0 OR ISNULL(ll_return_count) THEN RETURN//no value selected

//run the split code - ERROR msgs contained in code
wf_split_payment(ll_return_count) 


end event

type cb_refresh from commandbutton within w_maintain_benefit_entitlement
integer x = 2802
integer y = 2400
integer width = 265
integer height = 84
integer taborder = 100
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Refresh"
boolean default = true
end type

event clicked;//refresh the entitlement datawindow
setpointer(hourglass!) 

// just in case
idw_payments.setredraw(true)

wf_refresh_entitlement()

/*
Although this is a MUST for the Verify step - at this point it is a informational message for the users
we may want to take it out of the post open and only call it here - see BR1.110 & BR 1.120 for details
1.110	The Verify Benefit Entitlement checklist step must not be completed if benefit entitlement spans the annuity start date.
1.120 The Verify Benefit Entitlement checklist step must not be completed if benefit entitlement spans the annuity end date.
*/
wf_other_process_date_check()
end event

type cb_temp_save from commandbutton within w_maintain_benefit_entitlement
integer x = 3127
integer y = 2400
integer width = 265
integer height = 84
integer taborder = 110
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Save"
end type

event clicked;INTEGER				li_rtn, li_count, li_trancount
LONG					ll_claim_no
STRING				ls_error_text, ls_message
s_window_message	lstr_message


IF ib_cannot_save_SIN THEN
	MessageBox('No Data Entry','You cannot save any changes. An injured worker must have a Social Insurance Number.'+&
					  '~rPlease enter a valid SIN through the Individual maintenance module.',StopSign!)
	RETURN
ELSEIF ib_cannot_save_history THEN
	MessageBox('No Data Entry','You cannot save any changes. A individual must not be historical.'+&
					  '~rPlease reactivate this individual through the Individual maintenance module (Claimants) .' +&
					  '~ror through the Claim Participant module (Claimants and Surviving Spouses).',StopSign!)
	RETURN
ELSEIF ib_incomplete_annuity_checklist THEN
	IF is_claim_role_code =  'C' THEN
		ls_message = 'This injured worker has an incomplete annuity checklist.~r~n'&
		            +'Please complete all annuity checklists prior to attempting to save benefit entitlement'
	ELSE
		ls_message = 'This surviving spouse has an incomplete annuity checklist.~r~n'&
		            +'Please complete all annuity checklists prior to attempting to save benefit entitlement'
	END IF
	MessageBox('Incomplete Annuity Checklist',ls_message,StopSign!)
	RETURN
END IF

IF ib_locked THEN
	MessageBox('No Data Entry','The "Verify" step has been completed. You must complete this checklist and start another before you can add new data.',Exclamation!)
	RETURN
END IF


// 1.170	Benefit Entitlement work must not continue for an individual who has been merged since the module was opened.
li_count = inv_common_annuity.nf_check_individual_exists(il_individual_no)
IF li_count = 0 THEN	
	MessageBox('Individual Not Found','The individual number that you are working on no longer exists.'&
											+'~r~nThe individual may have been merged. The benefit entitlement will not be saved.'&
											+'~r~nPlease close this module and the Confirm Annuity Eligibility module (if it is open).', Exclamation!)
	RETURN
END IF


/* accept any new changes */
idw_benefit_entitlement.accepttext()

//make sure there is at least a row in there before proceding
IF idw_benefit_entitlement.rowcount() = 0 THEN 
	messagebox('Save Not Required', 'No benefit entitlement has been created to save.')
	RETURN 1
END IF


// delete any 'to be deleted' benefit entitlements
wf_delete_benefit_entitlement()

/* validate all of the rows*/
IF  wf_check_benefit_entitlement() = -1 THEN RETURN -1



SQLCA.nf_begin_transaction()

/* check and see if an annuity account record has been created 
    The information in this table is only created if the module is opened from the menu and 
	there has not been an annuity account established for the individual.
*/
IF il_action_code = 2 AND il_annuity_account_no = 0 THEN
	
	IF is_claim_role_code = 'SS' THEN
		ll_claim_no = il_claim_no
	ELSE
		ll_claim_no = 0
	END IF 
	
    il_annuity_account_no = inv_common_annuity.nf_insert_annuity_account(il_individual_no, ll_claim_no, is_claim_role_code,0,'N')
END IF 

/* SAVE THE ENTITLEMENT AND EVERYTHING ELSE THAT HAS BEEN DONE */
IF  wf_save_benefit_entitlement() = 1 OR ib_deleted = TRUE THEN 

	//DO THE UPDATE
	idw_benefit_entitlement.update()
	SQLCA.nf_handle_error("w_maintain_benefit_entitlement","idw_benefit_entitlement.update()",'cb_temp_save.clicked')
	
	SQLCA.nf_commit_transaction()
	
	ib_deleted = FALSE
	
	//refresh the entitlement datawindow
	wf_refresh_entitlement()
	
	idw_benefit_entitlement.SelectRow(0, FALSE)
	idw_benefit_entitlement.SetRow(1)
	idw_benefit_entitlement.SelectRow(1, TRUE)
	
	//reapply the filter on the first row
	wf_reapply_ben_filter(1)
	
	//REPOPULATE THE TOTALS - if it was only deletes wouldn't have been picked up in wf_refresh_entitlement
	wf_populate_benefit_totals(il_annuity_account_no)
	
	/*
		Although this is a MUST for the Verify step - at this point it is a informational message for the users
		we may want to take it out of the post open and only call it here - see BR1.110 & BR 1.120 for details
		1.110	The Verify Benefit Entitlement checklist step must not be completed if benefit entitlement spans the annuity start date.
		1.120 The Verify Benefit Entitlement checklist step must not be completed if benefit entitlement spans the annuity end date.
	*/
	wf_other_process_date_check()
ELSE
	SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
	IF li_trancount > 0 THEN
		SQLCA.nf_rollback_transaction()
	END IF
END IF 
end event

type st_multiple_accounts from statictext within w_maintain_benefit_entitlement
integer x = 14
integer y = 156
integer width = 1275
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 134217857
long backcolor = 67108864
string text = "Multiple annuity accounts exist for this Individual"
boolean focusrectangle = false
end type

type cb_extract from commandbutton within w_maintain_benefit_entitlement
integer x = 2272
integer y = 2400
integer width = 265
integer height = 84
integer taborder = 80
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Extract"
end type

event clicked;/*
	Extracting the Data
	Since the payout screen will not be available in phase 1 it is necessary for the users to create a 
	spreadsheet to balance the account and create the payout.  To aid in this, an extract button 
	is necessary that will create a file from the data.  This should be modeled after the ‘Extract’ 
	functionality available in Payment Inquiry in CMWB.  The Benefit Entitlement datawindow will be 
	the data that is extracted.
*/
IF idw_benefit_entitlement.RowCount() = 0 THEN
	MessageBox('No rows','There is no benefit entitlement data to extract.')
	RETURN
END IF

idw_benefit_entitlement.Saveas('',Excel!,TRUE)
end event

type cb_add from commandbutton within w_maintain_benefit_entitlement
integer x = 1161
integer y = 2396
integer width = 283
integer height = 84
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Add"
end type

event clicked;/* Basically this adds an empty row to the datawindow
	there are specific rules that must be followed for this 
	a User can choose to leave the opening and bencalc to 0 if they so wish
*/

LONG						ll_claim_no, ll_row, ll_data
DATAWINDOWCHILD	ldwc_child, ldwc_child_payment_type
INTEGER					li_counter, li_row, li_rowcount
dwobject   				ldwo 

IF ib_locked THEN
	MessageBox('No Data Entry','The "Verify" step has been completed. You must complete this checklist and start another before you can add new data.',Exclamation!)
	RETURN
END IF

// only allow if the selected tab is the payment tab
IF tab_qualification.selectedtab <> 1 THEN RETURN

//set the redraw off
idw_benefit_entitlement.setredraw(FALSE)

//just add a blank row - let the users fill in the information
li_row = idw_benefit_entitlement.insertrow(0)

//populate the claim dddw
wf_populate_only_claim_dddw()

/* POPULATE ANY DEFAULTS WE NEED
*/
idw_benefit_entitlement.setitem(li_row,'payment_sub_type_code',' ')

//set the first claim role
idw_benefit_entitlement.GetChild('claim_no', ldwc_child)
idw_benefit_entitlement.GetChild('payment_type_filtered', ldwc_child_payment_type)

//scroll to the claim number
ldwc_child.scrolltorow(1)

//scroll and set the row
idw_benefit_entitlement.SelectRow(0, FALSE)
idw_benefit_entitlement.SetRow(li_row)
idw_benefit_entitlement.SelectRow(li_row, TRUE)
idw_benefit_entitlement.scrolltorow(li_row)

//IF only the claim number exists just exit leave the filter as the basic retrieve
wf_retrieve_payment_type_dddw(is_claim_role_code)
ldwc_child_payment_type.SetTransObject(SQLCA)
ldwc_child_payment_type.SetFilter('')
ldwc_child_payment_type.Filter()

//set the redraw BACK ON
idw_benefit_entitlement.setredraw(TRUE)
end event

