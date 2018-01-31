$PBExportHeader$w_periodic_award.srw
forward
global type w_periodic_award from w_a_tool
end type
type dw_award_list from u_dw_online within w_periodic_award
end type
type dw_award_detail from u_dw_online within w_periodic_award
end type
type cb_add from commandbutton within w_periodic_award
end type
type cb_delete from commandbutton within w_periodic_award
end type
type cb_save from commandbutton within w_periodic_award
end type
type cb_cancel from commandbutton within w_periodic_award
end type
type cb_next from commandbutton within w_periodic_award
end type
type cb_previous from commandbutton within w_periodic_award
end type
type cb_add_recipient from commandbutton within w_periodic_award
end type
type dw_basic_claim from u_dw_online within w_periodic_award
end type
type cb_search from commandbutton within w_periodic_award
end type
type dw_recipient from u_dw_online within w_periodic_award
end type
type sle_overpayment from singlelineedit within w_periodic_award
end type
type cb_delete_recipient from commandbutton within w_periodic_award
end type
end forward

global type w_periodic_award from w_a_tool
string title = ""
boolean resizable = false
boolean border = false
dw_award_list dw_award_list
dw_award_detail dw_award_detail
cb_add cb_add
cb_delete cb_delete
cb_save cb_save
cb_cancel cb_cancel
cb_next cb_next
cb_previous cb_previous
cb_add_recipient cb_add_recipient
dw_basic_claim dw_basic_claim
cb_search cb_search
dw_recipient dw_recipient
sle_overpayment sle_overpayment
cb_delete_recipient cb_delete_recipient
end type
global w_periodic_award w_periodic_award

type variables
LONG il_claim_no, il_individual_no
N_AWARDS inv_awards
S_WINDOW_MESSAGE istr_message
end variables

forward prototypes
public function integer wf_retrieve_list ()
public function integer wf_read_only ()
public function integer wf_check_disable ()
public function integer wf_check_claim_openings (long al_claim_no, string as_opening_type_code)
end prototypes

public function integer wf_retrieve_list ();// wf_retrieve_list
//
Integer li_rtn
Long    ll_list_row_count, ll_current_row

ll_current_row = dw_award_list.GetRow()

ll_list_row_count = dw_award_list.Retrieve(il_claim_no)
li_rtn = SQLCA.nf_handle_error('w_periodic_award', '', 'wf_retrieve_list - dw_award_list.Retrieve(il_claim_no)') 

IF ll_list_row_count > 0 AND ll_current_row = 1 THEN 	// if current row was 1 then the rowfocuschanged event may not have fired
	dw_award_list.SetRow(1)
	dw_award_list.uf_processselect(1, 'KeyBoard')
	dw_award_list.TriggerEvent(RowFocusChanged!)
END IF

// If no rows retrieved then disable appropriate controls
IF ll_list_row_count = 0 THEN 
	cb_save.Enabled = FALSE
	cb_cancel.Enabled = FALSE
	cb_delete.Enabled = FALSE
	// PR6639 - 2008-04-28 - r.s. - only allow these controls to be enabled if NOT in READ only mode
	IF inv_awards.is_mode <> 'READ' THEN
		cb_add.Enabled = TRUE
		dw_award_list.Enabled = TRUE
	END IF
END IF 

RETURN ll_list_row_count
end function

public function integer wf_read_only ();//	protect all columns of visible dw's
dw_award_detail.uf_protect_allattributes(TRUE)
dw_recipient.uf_protect_allattributes(TRUE)

//	dw_award_detail.uf_set_backcolor()
//	dw_recipient.uf_set_backcolor()

st_title.Text = 'Awards Maintenance Read-Only'

cb_add.Enabled = FALSE
cb_cancel.Enabled = FALSE
cb_delete.Enabled = FALSE
cb_save.Enabled = FALSE
cb_add_recipient.Enabled = FALSE
cb_delete_recipient.Enabled = FALSE
cb_search.Enabled = FALSE

inv_awards.is_mode = 'READ'

RETURN 0

end function

public function integer wf_check_disable ();// wf_check_disable - This functions protects all the columns at first.  If the award isn't authorized then it enables
//                    all the columns.  If there isn't any payments against the award then all the columns are enabled.
//                    If there are any payments against the award then only the award_end_date is enabled.
//
Long     ll_find_row, ll_num_payments_against_award, ll_award_no 
Integer  li_rtn
String   ls_award_type_code, ls_authorization_type_code, ls_authorized_user_id
Datetime ldt_award_start_date 
Decimal  ldec_authorization_limit, ldec_total_award_amount
DataWindowChild ldwc_child
 
// Disable Buttons
cb_delete.Enabled = FALSE
cb_add_recipient.Enabled = FALSE
cb_delete_recipient.Enabled = FALSE

IF istr_message.as_mode = 'READ' THEN
	RETURN -1
END IF

// Protect all columns
dw_award_detail.uf_protect_allattributes(TRUE)
dw_recipient.uf_protect_allattributes(TRUE)
cb_search.Enabled = FALSE

// Get Award Details
ll_award_no = dw_award_detail.GetItemNumber(1, 'award_no')
ls_authorized_user_id = dw_award_detail.GetItemString(1, 'authorized_user_id')
ls_award_type_code = dw_award_detail.GetItemString(1, 'award_type_code')
ldec_total_award_amount = dw_award_detail.GetItemDecimal(1, 'total_award_amount')
ldt_award_start_date = dw_award_detail.GetItemDateTime(1, 'award_start_date')

// If the award is not authorized don't protect and columns
IF Trim(ls_authorized_user_id) = '' THEN
	dw_award_detail.uf_protect_allattributes(FALSE)
	dw_recipient.uf_protect_allattributes(FALSE)
	cb_delete.Enabled = TRUE
	cb_add_recipient.Enabled = TRUE
	cb_delete_recipient.Enabled = TRUE
	cb_search.Enabled = TRUE
	inv_awards.nf_set_authorization(TRUE)
	RETURN 0
END IF

// See if there are any payments against the award
SELECT COUNT(*) 
  INTO :ll_num_payments_against_award 
  FROM PAYMENT 
 WHERE claim_no = :il_claim_no  
   AND award_no = :ll_award_no ; 

li_rtn = SQLCA.nf_handle_error('w_periodic_award', '', 'wf_check_disable - SELECT from PERIODIC_AWARD_CONTROL')

// Check current user id against authorized_user_id
IF vgst_user_profile.user_id <> ls_authorized_user_id THEN
	// Current user did Not authorize the award
	dw_award_detail.GetChild('award_type_code', ldwc_child)
	ll_find_row = ldwc_child.Find('award_type_code = "' + ls_award_type_code + '"', 1, ldwc_child.RowCount())
	IF ll_find_row < 1 THEN
		RETURN -1
	ELSE
		ls_authorization_type_code = ldwc_child.GetItemString(ll_find_row, 'authorization_type_code')
	END IF

	// Get Current user's authorization limit and compare the award amount. 
	ldec_authorization_limit = inv_awards.nf_get_authorization_limit(ls_authorization_type_code)	

	// Open all fields if user's limit is good and there are no payments against award
	IF ldec_authorization_limit >= ldec_total_award_amount AND ll_num_payments_against_award = 0 THEN
		dw_award_detail.uf_protect_allattributes(FALSE)
		dw_recipient.uf_protect_allattributes(FALSE)
		// dw_award_detail.uf_protectcolumn('award_end_date', FALSE)
		// dw_award_detail.uf_protectcolumn('award_start_date', FALSE)
		cb_delete.Enabled = TRUE
		cb_add_recipient.Enabled = TRUE
		cb_delete_recipient.Enabled = TRUE
		cb_search.Enabled = TRUE

		// Set the authorized by id to '' so that the entry can be authorized again
		inv_awards.nf_set_authorization(TRUE)
	ELSE
		// Enable award_end_date only
		dw_award_detail.uf_protectcolumn('award_end_date', FALSE)
		inv_awards.nf_set_authorization(FALSE)
	END IF
ELSE  // Current user did authorize the award
	// Enable all columns if no payments against award
	IF ll_num_payments_against_award = 0 THEN
		dw_award_detail.uf_protect_allattributes(FALSE)
		dw_recipient.uf_protect_allattributes(FALSE)
		//dw_award_detail.uf_protectcolumn('award_end_date', FALSE)
		cb_delete.Enabled = TRUE
		cb_add_recipient.Enabled = TRUE
		cb_delete_recipient.Enabled = TRUE
		cb_search.Enabled = TRUE
		inv_awards.nf_set_authorization(TRUE)
	ELSE 
		// Enable award_end_date only
		dw_award_detail.uf_protectcolumn('award_end_date', FALSE)
		inv_awards.nf_set_authorization(FALSE)
	END IF
END IF

RETURN 0 
end function

public function integer wf_check_claim_openings (long al_claim_no, string as_opening_type_code);// wf_check_claim_openings
//
Integer li_count, li_rtn

SELECT count(*)
  INTO :li_count
  FROM OPENING
 WHERE claim_no = :al_claim_no 
   AND opening_type_code = :as_opening_type_code 
 USING SQLCA ; 

li_rtn = SQLCA.nf_handle_error('w_periodic_award', '', 'wf_check_claim_openings - SELECT count(*) FROM OPENING') 

IF IsNull(li_count) OR li_count = 0 THEN
	RETURN -1
ELSE
	RETURN li_count
END IF

end function

event open;call super::open;Integer li_rtn, li_rows
Long    ll_individual_no, ll_num_rows
Decimal ldec_balance_amount
String  ls_claim_admin_region,ls_claim_status_code,ls_claim_status_type_code
Boolean lb_valid_claim_status,lb_no_openings
u_dwa   ldw_dw[]
s_window_message lstr_message
U_DS    lds_overpayment_warning_sum


istr_message = Message.PowerObjectParm
inv_awards = CREATE n_awards

IF istr_message.as_mode = 'READ' THEN
	wf_read_only()
END IF

dw_award_list.SetTransObject(SQLCA)
dw_award_list.uf_setselect(1)
dw_basic_claim.SetTransObject(SQLCA)
il_claim_no = iw_active_sheet.dw_basic_claim.GetItemNumber(1, 'claim_no')
ll_individual_no = iw_active_sheet.dw_basic_claim.GetItemNumber(1, 'individual_no')

// Check authorizations PR4270 - J.Hawker 2004-09-23 Only check Authorizations if NOT in Read-Only mode.
ls_claim_admin_region = iw_active_sheet.dw_basic_claim.GetItemString(1,'admin_region_code')
IF inv_awards.is_mode <> 'READ' THEN
	IF gnv_user_authorizations.nf_user_has_module_authorizations(ls_claim_admin_region, 'awards') = FALSE THEN
		MessageBox('No Authorizations','You do not have the necessary authorizations to use this module.')
		Close(This)
		RETURN
	END IF
END IF

il_individual_no = ll_individual_no

inv_awards.nf_set_window_parent(THIS)

ldw_dw[1] = dw_award_detail
ldw_dw[2] = dw_recipient

inv_awards.nf_init(ldw_dw[], SQLCA, This)
inv_awards.nf_set_commit(TRUE)

// Send the claim number to the object
ll_num_rows = dw_basic_claim.Retrieve(il_claim_no)
li_rtn = SQLCA.nf_handle_error('w_periodic_award', '','open - dw_basic_claim.Retrieve(il_claim_no)')

// Let the object know about the basic claim info to avoid having to re-retrieve
inv_awards.nf_set_basic_claim(dw_basic_claim)

// Retrieve the claim for use in payment module 
inv_awards.nf_set_claim_no(il_claim_no)

// Validate that user is allowed here 
IF istr_message.as_mode <> 'READ' THEN
	IF inv_awards.nf_validate_claim() < 0 THEN
		Close(This)
		Return
	END IF
END IF
wf_retrieve_list()

// Retrieve claim's status
ls_claim_status_code = dw_basic_claim.GetItemString(1, 'claim_status_code')
ls_claim_status_type_code = dw_basic_claim.GetItemString(1, 'claim_status_type_code')

IF ls_claim_status_code <> 'A' AND ls_claim_status_code <> 'F' then 
	MessageBox('Invalid Claim Status', 'This claim has an invalid status for creating an award. ~r~nThe claim must be Active, Final-Final, Final-First & Finalled, Final-Lost Time Med Aid Only or Final-No Lost Time')
	Close(This)
	RETURN
END IF

// determine if there are openings
IF wf_check_claim_openings(il_claim_no, 'RLOE') <> -1 OR wf_check_claim_openings(il_claim_no, 'LTD') <> -1 OR &
	wf_check_claim_openings(il_claim_no, 'PEN') <> -1  OR wf_check_claim_openings(il_claim_no, 'S1') <> -1  OR &
	wf_check_claim_openings(il_claim_no, 'S2') <> -1   OR wf_check_claim_openings(il_claim_no, 'SV') <> -1  THEN
	
	CHOOSE CASE ls_claim_status_code
		CASE 'A'
			//ok
			lb_valid_claim_status = TRUE
		CASE 'F'
			CHOOSE CASE ls_claim_status_type_code
				CASE '01', '02', '03', '04'
					// OK
					lb_valid_claim_status = TRUE
			END CHOOSE
	END CHOOSE
ELSE
	lb_no_openings = TRUE
END IF

// if claim does have openings, but not a valid claim status type code for claims with openings, then raise an error message
IF lb_no_openings = FALSE AND lb_valid_claim_status = FALSE THEN
	MessageBox('Invalid Claim Status', 'This claim has an invalid status for creating an award on the existing opening. ~r~nThe claim must be Active, Final-Final, Final-First & Finalled, Final-Lost Time Med Aid Only or Final-No Lost Time')
	Close(This)
	RETURN

//P10151-286 - new entries in the Payment_Combination table, with an opening_type_code of 'I', will allow awards to be created when there are no openings, but only CA and CL type awards
ELSEIF lb_no_openings = TRUE THEN
// OK, carry on with CL or CA type awards
END IF

/*	check for overpayment to any claim participant
*/
	lds_overpayment_warning_sum = Create U_DS
	lds_overpayment_warning_sum.DataObject = 'ds_overpayment_warning_sum'
	lds_overpayment_warning_sum.SetTransObject(SQLCA)
	li_rows = lds_overpayment_warning_sum.Retrieve(il_claim_no)
	SQLCA.nf_handle_error('w_account_payment','ds_overpayment_warning_sum','lds_overpayment_warning_sum.Retrieve')
	
	IF li_rows > 0 THEN
		sle_overpayment.Text = 'Overpayment'
		lstr_message.as_stringparm[1] = 'OVERPAYMENT situation exists.'
		lstr_message.as_stringparm[2] = 'I'
		lstr_message.al_doubleparm[1] = ll_individual_no
		lstr_message.al_doubleparm[2] = il_claim_no
		
		OpenWithParm(w_payment_message, lstr_message)
	END IF 

end event

on w_periodic_award.create
int iCurrent
call super::create
this.dw_award_list=create dw_award_list
this.dw_award_detail=create dw_award_detail
this.cb_add=create cb_add
this.cb_delete=create cb_delete
this.cb_save=create cb_save
this.cb_cancel=create cb_cancel
this.cb_next=create cb_next
this.cb_previous=create cb_previous
this.cb_add_recipient=create cb_add_recipient
this.dw_basic_claim=create dw_basic_claim
this.cb_search=create cb_search
this.dw_recipient=create dw_recipient
this.sle_overpayment=create sle_overpayment
this.cb_delete_recipient=create cb_delete_recipient
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_award_list
this.Control[iCurrent+2]=this.dw_award_detail
this.Control[iCurrent+3]=this.cb_add
this.Control[iCurrent+4]=this.cb_delete
this.Control[iCurrent+5]=this.cb_save
this.Control[iCurrent+6]=this.cb_cancel
this.Control[iCurrent+7]=this.cb_next
this.Control[iCurrent+8]=this.cb_previous
this.Control[iCurrent+9]=this.cb_add_recipient
this.Control[iCurrent+10]=this.dw_basic_claim
this.Control[iCurrent+11]=this.cb_search
this.Control[iCurrent+12]=this.dw_recipient
this.Control[iCurrent+13]=this.sle_overpayment
this.Control[iCurrent+14]=this.cb_delete_recipient
end on

on w_periodic_award.destroy
call super::destroy
destroy(this.dw_award_list)
destroy(this.dw_award_detail)
destroy(this.cb_add)
destroy(this.cb_delete)
destroy(this.cb_save)
destroy(this.cb_cancel)
destroy(this.cb_next)
destroy(this.cb_previous)
destroy(this.cb_add_recipient)
destroy(this.dw_basic_claim)
destroy(this.cb_search)
destroy(this.dw_recipient)
destroy(this.sle_overpayment)
destroy(this.cb_delete_recipient)
end on

event closequery;call super::closequery;Integer li_rtn

IF cb_save.Enabled = TRUE THEN
	li_rtn = Messagebox("Save Changes?", "Changes have not been saved. Do you want to save your changes?",Question!, YesNoCancel!, 1)
	IF li_rtn = 1 THEN
		cb_save.Triggerevent(Clicked!)
		IF cb_save.Enabled = TRUE THEN
			RETURN 1
		END IF
	ELSEIF li_rtn = 3 THEN
		RETURN 1
	END IF
END IF

end event

type st_title from w_a_tool`st_title within w_periodic_award
string text = "Periodic Awards Maintenance"
end type

type cb_close from w_a_tool`cb_close within w_periodic_award
integer taborder = 100
end type

type dw_award_list from u_dw_online within w_periodic_award
integer x = 18
integer y = 108
integer width = 2597
integer height = 380
integer taborder = 130
string dataobject = "d_award_list"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;call super::rowfocuschanged;Long ll_row

IF This.RowCount() = 0 THEN
	cb_delete.enabled = FALSE
	RETURN
END IF

ll_row = This.GetRow()
IF ll_row > 0 THEN
	This.SelectRow(0, FALSE)
	This.SelectRow(ll_row, TRUE)

	//	Retrieve and display award details
	inv_awards.nf_retrieve(This.GetItemNumber(ll_row, "claim_no"), This.GetItemNumber(ll_row, "award_no"))
	wf_check_disable()
	IF istr_message.as_mode = 'READ' THEN
		cb_add_recipient.Enabled = FALSE
		cb_delete_recipient.Enabled = FALSE
	END IF
END IF

end event

type dw_award_detail from u_dw_online within w_periodic_award
integer x = 18
integer y = 492
integer width = 2619
integer height = 704
integer taborder = 90
boolean bringtotop = true
string dataobject = "d_award_details"
boolean border = false
end type

event itemchanged;call super::itemchanged;Integer li_rtn

cb_save.enabled = TRUE
cb_cancel.enabled = TRUE
uf_set_pbmessage(TRUE)

li_rtn = inv_awards.nf_change_item(1)
IF li_rtn = -1 THEN
	RETURN 1
END IF
end event

event editchanged;call super::editchanged;cb_save.enabled = TRUE
cb_cancel.enabled = TRUE
uf_set_pbmessage(TRUE)
end event

type cb_add from commandbutton within w_periodic_award
integer x = 41
integer y = 1680
integer width = 379
integer height = 100
integer taborder = 120
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Add"
end type

event clicked;Integer li_rtn 
String  ls_courtorder, ls_display_message 
S_WINDOW_MESSAGE lstr_message 

SetPointer(HourGlass!)

IF inv_awards.nf_insert(0) < 0 THEN
	RETURN
END IF

wf_check_disable()

dw_award_list.Enabled = FALSE
dw_award_list.SelectRow(0, FALSE)
cb_cancel.Enabled = TRUE
cb_save.Enabled = TRUE
cb_add.Enabled = FALSE
cb_add_recipient.Enabled = TRUE
cb_delete_recipient.Enabled = TRUE
cb_delete.Enabled = FALSE

SELECT court_order_flag 
  INTO :ls_CourtOrder 
  FROM INDIVIDUAL 
 WHERE individual_no = :il_individual_no 
 USING SQLCA ; 

li_rtn = SQLCA.nf_handle_error('w_periodic_award', '', 'cb_add - SELECT court_order_flag FROM INDIVIDUAL') 

IF ls_CourtOrder = 'Y' then
	ls_display_message = ls_display_message + '~r~n*********************************~r~n~Court Order Situation Exists.'
	ls_display_message = ls_display_message + '~r~n*********************************~r~n~r~n'
END IF

IF ls_display_message <> '' THEN
	ls_display_message = "NOTE:~r~n~r~n" + ls_display_message
	
	SetPointer(HourGlass!)

	lstr_message.as_stringparm[1] = ls_display_message
	lstr_message.as_stringparm[2] = 'I'
	lstr_message.al_doubleparm[1] = il_individual_no
	
	OpenWithParm(w_award_message,lstr_message)
END IF

end event

type cb_delete from commandbutton within w_periodic_award
integer x = 443
integer y = 1680
integer width = 379
integer height = 100
integer taborder = 70
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Delete"
end type

event clicked;INTEGER	li_rtn
N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '011' refers to the Award Maintenance module, '044' refers to the Payment Processing module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('011','044','delete',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	RETURN
END IF
/******************************************************************************************/

SetPointer(HourGlass!)
IF MessageBox('Deletion Verification','Delete award?', Question!, YesNo!) = 1 THEN
	IF inv_awards.nf_delete() >= 0 THEN
		wf_retrieve_list()
	ELSE
		dw_award_list.TriggerEvent(RowFocusChanged!)
	END IF
END IF
end event

type cb_save from commandbutton within w_periodic_award
integer x = 1111
integer y = 1680
integer width = 379
integer height = 100
integer taborder = 110
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
end type

event clicked;INTEGER	li_rtn, li_trancount
N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '011' refers to the Award Maintenance module, '044' refers to the Payment Processing module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('011','044','save',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	RETURN
END IF
/******************************************************************************************/

SetPointer(HourGlass!)

SQLCA.nf_begin_transaction()

IF inv_awards.nf_save() = 0 THEN
	SQLCA.nf_commit_transaction()
	
	wf_retrieve_list()
	cb_save.Enabled = FALSE
	cb_cancel.Enabled = FALSE
	cb_add.Enabled = TRUE
	dw_award_list.Enabled = TRUE
ELSE
	SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
	IF li_trancount > 0 THEN
		SQLCA.nf_rollback_transaction()
	END IF
END IF

	

end event

type cb_cancel from commandbutton within w_periodic_award
integer x = 1513
integer y = 1680
integer width = 379
integer height = 100
integer taborder = 80
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Cance&l"
end type

event clicked;SetPointer(HourGlass!)

//	Retrieve the last payment worked on
IF dw_award_list.RowCount() = 0 THEN
	cb_add.enabled = TRUE
	dw_award_detail.Reset()
	dw_recipient.Reset()
	cb_add_recipient.enabled = FALSE
	cb_delete_recipient.enabled = FALSE
	cb_save.enabled = FALSE
	cb_cancel.enabled = FALSE
	RETURN
END IF

dw_award_list.Enabled = TRUE
IF wf_retrieve_list() > 0 THEN
	dw_award_list.uf_processselect(1,'KeyBoard')
	cb_save.Enabled = FALSE
	cb_cancel.Enabled = FALSE
END IF
cb_add.Enabled = TRUE
	
RETURN

end event

type cb_next from commandbutton within w_periodic_award
integer x = 2345
integer y = 1580
integer width = 238
integer height = 80
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Next>>"
end type

event clicked;Long ll_tranrow

ll_tranrow =dw_recipient.GetRow()
IF ll_tranrow = dw_recipient.RowCount() THEN
	RETURN 
END IF

IF dw_recipient.AcceptText() < 0 THEN
	dw_recipient.SetFocus()
	RETURN
END IF

//	Move to the previous row and set up the screen accordingly
dw_recipient.ScrollToRow(ll_tranrow + 1)

end event

type cb_previous from commandbutton within w_periodic_award
integer x = 1175
integer y = 1580
integer width = 238
integer height = 80
integer taborder = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "<<&Prev"
end type

event clicked;Long ll_tranrow

ll_tranrow = dw_recipient.GetRow()
IF ll_tranrow = 1 THEN
	RETURN
END IF

IF dw_recipient.AcceptText() < 0 THEN
	dw_recipient.SetFocus()
	RETURN
END IF

// Move to the previous row and set up the screen accordingly 
dw_recipient.ScrollToRow(ll_tranrow - 1)

end event

type cb_add_recipient from commandbutton within w_periodic_award
integer x = 1417
integer y = 1580
integer width = 407
integer height = 80
integer taborder = 50
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Add &Recipient"
end type

event clicked;SetPointer(HourGlass!)
cb_add.Enabled = FALSE
inv_awards.nf_insert_recipient(0)
cb_next.Enabled = TRUE
cb_previous.Enabled = TRUE
cb_cancel.Enabled = TRUE
cb_delete.Enabled = False

end event

type dw_basic_claim from u_dw_online within w_periodic_award
boolean visible = false
integer x = 923
integer y = 1680
integer width = 128
integer height = 208
integer taborder = 10
string dataobject = "d_basic_claim_everything"
end type

type cb_search from commandbutton within w_periodic_award
integer x = 1243
integer y = 1356
integer width = 87
integer height = 68
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "?"
end type

event clicked;String  ls_type
Long    ll_row
s_window_message lstr_message

ll_row = dw_recipient.GetRow()
IF ll_row > 0 THEN
	ls_type = dw_recipient.GetItemString(ll_row, 'recipient_type_code')
END IF

IF ls_type = 'V' OR ls_type =  'M' OR ls_type =  'O' THEN
	OpenWithParm(w_service_provider_search, ls_type)
	lstr_message = Message.PowerObjectParm
	dw_recipient.SetColumn('recipient_no')
	dw_recipient.SetItem(ll_row, 'recipient_no', lstr_message.al_doubleparm[1])
	dw_recipient.TriggerEvent(ItemChanged!)
ELSE
	MessageBox('Warning', 'No search available for this recipient type.')
END IF

end event

type dw_recipient from u_dw_online within w_periodic_award
integer x = 5
integer y = 1172
integer width = 2592
integer height = 492
integer taborder = 20
string dataobject = "d_award_recipient"
boolean border = false
end type

event itemchanged;call super::itemchanged;Integer li_rtn

cb_save.Enabled = TRUE
cb_cancel.Enabled = TRUE
uf_set_pbmessage(TRUE)

li_rtn = inv_awards.nf_change_item(2)
IF li_rtn = -1 THEN
	RETURN 1
END IF

end event

event rowfocuschanging;call super::rowfocuschanging;Long ll_row

ll_row = newrow
IF ll_row > 0 THEN
	IF This.GetItemString(ll_row, 'recipient_type_code') = 'I' THEN
		inv_awards.nf_set_recipient_filter(FALSE)
	ELSE
		inv_awards.nf_set_recipient_filter(TRUE)
	END IF
END IF

end event

event editchanged;call super::editchanged;cb_save.Enabled = TRUE
cb_cancel.Enabled = TRUE
uf_set_pbmessage(TRUE)
end event

type sle_overpayment from singlelineedit within w_periodic_award
integer x = 2153
integer y = 12
integer width = 466
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 255
long backcolor = 67108864
boolean border = false
boolean autohscroll = false
end type

type cb_delete_recipient from commandbutton within w_periodic_award
integer x = 1824
integer y = 1580
integer width = 517
integer height = 80
integer taborder = 50
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Delete Recipient"
end type

event clicked;Integer li_rtn

SetPointer(HourGlass!)

li_rtn = inv_awards.nf_delete_recipient()

IF li_rtn < 1 THEN 
	RETURN
END IF

cb_save.Enabled = TRUE
cb_cancel.Enabled = TRUE
cb_add.Enabled = FALSE
cb_delete.Enabled = FALSE

end event

