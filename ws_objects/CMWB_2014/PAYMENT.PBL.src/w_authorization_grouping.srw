$PBExportHeader$w_authorization_grouping.srw
forward
global type w_authorization_grouping from w_ancestor
end type
type cb_cancel_reason from commandbutton within w_authorization_grouping
end type
type cb_save_reason from commandbutton within w_authorization_grouping
end type
type cb_delete_group from commandbutton within w_authorization_grouping
end type
type cb_remove from commandbutton within w_authorization_grouping
end type
type dw_details from datawindow within w_authorization_grouping
end type
type cb_close from commandbutton within w_authorization_grouping
end type
type cb_add from commandbutton within w_authorization_grouping
end type
type cb_create from commandbutton within w_authorization_grouping
end type
type dw_authorization_groups from u_dw_online within w_authorization_grouping
end type
type dw_groupable_payments from u_dw_online within w_authorization_grouping
end type
type gb_1 from groupbox within w_authorization_grouping
end type
type gb_2 from groupbox within w_authorization_grouping
end type
type gb_3 from groupbox within w_authorization_grouping
end type
end forward

global type w_authorization_grouping from w_ancestor
integer x = 0
integer width = 4082
integer height = 3040
string title = "Payment Grouping for Authorizations"
string menuname = ""
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
long backcolor = 67108864
event ue_postopen ( )
cb_cancel_reason cb_cancel_reason
cb_save_reason cb_save_reason
cb_delete_group cb_delete_group
cb_remove cb_remove
dw_details dw_details
cb_close cb_close
cb_add cb_add
cb_create cb_create
dw_authorization_groups dw_authorization_groups
dw_groupable_payments dw_groupable_payments
gb_1 gb_1
gb_2 gb_2
gb_3 gb_3
end type
global w_authorization_grouping w_authorization_grouping

type variables
LONG il_claim_no
STRING is_first_admin_region, is_first_type, is_comment_req
DATE idt_first_sched_proc_dt
n_payment inv_payment
end variables

forward prototypes
public subroutine wf_select_row (datawindow adw_requestor, long al_new_row)
public function integer wf_authorize_group (datawindow adw_dw, string as_action, boolean ab_remove)
end prototypes

event ue_postopen();LONG ll_rows

ll_rows = dw_groupable_payments.Retrieve(il_claim_no)
dw_authorization_groups.Retrieve(il_claim_no)

IF ll_rows = 0 THEN
	MessageBox('No Payments','There are no payments waiting to be grouped for Authorization.', Information!)
END IF

end event

public subroutine wf_select_row (datawindow adw_requestor, long al_new_row);
adw_requestor.SelectRow(0, FALSE)

IF al_new_row > 0 THEN
	adw_requestor.SelectRow(al_new_row, TRUE)
	adw_requestor.SetRow(al_new_row)
END IF

end subroutine

public function integer wf_authorize_group (datawindow adw_dw, string as_action, boolean ab_remove);//Authorization function - call to authorize new group, groups with additions/removals
LONG ll_cntr, ll_rowcount
DATETIME ldtm_authorize_date
STRING ls_authorize_user

IF ab_remove THEN
	adw_dw.SetFilter( "checkbox_group = 0")
	adw_dw.Filter()
END IF

IF as_action = 'U' THEN
	//IF not within limits or deleting group, unauthorize all payments
	ls_authorize_user = ''
	SetNull(ldtm_authorize_date)
ELSEIF as_action = 'A' THEN
	//IF within limits, authorize all payments	
	//IF removing a payment from a group, re-authorize all payments	
	//IF adding a payment to a group, re-authorize all payments
	ls_authorize_user = vgst_user_profile.user_id
	ldtm_authorize_date = f_server_datetime()
END IF

ll_rowcount = adw_dw.RowCount()

IF ll_rowcount > 0 THEN
	//Update records to either authorize or unauthorize records depending on which action user has taken
	FOR ll_cntr = 1 to ll_rowcount
		adw_dw.SetItem(ll_cntr, 'authorized_by_code', ls_authorize_user)
		adw_dw.SetItem(ll_cntr, 'authorized_date', ldtm_authorize_date)
	NEXT 
END IF

RETURN 0
end function

on w_authorization_grouping.create
int iCurrent
call super::create
this.cb_cancel_reason=create cb_cancel_reason
this.cb_save_reason=create cb_save_reason
this.cb_delete_group=create cb_delete_group
this.cb_remove=create cb_remove
this.dw_details=create dw_details
this.cb_close=create cb_close
this.cb_add=create cb_add
this.cb_create=create cb_create
this.dw_authorization_groups=create dw_authorization_groups
this.dw_groupable_payments=create dw_groupable_payments
this.gb_1=create gb_1
this.gb_2=create gb_2
this.gb_3=create gb_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_cancel_reason
this.Control[iCurrent+2]=this.cb_save_reason
this.Control[iCurrent+3]=this.cb_delete_group
this.Control[iCurrent+4]=this.cb_remove
this.Control[iCurrent+5]=this.dw_details
this.Control[iCurrent+6]=this.cb_close
this.Control[iCurrent+7]=this.cb_add
this.Control[iCurrent+8]=this.cb_create
this.Control[iCurrent+9]=this.dw_authorization_groups
this.Control[iCurrent+10]=this.dw_groupable_payments
this.Control[iCurrent+11]=this.gb_1
this.Control[iCurrent+12]=this.gb_2
this.Control[iCurrent+13]=this.gb_3
end on

on w_authorization_grouping.destroy
call super::destroy
destroy(this.cb_cancel_reason)
destroy(this.cb_save_reason)
destroy(this.cb_delete_group)
destroy(this.cb_remove)
destroy(this.dw_details)
destroy(this.cb_close)
destroy(this.cb_add)
destroy(this.cb_create)
destroy(this.dw_authorization_groups)
destroy(this.dw_groupable_payments)
destroy(this.gb_1)
destroy(this.gb_2)
destroy(this.gb_3)
end on

event open;call super::open;
il_claim_no = Message.DoubleParm

THIS.x = w_frame.x
THIS.y = w_frame.y

dw_groupable_payments.SetTransObject(SQLCA)
dw_authorization_groups.SetTransObject(SQLCA)
dw_details.SetTransObject(SQLCA)

inv_payment = CREATE n_payment

THIS.PostEvent('ue_postopen')


end event

type cb_cancel_reason from commandbutton within w_authorization_grouping
integer x = 2606
integer y = 2028
integer width = 919
integer height = 104
integer taborder = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Cancel Reason/Comment Change"
end type

event clicked;dw_groupable_payments.Retrieve(il_claim_no)
dw_authorization_groups.Retrieve(il_claim_no)

cb_save_reason.Enabled = FALSE
cb_cancel_reason.Enabled = FALSE
cb_create.Enabled = TRUE
cb_delete_group.Enabled = TRUE
cb_add.Enabled = TRUE
cb_remove.Enabled = TRUE
end event

type cb_save_reason from commandbutton within w_authorization_grouping
integer x = 1723
integer y = 2028
integer width = 869
integer height = 104
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Save Reason/Comment Change"
end type

event clicked;LONG ll_cntr, ll_rowcount, ll_rtn, ll_row
STRING ls_reason_code, ls_comment, ls_comment_required

dw_authorization_groups.AcceptText()
ll_rowcount = dw_details.RowCount()
ll_row = dw_authorization_groups.GetRow()

IF ll_row > 0 THEN
	
	SQLCA.nf_begin_transaction()
	
	ls_reason_code = dw_authorization_groups.GetItemString(ll_row,'authorization_group_reason_code')
	ls_comment = dw_authorization_groups.GetItemString(ll_row, 'authorization_group_reason_comment')

	IF IsNull(ls_reason_code) OR ls_reason_code = '' THEN
		MessageBox('Reason Required','You must select a reason for this Authorization Group.', Information!)
		SQLCA.nf_rollback_transaction()
		RETURN
	END IF
	
	IF ls_comment = '' OR IsNull(ls_comment) THEN
		IF is_comment_req = 'Y' THEN
			MessageBox('Comment Required','A comment is required for the Authorization Group Reason you have selected.', Information!)
			SQLCA.nf_rollback_transaction()
			RETURN 
		END IF
	END IF
	
	IF ll_rowcount > 0 THEN		
		FOR ll_cntr = 1 to ll_rowcount				
			dw_details.SetItem(ll_cntr, 'authorization_group_reason_code', ls_reason_code)
			dw_details.SetItem(ll_cntr, 'authorization_group_reason_comment', ls_comment)
		NEXT
	END IF
	
	ll_rtn = dw_details.Update()
	IF ll_rtn < 0 THEN
		SQLCA.nf_rollback_transaction()
	ELSE
		SQLCA.nf_commit_transaction()
		dw_groupable_payments.Retrieve(il_claim_no)
		dw_authorization_groups.Retrieve(il_claim_no)
	END IF
END IF

cb_save_reason.Enabled = FALSE
cb_cancel_reason.Enabled = FALSE
cb_create.Enabled = TRUE
cb_delete_group.Enabled = TRUE
cb_add.Enabled = TRUE
cb_remove.Enabled = TRUE
end event

type cb_delete_group from commandbutton within w_authorization_grouping
integer x = 3538
integer y = 2028
integer width = 430
integer height = 104
integer taborder = 70
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Delete Group"
end type

event clicked;LONG ll_auth_no, ll_rowcount, ll_cntr, ll_rtn
DATE ldt_null
STRING ls_action

SetNull(ldt_null)

IF dw_authorization_groups.RowCount() > 0 THEN
	IF MessageBox('Delete?','Are you sure you would like to delete this Authorization Group?',Question!, YesNo!) = 1 THEN
		ll_auth_no = dw_authorization_groups.GetItemNumber(dw_authorization_groups.GetRow(), 'authorization_group_no')
		IF ll_auth_no > 0 THEN
			ll_rowcount = dw_details.RowCount()
			IF ll_rowcount > 0 THEN
				SQLCA.nf_begin_transaction()
				FOR ll_cntr = 1 to ll_rowcount
					dw_details.SetItem(ll_cntr, 'authorization_group_no', 0)
					dw_details.SetItem(ll_cntr, 'authorization_group_user_id','')
					dw_details.SetItem(ll_cntr, 'authorization_group_date', ldt_null)
					dw_details.SetItem(ll_cntr, 'authorization_group_reason_code','')
					dw_details.SetItem(ll_cntr, 'authorization_group_reason_comment','')			
				NEXT

				//don't need to check limit, all are being unauthorized regardless			
				ll_rtn = inv_payment.nf_authorize_group(dw_details, 'U', FALSE)
				IF ll_rtn >= 0 THEN
					ll_rtn = dw_details.Update()
					IF ll_rtn < 0 THEN
						SQLCA.nf_rollback_transaction()
					ELSE
						SQLCA.nf_commit_transaction()
						dw_groupable_payments.Retrieve(il_claim_no)
						dw_authorization_groups.Retrieve(il_claim_no)
						
						MESSAGEBOX("Unauthorized payment", "Payments in the group being deleted will become unauthorized")
					END IF
				ELSE
					SQLCA.nf_rollback_transaction()
				END IF
			END IF
		END IF
	END IF
ELSE
	MessageBox('No Authorization Group','There are no Authorization Groups to delete.', Information!)
END IF
end event

type cb_remove from commandbutton within w_authorization_grouping
integer x = 3031
integer y = 2832
integer width = 603
integer height = 104
integer taborder = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Remove from Group"
end type

event clicked;LONG ll_rowcount, ll_cntr, ll_selected, ll_group_rowcount, ll_cntr2, ll_selected_group, ll_authorization_group_no, ll_rtn, ll_rowcnt_grp, ll_is_selected, ll_remaining, ll_removed
BOOLEAN lb_selected_group, lb_unauthorized_payments_message
STRING ls_reason, ls_reason_comment, ls_null, ls_action, ls_admin_region, ls_type, ls_remove
DATETIME ldtm_current_date
DECIMAL {2} ldec_limit, ldec_payment_amount

SetNull(ldtm_current_date)

ll_rowcount = dw_details.RowCount()

IF ll_rowcount > 0 THEN
	ldec_payment_amount = dw_details.GetItemNumber(dw_details.GetRow(), 'compute_total')
	ls_admin_region = dw_details.GetItemString(dw_details.GetRow(),'admin_region_code')
	ls_type = dw_details.GetItemString(dw_details.GetRow(), 'authorization_type_code')

	FOR ll_cntr = 1 to ll_rowcount
		ll_is_selected = dw_details.GetItemNumber(ll_cntr,'checkbox_group')
		IF ll_is_selected > 0 THEN 
			ll_selected = ll_selected + 1
		END IF
	NEXT
	IF ll_selected = 0 THEN
		MessageBox('No Payments Selected','You must have at least one payment selected before you can remove it from an Authorization Group.', Information!)
		RETURN
	END IF
	
	ll_remaining = ll_rowcount - ll_selected
	IF ll_remaining = 1 THEN
		MessageBox('Incorrect Number of Payments','You must have at least 2 payments remaining in the Authorization Group.', Information!)
		RETURN 
	ELSEIF ll_remaining = 0 THEN
		IF MessageBox('Remove Group?','Removing all payments from the Authorization Group will remove the entire group. Would you like to continue?', Question!, YesNo!) = 2 THEN
			RETURN 
		END IF
	END IF
	
	ll_cntr = 0
	ll_is_selected = 0
	
	SQLCA.nf_begin_transaction()
		
	FOR ll_cntr = 1 to ll_rowcount
		ll_is_selected = dw_details.GetItemNumber(ll_cntr,'checkbox_group')
		IF ll_is_selected > 0 THEN 
			dw_details.SetItem(ll_cntr, 'authorization_group_no',0)
			dw_details.SetItem(ll_cntr, 'authorization_group_date', ldtm_current_date)
			dw_details.SetItem(ll_cntr, 'authorization_group_user_id', '')
			dw_details.SetItem(ll_cntr, 'authorization_group_reason_code','')
			dw_details.SetItem(ll_cntr, 'authorization_group_reason_comment','')
			
			IF dw_details.GetItemString(ll_cntr, 'authorized_by_code') > '' THEN dw_details.SetItem(ll_cntr, 'authorized_by_code', '')
			IF NOT IsNull(dw_details.GetItemDateTime(ll_cntr, 'authorized_date')) THEN dw_details.SetItem(ll_cntr, 'authorized_date', ldtm_current_date)
			lb_unauthorized_payments_message = true
			ldec_payment_amount = ldec_payment_amount - dw_details.GetItemNumber(ll_cntr,'total_payment_amount')		
		END IF
	NEXT
		
	IF gnv_user_authorizations.nf_authorizations_exist(ls_admin_region,ls_type) = FALSE THEN
		MessageBox('Warning',"You don't have the proper Authorization authority for this claims region." )
		ls_action = 'U'
	ELSE

		ldec_limit = gnv_user_authorizations.nf_get_authorization_limit(ls_admin_region,ls_type)
		IF ldec_limit < 0 THEN
			SQLCA.nf_rollback_transaction()
			RETURN
		ELSE
			IF ldec_payment_amount > ldec_limit THEN
				Messagebox('No Authorization','You do not have a sufficient Authorization Limit to authorize this Authorization Group.~r~n~r~nThis Authorization Group will remain unauthorized until it can be authorized in the Payment Authorization screen.', Exclamation! )
				ls_action = 'U'
			ELSE
				ls_action= 'A'
			END IF
		END IF
	END IF

	ll_rtn = inv_payment.nf_authorize_group(dw_details, ls_action, TRUE)

	IF ll_rtn >= 0 THEN
		ll_rtn = dw_details.Update()
		IF ll_rtn < 0 THEN
			SQLCA.nf_rollback_transaction()
		ELSE
			SQLCA.nf_commit_transaction()
			dw_groupable_payments.Retrieve(il_claim_no)
			dw_authorization_groups.Retrieve(il_claim_no)
			IF lb_unauthorized_payments_message Then
				MESSAGEBOX("Unauthorized payment", "Payments being removed from the group will become unauthorized")
			END IF
		END IF
	ELSE
		SQLCA.nf_rollback_transaction()
	END IF
ELSE
	MessageBox('No Payments to Remove','There are no payments to remove.', Information!)
	RETURN
END IF

end event

type dw_details from datawindow within w_authorization_grouping
integer x = 87
integer y = 2212
integer width = 3886
integer height = 564
integer taborder = 40
string title = "none"
string dataobject = "d_unprocessed_auth_group_details"
boolean vscrollbar = true
boolean livescroll = true
end type

type cb_close from commandbutton within w_authorization_grouping
integer x = 3653
integer y = 2832
integer width = 315
integer height = 104
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Close"
end type

event clicked;CloseWithReturn(Parent, 1)

end event

type cb_add from commandbutton within w_authorization_grouping
integer x = 3191
integer y = 788
integer width = 773
integer height = 104
integer taborder = 40
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Add to Existing Group"
end type

event clicked;LONG ll_rowcount, ll_cntr, ll_selected, ll_group_rowcount, ll_cntr2, ll_selected_group, ll_authorization_group_no, ll_rtn, ll_rowcnt_grp, ll_is_selected, ll_groups
BOOLEAN lb_selected_group
STRING ls_reason, ls_reason_comment, ls_action, ls_admin_region, ls_type, ls_group_user_id
DECIMAL {2} ldec_limit, ldec_payment_amount
DATETIME ldtm_group_date
DATE ldt_sched_proc_dt

ll_rowcount = dw_groupable_payments.RowCount()
ll_rowcnt_grp = dw_details.RowCount()
ll_groups = dw_authorization_groups.RowCount()

IF ll_groups = 0 THEN
	MessageBox('No Authorization Groups','There are no existing Authorization Groups. You must create a New Group to add the selected payments to.', Information!)
	RETURN
END IF

IF ll_rowcount > 0 THEN
	IF ll_rowcnt_grp > 0 THEN
		is_first_admin_region = dw_details.GetItemString(1,'admin_region_code')
		is_first_type = dw_details.GetItemString(1, 'authorization_type_code')
		idt_first_sched_proc_dt = DATE(dw_details.GetItemDateTime(1,'scheduled_processing_date'))
	END IF
	FOR ll_cntr = 1 to ll_rowcount
		ll_is_selected = dw_groupable_payments.GetItemNumber(ll_cntr,'checkbox_group')
		IF ll_is_selected > 0 THEN 
			ls_admin_region = dw_groupable_payments.GetItemString(ll_cntr, 'admin_region_code')
			IF is_first_admin_region <> ls_admin_region AND ll_rowcnt_grp > 0 THEN
				MessageBox('Incorrect Admin Region','All payments within an Authorization Group must have the same admin region code.~r~n~r~nThe Authorization Group you have chosen contains payments in Admin Region: ' + ls_admin_region, Information!)
				RETURN
			END IF
			ls_type = dw_groupable_payments.GetItemString(ll_cntr, 'authorization_type_code')
			IF is_first_type <> ls_type  AND ll_rowcnt_grp > 0 THEN
				MessageBox('Incorrect Authorization Type Combination','All payments within an Authorization Group must have the same Authorization Type: ' &
				                  + " ~r~n - Loss of Earnings/Allowances, includes RLOE, LTD, Care and Clothing Allowance Payments(Not Awards) ~r~n - Pensions ~r~n - Survivor's Special Payment", Information!)
				RETURN
			END IF
			ldt_sched_proc_dt = DATE(dw_groupable_payments.GetItemDateTime(ll_cntr,'scheduled_processing_date'))
			IF idt_first_sched_proc_dt <> ldt_sched_proc_dt THEN
				MessageBox('Incorrect Scheduled Processing Date','All payments within an Authorization Group must have the same scheduled processing date', Information!)
				RETURN
			END IF
			ll_selected = ll_selected + 1
		END IF
	NEXT
	IF ll_selected = 0 THEN
		MessageBox('No Payments Selected','You must have at least one payment selected before you can assign to an Authorization Group.', Information!)
		RETURN
	END IF

	
	ll_cntr = 0
	ll_is_selected = 0
	
	ll_group_rowcount = dw_authorization_groups.RowCount()
	
	IF ll_group_rowcount > 0 THEN
		FOR ll_cntr2 = 1 to ll_group_rowcount
			lb_selected_group = dw_authorization_groups.IsSelected(ll_cntr2)
			IF lb_selected_group THEN
				ll_selected_group = ll_selected_group + 1
			END IF
		NEXT
		IF ll_selected_group = 0 THEN
			MessageBox('No Authorization Group Selected','You must have an Authorization Group selected before you group a payment', Information!)
			RETURN
		END IF
		
		SQLCA.nf_begin_transaction()
		ldtm_group_date = dw_authorization_groups.GetItemDateTime(dw_authorization_groups.GetRow(), 'authorization_group_date')
		ls_reason = dw_authorization_groups.GetItemString(dw_authorization_groups.GetRow(), 'authorization_group_reason_code')
		ls_reason_comment = dw_authorization_groups.GetItemString(dw_authorization_groups.GetRow(),'authorization_group_reason_comment')
		ll_authorization_group_no = dw_authorization_groups.GetItemNumber(dw_authorization_groups.GetRow(), 'authorization_group_no')
		ls_group_user_id = dw_authorization_groups.GetItemString(dw_authorization_groups.GetRow(), 'authorization_group_user_id')
		IF ll_authorization_group_no > 0 THEN
			FOR ll_cntr = 1 to ll_rowcount
				ll_is_selected = dw_groupable_payments.GetItemNumber(ll_cntr,'checkbox_group')
				IF ll_is_selected > 0 THEN 
					dw_groupable_payments.SetItem(ll_cntr, 'authorization_group_no',ll_authorization_group_no)
					dw_groupable_payments.SetItem(ll_cntr, 'authorization_group_date', ldtm_group_date)
					dw_groupable_payments.SetItem(ll_cntr, 'authorization_group_user_id', ls_group_user_id)
					dw_groupable_payments.SetItem(ll_cntr, 'authorization_group_reason_code', ls_reason)
					dw_groupable_payments.SetItem(ll_cntr, 'authorization_group_reason_comment', ls_reason_comment)
				END IF
			NEXT
		ELSE
			MessageBox('Authorization Group', 'The Authorization Group is not valid. Please select another Authorization Group.', Exclamation!)
			SQLCA.nf_rollback_transaction()
			RETURN
		END IF

		ll_rtn = dw_groupable_payments.Update()
		dw_details.Retrieve(ll_authorization_group_no)
		ldec_payment_amount = dw_details.GetItemNumber(dw_details.GetRow(),'compute_total')

		IF gnv_user_authorizations.nf_authorizations_exist(ls_admin_region,ls_type) = FALSE THEN
			MessageBox('Warning',"You don't have the proper Authorization authority for this claims region." )
			ls_action = 'U'
		ELSE
			ldec_limit = gnv_user_authorizations.nf_get_authorization_limit(ls_admin_region,ls_type)
			IF ldec_limit < 0 THEN
				SQLCA.nf_rollback_transaction()
				RETURN
			ELSE
				IF ldec_payment_amount > ldec_limit THEN
					Messagebox('No Authorization','You do not have a sufficient Authorization Limit to authorize this Authorization Group.~r~n~r~nThis Authorization Group will remain unauthorized until it can be authorized in the Payment Authorization screen.', Exclamation! )
					ls_action = 'U'
				ELSE
					ls_action= 'A'
				END IF
			END IF
		END IF

		ll_rtn = inv_payment.nf_authorize_group(dw_details, ls_action, FALSE)
		IF ll_rtn >= 0 THEN
			ll_rtn = dw_details.Update()

			IF ll_rtn < 0 THEN
				SQLCA.nf_rollback_transaction()
			ELSE
				SQLCA.nf_commit_transaction()
				dw_groupable_payments.Retrieve(il_claim_no)
				dw_authorization_groups.Retrieve(il_claim_no)
			END IF
		ELSE
			SQLCA.nf_rollback_transaction()
		END IF
	END IF
ELSE
	MessageBox('No Payments to Add','There are no flagged payments to add to an Authorization Group', Information!)
	RETURN
END IF

end event

type cb_create from commandbutton within w_authorization_grouping
integer x = 2715
integer y = 788
integer width = 443
integer height = 104
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Create Group"
end type

event clicked;LONG ll_rowcount, ll_cntr, ll_selected, ll_auth_no, ll_rtn, ll_is_selected
DATETIME ldtm_current_date
DECIMAL {2} ldec_total_payment_amount, ldec_payment_amount, ldec_limit
STRING ls_reason, ls_reason_comment, ls_action, ls_admin_region, ls_type
s_window_message lstr_reason
DATE ldt_sched_proc_dt
BOOLEAN lb_first_found

ll_rowcount = dw_groupable_payments.RowCount()
ldtm_current_date = f_server_datetime()


IF ll_rowcount > 0 THEN
	FOR ll_cntr = 1 to ll_rowcount
		ll_is_selected = dw_groupable_payments.GetItemNumber(ll_cntr, 'checkbox_group')
		IF ll_is_selected > 0  THEN 
			IF lb_first_found = FALSE THEN
				is_first_admin_region = dw_groupable_payments.GetItemString(ll_cntr, 'admin_region_code')
				is_first_type = dw_groupable_payments.GetItemString(ll_cntr, 'authorization_type_code')
				idt_first_sched_proc_dt = DATE(dw_groupable_payments.GetItemDateTime(ll_cntr,'scheduled_processing_date'))
				lb_first_found = TRUE
				ll_selected = ll_selected + 1
				ldec_payment_amount = ldec_payment_amount + dw_groupable_payments.GetItemNumber(ll_cntr, 'total_payment_amount')
				CONTINUE
			END IF
			ls_admin_region = dw_groupable_payments.GetItemString(ll_cntr, 'admin_region_code')
			IF is_first_admin_region <> ls_admin_region THEN
				MessageBox('Incorrect Admin Region','All payments within an Authorization Group must have the same admin region code', Information!)
				RETURN
			END IF
			ls_type = dw_groupable_payments.GetItemString(ll_cntr, 'authorization_type_code')
			IF is_first_type <> ls_type THEN
				MessageBox('Incorrect Authorization Type Combination','All payments within an Authorization Group must have the same Authorization Type: ' &
				                  + " ~r~n - Loss of Earnings/Allowances, includes RLOE, LTD, Care and Clothing Allowance Payments(Not Awards) ~r~n - Pensions ~r~n - Survivor's Special Payment", Information!)
				RETURN
			END IF
			ldt_sched_proc_dt = DATE(dw_groupable_payments.GetItemDateTime(ll_cntr,'scheduled_processing_date'))
			IF idt_first_sched_proc_dt <> ldt_sched_proc_dt THEN
				MessageBox('Incorrect Scheduled Processing Date','All payments within an Authorization Group must have the same scheduled processing date', Information!)
				RETURN
			END IF
			ll_selected = ll_selected + 1
			ldec_payment_amount = ldec_payment_amount + dw_groupable_payments.GetItemNumber(ll_cntr, 'total_payment_amount')
		END IF
	NEXT
	IF ll_selected < 2 THEN
		MessageBox('No Payments Selected','You must have two or more payments selected before you can create an Authorization Group.', Information!)
		RETURN
	END IF
	
	//			Open up popup to get reason & comment
	Open(w_authorization_group_reason)
    lstr_reason[] =	Message.PowerObjectParm
	 
	IF NOT IsNull(lstr_reason)  THEN		
		
		SQLCA.nf_begin_transaction()
		
		ls_reason = lstr_reason.as_stringparm[1]
		ls_reason_comment = lstr_reason.as_stringparm[2]
		
		ll_cntr = 0
		ll_selected = 0
		ll_is_selected = 0
		
		UPDATE Last_Authorization_Group_No
		SET       last_authorization_group_no = last_authorization_group_no + 1
		USING   SQLCA;
		
		SQLCA.nf_handle_error('w_authorization_grouping','cb_create','UPDATE Last_Authorization_Group_No')
		
		SELECT last_authorization_group_no 
		INTO     :ll_auth_no
		FROM     Last_Authorization_Group_No
		USING   SQLCA;
		
		SQLCA.nf_handle_error('w_authorization_grouping','cb_create','SELECT last_authorization_group_no')
		
		FOR ll_cntr = 1 to ll_rowcount	
		
			ll_is_selected = dw_groupable_payments.GetItemNumber(ll_cntr,'checkbox_group')
			IF ll_is_selected > 0 THEN 
				dw_groupable_payments.SetItem(ll_cntr, 'authorization_group_no',  ll_auth_no )
				dw_groupable_payments.SetItem(ll_cntr, 'authorization_group_date', DATE(ldtm_current_date ))
				dw_groupable_payments.SetItem(ll_cntr, 'authorization_group_user_id',  vgst_user_profile.user_id)
				dw_groupable_payments.SetItem(ll_cntr, 'authorization_group_reason_code', ls_reason)
				dw_groupable_payments.SetItem(ll_cntr, 'authorization_group_reason_comment', ls_reason_comment)
			 END IF
		NEXT
		
		IF gnv_user_authorizations.nf_authorizations_exist(ls_admin_region,ls_type) = FALSE THEN
			MessageBox('Warning',"You don't have the proper Authorization authority for this claims region.~r~n~r~nThis Authorization Group will remain unauthorized until it can be authorized in the Payment Authorization screen." )
			ls_action = 'U'
		ELSE

			ldec_limit = gnv_user_authorizations.nf_get_authorization_limit(ls_admin_region,ls_type)
			IF ldec_limit < 0 THEN
				Messagebox('No Authorization','There is a problem with your Authorization Limit, please contact HelpDesk', Exclamation! )
				SQLCA.nf_rollback_transaction()
				RETURN
			ELSE
				IF ldec_payment_amount > ldec_limit THEN
					Messagebox('No Authorization','You do not have a sufficient Authorization Limit to authorize this Authorization Group.~r~n~r~nThis Authorization Group will remain unauthorized until it can be authorized in the Payment Authorization screen.', Exclamation! )
					ls_action = 'U'
				ELSE
					ls_action= 'A'
				END IF
			END IF
		END IF
		
		ll_rtn =  inv_payment.nf_authorize_group(dw_groupable_payments, ls_action, FALSE)
		IF ll_rtn >= 0 THEN
			ll_rtn = dw_groupable_payments.Update()
			IF ll_rtn < 0 THEN
				SQLCA.nf_rollback_transaction()
			ELSE
				SQLCA.nf_commit_transaction()
				dw_groupable_payments.Retrieve(il_claim_no)
				dw_authorization_groups.Retrieve(il_claim_no)
			END IF
		ELSE
			SQLCA.nf_rollback_transaction()
		END IF
	END IF
ELSE
	MessageBox('No Payments','There are no payments flagged to be grouped for Authorizations.', Information!)
	RETURN
END IF




end event

type dw_authorization_groups from u_dw_online within w_authorization_grouping
integer x = 87
integer y = 984
integer width = 3886
integer height = 996
integer taborder = 20
string dataobject = "d_unprocessed_auth_groups"
boolean vscrollbar = true
end type

event clicked;call super::clicked;LONG	ll_return

IF row > 0 THEN
	wf_select_row(THIS, row)	
END IF
end event

event rowfocuschanged;call super::rowfocuschanged;LONG ll_rows, ll_auth_no
STRING ls_comment_req

IF currentrow > 0 THEN
	ll_auth_no = THIS.GetItemNumber(currentrow, 'authorization_group_no')
	ls_comment_req = THIS.GetItemString(currentrow, 'comment_required')
	
	IF ll_auth_no > 0 THEN
		dw_details.Retrieve(ll_auth_no)
	END IF
	
ELSE
	dw_authorization_groups.Reset()
	dw_details.Reset()
END IF
end event

event retrieveend;call super::retrieveend;LONG ll_auth_no

IF rowcount > 0 THEN
	THIS.SetRow(1)
	THIS.SelectRow(1, TRUE)
	
	ll_auth_no = dw_authorization_groups.GetItemNumber(1, 'authorization_group_no')
	IF ll_auth_no > 0 THEN
		dw_details.Retrieve(ll_auth_no)
	END IF
ELSE
	dw_details.Reset()
END IF

end event

event itemchanged;call super::itemchanged;STRING ls_reason_code

ls_reason_code = data

SELECT comment_required
INTO     :is_comment_req
FROM    Authorization_Group_Reason
WHERE  authorization_group_reason_code = :ls_reason_code
USING   SQLCA;

SQLCA.nf_handle_error('w_authorization_grouping','dw_authorization_groups.itemchanged','SELECT comment_required FROM Authorization_Group_Reason')

cb_save_reason.Enabled = TRUE
cb_cancel_reason.Enabled = TRUE
cb_create.Enabled = FALSE
cb_delete_group.Enabled = FALSE
cb_add.Enabled = FALSE
cb_remove.Enabled = FALSE
end event

event editchanged;call super::editchanged;
IF dwo.Name = 'authorization_group_reason_comment' THEN
	cb_save_reason.Enabled = TRUE
	cb_cancel_reason.Enabled = TRUE
	cb_create.Enabled = FALSE
	cb_delete_group.Enabled = FALSE
	cb_add.Enabled = FALSE
	cb_remove.Enabled = FALSE
END IF
end event

event rowfocuschanging;call super::rowfocuschanging;STRING ls_comment_old, ls_comment_new, ls_reason_old, ls_reason_new

dw_authorization_groups.AcceptText()

ls_reason_old = dw_authorization_groups.GetItemString(currentrow, 'authorization_group_reason_code', PRIMARY!, TRUE)
ls_reason_new = dw_authorization_groups.GetItemString(currentrow, 'authorization_group_reason_code', PRIMARY!, FALSE)

ls_comment_old = dw_authorization_groups.GetItemString(currentrow, 'authorization_group_reason_comment', PRIMARY!, TRUE)
ls_comment_new = dw_authorization_groups.GetItemString(currentrow, 'authorization_group_reason_comment', PRIMARY!, FALSE)

IF ls_reason_old <> ls_reason_new THEN
	MessageBox('Save','Please save the Authorization Group Reason before changing rows.', Information!)
	RETURN 1
END IF

IF ls_comment_old <> ls_comment_new THEN
	MessageBox('Save','Please save the Authorization Group Reason Comment before changing rows.', Information!)
	RETURN 1
END IF
end event

type dw_groupable_payments from u_dw_online within w_authorization_grouping
integer x = 87
integer y = 164
integer width = 3886
integer height = 564
integer taborder = 10
string dataobject = "d_ungrouped_payments"
boolean vscrollbar = true
end type

type gb_1 from groupbox within w_authorization_grouping
integer x = 41
integer y = 68
integer width = 3977
integer height = 692
integer taborder = 10
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Ungrouped Payments"
borderstyle borderstyle = stylebox!
end type

type gb_2 from groupbox within w_authorization_grouping
integer x = 41
integer y = 900
integer width = 3977
integer height = 1112
integer taborder = 20
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Authorization Groups"
borderstyle borderstyle = stylebox!
end type

type gb_3 from groupbox within w_authorization_grouping
integer x = 41
integer y = 2124
integer width = 3977
integer height = 684
integer taborder = 30
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Group Details"
borderstyle borderstyle = stylebox!
end type

