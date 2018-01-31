$PBExportHeader$w_injured_worker_or_ss.srw
$PBExportComments$select injured worker or surviving spouse from main menu based on DD criteria
forward
global type w_injured_worker_or_ss from window
end type
type dw_details from u_dw_online within w_injured_worker_or_ss
end type
type cb_cancel from commandbutton within w_injured_worker_or_ss
end type
type cb_ok from commandbutton within w_injured_worker_or_ss
end type
end forward

global type w_injured_worker_or_ss from window
integer x = 1335
integer y = 688
integer width = 1358
integer height = 756
boolean titlebar = true
string title = "Select Injured Worker"
windowtype windowtype = response!
long backcolor = 67108864
dw_details dw_details
cb_cancel cb_cancel
cb_ok cb_ok
end type
global w_injured_worker_or_ss w_injured_worker_or_ss

type variables
LONG	 il_claim_no
STRING is_window_to_open, is_annuity_payout_mode
end variables

event open;INTEGER              li_record_count
S_WINDOW_MESSAGE     lstr_message

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


/*
If accessing the module from the menu item, a search for a claim must first be performed.  
The menu item is only enabled if a claim is retrieved into the tombstone.  If the identified 
claim does not have a surviving spouse then all claims where the identified individual is the 
injured worker are loaded into the module.  If the selected claim has a surviving spouse as a 
claim participant then the user must select the individual (injured worker or surviving spouse) for 
which to open the module.  If the injured worker is selected then the data for all claims for the 
injured worker is displayed.  If the surviving spouse is selected then only the data for that individual
for the selected claim is displayed.
*/

/* grab the claim_no from the calling menu */
lstr_message = Message.PowerObjectParm	
il_claim_no = lstr_message.al_doubleparm[1]
is_window_to_open = lstr_message.as_stringparm[1]

IF UpperBound(lstr_message.as_stringparm) = 3 THEN
	is_annuity_payout_mode = lstr_message.as_stringparm[3]
END IF

/* make sure we have a valid claim no */
IF isnull(il_claim_no) OR il_claim_no <= 0 THEN
	//problem - return out
	CLOSE(THIS)
	RETURN 
END IF 

/*	Set up the DW */
dw_details.SetTransObject(SQLCA)
li_record_count = dw_details.retrieve(il_claim_no)
SQLCA.nf_handle_error("w_injured_worker_or_ss","open","li_record_count = dw_details.retrieve(il_claim_no)")

/* check the record count */
IF li_record_count < 1 THEN
	CLOSE(THIS)
	RETURN 
END IF 


end event

on w_injured_worker_or_ss.create
this.dw_details=create dw_details
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.Control[]={this.dw_details,&
this.cb_cancel,&
this.cb_ok}
end on

on w_injured_worker_or_ss.destroy
destroy(this.dw_details)
destroy(this.cb_cancel)
destroy(this.cb_ok)
end on

type dw_details from u_dw_online within w_injured_worker_or_ss
integer x = 37
integer y = 44
integer width = 1266
integer height = 428
integer taborder = 10
string dataobject = "d_entitlement_access"
boolean vscrollbar = true
end type

event constructor;call super::constructor;THIS.uf_setselect(1)
end event

type cb_cancel from commandbutton within w_injured_worker_or_ss
integer x = 704
integer y = 520
integer width = 379
integer height = 96
integer taborder = 70
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
end type

event clicked;CLOSE(PARENT)
end event

type cb_ok from commandbutton within w_injured_worker_or_ss
integer x = 283
integer y = 520
integer width = 379
integer height = 96
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;LONG              ll_individual_no, ll_claim_no
INTEGER           li_row, li_rows, li_rtn, li_incomplete_payout_count, li_other_annuity_accounts, li_msg_rtn
n_common_annuity  lnv_common_annuity
STRING            ls_claim_role, ls_exclude_reason, ls_exclude_message, ls_message
s_window_message  lstr_message
U_DS              lds_force_onto_payout
WINDOW            lw_window_to_open
		
/*
	If accessing the module from the menu item, a search for a claim must first be performed.  
	The menu item is only enabled if a claim is retrieved into the tombstone.  If the identified
	claim does not have a surviving spouse then all claims where the identified individual is the 
	injured worker are loaded into the module.  If the selected claim has a surviving spouse as a 
	claim participant then the user must select the individual (injured worker or surviving spouse)
	for which to open the module.  If the injured worker is selected then the data for all claims for
	the injured worker is displayed.  If the surviving spouse is selected then only the data 
	for that individual for the selected claim is displayed.
*/

//grab the selected row and see if the user has selected the surviving spouse or not
li_row = dw_details.getrow()

IF li_row > 0 THEN 
	//grab the claim role code
	ls_claim_role 		= dw_details.getitemstring(li_row,'claim_role_code')
	ll_individual_no 	= dw_details.getitemnumber(li_row,'individual_no')
	
	//make sure we have a valid value
	IF ISNULL(ls_claim_role) OR TRIM(ls_claim_role) = "" THEN
		messagebox('Claim Role Code', 'The Claim Role Code could not be determined. Please try again.')
		RETURN 
	END IF 
	
//	if the type <> 'SS' then go in as we do directly from the menu (action_code = 1) otherwise the action_code = 2
	IF ls_claim_role = 'C' THEN 
		ll_claim_no = 0
	ELSE
		ll_claim_no = il_claim_no
	END IF
	
	IF is_window_to_open = 'w_maintain_benefit_entitlement' THEN
		//setup the rest of the structure
		lstr_message.al_doubleparm[1] 	= 0// annuity_account_no
		lstr_message.al_doubleparm[2] 	= 0// eligibility_account_no	
		lstr_message.al_doubleparm[3] 	= 2//action code
		lstr_message.al_doubleparm[4] 	= il_claim_no//NO CLAIM_NO
		lstr_message.al_doubleparm[5] 	= ll_individual_no
		lstr_message.al_doubleparm[6] 	= 0 // annuity_payout_no
		lstr_message.as_stringparm[1] 	= ls_claim_role
		
	ELSEIF is_window_to_open = 'w_prepare_annuity_account' THEN
		
		lnv_common_annuity = Create n_common_annuity
		
		// does claimant have incomplete payout?
		SELECT COUNT(*)
		INTO   :li_incomplete_payout_count
		FROM   ANNUITY_PAYOUT  a
		JOIN   ANNUITY_ACCOUNT b ON a.annuity_account_no = b.annuity_account_no
		WHERE  b.individual_no              = :ll_individual_no
		AND    b.claim_no                   = :ll_claim_no
		AND    a.annuity_payout_status_code = 'I'
		USING SQLCA;
		SQLCA.nf_handle_error('w_injured_worker_or_ss','SELECT COUNT(*) FROM ANNUITY_PAYOUT, ANNUITY_ACCOUNT...','cb_ok.clicked')
		
		IF li_incomplete_payout_count = 0 THEN
			
			
			li_msg_rtn = MessageBox('','This individual might be added to the Prepare Annuity Payout list. If you do not want to add ' &
			             +'this individual to the list, click "No", Clear the person selected on the WorkBench and re-open the module.'&
							 +'~r~n~r~n'&
							 +'Do you want to continue?',Question!,YesNo!,2)
			IF li_msg_rtn = 2 THEN
				RETURN
			END IF
			
			
			// no incomplete payout, have to insert one using stored proc.			
			lds_force_onto_payout = Create U_DS
			lds_force_onto_payout.DataObject = 'ds_force_onto_payout'
			lds_force_onto_payout.SetTransObject(SQLCA)	
			
			li_rows = lds_force_onto_payout.Retrieve(ll_claim_no,ll_individual_no,'FORCE')
			SQLCA.nf_handle_error('w_injured_worker_or_ss','lds_force_onto_payout.retrieve','cb_ok.clicked')
			
			
			IF li_rows <> 1 THEN
				// problem!
				MessageBox('Payout List Error','The retrieval of the payout list for this individual encountered an error. Contact HELPDESK.')
			ELSE			
				ls_exclude_reason = lds_force_onto_payout.GetItemString(1,'exclude_reason')
				IF ls_exclude_reason <> '' THEN
					CHOOSE CASE ls_exclude_reason
						CASE 'paid out'
							IF ls_claim_role = 'C' THEN
								ls_exclude_message = 'The injured worker has a post-1992 Annuity Payout processed before the Annuity Payout module became effective.'
							ELSE
								ls_exclude_message = 'The surviving spouse has an Annuity Payout processed before the Annuity Payout module became effective.'
							END IF
						CASE 'no subledger'
							IF ls_claim_role = 'C' THEN
								ls_exclude_message = 'The injured worker has no post-1992 sub-ledger transactions.'
							ELSE
								ls_exclude_message = 'The surviving spouse has no sub-ledger transactions.'
							END IF
						CASE 'zero amount subledger'
							IF ls_claim_role = 'C' THEN
								ls_exclude_message = 'The injured worker has a post-1992 sub-ledger amount of $0.'
							ELSE
								ls_exclude_message = 'The surviving spouse has a sub-ledger amount of $0.'
							END IF
						CASE 'no annuity account'
							IF ls_claim_role = 'C' THEN
								ls_exclude_message = 'The injured worker does not have an annuity account.'
							ELSE
								ls_exclude_message = 'The surviving spouse does not have an annuity account.'
							END IF
						CASE 'no active eligibility'
							IF ls_claim_role = 'C' THEN
								ls_exclude_message = 'The injured worker is not eligible for annuity benefits.'
							ELSE
								ls_exclude_message = 'The surviving spouse is not eligible for annuity benefits.'
							END IF
						CASE 'future end date'
							IF ls_claim_role = 'C' THEN
								ls_exclude_message = 'The annuity end date for this injured worker is in the future.'
							ELSE
								ls_exclude_message = 'The annuity end date for this surviving spouse is in the future.'
							END IF
					END CHOOSE
					
					MessageBox('Not Eligible for List','This individual cannot be forced onto the Payout Annuity Account list for the following reason: ' + ls_exclude_message + '.')
					
					cb_cancel.TriggerEvent(clicked!)
					RETURN
				ELSE
					
					li_other_annuity_accounts = lnv_common_annuity.nf_individual_has_other_annuity_accounts(ll_individual_no,ll_claim_no)
			
					IF li_other_annuity_accounts > 0 THEN
						IF vgst_user_profile.default_admin_region_code = 'PRV' THEN 
							ls_message = 'This individual ('+String(ll_individual_no)+') has other annuity accounts. You can use the individual search to locate these annuity accounts.'
						ELSE
							ls_message = 'This individual ('+String(ll_individual_no)+') has other annuity accounts. You can use the individual search to locate these surviving spouse annuity accounts.'
						END IF
						MessageBox('',ls_message)
					END IF
					
					lstr_message.al_doubleparm[1] 	= ll_claim_no
					lstr_message.al_doubleparm[2] 	= ll_individual_no
					lstr_message.as_stringparm[1] 	= 'PRV'        // admin region had to be PRV to open this popup
					lstr_message.as_stringparm[2] 	= is_annuity_payout_mode
				END IF
			END IF
		ELSE
			li_other_annuity_accounts = lnv_common_annuity.nf_individual_has_other_annuity_accounts(ll_individual_no,ll_claim_no)
			
			IF li_other_annuity_accounts > 0 THEN
				IF vgst_user_profile.default_admin_region_code = 'PRV' THEN 
					ls_message = 'This individual ('+String(ll_individual_no)+') has other annuity accounts. You can use the individual search to locate these annuity accounts.'
				ELSE
					ls_message = 'This individual ('+String(ll_individual_no)+') has other annuity accounts. You can use the individual search to locate these surviving spouse annuity accounts.'
				END IF
				MessageBox('',ls_message)
			END IF
			
			lstr_message.al_doubleparm[1] 	= ll_claim_no
			lstr_message.al_doubleparm[2] 	= ll_individual_no
			lstr_message.as_stringparm[1] 	= 'PRV'        // admin region had to be PRV to open this popup
			lstr_message.as_stringparm[2] 	= is_annuity_payout_mode
		END IF
	END IF
		
	//OPEN THE MAINTAIN WINDOW
	openwithparm(lw_window_to_open,lstr_message,is_window_to_open)
	
	//close this window
	CLOSE(PARENT)
	
END IF 
end event

