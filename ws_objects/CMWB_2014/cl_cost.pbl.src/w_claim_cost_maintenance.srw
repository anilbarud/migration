$PBExportHeader$w_claim_cost_maintenance.srw
forward
global type w_claim_cost_maintenance from w_ancestor
end type
type dw_r1_payments_to_delete from datawindow within w_claim_cost_maintenance
end type
type st_splitbar_1 from u_splitbar_horizontal within w_claim_cost_maintenance
end type
type rb_total_amt from radiobutton within w_claim_cost_maintenance
end type
type rb_amount from radiobutton within w_claim_cost_maintenance
end type
type rb_pct from radiobutton within w_claim_cost_maintenance
end type
type rb_pay_loe from radiobutton within w_claim_cost_maintenance
end type
type rb_pay_act from radiobutton within w_claim_cost_maintenance
end type
type rb_pay_all_types from radiobutton within w_claim_cost_maintenance
end type
type cb_find from commandbutton within w_claim_cost_maintenance
end type
type st_sel from statictext within w_claim_cost_maintenance
end type
type cb_search from commandbutton within w_claim_cost_maintenance
end type
type cbx_1 from checkbox within w_claim_cost_maintenance
end type
type cb_clear from commandbutton within w_claim_cost_maintenance
end type
type gb_filter from groupbox within w_claim_cost_maintenance
end type
type em_amount from editmask within w_claim_cost_maintenance
end type
type em_pct from editmask within w_claim_cost_maintenance
end type
type cb_apply from commandbutton within w_claim_cost_maintenance
end type
type cb_cancel_pct from commandbutton within w_claim_cost_maintenance
end type
type cbx_oldest_to_newest from checkbox within w_claim_cost_maintenance
end type
type gb_pct from groupbox within w_claim_cost_maintenance
end type
type dw_confirm_summary_totals from u_dw_online within w_claim_cost_maintenance
end type
type dw_parameter from u_dw_online within w_claim_cost_maintenance
end type
type uo_filter from u_filter_control within w_claim_cost_maintenance
end type
type dw_confirmation_summary from u_dw_online within w_claim_cost_maintenance
end type
type cb_close_confirm from commandbutton within w_claim_cost_maintenance
end type
type cb_close from commandbutton within w_claim_cost_maintenance
end type
type cb_close_unprocessed from commandbutton within w_claim_cost_maintenance
end type
type cb_save from commandbutton within w_claim_cost_maintenance
end type
type cb_delete from commandbutton within w_claim_cost_maintenance
end type
type dw_confirmation from u_dw_online within w_claim_cost_maintenance
end type
type dw_claimcost_unapplied_claim_txn from u_dw_online within w_claim_cost_maintenance
end type
type cb_refresh from commandbutton within w_claim_cost_maintenance
end type
type cb_unit_of_work_save from commandbutton within w_claim_cost_maintenance
end type
type cb_cancel from commandbutton within w_claim_cost_maintenance
end type
type cb_confirm from commandbutton within w_claim_cost_maintenance
end type
type dw_transaction_list from u_dw_online within w_claim_cost_maintenance
end type
end forward

global type w_claim_cost_maintenance from w_ancestor
boolean visible = false
integer width = 4832
integer height = 2896
string title = "Claim Cost Maintenance"
string menuname = "m_cmwb_notools"
windowtype windowtype = main!
long backcolor = 67108864
boolean toolbarvisible = false
event ue_load_module_security ( )
event ue_print ( )
dw_r1_payments_to_delete dw_r1_payments_to_delete
st_splitbar_1 st_splitbar_1
rb_total_amt rb_total_amt
rb_amount rb_amount
rb_pct rb_pct
rb_pay_loe rb_pay_loe
rb_pay_act rb_pay_act
rb_pay_all_types rb_pay_all_types
cb_find cb_find
st_sel st_sel
cb_search cb_search
cbx_1 cbx_1
cb_clear cb_clear
gb_filter gb_filter
em_amount em_amount
em_pct em_pct
cb_apply cb_apply
cb_cancel_pct cb_cancel_pct
cbx_oldest_to_newest cbx_oldest_to_newest
gb_pct gb_pct
dw_confirm_summary_totals dw_confirm_summary_totals
dw_parameter dw_parameter
uo_filter uo_filter
dw_confirmation_summary dw_confirmation_summary
cb_close_confirm cb_close_confirm
cb_close cb_close
cb_close_unprocessed cb_close_unprocessed
cb_save cb_save
cb_delete cb_delete
dw_confirmation dw_confirmation
dw_claimcost_unapplied_claim_txn dw_claimcost_unapplied_claim_txn
cb_refresh cb_refresh
cb_unit_of_work_save cb_unit_of_work_save
cb_cancel cb_cancel
cb_confirm cb_confirm
dw_transaction_list dw_transaction_list
end type
global w_claim_cost_maintenance w_claim_cost_maintenance

type variables
boolean lb_already_clicked
boolean ib_filter_applied
boolean ib_oldest_to_newest

			
decimal {2} idcm_cheque_amount

U_DW_DOCUMENT_PATH iu_dw_document_path
M_DW_ONLINE_RMB_POPUP im_popup

string is_window_name
string is_filter_parm

long il_txn_unit_of_work, il_benefit_calculation_no

datawindowchild 	idwc_payment_sub_type_code_fr
datawindowchild 	idwc_payment_sub_type_code_to
datawindowchild	idwc_posting_period
		
windowstate		iws_frame_open_state


/* BUSINESS RULE OBJECT */	
n_claim_cost_maintenance	inv_ccm

u_ds ids_unapplied_claim_txn		
u_ds ids_payment												
u_ds ids_cost_of_claims_allocated						
u_ds ids_adjustment_txn_work_table						
u_ds ids_last_txn_unit_of_work_no		
u_ds ids_last_coc_allocated_no
u_ds ids_last_claim_txn_no								
u_ds ids_last_payment_no
u_ds ids_rehab_task_update
u_ds ids_txn_unit_of_work_no	
u_ds ids_rehab_task_authorization
u_ds ids_rehab_task_authorization_update		
u_ds ids_rehab_task_authorization_to_update_sum 
u_ds ids_rehab_task		
u_ds ids_rehab_task_authorization_insert
u_ds ids_work_group_txn_sub_type	
u_ds ids_unapplied_txn_unit_of_work
u_ds ids_interim_rehab_auth_xfer_event  
u_ds ids_rehab_invoice_line_item
u_ds ids_rehab_invoice_line_item_insert
u_ds ids_rehab_task_authorization_to_side
u_ds ids_last_rehab_invoice_no	
u_ds ids_rehab_invoice_insert

end variables

forward prototypes
public function integer wf_check_for_unprocessed (long al_txn_no)
public subroutine wf_set_amt_indicator (boolean ab_visible, ref string as_txn_type_code, ref string as_txn_sub_type_code)
public function integer wf_assign_cost_alloc_type_code (long al_cost_alloc_no_to, ref string as_cost_alloc_type_code_to)
public function string wf_get_maintain_zerod_allowed_flag (string as_txn_type_code, string as_txn_sub_type_code)
public function integer wf_determine_rehab_task_authorization (long al_claim_no, long al_authorization_no, ref long al_authorized_quantity, ref long al_paid_quantity)
public subroutine wf_determine_security (ref string as_inc_receiving_salary_flag, ref string as_inc_self_insured_flag, string as_txn_type_code, string as_txn_sub_type_code)
public function long wf_assign_unit_of_work (ref boolean ab_add_new, long al_add_txn_unit_of_work, string as_txn_type_code, string as_txn_sub_type_code)
public subroutine wf_set_defaults ()
public function integer wf_determine_cost_allocation (long al_original_related_txn_no, ref long al_cost_alloc_no, ref long al_cost_alloc_operation_no, ref string as_cost_alloc_type_code)
public subroutine wf_create_destroy_datastore (string as_mode)
public function integer wf_reset_parm_screen ()
public function integer wf_assign_admin_region (long al_claim_no, ref string as_admin_region_code)
public subroutine wf_set_parameter_screen (string as_txn_type_code, string as_txn_sub_type_code)
public function integer wf_save ()
public function integer wf_confirm ()
protected subroutine wf_calculate_remaining_hours_lost (long al_ultimate_txn_no, ref decimal adec_remaining_ult_hours_lost)
public subroutine wf_calculate_remaining_days_lost (long al_ultimate_txn_no, ref decimal adec_remaining_ult_days_lost)
private subroutine wf_calculate_remaining_ult_txn_amount (long al_ultimate_txn_no, ref decimal adec_remaining_ult_txn_amount)
private subroutine wf_calculate_remaining_payment_amount (long al_payment_no, ref decimal adec_remaining_payment_amount)
public function integer wf_calc_net_other_ult_txn_amount (long al_payment_no, long al_ultimate_txn_no, ref decimal adcm_net_other_ult_txn_amount)
public function integer wf_calc_net_other_adj_ult_days_lost (long al_payment_no, long al_ultimate_txn_no, ref decimal adcm_net_other_ult_days_lost)
public function integer wf_calc_net_other_adj_ult_hours_lost (long al_payment_no, long al_ultimate_txn_no, ref decimal adcm_net_other_ult_hours_lost)
public function integer wf_update_rehab_authorization (string as_txn_type_code, string as_txn_sub_type_code)
public subroutine wf_update_rehab_invoice_line_item_amend (string as_txn_type_code, string as_txn_sub_type_code, long al_payment_no)
public subroutine wf_insert_invoice_line_item (long al_new_payment_no, long al_payment_no_from, long al_claim_no_to, long al_authorization_no_to, long al_claim_no_from, ref long al_last_rehab_invoice_no)
end prototypes

event ue_load_module_security();datawindowchild ldwc_txn_type_code

string 	ls_script = 'ue_load_module_security'
string		ls_txn_type_code
string   	ls_find

long     	ll_found
long 		ll_rc
long		ll_rows
long		ll_row

ids_work_group_txn_sub_type	= CREATE u_ds 

/* create all datastores need to save the database entries */
ids_work_group_txn_sub_type.dataobject = 'ds_work_group_txn_sub_type'
ids_work_group_txn_sub_type.SetTransObject(SQLCA)

/* based on the work group code from User Profile - we can determine which txn type the user can maintain */
ll_rc = ids_work_group_txn_sub_type.retrieve(vgst_user_profile.work_group_code)

ll_rc = dw_parameter.GetChild("txn_type_code",ldwc_txn_type_code)
ldwc_txn_type_code.SetTransObject(SQLCA)

ll_rows = ldwc_txn_type_code.Retrieve()
SQLCA.nf_handle_error("ldwc_txn_type_code.Retrieve()",is_window_name,ls_script)


/* Filter THE TXN TYPE IF USER GROUP DOES NOT ALLOW FOR IT */
for ll_row = 1 to ldwc_txn_type_code.rowcount()
	
	ls_txn_type_code = ldwc_txn_type_code.getitemstring(ll_row,'txn_type_code')

	ls_find = "txn_type_code = '"  + ls_txn_type_code + "'"
	ll_found = ids_work_group_txn_sub_type.Find(ls_find,1,ids_work_group_txn_sub_type.rowcount())
	
	if ll_found <= 0 then
		ldwc_txn_type_code.deleterow(ll_row)
		ll_row = ll_row - 1
	end if
	
next

end event

event ue_print();STRING		ls_visible

ls_visible = dw_parameter.Describe("unprocessed.Visible")

IF cb_confirm.Visible = TRUE THEN
	dw_transaction_list.TriggerEvent('ue_print')
ELSEIF cb_save.Visible = TRUE THEN
	dw_confirmation.TriggerEvent('ue_print')
ELSEIF ls_visible = '1' THEN
	dw_claimcost_unapplied_claim_txn.TriggerEvent('ue_print')
END IF

end event

public function integer wf_check_for_unprocessed (long al_txn_no);/* 	wf_check_for_unprocessed()
	
		This functions checks to see if any unprocessed txn existing against the source 
		
		
		arg: 
		
		al_txn_no - the source txn being adjusted/transfered
		
		
		return:
		
		0 - sucessfull, -1 unsucessfull
		
*/

long ll_count
string ls_script = "wf_check_for_unprocessed()"


select count(*) 
into :ll_count
from UNAPPLIED_CLAIM_TXN
where related_txn_no = :al_txn_no
using SQLCA; 
IF SQLCA.nf_handle_error("select from UNAPPLIED_CLAIM_TXN",is_window_name,ls_script) = 100 THEN
	messagebox("Warning","Txn no " + STRING(al_txn_no) + " has unprocessed txn against - must unselect ",StopSign!)
	return -1
end if


return ll_count
		
end function

public subroutine wf_set_amt_indicator (boolean ab_visible, ref string as_txn_type_code, ref string as_txn_sub_type_code);/* disbale or enable to pct/amt/total amt functionality */
if (as_txn_type_code = 'J' AND	(as_txn_sub_type_code = '3' OR as_txn_sub_type_code = '5')) or	(as_txn_type_code = 'T') then
	ab_visible = true
else
	ab_visible = false
end if

if ab_visible then
	rb_pct.visible						= true
	rb_amount.visible					= true
	rb_total_amt.visible				= true
	gb_pct.visible						= true
else
	cbx_oldest_to_newest.visible 	= false
	cb_apply.visible 					= false
	em_pct.visible 					= false
	em_amount.visible					= false
	rb_pct.visible						= false
	rb_amount.visible					= false
	rb_total_amt.visible				= false
	gb_pct.visible						= false
	cb_cancel_pct.visible			= false
end if

rb_pct.CHECKed 						= FALSE
rb_amount.checked 					= false
rb_total_amt.checked 				= false
gb_pct.BringToTop 					= TRUE
cbx_oldest_to_newest.BringToTop 	= TRUE
cb_apply.BringToTop 					= TRUE
cb_cancel_pct.BringToTop 			= TRUE
cb_cancel_pct.BringToTop 			= TRUE
em_pct.BringToTop 					= TRUE
em_amount.BringToTop 				= TRUE
rb_pct.BringToTop 					= TRUE
rb_amount.BringToTop 				= TRUE
rb_total_amt.BringToTop 			= TRUE
end subroutine

public function integer wf_assign_cost_alloc_type_code (long al_cost_alloc_no_to, ref string as_cost_alloc_type_code_to);/* wf_assign_cost_alloc_type_code()

This function will assign the cost allocation type code 
for the cost allocation no being passed. The employer type code is
determined from the EMPLOYER.employer_type_code

arg: 	al_cost_alloc_no_to - cost allocation no (employer no) where the cost being transfered to
		as_cost_allocation_type_code_to - employer type code for the cost allocation being passed


return  0 - found, -1 not found
*/

string ls_script = "wf_assign_cost_alloc_type_code"

/* assign to cost allocation type */
SELECT dbo.EMPLOYER.employer_type_code  
INTO :as_cost_alloc_type_code_to 
FROM dbo.EMPLOYER  
WHERE dbo.EMPLOYER.employer_no = :al_cost_alloc_no_to  ;
if SQLCA.nf_handle_error("SELECT FROM EMPLOYER ",is_window_name,ls_script) = 100 THEN
	MESSAGEBOX("Warning","Problem assigning cost allcation type for employer no " + string(al_cost_alloc_no_to))
	RETURN -1
END IF

return 0
end function

public function string wf_get_maintain_zerod_allowed_flag (string as_txn_type_code, string as_txn_sub_type_code);string ls_maintain_zero_allowed_flag
STRING ls_script = "WF_GET_MAINTAIN_ZEROED_ALLOWED_FLAG()"


SELECT maintain_zero_allowed_flag
into :ls_maintain_zero_allowed_flag
FROM Txn_Type_Combination
WHERE txn_type_code = :as_txn_type_code
AND txn_sub_type_code = :as_txn_sub_type_code
using SQLCA;

if SQLCA.nf_handle_error("select FROM Txn_Type_Combination",is_window_name,ls_script ) = 100 then
	MESSAGEBOX("Error","Can find Txn Type Combination")
	RETURN "N"
end if


RETURN ls_maintain_zero_allowed_flag
end function

public function integer wf_determine_rehab_task_authorization (long al_claim_no, long al_authorization_no, ref long al_authorized_quantity, ref long al_paid_quantity);string ls_script = "wf_determine_rehab_task_authorization()"		
			



select  	authorized_quantity,
			paid_quantity 
into  	:al_authorized_quantity,
			:al_paid_quantity
from REHAB_TASK_AUTHORIZATION   
where claim_no 		= :al_claim_no
and authorization_no = :al_authorization_no
using sqlca;
if	SQLCA.nf_handle_error("SELECT FROM REHAB_TASK_AUTHORIZATION ",is_window_name,ls_script) = 100 then
	MESSAGEBOX("Warning","Cannot find any REHAB TASK AUTHORIZATION for this claim no",StopSign!)
	return -1
end if




return 0
			
			

	
end function

public subroutine wf_determine_security (ref string as_inc_receiving_salary_flag, ref string as_inc_self_insured_flag, string as_txn_type_code, string as_txn_sub_type_code);string   ls_find
long	   ll_found
string   ls_wg_receiving_salary_flag 
string	ls_wg_non_receiving_salary_flag 
string   ls_wg_self_insured_flag
string   ls_wg_non_self_insured_flag 


/* table driven - possible combinations */


//	receiving_salary_flag non_receiving_salary_flag    > value
//	--------------------- ------------------------- 

//	Y                     Y										>  1    do not exlcude any
//	N                     N										>  4	  REMOVE ALL
//	N                     Y										>  2    REMOVE RECEVING SALARY ONLY
//	Y                     N										>  3    REMOVE NON RECEIVING SALARY



//	self_insured_flag 	 non_selef_insurted_flag      >  value
//	--------------------- ------------------------- 
//	Y                     Y										>  1    do not exlcude any
//	N                     N										>  4	  REMOVE ALL
//	N                     Y										>  2    REMOVE self insured ONLY
//	Y                     N										>  3    REMOVE NON self insured




/* need to find out if txn should be displayed based on receiving salary and self insured flag */ 
ls_find = "txn_type_code = '"  + as_txn_type_code + "' and txn_sub_type_code = '" + as_txn_sub_type_code + "'"
ll_found = ids_work_group_txn_sub_type.Find(ls_find,1,ids_work_group_txn_sub_type.rowcount())

if ll_found > 0 then
	ls_wg_receiving_salary_flag  		= ids_work_group_txn_sub_type.getitemstring(ll_found,'receiving_salary_flag')
 	ls_wg_non_receiving_salary_flag  = ids_work_group_txn_sub_type.getitemstring(ll_found,'non_receiving_salary_flag')
	ls_wg_self_insured_flag   			= ids_work_group_txn_sub_type.getitemstring(ll_found,'self_insured_flag')
	ls_wg_non_self_insured_flag 		= ids_work_group_txn_sub_type.getitemstring(ll_found,'non_self_insured_flag')
end if
	

if ls_wg_receiving_salary_flag = 'Y' AND ls_wg_non_receiving_salary_flag = 'Y' THEN
	as_inc_receiving_salary_flag = '1'
ELSEif ls_wg_receiving_salary_flag = 'N' AND ls_wg_non_receiving_salary_flag = 'Y' THEN
	as_inc_receiving_salary_flag = '2'
ELSEif ls_wg_receiving_salary_flag = 'Y' AND ls_wg_non_receiving_salary_flag = 'N' THEN
	as_inc_receiving_salary_flag = '3'
ELSE
	as_inc_receiving_salary_flag = '4'
END IF
	
if ls_wg_self_insured_flag   = 'Y' AND ls_wg_non_self_insured_flag   = 'Y' THEN
	as_inc_self_insured_flag   = '1'
ELSEif ls_wg_self_insured_flag   = 'N' AND ls_wg_non_self_insured_flag   = 'Y' THEN
	as_inc_self_insured_flag   = '2'
ELSEif ls_wg_self_insured_flag   = 'Y' AND ls_wg_non_self_insured_flag   = 'N' THEN
	as_inc_self_insured_flag   = '3'
ELSE
	as_inc_self_insured_flag  = '4'
END IF
	

end subroutine

public function long wf_assign_unit_of_work (ref boolean ab_add_new, long al_add_txn_unit_of_work, string as_txn_type_code, string as_txn_sub_type_code);/* wf_assign_unit_of_work()

args:		ab_add_bew (ref)  - used to only prompt the screen once if user wants to append to a unit of work 
			al_add_txn_unit_of_work - refers to check box on parm screen if user always wants to add new unit of work (default to yes)

			as_txn_type_code		- the txn type - used to see if OTHERS exist in UNAPPLIED
			as_txn_sub_type_code	- the txn SUB type - used to see if OTHERS exist in UNAPPLIED

return   ll_txn_unit_of_work_no  >>  zero if new one is to be added or the txn unit of work user picks
*/

long 		ll_txn_unit_of_work_no
long 		ll_no_of_txn

string 	ls_script = "wf_assign_unit_of_work()"

s_window_message lstr_window_message



/* assign txn unit of work no - if check box is set allways assign new one*/	
/* check to see if pop-up window is required ?*/
if ab_add_new or al_add_txn_unit_of_work <> 0 then
else
	
	/* CHECK FOR SIMILAR TXN TYPES IN UNAPPLIED */
	ll_no_of_txn = ids_unapplied_txn_unit_of_work.retrieve(as_txn_type_code,as_txn_sub_type_code,vgst_user_profile.work_group_code)
	SQLCA.nf_handle_error("ids_unapplied_txn_unit_of_work.retrieve()",is_window_name,ls_script)

	IF ll_no_of_txn > 0 then
		lstr_window_message.as_stringparm[1] = as_txn_type_code
		lstr_window_message.as_stringparm[2] = as_txn_sub_type_code
																	
		OpenWithParm(w_popup_add_txn_unit_work_no,lstr_window_message)
		/* GRAB THE TXN UNIT OF WORK FROM THE STRUCTURE */
		lstr_window_message = Message.PowerObjectParm
		ll_txn_unit_of_work_no = long(lstr_window_message.as_stringparm[3])
		/* set the boolean - only show pop up once per group of work*/
		ab_add_new = true
	end if
end if
	
return ll_txn_unit_of_work_no
	
end function

public subroutine wf_set_defaults ();
dw_parameter.enabled 							= false
cb_search.enabled 								= false
cb_find.enabled									= false
cb_confirm.visible 								     = false
st_splitbar_1.visible							     = true
cb_clear.enabled									= false
cb_close.visible 									= false
dw_confirm_summary_totals.visible 			= true
dw_confirmation.visible 						    = true
dw_transaction_list.visible 					    = false
dw_claimcost_unapplied_claim_txn.visible 	= false
cb_cancel.visible 								    = true
cb_save.visible 								 	    = true
cb_save.enabled 									= true
cb_close_confirm.visible 						    = true
cb_cancel.enabled 								= TRUE
cbx_1.enabled 										= false
cb_apply.enabled									= false
uo_filter.visible 								    = false
dw_confirmation.GroupCalc()
end subroutine

public function integer wf_determine_cost_allocation (long al_original_related_txn_no, ref long al_cost_alloc_no, ref long al_cost_alloc_operation_no, ref string as_cost_alloc_type_code);/* GET COST ALLOCATION INFO FOR TXN NO PASSED */

STRING ls_script = 'wf_determine_cost_allocation()'


SELECT dbo.COST_OF_CLAIMS_ALLOCATED.cost_alloc_no,   
		dbo.COST_OF_CLAIMS_ALLOCATED.cost_alloc_operation_no,   
		dbo.COST_OF_CLAIMS_ALLOCATED.cost_alloc_type_code  
 INTO :al_cost_alloc_no,
		:al_cost_alloc_operation_no,
		:as_cost_alloc_type_code
 FROM dbo.COST_OF_CLAIMS_ALLOCATED  
WHERE dbo.COST_OF_CLAIMS_ALLOCATED.txn_no = :al_original_related_txn_no   
using SQLCA;
if SQLCA.nf_handle_error("select dbo.COST_OF_CLAIMS_ALLOCATED ",is_window_name,ls_script ) = 100 then
	return -1
end if

RETURN 1
end function

public subroutine wf_create_destroy_datastore (string as_mode);
if as_mode = 'CREATE' THEN

	ids_unapplied_txn_unit_of_work 					= CREATE u_ds	
	ids_unapplied_claim_txn								= CREATE u_ds 
	ids_payment											= CREATE u_ds 
	ids_cost_of_claims_allocated						= CREATE u_ds 
	ids_adjustment_txn_work_table				    = CREATE u_ds 
	ids_last_txn_unit_of_work_no						= CREATE u_ds 
	ids_last_claim_txn_no								    = CREATE u_ds 
	ids_last_payment_no									= CREATE u_ds 
	ids_txn_unit_of_work_no								= CREATE u_ds
	ids_last_coc_allocated_no							= create u_ds
	ids_rehab_task_authorization						= CREATE u_ds
	ids_rehab_task_authorization_to_update_sum 	= CREATE u_ds 
	ids_rehab_task_authorization_update				= CREATE u_ds
	ids_rehab_task											= CREATE u_ds
	ids_rehab_invoice_insert								= CREATE u_ds
	ids_rehab_task_authorization_insert				= create u_ds
	ids_rehab_task_update								= CREATE u_ds
	ids_rehab_task_authorization_insert				= create u_ds
	ids_rehab_invoice_line_item  				         = create u_ds
     ids_rehab_invoice_line_item_insert		         = create u_ds
	ids_rehab_task_authorization_to_side             = create u_ds
     ids_last_rehab_invoice_no	=  create u_ds
	
	/* create all datastores need to save the database entries */
	
	ids_unapplied_txn_unit_of_work.dataobject = 'ds_unapplied_txn_unit_of_work'
	ids_unapplied_txn_unit_of_work.SetTransObject(SQLCA)

	ids_unapplied_claim_txn.dataobject = 'ds_unapplied_claim_txn'
	ids_unapplied_claim_txn.SetTransObject(SQLCA)
	
	ids_payment.dataobject = 'ds_payment'
	ids_payment.SetTransObject(SQLCA)
	
	ids_cost_of_claims_allocated.dataobject = 'ds_cost_of_claims_allocated'
	ids_cost_of_claims_allocated.SetTransObject(SQLCA)
	
	ids_adjustment_txn_work_table.dataobject = 'ds_adjustment_txn_work_table'
	ids_adjustment_txn_work_table.SetTransObject(SQLCA)
	
	ids_last_txn_unit_of_work_no.dataobject = 'ds_last_txn_unit_of_work_no'
	ids_last_txn_unit_of_work_no.SetTransObject(SQLCA)
	
	ids_last_claim_txn_no.dataobject = 'ds_last_claim_txn_no'
	ids_last_claim_txn_no.SetTransObject(SQLCA)
	
	ids_last_payment_no.dataobject = 'ds_last_payment_no'
	ids_last_payment_no.SetTransObject(SQLCA)
	
	
	ids_last_coc_allocated_no.dataobject = 'ds_last_coc_allocated_no'
	ids_last_coc_allocated_no.SetTransObject(SQLCA)
	
	ids_txn_unit_of_work_no.dataobject = 'ds_txn_unit_of_work_no'
	ids_txn_unit_of_work_no.SetTransObject(SQLCA)
	
	ids_rehab_task_authorization_update.dataobject = 'd_rehab_task_authorization_update'
	ids_rehab_task_authorization_to_update_sum.dataobject = 'd_rehab_task_authorization_to_update_sum'
	
	
	ids_rehab_task_authorization.dataobject = 'ds_rehab_task_authorization'
	ids_rehab_task_authorization.SetTransObject(SQLCA)
	
     ids_rehab_task_authorization_to_side.dataobject = 'ds_rehab_task_authorization'
	ids_rehab_task_authorization_to_side.SetTransObject(SQLCA)
	
	
	 ids_rehab_task_update.dataobject = 'ds_rehab_task_update'
     ids_rehab_task_update.SetTransObject(SQLCA)
	
	ids_rehab_task_authorization_insert.dataobject = 'ds_rehab_task_authorization'
	ids_rehab_task_authorization_insert.SetTransObject(SQLCA)
	
	ids_rehab_task.dataobject = 'ds_rehab_task'
	ids_rehab_task.SetTransObject(SQLCA)
			
			
	ids_rehab_invoice_insert.dataobject = 'ds_rehab_invoice_insert'
	ids_rehab_invoice_insert.SetTransObject(SQLCA)
			
			
			
		
	ids_rehab_invoice_line_item.dataobject = 'ds_rehab_invoice_line_item'
     ids_rehab_invoice_line_item.SetTransObject(SQLCA)
	  
	  	
	ids_rehab_invoice_line_item_insert.dataobject = 'ds_rehab_invoice_line_item_insert'
     ids_rehab_invoice_line_item_insert.SetTransObject(SQLCA)
	
	 ids_last_rehab_invoice_no.dataobject = 'ds_last_rehab_invoice_no'
	  ids_last_rehab_invoice_no.SetTransObject(SQLCA)
	
ELSEif as_mode = 'DESTROY' THEN
	
	if isvalid(ids_payment) then 											    destroy ids_payment
	if isvalid(ids_unapplied_claim_txn) then 							    destroy ids_unapplied_claim_txn
	if isvalid(ids_cost_of_claims_allocated) then  					    destroy ids_cost_of_claims_allocated
	if isvalid(ids_adjustment_txn_work_table) then 			        destroy ids_adjustment_txn_work_table
	if isvalid(ids_last_txn_unit_of_work_no) then 					    destroy ids_last_txn_unit_of_work_no
	if isvalid(ids_last_claim_txn_no) then                                     destroy ids_last_claim_txn_no
	if isvalid(ids_last_payment_no) then 								    destroy ids_last_payment_no
	if isvalid(ids_last_coc_allocated_no) then						        destroy ids_last_coc_allocated_no		
	if isvalid(ids_rehab_task_update) THEN						        destroy ids_rehab_task_update
	if isvalid(ids_txn_unit_of_work_no)	THEN						    destroy ids_txn_unit_of_work_no
	if isvalid(ids_rehab_task_authorization) THEN					    destroy ids_rehab_task_authorization	
	if isvalid(ids_rehab_task_authorization_to_update_sum) THEN destroy ids_rehab_task_authorization_to_update_sum
	if isvalid(ids_rehab_task_authorization_update)	THEN			    destroy ids_rehab_task_authorization_update
	if isvalid(ids_rehab_task)	THEN										    destroy ids_rehab_task
	if isvalid(ids_rehab_invoice_insert)	THEN		                           destroy ids_rehab_invoice_insert
	if isvalid(ids_rehab_task_authorization_insert) THEN			    destroy ids_rehab_task_authorization_insert
	if isvalid(ids_unapplied_txn_unit_of_work ) THEN				   destroy ids_unapplied_txn_unit_of_work
	if isvalid(ids_rehab_invoice_line_item) THEN				        destroy ids_rehab_invoice_line_item
	if isvalid(ids_rehab_invoice_line_item_insert) THEN	            destroy ids_rehab_invoice_line_item_insert
	if isvalid(ids_rehab_task_authorization_to_side) THEN	       destroy ids_rehab_task_authorization_to_side
     if isvalid(ids_last_rehab_invoice_no) THEN	                    destroy ids_last_rehab_invoice_no

end if


end subroutine

public function integer wf_reset_parm_screen ();long 		ll_rc
long 		ll_rows
long 		ll_posting_period

string 	ls_script
STRING	ls_rc  = "Z"


dw_transaction_list.uf_setselect(1)
dw_transaction_list.uf_SetSort(True)
dw_transaction_list.uf_SetFilter(True)


rb_pay_all_types.checked = true
/* drop down's used on the parameter screen */

DATAWINDOWCHILD ldwc_payment_type_code_fr
DATAWINDOWCHILD ldwc_payment_type_code_to

 
/* get the cost of claims period */
ll_rc = dw_parameter.GetChild("posting_period",idwc_posting_period)
idwc_posting_period.SetTransObject(SQLCA)
ll_rows = idwc_posting_period.Retrieve()
SQLCA.nf_handle_error("lldwc_posting_period.Retrieve()",is_window_name,ls_script)

if ll_rows > 0 then
	ll_posting_period = idwc_posting_period.getitemnumber(ll_rows,'current_coc_period')
	dw_parameter.setitem(1,'posting_period',ll_posting_period)
elseif ll_rows = 0 then
	messagebox("Warning","The current coc period has not been created - Please call IT Help Desk")
	RETURN -1
end if



/* populate the valid payment type's FR SIDE*/
ll_rc = dw_parameter.GetChild("payment_type_code_fr",ldwc_payment_type_code_fr)
ldwc_payment_type_code_fr.SetTransObject(SQLCA)
ll_rows = ldwc_payment_type_code_fr.Retrieve()
SQLCA.nf_handle_error("ldwc_payment_type_code.Retrieve()",is_window_name,ls_script)


/* populate the valid payment type's TO SIDE*/
ll_rc = dw_parameter.GetChild("payment_type_code_to",ldwc_payment_type_code_to)
ldwc_payment_type_code_to.SetTransObject(SQLCA)
ll_rows = ldwc_payment_type_code_to.Retrieve()
SQLCA.nf_handle_error("ldwc_payment_type_code.Retrieve()",is_window_name,ls_script)


/* populate the valid payment SUB type's FROM SIDE*/
ll_rc = dw_parameter.GetChild("payment_sub_type_code_fr",idwc_payment_sub_type_code_fr)
idwc_payment_sub_type_code_fr.SetTransObject(SQLCA)

/* populate the valid payment SUB type's TO SIDE*/
ll_rc = dw_parameter.GetChild("payment_sub_type_code_to",idwc_payment_sub_type_code_to)
idwc_payment_sub_type_code_to.SetTransObject(SQLCA)

this.title = "Claim Cost Maintenance"


wf_set_amt_indicator(false,ls_rc ,ls_rc )


cbx_1.checked = false

RETURN 1
end function

public function integer wf_assign_admin_region (long al_claim_no, ref string as_admin_region_code);/* WF_ASSIGN_ADMIN_REGION() 

This function asssign the admin region of the claim no being passed

ARG: 	al_claim_no - claim no 
		as_admin_region_code - the admin region for the claim no being passed

return 1 - found,  -1 not found

*/

string ls_script = "wf_assign_admin_region()"

SELECT dbo.CLAIM.admin_region_code  
INTO :as_admin_region_code  
FROM dbo.CLAIM  
WHERE dbo.CLAIM.claim_no = :al_claim_no
using SQLCA;
if SQLCA.nf_handle_error("select from CLAIM",is_window_name,ls_script) = 100 then
	messagebox("Warning", "Claim No " + string(al_claim_no) + ' has no admin region ' )
	return -1
end if


return 1
end function

public subroutine wf_set_parameter_screen (string as_txn_type_code, string as_txn_sub_type_code);/* wf_set_paramter_screen()

	set default values in the paramter screen 
	
	arg:
	
	as_txn_type_code   			- txn type
	as_txn_sub_type_code			- txn sub type
*/

string 	ls_explanation
long 		ll_default_cost_alloc_no
long 		ll_default_cost_alloc_operation_no
date		ldt_null_date

/* set to the standard default screen */

setnull(ldt_null_date)

/* claim no */
dw_parameter.Object.claim_no.visible=1
dw_parameter.Object.claim_no_t.visible=1

/* claim no to */
dw_parameter.Object.t_claim_no_to.visible=0
dw_parameter.Object.claim_no_to.visible=0

/*provider no*/
dw_parameter.Object.t_provider.Visible = 0
dw_parameter.Object.provider_no.Visible = 0

/*provider type*/
dw_parameter.Object.t_provider_type.Visible = 0
dw_parameter.Object.provider_type_code.Visible = 0

/* cheque no */
dw_parameter.Object.cheque_no.visible=0
dw_parameter.Object.t_cheque_no.visible=0

/* xmit no */
dw_parameter.Object.xmit_no.visible=0
dw_parameter.Object.t_xmit_no.visible=0

/* cost relief date */
dw_parameter.Object.cost_relief_date.visible=0
dw_parameter.Object.t_cost_relief_date.visible=0
			
/* payment type from */
dw_parameter.Object.t_payment_type_fr.visible=0
dw_parameter.Object.payment_type_code_fr.visible=0
			

/* payment type to */
dw_parameter.Object.t_payment_type_to.visible=0
dw_parameter.Object.payment_type_code_to.visible=0
			
/* payment sub type fr */
dw_parameter.Object.t_payment_type_fr.visible=0
dw_parameter.Object.payment_sub_type_code_fr.visible=0
dw_parameter.Object.t_payment_sub_type_fr.visible=0		

/* individual name for claim to */
dw_parameter.Object.individual_name.visible=0

/* payment sub type to */
dw_parameter.Object.payment_sub_type_code_to.visible=0
dw_parameter.Object.t_payment_type_to.visible=0		
dw_parameter.Object.t_payment_sub_type_to.visible=0		
	
/* cost allocation to */
dw_parameter.Object.t_cost_alloc_to.visible=0	
dw_parameter.Object.cost_alloc_no_to.visible=0

/* cost allocation fr */
dw_parameter.Object.cost_alloc_no_fr.visible=0
dw_parameter.Object.t_cost_alloc_fr.visible=0
						
/* cost allocation operation to */						
dw_parameter.Object.cost_alloc_operation_no_to.visible=0

/* cost allocation operation from */
dw_parameter.Object.cost_alloc_operation_no_fr.visible=0

/* issue date */	
dw_parameter.Object.t_issue_date.visible=0
dw_parameter.Object.issue_date.visible=0

/* add new unit of work */
dw_parameter.Object.add_txn_unit_of_work.visible=1

/* amount */
dw_parameter.Object.amount.visible=1
dw_parameter.Object.t_amount.visible=1

rb_pay_all_types.checked = true

dw_parameter.Object.benefit_type.visible=0
dw_parameter.SetItem(1,'benefit_type','')
dw_parameter.Object.t_overpaid.visible=0


/*authorization no to*/
dw_parameter.Object.t_authorization_no_to.visible=0
dw_parameter.Object.authorization_no_to.visible=0




choose case as_txn_type_code
	case "T"
		
		choose case as_txn_sub_type_code
			case "6"   	/* claim transfer */
				/* claim no to */
				dw_parameter.Object.t_claim_no_to.visible=1
				dw_parameter.Object.claim_no_to.visible=1
				dw_parameter.Object.individual_name.visible=1
				dw_parameter.Object.t_authorization_no_to.visible=1
                  dw_parameter.Object.authorization_no_to.visible=1			
			case "7"		/* cost allocation transfer */
				dw_parameter.Object.cost_alloc_no_to.visible=1
				dw_parameter.Object.cost_alloc_no_fr.visible=1
				dw_parameter.Object.cost_alloc_operation_no_to.visible=1
				dw_parameter.Object.cost_alloc_operation_no_fr.visible=1
				dw_parameter.Object.t_cost_alloc_fr.visible=1
				dw_parameter.Object.t_cost_alloc_to.visible=1
			case "8" 	/* cost relief transfer */
				dw_parameter.Object.cost_alloc_no_to.visible=1
				dw_parameter.Object.cost_alloc_no_fr.visible=1
				dw_parameter.Object.cost_alloc_operation_no_to.visible=1
				dw_parameter.Object.cost_alloc_operation_no_fr.visible=1
				dw_parameter.Object.t_cost_alloc_fr.visible=1
				dw_parameter.Object.t_cost_alloc_to.visible=1
				dw_parameter.Object.cost_relief_date.visible=1
				dw_parameter.Object.t_cost_relief_date.visible=1
			case "9" 	/* payment type transfer */
				dw_parameter.Object.payment_sub_type_code_fr.visible=1
				dw_parameter.Object.payment_sub_type_code_to.visible=1
				dw_parameter.Object.payment_type_code_fr.visible=1
				dw_parameter.Object.payment_type_code_to.visible=1
				dw_parameter.Object.t_payment_type_fr.visible=1
				dw_parameter.Object.t_payment_type_to.visible=1
				dw_parameter.Object.t_payment_sub_type_fr.visible=1
				dw_parameter.Object.t_payment_sub_type_to.visible=1
		end choose
				
				
case "J"
	
		choose case as_txn_sub_type_code
			case "2" /* cancel cheque/deposit */
				/* cheque no */
				dw_parameter.Object.cheque_no.visible=1
				dw_parameter.Object.t_cheque_no.visible=1
				
				/* xmit no */
				dw_parameter.Object.xmit_no.visible=1
				dw_parameter.Object.t_xmit_no.visible=1
	
				/* issue date */	
				dw_parameter.Object.t_issue_date.visible=1
				dw_parameter.Object.issue_date.visible=1
				
				/*provider no*/
				dw_parameter.Object.t_provider.Visible = 1
				dw_parameter.Object.provider_no.Visible = 1
				
				/*provider type*/
				dw_parameter.Object.t_provider_type.Visible = 1
				dw_parameter.Object.provider_type_code.Visible = 1

			case "3" /* over payment */
				
				dw_parameter.Object.benefit_type.visible=1
				dw_parameter.Object.t_overpaid.visible=1

			CASE "5" /* third party recovery */
				

			case "4"	/* tax correction*/

				/* amount */
				dw_parameter.Object.amount.visible=0
				dw_parameter.Object.t_amount.visible=0


			case "Q" /* rehab qty correction */

				/* amount */
				dw_parameter.Object.amount.visible=0
				dw_parameter.Object.t_amount.visible=0
			
		end choose

end choose


dw_parameter.setitem(1,'payment_sub_type_code_fr',"")
dw_parameter.setitem(1,'payment_sub_type_code_to',"")
dw_parameter.setitem(1,'payment_type_code_fr',"")
dw_parameter.setitem(1,'payment_type_code_to',"")
dw_parameter.setitem(1,'cost_alloc_no_fr',0)
dw_parameter.setitem(1,'cost_alloc_operation_no_fr',0)
dw_parameter.setitem(1,'cost_alloc_no_to',0)
dw_parameter.setitem(1,'cost_alloc_operation_no_to',0)
dw_parameter.setitem(1,'amount',0.00)
dw_parameter.setitem(1,'claim_no',0)
dw_parameter.setitem(1,'cheque_no',0)
dw_parameter.setitem(1,'xmit_no',0)
dw_parameter.setitem(1,'claim_no_to',0)
dw_parameter.SetItem(1,'provider_no',0)
dw_parameter.setitem(1,'individual_name',"")
dw_parameter.setitem(1,'explanation',"")
dw_parameter.SetItem(1,'cost_relief_date',ldt_null_date)
dw_parameter.SetItem(1,'issue_date',ldt_null_date)

if as_txn_sub_type_code = '3' then
	dw_parameter.setcolumn("benefit_type")
else
	dw_parameter.setcolumn("claim_no")
end if

THIS.title = "Claim Cost Maintenance"
cbx_1.checked = false


end subroutine

public function integer wf_save ();pointer oldpointer // Declares a pointer 

decimal {2} ldcm_preacc_work_hours_per_day 

boolean 		lb_update_last_unit_of_work

date 			ldt_today

datetime		ldtm_cheque_deposit_date 
datetime		ldtm_paid_from_date   
datetime		ldtm_paid_to_date     
datetime		ldtm_authorized_date   
datetime		ldtm_processed_date
long ll_authorization_no_to

decimal {2} ldcm_tax_rate
decimal {2} ldcm_adjustment_hours_lost

decimal {2} ldcm_txn_amount
decimal {2} ldcm_total_payment_amount
decimal {5} ldcm_tax_amount
decimal {2} ldcm_cost_amount

integer 	li_rc 

long 		ll_rows_in_dw
long		ll_row
long 		ll_txn_no
long 		ll_payment_no
long 		ll_txn_unit_of_work_no
long 		ll_cost_alloc_no
long 		ll_cost_alloc_operation_no
long 		ll_insert_row
long 		ll_posting_period
long 		ll_claim_no
long 		ll_related_txn_no
long 		ll_rehab_qty
long		ll_manual_cheque_req_no 
long		ll_cheque_no  
long		ll_direct_deposit_xmit_no
long 		ll_recipient_no				
long 		ll_last_txn_no
long 		ll_last_authorization_no
long 		ll_last_payment_no
long 		ll_new_txn_cnt
long 		ll_new_payment_cnt
long 		ll_last_txn_unit_of_work_no
long		ll_opening_no   
long		ll_benefit_calculation_no   
long		ll_award_no  
long		ll_insert_row_payment
long		ll_last_coc_allocated_no
long		ll_new_coc_allocated_no
long 		ll_insert_row_coc
long		ll_paid_quantity   
long		ll_authorization_no 
long		ll_coc_allocated_no
long		ll_insert_row_work
long		ll_rc
long		ll_auth_row
long 		ll_org_paid_quantity_adjust
long		ll_rehab_task_row
long		ll_org_paid_quantity
long		ll_authorized_quantity
decimal {2} 		ldcm_adjustment_days_lost
long 		ll_found
long		ll_adjustment_rehab_qty
long 		ll_adjustment_rehab_qty_sum
long		ll_claim_no_from
long 		ll_prev_authorization_no
long 		ll_task_no
long 		ll_provider_no_fr
long 		ll_provider_no_to
long 		ll_task_no_to
long 		ll_task_no_from
long 		ll_provider_no_original
long 		ll_ins_unit_of_work_row
long		ll_org_no
long		ll_insert_row_rehab_task 
long		ll_authorization_from_no
long		ll_new_authorization_no_cnt
long		ll_ins_row_rehab_task
long		ll_max_task_no_for_claim
long		ll_ins_row_rehab_task_auth
long		ll_ins_claim_event
long		ll_max_claim_event_no
long		ll_ins_progress_note_event
long		ll_event_no 
string 	ls_task_type_code
string 	ls_task_sub_type_code
string 	ls_task_specific_code
string 	ls_new_txn_created
string	    ls_annuity_set_a_side
string 	ls_search
string 	ls_payment_type_code
string 	ls_payment_sub_type_code
string 	ls_txn_type_code
string 	ls_txn_sub_type_code
string 	ls_expanation 
string 	ls_maintain_allowed_flag
string  	ls_recipient_sub_type_code 
string	    ls_recipient_name 
string	    ls_address_line1 
string	    ls_address_line2
string	    ls_city 
string	    ls_prov_state_code
string	    ls_country 
string	    ls_postal_code
string	    ls_use_default_address_flag 
string	    ls_cheque_print_group_code
string     ls_script = "cb_save - clicked"
string	    ls_cost_alloc_type_code
string	    ls_final_payment_flag   
string	    ls_authorized_by_code   
string	    ls_payment_adjustment_flag   
string	    ls_loe_explanation  
string	    ls_recipient_type_code  
string	    ls_payment_method_code
string	    ls_admin_region_code
string 	ls_find
string 	ls_amount_required_flag
string 	ls_provider_type_code
string 	ls_responsible_user_id
string 	ls_comment
string 	ls_task_success_code
long	 	ll_provider_no
long		ll_prev_payment_no	
long       ll_prev_txn_no
long ll_claim_no_to
long ll_last_rehab_invoice_no 
long  ll_claim_no_fr	
long ll_org_payment_no



/* {The save  is called when the user confirms the confirmation page that it's okay to continue} */
ldt_today = date(f_server_datetime())

SetPointer(Hourglass!)


/* ENABLED THE SAVE BUTTON TO Prevent DOUBLE PROCESSING*/
cb_save.enabled = false

oldpointer = SetPointer(HourGlass!)

/* run through business rules again - after confirm screen */
TRY
	inv_ccm.of_check_business_rules()
CATCH (uo_br_exception luo_br_exception)
	if luo_br_exception.GetMessage() <> '' Then
		MessageBox('Error occured',luo_br_exception.GetMessage())
	End iF
	RETURN -1
END TRY

ll_claim_no_to							     = dw_parameter.GetItemNumber(1,'claim_no_to')


/* how mnay rows are in the confirmation dw */
ll_rows_in_dw = dw_confirmation.rowcount()

/* check to see if any txn need to be created - see if the txn is zero or already assigned */
if ll_rows_in_dw = 0 then 	
	messagebox("Warning","No rows to save")
	return 0
end if


/* the last # tables are read */
li_rc = ids_last_claim_txn_no.retrieve()
SQLCA.nf_handle_error("ids_last_claim_txn_no.retrieve()",is_window_name,ls_script)
ll_last_txn_no = ids_last_claim_txn_no.getitemnumber(1,'last_txn_no')

li_rc = ids_last_payment_no.retrieve()
SQLCA.nf_handle_error("ids_last_payment_no.retrieve()",is_window_name,ls_script)
ll_last_payment_no = ids_last_payment_no.getitemnumber(1,'last_payment_no')

li_rc = ids_last_coc_allocated_no.retrieve()
SQLCA.nf_handle_error("ids_last_coc_allocated_no.retrieve()",is_window_name,ls_script)
ll_last_coc_allocated_no = ids_last_coc_allocated_no.getitemnumber(1,'last_coc_allocated_no')

li_rc = ids_last_rehab_invoice_no.retrieve()
ll_last_rehab_invoice_no = ids_last_rehab_invoice_no.getitemnumber(1,'last_rehab_invoice_no')



/* get the authorization to from the screen - set t zero if not entered */
ll_authorization_no_to = dw_parameter.GetItemNumber(1,'authorization_no_to')
if isnull(ll_authorization_no_to) then ll_authorization_no_to = 0

ll_claim_no_fr								= dw_parameter.getitemnumber(1,'claim_no')


/* var used in processing */
lb_update_last_unit_of_work = false
ll_prev_payment_no	=			-1
ll_prev_txn_no			=			-1



SQLCA.nf_begin_transaction()

/* LOOP THROUGH EXTERNAL DW AND CREATE THE NEW TXN AND ASSOCIATED ENTIRES */
for ll_row = 1 to ll_rows_in_dw

	/* get all values from external dw before updating database */
	ll_txn_no 							= 	dw_confirmation.getitemnumber(ll_row,'txn_no')
    ll_payment_no						= 	dw_confirmation.getitemnumber(ll_row,'payment_no')

	

	
	ls_payment_type_code	 		= 	dw_confirmation.getitemstring(ll_row,'payment_type_code')
	ls_payment_sub_type_code		=	dw_confirmation.getitemstring(ll_row,'payment_sub_type_code')	
	ls_txn_type_code					=	dw_confirmation.getitemstring(ll_row,'txn_type_code')
	ls_txn_sub_type_code				=	dw_confirmation.getitemstring(ll_row,'txn_sub_type_code')
	ll_claim_no					 		=	dw_confirmation.getitemnumber(ll_row,'claim_no')	
	ldcm_txn_amount					=	dw_confirmation.getitemdecimal(ll_row,'txn_amount')
	ll_cost_alloc_no					= 	dw_confirmation.getitemnumber(ll_row,'cost_alloc_no')  
	ll_cost_alloc_operation_no		=	dw_confirmation.getitemnumber(ll_row,'cost_alloc_operation_no')
	ls_cost_alloc_type_code			=	dw_confirmation.getitemstring(ll_row,'cost_alloc_type_code')
	ldcm_total_payment_amount	=	dw_confirmation.getitemdecimal(ll_row,'total_payment_amount')
	ldcm_tax_amount					=	dw_confirmation.getitemdecimal(ll_row,'txn_tax_amount')
	ll_rehab_qty						    =	dw_confirmation.getitemnumber(ll_row,'adjustment_rehab_qty')	
	ls_recipient_sub_type_code 	    =	dw_confirmation.getitemstring(ll_row,'recipient_sub_type_code')  
	ls_recipient_type_code 			=	dw_confirmation.getitemstring(ll_row,'recipient_type_code')  
	ll_manual_cheque_req_no 		=	dw_confirmation.getitemnumber(ll_row,'manual_cheque_req_no')  
	ldtm_cheque_deposit_date 		=	dw_confirmation.getitemdatetime(ll_row,'cheque_deposit_date')    
	ls_recipient_name 				=	dw_confirmation.getitemstring(ll_row,'recipient_name')   
	ls_address_line1 					=	dw_confirmation.getitemstring(ll_row,'address_line1')  
	ls_address_line2 					=	dw_confirmation.getitemstring(ll_row,'address_line2')  
	ls_city 								=	dw_confirmation.getitemstring(ll_row,'city')  
	ls_prov_state_code 				=	dw_confirmation.getitemstring(ll_row,'prov_state_code')   
	ls_country 							=	dw_confirmation.getitemstring(ll_row,'country')  
	ls_postal_code 					     =	dw_confirmation.getitemstring(ll_row,'postal_code')   
	ls_use_default_address_flag	=	dw_confirmation.getitemstring(ll_row,'use_default_address_flag')   
	ls_cheque_print_group_code 	=	dw_confirmation.getitemstring(ll_row,'cheque_print_group_code')
	ll_recipient_no 					     =	dw_confirmation.getitemnumber(ll_row,'recipient_no')   
    ls_recipient_type_code			=	dw_confirmation.getitemstring(ll_row,'recipient_type_code')   
	ll_cheque_no						=	dw_confirmation.getitemnumber(ll_row,'cheque_no') 
    ll_direct_deposit_xmit_no		     =	dw_confirmation.getitemnumber(ll_row,'direct_deposit_xmit_no')  
    ls_payment_method_code		=	dw_confirmation.getitemstring(ll_row,'payment_method_code')  
    ls_admin_region_code			=	dw_confirmation.getitemstring(ll_row,'admin_region_code')  
	ls_maintain_allowed_flag		= 	dw_confirmation.getitemstring(ll_row,'maintain_allowed_flag')			
     ls_final_payment_flag			= 	dw_confirmation.getitemstring(ll_row,'final_payment_flag')	
	ldtm_paid_from_date				= 	dw_confirmation.getitemdatetime(ll_row,'paid_from_date')	
	ldtm_paid_to_date					= 	dw_confirmation.getitemdatetime(ll_row,'paid_to_date')		
	ll_authorization_no				= 	dw_confirmation.getitemnumber(ll_row,'authorization_no')	
	ls_authorized_by_code			= 	dw_confirmation.getitemstring(ll_row,'authorized_by_code')	
	ldtm_authorized_date				= 	dw_confirmation.getitemdatetime(ll_row,'authorized_date')	
	ls_payment_adjustment_flag	= 	dw_confirmation.getitemstring(ll_row,'payment_adjustment_flag')	
	ll_adjustment_rehab_qty			= 	dw_confirmation.getitemnumber(ll_row,'adjustment_rehab_qty')
	ldcm_adjustment_days_lost		= 	dw_confirmation.getitemdecimal(ll_row,'adjustment_days_lost')
	ldcm_adjustment_hours_lost	= 	dw_confirmation.getitemdecimal(ll_row,'adjustment_hours_lost')
	ll_claim_no_from					= 	dw_confirmation.getitemnumber(ll_row,'claim_no_original')	
	ldcm_tax_rate						= 	dw_confirmation.getitemdecimal(ll_row,'tax_rate')	
	ll_provider_no_original			= 	dw_confirmation.getitemdecimal(ll_row,'provider_no_original')	
	ll_award_no							= 	dw_confirmation.getitemnumber(ll_row,'award_no')	
	ls_new_txn_created				= 	dw_confirmation.getitemstring(ll_row,'new_txn_created')
	ls_annuity_set_a_side			    = 	dw_confirmation.getitemstring(ll_row,'annuity_set_a_side')
	ll_posting_period 				    = 	dw_confirmation.getitemnumber(ll_row,"coc_period")
	ls_expanation 						=	dw_confirmation.getitemstring(ll_row,"explanation")
	ldtm_processed_date				=	dw_confirmation.getitemdatetime(ll_row,"processed_date")
	ll_related_txn_no			 		=	dw_confirmation.getitemnumber(ll_row,'related_txn_no')
	ll_benefit_calculation_no 		=	dw_confirmation.getitemnumber(ll_row,'benefit_calculation_no')
	ll_opening_no						=	dw_confirmation.getitemnumber(ll_row,'opening_no')
	

	
	
	/* unit of work stuff is assigned here */
	if ll_row = 1 then
		ll_last_txn_unit_of_work_no =  	dw_confirmation.getitemnumber(ll_row,'txn_unit_of_work_no')
		
		/* if zero then need to get new number */
		if ll_last_txn_unit_of_work_no = 0 then
			li_rc = ids_last_txn_unit_of_work_no.retrieve()
			SQLCA.nf_handle_error("ids_last_unit_of_work_no.retrieve()",is_window_name,ls_script)
			ll_last_txn_unit_of_work_no = ids_last_txn_unit_of_work_no.getitemnumber(1,'last_txn_unit_of_work_no')

			/* all txn will be assign the same unit of work no for the group of txn being created*/
			ll_last_txn_unit_of_work_no++
			lb_update_last_unit_of_work = true /* used to update last no of table */
			
			ll_ins_unit_of_work_row = ids_txn_unit_of_work_no.insertrow(0)
			
			ids_txn_unit_of_work_no.setitem(ll_ins_unit_of_work_row,'txn_unit_of_work_no',ll_last_txn_unit_of_work_no)
			ids_txn_unit_of_work_no.setitem(ll_ins_unit_of_work_row,'ready_to_process_flag',"Y")
			ids_txn_unit_of_work_no.setitem(ll_ins_unit_of_work_row,'work_group_code',vgst_user_profile.work_group_code)
		end if
	end if		
	


	/* see if txn has to be created - the source txn does not have to be created 
	the source shows up the confirmation screen  so that the user can see clearly the originating txn*/
 	if	ls_new_txn_created = 'Y' then 
	else
		continue
	end if
	
	
	ll_new_txn_cnt++ /* counter for the new txn being created */
	
	ll_org_no = ll_txn_no
	ll_txn_no = ll_last_txn_no + ll_new_txn_cnt	 /* inc the last txn no */
	

	/* create a new payment if necessary (CLAIM TRANSFER & PAYMENT TYPE TRANSFER)  {T6 AND T9} */ 
	if ll_payment_no = 0 then
		
		ll_new_payment_cnt++ /* inc the payment no */
		ll_payment_no = 		ll_last_payment_no + ll_new_payment_cnt
				
		ll_insert_row_payment = ids_payment.insertrow(0)
		ids_payment.setitem(ll_insert_row_payment,"payment_no",ll_payment_no)
		ids_payment.setitem(ll_insert_row_payment,"claim_no",ll_claim_no)
		ids_payment.setitem(ll_insert_row_payment,"award_no",0)
		ids_payment.setitem(ll_insert_row_payment,"payment_type_code",ls_payment_type_code)
		ids_payment.setitem(ll_insert_row_payment,"payment_sub_type_code",ls_payment_sub_type_code)
		ids_payment.setitem(ll_insert_row_payment,"opening_no",ll_opening_no)
		ids_payment.setitem(ll_insert_row_payment,"benefit_calculation_no",ll_benefit_calculation_no  )
		ids_payment.setitem(ll_insert_row_payment,"final_payment_flag",ls_final_payment_flag)
		ids_payment.setitem(ll_insert_row_payment,"paid_hours_lost",0)
		ids_payment.setitem(ll_insert_row_payment,"paid_quantity",ll_adjustment_rehab_qty)
		ids_payment.setitem(ll_insert_row_payment,"paid_from_date",ldtm_paid_from_date)
		ids_payment.setitem(ll_insert_row_payment,"paid_to_date",ldtm_paid_to_date)
		ids_payment.setitem(ll_insert_row_payment,"total_award_amount",ldcm_txn_amount)
		ids_payment.setitem(ll_insert_row_payment,"submitted_amount",0.00)
		ids_payment.setitem(ll_insert_row_payment,"total_deductions",0.00)
		ids_payment.setitem(ll_insert_row_payment,"tax_amount",ldcm_tax_amount)
		ids_payment.setitem(ll_insert_row_payment,"tax_rate",ldcm_tax_rate)
		ids_payment.setitem(ll_insert_row_payment,"loe_explanation",ls_loe_explanation )
		
		if (ls_txn_type_code = "T" AND ls_txn_sub_type_code = "6")  then
		        ids_payment.setitem(ll_insert_row_payment,"authorization_no", ll_authorization_no_to)
         	    wf_insert_invoice_line_item(ll_payment_no,ll_prev_payment_no,ll_claim_no_to, ll_authorization_no_to,ll_claim_no_fr,ll_last_rehab_invoice_no)
	    else
	          ids_payment.setitem(ll_insert_row_payment,"authorization_no", ll_authorization_no)
         end if
	
	
		ids_payment.setitem(ll_insert_row_payment,"authorized_date",ldtm_authorized_date )
		ids_payment.setitem(ll_insert_row_payment,"authorized_by_code",ls_authorized_by_code)
		ids_payment.setitem(ll_insert_row_payment,"payment_adjustment_flag",ls_payment_adjustment_flag)
		ids_payment.setitem(ll_insert_row_payment,"award_no",ll_award_no)
	
	
		
		/* convert the paid hours lost to days */
		// PR 6226 - even though new payments created through claim transfers do not have a benefit calculation,
		// if they are involved in transferring hours, then the 'old' benefit calculation number is needed
		// in order to convert hours into days.
		if ldcm_adjustment_hours_lost <> 0 and il_benefit_calculation_no > 0 then
				
		SELECT isnull(dbo.BENEFIT_CALCULATION.preacc_work_hours_per_day,0.00)  
    	 	INTO :ldcm_preacc_work_hours_per_day 
    		FROM 	dbo.BENEFIT_CALCULATION  
   		WHERE ( dbo.BENEFIT_CALCULATION.claim_no 					= :ll_claim_no_from ) AND  
         	( dbo.BENEFIT_CALCULATION.benefit_calculation_no 	= :il_benefit_calculation_no )   
			using SQLCA;
			if SQLCA.nf_handle_error("select * from BENEFIT_CALCULATION ",is_window_name,ls_script) = 100 then
				messagebox("ERROR","Can't find BENEFIT_CALCULATION for claim no " +  string(ll_claim_no_from) + '  benefit no ' + string(ll_benefit_calculation_no ))
				return -1
			end if

			IF ldcm_preacc_work_hours_per_day > 0 THEN
				ldcm_adjustment_days_lost = ldcm_adjustment_days_lost + (1 * ldcm_adjustment_hours_lost ) / ldcm_preacc_work_hours_per_day 
			END IF
		end if
		
		
		
		ids_payment.setitem(ll_insert_row_payment,"paid_days_lost",ldcm_adjustment_days_lost)
	
		
	ELSE
		ll_prev_payment_no = ll_payment_no
		ll_prev_txn_no		 = ll_txn_no
		
		
		
		/* Only when new payment is not created */
		ll_insert_row_work = ids_adjustment_txn_work_table.insertrow(0)
		ids_adjustment_txn_work_table.setitem(ll_insert_row_work,"txn_no",ll_txn_no)  
		ids_adjustment_txn_work_table.setitem(ll_insert_row_work,"payment_no",ll_payment_no)  
		ids_adjustment_txn_work_table.setitem(ll_insert_row_work,"adjustment_days_lost",ldcm_adjustment_days_lost)  
		ids_adjustment_txn_work_table.setitem(ll_insert_row_work,"adjustment_hours_lost",ldcm_adjustment_hours_lost)  
		ids_adjustment_txn_work_table.setitem(ll_insert_row_work,"adjustment_quantity",ll_adjustment_rehab_qty)  
	
		/* keep the prev # for claim transfers - rehab task etc ..*/
		ll_prev_authorization_no = ll_authorization_no


		IF ll_authorization_no <> 0 THEN
			
			if (ls_txn_type_code = "T" AND ls_txn_sub_type_code = "6")  or &
			   (ls_txn_type_code = 'J' AND ls_txn_sub_type_code <> "4" ) THEN
				
				ll_insert_row_rehab_task = ids_rehab_task_authorization_update.insertrow(0)
				ids_rehab_task_authorization_update.setitem(ll_insert_row_rehab_task,'authorization_no',ll_authorization_no)
				ids_rehab_task_authorization_update.setitem(ll_insert_row_rehab_task,'adjustment_rehab_qty',ll_adjustment_rehab_qty)
				ids_rehab_task_authorization_update.setitem(ll_insert_row_rehab_task,'adjustment_amount',ldcm_txn_amount)		
				ids_rehab_task_authorization_update.setitem(ll_insert_row_rehab_task,'claim_no',ll_claim_no)
			end if
		END IF	
					
	end if
	

	
	/* new txn GETS POPULATED HERE */	
	ll_insert_row = ids_unapplied_claim_txn.insertrow(0)		
	ids_unapplied_claim_txn.setitem(ll_insert_row,"related_txn_no",ll_related_txn_no	)  
	ids_unapplied_claim_txn.setitem(ll_insert_row,"txn_no",ll_txn_no)  
	ids_unapplied_claim_txn.setitem(ll_insert_row,"claim_no",ll_claim_no) 
	ids_unapplied_claim_txn.setitem(ll_insert_row,"payment_no",ll_payment_no)   
	 
	ids_unapplied_claim_txn.setitem(ll_insert_row,"txn_type_code",ls_txn_type_code)   
	ids_unapplied_claim_txn.setitem(ll_insert_row,"txn_sub_type_code",ls_txn_sub_type_code)   
	ids_unapplied_claim_txn.setitem(ll_insert_row,"tax_amount",ldcm_tax_amount)   
	ids_unapplied_claim_txn.setitem(ll_insert_row,"txn_amount",ldcm_txn_amount)  
	ids_unapplied_claim_txn.setitem(ll_insert_row,"coc_period",ll_posting_period)   
	ids_unapplied_claim_txn.setitem(ll_insert_row,"scheduled_processing_date",ldt_today)   
	ids_unapplied_claim_txn.setitem(ll_insert_row,"explanation",ls_expanation)
	ids_unapplied_claim_txn.setitem(ll_insert_row,"txn_unit_of_work_no",ll_last_txn_unit_of_work_no)
	ids_unapplied_claim_txn.setitem(ll_insert_row,"recipient_no",ll_recipient_no)   
	ids_unapplied_claim_txn.setitem(ll_insert_row,"recipient_type_code",ls_recipient_type_code)  
	ids_unapplied_claim_txn.setitem(ll_insert_row,"recipient_sub_type_code",ls_recipient_sub_type_code)  
	ids_unapplied_claim_txn.setitem(ll_insert_row,"cheque_no",ll_cheque_no)   
	ids_unapplied_claim_txn.setitem(ll_insert_row,"direct_deposit_xmit_no",ll_direct_deposit_xmit_no)   
	ids_unapplied_claim_txn.setitem(ll_insert_row,"payment_method_code",ls_payment_method_code)   	
	ids_unapplied_claim_txn.setitem(ll_insert_row,"admin_region_code",ls_admin_region_code)   
	ids_unapplied_claim_txn.setitem(ll_insert_row,"maintain_allowed_flag",ls_maintain_allowed_flag)			
	ids_unapplied_claim_txn.setitem(ll_insert_row,"recipient_sub_type_code",ls_recipient_sub_type_code )   
	ids_unapplied_claim_txn.setitem(ll_insert_row,"manual_cheque_req_no",	ll_manual_cheque_req_no) 
	ids_unapplied_claim_txn.setitem(ll_insert_row,"cheque_deposit_date",ldtm_cheque_deposit_date)   
	ids_unapplied_claim_txn.setitem(ll_insert_row,"recipient_name",ls_recipient_name)   
	ids_unapplied_claim_txn.setitem(ll_insert_row,"address_line1",ls_address_line1)  
	ids_unapplied_claim_txn.setitem(ll_insert_row,"address_line2",ls_address_line2)  
	ids_unapplied_claim_txn.setitem(ll_insert_row,"city",ls_city)  
	ids_unapplied_claim_txn.setitem(ll_insert_row,"prov_state_code",ls_prov_state_code)   
	ids_unapplied_claim_txn.setitem(ll_insert_row,"country",ls_country )  
	ids_unapplied_claim_txn.setitem(ll_insert_row,"postal_code",ls_postal_code)   
	ids_unapplied_claim_txn.setitem(ll_insert_row,"use_default_address_flag",ls_use_default_address_flag)   
	ids_unapplied_claim_txn.setitem(ll_insert_row,"cheque_print_group_code",ls_cheque_print_group_code)        



	  
	  
	/* create cost allocated records  HERE*/
	ldcm_cost_amount = ldcm_txn_amount - ldcm_tax_amount
	
	if ldcm_cost_amount = 0.00 then
	else
	
		if ll_cost_alloc_no <> 0 then
			ll_insert_row_coc = ids_cost_of_claims_allocated.insertrow(0)
			
			ll_new_coc_allocated_no++
			
			ll_coc_allocated_no = ll_last_coc_allocated_no + ll_new_coc_allocated_no
						
			ids_cost_of_claims_allocated.setitem(ll_insert_row_coc,'coc_allocated_no',ll_coc_allocated_no)
			ids_cost_of_claims_allocated.setitem(ll_insert_row_coc,'claim_no',ll_claim_no)
			ids_cost_of_claims_allocated.setitem(ll_insert_row_coc,'payment_no',ll_payment_no)
			ids_cost_of_claims_allocated.setitem(ll_insert_row_coc,'txn_no',ll_txn_no)
			ids_cost_of_claims_allocated.setitem(ll_insert_row_coc,'cost_alloc_no',ll_cost_alloc_no)
			ids_cost_of_claims_allocated.setitem(ll_insert_row_coc,'cost_alloc_operation_no',ll_cost_alloc_operation_no)
			ids_cost_of_claims_allocated.setitem(ll_insert_row_coc,'cost_alloc_type_code',ls_cost_alloc_type_code)
			ids_cost_of_claims_allocated.setitem(ll_insert_row_coc,'cost_amount',ldcm_cost_amount)
			ids_cost_of_claims_allocated.setitem(ll_insert_row_coc,'coc_period',ll_posting_period)
		end if
	end if

	/* SO THAT THE NEW TXN NO IS VISIBLE ON THE DATAWINDOW */
	dw_confirmation.SETITEM(ll_row,'new_txn_created',"N")

	// If the Adjustment of cancel cheque and/or direct dpeosit or overpayment revover reduces the payment to zero then
	// then the REHAB_INVOICE_LINE_ITEM.line_item_amended_code will be set to  'OR' {e-physio Deployment 2 } for Overpayment Recover and
	// 'CC' for canceled cheque.
	// introduced from the E-physio project    - if adjusting up must blank out amended code 'or'
		 if ldcm_txn_amount = 0 then //
	            if (ls_txn_type_code = 'J' AND (ls_txn_sub_type_code= "2" or ls_txn_sub_type_code= "3") or (ls_txn_type_code = 'T' AND ls_txn_sub_type_code= "6") )		THEN
				     wf_update_rehab_invoice_line_item_amend( ls_txn_type_code,ls_txn_sub_type_code,ll_payment_no)
					  
					  if (  ids_rehab_invoice_line_item.rowcount() > 0)  then						 
					  	    li_rc = ids_rehab_invoice_line_item.update()
					        SQLCA.nf_handle_error(" ids_rehab_invoice_line_item.update()",is_window_name,ls_script)					
			         end if
                 end if
	END IF

next


/*  calls the logic to adjust the rehab authorization qty */
if wf_update_rehab_authorization(	ls_txn_type_code	,ls_txn_sub_type_code) < 0 then return -1// updates ids_rehab_task_authorization_update




/* UPDATE THE DATABASE */

/* if any txn were created  {update the last txn no tables and Unit of work*/
if ids_unapplied_claim_txn.rowcount() > 0 then

	ids_last_claim_txn_no.setitem(1,'last_txn_no',ll_txn_no)
	ids_last_txn_unit_of_work_no.setitem(1,'last_txn_unit_of_work_no',ll_last_txn_unit_of_work_no)

	if lb_update_last_unit_of_work then
		
		ids_last_txn_unit_of_work_no.update()
		SQLCA.nf_handle_error("ids_last_txn_unit_of_work_no.update()",is_window_name,ls_script)
		
		ids_txn_unit_of_work_no.update()
		SQLCA.nf_handle_error("ids_txn_unit_of_work_no.update()",is_window_name,ls_script)
		
	end if
	
	ids_last_claim_txn_no.update()
	SQLCA.nf_handle_error("ids_last_claim_txn_no.update()",is_window_name,ls_script)

	ids_unapplied_claim_txn.update()
	SQLCA.nf_handle_error("ids_unapplied_claim_txn.update()",is_window_name,ls_script)
	
end if


/* if any payments were created the rowcount would be greater than zero */
if ids_payment.rowcount() > 0 then
	
	ids_last_payment_no.setitem(1,'last_payment_no',ll_payment_no)
	ids_last_payment_no.update()
	SQLCA.nf_handle_error("ids_last_payment_no.update()",is_window_name,ls_script)

	ids_payment.update()
	SQLCA.nf_handle_error("ids_payment.update()",is_window_name,ls_script)
end if


if ids_rehab_task.rowcount() > 0 then
	ids_rehab_task.update()
	SQLCA.nf_handle_error("ids_rehab_task.update()",is_window_name,ls_script)		
end if

if ids_rehab_task_authorization_insert.rowcount() > 0 then
	ids_rehab_task_authorization_insert.update()
	SQLCA.nf_handle_error("ids_rehab_task_authorization_insert.update()",is_window_name,ls_script)	
end if



if ids_rehab_invoice_insert.rowcount() > 0 then
	
	ids_last_rehab_invoice_no.setitem(1,'last_rehab_invoice_no',ll_last_rehab_invoice_no)
    ids_last_rehab_invoice_no.update()
    SQLCA.nf_handle_error(" ids_rehab_invoice_line_item.update()",is_window_name,ls_script)

	ids_rehab_invoice_insert.update()
	SQLCA.nf_handle_error("ids_rehab_invoice_insert",is_window_name,ls_script)
	
	ids_rehab_invoice_line_item_insert.update()
	SQLCA.nf_handle_error("ids_rehab_invoice_line_item_insert.update()",is_window_name,ls_script)
	
end if


/* coc alloated -  takes care of any new cost allocation records */
if ids_cost_of_claims_allocated.rowcount() > 0 then	
	
	ids_last_coc_allocated_no.setitem(1,'last_coc_allocated_no',ll_coc_allocated_no)
	ids_last_coc_allocated_no.update()
	SQLCA.nf_handle_error("ids_last_coc_allocated_no.update()",is_window_name,ls_script)
	
	ids_cost_of_claims_allocated.update()
	SQLCA.nf_handle_error("ids_cost_of_claims_allocated.update()",is_window_name,ls_script)
end if



/* adjustment work table */
if ids_adjustment_txn_work_table.rowcount() > 0 then
	ids_adjustment_txn_work_table.update()
	SQLCA.nf_handle_error("ids_adjustment_txn_work_table.update()",is_window_name,ls_script)	
end if


SQLCA.nf_commit_transaction()



/* end peform some cleanup code */
dw_transaction_list.RESET()
cb_cancel.enabled = false
cb_save.enabled = FALSE

wf_create_destroy_datastore('DESTROY')

SetPointer(oldpointer)

/* reset the dw parm stuff */
dw_parameter.reset()
dw_parameter.insertrow(0)
wf_reset_parm_screen()

inv_ccm.of_reset()

/* disable the close button */
cb_close_confirm.enabled = true
inv_ccm.ib_suppress_warnings = false


return 0

end function

public function integer wf_confirm ();
pointer oldpointer // Declares a pointer variable			

DECIMAL {2} ldcm_txn_amount
DECIMAL {2} ldcm_control_amount
DECIMAL {2} ldcm_total_payment_amount
DECIMAL {2} ldcm_txn_tax_amount	
DECIMAL {2} ldcm_txn_non_tax_amt
DECIMAL {2} ldcm_tax_rate
DECIMAL {2} ldcm_pct
DECIMAL {2} ldcm_total_adjustment_amount
DECIMAL {2} ldcm_net_hours_lost
DECIMAL {2} ldcm_txn_amount_entered 				
DECIMAL {2} ldcm_tax_amount_entered 	
DECIMAL {2} ldcm_net_hours_lost_entered	
DECIMAL {2} ldcm_net_days_lost
DECIMAL {2} ldcm_paid_hours_lost
DECIMAL {2} ldcm_net_days_lost_entered		
DECIMAL {2} ldcm_total_txn_amount
DECIMAL {2} ldcm_total_txn_amount_transfered
DECIMAL {2} ldscm_x
decimal	ldcm_new_net_days_lost
decimal   ldcm_new_net_hours_lost
decimal   ldcm_sum_rehab_qty 


INTEGER	li_rc, li_msg_rtn
INTEGER	li_x1
INTEGER	li_new_row
INTEGER	li_seq_no // used to determine sequence of an ultimate txn within a split payment
integer     i


BOOLEAN 	lb_full_amount
BOOLEAN	lb_add_new
BOOLEAN	lb_full_transfer
BOOLEAN     lb_no_rehab_qty = false


DATETIME   ldtm_processed_date
DATETIME	ldtm_paid_from_date
DATETIME	ldtm_paid_to_date
DATETIME	ldtm_cheque_deposit_date
DATETIME	ldtm_authorized_date

string      ls_err
string		ls_find
STRING 	ls_filter
STRING	ls_annuity_set_a_side 
STRING 	ls_new_txn_type_code 
STRING	ls_new_txn_sub_type_code
STRING	ls_cost_alloc_type_code
STRING	ls_payment_type_code
STRING	ls_payment_sub_type_code
STRING 	ls_payment_type_code_to
STRING	ls_payment_type_code_fr
STRING	ls_payment_sub_type_code_fr
STRING	ls_payment_sub_type_code_to
STRING	ls_old_txn_type_code
STRING	ls_old_txn_sub_type_code
STRING	ls_recipient_name 
STRING	ls_address_line1
STRING	ls_address_line2
STRING	ls_city
STRING	ls_prov_state_code
STRING	ls_country 
STRING	ls_script = "wf_confirm"
STRING	ls_postal_code
STRING	ls_use_default_address_flag
STRING	ls_cheque_print_group_code
STRING	ls_cost_alloc_required_flag
STRING	ls_cost_alloc_type_code_to
STRING	ls_recipient_sub_type_code
STRING	ls_recipient_type_code
STRING 	ls_payment_method_code
STRING	ls_admin_region_code
STRING	ls_maintain_allowed_flag
STRING	ls_final_payment_flag
STRING	ls_authorized_by_code
STRING	ls_payment_adjustment_flag
STRING	ls_maintain_zero_allowed_flag
STRING	ls_explanation
STRING   ls_org_filter
STRING	ls_benefit_type
STRING	ls_split_payment_flag, ls_cost_relief_employer_type_code

long       ll_check  = 0
long       ll_cnt 
long		ll_claim_no_org
long		ll_check_box
LONG		ll_new_authorization_no
LONG		ll_coc_period
LONG		ll_authorization_no
LONG   	ll_dw_confirmation_row
LONG		ll_payment_no
LONG		ll_txn_no
LONG		ll_no_checked
LONG		ll_claim_no
LONG		ll_related_txn_no
LONG 		ll_cost_alloc_no_fr
LONG		ll_cost_alloc_no_to
LONG		ll_cost_alloc_operation_no_fr
LONG		ll_cost_alloc_operation_no_to
LONG		ll_cost_alloc_no
LONG		ll_cost_alloc_operation_no
LONG		ll_cheque_no
LONG 		ll_claim_no_to
LONG		ll_recipient_no
LONG		ll_manual_cheque_req_no 
LONG 		ll_direct_deposit_xmit_no
LONG		ll_award_no
LONG 		ll_no_txn
LONG	 	ll_row
LONG		ll_txn_unit_of_work_no
LONG		ll_annuity_txn 
LONG		ll_temp_txn_no	
LONG 		ll_net_quantity_entered								
LONG 		ll_net_quantity
LONG      ll_provider_no
long		ll_claim_no_fr
long       ll_error_payment_no
long		ll_error_txn_no
long		ll_error_claim_no
long		ll_opening_no
long		ll_ultimate_txn_no , ll_found, ll_old_found
long        ll_authorization_no_to
//long        ll_billable_xref_numbers[]
string      ls_receipient_type[]
datastore lds_temp1
datastore lds_temp2
string ls_recipient_type_to_check
long ll_found_row
long ll_recipient_no_to_check

long ll_zz
long ll_aa 




/* initialize var */
ldcm_total_adjustment_amount = 0


/* used to check br before send to the br object */
lds_temp2 = CREATE datastore
lds_temp2.dataobject = "ds_check_recipient"


lds_temp1 = Create datastore
lds_temp1 .dataobject = "ds_check_billable_xref"



oldpointer = SetPointer(HourGlass!)

/* reset the ds used in the business rule object */
inv_ccm.of_reset()

/*create the datastores required */
wf_create_destroy_datastore('CREATE')

/* make sure there are rows in the confirm dw - if not display warning */
if dw_transaction_list.rowcount() = 0 then
	Messagebox("Warning","There are no rows selected to confirm")
	return -1
end if




/* ACCEPT VALUES FROM PARAMETER SCREEN */
dw_parameter.accepttext()
dw_transaction_list.accepttext()

/* disable the close button */
cb_close_confirm.enabled = false

/* get the user input columns from screen */
ls_new_txn_type_code					= dw_parameter.getitemSTRING(1,'txn_type_code')
ls_new_txn_sub_type_code				= dw_parameter.getitemSTRING(1,'txn_sub_type_code')
ll_claim_no_to								= dw_parameter.getitemnumber(1,'claim_no_to')
ls_payment_type_code_to				= dw_parameter.getitemSTRING(1,'payment_type_code_to')
ls_payment_sub_type_code_to			= dw_parameter.getitemSTRING(1,'payment_sub_type_code_to')
ls_payment_type_code_fr				= dw_parameter.getitemSTRING(1,'payment_type_code_fr')
ls_payment_sub_type_code_fr			= dw_parameter.getitemSTRING(1,'payment_sub_type_code_fr')
ll_cost_alloc_no_to						= dw_parameter.getitemnumber(1,'cost_alloc_no_to')
ll_cost_alloc_operation_no_to			= dw_parameter.getitemnumber(1,'cost_alloc_operation_no_to')
ll_cost_alloc_no_fr							= dw_parameter.getitemnumber(1,'cost_alloc_no_fr')
ll_cost_alloc_operation_no_fr			= dw_parameter.getitemnumber(1,'cost_alloc_operation_no_fr')
ldcm_control_amount						= dw_parameter.GetItemDecimal(1,'amount')
ls_explanation								= dw_parameter.getitemSTRING(1,'explanation')
ll_coc_period								= dw_parameter.getitemnumber(1,'posting_period')
ll_claim_no_fr								= dw_parameter.getitemnumber(1,'claim_no')
ls_benefit_type								= dw_parameter.getitemstring(1,'benefit_type')
ll_provider_no								= dw_parameter.GetItemNumber(1,'provider_no')
ll_authorization_no_to					= dw_parameter.GetItemNumber(1,'authorization_no_to')


IF ll_provider_no > 0 THEN
	inv_ccm.ib_cancel_sp = TRUE
END IF


/* insert row in the batch control for br object */
li_new_row = inv_ccm.ids_batch_control.insertrow(0)


/* Data from the parameter screen */
inv_ccm.ids_batch_control.SetItem(li_new_row ,'new_txn_type_code',ls_new_txn_type_code)
inv_ccm.ids_batch_control.SetItem(li_new_row ,'new_txn_sub_type_code',ls_new_txn_sub_type_code)
inv_ccm.ids_batch_control.SetItem(li_new_row ,'explanation',ls_explanation)
inv_ccm.ids_batch_control.SetItem(li_new_row ,'to_claim_no',ll_claim_no_to)
inv_ccm.ids_batch_control.SetItem(li_new_row ,'from_claim_no',ll_claim_no_fr)
inv_ccm.ids_batch_control.SetItem(li_new_row,'authorization_no_to',ll_authorization_no_to)
inv_ccm.ids_batch_control.SetItem(li_new_row ,'from_payment_type_code',ls_payment_type_code_fr)
inv_ccm.ids_batch_control.SetItem(li_new_row ,'from_payment_sub_type_code',ls_payment_sub_type_code_fr)
inv_ccm.ids_batch_control.SetItem(li_new_row ,'to_payment_type_code',ls_payment_type_code_to )
inv_ccm.ids_batch_control.SetItem(li_new_row ,'to_payment_sub_type_code',ls_payment_sub_type_code_to)	
inv_ccm.ids_batch_control.SetItem(li_new_row ,'from_cost_alloc_no',ll_cost_alloc_no_fr )
inv_ccm.ids_batch_control.SetItem(li_new_row ,'from_cost_alloc_operation_no',ll_cost_alloc_operation_no_fr)
inv_ccm.ids_batch_control.SetItem(li_new_row ,'to_cost_alloc_no',ll_cost_alloc_no_to)
inv_ccm.ids_batch_control.SetItem(li_new_row ,'to_cost_alloc_operation_no',ll_cost_alloc_operation_no_to)
inv_ccm.ids_batch_control.SetItem(li_new_row ,'cheque_no',dw_parameter.GetItemNumber(1,'cheque_no'))	
inv_ccm.ids_batch_control.SetItem(li_new_row ,'direct_deposit_xmit_no',dw_parameter.GetItemNumber(1,'xmit_no'))	
inv_ccm.ids_batch_control.SetItem(li_new_row ,'cheque_deposit_date',dw_parameter.GetItemDateTime(1,'issue_date'))	
inv_ccm.ids_batch_control.SetItem(li_new_row ,'coc_period',ll_coc_period)	
inv_ccm.is_benefit_type = ls_benefit_type


/* setting claim no 'to'  when not suppplied */

if isnull(ll_claim_no_to) or ll_claim_no_to = 0 then
	ll_claim_no_to = 	dw_parameter.getitemnumber(1,'claim_no')
	inv_ccm.ids_batch_control.SetItem(li_new_row ,'to_claim_no',ll_claim_no_to)
end if

choose case ls_new_txn_type_code
	case "T"
	choose case ls_new_txn_sub_type_code
		case "6", "9" /* claim transfer */
			
		  SELECT dbo.CLAIM.cost_alloc_no,   
					dbo.CLAIM.cost_alloc_operation_no  
			 INTO :ll_cost_alloc_no_to,   
					:ll_cost_alloc_operation_no_to  
			 FROM dbo.CLAIM  
			WHERE dbo.CLAIM.claim_no = :ll_claim_no_to   ;

			if SQLCA.nf_handle_error("select from Payment_Sub_Type",is_window_name,ls_script) = 100 then
				messagebox("ERROR","Problem finding cost allocation")
				return -1
			end if
			inv_ccm.ids_batch_control.SetItem(li_new_row ,'to_cost_alloc_no',ll_cost_alloc_no_to)
			inv_ccm.ids_batch_control.SetItem(li_new_row ,'to_cost_alloc_operation_no',ll_cost_alloc_operation_no_to)
		CASE '8'
			SELECT isnull(employer_type_code,'')
			INTO   :ls_cost_relief_employer_type_code
			FROM   EMPLOYER
			WHERE  employer_no = :ll_cost_alloc_no_fr 
			USING sqlca; 
			SQLCA.nf_handle_error('w_claim_cost_maintenance', 'embedded SQL: SELECT employer_type_code FROM EMPLOYER', 'wf_confirm') 
			
			IF ls_cost_relief_employer_type_code = 'S' THEN
				li_msg_rtn = MessageBox('Cost Relief Warning','You are providing Cost Relief to a Self-Insured employer.' &
				                      + '~r~n~r~nDo you want to continue?',Question!,YesNo!,2)
				IF li_msg_rtn = 2 THEN
					// return 0 so that the parameters are not cleared
					RETURN 0
				END IF
			END IF
	end choose
end choose



/* USED TO ENSURE BR 1.30 IS ENFORCED*/
ls_maintain_zero_allowed_flag = wf_get_maintain_zerod_allowed_flag(ls_new_txn_type_code,ls_new_txn_sub_type_code)

/* LOOP THROUGH AND POPULATE DATASTORE REQUIRED FOR OBJECT */
	
ll_no_txn = dw_transaction_list.rowcount()

for li_x1 = 1 to ll_no_txn

	ll_check_box = dw_transaction_list.getitemnumber(li_x1,'check_box')
	
	if ll_check_box = 1 then
		
		ll_related_txn_no = dw_transaction_list.getitemnumber(li_x1,'related_txn_no')
		
		ll_row = inv_ccm.ids_maintenance.INSERTROW(0)
	
		//Data from the target txn	
		inv_ccm.ids_maintenance.SetItem(ll_row,'target_payment_no',dw_transaction_list.getitemnumber(li_x1,'payment_no'))
		inv_ccm.ids_maintenance.SetItem(ll_row,'target_txn_no',dw_transaction_list.getitemnumber(li_x1,'txn_no'))
		inv_ccm.ids_maintenance.SetItem(ll_row,'adjustment_txn_amount',dw_transaction_list.GetItemDecimal(li_x1,'amt_entered'))
		inv_ccm.ids_maintenance.SetItem(ll_row,'adjustment_tax_amount',dw_transaction_list.GetItemDecimal(li_x1,'txn_tax_amt_entered'))
		inv_ccm.ids_maintenance.SetItem(ll_row,'adjustment_quantity',dw_transaction_list.getitemnumber(li_x1,'rehab_qty_entered'))
		inv_ccm.ids_maintenance.SetItem(ll_row,'adjustment_days',dw_transaction_list.GetItemDecimal(li_x1,'days_entered'))	
		inv_ccm.ids_maintenance.SetItem(ll_row,'adjustment_hours',dw_transaction_list.GetItemDecimal(li_x1,'hours_entered'))	
		ldcm_total_adjustment_amount = ldcm_total_adjustment_amount +  dw_transaction_list.GetItemDecimal(li_x1,'amt_entered')
		ldcm_sum_rehab_qty = ldcm_sum_rehab_qty + dw_transaction_list.getitemnumber(li_x1,'rehab_qty_entered')
		
		if  dw_transaction_list.getitemnumber(li_x1,'rehab_qty_entered') = 0 then  lb_no_rehab_qty = true
		
		if wf_check_for_unprocessed(dw_transaction_list.getitemnumber(li_x1,'txn_no')) < 0 then return -1

		if ll_row = 1 then
			ls_payment_type_code			=  dw_transaction_list.getitemSTRING(li_x1,'payment_type_code')
			ls_payment_sub_type_code 	=  dw_transaction_list.getitemSTRING(li_x1,'payment_sub_type_code')	
			
			if ls_payment_type_code_fr = "" then
				inv_ccm.ids_batch_control.SetItem(li_new_row ,'from_payment_type_code',ls_payment_type_code)
				inv_ccm.ids_batch_control.SetItem(li_new_row ,'from_payment_sub_type_code',ls_payment_sub_type_code)
				inv_ccm.ids_batch_control.SetItem(li_new_row ,'to_payment_type_code',ls_payment_type_code)
				inv_ccm.ids_batch_control.SetItem(li_new_row ,'to_payment_sub_type_code',ls_payment_sub_type_code)	
			end if
		
		end if
		
		

	end if

NEXT


 
/* USED TO CHECK br 14.140 added this to check the the same billable xref no exist - store in array a list of billable xref no
also BR 14.70 - SAME PROVIDER */
if  ldcm_sum_rehab_qty  <> 0 then
	ll_no_txn = dw_transaction_list.rowcount()
	ll_cnt = 1
	for li_x1 = 1 to ll_no_txn
		ll_check_box = dw_transaction_list.getitemnumber(li_x1,'check_box')
		if ll_check_box = 1 then
			
			ll_aa = lds_temp1.insertrow(0)
			
			lds_temp1.setitem(ll_aa,'billable_xref_no',dw_transaction_list.getitemnumber(li_x1,'billable_xref_no'))

			ll_zz = lds_temp2.insertrow(0)
			lds_temp2.setitem(ll_zz,'recipient_type',dw_transaction_list.getitemstring(li_x1,'recipient_type_code'))
			lds_temp2.setitem(ll_zz,'recipient_no',dw_transaction_list.getitemnumber(li_x1,'recipient_no'))			
			ll_cnt++
		  end if
   next
end if

						
/* set the adjustment amount after running through the loop */
inv_ccm.ids_batch_control.SetItem(li_new_row ,'total_adjustment_txn_amount',ldcm_total_adjustment_amount)
inv_ccm.ids_batch_control.SetItem(li_new_row ,'sum_rehab_qty',ldcm_sum_rehab_qty)


if ls_new_txn_type_code = 'J' AND ls_new_txn_sub_type_code = '2' then
	dw_parameter.setitem(1,'amount',ldcm_total_adjustment_amount)
	dw_parameter.ACCEPTTEXT()
ELSEif ldcm_total_adjustment_amount <> ldcm_control_amount then
	MESSAGEBOX("Warning","The control amount does not match the total txn amount adjusted")	
	return -1
end if


if (ls_new_txn_type_code = "T"AND ls_new_txn_sub_type_cODE = '6') then

	
	/*14.20   All of the payments selected for transfer within the same transaction must have a rehab authorization linked to each payment or none of the payments must have a rehab authorization linked to each payment*/
	if ldcm_sum_rehab_qty  <> 0 and lb_no_rehab_qty then		
		MESSAGEBOX("Warning","BR 14.20 - You cannot transfer these payments in one unit of work because some of the payments have a rehab authorization and some of the payments do not.")	
		return -1
	end if

	
	/*14.70  If the recipient of the payment(s) being transferred is a provider, then the payment recipient for each payments must all be the same provider.*/
	for ll_zz = 1 to lds_temp2.rowcount()
		 ls_recipient_type_to_check =   lds_temp2.getitemstring(ll_zz,'recipient_type')
		 ll_recipient_no_to_check = lds_temp2.getitemnumber(ll_zz,'recipient_no')
		
		if ( ls_recipient_type_to_check= 'M' OR  ls_recipient_type_to_check= 'O' OR ls_recipient_type_to_check = 'C') THEN
				
				ls_find = 'recipient_type <> "' + ls_recipient_type_to_check  + '"  or recipient_no <> ' +  string( ll_recipient_no_to_check) 
				/* check to see if there are any other rows that are not for that provider */
		
				ll_found_row = lds_temp2.Find(ls_find, ll_zz,lds_temp2.RowCount())		
				
				if ll_found_row > 0 then
				  messagebox("Warning",'BR 14.70  - The recipients of the payments being transferred are for different providers – the payments must all have been paid to the same provider to transfer them to the same rehab authorization.')
				  return -1
				  exit
			end if
		end if
		
	next
	
	
	/* 14.140   The billable item are invoiced for the From Claim payment(s) must all be for the same billable item */
	/*    from the array - assign to datawindow to sort  dw - assign back to array then check for duplicats */
	
	
	
	
	lds_temp1.SetSort("billable_xref_no ASC")
	lds_temp1.Sort()

	
	FOR i = 1 to lds_temp1.rowcount()
		if  i  = 1 then
			ll_check = lds_temp1.getitemnumber(i,'billable_xref_no')
		else
			if (ll_check <> lds_temp1.getitemnumber(i,'billable_xref_no'))  then
				  messagebox("Warning",'BR 14.140 -The billable items for the payments being transferred are for different billable items – the billable items must all be the same billable items to transfer them together.')
				  return -1
				  exit
			end if
		end if
	
	NEXT

end if


DESTROY lds_temp1
DESTROY lds_temp2


/* all business rules will go through the common br object if possible  - A few are done prior */
TRY
	inv_ccm.of_check_business_rules()
CATCH (uo_br_exception luo_br_exception)
	if luo_br_exception.GetMessage() <> '' Then
		MessageBox('Error occured',luo_br_exception.GetMessage())	
	END IF
	luo_br_exception.getindicators(ll_error_payment_no,ll_error_txn_no,ll_error_claim_no)
	
	if ll_error_payment_no <> 0 then
		dw_parameter.setitem(1,'payment_no_find',ll_error_payment_no)
		cb_find.triggerevent(clicked!)
	end if
	
	RETURN -1
END TRY


/* variables used in process - set defaults */
lb_full_transfer 			= false
ll_txn_unit_of_work_no 	= 0 	
lb_add_new 					= false
ll_temp_txn_no 			= 0  /* use to assign related txn temp until save */

/* reset data in datastore and datawindow */
dw_confirmation.reset()
dw_confirmation_summary.reset()

/* assign a new unit of work - if required*/
ll_txn_unit_of_work_no = wf_assign_unit_of_work(lb_add_new,dw_parameter.getitemnumber(1,'add_txn_unit_of_work'),ls_new_txn_type_code,ls_new_txn_sub_type_code)

SELECT cost_alloc_required_flag
INTO 	:ls_cost_alloc_required_flag
FROM 	dbo.Payment_Sub_Type  
WHERE ( dbo.Payment_Sub_Type.payment_type_code = :ls_payment_type_code) AND  
		( dbo.Payment_Sub_Type.payment_sub_type_code = :ls_payment_sub_type_code )   
using SQLCA;
SQLCA.nf_handle_error("select from Payment_Sub_Type",is_window_name,ls_script)

dw_transaction_list.SETREDRAW(FALSE)


ll_no_txn = dw_transaction_list.rowcount()

for li_x1 = 1 to ll_no_txn
	
	ll_check_box = dw_transaction_list.getitemnumber(li_x1,'check_box')
	
	if ll_check_box = 1 then
		
		ll_no_checked++
		ll_payment_no 						=  dw_transaction_list.getitemnumber(li_x1,'payment_no')
		ll_txn_no 							=  dw_transaction_list.getitemnumber(li_x1,'txn_no')
		ls_payment_type_code			=  dw_transaction_list.getitemSTRING(li_x1,'payment_type_code')
		ls_payment_sub_type_code 		=  dw_transaction_list.getitemSTRING(li_x1,'payment_sub_type_code')	
		ll_claim_no 							=	dw_transaction_list.getitemnumber(li_x1,'claim_no')
		ldcm_total_payment_amount	=	dw_transaction_list.GetItemDecimal(li_x1,'total_payment_amount')
		ll_related_txn_no 					=	dw_transaction_list.getitemnumber(li_x1,'related_txn_no')
		ll_cost_alloc_no					=	dw_transaction_list.getitemnumber(li_x1,'cost_alloc_no')
		ll_cost_alloc_operation_no 		=	dw_transaction_list.getitemnumber(li_x1,'cost_alloc_operation_no')
		ls_cost_alloc_type_code			=	dw_transaction_list.getitemSTRING(li_x1,'cost_alloc_type_code')
		ldtm_processed_date 			=	dw_transaction_list.getitemdatetime(li_x1,'applied_processed_date')
		ls_old_txn_type_code				= 	dw_transaction_list.getitemSTRING(li_x1,'txn_type_code')
		ls_old_txn_sub_type_code		= 	dw_transaction_list.getitemSTRING(li_x1,'txn_sub_type_code')
		ls_recipient_sub_type_code 		= 	dw_transaction_list.getitemSTRING(li_x1,'recipient_sub_type_code')   
		ll_manual_cheque_req_no 		= 	dw_transaction_list.getitemnumber(li_x1,'manual_cheque_req_no')  
		ldtm_cheque_deposit_date 		= 	dw_transaction_list.getitemdatetime(li_x1,'cheque_deposit_date')    
		ls_recipient_name 				= 	dw_transaction_list.getitemSTRING(li_x1,'recipient_name')   
		ls_address_line1 					= 	dw_transaction_list.getitemSTRING(li_x1,'address_line1')  
		ls_address_line2 					= 	dw_transaction_list.getitemSTRING(li_x1,'address_line2')  
		ls_city 								= 	dw_transaction_list.getitemSTRING(li_x1,'city')  
		ls_prov_state_code 				= 	dw_transaction_list.getitemSTRING(li_x1,'prov_state_code')   
		ls_country 							= 	dw_transaction_list.getitemSTRING(li_x1,'country')  
		ls_postal_code 						= 	dw_transaction_list.getitemSTRING(li_x1,'postal_code')   
		ls_use_default_address_flag	=  dw_transaction_list.getitemSTRING(li_x1,'use_default_address_flag')   
		ls_cheque_print_group_code	=  dw_transaction_list.getitemSTRING(li_x1,'cheque_print_group_code')
		ll_recipient_no						= 	dw_transaction_list.getitemnumber(li_x1,"recipient_no")   
		ls_recipient_type_code			= 	dw_transaction_list.getitemSTRING(li_x1,"recipient_type_code")   
		ll_cheque_no						= 	dw_transaction_list.getitemnumber(li_x1,"cheque_no")   
		ll_direct_deposit_xmit_no		= 	dw_transaction_list.getitemnumber(li_x1,"direct_deposit_xmit_no")   
		ls_payment_method_code		= 	dw_transaction_list.getitemSTRING(li_x1,"payment_method_code")   
		ls_admin_region_code			= 	dw_transaction_list.getitemSTRING(li_x1,"admin_region_code")   
		ls_maintain_allowed_flag		= 	dw_transaction_list.getitemSTRING(li_x1,"maintain_allowed_flag")
		ls_final_payment_flag				= 	dw_transaction_list.getitemSTRING(li_x1,"final_payment_flag") 
		ldtm_paid_from_date				= 	dw_transaction_list.getitemdatetime(li_x1,"paid_from_date") 
		ldtm_paid_to_date					= 	dw_transaction_list.getitemdatetime(li_x1,"paid_to_date") 
		ll_authorization_no				= 	dw_transaction_list.getitemnumber(li_x1,"authorization_no") 
		ldtm_authorized_date				= 	dw_transaction_list.getitemdatetime(li_x1,"authorized_date") 
		ls_authorized_by_code			= 	dw_transaction_list.getitemSTRING(li_x1,"authorized_by_code") 
		ls_payment_adjustment_flag	= 	dw_transaction_list.getitemSTRING(li_x1,"payment_adjustment_flag") 
		ldcm_tax_rate						= 	dw_transaction_list.GetItemDecimal(li_x1,"tax_rate")
		ll_award_no							= 	dw_transaction_list.getitemnumber(li_x1,"award_no")
		ls_annuity_set_a_side				= 	dw_transaction_list.getitemSTRING(li_x1,"annuity_set_a_side")  
		il_benefit_calculation_no			= 	dw_transaction_list.getitemnumber(li_x1,"benefit_calculation_no")  
		ll_opening_no						= 	dw_transaction_list.getitemnumber(li_x1,"opening_no")  
	
		/* values from transaction */
		ldcm_txn_amount 				=	dw_transaction_list.GetItemDecimal(li_x1,'txn_amount')
		ldcm_txn_tax_amount			= 	dw_transaction_list.GetItemDecimal(li_x1,'txn_tax_amount')
		ldcm_txn_non_tax_amt		= 	dw_transaction_list.GetItemDecimal(li_x1,'txn_non_tax_amt')
		ll_net_quantity					= 	dw_transaction_list.getitemnumber(li_x1,'net_quantity')
		ldcm_net_days_lost			= 	dw_transaction_list.GetItemDecimal(li_x1,"net_days_lost") 
		ldcm_net_hours_lost			= 	dw_transaction_list.GetItemDecimal(li_x1,"net_hours_lost") 
		
		/* values entered */
		ldcm_txn_amount_entered 		=		dw_transaction_list.GetItemDecimal(li_x1,'amt_entered')
		ldcm_tax_amount_entered 		= 		dw_transaction_list.GetItemDecimal(li_x1,'txn_tax_amt_entered')
		ll_net_quantity_entered			= 		dw_transaction_list.getitemnumber(li_x1,'rehab_qty_entered')
		ldcm_net_days_lost_entered	= 		dw_transaction_list.getitemnumber(li_x1,'days_entered')
		ldcm_net_hours_lost_entered	= 		dw_transaction_list.GetItemDecimal(li_x1,'hours_entered')
		
		/* values to determine split payment status */
		ls_split_payment_flag = dw_transaction_list.GetItemString(li_x1,'split_payment_flag')
		ll_ultimate_txn_no = dw_transaction_list.GetItemNumber(li_x1,'ultimate_txn_no')
		
		
		ll_dw_confirmation_row = dw_confirmation.insertrow(0)	
				
		if ldcm_txn_amount = (ldcm_txn_amount_entered * -1)  then
			lb_full_amount = true
		else
			lb_full_amount = false
		end if
		
		/* get the admin region */
		if wf_assign_admin_region(ll_claim_no,ls_admin_region_code) < 0 then 
			messagebox("Warning","Can't assign admin region form this claim",StopSign!)
			return -1
		end if
		
		/* orginal payment/txn - source */
		ll_claim_no_org = ll_claim_no
		
		dw_confirmation.setitem(ll_dw_confirmation_row,'payment_no',	ll_payment_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'payment_type_code',ls_payment_type_code)
		dw_confirmation.setitem(ll_dw_confirmation_row,'txn_type_code',ls_old_txn_type_code)
		dw_confirmation.setitem(ll_dw_confirmation_row,'txn_sub_type_code',ls_old_txn_sub_type_code)
		dw_confirmation.setitem(ll_dw_confirmation_row,'payment_type_code',ls_payment_type_code)
		dw_confirmation.setitem(ll_dw_confirmation_row,'payment_sub_type_code',ls_payment_sub_type_code)
		dw_confirmation.setitem(ll_dw_confirmation_row,'claim_no',ll_claim_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'total_payment_amount',ldcm_total_payment_amount)
		dw_confirmation.setitem(ll_dw_confirmation_row,'txn_no',	ll_txn_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'related_txn_no',ll_related_txn_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_no',ll_cost_alloc_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_operation_no',ll_cost_alloc_operation_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_type_code',ls_cost_alloc_type_code)
		dw_confirmation.setitem(ll_dw_confirmation_row,'processed_date',ldtm_processed_date )
		dw_confirmation.setitem(ll_dw_confirmation_row,'indicator',"Z")
		dw_confirmation.setitem(ll_dw_confirmation_row,'message',"source")
		dw_confirmation.setitem(ll_dw_confirmation_row,'group_indicator',ll_row)
		dw_confirmation.setitem(ll_dw_confirmation_row,'recipient_sub_type_code',ls_recipient_sub_type_code)   
		dw_confirmation.setitem(ll_dw_confirmation_row,'manual_cheque_req_no',ll_manual_cheque_req_no)  
		dw_confirmation.setitem(ll_dw_confirmation_row,'cheque_deposit_date',ldtm_cheque_deposit_date)    
		dw_confirmation.setitem(ll_dw_confirmation_row,'recipient_name',ls_recipient_name)   
		dw_confirmation.setitem(ll_dw_confirmation_row,'address_line1',ls_address_line1)  
		dw_confirmation.setitem(ll_dw_confirmation_row,'address_line2',ls_address_line2)  
		dw_confirmation.setitem(ll_dw_confirmation_row,'city',ls_city)  
		dw_confirmation.setitem(ll_dw_confirmation_row,'prov_state_code',ls_prov_state_code)   
		dw_confirmation.setitem(ll_dw_confirmation_row,'country',ls_country)  
		dw_confirmation.setitem(ll_dw_confirmation_row,'postal_code',ls_postal_code)   
		dw_confirmation.setitem(ll_dw_confirmation_row,'use_default_address_flag',ls_use_default_address_flag)   
		dw_confirmation.setitem(ll_dw_confirmation_row,'cheque_print_group_code',ls_cheque_print_group_code)
		dw_confirmation.setitem(ll_dw_confirmation_row,'recipient_no',ll_recipient_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'recipient_type_code',ls_recipient_type_code)
		dw_confirmation.setitem(ll_dw_confirmation_row,'cheque_no',ll_cheque_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'direct_deposit_xmit_no',ll_direct_deposit_xmit_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'payment_method_code',ls_payment_method_code)
		dw_confirmation.setitem(ll_dw_confirmation_row,'admin_region_code',ls_admin_region_code)	
		dw_confirmation.setitem(ll_dw_confirmation_row,'maintain_allowed_flag',"N")
		dw_confirmation.setitem(ll_dw_confirmation_row,'final_payment_flag',ls_final_payment_flag)
		dw_confirmation.setitem(ll_dw_confirmation_row,'paid_from_date',ldtm_paid_from_date)
		dw_confirmation.setitem(ll_dw_confirmation_row,'paid_to_date',ldtm_paid_to_date)
		dw_confirmation.setitem(ll_dw_confirmation_row,'authorization_no',ll_authorization_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'authorized_date',ldtm_authorized_date)
		dw_confirmation.setitem(ll_dw_confirmation_row,'authorized_by_code',ls_authorized_by_code)
		dw_confirmation.setitem(ll_dw_confirmation_row,'payment_adjustment_flag',ls_payment_adjustment_flag)
		dw_confirmation.setitem(ll_dw_confirmation_row,'txn_unit_of_work_no',ll_txn_unit_of_work_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'claim_no_original',ll_claim_no_org)
		dw_confirmation.setitem(ll_dw_confirmation_row,'tax_rate',ldcm_tax_rate)
		dw_confirmation.setitem(ll_dw_confirmation_row,'award_no',ll_award_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'txn_amount',ldcm_txn_amount)	
		dw_confirmation.setitem(ll_dw_confirmation_row,'txn_tax_amount',ldcm_txn_tax_amount)
		dw_confirmation.setitem(ll_dw_confirmation_row,'net_quantity',ll_net_quantity)
		dw_confirmation.setitem(ll_dw_confirmation_row,'net_days_lost',ldcm_net_days_lost)
		dw_confirmation.setitem(ll_dw_confirmation_row,'net_hours_lost',ldcm_net_hours_lost)					
		dw_confirmation.setitem(ll_dw_confirmation_row,'adjustment_rehab_qty',ll_net_quantity)
		dw_confirmation.setitem(ll_dw_confirmation_row,'annuity_set_a_side',ls_annuity_set_a_side)
		dw_confirmation.setitem(ll_dw_confirmation_row,'coc_period',ll_coc_period)
		dw_confirmation.setitem(ll_dw_confirmation_row,'explanation',ls_explanation)
		dw_confirmation.setitem(ll_dw_confirmation_row,'benefit_calculation_no',il_benefit_calculation_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'opening_no',ll_opening_no)
		dw_confirmation.SetItem(ll_dw_confirmation_row,'split_payment_flag',ls_split_payment_flag)
		dw_confirmation.SetItem(ll_dw_confirmation_row,'ultimate_txn_no',ll_ultimate_txn_no)
	
		IF ls_split_payment_flag = 'Y' THEN
			ls_find = 'payment_no = ' + String(ll_payment_no) + ' and ultimate_txn_no <> ' + String(ll_ultimate_txn_no)
			ll_found = dw_confirmation.Find(ls_find,ll_found + 1,dw_confirmation.RowCount())
			IF ll_found > 0 THEN
				// found something already, so increment split_seq_no & var
				li_seq_no = li_seq_no + 1
				dw_confirmation.SetItem(ll_dw_confirmation_row,'split_seq_no',li_seq_no)
			ELSE
				// nothing was found, therefore nothing has been inserted yet, this is the first set of txns for split payment
				li_seq_no = 1
				dw_confirmation.SetItem(ll_dw_confirmation_row,'split_seq_no',1)
			END IF
		ELSE
			li_seq_no = 0
			// for clarity - split payments (above) don't display days/hrs in 'original' txn
			dw_confirmation.setitem(ll_dw_confirmation_row,'adjustment_days_lost',ldcm_net_days_lost)
			dw_confirmation.setitem(ll_dw_confirmation_row,'adjustment_hours_lost',ldcm_net_hours_lost)	
		END IF
		
	
	
		/* CREATE reversal txn - always required*/	
		ll_temp_txn_no++
		ll_dw_confirmation_row = dw_confirmation.insertrow(0)
		dw_confirmation.setitem(ll_dw_confirmation_row,'payment_no',	ll_payment_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'payment_type_code',ls_payment_type_code)
		dw_confirmation.setitem(ll_dw_confirmation_row,'payment_sub_type_code',ls_payment_sub_type_code)
		dw_confirmation.setitem(ll_dw_confirmation_row,'claim_no',ll_claim_no)	
		dw_confirmation.setitem(ll_dw_confirmation_row,'txn_no',	ll_temp_txn_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'txn_type_code',ls_new_txn_type_code)
		dw_confirmation.setitem(ll_dw_confirmation_row,'txn_sub_type_code',ls_new_txn_sub_type_code)
		dw_confirmation.setitem(ll_dw_confirmation_row,'related_txn_no',ll_txn_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_no',ll_cost_alloc_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_operation_no',ll_cost_alloc_operation_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_type_code',ls_cost_alloc_type_code)
		dw_confirmation.setitem(ll_dw_confirmation_row,'total_payment_amount',ldcm_total_payment_amount)
		dw_confirmation.setitem(ll_dw_confirmation_row,'indicator',"Z")
		dw_confirmation.setitem(ll_dw_confirmation_row,'message',"reversal")
		dw_confirmation.setitem(ll_dw_confirmation_row,'group_indicator',ll_row)
		dw_confirmation.setitem(ll_dw_confirmation_row,'recipient_sub_type_code',ls_recipient_sub_type_code)   
		dw_confirmation.setitem(ll_dw_confirmation_row,'manual_cheque_req_no',ll_manual_cheque_req_no)  
		dw_confirmation.setitem(ll_dw_confirmation_row,'cheque_deposit_date',ldtm_cheque_deposit_date)    
		dw_confirmation.setitem(ll_dw_confirmation_row,'recipient_name',ls_recipient_name)   
		dw_confirmation.setitem(ll_dw_confirmation_row,'address_line1',ls_address_line1)  
		dw_confirmation.setitem(ll_dw_confirmation_row,'address_line2',ls_address_line2)  
		dw_confirmation.setitem(ll_dw_confirmation_row,'city',ls_city)  
		dw_confirmation.setitem(ll_dw_confirmation_row,'prov_state_code',ls_prov_state_code)   
		dw_confirmation.setitem(ll_dw_confirmation_row,'country',ls_country)  
		dw_confirmation.setitem(ll_dw_confirmation_row,'postal_code',ls_postal_code)   
		dw_confirmation.setitem(ll_dw_confirmation_row,'use_default_address_flag',ls_use_default_address_flag)   
		dw_confirmation.setitem(ll_dw_confirmation_row,'cheque_print_group_code',ls_cheque_print_group_code)
		dw_confirmation.setitem(ll_dw_confirmation_row,'recipient_no',ll_recipient_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'recipient_type_code',ls_recipient_type_code)
		dw_confirmation.setitem(ll_dw_confirmation_row,'cheque_no',ll_cheque_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'direct_deposit_xmit_no',ll_direct_deposit_xmit_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'payment_method_code',ls_payment_method_code)
		dw_confirmation.setitem(ll_dw_confirmation_row,'admin_region_code',ls_admin_region_code)	
		dw_confirmation.setitem(ll_dw_confirmation_row,'maintain_allowed_flag',"N")
		dw_confirmation.setitem(ll_dw_confirmation_row,'final_payment_flag',ls_final_payment_flag)
		dw_confirmation.setitem(ll_dw_confirmation_row,'paid_from_date',ldtm_paid_from_date)
		dw_confirmation.setitem(ll_dw_confirmation_row,'paid_to_date',ldtm_paid_to_date)
		dw_confirmation.setitem(ll_dw_confirmation_row,'authorization_no',ll_authorization_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'authorized_date',ldtm_authorized_date)
		dw_confirmation.setitem(ll_dw_confirmation_row,'authorized_by_code',ls_authorized_by_code)
		dw_confirmation.setitem(ll_dw_confirmation_row,'payment_adjustment_flag',ls_payment_adjustment_flag)	
		dw_confirmation.setitem(ll_dw_confirmation_row,'claim_no_original',ll_claim_no_org)
		dw_confirmation.setitem(ll_dw_confirmation_row,'tax_rate',ldcm_tax_rate)
		dw_confirmation.setitem(ll_dw_confirmation_row,'award_no',ll_award_no)	
		dw_confirmation.setitem(ll_dw_confirmation_row,'txn_amount',ldcm_txn_amount * -1)	
		dw_confirmation.setitem(ll_dw_confirmation_row,'txn_tax_amount',ldcm_txn_tax_amount * -1)
		dw_confirmation.setitem(ll_dw_confirmation_row,'net_quantity',ll_net_quantity * -1)
		dw_confirmation.setitem(ll_dw_confirmation_row,'net_days_lost',ldcm_net_days_lost * -1)
		dw_confirmation.setitem(ll_dw_confirmation_row,'net_hours_lost',ldcm_net_hours_lost * -1)					
		dw_confirmation.setitem(ll_dw_confirmation_row,'adjustment_rehab_qty',ll_net_quantity * -1)
		dw_confirmation.SetItem(ll_dw_confirmation_row,'split_seq_no',li_seq_no)
		
		IF ls_split_payment_flag = 'Y' THEN
			IF li_seq_no = 1 THEN
				dw_confirmation.setitem(ll_dw_confirmation_row,'adjustment_days_lost',ldcm_net_days_lost * -1)
				dw_confirmation.setitem(ll_dw_confirmation_row,'adjustment_hours_lost',ldcm_net_hours_lost * -1)
			ELSEIF li_seq_no > 1 THEN
				// find the other record created for the payment & use the last value for the creation of the reversal
				ls_find = 'payment_no = ' + String(ll_payment_no) + ' and ultimate_txn_no <> ' + String(ll_ultimate_txn_no) + ' and first_remainder_txn_flag = "Y" and split_seq_no = ' + String(li_seq_no - 1)
				ll_found = -1
				DO WHILE ll_found <> 0
					ll_found = dw_confirmation.Find(ls_find,ll_found + 1,dw_confirmation.RowCount())
					IF ll_found > 0 THEN
						IF ll_found = ll_old_found OR ll_found = ll_dw_confirmation_row THEN
							// it found the same record already, or it found itself
							EXIT
						END IF
						// if the value of li_seq_no > 1, then should always find a record
						IF ll_found > ll_old_found THEN
							// if the record# is greater, save the value
							ll_old_found = ll_found
						END IF
					END IF
				LOOP
				 
				ldcm_new_net_days_lost = dw_confirmation.GetItemDecimal(ll_old_found,'adjustment_days_lost')
				ldcm_new_net_hours_lost = dw_confirmation.GetItemDecimal(ll_old_found,'adjustment_hours_lost')
				dw_confirmation.setitem(ll_dw_confirmation_row,'adjustment_days_lost',ldcm_new_net_days_lost * -1)
				dw_confirmation.setitem(ll_dw_confirmation_row,'adjustment_hours_lost',ldcm_new_net_hours_lost * -1)
			END IF
		ELSE
			dw_confirmation.setitem(ll_dw_confirmation_row,'adjustment_days_lost',ldcm_net_days_lost * -1)
			dw_confirmation.setitem(ll_dw_confirmation_row,'adjustment_hours_lost',ldcm_net_hours_lost * -1)			
		END IF
		
		dw_confirmation.setitem(ll_dw_confirmation_row,'annuity_set_a_side',ls_annuity_set_a_side)
		dw_confirmation.setitem(ll_dw_confirmation_row,'new_txn_created',"Y")
		dw_confirmation.setitem(ll_dw_confirmation_row,'coc_period',ll_coc_period)
		dw_confirmation.setitem(ll_dw_confirmation_row,'explanation',ls_explanation)
		dw_confirmation.setitem(ll_dw_confirmation_row,'benefit_calculation_no',il_benefit_calculation_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'opening_no',ll_opening_no)
		dw_confirmation.SetItem(ll_dw_confirmation_row,'split_payment_flag',ls_split_payment_flag)
		dw_confirmation.SetItem(ll_dw_confirmation_row,'ultimate_txn_no',ll_ultimate_txn_no)
		dw_confirmation.SetItem(ll_dw_confirmation_row,'first_remainder_txn_flag','N')
		
	
		/* if full amount is not transfered or adjusted - need to keep partial transaction*/
		if lb_full_amount = false then
	
			ll_temp_txn_no++
					
			ll_dw_confirmation_row = dw_confirmation.insertrow(0)
			dw_confirmation.setitem(ll_dw_confirmation_row,'payment_no',	ll_payment_no)
			dw_confirmation.setitem(ll_dw_confirmation_row,'payment_type_code',ls_payment_type_code)
			dw_confirmation.setitem(ll_dw_confirmation_row,'payment_sub_type_code',ls_payment_sub_type_code)
			dw_confirmation.setitem(ll_dw_confirmation_row,'claim_no',ll_claim_no)	
			dw_confirmation.setitem(ll_dw_confirmation_row,'txn_no',ll_temp_txn_no)
			dw_confirmation.setitem(ll_dw_confirmation_row,'txn_type_code',ls_new_txn_type_code)
			dw_confirmation.setitem(ll_dw_confirmation_row,'txn_sub_type_code',ls_new_txn_sub_type_code)
			dw_confirmation.setitem(ll_dw_confirmation_row,'related_txn_no',ll_txn_no)		
			dw_confirmation.setitem(ll_dw_confirmation_row,'total_payment_amount',ldcm_total_payment_amount)
			dw_confirmation.setitem(ll_dw_confirmation_row,'indicator',"Z")
			dw_confirmation.setitem(ll_dw_confirmation_row,'message',"current")
			dw_confirmation.setitem(ll_dw_confirmation_row,'group_indicator',ll_row)
			dw_confirmation.setitem(ll_dw_confirmation_row,'recipient_sub_type_code',ls_recipient_sub_type_code)   
			dw_confirmation.setitem(ll_dw_confirmation_row,'manual_cheque_req_no',ll_manual_cheque_req_no)  
			dw_confirmation.setitem(ll_dw_confirmation_row,'cheque_deposit_date',ldtm_cheque_deposit_date)    
			dw_confirmation.setitem(ll_dw_confirmation_row,'recipient_name',ls_recipient_name)   
			dw_confirmation.setitem(ll_dw_confirmation_row,'address_line1',ls_address_line1)  
			dw_confirmation.setitem(ll_dw_confirmation_row,'address_line2',ls_address_line2)  
			dw_confirmation.setitem(ll_dw_confirmation_row,'city',ls_city)  
			dw_confirmation.setitem(ll_dw_confirmation_row,'prov_state_code',ls_prov_state_code)   
			dw_confirmation.setitem(ll_dw_confirmation_row,'country',ls_country)  
			dw_confirmation.setitem(ll_dw_confirmation_row,'postal_code',ls_postal_code)   
			dw_confirmation.setitem(ll_dw_confirmation_row,'use_default_address_flag',ls_use_default_address_flag)   
			dw_confirmation.setitem(ll_dw_confirmation_row,'cheque_print_group_code',ls_cheque_print_group_code)
			dw_confirmation.setitem(ll_dw_confirmation_row,'recipient_no',ll_recipient_no)
			dw_confirmation.setitem(ll_dw_confirmation_row,'recipient_type_code',ls_recipient_type_code)
			dw_confirmation.setitem(ll_dw_confirmation_row,'cheque_no',ll_cheque_no)
			dw_confirmation.setitem(ll_dw_confirmation_row,'direct_deposit_xmit_no',ll_direct_deposit_xmit_no)
			dw_confirmation.setitem(ll_dw_confirmation_row,'payment_method_code',ls_payment_method_code)
			dw_confirmation.setitem(ll_dw_confirmation_row,'admin_region_code',ls_admin_region_code)	
			dw_confirmation.setitem(ll_dw_confirmation_row,'maintain_allowed_flag',"Y")
			dw_confirmation.setitem(ll_dw_confirmation_row,'final_payment_flag',ls_final_payment_flag)
			dw_confirmation.setitem(ll_dw_confirmation_row,'paid_from_date',ldtm_paid_from_date)
			dw_confirmation.setitem(ll_dw_confirmation_row,'paid_to_date',ldtm_paid_to_date)
			dw_confirmation.setitem(ll_dw_confirmation_row,'authorization_no',ll_authorization_no)
			dw_confirmation.setitem(ll_dw_confirmation_row,'authorized_date',ldtm_authorized_date)
			dw_confirmation.setitem(ll_dw_confirmation_row,'authorized_by_code',ls_authorized_by_code)
			dw_confirmation.setitem(ll_dw_confirmation_row,'payment_adjustment_flag',ls_payment_adjustment_flag)					
			dw_confirmation.setitem(ll_dw_confirmation_row,'claim_no_original',ll_claim_no_org)
			dw_confirmation.setitem(ll_dw_confirmation_row,'tax_rate',ldcm_tax_rate)
			dw_confirmation.setitem(ll_dw_confirmation_row,'award_no',ll_award_no)
			dw_confirmation.setitem(ll_dw_confirmation_row,'txn_amount',ldcm_txn_amount + ldcm_txn_amount_entered )
			dw_confirmation.setitem(ll_dw_confirmation_row,'txn_tax_amount',ldcm_txn_tax_amount + ldcm_tax_amount_entered )
			dw_confirmation.SetItem(ll_dw_confirmation_row,'split_payment_flag',ls_split_payment_flag)
			dw_confirmation.SetItem(ll_dw_confirmation_row,'ultimate_txn_no',ll_ultimate_txn_no)
			dw_confirmation.SetItem(ll_dw_confirmation_row,'split_seq_no',li_seq_no)
			dw_confirmation.SetItem(ll_dw_confirmation_row,'first_remainder_txn_flag','Y')
			
		
			if (ll_net_quantity_entered + ll_net_quantity = 0  ) and ll_authorization_no > 0 then
				if ls_new_txn_type_code = 'T' AND ls_new_txn_sub_type_code = '6' then
					dw_confirmation.setitem(ll_dw_confirmation_row,'adjustment_rehab_qty',1)
				else
					dw_confirmation.setitem(ll_dw_confirmation_row,'adjustment_rehab_qty',ll_net_quantity_entered + ll_net_quantity )
				end if
			else
				dw_confirmation.setitem(ll_dw_confirmation_row,'adjustment_rehab_qty',ll_net_quantity_entered + ll_net_quantity )
			end if
			
			IF li_seq_no > 1 AND ls_split_payment_flag = 'Y' THEN
				// split payment, this is at least 2nd split being adjusted
				dw_confirmation.setitem(ll_dw_confirmation_row,'adjustment_days_lost',	ldcm_net_days_lost_entered + ldcm_new_net_days_lost)
				dw_confirmation.setitem(ll_dw_confirmation_row,'adjustment_hours_lost',	ldcm_net_hours_lost_entered + ldcm_new_net_hours_lost)
			ELSE
				dw_confirmation.setitem(ll_dw_confirmation_row,'adjustment_days_lost',	ldcm_net_days_lost_entered + ldcm_net_days_lost)
				dw_confirmation.setitem(ll_dw_confirmation_row,'adjustment_hours_lost',	ldcm_net_hours_lost_entered + ldcm_net_hours_lost)
			END IF
			dw_confirmation.setitem(ll_dw_confirmation_row,'annuity_set_a_side',ls_annuity_set_a_side)
			dw_confirmation.setitem(ll_dw_confirmation_row,'new_txn_created',"Y")		
			dw_confirmation.setitem(ll_dw_confirmation_row,'coc_period',ll_coc_period)
			dw_confirmation.setitem(ll_dw_confirmation_row,'explanation',ls_explanation)
			dw_confirmation.setitem(ll_dw_confirmation_row,'benefit_calculation_no',il_benefit_calculation_no)
			dw_confirmation.setitem(ll_dw_confirmation_row,'opening_no',ll_opening_no)
			dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_no',ll_cost_alloc_no)
			dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_operation_no',ll_cost_alloc_operation_no)	
			dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_type_code',ls_cost_alloc_type_code)	

			
			if ls_cost_alloc_required_flag = "N" then
				dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_no',0)
				dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_operation_no',0)	
				dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_type_code','')
			ELSE
		
				if isnull(ll_cost_alloc_no) or ll_cost_alloc_no = 0 then	
					if wf_determine_cost_allocation(ll_related_txn_no,ll_cost_alloc_no,ll_cost_alloc_operation_no,ls_cost_alloc_type_code) < 0 then 
						messagebox("ERROR","Found integrity issue around having no cost allocation when one is required")
						return -1
					end if
					dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_no',ll_cost_alloc_no)
					dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_operation_no',ll_cost_alloc_operation_no)	
					dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_type_code',ls_cost_alloc_type_code)			
				end if
				
			end if	
			
		else
			ll_temp_txn_no++
			/* full amount is adjusted or transfered */
			ll_dw_confirmation_row = dw_confirmation.insertrow(0)
			dw_confirmation.setitem(ll_dw_confirmation_row,'payment_no',	ll_payment_no)
			dw_confirmation.setitem(ll_dw_confirmation_row,'payment_type_code',ls_payment_type_code)
			dw_confirmation.setitem(ll_dw_confirmation_row,'payment_sub_type_code',ls_payment_sub_type_code)
			dw_confirmation.setitem(ll_dw_confirmation_row,'claim_no',ll_claim_no)	
			dw_confirmation.setitem(ll_dw_confirmation_row,'txn_no',	ll_temp_txn_no)
			dw_confirmation.setitem(ll_dw_confirmation_row,'txn_type_code',ls_new_txn_type_code)
			dw_confirmation.setitem(ll_dw_confirmation_row,'txn_sub_type_code',ls_new_txn_sub_type_code)
			dw_confirmation.setitem(ll_dw_confirmation_row,'related_txn_no',ll_txn_no)
			dw_confirmation.setitem(ll_dw_confirmation_row,'total_payment_amount',ldcm_total_payment_amount)
			dw_confirmation.setitem(ll_dw_confirmation_row,'indicator',"Z")
			dw_confirmation.setitem(ll_dw_confirmation_row,'group_indicator',ll_row)
			dw_confirmation.setitem(ll_dw_confirmation_row,'txn_amount',0)
			dw_confirmation.setitem(ll_dw_confirmation_row,'txn_tax_amount',0)
			dw_confirmation.setitem(ll_dw_confirmation_row,'recipient_sub_type_code',ls_recipient_sub_type_code)   
			dw_confirmation.setitem(ll_dw_confirmation_row,'manual_cheque_req_no',ll_manual_cheque_req_no)  
			dw_confirmation.setitem(ll_dw_confirmation_row,'cheque_deposit_date',ldtm_cheque_deposit_date)    
			dw_confirmation.setitem(ll_dw_confirmation_row,'recipient_name',ls_recipient_name)   
			dw_confirmation.setitem(ll_dw_confirmation_row,'address_line1',ls_address_line1)  
			dw_confirmation.setitem(ll_dw_confirmation_row,'address_line2',ls_address_line2)  
			dw_confirmation.setitem(ll_dw_confirmation_row,'city',ls_city)  
			dw_confirmation.setitem(ll_dw_confirmation_row,'prov_state_code',ls_prov_state_code)   
			dw_confirmation.setitem(ll_dw_confirmation_row,'country',ls_country)  
			dw_confirmation.setitem(ll_dw_confirmation_row,'postal_code',ls_postal_code)   
			dw_confirmation.setitem(ll_dw_confirmation_row,'use_default_address_flag',ls_use_default_address_flag)   
			dw_confirmation.setitem(ll_dw_confirmation_row,'cheque_print_group_code',ls_cheque_print_group_code)
			dw_confirmation.setitem(ll_dw_confirmation_row,'recipient_no',ll_recipient_no)
			dw_confirmation.setitem(ll_dw_confirmation_row,'recipient_type_code',ls_recipient_type_code)
			dw_confirmation.setitem(ll_dw_confirmation_row,'cheque_no',ll_cheque_no)
			dw_confirmation.setitem(ll_dw_confirmation_row,'direct_deposit_xmit_no',ll_direct_deposit_xmit_no)
			dw_confirmation.setitem(ll_dw_confirmation_row,'payment_method_code',ls_payment_method_code)
			dw_confirmation.setitem(ll_dw_confirmation_row,'admin_region_code',ls_admin_region_code)
			dw_confirmation.SetItem(ll_dw_confirmation_row,'split_payment_flag',ls_split_payment_flag)
			dw_confirmation.SetItem(ll_dw_confirmation_row,'ultimate_txn_no',ll_ultimate_txn_no)
			dw_confirmation.SetItem(ll_dw_confirmation_row,'split_seq_no',li_seq_no)
			dw_confirmation.SetItem(ll_dw_confirmation_row,'first_remainder_txn_flag','Y')
			
			
			/* SETTING THE MAINTAIN ALLOWED FLAG - THE EXCEPTIONS */
			/* BR 1.30 */
			IF ls_maintain_zero_allowed_flag = "Y" THEN	
				dw_confirmation.setitem(ll_dw_confirmation_row,'message',"current")
	
				/* All adjustment where maintain_zero_allowed_flag = "Y" */
				IF ls_new_txn_type_code = 'J' THEN
					dw_confirmation.setitem(ll_dw_confirmation_row,'maintain_allowed_flag',"Y")
				/* TRANSFERS WHERE THERE IS AN AMOUNT LEFT OVER */
				ELSEIF ls_new_txn_type_code = 'T' THEN
					
					SELECT isnull(sum(dbo.APPLIED_CLAIM_TXN.txn_amount),0.00)
					INTO :ldcm_total_txn_amount
					FROM dbo.APPLIED_CLAIM_TXN ,
						  dbo.COST_OF_CLAIMS_ALLOCATED
					WHERE 	( dbo.APPLIED_CLAIM_TXN.txn_no 									= dbo.COST_OF_CLAIMS_ALLOCATED.txn_no ) and
								( dbo.APPLIED_CLAIM_TXN.claim_no 								= :ll_claim_no ) AND  
								( dbo.APPLIED_CLAIM_TXN.payment_no 								= :ll_payment_no ) AND  
								( dbo.COST_OF_CLAIMS_ALLOCATED.cost_alloc_no 				= :ll_cost_alloc_no)		  AND 
								( dbo.COST_OF_CLAIMS_ALLOCATED.cost_alloc_operation_no 	= :ll_cost_alloc_operation_no)  AND 
								( dbo.APPLIED_CLAIM_TXN.recipient_no 							= :ll_recipient_no )  AND 
								( dbo.APPLIED_CLAIM_TXN.recipient_type_code     			= :ls_recipient_type_code ) AND
								(( dbo.APPLIED_CLAIM_TXN.txn_type_code = 'J'            and dbo.APPLIED_CLAIM_TXN.txn_sub_type_code in(3,5)))					
				 USING SQLCA;
				 SQLCA.nf_handle_error("select dbo.APPLIED_CLAIM_TXN ",is_window_name,ls_script) 
				 IF ldcm_total_txn_amount < 0  THEN
					dw_confirmation.setitem(ll_dw_confirmation_row,'maintain_allowed_flag',"Y")
				 ELSE	
					dw_confirmation.setitem(ll_dw_confirmation_row,'maintain_allowed_flag',"N")
				 end if
			  ELSE
				 dw_confirmation.setitem(ll_dw_confirmation_row,'maintain_allowed_flag',"N")
			  END IF
				
			ELSE
				dw_confirmation.setitem(ll_dw_confirmation_row,'maintain_allowed_flag',"N")
				dw_confirmation.setitem(ll_dw_confirmation_row,'message',"zeroed")
			END IF
			
			
			
			dw_confirmation.setitem(ll_dw_confirmation_row,'final_payment_flag',ls_final_payment_flag)
			dw_confirmation.setitem(ll_dw_confirmation_row,'paid_from_date',ldtm_paid_from_date)
			dw_confirmation.setitem(ll_dw_confirmation_row,'paid_to_date',ldtm_paid_to_date)
			dw_confirmation.setitem(ll_dw_confirmation_row,'authorization_no',ll_authorization_no)
			dw_confirmation.setitem(ll_dw_confirmation_row,'authorized_date',ldtm_authorized_date)
			dw_confirmation.setitem(ll_dw_confirmation_row,'authorized_by_code',ls_authorized_by_code)
			dw_confirmation.setitem(ll_dw_confirmation_row,'payment_adjustment_flag',ls_payment_adjustment_flag)
			dw_confirmation.setitem(ll_dw_confirmation_row,'adjustment_rehab_qty',0)	
			dw_confirmation.setitem(ll_dw_confirmation_row,'claim_no_original',ll_claim_no_org)
			dw_confirmation.setitem(ll_dw_confirmation_row,'tax_rate',ldcm_tax_rate)				
			dw_confirmation.setitem(ll_dw_confirmation_row,'award_no',ll_award_no)
			dw_confirmation.setitem(ll_dw_confirmation_row,'adjustment_rehab_qty',ll_net_quantity_entered + ll_net_quantity )
			IF li_seq_no > 1 AND ls_split_payment_flag = 'Y' THEN
				// split payment, this is at least 2nd split being adjusted
				dw_confirmation.setitem(ll_dw_confirmation_row,'adjustment_days_lost',	ldcm_new_net_days_lost - (ldcm_net_days_lost_entered * -1))
				dw_confirmation.setitem(ll_dw_confirmation_row,'adjustment_hours_lost',	ldcm_new_net_hours_lost - (ldcm_net_hours_lost_entered * -1))
			ELSE
				dw_confirmation.setitem(ll_dw_confirmation_row,'adjustment_days_lost',ldcm_net_days_lost_entered + ldcm_net_days_lost)
				dw_confirmation.setitem(ll_dw_confirmation_row,'adjustment_hours_lost',ldcm_net_hours_lost_entered + ldcm_net_hours_lost)
			END IF
			dw_confirmation.setitem(ll_dw_confirmation_row,'annuity_set_a_side',ls_annuity_set_a_side)
			dw_confirmation.setitem(ll_dw_confirmation_row,'new_txn_created',"Y")
			dw_confirmation.setitem(ll_dw_confirmation_row,'coc_period',ll_coc_period)
			dw_confirmation.setitem(ll_dw_confirmation_row,'explanation',ls_explanation)
			dw_confirmation.setitem(ll_dw_confirmation_row,'benefit_calculation_no',il_benefit_calculation_no)
			dw_confirmation.setitem(ll_dw_confirmation_row,'opening_no',ll_opening_no)
			dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_no',0)
			dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_operation_no',0)
			dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_type_code',"")								
		
		end if
		
		if ls_new_txn_type_code = "J"  then		
			Continue /* next row */
		end if
		
	
		/* The 'indicator' column is used to determine if a new payment is required */
		/* insert blank row - new transaction required */
		ll_temp_txn_no++
			
		ll_dw_confirmation_row = dw_confirmation.insertrow(0)
		dw_confirmation.setitem(ll_dw_confirmation_row,'claim_no',ll_claim_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'payment_type_code',ls_payment_type_code)
		dw_confirmation.setitem(ll_dw_confirmation_row,'payment_sub_type_code',ls_payment_sub_type_code)
		dw_confirmation.setitem(ll_dw_confirmation_row,'indicator',"Z")
		dw_confirmation.setitem(ll_dw_confirmation_row,'claim_no_original',ll_claim_no_org)
			
	
		/* payment type and claim transfer require a new payment to created */
		if ls_new_txn_type_code = "T" and (ls_new_txn_sub_type_code = "6" or ls_new_txn_sub_type_code = "9") then
			dw_confirmation.setitem(ll_dw_confirmation_row,'payment_no',ll_dw_confirmation_row)
			dw_confirmation.setitem(ll_dw_confirmation_row,'indicator',"N")	
		
			if ls_new_txn_sub_type_code = "6"	then
				dw_confirmation.setitem(ll_dw_confirmation_row,'claim_no',ll_claim_no_to)
				
				/* must get the admin region of claim to */
				if wf_assign_admin_region(ll_claim_no_to,ls_admin_region_code) < 0 then 
					messagebox("Warning","Can't assign admin region for claim")
					return -1
				end if
							
			end if
			
			if ls_new_txn_sub_type_code = "9"	then
				dw_confirmation.setitem(ll_dw_confirmation_row,'payment_type_code',ls_payment_type_code_to)
				dw_confirmation.setitem(ll_dw_confirmation_row,'payment_sub_type_code',ls_payment_sub_type_code_to)
			end if
			
		else
			/* no need for new payment to be created - only txn */
			dw_confirmation.setitem(ll_dw_confirmation_row,'payment_no',ll_payment_no)	
		end if
			
			
		/* cost allocation & cost relief transfer - use the new cost allocation "to" */
		if ls_new_txn_type_code = "T"  then
			
			dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_no',ll_cost_alloc_no_to)
			dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_operation_no',ll_cost_alloc_operation_no_to)
					
			if ll_cost_alloc_no_to > 0 then
			/* assign to cost allocation type from 'EMPLOYER' */
				IF wf_assign_cost_alloc_type_code(ll_cost_alloc_no_to,ls_cost_alloc_type_code_to ) < 0 then 
					messagebox("Warning","Can't assign cost alloction type for cost allocation " + STRING(ll_cost_alloc_no_to),StopSign!)
					return -1
				else
					dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_type_code',ls_cost_alloc_type_code_to)
				end if			
			end if
	
		else
			if ldcm_total_payment_amount = 0.00 then
				dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_no',0)
				dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_operation_no',0)	
				dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_type_code',"")
			else
				dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_no',ll_cost_alloc_no)
				dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_operation_no',ll_cost_alloc_operation_no)	
				dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_type_code',ls_cost_alloc_type_code)
			end if
		end if
		
		dw_confirmation.setitem(ll_dw_confirmation_row,'txn_type_code',ls_new_txn_type_code)
		dw_confirmation.setitem(ll_dw_confirmation_row,'txn_sub_type_code',ls_new_txn_sub_type_code)
		dw_confirmation.setitem(ll_dw_confirmation_row,'txn_no',	ll_temp_txn_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'related_txn_no',ll_txn_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'total_payment_amount',ldcm_total_payment_amount)
		dw_confirmation.setitem(ll_dw_confirmation_row,'message',"current")
		dw_confirmation.setitem(ll_dw_confirmation_row,'group_indicator',ll_row)
		dw_confirmation.setitem(ll_dw_confirmation_row,'recipient_sub_type_code',ls_recipient_sub_type_code)   
		dw_confirmation.setitem(ll_dw_confirmation_row,'manual_cheque_req_no',ll_manual_cheque_req_no)  
		dw_confirmation.setitem(ll_dw_confirmation_row,'cheque_deposit_date',ldtm_cheque_deposit_date)    
		dw_confirmation.setitem(ll_dw_confirmation_row,'recipient_name',ls_recipient_name)   
		dw_confirmation.setitem(ll_dw_confirmation_row,'address_line1',ls_address_line1)  
		dw_confirmation.setitem(ll_dw_confirmation_row,'address_line2',ls_address_line2)  
		dw_confirmation.setitem(ll_dw_confirmation_row,'city',ls_city)  
		dw_confirmation.setitem(ll_dw_confirmation_row,'prov_state_code',ls_prov_state_code)   
		dw_confirmation.setitem(ll_dw_confirmation_row,'country',ls_country)  
		dw_confirmation.setitem(ll_dw_confirmation_row,'postal_code',ls_postal_code)   
		dw_confirmation.setitem(ll_dw_confirmation_row,'use_default_address_flag',ls_use_default_address_flag)   
		dw_confirmation.setitem(ll_dw_confirmation_row,'cheque_print_group_code',ls_cheque_print_group_code)
		dw_confirmation.setitem(ll_dw_confirmation_row,'recipient_no',ll_recipient_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'recipient_type_code',ls_recipient_type_code)
		dw_confirmation.setitem(ll_dw_confirmation_row,'cheque_no',ll_cheque_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'direct_deposit_xmit_no',ll_direct_deposit_xmit_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'payment_method_code',ls_payment_method_code)
		dw_confirmation.setitem(ll_dw_confirmation_row,'admin_region_code',ls_admin_region_code)	
		dw_confirmation.setitem(ll_dw_confirmation_row,'maintain_allowed_flag',"Y")
		dw_confirmation.setitem(ll_dw_confirmation_row,'final_payment_flag',ls_final_payment_flag)
		dw_confirmation.setitem(ll_dw_confirmation_row,'paid_from_date',ldtm_paid_from_date)
		dw_confirmation.setitem(ll_dw_confirmation_row,'paid_to_date',ldtm_paid_to_date)
		dw_confirmation.setitem(ll_dw_confirmation_row,'authorized_date',ldtm_authorized_date)
		dw_confirmation.setitem(ll_dw_confirmation_row,'authorized_by_code',ls_authorized_by_code)
		dw_confirmation.setitem(ll_dw_confirmation_row,'payment_adjustment_flag',ls_payment_adjustment_flag)
		dw_confirmation.setitem(ll_dw_confirmation_row,'tax_rate',ldcm_tax_rate)		
		dw_confirmation.setitem(ll_dw_confirmation_row,'award_no',ll_award_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'annuity_set_a_side',ls_annuity_set_a_side)
		dw_confirmation.setitem(ll_dw_confirmation_row,'new_txn_created',"Y")
		dw_confirmation.setitem(ll_dw_confirmation_row,'coc_period',ll_coc_period)
		dw_confirmation.setitem(ll_dw_confirmation_row,'explanation',ls_explanation)
		dw_confirmation.setitem(ll_dw_confirmation_row,'benefit_calculation_no',il_benefit_calculation_no)
		dw_confirmation.setitem(ll_dw_confirmation_row,'opening_no',ll_opening_no)
		dw_confirmation.SetItem(ll_dw_confirmation_row,'split_payment_flag',ls_split_payment_flag)
		dw_confirmation.SetItem(ll_dw_confirmation_row,'split_seq_no',li_seq_no)
		dw_confirmation.SetItem(ll_dw_confirmation_row,'ultimate_txn_no',ll_ultimate_txn_no)
		dw_confirmation.SetItem(ll_dw_confirmation_row,'first_remainder_txn_flag','N')
		IF ls_cost_alloc_required_flag = "N" THEN
			dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_no',0)
			dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_operation_no',0)	
			dw_confirmation.setitem(ll_dw_confirmation_row,'cost_alloc_type_code',"")
		END IF	
		dw_confirmation.setitem(ll_dw_confirmation_row,'adjustment_days_lost',ldcm_net_days_lost_entered * -1  )
		dw_confirmation.setitem(ll_dw_confirmation_row,'adjustment_hours_lost',ldcm_net_hours_lost_entered * -1 )
		dw_confirmation.setitem(ll_dw_confirmation_row,'txn_amount',ldcm_txn_amount_entered * -1 )
		dw_confirmation.setitem(ll_dw_confirmation_row,'txn_tax_amount',ldcm_tax_amount_entered * -1 )
	
		/* set the adjustment amounts accordingly */
		if lb_full_amount = false then
			
			if (ll_net_quantity_entered + ll_net_quantity = 0  ) and  ll_authorization_no > 0 and ls_new_txn_sub_type_code = "6" then
				dw_confirmation.setitem(ll_dw_confirmation_row,'adjustment_rehab_qty',1)
			ELSE
				dw_confirmation.setitem(ll_dw_confirmation_row,'adjustment_rehab_qty',ll_net_quantity_entered * -1)
			end if
		else
			dw_confirmation.setitem(ll_dw_confirmation_row,'adjustment_rehab_qty',ll_net_quantity_entered * -1)
		end if
		
		/* claim transfer link to an rehab authorization will require a new auth no - all others will be carried forward */
		if ls_new_txn_type_code = "T" and ls_new_txn_sub_type_code = "6" THEN
			dw_confirmation.setitem(ll_dw_confirmation_row,'authorization_no',ll_new_authorization_no)
			
		else
			dw_confirmation.setitem(ll_dw_confirmation_row,'authorization_no',ll_authorization_no)
		end if
		
		/* Ensure that opening_no, benefit_calculation_no & award_no are all set to zero where appropriate.
		*/
		IF ls_new_txn_type_code = "T" AND (ls_new_txn_sub_type_code = "6" OR ls_new_txn_sub_type_code = "9") THEN
			dw_confirmation.setitem(ll_dw_confirmation_row,'award_no',0)
			
			IF ls_new_txn_sub_type_code = "6" THEN
				dw_confirmation.setitem(ll_dw_confirmation_row,'opening_no',0)
				dw_confirmation.setitem(ll_dw_confirmation_row,'benefit_calculation_no',0)
			END IF
			
		END IF
	end if
	
NEXT


/* the payment no was assgined a no for grouping on confirm screen, but this will blank it out*/
/* set the payment no to zero for new payment that will be created - visually on screen */
FOR ll_row = 1 to dw_confirmation.rowcount()
	if dw_confirmation.getitemSTRING(ll_row,'indicator') = "N" then
		dw_confirmation.setitem(ll_row,'payment_no',0)
	END IF	
NEXT


if ll_no_checked = 0 then
	MESSAGEBOX("Warning","No rows selected to confirm",Exclamation!)
else
	li_rc = dw_confirmation.RowsCopy(1,dw_confirmation.RowCount(), Primary!, dw_confirmation_summary, 1, Primary!)
	dw_confirmation_summary.setfilter('message <> "source" ')
	dw_confirmation_summary.filter()	
	dw_confirmation_summary.setsort('payment_type_code,payment_sub_type_code')
	dw_confirmation_summary.sort()

	li_rc = dw_confirmation_summary.RowsCopy(1,dw_confirmation_summary.RowCount(), Primary!, dw_confirm_summary_totals, 1, Primary!)
	dw_confirm_summary_totals.visible = true
	dw_confirm_summary_totals.setredraw(true)
	wf_set_defaults()
end if


dw_transaction_list.SetRedraw(true)
/* set pointer back to orginal state */
SetPointer(oldpointer)

inv_ccm.ib_suppress_warnings = true

return 0
end function

protected subroutine wf_calculate_remaining_hours_lost (long al_ultimate_txn_no, ref decimal adec_remaining_ult_hours_lost);decimal {2} ldec_transferred_hours
decimal {2} ldec_paid_hours_lost, ldec_ult_paid_hours_lost
decimal {2} ldec_total_payment_amount
decimal {2} ldec_ult_txn_amount
long			ll_found, ll_rowcount
string			ls_find

SELECT	a.paid_hours_lost,
			a.total_payment_amount,
			b.txn_amount
INTO		:ldec_paid_hours_lost,
			:ldec_total_payment_amount,
			:ldec_ult_txn_amount
FROM		PAYMENT				a ,
			APPLIED_CLAIM_TXN	b
WHERE	a.payment_no	= b.payment_no
AND		b.txn_no			= :al_ultimate_txn_no
USING SQLCA;
SQLCA.nf_handle_error('w_claim_cost_maintenance', '', 'wf_calculate_remaining_hours_lost - Embedded SQL:Select from  PAYMENT') 

ldec_ult_paid_hours_lost = ldec_paid_hours_lost*(ldec_ult_txn_amount/ldec_total_payment_amount)


// determine T6s or T9s amount transferred
ls_find = 'ultimate_txn_no = ' + String(al_ultimate_txn_no) + ' and txn_type_code = "T" and txn_sub_type_code in ("6","9")  and ultimate_txn_no <> txn_no '
ll_rowcount = dw_transaction_list.RowCount()

// initialize
ll_found = -1
DO UNTIL ll_found = 0
	ll_found = dw_transaction_list.Find(ls_find,ll_found+1, ll_rowcount)
	IF ll_found > 0 THEN
		IF ll_found + 1 > ll_rowcount THEN
			EXIT
		ELSE
			ldec_transferred_hours = ldec_transferred_hours + dw_transaction_list.GetItemDecimal(ll_found,'adjustment_hours_lost')
		END IF
	END IF
LOOP

adec_remaining_ult_hours_lost = ldec_transferred_hours + ldec_ult_paid_hours_lost


end subroutine

public subroutine wf_calculate_remaining_days_lost (long al_ultimate_txn_no, ref decimal adec_remaining_ult_days_lost);decimal {2} ldec_transferred_days
decimal {2} ldec_paid_days_lost, ldec_ult_paid_days_lost
decimal {2} ldec_total_payment_amount
decimal {2} ldec_ult_txn_amount
long			ll_found, ll_rowcount
string			ls_find

SELECT	a.paid_days_lost,
			a.total_payment_amount,
			b.txn_amount
INTO		:ldec_paid_days_lost,
			:ldec_total_payment_amount,
			:ldec_ult_txn_amount
FROM		PAYMENT				a ,
			APPLIED_CLAIM_TXN	b
WHERE	a.payment_no	= b.payment_no
AND		b.txn_no			= :al_ultimate_txn_no
USING SQLCA;
SQLCA.nf_handle_error('w_claim_cost_maintenance', '', 'wf_calculate_remaining_days_lost - Embedded SQL:Select from  PAYMENT') 

ldec_ult_paid_days_lost = ldec_paid_days_lost*(ldec_ult_txn_amount/ldec_total_payment_amount)


// determine T6s or T9s amount transferred
ls_find = 'ultimate_txn_no = ' + String(al_ultimate_txn_no) + ' and txn_type_code = "T" and txn_sub_type_code in ("6","9")  and ultimate_txn_no <> txn_no  '
ll_rowcount = dw_transaction_list.RowCount()

// initialize
ll_found = -1
DO UNTIL ll_found = 0
	ll_found = dw_transaction_list.Find(ls_find,ll_found+1, ll_rowcount)
	IF ll_found > 0 THEN
		IF ll_found + 1 > ll_rowcount THEN
			EXIT
		ELSE
			ldec_transferred_days = ldec_transferred_days + dw_transaction_list.GetItemDecimal(ll_found,'adjustment_days_lost')
		END IF
	END IF
LOOP


adec_remaining_ult_days_lost = ldec_transferred_days + ldec_ult_paid_days_lost

end subroutine

private subroutine wf_calculate_remaining_ult_txn_amount (long al_ultimate_txn_no, ref decimal adec_remaining_ult_txn_amount);decimal {2} ldec_ultimate_txn_amount
decimal {2} ldec_transferred_txn_amount

long			ll_found, ll_rowcount
string			ls_find


SELECT	isnull(txn_amount,0) 
INTO		:ldec_ultimate_txn_amount
FROM		APPLIED_CLAIM_TXN
WHERE	txn_no = :al_ultimate_txn_no
USING SQLCA;
SQLCA.nf_handle_error('w_claim_cost_maintenance', '', 'wf_calculate_remaining_ult_txn_amount - Embedded SQL:Select from  PAYMENT') 

// determine T6s or T9s amount transferred
// do not consider 'regular' T6s & T9s as being transferred, i.e., any T6/T9 txn that serves as its own ultimate
ls_find = 'ultimate_txn_no = ' + String(al_ultimate_txn_no) + ' and txn_type_code = "T" and txn_sub_type_code in ("6","9") and ultimate_txn_no <> txn_no '
ll_rowcount = dw_transaction_list.RowCount()

// initialize
ll_found = -1
DO UNTIL ll_found = 0
	ll_found = dw_transaction_list.Find(ls_find,ll_found+1, ll_rowcount)
	IF ll_found > 0 THEN
		IF ll_found + 1 > ll_rowcount THEN
			EXIT
		ELSE
			ldec_transferred_txn_amount = ldec_transferred_txn_amount + dw_transaction_list.GetItemDecimal(ll_found,'txn_amount')
		END IF
	END IF
LOOP

adec_remaining_ult_txn_amount = ldec_ultimate_txn_amount + ldec_transferred_txn_amount
		
end subroutine

private subroutine wf_calculate_remaining_payment_amount (long al_payment_no, ref decimal adec_remaining_payment_amount);decimal {2} ldec_total_payment_amount
decimal {2} ldec_transfered_txn_amount


select isnull(total_payment_amount,0) 
into :ldec_total_payment_amount
from PAYMENT a
where a.payment_no = :al_payment_no
using SQLCA;
SQLCA.nf_handle_error('w_claim_cost_maintenance', '', 'wf_calculate_remaining_payment_amount - Embedded SQL:Select from  PAYMENT') 

SELECT IsNull(SUM(b.txn_amount),0) 
INTO :ldec_transfered_txn_amount
FROM   ADJUSTMENT_TXN_WORK_TABLE a,
		 APPLIED_CLAIM_TXN b
WHERE a.txn_no = b.txn_no 
  AND b.payment_no = :al_payment_no
  AND (    (b.txn_type_code = 'T' and txn_sub_type_code in(6,9)))
USING SQLCA;
SQLCA.nf_handle_error('w_claim_cost_maintenance', '', 'wf_calculate_remaining_payment_amount - Embedded SQL:Select from  ADJUSTMENT_TXN_WORK_TABLE') 


adec_remaining_payment_amount = ldec_total_payment_amount + ldec_transfered_txn_amount

end subroutine

public function integer wf_calc_net_other_ult_txn_amount (long al_payment_no, long al_ultimate_txn_no, ref decimal adcm_net_other_ult_txn_amount);decimal {2}	ldcm_ult_txn_amount, ldcm_adj_ult_txn_amount, ldcm_net_ult_txn_amount, ldcm_net_ult_unapp_txn_amount
long			ll_found, ll_rowcount, ll_other_ultimate_txn_no
string			ls_find
boolean		lb_last_row

ll_rowcount = dw_transaction_list.RowCount()

ls_find = 'payment_no = ' +String(al_payment_no)+ ' and ultimate_txn_no <> ' + String(al_ultimate_txn_no) + ' and ultimate_txn_no = txn_no'
// initialize
ll_found = -1
DO UNTIL ll_found = 0
	IF lb_last_row = TRUE THEN
		EXIT
	END IF
	ll_found = dw_transaction_list.Find(ls_find,ll_found+1, ll_rowcount)
	IF ll_found > 0 THEN
		IF ll_found = ll_rowcount THEN
			lb_last_row = TRUE
		END IF
		ldcm_ult_txn_amount = dw_transaction_list.GetItemDecimal(ll_found,'ultimate_txn_amount')
		ldcm_adj_ult_txn_amount = dw_transaction_list.GetItemDecimal(ll_found,'adj_txn_amount_for_ultimate')
		ldcm_net_ult_unapp_txn_amount = dw_transaction_list.GetItemDecimal(ll_found,'amt_entered')
		ll_other_ultimate_txn_no = dw_transaction_list.GetItemDecimal(ll_found,'ultimate_txn_no')
		IF ib_oldest_to_newest = FALSE THEN
			// newest txns are applied first, i.e., those with higher txn_nos
			IF ll_other_ultimate_txn_no < al_ultimate_txn_no THEN
				adcm_net_other_ult_txn_amount = adcm_net_other_ult_txn_amount + ldcm_ult_txn_amount + ldcm_adj_ult_txn_amount + ldcm_net_ult_unapp_txn_amount
			ELSE
				// Do not account for unapplied adj txn amounts
				adcm_net_other_ult_txn_amount = adcm_net_other_ult_txn_amount + ldcm_ult_txn_amount + ldcm_adj_ult_txn_amount
			END IF
		ELSE
			// oldest txns are applied first, i.e., those with lower txn_nos
			IF ll_other_ultimate_txn_no > al_ultimate_txn_no THEN
				adcm_net_other_ult_txn_amount = adcm_net_other_ult_txn_amount + ldcm_ult_txn_amount + ldcm_adj_ult_txn_amount + ldcm_net_ult_unapp_txn_amount
			ELSE
				// Do not account for unapplied adj txn amounts
				adcm_net_other_ult_txn_amount = adcm_net_other_ult_txn_amount + ldcm_ult_txn_amount + ldcm_adj_ult_txn_amount
			END IF
		END IF
	END IF
LOOP

return 0



end function

public function integer wf_calc_net_other_adj_ult_days_lost (long al_payment_no, long al_ultimate_txn_no, ref decimal adcm_net_other_ult_days_lost);decimal {2}	ldcm_net_ult_unapp_days_lost
long			ll_found, ll_rowcount
string			ls_find
boolean		lb_last_row

ll_rowcount = dw_transaction_list.RowCount()

ls_find = 'payment_no = ' +String(al_payment_no)+ ' and ultimate_txn_no <> ' + String(al_ultimate_txn_no) + ' and ultimate_txn_no = txn_no'
// initialize
ll_found = -1
DO UNTIL ll_found = 0
	IF lb_last_row = TRUE THEN
		EXIT
	END IF
	ll_found = dw_transaction_list.Find(ls_find,ll_found+1, ll_rowcount)
	IF ll_found > 0 THEN
		IF ll_found = ll_rowcount THEN
			lb_last_row = TRUE
		END IF
		ldcm_net_ult_unapp_days_lost = dw_transaction_list.GetItemDecimal(ll_found,'days_entered')
		adcm_net_other_ult_days_lost = adcm_net_other_ult_days_lost + ldcm_net_ult_unapp_days_lost
	END IF
LOOP

return 0



end function

public function integer wf_calc_net_other_adj_ult_hours_lost (long al_payment_no, long al_ultimate_txn_no, ref decimal adcm_net_other_ult_hours_lost);decimal {2}	ldcm_net_ult_unapp_hours_lost
long			ll_found, ll_rowcount
string			ls_find
boolean		lb_last_row

ll_rowcount = dw_transaction_list.RowCount()

ls_find = 'payment_no = ' +String(al_payment_no)+ ' and ultimate_txn_no < ' + String(al_ultimate_txn_no) + ' and ultimate_txn_no = txn_no'
// initialize
ll_found = -1
DO UNTIL ll_found = 0
	IF lb_last_row = TRUE THEN
		EXIT
	END IF
	ll_found = dw_transaction_list.Find(ls_find,ll_found+1, ll_rowcount)
	IF ll_found > 0 THEN
		IF ll_found = ll_rowcount THEN
			lb_last_row = TRUE
		END IF
		ldcm_net_ult_unapp_hours_lost = dw_transaction_list.GetItemDecimal(ll_found,'hours_entered')
		adcm_net_other_ult_hours_lost = adcm_net_other_ult_hours_lost + ldcm_net_ult_unapp_hours_lost
	END IF	
LOOP

return 0



end function

public function integer wf_update_rehab_authorization (string as_txn_type_code, string as_txn_sub_type_code);LONG 		ll_row
LONG 	    ll_claim_no
LONG 	    ll_authorization_no
LONG 	    ll_adjustment_rehab_qty
LONG		ll_org_paid_quantity_adjust
LONG		ll_rehab_task_row_auth
LONG		ll_paid_rehab_qty
LONG		ll_rc
LONG		ll_rc2
LONG      ll_paid_rehab_qty_to_side
LONG		ll_claim_no_to
LONG		ll_authorization_no_to

STRING  ls_find
STRING 	ls_script = 'wf_update_rehab_authorization()'

DECIMAL {2} ldcm_adjustment_amount
DECIMAL {2} ldcm_org_paid_amount_adjust
DECIMAL {2} ldcm_paid_amount
DECIMAL {2} ldcm_paid_amount_to_side

/* this will take care of updating the rehab task authorization - if required */
/*{     loop through the adjustment dw to create the adjustment amount in one dw to update the rehab task authorization  in the next step below}*/

for ll_row = 1 to ids_rehab_task_authorization_update.rowcount() /* LOOP 1*/
	
	ll_claim_no 							= ids_rehab_task_authorization_update.getitemnumber(ll_row,'claim_no')
	ll_authorization_no 				= ids_rehab_task_authorization_update.getitemnumber(ll_row,'authorization_no')
	ldcm_adjustment_amount 		= ids_rehab_task_authorization_update.getitemdecimal(ll_row,'adjustment_amount')
	ll_adjustment_rehab_qty 		= ids_rehab_task_authorization_update.getitemnumber(ll_row,'adjustment_rehab_qty')			

	
    /*	build the find  logic to search the dw */
	ls_find = "claim_no = " + string(ll_claim_no) + " and authorization_no = " + string(ll_authorization_no)

	ll_rc =  ids_rehab_task_authorization_to_update_sum.Find(ls_find, 1, ids_rehab_task_authorization_to_update_sum.ROWCOUNT() ) 
	
	
	/* the first time it will not find a row then it will perform an insert into the dw */
	if ll_rc > 0 then		
			
		ldcm_org_paid_amount_adjust = ids_rehab_task_authorization_to_update_sum.getitemdecimal(ll_rc,'paid_amount_adjust')
		ll_org_paid_quantity_adjust = ids_rehab_task_authorization_to_update_sum.getitemnumber(ll_rc,'paid_quantity_adjust')
		ids_rehab_task_authorization_to_update_sum.setitem(ll_rc,'paid_amount_adjust',ldcm_adjustment_amount + ldcm_org_paid_amount_adjust)
		ids_rehab_task_authorization_to_update_sum.setitem(ll_rc,'paid_quantity_adjust',ll_adjustment_rehab_qty + ll_org_paid_quantity_adjust)						
	else
		ll_rehab_task_row_auth = ids_rehab_task_authorization_to_update_sum.insertrow(0)
		ids_rehab_task_authorization_to_update_sum.setitem(ll_rehab_task_row_auth,'claim_no',ll_claim_no)
		ids_rehab_task_authorization_to_update_sum.setitem(ll_rehab_task_row_auth,'authorization_no',ll_authorization_no)
		ids_rehab_task_authorization_to_update_sum.setitem(ll_rehab_task_row_auth,'paid_amount_adjust',ldcm_adjustment_amount)
		ids_rehab_task_authorization_to_update_sum.setitem(ll_rehab_task_row_auth,'paid_quantity_adjust',ll_adjustment_rehab_qty)			
	end if			

	
next  /* END OF LOOP 1*/



/* { loop through the dw that has the total adjustment amounts to update the main dw for rehab task authorization  from side and to side of the claim } */
for ll_row = 1 to ids_rehab_task_authorization_to_update_sum.rowcount()	 /* LOOP 2*/
	
		ll_claim_no 							= 	ids_rehab_task_authorization_to_update_sum.getitemnumber(ll_row,'claim_no')
		ll_authorization_no 				= 	ids_rehab_task_authorization_to_update_sum.getitemnumber(ll_row,'authorization_no')
		ldcm_adjustment_amount		=	ids_rehab_task_authorization_to_update_sum.getitemdecimal(ll_row,'paid_amount_adjust') 
		ll_adjustment_rehab_qty 		= 	ids_rehab_task_authorization_to_update_sum.getitemnumber(ll_row,'paid_quantity_adjust')

	   /* from side of the claim 
		
		     need to retrive dw
			 get values from dw
			 only update the amount if the billable item says to
			 adjust values to dw
		
		*/
		/* get the rehab task to update 'to side'*/	
		ll_rc = ids_rehab_task_authorization.retrieve(ll_claim_no,ll_authorization_no)
		SQLCA.nf_handle_error("ids_rehab_task_authorization - retrieve()",is_window_name,ls_script) 
						
		if ll_rc <> 1 then	
			signalerror(-999,"Can not find REHAB TASK AUTHORIZATION to update in the from side")
		end if
		
			
	     /* get the paid qty and paid amount with the adjustment amounts */
		 ll_paid_rehab_qty 		= ids_rehab_task_authorization.getitemnumber(ll_rc,'paid_quantity')
          ldcm_paid_amount       = ids_rehab_task_authorization.getitemnumber(ll_rc,'paid_amount')
			
		/* set the dw column  properties for the 'from side'*/
		
		ids_rehab_task_authorization.setitem(ll_rc,'paid_quantity',ll_paid_rehab_qty + ll_adjustment_rehab_qty)  
		ids_rehab_task_authorization.setitem(ll_rc,'paid_amount', ldcm_paid_amount + ldcm_adjustment_amount)
			

		
		
		/* update the dw's  both the from and to side of the rehab tak authorization */						
		 /* update the dw - 'from side'*/
		 ll_rc = ids_rehab_task_authorization.update()
		 SQLCA.nf_handle_error("ds_rehab_task_authorization.update()",is_window_name,ls_script)	
		if ll_rc < 0 then 
			signalerror(-999,"Can not find REHAB TASK AUTHORIZATION to update " + " - ids_rehab_task_authorization.update()")
		end if
		
		
			
		/* to side  -   { Claim Transfers only }
			   set the to side by adjsuting the paid quantity and paid amounts 
	    */
		 
         if ( as_txn_type_code = 'T' AND as_txn_sub_type_code = '6' ) then   /* claim transfer - the two side   ADJUSTING */
		
		    /* grab the claim to and authorization to from the paramter screen that user enteres*/
			ll_authorization_no_to = dw_parameter.GetItemNumber(1,'authorization_no_to')
			ll_claim_no_to = dw_parameter.GetItemNumber(1,'claim_no_to')

			/* retrive  the 'two side' of the rehab task authorization */
			ll_rc2 = ids_rehab_task_authorization_to_side.retrieve(ll_claim_no_to,ll_authorization_no_to)
			SQLCA.nf_handle_error("ids_rehab_task_authorization_to_side - retrieve()",is_window_name,ls_script) 
				
			  if ll_rc2 <> 1 then	
				signalerror(-999,"Can not find REHAB TASK AUTHORIZATION to update for two side. Claim No is " + string(ll_claim_no_to) + " . The Auhtorization No is " + string(ll_authorization_no_to))
			end if
				

			/* get the paid qty and paid amount with the adjustment amounts */
			ll_paid_rehab_qty_to_side		= ids_rehab_task_authorization_to_side.getitemnumber(ll_rc2,'paid_quantity') 
			ldcm_paid_amount_to_side 	    = ids_rehab_task_authorization_to_side.getitemdecimal(ll_rc2,'paid_amount') 
		
			/* set the amount in var after applying adjustments */
			ll_paid_rehab_qty_to_side = ll_paid_rehab_qty_to_side -  ll_adjustment_rehab_qty
			ldcm_paid_amount_to_side  = ldcm_paid_amount_to_side  -  ldcm_adjustment_amount 
					
			/* set the dw column properties for the two side */
			ids_rehab_task_authorization_to_side.setitem(ll_rc2,'paid_quantity',ll_paid_rehab_qty_to_side)  
			ids_rehab_task_authorization_to_side.setitem(ll_rc,'paid_amount', ldcm_paid_amount_to_side)
	
			 /* update the dw 'to side' */
			 ll_rc2 = ids_rehab_task_authorization_to_side.update()
			 SQLCA.nf_handle_error("ds_rehab_task_authorization._to_side.update() ",is_window_name,ls_script)	
		
			if ll_rc2 < 0 then 
				signalerror(-999,"Can not find REHAB TASK AUTHORIZATION to update - to side")
			end if
			
			
		end if
			
			

next  /*END OF LOOP 2 */




return 1


end function

public subroutine wf_update_rehab_invoice_line_item_amend (string as_txn_type_code, string as_txn_sub_type_code, long al_payment_no);integer li_rc	
string ls_script = "wf_update_rehab_invoice_line_item_amend()"		
string   ls_amended_code

								
// If the Adjustment of cancel cheque and/or direct dpeosit or overpayment revover reduces the payment to zero then
// then the REHAB_INVOICE_LINE_ITEM.line_item_amended_code will be set to  'OR' {e-physio Deployment 2 } for Overpayment Recover and
// 'CC' for canceled cheque.

/* RETRIEVE THE REHAB INVOICE LINE ITEM BY PAYMENT NO */
li_rc = ids_rehab_invoice_line_item.retrieve(al_payment_no)
SQLCA.nf_handle_error(" ids_rehab_invoice_line_item.retrieve()",is_window_name,ls_script)

			
/* IF MORE THAN ONE ROW THEN THIS IS INTERGRITY ISSUE*/			
if (li_rc > 1) then SignalError(-666,"Data Intergrity Issue with Rehab Invoice Line Item - Expecting only one row - return more than one")

/* IF REHAB INVOICE LINE ITEM IS FOUND - WE NEED TO UPDATE THE AMENDED CODE*/
if (li_rc = 1) then
		  
	 ls_amended_code = ids_rehab_invoice_line_item.getitemstring(1,'line_item_amended_code')

		if (as_txn_type_code = 'J' AND as_txn_sub_type_code= "2") then
				 ids_rehab_invoice_line_item.setitem(1,'line_item_amended_code','CC')
		elseif (as_txn_type_code = 'J'  and as_txn_sub_type_code = "3") then
				  if  ls_amended_code = 'OR' THEN
					  ids_rehab_invoice_line_item.setitem(1,'line_item_amended_code','')
				ELSE
						ids_rehab_invoice_line_item.setitem(1,'line_item_amended_code','OR')
				  END IF		  
		elseif (as_txn_type_code = 'T'  and as_txn_sub_type_code = "6") then
				 ids_rehab_invoice_line_item.setitem(1,'line_item_amended_code','CT')
	      end if
		
	end if
		
		
end subroutine

public subroutine wf_insert_invoice_line_item (long al_new_payment_no, long al_payment_no_from, long al_claim_no_to, long al_authorization_no_to, long al_claim_no_from, ref long al_last_rehab_invoice_no);INTEGER 	li_rc
LONG 			ll_rehab_invoice_no, ll_line_no, ll_authorization_no, ll_claim_no, ll_task_no, ll_fee_no, ll_billable_xref_no
LONG 			ll_tax_rate_no, ll_payment_no, ll_from_rehab_invoice_no, ll_from_line_no, ll_web_create_id, ll_web_modify_id
LONG 			ll_task_no_to, ll_billable_xref_no_to, ll_row_to_insert, ll_row_to_insert_2,  ll_rehab_invoice_no_fr
LONG 			ll_provider_no_fr, ll_submitted_by_no_fr
DATETIME	ldtm_service_date, ldtm_web_modify_date, ltdm_submitted_date_fr, ldtm_invoice_date_fr
STRING 		ls_billable_unit_code, ls_tax_applied_flag, ls_line_item_amended_code,  ls_external_invoice_id_fr
STRING 		 ls_submitted_by_type_code_fr, ls_provider_type_code_fr
DECIMAL {2} ldcm_quantity, ldcm_unit_fee_to, ldcm_units_billed, ldcm_unit_price
DECIMAL {2} 	ldcm_authorization_quantity, ldcm_authorization_amount, ldcm_paid_quantity, ldcm_paid_amount, ldcm_tax_rate
				
STRING ls_script = "wf_insert_invoice_line_item()"


/* RETRIEVE THE REHAB INVOICE LINE ITEM BY PAYMENT NO */
li_rc = ids_rehab_invoice_line_item.retrieve(al_payment_no_from)
 SQLCA.nf_handle_error(" ids_rehab_invoice_line_item.retrieve()",is_window_name,ls_script)

if li_rc <> 1 then	
	return
end if
	

/* update last # table var */
al_last_rehab_invoice_no++

			 
 SELECT  task_no,
			billable_xref_no,
			authorized_quantity,
			authorized_amount,
			paid_quantity,
			paid_amount
			INTO
			:ll_task_no_to,
			:ll_billable_xref_no_to,
			:ldcm_authorization_quantity,
			:ldcm_authorization_amount,
			:ldcm_paid_quantity,
			:ldcm_paid_amount
  FROM REHAB_TASK_AUTHORIZATION
  where claim_no = :al_claim_no_to    and
				  authorization_no = :al_authorization_no_to
		 USING SQLCA;
		 if SQLCA.nf_handle_error('SELECT FROM REHAB_TASK_AUTHORIZATION',is_window_name,ls_script) = 100 then
	 	         SignalError(-666,'Found no rehab task authorizations for auhtorization no= ' + string(al_authorization_no_to))
      	end if
				
	ll_rehab_invoice_no 		    =   ids_rehab_invoice_line_item.getitemnumber(1,'rehab_invoice_no')	
	ll_line_no					    =   ids_rehab_invoice_line_item.getitemnumber(1,'line_no')
	ll_authorization_no		     =   ids_rehab_invoice_line_item.getitemnumber(1,'authorization_no')
	ll_claim_no					     =   ids_rehab_invoice_line_item.getitemnumber(1,'claim_no')
	ll_task_no					      =   ids_rehab_invoice_line_item.getitemnumber(1,'task_no')
	ldtm_service_date			      =   ids_rehab_invoice_line_item.getitemdatetime(1,'service_date')
	ll_fee_no						       =   ids_rehab_invoice_line_item.getitemnumber(1,'fee_no')
	ll_billable_xref_no			       =   ids_rehab_invoice_line_item.getitemnumber(1,'billable_xref_no')
	ls_billable_unit_code		        =   ids_rehab_invoice_line_item.getitemstring(1,'billable_unit_code')
	ldcm_quantity				        =   ids_rehab_invoice_line_item.getitemdecimal(1,'quantity')
	 ldcm_units_billed		             =   ids_rehab_invoice_line_item.getitemdecimal(1,'units_billed')
	ldcm_unit_price					=   ids_rehab_invoice_line_item.getitemdecimal(1,'unit_price')
	ll_tax_rate_no 						=   ids_rehab_invoice_line_item.getitemnumber(1,'tax_rate_no')
	ldcm_tax_rate							=   ids_rehab_invoice_line_item.getitemnumber(1,'tax_rate')
	ls_tax_applied_flag				=   ids_rehab_invoice_line_item.getitemstring(1,'tax_applied_flag')
	ll_payment_no						 =   ids_rehab_invoice_line_item.getitemnumber(1,'payment_no')
	ls_line_item_amended_code	 =   ids_rehab_invoice_line_item.getitemstring(1,'line_item_amended_code')
	ll_from_rehab_invoice_no		 =   ids_rehab_invoice_line_item.getitemnumber(1,'from_rehab_invoice_no')
	ll_from_line_no						 =   ids_rehab_invoice_line_item.getitemnumber(1,'from_line_no')
//	ldtm_web_modify_date			 =   ids_rehab_invoice_line_item.getitemdatetime(1,'web_modify_date')
//	ll_web_create_id			 		=   ids_rehab_invoice_line_item.getitemnumber(1,'web_create_id')
//	ll_web_modify_id			 		=   ids_rehab_invoice_line_item.getitemnumber(1,'web_modify_id')
				
	
	ll_row_to_insert = ids_rehab_invoice_line_item_insert.insertrow(0)
			 
			 
			
			/* set the value */
			 ids_rehab_invoice_line_item_insert.setitem(ll_row_to_insert,'rehab_invoice_no',al_last_rehab_invoice_no)				 
			ids_rehab_invoice_line_item_insert.setitem(ll_row_to_insert,'line_no',1)
			ids_rehab_invoice_line_item_insert.setitem(ll_row_to_insert,'authorization_no',al_authorization_no_to)
			ids_rehab_invoice_line_item_insert.setitem(ll_row_to_insert,'claim_no',al_claim_no_to)
			ids_rehab_invoice_line_item_insert.setitem(ll_row_to_insert,'task_no',ll_task_no_to)
			ids_rehab_invoice_line_item_insert.setitem(ll_row_to_insert,'service_date',ldtm_service_date)
			ids_rehab_invoice_line_item_insert.setitem(ll_row_to_insert,'fee_no',ll_fee_no)
			ids_rehab_invoice_line_item_insert.setitem(ll_row_to_insert,'billable_xref_no',ll_billable_xref_no)
			ids_rehab_invoice_line_item_insert.setitem(ll_row_to_insert,'billable_unit_code',ls_billable_unit_code)
			ids_rehab_invoice_line_item_insert.setitem(ll_row_to_insert,'quantity',ldcm_quantity	)		
			ids_rehab_invoice_line_item_insert.setitem(ll_row_to_insert,'units_billed', ldcm_units_billed)
			ids_rehab_invoice_line_item_insert.setitem(ll_row_to_insert,'unit_price',ldcm_unit_price)
			ids_rehab_invoice_line_item_insert.setitem(ll_row_to_insert,'tax_rate_no',ll_tax_rate_no)
			ids_rehab_invoice_line_item_insert.setitem(ll_row_to_insert,'tax_rate',ldcm_tax_rate)
			ids_rehab_invoice_line_item_insert.setitem(ll_row_to_insert,'tax_applied_flag',ls_tax_applied_flag)
			ids_rehab_invoice_line_item_insert.setitem(ll_row_to_insert,'payment_no',al_new_payment_no)
			ids_rehab_invoice_line_item_insert.setitem(ll_row_to_insert,'line_item_amended_code','')
			ids_rehab_invoice_line_item_insert.setitem(ll_row_to_insert,'from_rehab_invoice_no',ll_rehab_invoice_no)
			ids_rehab_invoice_line_item_insert.setitem(ll_row_to_insert,'from_line_no', ll_line_no)
//			ids_rehab_invoice_line_item_insert.setitem(ll_row_to_insert,'web_create_id',0)
//			ids_rehab_invoice_line_item_insert.setitem(ll_row_to_insert,'web_modify_id',0)
	
	//  (JP) Hi In the WorkBench, you should be able to rely on the database defaults to set the values for the web_create_id, web_modify_id, 
	//    web_create_date and web_modify_date.

	
	

			 SELECT      a.rehab_invoice_no,
							a.external_invoice_id,
							a.invoice_date,
							a.provider_no,
							a.provider_type_code,
							a.submitted_date,
							a.submitted_by_no,
							a.submitted_by_type_code									
					into 
							 :ll_rehab_invoice_no_fr,
							 :ls_external_invoice_id_fr,
							 :ldtm_invoice_date_fr,
							 :ll_provider_no_fr,
							 :ls_provider_type_code_fr,
							 :ltdm_submitted_date_fr,
							 :ll_submitted_by_no_fr,
							 :ls_submitted_by_type_code_fr				
	  FROM  REHAB_INVOICE a 
	  join REHAB_INVOICE_LINE_ITEM b
	  ON a.rehab_invoice_no = b.rehab_invoice_no
	  where b.payment_no = :al_payment_no_from and
				  b.claim_no = :al_claim_no_from
	  using SQLCA;
	  
	 if SQLCA.nf_handle_error('SELECT FROM REHAB_TASK_AUTHORIZATION',is_window_name,ls_script)= 100 then
			  SignalError(-666,'Found no rehab_invoice "from" for  payment ' + string(al_payment_no_from))
	 end if
			
		  
	ll_row_to_insert_2 = ids_rehab_invoice_insert.insertrow(0)	  
	ids_rehab_invoice_insert.setitem( ll_row_to_insert_2,'rehab_invoice_no',al_last_rehab_invoice_no)
	ids_rehab_invoice_insert.setitem( ll_row_to_insert_2,'external_invoice_id',ls_external_invoice_id_fr)
	ids_rehab_invoice_insert.setitem( ll_row_to_insert_2,'invoice_date',ldtm_invoice_date_fr)
	ids_rehab_invoice_insert.setitem( ll_row_to_insert_2,'provider_no',ll_provider_no_fr)
	ids_rehab_invoice_insert.setitem( ll_row_to_insert_2,'provider_type_code',ls_provider_type_code_fr)
	ids_rehab_invoice_insert.setitem( ll_row_to_insert_2,'submitted_date',ltdm_submitted_date_fr)
	ids_rehab_invoice_insert.setitem( ll_row_to_insert_2,'submitted_by_no',ll_submitted_by_no_fr)
	ids_rehab_invoice_insert.setitem( ll_row_to_insert_2,'submitted_by_type_code',ls_submitted_by_type_code_fr)
//	ids_rehab_invoice_insert.setitem( ll_row_to_insert_2,'web_create_id',0)
	ids_rehab_invoice_insert.setitem( ll_row_to_insert_2,'doc_id',0)
	ids_rehab_invoice_insert.setitem( ll_row_to_insert_2,'from_rehab_invoice_no',ll_rehab_invoice_no)
	
	//  (JP) Hi In the WorkBench, you should be able to rely on the database defaults to set the values for the web_create_id, web_modify_id, 
	//    web_create_date and web_modify_date.

end subroutine

on w_claim_cost_maintenance.create
int iCurrent
call super::create
if IsValid(this.MenuID) then destroy(this.MenuID)
if this.MenuName = "m_cmwb_notools" then this.MenuID = create m_cmwb_notools
this.dw_r1_payments_to_delete=create dw_r1_payments_to_delete
this.st_splitbar_1=create st_splitbar_1
this.rb_total_amt=create rb_total_amt
this.rb_amount=create rb_amount
this.rb_pct=create rb_pct
this.rb_pay_loe=create rb_pay_loe
this.rb_pay_act=create rb_pay_act
this.rb_pay_all_types=create rb_pay_all_types
this.cb_find=create cb_find
this.st_sel=create st_sel
this.cb_search=create cb_search
this.cbx_1=create cbx_1
this.cb_clear=create cb_clear
this.gb_filter=create gb_filter
this.em_amount=create em_amount
this.em_pct=create em_pct
this.cb_apply=create cb_apply
this.cb_cancel_pct=create cb_cancel_pct
this.cbx_oldest_to_newest=create cbx_oldest_to_newest
this.gb_pct=create gb_pct
this.dw_confirm_summary_totals=create dw_confirm_summary_totals
this.dw_parameter=create dw_parameter
this.uo_filter=create uo_filter
this.dw_confirmation_summary=create dw_confirmation_summary
this.cb_close_confirm=create cb_close_confirm
this.cb_close=create cb_close
this.cb_close_unprocessed=create cb_close_unprocessed
this.cb_save=create cb_save
this.cb_delete=create cb_delete
this.dw_confirmation=create dw_confirmation
this.dw_claimcost_unapplied_claim_txn=create dw_claimcost_unapplied_claim_txn
this.cb_refresh=create cb_refresh
this.cb_unit_of_work_save=create cb_unit_of_work_save
this.cb_cancel=create cb_cancel
this.cb_confirm=create cb_confirm
this.dw_transaction_list=create dw_transaction_list
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_r1_payments_to_delete
this.Control[iCurrent+2]=this.st_splitbar_1
this.Control[iCurrent+3]=this.rb_total_amt
this.Control[iCurrent+4]=this.rb_amount
this.Control[iCurrent+5]=this.rb_pct
this.Control[iCurrent+6]=this.rb_pay_loe
this.Control[iCurrent+7]=this.rb_pay_act
this.Control[iCurrent+8]=this.rb_pay_all_types
this.Control[iCurrent+9]=this.cb_find
this.Control[iCurrent+10]=this.st_sel
this.Control[iCurrent+11]=this.cb_search
this.Control[iCurrent+12]=this.cbx_1
this.Control[iCurrent+13]=this.cb_clear
this.Control[iCurrent+14]=this.gb_filter
this.Control[iCurrent+15]=this.em_amount
this.Control[iCurrent+16]=this.em_pct
this.Control[iCurrent+17]=this.cb_apply
this.Control[iCurrent+18]=this.cb_cancel_pct
this.Control[iCurrent+19]=this.cbx_oldest_to_newest
this.Control[iCurrent+20]=this.gb_pct
this.Control[iCurrent+21]=this.dw_confirm_summary_totals
this.Control[iCurrent+22]=this.dw_parameter
this.Control[iCurrent+23]=this.uo_filter
this.Control[iCurrent+24]=this.dw_confirmation_summary
this.Control[iCurrent+25]=this.cb_close_confirm
this.Control[iCurrent+26]=this.cb_close
this.Control[iCurrent+27]=this.cb_close_unprocessed
this.Control[iCurrent+28]=this.cb_save
this.Control[iCurrent+29]=this.cb_delete
this.Control[iCurrent+30]=this.dw_confirmation
this.Control[iCurrent+31]=this.dw_claimcost_unapplied_claim_txn
this.Control[iCurrent+32]=this.cb_refresh
this.Control[iCurrent+33]=this.cb_unit_of_work_save
this.Control[iCurrent+34]=this.cb_cancel
this.Control[iCurrent+35]=this.cb_confirm
this.Control[iCurrent+36]=this.dw_transaction_list
end on

on w_claim_cost_maintenance.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.dw_r1_payments_to_delete)
destroy(this.st_splitbar_1)
destroy(this.rb_total_amt)
destroy(this.rb_amount)
destroy(this.rb_pct)
destroy(this.rb_pay_loe)
destroy(this.rb_pay_act)
destroy(this.rb_pay_all_types)
destroy(this.cb_find)
destroy(this.st_sel)
destroy(this.cb_search)
destroy(this.cbx_1)
destroy(this.cb_clear)
destroy(this.gb_filter)
destroy(this.em_amount)
destroy(this.em_pct)
destroy(this.cb_apply)
destroy(this.cb_cancel_pct)
destroy(this.cbx_oldest_to_newest)
destroy(this.gb_pct)
destroy(this.dw_confirm_summary_totals)
destroy(this.dw_parameter)
destroy(this.uo_filter)
destroy(this.dw_confirmation_summary)
destroy(this.cb_close_confirm)
destroy(this.cb_close)
destroy(this.cb_close_unprocessed)
destroy(this.cb_save)
destroy(this.cb_delete)
destroy(this.dw_confirmation)
destroy(this.dw_claimcost_unapplied_claim_txn)
destroy(this.cb_refresh)
destroy(this.cb_unit_of_work_save)
destroy(this.cb_cancel)
destroy(this.cb_confirm)
destroy(this.dw_transaction_list)
end on

event open;call super::open;datawindowchild ldwc_txn_type_combination


/* the window name will be used in the SQLCA.nf_handle_error */
is_window_name = classname()


/* BR OBJECT - COMMON OBJECT */
inv_ccm = create n_claim_cost_maintenance

/* CLAIM COST MAINTENANCE MODULE */
inv_ccm.is_module_code = '005'


/* insert blank row into parameter section */
dw_parameter.insertrow(0)

/* determine initial state of frame */
iws_frame_open_state = w_frame.WindowState

/* maximize frame while module is envoked */
w_frame.WindowState = Maximized!

/* use to determine which txn type the user can maintain*/
dw_parameter.GetChild("txn_sub_type_code",ldwc_txn_type_combination)
ldwc_txn_type_combination.SetTransObject(SQLCA)

/* reset parm screen - common code - called in more the once place*/
wf_reset_parm_screen()

/* SPLIT BAR ON WINDOW */
st_splitbar_1.of_Register(dw_confirmation)
st_splitbar_1.of_Register(dw_confirm_summary_totals)

uo_filter.uf_set_Requestor(dw_transaction_list)

triggerevent("ue_load_module_security")

end event

event close;call super::close;/* SET THE WINDOW STATE BACK TO ORG STATE PRIOR TO CLOSE OF WINDOW */
w_frame.WindowState = iws_frame_open_state
end event

event closequery;call super::closequery;/* SET THE WINDOW STATE BACK TO ORG STATE PRIOR TO CLOSE OF WINDOW */
w_frame.WindowState = iws_frame_open_state
end event

type dw_r1_payments_to_delete from datawindow within w_claim_cost_maintenance
boolean visible = false
integer x = 2405
integer y = 2208
integer width = 192
integer height = 100
integer taborder = 190
string title = "none"
string dataobject = "dx_r1_unproc_pymts_to_delete"
boolean livescroll = true
end type

type st_splitbar_1 from u_splitbar_horizontal within w_claim_cost_maintenance
boolean visible = false
integer y = 1248
integer width = 4571
integer height = 36
boolean border = true
borderstyle borderstyle = styleraised!
end type

type rb_total_amt from radiobutton within w_claim_cost_maintenance
integer x = 3374
integer y = 356
integer width = 581
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Apply Total Amount"
end type

event clicked;em_pct.visible = false
em_pct.Text = ''
em_amount.visible = true
em_amount.text = '0'
cbx_oldest_to_newest.visible 	= true
cb_apply.visible = true
cb_apply.enabled = true
end event

type rb_amount from radiobutton within w_claim_cost_maintenance
integer x = 3374
integer y = 260
integer width = 439
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Apply Amount"
end type

event clicked;em_pct.visible = false
em_pct.Text = ''
em_amount.visible = true
em_amount.text = '0'
cbx_oldest_to_newest.visible 	= false
cb_apply.visible = true
cb_apply.enabled = true
end event

type rb_pct from radiobutton within w_claim_cost_maintenance
integer x = 3374
integer y = 164
integer width = 443
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Apply Percent"
end type

event clicked;em_pct.enabled = true
em_pct.visible = true
//em_amount.enabled = false
em_amount.visible = false
em_amount.Text = ''
em_amount.text = '0'
cbx_oldest_to_newest.visible 	= false
cb_apply.visible = true
cb_apply.enabled = true
end event

type rb_pay_loe from radiobutton within w_claim_cost_maintenance
integer x = 2597
integer y = 280
integer width = 658
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Non-Account Payment"
end type

type rb_pay_act from radiobutton within w_claim_cost_maintenance
integer x = 2597
integer y = 188
integer width = 539
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Account Payment"
end type

type rb_pay_all_types from radiobutton within w_claim_cost_maintenance
integer x = 2597
integer y = 96
integer width = 475
integer height = 96
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "All Payments"
end type

type cb_find from commandbutton within w_claim_cost_maintenance
integer x = 1367
integer y = 512
integer width = 302
integer height = 104
integer taborder = 220
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Fin&d"
end type

event clicked;long ll_payment_no
string ls_find
integer li_find , li_rc,li_row


dw_parameter.accepttext()

ll_payment_no = dw_parameter.getitemnumber(1,'payment_no_find')
	
if isnull(ll_payment_no) then
	MESSAGEBOX("Warning","Must enter a payment no to perform FIND")
	dw_parameter.setcolumn('payment_no_find')
	dw_parameter.setfocus()
	return
end if

if dw_transaction_list.rowcount() > 0 then
else
	MESSAGEBOX("Warning","Rows must be returned before the find can locate payments")
	dw_transaction_list.setredraw(true)
	return 
end if

dw_transaction_list.selectrow(0,false)

dw_transaction_list.setredraw(true)
	
ls_find = "payment_no = " + string(ll_payment_no)
li_find = dw_transaction_list.find(ls_find,1,dw_transaction_list.rowcount())


if li_find > 0 then
	do until  li_find <= 0 
		
		li_row = li_find
		
		li_find = dw_transaction_list.find(ls_find,li_find + 1,dw_transaction_list.rowcount())
		
		if li_find = dw_transaction_list.rowcount() then
			li_row = li_find
			exit
		end if
	loop
	
	li_rc = dw_transaction_list.scrolltorow(li_row)
	dw_transaction_list.selectrow(li_row,true)
else
	messagebox("Warning","No payment found for that payment no")
	dw_parameter.setcolumn('payment_no_find')
	dw_parameter.setfocus()
	return 
end if





end event

type st_sel from statictext within w_claim_cost_maintenance
integer x = 4434
integer y = 416
integer width = 110
integer height = 64
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "sel"
boolean focusrectangle = false
end type

type cb_search from commandbutton within w_claim_cost_maintenance
integer x = 2011
integer y = 512
integer width = 302
integer height = 104
integer taborder = 200
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Search"
end type

event clicked;Boolean	b_cheque_found
Boolean	lb_paid_fromto_date_checked
Boolean	lb_proc_date_checked
Boolean	lb_period_date_violation
Boolean	lb_no_period_date_violation
Boolean	lb_no_proc_date_violation
Boolean	lb_proc_date_violation

string		ls_cheque_reconciled_code 
string		ls_recipient_type_code 
string		ls_payment_types
string 	ls_txn_type_code
string		ls_filter
string 	ls_txn_sub_type_code
string 	ls_payment_sub_type_code_fr
string		ls_payment_sub_type_code_to
string 	ls_payment_type_code_fr
string 	ls_payment_type_code_to
string		ls_explanation	
string		ls_recipient_name
string 	ls_wg_receiving_salary_flag  
string 	ls_wg_non_receiving_salary_flag   
string 	ls_wg_self_insured_flag   
string 	ls_wg_non_self_insured_flag 
string 	ls_inc_receiving_salary_flag  
string		ls_inc_self_insured_flag  
string		ls_explanation_desc	
string 	ls_find
string		ls_message
string		ls_fromto_dates_flag
string 	ls_date_message
STRING 	ls_provider_type_code

long 		ll_no_rows
long		ll_row
long		ll_claim_no_to
long 		ll_claim_no
long		ll_cheque_no
long		ll_xmit_no
long 		ll_rc
long		ll_rows
long 		ll_no_checked
long 		ll_days
long		ll_no_of_recipients
long		ll_no_txn
long		ll_org_rc
long		ll_payment_no 
long		ll_min_payment_no
long		ll_max_payment_no
long		ll_min_no_date_payment_no
long		ll_max_no_date_payment_no
long		ll_no_claims
LONG    	ll_provider_no

date 		ldt_cost_relief_date

datetime	ldtm_current_datetime
datetime	ldtm_paid_from_date
datetime	ldtm_paid_to_date
datetime ldtm_issue_date
datetime	ldtm_min_paid_from_date
datetime ldtm_max_paid_to_date
datetime ldtm_payment_processed_date
datetime ldtm_min_payment_processed_date
datetime ldtm_max_payment_processed_date

decimal {2} ldcm_total_txn_amount
decimal {2} ldcm_amount	

long	  	ll_prev_recipient_no
long	  	ll_recipient_no
long	  	ll_rehab_qty
long		ll_found
LONG 		ll_cost_alloc_no_fr
LONG		ll_cost_alloc_no_to
LONG		ll_cost_alloc_operation_no_fr
LONG		ll_cost_alloc_operation_no_to
string		ls_message_receiving_salary_flag
string		ls_message_self_insured_flag
string		ls_benefit_type 

INTEGER li_count

u_ds lds_txn_list

pointer oldpointer // Declares a pointer variable

oldpointer = SetPointer(HourGlass!)

S_WINDOW_MESSAGE lstr_window_message

dw_parameter.accepttext()
dw_transaction_list.setredraw(false)
dw_transaction_list.reset()
cbx_1.checked = false

inv_ccm.of_reset()
dw_parameter.setitem(1,'amount',0.00)
dw_parameter.setitem(1,'payment_no_find',0)

lds_txn_list = CREATE u_ds
lds_txn_list.dataobject = 'd_claim_cost_transaction_list'
lds_txn_list.SetTransObject(SQLCA)


/* default the check box to un-checked when performing a new search */
FOR ll_row = 1 to dw_transaction_list.rowcount()
	dw_transaction_list.setitem(ll_row,'check_box',0)
NEXT	
		 

ib_filter_applied = false
cb_apply.enabled = false
	
/* reset the filter - if filter was applied */
dw_transaction_list.setfilter("")
dw_transaction_list.enabled = true

if rb_pay_all_types.checked = true then
	ls_payment_types = 'all'
elseif rb_pay_act.checked = true then
	ls_payment_types = 'act'
else
	ls_payment_types = 'loe'
end if
	

	
/* get the txn type/sub type and claim no */
ls_txn_type_code 						     = dw_parameter.GetItemString(1,'txn_type_code')
ls_txn_sub_type_code					     = dw_parameter.GetItemString(1,'txn_sub_type_code')
ll_claim_no								     = dw_parameter.GetItemNumber(1,'claim_no')
ll_claim_no_to							     = dw_parameter.GetItemNumber(1,'claim_no_to')
ll_provider_no                                  = dw_parameter.GetItemNumber(1, 'provider_no')
ls_provider_type_code                       = dw_parameter.GetItemString(1, 'provider_type_code')
ls_payment_type_code_fr 			     = dw_parameter.GetItemString(1,'payment_type_code_fr')
ls_payment_type_code_to 		 	     = dw_parameter.GetItemString(1,'payment_type_code_to')
ls_payment_sub_type_code_fr 		    = dw_parameter.GetItemString(1,'payment_sub_type_code_fr')
ls_payment_sub_type_code_to 		    = dw_parameter.GetItemString(1,'payment_sub_type_code_to')
ll_cheque_no							    = dw_parameter.GetItemNumber(1,'cheque_no')
ll_xmit_no								   = dw_parameter.GetItemNumber(1,'xmit_no')
ldtm_issue_date						   = dw_parameter.GetItemDateTime(1,'issue_date')	
ls_explanation							   = dw_parameter.GetItemString(1,'explanation')
ldcm_amount							   = dw_parameter.getitemdecimal(1,'amount')
ldt_cost_relief_date					   = dw_parameter.getitemdate(1,'cost_relief_date')
ll_cost_alloc_no_to 					  = dw_parameter.GetItemNumber(1,'cost_alloc_no_to')
ll_cost_alloc_operation_no_to 	      = dw_parameter.GetItemNumber(1,'cost_alloc_operation_no_to')
ll_cost_alloc_no_fr 					 = dw_parameter.GetItemNumber(1,'cost_alloc_no_fr')
ll_cost_alloc_operation_no_fr 	     = dw_parameter.GetItemNumber(1,'cost_alloc_operation_no_fr')
ls_benefit_type 						= dw_parameter.GetItemString(1,'benefit_type')

ls_message_receiving_salary_flag = ""
ls_message_self_insured_flag = ""


ldtm_current_datetime = f_server_datetime()
	
if isnull(ls_txn_type_code) or ls_txn_type_code = "" then
	MESSAGEBOX("Warning","Must enter a txn type before a seach can be performed")
	dw_parameter.SETCOLUMN('txn_type_code')
	dw_parameter.setfocus()
	return 
end if

if isnull(ls_txn_sub_type_code) or ls_txn_sub_type_code = ""  then
	MESSAGEBOX("Warning","Must enter a txn sub type before a seach can be performed")
	dw_parameter.SETCOLUMN('txn_sub_type_code')
	dw_parameter.setfocus()
	return 
end if

if ll_claim_no > 0 then
	
	select isnull(count(*),0)
	into :ll_no_claims
	from CLAIM
	where claim_no = :ll_claim_no;
	SQLCA.nf_handle_error("w_claim_cost_maintenance","select FROM CLAIM","retrieve by claim no")	
	
	
	if ll_no_claims = 0 then
		MESSAGEBOX("Warning","Must supply a valid claim no")
		dw_parameter.SETCOLUMN('claim_no')
		dw_parameter.setfocus()
		return
	end if
	
	
end if



choose case ls_txn_type_code
	
	case "J" /* ADJUSTMENT */
		
	
		if  ls_txn_sub_type_code = '2' THEN
		ELSE
			if isnull(ll_claim_no) or ll_claim_no = 0 then
				messagebox("Warning","Must supply a claim no")
				dw_parameter.SETCOLUMN('claim_no')
				dw_parameter.setfocus()
				return
			end if
			
			
		END IF
	
		choose case ls_txn_sub_type_code
			case "2"  /* CANCEL CHEQUE or DEPOSIT */
				
				ls_explanation_desc = 'CANCEL CHEQ/DEP :'
				
				rb_pay_all_types.checked = true 
				ls_payment_types = 'all'
		
				IF isnull(ll_cheque_no) and isnull(ll_xmit_no) then
					messagebox("Warning","Must enter a cheque no or direct deposit no to cancel a cheque or deposit")
					dw_parameter.SETCOLUMN('cheque_no')
					dw_parameter.setfocus()
					return
				elseif ll_cheque_no > 0 and ll_xmit_no > 0 then
					messagebox("Warning","Both cheque no and deposit no cannot be entered at same time")
					dw_parameter.SETCOLUMN('xmit_no')
					dw_parameter.setfocus()
					return
				elseif ll_claim_no > 0 and ll_cheque_no > 0 then 
					messagebox("Warning","Claim No and Cheque no is not required to cancel a cheque. Only Cheque No")
					dw_parameter.SETCOLUMN('claim_no')
					dw_parameter.setfocus()
					return
				elseif ll_xmit_no > 0 and ll_claim_no = 0 AND ll_provider_no = 0 then					
					messagebox("Warning","Claim No OR Provider No are required when direct deposit no is entered")
					dw_parameter.SETCOLUMN('claim_no')
					dw_parameter.setfocus()
					return
				ELSEIF ll_xmit_no > 0 AND ll_claim_no > 0 AND ll_provider_no > 0 THEN
					MessageBox("Warning","Both Claim No and Provider No are not required to cancel a deposit. Please enter only one.")
					dw_parameter.SetColumn('claim_no')
					dw_parameter.SetFocus()
					RETURN
				elseif ll_xmit_no = 0 and ll_cheque_no = 0 then	
					messagebox("Warning","Must enter either a cheque no or deposit no")
					dw_parameter.SETCOLUMN('cheque_no')
					dw_parameter.setfocus()
					return
				end if
				
				IF IsNull(ls_provider_type_code) AND ll_provider_no > 0 THEN
					MessageBox('Warning','Please select a Provider Type if searching on Provider No.')
					RETURN 
				END IF
			
				if isnull(ldtm_issue_date) then
					messagebox("Warning","Must enter a Cheque Issued Date")
					dw_parameter.SETCOLUMN('issue_date')
					dw_parameter.setfocus()
					return
				end if
				
				
				if ldtm_issue_date > ldtm_current_datetime then
					messagebox("Warning","Issue Date must be less than today Date")
					dw_parameter.SETCOLUMN('issue_date')
					dw_parameter.setfocus()
					return
				end if
				

				if ldtm_issue_date < datetime(date('1980/01/01')) then
					messagebox("Warning","Issue date must be Greater than '1980/01/01'")
					dw_parameter.SETCOLUMN('issue_date')
					dw_parameter.setfocus()
					return
				end if
				
				
				
				if ll_cheque_no > 0 then
						
						SELECT dbo.CHEQUE_HEADER.reconciled_code,
								 dbo.CHEQUE_HEADER.cheque_amount
						INTO  :ls_cheque_reconciled_code,
								:idcm_cheque_amount
						FROM dbo.CHEQUE_HEADER  
						WHERE dbo.CHEQUE_HEADER.cheque_no = :ll_cheque_no   
						using SQLCA;
						SQLCA.nf_handle_error("SELECT * FROM CHEQUE_HEADER","dw_transaction_list","retrieve by claim") 
 					
						ls_explanation_desc = ls_explanation_desc + ' CHEQUE NO IS: ' + STRING(ll_cheque_no)
				ELSE
						ls_explanation_desc = ls_explanation_desc + ' DEPOSIT NO IS: ' + STRING(ll_xmit_no)
				end if
				
				IF ll_xmit_no > 0 THEN
					IF ll_claim_no > 0 THEN					
						SELECT	Count(*)
						INTO		:li_count
						FROM		APPLIED_CLAIM_TXN
						WHERE	cheque_deposit_date = :ldtm_issue_date
						AND		direct_deposit_xmit_no = :ll_xmit_no
						AND       claim_no                         = :ll_claim_no
						AND		maintain_allowed_flag = 'Y'
						using SQLCA;
						SQLCA.nf_handle_error("SELECT Count(*) FROM APPLIED_CLAIM_TXN","cb_search","retrieve direct deposit count") 
						
						IF li_count = 0 THEN
							MessageBox('No records','There are no maintainable Direct Deposits for:' &
															+'~nClaim: ' + String(ll_claim_no) &
															+'~nXmit No: ' + String(ll_xmit_no) &
															+'~nDirect Deposit Date: ' + String(ldtm_issue_date,'yyyy-mm-dd'),StopSign!)
							RETURN
						END IF
					ELSEIF ll_provider_no > 0 THEN
						SELECT	Count(*)
						INTO		:li_count
						FROM		APPLIED_CLAIM_TXN
						WHERE	cheque_deposit_date = :ldtm_issue_date
						AND		direct_deposit_xmit_no = :ll_xmit_no
						AND       recipient_no                         = :ll_provider_no
						AND       recipient_type_code          = :ls_provider_type_code
						AND		maintain_allowed_flag = 'Y'
						using SQLCA;
						SQLCA.nf_handle_error("SELECT Count(*) FROM APPLIED_CLAIM_TXN","cb_search","retrieve direct deposit count") 
						
						IF li_count = 0 THEN
							MessageBox('No records','There are no maintainable Direct Deposits for:' &
															+'~nProvider No.: ' + String(ll_provider_no) &
															+'~nXmit No: ' + String(ll_xmit_no) &
															+'~nDirect Deposit Date: ' + String(ldtm_issue_date,'yyyy-mm-dd'),StopSign!)
							RETURN
						END IF
					END IF
				ELSEIF ll_cheque_no > 0 THEN					
					SELECT	Count(*)
					INTO		:li_count
					FROM		APPLIED_CLAIM_TXN
					WHERE	cheque_deposit_date = :ldtm_issue_date
					AND		cheque_no = :ll_cheque_no
					AND		maintain_allowed_flag = 'Y'
					using SQLCA;
					SQLCA.nf_handle_error("SELECT * FROM APPLIED_CLAIM_TXN","cb_search","retrieve cheque count") 
					
					IF li_count = 0 THEN
						MessageBox('No records','There are no maintainable Cheques for:' &
														+'~nCheque No: ' + String(ll_cheque_no) &
														+'~nCheque Date: ' + String(ldtm_issue_date,'yyyy-mm-dd'),StopSign!)
						RETURN
					END IF
				END IF
				
			case "3"  /* OVERPAYMENT RECOVERY */	
				
				ls_explanation_desc = 'OVERPAYMENT RECOVERY '

				if ls_benefit_type = '' OR ISnULL(ls_benefit_type) THEN
					messagebox("Warning","Must supply Benefit Level Type")
					dw_parameter.SETCOLUMN('benefit_type')
					dw_parameter.setfocus()
					return
				
				END IF
				
			case "4"  /* TAX CORRECTION */
				rb_pay_act.checked = true 
				ls_payment_types = 'act'
				ls_explanation_desc = 'TAX CORRECTION '
				
			case "5"  /* Third Party Recovery */
				ls_explanation_desc = 'THIRD PARTY RECOVERY'
				
			case "Q"	 /* Rehab Qty Correction */
				rb_pay_act.checked = true 
				ls_payment_types = 'act'
				ls_explanation_desc = 'REHAB QTY CORRECTION'

			case "R"	 /* Receiving Salary Correction */
				rb_pay_all_types.checked = true 
				ls_payment_types = 'all'
				ls_explanation_desc = 'REC SALARY CANCELLATION'
		
		end choose
case "T" /* TRANSFER */
	
		if isnull(ll_claim_no) or ll_claim_no = 0 then
			messagebox("Warning","Must supply a claim no")
			dw_parameter.SETCOLUMN('claim_no')
			dw_parameter.setfocus()
			return
		end if
			
		choose case ls_txn_sub_type_code
				
			case "7"
				
				if isnull(ll_cost_alloc_no_fr) or ll_cost_alloc_no_fr = 0 then
					messagebox("Warning","must enter Cost Allocation # FR")
					dw_parameter.SETCOLUMN('cost_alloc_no_fr')
					dw_parameter.setfocus()
					return
				end if
				
				if isnull(ll_cost_alloc_no_to) or ll_cost_alloc_no_to = 0 then
					messagebox("Warning","must enter Cost Allocation # TO")
					dw_parameter.SETCOLUMN('cost_alloc_no_to')
					dw_parameter.setfocus()
					return
				end if
				
				if isnull(ll_cost_alloc_operation_no_fr) or ll_cost_alloc_operation_no_fr = 0 then
					messagebox("Warning","must enter Cost Allocation Operation # FR")
					dw_parameter.SETCOLUMN('cost_alloc_operation_no_fr')
					dw_parameter.setfocus()
					return
				end if
				
				if isnull(ll_cost_alloc_operation_no_to) or ll_cost_alloc_operation_no_to = 0 then
					messagebox("Warning","must enter Cost Allocation Operation # TO")
					dw_parameter.SETCOLUMN('cost_alloc_operation_no_to')
					dw_parameter.setfocus()
					return
				end if
				

				if (ll_cost_alloc_no_fr = ll_cost_alloc_no_to ) and (ll_cost_alloc_operation_no_fr = ll_cost_alloc_operation_no_to) then
				 	messagebox("Warning","Cost Allocation from and to must be different")
					dw_parameter.SETCOLUMN('cost_alloc_no_fr')
					dw_parameter.setfocus()
					return
				end if
			
				ls_explanation_desc = 'C/A Xfer-FR: ' + STRING(ll_cost_alloc_no_fr,'######') + '/' + STRING(ll_cost_alloc_operation_no_fr,'####') + ' TO: ' +  STRING(ll_cost_alloc_no_to,'######') + '/' + STRING(ll_cost_alloc_operation_no_to,'####')
			
			
			case "8"  /* COST RELIEF */
				
					
				if isnull(ll_cost_alloc_no_fr) or ll_cost_alloc_no_fr = 0 then
					messagebox("Warning","must enter Cost Allocation # FR")
					dw_parameter.SETCOLUMN('cost_alloc_no_fr')
					dw_parameter.setfocus()
					return
				end if
				
				if isnull(ll_cost_alloc_no_to) or ll_cost_alloc_no_to = 0 then
					messagebox("Warning","must enter Cost Allocation # TO")
					dw_parameter.SETCOLUMN('cost_alloc_no_to')
					dw_parameter.setfocus()
					return
				end if
				
				if isnull(ll_cost_alloc_operation_no_fr) or ll_cost_alloc_operation_no_fr = 0 then
					messagebox("Warning","must enter Cost Allocation Operation # FR")
					dw_parameter.SETCOLUMN('cost_alloc_operation_no_fr')
					dw_parameter.setfocus()
					return
				end if
				
				if isnull(ll_cost_alloc_operation_no_to) or ll_cost_alloc_operation_no_to = 0 then
					messagebox("Warning","must enter Cost Allocation Operation # TO")
					dw_parameter.SETCOLUMN('cost_alloc_operation_no_to')
					dw_parameter.setfocus()
					return
				end if
				

				if (ll_cost_alloc_no_fr = ll_cost_alloc_no_to ) and (ll_cost_alloc_operation_no_fr = ll_cost_alloc_operation_no_to) then
				 	messagebox("Warning","Cost Allocation from and to must be different")
					dw_parameter.SETCOLUMN('cost_alloc_no_fr')
					dw_parameter.setfocus()
					return
				end if
				
				IF ISNULL(ldt_cost_relief_date) THEN
					messagebox("Warning","must enter Cost Relief date")
					dw_parameter.SETCOLUMN('cost_relief_date')
					dw_parameter.setfocus()
					return
				END IF
			
				if ldt_cost_relief_date < date('1980/01/01') then
					messagebox("Warning","Cost Relief date must be Greater than '1980/01/01'")
					dw_parameter.SETCOLUMN('cost_relief_date')
					dw_parameter.setfocus()
					return
				end if
				
					
				if ldt_cost_relief_date > DATE(ldtm_current_datetime) then
					messagebox("Warning","Cost Relief Date must be less than today Date")
					dw_parameter.SETCOLUMN('cost_relief_date')
					dw_parameter.setfocus()
					return
				end if
				
				ls_explanation_desc = 'Cost Relief Xfer-Effective: ' + string(ldt_cost_relief_date,'YYYY-MM-DD')
			
			
			
			case "6"  /* Claim */
					
				if isnull(ll_claim_no) or ll_claim_no = 0 then
					Messagebox("Warning","Must supply a 'claim no'")
					dw_parameter.SETCOLUMN('claim_no')
					dw_parameter.setfocus()
					return
				end if
				
				if isnull(ll_claim_no_to) or ll_claim_no_to = 0 then
					Messagebox("Warning","Must supply a 'claim no to'")
					dw_parameter.SETCOLUMN('claim_no_to')
					dw_parameter.setfocus()
					return
				end if
				
				ls_explanation_desc = 'Claim Xfer-FR: ' + string(ll_claim_no,'########') + ' TO: ' + string(ll_claim_no_to,'########')
		
					
			case "9"  /* Payment Type*/
		
				if isnull(ls_payment_type_code_fr)		or	ls_payment_type_code_fr 			=	'' then	
					Messagebox("Warning","Must supply a Payment Type fr")
					dw_parameter.SETCOLUMN('payment_type_code_fr')
					dw_parameter.setfocus()
					return
				end if
				
				if isnull(ls_payment_type_code_to)		or	ls_payment_type_code_to 			=	'' then
					Messagebox("Warning","Must supply a Payment Type to")
					dw_parameter.SETCOLUMN('payment_type_code_to')
					dw_parameter.setfocus()
					return
				end if	
				
				if (ls_payment_sub_type_code_fr = ls_payment_sub_type_code_to ) and (ls_payment_type_code_fr = ls_payment_type_code_to ) then	
					Messagebox("Warning","Payment Type/Sub Type must be different")
					dw_parameter.SETCOLUMN('payment_type_code_fr')
					dw_parameter.setfocus()
					return
				end if
			

				if ls_payment_sub_type_code_fr > '' and ls_payment_sub_type_code_to > '' then
					ls_explanation_desc = 'Pymt Type Xfer-FR: ' + ls_payment_type_code_fr + '/' + ls_payment_sub_type_code_fr + ' TO: ' +  ls_payment_type_code_to + '/' + ls_payment_sub_type_code_to
				elseif ls_payment_sub_type_code_fr > '' then
					ls_explanation_desc = 'Pymt Type Xfer-FR: ' + ls_payment_type_code_fr + '/' + ls_payment_sub_type_code_fr + ' TO: ' +  ls_payment_type_code_to 
				elseif ls_payment_sub_type_code_to > '' then
					ls_explanation_desc = 'Pymt Type Xfer-FR: ' + ls_payment_type_code_fr +  ' TO: ' +  ls_payment_type_code_to + '/' + ls_payment_sub_type_code_to
				else
					ls_explanation_desc = 'Pymt Type Xfer-FR: ' + ls_payment_type_code_fr + ' TO: ' +  ls_payment_type_code_to 
				end if
				
		
		END CHOOSE
		
end choose

IF ISNULL(ls_explanation) OR ls_explanation ='' THEN
	ls_explanation	= ls_explanation_desc						
	dw_parameter.SETITEM(1,'explanation',ls_explanation)	
END IF


/* call function to set if receiviving salary and self insured txn's should be returned */
wf_determine_security(ls_inc_receiving_salary_flag,ls_inc_self_insured_flag,ls_txn_type_code ,ls_txn_sub_type_code )

if ls_inc_receiving_salary_flag = "2" then
	ls_message_receiving_salary_flag = " All receiving salary transactions have been removed. " 
end if

if ls_inc_self_insured_flag = "2" then
	ls_message_self_insured_flag = " All self insured employer transactions have been removed."
end if


if ll_claim_no > 0 then
	ls_message = "This claim (" + string(ll_claim_no) + ") has no maintainable payment transactions."
else
	ls_message = "This deposit or cheque has no maintainable payment transactions."
end if


if rb_pay_act.checked = true then
	ls_message = ls_message + "Non Account Payment txn's have been removed. "
elseif rb_pay_loe.checked = true then
	ls_message = ls_message + "Account Payment txn's have been removed. "
end if


/* cancel cheque or deposit the retrieval arg will be different */
IF ls_txn_type_code = "J" and ls_txn_sub_type_code = "2" then
	/* direct deposit */
	if (ll_claim_no > 0 OR ll_provider_no > 0 ) and ll_xmit_no > 0  then 	

		ll_no_rows = dw_transaction_list.Retrieve(ll_claim_no,ll_provider_no, ls_provider_type_code,0,ll_xmit_no,ldtm_issue_date,ls_payment_types,ls_inc_receiving_salary_flag ,  ls_inc_self_insured_flag)
		SQLCA.nf_handle_error("w_claim_cost_maintenance","dw_transaction_list","retrieve by claim")
		
		if ll_no_rows = 0 then 
			ls_message = ls_message + 'For Xmit No (' + string(ll_xmit_no) + ") " 
		end if
		
	/* CHEQUE NO */
	ELSEif ll_cheque_no > 0 then
		 
			select count(*) 
			into :ll_no_txn
			FROM 	APPLIED_CLAIM_TXN
			where cheque_no = :ll_cheque_no							and 	
					cheque_deposit_date    = :ldtm_issue_date		and 	
					txn_type_code = "J" 									and	
					txn_sub_type_code = "2" 
			using SQLCA;
			SQLCA.nf_handle_error("w_claim_cost_maintenance","APPLIED_CLAIM_TXN","cheque no, issue date")
				
			if ll_no_txn > 0 then
				MESSAGEBOX("Warning","This cheque has already been cancelled.")
			end if

	
			ll_no_rows = dw_transaction_list.Retrieve(0,0,'',ll_cheque_no,0,ldtm_issue_date,ls_payment_types,ls_inc_receiving_salary_flag ,  ls_inc_self_insured_flag)
			SQLCA.nf_handle_error("w_claim_cost_maintenance","dw_transaction_list","retrieve by claim")
			
			if ll_no_rows = 0 then 
				ls_message = ls_message + ' Cheque No (' + string(ll_cheque_no) + ') . Issue Date (' + string(ldtm_issue_date,'yyyy-mm-dd') + " ) ."  
			end if
		
	END IF
ELSE
	/* claim no */
	

	
	
	
	ll_no_rows = dw_transaction_list.Retrieve(ll_claim_no,0, '', 0,0,ldtm_issue_date,ls_payment_types,ls_inc_receiving_salary_flag ,  ls_inc_self_insured_flag)
	SQLCA.nf_handle_error("w_claim_cost_maintenance","dw_transaction_list","retrieve by claim")	
	
	
end if


if ls_message_receiving_salary_flag <> "" then
	ls_message = ls_message + ls_message_receiving_salary_flag
end if

if ls_message_self_insured_flag <> "" then
	ls_message = ls_message + ls_message_self_insured_flag
end if


if ll_no_rows = 0 THEN
	messagebox("Warning",ls_message)
	dw_transaction_list.reset()
	dw_transaction_list.setredraw(true)
	return
else	 
	for ll_row = 1 to ll_no_rows
		dw_transaction_list.setitem(ll_row,'new_txn_type_code',ls_txn_type_code)
		dw_transaction_list.setitem(ll_row,'new_txn_sub_type_code',ls_txn_sub_type_code)
	next
end if
	
/* apply filtering based on txn type/sub type - if required */

/*  cost allocation & cost relief */
if ls_txn_type_code = "T" and (ls_txn_sub_type_code = "7" or ls_txn_sub_type_code = "8") then
				
// both cost allocation and cost relief will filter by the
//		cost allocation 
	ls_filter = "cost_alloc_no = " + string(dw_parameter.GetItemNumber(1,'cost_alloc_no_fr')) + ' and ' 
	ls_filter = ls_filter + "cost_alloc_operation_no = " +  string(dw_parameter.GetItemNumber(1,'cost_alloc_operation_no_fr'))
	dw_transaction_list.setfilter(ls_filter)
	ll_org_rc = dw_transaction_list.filter()

	if dw_transaction_list.rowcount() = 0 then
	else
		
		if ls_txn_sub_type_code = "8" then
			
			for ll_row = 1 to dw_transaction_list.rowcount()				
				ldtm_paid_from_date					= dw_transaction_list.GetItemDateTime(ll_row,'paid_from_date')
				ldtm_paid_to_date				 	= dw_transaction_list.GetItemDateTime(ll_row,'paid_to_date')
				ldtm_payment_processed_date 	= dw_transaction_list.GetItemDateTime(ll_row,'payment_processed_date')
				ll_payment_no							= dw_transaction_list.GetItemNumber(ll_row,'payment_no')	
				ls_fromto_dates_flag					= dw_transaction_list.GetItemString(ll_row,'fromto_dates_flag')
				
				if isnull(ldtm_paid_from_date) or isnull(ldtm_paid_to_date) then
					if ls_fromto_dates_flag	= 'N' THEN
						// check that processed date is > cost relief date
						// for those payment types that do not require paid from/to dates
						if lb_proc_date_checked = FALSE then
							// first time that payment proc date has been checked
							ldtm_min_payment_processed_date	= ldtm_payment_processed_date
							ll_min_no_date_payment_no		  	= ll_payment_no
							ldtm_max_payment_processed_date	= ldtm_payment_processed_date
							ll_max_no_date_payment_no		  	= ll_payment_no
							lb_proc_date_checked = TRUE
						else
							if ldtm_payment_processed_date < ldtm_min_payment_processed_date then
								// re-assign minimum
								ldtm_min_payment_processed_date	= ldtm_payment_processed_date
								ll_min_no_date_payment_no			= ll_payment_no
							ELSEIF ldtm_payment_processed_date > ldtm_max_payment_processed_date then
								// re-assign maximum
								ldtm_max_payment_processed_date	= ldtm_payment_processed_date
								ll_max_no_date_payment_no			= ll_payment_no
							END IF
						END IF
						
						IF Date(ldtm_payment_processed_date) >= ldt_cost_relief_date THEN
							// OK
						ELSE
							// no-period payent's processed date is prior to relief date, so delete from list
							dw_transaction_list.deleterow(ll_row)
							ll_row = ll_row - 1
						END IF
					ELSE
						dw_transaction_list.deleterow(ll_row)
						ll_row = ll_row - 1
					END IF
					
				ELSE
					if lb_paid_fromto_date_checked = FALSE then
						// first time that paid from/to dates have been checked
						ldtm_min_paid_from_date = ldtm_paid_from_date
						ldtm_max_paid_to_date   = ldtm_paid_to_date	
						ll_min_payment_no		   = ll_payment_no	
						ll_max_payment_no		   = ll_payment_no
						lb_paid_fromto_date_checked = TRUE
					else
						if ldtm_paid_from_date < ldtm_min_paid_from_date then
							// re-assign minimum
							ldtm_min_paid_from_date = ldtm_paid_from_date
							ll_min_payment_no		= ll_payment_no	
						END IF
						if ldtm_max_paid_to_date < ldtm_paid_to_date	then
							// re-assign maximum
							ldtm_max_paid_to_date = ldtm_paid_to_date	
							ll_max_payment_no		= ll_payment_no	
						END IF
					end if
					
					IF ldt_cost_relief_date > date(ldtm_paid_to_date) THEN
						// benefit period is prior to relief date, so delete from list
						dw_transaction_list.deleterow(ll_row)
						ll_row = ll_row - 1
					END IF		
				END IF

			next 
			
			IF String(ldtm_min_payment_processed_date,'yyyy-mm-dd') = '1900-01-01' THEN
				// since there were no payments that have a payment type that does not require benefit periods
				// then there can be no violation of BRs
				lb_no_proc_date_violation = TRUE
			END IF
			
			IF String(ldtm_min_paid_from_date,'yyyy-mm-dd') = '1900-01-01' THEN
				// since there were no payments that have a payment type that require benefit periods
				// then there can be no violation of BRs
				lb_no_period_date_violation = TRUE
			END IF
			
			IF lb_no_proc_date_violation = FALSE AND lb_no_period_date_violation = FALSE THEN
				// there are no-benefit-period payments & benefit-period payments
				IF Date(ldtm_min_paid_from_date) < Date(ldtm_min_payment_processed_date) THEN
					IF ldt_cost_relief_date< Date(ldtm_min_paid_from_date) THEN
						ls_date_message = "The Cost Relief Date entered must be >= the minimum payment paid from date" &
												+"~nfor payments that require a payment period." &
												+"~n~tMinimum paid from date: " + string(ldtm_min_paid_from_date,'yyyy-mm-dd') &
												+"~n~tPayment number: " + string(ll_min_payment_no)
						lb_period_date_violation = TRUE
					END IF
				ELSE
					IF ldt_cost_relief_date< Date(ldtm_min_payment_processed_date) THEN
						ls_date_message = "The Cost Relief Date entered must be >= the minimum payment processed date" &
												+"~nfor payments that do not require a payment period." &
												+"~n~tMinimum payment processed date: " + string(ldtm_min_payment_processed_date,'yyyy-mm-dd') &
												+"~n~tPayment number: " + string(ll_min_no_date_payment_no)
						lb_proc_date_violation = TRUE
					END IF
				END IF
				
				IF Date(ldtm_max_paid_to_date) > Date(ldtm_max_payment_processed_date) THEN
					IF ldt_cost_relief_date > Date(ldtm_max_paid_to_date) THEN
						ls_date_message = "The Cost Relief Date entered must be <= the maximum payment paid to date" &
												+"~nfor payments that require a payment period." &
												+"~n~tMaximum payment paid to date: "+string(ldtm_max_paid_to_date,'yyyy-mm-dd') &
												+"~n~tPayment number: " + string(ll_max_payment_no)
						lb_period_date_violation = TRUE
					END IF
				ELSE
					IF ldt_cost_relief_date > Date(ldtm_max_payment_processed_date) THEN
						ls_date_message = "The Cost Relief Date entered must be <= the maximum payment paid processed date" &
												+"~nfor payments that do not require a payment period." &
												+"~n~tMaximum payment processed date: "+string(ldtm_max_payment_processed_date,'yyyy-mm-dd') &
												+"~n~tPayment number: " + string(ll_max_no_date_payment_no)
						lb_proc_date_violation = TRUE
					END IF
				END IF
				
			ELSEIF lb_no_proc_date_violation = TRUE AND lb_no_period_date_violation = FALSE THEN
				// there aren't any no-benefit-period payments & there are benefit-period payments
				IF ldt_cost_relief_date< Date(ldtm_min_paid_from_date) THEN
					ls_date_message = "The Cost Relief Date entered must be >= the minimum payment paid from date" &
											+"~nfor payments that require a payment period." &
											+"~n~tMinimum paid from date: " + string(ldtm_min_paid_from_date,'yyyy-mm-dd') &
											+"~n~tPayment number: " + string(ll_min_payment_no)
					lb_period_date_violation = TRUE
				END IF
				IF ldt_cost_relief_date > Date(ldtm_max_paid_to_date) THEN
					ls_date_message = "The Cost Relief Date entered must be <= the maximum payment paid to date" &
												+"~nfor payments that require a payment period." &
												+"~n~tMaximum payment paid to date: "+string(ldtm_max_paid_to_date,'yyyy-mm-dd') &
												+"~n~tPayment number: " + string(ll_max_payment_no)
					lb_period_date_violation = TRUE
				END IF
				
			ELSEIF lb_no_proc_date_violation = FALSE AND lb_no_period_date_violation = TRUE THEN
				// there are no-benefit-period payments & there aren't any benefit-period payments
				IF ldt_cost_relief_date< Date(ldtm_min_payment_processed_date) THEN
					ls_date_message = "The Cost Relief Date entered must be >= the minimum payment processed date" &
												+"~nfor payments that do not require a payment period." &
												+"~n~tMinimum payment processed date: " + string(ldtm_min_payment_processed_date,'yyyy-mm-dd') &
												+"~n~tPayment number: " + string(ll_min_no_date_payment_no)
					lb_proc_date_violation = TRUE
				END IF
				IF ldt_cost_relief_date > Date(ldtm_max_payment_processed_date) THEN
					ls_date_message = "The Cost Relief Date entered must be <= the maximum payment paid processed date" &
												+"~nfor payments that do not require a payment period." &
												+"~n~tMaximum payment processed date: "+string(ldtm_max_payment_processed_date,'yyyy-mm-dd') &
												+"~n~tPayment number: " + string(ll_max_no_date_payment_no)
					lb_proc_date_violation = TRUE
				END IF
				
			END IF
			
			IF lb_proc_date_violation = TRUE OR lb_period_date_violation = TRUE THEN
				MessageBox('Error',ls_date_message)
				dw_transaction_list.reset()
				dw_transaction_list.setredraw(true)
				RETURN
			ELSE
				// OK
			END IF
				
	end if
end if


	dw_transaction_list.setredraw(true)
		
	if dw_transaction_list.rowcount() = 0 then
		dw_transaction_list.reset()
		dw_transaction_list.setredraw(true)
		messagebox("Warning","No rows returned for cost relief/cost allocation transfer.")
		return
	end if	
end if
	
				
				
/* PAYMENT TYPE TRANSFER */
IF ls_txn_type_code = 'T' AND ls_txn_sub_type_code = '9'  THEN
	
	if ls_payment_sub_type_code_fr <> '' then
		ls_filter = "payment_type_code = '" + ls_payment_type_code_fr + "'" + " and payment_sub_type_code = '" + ls_payment_sub_type_code_fr + "'"	
	else
		ls_filter = "payment_type_code = '" + ls_payment_type_code_fr + "'"
	end if
	
	ll_rc = dw_transaction_list.setfilter(ls_filter)	
	ll_rc = dw_transaction_list.filter()
	ib_filter_applied = true
	
	if dw_transaction_list.rowcount() = 0 then
		dw_transaction_list.reset()
		dw_transaction_list.setredraw(true)	
		messagebox("Warning","No payments available for maintenance for payment type/sub type:  payment type = " +  ls_payment_type_code_fr + " and sub type = " +  ls_payment_sub_type_code_fr )
		return
	end if
	
END IF

/* COST TRANSFER */
IF ls_txn_type_code = 'T' THEN
	IF ls_txn_sub_type_code = '7' OR ls_txn_sub_type_code = '8' THEN
		ib_filter_applied = true
	END IF
END IF

/* ADJUsTMENT = REHAB QTY - filter only the rows that have rehab authorization */
IF ls_txn_type_code = 'J' AND ls_txn_sub_type_code = 'Q'  THEN

	ls_filter = "authorization_no > 0 "
	ll_rc = dw_transaction_list.setfilter(ls_filter)
	is_filter_parm = ls_filter
			
	ll_rc = dw_transaction_list.filter()
	ib_filter_applied = true
	
	if dw_transaction_list.rowcount() = 0 then
		dw_transaction_list.reset()
		dw_transaction_list.setredraw(true)
		messagebox("Warning","No rows return having authorization no > 0 for performing rehab qty correction")
		return
	end if
	
END IF

/* ADJUsTMENT - tax correction - act payment only - filter only the rows  */
IF ls_txn_type_code = 'J' AND ls_txn_sub_type_code = '4'  THEN

	ls_filter = "tax_rate > 0 " 
	
	ll_rc = dw_transaction_list.setfilter(ls_filter)
	is_filter_parm = ls_filter
			
	ll_rc = dw_transaction_list.filter()
	ib_filter_applied = true
	
	if dw_transaction_list.rowcount() = 0 then
		dw_transaction_list.reset()
		dw_transaction_list.setredraw(true)
		messagebox("Warning","No rows return to perform tax correction - based on filter by tax_rate > 0 ")
		return
	end if
	
END IF

/* copy all rows into datastore - datastore used for filtering*/
dw_transaction_list.RowsCopy(1,dw_transaction_list.rowcount(),Primary!,lds_txn_list,1,Primary!)

/* when cancelling a cheque/deposit */
IF ls_txn_type_code = 'J' AND ls_txn_sub_type_code = '2'  THEN
	PARENT.SetRedraw(FALSE)
	/* IF CANCELLING A DEPOSIT - MAY BE FOR MORE THAN ONCE RECEIPieNT - (P10151-140 - unless cancelling a deposit for Service Provider, then the SP would be the recipient we want to cancel) */
	IF ll_xmit_no > 0 THEN
		
		lds_txn_list.setsort("recipient_no D")
		lds_txn_list.sort()
		
		// temporarily add filter to only have match on dd xmit num
		// this allows defaulting days/hrs on split payments where
		// the other txns in split are not same xmit num
		ls_filter = 'direct_deposit_xmit_no = ' + String(ll_xmit_no)
		ll_rc = lds_txn_list.SetFilter(ls_filter)
		ll_rc = lds_txn_list.Filter()
		ll_rc = lds_txn_list.rowcount()
		
		for ll_rc = 1 to lds_txn_list.rowcount()
				
			ll_recipient_no = lds_txn_list.GetItemNumber(ll_rc,'recipient_no')
			ls_recipient_name = lds_txn_list.GetItemString(ll_rc,'recipient_name')
			ls_recipient_type_code = lds_txn_list.GetItemString(ll_rc,'recipient_type_code')
		
			if ll_rc = 1 then
				ll_prev_recipient_no = ll_recipient_no
				ll_no_of_recipients++
				
				lstr_window_message.al_doubleparm[ll_no_of_recipients] = ll_recipient_no
				lstr_window_message.as_stringparm[ll_no_of_recipients] = ls_recipient_type_code  + ls_recipient_name

			else
				
				if ll_recipient_no = ll_prev_recipient_no then
				else
					ll_no_of_recipients++
					ll_prev_recipient_no = ll_recipient_no
						
					lstr_window_message.al_doubleparm[ll_no_of_recipients] = ll_recipient_no
					lstr_window_message.as_stringparm[ll_no_of_recipients] = ls_recipient_type_code + ls_recipient_name
				end if
			end if
		next
				
		IF ll_no_of_recipients > 1 THEN
			IF ll_provider_no = 0 THEN
				OpenWithParm(w_popup_enter_recipient_no,lstr_window_message)
				/* GRAB THE TXN UNIT OF WORK FROM THE STRUCTURE */
				lstr_window_message = Message.PowerObjectParm
				ll_recipient_no = lstr_window_message.al_doubleparm[1]
			
				if ll_recipient_no = 0 then
					PARENT.SetRedraw(TRUE)
					messagebox("Warning","Must Choose a Recipient when multiple exist")
					return
				end if
			ELSE
				// P10151-140 - If cancelling a deposit for a service provider, the user shouldn't need to select a recipient, as the s.p. was indicated as the recipient in the parameter dw.
				ll_recipient_no = ll_provider_no
			END IF
							
			ls_filter = "recipient_no = " + string(ll_recipient_no)
			
					
			ll_rc = dw_transaction_list.setfilter(ls_filter)	
			ll_rc = dw_transaction_list.filter()
			
			/* This will be used in the messaging command button - show messages */
			ib_filter_applied = true
			is_filter_parm = ls_filter
				
			if dw_transaction_list.rowcount() = 0 then 
				PARENT.SetRedraw(TRUE)
				messagebox("Warning","No rows return having based on recipient no " + string(ll_recipient_no) + " .")
				return
			else
		
			end if
			
			
		END IF
		
		// take filter off
		ls_filter = ''
		lds_txn_list.SetFilter(ls_filter)
		lds_txn_list.Filter()
	END IF
		
	/* trigger a selected all for cheque/deposits*/
	cbx_1.triggerevent(clicked!)
	
	// filter out other targets in split payment
	IF ll_xmit_no > 0 THEN
		ls_filter = 'direct_deposit_xmit_no = ' + String(ll_xmit_no)
		dw_transaction_list.SetFilter(ls_filter)
		dw_transaction_list.Filter()
	ELSE
		ls_filter = 'cheque_no = ' + String(ll_cheque_no)
		dw_transaction_list.SetFilter(ls_filter)
		dw_transaction_list.Filter()		
	END IF
	PARENT.SetRedraw(TRUE)

END IF


dw_transaction_list.sort()
dw_transaction_list.groupcalc()


if ib_filter_applied = false then 
	uo_filter.ENABLED = true
else
	uo_filter.ENABLED = false
END IF

		
if dw_transaction_list.rowcount() = 0 then 
	cb_apply.enabled = false
else
	cb_apply.enabled = true
end if



	
	
dw_transaction_list.setredraw(true)
SetPointer(oldpointer)

end event

type cbx_1 from checkbox within w_claim_cost_maintenance
integer x = 4443
integer y = 480
integer width = 64
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
end type

event clicked;long 		ll_row
long		ll_rc
long		ll_prescription_set_no
long		ll_txn_no
long		ll_no_related_txn
long		ll_checked
string 	ls_txn_type_code 
string 	ls_txn_sub_type_code
long		ll_count
string 	ls_payment_type_code
string 	ls_payment_sub_type_code
string 	ls_script = "cbx_1 - clicked"
string   ls_payment_method_code
decimal {2} ldcm_txn_amount,ldcm_total_amount
boolean lb_fail
long		ll_recipient_no
long		ll_ultimate_txn_no

string  ls_maintain_allowed_flag

decimal {2} ldcm_tax_amount

string		ls_benefit_type 
long 			ll_net_quantity
long			ll_payment_no
string 		ls_find
long 			ll_found
decimal {2} ldcm_remaining_days_lost
decimal {2} ldcm_remaining_hours_lost
decimal {2} ldcm_remaining_ult_txn_amount
decimal {2} ldcm_remaining_other_ult_txn_amount
decimal {2} ldcm_other_adj_ult_days
decimal {2} ldcm_other_adj_ult_hours
decimal {2} ldcm_net_days_lost
decimal {2} ldcm_net_hours_lost
decimal {2} ldcm_entered_amount

decimal {4} ldcm_portion_to_apply

long 			ll_rowcount
string			ls_split_payment_flag

boolean		lb_apply_remainder_days_hours

pointer oldpointer // Declares a pointer variable

// reset variable used in cb_apply
ib_oldest_to_newest = FALSE

if dw_transaction_list.rowcount() = 0 then
	messagebox("Warning","No Txn's returned to select")
	THIS.checked = false
	return -1
end if

dw_transaction_list.setredraw(false)
	

oldpointer = SetPointer(HourGlass!)


lb_fail = false


/* get txn type/sub type from parm screen */
ls_txn_type_code 					= dw_parameter.getitemstring(1,'txn_type_code')
ls_txn_sub_type_code			= dw_parameter.getitemstring(1,'txn_sub_type_code')
ls_benefit_type 					= dw_parameter.getitemstring(1,'benefit_type')


/* cancel a cheque or deposit */
IF ls_txn_type_code = "J" AND  ls_txn_sub_type_code = "2" then
	THIS.checked = TRUE
	lb_already_clicked = true
end if
		
	
if cbx_1.checked = TRUE then

	ll_rowcount = dw_transaction_list.rowcount()
	
	 FOR ll_row = 1 to ll_rowcount
		ll_txn_no 							= 	dw_transaction_list.getitemnumber(ll_row,'txn_no')
		ll_payment_no 						= 	dw_transaction_list.getitemnumber(ll_row,'payment_no')
		ll_ultimate_txn_no	 				= 	dw_transaction_list.getitemnumber(ll_row,'ultimate_txn_no')
		ls_payment_type_code 	 		= 	dw_transaction_list.getitemstring(ll_row,'payment_type_code')
		ls_payment_sub_type_code  	= 	dw_transaction_list.getitemstring(ll_row,'payment_sub_type_code')
		ls_payment_method_code		= 	dw_transaction_list.getitemstring(ll_row,'payment_method_code')
		ls_maintain_allowed_flag 		= 	dw_transaction_list.getitemstring(ll_row,'maintain_allowed_flag')
		ldcm_txn_amount 					=	dw_transaction_list.getitemdecimal(ll_row,'txn_amount')
		ldcm_tax_amount 					= 	dw_transaction_list.getitemdecimal(ll_row,'txn_tax_amount')
		ll_net_quantity						= 	dw_transaction_list.getitemnumber(ll_row,'net_quantity')
		ll_recipient_no						= 	dw_transaction_list.getitemnumber(ll_row,'recipient_no')
		ls_split_payment_flag				= 	dw_transaction_list.GetItemString(ll_row,'split_payment_flag')
		ldcm_entered_amount			=	ldcm_txn_amount * -1
		
		// reset variables
		lb_apply_remainder_days_hours = FALSE
		ldcm_remaining_days_lost = 0
		ldcm_remaining_hours_lost = 0
		ldcm_remaining_ult_txn_amount = 0
		ldcm_remaining_other_ult_txn_amount = 0
		ldcm_other_adj_ult_days = 0
		ldcm_other_adj_ult_hours = 0
		

		if ls_maintain_allowed_flag = "N" or ldcm_txn_amount = 0 Then
			Continue
		end if
		
		/* check to make sure that no unprocessed txn exist */
		select count(*) 
		into :ll_no_related_txn
		from UNAPPLIED_CLAIM_TXN
		where related_txn_no = :ll_txn_no
		using SQLCA; 
		SQLCA.nf_handle_error("select ~ from  UNAPPLIED_CLAIM_TXN",is_window_name,ls_script)
	
		IF ll_no_related_txn > 0 then
			MESSAGEBOX("ERROR","Unprocessed Txn's against dw_transaction_list txn prevent dw_transaction_list from being maintained. Txn no is "  + string(ll_txn_no) )
			dw_transaction_list.setitem(ll_row,'check_box',0)
			lb_fail = true
			continue
		end if
		
		
		IF ls_txn_type_code = "J" AND ls_txn_sub_type_code = "3" then
		
			ls_find = "recipient_no = " + string(ll_recipient_no)  + ' AND  check_box = 1 '
			
			ll_found =  dw_transaction_list.Find(ls_find, 1,  dw_transaction_list.RowCount())
			
			if ll_found > 0 then
			else
				ls_find =  "recipient_no <> " + string(ll_recipient_no) + " and check_box = 1" 
			
				ll_found =  dw_transaction_list.Find(ls_find, 1,  dw_transaction_list.RowCount())
			
				if ll_found > 0 then
					messagebox("error","Overpayment Recovery is limited to one receipient at one time - Please filter by Recipient Name - and re-select all")
					
					FOR ll_rc = 1 to ll_row
						dw_transaction_list.setitem(ll_rc,'check_box',0)				
						dw_transaction_list.setitem(ll_rc,'amt_entered',0.00)
						dw_transaction_list.setitem(ll_rc,'txn_tax_amt_entered',0.00)
						dw_transaction_list.setitem(ll_rc,'days_entered',0)
						dw_transaction_list.setitem(ll_rc,'hours_entered',0.00)
						dw_transaction_list.setitem(ll_rc,'rehab_qty_entered',0)
						dw_transaction_list.object.days_entered.protect = 0
						dw_transaction_list.object.hours_entered.protect = 0
					next
					cbx_1.checked = false
					dw_transaction_list.setredraw(true)
					RETURN -1
				end if
			end if
		end if

		if ls_payment_type_code  = '22' and (ls_payment_sub_type_code = 'BC' or ls_payment_sub_type_code = 'RC' ) then
			
			ll_prescription_set_no =  dw_transaction_list.getitemnumber(ll_row,'prescription_set_no')
			
			if ll_prescription_set_no > 0 then
			
				select count(*)
				INTO :ll_count
				from 	PAYMENT_PRESCRIPTION  a , 
						UNAPPLIED_CLAIM_TXN   b
				WHERE a.payment_no = b.payment_no and 
						a.prescription_set_no = :ll_prescription_set_no
				using SQLCA;	

				SQLCA.nf_handle_error("select count(*) from PAYMENT_PRESCRIPTION",is_window_name,ls_script)

				IF ll_count > 0 then
					messagebox("Warning","Cannot maintain an imported drug payment if there are UNAPPLIED_CLAIM_TXN for the same prescription. pr set no " + string(ll_prescription_set_no))
					dw_transaction_list.setitem(ll_row,'check_box',0)
					lb_fail = true
					continue
				end if


			END IF
		end if
	
	
		wf_calculate_remaining_days_lost(ll_ultimate_txn_no,ldcm_remaining_days_lost)
		wf_calculate_remaining_hours_lost(ll_ultimate_txn_no,ldcm_remaining_hours_lost)
		wf_calculate_remaining_ult_txn_amount(ll_ultimate_txn_no,ldcm_remaining_ult_txn_amount)
		
		IF ls_split_payment_flag = 'Y' THEN
			wf_calc_net_other_ult_txn_amount(ll_payment_no,ll_ultimate_txn_no,ldcm_remaining_other_ult_txn_amount)
			IF ldcm_remaining_other_ult_txn_amount = 0 THEN
				// split payment will be fully adjusted wrt $, so remainder of days & hours should also be fully adjusted
				lb_apply_remainder_days_hours = TRUE
				// how much have other txns been adj for days, hours
				wf_calc_net_other_adj_ult_days_lost(ll_payment_no,ll_ultimate_txn_no,ldcm_other_adj_ult_days)
				wf_calc_net_other_adj_ult_hours_lost(ll_payment_no,ll_ultimate_txn_no,ldcm_other_adj_ult_hours)
				ldcm_net_days_lost = dw_transaction_list.getitemdecimal(ll_row,'net_days_lost') + ldcm_other_adj_ult_days
				ldcm_net_hours_lost = dw_transaction_list.getitemdecimal(ll_row,'net_hours_lost') + ldcm_other_adj_ult_hours
			ELSE
				ldcm_portion_to_apply = ldcm_entered_amount  / ldcm_remaining_ult_txn_amount
			END IF
		ELSE
			IF ldcm_remaining_ult_txn_amount = 0 THEN
				ldcm_portion_to_apply = 1
			ELSEIF (ldcm_entered_amount + ldcm_txn_amount) = 0 THEN
				lb_apply_remainder_days_hours = TRUE
				ldcm_net_days_lost = dw_transaction_list.getitemdecimal(ll_row,'net_days_lost')
				ldcm_net_hours_lost = dw_transaction_list.getitemdecimal(ll_row,'net_hours_lost')
			ELSE
				ldcm_portion_to_apply = ldcm_entered_amount  / ldcm_remaining_ult_txn_amount
			END IF
		END IF
			
			
	 	/* setting the full adjustment or transfer amount */			
	 	dw_transaction_list.setitem(ll_row,'amt_entered',ldcm_entered_amount)
	 	dw_transaction_list.setitem(ll_row,'txn_tax_amt_entered',ldcm_tax_amount * -1)
		dw_transaction_list.setitem(ll_row,'check_box',1)

		
		if ls_txn_type_code = "J" AND ls_txn_sub_type_code = "4"  then			
			 dw_transaction_list.setitem(ll_row,'amt_entered',0.00)				 
		elseif ls_txn_type_code = "J" AND ls_txn_sub_type_code = "Q"  then
			 dw_transaction_list.setitem(ll_row,'txn_tax_amt_entered',0.00)	
		else
			dw_transaction_list.setitem(ll_row,'txn_tax_amt_entered',ldcm_tax_amount * -1)
			
			dw_transaction_list.setitem(ll_row,'rehab_qty_entered',ll_net_quantity * -1)
			/* DEFAULT THE DAYS AND HOURS LOST */
			IF lb_apply_remainder_days_hours = TRUE THEN
				dw_transaction_list.setitem(ll_row,'days_entered',ldcm_net_days_lost*-1)
				dw_transaction_list.setitem(ll_row,'hours_entered',ldcm_net_hours_lost*-1)
			ELSE
				IF ls_benefit_type = 'L' THEN
					dw_transaction_list.setitem(ll_row,'days_entered',0.00)
					dw_transaction_list.setitem(ll_row,'hours_entered',0.00)
				ELSE
					dw_transaction_list.setitem(ll_row,'days_entered',ldcm_portion_to_apply * ldcm_remaining_days_lost)
					dw_transaction_list.setitem(ll_row,'hours_entered',ldcm_portion_to_apply * ldcm_remaining_hours_lost)
				END IF
			END IF
		end if
		ll_checked++
		
		IF dw_transaction_list.GetItemString(ll_row,'target_flag') = 'Y' THEN
			ldcm_total_amount = ldcm_total_amount + ldcm_txn_amount
		END IF
		
	NEXT
	
	IF ls_txn_type_code = "J" AND  ls_txn_sub_type_code = "2" then
	
		dw_parameter.setitem(1,'amount',ldcm_total_amount * -1)
	end if
	
else
	
	 FOR ll_row = 1 to dw_transaction_list.rowcount()
		dw_transaction_list.setitem(ll_row,'check_box',0)				
	   dw_transaction_list.setitem(ll_row,'amt_entered',0.00)
	   dw_transaction_list.setitem(ll_row,'txn_tax_amt_entered',0.00)
	   dw_transaction_list.setitem(ll_row,'days_entered',0)
	   dw_transaction_list.setitem(ll_row,'hours_entered',0.00)
	   dw_transaction_list.setitem(ll_row,'rehab_qty_entered',0)
		dw_transaction_list.object.days_entered.protect = 0
		dw_transaction_list.object.hours_entered.protect = 0
	next
	
end if

if ll_checked = 0 then
	THIS.checked = false
end if

dw_transaction_list.setredraw(true)

SetPointer(oldpointer)

end event

type cb_clear from commandbutton within w_claim_cost_maintenance
integer x = 2341
integer y = 512
integer width = 302
integer height = 104
integer taborder = 210
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Clea&r"
end type

event clicked;dw_parameter.reset()
dw_parameter.insertrow(0)

wf_reset_parm_screen()
cbx_1.checked = false

dw_transaction_list.reset()

end event

type gb_filter from groupbox within w_claim_cost_maintenance
integer x = 2560
integer y = 32
integer width = 731
integer height = 352
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Payment Type Filter"
end type

type em_amount from editmask within w_claim_cost_maintenance
integer x = 3927
integer y = 220
integer width = 402
integer height = 104
integer taborder = 40
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
borderstyle borderstyle = stylelowered!
string mask = "###,###.00"
end type

type em_pct from editmask within w_claim_cost_maintenance
integer x = 3927
integer y = 224
integer width = 402
integer height = 96
integer taborder = 80
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
borderstyle borderstyle = stylelowered!
boolean spin = true
double increment = 1
string minmax = "1~~100"
end type

type cb_apply from commandbutton within w_claim_cost_maintenance
integer x = 3730
integer y = 480
integer width = 302
integer height = 104
integer taborder = 230
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Apply"
end type

event clicked;boolean  lb_continue 

decimal {2} ldcm_amt
decimal {2} ldcm_txn_amount 
decimal {2}	ldcm_tax_amount
decimal {2}	ldcm_pct
decimal {2} ldcm_entered_amount
decimal {2} ldcm_total_amt
decimal {2} ldcm_running_total
decimal {2} ldcm_remaining_days_lost
decimal {2} ldcm_remaining_hours_lost
decimal {2} ldcm_other_adj_ult_days
decimal {2} ldcm_other_adj_ult_hours

integer 	li_rc

long 				ll_txn_no
long 				ll_payment_no
long				ll_net_quantity
long				ll_row
long				ll_check_box
long				ll_no_checked
long				ll_cntr
string				ls_benefit_type 
string				ls_txn_type_code
string				ls_txn_sub_type_code	
decimal {4}		ldcm_portion_to_apply 
decimal {2}		ldcm_remaining_ult_txn_amount
decimal {2}		ldcm_remaining_other_ult_txn_amount
decimal {2}		ldcm_net_days_lost
decimal {2}		ldcm_net_hours_lost

long				ll_ultimate_txn_no
string				ls_split_payment_flag
boolean			lb_apply_remainder_days_hours


SETPOINTER(HOURGLASS!)

ls_benefit_type 						= dw_parameter.getitemstring(1,'benefit_type')
ls_txn_type_code 						= dw_parameter.getitemstring(1,'txn_type_code')
ls_txn_sub_type_code				= dw_parameter.getitemstring(1,'txn_sub_type_code')


ll_no_checked = 0

FOR ll_row = 1 to dw_transaction_list.rowcount()
	ll_check_box	=	dw_transaction_list.getitemnumber(ll_row,'check_box')
	if ll_check_box = 1 then ll_no_checked++
NEXT

if ll_no_checked = 0 then
	MESSAGEBOX("Warning","There are no transactions selected to apply an amount against")
	return -1
end if


/* PCT IS CHECKED */
if rb_pct.checked = true then
	
	/* EDIT CHECKS OF PCT ENTERED */
	if isnull(em_pct.text) or dec(em_pct.text) = 0.00 then
		messagebox("Warning","Pct must be entered")
		return
	end if
	IF dec(em_pct.text) <= 0.00 then
		messagebox("Warning","Pct must be entered as a positive")
		return
	end if
	IF dec(em_pct.text) > 100 then
		messagebox("Warning","Pct must be LESS or equal to 100%")
		return
	end if
	ldcm_pct = dec(em_pct.text)
	li_rc = MESSAGEBOX("warning","About to Apply a pct of " + em_pct.text + " % to all selected txn - continue",Question!,YesNo!,2 )	


	
	
	
elseif  rb_amount.checked = true then	
	
		ldcm_amt = dec(em_amount.text)
		/* edit checks on the amount entered */
		if isnull(em_amount.text) or dec(em_amount.text) = 0.00 then
			messagebox("Warning","Amount must be entered")
			return
		end if
		li_rc = MESSAGEBOX("warning","About to Apply an amount of " + em_amount.text + " to all selected txn - continue",Question!,YesNo!,2 )	

/* a total amount is to be applied to selected txn based on "oldest to newest" or "newest to oldest" */
elseif rb_total_amt.checked = true then
	
	/* edit checks on the total amount entered */
	ldcm_amt = dec(em_amount.text)
	if ldcm_amt = 0 then
		messagebox("Warning","Amount must be entered")
		return
	end if
	if ldcm_amt > 0 then
		messagebox("Warning","Amount must be entered as a negative value")
		return
	end if
	
	if cbx_oldest_to_newest.checked = true then
		li_rc = MESSAGEBOX("warning","About to Apply and total amount of " + em_amount.text + " to all selected txn - oldest to newest. Continue",Question!,YesNo!,2 )	
	else
		li_rc = MESSAGEBOX("warning","About to Apply and total amount of " + em_amount.text + " to all selected txn - newest to oldest. Continue",Question!,YesNo!,2 )		
	end if
else
	messagebox("Warning","Please select 'option type' to apply")
	return
end if

if li_rc = 2 then
	RETURN
end if	
		

lb_continue = true

FOR ll_row = 1 to dw_transaction_list.rowcount()
	
	ll_cntr = dw_transaction_list.rowcount() - (ll_row - 1 )
		
	if cbx_oldest_to_newest.checked = false then
		ib_oldest_to_newest = FALSE
		ll_cntr = ll_row
	ELSE
		ib_oldest_to_newest = TRUE
	end if
	
	ll_check_box					=	dw_transaction_list.getitemnumber(ll_cntr,'check_box')

	if ll_check_box = 1 then			
		ll_no_checked++
		
		ll_txn_no							=  dw_transaction_list.getitemdecimal(ll_cntr,'txn_no')
		ll_payment_no					=  dw_transaction_list.getitemdecimal(ll_cntr,'payment_no')
		ldcm_txn_amount 				=	dw_transaction_list.getitemdecimal(ll_cntr,'txn_amount')
		ldcm_tax_amount 				= 	dw_transaction_list.getitemdecimal(ll_cntr,'txn_tax_amount')
		ll_net_quantity					= 	dw_transaction_list.getitemnumber(ll_cntr,'net_quantity')
		ll_ultimate_txn_no				=	dw_transaction_list.getitemnumber(ll_cntr,'ultimate_txn_no')
		ls_split_payment_flag			=	dw_transaction_list.GetItemString(ll_cntr,'split_payment_flag')
	
		// reset variables
		lb_apply_remainder_days_hours = FALSE
		ldcm_remaining_days_lost = 0
		ldcm_remaining_hours_lost = 0
		ldcm_remaining_ult_txn_amount = 0
		ldcm_remaining_other_ult_txn_amount = 0
		ldcm_other_adj_ult_days = 0
		ldcm_other_adj_ult_hours = 0
		
		IF  rb_pct.checked = true THEN
			ldcm_entered_amount = ((ldcm_txn_amount * ldcm_pct)/100) * -1
	
		elseif rb_amount.checked = true then
			ldcm_entered_amount = ldcm_amt
			
		else
			if not lb_continue  then
				dw_transaction_list.setitem(ll_cntr,'amt_entered',0.00)
				continue
				
			else		
				ldcm_entered_amount = ldcm_txn_amount * -1
				ldcm_running_total = ldcm_running_total - (ldcm_txn_amount * -1)
				if ldcm_running_total > abs(ldcm_amt) then				
					ldcm_entered_amount = (ldcm_running_total + ldcm_amt) + (ldcm_txn_amount * -1)
			
					lb_continue = false
				end if
			end if
		end if		
		
		dw_transaction_list.setitem(ll_cntr,'amt_entered',ldcm_entered_amount)
		ldcm_total_amt = ldcm_total_amt +  ldcm_entered_amount
			

		
		wf_calculate_remaining_days_lost(ll_ultimate_txn_no,ldcm_remaining_days_lost)
		wf_calculate_remaining_hours_lost(ll_ultimate_txn_no,ldcm_remaining_hours_lost)
		wf_calculate_remaining_ult_txn_amount(ll_ultimate_txn_no,ldcm_remaining_ult_txn_amount)
		
		IF ls_split_payment_flag = 'Y' THEN
			wf_calc_net_other_ult_txn_amount(ll_payment_no,ll_ultimate_txn_no,ldcm_remaining_other_ult_txn_amount)
			IF ldcm_remaining_other_ult_txn_amount = 0 AND (ldcm_entered_amount + ldcm_txn_amount) = 0 THEN
				// split payment will be fully adjusted wrt $, so remainder of days & hours should also be fully adjusted
				lb_apply_remainder_days_hours = TRUE
				// how much have other txns been adj for days, hours
				wf_calc_net_other_adj_ult_days_lost(ll_payment_no,ll_ultimate_txn_no,ldcm_other_adj_ult_days)
				wf_calc_net_other_adj_ult_hours_lost(ll_payment_no,ll_ultimate_txn_no,ldcm_other_adj_ult_hours)
				ldcm_net_days_lost = dw_transaction_list.getitemdecimal(ll_cntr,'net_days_lost') + ldcm_other_adj_ult_days
				ldcm_net_hours_lost = dw_transaction_list.getitemdecimal(ll_cntr,'net_hours_lost') + ldcm_other_adj_ult_hours
			ELSE
				ldcm_portion_to_apply = ldcm_entered_amount  / ldcm_remaining_ult_txn_amount
			END IF
		ELSE
			IF ldcm_remaining_ult_txn_amount = 0 THEN
				ldcm_portion_to_apply = 1
			ELSEIF (ldcm_entered_amount + ldcm_txn_amount) = 0 THEN
				lb_apply_remainder_days_hours = TRUE
				ldcm_net_days_lost = dw_transaction_list.getitemdecimal(ll_cntr,'net_days_lost')
				ldcm_net_hours_lost = dw_transaction_list.getitemdecimal(ll_cntr,'net_hours_lost')
			ELSE
				lb_apply_remainder_days_hours = FALSE
				ldcm_portion_to_apply = ldcm_entered_amount  / ldcm_remaining_ult_txn_amount
			END IF
		END IF
		
		/* DEFAULT THE DAYS AND HOURS LOST */
		IF lb_apply_remainder_days_hours = TRUE THEN
			dw_transaction_list.setitem(ll_cntr,'days_entered',ldcm_net_days_lost*-1)
			dw_transaction_list.setitem(ll_cntr,'hours_entered',ldcm_net_hours_lost*-1)
		ELSE
			if ls_benefit_type = 'L' THEN
				dw_transaction_list.setitem(ll_cntr,'days_entered',0.00)
				dw_transaction_list.setitem(ll_cntr,'hours_entered',0.00)
			else
				dw_transaction_list.setitem(ll_cntr,'days_entered',ldcm_portion_to_apply * ldcm_remaining_days_lost)
				dw_transaction_list.setitem(ll_cntr,'hours_entered',ldcm_portion_to_apply * ldcm_remaining_hours_lost)
			end if
		END IF
				
	end if
NEXT


if rb_total_amt.checked = true then
	dw_parameter.setitem(1,'amount',ldcm_amt)
end if


if not lb_continue then
	MESSAGEBOX("Warning","The amount entered of " + String(ldcm_amt,'$#,##0.00') + " to apply against has not covered all the selected txn")
end if

if rb_total_amt.checked and (ldcm_total_amt <> ldcm_amt) then
	MESSAGEBOX("Warning","The Sum of all the txn's for the selected txn is less then the entered amount of " + String(ldcm_amt,'$#,##0.00') + "")	
end if

FOR ll_row = 1 to dw_transaction_list.rowcount()
	
	ll_check_box					=	dw_transaction_list.getitemnumber(ll_row,'check_box')

	if ll_check_box = 1 then	
		cbx_1.checked = false
		ldcm_entered_amount				=	dw_transaction_list.getitemdecimal(ll_row,'amt_entered')
		IF ldcm_entered_amount = 0 THEN
			dw_transaction_list.SETITEM(ll_row,'check_box',0)
			dw_transaction_list.setitem(ll_row,'txn_tax_amt_entered',0.00)	
			dw_transaction_list.setitem(ll_row,'days_entered',0.00)
			dw_transaction_list.setitem(ll_row,'hours_entered',0.00)
			dw_transaction_list.setitem(ll_row,'rehab_qty_entered',0)	
		END IF
	
	END IF
NEXT


	
cbx_1.enabled = false
cb_cancel_pct.visible = true	
this.enabled = false

end event

type cb_cancel_pct from commandbutton within w_claim_cost_maintenance
integer x = 4059
integer y = 480
integer width = 302
integer height = 104
integer taborder = 240
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cancel"
end type

event clicked;dw_transaction_list.setredraw(false)
cbx_1.enabled = true
cbx_1.checked = false
//
//dw_transaction_list.SetFilter("")
//dw_transaction_list.filter()
//
dw_transaction_list.selectrow(0,false)
dw_transaction_list.setredraw(true)
cb_apply.enabled = true
end event

type cbx_oldest_to_newest from checkbox within w_claim_cost_maintenance
integer x = 3803
integer y = 96
integer width = 526
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Oldest to Newest"
boolean checked = true
end type

type gb_pct from groupbox within w_claim_cost_maintenance
integer x = 3355
integer y = 36
integer width = 1061
integer height = 576
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Apply Value"
end type

type dw_confirm_summary_totals from u_dw_online within w_claim_cost_maintenance
boolean visible = false
integer y = 1280
integer width = 4571
integer height = 888
integer taborder = 100
string title = "none"
string dataobject = "d_confirm_summary_totals"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = false
borderstyle borderstyle = styleraised!
end type

event constructor;call super::constructor;THIS.object.datawindow.hidegrayline = true
end event

event ue_print;THIS.Modify("DataWindow.Print.Orientation= '1' ") // landscape
THIS.print()
end event

type dw_parameter from u_dw_online within w_claim_cost_maintenance
integer width = 4571
integer height = 640
integer taborder = 10
string dataobject = "d_parameter"
borderstyle borderstyle = styleraised!
end type

event itemchanged;call super::itemchanged;DATAWINDOWCHILD 	ldwc_txn_type_combination, ldwc_provider_type

string	ls_find
string 	ls_filter
string 	ls_payment_type_code_fr
string 	ls_payment_type_code_to
STRING 	ls_script = "itemchanged"
STRING 	ls_last_name, ls_first_name,ls_concat_name
string	ls_txn_sub_type_code
string	ls_txn_type_code
string	ls_explanation
string	ls_payment_sub_type_code_fr
string	ls_payment_sub_type_code_to
long 		ll_rc
long		ll_row
long		ll_rows
long		ll_found
long		ll_claim_no
long 		ll_default_cost_alloc_no
long 		ll_default_cost_alloc_operation_no
long 		ll_count
long		ll_cost_alloc_no_fr
long		ll_cost_alloc_no_to
long		ll_individual_no
long		ll_cheque_no
long		ll_xmit_no
long		ll_claim_no_fr
long		ll_cost_alloc_operation_no_fr
long		ll_cost_alloc_operation_no_to
			
date 		ldt_cost_relief_date
DATETIME ldt_accident_date 

datetime ld_null

setnull(ld_null)

choose case this.GetColumnName()
	
	/* filter the sub type dropdown based on the txn type */
	case 'txn_type_code'
		
		wf_set_parameter_screen(dw_parameter.getitemstring(1,'txn_type_code'),DATA)
		dw_parameter.setitem(1,'txn_sub_type_code','')

		/* populate the valid txn sub types */
		ll_rc = dw_parameter.GetChild("txn_sub_type_code",ldwc_txn_type_combination)
		ldwc_txn_type_combination.SetTransObject(SQLCA)
	
		ll_rows = ldwc_txn_type_combination.Retrieve(dw_parameter.getitemstring(1,'txn_type_code'))
		
	
		/* REMOVE THE TXN SUB TYPE IF USER GROUP DOES NOT ALLOW FOR IT */
		for ll_row = 1 to ldwc_txn_type_combination.rowcount()
			
			ls_txn_type_code 		= ldwc_txn_type_combination.getitemstring(ll_row,'txn_type_code')
			ls_txn_sub_type_code = ldwc_txn_type_combination.getitemstring(ll_row,'txn_sub_type_code')
		
			ls_find = "txn_type_code = '"  + ls_txn_type_code + "' and txn_sub_type_code = '" + ls_txn_sub_type_code + "'"
			ll_found = ids_work_group_txn_sub_type.Find(ls_find,1,ids_work_group_txn_sub_type.rowcount())
					
			if ll_found <= 0 then
				ldwc_txn_type_combination.deleterow(ll_row)
				ll_row = ll_row - 1
			end if
		
		next
	
		rb_pay_all_types.checked = true
		dw_transaction_list.reset()

		/* disbale or enable to pct/amt/total amt functionality */		
		wf_set_amt_indicator(false,DATA,DATA)
			

	case 'txn_sub_type_code'
		
			wf_set_parameter_screen(dw_parameter.getitemstring(1,'txn_type_code'),DATA)
		
			ls_txn_sub_type_code = data
			ls_txn_type_code = dw_parameter.getitemstring(1,'txn_type_code')
			
			if ls_txn_type_code = "J" and ls_txn_sub_type_code = "2" then
				/* populate the provider types */
				ll_rc = dw_parameter.GetChild("provider_type_code",ldwc_provider_type)
				ldwc_provider_type.SetTransObject(SQLCA)
	
				ll_rows = ldwc_provider_type.Retrieve()
						
				dw_parameter.SETCOLUMN("cheque_no")

			end if
			
			
			dw_transaction_list.reset()
			rb_pay_all_types.checked = true
			/* disbale or enable to pct/amt/total amt functionality */		
			wf_set_amt_indicator(true,ls_txn_type_code,ls_txn_sub_type_code)

case 'claim_no'
	
		
			w_claim_cost_maintenance.title = "Claim Cost Maintenance"			
					
			ll_claim_no = long(DATA)			
	
			if ll_claim_no = 0 then
			else
					
				ls_txn_type_code 		= dw_parameter.getitemstring(1,'txn_type_code')
				ls_txn_sub_type_code = dw_parameter.getitemstring(1,'txn_sub_type_code')
			
			
				if ls_txn_type_code = 'T' AND (ls_txn_sub_type_code = '7' OR ls_txn_sub_type_code = '8') THEN
				
				SELECT dbo.CLAIM.cost_alloc_no,   
						 dbo.CLAIM.cost_alloc_operation_no  
				 INTO :ll_default_cost_alloc_no,   
						:ll_default_cost_alloc_operation_no  
				FROM  dbo.CLAIM  
				WHERE dbo.CLAIM.claim_no = :ll_claim_no   ;
				SQLCA.nf_handle_error("select from CLAIM",is_window_name,ls_script)
				
				
				IF  ls_txn_sub_type_code = '7' then
					dw_parameter.setitem(1,'cost_alloc_no_to',ll_default_cost_alloc_no)
					dw_parameter.setitem(1,'cost_alloc_operation_no_to',ll_default_cost_alloc_operation_no)
				ELSE
					dw_parameter.setitem(1,'cost_alloc_no_fr',ll_default_cost_alloc_no)
					dw_parameter.setitem(1,'cost_alloc_operation_no_fr',ll_default_cost_alloc_operation_no)
				END IF
				
				
			END IF
			

			SELECT COUNT(*)
			INTO :ll_count   
			FROM dbo.CLAIM  
			WHERE dbo.CLAIM.claim_no = :ll_claim_no   
			using sqlca;
			SQLCA.nf_handle_error("select from CLAIM",is_window_name,ls_script)

			if ll_count = 0  then
				messagebox("Warning","In-valid claim no entered")
				return -1
			end if

			SELECT individual_no
			INTO :ll_individual_no
			FROM dbo.CLAIM  
			WHERE dbo.CLAIM.claim_no = :ll_claim_no   
			using sqlca;
			if SQLCA.nf_handle_error("select from CLAIM",is_window_name,ls_script) = 100 THEN
				messagebox("Warning","In-valid claim no entered")
				return -1
			end if


			SELECT  	dbo.INDIVIDUAL.last_name,   
						dbo.INDIVIDUAL.given_names  
			INTO :ls_last_name,   
					:ls_first_name
			FROM dbo.INDIVIDUAL  
			WHERE dbo.INDIVIDUAL.individual_no = :ll_individual_no
			using sqlca;
			if SQLCA.nf_handle_error("select from CLAIM",is_window_name,ls_script) = 100 then
				messagebox("Warning","In-valid individual")
				return -1
			end if
				
			ls_concat_name = trim(ls_last_name) + ', ' + trim(ls_first_name)
			w_claim_cost_maintenance.title = 			w_claim_cost_maintenance.title + ' - ' + ls_concat_name 
		
		end if		
		dw_parameter.setitem(1,'explanation',"")
		cbx_1.checked = false
		
	case 'claim_no_to'
		
	
			ll_claim_no_fr = dw_parameter.getitemnumber(1,'claim_no')
			
			ll_claim_no = long(DATA)
			
			SELECT individual_no
			INTO :ll_individual_no
			FROM dbo.CLAIM  
			WHERE dbo.CLAIM.claim_no = :ll_claim_no   
			using sqlca;
			if SQLCA.nf_handle_error("select from CLAIM",is_window_name,ls_script) = 100 THEN
				messagebox("Warning","In-valid claim no 'to' entered")
				return -1
			end if

		 SELECT  dbo.INDIVIDUAL.last_name,   
					dbo.INDIVIDUAL.given_names  
		 INTO :ls_last_name,   
				:ls_first_name
		 FROM dbo.INDIVIDUAL  
		 WHERE dbo.INDIVIDUAL.individual_no = :ll_individual_no
		 using sqlca;
		 if SQLCA.nf_handle_error("select from CLAIM",is_window_name,ls_script) = 100 then
			messagebox("Warning","In-valid indivdual")
			return -1
		end if
			
			ls_concat_name = trim(ls_last_name) + ', ' + trim(ls_first_name)
	
			dw_parameter.setitem(1,'individual_name',ls_concat_name)
			dw_parameter.setitem(1,'explanation',"")
			
	case 'cost_alloc_no_fr'
			
			
			ll_cost_alloc_no_fr = long(DATA)
				
			SELECT count(*)
		  	INTO :ll_count  
		  	FROM dbo.EMPLOYER  
		  	WHERE dbo.EMPLOYER.employer_no = :ll_cost_alloc_no_fr
		  	using SQLCA;
		  	SQLCA.nf_handle_error("select from EMPLOYER",is_window_name,ls_script)
	
			if ll_count = 0  then
				messagebox("Warning","In-valid 'fr' cost allocation no entered")
				return -1
			end if
			dw_parameter.setitem(1,'explanation',"")
		
	case 'cost_alloc_operation_no_to'
		
		ll_cost_alloc_no_to = dw_parameter.getitemnumber(1,'cost_alloc_no_to')
		ll_cost_alloc_no_fr = dw_parameter.getitemnumber(1,'cost_alloc_no_fr')
		ll_cost_alloc_operation_no_fr = dw_parameter.getitemnumber(1,'cost_alloc_operation_no_fr')
		ll_cost_alloc_operation_no_to = long(DATA)
		dw_parameter.setitem(1,'explanation',"")
		
	
	case 'cost_alloc_no_to'
			
		
			ll_cost_alloc_no_to = long(DATA)
				
			SELECT count(*)
		  	INTO :ll_count  
		  	FROM dbo.EMPLOYER  
		  	WHERE dbo.EMPLOYER.employer_no = :ll_cost_alloc_no_to
		  	using SQLCA;
		  	SQLCA.nf_handle_error("select from EMPLOYER",is_window_name,ls_script)
	
			if ll_count = 0  then
				messagebox("Warning","In-valid 'to' cost allocation no entered")
				return -1
			end if
			dw_parameter.setitem(1,'explanation',"")
		
	
		case 'cost_relief_date'
			
			if dw_transaction_list.rowcount() > 0 then
				dw_transaction_list.reset()
			end if
			dw_parameter.setitem(1,'explanation',"")	
			
case 'payment_type_code_fr'
		

	
		dw_parameter.setitem(1,'payment_sub_type_code_fr','')
		ls_payment_type_code_fr = data

		/* populate the valid payment sub type's bases on payment type selected */
		ll_rows = idwc_payment_sub_type_code_fr.Retrieve(ls_payment_type_code_fr)
		SQLCA.nf_handle_error("ldwc_payment_type_code.Retrieve()",is_window_name,ls_script)
		dw_parameter.setitem(1,'explanation',"")
		
		
case 'payment_type_code_to'
	
		dw_parameter.setitem(1,'payment_sub_type_code_to','')
		ls_payment_type_code_to = data
		
		/* populate the valid payment sub type's bases on payment type selected */
		ll_rows = idwc_payment_sub_type_code_to.Retrieve(ls_payment_type_code_to)
		SQLCA.nf_handle_error("idwc_payment_sub_type_code_to.Retrieve()",is_window_name,ls_script)
		dw_parameter.setitem(1,'explanation',"")
	

case 'payment_sub_type_code_fr'
	dw_parameter.setitem(1,'explanation',"")
		
case 'payment_sub_type_code_to'
	dw_parameter.setitem(1,'explanation',"")


case 'cheque_no'
		ll_cheque_no = long(data)
		
		IF ll_cheque_no = 0 then
			messagebox("Warning","In-valid cheque no entered")
		end if
			
		if ll_cheque_no > 1 then
		
			dw_parameter.SETCOLUMN("issue_date")	
			dw_parameter.setitem(1,'explanation',"")
			cbx_1.checked = false
		end if
		
case 'xmit_no'
		ll_xmit_no = long(data)

				
		if ll_xmit_no > 0 then
			dw_parameter.SETCOLUMN("issue_date")
			dw_parameter.setitem(1,'explanation',"")
			cbx_1.checked = false
		end if
		
		
		
		
case 'unprocessed'
	
	if data = "0" then
		cb_unit_of_work_save.visible = false
		gb_filter.visible = true
		rb_pay_all_types.visible = true
		rb_pay_act.visible = true
		rb_pay_loe.visible = true
		cb_cancel.visible = true
		cb_confirm.visible = true
		cb_search.visible = true
		dw_transaction_list.RESET()
		dw_transaction_list.visible = true
		uo_filter.visible = true 
		dw_parameter.Object.payment_sub_type_code_fr.Protect=0
		dw_parameter.Object.payment_sub_type_code_to.Protect=0
		dw_parameter.Object.payment_type_code_fr.Protect=0
		dw_parameter.Object.payment_type_code_to.Protect=0
		dw_parameter.Object.txn_type_code.Protect=0
		dw_parameter.Object.txn_sub_type_code.Protect=0
		dw_parameter.Object.claim_no.Protect=0
		dw_parameter.Object.claim_no_to.Protect=0
		dw_parameter.Object.provider_no.Protect=0
		dw_parameter.Object.provider_type_code.Protect=0
		dw_parameter.Object.posting_period.Protect=0
		dw_parameter.Object.amount.Protect=0
		dw_parameter.Object.explanation.Protect=0
		dw_parameter.Object.cheque_no.Protect=0
		dw_parameter.Object.xmit_no.Protect=0
		dw_parameter.Object.cost_alloc_no_fr.Protect=0
		dw_parameter.Object.cost_alloc_operation_no_fr.Protect=0
		dw_parameter.Object.cost_alloc_no_to.Protect=0
		dw_parameter.Object.cost_alloc_operation_no_to.Protect=0
		dw_parameter.Object.issue_date.Protect=0
		dw_parameter.Object.add_txn_unit_of_work.Protect=0	
		cbx_1.visible = true
		cbx_1.checked = false
		st_sel.visible = true
		dw_claimcost_unapplied_claim_txn.visible = false
		cb_delete.visible = false
		cb_close.visible = true
		cb_close_unprocessed.visible = false
		cb_refresh.VISIBLE = FALSE
		cb_clear.visible = true
		cb_find.visible = true
		
		ls_txn_sub_type_code = dw_parameter.getitemstring(1,'txn_sub_type_code')
		ls_txn_type_code = dw_parameter.getitemstring(1,'txn_type_code')
	
		/* disbale or enable to pct/amt/total amt functionality */		
		wf_set_amt_indicator(true,ls_txn_type_code,ls_txn_sub_type_code)
			
	
	else
	
		ll_rows = dw_claimcost_unapplied_claim_txn.retrieve(vgst_user_profile.work_group_code)
		
		IF ll_rows = 0 then
			messagebox("Warning","No rows in UN-PROCESSED")
			this.setitem(1,'unprocessed',0)
			return 1
		end if
		gb_filter.visible = false
		rb_pay_all_types.visible = false
		rb_pay_act.visible = false
		rb_pay_loe.visible = false
		cb_unit_of_work_save.visible = true
		cb_unit_of_work_save.ENABLED = TRUE
		cb_refresh.visible = false
		dw_claimcost_unapplied_claim_txn.visible = true
		il_txn_unit_of_work = 0
		cb_cancel.visible = false
		cb_confirm.visible = false
		cb_search.visible = false
		cbx_1.visible = false
		cbx_1.checked = false
		st_sel.visible = false
		dw_transaction_list.visible = false
		uo_filter.visible = false
		dw_parameter.Object.txn_type_code.Protect=1
		dw_parameter.Object.txn_sub_type_code.Protect=1
		dw_parameter.Object.claim_no.Protect=1
		dw_parameter.Object.claim_no_to.Protect=1
		dw_parameter.Object.provider_no.Protect=1
		dw_parameter.Object.provider_type_code.Protect=1
		dw_parameter.Object.posting_period.Protect=1
		dw_parameter.Object.amount.Protect=1
		dw_parameter.Object.explanation.Protect=1
		dw_parameter.Object.cheque_no.Protect=1
		dw_parameter.Object.xmit_no.Protect=1
		dw_parameter.Object.cost_alloc_no_fr.Protect=1
		dw_parameter.Object.cost_alloc_operation_no_fr.Protect=1
		dw_parameter.Object.cost_alloc_no_to.Protect=1
		dw_parameter.Object.cost_alloc_operation_no_to.Protect=1
		dw_parameter.Object.payment_sub_type_code_fr.Protect=1
		dw_parameter.Object.payment_sub_type_code_to.Protect=1
		dw_parameter.Object.issue_date.Protect=1
		dw_parameter.Object.payment_type_code_fr.Protect=1
		dw_parameter.Object.payment_type_code_to.Protect=1
		dw_parameter.Object.add_txn_unit_of_work.Protect=1		
		cb_delete.visible = true
		cb_close.visible = false
		cb_close_unprocessed.visible = true
		cb_refresh.visible = TRUE
		cb_clear.visible = false
		cb_find.visible = false
		/* disbale or enable to pct/amt/total amt functionality */		
		wf_set_amt_indicator(false,DATA,DATA)
		
	end if
	
		
			
end choose



end event

event ue_itemchangeaccepted;call super::ue_itemchangeaccepted;long ll_cheque_no, ll_direct_deposit_xmit_no
datastore ids_cheque_issue_date 
long ll_rc
datetime ldtm_cheque_issue_date
datetime ld_null
u_ds lds_direct_deposit_issue_date

setnull(ld_null)


S_WINDOW_MESSAGE	lstr_window_message



choose case as_column_name
	case 'xmit_no'
		this.setitem(al_row,'issue_date',ld_null)
		
		this.setcolumn('issue_date')
		
		lds_direct_deposit_issue_date = CREATE u_ds
		lds_direct_deposit_issue_date.dataobject = 'ds_direct_deposit_issue_date'
		lds_direct_deposit_issue_date.SetTransObject(SQLCA)

		ll_direct_deposit_xmit_no = this.getitemnumber(al_row,'xmit_no')
			
		if ll_direct_deposit_xmit_no <> 0 then
			
			ll_rc = lds_direct_deposit_issue_date.retrieve(ll_direct_deposit_xmit_no )
			
			if ll_rc = 1 then
				
				ldtm_cheque_issue_date = 	lds_direct_deposit_issue_date.getitemdatetime(1,'cheque_deposit_date')
				this.setitem(al_row,'issue_date',date(ldtm_cheque_issue_date))
	
		
			elseif ll_rc > 1 then
				MessageBox('Warning','More than one issue date for this direct deposit number',StopSign!)
			else
				messagebox("Warning","Cheque no not found")
				this.setcolumn('cheque_no')
				this.setfocus()
				return
			end if
			
				this.setcolumn('explanation')
		end if
		
	case 'cheque_no'
		
		this.setitem(al_row,'issue_date',ld_null)
		
		
		this.setcolumn('issue_date')

		if isvalid(ids_cheque_issue_date) then
			ids_cheque_issue_date.reset()
		else
			ids_cheque_issue_date = CREATE u_ds
			ids_cheque_issue_date.dataobject = 'ds_cheque_issue_date'
			ids_cheque_issue_date.SetTransObject(SQLCA)
			ids_cheque_issue_date.reset()
			
		end if

		ll_cheque_no = this.getitemnumber(al_row,'cheque_no')
			
		if ll_cheque_no <> 0 then
			
			ll_rc = ids_cheque_issue_date.retrieve(ll_cheque_no )
		
	
			
			if ll_rc = 1 then
				
				ldtm_cheque_issue_date = 	ids_cheque_issue_date.getitemdatetime(1,'cheque_deposit_date')
				this.setitem(al_row,'issue_date',date(ldtm_cheque_issue_date))
	
		
			elseif ll_rc > 1 then
				
				lstr_window_message.al_doubleparm[1] = ll_cheque_no
				OpenWithParm(w_popup_enter_cheque_issue_date,lstr_window_message)				
				lstr_window_message = Message.PowerObjectParm
				ldtm_cheque_issue_date =  lstr_window_message.adtm_datetimeparm[1]
				this.setitem(al_row,'issue_date',date(ldtm_cheque_issue_date))
	
			else
				messagebox("Warning","Cheque no not found")
				this.setcolumn('cheque_no')
				this.setfocus()
				return
			end if
			
				this.setcolumn('explanation')
		end if
		
		
	case 'claim_no'
		
		ll_cheque_no = this.getitemnumber(al_row,'cheque_no')
		
		if ll_cheque_no <> 0 then
			
			this.setitem(al_row,'cheque_no',0)
			this.setcolumn('xmit_no')
			
		end if
		
end choose

end event

event editchanged;call super::editchanged;
CHOOSE CASE this.GetColumnName()
	CASE 'amount','payment_no_find','explanation','add_txn_unit_of_work','unprocessed','posting_period'

	CASE ELSE
		dw_transaction_list.Reset()
END CHOOSE

end event

type uo_filter from u_filter_control within w_claim_cost_maintenance
integer x = 37
integer y = 2176
integer taborder = 120
end type

on uo_filter.destroy
call u_filter_control::destroy
end on

type dw_confirmation_summary from u_dw_online within w_claim_cost_maintenance
boolean visible = false
integer x = 1211
integer y = 1788
integer width = 425
integer height = 280
integer taborder = 70
boolean enabled = false
string title = "none"
string dataobject = "d_confirm_summary"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = false
borderstyle borderstyle = styleraised!
end type

type cb_close_confirm from commandbutton within w_claim_cost_maintenance
boolean visible = false
integer x = 4169
integer y = 2176
integer width = 402
integer height = 104
integer taborder = 160
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Close"
boolean default = true
end type

event clicked;cb_search.enabled = true
cb_find.enabled = true
cb_clear.enabled = true
cb_confirm.visible = true
cb_close.visible = true
//cb_reset.visible = true
cbx_1.enabled 	= true
cb_save.visible = false
cb_save.enabled = true
st_splitbar_1.visible		= false
dw_confirmation.visible = false
uo_filter.visible = true 
dw_transaction_list.visible = true
dw_claimcost_unapplied_claim_txn.visible = false
cb_cancel.visible = false
cb_cancel.enabled = false
dw_confirmation.reset()
cbx_1.checked = false
dw_parameter.enabled = true
cb_close_confirm.visible 	= false
cb_close_confirm.enabled 	= false
dw_confirm_summary_totals.visible = false
dw_transaction_list.bringtotop = true
inv_ccm.ib_suppress_warnings = false
inv_ccm.of_reset()

end event

type cb_close from commandbutton within w_claim_cost_maintenance
integer x = 4169
integer y = 2176
integer width = 402
integer height = 104
integer taborder = 130
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;cb_close.visible = true
cb_confirm.visible = true
//cb_reset.visible = true
close(parent)
end event

type cb_close_unprocessed from commandbutton within w_claim_cost_maintenance
boolean visible = false
integer x = 4169
integer y = 2176
integer width = 402
integer height = 104
integer taborder = 190
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;dw_parameter.setitem(1,'unprocessed',0)
cb_unit_of_work_save.enabled = false
cb_unit_of_work_save.visible = false
cb_cancel.visible = true
cb_confirm.visible = true
cb_close_confirm.visible = true
cb_search.visible = true
cb_clear.visible = true
cb_find.visible = true
dw_transaction_list.visible = true
uo_filter.visible = true 
dw_parameter.Object.payment_sub_type_code_fr.Protect=0
dw_parameter.Object.payment_sub_type_code_to.Protect=0
dw_parameter.Object.payment_type_code_fr.Protect=0
dw_parameter.Object.payment_type_code_to.Protect=0
dw_parameter.Object.txn_type_code.Protect=0
dw_parameter.Object.txn_sub_type_code.Protect=0
dw_parameter.Object.claim_no.Protect=0
dw_parameter.Object.claim_no_to.Protect=0
dw_parameter.Object.posting_period.Protect=0
dw_parameter.Object.amount.Protect=0
dw_parameter.Object.explanation.Protect=0
dw_parameter.Object.cheque_no.Protect=0
dw_parameter.Object.xmit_no.Protect=0
dw_parameter.Object.cost_alloc_no_fr.Protect=0
dw_parameter.Object.cost_alloc_operation_no_fr.Protect=0
dw_parameter.Object.cost_alloc_no_to.Protect=0
dw_parameter.Object.cost_alloc_operation_no_to.Protect=0

dw_parameter.Object.issue_date.Protect=0
dw_parameter.Object.add_txn_unit_of_work.Protect=0
cbx_1.visible = true
st_sel.visible = true
dw_claimcost_unapplied_claim_txn.visible = false
cb_delete.visible = false	

cb_close_unprocessed.visible = false
cb_refresh.visible = false
cb_close.enabled = true
cb_close.visible = true
dw_parameter.reset()
dw_parameter.insertrow(0)
gb_filter.visible = true
rb_pay_all_types.visible = true
rb_pay_act.visible = true
rb_pay_loe.visible = true
		
wf_reset_parm_screen()




end event

type cb_save from commandbutton within w_claim_cost_maintenance
boolean visible = false
integer x = 3342
integer y = 2176
integer width = 402
integer height = 104
integer taborder = 90
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
end type

event clicked;INTEGER	li_rtn
N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '005' refers to the Claim Cost Maintenance module, '044' refers to the Payment Processing module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('005','044','save',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF
/******************************************************************************************/

if  wf_save() < 0 then 
	cb_cancel.enabled = true
	cb_save.enabled = true
end if
end event

type cb_delete from commandbutton within w_claim_cost_maintenance
boolean visible = false
integer x = 3767
integer y = 2176
integer width = 379
integer height = 104
integer taborder = 170
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Delete"
end type

event clicked;INTEGER     li_rc, li_find, 	li_tier_no, li_opening_no, li_r1_rows, li_new_row, li_counter, li_r1_opening_no, li_r1_tier_no, li_r1_find, li_rtn
LONG		    ll_ins_row_rehab_task_auth, ll_row,  ll_row_coc,	ll_txn_no,	ll_related_txn_no,	ll_rc 
LONG 		   ll_payment_no,	ll_row_payment, ll_claim_no,	ll_row_coc_allocated, ll_row_adjust_txn_work
LONG		   ll_payment_authorization_no,	ll_adjustment_quantity,	ll_txn_unit_of_work_no, 	ll_found
LONG		   ll_rehab_task_authorization_task_no,	 ll_billable_xref_no
LONG		   ll_org_paid_quantity, ll_insert_row, ll_prev_txn_unit_of_work_no, ll_rows, ll_r1_claim_no, ll_r1_payment_no,li_confirm_delete ,ll_org_authorized_quantity	
STRING		ls_update_authorization, ls_find, ls_ready_to_process_flag, ls_payment_type_code, ls_amount_required_flag
STRING		ls_txn_of_work_create_user_id, ls_delete_flag, ls_txn_sub_type_code,ls_txn_type_code
STRING 	     ls_script = "CB_DELETE - CLICKED!"

	
DATETIME  ldtm_payment_processed_date

DECIMAL {2} ldcm_txn_amount, ldcm_org_paid_amount
DECIMAL {2} ldec_sum_txns , ldec_rtw_incentive_amount, ldec_new_r1_txn_sum, ldec_r1_txn_sum
	
BOOLEAN  lb_row_selected, lb_remove

decimal {2}    ldcm_amount_to_adjust,	ldcm_org_authorized_amount
long               ll_qty_to_adjust
long  			   ll_rehab_invoice_no_to_delete
long               ll_rehab_invoice_no
	 
datastore lds_payment, lds_cost_of_claims_allocated, lds_adjustment_txn_work_table, lds_rehab_task_authorization
datastore lds_unit_of_work_delete, lds_txn_unit_of_work_status, lds_rehab_task_authorization_update, lds_txn_sum_for_tier,lds_rehab_invoice_line_item

N_PROCESS_RUN_STATUS ln_process_run_status

/* BR FOR Removing Unprocessed Transactions
11.10	Unprocessed transactions created within this module may be deleted by this module.
11.20	Deleted payments and transactions must be placed in the “DELETED” tables to account for all payment and transaction numbers.
11.30	The paid amount & paid quantity for the From Claim rehab authorization must be increased by the amount of the unprocessed transaction(s) being deleted for the From Claim,  if that payment is linked to a rehab authorization.
11.35	The paid amount & paid quantity for the To Claim rehab authorization must be reduced by the amount of the unprocessed transaction(s) being deleted for the To Claim, if that payment is linked to a rehab authorization.
11.40	All unprocessed transactions for the same payment must be deleted.
11.50	Excluding adjustments for third party recovery, the sum of all job search Return to Work Incentive payment transactions for a specific claim and tier must not be more than the effective Rtw_Incentive_Tier incentive amount. (See Rationale – refer also to 5.50)
11.60 The unit of work must be removed, if all of the unprocessed transactions within the unit of work are removed.
11.70	The rehab invoice line item must be ‘un-amended’ for the From Claim if the unprocessed transaction(s) being deleted for the From Claim  is linked to the rehab invoice line item and the rehab invoice line item is amended.
11.80	The rehab invoice line item must be deleted for the To Claim if the unprocessed transaction(s) being deleted for the To Claim  is linked to a rehab invoice.
11.90	The rehab invoice must be deleted for the To Claim if the rehab invoice line item is being deleted for the To Claim (refer to rule 11.80) and there are no other payments linked to the rehab invoice.
*/

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '005' refers to the Claim Cost Maintenance module, '044' refers to the Payment Processing module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('005','044','delete',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF
/******************************************************************************************/


/* create all datastores need to save the database entries */
lds_rehab_task_authorization_update = CREATE u_ds
lds_rehab_task_authorization_update.dataobject = 'd_rehab_task_authorization_update'
lds_rehab_task_authorization_update.SetTransObject(SQLCA)

lds_payment = CREATE u_ds
lds_payment.dataobject = 'ds_payment'
lds_payment.SetTransObject(SQLCA)

lds_cost_of_claims_allocated = CREATE u_ds
lds_cost_of_claims_allocated.dataobject = 'ds_cost_of_claims_allocated'
lds_cost_of_claims_allocated.SetTransObject(SQLCA)

lds_adjustment_txn_work_table = CREATE u_ds
lds_adjustment_txn_work_table.dataobject = 'ds_adjustment_txn_work_table'
lds_adjustment_txn_work_table.SetTransObject(SQLCA)

lds_rehab_task_authorization = CREATE u_ds
lds_rehab_task_authorization.dataobject = 'ds_rehab_task_authorization'
lds_rehab_task_authorization.SetTransObject(SQLCA)

lds_unit_of_work_delete = CREATE u_ds
lds_unit_of_work_delete.dataobject = 'd_unit_of_work_delete'

lds_txn_unit_of_work_status = CREATE u_ds
lds_txn_unit_of_work_status.dataobject = 'd_txn_unit_of_work_status'
lds_txn_unit_of_work_status.SetTransObject(SQLCA)

lds_txn_sum_for_tier = CREATE u_ds
lds_txn_sum_for_tier.dataobject = 'ds_txn_sum_for_tier'
lds_txn_sum_for_tier.SetTransObject(SQLCA)


lds_rehab_invoice_line_item  = CREATE u_ds
lds_rehab_invoice_line_item.dataobject = 'ds_rehab_invoice_line_item'
lds_rehab_invoice_line_item.SetTransObject(SQLCA)


/* used to check if any rows are selected */
lb_row_selected = false

/* loop through dw to see if any have been selected */
for ll_row = 1 to dw_claimcost_unapplied_claim_txn.rowcount()
	if dw_claimcost_unapplied_claim_txn.IsSelected(ll_row) then	
		lb_row_selected = true
		exit
	end if
next

/* show warning if no rows are selected */
if lb_row_selected = false then
	Messagebox("Warning","No Rows to delete")
	return -1
end if

/* show confirmation message before proceeding */
if dw_claimcost_unapplied_claim_txn.rowcount()	> 0 then
	li_confirm_delete = MessageBox("Delete", "About to Delete selected Txn's - Continue",Question!, YesNo!, 2)
		/* if user does not want to proceed - they code with return without doing anything*/
	if ( li_confirm_delete = 2 ) then
		Return
	end if
else
	MESSAGEBOX("Warning","No Txn's to deleted")
	return -1
end if


/* retrieve all rows for a unit of work */
ll_rows = lds_txn_unit_of_work_status.retrieve()
SQLCA.nf_handle_error("lds_txn_unit_of_work_status.retrieve() ",is_window_name,ls_script)
				

/* check to see if any unit of work on set to on-hold */
/* check to see if the creator and delete user id are the same person - give warning if they are not*/
ll_prev_txn_unit_of_work_no = 0 


for ll_row = 1 to dw_claimcost_unapplied_claim_txn.rowcount()	
		
		if dw_claimcost_unapplied_claim_txn.IsSelected(ll_row) then				
			ll_txn_unit_of_work_no 	= 	dw_claimcost_unapplied_claim_txn.getitemnumber(ll_row,'txn_unit_of_work_no')						
			if ll_txn_unit_of_work_no = ll_prev_txn_unit_of_work_no then			
			else			
				/* flag the unit of work that are on-hold */
				ls_find = 'txn_unit_of_work_no = ' + string(ll_txn_unit_of_work_no)
				li_find = lds_txn_unit_of_work_status.find(	ls_find,1,lds_txn_unit_of_work_status.rowcount())
		
				if li_find > 0 then
					ls_ready_to_process_flag = lds_txn_unit_of_work_status.getitemstring(li_find,'ready_to_process_flag')
					if ls_ready_to_process_flag  = "N" THEN
						dw_claimcost_unapplied_claim_txn.selectrow(ll_row,false)
						CONTINUE
					END IF	
				end if
							
				ls_txn_of_work_create_user_id 	= 	dw_claimcost_unapplied_claim_txn.getitemstring(ll_row,'create_user_id')
				ll_prev_txn_unit_of_work_no 		=   ll_txn_unit_of_work_no 
					
				ll_insert_row = lds_unit_of_work_delete.insertrow(0)
				lds_unit_of_work_delete.setitem(ll_insert_row,'unit_of_work',ll_txn_unit_of_work_no)

				if ls_txn_of_work_create_user_id <> vgst_user_profile.user_id then	
					li_rc = MessageBox("Warning", "You are not the creator of this unit of work - continue anyway - " + string(ll_txn_unit_of_work_no ),    Question!, YesNo!, 2)
					
					IF li_rc = 2 THEN
						ls_delete_flag = "N"
					ELSE
			 			ls_delete_flag = "Y"
					END IF
					lds_unit_of_work_delete.setitem(ll_insert_row,'delete_flag',ls_delete_flag) 			
				else
					ls_delete_flag = "Y"
					lds_unit_of_work_delete.setitem(ll_insert_row,'delete_flag',ls_delete_flag) 
				end if
								
			END IF
		END IF
				
next	


/* remove any txn unit of work that user flag as not to be removed */
for ll_row = 1 to dw_claimcost_unapplied_claim_txn.rowcount()	
		if dw_claimcost_unapplied_claim_txn.IsSelected(ll_row) then
			ll_txn_unit_of_work_no 	= 	dw_claimcost_unapplied_claim_txn.getitemnumber(ll_row,'txn_unit_of_work_no')
		
			ls_find = 'unit_of_work = ' + string(ll_txn_unit_of_work_no)
			
			li_find = lds_unit_of_work_delete.find(	ls_find,1,lds_unit_of_work_delete.rowcount())
			
			if li_find > 0 then	
				ls_delete_flag = lds_unit_of_work_delete.getitemstring(li_find,'delete_flag')	
				if ls_delete_flag = "N" then
					dw_claimcost_unapplied_claim_txn.selectrow(ll_row,false)
				end if
			end if
		end if		
next


SQLCA.nf_begin_transaction()

/* loop through all the rows on dw - but only remove the selected one's */ 
for ll_row = 1 to dw_claimcost_unapplied_claim_txn.rowcount()	

	if dw_claimcost_unapplied_claim_txn.IsSelected(ll_row) then
		/* these columns are returned from the dw */
		lb_remove 								= true
		ll_claim_no 								=  dw_claimcost_unapplied_claim_txn.getitemnumber(ll_row,'claim_no')
		ll_txn_no 								= 	dw_claimcost_unapplied_claim_txn.getitemnumber(ll_row,'txn_no')	
		ll_related_txn_no						= 	dw_claimcost_unapplied_claim_txn.getitemnumber(ll_row,'related_txn_no')	
		ls_txn_type_code						= 	dw_claimcost_unapplied_claim_txn.getitemstring(ll_row,'txn_type_code')	
		ls_txn_sub_type_code				    = 	dw_claimcost_unapplied_claim_txn.getitemstring(ll_row,'txn_sub_type_code')	
		ll_txn_unit_of_work_no 				= 	dw_claimcost_unapplied_claim_txn.getitemnumber(ll_row,'txn_unit_of_work_no')
		ldtm_payment_processed_date 	=  dw_claimcost_unapplied_claim_txn.getitemdatetime(ll_row,'payment_processed_date')		
		ll_payment_authorization_no 		=  dw_claimcost_unapplied_claim_txn.getitemnumber(ll_row,'payment_authorization_no')		
		ldcm_txn_amount						=  dw_claimcost_unapplied_claim_txn.getitemdecimal(ll_row,'txn_amount')		
		ll_payment_no                             = dw_claimcost_unapplied_claim_txn.getitemnumber(ll_row,'payment_no')
		

		 /* retrieve the payment and columns that will  be used*/
		ll_row_payment = lds_payment.retrieve(ll_payment_no)
		ls_payment_type_code = lds_payment.GetItemString(1,'payment_type_code')
		li_opening_no = lds_payment.GetItemNumber(1,'opening_no')
		
		
		/* ????   RTW Incentive stuff*/
		IF ls_payment_type_code = 'R1' AND NOT (ls_txn_type_code = 'J' AND ls_txn_sub_type_code = '5') THEN
			li_r1_rows = dw_r1_payments_to_delete.RowCount()
			IF li_r1_rows > 0 THEN
				ls_find = 'payment_no = ' + String(ll_payment_no)
				li_r1_find = dw_r1_payments_to_delete.Find(ls_find, 1, li_r1_rows)
			END IF
			
			IF li_r1_find > 0 THEN
				// the row already exists in external datawindow
				// so, update the txn sum
				ldec_new_r1_txn_sum = dw_r1_payments_to_delete.GetItemDecimal(li_r1_find,'sum_txn_amount') + ldcm_txn_amount
				dw_r1_payments_to_delete.SetItem(li_r1_find,'sum_txn_amount',ldec_new_r1_txn_sum)
			ELSE
				// the row does not exist in the external datawindow
				// insert a row
				li_new_row = dw_r1_payments_to_delete.InsertRow(0)
				
				SELECT	b.tier_no
				INTO		:li_tier_no
				FROM		PAYMENT a
				JOIN		RTW_INCENTIVE_PAYMENT_XREF b ON a.payment_no = b.payment_no
				WHERE	a.payment_no = :ll_payment_no
				USING SQLCA;
				
				SQLCA.nf_handle_error(is_window_name,"select tier_no FROM RTW_INCENTIVE_PAYMENT_XREF",ls_script)
				
				dw_r1_payments_to_delete.SetItem(li_new_row,'claim_no',ll_claim_no)
				dw_r1_payments_to_delete.SetItem(li_new_row,'opening_no',li_opening_no)
				dw_r1_payments_to_delete.SetItem(li_new_row,'payment_no',ll_payment_no)
				dw_r1_payments_to_delete.SetItem(li_new_row,'tier_no',li_tier_no)
				dw_r1_payments_to_delete.SetItem(li_new_row,'sum_txn_amount',ldcm_txn_amount)
			END IF					
		END IF
		
		
		/* remove the txn unit of work if no more txn exists */
		ls_find = "txn_unit_of_work_no = " + string(ll_txn_unit_of_work_no)  + ' AND  related_txn_no <> ' + string(ll_related_txn_no)
		ll_found =  dw_claimcost_unapplied_claim_txn.Find(ls_find, 1,  dw_claimcost_unapplied_claim_txn.RowCount())	
		if ll_found = 0 then
			DELETE FROM dbo.TXN_UNIT_OF_WORK  
			WHERE dbo.TXN_UNIT_OF_WORK.txn_unit_of_work_no = :ll_txn_unit_of_work_no
			USING sqlca;
			SQLCA.nf_handle_error("DELETE FROM dbo.TXN_UNIT_OF_WORK  ",is_window_name,ls_script)
		end if
		

         /*  we need to adjust the rehab authorization for those txn thta have payment link to an authorization */
		if ll_payment_authorization_no > 0 then
			
			
			/* claim correction and rehab qty correction */
			if (ls_txn_type_code = "T" AND ls_txn_sub_type_code = "6")  or (ls_txn_type_code = 'J' AND ls_txn_sub_type_code <> "4" ) THEN
				
				
				ll_adjustment_quantity 	= 	dw_claimcost_unapplied_claim_txn.getitemnumber(ll_row,'adjustment_quantity')
				if isnull(ll_adjustment_quantity) then ll_adjustment_quantity 	= 	dw_claimcost_unapplied_claim_txn.getitemnumber(ll_row,'paid_quantity')
				ldcm_txn_amount			=  dw_claimcost_unapplied_claim_txn.getitemdecimal(ll_row,'txn_amount')	
	
					
				ls_find = 'claim_no = ' + string(ll_claim_no) + ' and authorization_no = ' + string(ll_payment_authorization_no)
		  
				ll_found = lds_rehab_task_authorization_update.find(ls_find,1,lds_rehab_task_authorization_update.rowcount())
				
				if ll_found = 0 then
					ll_ins_row_rehab_task_auth = lds_rehab_task_authorization_update.insertrow(0)
				
					lds_rehab_task_authorization_update.setitem(ll_ins_row_rehab_task_auth,'adjustment_rehab_qty',ll_adjustment_quantity )
					lds_rehab_task_authorization_update.setitem(ll_ins_row_rehab_task_auth,'adjustment_amount',ldcm_txn_amount )
					lds_rehab_task_authorization_update.setitem(ll_ins_row_rehab_task_auth,'claim_no',ll_claim_no)
					lds_rehab_task_authorization_update.setitem(ll_ins_row_rehab_task_auth,'authorization_no',ll_payment_authorization_no)
					lds_rehab_task_authorization_update.setitem(ll_ins_row_rehab_task_auth,'update_authorization',"N")
								
					IF  ls_txn_type_code	= 'T' and ls_txn_sub_type_code = '6' THEN
						lds_rehab_task_authorization_update.setitem(ll_ins_row_rehab_task_auth,'update_authorization',"Y")
					ELSE
						lds_rehab_task_authorization_update.setitem(ll_ins_row_rehab_task_auth,'update_authorization',"N")
					END IF				
				else
					lds_rehab_task_authorization_update.setitem(ll_found,'adjustment_rehab_qty',ll_adjustment_quantity + lds_rehab_task_authorization_update.getitemnumber(ll_found,'adjustment_rehab_qty' ))
					lds_rehab_task_authorization_update.setitem(ll_found,'adjustment_amount',ldcm_txn_amount  + lds_rehab_task_authorization_update.getitemdecimal(ll_found,'adjustment_amount'))				
				end if
				
				
			end if	
			
			
		end if
		
		
	    /*  remove the unapplied claim txn and decrments the looping counter*/
		li_rc = dw_claimcost_unapplied_claim_txn.deleterow(ll_row)
		if li_rc = -1 then SignalError(-666,'Problem deleting unapplied claim txn')
		ll_row --
	
	end if

next
	
		
/* P10261 - Job Search RTW Incentive payments */

li_r1_rows = dw_r1_payments_to_delete.RowCount()
IF li_r1_rows > 0 THEN
	// P10261 - Job Search RTW Incentive payments
	// ensure that deletion of a negative adjustment does not result
	// in an overpayment of the tier
	// if this is the case, then prevent the deletion & display an error.
	FOR li_counter = 1 TO li_r1_rows
		ll_r1_claim_no		= dw_r1_payments_to_delete.GetItemNumber(li_counter, 'claim_no')
		li_r1_opening_no	= dw_r1_payments_to_delete.GetItemNumber(li_counter, 'opening_no')
		ll_r1_payment_no	= dw_r1_payments_to_delete.GetItemNumber(li_counter, 'payment_no')
		li_r1_tier_no		= dw_r1_payments_to_delete.GetItemNumber(li_counter, 'tier_no')
		ldec_r1_txn_sum	= dw_r1_payments_to_delete.GetItemDecimal(li_counter, 'sum_txn_amount')
		
		SELECT	rtw_incentive_amount
		INTO		:ldec_rtw_incentive_amount
		FROM		Rtw_Incentive_Tier
		WHERE	rtw_incentive_type_code	= 'JS'
		AND		tier_no							= :li_r1_tier_no
		USING SQLCA;
		
		SQLCA.nf_handle_error(is_window_name,"SELECT rtw_incentive_amount FROM Rtw_Incentive_Tier",ls_script)
		
		ll_row_payment = lds_txn_sum_for_tier.Retrieve(ll_r1_claim_no, li_r1_opening_no, li_r1_tier_no)
		
		IF ll_row_payment = 1  THEN
			// current amount associated with the claim/opening/tier
			// the sum includes the txns that are about to be deleted
			ldec_sum_txns = lds_txn_sum_for_tier.GetItemDecimal(1,'sum_txns')
			
			IF ldec_sum_txns - ldec_r1_txn_sum > ldec_rtw_incentive_amount THEN
				SQLCA.nf_rollback_transaction()
				MessageBox('Deletion Error', 'At least one of the transactions in this unit of work cannot be deleted.' &
											+ '~n~nThe deletion of the chosen transactions for RTW Incentive payment #'+String(ll_r1_payment_no) &
											+ '~nwould result in a new payment total for RTW Incentive tier #' +String(li_r1_tier_no)+' for claim #'+String(ll_claim_no)+',' &
											+ '~nopening #'+String(li_r1_opening_no)+' that would exceed the amount authorized for the tier ('+String(ldec_rtw_incentive_amount,'$#,##0')+').',StopSign!)
				
				RETURN
			END IF
		END IF
		
	NEXT
	
END IF



/* this loop is used to update the rehab qty and paid amounts */
/* 11.35	The paid amount & paid quantity for the To Claim rehab authorization must be reduced by the amount of the unprocessed transaction(s) being deleted for the To Claim, if that payment is linked to a rehab authorization.*/



for ll_row = 1 to lds_rehab_task_authorization_update.rowcount()
	
	ll_adjustment_quantity			=	lds_rehab_task_authorization_update.getitemnumber(ll_row,'adjustment_rehab_qty' )
	ldcm_txn_amount 					= 	lds_rehab_task_authorization_update.getitemdecimal(ll_row,'adjustment_amount' )
	ll_claim_no 							= 	lds_rehab_task_authorization_update.getitemnumber(ll_row,'claim_no')
	ll_payment_authorization_no	=	lds_rehab_task_authorization_update.getitemnumber(ll_row,'authorization_no')
		ls_update_authorization			=	lds_rehab_task_authorization_update.getitemstring(ll_row,'update_authorization')
		

	ll_rc = lds_rehab_task_authorization.retrieve(ll_claim_no,ll_payment_authorization_no)
	SQLCA.nf_handle_error("lds_rehab_task_authorization - retrieve()",is_window_name,ls_script)
	
	if ll_rc <> 1 then
		SignalError(-666,'Retrieve - Problem finding rehab authorization for authorization no ' + string(ll_payment_authorization_no) )
	end if

	ll_org_paid_quantity					= 	lds_rehab_task_authorization.getitemnumber(1,'paid_quantity')
	ldcm_org_paid_amount				=	lds_rehab_task_authorization.getitemdecimal(1,'paid_amount')
	ll_org_authorized_quantity		   	=  lds_rehab_task_authorization.getitemnumber(1,'authorized_quantity')
	ldcm_org_authorized_amount		=  lds_rehab_task_authorization.getitemdecimal(1,'authorized_amount')
	
	
	if ls_update_authorization = "Y" THEN
			lds_rehab_task_authorization.setitem(1,'authorized_quantity',  ll_org_authorized_quantity + (ll_adjustment_quantity * -1))	
			lds_rehab_task_authorization.setitem(1,'authorized_amount',  ldcm_org_authorized_amount + (ldcm_txn_amount * -1))
	END IF
					

	 lds_rehab_task_authorization.setitem(1,'paid_amount', ldcm_org_paid_amount + ( ldcm_amount_to_adjust *-1))
     lds_rehab_task_authorization.setitem(1,'paid_quantity',ll_org_paid_quantity + ( ll_qty_to_adjust * -1))
		
			
	IF ll_org_authorized_quantity < ll_org_paid_quantity + (ll_adjustment_quantity * -1) THEN
		// reset, re-retrieve?
		MessageBox('Deletion Error', 'At least one of the transactions in this unit of work cannot be deleted.' &
										+ '~nThe deletion would result in the new total paid quantity for ' &
										+ '~nauthorization #'+String(ll_payment_authorization_no)+' exceeding the authorized quantity ('+String(ll_org_authorized_quantity)+').',StopSign!)
		SQLCA.nf_rollback_transaction()
		RETURN	
	END IF
		
					
	ll_rc = lds_rehab_task_authorization.update()
	SQLCA.nf_handle_error("ds_rehab_task_authorization.update()",is_window_name,ls_script)	
	
	if ll_rc < 0 then SignalError(-666,'Problem updating REHAB TASK AUTHORIZATION . Authorization is' + string(ll_payment_authorization_no))

next



/* take care removing the txn's , payments , cost allocation */
for ll_row = 1 to dw_claimcost_unapplied_claim_txn.deletedCount()
	
	ll_related_txn_no 	                        = dw_claimcost_unapplied_claim_txn.getitemnumber(ll_row,'related_txn_no',delete!,true)	
	ll_txn_no 				                   = dw_claimcost_unapplied_claim_txn.getitemnumber(ll_row,'txn_no',delete!,true)	
	ll_payment_no 		                       = dw_claimcost_unapplied_claim_txn.getitemnumber(ll_row,'payment_no',delete!,true)	
	ls_txn_type_code						= 	dw_claimcost_unapplied_claim_txn.getitemstring(ll_row,'txn_type_code',delete!,true)	
	ls_txn_sub_type_code				    = 	dw_claimcost_unapplied_claim_txn.getitemstring(ll_row,'txn_sub_type_code',delete!,true)	
     ldcm_txn_amount						=  dw_claimcost_unapplied_claim_txn.getitemdecimal(ll_row,'txn_amount',delete!,true)
			

	ll_rc =  dw_claimcost_unapplied_claim_txn.Find("related_txn_no = " + string(ll_related_txn_no), 1, dw_claimcost_unapplied_claim_txn.ROWCOUNT() ) 
	if ll_rc <> 0 then SignalError(-666,'Problem with dw_claimcost_unapplied_claim_txn.Find()')
		
	/* remove coc allocated - phase ii will be more than one */
	ll_row_coc_allocated = lds_cost_of_claims_allocated.retrieve(ll_txn_no)
	for ll_row_coc = 1 to ll_row_coc_allocated
		li_rc = lds_cost_of_claims_allocated.deleterow(ll_row_coc)
		if li_rc < 0 then SignalError(-666,'Problem with  lds_cost_of_claims_allocated.deleterow()')		
	next

	lds_cost_of_claims_allocated.update()
	SQLCA.nf_handle_error("lds_cost_of_claims_allocated.update()",is_window_name,ls_script)

	/* remove the adjustment txn work table */
	ll_row_adjust_txn_work = lds_adjustment_txn_work_table.retrieve(ll_payment_no,ll_txn_no)
	
	if ll_row_adjust_txn_work = 0 then
	elseif ll_row_adjust_txn_work = 1 then
		
		li_rc = lds_adjustment_txn_work_table.deleterow(ll_row_adjust_txn_work)
		if li_rc = -1 then SignalError(-666,'Problem with lds_adjustment_txn_work_table.deleterow()')
		
		lds_adjustment_txn_work_table.update()
		SQLCA.nf_handle_error("lds_adjustment_txn_work_table.update()",is_window_name,ls_script)
	
	elseif ll_row_adjust_txn_work < 0 then 
		SignalError(-666,'Integrity problem with  lds_adjustment_txn_work_table.deleterow()')
	END IF
	
	
	
	/* see if payment is new or already processed */
	ldtm_payment_processed_date =  dw_claimcost_unapplied_claim_txn.getitemdatetime(ll_row,'payment_processed_date',delete!,true)		
		
	if isnull(ldtm_payment_processed_date) then
		ll_row_payment = lds_payment.retrieve(ll_payment_no)
		li_rc = lds_payment.deleterow(ll_row_payment)
		if li_rc = -1 then SignalError(-666,'problem deleting lds_payment.deleterow(ll_row_payment)')
		lds_payment.UPDATE()
		SQLCA.nf_handle_error("lds_payment.UPDATE()",is_window_name,ls_script)
	end if
	
	
    /*  ephysio project - we need to reset the amended code back  for
	       adjustment - cancel cheque or deport and overpayment recovery
		  Transfer - claim correctio
		  BR 11.70	The rehab invoice line item must be ‘un-amended’ for the From Claim if the unprocessed transaction(s) being deleted for the From Claim  is linked to the rehab invoice line item and the rehab invoice line item is amended.
	*/
	 
	 
	if ( (ls_txn_type_code = 'J' AND ls_txn_sub_type_code= "2") or (ls_txn_type_code = 'J'  and ls_txn_sub_type_code = "3")  or  (ls_txn_type_code = 'T'  AND ls_txn_sub_type_code= "6"))  then
					  
		/* RETRIEVE THE REHAB INVOICE LINE ITEM BY PAYMENT NO */
		 li_rc = lds_rehab_invoice_line_item.retrieve(ll_payment_no)
		 SQLCA.nf_handle_error("lds_rehab_invoice_line_item.retrieve()",is_window_name,ls_script)
					
		/* IF MORE THAN ONE ROW THEN THIS IS INTERGRITY ISSUE*/			
		if (li_rc > 1) then SignalError(-666,"Data Intergrity Issue with Rehab Invoice Line Item - Expecting only one row - return more than one")
		
		if (li_rc = 1) then
			
			 ll_rehab_invoice_no =  lds_rehab_invoice_line_item.getitemnumber(1,'rehab_invoice_no')
			 
			/* IF REHAB INVOICE LINE ITEM IS FOUND and full adjustment - WE NEED TO UPDATE THE AMENDED CODE back to ''*/
			if (li_rc = 1 and ldcm_txn_amount = 0) then
					 lds_rehab_invoice_line_item.setitem(1,'line_item_amended_code','')			 
			end if
		end if
		
		/* 11.80	The rehab invoice line item must be deleted for the To Claim if the unprocessed transaction(s) being deleted for the To Claim  is linked to a rehab invoice.
             11.90	The rehab invoice must be deleted for the To Claim if the rehab invoice line item is being deleted for the To Claim (refer to rule 11.80) and there are no other payments linked to the rehab invoice.
		*/


		/* on a claim transfer - need to handle deleting the rehab invoice line item and rehab invoice if they exists */
	    if (ls_txn_type_code = 'T'  AND ls_txn_sub_type_code= "6") then
		
			
		    select  rehab_invoice_no
			into : ll_rehab_invoice_no_to_delete
			FROM dbo.REHAB_INVOICE_LINE_ITEM
			WHERE from_rehab_invoice_no = :ll_rehab_invoice_no 
			USING sqlca;			
			
			if SQLCA.nf_handle_error("REHAB_INVOICE_LINE_ITEM",is_window_name,ls_script) = 100 then
		    else	
				  DELETE FROM dbo.REHAB_INVOICE_LINE_ITEM
				WHERE rehab_invoice_no = :ll_rehab_invoice_no_to_delete
				USING sqlca;
				SQLCA.nf_handle_error("REHAB_INVOICE",is_window_name,ls_script)
				
				 DELETE FROM dbo.REHAB_INVOICE
				WHERE rehab_invoice_no = :ll_rehab_invoice_no_to_delete
				USING sqlca;
				SQLCA.nf_handle_error("REHAB_INVOICE",is_window_name,ls_script)	
			end if	
		
		end if
		
		
	end if

	
	/* update the rehab invoice line item - if required */
	if lds_rehab_invoice_line_item.rowcount()> 0 then
		 lds_rehab_invoice_line_item.update()
		 SQLCA.nf_handle_error(" ids_rehab_invoice_line_item.update()",is_window_name,ls_script)
	end if

next

dw_claimcost_unapplied_claim_txn.update()
SQLCA.nf_handle_error("dw_claimcost_unapplied_claim_txn.update()",is_window_name,ls_script)

SQLCA.nf_commit_transaction()



/* destroy datastores */
if isvalid(lds_payment) 									then   destroy lds_payment
if isvalid(lds_cost_of_claims_allocated) 				then	destroy lds_cost_of_claims_allocated
if isvalid(lds_adjustment_txn_work_table) 			then	destroy lds_adjustment_txn_work_table
if isvalid (lds_rehab_task_authorization) 				then	destroy lds_rehab_task_authorization
if isvalid (lds_rehab_task_authorization_update) 	then	destroy lds_rehab_task_authorization_update
if isvalid (lds_rehab_invoice_line_item) 	             then	destroy  lds_rehab_invoice_line_item


/*refresh dw */
dw_transaction_list.RESET()
dw_claimcost_unapplied_claim_txn.setredraw(true)			
end event

type dw_confirmation from u_dw_online within w_claim_cost_maintenance
boolean visible = false
integer y = 672
integer width = 4571
integer height = 576
integer taborder = 60
string title = "none"
string dataobject = "d_confirm"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = false
borderstyle borderstyle = styleraised!
end type

event constructor;THIS.object.datawindow.hidegrayline = true
end event

event ue_print;THIS.Modify("DataWindow.Print.Orientation= '1' ") // landscape
THIS.print()

end event

type dw_claimcost_unapplied_claim_txn from u_dw_online within w_claim_cost_maintenance
boolean visible = false
integer y = 672
integer width = 4571
integer height = 1472
integer taborder = 30
string dataobject = "d_claimcost_unapplied_claim_txn"
boolean vscrollbar = true
boolean livescroll = false
borderstyle borderstyle = styleraised!
end type

event constructor;call super::constructor;
this.SetTransObject(SQLCA)
THIS.object.datawindow.hidegrayline = true
end event

event clicked;call super::clicked;String 	dwobjectname
string 	ls_column_name
STRING	ls_txn_type_code
STRING	ls_txn_sub_type_code

string	ls_status


long		ll_target_payment_no
long 		ll_txn_unit_of_work


LONG		ll_row

long 		ll_target_related_txn_no
long 		ll_related_txn_no
long 		ll_target_txn_unit_of_work

long		ll_message_count
long		ll_txn_no
long		ll_payment_no
integer 	li_tab
integer 	li_rc

boolean 	lb_off

long	ll_prev_unit_of_work 



/* need to put comments in code */

setpointer(hourglass!)

this.setredraw(false)


dwobjectname = this.GetObjectAtPointer()
li_tab = Pos(dwobjectname, "~t", 1)
ls_column_name = Mid(dwobjectname, 1, li_tab - 1)
ll_row = long(Mid(dwobjectname, li_tab + 1))



choose case ls_column_name
			
	/* select all/deselect all */
	case 't_unit_of_work'
	
	
		ll_message_count = 0
		
		li_rc = messagebox("Warning","About to select/de-select all unit of work. Continue?",Question!,YesNo!)
		if li_rc = 2 then 
			this.setredraw(true)
			return 
		end if
		
		lb_off = false
		
		if this.isselected(ll_row) then
			lb_off = true	
		end if

		ll_prev_unit_of_work 	 = -1
		
		for ll_row = 1 to this.rowcount()
			ll_txn_unit_of_work  	 =  this.getitemnumber(ll_row,'txn_unit_of_work_no_1')

			if ll_prev_unit_of_work <> ll_txn_unit_of_work then
			 	ll_message_count = 0
			end if
			
		
			ls_status 					 =  this.getitemstring(ll_row,'txn_unit_of_work_ready_to_process_flag')
			ll_txn_no 					 =  this.getitemnumber(ll_row,'txn_no')
			ll_target_related_txn_no =  this.getitemnumber(ll_row,'related_txn_no')
			ll_payment_no			 	 =  this.getitemnumber(ll_row,'payment_no')
 
 		
	
			if lb_off then
				this.selectrow(ll_row,false)	
				this.setitem(ll_row,'group_txn',0)
			else	
				if ls_status = "N" THEN				
					if ll_message_count = 0 then
						MESSAGEBOX("Warning","You have selected a 'unit of work' that have been set to on-hold. unit of work is " & 
						+ string(ll_txn_unit_of_work) + " ." )
					end if	
					ll_message_count++
				ELSE
					this.selectrow(ll_row,true)
					this.setitem(ll_row,'group_txn',1)
				END IF
			end if
		next
		
		
		
		
		
		
	case 'txn_unit_of_work_no_1'
		
		   lb_off = false
			ll_target_txn_unit_of_work = this.getitemnumber(ll_row,'txn_unit_of_work_no_1')
						
			for ll_row = 1 to this.rowcount()
			
				ll_txn_unit_of_work = this.getitemnumber(ll_row,'txn_unit_of_work_no_1')
 
 				
				if ll_txn_unit_of_work = ll_target_txn_unit_of_work then
					if this.isselected(ll_row) then
						lb_off = true	
						exit
					end if		
				end if
				
			next
			
			
		
			ll_prev_unit_of_work 	 = -1
				
			for ll_row = 1 to this.rowcount()
				ll_txn_unit_of_work = this.getitemnumber(ll_row,'txn_unit_of_work_no_1')
 				ls_status =  this.getitemstring(ll_row,'txn_unit_of_work_ready_to_process_flag')
				
				if ll_prev_unit_of_work <> ll_txn_unit_of_work then
			 		ll_message_count = 0
				end if
				
				ll_prev_unit_of_work = ll_txn_unit_of_work
			
			
				if ll_txn_unit_of_work = ll_target_txn_unit_of_work then
					if lb_off then
						this.selectrow(ll_row,false)	
						this.setitem(ll_row,'group_txn',0)
					else	
						
						if ls_status = "N" THEN
							IF ll_message_count = 0 then
								MESSAGEBOX("Warning","You have selected a 'unit of work' that have been set to on-hold. unit of work is " + string(ll_txn_unit_of_work))
							end if
							ll_message_count++
						ELSE
							this.selectrow(ll_row,true)
							this.setitem(ll_row,'group_txn',1)
						END IF
					end if
				end if
			next
			
		this.setitem(ll_row,'unit_of_work',1)
	
		
		
	case 'group_txn'
		ll_target_txn_unit_of_work = this.getitemnumber(ll_row,'txn_unit_of_work_no_1')
		ll_target_related_txn_no 	= this.getitemnumber(ll_row,'related_txn_no')
		ll_target_payment_no 		= this.getitemnumber(ll_row,'payment_no')
		ls_txn_type_code 				= this.getitemstring(ll_row,'txn_type_code')
		ls_txn_sub_type_code			= this.getitemstring(ll_row,'txn_sub_type_code')
		ll_message_count = 0
		
		
		lb_off = false
		
		if this.isselected(ll_row) then
			lb_off = true	
		end if
		
		ll_prev_unit_of_work 	 = -1
		
		for ll_row = 1 to this.rowcount()
			ll_related_txn_no = this.getitemnumber(ll_row,'related_txn_no') 
			ls_status =  this.getitemstring(ll_row,'txn_unit_of_work_ready_to_process_flag') 

			if ll_prev_unit_of_work <> ll_txn_unit_of_work then
			 	ll_message_count = 0
			end if
				
			ll_prev_unit_of_work = ll_txn_unit_of_work
		
			
			if ll_related_txn_no = ll_target_related_txn_no then
				if lb_off then
					this.selectrow(ll_row,false)
					this.setitem(ll_row,'group_txn',0)
				else
					if ls_status = "N" THEN
						IF ll_message_count = 0 then
							MESSAGEBOX("Warning","You have selected a 'unit of work' that have been set to on-hold. unit of work is " + string(ll_txn_unit_of_work))
						end if
						ll_message_count++
					ELSE
						this.selectrow(ll_row,true)
						this.setitem(ll_row,'group_txn',1)
					END IF
				end if
			END IF
		next
	
		
			
		for ll_row = 1 to this.rowcount()
			
			ll_payment_no = this.getitemnumber(ll_row,'payment_no') 

			if ll_payment_no = ll_target_payment_no then
				if lb_off then 
					this.selectrow(ll_row,false)
					this.setitem(ll_row,'group_txn',0)
				else
					this.selectrow(ll_row,true)
					this.setitem(ll_row,'group_txn',1)
				end if	
			end if


		next
		
		
		if ls_txn_type_code = 'J' and ls_txn_sub_type_code = '2' then
				

				
			for ll_row = 1 to this.rowcount()
					
				ll_txn_unit_of_work = this.getitemnumber(ll_row,'txn_unit_of_work_no_1')
 
 		
				if ll_txn_unit_of_work = ll_target_txn_unit_of_work then
					if lb_off then 
						this.selectrow(ll_row,false)
						this.setitem(ll_row,'group_txn',0)
					else
						this.selectrow(ll_row,true)
						this.setitem(ll_row,'group_txn',1)
					end if	
				end if
			next
				
		end if
		
		
				
end choose

this.setredraw(true)



end event

event rbuttondown;//
///*	
//	Create the menu -	Note that this only gives default options.  If 
//							you want additional options, you should override
//							the ancestor and visible the options you desire.
//*/
//	If not isvalid(im_popup) Then
//
//		im_popup = Create m_dw_online_rmb_popup
//		im_popup.mf_set_datawindow(This)
//		im_popup.m_options.m_filterlist.visible = TRUE	
//		im_popup.m_options.m_sort.visible = TRUE	
//		//im_popup.m_options.m_tooltips.visible = TRUE	
//	End if
//
//	im_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))
//
//	//Destroy lm_popup
end event

event ue_filter;call super::ue_filter;

//string ls_null
//
//setnull(ls_null)
//
//this.setfilter(ls_null)
//
//this.filter()
end event

event ue_print;THIS.Modify("DataWindow.Print.Orientation= '1' ") // landscape
THIS.Print()
end event

type cb_refresh from commandbutton within w_claim_cost_maintenance
boolean visible = false
integer x = 2917
integer y = 2176
integer width = 402
integer height = 104
integer taborder = 180
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Refresh"
end type

event clicked;
dw_claimcost_unapplied_claim_txn.retrieve(vgst_user_profile.work_group_code)

end event

type cb_unit_of_work_save from commandbutton within w_claim_cost_maintenance
boolean visible = false
integer x = 3342
integer y = 2176
integer width = 402
integer height = 104
integer taborder = 150
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Status"
end type

event clicked;


OPEN(w_popup_txn_unit_of_work_status)

cb_refresh.triggerevent(clicked!)



end event

type cb_cancel from commandbutton within w_claim_cost_maintenance
boolean visible = false
integer x = 3767
integer y = 2176
integer width = 379
integer height = 104
integer taborder = 140
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
end type

event clicked;integer li_rc
  
if cb_save.enabled = false then
else
	li_rc = MessageBox("Cancel", "About to Abort Confirmation - Continue",Question!, YesNo!, 2)
	
	IF li_rc = 1 THEN
		
		cb_search.enabled = true
		cb_find.enabled = true
		cb_clear.enabled = true
		cbx_1.enabled 	= true
		cb_confirm.visible = true
		cb_close.visible = true
		cb_save.visible = false
		cb_cancel.visible = false
		dw_confirmation.visible = false	
		uo_filter.visible = true 
		st_splitbar_1.visible		= false
		dw_confirmation.reset()
		cb_save.enabled 				= true
		dw_parameter.enabled = true
		dw_confirm_summary_totals.visible = false
	
		dw_claimcost_unapplied_claim_txn.visible = false
			
		inv_ccm.of_reset()
		dw_transaction_list.visible = true
		dw_transaction_list.setredraw(true)
		inv_ccm.ib_suppress_warnings = FALSE
		
	end if
end if

end event

type cb_confirm from commandbutton within w_claim_cost_maintenance
integer x = 3767
integer y = 2176
integer width = 379
integer height = 104
integer taborder = 110
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Con&firm"
end type

event clicked;/* show the summary */

dw_confirm_summary_totals.reset()
dw_confirm_summary_totals.setredraw(false)
dw_confirmation_summary.reset()

if wf_confirm() < 0 then
	inv_ccm.of_reset()
	dw_transaction_list.setfocus()
	dw_transaction_list.enabled = true
	dw_parameter.enabled = true
	return
end if

end event

type dw_transaction_list from u_dw_online within w_claim_cost_maintenance
event ue_show_transfer_details ( string as_detail_type )
integer y = 672
integer width = 4571
integer height = 1472
integer taborder = 250
string dataobject = "d_claim_cost_transaction_list"
boolean vscrollbar = true
boolean livescroll = false
borderstyle borderstyle = styleraised!
end type

event ue_show_transfer_details(string as_detail_type);/*

Any changes made to this script should also be made in 
w_claim_cost_maintenance_inquiry.dw_txn_list.ue_show_transfer_details and
w_payment_inquiry.dw_list_payments.ue_show_transfer_details

*/

LONG			ll_selected_txn_no
LONG			ll_selected_payment_no
LONG			ll_payment_no, ll_max_payment_no
LONG			ll_claim_no
LONG			ll_max_applied_payment_no, ll_max_applied_txn_no
LONG			ll_max_unapplied_payment_no , ll_max_unapplied_txn_no
LONG			ll_cost_alloc_no , ll_cost_alloc_operation_no
STRING		ls_message
STRING		ls_txn_amount_desc
STRING		ls_txn_sub_type_code
STRING		ls_payment_type_code
STRING		ls_payment_sub_type_code
DECIMAL{2}	ldec_txn_amount
DECIMAL{2}  ldec_selected_txn_amount
LONG			ll_rows, ll_rows_unapplied
u_ds			lds_max_payment_applied, lds_max_payment_unapplied
u_ds			lds_transfer_details_applied, lds_transfer_details_unapplied

lds_transfer_details_applied	= CREATE u_ds
lds_transfer_details_unapplied	= CREATE u_ds
lds_max_payment_applied		= CREATE u_ds
lds_max_payment_unapplied	= CREATE u_ds

IF GetRow() = 0 THEN
	MessageBox('No Records','You have not chosen a record.')
	RETURN
END IF


ll_selected_txn_no = GetItemNumber(GetRow(),'txn_no')
ll_selected_payment_no = GetItemNumber(GetRow(),'payment_no')
ldec_selected_txn_amount = GetItemDecimal(GetRow(),'txn_amount')

CHOOSE CASE as_detail_type
	CASE 'TO'
		//The user wants to know where this transaction was transfered "to"
		//Get the maximum payment_no that is related to the selected transaction 
		
		lds_max_payment_applied.DataObject = 'd_xfer_to_applied_max_payment'
		lds_max_payment_applied.SetTransObject(SQLCA)
		ll_rows = lds_max_payment_applied.Retrieve(ll_selected_txn_no)
		IF ll_rows < 0 Then
			SignalError(-666,'Error retrieving applied transfer details.')
		End if
		SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','retrieve')
		
		lds_max_payment_unapplied.DataObject = 'd_xfer_to_unapplied_max_payment'
		lds_max_payment_unapplied.SetTransObject(SQLCA)
		ll_rows_unapplied = lds_max_payment_unapplied.Retrieve(ll_selected_txn_no)
		IF ll_rows_unapplied < 0 Then
			SignalError(-666,'Error retrieving unapplied transfer details.')
		End if
		SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','retrieve')
				
		IF ll_rows = 0 AND ll_rows_unapplied = 0 Then
			MessageBox('Not transferred','This transaction has not been transferred.')
			
		ELSEIF ll_rows <> 0 AND ll_rows_unapplied = 0 THEN // only applied txns transferred
			ll_max_applied_payment_no = lds_max_payment_applied.GetItemNumber(1,'max_transfer_payment_no')
			
			//Gather the information and create the message string to pass into the message window.
			
			// the txn sub type will determine the data passed to the messagebox
			SELECT	txn_sub_type_code , Max(txn_no)
			INTO		:ls_txn_sub_type_code , :ll_max_applied_txn_no
			FROM		APPLIED_CLAIM_TXN
			WHERE	payment_no	= :ll_max_applied_payment_no
			AND		txn_type_code = 'T'
			GROUP BY txn_sub_type_code
			USING	SQLCA;
			SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','select max txn from APPLIED_CLAIM_TXN')
			
			CHOOSE CASE ls_txn_sub_type_code
				CASE '7','8'
					SELECT	b.cost_alloc_no , b.cost_alloc_operation_no , a.txn_amount
					INTO		:ll_cost_alloc_no , :ll_cost_alloc_operation_no, :ldec_txn_amount
					FROM		APPLIED_CLAIM_TXN					a,
								COST_OF_CLAIMS_ALLOCATED	b
					WHERE	a.txn_no = b.txn_no
					AND		a.txn_no	= :ll_max_applied_txn_no
					USING	SQLCA;
					SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','select data for max txn from APPLIED_CLAIM_TXN')
					
					ls_message = ' (' + String(ldec_txn_amount,'$#,##0.00') + ') of the selected transaction was transferred to '&
						+ 'cost alloc ' + String(ll_cost_alloc_no) +'/'+ String(ll_cost_alloc_operation_no) + '.  See payment #' + String(ll_max_applied_payment_no) + '.'
						
				CASE '6'
					SELECT	claim_no , txn_amount
					INTO		:ll_claim_no , :ldec_txn_amount
					FROM		APPLIED_CLAIM_TXN
					WHERE	txn_no	= :ll_max_applied_txn_no
					USING	SQLCA;
					SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','select data for max txn from APPLIED_CLAIM_TXN')
					
					ls_message = ' (' + String(ldec_txn_amount,'$#,##0.00') + ') of the selected transaction was transferred to '&
						+ 'claim #' + String(ll_claim_no) + '.  See payment #' + String(ll_max_applied_payment_no) + '.'
						
				CASE '9'
					SELECT	b.payment_type_code , b.payment_sub_type_code , a.txn_amount
					INTO		:ls_payment_type_code , :ls_payment_sub_type_code , :ldec_txn_amount
					FROM		APPLIED_CLAIM_TXN	a ,
								PAYMENT				b
					WHERE	a.payment_no	= b.payment_no
					AND		a.txn_no			= :ll_max_applied_txn_no
					USING	SQLCA;
					SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','select data for max txn from APPLIED_CLAIM_TXN')
					
					IF ls_payment_sub_type_code <> '' THEN
						ls_message = ' (' + String(ldec_txn_amount,'$#,##0.00') + ') of the selected transaction was transferred to '&
							+ 'payment type/sub type ' + ls_payment_type_code +'/'+ ls_payment_sub_type_code  + '.  See payment #' + String(ll_max_applied_payment_no) + '.'
					ELSE
						ls_message = ' (' + String(ldec_txn_amount,'$#,##0.00') + ') of the selected transaction was transferred to '&
							+ 'payment type ' + ls_payment_type_code + '.  See payment #' + String(ll_max_applied_payment_no) + '.'
					END IF
					
			END CHOOSE
					
			IF ldec_selected_txn_amount = ldec_txn_amount Then
				ls_message = 'The full amount' + ls_message
			Else
				ls_message = 'A portion' + ls_message
			End if
			
			MessageBox('Transfer',ls_message)
			
		ELSEIF ll_rows = 0 AND ll_rows_unapplied <> 0 THEN // only unapplied txns transferred
			ll_max_unapplied_payment_no = lds_max_payment_unapplied.GetItemNumber(1,'max_transfer_payment_no')
			
			//Gather the information and create the message string to pass into the message window.
			
			// the txn sub type will determine the data passed to the messagebox
			SELECT	txn_sub_type_code , Max(txn_no)
			INTO		:ls_txn_sub_type_code , :ll_max_unapplied_txn_no
			FROM		UNAPPLIED_CLAIM_TXN
			WHERE	payment_no	= :ll_max_unapplied_payment_no
			AND		txn_type_code = 'T'
			GROUP BY txn_sub_type_code
			USING	SQLCA;
			SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','select max txn from UNAPPLIED_CLAIM_TXN')
			
			CHOOSE CASE ls_txn_sub_type_code
				CASE '7','8'
					SELECT	b.cost_alloc_no , b.cost_alloc_operation_no , a.txn_amount
					INTO		:ll_cost_alloc_no , :ll_cost_alloc_operation_no, :ldec_txn_amount
					FROM		UNAPPLIED_CLAIM_TXN				a,
								COST_OF_CLAIMS_ALLOCATED	b
					WHERE	a.txn_no = b.txn_no
					AND		a.txn_no	= :ll_max_unapplied_txn_no
					USING	SQLCA;
					SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','select data for max txn from UNAPPLIED_CLAIM_TXN')
					
					ls_message = ' (' + String(ldec_txn_amount,'$#,##0.00') + ') of the selected transaction is scheduled to be transferred to '&
						+ 'cost alloc ' + String(ll_cost_alloc_no) +'/'+ String(ll_cost_alloc_operation_no) + '.  See payment #' + String(ll_max_unapplied_payment_no) + '.'
						
				CASE '6'
					SELECT	claim_no , txn_amount
					INTO		:ll_claim_no , :ldec_txn_amount
					FROM		UNAPPLIED_CLAIM_TXN
					WHERE	txn_no	= :ll_max_unapplied_txn_no
					USING	SQLCA;
					SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','select data for max txn from UNAPPLIED_CLAIM_TXN')
					
					ls_message = ' (' + String(ldec_txn_amount,'$#,##0.00') + ') of the selected transaction is scheduled to be transferred to '&
						+ 'claim #' + String(ll_claim_no) + '.  See payment #' + String(ll_max_unapplied_payment_no) + '.'
						
				CASE '9'
					SELECT	b.payment_type_code , b.payment_sub_type_code , a.txn_amount
					INTO		:ls_payment_type_code , :ls_payment_sub_type_code , :ldec_txn_amount
					FROM		UNAPPLIED_CLAIM_TXN	a ,
								PAYMENT					b
					WHERE	a.payment_no	= b.payment_no
					AND		a.txn_no			= :ll_max_unapplied_txn_no
					USING	SQLCA;
					SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','select data for max txn from UNAPPLIED_CLAIM_TXN')
					
					IF ls_payment_sub_type_code <> '' THEN
						ls_message = ' (' + String(ldec_txn_amount,'$#,##0.00') + ') of the selected transaction is scheduled to be transferred to '&
							+ 'payment type/sub type ' + ls_payment_type_code +'/'+ ls_payment_sub_type_code  + '.  See payment #' + String(ll_max_unapplied_payment_no) + '.'
					ELSE
						ls_message = ' (' + String(ldec_txn_amount,'$#,##0.00') + ') of the selected transaction was transferred to '&
							+ 'payment type ' + ls_payment_type_code + '.  See payment #' + String(ll_max_unapplied_payment_no) + '.'
					END IF
					
			END CHOOSE
			IF ldec_selected_txn_amount = ldec_txn_amount Then
				ls_message = 'The full amount' + ls_message
			Else
				ls_message = 'A portion' + ls_message
			End if
			
			MessageBox('Transfer', ls_message)
			
		End if
		
	CASE 'FROM'
		//The user wants to know where this transactions was transfered "from"
		lds_transfer_details_applied.DataObject = 'd_transfer_from'
		lds_transfer_details_applied.SetTransObject(SQLCA)
		ll_rows = lds_transfer_details_applied.Retrieve(ll_selected_txn_no)
		IF ll_rows < 0 Then
			SignalError(-666,'Error retrieving transfer details.')
		End if
		SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','retrieve')
		
		lds_transfer_details_unapplied.DataObject = 'd_transfer_from_unapplied'
		lds_transfer_details_unapplied.SetTransObject(SQLCA)
		ll_rows_unapplied = lds_transfer_details_unapplied.Retrieve(ll_selected_txn_no)
		IF ll_rows_unapplied < 0 Then
			SignalError(-666,'Error retrieving unapplied transfer details.')
		End if
		SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','retrieve')
		
		IF ll_rows = 0 and ll_rows_unapplied = 0 Then
			MessageBox('Not transferred','This transaction has not been transferred.')
			
		ELSEIF ll_rows <> 0 AND ll_rows_unapplied = 0 THEN // only applied txns transferred
			//Gather the information and create the message string to pass into 
			//the message window.
			ll_payment_no  			= lds_transfer_details_applied.GetItemNumber(1,'payment_no')
			ll_max_applied_txn_no	= lds_transfer_details_applied.GetItemNumber(1,'max_transfer_txn_no')
			ls_txn_sub_type_code	= lds_transfer_details_applied.GetItemString(1,'txn_sub_type_code')

			CHOOSE CASE ls_txn_sub_type_code
				CASE '7','8'
					ll_cost_alloc_no = lds_transfer_details_applied.GetItemNumber(1,'cost_alloc_no')
					ll_cost_alloc_operation_no = lds_transfer_details_applied.GetItemNumber(1,'cost_alloc_operation_no')
					ls_message =  'The selected transaction was transferred from ' &
						+ 'cost alloc ' + String(ll_cost_alloc_no) +'/'+ String(ll_cost_alloc_operation_no) + '.  See payment #' + String(ll_payment_no) + '.'
						
				CASE '6'
					ll_claim_no = lds_transfer_details_applied.GetItemNumber(1,'claim_no')
					ls_message = 'The selected transaction was transferred from '&
						+ 'claim #' + String(ll_claim_no) + '.  See payment #' + String(ll_payment_no) + '.'
					
				CASE '9'
					ls_payment_type_code = lds_transfer_details_applied.GetItemString(1,'payment_type_code')
					ls_payment_sub_type_code = lds_transfer_details_applied.GetItemString(1,'payment_sub_type_code')
					IF ls_payment_sub_type_code <> '' THEN
						ls_message = 'The selected transaction was transferred from '&
							+ 'payment type/sub type ' + ls_payment_type_code +'/'+ ls_payment_sub_type_code  + '.  See payment #' + String(ll_payment_no) + '.'
					ELSE
						ls_message = 'The selected transaction was transferred from '&
						+ 'payment type ' + ls_payment_type_code + '.  See payment #' + String(ll_payment_no) + '.'
					END IF
					
			END CHOOSE
					
			IF ll_max_applied_txn_no = ll_selected_txn_no THEN
				MessageBox('Transfer',ls_message)
			ELSE
				MessageBox('Not transferred','This transaction has not been transferred.')
			END IF
			
		ELSEIF ll_rows = 0 AND ll_rows_unapplied <> 0 THEN // only unapplied txns transferred
			
			//Gather the information and create the message string to pass into 
			//the message window.
			ll_payment_no  				= lds_transfer_details_unapplied.GetItemNumber(1,'payment_no')
			ll_max_unapplied_txn_no	= lds_transfer_details_unapplied.GetItemNumber(1,'max_transfer_txn_no')
			ls_txn_sub_type_code		= lds_transfer_details_unapplied.GetItemString(1,'txn_sub_type_code')
			
			CHOOSE CASE ls_txn_sub_type_code
				CASE '7','8'
					ll_cost_alloc_no = lds_transfer_details_unapplied.GetItemNumber(1,'cost_alloc_no')
					ll_cost_alloc_operation_no = lds_transfer_details_unapplied.GetItemNumber(1,'cost_alloc_operation_no')
					ls_message =  'The selected transaction is scheduled to be transferred from ' &
						+ 'cost alloc ' + String(ll_cost_alloc_no) +'/'+ String(ll_cost_alloc_operation_no) + '.  See payment #' + String(ll_payment_no) + '.'
						
				CASE '6'
					ll_claim_no = lds_transfer_details_unapplied.GetItemNumber(1,'claim_no')
					ls_message = 'The selected transaction was transferred from '&
						+ 'claim #' + String(ll_claim_no) + '.  See payment #' + String(ll_payment_no) + '.'
					
				CASE '9'
					ls_payment_type_code = lds_transfer_details_unapplied.GetItemString(1,'payment_type_code')
					ls_payment_sub_type_code = lds_transfer_details_unapplied.GetItemString(1,'payment_sub_type_code')
					IF ls_payment_sub_type_code <> '' THEN
						ls_message = 'The selected transaction is scheduled to be transferred from '&
							+ 'payment type/sub type ' + ls_payment_type_code +'/'+ ls_payment_sub_type_code  + '.  See payment #' + String(ll_payment_no) + '.'
					ELSE
						ls_message = 'The selected transaction is scheduled to be transferred from '&
						+ 'payment type ' + ls_payment_type_code + '.  See payment #' + String(ll_payment_no) + '.'
					END IF
					
			END CHOOSE
			
			IF ll_max_unapplied_txn_no	= ll_selected_txn_no THEN
				MessageBox('Transfer',ls_message)
			ELSE
				MessageBox('Not transferred','This transaction has not been transferred.')
			END IF
			
		End if
END CHOOSE
end event

event constructor;call super::constructor;THIS.SetTransObject(SQLCA)

THIS.object.datawindow.hidegrayline = true
end event

event itemchanged;call super::itemchanged;long		ll_no_related_txn
long		ll_checked
long		ll_payment_no
long 		ll_check_box
long 		ll_txn_no
long 		ll_ultimate_txn_no
long		ll_recipient_no
long		ll_found 
long		ll_rowcount
long 		ll_row
long 		ll_net_quantity

string		ls_benefit_type 
string 	ls_script = "dw_transaction_list - itemchanged"
string 	ls_txn_type_code 
string 	ls_txn_sub_type_code 
string		ls_find
string		ls_split_payment_flag

decimal {2}	ldcm_amount
decimal {2}	ldcm_txn_amount
decimal {2}	ldcm_tax_amount
decimal {2}	ldcm_net_days_lost
decimal {2}	ldcm_net_hours_lost
decimal {2}	ldcm_remaining_days_lost
decimal {2}	ldcm_remaining_hours_lost
decimal {2}	ldcm_remaining_ult_txn_amount
decimal {2}	ldcm_remaining_other_ult_txn_amount
decimal {2}	ldcm_entered_amount
decimal {2}	ldcm_other_adj_ult_days
decimal {2}	ldcm_other_adj_ult_hours

decimal {4}	ldcm_portion_to_apply

integer li_rc

boolean	lb_apply_remainder_days_hours


ldcm_amount					= dw_parameter.getitemdecimal(1,'amount')
ls_benefit_type 			= dw_parameter.getitemstring(1,'benefit_type')
		
ll_ultimate_txn_no					= THIS.GetItemNumber(row,'ultimate_txn_no')
ll_payment_no						= THIS.GetItemNumber(row,'payment_no')
ls_split_payment_flag				= THIS.GetItemString(row,'split_payment_flag')

ll_rowcount = this.rowcount()

// reset variable used in cb_apply
ib_oldest_to_newest = FALSE
		
choose case this.GetColumnName()
		
	case 'amt_entered','txn_tax_amt_entered','rehab_qty_entered','days_entered','hours_entered'
		
		ll_txn_no							= this.getitemnumber(row,'txn_no')
		
		select count(*) 
		into :ll_no_related_txn
		from UNAPPLIED_CLAIM_TXN
		where related_txn_no = :ll_txn_no
		using SQLCA; 
		SQLCA.nf_handle_error("select ~ from  UNAPPLIED_CLAIM_TXN",is_window_name,ls_script)
	
		IF ll_no_related_txn > 0 then
			MESSAGEBOX("ERROR","Unprocessed Txn's against this txn prevent this from being maintained. Txn no is "  + string(ll_txn_no) )
			this.setitem(row,'amt_entered',0.00)
			this.setitem(row,'txn_tax_amt_entered',0.00)
			this.setitem(row,'days_entered',0.00)
			this.setitem(row,'hours_entered',0.00)
			this.setitem(row,'rehab_qty_entered',0)
			return 1
		else
				
			if this.getitemnumber(row,'check_box') = 1 then
			else
				this.setitem(row,'check_box',1)
			end if
		end if
		
		
		if this.GetColumnName() = 'amt_entered' then
			ls_txn_type_code 					= dw_parameter.getitemstring(1,'txn_type_code')
			ls_txn_sub_type_code				= dw_parameter.getitemstring(1,'txn_sub_type_code')
			ls_benefit_type 					= dw_parameter.getitemstring(1,'benefit_type')

			ldcm_entered_amount			= Dec(data)
			ll_txn_no							= this.getitemnumber(row,'txn_no')
			ldcm_txn_amount 					= this.getitemdecimal(row,'txn_amount')
			ldcm_tax_amount 					= this.getitemdecimal(row,'txn_tax_amount')
			ll_net_quantity					= this.getitemnumber(row,'net_quantity')
			ll_recipient_no					= this.getitemnumber(row,'recipient_no')
			
			// determine how much has been transferred out of payment thru payment or claim xfer
			wf_calculate_remaining_days_lost(ll_ultimate_txn_no,ldcm_remaining_days_lost)
			wf_calculate_remaining_hours_lost(ll_ultimate_txn_no,ldcm_remaining_hours_lost)
			wf_calculate_remaining_ult_txn_amount(ll_ultimate_txn_no,ldcm_remaining_ult_txn_amount)
			
			IF ls_split_payment_flag = 'Y' THEN
				wf_calc_net_other_ult_txn_amount(ll_payment_no,ll_ultimate_txn_no,ldcm_remaining_other_ult_txn_amount)
				IF ldcm_remaining_other_ult_txn_amount = 0 AND (ldcm_entered_amount + ldcm_txn_amount) = 0 THEN
					// split payment will be fully adjusted wrt $, so remainder of days & hours should also be fully adjusted
					lb_apply_remainder_days_hours = TRUE
					// how much have other txns been adj for days, hours
					wf_calc_net_other_adj_ult_days_lost(ll_payment_no,ll_ultimate_txn_no,ldcm_other_adj_ult_days)
					wf_calc_net_other_adj_ult_hours_lost(ll_payment_no,ll_ultimate_txn_no,ldcm_other_adj_ult_hours)
					ldcm_net_days_lost = dw_transaction_list.getitemdecimal(row,'net_days_lost') + ldcm_other_adj_ult_days
					ldcm_net_hours_lost = dw_transaction_list.getitemdecimal(row,'net_hours_lost') + ldcm_other_adj_ult_hours	
				ELSE
					ldcm_portion_to_apply = ldcm_entered_amount  / ldcm_remaining_ult_txn_amount
				END IF
			ELSE
				IF ldcm_remaining_ult_txn_amount = 0 THEN
					ldcm_portion_to_apply = 1
				ELSEIF (ldcm_entered_amount + ldcm_txn_amount) = 0 THEN
					lb_apply_remainder_days_hours = TRUE
					ldcm_net_days_lost = dw_transaction_list.getitemdecimal(row,'net_days_lost')
					ldcm_net_hours_lost = dw_transaction_list.getitemdecimal(row,'net_hours_lost')
				ELSE
					ldcm_portion_to_apply = ldcm_entered_amount  / ldcm_remaining_ult_txn_amount
				END IF
			END IF
			
			/* DEFAULT THE DAYS AND HOURS LOST */
			IF lb_apply_remainder_days_hours = TRUE THEN
				dw_transaction_list.setitem(row,'days_entered',ldcm_net_days_lost*-1)
				dw_transaction_list.setitem(row,'hours_entered',ldcm_net_hours_lost*-1)
			ELSE
				if ls_benefit_type = 'L' THEN
					dw_transaction_list.setitem(row,'days_entered',0.00)
					dw_transaction_list.setitem(row,'hours_entered',0.00)
				else
					dw_transaction_list.setitem(row,'days_entered',ldcm_portion_to_apply * ldcm_remaining_days_lost)
					dw_transaction_list.setitem(row,'hours_entered',ldcm_portion_to_apply * ldcm_remaining_hours_lost)
				end if
			END IF

			/* FOR OVERPAYMENT RECOVERY WHERE THE PAID BENEFIT IS LEVEL - SET DEFAULTS TO ZERO */
			IF ls_txn_type_code = "J" AND ls_txn_sub_type_code = "3" and ls_benefit_type = 'L' then
				this.setitem(row,'days_entered',0.00)
		 		this.setitem(row,'hours_entered',0.00)
			END IF
			
		end if
		
		
	case 'check_box'
		
		IF ls_txn_type_code = "J" AND ls_txn_sub_type_code = "2" then
			messagebox("WARNING","Can't unselect when cancelling a cheque/deposit")
			return 1 
		end if
		
		ll_checked = 0 
		
		ll_txn_no							= this.getitemnumber(row,'txn_no')
		ll_payment_no						= this.getitemnumber(row,'payment_no')
		
		IF DATA = "1" THEN
	
			this.triggerevent('ue_itemchangeaccepted(row)')
			
			ls_txn_type_code 				= dw_parameter.getitemstring(1,'txn_type_code')
			ls_txn_sub_type_code		= dw_parameter.getitemstring(1,'txn_sub_type_code')
			ls_benefit_type 				= dw_parameter.getitemstring(1,'benefit_type')

			ldcm_txn_amount 				= this.getitemdecimal(row,'txn_amount')
			ldcm_tax_amount 				= this.getitemdecimal(row,'txn_tax_amount')
			ll_net_quantity					= this.getitemnumber(row,'net_quantity')
			ll_recipient_no					= this.getitemnumber(row,'recipient_no')
			
			// determine how much has been transferred out of payment thru payment or claim xfer
			wf_calculate_remaining_days_lost(ll_ultimate_txn_no,ldcm_remaining_days_lost)
			wf_calculate_remaining_hours_lost(ll_ultimate_txn_no,ldcm_remaining_hours_lost)
			wf_calculate_remaining_ult_txn_amount(ll_ultimate_txn_no,ldcm_remaining_ult_txn_amount)
			
			ldcm_entered_amount = ldcm_txn_amount * -1
			
			IF ls_split_payment_flag = 'Y' THEN
				wf_calc_net_other_ult_txn_amount(ll_payment_no,ll_ultimate_txn_no,ldcm_remaining_other_ult_txn_amount)
				IF ldcm_remaining_other_ult_txn_amount = 0 AND (ldcm_entered_amount + ldcm_txn_amount) = 0 THEN
					// split payment will be fully adjusted wrt $, so remainder of days & hours should also be fully adjusted
					lb_apply_remainder_days_hours = TRUE
					// how much have other txns been adj for days, hours
					wf_calc_net_other_adj_ult_days_lost(ll_payment_no,ll_ultimate_txn_no,ldcm_other_adj_ult_days)
					wf_calc_net_other_adj_ult_hours_lost(ll_payment_no,ll_ultimate_txn_no,ldcm_other_adj_ult_hours)
					ldcm_net_days_lost = dw_transaction_list.getitemdecimal(row,'net_days_lost') + ldcm_other_adj_ult_days
					ldcm_net_hours_lost = dw_transaction_list.getitemdecimal(row,'net_hours_lost') + ldcm_other_adj_ult_hours	
				ELSE
					ldcm_portion_to_apply = ldcm_entered_amount  / ldcm_remaining_ult_txn_amount
				END IF
			ELSE
				IF ldcm_remaining_ult_txn_amount = 0 THEN
					ldcm_portion_to_apply = 1
				ELSEIF (ldcm_entered_amount + ldcm_txn_amount) = 0 THEN
					lb_apply_remainder_days_hours = TRUE
					ldcm_net_days_lost = dw_transaction_list.getitemdecimal(row,'net_days_lost')
					ldcm_net_hours_lost = dw_transaction_list.getitemdecimal(row,'net_hours_lost')
				ELSE
					ldcm_portion_to_apply = ldcm_entered_amount  / ldcm_remaining_ult_txn_amount
				END IF
			END IF
					
			
			// Functionality 5.20 in TP
			IF ls_txn_type_code = "J" AND ls_txn_sub_type_code = "3" then
		
				ls_find = "recipient_no = " + string(ll_recipient_no)  + ' AND  check_box = 1 '			
				ll_found =  this.Find(ls_find, 1,  ll_rowcount)
				
				if ll_found > 0 then
				else
					ls_find =  "recipient_no <> " + string(ll_recipient_no) + " and check_box = 1" 
					ll_found =  this.Find(ls_find, 1,  ll_rowcount)
				
					if ll_found > 0 then
						messagebox("error","Overpayment Recovery is limited to one recipient at one time")
						dw_transaction_list.setitem(ll_found,'check_box',0)
						return 1						
					end if
				end if
			end if
			
			
			// Functionality 1.60 in TP
			select count(*) 
			into :ll_no_related_txn
			from UNAPPLIED_CLAIM_TXN
			where related_txn_no = :ll_txn_no
			using SQLCA; 
			SQLCA.nf_handle_error("select ~ from  UNAPPLIED_CLAIM_TXN",is_window_name,ls_script)
		
			IF ll_no_related_txn > 0 then
				MESSAGEBOX("ERROR","Unprocessed Txn's against this txn prevent this from being maintained. Txn no is "  + string(ll_txn_no) )
				dw_transaction_list.setitem(row,'check_box',0)
				return 1
			else
			
			
			if ldcm_amount	<> 0 then
				for ll_row = 1 to ll_rowcount
					ll_check_box = this.getitemnumber(ll_row,'check_box')
					IF ll_row = row then
					else
						if ll_check_box = 1 then
							ll_checked++
							exit
						end if
					end if
				next
			end if
			
			
			if ls_txn_type_code = "J" AND ls_txn_sub_type_code = "4"  then			
				 this.setitem(row,'amt_entered',0.00)				 
			elseif ls_txn_type_code = "J" AND ls_txn_sub_type_code = "Q"  then
				 this.setitem(row,'txn_tax_amt_entered',0.00)	
			else
				this.setitem(row,'amt_entered',ldcm_txn_amount * -1)
				this.setitem(row,'txn_tax_amt_entered',ldcm_tax_amount * -1)
				
				this.setitem(row,'rehab_qty_entered',ll_net_quantity * -1)
				/* DEFAULT THE DAYS AND HOURS LOST */
				IF lb_apply_remainder_days_hours = TRUE THEN
					dw_transaction_list.setitem(row,'days_entered',ldcm_net_days_lost*-1)
					dw_transaction_list.setitem(row,'hours_entered',ldcm_net_hours_lost*-1)
				ELSE
					if ls_benefit_type = 'L' THEN
						dw_transaction_list.setitem(row,'days_entered',0.00)
						dw_transaction_list.setitem(row,'hours_entered',0.00)
					else
						dw_transaction_list.setitem(row,'days_entered',ldcm_portion_to_apply * ldcm_remaining_days_lost)
						dw_transaction_list.setitem(row,'hours_entered',ldcm_portion_to_apply * ldcm_remaining_hours_lost)
					end if
				END IF
					
			end if
		end if
		

	else
		// unchecking box
		this.setitem(row,"check_box",0)	
	 	this.setitem(row,'amt_entered',0.00)
	 	this.setitem(row,'txn_tax_amt_entered',0.00)
	 	this.setitem(row,'days_entered',0.00)
	 	this.setitem(row,'hours_entered',0.00)
	 	this.setitem(row,'rehab_qty_entered',0)
	
		for ll_row = 1 to ll_rowcount
			ll_check_box = this.getitemnumber(ll_row,'check_box')
			
			if ll_check_box = 1 and ll_row <> row then
				ll_checked++						 
			end if
		next
	
		if ll_checked = 0 then
			cbx_1.checked = false	
		end if
		
	END IF	
		
end choose


end event

event rbuttondown;

/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
	If not isvalid(im_popup) Then

		im_popup = Create m_dw_online_rmb_popup
		im_popup.mf_set_datawindow(This)
		im_popup.m_options.m_filterlist.visible = TRUE	
//		im_popup.m_options.m_sort.visible = TRUE	
		im_popup.m_options.m_moredetails.visible = TRUE	
		im_popup.m_options.m_transferdetails.visible = TRUE
	//	im_popup.m_options.m_tooltips.visible = TRUE	
	End if

	im_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))
end event

event ue_filter;string ls_filter
string ls_old_filter


if ib_filter_applied then
//
//ls_old_filter = dw_transaction_list.Describe("DataWindow.Table.Filter") 
//
//if ls_old_filter = "?" THEN
//ELSE
	messagebox("Warning","Filter already applied on search" + ls_old_filter)
	return -1
END IF



dw_transaction_list.setfilter("")
dw_transaction_list.filter()

Open(w_filter_claim_cost_transaction_list)

//	Apply the filter that was selected and recalculate total days
	ls_filter = Message.StringParm
	IF ls_filter = "Cancel" THEN
		Return
	END IF


dw_transaction_list.setfilter(ls_filter)
dw_transaction_list.filter()


if dw_transaction_list.RowCount() > 0 then
	dw_transaction_list.GroupCalc()
end if
end event

event ue_more_details;call super::ue_more_details;if this.getrow() > 0 then	
	/* open the inquiry screen */
	openwithparm(w_claim_cost_maintenance_inquiry,this.getitemnumber(this.getrow(),'txn_no'))
end if




end event

event ue_print;THIS.Modify("DataWindow.Print.Orientation= '1' ") // landscape
THIS.print()
end event

