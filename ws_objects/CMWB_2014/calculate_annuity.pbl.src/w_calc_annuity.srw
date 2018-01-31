$PBExportHeader$w_calc_annuity.srw
$PBExportComments$Used to maintain the benefit entitlement - inherited from w_checklist
forward
global type w_calc_annuity from w_ancestor
end type
type st_annuity_payout_no from statictext within w_calc_annuity
end type
type st_annuity_payout_label from statictext within w_calc_annuity
end type
type dw_calc_header_two from u_dw_online within w_calc_annuity
end type
type st_multiple_accounts from statictext within w_calc_annuity
end type
type st_warning from statictext within w_calc_annuity
end type
type dw_account_dates from u_dw_online within w_calc_annuity
end type
type dw_worker_details from u_dw_online within w_calc_annuity
end type
type dw_injured_worker from u_dw_online within w_calc_annuity
end type
type pb_1 from picturebutton within w_calc_annuity
end type
type pb_slide_tree from picturebutton within w_calc_annuity
end type
type st_vert_splitbar_tv_left from u_splitbar_vertical within w_calc_annuity
end type
type dw_benefit_entitlement from u_dw_online within w_calc_annuity
end type
type dw_information from u_dw_online within w_calc_annuity
end type
type tv_manager from treeview within w_calc_annuity
end type
type dw_annuity_calc_header from u_dw_online within w_calc_annuity
end type
type st_vert_splitbar_tv_right from u_splitbar_vertical within w_calc_annuity
end type
end forward

global type w_calc_annuity from w_ancestor
integer width = 6240
integer height = 2952
string title = "Calculate Annuity"
string menuname = "m_annuity"
windowtype windowtype = main!
long backcolor = 67108864
boolean clientedge = true
long il_design_time_height = 2512
long il_design_time_width = 4718
event ue_post_open ( )
st_annuity_payout_no st_annuity_payout_no
st_annuity_payout_label st_annuity_payout_label
dw_calc_header_two dw_calc_header_two
st_multiple_accounts st_multiple_accounts
st_warning st_warning
dw_account_dates dw_account_dates
dw_worker_details dw_worker_details
dw_injured_worker dw_injured_worker
pb_1 pb_1
pb_slide_tree pb_slide_tree
st_vert_splitbar_tv_left st_vert_splitbar_tv_left
dw_benefit_entitlement dw_benefit_entitlement
dw_information dw_information
tv_manager tv_manager
dw_annuity_calc_header dw_annuity_calc_header
st_vert_splitbar_tv_right st_vert_splitbar_tv_right
end type
global w_calc_annuity w_calc_annuity

type variables
LONG					il_individual_no, il_claim_no, il_annuity_payout_no, il_annuity_account_no, il_annuity_eligibility_no, il_calc_no
LONG                   il_open_mode
STRING				is_claim_role_code
ULONG				iul_handle
BOOLEAN			ib_create_annuity_calc_records	
DATETIME			idtm_annuity_start_date, idtm_annuity_end_date, idtm_annuity_eligibility_end_date_used
DECIMAL				idec_annuity_set_aside_percent

U_DS					ids_account_info_for_treeview, ids_annuity_payout
u_dw_online 		idw_benefit_entitlement
n_resize 				inv_tab_resize
windowstate		iws_frame_open_state

Protected:
n_transaction	inv_transobj
w_ancestor				iwi_window_parent

n_common_annuity 	inv_common_annuity



	



end variables

forward prototypes
public subroutine wf_pop_info_from_account_info (long al_annuity_account_no)
public function long wf_get_individual_no ()
public function string wf_get_claim_role_code ()
public function integer wf_populate_calculated_totals (long al_annuity_account_no)
public function integer wf_populate_treeview ()
public function integer wf_populate_account_ds ()
public function integer wf_treeview_clicked (long al_handle)
public function integer wf_get_max_annuity_calc_record (long al_annuity_account_no)
public function boolean wf_check_calc_reason_valid (string as_calc_reason)
public function integer wf_retrieve_claim_totals (long al_annuity_account_no, long al_claim_no)
public function integer wf_retrieve_adjustments_by_claim (long al_annuity_account_no, long al_claim_no)
public function integer wf_update_controller (long al_annuity_account_no)
public function boolean wf_check_calculate_needed (long al_eligibility_no, long al_annuity_account_no)
public function long wf_basic_count_check (long al_annuity_eligibility_no, long al_annuity_account_no)
public function integer wf_populate_worker_info (long al_individual_no, string as_claim_no)
public function integer wf_populate_worker_details (long al_individual_no)
public function integer wf_retrieve_account_totals (long al_annuity_account_no, long al_annuity_calc_no)
public function integer wf_refresh_entitlement (integer ai_type)
public subroutine wf_scroll_to_row_in_dw_child (integer ai_row)
public subroutine wf_populate_benefit_totals (long al_annuity_account_no, long al_calc_no, long al_claim_no, integer ai_mode)
public subroutine wf_set_be_filter (long al_claim_no)
public subroutine wf_report ()
public subroutine wf_display_annuity_payout (boolean ab_display, long al_annuity_payout_no)
end prototypes

event ue_post_open();BOOLEAN           lb_annuity_payout_visible
INTEGER				li_rtn
LONG			 		ll_tvi, ll_tvi_next_item, ll_calc_no, ll_currow, ll_eligibility_no, ll_rows, ll_annuity_payout_no
Treeviewitem 		ltvi
datawindowchild	ldwc_date_list, ldwc_child


/*Refresh the screen once all of the opening rendering is done*/
THIS.POST wf_refresh_entitlement(1)

/* 
In general, when the module is opened the information for the latest calculation will be displayed 
and saved or if nothing has changed (i.e. annuity eligibility has not been confirmed) since the calculation 
was created the existing calculation will be opened.

The benefit entitlement data is displayed on the left side of the screen and is entirely read-only. 
The rest of the data will be programmatically completed.  The user will be prompted for the calculation reason 
when the module is opened.
*/
IF il_open_mode = 2  THEN 
	IF ib_create_annuity_calc_records = TRUE THEN// open mode refers to calling object
	
		IF  wf_update_controller(il_annuity_account_no) < 0 THEN
			close(this)
			RETURN 
		END IF 	
	END IF 
END IF 

/* populates an annuity account array this will be used as the top level of our treeview*/
wf_populate_account_ds()

// retrieve the dropdown with the dates in it.
dw_account_dates.GetChild("calc_dates",ldwc_date_list)
ldwc_date_list.SetTransObject(SQLCA)

ldwc_date_list.Retrieve(il_annuity_account_no)
sqlca.nf_handle_error("w_calc_annuity","ue_post_open","ldwc_date_list.Retrieve(il_annuity_account_no)")

/*	Insert a row */
dw_account_dates.InsertRow(0)

IF il_open_mode = 1  THEN //read only report
	/* scroll to the first row */
	wf_scroll_to_row_in_dw_child(1)
END IF 

/* populate the treeview */
li_rtn = wf_populate_treeview()
IF li_rtn = -1 THEN
	st_warning.Visible = TRUE
ELSEIF li_rtn = -2 THEN
	MessageBox('Error','There was an error populating the treeview. Please call the HELPDESK.',Exclamation!)
END IF

/* now select the first item in the treeview and click it inorder to view */
ll_tvi = tv_manager.FindItem(RootTreeItem!, 0)

IF ll_tvi > 0 THEN 

	ll_tvi_next_item = tv_manager.FindItem(ChildTreeItem!,ll_tvi)

	/* trigger the retrieve through a function */
	wf_treeview_clicked(ll_tvi_next_item)
	
	/* highlight the first treeview record will not need to be set after this taken care 
	    of in the selectionchanged event */
	tv_manager.setdrophighlight(ll_tvi_next_item)
	
END IF 

idw_benefit_entitlement.uf_settooltip(TRUE)

// grab the child with all of the info we need
dw_account_dates.GetChild('calc_dates',ldwc_child)

//make sure we have a valid row
ll_currow = ldwc_child.getrow()

IF ll_currow > 0 THEN 
	
	//Grab the data we need to populate the treeview
	ll_calc_no = ldwc_child.getitemnumber(ll_currow, 'annuity_calc_no')
	
	dw_annuity_calc_header.retrieve(ll_calc_no)
	sqlca.nf_handle_error("w_calc_annuity","ue_post_open","dw_annuity_calc_header.retrieve(il_annuity_account_no)")
	
	ll_eligibility_no = ldwc_child.getitemnumber(ll_currow, 'annuity_eligibility_no')
	
ELSE 
	ll_eligibility_no = il_annuity_eligibility_no
END IF 

dw_calc_header_two.retrieve(ll_eligibility_no)
sqlca.nf_handle_error("w_calc_annuity","ue_post_open","dw_calc_header_two.retrieve(il_eligibility_no)")

IF il_annuity_payout_no > 0 THEN	
	// populate ANNUITY_PAYOUT.annuity_calc_no
	ids_annuity_payout = Create U_DS
	ids_annuity_payout.DataObject = 'ds_annuity_payout'
	ids_annuity_payout.SetTransObject(SQLCA)
	ll_rows = ids_annuity_payout.Retrieve(il_annuity_payout_no)
	SQLCA.nf_handle_error('w_calc_annuity','ids_annuity_payout','retrieve')
	IF ll_rows <> 1 THEN
		// should only retrieve one row
		MessageBox('Payout Error','Error - Should be a single annuity payout record retrieved. Actually retrieved '+String(ll_rows)+'. Call HELPDESK.',Exclamation!)
		RETURN		
	END IF
	
	ids_annuity_payout.SetItem(1,'annuity_calc_no',ll_calc_no)
	
	SQLCA.nf_begin_transaction()

	ids_annuity_payout.Update()

	SQLCA.nf_commit_transaction()

END IF

IF ib_create_annuity_calc_records = FALSE THEN
	// retrieving existing calculation records
	ll_annuity_payout_no = ldwc_child.getitemnumber(ll_currow, 'annuity_payout_no')
	IF ll_annuity_payout_no > 0 THEN
		lb_annuity_payout_visible = TRUE
	ELSE
		lb_annuity_payout_visible = FALSE
	END IF
ELSE
	// creating calculation records
	ll_annuity_payout_no = il_annuity_payout_no	
	IF il_annuity_payout_no > 0 THEN
		lb_annuity_payout_visible = TRUE
	ELSE
		lb_annuity_payout_visible = FALSE
	END IF
END IF
wf_display_annuity_payout(lb_annuity_payout_visible,ll_annuity_payout_no)
end event

public subroutine wf_pop_info_from_account_info (long al_annuity_account_no);/*Grabs the inforation from the annuity_account based on the annuity_account_no */

SELECT 	individual_no, claim_no, claim_role_code  
INTO 		:il_individual_no, :il_claim_no, :is_claim_role_code
FROM 	ANNUITY_ACCOUNT b
WHERE  	annuity_account_no  = :al_annuity_account_no
USING 	SQLCA;

SQLCA.nf_handle_error('w_calc_annuity','wf_pop_info_from_account_info()','SELECT claim_no INTO :ll_claim_no')


end subroutine

public function long wf_get_individual_no ();RETURN il_individual_no
end function

public function string wf_get_claim_role_code ();RETURN is_claim_role_code
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
SELECT annuity_start_date,
       annuity_end_date
INTO   :ldtm_annuity_start,
       :ldtm_annuity_end
FROM   ANNUITY_ELIGIBILITY
WHERE  annuity_eligibility_no = :il_annuity_eligibility_no
USING SQLCA;
SQLCA.nf_handle_error('w_calc_annuity','embedded SQL: SELECT annuity_start_date,annuity_end_date FROM ANNUITY_ELIGIBILITY...','wf_populate_calculated_totals')


//NOTHING CAN BE DONE
IF ISNULL(ldtm_annuity_start) OR ISNULL(ldtm_annuity_end) THEN RETURN -1

//initialize the variables

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
			
NEXT

/* POPULATE THE TEXT COLUMNS IN THE DATAWINDOW */
//ENTITLEMENT
idw_benefit_entitlement.Object.t_ben_hours.Text 		= STRING(ldc_Entitlement_hours)
idw_benefit_entitlement.Object.t_ben_days.Text 		= STRING(ldc_Entitlement_Days)
idw_benefit_entitlement.Object.t_ben_weeks.Text 	= STRING(ldc_Entitlement_Weeks)
idw_benefit_entitlement.Object.t_ben_months.Text 	= STRING(ldc_Entitlement_Months)
idw_benefit_entitlement.Object.t_ben_amount.Text 	= STRING(ldc_Entitlement_sum,'$#,##0.00;($#,##0.00)')


end function

public function integer wf_populate_treeview ();LONG  			ll_lev1, ll_lev2, ll_lev3, ll_lev4, ll_lev5, ll_tvi
LONG				ll_annuity_account_no, ll_previous_account_no, ll_claim_no
LONG				ll_annuity_account_no_ck, ll_treeview_data[]
STRING 			ls_label
INTEGER			li_counter, li_datastore_rowcount, li_claim_counter
Treeviewitem 	ltvi

/* Data will be the combination of account_no and claim_no in the following format
	annuity_ccount_no[1] claim_no[2] in a LONG array claim_no is not required for that level 
	the value will be 0
*/

tv_manager.PictureHeight 	= 32
tv_manager.PictureWidth 	= 32

// grab the rowcount
li_datastore_rowcount =  ids_account_info_for_treeview.rowcount()

IF li_datastore_rowcount = 0 THEN RETURN -1

IF isnull(li_datastore_rowcount) OR li_datastore_rowcount < 0 THEN RETURN -2

FOR li_counter = 1 TO li_datastore_rowcount
	
	//grab the claim  + account numbers
	ll_annuity_account_no 	= ids_account_info_for_treeview.getitemnumber(li_counter, 'annuity_account_no')
				
	IF ll_annuity_account_no <> ll_previous_account_no THEN
		
		/* top level */
		ls_label = 'Account # ' + STRING(il_annuity_account_no)
	
		ll_treeview_data[1] = il_annuity_account_no
		ll_treeview_data[2] = 0
		
		ltvi.label 						= ls_label 
		ltvi.data 						= ll_treeview_data
		ltvi.children 					= TRUE
		
		ll_lev1 = tv_manager.InsertItemlast(0, ltvi)
		
		/* put level in for account totals  */
		ltvi.label 		=  "Account Totals"
		
		ltvi.data 		= ll_treeview_data
		ll_lev2 		= tv_manager.InsertItemLast(ll_lev1,ltvi)
		
		FOR li_claim_counter = 1 TO li_datastore_rowcount
			
			ll_annuity_account_no_ck 	= ids_account_info_for_treeview.getitemnumber(li_claim_counter, 'annuity_account_no')
			ll_claim_no 						= ids_account_info_for_treeview.getitemnumber(li_claim_counter, 'claim_no')

			IF ll_annuity_account_no_ck = ll_annuity_account_no THEN 
				
					/* populate the data will need to do the retrieves */
					ll_treeview_data[1] 	= 	ll_annuity_account_no_ck
					ll_treeview_data[2] 	=	ll_claim_no
				
					/* next level  - claim_no */
					ltvi.label 			= STRING(ll_claim_no)
					
					
					ltvi.data 			= ll_treeview_data
					ll_lev3 			= tv_manager.InsertItemLast(ll_lev2, ltvi)
				
					/* next level */
					ltvi.label 			= "Totals"
					
					ltvi.data 			= ll_treeview_data
					ltvi.children 		= FALSE
					ll_lev4 			= tv_manager.InsertItemLast(ll_lev3, ltvi)
				
					/* next level */
					ltvi.label 			= "Adjustments"
					
					ltvi.data 			= ll_treeview_data
					ltvi.children 		= FALSE
					ll_lev5 			= tv_manager.InsertItemLast(ll_lev3, ltvi)
					
			END IF
		NEXT
	END IF
				
	ll_previous_account_no = ll_annuity_account_no
				
NEXT

/* expand all the items to start - may want to expand the top item
    but for now this will do 
*/
ll_tvi = tv_manager.FindItem(RootTreeItem! , 0)

tv_manager.ExpandAll(ll_tvi)


RETURN 1

end function

public function integer wf_populate_account_ds ();/* this array will be used to populate all of the associated annuity accounts 
    these will be used as the top level of the treeview
*/
INTEGER li_row

ids_account_info_for_treeview 					= CREATE u_ds
ids_account_info_for_treeview.dataobject 	= 'd_account_info_for_treeview'
ids_account_info_for_treeview.settransobject(sqlca)

li_row = ids_account_info_for_treeview.Retrieve(il_annuity_account_no)
SQLCA.nf_handle_error("w_calc_annuity","wf_populate_account_ds()","ids_account_info_for_treeview.Retrieve(il_annuity_account_no)")

IF ISNULL(li_row) OR  li_row < 0 THEN RETURN -1

RETURN 1
end function

public function integer wf_treeview_clicked (long al_handle);treeviewitem 				ltvi
INTEGER						li_level
STRING						ls_label
LONG				 			ll_treeview_array[], ll_max_annuity_calc_no, ll_currow
DATAWINDOWCHILD		ldwc_child
					

// grab the child with all of the info we need
dw_account_dates.GetChild('calc_dates',ldwc_child)

//make sure we have a valid row
ll_currow = ldwc_child.getrow()

IF isnull(ll_currow) OR ll_currow < 1 THEN RETURN -1

//Grab the data we need to populate the treeview
il_calc_no = ldwc_child.getitemnumber(ll_currow, 'annuity_calc_no')

// check for a valid value
IF ISNULL(il_calc_no) OR il_calc_no < 0 THEN RETURN -1

// Get current fax selected if it's not the right level then get out
tv_manager.GetItem(al_handle, ltvi)

/* we need to grab inforamtion from the treeview item in order to populate the information pane */
ls_label 		= ltvi.label
li_level 		= ltvi.level

/* need to get the data out of the treeview data value will be in the form of a long array 
	account_no [1]
	claim_no[2]
*/
ll_treeview_array = ltvi.data

//reset information
dw_information.reset()
dw_information.dataobject = ''

/* with our data values we will populate the information datawindow */
CHOOSE CASE  ltvi.level
	CASE 1 //Annuity_Account
		
		//refresh the entitlement screen back to nothing
		wf_refresh_entitlement(3)
		
	CASE 2 //Account Totals
		
		//do the retrieve
		IF wf_retrieve_account_totals(ll_treeview_array[1], il_calc_no) < 0 THEN RETURN -1
		
		//refresh the entitlement datawindow
		wf_refresh_entitlement(1)
						
	CASE 3 //Claim list
		
		//refresh the entitlement datawindow
		wf_refresh_entitlement(3)
		
	CASE 4 //Claim TOTALS & Adjustments
		
		CHOOSE CASE ls_label
			CASE 'Adjustments'
				
					IF wf_retrieve_adjustments_by_claim(ll_treeview_array[1],ll_treeview_array[2]) < 0 THEN RETURN -1
					
			CASE 'Totals'
					IF wf_retrieve_claim_totals(ll_treeview_array[1],ll_treeview_array[2]) < 0 THEN RETURN -1
				
		END CHOOSE		
		
		//refresh the entitlement datawindow
		il_claim_no = ll_treeview_array[2]
		wf_refresh_entitlement(2)
					
END CHOOSE

RETURN 1
	
end function

public function integer wf_get_max_annuity_calc_record (long al_annuity_account_no);/* In general, when the module is opened the information for the latest calculation will be displayed and 
    saved or if nothing has changed (i.e. annuity eligibility has not been confirmed) since the calculation was 
	created the existing calculation will be opened.
*/
LONG				ll_annuity_calc_no

SELECT 	max(annuity_calc_no) 
INTO		:ll_annuity_calc_no
FROM    	ANNUITY_CALC_ACCOUNT_HEADER
WHERE   annuity_account_no = :al_annuity_account_no 
USING	SQLCA;

SQLCA.nf_handle_error("w_calc_annuity","wf_get_max_annuity_calc_record()","SELECT max(annuity_calc_no)")

IF ISNULL(ll_annuity_calc_no) THEN  ll_annuity_calc_no = 0

RETURN ll_annuity_calc_no


end function

public function boolean wf_check_calc_reason_valid (string as_calc_reason);INTEGER li_count

SELECT 	count(*) 
INTO     :li_count
FROM 	Annuity_Calc_Reason
WHERE 	annuity_calc_reason_code = :as_calc_reason
USING	SQLCA;

sqlca.nf_handle_error("w_calc_annuity","wf_check_calc_reason_valid()","SELECT count(*)")

IF ISNULL(li_count) OR li_count <= 0  THEN RETURN FALSE

RETURN TRUE


end function

public function integer wf_retrieve_claim_totals (long al_annuity_account_no, long al_claim_no);
/* double check the parameters */
IF ISNULL(al_annuity_account_no) OR al_annuity_account_no 	<= 0 THEN RETURN -1
IF ISNULL(al_claim_no)                	OR al_claim_no 				<= 0 THEN RETURN -1

/* set the dataobject */
dw_information.dataobject = 'd_annuity_calc_claim_detail_return'
dw_information.settransobject(sqlca)
					
/* do the retrieve based on the claim number abd the last calc # */					
dw_information.retrieve(al_annuity_account_no, il_calc_no, al_claim_no, is_claim_role_code)
SQLCA.nf_handle_error('w_calc_annuity','wf_retrieve_claim_totals()','dw_information.retrieve()')
				
RETURN 1
					
end function

public function integer wf_retrieve_adjustments_by_claim (long al_annuity_account_no, long al_claim_no);/* double check the parameters */
IF ISNULL(al_annuity_account_no)  	OR al_annuity_account_no 	<= 0 THEN RETURN -1
IF ISNULL(al_claim_no)                 	OR al_claim_no 				<= 0 THEN RETURN -1

/* set the dataobject */
dw_information.dataobject = 'd_sub_ledger_adjustments_by_claim'
dw_information.settransobject(sqlca)
					
/* do the retrieve based on the claim number and the last calc # */					
dw_information.retrieve(al_annuity_account_no, il_calc_no, al_claim_no, is_claim_role_code)
SQLCA.nf_handle_error('w_calc_annuity','wf_retrieve_adjustments_by_claim()','dw_information.retrieve()')
					
RETURN 1
									
					

					
end function

public function integer wf_update_controller (long al_annuity_account_no);/* controls all of the functions used in the creation of the calculations for the annuity account

	Call Stored procedure
	
	p_calculate_account_balance 
	
	 @annuity_account_no       int,
     @annuity_calc_reason_code CHAR(2)
	
	Based on the following tables:

ANNUITY_CALC_BENEFIT_ENTITLEMENT_XREF
ANNUITY_CALC_ACCOUNT_HEADER
ANNUITY_CALC_ACCOUNT_DETAIL
ANNUITY_CALC_CLAIM_HEADER
ANNUITY_CALC_CLAIM_DETAIL
ANNUITY_CALC_SET_ASIDE_ADJUSTMENT
ANNUITY_CALC_INTEREST_ADJUSTMENT

*/
STRING 					ls_calculation_reason, ls_sql
LONG						ll_tvi, ll_tvi_next_item, ll_payout_no
INTEGER      			li_count
s_calc_reason_data 	lstr_calc_reason_data

IF isnull(il_annuity_eligibility_no) OR il_annuity_eligibility_no <= 0 THEN RETURN -1
IF isnull(al_annuity_account_no) OR al_annuity_account_no <= 0 THEN RETURN -1

/* popup the response so the user can select the calculation reason this will be passed in 
    The user will be prompted for the calculation reason when the module is opened.
*/

// Open the response window
lstr_calc_reason_data.individual_no                     = il_individual_no
lstr_calc_reason_data.claim_no                          = il_claim_no
lstr_calc_reason_data.claim_role_code                   = is_claim_role_code
lstr_calc_reason_data.annuity_eligibility_no            = il_annuity_eligibility_no
lstr_calc_reason_data.annuity_payout_no                 = il_annuity_payout_no
lstr_calc_reason_data.annuity_end_date                  = idtm_annuity_end_date
OpenWithParm(w_select_calc_reason_response,lstr_calc_reason_data)

// Check text returned in Message object
ls_calculation_reason = Message.StringParm

//make sure the returned value is a valid code
IF wf_check_calc_reason_valid(ls_calculation_reason) = FALSE THEN RETURN -1 

/* set the pointer to an hourglass as in some cases it may take a bit of time */
SetPointer(HourGlass!)


DECLARE lp_calculate_annuity_CONTROLLER PROCEDURE FOR p_calculate_annuity_CONTROLLER
	@annuity_account_no           		= :al_annuity_account_no,
	@annuity_calc_reason_code  	   	= :ls_calculation_reason,
	@annuity_eligibility_no         		= :il_annuity_eligibility_no,
	@annuity_payout_no            		= :il_annuity_payout_no,
	@annuity_start_date              	= :idtm_annuity_start_date,
	@annuity_eligibility_end_date_used  = :idtm_annuity_eligibility_end_date_used,
	@annuity_end_date                	= :idtm_annuity_end_date,
	@annuity_set_aside_percent       	= :idec_annuity_set_aside_percent,
	@mode                            	= 'insert'
USING SQLCA;
SQLCA.nf_handle_error("w_calc_annuity","wf_update_controller()","DECLARE lp_calculate_annuity_CONTROLLER")

EXECUTE lp_calculate_annuity_CONTROLLER;
SQLCA.nf_handle_error('w_calc_annuity','wf_update_controller()','EXECUTE lp_calculate_annuity_CONTROLLER;')


RETURN 1
end function

public function boolean wf_check_calculate_needed (long al_eligibility_no, long al_annuity_account_no);/*
Opening the Module

When the module is opened the user will be prompted for the calculation reason.  
If the module is opened to calculate for payout (a payout number greater than zero is passed in) 
and the last quarter’s interest rate is not available then the module will not open. 

If the module is opened for the same annuity eligibility as the last calculation, then no need to create a new calculation, 
the module can open with the latest calculation. Otherwise if the annuity eligibility is new, (not existing on a calculation) 
create a new calculation and save the data.

Besides the information from the prompts above, the module will need to know the annuity account number,
the eligibility number of the last confirmed eligibility and a payout number of either zero if not for payout or a valid payout number.

All the calculations will be made and saved during the open of the module, if a new calculation is required.  
All the saved data will then be displayed for the user to view.  If necessary, the user can select previous calculations 
from a list and view the previous calculations.(For second release of module – access to history not defined yet)

This module may be opened even if the individual does not qualify or no longer qualifies for annuity benefits.  
There will be no data to summarize by quarter, but there may be adjustments that need to be calculated.

The ‘calculated’ data needs to be displayed in three ways: 
·	Annuity benefits by annuity account
·	Annuity benefits by claim
·	Annuity sub-ledger adjustments by claim

*/
LONG				ll_check

SELECT 	count(*)
INTO		:ll_check
FROM 	ANNUITY_CALC_ACCOUNT_HEADER
WHERE 	annuity_account_no 	= :al_annuity_account_no
AND       annuity_eligibility_no 	= :al_eligibility_no
USING 	SQLCA;

SQLCA.nf_handle_error('w_calc_annuity','wf_check_calculate_needed()','SELECT count(*)') 

IF ll_check > 0 THEN  RETURN FALSE

RETURN TRUE










end function

public function long wf_basic_count_check (long al_annuity_eligibility_no, long al_annuity_account_no);/* give us the number of records that may be involved in the Stored procedure 
    if none we do not want to proceed with the SP execution
*/
INTEGER		li_count

select   	count(*)
into    	:li_count
from   	ANNUITY_ELIGIBILITY a  
join    	BENEFIT_ENTITLEMENT  b on   a.annuity_account_no = b.annuity_account_no
                                   and ( b.benefit_entitlement_from_date >= a.annuity_start_date 
                                   and   b.benefit_entitlement_to_date <= a.annuity_end_date )
join     	ANNUITY_ACCOUNT d on b.annuity_account_no = d.annuity_account_no
where    	a.annuity_eligibility_status_code		= 'A'
and      	a.annuity_eligibility_no               		= :al_annuity_eligibility_no
and     	b.annuity_account_no                 		= :al_annuity_account_no
and      	b.deleted_flag                            		= 'N'
using      sqlca;

sqlca.nf_handle_error("w_calc_annuity","wf_basic_count_check()","select  count(*)")


IF isnull(li_count) THEN li_count = 0

RETURN li_count
end function

public function integer wf_populate_worker_info (long al_individual_no, string as_claim_no);/* this function simply fills in the static text boxes at the top of the screen  with the 
    individual_no and the injured workers name
*/
dw_injured_worker.retrieve(al_individual_no, as_claim_no)
SQLCA.nf_handle_error("w_calc_annuity","wf_populate_worker_info()","dw_injured_worker.retrieve(al_individual_no)")

RETURN 1
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

public function integer wf_retrieve_account_totals (long al_annuity_account_no, long al_annuity_calc_no);LONG				ll_last_annuity_calc_no
INTEGER			li_rowcount

/* double check the parameters */
IF ISNULL(al_annuity_account_no) OR al_annuity_account_no <= 0 THEN RETURN -1

// if a annuity_calc_no is passed in here use it - comes from another window
IF al_annuity_calc_no > 0 THEN
	
	/* based on the parameters grab the last calc no */
	ll_last_annuity_calc_no = al_annuity_calc_no
ELSE
	
	/* based on the parameters grab the last calc no */
	ll_last_annuity_calc_no = wf_get_max_annuity_calc_record(al_annuity_account_no)
END IF 

//VALID?
IF ISNULL(ll_last_annuity_calc_no) OR ll_last_annuity_calc_no <= 0 THEN RETURN -1

/* set the dataobject */
dw_information.dataobject = 'd_annuity_account_header_tv'
dw_information.settransobject(sqlca)
					
/* do the retrieve based on the claim number abd the last calc # */					
li_rowcount = dw_information.retrieve( ll_last_annuity_calc_no, al_annuity_account_no,is_claim_role_code )
SQLCA.nf_handle_error('w_calc_annuity','wf_retrieve_account_totals()','dw_information.retrieve()')

/* if the main account window has no return most likely there are adjustments so try and return those
     the account window is a composite so just modify the object to the adjustment object and re-retrieve
*/
IF ISNULL(li_rowcount) OR li_rowcount <= 0 THEN 
	dw_information.dataobject = 'd_sub_ledger_adjustments_by_account'
	dw_information.SetTransObject(SQLCA)
	dw_information.Retrieve(al_annuity_account_no, ll_last_annuity_calc_no, is_claim_role_code)
	SQLCA.nf_handle_error("w_calc_annuity","wf_retrieve_account_totals()","dw_information.retrieve()")
END IF			
									
RETURN 1
					
end function

public function integer wf_refresh_entitlement (integer ai_type);/* refresh the entitlement datawindow */
INTEGER 				li_rowcount

dw_benefit_entitlement.dataobject = 'd_be_calculate_by_account'
dw_benefit_entitlement.SetTransObject(SQLCA)
idw_benefit_entitlement.Retrieve(il_annuity_account_no, il_calc_no)
SQLCA.nf_handle_error("w_calc_annuity","wf_refresh_entitlement()","idw_benefit_entitlement.retrieve(il_annuity_account_no)")

CHOOSE CASE ai_type//determines dataobject to use
	CASE 1//BY ACCOUNT ONLY
			wf_set_be_filter(0)
	CASE 2//BY CLAIM# AND ACCOUNT
		/* filter out the appropriate claim_no */
			wf_set_be_filter(il_claim_no)
		
	CASE 3//reset the datawindow to nothing
		dw_benefit_entitlement.reset()
END CHOOSE

//grab the rowcountafter the filter
li_rowcount = dw_benefit_entitlement.ROWCOUNT() 

IF li_rowcount > 0 THEN// populate the screen which uses text to populate the totals.
	wf_populate_calculated_totals(il_annuity_account_no)
	idw_benefit_entitlement.selectrow(0,FALSE)
	idw_benefit_entitlement.SelectRow(1, TRUE)
ELSE
	wf_populate_benefit_totals(0, 0, 0, 0)//reset to nothing
END IF

RETURN 1


end function

public subroutine wf_scroll_to_row_in_dw_child (integer ai_row);datawindowchild	ldwc_date_list
INTEGER				li_rowcount

// retrieve the dropdown with the dates in it.
dw_account_dates.GetChild("calc_dates",ldwc_date_list)
ldwc_date_list.SetTransObject(SQLCA)

li_rowcount = ldwc_date_list.rowcount()
ldwc_date_list.scrolltorow(1)

dw_account_dates.scrolltorow(1)
dw_account_dates.setitem(1,'calc_dates',ldwc_date_list.getitemdatetime(1,'create_date'))
end subroutine

public subroutine wf_populate_benefit_totals (long al_annuity_account_no, long al_calc_no, long al_claim_no, integer ai_mode);/* This functions populates the totals in the benefit datawindow to 0
At the bottom of the list of entitlement the following columns will be totaled. 
     Number of hours 
·	Number of days 
·	Number of weeks
·	Number of months
·	Total benefit entitlement amount

*/
DECIMAL{2} ldc_Entitlement_sum, ldc_Entitlement_Days, ldc_Entitlement_Weeks, ldc_Entitlement_Months, ldc_Entitlement_hours
	
ldc_Entitlement_sum 		= 0.00 
ldc_Entitlement_hours 	= 0.00
ldc_Entitlement_Days 		= 0.00
ldc_Entitlement_Weeks 	= 0.00
ldc_Entitlement_Months	= 0.00


/* POPULATE THE TEXT COLUMNS IN THE DATAWINDOW */
//ENTITLEMENT
idw_benefit_entitlement.Object.t_ben_hours.Text 		= STRING(ldc_Entitlement_hours)
idw_benefit_entitlement.Object.t_ben_days.Text 		= STRING(ldc_Entitlement_Days)
idw_benefit_entitlement.Object.t_ben_weeks.Text 	= STRING(ldc_Entitlement_Weeks)
idw_benefit_entitlement.Object.t_ben_months.Text 	= STRING(ldc_Entitlement_Months)
idw_benefit_entitlement.Object.t_ben_amount.Text 	= STRING(ldc_Entitlement_sum,'$#,##0.00;($#,##0.00)')


end subroutine

public subroutine wf_set_be_filter (long al_claim_no);IF ISNULL(al_claim_no) THEN al_claim_no = 0

CHOOSE CASE al_claim_no
	CASE 0

		dw_benefit_entitlement.SetFilter('' )
	CASE ELSE 
		
		dw_benefit_entitlement.SetFilter('claim_no = ' + String(al_claim_no) )
END CHOOSE


dw_benefit_entitlement.SetRedraw(FALSE)
dw_benefit_entitlement.Filter()
dw_benefit_entitlement.SetRedraw(TRUE)
end subroutine

public subroutine wf_report ();s_window_message 		lstr_message

/* window open requirements
		il_calc_no 					= lstr_message.al_doubleparm[1]
		il_annuity_account_no 	= lstr_message.al_doubleparm[2]
*/
IF isnull(il_calc_no)                  	OR il_calc_no <= 0 					THEN RETURN
IF isnull(il_annuity_account_no) 	OR il_annuity_account_no <= 0 	THEN RETURN

lstr_message.al_doubleparm[1] 			= il_calc_no
lstr_message.al_doubleparm[2] 			= il_annuity_account_no
lstr_message.apo_powerobjectparm[1] 	= ids_account_info_for_treeview

OpenWithParm(w_annuity_account_report_viewer, lstr_message)
end subroutine

public subroutine wf_display_annuity_payout (boolean ab_display, long al_annuity_payout_no);st_annuity_payout_label.Visible = ab_display
st_annuity_payout_no.Visible    = ab_display
st_annuity_payout_no.Text       = String(al_annuity_payout_no)
end subroutine

on w_calc_annuity.create
int iCurrent
call super::create
if IsValid(this.MenuID) then destroy(this.MenuID)
if this.MenuName = "m_annuity" then this.MenuID = create m_annuity
this.st_annuity_payout_no=create st_annuity_payout_no
this.st_annuity_payout_label=create st_annuity_payout_label
this.dw_calc_header_two=create dw_calc_header_two
this.st_multiple_accounts=create st_multiple_accounts
this.st_warning=create st_warning
this.dw_account_dates=create dw_account_dates
this.dw_worker_details=create dw_worker_details
this.dw_injured_worker=create dw_injured_worker
this.pb_1=create pb_1
this.pb_slide_tree=create pb_slide_tree
this.st_vert_splitbar_tv_left=create st_vert_splitbar_tv_left
this.dw_benefit_entitlement=create dw_benefit_entitlement
this.dw_information=create dw_information
this.tv_manager=create tv_manager
this.dw_annuity_calc_header=create dw_annuity_calc_header
this.st_vert_splitbar_tv_right=create st_vert_splitbar_tv_right
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_annuity_payout_no
this.Control[iCurrent+2]=this.st_annuity_payout_label
this.Control[iCurrent+3]=this.dw_calc_header_two
this.Control[iCurrent+4]=this.st_multiple_accounts
this.Control[iCurrent+5]=this.st_warning
this.Control[iCurrent+6]=this.dw_account_dates
this.Control[iCurrent+7]=this.dw_worker_details
this.Control[iCurrent+8]=this.dw_injured_worker
this.Control[iCurrent+9]=this.pb_1
this.Control[iCurrent+10]=this.pb_slide_tree
this.Control[iCurrent+11]=this.st_vert_splitbar_tv_left
this.Control[iCurrent+12]=this.dw_benefit_entitlement
this.Control[iCurrent+13]=this.dw_information
this.Control[iCurrent+14]=this.tv_manager
this.Control[iCurrent+15]=this.dw_annuity_calc_header
this.Control[iCurrent+16]=this.st_vert_splitbar_tv_right
end on

on w_calc_annuity.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.st_annuity_payout_no)
destroy(this.st_annuity_payout_label)
destroy(this.dw_calc_header_two)
destroy(this.st_multiple_accounts)
destroy(this.st_warning)
destroy(this.dw_account_dates)
destroy(this.dw_worker_details)
destroy(this.dw_injured_worker)
destroy(this.pb_1)
destroy(this.pb_slide_tree)
destroy(this.st_vert_splitbar_tv_left)
destroy(this.dw_benefit_entitlement)
destroy(this.dw_information)
destroy(this.tv_manager)
destroy(this.dw_annuity_calc_header)
destroy(this.st_vert_splitbar_tv_right)
end on

event open;call super::open;INTEGER						li_upperbound, li_row, li_annuity_set_aside_percent_no
STRING						ls_claim_no
s_window_message 		   lstr_message
WINDOW                  lw_win

/* 
Opening the Module 
(MODE 1)

The purpose of this module is to make annuity-related information available to everyone who requires access to it. 
It is intended to be a general inquiry module available to everyone who has access to the Workbench, 
similar to Payment Inquiry.

Instead of creating a physical file and placing it in the claim file via the current document infrastructure,
it was decided to allow inquiry of the data and the ability to print a report, as required.	

The Calculate Annuity module will open but in read only mode with a dropdown menu listing 
the calculation dates for selection of any calculation that is available.

The most recent calculation date will be selected and the most recent annuity benefit calculation 
will be displayed on opening the inquiry module.

(MODE 2)
When the module is opened the user will be prompted for the calculation reason. 
If the module is opened to calculate for payout (a payout number greater than zero is passed in) 
and the last quarter’s interest rate is not available then the module will not open. 

If the module is opened for the same annuity eligibility as the last calculation, then no need to create a new calculation,
the module can open with the latest calculation. Otherwise if the annuity eligibility is new, (not existing on a calculation) 
create a new calculation and save the data.

Besides the information from the prompts above, the module will need to know the annuity account number, 
the eligibility number of the last confirmed eligibility and a payout number of either zero if not for payout or a valid payout number.

All the calculations will be made and saved during the open of the module, if a new calculation is required.  
All the saved data will then be displayed for the user to view.  If necessary, the user can select 
previous calculations from a list and view the previous calculations.(For second release of module – access to history not defined yet)

This module may be opened even if the individual does not qualify or no longer qualifies for annuity benefits.  
There will be no data to summarize by quarter, but there may be adjustments that need to be calculated.

The ‘calculated’ data needs to be displayed in three ways: 
·	Annuity benefits by annuity account
·	Annuity benefits by claim
·	Annuity sub-ledger adjustments by claim

*/

lstr_message = Message.powerobjectparm

/***************************/
//grab the information from the calling screen
il_annuity_account_no                    = lstr_message.al_doubleparm[1]
il_annuity_eligibility_no                = lstr_message.al_doubleparm[2]
il_annuity_payout_no                     = lstr_message.al_doubleparm[3]
li_annuity_set_aside_percent_no          = lstr_message.al_doubleparm[4]
il_open_mode                             = lstr_message.al_doubleparm[5] // used to open in calculate or no claculate (viewing) mode. 1 - calculate, 2 -  no calculate
idtm_annuity_start_date                  = lstr_message.adtm_datetimeparm[1]
idtm_annuity_end_date                    = lstr_message.adtm_datetimeparm[2]
idtm_annuity_eligibility_end_date_used   = lstr_message.adtm_datetimeparm[3]

IF il_open_mode = 2 THEN
	IF lstr_message.al_doubleparm[6] = 0 THEN
		ib_create_annuity_calc_records = FALSE
	ELSE
		ib_create_annuity_calc_records = TRUE
	END IF
END IF

SELECT 	annuity_set_aside_percent
INTO   	:idec_annuity_set_aside_percent
FROM  	Annuity_Set_Aside_Percent
WHERE  	annuity_set_aside_percent_no = :li_annuity_set_aside_percent_no 
USING 	SQLCA;
SQLCA.nf_handle_error('w_calc_annuity', 'embedded SQL - SELECT annuity_set_aside_percent FROM Annuity_Set_Aside_Percent...', 'open event')

//create the NVO
inv_common_annuity 	= CREATE n_common_annuity 

// need the il_annuity_account_no populated if it isn't we have a problem
IF ISNULL(il_annuity_account_no) OR il_annuity_account_no = 0 THEN
	messagebox('Invalid Annuity Account Number','No valid Annuity Account number has been supplied, the application cannot proceed. Contact the helpdesk')
	CLOSE(THIS)
	RETURN
END IF 

// need the eligibility_no 
IF il_open_mode = 2  THEN 
	IF  ISNULL(il_annuity_eligibility_no) OR il_annuity_eligibility_no = 0 THEN
		messagebox('Invalid Annuity Eligibility Number','No valid Annuity Eligibility number has been supplied, the application cannot proceed. Contact the helpdesk')
		CLOSE(THIS)
		RETURN
	END IF 
	    
	// make the calc date invisible
	dw_account_dates.visible = FALSE
	
END IF 

/* if we have this then that's all we need fill in the other information */
wf_pop_info_from_account_info(il_annuity_account_no)

// this should be taken care of in the code that calls this but just in case add it here
IF is_claim_role_code = 'C' THEN il_claim_no = 0

/* set up the datawindows instances -- this will allow us to shorten the datawindow name*/
idw_benefit_entitlement 	= dw_benefit_entitlement


ls_claim_no = string(il_claim_no)

/* now retrieve the injured worker info - top datawindow */
wf_populate_worker_info(il_individual_no, ls_claim_no )

//check and see if the individual has multiple accounts if they do make the image visible
IF inv_common_annuity.nf_multiple_annuity_accounts(il_annuity_account_no) > 0 THEN
	st_multiple_accounts.visible = TRUE
ELSE
	st_multiple_accounts.visible = FALSE
END IF 

//re-populate the worker detail information just in case something has changed
wf_populate_worker_details(il_individual_no)

//populate the eligibility information
dw_calc_header_two.retrieve(il_annuity_eligibility_no)
sqlca.nf_handle_error("w_calc_annuity","open","dw_calc_header_two.retrieve(il_annuity_eligibility_no)")

//set the global variable used in the visible property of the BE datawindow for  60% flag
gs_claim_role_code =  is_claim_role_code             

/* determine initial state of frame */
iws_frame_open_state = w_frame.WindowState
	
THIS.post wf_refresh_entitlement(1)

li_upperbound = UpperBound(gstr_window_array) + 1

iul_handle = Handle(THIS)

gstr_window_array[li_upperbound].window_element = this
gstr_window_array[li_upperbound].handle_element 	= iul_handle

IF IsValid(inv_resize) THEN
ELSE
	inv_resize = CREATE n_resize
END IF

inv_resize.of_SetOrigSize (this.width, this.height)

/* maximize frame while module is envoked */
this.WindowState = Maximized!

//splitbar for Treeview and information
st_vert_splitbar_tv_left.of_register(dw_information)
st_vert_splitbar_tv_left.of_register(tv_manager)

//splitbar for Treeview and information
st_vert_splitbar_tv_right.of_register(dw_information)
st_vert_splitbar_tv_right.of_register(idw_benefit_entitlement)

inv_resize.of_register(st_vert_splitbar_tv_left,'ScaleToBottom')

inv_resize.of_register(st_vert_splitbar_tv_right,'ScaleToBottom')
inv_resize.of_register(dw_information,'ScaleToBottom')
inv_resize.of_register(idw_benefit_entitlement,'ScaleToRight&Bottom')
inv_resize.of_register(tv_manager,'ScaleToBottom')


lw_win = THIS
// make print menu item not visible
inv_common_annuity.nf_make_menu_item_invisible(lw_win,'m_file','m_print')
inv_common_annuity.nf_make_menu_item_invisible(lw_win,'m_file','m_printsetup')

// make document menu item not visible
inv_common_annuity.nf_make_menu_item_invisible(lw_win,'m_docmanagement','')

// make event log menu item not visible
inv_common_annuity.nf_make_menu_item_invisible(lw_win,'m_eventlog','')



this.post event ue_post_open()
end event

event close;call super::close;INTEGER		li_counter, li_upper

inv_common_annuity.nf_close_handle_array(iul_handle)

li_upper = UpperBound(gstr_window_array)

FOR li_counter = 1 TO li_upper
	IF gstr_window_array[li_counter].window_element.ClassName() = 'w_calc_annuity' THEN
		gstr_window_array[li_counter].window_element.SetFocus()
	END IF
NEXT

end event

event resize;call super::resize;st_vert_splitbar_tv_right.of_SetRequestor(THIS)

end event

type st_annuity_payout_no from statictext within w_calc_annuity
integer x = 5701
integer y = 8
integer width = 192
integer height = 60
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

type st_annuity_payout_label from statictext within w_calc_annuity
integer x = 5262
integer y = 8
integer width = 411
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Annuity Payout:"
boolean focusrectangle = false
end type

type dw_calc_header_two from u_dw_online within w_calc_annuity
integer x = 2281
integer y = 4
integer width = 1454
integer height = 216
integer taborder = 30
string dataobject = "d_calculate_eligibility_info"
boolean border = false
end type

event constructor;call super::constructor;settransobject(sqlca)
end event

type st_multiple_accounts from statictext within w_calc_annuity
integer x = 809
integer y = 196
integer width = 1961
integer height = 88
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
alignment alignment = center!
boolean focusrectangle = false
end type

type st_warning from statictext within w_calc_annuity
boolean visible = false
integer x = 1024
integer y = 956
integer width = 1385
integer height = 512
integer textsize = -18
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 255
long backcolor = 16777215
string text = "NO BENEFIT ENTITLEMENT OR SUB-LEDGER ADJUSTMENTS"
alignment alignment = center!
boolean focusrectangle = false
end type

type dw_account_dates from u_dw_online within w_calc_annuity
event ue_retrieve_treeview pbm_custom01
integer x = 5
integer y = 196
integer width = 585
integer height = 96
integer taborder = 30
string dataobject = "d_calc_dates_select"
boolean maxbox = true
boolean border = false
end type

event ue_retrieve_treeview;BOOLEAN           lb_display_annuity_payout
LONG						ll_currow, ll_rows, ll_calc_no, ll_tvi_next_item, tvi_hdl = 0, ll_tvi, ll_eligibility_no, ll_annuity_payout_no
DATETIME				ldt_test, ldtm_test
DATAWINDOWCHILD	ldwc_child
					

// grab the child with all of the info we need
THIS.GetChild('calc_dates',ldwc_child)

//make sure we have a valid row
ll_currow = ldwc_child.getrow()

IF isnull(ll_currow) OR ll_currow < 1 THEN RETURN

//Grab the data we need to populate the treeview
il_calc_no = ldwc_child.getitemnumber(ll_currow, 'annuity_calc_no')

IF il_calc_no < 1  THEN RETURN 

DO UNTIL tv_manager.FindItem(RootTreeItem!, 0) = -1

    tv_manager.DeleteItem(tvi_hdl)

LOOP

/* populate the treeview */
wf_populate_treeview()

dw_annuity_calc_header.retrieve(il_calc_no)
sqlca.nf_handle_error("w_calc_annuity","ue_retrieve_treeview","dw_annuity_calc_header.retrieve(il_calc_no)")


ll_eligibility_no = ldwc_child.getitemnumber(ll_currow, 'annuity_eligibility_no')

dw_calc_header_two.retrieve(ll_eligibility_no)
sqlca.nf_handle_error("w_calc_annuity","ue_retrieve_treeview","dw_calc_header_two.retrieve(ll_eligibility_no)")

/* now select the first item in the treeview and click it inorder to view */
ll_tvi = tv_manager.FindItem(RootTreeItem!, 0)

IF ll_tvi > 0 THEN 

	ll_tvi_next_item = tv_manager.FindItem(ChildTreeItem!,ll_tvi)

	/* trigger the retrieve through a function */
	wf_treeview_clicked(ll_tvi_next_item)
	
	/* highlight the first treeview record will not need to be set after this taken care 
	    of in the selectionchanged event */
	tv_manager.setdrophighlight(ll_tvi_next_item)
	
END IF 


ll_annuity_payout_no = ldwc_child.getitemnumber(ll_currow, 'annuity_payout_no')
IF ll_annuity_payout_no > 0 THEN
	lb_display_annuity_payout = TRUE
ELSE
	lb_display_annuity_payout = FALSE
END IF

wf_display_annuity_payout(lb_display_annuity_payout,ll_annuity_payout_no)

end event

event constructor;call super::constructor;this.settransobject(sqlca)
end event

event itemchanged;call super::itemchanged;LONG			ll_currow

//	Check to see if we have a current row (ie, they didn't click on the header or something)
IF GetRow() <=0 THEN RETURN

//	Post the event to retrieve the datawindow
PostEvent("ue_retrieve_treeview")
end event

type dw_worker_details from u_dw_online within w_calc_annuity
integer x = 1454
integer y = 12
integer width = 800
integer height = 176
integer taborder = 20
string dataobject = "d_worker_details"
boolean border = false
end type

type dw_injured_worker from u_dw_online within w_calc_annuity
integer x = 18
integer y = 12
integer width = 1413
integer height = 164
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_entitlement_worker_info_calculate"
boolean border = false
end type

event constructor;call super::constructor;settransobject(sqlca)
end event

type pb_1 from picturebutton within w_calc_annuity
integer x = 2784
integer y = 220
integer width = 169
integer height = 80
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "splitter_ver_24_h.bmp"
alignment htextalign = left!
end type

event clicked;Integer li_x

li_x = dw_benefit_entitlement.x + dw_benefit_entitlement.width + 20

IF st_vert_splitbar_tv_right.x  = 2853 then
	st_vert_splitbar_tv_right.x = li_x
ELSE
	st_vert_splitbar_tv_right.x = 2853	
END IF 

st_vert_splitbar_tv_right.Triggerevent("lbuttonup")
end event

type pb_slide_tree from picturebutton within w_calc_annuity
integer x = 594
integer y = 232
integer width = 169
integer height = 80
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "splitter_ver_24_h.bmp"
alignment htextalign = left!
end type

event clicked;IF st_vert_splitbar_tv_left.x = 667 THEN 
	st_vert_splitbar_tv_left.x 	= 20
	tv_manager.visible 			= FALSE
ELSE
	st_vert_splitbar_tv_left.x 	= 667
	tv_manager.visible 			= TRUE	
END IF 

st_vert_splitbar_tv_left.Triggerevent("lbuttonup")

end event

type st_vert_splitbar_tv_left from u_splitbar_vertical within w_calc_annuity
integer x = 667
integer y = 312
integer width = 23
integer height = 2432
boolean bringtotop = true
long il_min_units_from_left = 50
long il_min_units_from_right = 0
end type

event ue_moved;call super::ue_moved;/* dont allow the splitbar to go over the other splitbar*/
IF st_vert_splitbar_tv_left.x >= st_vert_splitbar_tv_right.x - 250 THEN
	 st_vert_splitbar_tv_left.x = st_vert_splitbar_tv_right.x - 250
	 st_vert_splitbar_tv_left.Triggerevent("lbuttonup")
END IF 
end event

type dw_benefit_entitlement from u_dw_online within w_calc_annuity
integer x = 2889
integer y = 316
integer width = 2354
integer height = 2428
integer taborder = 50
string dataobject = "d_be_calculate_by_account"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;settransobject(sqlca)
THIS.uf_setselect(1)

end event

event rbuttondown;m_dw_rmb_popup lm_popup
/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/

lm_popup = Create m_dw_rmb_popup
lm_popup.mf_set_datawindow(This)
lm_popup.m_options.m_sort.visible = FALSE

// don't display lines
lm_popup.m_options.m_1.Visible = FALSE
lm_popup.m_options.m_2.Visible = FALSE

lm_popup.m_options.PopMenu(PARENT.PointerX( ), PARENT.PointerY( ))
end event

event ue_print;THIS.Object.DataWindow.Print.Orientation = 1 //landscape
THIS.Print()
end event

type dw_information from u_dw_online within w_calc_annuity
integer x = 695
integer y = 316
integer width = 2153
integer height = 2428
integer taborder = 40
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rbuttondown;m_dw_rmb_popup lm_popup
/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/

lm_popup = Create m_dw_rmb_popup
lm_popup.mf_set_datawindow(This)
lm_popup.m_options.m_sort.visible = FALSE

// don't display lines
lm_popup.m_options.m_1.Visible = FALSE
lm_popup.m_options.m_2.Visible = FALSE

lm_popup.m_options.PopMenu(PARENT.PointerX( ), PARENT.PointerY( ))

end event

type tv_manager from treeview within w_calc_annuity
integer y = 316
integer width = 654
integer height = 2428
integer taborder = 30
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 134217752
borderstyle borderstyle = styleraised!
long picturemaskcolor = 536870912
long statepicturemaskcolor = 536870912
end type

event clicked;/* This needs to be called in other places so move the code to a common function that can be called from anywhere */

IF ISNULL(handle) OR handle <= 0 THEN RETURN 

PARENT.wf_treeview_clicked(handle)

end event

event selectionchanged;TREEVIEWITEM tvi

/* set the properties for the old handle */
tv_manager.GetItem(oldhandle, tvi)
tvi.selected 		= FALSE
tvi.bold        	= FALSE		
tv_manager.SetItem(oldhandle, tvi)

/* set the properties for the new handle */
tv_manager.GetItem(newhandle, tvi)
tvi.selected 		= TRUE
tvi.bold        	= TRUE		
tv_manager.SetItem(newhandle, tvi)

tv_manager.setdrophighlight(newhandle)
end event

type dw_annuity_calc_header from u_dw_online within w_calc_annuity
integer x = 3794
integer y = 8
integer width = 1394
integer height = 228
integer taborder = 0
boolean bringtotop = true
string dataobject = "d_calculate_annuity_dates"
boolean border = false
end type

event constructor;call super::constructor;settransobject(sqlca)
end event

type st_vert_splitbar_tv_right from u_splitbar_vertical within w_calc_annuity
integer x = 2853
integer y = 308
integer width = 23
integer height = 2432
boolean bringtotop = true
long il_min_units_from_left = 200
long il_min_units_from_right = 200
end type

event ue_moved;call super::ue_moved;/* dont allow the splitbar to go over the other splitbar*/
IF st_vert_splitbar_tv_right.x <= st_vert_splitbar_tv_left.x + 250 THEN
    st_vert_splitbar_tv_right.x = st_vert_splitbar_tv_left.x + 250
	 st_vert_splitbar_tv_right.Triggerevent("lbuttonup")
END IF 
end event

