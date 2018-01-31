$PBExportHeader$w_task_authorization_report.srw
forward
global type w_task_authorization_report from w_a_report
end type
type cb_ok from commandbutton within w_task_authorization_report
end type
type cb_clear from commandbutton within w_task_authorization_report
end type
type dw_search_criteria from u_dw_online within w_task_authorization_report
end type
end forward

global type w_task_authorization_report from w_a_report
integer y = 49
integer width = 3209
cb_ok cb_ok
cb_clear cb_clear
dw_search_criteria dw_search_criteria
end type
global w_task_authorization_report w_task_authorization_report

type variables
STRING			is_original_select
n_resize			inv_resize  
end variables

on w_task_authorization_report.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.cb_clear=create cb_clear
this.dw_search_criteria=create dw_search_criteria
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.cb_clear
this.Control[iCurrent+3]=this.dw_search_criteria
end on

on w_task_authorization_report.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.cb_clear)
destroy(this.dw_search_criteria)
end on

event open;call super::open;	DATAWINDOWCHILD	ldw_child
	INTEGER				li_result

/*	Make the search criteria datawindow visible. */	
	dw_search_criteria.SetTransObject(SQLCA)
	dw_search_criteria.InsertRow(0)

/*	Save the original query for the report. */
	is_original_select = dw_report.Describe("DataWindow.Table.Select")

/*	Initalize the report datawindows. */
dw_report.SetTransObject(SQLCA)
	

IF IsNull(inv_resize) OR NOT IsValid (inv_resize) THEN
	inv_resize = create n_resize
	inv_resize.of_SetOrigSize (3173,2580)
END IF

THIS.inv_resize.of_register(dw_report,0,0,100,100)


end event

event resize;call super::resize;
LONG ll_workspacewidth, ll_workspaceheight

// Notify the resize service that the w_a_report size has changed.
ll_workspacewidth 		= THIS.WorkSpaceWidth()
ll_workspaceheight 	= THIS.WorkSpaceHeight()

IF IsValid (inv_resize) THEN
	inv_resize.Event pfc_Resize (sizetype,ll_workspacewidth  , ll_workspaceheight )
END IF
end event

type dw_report from w_a_report`dw_report within w_task_authorization_report
integer x = 14
integer y = 904
integer width = 3145
integer height = 1632
integer taborder = 40
string dataobject = "d_task_authorization_report"
boolean hscrollbar = true
end type

type cb_ok from commandbutton within w_task_authorization_report
event clicked pbm_bnclicked
integer x = 2583
integer y = 212
integer width = 389
integer height = 108
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;	LONG			ll_numrows, ll_claim_no_from, ll_claim_no_to, ll_billable_item_xref_no, ll_billable_item_no
	STRING			ls_where_statement, ls_claim_manager_user_id, ls_modstring, ls_result, ls_rehab_program_code
	STRING			ls_report_parameters, ls_task_type_code, ls_task_sub_type_code, ls_rehab_service_code_desc
	STRING			ls_responsible_user_id, ls_admin_region_code, ls_task_specific_code, ls_rehab_service_code, ls_billable_item_desc
	INTEGER		li_rowcount
	
	datawindowchild		ldwc_child

	dw_search_criteria.AcceptText()
	ls_where_statement 				= ''
	ls_report_parameters 			= ''

	ll_claim_no_from					= dw_search_criteria.GetItemNumber(1,'claim_no_from')
	ll_claim_no_to						= dw_search_criteria.GetItemNumber(1,'claim_no_to')
	ls_claim_manager_user_id		= dw_search_criteria.GetItemString(1,'claim_manager_user_id')
	ls_responsible_user_id			= dw_search_criteria.GetItemString(1,'responsible_user_id')
	ls_admin_region_code			= dw_search_criteria.GetItemString(1,'admin_region_code')
	ll_billable_item_no					= dw_search_criteria.GetItemNumber(1,'billable_item_no')   
	ls_rehab_service_code_desc	= dw_search_criteria.GetItemString(1,'rehab_service_code')
	
	
	/* reset the report datawindow */
	dw_report.reset()
	
	
	/*
	d_task_authorization_report
	
	REHAB_TASK_AUTHORIZATION a
	REHAB_TASK								b
	CLAIM										c 
	Billable_Item_Rehab_Task_Xref	d
	Billable_Item          					e
	Task_Type								f 
	Task_Sub_Type          				g
	Task_Specific								h 

	*/
	
	IF NOT IsNull(ll_claim_no_from) THEN
		IF NOT IsNull(ll_claim_no_to) THEN
			IF ll_claim_no_from > ll_claim_no_to THEN
				MessageBox("Search Criteria Error","Cannot have a From: claim number that is greater than a To: claim number.")
				RETURN
			ELSE
				ls_where_statement = ls_where_statement + " AND a.claim_no >= " + String(ll_claim_no_from) + " AND a.claim_no <= " + String(ll_claim_no_to)
				IF ls_report_parameters = '' THEN
					ls_report_parameters = 'Claim: ' + String(ll_claim_no_from) + ' to ' + String(ll_claim_no_to)
				ELSE
					ls_report_parameters = ls_report_parameters + ', Claim: ' + String(ll_claim_no_from) + ' to ' + String(ll_claim_no_to)
				END IF
			END IF
		ELSE
			MessageBox("Search Criteria Error","Both the From: and To: claim numbers need to be entered.")
			RETURN
		END IF
	ELSE
		IF NOT IsNull(ll_claim_no_to) THEN
			MessageBox("Search Criteria Error","Both the From: and To: claim numbers need to be entered.")
			RETURN
		END IF
	END IF

	IF NOT IsNull(ls_admin_region_code) THEN
		ls_where_statement = ls_where_statement + ' AND c.admin_region_code = "' + ls_admin_region_code + '"'
		IF ls_report_parameters = '' THEN
			ls_report_parameters = 'Admin Region: ' + ls_admin_region_code
		ELSE
			ls_report_parameters = ls_report_parameters + ', Admin Region: ' + ls_admin_region_code
		END IF
	ELSE
		IF ls_report_parameters = '' THEN
			ls_report_parameters = 'Admin Region: ALL'
		ELSE
			ls_report_parameters = ls_report_parameters + ', Admin Region: ALL'
		END IF
	END IF

	IF NOT IsNull(ls_claim_manager_user_id) THEN
		ls_where_statement = ls_where_statement + ' AND c.claim_manager_user_id = "' + ls_claim_manager_user_id + '"'
		IF ls_report_parameters = '' THEN
			ls_report_parameters = 'Claim Manager: ' + ls_claim_manager_user_id
		ELSE
			ls_report_parameters = ls_report_parameters + ', Claim Manager: ' + ls_claim_manager_user_id
		END IF
	ELSE
		IF ls_report_parameters = '' THEN
			ls_report_parameters = 'Claim Manager: ALL'
		ELSE
			ls_report_parameters = ls_report_parameters + ', Claim Manager: ALL'
		END IF
	END IF

	IF NOT IsNull(ls_responsible_user_id) THEN
		ls_where_statement = ls_where_statement + ' AND b.responsible_user_id = "' + ls_responsible_user_id + '"'
		IF ls_report_parameters = '' THEN
			ls_report_parameters = 'Resp. User: ' + ls_responsible_user_id
		ELSE
			ls_report_parameters = ls_report_parameters + ', Resp. User: ' + ls_responsible_user_id
		END IF
	END IF

	IF NOT IsNull(ls_rehab_service_code_desc) THEN
			
		// need to grab the value from the dddw_child as the column is actually a dummy column	
		dw_search_criteria.getchild('rehab_service', ldwc_child)
		
		// do the retrieve and then filter based on the provider
		li_rowcount = ldwc_child.rowcount()
		IF isnull(li_rowcount) OR li_rowcount < 1 THEN 
		ELSE
			
			ls_rehab_service_code 	= ldwc_child.getitemstring(ldwc_child.getrow(),'rehab_service_code')
			ls_rehab_program_code = ldwc_child.getitemstring(ldwc_child.getrow(),'rehab_program_code')
			
			// service
			IF len(trim(ls_rehab_service_code )) > 0 THEN 
					ls_where_statement = ls_where_statement + ' AND d.rehab_service_code = "' + ls_rehab_service_code + '"'
			END IF
			
			//program
			IF len(trim(ls_rehab_program_code )) > 0 THEN 
					ls_where_statement = ls_where_statement + ' AND d.rehab_program_code = "' + ls_rehab_program_code + '"'
			END IF
			
		END IF 
	
		IF ls_report_parameters = '' THEN
			ls_report_parameters = 'Rehab Service: ' + ls_rehab_service_code_desc
		ELSE
			ls_report_parameters = ls_report_parameters + ', Rehab Service: ' + ls_rehab_service_code_desc
		END IF
		
	END IF
	
	IF ll_billable_item_no > 0 THEN
		ls_where_statement = ls_where_statement + ' AND e.billable_item_no = "' + string(ll_billable_item_no) + '"'
		
		dw_search_criteria.getchild('billable_item', ldwc_child)

		// do the retrieve and then filter based on the provider
		li_rowcount = ldwc_child.rowcount()
		IF isnull(li_rowcount) OR li_rowcount < 1 THEN 
		ELSE
			ls_billable_item_desc = ldwc_child.getitemstring(ldwc_child.getrow(), 'billable_item_desc_e')
			IF ISNULL(ls_billable_item_desc) THEN ls_billable_item_desc = ''
		END IF 
		
		IF ls_report_parameters = '' THEN
			ls_report_parameters = 'Billable Item: ' + ls_billable_item_desc
		ELSE
			ls_report_parameters = ls_report_parameters + ', Billable Item: ' + ls_billable_item_desc
		END IF
	END IF


/*	Modify the report to include the calculated WHERE statement. Then retrieve the report.
*/
	ls_modstring = "DataWindow.Table.Select='" + is_original_select + ls_where_statement + "'"
	ls_result = dw_report.Modify(ls_modstring)
	IF ls_result = '' THEN
		ll_numrows = dw_report.Retrieve()
		SQLCA.nf_handle_error("w_task_authorization_report","dw_report","cb_ok")
		IF ll_numrows <= 0 THEN
			MessageBox("Rehabilitation Planner - Task Authorization Report","No data was found to satisfy the entered criteria.")
		ELSE
			dw_report.SetItem(1,'report_parameters', ls_report_parameters)
		END IF
	ELSE
		MessageBox("Rehabilitation Planner - Task Authorization Report","An error occured while setting the search criteria for the report.")
	END IF

end event

type cb_clear from commandbutton within w_task_authorization_report
integer x = 2583
integer y = 344
integer width = 389
integer height = 108
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Clea&r"
end type

event clicked;/*	Reset the current search criteria to nothing.
*/
	dw_search_criteria.Reset()
	dw_search_criteria.InsertRow(0)
//	dw_search_criteria.SetTabOrder('task_sub_type_code',0)
//	dw_search_criteria.SetTabOrder('task_specific_code',0)

end event

type dw_search_criteria from u_dw_online within w_task_authorization_report
integer x = 9
integer y = 52
integer width = 2199
integer height = 840
integer taborder = 10
string dataobject = "d_task_authorization_rpt_search_criteria"
boolean border = false
end type

event itemchanged;call super::itemchanged;STRING				ls_filter, ls_rehab_service_code, ls_null_string
INTEGER 			li_result
	
DATAWINDOWCHILD	ldwc_child

SetNull(ls_null_string)

CHOOSE CASE THIS.GetColumnName()				
		CASE	'claim_no_from'
			THIS.SetItem(1,'claim_no_to',Long(data))
	
//		CASE	'task_type_code'
//
///*	Turn tab of task_sub_type_code column on and filter out the ones not related
//	to the chosen task_type_code.
//*/
//			THIS.SetTabOrder('task_sub_type_code',44)
//			THIS.SetTabOrder('task_specific_code',0)		
//			THIS.SetItem(1,'task_sub_type_code',ls_null_string)
//			THIS.SetItem(1,'task_specific_code',ls_null_string)
//			THIS.GetChild('task_sub_type_code',ldwc_child)
//			ls_filter = "task_type_code = '" + data + "'"
//			li_result = ldwc_child.SetFilter(ls_filter)
//			li_result = ldwc_child.Filter()
//		
//		CASE	'task_sub_type_code'
//		
///*	Turn tab of task_specific_code column on and filter out the ones not related
//	to the chosen task_type_code and task_sub_type_code.
//*/
//			THIS.SetTabOrder('task_specific_code',48)
//			THIS.SetItem(1,'task_specific_code',ls_null_string)
//			ls_task_type_code = THIS.GetItemString(1,'task_type_code')
//			THIS.GetChild('task_specific_code',ldwc_child)
//			ls_filter = "task_type_code = '" + ls_task_type_code + "' AND task_sub_type_code = '" + data + "'"
//			li_result = ldwc_child.SetFilter(ls_filter)
//			li_result = ldwc_child.Filter()


	CASE 'rehab_service_code'
		
			ls_rehab_service_code = THIS.GetItemString(1,'rehab_service_code')
			THIS.GetChild('billable_item_no',ldwc_child)
			ls_filter = "rehab_service_code = '" + ls_rehab_service_code + "'"
			li_result = ldwc_child.SetFilter(ls_filter)
			li_result = ldwc_child.Filter()

END CHOOSE

end event

