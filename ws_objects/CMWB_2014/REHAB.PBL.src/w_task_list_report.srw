$PBExportHeader$w_task_list_report.srw
forward
global type w_task_list_report from w_a_report
end type
type cb_ok from commandbutton within w_task_list_report
end type
type cb_provider_search from commandbutton within w_task_list_report
end type
type dw_search_criteria from u_dw_online within w_task_list_report
end type
type cb_clear from commandbutton within w_task_list_report
end type
end forward

global type w_task_list_report from w_a_report
integer y = 49
cb_ok cb_ok
cb_provider_search cb_provider_search
dw_search_criteria dw_search_criteria
cb_clear cb_clear
end type
global w_task_list_report w_task_list_report

type variables
STRING		is_original_select
end variables

on w_task_list_report.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.cb_provider_search=create cb_provider_search
this.dw_search_criteria=create dw_search_criteria
this.cb_clear=create cb_clear
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.cb_provider_search
this.Control[iCurrent+3]=this.dw_search_criteria
this.Control[iCurrent+4]=this.cb_clear
end on

on w_task_list_report.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.cb_provider_search)
destroy(this.dw_search_criteria)
destroy(this.cb_clear)
end on

event open;call super::open;	DATAWINDOWCHILD	ldw_child
	INTEGER				li_result

/*	Make the search criteria datawindow visible.
*/	
	dw_search_criteria.SetTransObject(SQLCA)
	dw_search_criteria.InsertRow(0)

/*	Save the original query for the report.
*/
	is_original_select = dw_report.Describe("DataWindow.Table.Select")

/*	Initalize the report datawindows.
*/
	dw_report.SetTransObject(SQLCA)

end event

type dw_report from w_a_report`dw_report within w_task_list_report
integer y = 1008
integer height = 1548
integer taborder = 40
string dataobject = "d_task_list_report"
boolean hscrollbar = true
end type

type cb_ok from commandbutton within w_task_list_report
event clicked pbm_bnclicked
integer x = 2153
integer y = 212
integer width = 389
integer height = 108
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;	STRING	ls_claim_manager_user_id, ls_admin_region_code, ls_report_parameters
	STRING	ls_task_type_code, ls_task_sub_type_code, ls_task_specific_code, ls_result
	STRING	ls_where_statement, ls_responsible_user_id, ls_provider_type_code, ls_modstring
	LONG		ll_numrows, ll_claim_no_from, ll_claim_no_to, ll_provider_no
	INTEGER	li_result

	dw_search_criteria.AcceptText()
	ls_where_statement = ''
	ls_report_parameters = ''

	ll_claim_no_from				= dw_search_criteria.GetItemNumber(1,'claim_no_from')
	ll_claim_no_to					= dw_search_criteria.GetItemNumber(1,'claim_no_to')
	ll_provider_no					= dw_search_criteria.GetItemNumber(1,'provider_no')
	ls_responsible_user_id		= dw_search_criteria.GetItemString(1,'responsible_user_id')
	ls_provider_type_code		= dw_search_criteria.GetItemString(1,'provider_type_code')
	ls_claim_manager_user_id	= dw_search_criteria.GetItemString(1,'claim_manager_user_id')
	ls_task_type_code				= dw_search_criteria.GetItemString(1,'task_type_code')
	ls_task_sub_type_code		= dw_search_criteria.GetItemString(1,'task_sub_type_code')
	ls_task_specific_code	         = dw_search_criteria.GetItemString(1,'task_specific_code')
	ls_admin_region_code		= dw_search_criteria.GetItemString(1,'admin_region_code')

//	ls_where_statement = " and REHAB_TASK.task_type_code <> ~~'AC~~' "
ls_where_statement = " WHERE  REHAB_TASK.task_type_code <> ~~'AC~~' "

	IF NOT IsNull(ll_claim_no_from) THEN
		IF NOT IsNull(ll_claim_no_to) THEN
			IF ll_claim_no_from > ll_claim_no_to THEN
				MessageBox("Search Criteria Error","Cannot have a From: claim number that is greater than a To: claim number.")
				RETURN
			ELSE
				ls_where_statement = ls_where_statement + " AND REHAB_TASK.claim_no >= " + String(ll_claim_no_from) + " AND REHAB_TASK.claim_no <= " + String(ll_claim_no_to)
				ls_report_parameters = 'Claim: ' + String(ll_claim_no_from) + ' to ' + String(ll_claim_no_to)
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

	IF NOT IsNull(ls_responsible_user_id) THEN
		ls_where_statement = ls_where_statement + ' AND REHAB_TASK.responsible_user_id = "' + ls_responsible_user_id + '"'
		IF ls_report_parameters = '' THEN
			ls_report_parameters = 'Responsible User: ' + ls_responsible_user_id
		ELSE
			ls_report_parameters = ls_report_parameters + ', Responsible User: ' + ls_responsible_user_id
		END IF
	END IF
	
	IF NOT IsNull(ls_provider_type_code) THEN
		IF NOT IsNull(ll_provider_no) THEN
			ls_where_statement = ls_where_statement + ' AND REHAB_TASK.provider_no = ' + String(ll_provider_no) + ' AND REHAB_TASK.provider_type_code = "' + ls_provider_type_code + '"'
			IF ls_report_parameters = '' THEN
				ls_report_parameters = 'Provider: ' + ls_provider_type_code + ' ' + String(ll_provider_no)
			ELSE
				ls_report_parameters = ls_report_parameters + ', Provider: ' + ls_provider_type_code + ' ' + String(ll_provider_no)
			END IF
		ELSE
			MessageBox("Search Criteria Error","Both the Provider No: and Provider Type: need to be entered.")
			RETURN
		END IF
	ELSE
		IF NOT IsNull(ll_provider_no) THEN
			MessageBox("Search Criteria Error","Both the Provider No: and Provider Type: need to be entered.")
			RETURN
		END IF
	END IF

	IF NOT IsNull(ls_admin_region_code) THEN
		ls_where_statement = ls_where_statement + ' AND CLAIM.admin_region_code = "' + ls_admin_region_code + '"'
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
		ls_where_statement = ls_where_statement + ' AND CLAIM.claim_manager_user_id = "' + ls_claim_manager_user_id + '"'
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

	IF NOT IsNull(ls_task_type_code) THEN
		ls_where_statement = ls_where_statement + ' AND REHAB_TASK.task_type_code = "' + ls_task_type_code + '"'
		IF ls_report_parameters = '' THEN
			ls_report_parameters = 'Task Type Code: ' + ls_task_type_code
		ELSE
			ls_report_parameters = ls_report_parameters + ', Task Type Code: ' + ls_task_type_code
		END IF
	END IF

	IF NOT IsNull(ls_task_sub_type_code) THEN
		ls_where_statement = ls_where_statement + ' AND REHAB_TASK.task_sub_type_code = "' + ls_task_sub_type_code + '"'
		IF ls_report_parameters = '' THEN
			ls_report_parameters = 'Task Sub Type Code: ' + ls_task_sub_type_code
		ELSE
			ls_report_parameters = ls_report_parameters + ', Task Sub Type Code: ' + ls_task_sub_type_code
		END IF
	END IF

	IF NOT IsNull(ls_task_specific_code) THEN
		ls_where_statement = ls_where_statement + ' AND REHAB_TASK.task_specific_code = "' + ls_task_specific_code + '"'
		IF ls_report_parameters = '' THEN
			ls_report_parameters = 'Task Specific Code: ' + ls_task_specific_code
		ELSE
			ls_report_parameters = ls_report_parameters + ', Task Specific Code: ' + ls_task_specific_code
		END IF
	END IF

/*	Modify the report to include the calculated WHERE statement. Then retrieve the report.
*/
	ls_modstring = "DataWindow.Table.Select='" + is_original_select + ls_where_statement + "'"
	ls_result = dw_report.Modify(ls_modstring)
	IF ls_result = '' THEN
		ll_numrows = dw_report.Retrieve()
		SQLCA.nf_handle_error("w_task_list_report","dw_report","cb_ok")
		IF ll_numrows <= 0 THEN
			MessageBox("Rehabilitation Planner - Task List Report","No data was found to satisfy the entered criteria.")
		ELSE
//			li_result = dw_report.SetItem(1,'report_parameters',ls_report_parameters)
		END IF
	ELSE
		MessageBox("Rehabilitation Planner - Task List Report","An error occured while setting the search criteria for the report.")
	END IF

end event

type cb_provider_search from commandbutton within w_task_list_report
event clicked pbm_bnclicked
integer x = 1947
integer y = 240
integer width = 96
integer height = 76
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "?"
end type

event clicked;S_WINDOW_MESSAGE	lstr_message
STRING 				ls_type

/*	Get the provider number to search for.
*/
	ls_type = dw_search_criteria.GetItemString(1,'provider_type_code')
	IF ls_type = 'V' OR ls_type =  'M' OR ls_type =  'O' THEN
		OpenWithParm(w_service_provider_search,ls_type)
		lstr_message = Message.PowerObjectParm
		dw_search_criteria.SetColumn('provider_no')
		dw_search_criteria.SetItem(1,'provider_no',lstr_message.al_doubleparm[1])
	ELSE
		MessageBox('Warning', 'No search available for this recipient type.')
	END IF

end event

type dw_search_criteria from u_dw_online within w_task_list_report
integer x = 37
integer y = 52
integer width = 2085
integer height = 932
integer taborder = 10
string dataobject = "d_task_list_report_search_criteria"
boolean border = false
end type

event itemchanged;call super::itemchanged;	STRING				ls_task_type_code, ls_task_sub_type_code, ls_null_string, ls_filter
	DATAWINDOWCHILD	ldwc_child
	INTEGER				li_result

	SetNull(ls_null_string)

	CHOOSE CASE THIS.GetColumnName()	
		CASE	'claim_no_from'
			THIS.SetItem(1,'claim_no_to',Long(data))
			
		CASE	'task_type_code'

/*	Turn tab of task_sub_type_code column on and filter out the ones not related
	to the chosen task_type_code.
*/
			THIS.SetTabOrder('task_sub_type_code',74)
			THIS.SetTabOrder('task_specific_code',0)		
			THIS.SetItem(1,'task_sub_type_code',ls_null_string)
			THIS.SetItem(1,'task_specific_code',ls_null_string)
			THIS.GetChild('task_sub_type_code',ldwc_child)
			ls_filter = "task_type_code = '" + data + "'"
			li_result = ldwc_child.SetFilter(ls_filter)
			li_result = ldwc_child.Filter()
		
		CASE	'task_sub_type_code'
		
/*	Turn tab of task_specific_code column on and filter out the ones not related
	to the chosen task_type_code and task_sub_type_code.
*/
			THIS.SetTabOrder('task_specific_code',78)
			THIS.SetItem(1,'task_specific_code',ls_null_string)
			ls_task_type_code = THIS.GetItemString(1,'task_type_code')
			THIS.GetChild('task_specific_code',ldwc_child)
			ls_filter = "task_type_code = '" + ls_task_type_code + "' AND task_sub_type_code = '" + data + "'"
			li_result = ldwc_child.SetFilter(ls_filter)
			li_result = ldwc_child.Filter()
	END CHOOSE

end event

type cb_clear from commandbutton within w_task_list_report
event clicked pbm_bnclicked
integer x = 2153
integer y = 344
integer width = 389
integer height = 108
integer taborder = 12
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

end event

