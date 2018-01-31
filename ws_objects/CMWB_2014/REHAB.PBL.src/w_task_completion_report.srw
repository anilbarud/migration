$PBExportHeader$w_task_completion_report.srw
forward
global type w_task_completion_report from w_a_report
end type
type cb_ok from commandbutton within w_task_completion_report
end type
type dw_search_criteria from u_dw_online within w_task_completion_report
end type
type cb_clear from commandbutton within w_task_completion_report
end type
end forward

global type w_task_completion_report from w_a_report
cb_ok cb_ok
dw_search_criteria dw_search_criteria
cb_clear cb_clear
end type
global w_task_completion_report w_task_completion_report

type variables

end variables

on w_task_completion_report.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.dw_search_criteria=create dw_search_criteria
this.cb_clear=create cb_clear
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.dw_search_criteria
this.Control[iCurrent+3]=this.cb_clear
end on

on w_task_completion_report.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.dw_search_criteria)
destroy(this.cb_clear)
end on

event open;call super::open;	DATAWINDOWCHILD	ldw_child
	INTEGER				li_result

/*	Make the search criteria datawindow visible.
*/	
	dw_search_criteria.InsertRow(0)
	
/*	Fill up the drop down datawindows in the search criteria datawindow.
*/	
	li_result = dw_search_criteria.GetChild('admin_region_code',ldw_child)
	IF li_result = -1 THEN
		MessageBox("Rehabilitation Planner - Task Completion Report Error","An error occured while opening the report window.")
		Close(THIS)
		RETURN
	ELSE
		ldw_child.SetTransObject(SQLCA)
		ldw_child.Retrieve()
	END IF

	li_result = dw_search_criteria.GetChild('responsible_user_id',ldw_child)
	IF li_result = -1 THEN
		MessageBox("Rehabilitation Planner - Task Completion Report Error","An error occured while opening the report window.")
		Close(THIS)
		RETURN
	ELSE
		ldw_child.SetTransObject(SQLCA)
		ldw_child.Retrieve()
	END IF

/*	Initalize the report datawindows.
*/
	dw_report.SetTransObject(SQLCA)

end event

type dw_report from w_a_report`dw_report within w_task_completion_report
integer height = 1816
integer taborder = 30
string dataobject = "d_task_completion_report"
boolean hscrollbar = true
end type

type cb_ok from commandbutton within w_task_completion_report
event clicked pbm_bnclicked
integer x = 2153
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

event clicked;	LONG		ll_numrows
	STRING	ls_responsible_user_id, ls_admin_region_code, ls_sql_statement
	DATETIME	ldt_from_date, ldt_to_date

	dw_search_criteria.AcceptText()

	ls_responsible_user_id = dw_search_criteria.GetItemString(1,'responsible_user_id')
	ls_admin_region_code = dw_search_criteria.GetItemString(1,'admin_region_code')
	ldt_from_date = dw_search_criteria.GetItemDatetime(1,'from_date')
	ldt_to_date = dw_search_criteria.GetItemDatetime(1,'to_date')

	IF IsNull(ls_responsible_user_id) THEN
		MessageBox("Search Criteria Error","A Case Manager is required and must be selected.")
		RETURN
	END IF

	IF IsNull(ls_admin_region_code) THEN
		MessageBox("Search Criteria Error","An Admin Region is required and must be selected.")
		RETURN
	END IF

	IF IsNull(ldt_from_date) THEN
		MessageBox("Search Criteria Error","A From Date is required and must be entered.")
		RETURN
	END IF

	IF IsNull(ldt_to_date) THEN
		MessageBox("Search Criteria Error","A To Date is required and must be entered.")
		RETURN
	END IF

			
	ll_numrows = dw_report.Retrieve(ls_admin_region_code,ls_responsible_user_id,ldt_from_date,ldt_to_date)
	SQLCA.nf_handle_error("w_task_completion_report","dw_report","cb_ok")


	IF ll_numrows <= 0 THEN
		MessageBox("Rehabilitation Planner - Task List Report","No data was found to satisfy the entered criteria.")
	END IF

end event

type dw_search_criteria from u_dw_online within w_task_completion_report
integer x = 37
integer y = 52
integer width = 1710
integer height = 420
integer taborder = 10
string dataobject = "d_task_completion_report_search_criteria"
boolean border = false
end type

type cb_clear from commandbutton within w_task_completion_report
event clicked pbm_bnclicked
integer x = 2153
integer y = 344
integer width = 389
integer height = 108
integer taborder = 21
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

