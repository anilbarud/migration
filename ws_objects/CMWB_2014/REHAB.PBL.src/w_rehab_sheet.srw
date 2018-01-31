$PBExportHeader$w_rehab_sheet.srw
forward
global type w_rehab_sheet from w_ancestor
end type
type cb_3 from commandbutton within w_rehab_sheet
end type
type p_maximize from picture within w_rehab_sheet
end type
type dw_basic_claim from u_dw_online within w_rehab_sheet
end type
type dw_document_path from u_dw_document_path within w_rehab_sheet
end type
type cb_refresh_document_list from commandbutton within w_rehab_sheet
end type
type dw_documents from u_dw_online within w_rehab_sheet
end type
type tab_rehab_plan from tab within w_rehab_sheet
end type
type tabpage_goals_objectives from userobject within tab_rehab_plan
end type
type dw_goal_list from u_dw_online within tabpage_goals_objectives
end type
type dw_goal from u_dw_online within tabpage_goals_objectives
end type
type cb_add_goal from commandbutton within tabpage_goals_objectives
end type
type dw_objective_list from u_dw_online within tabpage_goals_objectives
end type
type cb_add_objective from commandbutton within tabpage_goals_objectives
end type
type cb_save_goal_objective from commandbutton within tabpage_goals_objectives
end type
type cb_cancel_goal_objective from commandbutton within tabpage_goals_objectives
end type
type dw_objective from u_dw_online within tabpage_goals_objectives
end type
type tabpage_goals_objectives from userobject within tab_rehab_plan
dw_goal_list dw_goal_list
dw_goal dw_goal
cb_add_goal cb_add_goal
dw_objective_list dw_objective_list
cb_add_objective cb_add_objective
cb_save_goal_objective cb_save_goal_objective
cb_cancel_goal_objective cb_cancel_goal_objective
dw_objective dw_objective
end type
type tabpage_task from userobject within tab_rehab_plan
end type
type dw_insert_rehab_task_authorization from u_dw_online within tabpage_task
end type
type cb_add_task from commandbutton within tabpage_task
end type
type cb_save_task from commandbutton within tabpage_task
end type
type cb_cancel_task from commandbutton within tabpage_task
end type
type cb_send from commandbutton within tabpage_task
end type
type cb_attach_doc_tsk from commandbutton within tabpage_task
end type
type dw_task_authorizations from u_dw_online within tabpage_task
end type
type cb_add_follow_up from commandbutton within tabpage_task
end type
type dw_task_list from u_dw_online within tabpage_task
end type
type pb_task_list_maximize from cb_dw_maximize within tabpage_task
end type
type dw_task from u_dw_online within tabpage_task
end type
type dw_external_requestor from u_dw_online within tabpage_task
end type
type tabpage_task from userobject within tab_rehab_plan
dw_insert_rehab_task_authorization dw_insert_rehab_task_authorization
cb_add_task cb_add_task
cb_save_task cb_save_task
cb_cancel_task cb_cancel_task
cb_send cb_send
cb_attach_doc_tsk cb_attach_doc_tsk
dw_task_authorizations dw_task_authorizations
cb_add_follow_up cb_add_follow_up
dw_task_list dw_task_list
pb_task_list_maximize pb_task_list_maximize
dw_task dw_task
dw_external_requestor dw_external_requestor
end type
type tabpage_authorizations from userobject within tab_rehab_plan
end type
type cb_delete from commandbutton within tabpage_authorizations
end type
type dw_authorizations from u_dw_online within tabpage_authorizations
end type
type dw_authorization_task_list from u_dw_online within tabpage_authorizations
end type
type cb_add_authorization from commandbutton within tabpage_authorizations
end type
type dw_task_authorization_list from u_dw_online within tabpage_authorizations
end type
type cb_authorization_save from commandbutton within tabpage_authorizations
end type
type cb_authorization_cancel from commandbutton within tabpage_authorizations
end type
type dw_auth_let_provider_info from u_dw_online within tabpage_authorizations
end type
type dw_auth_let_toll_free_no from u_dw_online within tabpage_authorizations
end type
type tabpage_authorizations from userobject within tab_rehab_plan
cb_delete cb_delete
dw_authorizations dw_authorizations
dw_authorization_task_list dw_authorization_task_list
cb_add_authorization cb_add_authorization
dw_task_authorization_list dw_task_authorization_list
cb_authorization_save cb_authorization_save
cb_authorization_cancel cb_authorization_cancel
dw_auth_let_provider_info dw_auth_let_provider_info
dw_auth_let_toll_free_no dw_auth_let_toll_free_no
end type
type tabpage_progress from userobject within tab_rehab_plan
end type
type dw_progress_note from u_dw_online within tabpage_progress
end type
type dw_rehab_task_attachment from u_dw_online within tabpage_progress
end type
type dw_task_progress from u_dw_online within tabpage_progress
end type
type dw_progress_note_event from u_dw_online within tabpage_progress
end type
type cb_add_progress from commandbutton within tabpage_progress
end type
type dw_progress_task_list from u_dw_online within tabpage_progress
end type
type cb_save_progress from commandbutton within tabpage_progress
end type
type cb_cancel_progress from commandbutton within tabpage_progress
end type
type cb_attach_doc from commandbutton within tabpage_progress
end type
type cb_send_msg from commandbutton within tabpage_progress
end type
type cb_link_event from commandbutton within tabpage_progress
end type
type dw_progress_notes_for_task_list from u_dw_online within tabpage_progress
end type
type tabpage_progress from userobject within tab_rehab_plan
dw_progress_note dw_progress_note
dw_rehab_task_attachment dw_rehab_task_attachment
dw_task_progress dw_task_progress
dw_progress_note_event dw_progress_note_event
cb_add_progress cb_add_progress
dw_progress_task_list dw_progress_task_list
cb_save_progress cb_save_progress
cb_cancel_progress cb_cancel_progress
cb_attach_doc cb_attach_doc
cb_send_msg cb_send_msg
cb_link_event cb_link_event
dw_progress_notes_for_task_list dw_progress_notes_for_task_list
end type
type tabpage_voc_profile from userobject within tab_rehab_plan
end type
type dw_voc_profile from u_dw_online within tabpage_voc_profile
end type
type cb_save_voc_profile from commandbutton within tabpage_voc_profile
end type
type cb_cancel_voc_profile from commandbutton within tabpage_voc_profile
end type
type dw_percent_ppi from u_dw_online within tabpage_voc_profile
end type
type tabpage_voc_profile from userobject within tab_rehab_plan
dw_voc_profile dw_voc_profile
cb_save_voc_profile cb_save_voc_profile
cb_cancel_voc_profile cb_cancel_voc_profile
dw_percent_ppi dw_percent_ppi
end type
type tabpage_case_monitoring from userobject within tab_rehab_plan
end type
type dw_case_monitoring from u_dw_online within tabpage_case_monitoring
end type
type dw_complicating_factors_list from u_dw_online within tabpage_case_monitoring
end type
type st_complication_label from statictext within tabpage_case_monitoring
end type
type dw_complicating_factors from u_dw_online within tabpage_case_monitoring
end type
type cb_add_complicating_factor from commandbutton within tabpage_case_monitoring
end type
type cb_save_case_monitor from commandbutton within tabpage_case_monitoring
end type
type cb_cancel_case_monitor from commandbutton within tabpage_case_monitoring
end type
type dw_working_status_change_list from u_dw_online within tabpage_case_monitoring
end type
type dw_medical_func_status_change_list from u_dw_online within tabpage_case_monitoring
end type
type st_working_status_change from statictext within tabpage_case_monitoring
end type
type st_medical_func_stauts_change from statictext within tabpage_case_monitoring
end type
type gb_1 from groupbox within tabpage_case_monitoring
end type
type tabpage_case_monitoring from userobject within tab_rehab_plan
dw_case_monitoring dw_case_monitoring
dw_complicating_factors_list dw_complicating_factors_list
st_complication_label st_complication_label
dw_complicating_factors dw_complicating_factors
cb_add_complicating_factor cb_add_complicating_factor
cb_save_case_monitor cb_save_case_monitor
cb_cancel_case_monitor cb_cancel_case_monitor
dw_working_status_change_list dw_working_status_change_list
dw_medical_func_status_change_list dw_medical_func_status_change_list
st_working_status_change st_working_status_change
st_medical_func_stauts_change st_medical_func_stauts_change
gb_1 gb_1
end type
type tabpage_action_item from userobject within tab_rehab_plan
end type
type gb_2 from groupbox within tabpage_action_item
end type
type cb_add_progress_note from commandbutton within tabpage_action_item
end type
type cb_cancel from commandbutton within tabpage_action_item
end type
type cb_save from commandbutton within tabpage_action_item
end type
type dw_action_list from u_dw_online within tabpage_action_item
end type
type dw_progress_notes_list from u_dw_online within tabpage_action_item
end type
type pb_progress_list_maximize from cb_dw_maximize within tabpage_action_item
end type
type pb_max from cb_dw_maximize within tabpage_action_item
end type
type dw_action_item from u_dw_online within tabpage_action_item
end type
type cb_add_action_item from commandbutton within tabpage_action_item
end type
type gb_3 from groupbox within tabpage_action_item
end type
type dw_action_item_progress_note from u_dw_online within tabpage_action_item
end type
type tabpage_action_item from userobject within tab_rehab_plan
gb_2 gb_2
cb_add_progress_note cb_add_progress_note
cb_cancel cb_cancel
cb_save cb_save
dw_action_list dw_action_list
dw_progress_notes_list dw_progress_notes_list
pb_progress_list_maximize pb_progress_list_maximize
pb_max pb_max
dw_action_item dw_action_item
cb_add_action_item cb_add_action_item
gb_3 gb_3
dw_action_item_progress_note dw_action_item_progress_note
end type
type tab_rehab_plan from tab within w_rehab_sheet
tabpage_goals_objectives tabpage_goals_objectives
tabpage_task tabpage_task
tabpage_authorizations tabpage_authorizations
tabpage_progress tabpage_progress
tabpage_voc_profile tabpage_voc_profile
tabpage_case_monitoring tabpage_case_monitoring
tabpage_action_item tabpage_action_item
end type
type tab_rehab_plan_viewer from tab within w_rehab_sheet
end type
type tabpage_tasks_and_progress from userobject within tab_rehab_plan_viewer
end type
type dw_tasks_and_progress from u_dw_online within tabpage_tasks_and_progress
end type
type cb_refresh_viewer from commandbutton within tabpage_tasks_and_progress
end type
type cb_print_report1 from commandbutton within tabpage_tasks_and_progress
end type
type cb_attachments from commandbutton within tabpage_tasks_and_progress
end type
type dw_attachment_list from u_dw_online within tabpage_tasks_and_progress
end type
type cb_task_attachments from commandbutton within tabpage_tasks_and_progress
end type
type cb_1 from commandbutton within tabpage_tasks_and_progress
end type
type cb_2 from commandbutton within tabpage_tasks_and_progress
end type
type dw_rehab_viewer_report from u_dw_online within tabpage_tasks_and_progress
end type
type tabpage_tasks_and_progress from userobject within tab_rehab_plan_viewer
dw_tasks_and_progress dw_tasks_and_progress
cb_refresh_viewer cb_refresh_viewer
cb_print_report1 cb_print_report1
cb_attachments cb_attachments
dw_attachment_list dw_attachment_list
cb_task_attachments cb_task_attachments
cb_1 cb_1
cb_2 cb_2
dw_rehab_viewer_report dw_rehab_viewer_report
end type
type tabpage_goal_history from userobject within tab_rehab_plan_viewer
end type
type dw_goal_history from u_dw_online within tabpage_goal_history
end type
type tabpage_goal_history from userobject within tab_rehab_plan_viewer
dw_goal_history dw_goal_history
end type
type tabpage_authorizations_viewer from userobject within tab_rehab_plan_viewer
end type
type dw_rehab_viewer_authorizations from u_dw_online within tabpage_authorizations_viewer
end type
type cb_print_auth_rpt from commandbutton within tabpage_authorizations_viewer
end type
type tabpage_authorizations_viewer from userobject within tab_rehab_plan_viewer
dw_rehab_viewer_authorizations dw_rehab_viewer_authorizations
cb_print_auth_rpt cb_print_auth_rpt
end type
type tab_rehab_plan_viewer from tab within w_rehab_sheet
tabpage_tasks_and_progress tabpage_tasks_and_progress
tabpage_goal_history tabpage_goal_history
tabpage_authorizations_viewer tabpage_authorizations_viewer
end type
type st_save from statictext within w_rehab_sheet
end type
type uo_image_append from u_image_append within w_rehab_sheet
end type
type st_1 from statictext within w_rehab_sheet
end type
type st_2 from statictext within w_rehab_sheet
end type
type st_week_no from statictext within w_rehab_sheet
end type
end forward

global type w_rehab_sheet from w_ancestor
integer x = 46
integer y = 40
integer width = 5179
integer height = 3308
string title = "Rehab Plan"
string menuname = "m_rehab_plan"
boolean maxbox = false
windowtype windowtype = main!
long backcolor = 67108864
event ue_print pbm_custom50
event ue_postopen ( )
cb_3 cb_3
p_maximize p_maximize
dw_basic_claim dw_basic_claim
dw_document_path dw_document_path
cb_refresh_document_list cb_refresh_document_list
dw_documents dw_documents
tab_rehab_plan tab_rehab_plan
tab_rehab_plan_viewer tab_rehab_plan_viewer
st_save st_save
uo_image_append uo_image_append
st_1 st_1
st_2 st_2
st_week_no st_week_no
end type
global w_rehab_sheet w_rehab_sheet

type variables
W_SHEET		iw_passedwindow
s_window_message 	istr_window_message
long			il_document_row_number
BOOLEAN		ib_explodewasgood,ib_document_found
BOOLEAN 		ib_do_tab_selection
BOOLEAN		ib_fire_event_with_sync
BOOLEAN		ib_note
string                    		is_image_status
u_dw_document_path 	iu_dw_document_path

integer			ii_oldsheetwidth
INTEGER           ii_oldsheetx
integer			ii_oldsheetheight
INTEGER           ii_oldsheety

integer			ii_frameheight
integer			ii_framewidth
integer			ii_framex
integer			ii_framey
LONG			il_oldcontrolx[]

// The following are instance variables for each of the
// child windows that can be open on the sheet

// Declare the menu instance of this sheet
m_rehab_plan 		im_menu

N_TASK 			inv_tasks
N_REHAB_PROGRESS 	inv_rehab_progress
N_GOALS_OBJECTIVES 	inv_goals_objectives
N_AUTHORIZATIONS 	inv_authorizations
N_VOC_PROFILE		inv_voc_profile
N_CASE_MONITOR	inv_case_monitor
N_ACTION_ITEM			inv_action_item
//N_ACTION_ITEM_PROGRESS 		inv_action_item_progress


LONG                   		il_claim_no, il_task_no
w_document_details   	iw_document_details
n_cst_remote_print	inv_remote_print

BOOLEAN                               ib_new_task
BOOLEAN		ib_new_authorization

LONG                                      il_cRow

STRING			is_dropped_column_name
s_filter_state		str_state


BOOLEAN		ib_added_followup_from_task_tab = False
BOOLEAN		ib_allow_add_follow_up_from_task = False
BOOLEAN		ib_action_item_note_has_focus = False

//These booleans are used to delay the retrieve of datawindows until the datawindow
//is actually visible.
BOOLEAN		ib_delayed_task_tab_retrieve
BOOLEAN		ib_delayed_progress_tab_retrieve
BOOLEAN		ib_delayed_authorization_tab_retrieve
BOOLEAN		ib_delayed_goal_history_retrieve
BOOLEAN		ib_delayed_authorized_items_retrieve

//This boolean is used to see if retrieve of dw is being called from the Print
//button on Tasks/Progress tab.
BOOLEAN		ib_print

n_rehab_tab_controller		inv_tab_controller

DATE			idt_ephysio_br_check_date

DATASTORE ids_tasks_and_progress_print

LONG           il_sheet_handle

end variables

forward prototypes
public function integer wf_retrieve_goal_objective_lists ()
public subroutine wf_save_indicator ()
public function integer wf_accident_request ()
public function string wf_get_claimant_full_name ()
public subroutine wf_clear_rehab_sheet ()
public function integer wf_retrieve_action_item_list ()
public function integer wf_retrieve_progress_list ()
public function integer wf_retrieve_progress_note ()
public function integer wf_add_action_item (integer ai_related_task_no)
public subroutine wf_retrieve_action_item ()
public function integer wf_set_week_no ()
public function integer wf_enable_add_follow_up_button ()
public function integer wf_read_only ()
public function integer wf_retrieve_task_list ()
public subroutine wf_enable_action_item_controls (boolean ab_changes_pending)
public function integer wf_retrieve_task_details ()
public function integer wf_retrieve_auth_details ()
public function integer wf_retrieve_progress_details ()
public function integer wf_rehab_viewer_retrieve (integer ai_current_tab, long al_task_no)
public subroutine f_adjust_text (string as_tabpage)
public subroutine wf_display_caution_flag ()
public function integer wf_new_claim_no_refresh ()
public subroutine wf_reset_screen_sizes ()
public function integer wf_explode_frame_and_set_sheets ()
public function integer wf_retrieve_author_task_list (long al_authorization_no)
public function integer wf_call_function (string as_action)
public function integer wf_full_event_comments ()
public function long wf_get_sheet_handle ()
end prototypes

event ue_print;call super::ue_print;/*	This event is triggered by the print menu option.
*/
	tab_rehab_plan_viewer.tabpage_tasks_and_progress.cb_print_report1.TriggerEvent(Clicked!)
end event

event ue_postopen();INTEGER						li_result
LONG							ll_rows, ll_claim_no
U_DWA							ldw_dw[]	, ldw_null[]		
DATAWINDOWCHILD		ldwc_phone, ldwc_phone_rehab
STRING ls_filter

ids_tasks_and_progress_print = CREATE DATASTORE
ids_tasks_and_progress_print.DataObject = 'd_rehab_plan_viewer_task_progress_print'

inv_tab_controller = create n_rehab_tab_controller

ib_do_tab_selection = False
	
//Set up the tab controller
inv_tab_controller.idw_task_list 						= tab_rehab_plan.tabpage_task.dw_task_list
inv_tab_controller.idw_viewer 						= tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress
inv_tab_controller.idw_authorization_task_list 	= tab_rehab_plan.tabpage_authorizations.dw_authorization_task_list
inv_tab_controller.idw_task_note_list 				= tab_rehab_plan.tabpage_progress.dw_progress_task_list
inv_tab_controller.itb_rehab_tab 					= tab_rehab_plan
	
/*	Set up the tombstone and document list datawindows so that they are shared with the
	worksheet's tombstone and document list.
*/
THIS.dw_basic_claim.SetTransObject(SQLCA)
/*MA000705-START*/
iw_passedwindow.dw_basic_claim.getchild("phone",ldwc_phone)
This.dw_basic_claim.getchild("phone",ldwc_phone_rehab)
ldwc_phone_rehab.settransobject(sqlca)
ldwc_phone.ShareData(ldwc_phone_rehab)
/*MA000705-END*/
		
THIS.dw_documents.SetTransObject(ImageTrans)
iw_passedwindow.dw_basic_claim.ShareData(THIS.dw_basic_claim)
iw_passedwindow.dw_documents.ShareData(THIS.dw_documents)


IF iw_passedwindow.dw_documents.filteredcount() > 0 THEN
	THIS.dw_documents.object.t_filtered_indicator.visible = 1
END IF
	
tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_attachment_list.SetTransObject(SQLCA)
	
/*	VOCATIONAL PROFILE.
*/
tab_rehab_plan.tabpage_voc_profile.dw_voc_profile.SetTransObject(SQLCA)
tab_rehab_plan.tabpage_voc_profile.dw_percent_ppi.SetTransObject(SQLCA)

/*	Create the nonvisual user object for Voc Profile tab
*/
inv_voc_profile = Create n_voc_profile
inv_voc_profile.nf_set_window_parent(THIS)
	
/* Load the updateable datawindows into the array
*/
ldw_dw[1] = tab_rehab_plan.tabpage_voc_profile.dw_voc_profile
	
/*	Call the function to initialize the Voc Profile datawindow for data entry.
*/
inv_voc_profile.nf_init(ldw_dw[],SQLCA,THIS)
inv_voc_profile.nf_set_commit(TRUE)

/*	AUTHORIZATIONS.
*/
tab_rehab_plan.tabpage_authorizations.dw_authorization_task_list.SetTransObject(SQLCA)
tab_rehab_plan.tabpage_authorizations.dw_task_authorization_list.SetTransObject(SQLCA)
tab_rehab_plan.tabpage_authorizations.dw_auth_let_provider_info.SetTransObject(SQLCA)
tab_rehab_plan.tabpage_authorizations.dw_auth_let_toll_free_no.SetTransObject(SQLCA)

/*	Create the nonvisual user object for Authorization tab
*/
inv_authorizations = Create n_authorizations
inv_authorizations.nf_set_window_parent(THIS)
	
/* Load the updateable datawindows into the array
*/
ldw_dw[1] = tab_rehab_plan.tabpage_authorizations.dw_authorizations //REHAB_TASK_AUTHORIZATION
ldw_dw[2] = tab_rehab_plan.tabpage_authorizations.dw_authorization_task_list//REHAB_TASK

/*	Call the function to initialize the Authorization datawindows for data entry.
*/
inv_authorizations.nf_init(ldw_dw[],SQLCA,THIS)
inv_authorizations.nf_set_commit(TRUE)

/*	CASE MONITORING.
*/
tab_rehab_plan.tabpage_case_monitoring.dw_case_monitoring.SetTransObject(SQLCA)
tab_rehab_plan.tabpage_case_monitoring.dw_complicating_factors.SetTransObject(SQLCA)
tab_rehab_plan.tabpage_case_monitoring.dw_complicating_factors_list.SetTransObject(SQLCA)
tab_rehab_plan.tabpage_case_monitoring.dw_working_status_change_list.SetTransObject(SQLCA)
tab_rehab_plan.tabpage_case_monitoring.dw_medical_func_status_change_list.SetTransObject(SQLCA)

/*	Create the nonvisual user object for Case Monitoring tab.
*/
inv_case_monitor = Create n_case_monitor
inv_case_monitor.nf_set_window_parent(THIS)
	
/* Load the updateable datawindows into the array.
*/
ldw_dw[1] = tab_rehab_plan.tabpage_case_monitoring.dw_case_monitoring
ldw_dw[2] = tab_rehab_plan.tabpage_case_monitoring.dw_complicating_factors
ldw_dw[3] = tab_rehab_plan.tabpage_case_monitoring.dw_working_status_change_list
ldw_dw[4] = tab_rehab_plan.tabpage_case_monitoring.dw_medical_func_status_change_list

/*	Call the function to initialize the Case Monitoring datawindow for data entry.
*/
inv_case_monitor.nf_init(ldw_dw[],SQLCA,THIS)
inv_case_monitor.nf_set_commit(TRUE)

/*	GOALS AND OBJECTIVES. 
*/
inv_goals_objectives = Create n_goals_objectives
inv_goals_objectives.nf_set_window_parent(THIS)
	
/*	Initialize the array with updateable datawindows and call the function to initialize them. 
*/
ldw_dw[1] = tab_rehab_plan.tabpage_goals_objectives.dw_goal
ldw_dw[2] = tab_rehab_plan.tabpage_goals_objectives.dw_objective
inv_goals_objectives.nf_init(ldw_dw[],SQLCA,THIS)
inv_goals_objectives.nf_set_commit(TRUE)

tab_rehab_plan.tabpage_goals_objectives.dw_goal_list.SetTransObject(SQLCA)
tab_rehab_plan.tabpage_goals_objectives.dw_objective_list.SetTransObject(SQLCA)

/*	TASKS.
*/
tab_rehab_plan.tabpage_task.dw_task_list.SetTransObject(SQLCA)
tab_rehab_plan.tabpage_task.dw_task_list.ShareData(tab_rehab_plan.tabpage_authorizations.dw_authorization_task_list)
tab_rehab_plan.tabpage_task.dw_task_list.ShareData(tab_rehab_plan.tabpage_progress.dw_progress_task_list)

inv_tasks = Create n_task
inv_tasks.nf_set_window_parent(THIS)

ldw_dw[1] = tab_rehab_plan.tabpage_task.dw_task
ldw_dw[2] = tab_rehab_plan.tabpage_task.dw_external_requestor
ldw_dw[3] = tab_rehab_plan.tabpage_task.dw_task_authorizations

inv_tasks.nf_init(ldw_dw[],SQLCA,THIS)
inv_tasks.nf_set_commit(TRUE)
	
tab_rehab_plan.tabpage_task.pb_task_list_maximize.uf_set_max_position(0,12,1664,2629)	
	
/*	ACTION ITEMS 
*/
inv_action_item = Create n_action_item
	
ldw_dw = ldw_null
ldw_dw[1] = tab_rehab_plan.tabpage_action_item.dw_action_item
ldw_dw[2] = tab_rehab_plan.tabpage_action_item.dw_action_item_progress_note
inv_action_item.nf_init(ldw_dw[],SQLCA,THIS)
	
//Get a reference to all the child datawindows for use
//in filtering and to get the lookup flags(ie. reset_allowed_flag, ect..)
inv_action_item.nf_load_child_datawindows()
inv_action_item.nf_set_commit(True)
	
//Set the columns protect expressions.
inv_action_item.nf_set_protect_expressions()
inv_action_item.nf_set_visible_expressions()

tab_rehab_plan.tabpage_action_item.dw_action_list.uf_setsort(True)
	
//Set the default filter
tab_rehab_plan.tabpage_action_item.dw_action_list.triggerevent("ue_filter")

tab_rehab_plan.tabpage_action_item.dw_action_item.SetTransObject(SQLCA)
tab_rehab_plan.tabpage_action_item.dw_action_list.SetTransObject(SQLCA)
tab_rehab_plan.tabpage_action_item.dw_progress_notes_list.SetTransobject(SQLCA)
tab_rehab_plan.tabpage_action_item.dw_action_item_progress_note.SetTransobject(SQLCA)
	
tab_rehab_plan.tabpage_action_item.pb_progress_list_maximize.uf_set_max_position(0,12,1664,2629)
tab_rehab_plan.tabpage_action_item.pb_max.uf_set_max_position(0,12,1664,2629)	
	
//Only Case Managers can maintain information on the Action Item Tab
If vgst_user_profile.maintain_action_item_flag = 'N' Then
	tab_rehab_plan.tabpage_action_item.dw_action_item.enabled 			= False
	tab_rehab_plan.tabpage_action_item.dw_action_item.uf_set_backcolor()
	tab_rehab_plan.tabpage_action_item.cb_add_action_item.enabled 		= False
	tab_rehab_plan.tabpage_action_item.dw_action_item_progress_note.enabled = False
	tab_rehab_plan.tabpage_action_item.dw_action_item_progress_note.uf_set_backcolor()
	tab_rehab_plan.tabpage_action_item.cb_add_progress_note.enabled 	= False
End if
	
IF vgst_user_profile.maintain_action_item_flag = 'N' or istr_window_message.as_mode = 'READ' Then
	ib_allow_add_follow_up_from_task = False
Else
	ib_allow_add_follow_up_from_task = True
End if
		
/*	PROGRESS.
*/
tab_rehab_plan.tabpage_progress.dw_progress_task_list.SetTransObject(SQLCA)
tab_rehab_plan.tabpage_progress.dw_task_progress.SetTransObject(SQLCA)
tab_rehab_plan.tabpage_progress.dw_progress_note_event.SetTransObject(SQLCA)
tab_rehab_plan.tabpage_progress.dw_progress_note.SetTransObject(SQLCA)
tab_rehab_plan.tabpage_progress.dw_rehab_task_attachment.SetTransObject(SQLCA)
tab_rehab_plan.tabpage_progress.dw_progress_notes_for_task_list.SetTransObject(SQLCA)

/*	Create the nonvisual user object for Progress tab
*/
inv_rehab_progress = Create n_rehab_progress
inv_rehab_progress.nf_set_window_parent(THIS)
	
/* Load the updateable datawindows into the array
*/
ldw_dw[1] = tab_rehab_plan.tabpage_progress.dw_task_progress
ldw_dw[2] = tab_rehab_plan.tabpage_progress.dw_progress_note_event
ldw_dw[3] = tab_rehab_plan.tabpage_progress.dw_progress_note
ldw_dw[4] = tab_rehab_plan.tabpage_progress.dw_rehab_task_attachment

/*	Call the function to initialize the Task Progress datawindows for data entry.
*/
inv_rehab_progress.nf_init(ldw_dw[],SQLCA,THIS)
inv_rehab_progress.nf_set_commit(TRUE)
	
/*	VIEWER.
*/
/*	Set the lists on each tab so that 1 row is selected at a time.
*/
tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.SetTransObject(SQLCA)
tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_rehab_viewer_report.SetTransObject(SQLCA)
tab_rehab_plan_viewer.tabpage_goal_history.dw_goal_history.SetTransObject(SQLCA)
tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.uf_setselect(1)

ids_tasks_and_progress_print.SetTransObject(SQLCA)

/* rehab viewer authorization tab added Jun 1/98
*/
tab_rehab_plan_viewer.tabpage_authorizations_viewer.dw_rehab_viewer_authorizations.SetTransObject(SQLCA)

/* retrieve info. for viewer tab pages
*/	
li_result = wf_new_claim_no_refresh()
IF li_result < 0 THEN
	Close(THIS)
	RETURN
END IF
	
wf_display_caution_flag()
	
IF istr_window_message.as_mode = 'READ' THEN
	wf_read_only()
END IF
		
ib_do_tab_selection = True

dw_documents.SelectRow(1,TRUE)

// set it to the task tab - they do 99% of their work on it.
IF tab_rehab_plan.tabpage_goals_objectives.dw_goal_list.rowcount() > 0 THEN
	tab_rehab_plan.SelectedTab = 2
ELSE
	tab_rehab_plan.SelectedTab = 1
END IF
end event

public function integer wf_retrieve_goal_objective_lists ();LONG	ll_list_row_count, ll_current_row

ll_current_row = tab_rehab_plan.tabpage_goals_objectives.dw_goal_list.GetRow()
ll_list_row_count = tab_rehab_plan.tabpage_goals_objectives.dw_goal_list.Retrieve(il_claim_no)
IF ll_list_row_count > 0 AND (ll_current_row = 0 OR ll_current_row = 1) THEN  // if current row was 0 or 1 then the 
																								      // rowfocuschanged event may not have fired
	tab_rehab_plan.tabpage_goals_objectives.dw_goal_list.SetRow(1)
	tab_rehab_plan.tabpage_goals_objectives.dw_goal_list.uf_processselect(1,'KeyBoard')
	tab_rehab_plan.tabpage_goals_objectives.dw_goal_list.TriggerEvent(RowFocusChanged!)
END IF

RETURN ll_list_row_count



end function

public subroutine wf_save_indicator ();LONG	ll_ans

	IF tab_rehab_plan.tabpage_authorizations.cb_authorization_save.enabled OR &
		tab_rehab_plan.tabpage_case_monitoring.cb_save_case_monitor.enabled OR &
		tab_rehab_plan.tabpage_goals_objectives.cb_save_goal_objective.enabled OR &
		tab_rehab_plan.tabpage_progress.cb_save_progress.enabled OR &
		tab_rehab_plan.tabpage_task.cb_save_task.enabled OR &
		tab_rehab_plan.tabpage_action_item.cb_save.enabled OR &
		tab_rehab_plan.tabpage_voc_profile.cb_save_voc_profile.enabled THEN
		
		st_save.visible = TRUE
	ELSE
		st_save.visible = FALSE
	END IF
end subroutine

public function integer wf_accident_request ();// Define Variables
LONG			ll_claim_no
INTEGER		li_check_request, li_return = 1
						
// Is "12 month pre-accident earnings request" required (Yes/No). 

li_check_request = MessageBox('Automate','Is 12 month Pre-Accident earnings request required', Question!, YesNo!)

// IF "Yes" THEN
IF li_check_request = 1 THEN

	ll_claim_no = il_claim_no

	//	Create the nonvisual user object for Remote Print.
	If NOT (isvalid(inv_remote_print)) Then 
		inv_remote_print = Create n_cst_remote_print
		If NOT (isvalid(inv_remote_print)) Then
			li_return = -1
		End If
	End If
	
	If li_Return = 1 Then
		IF inv_remote_print.of_init() <> 1 THEN
			MessageBox("Error in of_init()","Cannot Create Letter for Remote Print.")
			li_RETURN  = -1
		END IF
	End If
	
	If li_Return = 1 Then
		IF inv_remote_print.of_Set_Mail_Package_Values(ll_claim_no, 'O', 'PE', 'LC', 'Pre-Accident Earning', 'B') <> 1 THEN
			MessageBox("Error in of_Set_Mail_Package_Values()","Cannot Create Letter for Remote Print.")
			li_return = -1
		END IF
	End If
	
	
	SQLCA.nf_begin_transaction()
	
	If li_Return = 1 Then
		IF inv_remote_print.of_Insert_into_mail_package_table() <> 1 THEN
			MessageBox("Error in of_Insert_into_mail_package_table()","Cannot Create Letter for Remote Print.")
			li_return = -1
		ELSE				
			SQLCA.nf_commit_transaction()
		END IF
	End If
END IF
	
If isValid(inv_remote_print) Then DESTROY inv_remote_print

RETURN li_Return

end function

public function string wf_get_claimant_full_name ();/*  This function retrieves given & last names from INDIVIDUAL & concatenates the strings into one
'full name' string, which is returned
*/

STRING	ls_full_name = " ", ls_given_names, ls_last_name
INT		li_return

SELECT given_names, last_name
INTO :ls_given_names, :ls_last_name
FROM INDIVIDUAL, CLAIM
WHERE INDIVIDUAL.individual_no = CLAIM.individual_no
AND claim_no = :il_claim_no
USING SQLCA;

li_return = SQLCA.nf_handle_error("Embedded SQL SELECT on INDIVIDUAL & CLAIM tables ","w_rehab_sheet", &
												"on wf_get_claimant_full_name")
IF li_return < 0 THEN
	RETURN 'error'
ELSE
	ls_full_name = TRIM(ls_given_names) + " " + &
							TRIM(ls_last_name)
END IF

RETURN ls_full_name

end function

public subroutine wf_clear_rehab_sheet ();/*	This function is used to clear the rehab sheet and disable all tabs when an invalid
	claim number is entered in the claim search on the worksheet.
*/

/*	Clear out all datawindows.
*/
	dw_basic_claim.Reset()
	dw_documents.Reset()

	tab_rehab_plan.tabpage_goals_objectives.dw_goal_list.Reset()
	tab_rehab_plan.tabpage_goals_objectives.dw_goal.Reset()
	tab_rehab_plan.tabpage_goals_objectives.dw_objective_list.Reset()
	tab_rehab_plan.tabpage_goals_objectives.dw_objective.Reset()

	tab_rehab_plan_viewer.tabpage_goal_history.dw_goal_history.Reset()
	tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.Reset()
	tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_attachment_list.Reset()
			
	tab_rehab_plan.tabpage_task.dw_external_requestor.Reset()
	tab_rehab_plan.tabpage_task.dw_task_list.Reset()
	tab_rehab_plan.tabpage_task.dw_task.Reset()
	
	tab_rehab_plan.tabpage_authorizations.dw_authorization_task_list.Reset()
	tab_rehab_plan.tabpage_authorizations.dw_task_authorization_list.Reset()
	tab_rehab_plan.tabpage_authorizations.dw_authorizations.Reset()
	
	tab_rehab_plan.tabpage_progress.dw_progress_task_list.Reset()
	tab_rehab_plan.tabpage_progress.dw_task_progress.Reset()
	tab_rehab_plan.tabpage_progress.dw_progress_note_event.Reset()
	tab_rehab_plan.tabpage_progress.dw_progress_note.Reset()
	tab_rehab_plan.tabpage_progress.dw_rehab_task_attachment.Reset()
	tab_rehab_plan.tabpage_progress.dw_progress_notes_for_task_list.Reset()
	
	tab_rehab_plan.tabpage_voc_profile.dw_voc_profile.Reset()
	
	tab_rehab_plan.tabpage_case_monitoring.dw_case_monitoring.Reset()
	tab_rehab_plan.tabpage_case_monitoring.dw_complicating_factors_list.Reset()
			
/*	Disable buttons.
*/
	dw_documents.Enabled					= False
	cb_refresh_document_list.Enabled	= False
	p_maximize.Enabled			= False
	
/*	Disable all tabs.
*/	
	tab_rehab_plan.tabpage_goals_objectives.Enabled = FALSE
	tab_rehab_plan.tabpage_task.Enabled = FALSE
	tab_rehab_plan.tabpage_authorizations.Enabled = FALSE
	tab_rehab_plan.tabpage_progress.Enabled = FALSE
	tab_rehab_plan.tabpage_voc_profile.Enabled = FALSE
	tab_rehab_plan.tabpage_case_monitoring.Enabled = FALSE
	
	tab_rehab_plan_viewer.tabpage_goal_history.Enabled = FALSE
	tab_rehab_plan_viewer.tabpage_tasks_and_progress.Enabled = FALSE
	/*  Authorization Tabpage added June 1/98
	*/
	tab_rehab_plan_viewer.tabpage_authorizations_viewer.dw_rehab_viewer_authorizations.Enabled=FALSE
			
/*	Reset the title bar for the rehab sheet.
*/
	w_rehab_sheet.Title = "Rehab Sheet"

	RETURN

end subroutine

public function integer wf_retrieve_action_item_list ();String		ls_filter
Datawindow	ldw_action_list
LONG			ll_action_item_rows


ldw_action_list = tab_rehab_plan.tabpage_action_item.dw_action_list
//Retrieve the action items
ldw_action_list.Reset()
ll_action_item_rows = ldw_action_list.Retrieve(il_claim_no)

SQLCA.nf_handle_error('w_rehab_sheet','wf_retrieve_action_item_list','dw_action_list.Retrieve(il_claim_no)')



return ldw_action_list.RowCount()
end function

public function integer wf_retrieve_progress_list ();LONG					ll_rows
LONG					ll_task_no
DATAWINDOW			ldw_action_progress_list

ldw_action_progress_list = tab_rehab_plan.tabpage_action_item.dw_progress_notes_list

/*	If there are zero rows in the action item details then we need to reset the progress list
	and the progress note details
*/
IF tab_rehab_plan.tabpage_action_item.dw_action_item.RowCount() = 0 THEN
	ldw_action_progress_list.Reset()
	tab_rehab_plan.tabpage_action_item.dw_action_item_progress_note.Reset()
else
	ll_task_no = tab_rehab_plan.tabpage_action_item.dw_action_item.GetItemNumber(1,'task_no')
	
	ldw_action_progress_list.Reset()
	ll_rows = ldw_action_progress_list.Retrieve(il_claim_no,ll_task_no)
	If ll_rows = 0 Then
		tab_rehab_plan.tabpage_action_item.dw_action_item_progress_note.Reset()
	End if
End if

return 1
end function

public function integer wf_retrieve_progress_note ();DataWindow		ldw_dw,	ldw_progress_list
LONG				ll_event_no,	ll_task_no
STRING				ls_comment,	ls_combined_comment

ldw_progress_list 	= tab_rehab_plan.tabpage_action_item.dw_progress_notes_list
ldw_dw 				= tab_rehab_plan.tabpage_action_item.dw_action_item_progress_note

ldw_dw.reset()
//If there isn't a note in the list then return since there is nothing else to do
IF ldw_progress_list.RowCount() = 0 THEN RETURN 0

ll_event_no 	= ldw_progress_list.GetItemNumber(ldw_progress_list.GetRow(),'event_no')
ll_task_no 		= ldw_progress_list.GetItemNumber(ldw_progress_list.GetRow(),'task_no')

ldw_dw.Retrieve(il_claim_no,ll_event_no,ll_task_no)
SQLCA.nf_handle_error('w_rehab_plan','wf_retrieve_progress_note','Error retrieving progress note')

ls_comment = ldw_dw.GetItemString(1,'event_comment')

IF IsNull(ls_comment ) THEN ls_comment = ''
ls_combined_comment = f_clean_string_4(ls_comment)
ldw_dw.SetItem(1,'combined_comment',TRIM(ls_combined_comment))
ldw_dw.SetItemStatus(1,'combined_comment',Primary!,NotModified!)

RETURN 1
end function

public function integer wf_add_action_item (integer ai_related_task_no);LONG		ll_opening_no
integer	li_rtn = 1

If inv_action_item.ib_claim_case_managed Then
	
	ll_opening_no = inv_action_item.nf_get_opening_no(False)
	
	
	If ll_opening_no > 0 THen
		//This variable will be checked in the save and cancel on the action item tab
		//If it is True then focus will be set back to the task tab because that
		//is where the function was called from.
		If tab_rehab_plan.SelectedTab = 2 THen
			ib_added_followup_from_task_tab = True
			tab_rehab_plan.SelectTab(7)
		Else
			ib_added_followup_from_task_tab = False
		End if
		//Reset the datawindow so on insert, we will only have one record
		tab_rehab_plan.tabpage_action_item.dw_action_item.Reset()
		
		inv_action_item.nf_insert(ai_related_task_no)
		
		wf_retrieve_progress_list()
	Else
		MessageBox('Insert validation','There are no Openings to link to.  New action items cannot be created.')
		li_rtn =  -1
	End if	
Else
	MessageBox('Insert validation','The claim must be case managed before any action items can be associated with it.')
	li_rtn =  -1
End if

tab_rehab_plan.tabpage_action_item.dw_action_item.SetFocus()
tab_rehab_plan.tabpage_action_item.dw_action_item.SetColumn('comment')


return li_rtn
end function

public subroutine wf_retrieve_action_item ();LONG			ll_task_no
LONG			ll_opening_no
Datawindow	ldw_action_list
DataWindow	ldw_action_details
DateTime		ldt_status_date
DateTime		ldt_disablement_date
INTEGER		li_week

SetRedraw(False)
ldw_action_list = tab_rehab_plan.tabpage_action_item.dw_action_list
ldw_action_details = tab_rehab_plan.tabpage_action_item.dw_action_item

ldw_action_details.ReSet()

If ldw_action_list.RowCount() <> 0 Then	
	//Retrieve the action item that corresponds to the action item selected in the list
	ll_task_no = ldw_action_list.GetItemNumber(ldw_action_list.GetRow(),'task_no')
	ldw_action_details.Retrieve(il_claim_no,ll_task_no)
	
	SQLCA.nf_handle_error('w_rehab_plan','wf_retrieve_action_item','Error retrieving action item')
	
	inv_action_item.nf_set_computed_date()
	
	ll_opening_no = ldw_action_details.GetItemNumber(1,'opening_no')
	ldt_disablement_date = inv_action_item.nf_get_disablement_date(ll_opening_no)
	
	ldw_action_details.SetItem(1,'disablement_date',ldt_disablement_date)
	ldw_action_details.SetItemStatus(1,'disablement_date',Primary!,NotModified!)
	
	//After setting dummy fields,set the column and row back to NotModified!
	//so an update is not issued eroneously
	ldw_action_details.SetItemStatus(1,0,Primary!,NotModified!)
	
	
End if

SetRedraw(True)
end subroutine

public function integer wf_set_week_no ();LONG			ll_opening_no
DATETIME		ldt_disablement_date
integer		li_week_no
LONG			ll_days_after


ll_opening_no = inv_action_item.nf_get_opening_no(False)

iF ll_opening_no > 0 THen
	ldt_disablement_date = inv_action_item.nf_get_disablement_date(ll_opening_no)
	//li_week_no = DaysAfter(Date(ldt_disablement_date), Date(f_server_datetime()))/7 + 1
	ll_days_after = DaysAfter(Date(ldt_disablement_date), Date(f_server_datetime()))
	IF ll_days_after < 0 Then
		li_week_no = ll_days_after / 7 -1
	Else
		li_week_no = ll_days_after / 7 + 1
	End if
	st_week_no.text = String(li_week_no)
Else
	st_week_no.text = ''
ENd if

return 1
end function

public function integer wf_enable_add_follow_up_button ();STRING		ls_medical_management_flag
LONG			ll_row

IF tab_rehab_plan.tabpage_task.dw_task_list.RowCount() > 0 Then
	ll_row = tab_rehab_plan.tabpage_task.dw_task_list.GetRow()
	ls_medical_management_flag = tab_rehab_plan.tabpage_task.dw_task_list.GetItemString(ll_row,'medical_management_flag')
	
	If ls_medical_management_flag = 'Y' Then
		tab_rehab_plan.tabpage_task.cb_add_follow_up.enabled = True
	Else
		tab_rehab_plan.tabpage_task.cb_add_follow_up.enabled = False
	End if
End if

return 1
end function

public function integer wf_read_only ();/*	This function is used to put the screen into 'READ ONLY' mode. This is
	accomplished by protecting all datawindow attributes and disabling all buttons.
*/

/*	Goal - Objective tab.
*/
	tab_rehab_plan.tabpage_goals_objectives.dw_objective.uf_protect_allattributes(TRUE)
	tab_rehab_plan.tabpage_goals_objectives.dw_goal.uf_protect_allattributes(TRUE)
	tab_rehab_plan.tabpage_goals_objectives.dw_objective.uf_set_backcolor()
	tab_rehab_plan.tabpage_goals_objectives.dw_goal.uf_set_backcolor()
	tab_rehab_plan.tabpage_goals_objectives.cb_add_goal.enabled 						= FALSE
	tab_rehab_plan.tabpage_goals_objectives.cb_add_objective.enabled 				= FALSE
	tab_rehab_plan.tabpage_goals_objectives.cb_cancel_goal_objective.enabled 	= FALSE
	tab_rehab_plan.tabpage_goals_objectives.cb_save_goal_objective.enabled 		= FALSE

/*	Task tab.
*/
	tab_rehab_plan.tabpage_task.dw_task.uf_protect_allattributes(TRUE)
	tab_rehab_plan.tabpage_task.dw_external_requestor.uf_protect_allattributes(TRUE)
	tab_rehab_plan.tabpage_task.dw_task.uf_set_backcolor()
	tab_rehab_plan.tabpage_task.dw_external_requestor.uf_set_backcolor()	
	tab_rehab_plan.tabpage_task.cb_add_task.enabled 			= FALSE
	tab_rehab_plan.tabpage_task.cb_cancel_task.enabled 		= FALSE
	tab_rehab_plan.tabpage_task.cb_save_task.enabled 			= FALSE
	tab_rehab_plan.tabpage_task.cb_send.enabled 					= FALSE
	tab_rehab_plan.tabpage_task.cb_attach_doc_tsk.enabled 	= FALSE
	tab_rehab_plan.tabpage_task.dw_task.Modify ("b_search_provider2.Visible = '0'") 
	tab_rehab_plan.tabpage_task.dw_external_requestor.Modify ("b_search_provider2.Visible = '0'") 

/*	Authorization tab.
*/
	tab_rehab_plan.tabpage_authorizations.dw_authorizations.uf_protect_allattributes(TRUE)
	tab_rehab_plan.tabpage_authorizations.dw_authorizations.uf_set_backcolor()
	tab_rehab_plan.tabpage_authorizations.cb_add_authorization.enabled 		= FALSE
	tab_rehab_plan.tabpage_authorizations.cb_authorization_cancel.enabled 	= FALSE
	tab_rehab_plan.tabpage_authorizations.cb_authorization_save.enabled 	= FALSE
	tab_rehab_plan.tabpage_authorizations.cb_delete.enabled 						= FALSE
	tab_rehab_plan.tabpage_authorizations.dw_authorizations.Modify("b_revision_history.Enabled = true")
	tab_rehab_plan.tabpage_authorizations.dw_authorizations.Modify("t_max_indicator.Enabled = true")
	tab_rehab_plan.tabpage_authorizations.dw_authorizations.Modify("t_fixed_rate_item.Enabled = true")	

/*	Progress tab.
*/
	tab_rehab_plan.tabpage_progress.dw_task_progress.uf_protect_allattributes(TRUE)
	tab_rehab_plan.tabpage_progress.dw_progress_note_event.uf_protect_allattributes(TRUE)
	tab_rehab_plan.tabpage_progress.dw_task_progress.uf_set_backcolor()
	tab_rehab_plan.tabpage_progress.dw_progress_note_event.uf_set_backcolor()
	tab_rehab_plan.tabpage_progress.cb_add_progress.enabled 		= FALSE
	tab_rehab_plan.tabpage_progress.cb_attach_doc.enabled 			= FALSE
	tab_rehab_plan.tabpage_progress.cb_cancel_progress.enabled 	= FALSE
	tab_rehab_plan.tabpage_progress.cb_save_progress.enabled 		= FALSE
	tab_rehab_plan.tabpage_progress.cb_send_msg.enabled 			= FALSE
	tab_rehab_plan.tabpage_progress.cb_link_event.enabled 			= FALSE

/*	Voc Profile tab.
*/
	tab_rehab_plan.tabpage_voc_profile.dw_voc_profile.uf_protect_allattributes(TRUE)
	tab_rehab_plan.tabpage_voc_profile.dw_voc_profile.uf_set_backcolor()	
	tab_rehab_plan.tabpage_voc_profile.cb_cancel_voc_profile.enabled = FALSE
	tab_rehab_plan.tabpage_voc_profile.cb_save_voc_profile.enabled 	= FALSE
	
/*	Case monitoring tab.
*/
	tab_rehab_plan.tabpage_case_monitoring.dw_case_monitoring.uf_protect_allattributes(TRUE)
	tab_rehab_plan.tabpage_case_monitoring.dw_complicating_factors.uf_protect_allattributes(TRUE)
	tab_rehab_plan.tabpage_case_monitoring.dw_case_monitoring.uf_set_backcolor()
	tab_rehab_plan.tabpage_case_monitoring.dw_complicating_factors.uf_set_backcolor()
	tab_rehab_plan.tabpage_case_monitoring.cb_cancel_case_monitor.enabled 			= FALSE
	tab_rehab_plan.tabpage_case_monitoring.cb_save_case_monitor.enabled 			= FALSE
	tab_rehab_plan.tabpage_case_monitoring.cb_add_complicating_factor.enabled 	= FALSE 

/*	Action item tab.
*/
	tab_rehab_plan.tabpage_action_item.dw_action_item.enabled 		= FALSE
	tab_rehab_plan.tabpage_action_item.cb_add_action_item.enabled 	= FALSE
	tab_rehab_plan.tabpage_action_item.dw_action_item.uf_set_backcolor()
	tab_rehab_plan.tabpage_action_item.dw_action_item_progress_note.enabled = FALSE
	tab_rehab_plan.tabpage_action_item.dw_action_item_progress_note.uf_set_backcolor()
	tab_rehab_plan.tabpage_action_item.cb_add_progress_note.enabled = FALSE

	RETURN 0
	
end function

public function integer wf_retrieve_task_list ();LONG	ll_list_row_count, ll_current_row

/* Check to make sure that a claim exists.  Rob Head 98/09/14. */
If dw_basic_claim.RowCount() = 0 Then
	MessageBox('Error', 'Unable to perform process when a claim is not active in basic claim window.')
	Return -1
End If
	
ll_current_row = tab_rehab_plan.tabpage_task.dw_task_list.GetRow()
tab_rehab_plan.tabpage_task.dw_task_list.ReSet()
ll_list_row_count = tab_rehab_plan.tabpage_task.dw_task_list.Retrieve(dw_basic_claim.GetItemNumber(1,'claim_no'))
SQLCA.nf_handle_error('Retrieve of task list','w_rehab_sheet','wf_retrieve_task_list') 
	
IF ll_list_row_count = 0 THEN
	tab_rehab_plan.tabpage_task.dw_task.Reset()
	tab_rehab_plan.tabpage_task.dw_external_requestor.Reset()
END IF

Return ll_list_row_count

end function

public subroutine wf_enable_action_item_controls (boolean ab_changes_pending);
tab_rehab_plan.tabpage_action_item.cb_save.Enabled = ab_changes_pending
tab_rehab_plan.tabpage_action_item.cb_cancel.Enabled = ab_changes_pending
tab_rehab_plan.tabpage_action_item.cb_add_action_item.Enabled = NOT ab_changes_pending
tab_rehab_plan.tabpage_action_item.dw_action_list.Enabled = NOT ab_changes_pending
tab_rehab_plan.tabpage_action_item.dw_progress_notes_list.Enabled = NOT ab_changes_pending

IF tab_rehab_plan.tabpage_action_item.dw_action_item.RowCount() = 0 Then
	tab_rehab_plan.tabpage_action_item.cb_add_progress_note.enabled = False
Else
	inv_action_item.nf_load_bus_rule_flags()
	If inv_action_item.ib_progress_note_allowed Then
		tab_rehab_plan.tabpage_action_item.cb_add_progress_note.enabled = True
	Else
		tab_rehab_plan.tabpage_action_item.cb_add_progress_note.enabled = False
	End if
End if


wf_save_indicator()
end subroutine

public function integer wf_retrieve_task_details ();STRING					ls_medical_management_flag, ls_rehab_service
LONG					ll_row, ll_task_no, ll_claim_no
DATETIME				ldtm_rehab_create_date
DATAWINDOW		ldw_task_list

ldw_task_list = tab_rehab_plan.tabpage_task.dw_task_list

IF ldw_task_list.RowCount() = 0 THEN
	tab_rehab_plan.tabpage_task.cb_add_follow_up.enabled = FALSE
	RETURN 0
END IF

ll_row 								= ldw_task_list.GetRow()

IF ll_row < 1 THEN RETURN -1

ll_claim_no 						= ldw_task_list.GetItemNumber(ll_row,"claim_no")
il_task_no 							= ldw_task_list.GetItemNumber(ll_row,"task_no")
ls_rehab_service 				= ldw_task_list.getitemstring(ll_row,'rehab_service_code')
ldtm_rehab_create_date 	= ldw_task_list.getitemdatetime(ll_row, 'create_date')

ldw_task_list.SelectRow(0,FALSE)
ldw_task_list.SelectRow(ll_row,TRUE)

inv_rehab_progress.nf_set_task_no(il_task_no)
inv_tab_controller.nf_current_task_no(il_task_no)	

ls_medical_management_flag = ldw_task_list.GetItemString(ll_row,'medical_management_flag')

IF ib_allow_add_follow_up_from_task THEN
	IF ls_medical_management_flag = 'Y' THEN
		tab_rehab_plan.tabpage_task.cb_add_follow_up.enabled = TRUE
	ELSE
		tab_rehab_plan.tabpage_task.cb_add_follow_up.enabled = FALSE
	END IF
END IF

/* Retrieve and display task details. */
inv_tasks.nf_retrieve(ll_claim_no, il_task_no)

/* Need a rule to prevent the user from modifying the task (especially the task status). 
	3.570	A task for the physiotherapy service must not be modified if the task was created prior to the date of the first Physio Deployment changes.
*/
IF ls_rehab_service = 'S022' AND DATE(ldtm_rehab_create_date) < idt_ephysio_br_check_date THEN
	tab_rehab_plan.tabpage_task.dw_task.enabled 						= FALSE
	tab_rehab_plan.tabpage_task.dw_external_requestor.enabled = FALSE
ELSE
	tab_rehab_plan.tabpage_task.dw_task.enabled 						= TRUE
	tab_rehab_plan.tabpage_task.dw_external_requestor.enabled = TRUE
END IF

RETURN 1
end function

public function integer wf_retrieve_auth_details ();LONG				ll_row, ll_authorization_no
BOOLEAN			lb_old_do_tab_selection
STRING				ls_task_status_code
INTEGER			li_dw_task_authorization_list_row
DATAWINDOW	ldw_task_auth_list


ldw_task_auth_list = tab_rehab_plan.tabpage_authorizations.dw_authorization_task_list

IF ldw_task_auth_list.RowCount() = 0 THEN
	tab_rehab_plan.tabpage_authorizations.cb_add_authorization.enabled = FALSE
	RETURN 0 
END IF

tab_rehab_plan.tabpage_authorizations.cb_add_authorization.enabled = TRUE

ll_row = ldw_task_auth_list.GetRow()	// the currentrow argument was not used because it does not always have a value

ldw_task_auth_list.SelectRow(0,FALSE)
ldw_task_auth_list.SelectRow(ll_row,TRUE)

il_task_no 					= 		ldw_task_auth_list.GetItemNumber(ll_row,'task_no')
ls_task_status_code 	=  	ldw_task_auth_list.GetItemstring(ll_row,'task_status_code')

/*This ib_do_tab_selection thing needs to be looked at. */
lb_old_do_tab_selection 	= ib_do_tab_selection
ib_do_tab_selection 			= FALSE
inv_tab_controller.nf_current_task_no(il_task_no)
ib_do_tab_selection = lb_old_do_tab_selection 

/*		Set the claim number and task number for the */
inv_authorizations.nf_set_claim_no(il_claim_no,il_task_no)

//grab the authorization so that we can scroll to it after the re-retrieve
li_dw_task_authorization_list_row = tab_rehab_plan.tabpage_authorizations.dw_task_authorization_list.getrow()
IF li_dw_task_authorization_list_row > 0 THEN 
	ll_authorization_no = tab_rehab_plan.tabpage_authorizations.dw_task_authorization_list.getitemnumber(li_dw_task_authorization_list_row, 'authorization_no')
ELSE
	ll_authorization_no = 0
END IF 

wf_retrieve_author_task_list(ll_authorization_no)

/* 4.390	An authorization must not be added  or modified on a task that is cancelled (task_status_code = ‘03’).*/
IF istr_window_message.as_mode = 'READ' OR ls_task_status_code = '03' THEN
	tab_rehab_plan.tabpage_authorizations.cb_add_authorization.enabled = FALSE
	tab_rehab_plan.tabpage_authorizations.dw_authorizations.uf_protect_allattributes(TRUE)
	tab_rehab_plan.tabpage_authorizations.dw_authorizations.uf_protect_allattributes(TRUE)
//	tab_rehab_plan.tabpage_authorizations.dw_authorizations.uf_set_backcolor()
	tab_rehab_plan.tabpage_authorizations.dw_authorizations.Modify("b_revision_history.Enabled = true")
	tab_rehab_plan.tabpage_authorizations.dw_authorizations.Modify("t_max_indicator.Enabled = true")
	tab_rehab_plan.tabpage_authorizations.dw_authorizations.Modify("t_fixed_rate_item.Enabled = true")	
ELSE
	tab_rehab_plan.tabpage_authorizations.cb_add_authorization.enabled = TRUE
	tab_rehab_plan.tabpage_authorizations.dw_authorizations.enabled = TRUE
	tab_rehab_plan.tabpage_authorizations.dw_authorizations.uf_protect_allattributes(FALSE)
	tab_rehab_plan.tabpage_authorizations.dw_authorizations.uf_protect_allattributes(FALSE)
END IF
	
RETURN 1

end function

public function integer wf_retrieve_progress_details ();LONG		ll_list_row_count
BOOLEAN	lb_old_do_tab_selection
LONG		ll_row
DATAWINDOW		ldw_progress_task_list

ldw_progress_task_list = tab_rehab_plan.tabpage_progress.dw_progress_task_list


IF ldw_progress_task_list.RowCount() = 0 THEN
	tab_rehab_plan.tabpage_progress.cb_add_progress.enabled = FALSE
	tab_rehab_plan.tabpage_progress.cb_send_msg.enabled = FALSE
	Return 0
END IF

ll_row = ldw_progress_task_list.GetRow()

If istr_window_message.as_mode <> 'READ' THEN
	tab_rehab_plan.tabpage_progress.cb_add_progress.enabled = TRUE
	tab_rehab_plan.tabpage_progress.cb_send_msg.enabled = TRUE
End if

ldw_progress_task_list.SelectRow(0,FALSE)
ldw_progress_task_list.SelectRow(ll_row,TRUE)

il_task_no = ldw_progress_task_list.GetItemNumber(ll_row,'task_no')

lb_old_do_tab_selection = ib_do_tab_selection
ib_do_tab_selection = False
inv_tab_controller.nf_current_task_no(il_task_no)
ib_do_tab_selection = lb_old_do_tab_selection 

/* Set the task no for the Progress tab user object 
*/
inv_rehab_progress.nf_set_task_no(il_task_no)
inv_rehab_progress.nf_retrieve_progress()		
	
/* Retrieve all current progress notes for the selected task.
*/
tab_rehab_plan.tabpage_progress.dw_progress_notes_for_task_list.Reset()		
ll_list_row_count = tab_rehab_plan.tabpage_progress.dw_progress_notes_for_task_list.Retrieve(il_claim_no,il_task_no)

SQLCA.nf_handle_error('w_rehab_sheet','wf_retrieve_progress_details()','dw_progress_notes_for_task_list.Retrieve(il_claim_no,il_task_no)')

IF ll_list_row_count = 0 THEN
	tab_rehab_plan.tabpage_progress.dw_progress_note_event.Reset()
END IF

return 1

end function

public function integer wf_rehab_viewer_retrieve (integer ai_current_tab, long al_task_no);/*	This function is used to retrieve the needed records into the rehab plan viewer.
*/
LONG			ll_claim_no, ll_rows, ll_trancount, ll_found_at, ll_current_row
STRING			ls_sql_statement
DATETIME		ldtm_date

/* Check to make sure that a claim exists.  Rob Head 98/09/14. */
IF w_rehab_sheet.dw_basic_claim.RowCount() = 0 Then
	MessageBox('Error', 'Unable to perform process when a claim is not active in basic claim window.')
	RETURN -1
END IF

ll_claim_no = w_rehab_sheet.dw_basic_claim.GetItemNumber(1,'claim_no')

/*	Retrieve the Task/Progress tab report. But first need to close the current transaction. This is
	because the retrieve uses a stored procedure that makes use of temporary tables and they cannot
	be created in an open transaction (SQL Server release 4.2).
*/


ll_current_row = tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.GetRow()
tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.Reset()
ll_rows = tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.Retrieve(ll_claim_no)


	
tab_rehab_plan_viewer.tabpage_goal_history.dw_goal_history.Retrieve(ll_claim_no)
SQLCA.nf_handle_error('Retrieve for dw_goal_history','w_rehab_sheet','wf_rehab_viewer_retrieve()') 

/*	Retrieve authorizations tab info. - added May 29/98 
*/
tab_rehab_plan_viewer.tabpage_authorizations_viewer.dw_rehab_viewer_authorizations.Retrieve(ll_claim_no)
SQLCA.nf_handle_error('Retrieve for dw_rehab_viewer_authorizations','w_rehab_sheet','wf_rehab_viewer_retrieve()') 

/*	End of new code
*/
IF ll_rows > 0 THEN
	tab_rehab_plan.SelectTab(ai_current_tab)
		ll_found_at = tab_rehab_plan.tabpage_progress.dw_progress_task_list.Find('task_no = ' + String(al_task_no),1,tab_rehab_plan.tabpage_progress.dw_progress_task_list.RowCount())
		IF ll_found_at > 0 THEN
			tab_rehab_plan.tabpage_progress.dw_progress_task_list.ScrollToRow(ll_found_at)
			tab_rehab_plan.tabpage_progress.dw_progress_task_list.SelectRow(0,FALSE)
			tab_rehab_plan.tabpage_progress.dw_progress_task_list.SelectRow(ll_found_at,TRUE)
		END IF
END IF
	
RETURN 0

end function

public subroutine f_adjust_text (string as_tabpage);STRING ls_text, ls_new_text, ls_text2, ls_new_text2, ls_tab, ls_double, ls_double2, ls_lengthy, ls_rlen, ls_llen
LONG 	ll_pos, ll_pos2,ll_string1, ll_string2, ll_len

IF as_tabpage = 'tabpage_progress' THEN
	ls_tab = 'progress'
	IF tab_rehab_plan.tabpage_progress.dw_progress_note_event.RowCount() > 0 THEN
		ls_text = tab_rehab_plan.tabpage_progress.dw_progress_note_event.GetItemString(tab_rehab_plan.tabpage_progress.dw_progress_note_event.GetRow(), "event_comment")
	END IF
ELSEIF as_tabpage = 'tabpage_action_item' THEN
	IF tab_rehab_plan.tabpage_action_item.dw_action_item_progress_note.RowCount() > 0 THEN
		ls_text = tab_rehab_plan.tabpage_action_item.dw_action_item_progress_note.GetItemString(tab_rehab_plan.tabpage_action_item.dw_action_item_progress_note.GetRow(), "combined_comment")
	END IF
END IF

ls_text = f_clean_string_4(ls_text)

IF as_tabpage = 'tabpage_progress' THEN
	tab_rehab_plan.tabpage_progress.dw_progress_note_event.SetItem(tab_rehab_plan.tabpage_progress.dw_progress_note_event.GetRow(), "event_comment",ls_text)
ELSEIF as_tabpage = 'tabpage_action_item' THEN
	tab_rehab_plan.tabpage_action_item.dw_action_item_progress_note.SetItem(tab_rehab_plan.tabpage_action_item.dw_action_item_progress_note.GetRow(), "combined_comment",ls_text)
END IF
ib_note = FALSE
end subroutine

public subroutine wf_display_caution_flag ();INTEGER li_counter

SELECT	count(*)
INTO		:li_counter
FROM	INDIVIDUAL					a,
			CLAIM_PARTICIPANT		b,
			REHAB_TASK					c
WHERE	a.individual_no		= b.individual_no
AND		b.claim_no				= c.claim_no
AND		a.caution_flag		= 'Y'
AND		c.claim_no				= :il_claim_no
USING SQLCA;

SQLCA.nf_handle_error("w_rehab_sheet","wf_display_caution_flag()","Select count(*) from INDIVIDUAL,....")

IF li_counter > 0 THEN
	tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.Modify("t_caution.Visible='1'")
ELSE
	tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.Modify("t_caution.Visible='0'")
END IF

end subroutine

public function integer wf_new_claim_no_refresh ();	LONG			ll_rows, ll_trancount
	STRING			ls_sql_statement
	DATETIME		ldtm_date
	
	w_rehab_sheet.SetRedraw(False)
	/* Check to make sure that a claim exists.  Rob Head 98/09/14. */
	If iw_passedwindow.dw_basic_claim.RowCount() = 0 Then
		MessageBox('Error', 'Unable to perform process when a claim is not active in basic claim window.')
		Return -1
	End If

	w_rehab_sheet.Title = 'Rehab Plan - ' + iw_passedwindow.Title
	
/*	Set the instance variable with the current claim number from the tombstone.
*/
	il_claim_no	= iw_passedwindow.dw_basic_claim.GetItemNumber(1,'claim_no')

/*	Enable all tabs & buttons. This is in case they were all disabled by an invalid claim search.
*/	
	tab_rehab_plan.tabpage_goals_objectives.Enabled = TRUE
	tab_rehab_plan.tabpage_task.Enabled = TRUE
	tab_rehab_plan.tabpage_authorizations.Enabled = TRUE
	tab_rehab_plan.tabpage_progress.Enabled = TRUE
	tab_rehab_plan.tabpage_voc_profile.Enabled = TRUE
	tab_rehab_plan.tabpage_case_monitoring.Enabled = TRUE
	tab_rehab_plan_viewer.tabpage_goal_history.Enabled = TRUE
	tab_rehab_plan_viewer.tabpage_tasks_and_progress.Enabled = TRUE
	dw_documents.Enabled	= TRUE
	cb_refresh_document_list.Enabled	= TRUE
	p_maximize.Enabled = TRUE

	tab_rehab_plan.tabpage_case_monitoring.cb_save_case_monitor.enabled = FALSE
	tab_rehab_plan.tabpage_goals_objectives.cb_save_goal_objective.enabled = FALSE
	tab_rehab_plan.tabpage_progress.cb_save_progress.enabled = FALSE
	tab_rehab_plan.tabpage_task.cb_save_task.enabled = FALSE
	tab_rehab_plan.tabpage_voc_profile.cb_save_voc_profile.enabled = FALSE
	tab_rehab_plan.tabpage_authorizations.cb_authorization_save.enabled = FALSE
	wf_save_indicator()
	
	inv_tab_controller.ib_do_row_selection = False
	
	
/*	GOALS AND OBJECTIVES.
*/
	inv_goals_objectives.nf_set_claim_no(il_claim_no)

/*	Call function to do initial retrieve into goal & objective lists (dw_goal_objective_list).
*/
	ll_rows = wf_retrieve_goal_objective_lists()
	IF ll_rows < 0 THEN
		RETURN -1
	ELSE
		IF ll_rows = 0 THEN
			/*	Disable all other tabs until a goal is saved.
			*/		
			tab_rehab_plan.tabpage_authorizations.enabled = FALSE
			tab_rehab_plan.tabpage_case_monitoring.enabled = FALSE
			tab_rehab_plan.tabpage_progress.enabled = FALSE
			tab_rehab_plan.tabpage_task.enabled = FALSE
			tab_rehab_plan.tabpage_voc_profile.enabled = FALSE
			tab_rehab_plan.tabpage_action_item.enabled = False
			tab_rehab_plan.tabpage_goals_objectives.dw_goal.Reset()
			tab_rehab_plan.tabpage_goals_objectives.dw_objective_list.Reset()
			tab_rehab_plan.tabpage_goals_objectives.dw_objective.Reset()
			inv_goals_objectives.nf_insert(0)
		ELSE
			tab_rehab_plan.tabpage_authorizations.enabled = TRUE
 			tab_rehab_plan.tabpage_case_monitoring.enabled = TRUE
			tab_rehab_plan.tabpage_progress.enabled = TRUE
			tab_rehab_plan.tabpage_task.enabled = TRUE
			tab_rehab_plan.tabpage_voc_profile.enabled = TRUE
			tab_rehab_plan.tabpage_action_item.enabled = True
		END IF
	END IF

/*	TASK.
*/
	inv_tasks.nf_set_claim_no(il_claim_no)
	inv_rehab_progress.nf_set_claim_no(il_claim_no)
	inv_action_item.nf_set_claim_no(il_claim_no)
	wf_retrieve_task_list()
	
/* ACTION ITEM
*/	
	wf_retrieve_action_item_list()
	wf_set_week_no()
	

/*	PROGRESS.
*/	
	inv_rehab_progress.nf_set_current_date()

/* Determine if there are any documents in the document list 
*/
	IF dw_documents.rowcount() > 0 THEN
		ib_document_found = TRUE
	ELSE
		ib_document_found = FALSE
	END IF
	
/*	VOCATIONAL PROFILE.
*/
	inv_voc_profile.nf_set_claim_no(il_claim_no)
	
/*	CASE MONITORING.
*/
	inv_case_monitor.nf_set_claim_no(il_claim_no)
	
/*	REPORTS.
*/

/*	VIEWER.
*/
/*	Retrieve the report. But first need to close the current transaction. This is because the 
	retrieve uses a stored procedure that makes use of temporary tables and they can not be 
	created in an open transaction (SQL Server release 4.2).
*/
	
	tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.Retrieve(il_claim_no)
	SQLCA.nf_handle_error('dw_tasks_and_progress.Retrieve','w_rehab_sheet','wf_new_claim_no_refresh()') 
	
	tab_rehab_plan_viewer.tabpage_goal_history.dw_goal_history.Retrieve(il_claim_no)
	SQLCA.nf_handle_error('dw_goal_history.Retrieve(il_claim_no)','w_rehab_sheet','wf_new_claim_no_refresh()') 
	
/* New code added Jun.1/98: retrieve for viewer's authorizations tabpage
*/
	tab_rehab_plan_viewer.tabpage_authorizations_viewer.dw_rehab_viewer_authorizations.Retrieve(il_claim_no)
	SQLCA.nf_handle_error('dw_rehab_viewer_authorizations.Retrieve(il_claim_no)','w_rehab_sheet','wf_new_claim_no_refresh()') 
	
	tab_rehab_plan_viewer.tabpage_authorizations_viewer.dw_rehab_viewer_authorizations.SelectRow(0, FALSE)


	IF ll_rows > 0 THEN
		tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.SelectRow(0,FALSE)
		tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.SelectRow(1,TRUE)
		tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.TriggerEvent(RowFocusChanged!)
	END IF

	inv_tab_controller.ib_do_row_selection = True

/*	Make sure the 1st tab is selected.*/
	tab_rehab_plan.SelectTab(1)	
	w_rehab_sheet.SetRedraw(True)
	
	wf_display_caution_flag()
	
	RETURN 0
	
end function

public subroutine wf_reset_screen_sizes ();COMMANDBUTTON	lcb_currentcontrol
DATAWINDOW		ldw_currentcontrol
USEROBJECT		luo_currentcontrol
INTEGER			li_loopcount, li_numberofcontrols, li_openedsheets
ANY				la_controltype
OBJECT			lo_controltype
WINDOWOBJECT	lwo_controls[]
W_SHEET			lw_activesheet
WINDOW			lw_wsheet
STRING			wName

/*	Set the size and position of the frame back to their originals.
*/
	w_frame.Width = ii_framewidth
	w_frame.Height = ii_frameheight
	w_frame.X = ii_framex
	w_frame.Y = ii_framey

/*	Determine if there is a work sheet opened, and if so then move all the controls over.
*/
	li_openedsheets = 0
	lw_wsheet = w_frame.GetFirstSheet()
	IF IsValid(lw_wsheet) THEN
		wName = lw_wsheet.ClassName()
		IF wname = 'w_sheet' THEN
			li_openedsheets ++
		END IF
		DO
			lw_wsheet = w_frame.GetNextSheet(lw_wsheet)
			IF IsValid(lw_wsheet) THEN
				wName = lw_wsheet.ClassName()
				IF wname = 'w_sheet' THEN
					li_openedsheets ++
				END IF
			END IF	
		LOOP WHILE IsValid(lw_wsheet)
	END IF

	IF li_openedsheets > 0 THEN
		iw_passedwindow.X = ii_oldsheetx
		iw_passedwindow.Width = ii_oldsheetwidth
		iw_passedwindow.Y = ii_oldsheety
		iw_passedwindow.Height = ii_oldsheetheight
	END IF
	
end subroutine

public function integer wf_explode_frame_and_set_sheets ();/*	This function is used to resize the frame and the 'Work Sheet'. It also move all
	the controls over to the right side.
*/
	INTEGER			li_pbxsize, li_pbysize, li_result, li_screenwidth, li_screenheight
	ENVIRONMENT		lenv_enviroment

/*	Turn off the drawing of the application until all is done.
*/
	w_frame.SetRedraw(FALSE)

/*	Grab the position and width of the both the frame and the sheet.
*/
	ii_frameheight = w_frame.Height
	ii_framewidth = w_frame.Width
	ii_framex = w_frame.X
	ii_framey = w_frame.y
	ii_oldsheetwidth = iw_passedwindow.Width
	ii_oldsheetx = iw_passedwindow.X
	ii_oldsheetheight = iw_passedwindow.Height
	ii_oldsheetx = iw_passedwindow.Y

/* Grab the enviroment of the system. Will need to get the size of the screen from here.
*/
	li_result = GetEnvironment(lenv_enviroment)
	IF li_result < 0 THEN
		RETURN li_result
	ELSE
		li_screenheight = lenv_enviroment.ScreenHeight
		li_screenwidth = lenv_enviroment.ScreenWidth
	END IF

/*	Moved the position of the frame window to the top left hand corner.
*/
	w_frame.X = 0
	w_frame.Y = 0

/*	Convert the size of the screen from pixels to PowerBuilder units.
*/
	li_pbxsize = PixelsToUnits(li_screenwidth,XPixelsToUnits!)
	li_pbysize = PixelsToUnits(li_screenheight,YPixelsToUnits!)

/*	Make the size of the window equal to the size of the screen.
*/
	w_frame.Height = li_pbysize
	w_frame.Width = li_pbxsize

// DELETE THIS CODE IF RESIZING OF THE SCREEN DOESN'T WORK OR IT'S UGLY.
/*	Set the size of the active sheet back to its original.
	This allows users to have the work sheet over top of the rehab sheet
	& with the left-hand side of the rehab sheet visible.
*/
	iw_passedwindow.Width = ii_oldsheetwidth
	iw_passedwindow.X = ii_framex + 20

/*	Turn the drawing of the application back on.
*/
	w_frame.SetRedraw(TRUE)
	
	RETURN 0
end function

public function integer wf_retrieve_author_task_list (long al_authorization_no);LONG		ll_list_row_count
INTEGER	li_found_at

/*		Retrieve and display authorization list     */
tab_rehab_plan.tabpage_authorizations.dw_task_authorization_list.reset()
ll_list_row_count = tab_rehab_plan.tabpage_authorizations.dw_task_authorization_list.Retrieve(il_claim_no,il_task_no)
SQLCA.nf_handle_error('w_rehab_sheet','wf_retrieve_author_task_list()','dw_task_authorization_list.Retrieve(il_claim_no,il_task_no)') 
	
IF ll_list_row_count = 0 THEN		//	no authorization item
	tab_rehab_plan.tabpage_authorizations.dw_authorizations.Reset()
ELSE
	li_found_at = tab_rehab_plan.tabpage_authorizations.dw_task_authorization_list.Find('authorization_no = ' + String(al_authorization_no),1,tab_rehab_plan.tabpage_authorizations.dw_task_authorization_list.RowCount())
	IF li_found_at > 0 THEN
		tab_rehab_plan.tabpage_authorizations.dw_task_authorization_list.ScrollToRow(li_found_at)
	END IF		
END IF

/*		retrieve provider address info. & toll-free phone no. in case auto. authorization letter required */
tab_rehab_plan.tabpage_authorizations.dw_auth_let_provider_info.Retrieve(il_claim_no, il_task_no)
SQLCA.nf_handle_error('w_rehab_sheet','wf_retrieve_author_task_list()','dw_auth_let_provider_info.Retrieve(il_claim_no, il_task_no)') 
		
tab_rehab_plan.tabpage_authorizations.dw_auth_let_toll_free_no.Retrieve(vgst_user_profile.default_admin_region_code)
SQLCA.nf_handle_error('w_rehab_sheet','wf_retrieve_author_task_list()','dw_auth_let_toll_free_no.Retrieve(vgst_user_profile.default_admin_region_code)') 

RETURN ll_list_row_count



end function

public function integer wf_call_function (string as_action);INTEGER li_error

CHOOSE CASE as_action
	CASE "FULL_EVENT_COMMENTS"
	   li_error = wf_full_event_comments()
	   RETURN li_error
END CHOOSE	
		
RETURN 1
end function

public function integer wf_full_event_comments ();INTEGER li_row
STRING  ls_event_type
LONG    ll_event_no

IF tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.RowCount() = 0 THEN RETURN 1

/* grab the row */
li_row = tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.GetRow()

IF ISNULL(li_row) OR li_row < 1  THEN RETURN 1

ll_event_no   = tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.GetItemNumber(li_row,"event_no")

// make sure everything is valid

IF isnull(il_claim_no) THEN RETURN 1
IF isnull(ll_event_no) THEN RETURN 1

//information needed to be sent to the event comments window
istr_window_message.as_stringparm[1] = 'P'
istr_window_message.al_doubleparm[1] = il_claim_no
istr_window_message.al_doubleparm[2] = ll_event_no

//open the event comments window
OpenWithParm(w_full_event_comments, istr_window_message)

RETURN 1
end function

public function long wf_get_sheet_handle ();RETURN il_sheet_handle
end function

event open;call super::open;INTEGER li_result


/*	Grab the active sheet from the Message object.
*/
	istr_window_message = Message.PowerObjectParm
	iw_passedwindow = istr_window_message.awi_parent_window
	
	il_sheet_handle = istr_window_message.al_doubleparm[1]
	
/*	Call function to explode the frame, sheet, and open up a rehab sheet.
*/
	ib_explodewasgood = TRUE
	ib_fire_event_with_sync = TRUE
	li_result = wf_explode_frame_and_set_sheets()
	IF li_result < 0 THEN
		ib_explodewasgood = FALSE
		CLOSE(THIS)
		RETURN
	END IF
	
idt_ephysio_br_check_date = date(ProfileString(vgs_ini_filename,"ePhysio","New_Task_Rules_Date ",""))

IF idt_ephysio_br_check_date = date('1900-01-01') THEN
	messagebox('Implementation Error -  cmwb.ini file', 'The ephysio Buisness rule date check could not be resolved. Please contact the helpdesk before proceeding.')
	RETURN
END IF  

/*	Create an instance of the user object for the view/print function.
*/
	iu_dw_document_path = dw_document_path
	iu_dw_document_path.uf_set_window_handle(Handle(This))
	iu_dw_document_path.Hide()
	
	//default filter
	tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.SetFilter("follow_up_flag = 'N'")
	tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.Filter()

	dw_documents.uf_SetSelect(3)
	
//* SPLITBAR */
//st_ver_splitbar.of_register(tab_rehab_plan)
//st_ver_splitbar.of_register(tab_rehab_plan_viewer)
//
// the datawindows on tab_qualification are resized as the tab control is resized

//IF IsValid(inv_resize) THEN
//ELSE
//	inv_resize = CREATE n_resize
//END IF
//
//inv_resize.of_SetOrigSize (THIS.width, THIS.height)

//starts the resize service in the ancestor object (Move H,Move V,Grow H, Grow V)
//right

//inv_resize.of_register(tab_rehab_plan,50,0,50,100)
//inv_resize.of_register(st_ver_splitbar,50,0,0,100)
//
//left
//inv_resize.of_register(tab_rehab_plan_viewer,0,0,50,100)
//inv_resize.of_register(tab_entitlement,0,0,50,100)
//inv_resize.of_register(tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress,0,0,50,100)
//inv_resize.of_register(idw_opening_master,0,0,50,0)
//inv_resize.of_register(idw_opening_detail,0,0,50,100)
//inv_resize.of_register(idw_payments,0,0,50,100)
//inv_resize.of_register(idw_awards,0,0,50,100)
	
	THIS.PostEvent('ue_postopen')

end event

on w_rehab_sheet.create
int iCurrent
call super::create
if IsValid(this.MenuID) then destroy(this.MenuID)
if this.MenuName = "m_rehab_plan" then this.MenuID = create m_rehab_plan
this.cb_3=create cb_3
this.p_maximize=create p_maximize
this.dw_basic_claim=create dw_basic_claim
this.dw_document_path=create dw_document_path
this.cb_refresh_document_list=create cb_refresh_document_list
this.dw_documents=create dw_documents
this.tab_rehab_plan=create tab_rehab_plan
this.tab_rehab_plan_viewer=create tab_rehab_plan_viewer
this.st_save=create st_save
this.uo_image_append=create uo_image_append
this.st_1=create st_1
this.st_2=create st_2
this.st_week_no=create st_week_no
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_3
this.Control[iCurrent+2]=this.p_maximize
this.Control[iCurrent+3]=this.dw_basic_claim
this.Control[iCurrent+4]=this.dw_document_path
this.Control[iCurrent+5]=this.cb_refresh_document_list
this.Control[iCurrent+6]=this.dw_documents
this.Control[iCurrent+7]=this.tab_rehab_plan
this.Control[iCurrent+8]=this.tab_rehab_plan_viewer
this.Control[iCurrent+9]=this.st_save
this.Control[iCurrent+10]=this.uo_image_append
this.Control[iCurrent+11]=this.st_1
this.Control[iCurrent+12]=this.st_2
this.Control[iCurrent+13]=this.st_week_no
end on

on w_rehab_sheet.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_3)
destroy(this.p_maximize)
destroy(this.dw_basic_claim)
destroy(this.dw_document_path)
destroy(this.cb_refresh_document_list)
destroy(this.dw_documents)
destroy(this.tab_rehab_plan)
destroy(this.tab_rehab_plan_viewer)
destroy(this.st_save)
destroy(this.uo_image_append)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_week_no)
end on

event close;call super::close;/*	Call function to reset the frame and the active Work Sheet back to their original sizes.
*/
IF ib_explodewasgood 				THEN	wf_reset_screen_sizes()

IF IsValid(inv_authorizations) 		THEN	Destroy(inv_authorizations)
IF IsValid(inv_case_monitor) 		THEN 	Destroy(inv_case_monitor)
IF IsValid(inv_goals_objectives) 	THEN	Destroy(inv_goals_objectives)
IF IsValid(inv_rehab_progress) 	THEN	Destroy(inv_rehab_progress)
IF IsValid(inv_tasks) 					THEN	Destroy(inv_tasks)
IF IsValid(inv_voc_profile) 			THEN	Destroy(inv_voc_profile)

end event

event closequery;call super::closequery;LONG	ll_ans

	IF tab_rehab_plan.tabpage_authorizations.cb_authorization_save.enabled OR &
		tab_rehab_plan.tabpage_case_monitoring.cb_save_case_monitor.enabled OR &
		tab_rehab_plan.tabpage_goals_objectives.cb_save_goal_objective.enabled OR &
		tab_rehab_plan.tabpage_progress.cb_save_progress.enabled OR &
		tab_rehab_plan.tabpage_action_item.cb_save.enabled OR &
		tab_rehab_plan.tabpage_task.cb_save_task.enabled OR &
		tab_rehab_plan.tabpage_voc_profile.cb_save_voc_profile.enabled THEN
		
		ll_ans = MessageBox('Save Data','Data not saved.  Go back and save now?', Question!, YesNo!)
		
		IF ll_ans = 1 THEN
			Return 1
		ELSE
		// Added by Rob Head 98/09/09
		tab_rehab_plan.tabpage_authorizations.cb_authorization_save.enabled = FALSE
		tab_rehab_plan.tabpage_case_monitoring.cb_save_case_monitor.enabled = FALSE
		tab_rehab_plan.tabpage_goals_objectives.cb_save_goal_objective.enabled = FALSE
		tab_rehab_plan.tabpage_progress.cb_save_progress.enabled = FALSE
		tab_rehab_plan.tabpage_task.cb_save_task.enabled = FALSE
		tab_rehab_plan.tabpage_voc_profile.cb_save_voc_profile.enabled = FALSE
		tab_rehab_plan.tabpage_action_item.cb_save.enabled = False
			Return 0
		END IF
	END IF
end event

event activate;call super::activate;/* Added by Rob Head 98/08/20. */
If iw_passedwindow.dw_basic_claim.RowCount() = 0 Then
	If iw_passedwindow.WindowState = Minimized! Then // Check for minimized added by Rob Head 98/09/13.
		iw_passedwindow.WindowState = Normal!
	End If
	iw_passedwindow.SetFocus()
	MessageBox('Warning', 'You must have an active claim in the sheet window before working with Rehab Plan')
	Return 0
End If          




end event

type cb_3 from commandbutton within w_rehab_sheet
integer x = 2441
integer y = 488
integer width = 402
integer height = 104
integer taborder = 12
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "none"
end type

type p_maximize from picture within w_rehab_sheet
integer x = 4933
integer y = 2256
integer width = 87
integer height = 60
string picturename = "maximize.bmp"
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

event clicked;//	Note: the picture on this command button is accomplished by using the
//	monotype sorts font.  A maximize arrow is the letter 's' and a
//	minimize arrow is the letter 't'


//	Hide the buttons

	cb_refresh_document_list.visible = False
	p_maximize.visible = False

If this.picturename = "maximize.bmp" Then

//	Move and resize the height of the Document List/Index

	dw_documents.Y = 377
	dw_documents.Height=2180

//	Move the controls that reside on the Document List/Index

	cb_refresh_document_list.Y=377
	p_maximize.Y=377


//	Scroll the current row into view

	IF il_document_row_number > 0 THEN
		dw_documents.ScrollToRow(il_document_row_number)
	END IF

	This.picturename = "restore.bmp"

Else

	//	Move and resize the height of the Document List/Index  

	dw_documents.Y=2209
	dw_documents.Height=361

	//	Move the controls that reside on the Document List/Index 

	cb_refresh_document_list.Y=2209
	p_maximize.Y=2209

	//	Scroll the current row into view

	IF il_document_row_number > 0 THEN
		dw_documents.ScrollToRow(il_document_row_number)
	END IF

	This.picturename = "maximize.bmp"
End If

//	Show the buttons

cb_refresh_document_list.visible = True
p_maximize.visible = True

end event

type dw_basic_claim from u_dw_online within w_rehab_sheet
integer x = 1952
integer y = 16
integer width = 3173
integer height = 348
integer taborder = 50
string dataobject = "d_basic_claim"
boolean livescroll = false
borderstyle borderstyle = styleraised!
end type

event itemchanged;call super::itemchanged;/*	This event is triggered by the retrieveend event of the dw_basic_claim on the
	worksheet. This is done to refresh the information on the rehab sheet as the
	datawindows are shared and their events do not fired.
*/
INTEGER	li_result

IF iw_passedwindow.wf_get_basic_claim_rfc() = TRUE THEN

	IF THIS.RowCount() > 0 THEN
		li_result = wf_new_claim_no_refresh()
		IF li_result < 0 THEN
			Close(PARENT)
			RETURN
		END IF
		
		IF Upper(THIS.GetItemString(1,'computed_history_indicator')) = 'HISTORY CLAIM' THEN
			tab_rehab_plan.Enabled = FALSE
			tab_rehab_plan_viewer.Enabled = FALSE
			MessageBox("History Claim","The Rehab Plan will be disabled as this claim is a history claim.",Exclamation!)
		ELSE
			tab_rehab_plan.Enabled = TRUE
			tab_rehab_plan_viewer.Enabled = TRUE
		END IF
	ELSE
		wf_clear_rehab_sheet()
	END IF
END IF
end event

event constructor;/* Overridden */
end event

type dw_document_path from u_dw_document_path within w_rehab_sheet
boolean visible = false
integer x = 2162
integer y = 368
integer width = 1147
integer taborder = 10
end type

type cb_refresh_document_list from commandbutton within w_rehab_sheet
integer x = 4846
integer y = 2256
integer width = 87
integer height = 60
integer taborder = 70
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "R"
end type

event clicked;/*	Refresh the document list by triggering the Worksheet's button.
*/
	iw_passedwindow.cb_refresh_document_list.TriggerEvent(Clicked!)
	dw_documents.object.t_filtered_indicator.visible = 0

end event

type dw_documents from u_dw_online within w_rehab_sheet
integer x = 1952
integer y = 2244
integer width = 3163
integer height = 320
integer taborder = 40
string dataobject = "d_documents"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event doubleclicked;call super::doubleclicked;LONG    	ll_doc_id
STRING 	ls_doc_type

/*	Get the number of the row that was selected
	Only continue if a row was selected
*/
il_document_row_number = row
IF il_document_row_number <= 0 THEN  RETURN

/*	Get the document id for selected row View the document
*/
ll_doc_id = dw_documents.GetItemNumber (il_document_row_number,"ref_docid")
			
IF uo_image_append.of_init(ll_doc_id) <= 0 THEN RETURN
		
ls_doc_type =  uo_image_append.of_get_file_type()
			
CHOOSE CASE ls_doc_type
	/*  Imaged document */ 
	CASE 'IMA', 'TIF'
		
		IF  uo_image_append.of_append_image(ll_doc_id) < 0 THEN RETURN

	CASE ELSE
			iu_dw_document_path.f_manage_document(ll_doc_id,"V","NORMAL")
END CHOOSE
			
	
		
		

end event

event rbuttondown;
m_rehab_document_popup	lm_rehab_document_popup
long ll_rownum
Long                   ll_selected_rows[]
long                    li_count = 1

IF THIS.RowCount() > 0 OR iw_passedwindow.dw_documents.filteredCount() > 0 THEN
	//	Create an instance of the popup menu
	lm_rehab_document_popup = CREATE m_rehab_document_popup

	//	Call the menu function to register the datawindow
	lm_rehab_document_popup.mf_set_datawindow(THIS)

	//	Popup the menu
	lm_rehab_document_popup.m_docdetails.popmenu(w_frame.PointerX(),w_frame.PointerY())
	DESTROY lm_rehab_document_popup
	
	//when changing the sort criteria, a row may still remain highlighted, but not be set as the current row, so do that here. 
	//This is so that when user right-clicks and selects 'more details,' the correct row (document) is used to display the 'more details'
	
	// PR20620 .. Also, users want to be able to select multiple documents, and THEN sort those by date etc, so we need to capture a list of selected row numbers, 
	// before setting the current row,then re-select those remaining rows that had been selected. (Because calling setRow() deselects all other selected rows)

	ll_rownum = this.getSelectedRow(0)
	
	do until ll_rownum = 0
		ll_selected_rows[li_count] = ll_rownum
		ll_rownum = this.getSelectedRow(ll_rownum)
		li_count++
	LOOP
	
	IF upperbound(ll_selected_rows) > 0 THEN
		this.setrow(ll_selected_rows[1]) // this will un-select any other selected rows
		this.scrolltorow(ll_selected_rows[1])
	END IF
	
	li_count = 1
	do until li_count > upperbound(ll_selected_rows)
		this.SelectRow(ll_selected_rows[li_count], TRUE) // now re-select any rows that had been previously selected
		li_count++
	LOOP
	
END IF 



end event

event resize;call super::resize;dw_documents.BringToTop					=TRUE
cb_refresh_document_list.BringToTop	=TRUE
p_maximize.BringToTop							=TRUE

end event

event rowfocuschanged;call super::rowfocuschanged;LONG 			ll_sel_row, ll_new_row, ll_rowcount, ll_cntr, ll_multi
BOOLEAN 		lb_selected


ll_rowcount = dw_documents.RowCount()

FOR ll_cntr = 1 to ll_rowcount
	lb_selected = IsSelected(ll_cntr)
	IF lb_selected = TRUE THEN
		ll_multi = ll_multi + 1
	END IF
NEXT

IF ll_multi = 1 THEN
	ll_sel_row = this.GetSelectedRow(0)

	IF ll_sel_row > 0 THEN
		ll_new_row = currentrow
		THIS.SelectRow(ll_sel_row,FALSE)
		IF ll_new_row > 0 THEN
			THIS.SelectRow(ll_new_row, TRUE)
		END IF
	END IF
END IF
end event

event constructor;/* Overridden */
end event

event ue_filter;call super::ue_filter;
Open(w_filter_doc_list)

// originally w_filter_doc_list was a response window but users wanted resizability functionality, so had to change it to a popup
// we pass in a reference to the datawindow being filtered so the popup can do its job directly within the OK button

IF isvalid(w_filter_doc_list) then
	/*pass in the iw_passedwindow.dw_documents from the main sheet as the reference, since the rehab plan dw_documents 
	   is a shared datawindow, and we need to know things about the main datawindow that the shared datawindow doesn't 
	   know (like what the currently applied filter string currently is), we want the original main datawindow so that 
	   wf_show_current_filter()  can actually work. Also pass the local  dw_documents to the function  (wf_set_shared_dw) 
	*/
	w_filter_doc_list.wf_set_dw(iw_passedwindow.dw_documents)   
	w_filter_doc_list.wf_set_shared_dw(this)   
	w_filter_doc_list.wf_show_current_filter()
END IF
end event

type tab_rehab_plan from tab within w_rehab_sheet
integer x = 1961
integer y = 364
integer width = 3154
integer height = 1860
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
boolean raggedright = true
integer selectedtab = 1
tabpage_goals_objectives tabpage_goals_objectives
tabpage_task tabpage_task
tabpage_authorizations tabpage_authorizations
tabpage_progress tabpage_progress
tabpage_voc_profile tabpage_voc_profile
tabpage_case_monitoring tabpage_case_monitoring
tabpage_action_item tabpage_action_item
end type

on tab_rehab_plan.create
this.tabpage_goals_objectives=create tabpage_goals_objectives
this.tabpage_task=create tabpage_task
this.tabpage_authorizations=create tabpage_authorizations
this.tabpage_progress=create tabpage_progress
this.tabpage_voc_profile=create tabpage_voc_profile
this.tabpage_case_monitoring=create tabpage_case_monitoring
this.tabpage_action_item=create tabpage_action_item
this.Control[]={this.tabpage_goals_objectives,&
this.tabpage_task,&
this.tabpage_authorizations,&
this.tabpage_progress,&
this.tabpage_voc_profile,&
this.tabpage_case_monitoring,&
this.tabpage_action_item}
end on

on tab_rehab_plan.destroy
destroy(this.tabpage_goals_objectives)
destroy(this.tabpage_task)
destroy(this.tabpage_authorizations)
destroy(this.tabpage_progress)
destroy(this.tabpage_voc_profile)
destroy(this.tabpage_case_monitoring)
destroy(this.tabpage_action_item)
end on

event selectionchanged;LONG		ll_nmbr_tasks, ll_found_at, ll_viewer_row, ll_rows, ll_action_task_no
BOOLEAN	lb_old_do_tab_selection

lb_old_do_tab_selection 	= ib_do_tab_selection
ib_do_tab_selection 			= FALSE

w_rehab_sheet.SetRedraw(FALSE)
CHOOSE CASE newindex
	CASE	1
	CASE	2			
		
		inv_tab_controller.nf_current_task_no(inv_tab_controller.nf_current_task_no())
		
	/* If the protect is not on field planned_start_date, turn it on
	*/
//		tab_rehab_plan.tabpage_task.dw_task.Object.planned_start_date.Protect 					= 1
//		tab_rehab_plan.tabpage_task.dw_task.Object.planned_start_date.Background.Color 	= 67108864 
		
	/* Determine if there are any documents in the document list
	*/
		IF dw_documents.rowcount() > 0 THEN
			ib_document_found = TRUE	
		ELSE
			ib_document_found = FALSE
		END IF

		ll_nmbr_tasks = tab_rehab_plan.tabpage_task.dw_task.RowCount() 
	
	
		/* If there are no tasks for the claim, disable the Add Doc button */
		IF ll_nmbr_tasks = 0 THEN
			tab_rehab_plan.tabpage_task.cb_attach_doc_tsk.enabled = FALSE
		ELSE
			IF ib_document_found THEN
				IF istr_window_message.as_mode <> 'READ' THEN
					tab_rehab_plan.tabpage_task.cb_attach_doc_tsk.enabled = TRUE
				ELSE
					tab_rehab_plan.tabpage_task.cb_attach_doc_tsk.enabled = FALSE
				END IF
			ELSE
				tab_rehab_plan.tabpage_task.cb_attach_doc_tsk.enabled = FALSE
			END IF
		END IF

	/*	If a retrieve was delayed because this tab was not the current one, then
		we need to retrieve the data now.
	*/
		IF ib_delayed_task_tab_retrieve THEN
			wf_retrieve_task_details()
			ib_delayed_task_tab_retrieve = FALSE
		END IF
	
	CASE 3		
		inv_tab_controller.nf_current_task_no(inv_tab_controller.nf_current_task_no())

	/*	If a retrieve was delayed because this tab was not the current one, then
		we need to retrieve the data now.
	*/		
		ib_delayed_authorization_tab_retrieve = TRUE
		IF	ib_delayed_authorization_tab_retrieve THEN
			wf_retrieve_auth_details()
			ib_delayed_authorization_tab_retrieve = FALSE
		END IF
			
	CASE 	4	
		inv_tab_controller.nf_current_task_no(inv_tab_controller.nf_current_task_no())
		
		IF ib_delayed_progress_tab_retrieve THEN
			wf_retrieve_progress_details()
			ib_delayed_progress_tab_retrieve = FALSE
		END IF
		
	/* Determine if there are any documents in the document list
	*/
		IF dw_documents.rowcount() > 0 THEN
			ib_document_found = TRUE
		ELSE
			ib_document_found = FALSE
		END IF

	IF tab_rehab_plan.tabpage_progress.dw_progress_task_list.RowCount() = 0 THEN
		tab_rehab_plan.tabpage_progress.cb_save_progress.enabled 		= FALSE
		tab_rehab_plan.tabpage_progress.cb_cancel_progress.enabled 	= FALSE
		tab_rehab_plan.tabpage_progress.cb_add_progress.enabled 		= FALSE
		tab_rehab_plan.tabpage_progress.cb_attach_doc.enabled 			= FALSE
		tab_rehab_plan.tabpage_progress.cb_link_event.enabled 			= FALSE
		tab_rehab_plan.tabpage_progress.cb_send_msg.enabled 			= FALSE
		tab_rehab_plan.tabpage_progress.dw_progress_note_event.Reset()
		tab_rehab_plan.tabpage_progress.dw_task_progress.Reset()
		tab_rehab_plan.tabpage_progress.dw_progress_notes_for_task_list.Reset()
	ELSE
		tab_rehab_plan.tabpage_progress.cb_save_progress.enabled 		= FALSE
		tab_rehab_plan.tabpage_progress.cb_cancel_progress.enabled 	= FALSE
		IF istr_window_message.as_mode = 'READ' THEN
			tab_rehab_plan.tabpage_progress.cb_add_progress.enabled 	= FALSE
			tab_rehab_plan.tabpage_progress.cb_link_event.enabled 		= FALSE
			tab_rehab_plan.tabpage_progress.cb_send_msg.enabled 		= FALSE
		ELSE
			tab_rehab_plan.tabpage_progress.cb_add_progress.enabled 	= TRUE
			tab_rehab_plan.tabpage_progress.cb_link_event.enabled 		= TRUE
			tab_rehab_plan.tabpage_progress.cb_send_msg.enabled 		= TRUE
		END IF
		IF ib_document_found THEN
			IF istr_window_message.as_mode <> 'READ' THEN
				tab_rehab_plan.tabpage_progress.cb_attach_doc.enabled = TRUE
			ELSE
				tab_rehab_plan.tabpage_progress.cb_attach_doc.enabled = FALSE
			END IF
		ELSE
			tab_rehab_plan.tabpage_progress.cb_attach_doc.enabled = FALSE
		END IF
	END IF
		
	/* Protect the newly added progress note from being updated
	*/
		inv_rehab_progress.nf_protect_add_event(true)
		
	CASE 	5	/* VOCATIONAL PROFILE tab */
		
	/* Disable the SAVE and CANCEL buttons
	*/
		tab_rehab_plan.tabpage_voc_profile.cb_save_voc_profile.enabled 	= FALSE
		tab_rehab_plan.tabpage_voc_profile.cb_cancel_voc_profile.enabled = FALSE
		
	/* Retrieve the Vocational Profile information for the claim if it exists
	*/
		ll_rows = tab_rehab_plan.tabpage_voc_profile.dw_voc_profile.Retrieve(il_claim_no)
		SQLCA.nf_handle_error("w_rehab_sheet","tab_rehab_plan - selectionchanged","dw_voc_profile.Retrieve(il_claim_no)") 
		
	/* If there is no Vocational Profile for the claim, insert a row for the user to enter data
	*/
		IF ll_rows = 0 THEN
			inv_voc_profile.nf_insert(1)
			inv_voc_profile.nf_set_defaults()
		END IF	
	
	/* Retrieve the PPI information for the claim
	*/
		tab_rehab_plan.tabpage_voc_profile.dw_percent_ppi.Retrieve(il_claim_no)
		SQLCA.nf_handle_error("w_rehab_sheet","tab_rehab_plan - selectionchanged","dw_percent_ppi.Retrieve(il_claim_no)") 

	CASE	 6	/* CASE MONITORING tab */
		
	/* Disable the SAVE and CANCEL buttons; enable the add Complicating Factor button
	*/
		tab_rehab_plan.tabpage_case_monitoring.cb_save_case_monitor.enabled 	= FALSE
		tab_rehab_plan.tabpage_case_monitoring.cb_cancel_case_monitor.enabled = FALSE
		IF istr_window_message.as_mode <> 'READ' THEN
			tab_rehab_plan.tabpage_case_monitoring.cb_add_complicating_factor.enabled = TRUE
		ELSE
			tab_rehab_plan.tabpage_case_monitoring.cb_add_complicating_factor.enabled = FALSE
		END IF
	/* Disable work & medical functional status dates, set backgrounds to grey
	*/
		tab_rehab_plan.tabpage_case_monitoring.dw_case_monitoring.Object.work_status_date.Protect 									= TRUE
		tab_rehab_plan.tabpage_case_monitoring.dw_case_monitoring.Object.work_status_date.Background.Color 						= 67108864
		tab_rehab_plan.tabpage_case_monitoring.dw_case_monitoring.Object.medical_functional_status_date.Protect 					= TRUE
		tab_rehab_plan.tabpage_case_monitoring.dw_case_monitoring.Object.medical_functional_status_date.Background.Color 	= 67108864
		
	/* Retrieve the Case Monitoring information for the claim if it exists
	*/
		ll_rows = tab_rehab_plan.tabpage_case_monitoring.dw_case_monitoring.Retrieve(il_claim_no)
		SQLCA.nf_handle_error("w_rehab_sheet","tab_rehab_plan - selectionchanged","dw_case_monitoring.Retrieve(il_claim_no)") 

		IF ll_rows > 0 THEN
			/* Retrieve Working Status Change List info. for claim if the Case Monitoring exists.
			*/
			tab_rehab_plan.tabpage_case_monitoring.dw_working_status_change_list.Retrieve(il_claim_no)
			SQLCA.nf_handle_error("w_rehab_sheet","tab_rehab_plan - selectionchanged","dw_working_status_change_list.Retrieve(il_claim_no)") 
		
			/* Retrieve Medical Functional Status Change List info. for claim if the Case Monitoring exists.
			*/
			tab_rehab_plan.tabpage_case_monitoring.dw_medical_func_status_change_list.Retrieve(il_claim_no)
			SQLCA.nf_handle_error("w_rehab_sheet","tab_rehab_plan - selectionchanged","dw_medical_func_status_change_list.Retrieve(il_claim_no)") 
		ELSE
			/* If there is no Case Monitoring for the claim, insert a row for the user to enter data
			*/
			inv_case_monitor.nf_add_case_monitoring()
			inv_case_monitor.nf_set_defaults()
		END IF
		
		/* Retrieve the Complicating Factors information for the claim if there are any Complicating Factors
		   for the Claim (Complicating Factors can exist without a Case Monitoring existing)
		*/
		tab_rehab_plan.tabpage_case_monitoring.dw_complicating_factors_list.Retrieve(il_claim_no)
		SQLCA.nf_handle_error("w_rehab_sheet","tab_rehab_plan - selectionchanged","dw_complicating_factors_list.Retrieve(il_claim_no)") 
		
		inv_case_monitor.nf_set_complication_list(tab_rehab_plan.tabpage_case_monitoring.dw_complicating_factors_list)
		inv_case_monitor.nf_set_complication_trans()
		tab_rehab_plan.tabpage_case_monitoring.st_complication_label.visible = FALSE
		
	CASE 7
		If tab_rehab_plan.tabpage_action_item.dw_action_item.RowCount() > 0 THEN
			ll_action_task_no 	= tab_rehab_plan.tabpage_action_item.dw_action_item.GetItemNumber(1,'task_no')
			ll_viewer_row 		= tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.GetRow()			
			IF ll_viewer_row > 0 THEN
				IF ll_action_task_no <> tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.GetItemNumber(ll_viewer_row,'task_no') THEN
					ll_found_at = tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.Find('task_no = ' +&
					+ String(ll_action_task_no) + ' and event_no = ' + String(0),1,tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.RowCount())
					IF ll_found_at > 0 THEN
						tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.ScrollToRow(ll_found_at)
					END IF
				END IF
			END IF
		END IF
END CHOOSE

ib_do_tab_selection = lb_old_do_tab_selection
	
w_rehab_sheet.SetRedraw(True)
end event

event selectionchanging;LONG	ll_ans

/*

tab_rehab_plan

cb_save_goal_objective tabpage_goals_objectives
cb_save_task tabpage_task
cb_authorization_save tabpage_authorizations
cb_save_progress tabpage_progress

cb_save_voc_profile tabpage_voc_profile
cb_save_case_monitor tabpage_case_monitoring
tabpage_action_item cb_save

*/




CHOOSE CASE oldindex
	CASE	4
		
		IF newindex = 2 THEN
			ib_delayed_task_tab_retrieve = TRUE
		END IF
		IF tabpage_progress.cb_save_progress.enabled THEN
			ll_ans = MessageBox('Save Progress','Do you want to save the progress?',Question!,YesNo!)
			IF ll_ans = 1 THEN
				tabpage_progress.cb_save_progress.TriggerEvent(Clicked!)
				RETURN 1
			ELSE
				inv_rehab_progress.nf_protect_add_event(TRUE)
				tabpage_progress.cb_save_progress.enabled = FALSE
				wf_save_indicator()
			END IF
		END IF
	/* Case 5 and Case 6 added by Rob Head 98/08/21. */
	CASE	5
		
		IF tabpage_voc_profile.cb_save_voc_profile.enabled THEN
			ll_ans = MessageBox('Save Vocational Profile','Do you want to save the profile?',Question!,YesNo!)
			IF ll_ans = 1 THEN
				tabpage_voc_profile.cb_save_voc_profile.TriggerEvent(Clicked!)
				RETURN 1
			ELSE
				inv_rehab_progress.nf_protect_add_event(TRUE)
				tabpage_voc_profile.cb_save_voc_profile.enabled = FALSE
				wf_save_indicator()
			END IF
		END IF
	CASE	6
		
		IF tabpage_case_monitoring.cb_save_case_monitor.enabled THEN
			ll_ans = MessageBox('Save Case Monitoring','Do you want to save the Case?',Question!,YesNo!)
			IF ll_ans = 1 THEN
				tabpage_case_monitoring.cb_save_case_monitor.TriggerEvent(Clicked!)
				RETURN 1
			ELSE
				inv_rehab_progress.nf_protect_add_event(TRUE)
				tabpage_case_monitoring.cb_save_case_monitor.enabled = FALSE
				wf_save_indicator()
			END IF
		END IF
END CHOOSE
	
RETURN 0
end event

type tabpage_goals_objectives from userobject within tab_rehab_plan
integer x = 18
integer y = 168
integer width = 3118
integer height = 1676
long backcolor = 67108864
string text = "Goals/~r~nObjectives"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_goal_list dw_goal_list
dw_goal dw_goal
cb_add_goal cb_add_goal
dw_objective_list dw_objective_list
cb_add_objective cb_add_objective
cb_save_goal_objective cb_save_goal_objective
cb_cancel_goal_objective cb_cancel_goal_objective
dw_objective dw_objective
end type

on tabpage_goals_objectives.create
this.dw_goal_list=create dw_goal_list
this.dw_goal=create dw_goal
this.cb_add_goal=create cb_add_goal
this.dw_objective_list=create dw_objective_list
this.cb_add_objective=create cb_add_objective
this.cb_save_goal_objective=create cb_save_goal_objective
this.cb_cancel_goal_objective=create cb_cancel_goal_objective
this.dw_objective=create dw_objective
this.Control[]={this.dw_goal_list,&
this.dw_goal,&
this.cb_add_goal,&
this.dw_objective_list,&
this.cb_add_objective,&
this.cb_save_goal_objective,&
this.cb_cancel_goal_objective,&
this.dw_objective}
end on

on tabpage_goals_objectives.destroy
destroy(this.dw_goal_list)
destroy(this.dw_goal)
destroy(this.cb_add_goal)
destroy(this.dw_objective_list)
destroy(this.cb_add_objective)
destroy(this.cb_save_goal_objective)
destroy(this.cb_cancel_goal_objective)
destroy(this.dw_objective)
end on

type dw_goal_list from u_dw_online within tabpage_goals_objectives
event rowfocuschanged pbm_dwnrowchange
integer y = 12
integer width = 2624
integer height = 328
integer taborder = 2
string dataobject = "d_goal_list"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;call super::rowfocuschanged;INTEGER	li_result
LONG		ll_row, ll_list_row_count,  ll_current_row
	
	IF THIS.RowCount() = 0 THEN
		inv_goals_objectives.nf_insert(0)
		cb_add_goal.enabled = FALSE
		cb_add_objective.enabled = FALSE
		RETURN
	END IF
	
	ll_row = This.GetRow()
	IF ll_row > 0 THEN
		IF cb_add_goal.Enabled = FALSE THEN cb_add_goal.Enabled = TRUE
		IF cb_add_objective.enabled = FALSE THEN cb_add_objective.enabled = TRUE
		
		This.SelectRow(0,FALSE)
		This.SelectRow(ll_row,TRUE)

/*		Retrieve and display goal and retrieve the objective list for the goal                                                              
*/
		li_result = inv_goals_objectives.nf_retrieve_goal(THIS.GetItemNumber(ll_row,'goal_no'))


		ll_list_row_count = tab_rehab_plan.tabpage_goals_objectives.dw_objective_list.Retrieve(il_claim_no,THIS.GetItemNumber(ll_row,'goal_no'))
		ll_current_row = tab_rehab_plan.tabpage_goals_objectives.dw_objective_list.GetRow()
		IF ll_list_row_count > 0 AND (ll_list_row_count = 0  OR ll_current_row = 1) THEN 	// if current row was 0 or 1 then the rowfocuschanged event 
																														// may not have fired
			tab_rehab_plan.tabpage_goals_objectives.dw_objective_list.SetRow(1)
			tab_rehab_plan.tabpage_goals_objectives.dw_objective_list.uf_processselect(1,'KeyBoard')
			tab_rehab_plan.tabpage_goals_objectives.dw_objective_list.TriggerEvent(RowFocusChanged!)
		END IF


	END IF









end event

event constructor;/* Overridden */
end event

type dw_goal from u_dw_online within tabpage_goals_objectives
integer y = 328
integer width = 2624
integer height = 364
integer taborder = 2
string dataobject = "d_goal"
boolean border = false
end type

event itemchanged;call super::itemchanged;LONG ll_return_code

cb_save_goal_objective.enabled = TRUE
cb_cancel_goal_objective.enabled = TRUE
uf_set_pbmessage(TRUE)

wf_save_indicator()

ll_return_code = inv_goals_objectives.nf_change_item(1)
RETURN ll_return_code
end event

event constructor;/* Overridden */
end event

type cb_add_goal from commandbutton within tabpage_goals_objectives
event clicked pbm_bnclicked
integer x = 9
integer y = 700
integer width = 379
integer height = 100
integer taborder = 3
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Add &Goal"
end type

event clicked;SetPointer(HourGlass!)

/* Ensure that all objectives for all goals are completed/cancelled	prior to adding a goal */
IF inv_goals_objectives.nf_check_outcomes() < 0 THEN	RETURN 
IF inv_goals_objectives.nf_insert(0) < 0 THEN	 RETURN

dw_goal_list.enabled 						= FALSE	
dw_goal_list.SelectRow(0,FALSE)
dw_objective_list.enabled 				= FALSE
dw_objective_list.Reset()
cb_add_goal.enabled 					= FALSE
cb_add_objective.enabled 				= FALSE
cb_cancel_goal_objective.enabled 	= TRUE

end event

type dw_objective_list from u_dw_online within tabpage_goals_objectives
event rowfocuschanged pbm_dwnrowchange
integer y = 816
integer width = 2624
integer height = 332
integer taborder = 2
string dataobject = "d_objective_list"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;call super::rowfocuschanged;INTEGER	li_result
LONG		ll_row	
	
	IF THIS.RowCount() = 0 THEN
		dw_objective.InsertRow(0)
		RETURN
	END IF
	ll_row = THIS.GetRow()
	IF ll_row > 0 THEN
		This.SelectRow(0,FALSE)
		This.SelectRow(ll_row,TRUE)

/*		Retrieve and display objective                                                        
*/
		li_result = inv_goals_objectives.nf_retrieve_objective(THIS.GetItemNumber(ll_row,'goal_no'),THIS.GetItemNumber(ll_row,'objective_no'))
		
	END IF



end event

event constructor;/* Overridden */
end event

type cb_add_objective from commandbutton within tabpage_goals_objectives
event clicked pbm_bnclicked
integer x = 14
integer y = 1576
integer width = 384
integer height = 88
integer taborder = 12
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Add Obj"
end type

event clicked;SetPointer(HourGlass!)

IF inv_goals_objectives.nf_insert_objective(0) < 0 THEN	RETURN

dw_objective_list.enabled 				= FALSE
dw_objective_list.SelectRow(0,FALSE)
dw_goal_list.enabled 						= FALSE	
dw_goal.Enabled 							= FALSE
cb_add_objective.enabled 				= FALSE
cb_add_goal.enabled 					= FALSE
cb_cancel_goal_objective.enabled 	= TRUE

end event

type cb_save_goal_objective from commandbutton within tabpage_goals_objectives
event clicked pbm_bnclicked
integer x = 2345
integer y = 1576
integer width = 384
integer height = 88
integer taborder = 14
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Save"
end type

event clicked;LONG     ll_row_count1, ll_row_count2
INTEGER  li_trancount


SetPointer(HourGlass!)

ll_row_count1 = dw_goal_list.RowCount()
ll_row_count2 = dw_objective_list.RowCount()

SQLCA.nf_begin_transaction()

IF inv_goals_objectives.nf_save() = 0 THEN
	
	SQLCA.nf_commit_transaction()
	
	wf_retrieve_goal_objective_lists()
	cb_save_goal_objective.Enabled = FALSE
	cb_cancel_goal_objective.Enabled = FALSE
	cb_add_goal.Enabled = TRUE
	dw_goal_list.Enabled = TRUE
	dw_goal.Enabled = TRUE
	cb_add_objective.Enabled = TRUE
	dw_objective_list.Enabled = TRUE
	tab_rehab_plan.tabpage_authorizations.enabled = TRUE
	tab_rehab_plan.tabpage_case_monitoring.enabled = TRUE
	tab_rehab_plan.tabpage_progress.enabled = TRUE
	tab_rehab_plan.tabpage_task.enabled = TRUE
	tab_rehab_plan.tabpage_voc_profile.enabled = TRUE
	tab_rehab_plan.tabpage_action_item.enabled = True
	wf_save_indicator()
	IF ll_row_count1 = 0 AND ll_row_count2 = 0 THEN
		// Run remote print here !!!
		wf_accident_request()
	END IF
	/* refresh Goal History report
	*/
	tab_rehab_plan_viewer.tabpage_goal_history.dw_goal_history.Retrieve(il_claim_no)
ELSE
	SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
	IF li_trancount > 0 THEN
		SQLCA.nf_rollback_transaction()
	END IF
END IF

end event

type cb_cancel_goal_objective from commandbutton within tabpage_goals_objectives
event clicked pbm_bnclicked
integer x = 2734
integer y = 1576
integer width = 384
integer height = 88
integer taborder = 15
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Cance&l"
end type

event clicked;	SetPointer(HourGlass!)

	cb_save_goal_objective.Enabled = FALSE //Rob Head 98/08/21
	wf_save_indicator() //Rob Head 98/08/21
	
/*	retrieve the last payment worked on
*/

	IF dw_goal_list.RowCount() = 0 THEN
		cb_add_goal.Enabled = TRUE
		cb_add_objective.Enabled = FALSE
		dw_goal.Reset()
		dw_objective.Reset()
		RETURN
	END IF

	dw_goal_list.Enabled = TRUE
	dw_goal.Enabled = TRUE
	dw_objective_list.Enabled = TRUE
	IF wf_retrieve_goal_objective_lists() > 0 THEN
		dw_goal_list.uf_processselect(1,'KeyBoard')
		dw_objective_list.uf_processselect(1,'KeyBoard')
		cb_cancel_goal_objective.Enabled = FALSE
		
	END IF
	cb_add_goal.Enabled = TRUE
	cb_add_objective.Enabled = TRUE	

	RETURN
end event

type dw_objective from u_dw_online within tabpage_goals_objectives
integer y = 1088
integer width = 2624
integer height = 472
integer taborder = 2
string dataobject = "d_objective"
boolean border = false
boolean livescroll = false
end type

event itemchanged;LONG ll_return_code

	cb_save_goal_objective.enabled = TRUE
	cb_cancel_goal_objective.enabled = TRUE
	uf_set_pbmessage(TRUE)
	wf_save_indicator()

	ll_return_code = inv_goals_objectives.nf_change_item(2)

RETURN ll_return_code
end event

event constructor;/* Overridden */
end event

type tabpage_task from userobject within tab_rehab_plan
integer x = 18
integer y = 168
integer width = 3118
integer height = 1676
long backcolor = 67108864
string text = "Task"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_insert_rehab_task_authorization dw_insert_rehab_task_authorization
cb_add_task cb_add_task
cb_save_task cb_save_task
cb_cancel_task cb_cancel_task
cb_send cb_send
cb_attach_doc_tsk cb_attach_doc_tsk
dw_task_authorizations dw_task_authorizations
cb_add_follow_up cb_add_follow_up
dw_task_list dw_task_list
pb_task_list_maximize pb_task_list_maximize
dw_task dw_task
dw_external_requestor dw_external_requestor
end type

on tabpage_task.create
this.dw_insert_rehab_task_authorization=create dw_insert_rehab_task_authorization
this.cb_add_task=create cb_add_task
this.cb_save_task=create cb_save_task
this.cb_cancel_task=create cb_cancel_task
this.cb_send=create cb_send
this.cb_attach_doc_tsk=create cb_attach_doc_tsk
this.dw_task_authorizations=create dw_task_authorizations
this.cb_add_follow_up=create cb_add_follow_up
this.dw_task_list=create dw_task_list
this.pb_task_list_maximize=create pb_task_list_maximize
this.dw_task=create dw_task
this.dw_external_requestor=create dw_external_requestor
this.Control[]={this.dw_insert_rehab_task_authorization,&
this.cb_add_task,&
this.cb_save_task,&
this.cb_cancel_task,&
this.cb_send,&
this.cb_attach_doc_tsk,&
this.dw_task_authorizations,&
this.cb_add_follow_up,&
this.dw_task_list,&
this.pb_task_list_maximize,&
this.dw_task,&
this.dw_external_requestor}
end on

on tabpage_task.destroy
destroy(this.dw_insert_rehab_task_authorization)
destroy(this.cb_add_task)
destroy(this.cb_save_task)
destroy(this.cb_cancel_task)
destroy(this.cb_send)
destroy(this.cb_attach_doc_tsk)
destroy(this.dw_task_authorizations)
destroy(this.cb_add_follow_up)
destroy(this.dw_task_list)
destroy(this.pb_task_list_maximize)
destroy(this.dw_task)
destroy(this.dw_external_requestor)
end on

type dw_insert_rehab_task_authorization from u_dw_online within tabpage_task
boolean visible = false
integer x = 1874
integer y = 836
integer width = 686
integer height = 400
integer taborder = 12
string title = "none"
string dataobject = "d_rehab_task_authorization_insert"
boolean border = false
end type

type cb_add_task from commandbutton within tabpage_task
integer x = 14
integer y = 1572
integer width = 329
integer height = 88
integer taborder = 3
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Add Task"
end type

event clicked;STRING							ls_filter
LONG							ll_row
DATAWINDOWCHILD		ldwc_child

il_cRow = dw_task_list.getrow()
SetPointer(HourGlass!)

IF inv_tasks.nf_insert(0) < 0 THEN	RETURN
	
dw_task_list.enabled 				= FALSE
cb_cancel_task.enabled 			= TRUE
cb_save_task.enabled 			= TRUE
cb_add_task.enabled 			= FALSE
cb_attach_doc_tsk.enabled 	= FALSE
cb_send.enabled 					= FALSE
cb_add_follow_up.enabled 	= FALSE
tab_rehab_plan.tabpage_task.dw_task.enabled 						= TRUE
tab_rehab_plan.tabpage_task.dw_external_requestor.enabled = TRUE
//tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.enabled = FALSE

/*	ib_new_task used in n_task functions to determine if filter needs to be set to filter on active
	tasks only (users cannot choose inactive task types for new tasks, but need to be able to
	view inactive info. for pre-existing tasks)
*/
ib_new_task = TRUE
inv_tasks.nf_set_new_task(ib_new_task)

/* Filter Task Type dddw so only active task types are displayed (i.e. user can only select
	an active task type when creating a new task).  and Task_Specific.task_entry_flag = 'Y'
*/
ls_filter 	= "active_flag = 'Y' AND task_entry_flag = 'Y'"
ll_row 	= dw_task.GetChild('task_xref_display', ldwc_child)
IF ll_row > 0 THEN
	ldwc_child.SetFilter(ls_filter)
	ldwc_child.Filter()
END IF

/* For new tasks, default task success radio group to invisible.  If task type which requires
a task success is selected (currently only Surgery & Treatment types require a task success),
the radio group will become visible from changeitem ... Added May 1/98 by EMcD. */
dw_task.Object.task_success_code.Visible = 0

RETURN 0





end event

type cb_save_task from commandbutton within tabpage_task
integer x = 1600
integer y = 1572
integer width = 329
integer height = 88
integer taborder = 4
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
end type

event clicked;DATAWINDOWCHILD		ldwc_child
LONG						ll_row, ll_cRow,	ll_task_no
INTEGER              li_trancount


dw_task.Accepttext()

inv_tab_controller.ib_do_row_selection = False

SetPointer(HourGlass!)
w_rehab_sheet.setredraw(false)
ll_cRow 		= dw_task_list.getrow()
ll_task_no 		= dw_task.GetItemNumber(1,'task_no')

SQLCA.nf_begin_transaction()

IF inv_tasks.nf_save() >= 0 THEN

	SQLCA.nf_commit_transaction()
	
	/* turn active_flag task type filter off*/	
	ll_task_no = dw_task.GetItemNumber(1,'task_no')
	ll_row 		= dw_task.GetChild('task_xref_display', ldwc_child)
	IF ll_row > 0 THEN
		ldwc_child.SetFilter("")
		ldwc_child.Filter()
	ELSE
		w_rehab_sheet.setredraw(true)
		RETURN -1
	END IF

/*	reset 'new task' boolean to false so active flag filter not in effect until next new task (i.e. user
	clicks add button again)
*/	
   ib_new_task = FALSE
	inv_tasks.nf_set_new_task(ib_new_task)
	
	wf_retrieve_task_list()
	
	If inv_tasks.ib_canceled_related_action_items or inv_tasks.ib_rescheduled_related_action_items THen
		wf_retrieve_action_item_list()
		If inv_tasks.ib_canceled_related_action_items THen
			MessageBox('Follow-ups cancelled','Some follow-ups were automatically cancelled because you cancelled or completed this task.',Information!,Ok!)
		End if	
	End if
	
	cb_save_task.enabled 		= FALSE
	cb_cancel_task.enabled 		= FALSE
	cb_attach_doc_tsk.enabled 	= TRUE
	dw_task_list.enabled 		= TRUE
	cb_add_task.enabled 			= TRUE
	cb_send.enabled 				= TRUE
//	tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.enabled = TRUE
	
	tab_rehab_plan_viewer.tabpage_tasks_and_progress.cb_refresh_viewer.TriggerEvent(Clicked!)
	
	/* refresh rehab viewer authorizations tab report */
	tab_rehab_plan_viewer.tabpage_authorizations_viewer.dw_rehab_viewer_authorizations.Retrieve(il_claim_no)
	tab_rehab_plan_viewer.tabpage_authorizations_viewer.dw_rehab_viewer_authorizations.SelectRow(0, FALSE)

	wf_save_indicator()
ELSE
	SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
	IF li_trancount > 0 THEN
		SQLCA.nf_rollback_transaction()
	END IF
	
	ImageTrans.nf_transaction_count(li_trancount,0,'','','',FALSE)
	IF li_trancount > 0 THEN
		ImageTrans.nf_rollback_transaction()
	END IF
	
END IF

inv_tab_controller.ib_do_row_selection = True
inv_tab_controller.nf_current_task_no(ll_task_no)



w_rehab_sheet.setredraw(true)
end event

type cb_cancel_task from commandbutton within tabpage_task
integer x = 1934
integer y = 1572
integer width = 329
integer height = 88
integer taborder = 5
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Cance&l"
end type

event clicked;DATAWINDOWCHILD	ldwc_child
LONG					ll_row, li_row
STRING					ls_rehab_service
DATETIME				ldtm_rehab_create_date

SetPointer(HourGlass!)
w_rehab_sheet.setredraw(false)
cb_save_task.enabled = FALSE //Rob Head 98/08/21.
wf_save_indicator() //Rob Head 98/08/21.

/*	Retrieve the last task worked on. */
IF dw_task_list.RowCount() = 0 THEN
	cb_add_task.enabled = TRUE
	dw_task.Reset()
	dw_external_requestor.Reset()
	cb_add_task.enabled 		= TRUE
	cb_send.enabled 				= TRUE
	cb_cancel_task.enabled 		= FALSE
	w_rehab_sheet.setredraw(true)
	RETURN
END IF

dw_task_list.enabled = TRUE

/* turn active_flag task type filter off*/	
ll_row = dw_task.GetChild('task_xref_display', ldwc_child)
IF ll_row > 0 THEN
	ldwc_child.SetFilter("")
	ldwc_child.Filter()
ELSE
	w_rehab_sheet.setredraw(true)
	dw_task_list.setrow(il_cRow)
	RETURN -1
END IF

/*	reset 'new task' boolean to false so active flag filter not in effect until next new task (i.e. user
	clicks add button again)
*/	
ib_new_task = FALSE
inv_tasks.nf_set_new_task(ib_new_task)

inv_tasks.nf_retrieve(dw_task_list.GetItemNumber(ll_row,"claim_no"),dw_task_list.GetItemNumber(dw_task_list.GetRow(),"task_no"))

cb_add_task.enabled 			= TRUE
cb_send.enabled 					= TRUE
dw_task_list.enabled 				= TRUE
cb_attach_doc_tsk.enabled 	= TRUE
cb_cancel_task.enabled 			= FALSE
//tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.enabled = TRUE

/*  disable planned start (set protect on/off & change background to grey for protected & white for unprotected) */
dw_task_list.setrow(il_cRow)
w_rehab_sheet.setredraw(true)

li_row =  tab_rehab_plan.tabpage_task.dw_task_list.getrow()
IF li_row > 0 THEN 

	// grab the service 
	ls_rehab_service 			 = tab_rehab_plan.tabpage_task.dw_task_list.getitemstring(li_row,'rehab_service_code')
	ldtm_rehab_create_date = tab_rehab_plan.tabpage_task.dw_task_list.getitemdatetime(li_row, 'create_date')
	
	IF ls_rehab_service = 'S022' AND DATE(ldtm_rehab_create_date) < idt_ephysio_br_check_date THEN
		tab_rehab_plan.tabpage_task.dw_task.enabled = FALSE
		tab_rehab_plan.tabpage_task.dw_external_requestor.enabled = FALSE
	ELSE
		tab_rehab_plan.tabpage_task.dw_task.enabled = TRUE
		tab_rehab_plan.tabpage_task.dw_external_requestor.enabled = TRUE
	END IF
END IF 

RETURN
	
end event

type cb_send from commandbutton within tabpage_task
integer x = 2395
integer y = 1572
integer width = 329
integer height = 88
integer taborder = 6
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Send &Msg"
end type

event clicked;S_SEND_DOC_PARAMETERS	ls_send_doc_parameters

/* Check to make sure that a claim exists.  Rob Head 98/09/14. */
If dw_basic_claim.RowCount() = 0 Then
	MessageBox('Error', 'Unable to perform process when a claim is not active in basic claim window.')
	Return -1
End If
	
ls_send_doc_parameters.msg_mode = TRUE
ls_send_doc_parameters.claim_no =  dw_basic_claim.GetItemNumber(1,'claim_no') // il_claim_no
IF dw_task.RowCount() <= 0 THEN
	MessageBox('Warning','There is no task selected to send.')
	Return
ELSE
	ls_send_doc_parameters.document_list = dw_task
END IF
OpenWithParm(w_send_folder_rehab,ls_send_doc_parameters)

end event

type cb_attach_doc_tsk from commandbutton within tabpage_task
event clicked pbm_bnclicked
integer x = 347
integer y = 1572
integer width = 329
integer height = 88
integer taborder = 13
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Attach &Doc"
end type

event clicked;LONG			ll_row,	ll_docid, ll_current_task_no, ll_result
INTEGER 	 	li_event_no
STRING			ls_document_type


/*	Verify the document selected is the document to be attached to the Rehab Plan. */
ll_row = dw_documents.GetRow()

IF ll_row > 0 THEN
	ll_docid 						= dw_documents.GetItemNumber(ll_row,'ref_docid')
	ls_document_type 	= dw_documents.GetItemString(ll_row,'document_type_document_type_desc')
	li_event_no 				= 0
	ll_result = inv_rehab_progress.nf_check_selected_doc(ll_docid,ls_document_type,li_event_no)
	IF ll_result = 0 THEN
		
		ll_current_task_no = inv_tab_controller.nf_current_task_no()
		
		//This will not allow an automatic tab selection based on the record type in the viewer
		ib_do_tab_selection 								= False
		inv_tab_controller.ib_do_row_selection 	= False
		tab_rehab_plan_viewer.tabpage_tasks_and_progress.cb_refresh_viewer.TriggerEvent(Clicked!)
		inv_tab_controller.ib_do_row_selection = True
		//Set the task number back to what it was before we refreshed the viewer.
		inv_tab_controller.nf_current_task_no(ll_current_task_no)
		ib_do_tab_selection = True
	END IF
END IF 

end event

type dw_task_authorizations from u_dw_online within tabpage_task
boolean visible = false
integer x = 407
integer y = 1216
integer height = 360
integer taborder = 2
string dataobject = "d_task_authorizations"
end type

event constructor;/* Overridden */
end event

type cb_add_follow_up from commandbutton within tabpage_task
integer x = 859
integer y = 1572
integer width = 416
integer height = 88
integer taborder = 12
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Add Follow-up"
end type

event clicked;LONG		ll_current_row, ll_related_task_no

ll_current_row = tab_rehab_plan.tabpage_task.dw_task.GetRow()

ll_related_task_no = tab_rehab_plan.tabpage_task.dw_task.GetItemNumber(ll_current_row ,'task_no')
If ll_related_task_no > 0 Then
	wf_add_action_item(ll_related_task_no )
Else
	SignalError(-666,'Error getting related task_no for new follow-up')
End if
end event

type dw_task_list from u_dw_online within tabpage_task
integer y = 4
integer width = 2629
integer height = 352
integer taborder = 2
boolean bringtotop = true
string dataobject = "d_task_tab_task_list"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;call super::rowfocuschanged;LONG			ll_current_tab
STRING			ls_rehab_service
DATETIME		ldtm_rehab_create_date

ll_current_tab = tab_rehab_plan.SelectedTab
IF ll_current_tab <> 2 Then
	ib_delayed_task_tab_retrieve = True
	return
Else
	wf_retrieve_task_details()
	ib_delayed_task_tab_retrieve = False
End if









end event

event constructor;/* Overridden */

pb_task_list_maximize.uf_set_requestor(This)
end event

event doubleclicked;call super::doubleclicked;S_WINDOW_MESSAGE		lstr_message


If dwo.name = 'c_follow_up_flag' Then
	
	If this.GetItemNumber(row,'follow_up_count') > 0 THen
	
		lstr_message.al_doubleparm[1] = il_claim_no
		lstr_message.al_doubleparm[2] = il_task_no
		lstr_message.as_stringparm[1] = ""
		OpenWithParm(w_view_rehab_plan_task_follow_up_list, lstr_message)
	End if
end if
end event

event retrieveend;call super::retrieveend;INTEGER		li_row
STRING			ls_rehab_service
DATETIME 	ldtm_rehab_create_date

/* Need a rule to prevent the user from modifying the task (especially the task status). 
	3.570	A task for the physiotherapy service must not be modified if the task was created prior to the date of the first Physio Deployment changes.
*/
li_row = THIS.GETROW()

IF li_row < 1 THEN RETURN 

// grab the service 
ls_rehab_service 			 = THIS.getitemstring(li_row,'rehab_service_code')
ldtm_rehab_create_date = THIS.getitemdatetime(li_row, 'create_date')

IF ls_rehab_service = 'S022' AND DATE(ldtm_rehab_create_date) < idt_ephysio_br_check_date THEN
	tab_rehab_plan.tabpage_task.dw_task.enabled = FALSE
	tab_rehab_plan.tabpage_task.dw_external_requestor.enabled = FALSE
ELSE
	tab_rehab_plan.tabpage_task.dw_task.enabled = TRUE
	tab_rehab_plan.tabpage_task.dw_external_requestor.enabled = TRUE
END IF
	 
end event

type pb_task_list_maximize from cb_dw_maximize within tabpage_task
integer x = 2459
integer y = 20
integer width = 91
integer height = 76
integer taborder = 12
boolean bringtotop = true
end type

type dw_task from u_dw_online within tabpage_task
integer y = 356
integer width = 2651
integer height = 928
integer taborder = 2
boolean bringtotop = true
string dataobject = "d_task_maintain"
boolean minbox = true
boolean vscrollbar = true
boolean border = false
end type

event itemchanged;call super::itemchanged;LONG	ll_return_value

uf_set_pbmessage(TRUE)
	
ll_return_value = inv_tasks.nf_change_item(1, row, dwo.Name, data)

cb_save_task.enabled 		= TRUE
cb_cancel_task.enabled 		= TRUE
wf_save_indicator()
	
RETURN ll_return_value
	
end event

event constructor;/* Overridden */

datawindowchild		ldwc_status

this.GetChild('task_status_code',ldwc_status)
ldwc_status.SetTransobject(SQLCA)

ldwc_status.Retrieve('Y','N') //task_entry_flag, action_item_entry_flag
end event

event clicked;call super::clicked;LONG							ll_docid
STRING							ls_doc_type, ls_type
S_WINDOW_MESSAGE	lstr_message

CHOOSE CASE dwo.name
	CASE 'p_referral'
			
		ll_docid = THIS.getitemnumber(row,'doc_id')
		
		//double check wwe have a valid number
		IF ll_docid > 0 THEN
			
			/*	Get the document id for selected row View the document */	
			IF uo_image_append.of_init(ll_docid)	<= 0 THEN RETURN
	
			ls_doc_type =  uo_image_append.of_get_file_type()
		
			CHOOSE CASE ls_doc_type
				/*  Imaged document */ 
				CASE 'IMA', 'TIF'
					IF uo_image_append.of_append_image(ll_docid) < 0 THEN	RETURN
				CASE ELSE
					iu_dw_document_path.f_manage_document(ll_docid,"V","NORMAL")
			END CHOOSE					
		END IF 
			
	CASE 'b_search_provider2'
		
		/*	get the type to search for */
		IF tab_rehab_plan.tabpage_task.dw_task.RowCount() > 0 THEN
			ls_type = tab_rehab_plan.tabpage_task.dw_task.GetItemString(1,'provider_type_code')
		
			IF ls_type = 'V' OR ls_type =  'M' OR ls_type =  'O' THEN
				OpenWithParm(w_service_provider_search, ls_type)
				lstr_message = Message.PowerObjectParm
				IF lstr_message.al_doubleparm[1] > 0 THEN
					tab_rehab_plan.tabpage_task.dw_task.SetColumn('provider_no')
					tab_rehab_plan.tabpage_task.dw_task.SetItem(1, 'provider_no', lstr_message.al_doubleparm[1])
					inv_tasks.nf_provider_address(ls_type, lstr_message.al_doubleparm[1], 1)
					
					//enable the save information
					cb_save_task.enabled 		= TRUE
					cb_cancel_task.enabled 		= TRUE
					wf_save_indicator()
				END IF
			ELSE
				MessageBox('Warning', 'No search available for this recipient type.')
			END IF
		END IF
	CASE ELSE
END CHOOSE
end event

type dw_external_requestor from u_dw_online within tabpage_task
integer y = 1288
integer width = 2651
integer height = 280
integer taborder = 2
string dataobject = "d_task_external_requestor"
boolean border = false
end type

event itemchanged;call super::itemchanged;LONG	ll_return_value

uf_set_pbmessage(TRUE)
	
ll_return_value = inv_tasks.nf_change_item(2,row,dwo.Name,data)

cb_save_task.enabled 		= TRUE
cb_cancel_task.enabled 		= TRUE
	
wf_save_indicator()
	
RETURN ll_return_value
	
end event

event constructor;/* Overridden */
end event

event clicked;call super::clicked;STRING							 	ls_type, ls_doc_type
LONG								ll_docid
S_WINDOW_MESSAGE		lstr_message

CHOOSE CASE dwo.name
		
	CASE 'b_search_provider2'

		/*	get the type to search for */
		IF tab_rehab_plan.tabpage_task.dw_external_requestor.RowCount() > 0 THEN
			ls_type = tab_rehab_plan.tabpage_task.dw_external_requestor.GetItemString(1,'referring_provider_type_code')
		
			IF ls_type = 'V' OR ls_type =  'M' OR ls_type =  'O' THEN
				OpenWithParm(w_service_provider_search, ls_type)
				lstr_message = Message.PowerObjectParm
				IF lstr_message.al_doubleparm[1] > 0 THEN
					tab_rehab_plan.tabpage_task.dw_external_requestor.SetColumn('referring_provider_no')
					tab_rehab_plan.tabpage_task.dw_external_requestor.SetItem(1, 'referring_provider_no', lstr_message.al_doubleparm[1])
					inv_tasks.nf_provider_address(ls_type, lstr_message.al_doubleparm[1], 2)
					
					//enable the save information
					cb_save_task.enabled 		= TRUE
					cb_cancel_task.enabled 		= TRUE
					wf_save_indicator()
				END IF
			ELSE
				MessageBox('Warning', 'No search available for this recipient type.')
			END IF
		END IF
		
		CASE 'p_referral'
			
		ll_docid = THIS.getitemnumber(row,'doc_id')
		
		//double check wwe have a valid number
		IF ll_docid > 0 THEN
			
			/*	Get the document id for selected row View the document */	
			IF uo_image_append.of_init(ll_docid)	<= 0 THEN RETURN
	
			ls_doc_type =  uo_image_append.of_get_file_type()
		
			CHOOSE CASE ls_doc_type
				/*  Imaged document */ 
				CASE 'IMA', 'TIF'
					IF uo_image_append.of_append_image(ll_docid) < 0 THEN	RETURN
				CASE ELSE
					iu_dw_document_path.f_manage_document(ll_docid,"V","NORMAL")
			END CHOOSE					
		END IF 

	CASE ELSE
END CHOOSE
end event

type tabpage_authorizations from userobject within tab_rehab_plan
integer x = 18
integer y = 168
integer width = 3118
integer height = 1676
long backcolor = 67108864
string text = "Authorizations"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
cb_delete cb_delete
dw_authorizations dw_authorizations
dw_authorization_task_list dw_authorization_task_list
cb_add_authorization cb_add_authorization
dw_task_authorization_list dw_task_authorization_list
cb_authorization_save cb_authorization_save
cb_authorization_cancel cb_authorization_cancel
dw_auth_let_provider_info dw_auth_let_provider_info
dw_auth_let_toll_free_no dw_auth_let_toll_free_no
end type

on tabpage_authorizations.create
this.cb_delete=create cb_delete
this.dw_authorizations=create dw_authorizations
this.dw_authorization_task_list=create dw_authorization_task_list
this.cb_add_authorization=create cb_add_authorization
this.dw_task_authorization_list=create dw_task_authorization_list
this.cb_authorization_save=create cb_authorization_save
this.cb_authorization_cancel=create cb_authorization_cancel
this.dw_auth_let_provider_info=create dw_auth_let_provider_info
this.dw_auth_let_toll_free_no=create dw_auth_let_toll_free_no
this.Control[]={this.cb_delete,&
this.dw_authorizations,&
this.dw_authorization_task_list,&
this.cb_add_authorization,&
this.dw_task_authorization_list,&
this.cb_authorization_save,&
this.cb_authorization_cancel,&
this.dw_auth_let_provider_info,&
this.dw_auth_let_toll_free_no}
end on

on tabpage_authorizations.destroy
destroy(this.cb_delete)
destroy(this.dw_authorizations)
destroy(this.dw_authorization_task_list)
destroy(this.cb_add_authorization)
destroy(this.dw_task_authorization_list)
destroy(this.cb_authorization_save)
destroy(this.cb_authorization_cancel)
destroy(this.dw_auth_let_provider_info)
destroy(this.dw_auth_let_toll_free_no)
end on

type cb_delete from commandbutton within tabpage_authorizations
integer x = 434
integer y = 1592
integer width = 411
integer height = 88
integer taborder = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Delete"
end type

event clicked;/*
Delete button
A new button is needed to allow the user to delete an authorization; 
there are restrictions on when an authorization can be deleted (refer to the business rules). 

If an manual authorization was created in error and no billing has been received (paid amount is zero [addition paid_quantity = 0]), 
the user will be allowed to delete it. The claim manager will be responsible for contacting the service provider that is responsible for the authorization. 
An audit trail record will be created for the deleted authorization record.
*/
LONG		ll_authorization_row, ll_task_authorization_row, ll_authorization_no, ll_billable_xref_no, ll_task_no
DECIMAL	ldec_paid_amount, ldec_paid_quantity
STRING		ls_auto_created_flag, ls_manual_invoice_flag


/* Make sure the save button is not enabled */
IF cb_authorization_save.enabled = TRUE  THEN
	messagebox("Save Required","Please Save or Cancel before proceeding with Delete.")
	RETURN 
END IF 

//GRAB THE UNALTERED DATABASE VALUE
ll_task_authorization_row 		= dw_task_authorization_list.getrow()
ll_authorization_row 				= dw_authorizations.getrow()
IF ll_authorization_row 			<= 0 OR ISNULL(ll_authorization_row) THEN RETURN 
IF ll_task_authorization_row 	<= 0 OR ISNULL(ll_task_authorization_row) THEN RETURN 

ldec_paid_amount 		= dw_authorizations.getitemnumber(ll_authorization_row,'paid_amount',primary!,TRUE)
ldec_paid_quantity		= dw_authorizations.getitemnumber(ll_authorization_row,'paid_quantity',primary!,TRUE)
ls_auto_created_flag 	= dw_authorizations.getitemstring(ll_authorization_row,'auto_created_flag',primary!,TRUE)

ll_billable_xref_no 	= dw_task_authorization_list.getitemnumber(ll_task_authorization_row,'billable_xref_no')
ll_task_no 				= dw_task_authorization_list.getitemnumber(ll_task_authorization_row,'task_no')

	
/* 4.350	An authorization must not be deleted if the billable item has been partially or fully paid (the paid amount is greater than zero or the paid quantity is greater than zero).   */
IF 	ldec_paid_amount > 0 OR ldec_paid_quantity > 0 THEN 
	messagebox("Error","An authorization must not be deleted if the billable item has been partially or fully paid (the paid amount is greater than zero or the paid quantity is greater than zero).")
	RETURN 
END IF 

/* 4.355	An authorization must not be deleted if the billable item cannot be manually invoiced and the authorization was automatically created 
              (Billable_Item_Rehab_Task_Xref.manual_invoice_flag = ‘N’ */
SELECT manual_invoice_flag 
INTO 	:ls_manual_invoice_flag
FROM  	Billable_Item_Rehab_Task_Xref
WHERE billable_xref_no = :ll_billable_xref_no
USING 	SQLCA;
SQLCA.nf_Handle_Error("w_rehab_sheet","tab_rehab_plan.tabpage_authorizations.cb_delete","SELECT manual_invoice_flag")

IF ls_auto_created_flag = 'Y' AND ls_manual_invoice_flag = 'N' THEN 
	messagebox("Error","An authorization must not be deleted if the billable item cannot be manually invoiced and the authorization was automatically created.")
	RETURN 
END IF 
	
// Ask user if they really want to delete
IF MessageBox("Authorizations", "Do you wish to delete the current authorization?",Question!,YesNo!) = 2 Then
	RETURN
END IF

/* A authorization must not be deleted if the billable item has been partially or fully paid (the paid amount is greater than zero). */
ll_authorization_no = dw_task_authorization_list.getitemnumber(ll_task_authorization_row, 'authorization_no')
IF ll_authorization_no > 0 THEN 
	
	SQLCA.nf_begin_transaction()
			
	DELETE REHAB_TASK_AUTHORIZATION  WHERE authorization_no = :ll_authorization_no USING SQLCA;
	SQLCA.nf_Handle_Error("w_rehab_sheet","tab_rehab_plan.tabpage_authorizations.cb_delete","DELETE REHAB_TASK_AUTHORIZATION ")

	SQLCA.nf_commit_transaction()
			
	/* this will trigger a rowchange causing the correct information to stay current (won't move to a different record ) */
	dw_authorization_task_list.triggerevent(rowfocuschanged!)
			
END IF 
 		

end event

type dw_authorizations from u_dw_online within tabpage_authorizations
integer x = 9
integer y = 808
integer width = 2725
integer height = 756
integer taborder = 10
string dataobject = "d_task_authorizations"
boolean minbox = true
boolean border = false
boolean livescroll = false
end type

event itemchanged;call super::itemchanged;LONG	ll_return_value

uf_set_pbmessage(TRUE)
	
ll_return_value = inv_authorizations.nf_change_item(1,row,dwo.Name,data)
cb_authorization_save.enabled 		= TRUE
cb_authorization_cancel.enabled 	= TRUE
wf_save_indicator()
	
RETURN ll_return_value
	
end event

event constructor;/* Overridden */
end event

event clicked;call super::clicked;S_WINDOW_MESSAGE	lstr_message
STRING 						ls_type , ls_authorized_provider_type_code, ls_program_code, ls_service_code, ls_task_type_code
STRING							ls_task_sub_type_code, ls_task_specific_code
LONG 							ll_authorized_provider_no, ll_authorization_no, ll_billable_xref_no, ll_rehab_task_row, ll_claim_no
INTEGER						li_entry_scenario
DECIMAL						ldec_unit_price, ldec_max_authorized_amount
BOOLEAN						lb_check_authorized_entry = FALSE

IF row <= 0  THEN RETURN 

CHOOSE CASE dwo.name
	CASE 'b_provider_search'

			/*	get the type to search for */
		IF tab_rehab_plan.tabpage_authorizations.dw_authorizations.RowCount() > 0 THEN
			ls_type = tab_rehab_plan.tabpage_authorizations.dw_authorizations.GetItemString(1,'authorized_provider_type_code')
		
			IF ls_type = 'V' OR ls_type =  'M' OR ls_type =  'O' THEN
				OpenWithParm(w_service_provider_search, ls_type)
				lstr_message = Message.PowerObjectParm
				IF lstr_message.al_doubleparm[1] > 0 THEN
					tab_rehab_plan.tabpage_authorizations.dw_authorizations.SetColumn('authorized_provider_no')
					tab_rehab_plan.tabpage_authorizations.dw_authorizations.SetItem(1, 'authorized_provider_no', lstr_message.al_doubleparm[1])
					inv_authorizations.nf_provider_address(ls_type, lstr_message.al_doubleparm[1])
					cb_authorization_save.enabled 		= TRUE
					cb_authorization_cancel.enabled 	= TRUE	
					lb_check_authorized_entry 			= TRUE
					wf_save_indicator()
				END IF
			ELSE
				MessageBox('Warning', 'No search available for this recipient type.')
			END IF
		END IF
	CASE 'b_revision_history'//REHAB_TASK_AUTHORIZATION_auth_info_CHANGE	
		
		//grab the authorization number
		ll_authorization_no = this.getitemnumber(1,'authorization_no')
		
		//open the authorization change revision history window
		IF isnull(ll_authorization_no) OR ll_authorization_no < 0  THEN RETURN 1
		
		openwithparm(w_rehab_task_auth_info_change, ll_authorization_no)
		
	CASE 't_fixed_rate_item'
		
			ll_billable_xref_no = this.getitemnumber(row,'billable_xref_no')
			IF ISNULL(ll_billable_xref_no) OR ll_billable_xref_no = 0 THEN RETURN
		
			ldec_unit_price = inv_authorizations.nf_get_unit_price( ll_billable_xref_no, date(f_server_datetime()))
			messagebox('Fixed Rate Item','The current price for this Fixed Rate item is: ' + string(ldec_unit_price,'$#,##0.00;($#,##0.00)'))
			
	CASE 't_max_indicator'
		
			messagebox('Max Authorized Amount','The authorized amount is equal to the Max authorized amount for the Billable Item.')
				
	CASE ELSE
END CHOOSE

//need to check if the authorized amount is enterable
IF lb_check_authorized_entry = TRUE THEN
	
	ll_billable_xref_no 							= 	THIS.GetItemnumber(row,'billable_xref_no')
	ls_authorized_provider_type_code 	=  THIS.GetItemString(row,'authorized_provider_type_code')
	ll_authorized_provider_no				=  THIS.GetItemnumber(row,'authorized_provider_no')
	ll_claim_no  									= 	 tab_rehab_plan.tabpage_authorizations.dw_authorization_task_list.getitemnumber ( tab_rehab_plan.tabpage_authorizations.dw_authorization_task_list.getrow(), 'claim_no') 
	
	ldec_max_authorized_amount = inv_authorizations.nf_get_max_authorized_amount(ll_billable_xref_no)
	
	li_entry_scenario = inv_authorizations.nf_authorized_amount_entry_scenario(ll_billable_xref_no, ll_authorized_provider_no, ls_authorized_provider_type_code)
	inv_authorizations.nf_set_authorized_amount_scenario(li_entry_scenario, ll_claim_no, ldec_max_authorized_amount, 1)
END IF 







end event

event rowfocuschanged;call super::rowfocuschanged;STRING			ls_other_billable_item_flag
LONG			ll_billable_item_no, 	ll_billable_xref_no 		


IF currentrow < 1 THEN RETURN 
/*
	2.2.	Rehab Plan changes

	#1. Authorization tab
		The Authorization tab will have additional functionality for the user to enter a description of a billable item, 
		if the billable item requires this description. The other description field should only be displayed if it is applicable for the billable item.
*/
ll_billable_xref_no 		= 	THIS.GetItemnumber(currentrow,'billable_xref_no')
ll_billable_item_no 		= inv_authorizations.nf_get_billable_item_no(ll_billable_xref_no)
	
IF ll_billable_item_no > 0 THEN 
	ls_other_billable_item_flag = inv_authorizations.nf_get_other_billable_item_flag(ll_billable_item_no)
	IF ls_other_billable_item_flag = 'Y'  THEN 
		 THIS.Modify("t_other_billable_item_desc.Visible=1")
		 THIS.Modify("other_billable_item_desc.Visible=1")
	
	ELSE
		 THIS.setitem(currentrow,'other_billable_item_desc', '') 
		 THIS.Modify("t_other_billable_item_desc.Visible=0")
		 THIS.Modify("other_billable_item_desc.Visible=0")
	END IF 
END IF 
end event

type dw_authorization_task_list from u_dw_online within tabpage_authorizations
integer y = 12
integer width = 2629
integer height = 376
integer taborder = 20
string dataobject = "d_task_tab_task_list"
boolean vscrollbar = true
boolean livescroll = false
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;call super::rowfocuschanged;LONG			ll_current_tab
	
ll_current_tab = tab_rehab_plan.SelectedTab
IF ll_current_tab <> 3 THEN 
	ib_delayed_authorization_tab_retrieve = TRUE
	RETURN
ELSE
	ib_delayed_authorization_tab_retrieve = FALSE
	wf_retrieve_auth_details()
END IF



end event

event constructor;/* Overridden */
end event

event doubleclicked;call super::doubleclicked;S_WINDOW_MESSAGE		lstr_message

IF dwo.name = 'c_follow_up_flag' THEN	
	IF THIS.GetItemNumber(row,'follow_up_count') > 0 THEN
	
		lstr_message.al_doubleparm[1] 	= il_claim_no
		lstr_message.al_doubleparm[2] 	= il_task_no
		lstr_message.as_stringparm[1] 	= ""
		OpenWithParm(w_view_rehab_plan_task_follow_up_list, lstr_message)
	END IF
END IF
end event

type cb_add_authorization from commandbutton within tabpage_authorizations
integer x = 14
integer y = 1592
integer width = 411
integer height = 88
integer taborder = 3
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Add"
boolean default = true
end type

event clicked;STRING		ls_task_type_code, ls_filter, ls_service_code, ls_program_code, ls_task_sub_type_code, ls_task_specific_code
STRING		ls_task_status_code
LONG		ll_row, ll_result, ll_rehab_task_row

IF dw_authorization_task_list.RowCount() < 1 THEN
	MessageBox('Error','No tasks to authorize.')
	RETURN 
END IF
ll_row = dw_authorization_task_list.GetRow()
		
/*	ib_new_authorization used in n_authorization functions to determine if filter needs to be set
	to filter on active task types only (users cannot choose inactive task types for new authorizations,
	but need to be able to view inactive info. for pre-existing authorizations)
*/
ib_new_authorization = TRUE
inv_authorizations.nf_set_new_authorization(ib_new_authorization)

IF ll_row > 0 THEN
	ls_task_type_code 	= dw_authorization_task_list.GetItemString(ll_row,'task_type_code')
	ls_task_status_code	= dw_authorization_task_list.GetItemString(ll_row,'task_status_code') 
	
	/*
	4.390	An authorization must not be added  or modified on a task that is cancelled (task_status_code = ‘03’).

	On the Authorization tab:
	Task status is cancelled - Do not allow an authorization to be added (they could change the task status if it really shouldn’t be cancelled)
	
	task_status_code task_status_desc
	---------------- ----------------------------------------
	01               planned
	02               in progress
	03               cancelled
	04               closed
	05               reset
	*/
	IF ls_task_status_code = '03'  THEN 
		MessageBox('Task status is cancelled','Authorization cannot be added once the task has been cancelled')
		RETURN 	
	END IF 
	
	inv_authorizations.nf_insert(0,ls_task_type_code)
	dw_authorization_task_list.enabled = FALSE
	dw_task_authorization_list.enabled = FALSE
	cb_add_authorization.enabled 		= FALSE
	cb_authorization_save.enabled 		= TRUE
	cb_authorization_cancel.enabled 	= TRUE
	
	//filter the billable_xref_no - billable_item based on the SERVICE/PROGRAM/TYPE/SUB_TYPE/SPECIFIC
	ll_rehab_task_row =  dw_authorization_task_list.Getrow()
	IF ISNULL(ll_rehab_task_row) OR ll_rehab_task_row < 1 THEN RETURN
	
	ls_program_code 			= dw_authorization_task_list.GetItemString(ll_rehab_task_row,'rehab_program_code')
	ls_service_code 				= dw_authorization_task_list.GetItemString(ll_rehab_task_row,'rehab_service_code')
	ls_task_type_code  		= dw_authorization_task_list.GetItemString(ll_rehab_task_row, 'task_type_code')
	ls_task_sub_type_code 	= dw_authorization_task_list.GetItemString(ll_rehab_task_row, 'task_sub_type_code')
	ls_task_specific_code 	= dw_authorization_task_list.GetItemString(ll_rehab_task_row,'task_specific_code')
	
	inv_authorizations.nf_set_billable_item_xref_filter(ls_service_code,ls_program_code,ls_task_type_code,ls_task_sub_type_code,ls_task_specific_code, TRUE)
	
ELSE
	MessageBox('Error','No task selected.  Please select a task first.')
	RETURN
END IF


end event

type dw_task_authorization_list from u_dw_online within tabpage_authorizations
integer y = 404
integer width = 2629
integer height = 404
integer taborder = 2
string dataobject = "d_task_authorization_list"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;call super::rowfocuschanged;LONG		ll_row, ll_authorization_row, ll_provider_no, ll_rehab_task_row, ll_billable_xref_no
DECIMAL	ldec_paid_amount, ldec_paid_quantity
STRING		ls_auto_created_flag, ls_fixed_fee_flag, ls_provider_type, ls_manual_invoice_flag
STRING		ls_service_code, ls_program_code, ls_task_type_code, ls_task_sub_type_code, ls_task_specific_code

IF THIS.RowCount() = 0 THEN
	dw_authorizations.Reset()
	RETURN
END IF
	
ll_row = THIS.GetRow()	// the currentrow argument was not used because it does not always have a value

IF ll_row > 0 THEN
	THIS.SelectRow(0,FALSE)
	THIS.SelectRow(ll_row,TRUE)

	/*	Retrieve and display task details   */
	inv_authorizations.nf_retrieve(il_claim_no, THIS.GetItemNumber(ll_row,"task_no"), THIS.GetItemNumber(ll_row,"authorization_no"))
	
	/* if the paid amount is = 0 than enable the delete button */
	ll_authorization_row = dw_authorizations.getrow()
	IF ll_authorization_row > 0 THEN 
		ldec_paid_amount 		= dw_authorizations.getitemnumber(ll_authorization_row,'paid_amount',primary!, TRUE)
		ldec_paid_quantity		= dw_authorizations.getitemnumber(ll_authorization_row,'paid_quantity',primary!,TRUE)
		ls_auto_created_flag 	= dw_authorizations.getitemstring(ll_authorization_row,'auto_created_flag',primary!,TRUE)
		ls_provider_type		= dw_authorizations.getitemstring(ll_authorization_row,'authorized_provider_type_code')
		ll_provider_no			= dw_authorizations.getitemnumber(ll_authorization_row,'authorized_provider_no')
		ll_billable_xref_no		= dw_authorizations.getitemnumber(ll_authorization_row,'billable_xref_no')
		
		//default the provider name 
		inv_authorizations.nf_provider_address(ls_provider_type, ll_provider_no)
	
		/*
		4.	The DELETE button is not enabled for an Authorization that was created – it isn’t paid yet and it can be manually invoiced (e.g. it created an auth for the Treatment but I cannot delete it & I should be able to)
		4.350	An authorization must not be deleted if the billable item has been partially or fully paid (the paid amount is greater than zero or the paid quantity is greater than zero).   
		4.350	An authorization must not be deleted if the billable item cannot be manually invoiced and the authorization was automatically created (Billable_Item_Rehab_Task_Xref.manual_invoice_flag = ‘N’
		*/
		
		SELECT manual_invoice_flag 
		INTO 	:ls_manual_invoice_flag
		FROM  	Billable_Item_Rehab_Task_Xref
		WHERE billable_xref_no = :ll_billable_xref_no
		USING 	SQLCA;
		SQLCA.nf_Handle_Error("w_rehab_sheet","tab_rehab_plan.tabpage_authorizations.dw_task_authorization_list.rowfocuschanged","SELECT manual_invoice_flag")
		
		IF  ldec_paid_amount > 0 OR ldec_paid_quantity > 0 THEN 
			cb_delete.enabled = FALSE
		ELSE
			IF ls_auto_created_flag = 'Y' AND ls_manual_invoice_flag = 'N' THEN
				cb_delete.enabled = FALSE
			ELSE		
				cb_delete.enabled = TRUE
			END IF 
		END IF 
		
		/* set some default behaviour */
		ls_fixed_fee_flag							= dw_authorizations.GetItemstring(1,'fixed_fee_flag')
		
		//filter the billable_xref_no - billable_item based on the SERVICE/PROGRAM/TYPE/SUB_TYPE/SPECIFIC
		ll_rehab_task_row =  dw_authorization_task_list.Getrow()
		IF ISNULL(ll_rehab_task_row) OR ll_rehab_task_row < 1 THEN RETURN
			
		ls_program_code 			= dw_authorization_task_list.GetItemString(ll_rehab_task_row,'rehab_program_code')
		ls_service_code 				= dw_authorization_task_list.GetItemString(ll_rehab_task_row,'rehab_service_code')
		ls_task_type_code  		= dw_authorization_task_list.GetItemString(ll_rehab_task_row, 'task_type_code')
		ls_task_sub_type_code 	= dw_authorization_task_list.GetItemString(ll_rehab_task_row, 'task_sub_type_code')
		ls_task_specific_code 	= dw_authorization_task_list.GetItemString(ll_rehab_task_row,'task_specific_code')
			
		inv_authorizations.nf_set_billable_item_xref_filter(ls_service_code, ls_program_code, ls_task_type_code, ls_task_sub_type_code, ls_task_specific_code, FALSE)

/*
>>The paid amount must not be entered. [DATAWINDOW]
>>The paid quantity must not be entered.  [DATAWINDOW]
>>The authorized by user must not be entered.  [DATAWINDOW]
>>The authorized date must not be entered. [DATAWINDOW]
>>The authorized amount must not be entered if the billable item has a fixed fee rate. [DATAWINDOW]
*/

/* >>The expedited service flag must be either Yes or No. [WHERE IS THIS FLAG????????] */
		
	END IF 
END IF

dw_authorizations.SetColumn ( 'billable_xref_no' )
dw_authorizations.setfocus()




end event

event constructor;/* Overridden */
end event

type cb_authorization_save from commandbutton within tabpage_authorizations
integer x = 1879
integer y = 1592
integer width = 411
integer height = 88
integer taborder = 4
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Save"
end type

event clicked;LONG							ll_claim_no, ll_row, ll_authorization_no, li_find
INTEGER						li_message, li_trancount
STRING							ls_task_status_code, ls_find
DATAWINDOWCHILD		ldwc_child
dwItemStatus				l_task_status_code_status, l_row_status

ll_claim_no = il_claim_no

SetPointer(HourGlass!)
	
/*
On the Authorization tab:
4.400	The user must be warned if an authorization is added or modified for a task that is closed (task_status_code = ‘04’).
	
task_status_code task_status_desc
---------------- ----------------------------------------
01               planned
02               in progress
03               cancelled
04               closed
05               reset
*/
ll_row = dw_authorization_task_list.getrow()

IF ll_row < 1  THEN RETURN 

ls_task_status_code			= 	dw_authorization_task_list.GetItemString(ll_row,'task_status_code') 		
l_task_status_code_status	= 	dw_authorization_task_list.GetItemStatus(ll_row,'task_status_code', Primary!)


IF  ls_task_status_code = '04' THEN 
	
		li_message = MessageBox('Closed', "The Task Status is closed." +&
														"~rContinue with Save?",Question!,OKCancel!, 2)
		IF li_message = 2 THEN RETURN -1 		
END IF 

SQLCA.nf_begin_transaction()

IF inv_authorizations.nf_save() < 0 THEN
	SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
	IF li_trancount > 0 THEN
		SQLCA.nf_rollback_transaction()
	END IF
	RETURN
ELSE
	SQLCA.nf_commit_transaction()
END IF
	
/*	set ib_new_authorization to FALSE so active flag filter not in effect unless user clicks add button */
ib_new_authorization = FALSE
inv_authorizations.nf_set_new_authorization(ib_new_authorization)

// grab the authorization number to re-retrieve
ll_authorization_no = dw_authorizations.getitemnumber( dw_authorizations.getrow(), 'authorization_no')

dw_task_authorization_list.reset()
dw_task_authorization_list.Retrieve(il_claim_no, il_task_no)
SQLCA.nf_handle_error('w_rehab_sheet','tabpage_authorizations','cb_authorization_save - dw_task_authorization_list.Retrieve(il_claim_no,il_task_no)') 
	
// set to the authorization_no we just added or worked on 
IF ISNULL(ll_authorization_no) OR ll_authorization_no = 0 THEN 
	
	SELECT max(authorization_no) 
	INTO		:ll_authorization_no
	FROM 	REHAB_TASK_AUTHORIZATION
	WHERE task_no 	= :il_task_no
	AND		claim_no 	= :il_claim_no
	USING SQLCA;
	SQLCA.nf_handle_error('w_rehab_sheet', 'tabpage_authorizations - cb_authorization_save', 'SELECT max(authorization_no)') 
		
END IF 

// DO THE FIND
ls_find 	= 'authorization_no = ' + string(ll_authorization_no)
li_find 	= dw_task_authorization_list.Find(ls_find, 1, dw_task_authorization_list.RowCount())

IF li_find > 0 THEN
	dw_task_authorization_list.ScrollToRow(li_find)
END IF 
	
cb_authorization_save.enabled 				= FALSE
cb_authorization_cancel.enabled 			= FALSE
dw_authorization_task_list.enabled 		= TRUE
dw_task_authorization_list.enabled 		= TRUE
cb_add_authorization.enabled 				= TRUE
wf_save_indicator()

//revision comment goes away
inv_authorizations.nf_set_column_action(FALSE)
		
/* refresh report on viewer authorizations tabpage*/
tab_rehab_plan_viewer.tabpage_authorizations_viewer.dw_rehab_viewer_authorizations.Retrieve(il_claim_no)
SQLCA.nf_handle_error('w_rehab_sheet','tabpage_authorizations- cb_authorization_save','dw_rehab_viewer_authorizations.Retrieve(il_claim_no)') 

		
end event

type cb_authorization_cancel from commandbutton within tabpage_authorizations
integer x = 2313
integer y = 1592
integer width = 411
integer height = 88
integer taborder = 5
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Cance&l"
end type

event clicked;LONG			ll_authorization_no
INTEGER		li_dw_task_authorization_list_row

SetPointer(HourGlass!)

cb_authorization_save.enabled = FALSE //Rob Head 98/08/21
wf_save_indicator() //Rob Head 98/08/21
	
/*	retrieve the last task worked on */
IF dw_authorization_task_list.RowCount() = 0 THEN
	cb_add_authorization.enabled = TRUE
	dw_authorizations.Reset()
	
	//revision comment goes away
	inv_authorizations.nf_set_column_action(FALSE)
	RETURN
END IF

/*	set ib_new_authorization back to FALSE so active flag filter not in effect unless user clicks add button again */
ib_new_authorization = FALSE
inv_authorizations.nf_set_new_authorization(ib_new_authorization)

//grab the authorization so that we can scroll to it after the re-retrieve
li_dw_task_authorization_list_row = dw_task_authorization_list.getrow()
IF li_dw_task_authorization_list_row > 0 THEN 
	ll_authorization_no = dw_task_authorization_list.getitemnumber(li_dw_task_authorization_list_row, 'authorization_no')
ELSE
	ll_authorization_no = 0
END IF 

dw_authorization_task_list.enabled = TRUE
IF wf_retrieve_author_task_list(ll_authorization_no) > 0 THEN
	dw_authorization_task_list.uf_processselect(1,'KeyBoard')
END IF

cb_add_authorization.enabled 			= TRUE
dw_task_authorization_list.enabled 	= TRUE
cb_authorization_cancel.enabled 		= FALSE

//revision comment goes away
inv_authorizations.nf_set_column_action(FALSE)

RETURN

end event

type dw_auth_let_provider_info from u_dw_online within tabpage_authorizations
boolean visible = false
integer x = 2053
integer y = 1592
integer width = 329
integer height = 76
integer taborder = 3
string dataobject = "d_auth_let_provider_info"
end type

type dw_auth_let_toll_free_no from u_dw_online within tabpage_authorizations
boolean visible = false
integer x = 942
integer y = 1592
integer width = 439
integer height = 76
integer taborder = 121
boolean bringtotop = true
string dataobject = "d_auth_let_toll_free_no"
end type

type tabpage_progress from userobject within tab_rehab_plan
event create ( )
event destroy ( )
integer x = 18
integer y = 168
integer width = 3118
integer height = 1676
long backcolor = 67108864
string text = "Progress"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_progress_note dw_progress_note
dw_rehab_task_attachment dw_rehab_task_attachment
dw_task_progress dw_task_progress
dw_progress_note_event dw_progress_note_event
cb_add_progress cb_add_progress
dw_progress_task_list dw_progress_task_list
cb_save_progress cb_save_progress
cb_cancel_progress cb_cancel_progress
cb_attach_doc cb_attach_doc
cb_send_msg cb_send_msg
cb_link_event cb_link_event
dw_progress_notes_for_task_list dw_progress_notes_for_task_list
end type

on tabpage_progress.create
this.dw_progress_note=create dw_progress_note
this.dw_rehab_task_attachment=create dw_rehab_task_attachment
this.dw_task_progress=create dw_task_progress
this.dw_progress_note_event=create dw_progress_note_event
this.cb_add_progress=create cb_add_progress
this.dw_progress_task_list=create dw_progress_task_list
this.cb_save_progress=create cb_save_progress
this.cb_cancel_progress=create cb_cancel_progress
this.cb_attach_doc=create cb_attach_doc
this.cb_send_msg=create cb_send_msg
this.cb_link_event=create cb_link_event
this.dw_progress_notes_for_task_list=create dw_progress_notes_for_task_list
this.Control[]={this.dw_progress_note,&
this.dw_rehab_task_attachment,&
this.dw_task_progress,&
this.dw_progress_note_event,&
this.cb_add_progress,&
this.dw_progress_task_list,&
this.cb_save_progress,&
this.cb_cancel_progress,&
this.cb_attach_doc,&
this.cb_send_msg,&
this.cb_link_event,&
this.dw_progress_notes_for_task_list}
end on

on tabpage_progress.destroy
destroy(this.dw_progress_note)
destroy(this.dw_rehab_task_attachment)
destroy(this.dw_task_progress)
destroy(this.dw_progress_note_event)
destroy(this.cb_add_progress)
destroy(this.dw_progress_task_list)
destroy(this.cb_save_progress)
destroy(this.cb_cancel_progress)
destroy(this.cb_attach_doc)
destroy(this.cb_send_msg)
destroy(this.cb_link_event)
destroy(this.dw_progress_notes_for_task_list)
end on

type dw_progress_note from u_dw_online within tabpage_progress
boolean visible = false
integer x = 1445
integer y = 496
integer height = 360
integer taborder = 10
string dataobject = "d_progress_note"
end type

event constructor;/* Overridden */
end event

type dw_rehab_task_attachment from u_dw_online within tabpage_progress
boolean visible = false
integer x = 110
integer y = 496
integer height = 360
integer taborder = 60
string dataobject = "d_rehab_task_attachment"
end type

event constructor;/* Overridden */
end event

type dw_task_progress from u_dw_online within tabpage_progress
integer x = 9
integer y = 672
integer width = 2629
integer height = 288
integer taborder = 20
string dataobject = "d_task_progress"
boolean border = false
end type

event itemchanged;call super::itemchanged;	LONG	ll_return

// dw_task_progress is loaded into the data window array position 1.//
	ll_return = inv_rehab_progress.nf_change_item (1,row, dwo.name,data)
	If ll_return = 1 Then // Added by Rob Head 98/09/16
		Return 1
	End If
//	Enable the Save and Cancel buttons as data has changed.//
	cb_save_progress.enabled = TRUE
	cb_cancel_progress.enabled = TRUE
	cb_add_progress.enabled = FALSE
	cb_send_msg.enabled = FALSE
	cb_link_event.enabled = FALSE
	cb_attach_doc.enabled = FALSE
	tab_rehab_plan_viewer.tabpage_tasks_and_progress.cb_refresh_viewer.Enabled = FALSE

// Disable the Attach Document button until the user either Saves or Cancels 
//	the Progress changes.
	cb_attach_doc.enabled = FALSE
	
	wf_save_indicator()

		
end event

event constructor;/* Overridden */
end event

type dw_progress_note_event from u_dw_online within tabpage_progress
integer y = 944
integer width = 2638
integer height = 592
integer taborder = 30
string dataobject = "d_progress_note_event"
boolean border = false
end type

event itemchanged;call super::itemchanged;
Long		ll_return
String	ls_text, ls_event_specific_code

/* dw_progress_note is loaded into the data window array position 2 
*/

	/* If/Else and validation added by Rob Head 98/09/15 as per PR137 */
	If dwo.Name = "event_date" Then
		IF Date(Left(data,10)) > Date(f_server_datetime()) THEN
			MessageBox("Event Log - Validation Error","The progress date cannot be greater than the current date",Exclamation!)
			Return 1
		END IF
	Else
		ll_return = inv_rehab_progress.nf_change_item(2,row, dwo.name,data)

		cb_save_progress.enabled = TRUE
		cb_cancel_progress.enabled = TRUE
	
		wf_save_indicator()
	End If

end event

event constructor;/* Overridden */
end event

event ue_print;DATASTORE lds_print_event
LONG  ll_claim_no, ll_event_no

lds_print_event = CREATE DATASTORE
lds_print_event.DataObject = 'd_progress_note_event_print'
lds_print_event.SetTransObject(SQLCA)

THIS.ShareData(lds_print_event)

IF lds_print_event.RowCount() > 0 THEN
	lds_print_event.Print()
END IF

end event

type cb_add_progress from commandbutton within tabpage_progress
event clicked pbm_bnclicked
integer x = 9
integer y = 1572
integer width = 407
integer height = 88
integer taborder = 70
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Add Progress"
end type

event clicked;STRING		ls_task_type_code, ls_task_status_code
LONG		ll_row

IF dw_progress_task_list.RowCount() < 1 THEN
	MessageBox('Error','No tasks to add a progress note for.')
	RETURN 
END IF

ll_row = dw_progress_task_list.GetRow()
IF ll_row > 0 THEN
	
	IF inv_rehab_progress.nf_add_event() = 0 THEN
		dw_progress_task_list.enabled 													= FALSE
		dw_progress_notes_for_task_list.enabled 									= FALSE
		tab_rehab_plan.tabpage_progress.cb_save_progress.enabled 		= TRUE
		tab_rehab_plan.tabpage_progress.cb_cancel_progress.enabled 	= TRUE
		tab_rehab_plan.tabpage_progress.cb_add_progress.enabled 		= FALSE
		tab_rehab_plan.tabpage_progress.cb_attach_doc.enabled 			= FALSE 
		tab_rehab_plan.tabpage_progress.cb_send_msg.enabled 			= FALSE
		tab_rehab_plan.tabpage_progress.cb_link_event.enabled 			= FALSE
		tab_rehab_plan_viewer.tabpage_tasks_and_progress.cb_refresh_viewer.enabled = FALSE
		ib_note = TRUE
	END IF
ELSE
	MessageBox('Error','No task selected.  Please select a task first.')
	RETURN
END IF

end event

type dw_progress_task_list from u_dw_online within tabpage_progress
event rowfocuschanged pbm_dwnrowchange
integer y = 12
integer width = 2629
integer height = 340
integer taborder = 40
string dataobject = "d_task_tab_task_list"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;call super::rowfocuschanged;	LONG		ll_current_tab

	ll_current_tab = tab_rehab_plan.SelectedTab
	IF ll_current_tab <> 4 Then 
		ib_delayed_progress_tab_retrieve = True
		return
	Else
		wf_retrieve_progress_details()
		ib_delayed_progress_tab_retrieve = False
	End if

end event

event constructor;/* Overridden */
end event

type cb_save_progress from commandbutton within tabpage_progress
event clicked pbm_bnclicked
integer x = 1307
integer y = 1572
integer width = 411
integer height = 88
integer taborder = 90
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
end type

event clicked;	INTEGER								li_rtn, li_trancount
	LONG									ll_row
	BOOLEAN								lb_old_do_tab_selection, 	lb_task_updated
	N_PROCESS_RUN_STATUS 	ln_process_run_status
	
	/******************************************************************************************
	P10275 - Daytime Payment Processing
	- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
	- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
	- '025' refers to the Rehab Plan Maintenance module, '044' refers to the Payment Processing module
	******************************************************************************************/
	ln_process_run_status = Create N_PROCESS_RUN_STATUS
	
	li_rtn = ln_process_run_status.nf_in_progress('025','044','save',SQLCA)
	
	IF li_rtn = 1 THEN
		// module is blocked
		return
	END IF
	/******************************************************************************************/
		
	dw_progress_note_event.Accepttext()
	dw_task_progress.Accepttext()
	
	SetPointer(HourGlass!)
	
	w_rehab_sheet.SetRedraw(False)
	ll_row = dw_progress_task_list.GetRow()
	IF ll_row > 0 THEN
		il_task_no = dw_progress_task_list.GetItemNumber(ll_row,'task_no')
		IF il_task_no = 0 THEN
			MessageBox("Task Number Error","Task number is 0. Please cancel and try again.")
			RETURN
		ELSE
			/*	Set the task no for the Progress tab user object.*/
			inv_rehab_progress.nf_set_task_no(il_task_no)
		END IF
	END IF

	IF dw_task_progress.GetItemStatus(1,0,Primary!) = DataModified! THen
		lb_task_updated = True
	End if
	IF ib_note THEN
		f_adjust_text('tabpage_progress')
	END IF
	
	SQLCA.nf_begin_transaction()
	
	IF inv_rehab_progress.nf_save() >= 0 THEN
		
		SQLCA.nf_commit_transaction()
		
		/*	Protect the newly added progress note from being updated.*/
		inv_rehab_progress.nf_protect_add_event(true)

		/* Retrieve the Progress details and clear the progress note event.*/
		//inv_rehab_progress.nf_retrieve_progress()	
		
		cb_save_progress.enabled = FALSE
		
		//wf_retrieve_progress_list()
		dw_progress_notes_for_task_list.Reset()
		dw_progress_notes_for_task_list.Retrieve(il_claim_no,il_task_no)
		SQLCA.nf_handle_error("w_rehab_sheet","cb_save_progress","dw_progress_notes_for_task_list.Retrieve(il_claim_no,il_task_no)") 
		
		If lb_task_updated Then
			dw_task_progress.Retrieve(il_claim_no,il_task_no)
			SQLCA.nf_handle_error("w_rehab_sheet","cb_save_progress","dw_task_progress.Retrieve(il_claim_no,il_task_no)") 
		End if

		/*	Reset the buttons.*/
		dw_progress_task_list.enabled = true
		dw_progress_notes_for_task_list.enabled = true
		
		cb_cancel_progress.enabled = FALSE
		tab_rehab_plan.tabpage_progress.cb_add_progress.enabled = true
		IF ib_document_found THEN
			cb_attach_doc.enabled = true
		ELSE
			cb_attach_doc.enabled = false
		END IF
		cb_send_msg.enabled = true
		cb_link_event.enabled = true
		tab_rehab_plan_viewer.tabpage_tasks_and_progress.cb_refresh_viewer.Enabled = TRUE
		
		lb_old_do_tab_selection = ib_do_tab_selection
		ib_do_tab_selection = False
		
		tab_rehab_plan_viewer.tabpage_tasks_and_progress.cb_refresh_viewer.TriggerEvent(Clicked!)
		ib_do_tab_selection = lb_old_do_tab_selection
		wf_save_indicator()
	ELSE
		SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
		IF li_trancount > 0 THEN
			SQLCA.nf_rollback_transaction()
		END IF
	END IF
	
		w_rehab_sheet.SetRedraw(True)
		
end event

type cb_cancel_progress from commandbutton within tabpage_progress
event clicked pbm_bnclicked
integer x = 1746
integer y = 1572
integer width = 411
integer height = 88
integer taborder = 110
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Cance&l"
end type

event clicked;LONG	ll_row, ll_event_no

/* Cancel the Progress Updates by retrieving the Progress details and
	their progress notes, if any. Reset the buttons.
*/
IF inv_rehab_progress.nf_retrieve_progress() = 0 THEN
	dw_progress_task_list.enabled 					= TRUE
	dw_progress_notes_for_task_list.enabled 	= TRUE
	cb_save_progress.enabled 							= FALSE
	cb_cancel_progress.enabled 						= FALSE
	cb_add_progress.enabled 							= TRUE
	cb_send_msg.enabled 								= TRUE
	cb_link_event.enabled 								= TRUE
	IF ib_document_found THEN
		cb_attach_doc.enabled = TRUE
	ELSE
		cb_attach_doc.enabled = FALSE
	END IF
	tab_rehab_plan_viewer.tabpage_tasks_and_progress.cb_refresh_viewer.Enabled = TRUE
END IF

wf_save_indicator()

/* Protect the retrieved progress note after a cancel.*/
inv_rehab_progress.nf_protect_add_event(TRUE)

IF dw_progress_notes_for_task_list.RowCount() = 0 THEN
	dw_progress_note_event.Reset()
	RETURN
END IF
	
ll_row = dw_progress_notes_for_task_list.GetRow()
IF ll_row > 0 THEN
		
	/*	Retrieve the details of the progress note.*/
	ll_event_no = dw_progress_notes_for_task_list.GetItemNumber(ll_row,'event_no')
	dw_progress_note_event.Retrieve(il_claim_no,ll_event_no,il_task_no)
	SQLCA.nf_handle_error('w_rehab_sheet','cb_cancel_progress - clicked','dw_progress_note_event.Retrieve(il_claim_no,ll_event_no,il_task_no)') 
END IF
	
ib_note = FALSE
end event

type cb_attach_doc from commandbutton within tabpage_progress
event clicked pbm_bnclicked
integer x = 425
integer y = 1572
integer width = 325
integer height = 88
integer taborder = 80
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Attach &Doc"
end type

event clicked;LONG		ll_row,	ll_row2, ll_docid,  ll_result
INTEGER  li_event_no
STRING	ls_document_type
LONG		ll_current_task_no

/* Verify the document selected is the document to be attached to the Rehab Plan. 
*/
	ll_row = dw_documents.GetRow()
	ll_row2 = dw_progress_notes_for_task_list.GetRow()
	IF ll_row > 0 AND ll_row2 > 0 THEN
		ll_docid = dw_documents.GetItemNumber(ll_row,'ref_docid')
		ls_document_type = dw_documents.GetItemString(ll_row,'document_type_document_type_desc')
		li_event_no = dw_progress_notes_for_task_list.GetItemNumber(ll_row2, 'event_no')
		ll_result = inv_rehab_progress.nf_check_selected_doc(ll_docid,ls_document_type,li_event_no)
		IF ll_result = 0 THEN
			ll_current_task_no = inv_tab_controller.nf_current_task_no()
			
			//This will not allow an automatic tab selection based on the record type in the viewer
			ib_do_tab_selection = False
			inv_tab_controller.ib_do_row_selection = False
			tab_rehab_plan_viewer.tabpage_tasks_and_progress.cb_refresh_viewer.TriggerEvent(Clicked!)
			inv_tab_controller.ib_do_row_selection = True
			//Set the task number back to what it was before we refreshed the viewer.
			inv_tab_controller.nf_current_task_no(ll_current_task_no)
			ib_do_tab_selection = True
		END IF
	END IF

end event

type cb_send_msg from commandbutton within tabpage_progress
event clicked pbm_bnclicked
integer x = 2313
integer y = 1572
integer width = 411
integer height = 88
integer taborder = 120
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Send &Msg"
end type

event clicked;S_SEND_DOC_PARAMETERS	ls_send_doc_parameters

ls_send_doc_parameters.msg_mode = TRUE
ls_send_doc_parameters.claim_no =  il_claim_no
IF dw_progress_notes_for_task_list.RowCount() <= 0 THEN
	MessageBox('Warning','There are no progress notes selected to send.')
	Return
ELSE
	ls_send_doc_parameters.document_list = dw_progress_notes_for_task_list
END IF
OpenWithParm(w_send_folder_rehab,ls_send_doc_parameters)
end event

type cb_link_event from commandbutton within tabpage_progress
event clicked pbm_bnclicked
integer x = 759
integer y = 1572
integer width = 315
integer height = 88
integer taborder = 100
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Link Event"
end type

event clicked;STRING				ls_did_a_link
LONG					ll_current_task_no
S_WINDOW_MESSAGE	lstr_message

	lstr_message.al_doubleparm[1] = il_claim_no
	lstr_message.al_doubleparm[2] = il_task_no
	OpenWithParm(w_claim_events_to_be_linked,lstr_message)
	ls_did_a_link = Message.StringParm

	IF ls_did_a_link = 'YES' THEN
		dw_progress_notes_for_task_list.Reset()
		dw_progress_notes_for_task_list.Retrieve(il_claim_no,il_task_no)
		
		
		ll_current_task_no = inv_tab_controller.nf_current_task_no()
			
		//This will not allow an automatic tab selection based on the record type in the viewer
		ib_do_tab_selection = False
		inv_tab_controller.ib_do_row_selection = False
		tab_rehab_plan_viewer.tabpage_tasks_and_progress.cb_refresh_viewer.TriggerEvent(Clicked!)
		inv_tab_controller.ib_do_row_selection = True
		//Set the task number back to what it was before we refreshed the viewer.
		inv_tab_controller.nf_current_task_no(ll_current_task_no)
		ib_do_tab_selection = True
	END IF
	
end event

type dw_progress_notes_for_task_list from u_dw_online within tabpage_progress
integer y = 368
integer width = 2629
integer height = 300
integer taborder = 50
string dataobject = "d_progress_notes_for_task_list"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;call super::rowfocuschanged;LONG	ll_row, ll_event_no

IF THIS.RowCount() = 0 THEN	RETURN
	
ll_row = THIS.GetRow()	// the currentrow argument was not used because it does not always have a value

IF ll_row < 1 THEN RETURN
		
/*	Do not allow the user to scroll to another task before saving or cancelling changes. */
IF cb_save_progress.enabled = TRUE THEN
	THIS.enabled = FALSE
	MessageBox ("DATA HAS CHANGED", "There were progress details entered, please SAVE or CANCEL the changes", STOPSIGN!)
	RETURN
END IF
		
THIS.SelectRow(0,FALSE)
THIS.SelectRow(ll_row,TRUE)
		
/*	Retrieve the details of the progress note.*/
ll_event_no = THIS.GetItemNumber(ll_row,'event_no')
	
dw_progress_note_event.Retrieve(il_claim_no,ll_event_no,il_task_no)
SQLCA.nf_handle_error('dw_progress_note_event.Retrieve(il_claim_no,ll_event_no,il_task_no)','w_rehab_sheet','rowfocuschanged for dw_progress_notes_for_task_list') 
	


end event

event constructor;/* Overridden */
end event

type tabpage_voc_profile from userobject within tab_rehab_plan
integer x = 18
integer y = 168
integer width = 3118
integer height = 1676
long backcolor = 67108864
string text = "Vocational~r~nProfile"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_voc_profile dw_voc_profile
cb_save_voc_profile cb_save_voc_profile
cb_cancel_voc_profile cb_cancel_voc_profile
dw_percent_ppi dw_percent_ppi
end type

on tabpage_voc_profile.create
this.dw_voc_profile=create dw_voc_profile
this.cb_save_voc_profile=create cb_save_voc_profile
this.cb_cancel_voc_profile=create cb_cancel_voc_profile
this.dw_percent_ppi=create dw_percent_ppi
this.Control[]={this.dw_voc_profile,&
this.cb_save_voc_profile,&
this.cb_cancel_voc_profile,&
this.dw_percent_ppi}
end on

on tabpage_voc_profile.destroy
destroy(this.dw_voc_profile)
destroy(this.cb_save_voc_profile)
destroy(this.cb_cancel_voc_profile)
destroy(this.dw_percent_ppi)
end on

type dw_voc_profile from u_dw_online within tabpage_voc_profile
integer y = 40
integer width = 2638
integer height = 668
integer taborder = 2
string dataobject = "d_voc_profile"
boolean border = false
end type

event itemchanged;call super::itemchanged;

/* Enable the Save and Cancel buttons as data has changed
*/
	cb_save_voc_profile.enabled = TRUE
	cb_cancel_voc_profile.enabled = TRUE
	wf_save_indicator()

end event

event constructor;/* Overridden */
end event

type cb_save_voc_profile from commandbutton within tabpage_voc_profile
event clicked pbm_bnclicked
integer x = 1870
integer y = 1572
integer width = 411
integer height = 88
integer taborder = 3
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
end type

event clicked;INTEGER li_trancount


	SetPointer(HourGlass!)
	
	SQLCA.nf_begin_transaction()
	
	IF inv_voc_profile.nf_save() >= 0 THEN
		SQLCA.nf_commit_transaction()
		
		cb_save_voc_profile.enabled = FALSE
		cb_cancel_voc_profile.enabled = FALSE
		/* Retrieve the Vocational Profile Details
		*/
		inv_voc_profile.nf_retrieve_voc_profile()
		wf_save_indicator()
	ELSE
		SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
		IF li_trancount > 0 THEN
			SQLCA.nf_rollback_transaction()
		END IF
	END IF
	
end event

type cb_cancel_voc_profile from commandbutton within tabpage_voc_profile
event clicked pbm_bnclicked
integer x = 2309
integer y = 1572
integer width = 411
integer height = 88
integer taborder = 4
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Cance&l"
end type

event clicked;
/* Cancel the Voc Profile Updates by re-retrieving the Vocational
	Profile details and reset the buttons 
*/
	IF inv_voc_profile.nf_retrieve_voc_profile() = 0 THEN
		cb_save_voc_profile.enabled = false
		cb_cancel_voc_profile.enabled = false
	END IF

	IF dw_voc_profile.RowCount() = 0 THEN
		inv_voc_profile.nf_insert(1)
		inv_voc_profile.nf_set_defaults()
	END IF
	wf_save_indicator()

end event

type dw_percent_ppi from u_dw_online within tabpage_voc_profile
integer x = 55
integer y = 716
integer width = 434
integer height = 96
integer taborder = 4
boolean bringtotop = true
boolean enabled = false
string dataobject = "d_percent_ppi"
boolean border = false
end type

type tabpage_case_monitoring from userobject within tab_rehab_plan
integer x = 18
integer y = 168
integer width = 3118
integer height = 1676
long backcolor = 67108864
string text = "Case~r~nMonitoring"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_case_monitoring dw_case_monitoring
dw_complicating_factors_list dw_complicating_factors_list
st_complication_label st_complication_label
dw_complicating_factors dw_complicating_factors
cb_add_complicating_factor cb_add_complicating_factor
cb_save_case_monitor cb_save_case_monitor
cb_cancel_case_monitor cb_cancel_case_monitor
dw_working_status_change_list dw_working_status_change_list
dw_medical_func_status_change_list dw_medical_func_status_change_list
st_working_status_change st_working_status_change
st_medical_func_stauts_change st_medical_func_stauts_change
gb_1 gb_1
end type

on tabpage_case_monitoring.create
this.dw_case_monitoring=create dw_case_monitoring
this.dw_complicating_factors_list=create dw_complicating_factors_list
this.st_complication_label=create st_complication_label
this.dw_complicating_factors=create dw_complicating_factors
this.cb_add_complicating_factor=create cb_add_complicating_factor
this.cb_save_case_monitor=create cb_save_case_monitor
this.cb_cancel_case_monitor=create cb_cancel_case_monitor
this.dw_working_status_change_list=create dw_working_status_change_list
this.dw_medical_func_status_change_list=create dw_medical_func_status_change_list
this.st_working_status_change=create st_working_status_change
this.st_medical_func_stauts_change=create st_medical_func_stauts_change
this.gb_1=create gb_1
this.Control[]={this.dw_case_monitoring,&
this.dw_complicating_factors_list,&
this.st_complication_label,&
this.dw_complicating_factors,&
this.cb_add_complicating_factor,&
this.cb_save_case_monitor,&
this.cb_cancel_case_monitor,&
this.dw_working_status_change_list,&
this.dw_medical_func_status_change_list,&
this.st_working_status_change,&
this.st_medical_func_stauts_change,&
this.gb_1}
end on

on tabpage_case_monitoring.destroy
destroy(this.dw_case_monitoring)
destroy(this.dw_complicating_factors_list)
destroy(this.st_complication_label)
destroy(this.dw_complicating_factors)
destroy(this.cb_add_complicating_factor)
destroy(this.cb_save_case_monitor)
destroy(this.cb_cancel_case_monitor)
destroy(this.dw_working_status_change_list)
destroy(this.dw_medical_func_status_change_list)
destroy(this.st_working_status_change)
destroy(this.st_medical_func_stauts_change)
destroy(this.gb_1)
end on

type dw_case_monitoring from u_dw_online within tabpage_case_monitoring
integer x = 9
integer y = 4
integer width = 2624
integer height = 644
integer taborder = 3
string dataobject = "d_case_monitoring"
boolean border = false
end type

event itemchanged;call super::itemchanged;LONG	ll_return_value

	uf_set_pbmessage(TRUE)
	
	ll_return_value = inv_case_monitor.nf_change_item(1,row,dwo.Name,data)
		
/* Enable the Save and Cancel buttons as data has changed */
	cb_save_case_monitor.enabled = TRUE
	cb_cancel_case_monitor.enabled = TRUE
	wf_save_indicator()
	
Return ll_return_value

end event

event constructor;/* Overridden */
end event

type dw_complicating_factors_list from u_dw_online within tabpage_case_monitoring
integer x = 14
integer y = 1248
integer width = 2496
integer height = 200
integer taborder = 2
boolean bringtotop = true
string dataobject = "d_complicating_factors_list"
boolean hscrollbar = true
boolean vscrollbar = true
boolean border = false
end type

event constructor;/* Overridden */
end event

type st_complication_label from statictext within tabpage_case_monitoring
integer x = 137
integer y = 1476
integer width = 494
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
boolean enabled = false
string text = "New Complication:"
boolean focusrectangle = false
end type

type dw_complicating_factors from u_dw_online within tabpage_case_monitoring
integer x = 613
integer y = 1464
integer width = 983
integer height = 104
integer taborder = 2
string dataobject = "d_complicating_factors"
boolean border = false
end type

event constructor;/* Overridden */
end event

type cb_add_complicating_factor from commandbutton within tabpage_case_monitoring
event clicked pbm_bnclicked
integer x = 18
integer y = 1572
integer width = 731
integer height = 88
integer taborder = 3
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Add Complicating Factor"
end type

event clicked;
st_complication_label.visible = true
IF inv_case_monitor.nf_add_complication() = 0 THEN
	cb_save_case_monitor.enabled = true
	cb_cancel_case_monitor.enabled = true
	cb_add_complicating_factor.enabled = false
	wf_save_indicator()

END IF


end event

type cb_save_case_monitor from commandbutton within tabpage_case_monitoring
event clicked pbm_bnclicked
integer x = 1874
integer y = 1572
integer width = 411
integer height = 88
integer taborder = 4
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
end type

event clicked;INTEGER		li_rtn, li_trancount
N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '025' refers to the Rehab Plan Maintenance module, '044' refers to the Payment Processing module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('025','044','save',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF
/******************************************************************************************/

SetPointer(HourGlass!)

SQLCA.nf_begin_transaction()

IF inv_case_monitor.nf_save() >= 0 THEN
	SQLCA.nf_commit_transaction()
	
	/* Re-Retrieve the Case Monitoring Details */
	inv_case_monitor.nf_retrieve_case_monitor() 
	
	/* Re-Retrieve the Complicating Factors information for the claim to ensure the most up-to-date listing	*/
	dw_complicating_factors_list.Retrieve(il_claim_no)
	SQLCA.nf_handle_error("w_rehab_sheet","cb_save_case_monitor - clicked","dw_complicating_factors_list.Retrieve(il_claim_no)") 
		
	/* Re-Retrieve the Working Status Change History List	*/
	dw_working_status_change_list.Retrieve(il_claim_no)
	SQLCA.nf_handle_error('w_rehab_sheet','cb_save_case_monitor - clicked', 'dw_working_status_change_list.Retrieve(il_claim_no)') 

	/* Re-Retrieve the Medical Functional Status Change History List	*/
	dw_medical_func_status_change_list.Retrieve(il_claim_no)
	SQLCA.nf_handle_error('w_rehab_sheet','cb_save_case_monitor - clicked', 'dw_medical_func_status_change_list.Retrieve(il_claim_no)') 

	/* Disable dw_case_monitoring dates to prevent user modification unless a new/revised status is entered.	*/
	dw_case_monitoring.Object.work_status_date.Protect 									= TRUE
	dw_case_monitoring.Object.work_status_date.Background.Color 					= 67108864
	dw_case_monitoring.Object.medical_functional_status_date.Protect 				= TRUE
	dw_case_monitoring.Object.medical_functional_status_date.Background.Color 	= 67108864

	/* Reset the Complicating Factor datawindow so that it no longer is visible and	reset the buttons	*/
	inv_case_monitor.nf_reset_complication()
	st_complication_label.visible 				= FALSE
	cb_save_case_monitor.enabled 			= FALSE
	cb_cancel_case_monitor.enabled 		= FALSE
	cb_add_complicating_factor.enabled 	= TRUE
	wf_save_indicator()
ELSE
	SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
	IF li_trancount > 0 THEN
		SQLCA.nf_rollback_transaction()
	END IF
END IF
		
	
end event

type cb_cancel_case_monitor from commandbutton within tabpage_case_monitoring
event clicked pbm_bnclicked
integer x = 2309
integer y = 1572
integer width = 411
integer height = 88
integer taborder = 5
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Cance&l"
end type

event clicked;/* Cancel the Case Monitoring Updates (by re-retrieving the Case Monitoring
	details) and reset the buttons 
*/
IF inv_case_monitor.nf_retrieve_case_monitor() = 0 THEN
	cb_save_case_monitor.enabled 			= FALSE
	cb_cancel_case_monitor.enabled 		= FALSE
	cb_add_complicating_factor.enabled 	= TRUE

	/* Disable dw_case_monitoring dates to prevent user modification unless a new/revised status is entered.*/
	dw_case_monitoring.Object.work_status_date.Protect 									= TRUE
	dw_case_monitoring.Object.work_status_date.Background.Color 					= 67108864
	dw_case_monitoring.Object.medical_functional_status_date.Protect 				= TRUE
	dw_case_monitoring.Object.medical_functional_status_date.Background.Color 	= 67108864

	/* Reset the Complicating Factor datawindow so that it no longer is visible and	reset the buttons */
	inv_case_monitor.nf_reset_complication()
	st_complication_label.visible = FALSE
END IF

IF dw_case_monitoring.RowCount() = 0 THEN
	/* If there is no Case Monitoring for the claim, insert a row for the user to enter data */
	inv_case_monitor.nf_add_case_monitoring()
	inv_case_monitor.nf_set_defaults()
		
	/* Retrieve the Complicating Factors information for the claim if it exists */
	tab_rehab_plan.tabpage_case_monitoring.dw_complicating_factors_list.Retrieve(il_claim_no)
	SQLCA.nf_handle_error("w_rehab_sheet","cb_cancel_case_monitor",".dw_complicating_factors_list.Retrieve(il_claim_no)") 
		
	inv_case_monitor.nf_set_complication_list(tab_rehab_plan.tabpage_case_monitoring.dw_complicating_factors_list)
	inv_case_monitor.nf_set_complication_trans()
	tab_rehab_plan.tabpage_case_monitoring.st_complication_label.visible = FALSE
END IF
	
wf_save_indicator()

end event

type dw_working_status_change_list from u_dw_online within tabpage_case_monitoring
integer x = 9
integer y = 700
integer width = 2615
integer height = 236
integer taborder = 2
string dataobject = "d_working_status_change_list"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event constructor;/* Overridden */
end event

type dw_medical_func_status_change_list from u_dw_online within tabpage_case_monitoring
integer x = 9
integer y = 996
integer width = 2615
integer height = 236
integer taborder = 2
boolean bringtotop = true
string dataobject = "d_medical_func_status_change_list"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event constructor;/* Overridden */
end event

type st_working_status_change from statictext within tabpage_case_monitoring
integer x = 18
integer y = 640
integer width = 645
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 79741120
boolean enabled = false
string text = " Working Status History"
boolean focusrectangle = false
end type

type st_medical_func_stauts_change from statictext within tabpage_case_monitoring
integer x = 14
integer y = 936
integer width = 901
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = " Medical Functional Status History"
boolean focusrectangle = false
end type

type gb_1 from groupbox within tabpage_case_monitoring
integer x = 5
integer y = 1208
integer width = 2629
integer height = 252
integer taborder = 11
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 67108864
long backcolor = 67108864
end type

type tabpage_action_item from userobject within tab_rehab_plan
integer x = 18
integer y = 168
integer width = 3118
integer height = 1676
long backcolor = 67108864
string text = "Action Item"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
gb_2 gb_2
cb_add_progress_note cb_add_progress_note
cb_cancel cb_cancel
cb_save cb_save
dw_action_list dw_action_list
dw_progress_notes_list dw_progress_notes_list
pb_progress_list_maximize pb_progress_list_maximize
pb_max pb_max
dw_action_item dw_action_item
cb_add_action_item cb_add_action_item
gb_3 gb_3
dw_action_item_progress_note dw_action_item_progress_note
end type

on tabpage_action_item.create
this.gb_2=create gb_2
this.cb_add_progress_note=create cb_add_progress_note
this.cb_cancel=create cb_cancel
this.cb_save=create cb_save
this.dw_action_list=create dw_action_list
this.dw_progress_notes_list=create dw_progress_notes_list
this.pb_progress_list_maximize=create pb_progress_list_maximize
this.pb_max=create pb_max
this.dw_action_item=create dw_action_item
this.cb_add_action_item=create cb_add_action_item
this.gb_3=create gb_3
this.dw_action_item_progress_note=create dw_action_item_progress_note
this.Control[]={this.gb_2,&
this.cb_add_progress_note,&
this.cb_cancel,&
this.cb_save,&
this.dw_action_list,&
this.dw_progress_notes_list,&
this.pb_progress_list_maximize,&
this.pb_max,&
this.dw_action_item,&
this.cb_add_action_item,&
this.gb_3,&
this.dw_action_item_progress_note}
end on

on tabpage_action_item.destroy
destroy(this.gb_2)
destroy(this.cb_add_progress_note)
destroy(this.cb_cancel)
destroy(this.cb_save)
destroy(this.dw_action_list)
destroy(this.dw_progress_notes_list)
destroy(this.pb_progress_list_maximize)
destroy(this.pb_max)
destroy(this.dw_action_item)
destroy(this.cb_add_action_item)
destroy(this.gb_3)
destroy(this.dw_action_item_progress_note)
end on

type gb_2 from groupbox within tabpage_action_item
integer y = 352
integer width = 2629
integer height = 544
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Action item"
end type

type cb_add_progress_note from commandbutton within tabpage_action_item
integer x = 14
integer y = 1572
integer width = 402
integer height = 88
integer taborder = 23
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Add &Note"
end type

event clicked;
If tab_rehab_plan.tabpage_action_item.dw_action_item.RowCount() > 0 Then
	
	inv_action_item.nf_load_bus_rule_flags()
	
	IF inv_action_item.ib_progress_note_allowed Then
		tab_rehab_plan.tabpage_action_item.dw_action_item_progress_note.reset()
		inv_action_item.nf_insert_note()
		
		wf_enable_action_item_controls(True)
		ib_note = TRUE
	else
		MessageBox('Disallowed','Unable to add a notes to this action item')
	End if
else
	MessageBox('Not allowed','There is no action item selected to associate with a new note.')
End if
end event

type cb_cancel from commandbutton within tabpage_action_item
integer x = 2322
integer y = 1572
integer width = 402
integer height = 88
integer taborder = 12
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Cance&l"
end type

event clicked;SetRedraw(False)

wf_retrieve_action_item()

wf_retrieve_progress_list()
//Re-enable the action list
wf_enable_action_item_controls(False)

SetRedraw(True)


If ib_added_followup_from_task_tab = True Then
	tab_rehab_plan.selecttab(2)
	ib_added_followup_from_task_tab =False
End if
ib_note = FALSE
end event

type cb_save from commandbutton within tabpage_action_item
integer x = 1897
integer y = 1572
integer width = 402
integer height = 88
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
end type

event clicked;LONG			ll_task_no,	 ll_found_row, ll_list_rows, ll_task_found, ll_related_task_no, ll_current_task_no
INTEGER		li_result, li_rtn, li_trancount
BOOLEAN		lb_current_do_tab_selection
dwitemstatus  						ldws_row_status
N_PROCESS_RUN_STATUS 	ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '025' refers to the Rehab Plan Maintenance module, '044' refers to the Payment Processing module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('025','044','save',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	RETURN
END IF
/******************************************************************************************/

dw_action_item.Accepttext()
dw_action_item_progress_note.Accepttext()

ldws_row_status = tab_rehab_plan.tabpage_action_item.dw_action_item.GetItemStatus(1,0,Primary!)

ll_related_task_no = dw_action_item.GetItemNumber(1,'related_task_no')

IF ib_note THEN
	f_adjust_text('tabpage_action_item')
END IF


SQLCA.nf_begin_transaction()

//Save the item
li_rtn = inv_action_item.nf_save()

IF li_rtn <> 0 THEN
	SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
	IF li_trancount > 0 THEN
		SQLCA.nf_rollback_transaction()
	END IF
	RETURN
END IF

SQLCA.nf_commit_transaction()


w_rehab_sheet.SetRedraw(FALSE)

//Get the task number that was assigned to the item during the save
ll_task_no = dw_action_item.GetItemNumber(1,'task_no')

//Get the current tab selection variable so we can set it back after the save
lb_current_do_tab_selection = ib_do_tab_selection
//We don't want the tab selection on because it will jump to the task tab
//after our save and we want to stay on the action item tab
ib_do_tab_selection = FALSE


tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.Reset()
tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.Retrieve(il_claim_no)


//Set the do_tab_selection variable back so the tabs will get selected properly
ib_do_tab_selection = lb_current_do_tab_selection
	
//Re-retrieve the action item list so the new item will show up
ll_list_rows = wf_retrieve_action_item_list()

//If the action item just save is related to a task then refresh the task list
IF ll_related_task_no > 0 THEN
	ll_current_task_no = inv_tab_controller.nf_current_task_no()
	inv_tab_controller.ib_do_row_selection = FALSE
	wf_retrieve_task_list()
	inv_tab_controller.nf_current_task_no(ll_current_task_no)
	inv_tab_controller.ib_do_row_selection = TRUE
End IF

IF ll_list_rows > 0 THEN
		
	//Search for the new task_no in the list
	ll_found_row = dw_action_list.Find('task_no = ' + String(ll_task_no),1,dw_action_list.RowCount())
	
	//If the task_no is found then scroll to that row
	IF ll_found_row > 0 THEN
		dw_action_list.Scrolltorow(ll_found_row)
	ELSEIF ll_found_row = 0 THEN
		//If the item is no found (because it is filter)
		//then scroll to the first item in the list
		IF dw_action_list.RowCount() > 0 THEN
			IF inv_action_item.il_new_reset_task_no > 0 THEN
				ll_found_row = dw_action_list.Find('task_no = ' + String(inv_action_item.il_new_reset_task_no),1,dw_action_list.RowCount())
				dw_action_list.Scrolltorow(ll_found_row)
			ELSE
				dw_action_list.Scrolltorow(1)
			END IF
		END IF
	ELSE
		SignalError(ll_found_row,'Error in action item list')
	END IF
END IF

//If the follow up was added from the task tab then make sure we scroll back to that
//task numbe on the task tab
IF ib_added_followup_from_task_tab THEN
	ib_added_followup_from_task_tab = FALSE
	tab_rehab_plan.SelectTab(2)
ELSE
	//Only set the il_task_no to the task_no we just added if we didn't start the
	//process from the task tab.  If we did then leave il_tasl_no as the current
	//task_no on the task tab.
	il_task_no = ll_task_no
END IF

//Re-enable the action list
wf_enable_action_item_controls(FALSE)

w_rehab_sheet.SetRedraw(TRUE)
end event

type dw_action_list from u_dw_online within tabpage_action_item
integer y = 12
integer width = 2629
integer height = 336
integer taborder = 11
string dataobject = "d_rehab_task_action_list"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;call super::rowfocuschanged;LONG			ll_task_no
LONG			ll_viewer_row
LONG			ll_found_at
INTEGER		li_current_tab
INTEGER		li_result
BOOLEAN		lb_tab_selection


If currentrow <> 0 THen
	wf_retrieve_action_item()
	SelectRow(0,False)
	SelectRow(currentrow,True)
	
	wf_retrieve_progress_list()
	
	ll_task_no = This.GetItemNumber(currentrow,'task_no')
	//Referesh the viewer
	ll_viewer_row = tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.GetRow()			
	IF ll_viewer_row > 0 THEN
		lb_tab_selection = ib_do_tab_selection
		ib_do_tab_selection = False
		IF ll_task_no <> tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.GetItemNumber(ll_viewer_row,'task_no') THEN
			ll_found_at = tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.Find('task_no = ' +&
			+ String(ll_task_no) ,1,tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.RowCount())
			IF ll_found_at > 0 THEN
				tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.ScrollToRow(ll_found_at)
			END IF
		END IF
		ib_do_tab_selection = lb_tab_selection
	END IF
Else
	tab_rehab_plan.tabpage_action_item.dw_action_item.Reset()
	tab_rehab_plan.tabpage_action_item.dw_progress_notes_list.reset()
	tab_rehab_plan.tabpage_action_item.dw_action_item_progress_note.Reset()
End if
end event

event constructor;call super::constructor;pb_max.uf_set_requestor( this)


end event

event rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup
/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
	STRING		ls_filter	

	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	lm_popup.m_options.m_sort.visible = True
	lm_popup.m_options.m_filterlist.visible = True
	lm_popup.m_options.m_.visible = False
	lm_popup.m_options.m_filterlist.text = 'Filter'
	If This.FilteredCount() > 0 THen
		lm_popup.m_options.m_filterlist.checked = True
	Else
		lm_popup.m_options.m_filterlist.checked = False
	End if
	
	lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))

	Destroy lm_popup
end event

event ue_filter;call super::ue_filter;STRING		ls_filter
LONG			ll_task_no
LONG			ll_current_row
LONG			ll_found_row


SetRedraw(false)

If THis.FilteredCount() > 0 THen
	ls_filter = ''
Else
	ls_filter = "task_status_code = '01' or task_status_code = '04'"
End if


ll_current_row = This.GetRow()
If ll_current_row > 0 Then
	ll_task_no = This.GetITemNumber(ll_current_row,'task_no')
End if

//Remove all the records from the datawindow so the rowfocuschanged
//code will execute if the current row is 1
If This.SetFilter('task_no = -1') = -1 Then SignalError(-666,'Error setting filter')
If This.Filter() = -1 Then SignalError(-666,'Error filtering')

//Set the real fitler
If This.SetFilter(ls_filter) = -1 Then SignalError(-666,'Error setting filter')
If This.Filter() = -1 Then SignalError(-666,'Error filtering')

If THis.RowCount() > 0 Then
	ll_found_row = This.Find("task_no = " + String(ll_task_no),1,THis.RowCount())
	If ll_found_row > 0 Then
		This.ScrollToRow(ll_found_row)
	Else
		This.ScrollToROw(1)
	End if		
End if

setredraw(true)
end event

event doubleclicked;call super::doubleclicked;pb_max.event ue_doubleclicked()
end event

event ue_print;DATASTORE lds_text

lds_text = CREATE DATASTORE

lds_text.dataobject = 'd_rehab_task_action_list_color'
lds_text.settransobject(sqlca)
dw_action_list.sharedata(lds_text)

lds_text.Print()
end event

type dw_progress_notes_list from u_dw_online within tabpage_action_item
integer y = 944
integer width = 2629
integer height = 284
integer taborder = 11
string dataobject = "d_progress_notes_for_task_list"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;call super::rowfocuschanged;LONG			ll_event_no,	ll_viewer_row, ll_found_at
BOOLEAN		lb_tab_selection

If currentrow <> 0 THen
	
	SelectRow(0,False)
	SelectRow(currentrow,True)
	
	wf_retrieve_progress_note()
	
	If ib_action_item_note_has_focus = True Then
		ll_event_no = This.GetItemNumber(currentrow,'event_no')
		//Referesh the viewer
		ll_viewer_row = tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.GetRow()			
		IF ll_viewer_row > 0 THEN
			
			lb_tab_selection 		= ib_do_tab_selection
			ib_do_tab_selection 	= False
			
			IF ll_event_no <> tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.GetItemNumber(ll_viewer_row,'event_no') THEN
				ll_found_at = tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.Find('event_no = ' +&
				+ String(ll_event_no) ,1,tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.RowCount())
				
				IF ll_found_at > 0 THEN
					tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.ScrollToRow(ll_found_at)
				END IF
				
			END IF
			ib_do_tab_selection = lb_tab_selection
		END IF
	End if
End if
end event

event constructor;call super::constructor;pb_progress_list_maximize.uf_set_requestor(this)


end event

event getfocus;call super::getfocus;ib_action_item_note_has_focus = True
end event

event losefocus;call super::losefocus;ib_action_item_note_has_focus = False
end event

type pb_progress_list_maximize from cb_dw_maximize within tabpage_action_item
integer x = 2459
integer y = 952
integer taborder = 21
boolean bringtotop = true
boolean originalsize = false
end type

type pb_max from cb_dw_maximize within tabpage_action_item
integer x = 2459
integer y = 20
integer width = 91
integer height = 76
integer taborder = 11
boolean bringtotop = true
end type

type dw_action_item from u_dw_online within tabpage_action_item
event ue_dropdown pbm_dwndropdown
event ue_collaps_dropdown pbm_ncpaint
integer x = 14
integer y = 416
integer width = 2592
integer height = 452
integer taborder = 11
boolean bringtotop = true
string dataobject = "d_rehab_task_action_item"
boolean border = false
end type

event constructor;call super::constructor;This.SetTransObject(SQLCA)


end event

event itemchanged;call super::itemchanged;INTEGER	li_rtn
//Edit rules will be in nf_item_changed
li_rtn = inv_action_item.nf_item_changed(row,dwo,data)

RETURN li_rtn






end event

event retrieveend;call super::retrieveend;
If rowcount > 0 THen
	//If the datawindow is in read only mode don't call the filter.  It will crash.
	//I"m pressed for time right now so I didn't bother to find out why.  
	IF istr_window_message.as_mode <> 'READ' and vgst_user_profile.maintain_action_item_flag <> 'N' THEN
		inv_action_item.nf_filter_child('')
		
		wf_enable_action_item_controls(False)
		
	End if
End if
end event

event ue_itemchangeaccepted;call super::ue_itemchangeaccepted;inv_action_item.nf_item_change_accepted(al_row,as_column_name)

wf_enable_action_item_controls(True)
end event

event itemerror;return 3
end event

event doubleclicked;call super::doubleclicked;LONG			ll_row
LONG			ll_found
LONG			ll_related_task_no
LONG			ll_task_rows
LONG			ll_task_current_row


If this.RowCount() > 0 Then
	If dwo.name = "related_task_no" Then
		
		ll_related_task_no = This.GetItemNumber(1,'related_task_no')
		If ll_related_task_no > 0 Then
			ll_task_rows = tab_rehab_plan.tabpage_task.dw_task_list.RowCount()
			ll_task_current_row = tab_rehab_plan.tabpage_task.dw_task_list.GetRow()
			ll_found = tab_rehab_plan.tabpage_task.dw_task_list.Find('task_no = ' + String(ll_related_task_no),1,ll_task_rows )
			IF ll_found > 0 THEN
				//This is done so the rowfocuschanged will fire
				If ll_task_rows =1 or ll_task_current_row = ll_found Then
					tab_rehab_plan.SelectTab(2)
				End if
				tab_rehab_plan.tabpage_task.dw_task_list.ScrollToRow(ll_found)
				tab_rehab_plan.tabpage_task.dw_task_list.SelectRow(0,FALSE)
				tab_rehab_plan.tabpage_task.dw_task_list.SelectRow(ll_found,TRUE)			
			END IF
		End if
		
	End if
End if
end event

type cb_add_action_item from commandbutton within tabpage_action_item
integer x = 14
integer y = 760
integer width = 402
integer height = 104
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Add Item"
end type

event clicked;SetRedraw(False)

wf_add_action_item(0)

SetRedraw(True)
end event

type gb_3 from groupbox within tabpage_action_item
integer y = 1232
integer width = 2629
integer height = 324
integer taborder = 12
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Note"
end type

type dw_action_item_progress_note from u_dw_online within tabpage_action_item
integer x = 14
integer y = 1292
integer width = 2592
integer height = 252
integer taborder = 11
boolean bringtotop = true
string dataobject = "d_rehab_task_action_item_progress_note"
boolean border = false
end type

event itemchanged;call super::itemchanged;inv_action_item.nf_item_changed(row,dwo,data)
end event

event ue_itemchangeaccepted;call super::ue_itemchangeaccepted;inv_action_item.nf_item_change_accepted(al_row,as_column_name)

wf_enable_action_item_controls(True)
end event

event itemerror;return 2
end event

event rowfocuschanged;call super::rowfocuschanged;IF currentrow > 0 Then
	inv_action_item.nf_protect_progress_comment()
END IF
end event

type tab_rehab_plan_viewer from tab within w_rehab_sheet
integer y = 24
integer width = 1943
integer height = 2548
integer taborder = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
boolean raggedright = true
integer selectedtab = 1
tabpage_tasks_and_progress tabpage_tasks_and_progress
tabpage_goal_history tabpage_goal_history
tabpage_authorizations_viewer tabpage_authorizations_viewer
end type

on tab_rehab_plan_viewer.create
this.tabpage_tasks_and_progress=create tabpage_tasks_and_progress
this.tabpage_goal_history=create tabpage_goal_history
this.tabpage_authorizations_viewer=create tabpage_authorizations_viewer
this.Control[]={this.tabpage_tasks_and_progress,&
this.tabpage_goal_history,&
this.tabpage_authorizations_viewer}
end on

on tab_rehab_plan_viewer.destroy
destroy(this.tabpage_tasks_and_progress)
destroy(this.tabpage_goal_history)
destroy(this.tabpage_authorizations_viewer)
end on

event selectionchanged;

CHOOSE CASE newindex
	CASE 2
		IF ib_delayed_goal_history_retrieve Then
			tab_rehab_plan_viewer.tabpage_goal_history.dw_goal_history.Retrieve(il_claim_no)
			SQLCA.nf_handle_error('Retrieve for dw_goal_history','w_rehab_sheet','tab_rehab_plan_viewer.selectionchanged')
			ib_delayed_goal_history_retrieve = False
		End if
	CASE 3
		IF ib_delayed_authorized_items_retrieve THEN
			tab_rehab_plan_viewer.tabpage_authorizations_viewer.dw_rehab_viewer_authorizations.Retrieve(Il_claim_no)
			SQLCA.nf_handle_error('Retrieve for dw_goal_history','w_rehab_sheet','tab_rehab_plan_viewer.selectionchanged')
			ib_delayed_authorized_items_retrieve = FALSE
		END IF
	
END CHOOSE
end event

type tabpage_tasks_and_progress from userobject within tab_rehab_plan_viewer
integer x = 18
integer y = 168
integer width = 1906
integer height = 2364
long backcolor = 67108864
string text = "Tasks / Progress"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_tasks_and_progress dw_tasks_and_progress
cb_refresh_viewer cb_refresh_viewer
cb_print_report1 cb_print_report1
cb_attachments cb_attachments
dw_attachment_list dw_attachment_list
cb_task_attachments cb_task_attachments
cb_1 cb_1
cb_2 cb_2
dw_rehab_viewer_report dw_rehab_viewer_report
end type

on tabpage_tasks_and_progress.create
this.dw_tasks_and_progress=create dw_tasks_and_progress
this.cb_refresh_viewer=create cb_refresh_viewer
this.cb_print_report1=create cb_print_report1
this.cb_attachments=create cb_attachments
this.dw_attachment_list=create dw_attachment_list
this.cb_task_attachments=create cb_task_attachments
this.cb_1=create cb_1
this.cb_2=create cb_2
this.dw_rehab_viewer_report=create dw_rehab_viewer_report
this.Control[]={this.dw_tasks_and_progress,&
this.cb_refresh_viewer,&
this.cb_print_report1,&
this.cb_attachments,&
this.dw_attachment_list,&
this.cb_task_attachments,&
this.cb_1,&
this.cb_2,&
this.dw_rehab_viewer_report}
end on

on tabpage_tasks_and_progress.destroy
destroy(this.dw_tasks_and_progress)
destroy(this.cb_refresh_viewer)
destroy(this.cb_print_report1)
destroy(this.cb_attachments)
destroy(this.dw_attachment_list)
destroy(this.cb_task_attachments)
destroy(this.cb_1)
destroy(this.cb_2)
destroy(this.dw_rehab_viewer_report)
end on

type dw_tasks_and_progress from u_dw_online within tabpage_tasks_and_progress
event rowfocuschanged pbm_dwnrowchange
event ue_claim_path ( integer ai_action )
integer y = 28
integer width = 1911
integer height = 2216
integer taborder = 52
string dataobject = "d_rehab_plan_viewer_task_progress"
boolean hscrollbar = true
boolean vscrollbar = true
end type

event rowfocuschanged;call super::rowfocuschanged;LONG		ll_row, ll_found_at, ll_oldrow, ll_task_no, ll_note_found_at
INTEGER	li_event_no
STRING		ls_task_type_code, ls_action_item_entry_flag

	
IF THIS.RowCount() = 0 THEN	RETURN

ll_row = THIS.GetRow()

IF ll_row > 0 THEN
	uf_processselect(ll_row,"Mouse")
	ll_task_no 							= THIS.GetItemNumber(ll_row,'task_no')
	li_event_no 						= THIS.GetItemNumber(ll_row,'event_no')
	ls_task_type_code 			= THIS.GetItemString(ll_row,'task_type_code')
	ls_action_item_entry_flag 	= THIS.GetITemString(ll_row,'action_item_entry_flag')	
	
	//If it's not an event
	IF li_event_no = 0 THEN
		If ls_action_item_entry_flag = 'N' THen
			IF ib_do_tab_selection THEN
				tab_rehab_plan.SelectTab(2)
			END IF
			inv_tab_controller.nf_current_task_no(ll_task_no)
		else	//ls_action_item_entry_flag = 'Y'
			IF ib_do_tab_selection THEN
				tab_rehab_plan.SelectTab(7)
			END IF
			ll_found_at = tab_rehab_plan.tabpage_action_item.dw_action_list.Find('task_no = ' + String(ll_task_no),1,tab_rehab_plan.tabpage_action_item.dw_action_list.RowCount())
			IF ll_found_at > 0 THEN
				tab_rehab_plan.tabpage_action_item.dw_action_list.ScrollToRow(ll_found_at)
			END IF		
		End if
	ELSE
		//If it's an event but it's a action item progress note then
		If ls_action_item_entry_flag = 'Y' Then
			IF ib_do_tab_selection THEN
				tab_rehab_plan.SelectTab(7)
			END IF
			ll_found_at = tab_rehab_plan.tabpage_action_item.dw_action_list.Find('task_no = ' + String(ll_task_no),1,tab_rehab_plan.tabpage_action_item.dw_action_list.RowCount())
			IF ll_found_at > 0 THEN
				tab_rehab_plan.tabpage_action_item.dw_action_list.ScrollToRow(ll_found_at)
				ll_note_found_at = tab_rehab_plan.tabpage_action_item.dw_progress_notes_list.find("event_no = " + String(li_event_no),1,1000)
				If ll_note_found_at > 0 Then
					tab_rehab_plan.tabpage_action_item.dw_progress_notes_list.ScrollToRow(ll_note_found_at)
				End if
			END IF
		else
			ll_oldrow = currentrow
			IF ib_do_tab_selection THEN
				tab_rehab_plan.SelectTab(4)
			END IF
			inv_tab_controller.nf_current_task_no(ll_task_no)
			tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.ScrollToRow(ll_oldrow)
		End if					
	END IF
END IF

end event

event ue_claim_path(integer ai_action);
/*	This function creates the filter to be returned back to the rehab sheet's rehab viewer.
*/

/* ACTION
1. claim path with progress notes and communication
2. claim path with progress note NO communications
3. clear filter

the code path as demonstrated appeared to have the following filter -  task_entry_flag='Y' and follow_up_flag='N'

and progress_note_flag='N'

*/

STRING	ls_the_rehab_filter
		
SetPointer(HourGlass!)

/* create the filter */
CHOOSE CASE ai_action
	CASE 1 //claim path with progress notes and communication
		ls_the_rehab_filter = "task_entry_flag='Y'  and follow_up_flag='N' and progress_note_flag='N'"
		
	CASE 2 //claim path with progress note NO communications
		ls_the_rehab_filter = "task_entry_flag='Y'  and follow_up_flag='N' and task_type_code <>'CO' and progress_note_flag='N'"
		
	CASE 3 //clear filter	
		ls_the_rehab_filter = ''
END CHOOSE

/*	Set filter on data window and filter. */		
THIS.SetFilter("")
THIS.Filter()
THIS.SetFilter(ls_the_rehab_filter)
THIS.Filter()




















//	
//IF dw_rehab_viewer_main_filter.AcceptText() = -1 THEN
//	is_return_filter = 'CANCEL'
//	RETURN 0
//END IF
//
////	Determine which main filter criteria choice was selected.
//ls_main_filter_choice = dw_rehab_viewer_main_filter.GetItemString(1,"main_filter_choice")
//
//// Build the main filter section of the filter string to be returned.
//ls_main_filter="task_entry_flag='Y'"
//is_return_filter= is_return_filter + " and progress_note_flag='Y'"
//	
////	Determine which sub filter criteria choice was selected.
//ls_sub_filter_choice = dw_rehab_viewer_main_filter.GetItemString(1,"sub_filter_choice")
//	
//	// Build the sub filter section of the filter string to be returned.	
//	CHOOSE CASE ls_sub_filter_choice
//		CASE "SA"		//Type
//			if ls_main_filter_choice <> "MB" then
//				ls_task_type_code=dw_rehab_viewer_main_filter.GetItemString(1,'task_type_code')
//				ls_task_sub_type_code=dw_rehab_viewer_main_filter.GetItemString(1,'task_sub_type_code')
//			else
//				ls_task_type_code=dw_rehab_viewer_main_filter.GetItemString(1,'mm_task_type_code')
//				ls_task_sub_type_code=dw_rehab_viewer_main_filter.GetItemString(1,'mm_task_sub_type_code')
//			end if
//			ls_task_specific_code=dw_rehab_viewer_main_filter.GetItemString(1,'task_specific_code')
//			
//			if not isnull(ls_task_type_code) and ls_task_type_code<>"" then
//				ls_sub_filter=" and task_type_code='" + ls_task_type_code + "'"
//				if not isnull(ls_task_sub_type_code) and ls_task_sub_type_code<>"" and ls_task_sub_type_code<>"." then
//					ls_sub_filter=ls_sub_filter + " and task_sub_type_code='" + ls_task_sub_type_code + "'"
//					if not isnull(ls_task_specific_code) and ls_task_specific_code<>"" and ls_task_specific_code<>"." then
//						ls_sub_filter=ls_sub_filter + " and task_specific_code='" + ls_task_specific_code + "'"
//					end if
//				end if
//			else
//				ls_sub_filter=""
//			end if
//			
//		CASE "SB"		//Date
//			ldt_from_datetime = dw_rehab_viewer_main_filter.GetItemDatetime(1,'start_date_from')
//			ldt_to_datetime = dw_rehab_viewer_main_filter.GetItemDatetime(1,'start_date_to')
//			IF (ldt_to_datetime < ldt_from_datetime) then
//				MessageBox("Invalid Date","The 'To:' date must be after or on the 'From:' date.")
//				dw_rehab_viewer_main_filter.SetColumn('start_date_to')
//				dw_rehab_viewer_main_filter.SetFocus()
//				ls_sub_filter = ""
//				RETURN -1
//			elseif (IsNull(ldt_from_datetime) and IsNull(ldt_to_datetime)) THEN
//				MessageBox("Invalid Date","A date must be entered into the 'From:' or 'To:'.")
//				dw_rehab_viewer_main_filter.SetColumn('start_date_from')
//				dw_rehab_viewer_main_filter.SetFocus()
//				ls_sub_filter = ""
//				RETURN -1
//			ELSE
//				
//				IF (not IsNull(ldt_to_datetime) and IsNull(ldt_from_datetime)) THEN
//					ls_sub_filter = " and (planned_start_date <= " 
//					ls_sub_filter = ls_sub_filter + String(ldt_to_datetime,'YYYY-MM-DD') 
//					ls_sub_filter = ls_sub_filter +" or actual_start_date<= " 
//					ls_sub_filter = ls_sub_filter + String(ldt_to_datetime,'YYYY-MM-DD') +")"
//					
//				elseif (not IsNull(ldt_to_datetime) and not IsNull(ldt_from_datetime)) then
//					ls_sub_filter = " and ((planned_start_date >= " 
//					ls_sub_filter = ls_sub_filter + String(ldt_from_datetime,'YYYY-MM-DD') 
//					ls_sub_filter = ls_sub_filter + " and planned_start_date <= " 
//					ls_sub_filter = ls_sub_filter + String(ldt_to_datetime,'YYYY-MM-DD') +") or "
//					ls_sub_filter = ls_sub_filter +" (actual_start_date >= " 
//					ls_sub_filter = ls_sub_filter + String(ldt_from_datetime,'YYYY-MM-DD') 
//					ls_sub_filter = ls_sub_filter + " and actual_start_date <= " 
//					ls_sub_filter = ls_sub_filter + String(ldt_to_datetime,'YYYY-MM-DD') +"))"
//					
//				elseif (IsNull(ldt_to_datetime) and not IsNull(ldt_from_datetime)) then 
//					ls_sub_filter = " and (planned_start_date >= " 
//					ls_sub_filter = ls_sub_filter + String(ldt_from_datetime,'YYYY-MM-DD') 
//					ls_sub_filter = ls_sub_filter +" or actual_start_date>= " 
//					ls_sub_filter = ls_sub_filter + String(ldt_from_datetime,'YYYY-MM-DD') +")"
//
//				END IF
//				
//			END IF
//
//		CASE "SC"		//Responsible User ID
//			ls_sub_filter= ""
//			ls_string=dw_rehab_viewer_main_filter.GetItemString(1,'responsible_user_id')
//			if isnull(ls_string) or ls_string="." then
//				MessageBox("Invalid Responsible User ID","No resonsible users found to select.")
//				ls_sub_filter=""
//				//RETURN -1
//			else
//				ls_sub_filter=" and responsible_user_id='" + ls_string + "'"
//			end if
//
//		CASE "SD"		//Task Status
//			ls_string=dw_rehab_viewer_main_filter.GetItemString(1,'task_status')
//			if isnull(ls_string) then
//				MessageBox("Invalid Task Status","A valid Task Status must be selected.")
//				ls_sub_filter=""
//				//RETURN -1
//			else
//				ls_sub_filter=" and task_status='" + ls_string + "'"
//			end if
//			
//		CASE "SE"		//Remove Current Sub-filter
//			ls_sub_filter=""
//			
//		CASE ELSE
//
///*	Remove current filter.
//*/
//		ls_sub_filter = ""
//
//	END CHOOSE
//
//
///*Build the required filter string to be used to filter the viewer display
//*/
//	if ls_main_filter<>"" then
//		if ls_sub_filter<>"" then
//			is_return_filter=ls_main_filter + ls_sub_filter	
//		else
//			is_return_filter=ls_main_filter
//		end if
//	else
//		if ls_sub_filter<>"" then
//			is_return_filter=ls_sub_filter
//		else
//			is_return_filter=""
//		end if
//	end if
//
///*Handle the Progress Notes dependant on main filter choice seelction
//*/
//if dw_rehab_viewer_main_filter.GetItemString(1,'progress_notes_flag') <>"Y" then
//	is_return_filter= is_return_filter + " and progress_note_flag='N'"
//end if
//
//
//RETURN 0
end event

event ue_filter;call super::ue_filter;/*This user event was modified to use the new filtering functionality
introduced during the Action item functionality implementation.
by Roland Baker (xwave) in November 2002.

this event calls the w_rehab_viewer_main_filter window passing 
along a s_filter_state structure variable contained in the REHAB.PBL library.

*/
STRING	ls_the_rehab_filter

SetPointer(HourGlass!)

//Set the claim number variable of the structure to be passed
	str_state.claim_no=il_claim_no

//Open the filter window paaing the filter structure variable.
	OpenWithParm(w_rehab_viewer_main_filter, str_state)

//Populate the filter structure with the structure returned by the filter window
	str_state=Message.PowerObjectParm

//Check to ensure the str_state structure has been instantiated
if isValid(str_state) then
//Get the filter_string value to be used to filter the viewer contents
	ls_the_rehab_filter=str_state.filter_string
/*	Check for CANCEL on response window.
*/
	IF ls_the_rehab_filter = "CANCEL" THEN
		RETURN
	ELSE
/*	Set filter on data window and filter.
*/		
		This.SetFilter("")
		This.Filter()
		THIS.SetFilter(ls_the_rehab_filter)
		THIS.Filter()
		
	END IF
end if
end event

event rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup
/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	lm_popup.m_options.m_sort.visible 			= TRUE
	lm_popup.m_options.m_filterlist.visible 		= TRUE
	lm_popup.m_options.m_claim_path.visible 	= TRUE
	lm_popup.m_options.m_full_event_comments.Visible 	= TRUE
		
	lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))

	Destroy lm_popup

end event

event doubleclicked;call super::doubleclicked;/*	Check and see what the value of attachment_total is, and if it is
	greater than 0, then open window asking user	to pick the document to view.
*/
LONG							ll_attachment_count, ll_docid
BOOLEAN						lb_do_view
INTEGER 						li_rtn
STRING							ls_attachment_icon, ls_doc_type
S_WINDOW_MESSAGE	lstr_message
	
IF row = 0 THEN RETURN

	
ls_attachment_icon	= THIS.getitemstring(row,'attachment_icon')
il_task_no	 				= THIS.GetItemNumber(row,'task_no')
	
CHOOSE CASE ls_attachment_icon

	CASE "2"		//Attachment Icon 

		lb_do_view 				= TRUE
		ll_attachment_count 	= THIS.GetItemNumber(row,'attachment_total')
				
		IF ll_attachment_count > 0 THEN
			lstr_message.al_doubleparm[1] = il_claim_no
			lstr_message.al_doubleparm[2] = il_task_no
			OpenWithParm(w_view_rehab_plan_attachments, lstr_message)
			lstr_message 		= Message.PowerObjectParm
			ll_docid 				= lstr_message.al_doubleparm[3]

			IF ll_docid <= 0 THEN
				lb_do_view = FALSE
			END IF
		
		/*	Get the document id for selected row view the document. */
			IF lb_do_view THEN
								
					IF uo_image_append.of_init(ll_docid)	<= 0 THEN	RETURN
						
					ls_doc_type =  uo_image_append.of_get_file_type()
						
					CHOOSE CASE ls_doc_type
						/*  Imaged document */ 
						CASE 'IMA', 'TIF'
							IF uo_image_append.of_append_image(ll_docid) < 0 THEN RETURN
						CASE ELSE
							iu_dw_document_path.f_manage_document(ll_docid,"V","NORMAL")
					END CHOOSE
				END IF
			END IF
						
	CASE ELSE			
END CHOOSE
end event

event constructor;/* Overridden */
end event

event ue_print;STRING	ls_visible
LONG ll_rows, ll_claim_no

ls_visible = tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.Describe("t_caution.Visible")
// warn users that there is a caution on an associated claim participant
IF ls_visible = '1' THEN
	MessageBox('Caution Flag','There is a caution on an individual associated with this claim. Please check the event log for caution events.')
END IF

ll_rows = tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.ShareData(ids_tasks_and_progress_print)

ll_rows = ids_tasks_and_progress_print.RowCount()
IF ll_rows > 0 THEN
	ids_tasks_and_progress_print.Print()
END IF
end event

type cb_refresh_viewer from commandbutton within tabpage_tasks_and_progress
event clicked pbm_bnclicked
integer x = 1399
integer y = 2252
integer width = 393
integer height = 108
integer taborder = 83
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Refresh Plan"
end type

event clicked;/*	Call function to do retrieve rehab plan.
*/
	INTEGER	li_result, li_current_tab

	li_current_tab = tab_rehab_plan.SelectedTab
	li_result = wf_rehab_viewer_retrieve(li_current_tab,il_task_no)
	IF li_result < 0 THEN
		RETURN
	END IF

end event

type cb_print_report1 from commandbutton within tabpage_tasks_and_progress
event clicked pbm_bnclicked
integer x = 5
integer y = 2252
integer width = 187
integer height = 108
integer taborder = 84
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Print"
end type

event clicked;/*	Retrieve the data for the report and then print it.
*/
LONG			ll_claim_no, ll_trancount
STRING		ls_sql_statement, ls_visible
DATETIME	ldtm_date	
	
/*Boolean used to determine if the Authorizations dw is being retrieved from the Print
  button from the Tasks/Progress tab 
*/
	ib_print = TRUE

	/* Check to make sure that a claim exists.  Rob Head 98/09/14. */
	If w_rehab_sheet.dw_basic_claim.RowCount() = 0 Then
		MessageBox('Error', 'Unable to perform process when a claim is not active in basic claim window.')
		Return -1
	End If
	
	ll_claim_no = w_rehab_sheet.dw_basic_claim.GetItemNumber(1,'claim_no')

/*	Retrieve the report. But first need to close the current transaction. This is because the 
	retrieve uses a stored procedure that makes use of temporary tables and they can not be 
	created in an open transaction (SQL Server release 4.2).
*/

		
	tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_rehab_viewer_report.Retrieve(ll_claim_no)
	IF SQLCA.nf_handle_error('dw_rehab_viewer_report.Retrieve(ll_claim_no)','w_rehab_sheet','cb_print_report1') < 0 THEN
	END IF


	  
	ldtm_date = f_server_datetime()
	ll_trancount = 0
	 
/*	Auth. report script added Jun.1/98 - users want authorization report to be automatically
	printed along with Tasks & progress report.
*/
/*	Retrieve authorizations report.
*/
	tab_rehab_plan_viewer.tabpage_authorizations_viewer.dw_rehab_viewer_authorizations.Retrieve(ll_claim_no)
	IF SQLCA.nf_handle_error('Retrieve for dw_rehab_viewer_authorizations','w_rehab_sheet',&
	'cb_print_auth_rpt clicked event') < 0 THEN
	END IF
	
/* Print the reports
*/
	tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_rehab_viewer_report.Print()
	tab_rehab_plan_viewer.tabpage_authorizations_viewer.dw_rehab_viewer_authorizations.Print()
	
	// warn users that there is a caution on an associated claim participant
	ls_visible = tab_rehab_plan_viewer.tabpage_tasks_and_progress.dw_tasks_and_progress.Describe("t_caution.Visible")
	IF ls_visible = '1' THEN
		MessageBox('Caution Flag','There is a caution on an individual associated with this claim. Please check the event log for caution events.')
	END IF

	ib_print = FALSE
end event

type cb_attachments from commandbutton within tabpage_tasks_and_progress
integer x = 197
integer y = 2252
integer width = 521
integer height = 108
integer taborder = 84
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Pr&int Attachments"
end type

event clicked;LONG	ll_rows, ll_loop, ll_docid


	ll_rows = dw_attachment_list.Retrieve(il_claim_no)
	
	IF ll_rows > 0 THEN
		ll_loop = 1
		DO WHILE ll_loop <= ll_rows
			ll_docid = dw_attachment_list.GetItemNumber(ll_loop,'docid')
			iu_dw_document_path.f_manage_document(ll_docid,"P","NORMAL")
			ll_loop = ll_loop + 1
		LOOP
	ELSE
		MessageBox('No Attachments','This rehab plan has no attachments.')
	END IF
end event

type dw_attachment_list from u_dw_online within tabpage_tasks_and_progress
boolean visible = false
integer x = 818
integer y = 2064
integer height = 360
integer taborder = 2
string dataobject = "d_attachment_list"
end type

event constructor;/* Overridden */
end event

type cb_task_attachments from commandbutton within tabpage_tasks_and_progress
integer x = 727
integer y = 2252
integer width = 663
integer height = 108
integer taborder = 84
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Print &Task Attachments"
end type

event clicked;LONG	ll_rows, ll_loop, ll_docid


	ll_rows = dw_attachment_list.Retrieve(il_claim_no)
	dw_attachment_list.SetFilter('task_no = ' + String(il_task_no))
	dw_attachment_list.Filter()
	ll_rows = dw_attachment_list.RowCount()
	IF ll_rows > 0 THEN
		ll_loop = 1
		DO WHILE ll_loop <= ll_rows
			ll_docid = dw_attachment_list.GetItemNumber(ll_loop,'docid')
			iu_dw_document_path.f_manage_document(ll_docid,"P",'NORMAL')
			ll_loop = ll_loop + 1
		LOOP
	ELSE
		MessageBox('No Attachments','This task has no attachments.')
	END IF
end event

type cb_1 from commandbutton within tabpage_tasks_and_progress
integer y = 2428
integer width = 187
integer height = 108
integer taborder = 84
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Print"
end type

type cb_2 from commandbutton within tabpage_tasks_and_progress
integer y = 2428
integer width = 187
integer height = 108
integer taborder = 85
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Print"
end type

type dw_rehab_viewer_report from u_dw_online within tabpage_tasks_and_progress
boolean visible = false
integer y = 508
integer width = 1778
integer height = 1500
integer taborder = 2
boolean bringtotop = true
string dataobject = "d_rehabilitation_plan_report"
boolean border = false
end type

event constructor;/* Overridden */
end event

type tabpage_goal_history from userobject within tab_rehab_plan_viewer
integer x = 18
integer y = 168
integer width = 1906
integer height = 2364
long backcolor = 67108864
string text = "Goal History"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_goal_history dw_goal_history
end type

on tabpage_goal_history.create
this.dw_goal_history=create dw_goal_history
this.Control[]={this.dw_goal_history}
end on

on tabpage_goal_history.destroy
destroy(this.dw_goal_history)
end on

type dw_goal_history from u_dw_online within tabpage_goal_history
integer x = 9
integer y = 20
integer width = 1792
integer height = 2264
string dataobject = "d_rehab_plan_viewer_report_goal_history"
boolean hscrollbar = true
boolean vscrollbar = true
end type

event constructor;/* Overridden */
end event

event retrievestart;call super::retrievestart;IF tab_rehab_plan_viewer.SelectedTab = 2 Then
	ib_delayed_goal_history_retrieve = False
	RETURN 0
ELSE
	ib_delayed_goal_history_retrieve = True
	RETURN 1
End if
	
end event

type tabpage_authorizations_viewer from userobject within tab_rehab_plan_viewer
integer x = 18
integer y = 168
integer width = 1906
integer height = 2364
long backcolor = 67108864
string text = "Authorized~r~nItems"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_rehab_viewer_authorizations dw_rehab_viewer_authorizations
cb_print_auth_rpt cb_print_auth_rpt
end type

on tabpage_authorizations_viewer.create
this.dw_rehab_viewer_authorizations=create dw_rehab_viewer_authorizations
this.cb_print_auth_rpt=create cb_print_auth_rpt
this.Control[]={this.dw_rehab_viewer_authorizations,&
this.cb_print_auth_rpt}
end on

on tabpage_authorizations_viewer.destroy
destroy(this.dw_rehab_viewer_authorizations)
destroy(this.cb_print_auth_rpt)
end on

type dw_rehab_viewer_authorizations from u_dw_online within tabpage_authorizations_viewer
integer y = 12
integer width = 1797
integer height = 2216
integer taborder = 2
string dataobject = "d_rehab_viewer_auth_rpt"
boolean hscrollbar = true
boolean vscrollbar = true
end type

event rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup
/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	lm_popup.m_options.m_sort.visible = TRUE
	lm_popup.m_options.m_filterlist.visible = TRUE
	lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))

	Destroy lm_popup

end event

event constructor;/* Overridden */
end event

event retrievestart;call super::retrievestart;IF tab_rehab_plan_viewer.SelectedTab = 3 Then
	ib_delayed_authorized_items_retrieve = False
	RETURN 0
ELSEIF tab_rehab_plan_viewer.SelectedTab = 1 THEN
	IF ib_print THEN
		ib_delayed_authorized_items_retrieve = False
		RETURN 0
	END IF
ELSE
	ib_delayed_authorized_items_retrieve = True
	RETURN 1
End if
end event

type cb_print_auth_rpt from commandbutton within tabpage_authorizations_viewer
integer y = 2248
integer width = 219
integer height = 108
integer taborder = 84
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Print"
end type

event clicked;/*	Retrieve the data for the report and then print it.
*/
LONG		ll_claim_no

/* Check to make sure that a claim exists.  Rob Head 98/09/14. */
If w_rehab_sheet.dw_basic_claim.RowCount() = 0 Then
	MessageBox('Error', 'Unable to perform process when a claim is not active in basic claim window.')
	Return -1
End If

ll_claim_no = w_rehab_sheet.dw_basic_claim.GetItemNumber(1,'claim_no')

/*	Retrieve the report, then print.
*/
	tab_rehab_plan_viewer.tabpage_authorizations_viewer.dw_rehab_viewer_authorizations.Retrieve(ll_claim_no)
	IF SQLCA.nf_handle_error('Retrieve for dw_rehab_viewer_authorizations','w_rehab_sheet',&
	'cb_print_auth_rpt clicked event') < 0 THEN
	END IF
	 
	tab_rehab_plan_viewer.tabpage_authorizations_viewer.dw_rehab_viewer_authorizations.Print()
end event

type st_save from statictext within w_rehab_sheet
boolean visible = false
integer x = 1321
integer y = 16
integer width = 521
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 255
long backcolor = 67108864
boolean enabled = false
string text = "Save Needed"
alignment alignment = center!
long bordercolor = 16777215
boolean focusrectangle = false
end type

type uo_image_append from u_image_append within w_rehab_sheet
boolean visible = false
integer x = 3250
integer y = 64
integer width = 1248
integer height = 240
integer taborder = 20
boolean bringtotop = true
end type

on uo_image_append.destroy
call u_image_append::destroy
end on

type st_1 from statictext within w_rehab_sheet
integer x = 4238
integer y = 392
integer width = 174
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Week:"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_2 from statictext within w_rehab_sheet
integer x = 4238
integer y = 460
integer width = 462
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "from disablement"
boolean focusrectangle = false
end type

type st_week_no from statictext within w_rehab_sheet
integer x = 4430
integer y = 392
integer width = 169
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

