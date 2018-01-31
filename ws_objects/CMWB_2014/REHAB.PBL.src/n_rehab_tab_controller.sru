$PBExportHeader$n_rehab_tab_controller.sru
forward
global type n_rehab_tab_controller from nonvisualobject
end type
end forward

global type n_rehab_tab_controller from nonvisualobject
event ue_action_item_updated ( long al_task_no )
event ue_task_updated ( long al_task_no )
event ue_current_action_item_changed ( long al_new_task_no )
event ue_current_task_changed ( long al_new_task_no )
event ue_action_item_added ( long al_task_no,  long al_related_task_no )
event ue_task_added ( long al_task_no )
event ue_current_action_item_note_changed ( long al_event_no )
event ue_current_task_note_changed ( long al_event_no )
end type
global n_rehab_tab_controller n_rehab_tab_controller

type variables
LONG	il_current_task_no
LONG	il_current_task_note_no
LONG	il_current_action_item_no
LONG	il_current_action_item_note_no
LONG	il_claim_no

Private			INTEGER	il_active_item


//These constants are used to identify which item is active on
//the rehab plan.  The constant
CONSTANT INTEGER 	II_TASK = 1
CONSTANT	INTEGER	II_ACTION_ITEM = 2
CONSTANT	INTEGER	II_TASK_NOTE = 3
CONSTANT	INTEGER	II_ACTION_ITEM_NOTE = 4

CONSTANT INTEGER	II_ACTION_ITEM_TAB = 7
CONSTANT INTEGER	II_TASK_TAB = 2
CONSTANT INTEGER	II_ACTION_ITEM_NOTE_TAB = 7
CONSTANT INTEGER	II_TASK_NOTE_TAB = 4

//Datawindows that will be controlled from this object
u_dw_online			idw_task_list
u_dw_online 		idw_action_item_list
u_dw_online			idw_viewer
u_dw_online			idw_task_note_list
u_dw_online			idw_action_item_note_list
u_dw_online			idw_authorization_task_list

TAB			itb_rehab_tab

BOOLEAN		ib_processing
BOOLEAN		ib_authorization_retrieve_pending
BOOLEAN		ib_do_row_selection = True

end variables

forward prototypes
public function integer nf_set_active_item (integer al_active_item)
public subroutine nf_set_claim_no (long al_claim_no)
public function long nf_current_task_no ()
public function integer nf_current_task_no (long al_task_no)
end prototypes

public function integer nf_set_active_item (integer al_active_item);
IF al_active_item = il_active_item THEN RETURN 0 //No change

CHOOSE CASE al_active_item
	CASE II_TASK
		itb_rehab_tab.SelectTab(II_TASK_TAB)
	CASE II_ACTION_ITEM
		itb_rehab_tab.SelectTab(II_ACTION_ITEM_TAB)
	CASE II_TASK_NOTE
		itb_rehab_tab.SelectTab(II_TASK_NOTE_TAB)
	CASE II_ACTION_ITEM_NOTE
		itb_rehab_tab.SelectTab(II_ACTION_ITEM_NOTE_TAB)
	CASE ELSE
		SignalError(-666,'Error setting current item on rehab plan')
END CHOOSE

il_active_item = al_active_item

RETURN 1
end function

public subroutine nf_set_claim_no (long al_claim_no);il_claim_no = al_claim_no
end subroutine

public function long nf_current_task_no ();RETURN il_current_task_no
end function

public function integer nf_current_task_no (long al_task_no);LONG 		ll_found_row
LONG		ll_task_dw_task_no
LONG		ll_viewer_dw_task_no
LONG		ll_auth_dw_task_no
LONG		ll_progress_dw_task_no

If al_task_no = 0 Then RETURN -1
If ib_processing Then RETURN 0

il_current_task_no = al_task_no
IF not ib_do_row_selection Then return 0

ib_processing = True


IF itb_rehab_tab.SelectedTab = 2 Then
	IF idw_task_list.RowCount() > 0 Then
		ll_task_dw_task_no = idw_task_list.GetItemNumber(idw_task_list.GetRow(),'task_no')
		If ll_task_dw_task_no <> al_task_no Then
			ll_found_row = idw_task_list.Find('task_no = ' + String(al_task_no),1,1000)
			If ll_found_row > 0 Then
				idw_task_list.ScrollToRow(ll_found_row)
			End if
		End if
	End if
END if

IF itb_rehab_tab.SelectedTab = 3 Then
	IF	idw_authorization_task_list.RowCount() > 0 Then
		ll_auth_dw_task_no = idw_authorization_task_list.GetItemNumber(idw_authorization_task_list.GetRow(),'task_no')
		IF ll_auth_dw_task_no <> al_task_no THEN
			ll_found_row = idw_authorization_task_list.Find('task_no = ' + String(al_task_no),1,1000)
			IF ll_found_row > 0 Then
				idw_authorization_task_list.ScrollToRow(ll_found_row)
			End if
		End if
	ENd if
ENd if
		
IF itb_rehab_tab.SelectedTab = 4 THen
	IF idw_task_note_list.RowCount() > 0 Then
		ll_progress_dw_task_no = idw_task_note_list.GetITemNumber(idw_task_note_list.GetRow(),'task_no')
		IF ll_progress_dw_task_no <> al_task_no THEN
			ll_found_row = idw_task_note_list.Find('task_no = ' + String(al_task_no),1,1000)
			IF ll_found_row > 0 Then
				idw_task_note_list.ScrollToRow(ll_found_row)
			End if
		End if
	End if
End if
		
		
If idw_viewer.RowCount() > 0 Then
	ll_viewer_dw_task_no = idw_viewer.GetItemNumber(idw_viewer.GetRow(),'task_no')
	If ll_viewer_dw_task_no <> al_task_no Then
		ll_found_row = idw_viewer.Find('task_no = ' + String(al_task_no),1,1000)
		If ll_found_row > 0 Then
			idw_viewer.ScrollToRow(ll_found_row)
		End if
	End if
End if



ib_processing = False
RETURN 1
end function

on n_rehab_tab_controller.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_rehab_tab_controller.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

