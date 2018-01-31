$PBExportHeader$n_checklist.sru
forward
global type n_checklist from nonvisualobject
end type
end forward

global type n_checklist from nonvisualobject
end type
global n_checklist n_checklist

type variables


n_transaction itr_trans_object

Boolean ib_saved_checklist
string  is_module_code, is_checklist_type_code, is_checklist_subscriber_type_code

Protected:
datawindow		idw_dw[]
u_checklist		iuo_checklist
end variables

forward prototypes
public function long nf_get_last_checklist_no ()
public subroutine nf_commit_transaction ()
public subroutine nf_rollback_transaction ()
public function integer nf_retrieve_checklists (string as_checklist_type_code, long al_checklist_no)
public function long nf_check_for_last_completed_step (long al_checklist_no)
public function integer nf_save_checklist (long ar_checklist_no, datetime adt_concluded_date, string as_concluded_by_user_id, string as_checklist_comment, string as_checklist_status_code)
public function integer nf_get_next_checklist_step (long al_checklist_no)
public function long nf_max_completed_step (long al_checklist_no)
public function integer nf_cancel_checklist_step (long al_checklist_no, long al_checklist_step_no)
public subroutine nf_set_notes_saved_value (boolean ab_value)
public function boolean nf_get_notes_saved_value ()
public subroutine nf_set_checklist_object (u_checklist auo_checklist)
public function integer nf_set_notes_row (long al_checklist_no, integer ai_step_no, integer ai_row)
public function long nf_get_last_subscriber_no ()
public function long nf_create_checklist_subscriber (long al_checklist_no, string as_checklist_subscriber_type_code)
public subroutine nf_insert_subscriber_checklist_xref (long al_subscriber_no, long al_checklist_no)
public function integer nf_find_row (string as_column, string as_value, datawindow adw_dw)
public function integer nf_retrieve_notes_checklist (long al_checklist_no, long al_step_no)
public subroutine nf_reset_datawindows ()
public function integer nf_check_note_modified_status ()
public function integer nf_filter_checklist_step_status (string as_module_code, string as_checklist_type_code, string as_checklist_step_type_code)
public function integer nf_filter_checklist_status (string as_checklist_type_code)
public function integer nf_save_checklist_step (integer ar_checklist_no, string as_checklist_step_type_code, string as_checklist_step_status_code, datetime adt_concluded_date, string as_concluded_by_user, string as_comment, string as_module_code, string as_checklist_type_code, string as_status_assigned_method_code)
public function integer nf_checklist_step_type_status_xref (string as_module_code, string as_checklist_type_code, string as_checklist_step_type_code, string as_checklist_step_status_code, string as_status_assigned_method_code, long al_checklist_no)
public function integer nf_checklist_type_status_xref (string as_module_code, string as_checklist_type_code, string as_checklist_status_code, string as_status_assigned_method_code, long al_checklist_no)
public function integer nf_check_creation_business_rules (string as_module_code, string as_checklist_type_code, string as_checklist_subscriber_type_code)
public subroutine nf_set_checklist_subscriber_type (string as_checklist_subscriber_type_code)
public function long nf_create_checklist (string as_module_code, string as_checklist_type_code, string as_checklist_status_code, string as_checklist_status_assignmt_meth_code, string as_checklist_step_status_code, string as_checklist_step_status_assmt_meth_code, long ar_related_checklist_no)
public subroutine nf_set_datawindow (datawindow adw_dw[], n_transaction anv_transobj)
public subroutine nf_set_transaction (ref n_transaction atr_trans_object)
end prototypes

public function long nf_get_last_checklist_no ();Long ll_last_checklist_no

Select last_checklist_no
Into    :ll_last_checklist_no
From Last_Checklist_No
Using itr_trans_object;

itr_trans_object.nf_handle_error('n_checklist','nf_last_checklist_no','Select last_checklist_no')

Update Last_Checklist_No
Set last_checklist_no = :ll_last_checklist_no + 1
Using itr_trans_object;

itr_trans_object.nf_handle_error('n_checklist','nf_last_checklist_no','Select last_checklist_no')

Select last_checklist_no
Into    :ll_last_checklist_no
From Last_Checklist_No
Using itr_trans_object;

itr_trans_object.nf_handle_error('n_checklist','nf_last_checklist_no','Select last_checklist_no')


Return ll_last_checklist_no

end function

public subroutine nf_commit_transaction ();
Commit Using itr_trans_object;

itr_trans_object.nf_handle_error('n_checklist','nf_commit_transaction','Commit')
end subroutine

public subroutine nf_rollback_transaction ();
itr_trans_object.nf_rollback_transaction()

end subroutine

public function integer nf_retrieve_checklists (string as_checklist_type_code, long al_checklist_no);/*idw_dw[1] = tab_checklist.tabpage_checklist.dw_checklist
idw_dw[2] = tab_checklist.tabpage_checklist_notes.dw_checklist_notes

idw_dw[4] = tab_checklist.tabpage_checklist_history.dw_checklist_history_master
idw_dw[5] = tab_checklist.tabpage_checklist_history.dw_checklist_history_detail */

INTEGER   li_rows, li_step_no, li_row,li_find, li_row_10
DATETIME  ldtm_checklist_concluded_date
LONG      ll_history_master_checklist_no
STRING    ls_step_type, ls_checklist_type_code

li_rows     = idw_dw[1].Retrieve(al_checklist_no)
itr_trans_object.nf_handle_error('n_checklist','nf_retrieve_checklists','idw_dw[1].Retrieve')

li_step_no = nf_get_next_checklist_step(al_checklist_no)

li_find = idw_dw[1].Find('checklist_step_no  = '+ String(li_step_no),1,idw_dw[1].RowCount())

IF li_find < 1 THEN
	li_find  = 1
END IF
	
idw_dw[1].SelectRow(0, false)
idw_dw[1].ScrollToRow(li_find)
idw_dw[1].SelectRow(li_find,TRUE)

li_step_no = idw_dw[1].GetItemNumber(li_find,'checklist_step_no')
ls_step_type = idw_dw[1].GetItemString(li_find,'checklist_step_type_code')

li_rows     = idw_dw[7].Retrieve(al_checklist_no)
itr_trans_object.nf_handle_error('n_checklist','nf_retrieve_checklists','idw_dw[7].Retrieve')


ldtm_checklist_concluded_date = idw_dw[7].GetItemDateTime(1,'concluded_date')
ls_checklist_type_code = idw_dw[7].GetItemString(1,'checklist_type_code')

IF IsNull(ldtm_checklist_concluded_date) THEN
	// filter checklist status - there is only ever one row in checklist master
	nf_filter_checklist_status(ls_checklist_type_code)
END IF


// notes master
li_rows     = idw_dw[6].Retrieve(al_checklist_no)
itr_trans_object.nf_handle_error('n_checklist','nf_retrieve_checklists','idw_dw[6].Retrieve')

//notes detail
li_rows     = idw_dw[2].Retrieve(al_checklist_no)
itr_trans_object.nf_handle_error('n_checklist','nf_retrieve_checklists','idw_dw[2].Retrieve')

// history master
li_rows     = idw_dw[4].Retrieve(al_checklist_no)
itr_trans_object.nf_handle_error('n_checklist','nf_retrieve_checklists','idw_dw[4].Retrieve')

idw_dw[4].SelectRow(0, false)
idw_dw[4].SelectRow(1, true)

// history detail
IF li_rows > 0 THEN
	ll_history_master_checklist_no = idw_dw[4].GetItemNumber(idw_dw[4].GetRow(),'checklist_no')
	li_rows     = idw_dw[5].Retrieve(ll_history_master_checklist_no)
	itr_trans_object.nf_handle_error('n_checklist','nf_retrieve_checklists','idw_dw[5].Retrieve')
END IF

// comment tabs
li_rows     = idw_dw[8].Retrieve(al_checklist_no,li_step_no)
itr_trans_object.nf_handle_error('n_checklist','nf_retrieve_checklists','idw_dw[5].Retrieve')

li_rows     = idw_dw[9].Retrieve(al_checklist_no)
itr_trans_object.nf_handle_error('n_checklist','nf_retrieve_checklists','idw_dw[5].Retrieve')

// cancelled comment
//li_rows     = idw_dw[3].Retrieve(al_checklist_no)
//itr_trans_object.nf_handle_error('n_checklist','nf_retrieve_checklists','idw_dw[3].Retrieve')

//dw_checklist_cancelled_step_comment_tab
li_row_10     = idw_dw[10].Retrieve(al_checklist_no,li_step_no)
itr_trans_object.nf_handle_error('n_checklist','nf_retrieve_checklists','idw_dw[5].Retrieve')

IF li_row_10 > 0 THEN
	IF idw_dw[10].GetItemString(li_row_10,'cancelled_comment') <> '' THEN
		iuo_checklist.tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_4.enabled = TRUE
		iuo_checklist.tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_4.visible = True
	ELSE
		iuo_checklist.tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_4.enabled = FALSE
		iuo_checklist.tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_4.visible = False
	END IF
ELSE
	iuo_checklist.tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_4.enabled = FALSE
	iuo_checklist.tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_4.visible = False
END IF




Return 1
end function

public function long nf_check_for_last_completed_step (long al_checklist_no);long ll_max_complete_mandatory_step_no, ll_min_incomplete_mandatory_step_no


Select Max(a.checklist_step_no)
Into :ll_max_complete_mandatory_step_no
From CHECKLIST_STEP a, Checklist_Step_Type_Xref b, CHECKLIST c
Where a.checklist_no = c.checklist_no
And a.checklist_step_type_code = b.checklist_step_type_code
 And c.checklist_type_code = b.checklist_type_code
 And b.module_code = c.module_code
And a.checklist_no = :al_checklist_no
And b.mandatory_flag = 'Y'
And b.active_flag = 'Y'
And a.concluded_date Is Not NULL
Using itr_trans_object;

itr_trans_object.nf_handle_error('w_checklist','dw_checklist.clicked','Select Max(a.checklist_step_no) 1')

Select Min(a.checklist_step_no)
Into :ll_min_incomplete_mandatory_step_no
From CHECKLIST_STEP a, Checklist_Step_Type_Xref b, CHECKLIST c
Where a.checklist_no = c.checklist_no
And a.checklist_step_type_code = b.checklist_step_type_code
And c.checklist_type_code = b.checklist_type_code
And b.module_code = c.module_code
And a.checklist_no = :al_checklist_no
And a.checklist_step_no > :ll_max_complete_mandatory_step_no
And b.mandatory_flag = 'Y'
And b.active_flag = 'Y'
And a.concluded_date Is NULL 
Using itr_trans_object;

itr_trans_object.nf_handle_error('w_checklist','dw_checklist.clicked','Select Max(a.checklist_step_no) 2')

Return ll_min_incomplete_mandatory_step_no
end function

public function integer nf_save_checklist (long ar_checklist_no, datetime adt_concluded_date, string as_concluded_by_user_id, string as_checklist_comment, string as_checklist_status_code);// Update CHECKLIST


Update CHECKLIST
Set checklist_status_code = :as_checklist_status_code, concluded_date = :adt_concluded_date, concluded_by_user_id = :as_concluded_by_user_id, checklist_comment = :as_checklist_comment
Where checklist_no = :ar_checklist_no
Using itr_trans_object;

itr_trans_object.nf_handle_error('n_checklist','nf_save_checklist','Update CHECKLIST')


Return 0
end function

public function integer nf_get_next_checklist_step (long al_checklist_no);LONG		ll_min_incomplete_step_no

	Select IsNull(Min(a.checklist_step_no),0)
	Into :ll_min_incomplete_step_no
	From CHECKLIST_STEP a, Checklist_Step_Type_Xref b, CHECKLIST c
	Where a.checklist_no = c.checklist_no
	And a.checklist_step_type_code = b.checklist_step_type_code
	 And c.checklist_type_code = b.checklist_type_code
	 And b.module_code = c.module_code
	And a.checklist_no = :al_checklist_no
	And a.concluded_date Is NULL
	Using itr_trans_object;

itr_trans_object.nf_handle_error('n_checklist','nf_set_next_checklist_step','Select Min(checklist_step_no)')

RETURN ll_min_incomplete_step_no
end function

public function long nf_max_completed_step (long al_checklist_no);LONG		ll_max_complete_step_no	
	
	Select IsNull(Max(a.checklist_step_no),0)
	Into :ll_max_complete_step_no
	From CHECKLIST_STEP a, Checklist_Step_Type_Xref b, CHECKLIST c
	Where a.checklist_no = c.checklist_no
	And a.checklist_step_type_code = b.checklist_step_type_code
	 And c.checklist_type_code = b.checklist_type_code
	 And b.module_code = c.module_code
	And a.checklist_no = :al_checklist_no
	And a.concluded_date Is NOT NULL
	Using itr_trans_object;

itr_trans_object.nf_handle_error('n_checklist','nf_max_completed_step','Select Max(checklist_step_no)')

RETURN ll_max_complete_step_no
end function

public function integer nf_cancel_checklist_step (long al_checklist_no, long al_checklist_step_no);Long ll_row
int li_find

//set row selection to the cancelled row, and retrieve that row in the comment datawindows.

li_find = idw_dw[1].Find('checklist_step_no = ' + String(al_checklist_step_no),1,idw_dw[1].RowCount())
IF li_find > 0 THEN
	idw_dw[1].ScrollToRow(li_find)
	idw_dw[1].SelectRow(li_find,TRUE)
	idw_dw[2].ScrollToRow(li_find)
	idw_dw[2].SelectRow(li_find,TRUE)
END IF

ll_row = idw_dw[8].Retrieve(al_checklist_no,al_checklist_step_no)
itr_trans_object.nf_handle_error('n_checklist','nf_retreive_checklists','idw_dw[8].Retrieve')

ll_row = idw_dw[10].Retrieve(al_checklist_no,al_checklist_step_no)
itr_trans_object.nf_handle_error('n_checklist','nf_retreive_checklists','idw_dw[10].Retrieve')



Messagebox('Notification','You are required to enter a step note when cancelling.',Information!)

iuo_checklist.tab_checklist.SelectedTab = 2
idw_dw[10].visible = True
idw_dw[10].enabled = True
iuo_checklist.tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_1.enabled = False
iuo_checklist.tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_2.enabled = False
iuo_checklist.tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_4.visible = true
iuo_checklist.tab_checklist.tabpage_checklist_notes.tab_comment.SelectedTab = 3

idw_dw[10].SetItem(idw_dw[10].getrow(),'checklist_step_status_code','XSM')
idw_dw[10].SetColumn("cancelled_comment")
idw_dw[10].setfocus()

Return 0
end function

public subroutine nf_set_notes_saved_value (boolean ab_value);
ib_saved_checklist = ab_value


end subroutine

public function boolean nf_get_notes_saved_value ();
Return(ib_saved_checklist)
end function

public subroutine nf_set_checklist_object (u_checklist auo_checklist);/*************************************************************************
	Description:		This function stores a reference to the parent window in an instance variable
*************************************************************************/

iuo_checklist = auo_checklist

end subroutine

public function integer nf_set_notes_row (long al_checklist_no, integer ai_step_no, integer ai_row);/*
idw_dw[2] = tab_checklist.tabpage_checklist_notes.dw_checklist_notes
idw_dw[3] = tab_checklist.tabpage_checklist_notes.dw_checklist_notes_entered
*/

int li_rows, li_step_no, li_find
datetime ldtm_concluded_date

iuo_checklist.uf_set_ib_notes_row(FALSE)

li_rows     = idw_dw[8].Retrieve(al_checklist_no,ai_step_no)
itr_trans_object.nf_handle_error('n_checklist','nf_retreive_checklists','idw_dw[3].Retrieve')

li_rows     = idw_dw[10].Retrieve(al_checklist_no,ai_step_no)
itr_trans_object.nf_handle_error('n_checklist','nf_retreive_checklists','idw_dw[3].Retrieve')

IF li_rows > 0 THEN
	//make tab 3 visible if there is a cancelled step note
	idw_dw[10].ScrollToRow(ai_row)
END IF

li_find = idw_dw[2].Find('checklist_step_no  = '+ String(ai_step_no),1,idw_dw[2].RowCount())

IF li_find > 0 THEN
	idw_dw[2].SelectRow(0, false)
	idw_dw[2].ScrollToRow(li_find)
	idw_dw[2].SelectRow(li_find,TRUE)
END IF

ldtm_concluded_date     = idw_dw[2].getitemdatetime(ai_row,'concluded_date')

idw_dw[8].ScrollToRow(ai_row)
idw_dw[8].visible = TRUE


IF IsNull(ldtm_concluded_date) THEN
	idw_dw[8].enabled = TRUE
ELSE
	idw_dw[8].enabled = False
END IF
	
idw_dw[2].setfocus()

Return 0
end function

public function long nf_get_last_subscriber_no ();Long ll_last_checklist_subscriber_no

Select last_checklist_subscriber_no
Into    :ll_last_checklist_subscriber_no
From Last_Checklist_Subscriber_No
Using itr_trans_object;

itr_trans_object.nf_handle_error('n_checklist','nf_last_checklist_no','Select last_checklist_no')

Update Last_Checklist_Subscriber_No
Set last_checklist_subscriber_no = :ll_last_checklist_subscriber_no + 1
Using itr_trans_object;

itr_trans_object.nf_handle_error('n_checklist','nf_last_checklist_scriber_no','Select last_checklist_subscriber_no')

Select last_checklist_subscriber_no
Into    :ll_last_checklist_subscriber_no
From Last_Checklist_Subscriber_No
Using itr_trans_object;

itr_trans_object.nf_handle_error('n_checklist','nf_last_checklist_subscriber_no','Select last_checklist_subscriber_no')


Return ll_last_checklist_subscriber_no

end function

public function long nf_create_checklist_subscriber (long al_checklist_no, string as_checklist_subscriber_type_code);long ll_checklist_no, ll_count, ll_checklist_subscriber_type_code, ll_subscriber_no
Int li_return

ll_subscriber_no = nf_get_last_subscriber_no()


Insert Into CHECKLIST_SUBSCRIBER
(	checklist_subscriber_no, 
	checklist_subscriber_type_code)
Values(
	:ll_subscriber_no,
	:as_checklist_subscriber_type_code)
Using itr_trans_object;

li_return = itr_trans_object.nf_handle_error('n_checklist','nf_create_checklist','insert into CHECKLIST')

is_checklist_subscriber_type_code = as_checklist_subscriber_type_code


Return ll_subscriber_no
end function

public subroutine nf_insert_subscriber_checklist_xref (long al_subscriber_no, long al_checklist_no);INSERT	SUBSCRIBER_CHECKLIST_XREF
(			checklist_subscriber_no, 
			checklist_no)
VALUES	(
			:al_subscriber_no ,
			:al_checklist_no)
USING     itr_trans_object;
itr_trans_object.nf_handle_error('n_checklist','nf_create_checklist','insert SUBSCRIBER_CHECKLIST_XREF')

end subroutine

public function integer nf_find_row (string as_column, string as_value, datawindow adw_dw);// This function gets the row that requires selection.

INTEGER	li_find
STRING	ls_find


ls_find = as_column + ' = ' + as_value

li_find = adw_dw.Find(ls_find,1,adw_dw.RowCount())

IF li_find > 0 THEN
	adw_dw.ScrollToRow(li_find)
	adw_dw.SelectRow(li_find,TRUE)
END IF


Return li_find
end function

public function integer nf_retrieve_notes_checklist (long al_checklist_no, long al_step_no);int li_rows, li_step_no, li_row,li_find, li_row_10
string ls_step_type, ls_checklist_type

// notes master
li_rows     = idw_dw[6].Retrieve(al_checklist_no)
itr_trans_object.nf_handle_error('n_checklist','nf_retreive_checklists','idw_dw[6].Retrieve')

//notes detail
li_rows     = idw_dw[2].Retrieve(al_checklist_no)
itr_trans_object.nf_handle_error('n_checklist','nf_retreive_checklists','idw_dw[2].Retrieve')

// comment tabs
li_rows     = idw_dw[8].Retrieve(al_checklist_no,li_step_no)
itr_trans_object.nf_handle_error('n_checklist','nf_retreive_checklists','idw_dw[5].Retrieve')

li_rows     = idw_dw[9].Retrieve(al_checklist_no)
itr_trans_object.nf_handle_error('n_checklist','nf_retreive_checklists','idw_dw[5].Retrieve')

// cancelled comment
//li_rows     = idw_dw[3].Retrieve(al_checklist_no)
//itr_trans_object.nf_handle_error('n_checklist','nf_retreive_checklists','idw_dw[3].Retrieve')

//dw_checklist_cancelled_step_comment_tab
li_row_10     = idw_dw[10].Retrieve(al_checklist_no,li_step_no)
itr_trans_object.nf_handle_error('n_checklist','nf_retreive_checklists','idw_dw[5].Retrieve')

IF li_row_10 > 0 THEN
	iuo_checklist.tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_4.enabled = TRUE
	iuo_checklist.tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_4.visible = True
ELSE
	iuo_checklist.tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_4.enabled = FALSE
	iuo_checklist.tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_4.visible = False
END IF


Return 1
end function

public subroutine nf_reset_datawindows ();// tab_checklist.tabpage_checklist.cb_1.TriggerEvent(clicked!)

INTEGER li_counter, li_upper, li_rtn

li_upper = UpperBound(idw_dw)

IF li_upper > 0 THEN
	FOR li_counter = 1 TO li_Upper
		li_rtn = idw_dw[li_counter].Reset()
	NEXT
END IF
end subroutine

public function integer nf_check_note_modified_status ();BOOLEAN   lb_modified

// function determines the modify status any of the checklist note datawindows 

/*
idw_dw[3] = tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_3.dw_checklist_cancelled_comment_tab
idw_dw[8] = tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_1.dw_checklist_step_notes_entered_tab
idw_dw[9] = tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_2.dw_checklist_notes_entered_tab
idw_dw[10] = tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_4.dw_checklist_cancelled_step_comment_tab
*/


CHOOSE CASE TRUE
	CASE idw_dw[8].GetItemStatus(1,0,Primary!) <> NotModified!
		MessageBox('Unsaved Checklist Note','There is an unsaved Checklist Step note. Please save or cancel the note.',Exclamation!)
		RETURN -1
		
	CASE idw_dw[9].GetItemStatus(1,0,Primary!) <> NotModified!
		MessageBox('Unsaved Checklist Note','There is an unsaved Checklist note. Please save or cancel the note.',Exclamation!)
		RETURN -1
		
	CASE idw_dw[10].GetItemStatus(1,0,Primary!) <> NotModified!
		MessageBox('Unsaved Checklist Note','There is an unsaved Cancelled Checklist Step note. Please save or cancel the note.',Exclamation!)
		RETURN -1
		
	CASE idw_dw[3].GetItemStatus(1,0,Primary!) <> NotModified!
		MessageBox('Unsaved Checklist Note','There is an unsaved Cancelled Checklist Step note. Please save or cancel the note.',Exclamation!)
		RETURN -1
		
END CHOOSE

RETURN 0

end function

public function integer nf_filter_checklist_step_status (string as_module_code, string as_checklist_type_code, string as_checklist_step_type_code);DataWindowChild   ldwc_step_status
LONG              ll_rows
STRING            ls_filter


// filter the step status dddw to prevent selection of inactive steps
ls_filter = 'xref_active = "Y" AND status_active = "Y" AND status_assigned_method_code = "M"'

idw_dw[1].GetChild('checklist_step_status_code',ldwc_step_status)
ldwc_step_status.SetFilter(ls_filter)
ldwc_step_status.Filter()


ldwc_step_status.SetTransObject(itr_trans_object)

ll_rows = ldwc_step_status.Retrieve(as_module_code,as_checklist_type_code,as_checklist_step_type_code)


RETURN 0
end function

public function integer nf_filter_checklist_status (string as_checklist_type_code);DataWindowChild   ldwc_status
LONG              ll_rows
STRING            ls_filter

// filter the checklist status dddw to prevent selection of inactive checklist statuses
ls_filter = '( xref_active = "Y" AND status_active = "Y" and status_assigned_method_code = "M") OR checklist_status_code = "IA"'

idw_dw[7].GetChild('checklist_status_code',ldwc_status)
ldwc_status.SetFilter(ls_filter)
ldwc_status.Filter()	

ldwc_status.SetTransObject(itr_trans_object)

ll_rows = ldwc_status.Retrieve(as_checklist_type_code)	


RETURN 0
end function

public function integer nf_save_checklist_step (integer ar_checklist_no, string as_checklist_step_type_code, string as_checklist_step_status_code, datetime adt_concluded_date, string as_concluded_by_user, string as_comment, string as_module_code, string as_checklist_type_code, string as_status_assigned_method_code);//Saves checklist Item data
INTEGER   li_rtn
STRING    ls_checklist_step_type_desc, ls_status_assigned_method_desc


li_rtn = nf_checklist_step_type_status_xref(as_module_code,as_checklist_type_code,as_checklist_step_type_code,as_checklist_step_status_code,as_status_assigned_method_code,ar_checklist_no)

IF li_rtn = 0 THEN
	RETURN -1
END IF
	
UPDATE CHECKLIST_STEP
SET    checklist_step_status_code = :as_checklist_step_status_code,
       concluded_date = :adt_concluded_date,
		 concluded_by_user_id = :as_concluded_by_user,
		 cancelled_comment = :as_comment
WHERE  checklist_no = :ar_checklist_no
AND    checklist_step_type_code = :as_checklist_step_type_code
Using itr_trans_object;

itr_trans_object.nf_handle_error('n_checklist','nf_save_checklist_step','Update CHECKLIST_STEP')


Return 0
end function

public function integer nf_checklist_step_type_status_xref (string as_module_code, string as_checklist_type_code, string as_checklist_step_type_code, string as_checklist_step_status_code, string as_status_assigned_method_code, long al_checklist_no);INTEGER      li_count
STRING       ls_status_assigned_method_desc, ls_checklist_step_type_desc

SELECT Count(*)
INTO   :li_count
FROM   Checklist_Step_Type_Status_Xref a
JOIN   Checklist_Step_Status           b ON a.checklist_step_status_code = b.checklist_step_status_code
WHERE  a.module_code                 = :as_module_code
AND    a.checklist_type_code         = :as_checklist_type_code
AND    a.checklist_step_type_code    = :as_checklist_step_type_code
AND    a.checklist_step_status_code  = :as_checklist_step_status_code
AND    b.status_assigned_method_code = :as_status_assigned_method_code
AND    a.active_flag                 = 'Y'
AND    b.active_flag                 = 'Y'
USING SQLCA;
SQLCA.nf_handle_error('n_checklist','embedded SQL: SELECT Count(*) FROM Checklist_Step_Type_Status_Xref...','nf_checklist_step_type_status_xref')

IF li_count = 0 THEN
	IF as_status_assigned_method_code = 'A' THEN
		ls_status_assigned_method_desc = 'automatic'
	ELSEIF as_status_assigned_method_code = 'M' THEN
		ls_status_assigned_method_desc = 'manual'
	ELSE
		ls_status_assigned_method_desc = 'unknown'
	END IF
	
	SELECT checklist_step_type_desc
	INTO   :ls_checklist_step_type_desc
	FROM   Checklist_Step_Type
	WHERE  checklist_step_type_code = :as_checklist_step_type_code
	USING SQLCA;
	SQLCA.nf_handle_error('u_checklist','select checklist_step_type_desc from Checklist_Step_Type...','dw_checklist.itemchanged')
	
	
	Error.Text = 'The attempt to change the step status on checklist_no '+String(al_checklist_no)+' has failed. The following combination is not valid/active:' &
	+ '~r~n~t Checklist Type: '+ as_checklist_type_code &
	+ '~r~n~t Checklist Step: '+ ls_checklist_step_type_desc &
	+ '~r~n~t Checklist Step Status: '+as_checklist_step_status_code &
	+ '~r~n~t Status Assigned Method: '+ls_status_assigned_method_desc+'. '
	Error.WindowMenu="cmwb"
	Error.Object="n_checklist.nf_checklist_step_type_status_xref"
	SignalError()
END IF




RETURN li_count
end function

public function integer nf_checklist_type_status_xref (string as_module_code, string as_checklist_type_code, string as_checklist_status_code, string as_status_assigned_method_code, long al_checklist_no);INTEGER      li_count
STRING       ls_status_assigned_method_desc

SELECT Count(*)
INTO   :li_count
FROM   Checklist_Type_Status_Xref a
JOIN   Checklist_Status           b ON a.checklist_status_code = b.checklist_status_code
WHERE  a.module_code                 = :as_module_code
AND    a.checklist_type_code         = :as_checklist_type_code
AND    a.checklist_status_code       = :as_checklist_status_code
AND    b.status_assigned_method_code = :as_status_assigned_method_code
AND    a.active_flag                 = 'Y'
AND    b.active_flag                 = 'Y'
USING SQLCA;

SQLCA.nf_handle_error('n_checklist','embedded SQL: SELECT Count(*) FROM Checklist_Type_Status_Xref...','nf_checklist_type_status_xref')

IF li_count = 0 THEN
	IF as_status_assigned_method_code = 'A' THEN
		ls_status_assigned_method_desc = 'automatic'
	ELSEIF as_status_assigned_method_code = 'M' THEN
		ls_status_assigned_method_desc = 'manual'
	ELSE
		ls_status_assigned_method_desc = 'unknown'
	END IF
	
	Error.Text = 'The attempt to change the status on checklist_no '+String(al_checklist_no)+' has failed. The following combination is not valid/active:' &
	+ '~r~n~t Checklist Type: '+ as_checklist_type_code &
	+ '~r~n~t Checklist Status: '+as_checklist_status_code &
	+ '~r~n~t Status Assigned Method: '+ls_status_assigned_method_desc+'. '
	Error.WindowMenu="cmwb"
	Error.Object="n_checklist.nf_checklist_type_status_xref"
	SignalError()
END IF

RETURN li_count
end function

public function integer nf_check_creation_business_rules (string as_module_code, string as_checklist_type_code, string as_checklist_subscriber_type_code);INTEGER  li_rtn
LONG     ll_count

/*

validate the business rules below

1.10	The checklist type must be applicable for the initiating module.

1.180	The checklist type of a new checklist must be active.

1.190	The module that is initiating a new checklist must be active.

5.10	The checklist subscriber type of a new checklist must be active. 

*/


SELECT count(*)
INTO   :ll_count
FROM   Module_Checklist_Type_Xref
WHERE  module_code = :as_module_code
AND    checklist_type_code = :as_checklist_type_code
AND    active_flag = 'Y'
USING itr_trans_object;

IF ll_count < 1 THEN
	MessageBox('Common Checklist BR 1.10','The checklist type must be applicable for the initiating module.',StopSign!)
	RETURN -1
END IF


SELECT count(*)
INTO   :ll_count
FROM   Checklist_Type
WHERE  checklist_type_code = :as_checklist_type_code
AND    active_flag = 'Y'
USING itr_trans_object;

IF ll_count < 1 THEN
	MessageBox('Common Checklist BR 1.180','The checklist type of a new checklist must be active.',StopSign!)
	RETURN -1
END IF


SELECT count(*)
INTO   :ll_count
FROM   Module
WHERE  module_code = :as_module_code
AND    active_flag = 'Y'
USING itr_trans_object;

IF ll_count < 1 THEN
	MessageBox('Common Checklist BR 1.190','The module that is initiating a new checklist must be active.',StopSign!)
	RETURN -1
END IF


SELECT count(*)
INTO   :ll_count
FROM   Checklist_Subscriber_Type
WHERE  checklist_subscriber_type_code = :is_checklist_subscriber_type_code
AND    active_flag = 'Y'
USING itr_trans_object;

IF ll_count < 1 THEN
	MessageBox('Common Checklist BR 5.10','The checklist subscriber type of a new checklist must be active.',StopSign!)
	RETURN -1
END IF

RETURN 0
end function

public subroutine nf_set_checklist_subscriber_type (string as_checklist_subscriber_type_code);is_checklist_subscriber_type_code = as_checklist_subscriber_type_code
end subroutine

public function long nf_create_checklist (string as_module_code, string as_checklist_type_code, string as_checklist_status_code, string as_checklist_status_assignmt_meth_code, string as_checklist_step_status_code, string as_checklist_step_status_assmt_meth_code, long ar_related_checklist_no);INTEGER    li_version_no, li_rtn, li_counter, li_rows
LONG       ll_checklist_no, ll_count, ll_step, ll_checklist_step_type_code
STRING     ls_checklist_step_type_code, ls_status_assigned_method_desc, ls_checklist_step_type_desc
DATASTORE  lds_checklist_steps

li_rtn = nf_check_creation_business_rules(as_module_code,as_checklist_type_code,ls_status_assigned_method_desc)
IF li_rtn < 0 THEN RETURN li_rtn


ll_checklist_no = nf_get_last_checklist_no()

SELECT version_no
INTO   :li_version_no
FROM   Module_Checklist_Type_Xref
WHERE  checklist_type_code = :as_checklist_type_code
AND    module_code = :as_module_code
AND    active_flag = 'Y'
USING itr_trans_object;

itr_trans_object.nf_handle_error('n_checklist','nf_create_checklist','insert into CHECKLIST')

Select Count(*)
Into   :ll_count
From   Checklist_Step_Type_Xref
Where  module_code = :as_module_code
And    checklist_type_code = :as_checklist_type_code
And    active_flag = 'Y'
Using itr_trans_object;

itr_trans_object.nf_handle_error('n_checklist','nf_create_checklist','Select From Checklist_Step_Type_Xref')

IF ll_count < 1 THEN 
	Messagebox('Error','The module code and checklist type code are missing or not active. Please report to helpdesk.',Exclamation!)
	itr_trans_object.nf_rollback_transaction()
	
	Return -1
END IF	

// If nothing is passed in, this checklist is new and not related to another.
IF ar_related_checklist_no = 0 THEN
	ar_related_checklist_no = ll_checklist_no
END IF

li_rtn = nf_checklist_type_status_xref (as_module_code,as_checklist_type_code,as_checklist_status_code,as_checklist_status_assignmt_meth_code,ll_checklist_no)
IF li_rtn < 0 THEN RETURN li_rtn

Insert Into CHECKLIST
(	checklist_no, 
	checklist_type_code,
	checklist_status_code,
	module_code,
	concluded_date, 
	concluded_by_user_id,
	checklist_comment,
	related_checklist_no,
	version_no)
Values(
	:ll_checklist_no,
	:as_checklist_type_code,
	:as_checklist_status_code,
	:as_module_code,
	Null,
	'',
	'',
	:ar_related_checklist_no,
	:li_version_no)
Using itr_trans_object;

itr_trans_object.nf_handle_error('n_checklist','nf_create_checklist','insert into CHECKLIST')


lds_checklist_steps = Create datastore
lds_checklist_steps.SetTransObject(SQLCA)
lds_checklist_steps.DataObject = 'ds_checklist_steps'
li_rows = lds_checklist_steps.Retrieve(as_checklist_type_code)

FOR li_counter = 1 TO li_rows
	ls_checklist_step_type_code = lds_checklist_steps.GetItemString(li_counter,'checklist_step_type_code') 
	li_rtn = nf_checklist_step_type_status_xref (as_module_code,as_checklist_type_code,ls_checklist_step_type_code,as_checklist_step_status_code,as_checklist_status_assignmt_meth_code,ll_checklist_no)
	IF li_rtn < 0 THEN
		RETURN li_rtn
	END IF
NEXT

INSERT	CHECKLIST_STEP
(			checklist_no, 
			checklist_step_type_code, 
			checklist_step_no, 
             checklist_step_status_code,
			concluded_date, 
			concluded_by_user_id, 
			step_comment,
			cancelled_comment)
SELECT	:ll_checklist_no ,
			a.checklist_step_type_code ,
			b.checklist_step_no ,
             :as_checklist_step_status_code,			
			null ,
			'',
			'',
			''
FROM		Checklist_Step_Type a
JOIN		Checklist_Step_Type_Xref b ON a.checklist_step_type_code = b.checklist_step_type_code
WHERE   a.active_flag = 'Y'
And        b.active_flag = 'Y'
And        b.checklist_type_code = :as_checklist_type_code
USING     itr_trans_object;

itr_trans_object.nf_handle_error('n_checklist','nf_create_checklist','insert CHECKLIST_STEP')

Return ll_checklist_no
end function

public subroutine nf_set_datawindow (datawindow adw_dw[], n_transaction anv_transobj);/*************************************************************************
	Description:		This function registers datawindows and transaction	objects
*************************************************************************/


Int	li_cntr = 1

Do While li_cntr 				<=	UpperBound(adw_dw)
	idw_dw[li_cntr] 			=	adw_dw[li_cntr]
	idw_dw[li_cntr].SetTransObject(anv_transobj)
	li_cntr ++
Loop

itr_trans_object 	=	anv_transobj

end subroutine

public subroutine nf_set_transaction (ref n_transaction atr_trans_object);
itr_trans_object = atr_trans_object
iuo_checklist.itr_trans_object = atr_trans_object

end subroutine

on n_checklist.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_checklist.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

