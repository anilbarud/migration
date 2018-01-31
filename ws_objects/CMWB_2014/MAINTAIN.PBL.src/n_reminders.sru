$PBExportHeader$n_reminders.sru
forward
global type n_reminders from n_pdc
end type
end forward

global type n_reminders from n_pdc
end type
global n_reminders n_reminders

type variables

LONG il_claim_no, il_reminder_no
STRING is_status_code, is_status_type_code
Datastore	ids_reminder
Datastore	ids_reminder_up
end variables

forward prototypes
public function integer nf_retrieve (long al_claim_no)
public function integer nf_insert (long al_row)
public function integer nf_change_item (long al_datawindow)
public subroutine nf_init (u_dwa adw_dw[], n_transaction anv_transobj, w_ancestor awi_window_parent)
public function integer nf_update_reminder (long al_claim_no, string as_new_reminder_status_code, string as_reminder_status_code)
public function integer nf_create_auto_reminder (long al_claim_no, string as_reminder_type, string as_reminder_sub)
public function integer nf_retrieve_reminders (string as_reminder_status_code)
end prototypes

public function integer nf_retrieve (long al_claim_no);LONG             ll_rows
DATAWINDOWCHILD  ldwc_child
STRING           ls_status, ls_filter, ls_string

/*	dw 11 = reminder
*/
	
	// P10151-7
	ll_rows = idw_dw[14].Retrieve(al_claim_no)
	IF SQLCA.nf_handle_error('retrieve child','n_claims', 'nf_retrieve') < 0 THEN
		Return -1
	END IF

	
Return ll_rows
end function

public function integer nf_insert (long al_row);Return 0

end function

public function integer nf_change_item (long al_datawindow);Long     ll_current_row
Integer  li_rtn
	

ll_current_row = idw_dw[11].GetRow()
  
CHOOSE CASE idw_dw[11].GetColumnName()

			
	CASE 'reminder_status_code'
		//done in window

				
				
END CHOOSE

Return 0
end function

public subroutine nf_init (u_dwa adw_dw[], n_transaction anv_transobj, w_ancestor awi_window_parent);

end subroutine

public function integer nf_update_reminder (long al_claim_no, string as_new_reminder_status_code, string as_reminder_status_code);int		li_reminder_no
long	ll_count, ll_row
String	ls_reminder_stat
Datetime	ldtm_today

ldtm_today = Datetime(Date(f_server_datetime()))

ids_reminder_up.retrieve(al_claim_no)
SQLCA.nf_handle_error('Error','n_reminders','Retrieve on ids_reminder_up')

ll_count = ids_reminder_up.RowCount()

// Usually checking if it is a Planned status it should be changed to Completed or Cancelled
// depending on what values are passed into the function

FOR ll_row = 1 to ll_count
	ls_reminder_stat = ids_reminder_up.GetItemString(ll_row,"reminder_status_code") 
	IF ls_reminder_stat = as_reminder_status_code THEN 
		ids_reminder_up.SetItem(ll_row,"reminder_status_code",as_new_reminder_status_code)
		ids_reminder_up.SetItem(ll_row,"closed_date",ldtm_today)
	END IF	
NEXT 	
	
ll_count = ids_reminder_up.Update()
IF SQLCA.nf_handle_error("n_reminders","nf_update()","UPDATE CLAIM_REMINDER") < 0 THEN
	RETURN -1
END IF	


Return 0
end function

public function integer nf_create_auto_reminder (long al_claim_no, string as_reminder_type, string as_reminder_sub);//Insert automatic reminder
Long ll_max_reminder, ll_row
String	ls_reminder_status
Date	ldt_today, ldt_closed_date
datetime	ldtm_today, ldtm_closed_date


ids_reminder.retrieve(al_claim_no)
SQLCA.nf_handle_error("n_reminders", "nf_create_suto_reminder", "ids_reminder - Retrieve")

ls_reminder_status = 'P'

ldt_today = Date(f_server_datetime())
ldtm_today = Datetime(ldt_today)

SetNull(ldtm_closed_date)

IF as_reminder_sub = "FU" THEN //planned due date must be 1 month in the future
	
	Select TOP 1 (DateAdd(MONTH,1,getdate()))
	Into	:ldtm_today
	From sysobjects;

	SQLCA.nf_handle_error('n_reminders','nf_create_auto_reminder','Select  TOP 1 (DateAdd(MONTH,1,getdate()))')

	ldtm_today = Datetime(Date(ldtm_today))
	
END IF

//Get last reminder no

Select max(reminder_no)
Into    :ll_max_reminder
From  CLAIM_REMINDER
Where claim_no = :al_claim_no
Using SQLCA;

SQLCA.nf_handle_error('n_reminders','nf_create_auto_reminder','Select from CLAIM_REMINDER')

IF ISNull(ll_max_reminder) THEN
	ll_max_reminder = 0
END IF	

//Set up the datastore

ll_row = ids_reminder.InsertRow(0)
ids_reminder.SetItem(ll_row, 'claim_no',al_claim_no)
ids_reminder.SetItem(ll_row, 'reminder_no',ll_max_reminder+1)
ids_reminder.SetItem(ll_row, 'reminder_type_code',as_reminder_type)
ids_reminder.SetItem(ll_row, 'reminder_sub_type_code',as_reminder_sub)
ids_reminder.SetItem(ll_row, 'reminder_status_code',ls_reminder_status)
ids_reminder.SetItem(ll_row, 'due_date',ldtm_today)
ids_reminder.SetItem(ll_row, 'closed_date',ldtm_closed_date)
ids_reminder.SetItem(ll_row, 'reminder_comment','')


ids_reminder.Update()
IF SQLCA.nf_handle_error("n_reminders","nf_create_auto_reminder()","UPDATE CLAIM_REMINDER") < 0 THEN
	RETURN -1
END IF	



Return 0
end function

public function integer nf_retrieve_reminders (string as_reminder_status_code);



Return 0
end function

on n_reminders.create
call super::create
end on

on n_reminders.destroy
call super::destroy
end on

event constructor;call super::constructor;ids_reminder = create datastore
ids_reminder.dataobject = 'ds_claim_reminder'
ids_reminder.settransobject(sqlca)

ids_reminder_up = create datastore
ids_reminder_up.dataobject = 'ds_claim_reminder_update'
ids_reminder_up.settransobject(sqlca)

end event

