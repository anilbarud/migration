$PBExportHeader$n_action_item.sru
forward
global type n_action_item from n_pdc
end type
end forward

global type n_action_item from n_pdc
end type
global n_action_item n_action_item

type variables
LONG						il_claim_no, il_opening_no
Boolean						ib_claim_case_managed
DateTime					idt_default_planned_date, idt_today
private DateTime		idt_disablement_date

//Datawindow constants
CONSTANT	INTEGER		ACTION_ITEM 		= 1
CONSTANT	INTEGER		PROGRESS_NOTE 	= 2

//Catid lookup constants
CONSTANT INTEGER		DIAGNOSIS_CHANGE = 1

DatawindowChild		idwc_type
DatawindowChild		idwc_sub_type
DatawindowChild		idwc_specific
DatawindowCHild		idwc_status
DatawindowCHild		idwc_responsible
DatawindowCHild		idwc_reset_reason
DatawindowChild		idwc_event_type
DatawindowChild		idwc_event_specific
DatawindowChild		idwc_reset_week_no

//flags that will determine processing and business rules
Boolean					ib_medical_management_item
Boolean					ib_rescheduled_allowed
Boolean					ib_reset_allowed
Boolean					ib_progress_note_required
Boolean					ib_progress_note_allowed
Boolean					ib_comment_required
Boolean					ib_success_code_required

//Datastore used to create the initial action items as well as the reset action item.
Private u_ds			ids_action_item
Private u_ds			ids_related_action_items
Private u_ds			ids_rehab_task_progress_note
Private u_ds			ids_progress_note

//Holds the task_no assigned to the action item that was created 
//by resetting another action item.
LONG						il_new_reset_task_no
n_working_folders		inv_working_folders


end variables

forward prototypes
public function long nf_get_next_identifier ()
public function integer nf_calculate_week (datetime adt_status_date)
public subroutine nf_load_bus_rule_flags ()
public function integer nf_set_unused_fields ()
public function integer nf_set_note_comment ()
public function datetime nf_calculate_status_date (integer ai_week)
public function integer nf_set_claim_no (long al_claim_no)
public function integer nf_set_visible_expressions ()
public function integer nf_get_max_reset_weeks (ref string as_healing_desc)
public subroutine nf_filter (string as_column, string as_filter, integer ai_datawindow_integer, boolean ab_auto_select)
public subroutine nf_filter (string as_column, string as_filter, integer ai_datawindow_integer)
public function long nf_get_default_weeks (string as_task_sub_type_code)
public function integer nf_set_computed_date ()
public function datetime nf_calculate_status_date (integer ai_week, datetime adt_from_date)
public function datetime nf_get_disablement_date (long al_opening_no)
public function integer nf_get_opening_no (boolean ab_active)
public function integer nf_insert (long al_row)
public function integer nf_set_defaults ()
public function integer nf_item_changed (long al_row, dwobject a_dwo, string as_data)
public function integer nf_create_note_action_item_link (long al_task_no, long al_event_no)
private function boolean nf_is_initial_item (long al_opening_no)
public function integer nf_insert_note ()
public function integer nf_validate_insert_bus_rules ()
public function long nf_set_identifiers ()
public function long nf_get_next_event_no ()
public function long nf_get_routing_catid (integer ai_process)
public subroutine nf_filter_child (string as_column)
public function long nf_cancel_related_action_items (long al_opening_no, long al_related_task_no, boolean ab_allow_user_selection)
private function integer nf_retrieve_related_action_items (long al_opening_no, long al_related_task_no, boolean ab_outstanding_only)
public function boolean nf_related_action_items_exist (long al_opening_no, long al_related_task_no, boolean ab_outstanding_only)
public function long nf_reschedule_related_action_items (long al_opening_no, long al_related_task_no, long al_days_diff, boolean ab_allow_user_selection)
public function datetime nf_get_min_outstanding_planned_date (long al_opening_no, long al_related_task_no)
public function integer nf_check_mandatory ()
public function integer nf_set_status_date (datetime adt_computed_date)
public function integer nf_create_initial_action_items (long al_opening_no, datetime adt_disablement_date, string as_responsible_user_id)
public function integer nf_update ()
public function integer nf_item_change_accepted (long al_row, string as_column_name)
public subroutine nf_load_child_datawindows ()
public function integer nf_validate_reset ()
public subroutine nf_set_protect_expressions ()
public function integer nf_protect_progress_comment ()
public function integer nf_reassign_related_action_items (long al_opening_no, long al_new_opening_no)
public function integer nf_check_bus_rule ()
public function integer nf_create_action_item (string as_sub_type_code, string as_specific_code, string as_status_code, datetime adt_status_date, string as_responsible_user_id, string as_comment, string as_rehab_program_code, string as_rehab_service_code)
end prototypes

public function long nf_get_next_identifier ();LONG			ll_next_task_no

If il_claim_no = 0 Then SignalError(-666,'Invalid claim number when getting next indentifier')



SELECT IsNull(Max(task_no),0) into :ll_next_task_no
FROM REHAB_TASK
WHERE claim_no = :il_claim_no
USING SQLCA;

SQLCA.nf_handle_error('w_rehab_sheet','n_action_item','nf_get_next_identifier')
ll_next_task_no += 1

RETURN ll_next_task_no 
end function

public function integer nf_calculate_week (datetime adt_status_date);INTEGER			li_week
LONG			ll_opening_no
DateTime		ldt_disablement_date

If idw_dw[ACTION_ITEM].RowCount() > 0 Then
	ll_opening_no = idw_dw[ACTION_ITEM].GetItemNumber(1,'opening_no')
	
	ldt_disablement_date = nf_get_disablement_date(ll_opening_no)
	
	li_week = DaysAfter(Date(ldt_disablement_date), Date(adt_status_date))/7 + 1
End if

RETURN li_week
end function

public subroutine nf_load_bus_rule_flags ();LONG		ll_row

//Make sure you load these flags each time they are used as they are not kept up
//to date in the itemchanged or any other events.

//Reset all the flags first
ib_medical_management_item = False
ib_rescheduled_allowed = False
ib_reset_allowed = False
ib_progress_note_required = False
ib_progress_note_allowed = False
ib_comment_required = False
ib_success_code_required = False

//Task flags
If idwc_type.GetItemString(idwc_type.GetRow(),'success_code_required') = 'Y' Then
	ib_success_code_required = True
End if

//Task specific flags

ll_row = idwc_specific.GetRow()
If idwc_specific.GetItemString(ll_row,'comment_required_flag') = 'Y' Then
	ib_comment_required = True
End if	

If idwc_specific.GetItemString(ll_row,'medical_management_flag') = 'Y' Then
	ib_medical_management_item = True
End if

IF idwc_specific.GetItemString(ll_row,'reschedule_allowed_flag') = 'Y' Then
	ib_rescheduled_allowed = True
End if

IF idwc_specific.GetItemString(ll_row,'reset_allowed_flag') = 'Y' Then
	ib_reset_allowed = True
End if

IF idwc_specific.GetItemString(ll_row,'progress_note_required_flag') = 'Y' &
	and idw_dw[ACTION_ITEM].GetITemString(1,'task_status_code') <> '01' Then
	ib_progress_note_required = True
End if

//If the progress note allowed flag is 'Y' and the status is not 'planned'
//and this is a new record or the completion date is equal to todays date
//the allow the addition of progress notes.
IF idwc_specific.GetItemString(ll_row,'progress_note_allowed_flag') = 'Y' &
	and idw_dw[ACTION_ITEM].GetITemString(1,'task_status_code') <> '01' &
	and (IsNull(idw_dw[ACTION_ITEM].GetItemDateTime(1,'actual_completion_date')) &
		OR Date(idw_dw[ACTION_ITEM].GetItemDateTime(1,'actual_completion_date')) = Date(idt_today ))Then
	ib_progress_note_allowed = True
End if


end subroutine

public function integer nf_set_unused_fields ();/*	Accept database defaults for the following columns
		provider_no		
		provider_type_code
*/
DateTime			ldt_actual_completion_date

//If an action item is inserted and the status was set to
IF IsNull(idw_dw[ACTION_ITEM].GetItemDateTime(1,'planned_start_date')) Then
	ldt_actual_completion_date = idw_dw[ACTION_ITEM].GetItemDateTime(1,'actual_completion_date')
	idw_dw[ACTION_ITEM].SetItem(1,'planned_start_date',date(ldt_actual_completion_date))
	idw_dw[ACTION_ITEM].SetItem(1,'planned_completion_date',ldt_actual_completion_date)
End if

/*	The following fields are unused */
If idw_dw[ACTION_ITEM].GetItemStatus(1,0,Primary!) = NewModified! Then
	If idw_dw[1].SetItem(1,'task_success_code','I')	= -1 Then SignalError(-666,'Error setting task_success_code')	//Inaplicable
End if

RETURN 1
end function

public function integer nf_set_note_comment ();STRING			ls_combined_comment

IF idw_dw[PROGRESS_NOTE].GetItemStatus(1,'combined_comment',Primary!) = DataModified! Then
	ls_combined_comment = f_clean_string_4(idw_dw[PROGRESS_NOTE].GetItemString(1,'combined_comment'))
	
	idw_dw[PROGRESS_NOTE].SetItem(1,'event_comment', TRIM(ls_combined_comment))
End if

return 1

		


end function

public function datetime nf_calculate_status_date (integer ai_week);DateTIme			ldt_disablement_date, ldt_status_date
LONG				ll_opening_no

ll_opening_no = IDW_DW[ACTION_ITEM].GetITemNumber(1,'opening_no')
If IsNull(ll_opening_no) or ll_opening_no = 0 Then SignalError(-666,'Unable to calculate the status date, missing opening number.')

ldt_disablement_date = nf_get_disablement_date(ll_opening_no)

ldt_status_date = nf_calculate_status_date(ai_week,ldt_disablement_date)


RETURN ldt_status_date 



end function

public function integer nf_set_claim_no (long al_claim_no);string			ls_case_managed_flag


If al_claim_no > 0 THen
	il_claim_no = al_claim_no
	
	Select case_managed_flag into :ls_case_managed_flag
	FROM CLAIM
	WHERE claim_no = :il_claim_no
	USING SQLCA;
	
	SQLCA.nf_handle_error('n_action_item','nf_set_claim_no','Select case_managed_flag into :ls_case_managed_flag')
	
	If ls_case_managed_flag = 'Y' Then
		ib_claim_case_managed = True
	Else
		ib_claim_case_managed = False
	End if
	
	inv_working_folders.nf_set_claim_no(il_claim_no)
else
	SignalError(-666,'Missing claim number')
ENd if

return 1
end function

public function integer nf_set_visible_expressions ();STRING			ls_modify[]
INTEGER			li_x
STRING			ls_error = 'Error setting visible expression.'

//The reset reason can be changed after it has been saved only on the day it was saved
//If the reason is changed, we don't want to create another new action item.  Therefore
//the reset date and reset week columns will remain invisible.

//The use will be warned that changing the reset reason could affect the planned
//date of the action item that was created.  It will be up to them to verify
//that it is still valid and make any changes if necessary.
ls_modify[1] = "reset_date.Visible='0~tIf(reset_reason_code.original <> ~~'~~',0,if( reset_reason_code <> ~~'~~' and (reset_reason_code <> reset_reason_code.original),1,0))'"
ls_modify[2] = "t_reset_date.Visible='0~tIf(reset_reason_code.original <> ~~'~~',0,if( reset_reason_code <> ~~'~~' and (reset_reason_code <> reset_reason_code.original),1,0))'"

ls_modify[3] = "reset_week_no_editmask.Visible='0~tIf(reset_reason_code.original <> ~~'~~',0,if(reset_reason_code <> ~~'~~' and (reset_reason_code <> reset_reason_code.original),If(task_sub_type_code = ~~'151~~' and reset_reason_code = ~~'06~~',0,1),0))'"
ls_modify[4] = "t_reset_week_no_editmask.Visible='0~tIf(reset_reason_code.original <> ~~'~~',0,if(reset_reason_code <> ~~'~~' and (reset_reason_code <> reset_reason_code.original),If(task_sub_type_code = ~~'151~~' and reset_reason_code = ~~'06~~',0,1),0))'"

ls_modify[5] = "reset_week_no_dddw.Visible='0~tIf(reset_reason_code.original <> ~~'~~',0,if(reset_reason_code <> ~~'~~' and (reset_reason_code <> reset_reason_code.original),If(task_sub_type_code = ~~'151~~' and reset_reason_code = ~~'06~~',1,0),0))'"
ls_modify[6] = "t_reset_week_no_dddw.Visible='0~tIf(reset_reason_code.original <> ~~'~~',0,if(reset_reason_code <> ~~'~~' and (reset_reason_code <> reset_reason_code.original),If(task_sub_type_code = ~~'151~~' and reset_reason_code = ~~'06~~',1,0),0))'"

ls_modify[7] = "reset_reason_code.Visible='0~tif(task_status_code = ~~'05~~',1,0)'"
ls_modify[8] = "reset_reason_code_t.Visible='0~tif(task_status_code = ~~'05~~',1,0)'"

For li_x = 1 to UpperBound(ls_modify)
	ls_error = idw_dw[ACTION_ITEM].Modify(ls_modify[li_x])
	If ls_error <> '' Then SignalError(-666,ls_error)
Next

return 1
end function

public function integer nf_get_max_reset_weeks (ref string as_healing_desc);STRING		ls_task_sub_type_code
STRING		ls_reset_reason_code
LONG			ll_max_reset_weeks 

ls_task_sub_type_code = idw_dw[ACTION_ITEM].GetItemString(1,'task_sub_type_code')
ls_reset_reason_code  = idw_dw[ACTION_ITEM].GetItemString(1,'reset_reason_code')


CHOOSE CASE ls_task_sub_type_code 
	CASE '151'	//Timely Intervention
		If ls_reset_reason_code <> '06' Then
			
			Select weeks_from_disablement,expected_healing_desc
					into :ll_max_reset_weeks, :as_healing_desc
			FROM Expected_Healing_Guideline a,
					ACCIDENT b,
					Diagnosis_Healing_Combination c
			WHERE claim_no = :il_claim_no
					and b.nature_of_injury_code = c.nature_of_injury_code	
					and c.expected_healing_code = a.expected_healing_code
					and c.active_flag = 'Y'
					and a.active_flag = 'Y'
			USING SQLCA;
			SQLCA.nf_handle_error('n_action_item','nf_get_reset_max_weeks','Select weeks_from_disablement into :ll_max_reset_weeks')
			
			If ll_max_reset_weeks = 0 Then
				
				Select weeks_from_disablement,expected_healing_desc 
					into :ll_max_reset_weeks,:as_healing_desc
				FROM Expected_Healing_Guideline
				WHERE active_flag = 'Y'
						and default_flag = 'Y'
				USING SQLCA;
			
				SQLCA.nf_handle_error('n_action_item','nf_get_reset_max_weeks','Select weeks_from_disablement into :ll_max_reset_weeks')
								
			End if
		else
			SignalError(-666,'Timely Intervention/Diagnosis Change must be choosen from the list so ther is no reason to check the max reset numbe of weeks.')
		End if

	CASE '150' //Confirm Diagnosis
		
		Select max_reset_number_weeks into :ll_max_reset_weeks
		FROM Confirm_Diagnosis_Guideline
		WHERE confirm_diagnosis_code = 'CD'
				and active_flag = 'Y'
		USING SQLCA;
			
		SQLCA.nf_handle_error('n_action_item','nf_get_reset_max_weeks','Select max_reset_number_weeks into :ll_max_reset_weeks')
		
	CASE '152' //Work REstriction
		
		Select max_reset_number_weeks into :ll_max_reset_weeks
		FROM Work_Restriction_Guideline
		WHERE work_restriction_code = 'DR'
				and active_flag = 'Y'
		USING SQLCA;
			
		SQLCA.nf_handle_error('n_action_item','nf_get_reset_max_weeks','Select max_reset_number_weeks into :ll_max_reset_weeks')
				
		
END CHOOSE		

If ll_max_reset_weeks = 0 Then SignalError(-666,'Error getting max reset number of weeks')

RETURN ll_max_reset_weeks 

end function

public subroutine nf_filter (string as_column, string as_filter, integer ai_datawindow_integer, boolean ab_auto_select);/*	ab_auto_select	Default (False) Set to True if when the drop down contains
		only 1 item and you want to default the column value to to item.  Should
		only be done for columns that are mandatory.
*/


DataWindowChild			ldwc_child
STRING						ls_code
STRING						ls_dddw_code
LONG							ll_rows

idw_dw[ai_datawindow_integer].GetChild(as_column,ldwc_child)
ll_rows = ldwc_child.RowCount()

IF ldwc_child.SetFilter(as_filter) = -1 Then 
	SignalError(-666,"Error setting filter for column '" + as_column + "'~r~n" &
							+ "Filter: " + as_filter)
End if
IF ldwc_child.Filter()  = -1 Then SignalError(-666,"Error filtering")
ldwc_child.Sort()

ls_code = idw_dw[ai_datawindow_integer].GetItemString(1,as_column)


If ls_code = '' or IsNull(ls_code) Then
	If ldwc_child.RowCount() = 1 THen
		If ab_auto_select = True THen
			If Left(ldwc_child.describe(as_column + ".coltype"),4) = 'char' Then
				ls_dddw_code = Ldwc_child.GetItemString(1,as_column)
				idw_dw[ai_datawindow_integer].SetColumn(as_column)
				idw_dw[ai_datawindow_integer].SetText(ls_dddw_code)
				idw_dw[ai_datawindow_integer].AcceptText()
			End if
		End if
	End if	
	
//If the data value in the column no longer matches an entry in the list then set it to be empty
elseIf ldwc_child.Find( as_column + " ='" + ls_code + "'",1,ldwc_child.RowCount()) = 0 Then
	idw_dw[ai_datawindow_integer].SetColumn(as_column)
	idw_dw[ai_datawindow_integer].SetText('')
	idw_dw[ai_datawindow_integer].AcceptText()
End if



end subroutine

public subroutine nf_filter (string as_column, string as_filter, integer ai_datawindow_integer);nf_filter(as_column,as_filter,ai_datawindow_integer,False)

end subroutine

public function long nf_get_default_weeks (string as_task_sub_type_code);/*	This function returns the default weeks from disablement
	for the nature of injury associated with the claim.
		
	If there is not specific weeks from disablement for the 
	particular nature of injury, then a default weeks is used.
*/
LONG		ll_weeks_from_disablement
STRING		ls_task_sub_type_code

//Verify that the claim no has been set before trying to use it
If il_claim_no = 0 Then SignalError(-666,'Claim number has not been set')

ls_task_sub_type_code = as_task_sub_type_code

CHOOSE CASE ls_task_sub_type_code 
	CASE '151' 

		//Get weeks from disablement that is associated with the 
		//ACCIDENTS nature_of_injury
		SELECT 	weeks_from_disablement 
		INTO 		:ll_weeks_from_disablement
		FROM 		Diagnosis_Disability_Combo a, Disability_Duration_Guideline b, 	ACCIDENT c
		WHERE 	a.disability_duration_code 	= b.disability_duration_code
		AND 			a.nature_of_injury_code 	= c.nature_of_injury_code
		AND 			claim_no 							= :il_claim_no
		AND 			a.active_flag 						= 'Y'
		AND 			b.active_flag 						= 'Y'
		USING 		SQLCA;
		SQLCA.nf_handle_error('n_action_item','nf_get_default_weeks()','SELECT weeks_from_disablement 1')
		
		IF ll_weeks_from_disablement = 0 THEN 
			
			//If there is no Guidline for this particular nature_of_injury
			//then get the  default weeks
			SELECT weeks_from_disablement 
			INTO 	:ll_weeks_from_disablement
			FROM 	Disability_Duration_Guideline 
			WHERE default_flag 		= 'Y'
			AND 		active_flag 		= 'Y'
			USING 	SQLCA;
			SQLCA.nf_handle_error('n_action_item','nf_get_default_weeks()','SELECT weeks_from_disablement 2')
		END IF
		
	CASE '150'
		
		SELECT weeks_from_disablement 
		INTO 	:ll_weeks_from_disablement
		FROM	Confirm_Diagnosis_Guideline
		WHERE confirm_diagnosis_code 	= 'CD'
		AND 		active_flag 					= 'Y'
		USING 	SQLCA;
		SQLCA.nf_handle_error('n_action_item','nf_get_default_weeks()','SELECT weeks_from_disablement into :ll_weeks_from_disablement 3')

		IF ll_weeks_from_disablement = 0 THEN SignalError(-666,'Error getting weeks_from_disablement')

	CASE '152'
		
		SELECT weeks_from_disablement 
		INTO 	:ll_weeks_from_disablement
		FROM	Work_Restriction_Guideline
		WHERE work_restriction_code 	= 'DR'
		AND 		active_flag 					= 'Y'
		USING 	SQLCA;
		SQLCA.nf_handle_error('n_action_item','nf_get_default_weeks()','SELECT weeks_from_disablement into :ll_weeks_from_disablement 4')

		IF ll_weeks_from_disablement = 0 THEN SignalError(-666,'Error getting weeks_from_disablement')

END CHOOSE

RETURN ll_weeks_from_disablement
end function

public function integer nf_set_computed_date ();STRING			ls_task_status_code, ls_task_sub_type_code, ls_date_type
DATETIME		ldt_planned_start_date,	ldt_actual_completion_date, ldt_default_planned_date
INTEGER		li_week


/*
task_status_code task_status_desc                         action_item_entry_flag
---------------- ---------------------------------------- ----------------------
01               planned                                  Y
02               in progress                              N
03               cancelled                                Y
04               closed                                   Y
05               reset                                    Y
*/


ls_task_status_code 			= idw_dw[ACTION_ITEM].GetItemString(1,'task_status_code')
ls_task_sub_type_code 		= idw_dw[ACTION_ITEM].GetItemString(1,'task_sub_type_code')

CHOOSE CASE  ls_task_status_code 
	CASE '01'//planned
		/**********************************************************************/		
		/*	If the status_code is being changed to planned then set the planned date.*/
			ldt_planned_start_date = idw_dw[ACTION_ITEM].GetItemDateTime(1,'planned_start_date')
			//If no planned date exists, set the planned date to the default date
			IF IsNull(ldt_planned_start_date ) THEN
				//If the sub_type_code = "follow-up"
				IF ls_task_sub_type_code = '153' THEN
					//Set it back to NULL because the is the default value for follow-up's
					SetNull(ldt_default_planned_date)
				ELSE
					//If it's not a follow-up then get the default planned date 
					//from the guidlines in the database
					li_week 							= nf_get_default_weeks(ls_task_sub_type_code)
					ldt_default_planned_date 	= nf_calculate_status_date(li_week)
					
					//IF the planned date is calculated to be earlier than today then set the date to null 
					IF ldt_default_planned_date < idt_today THEN
						SetNull(ldt_default_planned_date)
						li_week = 0
					END IF
				END IF
				idw_dw[ACTION_ITEM].SetItem(1,'computed_date',ldt_default_planned_date)
				idw_dw[ACTION_ITEM].SetItem(1,'week_no',li_week)
			ELSE
				//If a planned date exists already, then set it back to it's original
				idw_dw[ACTION_ITEM].SetItem(1,'computed_date',ldt_planned_start_date)
			END IF
			
		/************************************************************************/
		ls_date_type = 'Planned:'
		
		
	CASE '03'//cancelled		
		
		//do nothing
		ldt_actual_completion_date = idw_dw[ACTION_ITEM].GetItemDateTime(1,'actual_completion_date')
		IF IsNull(ldt_actual_completion_date) THEN
			SetNull(ldt_default_planned_date)
			li_week = 0
			idw_dw[ACTION_ITEM].SetItem(1,'week_no',li_week)
			idw_dw[ACTION_ITEM].SetItem(1,'computed_date',ldt_actual_completion_date)
		ELSE
			idw_dw[ACTION_ITEM].SetItem(1,'computed_date',ldt_actual_completion_date)
			idw_dw[ACTION_ITEM].SetItemStatus(1,'computed_date',Primary!,NotModified!)
			
			li_week = nf_calculate_week(ldt_actual_completion_date)
			idw_dw[ACTION_ITEM].SetItem(1,'week_no',li_week)
			idw_dw[ACTION_ITEM].SetItemStatus(1,'week_no',Primary!,NotModified!)
		END IF
			
		ls_date_type = 'Cancelled:'
		
		
	CASE ELSE
		
		ldt_actual_completion_date = idw_dw[ACTION_ITEM].GetItemDateTime(1,'actual_completion_date')
		IF IsNull(ldt_actual_completion_date) THEN
			idw_dw[ACTION_ITEM].SetItem(1,'computed_date',idt_today)
			li_week = nf_calculate_week(idt_today)
			idw_dw[ACTION_ITEM].SetItem(1,'week_no',li_week)
		ELSE
			idw_dw[ACTION_ITEM].SetItem(1,'computed_date',ldt_actual_completion_date)
			idw_dw[ACTION_ITEM].SetItemStatus(1,'computed_date',Primary!,NotModified!)
			
			li_week = nf_calculate_week(ldt_actual_completion_date)
			idw_dw[ACTION_ITEM].SetItem(1,'week_no',li_week)
			idw_dw[ACTION_ITEM].SetItemStatus(1,'week_no',Primary!,NotModified!)
		END IF
			
		ls_date_type = 'Actual:'
END CHOOSE

idw_dw[ACTION_ITEM].object.t_date_type.text = ls_date_type

RETURN 1
end function

public function datetime nf_calculate_status_date (integer ai_week, datetime adt_from_date);DATETIME 		ldt_status_date


If ai_week > 0 Then 
	ldt_status_date = DateTime(RelativeDate(Date(adt_from_date),(ai_week - 1) * 7))
End if

return ldt_status_date




end function

public function datetime nf_get_disablement_date (long al_opening_no);DATETIME		ldt_benefit_start_date

//If the opening passed in doesn't match the opening that 
//we used to get the disablement date last time
//then go get the new one else pass back the old one

il_opening_no = al_opening_no

If al_opening_no = 0 Then
	SetNUll(ldt_benefit_start_date)
	RETURN ldt_benefit_start_date
END IF

SELECT benefit_start_date into:ldt_benefit_start_date
FROM OPENING
WHERE claim_no = :il_claim_no
	and opening_no = :al_opening_no
USING SQLCA;

SQLCA.nf_handle_error('n_action_item','nf_get_disablement_date','')

idt_disablement_date = ldt_benefit_start_date


RETURN idt_disablement_date
end function

public function integer nf_get_opening_no (boolean ab_active);INTEGER		li_opening_no


/*		Action items can only be associated with an OPENING
	that has an opening_type_code = 'RLOE' and a recurrence_type_code = 'R'
		These OPENINGS are considered active if they don't have
	a benefit_end_date.  THe are inactive if the have a benefit end date.
	
		When adding the initail 3 action items they can only be associated
	whith an Active Opening.  Other action items should be associated with the
	active opening is one exists or the latest inactive one is not.
*/


//Get the active opening to associate the action item with
SELECT opening_no into :li_opening_no 
FROM OPENING
WHERE claim_no = :il_claim_no
AND opening_type_code = 'RLOE'
	and recurrence_type_code = 'R'
	and benefit_end_date is null
USING SQLCA;

SQLCA.nf_handle_error('n_action_item','nf_get_opening_no','select opening_no')

/*	If we found an opening number return it reguardless of wether or not 
	there are initial action items already.
*/
IF li_opening_no > 0 or ab_active = True Then 	
	RETURN li_opening_no
End if



/*	If there is no active opening then get the opening number of
	the latest inactive
*/
SELECT opening_no into :li_opening_no
FROM OPENING
WHERE claim_no = :il_claim_no
	and opening_type_code = 'RLOE'
	and recurrence_type_code = 'R'
	and benefit_end_date = (SELECT max(benefit_end_date) 
									FROM OPENING 
									WHERE claim_no = :il_claim_no
										and opening_type_code = 'RLOE'
										and recurrence_type_code = 'R'
										and benefit_end_date is not null)
USING SQLCA;

SQLCA.nf_handle_error('n_action_item','nf_get_opening_no','select opening_no')

RETURN li_opening_no
end function

public function integer nf_insert (long al_row);LONG			ll_row

//Row is actually going to hold the related_task_no if there is one
//if there isn't then the value will be zero

ll_row = idw_dw[ACTION_ITEM].insertRow(0)
If al_row > 0 Then
	idw_dw[ACTION_ITEM].SetItem(ll_row,'related_task_no',al_row)
End if

nf_set_defaults()

nf_filter_child('')

return 1
end function

public function integer nf_set_defaults ();/*	In order for the itemchanged even to fire properly
	we must set the default values in the following manner.
	Followed by AcceptText()
*/
STRING			ls_string
LONG			ll_row,	ll_default_weeks,	ll_opening_no
DATETIME		ldt_default_planned_date,	ldt_disablement_date

//We have to get the opening number here because it is used in
//calculating the default planned date
ll_opening_no = nf_get_opening_no(False)
idw_dw[ACTION_ITEM].SetItem(1,'opening_no',ll_opening_no)

ldt_disablement_date = nf_get_disablement_date(ll_opening_no)
idw_dw[ACTION_ITEM].SetItem(1,'disablement_date',ldt_disablement_date )
idw_dw[ACTION_ITEM].AcceptText()

idw_dw[ACTION_ITEM].SetColumn('task_type_code')
idw_dw[ACTION_ITEM].SetText('AC')

idw_dw[ACTION_ITEM].SetColumn('task_sub_type_code')
idw_dw[ACTION_ITEM].SetText('153')
idw_dw[ACTION_ITEM].SetItem(1,'task_sub_type_code','153')

idw_dw[ACTION_ITEM].SetColumn('task_specific_code')
idw_dw[ACTION_ITEM].SetText('.')

idw_dw[ACTION_ITEM].SetColumn('task_status_code')
idw_dw[ACTION_ITEM].SetText('01')

/* new columns for ephysio */
idw_dw[ACTION_ITEM].SetItem(1,'rehab_service_code','S002')
idw_dw[ACTION_ITEM].SetItem(1,'rehab_program_code','')
idw_dw[ACTION_ITEM].SetItem(1,'expedited_service_flag','N')
idw_dw[ACTION_ITEM].SetItem(1,'auto_created_flag','N')

//We have to filter the responsible user here because...
//If the current record has a responsible user that is not valid we still include
//it in the drop down so it will display the persons name and not just there user id
//if we didn't filter the list when inserting a new record this invalid entry
//would still be available to choose.
nf_filter_child('responsible_user_id')

If idwc_responsible.Find("user_id = '" + vgst_user_profile.user_id + "'",1,idwc_responsible.RowCount()) > 0 Then
	idw_dw[ACTION_ITEM].SetColumn('responsible_user_id')
	idw_dw[ACTION_ITEM].SetText(vgst_user_profile.user_id)
End if

idw_dw[ACTION_ITEM].AcceptText()

return 1

end function

public function integer nf_item_changed (long al_row, dwobject a_dwo, string as_data);DateTime			ldt_disablement_date,	ldt_new_computed_date,	ldt_old_computed_date
INTEGER			li_old_week_no
LONG				ll_opening_no
STRING				ls_error,		ls_column
dwItemStatus	ldis_item_status 

//The filter is called here because it must excute the script after
//the item changed has finished and the new values are actually
//in the datawindow buffer
CHOOSE CASE a_dwo.name
	CASE 'computed_date','week_no'
		ll_opening_no = idw_dw[ACTION_ITEM].GetItemNumber(al_row,'opening_no')
		ldt_disablement_date = nf_get_disablement_date(ll_opening_no)
		ls_column = a_dwo.name
		
		li_old_week_no = idw_dw[ACTION_ITEM].GetITemNumber(1,'week_no')
		ldt_old_computed_date = idw_dw[ACTION_ITEM].GetItemDateTime(1,'computed_date')		
	
		If a_dwo.name = 'computed_date' Then
			ldt_new_computed_date = datetime(date(left(as_data,10)))
		elseif a_dwo.name = 'week_no' then
			li_old_week_no = idw_dw[ACTION_ITEM].GetITemNumber(1,'week_no')
			//Make sure the week number is within a valid range	
			IF integer(as_data) < 1 or integer(as_data) > 999 Then
				ls_error = 'Week number must more than zero and less than 999.'
			End if			
			
			If IsNull(as_data) or as_data = '' Then
				SetNull(ldt_new_computed_date)
			Else
				ldt_new_computed_date = nf_calculate_status_date(integer(as_data))
			End if
		End if
		
		//Verify that the new status date is on or after the disablement date
		If ldt_new_computed_date < ldt_disablement_date Then
			ls_error = 'The date must be on or after the disablement date'
		Elseif ldt_new_computed_date > DateTime(Date('2079/01/01')) THen
			ls_error = "The date must be less than '2079/01/01'."
		Elseif ldt_new_computed_date < idt_today THen
			ls_error = "The status date must be greater than or equal to todays date"
		End if
		
		If ls_error <> '' Then
			//Get the ItemStatus so we can set it back if there is an error
			ldis_item_status = idw_dw[ACTION_ITEM].GetItemStatus(1,ls_column,Primary!)

			If a_dwo.name = 'computed_date' Then
				idw_dw[ACTION_ITEM].SetItem(1,'computed_date',ldt_old_computed_date)
			else
				IF li_old_week_no = 0 THen
					//We have to manually set the li_old_week_no back to NULL
					//because the GetItemNubmer will return 0 even though
					//the value is really NULL
					SetNull(li_old_week_no)
				End if
				idw_dw[ACTION_ITEM].SetItem(1,'week_no',li_old_week_no)
			End if
			
			MessageBox('Date error',ls_error)
			
			idw_dw[ACTION_ITEM].SetColumn(string(a_dwo.name))
			RETURN 2
		End if
			
END CHOOSE	
		
return 0	
end function

public function integer nf_create_note_action_item_link (long al_task_no, long al_event_no);LONG			ll_row


If IsNull(al_task_no) or al_task_no = 0 THen
	SignalError(-666,"Error creating 'REHAB_TASK_PROGRESS_NOTE' record. Invalid task_no")
End if

If IsNull(al_event_no) or al_event_no = 0 THen
	SignalError(-666,"Error creating 'REHAB_TASK_PROGRESS_NOTE' record. Invalid event_no")
End if

If Not IsValid(ids_rehab_task_progress_note) Then
	ids_rehab_task_progress_note = CREATE u_ds
	ids_rehab_task_progress_note.dataobject = 'd_rehab_task_progress_note'
	ids_rehab_task_progress_note.SetTransobject(SQLCA)
End if

ll_row = ids_rehab_task_progress_note.InsertRow(0)

ids_rehab_task_progress_note.SetItem(ll_row,'claim_no',il_claim_no)
ids_rehab_task_progress_note.SetItem(ll_row,'task_no',al_task_no)
ids_rehab_task_progress_note.SetItem(ll_row,'event_no',al_event_no)


return 1
end function

private function boolean nf_is_initial_item (long al_opening_no);/*	This function returns True if the current item is an initial action item.
	This should be used on the claim maintenance screen to decided whether or
	not to create the initial three action items.  
	It should be used to determine whether or not additional action items can
	be created from the action item tab.  
*/
LONG			ll_count


If il_claim_no = 0 THen SignalError(-666,'Claim number missing in n_action_item')

SELECT count(*) into :ll_count
FROM REHAB_TASK
WHERE claim_no = :il_claim_no
	and opening_no = :al_opening_no
	and task_type_code = 'AC'
	and task_sub_type_code in('150','151','152')
USING SQLCA;

SQLCA.nf_handle_error('n_action_item','nf_initial_item','selct count(*)')

//If the count = 0 then we didn't find any of the initial action items
//already created.
IF ll_count = 0 THen
	RETURN TRUE
ELSE
	RETURN FALSE
END IF
end function

public function integer nf_insert_note ();LONG				ll_row

idw_dw[PROGRESS_NOTE].Reset()
ll_row = idw_dw[PROGRESS_NOTE].InsertRow(0)

If ll_row < 1 Then SignalError(-666,'Error inserting progress note')

//I'm setting the defaults here because I want to 

idw_dw[PROGRESS_NOTE].SetITem(ll_row,'event_date',idt_today )
idw_dw[PROGRESS_NOTE].SetItem(ll_row,'event_type_code','')
idw_dw[PROGRESS_NOTE].SetItem(ll_row,'event_specific_code','')

nf_filter_child('event_type_code')
nf_filter_child('event_specific_code')

return ll_row
end function

public function integer nf_validate_insert_bus_rules ();STRING		ls_task_type_code, ls_task_sub_type_code, ls_task_specific_code
LONG		ll_count

ls_task_type_code 			= idw_dw[ACTION_ITEM].GetItemString(1,'task_type_code')
ls_task_sub_type_code 		= idw_dw[ACTION_ITEM].GetItemString(1,'task_sub_type_code')
ls_task_specific_code 		= idw_dw[ACTION_ITEM].GetItemString(1,'task_specific_code')

//If it's a Follow-up then return 1 as there are no restriction for them.
IF ls_task_sub_type_code = '153' THEN RETURN 1

SELECT count(*) INTO :ll_count
FROM 	REHAB_TASK
WHERE claim_no 					= :il_claim_no
AND 		task_type_code 		= :ls_task_type_code
AND 		task_sub_type_code 	= :ls_task_sub_type_code
AND 		task_specific_code 	= :ls_task_specific_code
AND 		task_status_code 		= '01'
USING 	SQLCA;

SQLCA.nf_handle_error('n_action_item','nf_validate_inser_bus_rules','SELECT count(*) into :ll_count')

IF ll_count > 0 THEN
	MessageBox('Error','A planned action item of this type already exists.  You cannot add another until the first has been addressed. ~r~n' &
				+ 'Please cancel the add item or change the type.')
	RETURN -1
END IF 

RETURN 1


end function

public function long nf_set_identifiers ();LONG			ll_next_task_no
LONG			ll_next_event_no
INTEGER		li_x
LONG			ll_task_no

If UpperBound(idw_dw) = 2 Then
	If idw_dw[ACTION_ITEM].GetItemStatus(1,0,Primary!) = New! &
		or idw_dw[ACTION_ITEM].GetItemStatus(1,0,Primary!) = NewModified! Then
		
		ll_next_task_no = nf_get_next_identifier()
	
		idw_dw[ACTION_ITEM].SetItem(1,'claim_no',il_claim_no)
		idw_dw[ACTION_ITEM].SetItem(1,'task_no',ll_next_task_no)
	End if
	
	
	If idw_dw[PROGRESS_NOTE].GetItemStatus(1,0,Primary!) = New! &
		or idw_dw[PROGRESS_NOTE].GetItemStatus(1,0,Primary!) = NewModified! Then
		
		ll_next_event_no = nf_get_next_event_no()
	
		idw_dw[PROGRESS_NOTE].SetItem(1,'claim_no',il_claim_no)
		idw_dw[PROGRESS_NOTE].SetItem(1,'event_no',ll_next_event_no)
		
		ll_task_no = idw_dw[ACTION_ITEM].GetItemNumber(idw_dw[ACTION_ITEM].GetRow(),'task_no')
		
		//Creates the 'REHAB_TASK_PROGRESS_NOTE' record
		//that linkes the REHAB_TASK record to the CLAIM_EVENT
		nf_create_note_action_item_link(ll_task_no,ll_next_event_no)
	End if
End if

//Reset this variable
il_new_reset_task_no = 0
If ids_action_item.RowCount() > 0 Then
	ll_next_task_no = nf_get_next_identifier()
	For li_x = 1 to ids_action_item.RowCount()
		ids_action_item.SetItem(li_x,'task_no',ll_next_task_no )
		il_new_reset_task_no = ll_next_task_no
		ll_next_task_no ++
	Next
End if
	
RETURN 1
end function

public function long nf_get_next_event_no ();LONG			ll_max_event_no, ll_next_event_no

SELECT 	IsNull(max(event_no),0) INTO :ll_max_event_no
FROM 		CLAIM_EVENT
WHERE 	claim_no = :il_claim_no
USING 		SQLCA;
SQLCA.nf_handle_error('n_action_item', 'nf_get_next_event_no', 'SELECT max(event_no) INTO:ll_max_event_no')

ll_next_event_no = ll_max_event_no + 1

RETURN ll_next_event_no
end function

public function long nf_get_routing_catid (integer ai_process);LONG			ll_cat_id

If ai_process = DIAGNOSIS_CHANGE Then
	Select diagnosis_msg_routing_cat_id into :ll_cat_id
	FROM Diagnosis_Msg_Routing
	using IMAGETRANS;
	
	IMAGETRANS.nf_handle_error('n_action_item','nf_get_routing_catid','Select diagnosis_msg_routing_cat_id into :ll_cat_id')
	
	IF ll_cat_id = 0 or IsNull(ll_cat_id) Then
		ll_cat_id = -1
	End if
	
Else
	ll_cat_id = -1
End if

RETURN ll_cat_id
end function

public subroutine nf_filter_child (string as_column);String				ls_task_type_code
String				ls_task_sub_type_code
String				ls_specific_code
String				ls_original_task_status_code
String				ls_current_task_status_code
String				ls_event_type_code
String				ls_filter
Boolean				lb_new
LONG					ll_row,ll_rowcount
String				ls_reset_allowed_flag

If idw_dw[1].GetITemStatus(1,0,Primary!) = New! or idw_dw[1].GetItemStatus(1,0,Primary!) = NewModified! THen
	lb_new = True
End if

ls_task_type_code = idw_dw[ACTION_ITEM].GetItemString(1,'task_type_code')
ls_task_sub_type_code = idw_dw[ACTION_ITEM].GetItemString(1,'task_sub_type_code')
ls_specific_code = idw_dw[ACTION_ITEM].GetItemString(1,'task_specific_code')
ls_original_task_status_code = idw_dw[ACTION_ITEM].GetItemString(1,'task_status_code',Primary!,True) 
ls_current_task_status_code = idw_dw[ACTION_ITEM].GetItemString(1,'task_status_code')

IF IsNull(ls_task_type_code) Then ls_task_type_code  = ''
If IsNull(ls_task_sub_type_code) Then ls_task_sub_type_code = ''
If IsNull(ls_specific_code) Then ls_specific_code = ''

If as_column = '' or as_column = 'task_type_code' Then
	//Filter the task_type_code dropdown
	ls_filter = "active_flag = 'Y'"
	IF not lb_new then 
		ls_filter += " or task_type_code = '" + ls_task_type_code + "'"
	End if		
	THIS.nf_filter('task_type_code',ls_filter,ACTION_ITEM)
End if

//TASK SUB TYPE CODE
If as_column = '' or as_column = 'task_sub_type_code' Then	
	ls_filter = "(active_flag = 'Y'"
	ls_filter += " and task_type_code = '" + ls_task_type_code + "')"
	If not lb_new Then
		ls_filter += " or task_sub_type_code = '" + ls_task_sub_type_code + "'"
	End if		
	THIS.nf_filter('task_sub_type_code',ls_filter,ACTION_ITEM)
End if

//TASK_SPECIFIC CODE
If as_column = '' or as_column = 'task_specific_code' Then	
	ls_filter = "(active_flag = 'Y'"
	ls_filter += " and task_type_code = '" + ls_task_type_code + "'"
	ls_filter += " and task_sub_type_code = '" + ls_task_sub_type_code + "')"

	nf_filter('task_specific_code',ls_filter,ACTION_ITEM)
End if

//RESPONSIBLE USER ID
If as_column = '' or as_column = 'responsible_user_id' Then 	
	ls_filter = "(job_position_code = 'CASMGR' and active_flag = 'Y' and rehab_task_responsible_flag = 'Y')"
	If not lb_new THen 
		ls_filter += " or (user_id = '" + idw_dw[1].GetITemString(1,'responsible_user_id') + "')"
	End if
	nf_filter('responsible_user_id',ls_filter,ACTION_ITEM)
End if

//TASK STATUS CODE
If as_column = '' or as_column = 'task_status_code' Then
	ls_filter = ''
	
	//If the record is NEW, don't  show cancelled or reset.
	If idw_dw[ACTION_ITEM].GetItemStatus(1,0,Primary!) = NewModified!Then
		ls_filter = "task_status_code <> '03' and task_status_code <> '05'"
	else
	
		ll_rowcount = idwc_specific.rowcount()
		//Set the task_specific_code part of the filter
		ll_row = idwc_specific.Find("task_specific_code = '" + ls_specific_code + "'",1,ll_rowcount)
		If ll_row > 0 THen 
			ls_reset_allowed_flag = idwc_specific.GetItemString(ll_row,'reset_allowed_flag')
			
			If ls_reset_allowed_flag = 'N' Then
				ls_filter = "task_status_code <> '05'"
			End if
		elseIf ll_row < 0 Then
			SignalError(-666,'Error setting filter for status')
		End if
	End if
	
	nf_filter('task_status_code',ls_filter,ACTION_ITEM)
End if
	
If as_column = '' or as_column = 'reset_reason_code'  Then
	ls_filter = "task_type_code = '" + ls_task_type_code + "'"
	ls_filter += " and task_sub_type_code = '" + ls_task_sub_type_code + "'"
	ls_filter += " and task_specific_code = '" + ls_specific_code + "'"
	ls_filter += " and active_flag = 'Y'"
	
	nf_filter('reset_reason_code',ls_filter,ACTION_ITEM)
End if


//Make sure there is a progress note before we filter then drop down datawindows
If idw_dw[PROGRESS_NOTE].RowCount() = 1 Then
	
	ls_event_type_code = idw_dw[PROGRESS_NOTE].GetItemString(1,'event_type_code')

	If as_column = '' Then
		ls_filter = ''
		nf_filter('event_type_code',ls_filter,PROGRESS_NOTE)
		nf_filter('event_specific_code',ls_filter,PROGRESS_NOTE)
	End if
	
	If as_column = 'event_type_code' then
		ls_filter = "task_type_code = '" + ls_task_type_code + "'"
		ls_filter += " and task_sub_type_code = '" + ls_task_sub_type_code + "'"
		ls_filter += " and task_specific_code = '" + ls_specific_code + "'"
		ls_filter += " and task_status_code = '" + ls_current_task_status_code + "'"
		
		nf_filter('event_type_code',ls_filter,PROGRESS_NOTE,True)
	End if
			
	If as_column = 'event_specific_code' THen
		ls_filter = "task_type_code = '" + ls_task_type_code + "'"
		ls_filter += " and task_sub_type_code = '" + ls_task_sub_type_code + "'"
		ls_filter += " and task_specific_code = '" + ls_specific_code + "'"
		ls_filter += " and task_status_code = '" + ls_current_task_status_code + "'"
		ls_filter += " and event_type_code = '" + ls_event_type_code + "'"
		
		nf_filter('event_specific_code',ls_filter,PROGRESS_NOTE,True)
	END if
End if
		



end subroutine

public function long nf_cancel_related_action_items (long al_opening_no, long al_related_task_no, boolean ab_allow_user_selection);//RETURN	-1 if the action was cancelled
//RETURN  0 if no records were found to cancel
//RETURN  1 if records were found and the save should continue

/* 3.425	The actual start date and actual completed date must be set to the current date for all of the
                related Follow-up action items if the task is cancelled or completed (task status ‘03’ or ‘04’).
*/
/*
modified to 
3.425		The actual start date and actual completed date must be set to the current date for all of the 
				related Follow-up action items if the task is closed (task status  ‘04’).
*/

/*
3.340	If the task is 'Planned' or ‘Cancelled’,  the Actual Start & Actual Completion dates must not be entered (must be null).
*/

/*3.325 	If the task is ‘Cancelled’ and the success status is required, the task success status must be Not Yet Determined. 
success code is not required for action items.

task_success_code 	task_success_desc
-------------------------	    ------------------------------
I                 				INAPPLICABLE
N                 			NOT SUCCESSFUL
X                 			NOT YET DETERMINED
Y                 			SUCCCESSFUL

*/

INTEGER		li_rtn = 1, li_x,	ll_rows,	ll_items_canceled,	ll_row,	ll_next_event_no,	ll_task_no
STRING			ls_task_sub_type_code,	ls_task_specific_code, ls_event_type_code, ls_event_specific_code, ls_task_success_code
DATE			ldt_null

S_WINDOW_MESSAGE		lstr_message

setnull(ldt_null)

nf_retrieve_related_action_items(al_opening_no,al_related_task_no,True)

//The nf_retrieve_related_action_items should have been called first.
//if there were rows returned from that function then this function could be called
//else it shouldn't be called.  I made it an application error so the programmer
//calling this function would not forget to call nf_retrieve_related_action_items first.
IF ids_related_action_items.RowCount() = 0 THEN 	RETURN 0

IF ab_allow_user_selection THEN
	lstr_message.apo_PowerObjectParm[1] 	= ids_related_action_items
	lstr_message.as_stringparm[1] 				= 'The following action items are ready to be cancelled.  Deselect any to remain active.'
	lstr_message.as_stringparm[2] 				= 'Cancel related action items'

	OpenWithParm(w_cancel_action_items,lstr_message)
	//Check the return from the window to see what button the user pressed (continue, cancel)
	li_rtn = Message.DoubleParm		
END IF


IF li_rtn = 1 THEN
	//Change the status and fill in the proper date for all the records that
	//are flagged to be cancelled.
	ll_next_event_no = nf_get_next_event_no()
	For li_x = 1 To ids_related_action_items.RowCount()
		If ids_related_action_items.GetItemString(li_x,'selection_flag') = 'Y' Then
			ids_related_action_items.SetItem(li_x,'task_status_code','03')
			ids_related_action_items.SetItem(li_x,'actual_start_date',ldt_null)//3.425
			ids_related_action_items.SetItem(li_x,'actual_completion_date',ldt_null)	//3.425
			
			ls_task_sub_type_code 	= ids_related_action_items.GetItemString(li_x,'task_sub_type_code')
			ls_task_specific_code 	= ids_related_action_items.GetItemString(li_x,'task_specific_code')
			ll_task_no 						= ids_related_action_items.GetItemNumber(li_x,'task_no')
			
			//Create a progress note if they are allowed
			IF ids_related_action_items.GetItemString(li_x,'progress_note_required_flag') = 'Y' THen
				ll_row = ids_progress_note.InsertRow(0)
				
				SELECT event_type_code, event_specific_code
				INTO 	:ls_event_type_code , :ls_event_specific_code
				FROM 	Task_Event_Combination
				WHERE task_type_code 		= 'AC'
				AND 		task_sub_type_code 	= :ls_task_sub_type_code
				AND 		task_specific_code 	= :ls_task_specific_code 
				AND 		task_status_code 		= '03'
				AND 		active_flag 				= 'Y'
				USING 	SQLCA;
				SQLCA.nf_handle_error('n_action_item','nf_cancel_related_action_items','Select event_type_code , event_specific_code')
				
				IF SQLCA.SQLNRows = 0 Then SignalError(-666,'No event type codes found.')			
				
				ids_progress_note.SetItem(ll_row,'claim_no',il_claim_no)
				ids_progress_note.SetItem(ll_row,'event_no',ll_next_event_no)
				ids_progress_note.SetItem(ll_row,'event_date',idt_today)
				ids_progress_note.SetItem(ll_row,'event_type_code',ls_event_type_code)
				ids_progress_note.SetITem(ll_row,'event_specific_code',ls_event_specific_code)
				ids_progress_note.SetITem(ll_row,'event_comment','Cancelled because the related Opening was closed.')
				
				nf_create_note_action_item_link(ll_task_no ,ll_next_event_no)
				
				ll_next_event_no ++
			End if
			ll_items_canceled ++
		End if
	Next	
		
Else
	ll_items_canceled = li_rtn
End if

return ll_items_canceled
end function

private function integer nf_retrieve_related_action_items (long al_opening_no, long al_related_task_no, boolean ab_outstanding_only);LONG		ll_rows
STRING		ls_filter

//Remove the filter, this might be the second time in here and the
//filter will affect the ll_rows count
ids_related_action_items.SetFilter('')
ids_related_action_items.Filter()

ll_rows = ids_related_action_items.Retrieve(il_claim_no,al_opening_no,al_related_task_no)

SQLCA.nf_handle_error('n_action_item','nf_retrieve_related_action_items()',' ids_related_action_items.Retrieve(il_claim_no,al_opening_no,al_related_task_no)')

If ll_rows > 0 Then
	If ab_outstanding_only Then	
		ls_filter = "task_status_code = '01'"
		If ids_related_action_items.SetFilter(ls_filter) = -1 Then SignalError(-666,'Error Setting filter for related action items')
		If ids_related_action_items.Filter() = -1 Then SignalError(-666,'Error filtering')
		ll_rows = ids_related_action_items.RowCount()
	End if
End if

return ll_rows
end function

public function boolean nf_related_action_items_exist (long al_opening_no, long al_related_task_no, boolean ab_outstanding_only);STRING		ls_task_status_code 
LONG			ll_count


IF ab_outstanding_only then
	ls_task_status_code = '01'
End if


SELECT COUNT(*) INTO :ll_count
FROM REHAB_TASK
WHERE task_type_code = 'AC'
		and (opening_no = :al_opening_no or :al_opening_no = 0)
		and (related_task_no = :al_related_task_no or :al_related_task_no = 0)
		and claim_no = :il_claim_no
		and (task_status_code = :ls_task_status_code or :ls_task_status_code = '')
USING SQLCA;

SQLCA.nf_handle_error('n_action_item','nf_related_action_items_exist','')

If ll_count = 0 or IsNull(ll_count) then
	RETURN FALSE
ELSE
	RETURN tRUE
eND IF
end function

public function long nf_reschedule_related_action_items (long al_opening_no, long al_related_task_no, long al_days_diff, boolean ab_allow_user_selection);//RETURN	-1 if the action was cancelled
//RETURN  0 if no records were found to cancel
//RETURN  1 if records were found and the save should continue

INTEGER						li_rtn = 1, 	li_x
LONG							ll_items_rescheduled
DATETIME						ldt_old_date, ldt_new_date
S_WINDOW_MESSAGE	lstr_message


nf_retrieve_related_action_items(al_opening_no,al_related_task_no,True)
//The nf_retrieve_related_action_items should have been called first.
//if there were rows returned from that function then this function could be called
//else it shouldn't be called.  I made it an application error so the programmer
//calling this function would not forget to call nf_retrieve_related_action_items first.
If ids_related_action_items.RowCount() = 0 THen 
	RETURN 0
End if


If ab_allow_user_selection Then
	lstr_message.apo_PowerObjectParm[1] = ids_related_action_items
	lstr_message.as_stringparm[1] = 'The following items will be rescheduled to retain the time frame/interval between the original planned dates of the task and action item.'
	lstr_message.as_stringparm[2] = 'Reschedule related action items'
	OpenWithParm(w_cancel_action_items,lstr_message)
	//Check the return from the window to see what button the user pressed (continue, cancel)
	li_rtn = Message.DoubleParm		
End if


If li_rtn = 1 THen
	//Change the status and fill in the proper date for all the records that
	//are flagged to be cancelled.
	For li_x = 1 To ids_related_action_items.RowCount()
		If ids_related_action_items.GetItemString(li_x,'selection_flag') = 'Y' Then
				
			ldt_old_date = ids_related_action_items.GetItemDateTime(li_x,'planned_start_date')
			
			
			ldt_new_date = DateTime(RelativeDate(Date(ldt_old_date),al_days_diff))
			
			ids_related_action_items.SetItem(li_x,'planned_start_date',ldt_new_date)
			ids_related_action_items.SetItem(li_x,'planned_completion_date',ldt_new_date)
			ll_items_rescheduled ++			
		End if
	Next	

Else
	ll_items_rescheduled = li_rtn
End if


RETURN ll_items_rescheduled
end function

public function datetime nf_get_min_outstanding_planned_date (long al_opening_no, long al_related_task_no);INTEGER			li_x
DATETIME			ldt_smallest_date, ldt_current_date, ldt_null

SetNull(ldt_null)

nf_retrieve_related_action_items(al_opening_no,al_related_task_no,True)

If ids_related_action_items.RowCount() = 0 Then 
	RETURN ldt_null
End if
	
ldt_smallest_date = ids_related_action_items.GetITemDateTIme(1,'planned_start_date')

For li_x = 2 to ids_related_action_items.RowCount()

	ldt_current_date = ids_related_action_items.GetITemDateTIme(li_x,'planned_start_date')
	
	If ldt_current_date < ldt_smallest_date Then
		ldt_smallest_date = ldt_current_date
	End if
Next

RETURN ldt_smallest_date
end function

public function integer nf_check_mandatory ();INTEGER		li_rtn

nf_load_bus_rule_flags()

If idw_dw[ACTION_ITEM].ModifiedCount() > 0 Then
	
	//Responsible user id
	IF IsNull(idw_dw[ACTION_ITEM].GetItemString(1,'responsible_user_id')) or &
		idw_dw[ACTION_ITEM].GetItemString(1,'responsible_user_id')  = '' Then
		MessageBox('Validation Error','The responsible user is a required field')
		idw_dw[ACTION_ITEM].SetColumn('responsible_user_id')
		idw_dw[ACTION_ITEM].SetFocus()
		return -1
	END IF
	
	//Task status code
	IF IsNull(idw_dw[ACTION_ITEM].GetItemString(1,'task_status_code')) or &
		idw_dw[ACTION_ITEM].GetItemString(1,'task_status_code')  = '' Then
		MessageBox('Validation Error','The task status is a required field')
		idw_dw[ACTION_ITEM].SetColumn('task_status_code')
		idw_dw[ACTION_ITEM].SetFocus()
		return -1
	END IF
	
	//Progress note
	If ib_progress_note_required Then
		//Make sure there is a row (ie they have added a progress note.
		If idw_dw[PROGRESS_NOTE].RowCount() < 1 THen
			MessageBox('Validation Error','Please add a note')
			return -1
		End if
	END IF
	
	If not ib_progress_note_allowed Then
		If idw_dw[PROGRESS_NOTE].RowCount() > 0 Then
			MessageBox('Validation Error','Notes are not allowed for the action item type.  Cancel and try again.')
			return -1
		End if
	End if
	
	//set the real dates based on the computed_date
	nf_set_status_date(idw_dw[1].GetItemDateTime(1,'computed_date'))
	
End if

IF idw_dw[PROGRESS_NOTE].ModifiedCount() > 0 Then
	
	//This function splits the combined comment into comment1 and comment2
	nf_set_note_comment()
		
	IF IsNull(idw_dw[PROGRESS_NOTE].GetItemString(1,'event_type_code')) or &
		idw_dw[PROGRESS_NOTE].GetItemString(1,'event_type_code')  = '' Then
		MessageBox('Validation Error','The note type is a required field')
		idw_dw[PROGRESS_NOTE].SetColumn('event_type_code')
		idw_dw[PROGRESS_NOTE].SetFocus()
		return -1
	END IF

	IF IsNull(idw_dw[PROGRESS_NOTE].GetItemString(1,'event_specific_code')) or &
		idw_dw[PROGRESS_NOTE].GetItemString(1,'event_specific_code')  = '' Then
		MessageBox('Validation Error','The note specific type is a required field')
		idw_dw[PROGRESS_NOTE].SetColumn('event_specific_code')
		idw_dw[PROGRESS_NOTE].SetFocus()
		return -1
	END IF
	
	IF IsNull(idw_dw[PROGRESS_NOTE].GetItemString(1,'combined_comment')) or &
		idw_dw[PROGRESS_NOTE].GetItemString(1,'combined_comment')  = '' Then
		MessageBox('Validation Error','The note is a required field')
		idw_dw[PROGRESS_NOTE].SetColumn('combined_comment')
		idw_dw[PROGRESS_NOTE].SetFocus()
		return -1
	END IF
	
End if

return 1
end function

public function integer nf_set_status_date (datetime adt_computed_date);STRING				ls_status_code, ls_column_to_update[]
INTEGER			li_x
DATETIME			ldtm_check
DATE				ldt_null_date
dwItemStatus	ldws_task_status_code, ldws_computed_date	

ldws_task_status_code 	=  idw_dw[1].GetItemStatus(1,'task_status_code', Primary!)
ldws_computed_date 	=  idw_dw[1].GetItemStatus(1,'computed_date', Primary!)

ls_status_code = idw_dw[1].GetITemString(1,'task_status_code') 

/*	Set the computed date to the appropriate date column
	depending on the status.  The validation for the date should be done in the nf_check_bus_rule
*/

//Reset all the dates back to there original values in case there were set
//previously and then the save was aborted due to a business rule violation
ldtm_check = idw_dw[ACTION_ITEM].GetItemDateTime(1,'planned_start_date',Primary!,True)
idw_dw[ACTION_ITEM].SetItem(1,'planned_start_date',date(idw_dw[ACTION_ITEM].GetItemDateTime(1,'planned_start_date',Primary!,True)))
idw_dw[ACTION_ITEM].SetItemStatus(1,'planned_start_date',Primary!,NotModified!)

idw_dw[ACTION_ITEM].SetItem(1,'planned_completion_date',idw_dw[ACTION_ITEM].GetItemDateTime(1,'planned_completion_date',Primary!,True))
idw_dw[ACTION_ITEM].SetItemStatus(1,'planned_completion_date',Primary!,NotModified!)

idw_dw[ACTION_ITEM].SetItem(1,'actual_start_date',idw_dw[ACTION_ITEM].GetItemDateTime(1,'actual_start_date',Primary!,True))
idw_dw[ACTION_ITEM].SetItemStatus(1,'actual_start_date',Primary!,NotModified!)

idw_dw[ACTION_ITEM].SetItem(1,'actual_completion_date',idw_dw[ACTION_ITEM].GetItemDateTime(1,'actual_completion_date',Primary!,True))
idw_dw[ACTION_ITEM].SetItemStatus(1,'actual_completion_date',Primary!,NotModified!)

//used as column default
setnull(ldt_null_date)

/*
8.90		If the task status is Planned, the planned start date must be entered.
8.100	If the task status is Planned, the actual start date and actual completed must not be entered.
8.110	If the task status is Planned, the planned completed date must be the set to the planned start date.
8.120   If the task status is Closed or Reset, the actual start date must be the current date.
8.130   If the task status is Cancelled, the actual start date must be null.
8.140   If the task status is Closed or Reset, the actual completed date must be the set to the actual start date.
8.150   If the task status is Cancelled, the actual completed date must be null.

task_status_code 	task_status_desc   action_item_entry_flag 	task_entry_flag
----------------         	------------------------- ---------------------- 			---------------
01               			planned               	Y                      			Y
02               			in progress           	N                      			Y
03               			cancelled             	Y                      			Y
04               			closed                 	Y                      			Y
05               			reset                  	Y                      			N
*/

IF ldws_task_status_code <> notmodified! OR ldws_computed_date <> notmodified!  THEN 
	CHOOSE CASE ls_status_code
		CASE '01'//PLANNED
				idw_dw[ACTION_ITEM].SetItem(1,'actual_start_date',ldt_null_date)
				idw_dw[ACTION_ITEM].SetItem(1,'actual_completion_date',ldt_null_date)
				idw_dw[ACTION_ITEM].SetItem(1,'planned_completion_date',adt_computed_date)
				idw_dw[ACTION_ITEM].SetItem(1,'planned_start_date',date(adt_computed_date))
			
		CASE '02'//IN PROGRESS
			/* N/A */
			
		CASE '03'//CANCELLED
			
				idw_dw[ACTION_ITEM].SetItem(1,'actual_start_date',ldt_null_date)
				idw_dw[ACTION_ITEM].SetItem(1,'actual_completion_date',ldt_null_date)
					
		CASE '04'//CLOSED
			
				idw_dw[ACTION_ITEM].SetItem(1,'actual_start_date',date(f_server_datetime()))
				idw_dw[ACTION_ITEM].SetItem(1,'actual_completion_date', date(f_server_datetime()))
					
		CASE '05'//RESET
			
				idw_dw[ACTION_ITEM].SetItem(1,'actual_start_date',date(f_server_datetime()))
				idw_dw[ACTION_ITEM].SetItem(1,'actual_completion_date', date(f_server_datetime()))	
			
	END CHOOSE
END IF 

return 1


end function

public function integer nf_create_initial_action_items (long al_opening_no, datetime adt_disablement_date, string as_responsible_user_id);//returns 0 if nothing was created because the intial 3 actions items where already created
//returns 1 if things went well
LONG			ll_opening_no
STRING			ls_sub_type_code[] = {'150','151','152'}, ls_status_code = '01'
STRING			ls_rehab_program_code, ls_rehab_service_code
INTEGER		li_default_weeks[], li_x
DATETIME		ldt_planned_start_date[], ldt_disablement_date

IF il_claim_no 					= 0 THEN SignalError(-666,'Claim number not set for action item')
IF as_responsible_user_id 	= '' THEN SignalError(-666,'NO responsible user for action items')

ls_rehab_service_code = 'S002'	

IF al_opening_no = 0 THEN
	il_opening_no = nf_get_opening_no(TRUE)
	IF il_opening_no = 0 THEN RETURN 0
	ldt_disablement_date = nf_get_disablement_date(il_opening_no)
ELSE
	il_opening_no 				= al_opening_no
	ldt_disablement_date 	= adt_disablement_date
END IF

IF nf_is_initial_item(il_opening_no) THEN		
	FOR li_x = 1 to UpperBound(ls_sub_type_code)
		li_default_weeks[li_x] 			= nf_get_default_weeks(ls_sub_type_code[li_x])
		ldt_planned_start_date[li_x] 	= DateTime(RelativeDate(Date(Ldt_disablement_date),(li_default_weeks[li_x] -1)*7))
		
		//sub_type,specific,status,status_date,responsible_user,comment
		nf_create_action_item(ls_sub_type_code[li_x], '.', ls_status_code, ldt_planned_start_date[li_x], as_responsible_user_id, '', ls_rehab_program_code, ls_rehab_service_code )
	NEXT
ELSE
	RETURN 0
END IF

nf_set_identifiers()

RETURN 1

end function

public function integer nf_update ();LONG ll_error
INT	li_cntr = 1, li_bound
INTEGER		li_event_no
STRING		ls_claimant_name
STRING		ls_action_code
STRING		ls_folder_name
LONG			ll_catid
INTEGER		li_rtn 
w_rehab_sheet		iw_rehab_sheet

iw_rehab_sheet = iwi_window_parent

li_bound = UpperBound(idw_dw)
DO WHILE li_cntr <= li_bound
	idw_dw[li_cntr].Update()
   ll_error = inv_transobj.nf_handle_error("n_pdc","nf_update","updateing dw")
   IF ll_error < 0 THEN
      Return ll_error
   END IF
	// Error Handling
	li_cntr ++
LOOP


If ids_action_item.RowCount() > 0 Then
	ids_action_item.Update()
	SQLCA.nf_handle_error('n_action_item','nf_update','update ids_action_item')
	ids_action_item.Reset()
End if

If IsValid(ids_related_action_items) Then
	IF ids_related_action_items.RowCount() > 0 THen
		ids_related_action_items.Update()
		sqlca.nf_handle_error('n_action_item','nf_update','update ids_cancel_action_items')
		ids_related_action_items.Reset()
	END IF
END IF

If IsValid(ids_rehab_task_progress_note) Then
	If ids_rehab_task_progress_note.RowCount() > 0 Then
		ids_rehab_task_progress_note.update()
		SQLCA.nf_handle_error('n_action_item','nf_update','update ids_rehab_task_progress_note')
		ids_rehab_task_progress_note.reset()
	End if
End if

If IsValid(ids_progress_note) THen
	If ids_progress_note.RowCount() > 0 Then
		ids_progress_note.Update()
		SQLCA.nf_handle_error('n_action_item','nf_update','update ids_progress_note')
		ids_progress_note.Reset()
	End if
End if

//The idw_dw datawindow array might not be used if
//this object is being used in the background with no user interface 
//connections
IF UpperBound(idw_dw) > 0 THen
	//Send a folder to the inbasket module if
	If idw_dw[ACTION_ITEM].GetItemString(1,'task_status_code') = '04' THen
		If ib_progress_note_required Then
			If idw_dw[PROGRESS_NOTE].GetItemString(1,'event_type_code') = '021' Then
				If idw_dw[PROGRESS_NOTE].GetItemString(1,'event_specific_code') = 'CG' Then
					//The confirm diagnosis is being completed and the event specific code
					//is 'Changed'.  Send a message to the inbasket telling the 
					//health records to look at this action item.
		
					ls_claimant_name = iw_rehab_sheet.dw_basic_claim.GetItemString(1,"given_names") + " " + iw_rehab_sheet.dw_basic_claim.GetItemString(1,"last_name")
					ls_action_code = 'HM'
					ls_folder_name = ls_action_code + String(il_claim_no) + ls_claimant_name
					ll_catid = nf_get_routing_catid(DIAGNOSIS_CHANGE)
					If ll_catid = -1 Then SignalError(-666,'Error getting diagnosis change catid')
					
					li_event_no = idw_dw[PROGRESS_NOTE].GetItemNumber(1,'event_no')
					inv_working_folders.nf_add(ls_action_code,ll_catid ,f_server_dateTime(),'See Event ' + String(li_event_no),ls_folder_name )
					
					ImageTrans.nf_begin_transaction()
					
					li_rtn = inv_working_folders.nf_save()				
					
					IF li_rtn < 0 THEN
						ImageTrans.nf_rollback_transaction()
					ELSE
						ImageTrans.nf_commit_transaction()
					END IF
				End if
			End if
		End if
	End if
End if


Return 0
end function

public function integer nf_item_change_accepted (long al_row, string as_column_name);//This is called after the item has been changed and the contents
//of the edit control have been applied to the datawindow buffer
DATETIME		ldt_new_computed_date, ldt_null, ldt_null_date
STRING			ls_rescheduled_allowed_flag, ls_task_status_code
INTEGER		li_week
LONG			ll_specific_row


CHOOSE CASE as_column_name
	CASE 'task_type_code'
		nf_filter_child('task_sub_type_code')
		
	CASE 'comment'
		
		//remove the trailing blanks AND THE ONES IN FRONT FOR CK_REHAB_TASK_5
		idw_dw[ACTION_ITEM].SetItem(1,'comment',trim(idw_dw[ACTION_ITEM].getitemstring(1,'comment')))
		
	CASE 'task_sub_type_code'
		//filter the specific
		nf_filter_child('task_specific_code')		
		//Filter the status
		nf_filter_child('task_status_code')
		//FIlter the reset reason
		nf_filter_child('reset_reason_code')
		//Filter the event type code
		nf_filter_child('event_type_code')
		
		ll_specific_row = idwc_specific.GetRow()
		If ll_specific_row > 0 THen
			ls_rescheduled_allowed_flag = idwc_specific.GetITemString(idwc_specific.GetRow(),'reschedule_allowed_flag')
			idw_dw[ACTION_ITEM].SetItem(1,'reschedule_allowed_flag',ls_rescheduled_allowed_flag )
		End if
		
		//Since the computed_date is not a database column we have to dynamically keep
		//it displaying the proper date.  This function does that.
		nf_set_computed_date()
		
	CASE 'task_specific_code'
		//Filter the status
		nf_filter_child('task_status_code')
		
	CASE 'computed_date'
		ldt_new_computed_date = idw_dw[ACTION_ITEM].GetItemDateTime(1,'computed_date')
		If DaysAfter(Date(idt_today),Date(ldt_new_computed_date)) > 365 Then
			MessageBox('Warning','The date you have entered is more than one year in the future.  Please make sure this is really the date you meant to enter.')
		End if
		
		li_week = nf_calculate_week(ldt_new_computed_date )
		idw_dw[ACTION_ITEM].SetItem(1,'week_no',li_week)
		idw_dw[ACTION_ITEM].SetItemStatus(1,'week_no',Primary!,NotModified!)
		
	CASE 'week_no'
		li_week = idw_dw[ACTION_ITEM].GetItemNumber(1,'week_no')
		ldt_new_computed_date = nf_calculate_status_date(li_week)
		If DaysAfter(Date(idt_today),Date(ldt_new_computed_date)) > 365 Then
			MessageBox('Warning','The date you have entered is more than one year in the future.  Please make sure this is really the date you meant to enter.')
		End if
		
		idw_dw[ACTION_ITEM].SetItem(1,'computed_date',ldt_new_computed_date)
		idw_dw[ACTION_ITEM].SetItemStatus(1,'computed_date',Primary!,NotModified!)	
	
	CASE 'reset_date'
		ldt_new_computed_date = idw_dw[ACTION_ITEM].GetItemDateTime(1,'reset_date')		
		
		li_week = nf_calculate_week(ldt_new_computed_date )
		//It shouldn't matter wether we set the reset_week_no_editmask or reset_week_no_dddw
		//since they are both pointing to the same database column
		idw_dw[ACTION_ITEM].SetItem(1,'reset_week_no_editmask',li_week)
		idw_dw[ACTION_ITEM].SetItemStatus(1,'reset_week_no_editmask',Primary!,NotModified!)
	
	CASE 'reset_week_no_dddw','reset_week_no_editmask'
		li_week = idw_dw[ACTION_ITEM].GetItemNumber(1,as_column_name)
		ldt_new_computed_date = nf_calculate_status_date(li_week)
		
		idw_dw[ACTION_ITEM].SetItem(1,'reset_date',ldt_new_computed_date)
		idw_dw[ACTION_ITEM].SetItemStatus(1,'reset_date',Primary!,NotModified!)

	CASE 'task_status_code'
		
		//used as column default
		setnull(ldt_null_date)
		
		/*
			task_status_code 	task_status_desc  action_item_entry_flag  task_entry_flag
			---------------- 			------------------------  ----------------------           ---------------
			01               			planned               Y                                Y
			02               			in progress           N                                Y
			03               			cancelled             Y                                Y
			04               			closed                 Y                                Y
			05               			reset                   Y                                N
		*/
		
			ls_task_status_code = idw_dw[ACTION_ITEM].GetItemString(1,'task_status_code') 

			//Since the computed_date is not a database column we have to dynamically keep
			//it displaying the proper date.  This function does that.
			nf_set_computed_date()
			
		//IF the status code is not 'reset' the set the reset reason code back to an empty value.
		IF ls_task_status_code  <> '05' Then
			idw_dw[ACTION_ITEM].SetItem(1,'reset_reason_code','')
		END IF
		
		nf_filter_child('event_specific_code')
		
	CASE 'event_type_code'
		nf_filter_child('event_specific_code')
		
	CASE 'reset_reason_code'		
		//If the sub_type = '151' (timely entervention) and reset_reason_code = '06' (Diagnosis Change)
		//Then the user is presented with a drop down containing a list of guidlines.  We will 
		//default to the record that has a default flag = 'Y'

			SetNull(ldt_null)
			idw_dw[ACTION_ITEM].SetItem(1,'reset_date',ldt_null)
			idw_dw[ACTION_ITEM].SetItem(1,'reset_week_no_dddw',0)
			idw_dw[ACTION_ITEM].SetItemStatus(1,'reset_week_no_dddw',Primary!,NotModified!)
			idw_dw[ACTION_ITEM].SetItemStatus(1,'reset_date',Primary!,NotModified!)
			
END CHOOSE

RETURN 1
end function

public subroutine nf_load_child_datawindows ();String			ls_error = 'Error getting reference to datawindow child'
LONG				ll_row
LONG				ll_rows_retrieved
/*	Load all the child datawindows.  Called when the datawindow is first opened
	so we have a reference to the child datawindow.
*/

//ACTION ITEM
If idw_dw[ACTION_ITEM].GetChild('task_type_code',idwc_type) = -1 THen SignalError(-666,ls_error)

IF idw_dw[ACTION_ITEM].GetChild('task_sub_type_code',idwc_sub_type) = -1 THen SignalError(-666,ls_error)

If idw_dw[ACTION_ITEM].GetChild('task_specific_code',idwc_specific) = -1 Then SignalError(-666,ls_error)

If idw_dw[ACTION_ITEM].GetChild('task_status_code',idwc_status) = -1 Then SignalError(-666,ls_error)

If idw_dw[ACTION_ITEM].GetChild('responsible_user_id',idwc_responsible) = -1 Then SignalError(-666,ls_error)

If idw_dw[ACTION_ITEM].GetChild('reset_reason_code',idwc_reset_reason) = -1 THen SignalError(-666,ls_error)

If idw_dw[ACTION_ITEM].GetChild('reset_week_no_dddw',idwc_reset_week_no) = -1 Then SignalError(-666,ls_error)

/* Insert a blank row in the drop down datawindow
*/
idwc_reset_week_no.SetTransObject(SQLCA)
ll_rows_retrieved = idwc_reset_week_no.Retrieve()
If ll_rows_retrieved < 1 THen SignalError(-666,'Error retrieving guidlines for resetting.')

ll_row = idwc_reset_week_no.InsertRow(1)
idwc_reset_week_no.SetItem(ll_row,'weeks_from_disablement',0)
idwc_reset_week_no.SetItem(ll_row,'disability_duration_desc','')



//PROGRESS NOTE
If idw_dw[PROGRESS_NOTE].GetChild('event_type_code',idwc_event_type) = -1 Then SignalError(-666,ls_error)

If idw_dw[PROGRESS_NOTE].GetChild('event_specific_code',idwc_event_specific) = -1 Then SignalError(-666,ls_error)


end subroutine

public function integer nf_validate_reset ();STRING			ls_reset_reason_code, ls_old_reset_reason_code, ls_task_sub_type_code,	ls_task_specific_code
STRING			ls_responsible_user_id, ls_expected_healing_desc,	ls_warning_message,	ls_rehab_program_code,	ls_rehab_service_code
DATETIME		ldt_reset_date,	ldt_disablement_date,	ldt_planned_start_date,	ldt_max_reset_date,	ldt_min_planned_start_date
LONG			ll_reset_week_no,	ll_max_reset_weeks
INTEGER		li_rtn

//If the reset is valid then create the new record
ls_task_sub_type_code 		= 	idw_dw[ACTION_ITEM].GetItemString(1,'task_sub_type_code')
ls_task_specific_code 		= 	idw_dw[ACTION_ITEM].GetItemString(1,'task_specific_code')		
ls_responsible_user_id 		= 	idw_dw[ACTION_ITEM].GetItemString(1,'responsible_user_id')	
ls_reset_reason_code 		= 	idw_dw[ACTION_ITEM].GetItemString(1,'reset_reason_code')
ls_old_reset_reason_code 	= idw_dw[ACTION_ITEM].GetItemString(1,'reset_reason_code',Primary!,True)
ls_rehab_program_code 	= 	idw_dw[ACTION_ITEM].GetItemString(1,'rehab_program_code')
ls_rehab_service_code 		= 	idw_dw[ACTION_ITEM].GetItemString(1,'rehab_service_code')		


If  IsNull(ls_reset_reason_code) OR ls_reset_reason_code = '' Then
	MessageBox('Validation Error','Please choose a reset reason')
	idw_dw[ACTION_ITEM].SetColumn('reset_reason_code')
	idw_dw[ACTION_ITEM].SetFocus()
	RETURN -1
End if

//If the old reset reason <> '' and it's different then the new reset reason then
//the user is changing the reason on the same day as the record was saved as "reset"
//Warn them that this change could affect what the planned_start_date would have
//been for the new action item and tell them to change it if necessary.
IF ls_old_reset_reason_code <> '' and (ls_old_reset_reason_code <> ls_reset_reason_code) THen
	MessageBox('Warning',"Changing the reset reason may affect the new action item's planned date.  Please check it and make changes if necessary.")
Else
		
	ldt_reset_date 		= idw_dw[ACTION_ITEM].GetItemDateTime(1,'reset_date')
	ll_reset_week_no 	= idw_dw[ACTION_ITEM].GetItemNumber(1,'reset_week_no_editmask')
	
	//If the reset reason is 'Diagnosis Change' then the user was forced to pick a reset
	//date from a list therefore it is not necessary to validate the date
	//Else it is
	IF ls_reset_reason_code = '06' Then
		If ll_reset_week_no = 0 Then
			MessageBox('Validation Error','You must make a selection from the guideline.')
			idw_dw[ACTION_ITEM].SetColumn('reset_week_no_dddw')
			idw_dw[ACTION_ITEM].SetFocus()
			RETURN -1
		END IF
	End if	
	
	If IsNull(ldt_reset_date) Then
		MessageBox('Validation Error','Please specify a reset date.')
		idw_dw[ACTION_ITEM].SetColumn('reset_date')
		idw_dw[ACTION_ITEM].SetFocus()
		RETURN -1
	End if
	
	ldt_disablement_date 	= idw_dw[ACTION_ITEM].GetItemDateTime(1,'disablement_date')
	ldt_planned_start_date 	= idw_dw[ACTION_ITEM].GetItemDateTime(1,'planned_start_date')
	
	
	IF ls_reset_reason_code <> '06' Then
	
		If ldt_reset_date < idt_today THen
			MessageBox('Validation Error','The planned date for the reset item cannot be less than todays date.')
			idw_dw[ACTION_ITEM].SetColumn('reset_date')
			idw_dw[ACTION_ITEM].SetFocus()
			RETURN -1
		End if
		
		If ldt_reset_date < ldt_disablement_date Then
			MessageBox('Validation Error','The planned date for the reset item must be on or after the disablement date.')
			idw_dw[ACTION_ITEM].SetColumn('reset_date')
			idw_dw[ACTION_ITEM].SetFocus()
			RETURN -1
		End if
		
		If ldt_reset_date = ldt_planned_start_date THen
			MessageBox('Validation Error','The planned date for the reset item cannot be the same as the planned date for the current item.')
			idw_dw[ACTION_ITEM].SetColumn('reset_date')
			idw_dw[ACTION_ITEM].SetFocus()
			RETURN -1
		End if		
	
		//Get the max reset weeks
		ll_max_reset_weeks = nf_get_max_reset_weeks(ls_expected_healing_desc)
		
		//the max reset date is used for sub_type '150','152'
		//For these two type we have to check the planned_reset_date
		//to make sure it is not passed the actual_completion_date + the max_reset_number_weeks
		ldt_max_reset_date = nf_calculate_status_date(ll_max_reset_weeks + 1,idt_today)
		
		
		If ls_task_sub_type_code = '150' Then //Confirm Diagnosis				
			IF ldt_reset_date > ldt_max_reset_date Then
				li_rtn = MessageBox('Guideline reminder','The suggested maximum weeks for resetting this type of Action Item is ' + String(ll_max_reset_weeks) + ' weeks (' + String(ldt_max_reset_date,"yyyy-mm-dd") + ') from the current date.' &
											+'  ~r~nDo you want to cancel the save so you can change the reset date.',Question!,YesNo!,2)
				If li_rtn = 1 Then
					idw_dw[ACTION_ITEM].SetColumn('reset_week_no_editmask')
					idw_dw[ACTION_ITEM].SetFocus()
					return -1
				End if
			End if
		End if
		
		If ls_task_sub_type_code = '152' Then 	//Work Restriction
			If ldt_reset_date > ldt_max_reset_date Then
				MessageBox('Guideline error','The reset date cannot be greater than ' + String(ll_max_reset_weeks) + ' weeks ('  + String(ldt_max_reset_date,"yyyy-mm-dd") + ') from the current date.')
				idw_dw[ACTION_ITEM].SetColumn('reset_week_no_editmask')
				idw_dw[ACTION_ITEM].SetFocus()
				return -1
			End if
		End if
		
		//The Timely Intervetion max reset weeks is from the disablement date not the actual completion
		//date like the other two types.
		If ls_task_sub_type_code = '151' Then
			If ll_reset_week_no > ll_max_reset_weeks Then
				ls_warning_message = 'You have entered a reset date that is past the suggested default of ' + ls_expected_healing_desc
				OpenWithParm(w_expected_healing_guideline_warning,ls_warning_message)
				
				li_rtn = Message.DoubleParm
				If li_rtn = -1 Then 
					idw_dw[ACTION_ITEM].SetColumn('reset_week_no_editmask')
					idw_dw[ACTION_ITEM].SetFocus()
					return -1
				End if
			End if
		End if
	ENd if
	
	//Warn the user if the reset date is after the earliest planned	
	If ls_task_sub_type_code = '150' Then
		SELECT min(planned_start_date) into :ldt_min_planned_start_date
		FROM REHAB_TASK
		WHERE claim_no = :il_claim_no
				and task_sub_type_code in('151','152')
				and actual_completion_date is null
		USING SQLCA;
		
		SQLCA.nf_handle_error('n_action_item','nf_validate_reset','SELECT min(planned_start_date) into :ldt_min_planned_start_date')
		
		If ldt_reset_date > ldt_min_planned_start_date THen
			MessageBox('Warning',"This planned date for the new action item is after the planned date for either the 'Timely Intervention' or the 'Work Restriction' action item.~r~n" &
										 + "Please review these other action itmes to verify their dates are still valid.")
		End if										 
	End if

	//Warn the user if the reset date is after the earliest planned	
	If ls_task_sub_type_code = '151' Then
		SELECT min(planned_start_date) into :ldt_min_planned_start_date
		FROM REHAB_TASK
		WHERE claim_no = :il_claim_no
				and task_sub_type_code in('152')
				and actual_completion_date is null
		USING SQLCA;
		
		SQLCA.nf_handle_error('n_action_item','nf_validate_reset','SELECT min(planned_start_date) into :ldt_min_planned_start_date')
		
		If ldt_reset_date > ldt_min_planned_start_date THen
			MessageBox('Warning',"This planned date for the new action item is after the planned date for the 'Work Restriction' action item.~r~n" &
										 + "Please review this action itme to verify the date is still valid.")
		End if										 
	End if
				
	//Move this to the end of the script!!!!!!!!!!!!
	nf_create_action_item(ls_task_sub_type_code,ls_task_specific_code ,'01',ldt_reset_date,ls_responsible_user_id ,'', ls_rehab_program_code, ls_rehab_service_code)
End if

return 1
end function

public subroutine nf_set_protect_expressions ();/*	These protect expressions will handle the protection of most columns.
	Protection that is dependent on flags within the drop downs will have
	to be handled in the item changed event.
*/

STRING			ls_error, ls_modify[], ls_null[]
INTEGER		li_x
DATETIME		ldt_server_datetime
DATE			ld_today
			
//We use the server datetime for the current date
ldt_server_datetime = f_server_datetime()
ld_today = Date(ldt_server_datetime)

/*************************************************************************/
//	Protect the Action Item datawindow

ls_modify[1] = "task_type_code.Protect='1~tIf(IsRowNew(),0,1)'"

ls_modify[2] = "task_sub_type_code.Protect='1~tIf(IsRowNew()and IsNull(related_task_no),0,1)'"

ls_modify[3] = "task_specific_code.Protect='1~tIf(IsRowNew(),0,1)'"

ls_modify[4] = "computed_date.Protect='1~tIf( task_status_code <> ~~'01~~',1,If(IsNull(planned_start_date),0,If(reschedule_allowed_flag = ~~'N~~',1,0)))'"

ls_modify[5] = "week_no.Protect='1~tIf( task_status_code <> ~~'01~~',1,If(IsNull(planned_start_date),0,If(reschedule_allowed_flag = ~~'N~~',1,0)))'"

ls_modify[6] = "reset_reason_code.Protect='1~tIf( actual_completion_date <> DateTime(Date(~~'" + String(ld_today,'yyyy-mm-dd') + "~~')),1,If( task_status_code <> ~~'05~~',1,0))'"

ls_modify[7] = "task_status_code.Protect='1~tIf( IsRowNew(),0,IF( task_status_code.original <> ~~'01~~',1,0))'"

ls_modify[8] = "comment.Protect='1~tIf( IsRowNew(),0,IF( task_status_code.original <> ~~'01~~',1,0))'"

ls_modify[9] = "responsible_user_id.Protect='1~tIf( IsNull(claim_no),0,IF( task_status_code.original <> ~~'01~~',1,0))'"

//IF the reset reason = '' the  protect the column 
//else if the sub_type = '151' (timely intervention) and reset_reason_code = '06' (Diagnosis Change) then protect the column
//else don't protect the column
ls_modify[10] = "reset_date.Protect='1~tIf( reset_reason_code <> ~~'~~' ,IF(task_sub_type_code = ~~'151~~' and reset_reason_code = ~~'06~~',1,0),1)'" 


For li_x = 1 to UpperBound(ls_modify)
	ls_error = idw_dw[ACTION_ITEM].Modify(ls_modify[li_x])
	If ls_error <> '' Then SignalError(-666,ls_error)
Next

//Reset the array
ls_modify = ls_null

//*************************************************************************/
//Protect the Progress Note datawindow
ls_modify[1] = "event_type_code.Protect='1~tIf(IsNull(event_no),0,1)'"

ls_modify[2] = "event_specific_code.Protect='1~tIf(IsNull(event_no),0,1)'"

//ls_modify[3] = "combined_comment.Protect='1~tIf(IsNull(event_no),0,1)'"

For li_x = 1 to UpperBound(ls_modify)
	ls_error = idw_dw[PROGRESS_NOTE].Modify(ls_modify[li_x])
	If ls_error <> '' Then SignalError(-666,ls_error)
Next
end subroutine

public function integer nf_protect_progress_comment ();STRING		ls_displayonly
STRING		ls_modify
STRING		ls_return

ls_modify = "combined_comment.edit.displayonly="


IF idw_dw[PROGRESS_NOTE].GetItemStatus(1,0,Primary!) = NewModified! &
	or idw_dw[PROGRESS_NOTE].GetItemStatus(1,0,Primary!) = New!Then
	ls_displayonly = 'no'
else
	ls_displayonly = 'yes'
End if
	
	
ls_return = idw_dw[PROGRESS_NOTE].modify(ls_modify + ls_displayonly)
IF ls_return <> '' Then SignalError(-666,ls_return)

RETURN 1
end function

public function integer nf_reassign_related_action_items (long al_opening_no, long al_new_opening_no);/**********************************************************************/
/* PR5108 - Recurrence to Administrative - J.Hawker, 2005.08.19       */
/* This function is used to reassign action items. When a Recurrence  */
/* opening is changed to an Administrative opening, the related action*/
/* items must be reassigned to the latest matching recurrence opening.*/
/**********************************************************************/

LONG  ll_cntr, ll_rows

/* Retrieve ALL of the related Action Items
*/
nf_retrieve_related_action_items(al_opening_no,0, FALSE)

ll_rows = ids_related_action_items.RowCount() 

/* If no rows are returned, something went wrong. This function should only
   be called if there are related action items so rows should be returned.
*/
IF ll_rows <= 0 THEN 
	RETURN -1
END IF

/* For each action item related to the old opening, update it with the 
   new opening number.
*/
FOR ll_cntr = 1 TO ll_rows
	ids_related_action_items.SetItem(ll_cntr,'opening_no',al_new_opening_no)
NEXT	

RETURN 0
end function

public function integer nf_check_bus_rule ();LONG    			ll_count
INTEGER 			li_rtn
STRING  			ls_event_specific_code, ls_event_type_code, ls_task_status
DATETIME			ldtm_planned_start_date, ldtm_actual_start_date, ldtm_actual_completion_date, ldtm_planned_completed_date
dwItemStatus	ldws_task_status_code, ldws_computed_date	


ldws_task_status_code 	=  idw_dw[1].GetItemStatus(1,'task_status_code', Primary!)
ldws_computed_date 	=  idw_dw[1].GetItemStatus(1,'computed_date', Primary!)

IF idw_dw[ACTION_ITEM].ModifiedCount() <= 0 THEN RETURN 1
	
// Validate the entry of a new action item.  RULE: Only one
// action item (excluding Follow-ups) can be planned at one time.
IF idw_dw[ACTION_ITEM].GetItemStatus(1,0,Primary!) = NewModified! THEN
	IF nf_validate_insert_bus_rules() = -1 THEN RETURN -1
END IF

/*
8.90	    If the task status is Planned, the planned start date must be entered.
8.100	If the task status is Planned, the actual start date and actual completed must not be entered.
8.110	If the task status is Planned, the planned completed date must be the set to the planned start date.
8.120   If the task status is Closed or Reset, the actual start date must be the current date.
8.130   If the task status is Cancelled, the actual start date must be null.
8.140   If the task status is Closed or Reset, the actual completed date must be the set to the actual start date.
8.150   If the task status is Cancelled, the actual completed date must be null.
planned_completion_date - Reasonable check would be 5 years PR12848 - to be added to test plan

task_status_code task_status_desc  action_item_entry_flag task_entry_flag
---------------- -------------------------------   -------------                   ---------------
01               planned                      Y                               Y
02               in progress                  N                               Y
03               cancelled                    Y                               Y
04               closed                        Y                               Y
05               reset                         Y                                N
*/

ldtm_planned_start_date 			=  idw_dw[ACTION_ITEM].GetItemdatetime(1,'planned_start_date')
ldtm_actual_start_date  			=  idw_dw[ACTION_ITEM].GetItemdatetime(1,'actual_start_date')
ldtm_actual_completion_date  	=  idw_dw[ACTION_ITEM].GetItemdatetime(1,'actual_completion_date')
ldtm_planned_completed_date 	=  idw_dw[ACTION_ITEM].GetItemdatetime(1,'planned_completion_date')
ls_task_status 							= 	idw_dw[ACTION_ITEM].GetItemString(1,'task_status_code')


/* 3.145 	The planned completion date should not be greater than 5 years in the future.   */
// planned_completion_date - Reasonable check would be 5 years PR12848 - to be added to test plan - is a should just message the users.
IF NOT isnull(ldtm_planned_completed_date) THEN 
	IF daysafter(date(f_server_datetime()),date(ldtm_planned_completed_date) ) > (365 * 5 ) THEN 
		MessageBox("Validation Error","The planned completion date should not be greater than 5 years in the future.")
	END IF 
END IF 

/* if there is a comment the comment must be trimmed */
idw_dw[ACTION_ITEM].setitem(1, 'comment' , trim(idw_dw[ACTION_ITEM].GetItemString(1,'comment')))

CHOOSE CASE ls_task_status
	CASE '01'//PLANNED
	
		//8.90	    If the task status is Planned, the planned start date must be entered.	
		IF ISNULL(ldtm_planned_start_date)  THEN 
			MessageBox("Validation Error","If the task status is Planned, the planned start date must be entered.")
			idw_dw[ACTION_ITEM].SetColumn('planned_start_date')
			idw_dw[ACTION_ITEM].SetFocus()
			RETURN -1
		END IF 
		
		//8.100	If the task status is Planned, the actual start date and actual completed must not be entered.
		IF NOT ISNULL(ldtm_actual_start_date)  THEN 
			MessageBox("Validation Error","If the task status is Planned, the actual start date must not be entered.")
			idw_dw[ACTION_ITEM].SetColumn('actual_start_date')
			idw_dw[ACTION_ITEM].SetFocus()
			RETURN -1
		END IF 
				
		//8.100	If the task status is Planned, the actual start date and actual completed must not be entered.
		IF NOT ISNULL(ldtm_actual_completion_date)  THEN 
			MessageBox("Validation Error","If the task status is Planned, the actual completed must not be entered.")
			idw_dw[ACTION_ITEM].SetColumn('actual_completion_date')
			idw_dw[ACTION_ITEM].SetFocus()
			RETURN -1
		END IF 
		
		//8.110	If the task status is Planned, the planned completed date must be the set to the planned start date.
		IF DATE(ldtm_planned_completed_date) <> DATE(ldtm_planned_start_date) THEN
			MessageBox("Validation Error","If the task status is Planned, the planned completed date must be set to the planned start date.")
			idw_dw[ACTION_ITEM].SetColumn('planned_start_date')
			idw_dw[ACTION_ITEM].SetFocus()
			RETURN -1
		END IF 
	
	CASE '02'// in progres
		
		/* N/A */
			
	CASE '03'//cancelled
		//8.130   If the task status is Cancelled, the actual start date must be null.
		IF NOT ISNULL(ldtm_actual_start_date)  THEN 
			MessageBox("Validation Error","If the task status is Cancelled, the actual start date must be null.")
			idw_dw[ACTION_ITEM].SetColumn('actual_start_date')
			idw_dw[ACTION_ITEM].SetFocus()
			RETURN -1
		END IF 

		//8.150   If the task status is Cancelled, the actual completed date must be null.
		IF NOT ISNULL(ldtm_actual_completion_date)  THEN 
			MessageBox("Validation Error","If the task status is Cancelled, the actual start date must be null.")
			idw_dw[ACTION_ITEM].SetColumn('actual_completion_date')
			idw_dw[ACTION_ITEM].SetFocus()
			RETURN -1
		END IF 
		
	CASE '04','05'
		
		//8.120   If the task status is Closed or Reset, the actual start date must be the current date.
		IF DATE(ldtm_actual_start_date) <> DATE(f_server_datetime())  THEN
			MessageBox("Validation Error","If the task status is Closed or Reset, the actual start date must be the current date.")
			idw_dw[ACTION_ITEM].SetColumn('actual_start_date')
			idw_dw[ACTION_ITEM].SetFocus()
			RETURN -1
		END IF 
		
		//8.140   If the task status is Closed or Reset, the actual completed date must be the set to the actual start date.
		IF DATE(ldtm_actual_completion_date) <> DATE(f_server_datetime())  THEN
			MessageBox("Validation Error","If the task status is Closed or Reset, the actual completed date must be the set to the actual start date.")
			idw_dw[ACTION_ITEM].SetColumn('actual_completion_date')
			idw_dw[ACTION_ITEM].SetFocus()
			RETURN -1
		END IF 
		
	CASE ELSE
		
		/* N/A */
		
END CHOOSE

// Make sure there is a comment entered if it is required
IF ib_comment_required Then
	IF idw_dw[ACTION_ITEM].GetItemString(1,'comment') = '' &
		OR IsNull(idw_dw[ACTION_ITEM].GetItemString(1,'comment'))THEN
		MessageBox("Validation Error","Comment is a required field")
		idw_dw[ACTION_ITEM].SetColumn('comment')
		idw_dw[ACTION_ITEM].SetFocus()
		RETURN -1
	END IF
END IF	
	
// VALIDATE RESET
IF idw_dw[ACTION_ITEM].GetItemString(1,'task_status_code') = '05' THEN		
	IF nf_validate_reset()  = -1 THEN RETURN -1
END IF			
	
// IF the action item is 'Confirm Diagnosis' and it's status is 'complete'
// and the event specific code = 'Diagnosis changed' then
//	If there are outstanding 'Timely Intervention' or' WOrk REstriction' 
// action items, warn the user
IF idw_dw[ACTION_ITEM].GetItemStatus(1,'task_status_code',Primary!) = DataModified! THEN
	IF idw_dw[ACTION_ITEM].GetItemString(1,'task_status_code') = '04' THEN
		IF idw_dw[ACTION_ITEM].GetItemString(1,'task_sub_type_code') = '150' THEN
			// Event specific 'Diagnosis Changed'
			IF idw_dw[PROGRESS_NOTE].GetItemString(1,'event_specific_code') = 'CG' THEN
				
				// Check for oustanding items
				SELECT 	COUNT(*) INTO :ll_count
				FROM 		REHAB_TASK
				WHERE 	task_type_code 		= 'AC'
				AND 			task_sub_type_code 	in('151','152')
				AND 			claim_no 					= :il_claim_no
				USING 		SQLCA;
				SQLCA.nf_handle_error('n_action_item','nf_check_bus_rule','Check for outstanding action items')
					
				IF ll_count <> 0 AND NOT IsNull(ll_count) THEN
					MessageBox('Diagnosis Changed','The Diagnosis has changed and Timely Intervention and/or Determine Work Restriction action items is/are still outstanding.' & 
									+ '~r~nPlease review these action items to verify their dates are still valid.')
				END IF
			END IF
		END IF
	END IF
END IF

RETURN 1
end function

public function integer nf_create_action_item (string as_sub_type_code, string as_specific_code, string as_status_code, datetime adt_status_date, string as_responsible_user_id, string as_comment, string as_rehab_program_code, string as_rehab_service_code);LONG			ll_row

If il_claim_no 			= 0 Then SignalError(-666,'Missing claim number')
IF il_opening_no 	= 0 THen SignalError(-666,'Missing opening number')

ll_row = ids_action_item.InsertRow(0)

// EPHYSIO
ids_action_item.SetItem(ll_row,'rehab_service_code',as_rehab_service_code)
ids_action_item.SetItem(ll_row,'rehab_program_code',as_rehab_program_code)
ids_action_item.SetItem(ll_row,'expedited_service_flag','N')
ids_action_item.SetItem(ll_row,'auto_created_flag','N')


ids_action_item.SetItem(ll_row,'claim_no',il_claim_no)
ids_action_item.SetItem(ll_row,'opening_no',il_opening_no)
ids_action_item.SetItem(ll_row,'task_type_code','AC')
ids_action_item.SetItem(ll_row,'task_sub_type_code',as_sub_type_code)
ids_action_item.SetItem(ll_row,'task_specific_code',as_specific_code)
ids_action_item.SetItem(ll_row,'comment',as_comment)
ids_action_item.SetItem(ll_row,'task_status_code',as_status_code)

IF as_status_code = '01' Then
	ids_action_item.SetItem(ll_row,'planned_start_date',date(adt_status_date))
	ids_action_item.SetItem(ll_row,'planned_completion_date',adt_status_date)
Else
	ids_action_item.SetItem(ll_row,'actual_start_date',adt_status_date)
	ids_action_item.SetItem(ll_row,'actual_completion_date',adt_status_date)
End if

ids_action_item.SetItem(ll_row,'task_success_code','I')
ids_action_item.SetItem(ll_row,'responsible_user_id',as_responsible_user_id)


return 1

end function

on n_action_item.create
call super::create
end on

on n_action_item.destroy
call super::destroy
end on

event constructor;call super::constructor;idt_today = DateTime(Date(f_server_datetime()))

ids_action_item = CREATE u_ds
ids_action_item.dataobject = 'd_rehab_task_action_item'
ids_action_item.SetTransObject(SQLCA)


ids_related_action_items= create u_ds
ids_related_action_items.DataObject = 'd_rehab_related_outstanding_action_items'
ids_related_action_items.SetTransObject(SQLCA)

ids_progress_note = CREATE u_ds
ids_progress_note.DataObject = 'd_rehab_task_action_item_progress_note'
ids_progress_note.SetTransObject(SQLCA)

inv_working_folders = CREATE n_working_folders
inv_working_folders.nf_set_commit(True)


end event

