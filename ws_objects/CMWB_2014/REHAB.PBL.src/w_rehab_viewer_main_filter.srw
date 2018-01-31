$PBExportHeader$w_rehab_viewer_main_filter.srw
forward
global type w_rehab_viewer_main_filter from window
end type
type cb_cancel from commandbutton within w_rehab_viewer_main_filter
end type
type cb_ok from commandbutton within w_rehab_viewer_main_filter
end type
type dw_rehab_viewer_main_filter from u_dw_online within w_rehab_viewer_main_filter
end type
end forward

shared variables

end variables

global type w_rehab_viewer_main_filter from window
integer x = 1289
integer y = 316
integer width = 2606
integer height = 1408
boolean titlebar = true
string title = "Rehab Viewer Filter Criteria Selection"
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
cb_cancel cb_cancel
cb_ok cb_ok
dw_rehab_viewer_main_filter dw_rehab_viewer_main_filter
end type
global w_rehab_viewer_main_filter w_rehab_viewer_main_filter

type variables
LONG			il_rehab_filter_cnt
DATAWINDOWCHILD	idw_child
STRING			is_return_filter

s_filter_state	str_state
s_window_message 	istr_window_message


end variables

forward prototypes
public function integer wf_create_rehab_filter ()
end prototypes

public function integer wf_create_rehab_filter ();/*	This function creates the filter to be returned back to the rehab sheet's rehab viewer.
*/
	STRING	ls_main_filter_choice,ls_sub_filter_choice, ls_string
	DATETIME	ldt_from_datetime, ldt_to_datetime
	string ls_main_filter, ls_sub_filter
	string ls_task_type_code, ls_task_sub_type_code, ls_task_specific_code
	INTEGER li_ret_code
	DATAWINDOWCHILD ldwc_child
		
	SetPointer(HourGlass!)
	
	IF dw_rehab_viewer_main_filter.AcceptText() = -1 THEN
		is_return_filter = 'CANCEL'
		RETURN 0
	END IF

//	Determine which main filter criteria choice was selected.
	ls_main_filter_choice = dw_rehab_viewer_main_filter.GetItemString(1,"main_filter_choice")

// Build the main filter section of the filter string to be returned.
	CHOOSE CASE ls_main_filter_choice
		CASE "MA"	//Tasks and Action Items
			/*Selects all tasks and action items excluding follow-ups and including Progress Notes.
 			Follow-ups included if the checkbox has been set by the user.
			 Progress Notes excluded if checkbox is un-checked.*/
			ls_main_filter="(action_item_entry_flag='Y' or task_entry_flag='Y')"
		CASE "MB"	//Medical Management
			/*Selects all tasks and action items where the Task_Specific.medical_management_flag="Y"
 			*/
			ls_main_filter="medical_management_flag='Y'"
		CASE "MC"	//Tasks Only
			/*Selects all tasks that were entered using the Task tab i.e. Task_Specific.task_entry_flag="Y"
 			*/
			ls_main_filter="task_entry_flag='Y'"
		CASE "MD"	// Action Items Only
			/*Selects all action items excluding follow-ups that were entered using the Action Item tab i.e. Task_Type.action_item_entry_flag="Y"
 			Follow-ups included if the checkbox has been set by the user.*/
			ls_main_filter="action_item_entry_flag='Y'"
		CASE "ME"	//Reset to Default Filter
			ls_main_filter="(action_item_entry_flag='Y' or task_entry_flag='Y')"
			ls_sub_filter=""
			dw_rehab_viewer_main_filter.SetItem(1,'progress_notes_flag',"Y")
			dw_rehab_viewer_main_filter.SetItem(1,'followups',"N")
			
		CASE ELSE

/*	Remove current filter.
*/
			ls_main_filter = ""

	END CHOOSE
	
	//	Determine which sub filter criteria choice was selected.
		
	ls_sub_filter_choice = dw_rehab_viewer_main_filter.GetItemString(1,"sub_filter_choice")
	
	// Build the sub filter section of the filter string to be returned.	
	CHOOSE CASE ls_sub_filter_choice
		CASE "SA"		//Type
			if ls_main_filter_choice <> "MB" then
				ls_task_type_code=dw_rehab_viewer_main_filter.GetItemString(1,'task_type_code')
				ls_task_sub_type_code=dw_rehab_viewer_main_filter.GetItemString(1,'task_sub_type_code')
			else
				ls_task_type_code=dw_rehab_viewer_main_filter.GetItemString(1,'mm_task_type_code')
				ls_task_sub_type_code=dw_rehab_viewer_main_filter.GetItemString(1,'mm_task_sub_type_code')
			end if
			ls_task_specific_code=dw_rehab_viewer_main_filter.GetItemString(1,'task_specific_code')
			
			if not isnull(ls_task_type_code) and ls_task_type_code<>"" then
				ls_sub_filter=" and task_type_code='" + ls_task_type_code + "'"
				if not isnull(ls_task_sub_type_code) and ls_task_sub_type_code<>"" and ls_task_sub_type_code<>"." then
					ls_sub_filter=ls_sub_filter + " and task_sub_type_code='" + ls_task_sub_type_code + "'"
					if not isnull(ls_task_specific_code) and ls_task_specific_code<>"" and ls_task_specific_code<>"." then
						ls_sub_filter=ls_sub_filter + " and task_specific_code='" + ls_task_specific_code + "'"
					end if
				end if
			else
				ls_sub_filter=""
			end if
			
		CASE "SB"		//Date
			ldt_from_datetime = dw_rehab_viewer_main_filter.GetItemDatetime(1,'start_date_from')
			ldt_to_datetime = dw_rehab_viewer_main_filter.GetItemDatetime(1,'start_date_to')
			IF (ldt_to_datetime < ldt_from_datetime) then
				MessageBox("Invalid Date","The 'To:' date must be after or on the 'From:' date.")
				dw_rehab_viewer_main_filter.SetColumn('start_date_to')
				dw_rehab_viewer_main_filter.SetFocus()
				ls_sub_filter = ""
				RETURN -1
			elseif (IsNull(ldt_from_datetime) and IsNull(ldt_to_datetime)) THEN
				MessageBox("Invalid Date","A date must be entered into the 'From:' or 'To:'.")
				dw_rehab_viewer_main_filter.SetColumn('start_date_from')
				dw_rehab_viewer_main_filter.SetFocus()
				ls_sub_filter = ""
				RETURN -1
			ELSE
				
				IF (not IsNull(ldt_to_datetime) and IsNull(ldt_from_datetime)) THEN
					ls_sub_filter = " and (planned_start_date <= " 
					ls_sub_filter = ls_sub_filter + String(ldt_to_datetime,'YYYY-MM-DD') 
					ls_sub_filter = ls_sub_filter +" or actual_start_date<= " 
					ls_sub_filter = ls_sub_filter + String(ldt_to_datetime,'YYYY-MM-DD') +")"
					
				elseif (not IsNull(ldt_to_datetime) and not IsNull(ldt_from_datetime)) then
					ls_sub_filter = " and ((planned_start_date >= " 
					ls_sub_filter = ls_sub_filter + String(ldt_from_datetime,'YYYY-MM-DD') 
					ls_sub_filter = ls_sub_filter + " and planned_start_date <= " 
					ls_sub_filter = ls_sub_filter + String(ldt_to_datetime,'YYYY-MM-DD') +") or "
					ls_sub_filter = ls_sub_filter +" (actual_start_date >= " 
					ls_sub_filter = ls_sub_filter + String(ldt_from_datetime,'YYYY-MM-DD') 
					ls_sub_filter = ls_sub_filter + " and actual_start_date <= " 
					ls_sub_filter = ls_sub_filter + String(ldt_to_datetime,'YYYY-MM-DD') +"))"
					
				elseif (IsNull(ldt_to_datetime) and not IsNull(ldt_from_datetime)) then 
					ls_sub_filter = " and (planned_start_date >= " 
					ls_sub_filter = ls_sub_filter + String(ldt_from_datetime,'YYYY-MM-DD') 
					ls_sub_filter = ls_sub_filter +" or actual_start_date>= " 
					ls_sub_filter = ls_sub_filter + String(ldt_from_datetime,'YYYY-MM-DD') +")"

				END IF
				
			END IF

		CASE "SC"		//Responsible User ID
			ls_sub_filter= ""
			ls_string=dw_rehab_viewer_main_filter.GetItemString(1,'responsible_user_id')
			if isnull(ls_string) or ls_string="." then
				MessageBox("Invalid Responsible User ID","No resonsible users found to select.")
				ls_sub_filter=""
				//RETURN -1
			else
				ls_sub_filter=" and responsible_user_id='" + ls_string + "'"
			end if

		CASE "SD"		//Task Status
			ls_string=dw_rehab_viewer_main_filter.GetItemString(1,'task_status')
			if isnull(ls_string) then
				MessageBox("Invalid Task Status","A valid Task Status must be selected.")
				ls_sub_filter=""
				//RETURN -1
			else
				ls_sub_filter=" and task_status='" + ls_string + "'"
			end if
			
		CASE "SE"		//Remove Current Sub-filter
			ls_sub_filter=""
			
		CASE ELSE

/*	Remove current filter.
*/
		ls_sub_filter = ""

	END CHOOSE


/*Build the required filter string to be used to filter the viewer display
*/
	if ls_main_filter<>"" then
		if ls_sub_filter<>"" then
			is_return_filter=ls_main_filter + ls_sub_filter	
		else
			is_return_filter=ls_main_filter
		end if
	else
		if ls_sub_filter<>"" then
			is_return_filter=ls_sub_filter
		else
			is_return_filter=""
		end if
	end if

/*Handle the Progress Notes dependant on main filter choice seelction
*/
if dw_rehab_viewer_main_filter.GetItemString(1,'progress_notes_flag') <>"Y" then
	is_return_filter= is_return_filter + " and progress_note_flag='N'"
end if

//Added by Roland Baker December 11, 2002
/*Handle the Follow ups dependant on main filter choice seelction
*/
if dw_rehab_viewer_main_filter.GetItemString(1,'followups') <>"Y" then
	is_return_filter= is_return_filter + " and follow_up_flag='N'"
end if
//End Add by Roland Baker

	
	RETURN 0
end function

on w_rehab_viewer_main_filter.create
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.dw_rehab_viewer_main_filter=create dw_rehab_viewer_main_filter
this.Control[]={this.cb_cancel,&
this.cb_ok,&
this.dw_rehab_viewer_main_filter}
end on

on w_rehab_viewer_main_filter.destroy
destroy(this.cb_cancel)
destroy(this.cb_ok)
destroy(this.dw_rehab_viewer_main_filter)
end on

event open;/*This window is used to filter the contents of the Task/Progress Tab selection display.
It is called from the following locations in the application:
	 w_rehab_sheet.ue_filter  library:REHAB.PBL 
*/

str_state = message.PowerObjectParm

LONG	ll_rows_loaded
LONG il_claim_no
DATAWINDOWCHILD ldwc_child

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


il_claim_no=str_state.claim_no

if str_state.follow_ups_included="" then str_state.follow_ups_included="N"
if str_state.progress_notes_included="" then str_state.progress_notes_included="Y"

SetPointer(HourGlass!)

w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.Visible = TRUE

//MOve the Medical Management drop downs to the same location as those required for
//the Tasks and Action Items
w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.object.mm_task_type_code.X=w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.object.task_type_code.X
w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.object.mm_task_type_code.Y=w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.object.task_type_code.Y
w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.object.mm_task_type_code.Width=w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.object.task_type_code.Width
w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.object.mm_task_type_code.height=w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.object.task_type_code.height

w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.object.mm_task_sub_type_code.X=w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.object.task_sub_type_code.X
w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.object.mm_task_sub_type_code.Y=w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.object.task_sub_type_code.Y
w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.object.mm_task_sub_type_code.Width=w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.object.task_sub_type_code.Width
w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.object.mm_task_sub_type_code.height=w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.object.task_sub_type_code.height

/* Set up default search by option.
*/
	w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.InsertRow(0)

/* Load the responsible users drop down datawindow.
*/
	
	w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.GetChild("responsible_user_id",idw_child)
	idw_child.SetTransObject(SQLCA)
	ll_rows_loaded = idw_child.Retrieve(il_claim_no)
	IF SQLCA.nf_handle_error("idw_child.Retrieve()","w_rehab_viewer_main_filter","Open Event") < 0 THEN
		CloseWithReturn(THIS,'CANCEL')
		RETURN
	END IF
	
	if ll_rows_loaded<=0 then
		idw_child.InsertRow(0)
		w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.setItem(1,"responsible_user_id",".")
	end if

/* Load the task and medical management type drop down datawindows.
*/
	w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.GetChild("task_type_code",idw_child)
	idw_child.SetTransObject(SQLCA)
	ll_rows_loaded = idw_child.Retrieve()
	IF SQLCA.nf_handle_error("idw_child.Retrieve()","w_rehab_viewer_main_filter","Open Event") < 0 THEN
		CloseWithReturn(THIS,'CANCEL')
		RETURN
	END IF

	IF ll_rows_loaded <= 0 THEN
	   MessageBox("Rehab Filter","No task types found for the system.",StopSign!,OK!)
		CloseWithReturn(THIS,'CANCEL')
		RETURN
	END IF

	w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.GetChild("mm_task_type_code",idw_child)
	idw_child.SetTransObject(SQLCA)
	ll_rows_loaded = idw_child.Retrieve()
	IF SQLCA.nf_handle_error("idw_child.Retrieve()","w_rehab_viewer_main_filter","Open Event") < 0 THEN
		CloseWithReturn(THIS,'CANCEL')
		RETURN
	END IF

	IF ll_rows_loaded <= 0 THEN
	   MessageBox("Rehab Filter","No Medical Management related task types found for the system.",StopSign!,OK!)
		CloseWithReturn(THIS,'CANCEL')
		RETURN
	END IF


/* Load the task and medical management sub type drop down datawindows.
*/
	w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.GetChild("task_sub_type_code",idw_child)
	idw_child.SetTransObject(SQLCA)
	ll_rows_loaded = idw_child.Retrieve()
	IF SQLCA.nf_handle_error("idw_child.Retrieve()","w_rehab_viewer_main_filter","Open Event") < 0 THEN
		CloseWithReturn(THIS,'CANCEL')
		RETURN
	END IF
	
	IF ll_rows_loaded <= 0 THEN
	   MessageBox("Rehab Filter","No task sub types found for the system.",StopSign!,OK!)
		CloseWithReturn(THIS,'CANCEL')
		RETURN
	END IF
	
	w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.GetChild("mm_task_sub_type_code",idw_child)
	idw_child.SetTransObject(SQLCA)
	ll_rows_loaded = idw_child.Retrieve()
	IF SQLCA.nf_handle_error("idw_child.Retrieve()","w_rehab_viewer_main_filter","Open Event") < 0 THEN
		CloseWithReturn(THIS,'CANCEL')
		RETURN
	END IF
	
	IF ll_rows_loaded <= 0 THEN
	   MessageBox("Rehab Filter","No Medical Management related task sub types found for the system.",StopSign!,OK!)
		CloseWithReturn(THIS,'CANCEL')
		RETURN
	END IF
	
/* Load the task specific drop down datawindow.
*/
	w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.GetChild("task_specific_code",idw_child)
	idw_child.SetTransObject(SQLCA)
	ll_rows_loaded = idw_child.Retrieve()
	IF SQLCA.nf_handle_error("idw_child.Retrieve()","w_rehab_viewer_main_filter","Open Event") < 0 THEN
		CloseWithReturn(THIS,'CANCEL')
		RETURN
	END IF

	IF ll_rows_loaded <= 0 THEN
	   MessageBox("Rehab Filter","No task specifics found for the system.",StopSign!,OK!)
		CloseWithReturn(THIS,'CANCEL')
		RETURN
	END IF

/*	Load the task status drop down datawindow.
*/
	w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.GetChild("task_status",idw_child)
	idw_child.SetTransObject(SQLCA)
	ll_rows_loaded = idw_child.Retrieve()
	IF SQLCA.nf_handle_error("idw_child.Retrieve()","w_rehab_viewer_main_filter","Open Event") < 0 THEN
		CloseWithReturn(THIS,'CANCEL')
		RETURN
	END IF

	IF ll_rows_loaded <= 0 THEN
	   MessageBox("Rehab Filter","No task status's found for the system.",StopSign!,OK!)
		CloseWithReturn(THIS,'CANCEL')
		RETURN
	END IF

/*Fill filter display with pre-sets stored in the s_filter_state global structure variable.
*/
if str_state.main_filter_choice<>"" then w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.SetItem(1,"main_filter_choice",str_state.main_filter_choice)
if str_state.sub_filter_choice<>"" then w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.SetItem(1,"sub_filter_choice",str_state.sub_filter_choice)

if str_state.dddw_task_type_filter<>"" then
	dw_rehab_viewer_main_filter.GetChild('task_type_code',ldwc_child)
	ldwc_child.SetFilter(str_state.dddw_task_type_filter)
	ldwc_child.Filter()
end if
if str_state.dddw_mm_task_type_filter<>"" then
	dw_rehab_viewer_main_filter.GetChild('mm_task_type_code',ldwc_child)
	ldwc_child.SetFilter(str_state.dddw_mm_task_type_filter)
	ldwc_child.Filter()
end if

if str_state.task_type_code<>"" then 
	if str_state.main_filter_choice="MB" then
		w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.SetItem(1,"mm_task_type_code",str_state.task_type_code)
	else
		w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.SetItem(1,"task_type_code",str_state.task_type_code)
	end if
end if

if str_state.dddw_task_sub_type_filter<>"" then
	dw_rehab_viewer_main_filter.GetChild('task_sub_type_code',ldwc_child)
	ldwc_child.SetFilter(str_state.dddw_task_sub_type_filter)
	ldwc_child.Filter()
end if
if str_state.dddw_mm_task_sub_type_filter<>"" then
	dw_rehab_viewer_main_filter.GetChild('mm_task_sub_type_code',ldwc_child)
	ldwc_child.SetFilter(str_state.dddw_mm_task_sub_type_filter)
	ldwc_child.Filter()
end if

if str_state.task_sub_type_code<>"" then 
	if str_state.main_filter_choice="MB" then
		w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.SetItem(1,"mm_task_sub_type_code",str_state.task_sub_type_code)
	else
		w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.SetItem(1,"task_sub_type_code",str_state.task_sub_type_code)
	end if
end if

if str_state.dddw_task_specific_filter<>"" then
	dw_rehab_viewer_main_filter.GetChild('task_specific_code',ldwc_child)
	ldwc_child.SetFilter(str_state.dddw_task_specific_filter)
	ldwc_child.Filter()
end if

if str_state.task_specific_code<>"" then 
		w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.SetItem(1,"task_specific_code",str_state.task_specific_code)
end if

if not isnull(str_state.start_date_from) then
	if	String(str_state.start_date_from,'YYYY-MM-DD') = "1900-01-01" then
		w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.SetItem(1,"start_date_from","0000-00-00")
	else
		w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.SetItem(1,"start_date_from",str_state.start_date_from)
	end if
end if	
if not isnull(str_state.start_date_to) then
	if	String(str_state.start_date_to,'YYYY-MM-DD') = "1900-01-01" then
		w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.SetItem(1,"start_date_to","0000-00-00")
	else
		w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.SetItem(1,"start_date_to",str_state.start_date_to)
	end if
end if	

if str_state.dddw_responsible_user_id_filter<>"" then
	dw_rehab_viewer_main_filter.GetChild('responsible_user_id',ldwc_child)
	ldwc_child.SetFilter(str_state.dddw_responsible_user_id_filter)
	ldwc_child.Filter()
end if

if str_state.responsible_user_id <>"" then
		w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.SetItem(1,"responsible_user_id",str_state.responsible_user_id)
end if

if str_state.dddw_task_status_filter<>"" then
	dw_rehab_viewer_main_filter.GetChild('task_status_code',ldwc_child)
	ldwc_child.SetFilter(str_state.dddw_task_status_filter)
	ldwc_child.Filter()
end if

if str_state.task_status <>"" then
		w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.SetItem(1,"task_status",str_state.task_status)
end if

if str_state.progress_notes_included<>"" then
	w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.SetItem(1,"progress_notes_flag",str_state.progress_notes_included)
end if

w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.SetItem(1,"followups",str_state.follow_ups_included)













end event

type cb_cancel from commandbutton within w_rehab_viewer_main_filter
integer x = 1303
integer y = 1208
integer width = 334
integer height = 100
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
end type

event clicked;str_state.filter_string="CANCEL"
CloseWithReturn(PARENT,str_state)
end event

type cb_ok from commandbutton within w_rehab_viewer_main_filter
integer x = 937
integer y = 1208
integer width = 334
integer height = 100
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
boolean default = true
end type

event clicked;INTEGER	li_result

SetPointer(HourGlass!)

li_result = wf_create_rehab_filter()

IF li_result = 0 THEN
	str_state.main_filter_choice=w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.GetItemString(1,"main_filter_choice")
	str_state.sub_filter_choice=w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.GetItemString(1,"sub_filter_choice")
	if str_state.main_filter_choice="MB" then
		str_state.task_type_code=w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.GetItemString(1,"mm_task_type_code")
		str_state.task_sub_type_code=w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.GetItemString(1,"mm_task_sub_type_code")
	else
		str_state.task_type_code=w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.GetItemString(1,"task_type_code")
		str_state.task_sub_type_code=w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.GetItemString(1,"task_sub_type_code")
	end if
	str_state.task_specific_code=w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.GetItemString(1,"task_specific_code")
	str_state.start_date_from=w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.GetItemDateTime(1,"start_date_from")
	str_state.start_date_to=w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.GetItemDateTime(1,"start_date_to")
	str_state.responsible_user_id=w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.GetItemString(1,"responsible_user_id")
	str_state.task_status=w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.GetItemString(1,"task_status")
	str_state.progress_notes_included=w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.GetItemString(1,"progress_notes_flag")
	str_state.follow_ups_included=w_rehab_viewer_main_filter.dw_rehab_viewer_main_filter.GetItemString(1,"followups")
	str_state.filter_string=is_return_filter
	
	CloseWithReturn(w_rehab_viewer_main_filter,str_state)
	
END IF

end event

type dw_rehab_viewer_main_filter from u_dw_online within w_rehab_viewer_main_filter
integer x = 27
integer y = 32
integer width = 2542
integer height = 1152
integer taborder = 10
string dataobject = "d_rehab_viewer_main_filter_list"
boolean border = false
boolean livescroll = false
end type

event itemchanged;call super::itemchanged;DATAWINDOWCHILD ldwc_child
INTEGER li_return
STRING ls_filter, ls_type_filter, ls_status_filter,ls_specific_filter
STRING ls_task_type_code, ls_task_sub_type_code, ls_task_specific_code

dw_rehab_viewer_main_filter.AcceptText()

CHOOSE CASE dwo.name
	CASE "main_filter_choice"
		//The user has selected a Main Filter Criteria radio button
		//Set the FollowUps included checkbox to the default value.
		li_return=dw_rehab_viewer_main_filter.SetItem(row,"followups","N")
		this.SetItem(1,"sub_filter_choice","")				
		if data="MB" then		//Medical Management radio button
			this.SetItem(1,'task_type_code','.')
			this.SetItem(1,'task_sub_type_code','.')
				//Populate the task_specific_code drop down datawindow with the allowable selections.
				ls_specific_filter="medical_management_flag='Y'"
				ls_type_filter=""
				ls_status_filter=""
				li_return = this.GetChild("task_specific_code",ldwc_child)
				if li_return<0 then
					messagebox("Error","Unable to get the task_specific_code child datawindow")
				else
					ldwc_child.SetFilter("")
					ldwc_child.Filter()
					ldwc_child.SetFilter(ls_specific_filter)
					ldwc_child.SetSort("task_specific_desc A")
					ldwc_child.Filter()
					ldwc_child.Sort()
					
					str_state.dddw_task_specific_filter=ls_specific_filter
					
				end if	
				//Initialize the task_type_code and task_sub_type_code drop 
				//down datawindows to default values
				this.SetItem(1,'task_specific_code','.')
				this.SetItem(1,'mm_task_type_code','.')
				this.SetItem(1,'mm_task_sub_type_code','.')
		else
			this.SetItem(1,'mm_task_type_code','.')
			this.SetItem(1,'mm_task_sub_type_code','.')
			if data="MA" then		//Tasks and Action Items
					ls_type_filter="(action_item_entry_flag='N' or action_item_entry_flag='Y')"
					ls_status_filter=""
					//ls_type_filter=""
			elseif data ="MC" then		//Tasks Only
					ls_type_filter="action_item_entry_flag='N'"
					ls_status_filter="task_entry_flag='Y'"
			elseif data="MD"	then		//Action Items Only
					ls_type_filter="action_item_entry_flag='Y'"
					ls_status_filter="action_item_entry_flag='Y'"
			elseif data= "ME" then		//Reset to Default Filter
//Modified by Roland Baker December 11, 2002
				//Added exclusion of followups
					ls_type_filter="(action_item_entry_flag='N' or action_item_entry_flag='Y') and follow_up_flag='N'"
//End Modification by Roland Baker December 11, 2002
					ls_status_filter=""
					this.SetItem(1,"sub_filter_choice","")
			END IF
			ls_type_filter = ls_type_filter + " and active_flag='Y'"
			/*Filter the task_type_code drop down datawindow to show 
			the appropriate selections
			*/
			li_return=dw_rehab_viewer_main_filter.GetChild("task_type_code",ldwc_child)
			//messagebox("itemchanged.ls_type_filter is ",ls_type_filter)  // for debugging only
			if li_return<0 then
				messagebox("Error","Problem Getting reference to child datawindow")
			else
				ldwc_child.SetFilter("")
				ldwc_child.Filter()
				ldwc_child.SetFilter(ls_type_filter)
				ldwc_child.SetSort("task_type_desc A")
				ldwc_child.Filter()
				ldwc_child.Sort()
				
				str_state.dddw_task_type_filter=ls_type_filter
				
				this.SetItem(1,'task_type_code','.')
				this.SetItem(1,'task_sub_type_code','.')
			end if
		END IF
		/*Filter the task_status drop down datawindow to show 
		the appropriate selections
		*/
		li_return=this.GetChild("task_status",ldwc_child)
		ldwc_child.SetFilter("")
		ldwc_child.Filter()
		ldwc_child.SetFilter(ls_status_filter)
		ldwc_child.SetSort("task_status_desc A")
		ldwc_child.Filter()
		ldwc_child.Sort()
				
		str_state.dddw_task_status_filter=ls_status_filter
		
	CASE "sub_filter_choice"
		/*The user has selected a Sub Filter Criteria radio button
		*/
		CHOOSE CASE data
			CASE "SA"		//Type
				/*The user has selected the Type option
				*/
				if this.getitemstring(1,"main_filter_choice")="MB" then		//Medical Management radio button
				//Populate the task_specific_code drop down datawindow with the allowable selections.
				ls_specific_filter="medical_management_flag='Y'"
				ls_type_filter=""
				ls_status_filter=""
				li_return = this.GetChild("task_specific_code",ldwc_child)
				if li_return<0 then
					messagebox("Error","Unable to get the task_specific_code child datawindow")
				else
					ldwc_child.SetFilter("")
					ldwc_child.Filter()
					ldwc_child.SetFilter(ls_specific_filter)
					ldwc_child.SetSort("task_specific_desc A")
					ldwc_child.Filter()
					ldwc_child.Sort()
					
					str_state.dddw_task_specific_filter=ls_specific_filter
					
				end if	
				//Initialize the task_type_code and task_sub_type_code drop 
				//down datawindows to default values
				this.SetItem(1,'task_specific_code','.')
				this.SetItem(1,'mm_task_type_code','.')
				this.SetItem(1,'mm_task_sub_type_code','.')
		else
			if this.getitemstring(1,"main_filter_choice")="MA" then		//Tasks and Action Items
					ls_type_filter="(action_item_entry_flag='N' or action_item_entry_flag='Y')"
					ls_status_filter=""
					//ls_type_filter=""
			elseif this.getitemstring(1,"main_filter_choice") ="MC" then		//Tasks Only
					ls_type_filter="action_item_entry_flag='N'"
					ls_status_filter="task_entry_flag='Y'"
			elseif this.getitemstring(1,"main_filter_choice")="MD"	then		//Action Items Only
					ls_type_filter="action_item_entry_flag='Y'"
					ls_status_filter="action_item_entry_flag='Y'"
			elseif this.getitemstring(1,"main_filter_choice")= "ME" then		//Reset to Default Filter
					ls_type_filter="(action_item_entry_flag='N' or action_item_entry_flag='Y')"
					ls_status_filter=""
					this.SetItem(1,"sub_filter_choice","")
			END IF
			ls_type_filter = ls_type_filter + " and active_flag='Y'"
			/*Filter the task_type_code drop down datawindow to show 
			the appropriate selections
			*/
			li_return=this.GetChild("task_type_code",ldwc_child)
			//messagebox("itemchanged.ls_type_filter is ",ls_type_filter) // for debugging only
			if li_return<0 then
				messagebox("Error","Problem Getting reference to child datawindow")
			else
				ldwc_child.SetFilter("")
				ldwc_child.Filter()
				ldwc_child.SetFilter(ls_type_filter)
				ldwc_child.SetSort("task_type_desc A")
				ldwc_child.Filter()
				ldwc_child.Sort()
				
				str_state.dddw_task_type_filter=ls_type_filter
				
				this.SetItem(1,'task_type_code','.')
				this.SetItem(1,'task_sub_type_code','.')
			end if
		END IF
			CASE "SB"		//Date
			CASE "SC"		//Responsible User ID
			CASE "SD"		//Task Status
//Added by Roland Baker December 11, 2002
				/*Filter the task_status drop down datawindow to show 
				the appropriate selections based on the Main Filter Choice option selected
				*/
				if this.getitemstring(1,"main_filter_choice") ="MC" then		//Tasks Only
					ls_status_filter="task_entry_flag='Y'"
				elseif this.getitemstring(1,"main_filter_choice")="MD"	then		//Action Items Only
					ls_status_filter="action_item_entry_flag='Y'"
				else
					ls_status_filter=""
				end if
				li_return=this.GetChild("task_status",ldwc_child)
				ldwc_child.SetFilter("")
				ldwc_child.Filter()
				ldwc_child.SetFilter(ls_status_filter)
				ldwc_child.SetSort("task_status_desc A")
				ldwc_child.Filter()
				ldwc_child.Sort()
//End Added by Roland Baker December 11, 2002
			CASE "SE"		//None
				this.setitem(row,"start_date_from","0000-00-00")
				this.setitem(row,"start_date_to","0000-00-00")
				this.setitem(row,"responsible_user_id","")
				this.setitem(row,"task_status","")
				this.setitem(row,"task_type_code",".")
				this.setitem(row,"task_sub_type_code",".")
				this.setitem(row,"mm_task_type_code",".")
				this.setitem(row,"mm_task_sub_type_code",".")
				this.setitem(row,"task_specific_code",".")
		END CHOOSE
		
	CASE "task_type_code"
		li_return = this.GetChild("task_type_code",ldwc_child)
		if li_return<0 then
			messagebox("Error","Unable to get the task_type_code child datawindow")
		else
			ls_task_type_code= this.GetItemString(row,"task_type_code",Primary!,false)
			if  ls_task_type_code="AC" then
				if dw_rehab_viewer_main_filter.GetItemString(1,"followups")="N" then
					ls_filter= "task_type_code='AC' and task_sub_type_code<>'153' and active_flag='Y'"
				else
					ls_filter= "task_type_code='AC' and active_flag='Y'"
				end if
			else
				ls_filter="task_type_code='" + ls_task_type_code  + "' and active_flag='Y'"				
			end if
			li_return = dw_rehab_viewer_main_filter.GetChild("task_sub_type_code",ldwc_child)
			if li_return<0 then
				messagebox("Error","Unable to get the task_sub_type_code child datawindow")
			else
				ldwc_child.SetFilter("")
				ldwc_child.Filter()
				ldwc_child.SetFilter(ls_filter)
				ldwc_child.SetSort("task_sub_type_desc A")
				ldwc_child.Filter()
				ldwc_child.Sort()
				
				str_state.dddw_task_sub_type_filter=ls_filter
				
			end if
			li_return = this.SetItem(1,'task_sub_type_code','.')
		end if
		
	CASE "task_sub_type_code"
		li_return = this.GetChild("task_sub_type_code",ldwc_child)
		if li_return<0 then
			messagebox("Error","Unable to get the task_sub_type_code child datawindow")
		else
			ls_task_type_code = this.GetItemString(row,"task_type_code",Primary!,false)			
			ls_task_sub_type_code = this.GetItemString(row,"task_sub_type_code",Primary!,false)
			ls_filter="task_type_code='" + ls_task_type_code + "' and task_sub_type_code='" + ls_task_sub_type_code + "' and active_flag='Y'"
			li_return = this.GetChild("task_specific_code",ldwc_child)
			if li_return<0 then
				messagebox("Error","Unable to get the task_specific_code child datawindow")
			else
				ldwc_child.SetFilter("")
				ldwc_child.Filter()
				ldwc_child.SetFilter(ls_filter)
				ldwc_child.SetSort("task_specific_desc A")
				ldwc_child.Filter()
				ldwc_child.Sort()
				
				str_state.dddw_task_specific_filter=ls_filter
				
			end if	
			this.SetItem(1,'task_specific_code','.')
		end if
	CASE "task_specific_code"
		
	CASE "mm_task_type_code"
		li_return = this.GetChild("mm_task_type_code",ldwc_child)
		if li_return<0 then
			messagebox("Error","Unable to get the mm_task_type_code child datawindow")
		else
			ls_task_type_code= this.GetItemString(row,"mm_task_type_code",Primary!,false)
			ls_filter="task_specific_task_type_code='" + ls_task_type_code  + "'"
			li_return = this.GetChild("mm_task_sub_type_code",ldwc_child)
			if li_return<0 then
				messagebox("Error","Unable to get the mm_task_sub_type_code child datawindow")
			else
				ldwc_child.SetFilter("")
				ldwc_child.Filter()
				ldwc_child.SetFilter(ls_filter)
				ldwc_child.SetSort("task_sub_type_task_sub_type_desc A")
				ldwc_child.Filter()
				ldwc_child.Sort()
				
				str_state.dddw_mm_task_sub_type_filter=ls_filter
				
			end if
			li_return = this.SetItem(1,'mm_task_sub_type_code','.')
			li_return=this.SetItem(1,'task_specific_code','.')
		end if
		
	CASE "mm_task_sub_type_code"
		li_return = this.GetChild("mm_task_sub_type_code",ldwc_child)
		if li_return<0 then
			messagebox("Error","Unable to get the mm_task_sub_type_code child datawindow")
		else
			ls_task_type_code = this.GetItemString(row,"mm_task_type_code",Primary!,false)			
			ls_task_sub_type_code = this.GetItemString(row,"mm_task_sub_type_code",Primary!,false)
			ls_filter="task_type_code='" + ls_task_type_code + "' AND task_sub_type_code='" + ls_task_sub_type_code + "' and active_flag='Y'"
			li_return = this.GetChild("task_specific_code",ldwc_child)
			if li_return<0 then
				messagebox("Error","Unable to get the task_specific_code child datawindow")
			else
				ldwc_child.SetFilter("")
				ldwc_child.Filter()
				ldwc_child.SetFilter(ls_filter)
				ldwc_child.SetSort("task_specific_desc A")
				ldwc_child.Filter()
				ldwc_child.Sort()
				
				str_state.dddw_task_specific_filter=ls_filter
				
			end if
			li_return=dw_rehab_viewer_main_filter.SetItem(1,'task_specific_code','.')
		end if
		
	CASE "followups"
		
		if data="N" then
			ls_filter= "task_type_code='AC' and task_sub_type_code<>'153' and active_flag='Y'"
		else
			ls_filter= "task_type_code='AC' and active_flag='Y'"
		end if
		li_return = this.GetChild("task_sub_type_code",ldwc_child)
		if li_return<0 then
			messagebox("Error","Unable to get the task_sub_type_code child datawindow")
		else
			ldwc_child.SetFilter("")
			ldwc_child.Filter()
			ldwc_child.SetFilter(ls_filter)
			ldwc_child.SetSort("task_sub_type_desc A")
			ldwc_child.Filter()
			ldwc_child.Sort()
	
			str_state.dddw_task_sub_type_filter=ls_filter
			
		end if
		li_return = this.SetItem(1,'task_sub_type_code','.')
		
	CASE "progress_notes_flag"
	CASE "task_status"
		
	CASE ELSE
		
END CHOOSE
end event

