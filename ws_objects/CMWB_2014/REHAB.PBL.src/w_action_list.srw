$PBExportHeader$w_action_list.srw
forward
global type w_action_list from w_a_tool
end type
type dw_action_search from u_dw_online within w_action_list
end type
type dw_details from u_dw_online within w_action_list
end type
type cb_search from commandbutton within w_action_list
end type
type cb_clear from commandbutton within w_action_list
end type
end forward

global type w_action_list from w_a_tool
integer width = 2674
string title = ""
boolean resizable = false
event ue_post_open ( )
dw_action_search dw_action_search
dw_details dw_details
cb_search cb_search
cb_clear cb_clear
end type
global w_action_list w_action_list

type variables
STRING is_SQLSyntax,is_WhereClause

end variables

forward prototypes
public function integer wf_validate_query ()
public function integer wf_refresh_search_results ()
public function integer wf_set_query ()
end prototypes

event ue_post_open();INTEGER			li_error
long           ll_found
STRING         ls_user_id



/* grab the user_id from the user_profile structure */
ls_user_id = vgst_user_profile.user_id

/* grab the datawindow child so that we can do a search on it */
/* find what we considewr to be the default and scroll to it Default	'01' (planned status)- */
/* default the claim manager to the one doing the search */
dw_action_search.setitem(1,'status','01')
dw_action_search.setitem(1,'claim_manager',ls_user_id)



/* set the defaults */
dw_action_search.setitem(dw_action_search.getrow(),"action_to",date(f_server_datetime()))

/* grab the origional select from the search result datawindow */
is_SQLSyntax = dw_details.GetSQLSelect()

/* make sure we have a valid value for the search */
IF TRIM(is_SQLSyntax) < "" THEN 
	messagebox("Application Error", "The search failed to retrieve the SQL from the result window")
END IF 





end event

public function integer wf_validate_query ();/*
Searching
	Select action items from the REHAB_TASK table with a task_type_code = 'AI' 
	and append the search criteria the user specified.  

An error message will appear if no criteria has been entered.  
An error message will appear if the from date is after the to date and vice versa. 
After a search has been aborted because of an error, 
search results list should remain unchanged.
*/

/* grab the values from the search datawindow */
DATE    ldt_from,ldt_to
STRING  ls_status,ls_claim_manager,ls_responsible_user, ls_admin_region_code
LONG    ll_claim_no

/* accept the values entered by the user */
dw_action_search.accepttext()

ldt_from            = dw_action_search.getitemdate(1,"action_from")
ldt_to              = dw_action_search.getitemdate(1,"action_to")
ls_status           = dw_action_search.getitemstring(1,"status")
ls_claim_manager    = dw_action_search.getitemstring(1,"claim_manager")
ls_responsible_user = dw_action_search.getitemstring(1,"responsible_user")
ll_claim_no         = dw_action_search.getitemnumber(1,"claim_no")
ls_admin_region_code = dw_action_search.GetItemString(1,"admin_region_code")

/* do the validations */
IF IsNull(ls_claim_manager) AND IsNull(ls_responsible_user) &
	AND (ISNULL(ll_claim_no) or ll_claim_no = 0 ) AND IsNull(ls_admin_region_code) THEN
	messagebox("Search Criteria Required","Atleast one of the following search criteria is required.~r~n'Claim Manager', 'Responsible User', 'Claim No', 'Region'.")
	RETURN -1 
END IF 

IF ldt_from > ldt_to THEN 
	messagebox("Search Criteria Error","The search TO date must be later than the search From date")
	RETURN -1 
END IF 

RETURN 1
end function

public function integer wf_refresh_search_results ();
wf_set_query()

	
return 1
end function

public function integer wf_set_query ();/* SQL used in datawindow

SELECT REHAB_TASK.claim_no,
       REHAB_TASK.comment,
       REHAB_TASK.task_no,
       REHAB_TASK.task_status_code,
       Task_Sub_Type.task_sub_type_desc,
       INDIVIDUAL_NAME.last_name + "," + INDIVIDUAL_NAME.given_names,
       CLAIM.claim_manager_user_id,
        CASE REHAB_TASK.task_status_code
          WHEN '01' THEN REHAB_TASK.planned_start_date
		 ELSE REHAB_TASK.actual_completion_date
		 END AS 'rehab task date'    ,
       User_Profile.user_last_name + ',' + User_Profile.user_first_name   
 FROM REHAB_TASK, Task_Sub_Type,Task_Status,INDIVIDUAL_NAME,CLAIM, User_Profile
WHERE INDIVIDUAL_NAME.individual_no   = CLAIM.individual_no
  AND REHAB_TASK.claim_no             = CLAIM.claim_no
  AND REHAB_TASK.responsible_user_id  = User_Profile.user_id                
  AND (REHAB_TASK.task_type_code      = Task_Sub_Type.task_type_code
  AND REHAB_TASK.task_sub_type_code   = Task_Sub_Type.task_sub_type_code)

grab the values that we will need in order to construct our return 
claim_manager
status
claim_no
from_date
to_date
responsible_user
*/

LONG     	ll_claim_no
STRING   	ls_claim_manager, ls_responsible_user, ls_status, ls_whereclause, ls_from_filter, ls_to_filter, ls_filter
STRING   	ls_additional_whereclause, ls_modstring, ls_sqlsyntax, ls_returncode, ls_admin_region_code
DATE     	ldt_from,	ldt_to
INTEGER  	li_error, li_rows, li_rtn = 1

/* SET THE POINTER TO AN HOURGLASS */
setpointer(hourglass!)

dw_details.SetRedraw(false)

dw_details.settransobject(sqlca)

/* grab the values we need */
ls_claim_manager    		= dw_action_search.getitemstring(1,"claim_manager")
ls_responsible_user 		= dw_action_search.getitemstring(1,"responsible_user")
ls_status           			= dw_action_search.getitemstring(1,"status")
ll_claim_no         			= dw_action_search.getitemnumber(1,"claim_no")
ldt_from            			= dw_action_search.getitemdate(1,"action_from")
ldt_to              				= dw_action_search.getitemdate(1,"action_to")
ls_admin_region_code 	= dw_action_search.GetITemString(1,"admin_region_code")

/* set the default where clause this will always be there */
ls_Additional_WhereClause = ''

/* grab the claim manager information if there is any */
IF trim(ls_claim_manager)  > "" THEN
	ls_Additional_WhereClause = ls_Additional_WhereClause + " and (CLAIM.claim_manager_user_id = " + "'" + ls_claim_manager + "'" + ")"
END IF

/* grab the responsible user information if there is any */
IF trim(ls_responsible_user)   > "" THEN
	ls_Additional_WhereClause = ls_Additional_WhereClause + " and (a.responsible_user_id = " + "'" + ls_responsible_user + "'" + ")"
END IF

/* grab the status information if there is any */
IF trim(ls_status)   > "" THEN
	ls_Additional_WhereClause = ls_Additional_WhereClause + " and (a.task_status_code = " + "'" + ls_status + "'" + ")"
END IF

/* grab the claim information if there is any */
IF NOT ISNULL(ll_claim_no) AND ll_claim_no > 0 THEN
	ls_Additional_WhereClause = ls_Additional_WhereClause + " and (a.claim_no = " + string(ll_claim_no) + ")"
END IF

/* grab the admin region information if there is any */
IF Trim(ls_admin_region_code) > "" THEN
	ls_Additional_WhereClause = ls_Additional_WhereClause + " and (CLAIM.admin_region_code = '" + ls_admin_region_code  + "')"
END IF

/*		Now, build the rest of the where string to be appended to the basic where clause */
/*		Modify the data window's syntax.....   */
dw_details.setsqlselect(trim(is_SQLSyntax) + ls_Additional_WhereClause)

/* Remove the old filter before calling retrieve  because it will affect li_rows*/
dw_details.SetFilter('')
dw_details.Filter()
dw_details.Reset()

/* retrieve the data */
li_rows = dw_details.retrieve()
SQLCA.nf_handle_error("w_action_list","wf_set_query()","dw_details.retrieve()") 

IF li_rows > 0 THEN 

	/* now because we didn't put this in the select we need to filter the 
		information returned in the datawindow - this is only applicable
		if the user has added from and to dates.
	*/
	
	IF NOT ISNULL(ldt_from)  THEN
		ls_from_filter = "Date(status_date) > Date('" + string(ldt_from) + "')" 
	End if
	
	If NOT ISNULL(ldt_to) THEN 
		If ls_from_filter <> '' Then 
			ls_to_filter = " and "
		End if
		ls_to_filter += "Date(status_date) <= Date('"+ string(ldt_to) +"')"       
	End if
	
	ls_filter = ls_from_filter + ls_to_filter
	If ls_filter <> '' THen    
		dw_details.SetFIlter("task_status_desc = '-999'")
		dw_details.Filter()
		
		dw_details.SetFilter(ls_filter)		
		dw_details.Filter()
	End if
	
Else
	li_rtn = -1	
END IF

dw_details.SetRedraw(true)

RETURN li_rtn







end function

on w_action_list.create
int iCurrent
call super::create
this.dw_action_search=create dw_action_search
this.dw_details=create dw_details
this.cb_search=create cb_search
this.cb_clear=create cb_clear
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_action_search
this.Control[iCurrent+2]=this.dw_details
this.Control[iCurrent+3]=this.cb_search
this.Control[iCurrent+4]=this.cb_clear
end on

on w_action_list.destroy
call super::destroy
destroy(this.dw_action_search)
destroy(this.dw_details)
destroy(this.cb_search)
destroy(this.cb_clear)
end on

event open;call super::open;DATAWINDOWCHILD	ldwc_child
STRING			ls_columns[4] = {'claim_manager','responsible_user','admin_region_code','status'}
INTEGER			li_x
LONG				ll_row

dw_details.uf_SetSort(True)

/* set the transaction object and insert a row */
dw_action_search.settransobject(sqlca)
dw_action_search.insertrow(0)

/* insert empty rows so the user can choose not to include certain criteria*/
For li_x = 1 To UpperBound(ls_columns)
	dw_action_search.GetChild(ls_columns[li_x],ldwc_child)
	If ls_columns[li_x] = 'responsible_user' Then
		If ldwc_child.SetSort('computed_user_full_name A') = -1 Then SignalError(-666,'Error setting sort criteria')
		If ldwc_child.Sort() = -1 Then SignalError(-666,'Error sorting responsible_user')
	End if
	ll_row = ldwc_child.InsertRow(1)
Next

/* post this event */
this.postevent("ue_post_open")
end event

type st_title from w_a_tool`st_title within w_action_list
integer x = 5
string text = "Action List"
end type

type cb_close from w_a_tool`cb_close within w_action_list
integer x = 2258
integer y = 1704
end type

event cb_close::clicked;call super::clicked;close(w_action_list)
end event

type dw_action_search from u_dw_online within w_action_list
integer x = 5
integer y = 96
integer width = 2629
integer height = 320
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_action_list_search"
borderstyle borderstyle = styleraised!
end type

event itemerror;return 2
end event

type dw_details from u_dw_online within w_action_list
integer x = 9
integer y = 432
integer width = 2615
integer height = 1252
integer taborder = 11
boolean bringtotop = true
string dataobject = "d_search_results"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;call super::rowfocuschanged;LONG			ll_claim_no
LONG			ll_basic_claim_no

This.selectrow(0, false)
this.selectrow(currentrow,true)


If currentrow > 0 Then	
	ll_claim_no = dw_details.GetItemNumber(currentrow,'claim_no')
	ll_basic_claim_no = iw_active_sheet.dw_basic_claim.GetItemNumber(1,'claim_no') 
	if IsNull(ll_basic_claim_no) or ll_claim_no <> ll_basic_claim_no Then
		iw_active_sheet.wf_set_claim(ll_claim_no)
	End if
End if
end event

event doubleclicked;call super::doubleclicked;//LONG			ll_claim_no
//INTEGER     li_row
//
///* check to see that we have a valid rowcount */
//IF THIS.rowcount() < 1 THEN RETURN -1
//
///* make sure we have a valid row */
//li_row = THIS.getrow()
//IF li_row < 1 THEN RETURN -1
//
///* there is a claim number so grab it and refresh the window */
//ll_claim_no = this.getitemnumber(li_row,"claim_no")
//
///* make sure we have a valid claim number */
//IF ll_claim_no < 1 THEN RETURN -1
//
///* everything is valid refresh the tombstone and the details section */
//w_rehab_sheet.iw_passedwindow.wf_set_claim(ll_claim_no)

end event

event rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup
/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	lm_popup.m_options.m_sort.visible = TRUE
	lm_popup.m_options.m_filterlist.visible = False
	lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))

	Destroy lm_popup
end event

event ue_filter;call super::ue_filter;/* if activated this will bring up the default powerbuilder datawindow */

STRING ls_null_string

SetNull(ls_null_string)
THIS.SetFilter(ls_null_string)
THIS.Filter()
end event

event ue_print;DATASTORE lds_text

lds_text = CREATE DATASTORE

lds_text.dataobject = 'd_search_results_color'
lds_text.settransobject(sqlca)
dw_details.sharedata(lds_text)

lds_text.Print()
end event

type cb_search from commandbutton within w_action_list
integer x = 2281
integer y = 160
integer width = 315
integer height = 84
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Search"
boolean default = true
end type

event clicked;/* set the hourglass */
setpointer(hourglass!)

/* messagebox this out if not handled in function */
/* validate the information accepted by the users for the search */
IF wf_validate_query() < 1 THEN RETURN -1

/* set the query and retrieve the required information */
IF wf_set_query() < 1 THEN RETURN -1
end event

type cb_clear from commandbutton within w_action_list
integer x = 2281
integer y = 260
integer width = 315
integer height = 84
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "C&lear"
end type

event clicked;/* reset the search datawindows */
dw_action_search.reset()
dw_details.reset()
dw_action_search.insertrow(0)

/* trigger the ue_post_open event to reset the values */
parent.triggerevent("ue_post_open")
end event

