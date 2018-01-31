$PBExportHeader$w_case_load.srw
forward
global type w_case_load from w_a_report
end type
type gb_1 from groupbox within w_case_load
end type
type dw_search_criteria from u_dw_online within w_case_load
end type
type cb_search from commandbutton within w_case_load
end type
type dw_search_type from u_dw_online within w_case_load
end type
end forward

global type w_case_load from w_a_report
integer width = 3291
integer height = 2736
string title = "Active Case Load Report"
boolean resizable = false
gb_1 gb_1
dw_search_criteria dw_search_criteria
cb_search cb_search
dw_search_type dw_search_type
end type
global w_case_load w_case_load

type variables
// General variables for User Object

String		  is_BasicClaimSyntax
String                      is_stats_syntax
String		  is_SQLSyntax		// Contains the SQL used to modify the select statement on d_claim_search
String		  is_WhereClause		// Contains standard where clause appended to end of SQL statement
String       is_from_clause
String         is_GroupBy


// Drop Down Data Windows

DataWindowChild	  idwc_claim_status_type_code
DataWindowChild	  idwc_claim_manager

BOOLEAN ib_force_plan

n_resize		inv_resize
end variables

forward prototypes
public function long wf_retrieve ()
public function integer wf_validate_criteria ()
public function integer wf_set_query ()
end prototypes

public function long wf_retrieve ();/*	This function retrieves d_search_list.
	It returns the number of rows retrieved.
	Set the .Redraw option to false while the datawindow is being
	loaded.
*/

	LONG	ll_ReturnCode

	IF ib_force_plan THEN
		EXECUTE IMMEDIATE 'set forceplan on' USING SQLCA;
		IF SQLCA.nf_handle_error('Setting force plan on', 'w_case_load', 'wf_retrieve') < 0 THEN
			Return -1
		END IF
	END IF
	ll_ReturnCode = dw_report.Retrieve()
	IF ib_force_plan THEN
		EXECUTE IMMEDIATE 'set forceplan off' USING SQLCA;
		IF SQLCA.nf_handle_error('Setting force plan off', 'w_case_load', 'wf_retrieve') < 0 THEN
			Return -1
		END IF
	END IF
	IF SQLCA.nf_handle_error("w_case_load","dw_report","wf_retrieve") < 0 Then
		Return -1
	END IF

	dw_report.SetFocus()

	Return ll_ReturnCode
end function

public function integer wf_validate_criteria ();STRING	ls_validate_string, ls_FieldValue

/*	
This function validates the search criteria.
It returns '1' if the search criteria is OK and -1 if there
are errors.   
*/

	ls_validate_string = dw_search_criteria.GetItemString(1,"region")
	IF IsNull(ls_validate_string) OR Trim(ls_validate_string) = "" THEN
		MessageBox("Invalid Search Criteria","You must provide a region!",Exclamation!)
		Return -1
	End IF

	
	ls_FieldValue = dw_search_criteria.GetItemString(1,"claim_status_code")
	IF ls_FieldValue <> 'N' THEN
		ls_validate_string = dw_search_criteria.GetItemString(1,"opening_type_code")
		IF IsNull(ls_validate_string) OR Trim(ls_validate_string) = "" THEN
			MessageBox("Invalid Search Criteria","You must provide an opening type!",Exclamation!)
			Return -1
		End IF
	END IF

Return 1
end function

public function integer wf_set_query ();/*	This function modifies the 'WHERE' clause based on the current search type and
	modIFies dw_search_list. It returns 1 IF successfull and -1 IF there were errors
*/
	STRING	ls_ReturnCode,	ls_Additional_WhereClause,	ls_ModString, ls_FieldValue, ls_SQLSyntax,	ls_WhereClause
	STRING   ls_from_clause, ls_header, ls_opening, ls_Additional_WhereClause_Union, ls_SQLSyntax_Union
	STRING ls_Additional_from_clause
	int li_pos

/*	Aug 28/96 the ib_force_plan  is used to force a query to use a better search plan
	one of the queries was causing a table scan and setting the set force plan on results in a 
	better performance
*/

	ib_force_plan = FALSE
	

/*		Take a copy of the basic SQL Syntax and the basic WHERE clause strings
*/
		ls_SQLSyntax     = is_SQLSyntax
		ls_WhereClause = is_WhereClause
		ls_from_clause   = is_from_clause

/*		this is used to better the performance of the query by SW region
*/
		ib_force_plan = TRUE


/*		If a rehab type desired, must add REHAB_GOAL table to the basic syntax
*/
		ls_FieldValue = dw_search_criteria.GetItemString(1, "rehab_type_code")
		IF Trim(ls_FieldValue) > "" THEN
			ls_from_clause = ls_from_clause + " INNER JOIN  REHAB_GOAL ON REHAB_GOAL.claim_no = CLAIM.claim_no"
			ls_Additional_WhereClause = ls_Additional_WhereClause + " and (REHAB_GOAL.rehab_type_code = ~~~""+ ls_FieldValue + "~~~")"
		END IF
		
		
		ls_FieldValue = dw_search_criteria.GetItemString(1,"user_id")
		IF ls_FieldValue  > "" THEN
			ls_Additional_WhereClause = ls_Additional_WhereClause + " and (CLAIM.claim_manager_user_id = ~~~"" + ls_FieldValue + "~~~")"
		END IF

		ls_FieldValue = dw_search_criteria.GetItemString(1,"comp_week_code")
		IF Trim(ls_FieldValue) > "" THEN
			ls_Additional_WhereClause = ls_Additional_WhereClause + " and (CLAIM.comp_week_code = ~~~"" + ls_FieldValue + "~~~")"
		END IF

		ls_FieldValue = dw_search_criteria.GetItemString(1,"comp_day_code")
		IF Trim(ls_FieldValue) > "" THEN
			ls_Additional_WhereClause = ls_Additional_WhereClause + " and (CLAIM.comp_day_code = ~~~"" + ls_FieldValue + "~~~")"
		END IF

		ls_FieldValue = dw_search_criteria.GetItemString(1, "occupational_therapist_user_id")
		IF ls_FieldValue  > "" THEN
			ls_Additional_WhereClause = ls_Additional_WhereClause + " and (CLAIM.occupational_therapist_user_id = ~~~"" + ls_FieldValue + "~~~")"
		END IF

		ls_FieldValue = dw_search_criteria.GetItemString(1, "rehab_officer_user_id")
		IF ls_FieldValue  > "" THEN
			ls_Additional_WhereClause = ls_Additional_WhereClause + " and (CLAIM.rehab_officer_user_id = ~~~"" + ls_FieldValue + "~~~")"
		END IF

		ls_FieldValue = dw_search_criteria.GetItemString(1,"region")
		ls_Additional_WhereClause = ls_Additional_WhereClause + "  and INDIVIDUAL_NAME.name_type_code = ~~~"M~~~" and (CLAIM.admin_region_code = ~~~"" + ls_FieldValue + "~~~")"

/*		Now, build the rest of the where string to be appended to the basic where clause
*/
		ls_opening = dw_search_criteria.GetItemString(1,"opening_type_code")
		ls_FieldValue = dw_search_criteria.GetItemString(1,"claim_status_code")
		IF Trim(ls_FieldValue) > "" THEN
			IF ls_FieldValue = 'N' THEN //means that it's for Active & Final NLT (case managed)
				IF Trim(ls_opening) > "" THEN //opening type is filled in
					ls_Additional_WhereClause_Union = ls_Additional_WhereClause + " and (CLAIM.claim_status_code = 'F' and CLAIM.claim_status_type_code in ('02', '03', '04') and CLAIM.case_managed_flag = 'Y') "
					ls_Additional_WhereClause = ls_Additional_WhereClause + " and (CLAIM.claim_status_code = 'A')  "
				ELSE
					ls_Additional_WhereClause =  ls_Additional_WhereClause + " and (CLAIM.claim_status_code = 'A' OR (CLAIM.claim_status_code = 'F' and CLAIM.case_managed_flag = 'Y' and CLAIM.claim_status_type_code in ('02', '03','04'))) "
				END IF
				ls_header = 'Active/Final NLT (CM) Case Load'
			ELSE
				ls_Additional_WhereClause = ls_Additional_WhereClause + " and (CLAIM.claim_status_code = ~~~"" + ls_FieldValue + "~~~")"
				ls_header = 'Active Case Load'
				ls_FieldValue = dw_search_criteria.GetItemString(1,"claim_status_type_code")
				IF Trim(ls_FieldValue) > "" THEN
					ls_Additional_WhereClause = ls_Additional_WhereClause + " and (CLAIM.claim_status_type_code = ~~~"" + ls_FieldValue + "~~~")"
				END IF
			END IF
		END IF

/*		If an opening type is desired, we have to add the OPENING table and it's where criteria to the basic syntax
*/
//		ls_FieldValue = dw_search_criteria.GetItemString(1,"opening_type_code")
//		IF Trim(ls_FieldValue) > "" THEN
//			ls_SQLSyntax = ls_SQLSyntax + ", OPENING.benefit_start_date " 
//			ls_from_clause = ls_from_clause + " INNER JOIN OPENING  ON CLAIM.claim_no = OPENING.claim_no "
//			ls_WhereClause = ls_WhereClause + " and (OPENING.benefit_end_date is null) "
//			ls_Additional_WhereClause = ls_Additional_WhereClause + " and (OPENING.opening_type_code = ~~~"" + ls_FieldValue + "~~~")"
//		END IF
		
		ls_FieldValue = dw_search_criteria.GetItemString(1,"opening_type_code")
		IF dw_search_criteria.GetItemString(1,"claim_status_code") = 'N' THEN 
			IF Trim(ls_FieldValue) > "" THEN
				ls_SQLSyntax_Union = " UNION ALL " + ls_SQLSyntax
			END IF
		END IF	
		IF Trim(ls_FieldValue) > "" THEN
			ls_Additional_from_clause = ls_from_clause + "    INNER JOIN OPENING ON CLAIM.claim_no = OPENING.claim_no"
			ls_Additional_WhereClause = ls_Additional_WhereClause + " and (OPENING.benefit_end_date is null) and (OPENING.opening_type_code = ~~~"" + ls_FieldValue + "~~~")"
		END IF
	

/*		Modify the data window's syntax.....   
*/

		dw_report.object.t_header.Text = ls_header
		
		IF dw_search_criteria.GetItemString(1,"claim_status_code") = 'N' and Trim(ls_opening) >"" THEN
			ls_ModString = "DataWindow.Table.Select=~"" + ls_SQLSyntax + ls_Additional_from_clause + ' ' + ls_WhereClause + ls_Additional_WhereClause + ls_SQLSyntax_Union + ls_from_clause + ls_WhereClause + ls_Additional_WhereClause_Union + "~""
		ELSEIF Trim(ls_opening) >"" THEN			
			ls_ModString = "DataWindow.Table.Select=~"" + ls_SQLSyntax + ls_Additional_from_clause + ' ' + ls_WhereClause + ls_Additional_WhereClause + "~""
		ELSE
			ls_ModString = "DataWindow.Table.Select=~"" + ls_SQLSyntax + ls_from_clause + ' ' + ls_WhereClause + ls_Additional_WhereClause + "~""
		END IF	

		
//		ls_ModString = "DataWindow.Table.Select=~"" + ls_SQLSyntax + ls_from_clause +  ls_WhereClause + ls_Additional_WhereClause + "~""
		ls_ReturnCode = dw_report.Modify(ls_ModString)
		IF Len(ls_ReturnCode) > 0 Then
			Return -1
		Else
			Return 1
		END IF
end function

event open;call super::open;	INTEGER				li_ReturnCode
	DATAWINDOWCHILD	ldwc_child
	

IF IsNull(inv_resize) OR NOT IsValid (inv_resize) THEN
	inv_resize = create n_resize
	inv_resize.of_SetOrigSize (3282,2580)
END IF

	dw_search_criteria.SetTransObject(SQLCA)
	dw_report.SetTransObject(SQLCA)

	dw_search_criteria.InsertRow(0)


/*  Set up the basic SQL Syntax that will be modified for the requested search.    
*/

is_from_Clause =  "FROM CLAIM " + &
 " LEFT OUTER JOIN EMPLOYER   ON CLAIM.accident_employer_no = EMPLOYER.employer_no" + &
 " INNER JOIN CLAIM_PARTICIPANT   ON CLAIM.claim_no = CLAIM_PARTICIPANT.claim_no" + &
 " INNER JOIN INDIVIDUAL   ON CLAIM_PARTICIPANT.individual_no = INDIVIDUAL.individual_no" + &
 " INNER JOIN INDIVIDUAL_NAME   ON INDIVIDUAL.individual_no = INDIVIDUAL_NAME.individual_no"

 //" INNER JOIN INDIVIDUAL_NAME   ON INDIVIDUAL.individual_no = INDIVIDUAL_NAME.individual_no"+ &
 //" LEFT OUTER JOIN OPENING   ON CLAIM.claim_no = OPENING.claim_no" 
 
 is_whereclause =  " WHERE CLAIM_PARTICIPANT.claim_role_code = ~~~"C~~~" and " + &
                             'CLAIM_PARTICIPANT.claimant_active_flag = ~~~"Y~~~"'
							
/* Load the search type datawindow, set default value & disable so user cannot change valu
*/
	dw_search_type.InsertRow(1)
	dw_search_type.SetItem(1, "search_type","Case Load")
	dw_search_type.Enabled = FALSE

/*	Load the claim status type drop down data window.
*/
	IF dw_search_criteria.GetChild("claim_status_type_code",idwc_claim_status_type_code) < 0 THEN
		MessageBox("DataWindow Error","Unable to retrieve claim status type codes!",StopSign!)
		Return
	END IF
	idwc_claim_status_type_code.SetTransObject(SQLCA)
	idwc_claim_status_type_code.Retrieve()

/*	Load the claim manager drop down data window
*/
	IF dw_search_criteria.GetChild("user_id",idwc_claim_manager) < 0 THEN
		MessageBox("DataWindow Error","Unable to load claim manager list!",StopSign!)
		Return
	END IF
	idwc_claim_manager.SetTransObject(SQLCA)
	idwc_claim_manager.Retrieve()
	idwc_claim_manager.InsertRow(1)
	idwc_claim_manager.SetItem(1,"user_id","")
	idwc_claim_manager.SetItem(1,"user_last_name","")
	idwc_claim_manager.SetItem(1,"user_first_name","")

/*	Load the Region drop down data window, then filter for active region codes
*/
	IF dw_search_criteria.GetChild("region",ldwc_child) < 0 THEN
		MessageBox("DataWindow Error","Unable to load admin region list!",StopSign!)
		Return
	End If
	ldwc_child.SetTransObject(SQLCA)
	ldwc_child.Retrieve()
	ldwc_child.SetFilter("active_flag = 'Y'")
	ldwc_child.Filter()

/*	Load the Opening Type drop down data window
*/
//	li_ReturnCode = dw_search_criteria.GetChild("opening_type_code",ldwc_child)
//	If li_ReturnCode < 0 Then
//		MessageBox("DataWindow Error","Unable to retrieve opening types!",StopSign!)
//		Return
//	End If
//	ldwc_child.SetTransObject(SQLCA)
//	ldwc_child.Retrieve()
//	ldwc_child.SetFilter("active_flag = 'Y'")
//	ldwc_child.Filter()
	
/* Load the Rehab Type drop down datawindow, then sort the data
*/
   IF dw_search_criteria.GetChild("rehab_type_code", ldwc_child) <0 THEN
		MessageBox("DataWindow Error","Unable to retrieve rehab types!",StopSign!)
		Return
	END IF
	ldwc_child.SetTransObject(SQLCA)
	ldwc_child.Retrieve()
	ldwc_child.SetSort("rehab_type_desc A")
	ldwc_child.Sort()
   ldwc_child.InsertRow(1)
	ldwc_child.SetItem(1,"rehab_type_desc","")
	ldwc_child.SetItem(1,"rehab_type_code","")

	// Load the Rehab Manager drop down data window 
	IF dw_search_criteria.GetChild("rehab_officer_user_id",ldwc_child) < 0 THEN
		MessageBox("DataWindow Error","Unable to load Rehab Officer list!",StopSign!)
		Return
	END IF
	ldwc_child.SetTransObject(SQLCA)
	ldwc_child.Retrieve()
	ldwc_child.InsertRow(1)
	ldwc_child.SetItem(1,"user_id","")
	ldwc_child.SetItem(1,"user_last_name","")
	ldwc_child.SetItem(1,"user_first_name","")

	// Load the Occupational Therapist drop down data window 
	IF dw_search_criteria.GetChild("occupational_therapist_user_id",ldwc_child) < 0 THEN
		MessageBox("DataWindow Error","Unable to load Occupational Therapist list!",StopSign!)
		Return
	END IF
	ldwc_child.SetTransObject(SQLCA)
	ldwc_child.Retrieve()
	ldwc_child.InsertRow(1)
	ldwc_child.SetItem(1,"user_id","")
	ldwc_child.SetItem(1,"user_last_name","")
	ldwc_child.SetItem(1,"user_first_name","")

/*	Defaults: set the region code to the users default admin region code;  
	set the status to 'Active' and filter the status_type drop down dw;
	default the claim manager to the current user (if this user is a claim manager).
*/
	dw_search_criteria.SetItem(1,"region",vgst_user_profile.default_admin_region_code)

	IF idwc_claim_manager.Find("user_id = '" + vgst_user_profile.user_id + "'",1,idwc_claim_manager.RowCount()) > 0 THEN
		dw_search_criteria.SetItem(1,"user_id",vgst_user_profile.user_id)
	END IF

	dw_search_criteria.SetItem(1,"claim_status_code","N")
//	idwc_claim_status_type_code.SetFilter("claim_status_code = 'N'")
//	idwc_claim_status_type_code.Filter()

//	dw_search_criteria.SetTabOrder("claim_status_code",0)
	dw_search_criteria.SetTabOrder("claim_status_type_code",0)
	dw_search_criteria.Modify("claim_status_type_code.Enabled = FALSE")
	

//(Move H,Move V,Grow H, Grow V)
inv_resize.of_register(dw_search_criteria,0,0,99,0)
inv_resize.of_register(gb_1,0,0,99,0)
inv_resize.of_register(dw_report,0,0,99,99)

	
	
	
end event

on w_case_load.create
int iCurrent
call super::create
this.gb_1=create gb_1
this.dw_search_criteria=create dw_search_criteria
this.cb_search=create cb_search
this.dw_search_type=create dw_search_type
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_1
this.Control[iCurrent+2]=this.dw_search_criteria
this.Control[iCurrent+3]=this.cb_search
this.Control[iCurrent+4]=this.dw_search_type
end on

on w_case_load.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.gb_1)
destroy(this.dw_search_criteria)
destroy(this.cb_search)
destroy(this.dw_search_type)
end on

event resize;call super::resize;long ll_workspacewidth,ll_workspaceheight


// Notify the resize service that the window size has changed.
ll_workspacewidth = This.WorkSpaceWidth()
ll_workspaceheight = This.WorkSpaceHeight()

If IsValid (inv_resize) Then
	inv_resize.Event pfc_Resize (sizetype,ll_workspacewidth  , ll_workspaceheight )
End If
end event

type dw_report from w_a_report`dw_report within w_case_load
integer width = 3191
integer taborder = 50
string dataobject = "d_case_load"
boolean hscrollbar = true
end type

type gb_1 from groupbox within w_case_load
integer x = 46
integer width = 3191
integer height = 548
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Case Load Parameters"
end type

type dw_search_criteria from u_dw_online within w_case_load
integer x = 55
integer y = 64
integer width = 3141
integer height = 472
integer taborder = 30
boolean bringtotop = true
string dataobject = "d_case_load_search_report"
boolean border = false
end type

on itemchanged;dw_report.Reset()
end on

type cb_search from commandbutton within w_case_load
integer x = 2871
integer y = 68
integer width = 283
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

event clicked;/*	This script controls validation of the search criteria, creating the SQL required for 
	the requested search and retrieving the data. 
*/

	INTEGER		li_ReturnCode
	STRING		ls_ModifyError

	dw_search_criteria.AcceptText()
	IF wf_validate_criteria() < 0 THEN
		Return
	END IF

IF dw_search_criteria.GetItemString(1,"claim_status_code") = 'N' THEN
	//use d_case_load
	dw_report.dataobject = "d_case_load"
	dw_report.SetTransObject(SQLCA)
ELSE
	//use_case_load2
	dw_report.dataobject = "d_case_load2"
	dw_report.SetTransObject(SQLCA)
END IF

is_SQLSyntax = dw_report.Describe("DataWindow.Table.Select")
is_SQLSyntax = left(is_SQLSyntax,pos(is_SQLSyntax,"FROM",1)-1)

/*	Build the SQL query based on the search criteria. 
*/
	IF wf_set_query() < 0 Then
		MessageBox("Search Criteria Errors!","Error setting search criteria. Contact Systems!",StopSign!)
		Return
	END IF

/* Retrieve the search list based on the SQL query just built. 
*/
	li_ReturnCode = wf_retrieve()
	If li_ReturnCode < 0 Then
		MessageBox("Search Errors!","Errors retrieving data matching your search criteria!",StopSign!)
		Return
	ElseIf li_ReturnCode = 0 Then
		MessageBox("No Claims Found!","No claims were located matching your search criteria!",Exclamation!)
		Return
	End If


end event

type dw_search_type from u_dw_online within w_case_load
integer x = 55
integer y = 72
integer width = 1088
integer height = 88
integer taborder = 40
boolean bringtotop = true
string dataobject = "d_search_type"
boolean border = false
end type

