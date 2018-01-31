$PBExportHeader$u_claim_search.sru
$PBExportComments$Claim Search - Visual User Object to search the Claim database for claims matching the entered search criteria.
forward
global type u_claim_search from userobject
end type
type cbx_inbasket from checkbox within u_claim_search
end type
type cb_annuity_only_list from commandbutton within u_claim_search
end type
type cb_more from commandbutton within u_claim_search
end type
type cb_clear from commandbutton within u_claim_search
end type
type cb_search from commandbutton within u_claim_search
end type
type dw_search_type from u_dw_online within u_claim_search
end type
type cb_extract from commandbutton within u_claim_search
end type
type uo_filter_control from u_filter_control within u_claim_search
end type
type dw_search_list from u_dw_online within u_claim_search
end type
type dw_search_list_individual from u_dw_online within u_claim_search
end type
type dw_search_list_annuity_individual from u_dw_online within u_claim_search
end type
type dw_search_criteria from u_dw_online within u_claim_search
end type
type gb_1 from groupbox within u_claim_search
end type
end forward

global type u_claim_search from userobject
integer width = 3182
integer height = 1824
long backcolor = 67108864
long tabtextcolor = 33554432
event ue_doubleclicked pbm_custom01
cbx_inbasket cbx_inbasket
cb_annuity_only_list cb_annuity_only_list
cb_more cb_more
cb_clear cb_clear
cb_search cb_search
dw_search_type dw_search_type
cb_extract cb_extract
uo_filter_control uo_filter_control
dw_search_list dw_search_list
dw_search_list_individual dw_search_list_individual
dw_search_list_annuity_individual dw_search_list_annuity_individual
dw_search_criteria dw_search_criteria
gb_1 gb_1
end type
global u_claim_search u_claim_search

type variables
// General variables for User Object

w_ancestor	  iwi_window_parent	// Contains pointer to parent window

String		  is_SearchType		// Contains the current search type value
String		  is_BasicClaimSyntax
String       is_stats_syntax
String		  is_SQLSyntax		// Contains the SQL used to modify the select statement on d_claim_search
String		  is_WhereClause		// Contains standard where clause appended to end of SQL statement
String		  is_SQLSyntaxI		// Contains the SQL used to modify the select statement on d_claim_search
String		  is_WhereClauseI		// Contains standard where clause appended to end of SQL statement
String		  is_SQLSyntaxRx1		// Contains the 1st half of SQL used to modify the select statement on d_claim_search for rx termination search
String		  is_SQLSyntaxRx1a		// Contains 2nd part of the 1st half of SQL used to modify the select statement on d_claim_search for rx termination search
String		  is_SQLSyntaxRx2		// Contains the 2nd half of SQL used to modify the select statement on d_claim_search for rx termination search
String		  is_SQLSyntax_to_do //Coatains the sql for the "to do" search.
String       is_SQLSyntax_basic_individual //P10273 P2-D3
String       is_SQLSyntax_individual_annuity_list
STRING	  is_filter

// Drop Down Data Windows

DataWindowChild	  idwc_claim_status_type_code
DataWindowChild	  idwc_claim_manager

// Search criteria values

String		  is_last_name		// Contains last name search value 
String		  is_first_name		// Contains first name search value
String		  is_name_search_option	// Contains name type search indicator ('B'egins with, 'E'quals or 'L'ike)
String		  is_soundex_search_option
String		  is_opening_type_code, is_rb_doc_type
	
Long		  il_claim_no		// Contains current selected claim number
Long		  il_sin			// Contains SIN search value
Long		  il_medicare		// Contains Medicare search value
Date		  id_birth_date		// Contains Birth date search value
Long        il_individual_no   // Contains current selected individual number

DateTime  idt_benefit_from_date		// Contains opening benefit from date used to determine due/overdue
DateTime  idt_benefit_to_date		// Contains opening benefit to date used to determine due/overdue
DateTime  idt_bdate_from_date		// Contains birth date from date used to determine due/overdue
DateTime  idt_bdate_to_date		// Contains birth date to date used to determine due/overdue
String		  is_review_type		// Contains type of due/overdue search to be performed
String		  is_overdue_ind		// Contains ind used to determine if only overdue claims are to be listed

BOOLEAN ib_force_plan


LONG			il_normal_y
LONG			il_normal_height

M_DW_ONLINE_RMB_POPUP im_popup
end variables

forward prototypes
public subroutine uf_set_search_type (string search_type)
public subroutine uf_set_parent (window parent_window)
public subroutine uf_protect_searchtype (string protect_type)
public subroutine uf_set_claim (long claim_no)
public subroutine uf_clear_search_criteria ()
public function integer uf_set_query ()
public function integer uf_validate_criteria ()
public subroutine uf_shrink_enlarge (readonly string as_arg)
public subroutine uf_set_search_defaults (s_claim_search_defaults astr_claim_search_defaults, string as_search_type)
protected function integer uf_retrieve ()
public subroutine uf_set_dw_object (string as_dw_object, string as_due_overdue_type)
public subroutine uf_refresh_search ()
end prototypes

public subroutine uf_set_search_type (string search_type);/*	This function sets the default claim search based on the value passed in triggers
	the item changed event for 'dw_search_type' to ensure that the appropriate search
	criteria datawindow is displayed
*/
	dw_search_type.InsertRow(0)
	dw_search_type.SetItem(1,"search_type",search_type)
	dw_search_type.SetFocus()
	is_SearchType = search_type
	dw_search_type.TriggerEvent(ItemChanged!)


end subroutine

public subroutine uf_set_parent (window parent_window);/*	This function sets the parent window to the window that is passed in.
*/

	iwi_window_parent = parent_window
end subroutine

public subroutine uf_protect_searchtype (string protect_type);/*	This function protects the current search type by disabling or enabling the
	search list data window passed on the value passed in.
*/

	STRING	ls_protect_type

	ls_protect_type = Upper(protect_type)

	CHOOSE CASE ls_protect_type
		CASE "ENABLE"
			dw_search_type.Enabled = True

		CASE "DISABLE"
			dw_search_type.Enabled = False
	END CHOOSE
end subroutine

public subroutine uf_set_claim (long claim_no);/*	This function triggers the parents 'wf_set_claim' function,
	which will retrieve and load the basic claim information.
	This function is called when a claim in the claim list is
	selected or when searching by the claim number.
*/

	LONG	ll_CurrentRow

	IF is_SearchType = "d_claim_number_search" THEN
		il_claim_no = dw_search_criteria.GetItemNumber(1,"claim_no")
	ELSE
	   IF (is_SearchType = "d_basic_claim_search" OR is_SearchType = "d_individual_number_search") AND dw_search_list_individual.visible = TRUE THEN
		  	ll_CurrentRow = dw_search_list_individual.GetRow()
			IF ll_CurrentRow > 0 THEN
				il_claim_no = dw_search_list_individual.GetItemNumber(ll_CurrentRow,"claim_no")
			END IF
		ELSE
  			ll_CurrentRow = dw_search_list.GetRow()
			IF ll_CurrentRow > 0 THEN
   			il_claim_no = dw_search_list.GetItemNumber(ll_CurrentRow,"claim_no")
			END IF
  		END IF
	END IF

	iwi_window_parent.wf_set_claim(il_claim_no)

end subroutine

public subroutine uf_clear_search_criteria ();/*	Reset the values shown on dw_search_criteria. Leave the current selected
	search items as they are.
*/

	INTEGER	li_ReturnCode
	STRING	ls_name_search_option,	ls_soundex_search_option

	CHOOSE CASE is_SearchType

		CASE "d_claim_number_search", "d_individual_number_search"

			dw_search_criteria.Reset()
			dw_search_criteria.InsertRow(0)

		CASE "d_basic_claim_search", "d_individual_search"

	      dw_search_list_individual.Reset()
	      dw_search_list_individual.visible = FALSE
	      cb_more.visible = FALSE
	      dw_search_list.visible = TRUE

			ls_name_search_option = dw_search_criteria.GetItemString(1,"name_search_option")
 			ls_soundex_search_option = dw_search_criteria.GetItemString(1,"soundex_search_option")

			dw_search_criteria.Reset()
			dw_search_criteria.InsertRow(0)

			dw_search_criteria.SetItem(1,"name_search_option",ls_name_search_option)
			dw_search_criteria.SetItem(1,"soundex_search_option",ls_soundex_search_option)

		CASE "d_case_load_search2"

			dw_search_criteria.Reset()
			dw_search_criteria.InsertRow(0)

/*			Defaults: set the region code to the users default admin region code;  
			set the status to 'Active' and filter the status_type drop down dw;
			set the claim manager to the current user (if the current user exists in the drop down)
*/
			dw_search_criteria.SetItem(1,"region",vgst_user_profile.default_admin_region_code)

			IF idwc_claim_manager.Find("user_id = '" + vgst_user_profile.user_id + "'",1,idwc_claim_manager.RowCount()) > 0 THEN
				dw_search_criteria.SetItem(1,"user_id",vgst_user_profile.user_id)
			END IF

			dw_search_criteria.SetItem(1,"claim_status_code","N")
	//		idwc_claim_status_type_code.SetFilter("claim_status_code = 'A'")
	//		idwc_claim_status_type_code.Filter()

		CASE "d_due_overdue_list"

			dw_search_criteria.Reset()
			dw_search_criteria.InsertRow(0)
			dw_search_criteria.SetItem(1,"overdue_ind","D")
			dw_search_criteria.SetItem(1,"opening_from_date",Today())


		CASE 'd_to_do_search'
			dw_search_criteria.Reset()
			dw_search_criteria.InsertRow(0)
			dw_search_criteria.SetItem(1,'user_id',vgst_user_profile.user_id)
			dw_search_criteria.SetItem(1,'task_status_code','01')
		
		CASE 'd_claim_rx_termination_search'
			dw_search_criteria.Reset()
			dw_search_criteria.InsertRow(0)
			dw_search_list.setFilter("")
			dw_search_list.filter()
			
	END CHOOSE	


dw_search_list.Reset()
dw_search_list_annuity_individual.Reset()
dw_search_list_individual.Reset()

cb_annuity_only_list.visible = FALSE
cb_more.Visible = FALSE



end subroutine

public function integer uf_set_query ();/*	This function modifies the 'WHERE' clause based on the current search type and
	modIFies dw_search_list. It returns 1 IF successfull and -1 IF there were errors
*/

STRING	ls_ReturnCode,	ls_Additional_WhereClause,	ls_ModString, ls_FieldValue, ls_SQLSyntax,	ls_WhereClause, ls_SQLSyntax_Union, ls_Additional_WhereClause_Union
DATE		ldt_from_date, ldt_to_date
BOOLEAN lb_twodates //Added PR 2073
STRING	ls_sr12_sr13								// added for SR12 and SR13
LONG		ll_sr12_sr13,ll_ls_sr12_sr13_len		// added for SR12 and SR13
STRING	ls_sort_criteria,  ls_and_or, ls_opening
INT       li_pos, li_rtn
BOOLEAN lb_no_sin, lb_no_medicare, lb_no_birthdate


// 		( CLAIM.claim_status_code <> 'R') AND

/*	Aug 28/96 the ib_force_plan  is used to force a query to use a better search plan
	one of the queries was causing a table scan and setting the set force plan on results in a 
	better performance
*/
	
	ib_force_plan = FALSE
	CHOOSE CASE is_SearchType

		CASE "d_claim_number_search"
						
/*			Build the WHERE clause for the basic claim search......
*/
			ls_Additional_WhereClause = ls_Additional_WhereClause + " and (CLAIM.claim_no = " + String(il_claim_no) + ")"

/*			Modify the data window syntax of both the search list datawindow and the search list individual.....   
*/
			ls_ModString = "DataWindow.Table.Select=~"" + is_SQLSyntaxI + is_WhereClauseI + ls_Additional_WhereClause + "~""
			ls_ReturnCode = dw_search_list_individual.ModIFy(ls_ModString)

			ls_ModString = "DataWindow.Table.Select=~"" + is_SQLSyntax + is_WhereClause + ls_Additional_WhereClause + "~""
			ls_ReturnCode = dw_search_list.ModIFy(ls_ModString)
			IF Len(ls_ReturnCode) > 0 Then
				Return -1
			Else
				Return 1
			END IF

		CASE "d_basic_claim_search"

/*			Build the WHERE clause for the basic claim search......
*/
			IF is_last_name > "" THEN
 				IF is_name_search_option = "B" THEN
					ls_Additional_WhereClause = ls_Additional_WhereClause + " and (INDIVIDUAL_NAME.last_name like ~~~"" + is_last_name + "%~~~" COLLATE SQL_Latin1_General_CP850_CI_AI)"
				ELSEIF is_soundex_search_option = "0" THEN
					ls_Additional_WhereClause = ls_Additional_WhereClause + " and (INDIVIDUAL_NAME.last_name = ~~~"" + is_last_name + "~~~")"
				ELSE
					ls_Additional_WhereClause = ls_Additional_WhereClause + " and (INDIVIDUAL_NAME.soundex_last_name = Soundex(~~~"" + is_last_name + "~~~"))"
				END IF
			END IF
			IF is_first_name > "" THEN
 				IF is_name_search_option = "B" THEN
					ls_Additional_WhereClause = ls_Additional_WhereClause + " and (INDIVIDUAL_NAME.given_names like ~~~"" + is_first_name + "%~~~" COLLATE SQL_Latin1_General_CP850_CI_AI)"
				ELSEIF is_soundex_search_option = "0" THEN
					ls_Additional_WhereClause = ls_Additional_WhereClause + " and (INDIVIDUAL_NAME.given_names = ~~~"" + is_first_name + "~~~")"
				ELSE
					ls_Additional_WhereClause = ls_Additional_WhereClause + " and (INDIVIDUAL_NAME.soundex_given_names = Soundex(~~~"" + is_first_name + "~~~"))"
				END IF
			END IF
			IF il_sin > 0 Then
				ls_Additional_WhereClause = ls_Additional_WhereClause + " and (INDIVIDUAL.sin_no = " + string(il_sin) + ")"
			END IF
			IF il_medicare > 0 Then
				ls_Additional_WhereClause = ls_Additional_WhereClause + " and (INDIVIDUAL.medicare_no = " + string(il_medicare) + ")"
			END IF
			IF String(id_birth_date) > "" Then
				ls_Additional_WhereClause = ls_Additional_WhereClause + " and (INDIVIDUAL.birth_date = ~~~"" + String(id_birth_date,"yyyy-mm-dd") + "~~~")"
			END IF

/*			Modify the data window syntax of both the search list datawindow and the search list individual.....
*/
			ls_ModString = "DataWindow.Table.Select=~"" + is_SQLSyntaxI + is_WhereClauseI + ls_Additional_WhereClause + "~""
			ls_ReturnCode = dw_search_list_individual.ModIFy(ls_ModString)

			ls_ModString = "DataWindow.Table.Select=~"" + is_SQLSyntax + is_WhereClause + ls_Additional_WhereClause + "~""
			ls_ReturnCode = dw_search_list.ModIFy(ls_ModString)
			IF Len(ls_ReturnCode) > 0 Then
				Return -1
			END IF
			
			ls_SQLSyntax = is_SQLSyntax_individual_annuity_list + ls_Additional_WhereClause
			ls_ModString = "DataWindow.Table.Select=~"" + ls_SQLSyntax + "~""
			ls_ReturnCode = dw_search_list_annuity_individual.ModIFy(ls_ModString)
			ls_ModString = dw_search_list_annuity_individual.getSQLSelect()
			IF Len(ls_ReturnCode) > 0 Then
				Return -1
			ELSE
				return 1
			END IF
	
		Case "d_case_load_search2"
			

/*			Take a copy of the basic SQL Syntax and the basic WHERE clause strings
*/
			ls_SQLSyntax = is_SQLSyntax
			ls_WhereClause = is_WhereClause

/*			this is used to better the performance of the query by SW region
*/
			ib_force_plan = TRUE


/*			If a Rehab Type desired, must add REHAB_GOAL table to the basic syntax
*/
			ls_FieldValue = dw_search_criteria.GetItemString(1,"rehab_type_code")
			IF Trim(ls_FieldValue) > "" THEN
				ls_SQLSyntax = ls_SQLSyntax + "  INNER JOIN REHAB_GOAL ON CLAIM.claim_no = REHAB_GOAL.claim_no"
				ls_Additional_WhereClause = ls_Additional_WhereClause + " and (REHAB_GOAL.rehab_type_code = ~~~"" + ls_FieldValue + "~~~")"
			END IF

/*			Now, build the rest of the where string to be appended to the basic where clause
*/

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
			ls_Additional_WhereClause = ls_Additional_WhereClause + " and INDIVIDUAL_NAME.name_type_code = ~~~"M~~~" and (CLAIM.admin_region_code = ~~~"" + ls_FieldValue + "~~~")"


/*			If claim status code = N then get both the active and final not lost time case managed claims.
           Ensure these are the last statements in the where clause
*/

			ls_FieldValue = dw_search_criteria.GetItemString(1,"claim_status_code")
			ls_opening = dw_search_criteria.GetItemString(1,"opening_type_code")
			IF Trim(ls_FieldValue) > "" THEN
				IF ls_FieldValue = 'N' THEN 
				   IF Trim(ls_opening) >"" THEN //means that it's for Active & Final NLT (case managed) and there is an opening type provided
						ls_Additional_WhereClause_Union = ls_Additional_WhereClause + " and (CLAIM.claim_status_code = 'F' and CLAIM.claim_status_type_code in ('02', '03', '04') and CLAIM.case_managed_flag = 'Y') "
						ls_Additional_WhereClause = ls_Additional_WhereClause + " and (CLAIM.claim_status_code = 'A')  "
					ELSE
						ls_Additional_WhereClause =  ls_Additional_WhereClause + " and (CLAIM.claim_status_code = 'A' OR (CLAIM.claim_status_code = 'F' and CLAIM.case_managed_flag = 'Y' and CLAIM.claim_status_type_code in ('02', '03','04'))) "
					END IF
				ELSE
					ls_Additional_WhereClause = ls_Additional_WhereClause + " and (CLAIM.claim_status_code = ~~~"" + ls_FieldValue + "~~~")"
					ls_FieldValue = dw_search_criteria.GetItemString(1,"claim_status_type_code")
					IF Trim(ls_FieldValue) > "" THEN
						ls_Additional_WhereClause = ls_Additional_WhereClause + " and (CLAIM.claim_status_type_code = ~~~"" + ls_FieldValue + "~~~")"
					END IF
				END IF
			END IF
			
/*			If an opening type is desired, we have to add the OPENING table and it's where criteria to the basic syntax
*/
			ls_FieldValue = dw_search_criteria.GetItemString(1,"opening_type_code")
			
			IF dw_search_criteria.GetItemString(1,"claim_status_code") = 'N' THEN 
				IF Trim(ls_FieldValue) > "" THEN
					ls_SQLSyntax_Union = " UNION ALL " + ls_SQLSyntax
				END IF
			END IF	
			IF Trim(ls_FieldValue) > "" THEN
				ls_SQLSyntax = ls_SQLSyntax + "    INNER JOIN OPENING ON CLAIM.claim_no = OPENING.claim_no"
				ls_Additional_WhereClause = ls_Additional_WhereClause + " and (OPENING.benefit_end_date is null) and (OPENING.opening_type_code = ~~~"" + ls_FieldValue + "~~~")"
			END IF

/*			Modify the data window's syntax.....   
*/

			IF dw_search_criteria.GetItemString(1,"claim_status_code") = 'N' and Trim(ls_opening) >"" THEN
				ls_ModString = "DataWindow.Table.Select=~"" + ls_SQLSyntax + ' ' + ls_WhereClause + ls_Additional_WhereClause + ls_SQLSyntax_Union + ls_WhereClause + ls_Additional_WhereClause_Union + "~""
			ELSE				
				ls_ModString = "DataWindow.Table.Select=~"" + ls_SQLSyntax + ' ' + ls_WhereClause + ls_Additional_WhereClause + "~""
			END IF	
				
			ls_ReturnCode = dw_search_list.Modify(ls_ModString)
			IF Len(ls_ReturnCode) > 0 Then
				Return -1
			Else
				Return 1
			END IF

		Case "d_stats_not_coded"

/*			This search uses syntax defined in the constructor event of this user object. There is no user 
			specified parameters, so there are no 'on the fly' modifications required.
*/
			ib_force_plan = TRUE

			ls_ModString = is_stats_syntax
			ls_ModString = dw_search_list.Modify("DataWindow.Table.Select=~"" + is_stats_syntax + ls_Additional_WhereClause + "~"")
			
			
			
			/*	If the modify went well then return.
			*/
			IF Len(ls_ModString) > 0 Then
				Return -1
			Else
				Return 1
			END IF

		Case "d_due_overdue_list" //PR20386, PR17517 2014-09-24 David Worboys

/*			Take a copy of the basic SQL Syntax and the basic WHERE clause strings
*/
			ls_SQLSyntax = is_SQLSyntax
			ls_WhereClause = is_WhereClause

/*			Add the OPENING table and it's where criteria to the basic syntax for the 12 week and annual review
			review searches. For all searches, add the appropriate selection criteria.
				For OverDue only - extract all with the recurrence date + number of days must be less than the from date
										 (the to date is inapplicable when looking for overdue only)
				For Due Only	  - extract all with the recurrence date + number of days >= than the from date and
										 <= the to date.
*/
			IF is_review_type = "12W" THEN
				/*begin changes for SR12 and SR13
					Kevin MacLeod Jan.16, 2001
				*/				
				ls_SQLSyntax = ls_SQLSyntax + " INNER JOIN OPENING ON CLAIM.claim_no = OPENING.claim_no"
				
				ls_WhereClause = " WHERE INDIVIDUAL_NAME.name_type_code = ~~~"M~~~" "
				ls_WhereClause = ls_WhereClause + " AND (OPENING.benefit_end_date is null OR "
				ls_WhereClause = ls_WhereClause + " (OPENING.benefit_end_date >= DateAdd(Day,84,OPENING.benefit_start_date))) " 
				ls_WhereClause = ls_WhereClause + " AND OPENING.opening_type_code = ~~~"RLOE~~~" " 
				ls_WhereClause = ls_WhereClause + " AND OPENING.review_12_week_date is null " 
				
				IF is_overdue_ind = "O" THEN
					ls_WhereClause = ls_WhereClause + " AND (Convert(Datetime,DateAdd(Day,84,OPENING.benefit_start_date)) < Getdate())"
				ELSE
					ls_WhereClause = ls_WhereClause + " AND (DateAdd(Day,84,OPENING.benefit_start_date) >= ~~~"" + String(idt_benefit_from_date,"yyyy-mm-dd") + "~~~")" 
					ls_WhereClause = ls_WhereClause + " AND (DateAdd(Day,84,OPENING.benefit_start_date) <= ~~~"" + String(idt_benefit_to_date,"yyyy-mm-dd") + "~~~")"  
				END IF
				
				ls_WhereClause = ls_WhereClause + " AND NOT (OPENING.benefit_end_date < '1996/10/14' AND CLAIM.claim_status_code = 'F') "
				ls_WhereClause = ls_WhereClause + " AND CLAIM.claim_status_code <> 'R' "
				ls_WhereClause = ls_WhereClause + " AND OPENING.accident_recurrence_date >= '1993/01/01' "
				ls_WhereClause = ls_WhereClause + " AND NOT EXISTS"
				ls_WhereClause = ls_WhereClause + "( SELECT * " 
				ls_WhereClause = ls_WhereClause + " FROM CLAIM_STATUS_CHANGE "
				ls_WhereClause = ls_WhereClause + " WHERE CLAIM.claim_no = CLAIM_STATUS_CHANGE.claim_no "
				ls_WhereClause = ls_WhereClause + " AND CLAIM.claim_status_code = CLAIM_STATUS_CHANGE.new_claim_status_code "
				ls_WhereClause = ls_WhereClause + " AND CLAIM_STATUS_CHANGE.new_claim_status_code = 'F' "
				ls_WhereClause = ls_WhereClause + " AND CLAIM_STATUS_CHANGE.create_date < DateAdd(day, -84, getdate()))"
				
				ls_Additional_WhereClause = " AND ( CLAIM.claim_status_code <> 'R') AND (CLAIM.claim_status_code <> 'F')" //2014-09-18 David Worboys 
				/*end changes for SR12 and SR13
				*/
				
				ls_FieldValue = dw_search_criteria.GetItemString(1,"user_id")
				IF Trim(ls_FieldValue)  > "" THEN
					ls_Additional_WhereClause = ls_Additional_WhereClause + " and (CLAIM.claim_manager_user_id = ~~~"" + ls_FieldValue + "~~~")"
				END IF
	
				ls_FieldValue = dw_search_criteria.GetItemString(1,"region")
				IF Trim(ls_fieldvalue) > "" THEN
					ls_Additional_WhereClause = ls_Additional_WhereClause + "  and INDIVIDUAL_NAME.name_type_code = ~~~"M~~~" and (CLAIM.admin_region_code = ~~~"" + ls_FieldValue + "~~~")"
				END IF
					
					
	/*			Modify the data window's syntax.....   
	*/
				
				ls_ModString = "DataWindow.Table.Select=~"" + ls_SQLSyntax + ls_WhereClause + ls_Additional_WhereClause + "~""
				ls_ReturnCode = dw_search_list.Modify(ls_ModString)
				IF Len(ls_ReturnCode) > 0 Then
					Return -1
				Else
					Return 1
				END IF
				
			ELSEIF is_review_type = "AR" THEN
				// do not modify, if it is an annual review search
				
			ELSE
				MessageBox("Error...","Unable to determine Review Type. Please try again.",Exclamation!)
				Return -1
			END IF



		CASE 'd_to_do_search'
			
			//Medical Management project changes
			//Don't include "Action Items"
			IF is_SQLSyntax_to_do = '' Then
				is_SQLSyntax_to_do = dw_search_list.Describe("DataWindow.Table.Select")
			End if
			
			ls_SQLSyntax = is_SQLSyntax_to_do
			
			ls_WhereClause = "   WHERE REHAB_TASK.task_type_code <> 'AC' "   
  


			ls_FieldValue = dw_search_criteria.GetItemString(1,"user_id")
			IF Trim(ls_FieldValue)  > "" THEN
				ls_Additional_WhereClause = ls_Additional_WhereClause + " and (REHAB_TASK.responsible_user_id = ~~~"" + ls_FieldValue + "~~~")"
			END IF

			ls_FieldValue = dw_search_criteria.GetItemString(1,"claim_manager_user_id")
			IF Trim(ls_FieldValue)  > "" THEN
				ls_Additional_WhereClause = ls_Additional_WhereClause + " and (CLAIM.claim_manager_user_id = ~~~"" + ls_FieldValue + "~~~")"
			END IF

			ls_FieldValue = dw_search_criteria.GetItemString(1,"task_status_code")
			IF Trim(ls_FieldValue)  > "" THEN
				ls_Additional_WhereClause = ls_Additional_WhereClause + " and (REHAB_TASK.task_status_code = ~~~"" + ls_FieldValue + "~~~")"
			END IF


			ldt_from_date = dw_search_criteria.GetItemDate(1,'from_date')
			ldt_to_date = dw_search_criteria.GetItemDate(1,'to_date')
			IF dw_search_criteria.GetItemString(1,"task_status_code") = '01' AND NOT IsNull(ldt_to_date) THEN
			
				lb_twodates = false
				IF NOT IsNull(ldt_from_date) then lb_twodates = true
				IF lb_twodates THEN
					ls_Additional_WhereClause = ls_Additional_WhereClause + " AND ( (REHAB_TASK.planned_start_date <= ~~~"" + String(ldt_to_date,'yyyy-mm-dd') + "~~~") and (REHAB_TASK.planned_start_date >= ~~~"" + String(ldt_from_date,'yyyy-mm-dd') + "~~~"))"
				ELSE
					ls_Additional_WhereClause = ls_Additional_WhereClause + " AND ( REHAB_TASK.planned_start_date <= ~~~"" + String(ldt_to_date,'yyyy-mm-dd') + "~~~")" 
				END IF
			
			ELSEIF dw_search_criteria.GetItemString(1,"task_status_code") = '02' AND &
			NOT IsNull(ldt_to_date) THEN
				ls_Additional_WhereClause = ls_Additional_WhereClause + " and (REHAB_TASK.planned_completion_date <= ~~~"" + String(ldt_to_date,'yyyy-mm-dd')  + "~~~")"
				IF NOT IsNull(ldt_from_date) THEN
					ls_Additional_WhereClause = ls_Additional_WhereClause + " and (REHAB_TASK.planned_completion_date >= ~~~"" + String(ldt_from_date,'yyyy-mm-dd') + "~~~")"
				END IF
			END IF
			
			

/*			Modify the data window's syntax.....   
*/
			
			ls_ModString = "DataWindow.Table.Select=~"" + ls_SQLSyntax + ls_WhereClause + ls_Additional_WhereClause + "~""
			ls_ReturnCode = dw_search_list.Modify(ls_ModString)
			IF Len(ls_ReturnCode) > 0 Then
				Return -1
			Else
				Return 1
			END IF
		
		
	CASE 'd_claim_rx_termination_search'
		dw_search_criteria.accepttext()


		IF Len(ls_ReturnCode) > 0 Then
			Return -1
		
		ELSE
			// now build the filter string. Use is_filter, an instance variable, to hold value
			// so that it can be reapllied after a user has cleared their own filter 
			// (applied from the filter contol object)
			ls_FieldValue = dw_search_criteria.getItemString(1,"eligibility_type")
			is_filter = "eligibility_type = '" + ls_FieldValue + "'"
			
			ls_FieldValue = dw_search_criteria.getItemString(1,"claim_status_code")
			IF not isnull(ls_FieldValue) AND Len(ls_FieldValue) > 0 THEN
				is_filter = is_filter + " and claim_status_code = '" +ls_FieldValue + "'"
			END IF
			
			ls_FieldValue = dw_search_criteria.getItemString(1,"user_id")
			IF not isnull(ls_FieldValue) AND Len(ls_FieldValue) > 0 THEN
				is_filter = is_filter + " and claim_manager_user_id = '" +ls_FieldValue + "'"
			END IF
			
			ls_FieldValue = dw_search_criteria.getItemString(1,"region")
			IF not isnull(ls_FieldValue) AND Len(ls_FieldValue) > 0 THEN
				is_filter = is_filter + " and admin_region_code = '" +ls_FieldValue + "'"
			END IF			
			dw_search_list.setfilter( is_filter)  // don't need to call the filter() method, when dw retrieves later, filter is applied by PB
			
			return 1
		END IF		
		Return 1
	
	CASE "d_individual_number_search"
	
	/*	Build the WHERE clause for the basic claim search......
	*/
		ls_Additional_WhereClause = ls_Additional_WhereClause + " and (INDIVIDUAL.individual_no = " + String(il_individual_no) + ")"
	
	/*	Modify the data window syntax of both the search list datawindow and the search list individual.....   
	*/
		ls_ModString = "DataWindow.Table.Select=~"" + is_SQLSyntaxI + is_WhereClauseI + ls_Additional_WhereClause + "~""
		ls_ReturnCode = dw_search_list_individual.ModIFy(ls_ModString)
	
		ls_ModString = "DataWindow.Table.Select=~"" + is_SQLSyntax + is_WhereClause + ls_Additional_WhereClause + "~""
		ls_ReturnCode = dw_search_list.ModIFy(ls_ModString)
		IF Len(ls_ReturnCode) > 0 Then
			Return -1
		END IF
		
		ls_SQLSyntax = is_SQLSyntax_individual_annuity_list + ls_Additional_WhereClause
		ls_ModString = "DataWindow.Table.Select=~"" + ls_SQLSyntax + "~""
		ls_ReturnCode = dw_search_list_annuity_individual.ModIFy(ls_ModString)
		ls_ModString = dw_search_list_annuity_individual.getSQLSelect()
		IF Len(ls_ReturnCode) > 0 Then
			Return -1
		ELSE
			return 1
		END IF
	
			
	CASE  "d_individual_search"
/*			Build the WHERE clause for the basic individual search......
*/		ls_FieldValue = dw_search_criteria.getItemString(1,'condition_or')
		IF ls_FieldValue = 'Y' THEN
			ls_and_or = ' OR '
		ELSE
			ls_and_or = ' AND '
		END IF

		IF is_last_name > "" THEN
			IF is_name_search_option = "B" THEN
				ls_Additional_WhereClause = ls_Additional_WhereClause + " and (INDIVIDUAL_NAME.last_name like ~~~"" + is_last_name + "%~~~" COLLATE SQL_Latin1_General_CP850_CI_AI)"
			ELSEIF is_soundex_search_option = "0" THEN
				ls_Additional_WhereClause = ls_Additional_WhereClause + " and (INDIVIDUAL_NAME.last_name = ~~~"" + is_last_name + "~~~")"
			ELSE
				ls_Additional_WhereClause = ls_Additional_WhereClause + " and (INDIVIDUAL_NAME.soundex_last_name = Soundex(~~~"" + is_last_name + "~~~"))"
			END IF
		END IF
		IF is_first_name > "" THEN
			IF is_name_search_option = "B" THEN
				ls_Additional_WhereClause = ls_Additional_WhereClause + " and (INDIVIDUAL_NAME.given_names like ~~~"" + is_first_name + "%~~~" COLLATE SQL_Latin1_General_CP850_CI_AI)"
			ELSEIF is_soundex_search_option = "0" THEN
				ls_Additional_WhereClause = ls_Additional_WhereClause + " and (INDIVIDUAL_NAME.given_names = ~~~"" + is_first_name + "~~~")"
			ELSE
				ls_Additional_WhereClause = ls_Additional_WhereClause + " and (INDIVIDUAL_NAME.soundex_given_names = Soundex(~~~"" + is_first_name + "~~~"))"
			END IF
		END IF
		
		IF il_sin > 0 THEN
			ls_Additional_WhereClause = ls_Additional_WhereClause + " and ( (INDIVIDUAL.sin_no = " + string(il_sin) + ")"
		ELSE
			lb_no_sin = TRUE
		END IF
		
		IF il_medicare > 0 THEN
			IF lb_no_sin THEN
				ls_Additional_WhereClause = ls_Additional_WhereClause + " and ( (INDIVIDUAL.medicare_no = " + string(il_medicare) + ")"
			ELSE
				ls_Additional_WhereClause = ls_Additional_WhereClause + ls_and_or + " (INDIVIDUAL.medicare_no = " + string(il_medicare) + ")"
			END IF		
		ELSE
			lb_no_medicare = TRUE
		END IF
	
		IF String(id_birth_date) > "" THEN
			IF lb_no_sin AND lb_no_medicare THEN
				ls_Additional_WhereClause = ls_Additional_WhereClause +  " and ( (INDIVIDUAL.birth_date = '" + String(id_birth_date,"yyyy-mm-dd") + "')"
			ELSE
				ls_Additional_WhereClause = ls_Additional_WhereClause + ls_and_or + " (INDIVIDUAL.birth_date = '" + String(id_birth_date,"yyyy-mm-dd") + "')"
			END IF
		ELSE
			lb_no_birthdate = TRUE
		END IF
		
		IF lb_no_sin AND lb_no_medicare AND lb_no_birthdate THEN
			//nothing to add
		ELSE
			ls_Additional_WhereClause = ls_Additional_WhereClause + ")"   // add outer parentheses
		END IF
		
		ls_SQLSyntax = is_SQLSyntax_basic_individual
		
		f_replace_text (ls_Additional_WhereClause,'~~','')

		f_replace_text (ls_SQLSyntax,'-- add_to_where_clause_here',ls_Additional_WhereClause)

		li_rtn = dw_search_list.setSQLSelect(ls_SQLSyntax)
		IF li_rtn < 0 Then
			Return -1
		Else
			Return 1
		END IF	
		
	END CHOOSE


end function

public function integer uf_validate_criteria ();STRING	  ls_validate_string, ls_string, ls_rehab_officer_user_id, ls_occupational_therapist_user_id
DATE		  ldt_from, ldt_to
DATETIME   ldtm_null,ldtm_today

/*	
This function validates the search criteria.
It returns '1' if the search criteria is OK and -1 if there
are errors.
*/

CHOOSE CASE is_searchtype

	CASE "d_claim_number_search"
		il_claim_no = dw_search_criteria.GetItemNumber(1,"claim_no")
		IF il_claim_no < 1 OR &
			IsNull(il_claim_no) THEN
			MessageBox("Invalid Search Criteria","Missing claim number!",Exclamation!)
			Return -1
		END IF
	
	CASE "d_individual_number_search"
		il_individual_no  = dw_search_criteria.GetItemNumber(1,"individual_no")
		IF il_individual_no < 1 OR IsNull(il_individual_no) THEN			
			MessageBox("Invalid Search Criteria","Missing or invalid individual number!",Exclamation!)
			Return -1
		END IF

	CASE "d_basic_claim_search", "d_individual_search"

		is_last_name          = dw_search_criteria.GetItemString(1,"last_name")
		is_first_name         = dw_search_criteria.GetItemString(1,"first_name")
		is_name_search_option = dw_search_criteria.GetItemString(1,"name_search_option")
		is_soundex_search_option = dw_search_criteria.GetItemString(1,"soundex_search_option")
		il_sin                = Long(dw_search_criteria.GetItemString(1,"sin"))
		il_medicare           = Long(dw_search_criteria.GetItemString(1,"medicare"))
		id_birth_date         = dw_search_criteria.GetItemdate(1,"birth_date")

/*		Aug20/96 -error if soundex is on then name search must be exact */
		IF is_soundex_search_option = '1' THEN
			is_name_search_option = 'E'
		END IF
		IF (IsNull(is_last_name) OR is_last_name = "") AND &
			(IsNull(is_first_name) OR is_first_name = "") AND &
			(IsNull(il_sin) OR il_sin = 0) AND &
			(IsNull(il_medicare) OR il_medicare = 0) AND &
			IsNull(id_birth_date) THEN
			MessageBox("Invalid Search Criteria","No search values have been entered!",Exclamation!)
			Return -1
		End IF

		IF is_last_name > "" and &
			Len(is_last_name) < 2 THEN
			MessageBox("Invalid Search Criteria","Last Name must contain at least 3 characters!",Exclamation!)
			Return -1
		End if

		IF Pos(is_last_name,'"') > 0 THEN
			MessageBox("Invalid Search Criteria","Last Name cannot contain double quotation marks!",Exclamation!)
			Return -1
		End IF

		IF Pos(is_last_name,'%') > 0 THEN
			MessageBox("Invalid Search Criteria","Last Name cannot contain percent sign!",Exclamation!)
			Return -1
		End IF

		IF Pos(is_last_name,'_') > 0 THEN
			MessageBox("Invalid Search Criteria","Last Name cannot contain an underscore!",Exclamation!)
			Return -1
		End IF

		IF Pos(is_first_name,'"') > 0 THEN
			MessageBox("Invalid Search Criteria","First Name cannot contain double quotation marks!",Exclamation!)
			Return -1
		End IF

		IF Pos(is_first_name,'%') > 0 THEN
			MessageBox("Invalid Search Criteria","First Name cannot contain percent sign!",Exclamation!)
			Return -1
		End IF

		IF Pos(is_first_name,'_') > 0 THEN
			MessageBox("Invalid Search Criteria","First Name cannot contain an underscore!",Exclamation!)
			Return -1
		End IF

		IF il_sin > 0 THEN

			IF f_CheckDigit(String(il_sin),8) < 0 THEN
				MessageBox("Invalid Search Criteria","Invalid Social Insurance Number!",Exclamation!)
				Return -1
			End if
		END IF

		IF il_medicare > 0 THEN
			IF Len(String(il_medicare)) <> 9 THEN
				MessageBox("Invalid Search Criteria","Medicare Number must contain 9 digits.",Exclamation!)
				Return -1
			End IF

			IF f_CheckDigit(String(il_medicare),8) < 0 THEN
				MessageBox("Invalid Search Criteria","Invalid Medicare Number!",Exclamation!)
				Return -1
			END IF
		END IF

		IF NOT IsNull(id_birth_date) THEN
			IF id_birth_date < Date(1900,01,01) THEN
				MessageBox("Validation Error","The birth date cannot be earlier than 1900-01-01!",Exclamation!)
				Return -1
			END IF

			IF id_birth_date > Date(f_server_datetime())  THEN
				MessageBox("Validation Error","The birth date cannot be in the future!",Exclamation!)
				Return -1
			END IF

		END IF

	CASE "d_case_load_search2"

		ls_validate_string = dw_search_criteria.GetItemString(1,"region")
		IF IsNull(ls_validate_string) OR Trim(ls_validate_string) = "" THEN
			MessageBox("Invalid Search Criteria","You must provide a region!",Exclamation!)
			Return -1
		End IF

		ls_validate_string = dw_search_criteria.GetItemString(1,"claim_status_code")
		IF IsNull(ls_validate_string) OR Trim(ls_validate_string) = "" THEN
			MessageBox("Invalid Search Criteria","You must provide a claim status!",Exclamation!)
			Return -1
		End IF

		IF not ls_validate_string = "A" and not ls_validate_string = "P" and not ls_validate_string = "J" and not ls_validate_string = "N" THEN
			ls_validate_string = dw_search_criteria.GetItemString(1,"user_id")
			ls_rehab_officer_user_id = Trim(dw_search_criteria.GetItemString(1,"rehab_officer_user_id"))
			ls_occupational_therapist_user_id = Trim(dw_search_criteria.GetItemString(1,"occupational_therapist_user_id"))
			
			IF (IsNull(ls_validate_string) OR Trim(ls_validate_string) = "") AND &
			   (IsNull(ls_occupational_therapist_user_id) OR ls_occupational_therapist_user_id = "") AND &
				(IsNull(ls_rehab_officer_user_id) OR ls_rehab_officer_user_id = "") THEN
				MessageBox("Invalid Search Criter","When the claim status is other than Pre-Adjudication, Adjudication or Active, ~r~n" + &
								"you must provide a claim manager or rehab manager or Occupational Therapist to narrow the search list",Exclamation!)
				Return -1
			END IF
		END IF

	CASE "d_due_overdue_list"

		is_review_type        = dw_search_criteria.GetItemString(1,'required_action')
		is_overdue_ind        = dw_search_criteria.GetItemString(1,'overdue_ind')
		is_opening_type_code  = dw_search_criteria.GetItemString(1,'opening_type_code')
		is_rb_doc_type        = dw_search_criteria.GetItemString(1,'rb_doc_type')
		IF is_rb_doc_type = 'X' THEN
			// neither chosen
			is_rb_doc_type = ''
		END IF
		
		idt_benefit_from_date = DateTime(Date(dw_search_criteria.GetItemDateTime(1,'opening_from_date')))
		idt_benefit_to_date   = dw_search_criteria.GetItemDateTime(1,'opening_to_date')
		idt_bdate_from_date   = dw_search_criteria.GetItemDateTime(1,'bdate_from_date')
		idt_bdate_to_date     = dw_search_criteria.GetItemDateTime(1,'bdate_to_date')

/*		Date validations only apply to the 'Due' claims, not the 'Overdue' claims, so only do the
		validations if the requested list is for the 'Due' claims.
*/
		IF is_overdue_ind = "D" THEN
			IF IsNull(idt_benefit_from_date) THEN
				MessageBox("Invalid Search Criteria","You must provide a From Date!",Exclamation!)
				Return -1
			End IF

			IF IsNull(idt_benefit_to_date) THEN
				MessageBox("Invalid Search Criteria","You must provide a To Date!",Exclamation!)
				Return -1
			End IF

			IF idt_benefit_from_date > idt_benefit_to_date THEN
				MessageBox("Invalid Search Criteria","From Date must not be greater than the To Date!",Exclamation!)
				Return -1
			END IF
		ELSEIF is_overdue_ind = 'O' THEN
			IF is_review_type = 'AR' THEN				
				SetNull(ldtm_null)
				idt_benefit_from_date = ldtm_null
				ldtm_today = DateTime(Today())
				idt_benefit_to_date   = ldtm_today
			END IF
		END IF
		
//		IF IsNull(idt_bdate_to_date) THEN
//			idt_bdate_to_date = DateTime(2079-06-06)
//		END IF
		
		IF IsNull(idt_bdate_from_date) THEN
			// OK
		ELSE
			IF idt_bdate_from_date > idt_bdate_to_date THEN
				MessageBox("Invalid Search Criteria","The 'Birthdate' From Date must not be greater than the 'Birthdate' To Date!",Exclamation!)
				Return -1
			END IF
		END IF
		
		
	CASE "d_to_do_search"

		ls_string = dw_search_criteria.GetItemString(1,'task_status_code')
		ldt_from = dw_search_criteria.GetItemdate(1,"from_date")
		ldt_to = dw_search_criteria.GetItemdate(1,"to_date")
		
		IF ls_string = '01' or ls_string = '02' THEN
			IF  IsNull(ldt_to) AND NOT IsNull(ldt_from) THEN
				MessageBox("Invalid Search Criteria","You must provide a to Date when a from date is entered!",Exclamation!)
				Return -1
			END IF
			IF NOT IsNull(ldt_from) THEN
				IF ldt_from > ldt_to THEN
					MessageBox("Invalid Search Criteria","From Date must not be greater than the To Date!",Exclamation!)
					Return -1
				END IF
			END IF
		ELSE
			IF NOT (IsNull(ldt_to) OR IsNull(ldt_from)) THEN
				MessageBox('Warning', 'The date are only applicable for planned and in progress status, they will be ignored.')
			END IF
		END IF
			
	CASE 'd_claim_rx_termination_search'		
		ls_string = STRING(dw_search_criteria.getItemNumber(1, 'days_to_termination'))
		if isnumber(ls_string)then
			if (abs(integer(ls_string))) > 365 THEN
				MessageBox("Information","Number of days exceeds 365",Information!)
			
			end if
		else
			MessageBox("Invalid criteria","Must enter a valid number",Information!)
			return -1
		end if		
		
		IF dw_search_criteria.GetItemNumber(1,'drug_payment_days') < 0 Then
			MessageBox('Invalid number of days','Drugs payment days must be a positive number.',StopSign!,Ok!)
			dw_search_criteria.SetFocus()
			dw_search_criteria.SetColumn('drug_payment_days')
			return -1
		End if

END CHOOSE

Return 1
end function

public subroutine uf_shrink_enlarge (readonly string as_arg);

If as_arg = "ENLARGE" Then

	il_normal_y = dw_search_list.y
	il_normal_height = dw_search_list.height
//	Move and resize the height of the Document List/Index

	dw_search_criteria.Visible = False
	dw_search_type.Visible = False
	IF dw_search_list_individual.RowCount() > 0 THEN
		cb_more.Visible = False
	END IF
	IF dw_search_list_annuity_individual.RowCount() > 0 THEN
		cb_annuity_only_list.Visible = False
	END IF
	dw_search_list.Y = 1
	dw_search_list.Height=il_normal_y + il_normal_height - dw_search_list.y
	
	dw_search_list_individual.Y = 1
	dw_search_list_individual.Height=il_normal_y + il_normal_height - dw_search_list_individual.y

	dw_search_list_annuity_individual.Y = 1
	dw_search_list_annuity_individual.Height=il_normal_y + il_normal_height - dw_search_list_annuity_individual.y

//	Scroll the current row into view
	IF dw_search_list.GetRow() > 0 THEN
		dw_search_list.ScrollToRow(dw_search_list.GetRow())
	END IF
	IF dw_search_list_individual.GetRow() > 0 THEN
		dw_search_list_individual.ScrollToRow(dw_search_list_individual.GetRow())
	END IF
	IF dw_search_list_annuity_individual.GetRow() > 0 THEN
		dw_search_list_annuity_individual.ScrollToRow(dw_search_list_annuity_individual.GetRow())
	END IF
	

Else
	
	
	
	//	Move and resize the height of the Document List/Index  

	dw_search_criteria.Visible = True
	dw_search_type.Visible = True
	IF dw_search_list_individual.RowCount() > 0 THEN
		cb_more.Visible = True
	END IF
	IF dw_search_list_annuity_individual.RowCount() > 0 THEN
		cb_annuity_only_list.Visible = TRUE
	END IF
	
	
	dw_search_list.Y=561
	CHOOSE CASE dw_search_list.dataobject
		CASE 'd_rx_termination_coverage_list','d_search_list_annual_review'
			dw_search_list.Height=1100
		CASE ELSE
			dw_search_list.Height=1213
	END CHOOSE
	
	dw_search_list_individual.Y = 561
	dw_search_list_individual.Height=1209
	
	dw_search_list_annuity_individual.Y = 561
	dw_search_list_annuity_individual.Height=1209

//	Scroll the current row into view
	IF dw_search_list.GetRow() > 0 THEN
		dw_search_list.ScrollToRow(dw_search_list.GetRow())
	END IF
	IF dw_search_list_individual.GetRow() > 0 THEN
		dw_search_list_individual.ScrollToRow(dw_search_list_individual.GetRow())
	END IF
	IF dw_search_list_annuity_individual.GetRow() > 0 THEN
		dw_search_list_annuity_individual.ScrollToRow(dw_search_list_annuity_individual.GetRow())
	END IF

End If
end subroutine

public subroutine uf_set_search_defaults (s_claim_search_defaults astr_claim_search_defaults, string as_search_type);
	IF as_search_type = 'd_basic_claim_search' THEN
		IF astr_claim_search_defaults.medicare_no <> 0 THEN 
			dw_search_criteria.SetItem(1,'medicare',String(astr_claim_search_defaults.medicare_no))
		END IF
		
		IF astr_claim_search_defaults.sin_no <> 0 THEN 
			dw_search_criteria.SetItem(1,'sin',String(astr_claim_search_defaults.sin_no))
		END IF
		
		IF astr_claim_search_defaults.given_names > '' THEN
			dw_search_criteria.SetItem(1,'first_name',astr_claim_search_defaults.given_names)
		END IF
		
		IF astr_claim_search_defaults.last_name > '' THEN
			dw_search_criteria.SetItem(1,'last_name',astr_claim_search_defaults.last_name)
		END IF
		
		IF String(astr_claim_search_defaults.birth_date) <> '' THEN
			dw_search_criteria.SetItem(1,'birth_date',astr_claim_search_defaults.birth_date)
		END IF
		
	ELSEIF as_search_type = 'd_claim_number_search' THEN
		dw_search_criteria.SetItem(1,'claim_no',astr_claim_search_defaults.claim_no)
	END IF

	cb_search.triggerevent('clicked')

end subroutine

protected function integer uf_retrieve ();/*	This function retrieves d_search_list.
	It returns the number of rows retrieved.
	Set the .Redraw option to false while the datawindow is being
	loaded.
*/

DATASTORE lds_ultimate_individual_no
INTEGER   li_rows, li_individual_merge_count, li_unarchived_count
LONG      ll_ReturnCode, ll_days_to_termination, ll_payment_days, ll_ultimate_individual_no
STRING    ls_coverage_type, ls_claim_manager_user_id, ls_admin_region_code



iwi_window_parent.SetRedraw(False)
dw_search_list_individual.visible = FALSE
dw_search_list.visible = TRUE

IF is_searchtype = 'd_basic_claim_search' OR is_searchtype = 'd_individual_number_search' THEN
	ll_ReturnCode = dw_search_list_individual.Retrieve()
	SQLCA.nf_handle_error("u_claim_search","dw_search_list_individual","uf_retrieve")

	IF ll_ReturnCode > 0 THEN
		cb_more.text = "Other Claim Participant &List"
	   cb_more.visible = TRUE
	ELSE
		cb_more.visible = FALSE
	END IF
		
	ll_ReturnCode = dw_search_list_annuity_individual.Retrieve()
	SQLCA.nf_handle_error("u_claim_search","dw_search_list_individual","uf_retrieve")
		
	IF ll_ReturnCode > 0 THEN	      
		cb_annuity_only_list.visible = TRUE
	ELSE
		cb_annuity_only_list.visible = FALSE
   END IF
	
END IF


/*	Reset the search list datawindow so that a rowfocuschange will always
	fire off -- then retrieve
*/
dw_search_list.Reset()
	
IF ib_force_plan THEN
	EXECUTE IMMEDIATE 'set forceplan on' USING SQLCA;
	IF SQLCA.nf_handle_error('Setting force plan on', 'u_claim_search', 'uf_retrieve') < 0 THEN
		Return -1
	END IF
END IF
	
IF is_searchtype = 'd_claim_rx_termination_search' Then
	ll_days_to_termination =dw_search_criteria.getItemNumber(1,"days_to_termination")
	ll_payment_days = dw_search_criteria.getItemNumber(1,"drug_payment_days")
	ls_coverage_type = dw_search_criteria.GetItemString(1,"coverage_type")

	ll_ReturnCode = dw_search_list.Retrieve(ll_days_to_termination,ll_payment_days,ls_coverage_type)
	SQLCA.nf_handle_error("u_claim_search","dw_search_list","uf_retrieve")
ELSEIF is_searchtype = 'd_due_overdue_list' AND is_review_type = 'AR' THEN
  
   ls_claim_manager_user_id = dw_search_criteria.GetItemString(1,'user_id')
   ls_admin_region_code     = dw_search_criteria.GetItemString(1,'region')
  
   ll_ReturnCode = dw_search_list.Retrieve(ls_claim_manager_user_id,ls_admin_region_code,is_opening_type_code,idt_benefit_from_date,idt_benefit_to_date,idt_bdate_from_date,idt_bdate_to_date,is_rb_doc_type,is_overdue_ind)
	SQLCA.nf_handle_error('u_claim_search','dw_search_list','uf_retrieve')
	
ELSE
	ll_ReturnCode = dw_search_list.Retrieve()
	SQLCA.nf_handle_error("u_claim_search","dw_search_list","uf_retrieve")
	
	IF is_searchtype = 'd_individual_number_search' THEN
		
		SELECT Count(*)
		INTO   :li_unarchived_count
		FROM   INDIVIDUAL
		WHERE  individual_no = :il_individual_no
		USING SQLCA;
		SQLCA.nf_handle_error('u_claim_search','SELECT Count(*) FROM INDIVIDUAL','uf_retrieve')
				
		IF li_unarchived_count = 0 THEN
			
			// determine if entered individual number was involved in a merge
			lds_ultimate_individual_no = Create DATASTORE
			lds_ultimate_individual_no.DataObject = 'ds_ultimate_individual_no'
			lds_ultimate_individual_no.SetTransObject(SQLCA)
			li_rows = lds_ultimate_individual_no.Retrieve(il_individual_no)
			SQLCA.nf_handle_error('u_claim_search','lds_ultimate_individual_no.retrieve','uf_retrieve')
			
			IF li_rows > 0 THEN
				ll_ultimate_individual_no = lds_ultimate_individual_no.GetItemNumber(1,'new_individual_no')
				
				// return the 'ultimate' new number in a messagebox
				IF MessageBox('Merged Individual','This individual has been merged with other individuals.' &
				                            +'~r~nThe most recent number assigned to this individual is ' + String(ll_ultimate_individual_no) + '.' &
													 +'~r~n' &
													 +'~r~nDo you want to search using the new individual number?',Question!,YesNo!,2) = 1 THEN
					// give user an opportunity to search, using this new number
					dw_search_criteria.SetItem(1,'individual_no',ll_ultimate_individual_no)
					cb_search.TriggerEvent(clicked!)
					RETURN 1
			   END IF
         END IF
			
		END IF
	END IF	
END IF

IF ib_force_plan THEN
	EXECUTE IMMEDIATE 'set forceplan off' USING SQLCA;
	
	IF SQLCA.nf_handle_error('Setting force plan off', 'u_claim_search', 'uf_retrieve') < 0 THEN
		Return -1
	END IF
END IF
		

iwi_window_parent.SetRedraw(True)
dw_search_list.SetFocus()
			

Return ll_ReturnCode
end function

public subroutine uf_set_dw_object (string as_dw_object, string as_due_overdue_type);INTEGER				li_ReturnCode, li_visible
DATAWINDOWCHILD	ldwc_child
STRING ls_sort_criteria



/*	Set the datawindow control to use the datawindow object required for the search
	type just selected 
*/
	dw_search_list_individual.Reset()
	dw_search_list_individual.visible = FALSE
	cb_more.visible = FALSE
	dw_search_list.visible = TRUE
	
	
	dw_search_list_annuity_individual.Reset()
	dw_search_list_annuity_individual.visible = FALSE
	
	cb_annuity_only_list.visible = FALSE

	dw_search_list.DataObject = 'd_search_list'
	dw_search_list.SetTransObject(SQLCA)
	
	dw_search_criteria.DataObject = as_dw_object
	dw_search_criteria.SetTransObject(SQLCA)	//	So that we don't have to retrieve all drop downs using the dwchild function.
	dw_search_criteria.Reset()
	li_ReturnCode = dw_search_criteria.InsertRow(0)

/* make filter control and extract button invisible in case it has been set visible by one of the search types
	and bring dw_search back to originale size. (we need the extra room for rx termination datawindow, for filter control)
	This code can be removed if you make these items visible for all other datawindows on this object
*/
	uo_filter_control.visible= false
	dw_search_list.height = 1208
	cb_extract.visible=false
/*	If the selected search criteria datawindow has any defaults to set, do it now!   */

CHOOSE CASE is_SearchType

	CASE "d_basic_claim_search"

		dw_search_criteria.SetItem(1,"name_search_option","B")
		dw_search_criteria.SetItem(1,"soundex_search_option","0")

	CASE "d_case_load_search2"

/*		Load the claim status type drop down data window.
*/
		IF dw_search_criteria.GetChild("claim_status_type_code",idwc_claim_status_type_code) < 0 THEN
			MessageBox("DataWindow Error","Unable to retrieve claim status type codes!",StopSign!)
			Return
		END IF
		idwc_claim_status_type_code.SetTransObject(SQLCA)
		idwc_claim_status_type_code.Retrieve()

/*		Load the claim manager drop down data window, then 
		filter the data.
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

/*		Load the rehab type drop down data window, then 
		filter the data.
*/
		IF dw_search_criteria.GetChild("rehab_type_code",ldwc_child) < 0 THEN
			MessageBox("DataWindow Error","Unable to load rehab type list!",StopSign!)
			Return
		END IF
		ldwc_child.SetTransObject(SQLCA)
		ldwc_child.Retrieve()
		ldwc_child.SetSort("rehab_type_desc A")
		ldwc_child.Sort()
		ldwc_child.InsertRow(1)
		ldwc_child.SetItem(1,"rehab_type_desc","")
		ldwc_child.SetItem(1,"rehab_type_code","")

/*		Load the Region drop down data window, then filter for active region codes
*/
		IF dw_search_criteria.GetChild("region",ldwc_child) < 0 THEN
			MessageBox("DataWindow Error","Unable to load admin region list!",StopSign!)
			Return
		End If
		ldwc_child.SetTransObject(SQLCA)
		ldwc_child.Retrieve()
		ldwc_child.SetFilter("active_flag = 'Y'")
		ldwc_child.Filter()

/*		Load the Opening Type drop down data window, then insert a blank row and filter the data
*/
		li_ReturnCode = dw_search_criteria.GetChild("opening_type_code",ldwc_child)
		If li_ReturnCode < 0 Then
			MessageBox("DataWindow Error","Unable to retrieve opening types!",StopSign!)
			Return
		End If
		ldwc_child.SetTransObject(SQLCA)
		ldwc_child.Retrieve()
		ldwc_child.SetFilter("active_flag = 'Y'")
		ldwc_child.Filter()
		ldwc_child.InsertRow(1)
		ldwc_child.SetItem(1,"opening_type_code","")
		ldwc_child.SetItem(1,"opening_type_desc","")

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

/*		Defaults: set the region code to the users default admin region code;  
		set the status to 'Active' and filter the status_type drop down dw;
		default the claim manager to the current user (if this user is a claim manager).
*/
		dw_search_criteria.SetItem(1,"region",vgst_user_profile.default_admin_region_code)

		IF idwc_claim_manager.Find("user_id = '" + vgst_user_profile.user_id + "'",1,idwc_claim_manager.RowCount()) > 0 THEN
			dw_search_criteria.SetItem(1,"user_id",vgst_user_profile.user_id)
		END IF

		dw_search_criteria.SetItem(1,"claim_status_code","N")
		idwc_claim_status_type_code.SetFilter("claim_status_code = 'A'")
		idwc_claim_status_type_code.Filter()
		
	CASE "d_stats_not_coded"
		
		/* The stats coded search results get sorted differently the all the others
			*/
			ls_sort_criteria = "claim_claim_status_code A, accident_date A, claim_no A"
			IF dw_search_list.SetSort(ls_sort_criteria) = -1 THEN
				SignalError(-666,"Error sorting the search results")
			End if
			dw_search_list.Sort()
	
	CASE "d_due_overdue_list"
		
		CHOOSE CASE as_due_overdue_type
			CASE '','12W'
				dw_search_list.DataObject = 'd_search_list'
				dw_search_list.SetTransObject(SQLCA)
				cb_extract.visible = FALSE
				
				li_visible = 0
			CASE 'AR'
				dw_search_list.DataObject = 'd_search_list_annual_review'
				dw_search_list.SetTransObject(SQLCA)
				
				dw_search_list.height = 1100
				cb_extract.visible = TRUE
				
				li_visible = 1
		END CHOOSE
		
		dw_search_criteria.SetItem(1,"overdue_ind","D")
		dw_search_criteria.SetItem(1,"opening_from_date",Today())
		dw_search_criteria.SetItem(1,"required_action",as_due_overdue_type)
		
/*		Load the claim manager drop down data window, then 
		filter the data.
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

/*		Load the Region drop down data window, then filter for active region codes
*/
		IF dw_search_criteria.GetChild("region",ldwc_child) < 0 THEN
			MessageBox("DataWindow Error","Unable to load admin region list!",StopSign!)
			Return
		End If
		ldwc_child.SetTransObject(SQLCA)
		ldwc_child.Retrieve()
		ldwc_child.InsertRow(1)
		ldwc_child.SetItem(1,"admin_region_code","")
		ldwc_child.SetItem(1,"admin_region_desc","")
		ldwc_child.Filter()
		
		// some columns are only visible for AR (annual review), but not 12W (12 week review)		
		dw_search_criteria.Object.rb_doc_type.Visible       = li_visible
		dw_search_criteria.Object.t_rb_doc_type.Visible     = li_visible
		
		dw_search_criteria.Object.opening_type_code.Visible   = li_visible
		dw_search_criteria.Object.t_opening_type_code.Visible = li_visible
					
		dw_search_criteria.Object.bdate_from_date.Visible   = li_visible
		dw_search_criteria.Object.t_bdate_from_date.Visible = li_visible
		
		dw_search_criteria.Object.bdate_to_date.Visible   = li_visible
		dw_search_criteria.Object.t_bdate_to_date.Visible = li_visible
		
		
	CASE 'd_to_do_search'
/* 	switch to the new 'list'
*/
		dw_search_list.DataObject = 'd_to_do_list'
		dw_search_list.SetTransObject(SQLCA)
		
		dw_search_criteria.SetItem(1,'user_id',vgst_user_profile.user_id)
		dw_search_criteria.SetItem(1,'task_status_code','01')
	
	CASE "d_claim_rx_termination_search"
		
		IF dw_search_criteria.GetChild("claim_status_code",idwc_claim_status_type_code) < 0 THEN
			MessageBox("DataWindow Error","Unable to load claim status type list!",StopSign!)
			Return
		END IF
		idwc_claim_status_type_code.InsertRow(1)	
		idwc_claim_status_type_code.SetItem(1,"claim_status_desc","Active & Finalled")
		idwc_claim_status_type_code.SetItem(1,"claim_status_code","")
		idwc_claim_status_type_code.SetFilter("claim_status_code = 'A' or claim_status_code = 'F'or claim_status_code = ''")    
		idwc_claim_status_type_code.Filter()

		
		/*		Load the claim manager drop down data window, then add a blank row
		*/
		IF dw_search_criteria.GetChild("user_id",idwc_claim_manager) < 0 THEN
			MessageBox("DataWindow Error","Unable to load claim manager list!",StopSign!)
			Return
		END IF
		idwc_claim_manager.InsertRow(1)
		idwc_claim_manager.SetItem(1,"user_id","")
		idwc_claim_manager.SetItem(1,"user_last_name","")
		idwc_claim_manager.SetItem(1,"user_first_name","")		
		
		/*		Load the Region drop down data window, then add a blank row
		*/
		IF dw_search_criteria.GetChild("region",ldwc_child) < 0 THEN
			MessageBox("DataWindow Error","Unable to load admin region list!",StopSign!)
			Return
		End If

		ldwc_child.InsertRow(1)
		ldwc_child.SetItem(1,"admin_region_code","")
		ldwc_child.SetItem(1,"admin_region_desc","")

		/* Now swap out the datawindow object for the dw_search_list
		*/
		
		dw_search_list.DataObject = 'd_rx_termination_coverage_list'
		dw_search_list.SetTransObject(SQLCA)
		
		/* set up the filter control object, and turn it on
		*/
		dw_search_list.uf_setfilter( true)
		uo_filter_control.uf_set_requestor(dw_search_list)
		uo_filter_control.visible= true
		dw_search_list.height = 1100
		cb_extract.visible=true
	
	CASE "d_individual_search"
		dw_search_criteria.object.datawindow.header.height = 106
		dw_search_list.DataObject = 'd_search_list_basic_individual'
		dw_search_list.SetTransObject(SQLCA)
		is_SQLSyntax_basic_individual = dw_search_list.getSQLSelect()
	
END CHOOSE

dw_search_list.Reset()


end subroutine

public subroutine uf_refresh_search ();INTEGER  li_find, li_current_row
LONG     ll_current_claim_no
STRING   ls_current_name_type_code, ls_find


// this function will refresh the search for d_search_list 'searches':
//   claim number search
//   basic claim search
//   case load search
//   stats not coded search
//   due/overdue (12 week only) search
//   individual number search
// it is currently called from Inbasket, and is intended to make the Inbasket Inbox envelope button invisible

li_current_row = dw_search_list.GetRow()

IF li_current_row > 0 THEN
		
	ll_current_claim_no       = dw_search_list.GetItemNumber(li_current_row,'claim_no')
	ls_current_name_type_code = dw_search_list.GetItemString(li_current_row,'name_type_code')
	
	cb_search.TriggerEvent(Clicked!)
	
	ls_find = 'claim_no = ' +String(ll_current_claim_no)+ ' AND name_type_code = "' +ls_current_name_type_code+ '"'
	
	li_find = dw_search_list.Find(ls_find, 1, dw_search_list.RowCount())
	IF li_find > 0 THEN
		dw_search_list.ScrollToRow(li_find)
		dw_search_list.SetRow(li_find)
	ELSE
		// ??
	END IF
	
ELSE
	// ??
END IF
end subroutine

event constructor;datawindowchild  ldwc_search

//	dw_search_type.SetTransObject(SQLCA)
	dw_search_type.InsertRow(0)
	
	dw_search_type.GetChild('search_type',ldwc_search)
	ldwc_search.SetTransObject(SQLCA)
	ldwc_search.Retrieve()
	
	dw_search_list.SetTransObject(SQLCA)
	dw_search_list_individual.SetTransObject(SQLCA)
	dw_search_list_annuity_individual.SetTransObject(SQLCA)

	dw_search_list.uf_SetSelect(1)

/*  Set up the basic SQL Syntax that will be modified for the requested search.    
*/
	is_SQLSyntax = dw_search_list.Describe("DataWindow.Table.Select")
	
	is_WhereClause = "WHERE CLAIM_PARTICIPANT.claim_role_code = 'C' and  &
							CLAIM_PARTICIPANT.claimant_active_flag = 'Y'  " 


/* Do the same for the individual list
*/
	is_SQLSyntaxI = dw_search_list_individual.Describe("DataWindow.Table.Select")
	is_SQLSyntaxI = left(is_SQLSyntaxI,pos(is_SQLSyntaxI,"WHERE",1)-1)
	is_whereclauseI = " WHERE   ( INDIVIDUAL.individual_no = INDIVIDUAL_NAME.individual_no ) and " + &
                  " ( INDIVIDUAL.individual_no = CLAIM_PARTICIPANT.individual_no ) and " + &
                  " ( CLAIM_PARTICIPANT.claim_role_code <> ~~~"C~~~" OR " + &
						" ( CLAIM_PARTICIPANT.claimant_active_flag = ~~~"N~~~" and CLAIM_PARTICIPANT.claim_role_code = ~~~"C~~~")) "


//get SQL for the annuity payout participant list
is_SQLSyntax_individual_annuity_list = dw_search_list_annuity_individual.getSQLSelect()

/*	Set up the SQL for the stats syntax
*/

is_stats_syntax =  &
"  SELECT CLAIM.claim_no ,                                                                                      " +& 
"         CLAIM.accident_date ,                                                                                 " +&
"         IsNull( ( SELECT a.claim_manager_catid  'claim_manager_catid'                                         " +&
"                   FROM   Routing_Assignments a                                                                " +&
"                   WHERE  a.claim_manager_user_id = CLAIM.claim_manager_user_id                                " +&
"                   AND    a.admin_region_code     = CLAIM.admin_region_code                                    " +&
"                   AND EXISTS ( SELECT *                                                                       " +&
"                                FROM   CLAIM_WORKING      b                                                    " +&
"                                JOIN   FLD                c ON b.folderid = c.fldid                            " +&
"                                WHERE  c.fldcatid = a.claim_manager_catid                                      " +&
"                                AND    b.claim_no = CLAIM.claim_no ) ), 0 ) 'inbasket_mail',                   " +&
"         INDIVIDUAL.sin_no ,                                                                                   " +&
"         INDIVIDUAL.medicare_no ,                                                                              " +&
"         INDIVIDUAL.birth_date ,                                                                               " +&
"         INDIVIDUAL.city ,                                                                                     " +&
"         INDIVIDUAL_NAME.last_name ,                                                                           " +&
"         INDIVIDUAL_NAME.given_names ,                                                                         " +&
"         INDIVIDUAL_NAME.name_type_code ,                                                                      " +&
"         CLAIM.claim_manager_user_id ,                                                                         " +&
"         CLAIM.admin_region_code ,                                                                             " +&
"         sb.side_of_body_desc_e +'/'+ pb.part_of_body_desc 'psob_desc',                                        " +&
"         ACCIDENT.nature_of_injury_code,                                                                       " +&
"         INDIVIDUAL.individual_no,                                                                             " +&
"         INDIVIDUAL.telephone_no,                                                                              " +&
"         EMPLOYER.employer_legal_name ,                                                                        " +&
"         CLAIM.claim_status_code ,                                                                             " +&
"         CLAIM.claim_status_type_code,                                                                         " +&
"         CLAIM.history_flag,                                                                                   " +&
"         INDIVIDUAL.individual_no  ,                                                                           " +&
"         CLAIM.administering_act_code ,                                                                        " +&
"         IsNull(fca_claim.rejected_wca_claim_no ,0) 'FCA_rejected_wca_claim_no'                                " +&
"  FROM   CLAIM                                                                                                 " +&
"  JOIN   Part_of_Body pb ON CLAIM.part_of_body_code = pb.part_of_body_code                                     " +&
"  JOIN   Side_of_Body sb ON CLAIM.side_of_body_code = sb.side_of_body_code                                     " +&
"  JOIN   CLAIM_PARTICIPANT  ON CLAIM.claim_no = CLAIM_PARTICIPANT.claim_no                                     " +&
"  JOIN   INDIVIDUAL  ON CLAIM_PARTICIPANT.individual_no = INDIVIDUAL.individual_no                             " +&
"  LEFT OUTER JOIN  EMPLOYER  ON CLAIM.accident_employer_no = EMPLOYER.employer_no                              " +&
"  JOIN   INDIVIDUAL_NAME  ON INDIVIDUAL.individual_no = INDIVIDUAL_NAME.individual_no                          " +&
"  JOIN   ACCIDENT  ON CLAIM.claim_no = ACCIDENT.claim_no                                                       " +&
"  LEFT OUTER JOIN  CLAIM fca_claim ON fca_claim.rejected_wca_claim_no = CLAIM.claim_no                         " +&
"                                  AND fca_claim.rejected_wca_claim_no <> 0                                     " +&
"  WHERE  CLAIM_PARTICIPANT.claim_role_code = 'C'                                                               " +&
"  AND    CLAIM_PARTICIPANT.claimant_active_flag = 'Y'                                                          " +&
"  AND    INDIVIDUAL_NAME.name_type_code = 'M'                                                                  " +&
"  AND    CLAIM.coding_complete_flag = 'N'                                                                      " +&
"  AND  ( CLAIM.claim_status_code = 'A'                                                                         " +&
"     OR ( CLAIM.claim_status_code = 'F'                                                                        " +&
"      AND CLAIM.claim_status_type_code in( '01', '02' , '03' )))                                               " +&
"  UNION ALL                                                                                                    " +&
"  SELECT CLAIM.claim_no ,                                                                                      " +& 
"         CLAIM.accident_date ,                                                                                 " +&
"         IsNull( ( SELECT a.claim_manager_catid  'claim_manager_catid'                                         " +&
"                   FROM   Routing_Assignments a                                                                " +&
"                   WHERE  a.claim_manager_user_id = CLAIM.claim_manager_user_id                                " +&
"                   AND    a.admin_region_code     = CLAIM.admin_region_code                                    " +&
"                   AND EXISTS ( SELECT *                                                                       " +&
"                                FROM   CLAIM_WORKING      b                                                    " +&
"                                JOIN   FLD                c ON b.folderid = c.fldid                            " +&
"                                WHERE  c.fldcatid = a.claim_manager_catid                                      " +&
"                                AND    b.claim_no = CLAIM.claim_no ) ), 0 ) 'inbasket_mail',                   " +&
"         INDIVIDUAL.sin_no ,                                                                                   " +&
"         INDIVIDUAL.medicare_no ,                                                                              " +&
"         INDIVIDUAL.birth_date ,                                                                               " +&
"         INDIVIDUAL.city ,                                                                                     " +&
"         INDIVIDUAL_NAME.last_name ,                                                                           " +&
"         INDIVIDUAL_NAME.given_names ,                                                                         " +&
"         INDIVIDUAL_NAME.name_type_code ,                                                                      " +&
"         CLAIM.claim_manager_user_id ,                                                                         " +&
"         CLAIM.admin_region_code                         ,                                                     " +&
"         sb.side_of_body_desc_e +'/'+ pb.part_of_body_desc 'psob_desc',                                        " +&
"         ACCIDENT.nature_of_injury_code,                                                                       " +&
"         INDIVIDUAL.individual_no,                                                                             " +&
"         INDIVIDUAL.telephone_no,                                                                              " +&
"         EMPLOYER.employer_legal_name ,                                                                        " +&
"         CLAIM.claim_status_code ,                                                                             " +&
"         CLAIM.claim_status_type_code,                                                                         " +&
"         CLAIM.history_flag,                                                                                   " +&
"         INDIVIDUAL.individual_no  ,                                                                           " +&
"         CLAIM.administering_act_code ,                                                                        " +&
"         IsNull(fca_claim.rejected_wca_claim_no ,0) 'FCA_rejected_wca_claim_no'                                " +&
"  FROM   DIFFICULT_CLAIM                                                                                       " +&
"  JOIN   CLAIM  ON CLAIM.claim_no = DIFFICULT_CLAIM.claim_no                                                   " +&
"  JOIN   Part_of_Body pb ON CLAIM.part_of_body_code = pb.part_of_body_code                                     " +&
"  JOIN   Side_of_Body sb ON CLAIM.side_of_body_code = sb.side_of_body_code                                     " +&
"  JOIN   CLAIM_PARTICIPANT  ON CLAIM.claim_no = CLAIM_PARTICIPANT.claim_no                                     " +&
"  JOIN   INDIVIDUAL  ON CLAIM_PARTICIPANT.individual_no = INDIVIDUAL.individual_no                             " +&
"  LEFT OUTER JOIN  EMPLOYER  ON CLAIM.accident_employer_no = EMPLOYER.employer_no                              " +&
"  JOIN   INDIVIDUAL_NAME  ON INDIVIDUAL.individual_no = INDIVIDUAL_NAME.individual_no                          " +&
"  JOIN   ACCIDENT  ON CLAIM.claim_no = ACCIDENT.claim_no                                                       " +&
"  LEFT OUTER JOIN  CLAIM fca_claim ON fca_claim.rejected_wca_claim_no = CLAIM.claim_no                         " +&
"                                  AND fca_claim.rejected_wca_claim_no <> 0                                     " +&
"  WHERE  CLAIM_PARTICIPANT.claim_role_code = 'C'                                                               " +&
"  and    CLAIM_PARTICIPANT.claimant_active_flag = 'Y'                                                          " +&
"  and    INDIVIDUAL_NAME.name_type_code = 'M'                                                                  " +&
"  and    CLAIM.claim_status_code = 'F'                                                                         " +&
"  and    DIFFICULT_CLAIM.fatality_flag = 'Y'                                                                   " +&
"  and    CLAIM.coding_complete_flag = 'N'                                                                      " 

/*	Set up the SQL for the rx termination search syntax
*/
	
	is_SQLSyntaxRx1 = "select CLAIM_ELIGIBILITY.claim_no, "+&
" 'eligibility_end_date' = max(isnull(convert(CHAR(10),CLAIM_ELIGIBILITY.eligibility_end_date,120), 'open-ended')),"+&      
" 'eligibility_type'='E',"+&       
" CLAIM.claim_manager_user_id,"+& 
" CLAIM.admin_region_code,"+&
" CLAIM.claim_status_code,"+&
" Claim_Status.claim_status_desc,"+&
" INDIVIDUAL.individual_no,"+& 
" 'individual_name' = (INDIVIDUAL.last_name + ', ' + INDIVIDUAL.given_names),"+&
" INDIVIDUAL.birth_date "+&    
" from CLAIM_ELIGIBILITY, INDIVIDUAL, CLAIM, Claim_Status"+&
" where CLAIM_ELIGIBILITY.claim_no = CLAIM.claim_no"+&
" and CLAIM.claim_status_code = Claim_Status.claim_status_code"+&
" and INDIVIDUAL.individual_no = CLAIM.individual_no"+&
" and  CLAIM.claim_status_code in ('A','F') "
							
	is_SQLSyntaxRx1a = " group by CLAIM_ELIGIBILITY.claim_no, "+&      
" CLAIM.claim_manager_user_id, "+&
" CLAIM.admin_region_code,"+&
" CLAIM.claim_status_code,"+&
" Claim_Status.claim_status_desc,"+&
" INDIVIDUAL.individual_no,"+& 
" INDIVIDUAL.last_name, "+&
" INDIVIDUAL.given_names,"+&
" INDIVIDUAL.birth_date "

	is_SQLSyntaxRx2 = "select CLAIM_FORMULARY.claim_no,"+&
" 'eligibility_end_date' = isnull(convert(CHAR(10),CLAIM_FORMULARY.formulary_end_date,120), 'open-ended'),"+&              
" formulary_type_code,"+&       
" CLAIM.claim_manager_user_id,"+&
" CLAIM.admin_region_code,"+&
" CLAIM.claim_status_code,"+&
" Claim_Status.claim_status_desc,"+&
" INDIVIDUAL.individual_no,"+& 
" 'individual_name' = (INDIVIDUAL.last_name + ', ' + INDIVIDUAL.given_names),"+&
" INDIVIDUAL.birth_date "+&     
" from CLAIM_FORMULARY, INDIVIDUAL, CLAIM, Claim_Status"+&
" where CLAIM_FORMULARY.claim_no = CLAIM.claim_no"+&
" and CLAIM.claim_status_code = Claim_Status.claim_status_code"+&
" and INDIVIDUAL.individual_no = CLAIM.individual_no"+&
" and  CLAIM.claim_status_code in ('A','F')" 


end event

on u_claim_search.create
this.cbx_inbasket=create cbx_inbasket
this.cb_annuity_only_list=create cb_annuity_only_list
this.cb_more=create cb_more
this.cb_clear=create cb_clear
this.cb_search=create cb_search
this.dw_search_type=create dw_search_type
this.cb_extract=create cb_extract
this.uo_filter_control=create uo_filter_control
this.dw_search_list=create dw_search_list
this.dw_search_list_individual=create dw_search_list_individual
this.dw_search_list_annuity_individual=create dw_search_list_annuity_individual
this.dw_search_criteria=create dw_search_criteria
this.gb_1=create gb_1
this.Control[]={this.cbx_inbasket,&
this.cb_annuity_only_list,&
this.cb_more,&
this.cb_clear,&
this.cb_search,&
this.dw_search_type,&
this.cb_extract,&
this.uo_filter_control,&
this.dw_search_list,&
this.dw_search_list_individual,&
this.dw_search_list_annuity_individual,&
this.dw_search_criteria,&
this.gb_1}
end on

on u_claim_search.destroy
destroy(this.cbx_inbasket)
destroy(this.cb_annuity_only_list)
destroy(this.cb_more)
destroy(this.cb_clear)
destroy(this.cb_search)
destroy(this.dw_search_type)
destroy(this.cb_extract)
destroy(this.uo_filter_control)
destroy(this.dw_search_list)
destroy(this.dw_search_list_individual)
destroy(this.dw_search_list_annuity_individual)
destroy(this.dw_search_criteria)
destroy(this.gb_1)
end on

type cbx_inbasket from checkbox within u_claim_search
boolean visible = false
integer x = 2574
integer y = 456
integer width = 457
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "New In-Basket"
boolean checked = true
boolean lefttext = true
end type

type cb_annuity_only_list from commandbutton within u_claim_search
boolean visible = false
integer x = 1371
integer y = 432
integer width = 965
integer height = 88
integer taborder = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Annuity Payout Participant Only List"
end type

event clicked;w_sheet lw_sheet

dw_search_list_annuity_individual.visible = TRUE
dw_search_list_annuity_individual.bringToTop = TRUE

IF iwi_window_parent.ClassName() = 'w_sheet' THEN lw_sheet = iwi_window_parent

if isvalid(lw_sheet) THEN
	lw_sheet.wf_clear_worksheet()
	lw_sheet.wf_initialize_basic_claim()			
END IF

	

end event

type cb_more from commandbutton within u_claim_search
boolean visible = false
integer x = 2363
integer y = 432
integer width = 777
integer height = 88
integer taborder = 50
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Other Claim Participant &List"
end type

event clicked;IF dw_search_list_individual.visible THEN
   dw_search_list_individual.visible = FALSE
   THIS.text = 'Other Claim Participant &List'
   dw_search_list.visible = TRUE
	dw_search_list.TriggerEvent(RowFocusChanged!)
   dw_search_list.SetFocus()
ELSE
   dw_search_list_individual.visible = TRUE
   THIS.text = 'Claimant Participant &List'
   dw_search_list.visible = FALSE
	dw_search_list_individual.TriggerEvent(RowFocusChanged!)
   dw_search_list_individual.SetFocus()
END IF

IF dw_search_list_annuity_individual.visible = TRUE  THEN
	dw_search_list_annuity_individual.visible = FALSE
	dw_search_list_annuity_individual.bringToTop = FALSE
END IF

end event

type cb_clear from commandbutton within u_claim_search
integer x = 2747
integer y = 256
integer width = 283
integer height = 108
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cl&ear"
end type

event clicked;/*	Clear the contents of the current datawindows. Calling uf_set_dw_object
	ensures that any defaults are set correctly.
*/
	
	
	iwi_window_parent.SetRedraw(False)
	uf_clear_search_criteria()
	dw_search_criteria.SetFocus()
	iwi_window_parent.wf_clear_identifier()
	
	window lw_sheet
	
	lw_sheet = parent.getparent() 
	
	if lw_sheet.classname() = "w_sheet" THEN
		lw_sheet.dynamic wf_initialize_basic_claim()		
	END IF
	iwi_window_parent.SetRedraw(True)

end event

type cb_search from commandbutton within u_claim_search
integer x = 2747
integer y = 136
integer width = 283
integer height = 108
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Search"
end type

event clicked;/*	This script controls validation of the search criteria, creating the SQL required for 
	the requested search and retrieving the data. 
*/

	INTEGER		li_ReturnCode
	STRING		ls_ModifyError, ls_msg

	dw_search_criteria.AcceptText()
	IF uf_validate_criteria() < 0 THEN
		Return
	END IF

/*	Build the SQL query based on the search criteria. 
*/
	IF uf_set_query() < 0 Then
		MessageBox("Search Criteria Errors!","Error setting search criteria. Contact Systems!",StopSign!)
		Return
	END IF

/* Retrieve the search list based on the SQL query just built. 
*/
	li_ReturnCode = uf_retrieve()
	If li_ReturnCode < 0 Then
		MessageBox("Search Errors!","Errors retrieving data matching your search criteria!",StopSign!)
		Return
	ElseIf li_ReturnCode = 0 Then
		CHOOSE CASE is_SearchType
			CASE 'd_basic_claim_search','d_individual_number_search'
				ls_msg = 'The person is not an active claimant in the claimant participant list!'
				
				IF cb_more.Visible = TRUE THEN
					ls_msg = ls_msg + ' Click on the "Other Claim Participant List" button for further information.' 
				ELSEIF cb_annuity_only_list.Visible = TRUE THEN
					ls_msg = ls_msg + ' Click on the "Annuity Payout Participant Only List" button for further information.' 
				END IF
				
				MessageBox('No Claims Found!',ls_msg,Exclamation!)
				
			CASE 'd_individual_search'
				MessageBox('No Individuals Found!','No individuals were located matching your search criteria!',Exclamation!)
				
			CASE 'd_case_load_search2','d_stats_not_coded','d_due_overdue_list','d_to_do_search','d_claim_rx_termination_search','d_claim_number_search'
				MessageBox("No Claims Found!",'No claims were located matching your search criteria!',Exclamation!)
				
		END CHOOSE
		Return
	End If

end event

type dw_search_type from u_dw_online within u_claim_search
integer x = 46
integer y = 68
integer width = 1102
integer height = 88
integer taborder = 10
string dataobject = "d_search_type"
boolean border = false
end type

event itemchanged;/*  Determine the search criteria datawindow to display.
*/
	dw_search_criteria.SetRedraw(False)

	is_SearchType = Trim(this.GetText())

	uf_set_dw_object(is_SearchType,'')
	
	
	IF is_SearchType = 'd_due_overdue_list' THEN
		dw_search_criteria.Object.rb_doc_type.Visible       = 0
		dw_search_criteria.Object.t_rb_doc_type.Visible     = 0
		
		dw_search_criteria.Object.opening_type_code.Visible   = 0
		dw_search_criteria.Object.t_opening_type_code.Visible = 0
					
		dw_search_criteria.Object.bdate_from_date.Visible   = 0
		dw_search_criteria.Object.t_bdate_from_date.Visible = 0
		
		dw_search_criteria.Object.bdate_to_date.Visible   = 0
		dw_search_criteria.Object.t_bdate_to_date.Visible = 0
	END IF
	
CHOOSE CASE is_SearchType
	CASE 'd_claim_number_search','d_basic_claim_search','d_case_load_search2','d_stats_not_coded','d_due_overdue_list','d_individual_number_search'
		cbx_inbasket.Visible = TRUE
	CASE ELSE
		cbx_inbasket.Visible = FALSE
END CHOOSE
	
	dw_search_type.BringToTop = True

	dw_search_criteria.SetRedraw(True)
	dw_search_criteria.SetFocus()

end event

type cb_extract from commandbutton within u_claim_search
integer x = 2363
integer y = 1684
integer width = 270
integer height = 84
integer taborder = 100
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Extract"
end type

event clicked;INTEGER    li_rtn
STRING     ls_c_drive, ls_filename, ls_return_filename
U_DS       lds_excel


IF dw_search_list.RowCount() = 0 THEN
	MessageBox('No rows','There are no rows to extract.')
	RETURN
END IF

IF is_SearchType = 'd_due_overdue_list' THEN
	ls_c_drive  = 'C:'
	
	ls_filename = 'annual_review_search_result'
	
	li_rtn =	GetFileSaveName('Save To', ls_filename, ls_return_filename,'xls','XLS File (*.xls), *.xls', ls_c_drive)
	
	IF li_rtn <> 1 THEN RETURN
	
	IF FileExists(ls_return_filename) THEN
		IF MessageBox('Overwrite File?','The file exists. Do you want to overwrite the file?',Question!,YesNo!,2) = 2 THEN
			RETURN
		END IF
	END IF
	
	lds_excel = Create U_DS
	lds_excel.DataObject = dw_search_list.DataObject
	lds_excel.SetTransObject(SQLCA)
	li_rtn = dw_search_list.ShareData(lds_excel)
	
	li_rtn = lds_excel.SaveAs(ls_return_filename, Excel8!,true)
	
	IF li_rtn <= 0  THEN
		MESSAGEBOX('Error Saving File', 'There was an error saving the file to the specified location.' &
		+'~r~nPlease check that the file is not already open and that you have accesss to the selected save-to location.', EXCLAMATION!)
	END IF
ELSE
	dw_search_list.Saveas('',Excel!,True)
END IF


end event

type uo_filter_control from u_filter_control within u_claim_search
event destroy ( )
integer x = 82
integer y = 1672
integer height = 100
integer taborder = 90
boolean bringtotop = true
end type

on uo_filter_control.destroy
call u_filter_control::destroy
end on

event destructor;call super::destructor;call u_filter_control::destroy
end event

event ue_filter_changed;call super::ue_filter_changed;
STRING ls_filter
// If the uo_filter_control's 'toggle filter' button is pushed for the rx termination screen, 
// and it is turning off the user-applied filter, then re-apply the original filter
// else, add the new filter to the original filter
if is_searchtype = 'd_claim_rx_termination_search' THEN

	IF Len(ls_new_filter) = 0 Then
		ls_filter = parent.is_filter
	ELSE
		ls_filter = ls_new_filter + " and " + 	parent.is_filter
	END IF
	dw_search_list.setfilter(ls_filter)
	dw_search_list.filter()

END IF
end event

type dw_search_list from u_dw_online within u_claim_search
integer x = 5
integer y = 552
integer width = 3168
integer height = 1208
integer taborder = 60
string dataobject = "d_search_list"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

on ue_print;call u_dw_online::ue_print;dw_search_criteria.Print()
end on

event rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup
WINDOW 						lwi_window

	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	lm_popup.m_options.m_sort.visible = TRUE
	/* P10236 2005-10-17, r.s.*/
	IF is_SearchType = 'd_claim_rx_termination_search' THEN
			lm_popup.m_options.m_moredetails.visible = TRUE
	END IF
	
	lwi_window = w_frame.GetActiveSheet()
	IF lwi_window <> iwi_window_parent THEN
		lm_popup.m_options.PopMenu(iwi_window_parent.PointerX( ), iwi_window_parent.PointerY( ))
	ELSE
		lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))
	END IF
	Destroy lm_popup
end event

event rowfocuschanged;call super::rowfocuschanged;/*	Highlight the new row, then get the claim number and retrieve it
*/
	LONG	ll_row_Number
	w_sheet lw_sheet

	ll_row_number = dw_search_list.GetRow()
	IF ll_row_number = 0 THEN
		Return
	End IF
	
	uf_processselect(ll_row_number,"Mouse")
	// added this IF statement code for P10273 P2-D3 new search functionality
	IF is_SearchType = "d_individual_search" THEN
		il_individual_no = dw_search_list.GetItemNumber(ll_row_number,"individual_no")
		
		IF iwi_window_parent.ClassName() = 'w_sheet' THEN lw_sheet = iwi_window_parent

		if isvalid(lw_sheet) THEN
			lw_sheet.wf_clear_worksheet()
			lw_sheet.wf_initialize_basic_claim()			
		END IF
	ELSE
		il_claim_no	= dw_search_list.GetItemNumber(ll_row_number,"claim_no")
		uf_set_claim(il_claim_no)
	END IF


	IF ClassName(iwi_window_parent) = 'w_indexing_search' THEN
		iwi_window_parent.Event Dynamic ue_rowfocuschanged(ll_row_number)
	END IF
	

end event

event ue_more_details;call super::ue_more_details;// this is called from the popup menu: 'More details...'
CHOOSE CASE is_searchtype
	CASE 'd_claim_rx_termination_search'		
		m_cmwb.m_tools.m_rxcoverage.triggerevent(CLICKED!)
END CHOOSE
end event

event buttonclicked;call super::buttonclicked;DATAWINDOW        ldw_inbasket_category_select
DataWindowChild   ldw_category_select_child
DWObject          l_dwo
INTEGER           li_find, li_child_rowcount, li_rtn, li_cat_count_for_user
LONG              ll_catid
STRING            ls_find, ls_catname, ls_claim_manager_user_id, ls_claim_manager_name
W_SHEET				lw_active_sheet
s_window_message  lstr_message

IF dwo.name = 'b_inbasket' THEN
	
	f_populate_app_log(gs_appname_and_handle , 400 , 'u_claim_search - dw_search_list - b_inbasket',  'ButtonClicked Event')
	
	lw_active_sheet = w_frame.GetActiveSheet()
	IF not IsValid(lw_active_sheet) THEN
		Return
	END If
	
	ll_catid = THIS.GetItemNumber(row,'inbasket_mail')
	
	// if the user has the right inbaskets set up, then the count will be > zero
	SELECT  COUNT(*)
	INTO    :li_cat_count_for_user
	FROM    CAT                a
	JOIN    User_Category_Xref b ON a.catid = b.category_id_nmbr
	WHERE   b.user_name_text = :vgst_user_profile.user_id
	AND     b.inbox_yn       = 'Y'
	AND     a.catid          = :ll_catid
	AND EXISTS ( SELECT *
	             FROM   CLAIM_WORKING c  
	             JOIN   FLD           d ON c.folderid = d.fldid
	             JOIN   CLAIM         e ON c.claim_no = e.claim_no
	             WHERE  d.fldcatid = a.catid )
	USING SQLCA;
	SQLCA.nf_handle_error('u_claim_search','embedded SQL: SELECT COUNT(*) FROM CAT, User_Category_Xref...','dw_search_list.ButtonClicked')
	
	IF li_cat_count_for_user > 0 THEN
		lstr_message.as_stringparm[1] = 'SEARCH'
		lstr_message.al_doubleparm[1] = il_claim_no
		lstr_message.apo_powerobjectparm[1] = lw_active_sheet
		
		IF cbx_inbasket.Checked THEN
			OpenWithParm (lw_active_sheet.iw_inBasket, lstr_message, lw_active_sheet)
			ldw_inbasket_category_select = lw_active_sheet.iw_inBasket.dw_inbasket_category_select
		ELSE
			OpenWithParm (lw_active_sheet.iw_inBasket_old, lstr_message, lw_active_sheet)
			ldw_inbasket_category_select = lw_active_sheet.iw_inBasket_old.dw_inbasket_category_select
		END IF
		
		ldw_inbasket_category_select.GetChild('catid',ldw_category_select_child)
		ldw_category_select_child.SetTransObject(ImageTrans)
		
		li_child_rowcount = ldw_category_select_child.Retrieve(vgst_user_profile.user_id)
		ImageTrans.nf_handle_error('ldw_category_select_child','w_inbasket','cb_inbasket_indicator.clicked')
		
		ls_find = 'cat_catid = ' + string(ll_catid)
		li_find = ldw_category_select_child.Find(ls_find,1,li_child_rowcount)
		
		if li_find > 0 THEN
			ls_catname = ldw_category_select_child.GetItemString(li_find,'cat_catname')
			
			li_rtn = ldw_category_select_child.ScrollToRow(li_find)
			li_rtn = ldw_category_select_child.SetRow(li_find)
			
			ldw_inbasket_category_select.ScrollToRow(li_find)
			ldw_inbasket_category_select.SetItem(li_find,'cat_catname',ls_catname)
			ldw_inbasket_category_select.SetText(ls_catname)
			
			l_dwo = ldw_inbasket_category_select.Object.cat_catname
			ldw_inbasket_category_select.EVENT ItemChanged(li_find,l_dwo,ls_catname)
			
			// item changed resets datawindowchild, so select row again
			li_rtn = ldw_category_select_child.ScrollToRow(li_find)
			li_rtn = ldw_category_select_child.SetRow(li_find)
			
			IF cbx_inbasket.Checked THEN
				lw_active_sheet.iw_inBasket.cbx_claim_view.Checked = TRUE
				lw_active_sheet.iw_inBasket.cbx_claim_view.postEvent(Clicked!)
			ELSE
				lw_active_sheet.iw_inBasket_old.cbx_claim_view.Checked = TRUE
				lw_active_sheet.iw_inBasket_old.cbx_claim_view.postEvent(Clicked!)				
			END IF				
		ELSE
			Messagebox('DataWindowChild error','The inbasket for the selected claim was not found.')
		END IF
	ELSE
		ls_claim_manager_user_id = dw_search_list.GetItemString(dw_search_list.GetRow(),'claim_manager')
		
		SELECT user_first_name + ' ' + user_last_name
		INTO   :ls_claim_manager_name
		FROM   User_Profile
		WHERE  user_id = :ls_claim_manager_user_id
		USING SQLCA;
		SQLCA.nf_handle_error('u_claim_search','embedded SQL: SELECT given_names,last_name FROM User_Profile...','dw_search_list.ButtonClicked')
		
		Messagebox('No Inbaskets','You do not have an inbasket set up for '+ls_claim_manager_name+'.')
	END IF
END IF

end event

type dw_search_list_individual from u_dw_online within u_claim_search
boolean visible = false
integer x = 5
integer y = 552
integer width = 3163
integer height = 1208
integer taborder = 70
string dataobject = "d_search_list_individual"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

on retrieveend;call u_dw_online::retrieveend;IF This.RowCount() > 0 THEN
	This.SelectRow(1, TRUE)
END IF
end on

on rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup


	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	lm_popup.m_options.m_sort.visible = TRUE
	lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))

	Destroy lm_popup
end on

on rowfocuschanged;call u_dw_online::rowfocuschanged;/*-------------------------------------------------------------*/
/*  Set the highlighting so that the current row is always     */
/*  highlighted.                                               */
/*-------------------------------------------------------------*/

Long	ll_row_Number

dw_search_list_individual.SelectRow (0, False)

ll_row_number = dw_search_list_individual.GetRow()
IF ll_row_number = 0 THEN
	Return
End IF
dw_search_list_individual.SelectRow (ll_row_number, True)

IF THIS.visible THEN
   il_claim_no	= dw_search_list_individual.GetItemNumber(ll_row_number,"claim_no")
   uf_set_claim(il_claim_no)
END IF

end on

on doubleclicked;call u_dw_online::doubleclicked;/*--------------------------------------------------------------*/
/*  Fire off the retrieve of the claim number when row is       */
/*  double clicked.                                             */
/*--------------------------------------------------------------*/

Long 	ll_rownumber

ll_rownumber = dw_search_list_individual.GetRow()
IF ll_rownumber = 0 THEN
	Return
End IF

il_claim_no	= dw_search_list_individual.GetItemNumber(ll_rownumber,"claim_no")

uf_set_claim(il_claim_no)

end on

type dw_search_list_annuity_individual from u_dw_online within u_claim_search
integer x = 5
integer y = 552
integer width = 3168
integer height = 1208
integer taborder = 70
boolean bringtotop = true
string dataobject = "d_search_list_annuity_individual"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event retrieveend;call super::retrieveend;IF This.RowCount() > 0 THEN
	This.SelectRow(1, TRUE)
END IF
end event

event rowfocuschanged;call super::rowfocuschanged;/*-------------------------------------------------------------*/
/*  Set the highlighting so that the current row is always     */
/*  highlighted.                                               */
/*-------------------------------------------------------------*/

Long	ll_row_Number

THIS.SelectRow (0, False)

ll_row_number = THIS.GetRow()
IF ll_row_number = 0 THEN
	Return
End IF
THIS.SelectRow (ll_row_number, True)


end event

event sqlpreview;call super::sqlpreview;STRING ls_SQL

ls_SQL = SQLSYNTAX

ls_SQL = ls_SQL
end event

type dw_search_criteria from u_dw_online within u_claim_search
integer x = 46
integer y = 56
integer width = 2697
integer height = 468
integer taborder = 20
string dataobject = "d_claim_number_search"
boolean border = false
end type

event itemchanged;INTEGER    li_visible
STRING     ls_filter, ls_claim_status_code, ls_old_claim_status_code, ls_claim_status_type_code

CHOOSE CASE is_SearchType

	CASE "d_case_load_search2"
      CHOOSE CASE THIS.GetColumnName()
         CASE 'claim_status_code'				
/*				Get the code and filter the type
*/
				ls_claim_status_code = Gettext()
            ls_filter = "claim_status_code = '" + ls_claim_status_code + "'"
            idwc_claim_status_type_code.SetFilter(ls_filter)
            idwc_claim_status_type_code.Filter()
				IF idwc_claim_status_type_code.RowCount() > 0 THEN
					ls_claim_status_type_code = idwc_claim_status_type_code.GetItemString(1,"claim_status_type_code")
					This.SetItem(1,"claim_status_type_code",ls_claim_status_type_code)
				ELSE
	            This.SetItem(1,'claim_status_type_code','')
				END IF
				
				ls_old_claim_status_code = THIS.GetItemString(1,'claim_status_code',Primary!,TRUE)
				
				IF ls_old_claim_status_code = "A" OR ls_old_claim_status_code = "N" THEN
					This.SetItem(1,"comp_day_code","")
					This.SetItem(1,"comp_week_code","")
					This.SetItem(1,"opening_type_code","")
				END IF
      END CHOOSE
	
   CASE 'd_claim_rx_termination_search'		
		IF dwo.name = "coverage_type" THEN
			IF data = "O" THEN  // thats an OH for Open-Ended, not a zero
				this.setItem(this.getrow(), 'days_to_termination', 0)
			ELSE
				this.setItem(this.getrow(), 'days_to_termination', 30)
			END IF
		END IF
		
	CASE 'd_due_overdue_list'
		IF dwo.name = 'required_action' THEN
			
			// some columns are only visible for AR (annual review), but not 12W (12 week review)
			IF data = '12W' THEN
				li_visible = 0
			ELSEIF data = 'AR' THEN
				li_visible = 1
			END IF
			
			dw_search_criteria.Object.rb_doc_type.Visible       = li_visible
			dw_search_criteria.Object.t_rb_doc_type.Visible     = li_visible
			
			dw_search_criteria.Object.opening_type_code.Visible   = li_visible
			dw_search_criteria.Object.t_opening_type_code.Visible = li_visible
						
			dw_search_criteria.Object.bdate_from_date.Visible   = li_visible
			dw_search_criteria.Object.t_bdate_from_date.Visible = li_visible
			
			dw_search_criteria.Object.bdate_to_date.Visible   = li_visible
			dw_search_criteria.Object.t_bdate_to_date.Visible = li_visible
			
			uf_set_dw_object(is_SearchType,data)
		END IF	
END CHOOSE

IF KeyDown(keyEnter!) THEN
   cb_search.PostEvent(Clicked!)
END IF
end event

event rbuttondown;STRING		ls_column_name, ls_describe, ls_style, ls_allowedit
BOOLEAN	lb_disable_menu_items

	If not isvalid(im_popup) Then
		im_popup = Create m_dw_online_rmb_popup
		im_popup.mf_set_datawindow(This)
		
		im_popup.m_options.m_0.visible = FALSE // divider
		im_popup.m_options.m_.visible = FALSE // divider
		im_popup.m_options.m_1.visible = TRUE // divider
		
		im_popup.m_options.m_cut.Visible = TRUE
		im_popup.m_options.m_copy.Visible = TRUE
		im_popup.m_options.m_delete.Visible = TRUE
		im_popup.m_options.m_paste.Visible = TRUE	
		im_popup.m_options.m_2.Visible = TRUE // divider
		im_popup.m_options.m_selectall.Visible = TRUE
	End if
	
	IF THIS.GetText() > '' THEN		
		IF Len(Trim(THIS.SelectedText())) = Len(Trim(THIS.GetText())) THEN
			im_popup.m_options.m_selectall.Enabled = FALSE
		ELSE
			im_popup.m_options.m_selectall.Enabled = TRUE
		END IF
	ELSE
		im_popup.m_options.m_selectall.Enabled = FALSE
	END IF

	IF THIS.SelectedText() > '' THEN
		im_popup.m_options.m_cut.Enabled = TRUE
		im_popup.m_options.m_copy.Enabled = TRUE
		im_popup.m_options.m_delete.Enabled = TRUE
	ELSE
		im_popup.m_options.m_cut.Enabled = FALSE
		im_popup.m_options.m_copy.Enabled = FALSE
		im_popup.m_options.m_delete.Enabled = FALSE
	END IF
	
	IF Clipboard() > '' THEN
		im_popup.m_options.m_paste.Enabled = TRUE	
	ELSE
		im_popup.m_options.m_paste.Enabled = FALSE
	END IF
	
	// some datawindow column styles do not allow for basic edit functionality
	ls_column_name = THIS.GetColumnName()
	ls_describe = ls_column_name + '.Edit.Style'
	ls_style = THIS.Describe(ls_describe)
	
	CHOOSE CASE ls_style
		CASE 'radiobuttons', 'checkbox'
			lb_disable_menu_items = TRUE
			
		CASE 'dddw'
			ls_describe = ls_column_name + '.DDDW.AllowEdit'
			ls_allowedit = THIS.Describe(ls_describe)
			IF ls_allowedit = 'no' THEN
				lb_disable_menu_items = TRUE
			END IF
			
		CASE 'ddlb'
			ls_describe = ls_column_name + '.DDLB.AllowEdit'
			ls_allowedit = THIS.Describe(ls_describe)
			IF ls_allowedit = 'no' THEN
				lb_disable_menu_items = TRUE
			END IF
			
		CASE 'edit','editmask'
			// Do nothing
			
	END CHOOSE
	
	IF lb_disable_menu_items THEN
		im_popup.m_options.m_cut.Enabled = FALSE
		im_popup.m_options.m_copy.Enabled = FALSE
		im_popup.m_options.m_paste.Enabled = FALSE
		im_popup.m_options.m_delete.Enabled = FALSE
		im_popup.m_options.m_selectall.Enabled = FALSE
	END IF

	im_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))

end event

event editchanged;call super::editchanged;
IF dwo.name = 'last_name' OR dwo.name ='first_name' THEN
	IF len(data) = 1  OR (  len(data) > 1 AND MATCH( MID(data, len(data) - 1, 1) , "[ '-]" ) ) THEN   //looks for a space or a hyphen or a apostrophy
		// this.settext(WordCap(data))  WordCap function works for the space but not the hyphen or apostrophy so manually upper case the last character
		this.settext (  left (data,len(data)-1) + upper(right(data,1)  ))	
		this.SelectText(this.selectedstart() + len(data) + 1, 0)	
	END IF
END IF

// this removes a leading edge space and any space that follows a space (so there is no double spaces)
IF dwo.name = 'last_name' OR dwo.name ='first_name' THEN
	IF MATCH( MID(data, len(data) - 1, 1) , "[ ]" )   AND right(data,1) = ' ' THEN
		this.settext (  left (data,len(data)-1) )	
		this.SelectText(this.selectedstart() + len(data) + 1, 0)	
	END IF
END IF
end event

type gb_1 from groupbox within u_claim_search
integer x = 5
integer width = 3159
integer height = 552
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Search Criteria"
end type

