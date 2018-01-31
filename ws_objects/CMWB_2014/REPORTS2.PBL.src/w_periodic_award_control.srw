$PBExportHeader$w_periodic_award_control.srw
$PBExportComments$Window to contain Report on Periodic Award Control table.
forward
global type w_periodic_award_control from w_a_report
end type
type gb_1 from groupbox within w_periodic_award_control
end type
type dw_search_criteria from u_dw_online within w_periodic_award_control
end type
type cb_search from commandbutton within w_periodic_award_control
end type
type cb_clear from commandbutton within w_periodic_award_control
end type
end forward

shared variables

end variables

global type w_periodic_award_control from w_a_report
int Width=2757
boolean TitleBar=true
string Title="Periodic Award Control Report"
gb_1 gb_1
dw_search_criteria dw_search_criteria
cb_search cb_search
cb_clear cb_clear
end type
global w_periodic_award_control w_periodic_award_control

type variables
// General variables for User Object

String		  is_BasicClaimSyntax
String                      is_stats_syntax
String		  is_SQLSyntax		// Contains the SQL used to modify the select statement on d_claim_search
String		  is_WhereClause		// Contains standard where clause appended to end of SQL statement

// Drop Down Data Windows

DataWindowChild	  idwc_award_type_code

BOOLEAN ib_force_plan
end variables

forward prototypes
public function long wf_retrieve ()
public function integer wf_set_query ()
public function integer wf_validate_criteria ()
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

public function integer wf_set_query ();/*	This function modifies the 'WHERE' clause based on the current search type and
	modIFies dw_search_list. It returns 1 IF successfull and -1 IF there were errors
*/
	STRING	ls_FieldValue, ls_SQLSyntax,	ls_WhereClause
	DATE		ld_sched_from, ld_sched_to, ld_period_from, ld_period_to, ld_processed_from, ld_processed_to
	DATE		ld_FieldValue
	BOOLEAN	lb_where_only
	INTEGER	li_return_code
	
/*	Aug 28/96 the ib_force_plan  is used to force a query to use a better search plan
	one of the queries was causing a table scan and setting the set force plan on results in a 
	better performance
*/
	ib_force_plan = FALSE

/*		this is used to better the performance of the query by SW region
*/
		ib_force_plan = TRUE

		ls_WhereClause = " where "
		lb_where_only = TRUE

/*		If an award type is chosen...
*/
		ls_FieldValue = dw_search_criteria.Object.award_type[1]
		IF Len(Trim(ls_FieldValue)) > 0 THEN
			ls_WhereClause = ls_WhereClause + " (award_type_code = '" + ls_FieldValue + "')"
			lb_where_only = FALSE
		END IF

		ls_FieldValue = dw_search_criteria.Object.processed[1]
		IF Trim(ls_FieldValue) = 'Y' THEN
			
			ld_FieldValue = dw_search_criteria.Object.processed_from[1]
			
			IF String(ld_FieldValue) = "0000-00-00" or IsNull(ld_FieldValue) THEN
				ld_FieldValue = Date("1990-01-01")
			END IF
			
			IF lb_where_only THEN
				ls_WhereClause = ls_WhereClause + " (processed_date between '" + String(ld_FieldValue) + "'"
			ELSE
				ls_WhereClause = ls_WhereClause + " and (processed_date between '" + String(ld_FieldValue) + "'"
			END IF
			
			ld_FieldValue = dw_search_criteria.Object.processed_to[1]
			
			IF String(ld_FieldValue) = "0000-00-00" or IsNull(ld_FieldValue) THEN
				ld_FieldValue = Date("2079-01-01")
			END IF
			
			ls_WhereClause = ls_WhereClause + " and '" + String(ld_FieldValue) + "')"
			
			lb_where_only = FALSE
		ELSE  // not processed .. add nothing to where clause
			IF lb_where_only THEN
				ls_WhereClause = ls_WhereClause + " (processed_date is null) "
			ELSE
				ls_WhereClause = ls_WhereClause + " and (processed_date is null) "
			END IF
			lb_where_only = FALSE
		END IF
		
		ld_FieldValue = dw_search_criteria.Object.scheduled_processing_from[1]
		IF String(ld_FieldValue) = "0000-00-00" or IsNull(ld_FieldValue) THEN
			ld_FieldValue = Date("1990-01-01")
		END IF
		
		IF lb_where_only THEN
			ls_WhereClause = ls_WhereClause + " (scheduled_processing_date between '" + String(ld_FieldValue) + "'"
		ELSE
			ls_WhereClause = ls_WhereClause + " and (scheduled_processing_date between '" + String(ld_FieldValue) + "'"
		END IF
		
		lb_where_only = FALSE
		
		ld_FieldValue = dw_search_criteria.Object.scheduled_processing_to[1]
		IF String(ld_FieldValue) = "0000-00-00" or IsNull(ld_FieldValue) THEN
			ld_FieldValue = Date("2079-01-01")
		END IF

		ls_WhereClause = ls_WhereClause + " and '" + String(ld_FieldValue) + "')"
		
		ld_FieldValue = dw_search_criteria.Object.period_from[1]
		IF String(ld_FieldValue) = "0000-00-00" or IsNull(ld_FieldValue) THEN
			ld_FieldValue = Date("1990-01-01")
		END IF
		
		IF lb_where_only THEN
			ls_WhereClause = ls_WhereClause + " (period_from_date between '" + String(ld_FieldValue) + "'"
		ELSE
			ls_WhereClause = ls_WhereClause + " and (period_from_date between '" + String(ld_FieldValue) + "'"
		END IF
		
		ld_FieldValue = dw_search_criteria.Object.period_to[1]
		IF String(ld_FieldValue) = "0000-00-00" or IsNull(ld_FieldValue) THEN
			ld_FieldValue = Date("2079-01-01")
		END IF

		ls_WhereClause = ls_WhereClause + " and '" + String(ld_FieldValue) + "')"
		
		IF lb_where_only THEN
			ls_WhereClause = ""
		END IF
		
/*		Modify the data window's syntax.....   
*/
	
		li_return_code = dw_report.SetSQLSelect(is_sqlsyntax + ls_WhereClause)
		
		IF li_return_code < 0 Then
			Return -1
		Else
			Return 1
		END IF
end function

public function integer wf_validate_criteria ();STRING ls_message
BOOLEAN lb_error

INTEGER li_row

li_row = dw_search_criteria.GetRow()

/*	
This function validates the search criteria.
It returns '1' if the search criteria is OK and -1 if there
are errors.   
*/

IF dw_search_criteria.Object.scheduled_processing_from[li_row] > Date("2079-01-01") THEN 
	lb_error = TRUE
	RETURN -1
END IF
IF dw_search_criteria.Object.scheduled_processing_to[li_row] > Date("2079-01-01") AND NOT lb_error THEN 
	lb_error = TRUE
	RETURN -1
END IF
IF dw_search_criteria.Object.period_from[li_row] > Date("2079-01-01") AND NOT lb_error THEN 
	lb_error = TRUE
	RETURN -1
END IF
IF dw_search_criteria.Object.period_to[li_row] > Date("2079-01-01") AND NOT lb_error THEN
	lb_error = TRUE
	RETURN -1
END IF

IF lb_error THEN
	ls_message = "The 'from' date cannot be greater than the 'to' date."
END IF

IF dw_search_criteria.Object.scheduled_processing_from[li_row] > dw_search_criteria.Object.scheduled_processing_to[li_row] AND &
		dw_search_criteria.Object.scheduled_processing_to[li_row] <> Date('0000-00-00') AND NOT lb_error THEN
	lb_error = TRUE
	RETURN -1
END IF
IF dw_search_criteria.Object.period_from[li_row] > dw_search_criteria.Object.period_to[li_row] AND &
		dw_search_criteria.Object.period_to[li_row] <> Date('0000-00-00') AND NOT lb_error THEN
	lb_error = TRUE
	RETURN -1
END IF

IF ls_message = '' AND lb_error THEN
	MessageBox("Date Error", "The 'from' date cannot be greater than the 'to' date.")
ELSEIF lb_error THEN
	MessageBox("Date Error", ls_message)
END IF

Return 1
end function

event open;call super::open;	INTEGER				li_row
	DATAWINDOWCHILD	ldwc_child

	dw_search_criteria.SetTransObject(SQLCA)
	dw_report.SetTransObject(SQLCA)

	li_row = dw_search_criteria.InsertRow(0)


/*  Set up the basic SQL Syntax that will be modified for the requested search.    
*/
	is_SQLSyntax = dw_report.GetSQLSelect()

/*	Load the claim status type drop down data window.
*/
	IF dw_search_criteria.GetChild("award_type",idwc_award_type_code) < 0 THEN
		MessageBox("DataWindow Error","Unable to retrieve claim status type codes!",StopSign!)
		Return
	END IF
	idwc_award_type_code.SetTransObject(SQLCA)
	idwc_award_type_code.Retrieve()
	
	dw_search_criteria.SetFocus()
end event

on w_periodic_award_control.create
int iCurrent
call super::create
this.gb_1=create gb_1
this.dw_search_criteria=create dw_search_criteria
this.cb_search=create cb_search
this.cb_clear=create cb_clear
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_1
this.Control[iCurrent+2]=this.dw_search_criteria
this.Control[iCurrent+3]=this.cb_search
this.Control[iCurrent+4]=this.cb_clear
end on

on w_periodic_award_control.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.gb_1)
destroy(this.dw_search_criteria)
destroy(this.cb_search)
destroy(this.cb_clear)
end on

type dw_report from w_a_report`dw_report within w_periodic_award_control
int Y=480
int Height=2012
int TabOrder=40
string DataObject="d_periodic_award_control"
boolean HScrollBar=true
end type

type gb_1 from groupbox within w_periodic_award_control
int X=46
int Y=20
int Width=2231
int Height=416
int TabOrder=10
string Text="Periodic Award Control Parameters"
BorderStyle BorderStyle=StyleLowered!
long TextColor=33554432
long BackColor=67108864
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontCharSet FontCharSet=Ansi!
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

type dw_search_criteria from u_dw_online within w_periodic_award_control
int X=78
int Y=100
int Width=2158
int Height=316
int TabOrder=30
boolean BringToTop=true
string DataObject="d_periodic_award_control_search"
boolean Border=false
boolean LiveScroll=true
end type

event itemchanged;dw_report.Reset()

IF dwo.name = "processed_from" OR dwo.name = "processed_to" THEN
	dw_search_criteria.Object.processed[1] = 'Y'
END IF
end event

type cb_search from commandbutton within w_periodic_award_control
int X=2341
int Y=88
int Width=283
int Height=108
int TabOrder=20
boolean BringToTop=true
string Text="&OK"
boolean Default=true
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
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
		MessageBox("No Data Found!","No periodic awards were located matching your search criteria!",Exclamation!)
		Return
	End If
end event

type cb_clear from commandbutton within w_periodic_award_control
int X=2341
int Y=220
int Width=283
int Height=108
int TabOrder=30
boolean BringToTop=true
string Text="&Clear"
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontCharSet FontCharSet=Ansi!
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;dw_search_criteria.Reset()
dw_search_criteria.InsertRow(0)
dw_report.reset()
end event

