$PBExportHeader$w_cost_of_claims_prod.srw
forward
global type w_cost_of_claims_prod from w_a_report
end type
type cb_ok from commandbutton within w_cost_of_claims_prod
end type
type dw_coc_statements_prod from u_dw_online within w_cost_of_claims_prod
end type
type dw_message from u_dw_online within w_cost_of_claims_prod
end type
type dw_limited from u_dw_online within w_cost_of_claims_prod
end type
end forward

global type w_cost_of_claims_prod from w_a_report
string title = "Cost Of Claims"
cb_ok cb_ok
dw_coc_statements_prod dw_coc_statements_prod
dw_message dw_message
dw_limited dw_limited
end type
global w_cost_of_claims_prod w_cost_of_claims_prod

type variables
STRING is_window_status
//passed from the calling menu in order to determine what the user can do

end variables

event open;call super::open;is_window_status = message.stringparm

CHOOSE CASE is_window_status
	CASE "REPRINT"
		
		dw_coc_statements_prod.SetTransObject(SQLCA)
		dw_coc_statements_prod.InsertRow(0)
		dw_coc_statements_prod.SetColumn("employer_no_from")
		dw_coc_statements_prod.SetFocus()
		dw_limited.visible                = FALSE
		dw_coc_statements_prod.visible    = TRUE
		
	CASE "LIMITED"
		dw_limited.SetTransObject(SQLCA)
		dw_limited.InsertRow(0)
		dw_limited.SetColumn("employer_no_from")
		dw_limited.SetFocus()
		dw_limited.visible                = TRUE
		dw_coc_statements_prod.visible    = FALSE
		
END CHOOSE

dw_report.SetTransObject(SQLCA)




end event

on w_cost_of_claims_prod.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.dw_coc_statements_prod=create dw_coc_statements_prod
this.dw_message=create dw_message
this.dw_limited=create dw_limited
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.dw_coc_statements_prod
this.Control[iCurrent+3]=this.dw_message
this.Control[iCurrent+4]=this.dw_limited
end on

on w_cost_of_claims_prod.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.dw_coc_statements_prod)
destroy(this.dw_message)
destroy(this.dw_limited)
end on

type dw_report from w_a_report`dw_report within w_cost_of_claims_prod
integer x = 0
integer y = 320
integer width = 2706
integer height = 2240
string dataobject = "d_print_coc_statements"
boolean hscrollbar = true
boolean resizable = true
end type

on dw_report::ue_filter;call w_a_report`dw_report::ue_filter;STRING	ls_filter

/* Open the filter window.
*/ 
	Open(w_filter_disposition_report)

/*	Apply the filter that was selected
*/	
	ls_filter = Message.StringParm
	IF ls_filter = "Cancel" THEN
		Return
	END IF

	dw_report.SetFilter(ls_filter)
	dw_report.Filter()

/*	If the entire report is to be viewed, then re-apply the sort as any filters that were applied
	may have messed the order of the report.
*/
	IF ls_filter = "" THEN
		dw_report.SetSort("claim_admin_region_code A, claim_claim_manager_user_id A, claim_disposition_type_claim_disposition_desc A, opening_claim_no A")
		dw_report.Sort()
	END IF

/* Regroup the dw to ensure that everyting is the way that we want it....
*/
	dw_report.GroupCalc()


end on

type cb_ok from commandbutton within w_cost_of_claims_prod
integer x = 1829
integer y = 128
integer width = 293
integer height = 96
integer taborder = 10
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
boolean default = true
end type

event clicked;LONG 		ll_numrows, ll_employer_no_from, ll_employer_no_to, ll_coc_period, ll_emp, ll_emp_no, ll_oper_no
LONG 		ll_file_only_employer_no
INTEGER	li_number_of_emp, li_rowcount,li_counter, li_coc_stmt_rows, li_msg_rtn
STRING   ls_employer_no, ls_operation, ls_reason ,ls_message,ls_string, ls_msg
U_DS     lds_coc_stmt_file_only



// determine which menu this window was called from 
CHOOSE CASE is_window_status
	CASE "LIMITED"
		
		dw_limited.AcceptText()
		//Validate that all the required information has been entered.
		ll_employer_no_from = dw_limited.GetItemNumber(1,"employer_no_from")
		ll_employer_no_to   = ll_employer_no_from
		ll_coc_period       = dw_limited.GetItemNumber(1,"coc_period")
		
	CASE "REPRINT"
		
		dw_coc_statements_prod.AcceptText()
		//	Validate that all the required information has been entered.
		ll_employer_no_from = dw_coc_statements_prod.GetItemNumber(1,"employer_no_from")
		ll_employer_no_to   = dw_coc_statements_prod.GetItemNumber(1,"employer_no_to")
		ll_coc_period       = dw_coc_statements_prod.GetItemNumber(1,"coc_period")
		
		IF IsNull(ll_employer_no_to) THEN
			If Messagebox("Making sure this is what You've intended","Do You want to retrieve statements for ** ALL **~nEmployers starting from " + &
						String(ll_employer_no_from) + " ?",Question!,YesNo!) = 1  Then
				ll_employer_no_to = 999999
			Else
			ll_employer_no_to = ll_employer_no_from
			End If
		END IF
		dw_coc_statements_prod.SetItem(1,"employer_no_to",ll_employer_no_to)
		dw_coc_statements_prod.SetFocus()
	
END CHOOSE

IF IsNull(ll_employer_no_from) OR IsNull(ll_coc_period) THEN
	MessageBox("Validation Error","Both the 'From Employer Number' and 'COC Period' must have a value.",Exclamation!)
	Return
END IF

//make sure the employer from is less than or equal to the employer to
IF NOT ISNULL(ll_employer_no_to) THEN
	IF ll_employer_no_from > ll_employer_no_to THEN
		MessageBox("Validation Error","'From Employer Number' must be less than To Employer Number.",Exclamation!)
		Return
	END IF 
END IF 

/* if the employer_no to = employer number from run the following code */
IF ll_employer_no_from = ll_employer_no_to THEN
	dw_message.settransobject(sqlca)
	li_rowcount = dw_message.retrieve(ll_coc_period,ll_employer_no_from)
	SQLCA.nf_handle_error("w_cost_of_claims_prod","cb_ok_clicked","dw_message.retrieve") 
	
	IF li_rowcount > 0 THEN
		ls_message = "The following associated employer(s) statement(s) were not printed " +&
		             "for the reasons indicated below~r~n"
		//create the message that will be displayed to the user
		FOR li_counter = 1 to dw_message.rowcount()
			ls_employer_no  = string(dw_message.getitemnumber(li_counter,"coc_employer_no"))
			ls_operation    = string(dw_message.getitemnumber(li_counter,"coc_operation_no"))
			ls_reason       = dw_message.getitemstring(li_counter,"reason_desc")
			ls_string       = "Employer #: " + ls_employer_no + " Operation #: " + ls_operation + " Reason: " + ls_reason + "~r~n"
			ls_message      = ls_message + ls_string
		NEXT
		
		messagebox("Associated Statements Not Printed",ls_message)
	END IF 
END IF

lds_coc_stmt_file_only = create u_ds
lds_coc_stmt_file_only.DataObject = 'd_coc_stmt_file_only'
lds_coc_stmt_file_only.SetTransObject(SQLCA)

li_coc_stmt_rows = lds_coc_stmt_file_only.Retrieve(ll_employer_no_from,ll_employer_no_to,ll_coc_period)
SQLCA.nf_handle_error('w_cost_of_claims_prod','lds_coc_stmt_file_only','cb_ok.clicked')

IF li_coc_stmt_rows > 0 THEN
	// warn users if there are 'file only' employers	
	IF is_window_status = 'LIMITED' THEN
		ls_msg = 'The selected employer has Cost of Claims delivery method of "File Only".'
	ELSEIF is_window_status = 'REPRINT' THEN
		ll_file_only_employer_no  = lds_coc_stmt_file_only.GetItemNumber(1,'employer_no')
		ls_msg = 'At least one employer ('+String(ll_file_only_employer_no)+') for the selected criteria has Cost of Claims delivery method of "File Only".'
	END IF
	
	li_msg_rtn = MessageBox('COC Reprint Warning',ls_msg +'~r~n~r~nDo you want to continue?',Exclamation!,YesNo!,2)
	IF li_msg_rtn = 2 THEN		
		RETURN
	END IF
	
END IF

CHOOSE CASE is_window_status
	CASE "LIMITED"
		ll_numrows = dw_report.Retrieve(ll_employer_no_from,ll_employer_no_to,ll_coc_period,'R')
      SQLCA.nf_handle_error("w_cost_of_claims_special - LIMITED","dw_report","cb_ok")

	CASE "REPRINT"
		ll_numrows = dw_report.Retrieve(ll_employer_no_from,ll_employer_no_to,ll_coc_period,'R')
		SQLCA.nf_handle_error("w_cost_of_claims_special - REPRINT","dw_report","cb_ok")
	
END CHOOSE
		

IF ll_numrows = 0 THEN
	MessageBox("Cost Of Claims Report","No data found to satisfy request")
	Return
END IF

// Calculate estimate Volume of printing
// It would be to the user to decide whether he/she wants to print

li_number_of_emp = 0
ll_emp_no = 0
ll_oper_no = 0
For ll_emp = 1 to ll_numrows
	If (ll_emp_no <> dw_report.GetItemNumber(ll_emp,"employer_no")  &
	 or ll_oper_no <> dw_report.GetItemNumber(ll_emp,"operation_no")) THEN
		li_number_of_emp = li_number_of_emp + 1
		ll_emp_no = dw_report.GetItemNumber(ll_emp,"employer_no")
		ll_oper_no = dw_report.GetItemNumber(ll_emp,"operation_no")
	End if
Next

Messagebox("Estimated Volume Retrieved","Number of payments Retrieved: " + String(ll_numrows) + & 
			  "~nNumber of Employer/Operations: " + String(li_number_of_emp) + &
			  "~nLast Employer retrieved: " + string(ll_emp_no))
end event

type dw_coc_statements_prod from u_dw_online within w_cost_of_claims_prod
integer x = 37
integer y = 32
integer width = 1792
integer height = 288
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_coc_criteria_prod"
boolean border = false
end type

type dw_message from u_dw_online within w_cost_of_claims_prod
boolean visible = false
integer x = 2194
integer y = 64
integer width = 439
integer height = 160
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "d_coc_not_printed_employers"
boolean hscrollbar = true
boolean resizable = true
end type

type dw_limited from u_dw_online within w_cost_of_claims_prod
integer x = 73
integer y = 64
integer width = 1646
integer height = 192
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "d_coc_criteria_prod_limited"
boolean border = false
end type

