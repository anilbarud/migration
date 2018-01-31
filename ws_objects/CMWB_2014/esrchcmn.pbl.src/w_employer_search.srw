$PBExportHeader$w_employer_search.srw
$PBExportComments$search for employer (cost alloc no) and operation no
forward
global type w_employer_search from window
end type
type cb_cancel from commandbutton within w_employer_search
end type
type cb_ok from commandbutton within w_employer_search
end type
type cb_back from commandbutton within w_employer_search
end type
type cb_empl_detail from commandbutton within w_employer_search
end type
type tab_coverage from tab within w_employer_search
end type
type tabpage_emp_op_coverage from userobject within tab_coverage
end type
type tabpage_emp_op_coverage from userobject within tab_coverage
end type
type tabpage_seasonal_coverage from userobject within tab_coverage
end type
type tabpage_seasonal_coverage from userobject within tab_coverage
end type
type tabpage_personal_coverage from userobject within tab_coverage
end type
type tabpage_personal_coverage from userobject within tab_coverage
end type
type tabpage_volunteer_coverage from userobject within tab_coverage
end type
type tabpage_volunteer_coverage from userobject within tab_coverage
end type
type tab_coverage from tab within w_employer_search
tabpage_emp_op_coverage tabpage_emp_op_coverage
tabpage_seasonal_coverage tabpage_seasonal_coverage
tabpage_personal_coverage tabpage_personal_coverage
tabpage_volunteer_coverage tabpage_volunteer_coverage
end type
type dw_employer_operation_payroll from u_dw_online within w_employer_search
end type
type cb_search from commandbutton within w_employer_search
end type
type cb_clear from commandbutton within w_employer_search
end type
type cb_view_purchasing_employer from commandbutton within w_employer_search
end type
type dw_emp_list from u_dw_online within w_employer_search
end type
type dw_search_criteria from u_dw_online within w_employer_search
end type
type dw_employer_data from u_dw_online within w_employer_search
end type
type sb_emp_search from u_splitbar_horizontal within w_employer_search
end type
type dw_coverage from u_dw_online within w_employer_search
end type
type dw_employer_operation_list from u_dw_online within w_employer_search
end type
end forward

global type w_employer_search from window
integer x = 1051
integer y = 456
integer width = 2766
integer height = 1824
boolean titlebar = true
string title = "Employer Search"
windowtype windowtype = response!
long backcolor = 67108864
cb_cancel cb_cancel
cb_ok cb_ok
cb_back cb_back
cb_empl_detail cb_empl_detail
tab_coverage tab_coverage
dw_employer_operation_payroll dw_employer_operation_payroll
cb_search cb_search
cb_clear cb_clear
cb_view_purchasing_employer cb_view_purchasing_employer
dw_emp_list dw_emp_list
dw_search_criteria dw_search_criteria
dw_employer_data dw_employer_data
sb_emp_search sb_emp_search
dw_coverage dw_coverage
dw_employer_operation_list dw_employer_operation_list
end type
global w_employer_search w_employer_search

type variables
STRING           is_type, is_select, is_charge_to_ind_message_displayed_flag
LONG             il_employer_no, il_accident_employer_no, il_accident_operation_no
INTEGER          ii_operation_no
S_WINDOW_MESSAGE istr_message
WINDOW           iw_win
end variables

forward prototypes
public function window wf_get_window_reference ()
end prototypes

public function window wf_get_window_reference ();RETURN THIS
end function

event open;istr_message = Message.PowerObjectParm

// get the type of search cost alloc or accident
is_type = istr_message.as_stringparm[1]
il_accident_employer_no  = istr_message.al_doubleparm[1]
il_accident_operation_no = istr_message.al_doubleparm[2]

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


/* if the type is X then the search was started from the frame and
   thus we need to disable some of the command buttons
*/
IF is_type = "X" THEN
	is_type           = "S"
	cb_cancel.visible = FALSE
	cb_ok.text        = "Close"
END IF

dw_emp_list.SetTransObject(SQLCA)
dw_search_criteria.SetTransObject(SQLCA)
dw_employer_operation_list.SetTransObject(SQLCA)
dw_employer_data.SetTransObject(SQLCA)
dw_employer_operation_payroll.SetTransObject(SQLCA)

dw_search_criteria.Reset()
dw_search_criteria.InsertRow(0)
dw_search_criteria.SetItem(1, "employer_name_type_code", "L")
dw_search_criteria.SetItem(1, "search_type_code","S")
dw_search_criteria.SetItem(1, "employer_name","")
dw_search_criteria.SetItem(1, "employer_no",0)
dw_search_criteria.SetFocus()

is_select = dw_emp_list.Describe("DataWindow.Table.Select")



sb_emp_search.of_register(dw_emp_list,0,100)
sb_emp_search.of_register(dw_employer_operation_list,100,-100)

iw_win = wf_get_window_reference()

end event

on w_employer_search.create
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.cb_back=create cb_back
this.cb_empl_detail=create cb_empl_detail
this.tab_coverage=create tab_coverage
this.dw_employer_operation_payroll=create dw_employer_operation_payroll
this.cb_search=create cb_search
this.cb_clear=create cb_clear
this.cb_view_purchasing_employer=create cb_view_purchasing_employer
this.dw_emp_list=create dw_emp_list
this.dw_search_criteria=create dw_search_criteria
this.dw_employer_data=create dw_employer_data
this.sb_emp_search=create sb_emp_search
this.dw_coverage=create dw_coverage
this.dw_employer_operation_list=create dw_employer_operation_list
this.Control[]={this.cb_cancel,&
this.cb_ok,&
this.cb_back,&
this.cb_empl_detail,&
this.tab_coverage,&
this.dw_employer_operation_payroll,&
this.cb_search,&
this.cb_clear,&
this.cb_view_purchasing_employer,&
this.dw_emp_list,&
this.dw_search_criteria,&
this.dw_employer_data,&
this.sb_emp_search,&
this.dw_coverage,&
this.dw_employer_operation_list}
end on

on w_employer_search.destroy
destroy(this.cb_cancel)
destroy(this.cb_ok)
destroy(this.cb_back)
destroy(this.cb_empl_detail)
destroy(this.tab_coverage)
destroy(this.dw_employer_operation_payroll)
destroy(this.cb_search)
destroy(this.cb_clear)
destroy(this.cb_view_purchasing_employer)
destroy(this.dw_emp_list)
destroy(this.dw_search_criteria)
destroy(this.dw_employer_data)
destroy(this.sb_emp_search)
destroy(this.dw_coverage)
destroy(this.dw_employer_operation_list)
end on

type cb_cancel from commandbutton within w_employer_search
integer x = 1774
integer y = 1608
integer width = 416
integer height = 108
integer taborder = 120
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
end type

event clicked;S_WINDOW_MESSAGE lstr_message

lstr_message.al_doubleparm[1] = 0
lstr_message.as_stringparm[1] = ''
lstr_message.al_doubleparm[2] = 0
lstr_message.as_stringparm[2] = ''
lstr_message.as_stringparm[3] = ''
lstr_message.as_stringparm[4] = ''
lstr_message.as_stringparm[5] = ''

CloseWithReturn(PARENT, lstr_message)
end event

type cb_ok from commandbutton within w_employer_search
integer x = 1344
integer y = 1608
integer width = 416
integer height = 108
integer taborder = 110
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;Long 		ll_row
INT    	li_error
S_WINDOW_MESSAGE  lstr_message

//	get the data from the employer data window if any data there
//	this way if the employer has been sold then the information will be picked up correctly
ll_row = dw_emp_list.GetRow()

IF dw_employer_data.RowCount() = 1 THEN 
	lstr_message.al_doubleparm[1] = dw_employer_data.GetItemNumber(1, 'employer_no')
	lstr_message.as_stringparm[1] = dw_employer_data.GetItemString(1, 'employer_employer_legal_name')
	lstr_message.as_stringparm[3] = dw_employer_data.GetItemString(1, 'employer_type_code')

ELSEIF ll_row > 0 THEN
	lstr_message.al_doubleparm[1] = dw_emp_list.GetItemNumber(ll_row, 'employer_no')
	lstr_message.as_stringparm[1] = dw_emp_list.GetItemString(ll_row, 'employer_name')
	lstr_message.as_stringparm[3] = dw_emp_list.GetItemString(ll_row, 'employer_type_code')
ELSE
	lstr_message.al_doubleparm[1] = 0
	lstr_message.as_stringparm[1] = ''
	lstr_message.as_stringparm[3] = ''
END IF

ll_row = dw_employer_operation_list.GetRow()

IF dw_employer_data.RowCount() = 1 THEN
	lstr_message.al_doubleparm[2] = dw_employer_data.GetItemNumber(1, 'operation_operation_no')
	lstr_message.as_stringparm[2] = dw_employer_data.GetItemString(1, 'operation_operation_name')
	lstr_message.as_stringparm[4] = dw_employer_data.GetItemString(1, 'classification_system_code')
	lstr_message.as_stringparm[5] = dw_employer_data.GetItemString(1, 'classification_system_type_code')
END IF

IF ll_row > 0 THEN
	lstr_message.al_doubleparm[2] = dw_employer_operation_list.GetItemNumber(ll_row, 'operation_no')
	lstr_message.as_stringparm[2] = dw_employer_operation_list.GetItemString(ll_row, 'operation_name')
	lstr_message.as_stringparm[4] = dw_employer_operation_list.GetItemString(ll_row, 'classification_system_code')
	lstr_message.as_stringparm[5] = dw_employer_operation_list.GetItemString(ll_row, 'classification_system_type_code')
ELSE
	lstr_message.al_doubleparm[2] = 0
	lstr_message.as_stringparm[2] = ''
	lstr_message.as_stringparm[4] = ''
	lstr_message.as_stringparm[5] = ''
END IF

/* Ed Lenarczyk  Aug 17, 99 - End of new structure   */

/* code added to allow for a search of employer independant of the claim window
*/
li_error = CloseWithReturn(PARENT, lstr_message)
IF li_error = -1 THEN
	CLOSE(PARENT)
END IF
end event

type cb_back from commandbutton within w_employer_search
boolean visible = false
integer x = 910
integer y = 1608
integer width = 416
integer height = 108
integer taborder = 130
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Prev"
end type

event clicked;dw_coverage.Hide()
tab_coverage.Hide()
dw_employer_data.Hide()
dw_employer_operation_payroll.Hide()
cb_empl_detail.Hide()
cb_back.Visible = FALSE
sb_emp_search.Visible = TRUE

PARENT.Width = 2738

dw_emp_list.Show()
dw_search_criteria.Show()
dw_employer_operation_list.Show()
cb_search.Visible = TRUE
cb_search.Enabled = TRUE
cb_empl_detail.Visible = TRUE
cb_clear.Visible = TRUE
end event

type cb_empl_detail from commandbutton within w_employer_search
boolean visible = false
integer x = 905
integer y = 1608
integer width = 416
integer height = 108
integer taborder = 90
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Empl Details"
end type

on clicked;dw_employer_operation_list.TriggerEvent(DoubleClicked!)
end on

type tab_coverage from tab within w_employer_search
boolean visible = false
integer x = 23
integer y = 1084
integer width = 2683
integer height = 516
integer taborder = 80
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
boolean raggedright = true
boolean focusonbuttondown = true
integer selectedtab = 2
tabpage_emp_op_coverage tabpage_emp_op_coverage
tabpage_seasonal_coverage tabpage_seasonal_coverage
tabpage_personal_coverage tabpage_personal_coverage
tabpage_volunteer_coverage tabpage_volunteer_coverage
end type

on tab_coverage.create
this.tabpage_emp_op_coverage=create tabpage_emp_op_coverage
this.tabpage_seasonal_coverage=create tabpage_seasonal_coverage
this.tabpage_personal_coverage=create tabpage_personal_coverage
this.tabpage_volunteer_coverage=create tabpage_volunteer_coverage
this.Control[]={this.tabpage_emp_op_coverage,&
this.tabpage_seasonal_coverage,&
this.tabpage_personal_coverage,&
this.tabpage_volunteer_coverage}
end on

on tab_coverage.destroy
destroy(this.tabpage_emp_op_coverage)
destroy(this.tabpage_seasonal_coverage)
destroy(this.tabpage_personal_coverage)
destroy(this.tabpage_volunteer_coverage)
end on

event selectionchanged;//IF Message.WordParm <> 0 THEN
//	newindex = Message.WordParm
//END IF

CHOOSE CASE newindex
	CASE 1
		//dw_coverage.DataObject = ''
		//dw_coverage.SetTransObject(SQLCA)
		//dw_coverage.Retrieve()
	CASE 2
		dw_coverage.DataObject = 'd_seasonal_coverage'
		dw_coverage.SetTransObject(SQLCA)
		dw_coverage.Retrieve(il_employer_no,ii_operation_no)
		PARENT.Width = 2738
		THIS.Width = 2683
		dw_coverage.Width = 2624
		dw_employer_operation_payroll.Width = 2642
		dw_employer_data.Width = 2642
	CASE 3
		dw_coverage.DataObject = 'd_personal_coverage'
		dw_coverage.SetTransObject(SQLCA)
		dw_coverage.Retrieve(il_employer_no,ii_operation_no)
		PARENT.Width = 3038
		THIS.Width = 2983
		dw_coverage.Width = 2924
		dw_employer_operation_payroll.Width = 2942
		dw_employer_data.Width = 2942
	CASE 4
		dw_coverage.DataObject = 'd_volunteer_coverage'
		dw_coverage.SetTransObject(SQLCA)
		dw_coverage.Retrieve(il_employer_no,ii_operation_no)
		PARENT.Width = 2738
		THIS.Width = 2683
		dw_coverage.Width = 2624
		dw_employer_operation_payroll.Width = 2642
		dw_employer_data.Width = 2642
END CHOOSE

end event

type tabpage_emp_op_coverage from userobject within tab_coverage
boolean visible = false
integer x = 18
integer y = 108
integer width = 2647
integer height = 392
long backcolor = 67108864
string text = "Empl/Operation Coverage"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
end type

type tabpage_seasonal_coverage from userobject within tab_coverage
integer x = 18
integer y = 108
integer width = 2647
integer height = 392
long backcolor = 67108864
string text = "Seasonal Coverage"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
end type

type tabpage_personal_coverage from userobject within tab_coverage
integer x = 18
integer y = 108
integer width = 2647
integer height = 392
long backcolor = 67108864
string text = "Personal Coverage"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
end type

type tabpage_volunteer_coverage from userobject within tab_coverage
integer x = 18
integer y = 108
integer width = 2647
integer height = 392
long backcolor = 67108864
string text = "Volunteer Coverage"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
end type

type dw_employer_operation_payroll from u_dw_online within w_employer_search
integer x = 50
integer y = 692
integer width = 2642
integer height = 384
integer taborder = 61
string dataobject = "d_employer_operation_payroll"
boolean vscrollbar = true
borderstyle borderstyle = styleraised!
end type

type cb_search from commandbutton within w_employer_search
integer x = 2313
integer y = 144
integer width = 283
integer height = 108
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Search"
boolean default = true
end type

event clicked;Long   ll_employer_no, ll_ReturnCode,	ll_search_employer_no
String ls_employer_name, ls_name, ls_employer_name_type_code, ls_search_type_code,	ls_select



//	This script is used to retrieve employers based on the search criteria entered.
SetPointer(HourGlass!)

//	Force an Accept Text to ensure that all data items have been validated.
dw_employer_operation_list.Reset()
dw_emp_list.Reset()
cb_empl_detail.Visible = FALSE

IF dw_search_criteria.AcceptText() = -1 THEN
	SetPointer(Arrow!)
	Return
END IF

//	Verify that the search criteria is valid. Either name OR number must be entered.
ls_search_type_code = dw_search_criteria.GetItemString(1,"search_type_code")
ls_name =  dw_search_criteria.GetItemString(1,"employer_name")
ls_employer_name_type_code = dw_search_criteria.GetItemString(1,"employer_name_type_code")
ll_search_employer_no = dw_search_criteria.GetItemNumber(1,"employer_no")

IF ls_name <= "" AND (IsNull(ll_search_employer_no) OR ll_search_employer_no = 0) THEN
	MessageBox("Invalid Search Criteria","You must search by either Name or Number. Please enter the value that you want to search by.",Exclamation!)
	Goto Common_Exit
END IF

IF ls_name > "" AND	ll_search_employer_no > 0 THEN
	MessageBox("Invalid Search Criteria","Search by Name OR Employer Number!",StopSign!)
	Goto Common_Exit
END IF

//	Select the appropriate search.
ls_select = is_select
IF is_type = 'A' THEN
	f_replace_text(ls_select,"WHERE ","WHERE (E.employer_type_code in (~~'A~~',~~'S~~')) and ")
ELSEIF is_type = 'C' THEN
	f_replace_text(ls_select,"WHERE ","WHERE (E.employer_type_code in (~~'A~~',~~'S~~',~~'G~~',~~'R~~')) and ")
ELSEIF is_type = 'P' THEN
	f_replace_text(ls_select,"WHERE ","WHERE (E.employer_type_code in (~~'P~~',~~'S~~' )) and ")
ELSEIF is_type = 'F' THEN
	f_replace_text(ls_select,"WHERE ","WHERE (E.employer_type_code in (~~'F~~')) and ")
END IF

IF ls_employer_name_type_code = 'B' THEN
	f_replace_text(ls_select,"AND EN.employer_name_type_code = :ls_employer_name_type_code","AND ( EN.employer_name_type_code = :ls_employer_name_type_code OR :ls_employer_name_type_code = ~~'B~~' )")
END IF


IF ll_search_employer_no > 0 THEN		// Search for employer using employer # (retreive only Legal Names)
	f_replace_text(ls_select,"employer_name = :ls_employer_name","employer_no = :ls_employer_no")
	f_replace_text(ls_select,"employer_name like :ls_employer_name","employer_no = :ls_employer_no")

	dw_emp_list.Modify("DataWindow.Table.Select='" + ls_select + "'")

	ll_ReturnCode = dw_emp_list.Retrieve('',ll_search_employer_no,'L')

ELSEIF ls_search_type_code = "E" THEN		//	Search for employer with exact name search
	ls_employer_name = Trim(Upper(ls_name))
	f_replace_text(ls_select,"employer_no = :ls_employer_no","EN.employer_name = :ls_employer_name")
	f_replace_text(ls_select,"EN.employer_name like :ls_employer_name","EN.employer_name = :ls_employer_name")
	dw_emp_list.Modify("DataWindow.Table.Select='" + ls_select + "'")
	ll_ReturnCode = dw_emp_list.Retrieve(ls_employer_name,0,ls_employer_name_type_code)

ELSEIF ls_search_type_code = "S" THEN		// Search for employer with name starting with entered name
	ls_employer_name = Trim(Upper(ls_name)) + "%"
	f_replace_text(ls_select,"employer_no = :ls_employer_no","EN.employer_name like :ls_employer_name")
	f_replace_text(ls_select,"EN.employer_name = :ls_employer_name","EN.employer_name like :ls_employer_name")
	ls_select =	dw_emp_list.Modify("DataWindow.Table.Select='" + ls_select + "'")
	dw_emp_list.Describe("DataWindow.Table.Select")
	ll_ReturnCode = dw_emp_list.Retrieve(ls_employer_name,0,ls_employer_name_type_code)

ELSEIF ls_search_type_code = "C" THEN 	// Search for employer with name containing entered name
	ll_ReturnCode = MessageBox("Warning!","The search you have requested has the potential to lock your PC for a couple of minutes. Do you want to continue?",Question!,YesNo!)
	IF ll_ReturnCode = 1 THEN
		ls_employer_name = "%" + Trim(Upper(ls_name)) + "%"
		f_replace_text(ls_select,"employer_no = :ls_employer_no","EN.employer_name like :ls_employer_name")
		f_replace_text(ls_select,"EN.employer_name = :ls_employer_name","EN.employer_name like :ls_employer_name")
		dw_emp_list.Modify("DataWindow.Table.Select='" + ls_select + "'")
		ll_ReturnCode = dw_emp_list.Retrieve(ls_employer_name,0,ls_employer_name_type_code)
	ELSE
		Goto Common_Exit
	END IF
END IF

IF SQLCA.nf_handle_error("w_employer_search","dw_emp_list","clicked for cb_search") < 0 THEN
	SetPointer(Arrow!)
	cb_cancel.TriggerEvent(Clicked!)
	Return 
END IF
IF ll_ReturnCode > 0 THEN
	ll_employer_no = dw_emp_list.GetItemNumber(1,"employer_no")
	dw_emp_list.Sort()
	dw_emp_list.SetRow(1)
	dw_emp_list.SelectRow(0, False)
	dw_emp_list.SelectRow(1, True)
	dw_emp_list.Show()
ELSE
	MessageBox("No Employers Found","Could not locate any employers matching your search criteria.",Exclamation!)
	Goto Common_Exit
END IF

//dw_emp_list.TriggerEvent(rowfocuschanged!)

Common_Exit:
	SetPointer(Arrow!)

RETURN
end event

type cb_clear from commandbutton within w_employer_search
integer x = 2313
integer y = 256
integer width = 283
integer height = 108
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Clear"
end type

event clicked;Long ll_ReturnCode

SetPointer(HourGlass!)

//  Disable all the command buttons while the script is executing
dw_search_criteria.SetItem(1,"employer_name_type_code","L")
dw_search_criteria.SetItem(1,"search_type_code","S")
dw_search_criteria.SetItem(1,"employer_name","")
dw_search_criteria.SetItem(1,"employer_no",0)
dw_search_criteria.SetFocus()

dw_employer_operation_list.Reset()
dw_emp_list.Reset()
cb_empl_detail.Visible = FALSE

SetPointer(Arrow!)

end event

type cb_view_purchasing_employer from commandbutton within w_employer_search
boolean visible = false
integer x = 1833
integer y = 572
integer width = 754
integer height = 88
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&View Purchasing Employer"
end type

event clicked;Long     ll_ReturnCode, ll_year, ll_purchasing_employer_no, ll_purchasing_operation_no
Datetime ldt_f100_receipt_date, ldt_start_date
String   ls_ReturnCode
INTEGER li_tab_index

ll_year = Year(Today())

//  Disable all the buttons while the script is running.
cb_view_purchasing_employer.Enabled = FALSE
cb_clear.Enabled = FALSE
cb_search.Enabled = FALSE
cb_empl_detail.Enabled = FALSE
SetPointer(HourGlass!)

ll_purchasing_employer_no = dw_employer_data.GetItemNumber(1, "operation_sold_to_employer_no")
ll_purchasing_operation_no = dw_employer_data.GetItemNumber(1, "operation_sold_to_operation_no")

IF ll_purchasing_employer_no > 0 THEN
	il_employer_no = ll_purchasing_employer_no
	ii_operation_no = dw_employer_data.GetItemNumber(1,"operation_sold_to_operation_no")
	li_tab_index = tab_coverage.SelectedTab
	tab_coverage.Event SelectionChanged(li_tab_index,li_tab_index)
END IF

ll_ReturnCode = dw_employer_data.Retrieve(ll_purchasing_employer_no, ll_purchasing_operation_no)
IF SQLCA.nf_handle_error("w_employer_search","dw_employer_data","clicked for cb_view_purchasing_employer") < 0 THEN
	cb_cancel.TriggerEvent(Clicked!)
	RETURN
ELSEIF ll_ReturnCode = 0 THEN
	MessageBox("Employer Information","Invalid Employer and Rate Group combination. Please correct and try again.")
	cb_view_purchasing_employer.Enabled = TRUE
	cb_clear.Enabled = TRUE
	cb_search.Enabled = TRUE
	cb_empl_detail.Enabled = TRUE
	SetPointer(Arrow!)
	RETURN
END IF

dw_employer_operation_payroll.Retrieve(ll_purchasing_employer_no, ll_purchasing_operation_no)
IF SQLCA.nf_handle_error("w_employer_search","dw_employer_operation_payroll","clicked for cb_view_purchasing_employer") < 0 THEN
	cb_cancel.TriggerEvent(Clicked!)
	RETURN
END IF

//	 IF the current employer/rate group has been sold, THEN display the command button to allow the
//  user to view the purchasing employer.
ll_purchasing_employer_no = dw_employer_data.GetItemNumber(1,"operation_sold_to_employer_no")
IF ll_purchasing_employer_no > 0 THEN
	cb_view_purchasing_employer.Visible = TRUE
ELSE
	cb_view_purchasing_employer.Visible = FALSE
END IF

//  IF the F100 Receipt date is <= jan 1, 1900, THEN don't show the date.
ldt_f100_receipt_date = dw_employer_data.GetItemDateTime(1,"employer_last_form100_received_date")
IF ldt_f100_receipt_date = ldt_start_date THEN
	dw_employer_data.Modify("employer_last_form100_received_date.Visible=0")
END IF

cb_view_purchasing_employer.Enabled = TRUE
cb_view_purchasing_employer.BringToTop = TRUE
cb_clear.Enabled = TRUE
cb_search.Enabled = TRUE
cb_empl_detail.Enabled = TRUE
SetPointer(Arrow!)
end event

type dw_emp_list from u_dw_online within w_employer_search
integer x = 18
integer y = 448
integer width = 2688
integer height = 704
integer taborder = 60
boolean bringtotop = true
string dataobject = "d_search_employer"
boolean vscrollbar = true
boolean livescroll = false
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;call super::rowfocuschanged;LONG    ll_num_rows, ll_employer_no, ll_row, ll_charge_to_ind_operation_no, ll_find
STRING  ls_find



SelectRow(0, False)
ll_row = dw_emp_list.GetRow()

IF ll_row <= 0 OR IsNull(ll_row) THEN
	Return
END IF

SelectRow(ll_row, TRUE)

ll_employer_no = GetItemNumber(ll_row, "employer_no")


ll_num_rows = dw_employer_operation_list.Retrieve(ll_employer_no)
SQLCA.nf_handle_error("w_employer_search","dw_employer_operation_list","clicked for dw_search_employer")

IF ll_num_rows = 0 THEN
	MessageBox("Employer","Employer has no operations listed.")
	RETURN
ELSE
	IF ll_employer_no = 4003 AND il_accident_employer_no > 0 AND il_accident_operation_no > 0 THEN
		IF is_charge_to_ind_message_displayed_flag = 'Y' THEN
			is_charge_to_ind_message_displayed_flag = 'N'
		ELSE
			// attempt to default the 'Charge to Industry' operation number to the one associated
			// with the accident operation number
	
			SELECT a.operation_no
			INTO   :ll_charge_to_ind_operation_no
			FROM   OPERATION_CLASSIFICATION a
			WHERE  employer_no  = 4003
			AND EXISTS ( SELECT *
							 FROM   OPERATION_CLASSIFICATION b
							 WHERE  b.classification_system_code = a.classification_system_code
							 AND    b.employer_no  = :il_accident_employer_no
							 AND    b.operation_no = :il_accident_operation_no )
			USING SQLCA;
			SQLCA.nf_handle_error('n_claims','embedded SQL: SELECT operation_no FROM OPERATION_CLASSIFICATION...', 'nf_change_item')
			
			ls_find = 'operation_no = ' + String(ll_charge_to_ind_operation_no)
			
			ll_find = dw_employer_operation_list.Find(ls_find,0,ll_num_rows)
			
			IF ll_find > 0 THEN
				dw_employer_operation_list.SetRow(ll_find)
				dw_employer_operation_list.ScrollToRow(ll_find)
				dw_employer_operation_list.SelectRow(ll_find, TRUE)
				MessageBox('Charge to Industry','The "Charge to Industry" operation that is consistent with the Accident Operation for this claim has been selected.',Information!)
				is_charge_to_ind_message_displayed_flag = 'Y'
			ELSE
				dw_employer_operation_list.SelectRow(0, FALSE)
				dw_employer_operation_list.SelectRow(1, TRUE)
			END IF
		END IF
		
	ELSE
		dw_employer_operation_list.SelectRow(0, FALSE)
		dw_employer_operation_list.SelectRow(1, TRUE)
	END IF
END IF

end event

event rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup

/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	
	lm_popup.m_options.PopMenu(iw_win.PointerX( ), iw_win.PointerY( ))

	Destroy lm_popup
end event

event constructor;call super::constructor;uf_SetSort(True)
end event

type dw_search_criteria from u_dw_online within w_employer_search
integer x = 18
integer y = 16
integer width = 2688
integer height = 428
integer taborder = 10
string dataobject = "d_employer_search_criteria"
borderstyle borderstyle = styleraised!
end type

event itemchanged;call super::itemchanged;CHOOSE CASE dwo.name
   CASE "employer_name"
  	   IF data > "" AND Len(data) < 2 THEN
   		MessageBox("Invalid Name","Employer Name must contain at least 2 characters!",StopSign!)
		   RETURN 1
   	END IF

   CASE "employer_no"
	   IF data > " " THEN
   		IF Len(data) > 6 THEN
   			MessageBox("Invalid Employer Number","Employer Number must not contain more than 6 digits.",StopSign!)
			   RETURN 1
			END IF
	   END IF
END CHOOSE


end event

type dw_employer_data from u_dw_online within w_employer_search
boolean visible = false
integer x = 50
integer y = 16
integer width = 2642
integer height = 680
integer taborder = 50
string dataobject = "d_employer_data"
borderstyle borderstyle = styleraised!
end type

type sb_emp_search from u_splitbar_horizontal within w_employer_search
integer x = 18
integer y = 1152
integer width = 2688
integer height = 64
boolean bringtotop = true
end type

type dw_coverage from u_dw_online within w_employer_search
boolean visible = false
integer x = 50
integer y = 1208
integer width = 2624
integer height = 376
integer taborder = 130
string title = "none"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type dw_employer_operation_list from u_dw_online within w_employer_search
integer x = 18
integer y = 1216
integer width = 2688
integer height = 372
integer taborder = 70
boolean bringtotop = true
string dataobject = "d_employer_operation_list"
boolean vscrollbar = true
boolean livescroll = false
borderstyle borderstyle = stylelowered!
end type

event doubleclicked;call super::doubleclicked;//	 This script loads the datawindows with the detailed employer/group data and ensures that
//  the correct datawindows and objects are visible and all others are hidden.
//  It also sets the text of the activity status static text object based on the
//  employer data. This text can then be checked to ensure that the user isn't selecting
//  an invalid employer/operation.
Long     ll_year, ll_operation_row_no, ll_ReturnCode, ll_purchasing_employer_no

SetPointer(HourGlass!)

//  Get the current highlighted operation.
ll_operation_row_no = GetRow()
IF ll_operation_row_no <= 0 THEN
	GoTo Common_Exit
END IF

il_employer_no =  This.GetItemNumber(ll_operation_row_no, "employer_no")
ii_operation_no = This.GetItemNumber(ll_operation_row_no, "operation_no")

ll_year = Year(Date(f_server_datetime()))

//  Hide the search datawindows.
THIS.Hide()
dw_search_criteria.Hide()
dw_emp_list.Hide()
cb_search.Visible = FALSE
cb_empl_detail.Visible = FALSE
cb_clear.Visible = FALSE

//  Load the datawindows with the detailed employer data.
ll_ReturnCode = dw_employer_data.Retrieve(il_employer_no, ii_operation_no)
IF SQLCA.nf_handle_error("w_employer_search","dw_employer_data","retrieve for dw_employer_data") < 0 THEN
	SetPointer(Arrow!)
	cb_cancel.TriggerEvent(Clicked!)
	Return 
ELSEIF ll_ReturnCode = 0 THEN
	MessageBox("Employer Information","Invalid Employer and Rate Group combination. Please correct and try again.")
	GoTo Common_Exit
END IF

dw_employer_operation_payroll.Retrieve(il_employer_no, ii_operation_no)
IF SQLCA.nf_handle_error("w_employer_search","dw_employer_operation_payroll","retrieve for dw_employer_operation_payroll") < 0 THEN
	SetPointer(Arrow!)
	cb_cancel.TriggerEvent(Clicked!)
	Return 
END IF

//  Show the detail employer data datawindows.
tab_coverage.Show()
dw_coverage.Show()
dw_employer_data.Show()
dw_employer_operation_payroll.Show()
cb_back.Visible = TRUE
sb_emp_search.Visible = FALSE

tab_coverage.SelectTab(2)
dw_coverage.DataObject = 'd_seasonal_coverage'
dw_coverage.SetTransObject(SQLCA)
dw_coverage.Retrieve(il_employer_no,ii_operation_no)

//	 IF the current employer/rate group has been sold, THEN display the command button to allow the
//  user to view the purchasing employer.
IF THIS.getrow() > 0 THEN
	ll_purchasing_employer_no = This.GetItemNumber(THIS.getrow(), "sold_to_employer_no")
	IF ll_purchasing_employer_no > 0 THEN
		cb_view_purchasing_employer.Visible = TRUE
	ELSE
		cb_view_purchasing_employer.Visible = FALSE
	END IF
END IF 

Common_Exit:
SetPointer(Arrow!)

end event

event retrieveend;call super::retrieveend;IF THIS.RowCount() > 0 THEN
	il_employer_no = THIS.GetItemNumber(THIS.GetRow(),'employer_no')
	ii_operation_no = THIS.GetItemNumber(THIS.GetRow(),'operation_no') 
	cb_empl_detail.Visible = TRUE
END IF
end event

event rowfocuschanged;call super::rowfocuschanged;SelectRow(0, FALSE)

IF currentrow <= 0 THEN
	Return
END IF

SelectRow(currentrow, TRUE)

end event

event clicked;call super::clicked;//	Turn off highlighting of all rows
dw_employer_operation_list.SelectRow (0, False)

// Highlight the row that has been clicked
IF row = 0 THEN
	RETURN
End IF

dw_employer_operation_list.SelectRow(row, TRUE)

end event

event rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup


/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	
	lm_popup.m_options.PopMenu(iw_win.PointerX( ), iw_win.PointerY( ))

	Destroy lm_popup
end event

event constructor;call super::constructor;uf_SetSort(True)
end event

