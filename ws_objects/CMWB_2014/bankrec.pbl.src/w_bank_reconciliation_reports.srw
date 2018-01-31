$PBExportHeader$w_bank_reconciliation_reports.srw
$PBExportComments$Outstanding Cheque Listing/Reconciled Cheque Listing Reports
forward
global type w_bank_reconciliation_reports from w_a_report
end type
type cb_ok from commandbutton within w_bank_reconciliation_reports
end type
type gb_1 from groupbox within w_bank_reconciliation_reports
end type
type dw_bankrec_parameters from u_dw_online within w_bank_reconciliation_reports
end type
type cb_clear from commandbutton within w_bank_reconciliation_reports
end type
type cb_export from commandbutton within w_bank_reconciliation_reports
end type
end forward

global type w_bank_reconciliation_reports from w_a_report
string title = "Bank Reconcilation Reports"
cb_ok cb_ok
gb_1 gb_1
dw_bankrec_parameters dw_bankrec_parameters
cb_clear cb_clear
cb_export cb_export
end type
global w_bank_reconciliation_reports w_bank_reconciliation_reports

type variables

end variables

event open;call super::open;
LONG ll_result
DATAWINDOWCHILD ldwc_cheque_type

/*	Database Connections 
*/

dw_report.SetTransObject(SQLCA)

			
dw_bankrec_parameters.SetTransObject(SQLCA)
dw_bankrec_parameters.insertrow(0)

/* Add option for all cheque types */
IF dw_bankrec_parameters.GetChild('cheque_type', ldwc_cheque_type) < 0 THEN
	MessageBox('Error', 'Not a DW child')
END IF

ldwc_cheque_type.InsertRow(1)
ldwc_cheque_type.SetItem(1, 'cheque_type_code', 'ALL')
ldwc_cheque_type.SetItem(1, 'cheque_type_desc', 'All Cheque Types')
end event

on w_bank_reconciliation_reports.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.gb_1=create gb_1
this.dw_bankrec_parameters=create dw_bankrec_parameters
this.cb_clear=create cb_clear
this.cb_export=create cb_export
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.gb_1
this.Control[iCurrent+3]=this.dw_bankrec_parameters
this.Control[iCurrent+4]=this.cb_clear
this.Control[iCurrent+5]=this.cb_export
end on

on w_bank_reconciliation_reports.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.gb_1)
destroy(this.dw_bankrec_parameters)
destroy(this.cb_clear)
destroy(this.cb_export)
end on

type dw_report from w_a_report`dw_report within w_bank_reconciliation_reports
integer y = 856
integer height = 1680
integer taborder = 60
string dataobject = "d_issued_but_not_transmitted"
boolean hscrollbar = true
end type

type cb_ok from commandbutton within w_bank_reconciliation_reports
integer x = 2359
integer y = 56
integer width = 293
integer height = 108
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
boolean default = true
end type

event clicked;/*	Initialization*/

LONG		ll_numrows
STRING	ls_reconciled_code,ls_cheque_type,DWfilter2, ls_commit
DATE adt_from_date , adt_to_date 
INTEGER	li_report_type , li_rc, li_days_in_month
DATETIME adtm_from_date , adtm_to_date 



dw_bankrec_parameters.AcceptText()


/* Validate date range. To date must be greater than the from date. As well, add one to the to date
	to ensure getting all items created that day, regardless of what time they were created. */ 
	
	adt_from_date = Date(dw_bankrec_parameters.GetItemDateTime(1,"from_date"))
	adt_to_date = Date(dw_bankrec_parameters.GetItemDateTime(1,"to_date"))
	
	
	
	li_report_type = dw_bankrec_parameters.GetItemNumber(1,"report_type")
	ls_reconciled_code = dw_bankrec_parameters.GetItemString(1,"reconciled_code")
	ls_cheque_type = dw_bankrec_parameters.GetItemString(1,"cheque_type")
	
	
	
	CHOOSE CASE li_report_type
	CASE 1
		dw_report.DataObject = 'd_issued_but_not_transmitted'
		dw_report.SetTransobject(SQLCA)
		ls_reconciled_code = ''
	CASE 2
		CHOOSE CASE Month(adt_to_date)
			CASE 1, 3, 5, 7, 8, 10, 12
				li_days_in_month = 31
			CASE 4, 6, 9, 11
				li_days_in_month = 30
			CASE 2
				IF Mod(Year(adt_to_date),4) = 0 THEN
					li_days_in_month = 29
				ELSE
					li_days_in_month = 28
				END IF		
		END CHOOSE
		
		IF Day(adt_to_date) = li_days_in_month THEN
			ls_reconciled_code = 'Y'  // this variable is being used to indicate that the end of the month was entered
		ELSE
			ls_reconciled_code = 'N' // this variable is being used to indicate that the end of the month was not entered
		END IF
		
		dw_report.DataObject = 'd_issued_but_not_reconciled_sp'
		dw_report.SetTransobject(SQLCA)
		adtm_from_date = DateTime(adt_from_date,00:00:00)
		adtm_to_date = DateTime(adt_to_date,00:00:00)
		
								
		ll_numrows = dw_report.Retrieve(adtm_from_date,adtm_to_date,ls_reconciled_code)
		SQLCA.nf_handle_error("w_bank_reconciliation_reports","dw_report","cb_ok")
		IF ll_numrows <= 0 then
			MessageBox("Bank Reconciliation Reports","No data found to satisfy request")			
			return
		END IF
		
	CASE 3
		dw_report.DataObject = 'd_reconciled'
		dw_report.SetTransobject(SQLCA)
		if isnull(ls_reconciled_code) then
			MessageBox("Missing Reconciled Code","You must select a Reconciled Code from drop down!",Exclamation!)
			RETURN
		end if
			
	CASE ELSE
		MessageBox("Missing Report Type","You must select a valid report type from drop down!",Exclamation!)
		RETURN
	END CHOOSE


	IF IsNull(adt_from_date) or &
		IsNull(adt_to_date) THEN
		MessageBox("Validation Error","Both the from and to dates must have a value.",Exclamation!)
		Return
	END IF

	IF adt_from_date < Date(1900,01,01) OR &
		adt_to_date < Date(1900,01,01) THEN
		MessageBox("Validation Error","Dates cannot be earlier than 1900-01-01!",Exclamation!)
		Return
	END IF

	IF adt_from_date > Date(2079,06,06) OR adt_to_date > Date(2079,06,06) THEN
		MessageBox("Validation Error","Dates cannot be later than 2079-06-06!",Exclamation!)
		Return
	END IF
	
	IF adt_from_date >= adt_to_date then
		MessageBox("Validation Error","The to date must be later than the from date",Exclamation!)
		Return
	END IF
	
	IF li_report_type <> 2 THEN
		ll_numrows = dw_report.Retrieve(adt_from_date,adt_to_date,ls_reconciled_code)
		SQLCA.nf_handle_error("w_bank_reconciliation_reports","dw_report","cb_ok")
		IF ll_numrows <= 0 then
			MessageBox("Bank Reconciliation Reports","No data found to satisfy request")
			return
		END IF
	END IF

	
	if ls_cheque_type > '' AND ls_cheque_type <> 'ALL' then
	
		/* need to filter */
		DWfilter2 = "cheque_type_code = '" + ls_cheque_type + "'"
		dw_report.SetFilter(DWfilter2)
		li_rc = dw_report.Filter( )	
		if dw_report.rowcount() = 0 then
			MessageBox("Bank Reconciliation Reports","No data found to satisfy request")
			return
		end if
	else
		DWfilter2 = ""
		dw_report.SetFilter(DWfilter2)
		li_rc = dw_report.Filter( )	
		if dw_report.rowcount() = 0 then
			MessageBox("Bank Reconciliation Reports","No data found to satisfy request")
			return
		end if
	end if
	

end event

type gb_1 from groupbox within w_bank_reconciliation_reports
integer x = 5
integer y = 28
integer width = 2309
integer height = 780
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Select Criteria"
end type

type dw_bankrec_parameters from u_dw_online within w_bank_reconciliation_reports
integer x = 78
integer y = 116
integer width = 2121
integer height = 596
integer taborder = 30
boolean bringtotop = true
string dataobject = "d_bankrec_parameters"
boolean border = false
end type

event itemchanged;DATAWINDOWCHILD ldwc_child

IF dwo.name = 'report_type' THEN
	THIS.SetColumn('to_date')
	THIS.SetText('0000-00-00')
	THIS.SetColumn('from_date')
	THIS.SetText('0000-00-00')
	
	IF data = '3' THEN
		THIS.Object.reconciled_code.Visible = TRUE
		THIS.GetChild( 'reconciled_code' , ldwc_child )
		ldwc_child.Reset()
		ldwc_child.SetTransObject(SQLCA)
		ldwc_child.Retrieve()
		ldwc_child.ScrollToRow(1)
	ELSE
		THIS.SetColumn('reconciled_code')
		THIS.SetText('')
		THIS.AcceptText()
		THIS.Object.reconciled_code.Visible = FALSE
	END IF
	
	THIS.GetChild( 'cheque_type' , ldwc_child )
	ldwc_child.Reset()
	ldwc_child.SetTransObject(SQLCA)
	ldwc_child.Retrieve()

	dw_bankrec_parameters.SetColumn('report_type')
END IF


end event

type cb_clear from commandbutton within w_bank_reconciliation_reports
integer x = 2359
integer y = 212
integer width = 293
integer height = 108
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Clear"
end type

event clicked;DATAWINDOWCHILD ldwc_child

dw_bankrec_parameters.reset()
dw_bankrec_parameters.insertrow(0)

// SR 94
dw_bankrec_parameters.SetColumn('to_date')
dw_bankrec_parameters.SetText('0000-00-00')
dw_bankrec_parameters.SetColumn('from_date')
dw_bankrec_parameters.SetText('0000-00-00')

dw_bankrec_parameters.SetColumn('reconciled_code')
dw_bankrec_parameters.SetText('')
dw_bankrec_parameters.AcceptText()
dw_bankrec_parameters.Object.reconciled_code.Visible = FALSE

dw_bankrec_parameters.GetChild( 'cheque_type' , ldwc_child )
ldwc_child.Reset()
ldwc_child.SetTransObject(SQLCA)
ldwc_child.Retrieve()

dw_bankrec_parameters.SetColumn('report_type')

end event

type cb_export from commandbutton within w_bank_reconciliation_reports
integer x = 2359
integer y = 384
integer width = 293
integer height = 108
integer taborder = 50
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Export"
end type

event clicked;// P10151-84 - Bank Reconciliation reports - add ability to produce reports in Excel - July 30, 2007

IF dw_report.RowCount() = 0 THEN
	MessageBox('No rows','There are no reconciliations to extract.')
	Return
END IF

dw_report.SaveAs('',Excel!,True)	

end event

