$PBExportHeader$w_gl_txn_report.srw
forward
global type w_gl_txn_report from w_a_report
end type
type dw_search from u_dw_online within w_gl_txn_report
end type
type cb_retrieve from commandbutton within w_gl_txn_report
end type
type cbx_unapplied_txns from checkbox within w_gl_txn_report
end type
type cbx_on_hold from checkbox within w_gl_txn_report
end type
type gb_1 from groupbox within w_gl_txn_report
end type
end forward

global type w_gl_txn_report from w_a_report
integer y = 49
integer height = 2761
string title = "Claim Cash Receipt Transactions"
dw_search dw_search
cb_retrieve cb_retrieve
cbx_unapplied_txns cbx_unapplied_txns
cbx_on_hold cbx_on_hold
gb_1 gb_1
end type
global w_gl_txn_report w_gl_txn_report

type variables
STRING			is_original_sql
LONG				il_group_by_pos
end variables

forward prototypes
public subroutine wf_process_report_criteria (string as_search_type, ref string as_message, ref string as_additional_where)
end prototypes

public subroutine wf_process_report_criteria (string as_search_type, ref string as_message, ref string as_additional_where);LONG							ll_claim_no, ll_batch_no, ll_coc_period
STRING						ls_coc_year, ls_coc_month
INTEGER						li_rows, li_find, li_coc_month
DATETIME					ldt_processed_date
DATAWINDOWCHILD		ldwc_processed_date

CHOOSE CASE as_search_type
	CASE "C" //claim_no
		ll_claim_no = dw_search.GetItemNumber(1,"claim_no")
		If ll_claim_no = 0 or IsNull(ll_claim_no) Then
			as_message = 'Claim number'
			RETURN
		End if
		
		as_additional_where = " and b.claim_no = " + String(ll_claim_no)
		dw_report.Object.t_parameters.Text = 'claim number = ' + String(ll_claim_no)
	CASE "B" //batch_no
		ll_batch_no = dw_search.GetItemNumber(1,"batch_no")
		IF ll_batch_no = 0 or IsNull(ll_batch_no) Then
			as_message = 'Processed date'
			RETURN
		End if
		
		dw_search.GetChild('batch_no',ldwc_processed_date)
		
		li_rows = ldwc_processed_date.Rowcount()
		
		li_find = ldwc_processed_date.Find('batch_no = ' + String(ll_batch_no), 1, li_rows ) 
		
		ldt_processed_date = ldwc_processed_date.GetItemDateTime(li_find,"processed_date")
		
		as_additional_where = " and b.batch_no = " + String(ll_batch_no)
		dw_report.Object.t_parameters.Text = 'processed date = ' + String(ldt_processed_date,'yyyy-mm-dd hh:mm')
	CASE "P" //coc_period
		ll_coc_period = dw_search.GetItemNumber(1,"coc_period")
		IF ll_coc_period = 0 or IsNull(ll_coc_period) Then
			as_message = 'Coc period'
			RETURN
		End if
		as_additional_where = " and b.coc_period = " + String(ll_coc_period)
		ls_coc_year = String (Integer(ll_coc_period/100))
		
		li_coc_month = Mod(ll_coc_period,100)
		IF li_coc_month < 10 THEN
			ls_coc_month = '0' + String (li_coc_month)
		ELSE
			ls_coc_month = String (li_coc_month)
		END IF
		
		dw_report.Object.t_parameters.Text = 'COC period = ' + ls_coc_year + '-' + ls_coc_month
END CHOOSE

end subroutine

event open;call super::open;DatawindowChild		ldw_coc_period
DatawindowChild		ldw_batch_no

LONG		ll_row
LONG		ll_batch_no
INTEGER	li_rtn


ll_row = dw_search.InsertRow(0)
If ll_row <= 0 Then SignalError(-666,'Error inserting row for search criteria')

dw_search.GetChild("coc_period",ldw_coc_period)
ldw_coc_period.SetTransObject(SQLCA)
li_rtn = ldw_coc_period.Retrieve()
SQLCA.nf_handle_error('w_gl_txn_report','open','ldw_coc_period.Retrieve')
IF li_rtn <= 0 THEN SignalError(-666,'Error retrieving coc period dddw')

dw_search.GetChild("batch_no",ldw_batch_no)
ldw_batch_no.SetTransObject(SQLCA)
li_rtn = ldw_batch_no.Retrieve()
SQLCA.nf_handle_error('w_gl_txn_report','open','ldw_coc_period.Retrieve')
IF li_rtn <= 0 THEN SignalError(-666,'Error retrieving batch_no dddw')

//Default the search criteria to processed date
dw_search.SetItem(1,"search_type",'B')

//Get the latest processed date from the dddw and default to that value
ll_batch_no = ldw_batch_no.GetItemNumber(1,"batch_no")
If ll_batch_no = 0 or IsNull(ll_batch_no) Then
	SignalError(-666,'Error getting most recent batch number for search criteria.')
End if

If dw_search.SetItem(1,"batch_no",ll_batch_no) < 0 THen SignalError(-666,'Error batch_no setitem.')

dw_report.SetTransObject(SQLCA)

//Get the original SQL
is_original_sql = dw_report.GetSqlSelect()
il_group_by_pos = Pos(Upper(is_original_sql),'GROUP BY')

IF il_group_by_pos = 0 Then
	SignalError(-666,'Remove the code chequing for a group by if one is not used anymore')
End if
end event

on w_gl_txn_report.create
int iCurrent
call super::create
this.dw_search=create dw_search
this.cb_retrieve=create cb_retrieve
this.cbx_unapplied_txns=create cbx_unapplied_txns
this.cbx_on_hold=create cbx_on_hold
this.gb_1=create gb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_search
this.Control[iCurrent+2]=this.cb_retrieve
this.Control[iCurrent+3]=this.cbx_unapplied_txns
this.Control[iCurrent+4]=this.cbx_on_hold
this.Control[iCurrent+5]=this.gb_1
end on

on w_gl_txn_report.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.dw_search)
destroy(this.cb_retrieve)
destroy(this.cbx_unapplied_txns)
destroy(this.cbx_on_hold)
destroy(this.gb_1)
end on

type dw_report from w_a_report`dw_report within w_gl_txn_report
integer x = 0
integer y = 392
integer width = 2711
integer height = 2168
string title = "Gl Transactions"
string dataobject = "d_gl_txn_report"
boolean hscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type dw_search from u_dw_online within w_gl_txn_report
integer x = 64
integer y = 84
integer width = 1330
integer height = 280
integer taborder = 20
boolean bringtotop = true
string title = "Gl Transactions"
string dataobject = "d_gl_txn_search_criteria"
boolean border = false
end type

event itemchanged;call super::itemchanged;LONG		ll_null

setNull(ll_null)


IF dwo.name = "search_type" Then
	
	CHOOSE CASE data
		CASE 'B'
			IF cbx_unapplied_txns.Checked = TRUE THEN
				RETURN 2
			END IF
			dw_search.SetItem(1,"coc_period",ll_Null)
			dw_search.SetITem(1,"claim_no",0)
		CASE 'P'
			dw_search.SetITem(1,"claim_no",0)
			dw_search.SetITem(1,"batch_no",ll_Null)
		CASE 'C'
			dw_search.SetITem(1,"batch_no",ll_Null)
			dw_search.SetITem(1,"coc_period",ll_Null)
	END CHOOSE
END IF
end event

type cb_retrieve from commandbutton within w_gl_txn_report
integer x = 2318
integer y = 260
integer width = 343
integer height = 104
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Retrieve"
boolean default = true
end type

event clicked;STRING		ls_search_type, ls_message
STRING		ls_additional_where
STRING		ls_new_sql
INTEGER		li_rtn, li_pos

dw_search.AcceptText()

// reset parameters
dw_report.Object.t_parameters.Text = ''

ls_search_type = dw_search.GetItemString(1,"search_type")
IF cbx_on_hold.Checked = TRUE THEN
	// when 'on hold' checkbox is clicked, the dataobject is switched to unapplied, there 'NOT EXISTS' will always be available to be altered below
	li_pos = Pos(is_original_sql, 'NOT EXISTS')
	ls_new_sql = Replace(is_original_sql,li_pos,4,'    ')
	
	dw_report.Object.t_parameters.Text = 'on hold'
ELSE
	IF cbx_unapplied_txns.Checked = TRUE THEN
		li_pos = Pos(is_original_sql, '    EXISTS')
		// its possible that 'on hold' checkbox has never been clicked, so 'NOT EXISTS' may not have been altered & therefore li_pos = 0
		IF li_pos > 0 THEN 
			ls_new_sql = Replace(is_original_sql,li_pos,4,'NOT ')
		ELSE
			ls_new_sql = is_original_sql
		END IF
		
		wf_process_report_criteria(ls_search_type,ls_message,ls_additional_where)
		
	ELSE
		// applied txns only
		wf_process_report_criteria(ls_search_type,ls_message,ls_additional_where)
		IF ls_message <> '' THEN
			MessageBox(ls_message, ls_message + 'must be supplied' )
			RETURN
		END IF
		
	END IF
	
	//il_group_by_pos is the position of the group by clause in the original sql select
	//the replace function will replace the space before the group by with the 
	//additional where statement
	
END IF

IF ls_additional_where <> '' THEN
	ls_new_sql = Replace(is_original_sql,il_group_by_pos -1,1, ls_additional_where + ' ')
END IF

IF dw_report.SetSqlSelect(ls_new_sql) = -1 THEN SignalError(-666,'Error setting new select')

li_rtn =  dw_report.Retrieve()
SQLCA.nf_handle_error("w_gl_txn_report",'cb_retrieve.clicked','Error retrieving report')

IF li_rtn = -1 Then 
	SignalError(-666,'Error Rerieving report')
Elseif li_rtn = 0 Then
	MessageBox('No rows','No records matched the search criteria.  Try again.')
End if
end event

type cbx_unapplied_txns from checkbox within w_gl_txn_report
integer x = 1559
integer y = 80
integer width = 608
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Unprocessed Txns"
end type

event clicked;LONG		ll_null

setNull(ll_null)

IF cbx_unapplied_txns.Checked = TRUE THEN
	dw_report.DataObject = 'd_gl_txn_report_unapplied'
	dw_report.SetTransObject(SQLCA)
	
	IF dw_search.GetItemString(1,'search_type') = 'B' THEN
		 dw_search.SetItem(1,'search_type', 'C')
	END IF
	dw_search.Modify("batch_no.Protect='1'")
	dw_search.Modify("batch_no.Background.Mode='1'") // transparent
	dw_search.SetItem(1,"batch_no",ll_Null)
ELSE
	dw_report.DataObject = 'd_gl_txn_report'
	dw_report.SetTransObject(SQLCA)
	
	dw_search.Modify("batch_no.Protect='0'")
	dw_search.Modify("batch_no.Background.Mode='0'") // opaque	
END IF

//Get the original SQL
is_original_sql = dw_report.GetSqlSelect()
il_group_by_pos = Pos(Upper(is_original_sql),'GROUP BY')

IF il_group_by_pos = 0 Then
	SignalError(-666,'Remove the code chequing for a group by if one is not used anymore')
End if
end event

type cbx_on_hold from checkbox within w_gl_txn_report
integer x = 1559
integer y = 200
integer width = 343
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "On Hold"
end type

event clicked;LONG		ll_null

setNull(ll_null)

IF cbx_on_hold.Checked = TRUE THEN
	dw_report.DataObject = 'd_gl_txn_report_unapplied'
	dw_report.SetTransObject(SQLCA)
	
	cbx_unapplied_txns.Checked = TRUE
	cbx_unapplied_txns.TriggerEvent(Clicked!) // clears & disables proc date parameter, sets original SQL variable
	
	dw_search.Modify("claim_no.Protect='1'")
	dw_search.Modify("claim_no.Background.Mode='1'") // transparent
	dw_search.SetItem(1,"claim_no",ll_Null) // clears & disables claim number parameter
	
	dw_search.Modify("coc_period.Protect='1'")
	dw_search.Modify("coc_period.Background.Mode='1'") // transparent
	dw_search.SetItem(1,"coc_period",ll_Null) //clears & disables coc period parameter
	
	cbx_unapplied_txns.Enabled = FALSE
	dw_search.Enabled = FALSE
	
ELSE
	// since 'unprocessed' checkbox is still checked, proc data parameter is not enabled
	
	dw_search.Modify("claim_no.Protect='0'")
	dw_search.Modify("claim_no.Background.Mode='0'") // opaque, enables claim number parameter
	
	dw_search.Modify("coc_period.Protect='0'")
	dw_search.Modify("coc_period.Background.Mode='0'") // opaque, enables coc period parameter
	
	cbx_unapplied_txns.Enabled = TRUE
	dw_search.Enabled = TRUE
	
END IF

end event

type gb_1 from groupbox within w_gl_txn_report
integer x = 23
integer width = 2267
integer height = 380
integer taborder = 10
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Search by:"
end type

