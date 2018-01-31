$PBExportHeader$w_t5_trustees.srw
$PBExportComments$Window to view and change recipient on T5 for payments to trustees.
forward
global type w_t5_trustees from w_a_report
end type
type dw_t5007_trustee_history from u_dw_online within w_t5_trustees
end type
type dw_trustees from u_dw_online within w_t5_trustees
end type
type vsb_year from vscrollbar within w_t5_trustees
end type
type cb_retrieve from commandbutton within w_t5_trustees
end type
type em_taxation_year from editmask within w_t5_trustees
end type
type st_transmission_date from statictext within w_t5_trustees
end type
type cb_print_details from commandbutton within w_t5_trustees
end type
type cb_save from commandbutton within w_t5_trustees
end type
type cb_close from commandbutton within w_t5_trustees
end type
type dw_t5_details from u_dw_online within w_t5_trustees
end type
end forward

global type w_t5_trustees from w_a_report
string title = "T5 Trustee Assignment"
event ue_open ( )
dw_t5007_trustee_history dw_t5007_trustee_history
dw_trustees dw_trustees
vsb_year vsb_year
cb_retrieve cb_retrieve
em_taxation_year em_taxation_year
st_transmission_date st_transmission_date
cb_print_details cb_print_details
cb_save cb_save
cb_close cb_close
dw_t5_details dw_t5_details
end type
global w_t5_trustees w_t5_trustees

type variables
datetime	idtm_start_date, idtm_end_date
datawindowchild idwc_child

end variables

event ue_open;datetime ldt_server_datetime

SetPointer(HourGlass!)

// Set database information
dw_report.SetTransObject(SQLCA)
dw_trustees.SetTransObject(SQLCA)
dw_t5_details.SetTransObject(SQLCA)
dw_t5007_trustee_history.SetTransObject(SQLCA)

// Set the taxation year on the edit mask
ldt_server_datetime = f_server_datetime()
IF Month(Date(ldt_server_datetime)) <= 3 THEN
	em_taxation_year.Text = String(Long(String(ldt_server_datetime, "yyyy"))-1,"####")
ELSE
	em_taxation_year.Text = String(ldt_server_datetime, "yyyy") 
END IF
em_taxation_year.SetFocus()

IF dw_report.GetChild('claim_participant', idwc_child) < 0 THEN
	MessageBox('Error', 'Not a datawindow child')
ELSE
	idwc_child.SetTransObject(SQLCA)
	idwc_child.InsertRow(0)
END IF
end event

on w_t5_trustees.create
int iCurrent
call super::create
this.dw_t5007_trustee_history=create dw_t5007_trustee_history
this.dw_trustees=create dw_trustees
this.vsb_year=create vsb_year
this.cb_retrieve=create cb_retrieve
this.em_taxation_year=create em_taxation_year
this.st_transmission_date=create st_transmission_date
this.cb_print_details=create cb_print_details
this.cb_save=create cb_save
this.cb_close=create cb_close
this.dw_t5_details=create dw_t5_details
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_t5007_trustee_history
this.Control[iCurrent+2]=this.dw_trustees
this.Control[iCurrent+3]=this.vsb_year
this.Control[iCurrent+4]=this.cb_retrieve
this.Control[iCurrent+5]=this.em_taxation_year
this.Control[iCurrent+6]=this.st_transmission_date
this.Control[iCurrent+7]=this.cb_print_details
this.Control[iCurrent+8]=this.cb_save
this.Control[iCurrent+9]=this.cb_close
this.Control[iCurrent+10]=this.dw_t5_details
end on

on w_t5_trustees.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.dw_t5007_trustee_history)
destroy(this.dw_trustees)
destroy(this.vsb_year)
destroy(this.cb_retrieve)
destroy(this.em_taxation_year)
destroy(this.st_transmission_date)
destroy(this.cb_print_details)
destroy(this.cb_save)
destroy(this.cb_close)
destroy(this.dw_t5_details)
end on

event open;Long    ll_num_rows, ll_row
Integer li_rtn

SetPointer(HourGlass!)

// Call this event so window opens quicker
PostEvent("ue_open")

end event

event closequery;call super::closequery;IF dw_report.ModifiedCount() > 0 THEN
	IF MessageBox('Warning', 'Data not saved.  Close window anyway?', Question!, YesNo!) = 2 THEN
		Message.ReturnValue = 1
	END IF
END IF

end event

type dw_report from w_a_report`dw_report within w_t5_trustees
integer x = 0
integer y = 164
integer width = 2725
integer height = 1204
string dataobject = "d_t5_trustees"
boolean hscrollbar = true
boolean hsplitscroll = true
borderstyle borderstyle = stylelowered!
end type

event dw_report::clicked;call super::clicked;This.ScrollToRow(row)
end event

event dw_report::constructor;call super::constructor;This.uf_setselect(1)
end event

event dw_report::retrievestart;call super::retrievestart;// Don't reset each time
Return 2
end event

event dw_report::rowfocuschanged;call super::rowfocuschanged;Long ll_recipient_no, ll_claim_no, ll_claim_participant, ll_row

IF This.GetRow() = 0 THEN
	RETURN
END IF

This.SelectRow(0, FALSE)
This.SelectRow(This.GetRow(), TRUE)

IF This.RowCount() > 0 THEN
	ll_recipient_no = This.GetItemNumber(This.GetRow(), 'recipient_no')
	ll_claim_no = This.GetItemNumber(This.GetRow(), 'claim_no')
	idwc_child.Retrieve(ll_claim_no)
	IF dw_t5_details.Retrieve(ll_recipient_no, ll_claim_no, idtm_start_date, idtm_end_date) < 0 THEN
		MessageBox('Error', 'Problem retrieving T5 Details')
		Return -1
	END IF
	
	IF SQLCA.nf_handle_error("w_t5_trustees","dw_report","rowfocuschanged - dw_t5_details.Retrieve(ll_recipient_no, ll_claim_no, idtm_start_date, idtm_end_date)") < 0 THEN
		Return -1
	END IF
END IF

end event

type dw_t5007_trustee_history from u_dw_online within w_t5_trustees
boolean visible = false
integer x = 151
integer y = 2404
integer width = 78
integer height = 128
integer taborder = 90
string dataobject = "d_t5007_trustee_history"
borderstyle borderstyle = stylelowered!
end type

type dw_trustees from u_dw_online within w_t5_trustees
boolean visible = false
integer x = 55
integer y = 2404
integer width = 78
integer height = 128
integer taborder = 40
string dataobject = "d_trustees"
borderstyle borderstyle = stylelowered!
end type

type vsb_year from vscrollbar within w_t5_trustees
integer x = 722
integer y = 32
integer width = 73
integer height = 96
end type

event linedown;datetime ldtm_server_date

ldtm_server_date = f_server_datetime()

IF Long(em_taxation_year.Text) < Year(Date(ldtm_server_date)) THEN
	em_taxation_year.Text = String(Long(em_taxation_year.Text) + 1)
END IF
end event

event lineup;IF Long(em_taxation_year.Text) > 1999 THEN
	em_taxation_year.Text = String(Long(em_taxation_year.Text) - 1)
END IF
end event

type cb_retrieve from commandbutton within w_t5_trustees
integer x = 325
integer y = 2412
integer width = 581
integer height = 108
integer taborder = 80
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Retrieve "
end type

event clicked;Long ll_taxation_year, ll_row, ll_rows, ll_recipient_no, ll_claim_no, ll_claim_participant
DateTime ldt_server_date, ldt_server_datetime

SetPointer(HourGlass!)

ldt_server_datetime = f_server_datetime()
ldt_server_date = Datetime(Date(ldt_server_datetime), Time("00:00:00"))

// Get Taxation Year
ll_taxation_year = Long(em_taxation_year.Text)
IF ll_taxation_year < 1999 OR ll_taxation_year > Year(Date(ldt_server_date)) THEN
	em_taxation_year.SetFocus()
	MessageBox('Error', 'Taxation year does not pass validation.  It must be >= 1999 and <= the current year.')
	RETURN -1
END IF
if ll_taxation_year > 2000 then
	
	dw_report.dataobject = 'd_t5_trustees_after2000'
	dw_report.settransobject(sqlca)
	dw_t5_details.dataobject = 'd_t5_details_claim_after2000'
	dw_t5_details.settransobject(sqlca)
	IF dw_report.GetChild('claim_participant', idwc_child) < 0 THEN
		MessageBox('Error', 'Not a datawindow child')
	ELSE
		idwc_child.SetTransObject(SQLCA)
		idwc_child.InsertRow(0)
	END IF
else
	dw_report.dataobject = 'd_t5_trustees'
	dw_report.settransobject(sqlca)
	dw_t5_details.dataobject = 'd_t5_details_claim'
	dw_t5_details.settransobject(sqlca)
	IF dw_report.GetChild('claim_participant', idwc_child) < 0 THEN
		MessageBox('Error', 'Not a datawindow child')
	ELSE
		idwc_child.SetTransObject(SQLCA)
		idwc_child.InsertRow(0)
	END IF
end if
idtm_start_date = DateTime(Date(String(ll_taxation_year) + '/01/01'))
idtm_end_date = DateTime(Date(String(ll_taxation_year + 1) + '/01/01'))

dw_trustees.Retrieve()
IF SQLCA.nf_handle_error('w_t5_trustees', '', 'cb_retrieve - dw_trustees.Retrieve()') < 0 THEN
	RETURN -1
END IF
	
dw_report.Reset()
ll_rows = dw_trustees.RowCount()
FOR ll_row = 1 TO ll_rows
	ll_recipient_no = dw_trustees.GetItemNumber(ll_row, 'individual_no')
	dw_report.Retrieve(ll_recipient_no, idtm_start_date, idtm_end_date)
	IF SQLCA.nf_handle_error('w_t5_trustees', '', 'cb_retrieve - dw_report.Retrieve(ll_recipient_no, idtm_start_date, idtm_end_date)') < 0 THEN
		RETURN -1
	END IF
NEXT

dw_report.ScrollToRow(1)
dw_report.TriggerEvent(RowFocusChanged!)

ll_rows = 0
SELECT COUNT(*) 
INTO :ll_rows
FROM T5007_HISTORY
WHERE taxation_year = :ll_taxation_year;

IF SQLCA.nf_handle_error('w_t5_trustees', '', 'cb_retrieve - Embedded select from T5007_HISTORY') < 0 THEN
	RETURN -1
END IF

IF ll_rows > 0 THEN
	MessageBox("Warning", "The T5 History file has already been created for taxation year " + String(ll_taxation_year) + '.  Please contact the helpdesk if you need to make changes.')
	cb_save.Enabled = FALSE
ELSE
	cb_save.Enabled = TRUE
END IF
cb_print_details.Enabled = TRUE

end event

type em_taxation_year from editmask within w_t5_trustees
integer x = 512
integer y = 32
integer width = 219
integer height = 96
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "####"
string displaydata = "L("
end type

type st_transmission_date from statictext within w_t5_trustees
integer x = 37
integer y = 36
integer width = 453
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
boolean enabled = false
string text = "Taxation Year:"
boolean focusrectangle = false
end type

type cb_print_details from commandbutton within w_t5_trustees
integer x = 1541
integer y = 2412
integer width = 581
integer height = 108
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Print Report"
end type

event clicked;dw_report.Print()

end event

type cb_save from commandbutton within w_t5_trustees
integer x = 933
integer y = 2412
integer width = 581
integer height = 108
integer taborder = 50
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
end type

event clicked;Long ll_row, ll_rows, ll_row2, ll_taxation_year
String ls_comment, ls_error_message

SetPointer(HourGlass!)
dw_report.AcceptText()



//ll_taxation_year = Long(em_taxation_year.Text)
ll_taxation_year = Year(Date(idtm_start_date))


SQLCA.nf_begin_transaction()

DELETE FROM T5007_TRUSTEE_HISTORY 
WHERE taxation_year = :ll_taxation_year
USING SQLCA;

IF SQLCA.nf_handle_error('w_t5_trustees', '', 'cb_save - DELETE FROM T5007_TRUSTEE_HISTORY') < 0 THEN
	RETURN -1
END IF

ll_rows = dw_report.RowCount()
dw_t5007_trustee_history.Reset()
FOR ll_row = 1 TO ll_rows
	
	/* begin Pr 6243: 2006-12-15  
	add this conditional check;  verify that benefit amount, individual #, 
	and recipient # is > 0 before adding a row to DW. 
	This will match the DB constraint (ck_T5007_TRUSTEE_HISTORY_02) placed on this table and 
	prevent the Datawindow SQL error dialog from coming up and prevent benefits amounts of $0.00 from being put in DB  (R.S.) */
	IF dw_report.GetItemNumber(ll_row, 'benefit_amount') <= 0 Or dw_report.GetItemNumber(ll_row, 'claim_participant') <= 0 OR dw_report.GetItemNumber(ll_row, 'recipient_no') <= 0  Then
		IF len(ls_error_message) = 0  then ls_error_message = "The following row(s) will not be inserted into the history table because either the benefit amount, ~nor the participant # or the claimant # is not a positive value: "
		ls_error_message += "~n"+ STRING(dw_report.GetItemNumber(ll_row, 'recipient_no')) + "  " + dw_report.GetItemString(ll_row, 'name')
		Continue
	END IF
	/* end pr*/		
	ll_row2 = dw_t5007_trustee_history.InsertRow(0)
	dw_t5007_trustee_history.SetItem(ll_row2, 'taxation_year', ll_taxation_year)
	dw_t5007_trustee_history.SetItem(ll_row2, 'trustee_individual_no', dw_report.GetItemNumber(ll_row, 'recipient_no'))
	dw_t5007_trustee_history.SetItem(ll_row2, 't5007_recipient_no', dw_report.GetItemNumber(ll_row, 'claim_participant'))
	dw_t5007_trustee_history.SetItem(ll_row2, 'total_benefit_amount', dw_report.GetItemNumber(ll_row, 'benefit_amount'))
	dw_t5007_trustee_history.SetItem(ll_row2, 'claim_no', dw_report.GetItemNumber(ll_row, 'claim_no'))
	ls_comment = dw_report.GetItemString(ll_row, 'comment')
	IF IsNull(ls_comment) THEN
		ls_comment = ''
	END IF
	dw_t5007_trustee_history.SetItem(ll_row2, 'comment', ls_comment)
	dw_t5007_trustee_history.SetItemStatus(ll_row2, 0, Primary!, NewModified!)
	
NEXT
/* part of PR 6243 above...display message if present*/
IF len(ls_error_message) >0 then
	MessageBox( "History Table update", ls_error_message)
END IF

dw_t5007_trustee_history.Update()
IF SQLCA.nf_handle_error('w_t5_trustees', '', 'cb_save - dw_t5007_trustee_history.Update()') < 0 THEN
	RETURN -1
END IF

dw_report.ResetUpdate()

SQLCA.nf_commit_transaction()

end event

type cb_close from commandbutton within w_t5_trustees
integer x = 2149
integer y = 2412
integer width = 434
integer height = 108
integer taborder = 70
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;Close(Parent)
end event

type dw_t5_details from u_dw_online within w_t5_trustees
integer y = 1432
integer width = 2725
integer height = 900
integer taborder = 30
string dataobject = "d_t5_details_claim"
boolean hscrollbar = true
boolean vscrollbar = true
end type

