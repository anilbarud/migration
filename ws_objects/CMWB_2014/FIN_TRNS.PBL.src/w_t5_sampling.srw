$PBExportHeader$w_t5_sampling.srw
$PBExportComments$Window to view, print and extract summary T5 data.
forward
global type w_t5_sampling from w_a_report
end type
type rb_t5007_history from radiobutton within w_t5_sampling
end type
type rb_applied_claim from radiobutton within w_t5_sampling
end type
type vsb_year from vscrollbar within w_t5_sampling
end type
type cb_retrieve from commandbutton within w_t5_sampling
end type
type st_details from statictext within w_t5_sampling
end type
type em_taxation_year from editmask within w_t5_sampling
end type
type st_transmission_date from statictext within w_t5_sampling
end type
type cb_print_details from commandbutton within w_t5_sampling
end type
type cb_extract from commandbutton within w_t5_sampling
end type
type cb_close from commandbutton within w_t5_sampling
end type
type dw_t5_details_trustee from u_dw_online within w_t5_sampling
end type
type gb_source from groupbox within w_t5_sampling
end type
end forward

global type w_t5_sampling from w_a_report
string title = "T5 Sampling Report"
event ue_open ( )
rb_t5007_history rb_t5007_history
rb_applied_claim rb_applied_claim
vsb_year vsb_year
cb_retrieve cb_retrieve
st_details st_details
em_taxation_year em_taxation_year
st_transmission_date st_transmission_date
cb_print_details cb_print_details
cb_extract cb_extract
cb_close cb_close
dw_t5_details_trustee dw_t5_details_trustee
gb_source gb_source
end type
global w_t5_sampling w_t5_sampling

type variables
datetime	idtm_start_date, idtm_end_date

end variables

event ue_open;datetime ldt_server_datetime

SetPointer(HourGlass!)

// Set database information
dw_report.SetTransObject(SQLCA)
dw_t5_details_trustee.SetTransObject(SQLCA)

// Set the taxation year on the edit mask
ldt_server_datetime = f_server_datetime()
IF Month(Date(ldt_server_datetime)) <= 3 THEN
	em_taxation_year.Text = String(Long(String(ldt_server_datetime, "yyyy"))-1,"####")
ELSE
	em_taxation_year.Text = String(ldt_server_datetime, "yyyy") 
END IF
em_taxation_year.SetFocus()
end event

on w_t5_sampling.create
int iCurrent
call super::create
this.rb_t5007_history=create rb_t5007_history
this.rb_applied_claim=create rb_applied_claim
this.vsb_year=create vsb_year
this.cb_retrieve=create cb_retrieve
this.st_details=create st_details
this.em_taxation_year=create em_taxation_year
this.st_transmission_date=create st_transmission_date
this.cb_print_details=create cb_print_details
this.cb_extract=create cb_extract
this.cb_close=create cb_close
this.dw_t5_details_trustee=create dw_t5_details_trustee
this.gb_source=create gb_source
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.rb_t5007_history
this.Control[iCurrent+2]=this.rb_applied_claim
this.Control[iCurrent+3]=this.vsb_year
this.Control[iCurrent+4]=this.cb_retrieve
this.Control[iCurrent+5]=this.st_details
this.Control[iCurrent+6]=this.em_taxation_year
this.Control[iCurrent+7]=this.st_transmission_date
this.Control[iCurrent+8]=this.cb_print_details
this.Control[iCurrent+9]=this.cb_extract
this.Control[iCurrent+10]=this.cb_close
this.Control[iCurrent+11]=this.dw_t5_details_trustee
this.Control[iCurrent+12]=this.gb_source
end on

on w_t5_sampling.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.rb_t5007_history)
destroy(this.rb_applied_claim)
destroy(this.vsb_year)
destroy(this.cb_retrieve)
destroy(this.st_details)
destroy(this.em_taxation_year)
destroy(this.st_transmission_date)
destroy(this.cb_print_details)
destroy(this.cb_extract)
destroy(this.cb_close)
destroy(this.dw_t5_details_trustee)
destroy(this.gb_source)
end on

event open;Long    ll_num_rows, ll_row
Integer li_rtn

SetPointer(HourGlass!)

// Call this event so window opens quicker
PostEvent("ue_open")

end event

type dw_report from w_a_report`dw_report within w_t5_sampling
integer x = 0
integer y = 164
integer width = 2725
integer height = 792
integer taborder = 30
string dataobject = "d_t5_summary"
borderstyle borderstyle = stylelowered!
end type

event dw_report::rowfocuschanged;call super::rowfocuschanged;Long ll_recipient_no, ll_individual_no, ll_t5007_recipient_no, ll_trustee_individual_no, ll_claim_no, ll_taxation_year
String ls_filter, ls_include_trustees

IF This.GetRow() = 0 THEN
	RETURN
END IF

This.SelectRow(0, FALSE)
This.SelectRow(This.GetRow(), TRUE)

ll_taxation_year = Year(Date(idtm_start_date))

/* PR1556 - SMANZER JAN 2001 -Modified code to use the one stored procedure */

IF rb_applied_claim.checked = true then
	ls_include_trustees = 'N'
ELSE
	ls_include_trustees = 'Y'
END IF

IF This.RowCount() > 0 THEN
	ll_recipient_no = This.GetItemNumber(This.GetRow(), 'recipient_no')
	IF dw_t5_details_trustee.Retrieve(idtm_start_date, idtm_end_date, ll_recipient_no, ls_include_trustees) < 0 THEN
		MessageBox('Error', 'Problem retrieving T5 Details.  ' + SQLCA.SQLErrText)
		Return -1
	END IF
END IF
end event

type rb_t5007_history from radiobutton within w_t5_sampling
integer x = 1458
integer y = 56
integer width = 439
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "T5007 History"
boolean lefttext = true
end type

event clicked;IF This.Checked = TRUE THEN
	dw_report.DataObject = 'd_t5_history_summary'
	dw_report.SetTransObject(SQLCA)
	dw_t5_details_trustee.Reset()
END IF
end event

type rb_applied_claim from radiobutton within w_t5_sampling
integer x = 901
integer y = 56
integer width = 544
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Applied Claim Txn"
boolean checked = true
boolean lefttext = true
end type

event clicked;IF This.Checked = TRUE THEN
	dw_report.DataObject = 'd_t5_summary'
	dw_report.SetTransObject(SQLCA)
	dw_t5_details_trustee.Reset()
END IF
end event

type vsb_year from vscrollbar within w_t5_sampling
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

type cb_retrieve from commandbutton within w_t5_sampling
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

event clicked;Long ll_taxation_year
DateTime ldt_server_date, ldt_server_datetime
String ls_history_flag

SetPointer(HourGlass!)

ldt_server_datetime = f_server_datetime()
ldt_server_date = Datetime(Date(ldt_server_datetime), Time("00:00:00"))

// Pr1556 Dec 1,2000
ls_history_flag = 'N'

// Get Taxation Year
ll_taxation_year = Long(em_taxation_year.Text)
IF ll_taxation_year < 1999 OR ll_taxation_year > Year(Date(ldt_server_date)) THEN
	em_taxation_year.SetFocus()
	MessageBox('Error', 'Taxation year does not pass validation.  It must be >= 1999 and <= the current year.')
	RETURN -1
END IF

idtm_start_date = DateTime(Date(String(ll_taxation_year) + '/01/01'))
idtm_end_date = DateTime(Date(String(ll_taxation_year + 1) + '/01/01'))

IF rb_applied_claim.Checked = TRUE THEN
	
	dw_report.Retrieve(idtm_start_date, idtm_end_date, ls_history_flag)
	IF SQLCA.nf_handle_error('w_t5_sampling', '', 'cb_retrieve - dw_report.Retrieve(idtm_start_date, idtm_end_date, ls_history_flag)') < 0 THEN
		RETURN -1
	END IF
ELSE
	dw_report.Retrieve(ll_taxation_year)
	IF SQLCA.nf_handle_error('w_t5_sampling', '', 'cb_retrieve - dw_report.Retrieve(ll_taxation_year)') < 0 THEN
		RETURN -1
	END IF
END IF

cb_extract.Enabled = TRUE
cb_print_details.Enabled = TRUE

end event

type st_details from statictext within w_t5_sampling
integer x = 23
integer y = 968
integer width = 370
integer height = 64
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
boolean enabled = false
string text = "Details:"
boolean focusrectangle = false
end type

type em_taxation_year from editmask within w_t5_sampling
integer x = 512
integer y = 32
integer width = 219
integer height = 96
integer taborder = 20
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

type st_transmission_date from statictext within w_t5_sampling
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

type cb_print_details from commandbutton within w_t5_sampling
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

type cb_extract from commandbutton within w_t5_sampling
integer x = 933
integer y = 2412
integer width = 581
integer height = 108
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Create Data File"
end type

event clicked;IF dw_report.SaveAs("", dBASE3!, TRUE) < 0 THEN
	MessageBox('Error', 'An error occured while saving T5 information')
END IF
end event

type cb_close from commandbutton within w_t5_sampling
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

type dw_t5_details_trustee from u_dw_online within w_t5_sampling
integer y = 1052
integer width = 2725
integer height = 1324
integer taborder = 50
string dataobject = "d_t5_details_trustee"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type gb_source from groupbox within w_t5_sampling
integer x = 878
integer width = 1047
integer height = 148
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Source"
end type

