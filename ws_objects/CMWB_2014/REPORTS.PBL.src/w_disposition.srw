$PBExportHeader$w_disposition.srw
$PBExportComments$Disposition by Case Manager Report main window
forward
global type w_disposition from w_a_report
end type
type gb_1 from groupbox within w_disposition
end type
type st_note from statictext within w_disposition
end type
type dw_enter_date_range from u_dw_online within w_disposition
end type
type cb_ok from commandbutton within w_disposition
end type
end forward

global type w_disposition from w_a_report
boolean TitleBar=true
string Title="Claim Disposition Report by Benefit End Date"
gb_1 gb_1
st_note st_note
dw_enter_date_range dw_enter_date_range
cb_ok cb_ok
end type
global w_disposition w_disposition

event open;call super::open;INTEGER li_message

li_message = Message.DoubleParm
IF li_message = 1 THEN			//use "by benefit end date" data object - PR 1458 - Kevin MacLeod
	dw_report.DataObject = "d_disposition"
	st_note.Text = "Note: Report is generated based on Benefit End Date"
	This.Title = "Claim Disposition Report by Benefit End Date"
ELSE 									//use "by disposition entered date" data object - PR 1458 - Kevin MacLeod
	dw_report.DataObject = "d_disposition_by_disposition_date"
	st_note.Text = "Note: Report is generated based on the date the disposition was entered"
	This.Title = "Claim Disposition Report by Disposition Entered Date"
END IF
dw_report.settransobject (SQLCA)

dw_enter_date_range.InsertRow(0)
dw_enter_date_range.SetColumn("from_date")

end event

on w_disposition.create
int iCurrent
call super::create
this.gb_1=create gb_1
this.st_note=create st_note
this.dw_enter_date_range=create dw_enter_date_range
this.cb_ok=create cb_ok
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_1
this.Control[iCurrent+2]=this.st_note
this.Control[iCurrent+3]=this.dw_enter_date_range
this.Control[iCurrent+4]=this.cb_ok
end on

on w_disposition.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.gb_1)
destroy(this.st_note)
destroy(this.dw_enter_date_range)
destroy(this.cb_ok)
end on

type dw_report from w_a_report`dw_report within w_disposition
int Y=508
int Height=1972
int TabOrder=40
string DataObject="d_disposition"
boolean HScrollBar=true
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

on dw_report::rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup
/*	
	create the menu - note: we're overriding the ancestor script as
	we want to add a couple of options
*/
	lm_popup = Create m_dw_online_rmb_popup

	lm_popup.mf_set_datawindow(This)
	lm_popup.m_options.m_filterlist.visible = true
	lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))

	Destroy lm_popup

end on

type gb_1 from groupbox within w_disposition
int X=50
int Y=84
int Width=1714
int Height=344
int TabOrder=10
string Text="Processing Period"
BorderStyle BorderStyle=StyleLowered!
long TextColor=33554432
long BackColor=67108864
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

type st_note from statictext within w_disposition
int X=110
int Y=340
int Width=1591
int Height=72
boolean Enabled=false
boolean BringToTop=true
string Text="Note: Report is generated based on Benefit End Date"
Alignment Alignment=Center!
boolean FocusRectangle=false
long TextColor=33554432
long BackColor=67108864
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

type dw_enter_date_range from u_dw_online within w_disposition
int X=105
int Y=188
int Width=1385
int Height=92
int TabOrder=20
boolean BringToTop=true
string DataObject="d_enter_date_range"
boolean Border=false
boolean LiveScroll=true
end type

type cb_ok from commandbutton within w_disposition
int X=2153
int Y=212
int Width=389
int Height=108
int TabOrder=30
boolean BringToTop=true
string Text="&OK"
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;/*	Initialization
*/
	LONG 		ll_numrows
	DATETIME	adt_from_date, adt_to_date, adt_today

	dw_enter_date_range.AcceptText()

/*	Validate the dates and set the from date to the next day at midnight to ensure that all transactions
	created on the last specified date are included. If this is not done, then any transactions
	created on the last day wont get selected because of the time.
*/
	adt_from_date = dw_enter_date_range.GetItemDateTime(1,"from_date")
	adt_to_date = DateTime(RelativeDate(Date(dw_enter_date_range.GetItemDateTime(1,"to_date")),1))

	IF IsNull(adt_from_date) or &
		IsNull(adt_to_date) THEN
		MessageBox("Validation Error","Both the from and to dates must have a value.",Exclamation!)
		Return
	END IF

	IF adt_from_date < DateTime(Date(1900,01,01)) OR &
		adt_to_date < DateTime(Date(1900,01,01)) THEN
		MessageBox("Validation Error","Dates cannot be earlier than 1900-01-01!",Exclamation!)
		Return
	END IF

	IF adt_from_date >= adt_to_date then
		MessageBox("Validation Error","The to date must be later than the from date",Exclamation!)
		Return
	END IF

	adt_today = f_server_datetime()
	adt_today = DateTime(RelativeDate(Date(adt_today), 1))

	If adt_to_date >= adt_today THEN
		MessageBox("Validation Error","The to date must be earlier than the current date",Exclamation!)
		Return
	END IF
	
/*	Retrieve the report.
*/
	ll_numrows = dw_report.Retrieve(adt_from_date,adt_to_date)
	SQLCA.nf_handle_error("w_disposition","dw_report","cb_ok")
	If ll_numrows <= 0 then
		MessageBox("Disposition Report","No data found to satisfy request")
	End If

end event

