$PBExportHeader$w_status_tracking.srw
$PBExportComments$Produces report of all claims whose status has changed within a user specified time frame for a specified region.
forward
global type w_status_tracking from w_a_report
end type
type dw_enter_date_range from u_dw_online within w_status_tracking
end type
type cb_ok from commandbutton within w_status_tracking
end type
type dw_select_region from u_dw_online within w_status_tracking
end type
type gb_1 from groupbox within w_status_tracking
end type
end forward

global type w_status_tracking from w_a_report
boolean TitleBar=true
string Title="Status Tracking Report"
dw_enter_date_range dw_enter_date_range
cb_ok cb_ok
dw_select_region dw_select_region
gb_1 gb_1
end type
global w_status_tracking w_status_tracking

type variables
datawindowchild	iw_region_list
end variables

on open;call w_a_report::open;	LONG ll_result

/*	Database Connections 
*/

	dw_report.SetTransObject(SQLCA)
	dw_select_region.SetTransObject(SQLCA)
	dw_enter_date_range.SetTransObject(SQLCA)

	dw_select_region.GetChild("admin_region_code",iw_region_list)
	iw_region_list.SetTransObject(SQLCA)

	ll_result = iw_region_list.Retrieve()
	SQLCA.nf_handle_error("w_status_tracking","dw_report","cb_ok")

/*	Insert a row into the Region List and default it to the user's default region (if there is one)
*/

	dw_select_region.InsertRow(0)
	
	If vgst_user_profile.default_admin_region_code <> "" Then
		dw_select_region.SetItem(1,"admin_region_code",vgst_user_profile.default_admin_region_code)
	End If

/*	Insert a row for the date range.
*/
	dw_enter_date_range.InsertRow(0)

end on

on w_status_tracking.create
int iCurrent
call w_a_report::create
this.dw_enter_date_range=create dw_enter_date_range
this.cb_ok=create cb_ok
this.dw_select_region=create dw_select_region
this.gb_1=create gb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=dw_enter_date_range
this.Control[iCurrent+2]=cb_ok
this.Control[iCurrent+3]=dw_select_region
this.Control[iCurrent+4]=gb_1
end on

on w_status_tracking.destroy
call w_a_report::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.dw_enter_date_range)
destroy(this.cb_ok)
destroy(this.dw_select_region)
destroy(this.gb_1)
end on

type dw_report from w_a_report`dw_report within w_status_tracking
int TabOrder=50
string DataObject="d_status_tracking"
boolean Border=false
BorderStyle BorderStyle=StyleBox!
boolean HScrollBar=true
end type

type dw_enter_date_range from u_dw_online within w_status_tracking
int X=97
int Y=265
int Width=1386
int Height=93
int TabOrder=30
boolean BringToTop=true
string DataObject="d_enter_date_range"
boolean Border=false
boolean LiveScroll=true
end type

type cb_ok from commandbutton within w_status_tracking
int X=2154
int Y=169
int Width=389
int Height=109
int TabOrder=40
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
	STRING	as_admin_region_code
	DATETIME	adt_start_date, adt_end_date, adt_today

	dw_select_region.AcceptText()
	dw_enter_date_range.AcceptText()

/*	Get the Region
*/
	as_admin_region_code = dw_select_region.GetItemString(1,"admin_region_code")
	IF IsNull(as_admin_region_code) or as_admin_region_code < " " THEN
		MessageBox("Invalid Select Criteria","Region must have a value!",Exclamation!)
		RETURN
	END IF

/*	Get the start and end dates and validate them.
*/
	adt_start_date = dw_enter_date_range.GetItemDateTime(1,"from_date")
	adt_end_date = datetime(relativedate(date(dw_enter_date_range.GetItemDateTime(1,"to_date")),1))
	
	IF IsNull(adt_start_date) OR &
	   IsNull(adt_end_date) THEN
		MessageBox("Validation Error","You must provide both the start and end dates.",Exclamation!)
		Return
	END IF

	IF adt_start_date < DateTime(Date(1900,01,01)) OR &
		adt_end_date < DateTime(Date(1900,01,01)) THEN
		MessageBox("Validation Error","Dates cannot be earlier than 1900-01-01!",Exclamation!)
		Return
	END IF

	IF adt_start_date > adt_end_date THEN
		MessageBox("Validation Error","The from date cannot be after the to date",Exclamation!)
		Return
	END IF

	adt_today = f_server_datetime()
	adt_today = DateTime(RelativeDate(Date(adt_today), 1))

	If adt_end_date >= adt_today THEN
		MessageBox("Validation Error","The to date must be earlier than the current date",Exclamation!)
		Return
	END IF
	
/*	Retrieve the report.
*/
	ll_numrows = dw_report.Retrieve(adt_start_date, adt_end_date, as_admin_region_code)
	SQLCA.nf_handle_error("w_status_tracking","dw_report","cb_ok")
	IF ll_numrows <= 0 then
		MessageBox("Status Tracking Report","No data found to satisfy request.")
	END IF


end event

type dw_select_region from u_dw_online within w_status_tracking
int X=97
int Y=145
int Width=1239
int Height=93
int TabOrder=10
string DataObject="d_display_active_admin_regions"
boolean Border=false
BorderStyle BorderStyle=StyleBox!
end type

type gb_1 from groupbox within w_status_tracking
int X=51
int Y=41
int Width=1518
int Height=353
int TabOrder=20
string Text="Select Criteria"
BorderStyle BorderStyle=StyleLowered!
long TextColor=33554432
long BackColor=67108864
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

