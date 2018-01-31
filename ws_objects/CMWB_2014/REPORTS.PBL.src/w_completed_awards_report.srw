$PBExportHeader$w_completed_awards_report.srw
$PBExportComments$Window to retrieve report of all selected award types within user entered date range.
forward
global type w_completed_awards_report from w_a_report
end type
type cb_ok from commandbutton within w_completed_awards_report
end type
type dw_enter_date_range from u_dw_online within w_completed_awards_report
end type
type gb_select_criteria from groupbox within w_completed_awards_report
end type
type dw_select_region from u_dw_online within w_completed_awards_report
end type
type dw_select_award from u_dw_online within w_completed_awards_report
end type
end forward

global type w_completed_awards_report from w_a_report
integer width = 3255
integer height = 2812
string title = "Scheduled Payments Report"
cb_ok cb_ok
dw_enter_date_range dw_enter_date_range
gb_select_criteria gb_select_criteria
dw_select_region dw_select_region
dw_select_award dw_select_award
end type
global w_completed_awards_report w_completed_awards_report

type variables
DATAWINDOWCHILD	iw_select_region
DATAWINDOWCHILD	iw_select_award

PRIVATE n_resize                 inv_resize
end variables

event open;call super::open;/*	This script controls setting up and defaulting the search criteria
	used to retrieve completed awards.
*/
	LONG			ll_result

/* Populate the list of regions and default to the current user's region.
*/
	dw_select_region.SetTransObject(SQLCA)
	dw_select_region.InsertRow(0)
	ll_result = dw_select_region.SetItem(1,"admin_region_code",vgst_user_profile.default_admin_region_code)

/* Populate the list of Award Types.
*/
	dw_select_award.SetTransobject(SQLCA)
	dw_select_award.InsertRow(0)
	dw_select_award.SetItem(1,"award_type_code","LTD")

/* Set up the dw to accept the date range.
*/
	dw_enter_date_range.InsertRow(0)

/*	Set up the report datawindow.
*/
	dw_report.SetTransObject(SQLCA)





//Setup the windows resizing
if IsNull(inv_resize) Or not IsValid (inv_resize) then
	inv_resize = create n_resize
	inv_resize.of_SetOrigSize (3237, 2580)
end if

// register controls for resizing
inv_resize.of_register(dw_report,'ScaleToRight&Bottom')
inv_resize.of_register(cb_ok,'FixedToRight')

end event

on w_completed_awards_report.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.dw_enter_date_range=create dw_enter_date_range
this.gb_select_criteria=create gb_select_criteria
this.dw_select_region=create dw_select_region
this.dw_select_award=create dw_select_award
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.dw_enter_date_range
this.Control[iCurrent+3]=this.gb_select_criteria
this.Control[iCurrent+4]=this.dw_select_region
this.Control[iCurrent+5]=this.dw_select_award
end on

on w_completed_awards_report.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.dw_enter_date_range)
destroy(this.gb_select_criteria)
destroy(this.dw_select_region)
destroy(this.dw_select_award)
end on

event resize;call super::resize;long ll_workspacewidth,ll_workspaceheight


// Notify the resize service that the window size has changed.
ll_workspacewidth = This.WorkSpaceWidth()
ll_workspaceheight = This.WorkSpaceHeight()

If IsValid (inv_resize) Then
	inv_resize.Event pfc_Resize (sizetype,ll_workspacewidth  , ll_workspaceheight )
End If
end event

type dw_report from w_a_report`dw_report within w_completed_awards_report
integer width = 3127
integer taborder = 50
string dataobject = "d_completed_awards_report"
boolean hscrollbar = true
end type

type cb_ok from commandbutton within w_completed_awards_report
integer x = 2784
integer y = 80
integer width = 389
integer height = 108
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;/*	Variables
*/
	STRING	as_admin_region_code, as_award_type_code
	LONG 		ll_numrows
	DATETIME	adt_from_date, adt_to_date

	SetPointer(HourGlass!)

	dw_select_region.AcceptText()
	dw_select_award.AcceptText()
	dw_enter_date_range.AcceptText()

/*	Acquire the region and award type.
*/
	as_admin_region_code = dw_select_region.GetItemString(1,"admin_region_code")
	as_award_type_code = dw_select_award.GetItemString(1,"award_type_code")

/* Validate date range. To date must be greater than the from date. As well, add one to the to date
	to ensure getting all items created that day, regardless of what time they were created.
*/ 
	adt_from_date = dw_enter_date_range.GetItemDateTime(1,"from_date")
	adt_to_date = dw_enter_date_range.GetItemDateTime(1,"to_date")

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

	IF adt_from_date > DateTime(Date(2079,06,06)) OR &
		adt_to_date > DateTime(Date(2079,06,06)) THEN
		MessageBox("Validation Error","Dates cannot be later than 2079-06-06!",Exclamation!)
		Return
	END IF
	
	IF adt_from_date >= adt_to_date then
		MessageBox("Validation Error","The to date must be later than the from date",Exclamation!)
		Return
	END IF

/*	Retrieve the report. It requires the user id, benefit type and from and to dates for parameters
*/
	ll_numrows = dw_report.Retrieve(as_admin_region_code,as_award_type_code,adt_from_date,adt_to_date)
	SQLCA.nf_handle_error("w_scheduled_payments","dw_report","cb_ok")
	If ll_numrows <= 0 then
		MessageBox("Completed Awards Report","No data found to satisfy request.")
	End If

	SetPointer(Arrow!)

end event

type dw_enter_date_range from u_dw_online within w_completed_awards_report
integer x = 119
integer y = 324
integer width = 1385
integer height = 144
integer taborder = 30
boolean bringtotop = true
string dataobject = "d_enter_date_range"
boolean border = false
end type

type gb_select_criteria from groupbox within w_completed_awards_report
integer x = 50
integer y = 40
integer width = 1518
integer height = 444
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Selection Criteria"
end type

type dw_select_region from u_dw_online within w_completed_awards_report
integer x = 110
integer y = 132
integer width = 1010
integer height = 92
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_display_active_admin_regions"
boolean border = false
end type

type dw_select_award from u_dw_online within w_completed_awards_report
integer x = 105
integer y = 224
integer width = 1175
integer height = 100
integer taborder = 20
boolean bringtotop = true
string dataobject = "d_select_award"
boolean border = false
end type

