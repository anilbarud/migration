$PBExportHeader$w_overdue_annual_review.srw
$PBExportComments$Window to produce report of all claims overdue for annual review for a user specified region.
forward
global type w_overdue_annual_review from w_a_report
end type
type cb_ok from commandbutton within w_overdue_annual_review
end type
type dw_select_region from u_dw_online within w_overdue_annual_review
end type
type gb_1 from groupbox within w_overdue_annual_review
end type
end forward

global type w_overdue_annual_review from w_a_report
integer width = 3081
integer height = 2784
string title = "Claims Overdue for Annual Review"
cb_ok cb_ok
dw_select_region dw_select_region
gb_1 gb_1
end type
global w_overdue_annual_review w_overdue_annual_review

type variables
datawindowchild	iw_region_list
end variables

on open;call w_a_report::open;
LONG ll_result

/*	Database Connections 
*/

dw_report.SetTransObject(SQLCA)
dw_select_region.SetTransObject(SQLCA)

dw_select_region.GetChild("admin_region_code",iw_region_list)
iw_region_list.SetTransObject(SQLCA)

ll_result = iw_region_list.Retrieve()
SQLCA.nf_handle_error("w_overdue_12_week_review","dw_report","cb_ok")

/*	Insert a row into the Region List and default it to the user's default region (if there is one)
*/

dw_select_region.InsertRow(0)

If vgst_user_profile.default_admin_region_code <> "" Then
	dw_select_region.SetItem(1,"admin_region_code",vgst_user_profile.default_admin_region_code)
End If


end on

on w_overdue_annual_review.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.dw_select_region=create dw_select_region
this.gb_1=create gb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.dw_select_region
this.Control[iCurrent+3]=this.gb_1
end on

on w_overdue_annual_review.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.dw_select_region)
destroy(this.gb_1)
end on

type dw_report from w_a_report`dw_report within w_overdue_annual_review
integer width = 2981
integer taborder = 40
string dataobject = "d_overdue_annual_review"
boolean hscrollbar = true
end type

type cb_ok from commandbutton within w_overdue_annual_review
integer x = 2153
integer y = 212
integer width = 389
integer height = 108
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;/* Modified March 24, 1999 - Erin Peterson For PB2000 */

/*	Initialization
*/
	LONG		ll_numrows
	STRING	as_admin_region_code

	dw_select_region.AcceptText()

/*	Get the Region and ensure that there is a value. This has to be done, as not all users
	have a default admin region.
*/
	as_admin_region_code = dw_select_region.GetItemString(1,"admin_region_code")
	IF IsNull(as_admin_region_code) or as_admin_region_code <= ' ' THEN
		MessageBox("Missing Region Code","You must select a region code before running this report!",Exclamation!)
		RETURN
	END IF

/*	Retrieve the report.
*/
//	ll_numrows = dw_report.Retrieve(as_admin_region_code,DateTime(Today()))
	ll_numrows = dw_report.Retrieve(as_admin_region_code,DateTime(Date(f_server_datetime())))
	SQLCA.nf_handle_error("w_overdue_annual_review","dw_report","cb_ok")
	IF ll_numrows <= 0 then
		MessageBox("Claims Overdue for Annual Review","No data found to satisfy request")
	END IF

end event

type dw_select_region from u_dw_online within w_overdue_annual_review
integer x = 96
integer y = 144
integer width = 1042
integer height = 96
integer taborder = 20
string dataobject = "d_display_active_admin_regions"
boolean border = false
end type

type gb_1 from groupbox within w_overdue_annual_review
integer x = 50
integer y = 40
integer width = 1147
integer height = 264
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

