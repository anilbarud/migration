$PBExportHeader$w_unassigned_preadj_report.srw
$PBExportComments$Main Window for report of claims at the pre-adjudication stage that only exist in a master folder
forward
global type w_unassigned_preadj_report from w_a_report
end type
type cb_ok from commandbutton within w_unassigned_preadj_report
end type
type dw_select_region from u_dw_online within w_unassigned_preadj_report
end type
type gb_1 from groupbox within w_unassigned_preadj_report
end type
end forward

global type w_unassigned_preadj_report from w_a_report
boolean TitleBar=true
string Title="Pre-Adjudication Claims Only in Master"
cb_ok cb_ok
dw_select_region dw_select_region
gb_1 gb_1
end type
global w_unassigned_preadj_report w_unassigned_preadj_report

type variables
datawindowchild	idwc_region_list
end variables

on open;call w_a_report::open;/* Set up the Drop down data window used to select the work set required.
	This report uses the stored procedure 'Pre_Adj_Claims_in_CLAIM_MASTER', which resides
	on SQLCA (Claim db). 
*/

LONG ll_result

/*	Database Connections 
*/

dw_report.SetTransObject(SQLCA)

dw_select_region.GetChild("admin_region_code",idwc_region_list)
idwc_region_list.SetTransObject(SQLCA)

ll_result = idwc_region_list.Retrieve()
SQLCA.nf_handle_error("w_unassigned_preadj_report","dw_report","cb_ok")

/*	Insert a row into the Region List and default it to the user's default region (if there is one)
*/

dw_select_region.InsertRow(0)

If vgst_user_profile.default_admin_region_code <> "" Then
	dw_select_region.SetItem(1,"admin_region_code",vgst_user_profile.default_admin_region_code)
End If


end on

on w_unassigned_preadj_report.create
int iCurrent
call w_a_report::create
this.cb_ok=create cb_ok
this.dw_select_region=create dw_select_region
this.gb_1=create gb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=cb_ok
this.Control[iCurrent+2]=dw_select_region
this.Control[iCurrent+3]=gb_1
end on

on w_unassigned_preadj_report.destroy
call w_a_report::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.dw_select_region)
destroy(this.gb_1)
end on

type dw_report from w_a_report`dw_report within w_unassigned_preadj_report
int TabOrder=40
string DataObject="d_unassigned_preadj_claims"
boolean Border=false
boolean HScrollBar=true
end type

type cb_ok from commandbutton within w_unassigned_preadj_report
int X=2154
int Y=169
int Width=389
int Height=109
int TabOrder=30
boolean BringToTop=true
string Text="&OK"
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

on clicked;/* This report uses the stored procedure 'Pre_Adj_Claims_in_CLAIM_MASTER', which resides
	on the SQLCA (CLAIM db) database.
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
	ll_numrows = dw_report.Retrieve(as_admin_region_code)
	SQLCA.nf_handle_error("w_unassigned_preadj_report","dw_report","cb_ok")
	IF ll_numrows <= 0 then
		MessageBox("Unassigned Pre-Adjudication Claims","No data found to satisfy request")
	END IF




end on

type dw_select_region from u_dw_online within w_unassigned_preadj_report
int X=97
int Y=145
int Width=1239
int Height=93
int TabOrder=10
string DataObject="d_display_active_admin_regions"
boolean Border=false
end type

type gb_1 from groupbox within w_unassigned_preadj_report
int X=51
int Y=41
int Width=1404
int Height=269
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

