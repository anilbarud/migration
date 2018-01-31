$PBExportHeader$w_case_manage_active.srw
$PBExportComments$Window to Report on CM > 70 or < 50 Active Claims.
forward
global type w_case_manage_active from w_a_report
end type
type cb_1 from commandbutton within w_case_manage_active
end type
end forward

global type w_case_manage_active from w_a_report
cb_1 cb_1
end type
global w_case_manage_active w_case_manage_active

on w_case_manage_active.create
int iCurrent
call w_a_report::create
this.cb_1=create cb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=cb_1
end on

on w_case_manage_active.destroy
call w_a_report::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_1)
end on

event open;call super::open;/*	Database Connections and initialization.
*/
	dw_report.SetTransObject (SQLCA)


end event

type dw_report from w_a_report`dw_report within w_case_manage_active
string DataObject="d_case_manage_active"
end type

type cb_1 from commandbutton within w_case_manage_active
int X=2067
int Y=269
int Width=449
int Height=109
int TabOrder=10
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
	LONG		ll_numrows

/*	Retrieve the report.
*/

	ll_numrows = dw_report.Retrieve()
	IF SQLCA.nf_handle_error("w_case_manage_active","dw_report","OK - Clicked") < 0 Then
		Return -1
	END IF

	IF ll_numrows = 0 THEN
		MessageBox("Query Results","No data was found for report.")
	END IF

	dw_report.SetFocus()


end event

