$PBExportHeader$w_aging_profile.srw
forward
global type w_aging_profile from w_a_report
end type
type cb_ok from commandbutton within w_aging_profile
end type
end forward

global type w_aging_profile from w_a_report
cb_ok cb_ok
end type
global w_aging_profile w_aging_profile

on open;call w_a_report::open;
	dw_report.SetTransobject (SQLCA)


end on

on w_aging_profile.create
int iCurrent
call w_a_report::create
this.cb_ok=create cb_ok
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=cb_ok
end on

on w_aging_profile.destroy
call w_a_report::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
end on

type dw_report from w_a_report`dw_report within w_aging_profile
string DataObject="d_aging_profile"
boolean HScrollBar=true
boolean LiveScroll=false
end type

type cb_ok from commandbutton within w_aging_profile
int X=1605
int Y=241
int Width=439
int Height=109
int TabOrder=10
boolean BringToTop=true
string Text="O&K"
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

on clicked;LONG 		ll_numrows
/*	Retrieve the report.
*/
	ll_numrows = dw_report.Retrieve()
	SQLCA.nf_handle_error("w_aging_profile","dw_report","cb_ok")
	IF ll_numrows <= 0 THEN
		MessageBox("Aging Profile","No data found.")
	END IF

end on

