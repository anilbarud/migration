$PBExportHeader$w_claim_status_change_error_report.srw
$PBExportComments$Listing of Errors From The Automated Claim Status Change run
forward
global type w_claim_status_change_error_report from w_a_report
end type
type dw_claim_managers from u_dw_online within w_claim_status_change_error_report
end type
type st_1 from statictext within w_claim_status_change_error_report
end type
type cb_ok from commandbutton within w_claim_status_change_error_report
end type
end forward

global type w_claim_status_change_error_report from w_a_report
boolean TitleBar=true
string Title="Claim Status Change Error Report"
dw_claim_managers dw_claim_managers
st_1 st_1
cb_ok cb_ok
end type
global w_claim_status_change_error_report w_claim_status_change_error_report

type variables

end variables

event open;call super::open;DATAWINDOWCHILD ldwc_child

dw_report.SetTransObject (SQLCA)

dw_report.Retrieve('')

IF dw_claim_managers.GetChild('claim_manager', ldwc_child) < 0 THEN
	MessageBox('Error', 'claim_manager is not a child datawindow')
	Return 0
END IF

ldwc_child.SetTransObject(SQLCA)
ldwc_child.Retrieve()
ldwc_child.InsertRow(1)
ldwc_child.SetItem(1,'user_id', '')
ldwc_child.SelectRow(0, FALSE)

end event

on w_claim_status_change_error_report.create
int iCurrent
call super::create
this.dw_claim_managers=create dw_claim_managers
this.st_1=create st_1
this.cb_ok=create cb_ok
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_claim_managers
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.cb_ok
end on

on w_claim_status_change_error_report.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.dw_claim_managers)
destroy(this.st_1)
destroy(this.cb_ok)
end on

type dw_report from w_a_report`dw_report within w_claim_status_change_error_report
int Y=460
int Height=2020
string DataObject="d_claim_status_change_error_report"
boolean HScrollBar=true
end type

type dw_claim_managers from u_dw_online within w_claim_status_change_error_report
int X=553
int Y=64
int Width=832
int Height=92
int TabOrder=10
boolean BringToTop=true
string DataObject="d_claim_managers"
BorderStyle BorderStyle=StyleLowered!
boolean LiveScroll=true
end type

type st_1 from statictext within w_claim_status_change_error_report
int X=110
int Y=72
int Width=416
int Height=76
boolean Enabled=false
boolean BringToTop=true
string Text="Claim Manager:"
boolean FocusRectangle=false
long TextColor=33554432
long BackColor=67108864
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontCharSet FontCharSet=Ansi!
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

type cb_ok from commandbutton within w_claim_status_change_error_report
int X=2153
int Y=216
int Width=389
int Height=108
int TabOrder=20
boolean BringToTop=true
string Text="OK"
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontCharSet FontCharSet=Ansi!
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;IF dw_claim_managers.GetItemString(1, 'claim_manager') = '' THEN
	dw_report.Retrieve('')
ELSE
	IF dw_report.Retrieve(dw_claim_managers.GetItemString(1, 'claim_manager')) = 0 THEN
		MessageBox('Claim Status Change Report', 'No records found for specified claim manager')
	END IF
END IF
end event

