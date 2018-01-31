$PBExportHeader$w_survivor_payment_utd.srw
$PBExportComments$Special Survivor Payments that have been paid to date.
forward
global type w_survivor_payment_utd from w_a_report
end type
type cb_print_details from commandbutton within w_survivor_payment_utd
end type
type cb_close from commandbutton within w_survivor_payment_utd
end type
type cb_retrieve from commandbutton within w_survivor_payment_utd
end type
end forward

global type w_survivor_payment_utd from w_a_report
cb_print_details cb_print_details
cb_close cb_close
cb_retrieve cb_retrieve
end type
global w_survivor_payment_utd w_survivor_payment_utd

on w_survivor_payment_utd.create
int iCurrent
call super::create
this.cb_print_details=create cb_print_details
this.cb_close=create cb_close
this.cb_retrieve=create cb_retrieve
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_print_details
this.Control[iCurrent+2]=this.cb_close
this.Control[iCurrent+3]=this.cb_retrieve
end on

on w_survivor_payment_utd.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_print_details)
destroy(this.cb_close)
destroy(this.cb_retrieve)
end on

event open;call super::open;
dw_report.SetTransobject (SQLCA)

end event

type dw_report from w_a_report`dw_report within w_survivor_payment_utd
int X=37
int Y=32
int Height=2284
int TabOrder=10
string DataObject="d_survivors_ptd"
boolean HScrollBar=true
end type

type cb_print_details from commandbutton within w_survivor_payment_utd
int X=1719
int Y=2380
int Width=434
int Height=108
int TabOrder=20
boolean Enabled=false
string Text="&Print Report"
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;Parent.TriggerEvent('ue_print')

end event

type cb_close from commandbutton within w_survivor_payment_utd
int X=2226
int Y=2380
int Width=434
int Height=108
int TabOrder=30
string Text="&Close"
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;Close(Parent)
end event

type cb_retrieve from commandbutton within w_survivor_payment_utd
int X=1211
int Y=2380
int Width=434
int Height=108
int TabOrder=30
boolean BringToTop=true
string Text="&Retrieve"
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontCharSet FontCharSet=Ansi!
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;LONG 		ll_numrows
/*	Retrieve the report.
*/
	ll_numrows = dw_report.Retrieve()
	SQLCA.nf_handle_error("w_survivor_payment_utd","dw_report","cb_retrieve")
	IF ll_numrows <= 0 THEN
		MessageBox("Special Survivor Payments","No data found.")
	ELSE 
		cb_print_details.Enabled = True
	END IF

end event

