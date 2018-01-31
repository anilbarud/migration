$PBExportHeader$w_pay_fin_claim.srw
$PBExportComments$Window to report on Active Claims First Benefit Cheque
forward
global type w_pay_fin_claim from w_a_report
end type
type cb_1 from commandbutton within w_pay_fin_claim
end type
type gb_1 from groupbox within w_pay_fin_claim
end type
type dw_enter_claim_date from u_dw_online within w_pay_fin_claim
end type
end forward

global type w_pay_fin_claim from w_a_report
cb_1 cb_1
gb_1 gb_1
dw_enter_claim_date dw_enter_claim_date
end type
global w_pay_fin_claim w_pay_fin_claim

on w_pay_fin_claim.create
int iCurrent
call w_a_report::create
this.cb_1=create cb_1
this.gb_1=create gb_1
this.dw_enter_claim_date=create dw_enter_claim_date
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=cb_1
this.Control[iCurrent+2]=gb_1
this.Control[iCurrent+3]=dw_enter_claim_date
end on

on w_pay_fin_claim.destroy
call w_a_report::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_1)
destroy(this.gb_1)
destroy(this.dw_enter_claim_date)
end on

event open;call super::open;/*	Database Connections and initialization.
*/
	dw_report.SetTransObject (SQLCA)
	dw_enter_claim_date.InsertRow(0)
	dw_enter_claim_date.SetFocus()
	dw_enter_claim_date.SetColumn("for_date")


end event

type dw_report from w_a_report`dw_report within w_pay_fin_claim
int TabOrder=40
string DataObject="d_pay_fin_claim"
end type

type cb_1 from commandbutton within w_pay_fin_claim
int X=2067
int Y=269
int Width=449
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

event clicked;/*	Initialization
*/
	LONG		ll_numrows
	DATE  	adt_for_date

	dw_enter_claim_date.AcceptText()

/*	Validate the date
*/

	adt_for_date = dw_enter_claim_date.GetItemDate(1,"for_date")

	IF IsNull(adt_for_date) THEN
		MessageBox("Validation Error","The for DATE must have a value.",Exclamation!)
		Return
	END IF

	IF adt_for_date < Date(Date(1900,01,01)) THEN
		MessageBox("Validation Error","Date cannot be earlier than 1900-01-01!",Exclamation!)
		Return
	END IF

	IF adt_for_date > Date(2079,06,06) THEN
		MessageBox("Validation Error","Date cannot be later than 2079-06-06!",Exclamation!)
		Return
	END IF
	
/*	Retrieve the report.
*/

	ll_numrows = dw_report.Retrieve(adt_for_date)
	IF SQLCA.nf_handle_error("w_pay_fin_claim","dw_report","OK - Clicked") < 0 Then
		Return -1
	END IF

	IF ll_numrows = 0 THEN
		MessageBox("Query Results","No data was found for report.")
	END IF

	dw_report.SetFocus()


end event

type gb_1 from groupbox within w_pay_fin_claim
int X=654
int Y=185
int Width=796
int Height=213
int TabOrder=10
string Text="Date "
BorderStyle BorderStyle=StyleLowered!
long TextColor=33554432
long BackColor=79741120
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

type dw_enter_claim_date from u_dw_online within w_pay_fin_claim
int X=764
int Y=265
int Width=517
int Height=101
int TabOrder=20
boolean BringToTop=true
string DataObject="d_enter_claim_date"
boolean Border=false
boolean LiveScroll=true
end type

