$PBExportHeader$w_rejected_claims_payments.srw
forward
global type w_rejected_claims_payments from w_a_report
end type
type cb_1 from commandbutton within w_rejected_claims_payments
end type
end forward

global type w_rejected_claims_payments from w_a_report
cb_1 cb_1
end type
global w_rejected_claims_payments w_rejected_claims_payments

on w_rejected_claims_payments.create
int iCurrent
call super::create
this.cb_1=create cb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_1
end on

on w_rejected_claims_payments.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_1)
end on

event open;call super::open;/*	Database Connections and initialization.
*/
	dw_report.SetTransObject (SQLCA)


end event

type dw_report from w_a_report`dw_report within w_rejected_claims_payments
string dataobject = "d_rejected_claims_w_payments"
boolean hscrollbar = true
end type

type cb_1 from commandbutton within w_rejected_claims_payments
integer x = 1824
integer y = 180
integer width = 402
integer height = 104
integer taborder = 10
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "OK"
end type

event clicked;/*	Initialization
*/
	LONG		ll_numrows
	DATE  	adt_for_date

/*	Retrieve the report.
*/

	ll_numrows = dw_report.Retrieve()
	IF SQLCA.nf_handle_error("w_rejected_claims_payment","dw_report","OK - Clicked") < 0 Then
		Return -1
	END IF

	IF ll_numrows = 0 THEN
		MessageBox("Query Results","No data was found for report.")
	END IF

	dw_report.SetFocus()


end event

