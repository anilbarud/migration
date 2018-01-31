$PBExportHeader$w_payment_processing_report.srw
forward
global type w_payment_processing_report from w_a_report
end type
type cb_ok from commandbutton within w_payment_processing_report
end type
type cb_clear from commandbutton within w_payment_processing_report
end type
type dw_enter_batch_no from u_dw_online within w_payment_processing_report
end type
type cb_close from commandbutton within w_payment_processing_report
end type
type gb_1 from groupbox within w_payment_processing_report
end type
end forward

global type w_payment_processing_report from w_a_report
string title = "Payment and Award Processed Balance Report"
cb_ok cb_ok
cb_clear cb_clear
dw_enter_batch_no dw_enter_batch_no
cb_close cb_close
gb_1 gb_1
end type
global w_payment_processing_report w_payment_processing_report

type variables

end variables

event open;call super::open;
LONG ll_result

/*	Database Connections 
*/

dw_report.SetTransObject(SQLCA)

			
dw_enter_batch_no.SetTransObject(SQLCA)
dw_enter_batch_no.insertrow(0)


dw_enter_batch_no.setitem(1,'batch_no',0)

  
        
			
             

end event

on w_payment_processing_report.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.cb_clear=create cb_clear
this.dw_enter_batch_no=create dw_enter_batch_no
this.cb_close=create cb_close
this.gb_1=create gb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.cb_clear
this.Control[iCurrent+3]=this.dw_enter_batch_no
this.Control[iCurrent+4]=this.cb_close
this.Control[iCurrent+5]=this.gb_1
end on

on w_payment_processing_report.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.cb_clear)
destroy(this.dw_enter_batch_no)
destroy(this.cb_close)
destroy(this.gb_1)
end on

type dw_report from w_a_report`dw_report within w_payment_processing_report
integer x = 5
integer y = 296
integer width = 2693
integer height = 2120
integer taborder = 40
string dataobject = "d_payment_processing_by_batch"
boolean hscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event dw_report::buttonclicked;call super::buttonclicked;LONG	ll_first_group_row

if dwo.name = 'b_1' Then
	If This.RowCount() > 0 Then
		ll_first_group_row = This.FindGroupChange(GetRow() + 1,1)
		IF ll_first_group_row = 0 Then 
			ll_first_group_row = 1
		End if
		This.SetRow(ll_first_group_row)
		This.ScrollToRow(ll_first_group_row)
	End if
End if
	
end event

type cb_ok from commandbutton within w_payment_processing_report
integer x = 2359
integer y = 16
integer width = 293
integer height = 92
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;/*	Initialization*/
LONG		ll_numrows
LONG		ll_batch_no 


dw_enter_batch_no.AcceptText()

ll_batch_no = dw_enter_batch_no.getitemnumber(1,'batch_no')

If ll_batch_no = 0 Then
	MessageBox(" Reports","Please enter a batch number")
	Return
End If


ll_numrows = dw_report.Retrieve(ll_batch_no)
SQLCA.nf_handle_error("w_payment_processing_report","dw_report","cb_ok")

IF ll_numrows < 0 then
	SignalError(-666,'Error retrieving payment processing report.')
elseif ll_numrows = 0 Then
	SignalError(-666,'No payments where found for this batch.  This is a data integrity error.')
END IF

end event

type cb_clear from commandbutton within w_payment_processing_report
integer x = 2359
integer y = 136
integer width = 293
integer height = 92
integer taborder = 31
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Clear"
end type

event clicked;datawindowchild ldwc_child_1
integer li_rc

dw_enter_batch_no.reset()
dw_enter_batch_no.insertrow(0)

dw_enter_batch_no.GetChild('batch_no', ldwc_child_1)
li_rc = ldwc_child_1.SetFilter("")
li_rc = ldwc_child_1.Filter()

dw_enter_batch_no.setitem(1,'batch_no',0)
end event

type dw_enter_batch_no from u_dw_online within w_payment_processing_report
integer x = 155
integer y = 72
integer width = 1207
integer height = 184
integer taborder = 30
string dataobject = "d_enter_batch_no"
boolean border = false
boolean livescroll = true
end type

event itemchanged;string ls_admin_region
DATAWINDOWCHILD	ldwc_child_1
integer li_rc
long ll_row

CHOOSE CASE this.GetColumnName()
	CASE "admin_region"
		dw_enter_batch_no.setitem(1,'batch_no',0)
			
	
		ls_admin_region = this.GetText()
		
		
		dw_enter_batch_no.GetChild('batch_no', ldwc_child_1)
		li_rc = ldwc_child_1.SetFilter("admin_region_code = '" +  ls_admin_region + "'" )
		li_rc = ldwc_child_1.Filter()
		
	CASE "batch_no"
		
		dw_enter_batch_no.GetChild('batch_no', ldwc_child_1)	
		ll_row = ldwc_child_1.getrow()
		ls_admin_region = ldwc_child_1.getitemstring(ll_row,'admin_region_code')
			
		dw_enter_batch_no.setitem(1,'admin_region',ls_admin_region)

END CHOOSE


end event

type cb_close from commandbutton within w_payment_processing_report
integer x = 2304
integer y = 2440
integer width = 311
integer height = 96
integer taborder = 50
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;Close(Parent)
end event

type gb_1 from groupbox within w_payment_processing_report
integer x = 37
integer width = 1522
integer height = 276
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

