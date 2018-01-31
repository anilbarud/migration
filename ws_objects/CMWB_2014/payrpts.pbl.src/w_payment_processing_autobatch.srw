$PBExportHeader$w_payment_processing_autobatch.srw
forward
global type w_payment_processing_autobatch from w_a_report
end type
type rb_awards from radiobutton within w_payment_processing_autobatch
end type
type rb_payments from radiobutton within w_payment_processing_autobatch
end type
type cb_ok from commandbutton within w_payment_processing_autobatch
end type
type cb_clear from commandbutton within w_payment_processing_autobatch
end type
type cb_close from commandbutton within w_payment_processing_autobatch
end type
type dw_enter_batch_no from u_dw_online within w_payment_processing_autobatch
end type
type gb_1 from groupbox within w_payment_processing_autobatch
end type
type gb_2 from groupbox within w_payment_processing_autobatch
end type
end forward

global type w_payment_processing_autobatch from w_a_report
integer y = 49
integer width = 2770
integer height = 2708
string title = "Payment/Award Batch Report"
rb_awards rb_awards
rb_payments rb_payments
cb_ok cb_ok
cb_clear cb_clear
cb_close cb_close
dw_enter_batch_no dw_enter_batch_no
gb_1 gb_1
gb_2 gb_2
end type
global w_payment_processing_autobatch w_payment_processing_autobatch

type variables
n_resize inv_resize
end variables

on w_payment_processing_autobatch.create
int iCurrent
call super::create
this.rb_awards=create rb_awards
this.rb_payments=create rb_payments
this.cb_ok=create cb_ok
this.cb_clear=create cb_clear
this.cb_close=create cb_close
this.dw_enter_batch_no=create dw_enter_batch_no
this.gb_1=create gb_1
this.gb_2=create gb_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.rb_awards
this.Control[iCurrent+2]=this.rb_payments
this.Control[iCurrent+3]=this.cb_ok
this.Control[iCurrent+4]=this.cb_clear
this.Control[iCurrent+5]=this.cb_close
this.Control[iCurrent+6]=this.dw_enter_batch_no
this.Control[iCurrent+7]=this.gb_1
this.Control[iCurrent+8]=this.gb_2
end on

on w_payment_processing_autobatch.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.rb_awards)
destroy(this.rb_payments)
destroy(this.cb_ok)
destroy(this.cb_clear)
destroy(this.cb_close)
destroy(this.dw_enter_batch_no)
destroy(this.gb_1)
destroy(this.gb_2)
end on

event open;call super::open;//tab_reports.tabpage_payments.dw_payments.SetTransObject(SQLCA)
//tab_reports.tabpage_awards.dw_awards.SetTransObject(SQLCA)
//
dw_enter_batch_no.SetTransObject(SQLCA)
dw_enter_batch_no.insertrow(0)
dw_enter_batch_no.setitem(1,'batch_no',0)

if IsNull(inv_resize) Or not IsValid (inv_resize) then
	inv_resize = create n_resize
	inv_resize.of_SetOrigSize (2743,2612)
end if

This.inv_resize.of_register(cb_close,'fixedtoright&bottom')
This.inv_resize.of_register(dw_report,'ScaleToRight&Bottom')

dw_report.Object.DataWindow.Print.Preview='Yes'
end event

event resize;call super::resize;//dw_report.width = This.width - 80

long ll_workspacewidth,ll_workspaceheight


// Notify the resize service that the window size has changed.
ll_workspacewidth = This.WorkSpaceWidth()
ll_workspaceheight = This.WorkSpaceHeight()

If IsValid (inv_resize) Then
	inv_resize.Event pfc_Resize (sizetype,ll_workspacewidth  , ll_workspaceheight )
End If

end event

type dw_report from w_a_report`dw_report within w_payment_processing_autobatch
integer x = 41
integer y = 308
integer width = 2642
integer height = 2052
string dataobject = "d_payment_balancing_report"
boolean resizable = true
boolean hsplitscroll = true
borderstyle borderstyle = stylelowered!
end type

type rb_awards from radiobutton within w_payment_processing_autobatch
integer x = 1490
integer y = 168
integer width = 402
integer height = 64
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Awards"
end type

event clicked;dw_report.Reset()
end event

type rb_payments from radiobutton within w_payment_processing_autobatch
integer x = 1490
integer y = 92
integer width = 402
integer height = 64
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Payments"
boolean checked = true
end type

event clicked;dw_report.Reset()
end event

type cb_ok from commandbutton within w_payment_processing_autobatch
integer x = 2277
integer y = 40
integer width = 402
integer height = 92
integer taborder = 10
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "OK"
boolean default = true
end type

event clicked;LONG		ll_rows
LONG		ll_batch_no 
STRING	ls_admin_region, ls_award_type

dw_enter_batch_no.AcceptText()

ll_batch_no = dw_enter_batch_no.GetItemNumber(1,'batch_no')
IF ll_batch_no = 0 THEN
	MessageBox(" Reports","Please enter a batch number.")
	Return
END IF

ls_admin_region = dw_enter_batch_no.GetItemString(1,'admin_region')
IF ls_admin_region = '' OR IsNull(ls_admin_region) THEN
	MessageBox(" Reports","Please choose an admin region.")
	RETURN
END IF

IF rb_payments.Checked = TRUE THEN
	ls_award_type = 'payments'
	dw_report.DataObject = 'd_payment_balancing_report'
	dw_report.SetTransObject(SQLCA)
ELSEIF rb_awards.Checked = TRUE THEN
	ls_award_type = 'awards'
	dw_report.DataObject = 'd_awards_balancing_report'
	dw_report.SetTransObject(SQLCA)
ELSE
	MessageBox('Choose Report', 'Please select either Payment or Award report', Exclamation!)
	RETURN
END IF

ll_rows = dw_report.Retrieve(ls_admin_region,ll_batch_no)
SQLCA.nf_handle_error("w_payment_processing_autobatch","dw_reports","cb_ok")

IF ll_rows = 0 THEN
	MessageBox('No Records','There are no '+ls_award_type+' for the chosen admin region and batch number.',Exclamation!)
END IF

dw_report.Object.DataWindow.Print.Preview='Yes'
end event

type cb_clear from commandbutton within w_payment_processing_autobatch
integer x = 2277
integer y = 160
integer width = 402
integer height = 92
integer taborder = 20
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "C&lear"
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

type cb_close from commandbutton within w_payment_processing_autobatch
integer x = 2327
integer y = 2432
integer width = 402
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Close"
end type

event clicked;Close(PARENT)
end event

type dw_enter_batch_no from u_dw_online within w_payment_processing_autobatch
integer x = 91
integer y = 68
integer width = 1207
integer height = 184
integer taborder = 40
boolean bringtotop = true
string dataobject = "d_enter_auto_batch_no"
boolean border = false
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

type gb_1 from groupbox within w_payment_processing_autobatch
integer x = 50
integer y = 12
integer width = 1262
integer height = 264
integer taborder = 30
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Select Criteria"
borderstyle borderstyle = stylebox!
end type

type gb_2 from groupbox within w_payment_processing_autobatch
integer x = 1413
integer y = 12
integer width = 576
integer height = 264
integer taborder = 30
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Report Type"
end type

