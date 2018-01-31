$PBExportHeader$w_ungrouped_payments.srw
forward
global type w_ungrouped_payments from w_a_report
end type
type cb_ok from commandbutton within w_ungrouped_payments
end type
type em_from from editmask within w_ungrouped_payments
end type
type em_to from editmask within w_ungrouped_payments
end type
type st_1 from statictext within w_ungrouped_payments
end type
type st_2 from statictext within w_ungrouped_payments
end type
type cb_print from commandbutton within w_ungrouped_payments
end type
type cb_clear from commandbutton within w_ungrouped_payments
end type
type cb_close from commandbutton within w_ungrouped_payments
end type
type pb_help from picturebutton within w_ungrouped_payments
end type
type gb_1 from groupbox within w_ungrouped_payments
end type
end forward

global type w_ungrouped_payments from w_a_report
integer width = 2761
string title = "Ungrouped Payments for Authorization Grouping"
cb_ok cb_ok
em_from em_from
em_to em_to
st_1 st_1
st_2 st_2
cb_print cb_print
cb_clear cb_clear
cb_close cb_close
pb_help pb_help
gb_1 gb_1
end type
global w_ungrouped_payments w_ungrouped_payments

type variables
DATASTORE ids_users
 n_resize inv_resize

end variables

on w_ungrouped_payments.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.em_from=create em_from
this.em_to=create em_to
this.st_1=create st_1
this.st_2=create st_2
this.cb_print=create cb_print
this.cb_clear=create cb_clear
this.cb_close=create cb_close
this.pb_help=create pb_help
this.gb_1=create gb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.em_from
this.Control[iCurrent+3]=this.em_to
this.Control[iCurrent+4]=this.st_1
this.Control[iCurrent+5]=this.st_2
this.Control[iCurrent+6]=this.cb_print
this.Control[iCurrent+7]=this.cb_clear
this.Control[iCurrent+8]=this.cb_close
this.Control[iCurrent+9]=this.pb_help
this.Control[iCurrent+10]=this.gb_1
end on

on w_ungrouped_payments.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.em_from)
destroy(this.em_to)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.cb_print)
destroy(this.cb_clear)
destroy(this.cb_close)
destroy(this.pb_help)
destroy(this.gb_1)
end on

event open;call super::open;LONG ll_rows, ll_cntr

dw_report.SetTransObject(SQLCA)

IF IsNull(inv_resize) OR NOT IsValid (inv_resize) THEN
	inv_resize = create n_resize
	inv_resize.of_SetOrigSize (2729,2580)
END IF

//THIS.inv_resize.of_register(dw_report,'scaletoright')
THIS.inv_resize.of_register(dw_report,0,0,100,100)
end event

event resize;call super::resize;
long ll_workspacewidth,ll_workspaceheight


// Notify the resize service that the window size has changed.
ll_workspacewidth = This.WorkSpaceWidth()
ll_workspaceheight = This.WorkSpaceHeight()

If IsValid (inv_resize) Then
	inv_resize.Event pfc_Resize (sizetype,ll_workspacewidth  , ll_workspaceheight )
End If
end event

type dw_report from w_a_report`dw_report within w_ungrouped_payments
integer y = 360
integer height = 2120
integer taborder = 80
string dataobject = "d_ungrouped_payments_report"
boolean hscrollbar = true
end type

type cb_ok from commandbutton within w_ungrouped_payments
integer x = 1445
integer y = 168
integer width = 288
integer height = 104
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "OK"
end type

event clicked;DATETIME ldtm_from, ldtm_to
LONG ll_rows, ll_item

ldtm_from = DATETIME(em_from.Text)
IF IsNull(ldtm_from)  THEN
	MessageBox('Missing Date','Please enter a "From" date', Information!)
	RETURN 
END IF

IF ldtm_from <= DATETIME('1900-01-01')  OR ldtm_from > DATETIME('2076-06-06') THEN
	MessageBox('Invalid Date', 'From date must be greater than "1900-01-01" and less than or equal to "2076-06-06".', Information!)
	RETURN
END IF

ldtm_to = DATETIME(em_to.Text)
IF IsNull(ldtm_to)  THEN
	MessageBox('Missing Date','Please enter a "To" date', Information!)
	RETURN
END IF

IF ldtm_to <= DATETIME('1900-01-01')  OR ldtm_to > DATETIME('2076-06-06') THEN
	MessageBox('Invalid Date', 'To date must be greater than "1900-01-01" and less than or equal to "2076-06-06".', Information!)
	RETURN
END IF

IF ldtm_from > ldtm_to THEN
	MessageBox('Invalid Date Range','The From date must be less than or equal to the To date. Please re-enter.', Information!)
	RETURN
END IF

ldtm_to = DATETIME(RelativeDate(DATE(ldtm_to), 1))

ll_rows = dw_report.Retrieve(ldtm_from, ldtm_to)
SQLCA.nf_handle_error("w_authorized_payments","cb_ok","dw_report.Retrieve")


IF ll_rows = 0 THEN
	MessageBox('No Records','The search returned no results for the criteria entered.', Information!)
	RETURN
END IF
end event

type em_from from editmask within w_ungrouped_payments
integer x = 443
integer y = 180
integer width = 411
integer height = 84
integer taborder = 10
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "yyyy-mm-dd"
boolean dropdowncalendar = true
end type

type em_to from editmask within w_ungrouped_payments
integer x = 997
integer y = 180
integer width = 411
integer height = 84
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "yyyy-mm-dd"
boolean dropdowncalendar = true
end type

type st_1 from statictext within w_ungrouped_payments
integer x = 274
integer y = 192
integer width = 174
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "From:"
boolean focusrectangle = false
end type

type st_2 from statictext within w_ungrouped_payments
integer x = 891
integer y = 192
integer width = 87
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "To:"
boolean focusrectangle = false
end type

type cb_print from commandbutton within w_ungrouped_payments
integer x = 2021
integer y = 168
integer width = 288
integer height = 104
integer taborder = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Print"
end type

event clicked;IF dw_report.RowCount() = 0 THEN
	MessageBox('No Records','There is nothing to print. Please retrieve the report, then try again.', Information!)
	RETURN
END IF

dw_report.Print()
end event

type cb_clear from commandbutton within w_ungrouped_payments
integer x = 1733
integer y = 168
integer width = 288
integer height = 104
integer taborder = 50
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Clear"
end type

event clicked;em_from.Text = '0000-00-00'
em_to.Text =  '0000-00-00'
dw_report.Reset()
end event

type cb_close from commandbutton within w_ungrouped_payments
integer x = 2313
integer y = 168
integer width = 306
integer height = 104
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Close"
end type

event clicked;CLOSE(PARENT)
end event

type pb_help from picturebutton within w_ungrouped_payments
integer x = 105
integer y = 180
integer width = 105
integer height = 88
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "Help!"
alignment htextalign = left!
end type

event clicked;OPEN(w_ungrouped_payments_report_help)
end event

type gb_1 from groupbox within w_ungrouped_payments
integer x = 46
integer y = 76
integer width = 2619
integer height = 248
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Report Parameters"
end type

