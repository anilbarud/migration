$PBExportHeader$w_authorized_payments.srw
forward
global type w_authorized_payments from w_a_report
end type
type cb_ok from commandbutton within w_authorized_payments
end type
type em_from from editmask within w_authorized_payments
end type
type em_to from editmask within w_authorized_payments
end type
type ddlb_users from dropdownlistbox within w_authorized_payments
end type
type st_1 from statictext within w_authorized_payments
end type
type st_2 from statictext within w_authorized_payments
end type
type st_3 from statictext within w_authorized_payments
end type
type cb_print from commandbutton within w_authorized_payments
end type
type cb_excel from commandbutton within w_authorized_payments
end type
type cb_clear from commandbutton within w_authorized_payments
end type
type gb_1 from groupbox within w_authorized_payments
end type
type gb_2 from groupbox within w_authorized_payments
end type
end forward

global type w_authorized_payments from w_a_report
integer width = 2761
string title = "Authorized Payments Report"
cb_ok cb_ok
em_from em_from
em_to em_to
ddlb_users ddlb_users
st_1 st_1
st_2 st_2
st_3 st_3
cb_print cb_print
cb_excel cb_excel
cb_clear cb_clear
gb_1 gb_1
gb_2 gb_2
end type
global w_authorized_payments w_authorized_payments

type variables
DATASTORE ids_users
 n_resize inv_resize

end variables

on w_authorized_payments.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.em_from=create em_from
this.em_to=create em_to
this.ddlb_users=create ddlb_users
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.cb_print=create cb_print
this.cb_excel=create cb_excel
this.cb_clear=create cb_clear
this.gb_1=create gb_1
this.gb_2=create gb_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.em_from
this.Control[iCurrent+3]=this.em_to
this.Control[iCurrent+4]=this.ddlb_users
this.Control[iCurrent+5]=this.st_1
this.Control[iCurrent+6]=this.st_2
this.Control[iCurrent+7]=this.st_3
this.Control[iCurrent+8]=this.cb_print
this.Control[iCurrent+9]=this.cb_excel
this.Control[iCurrent+10]=this.cb_clear
this.Control[iCurrent+11]=this.gb_1
this.Control[iCurrent+12]=this.gb_2
end on

on w_authorized_payments.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.em_from)
destroy(this.em_to)
destroy(this.ddlb_users)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.cb_print)
destroy(this.cb_excel)
destroy(this.cb_clear)
destroy(this.gb_1)
destroy(this.gb_2)
end on

event open;call super::open;DATASTORE lds_users
LONG ll_rows, ll_cntr

ids_users = CREATE DATASTORE
ids_users.DataObject = 'dddw_user_profile_active'
ids_users.SetTransObject(SQLCA)

dw_report.SetTransObject(SQLCA)

ll_rows = ids_users.Retrieve()

ddlb_users.AddItem('')

FOR ll_cntr = 1 TO ll_rows
	ddlb_users.AddItem(ids_users.GetItemString(ll_cntr,'user_name'))
NEXT

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

type dw_report from w_a_report`dw_report within w_authorized_payments
integer taborder = 80
string dataobject = "d_authorized_payments"
boolean hscrollbar = true
end type

type cb_ok from commandbutton within w_authorized_payments
integer x = 1705
integer y = 168
integer width = 347
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

event clicked;STRING ls_user, ls_user_id
DATETIME ldtm_from, ldtm_to
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

ls_user = ddlb_users.Text 
IF ls_user = '' THEN
	MessageBox('Missing User','Please select an Authorized User to search on.', Information!)
	RETURN
END IF
ll_rows = ids_users.RowCount()

ll_item = ids_users.Find("user_name = '" + TRIM(ls_user) + "'", 1, ll_rows)
IF ll_item > 0 THEN
	ls_user = ids_users.GetItemString(ll_item, 'user_id')
END IF


ll_rows = dw_report.Retrieve(ldtm_from, ldtm_to, TRIM(ls_user))
SQLCA.nf_handle_error("w_authorized_payments","cb_ok","dw_report.Retrieve")


IF ll_rows = 0 THEN
	MessageBox('No Records','The search returned no results for the criteria entered.', Information!)
	RETURN
END IF
end event

type em_from from editmask within w_authorized_payments
integer x = 407
integer y = 180
integer width = 448
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

type em_to from editmask within w_authorized_payments
integer x = 1120
integer y = 180
integer width = 448
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

type ddlb_users from dropdownlistbox within w_authorized_payments
integer x = 745
integer y = 300
integer width = 823
integer height = 404
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type st_1 from statictext within w_authorized_payments
integer x = 215
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

type st_2 from statictext within w_authorized_payments
integer x = 983
integer y = 192
integer width = 123
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

type st_3 from statictext within w_authorized_payments
integer x = 210
integer y = 320
integer width = 471
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
string text = "Authorized User: "
boolean focusrectangle = false
end type

type cb_print from commandbutton within w_authorized_payments
integer x = 2213
integer y = 168
integer width = 411
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

type cb_excel from commandbutton within w_authorized_payments
integer x = 2213
integer y = 292
integer width = 411
integer height = 104
integer taborder = 70
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Save to Excel"
end type

event clicked;dw_report.SaveAs('',Excel8!,TRUE,EncodingANSI! )
end event

type cb_clear from commandbutton within w_authorized_payments
integer x = 1705
integer y = 292
integer width = 347
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
ddlb_users.SelectItem(0)
dw_report.Reset()
end event

type gb_1 from groupbox within w_authorized_payments
integer x = 46
integer y = 76
integer width = 2071
integer height = 376
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Report Parameters"
end type

type gb_2 from groupbox within w_authorized_payments
integer x = 2144
integer y = 76
integer width = 530
integer height = 376
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
end type

