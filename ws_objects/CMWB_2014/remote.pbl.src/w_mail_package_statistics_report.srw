$PBExportHeader$w_mail_package_statistics_report.srw
forward
global type w_mail_package_statistics_report from w_a_report
end type
type cb_ok from commandbutton within w_mail_package_statistics_report
end type
type ddlb_from from dropdownlistbox within w_mail_package_statistics_report
end type
type st_from from statictext within w_mail_package_statistics_report
end type
type cb_export from commandbutton within w_mail_package_statistics_report
end type
type ddlb_to from dropdownlistbox within w_mail_package_statistics_report
end type
type st_to from statictext within w_mail_package_statistics_report
end type
end forward

global type w_mail_package_statistics_report from w_a_report
integer height = 2688
string title = "Mail Package Statistics Report"
cb_ok cb_ok
ddlb_from ddlb_from
st_from st_from
cb_export cb_export
ddlb_to ddlb_to
st_to st_to
end type
global w_mail_package_statistics_report w_mail_package_statistics_report

type variables

end variables

event open;call super::open;LONG 		ll_year, ll_this_year, ll_month, ll_this_month
DATETIME ldtm_datetime

SetPointer(HourGlass!)

dw_report.SetTransObject (SQLCA)

ldtm_datetime = f_server_datetime()

ll_this_year =  Year(Date(ldtm_datetime))
ll_this_month =  Month(Date(ldtm_datetime))

FOR ll_year = 1996 TO ll_this_year
	FOR ll_month = 1 TO 12
		IF ll_year = ll_this_year AND ll_month >= ll_this_month THEN
			// Ignore current year/month and months in the future
		ELSE
			ddlb_from.AddItem(String(ll_year) + String(ll_month, '00'))
			ddlb_to.AddItem(String(ll_year) + String(ll_month, '00'))
		END IF
	NEXT
NEXT

ddlb_from.Text = ddlb_from.Text(ddlb_from.TotalItems())
ddlb_to.Text = ddlb_to.Text(ddlb_to.TotalItems())
end event

on w_mail_package_statistics_report.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.ddlb_from=create ddlb_from
this.st_from=create st_from
this.cb_export=create cb_export
this.ddlb_to=create ddlb_to
this.st_to=create st_to
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.ddlb_from
this.Control[iCurrent+3]=this.st_from
this.Control[iCurrent+4]=this.cb_export
this.Control[iCurrent+5]=this.ddlb_to
this.Control[iCurrent+6]=this.st_to
end on

on w_mail_package_statistics_report.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.ddlb_from)
destroy(this.st_from)
destroy(this.cb_export)
destroy(this.ddlb_to)
destroy(this.st_to)
end on

type dw_report from w_a_report`dw_report within w_mail_package_statistics_report
integer y = 460
integer height = 2020
integer taborder = 10
string dataobject = "d_mail_package_statistics_report"
boolean hscrollbar = true
end type

event dw_report::rbuttondown;// OVERRIDE
M_DW_ONLINE_RMB_POPUP lm_popup
/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	
	lm_popup.m_options.PopMenu(Parent.PointerX(), Parent.PointerY())

	Destroy lm_popup
end event

type cb_ok from commandbutton within w_mail_package_statistics_report
integer x = 2363
integer y = 28
integer width = 293
integer height = 108
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;DATE ldt_start, ldt_end

SetPointer(HourGlass!)

ldt_start = Date(Mid(ddlb_from.Text,1,4) + '/' + Mid(ddlb_from.Text, 5, 2) + '/01')
IF Mid(ddlb_to.Text, 5, 2) < '12' THEN
	ldt_end = Date(Mid(ddlb_to.Text,1,4) + '/' + String(Long(Mid(ddlb_to.Text, 5, 2)) + 1, '00') + '/01')
ELSE
	ldt_end = Date(String(Long(Mid(ddlb_to.Text,1,4)) + 1, '00') + '/01/01')
END IF

IF ldt_start > ldt_end THEN
	MessageBox('Error', 'Start date must be less than or equal to end date')
	Return -1
END IF

IF DaysAfter(ldt_start, ldt_end) > 365 THEN
	IF MessageBox('Warning', 'Selecting more that one year may take considerable time and resources', Exclamation!, OkCancel!) = 2 THEN
		RETURN -1
	END IF
END IF


dw_report.Retrieve(ldt_start, ldt_end)
IF SQLCA.nf_handle_error('Retrieve', 'in ue_postopen', 'w_mail_package_statistics_report') < 0 THEN
	Return -1
END IF

end event

type ddlb_from from dropdownlistbox within w_mail_package_statistics_report
integer x = 613
integer y = 28
integer width = 311
integer height = 416
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

type st_from from statictext within w_mail_package_statistics_report
integer x = 174
integer y = 32
integer width = 416
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "From YYYYMM:"
alignment alignment = right!
boolean focusrectangle = false
end type

type cb_export from commandbutton within w_mail_package_statistics_report
integer x = 2363
integer y = 184
integer width = 293
integer height = 108
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Export"
end type

event clicked;dw_report.SaveAs()
end event

type ddlb_to from dropdownlistbox within w_mail_package_statistics_report
integer x = 1390
integer y = 28
integer width = 311
integer height = 416
integer taborder = 10
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

type st_to from statictext within w_mail_package_statistics_report
integer x = 1006
integer y = 32
integer width = 361
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "To YYYYMM:"
alignment alignment = right!
boolean focusrectangle = false
end type

