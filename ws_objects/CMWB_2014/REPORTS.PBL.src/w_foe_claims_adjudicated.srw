$PBExportHeader$w_foe_claims_adjudicated.srw
forward
global type w_foe_claims_adjudicated from window
end type
type cb_print from commandbutton within w_foe_claims_adjudicated
end type
type st_2 from statictext within w_foe_claims_adjudicated
end type
type st_1 from statictext within w_foe_claims_adjudicated
end type
type em_end_date from editmask within w_foe_claims_adjudicated
end type
type em_start_date from editmask within w_foe_claims_adjudicated
end type
type cb_retrieve from commandbutton within w_foe_claims_adjudicated
end type
type dw_report from u_dw_online within w_foe_claims_adjudicated
end type
type cb_close from commandbutton within w_foe_claims_adjudicated
end type
end forward

global type w_foe_claims_adjudicated from window
integer width = 2779
integer height = 2716
boolean titlebar = true
string title = "Form of Election Adjudicated Claims"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
cb_print cb_print
st_2 st_2
st_1 st_1
em_end_date em_end_date
em_start_date em_start_date
cb_retrieve cb_retrieve
dw_report dw_report
cb_close cb_close
end type
global w_foe_claims_adjudicated w_foe_claims_adjudicated

type variables
n_resize inv_resize
end variables

on w_foe_claims_adjudicated.create
this.cb_print=create cb_print
this.st_2=create st_2
this.st_1=create st_1
this.em_end_date=create em_end_date
this.em_start_date=create em_start_date
this.cb_retrieve=create cb_retrieve
this.dw_report=create dw_report
this.cb_close=create cb_close
this.Control[]={this.cb_print,&
this.st_2,&
this.st_1,&
this.em_end_date,&
this.em_start_date,&
this.cb_retrieve,&
this.dw_report,&
this.cb_close}
end on

on w_foe_claims_adjudicated.destroy
destroy(this.cb_print)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.em_end_date)
destroy(this.em_start_date)
destroy(this.cb_retrieve)
destroy(this.dw_report)
destroy(this.cb_close)
end on

event open;Integer li_rtn

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


SetPointer(HourGlass!)

li_rtn = dw_report.SetTransObject(SQLCA)

dw_report.object.datawindow.hidegrayline = TRUE

if IsNull(inv_resize) Or not IsValid (inv_resize) then
	inv_resize = create n_resize
	inv_resize.of_SetOrigSize (2743,2612)
end if

This.inv_resize.of_register(dw_report,'scaletoright&bottom')
This.inv_resize.of_register(cb_retrieve,'fixedtoright&bottom')
This.inv_resize.of_register(cb_print,'fixedtoright&bottom')
This.inv_resize.of_register(cb_close,'fixedtoright&bottom')
end event

event resize;//dw_report.width = This.width - 80

long ll_workspacewidth,ll_workspaceheight


// Notify the resize service that the window size has changed.
ll_workspacewidth = This.WorkSpaceWidth()
ll_workspaceheight = This.WorkSpaceHeight()

If IsValid (inv_resize) Then
	inv_resize.Event pfc_Resize (sizetype,ll_workspacewidth  , ll_workspaceheight )
End If

end event

type cb_print from commandbutton within w_foe_claims_adjudicated
integer x = 1792
integer y = 2452
integer width = 402
integer height = 104
integer taborder = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Print"
end type

event clicked;dw_report.Print()
end event

type st_2 from statictext within w_foe_claims_adjudicated
integer x = 869
integer y = 44
integer width = 256
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "To Date:"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_1 from statictext within w_foe_claims_adjudicated
integer x = 59
integer y = 44
integer width = 311
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "From Date:"
alignment alignment = right!
boolean focusrectangle = false
end type

type em_end_date from editmask within w_foe_claims_adjudicated
integer x = 1138
integer y = 32
integer width = 402
integer height = 84
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "yyyy-mm-dd"
end type

type em_start_date from editmask within w_foe_claims_adjudicated
integer x = 379
integer y = 32
integer width = 402
integer height = 84
integer taborder = 10
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
boolean autoskip = true
end type

type cb_retrieve from commandbutton within w_foe_claims_adjudicated
integer x = 1280
integer y = 2452
integer width = 402
integer height = 104
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "OK"
boolean default = true
end type

event clicked;LONG		ll_num_rows
INTEGER	li_rtn
STRING	ls_start_date_flag, ls_end_date_flag, ls_parameters
DATE		ldt_start_date, ldt_end_date, ldt_min_date, ldt_max_date

SetPointer(HourGlass!)
dw_report.Reset()
dw_report.Visible = FALSE

li_rtn = dw_report.SetTransObject(SQLCA)


ldt_min_date = Date(1900,01,01)
ldt_max_date = Date(2079,06,06)

// Get Start Date Parameter
IF IsDate(em_start_date.Text) = FALSE THEN
	ls_start_date_flag = "N"
ELSE
	ls_start_date_flag = "Y"
	ldt_start_date = Date(em_start_date.Text)

	IF ldt_start_date < ldt_min_date OR ldt_start_date > ldt_max_date THEN
		MessageBox("Invalid Start Date", "Start date must be after Jan 1, 1900 and before Jun 6, 2079.", Exclamation!)
		em_start_date.SetFocus()
		dw_report.Visible = TRUE
		RETURN
	END IF
	ls_parameters = "Start Date >= " + em_start_date.Text + " and "
	
END IF

// Get End Date Parameter
IF IsDate(em_end_date.Text) = FALSE THEN
	ls_end_date_flag = "N"
ELSE
	ls_end_date_flag = "Y"
	ldt_end_date = Date(em_end_date.Text)

	IF ldt_end_date < ldt_min_date OR ldt_end_date > ldt_max_date THEN
		MessageBox("Invalid End Date", "End date must be after Jan 1, 1900 and before Jun 6, 2079.", Exclamation!)
		em_end_date.SetFocus()
		dw_report.Visible = TRUE
		RETURN
	END IF
	ls_parameters = ls_parameters + "End Date <= " + em_end_date.Text + "  "
END IF

// Make sure start date isn't after end date
IF	ls_start_date_flag = "Y" AND ls_end_date_flag = "Y" AND ldt_start_date > ldt_end_date THEN
	MessageBox("Invalid Date Range", "Start Date must be before End Date.  Please re-enter dates.")
	dw_report.Visible = TRUE
	RETURN
END IF

// Make sure a parameter was specified
IF	ls_start_date_flag = "N" AND ls_end_date_flag = "N" THEN
	MessageBox("No Parameters Specified", "Date parameters must be supplied to run the report.  Enter them and try again.")
	dw_report.Visible = TRUE
	RETURN
ELSEIF ls_start_date_flag = "N" AND ls_end_date_flag = "Y" THEN
	MessageBox("No Start Date", "The Start Date must be supplied to run the report.  Enter it and try again.")
	dw_report.Visible = TRUE
	RETURN
ELSEIF ls_start_date_flag = "Y" AND ls_end_date_flag = "N" THEN
	MessageBox("No End Date", "The End Date must be supplied to run the report.  Enter it and try again.")
	dw_report.Visible = TRUE
	RETURN
END IF

// Retrieve the Report
ll_num_rows = dw_report.Retrieve(ldt_start_date, ldt_end_date)
li_rtn = SQLCA.nf_handle_error("w_din_report", "", "cb_retrieve - dw_report.Retrieve(ls_din_gp_pin_flag, ll_din_gp_pin)")

IF ll_num_rows = 0 THEN
	MessageBox("No data found", "No data was found with the parameters you specified:~r~r" + ls_parameters + "~r~rChange your parameters and try again.")
	dw_report.Visible = TRUE
	RETURN
END IF

dw_report.Modify("t_report_parameters.text = '" + ls_parameters + "'")

dw_report.Visible = TRUE

end event

type dw_report from u_dw_online within w_foe_claims_adjudicated
integer x = 27
integer y = 184
integer width = 2674
integer height = 2232
integer taborder = 30
string dataobject = "d_foe_claims_adjudicated"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type cb_close from commandbutton within w_foe_claims_adjudicated
integer x = 2304
integer y = 2452
integer width = 402
integer height = 104
integer taborder = 40
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "C&lose"
end type

event clicked;Close(PARENT)
end event

