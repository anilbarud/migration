$PBExportHeader$w_annuity_workflow_completed_report.srw
forward
global type w_annuity_workflow_completed_report from w_a_report
end type
type dw_admin_region from datawindow within w_annuity_workflow_completed_report
end type
type cb_retrieve from commandbutton within w_annuity_workflow_completed_report
end type
type st_region from statictext within w_annuity_workflow_completed_report
end type
type em_date_to from editmask within w_annuity_workflow_completed_report
end type
type em_date_from from editmask within w_annuity_workflow_completed_report
end type
type rb_detail from radiobutton within w_annuity_workflow_completed_report
end type
type rb_summary from radiobutton within w_annuity_workflow_completed_report
end type
type rb_enter from radiobutton within w_annuity_workflow_completed_report
end type
type rb_all_annuity_work from radiobutton within w_annuity_workflow_completed_report
end type
type st_period from statictext within w_annuity_workflow_completed_report
end type
type st_2 from statictext within w_annuity_workflow_completed_report
end type
type gb_1 from groupbox within w_annuity_workflow_completed_report
end type
type gb_2 from groupbox within w_annuity_workflow_completed_report
end type
end forward

global type w_annuity_workflow_completed_report from w_a_report
integer y = 49
integer width = 2761
string title = "Annuity Work Completed Report"
dw_admin_region dw_admin_region
cb_retrieve cb_retrieve
st_region st_region
em_date_to em_date_to
em_date_from em_date_from
rb_detail rb_detail
rb_summary rb_summary
rb_enter rb_enter
rb_all_annuity_work rb_all_annuity_work
st_period st_period
st_2 st_2
gb_1 gb_1
gb_2 gb_2
end type
global w_annuity_workflow_completed_report w_annuity_workflow_completed_report

type variables
date idt_use_confirm_annuity_eligibility_module_date
end variables

on w_annuity_workflow_completed_report.create
int iCurrent
call super::create
this.dw_admin_region=create dw_admin_region
this.cb_retrieve=create cb_retrieve
this.st_region=create st_region
this.em_date_to=create em_date_to
this.em_date_from=create em_date_from
this.rb_detail=create rb_detail
this.rb_summary=create rb_summary
this.rb_enter=create rb_enter
this.rb_all_annuity_work=create rb_all_annuity_work
this.st_period=create st_period
this.st_2=create st_2
this.gb_1=create gb_1
this.gb_2=create gb_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_admin_region
this.Control[iCurrent+2]=this.cb_retrieve
this.Control[iCurrent+3]=this.st_region
this.Control[iCurrent+4]=this.em_date_to
this.Control[iCurrent+5]=this.em_date_from
this.Control[iCurrent+6]=this.rb_detail
this.Control[iCurrent+7]=this.rb_summary
this.Control[iCurrent+8]=this.rb_enter
this.Control[iCurrent+9]=this.rb_all_annuity_work
this.Control[iCurrent+10]=this.st_period
this.Control[iCurrent+11]=this.st_2
this.Control[iCurrent+12]=this.gb_1
this.Control[iCurrent+13]=this.gb_2
end on

on w_annuity_workflow_completed_report.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.dw_admin_region)
destroy(this.cb_retrieve)
destroy(this.st_region)
destroy(this.em_date_to)
destroy(this.em_date_from)
destroy(this.rb_detail)
destroy(this.rb_summary)
destroy(this.rb_enter)
destroy(this.rb_all_annuity_work)
destroy(this.st_period)
destroy(this.st_2)
destroy(this.gb_1)
destroy(this.gb_2)
end on

event open;call super::open;
/*	Database Connections */
dw_report.SetTransObject(SQLCA)


SELECT use_confirm_annuity_eligibility_module_date
INTO   :idt_use_confirm_annuity_eligibility_module_date
FROM   Annuity_Parameter
WHERE  active_flag = 'Y'
USING SQLCA;
SQLCA.nf_handle_error('w_annuity_workflow_completed_report','embedded SQL: SELECT use_confirm_annuity_eligibility_module_date FROM Annuity_Parameter...','open event')
end event

event ue_print;u_ds ldw_summary
long ll_rows
string ls_admin_region_selected


//if we have a detail report for all regions, we want the sumary report (which is a nested report) to be 
// printed on a separte page so we'll turn off the nested report (dw_summary) , shrink the summary 
// band, and print, then create a datastore, re-retrieve the summary data and print that separately.
// otherwise, its the stand alone summary report so we'll just print it as is

ls_admin_region_selected  = dw_admin_region.getItemString(dw_admin_region.getRow(),'admin_region_code')

IF rb_detail.checked  = TRUE AND ls_admin_region_selected = 'ALL' THEN
	dw_report.object.dw_summary.visible = 0
	dw_report.object.datawindow.Summary.height = 200
		
	dw_report.Print()
		
	ldw_summary = create u_ds
	ldw_summary.dataobject = 'd_annuity_work_completed_summary'
	ldw_summary.settransobject(SQLCA)
	ll_rows = ldw_summary.retrieve(ls_admin_region_selected,datetime(em_date_from.text),datetime(em_date_to.text))
	ldw_summary.object.t_region.text = dw_report.object.t_region.text
	ldw_summary.object.t_entered_or_allwork.text = dw_report.object.t_entered_or_allwork.text
	
	ldw_summary.Print()
	
	destroy ldw_summary
	
	dw_report.object.dw_summary.visible = 1
	dw_report.object.datawindow.Summary.height = 1744
	
ELSE
	dw_report.Print()
END IF
end event

event resize;call super::resize;call super::resize
dw_report.width = newwidth - 24
dw_report.height = newheight - 320

end event

type dw_report from w_a_report`dw_report within w_annuity_workflow_completed_report
integer x = 14
integer y = 252
integer width = 2683
integer height = 2220
integer taborder = 0
string title = "Workflow Performance Report"
string dataobject = "d_annuity_work_completed_detail_and_summary"
boolean hscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type dw_admin_region from datawindow within w_annuity_workflow_completed_report
integer x = 23
integer y = 124
integer width = 850
integer height = 88
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "d_admin_region_select"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event constructor;datawindowchild adw_child
long ll_row, ll_find
string ls_user_profile_admin_region

this.setTransObject(SQLCA)
this.retrieve()

this.getChild('admin_region_code', adw_child)

ls_user_profile_admin_region = vgst_user_profile.default_admin_region_code

ll_find = adw_child.find("admin_region_code = '" + ls_user_profile_admin_region + "'", 1,adw_child.rowcount())

IF ll_find > 0 THEN
	this.setRow(ll_find)
	this.scrolltorow(ll_find)	
ELSE
	MESSAGEBOX("ERROR","Cannot locate a valid admin region code for you. Please call the help desk.")
	RETURN
END IF
end event

type cb_retrieve from commandbutton within w_annuity_workflow_completed_report
integer x = 2391
integer y = 76
integer width = 297
integer height = 116
integer taborder = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Retrieve"
boolean default = true
end type

event clicked;STRING ls_admin_region, ls_admin_region_selected, ls_admin_region_desc
INT     li_cntr, li_rowcount
LONG   ll_rows
DateTime    ldt_from, ldt_to
DataWindowChild ldwc_child

dw_admin_region.accepttext()
dw_admin_region.getChild('admin_region_code',ldwc_child)

ls_admin_region_selected  = dw_admin_region.getItemString(dw_admin_region.getRow(),'admin_region_code')

ls_admin_region_desc = ldwc_child.getitemstring(ldwc_child.getrow(), 'admin_region_desc')

IF NOT ISDATE(em_date_from.text) THEN 
	MESSAGEBOX("Date Validation", "Invalid From Date")
	RETURN
END IF

IF NOT ISDATE(em_date_to.text) THEN 
	MESSAGEBOX("Date Validation", "Invalid To Date")
	RETURN
END IF

ldt_from = DATETIME(em_date_from.text)
ldt_to    = DATETIME(em_date_to.text + ' 23:59:59') // make the to date the last minute of the day

IF DATE(ldt_from) < idt_use_confirm_annuity_eligibility_module_date  THEN
	MESSAGEBOX("Date Validation", "The 'From' date must be " + STRING(idt_use_confirm_annuity_eligibility_module_date,'yyyy-mm-dd') + " or later. Please re-enter a valid From Date.", Information!)
	RETURN
END IF

IF ldt_from > ldt_to THEN
	MESSAGEBOX("Date Validation", "The 'From' date that is greater than the 'To' date. Please re-enter a valid date range.", Information!)
	RETURN
END IF

IF DATE(ldt_to) > DATE(f_server_datetime()) THEN
	MESSAGEBOX("Date Validation", "The 'To' date is in the future. Please re-enter a valid 'To' date.", Information!)
	RETURN
END IF


//Now set up the proper datawindow object and nested report (summary) if applicable
IF rb_summary.checked = TRUE THEN
	dw_report.dataobject = 'd_annuity_work_completed_summary'
ELSE
	dw_report.dataobject = 'd_annuity_work_completed_detail_and_summary'
END IF

dw_report.setTransObject(SQLCA)

IF rb_detail.checked = TRUE AND ls_admin_region_selected = 'ALL' THEN
	dw_report.object.dw_summary.visible = 1
	
ELSEIF rb_detail.checked = TRUE AND ls_admin_region_selected <> 'ALL' THEN
	dw_report.object.dw_summary.visible = 0
	dw_report.object.datawindow.Summary.height = 0

ELSEIF rb_summary.checked = TRUE AND ls_admin_region_selected <> 'ALL' THEN
	dw_report.object.datawindow.Summary.height = 0
END IF

ll_rows = dw_report.retrieve(ls_admin_region_selected,ldt_from, ldt_to)
SQLCA.nf_handle_error("w_annuity_workflow_completed_report","cb_retrieve","dw_report.retrieve")

dw_report.groupcalc()

dw_report.object.t_region.text = ls_admin_region_desc

IF rb_enter.checked THEN
	dw_report.object.t_entered_or_allwork.text = 'Entered'
ELSE
	dw_report.object.t_entered_or_allwork.text = 'All Work'	
END IF

IF rb_detail.checked THEN
	dw_report.object.t_detail_or_summary.text = 'Detail'
ELSE
	dw_report.object.t_detail_or_summary.text = 'Summary'
END IF

IF rb_detail.checked = TRUE AND ls_admin_region_selected = 'ALL' THEN
	dw_report.object.dw_summary[dw_report.rowcount()].object.t_region.text = dw_report.object.t_region.text
	dw_report.object.dw_summary[dw_report.rowcount()].object.t_entered_or_allwork.text = dw_report.object.t_entered_or_allwork.text 
END IF

If ll_rows < 1 THEN
	Messagebox("No Data", "There were no records for the region and date range selected")
END IF
end event

type st_region from statictext within w_annuity_workflow_completed_report
integer x = 37
integer y = 32
integer width = 283
integer height = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Region:"
boolean focusrectangle = false
end type

type em_date_to from editmask within w_annuity_workflow_completed_report
integer x = 1422
integer y = 124
integer width = 338
integer height = 88
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
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

type em_date_from from editmask within w_annuity_workflow_completed_report
integer x = 983
integer y = 124
integer width = 338
integer height = 88
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
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

type rb_detail from radiobutton within w_annuity_workflow_completed_report
integer x = 1906
integer y = 48
integer width = 366
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
string text = "Detail"
boolean checked = true
end type

type rb_summary from radiobutton within w_annuity_workflow_completed_report
integer x = 1906
integer y = 140
integer width = 370
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
string text = "Summary"
end type

type rb_enter from radiobutton within w_annuity_workflow_completed_report
integer x = 997
integer y = 32
integer width = 288
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Enter"
boolean checked = true
end type

event clicked;
IF CHECKED THEN
	em_date_from.displayonly = false
	em_date_from.text = '0000-00-00'
	
	em_date_to.displayonly = false
	em_date_to.text = '0000-00-00'
	
	em_date_from.setFocus()
END IF
end event

type rb_all_annuity_work from radiobutton within w_annuity_workflow_completed_report
integer x = 1408
integer y = 32
integer width = 352
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "All Work"
end type

event clicked;
IF CHECKED THEN
	em_date_from.displayonly = true
	em_date_from.text = STRING(idt_use_confirm_annuity_eligibility_module_date,'yyyy-mm-dd')
	
	em_date_to.displayonly = true
	em_date_to.text = String(today(),'yyyy-mm-dd')

END IF
end event

type st_period from statictext within w_annuity_workflow_completed_report
integer x = 750
integer y = 32
integer width = 215
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Period:"
boolean focusrectangle = false
end type

type st_2 from statictext within w_annuity_workflow_completed_report
integer x = 1335
integer y = 140
integer width = 91
integer height = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "To"
boolean focusrectangle = false
end type

type gb_1 from groupbox within w_annuity_workflow_completed_report
boolean visible = false
integer x = 1847
integer width = 480
integer height = 228
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
end type

type gb_2 from groupbox within w_annuity_workflow_completed_report
boolean visible = false
integer x = 987
integer width = 786
integer height = 116
integer taborder = 40
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
end type

