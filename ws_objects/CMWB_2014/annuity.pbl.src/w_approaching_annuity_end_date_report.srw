$PBExportHeader$w_approaching_annuity_end_date_report.srw
forward
global type w_approaching_annuity_end_date_report from w_a_report
end type
type cb_retrieve from commandbutton within w_approaching_annuity_end_date_report
end type
type em_date_to from editmask within w_approaching_annuity_end_date_report
end type
type em_date_from from editmask within w_approaching_annuity_end_date_report
end type
type st_period from statictext within w_approaching_annuity_end_date_report
end type
type st_2 from statictext within w_approaching_annuity_end_date_report
end type
type dw_admin_region from datawindow within w_approaching_annuity_end_date_report
end type
type st_region from statictext within w_approaching_annuity_end_date_report
end type
type rb_detail from radiobutton within w_approaching_annuity_end_date_report
end type
type rb_summary from radiobutton within w_approaching_annuity_end_date_report
end type
type gb_1 from groupbox within w_approaching_annuity_end_date_report
end type
end forward

global type w_approaching_annuity_end_date_report from w_a_report
integer y = 49
string title = "Approaching Annuity End Date Report"
cb_retrieve cb_retrieve
em_date_to em_date_to
em_date_from em_date_from
st_period st_period
st_2 st_2
dw_admin_region dw_admin_region
st_region st_region
rb_detail rb_detail
rb_summary rb_summary
gb_1 gb_1
end type
global w_approaching_annuity_end_date_report w_approaching_annuity_end_date_report

on w_approaching_annuity_end_date_report.create
int iCurrent
call super::create
this.cb_retrieve=create cb_retrieve
this.em_date_to=create em_date_to
this.em_date_from=create em_date_from
this.st_period=create st_period
this.st_2=create st_2
this.dw_admin_region=create dw_admin_region
this.st_region=create st_region
this.rb_detail=create rb_detail
this.rb_summary=create rb_summary
this.gb_1=create gb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_retrieve
this.Control[iCurrent+2]=this.em_date_to
this.Control[iCurrent+3]=this.em_date_from
this.Control[iCurrent+4]=this.st_period
this.Control[iCurrent+5]=this.st_2
this.Control[iCurrent+6]=this.dw_admin_region
this.Control[iCurrent+7]=this.st_region
this.Control[iCurrent+8]=this.rb_detail
this.Control[iCurrent+9]=this.rb_summary
this.Control[iCurrent+10]=this.gb_1
end on

on w_approaching_annuity_end_date_report.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_retrieve)
destroy(this.em_date_to)
destroy(this.em_date_from)
destroy(this.st_period)
destroy(this.st_2)
destroy(this.dw_admin_region)
destroy(this.st_region)
destroy(this.rb_detail)
destroy(this.rb_summary)
destroy(this.gb_1)
end on

event open;call super::open;/*	Database Connections */
dw_report.SetTransObject(SQLCA)

em_date_from.setFocus()




end event

event ue_print;
u_ds ldw_summary
long ll_rows
string ls_admin_region_selected

dw_report.object.datawindow.print.orientation = 1

//if we have a detail report for all regions , we want the sumary report (which is a nested report) to be 
// printed on a separte page so we'll turn off the nested report (dw_summary) , shrink the summary 
// band, and print, then create a datastore, re-retrieve the summary data and print that separately.
// otherwise, its the stand alone summary report so we'll just print it as is

ls_admin_region_selected  = dw_admin_region.getItemString(dw_admin_region.getRow(),'admin_region_code')

IF rb_detail.checked  = TRUE AND ls_admin_region_selected = 'ALL' THEN
	dw_report.object.dw_summary.visible = 0
	dw_report.object.datawindow.Summary.height = 100
		
	dw_report.Print()
		
	ldw_summary = create u_ds
	ldw_summary.dataobject = 'd_annuity_eligibility_turning_65_summary'
	ldw_summary.settransobject(SQLCA)
	ll_rows = ldw_summary.retrieve(datetime(em_date_from.text),datetime(em_date_to.text))
	ldw_summary.Print()
	
	destroy ldw_summary
	
ELSE 
	dw_report.Print()
END IF
end event

event resize;call super::resize;call super::resize
dw_report.width = newwidth - 24
dw_report.height = newheight - 240
end event

type dw_report from w_a_report`dw_report within w_approaching_annuity_end_date_report
integer x = 14
integer y = 216
integer width = 2683
integer height = 2328
string title = "Workflow Performance Report"
string dataobject = "d_annuity_eligibility_turning_65_2line"
boolean hscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event dw_report::buttonclicked;call super::buttonclicked;if dwo.name = 'b_1' THEN
	messagebox("",  this.getitemstring(row,'annuity_calc_reason_desc'))
END IF
end event

type cb_retrieve from commandbutton within w_approaching_annuity_end_date_report
integer x = 2354
integer y = 40
integer width = 297
integer height = 116
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Retrieve"
end type

event clicked;STRING ls_admin_region_selected, ls_admin_region_desc
INT     li_cntr, li_rowcount
LONG   ll_rows
DateTime    ldt_from, ldt_to, ldt_effective_date
DataWindowChild ldwc_child

dw_admin_region.accepttext()
dw_admin_region.getChild('admin_region_code',ldwc_child)

ls_admin_region_selected  = dw_admin_region.getItemString(dw_admin_region.getRow(),'admin_region_code')

ls_admin_region_desc = ldwc_child.getitemstring(ldwc_child.getrow(), 'admin_region_desc')

IF ls_admin_region_selected = 'SELECT A REGION' THEN
	MESSAGEBOX("Region Validation", "Please select a valid region.", Information!)
	RETURN
END IF

IF NOT ISDATE(em_date_from.text) THEN 
	MESSAGEBOX("Date Validation", "Invalid From Date")
	RETURN
END IF

IF NOT ISDATE(em_date_to.text) THEN 
	MESSAGEBOX("Date Validation", "Invalid To Date")
	RETURN
END IF

ldt_from = DATETIME(em_date_from.text)
ldt_to    =  DATETIME(em_date_to.text + ' 23:59:59')

//check that 'From' date is on the first day of the month
IF DAY(DATE(ldt_from)) <> 1 THEN
	MESSAGEBOX("Date Validation", "The 'From' date must be on the first day of the month. Please re-enter a valid From Date.", Information!)
	RETURN
END IF

//check that To date is on the last day of the month
IF DAY( RelativeDate( DATE(ldt_to),1 )) <> 1 THEN
	MESSAGEBOX("Date Validation", "The 'To' date must be on the last day of the month. Please re-enter a valid To Date.", Information!)
	RETURN
END IF	

// BR1.90	The period should not cover more than two years.
IF DaysAfter(  DATE(ldt_from), DATE(ldt_to) ) > 365 * 2  THEN
	IF MESSAGEBOX("Date Validation", "The Report period exceeds 2 years. Do you want to continue?", Information!, YesNo!, 2) = 2 THEN
		RETURN
	END IF
END IF

IF DATE(ldt_from) > 2079-06-06 OR DATE(ldt_to) > 2079-06-06 THEN
	MESSAGEBOX("Date Validation","An entered date cannot be greater than 2079-06-06")
	RETURN
END IF

//check that the from date is not before the start of the  Confirm Annuity Eligibility module effective date, Oct 29, 2009 
SELECT use_confirm_annuity_eligibility_module_date
INTO :ldt_effective_date
FROM Annuity_Parameter
USING SQLCA;
SQLCA.nf_handle_error("w_approaching_annuity_end_date_report","cb_retrieve","SELECT use_confirm_annuity_eligibility_module_date from Annuity_Parameter")

IF DATE(ldt_from) < Date(ldt_effective_date)  THEN
	MESSAGEBOX("Date Validation", "The 'From' date must be " + STRING(Date(ldt_effective_date),'yyyy-mm-dd') + " or later. Please re-enter a valid From Date.", Information!)
	RETURN
END IF

IF ldt_from > ldt_to THEN
	MESSAGEBOX("Date Validation", "The 'From' date that is greater than the 'To' date. Please re-enter a valid date range.", Information!)
	RETURN
END IF


IF rb_summary.checked = TRUE THEN
	dw_report.dataobject = 'd_annuity_eligibility_turning_65_summary'
ELSE
	dw_report.dataobject = 'd_annuity_eligibility_turning_65_2line'
END IF
dw_report.setTransObject(SQLCA)


IF rb_detail.checked = TRUE THEN
	IF  ls_admin_region_selected = 'ALL' THEN
		dw_report.object.dw_summary.visible = 1		
	ELSE
		dw_report.object.dw_summary.visible = 0
		dw_report.object.datawindow.Summary.height = 0
	END IF
	dw_report.object.detail_summary_t.text = 'Detail'

ELSEIF rb_summary.checked = TRUE AND ls_admin_region_selected <> 'ALL' THEN
	dw_report.object.datawindow.Summary.height = 0
	dw_report.object.detail_summary_t.text = 'Summary'  //standalone summary report
END IF


IF ls_admin_region_selected = 'ALL'  THEN 
	dw_report.setFilter("")
	dw_report.object.region_t.text = 'ALL REGIONS'
ELSE
	dw_report.setFilter("admin_region_code = '" + ls_admin_region_selected + "'")
	dw_report.object.region_t.text = ls_admin_region_desc
END IF
	
dw_report.filter()
li_rowcount = dw_report.retrieve(ldt_from, ldt_to)
// PR 14321 Add nf_handle error to retrieve, so if stored proedure is not found an app error occurs, insread of just 'No rows retured' messagebox
SQLCA.nf_handle_error("w_approaching_annuity_end_date_report","cb_retrieve","dw_report.retrieve")

dw_report.groupcalc()

If li_rowcount <= 0 THEN
	Messagebox("No Data", "There were no records for the specified date range and region.")
END IF
	
end event

type em_date_to from editmask within w_approaching_annuity_end_date_report
integer x = 1403
integer y = 92
integer width = 338
integer height = 88
integer taborder = 40
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

type em_date_from from editmask within w_approaching_annuity_end_date_report
integer x = 933
integer y = 92
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

type st_period from statictext within w_approaching_annuity_end_date_report
integer x = 937
integer y = 20
integer width = 215
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
string text = "Period:"
boolean focusrectangle = false
end type

type st_2 from statictext within w_approaching_annuity_end_date_report
integer x = 1289
integer y = 108
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

type dw_admin_region from datawindow within w_approaching_annuity_end_date_report
integer x = 23
integer y = 92
integer width = 850
integer height = 88
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "d_admin_region_select"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event constructor;
datawindowchild adw_child
long ll_row
string ls_user_profile_admin_region

this.setTransObject(SQLCA)
this.retrieve()

this.getChild('admin_region_code', adw_child)

adw_child.settransobject(SQLCA)
adw_child.retrieve()
ls_user_profile_admin_region = vgst_user_profile.default_admin_region_code
IF trim(ls_user_profile_admin_region) = '' THEN ls_user_profile_admin_region = 'xx'    // dummy admin region code so it wont find 'UNKNOWN/ UNASSIGNED in next line

ll_row = adw_child.find("admin_region_code = '" + ls_user_profile_admin_region + "'", 1, adw_child.rowcount())

IF ll_row > 0 THEN
	this.setRow(ll_row)
	this.scrolltorow(ll_row)	
ELSE
	// if user has no default admin region code then insert a row, user will have to select a region
	ll_row = this.insertrow(0)
	this.setItem(ll_row,'admin_region_code',"SELECT A REGION")
	this.setRow(ll_row)
	this.scrolltorow(ll_row)
END IF
end event

type st_region from statictext within w_approaching_annuity_end_date_report
integer x = 41
integer y = 20
integer width = 270
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

type rb_detail from radiobutton within w_approaching_annuity_end_date_report
integer x = 1865
integer y = 28
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

type rb_summary from radiobutton within w_approaching_annuity_end_date_report
integer x = 1865
integer y = 116
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

event clicked;
//dw_admin_region.setitem(1,'admin_region_code','ALL')

end event

type gb_1 from groupbox within w_approaching_annuity_end_date_report
boolean visible = false
integer x = 1806
integer width = 475
integer height = 228
integer taborder = 20
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

