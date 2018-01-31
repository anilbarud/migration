$PBExportHeader$w_annuity_work_in_progress_report.srw
forward
global type w_annuity_work_in_progress_report from w_a_report
end type
type gb_1 from groupbox within w_annuity_work_in_progress_report
end type
type dw_admin_region from datawindow within w_annuity_work_in_progress_report
end type
type cb_retrieve from commandbutton within w_annuity_work_in_progress_report
end type
type st_1 from statictext within w_annuity_work_in_progress_report
end type
type rb_detail from radiobutton within w_annuity_work_in_progress_report
end type
type rb_summary from radiobutton within w_annuity_work_in_progress_report
end type
end forward

global type w_annuity_work_in_progress_report from w_a_report
integer y = 49
integer width = 2821
string title = "Annuity Work In Progress Report"
gb_1 gb_1
dw_admin_region dw_admin_region
cb_retrieve cb_retrieve
st_1 st_1
rb_detail rb_detail
rb_summary rb_summary
end type
global w_annuity_work_in_progress_report w_annuity_work_in_progress_report

on w_annuity_work_in_progress_report.create
int iCurrent
call super::create
this.gb_1=create gb_1
this.dw_admin_region=create dw_admin_region
this.cb_retrieve=create cb_retrieve
this.st_1=create st_1
this.rb_detail=create rb_detail
this.rb_summary=create rb_summary
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_1
this.Control[iCurrent+2]=this.dw_admin_region
this.Control[iCurrent+3]=this.cb_retrieve
this.Control[iCurrent+4]=this.st_1
this.Control[iCurrent+5]=this.rb_detail
this.Control[iCurrent+6]=this.rb_summary
end on

on w_annuity_work_in_progress_report.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.gb_1)
destroy(this.dw_admin_region)
destroy(this.cb_retrieve)
destroy(this.st_1)
destroy(this.rb_detail)
destroy(this.rb_summary)
end on

event open;call super::open;/*	Database Connections */
dw_report.SetTransObject(SQLCA)
//this.width = 2811




end event

event resize;call super::resize;call super::resize
dw_report.width = newwidth - 24
dw_report.height = newheight - 300
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
	ldw_summary.dataobject = 'd_annuity_work_in_progress_summary'
	ldw_summary.settransobject(SQLCA)
	ll_rows = ldw_summary.retrieve()
	ldw_summary.Print()
	
	destroy ldw_summary
	
ELSE
	dw_report.Print()
END IF
end event

type dw_report from w_a_report`dw_report within w_annuity_work_in_progress_report
integer x = 14
integer y = 192
integer width = 2702
integer height = 2324
string dataobject = "d_annuity_work_in_progress_detail"
boolean hscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type gb_1 from groupbox within w_annuity_work_in_progress_report
boolean visible = false
integer x = 1522
integer width = 517
integer height = 236
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
end type

type dw_admin_region from datawindow within w_annuity_work_in_progress_report
integer x = 485
integer y = 36
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

type cb_retrieve from commandbutton within w_annuity_work_in_progress_report
integer x = 2130
integer y = 32
integer width = 402
integer height = 96
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

event clicked;STRING ls_admin_region, ls_admin_region_selected
INT      li_rowcount

DataWindowChild ldwc_child

dw_admin_region.getChild('admin_region_code',ldwc_child)

ls_admin_region_selected  = dw_admin_region.getItemString(dw_admin_region.getRow(),'admin_region_code')
IF ls_admin_region_selected = '' THEN
	MESSAGEBOX("Region Validation", "Please select a region.", Information!)
	RETURN
END IF


IF rb_detail.checked = TRUE THEN
	dw_report.dataobject = 'd_annuity_work_in_progress_detail'
	IF  ls_admin_region_selected = 'ALL' THEN
		dw_report.object.dw_summary.visible = 1
		dw_report.object.DataWindow.summary.height = 1412
	ELSE
		dw_report.object.dw_summary.visible = 0
		dw_report.object.DataWindow.summary.height = 200
	END IF
END IF

IF rb_summary.checked = TRUE AND ls_admin_region_selected = 'ALL' THEN
	dw_report.dataobject = 'd_annuity_work_in_progress_summary'
ELSEIF rb_summary.checked = TRUE AND ls_admin_region_selected <> 'ALL' THEN
	dw_report.dataobject = 'd_annuity_work_in_progress_detail'
	dw_report.object.dw_summary.visible = 0
	dw_report.object.DataWindow.summary.height = 200
END IF

dw_report.setTransObject(SQLCA)

li_rowcount = dw_report.retrieve(ls_admin_region_selected)
SQLCA.nf_handle_error("w_annuity_work_in_progress_report","cb_retrieve","dw_report.retrieve")

dw_report.groupCalc()

If li_rowcount < 1 THEN
	Messagebox("No Data", "There were no records returned for the region selected.")
END IF




end event

type st_1 from statictext within w_annuity_work_in_progress_report
integer x = 169
integer y = 48
integer width = 251
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

type rb_detail from radiobutton within w_annuity_work_in_progress_report
integer x = 1541
integer y = 20
integer width = 402
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

type rb_summary from radiobutton within w_annuity_work_in_progress_report
integer x = 1541
integer y = 96
integer width = 402
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

