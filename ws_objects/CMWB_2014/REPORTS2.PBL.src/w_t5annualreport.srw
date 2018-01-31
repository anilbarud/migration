$PBExportHeader$w_t5annualreport.srw
forward
global type w_t5annualreport from w_a_report
end type
type dw_enter_date_range from u_dw_online within w_t5annualreport
end type
type rb_nonclaimant from radiobutton within w_t5annualreport
end type
type rb_annuitypayouts from radiobutton within w_t5annualreport
end type
type cb_retrieve from commandbutton within w_t5annualreport
end type
type st_refreshmessage from statictext within w_t5annualreport
end type
type cb_extract from commandbutton within w_t5annualreport
end type
type gb_1 from groupbox within w_t5annualreport
end type
type gb_reportcategory from groupbox within w_t5annualreport
end type
end forward

global type w_t5annualreport from w_a_report
integer height = 2960
string title = "T5 Review Reports"
event ue_changedstate ( )
dw_enter_date_range dw_enter_date_range
rb_nonclaimant rb_nonclaimant
rb_annuitypayouts rb_annuitypayouts
cb_retrieve cb_retrieve
st_refreshmessage st_refreshmessage
cb_extract cb_extract
gb_1 gb_1
gb_reportcategory gb_reportcategory
end type
global w_t5annualreport w_t5annualreport

type variables
private:

n_cst_t5annualreport ireport
end variables

forward prototypes
public subroutine wf_reportselected ()
public subroutine wf_modified ()
private subroutine wf_setneedrefresh (boolean abneedrefresh)
public subroutine wf_datenotvalid ()
private subroutine wf_resetdates ()
end prototypes

public subroutine wf_reportselected ();/*

Sven Oborn
April 13/2011

Called dynamically from n_cst_t5annualreport when report is selected

*/

wf_resetdates()
this.dw_report.modify("t_reportparameters.text='" + ireport.of_getreportparameterstext( ) + "' Datawindow.Print.Preview=Yes")

end subroutine

public subroutine wf_modified ();/*

Sven Oborn
April 13/2011


called when a date value is modified

*/
this.wf_setneedrefresh( ireport.of_needrefresh())


end subroutine

private subroutine wf_setneedrefresh (boolean abneedrefresh);/*

Sven Oborn
April 13/2011

If the report dates is inconsistent with the entered dates, signal to user that data needs to be retrieved again

*/
this.cb_retrieve.enabled = abneedrefresh

boolean lbshowmessage// only show message if there are rows in table
lbshowmessage = abneedrefresh
if abneedrefresh and dw_report.rowcount() <= 0  then
	lbshowmessage = false
end if
this.st_refreshmessage.visible = lbshowmessage

end subroutine

public subroutine wf_datenotvalid ();/*

Sven Oborn
April 18/2011

called from n_cst_t5annualreport if the date passed to of_handledateedit is not valid

*/

//wf_reportselected()
wf_resetdates()
end subroutine

private subroutine wf_resetdates ();//

/*

Sven Oborn
April 13/2011

Reset the dates to those of the report

*/

this.dw_enter_date_range.object.from_date[1] = ireport.of_getdate(true)
this.dw_enter_date_range.object.to_date[1] = ireport.of_getdate(false)

this.wf_setneedrefresh( ireport.of_needrefresh())


end subroutine

on w_t5annualreport.create
int iCurrent
call super::create
this.dw_enter_date_range=create dw_enter_date_range
this.rb_nonclaimant=create rb_nonclaimant
this.rb_annuitypayouts=create rb_annuitypayouts
this.cb_retrieve=create cb_retrieve
this.st_refreshmessage=create st_refreshmessage
this.cb_extract=create cb_extract
this.gb_1=create gb_1
this.gb_reportcategory=create gb_reportcategory
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_enter_date_range
this.Control[iCurrent+2]=this.rb_nonclaimant
this.Control[iCurrent+3]=this.rb_annuitypayouts
this.Control[iCurrent+4]=this.cb_retrieve
this.Control[iCurrent+5]=this.st_refreshmessage
this.Control[iCurrent+6]=this.cb_extract
this.Control[iCurrent+7]=this.gb_1
this.Control[iCurrent+8]=this.gb_reportcategory
end on

on w_t5annualreport.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.dw_enter_date_range)
destroy(this.rb_nonclaimant)
destroy(this.rb_annuitypayouts)
destroy(this.cb_retrieve)
destroy(this.st_refreshmessage)
destroy(this.cb_extract)
destroy(this.gb_1)
destroy(this.gb_reportcategory)
end on

event open;call super::open;
dw_enter_date_range.insertrow(0)

// initialize report service
ireport = create n_cst_t5annualreport
ireport.of_handleopen(dw_report)

// get current report
integer liselectedreport
liselectedreport = ireport.of_getselectedreport()
if liselectedreport = ireport.nonclaimant then
	rb_annuitypayouts.checked = false
	rb_nonclaimant.checked = true
elseif liselectedreport = ireport.annuitypayout then
	rb_nonclaimant.checked = true
	rb_annuitypayouts.checked = false
end if

// get initial dates
this.dw_enter_date_range.object.from_date[1] = ireport.of_getdate(true)
this.dw_enter_date_range.object.to_date[1] = ireport.of_getdate(false)

this.wf_setneedrefresh( ireport.of_needrefresh())
//this.cb_retrieve.enabled = ireport.of_needrefresh()


end event

event close;call super::close;destroy ireport
end event

event closequery;call super::closequery;ireport.of_handleclosing()

end event

type dw_report from w_a_report`dw_report within w_t5annualreport
string title = "T5 Review Report"
boolean hscrollbar = true
boolean livescroll = false
end type

type dw_enter_date_range from u_dw_online within w_t5annualreport
integer x = 1344
integer y = 196
integer width = 1239
integer height = 92
integer taborder = 20
boolean bringtotop = true
string dataobject = "d_enter_date_range"
boolean border = false
end type

event editchanged;call super::editchanged;//ireport.of_handlereportselection(ireport.nonclaimant)
//ireport.annuitypayout
if row = 1 then
	if dwo.name = "from_date" then
		ireport.of_handledateedit(true, data)
	elseif dwo.name = "to_date" then
		ireport.of_handledateedit(false, data)
	end if
end if
end event

event itemerror;call super::itemerror;integer lii
lii = 1
return 1
end event

type rb_nonclaimant from radiobutton within w_t5annualreport
integer x = 105
integer y = 172
integer width = 1079
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
string text = "Payments to Recipient Type ~'Other~'"
end type

event clicked;ireport.of_handlereportselection(ireport.nonclaimant)
end event

type rb_annuitypayouts from radiobutton within w_t5annualreport
integer x = 105
integer y = 272
integer width = 741
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
string text = "Annuity Payouts in Period"
end type

event clicked;
ireport.of_handlereportselection(ireport.annuitypayout)
end event

type cb_retrieve from commandbutton within w_t5annualreport
integer x = 2272
integer y = 432
integer width = 402
integer height = 104
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Retrieve"
end type

event clicked;SetPointer(HourGlass!)
parent.wf_setneedrefresh(false)
ireport.of_handleretrieveclick()
// modify report datawindow
parent.dw_report.modify("t_reportparameters.text='" + ireport.of_getreportparameterstext( ) + "'")
SetPointer(Arrow!)

end event

type st_refreshmessage from statictext within w_t5annualreport
integer x = 1335
integer y = 320
integer width = 1280
integer height = 60
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 255
long backcolor = 67108864
string text = "Please retrieve data to reflect modified reporting period"
boolean focusrectangle = false
end type

event constructor;this.visible = false
end event

type cb_extract from commandbutton within w_t5annualreport
integer x = 1755
integer y = 432
integer width = 503
integer height = 104
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Extract to Excel"
end type

event clicked;dw_report.SaveAs('',Excel8!,TRUE,EncodingANSI! )
end event

type gb_1 from groupbox within w_t5annualreport
integer x = 1298
integer y = 80
integer width = 1376
integer height = 336
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Enter Reporting Period"
end type

type gb_reportcategory from groupbox within w_t5annualreport
integer x = 46
integer y = 80
integer width = 1198
integer height = 332
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Report Selection"
end type

