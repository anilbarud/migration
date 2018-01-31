$PBExportHeader$w_electronic_form67_report.srw
$PBExportComments$Gives information on electronic/paper form67's
forward
global type w_electronic_form67_report from w_a_report
end type
type cb_ok from commandbutton within w_electronic_form67_report
end type
type rb_all_employers from radiobutton within w_electronic_form67_report
end type
type rb_by_employer_no from radiobutton within w_electronic_form67_report
end type
type em_employer_no from editmask within w_electronic_form67_report
end type
type cb_extract from commandbutton within w_electronic_form67_report
end type
end forward

global type w_electronic_form67_report from w_a_report
integer height = 2740
string title = "Electronic Form67 Information"
event ue_post_open ( )
cb_ok cb_ok
rb_all_employers rb_all_employers
rb_by_employer_no rb_by_employer_no
em_employer_no em_employer_no
cb_extract cb_extract
end type
global w_electronic_form67_report w_electronic_form67_report

type variables

 n_resize inv_resize

end variables

forward prototypes
public function string wf_replace_string (string as_string)
end prototypes

public function string wf_replace_string (string as_string);STRING ls_return

ls_return = as_string

f_replace_text(ls_return,'"','')

RETURN ls_return

end function

event open;call super::open;IF IsNull(inv_resize) OR NOT IsValid (inv_resize) THEN
	inv_resize = create n_resize
	inv_resize.of_SetOrigSize (2729,2580)
END IF

//THIS.inv_resize.of_register(dw_report,'scaletoright')
THIS.inv_resize.of_register(dw_report,0,0,100,100)
//THIS.inv_resize.of_register(dw_totals,0,0,100,100)
//THIS.inv_resize.of_register(st_splitbar_1,'scaletoright')

/* SPLIT BAR ON WINDOW */
//st_splitbar_1.of_Register(dw_report)
//st_splitbar_1.of_Register(dw_totals)

THIS.postevent('ue_postopen')


end event

on w_electronic_form67_report.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.rb_all_employers=create rb_all_employers
this.rb_by_employer_no=create rb_by_employer_no
this.em_employer_no=create em_employer_no
this.cb_extract=create cb_extract
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.rb_all_employers
this.Control[iCurrent+3]=this.rb_by_employer_no
this.Control[iCurrent+4]=this.em_employer_no
this.Control[iCurrent+5]=this.cb_extract
end on

on w_electronic_form67_report.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.rb_all_employers)
destroy(this.rb_by_employer_no)
destroy(this.em_employer_no)
destroy(this.cb_extract)
end on

event resize;call super::resize;
long ll_workspacewidth,ll_workspaceheight


// Notify the resize service that the window size has changed.
ll_workspacewidth = This.WorkSpaceWidth()
ll_workspaceheight = This.WorkSpaceHeight()

If IsValid (inv_resize) Then
	inv_resize.Event pfc_Resize (sizetype,ll_workspacewidth  , ll_workspaceheight )
End If
end event

event ue_postopen;call super::ue_postopen;//cb_ok.triggerevent('clicked')
end event

event rbuttondown;call super::rbuttondown;///*	Create the menu -	Note that this only gives default options.  If you want 
//   additional options, you should override the ancestor and visible the options you desire.
//*/
m_dw_online_rmb_popup					lm_popup
//
//// make sure there is a valid row
////IF ISNULL(row) OR row = 0 THEN RETURN
//
IF isvalid(lm_popup) THEN DESTROY lm_popup

lm_popup = CREATE m_dw_online_rmb_popup
lm_popup.mf_set_datawindow(dw_report)
lm_popup.m_options.m_sort.visible                  = TRUE	
lm_popup.m_options.PopMenu(w_frame.PointerX(), w_frame.PointerY())
end event

type dw_report from w_a_report`dw_report within w_electronic_form67_report
integer x = 0
integer y = 132
integer width = 2725
integer height = 2420
integer taborder = 60
string dataobject = "d_electronic_form67_info"
boolean hscrollbar = true
end type

event dw_report::rbuttondown;/*	Create the menu -	Note that this only gives default options.  If you want 
   additional options, you should override the ancestor and visible the options you desire.
*/
m_dw_online_rmb_popup					lm_popup

// make sure there is a valid row
IF ISNULL(row) OR row = 0 THEN RETURN

IF isvalid(lm_popup) THEN DESTROY lm_popup

lm_popup = CREATE m_dw_online_rmb_popup
lm_popup.mf_set_datawindow(THIS)
lm_popup.m_options.m_sort.visible                  = TRUE	
lm_popup.m_options.PopMenu(w_frame.PointerX(), w_frame.PointerY())


end event

event dw_report::ue_print;THIS.print()
end event

type cb_ok from commandbutton within w_electronic_form67_report
integer x = 1641
integer y = 24
integer width = 343
integer height = 92
integer taborder = 50
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Retrieve"
end type

event clicked;LONG		ll_numrows
STRING	ls_from_month, ls_to_month, ls_month[12]
INT		li_from_month, li_to_month, li_from_year, li_to_year, li_n, li_year, li_x, li_month
Long      ll_employer_no

dw_report.setredraw(false)
//do the retrieve

dw_report.settransobject(sqlca)

if rb_by_employer_no.checked THEN
	ll_employer_no = long(em_employer_no.text)
	IF isnull (ll_employer_no) or not ll_employer_no >= 1 THEN
		MESSAGEBOX("Invalid Argument", "The Employer Number entered must be a valid positive number")
		Return 0
	END IF
ELSE
	ll_employer_no = 0
END IF

ll_numrows = dw_report.Retrieve(ll_employer_no)  // stored procedure will check for a zero, and if 0 is passed in, will get all eligible employers

// Set report date parameters & month-year labels - P10151-114
//
// Report covers the 12 month period ending the month before the current month. For example, if the report is run in July '09, results from
// July '08 through June '09 are included.

DO UNTIL li_n > 12
	CHOOSE CASE li_n
		CASE 1
			ls_month[li_n] = 'Jan'
		CASE 2
			ls_month[li_n] = 'Feb'
		CASE 3
			ls_month[li_n] = 'March'
		CASE 4
			ls_month[li_n] = 'April'
		CASE 5
			ls_month[li_n] = 'May'
		CASE 6
			ls_month[li_n] = 'June'
		CASE 7
			ls_month[li_n] = 'July'
		CASE 8
			ls_month[li_n] = 'Aug'
		CASE 9
			ls_month[li_n] = 'Sept'
		CASE 10
			ls_month[li_n] = 'Oct'
		CASE 11
			ls_month[li_n] = 'Nov'
		CASE 12
			ls_month[li_n] = 'Dec'
	END CHOOSE
	li_n ++
LOOP

IF Month(Today()) = 1 THEN
//	If running report in Jan., report covers Jan. - Dec. of the previous year - both 'from' & 'to' dates in previous year
	li_from_month = 1
	li_to_month = 12
	li_from_year = Year(Today()) - 1
	li_to_year = li_from_year
ELSE
//	If running report in any month other than Jan., the report overlaps previous & current years - 'from' date in previous year, 'to' date in current year
	li_from_month = Month(Today())
	li_to_month = Month(Today()) - 1
	li_from_year = Year(Today()) -1
	li_to_year = Year(Today())
END IF

// Set Report Parameters text in report header
dw_report.Object.t_dates.text = ls_month[li_from_month] + ' ' + String(li_from_year) + ' to ' + ls_month[li_to_month] + ' ' + String(li_to_year)

// Set up month-year labels
li_year = li_from_year
li_month = li_from_month
li_x = 1

DO UNTIL li_x > 12	// 12 labels

//	If previous month was Dec. then need to change the year
	IF li_month > 12 THEN
		li_month = 1
		li_year = li_from_year + 1
	END IF
	
// Set text for labels
	CHOOSE CASE li_x
		CASE 1
			dw_report.Object.t_ym1.text = ls_month[li_month] + '~n' + string(li_year)
		CASE 2
			dw_report.Object.t_ym2.text = ls_month[li_month] + '~n' + string(li_year)
		CASE 3
			dw_report.Object.t_ym3.text = ls_month[li_month] + '~n' + string(li_year)
		CASE 4
			dw_report.Object.t_ym4.text = ls_month[li_month] + '~n' + string(li_year)
		CASE 5
			dw_report.Object.t_ym5.text = ls_month[li_month] + '~n' + string(li_year)		
		CASE 6
			dw_report.Object.t_ym6.text = ls_month[li_month] + '~n' + string(li_year)
		CASE 7
			dw_report.Object.t_ym7.text = ls_month[li_month] + '~n' + string(li_year)
		CASE 8
			dw_report.Object.t_ym8.text = ls_month[li_month] + '~n' + string(li_year)
		CASE 9
			dw_report.Object.t_ym9.text = ls_month[li_month] + '~n' + string(li_year)
		CASE 10
			dw_report.Object.t_ym10.text = ls_month[li_month] + '~n' + string(li_year)
		CASE 11
			dw_report.Object.t_ym11.text = ls_month[li_month] + '~n' + string(li_year)
		CASE 12
			dw_report.Object.t_ym12.text = ls_month[li_month] + '~n' + string(li_year)
		END CHOOSE
			
	li_month = li_month + 1
	li_x ++
LOOP
	

SQLCA.nf_handle_error("w_electronic_form67_report","dw_report","cb_ok")


dw_report.GroupCalc()

dw_report.setredraw(true)


end event

type rb_all_employers from radiobutton within w_electronic_form67_report
integer x = 178
integer y = 28
integer width = 434
integer height = 76
integer taborder = 10
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "All Employers"
boolean checked = true
end type

event clicked;em_employer_no.clear()
em_employer_no.text = ''
em_employer_no.enabled = false
end event

type rb_by_employer_no from radiobutton within w_electronic_form67_report
integer x = 690
integer y = 28
integer width = 421
integer height = 76
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Employer No:"
end type

event clicked;em_employer_no.enabled = true
em_employer_no.setFocus()
em_employer_no.clear()
end event

type em_employer_no from editmask within w_electronic_form67_report
integer x = 1143
integer y = 24
integer width = 448
integer height = 84
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
string text = "none"
borderstyle borderstyle = stylelowered!
string mask = "######"
end type

type cb_extract from commandbutton within w_electronic_form67_report
integer x = 2368
integer y = 24
integer width = 343
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Extract"
end type

event clicked;INTEGER	li_total_elec_by_op,  li_total_paper_by_op, li_total_ws_by_op, li_total_all_by_op, li_counter
STRING 	ls_header

/*																*/
/*	Arguments:			String	-	vas_string		*/
/*							String	-	vas_old_arg		*/
/*							String	-	vas_new_arg		*/

/* electronic,       WorkSafe,   Paper          */

/* Extracting the Data */
IF dw_report.RowCount() = 0 THEN
	MessageBox('No rows','There is no Report Data to extract.')
	RETURN
END IF

ls_header = wf_replace_string(dw_report.Object.t_ym1.text)
dw_report.Object.electronic_month_1.dbName  =  ls_header  + '_E'
dw_report.Object.paper_month_1.dbName       =  ls_header  + '_P'
dw_report.Object.ws_month_1.dbName          =  ls_header  + '_W'

ls_header = wf_replace_string(dw_report.Object.t_ym2.text)
dw_report.Object.electronic_month_2.dbName  = ls_header  + '_E' 
dw_report.Object.paper_month_2.dbName       = ls_header  + '_P' 
dw_report.Object.ws_month_2.dbName          = ls_header  + '_W' 

ls_header = wf_replace_string(dw_report.Object.t_ym3.text)
dw_report.Object.electronic_month_3.dbName  = ls_header  + '_E' 
dw_report.Object.paper_month_3.dbName       = ls_header  + '_P'
dw_report.Object.ws_month_3.dbName          = ls_header  + '_W'

ls_header = wf_replace_string(dw_report.Object.t_ym4.text)
dw_report.Object.electronic_month_4.dbName  = ls_header  + '_E' 
dw_report.Object.paper_month_4.dbName       = ls_header  + '_P'
dw_report.Object.ws_month_4.dbName          = ls_header  + '_W' 

ls_header = wf_replace_string(dw_report.Object.t_ym5.text)
dw_report.Object.electronic_month_5.dbName  = ls_header  + '_E' 
dw_report.Object.paper_month_5.dbName       = ls_header  + '_P'
dw_report.Object.ws_month_5.dbName          = ls_header  + '_W' 

ls_header = wf_replace_string(dw_report.Object.t_ym6.text)
dw_report.Object.electronic_month_6.dbName  = ls_header  + '_E' 
dw_report.Object.paper_month_6.dbName       = ls_header  + '_P'
dw_report.Object.ws_month_6.dbName          = ls_header  + '_W' 

ls_header = wf_replace_string(dw_report.Object.t_ym7.text)
dw_report.Object.electronic_month_7.dbName  = ls_header  + '_E' 
dw_report.Object.paper_month_7.dbName       = ls_header  + '_P'
dw_report.Object.ws_month_7.dbName          = ls_header  + '_W' 

ls_header = wf_replace_string(dw_report.Object.t_ym8.text)
dw_report.Object.electronic_month_8.dbName  = ls_header  + '_E' 
dw_report.Object.paper_month_8.dbName       = ls_header  + '_P'
dw_report.Object.ws_month_8.dbName          = ls_header  + '_W' 

ls_header = wf_replace_string(dw_report.Object.t_ym9.text)
dw_report.Object.electronic_month_9.dbName  = ls_header  + '_E'
dw_report.Object.paper_month_9.dbName       = ls_header  + '_P' 
dw_report.Object.ws_month_9.dbName          = ls_header  + '_W' 

ls_header = wf_replace_string(dw_report.Object.t_ym10.text)
dw_report.Object.electronic_month_10.dbName = ls_header  + '_E'
dw_report.Object.paper_month_10.dbName      = ls_header  + '_P' 
dw_report.Object.ws_month_10.dbName         = ls_header  + '_W'

ls_header = wf_replace_string(dw_report.Object.t_ym11.text)
dw_report.Object.electronic_month_11.dbName = ls_header  + '_E'
dw_report.Object.paper_month_11.dbName      = ls_header  + '_P' 
dw_report.Object.ws_month_11.dbName         = ls_header  + '_W' 

ls_header = wf_replace_string(dw_report.Object.t_ym12.text)
dw_report.Object.electronic_month_12.dbName = ls_header  + '_E'
dw_report.Object.paper_month_12.dbName      = ls_header  + '_P'
dw_report.Object.ws_month_12.dbName         = ls_header  + '_W'

     
FOR li_counter = 1 TO dw_report.rowcount()
	
	li_total_elec_by_op = dw_report.getitemnumber(li_counter,'total_by_emp_electronic')
	li_total_paper_by_op = dw_report.getitemnumber(li_counter,'total_by_emp_paper')
	li_total_ws_by_op = dw_report.getitemnumber(li_counter,'total_by_worksafe')

	li_total_all_by_op = li_total_elec_by_op + li_total_paper_by_op + li_total_ws_by_op
	
	dw_report.setitem(li_counter, 'total_elec_by_op',  li_total_elec_by_op)
	dw_report.setitem(li_counter, 'total_paper_by_op', li_total_paper_by_op)
	dw_report.setitem(li_counter, 'total_ws_by_op',    li_total_ws_by_op)
	dw_report.setitem(li_counter, 'total_all_by_op',   li_total_all_by_op)
			
NEXT                                              

dw_report.Saveas('',Excel!,TRUE)
end event

