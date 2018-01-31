$PBExportHeader$w_coc_summary_by_gl_account.srw
forward
global type w_coc_summary_by_gl_account from w_a_report
end type
type dw_criteria from u_dw_online within w_coc_summary_by_gl_account
end type
type cb_retrieve from commandbutton within w_coc_summary_by_gl_account
end type
type pb_filter_like from picturebutton within w_coc_summary_by_gl_account
end type
type pb_filter_not_like from picturebutton within w_coc_summary_by_gl_account
end type
type pb_toggle_filter from picturebutton within w_coc_summary_by_gl_account
end type
type st_1 from statictext within w_coc_summary_by_gl_account
end type
type st_filter from multilineedit within w_coc_summary_by_gl_account
end type
end forward

global type w_coc_summary_by_gl_account from w_a_report
string title = "Cost of Claims Summary by Gl Account"
dw_criteria dw_criteria
cb_retrieve cb_retrieve
pb_filter_like pb_filter_like
pb_filter_not_like pb_filter_not_like
pb_toggle_filter pb_toggle_filter
st_1 st_1
st_filter st_filter
end type
global w_coc_summary_by_gl_account w_coc_summary_by_gl_account

type variables
Date        id_current_date
end variables

forward prototypes
public function date wf_first_monthday (date ld_date)
public function date wf_last_monthday (date ld_date)
end prototypes

public function date wf_first_monthday (date ld_date);

RETURN Date(Year(ld_date),Month(ld_date),1)
end function

public function date wf_last_monthday (date ld_date);LONG		ll_year
LONG		ll_month

Date		ld_first_day_next_month

ll_year = Year(ld_date)
ll_month = Month(ld_date)

IF ll_month <> 12 THen
	ld_first_day_next_month = Date(ll_year,ll_month + 1,1)
Else
	ll_year = ll_year + 1
	ll_month = 1
	ld_first_day_next_month = Date(ll_year,ll_month,1)
End if

return RelativeDate(ld_first_day_next_month,-1)
end function

on w_coc_summary_by_gl_account.create
int iCurrent
call super::create
this.dw_criteria=create dw_criteria
this.cb_retrieve=create cb_retrieve
this.pb_filter_like=create pb_filter_like
this.pb_filter_not_like=create pb_filter_not_like
this.pb_toggle_filter=create pb_toggle_filter
this.st_1=create st_1
this.st_filter=create st_filter
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_criteria
this.Control[iCurrent+2]=this.cb_retrieve
this.Control[iCurrent+3]=this.pb_filter_like
this.Control[iCurrent+4]=this.pb_filter_not_like
this.Control[iCurrent+5]=this.pb_toggle_filter
this.Control[iCurrent+6]=this.st_1
this.Control[iCurrent+7]=this.st_filter
end on

on w_coc_summary_by_gl_account.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.dw_criteria)
destroy(this.cb_retrieve)
destroy(this.pb_filter_like)
destroy(this.pb_filter_not_like)
destroy(this.pb_toggle_filter)
destroy(this.st_1)
destroy(this.st_filter)
end on

event open;call super::open;
id_current_date = Date(f_server_datetime())

dw_criteria.InsertRow(0)
dw_criteria.SetItem(1,'year',Year(id_current_date))
dw_criteria.SetItem(1,'month',Month(id_current_date))
dw_criteria.SetItem(1,'from_date',wf_first_monthday(id_current_date))
dw_criteria.SetItem(1,'to_date',id_current_date)

end event

type dw_report from w_a_report`dw_report within w_coc_summary_by_gl_account
integer x = 0
integer y = 216
integer width = 2688
integer height = 2160
integer taborder = 30
string dataobject = "d_coc_summary_by_gl_account"
borderstyle borderstyle = stylelowered!
end type

event dw_report::constructor;call super::constructor;This.uf_SetSort(True)
This.uf_SetFilter(True)

This.SetTransObject(SQLCA)
end event

event dw_report::rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup
	
	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	
	lm_popup.m_options.m_saveas.visible = True
	lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))
	
end event

type dw_criteria from u_dw_online within w_coc_summary_by_gl_account
integer y = 8
integer width = 2272
integer height = 180
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_pick_from_to_dates"
boolean border = false
borderstyle borderstyle = stylelowered!
end type

event itemchanged;call super::itemchanged;LONG		ll_year
LONG		ll_month
Date		ld_from_date
Date		ld_to_date


ll_year = dw_criteria.GetItemNumber(1,'year')
ll_month = dw_criteria.GetItemNumber(1,'month')

CHOOSE CASE dwo.name
	CASE 'year'
		ld_from_date = Date(Integer(data),ll_month,1)
		ld_to_date = wf_last_monthday(ld_from_date)
		
		dw_criteria.SetItem(1,'from_date',ld_from_date)
		dw_criteria.SetItem(1,'to_date',ld_to_date)
	CASE 'month'
		
		ld_from_date = Date(ll_year,Integer(data),1)
		ld_to_date = wf_last_monthday(ld_from_date)
		
		dw_criteria.SetItem(1,'from_date',ld_from_date)
		dw_criteria.SetItem(1,'to_date',ld_to_date)
END CHOOSE
end event

type cb_retrieve from commandbutton within w_coc_summary_by_gl_account
integer x = 2327
integer y = 60
integer width = 311
integer height = 92
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Retrieve"
boolean default = true
end type

event clicked;DateTime		ldt_from_date
DateTime		ldt_to_date

DateTime    ldt_parameter_from_date
DateTime    ldt_parameter_to_date
LONG			ll_rows


SetPointer(Hourglass!)
dw_criteria.AcceptText()

ldt_from_date = dw_criteria.GetItemDateTime(1,'from_date')
//Validate the FROM date
IF IsNull(ldt_from_date) Then
	MessageBox('From date','Please enter a from date.')
	dw_criteria.SetColumn("from_date")
	dw_criteria.SetFocus()
	RETURN -1
ELSEIF Date(ldt_from_date) < Date('1980-01-01') THEN
	MessageBox('From date','From dates earlier than "1980-01-01" are not valid.')
	dw_criteria.SetColumn("from_date")
	dw_criteria.SetFocus()
	RETURN -1
ELSEIF Date(ldt_from_date) > Today() THEN
	MessageBox('From date','The "From date" cannot be in the future.')
	dw_criteria.SetColumn("from_date")
	dw_criteria.SetFocus()
	RETURN -1
END IF


ldt_to_date = dw_criteria.GetItemDateTime(1,'to_date')
//Validate the TO date
IF IsNUll(ldt_to_date) Then
	MessageBox('To date','Please enter a to date.')
	dw_criteria.SetColumn("to_date")
	dw_criteria.SetFocus()
	RETURN -1
ELSEIF Date(ldt_to_date) < Date('1980-01-01') THEN
	MessageBox('To date','To dates earlier than "1980-01-01" are not valid.')
	dw_criteria.SetColumn("to_date")
	dw_criteria.SetFocus()
	RETURN -1
ELSEIF Date(ldt_to_date) > Today() THEN
	MessageBox('To date','The "To date" cannot be in the future.')
	dw_criteria.SetColumn("to_date")
	dw_criteria.SetFocus()
	RETURN -1
END IF

IF ldt_from_date > ldt_to_date Then
	MessageBox('Date range','The "From date" cannot be after the "to date".')
	RETURN -1
END IF


//We have to add one day to the "to date" for the query to work properly
ldt_parameter_from_date = ldt_from_date
ldt_parameter_to_date = DateTime(RelativeDate(Date(ldt_to_date),1))


ll_rows = dw_report.Retrieve(ldt_parameter_from_date,ldt_parameter_to_date)
SQLCA.nf_handle_error('w_coc_summary_by_gl_account','cb_retrieve.clicked','dw_report.retrieve')


If ll_rows < 0 Then SignalError(-666,'Error retrieving gl accounts')

IF ll_rows = 0 Then
	MessageBox('No rows','No rows retrieved for this date range.')
	RETURN
END IF
end event

type pb_filter_like from picturebutton within w_coc_summary_by_gl_account
integer x = 41
integer y = 2416
integer width = 119
integer height = 100
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "filter_like.bmp"
alignment htextalign = left!
string powertiptext = "Include all records like selected field."
end type

event clicked;If dw_report.RowCount() > 0 Then
	st_filter.text = dw_report.inv_filter.of_filter_selection('like')
	pb_toggle_filter.enabled = True	
Else
	MessageBox('No records','There is nothing to filter.')
End if
end event

type pb_filter_not_like from picturebutton within w_coc_summary_by_gl_account
integer x = 160
integer y = 2416
integer width = 119
integer height = 100
integer taborder = 50
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "filter_not_like.bmp"
alignment htextalign = left!
string powertiptext = "Include all records except selected field."
end type

event clicked;If dw_report.RowCount() > 0 Then
	dw_report.SelectRow(dw_report.GetRow(),False)
	st_filter.text = dw_report.inv_filter.of_filter_selection('notlike')
	pb_toggle_filter.enabled = True
//	If dw_report.GetSelectedRow(0) = 0 Then
//		dw_report.SelectRow(1,True)
//	End if
Else
	MessageBox('No records','There is nothing to filter.')
End if
end event

type pb_toggle_filter from picturebutton within w_coc_summary_by_gl_account
integer x = 279
integer y = 2416
integer width = 119
integer height = 100
integer taborder = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "filtered.bmp"
string disabledname = "filtered_disabled.bmp"
alignment htextalign = left!
string powertiptext = "Toggle Filter"
end type

event clicked;STRING		ls_applied_filter


ls_applied_filter = dw_report.inv_filter.of_Toggle_Filter()
st_filter.text = ls_applied_filter
If ls_applied_filter = '' Then
	dw_report.SetRow(dw_report.GetSelectedRow(0))
End if

end event

type st_1 from statictext within w_coc_summary_by_gl_account
integer x = 434
integer y = 2384
integer width = 146
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Filter:"
boolean focusrectangle = false
end type

type st_filter from multilineedit within w_coc_summary_by_gl_account
integer x = 590
integer y = 2384
integer width = 2103
integer height = 148
integer taborder = 70
boolean bringtotop = true
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean vscrollbar = true
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

