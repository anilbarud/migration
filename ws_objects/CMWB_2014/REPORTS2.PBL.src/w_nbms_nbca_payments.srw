$PBExportHeader$w_nbms_nbca_payments.srw
$PBExportComments$list all physicians/chiros that have received early filing and/or reporting fee payments with the total amount of early filing and reporting fee payments
forward
global type w_nbms_nbca_payments from w_a_report
end type
type dw_physicians_filing from u_dw_online within w_nbms_nbca_payments
end type
type gb_1 from groupbox within w_nbms_nbca_payments
end type
type rb_nbms from radiobutton within w_nbms_nbca_payments
end type
type rb_nbca from radiobutton within w_nbms_nbca_payments
end type
type cb_clear from commandbutton within w_nbms_nbca_payments
end type
type cb_ok from commandbutton within w_nbms_nbca_payments
end type
type rb_all from radiobutton within w_nbms_nbca_payments
end type
type gb_2 from groupbox within w_nbms_nbca_payments
end type
end forward

global type w_nbms_nbca_payments from w_a_report
integer width = 3067
integer height = 2608
string title = "NBMS Payments Report"
dw_physicians_filing dw_physicians_filing
gb_1 gb_1
rb_nbms rb_nbms
rb_nbca rb_nbca
cb_clear cb_clear
cb_ok cb_ok
rb_all rb_all
gb_2 gb_2
end type
global w_nbms_nbca_payments w_nbms_nbca_payments

type variables
LONG       il_design_time_width, il_design_time_height, il_workspace_width_diff, il_workspace_height_diff
LONG       il_normal_y, il_normal_height
N_RESIZE   inv_resize
STRING     is_report_type
end variables

forward prototypes
public subroutine wf_setresize (boolean ab_switch)
end prototypes

public subroutine wf_setresize (boolean ab_switch);

IF ab_switch = True Then
	IF il_design_time_height = 0 or il_design_time_width = 0 THEN
		SignalError(-666,'The resize service requires that both the il_design_time_height and il_design_time_width be filled in.')
	End if
	
	/* default instance of the resize object */
	IF IsNull(inv_resize) OR NOT IsValid (inv_resize) THEN
		inv_resize = create n_resize
		If this.WindowType = Child! Then
			inv_resize.of_SetOrigSize (il_design_time_width , il_design_time_height)
			inv_resize.of_SetMinSize (il_design_time_width , il_design_time_height)
		Else
			inv_resize.of_SetOrigSize (il_design_time_width - il_workspace_width_diff, il_design_time_height - il_workspace_height_diff)
			inv_resize.of_SetMinSize (il_design_time_width - il_workspace_width_diff, il_design_time_height - il_workspace_height_diff)
		End if
	END IF 
Else
	Destroy inv_resize
End if
end subroutine

on w_nbms_nbca_payments.create
int iCurrent
call super::create
this.dw_physicians_filing=create dw_physicians_filing
this.gb_1=create gb_1
this.rb_nbms=create rb_nbms
this.rb_nbca=create rb_nbca
this.cb_clear=create cb_clear
this.cb_ok=create cb_ok
this.rb_all=create rb_all
this.gb_2=create gb_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_physicians_filing
this.Control[iCurrent+2]=this.gb_1
this.Control[iCurrent+3]=this.rb_nbms
this.Control[iCurrent+4]=this.rb_nbca
this.Control[iCurrent+5]=this.cb_clear
this.Control[iCurrent+6]=this.cb_ok
this.Control[iCurrent+7]=this.rb_all
this.Control[iCurrent+8]=this.gb_2
end on

on w_nbms_nbca_payments.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.dw_physicians_filing)
destroy(this.gb_1)
destroy(this.rb_nbms)
destroy(this.rb_nbca)
destroy(this.cb_clear)
destroy(this.cb_ok)
destroy(this.rb_all)
destroy(this.gb_2)
end on

event open;call super::open;/*	Database connections and initialization.
*/

dw_report.SetTransObject (SQLCA)

dw_physicians_filing.InsertRow(0)
dw_physicians_filing.SetRow(1)
dw_physicians_filing.setfocus()
dw_physicians_filing.SetColumn("date_start")


// default instance variable
is_report_type = 'NBMS'


il_design_time_width  = 3031
il_design_time_height = 2428

This.wf_SetResize(True)

THIS.inv_resize.of_register(dw_report,'ScaleToRight&Bottom')
THIS.inv_resize.of_register(cb_ok,    'FixedToRight')
THIS.inv_resize.of_register(cb_clear, 'FixedToRight')
end event

event resize;call super::resize;LONG ll_workspacewidth,ll_workspaceheight

IF IsValid(inv_resize) THEN
	// Notify the resize service that the window size has changed.
	ll_workspacewidth  = This.WorkSpaceWidth()
	ll_workspaceheight = This.WorkSpaceHeight()

	IF IsValid (inv_resize) THEN
		inv_resize.Event pfc_Resize (sizetype,ll_workspacewidth  , ll_workspaceheight )
	END IF 
END IF 
end event

type dw_report from w_a_report`dw_report within w_nbms_nbca_payments
integer x = 50
integer y = 380
integer width = 2917
integer height = 2004
string dataobject = "d_nbms_nbca_payments"
boolean hscrollbar = true
end type

type dw_physicians_filing from u_dw_online within w_nbms_nbca_payments
integer x = 1143
integer y = 172
integer width = 1317
integer height = 108
integer taborder = 30
string dataobject = "d_bonus_parameters"
boolean border = false
end type

type gb_1 from groupbox within w_nbms_nbca_payments
integer x = 1111
integer y = 88
integer width = 1362
integer height = 208
integer taborder = 40
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Processing Period"
end type

type rb_nbms from radiobutton within w_nbms_nbca_payments
integer x = 160
integer y = 184
integer width = 247
integer height = 56
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "NBMS"
boolean checked = true
boolean lefttext = true
end type

event clicked;PARENT.Title = 'NBMS Payments Report'

is_report_type = 'NBMS'
end event

type rb_nbca from radiobutton within w_nbms_nbca_payments
integer x = 453
integer y = 184
integer width = 247
integer height = 56
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "NBCA"
boolean lefttext = true
end type

event clicked;PARENT.Title = 'NBCA Payments Report'

is_report_type = 'NBCA'
end event

type cb_clear from commandbutton within w_nbms_nbca_payments
integer x = 2565
integer y = 220
integer width = 402
integer height = 108
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Clear"
end type

event clicked;
dw_physicians_filing.Reset()
dw_physicians_filing.InsertRow(0)
dw_report.reset()
end event

type cb_ok from commandbutton within w_nbms_nbca_payments
integer x = 2565
integer y = 84
integer width = 402
integer height = 120
integer taborder = 10
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
boolean default = true
end type

event clicked;/*	Variables and initialization.
*/
	LONG 		ll_numrows
	DATE	   ad_from_date, ad_to_date
	STRING   ls_sql_statement

	dw_physicians_filing.AcceptText()
	
/* reset the report window
*/
	dw_report.reset()

/*	Validate the dates and set the to-date to midnight of the next day to ensure that all transactions
	from the requested day are included.
*/

	ad_from_date = dw_physicians_filing.GetItemDate(1,"date_start")
	ad_to_date   = dw_physicians_filing.GetItemDate(1,"date_end")

	IF IsNull(ad_from_date) or &
		IsNull(ad_to_date) THEN
		MessageBox("Validation Error","Both the from and to dates must have a value.",Exclamation!)
		Return
	END IF

	IF ad_from_date < Date(1900,01,01) OR &
		ad_to_date < Date(1900,01,01) THEN
		MessageBox("Validation Error","Dates cannot be earlier than 1900-01-01!",Exclamation!)
		Return
	END IF

	IF ad_from_date > Date(2079,06,06) OR &
		ad_to_date > Date(2079,06,06) THEN
		MessageBox("Validation Error","Dates cannot be later than 2079-06-06!",Exclamation!)
		Return
	END IF
	
	IF ad_from_date >= ad_to_date then
		MessageBox("Validation Error","The to date must be later than the from date.",Exclamation!)
		Return
	END IF
	

/*	Retrieve the report. But first need to close the current transaction. This is because the 
	retrieve uses a stored procedure that makes use of temporary tables and they can not be 
	created in an open transaction (SQL Server release 4.2).
*/
	
	dw_report.settransobject(sqlca)
	ll_numrows = dw_report.Retrieve(datetime(ad_from_date),datetime(ad_to_date),is_report_type)
	SQLCA.nf_handle_error('dw_report.Retrieve','w_nbms_report','cb_ok')


	IF ll_numrows <= 0 then
		MessageBox("Physician Early Filing","No data found to satisfy request")
	END IF
	
	RETURN

end event

type rb_all from radiobutton within w_nbms_nbca_payments
integer x = 745
integer y = 184
integer width = 187
integer height = 56
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "All"
boolean lefttext = true
end type

event clicked;PARENT.Title = 'NBMS/NBCA Payments Report'

is_report_type = 'ALL'
end event

type gb_2 from groupbox within w_nbms_nbca_payments
integer x = 59
integer y = 88
integer width = 933
integer height = 208
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Report Type"
end type

