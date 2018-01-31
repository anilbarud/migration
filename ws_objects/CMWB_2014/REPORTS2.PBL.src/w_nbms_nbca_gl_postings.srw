$PBExportHeader$w_nbms_nbca_gl_postings.srw
$PBExportComments$Window to produce NBMS/NBCA GL Report
forward
global type w_nbms_nbca_gl_postings from w_a_report
end type
type cb_ok from commandbutton within w_nbms_nbca_gl_postings
end type
type dw_coc_range from u_dw_online within w_nbms_nbca_gl_postings
end type
type rb_nbms from radiobutton within w_nbms_nbca_gl_postings
end type
type rb_nbca from radiobutton within w_nbms_nbca_gl_postings
end type
type cb_clear from commandbutton within w_nbms_nbca_gl_postings
end type
type rb_all from radiobutton within w_nbms_nbca_gl_postings
end type
type gb_1 from groupbox within w_nbms_nbca_gl_postings
end type
end forward

global type w_nbms_nbca_gl_postings from w_a_report
integer width = 3067
integer height = 2608
string title = "NBMS General Ledger Postings"
cb_ok cb_ok
dw_coc_range dw_coc_range
rb_nbms rb_nbms
rb_nbca rb_nbca
cb_clear cb_clear
rb_all rb_all
gb_1 gb_1
end type
global w_nbms_nbca_gl_postings w_nbms_nbca_gl_postings

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

event open;call super::open;dw_report.SetTransObject(SQLCA)

//The two dropdowns in dw_coc_range consist of two dddw_coc_period objects.
dw_coc_range.SetTransObject(SQLCA)
dw_coc_range.InsertRow(0)
dw_coc_range.SetColumn('coc_period_from')


// initialize instance variable
is_report_type = 'NBMS'


il_design_time_width  = 3031
il_design_time_height = 2428

This.wf_SetResize(True)

THIS.inv_resize.of_register(dw_report,'ScaleToRight&Bottom')
THIS.inv_resize.of_register(cb_ok,    'FixedToRight')
THIS.inv_resize.of_register(cb_clear, 'FixedToRight')
end event

on w_nbms_nbca_gl_postings.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.dw_coc_range=create dw_coc_range
this.rb_nbms=create rb_nbms
this.rb_nbca=create rb_nbca
this.cb_clear=create cb_clear
this.rb_all=create rb_all
this.gb_1=create gb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.dw_coc_range
this.Control[iCurrent+3]=this.rb_nbms
this.Control[iCurrent+4]=this.rb_nbca
this.Control[iCurrent+5]=this.cb_clear
this.Control[iCurrent+6]=this.rb_all
this.Control[iCurrent+7]=this.gb_1
end on

on w_nbms_nbca_gl_postings.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.dw_coc_range)
destroy(this.rb_nbms)
destroy(this.rb_nbca)
destroy(this.cb_clear)
destroy(this.rb_all)
destroy(this.gb_1)
end on

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

type dw_report from w_a_report`dw_report within w_nbms_nbca_gl_postings
integer y = 372
integer width = 2930
integer height = 2004
string dataobject = "d_nbms_nbca_gl_postings"
boolean hscrollbar = true
end type

type cb_ok from commandbutton within w_nbms_nbca_gl_postings
integer x = 2574
integer y = 72
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
string text = "OK"
boolean default = true
end type

event clicked;LONG     ll_coc_from, ll_coc_to, ll_rows

/*
** This report retrieves data based on payment_type_code = '21', payment_sub_type_code = '03','04','05','06'
** for the chosen date range.
**	
** The code below ensures that a proper range is chosen, then runs the report.
**
**	SR 73, Kevin MacLeod 2000-02-26
**
*/

ll_coc_from = Long(Left(dw_coc_range.GetItemString(1,"coc_period_from"),4))*100 + &
              Long(Right(dw_coc_range.GetItemString(1,"coc_period_from"),2))
ll_coc_to = Long(Left(dw_coc_range.GetItemString(1,"coc_period_to"),4))*100 + &
              Long(Right(dw_coc_range.GetItemString(1,"coc_period_to"),2))

IF IsNull(ll_coc_to) OR (ll_coc_to = 0) THEN
	ll_coc_to = ll_coc_from
	ll_rows = dw_report.Retrieve(ll_coc_from, ll_coc_to)
ELSE
	IF ll_coc_to >= ll_coc_from THEN
		ll_rows = dw_report.Retrieve(ll_coc_from, ll_coc_to, is_report_type)
	ELSE
		MessageBox("Choose another COC", "Please choose an ending Cost of Claims Period that is greater than or equal to the starting Cost of Claims Period.")
	END IF
END IF

IF ll_rows = 0 THEN
	MessageBox("No Data", "There is no data for this date range")
END IF
end event

type dw_coc_range from u_dw_online within w_nbms_nbca_gl_postings
integer x = 1234
integer y = 36
integer width = 1129
integer height = 244
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_coc_range"
boolean border = false
end type

type rb_nbms from radiobutton within w_nbms_nbca_gl_postings
integer x = 219
integer y = 144
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

event clicked;PARENT.Title = 'NBMS Medical Payments'

is_report_type = 'NBMS'
end event

type rb_nbca from radiobutton within w_nbms_nbca_gl_postings
integer x = 498
integer y = 144
integer width = 238
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

event clicked;PARENT.Title = 'NBCA Medical Payments'

is_report_type = 'NBCA'
end event

type cb_clear from commandbutton within w_nbms_nbca_gl_postings
integer x = 2574
integer y = 224
integer width = 402
integer height = 108
integer taborder = 30
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
dw_coc_range.Reset()
dw_coc_range.InsertRow(0)
dw_report.reset()
end event

type rb_all from radiobutton within w_nbms_nbca_gl_postings
integer x = 777
integer y = 144
integer width = 165
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

event clicked;PARENT.Title = 'NBMS/NBCA Medical Payments'

is_report_type = 'ALL'
end event

type gb_1 from groupbox within w_nbms_nbca_gl_postings
integer x = 82
integer y = 44
integer width = 983
integer height = 212
integer taborder = 20
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

