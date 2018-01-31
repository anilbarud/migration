$PBExportHeader$w_outstanding_advances_report.srw
$PBExportComments$Window to produce report of outstanding advances.
forward
global type w_outstanding_advances_report from w_a_report
end type
type rb_nonzero from radiobutton within w_outstanding_advances_report
end type
type rb_all from radiobutton within w_outstanding_advances_report
end type
end forward

global type w_outstanding_advances_report from w_a_report
string title = "Advance Summary Report"
rb_nonzero rb_nonzero
rb_all rb_all
end type
global w_outstanding_advances_report w_outstanding_advances_report

type variables
n_resize inv_resize//resize object
LONG		il_design_time_height = 0 
LONG		il_design_time_width = 0
CONSTANT LONG		il_workspace_height_diff = 212
CONSTANT LONG		il_workspace_width_diff = 50 
STRING   is_sort
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

event open;call super::open;INTEGER  li_rtn
LONG     ll_nbr_rows


/*	Database Connections 
*/
IF (dw_report.SetTransObject(SQLCA) <> 1) then
	MessageBox(title,'Error Setting TransObject',Stopsign!)
	error.Number = -1
	SQLCA.nf_handle_error(this.ClassName(),'cb_ok','SetTrans of dw_report')
	Return
END IF

dw_report.Object.Datawindow.ReadOnly = 'Yes'
dw_report.Object.Datawindow.print.orientation = 1

dw_report.Object.t_parameters.Text = 'Report Parameters: Non-Zero Balances'

/*	Retrieve the report.		*/
dw_report.Object.as_region_hdg.text = 'All Regions'

ll_nbr_rows = dw_report.Retrieve()
SQLCA.nf_handle_error(this.ClassName(),"dw_report","clicked of cb_ok doing a Retrive")

is_sort = 'claim_admin_region_code asc, claim_no asc, overpayment_date asc'
rb_nonzero.TriggerEvent(Clicked!)


IF ll_nbr_rows <= 0 THEN
	MessageBox(title,"No data found to satisfy request")
END IF


il_design_time_width = 2800
il_design_time_height = 2700

This.wf_SetResize(True)

//(Move H,Move V,Grow H, Grow V)
inv_resize.of_register(dw_report,0,0,75,75)
end event

on w_outstanding_advances_report.create
int iCurrent
call super::create
this.rb_nonzero=create rb_nonzero
this.rb_all=create rb_all
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.rb_nonzero
this.Control[iCurrent+2]=this.rb_all
end on

on w_outstanding_advances_report.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.rb_nonzero)
destroy(this.rb_all)
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

event ue_print;dw_report.Object.DataWindow.Print.Orientation = '1'
dw_report.Print()
end event

type dw_report from w_a_report`dw_report within w_outstanding_advances_report
integer x = 27
integer y = 224
integer height = 2272
string dataobject = "d_detail_advance_report"
boolean hscrollbar = true
end type

type rb_nonzero from radiobutton within w_outstanding_advances_report
integer x = 41
integer y = 12
integer width = 576
integer height = 96
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Non-Zero Balances"
boolean checked = true
end type

event clicked;INTEGER     li_rtn
STRING      ls_filter

dw_report.Object.t_parameters.Text = 'Report Parameters: Non-Zero Balances'

ls_filter = 'balance_amount <> 0'
li_rtn = dw_report.SetFilter(ls_filter)
li_rtn = dw_report.Filter()

li_rtn = dw_report.SetSort(is_sort)
li_rtn = dw_report.Sort()

dw_report.GroupCalc()
end event

type rb_all from radiobutton within w_outstanding_advances_report
integer x = 41
integer y = 120
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
string text = "All Balances"
end type

event clicked;INTEGER     li_rtn
STRING      ls_filter

dw_report.Object.t_parameters.Text = 'Report Parameters: All Balances'
	
ls_filter = ''
li_rtn = dw_report.SetFilter(ls_filter)
li_rtn = dw_report.Filter()

li_rtn = dw_report.SetSort(is_sort)
li_rtn = dw_report.Sort()

dw_report.GroupCalc()
end event

