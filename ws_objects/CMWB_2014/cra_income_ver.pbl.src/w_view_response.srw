$PBExportHeader$w_view_response.srw
forward
global type w_view_response from window
end type
type dw_tax_info from u_datawindow within w_view_response
end type
type st_1 from statictext within w_view_response
end type
type cb_2 from commandbutton within w_view_response
end type
type cb_1 from commandbutton within w_view_response
end type
end forward

global type w_view_response from window
integer width = 3794
integer height = 3484
boolean titlebar = true
string title = "CRA Response"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
dw_tax_info dw_tax_info
st_1 st_1
cb_2 cb_2
cb_1 cb_1
end type
global w_view_response w_view_response

type variables
LONG		il_design_time_height = 0 
LONG		il_design_time_width = 0
CONSTANT LONG		il_workspace_height_diff = 50
CONSTANT LONG		il_workspace_width_diff = 50 
n_resize inv_resize//resize object
end variables

forward prototypes
public subroutine wf_setresize (boolean ab_switch)
end prototypes

public subroutine wf_setresize (boolean ab_switch);IF ab_switch = True Then
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

event open;STRING ls_message,  ls_year
BOOLEAN lb_single_yr
LONG    ll_year, ll_request_no
str_tax_years lstr_tax_years

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


il_design_time_height = 3350
il_design_time_width = 3710

lstr_tax_years = Message.PowerObjectParm	

lb_single_yr    = lstr_tax_years.single_year
ll_request_no = lstr_tax_years.request_no

IF lb_single_yr = TRUE THEN
	ll_year = lstr_tax_years.year
	dw_tax_info.DataObject = 'd_response_request_match_yr'
	dw_tax_info.SetTransObject(SQLCA)
	dw_tax_info.Retrieve(ll_request_no, ll_year)
ELSE
	dw_tax_info.DataObject = 'd_comp_request_match'
	dw_tax_info.SetTransObject(SQLCA)
	dw_tax_info.Retrieve(ll_request_no)
END IF

dw_tax_info.Object.t_scan.Font.Escapement = 300

wf_setresize(TRUE)

inv_resize.of_Register(dw_tax_info, 'ScaleToRight&Bottom')

end event

on w_view_response.create
this.dw_tax_info=create dw_tax_info
this.st_1=create st_1
this.cb_2=create cb_2
this.cb_1=create cb_1
this.Control[]={this.dw_tax_info,&
this.st_1,&
this.cb_2,&
this.cb_1}
end on

on w_view_response.destroy
destroy(this.dw_tax_info)
destroy(this.st_1)
destroy(this.cb_2)
destroy(this.cb_1)
end on

event resize;LONG ll_workspacewidth,ll_workspaceheight

IF IsValid(inv_resize) THEN
	// Notify the resize service that the window size has changed.
	ll_workspacewidth  = This.WorkSpaceWidth()
	ll_workspaceheight = This.WorkSpaceHeight()

	IF IsValid (inv_resize) THEN
		inv_resize.Event pfc_Resize (sizetype,ll_workspacewidth  , ll_workspaceheight )
	END IF 
END IF 


end event

type dw_tax_info from u_datawindow within w_view_response
integer x = 18
integer y = 116
integer width = 3602
integer height = 3156
integer taborder = 30
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = false
borderstyle borderstyle = stylelowered!
end type

type st_1 from statictext within w_view_response
integer x = 32
integer y = 4
integer width = 1531
integer height = 88
integer textsize = -14
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "CRA Income Tax Return Information"
boolean focusrectangle = false
end type

type cb_2 from commandbutton within w_view_response
integer x = 3296
integer y = 4
integer width = 334
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Close"
end type

event clicked;Close(Parent)
end event

type cb_1 from commandbutton within w_view_response
integer x = 2962
integer y = 4
integer width = 334
integer height = 104
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Print"
end type

event clicked;dw_tax_info.Object.t_scan.Visible = TRUE

dw_tax_info.Print()

dw_tax_info.Object.t_scan.Visible = FALSE

end event

