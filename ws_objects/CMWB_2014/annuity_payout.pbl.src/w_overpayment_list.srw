$PBExportHeader$w_overpayment_list.srw
forward
global type w_overpayment_list from window
end type
type dw_overpayment_preview from u_datawindow within w_overpayment_list
end type
type cb_op_recovery_preview from commandbutton within w_overpayment_list
end type
type cb_close from commandbutton within w_overpayment_list
end type
type dw_overpayments from u_datawindow within w_overpayment_list
end type
end forward

global type w_overpayment_list from window
integer width = 5737
integer height = 2352
boolean titlebar = true
string title = "Overpayment List"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
dw_overpayment_preview dw_overpayment_preview
cb_op_recovery_preview cb_op_recovery_preview
cb_close cb_close
dw_overpayments dw_overpayments
end type
global w_overpayment_list w_overpayment_list

type variables
LONG               il_annuity_payout_no, il_individual_no
LONG               il_design_time_height, il_design_time_width, il_workspace_width_diff, il_workspace_height_diff

n_common_annuity   inv_common_annuity
ULONG              iul_handle
WINDOW             iw_win
n_resize           inv_resize
end variables

forward prototypes
public function window wf_get_window_reference ()
public subroutine wf_setresize (boolean ab_switch)
end prototypes

public function window wf_get_window_reference ();RETURN THIS
end function

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

on w_overpayment_list.create
this.dw_overpayment_preview=create dw_overpayment_preview
this.cb_op_recovery_preview=create cb_op_recovery_preview
this.cb_close=create cb_close
this.dw_overpayments=create dw_overpayments
this.Control[]={this.dw_overpayment_preview,&
this.cb_op_recovery_preview,&
this.cb_close,&
this.dw_overpayments}
end on

on w_overpayment_list.destroy
destroy(this.dw_overpayment_preview)
destroy(this.cb_op_recovery_preview)
destroy(this.cb_close)
destroy(this.dw_overpayments)
end on

event open;INTEGER    li_rows, li_upperbound
LONG       ll_individual_no
S_WINDOW_MESSAGE  lstr_message

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


lstr_message = Message.PowerObjectParm

il_annuity_payout_no = lstr_message.al_doubleparm[1]
il_individual_no     = lstr_message.al_doubleparm[2]

SetRedraw(FALSE)

li_rows = dw_overpayments.Retrieve(il_individual_no)
SQLCA.nf_handle_error('w_overpayment_popup','dw_overpayments','open event')

//IF li_rows = 0 THEN
//	cb_op_recovery_preview.Enabled = FALSE
//END IF

SetRedraw(TRUE)

iul_handle = Handle(THIS)

// sets up to close this window if frame is closed
li_upperbound = UpperBound(gstr_window_array) + 1
gstr_window_array[li_upperbound].window_element = THIS
gstr_window_array[li_upperbound].handle_element = iul_handle

inv_common_annuity = Create n_common_annuity

iw_win = wf_get_window_reference()

il_design_time_height = 2248
il_design_time_width = 5701

// set up resizing
This.wf_SetResize(True)

inv_resize.of_register(dw_overpayment_preview,'ScaleToRight&Bottom')
inv_resize.of_register(dw_overpayments,'ScaleToRight&Bottom')

inv_resize.of_register(cb_close,'FixedToBottom')
inv_resize.of_register(cb_op_recovery_preview,'FixedToBottom')



end event

event close;INTEGER		li_counter, li_upper


inv_common_annuity.nf_close_handle_array(iul_handle)

li_upper = UpperBound(gstr_window_array)

FOR li_counter = 1 TO li_upper
	IF gstr_window_array[li_counter].window_element.ClassName() = 'w_overpayment_popup' THEN
		gstr_window_array[li_counter].window_element.SetFocus()
	END IF
NEXT
end event

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

type dw_overpayment_preview from u_datawindow within w_overpayment_list
boolean visible = false
integer x = 59
integer y = 52
integer width = 5568
integer height = 2004
integer taborder = 20
string dataobject = "d_annuity_overpayment_sp"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;THIS.SetTransObject(SQLCA)

THIS.Object.DataWindow.Print.Orientation= '1' // Landscape
end event

event rbuttondown;call super::rbuttondown;M_DW_RMB_POPUP lm_popup

IF IsValid(inv_rowselection) Then
	inv_rowselection.event pfc_rbuttondown(xpos,ypos,row,dwo)
End if


/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
lm_popup = Create m_dw_rmb_popup
lm_popup.mf_set_datawindow(This)

// don't display lines
lm_popup.m_options.m_1.Visible = FALSE
lm_popup.m_options.m_2.Visible = FALSE


lm_popup.m_options.PopMenu(iw_win.PointerX( ), iw_win.PointerY( ))

Destroy lm_popup
end event

type cb_op_recovery_preview from commandbutton within w_overpayment_list
integer x = 59
integer y = 2108
integer width = 882
integer height = 104
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Overpayment Recovery Preview"
end type

event clicked;INTEGER   li_rows


IF THIS.Text = 'Overpayment Recovery Preview' THEN
	THIS.Text = 'Close Preview'
	
	// display preview of OP recovery
	dw_overpayment_preview.Visible = TRUE
	dw_overpayments.Visible = FALSE
	
	IF dw_overpayment_preview.RowCount() = 0 THEN
		li_rows = dw_overpayment_preview.Retrieve(il_annuity_payout_no,'DISPLAY')
		SQLCA.nf_handle_error('w_overpayment_list','dw_overpayment_preview.Retrieve','cb_op_recovery_preview')
		
		dw_overpayment_preview.Object.DataWindow.HideGrayLine='Yes'
		
		IF li_rows = 0 THEN
			MessageBox('','There are no overpayments to recover for the annuity participants involved in this annuity payout.',Information!)
		END IF
	END IF
	
ELSE
	THIS.Text = 'Overpayment Recovery Preview'
	
   // display OP list
	dw_overpayment_preview.Visible = FALSE
	dw_overpayments.Visible = TRUE
	
	// already retrieved in open event
	
END IF
end event

type cb_close from commandbutton within w_overpayment_list
integer x = 5225
integer y = 2108
integer width = 402
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;Close(PARENT)
end event

type dw_overpayments from u_datawindow within w_overpayment_list
integer x = 59
integer y = 52
integer width = 5568
integer height = 2004
integer taborder = 10
string title = "none"
string dataobject = "d_entitlement_overpayment"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event constructor;THIS.SetTransObject(SQLCA)

THIS.Object.DataWindow.Print.Orientation= '1' // Landscape
end event

event rbuttondown;call super::rbuttondown;M_DW_RMB_POPUP lm_popup

IF IsValid(inv_rowselection) Then
	inv_rowselection.event pfc_rbuttondown(xpos,ypos,row,dwo)
End if


/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
lm_popup = Create m_dw_rmb_popup
lm_popup.mf_set_datawindow(This)

// don't display lines
lm_popup.m_options.m_1.Visible = FALSE
lm_popup.m_options.m_2.Visible = FALSE


lm_popup.m_options.PopMenu(iw_win.PointerX( ), iw_win.PointerY( ))

Destroy lm_popup
end event

