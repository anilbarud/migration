$PBExportHeader$w_ancestor.srw
$PBExportComments$Common - Ancestor of all ancestors
forward
global type w_ancestor from window
end type
type mdi_1 from mdiclient within w_ancestor
end type
end forward

global type w_ancestor from window
integer x = 1851
integer width = 3387
integer height = 3084
boolean titlebar = true
string title = "Case Management Work Bench "
string menuname = "m_frame"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
windowtype windowtype = mdihelp!
long backcolor = 284228607
event moved pbm_move
mdi_1 mdi_1
end type
global w_ancestor w_ancestor

type variables
// Attributes required for application security

boolean   I_Authorized_Access  //flags if security is to be checked on window.
n_resize inv_resize//resize object
LONG		il_design_time_height = 0 
LONG		il_design_time_width = 0
CONSTANT LONG		il_workspace_height_diff = 212
CONSTANT LONG		il_workspace_width_diff = 50 


end variables

forward prototypes
public function integer wf_set_claim (long claim_no)
public subroutine wf_set_provider_no (long al_provider_no, string as_provider_type_code)
public function integer wf_read_only ()
public subroutine wf_clear_identifier ()
public subroutine wf_setresize (boolean ab_switch)
end prototypes

public function integer wf_set_claim (long claim_no);Return 0
end function

public subroutine wf_set_provider_no (long al_provider_no, string as_provider_type_code);Return 
end subroutine

public function integer wf_read_only ();Return 0
end function

public subroutine wf_clear_identifier ();
end subroutine

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

event open;INT li_trancount
N_OBJECTHELPER lnv_object_helper

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname_and_handle ,lnv_object_helper.get_app_component_type(THIS), lnv_object_helper.getpath(THIS), 'Open Event' )

//	APPLICATION SECURITY CODE
//
	G_PFSecurity.UOF_Check_Access(This)
	This.I_Authorized_Access = True              //declared as an instance variable
	
// Get the screen resolution, depending on the size set the scroll bars on

environment env
string ls_env
integer rtn
long ll_height, ll_width

rtn = GetEnvironment(env)

IF rtn <> 1 THEN RETURN

ll_height = env.screenheight
ll_width = env.screenwidth


IF ll_height < 864 THEN
	//Add scroll bars
	this.vscrollbar= true
END IF




end event

on w_ancestor.create
if this.MenuName = "m_frame" then this.MenuID = create m_frame
this.mdi_1=create mdi_1
this.Control[]={this.mdi_1}
end on

on w_ancestor.destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.mdi_1)
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

event close;// write to the application log
N_OBJECTHELPER lnv_object_helper
f_populate_app_log(gs_appname_and_handle ,lnv_object_helper.get_app_component_type(THIS), lnv_object_helper.getpath(THIS), 'Close Event' )

end event

type mdi_1 from mdiclient within w_ancestor
long BackColor=284228607
end type

