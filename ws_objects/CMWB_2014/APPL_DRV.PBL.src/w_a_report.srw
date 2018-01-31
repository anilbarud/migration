$PBExportHeader$w_a_report.srw
$PBExportComments$Common - Ancestor window with security code, one datawindow and a standard print event.  Used primarily for reports.
forward
global type w_a_report from window
end type
type dw_report from u_dw_online within w_a_report
end type
end forward

global type w_a_report from window
integer x = 1893
integer y = 48
integer width = 2766
integer height = 2760
boolean titlebar = true
string menuname = "m_cmwb_notools"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 67108864
event ue_print pbm_custom01
event ue_postopen pbm_custom02
dw_report dw_report
end type
global w_a_report w_a_report

type variables
datawindowchild 	viw_user_list

m_cmwb_notools vim_menu

boolean I_Authorized_Access  //flag to check window security
end variables

on ue_print;//
// Print that report.
//

dw_report.Print()

end on

event open;INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname_and_handle,100,this.ClassName(),'open event')


//	Set up APPLICATION SECURITY CODE

G_PFSecurity.UOF_Check_Access(This)
This.I_Authorized_Access = True              //declared as an instance variable


// Set up the instance variable for the menu so that we can refer to the frames menu later

	vim_menu = m_cmwb_notools

environment env
string ls_env
integer rtn
long ll_height, ll_width

rtn = GetEnvironment(env)

IF rtn <> 1 THEN RETURN

ll_height = env.screenheight
ll_width = env.screenwidth


IF ll_height < 864 THEN
	//Add scroll bar
	this.vscrollbar= true
END IF
end event

on w_a_report.create
if this.MenuName = "m_cmwb_notools" then this.MenuID = create m_cmwb_notools
this.dw_report=create dw_report
this.Control[]={this.dw_report}
end on

on w_a_report.destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.dw_report)
end on

event close;// write to the application log
f_populate_app_log(gs_appname_and_handle,100,this.ClassName(),'close event')
end event

type dw_report from u_dw_online within w_a_report
integer x = 46
integer y = 552
integer width = 2629
integer height = 1928
integer taborder = 20
boolean vscrollbar = true
end type

