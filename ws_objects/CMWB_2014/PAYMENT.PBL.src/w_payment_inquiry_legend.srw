$PBExportHeader$w_payment_inquiry_legend.srw
forward
global type w_payment_inquiry_legend from window
end type
type dw_payment_inquiry_legend from u_dw_online within w_payment_inquiry_legend
end type
end forward

global type w_payment_inquiry_legend from window
integer width = 2601
integer height = 1468
boolean titlebar = true
string title = "Code Descriptions"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
dw_payment_inquiry_legend dw_payment_inquiry_legend
end type
global w_payment_inquiry_legend w_payment_inquiry_legend

on w_payment_inquiry_legend.create
this.dw_payment_inquiry_legend=create dw_payment_inquiry_legend
this.Control[]={this.dw_payment_inquiry_legend}
end on

on w_payment_inquiry_legend.destroy
destroy(this.dw_payment_inquiry_legend)
end on

event open;INTEGER		li_rtn

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


dw_payment_inquiry_legend.SetTransObject(SQLCA)
li_rtn = dw_payment_inquiry_legend.Retrieve()
SQLCA.nf_handle_error('w_payment_inquiry_legend','open','retrieve')
IF li_rtn <= 0 Then SignalError(-666,'Error retrieving legend')
end event

type dw_payment_inquiry_legend from u_dw_online within w_payment_inquiry_legend
integer width = 2574
integer height = 1372
integer taborder = 10
string dataobject = "d_payment_inquiry_legend"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup
/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	
	lm_popup.m_options.PopMenu(w_payment_inquiry_legend.PointerX( ), w_payment_inquiry_legend.PointerY( ))

	Destroy lm_popup
end event

