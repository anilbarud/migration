$PBExportHeader$w_annuity_eligibility_history.srw
forward
global type w_annuity_eligibility_history from window
end type
type dw_annuity_eligibility from u_datawindow within w_annuity_eligibility_history
end type
type cb_close from commandbutton within w_annuity_eligibility_history
end type
end forward

global type w_annuity_eligibility_history from window
integer width = 5033
integer height = 1820
boolean titlebar = true
string title = "Annuity Eligibility History"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
dw_annuity_eligibility dw_annuity_eligibility
cb_close cb_close
end type
global w_annuity_eligibility_history w_annuity_eligibility_history

type variables
S_WINDOW_MESSAGE istr_message

m_dw_rmb_popup					im_popup
end variables

on w_annuity_eligibility_history.create
this.dw_annuity_eligibility=create dw_annuity_eligibility
this.cb_close=create cb_close
this.Control[]={this.dw_annuity_eligibility,&
this.cb_close}
end on

on w_annuity_eligibility_history.destroy
destroy(this.dw_annuity_eligibility)
destroy(this.cb_close)
end on

event open;Long ll_annuity_account

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


istr_message= Message.PowerObjectParm


dw_annuity_eligibility.settransobject(sqlca)

ll_annuity_account = istr_message.al_doubleparm[3]

dw_annuity_eligibility.Retrieve( ll_annuity_account)
end event

type dw_annuity_eligibility from u_datawindow within w_annuity_eligibility_history
integer width = 5015
integer height = 1608
integer taborder = 10
string dataobject = "d_annuity_eligibility_history"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rbuttondown;call super::rbuttondown;/*	Create the menu -	Note that this only gives default options.  If you want 
   additional options, you should override the ancestor and visible the options 
	you desire.
*/

IF isvalid(im_popup) THEN DESTROY im_popup

im_popup = CREATE m_dw_rmb_popup
im_popup.mf_set_datawindow(THIS)

im_popup.m_options.PopMenu(w_frame.PointerX(), w_frame.PointerY())
end event

type cb_close from commandbutton within w_annuity_eligibility_history
integer x = 4608
integer y = 1620
integer width = 402
integer height = 104
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;Close(w_annuity_eligibility_history)
end event

