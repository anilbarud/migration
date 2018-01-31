$PBExportHeader$w_formulary_history.srw
$PBExportComments$A response window showing the formulary history
forward
global type w_formulary_history from window
end type
type st_2 from statictext within w_formulary_history
end type
type st_1 from statictext within w_formulary_history
end type
type dw_eligibility from u_dw_online within w_formulary_history
end type
type cb_1 from commandbutton within w_formulary_history
end type
type dw_formulary_history from u_dw_online within w_formulary_history
end type
end forward

global type w_formulary_history from window
integer width = 3337
integer height = 2180
boolean titlebar = true
string title = "Formulary History"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
st_2 st_2
st_1 st_1
dw_eligibility dw_eligibility
cb_1 cb_1
dw_formulary_history dw_formulary_history
end type
global w_formulary_history w_formulary_history

on w_formulary_history.create
this.st_2=create st_2
this.st_1=create st_1
this.dw_eligibility=create dw_eligibility
this.cb_1=create cb_1
this.dw_formulary_history=create dw_formulary_history
this.Control[]={this.st_2,&
this.st_1,&
this.dw_eligibility,&
this.cb_1,&
this.dw_formulary_history}
end on

on w_formulary_history.destroy
destroy(this.st_2)
destroy(this.st_1)
destroy(this.dw_eligibility)
destroy(this.cb_1)
destroy(this.dw_formulary_history)
end on

event open;LONG ll_claim_no

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


ll_claim_no = Message.doubleParm

/* CHECK THAT THE CLAIM NUMBER IS VALID */
IF ISNULL(ll_claim_no) OR ll_claim_no < 1  THEN
	CLOSE(THIS)
	RETURN
END IF

//set the claim number onto the title bar
this.title = "Formulary History for Claim Number: " + string(ll_claim_no)
 
/* RETRIEVE THE CLAIM FORMULARY HISTORY INFORMATION */
dw_formulary_history.settransobject(sqlca)
dw_formulary_history.Retrieve(ll_claim_no)
SQLCA.nf_handle_error('w_formulary_history','open','dw_formulary_history.Retrieve(ll_claim_no)') 

/* RETRIEVE THE CLAIM ELIGIBILITY HISTORY INFORMATION */
dw_eligibility.settransobject(sqlca)
dw_eligibility.Retrieve(ll_claim_no)
SQLCA.nf_handle_error('w_formulary_history','open','dw_eligibility.Retrieve(ll_claim_no)') 












		



end event

type st_2 from statictext within w_formulary_history
integer x = 5
integer y = 952
integer width = 361
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "FORMULARY"
boolean focusrectangle = false
end type

type st_1 from statictext within w_formulary_history
integer x = 9
integer y = 8
integer width = 325
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "ELIGIBILITY"
boolean focusrectangle = false
end type

type dw_eligibility from u_dw_online within w_formulary_history
integer y = 68
integer width = 3291
integer height = 856
integer taborder = 10
string dataobject = "d_eligibility_history"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = false
end type

event ue_print;this.Object.DataWindow.Print.orientation = 1
this.print()
end event

type cb_1 from commandbutton within w_formulary_history
integer x = 3049
integer y = 1984
integer width = 256
integer height = 96
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;CLOSE(PARENT)
end event

type dw_formulary_history from u_dw_online within w_formulary_history
integer y = 1020
integer width = 3310
integer height = 944
integer taborder = 10
string dataobject = "d_formulary_history"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = false
end type

event ue_print;this.Object.DataWindow.Print.orientation = 1
this.print()
end event

