$PBExportHeader$w_prompt_user_for_be_count.srw
$PBExportComments$To split a payment into multiple entitlements only one payment may be selected at a time.  The user will need to be prompted for the number of entitlement rows to create.
forward
global type w_prompt_user_for_be_count from window
end type
type em_count from editmask within w_prompt_user_for_be_count
end type
type st_1 from statictext within w_prompt_user_for_be_count
end type
type cb_cancel from commandbutton within w_prompt_user_for_be_count
end type
type cb_ok from commandbutton within w_prompt_user_for_be_count
end type
end forward

global type w_prompt_user_for_be_count from window
integer x = 1335
integer y = 688
integer width = 1486
integer height = 664
boolean titlebar = true
string title = "Enter Entitlement Count"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
em_count em_count
st_1 st_1
cb_cancel cb_cancel
cb_ok cb_ok
end type
global w_prompt_user_for_be_count w_prompt_user_for_be_count

type variables
LONG	il_claim_no
end variables

on w_prompt_user_for_be_count.create
this.em_count=create em_count
this.st_1=create st_1
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.Control[]={this.em_count,&
this.st_1,&
this.cb_cancel,&
this.cb_ok}
end on

on w_prompt_user_for_be_count.destroy
destroy(this.em_count)
destroy(this.st_1)
destroy(this.cb_cancel)
destroy(this.cb_ok)
end on

event open;INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


end event

type em_count from editmask within w_prompt_user_for_be_count
integer x = 576
integer y = 228
integer width = 233
integer height = 84
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
string mask = "##"
string displaydata = "0~t0/"
end type

type st_1 from statictext within w_prompt_user_for_be_count
integer x = 32
integer y = 68
integer width = 1417
integer height = 104
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Please enter the number of entitlement rows to create"
boolean focusrectangle = false
end type

type cb_cancel from commandbutton within w_prompt_user_for_be_count
integer x = 718
integer y = 448
integer width = 379
integer height = 96
integer taborder = 70
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
end type

event clicked;CLOSE(PARENT)
end event

type cb_ok from commandbutton within w_prompt_user_for_be_count
integer x = 297
integer y = 448
integer width = 379
integer height = 96
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;LONG  ll_return

ll_return = LONG(em_count.text)

IF ISNULL(ll_return) OR ll_return < 0 THEN ll_return = 0

//if  = 0 then no value was selected return 0

Closewithreturn(parent,ll_return)
end event

