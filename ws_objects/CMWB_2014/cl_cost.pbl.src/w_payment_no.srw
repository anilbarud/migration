$PBExportHeader$w_payment_no.srw
forward
global type w_payment_no from window
end type
type cb_cancel from commandbutton within w_payment_no
end type
type cb_ok from commandbutton within w_payment_no
end type
type st_1 from statictext within w_payment_no
end type
type dw_payment_no from u_dw_online within w_payment_no
end type
end forward

global type w_payment_no from window
integer width = 2053
integer height = 440
boolean titlebar = true
string title = "Ultimate payment"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
cb_cancel cb_cancel
cb_ok cb_ok
st_1 st_1
dw_payment_no dw_payment_no
end type
global w_payment_no w_payment_no

on w_payment_no.create
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.st_1=create st_1
this.dw_payment_no=create dw_payment_no
this.Control[]={this.cb_cancel,&
this.cb_ok,&
this.st_1,&
this.dw_payment_no}
end on

on w_payment_no.destroy
destroy(this.cb_cancel)
destroy(this.cb_ok)
destroy(this.st_1)
destroy(this.dw_payment_no)
end on

event open;INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


dw_payment_no.InsertRow(0)
dw_payment_no.SetFocus()
end event

type cb_cancel from commandbutton within w_payment_no
integer x = 1637
integer y = 232
integer width = 343
integer height = 92
integer taborder = 10
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Cancel"
end type

event clicked;CloseWithReturn(w_payment_no,0)
end event

type cb_ok from commandbutton within w_payment_no
integer x = 1289
integer y = 232
integer width = 320
integer height = 92
integer taborder = 20
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Ok"
boolean default = true
end type

event clicked;LONG		ll_payment_no

dw_payment_no.AcceptText()

ll_payment_no = dw_payment_no.GetItemNumber(1,'payment_no')
IF ll_payment_no = 0 or IsNull(ll_payment_no) Then
	MessageBox('Error','Please enter a payment number.')
	RETURN -1
END IF

CloseWithReturn(w_payment_no,ll_payment_no)
end event

type st_1 from statictext within w_payment_no
integer x = 41
integer y = 36
integer width = 1993
integer height = 160
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Please enter the earliest payment number that you cannot maintain. This will allow the application to list all manual transaction that need converting."
boolean focusrectangle = false
end type

type dw_payment_no from u_dw_online within w_payment_no
integer x = 279
integer y = 228
integer width = 837
integer height = 100
integer taborder = 10
string dataobject = "d_payment_no"
boolean border = false
end type

