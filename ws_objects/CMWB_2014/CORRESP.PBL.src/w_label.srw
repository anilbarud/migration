$PBExportHeader$w_label.srw
$PBExportComments$Allows user to modify a duplicate label
forward
global type w_label from window
end type
type cb_cancel from commandbutton within w_label
end type
type cb_ok from commandbutton within w_label
end type
type sle_label from singlelineedit within w_label
end type
end forward

global type w_label from window
integer x = 2409
integer y = 1812
integer width = 1029
integer height = 420
boolean titlebar = true
string title = "Maintenance"
windowtype windowtype = response!
long backcolor = 67108864
cb_cancel cb_cancel
cb_ok cb_ok
sle_label sle_label
end type
global w_label w_label

type variables
string	vis_prev_label
end variables

event open;INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


vis_prev_label = message.StringParm

sle_label.text = vis_prev_label 
end event

on w_label.create
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.sle_label=create sle_label
this.Control[]={this.cb_cancel,&
this.cb_ok,&
this.sle_label}
end on

on w_label.destroy
destroy(this.cb_cancel)
destroy(this.cb_ok)
destroy(this.sle_label)
end on

type cb_cancel from commandbutton within w_label
integer x = 530
integer y = 192
integer width = 274
integer height = 108
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
boolean cancel = true
end type

on clicked;closewithreturn(parent, "none")
end on

type cb_ok from commandbutton within w_label
integer x = 183
integer y = 192
integer width = 247
integer height = 108
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
boolean default = true
end type

on clicked;
IF Match(sle_label.text, "~"+") THEN 
	MessageBox(title,"Text cannot contain double quotes, please re-enter.")
	/* Don't close the window if double quotes are in the label   */
	sle_label.Setfocus()
	Return
END IF

IF sle_label.text <> vis_prev_label THEN
	closewithreturn(parent,sle_label.text)
ELSE
	MessageBox(parent.title, "No changes were made, Please try again or cancel",Information!)
	sle_label.SetFocus()
	Return
END IF
end on

type sle_label from singlelineedit within w_label
integer x = 73
integer y = 64
integer width = 859
integer height = 96
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
integer limit = 40
borderstyle borderstyle = stylelowered!
end type

