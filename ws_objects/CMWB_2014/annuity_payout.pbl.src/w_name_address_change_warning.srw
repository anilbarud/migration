$PBExportHeader$w_name_address_change_warning.srw
forward
global type w_name_address_change_warning from window
end type
type cb_print from commandbutton within w_name_address_change_warning
end type
type dw_message from datawindow within w_name_address_change_warning
end type
type cb_cancel from commandbutton within w_name_address_change_warning
end type
type cb_continue from commandbutton within w_name_address_change_warning
end type
end forward

global type w_name_address_change_warning from window
integer width = 3653
integer height = 2164
boolean titlebar = true
string title = "Annuity Payout Recipient - Name or Address Change"
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
cb_print cb_print
dw_message dw_message
cb_cancel cb_cancel
cb_continue cb_continue
end type
global w_name_address_change_warning w_name_address_change_warning

type variables

end variables

on w_name_address_change_warning.create
this.cb_print=create cb_print
this.dw_message=create dw_message
this.cb_cancel=create cb_cancel
this.cb_continue=create cb_continue
this.Control[]={this.cb_print,&
this.dw_message,&
this.cb_cancel,&
this.cb_continue}
end on

on w_name_address_change_warning.destroy
destroy(this.cb_print)
destroy(this.dw_message)
destroy(this.cb_cancel)
destroy(this.cb_continue)
end on

event open;INTEGER                 li_counter, li_upperbound, li_currentrow
STRING                  ls_change_header, ls_change_message, ls_change_footer
S_NAME_ADDRESS_CHANGE   lstr_name_address_change

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


lstr_name_address_change = Message.PowerObjectParm

ls_change_header = lstr_name_address_change.change_header
ls_change_footer = lstr_name_address_change.change_footer



li_upperbound = UpperBound(lstr_name_address_change.change_message)

FOR li_counter = 1 TO li_upperbound
	li_currentrow = dw_message.InsertRow(0)
	
	dw_message.SetItem(li_currentrow,'change_header',ls_change_header)
	dw_message.SetItem(li_currentrow,'change_footer',ls_change_footer)
	
	dw_message.SetItem(li_currentrow,'row_counter',String(li_counter) + ' of ' + String(li_upperbound))

	dw_message.SetItem(li_currentrow,'old_name',lstr_name_address_change.old_name[li_counter])
	dw_message.SetItem(li_currentrow,'new_name',lstr_name_address_change.new_name[li_counter])
	
	ls_change_message = lstr_name_address_change.change_message[li_counter]
	dw_message.SetItem(li_currentrow,'change_message',ls_change_message)	
	
	dw_message.SetItem(li_currentrow,'old_address_line1',lstr_name_address_change.old_address_line1[li_counter])
	dw_message.SetItem(li_currentrow,'new_address_line1',lstr_name_address_change.new_address_line1[li_counter])
	
	dw_message.SetItem(li_currentrow,'old_address_line2',lstr_name_address_change.old_address_line2[li_counter])
	dw_message.SetItem(li_currentrow,'new_address_line2',lstr_name_address_change.new_address_line2[li_counter])
	
	dw_message.SetItem(li_currentrow,'old_address_line3',lstr_name_address_change.old_address_line3[li_counter])
	dw_message.SetItem(li_currentrow,'new_address_line3',lstr_name_address_change.new_address_line3[li_counter])
	
	dw_message.SetItem(li_currentrow,'old_address_line4',lstr_name_address_change.old_address_line4[li_counter])
	dw_message.SetItem(li_currentrow,'new_address_line4',lstr_name_address_change.new_address_line4[li_counter])
	
	dw_message.SetItem(li_currentrow,'old_address_line5',lstr_name_address_change.old_address_line5[li_counter])
	dw_message.SetItem(li_currentrow,'new_address_line5',lstr_name_address_change.new_address_line5[li_counter])

NEXT

end event

type cb_print from commandbutton within w_name_address_change_warning
integer x = 55
integer y = 1932
integer width = 402
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Print"
end type

event clicked;dw_message.Print()
end event

type dw_message from datawindow within w_name_address_change_warning
integer x = 55
integer y = 40
integer width = 3547
integer height = 1852
integer taborder = 10
string title = "none"
string dataobject = "dx_name_address_change"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type cb_cancel from commandbutton within w_name_address_change_warning
integer x = 3200
integer y = 1932
integer width = 402
integer height = 104
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cancel"
boolean default = true
end type

event clicked;CloseWithReturn(PARENT,'NO')
end event

type cb_continue from commandbutton within w_name_address_change_warning
integer x = 2706
integer y = 1932
integer width = 402
integer height = 104
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Continue"
end type

event clicked;CloseWithReturn(PARENT,'YES')
end event

