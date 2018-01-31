$PBExportHeader$w_account_authorizations.srw
forward
global type w_account_authorizations from window
end type
type cb_cancel from commandbutton within w_account_authorizations
end type
type cb_ok from commandbutton within w_account_authorizations
end type
type dw_authorization from u_dw_online within w_account_authorizations
end type
end forward

global type w_account_authorizations from window
integer x = 1074
integer y = 480
integer width = 3474
integer height = 1528
boolean titlebar = true
string title = "Account Authorization"
windowtype windowtype = response!
long backcolor = 67108864
cb_cancel cb_cancel
cb_ok cb_ok
dw_authorization dw_authorization
end type
global w_account_authorizations w_account_authorizations

type variables
Date		idt_ephysio_implementation_date
end variables

on w_account_authorizations.create
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.dw_authorization=create dw_authorization
this.Control[]={this.cb_cancel,&
this.cb_ok,&
this.dw_authorization}
end on

on w_account_authorizations.destroy
destroy(this.cb_cancel)
destroy(this.cb_ok)
destroy(this.dw_authorization)
end on

event open;S_WINDOW_MESSAGE	lstr_message
LONG					ll_rows

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


	lstr_message = Message.PowerObjectParm
	
	idt_ephysio_implementation_date = Date(gdtm_ephysio_implementation_date)
	
	dw_authorization.SetTransObject(SQLCA)
	
   dw_authorization.Modify("authorization_comment.Multiline=yes") 
	
	ll_rows = dw_authorization.Retrieve(lstr_message.al_doubleparm[1], idt_ephysio_implementation_date)  //claim_no, implementation date
	
	IF ll_rows < 1 THEN
		MessageBox('No Authorizations', 'There are no authorization items for this claim.')
		lstr_message.as_mode = 'NOTOK'
		CloseWithReturn(This,lstr_message)
		Return
	END IF
	
	
	

end event

type cb_cancel from commandbutton within w_account_authorizations
integer x = 1573
integer y = 1260
integer width = 274
integer height = 108
integer taborder = 3
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
end type

event clicked;S_WINDOW_MESSAGE	lstr_message

	SetPointer(HourGlass!)
	lstr_message.as_mode = 'CANCEL'
	CloseWithReturn(Parent,lstr_message)
end event

type cb_ok from commandbutton within w_account_authorizations
integer x = 1298
integer y = 1260
integer width = 247
integer height = 108
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "OK"
end type

event clicked;LONG					ll_row 
S_WINDOW_MESSAGE	lstr_message


	SetPointer(HourGlass!)
	
	ll_row = dw_authorization.GetSelectedRow(0)
	lstr_message.as_mode = 'OK'
	IF ll_row > 0 THEN
		lstr_message.al_doubleparm[1] = dw_authorization.GetItemNumber(ll_row,'authorization_no')
	ELSE
		lstr_message.al_doubleparm[1] = 0
	END IF
	
	
	CloseWithReturn(Parent,lstr_message)
end event

type dw_authorization from u_dw_online within w_account_authorizations
integer y = 4
integer width = 3424
integer height = 1200
integer taborder = 10
string dataobject = "d_account_authorizations"
boolean vscrollbar = true
end type

event rowfocuschanged;call super::rowfocuschanged;LONG	ll_row

	IF This.RowCount() = 0 THEN
		Return
	END IF
	ll_row = This.GetRow()				// the currentrow argument was not used because it does not always have a value


	IF ll_row > 0 THEN
		IF this.getitemstring(ll_row,"rehab_service_code") = "S022" AND Date(this.getitemdatetime(ll_row,"rehab_task_authorization_create_date")) >= idt_ephysio_implementation_date  THEN
			Return
		ELSE
			This.SelectRow(0,FALSE)
			This.SelectRow(ll_row,TRUE)
		END IF
	END IF

end event

