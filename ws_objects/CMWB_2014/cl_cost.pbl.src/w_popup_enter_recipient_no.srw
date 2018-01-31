$PBExportHeader$w_popup_enter_recipient_no.srw
forward
global type w_popup_enter_recipient_no from window
end type
type cb_1 from commandbutton within w_popup_enter_recipient_no
end type
type st_1 from statictext within w_popup_enter_recipient_no
end type
type dw_recipient_name from u_dw_online within w_popup_enter_recipient_no
end type
type cb_accept from commandbutton within w_popup_enter_recipient_no
end type
end forward

global type w_popup_enter_recipient_no from window
integer x = 2002
integer y = 1000
integer width = 2025
integer height = 812
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
cb_1 cb_1
st_1 st_1
dw_recipient_name dw_recipient_name
cb_accept cb_accept
end type
global w_popup_enter_recipient_no w_popup_enter_recipient_no

type variables
s_window_message istr_window_message

string is_window_name = "w_popup_add_txn_unit_of_work_no"


end variables

event open;long 		ll_row
long		ll_rc
long 		ll_recipient_no
long		ll_no_recipient

string 	ls_recipient_name
string	ls_recipient_type
STRING	ls_claim_role_desc

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


istr_window_message = Message.PowerObjectParm
ll_no_recipient = upperbound(istr_window_message.al_doubleparm[])

for ll_row = 1 to ll_no_recipient
	ll_recipient_no = istr_window_message.al_doubleparm[ll_row]
	ls_recipient_name = mid(istr_window_message.as_stringparm[ll_row],2,len(istr_window_message.as_stringparm[ll_row]))	
	ls_recipient_type = mid(istr_window_message.as_stringparm[ll_row],1,1)	
	
	
	if ls_recipient_type = "I" then
		
			SELECT dbo.Claim_Role.claim_role_desc  
			 INTO :ls_claim_role_desc 
			 FROM dbo.INDIVIDUAL,   
					dbo.Claim_Role,   
					dbo.CLAIM_PARTICIPANT  
			WHERE ( dbo.Claim_Role.claim_role_code = dbo.CLAIM_PARTICIPANT.claim_role_code ) and  
					( dbo.INDIVIDUAL.individual_no = dbo.CLAIM_PARTICIPANT.individual_no ) and  
					( ( dbo.INDIVIDUAL.individual_no = :ll_recipient_no ) )   
					using SQLCA;
			IF SQLCA.nf_handle_error("SELECT * FROM dbo.CLAIM_PARTICIPANT ","","retrieve by individual") = 100 THEN
				messagebox("Warning","No CLAIM_PARTICIPANT found for recipient no " + string (ll_recipient_no))		
				RETURN -1
			END IF				
					
	ELSE
		ls_claim_role_desc = ""			
	END IF
				

	ll_rc = dw_recipient_name.insertrow(0)
	dw_recipient_name.setitem(ll_rc,'recipient_name',ls_recipient_name)
	dw_recipient_name.setitem(ll_rc,'recipient_no',ll_recipient_no)
	dw_recipient_name.setitem(ll_rc,'claim_role_code',ls_claim_role_desc)
next

dw_recipient_name.selectrow(1,true)


end event

on w_popup_enter_recipient_no.create
this.cb_1=create cb_1
this.st_1=create st_1
this.dw_recipient_name=create dw_recipient_name
this.cb_accept=create cb_accept
this.Control[]={this.cb_1,&
this.st_1,&
this.dw_recipient_name,&
this.cb_accept}
end on

on w_popup_enter_recipient_no.destroy
destroy(this.cb_1)
destroy(this.st_1)
destroy(this.dw_recipient_name)
destroy(this.cb_accept)
end on

type cb_1 from commandbutton within w_popup_enter_recipient_no
integer x = 1536
integer y = 672
integer width = 402
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Cancel"
end type

event clicked;istr_window_message.al_doubleparm[1] = 0 
closewithreturn(parent,istr_window_message)
end event

type st_1 from statictext within w_popup_enter_recipient_no
integer x = 146
integer y = 64
integer width = 1321
integer height = 64
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "The following recipients have been detected"
boolean focusrectangle = false
end type

type dw_recipient_name from u_dw_online within w_popup_enter_recipient_no
integer x = 110
integer y = 192
integer width = 1865
integer height = 384
integer taborder = 10
string title = "none"
string dataobject = "d_recipient_name"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;selectrow(0,false)
selectrow(currentrow,true)

end event

type cb_accept from commandbutton within w_popup_enter_recipient_no
integer x = 1061
integer y = 672
integer width = 402
integer height = 104
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Accept"
end type

event clicked;long ll_row
long ll_recipient_no


ll_row = dw_recipient_name.getrow()

ll_recipient_no = dw_recipient_name.getitemnumber(ll_row,'recipient_no')
	
istr_window_message.al_doubleparm[1] = ll_recipient_no 

closewithreturn(parent,istr_window_message)
end event

