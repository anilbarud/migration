$PBExportHeader$w_maintain_claim_participant.srw
$PBExportComments$a list of the claim roles - calls w_add_participant_individual
forward
global type w_maintain_claim_participant from w_a_tool
end type
type dw_claim_participant from u_dw_online within w_maintain_claim_participant
end type
type cb_add from commandbutton within w_maintain_claim_participant
end type
type cb_save from commandbutton within w_maintain_claim_participant
end type
type cb_change from commandbutton within w_maintain_claim_participant
end type
end forward

global type w_maintain_claim_participant from w_a_tool
integer x = 0
integer y = 0
boolean resizable = false
boolean border = false
dw_claim_participant dw_claim_participant
cb_add cb_add
cb_save cb_save
cb_change cb_change
end type
global w_maintain_claim_participant w_maintain_claim_participant

type variables
N_CLAIM_PARTICIPANT inv_claim_participant
S_WINDOW_MESSAGE istr_message
end variables

on open;call w_a_tool::open;U_DWA ldw_dw[]

	istr_message = Message.PowerObjectParm
	IF istr_message.as_mode = 'READ' THEN
/*	set up window for read only
*/
		cb_add.enabled = FALSE
	END IF
	dw_claim_participant.uf_setselect(1)

	inv_claim_participant = Create n_claim_participant

	ldw_dw[1] = dw_claim_participant
	inv_claim_participant.nf_set_datawindow(ldw_dw[],SQLCA)

	inv_claim_participant.nf_init(ldw_dw[], SQLCA, THIS)
	inv_claim_participant.nf_set_commit(TRUE)
	inv_claim_participant.nf_retrieve(istr_message.al_doubleparm[1])


end on

on close;call w_a_tool::close;Destroy inv_claim_participant
end on

on w_maintain_claim_participant.create
int iCurrent
call super::create
this.dw_claim_participant=create dw_claim_participant
this.cb_add=create cb_add
this.cb_save=create cb_save
this.cb_change=create cb_change
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_claim_participant
this.Control[iCurrent+2]=this.cb_add
this.Control[iCurrent+3]=this.cb_save
this.Control[iCurrent+4]=this.cb_change
end on

on w_maintain_claim_participant.destroy
call super::destroy
destroy(this.dw_claim_participant)
destroy(this.cb_add)
destroy(this.cb_save)
destroy(this.cb_change)
end on

type st_title from w_a_tool`st_title within w_maintain_claim_participant
integer y = 12
integer width = 2661
string text = "Claim Participant Maintenance"
end type

type cb_close from w_a_tool`cb_close within w_maintain_claim_participant
integer x = 2286
integer y = 1712
integer taborder = 10
end type

type dw_claim_participant from u_dw_online within w_maintain_claim_participant
integer x = 14
integer y = 104
integer width = 2651
integer height = 1532
integer taborder = 20
string dataobject = "d_claim_participant_maint"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

on retrieveend;call u_dw_online::retrieveend;IF This.RowCount() > 0 THEN
	This.SelectRow(1,TRUE)
	This.SetRow(1)
END IF
end on

event doubleclicked;call super::doubleclicked;IF row > 0 THEN
   THIS.ScrollToRow(row)
   cb_change.TriggerEvent(Clicked!)
END IF   

end event

type cb_add from commandbutton within w_maintain_claim_participant
integer x = 453
integer y = 1712
integer width = 379
integer height = 100
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Add..."
end type

event clicked;	SetPointer(HourGlass!)
	istr_message.al_doubleparm[2] = 0
	istr_message.as_stringparm[1] = 'ADD'
	OpenWithParm(w_add_participant_individual, istr_message)
	inv_claim_participant.nf_retrieve(istr_message.al_doubleparm[1])
	iw_active_sheet.wf_set_claim(istr_message.al_doubleparm[1])

// return and retrieve the data back in


end event

type cb_save from commandbutton within w_maintain_claim_participant
boolean visible = false
integer x = 1559
integer y = 1536
integer width = 247
integer height = 108
integer taborder = 50
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Save"
end type

event clicked;SetPointer(HourGlass!)

SQLCA.nf_begin_transaction()

IF inv_claim_participant.nf_save() < 0 THEN
	SQLCA.nf_rollback_transaction()
// disable/enable buttons
ELSE
	SQLCA.nf_commit_transaction()
// do whatever
   inv_claim_participant.nf_retrieve(istr_message.al_doubleparm[1])
   Return 
END IF
end event

type cb_change from commandbutton within w_maintain_claim_participant
integer x = 55
integer y = 1712
integer width = 379
integer height = 100
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Details..."
end type

event clicked;LONG  ll_row
STRING ls_command

// get the individual number and send it with the claim number to the edit screen
	SetPointer(HourGlass!)
	ll_row = dw_claim_participant.GetRow()
	istr_message.al_doubleparm[2] = dw_claim_participant.GetItemNumber(ll_row, 'individual_no')
	istr_message.as_stringparm[1] = 'MOD'
	OpenWithParm(w_add_participant_individual, istr_message)
	
	inv_claim_participant.nf_retrieve(istr_message.al_doubleparm[1])
	iw_active_sheet.wf_set_claim(istr_message.al_doubleparm[1])

// return and retrieve the data back in


end event

