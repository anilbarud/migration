$PBExportHeader$w_confirm_contract_reminder.srw
forward
global type w_confirm_contract_reminder from window
end type
type cb_print from commandbutton within w_confirm_contract_reminder
end type
type cb_close from commandbutton within w_confirm_contract_reminder
end type
type dw_external from u_datawindow within w_confirm_contract_reminder
end type
end forward

global type w_confirm_contract_reminder from window
integer width = 3218
integer height = 1600
boolean titlebar = true
string title = "Annuity Purchase - Confirm Contract"
boolean controlmenu = true
boolean minbox = true
windowtype windowtype = popup!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
cb_print cb_print
cb_close cb_close
dw_external dw_external
end type
global w_confirm_contract_reminder w_confirm_contract_reminder

type variables
N_COMMON_ANNUITY   inv_common_annuity
ULONG              iul_handle
WINDOW             iw_win

end variables

forward prototypes
public function WINDOW wf_get_window_reference ()
end prototypes

public function WINDOW wf_get_window_reference ();RETURN THIS
end function

event open;INTEGER          li_inserted_row, li_upperbound
LONG             ll_individual_no, ll_claim_no, ll_annuity_payout_no
STRING           ls_name, ls_annuity_role_desc, ls_payout_info, ls_payout_info_2
S_WINDOW_MESSAGE lstr_message

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


lstr_message = Message.PowerObjectParm
ls_name = lstr_message.as_stringparm[1]
ls_annuity_role_desc = lstr_message.as_stringparm[2]

ll_annuity_payout_no = lstr_message.al_doubleparm[1]
ll_individual_no = lstr_message.al_doubleparm[2]
ll_claim_no = lstr_message.al_doubleparm[3]


ls_payout_info = ls_name +'     '+ 'Annuity Role: ' + ls_annuity_role_desc
IF ll_claim_no = 0 THEN
	// claimant
	ls_payout_info_2 = 'Individual#: ' + String(ll_individual_no) +'     '+ 'Annuity Payout#: ' + String(ll_annuity_payout_no)
ELSE
	// surviving spouse
	ls_payout_info_2 = 'Individual#: ' + String(ll_individual_no) +'     '+ 'Claim#: ' + String(ll_claim_no) +'     '+ 'Annuity Payout#: ' + String(ll_annuity_payout_no)
END IF


li_inserted_row = dw_external.InsertRow(0)

dw_external.SetItem(li_inserted_row,'payout_info',ls_payout_info)
dw_external.SetItem(li_inserted_row,'payout_info_2',ls_payout_info_2)
dw_external.SetItem(li_inserted_row,'line01', 'Contract Details MUST follow legislation. Please confirm the following items:')
dw_external.SetItem(li_inserted_row,'line02', '     The contract must issue a monthly benefit amount, not yearly or any other period')
dw_external.SetItem(li_inserted_row,'line03', '     The contract must have WHSCC as a beneficiary')
dw_external.SetItem(li_inserted_row,'line04', '     Any other beneficiaries MUST be DEPENDANTS')
dw_external.SetItem(li_inserted_row,'line05', '     The Beneficiaries must be irrevocable')
dw_external.SetItem(li_inserted_row,'line06', '     The contract must be for an Annuity only, not a RIF, TIF, GIC, LIRA  etc.')
dw_external.SetItem(li_inserted_row,'line07', '     The contract must be for the terms specified in our legislation (any other period is not permitted):')
dw_external.SetItem(li_inserted_row,'line08', 'NOTE that this checklist is not saved as part of the Annuity Payout and may be printed for future reference, if required.')

iw_win = wf_get_window_reference()



iul_handle = Handle(THIS)

// sets up to close this window if frame is closed
li_upperbound = UpperBound(gstr_window_array) + 1
gstr_window_array[li_upperbound].window_element = THIS
gstr_window_array[li_upperbound].handle_element = iul_handle

inv_common_annuity = Create n_common_annuity
end event

on w_confirm_contract_reminder.create
this.cb_print=create cb_print
this.cb_close=create cb_close
this.dw_external=create dw_external
this.Control[]={this.cb_print,&
this.cb_close,&
this.dw_external}
end on

on w_confirm_contract_reminder.destroy
destroy(this.cb_print)
destroy(this.cb_close)
destroy(this.dw_external)
end on

event close;INTEGER		li_counter, li_upper

inv_common_annuity.nf_close_handle_array(iul_handle)

li_upper = UpperBound(gstr_window_array)

FOR li_counter = 1 TO li_upper
	IF gstr_window_array[li_counter].window_element.ClassName() = 'w_confirm_contract_external_checklist' THEN
		gstr_window_array[li_counter].window_element.SetFocus()
	END IF
NEXT
end event

type cb_print from commandbutton within w_confirm_contract_reminder
integer x = 32
integer y = 1396
integer width = 402
integer height = 104
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Print"
end type

event clicked;dw_external.Print()
end event

type cb_close from commandbutton within w_confirm_contract_reminder
integer x = 2775
integer y = 1396
integer width = 402
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;Close(PARENT)
end event

type dw_external from u_datawindow within w_confirm_contract_reminder
integer x = 32
integer y = 24
integer width = 3145
integer height = 1360
integer taborder = 10
string dataobject = "d_confirm_contract_reminder"
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;THIS.Object.DataWindow.Print.Orientation= '1' // Landscape
end event

event rbuttondown;call super::rbuttondown;M_DW_RMB_POPUP lm_popup

/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
lm_popup = Create m_dw_rmb_popup
lm_popup.mf_set_datawindow(This)

// don't display lines
lm_popup.m_options.m_1.Visible = FALSE
lm_popup.m_options.m_2.Visible = FALSE


lm_popup.m_options.PopMenu(iw_win.PointerX( ), iw_win.PointerY( ))

Destroy lm_popup
end event

