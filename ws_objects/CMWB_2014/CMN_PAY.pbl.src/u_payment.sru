$PBExportHeader$u_payment.sru
forward
global type u_payment from userobject
end type
type dw_authorization_groups from u_dw_online within u_payment
end type
type dw_invoice from u_dw_online within u_payment
end type
type dw_basic_claim from u_dw_online within u_payment
end type
type dw_rtw_incentive_payment_xref from u_dw_online within u_payment
end type
type dw_account_payment from u_dw_online within u_payment
end type
type dw_payment_details from u_dw_online within u_payment
end type
type dw_account_transactions from u_dw_online within u_payment
end type
type dw_transaction_details from u_dw_online within u_payment
end type
type cb_search from commandbutton within u_payment
end type
type cb_search2 from commandbutton within u_payment
end type
end forward

global type u_payment from userobject
integer width = 2898
integer height = 1560
long backcolor = 67108864
long tabtextcolor = 33554432
event ue_payment_changes pbm_custom10
event ue_txn_changes pbm_custom11
event ue_itemfocuschanged pbm_custom12
dw_authorization_groups dw_authorization_groups
dw_invoice dw_invoice
dw_basic_claim dw_basic_claim
dw_rtw_incentive_payment_xref dw_rtw_incentive_payment_xref
dw_account_payment dw_account_payment
dw_payment_details dw_payment_details
dw_account_transactions dw_account_transactions
dw_transaction_details dw_transaction_details
cb_search cb_search
cb_search2 cb_search2
end type
global u_payment u_payment

type variables
BOOLEAN ib_payment_type_changed 
//pr1679
end variables

forward prototypes
public subroutine uf_set_account_on ()
public function integer uf_set_visible_expressions ()
public function integer uf_set_protect_expressions ()
end prototypes

public subroutine uf_set_account_on ();// set the account dw's on and turn the payment ones off

dw_account_payment.visible = TRUE
dw_account_payment.enabled = TRUE
dw_account_transactions.visible = TRUE
dw_account_transactions.enabled = TRUE
dw_invoice.visible = TRUE
dw_invoice.enabled = TRUE
cb_search2.visible = TRUE
dw_payment_details.visible = FALSE
dw_payment_details.enabled = FALSE
dw_transaction_details.visible = FALSE
dw_transaction_details.visible = FALSE


end subroutine

public function integer uf_set_visible_expressions ();STRING		ls_address_visible_expression
STRING		ls_use_default_address_flag_visible_expression
STRING		ls_mod

ls_address_visible_expression = ".visible = '0~t if(payment_method_code not in(~~'D~~',~~'R~~',~~'I~~') ,1,0)'"
ls_use_default_address_flag_visible_expression = ".visible = '0~t if(payment_method_code not in(~~'D~~',~~'R~~',~~'I~~') and recipient_type_code = ~~'I~~',1,0)'"


ls_mod = dw_transaction_details.modify("address_line1" + ls_address_visible_expression ) 
if ls_mod <> '' Then SignalError(-666,ls_mod)
ls_mod = dw_transaction_details.modify("t_address_line1" + ls_address_visible_expression ) 
if ls_mod <> '' Then SignalError(-666,ls_mod)

ls_mod = dw_transaction_details.modify("address_line2" + ls_address_visible_expression ) 
if ls_mod <> '' Then SignalError(-666,ls_mod)
ls_mod = dw_transaction_details.modify("t_address_line2" + ls_address_visible_expression ) 
if ls_mod <> '' Then SignalError(-666,ls_mod)

ls_mod = dw_transaction_details.modify("city" + ls_address_visible_expression ) 
if ls_mod <> '' Then SignalError(-666,ls_mod)
ls_mod = dw_transaction_details.modify("t_city" + ls_address_visible_expression ) 
if ls_mod <> '' Then SignalError(-666,ls_mod)

ls_mod = dw_transaction_details.modify("country" + ls_address_visible_expression ) 
if ls_mod <> '' Then SignalError(-666,ls_mod)
ls_mod = dw_transaction_details.modify("t_country" + ls_address_visible_expression ) 
if ls_mod <> '' Then SignalError(-666,ls_mod)

ls_mod = dw_transaction_details.modify("prov_state_code" + ls_address_visible_expression ) 
if ls_mod <> '' Then SignalError(-666,ls_mod)
ls_mod = dw_transaction_details.modify("t_prov_state_code" + ls_address_visible_expression ) 
if ls_mod <> '' Then SignalError(-666,ls_mod)

ls_mod = dw_transaction_details.modify("postal_code" + ls_address_visible_expression ) 
if ls_mod <> '' Then SignalError(-666,ls_mod)
ls_mod = dw_transaction_details.modify("t_postal_code" + ls_address_visible_expression ) 
if ls_mod <> '' Then SignalError(-666,ls_mod)

ls_mod = dw_transaction_details.modify("use_default_address_flag" + ls_use_default_address_flag_visible_expression ) 
if ls_mod <> '' Then SignalError(-666,ls_mod)

return 1
end function

public function integer uf_set_protect_expressions ();STRING		ls_address_protect

ls_address_protect = "use_default_address_flag = 'N'"

dw_transaction_details.uf_protectcolumn("address_line1",ls_address_protect)
dw_transaction_details.uf_protectcolumn("address_line2",ls_address_protect)
dw_transaction_details.uf_protectcolumn("city",ls_address_protect)
dw_transaction_details.uf_protectcolumn("country",ls_address_protect)
dw_transaction_details.uf_protectcolumn("prov_state_code",ls_address_protect)
dw_transaction_details.uf_protectcolumn("postal_code",ls_address_protect)

return 1
end function

on u_payment.create
this.dw_authorization_groups=create dw_authorization_groups
this.dw_invoice=create dw_invoice
this.dw_basic_claim=create dw_basic_claim
this.dw_rtw_incentive_payment_xref=create dw_rtw_incentive_payment_xref
this.dw_account_payment=create dw_account_payment
this.dw_payment_details=create dw_payment_details
this.dw_account_transactions=create dw_account_transactions
this.dw_transaction_details=create dw_transaction_details
this.cb_search=create cb_search
this.cb_search2=create cb_search2
this.Control[]={this.dw_authorization_groups,&
this.dw_invoice,&
this.dw_basic_claim,&
this.dw_rtw_incentive_payment_xref,&
this.dw_account_payment,&
this.dw_payment_details,&
this.dw_account_transactions,&
this.dw_transaction_details,&
this.cb_search,&
this.cb_search2}
end on

on u_payment.destroy
destroy(this.dw_authorization_groups)
destroy(this.dw_invoice)
destroy(this.dw_basic_claim)
destroy(this.dw_rtw_incentive_payment_xref)
destroy(this.dw_account_payment)
destroy(this.dw_payment_details)
destroy(this.dw_account_transactions)
destroy(this.dw_transaction_details)
destroy(this.cb_search)
destroy(this.cb_search2)
end on

type dw_authorization_groups from u_dw_online within u_payment
boolean visible = false
integer x = 1797
integer y = 1348
integer height = 360
integer taborder = 120
string dataobject = "d_authorization_group"
end type

type dw_invoice from u_dw_online within u_payment
boolean visible = false
integer x = 1778
integer y = 248
integer width = 814
integer height = 244
integer taborder = 60
string dataobject = "d_invoice"
boolean border = false
end type

event constructor;/* Overridden for performance reasons.  No column level security needed. */
end event

type dw_basic_claim from u_dw_online within u_payment
boolean visible = false
integer x = 1856
integer y = 1144
integer width = 183
integer height = 164
integer taborder = 90
string dataobject = "d_basic_claim_everything"
end type

event constructor;/* Overridden for performance reasons.  No column level security needed. */
end event

type dw_rtw_incentive_payment_xref from u_dw_online within u_payment
boolean visible = false
integer x = 2075
integer y = 1144
integer width = 183
integer height = 164
integer taborder = 100
string dataobject = "ds_rtw_incentive_payment_xref"
end type

type dw_account_payment from u_dw_online within u_payment
boolean visible = false
integer y = 28
integer width = 2871
integer height = 776
integer taborder = 50
boolean enabled = false
string dataobject = "d_account_payment"
boolean border = false
end type

event itemchanged;call super::itemchanged;Integer li_rtn
Long    ll_rtn, ll_rtn2

This.uf_set_pbmessage(TRUE)
li_rtn = Parent.Event Dynamic ue_payment_changes(ll_rtn, ll_rtn2)
If li_rtn = -1 Then
	Return 2
ELSEIF li_rtn = 2 THEN
	/*
	the payment type code has been changed and the transaction information will be changed later.
	*/
	ib_payment_type_changed = TRUE
End If

end event

event itemfocuschanged;call super::itemfocuschanged;LONG ll_rtn,ll_rtn2

IF dwo.name <> "payment_type_code" THEN
	IF ib_payment_type_changed THEN
		/*
		let's make changes to transaction datawindow now
		*/
		ll_rtn = Parent.Event Dynamic ue_payment_type_changed(ll_rtn, ll_rtn2)
		IF ll_rtn = 0 THEN
			ib_payment_type_changed = FALSE
		END IF
	END IF
END IF
end event

event constructor;/* Overridden for performance reasons.  No column level security needed. */
end event

type dw_payment_details from u_dw_online within u_payment
integer x = 5
integer width = 2866
integer height = 724
integer taborder = 10
string dataobject = "d_payment_details"
boolean border = false
end type

event itemchanged;call super::itemchanged;LONG ll_rtn
LONG ll_arg1, ll_arg2

This.uf_set_pbmessage(TRUE)

ll_rtn = parent.event dynamic ue_payment_changes(ll_arg1,ll_arg2)
if ll_rtn = -1 Then
	Return 2
END IF


end event

event constructor;/* Overridden for performance reasons.  No column level security needed. */
end event

event buttonclicked;call super::buttonclicked;IF dwo.name = 'b_group_help' THEN
	OPEN(w_group_for_authorizing_help)
END IF
end event

type dw_account_transactions from u_dw_online within u_payment
boolean visible = false
integer y = 800
integer width = 2866
integer height = 732
integer taborder = 70
boolean bringtotop = true
boolean enabled = false
string dataobject = "d_claim_txn"
boolean border = false
end type

event itemchanged;call super::itemchanged;Long    ll_rtn, ll_rtn2
Integer li_rtn

This.uf_set_pbmessage(TRUE)

/*	code changed may 04, 2000 MA,  li_rtn was capturing the return code from the triggerevent function
	but was ment to capture the return code from the event ue_txn_changes
*/
li_rtn = Parent.Event Dynamic ue_txn_changes(ll_rtn, ll_rtn2)
If li_rtn = -1 Then
	Return 2
End If
end event

on itemfocuschanged;call u_dw_online::itemfocuschanged;Parent.TriggerEvent('ue_itemfocuschanged')
end on

event constructor;/* Overridden for performance reasons.  No column level security needed. */
end event

type dw_transaction_details from u_dw_online within u_payment
integer x = 18
integer y = 724
integer width = 2880
integer height = 628
integer taborder = 20
boolean bringtotop = true
string dataobject = "d_transaction_details"
boolean border = false
end type

event itemchanged;call super::itemchanged;Long    ll_rtn, ll_rtn2
Integer li_rtn

This.uf_set_pbmessage(TRUE)

/*	code changed may 04, 2000 MA,  li_rtn was capturing the return code from the triggerevent function
	but was ment to capture the return code from the event ue_txn_changes
*/
li_rtn = Parent.Event Dynamic ue_txn_changes(ll_rtn, ll_rtn2)
If li_rtn = -1 Then
	Return 2
End If




end event

event constructor;/* Overridden for performance reasons.  No column level security needed. */

uf_set_visible_expressions()

uf_set_protect_expressions()
end event

type cb_search from commandbutton within u_payment
integer x = 1129
integer y = 872
integer width = 87
integer height = 72
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "?"
end type

event clicked;S_WINDOW_MESSAGE	lstr_message
STRING 				ls_type
BOOLEAN				lb_account_visible
LONG					ll_row
/*	get the type to search for
*/

	IF dw_account_transactions.visible THEN
		lb_account_visible = TRUE
		ll_row = dw_account_transactions.GetRow()
		IF ll_row > 0 THEN
			ls_type = dw_account_transactions.GetItemString(ll_row,'recipient_type_code')
		END IF
	ELSE
		ll_row = dw_transaction_details.GetRow()
		IF ll_row > 0 THEN
			ls_type = dw_transaction_details.GetItemString(ll_row,'recipient_type_code')
		END IF
	END IF

	IF ls_type = 'V' OR ls_type =  'M' OR ls_type =  'O' THEN
		OpenWithParm(w_service_provider_search, ls_type)
		lstr_message = Message.PowerObjectParm
		IF lb_account_visible THEN
			dw_account_transactions.SetColumn('recipient_no')
			dw_account_transactions.SetItem(ll_row,'recipient_no', lstr_message.al_doubleparm[1])
			dw_account_transactions.TriggerEvent(ItemChanged!)
		ELSE
			dw_transaction_details.SetColumn('recipient_no')
			dw_transaction_details.SetItem(ll_row,'recipient_no',lstr_message.al_doubleparm[1])
			dw_transaction_details.TriggerEvent(ItemChanged!)
		END IF
	ELSE
		MessageBox('Warning', 'No search available for this recipient type.')
	END IF

end event

type cb_search2 from commandbutton within u_payment
boolean visible = false
integer x = 773
integer y = 1028
integer width = 87
integer height = 72
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "?"
end type

on clicked;cb_search.TriggerEvent(Clicked!)
end on

