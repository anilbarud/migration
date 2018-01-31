$PBExportHeader$w_select_claims.srw
forward
global type w_select_claims from window
end type
type dw_claims from u_dw_online within w_select_claims
end type
type cb_open_event_log from commandbutton within w_select_claims
end type
type cb_close from commandbutton within w_select_claims
end type
end forward

global type w_select_claims from window
integer width = 2830
integer height = 1240
boolean titlebar = true
string title = "Select Claim for Event Log"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
dw_claims dw_claims
cb_open_event_log cb_open_event_log
cb_close cb_close
end type
global w_select_claims w_select_claims

type variables
LONG						il_individual_no
STRING					is_claim_role_code
s_window_message	   istr_message
ULONG                iul_handle
end variables

event open;INTEGER					li_rows, li_upperbound

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


istr_message = Message.PowerObjectParm

il_individual_no = istr_message.al_doubleparm[2]

is_claim_role_code = istr_message.as_stringparm[1]

dw_claims.SetTransObject(SQLCA)
li_rows = dw_claims.Retrieve(il_individual_no,is_claim_role_code)
SQLCA.nf_handle_error('w_select_claims', 'open', 'dw_claims.Retrieve')

iul_handle = Handle(This)

// sets up to close this window if frame is closed
li_upperbound = UpperBound(gstr_window_array) + 1
gstr_window_array[li_upperbound].window_element = THIS
gstr_window_array[li_upperbound].handle_element = iul_handle
end event

on w_select_claims.create
this.dw_claims=create dw_claims
this.cb_open_event_log=create cb_open_event_log
this.cb_close=create cb_close
this.Control[]={this.dw_claims,&
this.cb_open_event_log,&
this.cb_close}
end on

on w_select_claims.destroy
destroy(this.dw_claims)
destroy(this.cb_open_event_log)
destroy(this.cb_close)
end on

event close;n_common_annuity	lnv_common_annuity

lnv_common_annuity = Create n_common_annuity

lnv_common_annuity.nf_close_handle_array(iul_handle)

end event

type dw_claims from u_dw_online within w_select_claims
integer x = 64
integer y = 32
integer width = 2665
integer height = 924
integer taborder = 10
string dataobject = "d_claims"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;THIS.uf_setselect(1)
end event

type cb_open_event_log from commandbutton within w_select_claims
integer x = 59
integer y = 1000
integer width = 498
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Open Event Log"
end type

event clicked;DATE			ldt_accident_date
INTEGER		li_row
LONG			ll_claim_no
STRING		ls_last_name, ls_given_names


/*
lstr_message.al_doubleparm[1] = al_claim_no
lstr_message.al_doubleparm[2] = al_individual_no

lstr_message.adt_DateParm[1] = adt_accident_date

lstr_message.as_StringParm[1] = as_claim_role_code
lstr_message.as_StringParm[2] = as_last_name
lstr_message.as_StringParm[3] = as_given_names
lstr_message.as_StringParm[4] = as_event_category_code
lstr_message.as_StringParm[5] = as_event_type_code
lstr_message.as_StringParm[6] = as_event_specific_code
lstr_message.as_StringParm[7] = as_allow_parameter_change
lstr_message.as_StringParm[8] = as_add_new_event
*/

li_row = dw_claims.GetRow()

IF li_row > 0 THEN
	ll_claim_no = dw_claims.GetItemNumber(li_row,'claim_no')
	ldt_accident_date = Date(dw_claims.GetItemDateTime(li_row,'accident_date'))
	
	SELECT	last_name, given_names
	INTO		:ls_last_name, :ls_given_names
	FROM		INDIVIDUAL
	WHERE	individual_no = :il_individual_no
	USING SQLCA;
	SQLCA.nf_handle_error('w_select_claims', 'cb_open_event_log.clicked', 'select last_name, given_names from INDIVIDUAL')
	
	istr_message.al_doubleparm[1] = ll_claim_no
	istr_message.al_doubleparm[2] = il_individual_no
	istr_message.al_doubleparm[3] = 0
	
	istr_message.adt_DateParm[1] = ldt_accident_date
	
	istr_message.as_stringparm[1] = is_claim_role_code
	istr_message.as_stringparm[2] = ls_last_name
	istr_message.as_stringparm[3] = ls_given_names
	istr_message.as_stringparm[4] = ''
	istr_message.as_stringparm[5] = ''
	istr_message.as_stringparm[6] = ''
	istr_message.as_stringparm[7] = 'Y'
	istr_message.as_stringparm[8] = 'N'
	istr_message.as_stringparm[9] = ''
	istr_message.as_stringparm[10]= ''
	istr_message.as_stringparm[11]= ''
	
	OpenWithParm(w_event_log_response,istr_message)
ELSE
	MessageBox('No Claim','Please select a claim before opening Event Log window.',Exclamation!)
END IF
end event

type cb_close from commandbutton within w_select_claims
integer x = 2331
integer y = 1000
integer width = 402
integer height = 104
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;Close(Parent)
end event

