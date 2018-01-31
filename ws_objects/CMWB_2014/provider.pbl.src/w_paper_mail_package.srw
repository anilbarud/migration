$PBExportHeader$w_paper_mail_package.srw
forward
global type w_paper_mail_package from window
end type
type u_print_mail_pacakge from u_print_mail_package within w_paper_mail_package
end type
type st_4 from statictext within w_paper_mail_package
end type
type p_1 from picture within w_paper_mail_package
end type
type st_2 from statictext within w_paper_mail_package
end type
type dw_enter_registration_code from datawindow within w_paper_mail_package
end type
type st_1 from statictext within w_paper_mail_package
end type
type cbx_1 from checkbox within w_paper_mail_package
end type
type cb_2 from commandbutton within w_paper_mail_package
end type
type cb_produce from commandbutton within w_paper_mail_package
end type
end forward

global type w_paper_mail_package from window
integer width = 2811
integer height = 1664
boolean titlebar = true
string title = "Print Registration Letter"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
u_print_mail_pacakge u_print_mail_pacakge
st_4 st_4
p_1 p_1
st_2 st_2
dw_enter_registration_code dw_enter_registration_code
st_1 st_1
cbx_1 cbx_1
cb_2 cb_2
cb_produce cb_produce
end type
global w_paper_mail_package w_paper_mail_package

type variables
S_WINDOW_MESSAGE istr_message

end variables

on w_paper_mail_package.create
this.u_print_mail_pacakge=create u_print_mail_pacakge
this.st_4=create st_4
this.p_1=create p_1
this.st_2=create st_2
this.dw_enter_registration_code=create dw_enter_registration_code
this.st_1=create st_1
this.cbx_1=create cbx_1
this.cb_2=create cb_2
this.cb_produce=create cb_produce
this.Control[]={this.u_print_mail_pacakge,&
this.st_4,&
this.p_1,&
this.st_2,&
this.dw_enter_registration_code,&
this.st_1,&
this.cbx_1,&
this.cb_2,&
this.cb_produce}
end on

on w_paper_mail_package.destroy
destroy(this.u_print_mail_pacakge)
destroy(this.st_4)
destroy(this.p_1)
destroy(this.st_2)
destroy(this.dw_enter_registration_code)
destroy(this.st_1)
destroy(this.cbx_1)
destroy(this.cb_2)
destroy(this.cb_produce)
end on

event open;long ll_entity_no
string  ls_wif_entity_subtype_code,ls_entity_name

istr_message= Message.PowerObjectParm

ll_entity_no = istr_message.al_doubleparm[1]
ls_wif_entity_subtype_code = istr_message.as_stringparm[1]
ls_entity_name  = istr_message.as_stringparm[2]


dw_enter_registration_code.insertrow(0)

st_2.text =  istr_message.as_stringparm[2] + " - Provider Number  " + string(istr_message.al_doubleparm[1]) 
end event

type u_print_mail_pacakge from u_print_mail_package within w_paper_mail_package
boolean visible = false
integer x = 2103
integer y = 1168
integer taborder = 30
end type

type st_4 from statictext within w_paper_mail_package
integer x = 96
integer y = 476
integer width = 2295
integer height = 188
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "The Registration Code is case-sensitive and must be entered by the Provider EXACTLY as you have typed it in here."
boolean focusrectangle = false
end type

type p_1 from picture within w_paper_mail_package
integer x = 1783
integer y = 1060
integer width = 128
integer height = 96
string picturename = "Help!"
boolean focusrectangle = false
end type

event clicked;open(w_paper_mail_package_help)
end event

type st_2 from statictext within w_paper_mail_package
integer x = 41
integer y = 52
integer width = 2651
integer height = 84
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
alignment alignment = center!
boolean focusrectangle = false
end type

type dw_enter_registration_code from datawindow within w_paper_mail_package
integer x = 73
integer y = 716
integer width = 2583
integer height = 284
integer taborder = 10
string title = "none"
string dataobject = "d_enter_registration_code"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type st_1 from statictext within w_paper_mail_package
integer x = 96
integer y = 236
integer width = 2295
integer height = 192
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "The Registration Code can be a combination of spaces, letters and numbers. It can be a phrase, a word or a sentence. It is restricted to minimum of 5 characters and a maximum of 25 characters. "
boolean focusrectangle = false
end type

type cbx_1 from checkbox within w_paper_mail_package
integer x = 142
integer y = 1076
integer width = 1952
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "  I have shared this Registration Code with the Provider "
end type

event clicked;if (this.Checked = true) then
	
	cb_produce.enabled = true
else
	
	cb_produce.enabled = false
end if
end event

type cb_2 from commandbutton within w_paper_mail_package
integer x = 1353
integer y = 1408
integer width = 402
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = " Close"
end type

event clicked;close(w_paper_mail_package)
end event

type cb_produce from commandbutton within w_paper_mail_package
integer x = 855
integer y = 1404
integer width = 402
integer height = 104
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Produce"
end type

event clicked;string ls_registration_code
long ll_len
string  ls_reconfirm_registration_code
long     ll_wif_entity_no 
string ls_wif_entity_subtype_code                   
int     li_rc
string ls_GUID
string ls_error
string ls_exp_datetime
u_ds lds_create_token

//5.20 A registration code is required to produce a Paper Mail Package.
//Message: The Registration Code must be entered and communicated to the Entity to produce a Paper Mail Package. 

//5.30 The registration code must be less than or equal to 25 characters.
//	Message:  The registration code must be less than or equal to 25 characters.

//5.40 The registration code must be greater than or equal to 5 characters.
//	Message:  The registration code must be greater than or equal to 5 characters.

//5.50   The registration code must be re-entered to confirm the registration code.
//	Message:  You must confirm the registration code you have entered that will be communicated to the Entity so they can register.

lds_create_token = CREATE u_ds
lds_create_token.DataObject =  "d_create_token"
lds_create_token.SetTransObject(SQLCA)

dw_enter_registration_code.accepttext()

IF dw_enter_registration_code.rowcount() < 1 THEN RETURN

ls_registration_code = dw_enter_registration_code.getitemstring(1,'registration_code')
if (isnull(ls_registration_code)) then
	messagebox('Information', 'A "Registration Code" must be entered and communicated to the provider to produce a registration letter') //5.30
	RETURN -1
end if

ls_reconfirm_registration_code = dw_enter_registration_code.getitemstring(1,'reconfirm_registration_code')
if (isnull(ls_reconfirm_registration_code) OR Trim(ls_reconfirm_registration_code) = "") then
	messagebox('Information',  ' You must "Re-Type" the "Registration Code" you entered') //5.60
	RETURN -1
end if


ll_len = LEN(ls_registration_code )
if(ll_len < 5)   then
	messagebox('Information', 'The "Registration Code" must be greater than or equal to 5 characters.') //5:40
	RETURN -1
end if


if(ll_len >25)  then
	messagebox('Information', 'The "Registration Code" must be less than or equal to 25 characters.') //5.40
	RETURN -1
end if


if (ls_registration_code <> ls_reconfirm_registration_code) then
	messagebox('Information', 'The "Registration Code" must match the "Re-Type Code"') //5.60,5.65
	RETURN -1
end if


ll_wif_entity_no = istr_message.al_doubleparm[1]
ls_wif_entity_subtype_code = istr_message.as_stringparm[1]


li_rc = lds_create_token.retrieve(ll_wif_entity_no, ls_wif_entity_subtype_code, ls_registration_code, gi_no_minutes_to_expire, 'P', 2)
SQLCA.nf_handle_error('w_paper_mail_package','cb_print.clicked',"lds_create_token.retrieve")

ls_GUID = lds_create_token.getitemstring(li_rc,'guid_return')

ls_exp_datetime =  lds_create_token.getitemstring(li_rc,'exp_datetime')

	
istr_message.as_stringparm[3] = ls_registration_code
istr_message.adtm_datetimeparm[1] = DATETIME(ls_exp_datetime)	

if li_rc > 0 then //print the beast
	u_print_mail_pacakge.uf_print_mail_package(ls_GUID) //2014-08-27 David Worboys	
else
	ls_error = "Problem Finding Mail Package"	
end if

IF (ls_error <> "") THEN //error encountered
	istr_message.al_doubleparm[1] = -1
	Messagebox("Error",ls_error, EXCLAMATION!)
ELSE
	istr_message.al_doubleparm[1] = 1
END IF

destroy lds_create_token

CloseWithReturn(Parent,istr_message)
end event

