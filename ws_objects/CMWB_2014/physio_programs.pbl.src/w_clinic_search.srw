$PBExportHeader$w_clinic_search.srw
forward
global type w_clinic_search from w_ancestor
end type
type uo_search from u_clinic_search within w_clinic_search
end type
type cb_ok from commandbutton within w_clinic_search
end type
type cb_cancel from commandbutton within w_clinic_search
end type
end forward

global type w_clinic_search from w_ancestor
integer x = 960
integer y = 552
integer width = 3214
integer height = 1260
string title = "Clinic Search"
string menuname = ""
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
long backcolor = 67108864
uo_search uo_search
cb_ok cb_ok
cb_cancel cb_cancel
end type
global w_clinic_search w_clinic_search

type variables
LONG il_provider_no 
STRING is_provider_type_code
end variables

forward prototypes
public subroutine wf_set_provider_no (long al_provider_no, string al_provider_type_code)
end prototypes

public subroutine wf_set_provider_no (long al_provider_no, string al_provider_type_code);
	il_provider_no = al_provider_no
	is_provider_type_code = al_provider_type_code
	cb_ok.enabled = TRUE
Return 
end subroutine

event open;call super::open;//uo_search.is_passed_type_code = Message.StringParm

uo_search.uf_set_parent(THIS)



end event

on w_clinic_search.create
int iCurrent
call super::create
this.uo_search=create uo_search
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.uo_search
this.Control[iCurrent+2]=this.cb_ok
this.Control[iCurrent+3]=this.cb_cancel
end on

on w_clinic_search.destroy
call super::destroy
destroy(this.uo_search)
destroy(this.cb_ok)
destroy(this.cb_cancel)
end on

type uo_search from u_clinic_search within w_clinic_search
integer taborder = 30
end type

on uo_search.destroy
call u_clinic_search::destroy
end on

type cb_ok from commandbutton within w_clinic_search
integer x = 1403
integer y = 1052
integer width = 274
integer height = 108
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&OK"
end type

on clicked;S_WINDOW_MESSAGE lstr_message

	lstr_message.al_doubleparm[1] = il_provider_no
	lstr_message.as_stringparm[1] = is_provider_type_code
	CloseWithReturn(PARENT,lstr_message)

end on

type cb_cancel from commandbutton within w_clinic_search
integer x = 1682
integer y = 1052
integer width = 274
integer height = 108
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
end type

on clicked;S_WINDOW_MESSAGE lstr_message

	lstr_message.al_doubleparm[1] = 0
	lstr_message.as_stringparm[1] = ""
	CloseWithReturn(PARENT,lstr_message)

end on

