$PBExportHeader$w_therapist_search.srw
forward
global type w_therapist_search from w_ancestor
end type
type uo_search from u_therapist_search within w_therapist_search
end type
type cb_ok from commandbutton within w_therapist_search
end type
type cb_cancel from commandbutton within w_therapist_search
end type
end forward

global type w_therapist_search from w_ancestor
integer x = 960
integer y = 552
integer width = 3826
integer height = 1388
string title = "Therapist Search"
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
global w_therapist_search w_therapist_search

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

event open;call super::open;// get the parm passed
// this window requires a parameter of the type of service provider to search for

uo_search.is_passed_type_code = Message.StringParm

uo_search.uf_set_parent(THIS)






end event

on w_therapist_search.create
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

on w_therapist_search.destroy
call super::destroy
destroy(this.uo_search)
destroy(this.cb_ok)
destroy(this.cb_cancel)
end on

type uo_search from u_therapist_search within w_therapist_search
integer x = 18
integer taborder = 30
end type

on uo_search.destroy
call u_therapist_search::destroy
end on

type cb_ok from commandbutton within w_therapist_search
integer x = 1664
integer y = 1160
integer width = 274
integer height = 108
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;INTEGER		li_counter, li_rowcount, li_array_counter

LONG			ll_therapist_no

S_WINDOW_MESSAGE lstr_message

li_rowcount = uo_search.dw_list.rowcount()

IF isnull(li_rowcount) OR li_rowcount <= 0 THEN 
	cb_cancel.triggerevent('clicked')
	RETURN
END IF 

li_array_counter = 1

FOR li_counter = 1 TO li_rowcount
	
	IF uo_search.dw_list.isselected(li_counter) = TRUE  THEN
		
		ll_therapist_no = uo_search.dw_list.GetItemNumber(li_counter, 'therapist_no' )
		lstr_message.al_doubleparm[li_array_counter] = ll_therapist_no
		li_array_counter ++
		
	END IF 
	
NEXT

CloseWithReturn(PARENT,lstr_message)

end event

type cb_cancel from commandbutton within w_therapist_search
integer x = 1943
integer y = 1160
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

event clicked;S_WINDOW_MESSAGE lstr_message

	lstr_message.al_doubleparm[1] = 0
	CloseWithReturn(PARENT,lstr_message)

end event

