$PBExportHeader$w_individual_search2.srw
forward
global type w_individual_search2 from w_ancestor
end type
type uo_claim_search from u_claim_search within w_individual_search2
end type
type cb_ok from commandbutton within w_individual_search2
end type
type cb_cancel from commandbutton within w_individual_search2
end type
end forward

global type w_individual_search2 from w_ancestor
integer x = 910
integer y = 468
integer width = 3136
integer height = 2064
string menuname = ""
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
long backcolor = 67108864
uo_claim_search uo_claim_search
cb_ok cb_ok
cb_cancel cb_cancel
end type
global w_individual_search2 w_individual_search2

type variables
w_a_report	                iwi_window_parent
s_window_message		istr_window_message
end variables

forward prototypes
public subroutine wf_set_parent (window window_parent)
end prototypes

public subroutine wf_set_parent (window window_parent);/*------------------------------------------------------------------------*/
/*  This function sets a pointer to the parent window.                    */
/*------------------------------------------------------------------------*/

iwi_window_parent = window_parent
end subroutine

on open;call w_ancestor::open;/*------------------------------------------------------------------*/
/*  Set the default to an 'or' type of search for indexing.         */
/*------------------------------------------------------------------*/

S_WINDOW_MESSAGE lstr_window_message

lstr_window_message = Message.PowerObjectParm

wf_set_parent(lstr_window_message.awi_parent_window)
uo_claim_search.uf_set_parent(This)
uo_claim_search.uf_set_search_type(lstr_window_message.as_StringParm[1])
uo_claim_search.uf_protect_searchtype("DISABLE")

end on

on w_individual_search2.create
int iCurrent
call super::create
this.uo_claim_search=create uo_claim_search
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.uo_claim_search
this.Control[iCurrent+2]=this.cb_ok
this.Control[iCurrent+3]=this.cb_cancel
end on

on w_individual_search2.destroy
call super::destroy
destroy(this.uo_claim_search)
destroy(this.cb_ok)
destroy(this.cb_cancel)
end on

type uo_claim_search from u_claim_search within w_individual_search2
integer x = 18
integer y = 8
integer width = 3077
integer height = 1828
integer taborder = 10
end type

on ue_doubleclicked;call u_claim_search::ue_doubleclicked;cb_ok.TriggerEvent(Clicked!)
end on

on uo_claim_search.destroy
call u_claim_search::destroy
end on

type cb_ok from commandbutton within w_individual_search2
integer x = 1079
integer y = 1840
integer width = 293
integer height = 96
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;STRING            ls_last_name, ls_first_name
INTEGER	         li_ReturnCode, li_row
S_WINDOW_MESSAGE  lstr_window_message

/*-----------------------------------------------------------------*/
/*  If any claims were found matching the search criteria, check   */
/*  to see if any are for the individual you want to create a claim*/
/*  for.                                                           */
/*-----------------------------------------------------------------*/

//*******************************************************************
// determine which search list is visible
// the claim list or the individual list
//*******************************************************************
IF uo_claim_search.dw_search_list_individual.visible THEN
// the double clicking was on the individual list
   IF uo_claim_search.dw_search_list_individual.RowCount() > 0 THEN
	   li_Row = uo_claim_search.dw_search_list_individual.GetRow()
   	ls_last_name = uo_claim_search.dw_search_list_individual.GetItemString(li_Row,"last_name")
	   ls_first_name = uo_claim_search.dw_search_list_individual.GetItemString(li_Row,"given_names")

  		lstr_window_message.awi_parent_window = iwi_window_parent
	   lstr_window_message.al_DoubleParm[1] = uo_claim_search.dw_search_list_individual.GetItemNumber(li_Row,"individual_no")
   	CloseWithReturn(Parent,lstr_window_message)
   ELSE
   	lstr_window_message.awi_parent_window = iwi_window_parent
	   lstr_window_message.al_DoubleParm[1] = 0
   	CloseWithReturn(Parent,lstr_window_message)
		Return
   END IF
ELSE
   IF uo_claim_search.dw_search_list.RowCount() > 0 THEN
	   li_Row = uo_claim_search.dw_search_list.GetRow()
   	ls_last_name = uo_claim_search.dw_search_list.GetItemString(li_Row,"last_name")
	   ls_first_name = uo_claim_search.dw_search_list.GetItemString(li_Row,"given_names")

  		lstr_window_message.awi_parent_window = iwi_window_parent
		lstr_window_message.al_DoubleParm[1] = uo_claim_search.dw_search_list.GetItemNumber(li_Row,"individual_no")
	   CloseWithReturn(Parent,lstr_window_message)
		Return
   ELSE
   	lstr_window_message.awi_parent_window = iwi_window_parent
	   lstr_window_message.al_DoubleParm[1] = 0
   	CloseWithReturn(Parent,lstr_window_message)
		Return
   END IF
END IF
end event

type cb_cancel from commandbutton within w_individual_search2
integer x = 1390
integer y = 1840
integer width = 293
integer height = 96
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
lstr_message.al_doubleparm[2] = 0

CloseWithReturn(Parent,lstr_message)
end on

